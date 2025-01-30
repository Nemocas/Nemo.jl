VERSION >= v"1.9" || error("This script requires Julia 1.7 or later")

const headerfile_begin_regex = r"#ifdef __cplusplus\s*extern \"C\"\s*\{\s*#endif"
const headerfile_end_regex = r"#ifdef __cplusplus\s*\}\s*#endif"

function expand_templates(input, flintdir)
  matches = eachmatch(r"^@include_c_header\(\"([A-Za-z_.]+)\"\)$"m, input)
  substitutions = Pair{String,String}[]
  for m in matches
    filename = m.captures[1]
    @info "Including header \"$filename\""
    template_key = "@include_c_header(\"$filename\")"

    # Read the file
    content = open(joinpath(flintdir, filename)) do headerfile
      read(headerfile, String)
    end
    # Restrict to the relevant part (ignore include guards etc.)
    begin_match = match(headerfile_begin_regex, content)
    @assert !isnothing(begin_match)
    end_match = match(headerfile_end_regex, content)
    @assert !isnothing(end_match)
    relevant_content = content[(begin_match.offset + length(begin_match.match)):(end_match.offset - 1)]
    # Convert to julia
    translated_content = c2julia(relevant_content)
    # Build the substitution value
    template_val =
      "###############################################################################\n" *
      "# begin $filename\n\n" *
      strip(translated_content) *
      "\n\n# end $filename\n" *
      "###############################################################################\n"
    push!(substitutions, template_key => template_val)
  end
  return replace(input, substitutions...)
end

const regex_typedef_struct_fields_name = r"^typedef struct\s*\{([^{}]+)\}\s*([a-z_]+);"m
const regex_typedef_struct_fields_ptrname = r"^typedef struct\s*\{([^{}]+)\}\s*([a-z_]+)\[1\];"m
const regex_struct_structname_fields = r"^struct *([a-z_]+)\s*\{([^{}]+)\}\s*;"m
const regex_typedef_struct_structname_fields_name = r"^typedef struct *([a-z_]+)\s*\{([^{}]+)\}\s*([a-z_]+);"m

const regex_typedef_enum_values_name = r"^typedef enum\s*\{([^{}]+)\}\s*([a-z_]+);"m
const regex_enum_enumname_values = r"^enum *([a-z_]+)\s*\{([^{}]+)\}\s*;"m
const regex_typedef_enum_enumname_values_name = r"^typedef enum *([a-z_]+)\s*\{([^{}]+)\}\s*([a-z_]+);"m

function convert_struct(str)
  substitutions = Pair{Regex,Union{SubstitutionString,Function}}[
    regex_typedef_struct_fields_name => s"struct \2\1end",                                  # whole typedef struct construct
    regex_struct_structname_fields => s"struct struct_\1\2end",                             # whole struct construct
    regex_typedef_struct_fields_ptrname => s"struct struct_\2\1end\nconst \2 = Ptr{struct_\2}", # whole typedef struct construct with [1]
    regex_typedef_struct_structname_fields_name => s"struct \3\2end\nconst struct_\1 = \3", # whole typedef struct construct with two names
    r"^ +([a-z_]+) +([A-Za-z0-9_]+);"m => s"  \2::\1",                                      # simple fields (one to five declared on one line)
    r"^ +([a-z_]+) +([A-Za-z0-9_]+) *, *([A-Za-z0-9_]+);"m => s"  \2::\1\n  \3::\1",
    r"^ +([a-z_]+) +([A-Za-z0-9_]+) *, *([A-Za-z0-9_]+) *, *([A-Za-z0-9_]+);"m => s"  \2::\1\n  \3::\1\n  \4::\1",
    r"^ +([a-z_]+) +([A-Za-z0-9_]+) *, *([A-Za-z0-9_]+) *, *([A-Za-z0-9_]+) *, *([A-Za-z0-9_]+);"m => s"  \2::\1\n  \3::\1\n  \4::\1\n  \5::\1",
    r"^ +([a-z_]+) +([A-Za-z0-9_]+) *, *([A-Za-z0-9_]+) *, *([A-Za-z0-9_]+) *, *([A-Za-z0-9_]+) *, *([A-Za-z0-9_]+);"m => s"  \2::\1\n  \3::\1\n  \4::\1\n  \5::\1\n  \6::\1",
    r"^ +([a-z_]+) *\* *([A-Za-z0-9_]+);"m => s"  \2::Ptr{\1}",                             # pointer field (one to five declared on one line)
    r"^ +([a-z_]+) *\* *([A-Za-z0-9_]+) *, *\* *([A-Za-z0-9_]+);"m => s"  \2::Ptr{\1}\n  \3::Ptr{\1}",
    r"^ +([a-z_]+) *\* *([A-Za-z0-9_]+) *, *\* *([A-Za-z0-9_]+) *, *\* *([A-Za-z0-9_]+);"m => s"  \2::Ptr{\1}\n  \3::Ptr{\1}\n  \4::Ptr{\1}",
    r"^ +([a-z_]+) *\* *([A-Za-z0-9_]+) *, *\* *([A-Za-z0-9_]+) *, *\* *([A-Za-z0-9_]+) *, *\* *([A-Za-z0-9_]+);"m => s"  \2::Ptr{\1}\n  \3::Ptr{\1}\n  \4::Ptr{\1}\n  \5::Ptr{\1}",
    r"^ +([a-z_]+) *\* *([A-Za-z0-9_]+) *, *\* *([A-Za-z0-9_]+) *, *\* *([A-Za-z0-9_]+) *, *\* *([A-Za-z0-9_]+) *, *\* *([A-Za-z0-9_]+);"m => s"  \2::Ptr{\1}\n  \3::Ptr{\1}\n  \4::Ptr{\1}\n  \5::Ptr{\1}\n  \6::Ptr{\1}",
    r"^ +([a-z_]+) *\* *\* *([A-Za-z0-9_]+);"m => s"  \2::Ptr{Ptr{\1}}",                   # double pointer field
    r"^ +([a-z_]+) +([A-Za-z0-9_]+)\[([A-Za-z0-9_]+)\];"m => s"  \2::NTuple{\3, \1}",       # fixed len array field
    r"^ +[a-z_]+ *\( *\* *([A-Za-z0-9_]+) *\) *\([a-z_, *]+\);"m => s"  \1::Ptr{Nothing}",  # function pointer field
    r"^ +struct *([a-z_]+) +([A-Za-z0-9_]+);"m => s"  \2::struct_\1",                       # struct field (without typedef)
    r"^ +enum *([a-z_]+) +([A-Za-z0-9_]+);"m => s"  \2::enum_\1",                           # enum field (without typedef)
  ]
  for substitution in substitutions
    str = replace(str, substitution)
  end
  return str
end

function convert_enum(str)
  substitutions = Pair{Regex,Union{SubstitutionString,Function}}[
    regex_typedef_enum_values_name => s"@enum \2 begin\1end",                               # whole typedef enum construct
    regex_enum_enumname_values => s"@enum enum_\1 begin\2end",                              # whole enum construct
    regex_typedef_enum_enumname_values_name => s"@enum \3 begin\2end\nconst enum_\1 = \3",  # whole typedef enum construct with two names
    r"^ +([A-Za-z0-9_]+),?"m => s"  \1",                                                    # simple enum values
  ]
  for substitution in substitutions
    str = replace(str, substitution)
  end
  return str
end

function c2julia(str::String)
  preprocessing = Pair{Regex,Union{SubstitutionString,Function}}[
    r"unsigned int" => s"unsigned_int",                                       # convert `unsigned int` to a single token
    r"unsigned char" => s"unsigned_char",                                     # convert `unsigned char` to a single token
    r" const " => s" ",                                                       # remove all `const`
    r"^//(.*)$"m => s"#\1",                                                   # line comment
    r"/\*(.*?)\*/"s => s"#=\1=#",                                             # block comment
  ]
  for substitution in preprocessing
    str = replace(str, substitution)
  end
  substitutions = Pair{Regex,Union{SubstitutionString,Function}}[
    regex_typedef_struct_fields_name => convert_struct,                       # whole typedef struct construct
    regex_typedef_struct_fields_ptrname => convert_struct,                    # whole typedef struct construct with [1]
    regex_struct_structname_fields => convert_struct,                         # whole struct construct
    regex_typedef_struct_structname_fields_name => convert_struct,            # whole typedef struct construct with two names
    regex_typedef_enum_values_name => convert_enum,                           # whole typedef enum construct
    regex_enum_enumname_values => convert_enum,                               # whole enum construct
    regex_typedef_enum_enumname_values_name => convert_enum,                  # whole typedef enum construct with two names
    r"^typedef +([A-Za-z_]+) +([A-Za-z_]+);"m => s"const \2 = \1",            # simple typedef
    r"^typedef +([A-Za-z_]+) *\* *([A-Za-z_]+);"m => s"const \2 = Ptr{\1}",   # pointer typedef
    r"^typedef +([A-Za-z_]+) +([A-Za-z_]+)\[1\];"m => s"const \2 = Ptr{\1}",  # pointer typedef
    r"^#define +([A-Za-z_]+) +(\d+) *$"m => s"const \1 = \2",                 # defines of integer constants
  ]
  combined_regex = Regex(join(map(re -> re.pattern, first.(substitutions)), "|"), "m")
  output = join(
    map(m -> replace(m.match, substitutions...), eachmatch(combined_regex, str)), "\n\n"
  )
  return output
end

file_header_notice(FLINT_jll_version) = """
# Most of this file is generated from FLINT's header files.
# Do not edit manually, only the corresponding `etc/*_template.jl` file should be edited.

# This file was generated using FLINT_jll v$(FLINT_jll_version).

"""

################################################################################
#
# Main script
#
################################################################################

function expand_template_file(
  infile::String, outfile::String, flintpath::String, file_header::String
)
  @info "Expanding file" infile outfile
  open(outfile, "w") do io
    write(io, file_header)
    write(
      io, expand_templates(read(infile, String), joinpath(flintpath, "include", "flint"))
    )
  end
end

function main()
  flintpath = FLINT_jll.find_artifact_dir()
  flintversion = pkgversion(FLINT_jll)
  @info "Found FLINT" flintpath flintversion

  infile = joinpath(nemopath, "etc", "FlintCTypes_template.jl")
  outfile = joinpath(nemopath, "src", "flint", "FlintCTypes.jl")
  expand_template_file(infile, outfile, flintpath, file_header_notice(flintversion))
end

const nemopath = dirname(dirname(@__FILE__))

@info "Setting up environment"
using Pkg
Pkg.activate(; temp=true)
Pkg.develop(PackageSpec(; path=nemopath))
Pkg.add("FLINT_jll") # version is fixed by Nemo.jl in the line above
using FLINT_jll

main()
