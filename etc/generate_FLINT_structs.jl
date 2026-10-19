VERSION >= v"1.10" || error("This script requires Julia 1.10 or later")

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
    # Convert to julia
    translated_content = c2julia(content)
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

const regex_typedef_struct_fields_name = r"^typedef struct\s*\{((?:[^{}]|\{[^{}]*\})+)\}\s*([A-Za-z0-9_]+);"m
const regex_typedef_struct_fields_refname = r"^typedef struct\s*\{((?:[^{}]|\{[^{}]*\})+)\}\s*([A-Za-z0-9_]+)\[1\];"m
const regex_struct_structname_fields = r"^struct *([A-Za-z0-9_]+)\s*\{((?:[^{}]|\{[^{}]*\})+)\}\s*;"m
const regex_struct_structname = r"^struct *([A-Za-z0-9_]+);"m
const regex_typedef_struct_structname_fields_name = r"^typedef struct *([A-Za-z0-9_]+)\s*\{((?:[^{}]|\{[^{}]*\})+)\}\s*([A-Za-z0-9_]+);"m

const regex_typedef_enum_values_name = r"^typedef enum\s*\{([^{}]+)\}\s*([A-Za-z0-9_]+);"m
const regex_enum_enumname_values = r"^enum *([A-Za-z0-9_]+)\s*\{([^{}]+)\}\s*;"m
const regex_typedef_enum_enumname_values_name = r"^typedef enum *([A-Za-z0-9_]+)\s*\{([^{}]+)\}\s*([A-Za-z0-9_]+);"m

const regex_typedef_union_values_name = r"^typedef union\s*\{([^{}]+)\}\s*([A-Za-z0-9_]+);"m
const regex_union_unionname_values = r"^union *([A-Za-z0-9_]+)\s*\{([^{}]+)\}\s*;"m
const regex_typedef_union_unionname_values_name = r"^typedef union *([A-Za-z0-9_]+)\s*\{([^{}]+)\}\s*([A-Za-z0-9_]+);"m

# `union { T1 a; T2 b; } name;` inside a struct
function convert_inline_union(str::AbstractString)
  m = match(r"^ +union\s*\{([^{}]+)\}\s*([A-Za-z0-9_]+);"m, str)
  types = [isempty(t.captures[2]) ? t.captures[1] : "Ptr{$(t.captures[1])}" for t in eachmatch(r"^ +([A-Za-z0-9_]+) *(\**) *([A-Za-z0-9_]+);"m, m.captures[1])]
  return "  $(m.captures[2])::NTuple{cld(maximum(sizeof, ($(join(types, ", ")),)), sizeof(UInt)), UInt}"
end

function convert_struct(str::AbstractString)
  str = replace(str, r"^ +union\s*\{[^{}]+\}\s*[A-Za-z0-9_]+;"m => convert_inline_union)
  substitutions = Pair{Regex,Union{SubstitutionString,Function}}[
    regex_typedef_struct_fields_name => s"struct \2 <: FlintStruct\1end",                                       # whole typedef struct construct
    regex_struct_structname_fields => s"struct struct_\1 <: FlintStruct\2end",                                  # whole struct construct
    regex_struct_structname => s"struct struct_\1 <: FlintStruct end",                                           # whole struct construct without fields
    regex_typedef_struct_fields_refname => s"struct struct_\2 <: FlintStruct\1end\nconst \2 = Tuple{struct_\2}", # whole typedef struct singleton array construct
    regex_typedef_struct_structname_fields_name => s"struct \3 <: FlintStruct\2end\nconst struct_\1 = \3",       # whole typedef struct construct with two names
    r"^ +([A-Za-z0-9_]+) +([A-Za-z0-9_]+);"m => s"  \2::\1",                                      # simple fields (one to five declared on one line)
    r"^ +([A-Za-z0-9_]+) +([A-Za-z0-9_]+) *, *([A-Za-z0-9_]+);"m => s"  \2::\1\n  \3::\1",
    r"^ +([A-Za-z0-9_]+) +([A-Za-z0-9_]+) *, *([A-Za-z0-9_]+) *, *([A-Za-z0-9_]+);"m => s"  \2::\1\n  \3::\1\n  \4::\1",
    r"^ +([A-Za-z0-9_]+) +([A-Za-z0-9_]+) *, *([A-Za-z0-9_]+) *, *([A-Za-z0-9_]+) *, *([A-Za-z0-9_]+);"m => s"  \2::\1\n  \3::\1\n  \4::\1\n  \5::\1",
    r"^ +([A-Za-z0-9_]+) +([A-Za-z0-9_]+) *, *([A-Za-z0-9_]+) *, *([A-Za-z0-9_]+) *, *([A-Za-z0-9_]+) *, *([A-Za-z0-9_]+);"m => s"  \2::\1\n  \3::\1\n  \4::\1\n  \5::\1\n  \6::\1",
    r"^ +([A-Za-z0-9_]+) *\* *([A-Za-z0-9_]+);"m => s"  \2::Ptr{\1}",                             # pointer field (one to five declared on one line)
    r"^ +([A-Za-z0-9_]+) *\* *([A-Za-z0-9_]+) *, *\* *([A-Za-z0-9_]+);"m => s"  \2::Ptr{\1}\n  \3::Ptr{\1}",
    r"^ +([A-Za-z0-9_]+) *\* *([A-Za-z0-9_]+) *, *\* *([A-Za-z0-9_]+) *, *\* *([A-Za-z0-9_]+);"m => s"  \2::Ptr{\1}\n  \3::Ptr{\1}\n  \4::Ptr{\1}",
    r"^ +([A-Za-z0-9_]+) *\* *([A-Za-z0-9_]+) *, *\* *([A-Za-z0-9_]+) *, *\* *([A-Za-z0-9_]+) *, *\* *([A-Za-z0-9_]+);"m => s"  \2::Ptr{\1}\n  \3::Ptr{\1}\n  \4::Ptr{\1}\n  \5::Ptr{\1}",
    r"^ +([A-Za-z0-9_]+) *\* *([A-Za-z0-9_]+) *, *\* *([A-Za-z0-9_]+) *, *\* *([A-Za-z0-9_]+) *, *\* *([A-Za-z0-9_]+) *, *\* *([A-Za-z0-9_]+);"m => s"  \2::Ptr{\1}\n  \3::Ptr{\1}\n  \4::Ptr{\1}\n  \5::Ptr{\1}\n  \6::Ptr{\1}",
    r"^ +([A-Za-z0-9_]+) *\* *\* *([A-Za-z0-9_]+);"m => s"  \2::Ptr{Ptr{\1}}",                    # double pointer field
    r"^ +([A-Za-z0-9_]+) +([A-Za-z0-9_]+)\[([A-Za-z0-9_+*/ ()-]+)\];"m => s"  \2::NTuple{\3, \1}", # fixed len array field
    r"^ +[A-Za-z0-9_]+ *\( *\* *([A-Za-z0-9_]+) *\) *\([A-Za-z0-9_, *]+\);"m => s"  \1::Ptr{Cvoid}",  # function pointer field
    r"^ +struct *([A-Za-z0-9_]+) +([A-Za-z0-9_]+);"m => s"  \2::struct_\1",                       # struct field (without typedef)
    r"^ +enum *([A-Za-z0-9_]+) +([A-Za-z0-9_]+);"m => s"  \2::enum_\1",                           # enum field (without typedef)
  ]
  for substitution in substitutions
    str = replace(str, substitution)
  end
  return str
end

function convert_enum(str::AbstractString)
  substitutions = Pair{Regex,Union{SubstitutionString,Function}}[
    regex_typedef_enum_values_name => s"@enum \2 begin\1end",                               # whole typedef enum construct
    regex_enum_enumname_values => s"@enum enum_\1 begin\2end",                              # whole enum construct
    regex_typedef_enum_enumname_values_name => s"@enum \3 begin\2end\nconst enum_\1 = \3",  # whole typedef enum construct with two names
    r"^ +([A-Za-z0-9_]+)( *= *[0-9]+)?,?"m => s"  \1\2",                                      # enum values, optionally with explicit value
  ]
  for substitution in substitutions
    str = replace(str, substitution)
  end
  return str
end

function convert_union(str::AbstractString)
  substitutions = Pair{Regex,Union{SubstitutionString,Function}}[
    regex_typedef_union_values_name => s"struct \2 <: FlintStruct\n  uniondata::NTuple{cld(maximum(sizeof, (\1  )), sizeof(UInt)), UInt}\nend",                                 # whole typedef union construct
    regex_union_unionname_values => s"struct union_\1 <: FlintStruct\n  uniondata::NTuple{cld(maximum(sizeof, (\2  )), sizeof(UInt)), UInt}\nend",                             # whole union construct
    regex_typedef_union_unionname_values_name => s"struct \3 <: FlintStruct\n  uniondata::NTuple{cld(maximum(sizeof, (\2  )), sizeof(UInt)), UInt}\nend\nconst union_\1 = \3", # whole typedef union construct with two names
    r"^ +([A-Za-z0-9_]+) *\* *([A-Za-z0-9_]+);"m => s"    Ptr{\1},",  # pointer member
    r"^ +([A-Za-z0-9_]+) +([A-Za-z0-9_]+);"m => s"    \1,",
  ]
  for substitution in substitutions
    str = replace(str, substitution)
  end
  return str
end

# `ulong` and `slong` are platform dependent in flint.h and defined by hand in the template
const ignored_constructs = r"^typedef .* (ulong|slong);$|^struct [A-Za-z0-9_]+ ?\*[A-Za-z0-9_]+\("

# Every type construct in the header must be covered by some match,
# otherwise a FLINT change would silently drop a type.
function check_all_constructs_matched(str::String, matches::Vector{RegexMatch})
  covered = [m.offset:(m.offset + ncodeunits(m.match) - 1) for m in matches]
  unmatched = String[]
  for m in eachmatch(r"^(typedef|struct|enum|union)\b.*$"m, str)
    occursin(ignored_constructs, m.match) && continue
    any(r -> m.offset in r, covered) || push!(unmatched, m.match)
  end
  isempty(unmatched) || error("Unhandled constructs:\n" * join(unmatched, "\n"))
end

function c2julia(str::String)
  preprocessing = Pair{Regex,String}[
    r"^//(.*)$"m => "",                     # remove line comment
    r"/\*(.*?)\*/"s => "",                  # remove block comment
    r"^#(if|ifdef|ifndef|elif|else|endif)\b.*$"m => "", # remove preprocessor conditionals
    r" const " => " ",                      # remove all `const`
    r"\bvoid\b" => "Cvoid",                 # replace `void` C type
    r"\bunsigned int\b" => "Cuint",         # replace `unsigned int` C type
    r"\bint\b" => "Cint",                    # replace `int` C type
    r"\bunsigned char\b" => "Cuchar",       # replace `unsigned char` C type
    r"\bchar\b" => "Cchar",                 # replace `char` C type
    r"\bdouble\b" => "Cdouble",             # replace `double` C type
  ]
  for substitution in preprocessing
    str = replace(str, substitution)
  end
  substitutions = Pair{Regex,Union{SubstitutionString,Function}}[
    regex_typedef_struct_fields_name => convert_struct,                       # whole typedef struct construct
    regex_typedef_struct_fields_refname => convert_struct,                    # whole typedef struct singleton array construct
    regex_struct_structname_fields => convert_struct,                         # whole struct construct
    regex_struct_structname => convert_struct,                                # whole struct construct without fields
    regex_typedef_struct_structname_fields_name => convert_struct,            # whole typedef struct construct with two names
    regex_typedef_enum_values_name => convert_enum,                           # whole typedef enum construct
    regex_enum_enumname_values => convert_enum,                               # whole enum construct
    regex_typedef_enum_enumname_values_name => convert_enum,                  # whole typedef enum construct with two names
    regex_typedef_union_values_name => convert_union,                         # whole typedef union construct
    regex_union_unionname_values => convert_union,                            # whole union construct
    regex_typedef_union_unionname_values_name => convert_union,               # whole typedef union construct with two names
    r"^typedef +([A-Za-z_]+) +([A-Za-z_]+);"m => s"const \2 = \1",            # simple typedef
    r"^typedef +([A-Za-z_]+) *\* *([A-Za-z_]+);"m => s"const \2 = Ptr{\1}",   # pointer typedef
    r"^typedef +([A-Za-z_]+) +([A-Za-z_]+)\[1\];"m => s"const \2 = Tuple{\1}",  # singleton array typedef
    r"^typedef +struct +([A-Za-z_]+) +([A-Za-z_]+);"m => s"const \2 = struct_\1", # struct typedef
    r"^typedef +enum +([A-Za-z_]+) +([A-Za-z_]+);"m => s"const \2 = enum_\1",   # enum typedef
    r"^typedef +union +([A-Za-z_]+) +([A-Za-z_]+);"m => s"const \2 = union_\1", # union typedef
    r"^typedef +[A-Za-z0-9_]+ *\( *\* *([A-Za-z0-9_]+) *\) *\([A-Za-z0-9_, *\n]+\);"m => s"const \1 = Ptr{Cvoid}", # function pointer typedef, may span lines
    r"^#define +([A-Za-z_]+) +(\d+) *$"m => s"const \1 = \2",                 # defines of integer constants
    r"^#define +([A-Za-z_]+) +\(([A-Za-z0-9+*() ]+)\) *$"m => s"const \1 = \2",   # defines of more complex constants
  ]
  combined_regex = Regex(join(map(re -> re.pattern, first.(substitutions)), "|"), "m")
  matches = collect(eachmatch(combined_regex, str))
  check_all_constructs_matched(str, matches)
  output = join(map(m -> replace(m.match, substitutions...), matches), "\n\n")
  output = replace(output, r"\h*\n" => "\n") # remove trailing whitespace
  output = replace(output, r"NTuple\{([^,}]*)/([^,}]*),"  => s"NTuple{\1÷\2,") # integer division in array lengths
  # a C statement that survived translation would be a syntax error in Julia
  for m in eachmatch(r"^.*;\h*$"m, output)
    error("Untranslated line: $(m.match)")
  end
  return output
end

file_header_notice(FLINT_jll_version) = """
# Most of this file is generated from FLINT's header files.
# Do not edit manually, only the corresponding `etc/*_template.jl` file should be edited.
#
# It mirrors FLINT internals, so it is not covered by Nemo's SemVer guarantees
# and may change in any release.

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
  # translate first, so a failure leaves the old file untouched
  output = expand_templates(read(infile, String), joinpath(flintpath, "include", "flint"))
  open(outfile, "w") do io
    write(io, file_header)
    write(io, output)
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
ENV["JULIA_PKG_PRECOMPILE_AUTO"]=0
Pkg.activate(; temp=true)
Pkg.develop(PackageSpec(; path=nemopath))
Pkg.add("FLINT_jll") # version is fixed by Nemo.jl in the line above
using FLINT_jll

main()
