const BEGIN_MARKER = """
################################################################################
#
#  Auto-generated bindings (DO NOT EDIT)
#
################################################################################
"""

const flint_type_to_nemo = Dict{Symbol, Vector{Symbol}}(
  :fmpz => [:ZZRingElem],
  :fmpq => [:QQFieldElem],
  :qqbar => [:QQBarFieldElem]
)

const flint_param_type_to_nemo = Dict{String, Tuple{Symbol, Bool}}(
  "ui" => (:UInt, false),
  "si" => (:Int, false),
  "fmpz" => (:ZZRingElem, true),
  "fmpq" => (:QQFieldElem, true),
)

const type_to_file = Dict{Symbol, String}(
  :ZZRingElem => "flint/fmpz.jl",
  :QQFieldElem => "flint/fmpq.jl",
  :QQBarFieldElem => "calcium/qqbar.jl"
)

function collect_flint_headers(flint_path::String, headersfile::String)
  cmd = pipeline(
    `grep -Pzorh ".. function:: ([0-9a-zA-Z _*(),]+\\n)+[[:blank:]]*\\n" $flint_path/doc/source/`,
    `sed "s/.. function:: //"`,
    `awk '{$1=$1};1'`,
    `tr '\0' '\n'`,
    `sed '/^$/d'`,
    headersfile,
  )
  run(cmd)
end

macro assert_return_type(return_type::Symbol)
  return :($(esc(:(sig.return_type))) == $(string(return_type)) || print_error("Expected $($(esc(:(sig.function_name)))) should return void"))
end

macro assert_num_parameters(n_params::Int)
  return :(length($(esc(:(sig.parameters)))) == $(n_params) || print_error("Expected $($(esc(:(sig.function_name)))) to have $(n_params) parameters"))
end

function print_error(msg::String)
  println(stderr, "ERROR: $msg")
end

function write_to_files(generated_content::Dict{String, Vector{String}})
  for (path, content) in generated_content
    open("src/$path", "r+") do file
      readuntil(file, BEGIN_MARKER)
      if eof(file)
        print(file, '\n', BEGIN_MARKER)
      end
      println(file)
      println(file, join(content, "\n\n"))
      truncate(file, position(file))
    end
  end
end

struct CFunctionSignature
  return_type::String
  function_name::String
  parameters::Vector{Tuple{String, String}}
end

function Base.show(io::IO, sig::CFunctionSignature)
  print(io, "$(sig.return_type) $(sig.function_name)($(join((join(p, " ") for p in sig.parameters), ", ")))")
end

function parse_line(line::String)
  # Thanks to ChatGPT for coming up with the regexes
  function_regex = r"(?<return_type>[\w\s\*]+)\s+(?<function_name>\w+)\s*\((?<parameters>[^\)]*)\)"
  param_regex = r"\s*(?<param_type>[^\s,]+\s*[\*\[\]\w\s]*)\s+(?<param_name>\w+)"

  m = match(function_regex, line)
  if m === nothing 
    print_error("Could not parse line: $line") 
    return nothing
  end
  return CFunctionSignature(m["return_type"], m["function_name"], [(pm["param_type"], pm["param_name"]) for pm in eachmatch(param_regex, m["parameters"])])
end

function maybe_generate_content(sig::CFunctionSignature, content_per_file::Dict{String, Vector{String}})
  for (flint_type, nemo_types) in flint_type_to_nemo
    maybe_generate_neg!(sig, flint_type, nemo_types, content_per_file)
    maybe_generate_add!(sig, flint_type, nemo_types, content_per_file)
    maybe_generate_sub!(sig, flint_type, nemo_types, content_per_file)
    maybe_generate_mul!(sig, flint_type, nemo_types, content_per_file)
  end
end

function maybe_generate_neg!(sig::CFunctionSignature, flint_type::Symbol, nemo_types::Vector{Symbol}, content_per_file::Dict{String, Vector{String}})
  sig.function_name == "$(flint_type)_neg" || return

  @assert_return_type void
  @assert_num_parameters 2
  @debug sig
  for nemo_type in nemo_types
    content = get!(content_per_file, type_to_file[nemo_type], String[])
    push!(content,
      """function neg!(z::$(nemo_type)OrPtr, x::$(nemo_type)OrPtr)
        ccall((:$(sig.function_name), libflint), Nothing, (Ref{$(nemo_type)}, Ref{$(nemo_type)}), z, x)
        return z
      end""")
  end
end

for (func, other_order_body) in [
  ("add", "add!(z, y, x)"),
  ("sub", "neg!(sub!(z, y, x))"),         # TODO: use functions like `qqbar_si_sub`
  ("mul", "mul!(z, y, x)"),
]
  @eval function $(Symbol("maybe_generate_$(func)!"))(
    sig::CFunctionSignature,
    flint_type::Symbol,
    nemo_types::Vector{Symbol},
    content_per_file::Dict{String,Vector{String}},
  )
    startswith(sig.function_name, "$(flint_type)_$($func)") || return nothing

    matched = false
    if sig.function_name == "$(flint_type)_$($func)"
      @assert_return_type void
      @assert_num_parameters 3
      @debug sig
      for nemo_type in nemo_types
        content = get!(content_per_file, type_to_file[nemo_type], String[])
        push!(content,
          """function $($func)!(z::$(nemo_type)OrPtr, x::$(nemo_type)OrPtr, y::$(nemo_type)OrPtr)
            ccall((:$(sig.function_name), libflint), Nothing, (Ref{$(nemo_type)}, Ref{$(nemo_type)}, Ref{$(nemo_type)}), z, x, y)
            return z
          end""")
      end
    else
      l = length("$(flint_type)_$($func)")
      sig.function_name[l+1] == '_' || return
      rest = sig.function_name[l+2:end]
      haskey(flint_param_type_to_nemo, rest) || return
      param_abbrev = rest
      (param_type, param_is_nemo) = flint_param_type_to_nemo[param_abbrev]
      @assert_return_type void
      @assert_num_parameters 3
      @debug sig
      for nemo_type in nemo_types
        content = get!(content_per_file, type_to_file[nemo_type], String[])
        push!(content,
          """function $($func)!(z::$(nemo_type)OrPtr, x::$(nemo_type)OrPtr, y::$(param_is_nemo ? "$(param_type)OrPtr" : "$(param_type)"))
            ccall((:$(sig.function_name), libflint), Nothing, (Ref{$(nemo_type)}, Ref{$(nemo_type)}, $(param_is_nemo ? "Ref{$(param_type)}" : "$(param_type)")), z, x, y)
            return z
          end""")
        push!(content,
          """$($func)!(z::$(nemo_type)OrPtr, x::$(param_is_nemo ? "$(param_type)OrPtr" : "$(param_type)"), y::$(nemo_type)OrPtr) = $($other_order_body)""")
      end
    end
  end
end

function main(args::Vector{String})
  length(args) == 1 || error("One argument containing the path to FLINT required.")
  flint_path = args[1]
  headersfile = tempname()

  collect_flint_headers(flint_path, headersfile)

  content_per_file = Dict{String, Vector{String}}()

  for line in eachline(headersfile)
    sig = parse_line(line)
    isnothing(sig) && continue
    maybe_generate_content(sig, content_per_file)
  end

  write_to_files(content_per_file)
end

main(ARGS)
