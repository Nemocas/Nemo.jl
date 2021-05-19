@static if VERSION < v"1.3.0"

  using Pkg, BinaryProvider

   # This does not work on julia >= 1.3, but there we use the *jll package anyway.
  flint_ver = Pkg.API.__installed(PKGMODE_MANIFEST)["FLINT_jll"]

  # Parse some basic command-line arguments
  const verbose = "--verbose" in ARGS
  const prefix = Prefix(get([a for a in ARGS if a != "--verbose"], 1, joinpath(@__DIR__, "usr")))

  products = [ ]

  dependencies = [
  "build_GMP.v6.1.2.jl",
  "build_MPFR.v4.0.2.jl",
  ]

  @show flint_ver
  if flint_ver == v"200.700.0+0"
    push!(dependencies, "build_FLINT.v200.700.0.jl")
  elseif flint_ver == v"200.700.100+0"
    push!(dependencies, "build_FLINT.v200.700.100.0.jl")
  else
    throw(error("Flint version $flint_ver not supported for julia version <= 1.3"))
  end

  append!(dependencies, [
  "build_Arb.v200.1900.0.jl",
  "build_Antic.v0.200.400.jl",
  "build_Calcium.v0.300.0.jl",
  ])

  for file in dependencies
       build_file = joinpath(@__DIR__, file)
       m = @eval module $(gensym()); include($build_file); end
       if !occursin("FLINT", file) && !occursin("GMP", file) && !occursin("MPFR", file)
         append!(products, m.products)
       end
  end

  # Finally, write out a deps.jl file
  write_deps_file(joinpath(@__DIR__, "deps.jl"), Array{Product,1}(products), verbose=verbose)

end # VERSION
