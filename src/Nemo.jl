module Nemo

using AbstractAlgebra

using Markdown

using InteractiveUtils

using Libdl

using Random

import Base: Array, abs, acos, acosh, asin, asinh, atan, atanh,
             bin, binomial, ceil, checkbounds, conj, convert, cmp, cos, cosh,
             cospi, cot, coth, dec, deepcopy, deepcopy_internal, denominator,
             div, divrem, expm1, exp, factorial, floor, gcd, gcdx, getindex,
             hash, hcat, hex, hypot, intersect, inv, invmod, isequal, iseven,
             isfinite, isinteger, isless, isodd, isqrt, isreal, iszero, lcm,
             ldexp, length,
             log, log1p, mod, ndigits, numerator, oct, one, parent, parse,
             precision,
             rand, Rational, rem, reverse,
             setindex!, show, similar, sign, sin, sinh, sinpi, size, sqrt, string,
             tan, tanh, trailing_zeros, transpose, truncate,
             typed_hvcat, typed_hcat, vcat, xor, zero, zeros, +, -, *, ==, ^,
             &, |, <<, >>, ~, <=, >=, <, >, //, /, !=

import LinearAlgebra: det, norm, nullspace, rank, transpose!, hessenberg, tr,
                      lu, lu!, eigvals

import AbstractAlgebra: nullspace, @show_name, @show_special, 
                        find_name, get_special, set_special, @declare_other, @show_special_elem

# We don't want the QQ, ZZ, FiniteField, NumberField from AbstractAlgebra
# as they are for parents of Julia types or naive implementations
# We only import AbstractAlgebra, not export
# We do not want the AbstractAlgebra version of certain functions as the Base version
# is the only place user friendly versions are defined
# AbstractAlgebra/Nemo has its own promote_rule, distinct from Base
# Set, Module, Ring, Group and Field are too generic to pollute the users namespace with
for i in names(AbstractAlgebra)
   i in AbstractAlgebra.import_exclude && continue
   i == :GF && continue
   eval(Meta.parse("import AbstractAlgebra." * string(i)))
   eval(Expr(:export, i))
end

export GF

import AbstractAlgebra: Set, Module, Ring, Group, Field, promote_rule

export flint_cleanup, flint_set_num_threads

export error_dim_negative, ErrorConstrDimMismatch

export iswindows64

export CyclotomicField, MaximalRealSubfield, NumberField, ComplexField, PadicField,
       QadicField

# Things/constants which are also defined in AbstractAlgebra:
export ZZ, QQ, RealField, FiniteField, NumberField

if VERSION >= v"0.6.0-dev.2024" # julia started exporting iszero (again?)
   import Base: iszero
end

if VERSION >= v"0.7.0-DEV.264" # julia started exporting sincos
   import Base: sincos
end

if VERSION >= v"0.7.0-DEV.1144"
    import Base: isone
end

###############################################################################
#
#   Set up environment / load libraries
#
###############################################################################

iswindows64() = (Sys.iswindows() ? true : false) && (Int == Int64)

const pkgdir = realpath(joinpath(dirname(@__DIR__)))
const libdir = joinpath(pkgdir, "deps", "usr", "lib")
const bindir = joinpath(pkgdir, "deps", "usr", "bin")
if Sys.iswindows()
   const libgmp = joinpath(pkgdir, "deps", "usr", "bin", "libgmp-10")
   const libmpfr = joinpath(pkgdir, "deps", "usr", "bin", "libmpfr-6")
   const libflint = joinpath(pkgdir, "deps", "usr", "bin", "libflint")
   const libarb = joinpath(pkgdir, "deps", "usr", "bin", "libarb")
   const libantic = joinpath(pkgdir, "deps", "usr", "bin", "libantic")

else
   const libgmp = joinpath(pkgdir, "deps", "usr", "lib", "libgmp")
   const libmpfr = joinpath(pkgdir, "deps", "usr", "lib", "libmpfr")
   const libflint = joinpath(pkgdir, "deps", "usr", "lib", "libflint")
   const libarb = joinpath(pkgdir, "deps", "usr", "lib", "libarb")
   const libantic = joinpath(pkgdir, "deps", "usr", "lib", "libantic")
end

function flint_abort()
  error("Problem in the Flint-Subsystem")
end

################################################################################
#
#  Debugging tools for allocation tracking
#
################################################################################

active_mem = Dict{UInt, Tuple{Symbol, UInt, Any}}()

function trace_malloc(n::UInt)
  u = ccall(:jl_malloc, UInt, (UInt, ), n)
  global active_mem
  active_mem[u] = (:malloc, n, backtrace())
  return u
end

function trace_calloc(n::UInt, s::UInt)
  u = ccall(:jl_calloc, UInt, (UInt, UInt), n, s)
  global active_mem
  active_mem[u] = (:calloc, n*s, backtrace())
  return u
end

function trace_free(n::UInt)
  global active_mem
#  @assert haskey(active_mem, n)
  delete!(active_mem, n)
  ccall(:jl_free, Nothing, (UInt, ), n)
end

function trace_realloc(n::UInt, s::UInt)
  global active_mem
  p = ccall(:jl_realloc, UInt, (UInt, UInt), n, s)
#  @assert haskey(active_mem, n)
  delete!(active_mem, n)
  active_mem[p] = (:realloc, s, backtrace())
  return p
end

function trace_counted_malloc(n::UInt)
  global active_mem
  p = ccall(:jl_gc_counted_malloc, UInt, (UInt, ), n)
  active_mem[p] = (:counted_malloc, n, backtrace())
  return p
end

function trace_counted_realloc(n::UInt, m::UInt, o::UInt)
  global active_mem
  p = ccall(:jl_gc_counted_realloc_with_old_size, UInt, (UInt, UInt, UInt), n, m, o)
#  @assert n==0 || haskey(active_mem, n)
  delete!(active_mem, n)
  active_mem[p] = (:counted_realloc, o, backtrace())
  return p
end

function trace_counted_free(n::UInt, s::UInt)
  global active_mem
#  @assert haskey(active_mem, n)
  delete!(active_mem, n)
  ccall(:jl_gc_counted_free_with_size, Nothing, (UInt, UInt), n, s)
end

function show_active(l::UInt = UInt(0), frames::Int = 2)
  global active_mem
  for i = keys(active_mem)
    v = active_mem[i]
    if v[2] >= l
      n = min(frames, length(v[3]))
      Base.show_backtrace(stdout, v[3][1:n])
    end
  end
end

function trace_memory(b::Bool)
  if Sys.iswindows()
    return
  end
  if b
    ccall((:__gmp_set_memory_functions, libgmp), Nothing,
       (Ptr{Nothing},Ptr{Nothing},Ptr{Nothing}),
       @cfunction(trace_counted_malloc, UInt, (UInt, )),
       @cfunction(trace_counted_realloc, UInt, (UInt, UInt, UInt)),
       @cfunction(trace_counted_free, Nothing, (UInt, UInt)))

    ccall((:__flint_set_memory_functions, libflint), Nothing,
       (Ptr{Nothing},Ptr{Nothing},Ptr{Nothing},Ptr{Nothing}),
       @cfunction(trace_malloc, UInt, (UInt, )),
       @cfunction(trace_calloc, UInt, (UInt, UInt)),
       @cfunction(trace_realloc, UInt, (UInt, UInt)),
       @cfunction(trace_free, Nothing, (UInt, )))
  else
    ccall((:__gmp_set_memory_functions, libgmp), Nothing,
       (Ptr{Nothing},Ptr{Nothing},Ptr{Nothing}),
       cglobal(:jl_gc_counted_malloc),
       cglobal(:jl_gc_counted_realloc_with_old_size),
       cglobal(:jl_gc_counted_free_with_size))

    ccall((:__flint_set_memory_functions, libflint), Nothing,
       (Ptr{Nothing},Ptr{Nothing},Ptr{Nothing},Ptr{Nothing}),
       cglobal(:jl_malloc),
       cglobal(:jl_calloc),
       cglobal(:jl_realloc),
       cglobal(:jl_free))
  end
end

################################################################################
#
#  Initialization function
#
################################################################################

const __isthreaded = Ref(false)

function __init__()
   # In case libgmp picks up the wrong libgmp later on, we "unset" the jl_*
   # functions from the julia :libgmp.

   __isthreaded[] = get(ENV, "NEMO_THREADED", "") == "1"

   if __isthreaded[]
      ccall((:__gmp_set_memory_functions, :libgmp), Nothing,
            (Int, Int, Int), 0, 0, 0)
   end

   if "HOSTNAME" in keys(ENV) && ENV["HOSTNAME"] == "juliabox"
       push!(Libdl.DL_LOAD_PATH, "/usr/local/lib")
   elseif Sys.islinux()
       push!(Libdl.DL_LOAD_PATH, libdir)
       Libdl.dlopen(libgmp)
       Libdl.dlopen(libmpfr)
       Libdl.dlopen(libflint)
       Libdl.dlopen(libarb)
       Libdl.dlopen(libantic)
   else
      push!(Libdl.DL_LOAD_PATH, libdir)
      push!(Libdl.DL_LOAD_PATH, bindir)
   end

   if !Sys.iswindows()
     if !__isthreaded[]
         ccall((:__gmp_set_memory_functions, libgmp), Nothing,
            (Ptr{Nothing},Ptr{Nothing},Ptr{Nothing}),
            cglobal(:jl_gc_counted_malloc),
            cglobal(:jl_gc_counted_realloc_with_old_size),
            cglobal(:jl_gc_counted_free_with_size))

         ccall((:__flint_set_memory_functions, libflint), Nothing,
            (Ptr{Nothing},Ptr{Nothing},Ptr{Nothing},Ptr{Nothing}),
            cglobal(:jl_malloc),
            cglobal(:jl_calloc),
            cglobal(:jl_realloc),
            cglobal(:jl_free))
      end
   end

   ccall((:flint_set_abort, libflint), Nothing,
         (Ptr{Nothing},), @cfunction(flint_abort, Nothing, ()))

   if isinteractive() &&
         !any(x -> x.name in ("Hecke", "Oscar"), keys(Base.package_locks)) &&
         get(ENV, "NEMO_PRINT_BANNER", "true") != "false"

      println("")
      println("Welcome to Nemo version $(version())")
      println("")
      println("Nemo comes with absolutely no warranty whatsoever")
      println("")
   end

  t = create_accessors(AnticNumberField, Dict, get_handle())

  global _get_Special_of_nf = t[1]
  global _set_Special_of_nf = t[2]

  # Initialize the thread local random state
  resize!(_flint_rand_states, Threads.nthreads())
  for i in 1:Threads.nthreads()
     _flint_rand_states[i] = rand_ctx()
  end

  # Initialize the thread local ECM parameters
  Threads.resize_nthreads!(_ecm_B1s)
  Threads.resize_nthreads!(_ecm_nCs)
end

function flint_set_num_threads(a::Int)
   if !__isthreaded[]
     error("To use threaded flint, julia has to be started with NEMO_THREADED=1")
   else
     ccall((:flint_set_num_threads, libflint), Nothing, (Int,), a)
   end
end

function flint_cleanup()
   ccall((:flint_cleanup, libflint), Nothing, ())
end

###############################################################################
#
#  Version information
#
################################################################################

version() = v"0.16.5-dev"

function versioninfo()
  print("Nemo version $(version())\n")
  nemorepo = dirname(dirname(@__FILE__))

  print("Nemo: ")
  prepo = Base.LibGit2.GitRepo(nemorepo)
  Base.LibGit2.with(LibGit2.head(prepo)) do phead
    print("commit: ")
    print(string(LibGit2.Oid(phead))[1:8])
    print(" date: ")
    commit = Base.LibGit2.get(Base.LibGit2.GitCommit, prepo, LibGit2.Oid(phead))
    print(Base.Dates.unix2datetime(Base.LibGit2.author(commit).time))
    print(")\n")
  end

  finalize(prepo)

  for deps in ["flint2", "arb", "antic"]
    if ispath(joinpath(nemorepo, "deps", deps))
      print("$deps: ")
      repo = joinpath(nemorepo, "deps", deps)

      prepo = Base.LibGit2.GitRepo(repo)

      Base.LibGit2.with(LibGit2.head(prepo)) do phead
        print("commit: ")
        print(string(LibGit2.Oid(phead))[1:8])
        print(" date: ")
        commit = Base.LibGit2.get(Base.LibGit2.GitCommit, prepo, LibGit2.Oid(phead))
        print(Base.Dates.unix2datetime(Base.LibGit2.author(commit).time))
        print(")\n")
      end
      finalize(prepo)
    end
  end

  return nothing
end

###############################################################################
#
#   Generic submodule
#
###############################################################################

export PowerSeriesRing, PolynomialRing, SparsePolynomialRing, MatrixSpace,
       FractionField, ResidueRing, Partition, PermGroup, YoungTableau,
       AllParts, SkewDiagram, AllPerms, Perm, LaurentSeriesRing,
       LaurentSeriesField, PuiseuxSeriesRing, ResidueField

export Generic

###############################################################################
#
#   Load Nemo Rings/Fields/etc
#
###############################################################################

include("embedding/EmbeddingTypes.jl")

include("flint/FlintTypes.jl")

include("antic/AnticTypes.jl")

include("arb/ArbTypes.jl")

#include("ambiguities.jl") # remove ambiguity warnings

include("flint/adhoc.jl")

include("embedding/embedding.jl")

include("Rings.jl")

include("common.jl")

################################################################################
#
#  Thread local storages
#
################################################################################

const _flint_rand_states = rand_ctx[]

# Data from http://www.mersennewiki.org/index.php/Elliptic_Curve_Method
const _ecm_B1 = Int[2, 11, 50, 250, 1000, 3000, 11000, 43000, 110000, 260000, 850000, 2900000];
const _ecm_nC = Int[25, 90, 300, 700, 1800, 5100, 10600, 19300, 49000, 124000, 210000, 340000];

const _ecm_B1s = Vector{Int}[_ecm_B1]
const _ecm_nCs = Vector{Int}[_ecm_nC]

###############################################################################
#
#   Set domain for ZZ, QQ, PadicField, FiniteField to Flint
#
###############################################################################

const ZZ = FlintZZ
const QQ = FlintQQ
const PadicField = FlintPadicField
const QadicField = FlintQadicField
const FiniteField = FlintFiniteField

###############################################################################
#
#   Set domain for RR, CC to Arb
#
###############################################################################

const RealField = ArbField
const ComplexField = AcbField

###############################################################################
#
#   Test code
#
###############################################################################

include("../benchmarks/runbenchmarks.jl")

function test_module(x, y)
   julia_exe = Base.julia_cmd()
   test_file = joinpath(pkgdir, "test/$x/")
   test_file = test_file * "$y-test.jl";
   test_function_name = "test_"

   if x in ["flint", "arb", "antic"]
     test_function_name *= y
   else x == "generic"
     if y == "RelSeries"
       test_function_name *= "gen_rel_series"
     elseif y == "AbsSeries"
       test_function_name *= "gen_abs_series"
     elseif y == "Matrix"
       test_function_name *= "gen_mat"
     elseif y == "Fraction"
       test_function_name *= "gen_frac"
     elseif y == "Residue"
       test_function_name *= "gen_res"
     else
       test_function_name *= "gen_$(lowercase(y))"
     end
   end

   cmd = "using Test; using Nemo; include(\"$test_file\"); $test_function_name();"
   println("spawning ", `$julia_exe -e \"$cmd\"`)
   run(`$julia_exe -e $cmd`)
end

end # module
