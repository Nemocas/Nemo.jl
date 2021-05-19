@doc Markdown.doc"""
  Nemo is a computer algebra package for the Julia programming language, maintained by William Hart, Tommy Hofmann, Claus Fieker and Fredrik Johansson with additional code by Oleksandr Motsak and other contributors.

 The Nemo code written in Julia is licensed under the BSD license and it makes use of GPL and LGPL C/C++ libraries such as Flint, Antic, GMP/MPIR, MPFR, Singular and Arb.

 For more information please visit: https://nemocas.org
"""
module Nemo

using AbstractAlgebra

using Markdown

using InteractiveUtils

using Libdl

using Random
using Random: SamplerTrivial

using RandomExtensions: RandomExtensions, make, Make2, Make3

using LoadFlint

using Pkg

import SHA

import AbstractAlgebra: div, divrem

# N.B: do not import div, divrem from Base
import Base: Array, abs, acos, acosh, asin, asinh, atan, atanh, bin, binomial,
             ceil, checkbounds, conj, convert, cmp, cos, cosh, cospi, cot,
             coth, dec, deepcopy, deepcopy_internal, denominator,
             expm1, exp, factorial, floor, gcd, gcdx, getindex, hash, hcat,
             hex, hypot, intersect, inv, invmod, isequal, iseven, isfinite,
             isinteger, isless, isodd, isone, isqrt, isreal, iszero, lcm,
             ldexp, length, log, log1p, mod, ndigits, numerator, oct, one,
             parent, parse, powermod,
             precision, rand, Rational, rem, reverse, setindex!,
             show, similar, sign, sin, sincos, sinh, sinpi, size, sqrt, string,
             tan, tanh, trailing_zeros, transpose, truncate, typed_hvcat,
             typed_hcat, vcat, xor, zero, zeros, +, -, *, ==, ^, &, |, <<, >>,
             ~, <=, >=, <, >, //, /, !=

if VERSION >= v"1.5.0-DEV.639"
  import Base: contains
end

if VERSION >= v"1.6.0-DEV.292"
  import Base: sincospi
end

import LinearAlgebra: det, norm, nullspace, rank, transpose!, hessenberg, tr,
                      lu, lu!, eigvals

import AbstractAlgebra: nullspace, @show_name, @show_special, find_name,
                        get_special, set_special, @declare_other,
                        @show_special_elem, force_coerce, force_op, expressify

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

export ComplexField, PadicField, QadicField

# Things/constants which are also defined in AbstractAlgebra:
export ZZ, QQ, RealField, FiniteField, NumberField

###############################################################################
#
#   Set up environment / load libraries
#
###############################################################################

if VERSION > v"1.3.0-rc4"
  # this should do the dlopen for 1.3 and later
  # and imports the libxxx variables
  using Arb_jll
  using Antic_jll
  using Calcium_jll
else
  deps_dir = joinpath(@__DIR__, "..", "deps")
  include(joinpath(deps_dir,"deps.jl"))
end

iswindows64() = (Sys.iswindows() ? true : false) && (Int == Int64)

const pkgdir = realpath(joinpath(dirname(@__DIR__)))

#const libdir = joinpath(pkgdir, "deps", "usr", "lib")
#const bindir = joinpath(pkgdir, "deps", "usr", "bin")

const libflint = LoadFlint.libflint
const libgmp = LoadFlint.libgmp
const libmpfr = LoadFlint.libmpfr

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
  if VERSION < v"1.3.0-rc4"
    # this does the dlopen for 1.0-1.2
    check_deps()
  end

   # In case libgmp picks up the wrong libgmp later on, we "unset" the jl_*
   # functions from the julia :libgmp.

   __isthreaded[] = get(ENV, "NEMO_THREADED", "") == "1"

   if __isthreaded[]
      ccall((:__gmp_set_memory_functions, :libgmp), Nothing,
            (Int, Int, Int), 0, 0, 0)
   end

   ccall((:flint_set_abort, libflint), Nothing,
         (Ptr{Nothing},), @cfunction(flint_abort, Nothing, ()))

   # Check if were are non-interactive
   bt = Base.process_backtrace(Base.backtrace())
   isinteractive_manual = all(sf -> sf[1].func != :_tryrequire_from_serialized, bt)

   # Respect the -q flag
   isquiet = Bool(Base.JLOptions().quiet)

   if !isquiet && isinteractive_manual && isinteractive() &&
         !any(x -> x.name in ("Hecke", "Oscar", "Singular"), keys(Base.package_locks)) &&
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

if VERSION >= v"1.4"
   deps = Pkg.dependencies()
   if !haskey(deps, Base.UUID("2edaba10-b0f1-5616-af89-8c11ac63239a"))
      version() = "building"
   else
      ver = deps[Base.UUID("2edaba10-b0f1-5616-af89-8c11ac63239a")]
      if occursin("/dev/", ver.source)
         version() = VersionNumber("$(ver.version)-dev")
      else
         version() = VersionNumber("$(ver.version)")
      end
   end
else
   ver = Pkg.API.__installed(PKGMODE_MANIFEST)["Nemo"]
   dir = dirname(@__DIR__)
   if occursin("/dev/", dir)
      version() = VersionNumber("$(ver)-dev")
   else
      version() = VersionNumber("$(ver)")
   end
end


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
       FractionField, ResidueRing, Partition, SymmetricGroup, YoungTableau,
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



###############################################################################
#
#  Random
#
################################################################################

"""
    randseed!([seed::Integer])

Reseed Nemo's global RNG with `seed`. Note that each thread has its own global RNG,
and that `randseed!` reseeds only the RNG from the current thread.
This is similar to what `Random.seed!(seed)` does for Julia's global RNG.

The given `seed` must be a non-negative integer.
When `seed` is not specified, a random seed is generated from Julia's global RNG.

For a fixed seed, the stream of generated numbers is allowed to change between
different versions of Nemo.
"""
randseed!(seed::Union{Integer,Nothing}=nothing) =
   Random.seed!(_flint_rand_states[Threads.threadid()], seed)

function Random.seed!(a::rand_ctx, s::Integer)
   # we hash the seed to obtain better independence of streams for
   # two given seeds which could be "not very different"
   # (cf. the documentation of `gmp_randseed`).
   # Hashing has a negligible cost compared to the call to `gmp_randseed`.
   ctx = SHA.SHA2_512_CTX()
   seed = Random.make_seed(s)::Vector{UInt32}
   SHA.update!(ctx, reinterpret(UInt8, seed))
   digest = reinterpret(UInt, SHA.digest!(ctx))
   @assert Base.GMP.Limb == UInt

   # two last words go for flint_randseed!
   flint_randseed!(a, digest[end], digest[end-1])

   # remaining words (6 or 14) for flint_gmp_randseed!
   seedbits = 512 - 2*sizeof(UInt)*8
   n = Int(seedbits / (sizeof(UInt)*8))
   @assert n == 6 && UInt === UInt64 || n == 14 && UInt === UInt32
   if VERSION >= v"1.3"
      b = BigInt(nbits = seedbits)
   else
      b = Base.GMP.MPZ.realloc2(seedbits)
   end

   @assert b.alloc >= n
   GC.@preserve digest b unsafe_copyto!(b.d, pointer(digest), n)
   b.size = n
   flint_gmp_randseed!(a, b)
   return a
end

Random.seed!(a::rand_ctx, s::Nothing=nothing) = Random.seed!(a, rand(UInt128))

flint_randseed!(a::rand_ctx, seed1::UInt, seed2::UInt) =
   ccall((:flint_randseed, libflint), Cvoid, (Ptr{Cvoid}, UInt, UInt), a.ptr, seed1, seed2)

function flint_gmp_randseed!(a::rand_ctx, seed::BigInt)
   ccall((:_flint_rand_init_gmp, libflint), Cvoid, (Ptr{Cvoid},), a.ptr)
   ccall((:__gmp_randseed, :libgmp), Cvoid, (Ptr{Cvoid}, Ref{BigInt}),
         a.ptr, # gmp_state is the first field of a.ptr (cf. flint.h)
         seed)
end

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

################################################################################
#
#   Deprecations
#
################################################################################

include("Deprecations.jl")

end # module
