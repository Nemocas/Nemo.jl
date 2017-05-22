VERSION >= v"0.4.0-dev+6521" && __precompile__()

module Nemo

import Base: Array, abs, asin, asinh, atan, atanh, base, bin, checkbounds,
             convert, cmp, contains, cos, cosh, dec, deepcopy,
             deepcopy_internal, den, deserialize, det, div, divrem, exp, eye,
             gcd, gcdx, getindex, hash, hcat, hex, intersect, inv, invmod,
             isequal, isfinite, isless, isqrt, isreal, lcm, ldexp, length, log,
             lufact, lufact!, mod, ndigits, nextpow2, norm, nullspace, num,
             oct, one, parent, parse, precision, prevpow2, promote_rule, rank,
             Rational, rem, reverse, serialize, setindex!, show, sign, sin,
             sinh, size, sqrt, string, tan, tanh, trace, trailing_zeros,
             transpose, transpose!, truncate, typed_hvcat, typed_hcat, var,
             vcat, zero, zeros, +, -, *, ==, ^, &, |, $, <<, >>, ~, <=, >=, <,
             >, //, /, !=

import Base: floor, ceil, hypot, sqrt, log, log1p, exp, expm1, sin, cos, sinpi,
             cospi, tan, cot, sinh, cosh, tanh, coth, atan, asin, acos, atanh,
             asinh, acosh, gamma, lgamma, digamma, zeta, sinpi, cospi, atan2

export SetElem, GroupElem, RingElem, FieldElem

export PolyElem, SeriesElem, AbsSeriesElem, RelSeriesElem, ResElem, FracElem,
       MatElem, FinFieldElem

export PolyRing, SeriesRing, AbsSeriesRing, ResRing, FracField, MatSpace, FinField

export ZZ, QQ, PadicField, FiniteField, NumberField, CyclotomicField,
       MaximalRealSubfield, PermutationGroup

export RealField, ComplexField

export create_accessors, get_handle, package_handle, zeros,
       Array, sig_exists

export flint_cleanup, flint_set_num_threads

export error_dim_negative, ErrorConstrDimMismatch

export is_windows64

if VERSION >= v"0.6.0-dev.2024" # julia started exporting iszero (again?)
   import Base: iszero
end

if VERSION < v"0.6-"
   import Base: isprime, factor, parity, sub, call
end

include("AbstractTypes.jl")

###############################################################################
#
#   Set up environment / load libraries
#
###############################################################################

is_windows64() = (is_windows() ? true : false) && (Int == Int64)

const pkgdir = realpath(joinpath(dirname(@__FILE__), ".."))
const libdir = joinpath(pkgdir, "local", "lib")
if is_windows()
   const libgmp = joinpath(pkgdir, "local", "lib", "libgmp-16")
else
   const libgmp = joinpath(pkgdir, "local", "lib", "libgmp")
end
const libmpfr = joinpath(pkgdir, "local", "lib", "libmpfr")
const libflint = joinpath(pkgdir, "local", "lib", "libflint")
const libarb = joinpath(pkgdir, "local", "lib", "libarb")

function flint_abort()
  error("Problem in the Flint-Subsystem")
end

function __init__()

   if "HOSTNAME" in keys(ENV) && ENV["HOSTNAME"] == "juliabox"
       push!(Libdl.DL_LOAD_PATH, "/usr/local/lib")
   elseif is_linux()
       push!(Libdl.DL_LOAD_PATH, libdir)
       Libdl.dlopen(libgmp)
       Libdl.dlopen(libmpfr)
       Libdl.dlopen(libflint)
       Libdl.dlopen(libarb)
   else
      push!(Libdl.DL_LOAD_PATH, libdir)
   end

   if !is_windows()
      ccall((:__gmp_set_memory_functions, libgmp), Void,
         (Ptr{Void},Ptr{Void},Ptr{Void}),
         cglobal(:jl_gc_counted_malloc),
         cglobal(:jl_gc_counted_realloc_with_old_size),
         cglobal(:jl_gc_counted_free))

      ccall((:__flint_set_memory_functions, libflint), Void,
         (Ptr{Void},Ptr{Void},Ptr{Void},Ptr{Void}),
         cglobal(:jl_malloc),
         cglobal(:jl_calloc),
         cglobal(:jl_realloc),
         cglobal(:jl_free))
   end

   ccall((:flint_set_abort, libflint), Void,
      (Ptr{Void},), cfunction(flint_abort, Void, ()))

   println("")
   println("Welcome to Nemo version 0.6.0")
   println("")
   println("Nemo comes with absolutely no warranty whatsoever")
   println("")
end

function flint_set_num_threads(a::Int)
   ccall((:flint_set_num_threads, libflint), Void, (Int,), a)
end

function flint_cleanup()
   ccall((:flint_cleanup, libflint), Void, ())
end

###############################################################################
#
#  Version information
#
################################################################################

function versioninfo()
  print("Nemo version 0.6.0 \n")
  nemorepo = Pkg.dir("Nemo")

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
#   Load Nemo Rings/Fields/etc
#
###############################################################################

include("generic/GenericTypes.jl")

include("flint/FlintTypes.jl")

include("antic/AnticTypes.jl")

include("arb/ArbTypes.jl")

include("ambiguities.jl") # remove ambiguity warnings

include("Groups.jl")

###########################################################
#
#   Package handle creation
#
###########################################################

const package_handle = [1]

function get_handle()
   package_handle[1] += 1
   return package_handle[1] - 1
end

###############################################################################
#
#   Auxilliary data accessors
#
###############################################################################

function create_accessors(T, S, handle)
   get = function(a)
      return a.auxilliary_data[handle]
   end
   set = function(a, b)
      if handle > length(a.auxilliary_data)
         resize!(a.auxilliary_data, handle)
      end
      a.auxilliary_data[handle] = b
   end
   return get, set
end

###############################################################################
#
#   Promote rule helpers
#
###############################################################################

if VERSION >= v"0.5.0-dev+3171"

function sig_exists{U, V, W, X}(T::Type{Tuple{U, V, W}}, sig_table::Array{X, 1})
   for s in sig_table
      if s === T
         return true
      end
   end
   return false
end

else

function sig_exists{U, V, W, X}(T::Type{Tuple{U, V, W}}, sig_table::Array{X, 1})
   return false
end

end # if VERSION

###############################################################################
#
#   Array creation functions
#
###############################################################################

Array(R::Ring, r::Int...) = Array{elem_type(R)}(r)

function zeros(R::Ring, r::Int...)
   T = elem_type(R)
   A = Array{T}(r)
   for i in eachindex(A)
      A[i] = R()
   end
   return A
end

###############################################################################
#
#   Set domain for PermutationGroup to Flint
#
###############################################################################

PermutationGroup = PermGroup

###############################################################################
#
#   Set domain for ZZ, QQ, PadicField, FiniteField to Flint
#
###############################################################################

ZZ = FlintZZ
QQ = FlintQQ
PadicField = FlintPadicField
FiniteField = FlintFiniteField

###############################################################################
#
#   Set domain for RR, CC to Arb
#
###############################################################################

RealField = ArbField
ComplexField = AcbField

###############################################################################
#
#   Set domain for NumberField to Antic
#
###############################################################################

NumberField = AnticNumberField
CyclotomicField = AnticCyclotomicField
MaximalRealSubfield = AnticMaximalRealSubfield

###############################################################################
#
#   Error objects
#
###############################################################################

type ErrorConstrDimMismatch <: Exception
  expect_r::Int
  expect_c::Int
  get_r::Int
  get_c::Int
  get_l::Int

  function ErrorConstrDimMismatch(er::Int, ec::Int, gr::Int, gc::Int)
    e = new(er, ec, gr, gc, -1)
    return e
  end

  function ErrorConstrDimMismatch(er::Int, ec::Int, gl::Int)
    e = new(er, ec, -1, -1, gl)
    return e
  end

  function ErrorConstrDimMismatch{T}(er::Int, ec::Int, a::Array{T, 2})
    gr, gc = size(a)
    return ErrorConstrDimMismatch(er, ec, gr, gc)
  end

  function ErrorConstrDimMismatch{T}(er::Int, ec::Int, a::Array{T, 1})
    gl = length(a)
    return ErrorConstrDimMismatch(er, ec, gl)
  end
end

function Base.showerror(io::IO, e::ErrorConstrDimMismatch)
  if e.get_l == -1
    print(io, "Expected dimension $(e.expect_r) x $(e.expect_c), ")
    print(io, "got $(e.get_r) x $(e.get_c)")
  else
    print(io, "Expected an array of length $(e.expect_r * e.expect_c), ")
    print(io, "got $(e.get_l)")
  end
end

const error_dim_negative = ErrorException("Dimensions must be non-negative")

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

   cmd = "using Base.Test; using Nemo; include(\"$test_file\"); $test_function_name();"
   info("spawning ", `$julia_exe -e \"$cmd\"`)
   run(`$julia_exe -e $cmd`)
end

end # module
