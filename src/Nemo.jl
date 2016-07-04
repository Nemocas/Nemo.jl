VERSION >= v"0.4.0-dev+6521" && __precompile__()

module Nemo
 
import Base: Array, abs, asin, asinh, atan, atanh, base, bin, call,
             checkbounds, convert, cmp, contains, cos, cosh, dec, deepcopy,
             den, deserialize, div, divrem, exp, factor, gcd, gcdx, getindex,
             hash, hcat, hex, intersect, inv, invmod, isequal, isfinite,
             isless, isprime, isqrt, lcm, ldexp, length, log, lufact, mod,
             ndigits, nextpow2, norm, nullspace, num, oct, one, parent, parity,
             parseint, precision, prevpow2, promote_rule, rank, Rational, rem,
             reverse, serialize, setindex!, show, sign, sin, sinh, size, sqrt,
             string, sub, tan, tanh, trace, trailing_zeros, transpose,
             transpose!, truncate, typed_hvcat, typed_hcat, var, vcat, zero,
             zeros, det,
             +, -, *, ==, ^, &, |, $, <<, >>, ~, <=, >=, <, >, //,
             /, !=

import Base: floor, ceil, hypot, sqrt,
             log, log1p, exp, expm1, sin, cos, sinpi, cospi, tan, cot,
             sinh, cosh, tanh, coth, atan, asin, acos,
             atanh, asinh, acosh, gamma, lgamma, digamma, zeta,
             sinpi, cospi, atan2

export SetElem, GroupElem, RingElem, FieldElem

export PolyElem, SeriesElem, ResElem, FracElem, MatElem, FinFieldElem

export PolyRing, SeriesRing, ResRing, FracField, MatSpace, FinField

export ZZ, QQ, PadicField, FiniteField, NumberField, CyclotomicField,
       MaximalRealSubfield, MaximalOrder, Ideal, PermutationGroup

export create_accessors, get_handle, package_handle, allocatemem, zeros,
       Array, sig_exists

export flint_cleanup, flint_set_num_threads

export error_dim_negative

export on_windows64

include("AbstractTypes.jl")

###############################################################################
#
#   Set up environment / load libraries
#
###############################################################################

const pkgdir = realpath(joinpath(dirname(@__FILE__), ".."))
const libdir = joinpath(pkgdir, "local", "lib")
const libgmp = joinpath(pkgdir, "local", "lib", "libgmp")
const libmpfr = joinpath(pkgdir, "local", "lib", "libmpfr")
const libflint = joinpath(pkgdir, "local", "lib", "libflint")
const libpari = joinpath(pkgdir, "local", "lib", "libpari")
const libarb = joinpath(pkgdir, "local", "lib", "libarb")
  
function allocatemem(bytes::Int)
   newsize = pari(fmpz(bytes)).d
   ccall((:gp_allocatemem, :libpari), Void, (Ptr{Int},), newsize)
end

function pari_sigint_handler()
   error("User interrupt")
end

function flint_abort()
  error("Problem in the Flint-Subsystem")
end


on_windows64 = (@windows ? true : false) && (Int == Int64)

function __init__()

   on_windows = @windows ? true : false
   on_linux = @linux ? true : false

   if "HOSTNAME" in keys(ENV) && ENV["HOSTNAME"] == "juliabox"
       push!(Libdl.DL_LOAD_PATH, "/usr/local/lib")
   elseif on_linux
       push!(Libdl.DL_LOAD_PATH, libdir)
       Libdl.dlopen(libgmp)
       Libdl.dlopen(libmpfr)
       Libdl.dlopen(libflint)
       Libdl.dlopen(libpari)
       Libdl.dlopen(libarb)
   else
      push!(Libdl.DL_LOAD_PATH, libdir)
   end
 
   ccall((:pari_set_memory_functions, libpari), Void,
      (Ptr{Void},Ptr{Void},Ptr{Void},Ptr{Void}),
      cglobal(:jl_malloc),
      cglobal(:jl_calloc),
      cglobal(:jl_realloc),
      cglobal(:jl_free))

   ccall((:pari_init, libpari), Void, (Int, Int), 300000000, 10000)
  
   global avma = cglobal((:avma, libpari), Ptr{Int})

   global gen_0 = cglobal((:gen_0, libpari), Ptr{Int})

   global gen_1 = cglobal((:gen_1, libpari), Ptr{Int})

   global pari_sigint = cglobal((:cb_pari_sigint, libpari), Ptr{Void})

   unsafe_store!(pari_sigint, cfunction(pari_sigint_handler, Void, ()), 1)

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

   ccall((:flint_set_abort, libflint), Void,
      (Ptr{Void},), cfunction(flint_abort, Void, ()))

   println("")
   println("Welcome to Nemo version 0.4.0")
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
#   Load Nemo Rings/Fields/etc
#
###############################################################################

include("generic/GenericTypes.jl")

include("flint/FlintTypes.jl")

include("antic/AnticTypes.jl")

include("arb/ArbTypes.jl")

include("pari/PariTypes.jl")

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
   accessor_name = gensym()
   @eval begin
      function $(symbol(:get, accessor_name))(a::$T)
         return a.auxilliary_data[$handle]::$S
      end,
      function $(symbol(:set, accessor_name))(a::$T, b::$S)
         if $handle > length(a.auxilliary_data)
            resize(a.auxilliary_data, $handle)
         end
         a.auxilliary_data[$handle] = b
      end
   end
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

Array(R::Ring, r::Int...) = Array(elem_type(R), r)

function zeros(R::Ring, r::Int...)
   T = elem_type(R)
   A = Array(T, r)
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

PermutationGroup = FlintPermGroup

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
#   Set domain for NumberField to Antic
#
###############################################################################

NumberField = AnticNumberField
CyclotomicField = AnticCyclotomicField
MaximalRealSubfield = AnticMaximalRealSubfield

###############################################################################
#
#   Set domain for MaximalOrder and Ideal to Pari
#
###############################################################################

MaximalOrder = PariMaximalOrder
Ideal = PariIdeal

###############################################################################
#
#   Error objects
#
###############################################################################

const error_dim_negative = ErrorException("Dimensions must be non-negative")

###############################################################################
#
#   Test code
#
###############################################################################

include("../benchmarks/runbenchmarks.jl")

end # module
