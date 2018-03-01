VERSION >= v"0.4.0-dev+6521" && __precompile__()

module Nemo

import Base: Array, abs, acos, acosh, asin, asinh, atan, atan2, atanh, base,
             bin, ceil, checkbounds, conj, convert, cmp, contains, cos, cosh,
             cospi, cot, coth, dec, deepcopy, deepcopy_internal, deserialize,
             det, div, divrem, expm1, eye, floor, gamma, gcd, gcdx, getindex,
             hash, hcat, hex, hypot, intersect, inv, invmod, isequal,
             isfinite, isless, isqrt, isreal, iszero, lcm, ldexp, length,
             lgamma, log, log1p, lufact, lufact!, mod, ndigits, nextpow2, norm,
             nullspace, numerator, oct, one, parent, parse, precision,
             prevpow2, rand, rank, Rational, rem, reverse, serialize,
             setindex!, show, similar, sign, sin, sinh, sinpi, size, string,
             tan, tanh, trace, trailing_zeros, transpose, transpose!, truncate,
             typed_hvcat, typed_hcat, var, vcat, zero, zeros, +, -, *, ==, ^,
             &, |, $, <<, >>, ~, <=, >=, <, >, //, /, !=

import AbstractAlgebra: Generic, Ring, Field, RingElem, FieldElem, RingElement,
       FieldElement, FracField, FracElem, ResRing, ResField, PolyRing, PolyElem,
       ResElem, FinField, FinFieldElem, SeriesRing, RelSeriesElem, AbsSeriesElem,
       MatSpace, MatElem, SeriesElem, Set, SetElem, Module, ModuleElem, Group,
       GroupElem

import AbstractAlgebra: PermGroup, AllPerms, perm, Partition, AllParts, SkewDiagram,
       YoungTableau, PolynomialRing, PowerSeriesRing, LaurentSeriesRing,
       LaurentSeriesField, SparsePolynomialRing, ResidueRing, ResidueField, MatrixSpace

import AbstractAlgebra: zero_matrix, identity_matrix, matrix

import AbstractAlgebra.Generic: add!, addeq!, addmul!, base_ring,
           canonical_unit, character, characteristic, charpoly,
           charpoly_danilevsky!, charpoly_danilevsky_ff!, charpoly_hessenberg!,
           chebyshev_t, chebyshev_u, _check_dim, check_parent, coeff, cols,
           compose, content, cycles, data, degree, denominator, derivative, det,
           det_clow, det_df, det_fflu, det_popov, dim, discriminant, divexact,
           divides, divrem, elem_type, elements, evaluate, extended_weak_popov,
           extended_weak_popov_with_trafo, fflu!, fflu, find_pivot_popov, fit!,
           gcd, gen, gens, gcdinv, gcdx, gram, has_left_neighbor,
           has_bottom_neighbor, hash, hessenberg!, hessenberg, hnf, hnf_cohen,
           hnf_cohen_with_trafo, hnf_kb, hnf_kb_with_trafo, hnf_minors,
           hnf_minors_with_trafo, hnf_with_trafo, hnf_via_popov,
           hnf_via_popov_with_trafo, hooklength, inskewdiag,
           integral, interpolate, inv, inv!, invmod, isconstant, isdegree,
           isdomain_type, isexact_type, isgen, ishessenberg, ismonomial,
           isnegative, isone, isreverse, isrimhook, isrref, issquare, isterm,
           isunit, lcm, lead, length, leglength, main_variable,
           main_variable_extract, main_variable_insert, matrix_repr,
           max_degrees, max_precision, minpoly, mod, modulus, monomial_iszero,
           monomial_set!, monomial_to_newton!, mul!, mul_classical,
           mul_karatsuba, mul_ks, mullow, mulmod, needs_parentheses,
           newton_to_monomial!, normalise, nvars, numerator, O, one, order,
           ordering, parent_type, parity, partitionseq, polcoeff, pol_length,
           powmod, pow_multinomial, popov, powers, ppio, precision, primpart,
           promote_rule, pseudodivrem, pseudorem, randmat_triu, randmat_with_rank,
           rand_ordering, rank_profile_popov, remove, renormalize!, resultant,
           resultant_ducos, resultant_euclidean, resultant_subresultant,
           resultant_sylvester, resx, reverse, rows, rref, rref!, setcoeff!,
           set_length!, setpermstyle, set_prec!, set_val!, size, shift_left,
           shift_right, show_minus_one, similarity!, snf, snf_kb,
           snf_kb_with_trafo, snf_with_trafo, solve, solve_rational, solve_triu,
           sub, subst, swap_rows, swap_rows!, trail, truncate, typed_hcat,
           typed_hvcat, valuation, var, vars, weak_popov, weak_popov_with_trafo,
           zero, zero!, kronecker_product, ErrorConstrDimMismatch, error_dim_negative

export elem_type, parent_type

export SetElem, GroupElem, RingElem, FieldElem, AccessorNotSetError

export PolyElem, SeriesElem, AbsSeriesElem, RelSeriesElem, ResElem, FracElem,
       MatElem, FinFieldElem, MPolyElem

export PolyRing, SeriesRing, AbsSeriesRing, ResRing, FracField, MatSpace,
       FinField, MPolyRing

export PermutationGroup, ZZ, QQ, PadicField, FiniteField, RealField, ComplexField,
       CyclotomicField, MaximalRealSubfield, NumberField

export create_accessors, get_handle, package_handle, zeros,
       Array, sig_exists

export flint_cleanup, flint_set_num_threads

export error_dim_negative, ErrorConstrDimMismatch

export is_windows64

export add!, addeq!, addmul!, base_ring, canonical_unit, character,
                 characteristic, charpoly, charpoly_danilevsky!,
                 charpoly_danilevsky_ff!, charpoly_hessenberg!, chebyshev_t,
                 chebyshev_u, _check_dim, check_parent, coeff, cols, compose,
                 content, cycles, data, degree, denominator, derivative, det, det_clow,
                 det_df, det_fflu, det_popov, dim, discriminant, divexact,
                 divides, divrem, elem_type, elements, evaluate,
                 extended_weak_popov, extended_weak_popov_with_trafo, fflu!,
                 fflu, find_pivot_popov, fit!, gcd, gen, gens, gcdinv, gcdx,
                 gram, has_left_neighbor, has_bottom_neighbor, hash,
                 hessenberg!, hessenberg, hnf, hnf_cohen, hnf_cohen_with_trafo,
                 hnf_kb, hnf_kb_with_trafo, hnf_minors, hnf_minors_with_trafo,
                 hnf_with_trafo, hnf_via_popov, hnf_via_popov_with_trafo,
                 hooklength, identity_matrix, inskewdiag, integral,
                 interpolate, inv, inv!, invmod, isconstant, isdegree,
                 isdomain_type, isexact, isexact_type, isgen, ishessenberg,
                 ismonomial, isnegative, isone, isreverse, isrimhook, isrref, issquare,
                 isterm, isunit, iszero, lcm, lead, leglength, length,
                 main_variable, main_variable_extract, main_variable_insert,
                 matrix, matrix_repr, max_degrees, max_precision, minpoly, mod,
                 modulus, monomial_iszero, monomial_set!, monomial_to_newton!,
                 mul!, muleq!, mul_classical, mul_karatsuba, mul_ks, mullow, mulmod,
                 needs_parentheses, newton_to_monomial!, normalise, nvars,
                 numerator, O, one, order, ordering, parent_type, parity,
                 partitionseq, polcoeff, pol_length, powmod, pow_multinomial,
                 popov, powers, ppio, precision, primpart, pseudodivrem,
                 pseudorem, randmat_triu, randmat_with_rank, rand_ordering,
                 rank_profile_popov, remove, renormalize!, resultant,
                 resultant_ducos, resultant_euclidean, resultant_subresultant,
                 resultant_sylvester, resx, reverse, rows, rref, rref!,
                 setcoeff!, set_length!, setpermstyle, set_prec!, set_val!,
                 shift_left, shift_right, show_minus_one, similarity!, size, snf,
                 snf_kb, snf_kb_with_trafo, snf_with_trafo, solve,
                 solve_rational, solve_triu, sub, subst, swap_rows, swap_rows!,
                 trail, truncate, typed_hcat, typed_hvcat, valuation, var,
                 vars, weak_popov, weak_popov_with_trafo, zero, zero!,
                 zero_matrix, kronecker_product

if VERSION >= v"0.6.0-dev.2024" # julia started exporting iszero (again?)
   import Base: iszero
end

if VERSION < v"0.6-"
   import Base: isprime, factor, parity, sub, call
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

active_mem=Dict{UInt, Tuple{Symbol, UInt, Any}}()

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
  ccall(:jl_free, Void, (UInt, ), n)
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
  ccall(:jl_gc_counted_free, Void, (UInt, UInt), n, s)
end

function show_active(l::UInt = UInt(0), frames::Int = 2)
  global active_mem
  for i = keys(active_mem)
    v = active_mem[i]
    if v[2] >= l
      n = min(frames, length(v[3]))
      Base.show_backtrace(STDOUT, v[3][1:n])
    end
  end
end

function trace_memory(b::Bool)
  if is_windows()
    return
  end
  if b
    ccall((:__gmp_set_memory_functions, libgmp), Void,
       (Ptr{Void},Ptr{Void},Ptr{Void}),
       cfunction(trace_counted_malloc, UInt, (UInt, )),
       cfunction(trace_counted_realloc, UInt, (UInt, UInt, UInt)),
       cfunction(trace_counted_free, Void, (UInt, UInt)))

    ccall((:__flint_set_memory_functions, libflint), Void,
       (Ptr{Void},Ptr{Void},Ptr{Void},Ptr{Void}),
       cfunction(trace_malloc, UInt, (UInt, )),
       cfunction(trace_calloc, UInt, (UInt, UInt)),
       cfunction(trace_realloc, UInt, (UInt, UInt)),
       cfunction(trace_free, Void, (UInt, )))
  else    
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
   println("Welcome to Nemo version 0.8.0")
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
  print("Nemo version 0.8.0\n")
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

function exp(a::T) where T
   return Base.exp(a)
end

function sqrt(a::T) where T
  return Base.sqrt(a)
end

export PowerSeriesRing, PolynomialRing, SparsePolynomialRing, MatrixSpace,
       FractionField, ResidueRing, Partition, PermGroup, YoungTableau,
       AllParts, SkewDiagram, AllPerms, perm, LaurentSeriesRing,
       LaurentSeriesField, ResidueField

export Generic

###############################################################################
#
#   Load Nemo Rings/Fields/etc
#
###############################################################################

include("flint/FlintTypes.jl")

include("antic/AnticTypes.jl")

include("arb/ArbTypes.jl")

#include("ambiguities.jl") # remove ambiguity warnings

include("flint/adhoc.jl")

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

mutable struct AccessorNotSetError <: Exception
end

function create_accessors(T, S, handle)
   get = function(a)
      if handle > length(a.auxilliary_data) ||
         !isassigned(a.auxilliary_data, handle)
        throw(AccessorNotSetError())
      end
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

function sig_exists(T::Type{Tuple{U, V, W}}, sig_table::Array{X, 1}) where {U, V, W, X}
   for s in sig_table
      if s === T
         return true
      end
   end
   return false
end

else

function sig_exists(T::Type{Tuple{U, V, W}}, sig_table::Array{X, 1}) where {U, V, W, X}
   return false
end

end # if VERSION

include("Rings.jl")

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
