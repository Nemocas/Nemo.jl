# This file has been generated from FLINT's `limb_types.h`. DO NOT EDIT MANUALLY.
#
# To regenerate it, e.g. after updating FLINT_jll, run
#   julia etc/generate_FLINT_structs.jl
#
# Generated with FLINT_jll v301.600.1+0.

const FLINT_MAX_FACTORS_IN_LIMB = 15

struct n_factor_t <: FlintStruct
  num::Cint
  exp::NTuple{FLINT_MAX_FACTORS_IN_LIMB, Cint}
  p::NTuple{FLINT_MAX_FACTORS_IN_LIMB, ulong}
end

struct n_primes_struct <: FlintStruct
  small_i::slong
  small_num::slong
  small_primes::Ptr{Cuint}

  sieve_a::ulong
  sieve_b::ulong
  sieve_i::slong
  sieve_num::slong
  sieve::Ptr{Cchar}
end

const n_primes_t = Tuple{n_primes_struct}
