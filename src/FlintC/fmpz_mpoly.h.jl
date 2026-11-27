# This file has been generated from FLINT's `fmpz_mpoly.h`. DO NOT EDIT MANUALLY.
#
# To regenerate it, e.g. after updating FLINT_jll, run
#   julia etc/generate_FLINT_structs.jl
#
# Generated with FLINT_jll v301.600.1+0.

struct fmpz_mpoly_univar_struct <: FlintStruct
  coeffs::Ptr{fmpz_mpoly_struct}
  exps::Ptr{fmpz}
  alloc::slong
  length::slong
end

const fmpz_mpoly_univar_t = Tuple{fmpz_mpoly_univar_struct}

struct fmpz_mpolyd_struct <: FlintStruct
  nvars::slong
  degb_alloc::slong
  deg_bounds::Ptr{slong}
  length::slong
  coeff_alloc::slong
  coeffs::Ptr{fmpz}
end

const fmpz_mpolyd_t = Tuple{fmpz_mpolyd_struct}

struct fmpz_mpoly_vec_struct <: FlintStruct
  p::Ptr{fmpz_mpoly_struct}
  alloc::slong
  length::slong
end

const fmpz_mpoly_vec_t = Tuple{fmpz_mpoly_vec_struct}

struct fmpz_mpoly_stripe_struct <: FlintStruct
  big_mem::Ptr{Cchar}
  big_mem_alloc::slong
  N::slong
  bits::flint_bitcnt_t
  cmpmask::Ptr{ulong}
  startidx::Ptr{slong}
  endidx::Ptr{slong}
  emin::Ptr{ulong}
  emax::Ptr{ulong}
  coeff_bits::flint_bitcnt_t
  upperclosed::Cint
  flint_small::Cint
end
const struct__fmpz_mpoly_stripe_struct = fmpz_mpoly_stripe_struct

const fmpz_mpoly_stripe_t = Tuple{fmpz_mpoly_stripe_struct}

struct fmpz_mpolyd_ctx_struct <: FlintStruct
  nvars::slong
  perm::Ptr{slong}
end

const fmpz_mpolyd_ctx_t = Tuple{fmpz_mpolyd_ctx_struct}

struct struct_fmpz_pow_cache_t <: FlintStruct
  powers::Ptr{fmpz}
  length::slong
  alloc::slong
  tmp::fmpz_t
end
const fmpz_pow_cache_t = Tuple{struct_fmpz_pow_cache_t}

struct fmpz_mpoly_geobucket_struct <: FlintStruct
  polys::NTuple{FLINT_BITS÷2, fmpz_mpoly_struct}
  temps::NTuple{FLINT_BITS÷2, fmpz_mpoly_struct}
  length::slong
end
const struct_fmpz_mpoly_geobucket = fmpz_mpoly_geobucket_struct

const fmpz_mpoly_geobucket_t = Tuple{fmpz_mpoly_geobucket_struct}
