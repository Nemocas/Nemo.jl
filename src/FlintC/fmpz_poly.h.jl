# This file has been generated from FLINT's `fmpz_poly.h`. DO NOT EDIT MANUALLY.
#
# To regenerate it, e.g. after updating FLINT_jll, run
#   julia etc/generate_FLINT_structs.jl
#
# Generated with FLINT_jll v301.600.1+0.

const FMPZ_POLY_INV_NEWTON_CUTOFF = 32

const FMPZ_POLY_SQRT_DIVCONQUER_CUTOFF = 16

const FMPZ_POLY_SQRTREM_DIVCONQUER_CUTOFF = 16

struct fmpz_poly_powers_precomp_struct <: FlintStruct
  powers::Ptr{Ptr{fmpz}}
  len::slong
end

const fmpz_poly_powers_precomp_t = Tuple{fmpz_poly_powers_precomp_struct}

struct fmpz_poly_mul_precache_struct <: FlintStruct
  jj::Ptr{Ptr{ulong}}
  n::slong
  len2::slong
  loglen::slong
  bits2::slong
  limbs::slong
  poly2::fmpz_poly_t
end

const fmpz_poly_mul_precache_t = Tuple{fmpz_poly_mul_precache_struct}
