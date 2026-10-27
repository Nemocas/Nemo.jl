# This file has been generated from FLINT's `fq_nmod_mpoly_factor.h`. DO NOT EDIT MANUALLY.
#
# To regenerate it, e.g. after updating FLINT_jll, run
#   julia etc/generate_FLINT_structs.jl
#
# Generated with FLINT_jll v301.600.1+0.

struct fq_nmod_mpoly_factor_struct <: FlintStruct
  constant::fq_nmod_t
  poly::Ptr{fq_nmod_mpoly_struct}
  exp::Ptr{fmpz}
  num::slong
  alloc::slong
end

const fq_nmod_mpoly_factor_t = Tuple{fq_nmod_mpoly_factor_struct}

struct fq_nmod_mpolyv_struct <: FlintStruct
  coeffs::Ptr{fq_nmod_mpoly_struct}
  alloc::slong
  length::slong
end

const fq_nmod_mpolyv_t = Tuple{fq_nmod_mpolyv_struct}

struct fq_nmod_mpoly_pfrac_struct <: FlintStruct
  bits::flint_bitcnt_t
  w::slong
  r::slong
  inv_prod_dbetas::Ptr{fq_nmod_poly_struct}
  inv_prod_dbetas_mvar::Ptr{fq_nmod_mpoly_struct}
  dbetas::Ptr{fq_nmod_poly_struct}
  dbetas_mvar::Ptr{fq_nmod_mpoly_struct}
  prod_mbetas::Ptr{fq_nmod_mpoly_struct}
  prod_mbetas_coeffs::Ptr{fq_nmod_mpolyv_struct}
  mbetas::Ptr{fq_nmod_mpoly_struct}
  deltas::Ptr{fq_nmod_mpoly_struct}
  xalpha::Ptr{fq_nmod_mpoly_struct}
  q::Ptr{fq_nmod_mpoly_struct}
  G::Ptr{fq_nmod_mpoly_geobucket_struct}
  qt::Ptr{fq_nmod_mpoly_struct}
  newt::Ptr{fq_nmod_mpoly_struct}
  delta_coeffs::Ptr{fq_nmod_mpolyv_struct}
  T::fq_nmod_mpoly_t
  Q::fq_nmod_mpoly_t
  R::fq_nmod_mpoly_t
end

const fq_nmod_mpoly_pfrac_t = Tuple{fq_nmod_mpoly_pfrac_struct}
