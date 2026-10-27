# This file has been generated from FLINT's `fq_nmod_types.h`. DO NOT EDIT MANUALLY.
#
# To regenerate it, e.g. after updating FLINT_jll, run
#   julia etc/generate_FLINT_structs.jl
#
# Generated with FLINT_jll v301.600.1+0.

const fq_nmod_t = nmod_poly_t

const fq_nmod_struct = nmod_poly_struct

struct fq_nmod_ctx_struct <: FlintStruct
  mod::nmod_t

  sparse_modulus::Cint
  is_conway::Cint

  a::Ptr{ulong}
  j::Ptr{slong}
  len::slong

  modulus::nmod_poly_t
  inv::nmod_poly_t

  var::Ptr{Cchar}
end

const fq_nmod_ctx_t = Tuple{fq_nmod_ctx_struct}

struct fq_nmod_mat_struct <: FlintStruct
  entries::Ptr{fq_nmod_struct}
  r::slong
  c::slong
  stride::slong
end

const fq_nmod_mat_t = Tuple{fq_nmod_mat_struct}

struct fq_nmod_poly_struct <: FlintStruct
  coeffs::Ptr{fq_nmod_struct}
  alloc::slong
  length::slong
end

const fq_nmod_poly_t = Tuple{fq_nmod_poly_struct}

struct fq_nmod_poly_factor_struct <: FlintStruct
  poly::Ptr{fq_nmod_poly_struct}
  exp::Ptr{slong}
  num::slong
  alloc::slong
end

const fq_nmod_poly_factor_t = Tuple{fq_nmod_poly_factor_struct}

struct fq_nmod_mpoly_struct <: FlintStruct
  coeffs::Ptr{ulong}
  exps::Ptr{ulong}
  length::slong
  bits::flint_bitcnt_t
  coeffs_alloc::slong
  exps_alloc::slong
end

const fq_nmod_mpoly_t = Tuple{fq_nmod_mpoly_struct}
