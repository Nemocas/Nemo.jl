# This file has been generated from FLINT's `fq_nmod_mpoly.h`. DO NOT EDIT MANUALLY.
#
# To regenerate it, e.g. after updating FLINT_jll, run
#   julia etc/generate_FLINT_structs.jl
#
# Generated with FLINT_jll v301.600.1+0.

struct fq_nmod_mpoly_univar_struct <: FlintStruct
  coeffs::Ptr{fq_nmod_mpoly_struct}
  exps::Ptr{fmpz}
  alloc::slong
  length::slong
end

const fq_nmod_mpoly_univar_t = Tuple{fq_nmod_mpoly_univar_struct}

struct fq_nmod_mpolyu_struct <: FlintStruct
  coeffs::Ptr{fq_nmod_mpoly_struct}
  exps::Ptr{ulong}
  alloc::slong
  length::slong
  bits::flint_bitcnt_t
end

const fq_nmod_mpolyu_t = Tuple{fq_nmod_mpolyu_struct}

struct fq_nmod_mpolyn_struct <: FlintStruct
  coeffs::Ptr{n_fq_poly_struct}
  exps::Ptr{ulong}
  alloc::slong
  length::slong
  bits::slong
end

const fq_nmod_mpolyn_t = Tuple{fq_nmod_mpolyn_struct}

struct fq_nmod_mpolyun_struct <: FlintStruct
  coeffs::Ptr{fq_nmod_mpolyn_struct}
  exps::Ptr{ulong}
  alloc::slong
  length::slong
  bits::flint_bitcnt_t
end

const fq_nmod_mpolyun_t = Tuple{fq_nmod_mpolyun_struct}

struct bad_fq_nmod_embed_struct <: FlintStruct
  smctx::Ptr{fq_nmod_ctx_struct}
  phi_sm::fq_nmod_poly_t
  h::fq_nmod_poly_t
  h_as_n_fq_poly::n_fq_poly_t
  lgctx::Ptr{fq_nmod_ctx_struct}
  theta_lg::fq_nmod_t
  x_lg::fq_nmod_t
  lg_to_sm_mat::nmod_mat_t
  sm_to_lg_mat::nmod_mat_t
end
const struct_bad_fq_nmod_embed = bad_fq_nmod_embed_struct

const bad_fq_nmod_embed_t = Tuple{bad_fq_nmod_embed_struct}

struct bad_fq_nmod_mpoly_embed_chooser_struct <: FlintStruct
  embed::Ptr{bad_fq_nmod_embed_struct}
  m::slong
  n::slong
  k::slong
  p::ulong
end
const struct_bad_fq_nmod_mpoly_embed_chooser = bad_fq_nmod_mpoly_embed_chooser_struct

const bad_fq_nmod_mpoly_embed_chooser_t = Tuple{bad_fq_nmod_mpoly_embed_chooser_struct}

struct fq_nmod_mpoly_geobucket_struct <: FlintStruct
  polys::NTuple{FLINT_BITS÷2, fq_nmod_mpoly_struct}
  temps::NTuple{FLINT_BITS÷2, fq_nmod_mpoly_struct}
  length::slong
end
const struct_fq_nmod_mpoly_geobucket = fq_nmod_mpoly_geobucket_struct

const fq_nmod_mpoly_geobucket_t = Tuple{fq_nmod_mpoly_geobucket_struct}
