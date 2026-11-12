# This file has been generated from FLINT's `fq_zech_types.h`. DO NOT EDIT MANUALLY.
#
# To regenerate it, e.g. after updating FLINT_jll, run
#   julia etc/generate_FLINT_structs.jl
#
# Generated with FLINT_jll v301.600.1+0.

struct fq_zech_struct <: FlintStruct
  value::ulong
end

const fq_zech_t = Tuple{fq_zech_struct}

struct fq_zech_ctx_struct <: FlintStruct
  qm1::ulong
  qm1o2::ulong
  qm1opm1::ulong
  p::ulong
  ppre::Cdouble
  prime_root::ulong
  zech_log_table::Ptr{ulong}
  prime_field_table::Ptr{ulong}
  eval_table::Ptr{ulong}

  fq_nmod_ctx::Ptr{fq_nmod_ctx_struct}
  owns_fq_nmod_ctx::Cint
  is_conway::Cint
end

const fq_zech_ctx_t = Tuple{fq_zech_ctx_struct}

struct fq_zech_mat_struct <: FlintStruct
  entries::Ptr{fq_zech_struct}
  r::slong
  c::slong
  stride::slong
end

const fq_zech_mat_t = Tuple{fq_zech_mat_struct}

struct fq_zech_poly_struct <: FlintStruct
  coeffs::Ptr{fq_zech_struct}
  alloc::slong
  length::slong
end

const fq_zech_poly_t = Tuple{fq_zech_poly_struct}

struct fq_zech_poly_factor_struct <: FlintStruct
  poly::Ptr{fq_zech_poly_struct}
  exp::Ptr{slong}
  num::slong
  alloc::slong
end

const fq_zech_poly_factor_t = Tuple{fq_zech_poly_factor_struct}
