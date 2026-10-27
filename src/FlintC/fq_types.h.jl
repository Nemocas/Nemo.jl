# This file has been generated from FLINT's `fq_types.h`. DO NOT EDIT MANUALLY.
#
# To regenerate it, e.g. after updating FLINT_jll, run
#   julia etc/generate_FLINT_structs.jl
#
# Generated with FLINT_jll v301.600.1+0.

const fq_t = fmpz_poly_t

const fq_struct = fmpz_poly_struct

struct fq_ctx_struct <: FlintStruct
  ctxp::fmpz_mod_ctx_t

  sparse_modulus::Cint
  is_conway::Cint

  a::Ptr{fmpz}
  j::Ptr{slong}
  len::slong

  modulus::fmpz_mod_poly_t
  inv::fmpz_mod_poly_t

  var::Ptr{Cchar}
end

const fq_ctx_t = Tuple{fq_ctx_struct}

struct fq_mat_struct <: FlintStruct
  entries::Ptr{fq_struct}
  r::slong
  c::slong
  stride::slong
end

const fq_mat_t = Tuple{fq_mat_struct}

struct fq_poly_struct <: FlintStruct
  coeffs::Ptr{fq_struct}
  alloc::slong
  length::slong
end

const fq_poly_t = Tuple{fq_poly_struct}

struct fq_poly_factor_struct <: FlintStruct
  poly::Ptr{fq_poly_struct}
  exp::Ptr{slong}
  num::slong
  alloc::slong
end

const fq_poly_factor_t = Tuple{fq_poly_factor_struct}
