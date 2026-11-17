# This file has been generated from FLINT's `nmod_types.h`. DO NOT EDIT MANUALLY.
#
# To regenerate it, e.g. after updating FLINT_jll, run
#   julia etc/generate_FLINT_structs.jl
#
# Generated with FLINT_jll v301.600.1+0.

struct nmod_mat_struct <: FlintStruct
  entries::Ptr{ulong}
  r::slong
  c::slong
  stride::slong
  mod::nmod_t
end

const nmod_mat_t = Tuple{nmod_mat_struct}

struct nmod_poly_struct <: FlintStruct
  coeffs::nn_ptr
  alloc::slong
  length::slong
  mod::nmod_t
end

const nmod_poly_t = Tuple{nmod_poly_struct}

struct nmod_poly_factor_struct <: FlintStruct
  p::Ptr{nmod_poly_struct}
  exp::Ptr{slong}
  num::slong
  alloc::slong
end

const nmod_poly_factor_t = Tuple{nmod_poly_factor_struct}

struct nmod_poly_mat_struct <: FlintStruct
  entries::Ptr{nmod_poly_struct}
  r::slong
  c::slong
  stride::slong
  modulus::ulong
end

const nmod_poly_mat_t = Tuple{nmod_poly_mat_struct}

struct nmod_mpoly_struct <: FlintStruct
  coeffs::Ptr{ulong}
  exps::Ptr{ulong}
  length::slong
  bits::flint_bitcnt_t
  coeffs_alloc::slong
  exps_alloc::slong
end

const nmod_mpoly_t = Tuple{nmod_mpoly_struct}

struct nmod_mpoly_factor_struct <: FlintStruct
  constant::ulong
  poly::Ptr{nmod_mpoly_struct}
  exp::Ptr{fmpz}
  num::slong
  alloc::slong
end

const nmod_mpoly_factor_t = Tuple{nmod_mpoly_factor_struct}

@enum dot_method_t begin
  _DOT0 = 0
  _DOT1 = 1

  _DOT2_SPLIT = 2

  _DOT2_HALF = 3
  _DOT2 = 4
  _DOT3_ACC = 5
  _DOT3 = 6
  _DOT_POW2 = 7
end

struct dot_params_t <: FlintStruct
  method::dot_method_t
  pow2_precomp::ulong
end
