# This file has been generated from FLINT's `fmpz_mod_types.h`. DO NOT EDIT MANUALLY.
#
# To regenerate it, e.g. after updating FLINT_jll, run
#   julia etc/generate_FLINT_structs.jl
#
# Generated with FLINT_jll v301.600.1+0.

struct fmpz_mod_ctx_struct <: FlintStruct
  n::fmpz_t
  add_fxn::Ptr{Cvoid}
  sub_fxn::Ptr{Cvoid}
  mul_fxn::Ptr{Cvoid}
  mod::nmod_t
  n_limbs::NTuple{3, ulong}
  ninv_limbs::NTuple{3, ulong}
  ninv_huge::Ptr{fmpz_preinvn_struct}
end
const struct_fmpz_mod_ctx = fmpz_mod_ctx_struct

const fmpz_mod_ctx_t = Tuple{fmpz_mod_ctx_struct}

const fmpz_mod_mat_struct = fmpz_mat_struct

const fmpz_mod_mat_t = Tuple{fmpz_mod_mat_struct}

struct fmpz_mod_poly_struct <: FlintStruct
  coeffs::Ptr{fmpz}
  alloc::slong
  length::slong
end

const fmpz_mod_poly_t = Tuple{fmpz_mod_poly_struct}

struct fmpz_mod_poly_factor_struct <: FlintStruct
  poly::Ptr{fmpz_mod_poly_struct}
  exp::Ptr{slong}
  num::slong
  alloc::slong
end

const fmpz_mod_poly_factor_t = Tuple{fmpz_mod_poly_factor_struct}

struct fmpz_mod_mpoly_struct <: FlintStruct
  coeffs::Ptr{fmpz}
  exps::Ptr{ulong}
  length::slong
  bits::flint_bitcnt_t
  coeffs_alloc::slong
  exps_alloc::slong
end

const fmpz_mod_mpoly_t = Tuple{fmpz_mod_mpoly_struct}

struct fmpz_mod_mpoly_factor_struct <: FlintStruct
  constant::fmpz_t
  poly::Ptr{fmpz_mod_mpoly_struct}
  exp::Ptr{fmpz}
  num::slong
  alloc::slong
end

const fmpz_mod_mpoly_factor_t = Tuple{fmpz_mod_mpoly_factor_struct}

struct fmpz_mod_mpoly_q_struct <: FlintStruct
  num::fmpz_mod_mpoly_struct
  den::fmpz_mod_mpoly_struct
end

const fmpz_mod_mpoly_q_t = Tuple{fmpz_mod_mpoly_q_struct}
