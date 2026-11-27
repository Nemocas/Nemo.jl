# This file has been generated from FLINT's `fmpz_types.h`. DO NOT EDIT MANUALLY.
#
# To regenerate it, e.g. after updating FLINT_jll, run
#   julia etc/generate_FLINT_structs.jl
#
# Generated with FLINT_jll v301.600.1+0.

struct zz_struct <: FlintStruct
  alloc::Cint
  size::Cint
  ptr::nn_ptr
end

const zz_ptr = Ptr{zz_struct}

const zz_srcptr = Ptr{zz_struct}

struct fmpz_factor_struct <: FlintStruct
  sign::Cint
  p::Ptr{fmpz}
  exp::Ptr{ulong}
  alloc::slong
  num::slong
end

const fmpz_factor_t = Tuple{fmpz_factor_struct}

struct fmpz_preinvn_struct <: FlintStruct
  dinv::nn_ptr
  n::slong
  norm::flint_bitcnt_t
end

const fmpz_preinvn_t = Tuple{fmpz_preinvn_struct}

struct fmpz_vec_struct <: FlintStruct
  entries::Ptr{fmpz}
  alloc::slong
  length::slong
end

const fmpz_vec_t = Tuple{fmpz_vec_struct}

struct fmpz_poly_struct <: FlintStruct
  coeffs::Ptr{fmpz}
  alloc::slong
  length::slong
end

const fmpz_poly_t = Tuple{fmpz_poly_struct}

struct fmpz_poly_factor_struct <: FlintStruct
  c::fmpz
  p::Ptr{fmpz_poly_struct}
  exp::Ptr{slong}
  num::slong
  alloc::slong
end

const fmpz_poly_factor_t = Tuple{fmpz_poly_factor_struct}

struct fmpz_mat_struct <: FlintStruct
  entries::Ptr{fmpz}
  r::slong
  c::slong
  stride::slong
end

const fmpz_mat_t = Tuple{fmpz_mat_struct}

struct fmpz_poly_mat_struct <: FlintStruct
  entries::Ptr{fmpz_poly_struct}
  r::slong
  c::slong
  stride::slong
end

const fmpz_poly_mat_t = Tuple{fmpz_poly_mat_struct}

struct fmpz_mpoly_struct <: FlintStruct
  coeffs::Ptr{fmpz}
  exps::Ptr{ulong}
  alloc::slong
  length::slong
  bits::flint_bitcnt_t
end

const fmpz_mpoly_t = Tuple{fmpz_mpoly_struct}

struct fmpz_mpoly_factor_struct <: FlintStruct
  constant::fmpz_t
  constant_den::fmpz_t
  poly::Ptr{fmpz_mpoly_struct}
  exp::Ptr{fmpz}
  num::slong
  alloc::slong
end

const fmpz_mpoly_factor_t = Tuple{fmpz_mpoly_factor_struct}

struct fmpz_poly_q_struct <: FlintStruct
  num::Ptr{fmpz_poly_struct}
  den::Ptr{fmpz_poly_struct}
end

const fmpz_poly_q_t = Tuple{fmpz_poly_q_struct}

struct fmpz_mpoly_q_struct <: FlintStruct
  num::fmpz_mpoly_struct
  den::fmpz_mpoly_struct
end

const fmpz_mpoly_q_t = Tuple{fmpz_mpoly_q_struct}

struct fmpzi_struct <: FlintStruct
  a::fmpz
  b::fmpz
end

const fmpzi_t = Tuple{fmpzi_struct}
