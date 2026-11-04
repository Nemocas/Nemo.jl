# This file has been generated from FLINT's `fmpq_types.h`. DO NOT EDIT MANUALLY.
#
# To regenerate it, e.g. after updating FLINT_jll, run
#   julia etc/generate_FLINT_structs.jl
#
# Generated with FLINT_jll v301.600.1+0.

struct fmpq_mat_struct <: FlintStruct
  entries::Ptr{fmpq}
  r::slong
  c::slong
  stride::slong
end

const fmpq_mat_t = Tuple{fmpq_mat_struct}

struct fmpq_poly_struct <: FlintStruct
  coeffs::Ptr{fmpz}
  alloc::slong
  length::slong
  den::fmpz_t
end

const fmpq_poly_t = Tuple{fmpq_poly_struct}

struct fmpq_mpoly_struct <: FlintStruct
  content::fmpq_t
  zpoly::fmpz_mpoly_t
end

const fmpq_mpoly_t = Tuple{fmpq_mpoly_struct}

struct fmpq_mpoly_factor_struct <: FlintStruct
  constant::fmpq_t
  poly::Ptr{fmpq_mpoly_struct}
  exp::Ptr{fmpz}
  num::slong
  alloc::slong
end

const fmpq_mpoly_factor_t = Tuple{fmpq_mpoly_factor_struct}
