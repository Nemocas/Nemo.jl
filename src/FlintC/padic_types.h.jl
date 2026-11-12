# This file has been generated from FLINT's `padic_types.h`. DO NOT EDIT MANUALLY.
#
# To regenerate it, e.g. after updating FLINT_jll, run
#   julia etc/generate_FLINT_structs.jl
#
# Generated with FLINT_jll v301.600.1+0.

struct padic_struct <: FlintStruct
  u::fmpz
  v::slong
  N::slong
end

const padic_t = Tuple{padic_struct}

@enum enum_padic_print_mode begin
  PADIC_TERSE
  PADIC_SERIES
  PADIC_VAL_UNIT
end

struct padic_ctx_struct <: FlintStruct
  p::fmpz_t
  pinv::Cdouble
  pow::Ptr{fmpz}
  min::slong
  max::slong
  mode::enum_padic_print_mode
end

const padic_ctx_t = Tuple{padic_ctx_struct}

struct padic_inv_struct <: FlintStruct
  n::slong
  pow::Ptr{fmpz}
end

const padic_inv_t = Tuple{padic_inv_struct}

struct padic_mat_struct <: FlintStruct
  mat::fmpz_mat_struct
  val::slong
  N::slong
end

const padic_mat_t = Tuple{padic_mat_struct}

struct padic_poly_struct <: FlintStruct
  coeffs::Ptr{fmpz}
  alloc::slong
  length::slong
  val::slong
  N::slong
end

const padic_poly_t = Tuple{padic_poly_struct}
