# This file has been generated from FLINT's `acb_types.h`. DO NOT EDIT MANUALLY.
#
# To regenerate it, e.g. after updating FLINT_jll, run
#   julia etc/generate_FLINT_structs.jl
#
# Generated with FLINT_jll v301.600.1+0.

struct acb_struct <: FlintStruct
  real::arb_struct
  imag::arb_struct
end

const acb_t = Tuple{acb_struct}

const acb_ptr = Ptr{acb_struct}

const acb_srcptr = Ptr{acb_struct}

struct acb_mat_struct <: FlintStruct
  entries::acb_ptr
  r::slong
  c::slong
  stride::slong
end

const acb_mat_t = Tuple{acb_mat_struct}

struct acb_poly_struct <: FlintStruct
  coeffs::acb_ptr
  alloc::slong
  length::slong
end

const acb_poly_t = Tuple{acb_poly_struct}
