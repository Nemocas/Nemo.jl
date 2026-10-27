# This file has been generated from FLINT's `arb_types.h`. DO NOT EDIT MANUALLY.
#
# To regenerate it, e.g. after updating FLINT_jll, run
#   julia etc/generate_FLINT_structs.jl
#
# Generated with FLINT_jll v301.600.1+0.

struct mag_struct <: FlintStruct
  exp::fmpz
  man::ulong
end

const mag_t = Tuple{mag_struct}

const mag_ptr = Ptr{mag_struct}

const mag_srcptr = Ptr{mag_struct}

struct arb_struct <: FlintStruct
  mid::arf_struct
  rad::mag_struct
end

const arb_t = Tuple{arb_struct}

const arb_ptr = Ptr{arb_struct}

const arb_srcptr = Ptr{arb_struct}

struct arb_mat_struct <: FlintStruct
  entries::arb_ptr
  r::slong
  c::slong
  stride::slong
end

const arb_mat_t = Tuple{arb_mat_struct}

struct arb_poly_struct <: FlintStruct
  coeffs::arb_ptr
  alloc::slong
  length::slong
end

const arb_poly_t = Tuple{arb_poly_struct}
