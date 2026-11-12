# This file has been generated from FLINT's `nf_elem.h`. DO NOT EDIT MANUALLY.
#
# To regenerate it, e.g. after updating FLINT_jll, run
#   julia etc/generate_FLINT_structs.jl
#
# Generated with FLINT_jll v301.600.1+0.

struct lnf_elem_struct <: FlintStruct
  num::fmpz_t
  den::fmpz_t
end

const lnf_elem_t = Tuple{lnf_elem_struct}

struct qnf_elem_struct <: FlintStruct
  num::NTuple{3, fmpz}
  den::fmpz_t
end

const qnf_elem_t = Tuple{qnf_elem_struct}

struct nf_elem_struct <: FlintStruct
  uniondata::NTuple{cld(maximum(sizeof, (
    fmpq_poly_t,
    lnf_elem_t,
    qnf_elem_t,
  )), sizeof(UInt)), UInt}
end

const nf_elem_t = Tuple{nf_elem_struct}
