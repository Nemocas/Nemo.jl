# This file has been generated from FLINT's `fq_default_poly.h`. DO NOT EDIT MANUALLY.
#
# To regenerate it, e.g. after updating FLINT_jll, run
#   julia etc/generate_FLINT_structs.jl
#
# Generated with FLINT_jll v301.600.1+0.

struct fq_default_poly_struct <: FlintStruct
  uniondata::NTuple{cld(maximum(sizeof, (
    fq_poly_t,
    fq_nmod_poly_t,
    fq_zech_poly_t,
    nmod_poly_t,
    fmpz_mod_poly_t,
  )), sizeof(UInt)), UInt}
end
const union_fq_default_poly_struct = fq_default_poly_struct

const fq_default_poly_t = Tuple{fq_default_poly_struct}
