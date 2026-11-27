# This file has been generated from FLINT's `fq_default_mat.h`. DO NOT EDIT MANUALLY.
#
# To regenerate it, e.g. after updating FLINT_jll, run
#   julia etc/generate_FLINT_structs.jl
#
# Generated with FLINT_jll v301.600.1+0.

struct fq_default_mat_struct <: FlintStruct
  uniondata::NTuple{cld(maximum(sizeof, (
    fq_mat_t,
    fq_nmod_mat_t,
    fq_zech_mat_t,
    nmod_mat_t,
    fmpz_mod_mat_t,
  )), sizeof(UInt)), UInt}
end
const union_fq_default_mat_struct = fq_default_mat_struct

const fq_default_mat_t = Tuple{fq_default_mat_struct}
