# This file has been generated from FLINT's `fq_default.h`. DO NOT EDIT MANUALLY.
#
# To regenerate it, e.g. after updating FLINT_jll, run
#   julia etc/generate_FLINT_structs.jl
#
# Generated with FLINT_jll v301.600.1+0.

const FQ_DEFAULT_FQ_ZECH = 1

const FQ_DEFAULT_FQ_NMOD = 2

const FQ_DEFAULT_FQ = 3

const FQ_DEFAULT_NMOD = 4

const FQ_DEFAULT_FMPZ_MOD = 5

struct fq_default_struct <: FlintStruct
  uniondata::NTuple{cld(maximum(sizeof, (
    fq_t,
    fq_nmod_t,
    fq_zech_t,
    ulong,
    fmpz_t,
  )), sizeof(UInt)), UInt}
end
const union_fq_default_struct = fq_default_struct

const fq_default_t = Tuple{fq_default_struct}

const fq_default_ctx_struct = gr_ctx_struct

const fq_default_ctx_t = Tuple{fq_default_ctx_struct}

struct _gr_fmpz_mod_ctx_struct <: FlintStruct
  ctx::Ptr{fmpz_mod_ctx_struct}
  is_prime::truth_t
  a::fmpz
end
