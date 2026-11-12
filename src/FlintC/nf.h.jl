# This file has been generated from FLINT's `nf.h`. DO NOT EDIT MANUALLY.
#
# To regenerate it, e.g. after updating FLINT_jll, run
#   julia etc/generate_FLINT_structs.jl
#
# Generated with FLINT_jll v301.600.1+0.

struct nf_struct <: FlintStruct
  pol::fmpq_poly_t
  pinv::NTuple{cld(maximum(sizeof, (fmpz_preinvn_t,)), sizeof(UInt)), UInt}
  powers::NTuple{cld(maximum(sizeof, (fmpq_poly_powers_precomp_t, fmpz_poly_powers_precomp_t,)), sizeof(UInt)), UInt}
  traces::fmpq_poly_t
  flag::ulong
end

const nf_t = Tuple{nf_struct}

const NF_POWERS_CUTOFF = 30

const NF_GENERIC = 0

const NF_MONIC = 1

const NF_LINEAR = 2

const NF_QUADRATIC = 4

const NF_GAUSSIAN = 8
