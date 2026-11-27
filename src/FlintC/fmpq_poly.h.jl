# This file has been generated from FLINT's `fmpq_poly.h`. DO NOT EDIT MANUALLY.
#
# To regenerate it, e.g. after updating FLINT_jll, run
#   julia etc/generate_FLINT_structs.jl
#
# Generated with FLINT_jll v301.600.1+0.

struct fmpq_poly_powers_precomp_struct <: FlintStruct
  powers::Ptr{fmpq_poly_struct}
  len::slong
end

const fmpq_poly_powers_precomp_t = Tuple{fmpq_poly_powers_precomp_struct}

const WEAK_CANONICALISE_CUTOFF = 25600
