# This file has been generated from FLINT's `qqbar.h`. DO NOT EDIT MANUALLY.
#
# To regenerate it, e.g. after updating FLINT_jll, run
#   julia etc/generate_FLINT_structs.jl
#
# Generated with FLINT_jll v301.600.1+0.

struct qqbar_struct <: FlintStruct
  poly::fmpz_poly_struct
  enclosure::acb_struct
end

const qqbar_t = Tuple{qqbar_struct}

const qqbar_ptr = Ptr{qqbar_struct}

const qqbar_srcptr = Ptr{qqbar_struct}

const QQBAR_DEFAULT_PREC = 128

const QQBAR_ROOTS_IRREDUCIBLE = 1

const QQBAR_ROOTS_UNSORTED = 2

const QQBAR_FORMULA_GAUSSIANS = 1

const QQBAR_FORMULA_QUADRATICS = 2

const QQBAR_FORMULA_CYCLOTOMICS = 4

const QQBAR_FORMULA_CUBICS = 8

const QQBAR_FORMULA_QUARTICS = 16

const QQBAR_FORMULA_QUINTICS = 32

const QQBAR_FORMULA_DEPRESSION = 64

const QQBAR_FORMULA_DEFLATION = 128

const QQBAR_FORMULA_SEPARATION = 256

const QQBAR_FORMULA_EXP_FORM = 2048

const QQBAR_FORMULA_TRIG_FORM = 4096

const QQBAR_FORMULA_RADICAL_FORM = 8192

const QQBAR_FORMULA_AUTO_FORM = 0
