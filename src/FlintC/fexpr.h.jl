# This file has been generated from FLINT's `fexpr.h`. DO NOT EDIT MANUALLY.
#
# To regenerate it, e.g. after updating FLINT_jll, run
#   julia etc/generate_FLINT_structs.jl
#
# Generated with FLINT_jll v301.600.1+0.

const FEXPR_TYPE_BITS = 4

struct fexpr_struct <: FlintStruct
  data::Ptr{ulong}
  alloc::slong
end

const fexpr_t = Tuple{fexpr_struct}

const fexpr_ptr = Ptr{fexpr_struct}

const fexpr_srcptr = Ptr{fexpr_struct}

struct fexpr_vec_struct <: FlintStruct
  entries::Ptr{fexpr_struct}
  alloc::slong
  length::slong
end

const fexpr_vec_t = Tuple{fexpr_vec_struct}

const FEXPR_LATEX_SMALL = 1

const FEXPR_LATEX_LOGIC = 2
