# This file has been generated from FLINT's `qadic.h`. DO NOT EDIT MANUALLY.
#
# To regenerate it, e.g. after updating FLINT_jll, run
#   julia etc/generate_FLINT_structs.jl
#
# Generated with FLINT_jll v301.600.1+0.

const qadic_t = padic_poly_t

const qadic_struct = padic_poly_struct

struct qadic_ctx_struct <: FlintStruct
  pctx::padic_ctx_struct

  a::Ptr{fmpz}
  j::Ptr{slong}
  len::slong

  var::Ptr{Cchar}
end

const qadic_ctx_t = Tuple{qadic_ctx_struct}

struct struct_qadic2_sqrt_precomp <: FlintStruct end
