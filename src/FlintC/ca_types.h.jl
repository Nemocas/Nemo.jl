# This file has been generated from FLINT's `ca_types.h`. DO NOT EDIT MANUALLY.
#
# To regenerate it, e.g. after updating FLINT_jll, run
#   julia etc/generate_FLINT_structs.jl
#
# Generated with FLINT_jll v301.600.1+0.

@enum calcium_func_code begin

  CA_QQBar

  CA_Neg
  CA_Add
  CA_Sub
  CA_Mul
  CA_Div

  CA_Sqrt
  CA_Cbrt
  CA_Root

  CA_Floor
  CA_Ceil
  CA_Abs
  CA_Sign
  CA_Re
  CA_Im
  CA_Arg
  CA_Conjugate

  CA_Pi

  CA_Sin
  CA_Cos
  CA_Exp
  CA_Log
  CA_Pow
  CA_Tan
  CA_Cot
  CA_Cosh
  CA_Sinh
  CA_Tanh
  CA_Coth
  CA_Atan
  CA_Acos
  CA_Asin
  CA_Acot
  CA_Atanh
  CA_Acosh
  CA_Asinh
  CA_Acoth

  CA_Euler

  CA_Gamma
  CA_LogGamma
  CA_Psi
  CA_Erf
  CA_Erfc
  CA_Erfi
  CA_RiemannZeta
  CA_HurwitzZeta
  CA_FUNC_CODE_LENGTH
end

struct ca_elem_struct <: FlintStruct
  uniondata::NTuple{cld(maximum(sizeof, (
    fmpq,
    nf_elem_struct,
    Ptr{fmpz_mpoly_q_struct},
  )), sizeof(UInt)), UInt}
end

struct ca_struct <: FlintStruct
  field::ulong
  elem::ca_elem_struct
end

const ca_t = Tuple{ca_struct}

const ca_ptr = Ptr{ca_struct}

const ca_srcptr = Ptr{ca_struct}

struct ca_ext_qqbar <: FlintStruct
  x::qqbar_struct
  nf::Ptr{nf_struct}
end

struct ca_ext_func_data <: FlintStruct
  args::Ptr{ca_struct}
  nargs::slong
  enclosure::acb_struct
  prec::slong
  qqbar::Ptr{qqbar_struct}
end

struct ca_ext_struct <: FlintStruct
  head::calcium_func_code
  hash::ulong
  depth::slong
  data::NTuple{cld(maximum(sizeof, (ca_ext_qqbar, ca_ext_func_data,)), sizeof(UInt)), UInt}
end

const ca_ext_t = Tuple{ca_ext_struct}

const ca_ext_ptr = Ptr{ca_ext_struct}

const ca_ext_srcptr = Ptr{ca_ext_struct}

struct ca_ext_cache_struct <: FlintStruct
  items::Ptr{Ptr{ca_ext_struct}}
  length::slong
  alloc::slong

  hash_size::slong
  hash_table::Ptr{slong}
end

const ca_ext_cache_t = Tuple{ca_ext_cache_struct}

struct ca_field_struct <: FlintStruct
  length::slong
  ext::Ptr{Ptr{ca_ext_struct}}
  ideal::fmpz_mpoly_vec_struct
  hash::ulong
end

const ca_field_t = Tuple{ca_field_struct}

const ca_field_ptr = Ptr{ca_field_struct}

const ca_field_srcptr = Ptr{ca_field_struct}

struct ca_field_cache_struct <: FlintStruct
  items::Ptr{Ptr{ca_field_struct}}
  length::slong
  alloc::slong

  hash_size::slong
  hash_table::Ptr{slong}
end

const ca_field_cache_t = Tuple{ca_field_cache_struct}

struct ca_ctx_struct <: FlintStruct
  ext_cache::ca_ext_cache_struct
  field_cache::ca_field_cache_struct
  field_qq::Ptr{ca_field_struct}
  field_qq_i::Ptr{ca_field_struct}
  mctx::Ptr{Ptr{fmpz_mpoly_ctx_struct}}
  mctx_len::slong
  options::Ptr{slong}
end

const ca_ctx_t = Tuple{ca_ctx_struct}

struct ca_factor_struct <: FlintStruct
  base::ca_ptr
  exp::ca_ptr
  length::slong
  alloc::slong
end

const ca_factor_t = Tuple{ca_factor_struct}

struct ca_vec_struct <: FlintStruct
  entries::ca_ptr
  alloc::slong
  length::slong
end

const ca_vec_t = Tuple{ca_vec_struct}

struct ca_mat_struct <: FlintStruct
  entries::ca_ptr
  r::slong
  c::slong
  stride::slong
end

const ca_mat_t = Tuple{ca_mat_struct}

struct ca_poly_struct <: FlintStruct
  coeffs::Ptr{ca_struct}
  alloc::slong
  length::slong
end

const ca_poly_t = Tuple{ca_poly_struct}
