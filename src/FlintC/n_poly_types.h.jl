# This file has been generated from FLINT's `n_poly_types.h`. DO NOT EDIT MANUALLY.
#
# To regenerate it, e.g. after updating FLINT_jll, run
#   julia etc/generate_FLINT_structs.jl
#
# Generated with FLINT_jll v301.600.1+0.

struct n_poly_struct <: FlintStruct
  coeffs::Ptr{ulong}
  alloc::slong
  length::slong
end

const n_poly_t = Tuple{n_poly_struct}

const n_fq_poly_struct = n_poly_struct

const n_fq_poly_t = n_poly_t

struct n_bpoly_struct <: FlintStruct
  coeffs::Ptr{n_poly_struct}
  alloc::slong
  length::slong
end

const n_bpoly_t = Tuple{n_bpoly_struct}

const n_fq_bpoly_struct = n_bpoly_struct

const n_fq_bpoly_t = n_bpoly_t

struct n_tpoly_struct <: FlintStruct
  coeffs::Ptr{n_bpoly_struct}
  alloc::slong
  length::slong
end

const n_tpoly_t = Tuple{n_tpoly_struct}

const n_fq_tpoly_struct = n_tpoly_struct

const n_fq_tpoly_t = n_tpoly_t

struct n_polyu_struct <: FlintStruct
  exps::Ptr{ulong}
  coeffs::Ptr{ulong}
  length::slong
  alloc::slong
end

const n_polyu_t = Tuple{n_polyu_struct}

const n_fq_polyu_struct = n_polyu_struct

const n_fq_polyu_t = n_polyu_t

struct n_polyun_struct <: FlintStruct
  coeffs::Ptr{n_poly_struct}
  exps::Ptr{ulong}
  length::slong
  alloc::slong
end

const n_polyun_t = Tuple{n_polyun_struct}

const n_fq_polyun_struct = n_polyun_struct

const n_fq_polyun_t = n_polyun_t

struct n_poly_stack_struct <: FlintStruct
  array::Ptr{Ptr{n_poly_struct}}
  alloc::slong
  top::slong
end

const n_poly_stack_t = Tuple{n_poly_stack_struct}

struct n_bpoly_stack_struct <: FlintStruct
  array::Ptr{Ptr{n_bpoly_struct}}
  alloc::slong
  top::slong
end

const n_bpoly_stack_t = Tuple{n_bpoly_stack_struct}

struct n_poly_bpoly_stack_struct <: FlintStruct
  poly_stack::n_poly_stack_t
  bpoly_stack::n_bpoly_stack_t
end

const n_poly_bpoly_stack_t = Tuple{n_poly_bpoly_stack_struct}

struct nmod_eval_interp_struct <: FlintStruct
  M::Ptr{ulong}
  T::Ptr{ulong}
  Q::Ptr{ulong}
  array::Ptr{ulong}
  alloc::slong
  d::slong
  radix::slong
  w::ulong
end

const nmod_eval_interp_t = Tuple{nmod_eval_interp_struct}
