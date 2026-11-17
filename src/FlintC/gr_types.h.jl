# This file has been generated from FLINT's `gr_types.h`. DO NOT EDIT MANUALLY.
#
# To regenerate it, e.g. after updating FLINT_jll, run
#   julia etc/generate_FLINT_structs.jl
#
# Generated with FLINT_jll v301.600.1+0.

const GR_SUCCESS = 0

const GR_DOMAIN = 1

const GR_UNABLE = 2

const GR_TEST_FAIL = 4

@enum truth_t begin
  T_TRUE
  T_FALSE
  T_UNKNOWN
end

struct gr_stream_struct <: FlintStruct
  fp::Ptr{FLINT_FILE}
  s::Ptr{Cchar}
  len::slong
  alloc::slong
end

const gr_stream_t = Tuple{gr_stream_struct}

const gr_funcptr = Ptr{Cvoid}

const GR_CTX_STRUCT_DATA_BYTES = 6 * sizeof(ulong)

struct gr_ctx_struct <: FlintStruct
  data::NTuple{GR_CTX_STRUCT_DATA_BYTES, Cchar}
  which_ring::ulong
  sizeof_elem::slong
  methods::Ptr{gr_funcptr}
  size_limit::ulong
end

const gr_ctx_t = Tuple{gr_ctx_struct}

const gr_ptr = Ptr{Cvoid}

const gr_srcptr = Ptr{Cvoid}

const gr_ctx_ptr = Ptr{Cvoid}

struct gr_vec_struct <: FlintStruct
  entries::gr_ptr
  alloc::slong
  length::slong
end

const gr_vec_t = Tuple{gr_vec_struct}

struct gr_mat_struct <: FlintStruct
  entries::gr_ptr
  r::slong
  c::slong
  stride::slong
end

const gr_mat_t = Tuple{gr_mat_struct}

struct gr_poly_struct <: FlintStruct
  coeffs::gr_ptr
  alloc::slong
  length::slong
end

const gr_poly_t = Tuple{gr_poly_struct}

struct gr_poly_vec_struct <: FlintStruct
  entries::Ptr{gr_poly_struct}
  alloc::slong
  length::slong
end

const gr_poly_vec_t = Tuple{gr_poly_vec_struct}
