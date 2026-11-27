# This file has been generated from FLINT's `arf_types.h`. DO NOT EDIT MANUALLY.
#
# To regenerate it, e.g. after updating FLINT_jll, run
#   julia etc/generate_FLINT_structs.jl
#
# Generated with FLINT_jll v301.600.1+0.

const ARF_NOPTR_LIMBS = 2

struct mantissa_noptr_struct <: FlintStruct
  d::NTuple{ARF_NOPTR_LIMBS, ulong}
end

struct mantissa_ptr_struct <: FlintStruct
  alloc::slong
  d::nn_ptr
end

struct mantissa_struct <: FlintStruct
  uniondata::NTuple{cld(maximum(sizeof, (
    mantissa_noptr_struct,
    mantissa_ptr_struct,
  )), sizeof(UInt)), UInt}
end

struct arf_struct <: FlintStruct
  exp::fmpz
  size::slong
  d::mantissa_struct
end

const arf_t = Tuple{arf_struct}

const arf_ptr = Ptr{arf_struct}

const arf_srcptr = Ptr{arf_struct}

struct arf_interval_struct <: FlintStruct
  a::arf_struct
  b::arf_struct
end

const arf_interval_t = Tuple{arf_interval_struct}

const arf_interval_ptr = Ptr{arf_interval_struct}

const arf_interval_srcptr = Ptr{arf_interval_struct}
