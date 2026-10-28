# This file has been generated from FLINT's `flint.h`. DO NOT EDIT MANUALLY.
#
# To regenerate it, e.g. after updating FLINT_jll, run
#   julia etc/generate_FLINT_structs.jl
#
# Generated with FLINT_jll v301.600.1+0.

const __FLINT_VERSION = 3

const __FLINT_VERSION_MINOR = 6

const __FLINT_VERSION_PATCHLEVEL = 0

struct struct___FLINT_FILE <: FlintStruct end

const FLINT_FILE = struct___FLINT_FILE

const flint_bitcnt_t = ulong

const nn_ptr = Ptr{ulong}

const nn_srcptr = Ptr{ulong}

const flint_cleanup_function_t = Ptr{Cvoid}

const thread_pool_handle = Cint

struct flint_rand_struct <: FlintStruct
  __gmp_state::Ptr{Cvoid}
  __randval::ulong
  __randval2::ulong
end

const flint_rand_t = Tuple{flint_rand_struct}

@enum flint_err_t begin
  FLINT_ERROR
  FLINT_OVERFLOW
  FLINT_IMPINV
  FLINT_DOMERR
  FLINT_DIVZERO
  FLINT_EXPOF
  FLINT_INEXACT
  FLINT_TEST_FAIL
end

struct nmod_t <: FlintStruct
  n::ulong
  ninv::ulong
  norm::flint_bitcnt_t
end

const fmpz = slong

const fmpz_t = Tuple{fmpz}

struct fmpq <: FlintStruct
  num::fmpz
  den::fmpz
end

const fmpq_t = Tuple{fmpq}

const MPZ_MIN_ALLOC = 2
