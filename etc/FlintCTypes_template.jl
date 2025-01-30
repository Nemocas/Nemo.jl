###############################################################################
#
#   Flint C types
#
###############################################################################

module FlintC

#
# C types (names provided to ease automatic conversion of struct definitions)
#
const void = Cvoid
const int = Cint
const unsigned_int = Cuint
const char = Cchar
const unsigned_char = Cuchar
const double = Cdouble
const ulong = UInt
const slong = Int


#
# from flint.h
#

const flint_bitcnt_t = ulong
const nn_ptr = Ptr{ulong}
const nn_srcptr = Ptr{ulong}
const FLINT_BITS = UInt == UInt64 ? 64 : 32

struct nmod_t
  n::mp_limb_t
  ninv::mp_limb_t
  norm::flint_bitcnt_t
end

const fmpz = slong
const fmpz_t = Ptr{fmpz}

struct fmpq
  num::fmpz
  den::fmpz
end

const fmpq_t = Ptr{fmpq}


@include_c_header("limb_types.h")

@include_c_header("nmod_types.h")

@include_c_header("fmpz_types.h")

@include_c_header("fmpq_types.h")

@include_c_header("fmpz_mod_types.h")

@include_c_header("fq_nmod_types.h")

@include_c_header("fq_zech_types.h")

@include_c_header("fq_types.h")

@include_c_header("padic_types.h")

@include_c_header("n_poly_types.h")

@include_c_header("mpoly_types.h")

end # module FlintC
