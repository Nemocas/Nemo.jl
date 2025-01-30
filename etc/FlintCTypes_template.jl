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

#
# from gmp.h
#

const mp_limb_t = Base.GMP.Limb
const mp_limb_signed_t = signed(Base.GMP.Limb)
const mp_bitcnt_t = Base.GMP.MPZ.bitcnt_t

const mp_ptr = Ptr{mp_limb_t}
const mp_srcptr = Ptr{mp_limb_t}

#
# from flint.h
#

const ulong = mp_limb_t
const slong = mp_limb_signed_t
const flint_bitcnt_t = ulong
const FLINT_BITS = Base.GMP.BITS_PER_LIMB

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
