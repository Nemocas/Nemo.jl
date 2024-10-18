###############################################################################
#
#   Flint C types
#
###############################################################################

module Flint

#
# C types (names provided to ease automatic conversion of struct definitions)
#
const int = Cint
const char = Cchar

#
# from flint.h
#
const ulong = Culong
const slong = Clong

const flint_bitcnt_t = ulong
const nn_ptr = Ptr{ulong}

const fmpz = Clong

const fmpz_t = Ptr{fmpz}

struct fmpq
    num::fmpz
    den::fmpz
end

const fmpq_t = Ptr{fmpq}

struct nmod_t
   n::ulong
   ninv::ulong
   norm::flint_bitcnt_t
end

#
# from limb_types.h
#

const FLINT_MAX_FACTORS_IN_LIMB = 15

struct n_factor_t
  num::int
  exp::NTuple{FLINT_MAX_FACTORS_IN_LIMB, int}
  p::NTuple{FLINT_MAX_FACTORS_IN_LIMB, ulong}
end

struct n_primes_struct
  small_i::slong
  small_num::slong
  small_primes::Ptr{Cuint}

  sieve_a::ulong
  sieve_b::ulong
  sieve_i::slong
  sieve_num::slong
  sieve::Ptr{char}
end

#
# from fmpz_types.h
#

struct zz_struct
  alloc::int
  size::int
  ptr::nn_ptr
end

const zz_ptr = Ptr{zz_struct}

struct fmpz_factor_struct
  sign::int
  p::Ptr{fmpz}
  exp::Ptr{ulong}
  alloc::slong
  num::slong
end

struct fmpz_preinvn_struct
  dinv::nn_ptr
  n::slong
  norm::flint_bitcnt_t
end

struct fmpz_poly_struct
  coeffs::Ptr{fmpz}
  alloc::slong
  length::slong
end

struct fmpz_poly_factor_struct
  c::fmpz
  p::Ptr{fmpz_poly_struct}
  exp::Ptr{slong}
  num::slong
  alloc::slong
end

struct fmpz_mat_struct
  entries::Ptr{fmpz}
  r::slong
  c::slong
  rows::Ptr{Ptr{fmpz}}
end

struct fmpz_poly_mat_struct
  entries::Ptr{fmpz_poly_struct}
  r::slong
  c::slong
  rows::Ptr{Ptr{fmpz_poly_struct}}
end

struct fmpz_mpoly_struct
  coeffs::Ptr{fmpz}
  exps::Ptr{ulong}
  alloc::slong
  length::slong
  bits::flint_bitcnt_t
end

struct fmpz_mpoly_factor_struct
  constant::fmpz_t
  constant_den::fmpz_t
  poly::Ptr{fmpz_mpoly_struct}
  exp::Ptr{fmpz}
  num::slong
  alloc::slong
end

struct fmpz_poly_q_struct
  num::Ptr{fmpz_poly_struct}
  den::Ptr{fmpz_poly_struct}
end

struct fmpz_mpoly_q_struct
  num::fmpz_mpoly_struct
  den::fmpz_mpoly_struct
end

struct fmpzi_struct
  a::fmpz
  b::fmpz
end


#
# from nmod_types.h
#
struct nmod_mat_struct
  entries::Ptr{ulong}
  r::slong
  c::slong
  rows::Ptr{Ptr{ulong}}
  mod::nmod_t
end

struct nmod_poly_struct
  coeffs::nn_ptr
  alloc::slong
  length::slong
  mod::nmod_t
end

const nmod_poly_t = Ptr{nmod_poly_struct}

struct nmod_poly_factor_struct
  p::Ptr{nmod_poly_struct}
  exp::Ptr{slong}
  num::slong
  alloc::slong
end

struct nmod_poly_mat_struct
  entries::Ptr{nmod_poly_struct}
  r::slong
  c::slong
  rows::Ptr{Ptr{nmod_poly_struct}}
  modulus::ulong
end

struct nmod_mpoly_struct
  coeffs::Ptr{ulong}
  exps::Ptr{ulong}
  length::slong
  bits::flint_bitcnt_t
  coeffs_alloc::slong
  exps_alloc::slong
end

struct nmod_mpoly_factor_struct
  constant::ulong
  poly::Ptr{nmod_mpoly_struct}
  exp::Ptr{fmpz}
  num::slong
  alloc::slong
end

#
# from fq_nmod_types.h
#

const fq_nmod_struct = nmod_poly_struct

struct fq_nmod_ctx_struct
  mod::nmod_t

  sparse_modulus::int
  is_conway::int

  a::Ptr{ulong}
  j::Ptr{slong}
  len::slong

  modulus::nmod_poly_t
  inv::nmod_poly_t

  var::Ptr{char}
end

struct fq_nmod_mat_struct
  entries::Ptr{fq_nmod_struct}
  r::slong
  c::slong
  rows::Ptr{Ptr{fq_nmod_struct}}
end

struct fq_nmod_poly_struct
  coeffs::Ptr{fq_nmod_struct}
  alloc::slong
  length::slong
end

struct fq_nmod_poly_factor_struct
  poly::Ptr{fq_nmod_poly_struct}
  exp::Ptr{slong}
  num::slong
  alloc::slong
end

struct fq_nmod_mpoly_struct
  coeffs::Ptr{ulong}
  exps::Ptr{ulong}
  length::slong
  bits::flint_bitcnt_t
  coeffs_alloc::slong
  exps_alloc::slong
end

end
