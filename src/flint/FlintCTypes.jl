# Most of this file is generated from FLINT's header files.
# Do not edit manually, only the corresponding `etc/*_template.jl` file should be edited.
#
# It mirrors FLINT internals, so it is not covered by Nemo's SemVer guarantees
# and may change in any release.

# This file was generated using FLINT_jll v301.600.0+0.

###############################################################################
#
#   Flint C types
#
###############################################################################

module FlintC

using ..Nemo: FlintStruct

#
# FLINT configuration and global definitions
#

const ulong = UInt
const slong = Int

WORD(n) = Int(n)
UWORD(n) = UInt(n)

const FLINT_BITS = UInt == UInt64 ? 64 : 32


###############################################################################
# begin flint.h

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

# end flint.h
###############################################################################


###############################################################################
# begin limb_types.h

const FLINT_MAX_FACTORS_IN_LIMB = 15

struct n_factor_t <: FlintStruct
  num::Cint
  exp::NTuple{FLINT_MAX_FACTORS_IN_LIMB, Cint}
  p::NTuple{FLINT_MAX_FACTORS_IN_LIMB, ulong}
end

struct n_primes_struct <: FlintStruct
  small_i::slong
  small_num::slong
  small_primes::Ptr{Cuint}

  sieve_a::ulong
  sieve_b::ulong
  sieve_i::slong
  sieve_num::slong
  sieve::Ptr{Cchar}
end

const n_primes_t = Tuple{n_primes_struct}

# end limb_types.h
###############################################################################


###############################################################################
# begin nmod_types.h

struct nmod_mat_struct <: FlintStruct
  entries::Ptr{ulong}
  r::slong
  c::slong
  stride::slong
  mod::nmod_t
end

const nmod_mat_t = Tuple{nmod_mat_struct}

struct nmod_poly_struct <: FlintStruct
  coeffs::nn_ptr
  alloc::slong
  length::slong
  mod::nmod_t
end

const nmod_poly_t = Tuple{nmod_poly_struct}

struct nmod_poly_factor_struct <: FlintStruct
  p::Ptr{nmod_poly_struct}
  exp::Ptr{slong}
  num::slong
  alloc::slong
end

const nmod_poly_factor_t = Tuple{nmod_poly_factor_struct}

struct nmod_poly_mat_struct <: FlintStruct
  entries::Ptr{nmod_poly_struct}
  r::slong
  c::slong
  stride::slong
  modulus::ulong
end

const nmod_poly_mat_t = Tuple{nmod_poly_mat_struct}

struct nmod_mpoly_struct <: FlintStruct
  coeffs::Ptr{ulong}
  exps::Ptr{ulong}
  length::slong
  bits::flint_bitcnt_t
  coeffs_alloc::slong
  exps_alloc::slong
end

const nmod_mpoly_t = Tuple{nmod_mpoly_struct}

struct nmod_mpoly_factor_struct <: FlintStruct
  constant::ulong
  poly::Ptr{nmod_mpoly_struct}
  exp::Ptr{fmpz}
  num::slong
  alloc::slong
end

const nmod_mpoly_factor_t = Tuple{nmod_mpoly_factor_struct}

@enum dot_method_t begin
  _DOT0 = 0
  _DOT1 = 1

  _DOT2_SPLIT = 2

  _DOT2_HALF = 3
  _DOT2 = 4
  _DOT3_ACC = 5
  _DOT3 = 6
  _DOT_POW2 = 7
end

struct dot_params_t <: FlintStruct
  method::dot_method_t
  pow2_precomp::ulong
end

# end nmod_types.h
###############################################################################


###############################################################################
# begin fmpz_types.h

struct zz_struct <: FlintStruct
  alloc::Cint
  size::Cint
  ptr::nn_ptr
end

const zz_ptr = Ptr{zz_struct}

const zz_srcptr = Ptr{zz_struct}

struct fmpz_factor_struct <: FlintStruct
  sign::Cint
  p::Ptr{fmpz}
  exp::Ptr{ulong}
  alloc::slong
  num::slong
end

const fmpz_factor_t = Tuple{fmpz_factor_struct}

struct fmpz_preinvn_struct <: FlintStruct
  dinv::nn_ptr
  n::slong
  norm::flint_bitcnt_t
end

const fmpz_preinvn_t = Tuple{fmpz_preinvn_struct}

struct fmpz_vec_struct <: FlintStruct
  entries::Ptr{fmpz}
  alloc::slong
  length::slong
end

const fmpz_vec_t = Tuple{fmpz_vec_struct}

struct fmpz_poly_struct <: FlintStruct
  coeffs::Ptr{fmpz}
  alloc::slong
  length::slong
end

const fmpz_poly_t = Tuple{fmpz_poly_struct}

struct fmpz_poly_factor_struct <: FlintStruct
  c::fmpz
  p::Ptr{fmpz_poly_struct}
  exp::Ptr{slong}
  num::slong
  alloc::slong
end

const fmpz_poly_factor_t = Tuple{fmpz_poly_factor_struct}

struct fmpz_mat_struct <: FlintStruct
  entries::Ptr{fmpz}
  r::slong
  c::slong
  stride::slong
end

const fmpz_mat_t = Tuple{fmpz_mat_struct}

struct fmpz_poly_mat_struct <: FlintStruct
  entries::Ptr{fmpz_poly_struct}
  r::slong
  c::slong
  stride::slong
end

const fmpz_poly_mat_t = Tuple{fmpz_poly_mat_struct}

struct fmpz_mpoly_struct <: FlintStruct
  coeffs::Ptr{fmpz}
  exps::Ptr{ulong}
  alloc::slong
  length::slong
  bits::flint_bitcnt_t
end

const fmpz_mpoly_t = Tuple{fmpz_mpoly_struct}

struct fmpz_mpoly_factor_struct <: FlintStruct
  constant::fmpz_t
  constant_den::fmpz_t
  poly::Ptr{fmpz_mpoly_struct}
  exp::Ptr{fmpz}
  num::slong
  alloc::slong
end

const fmpz_mpoly_factor_t = Tuple{fmpz_mpoly_factor_struct}

struct fmpz_poly_q_struct <: FlintStruct
  num::Ptr{fmpz_poly_struct}
  den::Ptr{fmpz_poly_struct}
end

const fmpz_poly_q_t = Tuple{fmpz_poly_q_struct}

struct fmpz_mpoly_q_struct <: FlintStruct
  num::fmpz_mpoly_struct
  den::fmpz_mpoly_struct
end

const fmpz_mpoly_q_t = Tuple{fmpz_mpoly_q_struct}

struct fmpzi_struct <: FlintStruct
  a::fmpz
  b::fmpz
end

const fmpzi_t = Tuple{fmpzi_struct}

# end fmpz_types.h
###############################################################################


###############################################################################
# begin fmpq_types.h

struct fmpq_mat_struct <: FlintStruct
  entries::Ptr{fmpq}
  r::slong
  c::slong
  stride::slong
end

const fmpq_mat_t = Tuple{fmpq_mat_struct}

struct fmpq_poly_struct <: FlintStruct
  coeffs::Ptr{fmpz}
  alloc::slong
  length::slong
  den::fmpz_t
end

const fmpq_poly_t = Tuple{fmpq_poly_struct}

struct fmpq_mpoly_struct <: FlintStruct
  content::fmpq_t
  zpoly::fmpz_mpoly_t
end

const fmpq_mpoly_t = Tuple{fmpq_mpoly_struct}

struct fmpq_mpoly_factor_struct <: FlintStruct
  constant::fmpq_t
  poly::Ptr{fmpq_mpoly_struct}
  exp::Ptr{fmpz}
  num::slong
  alloc::slong
end

const fmpq_mpoly_factor_t = Tuple{fmpq_mpoly_factor_struct}

# end fmpq_types.h
###############################################################################


###############################################################################
# begin fmpz_mod_types.h

struct fmpz_mod_ctx_struct <: FlintStruct
  n::fmpz_t
  add_fxn::Ptr{Cvoid}
  sub_fxn::Ptr{Cvoid}
  mul_fxn::Ptr{Cvoid}
  mod::nmod_t
  n_limbs::NTuple{3, ulong}
  ninv_limbs::NTuple{3, ulong}
  ninv_huge::Ptr{fmpz_preinvn_struct}
end
const struct_fmpz_mod_ctx = fmpz_mod_ctx_struct

const fmpz_mod_ctx_t = Tuple{fmpz_mod_ctx_struct}

const fmpz_mod_mat_struct = fmpz_mat_struct

const fmpz_mod_mat_t = Tuple{fmpz_mod_mat_struct}

struct fmpz_mod_poly_struct <: FlintStruct
  coeffs::Ptr{fmpz}
  alloc::slong
  length::slong
end

const fmpz_mod_poly_t = Tuple{fmpz_mod_poly_struct}

struct fmpz_mod_poly_factor_struct <: FlintStruct
  poly::Ptr{fmpz_mod_poly_struct}
  exp::Ptr{slong}
  num::slong
  alloc::slong
end

const fmpz_mod_poly_factor_t = Tuple{fmpz_mod_poly_factor_struct}

struct fmpz_mod_mpoly_struct <: FlintStruct
  coeffs::Ptr{fmpz}
  exps::Ptr{ulong}
  length::slong
  bits::flint_bitcnt_t
  coeffs_alloc::slong
  exps_alloc::slong
end

const fmpz_mod_mpoly_t = Tuple{fmpz_mod_mpoly_struct}

struct fmpz_mod_mpoly_factor_struct <: FlintStruct
  constant::fmpz_t
  poly::Ptr{fmpz_mod_mpoly_struct}
  exp::Ptr{fmpz}
  num::slong
  alloc::slong
end

const fmpz_mod_mpoly_factor_t = Tuple{fmpz_mod_mpoly_factor_struct}

struct fmpz_mod_mpoly_q_struct <: FlintStruct
  num::fmpz_mod_mpoly_struct
  den::fmpz_mod_mpoly_struct
end

const fmpz_mod_mpoly_q_t = Tuple{fmpz_mod_mpoly_q_struct}

# end fmpz_mod_types.h
###############################################################################


###############################################################################
# begin fq_nmod_types.h

const fq_nmod_t = nmod_poly_t

const fq_nmod_struct = nmod_poly_struct

struct fq_nmod_ctx_struct <: FlintStruct
  mod::nmod_t

  sparse_modulus::Cint
  is_conway::Cint

  a::Ptr{ulong}
  j::Ptr{slong}
  len::slong

  modulus::nmod_poly_t
  inv::nmod_poly_t

  var::Ptr{Cchar}
end

const fq_nmod_ctx_t = Tuple{fq_nmod_ctx_struct}

struct fq_nmod_mat_struct <: FlintStruct
  entries::Ptr{fq_nmod_struct}
  r::slong
  c::slong
  stride::slong
end

const fq_nmod_mat_t = Tuple{fq_nmod_mat_struct}

struct fq_nmod_poly_struct <: FlintStruct
  coeffs::Ptr{fq_nmod_struct}
  alloc::slong
  length::slong
end

const fq_nmod_poly_t = Tuple{fq_nmod_poly_struct}

struct fq_nmod_poly_factor_struct <: FlintStruct
  poly::Ptr{fq_nmod_poly_struct}
  exp::Ptr{slong}
  num::slong
  alloc::slong
end

const fq_nmod_poly_factor_t = Tuple{fq_nmod_poly_factor_struct}

struct fq_nmod_mpoly_struct <: FlintStruct
  coeffs::Ptr{ulong}
  exps::Ptr{ulong}
  length::slong
  bits::flint_bitcnt_t
  coeffs_alloc::slong
  exps_alloc::slong
end

const fq_nmod_mpoly_t = Tuple{fq_nmod_mpoly_struct}

# end fq_nmod_types.h
###############################################################################


###############################################################################
# begin fq_zech_types.h

struct fq_zech_struct <: FlintStruct
  value::ulong
end

const fq_zech_t = Tuple{fq_zech_struct}

struct fq_zech_ctx_struct <: FlintStruct
  qm1::ulong
  qm1o2::ulong
  qm1opm1::ulong
  p::ulong
  ppre::Cdouble
  prime_root::ulong
  zech_log_table::Ptr{ulong}
  prime_field_table::Ptr{ulong}
  eval_table::Ptr{ulong}

  fq_nmod_ctx::Ptr{fq_nmod_ctx_struct}
  owns_fq_nmod_ctx::Cint
  is_conway::Cint
end

const fq_zech_ctx_t = Tuple{fq_zech_ctx_struct}

struct fq_zech_mat_struct <: FlintStruct
  entries::Ptr{fq_zech_struct}
  r::slong
  c::slong
  stride::slong
end

const fq_zech_mat_t = Tuple{fq_zech_mat_struct}

struct fq_zech_poly_struct <: FlintStruct
  coeffs::Ptr{fq_zech_struct}
  alloc::slong
  length::slong
end

const fq_zech_poly_t = Tuple{fq_zech_poly_struct}

struct fq_zech_poly_factor_struct <: FlintStruct
  poly::Ptr{fq_zech_poly_struct}
  exp::Ptr{slong}
  num::slong
  alloc::slong
end

const fq_zech_poly_factor_t = Tuple{fq_zech_poly_factor_struct}

# end fq_zech_types.h
###############################################################################


###############################################################################
# begin fq_types.h

const fq_t = fmpz_poly_t

const fq_struct = fmpz_poly_struct

struct fq_ctx_struct <: FlintStruct
  ctxp::fmpz_mod_ctx_t

  sparse_modulus::Cint
  is_conway::Cint

  a::Ptr{fmpz}
  j::Ptr{slong}
  len::slong

  modulus::fmpz_mod_poly_t
  inv::fmpz_mod_poly_t

  var::Ptr{Cchar}
end

const fq_ctx_t = Tuple{fq_ctx_struct}

struct fq_mat_struct <: FlintStruct
  entries::Ptr{fq_struct}
  r::slong
  c::slong
  stride::slong
end

const fq_mat_t = Tuple{fq_mat_struct}

struct fq_poly_struct <: FlintStruct
  coeffs::Ptr{fq_struct}
  alloc::slong
  length::slong
end

const fq_poly_t = Tuple{fq_poly_struct}

struct fq_poly_factor_struct <: FlintStruct
  poly::Ptr{fq_poly_struct}
  exp::Ptr{slong}
  num::slong
  alloc::slong
end

const fq_poly_factor_t = Tuple{fq_poly_factor_struct}

# end fq_types.h
###############################################################################


###############################################################################
# begin gr_types.h

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

# end gr_types.h
###############################################################################


###############################################################################
# begin fq_default.h

const FQ_DEFAULT_FQ_ZECH = 1

const FQ_DEFAULT_FQ_NMOD = 2

const FQ_DEFAULT_FQ = 3

const FQ_DEFAULT_NMOD = 4

const FQ_DEFAULT_FMPZ_MOD = 5

struct fq_default_struct <: FlintStruct
  uniondata::NTuple{cld(maximum(sizeof, (
    fq_t,
    fq_nmod_t,
    fq_zech_t,
    ulong,
    fmpz_t,
  )), sizeof(UInt)), UInt}
end
const union_fq_default_struct = fq_default_struct

const fq_default_t = Tuple{fq_default_struct}

const fq_default_ctx_struct = gr_ctx_struct

const fq_default_ctx_t = Tuple{fq_default_ctx_struct}

struct _gr_fmpz_mod_ctx_struct <: FlintStruct
  ctx::Ptr{fmpz_mod_ctx_struct}
  is_prime::truth_t
  a::fmpz
end

# end fq_default.h
###############################################################################


###############################################################################
# begin fq_default_mat.h

struct fq_default_mat_struct <: FlintStruct
  uniondata::NTuple{cld(maximum(sizeof, (
    fq_mat_t,
    fq_nmod_mat_t,
    fq_zech_mat_t,
    nmod_mat_t,
    fmpz_mod_mat_t,
  )), sizeof(UInt)), UInt}
end
const union_fq_default_mat_struct = fq_default_mat_struct

const fq_default_mat_t = Tuple{fq_default_mat_struct}

# end fq_default_mat.h
###############################################################################


###############################################################################
# begin fq_default_poly.h

struct fq_default_poly_struct <: FlintStruct
  uniondata::NTuple{cld(maximum(sizeof, (
    fq_poly_t,
    fq_nmod_poly_t,
    fq_zech_poly_t,
    nmod_poly_t,
    fmpz_mod_poly_t,
  )), sizeof(UInt)), UInt}
end
const union_fq_default_poly_struct = fq_default_poly_struct

const fq_default_poly_t = Tuple{fq_default_poly_struct}

# end fq_default_poly.h
###############################################################################


###############################################################################
# begin fq_default_poly_factor.h

struct fq_default_poly_factor_struct <: FlintStruct
  uniondata::NTuple{cld(maximum(sizeof, (
    fq_poly_factor_t,
    fq_nmod_poly_factor_t,
    fq_zech_poly_factor_t,
    nmod_poly_factor_t,
    fmpz_mod_poly_factor_t,
  )), sizeof(UInt)), UInt}
end
const union_fq_default_poly_factor_struct = fq_default_poly_factor_struct

const fq_default_poly_factor_t = Tuple{fq_default_poly_factor_struct}

# end fq_default_poly_factor.h
###############################################################################


###############################################################################
# begin padic_types.h

struct padic_struct <: FlintStruct
  u::fmpz
  v::slong
  N::slong
end

const padic_t = Tuple{padic_struct}

@enum enum_padic_print_mode begin
  PADIC_TERSE
  PADIC_SERIES
  PADIC_VAL_UNIT
end

struct padic_ctx_struct <: FlintStruct
  p::fmpz_t
  pinv::Cdouble
  pow::Ptr{fmpz}
  min::slong
  max::slong
  mode::enum_padic_print_mode
end

const padic_ctx_t = Tuple{padic_ctx_struct}

struct padic_inv_struct <: FlintStruct
  n::slong
  pow::Ptr{fmpz}
end

const padic_inv_t = Tuple{padic_inv_struct}

struct padic_mat_struct <: FlintStruct
  mat::fmpz_mat_struct
  val::slong
  N::slong
end

const padic_mat_t = Tuple{padic_mat_struct}

struct padic_poly_struct <: FlintStruct
  coeffs::Ptr{fmpz}
  alloc::slong
  length::slong
  val::slong
  N::slong
end

const padic_poly_t = Tuple{padic_poly_struct}

# end padic_types.h
###############################################################################


###############################################################################
# begin n_poly_types.h

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

# end n_poly_types.h
###############################################################################


###############################################################################
# begin mpoly_types.h

const MPOLY_MIN_BITS = UWORD(8)

@enum ordering_t begin
  ORD_LEX
  ORD_DEGLEX
  ORD_DEGREVLEX
end

const MPOLY_NUM_ORDERINGS = 3

struct mpoly_ctx_struct <: FlintStruct
  nvars::slong
  nfields::slong
  ord::ordering_t
  deg::Cint
  rev::Cint
  lut_words_per_exp::NTuple{FLINT_BITS, slong}
  lut_fix_bits::NTuple{FLINT_BITS, Cuchar}
end

const mpoly_ctx_t = Tuple{mpoly_ctx_struct}

struct nmod_mpoly_ctx_struct <: FlintStruct
  minfo::mpoly_ctx_t
  mod::nmod_t
end

const nmod_mpoly_ctx_t = Tuple{nmod_mpoly_ctx_struct}

struct fmpz_mpoly_ctx_struct <: FlintStruct
  minfo::mpoly_ctx_t
end

const fmpz_mpoly_ctx_t = Tuple{fmpz_mpoly_ctx_struct}

struct fmpq_mpoly_ctx_struct <: FlintStruct
  zctx::fmpz_mpoly_ctx_t
end

const fmpq_mpoly_ctx_t = Tuple{fmpq_mpoly_ctx_struct}

struct fmpz_mod_mpoly_ctx_struct <: FlintStruct
  minfo::mpoly_ctx_t
  ffinfo::fmpz_mod_ctx_t
end

const fmpz_mod_mpoly_ctx_t = Tuple{fmpz_mod_mpoly_ctx_struct}

struct fq_nmod_mpoly_ctx_struct <: FlintStruct
  minfo::mpoly_ctx_t
  fqctx::fq_nmod_ctx_t
end

const fq_nmod_mpoly_ctx_t = Tuple{fq_nmod_mpoly_ctx_struct}

struct struct_mpoly_void_ring_t <: FlintStruct
  elem_size::slong
  ctx::Ptr{Cvoid}
  init::Ptr{Cvoid}
  clear::Ptr{Cvoid}
  is_zero::Ptr{Cvoid}
  zero::Ptr{Cvoid}
  one::Ptr{Cvoid}
  set_fmpz::Ptr{Cvoid}
  set::Ptr{Cvoid}
  swap::Ptr{Cvoid}
  neg::Ptr{Cvoid}
  add::Ptr{Cvoid}
  sub::Ptr{Cvoid}
  mul_fmpz::Ptr{Cvoid}
  mul::Ptr{Cvoid}
  divexact::Ptr{Cvoid}
  divides::Ptr{Cvoid}
  pow_fmpz::Ptr{Cvoid}
  length::Ptr{Cvoid}
end
const mpoly_void_ring_t = Tuple{struct_mpoly_void_ring_t}

struct mpoly_gcd_info_struct <: FlintStruct
  Amax_exp::Ptr{ulong}
  Amin_exp::Ptr{ulong}
  Astride::Ptr{ulong}
  Adeflate_deg::Ptr{slong}
  Alead_count::Ptr{slong}
  Atail_count::Ptr{slong}

  Bmax_exp::Ptr{ulong}
  Bmin_exp::Ptr{ulong}
  Bstride::Ptr{ulong}
  Bdeflate_deg::Ptr{slong}
  Blead_count::Ptr{slong}
  Btail_count::Ptr{slong}

  Gmin_exp::Ptr{ulong}
  Abarmin_exp::Ptr{ulong}
  Bbarmin_exp::Ptr{ulong}
  Gstride::Ptr{ulong}
  Gterm_count_est::Ptr{slong}
  Gdeflate_deg_bound::Ptr{slong}

  Gbits::flint_bitcnt_t
  Abarbits::flint_bitcnt_t
  Bbarbits::flint_bitcnt_t

  mvars::slong
  Adeflate_tdeg::slong
  Bdeflate_tdeg::slong

  Adensity::Cdouble
  Bdensity::Cdouble

  hensel_time::Cdouble
  brown_time::Cdouble
  zippel_time::Cdouble
  zippel2_time::Cdouble
  hensel_perm::Ptr{slong}
  brown_perm::Ptr{slong}
  zippel_perm::Ptr{slong}
  zippel2_perm::Ptr{slong}
  can_use::Cuint
  Gdeflate_deg_bounds_are_nice::Cint

  data::Ptr{Cchar}
end

const mpoly_gcd_info_t = Tuple{mpoly_gcd_info_struct}

struct mpoly_compression_struct <: FlintStruct
  mvars::slong
  nvars::slong
  exps::Ptr{slong}
  exps_alloc::slong
  rest::Ptr{slong}
  rest_alloc::slong
  umat::Ptr{slong}
  deltas::Ptr{slong}
  degs::Ptr{slong}
  is_trivial::Cint
  is_perm::Cint
  is_irred::Cint
end

const mpoly_compression_t = Tuple{mpoly_compression_struct}

struct nmod_mpolyn_struct <: FlintStruct
  coeffs::Ptr{n_poly_struct}
  exps::Ptr{ulong}
  alloc::slong
  length::slong
  bits::slong
end

const nmod_mpolyn_t = Tuple{nmod_mpolyn_struct}

struct nmod_mpolyun_struct <: FlintStruct
  coeffs::Ptr{nmod_mpolyn_struct}
  exps::Ptr{ulong}
  alloc::slong
  length::slong
  bits::flint_bitcnt_t
end

const nmod_mpolyun_t = Tuple{nmod_mpolyun_struct}

@enum nmod_gcds_ret_t begin
  nmod_gcds_success
  nmod_gcds_form_main_degree_too_high
  nmod_gcds_form_wrong
  nmod_gcds_no_solution
  nmod_gcds_scales_not_found
  nmod_gcds_eval_point_not_found
  nmod_gcds_eval_gcd_deg_too_high
end

# end mpoly_types.h
###############################################################################


###############################################################################
# begin fq_nmod_mpoly.h

struct fq_nmod_mpoly_univar_struct <: FlintStruct
  coeffs::Ptr{fq_nmod_mpoly_struct}
  exps::Ptr{fmpz}
  alloc::slong
  length::slong
end

const fq_nmod_mpoly_univar_t = Tuple{fq_nmod_mpoly_univar_struct}

struct fq_nmod_mpolyu_struct <: FlintStruct
  coeffs::Ptr{fq_nmod_mpoly_struct}
  exps::Ptr{ulong}
  alloc::slong
  length::slong
  bits::flint_bitcnt_t
end

const fq_nmod_mpolyu_t = Tuple{fq_nmod_mpolyu_struct}

struct fq_nmod_mpolyn_struct <: FlintStruct
  coeffs::Ptr{n_fq_poly_struct}
  exps::Ptr{ulong}
  alloc::slong
  length::slong
  bits::slong
end

const fq_nmod_mpolyn_t = Tuple{fq_nmod_mpolyn_struct}

struct fq_nmod_mpolyun_struct <: FlintStruct
  coeffs::Ptr{fq_nmod_mpolyn_struct}
  exps::Ptr{ulong}
  alloc::slong
  length::slong
  bits::flint_bitcnt_t
end

const fq_nmod_mpolyun_t = Tuple{fq_nmod_mpolyun_struct}

struct bad_fq_nmod_embed_struct <: FlintStruct
  smctx::Ptr{fq_nmod_ctx_struct}
  phi_sm::fq_nmod_poly_t
  h::fq_nmod_poly_t
  h_as_n_fq_poly::n_fq_poly_t
  lgctx::Ptr{fq_nmod_ctx_struct}
  theta_lg::fq_nmod_t
  x_lg::fq_nmod_t
  lg_to_sm_mat::nmod_mat_t
  sm_to_lg_mat::nmod_mat_t
end
const struct_bad_fq_nmod_embed = bad_fq_nmod_embed_struct

const bad_fq_nmod_embed_t = Tuple{bad_fq_nmod_embed_struct}

struct bad_fq_nmod_mpoly_embed_chooser_struct <: FlintStruct
  embed::Ptr{bad_fq_nmod_embed_struct}
  m::slong
  n::slong
  k::slong
  p::ulong
end
const struct_bad_fq_nmod_mpoly_embed_chooser = bad_fq_nmod_mpoly_embed_chooser_struct

const bad_fq_nmod_mpoly_embed_chooser_t = Tuple{bad_fq_nmod_mpoly_embed_chooser_struct}

struct fq_nmod_mpoly_geobucket_struct <: FlintStruct
  polys::NTuple{FLINT_BITS÷2, fq_nmod_mpoly_struct}
  temps::NTuple{FLINT_BITS÷2, fq_nmod_mpoly_struct}
  length::slong
end
const struct_fq_nmod_mpoly_geobucket = fq_nmod_mpoly_geobucket_struct

const fq_nmod_mpoly_geobucket_t = Tuple{fq_nmod_mpoly_geobucket_struct}

# end fq_nmod_mpoly.h
###############################################################################


###############################################################################
# begin fq_nmod_mpoly_factor.h

struct fq_nmod_mpoly_factor_struct <: FlintStruct
  constant::fq_nmod_t
  poly::Ptr{fq_nmod_mpoly_struct}
  exp::Ptr{fmpz}
  num::slong
  alloc::slong
end

const fq_nmod_mpoly_factor_t = Tuple{fq_nmod_mpoly_factor_struct}

struct fq_nmod_mpolyv_struct <: FlintStruct
  coeffs::Ptr{fq_nmod_mpoly_struct}
  alloc::slong
  length::slong
end

const fq_nmod_mpolyv_t = Tuple{fq_nmod_mpolyv_struct}

struct fq_nmod_mpoly_pfrac_struct <: FlintStruct
  bits::flint_bitcnt_t
  w::slong
  r::slong
  inv_prod_dbetas::Ptr{fq_nmod_poly_struct}
  inv_prod_dbetas_mvar::Ptr{fq_nmod_mpoly_struct}
  dbetas::Ptr{fq_nmod_poly_struct}
  dbetas_mvar::Ptr{fq_nmod_mpoly_struct}
  prod_mbetas::Ptr{fq_nmod_mpoly_struct}
  prod_mbetas_coeffs::Ptr{fq_nmod_mpolyv_struct}
  mbetas::Ptr{fq_nmod_mpoly_struct}
  deltas::Ptr{fq_nmod_mpoly_struct}
  xalpha::Ptr{fq_nmod_mpoly_struct}
  q::Ptr{fq_nmod_mpoly_struct}
  G::Ptr{fq_nmod_mpoly_geobucket_struct}
  qt::Ptr{fq_nmod_mpoly_struct}
  newt::Ptr{fq_nmod_mpoly_struct}
  delta_coeffs::Ptr{fq_nmod_mpolyv_struct}
  T::fq_nmod_mpoly_t
  Q::fq_nmod_mpoly_t
  R::fq_nmod_mpoly_t
end

const fq_nmod_mpoly_pfrac_t = Tuple{fq_nmod_mpoly_pfrac_struct}

# end fq_nmod_mpoly_factor.h
###############################################################################


###############################################################################
# begin fmpz_poly.h

const FMPZ_POLY_INV_NEWTON_CUTOFF = 32

const FMPZ_POLY_SQRT_DIVCONQUER_CUTOFF = 16

const FMPZ_POLY_SQRTREM_DIVCONQUER_CUTOFF = 16

struct fmpz_poly_powers_precomp_struct <: FlintStruct
  powers::Ptr{Ptr{fmpz}}
  len::slong
end

const fmpz_poly_powers_precomp_t = Tuple{fmpz_poly_powers_precomp_struct}

struct fmpz_poly_mul_precache_struct <: FlintStruct
  jj::Ptr{Ptr{ulong}}
  n::slong
  len2::slong
  loglen::slong
  bits2::slong
  limbs::slong
  poly2::fmpz_poly_t
end

const fmpz_poly_mul_precache_t = Tuple{fmpz_poly_mul_precache_struct}

# end fmpz_poly.h
###############################################################################


###############################################################################
# begin fmpq_poly.h

struct fmpq_poly_powers_precomp_struct <: FlintStruct
  powers::Ptr{fmpq_poly_struct}
  len::slong
end

const fmpq_poly_powers_precomp_t = Tuple{fmpq_poly_powers_precomp_struct}

const WEAK_CANONICALISE_CUTOFF = 25600

# end fmpq_poly.h
###############################################################################


###############################################################################
# begin nf.h

struct nf_struct <: FlintStruct
  pol::fmpq_poly_t
  pinv::NTuple{cld(maximum(sizeof, (fmpz_preinvn_t,)), sizeof(UInt)), UInt}
  powers::NTuple{cld(maximum(sizeof, (fmpq_poly_powers_precomp_t, fmpz_poly_powers_precomp_t,)), sizeof(UInt)), UInt}
  traces::fmpq_poly_t
  flag::ulong
end

const nf_t = Tuple{nf_struct}

const NF_POWERS_CUTOFF = 30

const NF_GENERIC = 0

const NF_MONIC = 1

const NF_LINEAR = 2

const NF_QUADRATIC = 4

const NF_GAUSSIAN = 8

# end nf.h
###############################################################################


###############################################################################
# begin nf_elem.h

struct lnf_elem_struct <: FlintStruct
  num::fmpz_t
  den::fmpz_t
end

const lnf_elem_t = Tuple{lnf_elem_struct}

struct qnf_elem_struct <: FlintStruct
  num::NTuple{3, fmpz}
  den::fmpz_t
end

const qnf_elem_t = Tuple{qnf_elem_struct}

struct nf_elem_struct <: FlintStruct
  uniondata::NTuple{cld(maximum(sizeof, (
    fmpq_poly_t,
    lnf_elem_t,
    qnf_elem_t,
  )), sizeof(UInt)), UInt}
end

const nf_elem_t = Tuple{nf_elem_struct}

# end nf_elem.h
###############################################################################


###############################################################################
# begin arf_types.h

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

# end arf_types.h
###############################################################################


###############################################################################
# begin arb_types.h

struct mag_struct <: FlintStruct
  exp::fmpz
  man::ulong
end

const mag_t = Tuple{mag_struct}

const mag_ptr = Ptr{mag_struct}

const mag_srcptr = Ptr{mag_struct}

struct arb_struct <: FlintStruct
  mid::arf_struct
  rad::mag_struct
end

const arb_t = Tuple{arb_struct}

const arb_ptr = Ptr{arb_struct}

const arb_srcptr = Ptr{arb_struct}

struct arb_mat_struct <: FlintStruct
  entries::arb_ptr
  r::slong
  c::slong
  stride::slong
end

const arb_mat_t = Tuple{arb_mat_struct}

struct arb_poly_struct <: FlintStruct
  coeffs::arb_ptr
  alloc::slong
  length::slong
end

const arb_poly_t = Tuple{arb_poly_struct}

# end arb_types.h
###############################################################################


###############################################################################
# begin acb_types.h

struct acb_struct <: FlintStruct
  real::arb_struct
  imag::arb_struct
end

const acb_t = Tuple{acb_struct}

const acb_ptr = Ptr{acb_struct}

const acb_srcptr = Ptr{acb_struct}

struct acb_mat_struct <: FlintStruct
  entries::acb_ptr
  r::slong
  c::slong
  stride::slong
end

const acb_mat_t = Tuple{acb_mat_struct}

struct acb_poly_struct <: FlintStruct
  coeffs::acb_ptr
  alloc::slong
  length::slong
end

const acb_poly_t = Tuple{acb_poly_struct}

# end acb_types.h
###############################################################################


###############################################################################
# begin fexpr.h

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

# end fexpr.h
###############################################################################


###############################################################################
# begin qqbar.h

struct qqbar_struct <: FlintStruct
  poly::fmpz_poly_struct
  enclosure::acb_struct
end

const qqbar_t = Tuple{qqbar_struct}

const qqbar_ptr = Ptr{qqbar_struct}

const qqbar_srcptr = Ptr{qqbar_struct}

const QQBAR_DEFAULT_PREC = 128

const QQBAR_ROOTS_IRREDUCIBLE = 1

const QQBAR_ROOTS_UNSORTED = 2

const QQBAR_FORMULA_GAUSSIANS = 1

const QQBAR_FORMULA_QUADRATICS = 2

const QQBAR_FORMULA_CYCLOTOMICS = 4

const QQBAR_FORMULA_CUBICS = 8

const QQBAR_FORMULA_QUARTICS = 16

const QQBAR_FORMULA_QUINTICS = 32

const QQBAR_FORMULA_DEPRESSION = 64

const QQBAR_FORMULA_DEFLATION = 128

const QQBAR_FORMULA_SEPARATION = 256

const QQBAR_FORMULA_EXP_FORM = 2048

const QQBAR_FORMULA_TRIG_FORM = 4096

const QQBAR_FORMULA_RADICAL_FORM = 8192

const QQBAR_FORMULA_AUTO_FORM = 0

# end qqbar.h
###############################################################################


###############################################################################
# begin fmpz_mpoly.h

struct fmpz_mpoly_univar_struct <: FlintStruct
  coeffs::Ptr{fmpz_mpoly_struct}
  exps::Ptr{fmpz}
  alloc::slong
  length::slong
end

const fmpz_mpoly_univar_t = Tuple{fmpz_mpoly_univar_struct}

struct fmpz_mpolyd_struct <: FlintStruct
  nvars::slong
  degb_alloc::slong
  deg_bounds::Ptr{slong}
  length::slong
  coeff_alloc::slong
  coeffs::Ptr{fmpz}
end

const fmpz_mpolyd_t = Tuple{fmpz_mpolyd_struct}

struct fmpz_mpoly_vec_struct <: FlintStruct
  p::Ptr{fmpz_mpoly_struct}
  alloc::slong
  length::slong
end

const fmpz_mpoly_vec_t = Tuple{fmpz_mpoly_vec_struct}

struct fmpz_mpoly_stripe_struct <: FlintStruct
  big_mem::Ptr{Cchar}
  big_mem_alloc::slong
  N::slong
  bits::flint_bitcnt_t
  cmpmask::Ptr{ulong}
  startidx::Ptr{slong}
  endidx::Ptr{slong}
  emin::Ptr{ulong}
  emax::Ptr{ulong}
  coeff_bits::flint_bitcnt_t
  upperclosed::Cint
  flint_small::Cint
end
const struct__fmpz_mpoly_stripe_struct = fmpz_mpoly_stripe_struct

const fmpz_mpoly_stripe_t = Tuple{fmpz_mpoly_stripe_struct}

struct fmpz_mpolyd_ctx_struct <: FlintStruct
  nvars::slong
  perm::Ptr{slong}
end

const fmpz_mpolyd_ctx_t = Tuple{fmpz_mpolyd_ctx_struct}

struct struct_fmpz_pow_cache_t <: FlintStruct
  powers::Ptr{fmpz}
  length::slong
  alloc::slong
  tmp::fmpz_t
end
const fmpz_pow_cache_t = Tuple{struct_fmpz_pow_cache_t}

struct fmpz_mpoly_geobucket_struct <: FlintStruct
  polys::NTuple{FLINT_BITS÷2, fmpz_mpoly_struct}
  temps::NTuple{FLINT_BITS÷2, fmpz_mpoly_struct}
  length::slong
end
const struct_fmpz_mpoly_geobucket = fmpz_mpoly_geobucket_struct

const fmpz_mpoly_geobucket_t = Tuple{fmpz_mpoly_geobucket_struct}

# end fmpz_mpoly.h
###############################################################################


###############################################################################
# begin ca_types.h

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

# end ca_types.h
###############################################################################


end # module FlintC
