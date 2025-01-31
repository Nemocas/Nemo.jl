# Most of this file is generated from FLINT's header files.
# Do not edit manually, only the corresponding `etc/*_template.jl` file should be edited.

# This file was generated using FLINT_jll v300.100.301+0.

###############################################################################
#
#   Flint C types
#
###############################################################################

module FlintC

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

const __FLINT_VERSION_MINOR = 2

const __FLINT_VERSION_PATCHLEVEL = 0

struct struct___FLINT_FILE end

const FLINT_FILE = struct___FLINT_FILE

const flint_bitcnt_t = ulong

const nn_ptr = Ptr{ulong}

const nn_srcptr = Ptr{ulong}

const flint_cleanup_function_t = Ptr{Cvoid}

const thread_pool_handle = Cint

struct flint_rand_struct
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

struct nmod_t
  n::ulong
  ninv::ulong
  norm::flint_bitcnt_t
end

const fmpz = slong

const fmpz_t = Tuple{fmpz}

struct fmpq
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

struct n_factor_t
  num::Cint
  exp::NTuple{FLINT_MAX_FACTORS_IN_LIMB, Cint}
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
  sieve::Ptr{Cchar}
end

const n_primes_t = Tuple{n_primes_struct}

# end limb_types.h
###############################################################################


###############################################################################
# begin nmod_types.h

struct nmod_mat_struct
  entries::Ptr{ulong}
  r::slong
  c::slong
  rows::Ptr{Ptr{ulong}}
  mod::nmod_t
end

const nmod_mat_t = Tuple{nmod_mat_struct}

struct nmod_poly_struct
  coeffs::nn_ptr
  alloc::slong
  length::slong
  mod::nmod_t
end

const nmod_poly_t = Tuple{nmod_poly_struct}

struct nmod_poly_factor_struct
  p::Ptr{nmod_poly_struct}
  exp::Ptr{slong}
  num::slong
  alloc::slong
end

const nmod_poly_factor_t = Tuple{nmod_poly_factor_struct}

struct nmod_poly_mat_struct
  entries::Ptr{nmod_poly_struct}
  r::slong
  c::slong
  rows::Ptr{Ptr{nmod_poly_struct}}
  modulus::ulong
end

const nmod_poly_mat_t = Tuple{nmod_poly_mat_struct}

struct nmod_mpoly_struct
  coeffs::Ptr{ulong}
  exps::Ptr{ulong}
  length::slong
  bits::flint_bitcnt_t
  coeffs_alloc::slong
  exps_alloc::slong
end

const nmod_mpoly_t = Tuple{nmod_mpoly_struct}

struct nmod_mpoly_factor_struct
  constant::ulong
  poly::Ptr{nmod_mpoly_struct}
  exp::Ptr{fmpz}
  num::slong
  alloc::slong
end

const nmod_mpoly_factor_t = Tuple{nmod_mpoly_factor_struct}

# end nmod_types.h
###############################################################################


###############################################################################
# begin fmpz_types.h

struct zz_struct
  alloc::Cint
  size::Cint
  ptr::nn_ptr
end

const zz_ptr = Ptr{zz_struct}

const zz_srcptr = Ptr{zz_struct}

struct fmpz_factor_struct
  sign::Cint
  p::Ptr{fmpz}
  exp::Ptr{ulong}
  alloc::slong
  num::slong
end

const fmpz_factor_t = Tuple{fmpz_factor_struct}

struct fmpz_preinvn_struct
  dinv::nn_ptr
  n::slong
  norm::flint_bitcnt_t
end

const fmpz_preinvn_t = Tuple{fmpz_preinvn_struct}

struct fmpz_poly_struct
  coeffs::Ptr{fmpz}
  alloc::slong
  length::slong
end

const fmpz_poly_t = Tuple{fmpz_poly_struct}

struct fmpz_poly_factor_struct
  c::fmpz
  p::Ptr{fmpz_poly_struct}
  exp::Ptr{slong}
  num::slong
  alloc::slong
end

const fmpz_poly_factor_t = Tuple{fmpz_poly_factor_struct}

struct fmpz_mat_struct
  entries::Ptr{fmpz}
  r::slong
  c::slong
  rows::Ptr{Ptr{fmpz}}
end

const fmpz_mat_t = Tuple{fmpz_mat_struct}

struct fmpz_poly_mat_struct
  entries::Ptr{fmpz_poly_struct}
  r::slong
  c::slong
  rows::Ptr{Ptr{fmpz_poly_struct}}
end

const fmpz_poly_mat_t = Tuple{fmpz_poly_mat_struct}

struct fmpz_mpoly_struct
  coeffs::Ptr{fmpz}
  exps::Ptr{ulong}
  alloc::slong
  length::slong
  bits::flint_bitcnt_t
end

const fmpz_mpoly_t = Tuple{fmpz_mpoly_struct}

struct fmpz_mpoly_factor_struct
  constant::fmpz_t
  constant_den::fmpz_t
  poly::Ptr{fmpz_mpoly_struct}
  exp::Ptr{fmpz}
  num::slong
  alloc::slong
end

const fmpz_mpoly_factor_t = Tuple{fmpz_mpoly_factor_struct}

struct fmpz_poly_q_struct
  num::Ptr{fmpz_poly_struct}
  den::Ptr{fmpz_poly_struct}
end

const fmpz_poly_q_t = Tuple{fmpz_poly_q_struct}

struct fmpz_mpoly_q_struct
  num::fmpz_mpoly_struct
  den::fmpz_mpoly_struct
end

const fmpz_mpoly_q_t = Tuple{fmpz_mpoly_q_struct}

struct fmpzi_struct
  a::fmpz
  b::fmpz
end

const fmpzi_t = Tuple{fmpzi_struct}

# end fmpz_types.h
###############################################################################


###############################################################################
# begin fmpq_types.h

struct fmpq_mat_struct
  entries::Ptr{fmpq}
  r::slong
  c::slong
  rows::Ptr{Ptr{fmpq}}
end

const fmpq_mat_t = Tuple{fmpq_mat_struct}

struct fmpq_poly_struct
  coeffs::Ptr{fmpz}
  alloc::slong
  length::slong
  den::fmpz_t
end

const fmpq_poly_t = Tuple{fmpq_poly_struct}

struct fmpq_mpoly_struct
  content::fmpq_t
  zpoly::fmpz_mpoly_t
end

const fmpq_mpoly_t = Tuple{fmpq_mpoly_struct}

struct fmpq_mpoly_factor_struct
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

struct fmpz_mod_ctx_struct
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

struct fmpz_mod_poly_struct
  coeffs::Ptr{fmpz}
  alloc::slong
  length::slong
end

const fmpz_mod_poly_t = Tuple{fmpz_mod_poly_struct}

struct fmpz_mod_poly_factor_struct
  poly::Ptr{fmpz_mod_poly_struct}
  exp::Ptr{slong}
  num::slong
  alloc::slong
end

const fmpz_mod_poly_factor_t = Tuple{fmpz_mod_poly_factor_struct}

struct fmpz_mod_mpoly_struct
  coeffs::Ptr{fmpz}
  exps::Ptr{ulong}
  length::slong
  bits::flint_bitcnt_t
  coeffs_alloc::slong
  exps_alloc::slong
end

const fmpz_mod_mpoly_t = Tuple{fmpz_mod_mpoly_struct}

struct fmpz_mod_mpoly_factor_struct
  constant::fmpz_t
  poly::Ptr{fmpz_mod_mpoly_struct}
  exp::Ptr{fmpz}
  num::slong
  alloc::slong
end

const fmpz_mod_mpoly_factor_t = Tuple{fmpz_mod_mpoly_factor_struct}

# end fmpz_mod_types.h
###############################################################################


###############################################################################
# begin fq_nmod_types.h

const fq_nmod_t = nmod_poly_t

const fq_nmod_struct = nmod_poly_struct

struct fq_nmod_ctx_struct
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

struct fq_nmod_mat_struct
  entries::Ptr{fq_nmod_struct}
  r::slong
  c::slong
  rows::Ptr{Ptr{fq_nmod_struct}}
end

const fq_nmod_mat_t = Tuple{fq_nmod_mat_struct}

struct fq_nmod_poly_struct
  coeffs::Ptr{fq_nmod_struct}
  alloc::slong
  length::slong
end

const fq_nmod_poly_t = Tuple{fq_nmod_poly_struct}

struct fq_nmod_poly_factor_struct
  poly::Ptr{fq_nmod_poly_struct}
  exp::Ptr{slong}
  num::slong
  alloc::slong
end

const fq_nmod_poly_factor_t = Tuple{fq_nmod_poly_factor_struct}

struct fq_nmod_mpoly_struct
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

struct fq_zech_struct
  value::ulong
end

const fq_zech_t = Tuple{fq_zech_struct}

struct fq_zech_ctx_struct
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

struct fq_zech_mat_struct
  entries::Ptr{fq_zech_struct}
  r::slong
  c::slong
  rows::Ptr{Ptr{fq_zech_struct}}
end

const fq_zech_mat_t = Tuple{fq_zech_mat_struct}

struct fq_zech_poly_struct
  coeffs::Ptr{fq_zech_struct}
  alloc::slong
  length::slong
end

const fq_zech_poly_t = Tuple{fq_zech_poly_struct}

struct fq_zech_poly_factor_struct
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

struct fq_ctx_struct
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

struct fq_mat_struct
  entries::Ptr{fq_struct}
  r::slong
  c::slong
  rows::Ptr{Ptr{fq_struct}}
end

const fq_mat_t = Tuple{fq_mat_struct}

struct fq_poly_struct
  coeffs::Ptr{fq_struct}
  alloc::slong
  length::slong
end

const fq_poly_t = Tuple{fq_poly_struct}

struct fq_poly_factor_struct
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

struct gr_stream_struct
  fp::Ptr{FLINT_FILE}
  s::Ptr{Cchar}
  len::slong
  alloc::slong
end

const gr_stream_t = Tuple{gr_stream_struct}

const gr_funcptr = Ptr{Cvoid}

const GR_CTX_STRUCT_DATA_BYTES = 6 * sizeof(ulong)

struct gr_ctx_struct
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

struct gr_vec_struct
  entries::gr_ptr
  alloc::slong
  length::slong
end

const gr_vec_t = Tuple{gr_vec_struct}

struct gr_mat_struct
  entries::gr_ptr
  r::slong
  c::slong
  rows::Ptr{gr_ptr}
end

const gr_mat_t = Tuple{gr_mat_struct}

struct gr_poly_struct
  coeffs::gr_ptr
  alloc::slong
  length::slong
end

const gr_poly_t = Tuple{gr_poly_struct}

# end gr_types.h
###############################################################################


###############################################################################
# begin fq_default.h

const FQ_DEFAULT_FQ_ZECH = 1

const FQ_DEFAULT_FQ_NMOD = 2

const FQ_DEFAULT_FQ = 3

const FQ_DEFAULT_NMOD = 4

const FQ_DEFAULT_FMPZ_MOD = 5

struct fq_default_struct
  uniondata::NTuple{maximum(sizeof, (
    fq_t,
    fq_nmod_t,
    fq_zech_t,
    ulong,
    fmpz_t,
  )), UInt8}
end
const union_fq_default_struct = fq_default_struct

const fq_default_t = Tuple{fq_default_struct}

const fq_default_ctx_struct = gr_ctx_struct

const fq_default_ctx_t = Tuple{fq_default_ctx_struct}

struct _gr_fmpz_mod_ctx_struct
  ctx::Ptr{fmpz_mod_ctx_struct}
  is_prime::truth_t
  a::fmpz
end

struct _gr_nmod_ctx_struct
  nmod::nmod_t
  a::ulong
end

# end fq_default.h
###############################################################################


###############################################################################
# begin padic_types.h

struct padic_struct
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

struct padic_ctx_struct
  p::fmpz_t
  pinv::Cdouble
  pow::Ptr{fmpz}
  min::slong
  max::slong
  mode::enum_padic_print_mode
end

const padic_ctx_t = Tuple{padic_ctx_struct}

struct padic_inv_struct
  n::slong
  pow::Ptr{fmpz}
end

const padic_inv_t = Tuple{padic_inv_struct}

struct padic_mat_struct
  mat::fmpz_mat_struct
  val::slong
  N::slong
end

const padic_mat_t = Tuple{padic_mat_struct}

struct padic_poly_struct
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

struct n_poly_struct
  coeffs::Ptr{ulong}
  alloc::slong
  length::slong
end

const n_poly_t = Tuple{n_poly_struct}

const n_fq_poly_struct = n_poly_struct

const n_fq_poly_t = n_poly_t

struct n_bpoly_struct
  coeffs::Ptr{n_poly_struct}
  alloc::slong
  length::slong
end

const n_bpoly_t = Tuple{n_bpoly_struct}

const n_fq_bpoly_struct = n_bpoly_struct

const n_fq_bpoly_t = n_bpoly_t

struct n_tpoly_struct
  coeffs::Ptr{n_bpoly_struct}
  alloc::slong
  length::slong
end

const n_tpoly_t = Tuple{n_tpoly_struct}

const n_fq_tpoly_struct = n_tpoly_struct

const n_fq_tpoly_t = n_tpoly_t

struct n_polyu_struct
  exps::Ptr{ulong}
  coeffs::Ptr{ulong}
  length::slong
  alloc::slong
end

const n_polyu_t = Tuple{n_polyu_struct}

const n_fq_polyu_struct = n_polyu_struct

const n_fq_polyu_t = n_polyu_t

struct n_polyun_struct
  coeffs::Ptr{n_poly_struct}
  exps::Ptr{ulong}
  length::slong
  alloc::slong
end

const n_polyun_t = Tuple{n_polyun_struct}

const n_fq_polyun_struct = n_polyun_struct

const n_fq_polyun_t = n_polyun_t

struct n_poly_stack_struct
  array::Ptr{Ptr{n_poly_struct}}
  alloc::slong
  top::slong
end

const n_poly_stack_t = Tuple{n_poly_stack_struct}

struct n_bpoly_stack_struct
  array::Ptr{Ptr{n_bpoly_struct}}
  alloc::slong
  top::slong
end

const n_bpoly_stack_t = Tuple{n_bpoly_stack_struct}

struct n_poly_bpoly_stack_struct
  poly_stack::n_poly_stack_t
  bpoly_stack::n_bpoly_stack_t
end

const n_poly_bpoly_stack_t = Tuple{n_poly_bpoly_stack_struct}

struct nmod_eval_interp_struct
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

struct mpoly_ctx_struct
  nvars::slong
  nfields::slong
  ord::ordering_t
  deg::Cint
  rev::Cint
  lut_words_per_exp::NTuple{FLINT_BITS, slong}
  lut_fix_bits::NTuple{FLINT_BITS, Cuchar}
end

const mpoly_ctx_t = Tuple{mpoly_ctx_struct}

struct nmod_mpoly_ctx_struct
  minfo::mpoly_ctx_t
  mod::nmod_t
end

const nmod_mpoly_ctx_t = Tuple{nmod_mpoly_ctx_struct}

struct fmpz_mpoly_ctx_struct
  minfo::mpoly_ctx_t
end

const fmpz_mpoly_ctx_t = Tuple{fmpz_mpoly_ctx_struct}

struct fmpq_mpoly_ctx_struct
  zctx::fmpz_mpoly_ctx_t
end

const fmpq_mpoly_ctx_t = Tuple{fmpq_mpoly_ctx_struct}

struct fmpz_mod_mpoly_ctx_struct
  minfo::mpoly_ctx_t
  ffinfo::fmpz_mod_ctx_t
end

const fmpz_mod_mpoly_ctx_t = Tuple{fmpz_mod_mpoly_ctx_struct}

struct fq_nmod_mpoly_ctx_struct
  minfo::mpoly_ctx_t
  fqctx::fq_nmod_ctx_t
end

const fq_nmod_mpoly_ctx_t = Tuple{fq_nmod_mpoly_ctx_struct}

struct struct_mpoly_void_ring_t
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

struct mpoly_gcd_info_struct
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

struct mpoly_compression_struct
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

struct nmod_mpolyn_struct
  coeffs::Ptr{n_poly_struct}
  exps::Ptr{ulong}
  alloc::slong
  length::slong
  bits::slong
end

const nmod_mpolyn_t = Tuple{nmod_mpolyn_struct}

struct nmod_mpolyun_struct
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


end # module FlintC
