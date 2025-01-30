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


###############################################################################
# begin limb_types.h

const FLINT_MAX_FACTORS_IN_LIMB = 15

struct n_factor_t
  num::int
  exp::NTuple{FLINT_MAX_FACTORS_IN_LIMB, int}
  p::NTuple{FLINT_MAX_FACTORS_IN_LIMB, ulong}
end

struct n_primes_struct
  small_i::slong
  small_num::slong
  small_primes::Ptr{unsigned_int}

  sieve_a::ulong
  sieve_b::ulong
  sieve_i::slong
  sieve_num::slong
  sieve::Ptr{char}
end

const n_primes_t = Ptr{n_primes_struct}

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

const nmod_mat_t = Ptr{nmod_mat_struct}

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

const nmod_poly_factor_t = Ptr{nmod_poly_factor_struct}

struct nmod_poly_mat_struct
  entries::Ptr{nmod_poly_struct}
  r::slong
  c::slong
  rows::Ptr{Ptr{nmod_poly_struct}}
  modulus::ulong
end

const nmod_poly_mat_t = Ptr{nmod_poly_mat_struct}

struct nmod_mpoly_struct
  coeffs::Ptr{ulong}
  exps::Ptr{ulong}
  length::slong
  bits::flint_bitcnt_t    #= number of bits per exponent =#
  coeffs_alloc::slong     #= abs size in ulong units =#
  exps_alloc::slong       #= abs size in ulong units =#
end

const nmod_mpoly_t = Ptr{nmod_mpoly_struct}

struct nmod_mpoly_factor_struct
  constant::ulong
  poly::Ptr{nmod_mpoly_struct}
  exp::Ptr{fmpz}
  num::slong
  alloc::slong
end

const nmod_mpoly_factor_t = Ptr{nmod_mpoly_factor_struct}

# end nmod_types.h
###############################################################################


###############################################################################
# begin fmpz_types.h

struct zz_struct
  alloc::int
  size::int
  ptr::nn_ptr
end

typedef zz_struct * zz_ptr;
typedef const zz_struct * zz_srcptr;
#define FMPZ_TO_ZZ(x) ((zz_ptr) ((ulong) (x) << 2))

struct fmpz_factor_struct
  sign::int
  p::Ptr{fmpz}
  exp::Ptr{ulong}
  alloc::slong
  num::slong
end

const fmpz_factor_t = Ptr{fmpz_factor_struct}

struct fmpz_preinvn_struct
  dinv::nn_ptr
  n::slong
  norm::flint_bitcnt_t
end

const fmpz_preinvn_t = Ptr{fmpz_preinvn_struct}

struct fmpz_poly_struct
  coeffs::Ptr{fmpz}
  alloc::slong
  length::slong
end

const fmpz_poly_t = Ptr{fmpz_poly_struct}

struct fmpz_poly_factor_struct
  c::fmpz
  p::Ptr{fmpz_poly_struct}
  exp::Ptr{slong}
  num::slong
  alloc::slong
end

const fmpz_poly_factor_t = Ptr{fmpz_poly_factor_struct}

struct fmpz_mat_struct
  entries::Ptr{fmpz}
  r::slong
  c::slong
  rows::Ptr{Ptr{fmpz}}
end

const fmpz_mat_t = Ptr{fmpz_mat_struct}

struct fmpz_poly_mat_struct
  entries::Ptr{fmpz_poly_struct}
  r::slong
  c::slong
  rows::Ptr{Ptr{fmpz_poly_struct}}
end

const fmpz_poly_mat_t = Ptr{fmpz_poly_mat_struct}

struct fmpz_mpoly_struct
  coeffs::Ptr{fmpz} #= alloc fmpzs =#
  exps::Ptr{ulong}
  alloc::slong
  length::slong
  bits::flint_bitcnt_t     #= number of bits per exponent =#
end

const fmpz_mpoly_t = Ptr{fmpz_mpoly_struct}

struct fmpz_mpoly_factor_struct
  constant::fmpz_t
  constant_den::fmpz_t        #= should be one after normal operations =#
  poly::Ptr{fmpz_mpoly_struct}
  exp::Ptr{fmpz}
  num::slong
  alloc::slong
end

const fmpz_mpoly_factor_t = Ptr{fmpz_mpoly_factor_struct}

struct fmpz_poly_q_struct
  num::Ptr{fmpz_poly_struct}
  den::Ptr{fmpz_poly_struct}
end

const fmpz_poly_q_t = Ptr{fmpz_poly_q_struct}

struct fmpz_mpoly_q_struct
  num::fmpz_mpoly_struct
  den::fmpz_mpoly_struct
end

const fmpz_mpoly_q_t = Ptr{fmpz_mpoly_q_struct}

struct fmpzi_struct
  a::fmpz
  b::fmpz
end

const fmpzi_t = Ptr{fmpzi_struct}

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

const fmpq_mat_t = Ptr{fmpq_mat_struct}

struct fmpq_poly_struct
  coeffs::Ptr{fmpz}
  alloc::slong
  length::slong
  den::fmpz_t
end

const fmpq_poly_t = Ptr{fmpq_poly_struct}

#=
    A polynomial f is represented as
        content * zpoly,
    where zpoly should have positive leading coefficient and trivial content.
    If f is zero, then the representation should have
        content = 0 and zpoly = 0
=#

struct fmpq_mpoly_struct                       #= non zero case:                   |  zero case: =#
  content::fmpq_t     #= positive or negative content     |  zero       =#
  zpoly::fmpz_mpoly_t #= contentless poly, lc is positive |  zero       =#
end

const fmpq_mpoly_t = Ptr{fmpq_mpoly_struct}

struct fmpq_mpoly_factor_struct
  constant::fmpq_t
  poly::Ptr{fmpq_mpoly_struct}
  exp::Ptr{fmpz}
  num::slong
  alloc::slong
end

const fmpq_mpoly_factor_t = Ptr{fmpq_mpoly_factor_struct}

# end fmpq_types.h
###############################################################################


###############################################################################
# begin fmpz_mod_types.h

struct fmpz_mod_ctx_struct
  n::fmpz_t
  add_fxn::Ptr{Nothing}
  sub_fxn::Ptr{Nothing}
  mul_fxn::Ptr{Nothing}
  mod::nmod_t
  n_limbs::NTuple{3, ulong}
  ninv_limbs::NTuple{3, ulong}
  ninv_huge::Ptr{fmpz_preinvn_struct}
end
const struct_fmpz_mod_ctx = fmpz_mod_ctx_struct

const fmpz_mod_ctx_t = Ptr{fmpz_mod_ctx_struct}

const fmpz_mod_mat_struct = fmpz_mat_struct

const fmpz_mod_mat_t = Ptr{fmpz_mod_mat_struct}

struct fmpz_mod_poly_struct
  coeffs::Ptr{fmpz}
  alloc::slong
  length::slong
end

const fmpz_mod_poly_t = Ptr{fmpz_mod_poly_struct}

struct fmpz_mod_poly_factor_struct
  poly::Ptr{fmpz_mod_poly_struct}
  exp::Ptr{slong}
  num::slong
  alloc::slong
end

const fmpz_mod_poly_factor_t = Ptr{fmpz_mod_poly_factor_struct}

struct fmpz_mod_mpoly_struct
  coeffs::Ptr{fmpz}
  exps::Ptr{ulong}
  length::slong
  bits::flint_bitcnt_t    #= number of bits per exponent =#
  coeffs_alloc::slong     #= abs size in ulong units =#
  exps_alloc::slong       #= abs size in ulong units =#
end

const fmpz_mod_mpoly_t = Ptr{fmpz_mod_mpoly_struct}

struct fmpz_mod_mpoly_factor_struct
  constant::fmpz_t
  poly::Ptr{fmpz_mod_mpoly_struct}
  exp::Ptr{fmpz}
  num::slong
  alloc::slong
end

const fmpz_mod_mpoly_factor_t = Ptr{fmpz_mod_mpoly_factor_struct}

# end fmpz_mod_types.h
###############################################################################


###############################################################################
# begin fq_nmod_types.h

const fq_nmod_t = nmod_poly_t
const fq_nmod_struct = nmod_poly_struct

struct fq_nmod_ctx_struct
  mod::nmod_t

  sparse_modulus::int
  is_conway::int #= whether field was generated using Flint Conway table (assures primitivity =#

  a::Ptr{ulong}
  j::Ptr{slong}
  len::slong

  modulus::nmod_poly_t
  inv::nmod_poly_t

  var::Ptr{char}
end

const fq_nmod_ctx_t = Ptr{fq_nmod_ctx_struct}

struct fq_nmod_mat_struct
  entries::Ptr{fq_nmod_struct}
  r::slong
  c::slong
  rows::Ptr{Ptr{fq_nmod_struct}}
end

const fq_nmod_mat_t = Ptr{fq_nmod_mat_struct}

struct fq_nmod_poly_struct
  coeffs::Ptr{fq_nmod_struct}
  alloc::slong
  length::slong
end

const fq_nmod_poly_t = Ptr{fq_nmod_poly_struct}

struct fq_nmod_poly_factor_struct
  poly::Ptr{fq_nmod_poly_struct}
  exp::Ptr{slong}
  num::slong
  alloc::slong
end

const fq_nmod_poly_factor_t = Ptr{fq_nmod_poly_factor_struct}

struct fq_nmod_mpoly_struct
  coeffs::Ptr{ulong}
  exps::Ptr{ulong}
  length::slong
  bits::flint_bitcnt_t    #= number of bits per exponent =#
  coeffs_alloc::slong     #= abs size in ulong units =#
  exps_alloc::slong       #= abs size in ulong units =#
end

const fq_nmod_mpoly_t = Ptr{fq_nmod_mpoly_struct}

# end fq_nmod_types.h
###############################################################################


###############################################################################
# begin fq_zech_types.h

struct fq_zech_struct
  value::ulong
end

const fq_zech_t = Ptr{fq_zech_struct}

struct fq_zech_ctx_struct
  qm1::ulong     #= q - 1 =#
  qm1o2::ulong   #= (q - 1) / 2 or 1 when p == 2 =#
  qm1opm1::ulong #= (q - 1) / (p - 1) =#
  p::ulong
  ppre::double
  prime_root::ulong       #= primitive root for prime subfield =#
  zech_log_table::Ptr{ulong}
  prime_field_table::Ptr{ulong}
  eval_table::Ptr{ulong}

  fq_nmod_ctx::Ptr{fq_nmod_ctx_struct}
  owns_fq_nmod_ctx::int
  is_conway::int #= whether field was generated using Flint Conway tables (assures primitivity) =#
end

const fq_zech_ctx_t = Ptr{fq_zech_ctx_struct}

struct fq_zech_mat_struct
  entries::Ptr{fq_zech_struct}
  r::slong
  c::slong
  rows::Ptr{Ptr{fq_zech_struct}}
end

const fq_zech_mat_t = Ptr{fq_zech_mat_struct}

struct fq_zech_poly_struct
  coeffs::Ptr{fq_zech_struct}
  alloc::slong
  length::slong
end

const fq_zech_poly_t = Ptr{fq_zech_poly_struct}

struct fq_zech_poly_factor_struct
  poly::Ptr{fq_zech_poly_struct}
  exp::Ptr{slong}
  num::slong
  alloc::slong
end

const fq_zech_poly_factor_t = Ptr{fq_zech_poly_factor_struct}

# end fq_zech_types.h
###############################################################################


###############################################################################
# begin fq_types.h

const fq_t = fmpz_poly_t
const fq_struct = fmpz_poly_struct

struct fq_ctx_struct
  ctxp::fmpz_mod_ctx_t

  sparse_modulus::int
  is_conway::int #= whether field was initialized with the Flint Conway tables  (assures primitivity) =#

  a::Ptr{fmpz}
  j::Ptr{slong}
  len::slong

  modulus::fmpz_mod_poly_t
  inv::fmpz_mod_poly_t

  var::Ptr{char}
end

const fq_ctx_t = Ptr{fq_ctx_struct}

struct fq_mat_struct
  entries::Ptr{fq_struct}
  r::slong
  c::slong
  rows::Ptr{Ptr{fq_struct}}
end

const fq_mat_t = Ptr{fq_mat_struct}

struct fq_poly_struct
  coeffs::Ptr{fq_struct}
  alloc::slong
  length::slong
end

const fq_poly_t = Ptr{fq_poly_struct}

struct fq_poly_factor_struct
  poly::Ptr{fq_poly_struct}
  exp::Ptr{slong}
  num::slong
  alloc::slong
end

const fq_poly_factor_t = Ptr{fq_poly_factor_struct}

# end fq_types.h
###############################################################################


###############################################################################
# begin padic_types.h

struct padic_struct
  u::fmpz
  v::slong
  N::slong
end

const padic_t = Ptr{padic_struct}

@enum enum_padic_print_mode begin
  PADIC_TERSE
  PADIC_SERIES
  PADIC_VAL_UNIT
end

struct padic_ctx_struct
  p::fmpz_t
  pinv::double
  pow::Ptr{fmpz}
  min::slong
  max::slong
  mode::enum_padic_print_mode
end

const padic_ctx_t = Ptr{padic_ctx_struct}

struct padic_inv_struct
  n::slong
  pow::Ptr{fmpz}
end

const padic_inv_t = Ptr{padic_inv_struct}

struct padic_mat_struct
  mat::fmpz_mat_struct
  val::slong
  N::slong
end

const padic_mat_t = Ptr{padic_mat_struct}

struct padic_poly_struct
  coeffs::Ptr{fmpz}
  alloc::slong
  length::slong
  val::slong
  N::slong
end

const padic_poly_t = Ptr{padic_poly_struct}

# end padic_types.h
###############################################################################


###############################################################################
# begin n_poly_types.h

#= arrays of ulong =#
struct n_poly_struct
  coeffs::Ptr{ulong}
  alloc::slong
  length::slong
end

const n_poly_t = Ptr{n_poly_struct}
const n_fq_poly_struct = n_poly_struct
const n_fq_poly_t = n_poly_t

#= arrays of arrays of ulong =#
struct n_bpoly_struct
  coeffs::Ptr{n_poly_struct}
  alloc::slong
  length::slong
end

const n_bpoly_t = Ptr{n_bpoly_struct}
const n_fq_bpoly_struct = n_bpoly_struct
const n_fq_bpoly_t = n_bpoly_t

#= arrays of arrays of arrays of ulong =#
struct n_tpoly_struct
  coeffs::Ptr{n_bpoly_struct}
  alloc::slong
  length::slong
end

const n_tpoly_t = Ptr{n_tpoly_struct}
const n_fq_tpoly_struct = n_tpoly_struct
const n_fq_tpoly_t = n_tpoly_t

#= sparse arrays of ulong =#
struct n_polyu_struct
  exps::Ptr{ulong}
  coeffs::Ptr{ulong}
  length::slong
  alloc::slong
end

const n_polyu_t = Ptr{n_polyu_struct}
const n_fq_polyu_struct = n_polyu_struct
const n_fq_polyu_t = n_polyu_t

#=
    sparse arrays of arrays of ulong
    n_polyu1n => one exponent is in the exps[i]
    n_polyu2n => two exponents are packed into the exps[i]
    ...
=#
struct n_polyun_struct
  coeffs::Ptr{n_poly_struct}
  exps::Ptr{ulong}
  length::slong
  alloc::slong
end

const n_polyun_t = Ptr{n_polyun_struct}
const n_fq_polyun_struct = n_polyun_struct
const n_fq_polyun_t = n_polyun_t

#= n_poly stack =#
struct n_poly_stack_struct
  array::Ptr{Ptr{n_poly_struct}}
  alloc::slong
  top::slong
end

const n_poly_stack_t = Ptr{n_poly_stack_struct}

#= n_bpoly stack =#
struct n_bpoly_stack_struct
  array::Ptr{Ptr{n_bpoly_struct}}
  alloc::slong
  top::slong
end

const n_bpoly_stack_t = Ptr{n_bpoly_stack_struct}

struct n_poly_bpoly_stack_struct
  poly_stack::n_poly_stack_t
  bpoly_stack::n_bpoly_stack_t
end

const n_poly_bpoly_stack_t = Ptr{n_poly_bpoly_stack_struct}

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

const nmod_eval_interp_t = Ptr{nmod_eval_interp_struct}

# end n_poly_types.h
###############################################################################


###############################################################################
# begin mpoly_types.h

#define MPOLY_MIN_BITS (UWORD(8))    #= minimum number of bits to pack into =#

@enum ordering_t begin
  ORD_LEX
  ORD_DEGLEX
  ORD_DEGREVLEX
end

const MPOLY_NUM_ORDERINGS = 3

struct mpoly_ctx_struct
  nvars::slong    #= number of variables =#
  nfields::slong  #= number of fields in exponent vector =#
  ord::ordering_t #= monomial ordering =#
  deg::int        #= is ord a degree ordering? =#
  rev::int        #= is ord a reversed ordering? =#
  lut_words_per_exp::NTuple{FLINT_BITS, slong}
  lut_fix_bits::NTuple{FLINT_BITS, unsigned_char} #= FLINT_BITS < 256 =#
end

const mpoly_ctx_t = Ptr{mpoly_ctx_struct}

struct nmod_mpoly_ctx_struct
  minfo::mpoly_ctx_t
  mod::nmod_t
end

const nmod_mpoly_ctx_t = Ptr{nmod_mpoly_ctx_struct}

struct fmpz_mpoly_ctx_struct
  minfo::mpoly_ctx_t
end

const fmpz_mpoly_ctx_t = Ptr{fmpz_mpoly_ctx_struct}

struct fmpq_mpoly_ctx_struct
  zctx::fmpz_mpoly_ctx_t
end

const fmpq_mpoly_ctx_t = Ptr{fmpq_mpoly_ctx_struct}

struct fmpz_mod_mpoly_ctx_struct
  minfo::mpoly_ctx_t
  ffinfo::fmpz_mod_ctx_t
end

const fmpz_mod_mpoly_ctx_t = Ptr{fmpz_mod_mpoly_ctx_struct}

struct fq_nmod_mpoly_ctx_struct
  minfo::mpoly_ctx_t
  fqctx::fq_nmod_ctx_t
end

const fq_nmod_mpoly_ctx_t = Ptr{fq_nmod_mpoly_ctx_struct}


struct struct_mpoly_void_ring_t
  elem_size::slong
  ctx::Ptr{void}
  init::Ptr{Nothing}
  clear::Ptr{Nothing}
  is_zero::Ptr{Nothing}
  zero::Ptr{Nothing}
  one::Ptr{Nothing}
  set_fmpz::Ptr{Nothing}
  set::Ptr{Nothing}
  swap::Ptr{Nothing}
  neg::Ptr{Nothing}
  add::Ptr{Nothing}
  sub::Ptr{Nothing}
  mul_fmpz::Ptr{Nothing}
  mul::Ptr{Nothing}
  divexact::Ptr{Nothing}
  divides::Ptr{Nothing}
  pow_fmpz::Ptr{Nothing}
  length::Ptr{Nothing}
end
const mpoly_void_ring_t = Ptr{struct_mpoly_void_ring_t}

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

  Adensity::double
  Bdensity::double

  hensel_time::double
  brown_time::double
  zippel_time::double
  zippel2_time::double
  hensel_perm::Ptr{slong}
  brown_perm::Ptr{slong}
  zippel_perm::Ptr{slong}
  zippel2_perm::Ptr{slong}
  can_use::unsigned_int
  Gdeflate_deg_bounds_are_nice::int #= all of Gdeflate_deg_bound came from real gcd computations =#

  data::Ptr{char}
end

const mpoly_gcd_info_t = Ptr{mpoly_gcd_info_struct}

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
  is_trivial::int
  is_perm::int
  is_irred::int
end

const mpoly_compression_t = Ptr{mpoly_compression_struct}

#=
    nmod_mpolyn_t
    multivariates with n_poly_t coefficients
=#
struct nmod_mpolyn_struct
  coeffs::Ptr{n_poly_struct}
  exps::Ptr{ulong}
  alloc::slong
  length::slong
  bits::slong
end
const nmod_mpolyn_t = Ptr{nmod_mpolyn_struct}

#=
    nmod_mpolyun_t
    sparse univariates with nmod_mpolyn_t coefficients
        with uniform bits and LEX ordering
=#
struct nmod_mpolyun_struct
  coeffs::Ptr{nmod_mpolyn_struct}
  exps::Ptr{ulong}
  alloc::slong
  length::slong
  bits::flint_bitcnt_t   #= default bits to construct coeffs =#
end
const nmod_mpolyun_t = Ptr{nmod_mpolyun_struct}

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
