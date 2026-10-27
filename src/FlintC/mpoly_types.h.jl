# This file has been generated from FLINT's `mpoly_types.h`. DO NOT EDIT MANUALLY.
#
# To regenerate it, e.g. after updating FLINT_jll, run
#   julia etc/generate_FLINT_structs.jl
#
# Generated with FLINT_jll v301.600.1+0.

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
