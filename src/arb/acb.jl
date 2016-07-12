###############################################################################
#
#   acb.jl : Arb complex numbers
#
#   Copyright (C) 2015 Tommy Hofmann
#   Copyright (C) 2015 Fredrik Johansson
#
###############################################################################

import Base: real, imag, abs, conj, angle,
       sqrt, log, log1p, exp, sin, cos, tan, cot,
       sinpi, cospi, sinh, cosh, tanh, coth,
       atan, gamma, lgamma, digamma, polygamma,
       erf, erfi, erfc, gamma,
       besselj, bessely, besseli, besselk

export one, onei, real, imag, conj, abs, inv, angle

export sqrt, rsqrt, log, log1p, exp, exppii, sin, cos, tan, cot,
       sinpi, cospi, tanpi, cotpi, sincos, sincospi, sinh, cosh, tanh, coth,
       sinhcosh, atan, logsinpi, gamma, rgamma, lgamma, digamma, risingfac,
       risingfac2, polygamma, polylog, zeta, barnesg, logbarnesg, agm,
       erf, erfi, erfc, ei, si, ci, shi, chi, li, lioffset, expint, gamma,
       besselj, bessely, besseli, besselk, hyp1f1, hyp1f1r, hyperu,
       jtheta, modeta, modj, modlambda, moddelta, ellipwp, ellipk, ellipe


###############################################################################
#
#   Basic manipulation
#
###############################################################################

elem_type(::AcbField) = acb

parent_type(::Type{acb}) = AcbField

base_ring(R::AcbField) = Union{} 

################################################################################
#
#  Real and imaginary part
#
################################################################################

function real(x::acb)
  z = arb()
  ccall((:acb_get_real, :libarb), Void, (Ptr{arb}, Ptr{acb}), &z, &x)
  z.parent = ArbField(parent(x).prec)
  return z
end

function imag(x::acb)
  z = arb()
  ccall((:acb_get_imag, :libarb), Void, (Ptr{arb}, Ptr{acb}), &z, &x)
  z.parent = ArbField(parent(x).prec)
  return z
end

################################################################################
#
#  String I/O
#
################################################################################

function show(io::IO, x::acb)
  show(io, real(x))
  print(io, " + i*")
  show(io, imag(x))
end

################################################################################
#
#  Special elements
#
################################################################################

function zero(r::AcbField)
  z = acb()
  z.parent = r
  return z
end

function one(r::AcbField)
  z = acb()
  ccall((:acb_one, :libarb), Void, (Ptr{acb}, ), &z)
  z.parent = r
  return z
end

function onei(r::AcbField)
  z = acb()
  ccall((:acb_onei, :libarb), Void, (Ptr{acb}, ), &z)
  z.parent = r
  return z
end

################################################################################
#
#  Comparison and predicates
#
################################################################################

function isequal(x::acb, y::acb)
  r = ccall((:acb_equal, :libarb), Cint, (Ptr{acb}, Ptr{acb}), &x, &y)
  return Bool(r)
end

function ==(x::acb, y::acb)
  r = ccall((:acb_eq, :libarb), Cint, (Ptr{acb}, Ptr{acb}), &x, &y)
  return Bool(r)
end

function !=(x::acb, y::acb)
  r = ccall((:acb_ne, :libarb), Cint, (Ptr{acb}, Ptr{acb}), &x, &y)
  return Bool(r)
end

==(x::acb,y::Int) = (x == parent(x)(y))
==(x::Int,y::acb) = (y == parent(y)(x))

==(x::acb,y::arb) = (x == parent(x)(y))
==(x::arb,y::acb) = (y == parent(y)(x))

==(x::acb,y::fmpz) = (x == parent(x)(y))
==(x::fmpz,y::acb) = (y == parent(y)(x))

==(x::acb,y::Float64) = (x == parent(x)(y))
==(x::Float64,y::acb) = (y == parent(y)(x))

!=(x::acb,y::Int) = (x != parent(x)(y))
!=(x::Int,y::acb) = (y != parent(y)(x))

!=(x::acb,y::arb) = (x != parent(x)(y))
!=(x::arb,y::acb) = (y != parent(y)(x))

!=(x::acb,y::fmpz) = (x != parent(x)(y))
!=(x::fmpz,y::acb) = (y != parent(y)(x))

!=(x::acb,y::Float64) = (x != parent(x)(y))
!=(x::Float64,y::acb) = (y != parent(y)(x))

function overlaps(x::acb, y::acb)
  r = ccall((:acb_overlaps, :libarb), Cint, (Ptr{acb}, Ptr{acb}), &x, &y)
  return Bool(r)
end

function contains(x::acb, y::acb)
  r = ccall((:acb_contains, :libarb), Cint, (Ptr{acb}, Ptr{acb}), &x, &y)
  return Bool(r)
end

function contains(x::acb, y::fmpq)
  r = ccall((:acb_contains_fmpq, :libarb), Cint, (Ptr{acb}, Ptr{fmpq}), &x, &y)
  return Bool(r)
end

function contains(x::acb, y::fmpz)
  r = ccall((:acb_contains_fmpz, :libarb), Cint, (Ptr{acb}, Ptr{fmpz}), &x, &y)
  return Bool(r)
end

function contains(x::acb, y::Int)
  v = fmpz(y)
  r = ccall((:acb_contains_fmpz, :libarb), Cint, (Ptr{acb}, Ptr{fmpz}), &x, &v)
  return Bool(r)
end

for (s,f) in (("iszero", "acb_is_zero"),
              ("isone", "acb_is_one"),
              ("isfinite", "acb_is_finite"),
              ("isexact", "acb_is_exact"),
              ("isint", "acb_is_int"),
              ("isreal", "acb_is_real"),
              ("contains_zero", "acb_contains_zero"))
  @eval begin
    function($(Symbol(s)))(x::acb)
      return Bool(ccall(($f, :libarb), Cint, (Ptr{acb},), &x))
    end
  end
end

################################################################################
#
#  Precision, shifting and other operations
#
################################################################################

function ldexp(x::acb, y::Int)
  z = parent(x)()
  ccall((:acb_mul_2exp_si, :libarb), Void,
              (Ptr{acb}, Ptr{acb}, Int), &z, &x, y)
  return z
end

function ldexp(x::acb, y::fmpz)
  z = parent(x)()
  ccall((:acb_mul_2exp_fmpz, :libarb), Void,
              (Ptr{acb}, Ptr{acb}, Ptr{fmpz}), &z, &x, &y)
  return z
end

function trim(x::acb)
  z = parent(x)()
  ccall((:acb_trim, :libarb), Void, (Ptr{acb}, Ptr{acb}), &z, &x)
  return z
end

function accuracy_bits(x::acb)
  # bug in acb.h: rel_accuracy_bits is not in the library
  return -ccall((:acb_rel_error_bits, :libarb), Int, (Ptr{acb},), &x)
end

function unique_integer(x::acb)
  z = fmpz()
  unique = ccall((:arb_get_unique_fmpz, :libarb), Int,
    (Ptr{fmpz}, Ptr{acb}), &z, &x)
  return (unique != 0, z)
end

################################################################################
#
#  Unary operations
#
################################################################################

function -(x::acb)
  z = parent(x)()
  ccall((:acb_neg, :libarb), Void, (Ptr{acb}, Ptr{acb}), &z, &x)
  return z
end

function inv(x::acb)
  z = parent(x)()
  ccall((:acb_inv, :libarb), Void, (Ptr{acb}, Ptr{acb}, Int), &z, &x, parent(x).prec)
  return z
end

################################################################################
#
#  Binary operations
#
################################################################################

# acb - acb

for (s,f) in ((:+,"acb_add"), (:*,"acb_mul"), (://, "acb_div"), (:-,"acb_sub"), (:^,"acb_pow"))
  @eval begin
    function ($s)(x::acb, y::acb)
      z = parent(x)()
      ccall(($f, :libarb), Void, (Ptr{acb}, Ptr{acb}, Ptr{acb}, Int),
                           &z, &x, &y, parent(x).prec)
      return z
    end
  end
end

for (f,s) in ((:+, "add"), (:-, "sub"), (:*, "mul"), (://, "div"), (:^, "pow"))
  @eval begin

    function ($f)(x::acb, y::UInt)
      z = parent(x)()
      ccall(($("acb_"*s*"_ui"), :libarb), Void,
                  (Ptr{acb}, Ptr{acb}, UInt, Int),
                  &z, &x, y, parent(x).prec)
      return z
    end

    function ($f)(x::acb, y::Int)
      z = parent(x)()
      ccall(($("acb_"*s*"_si"), :libarb), Void,
      (Ptr{acb}, Ptr{acb}, Int, Int), &z, &x, y, parent(x).prec)
      return z
    end

    function ($f)(x::acb, y::fmpz)
      z = parent(x)()
      ccall(($("acb_"*s*"_fmpz"), :libarb), Void,
                  (Ptr{acb}, Ptr{acb}, Ptr{fmpz}, Int),
                  &z, &x, &y, parent(x).prec)
      return z
    end

    function ($f)(x::acb, y::arb)
      z = parent(x)()
      ccall(($("acb_"*s*"_arb"), :libarb), Void,
                  (Ptr{acb}, Ptr{acb}, Ptr{arb}, Int),
                  &z, &x, &y, parent(x).prec)
      return z
    end
  end
end


+(x::UInt,y::acb) = +(y,x)
+(x::Int,y::acb) = +(y,x)
+(x::fmpz,y::acb) = +(y,x)
+(x::arb,y::acb) = +(y,x)

*(x::UInt,y::acb) = *(y,x)
*(x::Int,y::acb) = *(y,x)
*(x::fmpz,y::acb) = *(y,x)
*(x::arb,y::acb) = *(y,x)

//(x::UInt,y::acb) = (x == 1) ? inv(y) : parent(y)(x) // y
//(x::Int,y::acb) = (x == 1) ? inv(y) : parent(y)(x) // y
//(x::fmpz,y::acb) = isone(x) ? inv(y) : parent(y)(x) // y
//(x::arb,y::acb) = isone(x) ? inv(y) : parent(y)(x) // y

^(x::UInt,y::acb) = parent(y)(x) ^ y
^(x::Int,y::acb) = parent(y)(x) ^ y
^(x::fmpz,y::acb) = parent(y)(x) ^ y
^(x::arb,y::acb) = parent(y)(x) ^ y

function -(x::UInt, y::acb)
  z = parent(y)()
  ccall((:acb_sub_ui, :libarb), Void, (Ptr{acb}, Ptr{acb}, UInt, Int), &z, &y, x, parent(y).prec)
  ccall((:acb_neg, :libarb), Void, (Ptr{acb}, Ptr{acb}), &z, &z)
  return z
end

function -(x::Int, y::acb)
  z = parent(y)()
  ccall((:acb_sub_si, :libarb), Void, (Ptr{acb}, Ptr{acb}, Int, Int), &z, &y, x, parent(y).prec)
  ccall((:acb_neg, :libarb), Void, (Ptr{acb}, Ptr{acb}), &z, &z)
  return z
end

function -(x::fmpz, y::acb)
  z = parent(y)()
  ccall((:acb_sub_fmpz, :libarb), Void, (Ptr{acb}, Ptr{acb}, Ptr{fmpz}, Int), &z, &y, &x, parent(y).prec)
  ccall((:acb_neg, :libarb), Void, (Ptr{acb}, Ptr{acb}), &z, &z)
  return z
end

function -(x::arb, y::acb)
  z = parent(y)()
  ccall((:acb_sub_arb, :libarb), Void, (Ptr{acb}, Ptr{acb}, Ptr{arb}, Int), &z, &y, &x, parent(y).prec)
  ccall((:acb_neg, :libarb), Void, (Ptr{acb}, Ptr{acb}), &z, &z)
  return z
end

+(x::acb, y::fmpq) = x + parent(x)(y)
-(x::acb, y::fmpq) = x - parent(x)(y)
*(x::acb, y::fmpq) = x * parent(x)(y)
//(x::acb, y::fmpq) = x // parent(x)(y)
^(x::acb, y::fmpq) = x ^ parent(x)(y)

+(x::fmpq, y::acb) = parent(y)(x) + y
-(x::fmpq, y::acb) = parent(y)(x) - y
*(x::fmpq, y::acb) = parent(y)(x) * y
//(x::fmpq, y::acb) = parent(y)(x) // y
^(x::fmpq, y::acb) = parent(y)(x) ^ y

################################################################################
#
#  Basic functions
#
###############################################################################

function conj(x::acb)
  z = parent(x)()
  ccall((:acb_conj, :libarb), Void, (Ptr{acb}, Ptr{acb}), &z, &x)
  return z
end

function abs(x::acb)
  z = arb()
  ccall((:acb_abs, :libarb), Void,
                (Ptr{arb}, Ptr{acb}, Int), &z, &x, parent(x).prec)
  z.parent = ArbField(parent(x).prec)
  return z
end

function angle(x::acb)
  z = arb()
  ccall((:acb_arg, :libarb), Void,
                (Ptr{arb}, Ptr{acb}, Int), &z, &x, parent(x).prec)
  z.parent = ArbField(parent(x).prec)
  return z
end

################################################################################
#
#  Constants
#
################################################################################

function const_pi(r::AcbField)
  z = r()
  ccall((:acb_const_pi, :libarb), Void, (Ptr{acb}, Int), &z, prec(r))
  return z
end

################################################################################
#
#  Complex valued functions
#
################################################################################

# complex - complex functions
for (s,f) in (
              ("sqrt", "acb_sqrt"),
              ("rsqrt", "acb_rsqrt"),
              ("log", "acb_log"),
              ("log1p", "acb_log1p"),
              ("exp", "acb_exp"),
              ("exppii", "acb_exp_pi_i"),
              ("sin", "acb_sin"),
              ("cos", "acb_cos"),
              ("tan", "acb_tan"),
              ("cot", "acb_cot"),
              ("sinpi", "acb_sin_pi"),
              ("cospi", "acb_cos_pi"),
              ("tanpi", "acb_tan_pi"),
              ("cotpi", "acb_cot_pi"),
              ("sinh", "acb_sinh"),
              ("cosh", "acb_cosh"),
              ("tanh", "acb_tanh"),
              ("coth", "acb_coth"),
              ("atan", "acb_atan"),
              ("logsinpi", "acb_log_sin_pi"),
              ("gamma", "acb_gamma"),
              ("rgamma", "acb_rgamma"),
              ("lgamma", "acb_lgamma"),
              ("digamma", "acb_digamma"),
              ("zeta", "acb_zeta"),
              ("barnesg", "acb_barnes_g"),
              ("logbarnesg", "acb_log_barnes_g"),
              ("agm", "acb_agm1"),
              ("erf", "acb_hypgeom_erf"),
              ("erfi", "acb_hypgeom_erfi"),
              ("erfc", "acb_hypgeom_erfc"),
              ("ei", "acb_hypgeom_ei"),
              ("si", "acb_hypgeom_si"),
              ("ci", "acb_hypgeom_ci"),
              ("shi", "acb_hypgeom_shi"),
              ("chi", "acb_hypgeom_chi"),
              ("modeta", "acb_modular_eta"),
              ("modj", "acb_modular_j"),
              ("modlambda", "acb_modular_lambda"),
              ("moddelta", "acb_modular_delta"),
              ("ellipk", "acb_modular_elliptic_k"),
              ("ellipe", "acb_modular_elliptic_e"),
             )
  @eval begin
    function($(Symbol(s)))(x::acb)
      z = parent(x)()
      ccall(($f, :libarb), Void, (Ptr{acb}, Ptr{acb}, Int), &z, &x, parent(x).prec)
      return z
    end
  end
end

function sincos(x::acb)
  s = parent(x)()
  c = parent(x)()
  ccall((:acb_sin_cos, :libarb), Void,
              (Ptr{acb}, Ptr{acb}, Ptr{acb}, Int), &s, &c, &x, parent(x).prec)
  return (s, c)
end

function sincospi(x::acb)
  s = parent(x)()
  c = parent(x)()
  ccall((:acb_sin_cos_pi, :libarb), Void,
              (Ptr{acb}, Ptr{acb}, Ptr{acb}, Int), &s, &c, &x, parent(x).prec)
  return (s, c)
end

function sinhcosh(x::acb)
  s = parent(x)()
  c = parent(x)()
  ccall((:acb_sinh_cosh, :libarb), Void,
              (Ptr{acb}, Ptr{acb}, Ptr{acb}, Int), &s, &c, &x, parent(x).prec)
  return (s, c)
end

function zeta(s::acb, a::acb)
  z = parent(s)()
  ccall((:acb_hurwitz_zeta, :libarb), Void,
              (Ptr{acb}, Ptr{acb}, Ptr{acb}, Int), &z, &s, &a, parent(s).prec)
  return z
end

function polygamma(s::acb, a::acb)
  z = parent(s)()
  ccall((:acb_polygamma, :libarb), Void,
              (Ptr{acb}, Ptr{acb}, Ptr{acb}, Int), &z, &s, &a, parent(s).prec)
  return z
end

function risingfac(x::acb, n::UInt)
  z = parent(x)()
  ccall((:acb_rising_ui, :libarb), Void,
              (Ptr{acb}, Ptr{acb}, UInt, Int), &z, &x, n, parent(x).prec)
  return z
end

risingfac(x::acb, n::Int) = n < 0 ? throw(DomainError()) : risingfac(x, UInt(n))

function risingfac2(x::acb, n::UInt)
  z = parent(x)()
  w = parent(x)()
  ccall((:acb_rising2_ui, :libarb), Void,
              (Ptr{acb}, Ptr{acb}, Ptr{acb}, UInt, Int), &z, &w, &x, n, parent(x).prec)
  return (z, w)
end

risingfac2(x::acb, n::Int) = n < 0 ? throw(DomainError()) : risingfac2(x, UInt(n))

function polylog(s::acb, a::acb)
  z = parent(s)()
  ccall((:acb_polylog, :libarb), Void,
              (Ptr{acb}, Ptr{acb}, Ptr{acb}, Int), &z, &s, &a, parent(s).prec)
  return z
end

function polylog(s::Int, a::acb)
  z = parent(a)()
  ccall((:acb_polylog_si, :libarb), Void,
              (Ptr{acb}, Int, Ptr{acb}, Int), &z, s, &a, parent(a).prec)
  return z
end

function li(x::acb)
  z = parent(x)()
  ccall((:acb_hypgeom_li, :libarb), Void,
              (Ptr{acb}, Ptr{acb}, Int, Int), &z, &x, 0, parent(x).prec)
  return z
end

function lioffset(x::acb)
  z = parent(x)()
  ccall((:acb_hypgeom_li, :libarb), Void,
              (Ptr{acb}, Ptr{acb}, Int, Int), &z, &x, 1, parent(x).prec)
  return z
end

function expint(s::acb, x::acb)
  z = parent(s)()
  ccall((:acb_hypgeom_expint, :libarb), Void,
              (Ptr{acb}, Ptr{acb}, Ptr{acb}, Int), &z, &s, &x, parent(s).prec)
  return z
end

function gamma(s::acb, x::acb)
  z = parent(s)()
  ccall((:acb_hypgeom_gamma_upper, :libarb), Void,
              (Ptr{acb}, Ptr{acb}, Ptr{acb}, Int, Int), &z, &s, &x, 0, parent(s).prec)
  return z
end

function besselj(nu::acb, x::acb)
  z = parent(x)()
  ccall((:acb_hypgeom_bessel_j, :libarb), Void,
              (Ptr{acb}, Ptr{acb}, Ptr{acb}, Int), &z, &nu, &x, parent(x).prec)
  return z
end

function bessely(nu::acb, x::acb)
  z = parent(x)()
  ccall((:acb_hypgeom_bessel_y, :libarb), Void,
              (Ptr{acb}, Ptr{acb}, Ptr{acb}, Int), &z, &nu, &x, parent(x).prec)
  return z
end

function besseli(nu::acb, x::acb)
  z = parent(x)()
  ccall((:acb_hypgeom_bessel_i, :libarb), Void,
              (Ptr{acb}, Ptr{acb}, Ptr{acb}, Int), &z, &nu, &x, parent(x).prec)
  return z
end

function besselk(nu::acb, x::acb)
  z = parent(x)()
  ccall((:acb_hypgeom_bessel_k, :libarb), Void,
              (Ptr{acb}, Ptr{acb}, Ptr{acb}, Int), &z, &nu, &x, parent(x).prec)
  return z
end

function hyp1f1(a::acb, b::acb, x::acb)
  z = parent(x)()
  ccall((:acb_hypgeom_m, :libarb), Void,
              (Ptr{acb}, Ptr{acb}, Ptr{acb}, Ptr{acb}, Int, Int), &z, &a, &b, &x, 0, parent(x).prec)
  return z
end

function hyp1f1r(a::acb, b::acb, x::acb)
  z = parent(x)()
  ccall((:acb_hypgeom_m, :libarb), Void,
              (Ptr{acb}, Ptr{acb}, Ptr{acb}, Ptr{acb}, Int, Int), &z, &a, &b, &x, 1, parent(x).prec)
  return z
end

function hyperu(a::acb, b::acb, x::acb)
  z = parent(x)()
  ccall((:acb_hypgeom_u, :libarb), Void,
              (Ptr{acb}, Ptr{acb}, Ptr{acb}, Ptr{acb}, Int, Int), &z, &a, &b, &x, parent(x).prec)
  return z
end

function jtheta(z::acb, tau::acb)
  t1 = parent(z)()
  t2 = parent(z)()
  t3 = parent(z)()
  t4 = parent(z)()
  ccall((:acb_modular_theta, :libarb), Void,
              (Ptr{acb}, Ptr{acb}, Ptr{acb}, Ptr{acb}, Ptr{acb}, Ptr{acb}, Int),
                &t1, &t2, &t3, &t4, &z, &tau, parent(z).prec)
  return (t1, t2, t3, t4)
end

function ellipwp(z::acb, tau::acb)
  r = parent(z)()
  ccall((:acb_modular_elliptic_p, :libarb), Void,
              (Ptr{acb}, Ptr{acb}, Ptr{acb}, Int), &r, &z, &tau, parent(z).prec)
  return r
end

function agm(x::acb, y::acb)
  v = inv(y)
  if isfinite(v)
    return agm(x * v) * y
  else
    v = inv(x)
    return agm(y * v) * x
  end
end

################################################################################
#
#  Unsafe arithmetic
#
################################################################################

function add!(z::acb, x::acb, y::acb)
  ccall((:acb_add, :libarb), Void, (Ptr{acb}, Ptr{acb}, Ptr{acb}, Int),
         &z, &x, &y, parent(z).prec)
end

function sub!(z::acb, x::acb, y::acb)
  ccall((:acb_sub, :libarb), Void, (Ptr{acb}, Ptr{acb}, Ptr{acb}, Int),
        &z, &x, &y, parent(z).prec)
end

function mul!(z::acb, x::acb, y::acb)
  ccall((:acb_mul, :libarb), Void, (Ptr{acb}, Ptr{acb}, Ptr{acb}, Int),
        &z, &x, &y, parent(z).prec)
end

function div!(z::acb, x::acb, y::acb)
  ccall((:acb_div, :libarb), Void, (Ptr{acb}, Ptr{acb}, Ptr{acb}, Int),
        &z, &x, &y, parent(z).prec)
end

################################################################################
#
#  Unsafe setting
#
################################################################################

for (typeofx, passtoc) in ((acb, Ref{acb}), (Ptr{acb}, Ptr{acb}))
  for (f,t) in (("acb_set_si", Int), ("acb_set_ui", UInt),
                ("acb_set_d", Float64))
    @eval begin
      function _acb_set(x::($typeofx), y::($t))
        ccall(($f, :libarb), Void, (($passtoc), ($t)), x, y)
      end

      function _acb_set(x::($typeofx), y::($t), p::Int)
        _acb_set(x, y)
        ccall((:acb_set_round, :libarb), Void,
                    (($passtoc), ($passtoc), Int), x, x, p)
      end
    end
  end

  @eval begin
    function _acb_set(x::($typeofx), y::fmpz)
      ccall((:acb_set_fmpz, :libarb), Void, (($passtoc), Ptr{fmpz}), x, &y)
    end

    function _acb_set(x::($typeofx), y::fmpz, p::Int)
      ccall((:acb_set_round_fmpz, :libarb), Void,
                  (($passtoc), Ptr{fmpz}, Int), x, &y, p)
    end

    function _acb_set(x::($typeofx), y::fmpq, p::Int)
      ccall((:acb_set_fmpq, :libarb), Void,
                  (($passtoc), Ptr{fmpq}, Int), x, &y, p)
    end

    function _acb_set(x::($typeofx), y::arb)
      ccall((:acb_set_arb, :libarb), Void, (($passtoc), Ptr{arb}), x, &y)
    end

    function _acb_set(x::($typeofx), y::arb, p::Int)
      _acb_set(x, y)
      ccall((:acb_set_round, :libarb), Void,
                  (($passtoc), ($passtoc), Int), x, x, p)
    end

    function _acb_set(x::($typeofx), y::acb)
      ccall((:acb_set, :libarb), Void, (($passtoc), Ptr{acb}, Int), x, &y)
    end

    function _acb_set(x::($typeofx), y::acb, p::Int)
      ccall((:acb_set_round, :libarb), Void,
                  (($passtoc), Ptr{acb}, Int), x, &y, p)
    end

    function _acb_set(x::($typeofx), y::AbstractString, p::Int)
      r = ccall((:acb_real_ptr, :libarb), Ptr{arb}, (($passtoc), ), x)
      _arb_set(r, y, p)
      i = ccall((:acb_imag_ptr, :libarb), Ptr{arb}, (($passtoc), ), x)
      ccall((:arb_zero, :libarb), Void, (Ptr{arb}, ), i)
    end

    function _acb_set(x::($typeofx), y::BigFloat)
      r = ccall((:acb_real_ptr, :libarb), Ptr{arb}, (($passtoc), ), x)
      _arb_set(r, y)
      i = ccall((:acb_imag_ptr, :libarb), Ptr{arb}, (($passtoc), ), x)
      ccall((:arb_zero, :libarb), Void, (Ptr{arb}, ), i)
    end

    function _acb_set(x::($typeofx), y::BigFloat, p::Int)
      r = ccall((:acb_real_ptr, :libarb), Ptr{arb}, (($passtoc), ), x)
      _arb_set(r, y, p)
      i = ccall((:acb_imag_ptr, :libarb), Ptr{arb}, (($passtoc), ), x)
      ccall((:arb_zero, :libarb), Void, (Ptr{arb}, ), i)
    end

    function _acb_set(x::($typeofx), y::Int, z::Int, p::Int)
      ccall((:acb_set_si_si, :libarb), Void,
                  (($passtoc), Int, Int), x, y, z)
      ccall((:acb_set_round, :libarb), Void,
                  (($passtoc), ($passtoc), Int), x, x, p)
    end

    function _acb_set(x::($typeofx), y::arb, z::arb)
      ccall((:acb_set_arb_arb, :libarb), Void,
                  (($passtoc), Ptr{arb}, Ptr{arb}), x, &y, &z)
    end

    function _acb_set(x::($typeofx), y::arb, z::arb, p::Int)
      _acb_set(x, y, z)
      ccall((:acb_set_round, :libarb), Void,
                  (($passtoc), ($passtoc), Int), x, x, p)
    end

    function _acb_set(x::($typeofx), y::fmpq, z::fmpq, p::Int)
      r = ccall((:acb_real_ptr, :libarb), Ptr{arb}, (($passtoc), ), x)
      _arb_set(r, y, p)
      i = ccall((:acb_imag_ptr, :libarb), Ptr{arb}, (($passtoc), ), x)
      _arb_set(i, z, p)
    end
    
    function _acb_set{T <: AbstractString}(x::($typeofx), y::T, z::T, p::Int)
      r = ccall((:acb_real_ptr, :libarb), Ptr{arb}, (($passtoc), ), x)
      _arb_set(r, y, p)
      i = ccall((:acb_imag_ptr, :libarb), Ptr{arb}, (($passtoc), ), x)
      _arb_set(i, z, p)
    end

  end

  for T in (Float64, BigFloat, UInt, fmpz)
    @eval begin
      function _acb_set(x::($typeofx), y::($T), z::($T))
        r = ccall((:acb_real_ptr, :libarb), Ptr{arb}, (($passtoc), ), x)
        _arb_set(r, y)
        i = ccall((:acb_imag_ptr, :libarb), Ptr{arb}, (($passtoc), ), x)
        _arb_set(i, z)
      end
      
      function _acb_set(x::($typeofx), y::($T), z::($T), p::Int)
        r = ccall((:acb_real_ptr, :libarb), Ptr{arb}, (($passtoc), ), x)
        _arb_set(r, y, p)
        i = ccall((:acb_imag_ptr, :libarb), Ptr{arb}, (($passtoc), ), x)
        _arb_set(i, z, p)
      end
    end
  end
end

################################################################################
#
#  Parent object overload
#
################################################################################

function call(r::AcbField)
  z = acb()
  z.parent = r
  return z
end

function call(r::AcbField, x::Union{Int, UInt, fmpz, fmpq, arb, acb, Float64, BigFloat, AbstractString})
  z = acb(x, r.prec)
  z.parent = r
  return z
end

function call{T <: Union{Int, UInt, fmpz, fmpq, arb, Float64, BigFloat, AbstractString}}(r::AcbField, x::T, y::T)
  z = acb(x, y, r.prec)
  z.parent = r
  return z
end
