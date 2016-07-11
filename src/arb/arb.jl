###############################################################################
#
#   arb.jl : Arb real numbers
#
#   Copyright (C) 2015 Tommy Hofmann
#   Copyright (C) 2015 Fredrik Johansson
#
###############################################################################

import Base: ceil

export ball, radius, midpoint, contains, contains_zero,
       contains_negative, contains_positive, contains_nonnegative,
       contains_nonpositive, iszero,
       isnonzero, isexact, isint, ispositive, isfinite,
       isnonnegative, isnegative, isnonpositive, add!, mul!,
       sub!, div!, strongequal, prec, overlaps, unique_integer,
       accuracy_bits, trim, ldexp, setunion,
       const_pi, const_e, const_log2, const_log10, const_euler,
       const_catalan, const_khinchin, const_glaisher,
       floor, ceil, hypot, sqrt, rsqrt, sqrt1pm1, root,
       log, log1p, exp, expm1, sin, cos, sinpi, cospi, tan, cot,
       tanpi, cotpi, sinh, cosh, tanh, coth, atan, asin, acos,
       atanh, asinh, acosh, gamma, lgamma, rgamma, digamma, zeta,
       sincos, sincospi, sinhcosh, atan2,
       agm, fac, binom, fib, bernoulli, risingfac, risingfac2, polylog,
       chebyshev_t, chebyshev_t2, chebyshev_u, chebyshev_u2, bell

###############################################################################
#
#   Basic manipulation
#
###############################################################################

elem_type(::ArbField) = arb

parent_type(::Type{arb}) = ArbField

base_ring(R::ArbField) = Union{}

zero(R::ArbField) = R(0)

one(R::ArbField) = R(1)

################################################################################
#
#  Parent object overloading
#
################################################################################

function call(r::ArbField)
  z = arb()
  z.parent = r
  return z
end

function call(r::ArbField, x::Int)
  z = arb(fmpz(x), r.prec)
  z.parent = r
  return z
end

function call(r::ArbField, x::UInt)
  z = arb(fmpz(x), r.prec)
  z.parent = r
  return z
end

function call(r::ArbField, x::fmpz)
  z = arb(x, r.prec)
  z.parent = r
  return z
end

function call(r::ArbField, x::fmpq)
  z = arb(x, r.prec)
  z.parent = r
  return z
end

#function call(r::ArbField, x::arf)
#  z = arb(arb(x), r.prec)
#  z.parent = r
#  return z
#end

function call(r::ArbField, x::Float64)
  z = arb(x, r.prec)
  z.parent = r
  return z
end

function call(r::ArbField, x::arb)
  z = arb(x, r.prec)
  z.parent = r
  return z
end

function call(r::ArbField, x::AbstractString)
  z = arb(x, r.prec)
  z.parent = r
  return z
end

function call(r::ArbField, x::Irrational)
  if x == pi
    return const_pi(r)
  elseif x == e
    return const_e(r.prec)
  else
    error("constant not supported")
  end
end

function call(r::ArbField, x::Union{Int, UInt, fmpz, fmpq, Float64, arb,
                                    AbstractString, BigFloat})
  z = arb(x, r.prec)
  z.parent = r
  return z
end

################################################################################
#
#  Conversions
#
################################################################################

function convert(::Type{Float64}, x::arb)
    t = arf_struct(0, 0, 0, 0)
    t.exp = x.mid_exp
    t.size = x.mid_size
    t.d1 = x.mid_d1
    t.d2 = x.mid_d2
    # rounds to nearest
    return ccall((:arf_get_d, :libarb), Float64, (Ptr{arf_struct}, Int), &t, 4)
end

################################################################################
#
#  String I/O
#
################################################################################

function show(io::IO, x::arb)
  d = ceil(parent(x).prec * 0.30102999566398119521)
  cstr = ccall((:arb_get_str, :libarb), Ptr{UInt8}, (Ptr{arb}, Int, UInt),
                                                  &x, Int(d), UInt(0))
  print(io, bytestring(cstr))
  ccall((:flint_free, :libflint), Void, (Ptr{UInt8},), cstr)
end

################################################################################
#
#  Comparison and containment
#
################################################################################

function strongequal(x::arb, y::arb)
  r = ccall((:arb_equal, :libarb), Cint, (Ptr{arb}, Ptr{arb}), &x, &y)
  return Bool(r)
end

function overlaps(x::arb, y::arb)
  r = ccall((:arb_overlaps, :libarb), Cint, (Ptr{arb}, Ptr{arb}), &x, &y)
  return Bool(r)
end

#function contains(x::arb, y::arf)
#  r = ccall((:arb_contains_arf, :libarb), Cint, (Ptr{arb}, Ptr{arf}), &x, &y)
#  return Bool(r)
#end

function contains(x::arb, y::fmpq)
  r = ccall((:arb_contains_fmpq, :libarb), Cint, (Ptr{arb}, Ptr{fmpq}), &x, &y)
  return Bool(r)
end

function contains(x::arb, y::fmpz)
  r = ccall((:arb_contains_fmpz, :libarb), Cint, (Ptr{arb}, Ptr{fmpz}), &x, &y)
  return Bool(r)
end

function contains(x::arb, y::Int)
  r = ccall((:arb_contains_si, :libarb), Cint, (Ptr{arb}, Int), &x, y)
  return Bool(r)
end

function contains(x::arb, y::BigFloat)
  r = ccall((:arb_contains_mpfr, :libarb), Cint,
              (Ptr{arb}, Ptr{BigFloat}), &x, &y)
  return Bool(r)
end

function contains(x::arb, y::arb)
  r = ccall((:arb_contains, :libarb), Cint, (Ptr{arb}, Ptr{arb}), &x, &y)
  return Bool(r)
end

for (s,f) in (("contains_zero", "arb_contains_zero"),
              ("contains_negative", "arb_contains_negative"),
              ("contains_positive", "arb_contains_positive"),
              ("contains_nonpositive", "arb_contains_nonpositive"),
              ("contains_nonnegative", "arb_contains_nonnegative"))
  @eval begin
    function($(Symbol(s)))(x::arb)
      r = ccall(($f, :libarb), Cint, (Ptr{arb}, ), &x)
      return Bool(r)
    end
  end
end

function ==(x::arb, y::arb)
    return Bool(ccall((:arb_eq, :libarb), Cint, (Ptr{arb}, Ptr{arb}), &x, &y))
end

function !=(x::arb, y::arb)
    return Bool(ccall((:arb_ne, :libarb), Cint, (Ptr{arb}, Ptr{arb}), &x, &y))
end

function >(x::arb, y::arb)
    return Bool(ccall((:arb_gt, :libarb), Cint, (Ptr{arb}, Ptr{arb}), &x, &y))
end

function >=(x::arb, y::arb)
    return Bool(ccall((:arb_ge, :libarb), Cint, (Ptr{arb}, Ptr{arb}), &x, &y))
end

function <(x::arb, y::arb)
    return Bool(ccall((:arb_lt, :libarb), Cint, (Ptr{arb}, Ptr{arb}), &x, &y))
end

function <=(x::arb, y::arb)
    return Bool(ccall((:arb_le, :libarb), Cint, (Ptr{arb}, Ptr{arb}), &x, &y))
end

==(x::arb, y::Int) = x == arb(y)
!=(x::arb, y::Int) = x != arb(y)
<=(x::arb, y::Int) = x <= arb(y)
>=(x::arb, y::Int) = x >= arb(y)
<(x::arb, y::Int) = x < arb(y)
>(x::arb, y::Int) = x > arb(y)

==(x::Int, y::arb) = arb(x) == y
!=(x::Int, y::arb) = arb(x) != y
<=(x::Int, y::arb) = arb(x) <= y
>=(x::Int, y::arb) = arb(x) >= y
<(x::Int, y::arb) = arb(x) < y
>(x::Int, y::arb) = arb(x) > y

==(x::arb, y::fmpz) = x == arb(y)
!=(x::arb, y::fmpz) = x != arb(y)
<=(x::arb, y::fmpz) = x <= arb(y)
>=(x::arb, y::fmpz) = x >= arb(y)
<(x::arb, y::fmpz) = x < arb(y)
>(x::arb, y::fmpz) = x > arb(y)

==(x::fmpz, y::arb) = arb(x) == y
!=(x::fmpz, y::arb) = arb(x) != y
<=(x::fmpz, y::arb) = arb(x) <= y
>=(x::fmpz, y::arb) = arb(x) >= y
<(x::fmpz, y::arb) = arb(x) < y
>(x::fmpz, y::arb) = arb(x) > y

==(x::arb, y::Float64) = x == arb(y)
!=(x::arb, y::Float64) = x != arb(y)
<=(x::arb, y::Float64) = x <= arb(y)
>=(x::arb, y::Float64) = x >= arb(y)
<(x::arb, y::Float64) = x < arb(y)
>(x::arb, y::Float64) = x > arb(y)

==(x::Float64, y::arb) = arb(x) == y
!=(x::Float64, y::arb) = arb(x) != y
<=(x::Float64, y::arb) = arb(x) <= y
>=(x::Float64, y::arb) = arb(x) >= y
<(x::Float64, y::arb) = arb(x) < y
>(x::Float64, y::arb) = arb(x) > y

################################################################################
#
#  Predicates
#
################################################################################

for (s,f) in (("iszero", "arb_is_zero"),
              ("isnonzero", "arb_is_nonzero"),
              ("isone", "arb_is_one"),
              ("isfinite", "arb_is_finite"),
              ("isexact", "arb_is_exact"),
              ("isint", "arb_is_int"),
              ("ispositive", "arb_is_positive"),
              ("isnonnegative", "arb_is_nonnegative"),
              ("isnegative", "arb_is_negative"),
              ("isnonpositive", "arb_is_nonpositive"))
  @eval begin
    function($(Symbol(s)))(x::arb)
      return Bool(ccall(($f, :libarb), Cint, (Ptr{arb},), &x))
    end
  end
end

################################################################################
#
#  Parts of numbers
#
################################################################################

function ball(mid::arb, rad::arb)
  z = arb(mid, rad)
  z.parent = parent(mid)
  return z
end

function radius(x::arb)
  z = parent(x)()
  ccall((:arb_get_rad_arb, :libarb), Void, (Ptr{arb}, Ptr{arb}), &z, &x)
  return z
end

function midpoint(x::arb)
  z = parent(x)()
  ccall((:arb_get_mid_arb, :libarb), Void, (Ptr{arb}, Ptr{arb}), &z, &x)
  return z
end

################################################################################
#
#  Unary operations
#
################################################################################

function -(x::arb)
  z = parent(x)()
  ccall((:arb_neg, :libarb), Void, (Ptr{arb}, Ptr{arb}), &z, &x)
  return z
end

function abs(x::arb)
  z = parent(x)()
  ccall((:arb_abs, :libarb), Void, (Ptr{arb}, Ptr{arb}), &z, &x)
  return z
end

function inv(x::arb)
  z = parent(x)()
  ccall((:arb_inv, :libarb), Void,
              (Ptr{arb}, Ptr{arb}, Int), &z, &x, parent(x).prec)
  return parent(x)(z)
end

################################################################################
#
#  Binary operations
#
################################################################################

for (s,f) in ((:+,"arb_add"), (:*,"arb_mul"), (://, "arb_div"), (:-,"arb_sub"))
  @eval begin
    function ($s)(x::arb, y::arb)
      z = parent(x)()
      ccall(($f, :libarb), Void, (Ptr{arb}, Ptr{arb}, Ptr{arb}, Int),
                           &z, &x, &y, parent(x).prec)
      return z
    end
  end
end

for (f,s) in ((:+, "add"), (:*, "mul"))
  @eval begin
    #function ($f)(x::arb, y::arf)
    #  z = parent(x)()
    #  ccall(($("arb_"*s*"_arf"), :libarb), Void,
    #              (Ptr{arb}, Ptr{arb}, Ptr{arf}, Int),
    #              &z, &x, &y, parent(x).prec)
    #  return z
    #end

    #($f)(x::arf, y::arb) = ($f)(y, x)

    function ($f)(x::arb, y::UInt)
      z = parent(x)()
      ccall(($("arb_"*s*"_ui"), :libarb), Void,
                  (Ptr{arb}, Ptr{arb}, UInt, Int),
                  &z, &x, y, parent(x).prec)
      return z
    end

    ($f)(x::UInt, y::arb) = ($f)(y, x)

    function ($f)(x::arb, y::Int)
      z = parent(x)()
      ccall(($("arb_"*s*"_si"), :libarb), Void,
      (Ptr{arb}, Ptr{arb}, Int, Int), &z, &x, y, parent(x).prec)
      return z
    end

    ($f)(x::Int, y::arb) = ($f)(y,x)

    function ($f)(x::arb, y::fmpz)
      z = parent(x)()
      ccall(($("arb_"*s*"_fmpz"), :libarb), Void,
                  (Ptr{arb}, Ptr{arb}, Ptr{fmpz}, Int),
                  &z, &x, &y, parent(x).prec)
      return z
    end

    ($f)(x::fmpz, y::arb) = ($f)(y,x)
  end
end

#function -(x::arb, y::arf)
#  z = parent(x)()
#  ccall((:arb_sub_arf, :libarb), Void,
#              (Ptr{arb}, Ptr{arb}, Ptr{arf}, Int), &z, &x, &y, parent(x).prec)
#  return z
#end

#-(x::arf, y::arb) = -(y - x)

function -(x::arb, y::UInt)
  z = parent(x)()
  ccall((:arb_sub_ui, :libarb), Void,
              (Ptr{arb}, Ptr{arb}, UInt, Int), &z, &x, y, parent(x).prec)
  return z
end

-(x::UInt, y::arb) = -(y - x)

function -(x::arb, y::Int)
  z = parent(x)()
  ccall((:arb_sub_si, :libarb), Void,
              (Ptr{arb}, Ptr{arb}, Int, Int), &z, &x, y, parent(x).prec)
  return z
end

-(x::Int, y::arb) = -(y - x)

function -(x::arb, y::fmpz)
  z = parent(x)()
  ccall((:arb_sub_fmpz, :libarb), Void,
              (Ptr{arb}, Ptr{arb}, Ptr{fmpz}, Int),
              &z, &x, &y, parent(x).prec)
  return z
end

-(x::fmpz, y::arb) = -(y-x)

#function //(x::arb, y::arf)
#  z = parent(x)()
#  ccall((:arb_div_arf, :libarb), Void,
#              (Ptr{arb}, Ptr{arb}, Ptr{arf}, Int), &z, &x, &y, parent(x).prec)
#  return z
#end

function //(x::arb, y::UInt)
  z = parent(x)()
  ccall((:arb_div_ui, :libarb), Void,
              (Ptr{arb}, Ptr{arb}, UInt, Int), &z, &x, y, parent(x).prec)
  return z
end

function //(x::arb, y::Int)
  z = parent(x)()
  ccall((:arb_div_si, :libarb), Void,
              (Ptr{arb}, Ptr{arb}, Int, Int), &z, &x, y, parent(x).prec)
  return z
end

function //(x::arb, y::fmpz)
  z = parent(x)()
  ccall((:arb_div_fmpz, :libarb), Void,
              (Ptr{arb}, Ptr{arb}, Ptr{fmpz}, Int),
              &z, &x, &y, parent(x).prec)
  return z
end

function //(x::UInt, y::arb)
  z = parent(y)()
  ccall((:arb_ui_div, :libarb), Void,
              (Ptr{arb}, UInt, Ptr{arb}, Int), &z, x, &y, parent(y).prec)
  return z
end

function //(x::Int, y::arb)
  z = parent(y)()
  t = arb(x)
  ccall((:arb_div, :libarb), Void,
              (Ptr{arb}, Ptr{arb}, Ptr{arb}, Int), &z, &t, &y, parent(y).prec)
  return z
end

function //(x::fmpz, y::arb)
  z = parent(y)()
  t = arb(x)
  ccall((:arb_div, :libarb), Void,
              (Ptr{arb}, Ptr{arb}, Ptr{arb}, Int), &z, &t, &y, parent(y).prec)
  return z
end

function ^(x::arb, y::arb)
  z = parent(x)()
  ccall((:arb_pow, :libarb), Void,
              (Ptr{arb}, Ptr{arb}, Ptr{arb}, Int), &z, &x, &y, parent(x).prec)
  return z
end

function ^(x::arb, y::fmpz)
  z = parent(x)()
  ccall((:arb_pow_fmpz, :libarb), Void,
              (Ptr{arb}, Ptr{arb}, Ptr{fmpz}, Int),
              &z, &x, &y, parent(x).prec)
  return z
end

^(x::arb, y::Int) = ^(x, fmpz(y))

function ^(x::arb, y::UInt)
  z = parent(x)()
  ccall((:arb_pow_ui, :libarb), Void,
              (Ptr{arb}, Ptr{arb}, UInt, Int), &z, &x, y, parent(x).prec)
  return z
end

function ^(x::arb, y::fmpq)
  z = parent(x)()
  ccall((:arb_pow_fmpq, :libarb), Void,
              (Ptr{arb}, Ptr{arb}, Ptr{fmpq}, Int),
              &z, &x, &y, parent(x).prec)
  return z
end

################################################################################
#
#  Precision, shifting and other operations
#
################################################################################

function ldexp(x::arb, y::Int)
  z = parent(x)()
  ccall((:arb_mul_2exp_si, :libarb), Void,
              (Ptr{arb}, Ptr{arb}, Int), &z, &x, y)
  return z
end

function ldexp(x::arb, y::fmpz)
  z = parent(x)()
  ccall((:arb_mul_2exp_fmpz, :libarb), Void,
              (Ptr{arb}, Ptr{arb}, Ptr{fmpz}), &z, &x, &y)
  return z
end

function trim(x::arb)
  z = parent(x)()
  ccall((:arb_trim, :libarb), Void, (Ptr{arb}, Ptr{arb}), &z, &x)
  return z
end

function accuracy_bits(x::arb)
  return ccall((:arb_rel_accuracy_bits, :libarb), Int, (Ptr{arb},), &x)
end

function unique_integer(x::arb)
  z = fmpz()
  unique = ccall((:arb_get_unique_fmpz, :libarb), Int,
    (Ptr{fmpz}, Ptr{arb}), &z, &x)
  return (unique != 0, z)
end

function setunion(x::arb, y::arb)
  z = parent(x)()
  ccall((:arb_union, :libarb), Void,
              (Ptr{arb}, Ptr{arb}, Ptr{arb}, Int), &z, &x, &y, parent(x).prec)
  return z
end

################################################################################
#
#  Constants
#
################################################################################

function const_pi(r::ArbField)
  z = r()
  ccall((:arb_const_pi, :libarb), Void, (Ptr{arb}, Int), &z, prec(r))
  return z
end

function const_e(r::ArbField)
  z = r()
  ccall((:arb_const_e, :libarb), Void, (Ptr{arb}, Int), &z, prec(r))
  return z
end

function const_log2(r::ArbField)
  z = r()
  ccall((:arb_const_log2, :libarb), Void, (Ptr{arb}, Int), &z, prec(r))
  return z
end

function const_log10(r::ArbField)
  z = r()
  ccall((:arb_const_log10, :libarb), Void, (Ptr{arb}, Int), &z, prec(r))
  return z
end

function const_euler(r::ArbField)
  z = r()
  ccall((:arb_const_euler, :libarb), Void, (Ptr{arb}, Int), &z, prec(r))
  return z
end

function const_catalan(r::ArbField)
  z = r()
  ccall((:arb_const_catalan, :libarb), Void, (Ptr{arb}, Int), &z, prec(r))
  return z
end

function const_khinchin(r::ArbField)
  z = r()
  ccall((:arb_const_khinchin, :libarb), Void, (Ptr{arb}, Int), &z, prec(r))
  return z
end

function const_glaisher(r::ArbField)
  z = r()
  ccall((:arb_const_glaisher, :libarb), Void, (Ptr{arb}, Int), &z, prec(r))
  return z
end

################################################################################
#
#  Real valued functions
#
################################################################################

# real - real functions
for (s,f) in (("floor", "arb_floor"),
              ("ceil", "arb_ceil"),
              ("sqrt", "arb_sqrt"),
              ("rsqrt", "arb_rsqrt"),
              ("sqrt1pm1", "arb_sqrt1pm1"),
              ("log", "arb_log"),
              ("log1p", "arb_log1p"),
              ("exp", "arb_exp"),
              ("expm1", "arb_expm1"),
              ("sin", "arb_sin"),
              ("cos", "arb_cos"),
              ("sinpi", "arb_sin_pi"),
              ("cospi", "arb_cos_pi"),
              ("tan", "arb_tan"),
              ("cot", "arb_cot"),
              ("tanpi", "arb_tan_pi"),
              ("cotpi", "arb_cot_pi"),
              ("sinh", "arb_sinh"),
              ("cosh", "arb_cosh"),
              ("tanh", "arb_tanh"),
              ("coth", "arb_coth"),
              ("atan", "arb_atan"),
              ("asin", "arb_asin"),
              ("acos", "arb_acos"),
              ("atanh", "arb_atanh"),
              ("asinh", "arb_asinh"),
              ("acosh", "arb_acosh"),
              ("gamma", "arb_gamma"),
              ("lgamma", "arb_lgamma"),
              ("rgamma", "arb_rgamma"),
              ("digamma", "arb_digamma"),
              ("zeta", "arb_zeta"),
             )
  @eval begin
    function($(Symbol(s)))(x::arb)
      z = parent(x)()
      ccall(($f, :libarb), Void, (Ptr{arb}, Ptr{arb}, Int), &z, &x, parent(x).prec)
      return z
    end
  end
end

function sincos(x::arb)
  s = parent(x)()
  c = parent(x)()
  ccall((:arb_sin_cos, :libarb), Void,
              (Ptr{arb}, Ptr{arb}, Ptr{arb}, Int), &s, &c, &x, parent(x).prec)
  return (s, c)
end

function sincospi(x::arb)
  s = parent(x)()
  c = parent(x)()
  ccall((:arb_sin_cos_pi, :libarb), Void,
              (Ptr{arb}, Ptr{arb}, Ptr{arb}, Int), &s, &c, &x, parent(x).prec)
  return (s, c)
end

function sinpi(x::fmpq, r::ArbField)
  z = r()
  ccall((:arb_sin_pi_fmpq, :libarb), Void,
        (Ptr{arb}, Ptr{fmpq}, Int), &z, &x, prec(r))
  return z
end

function cospi(x::fmpq, r::ArbField)
  z = r()
  ccall((:arb_cos_pi_fmpq, :libarb), Void,
        (Ptr{arb}, Ptr{fmpq}, Int), &z, &x, prec(r))
  return z
end

function sincospi(x::fmpq, r::ArbField)
  s = r()
  c = r()
  ccall((:arb_sin_cos_pi_fmpq, :libarb), Void,
        (Ptr{arb}, Ptr{arb}, Ptr{fmpq}, Int), &s, &c, &x, prec(r))
  return (s, c)
end

function sinhcosh(x::arb)
  s = parent(x)()
  c = parent(x)()
  ccall((:arb_sinh_cosh, :libarb), Void,
              (Ptr{arb}, Ptr{arb}, Ptr{arb}, Int), &s, &c, &x, parent(x).prec)
  return (s, c)
end

function atan2(x::arb, y::arb)
  z = parent(x)()
  ccall((:arb_atan2, :libarb), Void,
              (Ptr{arb}, Ptr{arb}, Ptr{arb}, Int), &z, &x, &y, parent(x).prec)
  return z
end

function agm(x::arb, y::arb)
  z = parent(x)()
  ccall((:arb_agm, :libarb), Void,
              (Ptr{arb}, Ptr{arb}, Ptr{arb}, Int), &z, &x, &y, parent(x).prec)
  return z
end

function zeta(s::arb, a::arb)
  z = parent(s)()
  ccall((:arb_hurwitz_zeta, :libarb), Void,
              (Ptr{arb}, Ptr{arb}, Ptr{arb}, Int), &z, &s, &a, parent(s).prec)
  return z
end

function hypot(x::arb, y::arb)
  z = parent(x)()
  ccall((:arb_hypot, :libarb), Void,
              (Ptr{arb}, Ptr{arb}, Ptr{arb}, Int), &z, &x, &y, parent(x).prec)
  return z
end

function root(x::arb, n::UInt)
  z = parent(x)()
  ccall((:arb_root, :libarb), Void,
              (Ptr{arb}, Ptr{arb}, UInt, Int), &z, &x, n, parent(x).prec)
  return z
end

root(x::arb, n::Int) = x < 0 ? throw(DomainError()) : root(x, UInt(n))

fac(x::arb) = gamma(x+1)

function fac(n::UInt, r::ArbField)
  z = r()
  ccall((:arb_fac_ui, :libarb), Void, (Ptr{arb}, UInt, Int), &z, n, r.prec)
  return z
end

fac(n::Int, r::ArbField) = n < 0 ? fac(r(n)) : fac(UInt(n), r)

function binom(x::arb, n::UInt)
  z = parent(x)()
  ccall((:arb_bin_ui, :libarb), Void,
              (Ptr{arb}, Ptr{arb}, UInt, Int), &z, &x, n, parent(x).prec)
  return z
end

function binom(n::UInt, k::UInt, r::ArbField)
  z = r()
  ccall((:arb_bin_uiui, :libarb), Void,
              (Ptr{arb}, UInt, UInt, Int), &z, n, k, r.prec)
  return z
end

function fib(n::fmpz, r::ArbField)
  z = r()
  ccall((:arb_fib_fmpz, :libarb), Void,
              (Ptr{arb}, Ptr{fmpz}, Int), &z, &n, r.prec)
  return z
end

function fib(n::UInt, r::ArbField)
  z = r()
  ccall((:arb_fib_ui, :libarb), Void,
              (Ptr{arb}, UInt, Int), &z, n, r.prec)
  return z
end

fib(n::Int, r::ArbField) = n >= 0 ? fib(UInt(n), r) : fib(fmpz(n), r)

function gamma(x::fmpz, r::ArbField)
  z = r()
  ccall((:arb_gamma_fmpz, :libarb), Void,
              (Ptr{arb}, Ptr{fmpz}, Int), &z, &x, r.prec)
  return z
end

function gamma(x::fmpq, r::ArbField)
  z = r()
  ccall((:arb_gamma_fmpq, :libarb), Void,
              (Ptr{arb}, Ptr{fmpq}, Int), &z, &x, r.prec)
  return z
end

function zeta(n::UInt, r::ArbField)
  z = r()
  ccall((:arb_zeta_ui, :libarb), Void,
              (Ptr{arb}, UInt, Int), &z, n, r.prec)
  return z
end

zeta(n::Int, r::ArbField) = n >= 0 ? zeta(UInt(n), r) : zeta(r(n))

function bernoulli(n::UInt, r::ArbField)
  z = r()
  ccall((:arb_bernoulli_ui, :libarb), Void,
              (Ptr{arb}, UInt, Int), &z, n, r.prec)
  return z
end

bernoulli(n::Int, r::ArbField) = n >= 0 ? bernoulli(UInt(n), r) : throw(DomainError)

function risingfac(x::arb, n::UInt)
  z = parent(x)()
  ccall((:arb_rising_ui, :libarb), Void,
              (Ptr{arb}, Ptr{arb}, UInt, Int), &z, &x, n, parent(x).prec)
  return z
end

risingfac(x::arb, n::Int) = n < 0 ? throw(DomainError()) : risingfac(x, UInt(n))

function risingfac(x::fmpq, n::UInt, r::ArbField)
  z = r()
  ccall((:arb_rising_fmpq_ui, :libarb), Void,
              (Ptr{arb}, Ptr{fmpq}, UInt, Int), &z, &x, n, r.prec)
  return z
end

risingfac(x::fmpq, n::Int, r::ArbField) = n < 0 ? throw(DomainError()) : risingfac(x, UInt(n), r)

function risingfac2(x::arb, n::UInt)
  z = parent(x)()
  w = parent(x)()
  ccall((:arb_rising2_ui, :libarb), Void,
              (Ptr{arb}, Ptr{arb}, Ptr{arb}, UInt, Int), &z, &w, &x, n, parent(x).prec)
  return (z, w)
end

risingfac2(x::arb, n::Int) = n < 0 ? throw(DomainError()) : risingfac2(x, UInt(n))

function polylog(s::arb, a::arb)
  z = parent(s)()
  ccall((:arb_polylog, :libarb), Void,
              (Ptr{arb}, Ptr{arb}, Ptr{arb}, Int), &z, &s, &a, parent(s).prec)
  return z
end

function polylog(s::Int, a::arb)
  z = parent(a)()
  ccall((:arb_polylog_si, :libarb), Void,
              (Ptr{arb}, Int, Ptr{arb}, Int), &z, s, &a, parent(a).prec)
  return z
end

function chebyshev_t(n::UInt, x::arb)
  z = parent(x)()
  ccall((:arb_chebyshev_t_ui, :libarb), Void,
              (Ptr{arb}, UInt, Ptr{arb}, Int), &z, n, &x, parent(x).prec)
  return z
end

function chebyshev_u(n::UInt, x::arb)
  z = parent(x)()
  ccall((:arb_chebyshev_u_ui, :libarb), Void,
              (Ptr{arb}, UInt, Ptr{arb}, Int), &z, n, &x, parent(x).prec)
  return z
end

function chebyshev_t2(n::UInt, x::arb)
  z = parent(x)()
  w = parent(x)()
  ccall((:arb_chebyshev_t2_ui, :libarb), Void,
              (Ptr{arb}, Ptr{arb}, UInt, Ptr{arb}, Int), &z, &w, n, &x, parent(x).prec)
  return z, w
end

function chebyshev_u2(n::UInt, x::arb)
  z = parent(x)()
  w = parent(x)()
  ccall((:arb_chebyshev_u2_ui, :libarb), Void,
              (Ptr{arb}, Ptr{arb}, UInt, Ptr{arb}, Int), &z, &w, n, &x, parent(x).prec)
  return z, w
end

chebyshev_t(n::Int, x::arb) = n < 0 ? throw(DomainError()) : chebyshev_t(UInt(n), x)
chebyshev_u(n::Int, x::arb) = n < 0 ? throw(DomainError()) : chebyshev_u(UInt(n), x)
chebyshev_t2(n::Int, x::arb) = n < 0 ? throw(DomainError()) : chebyshev_t2(UInt(n), x)
chebyshev_u2(n::Int, x::arb) = n < 0 ? throw(DomainError()) : chebyshev_u2(UInt(n), x)

function bell(n::fmpz, r::ArbField)
  z = r()
  ccall((:arb_bell_fmpz, :libarb), Void,
              (Ptr{arb}, Ptr{fmpz}, Int), &z, &n, r.prec)
  return z
end

bell(n::Int, r::ArbField) = bell(fmpz(n), r)

################################################################################
#
#  Unsafe binary operations
#
################################################################################

for (s,f) in (("add!","arb_add"), ("mul!","arb_mul"), ("div!", "arb_div"),
              ("sub!","arb_sub"))
  @eval begin
    function ($(Symbol(s)))(z::arb, x::arb, y::arb)
      ccall(($f, :libarb), Void, (Ptr{arb}, Ptr{arb}, Ptr{arb}, Int),
                           &z, &x, &y, parent(x).prec)
    end
  end
end

################################################################################
#
#  Unsafe setting
#
################################################################################

for (typeofx, passtoc) in ((arb, Ref{arb}), (Ptr{arb}, Ptr{arb}))
  for (f,t) in (("arb_set_si", Int), ("arb_set_ui", UInt),
                ("arb_set_d", Float64))
    @eval begin
      function _arb_set(x::($typeofx), y::($t))
        ccall(($f, :libarb), Void, (($passtoc), ($t)), x, y)
      end

      function _arb_set(x::($typeofx), y::($t), p::Int)
        _arb_set(x, y)
        ccall((:arb_set_round, :libarb), Void,
                    (($passtoc), ($passtoc), Int), x, x, p)
      end
    end
  end

  @eval begin
    function _arb_set(x::($typeofx), y::fmpz)
      ccall((:arb_set_fmpz, :libarb), Void, (($passtoc), Ptr{fmpz}), x, &y)
    end

    function _arb_set(x::($typeofx), y::fmpz, p::Int)
      ccall((:arb_set_round_fmpz, :libarb), Void,
                  (($passtoc), Ptr{fmpz}, Int), x, &y, p)
    end

    function _arb_set(x::($typeofx), y::fmpq, p::Int)
      ccall((:arb_set_fmpq, :libarb), Void,
                  (($passtoc), Ptr{fmpq}, Int), x, &y, p)
    end

    function _arb_set(x::($typeofx), y::arb)
      ccall((:arb_set, :libarb), Void, (($passtoc), Ptr{arb}, Int), x, &y)
    end

    function _arb_set(x::($typeofx), y::arb, p::Int)
      ccall((:arb_set_round, :libarb), Void,
                  (($passtoc), Ptr{arb}, Int), x, &y, p)
    end

    function _arb_set(x::($typeofx), y::AbstractString, p::Int)
      s = bytestring(y)
      err = ccall((:arb_set_str, :libarb), Int32,
                  (($passtoc), Ptr{UInt8}, Int), x, s, p)
      err == 0 || error("Invalid real string: $(repr(s))")
    end

    function _arb_set(x::($typeofx), y::BigFloat)
      m = ccall((:arb_mid_ptr, :libarb), Ptr{arf_struct},
                  (($passtoc), ), x)
      r = ccall((:arb_rad_ptr, :libarb), Ptr{mag_struct},
                  (($passtoc), ), x)
      ccall((:arf_set_mpfr, :libarb), Void,
                  (Ptr{arf_struct}, Ptr{BigFloat}), m, &y)
      ccall((:mag_zero, :libarb), Void, (Ptr{mag_struct}, ), r)
    end

    function _arb_set(x::($typeofx), y::BigFloat, p::Int)
      m = ccall((:arb_mid_ptr, :libarb), Ptr{arf_struct}, (($passtoc), ), x)
      r = ccall((:arb_rad_ptr, :libarb), Ptr{mag_struct}, (($passtoc), ), x)
      ccall((:arf_set_mpfr, :libarb), Void,
                  (Ptr{arf_struct}, Ptr{BigFloat}), m, &y)
      ccall((:mag_zero, :libarb), Void, (Ptr{mag_struct}, ), r)
      ccall((:arb_set_round, :libarb), Void,
                  (($passtoc), ($passtoc), Int), x, x, p)
    end
  end
end

