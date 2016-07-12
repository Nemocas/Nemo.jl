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

doc"""
    base_ring(R::ArbField)
> Returns `Union{}` since an Arb field does not depend on any other ring.
"""
base_ring(R::ArbField) = Union{} 

doc"""
    base_ring(x::arb)
> Returns `Union{}` since an Arb field does not depend on any other ring.
"""
base_ring(x::arb) = Union{}

doc"""
    zero(R::ArbField)
> Return exact zero in the given Arb field. 
"""
zero(R::ArbField) = R(0)

doc"""
    one(R::ArbField)
> Return exact one in the given Arb field. 
"""
one(R::ArbField) = R(1)

# TODO: Add deepcopy and hash (and document under arb basic functionality)

doc"""
    accuracy_bits(x::arb)
> Return the relative accuracy of $x$ measured in bits, capped between
> `typemax(Int)` and `-typemax(Int)`.
"""
function accuracy_bits(x::arb)
  return ccall((:arb_rel_accuracy_bits, :libarb), Int, (Ptr{arb},), &x)
end

################################################################################
#
#  Conversions
#
################################################################################

doc"""
    convert(::Type{Float64}, x::arb)
> Return the midpoint of $x$ rounded down to a machine double.
"""
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
#  Containment
#
################################################################################

doc"""
    overlaps(x::arb, y::arb)
> Returns `true` if any part of the ball $x$ overlaps any part of the ball $y$,
> otherwise return `false`.
"""
function overlaps(x::arb, y::arb)
  r = ccall((:arb_overlaps, :libarb), Cint, (Ptr{arb}, Ptr{arb}), &x, &y)
  return Bool(r)
end

#function contains(x::arb, y::arf)
#  r = ccall((:arb_contains_arf, :libarb), Cint, (Ptr{arb}, Ptr{arf}), &x, &y)
#  return Bool(r)
#end

doc"""
    contains(x::arb, y::fmpq)
> Returns `true` if the ball $x$ contains the given rational value, otherwise
> return `false`.
"""
function contains(x::arb, y::fmpq)
  r = ccall((:arb_contains_fmpq, :libarb), Cint, (Ptr{arb}, Ptr{fmpq}), &x, &y)
  return Bool(r)
end

doc"""
    contains(x::arb, y::fmpz)
> Returns `true` if the ball $x$ contains the given integer value, otherwise
> return `false`.
"""
function contains(x::arb, y::fmpz)
  r = ccall((:arb_contains_fmpz, :libarb), Cint, (Ptr{arb}, Ptr{fmpz}), &x, &y)
  return Bool(r)
end

function contains(x::arb, y::Int)
  r = ccall((:arb_contains_si, :libarb), Cint, (Ptr{arb}, Int), &x, y)
  return Bool(r)
end

doc"""
    contains(x::arb, y::Integer)
> Returns `true` if the ball $x$ contains the given integer value, otherwise
> return `false`.
"""
contains(x::arb, y::Integer) = contains(x, fmpz(y))

doc"""
    contains(x::arb, y::BigFloat)
> Returns `true` if the ball $x$ contains the given floating point value, 
> otherwise return `false`.
"""
function contains(x::arb, y::BigFloat)
  r = ccall((:arb_contains_mpfr, :libarb), Cint,
              (Ptr{arb}, Ptr{BigFloat}), &x, &y)
  return Bool(r)
end

doc"""
    contains(x::arb, y::arb)
> Returns `true` if the ball $x$ contains the ball $y$, otherwise return
> `false`.
"""
function contains(x::arb, y::arb)
  r = ccall((:arb_contains, :libarb), Cint, (Ptr{arb}, Ptr{arb}), &x, &y)
  return Bool(r)
end

doc"""
    contains_zero(x::arb)
> Returns `true` if the ball $x$ contains zero, otherwise return `false`.
"""
function contains_zero(x::arb)
   r = ccall((:arb_contains_zero, :libarb), Cint, (Ptr{arb}, ), &x)
   return Bool(r)
end

doc"""
    contains_negative(x::arb)
> Returns `true` if the ball $x$ contains any negative value, otherwise return
> `false`.
"""
function contains_negative(x::arb)
   r = ccall((:arb_contains_negative, :libarb), Cint, (Ptr{arb}, ), &x)
   return Bool(r)
end

doc"""
    contains_positive(x::arb)
> Returns `true` if the ball $x$ contains any positive value, otherwise return
> `false`.
"""
function contains_positive(x::arb)
   r = ccall((:arb_contains_positive, :libarb), Cint, (Ptr{arb}, ), &x)
   return Bool(r)
end

doc"""
    contains_nonnegative(x::arb)
> Returns `true` if the ball $x$ contains any nonnegative value, otherwise
> return `false`.
"""
function contains_nonnegative(x::arb)
   r = ccall((:arb_contains_nonnegative, :libarb), Cint, (Ptr{arb}, ), &x)
   return Bool(r)
end

doc"""
    contains_nonpositive(x::arb)
> Returns `true` if the ball $x$ contains any nonpositive value, otherwise
> return `false`.
"""
function contains_nonpositive(x::arb)
   r = ccall((:arb_contains_nonpositive, :libarb), Cint, (Ptr{arb}, ), &x)
   return Bool(r)
end

################################################################################
#
#  Comparison
#
################################################################################

doc"""
    isequal(x::arb, y::arb)
> Return `true` if the balls $x$ and $y$ are precisely equal, i.e. have the
> same midpoints and radii.
"""
function isequal(x::arb, y::arb)
  r = ccall((:arb_equal, :libarb), Cint, (Ptr{arb}, Ptr{arb}), &x, &y)
  return Bool(r)
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

==(x::arb, y::Integer) = x == fmpz(y)
!=(x::arb, y::Integer) = x != fmpz(y)
<=(x::arb, y::Integer) = x <= fmpz(y)
>=(x::arb, y::Integer) = x >= fmpz(y)
<(x::arb, y::Integer) = x < fmpz(y)
>(x::arb, y::Integer) = x > fmpz(y)

==(x::fmpz, y::arb) = arb(x) == y
!=(x::fmpz, y::arb) = arb(x) != y
<=(x::fmpz, y::arb) = arb(x) <= y
>=(x::fmpz, y::arb) = arb(x) >= y
<(x::fmpz, y::arb) = arb(x) < y
>(x::fmpz, y::arb) = arb(x) > y

==(x::Integer, y::arb) = fmpz(x) == y
!=(x::Integer, y::arb) = fmpz(x) != y
<=(x::Integer, y::arb) = fmpz(x) <= y
>=(x::Integer, y::arb) = fmpz(x) >= y
<(x::Integer, y::arb) = fmpz(x) < y
>(x::Integer, y::arb) = fmpz(x) > y

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

doc"""
    iszero(x::arb)
> Return `true` if $x$ is certainly zero, otherwise return `false`.
"""
function iszero(x::arb)
   return Bool(ccall((:arb_is_zero, :libarb), Cint, (Ptr{arb},), &x))
end

doc"""
    isnonzero(x::arb)
> Return `true` if $x$ is certainly not equal to zero, otherwise return
> `false`.
"""
function isnonzero(x::arb)
   return Bool(ccall((:arb_is_nonzero, :libarb), Cint, (Ptr{arb},), &x))
end

doc"""
    isone(x::arb)
> Return `true` if $x$ is certainly not equal to oneo, otherwise return
> `false`.
"""
function isone(x::arb)
   return Bool(ccall((:arb_is_one, :libarb), Cint, (Ptr{arb},), &x))
end

doc"""
    isfinite(x::arb)
> Return `true` if $x$ is finite, i.e. having finite midpoint and radius,
> otherwise return `false`.
"""
function isfinite(x::arb)
   return Bool(ccall((:arb_is_finite, :libarb), Cint, (Ptr{arb},), &x))
end

doc"""
    isexact(x::arb)
> Return `true` if $x$ is exact, i.e. has zero radius, otherwise return
> `false`.
"""
function isexact(x::arb)
   return Bool(ccall((:arb_is_exact, :libarb), Cint, (Ptr{arb},), &x))
end

doc"""
    isint(x::arb)
> Return `true` if $x$ is an exact integer, otherwise return `false`.
"""
function isint(x::arb)
   return Bool(ccall((:arb_is_int, :libarb), Cint, (Ptr{arb},), &x))
end

doc"""
    ispositive(x::arb)
> Return `true` if $x$ is certainly positive, otherwise return `false`.
"""
function ispositive(x::arb)
   return Bool(ccall((:arb_is_positive, :libarb), Cint, (Ptr{arb},), &x))
end

doc"""
    isnonnegative(x::arb)
> Return `true` if $x$ is certainly nonnegative, otherwise return `false`.
"""
function isnonnegative(x::arb)
   return Bool(ccall((:arb_is_nonnegative, :libarb), Cint, (Ptr{arb},), &x))
end

doc"""
    isnegative(x::arb)
> Return `true` if $x$ is certainly negative, otherwise return `false`.
"""
function isnegative(x::arb)
   return Bool(ccall((:arb_is_negative, :libarb), Cint, (Ptr{arb},), &x))
end

doc"""
    isnonpositive(x::arb)    
> Return `true` if $x$ is certainly nonpositive, otherwise return `false`.
"""
function isnonpositive(x::arb)
   return Bool(ccall((:arb_is_nonpositive, :libarb), Cint, (Ptr{arb},), &x))
end

################################################################################
#
#  Parts of numbers
#
################################################################################

doc"""
    ball(mid::arb, rad::arb)
> Constructs an `arb` enclosing the range $[m-|r|, m+|r|]$, given the pair
> $(m, r)$.
"""
function ball(mid::arb, rad::arb)
  z = arb(mid, rad)
  z.parent = parent(mid)
  return z
end

doc"""
    radius(x::arb)
> Return the radius of the ball $x$ as an Arb ball.
"""
function radius(x::arb)
  z = parent(x)()
  ccall((:arb_get_rad_arb, :libarb), Void, (Ptr{arb}, Ptr{arb}), &z, &x)
  return z
end

doc"""
    midpoint(x::arb)
> Return the midpoint of the ball $x$ as an Arb ball.
"""
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

doc"""
    abs(x::arb)
> Return the absolute value of $x$.
"""
function abs(x::arb)
  z = parent(x)()
  ccall((:arb_abs, :libarb), Void, (Ptr{arb}, Ptr{arb}), &z, &x)
  return z
end

doc"""
    inv(x::arb)
> Return the multiplicative inverse of $x$, i.e. $1/x$.
"""
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

+(x::arb, y::Integer) = x + fmpz(y)

-(x::arb, y::Integer) = x - fmpz(y)

*(x::arb, y::Integer) = x*fmpz(y)

//(x::arb, y::Integer) = x//fmpz(y)

+(x::Integer, y::arb) = fmpz(x) + y

-(x::Integer, y::arb) = fmpz(x) - y

*(x::Integer, y::arb) = fmpz(x)*y

//(x::Integer, y::arb) = fmpz(x)//y

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

^(x::arb, y::Integer) = x^fmpz(y)

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

doc"""
    ldexp(x::arb, y::Int)
> Return $2^yx$. Note that $y$ can be positive, zero or negative.
"""
function ldexp(x::arb, y::Int)
  z = parent(x)()
  ccall((:arb_mul_2exp_si, :libarb), Void,
              (Ptr{arb}, Ptr{arb}, Int), &z, &x, y)
  return z
end

doc"""
    ldexp(x::arb, y::fmpz)
> Return $2^yx$. Note that $y$ can be positive, zero or negative.
"""
function ldexp(x::arb, y::fmpz)
  z = parent(x)()
  ccall((:arb_mul_2exp_fmpz, :libarb), Void,
              (Ptr{arb}, Ptr{arb}, Ptr{fmpz}), &z, &x, &y)
  return z
end

doc"""
    trim(x::arb)
> Return an `arb` interval containing $x$ but which may be more economical,
> by rounding off insignificant bits from the midpoint.
"""
function trim(x::arb)
  z = parent(x)()
  ccall((:arb_trim, :libarb), Void, (Ptr{arb}, Ptr{arb}), &z, &x)
  return z
end

doc"""
    unique_integer(x::arb)
> Return a pair where the first value is a boolean and the second is an `fmpz`
> integer. The boolean indicates whether the interval $x$ contains a unique
> integer. If this is the case, the second return value is set to this unique
> integer.
"""
function unique_integer(x::arb)
  z = fmpz()
  unique = ccall((:arb_get_unique_fmpz, :libarb), Int,
    (Ptr{fmpz}, Ptr{arb}), &z, &x)
  return (unique != 0, z)
end

doc"""
    setunion(x::arb, y::arb)
> Return an `arb` containing the union of the intervals represented by $x$ and
> $y$.
"""
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

doc"""
    const_pi(r::ArbField)
> Return $\pi = 3.14159\ldots$ as an element of $r$.
"""
function const_pi(r::ArbField)
  z = r()
  ccall((:arb_const_pi, :libarb), Void, (Ptr{arb}, Int), &z, prec(r))
  return z
end

doc"""
    const_e(r::ArbField)
> Return $e = 2.71828\ldots$ as an element of $r$.
"""
function const_e(r::ArbField)
  z = r()
  ccall((:arb_const_e, :libarb), Void, (Ptr{arb}, Int), &z, prec(r))
  return z
end

doc"""
    const_log2(r::ArbField)
> Return $\log(2) = 0.69314\ldots$ as an element of $r$.
"""
function const_log2(r::ArbField)
  z = r()
  ccall((:arb_const_log2, :libarb), Void, (Ptr{arb}, Int), &z, prec(r))
  return z
end

doc"""
    const_log10(r::ArbField)
> Return $\log(10) = 2.302585\ldots$ as an element of $r$.
"""
function const_log10(r::ArbField)
  z = r()
  ccall((:arb_const_log10, :libarb), Void, (Ptr{arb}, Int), &z, prec(r))
  return z
end

doc"""
    const_euler(r::ArbField)
> Return Euler's constant $\gamma = 0.577215\ldots$ as an element of $r$.
"""
function const_euler(r::ArbField)
  z = r()
  ccall((:arb_const_euler, :libarb), Void, (Ptr{arb}, Int), &z, prec(r))
  return z
end

doc"""
    const_catalan(r::ArbField)
> Return Catalan's constant $C = 0.915965\ldots$ as an element of $r$.
"""
function const_catalan(r::ArbField)
  z = r()
  ccall((:arb_const_catalan, :libarb), Void, (Ptr{arb}, Int), &z, prec(r))
  return z
end

doc"""
    const_khinchin(r::ArbField)
> Return Khinchin's constant $K = 2.685452\ldots$ as an element of $r$.
"""
function const_khinchin(r::ArbField)
  z = r()
  ccall((:arb_const_khinchin, :libarb), Void, (Ptr{arb}, Int), &z, prec(r))
  return z
end

doc"""
    const_glaisher(r::ArbField)
> Return Glaisher's constant $A = 1.282427\ldots$ as an element of $r$.
"""
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

doc"""
    floor(x::arb)
> Compute the floor of $x$, i.e. the greatest integer not exceeding $x$, as an
> Arb.
"""
function floor(x::arb)
   z = parent(x)()
   ccall((:arb_floor, :libarb), Void, (Ptr{arb}, Ptr{arb}, Int), &z, &x, parent(x).prec)
   return z
end

doc"""
    ceil(x::arb)
> Return the ceiling of $x$, i.e. the least integer not less than $x$, as an
> Arb.
"""
function ceil(x::arb)
   z = parent(x)()
   ccall((:arb_ceil, :libarb), Void, (Ptr{arb}, Ptr{arb}, Int), &z, &x, parent(x).prec)
   return z
end

doc"""
    sqrt(x::arb)
> Return the square root of $x$.
"""
function sqrt(x::arb)
   z = parent(x)()
   ccall((:arb_sqrt, :libarb), Void, (Ptr{arb}, Ptr{arb}, Int), &z, &x, parent(x).prec)
   return z
end

doc"""
    rsqrt(x::arb)
> Return the inverse of the square root of $x$, i.e. $1/\sqrt{x}$.
"""
function rsqrt(x::arb)
   z = parent(x)()
   ccall((:arb_rsqrt, :libarb), Void, (Ptr{arb}, Ptr{arb}, Int), &z, &x, parent(x).prec)
   return z
end

doc"""
    sqrt1pm1(x::arb)
> Return $\sqrt{1+x}-1$, evaluated accurately for small $x$.
"""
function sqrt1pm1(x::arb)
   z = parent(x)()
   ccall((:arb_sqrt1pm1, :libarb), Void, (Ptr{arb}, Ptr{arb}, Int), &z, &x, parent(x).prec)
   return z
end

doc"""
    log(x::arb)
> Return the principal branch of the logarithm of $x$.
"""
function log(x::arb)
   z = parent(x)()
   ccall((:arb_log, :libarb), Void, (Ptr{arb}, Ptr{arb}, Int), &z, &x, parent(x).prec)
   return z
end

doc"""
    log1p(x::arb)
> Return $\log(1+x)$, evaluated accurately for small $x$.
"""
function log1p(x::arb)
   z = parent(x)()
   ccall((:arb_log1p, :libarb), Void, (Ptr{arb}, Ptr{arb}, Int), &z, &x, parent(x).prec)
   return z
end

doc"""
    exp(x::arb)
> Return the exponential of $x$.
"""
function exp(x::arb)
   z = parent(x)()
   ccall((:arb_exp, :libarb), Void, (Ptr{arb}, Ptr{arb}, Int), &z, &x, parent(x).prec)
   return z
end

doc"""
    expm1(x::arb)
> Return $\exp(x)-1$, evaluated accurately for small $x$.
"""
function expm1(x::arb)
   z = parent(x)()
   ccall((:arb_expm1, :libarb), Void, (Ptr{arb}, Ptr{arb}, Int), &z, &x, parent(x).prec)
   return z
end

doc"""
    sin(x::arb)
> Return the sine of $x$.
"""
function sin(x::arb)
   z = parent(x)()
   ccall((:arb_sin, :libarb), Void, (Ptr{arb}, Ptr{arb}, Int), &z, &x, parent(x).prec)
   return z
end

doc"""
    cos(x::arb)
> Return the cosine of $x$.
"""
function cos(x::arb)
   z = parent(x)()
   ccall((:arb_cos, :libarb), Void, (Ptr{arb}, Ptr{arb}, Int), &z, &x, parent(x).prec)
   return z
end

doc"""
    sinpi(x::arb)
> Return the sine of $\pi x$.
"""
function sinpi(x::arb)
   z = parent(x)()
   ccall((:arb_sin_pi, :libarb), Void, (Ptr{arb}, Ptr{arb}, Int), &z, &x, parent(x).prec)
   return z
end

doc"""
    cospi(x::arb)
> Return the cosine of $\pi x$.
"""
function cospi(x::arb)
   z = parent(x)()
   ccall((:arb_cos_pi, :libarb), Void, (Ptr{arb}, Ptr{arb}, Int), &z, &x, parent(x).prec)
   return z
end

doc"""
    tan(x::arb)
> Return the tangent of $x$.
"""
function tan(x::arb)
   z = parent(x)()
   ccall((:arb_tan, :libarb), Void, (Ptr{arb}, Ptr{arb}, Int), &z, &x, parent(x).prec)
   return z
end

doc"""
    cot(x::arb)
> Return the cotangent of $x$.
"""
function cot(x::arb)
   z = parent(x)()
   ccall((:arb_cot, :libarb), Void, (Ptr{arb}, Ptr{arb}, Int), &z, &x, parent(x).prec)
   return z
end

doc"""
    tanpi(x::arb)
> Return the tangent of $\pi x$.
"""
function tanpi(x::arb)
   z = parent(x)()
   ccall((:arb_tan_pi, :libarb), Void, (Ptr{arb}, Ptr{arb}, Int), &z, &x, parent(x).prec)
   return z
end

doc"""
    cotpi(x::arb)
> Return the cotangent of $\pi x$.
"""
function cotpi(x::arb)
   z = parent(x)()
   ccall((:arb_cot_pi, :libarb), Void, (Ptr{arb}, Ptr{arb}, Int), &z, &x, parent(x).prec)
   return z
end

doc"""
    sinh(x::arb)
> Return the hyperbolic sine of $x$.
"""
function sinh(x::arb)
   z = parent(x)()
   ccall((:arb_sinh, :libarb), Void, (Ptr{arb}, Ptr{arb}, Int), &z, &x, parent(x).prec)
   return z
end

doc"""
    cosh(x::arb)
> Return the hyperbolic cosine of $x$.
"""
function cosh(x::arb)
   z = parent(x)()
   ccall((:arb_cosh, :libarb), Void, (Ptr{arb}, Ptr{arb}, Int), &z, &x, parent(x).prec)
   return z
end

doc"""
    tanh(x::arb)
> Return the hyperbolic tangent of $x$.
"""
function tanh(x::arb)
   z = parent(x)()
   ccall((:arb_tanh, :libarb), Void, (Ptr{arb}, Ptr{arb}, Int), &z, &x, parent(x).prec)
   return z
end

doc"""
    coth(x::arb)
> Return the hyperbolic cotangent of $x$.
"""
function coth(x::arb)
   z = parent(x)()
   ccall((:arb_coth, :libarb), Void, (Ptr{arb}, Ptr{arb}, Int), &z, &x, parent(x).prec)
   return z
end

doc"""
    atan(x::arb)
> Return the arctangent of $x$.
"""
function atan(x::arb)
   z = parent(x)()
   ccall((:arb_atan, :libarb), Void, (Ptr{arb}, Ptr{arb}, Int), &z, &x, parent(x).prec)
   return z
end

doc"""
    asin(x::arb)
> Return the arcsine of $x$.
"""
function asin(x::arb)
   z = parent(x)()
   ccall((:arb_asin, :libarb), Void, (Ptr{arb}, Ptr{arb}, Int), &z, &x, parent(x).prec)
   return z
end

doc"""
    acos(x::arb)
> Return the arccosine of $x$.
"""
function acos(x::arb)
   z = parent(x)()
   ccall((:arb_acos, :libarb), Void, (Ptr{arb}, Ptr{arb}, Int), &z, &x, parent(x).prec)
   return z
end

doc"""
    atanh(x::arb)
> Return the hyperbolic arctangent of $x$.
"""
function atanh(x::arb)
   z = parent(x)()
   ccall((:arb_atanh, :libarb), Void, (Ptr{arb}, Ptr{arb}, Int), &z, &x, parent(x).prec)
   return z
end

doc"""
    asinh(x::arb)
> Return the hyperbolic arcsine of $x$.
"""
function asinh(x::arb)
   z = parent(x)()
   ccall((:arb_asinh, :libarb), Void, (Ptr{arb}, Ptr{arb}, Int), &z, &x, parent(x).prec)
   return z
end

doc"""
    acosh(x::arb)
> Return the hyperbolic arccosine of $x$.
"""
function acosh(x::arb)
   z = parent(x)()
   ccall((:arb_acosh, :libarb), Void, (Ptr{arb}, Ptr{arb}, Int), &z, &x, parent(x).prec)
   return z
end

doc"""
    gamma(x::arb)
> Return the Gamma function evaluated at $x$.
"""
function gamma(x::arb)
   z = parent(x)()
   ccall((:arb_gamma, :libarb), Void, (Ptr{arb}, Ptr{arb}, Int), &z, &x, parent(x).prec)
   return z
end

doc"""
    lgamma(x::arb)
> Return the logarithm of the Gamma function evaluated at $x$.
"""
function lgamma(x::arb)
   z = parent(x)()
   ccall((:arb_lgamma, :libarb), Void, (Ptr{arb}, Ptr{arb}, Int), &z, &x, parent(x).prec)
   return z
end

doc"""
    rgamma(x::arb)
> Return the reciprocal of the Gamma function evaluated at $x$.
"""
function rgamma(x::arb)
   z = parent(x)()
   ccall((:arb_rgamma, :libarb), Void, (Ptr{arb}, Ptr{arb}, Int), &z, &x, parent(x).prec)
   return z
end

doc"""
    digamma(x::arb)
> Return the  logarithmic derivative of the gamma function evaluated at $x$,
> i.e. $\psi(x)$.
"""
function digamma(x::arb)
   z = parent(x)()
   ccall((:arb_digamma, :libarb), Void, (Ptr{arb}, Ptr{arb}, Int), &z, &x, parent(x).prec)
   return z
end

doc"""
    zeta(x::arb)
> Return the Riemann zeta function evaluated at $x$.
"""
function zeta(x::arb)
   z = parent(x)()
   ccall((:arb_zeta, :libarb), Void, (Ptr{arb}, Ptr{arb}, Int), &z, &x, parent(x).prec)
   return z
end

doc"""
    sincos(x::arb)
> Return a tuple $s, c$ consisting of the sine $s$ and cosine $c$ of $x$.
"""
function sincos(x::arb)
  s = parent(x)()
  c = parent(x)()
  ccall((:arb_sin_cos, :libarb), Void,
              (Ptr{arb}, Ptr{arb}, Ptr{arb}, Int), &s, &c, &x, parent(x).prec)
  return (s, c)
end

doc"""
    sincospi(x::arb)
> Return a tuple $s, c$ consisting of the sine $s$ and cosine $c$ of $\pi x$.
"""
function sincospi(x::arb)
  s = parent(x)()
  c = parent(x)()
  ccall((:arb_sin_cos_pi, :libarb), Void,
              (Ptr{arb}, Ptr{arb}, Ptr{arb}, Int), &s, &c, &x, parent(x).prec)
  return (s, c)
end

doc"""
    sinpi(x::fmpq, r::ArbField)
> Return the sine of $\pi x$ in the given Arb field.
"""
function sinpi(x::fmpq, r::ArbField)
  z = r()
  ccall((:arb_sin_pi_fmpq, :libarb), Void,
        (Ptr{arb}, Ptr{fmpq}, Int), &z, &x, prec(r))
  return z
end

doc"""
    cospi(x::fmpq, r::ArbField)
>  Return the cosine of $\pi x$ in the given Arb field.
"""
function cospi(x::fmpq, r::ArbField)
  z = r()
  ccall((:arb_cos_pi_fmpq, :libarb), Void,
        (Ptr{arb}, Ptr{fmpq}, Int), &z, &x, prec(r))
  return z
end

doc"""
    sincospi(x::fmpq, r::ArbField)
> Return a tuple $s, c$ consisting of the sine and cosine of $\pi x$ in the
> given Arb field.
"""
function sincospi(x::fmpq, r::ArbField)
  s = r()
  c = r()
  ccall((:arb_sin_cos_pi_fmpq, :libarb), Void,
        (Ptr{arb}, Ptr{arb}, Ptr{fmpq}, Int), &s, &c, &x, prec(r))
  return (s, c)
end

doc"""
    sinhcosh(x::arb)
> Return a tuple $s, c$ consisting of the hyperbolic sine and cosine of $x$.
"""
function sinhcosh(x::arb)
  s = parent(x)()
  c = parent(x)()
  ccall((:arb_sinh_cosh, :libarb), Void,
              (Ptr{arb}, Ptr{arb}, Ptr{arb}, Int), &s, &c, &x, parent(x).prec)
  return (s, c)
end

doc"""
    atan2(x::arb, y::arb)
> Return atan2$(b,a) = \arg(a+bi)$.
"""
function atan2(x::arb, y::arb)
  z = parent(x)()
  ccall((:arb_atan2, :libarb), Void,
              (Ptr{arb}, Ptr{arb}, Ptr{arb}, Int), &z, &x, &y, parent(x).prec)
  return z
end

doc"""
    agm(x::arb, y::arb)
> Return the arithmetic-geometric mean of $x$ and $y$
"""
function agm(x::arb, y::arb)
  z = parent(x)()
  ccall((:arb_agm, :libarb), Void,
              (Ptr{arb}, Ptr{arb}, Ptr{arb}, Int), &z, &x, &y, parent(x).prec)
  return z
end

doc"""
    zeta(s::arb, a::arb)
> Return the Hurwitz zeta function $\zeta(s,a)$..
"""
function zeta(s::arb, a::arb)
  z = parent(s)()
  ccall((:arb_hurwitz_zeta, :libarb), Void,
              (Ptr{arb}, Ptr{arb}, Ptr{arb}, Int), &z, &s, &a, parent(s).prec)
  return z
end

doc"""
    hypot(x::arb, y::arb)
> Return $\sqrt{x^2 + y^2}$.
"""
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

doc"""
    root(x::arb, n::Int)
> Return the $n$-th root of $x$. We require $x \geq 0$.
"""
root(x::arb, n::Int) = x < 0 ? throw(DomainError()) : root(x, UInt(n))

doc"""
    fac(x::arb)
> Return the factorial of $x$.
"""
fac(x::arb) = gamma(x+1)

function fac(n::UInt, r::ArbField)
  z = r()
  ccall((:arb_fac_ui, :libarb), Void, (Ptr{arb}, UInt, Int), &z, n, r.prec)
  return z
end

doc"""
    fac(n::Int, r::ArbField)
> Return the factorial of $n$ in the given Arb field.
"""
fac(n::Int, r::ArbField) = n < 0 ? fac(r(n)) : fac(UInt(n), r)

doc"""
    binom(x::arb, n::UInt)
> Return the binomial coefficient ${x \choose n}$.
"""
function binom(x::arb, n::UInt)
  z = parent(x)()
  ccall((:arb_bin_ui, :libarb), Void,
              (Ptr{arb}, Ptr{arb}, UInt, Int), &z, &x, n, parent(x).prec)
  return z
end

doc"""
    binom(n::UInt, k::UInt, r::ArbField)
> Return the binomial coefficient ${n \choose k}$ in the given Arb field.
"""
function binom(n::UInt, k::UInt, r::ArbField)
  z = r()
  ccall((:arb_bin_uiui, :libarb), Void,
              (Ptr{arb}, UInt, UInt, Int), &z, n, k, r.prec)
  return z
end

doc"""
    fib(n::fmpz, r::ArbField)
> Return the $n$-th Fibonacci number in the given Arb field.
"""
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

doc"""
    fib(n::Int, r::ArbField)
> Return the $n$-th Fibonacci number in the given Arb field.
"""
fib(n::Int, r::ArbField) = n >= 0 ? fib(UInt(n), r) : fib(fmpz(n), r)

doc"""
    gamma(x::fmpz, r::ArbField)
> Return the Gamma function evaluated at $x$ in the given Arb field.
"""
function gamma(x::fmpz, r::ArbField)
  z = r()
  ccall((:arb_gamma_fmpz, :libarb), Void,
              (Ptr{arb}, Ptr{fmpz}, Int), &z, &x, r.prec)
  return z
end

doc"""
    gamma(x::fmpq, r::ArbField)
> Return the Gamma function evaluated at $x$ in the given Arb field.
"""
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

doc"""
    zeta(n::Int, r::ArbField)
> Return the Riemann zeta function $\zeta(n)$ as an element of the given Arb
> field.
"""
zeta(n::Int, r::ArbField) = n >= 0 ? zeta(UInt(n), r) : zeta(r(n))

function bernoulli(n::UInt, r::ArbField)
  z = r()
  ccall((:arb_bernoulli_ui, :libarb), Void,
              (Ptr{arb}, UInt, Int), &z, n, r.prec)
  return z
end

doc"""
    bernoulli(n::Int, r::ArbField)
> Return the $n$-th Bernoulli number as an element of the given Arb field.
"""
bernoulli(n::Int, r::ArbField) = n >= 0 ? bernoulli(UInt(n), r) : throw(DomainError)

function risingfac(x::arb, n::UInt)
  z = parent(x)()
  ccall((:arb_rising_ui, :libarb), Void,
              (Ptr{arb}, Ptr{arb}, UInt, Int), &z, &x, n, parent(x).prec)
  return z
end

doc"""
    risingfac(x::arb, n::Int)
> Return the rising factorial $x(x + 1)\ldots (x + n - 1)$ as an Arb.
"""
risingfac(x::arb, n::Int) = n < 0 ? throw(DomainError()) : risingfac(x, UInt(n))

function risingfac(x::fmpq, n::UInt, r::ArbField)
  z = r()
  ccall((:arb_rising_fmpq_ui, :libarb), Void,
              (Ptr{arb}, Ptr{fmpq}, UInt, Int), &z, &x, n, r.prec)
  return z
end

doc"""
    risingfac(x::fmpq, n::Int, r::ArbField)
> Return the rising factorial $x(x + 1)\ldots (x + n - 1)$ as an element of the
> given Arb field.
"""
risingfac(x::fmpq, n::Int, r::ArbField) = n < 0 ? throw(DomainError()) : risingfac(x, UInt(n), r)

function risingfac2(x::arb, n::UInt)
  z = parent(x)()
  w = parent(x)()
  ccall((:arb_rising2_ui, :libarb), Void,
              (Ptr{arb}, Ptr{arb}, Ptr{arb}, UInt, Int), &z, &w, &x, n, parent(x).prec)
  return (z, w)
end

doc"""
    risingfac2(x::arb, n::Int)
> Return a tuple containing the rising factorial $x(x + 1)\ldots (x + n - 1)$
> and its derivative.
"""
risingfac2(x::arb, n::Int) = n < 0 ? throw(DomainError()) : risingfac2(x, UInt(n))

doc"""
    polylog(s::arb, a::arb)
> Return the polylogarithm Li$_s(a)$.
"""
function polylog(s::arb, a::arb)
  z = parent(s)()
  ccall((:arb_polylog, :libarb), Void,
              (Ptr{arb}, Ptr{arb}, Ptr{arb}, Int), &z, &s, &a, parent(s).prec)
  return z
end

doc"""
    polylog(s::Int, a::arb)
> Return the polylogarithm Li$_s(a)$.
"""
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

doc"""
    chebyshev_t(n::Int, x::arb)
> Return the value of the Chebyshev polynomial $T_n(x)$.
"""
chebyshev_t(n::Int, x::arb) = n < 0 ? throw(DomainError()) : chebyshev_t(UInt(n), x)

doc"""
    chebyshev_u(n::Int, x::arb)
> Return the value of the Chebyshev polynomial $U_n(x)$.
"""
chebyshev_u(n::Int, x::arb) = n < 0 ? throw(DomainError()) : chebyshev_u(UInt(n), x)

doc"""
    chebyshev_t2(n::Int, x::arb)
> Return the tuple $(T_{n}(x), T_{n-1}(x))$.
"""
chebyshev_t2(n::Int, x::arb) = n < 0 ? throw(DomainError()) : chebyshev_t2(UInt(n), x)

doc"""
    chebyshev_u2(n::Int, x::arb)
> Return the tuple $(U_{n}(x), U_{n-1}(x))$ 
"""
chebyshev_u2(n::Int, x::arb) = n < 0 ? throw(DomainError()) : chebyshev_u2(UInt(n), x)

doc"""
    bell(n::fmpz, r::ArbField)
> Return the Bell number $B_n$ as an element of $r$.
"""
function bell(n::fmpz, r::ArbField)
  z = r()
  ccall((:arb_bell_fmpz, :libarb), Void,
              (Ptr{arb}, Ptr{fmpz}, Int), &z, &n, r.prec)
  return z
end

doc"""
    bell(n::Int, r::ArbField)
> Return the Bell number $B_n$ as an element of $r$.
"""
bell(n::Int, r::ArbField) = bell(fmpz(n), r)

################################################################################
#
#  Unsafe operations
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

function addeq!(z::arb, x::arb)
    ccall((:arb_add, :libarb), Void, (Ptr{arb}, Ptr{arb}, Ptr{arb}, Int),
                           &z, &z, &y, parent(x).prec)
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

call(r::ArbField, x::Integer) = r(fmpz(x))

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

function call(r::ArbField, x::BigFloat)
  z = arb(x, r.prec)
  z.parent = r
  return z
end

################################################################################
#
#  Arb real field constructor
#
################################################################################

# see inner constructor for ArbField
