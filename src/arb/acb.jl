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

export one, onei, real, imag, conj, abs, inv, angle, isreal

export sqrt, rsqrt, log, log1p, exp, exppii, sin, cos, tan, cot,
       sinpi, cospi, tanpi, cotpi, sincos, sincospi, sinh, cosh, tanh, coth,
       sinhcosh, atan, logsinpi, gamma, rgamma, lgamma, digamma, risingfac,
       risingfac2, polygamma, polylog, zeta, barnesg, logbarnesg, agm,
       erf, erfi, erfc, ei, si, ci, shi, chi, li, lioffset, expint, gamma,
       besselj, bessely, besseli, besselk, hyp1f1, hyp1f1r, hyperu, hyp2f1,
       jtheta, modeta, modj, modlambda, moddelta, ellipwp, ellipk, ellipe


###############################################################################
#
#   Basic manipulation
#
###############################################################################

elem_type(::AcbField) = acb

parent_type(::Type{acb}) = AcbField

doc"""
    base_ring(R::AcbField)
> Returns `Union{}` since an Arb complex field does not depend on any other
> ring.
"""
base_ring(R::AcbField) = Union{} 

doc"""
    base_ring(a::acb)
> Returns `Union{}` since an Arb complex field does not depend on any other
> ring.
"""
base_ring(a::acb) = Union{} 

doc"""
    parent(x::acb)
> Return the parent of the given Arb complex field element.
"""
parent(x::acb) = x.parent

doc"""
    zero(R::AcbField)
> Return exact zero in the given Arb complex field. 
"""
function zero(r::AcbField)
  z = acb()
  z.parent = r
  return z
end

doc"""
    one(R::AcbField)
> Return exact one in the given Arb complex field. 
"""
function one(r::AcbField)
  z = acb()
  ccall((:acb_one, :libarb), Void, (Ptr{acb}, ), &z)
  z.parent = r
  return z
end

doc"""
    onei(R::AcbField)
> Return exact one times $i$ in the given Arb complex field. 
"""
function onei(r::AcbField)
  z = acb()
  ccall((:acb_onei, :libarb), Void, (Ptr{acb}, ), &z)
  z.parent = r
  return z
end

doc"""
    accuracy_bits(x::acb)
> Return the relative accuracy of $x$ measured in bits, capped between
> `typemax(Int)` and `-typemax(Int)`.
"""
function accuracy_bits(x::acb)
  # bug in acb.h: rel_accuracy_bits is not in the library
  return -ccall((:acb_rel_error_bits, :libarb), Int, (Ptr{acb},), &x)
end

function deepcopy_internal(a::acb, dict::ObjectIdDict)
  b = parent(a)()
  ccall((:acb_set, :libarb), Void, (Ptr{acb}, Ptr{acb}), &b, &a)
  return b
end

# TODO: implement hash 

################################################################################
#
#  Conversions
#
################################################################################

function convert(::Type{Complex128}, x::acb)
    re = ccall((:acb_real_ptr, :libarb), Ptr{arb_struct}, (Ptr{acb}, ), &x)
    im = ccall((:acb_imag_ptr, :libarb), Ptr{arb_struct}, (Ptr{acb}, ), &x)
    t = ccall((:arb_mid_ptr, :libarb), Ptr{arf_struct}, (Ptr{arb}, ), re)
    u = ccall((:arb_mid_ptr, :libarb), Ptr{arf_struct}, (Ptr{arb}, ), im)
    # 4 == round to nearest
    v = ccall((:arf_get_d, :libarb), Float64, (Ptr{arf_struct}, Int), t, 4)
    w = ccall((:arf_get_d, :libarb), Float64, (Ptr{arf_struct}, Int), u, 4)
    return complex(v, w)
end

################################################################################
#
#  Real and imaginary part
#
################################################################################

doc"""
    real(x::acb)
> Return the real part of $x$ as an `arb`.
"""
function real(x::acb)
  z = arb()
  ccall((:acb_get_real, :libarb), Void, (Ptr{arb}, Ptr{acb}), &z, &x)
  z.parent = ArbField(parent(x).prec)
  return z
end

doc"""
    imag(x::acb)
> Return the imaginary part of $x$ as an `arb`.
"""
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

function show(io::IO, x::AcbField)
  print(io, "Complex Field with ")
  print(io, prec(x))
  print(io, " bits of precision and error bounds")
end

needs_parentheses(x::acb) = true

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
^(x::Integer, y::acb) = fmpz(x)^y

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

+(x::acb, y::Integer) = x + fmpz(y)

-(x::acb, y::Integer) = x - fmpz(y)

*(x::acb, y::Integer) = x*fmpz(y)

//(x::acb, y::Integer) = x//fmpz(y)

+(x::Integer, y::acb) = fmpz(x) + y

-(x::Integer, y::acb) = fmpz(x) - y

*(x::Integer, y::acb) = fmpz(x)*y

//(x::Integer, y::acb) = fmpz(x)//y

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

divexact(x::acb, y::acb) = x // y
divexact(x::fmpz, y::acb) = x // y
divexact(x::acb, y::fmpz) = x // y
divexact(x::Int, y::acb) = x // y
divexact(x::acb, y::Int) = x // y
divexact(x::UInt, y::acb) = x // y
divexact(x::acb, y::UInt) = x // y
divexact(x::fmpq, y::acb) = x // y
divexact(x::acb, y::fmpq) = x // y
divexact(x::arb, y::acb) = x // y
divexact(x::acb, y::arb) = x // y

/(x::acb, y::acb) = x // y
/(x::fmpz, y::acb) = x // y
/(x::acb, y::fmpz) = x // y
/(x::Int, y::acb) = x // y
/(x::acb, y::Int) = x // y
/(x::UInt, y::acb) = x // y
/(x::acb, y::UInt) = x // y
/(x::fmpq, y::acb) = x // y
/(x::acb, y::fmpq) = x // y
/(x::arb, y::acb) = x // y
/(x::acb, y::arb) = x // y

################################################################################
#
#  Comparison
#
################################################################################

doc"""
    isequal(x::acb, y::acb)
> Return `true` if the boxes $x$ and $y$ are precisely equal, i.e. their real
> and imaginary parts have the same midpoints and radii.
"""
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

==(x::acb,y::Integer) = x == fmpz(y)
==(x::Integer,y::acb) = fmpz(x) == y

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

################################################################################
#
#  Containment
#
################################################################################

doc"""
    overlaps(x::acb, y::acb)
> Returns `true` if any part of the box $x$ overlaps any part of the box $y$,
> otherwise return `false`.
"""
function overlaps(x::acb, y::acb)
  r = ccall((:acb_overlaps, :libarb), Cint, (Ptr{acb}, Ptr{acb}), &x, &y)
  return Bool(r)
end

doc"""
    contains(x::acb, y::acb)
> Returns `true` if the box $x$ contains the box $y$, otherwise return
> `false`.
"""
function contains(x::acb, y::acb)
  r = ccall((:acb_contains, :libarb), Cint, (Ptr{acb}, Ptr{acb}), &x, &y)
  return Bool(r)
end

doc"""
    contains(x::acb, y::fmpq)
> Returns `true` if the box $x$ contains the given rational value, otherwise
> return `false`.
"""
function contains(x::acb, y::fmpq)
  r = ccall((:acb_contains_fmpq, :libarb), Cint, (Ptr{acb}, Ptr{fmpq}), &x, &y)
  return Bool(r)
end

doc"""
    contains(x::acb, y::fmpz)
> Returns `true` if the box $x$ contains the given integer value, otherwise
> return `false`.
"""
function contains(x::acb, y::fmpz)
  r = ccall((:acb_contains_fmpz, :libarb), Cint, (Ptr{acb}, Ptr{fmpz}), &x, &y)
  return Bool(r)
end

function contains(x::acb, y::Int)
  v = fmpz(y)
  r = ccall((:acb_contains_fmpz, :libarb), Cint, (Ptr{acb}, Ptr{fmpz}), &x, &v)
  return Bool(r)
end

doc"""
    contains(x::acb, y::Integer)
> Returns `true` if the box $x$ contains the given integer value, otherwise
> return `false`.
"""
contains(x::acb, y::Integer) = contains(x, fmpz(y))

doc"""
    contains_zero(x::acb)
> Returns `true` if the box $x$ contains zero, otherwise return `false`.
"""
function contains_zero(x::acb)
   return Bool(ccall((:acb_contains_zero, :libarb), Cint, (Ptr{acb},), &x))
end

################################################################################
#
#  Predicates
#
################################################################################

function isunit(x::acb)
   !iszero(x)
end

doc"""
    iszero(x::acb)
> Return `true` if $x$ is certainly zero, otherwise return `false`.
"""
function iszero(x::acb)
   return Bool(ccall((:acb_is_zero, :libarb), Cint, (Ptr{acb},), &x))
end

doc"""
    isone(x::acb)
> Return `true` if $x$ is certainly zero, otherwise return `false`.
"""
function isone(x::acb)
   return Bool(ccall((:acb_is_one, :libarb), Cint, (Ptr{acb},), &x))
end

doc"""
    isfinite(x::acb)
> Return `true` if $x$ is finite, i.e. its real and imaginary parts have finite
> midpoint and radius, otherwise return `false`.
"""
function isfinite(x::acb)
   return Bool(ccall((:acb_is_finite, :libarb), Cint, (Ptr{acb},), &x))
end

doc"""
    isexact(x::acb)
> Return `true` if $x$ is exact, i.e. has its real and imaginary parts have
> zero radius, otherwise return `false`.
"""
function isexact(x::acb)
   return Bool(ccall((:acb_is_exact, :libarb), Cint, (Ptr{acb},), &x))
end

doc"""
    isint(x::acb)
> Return `true` if $x$ is an exact integer, otherwise return `false`.
"""
function isint(x::acb)
   return Bool(ccall((:acb_is_int, :libarb), Cint, (Ptr{acb},), &x))
end

doc"""
    isint(x::acb)
> Return `true` if $x$ is purely real, i.e. having zero imaginary part,
> otherwise return `false`.
"""
function isreal(x::acb)
   return Bool(ccall((:acb_is_real, :libarb), Cint, (Ptr{acb},), &x))
end

isnegative(x::acb) = isreal(x) && isnegative(real(x))

################################################################################
#
#  Absolute value
#
################################################################################

doc"""
    abs(x::acb)
> Return the complex absolute value of $x$.
"""
function abs(x::acb)
  z = arb()
  ccall((:acb_abs, :libarb), Void,
                (Ptr{arb}, Ptr{acb}, Int), &z, &x, parent(x).prec)
  z.parent = ArbField(parent(x).prec)
  return z
end

################################################################################
#
#  Inversion
#
################################################################################

doc"""
    inv(x::acb)
> Return the multiplicative inverse of $x$, i.e. $1/x$.
"""
function inv(x::acb)
  z = parent(x)()
  ccall((:acb_inv, :libarb), Void, (Ptr{acb}, Ptr{acb}, Int), &z, &x, parent(x).prec)
  return z
end

################################################################################
#
#  Shifting
#
################################################################################

doc"""
    ldexp(x::acb, y::Int)
> Return $2^yx$. Note that $y$ can be positive, zero or negative.
"""
function ldexp(x::acb, y::Int)
  z = parent(x)()
  ccall((:acb_mul_2exp_si, :libarb), Void,
              (Ptr{acb}, Ptr{acb}, Int), &z, &x, y)
  return z
end

doc"""
    ldexp(x::acb, y::fmpz)
> Return $2^yx$. Note that $y$ can be positive, zero or negative.
"""
function ldexp(x::acb, y::fmpz)
  z = parent(x)()
  ccall((:acb_mul_2exp_fmpz, :libarb), Void,
              (Ptr{acb}, Ptr{acb}, Ptr{fmpz}), &z, &x, &y)
  return z
end

################################################################################
#
#  Miscellaneous
#
################################################################################

doc"""
    trim(x::acb)
> Return an `acb` box containing $x$ but which may be more economical,
> by rounding off insignificant bits from midpoints.
"""
function trim(x::acb)
  z = parent(x)()
  ccall((:acb_trim, :libarb), Void, (Ptr{acb}, Ptr{acb}), &z, &x)
  return z
end

doc"""
    unique_integer(x::acb)
> Return a pair where the first value is a boolean and the second is an `fmpz`
> integer. The boolean indicates whether the box $x$ contains a unique
> integer. If this is the case, the second return value is set to this unique
> integer.
"""
function unique_integer(x::acb)
  z = fmpz()
  unique = ccall((:acb_get_unique_fmpz, :libarb), Int,
    (Ptr{fmpz}, Ptr{acb}), &z, &x)
  return (unique != 0, z)
end

doc"""
    conj(x::acb)
> Return the complex conjugate of $x$.
"""
function conj(x::acb)
  z = parent(x)()
  ccall((:acb_conj, :libarb), Void, (Ptr{acb}, Ptr{acb}), &z, &x)
  return z
end

doc"""
    angle(x::acb)
> Return the angle in radians that the complex vector $x$ makes with the
> positive real axis in a counterclockwise direction.
"""
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

doc"""
    sqrt(x::acb)
> Return the square root of $x$.
"""
function sqrt(x::acb)
   z = parent(x)()
   ccall((:acb_sqrt, :libarb), Void, (Ptr{acb}, Ptr{acb}, Int), &z, &x, parent(x).prec)
   return z
end

doc"""
    rsqrt(x::acb)
> Return the reciprocal of the square root of $x$, i.e. $1/\sqrt{x}$.
"""
function rsqrt(x::acb)
   z = parent(x)()
   ccall((:acb_rsqrt, :libarb), Void, (Ptr{acb}, Ptr{acb}, Int), &z, &x, parent(x).prec)
   return z
end

doc"""
    log(x::acb)
> Return the principal branch of the logarithm of $x$.

"""
function log(x::acb)
   z = parent(x)()
   ccall((:acb_log, :libarb), Void, (Ptr{acb}, Ptr{acb}, Int), &z, &x, parent(x).prec)
   return z
end

doc"""
    log1p(x::acb)
> Return $\log(1+x)$, evaluated accurately for small $x$.
"""
function log1p(x::acb)
   z = parent(x)()
   ccall((:acb_log1p, :libarb), Void, (Ptr{acb}, Ptr{acb}, Int), &z, &x, parent(x).prec)
   return z
end

doc"""
    exp(x::acb)
> Return the exponential of $x$.
"""
function exp(x::acb)
   z = parent(x)()
   ccall((:acb_exp, :libarb), Void, (Ptr{acb}, Ptr{acb}, Int), &z, &x, parent(x).prec)
   return z
end

doc"""
    exppii(x::acb)
> Return the exponential of $\pi i x$.
"""
function exppii(x::acb)
   z = parent(x)()
   ccall((:acb_exp_pi_i, :libarb), Void, (Ptr{acb}, Ptr{acb}, Int), &z, &x, parent(x).prec)
   return z
end

doc"""
    sin(x::acb)
> Return the sine of $x$.
"""
function sin(x::acb)
   z = parent(x)()
   ccall((:acb_sin, :libarb), Void, (Ptr{acb}, Ptr{acb}, Int), &z, &x, parent(x).prec)
   return z
end

doc"""
    cos(x::acb)
> Return the cosine of $x$.
"""
function cos(x::acb)
   z = parent(x)()
   ccall((:acb_cos, :libarb), Void, (Ptr{acb}, Ptr{acb}, Int), &z, &x, parent(x).prec)
   return z
end

doc"""
    tan(x::acb)
> Return the tangent of $x$.
"""
function tan(x::acb)
   z = parent(x)()
   ccall((:acb_tan, :libarb), Void, (Ptr{acb}, Ptr{acb}, Int), &z, &x, parent(x).prec)
   return z
end

doc"""
    cot(x::acb)
> Return the cotangent of $x$.
"""
function cot(x::acb)
   z = parent(x)()
   ccall((:acb_cot, :libarb), Void, (Ptr{acb}, Ptr{acb}, Int), &z, &x, parent(x).prec)
   return z
end

doc"""
    sinpi(x::acb)
> Return the sine of $\pi x$.
"""
function sinpi(x::acb)
   z = parent(x)()
   ccall((:acb_sin_pi, :libarb), Void, (Ptr{acb}, Ptr{acb}, Int), &z, &x, parent(x).prec)
   return z
end

doc"""
    cospi(x::acb)
> Return the cosine of $\pi x$.
"""
function cospi(x::acb)
   z = parent(x)()
   ccall((:acb_cos_pi, :libarb), Void, (Ptr{acb}, Ptr{acb}, Int), &z, &x, parent(x).prec)
   return z
end

doc"""
    tanpi(x::acb)
> Return the tangent of $\pi x$.
"""
function tanpi(x::acb)
   z = parent(x)()
   ccall((:acb_tan_pi, :libarb), Void, (Ptr{acb}, Ptr{acb}, Int), &z, &x, parent(x).prec)
   return z
end

doc"""
    cotpi(x::acb)
> Return the cotangent of $\pi x$.
"""
function cotpi(x::acb)
   z = parent(x)()
   ccall((:acb_cot_pi, :libarb), Void, (Ptr{acb}, Ptr{acb}, Int), &z, &x, parent(x).prec)
   return z
end

doc"""
    sinh(x::acb)
> Return the hyperbolic sine of $x$.
"""
function sinh(x::acb)
   z = parent(x)()
   ccall((:acb_sinh, :libarb), Void, (Ptr{acb}, Ptr{acb}, Int), &z, &x, parent(x).prec)
   return z
end

doc"""
    cosh(x::acb)
> Return the hyperbolic cosine of $x$.
"""
function cosh(x::acb)
   z = parent(x)()
   ccall((:acb_cosh, :libarb), Void, (Ptr{acb}, Ptr{acb}, Int), &z, &x, parent(x).prec)
   return z
end

doc"""
    tanh(x::acb)
> Return the hyperbolic tangent of $x$.
"""
function tanh(x::acb)
   z = parent(x)()
   ccall((:acb_tanh, :libarb), Void, (Ptr{acb}, Ptr{acb}, Int), &z, &x, parent(x).prec)
   return z
end

doc"""
    coth(x::acb)
> Return the hyperbolic cotangent of $x$.
"""
function coth(x::acb)
   z = parent(x)()
   ccall((:acb_coth, :libarb), Void, (Ptr{acb}, Ptr{acb}, Int), &z, &x, parent(x).prec)
   return z
end

doc"""
    atan(x::acb)
> Return the arctangent of $x$.
"""
function atan(x::acb)
   z = parent(x)()
   ccall((:acb_atan, :libarb), Void, (Ptr{acb}, Ptr{acb}, Int), &z, &x, parent(x).prec)
   return z
end

doc"""
    logsinpi(x::acb)
> Return $\log\sin(\pi x)$, constructed without branch cuts off the real line.
"""
function logsinpi(x::acb)
   z = parent(x)()
   ccall((:acb_log_sin_pi, :libarb), Void, (Ptr{acb}, Ptr{acb}, Int), &z, &x, parent(x).prec)
   return z
end

doc"""
    gamma(x::acb)
> Return the Gamma function evaluated at $x$.
"""
function gamma(x::acb)
   z = parent(x)()
   ccall((:acb_gamma, :libarb), Void, (Ptr{acb}, Ptr{acb}, Int), &z, &x, parent(x).prec)
   return z
end

doc"""
    rgamma(x::acb)
> Return the reciprocal of the Gamma function evaluated at $x$.
"""
function rgamma(x::acb)
   z = parent(x)()
   ccall((:acb_rgamma, :libarb), Void, (Ptr{acb}, Ptr{acb}, Int), &z, &x, parent(x).prec)
   return z
end

doc"""
    lgamma(x::acb)
> Return the logarithm of the Gamma function evaluated at $x$.
"""
function lgamma(x::acb)
   z = parent(x)()
   ccall((:acb_lgamma, :libarb), Void, (Ptr{acb}, Ptr{acb}, Int), &z, &x, parent(x).prec)
   return z
end

doc"""
    digamma(x::acb)
> Return the  logarithmic derivative of the gamma function evaluated at $x$,
> i.e. $\psi(x)$.
"""
function digamma(x::acb)
   z = parent(x)()
   ccall((:acb_digamma, :libarb), Void, (Ptr{acb}, Ptr{acb}, Int), &z, &x, parent(x).prec)
   return z
end

doc"""
    zeta(x::acb)
> Return the Riemann zeta function evaluated at $x$.
"""
function zeta(x::acb)
   z = parent(x)()
   ccall((:acb_zeta, :libarb), Void, (Ptr{acb}, Ptr{acb}, Int), &z, &x, parent(x).prec)
   return z
end

doc"""
    barnesg(x::acb)
> Return the Barnes $G$-function, evaluated at $x$.
"""
function barnesg(x::acb)
   z = parent(x)()
   ccall((:acb_barnes_g, :libarb), Void, (Ptr{acb}, Ptr{acb}, Int), &z, &x, parent(x).prec)
   return z
end

doc"""
    logbarnesg(x::acb)
> Return the logarithm of the Barnes $G$-function, evaluated at $x$.
"""
function logbarnesg(x::acb)
   z = parent(x)()
   ccall((:acb_log_barnes_g, :libarb), Void, (Ptr{acb}, Ptr{acb}, Int), &z, &x, parent(x).prec)
   return z
end

doc"""
    agm(x::acb)
> Return the arithmetic-geometric mean of $1$ and $x$.
"""
function agm(x::acb)
   z = parent(x)()
   ccall((:acb_agm1, :libarb), Void, (Ptr{acb}, Ptr{acb}, Int), &z, &x, parent(x).prec)
   return z
end

doc"""
    erf(x::acb)
> Return the error function evaluated at $x$.
"""
function erf(x::acb)
   z = parent(x)()
   ccall((:acb_hypgeom_erf, :libarb), Void, (Ptr{acb}, Ptr{acb}, Int), &z, &x, parent(x).prec)
   return z
end

doc"""
    erfi(x::acb)
> Return the imaginary error function evaluated at $x$.
"""
function erfi(x::acb)
   z = parent(x)()
   ccall((:acb_hypgeom_erfi, :libarb), Void, (Ptr{acb}, Ptr{acb}, Int), &z, &x, parent(x).prec)
   return z
end

doc"""
    erfc(x::acb)
> Return the complex error function evaluated at $x$.
"""
function erfc(x::acb)
   z = parent(x)()
   ccall((:acb_hypgeom_erfc, :libarb), Void, (Ptr{acb}, Ptr{acb}, Int), &z, &x, parent(x).prec)
   return z
end

doc"""
    ei(x::acb)
> Return the exponential integral evaluated at $x$.
"""
function ei(x::acb)
   z = parent(x)()
   ccall((:acb_hypgeom_ei, :libarb), Void, (Ptr{acb}, Ptr{acb}, Int), &z, &x, parent(x).prec)
   return z
end

doc"""
    si(x::acb)
> Return the sine integral evaluated at $x$.
"""
function si(x::acb)
   z = parent(x)()
   ccall((:acb_hypgeom_si, :libarb), Void, (Ptr{acb}, Ptr{acb}, Int), &z, &x, parent(x).prec)
   return z
end

doc"""
    ci(x::acb)
> Return the exponential cosine integral evaluated at $x$.
"""
function ci(x::acb)
   z = parent(x)()
   ccall((:acb_hypgeom_ci, :libarb), Void, (Ptr{acb}, Ptr{acb}, Int), &z, &x, parent(x).prec)
   return z
end

doc"""
    shi(x::acb)
> Return the hyperbolic sine integral evaluated at $x$.
"""
function shi(x::acb)
   z = parent(x)()
   ccall((:acb_hypgeom_shi, :libarb), Void, (Ptr{acb}, Ptr{acb}, Int), &z, &x, parent(x).prec)
   return z
end

doc"""
    chi(x::acb)
> Return the hyperbolic cosine integral evaluated at $x$.
"""
function chi(x::acb)
   z = parent(x)()
   ccall((:acb_hypgeom_chi, :libarb), Void, (Ptr{acb}, Ptr{acb}, Int), &z, &x, parent(x).prec)
   return z
end

doc"""
    modeta(x::acb)
> Return the Dedekind eta function $\eta(\tau)$ at $\tau = x$.
"""
function modeta(x::acb)
   z = parent(x)()
   ccall((:acb_modular_eta, :libarb), Void, (Ptr{acb}, Ptr{acb}, Int), &z, &x, parent(x).prec)
   return z
end

doc"""
    modj(x::acb)
> Return the $j$-invariant $j(\tau)$ at $\tau = x$.
"""
function modj(x::acb)
   z = parent(x)()
   ccall((:acb_modular_j, :libarb), Void, (Ptr{acb}, Ptr{acb}, Int), &z, &x, parent(x).prec)
   return z
end

doc"""
    modlambda(x::acb)
> Return the modular lambda function $\lambda(\tau)$ at $\tau = x$.
"""
function modlambda(x::acb)
   z = parent(x)()
   ccall((:acb_modular_lambda, :libarb), Void, (Ptr{acb}, Ptr{acb}, Int), &z, &x, parent(x).prec)
   return z
end

doc"""
    moddelta(x::acb)
> Return the modular delta function $\Delta(\tau)$ at $\tau = x$.
"""
function moddelta(x::acb)
   z = parent(x)()
   ccall((:acb_modular_delta, :libarb), Void, (Ptr{acb}, Ptr{acb}, Int), &z, &x, parent(x).prec)
   return z
end
  
doc"""
    ellipk(x::acb)
> Return the complete elliptic integral $K(x)$.
"""
function ellipk(x::acb)
   z = parent(x)()
   ccall((:acb_modular_elliptic_k, :libarb), Void, (Ptr{acb}, Ptr{acb}, Int), &z, &x, parent(x).prec)
   return z
end

doc"""
    ellipe(x::acb)
> Return the complete elliptic integral $E(x)$.
"""
function ellipe(x::acb)
   z = parent(x)()
   ccall((:acb_modular_elliptic_e, :libarb), Void, (Ptr{acb}, Ptr{acb}, Int), &z, &x, parent(x).prec)
   return z
end

doc"""
    sincos(x::acb)
> Return a tuple $s, c$ consisting of the sine $s$ and cosine $c$ of $x$.
"""
function sincos(x::acb)
  s = parent(x)()
  c = parent(x)()
  ccall((:acb_sin_cos, :libarb), Void,
              (Ptr{acb}, Ptr{acb}, Ptr{acb}, Int), &s, &c, &x, parent(x).prec)
  return (s, c)
end

doc"""
    sincospi(x::acb)
> Return a tuple $s, c$ consisting of the sine $s$ and cosine $c$ of $\pi x$.
"""
function sincospi(x::acb)
  s = parent(x)()
  c = parent(x)()
  ccall((:acb_sin_cos_pi, :libarb), Void,
              (Ptr{acb}, Ptr{acb}, Ptr{acb}, Int), &s, &c, &x, parent(x).prec)
  return (s, c)
end

doc"""
    sinhcosh(x::acb)
> Return a tuple $s, c$ consisting of the hyperbolic sine and cosine of $x$.
"""
function sinhcosh(x::acb)
  s = parent(x)()
  c = parent(x)()
  ccall((:acb_sinh_cosh, :libarb), Void,
              (Ptr{acb}, Ptr{acb}, Ptr{acb}, Int), &s, &c, &x, parent(x).prec)
  return (s, c)
end

doc"""
    zeta(s::acb, a::acb)
> Return the Hurwitz zeta function $\zeta(s,a)$.
"""
function zeta(s::acb, a::acb)
  z = parent(s)()
  ccall((:acb_hurwitz_zeta, :libarb), Void,
              (Ptr{acb}, Ptr{acb}, Ptr{acb}, Int), &z, &s, &a, parent(s).prec)
  return z
end

doc"""
    polygamma(s::acb, a::acb)
> Return the generalised polygamma function $\psi(s,z)$.
"""
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

doc"""
    risingfac(x::acb, n::Int)
> Return the rising factorial $x(x + 1)\ldots (x + n - 1)$ as an Acb.
"""
risingfac(x::acb, n::Int) = n < 0 ? throw(DomainError()) : risingfac(x, UInt(n))

function risingfac2(x::acb, n::UInt)
  z = parent(x)()
  w = parent(x)()
  ccall((:acb_rising2_ui, :libarb), Void,
              (Ptr{acb}, Ptr{acb}, Ptr{acb}, UInt, Int), &z, &w, &x, n, parent(x).prec)
  return (z, w)
end

doc"""
    risingfac2(x::acb, n::Int)
> Return a tuple containing the rising factorial $x(x + 1)\ldots (x + n - 1)$
> and its derivative.
"""
risingfac2(x::acb, n::Int) = n < 0 ? throw(DomainError()) : risingfac2(x, UInt(n))

doc"""
    polylog(s::acb, a::acb)
> 
"""
function polylog(s::acb, a::acb)
  z = parent(s)()
  ccall((:acb_polylog, :libarb), Void,
              (Ptr{acb}, Ptr{acb}, Ptr{acb}, Int), &z, &s, &a, parent(s).prec)
  return z
end

doc"""
    polylog(s::Int, a::acb)
> Return the polylogarithm Li$_s(a)$.
"""
function polylog(s::Int, a::acb)
  z = parent(a)()
  ccall((:acb_polylog_si, :libarb), Void,
              (Ptr{acb}, Int, Ptr{acb}, Int), &z, s, &a, parent(a).prec)
  return z
end

doc"""
    li(x::acb)
> Return the logarithmic integral, evaluated at $x$.
"""
function li(x::acb)
  z = parent(x)()
  ccall((:acb_hypgeom_li, :libarb), Void,
              (Ptr{acb}, Ptr{acb}, Int, Int), &z, &x, 0, parent(x).prec)
  return z
end

doc"""
    lioffset(x::acb)
> Return the offset logarithmic integral, evaluated at $x$.
"""
function lioffset(x::acb)
  z = parent(x)()
  ccall((:acb_hypgeom_li, :libarb), Void,
              (Ptr{acb}, Ptr{acb}, Int, Int), &z, &x, 1, parent(x).prec)
  return z
end

doc"""
    expint(s::acb, x::acb)
> Return the generalised exponential integral $E_s(x)$.
"""
function expint(s::acb, x::acb)
  z = parent(s)()
  ccall((:acb_hypgeom_expint, :libarb), Void,
              (Ptr{acb}, Ptr{acb}, Ptr{acb}, Int), &z, &s, &x, parent(s).prec)
  return z
end

doc"""
    gamma(s::acb, x::acb)
> Return the upper incomplete gamma function $\Gamma(s,x)$.
"""
function gamma(s::acb, x::acb)
  z = parent(s)()
  ccall((:acb_hypgeom_gamma_upper, :libarb), Void,
              (Ptr{acb}, Ptr{acb}, Ptr{acb}, Int, Int), &z, &s, &x, 0, parent(s).prec)
  return z
end

doc"""
    besselj(nu::acb, x::acb)
> Return the Bessel function $J_{\nu}(x)$.
"""
function besselj(nu::acb, x::acb)
  z = parent(x)()
  ccall((:acb_hypgeom_bessel_j, :libarb), Void,
              (Ptr{acb}, Ptr{acb}, Ptr{acb}, Int), &z, &nu, &x, parent(x).prec)
  return z
end

doc"""
    bessely(nu::acb, x::acb)
> Return the Bessel function $Y_{\nu}(x)$.
"""
function bessely(nu::acb, x::acb)
  z = parent(x)()
  ccall((:acb_hypgeom_bessel_y, :libarb), Void,
              (Ptr{acb}, Ptr{acb}, Ptr{acb}, Int), &z, &nu, &x, parent(x).prec)
  return z
end

doc"""
    besseli(nu::acb, x::acb)
> Return the Bessel function $I_{\nu}(x)$.
"""
function besseli(nu::acb, x::acb)
  z = parent(x)()
  ccall((:acb_hypgeom_bessel_i, :libarb), Void,
              (Ptr{acb}, Ptr{acb}, Ptr{acb}, Int), &z, &nu, &x, parent(x).prec)
  return z
end

doc"""
    besselk(nu::acb, x::acb)
> Return the Bessel function $K_{\nu}(x)$.
"""
function besselk(nu::acb, x::acb)
  z = parent(x)()
  ccall((:acb_hypgeom_bessel_k, :libarb), Void,
              (Ptr{acb}, Ptr{acb}, Ptr{acb}, Int), &z, &nu, &x, parent(x).prec)
  return z
end

doc"""
    hyp1f1(a::acb, b::acb, x::acb)
> Return the confluent hypergeometric function ${}_1F1(a,b,x)$.
"""
function hyp1f1(a::acb, b::acb, x::acb)
  z = parent(x)()
  ccall((:acb_hypgeom_m, :libarb), Void,
              (Ptr{acb}, Ptr{acb}, Ptr{acb}, Ptr{acb}, Int, Int), &z, &a, &b, &x, 0, parent(x).prec)
  return z
end

doc"""
    hyp1f1r(a::acb, b::acb, x::acb)
> Return the regularized confluent hypergeometric function
> ${}_1F1(a,b,x) / \Gamma(b)$.
"""
function hyp1f1r(a::acb, b::acb, x::acb)
  z = parent(x)()
  ccall((:acb_hypgeom_m, :libarb), Void,
              (Ptr{acb}, Ptr{acb}, Ptr{acb}, Ptr{acb}, Int, Int), &z, &a, &b, &x, 1, parent(x).prec)
  return z
end

doc"""
    hyperu(a::acb, b::acb, x::acb)
> Return the confluent hypergeometric function $U(a,b,x)$.
"""
function hyperu(a::acb, b::acb, x::acb)
  z = parent(x)()
  ccall((:acb_hypgeom_u, :libarb), Void,
              (Ptr{acb}, Ptr{acb}, Ptr{acb}, Ptr{acb}, Int), &z, &a, &b, &x, parent(x).prec)
  return z
end

doc"""
    hyp2f1(a::acb, b::acb, c::acb, x::acb)
> Return the Gauss hypergeometric function ${}_2F_1(a,b,c,x)$.
"""
function hyp2f1(a::acb, b::acb, c::acb, x::acb; flags=0)
  z = parent(x)()
  ccall((:acb_hypgeom_2f1, :libarb), Void,
              (Ptr{acb}, Ptr{acb}, Ptr{acb}, Ptr{acb}, Ptr{acb}, Int, Int), &z, &a, &b, &c, &x, flags, parent(x).prec)
  return z
end

doc"""
    jtheta(z::acb, tau::acb)
> Return a tuple of four elements containing the Jacobi theta function values
> $\theta_1, \theta_2, \theta_3, \theta_4$ evaluated at $z, \tau$.
"""
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

doc"""
    ellipwp(z::acb, tau::acb)
> Return the Weierstrass elliptic function $\wp(z,\tau)$.
"""
function ellipwp(z::acb, tau::acb)
  r = parent(z)()
  ccall((:acb_modular_elliptic_p, :libarb), Void,
              (Ptr{acb}, Ptr{acb}, Ptr{acb}, Int), &r, &z, &tau, parent(z).prec)
  return r
end

doc"""
    agm(x::acb, y::acb)
> Return the arithmetic-geometric mean of $x$ and $y$.
"""
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

function zero!(z::acb)
   ccall((:acb_zero, :libarb), Void, (Ptr{acb},), &z)
   nothing
end

function add!(z::acb, x::acb, y::acb)
  ccall((:acb_add, :libarb), Void, (Ptr{acb}, Ptr{acb}, Ptr{acb}, Int),
         &z, &x, &y, parent(z).prec)
end

function addeq!(z::acb, y::acb)
  ccall((:acb_add, :libarb), Void, (Ptr{acb}, Ptr{acb}, Ptr{acb}, Int),
         &z, &z, &y, parent(z).prec)
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

function (r::AcbField)()
  z = acb()
  z.parent = r
  return z
end

function (r::AcbField)(x::Union{Int, UInt, fmpz, fmpq, arb, acb, Float64,
                                BigFloat, AbstractString})
  z = acb(x, r.prec)
  z.parent = r
  return z
end

(r::AcbField)(x::Integer) = r(fmpz(x))

function (r::AcbField){T <: Union{Int, UInt, fmpz, fmpq, arb, Float64,
                                              BigFloat, AbstractString}}(x::T, y::T)
  z = acb(x, y, r.prec)
  z.parent = r
  return z
end

################################################################################
#
#  AcbField constructor
#
################################################################################

# see internal constructor
