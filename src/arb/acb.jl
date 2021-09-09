###############################################################################
#
#   acb.jl : Arb complex numbers
#
#   Copyright (C) 2015 Tommy Hofmann
#   Copyright (C) 2015 Fredrik Johansson
#
###############################################################################

import Base: real, imag, abs, conj, angle, log, log1p, sin, cos,
             tan, cot, sinpi, cospi, sinh, cosh, tanh, coth, atan, expm1


export one, onei, real, imag, conj, abs, inv, angle, isreal, polygamma, erf,
       erfi, erfc, bessel_j, bessel_k, bessel_i, bessel_y, airy_ai, airy_bi,
       airy_ai_prime, airy_bi_prime

export rsqrt, log, log1p, exppii, sin, cos, tan, cot, sinpi, cospi, tanpi,
       cotpi, sincos, sincospi, sinh, cosh, tanh, coth, sinhcosh, atan,
       log_sinpi, gamma, rgamma, lgamma, gamma_regularized, gamma_lower,
       gamma_lower_regularized, rising_factorial, rising_factorial2, polylog,
       barnes_g, log_barnes_g, agm, exp_integral_ei, sin_integral,
       cos_integral, sinh_integral, cosh_integral, log_integral,
       log_integral_offset, exp_integral_e, gamma, hypergeometric_1f1,
       hypergeometric_1f1_regularized, hypergeometric_u, hypergeometric_2f1,
       jacobi_theta, modular_delta, dedekind_eta, eisenstein_g, j_invariant,
       modular_lambda, modular_weber_f, modular_weber_f1, modular_weber_f2,
       weierstrass_p, weierstrass_p_prime, elliptic_k, elliptic_e,
       canonical_unit, root_of_unity, hilbert_class_polynomial

###############################################################################
#
#   Basic manipulation
#
###############################################################################

elem_type(::Type{AcbField}) = acb

parent_type(::Type{acb}) = AcbField

@doc Markdown.doc"""
    base_ring(R::AcbField)

Returns `Union{}` since an Arb complex field does not depend on any other
ring.
"""
base_ring(R::AcbField) = Union{}

@doc Markdown.doc"""
    base_ring(a::acb)

Returns `Union{}` since an Arb complex field does not depend on any other
ring.
"""
base_ring(a::acb) = Union{}

parent(x::acb) = x.parent

isdomain_type(::Type{acb}) = true

isexact_type(::Type{acb}) = false

function zero(r::AcbField)
  z = acb()
  z.parent = r
  return z
end

function one(r::AcbField)
  z = acb()
  ccall((:acb_one, libarb), Nothing, (Ref{acb}, ), z)
  z.parent = r
  return z
end

@doc Markdown.doc"""
    onei(r::AcbField)

Return exact one times $i$ in the given Arb complex field.
"""
function onei(r::AcbField)
  z = acb()
  ccall((:acb_onei, libarb), Nothing, (Ref{acb}, ), z)
  z.parent = r
  return z
end

@doc Markdown.doc"""
    accuracy_bits(x::acb)

Return the relative accuracy of $x$ measured in bits, capped between
`typemax(Int)` and `-typemax(Int)`.
"""
function accuracy_bits(x::acb)
  # bug in acb.h: rel_accuracy_bits is not in the library
  return -ccall((:acb_rel_error_bits, libarb), Int, (Ref{acb},), x)
end

function deepcopy_internal(a::acb, dict::IdDict)
  b = parent(a)()
  ccall((:acb_set, libarb), Nothing, (Ref{acb}, Ref{acb}), b, a)
  return b
end

function canonical_unit(x::acb)
   return x
end

# TODO: implement hash

function check_parent(a::acb, b::acb)
   parent(a) != parent(b) &&
             error("Incompatible acb elements")
end

characteristic(::AcbField) = 0

################################################################################
#
#  Conversions
#
################################################################################

function convert(::Type{ComplexF64}, x::acb)
    GC.@preserve x begin
      re = ccall((:acb_real_ptr, libarb), Ptr{arb_struct}, (Ref{acb}, ), x)
      im = ccall((:acb_imag_ptr, libarb), Ptr{arb_struct}, (Ref{acb}, ), x)
      t = ccall((:arb_mid_ptr, libarb), Ptr{arf_struct}, (Ptr{arb}, ), re)
      u = ccall((:arb_mid_ptr, libarb), Ptr{arf_struct}, (Ptr{arb}, ), im)
      # 4 == round to nearest
      v = ccall((:arf_get_d, libarb), Float64, (Ptr{arf_struct}, Int), t, 4)
      w = ccall((:arf_get_d, libarb), Float64, (Ptr{arf_struct}, Int), u, 4)
    end
    return complex(v, w)
end

################################################################################
#
#  Real and imaginary part
#
################################################################################

function real(x::acb)
  z = arb()
  ccall((:acb_get_real, libarb), Nothing, (Ref{arb}, Ref{acb}), z, x)
  z.parent = ArbField(parent(x).prec)
  return z
end

function imag(x::acb)
  z = arb()
  ccall((:acb_get_imag, libarb), Nothing, (Ref{arb}, Ref{acb}), z, x)
  z.parent = ArbField(parent(x).prec)
  return z
end

################################################################################
#
#  String I/O
#
################################################################################

function expressify(z::acb; context = nothing)
   x = real(z)
   y = imag(z)
   if iszero(y) # is exact zero!
      return expressify(x, context = context)
   else
      y = Expr(:call, :*, expressify(y, context = context), :im)
      if iszero(x)
         return y
      else
         x = expressify(x, context = context)
         return Expr(:call, :+, x, y)
      end
   end
end

function Base.show(io::IO, ::MIME"text/plain", z::acb)
   print(io, AbstractAlgebra.obj_to_string(z, context = io))
end

function Base.show(io::IO, z::acb)
   print(io, AbstractAlgebra.obj_to_string(z, context = io))
end

function show(io::IO, x::AcbField)
  print(io, "Complex Field with ")
  print(io, precision(x))
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
  ccall((:acb_neg, libarb), Nothing, (Ref{acb}, Ref{acb}), z, x)
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
      ccall(($f, libarb), Nothing, (Ref{acb}, Ref{acb}, Ref{acb}, Int),
                           z, x, y, parent(x).prec)
      return z
    end
  end
end

for (f,s) in ((:+, "add"), (:-, "sub"), (:*, "mul"), (://, "div"), (:^, "pow"))
  @eval begin

    function ($f)(x::acb, y::UInt)
      z = parent(x)()
      ccall(($("acb_"*s*"_ui"), libarb), Nothing,
                  (Ref{acb}, Ref{acb}, UInt, Int),
                  z, x, y, parent(x).prec)
      return z
    end

    function ($f)(x::acb, y::Int)
      z = parent(x)()
      ccall(($("acb_"*s*"_si"), libarb), Nothing,
      (Ref{acb}, Ref{acb}, Int, Int), z, x, y, parent(x).prec)
      return z
    end

    function ($f)(x::acb, y::fmpz)
      z = parent(x)()
      ccall(($("acb_"*s*"_fmpz"), libarb), Nothing,
                  (Ref{acb}, Ref{acb}, Ref{fmpz}, Int),
                  z, x, y, parent(x).prec)
      return z
    end

    function ($f)(x::acb, y::arb)
      z = parent(x)()
      ccall(($("acb_"*s*"_arb"), libarb), Nothing,
                  (Ref{acb}, Ref{acb}, Ref{arb}, Int),
                  z, x, y, parent(x).prec)
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
  ccall((:acb_sub_ui, libarb), Nothing, (Ref{acb}, Ref{acb}, UInt, Int), z, y, x, parent(y).prec)
  ccall((:acb_neg, libarb), Nothing, (Ref{acb}, Ref{acb}), z, z)
  return z
end

function -(x::Int, y::acb)
  z = parent(y)()
  ccall((:acb_sub_si, libarb), Nothing, (Ref{acb}, Ref{acb}, Int, Int), z, y, x, parent(y).prec)
  ccall((:acb_neg, libarb), Nothing, (Ref{acb}, Ref{acb}), z, z)
  return z
end

function -(x::fmpz, y::acb)
  z = parent(y)()
  ccall((:acb_sub_fmpz, libarb), Nothing, (Ref{acb}, Ref{acb}, Ref{fmpz}, Int), z, y, x, parent(y).prec)
  ccall((:acb_neg, libarb), Nothing, (Ref{acb}, Ref{acb}), z, z)
  return z
end

function -(x::arb, y::acb)
  z = parent(y)()
  ccall((:acb_sub_arb, libarb), Nothing, (Ref{acb}, Ref{acb}, Ref{arb}, Int), z, y, x, parent(y).prec)
  ccall((:acb_neg, libarb), Nothing, (Ref{acb}, Ref{acb}), z, z)
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

^(x::acb, y::Integer) = x ^ parent(x)(y)

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

divexact(x::acb, y::acb; check::Bool=true) = x // y
divexact(x::fmpz, y::acb; check::Bool=true) = x // y
divexact(x::acb, y::fmpz; check::Bool=true) = x // y
divexact(x::Int, y::acb; check::Bool=true) = x // y
divexact(x::acb, y::Int; check::Bool=true) = x // y
divexact(x::UInt, y::acb; check::Bool=true) = x // y
divexact(x::acb, y::UInt; check::Bool=true) = x // y
divexact(x::fmpq, y::acb; check::Bool=true) = x // y
divexact(x::acb, y::fmpq; check::Bool=true) = x // y
divexact(x::arb, y::acb; check::Bool=true) = x // y
divexact(x::acb, y::arb; check::Bool=true) = x // y
divexact(x::Float64, y::acb; check::Bool=true) = x // y
divexact(x::acb, y::Float64; check::Bool=true) = x // y
divexact(x::BigFloat, y::acb; check::Bool=true) = x // y
divexact(x::acb, y::BigFloat; check::Bool=true) = x // y
divexact(x::Integer, y::acb; check::Bool=true) = x // y
divexact(x::acb, y::Integer; check::Bool=true) = x // y
divexact(x::Rational{T}, y::acb; check::Bool=true) where {T <: Integer} = x // y
divexact(x::acb, y::Rational{T}; check::Bool=true) where {T <: Integer} = x // y

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

+(x::Rational{T}, y::acb) where {T <: Integer} = fmpq(x) + y
+(x::acb, y::Rational{T}) where {T <: Integer} = x + fmpq(y)
-(x::Rational{T}, y::acb) where {T <: Integer} = fmpq(x) - y
-(x::acb, y::Rational{T}) where {T <: Integer} = x - fmpq(y)
*(x::Rational{T}, y::acb) where {T <: Integer} = fmpq(x) * y
*(x::acb, y::Rational{T}) where {T <: Integer} = x * fmpq(y)
//(x::Rational{T}, y::acb) where {T <: Integer} = fmpq(x) // y
//(x::acb, y::Rational{T}) where {T <: Integer} = x // fmpq(y)
^(x::Rational{T}, y::acb) where {T <: Integer} = fmpq(x)^y
^(x::acb, y::Rational{T}) where {T <: Integer} = x ^ fmpq(y)

+(x::Float64, y::acb) = parent(y)(x) + y
+(x::acb, y::Float64) = x + parent(x)(y)
-(x::Float64, y::acb) = parent(y)(x) - y
-(x::acb, y::Float64) = x - parent(x)(y)
*(x::Float64, y::acb) = parent(y)(x) * y
*(x::acb, y::Float64) = x * parent(x)(y)
//(x::Float64, y::acb) = parent(y)(x) // y
//(x::acb, y::Float64) = x // parent(x)(y)
^(x::Float64, y::acb) = parent(y)(x)^y
^(x::acb, y::Float64) = x ^ parent(x)(y)

+(x::BigFloat, y::acb) = parent(y)(x) + y
+(x::acb, y::BigFloat) = x + parent(x)(y)
-(x::BigFloat, y::acb) = parent(y)(x) - y
-(x::acb, y::BigFloat) = x - parent(x)(y)
*(x::BigFloat, y::acb) = parent(y)(x) * y
*(x::acb, y::BigFloat) = x * parent(x)(y)
//(x::BigFloat, y::acb) = parent(y)(x) // y
//(x::acb, y::BigFloat) = x // parent(x)(y)
^(x::BigFloat, y::acb) = parent(y)(x)^y
^(x::acb, y::BigFloat) = x ^ parent(x)(y)

################################################################################
#
#  Comparison
#
################################################################################

@doc Markdown.doc"""
    isequal(x::acb, y::acb)

Return `true` if the boxes $x$ and $y$ are precisely equal, i.e. their real
and imaginary parts have the same midpoints and radii.
"""
function isequal(x::acb, y::acb)
  r = ccall((:acb_equal, libarb), Cint, (Ref{acb}, Ref{acb}), x, y)
  return Bool(r)
end

function ==(x::acb, y::acb)
  r = ccall((:acb_eq, libarb), Cint, (Ref{acb}, Ref{acb}), x, y)
  return Bool(r)
end

function !=(x::acb, y::acb)
  r = ccall((:acb_ne, libarb), Cint, (Ref{acb}, Ref{acb}), x, y)
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

@doc Markdown.doc"""
    overlaps(x::acb, y::acb)

Returns `true` if any part of the box $x$ overlaps any part of the box $y$,
otherwise return `false`.
"""
function overlaps(x::acb, y::acb)
  r = ccall((:acb_overlaps, libarb), Cint, (Ref{acb}, Ref{acb}), x, y)
  return Bool(r)
end

@doc Markdown.doc"""
    contains(x::acb, y::acb)

Returns `true` if the box $x$ contains the box $y$, otherwise return
`false`.
"""
function contains(x::acb, y::acb)
  r = ccall((:acb_contains, libarb), Cint, (Ref{acb}, Ref{acb}), x, y)
  return Bool(r)
end

@doc Markdown.doc"""
    contains(x::acb, y::fmpq)

Returns `true` if the box $x$ contains the given rational value, otherwise
return `false`.
"""
function contains(x::acb, y::fmpq)
  r = ccall((:acb_contains_fmpq, libarb), Cint, (Ref{acb}, Ref{fmpq}), x, y)
  return Bool(r)
end

@doc Markdown.doc"""
    contains(x::acb, y::fmpz)

Returns `true` if the box $x$ contains the given integer value, otherwise
return `false`.
"""
function contains(x::acb, y::fmpz)
  r = ccall((:acb_contains_fmpz, libarb), Cint, (Ref{acb}, Ref{fmpz}), x, y)
  return Bool(r)
end

function contains(x::acb, y::Int)
  v = fmpz(y)
  r = ccall((:acb_contains_fmpz, libarb), Cint, (Ref{acb}, Ref{fmpz}), x, v)
  return Bool(r)
end

@doc Markdown.doc"""
    contains(x::acb, y::Integer)

Returns `true` if the box $x$ contains the given integer value, otherwise
return `false`.
"""
contains(x::acb, y::Integer) = contains(x, fmpz(y))

@doc Markdown.doc"""
    contains(x::acb, y::Rational{T}) where {T <: Integer}

Returns `true` if the box $x$ contains the given rational value, otherwise
return `false`.
"""
contains(x::acb, y::Rational{T}) where {T <: Integer} = contains(x, fmpz(y))

@doc Markdown.doc"""
    contains_zero(x::acb)

Returns `true` if the box $x$ contains zero, otherwise return `false`.
"""
function contains_zero(x::acb)
   return Bool(ccall((:acb_contains_zero, libarb), Cint, (Ref{acb},), x))
end

################################################################################
#
#  Predicates
#
################################################################################

function isunit(x::acb)
   !iszero(x)
end

@doc Markdown.doc"""
    iszero(x::acb)

Return `true` if $x$ is certainly zero, otherwise return `false`.
"""
function iszero(x::acb)
   return Bool(ccall((:acb_is_zero, libarb), Cint, (Ref{acb},), x))
end

@doc Markdown.doc"""
    isone(x::acb)

Return `true` if $x$ is certainly zero, otherwise return `false`.
"""
function isone(x::acb)
   return Bool(ccall((:acb_is_one, libarb), Cint, (Ref{acb},), x))
end

@doc Markdown.doc"""
    isfinite(x::acb)

Return `true` if $x$ is finite, i.e. its real and imaginary parts have finite
midpoint and radius, otherwise return `false`.
"""
function isfinite(x::acb)
   return Bool(ccall((:acb_is_finite, libarb), Cint, (Ref{acb},), x))
end

@doc Markdown.doc"""
    isexact(x::acb)

Return `true` if $x$ is exact, i.e. has its real and imaginary parts have
zero radius, otherwise return `false`.
"""
function isexact(x::acb)
   return Bool(ccall((:acb_is_exact, libarb), Cint, (Ref{acb},), x))
end

@doc Markdown.doc"""
    isint(x::acb)

Return `true` if $x$ is an exact integer, otherwise return `false`.
"""
function isint(x::acb)
   return Bool(ccall((:acb_is_int, libarb), Cint, (Ref{acb},), x))
end

function isreal(x::acb)
   return Bool(ccall((:acb_is_real, libarb), Cint, (Ref{acb},), x))
end

isnegative(x::acb) = isreal(x) && isnegative(real(x))

################################################################################
#
#  Absolute value
#
################################################################################

function abs(x::acb)
  z = arb()
  ccall((:acb_abs, libarb), Nothing,
                (Ref{arb}, Ref{acb}, Int), z, x, parent(x).prec)
  z.parent = ArbField(parent(x).prec)
  return z
end

################################################################################
#
#  Inversion
#
################################################################################

function inv(x::acb)
  z = parent(x)()
  ccall((:acb_inv, libarb), Nothing, (Ref{acb}, Ref{acb}, Int), z, x, parent(x).prec)
  return z
end

################################################################################
#
#  Shifting
#
################################################################################

function ldexp(x::acb, y::Int)
  z = parent(x)()
  ccall((:acb_mul_2exp_si, libarb), Nothing,
              (Ref{acb}, Ref{acb}, Int), z, x, y)
  return z
end

function ldexp(x::acb, y::fmpz)
  z = parent(x)()
  ccall((:acb_mul_2exp_fmpz, libarb), Nothing,
              (Ref{acb}, Ref{acb}, Ref{fmpz}), z, x, y)
  return z
end

################################################################################
#
#  Miscellaneous
#
################################################################################

@doc Markdown.doc"""
    trim(x::acb)

Return an `acb` box containing $x$ but which may be more economical,
by rounding off insignificant bits from midpoints.
"""
function trim(x::acb)
  z = parent(x)()
  ccall((:acb_trim, libarb), Nothing, (Ref{acb}, Ref{acb}), z, x)
  return z
end

@doc Markdown.doc"""
    unique_integer(x::acb)

Return a pair where the first value is a boolean and the second is an `fmpz`
integer. The boolean indicates whether the box $x$ contains a unique
integer. If this is the case, the second return value is set to this unique
integer.
"""
function unique_integer(x::acb)
  z = fmpz()
  unique = ccall((:acb_get_unique_fmpz, libarb), Int,
    (Ref{fmpz}, Ref{acb}), z, x)
  return (unique != 0, z)
end

function conj(x::acb)
  z = parent(x)()
  ccall((:acb_conj, libarb), Nothing, (Ref{acb}, Ref{acb}), z, x)
  return z
end

function angle(x::acb)
  z = arb()
  ccall((:acb_arg, libarb), Nothing,
                (Ref{arb}, Ref{acb}, Int), z, x, parent(x).prec)
  z.parent = ArbField(parent(x).prec)
  return z
end

################################################################################
#
#  Constants
#
################################################################################

@doc Markdown.doc"""
    const_pi(r::AcbField)

Return $\pi = 3.14159\ldots$ as an element of $r$.
"""
function const_pi(r::AcbField)
  z = r()
  ccall((:acb_const_pi, libarb), Nothing, (Ref{acb}, Int), z, precision(r))
  return z
end

################################################################################
#
#  Complex valued functions
#
################################################################################

# complex - complex functions

function Base.sqrt(x::acb; check::Bool=true)
   z = parent(x)()
   ccall((:acb_sqrt, libarb), Nothing, (Ref{acb}, Ref{acb}, Int), z, x, parent(x).prec)
   return z
end

@doc Markdown.doc"""
    rsqrt(x::acb)

Return the reciprocal of the square root of $x$, i.e. $1/\sqrt{x}$.
"""
function rsqrt(x::acb)
   z = parent(x)()
   ccall((:acb_rsqrt, libarb), Nothing, (Ref{acb}, Ref{acb}, Int), z, x, parent(x).prec)
   return z
end

function log(x::acb)
   z = parent(x)()
   ccall((:acb_log, libarb), Nothing, (Ref{acb}, Ref{acb}, Int), z, x, parent(x).prec)
   return z
end

function log1p(x::acb)
   z = parent(x)()
   ccall((:acb_log1p, libarb), Nothing, (Ref{acb}, Ref{acb}, Int), z, x, parent(x).prec)
   return z
end

function Base.exp(x::acb)
   z = parent(x)()
   ccall((:acb_exp, libarb), Nothing, (Ref{acb}, Ref{acb}, Int), z, x, parent(x).prec)
   return z
end

function Base.expm1(x::acb)
   z = parent(x)()
   ccall((:acb_expm1, libarb), Nothing, (Ref{acb}, Ref{acb}, Int), z, x, parent(x).prec)
   return z
end

@doc Markdown.doc"""
    exppii(x::acb)

Return the exponential of $\pi i x$.
"""
function exppii(x::acb)
   z = parent(x)()
   ccall((:acb_exp_pi_i, libarb), Nothing, (Ref{acb}, Ref{acb}, Int), z, x, parent(x).prec)
   return z
end

@doc Markdown.doc"""
    root_of_unity(C::AcbField, k::Int)

Return $\exp(2\pi i/k)$.
"""
function root_of_unity(C::AcbField, k::Int)
   k <= 0 && throw(ArgumentError("Order must be positive ($k)"))
   z = C()
   ccall((:acb_unit_root, libarb), Nothing, (Ref{acb}, UInt, Int), z, k, C.prec)
   return z
end

function sin(x::acb)
   z = parent(x)()
   ccall((:acb_sin, libarb), Nothing, (Ref{acb}, Ref{acb}, Int), z, x, parent(x).prec)
   return z
end

function cos(x::acb)
   z = parent(x)()
   ccall((:acb_cos, libarb), Nothing, (Ref{acb}, Ref{acb}, Int), z, x, parent(x).prec)
   return z
end

function tan(x::acb)
   z = parent(x)()
   ccall((:acb_tan, libarb), Nothing, (Ref{acb}, Ref{acb}, Int), z, x, parent(x).prec)
   return z
end

function cot(x::acb)
   z = parent(x)()
   ccall((:acb_cot, libarb), Nothing, (Ref{acb}, Ref{acb}, Int), z, x, parent(x).prec)
   return z
end

function sinpi(x::acb)
   z = parent(x)()
   ccall((:acb_sin_pi, libarb), Nothing, (Ref{acb}, Ref{acb}, Int), z, x, parent(x).prec)
   return z
end

function cospi(x::acb)
   z = parent(x)()
   ccall((:acb_cos_pi, libarb), Nothing, (Ref{acb}, Ref{acb}, Int), z, x, parent(x).prec)
   return z
end

function tanpi(x::acb)
   z = parent(x)()
   ccall((:acb_tan_pi, libarb), Nothing, (Ref{acb}, Ref{acb}, Int), z, x, parent(x).prec)
   return z
end

function cotpi(x::acb)
   z = parent(x)()
   ccall((:acb_cot_pi, libarb), Nothing, (Ref{acb}, Ref{acb}, Int), z, x, parent(x).prec)
   return z
end

function sinh(x::acb)
   z = parent(x)()
   ccall((:acb_sinh, libarb), Nothing, (Ref{acb}, Ref{acb}, Int), z, x, parent(x).prec)
   return z
end

function cosh(x::acb)
   z = parent(x)()
   ccall((:acb_cosh, libarb), Nothing, (Ref{acb}, Ref{acb}, Int), z, x, parent(x).prec)
   return z
end

function tanh(x::acb)
   z = parent(x)()
   ccall((:acb_tanh, libarb), Nothing, (Ref{acb}, Ref{acb}, Int), z, x, parent(x).prec)
   return z
end

function coth(x::acb)
   z = parent(x)()
   ccall((:acb_coth, libarb), Nothing, (Ref{acb}, Ref{acb}, Int), z, x, parent(x).prec)
   return z
end

function atan(x::acb)
   z = parent(x)()
   ccall((:acb_atan, libarb), Nothing, (Ref{acb}, Ref{acb}, Int), z, x, parent(x).prec)
   return z
end

@doc Markdown.doc"""
    log_sinpi(x::acb)

Return $\log\sin(\pi x)$, constructed without branch cuts off the real line.
"""
function log_sinpi(x::acb)
   z = parent(x)()
   ccall((:acb_log_sin_pi, libarb), Nothing, (Ref{acb}, Ref{acb}, Int), z, x, parent(x).prec)
   return z
end

@doc Markdown.doc"""
    gamma(x::acb)

Return the Gamma function evaluated at $x$.
"""
function gamma(x::acb)
   z = parent(x)()
   ccall((:acb_gamma, libarb), Nothing, (Ref{acb}, Ref{acb}, Int), z, x, parent(x).prec)
   return z
end

@doc Markdown.doc"""
    rgamma(x::acb)

Return the reciprocal of the Gamma function evaluated at $x$.
"""
function rgamma(x::acb)
   z = parent(x)()
   ccall((:acb_rgamma, libarb), Nothing, (Ref{acb}, Ref{acb}, Int), z, x, parent(x).prec)
   return z
end

@doc Markdown.doc"""
    lgamma(x::acb)

Return the logarithm of the Gamma function evaluated at $x$.
"""
function lgamma(x::acb)
   z = parent(x)()
   ccall((:acb_lgamma, libarb), Nothing, (Ref{acb}, Ref{acb}, Int), z, x, parent(x).prec)
   return z
end

@doc Markdown.doc"""
    digamma(x::acb)

Return the  logarithmic derivative of the gamma function evaluated at $x$,
i.e. $\psi(x)$.
"""
function digamma(x::acb)
   z = parent(x)()
   ccall((:acb_digamma, libarb), Nothing, (Ref{acb}, Ref{acb}, Int), z, x, parent(x).prec)
   return z
end

@doc Markdown.doc"""
    zeta(x::acb)

Return the Riemann zeta function evaluated at $x$.
"""
function zeta(x::acb)
   z = parent(x)()
   ccall((:acb_zeta, libarb), Nothing, (Ref{acb}, Ref{acb}, Int), z, x, parent(x).prec)
   return z
end

@doc Markdown.doc"""
    barnes_g(x::acb)

Return the Barnes $G$-function, evaluated at $x$.
"""
function barnes_g(x::acb)
   z = parent(x)()
   ccall((:acb_barnes_g, libarb), Nothing, (Ref{acb}, Ref{acb}, Int), z, x, parent(x).prec)
   return z
end

@doc Markdown.doc"""
    log_barnes_g(x::acb)

Return the logarithm of the Barnes $G$-function, evaluated at $x$.
"""
function log_barnes_g(x::acb)
   z = parent(x)()
   ccall((:acb_log_barnes_g, libarb), Nothing, (Ref{acb}, Ref{acb}, Int), z, x, parent(x).prec)
   return z
end

@doc Markdown.doc"""
    agm(x::acb)

Return the arithmetic-geometric mean of $1$ and $x$.
"""
function agm(x::acb)
   z = parent(x)()
   ccall((:acb_agm1, libarb), Nothing, (Ref{acb}, Ref{acb}, Int), z, x, parent(x).prec)
   return z
end

@doc Markdown.doc"""
    erf(x::acb)

Return the error function evaluated at $x$.
"""
function erf(x::acb)
   z = parent(x)()
   ccall((:acb_hypgeom_erf, libarb), Nothing, (Ref{acb}, Ref{acb}, Int), z, x, parent(x).prec)
   return z
end

@doc Markdown.doc"""
    erfi(x::acb)

Return the imaginary error function evaluated at $x$.
"""
function erfi(x::acb)
   z = parent(x)()
   ccall((:acb_hypgeom_erfi, libarb), Nothing, (Ref{acb}, Ref{acb}, Int), z, x, parent(x).prec)
   return z
end

@doc Markdown.doc"""
    erfc(x::acb)

Return the complementary error function evaluated at $x$.
"""
function erfc(x::acb)
   z = parent(x)()
   ccall((:acb_hypgeom_erfc, libarb), Nothing, (Ref{acb}, Ref{acb}, Int), z, x, parent(x).prec)
   return z
end

@doc Markdown.doc"""
    exp_integral_ei(x::acb)

Return the exponential integral evaluated at $x$.
"""
function exp_integral_ei(x::acb)
   z = parent(x)()
   ccall((:acb_hypgeom_ei, libarb), Nothing, (Ref{acb}, Ref{acb}, Int), z, x, parent(x).prec)
   return z
end

@doc Markdown.doc"""
    sin_integral(x::acb)

Return the sine integral evaluated at $x$.
"""
function sin_integral(x::acb)
   z = parent(x)()
   ccall((:acb_hypgeom_si, libarb), Nothing, (Ref{acb}, Ref{acb}, Int), z, x, parent(x).prec)
   return z
end

@doc Markdown.doc"""
    cos_integral(x::acb)

Return the exponential cosine integral evaluated at $x$.
"""
function cos_integral(x::acb)
   z = parent(x)()
   ccall((:acb_hypgeom_ci, libarb), Nothing, (Ref{acb}, Ref{acb}, Int), z, x, parent(x).prec)
   return z
end

@doc Markdown.doc"""
    sinh_integral(x::acb)

Return the hyperbolic sine integral evaluated at $x$.
"""
function sinh_integral(x::acb)
   z = parent(x)()
   ccall((:acb_hypgeom_shi, libarb), Nothing, (Ref{acb}, Ref{acb}, Int), z, x, parent(x).prec)
   return z
end

@doc Markdown.doc"""
    cosh_integral(x::acb)

Return the hyperbolic cosine integral evaluated at $x$.
"""
function cosh_integral(x::acb)
   z = parent(x)()
   ccall((:acb_hypgeom_chi, libarb), Nothing, (Ref{acb}, Ref{acb}, Int), z, x, parent(x).prec)
   return z
end

@doc Markdown.doc"""
    dedekind_eta(x::acb)

Return the Dedekind eta function $\eta(\tau)$ at $\tau = x$.
"""
function dedekind_eta(x::acb)
   z = parent(x)()
   ccall((:acb_modular_eta, libarb), Nothing, (Ref{acb}, Ref{acb}, Int), z, x, parent(x).prec)
   return z
end

@doc Markdown.doc"""
    modular_weber_f(x::acb)

Return the modular Weber function
$\mathfrak{f}(\tau) = \frac{\eta^2(\tau)}{\eta(\tau/2)\eta(2\tau)},$
at $x$ in the complex upper half plane.
"""
function modular_weber_f(x::acb)
   x_on_2 = divexact(x, 2)
   x_times_2 = 2*x
   return divexact(dedekind_eta(x)^2, dedekind_eta(x_on_2)*dedekind_eta(x_times_2))
end

@doc Markdown.doc"""
    modular_weber_f1(x::acb)

Return the modular Weber function
$\mathfrak{f}_1(\tau) = \frac{\eta(\tau/2)}{\eta(\tau)},$
at $x$ in the complex upper half plane.
"""
function modular_weber_f1(x::acb)
   x_on_2 = divexact(x, 2)
   return divexact(dedekind_eta(x_on_2), dedekind_eta(x))
end

@doc Markdown.doc"""
    modular_weber_f2(x::acb)

Return the modular Weber function
$\mathfrak{f}_2(\tau) = \frac{\sqrt{2}\eta(2\tau)}{\eta(\tau)}$
at $x$ in the complex upper half plane.
"""
function modular_weber_f2(x::acb)
   x_times_2 = x*2
   return divexact(dedekind_eta(x_times_2), dedekind_eta(x))*sqrt(parent(x)(2))
end

@doc Markdown.doc"""
    j_invariant(x::acb)

Return the $j$-invariant $j(\tau)$ at $\tau = x$.
"""
function j_invariant(x::acb)
   z = parent(x)()
   ccall((:acb_modular_j, libarb), Nothing, (Ref{acb}, Ref{acb}, Int), z, x, parent(x).prec)
   return z
end

@doc Markdown.doc"""
    modular_lambda(x::acb)

Return the modular lambda function $\lambda(\tau)$ at $\tau = x$.
"""
function modular_lambda(x::acb)
   z = parent(x)()
   ccall((:acb_modular_lambda, libarb), Nothing, (Ref{acb}, Ref{acb}, Int), z, x, parent(x).prec)
   return z
end

@doc Markdown.doc"""
    modular_delta(x::acb)

Return the modular delta function $\Delta(\tau)$ at $\tau = x$.
"""
function modular_delta(x::acb)
   z = parent(x)()
   ccall((:acb_modular_delta, libarb), Nothing, (Ref{acb}, Ref{acb}, Int), z, x, parent(x).prec)
   return z
end

@doc Markdown.doc"""
    eisenstein_g(k::Int, x::acb)

Return the non-normalized Eisenstein series $G_k(\tau)$ of
$\mathrm{SL}_2(\mathbb{Z})$. Also defined for $\tau = i \infty$.
"""
function eisenstein_g(k::Int, x::acb)
  CC = parent(x)

  k <= 2 && error("Eisenstein series are not absolute convergent for k = $k")
  imag(x) < 0 && error("x is not in upper half plane.")
  isodd(k) && return zero(CC)
  imag(x) == Inf && return 2 * zeta(CC(k))

  len = div(k, 2) - 1
  vec = acb_vec(len)
  ccall((:acb_modular_eisenstein, libarb), Nothing,
        (Ptr{acb_struct}, Ref{acb}, Int, Int), vec, x, len, CC.prec)
  z = array(CC, vec, len)
  acb_vec_clear(vec, len)
  return z[end]
end

@doc Markdown.doc"""
    hilbert_class_polynomial(D::Int, R::FmpzPolyRing)

Return in the ring $R$ the Hilbert class polynomial of discriminant $D$,
which is only defined for $D < 0$ and $D \equiv 0, 1 \pmod 4$.
"""
function hilbert_class_polynomial(D::Int, R::FmpzPolyRing)
   D < 0 && mod(D, 4) < 2 || throw(ArgumentError("$D is not a negative discriminant"))
   z = R()
   ccall((:acb_modular_hilbert_class_poly, Nemo.libarb), Nothing,
         (Ref{fmpz_poly}, Int),
         z, D)
   return z
end

@doc Markdown.doc"""
    elliptic_k(x::acb)

Return the complete elliptic integral $K(x)$.
"""
function elliptic_k(x::acb)
   z = parent(x)()
   ccall((:acb_modular_elliptic_k, libarb), Nothing, (Ref{acb}, Ref{acb}, Int), z, x, parent(x).prec)
   return z
end

@doc Markdown.doc"""
    elliptic_e(x::acb)

Return the complete elliptic integral $E(x)$.
"""
function elliptic_e(x::acb)
   z = parent(x)()
   ccall((:acb_modular_elliptic_e, libarb), Nothing, (Ref{acb}, Ref{acb}, Int), z, x, parent(x).prec)
   return z
end

function sincos(x::acb)
  s = parent(x)()
  c = parent(x)()
  ccall((:acb_sin_cos, libarb), Nothing,
              (Ref{acb}, Ref{acb}, Ref{acb}, Int), s, c, x, parent(x).prec)
  return (s, c)
end

function sincospi(x::acb)
  s = parent(x)()
  c = parent(x)()
  ccall((:acb_sin_cos_pi, libarb), Nothing,
              (Ref{acb}, Ref{acb}, Ref{acb}, Int), s, c, x, parent(x).prec)
  return (s, c)
end

@doc Markdown.doc"""
    sinhcosh(x::acb)

Return a tuple $s, c$ consisting of the hyperbolic sine and cosine of $x$.
"""
function sinhcosh(x::acb)
  s = parent(x)()
  c = parent(x)()
  ccall((:acb_sinh_cosh, libarb), Nothing,
              (Ref{acb}, Ref{acb}, Ref{acb}, Int), s, c, x, parent(x).prec)
  return (s, c)
end

@doc Markdown.doc"""
    zeta(s::acb, a::acb)

Return the Hurwitz zeta function $\zeta(s,a)$.
"""
function zeta(s::acb, a::acb)
  z = parent(s)()
  ccall((:acb_hurwitz_zeta, libarb), Nothing,
              (Ref{acb}, Ref{acb}, Ref{acb}, Int), z, s, a, parent(s).prec)
  return z
end

@doc Markdown.doc"""
    polygamma(s::acb, a::acb)

Return the generalised polygamma function $\psi(s,z)$.
"""
function polygamma(s::acb, a::acb)
  z = parent(s)()
  ccall((:acb_polygamma, libarb), Nothing,
              (Ref{acb}, Ref{acb}, Ref{acb}, Int), z, s, a, parent(s).prec)
  return z
end

function rising_factorial(x::acb, n::UInt)
  z = parent(x)()
  ccall((:acb_rising_ui, libarb), Nothing,
              (Ref{acb}, Ref{acb}, UInt, Int), z, x, n, parent(x).prec)
  return z
end

@doc Markdown.doc"""
    rising_factorial(x::acb, n::Int)

Return the rising factorial $x(x + 1)\ldots (x + n - 1)$ as an Acb.
"""
function rising_factorial(x::acb, n::Int)
  n < 0 && throw(DomainError(n, "Argument must be non-negative"))
  return rising_factorial(x, UInt(n))
end

function rising_factorial2(x::acb, n::UInt)
  z = parent(x)()
  w = parent(x)()
  ccall((:acb_rising2_ui, libarb), Nothing,
              (Ref{acb}, Ref{acb}, Ref{acb}, UInt, Int), z, w, x, n, parent(x).prec)
  return (z, w)
end

@doc Markdown.doc"""
    rising_factorial2(x::acb, n::Int)

Return a tuple containing the rising factorial $x(x + 1)\ldots (x + n - 1)$
and its derivative.
"""
function rising_factorial2(x::acb, n::Int)
  n < 0 && throw(DomainError(n, "Argument must be non-negative"))
  return rising_factorial2(x, UInt(n))
end

function polylog(s::acb, a::acb)
  z = parent(s)()
  ccall((:acb_polylog, libarb), Nothing,
              (Ref{acb}, Ref{acb}, Ref{acb}, Int), z, s, a, parent(s).prec)
  return z
end

function polylog(s::Int, a::acb)
  z = parent(a)()
  ccall((:acb_polylog_si, libarb), Nothing,
              (Ref{acb}, Int, Ref{acb}, Int), z, s, a, parent(a).prec)
  return z
end

@doc Markdown.doc"""
    polylog(s::Union{acb,Int}, a::acb)

Return the polylogarithm Li$_s(a)$.
""" polylog(s::Union{acb,Int}, ::acb)

@doc Markdown.doc"""
    log_integral(x::acb)

Return the logarithmic integral, evaluated at $x$.
"""
function log_integral(x::acb)
  z = parent(x)()
  ccall((:acb_hypgeom_li, libarb), Nothing,
              (Ref{acb}, Ref{acb}, Int, Int), z, x, 0, parent(x).prec)
  return z
end

@doc Markdown.doc"""
    log_integral_offset(x::acb)

Return the offset logarithmic integral, evaluated at $x$.
"""
function log_integral_offset(x::acb)
  z = parent(x)()
  ccall((:acb_hypgeom_li, libarb), Nothing,
              (Ref{acb}, Ref{acb}, Int, Int), z, x, 1, parent(x).prec)
  return z
end

@doc Markdown.doc"""
    exp_integral_e(s::acb, x::acb)

Return the generalised exponential integral $E_s(x)$.
"""
function exp_integral_e(s::acb, x::acb)
  z = parent(s)()
  ccall((:acb_hypgeom_expint, libarb), Nothing,
              (Ref{acb}, Ref{acb}, Ref{acb}, Int), z, s, x, parent(s).prec)
  return z
end

@doc Markdown.doc"""
    gamma(s::acb, x::acb)

Return the upper incomplete gamma function $\Gamma(s,x)$.
"""
function gamma(s::acb, x::acb)
  z = parent(s)()
  ccall((:acb_hypgeom_gamma_upper, libarb), Nothing,
        (Ref{acb}, Ref{acb}, Ref{acb}, Int, Int), z, s, x, 0, parent(s).prec)
  return z
end

@doc Markdown.doc"""
    gamma_regularized(s::acb, x::acb)

Return the regularized upper incomplete gamma function
$\Gamma(s,x) / \Gamma(s)$.
"""
function gamma_regularized(s::acb, x::acb)
  z = parent(s)()
  ccall((:acb_hypgeom_gamma_upper, libarb), Nothing,
        (Ref{acb}, Ref{acb}, Ref{acb}, Int, Int), z, s, x, 1, parent(s).prec)
  return z
end

@doc Markdown.doc"""
    gamma_lower(s::acb, x::acb)

Return the lower incomplete gamma function $\gamma(s,x) / \Gamma(s)$.
"""
function gamma_lower(s::acb, x::acb)
  z = parent(s)()
  ccall((:acb_hypgeom_gamma_lower, libarb), Nothing,
        (Ref{acb}, Ref{acb}, Ref{acb}, Int, Int), z, s, x, 0, parent(s).prec)
  return z
end

@doc Markdown.doc"""
    gamma_lower_regularized(s::acb, x::acb)

Return the regularized lower incomplete gamma function
$\gamma(s,x) / \Gamma(s)$.
"""
function gamma_lower_regularized(s::acb, x::acb)
  z = parent(s)()
  ccall((:acb_hypgeom_gamma_lower, libarb), Nothing,
        (Ref{acb}, Ref{acb}, Ref{acb}, Int, Int), z, s, x, 1, parent(s).prec)
  return z
end

@doc Markdown.doc"""
    bessel_j(nu::acb, x::acb)

Return the Bessel function $J_{\nu}(x)$.
"""
function bessel_j(nu::acb, x::acb)
  z = parent(x)()
  ccall((:acb_hypgeom_bessel_j, libarb), Nothing,
              (Ref{acb}, Ref{acb}, Ref{acb}, Int), z, nu, x, parent(x).prec)
  return z
end

@doc Markdown.doc"""
    bessel_y(nu::acb, x::acb)

Return the Bessel function $Y_{\nu}(x)$.
"""
function bessel_y(nu::acb, x::acb)
  z = parent(x)()
  ccall((:acb_hypgeom_bessel_y, libarb), Nothing,
              (Ref{acb}, Ref{acb}, Ref{acb}, Int), z, nu, x, parent(x).prec)
  return z
end

@doc Markdown.doc"""
    bessel_i(nu::acb, x::acb)

Return the Bessel function $I_{\nu}(x)$.
"""
function bessel_i(nu::acb, x::acb)
  z = parent(x)()
  ccall((:acb_hypgeom_bessel_i, libarb), Nothing,
              (Ref{acb}, Ref{acb}, Ref{acb}, Int), z, nu, x, parent(x).prec)
  return z
end

@doc Markdown.doc"""
    bessel_k(nu::acb, x::acb)

Return the Bessel function $K_{\nu}(x)$.
"""
function bessel_k(nu::acb, x::acb)
  z = parent(x)()
  ccall((:acb_hypgeom_bessel_k, libarb), Nothing,
              (Ref{acb}, Ref{acb}, Ref{acb}, Int), z, nu, x, parent(x).prec)
  return z
end

@doc Markdown.doc"""
    airy_ai(x::acb)

Return the Airy function $\operatorname{Ai}(x)$.
"""
function airy_ai(x::acb)
  ai = parent(x)()
  ccall((:acb_hypgeom_airy, libarb), Nothing,
              (Ref{acb}, Ptr{Cvoid}, Ptr{Cvoid}, Ptr{Cvoid}, Ref{acb}, Int),
              ai, C_NULL, C_NULL, C_NULL, x, parent(x).prec)
  return ai
end

@doc Markdown.doc"""
    airy_bi(x::acb)

Return the Airy function $\operatorname{Bi}(x)$.
"""
function airy_bi(x::acb)
  bi = parent(x)()
  ccall((:acb_hypgeom_airy, libarb), Nothing,
              (Ptr{Cvoid}, Ptr{Cvoid}, Ref{acb}, Ptr{Cvoid}, Ref{acb}, Int),
              C_NULL, C_NULL, bi, C_NULL, x, parent(x).prec)
  return bi
end

@doc Markdown.doc"""
    airy_ai_prime(x::acb)

Return the derivative of the Airy function $\operatorname{Ai}^\prime(x)$.
"""
function airy_ai_prime(x::acb)
  ai_prime = parent(x)()
  ccall((:acb_hypgeom_airy, libarb), Nothing,
              (Ptr{Cvoid}, Ref{acb}, Ptr{Cvoid}, Ptr{Cvoid}, Ref{acb}, Int),
              C_NULL, ai_prime, C_NULL, C_NULL, x, parent(x).prec)
  return ai_prime
end

@doc Markdown.doc"""
    airy_bi_prime(x::acb)

Return the derivative of the Airy function $\operatorname{Bi}^\prime(x)$.
"""
function airy_bi_prime(x::acb)
  bi_prime = parent(x)()
  ccall((:acb_hypgeom_airy, libarb), Nothing,
              (Ptr{Cvoid}, Ptr{Cvoid}, Ptr{Cvoid}, Ref{acb}, Ref{acb}, Int),
              C_NULL, C_NULL, C_NULL, bi_prime, x, parent(x).prec)
  return bi_prime
end

@doc Markdown.doc"""
    hypergeometric_1f1(a::acb, b::acb, x::acb)

Return the confluent hypergeometric function ${}_1F_1(a,b,x)$.
"""
function hypergeometric_1f1(a::acb, b::acb, x::acb)
  z = parent(x)()
  ccall((:acb_hypgeom_m, libarb), Nothing,
              (Ref{acb}, Ref{acb}, Ref{acb}, Ref{acb}, Int, Int), z, a, b, x, 0, parent(x).prec)
  return z
end

@doc Markdown.doc"""
    hypergeometric_1f1_regularized(a::acb, b::acb, x::acb)

Return the regularized confluent hypergeometric function
${}_1F_1(a,b,x) / \Gamma(b)$.
"""
function hypergeometric_1f1_regularized(a::acb, b::acb, x::acb)
  z = parent(x)()
  ccall((:acb_hypgeom_m, libarb), Nothing,
              (Ref{acb}, Ref{acb}, Ref{acb}, Ref{acb}, Int, Int), z, a, b, x, 1, parent(x).prec)
  return z
end

@doc Markdown.doc"""
    hypergeometric_u(a::acb, b::acb, x::acb)

Return the confluent hypergeometric function $U(a,b,x)$.
"""
function hypergeometric_u(a::acb, b::acb, x::acb)
  z = parent(x)()
  ccall((:acb_hypgeom_u, libarb), Nothing,
              (Ref{acb}, Ref{acb}, Ref{acb}, Ref{acb}, Int), z, a, b, x, parent(x).prec)
  return z
end

@doc Markdown.doc"""
    hypergeometric_2f1(a::acb, b::acb, c::acb, x::acb; flags=0)

Return the Gauss hypergeometric function ${}_2F_1(a,b,c,x)$.
"""
function hypergeometric_2f1(a::acb, b::acb, c::acb, x::acb; flags=0)
  z = parent(x)()
  ccall((:acb_hypgeom_2f1, libarb), Nothing,
              (Ref{acb}, Ref{acb}, Ref{acb}, Ref{acb}, Ref{acb}, Int, Int), z, a, b, c, x, flags, parent(x).prec)
  return z
end

@doc Markdown.doc"""
    jacobi_theta(z::acb, tau::acb)

Return a tuple of four elements containing the Jacobi theta function values
$\theta_1, \theta_2, \theta_3, \theta_4$ evaluated at $z, \tau$.
"""
function jacobi_theta(z::acb, tau::acb)
  t1 = parent(z)()
  t2 = parent(z)()
  t3 = parent(z)()
  t4 = parent(z)()
  ccall((:acb_modular_theta, libarb), Nothing,
              (Ref{acb}, Ref{acb}, Ref{acb}, Ref{acb}, Ref{acb}, Ref{acb}, Int),
                t1, t2, t3, t4, z, tau, parent(z).prec)
  return (t1, t2, t3, t4)
end

@doc Markdown.doc"""
    weierstrass_p(z::acb, tau::acb)

Return the Weierstrass elliptic function $\wp(z,\tau)$.
"""
function weierstrass_p(z::acb, tau::acb)
  r = parent(z)()
  ccall((:acb_elliptic_p, libarb), Nothing,
              (Ref{acb}, Ref{acb}, Ref{acb}, Int), r, z, tau, parent(z).prec)
  return r
end

@doc Markdown.doc"""
    weierstrass_p_prime(z::acb, tau::acb)

Return the derivative of the Weierstrass elliptic function $\frac{\partial}{\partial z}\wp(z,\tau)$.
"""
function weierstrass_p_prime(z::acb, tau::acb)
  r = parent(z)()
  ccall((:acb_elliptic_p_prime, libarb), Nothing,
              (Ref{acb}, Ref{acb}, Ref{acb}, Int), r, z, tau, parent(z).prec)
  return r
end

@doc Markdown.doc"""
    agm(x::acb, y::acb)

Return the arithmetic-geometric mean of $x$ and $y$.
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

@doc Markdown.doc"""
    lindep(A::Vector{acb}, bits::Int)

Find a small linear combination of the entries of the array $A$ that is small
(using LLL). The entries are first scaled by the given number of bits before
truncating the real and imaginary parts to integers for use in LLL. This function can
be used to find linear dependence between a list of complex numbers. The algorithm is
heuristic only and returns an array of Nemo integers representing the linear
combination.
"""
function lindep(A::Vector{acb}, bits::Int)
  bits < 0 && throw(DomainError(bits, "Number of bits must be non-negative"))
  n = length(A)
  V = [ldexp(s, bits) for s in A]
  M = zero_matrix(ZZ, n, n + 2)
  for i = 1:n
    M[i, i] = ZZ(1)
    flag, M[i, n + 1] = unique_integer(floor(real(V[i]) + 0.5))
    !flag && error("Insufficient precision in lindep")
    flag, M[i, n + 2] = unique_integer(floor(imag(V[i]) + 0.5))
    !flag && error("Insufficient precision in lindep")
  end
  L = lll(M)
  return [L[1, i] for i = 1:n]
end

@doc Markdown.doc"""
    lindep(A::Matrix{acb}, bits::Int)

Find a (common) small linear combination of the entries in each row of the array $A$,
that is small (using LLL). It is assumed that the complex numbers in each row of the
array share the same linear combination. The entries are first scaled by the given
number of bits before truncating the real and imaginary parts to integers for use in
LLL. This function can be used to find a common linear dependence shared across a
number of lists of complex numbers. The algorithm is heuristic only and returns an
array of Nemo integers representing the common linear combination.
"""
function lindep(A::Matrix{acb}, bits::Int)
  bits < 0 && throw(DomainError(bits, "Number of bits must be non-negative"))
  m, n = size(A)
  V = [ldexp(s, bits) for s in A]
  M = zero_matrix(ZZ, n, n + 2*m)
  for i = 1:n
     M[i, i] = ZZ(1)
  end
  for j = 1:m
     for i = 1:n
        flag, M[i, n + 2*j - 1] = unique_integer(floor(real(V[j, i]) + 0.5))
        !flag && error("Insufficient precision in lindep")
        flag, M[i, n + 2*j] = unique_integer(floor(imag(V[j, i]) + 0.5))
        !flag && error("Insufficient precision in lindep")
     end
  end
  L = lll(M)
  return [L[1, i] for i = 1:n]
end

################################################################################
#
#  Unsafe arithmetic
#
################################################################################

function zero!(z::acb)
   ccall((:acb_zero, libarb), Nothing, (Ref{acb},), z)
   return z
end

function add!(z::acb, x::acb, y::acb)
  ccall((:acb_add, libarb), Nothing, (Ref{acb}, Ref{acb}, Ref{acb}, Int),
         z, x, y, parent(z).prec)
  return z
end

function addeq!(z::acb, y::acb)
  ccall((:acb_add, libarb), Nothing, (Ref{acb}, Ref{acb}, Ref{acb}, Int),
         z, z, y, parent(z).prec)
  return z
end

function sub!(z::acb, x::acb, y::acb)
  ccall((:acb_sub, libarb), Nothing, (Ref{acb}, Ref{acb}, Ref{acb}, Int),
        z, x, y, parent(z).prec)
  return z
end

function mul!(z::acb, x::acb, y::acb)
  ccall((:acb_mul, libarb), Nothing, (Ref{acb}, Ref{acb}, Ref{acb}, Int),
        z, x, y, parent(z).prec)
  return z
end

function div!(z::acb, x::acb, y::acb)
  ccall((:acb_div, libarb), Nothing, (Ref{acb}, Ref{acb}, Ref{acb}, Int),
        z, x, y, parent(z).prec)
  return z
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
        ccall(($f, libarb), Nothing, (($passtoc), ($t)), x, y)
      end

      function _acb_set(x::($typeofx), y::($t), p::Int)
        _acb_set(x, y)
        ccall((:acb_set_round, libarb), Nothing,
                    (($passtoc), ($passtoc), Int), x, x, p)
      end
    end
  end

  @eval begin
    function _acb_set(x::($typeofx), y::fmpz)
      ccall((:acb_set_fmpz, libarb), Nothing, (($passtoc), Ref{fmpz}), x, y)
    end

    function _acb_set(x::($typeofx), y::fmpz, p::Int)
      ccall((:acb_set_round_fmpz, libarb), Nothing,
                  (($passtoc), Ref{fmpz}, Int), x, y, p)
    end

    function _acb_set(x::($typeofx), y::fmpq, p::Int)
      ccall((:acb_set_fmpq, libarb), Nothing,
                  (($passtoc), Ref{fmpq}, Int), x, y, p)
    end

    function _acb_set(x::($typeofx), y::arb)
      ccall((:acb_set_arb, libarb), Nothing, (($passtoc), Ref{arb}), x, y)
    end

    function _acb_set(x::($typeofx), y::arb, p::Int)
      _acb_set(x, y)
      ccall((:acb_set_round, libarb), Nothing,
                  (($passtoc), ($passtoc), Int), x, x, p)
    end

    function _acb_set(x::($typeofx), y::acb)
      ccall((:acb_set, libarb), Nothing, (($passtoc), Ref{acb}), x, y)
    end

    function _acb_set(x::($typeofx), y::acb, p::Int)
      ccall((:acb_set_round, libarb), Nothing,
                  (($passtoc), Ref{acb}, Int), x, y, p)
    end

    function _acb_set(x::($typeofx), y::AbstractString, p::Int)
      r = ccall((:acb_real_ptr, libarb), Ptr{arb}, (($passtoc), ), x)
      _arb_set(r, y, p)
      i = ccall((:acb_imag_ptr, libarb), Ptr{arb}, (($passtoc), ), x)
      ccall((:arb_zero, libarb), Nothing, (Ptr{arb}, ), i)
    end

    function _acb_set(x::($typeofx), y::BigFloat)
      r = ccall((:acb_real_ptr, libarb), Ptr{arb}, (($passtoc), ), x)
      _arb_set(r, y)
      i = ccall((:acb_imag_ptr, libarb), Ptr{arb}, (($passtoc), ), x)
      ccall((:arb_zero, libarb), Nothing, (Ptr{arb}, ), i)
    end

    function _acb_set(x::($typeofx), y::BigFloat, p::Int)
      r = ccall((:acb_real_ptr, libarb), Ptr{arb}, (($passtoc), ), x)
      _arb_set(r, y, p)
      i = ccall((:acb_imag_ptr, libarb), Ptr{arb}, (($passtoc), ), x)
      ccall((:arb_zero, libarb), Nothing, (Ptr{arb}, ), i)
    end

    function _acb_set(x::($typeofx), y::Int, z::Int, p::Int)
      ccall((:acb_set_si_si, libarb), Nothing,
                  (($passtoc), Int, Int), x, y, z)
      ccall((:acb_set_round, libarb), Nothing,
                  (($passtoc), ($passtoc), Int), x, x, p)
    end

    function _acb_set(x::($typeofx), y::arb, z::arb)
      ccall((:acb_set_arb_arb, libarb), Nothing,
                  (($passtoc), Ref{arb}, Ref{arb}), x, y, z)
    end

    function _acb_set(x::($typeofx), y::arb, z::arb, p::Int)
      _acb_set(x, y, z)
      ccall((:acb_set_round, libarb), Nothing,
                  (($passtoc), ($passtoc), Int), x, x, p)
    end

    function _acb_set(x::($typeofx), y::fmpq, z::fmpq, p::Int)
      r = ccall((:acb_real_ptr, libarb), Ptr{arb}, (($passtoc), ), x)
      _arb_set(r, y, p)
      i = ccall((:acb_imag_ptr, libarb), Ptr{arb}, (($passtoc), ), x)
      _arb_set(i, z, p)
    end

    function _acb_set(x::($typeofx), y::T, z::T, p::Int) where {T <: AbstractString}
      r = ccall((:acb_real_ptr, libarb), Ptr{arb}, (($passtoc), ), x)
      _arb_set(r, y, p)
      i = ccall((:acb_imag_ptr, libarb), Ptr{arb}, (($passtoc), ), x)
      _arb_set(i, z, p)
    end

  end

  for T in (Float64, BigFloat, UInt, fmpz)
    @eval begin
      function _acb_set(x::($typeofx), y::($T), z::($T))
        r = ccall((:acb_real_ptr, libarb), Ptr{arb}, (($passtoc), ), x)
        _arb_set(r, y)
        i = ccall((:acb_imag_ptr, libarb), Ptr{arb}, (($passtoc), ), x)
        _arb_set(i, z)
      end

      function _acb_set(x::($typeofx), y::($T), z::($T), p::Int)
        r = ccall((:acb_real_ptr, libarb), Ptr{arb}, (($passtoc), ), x)
        _arb_set(r, y, p)
        i = ccall((:acb_imag_ptr, libarb), Ptr{arb}, (($passtoc), ), x)
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

(r::AcbField)(x::Rational{T}) where {T <: Integer} = r(fmpq(x))

function (r::AcbField)(x::T, y::T) where {T <: Union{Int, UInt, fmpz, fmpq, arb, Float64, BigFloat, AbstractString}}
  z = acb(x, y, r.prec)
  z.parent = r
  return z
end

for S in (Int, UInt, fmpz, fmpq, arb, Float64, BigFloat, AbstractString, BigInt)
  for T in (Int, UInt, fmpz, fmpq, arb, Float64, BigFloat, AbstractString, BigInt)
    if S != T
      @eval begin
        function (r::AcbField)(x::$(S), y::$(T))
          RR = ArbField(r.prec, cached = false)
          z = acb(RR(x), RR(y), r.prec)
          z.parent = r
          return z
        end
      end
    end
  end
end

for T in (Int, UInt, fmpz, fmpq, arb, Float64, BigFloat, AbstractString, BigInt)
  @eval begin
    function (r::AcbField)(x::Rational{S}, y::$(T)) where {S <: Integer}
      RR = ArbField(r.prec, cached = false)
      z = acb(RR(x), RR(y), r.prec)
      z.parent = r
      return z
    end
    function (r::AcbField)(x::$(T), y::Rational{S}) where {S <: Integer}
      RR = ArbField(r.prec, cached = false)
      z = acb(RR(x), RR(y), r.prec)
      z.parent = r
      return z
    end
  end
end

(r::AcbField)(x::BigInt, y::BigInt) = r(fmpz(x), fmpz(y))

(r::AcbField)(x::Rational{S}, y::Rational{T}) where {S <: Integer, T <: Integer} =
      r(fmpq(x), fmpq(y))

################################################################################
#
#  AcbField constructor
#
################################################################################

# see internal constructor
