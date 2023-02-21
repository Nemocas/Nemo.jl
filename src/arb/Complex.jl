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

import Base: cispi

export one, onei, real, imag, conj, abs, inv, angle, isreal, polygamma, erf,
       erfi, erfc, bessel_j, bessel_k, bessel_i, bessel_y, airy_ai, airy_bi,
       airy_ai_prime, airy_bi_prime

export rsqrt, log, log1p, cispi, sin, cos, tan, cot, sinpi, cospi, tanpi,
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

elem_type(::Type{ComplexField}) = ComplexElem

parent_type(::Type{ComplexElem}) = ComplexField

base_ring(R::ComplexField) = Union{}

base_ring(a::ComplexElem) = Union{}

parent(x::ComplexElem) = ComplexField()

is_domain_type(::Type{ComplexElem}) = true

is_exact_type(::Type{ComplexElem}) = false

function zero(r::ComplexField)
  z = ComplexElem()
  return z
end

function one(r::ComplexField)
  z = ComplexElem()
  ccall((:acb_one, libarb), Nothing, (Ref{ComplexElem}, ), z)
  return z
end

@doc Markdown.doc"""
    onei(r::ComplexField)

Return exact one times $i$ in the given Arb complex field.
"""
function onei(r::ComplexField)
  z = ComplexElem()
  ccall((:acb_onei, libarb), Nothing, (Ref{ComplexElem}, ), z)
  return z
end

@doc Markdown.doc"""
    accuracy_bits(x::ComplexElem)

Return the relative accuracy of $x$ measured in bits, capped between
`typemax(Int)` and `-typemax(Int)`.
"""
function accuracy_bits(x::ComplexElem)
  # bug in acb.h: rel_accuracy_bits is not in the library
  return -ccall((:acb_rel_error_bits, libarb), Int, (Ref{ComplexElem},), x)
end

function deepcopy_internal(a::ComplexElem, dict::IdDict)
  b = ComplexElem()
  ccall((:acb_set, libarb), Nothing, (Ref{ComplexElem}, Ref{ComplexElem}), b, a)
  return b
end

function canonical_unit(x::ComplexElem)
   return x
end

# TODO: implement hash

function check_parent(a::ComplexElem, b::ComplexElem)
   return true
end

characteristic(::ComplexField) = 0

################################################################################
#
#  Conversions
#
################################################################################

function convert(::Type{ComplexF64}, x::ComplexElem)
    GC.@preserve x begin
      re = ccall((:acb_real_ptr, libarb), Ptr{arb_struct}, (Ref{ComplexElem}, ), x)
      im = ccall((:acb_imag_ptr, libarb), Ptr{arb_struct}, (Ref{ComplexElem}, ), x)
      t = ccall((:arb_mid_ptr, libarb), Ptr{arf_struct}, (Ptr{RealElem}, ), re)
      u = ccall((:arb_mid_ptr, libarb), Ptr{arf_struct}, (Ptr{RealElem}, ), im)
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

function real(x::ComplexElem)
  z = RealElem()
  ccall((:acb_get_real, libarb), Nothing, (Ref{RealElem}, Ref{ComplexElem}), z, x)
  return z
end

function imag(x::ComplexElem)
  z = RealElem()
  ccall((:acb_get_imag, libarb), Nothing, (Ref{RealElem}, Ref{ComplexElem}), z, x)
  return z
end

################################################################################
#
#  String I/O
#
################################################################################

function expressify(z::ComplexElem; context = nothing)
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

function Base.show(io::IO, ::MIME"text/plain", z::ComplexElem)
   print(io, AbstractAlgebra.obj_to_string(z, context = io))
end

function Base.show(io::IO, z::ComplexElem)
   print(io, AbstractAlgebra.obj_to_string(z, context = io))
end

function show(io::IO, x::ComplexField)
  print(io, "Complex Field")# with ")
  #print(io, precision(x))
  #print(io, " bits of precision and error bounds")
end

################################################################################
#
#  Unary operations
#
################################################################################

function -(x::ComplexElem)
  z = ComplexElem()
  ccall((:acb_neg, libarb), Nothing, (Ref{ComplexElem}, Ref{ComplexElem}), z, x)
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
    function ($s)(x::ComplexElem, y::ComplexElem, prec::Int = precision(Balls))
      z = ComplexElem()
      ccall(($f, libarb), Nothing, (Ref{ComplexElem}, Ref{ComplexElem}, Ref{ComplexElem}, Int),
                           z, x, y, prec)
      return z
    end
  end
end

for (f,s) in ((:+, "add"), (:-, "sub"), (:*, "mul"), (://, "div"), (:^, "pow"))
  @eval begin

    function ($f)(x::ComplexElem, y::UInt, prec::Int = precision(Balls))
      z = ComplexElem()
      ccall(($("acb_"*s*"_ui"), libarb), Nothing,
                  (Ref{ComplexElem}, Ref{ComplexElem}, UInt, Int),
                  z, x, y, prec)
      return z
    end

    function ($f)(x::ComplexElem, y::Int, prec::Int = precision(Balls))
      z = ComplexElem()
      ccall(($("acb_"*s*"_si"), libarb), Nothing,
      (Ref{ComplexElem}, Ref{ComplexElem}, Int, Int), z, x, y, prec)
      return z
    end

    function ($f)(x::ComplexElem, y::ZZRingElem, prec::Int = precision(Balls))
      z = ComplexElem()
      ccall(($("acb_"*s*"_fmpz"), libarb), Nothing,
                  (Ref{ComplexElem}, Ref{ComplexElem}, Ref{ZZRingElem}, Int),
                  z, x, y, prec)
      return z
    end

    function ($f)(x::ComplexElem, y::RealElem, prec::Int = precision(Balls))
      z = ComplexElem()
      ccall(($("acb_"*s*"_arb"), libarb), Nothing,
                  (Ref{ComplexElem}, Ref{ComplexElem}, Ref{RealElem}, Int),
                  z, x, y, prec)
      return z
    end
  end
end


+(x::UInt,y::ComplexElem) = +(y,x)
+(x::Int,y::ComplexElem) = +(y,x)
+(x::ZZRingElem,y::ComplexElem) = +(y,x)
+(x::RealElem,y::ComplexElem) = +(y,x)

*(x::UInt,y::ComplexElem) = *(y,x)
*(x::Int,y::ComplexElem) = *(y,x)
*(x::ZZRingElem,y::ComplexElem) = *(y,x)
*(x::RealElem,y::ComplexElem) = *(y,x)

//(x::UInt,y::ComplexElem) = (x == 1) ? inv(y) : parent(y)(x) // y
//(x::Int,y::ComplexElem) = (x == 1) ? inv(y) : parent(y)(x) // y
//(x::ZZRingElem,y::ComplexElem) = isone(x) ? inv(y) : parent(y)(x) // y
//(x::RealElem,y::ComplexElem) = isone(x) ? inv(y) : parent(y)(x) // y

^(x::UInt,y::ComplexElem) = parent(y)(x) ^ y
^(x::Int,y::ComplexElem) = parent(y)(x) ^ y
^(x::ZZRingElem,y::ComplexElem) = parent(y)(x) ^ y
^(x::RealElem,y::ComplexElem) = parent(y)(x) ^ y
^(x::Integer, y::ComplexElem) = ZZRingElem(x)^y

function -(x::UInt, y::ComplexElem)
  z = ComplexElem()
  ccall((:acb_sub_ui, libarb), Nothing, (Ref{ComplexElem}, Ref{ComplexElem}, UInt, Int), z, y, x, precision(Balls))
  ccall((:acb_neg, libarb), Nothing, (Ref{ComplexElem}, Ref{ComplexElem}), z, z)
  return z
end

function -(x::Int, y::ComplexElem)
  z = ComplexElem()
  ccall((:acb_sub_si, libarb), Nothing, (Ref{ComplexElem}, Ref{ComplexElem}, Int, Int), z, y, x, precision(Balls))
  ccall((:acb_neg, libarb), Nothing, (Ref{ComplexElem}, Ref{ComplexElem}), z, z)
  return z
end

function -(x::ZZRingElem, y::ComplexElem)
  z = ComplexElem()
  ccall((:acb_sub_fmpz, libarb), Nothing, (Ref{ComplexElem}, Ref{ComplexElem}, Ref{ZZRingElem}, Int), z, y, x, precision(Balls))
  ccall((:acb_neg, libarb), Nothing, (Ref{ComplexElem}, Ref{ComplexElem}), z, z)
  return z
end

function -(x::RealElem, y::ComplexElem)
  z = ComplexElem()
  ccall((:acb_sub_arb, libarb), Nothing, (Ref{ComplexElem}, Ref{ComplexElem}, Ref{RealElem}, Int), z, y, x, precision(Balls))
  ccall((:acb_neg, libarb), Nothing, (Ref{ComplexElem}, Ref{ComplexElem}), z, z)
  return z
end

+(x::ComplexElem, y::Integer) = x + ZZRingElem(y)

-(x::ComplexElem, y::Integer) = x - ZZRingElem(y)

*(x::ComplexElem, y::Integer) = x*ZZRingElem(y)

//(x::ComplexElem, y::Integer) = x//ZZRingElem(y)

+(x::Integer, y::ComplexElem) = ZZRingElem(x) + y

-(x::Integer, y::ComplexElem) = ZZRingElem(x) - y

*(x::Integer, y::ComplexElem) = ZZRingElem(x)*y

//(x::Integer, y::ComplexElem) = ZZRingElem(x)//y

^(x::ComplexElem, y::Integer) = x ^ parent(x)(y)

+(x::ComplexElem, y::QQFieldElem) = x + parent(x)(y)
-(x::ComplexElem, y::QQFieldElem) = x - parent(x)(y)
*(x::ComplexElem, y::QQFieldElem) = x * parent(x)(y)
//(x::ComplexElem, y::QQFieldElem) = x // parent(x)(y)
^(x::ComplexElem, y::QQFieldElem) = x ^ parent(x)(y)

+(x::QQFieldElem, y::ComplexElem) = parent(y)(x) + y
-(x::QQFieldElem, y::ComplexElem) = parent(y)(x) - y
*(x::QQFieldElem, y::ComplexElem) = parent(y)(x) * y
//(x::QQFieldElem, y::ComplexElem) = parent(y)(x) // y
^(x::QQFieldElem, y::ComplexElem) = parent(y)(x) ^ y

divexact(x::ComplexElem, y::ComplexElem; check::Bool=true) = x // y
divexact(x::ZZRingElem, y::ComplexElem; check::Bool=true) = x // y
divexact(x::ComplexElem, y::ZZRingElem; check::Bool=true) = x // y
divexact(x::Int, y::ComplexElem; check::Bool=true) = x // y
divexact(x::ComplexElem, y::Int; check::Bool=true) = x // y
divexact(x::UInt, y::ComplexElem; check::Bool=true) = x // y
divexact(x::ComplexElem, y::UInt; check::Bool=true) = x // y
divexact(x::QQFieldElem, y::ComplexElem; check::Bool=true) = x // y
divexact(x::ComplexElem, y::QQFieldElem; check::Bool=true) = x // y
divexact(x::RealElem, y::ComplexElem; check::Bool=true) = x // y
divexact(x::ComplexElem, y::RealElem; check::Bool=true) = x // y
divexact(x::Float64, y::ComplexElem; check::Bool=true) = x // y
divexact(x::ComplexElem, y::Float64; check::Bool=true) = x // y
divexact(x::BigFloat, y::ComplexElem; check::Bool=true) = x // y
divexact(x::ComplexElem, y::BigFloat; check::Bool=true) = x // y
divexact(x::Integer, y::ComplexElem; check::Bool=true) = x // y
divexact(x::ComplexElem, y::Integer; check::Bool=true) = x // y
divexact(x::Rational{T}, y::ComplexElem; check::Bool=true) where {T <: Integer} = x // y
divexact(x::ComplexElem, y::Rational{T}; check::Bool=true) where {T <: Integer} = x // y

/(x::ComplexElem, y::ComplexElem) = x // y
/(x::ZZRingElem, y::ComplexElem) = x // y
/(x::ComplexElem, y::ZZRingElem) = x // y
/(x::Int, y::ComplexElem) = x // y
/(x::ComplexElem, y::Int) = x // y
/(x::UInt, y::ComplexElem) = x // y
/(x::ComplexElem, y::UInt) = x // y
/(x::QQFieldElem, y::ComplexElem) = x // y
/(x::ComplexElem, y::QQFieldElem) = x // y
/(x::RealElem, y::ComplexElem) = x // y
/(x::ComplexElem, y::RealElem) = x // y

+(x::Rational{T}, y::ComplexElem) where {T <: Integer} = QQFieldElem(x) + y
+(x::ComplexElem, y::Rational{T}) where {T <: Integer} = x + QQFieldElem(y)
-(x::Rational{T}, y::ComplexElem) where {T <: Integer} = QQFieldElem(x) - y
-(x::ComplexElem, y::Rational{T}) where {T <: Integer} = x - QQFieldElem(y)
*(x::Rational{T}, y::ComplexElem) where {T <: Integer} = QQFieldElem(x) * y
*(x::ComplexElem, y::Rational{T}) where {T <: Integer} = x * QQFieldElem(y)
//(x::Rational{T}, y::ComplexElem) where {T <: Integer} = QQFieldElem(x) // y
//(x::ComplexElem, y::Rational{T}) where {T <: Integer} = x // QQFieldElem(y)
^(x::Rational{T}, y::ComplexElem) where {T <: Integer} = QQFieldElem(x)^y
^(x::ComplexElem, y::Rational{T}) where {T <: Integer} = x ^ QQFieldElem(y)

+(x::Float64, y::ComplexElem) = parent(y)(x) + y
+(x::ComplexElem, y::Float64) = x + parent(x)(y)
-(x::Float64, y::ComplexElem) = parent(y)(x) - y
-(x::ComplexElem, y::Float64) = x - parent(x)(y)
*(x::Float64, y::ComplexElem) = parent(y)(x) * y
*(x::ComplexElem, y::Float64) = x * parent(x)(y)
//(x::Float64, y::ComplexElem) = parent(y)(x) // y
//(x::ComplexElem, y::Float64) = x // parent(x)(y)
^(x::Float64, y::ComplexElem) = parent(y)(x)^y
^(x::ComplexElem, y::Float64) = x ^ parent(x)(y)

+(x::BigFloat, y::ComplexElem) = parent(y)(x) + y
+(x::ComplexElem, y::BigFloat) = x + parent(x)(y)
-(x::BigFloat, y::ComplexElem) = parent(y)(x) - y
-(x::ComplexElem, y::BigFloat) = x - parent(x)(y)
*(x::BigFloat, y::ComplexElem) = parent(y)(x) * y
*(x::ComplexElem, y::BigFloat) = x * parent(x)(y)
//(x::BigFloat, y::ComplexElem) = parent(y)(x) // y
//(x::ComplexElem, y::BigFloat) = x // parent(x)(y)
^(x::BigFloat, y::ComplexElem) = parent(y)(x)^y
^(x::ComplexElem, y::BigFloat) = x ^ parent(x)(y)

################################################################################
#
#  Comparison
#
################################################################################

@doc Markdown.doc"""
    isequal(x::ComplexElem, y::ComplexElem)

Return `true` if the boxes $x$ and $y$ are precisely equal, i.e. their real
and imaginary parts have the same midpoints and radii.
"""
function isequal(x::ComplexElem, y::ComplexElem)
  r = ccall((:acb_equal, libarb), Cint, (Ref{ComplexElem}, Ref{ComplexElem}), x, y)
  return Bool(r)
end

function ==(x::ComplexElem, y::ComplexElem)
  r = ccall((:acb_eq, libarb), Cint, (Ref{ComplexElem}, Ref{ComplexElem}), x, y)
  return Bool(r)
end

function !=(x::ComplexElem, y::ComplexElem)
  r = ccall((:acb_ne, libarb), Cint, (Ref{ComplexElem}, Ref{ComplexElem}), x, y)
  return Bool(r)
end

==(x::ComplexElem,y::Int) = (x == parent(x)(y))
==(x::Int,y::ComplexElem) = (y == parent(y)(x))

==(x::ComplexElem,y::RealElem) = (x == parent(x)(y))
==(x::RealElem,y::ComplexElem) = (y == parent(y)(x))

==(x::ComplexElem,y::ZZRingElem) = (x == parent(x)(y))
==(x::ZZRingElem,y::ComplexElem) = (y == parent(y)(x))

==(x::ComplexElem,y::Integer) = x == ZZRingElem(y)
==(x::Integer,y::ComplexElem) = ZZRingElem(x) == y

==(x::ComplexElem,y::Float64) = (x == parent(x)(y))
==(x::Float64,y::ComplexElem) = (y == parent(y)(x))

!=(x::ComplexElem,y::Int) = (x != parent(x)(y))
!=(x::Int,y::ComplexElem) = (y != parent(y)(x))

!=(x::ComplexElem,y::RealElem) = (x != parent(x)(y))
!=(x::RealElem,y::ComplexElem) = (y != parent(y)(x))

!=(x::ComplexElem,y::ZZRingElem) = (x != parent(x)(y))
!=(x::ZZRingElem,y::ComplexElem) = (y != parent(y)(x))

!=(x::ComplexElem,y::Float64) = (x != parent(x)(y))
!=(x::Float64,y::ComplexElem) = (y != parent(y)(x))

################################################################################
#
#  Containment
#
################################################################################

@doc Markdown.doc"""
    overlaps(x::ComplexElem, y::ComplexElem)

Returns `true` if any part of the box $x$ overlaps any part of the box $y$,
otherwise return `false`.
"""
function overlaps(x::ComplexElem, y::ComplexElem)
  r = ccall((:acb_overlaps, libarb), Cint, (Ref{ComplexElem}, Ref{ComplexElem}), x, y)
  return Bool(r)
end

@doc Markdown.doc"""
    contains(x::ComplexElem, y::ComplexElem)

Returns `true` if the box $x$ contains the box $y$, otherwise return
`false`.
"""
function contains(x::ComplexElem, y::ComplexElem)
  r = ccall((:acb_contains, libarb), Cint, (Ref{ComplexElem}, Ref{ComplexElem}), x, y)
  return Bool(r)
end

@doc Markdown.doc"""
    contains(x::ComplexElem, y::QQFieldElem)

Returns `true` if the box $x$ contains the given rational value, otherwise
return `false`.
"""
function contains(x::ComplexElem, y::QQFieldElem)
  r = ccall((:acb_contains_fmpq, libarb), Cint, (Ref{ComplexElem}, Ref{QQFieldElem}), x, y)
  return Bool(r)
end

@doc Markdown.doc"""
    contains(x::ComplexElem, y::ZZRingElem)

Returns `true` if the box $x$ contains the given integer value, otherwise
return `false`.
"""
function contains(x::ComplexElem, y::ZZRingElem)
  r = ccall((:acb_contains_fmpz, libarb), Cint, (Ref{ComplexElem}, Ref{ZZRingElem}), x, y)
  return Bool(r)
end

function contains(x::ComplexElem, y::Int)
  v = ZZRingElem(y)
  r = ccall((:acb_contains_fmpz, libarb), Cint, (Ref{ComplexElem}, Ref{ZZRingElem}), x, v)
  return Bool(r)
end

@doc Markdown.doc"""
    contains(x::ComplexElem, y::Integer)

Returns `true` if the box $x$ contains the given integer value, otherwise
return `false`.
"""
contains(x::ComplexElem, y::Integer) = contains(x, ZZRingElem(y))

@doc Markdown.doc"""
    contains(x::ComplexElem, y::Rational{T}) where {T <: Integer}

Returns `true` if the box $x$ contains the given rational value, otherwise
return `false`.
"""
contains(x::ComplexElem, y::Rational{T}) where {T <: Integer} = contains(x, ZZRingElem(y))

@doc Markdown.doc"""
    contains_zero(x::ComplexElem)

Returns `true` if the box $x$ contains zero, otherwise return `false`.
"""
function contains_zero(x::ComplexElem)
   return Bool(ccall((:acb_contains_zero, libarb), Cint, (Ref{ComplexElem},), x))
end

################################################################################
#
#  Predicates
#
################################################################################

function is_unit(x::ComplexElem)
   !iszero(x)
end

@doc Markdown.doc"""
    iszero(x::ComplexElem)

Return `true` if $x$ is certainly zero, otherwise return `false`.
"""
function iszero(x::ComplexElem)
   return Bool(ccall((:acb_is_zero, libarb), Cint, (Ref{ComplexElem},), x))
end

@doc Markdown.doc"""
    isone(x::ComplexElem)

Return `true` if $x$ is certainly zero, otherwise return `false`.
"""
function isone(x::ComplexElem)
   return Bool(ccall((:acb_is_one, libarb), Cint, (Ref{ComplexElem},), x))
end

@doc Markdown.doc"""
    isfinite(x::ComplexElem)

Return `true` if $x$ is finite, i.e. its real and imaginary parts have finite
midpoint and radius, otherwise return `false`.
"""
function isfinite(x::ComplexElem)
   return Bool(ccall((:acb_is_finite, libarb), Cint, (Ref{ComplexElem},), x))
end

@doc Markdown.doc"""
    is_exact(x::ComplexElem)

Return `true` if $x$ is exact, i.e. has its real and imaginary parts have
zero radius, otherwise return `false`.
"""
function is_exact(x::ComplexElem)
   return Bool(ccall((:acb_is_exact, libarb), Cint, (Ref{ComplexElem},), x))
end

@doc Markdown.doc"""
    isinteger(x::ComplexElem)

Return `true` if $x$ is an exact integer, otherwise return `false`.
"""
function isinteger(x::ComplexElem)
   return Bool(ccall((:acb_is_int, libarb), Cint, (Ref{ComplexElem},), x))
end

function isreal(x::ComplexElem)
   return Bool(ccall((:acb_is_real, libarb), Cint, (Ref{ComplexElem},), x))
end

is_negative(x::ComplexElem) = isreal(x) && is_negative(real(x))

################################################################################
#
#  Absolute value
#
################################################################################

function abs(x::ComplexElem, prec::Int = precision(Balls))
  z = RealElem()
  ccall((:acb_abs, libarb), Nothing,
                (Ref{RealElem}, Ref{ComplexElem}, Int), z, x, prec)
  return z
end

################################################################################
#
#  Inversion
#
################################################################################

function inv(x::ComplexElem, prec::Int = precision(Balls))
  z = ComplexElem()
  ccall((:acb_inv, libarb), Nothing, (Ref{ComplexElem}, Ref{ComplexElem}, Int), z, x, prec)
  return z
end

################################################################################
#
#  Shifting
#
################################################################################

function ldexp(x::ComplexElem, y::Int)
  z = ComplexElem()
  ccall((:acb_mul_2exp_si, libarb), Nothing,
              (Ref{ComplexElem}, Ref{ComplexElem}, Int), z, x, y)
  return z
end

function ldexp(x::ComplexElem, y::ZZRingElem)
  z = ComplexElem()
  ccall((:acb_mul_2exp_fmpz, libarb), Nothing,
              (Ref{ComplexElem}, Ref{ComplexElem}, Ref{ZZRingElem}), z, x, y)
  return z
end

################################################################################
#
#  Miscellaneous
#
################################################################################

@doc Markdown.doc"""
    trim(x::ComplexElem)

Return an `acb` box containing $x$ but which may be more economical,
by rounding off insignificant bits from midpoints.
"""
function trim(x::ComplexElem)
  z = ComplexElem()
  ccall((:acb_trim, libarb), Nothing, (Ref{ComplexElem}, Ref{ComplexElem}), z, x)
  return z
end

@doc Markdown.doc"""
    unique_integer(x::ComplexElem)

Return a pair where the first value is a boolean and the second is an `ZZRingElem`
integer. The boolean indicates whether the box $x$ contains a unique
integer. If this is the case, the second return value is set to this unique
integer.
"""
function unique_integer(x::ComplexElem)
  z = ZZRingElem()
  unique = ccall((:acb_get_unique_fmpz, libarb), Int,
    (Ref{ZZRingElem}, Ref{ComplexElem}), z, x)
  return (unique != 0, z)
end

function conj(x::ComplexElem)
  z = ComplexElem()
  ccall((:acb_conj, libarb), Nothing, (Ref{ComplexElem}, Ref{ComplexElem}), z, x)
  return z
end

function angle(x::ComplexElem, prec::Int = precision(Balls))
  z = RealElem()
  ccall((:acb_arg, libarb), Nothing,
                (Ref{RealElem}, Ref{ComplexElem}, Int), z, x, prec)
  z.parent = RealField(precision(Balls))
  return z
end

################################################################################
#
#  Constants
#
################################################################################

@doc Markdown.doc"""
    const_pi(r::ComplexField)

Return $\pi = 3.14159\ldots$ as an element of $r$.
"""
function const_pi(r::ComplexField, prec::Int = precision(Balls))
  z = r()
  ccall((:acb_const_pi, libarb), Nothing, (Ref{ComplexElem}, Int), z, prec)
  return z
end

################################################################################
#
#  Complex valued functions
#
################################################################################

# complex - complex functions

function Base.sqrt(x::ComplexElem, prec::Int = precision(Balls); check::Bool=true)
   z = ComplexElem()
   ccall((:acb_sqrt, libarb), Nothing, (Ref{ComplexElem}, Ref{ComplexElem}, Int), z, x, prec)
   return z
end

@doc Markdown.doc"""
    rsqrt(x::ComplexElem)

Return the reciprocal of the square root of $x$, i.e. $1/\sqrt{x}$.
"""
function rsqrt(x::ComplexElem, prec::Int = precision(Balls))
   z = ComplexElem()
   ccall((:acb_rsqrt, libarb), Nothing, (Ref{ComplexElem}, Ref{ComplexElem}, Int), z, x, prec)
   return z
end

function log(x::ComplexElem, prec::Int = precision(Balls))
   z = ComplexElem()
   ccall((:acb_log, libarb), Nothing, (Ref{ComplexElem}, Ref{ComplexElem}, Int), z, x, prec)
   return z
end

function log1p(x::ComplexElem, prec::Int = precision(Balls))
   z = ComplexElem()
   ccall((:acb_log1p, libarb), Nothing, (Ref{ComplexElem}, Ref{ComplexElem}, Int), z, x, prec)
   return z
end

function Base.exp(x::ComplexElem, prec::Int = precision(Balls))
   z = ComplexElem()
   ccall((:acb_exp, libarb), Nothing, (Ref{ComplexElem}, Ref{ComplexElem}, Int), z, x, prec)
   return z
end

function Base.expm1(x::ComplexElem, prec::Int = precision(Balls))
   z = ComplexElem()
   ccall((:acb_expm1, libarb), Nothing, (Ref{ComplexElem}, Ref{ComplexElem}, Int), z, x, prec)
   return z
end

@doc Markdown.doc"""
    cispi(x::ComplexElem)

Return the exponential of $\pi i x$.
"""
function cispi(x::ComplexElem, prec::Int = precision(Balls))
   z = ComplexElem()
   ccall((:acb_exp_pi_i, libarb), Nothing, (Ref{ComplexElem}, Ref{ComplexElem}, Int), z, x, prec)
   return z
end

@doc Markdown.doc"""
    root_of_unity(C::ComplexField, k::Int)

Return $\exp(2\pi i/k)$.
"""
function root_of_unity(C::ComplexField, k::Int, prec::Int = precision(Balls))
   k <= 0 && throw(ArgumentError("Order must be positive ($k)"))
   z = C()
   ccall((:acb_unit_root, libarb), Nothing, (Ref{ComplexElem}, UInt, Int), z, k, prec)
   return z
end

function sin(x::ComplexElem, prec::Int = precision(Balls))
   z = ComplexElem()
   ccall((:acb_sin, libarb), Nothing, (Ref{ComplexElem}, Ref{ComplexElem}, Int), z, x, prec)
   return z
end

function cos(x::ComplexElem, prec::Int = precision(Balls))
   z = ComplexElem()
   ccall((:acb_cos, libarb), Nothing, (Ref{ComplexElem}, Ref{ComplexElem}, Int), z, x, prec)
   return z
end

function tan(x::ComplexElem, prec::Int = precision(Balls))
   z = ComplexElem()
   ccall((:acb_tan, libarb), Nothing, (Ref{ComplexElem}, Ref{ComplexElem}, Int), z, x, prec)
   return z
end

function cot(x::ComplexElem, prec::Int = precision(Balls))
   z = ComplexElem()
   ccall((:acb_cot, libarb), Nothing, (Ref{ComplexElem}, Ref{ComplexElem}, Int), z, x, prec)
   return z
end

function sinpi(x::ComplexElem, prec::Int = precision(Balls))
   z = ComplexElem()
   ccall((:acb_sin_pi, libarb), Nothing, (Ref{ComplexElem}, Ref{ComplexElem}, Int), z, x, prec)
   return z
end

function cospi(x::ComplexElem, prec::Int = precision(Balls))
   z = ComplexElem()
   ccall((:acb_cos_pi, libarb), Nothing, (Ref{ComplexElem}, Ref{ComplexElem}, Int), z, x, prec)
   return z
end

function tanpi(x::ComplexElem, prec::Int = precision(Balls))
   z = ComplexElem()
   ccall((:acb_tan_pi, libarb), Nothing, (Ref{ComplexElem}, Ref{ComplexElem}, Int), z, x, prec)
   return z
end

function cotpi(x::ComplexElem, prec::Int = precision(Balls))
   z = ComplexElem()
   ccall((:acb_cot_pi, libarb), Nothing, (Ref{ComplexElem}, Ref{ComplexElem}, Int), z, x, prec)
   return z
end

function sinh(x::ComplexElem, prec::Int = precision(Balls))
   z = ComplexElem()
   ccall((:acb_sinh, libarb), Nothing, (Ref{ComplexElem}, Ref{ComplexElem}, Int), z, x, prec)
   return z
end

function cosh(x::ComplexElem, prec::Int = precision(Balls))
   z = ComplexElem()
   ccall((:acb_cosh, libarb), Nothing, (Ref{ComplexElem}, Ref{ComplexElem}, Int), z, x, prec)
   return z
end

function tanh(x::ComplexElem, prec::Int = precision(Balls))
   z = ComplexElem()
   ccall((:acb_tanh, libarb), Nothing, (Ref{ComplexElem}, Ref{ComplexElem}, Int), z, x, prec)
   return z
end

function coth(x::ComplexElem, prec::Int = precision(Balls))
   z = ComplexElem()
   ccall((:acb_coth, libarb), Nothing, (Ref{ComplexElem}, Ref{ComplexElem}, Int), z, x, prec)
   return z
end

function atan(x::ComplexElem, prec::Int = precision(Balls))
   z = ComplexElem()
   ccall((:acb_atan, libarb), Nothing, (Ref{ComplexElem}, Ref{ComplexElem}, Int), z, x, prec)
   return z
end

@doc Markdown.doc"""
    log_sinpi(x::ComplexElem)

Return $\log\sin(\pi x)$, constructed without branch cuts off the real line.
"""
function log_sinpi(x::ComplexElem, prec::Int = precision(Balls))
   z = ComplexElem()
   ccall((:acb_log_sin_pi, libarb), Nothing, (Ref{ComplexElem}, Ref{ComplexElem}, Int), z, x, prec)
   return z
end

@doc Markdown.doc"""
    gamma(x::ComplexElem)

Return the Gamma function evaluated at $x$.
"""
function gamma(x::ComplexElem, prec::Int = precision(Balls))
   z = ComplexElem()
   ccall((:acb_gamma, libarb), Nothing, (Ref{ComplexElem}, Ref{ComplexElem}, Int), z, x, prec)
   return z
end

@doc Markdown.doc"""
    rgamma(x::ComplexElem)

Return the reciprocal of the Gamma function evaluated at $x$.
"""
function rgamma(x::ComplexElem, prec::Int = precision(Balls))
   z = ComplexElem()
   ccall((:acb_rgamma, libarb), Nothing, (Ref{ComplexElem}, Ref{ComplexElem}, Int), z, x, prec)
   return z
end

@doc Markdown.doc"""
    lgamma(x::ComplexElem)

Return the logarithm of the Gamma function evaluated at $x$.
"""
function lgamma(x::ComplexElem, prec::Int = precision(Balls))
   z = ComplexElem()
   ccall((:acb_lgamma, libarb), Nothing, (Ref{ComplexElem}, Ref{ComplexElem}, Int), z, x, prec)
   return z
end

@doc Markdown.doc"""
    digamma(x::ComplexElem)

Return the  logarithmic derivative of the gamma function evaluated at $x$,
i.e. $\psi(x)$.
"""
function digamma(x::ComplexElem, prec::Int = precision(Balls))
   z = ComplexElem()
   ccall((:acb_digamma, libarb), Nothing, (Ref{ComplexElem}, Ref{ComplexElem}, Int), z, x, prec)
   return z
end

@doc Markdown.doc"""
    zeta(x::ComplexElem)

Return the Riemann zeta function evaluated at $x$.
"""
function zeta(x::ComplexElem, prec::Int = precision(Balls))
   z = ComplexElem()
   ccall((:acb_zeta, libarb), Nothing, (Ref{ComplexElem}, Ref{ComplexElem}, Int), z, x, prec)
   return z
end

@doc Markdown.doc"""
    barnes_g(x::ComplexElem)

Return the Barnes $G$-function, evaluated at $x$.
"""
function barnes_g(x::ComplexElem, prec::Int = precision(Balls))
   z = ComplexElem()
   ccall((:acb_barnes_g, libarb), Nothing, (Ref{ComplexElem}, Ref{ComplexElem}, Int), z, x, prec)
   return z
end

@doc Markdown.doc"""
    log_barnes_g(x::ComplexElem)

Return the logarithm of the Barnes $G$-function, evaluated at $x$.
"""
function log_barnes_g(x::ComplexElem, prec::Int = precision(Balls))
   z = ComplexElem()
   ccall((:acb_log_barnes_g, libarb), Nothing, (Ref{ComplexElem}, Ref{ComplexElem}, Int), z, x, prec)
   return z
end

@doc Markdown.doc"""
    agm(x::ComplexElem)

Return the arithmetic-geometric mean of $1$ and $x$.
"""
function agm(x::ComplexElem, prec::Int = precision(Balls))
   z = ComplexElem()
   ccall((:acb_agm1, libarb), Nothing, (Ref{ComplexElem}, Ref{ComplexElem}, Int), z, x, prec)
   return z
end

@doc Markdown.doc"""
    erf(x::ComplexElem)

Return the error function evaluated at $x$.
"""
function erf(x::ComplexElem, prec::Int = precision(Balls))
   z = ComplexElem()
   ccall((:acb_hypgeom_erf, libarb), Nothing, (Ref{ComplexElem}, Ref{ComplexElem}, Int), z, x, prec)
   return z
end

@doc Markdown.doc"""
    erfi(x::ComplexElem)

Return the imaginary error function evaluated at $x$.
"""
function erfi(x::ComplexElem, prec::Int = precision(Balls))
   z = ComplexElem()
   ccall((:acb_hypgeom_erfi, libarb), Nothing, (Ref{ComplexElem}, Ref{ComplexElem}, Int), z, x, prec)
   return z
end

@doc Markdown.doc"""
    erfc(x::ComplexElem)

Return the complementary error function evaluated at $x$.
"""
function erfc(x::ComplexElem, prec::Int = precision(Balls))
   z = ComplexElem()
   ccall((:acb_hypgeom_erfc, libarb), Nothing, (Ref{ComplexElem}, Ref{ComplexElem}, Int), z, x, prec)
   return z
end

@doc Markdown.doc"""
    exp_integral_ei(x::ComplexElem)

Return the exponential integral evaluated at $x$.
"""
function exp_integral_ei(x::ComplexElem, prec::Int = precision(Balls))
   z = ComplexElem()
   ccall((:acb_hypgeom_ei, libarb), Nothing, (Ref{ComplexElem}, Ref{ComplexElem}, Int), z, x, prec)
   return z
end

@doc Markdown.doc"""
    sin_integral(x::ComplexElem)

Return the sine integral evaluated at $x$.
"""
function sin_integral(x::ComplexElem, prec::Int = precision(Balls))
   z = ComplexElem()
   ccall((:acb_hypgeom_si, libarb), Nothing, (Ref{ComplexElem}, Ref{ComplexElem}, Int), z, x, prec)
   return z
end

@doc Markdown.doc"""
    cos_integral(x::ComplexElem)

Return the exponential cosine integral evaluated at $x$.
"""
function cos_integral(x::ComplexElem, prec::Int = precision(Balls))
   z = ComplexElem()
   ccall((:acb_hypgeom_ci, libarb), Nothing, (Ref{ComplexElem}, Ref{ComplexElem}, Int), z, x, prec)
   return z
end

@doc Markdown.doc"""
    sinh_integral(x::ComplexElem)

Return the hyperbolic sine integral evaluated at $x$.
"""
function sinh_integral(x::ComplexElem, prec::Int = precision(Balls))
   z = ComplexElem()
   ccall((:acb_hypgeom_shi, libarb), Nothing, (Ref{ComplexElem}, Ref{ComplexElem}, Int), z, x, prec)
   return z
end

@doc Markdown.doc"""
    cosh_integral(x::ComplexElem)

Return the hyperbolic cosine integral evaluated at $x$.
"""
function cosh_integral(x::ComplexElem, prec::Int = precision(Balls))
   z = ComplexElem()
   ccall((:acb_hypgeom_chi, libarb), Nothing, (Ref{ComplexElem}, Ref{ComplexElem}, Int), z, x, prec)
   return z
end

@doc Markdown.doc"""
    dedekind_eta(x::ComplexElem)

Return the Dedekind eta function $\eta(\tau)$ at $\tau = x$.
"""
function dedekind_eta(x::ComplexElem, prec::Int = precision(Balls))
   z = ComplexElem()
   ccall((:acb_modular_eta, libarb), Nothing, (Ref{ComplexElem}, Ref{ComplexElem}, Int), z, x, prec)
   return z
end

@doc Markdown.doc"""
    modular_weber_f(x::ComplexElem)

Return the modular Weber function
$\mathfrak{f}(\tau) = \frac{\eta^2(\tau)}{\eta(\tau/2)\eta(2\tau)},$
at $x$ in the complex upper half plane.
"""
function modular_weber_f(x::ComplexElem)
   x_on_2 = divexact(x, 2)
   x_times_2 = 2*x
   return divexact(dedekind_eta(x)^2, dedekind_eta(x_on_2)*dedekind_eta(x_times_2))
end

@doc Markdown.doc"""
    modular_weber_f1(x::ComplexElem)

Return the modular Weber function
$\mathfrak{f}_1(\tau) = \frac{\eta(\tau/2)}{\eta(\tau)},$
at $x$ in the complex upper half plane.
"""
function modular_weber_f1(x::ComplexElem)
   x_on_2 = divexact(x, 2)
   return divexact(dedekind_eta(x_on_2), dedekind_eta(x))
end

@doc Markdown.doc"""
    modular_weber_f2(x::ComplexElem)

Return the modular Weber function
$\mathfrak{f}_2(\tau) = \frac{\sqrt{2}\eta(2\tau)}{\eta(\tau)}$
at $x$ in the complex upper half plane.
"""
function modular_weber_f2(x::ComplexElem)
   x_times_2 = x*2
   return divexact(dedekind_eta(x_times_2), dedekind_eta(x))*sqrt(parent(x)(2))
end

@doc Markdown.doc"""
    j_invariant(x::ComplexElem)

Return the $j$-invariant $j(\tau)$ at $\tau = x$.
"""
function j_invariant(x::ComplexElem, prec::Int = precision(Balls))
   z = ComplexElem()
   ccall((:acb_modular_j, libarb), Nothing, (Ref{ComplexElem}, Ref{ComplexElem}, Int), z, x, prec)
   return z
end

@doc Markdown.doc"""
    modular_lambda(x::ComplexElem)

Return the modular lambda function $\lambda(\tau)$ at $\tau = x$.
"""
function modular_lambda(x::ComplexElem, prec::Int = precision(Balls))
   z = ComplexElem()
   ccall((:acb_modular_lambda, libarb), Nothing, (Ref{ComplexElem}, Ref{ComplexElem}, Int), z, x, prec)
   return z
end

@doc Markdown.doc"""
    modular_delta(x::ComplexElem)

Return the modular delta function $\Delta(\tau)$ at $\tau = x$.
"""
function modular_delta(x::ComplexElem, prec::Int = precision(Balls))
   z = ComplexElem()
   ccall((:acb_modular_delta, libarb), Nothing, (Ref{ComplexElem}, Ref{ComplexElem}, Int), z, x, prec)
   return z
end

@doc Markdown.doc"""
    eisenstein_g(k::Int, x::ComplexElem)

Return the non-normalized Eisenstein series $G_k(\tau)$ of
$\mathrm{SL}_2(\mathbb{Z})$. Also defined for $\tau = i \infty$.
"""
function eisenstein_g(k::Int, x::ComplexElem, prec::Int = precision(Balls))
  CC = parent(x)

  k <= 2 && error("Eisenstein series are not absolute convergent for k = $k")
  imag(x) < 0 && error("x is not in upper half plane.")
  isodd(k) && return zero(CC)
  imag(x) == Inf && return 2 * zeta(CC(k))

  len = div(k, 2) - 1
  vec = acb_vec(len)
  ccall((:acb_modular_eisenstein, libarb), Nothing,
        (Ptr{acb_struct}, Ref{ComplexElem}, Int, Int), vec, x, len, prec)
  z = array(CC, vec, len)
  acb_vec_clear(vec, len)
  return z[end]
end

@doc Markdown.doc"""
    hilbert_class_polynomial(D::Int, R::ZZPolyRing)

Return in the ring $R$ the Hilbert class polynomial of discriminant $D$,
which is only defined for $D < 0$ and $D \equiv 0, 1 \pmod 4$.
"""
function hilbert_class_polynomial(D::Int, R::ZZPolyRing)
   D < 0 && mod(D, 4) < 2 || throw(ArgumentError("$D is not a negative discriminant"))
   z = R()
   ccall((:acb_modular_hilbert_class_poly, Nemo.libarb), Nothing,
         (Ref{ZZPolyRingElem}, Int),
         z, D)
   return z
end

@doc Markdown.doc"""
    elliptic_k(x::ComplexElem)

Return the complete elliptic integral $K(x)$.
"""
function elliptic_k(x::ComplexElem, prec::Int = precision(Balls))
   z = ComplexElem()
   ccall((:acb_modular_elliptic_k, libarb), Nothing, (Ref{ComplexElem}, Ref{ComplexElem}, Int), z, x, prec)
   return z
end

@doc Markdown.doc"""
    elliptic_e(x::ComplexElem)

Return the complete elliptic integral $E(x)$.
"""
function elliptic_e(x::ComplexElem, prec::Int = precision(Balls))
   z = ComplexElem()
   ccall((:acb_modular_elliptic_e, libarb), Nothing, (Ref{ComplexElem}, Ref{ComplexElem}, Int), z, x, prec)
   return z
end

function sincos(x::ComplexElem, prec::Int = precision(Balls))
  s = ComplexElem()
  c = ComplexElem()
  ccall((:acb_sin_cos, libarb), Nothing,
              (Ref{ComplexElem}, Ref{ComplexElem}, Ref{ComplexElem}, Int), s, c, x, prec)
  return (s, c)
end

function sincospi(x::ComplexElem, prec::Int = precision(Balls))
  s = ComplexElem()
  c = ComplexElem()
  ccall((:acb_sin_cos_pi, libarb), Nothing,
              (Ref{ComplexElem}, Ref{ComplexElem}, Ref{ComplexElem}, Int), s, c, x, prec)
  return (s, c)
end

@doc Markdown.doc"""
    sinhcosh(x::ComplexElem)

Return a tuple $s, c$ consisting of the hyperbolic sine and cosine of $x$.
"""
function sinhcosh(x::ComplexElem, prec::Int = precision(Balls))
  s = ComplexElem()
  c = ComplexElem()
  ccall((:acb_sinh_cosh, libarb), Nothing,
              (Ref{ComplexElem}, Ref{ComplexElem}, Ref{ComplexElem}, Int), s, c, x, prec)
  return (s, c)
end

@doc Markdown.doc"""
    zeta(s::ComplexElem, a::ComplexElem)

Return the Hurwitz zeta function $\zeta(s,a)$.
"""
function zeta(s::ComplexElem, a::ComplexElem, prec::Int = precision(Balls))
  z = parent(s)()
  ccall((:acb_hurwitz_zeta, libarb), Nothing,
              (Ref{ComplexElem}, Ref{ComplexElem}, Ref{ComplexElem}, Int), z, s, a, prec)
  return z
end

@doc Markdown.doc"""
    polygamma(s::ComplexElem, a::ComplexElem)

Return the generalised polygamma function $\psi(s,z)$.
"""
function polygamma(s::ComplexElem, a::ComplexElem, prec::Int = precision(Balls))
  z = parent(s)()
  ccall((:acb_polygamma, libarb), Nothing,
              (Ref{ComplexElem}, Ref{ComplexElem}, Ref{ComplexElem}, Int), z, s, a, prec)
  return z
end

function rising_factorial(x::ComplexElem, n::UInt, prec::Int = precision(Balls))
  z = ComplexElem()
  ccall((:acb_rising_ui, libarb), Nothing,
              (Ref{ComplexElem}, Ref{ComplexElem}, UInt, Int), z, x, n, prec)
  return z
end

@doc Markdown.doc"""
    rising_factorial(x::ComplexElem, n::Int)

Return the rising factorial $x(x + 1)\ldots (x + n - 1)$ as an Acb.
"""
function rising_factorial(x::ComplexElem, n::Int)
  n < 0 && throw(DomainError(n, "Argument must be non-negative"))
  return rising_factorial(x, UInt(n))
end

function rising_factorial2(x::ComplexElem, n::UInt, prec::Int = precision(Balls))
  z = ComplexElem()
  w = ComplexElem()
  ccall((:acb_rising2_ui, libarb), Nothing,
              (Ref{ComplexElem}, Ref{ComplexElem}, Ref{ComplexElem}, UInt, Int), z, w, x, n, prec)
  return (z, w)
end

@doc Markdown.doc"""
    rising_factorial2(x::ComplexElem, n::Int)

Return a tuple containing the rising factorial $x(x + 1)\ldots (x + n - 1)$
and its derivative.
"""
function rising_factorial2(x::ComplexElem, n::Int)
  n < 0 && throw(DomainError(n, "Argument must be non-negative"))
  return rising_factorial2(x, UInt(n))
end

function polylog(s::ComplexElem, a::ComplexElem, prec::Int = precision(Balls))
  z = parent(s)()
  ccall((:acb_polylog, libarb), Nothing,
              (Ref{ComplexElem}, Ref{ComplexElem}, Ref{ComplexElem}, Int), z, s, a, prec)
  return z
end

function polylog(s::Int, a::ComplexElem, prec::Int = precision(Balls))
  z = parent(a)()
  ccall((:acb_polylog_si, libarb), Nothing,
              (Ref{ComplexElem}, Int, Ref{ComplexElem}, Int), z, s, a, prec)
  return z
end

@doc Markdown.doc"""
    polylog(s::Union{ComplexElem,Int}, a::ComplexElem)

Return the polylogarithm Li$_s(a)$.
""" polylog(s::Union{ComplexElem,Int}, ::ComplexElem)

@doc Markdown.doc"""
    log_integral(x::ComplexElem)

Return the logarithmic integral, evaluated at $x$.
"""
function log_integral(x::ComplexElem, prec::Int = precision(Balls))
  z = ComplexElem()
  ccall((:acb_hypgeom_li, libarb), Nothing,
              (Ref{ComplexElem}, Ref{ComplexElem}, Int, Int), z, x, 0, prec)
  return z
end

@doc Markdown.doc"""
    log_integral_offset(x::ComplexElem)

Return the offset logarithmic integral, evaluated at $x$.
"""
function log_integral_offset(x::ComplexElem, prec::Int = precision(Balls))
  z = ComplexElem()
  ccall((:acb_hypgeom_li, libarb), Nothing,
              (Ref{ComplexElem}, Ref{ComplexElem}, Int, Int), z, x, 1, prec)
  return z
end

@doc Markdown.doc"""
    exp_integral_e(s::ComplexElem, x::ComplexElem)

Return the generalised exponential integral $E_s(x)$.
"""
function exp_integral_e(s::ComplexElem, x::ComplexElem, prec::Int = precision(Balls))
  z = parent(s)()
  ccall((:acb_hypgeom_expint, libarb), Nothing,
              (Ref{ComplexElem}, Ref{ComplexElem}, Ref{ComplexElem}, Int), z, s, x, prec)
  return z
end

@doc Markdown.doc"""
    gamma(s::ComplexElem, x::ComplexElem)

Return the upper incomplete gamma function $\Gamma(s,x)$.
"""
function gamma(s::ComplexElem, x::ComplexElem, prec::Int = precision(Balls))
  z = parent(s)()
  ccall((:acb_hypgeom_gamma_upper, libarb), Nothing,
        (Ref{ComplexElem}, Ref{ComplexElem}, Ref{ComplexElem}, Int, Int), z, s, x, 0, prec)
  return z
end

@doc Markdown.doc"""
    gamma_regularized(s::ComplexElem, x::ComplexElem)

Return the regularized upper incomplete gamma function
$\Gamma(s,x) / \Gamma(s)$.
"""
function gamma_regularized(s::ComplexElem, x::ComplexElem, prec::Int = precision(Balls))
  z = parent(s)()
  ccall((:acb_hypgeom_gamma_upper, libarb), Nothing,
        (Ref{ComplexElem}, Ref{ComplexElem}, Ref{ComplexElem}, Int, Int), z, s, x, 1, prec)
  return z
end

@doc Markdown.doc"""
    gamma_lower(s::ComplexElem, x::ComplexElem)

Return the lower incomplete gamma function $\gamma(s,x) / \Gamma(s)$.
"""
function gamma_lower(s::ComplexElem, x::ComplexElem, prec::Int = precision(Balls))
  z = parent(s)()
  ccall((:acb_hypgeom_gamma_lower, libarb), Nothing,
        (Ref{ComplexElem}, Ref{ComplexElem}, Ref{ComplexElem}, Int, Int), z, s, x, 0, prec)
  return z
end

@doc Markdown.doc"""
    gamma_lower_regularized(s::ComplexElem, x::ComplexElem)

Return the regularized lower incomplete gamma function
$\gamma(s,x) / \Gamma(s)$.
"""
function gamma_lower_regularized(s::ComplexElem, x::ComplexElem, prec::Int = precision(Balls))
  z = parent(s)()
  ccall((:acb_hypgeom_gamma_lower, libarb), Nothing,
        (Ref{ComplexElem}, Ref{ComplexElem}, Ref{ComplexElem}, Int, Int), z, s, x, 1, prec)
  return z
end

@doc Markdown.doc"""
    bessel_j(nu::ComplexElem, x::ComplexElem)

Return the Bessel function $J_{\nu}(x)$.
"""
function bessel_j(nu::ComplexElem, x::ComplexElem, prec::Int = precision(Balls))
  z = ComplexElem()
  ccall((:acb_hypgeom_bessel_j, libarb), Nothing,
              (Ref{ComplexElem}, Ref{ComplexElem}, Ref{ComplexElem}, Int), z, nu, x, prec)
  return z
end

@doc Markdown.doc"""
    bessel_y(nu::ComplexElem, x::ComplexElem)

Return the Bessel function $Y_{\nu}(x)$.
"""
function bessel_y(nu::ComplexElem, x::ComplexElem, prec::Int = precision(Balls))
  z = ComplexElem()
  ccall((:acb_hypgeom_bessel_y, libarb), Nothing,
              (Ref{ComplexElem}, Ref{ComplexElem}, Ref{ComplexElem}, Int), z, nu, x, prec)
  return z
end

@doc Markdown.doc"""
    bessel_i(nu::ComplexElem, x::ComplexElem)

Return the Bessel function $I_{\nu}(x)$.
"""
function bessel_i(nu::ComplexElem, x::ComplexElem, prec::Int = precision(Balls))
  z = ComplexElem()
  ccall((:acb_hypgeom_bessel_i, libarb), Nothing,
              (Ref{ComplexElem}, Ref{ComplexElem}, Ref{ComplexElem}, Int), z, nu, x, prec)
  return z
end

@doc Markdown.doc"""
    bessel_k(nu::ComplexElem, x::ComplexElem)

Return the Bessel function $K_{\nu}(x)$.
"""
function bessel_k(nu::ComplexElem, x::ComplexElem, prec::Int = precision(Balls))
  z = ComplexElem()
  ccall((:acb_hypgeom_bessel_k, libarb), Nothing,
              (Ref{ComplexElem}, Ref{ComplexElem}, Ref{ComplexElem}, Int), z, nu, x, prec)
  return z
end

@doc Markdown.doc"""
    airy_ai(x::ComplexElem)

Return the Airy function $\operatorname{Ai}(x)$.
"""
function airy_ai(x::ComplexElem, prec::Int = precision(Balls))
  ai = ComplexElem()
  ccall((:acb_hypgeom_airy, libarb), Nothing,
              (Ref{ComplexElem}, Ptr{Cvoid}, Ptr{Cvoid}, Ptr{Cvoid}, Ref{ComplexElem}, Int),
              ai, C_NULL, C_NULL, C_NULL, x, prec)
  return ai
end

@doc Markdown.doc"""
    airy_bi(x::ComplexElem)

Return the Airy function $\operatorname{Bi}(x)$.
"""
function airy_bi(x::ComplexElem, prec::Int = precision(Balls))
  bi = ComplexElem()
  ccall((:acb_hypgeom_airy, libarb), Nothing,
              (Ptr{Cvoid}, Ptr{Cvoid}, Ref{ComplexElem}, Ptr{Cvoid}, Ref{ComplexElem}, Int),
              C_NULL, C_NULL, bi, C_NULL, x, prec)
  return bi
end

@doc Markdown.doc"""
    airy_ai_prime(x::ComplexElem)

Return the derivative of the Airy function $\operatorname{Ai}^\prime(x)$.
"""
function airy_ai_prime(x::ComplexElem, prec::Int = precision(Balls))
  ai_prime = ComplexElem()
  ccall((:acb_hypgeom_airy, libarb), Nothing,
              (Ptr{Cvoid}, Ref{ComplexElem}, Ptr{Cvoid}, Ptr{Cvoid}, Ref{ComplexElem}, Int),
              C_NULL, ai_prime, C_NULL, C_NULL, x, prec)
  return ai_prime
end

@doc Markdown.doc"""
    airy_bi_prime(x::ComplexElem)

Return the derivative of the Airy function $\operatorname{Bi}^\prime(x)$.
"""
function airy_bi_prime(x::ComplexElem, prec::Int = precision(Balls))
  bi_prime = ComplexElem()
  ccall((:acb_hypgeom_airy, libarb), Nothing,
              (Ptr{Cvoid}, Ptr{Cvoid}, Ptr{Cvoid}, Ref{ComplexElem}, Ref{ComplexElem}, Int),
              C_NULL, C_NULL, C_NULL, bi_prime, x, prec)
  return bi_prime
end

@doc Markdown.doc"""
    hypergeometric_1f1(a::ComplexElem, b::ComplexElem, x::ComplexElem)

Return the confluent hypergeometric function ${}_1F_1(a,b,x)$.
"""
function hypergeometric_1f1(a::ComplexElem, b::ComplexElem, x::ComplexElem, prec::Int = precision(Balls))
  z = ComplexElem()
  ccall((:acb_hypgeom_m, libarb), Nothing,
              (Ref{ComplexElem}, Ref{ComplexElem}, Ref{ComplexElem}, Ref{ComplexElem}, Int, Int), z, a, b, x, 0, prec)
  return z
end

@doc Markdown.doc"""
    hypergeometric_1f1_regularized(a::ComplexElem, b::ComplexElem, x::ComplexElem)

Return the regularized confluent hypergeometric function
${}_1F_1(a,b,x) / \Gamma(b)$.
"""
function hypergeometric_1f1_regularized(a::ComplexElem, b::ComplexElem, x::ComplexElem, prec::Int = precision(Balls))
  z = ComplexElem()
  ccall((:acb_hypgeom_m, libarb), Nothing,
              (Ref{ComplexElem}, Ref{ComplexElem}, Ref{ComplexElem}, Ref{ComplexElem}, Int, Int), z, a, b, x, 1, prec)
  return z
end

@doc Markdown.doc"""
    hypergeometric_u(a::ComplexElem, b::ComplexElem, x::ComplexElem)

Return the confluent hypergeometric function $U(a,b,x)$.
"""
function hypergeometric_u(a::ComplexElem, b::ComplexElem, x::ComplexElem, prec::Int = precision(Balls))
  z = ComplexElem()
  ccall((:acb_hypgeom_u, libarb), Nothing,
              (Ref{ComplexElem}, Ref{ComplexElem}, Ref{ComplexElem}, Ref{ComplexElem}, Int), z, a, b, x, prec)
  return z
end

@doc Markdown.doc"""
    hypergeometric_2f1(a::ComplexElem, b::ComplexElem, c::ComplexElem, x::ComplexElem; flags=0)

Return the Gauss hypergeometric function ${}_2F_1(a,b,c,x)$.
"""
function hypergeometric_2f1(a::ComplexElem, b::ComplexElem, c::ComplexElem, x::ComplexElem, prec::Int = precision(Balls); flags=0)
  z = ComplexElem()
  ccall((:acb_hypgeom_2f1, libarb), Nothing,
              (Ref{ComplexElem}, Ref{ComplexElem}, Ref{ComplexElem}, Ref{ComplexElem}, Ref{ComplexElem}, Int, Int), z, a, b, c, x, flags, prec)
  return z
end

@doc Markdown.doc"""
    jacobi_theta(z::ComplexElem, tau::ComplexElem)

Return a tuple of four elements containing the Jacobi theta function values
$\theta_1, \theta_2, \theta_3, \theta_4$ evaluated at $z, \tau$.
"""
function jacobi_theta(z::ComplexElem, tau::ComplexElem, prec::Int = precision(Balls))
  t1 = ComplexElem()
  t2 = ComplexElem()
  t3 = ComplexElem()
  t4 = ComplexElem()
  ccall((:acb_modular_theta, libarb), Nothing,
              (Ref{ComplexElem}, Ref{ComplexElem}, Ref{ComplexElem}, Ref{ComplexElem}, Ref{ComplexElem}, Ref{ComplexElem}, Int),
                t1, t2, t3, t4, z, tau, prec)
  return (t1, t2, t3, t4)
end

@doc Markdown.doc"""
    weierstrass_p(z::ComplexElem, tau::ComplexElem)

Return the Weierstrass elliptic function $\wp(z,\tau)$.
"""
function weierstrass_p(z::ComplexElem, tau::ComplexElem, prec::Int = precision(Balls))
  r = parent(z)()
  ccall((:acb_elliptic_p, libarb), Nothing,
              (Ref{ComplexElem}, Ref{ComplexElem}, Ref{ComplexElem}, Int), r, z, tau, prec)
  return r
end

@doc Markdown.doc"""
    weierstrass_p_prime(z::ComplexElem, tau::ComplexElem)

Return the derivative of the Weierstrass elliptic function $\frac{\partial}{\partial z}\wp(z,\tau)$.
"""
function weierstrass_p_prime(z::ComplexElem, tau::ComplexElem, prec::Int = precision(Balls))
  r = parent(z)()
  ccall((:acb_elliptic_p_prime, libarb), Nothing,
              (Ref{ComplexElem}, Ref{ComplexElem}, Ref{ComplexElem}, Int), r, z, tau, prec)
  return r
end

@doc Markdown.doc"""
    agm(x::ComplexElem, y::ComplexElem)

Return the arithmetic-geometric mean of $x$ and $y$.
"""
function agm(x::ComplexElem, y::ComplexElem)
  v = inv(y)
  if isfinite(v)
    return agm(x * v) * y
  else
    v = inv(x)
    return agm(y * v) * x
  end
end

@doc Markdown.doc"""
    lindep(A::Vector{ComplexElem}, bits::Int)

Find a small linear combination of the entries of the array $A$ that is small
(using LLL). The entries are first scaled by the given number of bits before
truncating the real and imaginary parts to integers for use in LLL. This function can
be used to find linear dependence between a list of complex numbers. The algorithm is
heuristic only and returns an array of Nemo integers representing the linear
combination.
"""
function lindep(A::Vector{ComplexElem}, bits::Int)
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
    lindep(A::Matrix{ComplexElem}, bits::Int)

Find a (common) small linear combination of the entries in each row of the array $A$,
that is small (using LLL). It is assumed that the complex numbers in each row of the
array share the same linear combination. The entries are first scaled by the given
number of bits before truncating the real and imaginary parts to integers for use in
LLL. This function can be used to find a common linear dependence shared across a
number of lists of complex numbers. The algorithm is heuristic only and returns an
array of Nemo integers representing the common linear combination.
"""
function lindep(A::Matrix{ComplexElem}, bits::Int)
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

function zero!(z::ComplexElem)
   ccall((:acb_zero, libarb), Nothing, (Ref{ComplexElem},), z)
   return z
end

function add!(z::ComplexElem, x::ComplexElem, y::ComplexElem, prec::Int = precision(Balls))
  ccall((:acb_add, libarb), Nothing, (Ref{ComplexElem}, Ref{ComplexElem}, Ref{ComplexElem}, Int),
         z, x, y, prec)
  return z
end

function addeq!(z::ComplexElem, y::ComplexElem, prec::Int = precision(Balls))
  ccall((:acb_add, libarb), Nothing, (Ref{ComplexElem}, Ref{ComplexElem}, Ref{ComplexElem}, Int),
         z, z, y, prec)
  return z
end

function sub!(z::ComplexElem, x::ComplexElem, y::ComplexElem, prec::Int = precision(Balls))
  ccall((:acb_sub, libarb), Nothing, (Ref{ComplexElem}, Ref{ComplexElem}, Ref{ComplexElem}, Int),
        z, x, y, prec)
  return z
end

function mul!(z::ComplexElem, x::ComplexElem, y::ComplexElem, prec::Int = precision(Balls))
  ccall((:acb_mul, libarb), Nothing, (Ref{ComplexElem}, Ref{ComplexElem}, Ref{ComplexElem}, Int),
        z, x, y, prec)
  return z
end

function div!(z::ComplexElem, x::ComplexElem, y::ComplexElem, prec::Int = precision(Balls))
  ccall((:acb_div, libarb), Nothing, (Ref{ComplexElem}, Ref{ComplexElem}, Ref{ComplexElem}, Int),
        z, x, y, prec)
  return z
end

################################################################################
#
#  Unsafe setting
#
################################################################################

for (typeofx, passtoc) in ((ComplexElem, Ref{ComplexElem}), (Ptr{ComplexElem}, Ptr{ComplexElem}))
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
    function _acb_set(x::($typeofx), y::ZZRingElem)
      ccall((:acb_set_fmpz, libarb), Nothing, (($passtoc), Ref{ZZRingElem}), x, y)
    end

    function _acb_set(x::($typeofx), y::ZZRingElem, p::Int)
      ccall((:acb_set_round_fmpz, libarb), Nothing,
                  (($passtoc), Ref{ZZRingElem}, Int), x, y, p)
    end

    function _acb_set(x::($typeofx), y::QQFieldElem, p::Int)
      ccall((:acb_set_fmpq, libarb), Nothing,
                  (($passtoc), Ref{QQFieldElem}, Int), x, y, p)
    end

    function _acb_set(x::($typeofx), y::RealElem)
      ccall((:acb_set_arb, libarb), Nothing, (($passtoc), Ref{RealElem}), x, y)
    end

    function _acb_set(x::($typeofx), y::RealElem, p::Int)
      _acb_set(x, y)
      ccall((:acb_set_round, libarb), Nothing,
                  (($passtoc), ($passtoc), Int), x, x, p)
    end

    function _acb_set(x::($typeofx), y::ComplexElem)
      ccall((:acb_set, libarb), Nothing, (($passtoc), Ref{ComplexElem}), x, y)
    end

    function _acb_set(x::($typeofx), y::ComplexElem, p::Int)
      ccall((:acb_set_round, libarb), Nothing,
                  (($passtoc), Ref{ComplexElem}, Int), x, y, p)
    end

    function _acb_set(x::($typeofx), y::AbstractString, p::Int)
      r = ccall((:acb_real_ptr, libarb), Ptr{RealElem}, (($passtoc), ), x)
      _arb_set(r, y, p)
      i = ccall((:acb_imag_ptr, libarb), Ptr{RealElem}, (($passtoc), ), x)
      ccall((:arb_zero, libarb), Nothing, (Ptr{RealElem}, ), i)
    end

    function _acb_set(x::($typeofx), y::BigFloat)
      r = ccall((:acb_real_ptr, libarb), Ptr{RealElem}, (($passtoc), ), x)
      _arb_set(r, y)
      i = ccall((:acb_imag_ptr, libarb), Ptr{RealElem}, (($passtoc), ), x)
      ccall((:arb_zero, libarb), Nothing, (Ptr{RealElem}, ), i)
    end

    function _acb_set(x::($typeofx), y::BigFloat, p::Int)
      r = ccall((:acb_real_ptr, libarb), Ptr{RealElem}, (($passtoc), ), x)
      _arb_set(r, y, p)
      i = ccall((:acb_imag_ptr, libarb), Ptr{RealElem}, (($passtoc), ), x)
      ccall((:arb_zero, libarb), Nothing, (Ptr{RealElem}, ), i)
    end

    function _acb_set(x::($typeofx), y::Int, z::Int, p::Int)
      ccall((:acb_set_si_si, libarb), Nothing,
                  (($passtoc), Int, Int), x, y, z)
      ccall((:acb_set_round, libarb), Nothing,
                  (($passtoc), ($passtoc), Int), x, x, p)
    end

    function _acb_set(x::($typeofx), y::RealElem, z::RealElem)
      ccall((:acb_set_arb_arb, libarb), Nothing,
                  (($passtoc), Ref{RealElem}, Ref{RealElem}), x, y, z)
    end

    function _acb_set(x::($typeofx), y::RealElem, z::RealElem, p::Int)
      _acb_set(x, y, z)
      ccall((:acb_set_round, libarb), Nothing,
                  (($passtoc), ($passtoc), Int), x, x, p)
    end

    function _acb_set(x::($typeofx), y::QQFieldElem, z::QQFieldElem, p::Int)
      r = ccall((:acb_real_ptr, libarb), Ptr{RealElem}, (($passtoc), ), x)
      _arb_set(r, y, p)
      i = ccall((:acb_imag_ptr, libarb), Ptr{RealElem}, (($passtoc), ), x)
      _arb_set(i, z, p)
    end

    function _acb_set(x::($typeofx), y::T, z::T, p::Int) where {T <: AbstractString}
      r = ccall((:acb_real_ptr, libarb), Ptr{RealElem}, (($passtoc), ), x)
      _arb_set(r, y, p)
      i = ccall((:acb_imag_ptr, libarb), Ptr{RealElem}, (($passtoc), ), x)
      _arb_set(i, z, p)
    end

  end

  for T in (Float64, BigFloat, UInt, ZZRingElem)
    @eval begin
      function _acb_set(x::($typeofx), y::($T), z::($T))
        r = ccall((:acb_real_ptr, libarb), Ptr{RealElem}, (($passtoc), ), x)
        _arb_set(r, y)
        i = ccall((:acb_imag_ptr, libarb), Ptr{RealElem}, (($passtoc), ), x)
        _arb_set(i, z)
      end

      function _acb_set(x::($typeofx), y::($T), z::($T), p::Int)
        r = ccall((:acb_real_ptr, libarb), Ptr{RealElem}, (($passtoc), ), x)
        _arb_set(r, y, p)
        i = ccall((:acb_imag_ptr, libarb), Ptr{RealElem}, (($passtoc), ), x)
        _arb_set(i, z, p)
      end
    end
  end
end

###############################################################################
#
#   Promote rules
#
###############################################################################

promote_rule(::Type{ComplexElem}, ::Type{T}) where {T <: Number} = ComplexElem

promote_rule(::Type{ComplexElem}, ::Type{ZZRingElem}) = ComplexElem

promote_rule(::Type{ComplexElem}, ::Type{QQFieldElem}) = ComplexElem

promote_rule(::Type{ComplexElem}, ::Type{RealElem}) = ComplexElem

################################################################################
#
#  Parent object overload
#
################################################################################

function (r::ComplexField)()
  z = ComplexElem()
  return z
end

function (r::ComplexField)(x::Union{Int, UInt, ZZRingElem, QQFieldElem, RealElem, ComplexElem, Float64,
                                    BigFloat, AbstractString}; precision::Int = precision(Balls))
  z = ComplexElem(x, precision)
  return z
end

(r::ComplexField)(x::Integer; precision::Int = precision(Balls)) = r(ZZRingElem(x); precision = precision)

(r::ComplexField)(x::Rational{T}; precision::Int = precision(Balls)) where {T <: Integer} = r(QQFieldElem(x), precision = precision)

function (r::ComplexField)(x::T, y::T; precision::Int = precision(Balls)) where {T <: Union{Int, UInt, ZZRingElem, QQFieldElem, RealElem, Float64, BigFloat, AbstractString}}
  z = ComplexElem(x, y, precision)
  return z
end

for S in (Int, UInt, ZZRingElem, QQFieldElem, RealElem, Float64, BigFloat, AbstractString, BigInt)
  for T in (Int, UInt, ZZRingElem, QQFieldElem, RealElem, Float64, BigFloat, AbstractString, BigInt)
    if S != T
      @eval begin
        function (r::ComplexField)(x::$(S), y::$(T); precision::Int = precision(Balls))
          z = ComplexElem(RealField()(x), RealField()(y), precision)
          return z
        end
      end
    end
  end
end

for T in (Int, UInt, ZZRingElem, QQFieldElem, RealElem, Float64, BigFloat, AbstractString, BigInt)
  @eval begin
    function (r::ComplexField)(x::Rational{S}, y::$(T); precision::Int = precision(Balls)) where {S <: Integer}
      z = ComplexElem(RealField()(x), RealField()(y), precision)
      return z
    end
    function (r::ComplexField)(x::$(T), y::Rational{S}; precision::Int = precision(Balls)) where {S <: Integer}
      z = ComplexElem(RealField()(x), RealField()(y), precision)
      return z
    end
  end
end

(r::ComplexField)(x::BigInt, y::BigInt; precision::Int = precision(Balls)) = r(ZZRingElem(x), ZZRingElem(y), precision = precision)

(r::ComplexField)(x::Rational{S}, y::Rational{T}; precision::Int = precision(Balls)) where {S <: Integer, T <: Integer} =
      r(QQFieldElem(x), QQFieldElem(y); precision = precision)

################################################################################
#
#  ComplexField constructor
#
################################################################################

# see internal constructor
