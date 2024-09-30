###############################################################################
#
#   Complex.jl : Arb complex numbers
#
#   Copyright (C) 2015 Tommy Hofmann
#   Copyright (C) 2015 Fredrik Johansson
#
###############################################################################

###############################################################################
#
#   Basic manipulation
#
###############################################################################

elem_type(::Type{ComplexField}) = ComplexFieldElem

parent_type(::Type{ComplexFieldElem}) = ComplexField

base_ring_type(::Type{ComplexField}) = typeof(Union{})

base_ring(R::ComplexField) = Union{}

parent(x::ComplexFieldElem) = ComplexField()

is_domain_type(::Type{ComplexFieldElem}) = true

is_exact_type(::Type{ComplexFieldElem}) = false

function zero(r::ComplexField)
  z = ComplexFieldElem()
  return z
end

function one(r::ComplexField)
  z = ComplexFieldElem()
  ccall((:acb_one, libflint), Nothing, (Ref{ComplexFieldElem}, ), z)
  return z
end

@doc raw"""
    onei(r::ComplexField)

Return exact one times $i$ in the given Arb complex field.
"""
function onei(r::ComplexField)
  z = ComplexFieldElem()
  ccall((:acb_onei, libflint), Nothing, (Ref{ComplexFieldElem}, ), z)
  return z
end

@doc raw"""
    accuracy_bits(x::ComplexFieldElem)

Return the relative accuracy of $x$ measured in bits, capped between
`typemax(Int)` and `-typemax(Int)`.
"""
function accuracy_bits(x::ComplexFieldElem)
  # bug in acb.h: rel_accuracy_bits is not in the library
  return -ccall((:acb_rel_error_bits, libflint), Int, (Ref{ComplexFieldElem},), x)
end

function deepcopy_internal(a::ComplexFieldElem, dict::IdDict)
  b = ComplexFieldElem()
  ccall((:acb_set, libflint), Nothing, (Ref{ComplexFieldElem}, Ref{ComplexFieldElem}), b, a)
  return b
end

function canonical_unit(x::ComplexFieldElem)
  return x
end

# TODO: implement hash

characteristic(::ComplexField) = 0

################################################################################
#
#  Conversions
#
################################################################################

function convert(::Type{ComplexF64}, x::ComplexFieldElem)
  GC.@preserve x begin
    re = ccall((:acb_real_ptr, libflint), Ptr{arb_struct}, (Ref{ComplexFieldElem}, ), x)
    im = ccall((:acb_imag_ptr, libflint), Ptr{arb_struct}, (Ref{ComplexFieldElem}, ), x)
    t = ccall((:arb_mid_ptr, libflint), Ptr{arf_struct}, (Ptr{RealFieldElem}, ), re)
    u = ccall((:arb_mid_ptr, libflint), Ptr{arf_struct}, (Ptr{RealFieldElem}, ), im)
    # 4 == round to nearest
    v = ccall((:arf_get_d, libflint), Float64, (Ptr{arf_struct}, Int), t, 4)
    w = ccall((:arf_get_d, libflint), Float64, (Ptr{arf_struct}, Int), u, 4)
  end
  return complex(v, w)
end

################################################################################
#
#  Real and imaginary part
#
################################################################################

function real(x::ComplexFieldElem)
  z = RealFieldElem()
  ccall((:acb_get_real, libflint), Nothing, (Ref{RealFieldElem}, Ref{ComplexFieldElem}), z, x)
  return z
end

function imag(x::ComplexFieldElem)
  z = RealFieldElem()
  ccall((:acb_get_imag, libflint), Nothing, (Ref{RealFieldElem}, Ref{ComplexFieldElem}), z, x)
  return z
end

################################################################################
#
#  String I/O
#
################################################################################

function expressify(z::ComplexFieldElem; context = nothing)
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

function Base.show(io::IO, ::MIME"text/plain", z::ComplexFieldElem)
  print(io, AbstractAlgebra.obj_to_string(z, context = io))
end

function Base.show(io::IO, z::ComplexFieldElem)
  print(io, AbstractAlgebra.obj_to_string(z, context = io))
end

function show(io::IO, x::ComplexField)
  # deliberately no @show_name or @show_special here as this is a singleton type
  if is_terse(io)
    print(io, LowercaseOff(), "CC")
  else
    print(io, "Complex field")
  end
end

################################################################################
#
#  Unary operations
#
################################################################################

function -(x::ComplexFieldElem)
  z = ComplexFieldElem()
  ccall((:acb_neg, libflint), Nothing, (Ref{ComplexFieldElem}, Ref{ComplexFieldElem}), z, x)
  return z
end

################################################################################
#
#  Binary operations
#
################################################################################

# AcbFieldElem - AcbFieldElem

for (s,f) in ((:+,"acb_add"), (:*,"acb_mul"), (://, "acb_div"), (:-,"acb_sub"), (:^,"acb_pow"))
  @eval begin
    function ($s)(x::ComplexFieldElem, y::ComplexFieldElem, prec::Int = precision(Balls))
      z = ComplexFieldElem()
      ccall(($f, libflint), Nothing, (Ref{ComplexFieldElem}, Ref{ComplexFieldElem}, Ref{ComplexFieldElem}, Int),
            z, x, y, prec)
      return z
    end
  end
end

for (f,s) in ((:+, "add"), (:-, "sub"), (:*, "mul"), (://, "div"), (:^, "pow"))
  @eval begin

    function ($f)(x::ComplexFieldElem, y::UInt, prec::Int = precision(Balls))
      z = ComplexFieldElem()
      ccall(($("acb_"*s*"_ui"), libflint), Nothing,
            (Ref{ComplexFieldElem}, Ref{ComplexFieldElem}, UInt, Int),
            z, x, y, prec)
      return z
    end

    function ($f)(x::ComplexFieldElem, y::Int, prec::Int = precision(Balls))
      z = ComplexFieldElem()
      ccall(($("acb_"*s*"_si"), libflint), Nothing,
            (Ref{ComplexFieldElem}, Ref{ComplexFieldElem}, Int, Int), z, x, y, prec)
      return z
    end

    function ($f)(x::ComplexFieldElem, y::ZZRingElem, prec::Int = precision(Balls))
      z = ComplexFieldElem()
      ccall(($("acb_"*s*"_fmpz"), libflint), Nothing,
            (Ref{ComplexFieldElem}, Ref{ComplexFieldElem}, Ref{ZZRingElem}, Int),
            z, x, y, prec)
      return z
    end

    function ($f)(x::ComplexFieldElem, y::RealFieldElem, prec::Int = precision(Balls))
      z = ComplexFieldElem()
      ccall(($("acb_"*s*"_arb"), libflint), Nothing,
            (Ref{ComplexFieldElem}, Ref{ComplexFieldElem}, Ref{RealFieldElem}, Int),
            z, x, y, prec)
      return z
    end
  end
end


+(x::UInt,y::ComplexFieldElem) = +(y,x)
+(x::Int,y::ComplexFieldElem) = +(y,x)
+(x::ZZRingElem,y::ComplexFieldElem) = +(y,x)
+(x::RealFieldElem,y::ComplexFieldElem) = +(y,x)

*(x::UInt,y::ComplexFieldElem) = *(y,x)
*(x::Int,y::ComplexFieldElem) = *(y,x)
*(x::ZZRingElem,y::ComplexFieldElem) = *(y,x)
*(x::RealFieldElem,y::ComplexFieldElem) = *(y,x)

//(x::UInt,y::ComplexFieldElem) = (x == 1) ? inv(y) : parent(y)(x) // y
//(x::Int,y::ComplexFieldElem) = (x == 1) ? inv(y) : parent(y)(x) // y
//(x::ZZRingElem,y::ComplexFieldElem) = isone(x) ? inv(y) : parent(y)(x) // y
//(x::RealFieldElem,y::ComplexFieldElem) = isone(x) ? inv(y) : parent(y)(x) // y

^(x::ZZRingElem,y::ComplexFieldElem) = parent(y)(x) ^ y
^(x::RealFieldElem,y::ComplexFieldElem) = parent(y)(x) ^ y

function -(x::UInt, y::ComplexFieldElem)
  z = ComplexFieldElem()
  ccall((:acb_sub_ui, libflint), Nothing, (Ref{ComplexFieldElem}, Ref{ComplexFieldElem}, UInt, Int), z, y, x, precision(Balls))
  ccall((:acb_neg, libflint), Nothing, (Ref{ComplexFieldElem}, Ref{ComplexFieldElem}), z, z)
  return z
end

function -(x::Int, y::ComplexFieldElem)
  z = ComplexFieldElem()
  ccall((:acb_sub_si, libflint), Nothing, (Ref{ComplexFieldElem}, Ref{ComplexFieldElem}, Int, Int), z, y, x, precision(Balls))
  ccall((:acb_neg, libflint), Nothing, (Ref{ComplexFieldElem}, Ref{ComplexFieldElem}), z, z)
  return z
end

function -(x::ZZRingElem, y::ComplexFieldElem)
  z = ComplexFieldElem()
  ccall((:acb_sub_fmpz, libflint), Nothing, (Ref{ComplexFieldElem}, Ref{ComplexFieldElem}, Ref{ZZRingElem}, Int), z, y, x, precision(Balls))
  ccall((:acb_neg, libflint), Nothing, (Ref{ComplexFieldElem}, Ref{ComplexFieldElem}), z, z)
  return z
end

function -(x::RealFieldElem, y::ComplexFieldElem)
  z = ComplexFieldElem()
  ccall((:acb_sub_arb, libflint), Nothing, (Ref{ComplexFieldElem}, Ref{ComplexFieldElem}, Ref{RealFieldElem}, Int), z, y, x, precision(Balls))
  ccall((:acb_neg, libflint), Nothing, (Ref{ComplexFieldElem}, Ref{ComplexFieldElem}), z, z)
  return z
end

+(x::ComplexFieldElem, y::Integer) = x + flintify(y)

-(x::ComplexFieldElem, y::Integer) = x - flintify(y)

*(x::ComplexFieldElem, y::Integer) = x*flintify(y)

//(x::ComplexFieldElem, y::Integer) = x//flintify(y)

+(x::Integer, y::ComplexFieldElem) = flintify(x) + y

-(x::Integer, y::ComplexFieldElem) = flintify(x) - y

*(x::Integer, y::ComplexFieldElem) = flintify(x)*y

//(x::Integer, y::ComplexFieldElem) = flintify(x)//y

divexact(x::ComplexFieldElem, y::ComplexFieldElem; check::Bool=true) = x // y
divexact(x::ZZRingElem, y::ComplexFieldElem; check::Bool=true) = x // y
divexact(x::ComplexFieldElem, y::ZZRingElem; check::Bool=true) = x // y
divexact(x::RealFieldElem, y::ComplexFieldElem; check::Bool=true) = x // y
divexact(x::ComplexFieldElem, y::RealFieldElem; check::Bool=true) = x // y

/(x::ComplexFieldElem, y::ComplexFieldElem) = x // y
/(x::ZZRingElem, y::ComplexFieldElem) = x // y
/(x::ComplexFieldElem, y::ZZRingElem) = x // y
/(x::RealFieldElem, y::ComplexFieldElem) = x // y
/(x::ComplexFieldElem, y::RealFieldElem) = x // y

for T in (Float64, BigFloat, Rational, QQFieldElem)
  @eval begin
    +(x::$T, y::ComplexFieldElem) = parent(y)(x) + y
    +(x::ComplexFieldElem, y::$T) = x + parent(x)(y)
    -(x::$T, y::ComplexFieldElem) = parent(y)(x) - y
    -(x::ComplexFieldElem, y::$T) = x - parent(x)(y)
    *(x::$T, y::ComplexFieldElem) = parent(y)(x) * y
    *(x::ComplexFieldElem, y::$T) = x * parent(x)(y)
    //(x::$T, y::ComplexFieldElem) = parent(y)(x) // y
    //(x::ComplexFieldElem, y::$T) = x // parent(x)(y)
  end
end

for T in (Float64, BigFloat, Integer, Rational, QQFieldElem)
  @eval begin
    ^(x::$T, y::ComplexFieldElem) = parent(y)(x)^y
    ^(x::ComplexFieldElem, y::$T) = x ^ parent(x)(y)
    /(x::$T, y::ComplexFieldElem) = x // y
    /(x::ComplexFieldElem, y::$T) = x // y
    divexact(x::$T, y::ComplexFieldElem; check::Bool=true) = x // y
    divexact(x::ComplexFieldElem, y::$T; check::Bool=true) = x // y
  end
end

################################################################################
#
#  Comparison
#
################################################################################

@doc raw"""
    isequal(x::ComplexFieldElem, y::ComplexFieldElem)

Return `true` if the boxes $x$ and $y$ are precisely equal, i.e. their real
and imaginary parts have the same midpoints and radii.
"""
function isequal(x::ComplexFieldElem, y::ComplexFieldElem)
  r = ccall((:acb_equal, libflint), Cint, (Ref{ComplexFieldElem}, Ref{ComplexFieldElem}), x, y)
  return Bool(r)
end

function ==(x::ComplexFieldElem, y::ComplexFieldElem)
  r = ccall((:acb_eq, libflint), Cint, (Ref{ComplexFieldElem}, Ref{ComplexFieldElem}), x, y)
  return Bool(r)
end

function !=(x::ComplexFieldElem, y::ComplexFieldElem)
  r = ccall((:acb_ne, libflint), Cint, (Ref{ComplexFieldElem}, Ref{ComplexFieldElem}), x, y)
  return Bool(r)
end

==(x::ComplexFieldElem,y::Int) = (x == parent(x)(y))
==(x::Int,y::ComplexFieldElem) = (y == parent(y)(x))

==(x::ComplexFieldElem,y::RealFieldElem) = (x == parent(x)(y))
==(x::RealFieldElem,y::ComplexFieldElem) = (y == parent(y)(x))

==(x::ComplexFieldElem,y::ZZRingElem) = (x == parent(x)(y))
==(x::ZZRingElem,y::ComplexFieldElem) = (y == parent(y)(x))

==(x::ComplexFieldElem,y::Integer) = x == flintify(y)
==(x::Integer,y::ComplexFieldElem) = flintify(x) == y

==(x::ComplexFieldElem,y::Float64) = (x == parent(x)(y))
==(x::Float64,y::ComplexFieldElem) = (y == parent(y)(x))

!=(x::ComplexFieldElem,y::Int) = (x != parent(x)(y))
!=(x::Int,y::ComplexFieldElem) = (y != parent(y)(x))

!=(x::ComplexFieldElem,y::RealFieldElem) = (x != parent(x)(y))
!=(x::RealFieldElem,y::ComplexFieldElem) = (y != parent(y)(x))

!=(x::ComplexFieldElem,y::ZZRingElem) = (x != parent(x)(y))
!=(x::ZZRingElem,y::ComplexFieldElem) = (y != parent(y)(x))

!=(x::ComplexFieldElem,y::Float64) = (x != parent(x)(y))
!=(x::Float64,y::ComplexFieldElem) = (y != parent(y)(x))

################################################################################
#
#  Containment
#
################################################################################

@doc raw"""
    overlaps(x::ComplexFieldElem, y::ComplexFieldElem)

Returns `true` if any part of the box $x$ overlaps any part of the box $y$,
otherwise return `false`.
"""
function overlaps(x::ComplexFieldElem, y::ComplexFieldElem)
  r = ccall((:acb_overlaps, libflint), Cint, (Ref{ComplexFieldElem}, Ref{ComplexFieldElem}), x, y)
  return Bool(r)
end

@doc raw"""
    contains(x::ComplexFieldElem, y::ComplexFieldElem)

Returns `true` if the box $x$ contains the box $y$, otherwise return
`false`.
"""
function contains(x::ComplexFieldElem, y::ComplexFieldElem)
  r = ccall((:acb_contains, libflint), Cint, (Ref{ComplexFieldElem}, Ref{ComplexFieldElem}), x, y)
  return Bool(r)
end

@doc raw"""
    contains(x::ComplexFieldElem, y::QQFieldElem)

Returns `true` if the box $x$ contains the given rational value, otherwise
return `false`.
"""
function contains(x::ComplexFieldElem, y::QQFieldElem)
  r = ccall((:acb_contains_fmpq, libflint), Cint, (Ref{ComplexFieldElem}, Ref{QQFieldElem}), x, y)
  return Bool(r)
end

@doc raw"""
    contains(x::ComplexFieldElem, y::ZZRingElem)

Returns `true` if the box $x$ contains the given integer value, otherwise
return `false`.
"""
function contains(x::ComplexFieldElem, y::ZZRingElem)
  r = ccall((:acb_contains_fmpz, libflint), Cint, (Ref{ComplexFieldElem}, Ref{ZZRingElem}), x, y)
  return Bool(r)
end

function contains(x::ComplexFieldElem, y::Int)
  v = ZZRingElem(y)
  r = ccall((:acb_contains_fmpz, libflint), Cint, (Ref{ComplexFieldElem}, Ref{ZZRingElem}), x, v)
  return Bool(r)
end

@doc raw"""
    contains(x::ComplexFieldElem, y::Integer)

Returns `true` if the box $x$ contains the given integer value, otherwise
return `false`.
"""
contains(x::ComplexFieldElem, y::Integer) = contains(x, ZZRingElem(y))

@doc raw"""
    contains(x::ComplexFieldElem, y::Rational{T}) where {T <: Integer}

Returns `true` if the box $x$ contains the given rational value, otherwise
return `false`.
"""
contains(x::ComplexFieldElem, y::Rational{T}) where {T <: Integer} = contains(x, ZZRingElem(y))

@doc raw"""
    contains_zero(x::ComplexFieldElem)

Returns `true` if the box $x$ contains zero, otherwise return `false`.
"""
function contains_zero(x::ComplexFieldElem)
  return Bool(ccall((:acb_contains_zero, libflint), Cint, (Ref{ComplexFieldElem},), x))
end

################################################################################
#
#  Predicates
#
################################################################################

function is_unit(x::ComplexFieldElem)
  !iszero(x)
end

@doc raw"""
    iszero(x::ComplexFieldElem)

Return `true` if $x$ is certainly zero, otherwise return `false`.
"""
function iszero(x::ComplexFieldElem)
  return Bool(ccall((:acb_is_zero, libflint), Cint, (Ref{ComplexFieldElem},), x))
end

@doc raw"""
    isone(x::ComplexFieldElem)

Return `true` if $x$ is certainly one, otherwise return `false`.
"""
function isone(x::ComplexFieldElem)
  return Bool(ccall((:acb_is_one, libflint), Cint, (Ref{ComplexFieldElem},), x))
end

@doc raw"""
    isfinite(x::ComplexFieldElem)

Return `true` if $x$ is finite, i.e. its real and imaginary parts have finite
midpoint and radius, otherwise return `false`.
"""
function isfinite(x::ComplexFieldElem)
  return Bool(ccall((:acb_is_finite, libflint), Cint, (Ref{ComplexFieldElem},), x))
end

@doc raw"""
    is_exact(x::ComplexFieldElem)

Return `true` if $x$ is exact, i.e. has its real and imaginary parts have
zero radius, otherwise return `false`.
"""
function is_exact(x::ComplexFieldElem)
  return Bool(ccall((:acb_is_exact, libflint), Cint, (Ref{ComplexFieldElem},), x))
end

@doc raw"""
    isinteger(x::ComplexFieldElem)

Return `true` if $x$ is an exact integer, otherwise return `false`.
"""
function isinteger(x::ComplexFieldElem)
  return Bool(ccall((:acb_is_int, libflint), Cint, (Ref{ComplexFieldElem},), x))
end

function isreal(x::ComplexFieldElem)
  return Bool(ccall((:acb_is_real, libflint), Cint, (Ref{ComplexFieldElem},), x))
end

is_negative(x::ComplexFieldElem) = isreal(x) && is_negative(real(x))

################################################################################
#
#  Absolute value
#
################################################################################

function abs(x::ComplexFieldElem, prec::Int = precision(Balls))
  z = RealFieldElem()
  ccall((:acb_abs, libflint), Nothing,
        (Ref{RealFieldElem}, Ref{ComplexFieldElem}, Int), z, x, prec)
  return z
end

################################################################################
#
#  Inversion
#
################################################################################

function inv(x::ComplexFieldElem, prec::Int = precision(Balls))
  z = ComplexFieldElem()
  ccall((:acb_inv, libflint), Nothing, (Ref{ComplexFieldElem}, Ref{ComplexFieldElem}, Int), z, x, prec)
  return z
end

################################################################################
#
#  Shifting
#
################################################################################

function ldexp(x::ComplexFieldElem, y::Int)
  z = ComplexFieldElem()
  ccall((:acb_mul_2exp_si, libflint), Nothing,
        (Ref{ComplexFieldElem}, Ref{ComplexFieldElem}, Int), z, x, y)
  return z
end

function ldexp(x::ComplexFieldElem, y::ZZRingElem)
  z = ComplexFieldElem()
  ccall((:acb_mul_2exp_fmpz, libflint), Nothing,
        (Ref{ComplexFieldElem}, Ref{ComplexFieldElem}, Ref{ZZRingElem}), z, x, y)
  return z
end

################################################################################
#
#  Miscellaneous
#
################################################################################

@doc raw"""
    trim(x::ComplexFieldElem)

Return an `ComplexFieldElem` box containing $x$ but which may be more economical,
by rounding off insignificant bits from midpoints.
"""
function trim(x::ComplexFieldElem)
  z = ComplexFieldElem()
  ccall((:acb_trim, libflint), Nothing, (Ref{ComplexFieldElem}, Ref{ComplexFieldElem}), z, x)
  return z
end

@doc raw"""
    unique_integer(x::ComplexFieldElem)

Return a pair where the first value is a boolean and the second is an `ZZRingElem`
integer. The boolean indicates whether the box $x$ contains a unique
integer. If this is the case, the second return value is set to this unique
integer.
"""
function unique_integer(x::ComplexFieldElem)
  z = ZZRingElem()
  unique = ccall((:acb_get_unique_fmpz, libflint), Int,
                 (Ref{ZZRingElem}, Ref{ComplexFieldElem}), z, x)
  return (unique != 0, z)
end

function conj(x::ComplexFieldElem)
  z = ComplexFieldElem()
  ccall((:acb_conj, libflint), Nothing, (Ref{ComplexFieldElem}, Ref{ComplexFieldElem}), z, x)
  return z
end

function angle(x::ComplexFieldElem, prec::Int = precision(Balls))
  z = RealFieldElem()
  ccall((:acb_arg, libflint), Nothing,
        (Ref{RealFieldElem}, Ref{ComplexFieldElem}, Int), z, x, prec)
  return z
end

################################################################################
#
#  Constants
#
################################################################################

@doc raw"""
    const_pi(r::ComplexField)

Return $\pi = 3.14159\ldots$ as an element of $r$.
"""
function const_pi(r::ComplexField, prec::Int = precision(Balls))
  z = r()
  ccall((:acb_const_pi, libflint), Nothing, (Ref{ComplexFieldElem}, Int), z, prec)
  return z
end

################################################################################
#
#  Complex valued functions
#
################################################################################

# complex - complex functions

function Base.sqrt(x::ComplexFieldElem, prec::Int = precision(Balls); check::Bool=true)
  z = ComplexFieldElem()
  ccall((:acb_sqrt, libflint), Nothing, (Ref{ComplexFieldElem}, Ref{ComplexFieldElem}, Int), z, x, prec)
  return z
end

@doc raw"""
    rsqrt(x::ComplexFieldElem)

Return the reciprocal of the square root of $x$, i.e. $1/\sqrt{x}$.
"""
function rsqrt(x::ComplexFieldElem, prec::Int = precision(Balls))
  z = ComplexFieldElem()
  ccall((:acb_rsqrt, libflint), Nothing, (Ref{ComplexFieldElem}, Ref{ComplexFieldElem}, Int), z, x, prec)
  return z
end

@doc raw"""
    root(x::ComplexFieldElem, n::Int)

Return the principal $n$-th root of $x$.
"""
function root(x::ComplexFieldElem, n::Int, prec::Int = precision(Balls))
  z = ComplexFieldElem()
  n == 0 && error("cannot take 0-th root")
  if n < 0
    n = -n
    x = inv(x)
  end
  ccall((:acb_root_ui, libflint), Nothing, (Ref{ComplexFieldElem}, Ref{ComplexFieldElem}, UInt, Int), z, x, UInt(n), prec)
  return z
end


function log(x::ComplexFieldElem, prec::Int = precision(Balls))
  z = ComplexFieldElem()
  ccall((:acb_log, libflint), Nothing, (Ref{ComplexFieldElem}, Ref{ComplexFieldElem}, Int), z, x, prec)
  return z
end

function log1p(x::ComplexFieldElem, prec::Int = precision(Balls))
  z = ComplexFieldElem()
  ccall((:acb_log1p, libflint), Nothing, (Ref{ComplexFieldElem}, Ref{ComplexFieldElem}, Int), z, x, prec)
  return z
end

function Base.exp(x::ComplexFieldElem, prec::Int = precision(Balls))
  z = ComplexFieldElem()
  ccall((:acb_exp, libflint), Nothing, (Ref{ComplexFieldElem}, Ref{ComplexFieldElem}, Int), z, x, prec)
  return z
end

function Base.expm1(x::ComplexFieldElem, prec::Int = precision(Balls))
  z = ComplexFieldElem()
  ccall((:acb_expm1, libflint), Nothing, (Ref{ComplexFieldElem}, Ref{ComplexFieldElem}, Int), z, x, prec)
  return z
end

@doc raw"""
    cispi(x::ComplexFieldElem)

Return the exponential of $\pi i x$.
"""
function cispi(x::ComplexFieldElem, prec::Int = precision(Balls))
  z = ComplexFieldElem()
  ccall((:acb_exp_pi_i, libflint), Nothing, (Ref{ComplexFieldElem}, Ref{ComplexFieldElem}, Int), z, x, prec)
  return z
end

@doc raw"""
    root_of_unity(C::ComplexField, k::Int)

Return $\exp(2\pi i/k)$.
"""
function root_of_unity(C::ComplexField, k::Int, prec::Int = precision(Balls))
  k <= 0 && throw(ArgumentError("Order must be positive ($k)"))
  z = C()
  ccall((:acb_unit_root, libflint), Nothing, (Ref{ComplexFieldElem}, UInt, Int), z, k, prec)
  return z
end

function sin(x::ComplexFieldElem, prec::Int = precision(Balls))
  z = ComplexFieldElem()
  ccall((:acb_sin, libflint), Nothing, (Ref{ComplexFieldElem}, Ref{ComplexFieldElem}, Int), z, x, prec)
  return z
end

function cos(x::ComplexFieldElem, prec::Int = precision(Balls))
  z = ComplexFieldElem()
  ccall((:acb_cos, libflint), Nothing, (Ref{ComplexFieldElem}, Ref{ComplexFieldElem}, Int), z, x, prec)
  return z
end

function tan(x::ComplexFieldElem, prec::Int = precision(Balls))
  z = ComplexFieldElem()
  ccall((:acb_tan, libflint), Nothing, (Ref{ComplexFieldElem}, Ref{ComplexFieldElem}, Int), z, x, prec)
  return z
end

function cot(x::ComplexFieldElem, prec::Int = precision(Balls))
  z = ComplexFieldElem()
  ccall((:acb_cot, libflint), Nothing, (Ref{ComplexFieldElem}, Ref{ComplexFieldElem}, Int), z, x, prec)
  return z
end

function sinpi(x::ComplexFieldElem, prec::Int = precision(Balls))
  z = ComplexFieldElem()
  ccall((:acb_sin_pi, libflint), Nothing, (Ref{ComplexFieldElem}, Ref{ComplexFieldElem}, Int), z, x, prec)
  return z
end

function cospi(x::ComplexFieldElem, prec::Int = precision(Balls))
  z = ComplexFieldElem()
  ccall((:acb_cos_pi, libflint), Nothing, (Ref{ComplexFieldElem}, Ref{ComplexFieldElem}, Int), z, x, prec)
  return z
end

function tanpi(x::ComplexFieldElem, prec::Int = precision(Balls))
  z = ComplexFieldElem()
  ccall((:acb_tan_pi, libflint), Nothing, (Ref{ComplexFieldElem}, Ref{ComplexFieldElem}, Int), z, x, prec)
  return z
end

function cotpi(x::ComplexFieldElem, prec::Int = precision(Balls))
  z = ComplexFieldElem()
  ccall((:acb_cot_pi, libflint), Nothing, (Ref{ComplexFieldElem}, Ref{ComplexFieldElem}, Int), z, x, prec)
  return z
end

function sinh(x::ComplexFieldElem, prec::Int = precision(Balls))
  z = ComplexFieldElem()
  ccall((:acb_sinh, libflint), Nothing, (Ref{ComplexFieldElem}, Ref{ComplexFieldElem}, Int), z, x, prec)
  return z
end

function cosh(x::ComplexFieldElem, prec::Int = precision(Balls))
  z = ComplexFieldElem()
  ccall((:acb_cosh, libflint), Nothing, (Ref{ComplexFieldElem}, Ref{ComplexFieldElem}, Int), z, x, prec)
  return z
end

function tanh(x::ComplexFieldElem, prec::Int = precision(Balls))
  z = ComplexFieldElem()
  ccall((:acb_tanh, libflint), Nothing, (Ref{ComplexFieldElem}, Ref{ComplexFieldElem}, Int), z, x, prec)
  return z
end

function coth(x::ComplexFieldElem, prec::Int = precision(Balls))
  z = ComplexFieldElem()
  ccall((:acb_coth, libflint), Nothing, (Ref{ComplexFieldElem}, Ref{ComplexFieldElem}, Int), z, x, prec)
  return z
end

function atan(x::ComplexFieldElem, prec::Int = precision(Balls))
  z = ComplexFieldElem()
  ccall((:acb_atan, libflint), Nothing, (Ref{ComplexFieldElem}, Ref{ComplexFieldElem}, Int), z, x, prec)
  return z
end

@doc raw"""
    log_sinpi(x::ComplexFieldElem)

Return $\log\sin(\pi x)$, constructed without branch cuts off the real line.
"""
function log_sinpi(x::ComplexFieldElem, prec::Int = precision(Balls))
  z = ComplexFieldElem()
  ccall((:acb_log_sin_pi, libflint), Nothing, (Ref{ComplexFieldElem}, Ref{ComplexFieldElem}, Int), z, x, prec)
  return z
end

@doc raw"""
    gamma(x::ComplexFieldElem)

Return the Gamma function evaluated at $x$.
"""
function gamma(x::ComplexFieldElem, prec::Int = precision(Balls))
  z = ComplexFieldElem()
  ccall((:acb_gamma, libflint), Nothing, (Ref{ComplexFieldElem}, Ref{ComplexFieldElem}, Int), z, x, prec)
  return z
end

@doc raw"""
    rgamma(x::ComplexFieldElem)

Return the reciprocal of the Gamma function evaluated at $x$.
"""
function rgamma(x::ComplexFieldElem, prec::Int = precision(Balls))
  z = ComplexFieldElem()
  ccall((:acb_rgamma, libflint), Nothing, (Ref{ComplexFieldElem}, Ref{ComplexFieldElem}, Int), z, x, prec)
  return z
end

@doc raw"""
    lgamma(x::ComplexFieldElem)

Return the logarithm of the Gamma function evaluated at $x$.
"""
function lgamma(x::ComplexFieldElem, prec::Int = precision(Balls))
  z = ComplexFieldElem()
  ccall((:acb_lgamma, libflint), Nothing, (Ref{ComplexFieldElem}, Ref{ComplexFieldElem}, Int), z, x, prec)
  return z
end

@doc raw"""
    digamma(x::ComplexFieldElem)

Return the  logarithmic derivative of the gamma function evaluated at $x$,
i.e. $\psi(x)$.
"""
function digamma(x::ComplexFieldElem, prec::Int = precision(Balls))
  z = ComplexFieldElem()
  ccall((:acb_digamma, libflint), Nothing, (Ref{ComplexFieldElem}, Ref{ComplexFieldElem}, Int), z, x, prec)
  return z
end

@doc raw"""
    zeta(x::ComplexFieldElem)

Return the Riemann zeta function evaluated at $x$.
"""
function zeta(x::ComplexFieldElem, prec::Int = precision(Balls))
  z = ComplexFieldElem()
  ccall((:acb_zeta, libflint), Nothing, (Ref{ComplexFieldElem}, Ref{ComplexFieldElem}, Int), z, x, prec)
  return z
end

@doc raw"""
    barnes_g(x::ComplexFieldElem)

Return the Barnes $G$-function, evaluated at $x$.
"""
function barnes_g(x::ComplexFieldElem, prec::Int = precision(Balls))
  z = ComplexFieldElem()
  ccall((:acb_barnes_g, libflint), Nothing, (Ref{ComplexFieldElem}, Ref{ComplexFieldElem}, Int), z, x, prec)
  return z
end

@doc raw"""
    log_barnes_g(x::ComplexFieldElem)

Return the logarithm of the Barnes $G$-function, evaluated at $x$.
"""
function log_barnes_g(x::ComplexFieldElem, prec::Int = precision(Balls))
  z = ComplexFieldElem()
  ccall((:acb_log_barnes_g, libflint), Nothing, (Ref{ComplexFieldElem}, Ref{ComplexFieldElem}, Int), z, x, prec)
  return z
end

@doc raw"""
    agm(x::ComplexFieldElem)

Return the arithmetic-geometric mean of $1$ and $x$.
"""
function agm(x::ComplexFieldElem, prec::Int = precision(Balls))
  z = ComplexFieldElem()
  ccall((:acb_agm1, libflint), Nothing, (Ref{ComplexFieldElem}, Ref{ComplexFieldElem}, Int), z, x, prec)
  return z
end

@doc raw"""
    erf(x::ComplexFieldElem)

Return the error function evaluated at $x$.
"""
function erf(x::ComplexFieldElem, prec::Int = precision(Balls))
  z = ComplexFieldElem()
  ccall((:acb_hypgeom_erf, libflint), Nothing, (Ref{ComplexFieldElem}, Ref{ComplexFieldElem}, Int), z, x, prec)
  return z
end

@doc raw"""
    erfi(x::ComplexFieldElem)

Return the imaginary error function evaluated at $x$.
"""
function erfi(x::ComplexFieldElem, prec::Int = precision(Balls))
  z = ComplexFieldElem()
  ccall((:acb_hypgeom_erfi, libflint), Nothing, (Ref{ComplexFieldElem}, Ref{ComplexFieldElem}, Int), z, x, prec)
  return z
end

@doc raw"""
    erfc(x::ComplexFieldElem)

Return the complementary error function evaluated at $x$.
"""
function erfc(x::ComplexFieldElem, prec::Int = precision(Balls))
  z = ComplexFieldElem()
  ccall((:acb_hypgeom_erfc, libflint), Nothing, (Ref{ComplexFieldElem}, Ref{ComplexFieldElem}, Int), z, x, prec)
  return z
end

@doc raw"""
    exp_integral_ei(x::ComplexFieldElem)

Return the exponential integral evaluated at $x$.
"""
function exp_integral_ei(x::ComplexFieldElem, prec::Int = precision(Balls))
  z = ComplexFieldElem()
  ccall((:acb_hypgeom_ei, libflint), Nothing, (Ref{ComplexFieldElem}, Ref{ComplexFieldElem}, Int), z, x, prec)
  return z
end

@doc raw"""
    sin_integral(x::ComplexFieldElem)

Return the sine integral evaluated at $x$.
"""
function sin_integral(x::ComplexFieldElem, prec::Int = precision(Balls))
  z = ComplexFieldElem()
  ccall((:acb_hypgeom_si, libflint), Nothing, (Ref{ComplexFieldElem}, Ref{ComplexFieldElem}, Int), z, x, prec)
  return z
end

@doc raw"""
    cos_integral(x::ComplexFieldElem)

Return the exponential cosine integral evaluated at $x$.
"""
function cos_integral(x::ComplexFieldElem, prec::Int = precision(Balls))
  z = ComplexFieldElem()
  ccall((:acb_hypgeom_ci, libflint), Nothing, (Ref{ComplexFieldElem}, Ref{ComplexFieldElem}, Int), z, x, prec)
  return z
end

@doc raw"""
    sinh_integral(x::ComplexFieldElem)

Return the hyperbolic sine integral evaluated at $x$.
"""
function sinh_integral(x::ComplexFieldElem, prec::Int = precision(Balls))
  z = ComplexFieldElem()
  ccall((:acb_hypgeom_shi, libflint), Nothing, (Ref{ComplexFieldElem}, Ref{ComplexFieldElem}, Int), z, x, prec)
  return z
end

@doc raw"""
    cosh_integral(x::ComplexFieldElem)

Return the hyperbolic cosine integral evaluated at $x$.
"""
function cosh_integral(x::ComplexFieldElem, prec::Int = precision(Balls))
  z = ComplexFieldElem()
  ccall((:acb_hypgeom_chi, libflint), Nothing, (Ref{ComplexFieldElem}, Ref{ComplexFieldElem}, Int), z, x, prec)
  return z
end

@doc raw"""
    dedekind_eta(x::ComplexFieldElem)

Return the Dedekind eta function $\eta(\tau)$ at $\tau = x$.
"""
function dedekind_eta(x::ComplexFieldElem, prec::Int = precision(Balls))
  z = ComplexFieldElem()
  ccall((:acb_modular_eta, libflint), Nothing, (Ref{ComplexFieldElem}, Ref{ComplexFieldElem}, Int), z, x, prec)
  return z
end

@doc raw"""
    modular_weber_f(x::ComplexFieldElem)

Return the modular Weber function
$\mathfrak{f}(\tau) = \frac{\eta^2(\tau)}{\eta(\tau/2)\eta(2\tau)},$
at $x$ in the complex upper half plane.
"""
function modular_weber_f(x::ComplexFieldElem)
  x_on_2 = divexact(x, 2)
  x_times_2 = 2*x
  return divexact(dedekind_eta(x)^2, dedekind_eta(x_on_2)*dedekind_eta(x_times_2))
end

@doc raw"""
    modular_weber_f1(x::ComplexFieldElem)

Return the modular Weber function
$\mathfrak{f}_1(\tau) = \frac{\eta(\tau/2)}{\eta(\tau)},$
at $x$ in the complex upper half plane.
"""
function modular_weber_f1(x::ComplexFieldElem)
  x_on_2 = divexact(x, 2)
  return divexact(dedekind_eta(x_on_2), dedekind_eta(x))
end

@doc raw"""
    modular_weber_f2(x::ComplexFieldElem)

Return the modular Weber function
$\mathfrak{f}_2(\tau) = \frac{\sqrt{2}\eta(2\tau)}{\eta(\tau)}$
at $x$ in the complex upper half plane.
"""
function modular_weber_f2(x::ComplexFieldElem)
  x_times_2 = x*2
  return divexact(dedekind_eta(x_times_2), dedekind_eta(x))*sqrt(parent(x)(2))
end

@doc raw"""
    j_invariant(x::ComplexFieldElem)

Return the $j$-invariant $j(\tau)$ at $\tau = x$.
"""
function j_invariant(x::ComplexFieldElem, prec::Int = precision(Balls))
  z = ComplexFieldElem()
  ccall((:acb_modular_j, libflint), Nothing, (Ref{ComplexFieldElem}, Ref{ComplexFieldElem}, Int), z, x, prec)
  return z
end

@doc raw"""
    modular_lambda(x::ComplexFieldElem)

Return the modular lambda function $\lambda(\tau)$ at $\tau = x$.
"""
function modular_lambda(x::ComplexFieldElem, prec::Int = precision(Balls))
  z = ComplexFieldElem()
  ccall((:acb_modular_lambda, libflint), Nothing, (Ref{ComplexFieldElem}, Ref{ComplexFieldElem}, Int), z, x, prec)
  return z
end

@doc raw"""
    modular_delta(x::ComplexFieldElem)

Return the modular delta function $\Delta(\tau)$ at $\tau = x$.
"""
function modular_delta(x::ComplexFieldElem, prec::Int = precision(Balls))
  z = ComplexFieldElem()
  ccall((:acb_modular_delta, libflint), Nothing, (Ref{ComplexFieldElem}, Ref{ComplexFieldElem}, Int), z, x, prec)
  return z
end

@doc raw"""
    eisenstein_g(k::Int, x::ComplexFieldElem)

Return the non-normalized Eisenstein series $G_k(\tau)$ of
$\mathrm{SL}_2(\mathbb{Z})$. Also defined for $\tau = i \infty$.
"""
function eisenstein_g(k::Int, x::ComplexFieldElem, prec::Int = precision(Balls))
  CC = parent(x)

  k <= 2 && error("Eisenstein series are not absolute convergent for k = $k")
  imag(x) < 0 && error("x is not in upper half plane.")
  isodd(k) && return zero(CC)
  imag(x) == Inf && return 2 * zeta(CC(k))

  len = div(k, 2) - 1
  vec = acb_vec(len)
  ccall((:acb_modular_eisenstein, libflint), Nothing,
        (Ptr{acb_struct}, Ref{ComplexFieldElem}, Int, Int), vec, x, len, prec)
  z = array(CC, vec, len)
  acb_vec_clear(vec, len)
  return z[end]
end

@doc raw"""
    hilbert_class_polynomial(D::Int, R::ZZPolyRing)

Return in the ring $R$ the Hilbert class polynomial of discriminant $D$,
which is only defined for $D < 0$ and $D \equiv 0, 1 \pmod 4$.
"""
function hilbert_class_polynomial(D::Int, R::ZZPolyRing)
  D < 0 && mod(D, 4) < 2 || throw(ArgumentError("$D is not a negative discriminant"))
  z = R()
  ccall((:acb_modular_hilbert_class_poly, libflint), Nothing,
        (Ref{ZZPolyRingElem}, Int),
        z, D)
  return z
end

@doc raw"""
    elliptic_k(x::ComplexFieldElem)

Return the complete elliptic integral $K(x)$.
"""
function elliptic_k(x::ComplexFieldElem, prec::Int = precision(Balls))
  z = ComplexFieldElem()
  ccall((:acb_modular_elliptic_k, libflint), Nothing, (Ref{ComplexFieldElem}, Ref{ComplexFieldElem}, Int), z, x, prec)
  return z
end

@doc raw"""
    elliptic_e(x::ComplexFieldElem)

Return the complete elliptic integral $E(x)$.
"""
function elliptic_e(x::ComplexFieldElem, prec::Int = precision(Balls))
  z = ComplexFieldElem()
  ccall((:acb_modular_elliptic_e, libflint), Nothing, (Ref{ComplexFieldElem}, Ref{ComplexFieldElem}, Int), z, x, prec)
  return z
end

function sincos(x::ComplexFieldElem, prec::Int = precision(Balls))
  s = ComplexFieldElem()
  c = ComplexFieldElem()
  ccall((:acb_sin_cos, libflint), Nothing,
        (Ref{ComplexFieldElem}, Ref{ComplexFieldElem}, Ref{ComplexFieldElem}, Int), s, c, x, prec)
  return (s, c)
end

function sincospi(x::ComplexFieldElem, prec::Int = precision(Balls))
  s = ComplexFieldElem()
  c = ComplexFieldElem()
  ccall((:acb_sin_cos_pi, libflint), Nothing,
        (Ref{ComplexFieldElem}, Ref{ComplexFieldElem}, Ref{ComplexFieldElem}, Int), s, c, x, prec)
  return (s, c)
end

@doc raw"""
    sinhcosh(x::ComplexFieldElem)

Return a tuple $s, c$ consisting of the hyperbolic sine and cosine of $x$.
"""
function sinhcosh(x::ComplexFieldElem, prec::Int = precision(Balls))
  s = ComplexFieldElem()
  c = ComplexFieldElem()
  ccall((:acb_sinh_cosh, libflint), Nothing,
        (Ref{ComplexFieldElem}, Ref{ComplexFieldElem}, Ref{ComplexFieldElem}, Int), s, c, x, prec)
  return (s, c)
end

@doc raw"""
    zeta(s::ComplexFieldElem, a::ComplexFieldElem)

Return the Hurwitz zeta function $\zeta(s,a)$.
"""
function zeta(s::ComplexFieldElem, a::ComplexFieldElem, prec::Int = precision(Balls))
  z = parent(s)()
  ccall((:acb_hurwitz_zeta, libflint), Nothing,
        (Ref{ComplexFieldElem}, Ref{ComplexFieldElem}, Ref{ComplexFieldElem}, Int), z, s, a, prec)
  return z
end

@doc raw"""
    polygamma(s::ComplexFieldElem, a::ComplexFieldElem)

Return the generalised polygamma function $\psi(s,z)$.
"""
function polygamma(s::ComplexFieldElem, a::ComplexFieldElem, prec::Int = precision(Balls))
  z = parent(s)()
  ccall((:acb_polygamma, libflint), Nothing,
        (Ref{ComplexFieldElem}, Ref{ComplexFieldElem}, Ref{ComplexFieldElem}, Int), z, s, a, prec)
  return z
end

function rising_factorial(x::ComplexFieldElem, n::UInt, prec::Int = precision(Balls))
  z = ComplexFieldElem()
  ccall((:acb_rising_ui, libflint), Nothing,
        (Ref{ComplexFieldElem}, Ref{ComplexFieldElem}, UInt, Int), z, x, n, prec)
  return z
end

@doc raw"""
    rising_factorial(x::ComplexFieldElem, n::Int)

Return the rising factorial $x(x + 1)\ldots (x + n - 1)$ as an Acb.
"""
function rising_factorial(x::ComplexFieldElem, n::Int)
  n < 0 && throw(DomainError(n, "Argument must be non-negative"))
  return rising_factorial(x, UInt(n))
end

function rising_factorial2(x::ComplexFieldElem, n::UInt, prec::Int = precision(Balls))
  z = ComplexFieldElem()
  w = ComplexFieldElem()
  ccall((:acb_rising2_ui, libflint), Nothing,
        (Ref{ComplexFieldElem}, Ref{ComplexFieldElem}, Ref{ComplexFieldElem}, UInt, Int), z, w, x, n, prec)
  return (z, w)
end

@doc raw"""
    rising_factorial2(x::ComplexFieldElem, n::Int)

Return a tuple containing the rising factorial $x(x + 1)\ldots (x + n - 1)$
and its derivative.
"""
function rising_factorial2(x::ComplexFieldElem, n::Int)
  n < 0 && throw(DomainError(n, "Argument must be non-negative"))
  return rising_factorial2(x, UInt(n))
end

function polylog(s::ComplexFieldElem, a::ComplexFieldElem, prec::Int = precision(Balls))
  z = parent(s)()
  ccall((:acb_polylog, libflint), Nothing,
        (Ref{ComplexFieldElem}, Ref{ComplexFieldElem}, Ref{ComplexFieldElem}, Int), z, s, a, prec)
  return z
end

function polylog(s::Int, a::ComplexFieldElem, prec::Int = precision(Balls))
  z = parent(a)()
  ccall((:acb_polylog_si, libflint), Nothing,
        (Ref{ComplexFieldElem}, Int, Ref{ComplexFieldElem}, Int), z, s, a, prec)
  return z
end

@doc raw"""
    polylog(s::Union{ComplexFieldElem,Int}, a::ComplexFieldElem)

Return the polylogarithm Li$_s(a)$.
""" polylog(s::Union{ComplexFieldElem,Int}, ::ComplexFieldElem)

@doc raw"""
    log_integral(x::ComplexFieldElem)

Return the logarithmic integral, evaluated at $x$.
"""
function log_integral(x::ComplexFieldElem, prec::Int = precision(Balls))
  z = ComplexFieldElem()
  ccall((:acb_hypgeom_li, libflint), Nothing,
        (Ref{ComplexFieldElem}, Ref{ComplexFieldElem}, Int, Int), z, x, 0, prec)
  return z
end

@doc raw"""
    log_integral_offset(x::ComplexFieldElem)

Return the offset logarithmic integral, evaluated at $x$.
"""
function log_integral_offset(x::ComplexFieldElem, prec::Int = precision(Balls))
  z = ComplexFieldElem()
  ccall((:acb_hypgeom_li, libflint), Nothing,
        (Ref{ComplexFieldElem}, Ref{ComplexFieldElem}, Int, Int), z, x, 1, prec)
  return z
end

@doc raw"""
    exp_integral_e(s::ComplexFieldElem, x::ComplexFieldElem)

Return the generalised exponential integral $E_s(x)$.
"""
function exp_integral_e(s::ComplexFieldElem, x::ComplexFieldElem, prec::Int = precision(Balls))
  z = parent(s)()
  ccall((:acb_hypgeom_expint, libflint), Nothing,
        (Ref{ComplexFieldElem}, Ref{ComplexFieldElem}, Ref{ComplexFieldElem}, Int), z, s, x, prec)
  return z
end

@doc raw"""
    gamma(s::ComplexFieldElem, x::ComplexFieldElem)

Return the upper incomplete gamma function $\Gamma(s,x)$.
"""
function gamma(s::ComplexFieldElem, x::ComplexFieldElem, prec::Int = precision(Balls))
  z = parent(s)()
  ccall((:acb_hypgeom_gamma_upper, libflint), Nothing,
        (Ref{ComplexFieldElem}, Ref{ComplexFieldElem}, Ref{ComplexFieldElem}, Int, Int), z, s, x, 0, prec)
  return z
end

@doc raw"""
    gamma_regularized(s::ComplexFieldElem, x::ComplexFieldElem)

Return the regularized upper incomplete gamma function
$\Gamma(s,x) / \Gamma(s)$.
"""
function gamma_regularized(s::ComplexFieldElem, x::ComplexFieldElem, prec::Int = precision(Balls))
  z = parent(s)()
  ccall((:acb_hypgeom_gamma_upper, libflint), Nothing,
        (Ref{ComplexFieldElem}, Ref{ComplexFieldElem}, Ref{ComplexFieldElem}, Int, Int), z, s, x, 1, prec)
  return z
end

@doc raw"""
    gamma_lower(s::ComplexFieldElem, x::ComplexFieldElem)

Return the lower incomplete gamma function $\gamma(s,x) / \Gamma(s)$.
"""
function gamma_lower(s::ComplexFieldElem, x::ComplexFieldElem, prec::Int = precision(Balls))
  z = parent(s)()
  ccall((:acb_hypgeom_gamma_lower, libflint), Nothing,
        (Ref{ComplexFieldElem}, Ref{ComplexFieldElem}, Ref{ComplexFieldElem}, Int, Int), z, s, x, 0, prec)
  return z
end

@doc raw"""
    gamma_lower_regularized(s::ComplexFieldElem, x::ComplexFieldElem)

Return the regularized lower incomplete gamma function
$\gamma(s,x) / \Gamma(s)$.
"""
function gamma_lower_regularized(s::ComplexFieldElem, x::ComplexFieldElem, prec::Int = precision(Balls))
  z = parent(s)()
  ccall((:acb_hypgeom_gamma_lower, libflint), Nothing,
        (Ref{ComplexFieldElem}, Ref{ComplexFieldElem}, Ref{ComplexFieldElem}, Int, Int), z, s, x, 1, prec)
  return z
end

@doc raw"""
    bessel_j(nu::ComplexFieldElem, x::ComplexFieldElem)

Return the Bessel function $J_{\nu}(x)$.
"""
function bessel_j(nu::ComplexFieldElem, x::ComplexFieldElem, prec::Int = precision(Balls))
  z = ComplexFieldElem()
  ccall((:acb_hypgeom_bessel_j, libflint), Nothing,
        (Ref{ComplexFieldElem}, Ref{ComplexFieldElem}, Ref{ComplexFieldElem}, Int), z, nu, x, prec)
  return z
end

@doc raw"""
    bessel_y(nu::ComplexFieldElem, x::ComplexFieldElem)

Return the Bessel function $Y_{\nu}(x)$.
"""
function bessel_y(nu::ComplexFieldElem, x::ComplexFieldElem, prec::Int = precision(Balls))
  z = ComplexFieldElem()
  ccall((:acb_hypgeom_bessel_y, libflint), Nothing,
        (Ref{ComplexFieldElem}, Ref{ComplexFieldElem}, Ref{ComplexFieldElem}, Int), z, nu, x, prec)
  return z
end

@doc raw"""
    bessel_i(nu::ComplexFieldElem, x::ComplexFieldElem)

Return the Bessel function $I_{\nu}(x)$.
"""
function bessel_i(nu::ComplexFieldElem, x::ComplexFieldElem, prec::Int = precision(Balls))
  z = ComplexFieldElem()
  ccall((:acb_hypgeom_bessel_i, libflint), Nothing,
        (Ref{ComplexFieldElem}, Ref{ComplexFieldElem}, Ref{ComplexFieldElem}, Int), z, nu, x, prec)
  return z
end

@doc raw"""
    bessel_k(nu::ComplexFieldElem, x::ComplexFieldElem)

Return the Bessel function $K_{\nu}(x)$.
"""
function bessel_k(nu::ComplexFieldElem, x::ComplexFieldElem, prec::Int = precision(Balls))
  z = ComplexFieldElem()
  ccall((:acb_hypgeom_bessel_k, libflint), Nothing,
        (Ref{ComplexFieldElem}, Ref{ComplexFieldElem}, Ref{ComplexFieldElem}, Int), z, nu, x, prec)
  return z
end

@doc raw"""
    airy_ai(x::ComplexFieldElem)

Return the Airy function $\operatorname{Ai}(x)$.
"""
function airy_ai(x::ComplexFieldElem, prec::Int = precision(Balls))
  ai = ComplexFieldElem()
  ccall((:acb_hypgeom_airy, libflint), Nothing,
        (Ref{ComplexFieldElem}, Ptr{Cvoid}, Ptr{Cvoid}, Ptr{Cvoid}, Ref{ComplexFieldElem}, Int),
        ai, C_NULL, C_NULL, C_NULL, x, prec)
  return ai
end

@doc raw"""
    airy_bi(x::ComplexFieldElem)

Return the Airy function $\operatorname{Bi}(x)$.
"""
function airy_bi(x::ComplexFieldElem, prec::Int = precision(Balls))
  bi = ComplexFieldElem()
  ccall((:acb_hypgeom_airy, libflint), Nothing,
        (Ptr{Cvoid}, Ptr{Cvoid}, Ref{ComplexFieldElem}, Ptr{Cvoid}, Ref{ComplexFieldElem}, Int),
        C_NULL, C_NULL, bi, C_NULL, x, prec)
  return bi
end

@doc raw"""
    airy_ai_prime(x::ComplexFieldElem)

Return the derivative of the Airy function $\operatorname{Ai}^\prime(x)$.
"""
function airy_ai_prime(x::ComplexFieldElem, prec::Int = precision(Balls))
  ai_prime = ComplexFieldElem()
  ccall((:acb_hypgeom_airy, libflint), Nothing,
        (Ptr{Cvoid}, Ref{ComplexFieldElem}, Ptr{Cvoid}, Ptr{Cvoid}, Ref{ComplexFieldElem}, Int),
        C_NULL, ai_prime, C_NULL, C_NULL, x, prec)
  return ai_prime
end

@doc raw"""
    airy_bi_prime(x::ComplexFieldElem)

Return the derivative of the Airy function $\operatorname{Bi}^\prime(x)$.
"""
function airy_bi_prime(x::ComplexFieldElem, prec::Int = precision(Balls))
  bi_prime = ComplexFieldElem()
  ccall((:acb_hypgeom_airy, libflint), Nothing,
        (Ptr{Cvoid}, Ptr{Cvoid}, Ptr{Cvoid}, Ref{ComplexFieldElem}, Ref{ComplexFieldElem}, Int),
        C_NULL, C_NULL, C_NULL, bi_prime, x, prec)
  return bi_prime
end

@doc raw"""
    hypergeometric_1f1(a::ComplexFieldElem, b::ComplexFieldElem, x::ComplexFieldElem)

Return the confluent hypergeometric function ${}_1F_1(a,b,x)$.
"""
function hypergeometric_1f1(a::ComplexFieldElem, b::ComplexFieldElem, x::ComplexFieldElem, prec::Int = precision(Balls))
  z = ComplexFieldElem()
  ccall((:acb_hypgeom_m, libflint), Nothing,
        (Ref{ComplexFieldElem}, Ref{ComplexFieldElem}, Ref{ComplexFieldElem}, Ref{ComplexFieldElem}, Int, Int), z, a, b, x, 0, prec)
  return z
end

@doc raw"""
    hypergeometric_1f1_regularized(a::ComplexFieldElem, b::ComplexFieldElem, x::ComplexFieldElem)

Return the regularized confluent hypergeometric function
${}_1F_1(a,b,x) / \Gamma(b)$.
"""
function hypergeometric_1f1_regularized(a::ComplexFieldElem, b::ComplexFieldElem, x::ComplexFieldElem, prec::Int = precision(Balls))
  z = ComplexFieldElem()
  ccall((:acb_hypgeom_m, libflint), Nothing,
        (Ref{ComplexFieldElem}, Ref{ComplexFieldElem}, Ref{ComplexFieldElem}, Ref{ComplexFieldElem}, Int, Int), z, a, b, x, 1, prec)
  return z
end

@doc raw"""
    hypergeometric_u(a::ComplexFieldElem, b::ComplexFieldElem, x::ComplexFieldElem)

Return the confluent hypergeometric function $U(a,b,x)$.
"""
function hypergeometric_u(a::ComplexFieldElem, b::ComplexFieldElem, x::ComplexFieldElem, prec::Int = precision(Balls))
  z = ComplexFieldElem()
  ccall((:acb_hypgeom_u, libflint), Nothing,
        (Ref{ComplexFieldElem}, Ref{ComplexFieldElem}, Ref{ComplexFieldElem}, Ref{ComplexFieldElem}, Int), z, a, b, x, prec)
  return z
end

@doc raw"""
    hypergeometric_2f1(a::ComplexFieldElem, b::ComplexFieldElem, c::ComplexFieldElem, x::ComplexFieldElem; flags=0)

Return the Gauss hypergeometric function ${}_2F_1(a,b,c,x)$.
"""
function hypergeometric_2f1(a::ComplexFieldElem, b::ComplexFieldElem, c::ComplexFieldElem, x::ComplexFieldElem, prec::Int = precision(Balls); flags=0)
  z = ComplexFieldElem()
  ccall((:acb_hypgeom_2f1, libflint), Nothing,
        (Ref{ComplexFieldElem}, Ref{ComplexFieldElem}, Ref{ComplexFieldElem}, Ref{ComplexFieldElem}, Ref{ComplexFieldElem}, Int, Int), z, a, b, c, x, flags, prec)
  return z
end

@doc raw"""
    jacobi_theta(z::ComplexFieldElem, tau::ComplexFieldElem)

Return a tuple of four elements containing the Jacobi theta function values
$\theta_1, \theta_2, \theta_3, \theta_4$ evaluated at $z, \tau$.
"""
function jacobi_theta(z::ComplexFieldElem, tau::ComplexFieldElem, prec::Int = precision(Balls))
  t1 = ComplexFieldElem()
  t2 = ComplexFieldElem()
  t3 = ComplexFieldElem()
  t4 = ComplexFieldElem()
  ccall((:acb_modular_theta, libflint), Nothing,
        (Ref{ComplexFieldElem}, Ref{ComplexFieldElem}, Ref{ComplexFieldElem}, Ref{ComplexFieldElem}, Ref{ComplexFieldElem}, Ref{ComplexFieldElem}, Int),
        t1, t2, t3, t4, z, tau, prec)
  return (t1, t2, t3, t4)
end

@doc raw"""
    weierstrass_p(z::ComplexFieldElem, tau::ComplexFieldElem)

Return the Weierstrass elliptic function $\wp(z,\tau)$.
"""
function weierstrass_p(z::ComplexFieldElem, tau::ComplexFieldElem, prec::Int = precision(Balls))
  r = parent(z)()
  ccall((:acb_elliptic_p, libflint), Nothing,
        (Ref{ComplexFieldElem}, Ref{ComplexFieldElem}, Ref{ComplexFieldElem}, Int), r, z, tau, prec)
  return r
end

@doc raw"""
    weierstrass_p_prime(z::ComplexFieldElem, tau::ComplexFieldElem)

Return the derivative of the Weierstrass elliptic function $\frac{\partial}{\partial z}\wp(z,\tau)$.
"""
function weierstrass_p_prime(z::ComplexFieldElem, tau::ComplexFieldElem, prec::Int = precision(Balls))
  r = parent(z)()
  ccall((:acb_elliptic_p_prime, libflint), Nothing,
        (Ref{ComplexFieldElem}, Ref{ComplexFieldElem}, Ref{ComplexFieldElem}, Int), r, z, tau, prec)
  return r
end

@doc raw"""
    agm(x::ComplexFieldElem, y::ComplexFieldElem)

Return the arithmetic-geometric mean of $x$ and $y$.
"""
function agm(x::ComplexFieldElem, y::ComplexFieldElem)
  v = inv(y)
  if isfinite(v)
    return agm(x * v) * y
  else
    v = inv(x)
    return agm(y * v) * x
  end
end

@doc raw"""
    lindep(A::Vector{ComplexFieldElem}, bits::Int)

Find a small linear combination of the entries of the array $A$ that is small
(using LLL). The entries are first scaled by the given number of bits before
truncating the real and imaginary parts to integers for use in LLL. This function can
be used to find linear dependence between a list of complex numbers. The algorithm is
heuristic only and returns an array of Nemo integers representing the linear
combination.
"""
function lindep(A::Vector{ComplexFieldElem}, bits::Int)
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

@doc raw"""
    lindep(A::Matrix{ComplexFieldElem}, bits::Int)

Find a (common) small linear combination of the entries in each row of the array $A$,
that is small (using LLL). It is assumed that the complex numbers in each row of the
array share the same linear combination. The entries are first scaled by the given
number of bits before truncating the real and imaginary parts to integers for use in
LLL. This function can be used to find a common linear dependence shared across a
number of lists of complex numbers. The algorithm is heuristic only and returns an
array of Nemo integers representing the common linear combination.
"""
function lindep(A::Matrix{ComplexFieldElem}, bits::Int)
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

function zero!(z::ComplexFieldElem)
  ccall((:acb_zero, libflint), Nothing, (Ref{ComplexFieldElem},), z)
  return z
end

function add!(z::ComplexFieldElem, x::ComplexFieldElem, y::ComplexFieldElem, prec::Int = precision(Balls))
  ccall((:acb_add, libflint), Nothing, (Ref{ComplexFieldElem}, Ref{ComplexFieldElem}, Ref{ComplexFieldElem}, Int),
        z, x, y, prec)
  return z
end

function sub!(z::ComplexFieldElem, x::ComplexFieldElem, y::ComplexFieldElem, prec::Int = precision(Balls))
  ccall((:acb_sub, libflint), Nothing, (Ref{ComplexFieldElem}, Ref{ComplexFieldElem}, Ref{ComplexFieldElem}, Int),
        z, x, y, prec)
  return z
end

function mul!(z::ComplexFieldElem, x::ComplexFieldElem, y::ComplexFieldElem, prec::Int = precision(Balls))
  ccall((:acb_mul, libflint), Nothing, (Ref{ComplexFieldElem}, Ref{ComplexFieldElem}, Ref{ComplexFieldElem}, Int),
        z, x, y, prec)
  return z
end

function div!(z::ComplexFieldElem, x::ComplexFieldElem, y::ComplexFieldElem, prec::Int = precision(Balls))
  ccall((:acb_div, libflint), Nothing, (Ref{ComplexFieldElem}, Ref{ComplexFieldElem}, Ref{ComplexFieldElem}, Int),
        z, x, y, prec)
  return z
end

################################################################################
#
#  Unsafe setting
#
################################################################################

for (typeofx, passtoc) in ((ComplexFieldElem, Ref{ComplexFieldElem}), (Ptr{ComplexFieldElem}, Ptr{ComplexFieldElem}))
  for (f,t) in (("acb_set_si", Int), ("acb_set_ui", UInt),
                ("acb_set_d", Float64))
    @eval begin
      function _acb_set(x::($typeofx), y::($t))
        ccall(($f, libflint), Nothing, (($passtoc), ($t)), x, y)
      end

      function _acb_set(x::($typeofx), y::($t), p::Int)
        _acb_set(x, y)
        ccall((:acb_set_round, libflint), Nothing,
              (($passtoc), ($passtoc), Int), x, x, p)
      end
    end
  end

  @eval begin
    function _acb_set(x::($typeofx), y::ZZRingElem)
      ccall((:acb_set_fmpz, libflint), Nothing, (($passtoc), Ref{ZZRingElem}), x, y)
    end

    function _acb_set(x::($typeofx), y::ZZRingElem, p::Int)
      ccall((:acb_set_round_fmpz, libflint), Nothing,
            (($passtoc), Ref{ZZRingElem}, Int), x, y, p)
    end

    function _acb_set(x::($typeofx), y::QQFieldElem, p::Int)
      ccall((:acb_set_fmpq, libflint), Nothing,
            (($passtoc), Ref{QQFieldElem}, Int), x, y, p)
    end

    function _acb_set(x::($typeofx), y::RealFieldElem)
      ccall((:acb_set_arb, libflint), Nothing, (($passtoc), Ref{RealFieldElem}), x, y)
    end

    function _acb_set(x::($typeofx), y::RealFieldElem, p::Int)
      _acb_set(x, y)
      ccall((:acb_set_round, libflint), Nothing,
            (($passtoc), ($passtoc), Int), x, x, p)
    end

    function _acb_set(x::($typeofx), y::ComplexFieldElem)
      ccall((:acb_set, libflint), Nothing, (($passtoc), Ref{ComplexFieldElem}), x, y)
    end

    function _acb_set(x::($typeofx), y::ComplexFieldElem, p::Int)
      ccall((:acb_set_round, libflint), Nothing,
            (($passtoc), Ref{ComplexFieldElem}, Int), x, y, p)
    end

    function _acb_set(x::($typeofx), y::AbstractString, p::Int)
      r = ccall((:acb_real_ptr, libflint), Ptr{RealFieldElem}, (($passtoc), ), x)
      _arb_set(r, y, p)
      i = ccall((:acb_imag_ptr, libflint), Ptr{RealFieldElem}, (($passtoc), ), x)
      ccall((:arb_zero, libflint), Nothing, (Ptr{RealFieldElem}, ), i)
    end

    function _acb_set(x::($typeofx), y::BigFloat)
      r = ccall((:acb_real_ptr, libflint), Ptr{RealFieldElem}, (($passtoc), ), x)
      _arb_set(r, y)
      i = ccall((:acb_imag_ptr, libflint), Ptr{RealFieldElem}, (($passtoc), ), x)
      ccall((:arb_zero, libflint), Nothing, (Ptr{RealFieldElem}, ), i)
    end

    function _acb_set(x::($typeofx), y::BigFloat, p::Int)
      r = ccall((:acb_real_ptr, libflint), Ptr{RealFieldElem}, (($passtoc), ), x)
      _arb_set(r, y, p)
      i = ccall((:acb_imag_ptr, libflint), Ptr{RealFieldElem}, (($passtoc), ), x)
      ccall((:arb_zero, libflint), Nothing, (Ptr{RealFieldElem}, ), i)
    end

    function _acb_set(x::($typeofx), y::Int, z::Int, p::Int)
      ccall((:acb_set_si_si, libflint), Nothing,
            (($passtoc), Int, Int), x, y, z)
      ccall((:acb_set_round, libflint), Nothing,
            (($passtoc), ($passtoc), Int), x, x, p)
    end

    function _acb_set(x::($typeofx), y::RealFieldElem, z::RealFieldElem)
      ccall((:acb_set_arb_arb, libflint), Nothing,
            (($passtoc), Ref{RealFieldElem}, Ref{RealFieldElem}), x, y, z)
    end

    function _acb_set(x::($typeofx), y::RealFieldElem, z::RealFieldElem, p::Int)
      _acb_set(x, y, z)
      ccall((:acb_set_round, libflint), Nothing,
            (($passtoc), ($passtoc), Int), x, x, p)
    end

    function _acb_set(x::($typeofx), y::QQFieldElem, z::QQFieldElem, p::Int)
      r = ccall((:acb_real_ptr, libflint), Ptr{RealFieldElem}, (($passtoc), ), x)
      _arb_set(r, y, p)
      i = ccall((:acb_imag_ptr, libflint), Ptr{RealFieldElem}, (($passtoc), ), x)
      _arb_set(i, z, p)
    end

    function _acb_set(x::($typeofx), y::T, z::T, p::Int) where {T <: AbstractString}
      r = ccall((:acb_real_ptr, libflint), Ptr{RealFieldElem}, (($passtoc), ), x)
      _arb_set(r, y, p)
      i = ccall((:acb_imag_ptr, libflint), Ptr{RealFieldElem}, (($passtoc), ), x)
      _arb_set(i, z, p)
    end

    function _acb_set(x::($typeofx), y::Real, p::Int)
      r = ccall((:acb_real_ptr, libflint), Ptr{RealFieldElem}, (($passtoc), ), x)
      _arb_set(r, y, p)
      i = ccall((:acb_imag_ptr, libflint), Ptr{RealFieldElem}, (($passtoc), ), x)
      ccall((:arb_zero, libflint), Nothing, (Ptr{ArbFieldElem}, ), i)
    end

    function _acb_set(x::($typeofx), y::Complex, p::Int)
      r = ccall((:acb_real_ptr, libflint), Ptr{RealFieldElem}, (($passtoc), ), x)
      _arb_set(r, real(y), p)
      i = ccall((:acb_imag_ptr, libflint), Ptr{RealFieldElem}, (($passtoc), ), x)
      _arb_set(i, imag(y), p)
    end

  end

  for T in (Real, ZZRingElem)
    @eval begin
      function _acb_set(x::($typeofx), y::($T), z::($T), p::Int)
        r = ccall((:acb_real_ptr, libflint), Ptr{RealFieldElem}, (($passtoc), ), x)
        _arb_set(r, y, p)
        i = ccall((:acb_imag_ptr, libflint), Ptr{RealFieldElem}, (($passtoc), ), x)
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

promote_rule(::Type{ComplexFieldElem}, ::Type{T}) where {T <: Number} = ComplexFieldElem

promote_rule(::Type{ComplexFieldElem}, ::Type{ZZRingElem}) = ComplexFieldElem

promote_rule(::Type{ComplexFieldElem}, ::Type{QQFieldElem}) = ComplexFieldElem

promote_rule(::Type{ComplexFieldElem}, ::Type{RealFieldElem}) = ComplexFieldElem

################################################################################
#
#  Parent object overload
#
################################################################################

(r::ComplexField)() = ComplexFieldElem()

(r::ComplexField)(x::Any; precision::Int = precision(Balls)) = ComplexFieldElem(x, precision)

(r::ComplexField)(x::T, y::T; precision::Int = precision(Balls)) where T = ComplexFieldElem(x, y, precision)

for S in (Real, ZZRingElem, QQFieldElem, RealFieldElem, AbstractString)
  for T in (Real, ZZRingElem, QQFieldElem, RealFieldElem, AbstractString)
    if S != T || S == Real
      @eval begin
        function (r::ComplexField)(x::$(S), y::$(T); precision::Int = precision(Balls))
          z = ComplexFieldElem(RealField()(x), RealField()(y), precision)
          return z
        end
      end
    end
  end
end

################################################################################
#
#  ComplexField constructor
#
################################################################################

# see internal constructor
