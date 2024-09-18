###############################################################################
#
#   acb.jl : Arb complex numbers
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

elem_type(::Type{AcbField}) = AcbFieldElem

parent_type(::Type{AcbFieldElem}) = AcbField

base_ring_type(::Type{AcbField}) = typeof(Union{})

base_ring(R::AcbField) = Union{}

parent(x::AcbFieldElem) = x.parent

is_domain_type(::Type{AcbFieldElem}) = true

is_exact_type(::Type{AcbFieldElem}) = false

function zero(r::AcbField)
  z = AcbFieldElem()
  z.parent = r
  return z
end

function one(r::AcbField)
  z = AcbFieldElem()
  ccall((:acb_one, libflint), Nothing, (Ref{AcbFieldElem}, ), z)
  z.parent = r
  return z
end

@doc raw"""
    onei(r::AcbField)

Return exact one times $i$ in the given Arb complex field.
"""
function onei(r::AcbField)
  z = AcbFieldElem()
  ccall((:acb_onei, libflint), Nothing, (Ref{AcbFieldElem}, ), z)
  z.parent = r
  return z
end

@doc raw"""
    accuracy_bits(x::AcbFieldElem)

Return the relative accuracy of $x$ measured in bits, capped between
`typemax(Int)` and `-typemax(Int)`.
"""
function accuracy_bits(x::AcbFieldElem)
  # bug in acb.h: rel_accuracy_bits is not in the library
  return -ccall((:acb_rel_error_bits, libflint), Int, (Ref{AcbFieldElem},), x)
end

function deepcopy_internal(a::AcbFieldElem, dict::IdDict)
  b = parent(a)()
  ccall((:acb_set, libflint), Nothing, (Ref{AcbFieldElem}, Ref{AcbFieldElem}), b, a)
  return b
end

function canonical_unit(x::AcbFieldElem)
  return x
end

# TODO: implement hash

characteristic(::AcbField) = 0

################################################################################
#
#  Conversions
#
################################################################################

function convert(::Type{ComplexF64}, x::AcbFieldElem)
  GC.@preserve x begin
    re = ccall((:acb_real_ptr, libflint), Ptr{arb_struct}, (Ref{AcbFieldElem}, ), x)
    im = ccall((:acb_imag_ptr, libflint), Ptr{arb_struct}, (Ref{AcbFieldElem}, ), x)
    t = ccall((:arb_mid_ptr, libflint), Ptr{arf_struct}, (Ptr{ArbFieldElem}, ), re)
    u = ccall((:arb_mid_ptr, libflint), Ptr{arf_struct}, (Ptr{ArbFieldElem}, ), im)
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

function real(x::AcbFieldElem)
  z = ArbFieldElem()
  ccall((:acb_get_real, libflint), Nothing, (Ref{ArbFieldElem}, Ref{AcbFieldElem}), z, x)
  z.parent = ArbField(parent(x).prec)
  return z
end

function imag(x::AcbFieldElem)
  z = ArbFieldElem()
  ccall((:acb_get_imag, libflint), Nothing, (Ref{ArbFieldElem}, Ref{AcbFieldElem}), z, x)
  z.parent = ArbField(parent(x).prec)
  return z
end

################################################################################
#
#  String I/O
#
################################################################################

function expressify(z::AcbFieldElem; context = nothing)
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

function Base.show(io::IO, ::MIME"text/plain", z::AcbFieldElem)
  print(io, AbstractAlgebra.obj_to_string(z, context = io))
end

function Base.show(io::IO, z::AcbFieldElem)
  print(io, AbstractAlgebra.obj_to_string(z, context = io))
end

function show(io::IO, x::AcbField)
  @show_name(io, x)
  @show_special(io, x)
  print(io, "Complex Field with ")
  print(io, precision(x))
  print(io, " bits of precision and error bounds")
end

################################################################################
#
#  Unary operations
#
################################################################################

function -(x::AcbFieldElem)
  z = parent(x)()
  ccall((:acb_neg, libflint), Nothing, (Ref{AcbFieldElem}, Ref{AcbFieldElem}), z, x)
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
    function ($s)(x::AcbFieldElem, y::AcbFieldElem)
      z = parent(x)()
      ccall(($f, libflint), Nothing, (Ref{AcbFieldElem}, Ref{AcbFieldElem}, Ref{AcbFieldElem}, Int),
            z, x, y, parent(x).prec)
      return z
    end
  end
end

for (f,s) in ((:+, "add"), (:-, "sub"), (:*, "mul"), (://, "div"), (:^, "pow"))
  @eval begin

    function ($f)(x::AcbFieldElem, y::UInt)
      z = parent(x)()
      ccall(($("acb_"*s*"_ui"), libflint), Nothing,
            (Ref{AcbFieldElem}, Ref{AcbFieldElem}, UInt, Int),
            z, x, y, parent(x).prec)
      return z
    end

    function ($f)(x::AcbFieldElem, y::Int)
      z = parent(x)()
      ccall(($("acb_"*s*"_si"), libflint), Nothing,
            (Ref{AcbFieldElem}, Ref{AcbFieldElem}, Int, Int), z, x, y, parent(x).prec)
      return z
    end

    function ($f)(x::AcbFieldElem, y::ZZRingElem)
      z = parent(x)()
      ccall(($("acb_"*s*"_fmpz"), libflint), Nothing,
            (Ref{AcbFieldElem}, Ref{AcbFieldElem}, Ref{ZZRingElem}, Int),
            z, x, y, parent(x).prec)
      return z
    end

    function ($f)(x::AcbFieldElem, y::ArbFieldElem)
      z = parent(x)()
      ccall(($("acb_"*s*"_arb"), libflint), Nothing,
            (Ref{AcbFieldElem}, Ref{AcbFieldElem}, Ref{ArbFieldElem}, Int),
            z, x, y, parent(x).prec)
      return z
    end
  end
end


+(x::UInt,y::AcbFieldElem) = +(y,x)
+(x::Int,y::AcbFieldElem) = +(y,x)
+(x::ZZRingElem,y::AcbFieldElem) = +(y,x)
+(x::ArbFieldElem,y::AcbFieldElem) = +(y,x)

*(x::UInt,y::AcbFieldElem) = *(y,x)
*(x::Int,y::AcbFieldElem) = *(y,x)
*(x::ZZRingElem,y::AcbFieldElem) = *(y,x)
*(x::ArbFieldElem,y::AcbFieldElem) = *(y,x)

//(x::UInt,y::AcbFieldElem) = (x == 1) ? inv(y) : parent(y)(x) // y
//(x::Int,y::AcbFieldElem) = (x == 1) ? inv(y) : parent(y)(x) // y
//(x::ZZRingElem,y::AcbFieldElem) = isone(x) ? inv(y) : parent(y)(x) // y
//(x::ArbFieldElem,y::AcbFieldElem) = isone(x) ? inv(y) : parent(y)(x) // y

^(x::ZZRingElem,y::AcbFieldElem) = parent(y)(x) ^ y
^(x::ArbFieldElem,y::AcbFieldElem) = parent(y)(x) ^ y

function -(x::UInt, y::AcbFieldElem)
  z = parent(y)()
  ccall((:acb_sub_ui, libflint), Nothing, (Ref{AcbFieldElem}, Ref{AcbFieldElem}, UInt, Int), z, y, x, parent(y).prec)
  ccall((:acb_neg, libflint), Nothing, (Ref{AcbFieldElem}, Ref{AcbFieldElem}), z, z)
  return z
end

function -(x::Int, y::AcbFieldElem)
  z = parent(y)()
  ccall((:acb_sub_si, libflint), Nothing, (Ref{AcbFieldElem}, Ref{AcbFieldElem}, Int, Int), z, y, x, parent(y).prec)
  ccall((:acb_neg, libflint), Nothing, (Ref{AcbFieldElem}, Ref{AcbFieldElem}), z, z)
  return z
end

function -(x::ZZRingElem, y::AcbFieldElem)
  z = parent(y)()
  ccall((:acb_sub_fmpz, libflint), Nothing, (Ref{AcbFieldElem}, Ref{AcbFieldElem}, Ref{ZZRingElem}, Int), z, y, x, parent(y).prec)
  ccall((:acb_neg, libflint), Nothing, (Ref{AcbFieldElem}, Ref{AcbFieldElem}), z, z)
  return z
end

function -(x::ArbFieldElem, y::AcbFieldElem)
  z = parent(y)()
  ccall((:acb_sub_arb, libflint), Nothing, (Ref{AcbFieldElem}, Ref{AcbFieldElem}, Ref{ArbFieldElem}, Int), z, y, x, parent(y).prec)
  ccall((:acb_neg, libflint), Nothing, (Ref{AcbFieldElem}, Ref{AcbFieldElem}), z, z)
  return z
end

+(x::AcbFieldElem, y::Integer) = x + ZZRingElem(y)

-(x::AcbFieldElem, y::Integer) = x - ZZRingElem(y)

*(x::AcbFieldElem, y::Integer) = x*ZZRingElem(y)

//(x::AcbFieldElem, y::Integer) = x//ZZRingElem(y)

+(x::Integer, y::AcbFieldElem) = ZZRingElem(x) + y

-(x::Integer, y::AcbFieldElem) = ZZRingElem(x) - y

*(x::Integer, y::AcbFieldElem) = ZZRingElem(x)*y

//(x::Integer, y::AcbFieldElem) = ZZRingElem(x)//y

divexact(x::AcbFieldElem, y::AcbFieldElem; check::Bool=true) = x // y
divexact(x::ZZRingElem, y::AcbFieldElem; check::Bool=true) = x // y
divexact(x::AcbFieldElem, y::ZZRingElem; check::Bool=true) = x // y
divexact(x::ArbFieldElem, y::AcbFieldElem; check::Bool=true) = x // y
divexact(x::AcbFieldElem, y::ArbFieldElem; check::Bool=true) = x // y

/(x::AcbFieldElem, y::AcbFieldElem) = x // y
/(x::ZZRingElem, y::AcbFieldElem) = x // y
/(x::AcbFieldElem, y::ZZRingElem) = x // y
/(x::ArbFieldElem, y::AcbFieldElem) = x // y
/(x::AcbFieldElem, y::ArbFieldElem) = x // y

for T in (Float64, BigFloat, Rational, QQFieldElem)
  @eval begin
    +(x::$T, y::AcbFieldElem) = parent(y)(x) + y
    +(x::AcbFieldElem, y::$T) = x + parent(x)(y)
    -(x::$T, y::AcbFieldElem) = parent(y)(x) - y
    -(x::AcbFieldElem, y::$T) = x - parent(x)(y)
    *(x::$T, y::AcbFieldElem) = parent(y)(x) * y
    *(x::AcbFieldElem, y::$T) = x * parent(x)(y)
    //(x::$T, y::AcbFieldElem) = parent(y)(x) // y
    //(x::AcbFieldElem, y::$T) = x // parent(x)(y)
  end
end

for T in (Float64, BigFloat, Integer, Rational, QQFieldElem)
  @eval begin
    ^(x::$T, y::AcbFieldElem) = parent(y)(x)^y
    ^(x::AcbFieldElem, y::$T) = x ^ parent(x)(y)
    /(x::$T, y::AcbFieldElem) = x // y
    /(x::AcbFieldElem, y::$T) = x // y
    divexact(x::$T, y::AcbFieldElem; check::Bool=true) = x // y
    divexact(x::AcbFieldElem, y::$T; check::Bool=true) = x // y
  end
end

################################################################################
#
#  Comparison
#
################################################################################

@doc raw"""
    isequal(x::AcbFieldElem, y::AcbFieldElem)

Return `true` if the boxes $x$ and $y$ are precisely equal, i.e. their real
and imaginary parts have the same midpoints and radii.
"""
function isequal(x::AcbFieldElem, y::AcbFieldElem)
  r = ccall((:acb_equal, libflint), Cint, (Ref{AcbFieldElem}, Ref{AcbFieldElem}), x, y)
  return Bool(r)
end

function ==(x::AcbFieldElem, y::AcbFieldElem)
  r = ccall((:acb_eq, libflint), Cint, (Ref{AcbFieldElem}, Ref{AcbFieldElem}), x, y)
  return Bool(r)
end

function !=(x::AcbFieldElem, y::AcbFieldElem)
  r = ccall((:acb_ne, libflint), Cint, (Ref{AcbFieldElem}, Ref{AcbFieldElem}), x, y)
  return Bool(r)
end

==(x::AcbFieldElem,y::Int) = (x == parent(x)(y))
==(x::Int,y::AcbFieldElem) = (y == parent(y)(x))

==(x::AcbFieldElem,y::ArbFieldElem) = (x == parent(x)(y))
==(x::ArbFieldElem,y::AcbFieldElem) = (y == parent(y)(x))

==(x::AcbFieldElem,y::ZZRingElem) = (x == parent(x)(y))
==(x::ZZRingElem,y::AcbFieldElem) = (y == parent(y)(x))

==(x::AcbFieldElem,y::Integer) = x == ZZRingElem(y)
==(x::Integer,y::AcbFieldElem) = ZZRingElem(x) == y

==(x::AcbFieldElem,y::Float64) = (x == parent(x)(y))
==(x::Float64,y::AcbFieldElem) = (y == parent(y)(x))

!=(x::AcbFieldElem,y::Int) = (x != parent(x)(y))
!=(x::Int,y::AcbFieldElem) = (y != parent(y)(x))

!=(x::AcbFieldElem,y::ArbFieldElem) = (x != parent(x)(y))
!=(x::ArbFieldElem,y::AcbFieldElem) = (y != parent(y)(x))

!=(x::AcbFieldElem,y::ZZRingElem) = (x != parent(x)(y))
!=(x::ZZRingElem,y::AcbFieldElem) = (y != parent(y)(x))

!=(x::AcbFieldElem,y::Float64) = (x != parent(x)(y))
!=(x::Float64,y::AcbFieldElem) = (y != parent(y)(x))

################################################################################
#
#  Containment
#
################################################################################

@doc raw"""
    overlaps(x::AcbFieldElem, y::AcbFieldElem)

Returns `true` if any part of the box $x$ overlaps any part of the box $y$,
otherwise return `false`.
"""
function overlaps(x::AcbFieldElem, y::AcbFieldElem)
  r = ccall((:acb_overlaps, libflint), Cint, (Ref{AcbFieldElem}, Ref{AcbFieldElem}), x, y)
  return Bool(r)
end

@doc raw"""
    contains(x::AcbFieldElem, y::AcbFieldElem)

Returns `true` if the box $x$ contains the box $y$, otherwise return
`false`.
"""
function contains(x::AcbFieldElem, y::AcbFieldElem)
  r = ccall((:acb_contains, libflint), Cint, (Ref{AcbFieldElem}, Ref{AcbFieldElem}), x, y)
  return Bool(r)
end

@doc raw"""
    contains(x::AcbFieldElem, y::QQFieldElem)

Returns `true` if the box $x$ contains the given rational value, otherwise
return `false`.
"""
function contains(x::AcbFieldElem, y::QQFieldElem)
  r = ccall((:acb_contains_fmpq, libflint), Cint, (Ref{AcbFieldElem}, Ref{QQFieldElem}), x, y)
  return Bool(r)
end

@doc raw"""
    contains(x::AcbFieldElem, y::ZZRingElem)

Returns `true` if the box $x$ contains the given integer value, otherwise
return `false`.
"""
function contains(x::AcbFieldElem, y::ZZRingElem)
  r = ccall((:acb_contains_fmpz, libflint), Cint, (Ref{AcbFieldElem}, Ref{ZZRingElem}), x, y)
  return Bool(r)
end

function contains(x::AcbFieldElem, y::Int)
  v = ZZRingElem(y)
  r = ccall((:acb_contains_fmpz, libflint), Cint, (Ref{AcbFieldElem}, Ref{ZZRingElem}), x, v)
  return Bool(r)
end

@doc raw"""
    contains(x::AcbFieldElem, y::Integer)

Returns `true` if the box $x$ contains the given integer value, otherwise
return `false`.
"""
contains(x::AcbFieldElem, y::Integer) = contains(x, ZZRingElem(y))

@doc raw"""
    contains(x::AcbFieldElem, y::Rational{T}) where {T <: Integer}

Returns `true` if the box $x$ contains the given rational value, otherwise
return `false`.
"""
contains(x::AcbFieldElem, y::Rational{T}) where {T <: Integer} = contains(x, ZZRingElem(y))

@doc raw"""
    contains_zero(x::AcbFieldElem)

Returns `true` if the box $x$ contains zero, otherwise return `false`.
"""
function contains_zero(x::AcbFieldElem)
  return Bool(ccall((:acb_contains_zero, libflint), Cint, (Ref{AcbFieldElem},), x))
end

################################################################################
#
#  Predicates
#
################################################################################

function is_unit(x::AcbFieldElem)
  !iszero(x)
end

@doc raw"""
    iszero(x::AcbFieldElem)

Return `true` if $x$ is certainly zero, otherwise return `false`.
"""
function iszero(x::AcbFieldElem)
  return Bool(ccall((:acb_is_zero, libflint), Cint, (Ref{AcbFieldElem},), x))
end

@doc raw"""
    isone(x::AcbFieldElem)

Return `true` if $x$ is certainly one, otherwise return `false`.
"""
function isone(x::AcbFieldElem)
  return Bool(ccall((:acb_is_one, libflint), Cint, (Ref{AcbFieldElem},), x))
end

@doc raw"""
    isfinite(x::AcbFieldElem)

Return `true` if $x$ is finite, i.e. its real and imaginary parts have finite
midpoint and radius, otherwise return `false`.
"""
function isfinite(x::AcbFieldElem)
  return Bool(ccall((:acb_is_finite, libflint), Cint, (Ref{AcbFieldElem},), x))
end

@doc raw"""
    is_exact(x::AcbFieldElem)

Return `true` if $x$ is exact, i.e. has its real and imaginary parts have
zero radius, otherwise return `false`.
"""
function is_exact(x::AcbFieldElem)
  return Bool(ccall((:acb_is_exact, libflint), Cint, (Ref{AcbFieldElem},), x))
end

@doc raw"""
    isinteger(x::AcbFieldElem)

Return `true` if $x$ is an exact integer, otherwise return `false`.
"""
function isinteger(x::AcbFieldElem)
  return Bool(ccall((:acb_is_int, libflint), Cint, (Ref{AcbFieldElem},), x))
end

function isreal(x::AcbFieldElem)
  return Bool(ccall((:acb_is_real, libflint), Cint, (Ref{AcbFieldElem},), x))
end

is_negative(x::AcbFieldElem) = isreal(x) && is_negative(real(x))

################################################################################
#
#  Absolute value
#
################################################################################

function abs(x::AcbFieldElem)
  z = ArbFieldElem()
  ccall((:acb_abs, libflint), Nothing,
        (Ref{ArbFieldElem}, Ref{AcbFieldElem}, Int), z, x, parent(x).prec)
  z.parent = ArbField(parent(x).prec)
  return z
end

################################################################################
#
#  Inversion
#
################################################################################

function inv(x::AcbFieldElem)
  z = parent(x)()
  ccall((:acb_inv, libflint), Nothing, (Ref{AcbFieldElem}, Ref{AcbFieldElem}, Int), z, x, parent(x).prec)
  return z
end

################################################################################
#
#  Sign
#
################################################################################

function sign(::Type{Int}, x::AcbFieldElem)
  if isreal(x)
    return sign(Int, real(x))
  else
    error("Element is not real")
  end
end

################################################################################
#
#  Shifting
#
################################################################################

function ldexp(x::AcbFieldElem, y::Int)
  z = parent(x)()
  ccall((:acb_mul_2exp_si, libflint), Nothing,
        (Ref{AcbFieldElem}, Ref{AcbFieldElem}, Int), z, x, y)
  return z
end

function ldexp(x::AcbFieldElem, y::ZZRingElem)
  z = parent(x)()
  ccall((:acb_mul_2exp_fmpz, libflint), Nothing,
        (Ref{AcbFieldElem}, Ref{AcbFieldElem}, Ref{ZZRingElem}), z, x, y)
  return z
end

################################################################################
#
#  Miscellaneous
#
################################################################################

@doc raw"""
    trim(x::AcbFieldElem)

Return an `AcbFieldElem` box containing $x$ but which may be more economical,
by rounding off insignificant bits from midpoints.
"""
function trim(x::AcbFieldElem)
  z = parent(x)()
  ccall((:acb_trim, libflint), Nothing, (Ref{AcbFieldElem}, Ref{AcbFieldElem}), z, x)
  return z
end

@doc raw"""
    unique_integer(x::AcbFieldElem)

Return a pair where the first value is a boolean and the second is an `ZZRingElem`
integer. The boolean indicates whether the box $x$ contains a unique
integer. If this is the case, the second return value is set to this unique
integer.
"""
function unique_integer(x::AcbFieldElem)
  z = ZZRingElem()
  unique = ccall((:acb_get_unique_fmpz, libflint), Int,
                 (Ref{ZZRingElem}, Ref{AcbFieldElem}), z, x)
  return (unique != 0, z)
end

function conj(x::AcbFieldElem)
  z = parent(x)()
  ccall((:acb_conj, libflint), Nothing, (Ref{AcbFieldElem}, Ref{AcbFieldElem}), z, x)
  return z
end

function angle(x::AcbFieldElem)
  z = ArbFieldElem()
  ccall((:acb_arg, libflint), Nothing,
        (Ref{ArbFieldElem}, Ref{AcbFieldElem}, Int), z, x, parent(x).prec)
  z.parent = ArbField(parent(x).prec)
  return z
end

################################################################################
#
#  Constants
#
################################################################################

@doc raw"""
    const_pi(r::AcbField)

Return $\pi = 3.14159\ldots$ as an element of $r$.
"""
function const_pi(r::AcbField)
  z = r()
  ccall((:acb_const_pi, libflint), Nothing, (Ref{AcbFieldElem}, Int), z, precision(r))
  return z
end

################################################################################
#
#  Complex valued functions
#
################################################################################

# complex - complex functions

function Base.sqrt(x::AcbFieldElem; check::Bool=true)
  z = parent(x)()
  ccall((:acb_sqrt, libflint), Nothing, (Ref{AcbFieldElem}, Ref{AcbFieldElem}, Int), z, x, parent(x).prec)
  return z
end

@doc raw"""
    rsqrt(x::AcbFieldElem)

Return the reciprocal of the square root of $x$, i.e. $1/\sqrt{x}$.
"""
function rsqrt(x::AcbFieldElem)
  z = parent(x)()
  ccall((:acb_rsqrt, libflint), Nothing, (Ref{AcbFieldElem}, Ref{AcbFieldElem}, Int), z, x, parent(x).prec)
  return z
end

function log(x::AcbFieldElem)
  z = parent(x)()
  ccall((:acb_log, libflint), Nothing, (Ref{AcbFieldElem}, Ref{AcbFieldElem}, Int), z, x, parent(x).prec)
  return z
end

function log1p(x::AcbFieldElem)
  z = parent(x)()
  ccall((:acb_log1p, libflint), Nothing, (Ref{AcbFieldElem}, Ref{AcbFieldElem}, Int), z, x, parent(x).prec)
  return z
end

function Base.exp(x::AcbFieldElem)
  z = parent(x)()
  ccall((:acb_exp, libflint), Nothing, (Ref{AcbFieldElem}, Ref{AcbFieldElem}, Int), z, x, parent(x).prec)
  return z
end

function Base.expm1(x::AcbFieldElem)
  z = parent(x)()
  ccall((:acb_expm1, libflint), Nothing, (Ref{AcbFieldElem}, Ref{AcbFieldElem}, Int), z, x, parent(x).prec)
  return z
end

@doc raw"""
    cispi(x::AcbFieldElem)

Return the exponential of $\pi i x$.
"""
function cispi(x::AcbFieldElem)
  z = parent(x)()
  ccall((:acb_exp_pi_i, libflint), Nothing, (Ref{AcbFieldElem}, Ref{AcbFieldElem}, Int), z, x, parent(x).prec)
  return z
end

@doc raw"""
    root_of_unity(C::AcbField, k::Int)

Return $\exp(2\pi i/k)$.
"""
function root_of_unity(C::AcbField, k::Int)
  k <= 0 && throw(ArgumentError("Order must be positive ($k)"))
  z = C()
  ccall((:acb_unit_root, libflint), Nothing, (Ref{AcbFieldElem}, UInt, Int), z, k, C.prec)
  return z
end

function sin(x::AcbFieldElem)
  z = parent(x)()
  ccall((:acb_sin, libflint), Nothing, (Ref{AcbFieldElem}, Ref{AcbFieldElem}, Int), z, x, parent(x).prec)
  return z
end

function cos(x::AcbFieldElem)
  z = parent(x)()
  ccall((:acb_cos, libflint), Nothing, (Ref{AcbFieldElem}, Ref{AcbFieldElem}, Int), z, x, parent(x).prec)
  return z
end

function tan(x::AcbFieldElem)
  z = parent(x)()
  ccall((:acb_tan, libflint), Nothing, (Ref{AcbFieldElem}, Ref{AcbFieldElem}, Int), z, x, parent(x).prec)
  return z
end

function cot(x::AcbFieldElem)
  z = parent(x)()
  ccall((:acb_cot, libflint), Nothing, (Ref{AcbFieldElem}, Ref{AcbFieldElem}, Int), z, x, parent(x).prec)
  return z
end

function sinpi(x::AcbFieldElem)
  z = parent(x)()
  ccall((:acb_sin_pi, libflint), Nothing, (Ref{AcbFieldElem}, Ref{AcbFieldElem}, Int), z, x, parent(x).prec)
  return z
end

function cospi(x::AcbFieldElem)
  z = parent(x)()
  ccall((:acb_cos_pi, libflint), Nothing, (Ref{AcbFieldElem}, Ref{AcbFieldElem}, Int), z, x, parent(x).prec)
  return z
end

function tanpi(x::AcbFieldElem)
  z = parent(x)()
  ccall((:acb_tan_pi, libflint), Nothing, (Ref{AcbFieldElem}, Ref{AcbFieldElem}, Int), z, x, parent(x).prec)
  return z
end

function cotpi(x::AcbFieldElem)
  z = parent(x)()
  ccall((:acb_cot_pi, libflint), Nothing, (Ref{AcbFieldElem}, Ref{AcbFieldElem}, Int), z, x, parent(x).prec)
  return z
end

function sinh(x::AcbFieldElem)
  z = parent(x)()
  ccall((:acb_sinh, libflint), Nothing, (Ref{AcbFieldElem}, Ref{AcbFieldElem}, Int), z, x, parent(x).prec)
  return z
end

function cosh(x::AcbFieldElem)
  z = parent(x)()
  ccall((:acb_cosh, libflint), Nothing, (Ref{AcbFieldElem}, Ref{AcbFieldElem}, Int), z, x, parent(x).prec)
  return z
end

function tanh(x::AcbFieldElem)
  z = parent(x)()
  ccall((:acb_tanh, libflint), Nothing, (Ref{AcbFieldElem}, Ref{AcbFieldElem}, Int), z, x, parent(x).prec)
  return z
end

function coth(x::AcbFieldElem)
  z = parent(x)()
  ccall((:acb_coth, libflint), Nothing, (Ref{AcbFieldElem}, Ref{AcbFieldElem}, Int), z, x, parent(x).prec)
  return z
end

function atan(x::AcbFieldElem)
  z = parent(x)()
  ccall((:acb_atan, libflint), Nothing, (Ref{AcbFieldElem}, Ref{AcbFieldElem}, Int), z, x, parent(x).prec)
  return z
end

@doc raw"""
    log_sinpi(x::AcbFieldElem)

Return $\log\sin(\pi x)$, constructed without branch cuts off the real line.
"""
function log_sinpi(x::AcbFieldElem)
  z = parent(x)()
  ccall((:acb_log_sin_pi, libflint), Nothing, (Ref{AcbFieldElem}, Ref{AcbFieldElem}, Int), z, x, parent(x).prec)
  return z
end

@doc raw"""
    gamma(x::AcbFieldElem)

Return the Gamma function evaluated at $x$.
"""
function gamma(x::AcbFieldElem)
  z = parent(x)()
  ccall((:acb_gamma, libflint), Nothing, (Ref{AcbFieldElem}, Ref{AcbFieldElem}, Int), z, x, parent(x).prec)
  return z
end

@doc raw"""
    rgamma(x::AcbFieldElem)

Return the reciprocal of the Gamma function evaluated at $x$.
"""
function rgamma(x::AcbFieldElem)
  z = parent(x)()
  ccall((:acb_rgamma, libflint), Nothing, (Ref{AcbFieldElem}, Ref{AcbFieldElem}, Int), z, x, parent(x).prec)
  return z
end

@doc raw"""
    lgamma(x::AcbFieldElem)

Return the logarithm of the Gamma function evaluated at $x$.
"""
function lgamma(x::AcbFieldElem)
  z = parent(x)()
  ccall((:acb_lgamma, libflint), Nothing, (Ref{AcbFieldElem}, Ref{AcbFieldElem}, Int), z, x, parent(x).prec)
  return z
end

@doc raw"""
    digamma(x::AcbFieldElem)

Return the  logarithmic derivative of the gamma function evaluated at $x$,
i.e. $\psi(x)$.
"""
function digamma(x::AcbFieldElem)
  z = parent(x)()
  ccall((:acb_digamma, libflint), Nothing, (Ref{AcbFieldElem}, Ref{AcbFieldElem}, Int), z, x, parent(x).prec)
  return z
end

@doc raw"""
    zeta(x::AcbFieldElem)

Return the Riemann zeta function evaluated at $x$.
"""
function zeta(x::AcbFieldElem)
  z = parent(x)()
  ccall((:acb_zeta, libflint), Nothing, (Ref{AcbFieldElem}, Ref{AcbFieldElem}, Int), z, x, parent(x).prec)
  return z
end

@doc raw"""
    barnes_g(x::AcbFieldElem)

Return the Barnes $G$-function, evaluated at $x$.
"""
function barnes_g(x::AcbFieldElem)
  z = parent(x)()
  ccall((:acb_barnes_g, libflint), Nothing, (Ref{AcbFieldElem}, Ref{AcbFieldElem}, Int), z, x, parent(x).prec)
  return z
end

@doc raw"""
    log_barnes_g(x::AcbFieldElem)

Return the logarithm of the Barnes $G$-function, evaluated at $x$.
"""
function log_barnes_g(x::AcbFieldElem)
  z = parent(x)()
  ccall((:acb_log_barnes_g, libflint), Nothing, (Ref{AcbFieldElem}, Ref{AcbFieldElem}, Int), z, x, parent(x).prec)
  return z
end

@doc raw"""
    agm(x::AcbFieldElem)

Return the arithmetic-geometric mean of $1$ and $x$.
"""
function agm(x::AcbFieldElem)
  z = parent(x)()
  ccall((:acb_agm1, libflint), Nothing, (Ref{AcbFieldElem}, Ref{AcbFieldElem}, Int), z, x, parent(x).prec)
  return z
end

@doc raw"""
    erf(x::AcbFieldElem)

Return the error function evaluated at $x$.
"""
function erf(x::AcbFieldElem)
  z = parent(x)()
  ccall((:acb_hypgeom_erf, libflint), Nothing, (Ref{AcbFieldElem}, Ref{AcbFieldElem}, Int), z, x, parent(x).prec)
  return z
end

@doc raw"""
    erfi(x::AcbFieldElem)

Return the imaginary error function evaluated at $x$.
"""
function erfi(x::AcbFieldElem)
  z = parent(x)()
  ccall((:acb_hypgeom_erfi, libflint), Nothing, (Ref{AcbFieldElem}, Ref{AcbFieldElem}, Int), z, x, parent(x).prec)
  return z
end

@doc raw"""
    erfc(x::AcbFieldElem)

Return the complementary error function evaluated at $x$.
"""
function erfc(x::AcbFieldElem)
  z = parent(x)()
  ccall((:acb_hypgeom_erfc, libflint), Nothing, (Ref{AcbFieldElem}, Ref{AcbFieldElem}, Int), z, x, parent(x).prec)
  return z
end

@doc raw"""
    exp_integral_ei(x::AcbFieldElem)

Return the exponential integral evaluated at $x$.
"""
function exp_integral_ei(x::AcbFieldElem)
  z = parent(x)()
  ccall((:acb_hypgeom_ei, libflint), Nothing, (Ref{AcbFieldElem}, Ref{AcbFieldElem}, Int), z, x, parent(x).prec)
  return z
end

@doc raw"""
    sin_integral(x::AcbFieldElem)

Return the sine integral evaluated at $x$.
"""
function sin_integral(x::AcbFieldElem)
  z = parent(x)()
  ccall((:acb_hypgeom_si, libflint), Nothing, (Ref{AcbFieldElem}, Ref{AcbFieldElem}, Int), z, x, parent(x).prec)
  return z
end

@doc raw"""
    cos_integral(x::AcbFieldElem)

Return the exponential cosine integral evaluated at $x$.
"""
function cos_integral(x::AcbFieldElem)
  z = parent(x)()
  ccall((:acb_hypgeom_ci, libflint), Nothing, (Ref{AcbFieldElem}, Ref{AcbFieldElem}, Int), z, x, parent(x).prec)
  return z
end

@doc raw"""
    sinh_integral(x::AcbFieldElem)

Return the hyperbolic sine integral evaluated at $x$.
"""
function sinh_integral(x::AcbFieldElem)
  z = parent(x)()
  ccall((:acb_hypgeom_shi, libflint), Nothing, (Ref{AcbFieldElem}, Ref{AcbFieldElem}, Int), z, x, parent(x).prec)
  return z
end

@doc raw"""
    cosh_integral(x::AcbFieldElem)

Return the hyperbolic cosine integral evaluated at $x$.
"""
function cosh_integral(x::AcbFieldElem)
  z = parent(x)()
  ccall((:acb_hypgeom_chi, libflint), Nothing, (Ref{AcbFieldElem}, Ref{AcbFieldElem}, Int), z, x, parent(x).prec)
  return z
end

@doc raw"""
    dedekind_eta(x::AcbFieldElem)

Return the Dedekind eta function $\eta(\tau)$ at $\tau = x$.
"""
function dedekind_eta(x::AcbFieldElem)
  z = parent(x)()
  ccall((:acb_modular_eta, libflint), Nothing, (Ref{AcbFieldElem}, Ref{AcbFieldElem}, Int), z, x, parent(x).prec)
  return z
end

@doc raw"""
    modular_weber_f(x::AcbFieldElem)

Return the modular Weber function
$\mathfrak{f}(\tau) = \frac{\eta^2(\tau)}{\eta(\tau/2)\eta(2\tau)},$
at $x$ in the complex upper half plane.
"""
function modular_weber_f(x::AcbFieldElem)
  x_on_2 = divexact(x, 2)
  x_times_2 = 2*x
  return divexact(dedekind_eta(x)^2, dedekind_eta(x_on_2)*dedekind_eta(x_times_2))
end

@doc raw"""
    modular_weber_f1(x::AcbFieldElem)

Return the modular Weber function
$\mathfrak{f}_1(\tau) = \frac{\eta(\tau/2)}{\eta(\tau)},$
at $x$ in the complex upper half plane.
"""
function modular_weber_f1(x::AcbFieldElem)
  x_on_2 = divexact(x, 2)
  return divexact(dedekind_eta(x_on_2), dedekind_eta(x))
end

@doc raw"""
    modular_weber_f2(x::AcbFieldElem)

Return the modular Weber function
$\mathfrak{f}_2(\tau) = \frac{\sqrt{2}\eta(2\tau)}{\eta(\tau)}$
at $x$ in the complex upper half plane.
"""
function modular_weber_f2(x::AcbFieldElem)
  x_times_2 = x*2
  return divexact(dedekind_eta(x_times_2), dedekind_eta(x))*sqrt(parent(x)(2))
end

@doc raw"""
    j_invariant(x::AcbFieldElem)

Return the $j$-invariant $j(\tau)$ at $\tau = x$.
"""
function j_invariant(x::AcbFieldElem)
  z = parent(x)()
  ccall((:acb_modular_j, libflint), Nothing, (Ref{AcbFieldElem}, Ref{AcbFieldElem}, Int), z, x, parent(x).prec)
  return z
end

@doc raw"""
    modular_lambda(x::AcbFieldElem)

Return the modular lambda function $\lambda(\tau)$ at $\tau = x$.
"""
function modular_lambda(x::AcbFieldElem)
  z = parent(x)()
  ccall((:acb_modular_lambda, libflint), Nothing, (Ref{AcbFieldElem}, Ref{AcbFieldElem}, Int), z, x, parent(x).prec)
  return z
end

@doc raw"""
    modular_delta(x::AcbFieldElem)

Return the modular delta function $\Delta(\tau)$ at $\tau = x$.
"""
function modular_delta(x::AcbFieldElem)
  z = parent(x)()
  ccall((:acb_modular_delta, libflint), Nothing, (Ref{AcbFieldElem}, Ref{AcbFieldElem}, Int), z, x, parent(x).prec)
  return z
end

@doc raw"""
    eisenstein_g(k::Int, x::AcbFieldElem)

Return the non-normalized Eisenstein series $G_k(\tau)$ of
$\mathrm{SL}_2(\mathbb{Z})$. Also defined for $\tau = i \infty$.
"""
function eisenstein_g(k::Int, x::AcbFieldElem)
  CC = parent(x)

  k <= 2 && error("Eisenstein series are not absolute convergent for k = $k")
  imag(x) < 0 && error("x is not in upper half plane.")
  isodd(k) && return zero(CC)
  imag(x) == Inf && return 2 * zeta(CC(k))

  len = div(k, 2) - 1
  vec = acb_vec(len)
  ccall((:acb_modular_eisenstein, libflint), Nothing,
        (Ptr{acb_struct}, Ref{AcbFieldElem}, Int, Int), vec, x, len, CC.prec)
  z = array(CC, vec, len)
  acb_vec_clear(vec, len)
  return z[end]
end

@doc raw"""
    elliptic_k(x::AcbFieldElem)

Return the complete elliptic integral $K(x)$.
"""
function elliptic_k(x::AcbFieldElem)
  z = parent(x)()
  ccall((:acb_modular_elliptic_k, libflint), Nothing, (Ref{AcbFieldElem}, Ref{AcbFieldElem}, Int), z, x, parent(x).prec)
  return z
end

@doc raw"""
    elliptic_e(x::AcbFieldElem)

Return the complete elliptic integral $E(x)$.
"""
function elliptic_e(x::AcbFieldElem)
  z = parent(x)()
  ccall((:acb_modular_elliptic_e, libflint), Nothing, (Ref{AcbFieldElem}, Ref{AcbFieldElem}, Int), z, x, parent(x).prec)
  return z
end

function sincos(x::AcbFieldElem)
  s = parent(x)()
  c = parent(x)()
  ccall((:acb_sin_cos, libflint), Nothing,
        (Ref{AcbFieldElem}, Ref{AcbFieldElem}, Ref{AcbFieldElem}, Int), s, c, x, parent(x).prec)
  return (s, c)
end

function sincospi(x::AcbFieldElem)
  s = parent(x)()
  c = parent(x)()
  ccall((:acb_sin_cos_pi, libflint), Nothing,
        (Ref{AcbFieldElem}, Ref{AcbFieldElem}, Ref{AcbFieldElem}, Int), s, c, x, parent(x).prec)
  return (s, c)
end

@doc raw"""
    sinhcosh(x::AcbFieldElem)

Return a tuple $s, c$ consisting of the hyperbolic sine and cosine of $x$.
"""
function sinhcosh(x::AcbFieldElem)
  s = parent(x)()
  c = parent(x)()
  ccall((:acb_sinh_cosh, libflint), Nothing,
        (Ref{AcbFieldElem}, Ref{AcbFieldElem}, Ref{AcbFieldElem}, Int), s, c, x, parent(x).prec)
  return (s, c)
end

@doc raw"""
    zeta(s::AcbFieldElem, a::AcbFieldElem)

Return the Hurwitz zeta function $\zeta(s,a)$.
"""
function zeta(s::AcbFieldElem, a::AcbFieldElem)
  z = parent(s)()
  ccall((:acb_hurwitz_zeta, libflint), Nothing,
        (Ref{AcbFieldElem}, Ref{AcbFieldElem}, Ref{AcbFieldElem}, Int), z, s, a, parent(s).prec)
  return z
end

@doc raw"""
    polygamma(s::AcbFieldElem, a::AcbFieldElem)

Return the generalised polygamma function $\psi(s,z)$.
"""
function polygamma(s::AcbFieldElem, a::AcbFieldElem)
  z = parent(s)()
  ccall((:acb_polygamma, libflint), Nothing,
        (Ref{AcbFieldElem}, Ref{AcbFieldElem}, Ref{AcbFieldElem}, Int), z, s, a, parent(s).prec)
  return z
end

function rising_factorial(x::AcbFieldElem, n::UInt)
  z = parent(x)()
  ccall((:acb_rising_ui, libflint), Nothing,
        (Ref{AcbFieldElem}, Ref{AcbFieldElem}, UInt, Int), z, x, n, parent(x).prec)
  return z
end

@doc raw"""
    rising_factorial(x::AcbFieldElem, n::Int)

Return the rising factorial $x(x + 1)\ldots (x + n - 1)$ as an Acb.
"""
function rising_factorial(x::AcbFieldElem, n::Int)
  n < 0 && throw(DomainError(n, "Argument must be non-negative"))
  return rising_factorial(x, UInt(n))
end

function rising_factorial2(x::AcbFieldElem, n::UInt)
  z = parent(x)()
  w = parent(x)()
  ccall((:acb_rising2_ui, libflint), Nothing,
        (Ref{AcbFieldElem}, Ref{AcbFieldElem}, Ref{AcbFieldElem}, UInt, Int), z, w, x, n, parent(x).prec)
  return (z, w)
end

@doc raw"""
    rising_factorial2(x::AcbFieldElem, n::Int)

Return a tuple containing the rising factorial $x(x + 1)\ldots (x + n - 1)$
and its derivative.
"""
function rising_factorial2(x::AcbFieldElem, n::Int)
  n < 0 && throw(DomainError(n, "Argument must be non-negative"))
  return rising_factorial2(x, UInt(n))
end

function polylog(s::AcbFieldElem, a::AcbFieldElem)
  z = parent(s)()
  ccall((:acb_polylog, libflint), Nothing,
        (Ref{AcbFieldElem}, Ref{AcbFieldElem}, Ref{AcbFieldElem}, Int), z, s, a, parent(s).prec)
  return z
end

function polylog(s::Int, a::AcbFieldElem)
  z = parent(a)()
  ccall((:acb_polylog_si, libflint), Nothing,
        (Ref{AcbFieldElem}, Int, Ref{AcbFieldElem}, Int), z, s, a, parent(a).prec)
  return z
end

@doc raw"""
    polylog(s::Union{AcbFieldElem,Int}, a::AcbFieldElem)

Return the polylogarithm Li$_s(a)$.
""" polylog(s::Union{AcbFieldElem,Int}, ::AcbFieldElem)

@doc raw"""
    log_integral(x::AcbFieldElem)

Return the logarithmic integral, evaluated at $x$.
"""
function log_integral(x::AcbFieldElem)
  z = parent(x)()
  ccall((:acb_hypgeom_li, libflint), Nothing,
        (Ref{AcbFieldElem}, Ref{AcbFieldElem}, Int, Int), z, x, 0, parent(x).prec)
  return z
end

@doc raw"""
    log_integral_offset(x::AcbFieldElem)

Return the offset logarithmic integral, evaluated at $x$.
"""
function log_integral_offset(x::AcbFieldElem)
  z = parent(x)()
  ccall((:acb_hypgeom_li, libflint), Nothing,
        (Ref{AcbFieldElem}, Ref{AcbFieldElem}, Int, Int), z, x, 1, parent(x).prec)
  return z
end

@doc raw"""
    exp_integral_e(s::AcbFieldElem, x::AcbFieldElem)

Return the generalised exponential integral $E_s(x)$.
"""
function exp_integral_e(s::AcbFieldElem, x::AcbFieldElem)
  z = parent(s)()
  ccall((:acb_hypgeom_expint, libflint), Nothing,
        (Ref{AcbFieldElem}, Ref{AcbFieldElem}, Ref{AcbFieldElem}, Int), z, s, x, parent(s).prec)
  return z
end

@doc raw"""
    gamma(s::AcbFieldElem, x::AcbFieldElem)

Return the upper incomplete gamma function $\Gamma(s,x)$.
"""
function gamma(s::AcbFieldElem, x::AcbFieldElem)
  z = parent(s)()
  ccall((:acb_hypgeom_gamma_upper, libflint), Nothing,
        (Ref{AcbFieldElem}, Ref{AcbFieldElem}, Ref{AcbFieldElem}, Int, Int), z, s, x, 0, parent(s).prec)
  return z
end

@doc raw"""
    gamma_regularized(s::AcbFieldElem, x::AcbFieldElem)

Return the regularized upper incomplete gamma function
$\Gamma(s,x) / \Gamma(s)$.
"""
function gamma_regularized(s::AcbFieldElem, x::AcbFieldElem)
  z = parent(s)()
  ccall((:acb_hypgeom_gamma_upper, libflint), Nothing,
        (Ref{AcbFieldElem}, Ref{AcbFieldElem}, Ref{AcbFieldElem}, Int, Int), z, s, x, 1, parent(s).prec)
  return z
end

@doc raw"""
    gamma_lower(s::AcbFieldElem, x::AcbFieldElem)

Return the lower incomplete gamma function $\gamma(s,x) / \Gamma(s)$.
"""
function gamma_lower(s::AcbFieldElem, x::AcbFieldElem)
  z = parent(s)()
  ccall((:acb_hypgeom_gamma_lower, libflint), Nothing,
        (Ref{AcbFieldElem}, Ref{AcbFieldElem}, Ref{AcbFieldElem}, Int, Int), z, s, x, 0, parent(s).prec)
  return z
end

@doc raw"""
    gamma_lower_regularized(s::AcbFieldElem, x::AcbFieldElem)

Return the regularized lower incomplete gamma function
$\gamma(s,x) / \Gamma(s)$.
"""
function gamma_lower_regularized(s::AcbFieldElem, x::AcbFieldElem)
  z = parent(s)()
  ccall((:acb_hypgeom_gamma_lower, libflint), Nothing,
        (Ref{AcbFieldElem}, Ref{AcbFieldElem}, Ref{AcbFieldElem}, Int, Int), z, s, x, 1, parent(s).prec)
  return z
end

@doc raw"""
    bessel_j(nu::AcbFieldElem, x::AcbFieldElem)

Return the Bessel function $J_{\nu}(x)$.
"""
function bessel_j(nu::AcbFieldElem, x::AcbFieldElem)
  z = parent(x)()
  ccall((:acb_hypgeom_bessel_j, libflint), Nothing,
        (Ref{AcbFieldElem}, Ref{AcbFieldElem}, Ref{AcbFieldElem}, Int), z, nu, x, parent(x).prec)
  return z
end

@doc raw"""
    bessel_y(nu::AcbFieldElem, x::AcbFieldElem)

Return the Bessel function $Y_{\nu}(x)$.
"""
function bessel_y(nu::AcbFieldElem, x::AcbFieldElem)
  z = parent(x)()
  ccall((:acb_hypgeom_bessel_y, libflint), Nothing,
        (Ref{AcbFieldElem}, Ref{AcbFieldElem}, Ref{AcbFieldElem}, Int), z, nu, x, parent(x).prec)
  return z
end

@doc raw"""
    bessel_i(nu::AcbFieldElem, x::AcbFieldElem)

Return the Bessel function $I_{\nu}(x)$.
"""
function bessel_i(nu::AcbFieldElem, x::AcbFieldElem)
  z = parent(x)()
  ccall((:acb_hypgeom_bessel_i, libflint), Nothing,
        (Ref{AcbFieldElem}, Ref{AcbFieldElem}, Ref{AcbFieldElem}, Int), z, nu, x, parent(x).prec)
  return z
end

@doc raw"""
    bessel_k(nu::AcbFieldElem, x::AcbFieldElem)

Return the Bessel function $K_{\nu}(x)$.
"""
function bessel_k(nu::AcbFieldElem, x::AcbFieldElem)
  z = parent(x)()
  ccall((:acb_hypgeom_bessel_k, libflint), Nothing,
        (Ref{AcbFieldElem}, Ref{AcbFieldElem}, Ref{AcbFieldElem}, Int), z, nu, x, parent(x).prec)
  return z
end

@doc raw"""
    airy_ai(x::AcbFieldElem)

Return the Airy function $\operatorname{Ai}(x)$.
"""
function airy_ai(x::AcbFieldElem)
  ai = parent(x)()
  ccall((:acb_hypgeom_airy, libflint), Nothing,
        (Ref{AcbFieldElem}, Ptr{Cvoid}, Ptr{Cvoid}, Ptr{Cvoid}, Ref{AcbFieldElem}, Int),
        ai, C_NULL, C_NULL, C_NULL, x, parent(x).prec)
  return ai
end

@doc raw"""
    airy_bi(x::AcbFieldElem)

Return the Airy function $\operatorname{Bi}(x)$.
"""
function airy_bi(x::AcbFieldElem)
  bi = parent(x)()
  ccall((:acb_hypgeom_airy, libflint), Nothing,
        (Ptr{Cvoid}, Ptr{Cvoid}, Ref{AcbFieldElem}, Ptr{Cvoid}, Ref{AcbFieldElem}, Int),
        C_NULL, C_NULL, bi, C_NULL, x, parent(x).prec)
  return bi
end

@doc raw"""
    airy_ai_prime(x::AcbFieldElem)

Return the derivative of the Airy function $\operatorname{Ai}^\prime(x)$.
"""
function airy_ai_prime(x::AcbFieldElem)
  ai_prime = parent(x)()
  ccall((:acb_hypgeom_airy, libflint), Nothing,
        (Ptr{Cvoid}, Ref{AcbFieldElem}, Ptr{Cvoid}, Ptr{Cvoid}, Ref{AcbFieldElem}, Int),
        C_NULL, ai_prime, C_NULL, C_NULL, x, parent(x).prec)
  return ai_prime
end

@doc raw"""
    airy_bi_prime(x::AcbFieldElem)

Return the derivative of the Airy function $\operatorname{Bi}^\prime(x)$.
"""
function airy_bi_prime(x::AcbFieldElem)
  bi_prime = parent(x)()
  ccall((:acb_hypgeom_airy, libflint), Nothing,
        (Ptr{Cvoid}, Ptr{Cvoid}, Ptr{Cvoid}, Ref{AcbFieldElem}, Ref{AcbFieldElem}, Int),
        C_NULL, C_NULL, C_NULL, bi_prime, x, parent(x).prec)
  return bi_prime
end

@doc raw"""
    hypergeometric_1f1(a::AcbFieldElem, b::AcbFieldElem, x::AcbFieldElem)

Return the confluent hypergeometric function ${}_1F_1(a,b,x)$.
"""
function hypergeometric_1f1(a::AcbFieldElem, b::AcbFieldElem, x::AcbFieldElem)
  z = parent(x)()
  ccall((:acb_hypgeom_m, libflint), Nothing,
        (Ref{AcbFieldElem}, Ref{AcbFieldElem}, Ref{AcbFieldElem}, Ref{AcbFieldElem}, Int, Int), z, a, b, x, 0, parent(x).prec)
  return z
end

@doc raw"""
    hypergeometric_1f1_regularized(a::AcbFieldElem, b::AcbFieldElem, x::AcbFieldElem)

Return the regularized confluent hypergeometric function
${}_1F_1(a,b,x) / \Gamma(b)$.
"""
function hypergeometric_1f1_regularized(a::AcbFieldElem, b::AcbFieldElem, x::AcbFieldElem)
  z = parent(x)()
  ccall((:acb_hypgeom_m, libflint), Nothing,
        (Ref{AcbFieldElem}, Ref{AcbFieldElem}, Ref{AcbFieldElem}, Ref{AcbFieldElem}, Int, Int), z, a, b, x, 1, parent(x).prec)
  return z
end

@doc raw"""
    hypergeometric_u(a::AcbFieldElem, b::AcbFieldElem, x::AcbFieldElem)

Return the confluent hypergeometric function $U(a,b,x)$.
"""
function hypergeometric_u(a::AcbFieldElem, b::AcbFieldElem, x::AcbFieldElem)
  z = parent(x)()
  ccall((:acb_hypgeom_u, libflint), Nothing,
        (Ref{AcbFieldElem}, Ref{AcbFieldElem}, Ref{AcbFieldElem}, Ref{AcbFieldElem}, Int), z, a, b, x, parent(x).prec)
  return z
end

@doc raw"""
    hypergeometric_2f1(a::AcbFieldElem, b::AcbFieldElem, c::AcbFieldElem, x::AcbFieldElem; flags=0)

Return the Gauss hypergeometric function ${}_2F_1(a,b,c,x)$.
"""
function hypergeometric_2f1(a::AcbFieldElem, b::AcbFieldElem, c::AcbFieldElem, x::AcbFieldElem; flags=0)
  z = parent(x)()
  ccall((:acb_hypgeom_2f1, libflint), Nothing,
        (Ref{AcbFieldElem}, Ref{AcbFieldElem}, Ref{AcbFieldElem}, Ref{AcbFieldElem}, Ref{AcbFieldElem}, Int, Int), z, a, b, c, x, flags, parent(x).prec)
  return z
end

@doc raw"""
    jacobi_theta(z::AcbFieldElem, tau::AcbFieldElem)

Return a tuple of four elements containing the Jacobi theta function values
$\theta_1, \theta_2, \theta_3, \theta_4$ evaluated at $z, \tau$.
"""
function jacobi_theta(z::AcbFieldElem, tau::AcbFieldElem)
  t1 = parent(z)()
  t2 = parent(z)()
  t3 = parent(z)()
  t4 = parent(z)()
  ccall((:acb_modular_theta, libflint), Nothing,
        (Ref{AcbFieldElem}, Ref{AcbFieldElem}, Ref{AcbFieldElem}, Ref{AcbFieldElem}, Ref{AcbFieldElem}, Ref{AcbFieldElem}, Int),
        t1, t2, t3, t4, z, tau, parent(z).prec)
  return (t1, t2, t3, t4)
end

@doc raw"""
    weierstrass_p(z::AcbFieldElem, tau::AcbFieldElem)

Return the Weierstrass elliptic function $\wp(z,\tau)$.
"""
function weierstrass_p(z::AcbFieldElem, tau::AcbFieldElem)
  r = parent(z)()
  ccall((:acb_elliptic_p, libflint), Nothing,
        (Ref{AcbFieldElem}, Ref{AcbFieldElem}, Ref{AcbFieldElem}, Int), r, z, tau, parent(z).prec)
  return r
end

@doc raw"""
    weierstrass_p_prime(z::AcbFieldElem, tau::AcbFieldElem)

Return the derivative of the Weierstrass elliptic function $\frac{\partial}{\partial z}\wp(z,\tau)$.
"""
function weierstrass_p_prime(z::AcbFieldElem, tau::AcbFieldElem)
  r = parent(z)()
  ccall((:acb_elliptic_p_prime, libflint), Nothing,
        (Ref{AcbFieldElem}, Ref{AcbFieldElem}, Ref{AcbFieldElem}, Int), r, z, tau, parent(z).prec)
  return r
end

@doc raw"""
    agm(x::AcbFieldElem, y::AcbFieldElem)

Return the arithmetic-geometric mean of $x$ and $y$.
"""
function agm(x::AcbFieldElem, y::AcbFieldElem)
  v = inv(y)
  if isfinite(v)
    return agm(x * v) * y
  else
    v = inv(x)
    return agm(y * v) * x
  end
end

@doc raw"""
    lindep(A::Vector{AcbFieldElem}, bits::Int)

Find a small linear combination of the entries of the array $A$ that is small
(using LLL). The entries are first scaled by the given number of bits before
truncating the real and imaginary parts to integers for use in LLL. This function can
be used to find linear dependence between a list of complex numbers. The algorithm is
heuristic only and returns an array of Nemo integers representing the linear
combination.
"""
function lindep(A::Vector{AcbFieldElem}, bits::Int)
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
    lindep(A::Matrix{AcbFieldElem}, bits::Int)

Find a (common) small linear combination of the entries in each row of the array $A$,
that is small (using LLL). It is assumed that the complex numbers in each row of the
array share the same linear combination. The entries are first scaled by the given
number of bits before truncating the real and imaginary parts to integers for use in
LLL. This function can be used to find a common linear dependence shared across a
number of lists of complex numbers. The algorithm is heuristic only and returns an
array of Nemo integers representing the common linear combination.
"""
function lindep(A::Matrix{AcbFieldElem}, bits::Int)
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

function zero!(z::AcbFieldElem)
  ccall((:acb_zero, libflint), Nothing, (Ref{AcbFieldElem},), z)
  return z
end

function add!(z::AcbFieldElem, x::AcbFieldElem, y::AcbFieldElem)
  ccall((:acb_add, libflint), Nothing, (Ref{AcbFieldElem}, Ref{AcbFieldElem}, Ref{AcbFieldElem}, Int),
        z, x, y, parent(z).prec)
  return z
end

function sub!(z::AcbFieldElem, x::AcbFieldElem, y::AcbFieldElem)
  ccall((:acb_sub, libflint), Nothing, (Ref{AcbFieldElem}, Ref{AcbFieldElem}, Ref{AcbFieldElem}, Int),
        z, x, y, parent(z).prec)
  return z
end

function mul!(z::AcbFieldElem, x::AcbFieldElem, y::AcbFieldElem)
  ccall((:acb_mul, libflint), Nothing, (Ref{AcbFieldElem}, Ref{AcbFieldElem}, Ref{AcbFieldElem}, Int),
        z, x, y, parent(z).prec)
  return z
end

function div!(z::AcbFieldElem, x::AcbFieldElem, y::AcbFieldElem)
  ccall((:acb_div, libflint), Nothing, (Ref{AcbFieldElem}, Ref{AcbFieldElem}, Ref{AcbFieldElem}, Int),
        z, x, y, parent(z).prec)
  return z
end

################################################################################
#
#  Unsafe setting
#
################################################################################

for (typeofx, passtoc) in ((AcbFieldElem, Ref{AcbFieldElem}), (Ptr{AcbFieldElem}, Ptr{AcbFieldElem}))
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

    function _acb_set(x::($typeofx), y::ArbFieldElem)
      ccall((:acb_set_arb, libflint), Nothing, (($passtoc), Ref{ArbFieldElem}), x, y)
    end

    function _acb_set(x::($typeofx), y::ArbFieldElem, p::Int)
      _acb_set(x, y)
      ccall((:acb_set_round, libflint), Nothing,
            (($passtoc), ($passtoc), Int), x, x, p)
    end

    function _acb_set(x::($typeofx), y::AcbFieldElem)
      ccall((:acb_set, libflint), Nothing, (($passtoc), Ref{AcbFieldElem}), x, y)
    end

    function _acb_set(x::($typeofx), y::AcbFieldElem, p::Int)
      ccall((:acb_set_round, libflint), Nothing,
            (($passtoc), Ref{AcbFieldElem}, Int), x, y, p)
    end

    function _acb_set(x::($typeofx), y::AbstractString, p::Int)
      r = ccall((:acb_real_ptr, libflint), Ptr{ArbFieldElem}, (($passtoc), ), x)
      _arb_set(r, y, p)
      i = ccall((:acb_imag_ptr, libflint), Ptr{ArbFieldElem}, (($passtoc), ), x)
      ccall((:arb_zero, libflint), Nothing, (Ptr{ArbFieldElem}, ), i)
    end

    function _acb_set(x::($typeofx), y::BigFloat)
      r = ccall((:acb_real_ptr, libflint), Ptr{ArbFieldElem}, (($passtoc), ), x)
      _arb_set(r, y)
      i = ccall((:acb_imag_ptr, libflint), Ptr{ArbFieldElem}, (($passtoc), ), x)
      ccall((:arb_zero, libflint), Nothing, (Ptr{ArbFieldElem}, ), i)
    end

    function _acb_set(x::($typeofx), y::BigFloat, p::Int)
      r = ccall((:acb_real_ptr, libflint), Ptr{ArbFieldElem}, (($passtoc), ), x)
      _arb_set(r, y, p)
      i = ccall((:acb_imag_ptr, libflint), Ptr{ArbFieldElem}, (($passtoc), ), x)
      ccall((:arb_zero, libflint), Nothing, (Ptr{ArbFieldElem}, ), i)
    end

    function _acb_set(x::($typeofx), y::Int, z::Int, p::Int)
      ccall((:acb_set_si_si, libflint), Nothing,
            (($passtoc), Int, Int), x, y, z)
      ccall((:acb_set_round, libflint), Nothing,
            (($passtoc), ($passtoc), Int), x, x, p)
    end

    function _acb_set(x::($typeofx), y::ArbFieldElem, z::ArbFieldElem)
      ccall((:acb_set_arb_arb, libflint), Nothing,
            (($passtoc), Ref{ArbFieldElem}, Ref{ArbFieldElem}), x, y, z)
    end

    function _acb_set(x::($typeofx), y::ArbFieldElem, z::ArbFieldElem, p::Int)
      _acb_set(x, y, z)
      ccall((:acb_set_round, libflint), Nothing,
            (($passtoc), ($passtoc), Int), x, x, p)
    end

    function _acb_set(x::($typeofx), y::QQFieldElem, z::QQFieldElem, p::Int)
      r = ccall((:acb_real_ptr, libflint), Ptr{ArbFieldElem}, (($passtoc), ), x)
      _arb_set(r, y, p)
      i = ccall((:acb_imag_ptr, libflint), Ptr{ArbFieldElem}, (($passtoc), ), x)
      _arb_set(i, z, p)
    end

    function _acb_set(x::($typeofx), y::T, z::T, p::Int) where {T <: AbstractString}
      r = ccall((:acb_real_ptr, libflint), Ptr{ArbFieldElem}, (($passtoc), ), x)
      _arb_set(r, y, p)
      i = ccall((:acb_imag_ptr, libflint), Ptr{ArbFieldElem}, (($passtoc), ), x)
      _arb_set(i, z, p)
    end

    function _acb_set(x::($typeofx), y::Real, p::Int)
      r = ccall((:acb_real_ptr, libflint), Ptr{ArbFieldElem}, (($passtoc), ), x)
      _arb_set(r, y, p)
      i = ccall((:acb_imag_ptr, libflint), Ptr{ArbFieldElem}, (($passtoc), ), x)
      ccall((:arb_zero, libflint), Nothing, (Ptr{ArbFieldElem}, ), i)
    end

    function _acb_set(x::($typeofx), y::Complex, p::Int)
      r = ccall((:acb_real_ptr, libflint), Ptr{ArbFieldElem}, (($passtoc), ), x)
      _arb_set(r, real(y), p)
      i = ccall((:acb_imag_ptr, libflint), Ptr{ArbFieldElem}, (($passtoc), ), x)
      _arb_set(i, imag(y), p)
    end

  end

  for T in (Real, ZZRingElem)
    @eval begin
      function _acb_set(x::($typeofx), y::($T), z::($T), p::Int)
        r = ccall((:acb_real_ptr, libflint), Ptr{ArbFieldElem}, (($passtoc), ), x)
        _arb_set(r, y, p)
        i = ccall((:acb_imag_ptr, libflint), Ptr{ArbFieldElem}, (($passtoc), ), x)
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

promote_rule(::Type{AcbFieldElem}, ::Type{T}) where {T <: Number} = AcbFieldElem

promote_rule(::Type{AcbFieldElem}, ::Type{ZZRingElem}) = AcbFieldElem

promote_rule(::Type{AcbFieldElem}, ::Type{QQFieldElem}) = AcbFieldElem

promote_rule(::Type{AcbFieldElem}, ::Type{ArbFieldElem}) = AcbFieldElem

################################################################################
#
#  Parent object overload
#
################################################################################

function (r::AcbField)()
  z = AcbFieldElem()
  z.parent = r
  return z
end

function (r::AcbField)(x::Any)
  z = AcbFieldElem(x, r.prec)
  z.parent = r
  return z
end

function (r::AcbField)(x::T, y::T) where T
  z = AcbFieldElem(x, y, r.prec)
  z.parent = r
  return z
end

for S in (Real, ZZRingElem, QQFieldElem, ArbFieldElem, AbstractString)
  for T in (Real, ZZRingElem, QQFieldElem, ArbFieldElem, AbstractString)
    if S != T || S == Real
      @eval begin
        function (r::AcbField)(x::$(S), y::$(T))
          RR = ArbField(r.prec, cached = false)
          z = AcbFieldElem(RR(x), RR(y), r.prec)
          z.parent = r
          return z
        end
      end
    end
  end
end

################################################################################
#
#  AcbField constructor
#
################################################################################

# see internal constructor
