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

zero(r::ComplexField) = ComplexFieldElem()

one(r::ComplexField) = one!(ComplexFieldElem())

@doc raw"""
    onei(r::ComplexField)

Return exact one times $i$ in the given Arb complex field.
"""
function onei(r::ComplexField)
  z = ComplexFieldElem()
  onei!(z)
  return z
end

@doc raw"""
    accuracy_bits(x::ComplexFieldElem)

Return the relative accuracy of $x$ measured in bits, capped between
`typemax(Int)` and `-typemax(Int)`.
"""
function accuracy_bits(x::ComplexFieldElem)
  # bug in acb.h: rel_accuracy_bits is not in the library
  return -@ccall libflint.acb_rel_error_bits(x::Ref{ComplexFieldElem})::Int
end

function deepcopy_internal(a::ComplexFieldElem, dict::IdDict)
  b = ComplexFieldElem()
  _acb_set(b, a)
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

@doc raw"""
    ComplexF64(x::ComplexFieldElem)

Converts $x$ to a `ComplexF64`, rounded to the nearest.
The return value approximates the midpoint of the real and imaginary parts of $x$.
"""
function Base.ComplexF64(x::ComplexFieldElem)
  GC.@preserve x begin
    re = _real_ptr(x)
    im = _imag_ptr(x)
    t = _mid_ptr(re)
    u = _mid_ptr(im)
    # 4 == round to nearest
    v = @ccall libflint.arf_get_d(t::Ptr{arf_struct}, 4::Int)::Float64
    w = @ccall libflint.arf_get_d(u::Ptr{arf_struct}, 4::Int)::Float64
  end
  return complex(v, w)
end

@doc raw"""
    Float64(x::ComplexFieldElem)

Converts $x$ to a `Float64`, rounded to the nearest.
The return value approximates the midpoint of the real part of $x$.
"""
function Float64(x::ComplexFieldElem)
  @req isreal(x) "conversion to float must have no imaginary part"
  GC.@preserve x begin
    re = _real_ptr(x)
    t = _mid_ptr(re)
    # 4 == round to nearest
    v = @ccall libflint.arf_get_d(t::Ptr{arf_struct}, 4::Int)::Float64
  end
  return v
end

function convert(::Type{ComplexF64}, x::ComplexFieldElem)
  return ComplexF64(x)
end

function convert(::Type{Float64}, x::ComplexFieldElem)
  return Float64(x)
end

################################################################################
#
#  Real and imaginary part
#
################################################################################

function real(x::ComplexFieldElem)
  z = RealFieldElem()
  @ccall libflint.acb_get_real(z::Ref{RealFieldElem}, x::Ref{ComplexFieldElem})::Nothing
  return z
end

function imag(x::ComplexFieldElem)
  z = RealFieldElem()
  @ccall libflint.acb_get_imag(z::Ref{RealFieldElem}, x::Ref{ComplexFieldElem})::Nothing
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

-(x::ComplexFieldElem) = neg!(ComplexFieldElem(), x)

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
  @ccall libflint.acb_sub_ui(z::Ref{ComplexFieldElem}, y::Ref{ComplexFieldElem}, x::UInt, precision(Balls)::Int)::Nothing
  return neg!(z)
end

function -(x::Int, y::ComplexFieldElem)
  z = ComplexFieldElem()
  @ccall libflint.acb_sub_si(z::Ref{ComplexFieldElem}, y::Ref{ComplexFieldElem}, x::Int, precision(Balls)::Int)::Nothing
  return neg!(z)
end

function -(x::ZZRingElem, y::ComplexFieldElem)
  z = ComplexFieldElem()
  @ccall libflint.acb_sub_fmpz(z::Ref{ComplexFieldElem}, y::Ref{ComplexFieldElem}, x::Ref{ZZRingElem}, precision(Balls)::Int)::Nothing
  return neg!(z)
end

function -(x::RealFieldElem, y::ComplexFieldElem)
  z = ComplexFieldElem()
  @ccall libflint.acb_sub_arb(z::Ref{ComplexFieldElem}, y::Ref{ComplexFieldElem}, x::Ref{RealFieldElem}, precision(Balls)::Int)::Nothing
  return neg!(z)
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
  r = @ccall libflint.acb_equal(x::Ref{ComplexFieldElem}, y::Ref{ComplexFieldElem})::Cint
  return Bool(r)
end

function Base.hash(x::ComplexFieldElem, h::UInt)
  # TODO: improve me
  return h
end

function ==(x::ComplexFieldElem, y::ComplexFieldElem)
  r = @ccall libflint.acb_eq(x::Ref{ComplexFieldElem}, y::Ref{ComplexFieldElem})::Cint
  return Bool(r)
end

function !=(x::ComplexFieldElem, y::ComplexFieldElem)
  r = @ccall libflint.acb_ne(x::Ref{ComplexFieldElem}, y::Ref{ComplexFieldElem})::Cint
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
  r = @ccall libflint.acb_overlaps(x::Ref{ComplexFieldElem}, y::Ref{ComplexFieldElem})::Cint
  return Bool(r)
end

@doc raw"""
    contains(x::ComplexFieldElem, y::ComplexFieldElem)

Returns `true` if the box $x$ contains the box $y$, otherwise return
`false`.
"""
function contains(x::ComplexFieldElem, y::ComplexFieldElem)
  r = @ccall libflint.acb_contains(x::Ref{ComplexFieldElem}, y::Ref{ComplexFieldElem})::Cint
  return Bool(r)
end

@doc raw"""
    contains(x::ComplexFieldElem, y::QQFieldElem)

Returns `true` if the box $x$ contains the given rational value, otherwise
return `false`.
"""
function contains(x::ComplexFieldElem, y::QQFieldElem)
  r = @ccall libflint.acb_contains_fmpq(x::Ref{ComplexFieldElem}, y::Ref{QQFieldElem})::Cint
  return Bool(r)
end

@doc raw"""
    contains(x::ComplexFieldElem, y::ZZRingElem)

Returns `true` if the box $x$ contains the given integer value, otherwise
return `false`.
"""
function contains(x::ComplexFieldElem, y::ZZRingElem)
  r = @ccall libflint.acb_contains_fmpz(x::Ref{ComplexFieldElem}, y::Ref{ZZRingElem})::Cint
  return Bool(r)
end

function contains(x::ComplexFieldElem, y::Int)
  v = ZZRingElem(y)
  r = @ccall libflint.acb_contains_fmpz(x::Ref{ComplexFieldElem}, v::Ref{ZZRingElem})::Cint
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
  return Bool(@ccall libflint.acb_contains_zero(x::Ref{ComplexFieldElem})::Cint)
end

################################################################################
#
#  Predicates
#
################################################################################

@doc raw"""
    iszero(x::ComplexFieldElem)

Return `true` if $x$ is certainly zero, otherwise return `false`.
"""
function iszero(x::ComplexFieldElem)
  return Bool(@ccall libflint.acb_is_zero(x::Ref{ComplexFieldElem})::Cint)
end

@doc raw"""
    isone(x::ComplexFieldElem)

Return `true` if $x$ is certainly one, otherwise return `false`.
"""
function isone(x::ComplexFieldElem)
  return Bool(@ccall libflint.acb_is_one(x::Ref{ComplexFieldElem})::Cint)
end

@doc raw"""
    isfinite(x::ComplexFieldElem)

Return `true` if $x$ is finite, i.e. its real and imaginary parts have finite
midpoint and radius, otherwise return `false`.
"""
function isfinite(x::ComplexFieldElem)
  return Bool(@ccall libflint.acb_is_finite(x::Ref{ComplexFieldElem})::Cint)
end

@doc raw"""
    is_exact(x::ComplexFieldElem)

Return `true` if $x$ is exact, i.e. has its real and imaginary parts have
zero radius, otherwise return `false`.
"""
function is_exact(x::ComplexFieldElem)
  return Bool(@ccall libflint.acb_is_exact(x::Ref{ComplexFieldElem})::Cint)
end

@doc raw"""
    isinteger(x::ComplexFieldElem)

Return `true` if $x$ is an exact integer, otherwise return `false`.
"""
function isinteger(x::ComplexFieldElem)
  return Bool(@ccall libflint.acb_is_int(x::Ref{ComplexFieldElem})::Cint)
end

function isreal(x::ComplexFieldElem)
  return Bool(@ccall libflint.acb_is_real(x::Ref{ComplexFieldElem})::Cint)
end

is_negative(x::ComplexFieldElem) = isreal(x) && is_negative(real(x))

################################################################################
#
#  Absolute value
#
################################################################################

function abs(x::ComplexFieldElem, prec::Int = precision(Balls))
  z = RealFieldElem()
  @ccall libflint.acb_abs(z::Ref{RealFieldElem}, x::Ref{ComplexFieldElem}, prec::Int)::Nothing
  return z
end

################################################################################
#
#  Inversion
#
################################################################################

function inv(x::ComplexFieldElem, prec::Int = precision(Balls))
  z = ComplexFieldElem()
  @ccall libflint.acb_inv(z::Ref{ComplexFieldElem}, x::Ref{ComplexFieldElem}, prec::Int)::Nothing
  return z
end

################################################################################
#
#  Shifting
#
################################################################################

function ldexp(x::ComplexFieldElem, y::Int)
  z = ComplexFieldElem()
  @ccall libflint.acb_mul_2exp_si(z::Ref{ComplexFieldElem}, x::Ref{ComplexFieldElem}, y::Int)::Nothing
  return z
end

function ldexp(x::ComplexFieldElem, y::ZZRingElem)
  z = ComplexFieldElem()
  @ccall libflint.acb_mul_2exp_fmpz(z::Ref{ComplexFieldElem}, x::Ref{ComplexFieldElem}, y::Ref{ZZRingElem})::Nothing
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
  @ccall libflint.acb_trim(z::Ref{ComplexFieldElem}, x::Ref{ComplexFieldElem})::Nothing
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
  unique = @ccall libflint.acb_get_unique_fmpz(z::Ref{ZZRingElem}, x::Ref{ComplexFieldElem})::Int
  return (unique != 0, z)
end

function conj(x::ComplexFieldElem)
  z = ComplexFieldElem()
  @ccall libflint.acb_conj(z::Ref{ComplexFieldElem}, x::Ref{ComplexFieldElem})::Nothing
  return z
end

function angle(x::ComplexFieldElem, prec::Int = precision(Balls))
  z = RealFieldElem()
  @ccall libflint.acb_arg(z::Ref{RealFieldElem}, x::Ref{ComplexFieldElem}, prec::Int)::Nothing
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
  @ccall libflint.acb_const_pi(z::Ref{ComplexFieldElem}, prec::Int)::Nothing
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
  @ccall libflint.acb_sqrt(z::Ref{ComplexFieldElem}, x::Ref{ComplexFieldElem}, prec::Int)::Nothing
  return z
end

@doc raw"""
    rsqrt(x::ComplexFieldElem)

Return the reciprocal of the square root of $x$, i.e. $1/\sqrt{x}$.
"""
function rsqrt(x::ComplexFieldElem, prec::Int = precision(Balls))
  z = ComplexFieldElem()
  @ccall libflint.acb_rsqrt(z::Ref{ComplexFieldElem}, x::Ref{ComplexFieldElem}, prec::Int)::Nothing
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
  @ccall libflint.acb_root_ui(z::Ref{ComplexFieldElem}, x::Ref{ComplexFieldElem}, UInt(n)::UInt, prec::Int)::Nothing
  return z
end


function log(x::ComplexFieldElem, prec::Int = precision(Balls))
  z = ComplexFieldElem()
  @ccall libflint.acb_log(z::Ref{ComplexFieldElem}, x::Ref{ComplexFieldElem}, prec::Int)::Nothing
  return z
end

function log1p(x::ComplexFieldElem, prec::Int = precision(Balls))
  z = ComplexFieldElem()
  @ccall libflint.acb_log1p(z::Ref{ComplexFieldElem}, x::Ref{ComplexFieldElem}, prec::Int)::Nothing
  return z
end

function Base.exp(x::ComplexFieldElem, prec::Int = precision(Balls))
  z = ComplexFieldElem()
  @ccall libflint.acb_exp(z::Ref{ComplexFieldElem}, x::Ref{ComplexFieldElem}, prec::Int)::Nothing
  return z
end

function Base.expm1(x::ComplexFieldElem, prec::Int = precision(Balls))
  z = ComplexFieldElem()
  @ccall libflint.acb_expm1(z::Ref{ComplexFieldElem}, x::Ref{ComplexFieldElem}, prec::Int)::Nothing
  return z
end

@doc raw"""
    cispi(x::ComplexFieldElem)

Return the exponential of $\pi i x$.
"""
function cispi(x::ComplexFieldElem, prec::Int = precision(Balls))
  z = ComplexFieldElem()
  @ccall libflint.acb_exp_pi_i(z::Ref{ComplexFieldElem}, x::Ref{ComplexFieldElem}, prec::Int)::Nothing
  return z
end

@doc raw"""
    root_of_unity(C::ComplexField, k::Int)

Return $\exp(2\pi i/k)$.
"""
function root_of_unity(C::ComplexField, k::Int, prec::Int = precision(Balls))
  k <= 0 && throw(ArgumentError("Order must be positive ($k)"))
  z = C()
  @ccall libflint.acb_unit_root(z::Ref{ComplexFieldElem}, k::UInt, prec::Int)::Nothing
  return z
end

function sin(x::ComplexFieldElem, prec::Int = precision(Balls))
  z = ComplexFieldElem()
  @ccall libflint.acb_sin(z::Ref{ComplexFieldElem}, x::Ref{ComplexFieldElem}, prec::Int)::Nothing
  return z
end

function cos(x::ComplexFieldElem, prec::Int = precision(Balls))
  z = ComplexFieldElem()
  @ccall libflint.acb_cos(z::Ref{ComplexFieldElem}, x::Ref{ComplexFieldElem}, prec::Int)::Nothing
  return z
end

function tan(x::ComplexFieldElem, prec::Int = precision(Balls))
  z = ComplexFieldElem()
  @ccall libflint.acb_tan(z::Ref{ComplexFieldElem}, x::Ref{ComplexFieldElem}, prec::Int)::Nothing
  return z
end

function cot(x::ComplexFieldElem, prec::Int = precision(Balls))
  z = ComplexFieldElem()
  @ccall libflint.acb_cot(z::Ref{ComplexFieldElem}, x::Ref{ComplexFieldElem}, prec::Int)::Nothing
  return z
end

function sinpi(x::ComplexFieldElem, prec::Int = precision(Balls))
  z = ComplexFieldElem()
  @ccall libflint.acb_sin_pi(z::Ref{ComplexFieldElem}, x::Ref{ComplexFieldElem}, prec::Int)::Nothing
  return z
end

function cospi(x::ComplexFieldElem, prec::Int = precision(Balls))
  z = ComplexFieldElem()
  @ccall libflint.acb_cos_pi(z::Ref{ComplexFieldElem}, x::Ref{ComplexFieldElem}, prec::Int)::Nothing
  return z
end

function tanpi(x::ComplexFieldElem, prec::Int = precision(Balls))
  z = ComplexFieldElem()
  @ccall libflint.acb_tan_pi(z::Ref{ComplexFieldElem}, x::Ref{ComplexFieldElem}, prec::Int)::Nothing
  return z
end

function cotpi(x::ComplexFieldElem, prec::Int = precision(Balls))
  z = ComplexFieldElem()
  @ccall libflint.acb_cot_pi(z::Ref{ComplexFieldElem}, x::Ref{ComplexFieldElem}, prec::Int)::Nothing
  return z
end

function sinh(x::ComplexFieldElem, prec::Int = precision(Balls))
  z = ComplexFieldElem()
  @ccall libflint.acb_sinh(z::Ref{ComplexFieldElem}, x::Ref{ComplexFieldElem}, prec::Int)::Nothing
  return z
end

function cosh(x::ComplexFieldElem, prec::Int = precision(Balls))
  z = ComplexFieldElem()
  @ccall libflint.acb_cosh(z::Ref{ComplexFieldElem}, x::Ref{ComplexFieldElem}, prec::Int)::Nothing
  return z
end

function tanh(x::ComplexFieldElem, prec::Int = precision(Balls))
  z = ComplexFieldElem()
  @ccall libflint.acb_tanh(z::Ref{ComplexFieldElem}, x::Ref{ComplexFieldElem}, prec::Int)::Nothing
  return z
end

function coth(x::ComplexFieldElem, prec::Int = precision(Balls))
  z = ComplexFieldElem()
  @ccall libflint.acb_coth(z::Ref{ComplexFieldElem}, x::Ref{ComplexFieldElem}, prec::Int)::Nothing
  return z
end

function atan(x::ComplexFieldElem, prec::Int = precision(Balls))
  z = ComplexFieldElem()
  @ccall libflint.acb_atan(z::Ref{ComplexFieldElem}, x::Ref{ComplexFieldElem}, prec::Int)::Nothing
  return z
end

@doc raw"""
    log_sinpi(x::ComplexFieldElem)

Return $\log\sin(\pi x)$, constructed without branch cuts off the real line.
"""
function log_sinpi(x::ComplexFieldElem, prec::Int = precision(Balls))
  z = ComplexFieldElem()
  @ccall libflint.acb_log_sin_pi(z::Ref{ComplexFieldElem}, x::Ref{ComplexFieldElem}, prec::Int)::Nothing
  return z
end

@doc raw"""
    gamma(x::ComplexFieldElem)

Return the Gamma function evaluated at $x$.
"""
function gamma(x::ComplexFieldElem, prec::Int = precision(Balls))
  z = ComplexFieldElem()
  @ccall libflint.acb_gamma(z::Ref{ComplexFieldElem}, x::Ref{ComplexFieldElem}, prec::Int)::Nothing
  return z
end

@doc raw"""
    rgamma(x::ComplexFieldElem)

Return the reciprocal of the Gamma function evaluated at $x$.
"""
function rgamma(x::ComplexFieldElem, prec::Int = precision(Balls))
  z = ComplexFieldElem()
  @ccall libflint.acb_rgamma(z::Ref{ComplexFieldElem}, x::Ref{ComplexFieldElem}, prec::Int)::Nothing
  return z
end

@doc raw"""
    lgamma(x::ComplexFieldElem)

Return the logarithm of the Gamma function evaluated at $x$.
"""
function lgamma(x::ComplexFieldElem, prec::Int = precision(Balls))
  z = ComplexFieldElem()
  @ccall libflint.acb_lgamma(z::Ref{ComplexFieldElem}, x::Ref{ComplexFieldElem}, prec::Int)::Nothing
  return z
end

@doc raw"""
    digamma(x::ComplexFieldElem)

Return the  logarithmic derivative of the gamma function evaluated at $x$,
i.e. $\psi(x)$.
"""
function digamma(x::ComplexFieldElem, prec::Int = precision(Balls))
  z = ComplexFieldElem()
  @ccall libflint.acb_digamma(z::Ref{ComplexFieldElem}, x::Ref{ComplexFieldElem}, prec::Int)::Nothing
  return z
end

@doc raw"""
    zeta(x::ComplexFieldElem)

Return the Riemann zeta function evaluated at $x$.
"""
function zeta(x::ComplexFieldElem, prec::Int = precision(Balls))
  z = ComplexFieldElem()
  @ccall libflint.acb_zeta(z::Ref{ComplexFieldElem}, x::Ref{ComplexFieldElem}, prec::Int)::Nothing
  return z
end

@doc raw"""
    barnes_g(x::ComplexFieldElem)

Return the Barnes $G$-function, evaluated at $x$.
"""
function barnes_g(x::ComplexFieldElem, prec::Int = precision(Balls))
  z = ComplexFieldElem()
  @ccall libflint.acb_barnes_g(z::Ref{ComplexFieldElem}, x::Ref{ComplexFieldElem}, prec::Int)::Nothing
  return z
end

@doc raw"""
    log_barnes_g(x::ComplexFieldElem)

Return the logarithm of the Barnes $G$-function, evaluated at $x$.
"""
function log_barnes_g(x::ComplexFieldElem, prec::Int = precision(Balls))
  z = ComplexFieldElem()
  @ccall libflint.acb_log_barnes_g(z::Ref{ComplexFieldElem}, x::Ref{ComplexFieldElem}, prec::Int)::Nothing
  return z
end

@doc raw"""
    agm(x::ComplexFieldElem)

Return the arithmetic-geometric mean of $1$ and $x$.
"""
function agm(x::ComplexFieldElem, prec::Int = precision(Balls))
  z = ComplexFieldElem()
  @ccall libflint.acb_agm1(z::Ref{ComplexFieldElem}, x::Ref{ComplexFieldElem}, prec::Int)::Nothing
  return z
end

@doc raw"""
    erf(x::ComplexFieldElem)

Return the error function evaluated at $x$.
"""
function erf(x::ComplexFieldElem, prec::Int = precision(Balls))
  z = ComplexFieldElem()
  @ccall libflint.acb_hypgeom_erf(z::Ref{ComplexFieldElem}, x::Ref{ComplexFieldElem}, prec::Int)::Nothing
  return z
end

@doc raw"""
    erfi(x::ComplexFieldElem)

Return the imaginary error function evaluated at $x$.
"""
function erfi(x::ComplexFieldElem, prec::Int = precision(Balls))
  z = ComplexFieldElem()
  @ccall libflint.acb_hypgeom_erfi(z::Ref{ComplexFieldElem}, x::Ref{ComplexFieldElem}, prec::Int)::Nothing
  return z
end

@doc raw"""
    erfc(x::ComplexFieldElem)

Return the complementary error function evaluated at $x$.
"""
function erfc(x::ComplexFieldElem, prec::Int = precision(Balls))
  z = ComplexFieldElem()
  @ccall libflint.acb_hypgeom_erfc(z::Ref{ComplexFieldElem}, x::Ref{ComplexFieldElem}, prec::Int)::Nothing
  return z
end

@doc raw"""
    exp_integral_ei(x::ComplexFieldElem)

Return the exponential integral evaluated at $x$.
"""
function exp_integral_ei(x::ComplexFieldElem, prec::Int = precision(Balls))
  z = ComplexFieldElem()
  @ccall libflint.acb_hypgeom_ei(z::Ref{ComplexFieldElem}, x::Ref{ComplexFieldElem}, prec::Int)::Nothing
  return z
end

@doc raw"""
    sin_integral(x::ComplexFieldElem)

Return the sine integral evaluated at $x$.
"""
function sin_integral(x::ComplexFieldElem, prec::Int = precision(Balls))
  z = ComplexFieldElem()
  @ccall libflint.acb_hypgeom_si(z::Ref{ComplexFieldElem}, x::Ref{ComplexFieldElem}, prec::Int)::Nothing
  return z
end

@doc raw"""
    cos_integral(x::ComplexFieldElem)

Return the exponential cosine integral evaluated at $x$.
"""
function cos_integral(x::ComplexFieldElem, prec::Int = precision(Balls))
  z = ComplexFieldElem()
  @ccall libflint.acb_hypgeom_ci(z::Ref{ComplexFieldElem}, x::Ref{ComplexFieldElem}, prec::Int)::Nothing
  return z
end

@doc raw"""
    sinh_integral(x::ComplexFieldElem)

Return the hyperbolic sine integral evaluated at $x$.
"""
function sinh_integral(x::ComplexFieldElem, prec::Int = precision(Balls))
  z = ComplexFieldElem()
  @ccall libflint.acb_hypgeom_shi(z::Ref{ComplexFieldElem}, x::Ref{ComplexFieldElem}, prec::Int)::Nothing
  return z
end

@doc raw"""
    cosh_integral(x::ComplexFieldElem)

Return the hyperbolic cosine integral evaluated at $x$.
"""
function cosh_integral(x::ComplexFieldElem, prec::Int = precision(Balls))
  z = ComplexFieldElem()
  @ccall libflint.acb_hypgeom_chi(z::Ref{ComplexFieldElem}, x::Ref{ComplexFieldElem}, prec::Int)::Nothing
  return z
end

@doc raw"""
    dedekind_eta(x::ComplexFieldElem)

Return the Dedekind eta function $\eta(\tau)$ at $\tau = x$.
"""
function dedekind_eta(x::ComplexFieldElem, prec::Int = precision(Balls))
  z = ComplexFieldElem()
  @ccall libflint.acb_modular_eta(z::Ref{ComplexFieldElem}, x::Ref{ComplexFieldElem}, prec::Int)::Nothing
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
  @ccall libflint.acb_modular_j(z::Ref{ComplexFieldElem}, x::Ref{ComplexFieldElem}, prec::Int)::Nothing
  return z
end

@doc raw"""
    modular_lambda(x::ComplexFieldElem)

Return the modular lambda function $\lambda(\tau)$ at $\tau = x$.
"""
function modular_lambda(x::ComplexFieldElem, prec::Int = precision(Balls))
  z = ComplexFieldElem()
  @ccall libflint.acb_modular_lambda(z::Ref{ComplexFieldElem}, x::Ref{ComplexFieldElem}, prec::Int)::Nothing
  return z
end

@doc raw"""
    modular_delta(x::ComplexFieldElem)

Return the modular delta function $\Delta(\tau)$ at $\tau = x$.
"""
function modular_delta(x::ComplexFieldElem, prec::Int = precision(Balls))
  z = ComplexFieldElem()
  @ccall libflint.acb_modular_delta(z::Ref{ComplexFieldElem}, x::Ref{ComplexFieldElem}, prec::Int)::Nothing
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
  @ccall libflint.acb_modular_eisenstein(vec::Ptr{acb_struct}, x::Ref{ComplexFieldElem}, len::Int, prec::Int)::Nothing
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
  @ccall libflint.acb_modular_hilbert_class_poly(z::Ref{ZZPolyRingElem}, D::Int)::Nothing
  return z
end

@doc raw"""
    elliptic_k(x::ComplexFieldElem)

Return the complete elliptic integral $K(x)$.
"""
function elliptic_k(x::ComplexFieldElem, prec::Int = precision(Balls))
  z = ComplexFieldElem()
  @ccall libflint.acb_modular_elliptic_k(z::Ref{ComplexFieldElem}, x::Ref{ComplexFieldElem}, prec::Int)::Nothing
  return z
end

@doc raw"""
    elliptic_e(x::ComplexFieldElem)

Return the complete elliptic integral $E(x)$.
"""
function elliptic_e(x::ComplexFieldElem, prec::Int = precision(Balls))
  z = ComplexFieldElem()
  @ccall libflint.acb_modular_elliptic_e(z::Ref{ComplexFieldElem}, x::Ref{ComplexFieldElem}, prec::Int)::Nothing
  return z
end

function sincos(x::ComplexFieldElem, prec::Int = precision(Balls))
  s = ComplexFieldElem()
  c = ComplexFieldElem()
  @ccall libflint.acb_sin_cos(s::Ref{ComplexFieldElem}, c::Ref{ComplexFieldElem}, x::Ref{ComplexFieldElem}, prec::Int)::Nothing
  return (s, c)
end

function sincospi(x::ComplexFieldElem, prec::Int = precision(Balls))
  s = ComplexFieldElem()
  c = ComplexFieldElem()
  @ccall libflint.acb_sin_cos_pi(s::Ref{ComplexFieldElem}, c::Ref{ComplexFieldElem}, x::Ref{ComplexFieldElem}, prec::Int)::Nothing
  return (s, c)
end

@doc raw"""
    sinhcosh(x::ComplexFieldElem)

Return a tuple $s, c$ consisting of the hyperbolic sine and cosine of $x$.
"""
function sinhcosh(x::ComplexFieldElem, prec::Int = precision(Balls))
  s = ComplexFieldElem()
  c = ComplexFieldElem()
  @ccall libflint.acb_sinh_cosh(s::Ref{ComplexFieldElem}, c::Ref{ComplexFieldElem}, x::Ref{ComplexFieldElem}, prec::Int)::Nothing
  return (s, c)
end

@doc raw"""
    zeta(s::ComplexFieldElem, a::ComplexFieldElem)

Return the Hurwitz zeta function $\zeta(s,a)$.
"""
function zeta(s::ComplexFieldElem, a::ComplexFieldElem, prec::Int = precision(Balls))
  z = parent(s)()
  @ccall libflint.acb_hurwitz_zeta(z::Ref{ComplexFieldElem}, s::Ref{ComplexFieldElem}, a::Ref{ComplexFieldElem}, prec::Int)::Nothing
  return z
end

@doc raw"""
    polygamma(s::ComplexFieldElem, a::ComplexFieldElem)

Return the generalised polygamma function $\psi(s,z)$.
"""
function polygamma(s::ComplexFieldElem, a::ComplexFieldElem, prec::Int = precision(Balls))
  z = parent(s)()
  @ccall libflint.acb_polygamma(z::Ref{ComplexFieldElem}, s::Ref{ComplexFieldElem}, a::Ref{ComplexFieldElem}, prec::Int)::Nothing
  return z
end

function rising_factorial(x::ComplexFieldElem, n::UInt, prec::Int = precision(Balls))
  z = ComplexFieldElem()
  @ccall libflint.acb_rising_ui(z::Ref{ComplexFieldElem}, x::Ref{ComplexFieldElem}, n::UInt, prec::Int)::Nothing
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
  @ccall libflint.acb_rising2_ui(z::Ref{ComplexFieldElem}, w::Ref{ComplexFieldElem}, x::Ref{ComplexFieldElem}, n::UInt, prec::Int)::Nothing
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
  @ccall libflint.acb_polylog(z::Ref{ComplexFieldElem}, s::Ref{ComplexFieldElem}, a::Ref{ComplexFieldElem}, prec::Int)::Nothing
  return z
end

function polylog(s::Int, a::ComplexFieldElem, prec::Int = precision(Balls))
  z = parent(a)()
  @ccall libflint.acb_polylog_si(z::Ref{ComplexFieldElem}, s::Int, a::Ref{ComplexFieldElem}, prec::Int)::Nothing
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
  @ccall libflint.acb_hypgeom_li(z::Ref{ComplexFieldElem}, x::Ref{ComplexFieldElem}, 0::Int, prec::Int)::Nothing
  return z
end

@doc raw"""
    log_integral_offset(x::ComplexFieldElem)

Return the offset logarithmic integral, evaluated at $x$.
"""
function log_integral_offset(x::ComplexFieldElem, prec::Int = precision(Balls))
  z = ComplexFieldElem()
  @ccall libflint.acb_hypgeom_li(z::Ref{ComplexFieldElem}, x::Ref{ComplexFieldElem}, 1::Int, prec::Int)::Nothing
  return z
end

@doc raw"""
    exp_integral_e(s::ComplexFieldElem, x::ComplexFieldElem)

Return the generalised exponential integral $E_s(x)$.
"""
function exp_integral_e(s::ComplexFieldElem, x::ComplexFieldElem, prec::Int = precision(Balls))
  z = parent(s)()
  @ccall libflint.acb_hypgeom_expint(z::Ref{ComplexFieldElem}, s::Ref{ComplexFieldElem}, x::Ref{ComplexFieldElem}, prec::Int)::Nothing
  return z
end

@doc raw"""
    gamma(s::ComplexFieldElem, x::ComplexFieldElem)

Return the upper incomplete gamma function $\Gamma(s,x)$.
"""
function gamma(s::ComplexFieldElem, x::ComplexFieldElem, prec::Int = precision(Balls))
  z = parent(s)()
  @ccall libflint.acb_hypgeom_gamma_upper(z::Ref{ComplexFieldElem}, s::Ref{ComplexFieldElem}, x::Ref{ComplexFieldElem}, 0::Int, prec::Int)::Nothing
  return z
end

@doc raw"""
    gamma_regularized(s::ComplexFieldElem, x::ComplexFieldElem)

Return the regularized upper incomplete gamma function
$\Gamma(s,x) / \Gamma(s)$.
"""
function gamma_regularized(s::ComplexFieldElem, x::ComplexFieldElem, prec::Int = precision(Balls))
  z = parent(s)()
  @ccall libflint.acb_hypgeom_gamma_upper(z::Ref{ComplexFieldElem}, s::Ref{ComplexFieldElem}, x::Ref{ComplexFieldElem}, 1::Int, prec::Int)::Nothing
  return z
end

@doc raw"""
    gamma_lower(s::ComplexFieldElem, x::ComplexFieldElem)

Return the lower incomplete gamma function $\gamma(s,x) / \Gamma(s)$.
"""
function gamma_lower(s::ComplexFieldElem, x::ComplexFieldElem, prec::Int = precision(Balls))
  z = parent(s)()
  @ccall libflint.acb_hypgeom_gamma_lower(z::Ref{ComplexFieldElem}, s::Ref{ComplexFieldElem}, x::Ref{ComplexFieldElem}, 0::Int, prec::Int)::Nothing
  return z
end

@doc raw"""
    gamma_lower_regularized(s::ComplexFieldElem, x::ComplexFieldElem)

Return the regularized lower incomplete gamma function
$\gamma(s,x) / \Gamma(s)$.
"""
function gamma_lower_regularized(s::ComplexFieldElem, x::ComplexFieldElem, prec::Int = precision(Balls))
  z = parent(s)()
  @ccall libflint.acb_hypgeom_gamma_lower(z::Ref{ComplexFieldElem}, s::Ref{ComplexFieldElem}, x::Ref{ComplexFieldElem}, 1::Int, prec::Int)::Nothing
  return z
end

@doc raw"""
    bessel_j(nu::ComplexFieldElem, x::ComplexFieldElem)

Return the Bessel function $J_{\nu}(x)$.
"""
function bessel_j(nu::ComplexFieldElem, x::ComplexFieldElem, prec::Int = precision(Balls))
  z = ComplexFieldElem()
  @ccall libflint.acb_hypgeom_bessel_j(z::Ref{ComplexFieldElem}, nu::Ref{ComplexFieldElem}, x::Ref{ComplexFieldElem}, prec::Int)::Nothing
  return z
end

@doc raw"""
    bessel_y(nu::ComplexFieldElem, x::ComplexFieldElem)

Return the Bessel function $Y_{\nu}(x)$.
"""
function bessel_y(nu::ComplexFieldElem, x::ComplexFieldElem, prec::Int = precision(Balls))
  z = ComplexFieldElem()
  @ccall libflint.acb_hypgeom_bessel_y(z::Ref{ComplexFieldElem}, nu::Ref{ComplexFieldElem}, x::Ref{ComplexFieldElem}, prec::Int)::Nothing
  return z
end

@doc raw"""
    bessel_i(nu::ComplexFieldElem, x::ComplexFieldElem)

Return the Bessel function $I_{\nu}(x)$.
"""
function bessel_i(nu::ComplexFieldElem, x::ComplexFieldElem, prec::Int = precision(Balls))
  z = ComplexFieldElem()
  @ccall libflint.acb_hypgeom_bessel_i(z::Ref{ComplexFieldElem}, nu::Ref{ComplexFieldElem}, x::Ref{ComplexFieldElem}, prec::Int)::Nothing
  return z
end

@doc raw"""
    bessel_k(nu::ComplexFieldElem, x::ComplexFieldElem)

Return the Bessel function $K_{\nu}(x)$.
"""
function bessel_k(nu::ComplexFieldElem, x::ComplexFieldElem, prec::Int = precision(Balls))
  z = ComplexFieldElem()
  @ccall libflint.acb_hypgeom_bessel_k(z::Ref{ComplexFieldElem}, nu::Ref{ComplexFieldElem}, x::Ref{ComplexFieldElem}, prec::Int)::Nothing
  return z
end

@doc raw"""
    airy_ai(x::ComplexFieldElem)

Return the Airy function $\operatorname{Ai}(x)$.
"""
function airy_ai(x::ComplexFieldElem, prec::Int = precision(Balls))
  ai = ComplexFieldElem()
  @ccall libflint.acb_hypgeom_airy(ai::Ref{ComplexFieldElem}, C_NULL::Ptr{Cvoid}, C_NULL::Ptr{Cvoid}, C_NULL::Ptr{Cvoid}, x::Ref{ComplexFieldElem}, prec::Int)::Nothing
  return ai
end

@doc raw"""
    airy_bi(x::ComplexFieldElem)

Return the Airy function $\operatorname{Bi}(x)$.
"""
function airy_bi(x::ComplexFieldElem, prec::Int = precision(Balls))
  bi = ComplexFieldElem()
  @ccall libflint.acb_hypgeom_airy(C_NULL::Ptr{Cvoid}, C_NULL::Ptr{Cvoid}, bi::Ref{ComplexFieldElem}, C_NULL::Ptr{Cvoid}, x::Ref{ComplexFieldElem}, prec::Int)::Nothing
  return bi
end

@doc raw"""
    airy_ai_prime(x::ComplexFieldElem)

Return the derivative of the Airy function $\operatorname{Ai}^\prime(x)$.
"""
function airy_ai_prime(x::ComplexFieldElem, prec::Int = precision(Balls))
  ai_prime = ComplexFieldElem()
  @ccall libflint.acb_hypgeom_airy(C_NULL::Ptr{Cvoid}, ai_prime::Ref{ComplexFieldElem}, C_NULL::Ptr{Cvoid}, C_NULL::Ptr{Cvoid}, x::Ref{ComplexFieldElem}, prec::Int)::Nothing
  return ai_prime
end

@doc raw"""
    airy_bi_prime(x::ComplexFieldElem)

Return the derivative of the Airy function $\operatorname{Bi}^\prime(x)$.
"""
function airy_bi_prime(x::ComplexFieldElem, prec::Int = precision(Balls))
  bi_prime = ComplexFieldElem()
  @ccall libflint.acb_hypgeom_airy(C_NULL::Ptr{Cvoid}, C_NULL::Ptr{Cvoid}, C_NULL::Ptr{Cvoid}, bi_prime::Ref{ComplexFieldElem}, x::Ref{ComplexFieldElem}, prec::Int)::Nothing
  return bi_prime
end

@doc raw"""
    hypergeometric_1f1(a::ComplexFieldElem, b::ComplexFieldElem, x::ComplexFieldElem)

Return the confluent hypergeometric function ${}_1F_1(a,b,x)$.
"""
function hypergeometric_1f1(a::ComplexFieldElem, b::ComplexFieldElem, x::ComplexFieldElem, prec::Int = precision(Balls))
  z = ComplexFieldElem()
  @ccall libflint.acb_hypgeom_m(z::Ref{ComplexFieldElem}, a::Ref{ComplexFieldElem}, b::Ref{ComplexFieldElem}, x::Ref{ComplexFieldElem}, 0::Int, prec::Int)::Nothing
  return z
end

@doc raw"""
    hypergeometric_1f1_regularized(a::ComplexFieldElem, b::ComplexFieldElem, x::ComplexFieldElem)

Return the regularized confluent hypergeometric function
${}_1F_1(a,b,x) / \Gamma(b)$.
"""
function hypergeometric_1f1_regularized(a::ComplexFieldElem, b::ComplexFieldElem, x::ComplexFieldElem, prec::Int = precision(Balls))
  z = ComplexFieldElem()
  @ccall libflint.acb_hypgeom_m(z::Ref{ComplexFieldElem}, a::Ref{ComplexFieldElem}, b::Ref{ComplexFieldElem}, x::Ref{ComplexFieldElem}, 1::Int, prec::Int)::Nothing
  return z
end

@doc raw"""
    hypergeometric_u(a::ComplexFieldElem, b::ComplexFieldElem, x::ComplexFieldElem)

Return the confluent hypergeometric function $U(a,b,x)$.
"""
function hypergeometric_u(a::ComplexFieldElem, b::ComplexFieldElem, x::ComplexFieldElem, prec::Int = precision(Balls))
  z = ComplexFieldElem()
  @ccall libflint.acb_hypgeom_u(z::Ref{ComplexFieldElem}, a::Ref{ComplexFieldElem}, b::Ref{ComplexFieldElem}, x::Ref{ComplexFieldElem}, prec::Int)::Nothing
  return z
end

@doc raw"""
    hypergeometric_2f1(a::ComplexFieldElem, b::ComplexFieldElem, c::ComplexFieldElem, x::ComplexFieldElem; flags=0)

Return the Gauss hypergeometric function ${}_2F_1(a,b,c,x)$.
"""
function hypergeometric_2f1(a::ComplexFieldElem, b::ComplexFieldElem, c::ComplexFieldElem, x::ComplexFieldElem, prec::Int = precision(Balls); flags=0)
  z = ComplexFieldElem()
  @ccall libflint.acb_hypgeom_2f1(z::Ref{ComplexFieldElem}, a::Ref{ComplexFieldElem}, b::Ref{ComplexFieldElem}, c::Ref{ComplexFieldElem}, x::Ref{ComplexFieldElem}, flags::Int, prec::Int)::Nothing
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
  @ccall libflint.acb_modular_theta(t1::Ref{ComplexFieldElem}, t2::Ref{ComplexFieldElem}, t3::Ref{ComplexFieldElem}, t4::Ref{ComplexFieldElem}, z::Ref{ComplexFieldElem}, tau::Ref{ComplexFieldElem}, prec::Int)::Nothing
  return (t1, t2, t3, t4)
end

@doc raw"""
    weierstrass_p(z::ComplexFieldElem, tau::ComplexFieldElem)

Return the Weierstrass elliptic function $\wp(z,\tau)$.
"""
function weierstrass_p(z::ComplexFieldElem, tau::ComplexFieldElem, prec::Int = precision(Balls))
  r = parent(z)()
  @ccall libflint.acb_elliptic_p(r::Ref{ComplexFieldElem}, z::Ref{ComplexFieldElem}, tau::Ref{ComplexFieldElem}, prec::Int)::Nothing
  return r
end

@doc raw"""
    weierstrass_p_prime(z::ComplexFieldElem, tau::ComplexFieldElem)

Return the derivative of the Weierstrass elliptic function $\frac{\partial}{\partial z}\wp(z,\tau)$.
"""
function weierstrass_p_prime(z::ComplexFieldElem, tau::ComplexFieldElem, prec::Int = precision(Balls))
  r = parent(z)()
  @ccall libflint.acb_elliptic_p_prime(r::Ref{ComplexFieldElem}, z::Ref{ComplexFieldElem}, tau::Ref{ComplexFieldElem}, prec::Int)::Nothing
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

function zero!(z::ComplexFieldElemOrPtr)
  @ccall libflint.acb_zero(z::Ref{ComplexFieldElem})::Nothing
  return z
end

function one!(z::ComplexFieldElemOrPtr)
  @ccall libflint.acb_one(z::Ref{ComplexFieldElem})::Nothing
  return z
end

function onei!(z::ComplexFieldElemOrPtr)
  @ccall libflint.acb_onei(z::Ref{ComplexFieldElem})::Nothing
  return z
end

function neg!(z::ComplexFieldElemOrPtr, a::ComplexFieldElemOrPtr)
  @ccall libflint.acb_neg(z::Ref{ComplexFieldElem}, a::Ref{ComplexFieldElem})::Nothing
  return z
end

function add!(z::ComplexFieldElem, x::ComplexFieldElem, y::ComplexFieldElem, prec::Int = precision(Balls))
  @ccall libflint.acb_add(z::Ref{ComplexFieldElem}, x::Ref{ComplexFieldElem}, y::Ref{ComplexFieldElem}, prec::Int)::Nothing
  return z
end

function sub!(z::ComplexFieldElem, x::ComplexFieldElem, y::ComplexFieldElem, prec::Int = precision(Balls))
  @ccall libflint.acb_sub(z::Ref{ComplexFieldElem}, x::Ref{ComplexFieldElem}, y::Ref{ComplexFieldElem}, prec::Int)::Nothing
  return z
end

function mul!(z::ComplexFieldElem, x::ComplexFieldElem, y::ComplexFieldElem, prec::Int = precision(Balls))
  @ccall libflint.acb_mul(z::Ref{ComplexFieldElem}, x::Ref{ComplexFieldElem}, y::Ref{ComplexFieldElem}, prec::Int)::Nothing
  return z
end

function div!(z::ComplexFieldElem, x::ComplexFieldElem, y::ComplexFieldElem, prec::Int = precision(Balls))
  @ccall libflint.acb_div(z::Ref{ComplexFieldElem}, x::Ref{ComplexFieldElem}, y::Ref{ComplexFieldElem}, prec::Int)::Nothing
  return z
end

################################################################################
#
#  Unsafe setting
#
################################################################################

_real_ptr(x::ComplexFieldElemOrPtr) = @ccall libflint.acb_real_ptr(x::Ref{ComplexFieldElem})::Ptr{RealFieldElem}
_imag_ptr(x::ComplexFieldElemOrPtr) = @ccall libflint.acb_imag_ptr(x::Ref{ComplexFieldElem})::Ptr{RealFieldElem}

function _acb_set(x::ComplexFieldElemOrPtr, y::Int)
  @ccall libflint.acb_set_si(x::Ref{ComplexFieldElem}, y::Int)::Nothing
end

function _acb_set(x::ComplexFieldElemOrPtr, y::UInt)
  @ccall libflint.acb_set_ui(x::Ref{ComplexFieldElem}, y::UInt)::Nothing
end

function _acb_set(x::ComplexFieldElemOrPtr, y::Float64)
  @ccall libflint.acb_set_d(x::Ref{ComplexFieldElem}, y::Float64)::Nothing
end

function _acb_set(x::ComplexFieldElemOrPtr, y::Union{Int,UInt,Float64}, p::Int)
  _acb_set(x, y)
  @ccall libflint.acb_set_round(x::Ref{ComplexFieldElem}, x::Ref{ComplexFieldElem}, p::Int)::Nothing
end

function _acb_set(x::ComplexFieldElemOrPtr, y::ZZRingElem)
  @ccall libflint.acb_set_fmpz(x::Ref{ComplexFieldElem}, y::Ref{ZZRingElem})::Nothing
end

function _acb_set(x::ComplexFieldElemOrPtr, y::ZZRingElem, p::Int)
  @ccall libflint.acb_set_round_fmpz(x::Ref{ComplexFieldElem}, y::Ref{ZZRingElem}, p::Int)::Nothing
end

function _acb_set(x::ComplexFieldElemOrPtr, y::QQFieldElem, p::Int)
  @ccall libflint.acb_set_fmpq(x::Ref{ComplexFieldElem}, y::Ref{QQFieldElem}, p::Int)::Nothing
end

function _acb_set(x::ComplexFieldElemOrPtr, y::RealFieldElem)
  @ccall libflint.acb_set_arb(x::Ref{ComplexFieldElem}, y::Ref{RealFieldElem})::Nothing
end

function _acb_set(x::ComplexFieldElemOrPtr, y::RealFieldElem, p::Int)
  _acb_set(x, y)
  @ccall libflint.acb_set_round(x::Ref{ComplexFieldElem}, x::Ref{ComplexFieldElem}, p::Int)::Nothing
end

function _acb_set(x::ComplexFieldElemOrPtr, y::ComplexFieldElemOrPtr)
  @ccall libflint.acb_set(x::Ref{ComplexFieldElem}, y::Ref{ComplexFieldElem})::Nothing
end

function _acb_set(x::ComplexFieldElemOrPtr, y::Ptr{acb_struct})
  @ccall libflint.acb_set(x::Ref{ComplexFieldElem}, y::Ptr{acb_struct})::Nothing
end

function _acb_set(x::Ptr{acb_struct}, y::ComplexFieldElemOrPtr)
  @ccall libflint.acb_set(x::Ptr{acb_struct}, y::Ref{ComplexFieldElem})::Nothing
end

function _acb_set(x::ComplexFieldElemOrPtr, y::ComplexFieldElemOrPtr, p::Int)
  @ccall libflint.acb_set_round(x::Ref{ComplexFieldElem}, y::Ref{ComplexFieldElem}, p::Int)::Nothing
end

function _acb_set(x::ComplexFieldElemOrPtr, y::AbstractString, p::Int)
  r = _real_ptr(x)
  _arb_set(r, y, p)
  i = _imag_ptr(x)
  zero!(i)
end

function _acb_set(x::ComplexFieldElemOrPtr, y::BigFloat)
  r = _real_ptr(x)
  _arb_set(r, y)
  i = _imag_ptr(x)
  zero!(i)
end

function _acb_set(x::ComplexFieldElemOrPtr, y::BigFloat, p::Int)
  r = _real_ptr(x)
  _arb_set(r, y, p)
  i = _imag_ptr(x)
  zero!(i)
end

function _acb_set(x::ComplexFieldElemOrPtr, yz::Tuple{Int,Int}, p::Int)
  @ccall libflint.acb_set_si_si(x::Ref{ComplexFieldElem}, yz[1]::Int, yz[2]::Int)::Nothing
  @ccall libflint.acb_set_round(x::Ref{ComplexFieldElem}, x::Ref{ComplexFieldElem}, p::Int)::Nothing
end

function _acb_set(x::ComplexFieldElemOrPtr, yz::Tuple{RealFieldElem,RealFieldElem})
  @ccall libflint.acb_set_arb_arb(x::Ref{ComplexFieldElem}, yz[1]::Ref{RealFieldElem}, yz[2]::Ref{RealFieldElem})::Nothing
end

function _acb_set(x::ComplexFieldElemOrPtr, yz::Tuple{RealFieldElem,RealFieldElem}, p::Int)
  _acb_set(x, yz)
  @ccall libflint.acb_set_round(x::Ref{ComplexFieldElem}, x::Ref{ComplexFieldElem}, p::Int)::Nothing
end

function _acb_set(x::ComplexFieldElemOrPtr, yz::Tuple{QQFieldElem,QQFieldElem}, p::Int)
  r = _real_ptr(x)
  _arb_set(r, yz[1], p)
  i = _imag_ptr(x)
  _arb_set(i, yz[2], p)
end

function _acb_set(x::ComplexFieldElemOrPtr, yz::Tuple{AbstractString,AbstractString}, p::Int)
  r = _real_ptr(x)
  _arb_set(r, yz[1], p)
  i = _imag_ptr(x)
  _arb_set(i, yz[2], p)
end

function _acb_set(x::ComplexFieldElemOrPtr, y::Real, p::Int)
  r = _real_ptr(x)
  _arb_set(r, y, p)
  i = _imag_ptr(x)
  zero!(i)
end

function _acb_set(x::ComplexFieldElemOrPtr, y::Complex, p::Int)
  r = _real_ptr(x)
  _arb_set(r, real(y), p)
  i = _imag_ptr(x)
  _arb_set(i, imag(y), p)
end

function _acb_set(x::ComplexFieldElemOrPtr, yz::Tuple{IntegerUnion,IntegerUnion}, p::Int)
  r = _real_ptr(x)
  _arb_set(r, yz[1], p)
  i = _imag_ptr(x)
  _arb_set(i, yz[2], p)
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
