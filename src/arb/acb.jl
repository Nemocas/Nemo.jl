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
  one!(z)
  z.parent = r
  return z
end

@doc raw"""
    onei(r::AcbField)

Return exact one times $i$ in the given Arb complex field.
"""
function onei(r::AcbField)
  z = AcbFieldElem()
  onei!(z)
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
  return -@ccall libflint.acb_rel_error_bits(x::Ref{AcbFieldElem})::Int
end

function deepcopy_internal(a::AcbFieldElem, dict::IdDict)
  b = parent(a)()
  _acb_set(b, a)
  return b
end

function canonical_unit(x::AcbFieldElem)
  return x
end

characteristic(::AcbField) = 0

################################################################################
#
#  Conversions
#
################################################################################

@doc raw"""
    Float64(x::AcbFieldElem)

Converts $x$ to a `Float64`, rounded to the nearest.
The return value approximates the midpoint of $x$.
"""
function Base.Float64(x::AcbFieldElem)
  @req isreal(x) "conversion to float must have no imaginary part"
  GC.@preserve x begin
    re = _real_ptr(x)
    t = _mid_ptr(re)
    v = @ccall libflint.arf_get_d(t::Ptr{arf_struct}, ARB_RND_NEAR::Int)::Float64
  end
  return v
end

@doc raw"""
    ComplexF64(x::AcbFieldElem)

Converts $x$ to a `ComplexF64`, rounded to the nearest.
The return value approximates the midpoint of the real and imaginary parts of $x$.
"""
function Base.ComplexF64(x::AcbFieldElem)
  GC.@preserve x begin
    re = _real_ptr(x)
    im = _imag_ptr(x)
    t = _mid_ptr(re)
    u = _mid_ptr(im)
    v = @ccall libflint.arf_get_d(t::Ptr{arf_struct}, ARB_RND_NEAR::Int)::Float64
    w = @ccall libflint.arf_get_d(u::Ptr{arf_struct}, ARB_RND_NEAR::Int)::Float64
  end
  return complex(v, w)
end

function convert(::Type{ComplexF64}, x::AcbFieldElem)
  return ComplexF64(x)
end

function convert(::Type{Float64}, x::AcbFieldElem)
  return Float64(x)
end

################################################################################
#
#  Real and imaginary part
#
################################################################################

function real(x::AcbFieldElem)
  z = ArbFieldElem()
  @ccall libflint.acb_get_real(z::Ref{ArbFieldElem}, x::Ref{AcbFieldElem})::Nothing
  z.parent = ArbField(parent(x).prec)
  return z
end

function imag(x::AcbFieldElem)
  z = ArbFieldElem()
  @ccall libflint.acb_get_imag(z::Ref{ArbFieldElem}, x::Ref{AcbFieldElem})::Nothing
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

-(x::AcbFieldElem) = neg!(parent(x)(), x)

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
      @ccall libflint.$f(z::Ref{AcbFieldElem}, x::Ref{AcbFieldElem}, y::Ref{AcbFieldElem}, parent(x).prec::Int)::Nothing
      return z
    end
  end
end

for (f,s) in ((:+, "add"), (:-, "sub"), (:*, "mul"), (://, "div"), (:^, "pow"))
  @eval begin

    function ($f)(x::AcbFieldElem, y::UInt)
      z = parent(x)()
      @ccall libflint.$("acb_$(s)_ui")(z::Ref{AcbFieldElem}, x::Ref{AcbFieldElem}, y::UInt, parent(x).prec::Int)::Nothing
      return z
    end

    function ($f)(x::AcbFieldElem, y::Int)
      z = parent(x)()
      @ccall libflint.$("acb_$(s)_si")(z::Ref{AcbFieldElem}, x::Ref{AcbFieldElem}, y::Int, parent(x).prec::Int)::Nothing
      return z
    end

    function ($f)(x::AcbFieldElem, y::ZZRingElem)
      z = parent(x)()
      @ccall libflint.$("acb_$(s)_fmpz")(z::Ref{AcbFieldElem}, x::Ref{AcbFieldElem}, y::Ref{ZZRingElem}, parent(x).prec::Int)::Nothing
      return z
    end

    function ($f)(x::AcbFieldElem, y::ArbFieldElem)
      z = parent(x)()
      @ccall libflint.$("acb_$(s)_arb")(z::Ref{AcbFieldElem}, x::Ref{AcbFieldElem}, y::Ref{ArbFieldElem}, parent(x).prec::Int)::Nothing
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
  @ccall libflint.acb_sub_ui(z::Ref{AcbFieldElem}, y::Ref{AcbFieldElem}, x::UInt, parent(y).prec::Int)::Nothing
  return neg!(z)
end

function -(x::Int, y::AcbFieldElem)
  z = parent(y)()
  @ccall libflint.acb_sub_si(z::Ref{AcbFieldElem}, y::Ref{AcbFieldElem}, x::Int, parent(y).prec::Int)::Nothing
  return neg!(z)
end

function -(x::ZZRingElem, y::AcbFieldElem)
  z = parent(y)()
  @ccall libflint.acb_sub_fmpz(z::Ref{AcbFieldElem}, y::Ref{AcbFieldElem}, x::Ref{ZZRingElem}, parent(y).prec::Int)::Nothing
  return neg!(z)
end

function -(x::ArbFieldElem, y::AcbFieldElem)
  z = parent(y)()
  @ccall libflint.acb_sub_arb(z::Ref{AcbFieldElem}, y::Ref{AcbFieldElem}, x::Ref{ArbFieldElem}, parent(y).prec::Int)::Nothing
  return neg!(z)
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
  r = @ccall libflint.acb_equal(x::Ref{AcbFieldElem}, y::Ref{AcbFieldElem})::Cint
  return Bool(r)
end

function Base.hash(x::AcbFieldElem, h::UInt)
  # TODO: improve me
  return h
end

function ==(x::AcbFieldElem, y::AcbFieldElem)
  r = @ccall libflint.acb_eq(x::Ref{AcbFieldElem}, y::Ref{AcbFieldElem})::Cint
  return Bool(r)
end

function !=(x::AcbFieldElem, y::AcbFieldElem)
  r = @ccall libflint.acb_ne(x::Ref{AcbFieldElem}, y::Ref{AcbFieldElem})::Cint
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
  r = @ccall libflint.acb_overlaps(x::Ref{AcbFieldElem}, y::Ref{AcbFieldElem})::Cint
  return Bool(r)
end

@doc raw"""
    contains(x::AcbFieldElem, y::AcbFieldElem)

Returns `true` if the box $x$ contains the box $y$, otherwise return
`false`.
"""
function contains(x::AcbFieldElem, y::AcbFieldElem)
  r = @ccall libflint.acb_contains(x::Ref{AcbFieldElem}, y::Ref{AcbFieldElem})::Cint
  return Bool(r)
end

@doc raw"""
    contains(x::AcbFieldElem, y::QQFieldElem)

Returns `true` if the box $x$ contains the given rational value, otherwise
return `false`.
"""
function contains(x::AcbFieldElem, y::QQFieldElem)
  r = @ccall libflint.acb_contains_fmpq(x::Ref{AcbFieldElem}, y::Ref{QQFieldElem})::Cint
  return Bool(r)
end

@doc raw"""
    contains(x::AcbFieldElem, y::ZZRingElem)

Returns `true` if the box $x$ contains the given integer value, otherwise
return `false`.
"""
function contains(x::AcbFieldElem, y::ZZRingElem)
  r = @ccall libflint.acb_contains_fmpz(x::Ref{AcbFieldElem}, y::Ref{ZZRingElem})::Cint
  return Bool(r)
end

function contains(x::AcbFieldElem, y::Int)
  v = ZZRingElem(y)
  r = @ccall libflint.acb_contains_fmpz(x::Ref{AcbFieldElem}, v::Ref{ZZRingElem})::Cint
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
  return Bool(@ccall libflint.acb_contains_zero(x::Ref{AcbFieldElem})::Cint)
end

################################################################################
#
#  Predicates
#
################################################################################

@doc raw"""
    iszero(x::AcbFieldElem)

Return `true` if $x$ is certainly zero, otherwise return `false`.
"""
function iszero(x::AcbFieldElem)
  return Bool(@ccall libflint.acb_is_zero(x::Ref{AcbFieldElem})::Cint)
end

@doc raw"""
    isone(x::AcbFieldElem)

Return `true` if $x$ is certainly one, otherwise return `false`.
"""
function isone(x::AcbFieldElem)
  return Bool(@ccall libflint.acb_is_one(x::Ref{AcbFieldElem})::Cint)
end

@doc raw"""
    isfinite(x::AcbFieldElem)

Return `true` if $x$ is finite, i.e. its real and imaginary parts have finite
midpoint and radius, otherwise return `false`.
"""
function isfinite(x::AcbFieldElem)
  return Bool(@ccall libflint.acb_is_finite(x::Ref{AcbFieldElem})::Cint)
end

@doc raw"""
    is_exact(x::AcbFieldElem)

Return `true` if $x$ is exact, i.e. has its real and imaginary parts have
zero radius, otherwise return `false`.
"""
function is_exact(x::AcbFieldElem)
  return Bool(@ccall libflint.acb_is_exact(x::Ref{AcbFieldElem})::Cint)
end

@doc raw"""
    isinteger(x::AcbFieldElem)

Return `true` if $x$ is an exact integer, otherwise return `false`.
"""
function isinteger(x::AcbFieldElem)
  return Bool(@ccall libflint.acb_is_int(x::Ref{AcbFieldElem})::Cint)
end

function isreal(x::AcbFieldElem)
  return Bool(@ccall libflint.acb_is_real(x::Ref{AcbFieldElem})::Cint)
end

is_negative(x::AcbFieldElem) = isreal(x) && is_negative(real(x))

################################################################################
#
#  Absolute value
#
################################################################################

function abs(x::AcbFieldElem)
  z = ArbFieldElem()
  @ccall libflint.acb_abs(z::Ref{ArbFieldElem}, x::Ref{AcbFieldElem}, parent(x).prec::Int)::Nothing
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
  @ccall libflint.acb_inv(z::Ref{AcbFieldElem}, x::Ref{AcbFieldElem}, parent(x).prec::Int)::Nothing
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
  @ccall libflint.acb_mul_2exp_si(z::Ref{AcbFieldElem}, x::Ref{AcbFieldElem}, y::Int)::Nothing
  return z
end

function ldexp(x::AcbFieldElem, y::ZZRingElem)
  z = parent(x)()
  @ccall libflint.acb_mul_2exp_fmpz(z::Ref{AcbFieldElem}, x::Ref{AcbFieldElem}, y::Ref{ZZRingElem})::Nothing
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
  @ccall libflint.acb_trim(z::Ref{AcbFieldElem}, x::Ref{AcbFieldElem})::Nothing
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
  unique = @ccall libflint.acb_get_unique_fmpz(z::Ref{ZZRingElem}, x::Ref{AcbFieldElem})::Int
  return (unique != 0, z)
end

function conj(x::AcbFieldElem)
  z = parent(x)()
  @ccall libflint.acb_conj(z::Ref{AcbFieldElem}, x::Ref{AcbFieldElem})::Nothing
  return z
end

function angle(x::AcbFieldElem)
  z = ArbFieldElem()
  @ccall libflint.acb_arg(z::Ref{ArbFieldElem}, x::Ref{AcbFieldElem}, parent(x).prec::Int)::Nothing
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
  @ccall libflint.acb_const_pi(z::Ref{AcbFieldElem}, precision(r)::Int)::Nothing
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
  @ccall libflint.acb_sqrt(z::Ref{AcbFieldElem}, x::Ref{AcbFieldElem}, parent(x).prec::Int)::Nothing
  return z
end

@doc raw"""
    rsqrt(x::AcbFieldElem)

Return the reciprocal of the square root of $x$, i.e. $1/\sqrt{x}$.
"""
function rsqrt(x::AcbFieldElem)
  z = parent(x)()
  @ccall libflint.acb_rsqrt(z::Ref{AcbFieldElem}, x::Ref{AcbFieldElem}, parent(x).prec::Int)::Nothing
  return z
end

function log(x::AcbFieldElem)
  z = parent(x)()
  @ccall libflint.acb_log(z::Ref{AcbFieldElem}, x::Ref{AcbFieldElem}, parent(x).prec::Int)::Nothing
  return z
end

function log1p(x::AcbFieldElem)
  z = parent(x)()
  @ccall libflint.acb_log1p(z::Ref{AcbFieldElem}, x::Ref{AcbFieldElem}, parent(x).prec::Int)::Nothing
  return z
end

function Base.exp(x::AcbFieldElem)
  z = parent(x)()
  @ccall libflint.acb_exp(z::Ref{AcbFieldElem}, x::Ref{AcbFieldElem}, parent(x).prec::Int)::Nothing
  return z
end

function Base.expm1(x::AcbFieldElem)
  z = parent(x)()
  @ccall libflint.acb_expm1(z::Ref{AcbFieldElem}, x::Ref{AcbFieldElem}, parent(x).prec::Int)::Nothing
  return z
end

@doc raw"""
    cispi(x::AcbFieldElem)

Return the exponential of $\pi i x$.
"""
function cispi(x::AcbFieldElem)
  z = parent(x)()
  @ccall libflint.acb_exp_pi_i(z::Ref{AcbFieldElem}, x::Ref{AcbFieldElem}, parent(x).prec::Int)::Nothing
  return z
end

@doc raw"""
    root_of_unity(C::AcbField, k::Int)

Return $\exp(2\pi i/k)$.
"""
function root_of_unity(C::AcbField, k::Int)
  k <= 0 && throw(ArgumentError("Order must be positive ($k)"))
  z = C()
  @ccall libflint.acb_unit_root(z::Ref{AcbFieldElem}, k::UInt, C.prec::Int)::Nothing
  return z
end

function sin(x::AcbFieldElem)
  z = parent(x)()
  @ccall libflint.acb_sin(z::Ref{AcbFieldElem}, x::Ref{AcbFieldElem}, parent(x).prec::Int)::Nothing
  return z
end

function cos(x::AcbFieldElem)
  z = parent(x)()
  @ccall libflint.acb_cos(z::Ref{AcbFieldElem}, x::Ref{AcbFieldElem}, parent(x).prec::Int)::Nothing
  return z
end

function tan(x::AcbFieldElem)
  z = parent(x)()
  @ccall libflint.acb_tan(z::Ref{AcbFieldElem}, x::Ref{AcbFieldElem}, parent(x).prec::Int)::Nothing
  return z
end

function cot(x::AcbFieldElem)
  z = parent(x)()
  @ccall libflint.acb_cot(z::Ref{AcbFieldElem}, x::Ref{AcbFieldElem}, parent(x).prec::Int)::Nothing
  return z
end

function sinpi(x::AcbFieldElem)
  z = parent(x)()
  @ccall libflint.acb_sin_pi(z::Ref{AcbFieldElem}, x::Ref{AcbFieldElem}, parent(x).prec::Int)::Nothing
  return z
end

function cospi(x::AcbFieldElem)
  z = parent(x)()
  @ccall libflint.acb_cos_pi(z::Ref{AcbFieldElem}, x::Ref{AcbFieldElem}, parent(x).prec::Int)::Nothing
  return z
end

function tanpi(x::AcbFieldElem)
  z = parent(x)()
  @ccall libflint.acb_tan_pi(z::Ref{AcbFieldElem}, x::Ref{AcbFieldElem}, parent(x).prec::Int)::Nothing
  return z
end

function cotpi(x::AcbFieldElem)
  z = parent(x)()
  @ccall libflint.acb_cot_pi(z::Ref{AcbFieldElem}, x::Ref{AcbFieldElem}, parent(x).prec::Int)::Nothing
  return z
end

function sinh(x::AcbFieldElem)
  z = parent(x)()
  @ccall libflint.acb_sinh(z::Ref{AcbFieldElem}, x::Ref{AcbFieldElem}, parent(x).prec::Int)::Nothing
  return z
end

function cosh(x::AcbFieldElem)
  z = parent(x)()
  @ccall libflint.acb_cosh(z::Ref{AcbFieldElem}, x::Ref{AcbFieldElem}, parent(x).prec::Int)::Nothing
  return z
end

function tanh(x::AcbFieldElem)
  z = parent(x)()
  @ccall libflint.acb_tanh(z::Ref{AcbFieldElem}, x::Ref{AcbFieldElem}, parent(x).prec::Int)::Nothing
  return z
end

function coth(x::AcbFieldElem)
  z = parent(x)()
  @ccall libflint.acb_coth(z::Ref{AcbFieldElem}, x::Ref{AcbFieldElem}, parent(x).prec::Int)::Nothing
  return z
end

function atan(x::AcbFieldElem)
  z = parent(x)()
  @ccall libflint.acb_atan(z::Ref{AcbFieldElem}, x::Ref{AcbFieldElem}, parent(x).prec::Int)::Nothing
  return z
end

@doc raw"""
    log_sinpi(x::AcbFieldElem)

Return $\log\sin(\pi x)$, constructed without branch cuts off the real line.
"""
function log_sinpi(x::AcbFieldElem)
  z = parent(x)()
  @ccall libflint.acb_log_sin_pi(z::Ref{AcbFieldElem}, x::Ref{AcbFieldElem}, parent(x).prec::Int)::Nothing
  return z
end

@doc raw"""
    gamma(x::AcbFieldElem)

Return the Gamma function evaluated at $x$.
"""
function gamma(x::AcbFieldElem)
  z = parent(x)()
  @ccall libflint.acb_gamma(z::Ref{AcbFieldElem}, x::Ref{AcbFieldElem}, parent(x).prec::Int)::Nothing
  return z
end

@doc raw"""
    rgamma(x::AcbFieldElem)

Return the reciprocal of the Gamma function evaluated at $x$.
"""
function rgamma(x::AcbFieldElem)
  z = parent(x)()
  @ccall libflint.acb_rgamma(z::Ref{AcbFieldElem}, x::Ref{AcbFieldElem}, parent(x).prec::Int)::Nothing
  return z
end

@doc raw"""
    lgamma(x::AcbFieldElem)

Return the logarithm of the Gamma function evaluated at $x$.
"""
function lgamma(x::AcbFieldElem)
  z = parent(x)()
  @ccall libflint.acb_lgamma(z::Ref{AcbFieldElem}, x::Ref{AcbFieldElem}, parent(x).prec::Int)::Nothing
  return z
end

@doc raw"""
    digamma(x::AcbFieldElem)

Return the  logarithmic derivative of the gamma function evaluated at $x$,
i.e. $\psi(x)$.
"""
function digamma(x::AcbFieldElem)
  z = parent(x)()
  @ccall libflint.acb_digamma(z::Ref{AcbFieldElem}, x::Ref{AcbFieldElem}, parent(x).prec::Int)::Nothing
  return z
end

@doc raw"""
    zeta(x::AcbFieldElem)

Return the Riemann zeta function evaluated at $x$.
"""
function zeta(x::AcbFieldElem)
  z = parent(x)()
  @ccall libflint.acb_zeta(z::Ref{AcbFieldElem}, x::Ref{AcbFieldElem}, parent(x).prec::Int)::Nothing
  return z
end

@doc raw"""
    barnes_g(x::AcbFieldElem)

Return the Barnes $G$-function, evaluated at $x$.
"""
function barnes_g(x::AcbFieldElem)
  z = parent(x)()
  @ccall libflint.acb_barnes_g(z::Ref{AcbFieldElem}, x::Ref{AcbFieldElem}, parent(x).prec::Int)::Nothing
  return z
end

@doc raw"""
    log_barnes_g(x::AcbFieldElem)

Return the logarithm of the Barnes $G$-function, evaluated at $x$.
"""
function log_barnes_g(x::AcbFieldElem)
  z = parent(x)()
  @ccall libflint.acb_log_barnes_g(z::Ref{AcbFieldElem}, x::Ref{AcbFieldElem}, parent(x).prec::Int)::Nothing
  return z
end

@doc raw"""
    agm(x::AcbFieldElem)

Return the arithmetic-geometric mean of $1$ and $x$.
"""
function agm(x::AcbFieldElem)
  z = parent(x)()
  @ccall libflint.acb_agm1(z::Ref{AcbFieldElem}, x::Ref{AcbFieldElem}, parent(x).prec::Int)::Nothing
  return z
end

@doc raw"""
    erf(x::AcbFieldElem)

Return the error function evaluated at $x$.
"""
function erf(x::AcbFieldElem)
  z = parent(x)()
  @ccall libflint.acb_hypgeom_erf(z::Ref{AcbFieldElem}, x::Ref{AcbFieldElem}, parent(x).prec::Int)::Nothing
  return z
end

@doc raw"""
    erfi(x::AcbFieldElem)

Return the imaginary error function evaluated at $x$.
"""
function erfi(x::AcbFieldElem)
  z = parent(x)()
  @ccall libflint.acb_hypgeom_erfi(z::Ref{AcbFieldElem}, x::Ref{AcbFieldElem}, parent(x).prec::Int)::Nothing
  return z
end

@doc raw"""
    erfc(x::AcbFieldElem)

Return the complementary error function evaluated at $x$.
"""
function erfc(x::AcbFieldElem)
  z = parent(x)()
  @ccall libflint.acb_hypgeom_erfc(z::Ref{AcbFieldElem}, x::Ref{AcbFieldElem}, parent(x).prec::Int)::Nothing
  return z
end

@doc raw"""
    exp_integral_ei(x::AcbFieldElem)

Return the exponential integral evaluated at $x$.
"""
function exp_integral_ei(x::AcbFieldElem)
  z = parent(x)()
  @ccall libflint.acb_hypgeom_ei(z::Ref{AcbFieldElem}, x::Ref{AcbFieldElem}, parent(x).prec::Int)::Nothing
  return z
end

@doc raw"""
    sin_integral(x::AcbFieldElem)

Return the sine integral evaluated at $x$.
"""
function sin_integral(x::AcbFieldElem)
  z = parent(x)()
  @ccall libflint.acb_hypgeom_si(z::Ref{AcbFieldElem}, x::Ref{AcbFieldElem}, parent(x).prec::Int)::Nothing
  return z
end

@doc raw"""
    cos_integral(x::AcbFieldElem)

Return the exponential cosine integral evaluated at $x$.
"""
function cos_integral(x::AcbFieldElem)
  z = parent(x)()
  @ccall libflint.acb_hypgeom_ci(z::Ref{AcbFieldElem}, x::Ref{AcbFieldElem}, parent(x).prec::Int)::Nothing
  return z
end

@doc raw"""
    sinh_integral(x::AcbFieldElem)

Return the hyperbolic sine integral evaluated at $x$.
"""
function sinh_integral(x::AcbFieldElem)
  z = parent(x)()
  @ccall libflint.acb_hypgeom_shi(z::Ref{AcbFieldElem}, x::Ref{AcbFieldElem}, parent(x).prec::Int)::Nothing
  return z
end

@doc raw"""
    cosh_integral(x::AcbFieldElem)

Return the hyperbolic cosine integral evaluated at $x$.
"""
function cosh_integral(x::AcbFieldElem)
  z = parent(x)()
  @ccall libflint.acb_hypgeom_chi(z::Ref{AcbFieldElem}, x::Ref{AcbFieldElem}, parent(x).prec::Int)::Nothing
  return z
end

@doc raw"""
    dedekind_eta(x::AcbFieldElem)

Return the Dedekind eta function $\eta(\tau)$ at $\tau = x$.
"""
function dedekind_eta(x::AcbFieldElem)
  z = parent(x)()
  @ccall libflint.acb_modular_eta(z::Ref{AcbFieldElem}, x::Ref{AcbFieldElem}, parent(x).prec::Int)::Nothing
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
  @ccall libflint.acb_modular_j(z::Ref{AcbFieldElem}, x::Ref{AcbFieldElem}, parent(x).prec::Int)::Nothing
  return z
end

@doc raw"""
    modular_lambda(x::AcbFieldElem)

Return the modular lambda function $\lambda(\tau)$ at $\tau = x$.
"""
function modular_lambda(x::AcbFieldElem)
  z = parent(x)()
  @ccall libflint.acb_modular_lambda(z::Ref{AcbFieldElem}, x::Ref{AcbFieldElem}, parent(x).prec::Int)::Nothing
  return z
end

@doc raw"""
    modular_delta(x::AcbFieldElem)

Return the modular delta function $\Delta(\tau)$ at $\tau = x$.
"""
function modular_delta(x::AcbFieldElem)
  z = parent(x)()
  @ccall libflint.acb_modular_delta(z::Ref{AcbFieldElem}, x::Ref{AcbFieldElem}, parent(x).prec::Int)::Nothing
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
  @ccall libflint.acb_modular_eisenstein(vec::Ptr{acb_struct}, x::Ref{AcbFieldElem}, len::Int, CC.prec::Int)::Nothing
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
  @ccall libflint.acb_modular_elliptic_k(z::Ref{AcbFieldElem}, x::Ref{AcbFieldElem}, parent(x).prec::Int)::Nothing
  return z
end

@doc raw"""
    elliptic_e(x::AcbFieldElem)

Return the complete elliptic integral $E(x)$.
"""
function elliptic_e(x::AcbFieldElem)
  z = parent(x)()
  @ccall libflint.acb_modular_elliptic_e(z::Ref{AcbFieldElem}, x::Ref{AcbFieldElem}, parent(x).prec::Int)::Nothing
  return z
end

function sincos(x::AcbFieldElem)
  s = parent(x)()
  c = parent(x)()
  @ccall libflint.acb_sin_cos(s::Ref{AcbFieldElem}, c::Ref{AcbFieldElem}, x::Ref{AcbFieldElem}, parent(x).prec::Int)::Nothing
  return (s, c)
end

function sincospi(x::AcbFieldElem)
  s = parent(x)()
  c = parent(x)()
  @ccall libflint.acb_sin_cos_pi(s::Ref{AcbFieldElem}, c::Ref{AcbFieldElem}, x::Ref{AcbFieldElem}, parent(x).prec::Int)::Nothing
  return (s, c)
end

@doc raw"""
    sinhcosh(x::AcbFieldElem)

Return a tuple $s, c$ consisting of the hyperbolic sine and cosine of $x$.
"""
function sinhcosh(x::AcbFieldElem)
  s = parent(x)()
  c = parent(x)()
  @ccall libflint.acb_sinh_cosh(s::Ref{AcbFieldElem}, c::Ref{AcbFieldElem}, x::Ref{AcbFieldElem}, parent(x).prec::Int)::Nothing
  return (s, c)
end

@doc raw"""
    zeta(s::AcbFieldElem, a::AcbFieldElem)

Return the Hurwitz zeta function $\zeta(s,a)$.
"""
function zeta(s::AcbFieldElem, a::AcbFieldElem)
  z = parent(s)()
  @ccall libflint.acb_hurwitz_zeta(z::Ref{AcbFieldElem}, s::Ref{AcbFieldElem}, a::Ref{AcbFieldElem}, parent(s).prec::Int)::Nothing
  return z
end

@doc raw"""
    polygamma(s::AcbFieldElem, a::AcbFieldElem)

Return the generalised polygamma function $\psi(s,z)$.
"""
function polygamma(s::AcbFieldElem, a::AcbFieldElem)
  z = parent(s)()
  @ccall libflint.acb_polygamma(z::Ref{AcbFieldElem}, s::Ref{AcbFieldElem}, a::Ref{AcbFieldElem}, parent(s).prec::Int)::Nothing
  return z
end

function rising_factorial(x::AcbFieldElem, n::UInt)
  z = parent(x)()
  @ccall libflint.acb_rising_ui(z::Ref{AcbFieldElem}, x::Ref{AcbFieldElem}, n::UInt, parent(x).prec::Int)::Nothing
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
  @ccall libflint.acb_rising2_ui(z::Ref{AcbFieldElem}, w::Ref{AcbFieldElem}, x::Ref{AcbFieldElem}, n::UInt, parent(x).prec::Int)::Nothing
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
  @ccall libflint.acb_polylog(z::Ref{AcbFieldElem}, s::Ref{AcbFieldElem}, a::Ref{AcbFieldElem}, parent(s).prec::Int)::Nothing
  return z
end

function polylog(s::Int, a::AcbFieldElem)
  z = parent(a)()
  @ccall libflint.acb_polylog_si(z::Ref{AcbFieldElem}, s::Int, a::Ref{AcbFieldElem}, parent(a).prec::Int)::Nothing
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
  @ccall libflint.acb_hypgeom_li(z::Ref{AcbFieldElem}, x::Ref{AcbFieldElem}, 0::Int, parent(x).prec::Int)::Nothing
  return z
end

@doc raw"""
    log_integral_offset(x::AcbFieldElem)

Return the offset logarithmic integral, evaluated at $x$.
"""
function log_integral_offset(x::AcbFieldElem)
  z = parent(x)()
  @ccall libflint.acb_hypgeom_li(z::Ref{AcbFieldElem}, x::Ref{AcbFieldElem}, 1::Int, parent(x).prec::Int)::Nothing
  return z
end

@doc raw"""
    exp_integral_e(s::AcbFieldElem, x::AcbFieldElem)

Return the generalised exponential integral $E_s(x)$.
"""
function exp_integral_e(s::AcbFieldElem, x::AcbFieldElem)
  z = parent(s)()
  @ccall libflint.acb_hypgeom_expint(z::Ref{AcbFieldElem}, s::Ref{AcbFieldElem}, x::Ref{AcbFieldElem}, parent(s).prec::Int)::Nothing
  return z
end

@doc raw"""
    gamma(s::AcbFieldElem, x::AcbFieldElem)

Return the upper incomplete gamma function $\Gamma(s,x)$.
"""
function gamma(s::AcbFieldElem, x::AcbFieldElem)
  z = parent(s)()
  @ccall libflint.acb_hypgeom_gamma_upper(z::Ref{AcbFieldElem}, s::Ref{AcbFieldElem}, x::Ref{AcbFieldElem}, 0::Int, parent(s).prec::Int)::Nothing
  return z
end

@doc raw"""
    gamma_regularized(s::AcbFieldElem, x::AcbFieldElem)

Return the regularized upper incomplete gamma function
$\Gamma(s,x) / \Gamma(s)$.
"""
function gamma_regularized(s::AcbFieldElem, x::AcbFieldElem)
  z = parent(s)()
  @ccall libflint.acb_hypgeom_gamma_upper(z::Ref{AcbFieldElem}, s::Ref{AcbFieldElem}, x::Ref{AcbFieldElem}, 1::Int, parent(s).prec::Int)::Nothing
  return z
end

@doc raw"""
    gamma_lower(s::AcbFieldElem, x::AcbFieldElem)

Return the lower incomplete gamma function $\gamma(s,x) / \Gamma(s)$.
"""
function gamma_lower(s::AcbFieldElem, x::AcbFieldElem)
  z = parent(s)()
  @ccall libflint.acb_hypgeom_gamma_lower(z::Ref{AcbFieldElem}, s::Ref{AcbFieldElem}, x::Ref{AcbFieldElem}, 0::Int, parent(s).prec::Int)::Nothing
  return z
end

@doc raw"""
    gamma_lower_regularized(s::AcbFieldElem, x::AcbFieldElem)

Return the regularized lower incomplete gamma function
$\gamma(s,x) / \Gamma(s)$.
"""
function gamma_lower_regularized(s::AcbFieldElem, x::AcbFieldElem)
  z = parent(s)()
  @ccall libflint.acb_hypgeom_gamma_lower(z::Ref{AcbFieldElem}, s::Ref{AcbFieldElem}, x::Ref{AcbFieldElem}, 1::Int, parent(s).prec::Int)::Nothing
  return z
end

@doc raw"""
    bessel_j(nu::AcbFieldElem, x::AcbFieldElem)

Return the Bessel function $J_{\nu}(x)$.
"""
function bessel_j(nu::AcbFieldElem, x::AcbFieldElem)
  z = parent(x)()
  @ccall libflint.acb_hypgeom_bessel_j(z::Ref{AcbFieldElem}, nu::Ref{AcbFieldElem}, x::Ref{AcbFieldElem}, parent(x).prec::Int)::Nothing
  return z
end

@doc raw"""
    bessel_y(nu::AcbFieldElem, x::AcbFieldElem)

Return the Bessel function $Y_{\nu}(x)$.
"""
function bessel_y(nu::AcbFieldElem, x::AcbFieldElem)
  z = parent(x)()
  @ccall libflint.acb_hypgeom_bessel_y(z::Ref{AcbFieldElem}, nu::Ref{AcbFieldElem}, x::Ref{AcbFieldElem}, parent(x).prec::Int)::Nothing
  return z
end

@doc raw"""
    bessel_i(nu::AcbFieldElem, x::AcbFieldElem)

Return the Bessel function $I_{\nu}(x)$.
"""
function bessel_i(nu::AcbFieldElem, x::AcbFieldElem)
  z = parent(x)()
  @ccall libflint.acb_hypgeom_bessel_i(z::Ref{AcbFieldElem}, nu::Ref{AcbFieldElem}, x::Ref{AcbFieldElem}, parent(x).prec::Int)::Nothing
  return z
end

@doc raw"""
    bessel_k(nu::AcbFieldElem, x::AcbFieldElem)

Return the Bessel function $K_{\nu}(x)$.
"""
function bessel_k(nu::AcbFieldElem, x::AcbFieldElem)
  z = parent(x)()
  @ccall libflint.acb_hypgeom_bessel_k(z::Ref{AcbFieldElem}, nu::Ref{AcbFieldElem}, x::Ref{AcbFieldElem}, parent(x).prec::Int)::Nothing
  return z
end

@doc raw"""
    airy_ai(x::AcbFieldElem)

Return the Airy function $\operatorname{Ai}(x)$.
"""
function airy_ai(x::AcbFieldElem)
  ai = parent(x)()
  @ccall libflint.acb_hypgeom_airy(ai::Ref{AcbFieldElem}, C_NULL::Ptr{Cvoid}, C_NULL::Ptr{Cvoid}, C_NULL::Ptr{Cvoid}, x::Ref{AcbFieldElem}, parent(x).prec::Int)::Nothing
  return ai
end

@doc raw"""
    airy_bi(x::AcbFieldElem)

Return the Airy function $\operatorname{Bi}(x)$.
"""
function airy_bi(x::AcbFieldElem)
  bi = parent(x)()
  @ccall libflint.acb_hypgeom_airy(C_NULL::Ptr{Cvoid}, C_NULL::Ptr{Cvoid}, bi::Ref{AcbFieldElem}, C_NULL::Ptr{Cvoid}, x::Ref{AcbFieldElem}, parent(x).prec::Int)::Nothing
  return bi
end

@doc raw"""
    airy_ai_prime(x::AcbFieldElem)

Return the derivative of the Airy function $\operatorname{Ai}^\prime(x)$.
"""
function airy_ai_prime(x::AcbFieldElem)
  ai_prime = parent(x)()
  @ccall libflint.acb_hypgeom_airy(C_NULL::Ptr{Cvoid}, ai_prime::Ref{AcbFieldElem}, C_NULL::Ptr{Cvoid}, C_NULL::Ptr{Cvoid}, x::Ref{AcbFieldElem}, parent(x).prec::Int)::Nothing
  return ai_prime
end

@doc raw"""
    airy_bi_prime(x::AcbFieldElem)

Return the derivative of the Airy function $\operatorname{Bi}^\prime(x)$.
"""
function airy_bi_prime(x::AcbFieldElem)
  bi_prime = parent(x)()
  @ccall libflint.acb_hypgeom_airy(C_NULL::Ptr{Cvoid}, C_NULL::Ptr{Cvoid}, C_NULL::Ptr{Cvoid}, bi_prime::Ref{AcbFieldElem}, x::Ref{AcbFieldElem}, parent(x).prec::Int)::Nothing
  return bi_prime
end

@doc raw"""
    hypergeometric_1f1(a::AcbFieldElem, b::AcbFieldElem, x::AcbFieldElem)

Return the confluent hypergeometric function ${}_1F_1(a,b,x)$.
"""
function hypergeometric_1f1(a::AcbFieldElem, b::AcbFieldElem, x::AcbFieldElem)
  z = parent(x)()
  @ccall libflint.acb_hypgeom_m(z::Ref{AcbFieldElem}, a::Ref{AcbFieldElem}, b::Ref{AcbFieldElem}, x::Ref{AcbFieldElem}, 0::Int, parent(x).prec::Int)::Nothing
  return z
end

@doc raw"""
    hypergeometric_1f1_regularized(a::AcbFieldElem, b::AcbFieldElem, x::AcbFieldElem)

Return the regularized confluent hypergeometric function
${}_1F_1(a,b,x) / \Gamma(b)$.
"""
function hypergeometric_1f1_regularized(a::AcbFieldElem, b::AcbFieldElem, x::AcbFieldElem)
  z = parent(x)()
  @ccall libflint.acb_hypgeom_m(z::Ref{AcbFieldElem}, a::Ref{AcbFieldElem}, b::Ref{AcbFieldElem}, x::Ref{AcbFieldElem}, 1::Int, parent(x).prec::Int)::Nothing
  return z
end

@doc raw"""
    hypergeometric_u(a::AcbFieldElem, b::AcbFieldElem, x::AcbFieldElem)

Return the confluent hypergeometric function $U(a,b,x)$.
"""
function hypergeometric_u(a::AcbFieldElem, b::AcbFieldElem, x::AcbFieldElem)
  z = parent(x)()
  @ccall libflint.acb_hypgeom_u(z::Ref{AcbFieldElem}, a::Ref{AcbFieldElem}, b::Ref{AcbFieldElem}, x::Ref{AcbFieldElem}, parent(x).prec::Int)::Nothing
  return z
end

@doc raw"""
    hypergeometric_2f1(a::AcbFieldElem, b::AcbFieldElem, c::AcbFieldElem, x::AcbFieldElem; flags=0)

Return the Gauss hypergeometric function ${}_2F_1(a,b,c,x)$.
"""
function hypergeometric_2f1(a::AcbFieldElem, b::AcbFieldElem, c::AcbFieldElem, x::AcbFieldElem; flags=0)
  z = parent(x)()
  @ccall libflint.acb_hypgeom_2f1(z::Ref{AcbFieldElem}, a::Ref{AcbFieldElem}, b::Ref{AcbFieldElem}, c::Ref{AcbFieldElem}, x::Ref{AcbFieldElem}, flags::Int, parent(x).prec::Int)::Nothing
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
  @ccall libflint.acb_modular_theta(t1::Ref{AcbFieldElem}, t2::Ref{AcbFieldElem}, t3::Ref{AcbFieldElem}, t4::Ref{AcbFieldElem}, z::Ref{AcbFieldElem}, tau::Ref{AcbFieldElem}, parent(z).prec::Int)::Nothing
  return (t1, t2, t3, t4)
end

@doc raw"""
    weierstrass_p(z::AcbFieldElem, tau::AcbFieldElem)

Return the Weierstrass elliptic function $\wp(z,\tau)$.
"""
function weierstrass_p(z::AcbFieldElem, tau::AcbFieldElem)
  r = parent(z)()
  @ccall libflint.acb_elliptic_p(r::Ref{AcbFieldElem}, z::Ref{AcbFieldElem}, tau::Ref{AcbFieldElem}, parent(z).prec::Int)::Nothing
  return r
end

@doc raw"""
    weierstrass_p_prime(z::AcbFieldElem, tau::AcbFieldElem)

Return the derivative of the Weierstrass elliptic function $\frac{\partial}{\partial z}\wp(z,\tau)$.
"""
function weierstrass_p_prime(z::AcbFieldElem, tau::AcbFieldElem)
  r = parent(z)()
  @ccall libflint.acb_elliptic_p_prime(r::Ref{AcbFieldElem}, z::Ref{AcbFieldElem}, tau::Ref{AcbFieldElem}, parent(z).prec::Int)::Nothing
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

function zero!(z::AcbFieldElemOrPtr)
  @ccall libflint.acb_zero(z::Ref{AcbFieldElem})::Nothing
  return z
end

function one!(z::AcbFieldElemOrPtr)
  @ccall libflint.acb_one(z::Ref{AcbFieldElem})::Nothing
  return z
end

function onei!(z::AcbFieldElemOrPtr)
  @ccall libflint.acb_onei(z::Ref{AcbFieldElem})::Nothing
  return z
end

function neg!(z::AcbFieldElemOrPtr, a::AcbFieldElemOrPtr)
  @ccall libflint.acb_neg(z::Ref{AcbFieldElem}, a::Ref{AcbFieldElem})::Nothing
  return z
end

function add!(z::AcbFieldElem, x::AcbFieldElem, y::AcbFieldElem)
  @ccall libflint.acb_add(z::Ref{AcbFieldElem}, x::Ref{AcbFieldElem}, y::Ref{AcbFieldElem}, parent(z).prec::Int)::Nothing
  return z
end

function sub!(z::AcbFieldElem, x::AcbFieldElem, y::AcbFieldElem)
  @ccall libflint.acb_sub(z::Ref{AcbFieldElem}, x::Ref{AcbFieldElem}, y::Ref{AcbFieldElem}, parent(z).prec::Int)::Nothing
  return z
end

function mul!(z::AcbFieldElem, x::AcbFieldElem, y::AcbFieldElem)
  @ccall libflint.acb_mul(z::Ref{AcbFieldElem}, x::Ref{AcbFieldElem}, y::Ref{AcbFieldElem}, parent(z).prec::Int)::Nothing
  return z
end

function div!(z::AcbFieldElem, x::AcbFieldElem, y::AcbFieldElem)
  @ccall libflint.acb_div(z::Ref{AcbFieldElem}, x::Ref{AcbFieldElem}, y::Ref{AcbFieldElem}, parent(z).prec::Int)::Nothing
  return z
end

################################################################################
#
#  Unsafe setting
#
################################################################################

_real_ptr(x::AcbFieldElemOrPtr) = @ccall libflint.acb_real_ptr(x::Ref{AcbFieldElem})::Ptr{ArbFieldElem}
_imag_ptr(x::AcbFieldElemOrPtr) = @ccall libflint.acb_imag_ptr(x::Ref{AcbFieldElem})::Ptr{ArbFieldElem}

function _acb_set(x::AcbFieldElemOrPtr, y::Int)
  @ccall libflint.acb_set_si(x::Ref{AcbFieldElem}, y::Int)::Nothing
end

function _acb_set(x::AcbFieldElemOrPtr, y::UInt)
  @ccall libflint.acb_set_ui(x::Ref{AcbFieldElem}, y::UInt)::Nothing
end

function _acb_set(x::AcbFieldElemOrPtr, y::Float64)
  @ccall libflint.acb_set_d(x::Ref{AcbFieldElem}, y::Float64)::Nothing
end

function _acb_set(x::AcbFieldElemOrPtr, y::Union{Int,UInt,Float64}, p::Int)
  _acb_set(x, y)
  @ccall libflint.acb_set_round(x::Ref{AcbFieldElem}, x::Ref{AcbFieldElem}, p::Int)::Nothing
end

function _acb_set(x::AcbFieldElemOrPtr, y::ZZRingElem)
  @ccall libflint.acb_set_fmpz(x::Ref{AcbFieldElem}, y::Ref{ZZRingElem})::Nothing
end

function _acb_set(x::AcbFieldElemOrPtr, y::ZZRingElem, p::Int)
  @ccall libflint.acb_set_round_fmpz(x::Ref{AcbFieldElem}, y::Ref{ZZRingElem}, p::Int)::Nothing
end

function _acb_set(x::AcbFieldElemOrPtr, y::QQFieldElem, p::Int)
  @ccall libflint.acb_set_fmpq(x::Ref{AcbFieldElem}, y::Ref{QQFieldElem}, p::Int)::Nothing
end

function _acb_set(x::AcbFieldElemOrPtr, y::ArbFieldElem)
  @ccall libflint.acb_set_arb(x::Ref{AcbFieldElem}, y::Ref{ArbFieldElem})::Nothing
end

function _acb_set(x::AcbFieldElemOrPtr, y::ArbFieldElem, p::Int)
  _acb_set(x, y)
  @ccall libflint.acb_set_round(x::Ref{AcbFieldElem}, x::Ref{AcbFieldElem}, p::Int)::Nothing
end

function _acb_set(x::AcbFieldElemOrPtr, y::AcbFieldElemOrPtr)
  @ccall libflint.acb_set(x::Ref{AcbFieldElem}, y::Ref{AcbFieldElem})::Nothing
end

function _acb_set(x::AcbFieldElemOrPtr, y::Ptr{acb_struct})
  @ccall libflint.acb_set(x::Ref{AcbFieldElem}, y::Ptr{acb_struct})::Nothing
end

function _acb_set(x::Ptr{acb_struct}, y::AcbFieldElemOrPtr)
  @ccall libflint.acb_set(x::Ptr{acb_struct}, y::Ref{AcbFieldElem})::Nothing
end

function _acb_set(x::AcbFieldElemOrPtr, y::AcbFieldElemOrPtr, p::Int)
  @ccall libflint.acb_set_round(x::Ref{AcbFieldElem}, y::Ref{AcbFieldElem}, p::Int)::Nothing
end

function _acb_set(x::AcbFieldElemOrPtr, y::AbstractString, p::Int)
  r = _real_ptr(x)
  _arb_set(r, y, p)
  i = _imag_ptr(x)
  zero!(i)
end

function _acb_set(x::AcbFieldElemOrPtr, y::BigFloat)
  r = _real_ptr(x)
  _arb_set(r, y)
  i = _imag_ptr(x)
  zero!(i)
end

function _acb_set(x::AcbFieldElemOrPtr, y::BigFloat, p::Int)
  r = _real_ptr(x)
  _arb_set(r, y, p)
  i = _imag_ptr(x)
  zero!(i)
end

function _acb_set(x::AcbFieldElemOrPtr, yz::Tuple{Int,Int}, p::Int)
  @ccall libflint.acb_set_si_si(x::Ref{AcbFieldElem}, yz[1]::Int, yz[2]::Int)::Nothing
  @ccall libflint.acb_set_round(x::Ref{AcbFieldElem}, x::Ref{AcbFieldElem}, p::Int)::Nothing
end

function _acb_set(x::AcbFieldElemOrPtr, yz::Tuple{ArbFieldElem,ArbFieldElem})
  @ccall libflint.acb_set_arb_arb(x::Ref{AcbFieldElem}, yz[1]::Ref{ArbFieldElem}, yz[2]::Ref{ArbFieldElem})::Nothing
end

function _acb_set(x::AcbFieldElemOrPtr, yz::Tuple{ArbFieldElem,ArbFieldElem}, p::Int)
  _acb_set(x, yz)
  @ccall libflint.acb_set_round(x::Ref{AcbFieldElem}, x::Ref{AcbFieldElem}, p::Int)::Nothing
end

function _acb_set(x::AcbFieldElemOrPtr, yz::Tuple{QQFieldElem,QQFieldElem}, p::Int)
  r = _real_ptr(x)
  _arb_set(r, yz[1], p)
  i = _imag_ptr(x)
  _arb_set(i, yz[2], p)
end

function _acb_set(x::AcbFieldElemOrPtr, yz::Tuple{AbstractString,AbstractString}, p::Int)
  r = _real_ptr(x)
  _arb_set(r, yz[1], p)
  i = _imag_ptr(x)
  _arb_set(i, yz[2], p)
end

function _acb_set(x::AcbFieldElemOrPtr, y::Real, p::Int)
  r = _real_ptr(x)
  _arb_set(r, y, p)
  i = _imag_ptr(x)
  zero!(i)
end

function _acb_set(x::AcbFieldElemOrPtr, y::Complex, p::Int)
  r = _real_ptr(x)
  _arb_set(r, real(y), p)
  i = _imag_ptr(x)
  _arb_set(i, imag(y), p)
end

function _acb_set(x::AcbFieldElemOrPtr, yz::Tuple{IntegerUnion,IntegerUnion}, p::Int)
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

################################################################################
#
#  Random generation
#
################################################################################

@doc raw"""
    rand(r::AcbField; randtype::Symbol=:urandom)

Return a random element in given Acb field.

The `randtype` default is `:urandom` which returns an `AcbFieldElem` contained in
the closed unit disc.

The rest of the methods return non-uniformly distributed values in order to
exercise corner cases. The option `:randtest` will return a finite number, and
`:randtest_exact` the same but with a zero radius. The option
`:randtest_precise` return an `ArbFieldElem` with a radius around $2^{-\mathrm{prec}}$
the magnitude of the midpoint, while `:randtest_wide` return a radius that
might be big relative to its midpoint. The `:randtest_special`-option might
return a midpoint and radius whose values are `NaN` or `inf`.
"""
function rand(r::AcbField; randtype::Symbol=:urandom)
  state = _flint_rand_states[Threads.threadid()]
  x = r()

  if randtype == :urandom
    @ccall libflint.acb_urandom(x::Ref{AcbFieldElem}, state::Ref{rand_ctx}, r.prec::Int)::Nothing
  elseif randtype == :randtest
    @ccall libflint.acb_randtest(x::Ref{AcbFieldElem}, state::Ref{rand_ctx}, r.prec::Int, 30::Int)::Nothing
  elseif randtype == :randtest_exact
    @ccall libflint.acb_randtest_exact(x::Ref{AcbFieldElem}, state::Ref{rand_ctx}, r.prec::Int, 30::Int)::Nothing
  elseif randtype == :randtest_precise
    @ccall libflint.acb_randtest_precise(x::Ref{AcbFieldElem}, state::Ref{rand_ctx}, r.prec::Int, 30::Int)::Nothing
  elseif randtype == :randtest_wide
    @ccall libflint.acb_randtest_wide(x::Ref{AcbFieldElem}, state::Ref{rand_ctx}, r.prec::Int, 30::Int)::Nothing
  elseif randtype == :randtest_special
    @ccall libflint.acb_randtest_special(x::Ref{AcbFieldElem}, state::Ref{rand_ctx}, r.prec::Int, 30::Int)::Nothing
  else
    error("Acb random generation `" * String(randtype) * "` is not defined")
  end

  return x
end
