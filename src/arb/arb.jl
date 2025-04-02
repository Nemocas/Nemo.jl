###############################################################################
#
#   arb.jl : Arb real numbers
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

elem_type(::Type{ArbField}) = ArbFieldElem

parent_type(::Type{ArbFieldElem}) = ArbField

base_ring_type(::Type{ArbField}) = typeof(Union{})

base_ring(R::ArbField) = Union{}

parent(x::ArbFieldElem) = x.parent

is_domain_type(::Type{ArbFieldElem}) = true

is_exact_type(::Type{ArbFieldElem}) = false

zero(R::ArbField) = R(0)

one(R::ArbField) = R(1)

# TODO: Add hash (and document under ArbFieldElem basic functionality)

@doc raw"""
    accuracy_bits(x::ArbFieldElem)

Return the relative accuracy of $x$ measured in bits, capped between
`typemax(Int)` and `-typemax(Int)`.
"""
function accuracy_bits(x::ArbFieldElem)
  return @ccall libflint.arb_rel_accuracy_bits(x::Ref{ArbFieldElem})::Int
end

function deepcopy_internal(a::ArbFieldElem, dict::IdDict)
  b = parent(a)()
  _arb_set(b, a)
  return b
end


function canonical_unit(x::ArbFieldElem)
  return x
end

characteristic(::ArbField) = 0

_mid_ptr(x::ArbFieldElemOrPtr) = @ccall libflint.arb_mid_ptr(x::Ref{ArbFieldElem})::Ptr{arf_struct}
_rad_ptr(x::ArbFieldElemOrPtr) = @ccall libflint.arb_rad_ptr(x::Ref{ArbFieldElem})::Ptr{mag_struct}

################################################################################
#
#  Conversions
#
################################################################################

@doc raw"""
    Float64(x::ArbFieldElem, round::RoundingMode=RoundNearest)

Converts $x$ to a `Float64`, rounded in the direction specified by $round$.
For `RoundNearest` the return value approximates the midpoint of $x$. For
`RoundDown` or `RoundUp` the return value is a lower bound or upper bound for
all values in $x$.
"""
function Base.Float64(x::ArbFieldElem, round::RoundingMode=RoundNearest)
  t = _arb_get_arf(x, round)
  return _arf_get_d(t, round)
end

@doc raw"""
    BigFloat(x::ArbFieldElem, round::RoundingMode=RoundNearest)

Converts $x$ to a `BigFloat` of the currently used precision, rounded in the
direction specified by $round$. For `RoundNearest` the return value
approximates the midpoint of $x$. For `RoundDown` or `RoundUp` the return
value is a lower bound or upper bound for all values in $x$.
"""
function Base.BigFloat(x::ArbFieldElem, round::RoundingMode=RoundNearest)
  t = _arb_get_arf(x, round)
  return _arf_get_mpfr(t, round)
end

function _arb_get_arf(x::ArbFieldElem, ::RoundingMode{:Nearest})
  t = arf_struct()
  GC.@preserve x begin
    t1 = _mid_ptr(x)
    @ccall libflint.arf_set(t::Ref{arf_struct}, t1::Ptr{arf_struct})::Nothing
  end
  return t
end

for (b, f) in ((RoundingMode{:Down}, :arb_get_lbound_arf),
               (RoundingMode{:Up}, :arb_get_ubound_arf))
  @eval begin
    function _arb_get_arf(x::ArbFieldElem, ::$b)
      t = arf_struct()
      ccall(($(string(f)), libflint), Nothing,
            (Ref{arf_struct}, Ref{ArbFieldElem}, Int),
            t, x, parent(x).prec)
      return t
    end
  end
end

for (b, i) in ((RoundingMode{:Down}, ARB_RND_FLOOR),
               (RoundingMode{:Up}, ARB_RND_CEIL),
               (RoundingMode{:Nearest}, ARB_RND_NEAR))
  @eval begin
    function _arf_get_d(t::arf_struct, ::$b)
      d = @ccall libflint.arf_get_d(t::Ref{arf_struct}, $i::Int)::Float64
      return d
    end
    function _arf_get_mpfr(t::arf_struct, ::$b)
      d = BigFloat()
      @ccall libflint.arf_get_mpfr(d::Ref{BigFloat}, t::Ref{arf_struct}, $b()::Base.MPFR.MPFRRoundingMode)::Int32
      return d
    end
  end
end

function convert(::Type{Float64}, x::ArbFieldElem)
  return Float64(x)
end

function convert(::Type{BigFloat}, x::ArbFieldElem)
  return BigFloat(x)
end

@doc raw"""
    ZZRingElem(x::ArbFieldElem)

Return $x$ as an `ZZRingElem` if it represents an unique integer, else throws an
error.
"""
function ZZRingElem(x::ArbFieldElem)
  if is_exact(x)
    ok, z = unique_integer(x)
    ok && return z
  end
  error("Argument must represent a unique integer")
end

Base.BigInt(x::ArbFieldElem) = BigInt(ZZRingElem(x))

function (::Type{T})(x::ArbFieldElem) where {T <: Integer}
  typemin(T) <= x <= typemax(T) ||
  error("Argument does not fit inside datatype.")
  return T(ZZRingElem(x))
end

################################################################################
#
#  String I/O
#
################################################################################

function native_string(x::ArbFieldElem)
  d = ceil(parent(x).prec * 0.30102999566398119521)
  cstr = @ccall libflint.arb_get_str(x::Ref{ArbFieldElem}, Int(d)::Int, UInt(0)::UInt)::Ptr{UInt8}
  res = unsafe_string(cstr)
  @ccall libflint.flint_free(cstr::Ptr{UInt8})::Nothing
  return res
end

function expressify(x::ArbFieldElem; context = nothing)
  if is_exact(x) && is_negative(x)
    # TODO is_exact does not imply it is printed without radius
    return Expr(:call, :-, native_string(-x))
  else
    return native_string(x)
  end
end

function show(io::IO, x::ArbField)
  @show_name(io, x)
  @show_special(io, x)
  print(io, "Real Field with ")
  print(io, precision(x))
  print(io, " bits of precision and error bounds")
end

function show(io::IO, x::ArbFieldElem)
  print(io, native_string(x))
end

################################################################################
#
#  Containment
#
################################################################################

@doc raw"""
    overlaps(x::ArbFieldElem, y::ArbFieldElem)

Returns `true` if any part of the ball $x$ overlaps any part of the ball $y$,
otherwise return `false`.
"""
function overlaps(x::ArbFieldElem, y::ArbFieldElem)
  r = @ccall libflint.arb_overlaps(x::Ref{ArbFieldElem}, y::Ref{ArbFieldElem})::Cint
  return Bool(r)
end

#function contains(x::ArbFieldElem, y::arf)
#  r = @ccall libflint.arb_contains_arf(x::Ref{ArbFieldElem}, y::Ref{arf})::Cint
#  return Bool(r)
#end

@doc raw"""
    contains(x::ArbFieldElem, y::QQFieldElem)

Returns `true` if the ball $x$ contains the given rational value, otherwise
return `false`.
"""
function contains(x::ArbFieldElem, y::QQFieldElem)
  r = @ccall libflint.arb_contains_fmpq(x::Ref{ArbFieldElem}, y::Ref{QQFieldElem})::Cint
  return Bool(r)
end

@doc raw"""
    contains(x::ArbFieldElem, y::ZZRingElem)

Returns `true` if the ball $x$ contains the given integer value, otherwise
return `false`.
"""
function contains(x::ArbFieldElem, y::ZZRingElem)
  r = @ccall libflint.arb_contains_fmpz(x::Ref{ArbFieldElem}, y::Ref{ZZRingElemRaw})::Cint
  return Bool(r)
end

function contains(x::ArbFieldElem, y::Int)
  r = @ccall libflint.arb_contains_si(x::Ref{ArbFieldElem}, y::Int)::Cint
  return Bool(r)
end

@doc raw"""
    contains(x::ArbFieldElem, y::Integer)

Returns `true` if the ball $x$ contains the given integer value, otherwise
return `false`.
"""
contains(x::ArbFieldElem, y::Integer) = contains(x, ZZRingElem(y))

@doc raw"""
    contains(x::ArbFieldElem, y::Rational{T}) where {T <: Integer}

Returns `true` if the ball $x$ contains the given rational value, otherwise
return `false`.
"""
contains(x::ArbFieldElem, y::Rational{T}) where {T <: Integer} = contains(x, QQFieldElem(y))

@doc raw"""
    contains(x::ArbFieldElem, y::BigFloat)

Returns `true` if the ball $x$ contains the given floating point value,
otherwise return `false`.
"""
function contains(x::ArbFieldElem, y::BigFloat)
  r = @ccall libflint.arb_contains_mpfr(x::Ref{ArbFieldElem}, y::Ref{BigFloat})::Cint
  return Bool(r)
end

@doc raw"""
    contains(x::ArbFieldElem, y::ArbFieldElem)

Returns `true` if the ball $x$ contains the ball $y$, otherwise return
`false`.
"""
function contains(x::ArbFieldElem, y::ArbFieldElem)
  r = @ccall libflint.arb_contains(x::Ref{ArbFieldElem}, y::Ref{ArbFieldElem})::Cint
  return Bool(r)
end

@doc raw"""
    contains_zero(x::ArbFieldElem)

Returns `true` if the ball $x$ contains zero, otherwise return `false`.
"""
function contains_zero(x::ArbFieldElem)
  r = @ccall libflint.arb_contains_zero(x::Ref{ArbFieldElem})::Cint
  return Bool(r)
end

@doc raw"""
    contains_negative(x::ArbFieldElem)

Returns `true` if the ball $x$ contains any negative value, otherwise return
`false`.
"""
function contains_negative(x::ArbFieldElem)
  r = @ccall libflint.arb_contains_negative(x::Ref{ArbFieldElem})::Cint
  return Bool(r)
end

@doc raw"""
    contains_positive(x::ArbFieldElem)

Returns `true` if the ball $x$ contains any positive value, otherwise return
`false`.
"""
function contains_positive(x::ArbFieldElem)
  r = @ccall libflint.arb_contains_positive(x::Ref{ArbFieldElem})::Cint
  return Bool(r)
end

@doc raw"""
    contains_nonnegative(x::ArbFieldElem)

Returns `true` if the ball $x$ contains any non-negative value, otherwise
return `false`.
"""
function contains_nonnegative(x::ArbFieldElem)
  r = @ccall libflint.arb_contains_nonnegative(x::Ref{ArbFieldElem})::Cint
  return Bool(r)
end

@doc raw"""
    contains_nonpositive(x::ArbFieldElem)

Returns `true` if the ball $x$ contains any nonpositive value, otherwise
return `false`.
"""
function contains_nonpositive(x::ArbFieldElem)
  r = @ccall libflint.arb_contains_nonpositive(x::Ref{ArbFieldElem})::Cint
  return Bool(r)
end

################################################################################
#
#  Comparison
#
################################################################################

@doc raw"""
    isequal(x::ArbFieldElem, y::ArbFieldElem)

Return `true` if the balls $x$ and $y$ are precisely equal, i.e. have the
same midpoints and radii.
"""
function isequal(x::ArbFieldElem, y::ArbFieldElem)
  r = @ccall libflint.arb_equal(x::Ref{ArbFieldElem}, y::Ref{ArbFieldElem})::Cint
  return Bool(r)
end

function Base.hash(x::ArbFieldElem, h::UInt)
  # TODO: improve me
  return h
end

function ==(x::ArbFieldElem, y::ArbFieldElem)
  return Bool(@ccall libflint.arb_eq(x::Ref{ArbFieldElem}, y::Ref{ArbFieldElem})::Cint)
end

function !=(x::ArbFieldElem, y::ArbFieldElem)
  return Bool(@ccall libflint.arb_ne(x::Ref{ArbFieldElem}, y::Ref{ArbFieldElem})::Cint)
end

function isless(x::ArbFieldElem, y::ArbFieldElem)
  return Bool(@ccall libflint.arb_lt(x::Ref{ArbFieldElem}, y::Ref{ArbFieldElem})::Cint)
end

function <=(x::ArbFieldElem, y::ArbFieldElem)
  return Bool(@ccall libflint.arb_le(x::Ref{ArbFieldElem}, y::Ref{ArbFieldElem})::Cint)
end

==(x::ArbFieldElem, y::Int) = x == ArbFieldElem(y)
!=(x::ArbFieldElem, y::Int) = x != ArbFieldElem(y)
<=(x::ArbFieldElem, y::Int) = x <= ArbFieldElem(y)
<(x::ArbFieldElem, y::Int) = x < ArbFieldElem(y)

==(x::Int, y::ArbFieldElem) = ArbFieldElem(x) == y
!=(x::Int, y::ArbFieldElem) = ArbFieldElem(x) != y
<=(x::Int, y::ArbFieldElem) = ArbFieldElem(x) <= y
<(x::Int, y::ArbFieldElem) = ArbFieldElem(x) < y

==(x::ArbFieldElem, y::ZZRingElem) = x == ArbFieldElem(y)
!=(x::ArbFieldElem, y::ZZRingElem) = x != ArbFieldElem(y)
<=(x::ArbFieldElem, y::ZZRingElem) = x <= ArbFieldElem(y)
<(x::ArbFieldElem, y::ZZRingElem) = x < ArbFieldElem(y)

==(x::ZZRingElem, y::ArbFieldElem) = ArbFieldElem(x) == y
!=(x::ZZRingElem, y::ArbFieldElem) = ArbFieldElem(x) != y
<=(x::ZZRingElem, y::ArbFieldElem) = ArbFieldElem(x) <= y
<(x::ZZRingElem, y::ArbFieldElem) = ArbFieldElem(x) < y

==(x::ArbFieldElem, y::Integer) = x == ZZRingElem(y)
!=(x::ArbFieldElem, y::Integer) = x != ZZRingElem(y)
<=(x::ArbFieldElem, y::Integer) = x <= ZZRingElem(y)
<(x::ArbFieldElem, y::Integer) = x < ZZRingElem(y)


==(x::Integer, y::ArbFieldElem) = ZZRingElem(x) == y
!=(x::Integer, y::ArbFieldElem) = ZZRingElem(x) != y
<=(x::Integer, y::ArbFieldElem) = ZZRingElem(x) <= y
<(x::Integer, y::ArbFieldElem) = ZZRingElem(x) < y

==(x::ArbFieldElem, y::Float64) = x == ArbFieldElem(y)
!=(x::ArbFieldElem, y::Float64) = x != ArbFieldElem(y)
<=(x::ArbFieldElem, y::Float64) = x <= ArbFieldElem(y)
<(x::ArbFieldElem, y::Float64) = x < ArbFieldElem(y)

==(x::Float64, y::ArbFieldElem) = ArbFieldElem(x) == y
!=(x::Float64, y::ArbFieldElem) = ArbFieldElem(x) != y
<=(x::Float64, y::ArbFieldElem) = ArbFieldElem(x) <= y
<(x::Float64, y::ArbFieldElem) = ArbFieldElem(x) < y

==(x::ArbFieldElem, y::BigFloat) = x == ArbFieldElem(y)
!=(x::ArbFieldElem, y::BigFloat) = x != ArbFieldElem(y)
<=(x::ArbFieldElem, y::BigFloat) = x <= ArbFieldElem(y)
<(x::ArbFieldElem, y::BigFloat) = x < ArbFieldElem(y)

==(x::BigFloat, y::ArbFieldElem) = ArbFieldElem(x) == y
!=(x::BigFloat, y::ArbFieldElem) = ArbFieldElem(x) != y
<=(x::BigFloat, y::ArbFieldElem) = ArbFieldElem(x) <= y
<(x::BigFloat, y::ArbFieldElem) = ArbFieldElem(x) < y

==(x::ArbFieldElem, y::QQFieldElem) = x == ArbFieldElem(y, precision(parent(x)))
!=(x::ArbFieldElem, y::QQFieldElem) = x != ArbFieldElem(y, precision(parent(x)))
<=(x::ArbFieldElem, y::QQFieldElem) = x <= ArbFieldElem(y, precision(parent(x)))
<(x::ArbFieldElem, y::QQFieldElem) = x < ArbFieldElem(y, precision(parent(x)))

==(x::QQFieldElem, y::ArbFieldElem) = ArbFieldElem(x, precision(parent(y))) == y
!=(x::QQFieldElem, y::ArbFieldElem) = ArbFieldElem(x, precision(parent(y))) != y
<=(x::QQFieldElem, y::ArbFieldElem) = ArbFieldElem(x, precision(parent(y))) <= y
<(x::QQFieldElem, y::ArbFieldElem) = ArbFieldElem(x, precision(parent(y))) < y

==(x::ArbFieldElem, y::Rational{T}) where {T <: Integer} = x == QQFieldElem(y)
!=(x::ArbFieldElem, y::Rational{T}) where {T <: Integer} = x != QQFieldElem(y)
<=(x::ArbFieldElem, y::Rational{T}) where {T <: Integer} = x <= QQFieldElem(y)
<(x::ArbFieldElem, y::Rational{T}) where {T <: Integer} = x < QQFieldElem(y)

==(x::Rational{T}, y::ArbFieldElem) where {T <: Integer} = QQFieldElem(x) == y
!=(x::Rational{T}, y::ArbFieldElem) where {T <: Integer} = QQFieldElem(x) != y
<=(x::Rational{T}, y::ArbFieldElem) where {T <: Integer} = QQFieldElem(x) <= y
<(x::Rational{T}, y::ArbFieldElem) where {T <: Integer} = QQFieldElem(x) < y

################################################################################
#
#  Predicates
#
################################################################################

@doc raw"""
    iszero(x::ArbFieldElem)

Return `true` if $x$ is certainly zero, otherwise return `false`.
"""
function iszero(x::ArbFieldElem)
  return Bool(@ccall libflint.arb_is_zero(x::Ref{ArbFieldElem})::Cint)
end

@doc raw"""
    is_nonzero(x::ArbFieldElem)

Return `true` if $x$ is certainly not equal to zero, otherwise return
`false`.
"""
function is_nonzero(x::ArbFieldElem)
  return Bool(@ccall libflint.arb_is_nonzero(x::Ref{ArbFieldElem})::Cint)
end

@doc raw"""
    isone(x::ArbFieldElem)

Return `true` if $x$ is certainly one, otherwise return `false`.
"""
function isone(x::ArbFieldElem)
  return Bool(@ccall libflint.arb_is_one(x::Ref{ArbFieldElem})::Cint)
end

@doc raw"""
    isfinite(x::ArbFieldElem)

Return `true` if $x$ is finite, i.e. having finite midpoint and radius,
otherwise return `false`.
"""
function isfinite(x::ArbFieldElem)
  return Bool(@ccall libflint.arb_is_finite(x::Ref{ArbFieldElem})::Cint)
end

@doc raw"""
    is_exact(x::ArbFieldElem)

Return `true` if $x$ is exact, i.e. has zero radius, otherwise return
`false`.
"""
function is_exact(x::ArbFieldElem)
  return Bool(@ccall libflint.arb_is_exact(x::Ref{ArbFieldElem})::Cint)
end

@doc raw"""
    isinteger(x::ArbFieldElem)

Return `true` if $x$ is an exact integer, otherwise return `false`.
"""
function isinteger(x::ArbFieldElem)
  return Bool(@ccall libflint.arb_is_int(x::Ref{ArbFieldElem})::Cint)
end

@doc raw"""
    is_positive(x::ArbFieldElem)

Return `true` if $x$ is certainly positive, otherwise return `false`.
"""
function is_positive(x::ArbFieldElem)
  return Bool(@ccall libflint.arb_is_positive(x::Ref{ArbFieldElem})::Cint)
end

@doc raw"""
    is_nonnegative(x::ArbFieldElem)

Return `true` if $x$ is certainly non-negative, otherwise return `false`.
"""
function is_nonnegative(x::ArbFieldElem)
  return Bool(@ccall libflint.arb_is_nonnegative(x::Ref{ArbFieldElem})::Cint)
end

@doc raw"""
    is_negative(x::ArbFieldElem)

Return `true` if $x$ is certainly negative, otherwise return `false`.
"""
function is_negative(x::ArbFieldElem)
  return Bool(@ccall libflint.arb_is_negative(x::Ref{ArbFieldElem})::Cint)
end

@doc raw"""
    is_nonpositive(x::ArbFieldElem)

Return `true` if $x$ is certainly nonpositive, otherwise return `false`.
"""
function is_nonpositive(x::ArbFieldElem)
  return Bool(@ccall libflint.arb_is_nonpositive(x::Ref{ArbFieldElem})::Cint)
end

################################################################################
#
#  Parts of numbers
#
################################################################################

@doc raw"""
    ball(x::ArbFieldElem, y::ArbFieldElem)

Constructs an Arb ball enclosing $x_m \pm (|x_r| + |y_m| + |y_r|)$, given the
pair $(x, y) = (x_m \pm x_r, y_m \pm y_r)$.
"""
function ball(mid::ArbFieldElem, rad::ArbFieldElem)
  z = ArbFieldElem(mid, rad)
  z.parent = parent(mid)
  return z
end

@doc raw"""
    radius(x::ArbFieldElem)

Return the radius of the ball $x$ as an Arb ball.
"""
function radius(x::ArbFieldElem)
  z = parent(x)()
  @ccall libflint.arb_get_rad_arb(z::Ref{ArbFieldElem}, x::Ref{ArbFieldElem})::Nothing
  return z
end

@doc raw"""
    midpoint(x::ArbFieldElem)

Return the midpoint of the ball $x$ as an Arb ball.
"""
function midpoint(x::ArbFieldElem)
  z = parent(x)()
  @ccall libflint.arb_get_mid_arb(z::Ref{ArbFieldElem}, x::Ref{ArbFieldElem})::Nothing
  return z
end

@doc raw"""
    add_error!(x::ArbFieldElem, y::ArbFieldElem)

Adds the absolute values of the midpoint and radius of $y$ to the radius of $x$.
"""
function add_error!(x::ArbFieldElem, y::ArbFieldElem)
  @ccall libflint.arb_add_error(x::Ref{ArbFieldElem}, y::Ref{ArbFieldElem})::Nothing
end

################################################################################
#
#  Sign
#
################################################################################

function sign(::Type{Int}, x::ArbFieldElem)
  if is_positive(x)
    return 1
  elseif is_negative(x)
    return -1
  else
    error("Could not determine sign")
  end
end

Base.signbit(x::ArbFieldElem) = signbit(sign(Int, x))

################################################################################
#
#  Unary operations
#
################################################################################

-(x::ArbFieldElem) = neg!(parent(x)(), x)

################################################################################
#
#  Binary operations
#
################################################################################

for (s,f) in ((:+,"arb_add"), (:*,"arb_mul"), (://, "arb_div"), (:-,"arb_sub"))
  @eval begin
    function ($s)(x::ArbFieldElem, y::ArbFieldElem)
      z = parent(x)()
      ccall(($f, libflint), Nothing, (Ref{ArbFieldElem}, Ref{ArbFieldElem}, Ref{ArbFieldElem}, Int),
            z, x, y, parent(x).prec)
      return z
    end
  end
end

for (f,s) in ((:+, "add"), (:*, "mul"))
  @eval begin
    #function ($f)(x::ArbFieldElem, y::arf)
    #  z = parent(x)()
    #  ccall(($("arb_"*s*"_arf"), libflint), Nothing,
    #              (Ref{ArbFieldElem}, Ref{ArbFieldElem}, Ref{arf}, Int),
    #              z, x, y, parent(x).prec)
    #  return z
    #end

    #($f)(x::arf, y::ArbFieldElem) = ($f)(y, x)

    function ($f)(x::ArbFieldElem, y::UInt)
      z = parent(x)()
      ccall(($("arb_"*s*"_ui"), libflint), Nothing,
            (Ref{ArbFieldElem}, Ref{ArbFieldElem}, UInt, Int),
            z, x, y, parent(x).prec)
      return z
    end

    ($f)(x::UInt, y::ArbFieldElem) = ($f)(y, x)

    function ($f)(x::ArbFieldElem, y::Int)
      z = parent(x)()
      ccall(($("arb_"*s*"_si"), libflint), Nothing,
            (Ref{ArbFieldElem}, Ref{ArbFieldElem}, Int, Int), z, x, y, parent(x).prec)
      return z
    end

    ($f)(x::Int, y::ArbFieldElem) = ($f)(y,x)

    function ($f)(x::ArbFieldElem, y::ZZRingElem)
      z = parent(x)()
      ccall(($("arb_"*s*"_fmpz"), libflint), Nothing,
            (Ref{ArbFieldElem}, Ref{ArbFieldElem}, Ref{ZZRingElemRaw}, Int),
            z, x, y, parent(x).prec)
      return z
    end

    ($f)(x::ZZRingElem, y::ArbFieldElem) = ($f)(y,x)
  end
end

#function -(x::ArbFieldElem, y::arf)
#  z = parent(x)()
#  ccall((:arb_sub_arf, libflint), Nothing,
#              (Ref{ArbFieldElem}, Ref{ArbFieldElem}, Ref{arf}, Int), z, x, y, parent(x).prec)
#  return z
#end

#-(x::arf, y::ArbFieldElem) = -(y - x)

function -(x::ArbFieldElem, y::UInt)
  z = parent(x)()
  @ccall libflint.arb_sub_ui(z::Ref{ArbFieldElem}, x::Ref{ArbFieldElem}, y::UInt, parent(x).prec::Int)::Nothing
  return z
end

-(x::UInt, y::ArbFieldElem) = -(y - x)

function -(x::ArbFieldElem, y::Int)
  z = parent(x)()
  @ccall libflint.arb_sub_si(z::Ref{ArbFieldElem}, x::Ref{ArbFieldElem}, y::Int, parent(x).prec::Int)::Nothing
  return z
end

-(x::Int, y::ArbFieldElem) = -(y - x)

function -(x::ArbFieldElem, y::ZZRingElem)
  z = parent(x)()
  @ccall libflint.arb_sub_fmpz(z::Ref{ArbFieldElem}, x::Ref{ArbFieldElem}, y::Ref{ZZRingElemRaw}, parent(x).prec::Int)::Nothing
  return z
end

-(x::ZZRingElem, y::ArbFieldElem) = -(y-x)

+(x::ArbFieldElem, y::Integer) = x + ZZRingElem(y)

-(x::ArbFieldElem, y::Integer) = x - ZZRingElem(y)

*(x::ArbFieldElem, y::Integer) = x*ZZRingElem(y)

//(x::ArbFieldElem, y::Integer) = x//ZZRingElem(y)

+(x::Integer, y::ArbFieldElem) = ZZRingElem(x) + y

-(x::Integer, y::ArbFieldElem) = ZZRingElem(x) - y

*(x::Integer, y::ArbFieldElem) = ZZRingElem(x)*y

//(x::Integer, y::ArbFieldElem) = ZZRingElem(x)//y

#function //(x::ArbFieldElem, y::arf)
#  z = parent(x)()
#  ccall((:arb_div_arf, libflint), Nothing,
#              (Ref{ArbFieldElem}, Ref{ArbFieldElem}, Ref{arf}, Int), z, x, y, parent(x).prec)
#  return z
#end

function //(x::ArbFieldElem, y::UInt)
  z = parent(x)()
  @ccall libflint.arb_div_ui(z::Ref{ArbFieldElem}, x::Ref{ArbFieldElem}, y::UInt, parent(x).prec::Int)::Nothing
  return z
end

function //(x::ArbFieldElem, y::Int)
  z = parent(x)()
  @ccall libflint.arb_div_si(z::Ref{ArbFieldElem}, x::Ref{ArbFieldElem}, y::Int, parent(x).prec::Int)::Nothing
  return z
end

function //(x::ArbFieldElem, y::ZZRingElem)
  z = parent(x)()
  @ccall libflint.arb_div_fmpz(z::Ref{ArbFieldElem}, x::Ref{ArbFieldElem}, y::Ref{ZZRingElemRaw}, parent(x).prec::Int)::Nothing
  return z
end

function //(x::UInt, y::ArbFieldElem)
  z = parent(y)()
  @ccall libflint.arb_ui_div(z::Ref{ArbFieldElem}, x::UInt, y::Ref{ArbFieldElem}, parent(y).prec::Int)::Nothing
  return z
end

function //(x::Int, y::ArbFieldElem)
  z = parent(y)()
  t = ArbFieldElem(x)
  @ccall libflint.arb_div(z::Ref{ArbFieldElem}, t::Ref{ArbFieldElem}, y::Ref{ArbFieldElem}, parent(y).prec::Int)::Nothing
  return z
end

function //(x::ZZRingElem, y::ArbFieldElem)
  z = parent(y)()
  t = ArbFieldElem(x)
  @ccall libflint.arb_div(z::Ref{ArbFieldElem}, t::Ref{ArbFieldElem}, y::Ref{ArbFieldElem}, parent(y).prec::Int)::Nothing
  return z
end

function ^(x::ArbFieldElem, y::ArbFieldElem)
  z = parent(x)()
  @ccall libflint.arb_pow(z::Ref{ArbFieldElem}, x::Ref{ArbFieldElem}, y::Ref{ArbFieldElem}, parent(x).prec::Int)::Nothing
  return z
end

function ^(x::ArbFieldElem, y::ZZRingElem)
  z = parent(x)()
  @ccall libflint.arb_pow_fmpz(z::Ref{ArbFieldElem}, x::Ref{ArbFieldElem}, y::Ref{ZZRingElemRaw}, parent(x).prec::Int)::Nothing
  return z
end

^(x::ArbFieldElem, y::Integer) = x^ZZRingElem(y)

function ^(x::ArbFieldElem, y::UInt)
  z = parent(x)()
  @ccall libflint.arb_pow_ui(z::Ref{ArbFieldElem}, x::Ref{ArbFieldElem}, y::UInt, parent(x).prec::Int)::Nothing
  return z
end

function ^(x::ArbFieldElem, y::QQFieldElem)
  z = parent(x)()
  @ccall libflint.arb_pow_fmpq(z::Ref{ArbFieldElem}, x::Ref{ArbFieldElem}, y::Ref{QQFieldElem}, parent(x).prec::Int)::Nothing
  return z
end


for T in (Float64, BigFloat, Rational, QQFieldElem)
  @eval begin
    +(x::$T, y::ArbFieldElem) = parent(y)(x) + y
    +(x::ArbFieldElem, y::$T) = x + parent(x)(y)
    -(x::$T, y::ArbFieldElem) = parent(y)(x) - y
    //(x::ArbFieldElem, y::$T) = x//parent(x)(y)
    //(x::$T, y::ArbFieldElem) = parent(y)(x)//y
    -(x::ArbFieldElem, y::$T) = x - parent(x)(y)
    *(x::$T, y::ArbFieldElem) = parent(y)(x) * y
    *(x::ArbFieldElem, y::$T) = x * parent(x)(y)
    ^(x::$T, y::ArbFieldElem) = parent(y)(x) ^ y
  end
end

for T in (Float64, BigFloat, Rational)
  @eval begin
    ^(x::ArbFieldElem, y::$T) = x ^ parent(x)(y)
  end
end

for T in (Float64, BigFloat, Integer, Rational, QQFieldElem)
  @eval begin
    /(x::$T, y::ArbFieldElem) = x // y
    /(x::ArbFieldElem, y::$T) = x // y
    divexact(x::$T, y::ArbFieldElem; check::Bool=true) = x // y
    divexact(x::ArbFieldElem, y::$T; check::Bool=true) = x // y
  end
end

/(x::ArbFieldElem, y::ArbFieldElem) = x // y
/(x::ZZRingElem, y::ArbFieldElem) = x // y
/(x::ArbFieldElem, y::ZZRingElem) = x // y

divexact(x::ArbFieldElem, y::ArbFieldElem; check::Bool=true) = x // y
divexact(x::ZZRingElem, y::ArbFieldElem; check::Bool=true) = x // y
divexact(x::ArbFieldElem, y::ZZRingElem; check::Bool=true) = x // y

################################################################################
#
#  Absolute value
#
################################################################################

function abs(x::ArbFieldElem)
  z = parent(x)()
  @ccall libflint.arb_abs(z::Ref{ArbFieldElem}, x::Ref{ArbFieldElem})::Nothing
  return z
end

################################################################################
#
#  Inverse
#
################################################################################

function inv(x::ArbFieldElem)
  z = parent(x)()
  @ccall libflint.arb_inv(z::Ref{ArbFieldElem}, x::Ref{ArbFieldElem}, parent(x).prec::Int)::Nothing
  return parent(x)(z)
end

################################################################################
#
#  Shifting
#
################################################################################

function ldexp(x::ArbFieldElem, y::Int)
  z = parent(x)()
  @ccall libflint.arb_mul_2exp_si(z::Ref{ArbFieldElem}, x::Ref{ArbFieldElem}, y::Int)::Nothing
  return z
end

function ldexp(x::ArbFieldElem, y::ZZRingElem)
  z = parent(x)()
  @ccall libflint.arb_mul_2exp_fmpz(z::Ref{ArbFieldElem}, x::Ref{ArbFieldElem}, y::Ref{ZZRingElemRaw})::Nothing
  return z
end

################################################################################
#
#  Miscellaneous
#
################################################################################

@doc raw"""
    trim(x::ArbFieldElem)

Return an `ArbFieldElem` interval containing $x$ but which may be more economical,
by rounding off insignificant bits from the midpoint.
"""
function trim(x::ArbFieldElem)
  z = parent(x)()
  @ccall libflint.arb_trim(z::Ref{ArbFieldElem}, x::Ref{ArbFieldElem})::Nothing
  return z
end

@doc raw"""
    unique_integer(x::ArbFieldElem)

Return a pair where the first value is a boolean and the second is an `ZZRingElem`
integer. The boolean indicates whether the interval $x$ contains a unique
integer. If this is the case, the second return value is set to this unique
integer.
"""
function unique_integer(x::ArbFieldElem)
  z = ZZRingElem()
  unique = @ccall libflint.arb_get_unique_fmpz(z::Ref{ZZRingElemRaw}, x::Ref{ArbFieldElem})::Int
  return (unique != 0, z)
end

function (::ZZRing)(a::ArbFieldElem)
  return ZZRingElem(a)
end

@doc raw"""
    setunion(x::ArbFieldElem, y::ArbFieldElem)

Return an `ArbFieldElem` containing the union of the intervals represented by $x$ and
$y$.
"""
function setunion(x::ArbFieldElem, y::ArbFieldElem)
  z = parent(x)()
  @ccall libflint.arb_union(z::Ref{ArbFieldElem}, x::Ref{ArbFieldElem}, y::Ref{ArbFieldElem}, parent(x).prec::Int)::Nothing
  return z
end

@doc raw"""
    setintersection(x::ArbFieldElem, y::ArbFieldElem)

Return an `ArbFieldElem` containing the intersection of the intervals represented by
$x$ and $y$.
"""
function setintersection(x::ArbFieldElem, y::ArbFieldElem)
  z = parent(x)()
  @ccall libflint.arb_intersection(z::Ref{ArbFieldElem}, x::Ref{ArbFieldElem}, y::Ref{ArbFieldElem}, parent(x).prec::Int)::Nothing
  return z
end

################################################################################
#
#  Constants
#
################################################################################

@doc raw"""
    const_pi(r::ArbField)

Return $\pi = 3.14159\ldots$ as an element of $r$.
"""
function const_pi(r::ArbField)
  z = r()
  @ccall libflint.arb_const_pi(z::Ref{ArbFieldElem}, precision(r)::Int)::Nothing
  return z
end

@doc raw"""
    const_e(r::ArbField)

Return $e = 2.71828\ldots$ as an element of $r$.
"""
function const_e(r::ArbField)
  z = r()
  @ccall libflint.arb_const_e(z::Ref{ArbFieldElem}, precision(r)::Int)::Nothing
  return z
end

@doc raw"""
    const_log2(r::ArbField)

Return $\log(2) = 0.69314\ldots$ as an element of $r$.
"""
function const_log2(r::ArbField)
  z = r()
  @ccall libflint.arb_const_log2(z::Ref{ArbFieldElem}, precision(r)::Int)::Nothing
  return z
end

@doc raw"""
    const_log10(r::ArbField)

Return $\log(10) = 2.302585\ldots$ as an element of $r$.
"""
function const_log10(r::ArbField)
  z = r()
  @ccall libflint.arb_const_log10(z::Ref{ArbFieldElem}, precision(r)::Int)::Nothing
  return z
end

@doc raw"""
    const_euler(r::ArbField)

Return Euler's constant $\gamma = 0.577215\ldots$ as an element of $r$.
"""
function const_euler(r::ArbField)
  z = r()
  @ccall libflint.arb_const_euler(z::Ref{ArbFieldElem}, precision(r)::Int)::Nothing
  return z
end

@doc raw"""
    const_catalan(r::ArbField)

Return Catalan's constant $C = 0.915965\ldots$ as an element of $r$.
"""
function const_catalan(r::ArbField)
  z = r()
  @ccall libflint.arb_const_catalan(z::Ref{ArbFieldElem}, precision(r)::Int)::Nothing
  return z
end

@doc raw"""
    const_khinchin(r::ArbField)

Return Khinchin's constant $K = 2.685452\ldots$ as an element of $r$.
"""
function const_khinchin(r::ArbField)
  z = r()
  @ccall libflint.arb_const_khinchin(z::Ref{ArbFieldElem}, precision(r)::Int)::Nothing
  return z
end

@doc raw"""
    const_glaisher(r::ArbField)

Return Glaisher's constant $A = 1.282427\ldots$ as an element of $r$.
"""
function const_glaisher(r::ArbField)
  z = r()
  @ccall libflint.arb_const_glaisher(z::Ref{ArbFieldElem}, precision(r)::Int)::Nothing
  return z
end

################################################################################
#
#  Real valued functions
#
################################################################################

# real - real functions

function floor(x::ArbFieldElem)
  z = parent(x)()
  @ccall libflint.arb_floor(z::Ref{ArbFieldElem}, x::Ref{ArbFieldElem}, parent(x).prec::Int)::Nothing
  return z
end

floor(::Type{ArbFieldElem}, x::ArbFieldElem) = floor(x)
floor(::Type{ZZRingElem}, x::ArbFieldElem) = ZZRingElem(floor(x))
floor(::Type{T}, x::ArbFieldElem) where {T <: Integer} = T(floor(x))

function ceil(x::ArbFieldElem)
  z = parent(x)()
  @ccall libflint.arb_ceil(z::Ref{ArbFieldElem}, x::Ref{ArbFieldElem}, parent(x).prec::Int)::Nothing
  return z
end

ceil(::Type{ArbFieldElem}, x::ArbFieldElem) = ceil(x)
ceil(::Type{ZZRingElem}, x::ArbFieldElem) = ZZRingElem(ceil(x))
ceil(::Type{T}, x::ArbFieldElem) where {T <: Integer} = T(ceil(x))

function Base.sqrt(x::ArbFieldElem; check::Bool=true)
  z = parent(x)()
  @ccall libflint.arb_sqrt(z::Ref{ArbFieldElem}, x::Ref{ArbFieldElem}, parent(x).prec::Int)::Nothing
  return z
end

@doc raw"""
    rsqrt(x::ArbFieldElem)

Return the reciprocal of the square root of $x$, i.e. $1/\sqrt{x}$.
"""
function rsqrt(x::ArbFieldElem)
  z = parent(x)()
  @ccall libflint.arb_rsqrt(z::Ref{ArbFieldElem}, x::Ref{ArbFieldElem}, parent(x).prec::Int)::Nothing
  return z
end

@doc raw"""
    sqrt1pm1(x::ArbFieldElem)

Return $\sqrt{1+x}-1$, evaluated accurately for small $x$.
"""
function sqrt1pm1(x::ArbFieldElem)
  z = parent(x)()
  @ccall libflint.arb_sqrt1pm1(z::Ref{ArbFieldElem}, x::Ref{ArbFieldElem}, parent(x).prec::Int)::Nothing
  return z
end

@doc raw"""
    sqrtpos(x::ArbFieldElem)

Return the sqrt root of $x$, assuming that $x$ represents a non-negative
number. Thus any negative number in the input interval is discarded.
"""
function sqrtpos(x::ArbFieldElem)
  z = parent(x)()
  @ccall libflint.arb_sqrtpos(z::Ref{ArbFieldElem}, x::Ref{ArbFieldElem}, parent(x).prec::Int)::Nothing
  return z
end

function log(x::ArbFieldElem)
  z = parent(x)()
  @ccall libflint.arb_log(z::Ref{ArbFieldElem}, x::Ref{ArbFieldElem}, parent(x).prec::Int)::Nothing
  return z
end

function log1p(x::ArbFieldElem)
  z = parent(x)()
  @ccall libflint.arb_log1p(z::Ref{ArbFieldElem}, x::Ref{ArbFieldElem}, parent(x).prec::Int)::Nothing
  return z
end

function Base.exp(x::ArbFieldElem)
  z = parent(x)()
  @ccall libflint.arb_exp(z::Ref{ArbFieldElem}, x::Ref{ArbFieldElem}, parent(x).prec::Int)::Nothing
  return z
end

function expm1(x::ArbFieldElem)
  z = parent(x)()
  @ccall libflint.arb_expm1(z::Ref{ArbFieldElem}, x::Ref{ArbFieldElem}, parent(x).prec::Int)::Nothing
  return z
end

function sin(x::ArbFieldElem)
  z = parent(x)()
  @ccall libflint.arb_sin(z::Ref{ArbFieldElem}, x::Ref{ArbFieldElem}, parent(x).prec::Int)::Nothing
  return z
end

function cos(x::ArbFieldElem)
  z = parent(x)()
  @ccall libflint.arb_cos(z::Ref{ArbFieldElem}, x::Ref{ArbFieldElem}, parent(x).prec::Int)::Nothing
  return z
end

function sinpi(x::ArbFieldElem)
  z = parent(x)()
  @ccall libflint.arb_sin_pi(z::Ref{ArbFieldElem}, x::Ref{ArbFieldElem}, parent(x).prec::Int)::Nothing
  return z
end

function cospi(x::ArbFieldElem)
  z = parent(x)()
  @ccall libflint.arb_cos_pi(z::Ref{ArbFieldElem}, x::Ref{ArbFieldElem}, parent(x).prec::Int)::Nothing
  return z
end

function tan(x::ArbFieldElem)
  z = parent(x)()
  @ccall libflint.arb_tan(z::Ref{ArbFieldElem}, x::Ref{ArbFieldElem}, parent(x).prec::Int)::Nothing
  return z
end

function cot(x::ArbFieldElem)
  z = parent(x)()
  @ccall libflint.arb_cot(z::Ref{ArbFieldElem}, x::Ref{ArbFieldElem}, parent(x).prec::Int)::Nothing
  return z
end

function tanpi(x::ArbFieldElem)
  z = parent(x)()
  @ccall libflint.arb_tan_pi(z::Ref{ArbFieldElem}, x::Ref{ArbFieldElem}, parent(x).prec::Int)::Nothing
  return z
end

function cotpi(x::ArbFieldElem)
  z = parent(x)()
  @ccall libflint.arb_cot_pi(z::Ref{ArbFieldElem}, x::Ref{ArbFieldElem}, parent(x).prec::Int)::Nothing
  return z
end

function sinh(x::ArbFieldElem)
  z = parent(x)()
  @ccall libflint.arb_sinh(z::Ref{ArbFieldElem}, x::Ref{ArbFieldElem}, parent(x).prec::Int)::Nothing
  return z
end

function cosh(x::ArbFieldElem)
  z = parent(x)()
  @ccall libflint.arb_cosh(z::Ref{ArbFieldElem}, x::Ref{ArbFieldElem}, parent(x).prec::Int)::Nothing
  return z
end

function tanh(x::ArbFieldElem)
  z = parent(x)()
  @ccall libflint.arb_tanh(z::Ref{ArbFieldElem}, x::Ref{ArbFieldElem}, parent(x).prec::Int)::Nothing
  return z
end

function coth(x::ArbFieldElem)
  z = parent(x)()
  @ccall libflint.arb_coth(z::Ref{ArbFieldElem}, x::Ref{ArbFieldElem}, parent(x).prec::Int)::Nothing
  return z
end

function atan(x::ArbFieldElem)
  z = parent(x)()
  @ccall libflint.arb_atan(z::Ref{ArbFieldElem}, x::Ref{ArbFieldElem}, parent(x).prec::Int)::Nothing
  return z
end

function asin(x::ArbFieldElem)
  z = parent(x)()
  @ccall libflint.arb_asin(z::Ref{ArbFieldElem}, x::Ref{ArbFieldElem}, parent(x).prec::Int)::Nothing
  return z
end

function acos(x::ArbFieldElem)
  z = parent(x)()
  @ccall libflint.arb_acos(z::Ref{ArbFieldElem}, x::Ref{ArbFieldElem}, parent(x).prec::Int)::Nothing
  return z
end

function atanh(x::ArbFieldElem)
  z = parent(x)()
  @ccall libflint.arb_atanh(z::Ref{ArbFieldElem}, x::Ref{ArbFieldElem}, parent(x).prec::Int)::Nothing
  return z
end

function asinh(x::ArbFieldElem)
  z = parent(x)()
  @ccall libflint.arb_asinh(z::Ref{ArbFieldElem}, x::Ref{ArbFieldElem}, parent(x).prec::Int)::Nothing
  return z
end

function acosh(x::ArbFieldElem)
  z = parent(x)()
  @ccall libflint.arb_acosh(z::Ref{ArbFieldElem}, x::Ref{ArbFieldElem}, parent(x).prec::Int)::Nothing
  return z
end

@doc raw"""
    gamma(x::ArbFieldElem)

Return the Gamma function evaluated at $x$.
"""
function gamma(x::ArbFieldElem)
  z = parent(x)()
  @ccall libflint.arb_gamma(z::Ref{ArbFieldElem}, x::Ref{ArbFieldElem}, parent(x).prec::Int)::Nothing
  return z
end

@doc raw"""
    lgamma(x::ArbFieldElem)

Return the logarithm of the Gamma function evaluated at $x$.
"""
function lgamma(x::ArbFieldElem)
  z = parent(x)()
  @ccall libflint.arb_lgamma(z::Ref{ArbFieldElem}, x::Ref{ArbFieldElem}, parent(x).prec::Int)::Nothing
  return z
end

@doc raw"""
    rgamma(x::ArbFieldElem)

Return the reciprocal of the Gamma function evaluated at $x$.
"""
function rgamma(x::ArbFieldElem)
  z = parent(x)()
  @ccall libflint.arb_rgamma(z::Ref{ArbFieldElem}, x::Ref{ArbFieldElem}, parent(x).prec::Int)::Nothing
  return z
end

@doc raw"""
    digamma(x::ArbFieldElem)

Return the  logarithmic derivative of the gamma function evaluated at $x$,
i.e. $\psi(x)$.
"""
function digamma(x::ArbFieldElem)
  z = parent(x)()
  @ccall libflint.arb_digamma(z::Ref{ArbFieldElem}, x::Ref{ArbFieldElem}, parent(x).prec::Int)::Nothing
  return z
end

@doc raw"""
    gamma(s::ArbFieldElem, x::ArbFieldElem)

Return the upper incomplete gamma function $\Gamma(s,x)$.
"""
function gamma(s::ArbFieldElem, x::ArbFieldElem)
  z = parent(s)()
  @ccall libflint.arb_hypgeom_gamma_upper(z::Ref{ArbFieldElem}, s::Ref{ArbFieldElem}, x::Ref{ArbFieldElem}, 0::Int, parent(s).prec::Int)::Nothing
  return z
end

@doc raw"""
    gamma_regularized(s::ArbFieldElem, x::ArbFieldElem)

Return the regularized upper incomplete gamma function
$\Gamma(s,x) / \Gamma(s)$.
"""
function gamma_regularized(s::ArbFieldElem, x::ArbFieldElem)
  z = parent(s)()
  @ccall libflint.arb_hypgeom_gamma_upper(z::Ref{ArbFieldElem}, s::Ref{ArbFieldElem}, x::Ref{ArbFieldElem}, 1::Int, parent(s).prec::Int)::Nothing
  return z
end

@doc raw"""
    gamma_lower(s::ArbFieldElem, x::ArbFieldElem)

Return the lower incomplete gamma function $\gamma(s,x) / \Gamma(s)$.
"""
function gamma_lower(s::ArbFieldElem, x::ArbFieldElem)
  z = parent(s)()
  @ccall libflint.arb_hypgeom_gamma_lower(z::Ref{ArbFieldElem}, s::Ref{ArbFieldElem}, x::Ref{ArbFieldElem}, 0::Int, parent(s).prec::Int)::Nothing
  return z
end

@doc raw"""
    gamma_lower_regularized(s::ArbFieldElem, x::ArbFieldElem)

Return the regularized lower incomplete gamma function
$\gamma(s,x) / \Gamma(s)$.
"""
function gamma_lower_regularized(s::ArbFieldElem, x::ArbFieldElem)
  z = parent(s)()
  @ccall libflint.arb_hypgeom_gamma_lower(z::Ref{ArbFieldElem}, s::Ref{ArbFieldElem}, x::Ref{ArbFieldElem}, 1::Int, parent(s).prec::Int)::Nothing
  return z
end


@doc raw"""
    zeta(x::ArbFieldElem)

Return the Riemann zeta function evaluated at $x$.
"""
function zeta(x::ArbFieldElem)
  z = parent(x)()
  @ccall libflint.arb_zeta(z::Ref{ArbFieldElem}, x::Ref{ArbFieldElem}, parent(x).prec::Int)::Nothing
  return z
end

function sincos(x::ArbFieldElem)
  s = parent(x)()
  c = parent(x)()
  @ccall libflint.arb_sin_cos(s::Ref{ArbFieldElem}, c::Ref{ArbFieldElem}, x::Ref{ArbFieldElem}, parent(x).prec::Int)::Nothing
  return (s, c)
end

function sincospi(x::ArbFieldElem)
  s = parent(x)()
  c = parent(x)()
  @ccall libflint.arb_sin_cos_pi(s::Ref{ArbFieldElem}, c::Ref{ArbFieldElem}, x::Ref{ArbFieldElem}, parent(x).prec::Int)::Nothing
  return (s, c)
end

function sinpi(x::QQFieldElem, r::ArbField)
  z = r()
  @ccall libflint.arb_sin_pi_fmpq(z::Ref{ArbFieldElem}, x::Ref{QQFieldElem}, precision(r)::Int)::Nothing
  return z
end

function cospi(x::QQFieldElem, r::ArbField)
  z = r()
  @ccall libflint.arb_cos_pi_fmpq(z::Ref{ArbFieldElem}, x::Ref{QQFieldElem}, precision(r)::Int)::Nothing
  return z
end

function sincospi(x::QQFieldElem, r::ArbField)
  s = r()
  c = r()
  @ccall libflint.arb_sin_cos_pi_fmpq(s::Ref{ArbFieldElem}, c::Ref{ArbFieldElem}, x::Ref{QQFieldElem}, precision(r)::Int)::Nothing
  return (s, c)
end

function sinhcosh(x::ArbFieldElem)
  s = parent(x)()
  c = parent(x)()
  @ccall libflint.arb_sinh_cosh(s::Ref{ArbFieldElem}, c::Ref{ArbFieldElem}, x::Ref{ArbFieldElem}, parent(x).prec::Int)::Nothing
  return (s, c)
end

function atan(y::ArbFieldElem, x::ArbFieldElem)
  z = parent(y)()
  @ccall libflint.arb_atan2(z::Ref{ArbFieldElem}, y::Ref{ArbFieldElem}, x::Ref{ArbFieldElem}, parent(y).prec::Int)::Nothing
  return z
end

@doc raw"""
    atan2(y::ArbFieldElem, x::ArbFieldElem)

Return $\operatorname{atan2}(y,x) = \arg(x+yi)$. Same as `atan(y, x)`.
"""
function atan2(y::ArbFieldElem, x::ArbFieldElem)
  return atan(y, x)
end

@doc raw"""
    agm(x::ArbFieldElem, y::ArbFieldElem)

Return the arithmetic-geometric mean of $x$ and $y$
"""
function agm(x::ArbFieldElem, y::ArbFieldElem)
  z = parent(x)()
  @ccall libflint.arb_agm(z::Ref{ArbFieldElem}, x::Ref{ArbFieldElem}, y::Ref{ArbFieldElem}, parent(x).prec::Int)::Nothing
  return z
end

@doc raw"""
    zeta(s::ArbFieldElem, a::ArbFieldElem)

Return the Hurwitz zeta function $\zeta(s,a)$.
"""
function zeta(s::ArbFieldElem, a::ArbFieldElem)
  z = parent(s)()
  @ccall libflint.arb_hurwitz_zeta(z::Ref{ArbFieldElem}, s::Ref{ArbFieldElem}, a::Ref{ArbFieldElem}, parent(s).prec::Int)::Nothing
  return z
end

function hypot(x::ArbFieldElem, y::ArbFieldElem)
  z = parent(x)()
  @ccall libflint.arb_hypot(z::Ref{ArbFieldElem}, x::Ref{ArbFieldElem}, y::Ref{ArbFieldElem}, parent(x).prec::Int)::Nothing
  return z
end

function root(x::ArbFieldElem, n::UInt)
  is_zero(x) && return x
  z = parent(x)()
  @ccall libflint.arb_root(z::Ref{ArbFieldElem}, x::Ref{ArbFieldElem}, n::UInt, parent(x).prec::Int)::Nothing
  return z
end

@doc raw"""
    root(x::ArbFieldElem, n::Int)

Return the $n$-th root of $x$. We require $x \geq 0$.
"""
function root(x::ArbFieldElem, n::Int)
  x < 0 && throw(DomainError(x, "Argument must be positive"))
  return root(x, UInt(n))
end

@doc raw"""
    factorial(x::ArbFieldElem)

Return the factorial of $x$.
"""
factorial(x::ArbFieldElem) = gamma(x+1)

function factorial(n::UInt, r::ArbField)
  z = r()
  @ccall libflint.arb_fac_ui(z::Ref{ArbFieldElem}, n::UInt, r.prec::Int)::Nothing
  return z
end

@doc raw"""
    factorial(n::Int, r::ArbField)

Return the factorial of $n$ in the given Arb field.
"""
factorial(n::Int, r::ArbField) = n < 0 ? factorial(r(n)) : factorial(UInt(n), r)

@doc raw"""
    binomial(x::ArbFieldElem, n::UInt)

Return the binomial coefficient ${x \choose n}$.
"""
function binomial(x::ArbFieldElem, n::UInt)
  z = parent(x)()
  @ccall libflint.arb_bin_ui(z::Ref{ArbFieldElem}, x::Ref{ArbFieldElem}, n::UInt, parent(x).prec::Int)::Nothing
  return z
end

@doc raw"""
    binomial(n::UInt, k::UInt, r::ArbField)

Return the binomial coefficient ${n \choose k}$ in the given Arb field.
"""
function binomial(n::UInt, k::UInt, r::ArbField)
  z = r()
  @ccall libflint.arb_bin_uiui(z::Ref{ArbFieldElem}, n::UInt, k::UInt, r.prec::Int)::Nothing
  return z
end

@doc raw"""
    fibonacci(n::ZZRingElem, r::ArbField)

Return the $n$-th Fibonacci number in the given Arb field.
"""
function fibonacci(n::ZZRingElem, r::ArbField)
  z = r()
  @ccall libflint.arb_fib_fmpz(z::Ref{ArbFieldElem}, n::Ref{ZZRingElemRaw}, r.prec::Int)::Nothing
  return z
end

function fibonacci(n::UInt, r::ArbField)
  z = r()
  @ccall libflint.arb_fib_ui(z::Ref{ArbFieldElem}, n::UInt, r.prec::Int)::Nothing
  return z
end

@doc raw"""
    fibonacci(n::Int, r::ArbField)

Return the $n$-th Fibonacci number in the given Arb field.
"""
fibonacci(n::Int, r::ArbField) = n >= 0 ? fibonacci(UInt(n), r) : fibonacci(ZZRingElem(n), r)

@doc raw"""
    gamma(x::ZZRingElem, r::ArbField)

Return the Gamma function evaluated at $x$ in the given Arb field.
"""
function gamma(x::ZZRingElem, r::ArbField)
  z = r()
  @ccall libflint.arb_gamma_fmpz(z::Ref{ArbFieldElem}, x::Ref{ZZRingElemRaw}, r.prec::Int)::Nothing
  return z
end

@doc raw"""
    gamma(x::QQFieldElem, r::ArbField)

Return the Gamma function evaluated at $x$ in the given Arb field.
"""
function gamma(x::QQFieldElem, r::ArbField)
  z = r()
  @ccall libflint.arb_gamma_fmpq(z::Ref{ArbFieldElem}, x::Ref{QQFieldElem}, r.prec::Int)::Nothing
  return z
end


function zeta(n::UInt, r::ArbField)
  z = r()
  @ccall libflint.arb_zeta_ui(z::Ref{ArbFieldElem}, n::UInt, r.prec::Int)::Nothing
  return z
end

@doc raw"""
    zeta(n::Int, r::ArbField)

Return the Riemann zeta function $\zeta(n)$ as an element of the given Arb
field.
"""
zeta(n::Int, r::ArbField) = n >= 0 ? zeta(UInt(n), r) : zeta(r(n))

function bernoulli(n::UInt, r::ArbField)
  z = r()
  @ccall libflint.arb_bernoulli_ui(z::Ref{ArbFieldElem}, n::UInt, r.prec::Int)::Nothing
  return z
end

@doc raw"""
    bernoulli(n::Int, r::ArbField)

Return the $n$-th Bernoulli number as an element of the given Arb field.
"""
bernoulli(n::Int, r::ArbField) = n >= 0 ? bernoulli(UInt(n), r) : throw(DomainError(n, "Index must be non-negative"))

function rising_factorial(x::ArbFieldElem, n::UInt)
  z = parent(x)()
  @ccall libflint.arb_rising_ui(z::Ref{ArbFieldElem}, x::Ref{ArbFieldElem}, n::UInt, parent(x).prec::Int)::Nothing
  return z
end

@doc raw"""
    rising_factorial(x::ArbFieldElem, n::Int)

Return the rising factorial $x(x + 1)\ldots (x + n - 1)$ as an Arb.
"""
rising_factorial(x::ArbFieldElem, n::Int) = n < 0 ? throw(DomainError(n, "Index must be non-negative")) : rising_factorial(x, UInt(n))

function rising_factorial(x::QQFieldElem, n::UInt, r::ArbField)
  z = r()
  @ccall libflint.arb_rising_fmpq_ui(z::Ref{ArbFieldElem}, x::Ref{QQFieldElem}, n::UInt, r.prec::Int)::Nothing
  return z
end

@doc raw"""
    rising_factorial(x::QQFieldElem, n::Int, r::ArbField)

Return the rising factorial $x(x + 1)\ldots (x + n - 1)$ as an element of the
given Arb field.
"""
rising_factorial(x::QQFieldElem, n::Int, r::ArbField) = n < 0 ? throw(DomainError(n, "Index must be non-negative")) : rising_factorial(x, UInt(n), r)

function rising_factorial2(x::ArbFieldElem, n::UInt)
  z = parent(x)()
  w = parent(x)()
  @ccall libflint.arb_rising2_ui(z::Ref{ArbFieldElem}, w::Ref{ArbFieldElem}, x::Ref{ArbFieldElem}, n::UInt, parent(x).prec::Int)::Nothing
  return (z, w)
end

@doc raw"""
    rising_factorial2(x::ArbFieldElem, n::Int)

Return a tuple containing the rising factorial $x(x + 1)\ldots (x + n - 1)$
and its derivative.
"""
rising_factorial2(x::ArbFieldElem, n::Int) = n < 0 ? throw(DomainError(n, "Index must be non-negative")) : rising_factorial2(x, UInt(n))

function polylog(s::ArbFieldElem, a::ArbFieldElem)
  z = parent(s)()
  @ccall libflint.arb_polylog(z::Ref{ArbFieldElem}, s::Ref{ArbFieldElem}, a::Ref{ArbFieldElem}, parent(s).prec::Int)::Nothing
  return z
end

function polylog(s::Int, a::ArbFieldElem)
  z = parent(a)()
  @ccall libflint.arb_polylog_si(z::Ref{ArbFieldElem}, s::Int, a::Ref{ArbFieldElem}, parent(a).prec::Int)::Nothing
  return z
end

@doc raw"""
    polylog(s::Union{ArbFieldElem,Int}, a::ArbFieldElem)

Return the polylogarithm Li$_s(a)$.
""" polylog(s::Union{ArbFieldElem,Int}, a::ArbFieldElem)

function chebyshev_t(n::UInt, x::ArbFieldElem)
  z = parent(x)()
  @ccall libflint.arb_chebyshev_t_ui(z::Ref{ArbFieldElem}, n::UInt, x::Ref{ArbFieldElem}, parent(x).prec::Int)::Nothing
  return z
end

function chebyshev_u(n::UInt, x::ArbFieldElem)
  z = parent(x)()
  @ccall libflint.arb_chebyshev_u_ui(z::Ref{ArbFieldElem}, n::UInt, x::Ref{ArbFieldElem}, parent(x).prec::Int)::Nothing
  return z
end

function chebyshev_t2(n::UInt, x::ArbFieldElem)
  z = parent(x)()
  w = parent(x)()
  @ccall libflint.arb_chebyshev_t2_ui(z::Ref{ArbFieldElem}, w::Ref{ArbFieldElem}, n::UInt, x::Ref{ArbFieldElem}, parent(x).prec::Int)::Nothing
  return z, w
end

function chebyshev_u2(n::UInt, x::ArbFieldElem)
  z = parent(x)()
  w = parent(x)()
  @ccall libflint.arb_chebyshev_u2_ui(z::Ref{ArbFieldElem}, w::Ref{ArbFieldElem}, n::UInt, x::Ref{ArbFieldElem}, parent(x).prec::Int)::Nothing
  return z, w
end

@doc raw"""
    chebyshev_t(n::Int, x::ArbFieldElem)

Return the value of the Chebyshev polynomial $T_n(x)$.
"""
chebyshev_t(n::Int, x::ArbFieldElem) = n < 0 ? throw(DomainError(n, "Index must be non-negative")) : chebyshev_t(UInt(n), x)

@doc raw"""
    chebyshev_u(n::Int, x::ArbFieldElem)

Return the value of the Chebyshev polynomial $U_n(x)$.
"""
chebyshev_u(n::Int, x::ArbFieldElem) = n < 0 ? throw(DomainError(n, "Index must be non-negative")) : chebyshev_u(UInt(n), x)

@doc raw"""
    chebyshev_t2(n::Int, x::ArbFieldElem)

Return the tuple $(T_{n}(x), T_{n-1}(x))$.
"""
chebyshev_t2(n::Int, x::ArbFieldElem) = n < 0 ? throw(DomainError(n, "Index must be non-negative")) : chebyshev_t2(UInt(n), x)

@doc raw"""
    chebyshev_u2(n::Int, x::ArbFieldElem)

Return the tuple $(U_{n}(x), U_{n-1}(x))$
"""
chebyshev_u2(n::Int, x::ArbFieldElem) = n < 0 ? throw(DomainError(n, "Index must be non-negative")) : chebyshev_u2(UInt(n), x)

@doc raw"""
    bell(n::ZZRingElem, r::ArbField)

Return the Bell number $B_n$ as an element of $r$.
"""
function bell(n::ZZRingElem, r::ArbField)
  z = r()
  @ccall libflint.arb_bell_fmpz(z::Ref{ArbFieldElem}, n::Ref{ZZRingElemRaw}, r.prec::Int)::Nothing
  return z
end

@doc raw"""
    bell(n::Int, r::ArbField)

Return the Bell number $B_n$ as an element of $r$.
"""
bell(n::Int, r::ArbField) = bell(ZZRingElem(n), r)

@doc raw"""
    numpart(n::ZZRingElem, r::ArbField)

Return the number of partitions $p(n)$ as an element of $r$.
"""
function numpart(n::ZZRingElem, r::ArbField)
  z = r()
  @ccall libflint.arb_partitions_fmpz(z::Ref{ArbFieldElem}, n::Ref{ZZRingElemRaw}, r.prec::Int)::Nothing
  return z
end

@doc raw"""
    numpart(n::Int, r::ArbField)

Return the number of partitions $p(n)$ as an element of $r$.
"""
numpart(n::Int, r::ArbField) = numpart(ZZRingElem(n), r)

################################################################################
#
#  Hypergeometric and related functions
#
################################################################################

@doc raw"""
    airy_ai(x::ArbFieldElem)

Return the Airy function $\operatorname{Ai}(x)$.
"""
function airy_ai(x::ArbFieldElem)
  ai = parent(x)()
  @ccall libflint.arb_hypgeom_airy(ai::Ref{ArbFieldElem}, C_NULL::Ptr{Cvoid}, C_NULL::Ptr{Cvoid}, C_NULL::Ptr{Cvoid}, x::Ref{ArbFieldElem}, parent(x).prec::Int)::Nothing
  return ai
end

@doc raw"""
    airy_bi(x::ArbFieldElem)

Return the Airy function $\operatorname{Bi}(x)$.
"""
function airy_bi(x::ArbFieldElem)
  bi = parent(x)()
  @ccall libflint.arb_hypgeom_airy(C_NULL::Ptr{Cvoid}, C_NULL::Ptr{Cvoid}, bi::Ref{ArbFieldElem}, C_NULL::Ptr{Cvoid}, x::Ref{ArbFieldElem}, parent(x).prec::Int)::Nothing
  return bi
end

@doc raw"""
    airy_ai_prime(x::ArbFieldElem)

Return the derivative of the Airy function $\operatorname{Ai}^\prime(x)$.
"""
function airy_ai_prime(x::ArbFieldElem)
  ai_prime = parent(x)()
  @ccall libflint.arb_hypgeom_airy(C_NULL::Ptr{Cvoid}, ai_prime::Ref{ArbFieldElem}, C_NULL::Ptr{Cvoid}, C_NULL::Ptr{Cvoid}, x::Ref{ArbFieldElem}, parent(x).prec::Int)::Nothing
  return ai_prime
end

@doc raw"""
    airy_bi_prime(x::ArbFieldElem)

Return the derivative of the Airy function $\operatorname{Bi}^\prime(x)$.
"""
function airy_bi_prime(x::ArbFieldElem)
  bi_prime = parent(x)()
  @ccall libflint.arb_hypgeom_airy(C_NULL::Ptr{Cvoid}, C_NULL::Ptr{Cvoid}, C_NULL::Ptr{Cvoid}, bi_prime::Ref{ArbFieldElem}, x::Ref{ArbFieldElem}, parent(x).prec::Int)::Nothing
  return bi_prime
end

################################################################################
#
#  Linear dependence
#
################################################################################

@doc raw"""
    lindep(A::Vector{ArbFieldElem}, bits::Int)

Find a small linear combination of the entries of the array $A$ that is small
(using LLL). The entries are first scaled by the given number of bits before
truncating to integers for use in LLL. This function can be used to find linear
dependence between a list of real numbers. The algorithm is heuristic only and
returns an array of Nemo integers representing the linear combination.

# Examples

```jldoctest
julia> RR = ArbField(64)
Real Field with 64 bits of precision and error bounds

julia> a = RR(-0.33198902958450931620250069492231652319)
[-0.33198902958450932088 +/- 4.15e-22]

julia> V = [RR(1), a, a^2, a^3, a^4, a^5]
6-element Vector{ArbFieldElem}:
 1.0000000000000000000
 [-0.33198902958450932088 +/- 4.15e-22]
 [0.11021671576446420510 +/- 7.87e-21]
 [-0.03659074051063616184 +/- 4.17e-21]
 [0.012147724433904692427 +/- 4.99e-22]
 [-0.004032911246472051677 +/- 6.25e-22]

julia> W = lindep(V, 20)
6-element Vector{ZZRingElem}:
 1
 3
 0
 0
 0
 1
```
"""
function lindep(A::Vector{ArbFieldElem}, bits::Int)
  bits < 0 && throw(DomainError(bits, "Number of bits must be non-negative"))
  n = length(A)
  V = [floor(ldexp(s, bits) + 0.5) for s in A]
  M = zero_matrix(ZZ, n, n + 1)
  for i = 1:n
    M[i, i] = ZZ(1)
    flag, M[i, n + 1] = unique_integer(V[i])
    !flag && error("Insufficient precision in lindep")
  end
  L = lll(M)
  return [L[1, i] for i = 1:n]
end

################################################################################
#
#  Simplest rational inside
#
################################################################################

@doc raw"""
      simplest_rational_inside(x::ArbFieldElem)

Return the simplest fraction inside the ball $x$. A canonical fraction
$a_1/b_1$ is defined to be simpler than $a_2/b_2$ iff $b_1 < b_2$ or $b_1 =
b_2$ and $a_1 < a_2$.

# Examples

```jldoctest
julia> RR = ArbField(64)
Real Field with 64 bits of precision and error bounds

julia> simplest_rational_inside(const_pi(RR))
8717442233//2774848045
```
"""
function simplest_rational_inside(x::ArbFieldElem)
  a = ZZRingElem()
  b = ZZRingElem()
  e = ZZRingElem()

  @ccall libflint.arb_get_interval_fmpz_2exp(a::Ref{ZZRingElemRaw}, b::Ref{ZZRingElemRaw}, e::Ref{ZZRingElemRaw}, x::Ref{ArbFieldElem})::Nothing
  !fits(Int, e) && error("Result does not fit into an QQFieldElem")
  _e = Int(e)
  if e >= 0
    return a << _e
  end
  _e = -_e
  d = ZZRingElem(1) << _e
  return _fmpq_simplest_between(a, d, b, d)
end

################################################################################
#
#  Unsafe operations
#
################################################################################

function zero!(z::Union{ArbFieldElemOrPtr})
  @ccall libflint.arb_zero(z::Ref{ArbFieldElem})::Nothing
  return z
end

function one!(z::ArbFieldElemOrPtr)
  @ccall libflint.arb_one(z::Ref{ArbFieldElem})::Nothing
  return z
end

function neg!(z::ArbFieldElemOrPtr, a::ArbFieldElemOrPtr)
  @ccall libflint.arb_neg(z::Ref{ArbFieldElem}, a::Ref{ArbFieldElem})::Nothing
  return z
end

for (s,f) in (("add!","arb_add"), ("mul!","arb_mul"), ("div!", "arb_div"),
              ("sub!","arb_sub"))
  @eval begin
    function ($(Symbol(s)))(z::ArbFieldElem, x::ArbFieldElem, y::ArbFieldElem)
      ccall(($f, libflint), Nothing, (Ref{ArbFieldElem}, Ref{ArbFieldElem}, Ref{ArbFieldElem}, Int),
            z, x, y, parent(x).prec)
      return z
    end
  end
end

function addmul!(z::ArbFieldElem, x::ArbFieldElem, y::ZZRingElem)
  q = max(bits(z), bits(x))
  @ccall libflint.arb_addmul_fmpz(z::Ref{ArbFieldElem}, x::Ref{ArbFieldElem}, y::Ref{ZZRingElemRaw}, q::Int)::Nothing
  return z
end

################################################################################
#
#  Unsafe setting
#
################################################################################

function _arb_set(x::ArbFieldElemOrPtr, y::Int)
  @ccall libflint.arb_set_si(x::Ref{ArbFieldElem}, y::Int)::Nothing
end

function _arb_set(x::ArbFieldElemOrPtr, y::UInt)
  @ccall libflint.arb_set_ui(x::Ref{ArbFieldElem}, y::UInt)::Nothing
end

function _arb_set(x::ArbFieldElemOrPtr, y::Float64)
  @ccall libflint.arb_set_d(x::Ref{ArbFieldElem}, y::Float64)::Nothing
end

function _arb_set(x::ArbFieldElemOrPtr, y::Union{Int,UInt,Float64}, p::Int)
  _arb_set(x, y)
  @ccall libflint.arb_set_round(x::Ref{ArbFieldElem}, x::Ref{ArbFieldElem}, p::Int)::Nothing
end

function _arb_set(x::ArbFieldElemOrPtr, y::ZZRingElem)
  @ccall libflint.arb_set_fmpz(x::Ref{ArbFieldElem}, y::Ref{ZZRingElemRaw})::Nothing
end

function _arb_set(x::ArbFieldElemOrPtr, y::ZZRingElem, p::Int)
  @ccall libflint.arb_set_round_fmpz(x::Ref{ArbFieldElem}, y::Ref{ZZRingElemRaw}, p::Int)::Nothing
end

function _arb_set(x::ArbFieldElemOrPtr, y::QQFieldElem, p::Int)
  @ccall libflint.arb_set_fmpq(x::Ref{ArbFieldElem}, y::Ref{QQFieldElem}, p::Int)::Nothing
end

function _arb_set(x::ArbFieldElemOrPtr, y::ArbFieldElemOrPtr)
  @ccall libflint.arb_set(x::Ref{ArbFieldElem}, y::Ref{ArbFieldElem})::Nothing
end

function _arb_set(x::ArbFieldElemOrPtr, y::Ptr{arb_struct})
  @ccall libflint.arb_set(x::Ref{ArbFieldElem}, y::Ptr{arb_struct})::Nothing
end

function _arb_set(x::Ptr{arb_struct}, y::ArbFieldElemOrPtr)
  @ccall libflint.arb_set(x::Ptr{arb_struct}, y::Ref{ArbFieldElem})::Nothing
end

function _arb_set(x::ArbFieldElemOrPtr, y::ArbFieldElemOrPtr, p::Int)
  @ccall libflint.arb_set_round(x::Ref{ArbFieldElem}, y::Ref{ArbFieldElem}, p::Int)::Nothing
end

function _arb_set(x::ArbFieldElemOrPtr, y::AbstractString, p::Int)
  s = string(y)
  err = @ccall libflint.arb_set_str(x::Ref{ArbFieldElem}, s::Ptr{UInt8}, p::Int)::Int32
  err == 0 || error("Invalid real string: $(repr(s))")
end

function _arb_set(x::ArbFieldElemOrPtr, y::BigFloat)
  m = _mid_ptr(x)
  r = _rad_ptr(x)
  @ccall libflint.arf_set_mpfr(m::Ptr{arf_struct}, y::Ref{BigFloat})::Nothing
  @ccall libflint.mag_zero(r::Ptr{mag_struct})::Nothing
end

function _arb_set(x::ArbFieldElemOrPtr, y::BigFloat, p::Int)
  m = _mid_ptr(x)
  r = _rad_ptr(x)
  @ccall libflint.arf_set_mpfr(m::Ptr{arf_struct}, y::Ref{BigFloat})::Nothing
  @ccall libflint.mag_zero(r::Ptr{mag_struct})::Nothing
  @ccall libflint.arb_set_round(x::Ref{ArbFieldElem}, x::Ref{ArbFieldElem}, p::Int)::Nothing
end

function _arb_set(x::ArbFieldElemOrPtr, y::Integer)
  _arb_set(x, ZZRingElem(y))
end

function _arb_set(x::ArbFieldElemOrPtr, y::Integer, p::Int)
  _arb_set(x, ZZRingElem(y), p)
end

function _arb_set(x::ArbFieldElemOrPtr, y::Real)
  _arb_set(x, BigFloat(y))
end

function _arb_set(x::ArbFieldElemOrPtr, y::Real, p::Int)
  _arb_set(x, BigFloat(y), p)
end

################################################################################
#
#  Parent object overloading
#
################################################################################

function (r::ArbField)()
  z = ArbFieldElem()
  z.parent = r
  return z
end

function (r::ArbField)(x::Any)
  z = ArbFieldElem(x, r.prec)
  z.parent = r
  return z
end

function (r::ArbField)(x::Irrational)
  if x == pi
    return const_pi(r)
  elseif x == MathConstants.e
    return const_e(r)
  elseif x == MathConstants.catalan
    return const_catalan(r)
  elseif x == MathConstants.eulergamma
    return const_euler(r)
  else
    error("constant not supported")
  end
end

################################################################################
#
#  Arb real field constructor
#
################################################################################

# see inner constructor for ArbField

################################################################################
#
#  Random generation
#
################################################################################

@doc raw"""
    rand(r::ArbField; randtype::Symbol=:urandom)

Return a random element in given Arb field.

The `randtype` default is `:urandom` which return an `ArbFieldElem` contained in
$[0,1]$.

The rest of the methods return non-uniformly distributed values in order to
exercise corner cases. The option `:randtest` will return a finite number, and
`:randtest_exact` the same but with a zero radius. The option
`:randtest_precise` return an `ArbFieldElem` with a radius around $2^{-\mathrm{prec}}$
the magnitude of the midpoint, while `:randtest_wide` return a radius that
might be big relative to its midpoint. The `:randtest_special`-option might
return a midpoint and radius whose values are `NaN` or `inf`.
"""
function rand(r::ArbField; randtype::Symbol=:urandom)
  state = _flint_rand_states[Threads.threadid()]
  x = r()

  if randtype == :urandom
    @ccall libflint.arb_urandom(x::Ref{ArbFieldElem}, state::Ref{rand_ctx}, r.prec::Int)::Nothing
  elseif randtype == :randtest
    @ccall libflint.arb_randtest(x::Ref{ArbFieldElem}, state::Ref{rand_ctx}, r.prec::Int, 30::Int)::Nothing
  elseif randtype == :randtest_exact
    @ccall libflint.arb_randtest_exact(x::Ref{ArbFieldElem}, state::Ref{rand_ctx}, r.prec::Int, 30::Int)::Nothing
  elseif randtype == :randtest_precise
    @ccall libflint.arb_randtest_precise(x::Ref{ArbFieldElem}, state::Ref{rand_ctx}, r.prec::Int, 30::Int)::Nothing
  elseif randtype == :randtest_wide
    @ccall libflint.arb_randtest_wide(x::Ref{ArbFieldElem}, state::Ref{rand_ctx}, r.prec::Int, 30::Int)::Nothing
  elseif randtype == :randtest_special
    @ccall libflint.arb_randtest_special(x::Ref{ArbFieldElem}, state::Ref{rand_ctx}, r.prec::Int, 30::Int)::Nothing
  else
    error("Arb random generation `" * String(randtype) * "` is not defined")
  end

  return x
end
