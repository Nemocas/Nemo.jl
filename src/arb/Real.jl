###############################################################################
#
#   Real.jl : Arb real numbers
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

elem_type(::Type{RealField}) = RealFieldElem

parent_type(::Type{RealFieldElem}) = RealField

base_ring_type(::Type{RealField}) = typeof(Union{})

base_ring(R::RealField) = Union{}

parent(x::RealFieldElem) = RealField()

is_domain_type(::Type{RealFieldElem}) = true

is_exact_type(::Type{RealFieldElem}) = false

zero(R::RealField) = R(0)

one(R::RealField) = R(1)

@doc raw"""
    accuracy_bits(x::RealFieldElem)

Return the relative accuracy of $x$ measured in bits, capped between
`typemax(Int)` and `-typemax(Int)`.
"""
function accuracy_bits(x::RealFieldElem)
  return @ccall libflint.arb_rel_accuracy_bits(x::Ref{RealFieldElem})::Int
end

function deepcopy_internal(a::RealFieldElem, dict::IdDict)
  b = parent(a)()
  _arb_set(b, a)
  return b
end

function canonical_unit(x::RealFieldElem)
  return x
end

characteristic(::RealField) = 0

_mid_ptr(x::RealFieldElemOrPtr) = @ccall libflint.arb_mid_ptr(x::Ref{RealFieldElem})::Ptr{arf_struct}
_rad_ptr(x::RealFieldElemOrPtr) = @ccall libflint.arb_rad_ptr(x::Ref{RealFieldElem})::Ptr{mag_struct}

################################################################################
#
#  Conversions
#
################################################################################

@doc raw"""
    Float64(x::RealFieldElem, round::RoundingMode=RoundNearest)

Converts $x$ to a `Float64`, rounded in the direction specified by $round$.
For `RoundNearest` the return value approximates the midpoint of $x$. For
`RoundDown` or `RoundUp` the return value is a lower bound or upper bound for
all values in $x$.
"""
function Base.Float64(x::RealFieldElem, round::RoundingMode=RoundNearest)
  t = _arb_get_arf(x, round)
  return _arf_get_d(t, round)
end

@doc raw"""
    BigFloat(x::RealFieldElem, round::RoundingMode=RoundNearest)

Converts $x$ to a `BigFloat` of the currently used precision, rounded in the
direction specified by $round$. For `RoundNearest` the return value
approximates the midpoint of $x$. For `RoundDown` or `RoundUp` the return
value is a lower bound or upper bound for all values in $x$.
"""
function Base.BigFloat(x::RealFieldElem, round::RoundingMode=RoundNearest)
  t = _arb_get_arf(x, round)
  return _arf_get_mpfr(t, round)
end

function _arb_get_arf(x::RealFieldElem, ::RoundingMode{:Nearest})
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
    function _arb_get_arf(x::RealFieldElem, ::$b, prec::Int = precision(Balls))
      t = arf_struct()
      @ccall libflint.$f(t::Ref{arf_struct}, x::Ref{RealFieldElem}, prec::Int)::Nothing
      return t
    end
  end
end

function convert(::Type{Float64}, x::RealFieldElem)
  return Float64(x)
end

function convert(::Type{BigFloat}, x::RealFieldElem)
  return BigFloat(x)
end

@doc raw"""
    ZZRingElem(x::RealFieldElem)

Return $x$ as an `ZZRingElem` if it represents an unique integer, else throws an
error.
"""
function ZZRingElem(x::RealFieldElem)
  if is_exact(x)
    ok, z = unique_integer(x)
    ok && return z
  end
  error("Argument must represent a unique integer")
end

Base.BigInt(x::RealFieldElem) = BigInt(ZZRingElem(x))

function (::Type{T})(x::RealFieldElem) where {T <: Integer}
  typemin(T) <= x <= typemax(T) ||
  error("Argument does not fit inside datatype.")
  return T(ZZRingElem(x))
end

################################################################################
#
#  String I/O
#
################################################################################

function native_string(x::RealFieldElem)
  d = ceil(precision(Balls) * 0.30102999566398119521)
  cstr = @ccall libflint.arb_get_str(x::Ref{RealFieldElem}, Int(d)::Int, UInt(0)::UInt)::Ptr{UInt8}
  res = unsafe_string(cstr)
  @ccall libflint.flint_free(cstr::Ptr{UInt8})::Nothing
  return res
end

function expressify(x::RealFieldElem; context = nothing)
  if is_exact(x) && is_negative(x)
    # TODO is_exact does not imply it is printed without radius
    return Expr(:call, :-, native_string(-x))
  else
    return native_string(x)
  end
end

function show(io::IO, x::RealField)
  # deliberately no @show_name or @show_special here as this is a singleton type
  if is_terse(io)
    print(io, LowercaseOff(), "RR")
  else
    print(io, "Real field")
  end
end

function show(io::IO, x::RealFieldElem)
  print(io, native_string(x))
end

################################################################################
#
#  Containment
#
################################################################################

@doc raw"""
    overlaps(x::RealFieldElem, y::RealFieldElem)

Returns `true` if any part of the ball $x$ overlaps any part of the ball $y$,
otherwise return `false`.
"""
function overlaps(x::RealFieldElem, y::RealFieldElem)
  r = @ccall libflint.arb_overlaps(x::Ref{RealFieldElem}, y::Ref{RealFieldElem})::Cint
  return Bool(r)
end

#function contains(x::RealFieldElem, y::arf)
#  r = @ccall libflint.arb_contains_arf(x::Ref{RealFieldElem}, y::Ref{arf})::Cint
#  return Bool(r)
#end

@doc raw"""
    contains(x::RealFieldElem, y::QQFieldElem)

Returns `true` if the ball $x$ contains the given rational value, otherwise
return `false`.
"""
function contains(x::RealFieldElem, y::QQFieldElem)
  r = @ccall libflint.arb_contains_fmpq(x::Ref{RealFieldElem}, y::Ref{QQFieldElem})::Cint
  return Bool(r)
end

@doc raw"""
    contains(x::RealFieldElem, y::ZZRingElem)

Returns `true` if the ball $x$ contains the given integer value, otherwise
return `false`.
"""
function contains(x::RealFieldElem, y::ZZRingElem)
  r = @ccall libflint.arb_contains_fmpz(x::Ref{RealFieldElem}, y::Ref{ZZRingElem})::Cint
  return Bool(r)
end

function contains(x::RealFieldElem, y::Int)
  r = @ccall libflint.arb_contains_si(x::Ref{RealFieldElem}, y::Int)::Cint
  return Bool(r)
end

@doc raw"""
    contains(x::RealFieldElem, y::Integer)

Returns `true` if the ball $x$ contains the given integer value, otherwise
return `false`.
"""
contains(x::RealFieldElem, y::Integer) = contains(x, ZZRingElem(y))

@doc raw"""
    contains(x::RealFieldElem, y::Rational{T}) where {T <: Integer}

Returns `true` if the ball $x$ contains the given rational value, otherwise
return `false`.
"""
contains(x::RealFieldElem, y::Rational{T}) where {T <: Integer} = contains(x, QQFieldElem(y))

@doc raw"""
    contains(x::RealFieldElem, y::BigFloat)

Returns `true` if the ball $x$ contains the given floating point value,
otherwise return `false`.
"""
function contains(x::RealFieldElem, y::BigFloat)
  r = @ccall libflint.arb_contains_mpfr(x::Ref{RealFieldElem}, y::Ref{BigFloat})::Cint
  return Bool(r)
end

@doc raw"""
    contains(x::RealFieldElem, y::RealFieldElem)

Returns `true` if the ball $x$ contains the ball $y$, otherwise return
`false`.
"""
function contains(x::RealFieldElem, y::RealFieldElem)
  r = @ccall libflint.arb_contains(x::Ref{RealFieldElem}, y::Ref{RealFieldElem})::Cint
  return Bool(r)
end

@doc raw"""
    contains_zero(x::RealFieldElem)

Returns `true` if the ball $x$ contains zero, otherwise return `false`.
"""
function contains_zero(x::RealFieldElem)
  r = @ccall libflint.arb_contains_zero(x::Ref{RealFieldElem})::Cint
  return Bool(r)
end

@doc raw"""
    contains_negative(x::RealFieldElem)

Returns `true` if the ball $x$ contains any negative value, otherwise return
`false`.
"""
function contains_negative(x::RealFieldElem)
  r = @ccall libflint.arb_contains_negative(x::Ref{RealFieldElem})::Cint
  return Bool(r)
end

@doc raw"""
    contains_positive(x::RealFieldElem)

Returns `true` if the ball $x$ contains any positive value, otherwise return
`false`.
"""
function contains_positive(x::RealFieldElem)
  r = @ccall libflint.arb_contains_positive(x::Ref{RealFieldElem})::Cint
  return Bool(r)
end

@doc raw"""
    contains_nonnegative(x::RealFieldElem)

Returns `true` if the ball $x$ contains any non-negative value, otherwise
return `false`.
"""
function contains_nonnegative(x::RealFieldElem)
  r = @ccall libflint.arb_contains_nonnegative(x::Ref{RealFieldElem})::Cint
  return Bool(r)
end

@doc raw"""
    contains_nonpositive(x::RealFieldElem)

Returns `true` if the ball $x$ contains any nonpositive value, otherwise
return `false`.
"""
function contains_nonpositive(x::RealFieldElem)
  r = @ccall libflint.arb_contains_nonpositive(x::Ref{RealFieldElem})::Cint
  return Bool(r)
end

################################################################################
#
#  Comparison
#
################################################################################

@doc raw"""
    isequal(x::RealFieldElem, y::RealFieldElem)

Return `true` if the balls $x$ and $y$ are precisely equal, i.e. have the
same midpoints and radii.
"""
function isequal(x::RealFieldElem, y::RealFieldElem)
  r = @ccall libflint.arb_equal(x::Ref{RealFieldElem}, y::Ref{RealFieldElem})::Cint
  return Bool(r)
end

function Base.hash(x::RealFieldElem, h::UInt)
  # TODO: improve me
  return h
end

function ==(x::RealFieldElem, y::RealFieldElem)
  return Bool(@ccall libflint.arb_eq(x::Ref{RealFieldElem}, y::Ref{RealFieldElem})::Cint)
end

function !=(x::RealFieldElem, y::RealFieldElem)
  return Bool(@ccall libflint.arb_ne(x::Ref{RealFieldElem}, y::Ref{RealFieldElem})::Cint)
end

function isless(x::RealFieldElem, y::RealFieldElem)
  return Bool(@ccall libflint.arb_lt(x::Ref{RealFieldElem}, y::Ref{RealFieldElem})::Cint)
end

function <=(x::RealFieldElem, y::RealFieldElem)
  return Bool(@ccall libflint.arb_le(x::Ref{RealFieldElem}, y::Ref{RealFieldElem})::Cint)
end

==(x::RealFieldElem, y::Int) = x == RealFieldElem(y)
!=(x::RealFieldElem, y::Int) = x != RealFieldElem(y)
<=(x::RealFieldElem, y::Int) = x <= RealFieldElem(y)
<(x::RealFieldElem, y::Int) = x < RealFieldElem(y)

==(x::Int, y::RealFieldElem) = RealFieldElem(x) == y
!=(x::Int, y::RealFieldElem) = RealFieldElem(x) != y
<=(x::Int, y::RealFieldElem) = RealFieldElem(x) <= y
<(x::Int, y::RealFieldElem) = RealFieldElem(x) < y

==(x::RealFieldElem, y::ZZRingElem) = x == RealFieldElem(y)
!=(x::RealFieldElem, y::ZZRingElem) = x != RealFieldElem(y)
<=(x::RealFieldElem, y::ZZRingElem) = x <= RealFieldElem(y)
<(x::RealFieldElem, y::ZZRingElem) = x < RealFieldElem(y)

==(x::ZZRingElem, y::RealFieldElem) = RealFieldElem(x) == y
!=(x::ZZRingElem, y::RealFieldElem) = RealFieldElem(x) != y
<=(x::ZZRingElem, y::RealFieldElem) = RealFieldElem(x) <= y
<(x::ZZRingElem, y::RealFieldElem) = RealFieldElem(x) < y

==(x::RealFieldElem, y::Integer) = x == ZZRingElem(y)
!=(x::RealFieldElem, y::Integer) = x != ZZRingElem(y)
<=(x::RealFieldElem, y::Integer) = x <= ZZRingElem(y)
<(x::RealFieldElem, y::Integer) = x < ZZRingElem(y)


==(x::Integer, y::RealFieldElem) = ZZRingElem(x) == y
!=(x::Integer, y::RealFieldElem) = ZZRingElem(x) != y
<=(x::Integer, y::RealFieldElem) = ZZRingElem(x) <= y
<(x::Integer, y::RealFieldElem) = ZZRingElem(x) < y

==(x::RealFieldElem, y::Float64) = x == RealFieldElem(y)
!=(x::RealFieldElem, y::Float64) = x != RealFieldElem(y)
<=(x::RealFieldElem, y::Float64) = x <= RealFieldElem(y)
<(x::RealFieldElem, y::Float64) = x < RealFieldElem(y)

==(x::Float64, y::RealFieldElem) = RealFieldElem(x) == y
!=(x::Float64, y::RealFieldElem) = RealFieldElem(x) != y
<=(x::Float64, y::RealFieldElem) = RealFieldElem(x) <= y
<(x::Float64, y::RealFieldElem) = RealFieldElem(x) < y

==(x::RealFieldElem, y::BigFloat) = x == RealFieldElem(y)
!=(x::RealFieldElem, y::BigFloat) = x != RealFieldElem(y)
<=(x::RealFieldElem, y::BigFloat) = x <= RealFieldElem(y)
<(x::RealFieldElem, y::BigFloat) = x < RealFieldElem(y)

==(x::BigFloat, y::RealFieldElem) = RealFieldElem(x) == y
!=(x::BigFloat, y::RealFieldElem) = RealFieldElem(x) != y
<=(x::BigFloat, y::RealFieldElem) = RealFieldElem(x) <= y
<(x::BigFloat, y::RealFieldElem) = RealFieldElem(x) < y

==(x::RealFieldElem, y::QQFieldElem) = x == RealFieldElem(y, precision(Balls))
!=(x::RealFieldElem, y::QQFieldElem) = x != RealFieldElem(y, precision(Balls))
<=(x::RealFieldElem, y::QQFieldElem) = x <= RealFieldElem(y, precision(Balls))
<(x::RealFieldElem, y::QQFieldElem) = x < RealFieldElem(y, precision(Balls))

==(x::QQFieldElem, y::RealFieldElem) = RealFieldElem(x, precision(Balls)) == y
!=(x::QQFieldElem, y::RealFieldElem) = RealFieldElem(x, precision(Balls)) != y
<=(x::QQFieldElem, y::RealFieldElem) = RealFieldElem(x, precision(Balls)) <= y
<(x::QQFieldElem, y::RealFieldElem) = RealFieldElem(x, precision(Balls)) < y

==(x::RealFieldElem, y::Rational{T}) where {T <: Integer} = x == QQFieldElem(y)
!=(x::RealFieldElem, y::Rational{T}) where {T <: Integer} = x != QQFieldElem(y)
<=(x::RealFieldElem, y::Rational{T}) where {T <: Integer} = x <= QQFieldElem(y)
<(x::RealFieldElem, y::Rational{T}) where {T <: Integer} = x < QQFieldElem(y)

==(x::Rational{T}, y::RealFieldElem) where {T <: Integer} = QQFieldElem(x) == y
!=(x::Rational{T}, y::RealFieldElem) where {T <: Integer} = QQFieldElem(x) != y
<=(x::Rational{T}, y::RealFieldElem) where {T <: Integer} = QQFieldElem(x) <= y
<(x::Rational{T}, y::RealFieldElem) where {T <: Integer} = QQFieldElem(x) < y

################################################################################
#
#  Predicates
#
################################################################################

function is_unit(x::RealFieldElem)
  !contains_zero(x)
end

@doc raw"""
    iszero(x::RealFieldElem)

Return `true` if $x$ is certainly zero, otherwise return `false`.
"""
function iszero(x::RealFieldElem)
  return Bool(@ccall libflint.arb_is_zero(x::Ref{RealFieldElem})::Cint)
end

@doc raw"""
    is_nonzero(x::RealFieldElem)

Return `true` if $x$ is certainly not equal to zero, otherwise return
`false`.
"""
function is_nonzero(x::RealFieldElem)
  return Bool(@ccall libflint.arb_is_nonzero(x::Ref{RealFieldElem})::Cint)
end

@doc raw"""
    isone(x::RealFieldElem)

Return `true` if $x$ is certainly one, otherwise return `false`.
"""
function isone(x::RealFieldElem)
  return Bool(@ccall libflint.arb_is_one(x::Ref{RealFieldElem})::Cint)
end

@doc raw"""
    isfinite(x::RealFieldElem)

Return `true` if $x$ is finite, i.e. having finite midpoint and radius,
otherwise return `false`.
"""
function isfinite(x::RealFieldElem)
  return Bool(@ccall libflint.arb_is_finite(x::Ref{RealFieldElem})::Cint)
end

@doc raw"""
    is_exact(x::RealFieldElem)

Return `true` if $x$ is exact, i.e. has zero radius, otherwise return
`false`.
"""
function is_exact(x::RealFieldElem)
  return Bool(@ccall libflint.arb_is_exact(x::Ref{RealFieldElem})::Cint)
end

@doc raw"""
    isinteger(x::RealFieldElem)

Return `true` if $x$ is an exact integer, otherwise return `false`.
"""
function isinteger(x::RealFieldElem)
  return Bool(@ccall libflint.arb_is_int(x::Ref{RealFieldElem})::Cint)
end

@doc raw"""
    is_positive(x::RealFieldElem)

Return `true` if $x$ is certainly positive, otherwise return `false`.
"""
function is_positive(x::RealFieldElem)
  return Bool(@ccall libflint.arb_is_positive(x::Ref{RealFieldElem})::Cint)
end

@doc raw"""
    is_nonnegative(x::RealFieldElem)

Return `true` if $x$ is certainly non-negative, otherwise return `false`.
"""
function is_nonnegative(x::RealFieldElem)
  return Bool(@ccall libflint.arb_is_nonnegative(x::Ref{RealFieldElem})::Cint)
end

@doc raw"""
    is_negative(x::RealFieldElem)

Return `true` if $x$ is certainly negative, otherwise return `false`.
"""
function is_negative(x::RealFieldElem)
  return Bool(@ccall libflint.arb_is_negative(x::Ref{RealFieldElem})::Cint)
end

@doc raw"""
    is_nonpositive(x::RealFieldElem)

Return `true` if $x$ is certainly nonpositive, otherwise return `false`.
"""
function is_nonpositive(x::RealFieldElem)
  return Bool(@ccall libflint.arb_is_nonpositive(x::Ref{RealFieldElem})::Cint)
end

################################################################################
#
#  Parts of numbers
#
################################################################################

@doc raw"""
    ball(x::RealFieldElem, y::RealFieldElem)

Constructs an Arb ball enclosing $x_m \pm (|x_r| + |y_m| + |y_r|)$, given the
pair $(x, y) = (x_m \pm x_r, y_m \pm y_r)$.
"""
function ball(mid::RealFieldElem, rad::RealFieldElem)
  z = RealFieldElem(mid, rad)
  return z
end

@doc raw"""
    radius(x::RealFieldElem)

Return the radius of the ball $x$ as an Arb ball.
"""
function radius(x::RealFieldElem)
  z = RealFieldElem()
  @ccall libflint.arb_get_rad_arb(z::Ref{RealFieldElem}, x::Ref{RealFieldElem})::Nothing
  return z
end

@doc raw"""
    midpoint(x::RealFieldElem)

Return the midpoint of the ball $x$ as an Arb ball.
"""
function midpoint(x::RealFieldElem)
  z = RealFieldElem()
  @ccall libflint.arb_get_mid_arb(z::Ref{RealFieldElem}, x::Ref{RealFieldElem})::Nothing
  return z
end

@doc raw"""
    add_error!(x::RealFieldElem, y::RealFieldElem)

Adds the absolute values of the midpoint and radius of $y$ to the radius of $x$.
"""
function add_error!(x::RealFieldElem, y::RealFieldElem)
  @ccall libflint.arb_add_error(x::Ref{RealFieldElem}, y::Ref{RealFieldElem})::Nothing
end

################################################################################
#
#  Unary operations
#
################################################################################

-(x::RealFieldElem) = neg!(RealFieldElem(), x)

################################################################################
#
#  Binary operations
#
################################################################################

for (s,f) in ((:+,"arb_add"), (:*,"arb_mul"), (://, "arb_div"), (:-,"arb_sub"))
  @eval begin
    function ($s)(x::RealFieldElem, y::RealFieldElem, prec::Int = precision(Balls))
      z = RealFieldElem()
      @ccall libflint.$f(z::Ref{RealFieldElem}, x::Ref{RealFieldElem}, y::Ref{RealFieldElem}, prec::Int)::Nothing
      return z
    end
  end
end

for (f,s) in ((:+, "add"), (:*, "mul"))
  @eval begin
    #function ($f)(x::RealFieldElem, y::arf)
    #  z = RealFieldElem()
    #  @ccall libflint.$("arb_$(s)_arf")(z::Ref{RealFieldElem}, x::Ref{RealFieldElem}, y::Ref{arf}, precision(Balls)::Int)::Nothing
    #  return z
    #end

    #($f)(x::arf, y::RealFieldElem) = ($f)(y, x)

    function ($f)(x::RealFieldElem, y::UInt, prec::Int = precision(Balls))
      z = RealFieldElem()
      @ccall libflint.$("arb_$(s)_ui")(z::Ref{RealFieldElem}, x::Ref{RealFieldElem}, y::UInt, prec::Int)::Nothing
      return z
    end

    ($f)(x::UInt, y::RealFieldElem) = ($f)(y, x)

    function ($f)(x::RealFieldElem, y::Int, prec::Int = precision(Balls))
      z = RealFieldElem()
      @ccall libflint.$("arb_$(s)_si")(z::Ref{RealFieldElem}, x::Ref{RealFieldElem}, y::Int, prec::Int)::Nothing
      return z
    end

    ($f)(x::Int, y::RealFieldElem, prec::Int = precision(Balls)) = ($f)(y, x, prec)

    function ($f)(x::RealFieldElem, y::ZZRingElem, prec::Int = precision(Balls))
      z = RealFieldElem()
      @ccall libflint.$("arb_$(s)_fmpz")(z::Ref{RealFieldElem}, x::Ref{RealFieldElem}, y::Ref{ZZRingElem}, prec::Int)::Nothing
      return z
    end

    ($f)(x::ZZRingElem, y::RealFieldElem, prec::Int = precision(Balls)) = ($f)(y, x, prec)
  end
end

#function -(x::RealFieldElem, y::arf)
#  z = RealFieldElem()
#  ccall((:arb_sub_arf, libflint), Nothing,
#              (Ref{RealFieldElem}, Ref{RealFieldElem}, Ref{arf}, Int), z, x, y, precision(Balls))
#  return z
#end

#-(x::arf, y::RealFieldElem) = -(y - x)

function -(x::RealFieldElem, y::UInt, prec::Int = precision(Balls))
  z = RealFieldElem()
  @ccall libflint.arb_sub_ui(z::Ref{RealFieldElem}, x::Ref{RealFieldElem}, y::UInt, prec::Int)::Nothing
  return z
end

-(x::UInt, y::RealFieldElem) = -(y - x)

function -(x::RealFieldElem, y::Int, prec::Int = precision(Balls))
  z = RealFieldElem()
  @ccall libflint.arb_sub_si(z::Ref{RealFieldElem}, x::Ref{RealFieldElem}, y::Int, prec::Int)::Nothing
  return z
end

-(x::Int, y::RealFieldElem) = -(y - x)

function -(x::RealFieldElem, y::ZZRingElem, prec::Int = precision(Balls))
  z = RealFieldElem()
  @ccall libflint.arb_sub_fmpz(z::Ref{RealFieldElem}, x::Ref{RealFieldElem}, y::Ref{ZZRingElem}, prec::Int)::Nothing
  return z
end

-(x::ZZRingElem, y::RealFieldElem) = -(y-x)

+(x::RealFieldElem, y::Integer) = x + ZZRingElem(y)

-(x::RealFieldElem, y::Integer) = x - ZZRingElem(y)

*(x::RealFieldElem, y::Integer) = x*ZZRingElem(y)

//(x::RealFieldElem, y::Integer) = x//ZZRingElem(y)

+(x::Integer, y::RealFieldElem) = ZZRingElem(x) + y

-(x::Integer, y::RealFieldElem) = ZZRingElem(x) - y

*(x::Integer, y::RealFieldElem) = ZZRingElem(x)*y

//(x::Integer, y::RealFieldElem) = ZZRingElem(x)//y

#function //(x::RealFieldElem, y::arf)
#  z = RealFieldElem()
#  ccall((:arb_div_arf, libflint), Nothing,
#              (Ref{RealFieldElem}, Ref{RealFieldElem}, Ref{arf}, Int), z, x, y, precision(Balls))
#  return z
#end

function //(x::RealFieldElem, y::UInt, prec::Int = precision(Balls))
  z = RealFieldElem()
  @ccall libflint.arb_div_ui(z::Ref{RealFieldElem}, x::Ref{RealFieldElem}, y::UInt, prec::Int)::Nothing
  return z
end

function //(x::RealFieldElem, y::Int, prec::Int = precision(Balls))
  z = RealFieldElem()
  @ccall libflint.arb_div_si(z::Ref{RealFieldElem}, x::Ref{RealFieldElem}, y::Int, prec::Int)::Nothing
  return z
end

function //(x::RealFieldElem, y::ZZRingElem, prec::Int = precision(Balls))
  z = RealFieldElem()
  @ccall libflint.arb_div_fmpz(z::Ref{RealFieldElem}, x::Ref{RealFieldElem}, y::Ref{ZZRingElem}, prec::Int)::Nothing
  return z
end

function //(x::UInt, y::RealFieldElem, prec::Int = precision(Balls))
  z = parent(y)()
  @ccall libflint.arb_ui_div(z::Ref{RealFieldElem}, x::UInt, y::Ref{RealFieldElem}, prec::Int)::Nothing
  return z
end

function //(x::Int, y::RealFieldElem, prec::Int = precision(Balls))
  z = parent(y)()
  t = RealFieldElem(x)
  @ccall libflint.arb_div(z::Ref{RealFieldElem}, t::Ref{RealFieldElem}, y::Ref{RealFieldElem}, prec::Int)::Nothing
  return z
end

function //(x::ZZRingElem, y::RealFieldElem, prec::Int = precision(Balls))
  z = parent(y)()
  t = RealFieldElem(x)
  @ccall libflint.arb_div(z::Ref{RealFieldElem}, t::Ref{RealFieldElem}, y::Ref{RealFieldElem}, prec::Int)::Nothing
  return z
end

function ^(x::RealFieldElem, y::RealFieldElem, prec::Int = precision(Balls))
  z = RealFieldElem()
  @ccall libflint.arb_pow(z::Ref{RealFieldElem}, x::Ref{RealFieldElem}, y::Ref{RealFieldElem}, prec::Int)::Nothing
  return z
end

function ^(x::RealFieldElem, y::ZZRingElem, prec::Int = precision(Balls))
  z = RealFieldElem()
  @ccall libflint.arb_pow_fmpz(z::Ref{RealFieldElem}, x::Ref{RealFieldElem}, y::Ref{ZZRingElem}, prec::Int)::Nothing
  return z
end

^(x::RealFieldElem, y::Integer, prec::Int = precision(Balls)) = ^(x, ZZRingElem(y), prec)

function ^(x::RealFieldElem, y::UInt, prec::Int = precision(Balls))
  z = RealFieldElem()
  @ccall libflint.arb_pow_ui(z::Ref{RealFieldElem}, x::Ref{RealFieldElem}, y::UInt, prec::Int)::Nothing
  return z
end

function ^(x::RealFieldElem, y::QQFieldElem, prec::Int = precision(Balls))
  z = RealFieldElem()
  @ccall libflint.arb_pow_fmpq(z::Ref{RealFieldElem}, x::Ref{RealFieldElem}, y::Ref{QQFieldElem}, prec::Int)::Nothing
  return z
end

+(x::QQFieldElem, y::RealFieldElem) = parent(y)(x) + y
+(x::RealFieldElem, y::QQFieldElem) = x + parent(x)(y)
-(x::QQFieldElem, y::RealFieldElem) = parent(y)(x) - y
//(x::RealFieldElem, y::QQFieldElem) = x//parent(x)(y)
//(x::QQFieldElem, y::RealFieldElem) = parent(y)(x)//y
-(x::RealFieldElem, y::QQFieldElem) = x - parent(x)(y)
*(x::QQFieldElem, y::RealFieldElem) = parent(y)(x) * y
*(x::RealFieldElem, y::QQFieldElem) = x * parent(x)(y)
^(x::QQFieldElem, y::RealFieldElem) = parent(y)(x) ^ y

+(x::Float64, y::RealFieldElem) = parent(y)(x) + y
+(x::RealFieldElem, y::Float64) = x + parent(x)(y)
-(x::Float64, y::RealFieldElem) = parent(y)(x) - y
//(x::RealFieldElem, y::Float64) = x//parent(x)(y)
//(x::Float64, y::RealFieldElem) = parent(y)(x)//y
-(x::RealFieldElem, y::Float64) = x - parent(x)(y)
*(x::Float64, y::RealFieldElem) = parent(y)(x) * y
*(x::RealFieldElem, y::Float64) = x * parent(x)(y)
^(x::Float64, y::RealFieldElem) = parent(y)(x) ^ y
^(x::RealFieldElem, y::Float64) = x ^ parent(x)(y)

+(x::BigFloat, y::RealFieldElem) = parent(y)(x) + y
+(x::RealFieldElem, y::BigFloat) = x + parent(x)(y)
-(x::BigFloat, y::RealFieldElem) = parent(y)(x) - y
//(x::RealFieldElem, y::BigFloat) = x//parent(x)(y)
//(x::BigFloat, y::RealFieldElem) = parent(y)(x)//y
-(x::RealFieldElem, y::BigFloat) = x - parent(x)(y)
*(x::BigFloat, y::RealFieldElem) = parent(y)(x) * y
*(x::RealFieldElem, y::BigFloat) = x * parent(x)(y)
^(x::BigFloat, y::RealFieldElem) = parent(y)(x) ^ y
^(x::RealFieldElem, y::BigFloat) = x ^ parent(x)(y)

+(x::Rational{T}, y::RealFieldElem) where {T <: Integer} = QQFieldElem(x) + y
+(x::RealFieldElem, y::Rational{T}) where {T <: Integer} = x + QQFieldElem(y)
-(x::Rational{T}, y::RealFieldElem) where {T <: Integer} = QQFieldElem(x) - y
-(x::RealFieldElem, y::Rational{T}) where {T <: Integer} = x - QQFieldElem(y)
//(x::Rational{T}, y::RealFieldElem) where {T <: Integer} = QQFieldElem(x)//y
//(x::RealFieldElem, y::Rational{T}) where {T <: Integer} = x//QQFieldElem(y)
*(x::Rational{T}, y::RealFieldElem) where {T <: Integer} = QQFieldElem(x) * y
*(x::RealFieldElem, y::Rational{T}) where {T <: Integer} = x * QQFieldElem(y)
^(x::Rational{T}, y::RealFieldElem) where {T <: Integer} = QQFieldElem(x) ^ y
^(x::RealFieldElem, y::Rational{T}) where {T <: Integer} = x ^ QQFieldElem(y)

/(x::RealFieldElem, y::RealFieldElem) = x // y
/(x::ZZRingElem, y::RealFieldElem) = x // y
/(x::RealFieldElem, y::ZZRingElem) = x // y
/(x::Int, y::RealFieldElem) = x // y
/(x::RealFieldElem, y::Int) = x // y
/(x::UInt, y::RealFieldElem) = x // y
/(x::RealFieldElem, y::UInt) = x // y
/(x::QQFieldElem, y::RealFieldElem) = x // y
/(x::RealFieldElem, y::QQFieldElem) = x // y
/(x::Float64, y::RealFieldElem) = x // y
/(x::RealFieldElem, y::Float64) = x // y
/(x::BigFloat, y::RealFieldElem) = x // y
/(x::RealFieldElem, y::BigFloat) = x // y
/(x::Rational{T}, y::RealFieldElem) where {T <: Integer} = x // y
/(x::RealFieldElem, y::Rational{T}) where {T <: Integer} = x // y

divexact(x::RealFieldElem, y::RealFieldElem; check::Bool=true) = x // y
divexact(x::ZZRingElem, y::RealFieldElem; check::Bool=true) = x // y
divexact(x::RealFieldElem, y::ZZRingElem; check::Bool=true) = x // y
divexact(x::Int, y::RealFieldElem; check::Bool=true) = x // y
divexact(x::RealFieldElem, y::Int; check::Bool=true) = x // y
divexact(x::UInt, y::RealFieldElem; check::Bool=true) = x // y
divexact(x::RealFieldElem, y::UInt; check::Bool=true) = x // y
divexact(x::QQFieldElem, y::RealFieldElem; check::Bool=true) = x // y
divexact(x::RealFieldElem, y::QQFieldElem; check::Bool=true) = x // y
divexact(x::Float64, y::RealFieldElem; check::Bool=true) = x // y
divexact(x::RealFieldElem, y::Float64; check::Bool=true) = x // y
divexact(x::BigFloat, y::RealFieldElem; check::Bool=true) = x // y
divexact(x::RealFieldElem, y::BigFloat; check::Bool=true) = x // y
divexact(x::Rational{T}, y::RealFieldElem; check::Bool=true) where {T <: Integer} = x // y
divexact(x::RealFieldElem, y::Rational{T}; check::Bool=true) where {T <: Integer} = x // y

################################################################################
#
#  Absolute value
#
################################################################################

function abs(x::RealFieldElem)
  z = RealFieldElem()
  @ccall libflint.arb_abs(z::Ref{RealFieldElem}, x::Ref{RealFieldElem})::Nothing
  return z
end

################################################################################
#
#  Inverse
#
################################################################################

function inv(x::RealFieldElem, prec::Int = precision(Balls))
  z = RealFieldElem()
  @ccall libflint.arb_inv(z::Ref{RealFieldElem}, x::Ref{RealFieldElem}, prec::Int)::Nothing
  return parent(x)(z)
end

################################################################################
#
#  Shifting
#
################################################################################

function ldexp(x::RealFieldElem, y::Int)
  z = RealFieldElem()
  @ccall libflint.arb_mul_2exp_si(z::Ref{RealFieldElem}, x::Ref{RealFieldElem}, y::Int)::Nothing
  return z
end

function ldexp(x::RealFieldElem, y::ZZRingElem)
  z = RealFieldElem()
  @ccall libflint.arb_mul_2exp_fmpz(z::Ref{RealFieldElem}, x::Ref{RealFieldElem}, y::Ref{ZZRingElem})::Nothing
  return z
end

################################################################################
#
#  Miscellaneous
#
################################################################################

@doc raw"""
    trim(x::RealFieldElem)

Return an `RealFieldElem` interval containing $x$ but which may be more economical,
by rounding off insignificant bits from the midpoint.
"""
function trim(x::RealFieldElem)
  z = RealFieldElem()
  @ccall libflint.arb_trim(z::Ref{RealFieldElem}, x::Ref{RealFieldElem})::Nothing
  return z
end

@doc raw"""
    unique_integer(x::RealFieldElem)

Return a pair where the first value is a boolean and the second is an `ZZRingElem`
integer. The boolean indicates whether the interval $x$ contains a unique
integer. If this is the case, the second return value is set to this unique
integer.
"""
function unique_integer(x::RealFieldElem)
  z = ZZRingElem()
  unique = @ccall libflint.arb_get_unique_fmpz(z::Ref{ZZRingElem}, x::Ref{RealFieldElem})::Int
  return (unique != 0, z)
end

function (::ZZRing)(a::RealFieldElem)
  return ZZRingElem(a)
end

@doc raw"""
    setunion(x::RealFieldElem, y::RealFieldElem)

Return an `ArbFieldElem` containing the union of the intervals represented by $x$ and
$y$.
"""
function setunion(x::RealFieldElem, y::RealFieldElem, prec::Int = precision(Balls))
  z = RealFieldElem()
  @ccall libflint.arb_union(z::Ref{RealFieldElem}, x::Ref{RealFieldElem}, y::Ref{RealFieldElem}, prec::Int)::Nothing
  return z
end

@doc raw"""
    setintersection(x::RealFieldElem, y::RealFieldElem)

Return an `ArbFieldElem` containing the intersection of the intervals represented by
$x$ and $y$.
"""
function setintersection(x::RealFieldElem, y::RealFieldElem, prec::Int = precision(Balls))
  z = RealFieldElem()
  @ccall libflint.arb_intersection(z::Ref{RealFieldElem}, x::Ref{RealFieldElem}, y::Ref{RealFieldElem}, prec::Int)::Nothing
  return z
end

################################################################################
#
#  Constants
#
################################################################################

@doc raw"""
    const_pi(r::RealField)

Return $\pi = 3.14159\ldots$ as an element of $r$.
"""
function const_pi(r::RealField, prec::Int = precision(Balls))
  z = r()
  @ccall libflint.arb_const_pi(z::Ref{RealFieldElem}, prec::Int)::Nothing
  return z
end

@doc raw"""
    const_e(r::RealField)

Return $e = 2.71828\ldots$ as an element of $r$.
"""
function const_e(r::RealField, prec::Int = precision(Balls))
  z = r()
  @ccall libflint.arb_const_e(z::Ref{RealFieldElem}, prec::Int)::Nothing
  return z
end

@doc raw"""
    const_log2(r::RealField)

Return $\log(2) = 0.69314\ldots$ as an element of $r$.
"""
function const_log2(r::RealField, prec::Int = precision(Balls))
  z = r()
  @ccall libflint.arb_const_log2(z::Ref{RealFieldElem}, prec::Int)::Nothing
  return z
end

@doc raw"""
    const_log10(r::RealField)

Return $\log(10) = 2.302585\ldots$ as an element of $r$.
"""
function const_log10(r::RealField, prec::Int = precision(Balls))
  z = r()
  @ccall libflint.arb_const_log10(z::Ref{RealFieldElem}, prec::Int)::Nothing
  return z
end

@doc raw"""
    const_euler(r::RealField)

Return Euler's constant $\gamma = 0.577215\ldots$ as an element of $r$.
"""
function const_euler(r::RealField, prec::Int = precision(Balls))
  z = r()
  @ccall libflint.arb_const_euler(z::Ref{RealFieldElem}, prec::Int)::Nothing
  return z
end

@doc raw"""
    const_catalan(r::RealField)

Return Catalan's constant $C = 0.915965\ldots$ as an element of $r$.
"""
function const_catalan(r::RealField, prec::Int = precision(Balls))
  z = r()
  @ccall libflint.arb_const_catalan(z::Ref{RealFieldElem}, prec::Int)::Nothing
  return z
end

@doc raw"""
    const_khinchin(r::RealField)

Return Khinchin's constant $K = 2.685452\ldots$ as an element of $r$.
"""
function const_khinchin(r::RealField, prec::Int = precision(Balls))
  z = r()
  @ccall libflint.arb_const_khinchin(z::Ref{RealFieldElem}, prec::Int)::Nothing
  return z
end

@doc raw"""
    const_glaisher(r::RealField)

Return Glaisher's constant $A = 1.282427\ldots$ as an element of $r$.
"""
function const_glaisher(r::RealField, prec::Int = precision(Balls))
  z = r()
  @ccall libflint.arb_const_glaisher(z::Ref{RealFieldElem}, prec::Int)::Nothing
  return z
end

################################################################################
#
#  Real valued functions
#
################################################################################

# real - real functions

function floor(x::RealFieldElem, prec::Int = precision(Balls))
  z = RealFieldElem()
  @ccall libflint.arb_floor(z::Ref{RealFieldElem}, x::Ref{RealFieldElem}, prec::Int)::Nothing
  return z
end

floor(::Type{RealFieldElem}, x::RealFieldElem) = floor(x)
floor(::Type{ZZRingElem}, x::RealFieldElem) = ZZRingElem(floor(x))
floor(::Type{T}, x::RealFieldElem) where {T <: Integer} = T(floor(x))

function ceil(x::RealFieldElem, prec::Int = precision(Balls))
  z = RealFieldElem()
  @ccall libflint.arb_ceil(z::Ref{RealFieldElem}, x::Ref{RealFieldElem}, prec::Int)::Nothing
  return z
end

ceil(::Type{RealFieldElem}, x::RealFieldElem) = ceil(x)
ceil(::Type{ZZRingElem}, x::RealFieldElem) = ZZRingElem(ceil(x))
ceil(::Type{T}, x::RealFieldElem) where {T <: Integer} = T(ceil(x))

function Base.sqrt(x::RealFieldElem, prec::Int = precision(Balls); check::Bool=true)
  z = RealFieldElem()
  @ccall libflint.arb_sqrt(z::Ref{RealFieldElem}, x::Ref{RealFieldElem}, prec::Int)::Nothing
  return z
end

@doc raw"""
    rsqrt(x::RealFieldElem)

Return the reciprocal of the square root of $x$, i.e. $1/\sqrt{x}$.
"""
function rsqrt(x::RealFieldElem, prec::Int = precision(Balls))
  z = RealFieldElem()
  @ccall libflint.arb_rsqrt(z::Ref{RealFieldElem}, x::Ref{RealFieldElem}, prec::Int)::Nothing
  return z
end

@doc raw"""
    sqrt1pm1(x::RealFieldElem)

Return $\sqrt{1+x}-1$, evaluated accurately for small $x$.
"""
function sqrt1pm1(x::RealFieldElem, prec::Int = precision(Balls))
  z = RealFieldElem()
  @ccall libflint.arb_sqrt1pm1(z::Ref{RealFieldElem}, x::Ref{RealFieldElem}, prec::Int)::Nothing
  return z
end

@doc raw"""
    sqrtpos(x::RealFieldElem)

Return the sqrt root of $x$, assuming that $x$ represents a non-negative
number. Thus any negative number in the input interval is discarded.
"""
function sqrtpos(x::RealFieldElem, prec::Int = precision(Balls))
  z = RealFieldElem()
  @ccall libflint.arb_sqrtpos(z::Ref{RealFieldElem}, x::Ref{RealFieldElem}, prec::Int)::Nothing
  return z
end

function log(x::RealFieldElem, prec::Int = precision(Balls))
  z = RealFieldElem()
  @ccall libflint.arb_log(z::Ref{RealFieldElem}, x::Ref{RealFieldElem}, prec::Int)::Nothing
  return z
end

function log1p(x::RealFieldElem, prec::Int = precision(Balls))
  z = RealFieldElem()
  @ccall libflint.arb_log1p(z::Ref{RealFieldElem}, x::Ref{RealFieldElem}, prec::Int)::Nothing
  return z
end

function Base.exp(x::RealFieldElem, prec::Int = precision(Balls))
  z = RealFieldElem()
  @ccall libflint.arb_exp(z::Ref{RealFieldElem}, x::Ref{RealFieldElem}, prec::Int)::Nothing
  return z
end

function expm1(x::RealFieldElem, prec::Int = precision(Balls))
  z = RealFieldElem()
  @ccall libflint.arb_expm1(z::Ref{RealFieldElem}, x::Ref{RealFieldElem}, prec::Int)::Nothing
  return z
end

function sin(x::RealFieldElem, prec::Int = precision(Balls))
  z = RealFieldElem()
  @ccall libflint.arb_sin(z::Ref{RealFieldElem}, x::Ref{RealFieldElem}, prec::Int)::Nothing
  return z
end

function cos(x::RealFieldElem, prec::Int = precision(Balls))
  z = RealFieldElem()
  @ccall libflint.arb_cos(z::Ref{RealFieldElem}, x::Ref{RealFieldElem}, prec::Int)::Nothing
  return z
end

function sinpi(x::RealFieldElem, prec::Int = precision(Balls))
  z = RealFieldElem()
  @ccall libflint.arb_sin_pi(z::Ref{RealFieldElem}, x::Ref{RealFieldElem}, prec::Int)::Nothing
  return z
end

function cospi(x::RealFieldElem, prec::Int = precision(Balls))
  z = RealFieldElem()
  @ccall libflint.arb_cos_pi(z::Ref{RealFieldElem}, x::Ref{RealFieldElem}, prec::Int)::Nothing
  return z
end

function tan(x::RealFieldElem, prec::Int = precision(Balls))
  z = RealFieldElem()
  @ccall libflint.arb_tan(z::Ref{RealFieldElem}, x::Ref{RealFieldElem}, prec::Int)::Nothing
  return z
end

function cot(x::RealFieldElem, prec::Int = precision(Balls))
  z = RealFieldElem()
  @ccall libflint.arb_cot(z::Ref{RealFieldElem}, x::Ref{RealFieldElem}, prec::Int)::Nothing
  return z
end

function tanpi(x::RealFieldElem, prec::Int = precision(Balls))
  z = RealFieldElem()
  @ccall libflint.arb_tan_pi(z::Ref{RealFieldElem}, x::Ref{RealFieldElem}, prec::Int)::Nothing
  return z
end

function cotpi(x::RealFieldElem, prec::Int = precision(Balls))
  z = RealFieldElem()
  @ccall libflint.arb_cot_pi(z::Ref{RealFieldElem}, x::Ref{RealFieldElem}, prec::Int)::Nothing
  return z
end

function sinh(x::RealFieldElem, prec::Int = precision(Balls))
  z = RealFieldElem()
  @ccall libflint.arb_sinh(z::Ref{RealFieldElem}, x::Ref{RealFieldElem}, prec::Int)::Nothing
  return z
end

function cosh(x::RealFieldElem, prec::Int = precision(Balls))
  z = RealFieldElem()
  @ccall libflint.arb_cosh(z::Ref{RealFieldElem}, x::Ref{RealFieldElem}, prec::Int)::Nothing
  return z
end

function tanh(x::RealFieldElem, prec::Int = precision(Balls))
  z = RealFieldElem()
  @ccall libflint.arb_tanh(z::Ref{RealFieldElem}, x::Ref{RealFieldElem}, prec::Int)::Nothing
  return z
end

function coth(x::RealFieldElem, prec::Int = precision(Balls))
  z = RealFieldElem()
  @ccall libflint.arb_coth(z::Ref{RealFieldElem}, x::Ref{RealFieldElem}, prec::Int)::Nothing
  return z
end

function atan(x::RealFieldElem, prec::Int = precision(Balls))
  z = RealFieldElem()
  @ccall libflint.arb_atan(z::Ref{RealFieldElem}, x::Ref{RealFieldElem}, prec::Int)::Nothing
  return z
end

function asin(x::RealFieldElem, prec::Int = precision(Balls))
  z = RealFieldElem()
  @ccall libflint.arb_asin(z::Ref{RealFieldElem}, x::Ref{RealFieldElem}, prec::Int)::Nothing
  return z
end

function acos(x::RealFieldElem, prec::Int = precision(Balls))
  z = RealFieldElem()
  @ccall libflint.arb_acos(z::Ref{RealFieldElem}, x::Ref{RealFieldElem}, prec::Int)::Nothing
  return z
end

function atanh(x::RealFieldElem, prec::Int = precision(Balls))
  z = RealFieldElem()
  @ccall libflint.arb_atanh(z::Ref{RealFieldElem}, x::Ref{RealFieldElem}, prec::Int)::Nothing
  return z
end

function asinh(x::RealFieldElem, prec::Int = precision(Balls))
  z = RealFieldElem()
  @ccall libflint.arb_asinh(z::Ref{RealFieldElem}, x::Ref{RealFieldElem}, prec::Int)::Nothing
  return z
end

function acosh(x::RealFieldElem, prec::Int = precision(Balls))
  z = RealFieldElem()
  @ccall libflint.arb_acosh(z::Ref{RealFieldElem}, x::Ref{RealFieldElem}, prec::Int)::Nothing
  return z
end

@doc raw"""
    gamma(x::RealFieldElem)

Return the Gamma function evaluated at $x$.
"""
function gamma(x::RealFieldElem, prec::Int = precision(Balls))
  z = RealFieldElem()
  @ccall libflint.arb_gamma(z::Ref{RealFieldElem}, x::Ref{RealFieldElem}, prec::Int)::Nothing
  return z
end

@doc raw"""
    lgamma(x::RealFieldElem)

Return the logarithm of the Gamma function evaluated at $x$.
"""
function lgamma(x::RealFieldElem, prec::Int = precision(Balls))
  z = RealFieldElem()
  @ccall libflint.arb_lgamma(z::Ref{RealFieldElem}, x::Ref{RealFieldElem}, prec::Int)::Nothing
  return z
end

@doc raw"""
    rgamma(x::RealFieldElem)

Return the reciprocal of the Gamma function evaluated at $x$.
"""
function rgamma(x::RealFieldElem, prec::Int = precision(Balls))
  z = RealFieldElem()
  @ccall libflint.arb_rgamma(z::Ref{RealFieldElem}, x::Ref{RealFieldElem}, prec::Int)::Nothing
  return z
end

@doc raw"""
    digamma(x::RealFieldElem)

Return the  logarithmic derivative of the gamma function evaluated at $x$,
i.e. $\psi(x)$.
"""
function digamma(x::RealFieldElem, prec::Int = precision(Balls))
  z = RealFieldElem()
  @ccall libflint.arb_digamma(z::Ref{RealFieldElem}, x::Ref{RealFieldElem}, prec::Int)::Nothing
  return z
end

@doc raw"""
    gamma(s::RealFieldElem, x::RealFieldElem)

Return the upper incomplete gamma function $\Gamma(s,x)$.
"""
function gamma(s::RealFieldElem, x::RealFieldElem, prec::Int = precision(Balls))
  z = parent(s)()
  @ccall libflint.arb_hypgeom_gamma_upper(z::Ref{RealFieldElem}, s::Ref{RealFieldElem}, x::Ref{RealFieldElem}, 0::Int, prec::Int)::Nothing
  return z
end

@doc raw"""
    gamma_regularized(s::RealFieldElem, x::RealFieldElem)

Return the regularized upper incomplete gamma function
$\Gamma(s,x) / \Gamma(s)$.
"""
function gamma_regularized(s::RealFieldElem, x::RealFieldElem, prec::Int = precision(Balls))
  z = parent(s)()
  @ccall libflint.arb_hypgeom_gamma_upper(z::Ref{RealFieldElem}, s::Ref{RealFieldElem}, x::Ref{RealFieldElem}, 1::Int, prec::Int)::Nothing
  return z
end

@doc raw"""
    gamma_lower(s::RealFieldElem, x::RealFieldElem)

Return the lower incomplete gamma function $\gamma(s,x) / \Gamma(s)$.
"""
function gamma_lower(s::RealFieldElem, x::RealFieldElem, prec::Int = precision(Balls))
  z = parent(s)()
  @ccall libflint.arb_hypgeom_gamma_lower(z::Ref{RealFieldElem}, s::Ref{RealFieldElem}, x::Ref{RealFieldElem}, 0::Int, prec::Int)::Nothing
  return z
end

@doc raw"""
    gamma_lower_regularized(s::RealFieldElem, x::RealFieldElem)

Return the regularized lower incomplete gamma function
$\gamma(s,x) / \Gamma(s)$.
"""
function gamma_lower_regularized(s::RealFieldElem, x::RealFieldElem, prec::Int = precision(Balls))
  z = parent(s)()
  @ccall libflint.arb_hypgeom_gamma_lower(z::Ref{RealFieldElem}, s::Ref{RealFieldElem}, x::Ref{RealFieldElem}, 1::Int, prec::Int)::Nothing
  return z
end


@doc raw"""
    zeta(x::RealFieldElem)

Return the Riemann zeta function evaluated at $x$.
"""
function zeta(x::RealFieldElem, prec::Int = precision(Balls))
  z = RealFieldElem()
  @ccall libflint.arb_zeta(z::Ref{RealFieldElem}, x::Ref{RealFieldElem}, prec::Int)::Nothing
  return z
end

function sincos(x::RealFieldElem, prec::Int = precision(Balls))
  s = RealFieldElem()
  c = RealFieldElem()
  @ccall libflint.arb_sin_cos(s::Ref{RealFieldElem}, c::Ref{RealFieldElem}, x::Ref{RealFieldElem}, prec::Int)::Nothing
  return (s, c)
end

function sincospi(x::RealFieldElem, prec::Int = precision(Balls))
  s = RealFieldElem()
  c = RealFieldElem()
  @ccall libflint.arb_sin_cos_pi(s::Ref{RealFieldElem}, c::Ref{RealFieldElem}, x::Ref{RealFieldElem}, prec::Int)::Nothing
  return (s, c)
end

function sinpi(x::QQFieldElem, r::RealField, prec::Int = precision(Balls))
  z = r()
  @ccall libflint.arb_sin_pi_fmpq(z::Ref{RealFieldElem}, x::Ref{QQFieldElem}, prec::Int)::Nothing
  return z
end

function cospi(x::QQFieldElem, r::RealField, prec::Int = precision(Balls))
  z = r()
  @ccall libflint.arb_cos_pi_fmpq(z::Ref{RealFieldElem}, x::Ref{QQFieldElem}, prec::Int)::Nothing
  return z
end

function sincospi(x::QQFieldElem, r::RealField, prec::Int = precision(Balls))
  s = r()
  c = r()
  @ccall libflint.arb_sin_cos_pi_fmpq(s::Ref{RealFieldElem}, c::Ref{RealFieldElem}, x::Ref{QQFieldElem}, prec::Int)::Nothing
  return (s, c)
end

function sinhcosh(x::RealFieldElem, prec::Int = precision(Balls))
  s = RealFieldElem()
  c = RealFieldElem()
  @ccall libflint.arb_sinh_cosh(s::Ref{RealFieldElem}, c::Ref{RealFieldElem}, x::Ref{RealFieldElem}, prec::Int)::Nothing
  return (s, c)
end

function atan(y::RealFieldElem, x::RealFieldElem, prec::Int = precision(Balls))
  z = parent(y)()
  @ccall libflint.arb_atan2(z::Ref{RealFieldElem}, y::Ref{RealFieldElem}, x::Ref{RealFieldElem}, prec::Int)::Nothing
  return z
end

@doc raw"""
    atan2(y::RealFieldElem, x::RealFieldElem)

Return $\operatorname{atan2}(y,x) = \arg(x+yi)$. Same as `atan(y, x)`.
"""
function atan2(y::RealFieldElem, x::RealFieldElem, prec::Int = precision(Balls))
  return atan(y, x, prec)
end

@doc raw"""
    agm(x::RealFieldElem, y::RealFieldElem)

Return the arithmetic-geometric mean of $x$ and $y$
"""
function agm(x::RealFieldElem, y::RealFieldElem, prec::Int = precision(Balls))
  z = RealFieldElem()
  @ccall libflint.arb_agm(z::Ref{RealFieldElem}, x::Ref{RealFieldElem}, y::Ref{RealFieldElem}, prec::Int)::Nothing
  return z
end

@doc raw"""
    zeta(s::RealFieldElem, a::RealFieldElem)

Return the Hurwitz zeta function $\zeta(s,a)$.
"""
function zeta(s::RealFieldElem, a::RealFieldElem, prec::Int = precision(Balls))
  z = parent(s)()
  @ccall libflint.arb_hurwitz_zeta(z::Ref{RealFieldElem}, s::Ref{RealFieldElem}, a::Ref{RealFieldElem}, prec::Int)::Nothing
  return z
end

function hypot(x::RealFieldElem, y::RealFieldElem, prec::Int = precision(Balls))
  z = RealFieldElem()
  @ccall libflint.arb_hypot(z::Ref{RealFieldElem}, x::Ref{RealFieldElem}, y::Ref{RealFieldElem}, prec::Int)::Nothing
  return z
end

function root(x::RealFieldElem, n::UInt, prec::Int = precision(Balls))
  is_zero(x) && return x
  z = RealFieldElem()
  @ccall libflint.arb_root(z::Ref{RealFieldElem}, x::Ref{RealFieldElem}, n::UInt, prec::Int)::Nothing
  return z
end

@doc raw"""
    root(x::RealFieldElem, n::Int)

Return the $n$-th root of $x$. We require $x \geq 0$.
"""
function root(x::RealFieldElem, n::Int, prec::Int = precision(Balls))
  x < 0 && throw(DomainError(x, "Argument must be positive"))
  return root(x, UInt(n))
end

@doc raw"""
    factorial(x::RealFieldElem)

Return the factorial of $x$.
"""
factorial(x::RealFieldElem, prec::Int = precision(Balls)) = gamma(x+1)

function factorial(n::UInt, r::RealField, prec::Int = precision(Balls))
  z = r()
  @ccall libflint.arb_fac_ui(z::Ref{RealFieldElem}, n::UInt, prec::Int)::Nothing
  return z
end

@doc raw"""
    factorial(n::Int, r::RealField)

Return the factorial of $n$ in the given Arb field.
"""
factorial(n::Int, r::RealField, prec::Int = precision(Balls)) = n < 0 ? factorial(r(n), prec) : factorial(UInt(n), r, prec)

@doc raw"""
    binomial(x::RealFieldElem, n::UInt)

Return the binomial coefficient ${x \choose n}$.
"""
function binomial(x::RealFieldElem, n::UInt, prec::Int = precision(Balls))
  z = RealFieldElem()
  @ccall libflint.arb_bin_ui(z::Ref{RealFieldElem}, x::Ref{RealFieldElem}, n::UInt, prec::Int)::Nothing
  return z
end

@doc raw"""
    binomial(n::UInt, k::UInt, r::RealField)

Return the binomial coefficient ${n \choose k}$ in the given Arb field.
"""
function binomial(n::UInt, k::UInt, r::RealField, prec::Int = precision(Balls))
  z = r()
  @ccall libflint.arb_bin_uiui(z::Ref{RealFieldElem}, n::UInt, k::UInt, prec::Int)::Nothing
  return z
end

@doc raw"""
    fibonacci(n::ZZRingElem, r::RealField)

Return the $n$-th Fibonacci number in the given Arb field.
"""
function fibonacci(n::ZZRingElem, r::RealField, prec::Int = precision(Balls))
  z = r()
  @ccall libflint.arb_fib_fmpz(z::Ref{RealFieldElem}, n::Ref{ZZRingElem}, prec::Int)::Nothing
  return z
end

function fibonacci(n::UInt, r::RealField, prec::Int = precision(Balls))
  z = r()
  @ccall libflint.arb_fib_ui(z::Ref{RealFieldElem}, n::UInt, prec::Int)::Nothing
  return z
end

@doc raw"""
    fibonacci(n::Int, r::RealField)

Return the $n$-th Fibonacci number in the given Arb field.
"""
fibonacci(n::Int, r::RealField, prec::Int = precision(Balls)) = n >= 0 ? fibonacci(UInt(n), r, prec) : fibonacci(ZZRingElem(n), r, prec)

@doc raw"""
    gamma(x::ZZRingElem, r::RealField)

Return the Gamma function evaluated at $x$ in the given Arb field.
"""
function gamma(x::ZZRingElem, r::RealField, prec::Int = precision(Balls))
  z = r()
  @ccall libflint.arb_gamma_fmpz(z::Ref{RealFieldElem}, x::Ref{ZZRingElem}, prec::Int)::Nothing
  return z
end

@doc raw"""
    gamma(x::QQFieldElem, r::RealField)

Return the Gamma function evaluated at $x$ in the given Arb field.
"""
function gamma(x::QQFieldElem, r::RealField, prec::Int = precision(Balls))
  z = r()
  @ccall libflint.arb_gamma_fmpq(z::Ref{RealFieldElem}, x::Ref{QQFieldElem}, prec::Int)::Nothing
  return z
end


function zeta(n::UInt, r::RealField, prec::Int = precision(Balls))
  z = r()
  @ccall libflint.arb_zeta_ui(z::Ref{RealFieldElem}, n::UInt, prec::Int)::Nothing
  return z
end

@doc raw"""
    zeta(n::Int, r::RealField)

Return the Riemann zeta function $\zeta(n)$ as an element of the given Arb
field.
"""
zeta(n::Int, r::RealField, prec::Int = precision(Balls)) = n >= 0 ? zeta(UInt(n), r, prec) : zeta(r(n), prec)

function bernoulli(n::UInt, r::RealField, prec::Int = precision(Balls))
  z = r()
  @ccall libflint.arb_bernoulli_ui(z::Ref{RealFieldElem}, n::UInt, prec::Int)::Nothing
  return z
end

@doc raw"""
    bernoulli(n::Int, r::RealField)

Return the $n$-th Bernoulli number as an element of the given Arb field.
"""
bernoulli(n::Int, r::RealField, prec::Int = precision(Balls)) = n >= 0 ? bernoulli(UInt(n), r, prec) : throw(DomainError(n, "Index must be non-negative"))

function rising_factorial(x::RealFieldElem, n::UInt, prec::Int = precision(Balls))
  z = RealFieldElem()
  @ccall libflint.arb_rising_ui(z::Ref{RealFieldElem}, x::Ref{RealFieldElem}, n::UInt, prec::Int)::Nothing
  return z
end

@doc raw"""
    rising_factorial(x::RealFieldElem, n::Int)

Return the rising factorial $x(x + 1)\ldots (x + n - 1)$ as an Arb.
"""
rising_factorial(x::RealFieldElem, n::Int, prec::Int = precision(Balls)) = n < 0 ? throw(DomainError(n, "Index must be non-negative")) : rising_factorial(x, UInt(n), prec)

function rising_factorial(x::QQFieldElem, n::UInt, r::RealField, prec::Int = precision(Balls))
  z = r()
  @ccall libflint.arb_rising_fmpq_ui(z::Ref{RealFieldElem}, x::Ref{QQFieldElem}, n::UInt, prec::Int)::Nothing
  return z
end

@doc raw"""
    rising_factorial(x::QQFieldElem, n::Int, r::RealField)

Return the rising factorial $x(x + 1)\ldots (x + n - 1)$ as an element of the
given Arb field.
"""
rising_factorial(x::QQFieldElem, n::Int, r::RealField, prec::Int = precision(Balls)) = n < 0 ? throw(DomainError(n, "Index must be non-negative")) : rising_factorial(x, UInt(n), r, prec)

function rising_factorial2(x::RealFieldElem, n::UInt, prec::Int = precision(Balls))
  z = RealFieldElem()
  w = RealFieldElem()
  @ccall libflint.arb_rising2_ui(z::Ref{RealFieldElem}, w::Ref{RealFieldElem}, x::Ref{RealFieldElem}, n::UInt, prec::Int)::Nothing
  return (z, w)
end

@doc raw"""
    rising_factorial2(x::RealFieldElem, n::Int)

Return a tuple containing the rising factorial $x(x + 1)\ldots (x + n - 1)$
and its derivative.
"""
rising_factorial2(x::RealFieldElem, n::Int, prec::Int = precision(Balls)) = n < 0 ? throw(DomainError(n, "Index must be non-negative")) : rising_factorial2(x, UInt(n), prec)

function polylog(s::RealFieldElem, a::RealFieldElem, prec::Int = precision(Balls))
  z = parent(s)()
  @ccall libflint.arb_polylog(z::Ref{RealFieldElem}, s::Ref{RealFieldElem}, a::Ref{RealFieldElem}, prec::Int)::Nothing
  return z
end

function polylog(s::Int, a::RealFieldElem, prec::Int = precision(Balls))
  z = parent(a)()
  @ccall libflint.arb_polylog_si(z::Ref{RealFieldElem}, s::Int, a::Ref{RealFieldElem}, prec::Int)::Nothing
  return z
end

@doc raw"""
    polylog(s::Union{RealFieldElem,Int}, a::RealFieldElem)

Return the polylogarithm Li$_s(a)$.
""" polylog(s::Union{RealFieldElem,Int}, a::RealFieldElem)

function chebyshev_t(n::UInt, x::RealFieldElem, prec::Int = precision(Balls))
  z = RealFieldElem()
  @ccall libflint.arb_chebyshev_t_ui(z::Ref{RealFieldElem}, n::UInt, x::Ref{RealFieldElem}, prec::Int)::Nothing
  return z
end

function chebyshev_u(n::UInt, x::RealFieldElem, prec::Int = precision(Balls))
  z = RealFieldElem()
  @ccall libflint.arb_chebyshev_u_ui(z::Ref{RealFieldElem}, n::UInt, x::Ref{RealFieldElem}, prec::Int)::Nothing
  return z
end

function chebyshev_t2(n::UInt, x::RealFieldElem, prec::Int = precision(Balls))
  z = RealFieldElem()
  w = RealFieldElem()
  @ccall libflint.arb_chebyshev_t2_ui(z::Ref{RealFieldElem}, w::Ref{RealFieldElem}, n::UInt, x::Ref{RealFieldElem}, prec::Int)::Nothing
  return z, w
end

function chebyshev_u2(n::UInt, x::RealFieldElem, prec::Int = precision(Balls))
  z = RealFieldElem()
  w = RealFieldElem()
  @ccall libflint.arb_chebyshev_u2_ui(z::Ref{RealFieldElem}, w::Ref{RealFieldElem}, n::UInt, x::Ref{RealFieldElem}, prec::Int)::Nothing
  return z, w
end

@doc raw"""
    chebyshev_t(n::Int, x::RealFieldElem)

Return the value of the Chebyshev polynomial $T_n(x)$.
"""
chebyshev_t(n::Int, x::RealFieldElem, prec::Int = precision(Balls)) = n < 0 ? throw(DomainError(n, "Index must be non-negative")) : chebyshev_t(UInt(n), x, prec)

@doc raw"""
    chebyshev_u(n::Int, x::RealFieldElem)

Return the value of the Chebyshev polynomial $U_n(x)$.
"""
chebyshev_u(n::Int, x::RealFieldElem, prec::Int = precision(Balls)) = n < 0 ? throw(DomainError(n, "Index must be non-negative")) : chebyshev_u(UInt(n), x, prec)

@doc raw"""
    chebyshev_t2(n::Int, x::RealFieldElem)

Return the tuple $(T_{n}(x), T_{n-1}(x))$.
"""
chebyshev_t2(n::Int, x::RealFieldElem, prec::Int = precision(Balls)) = n < 0 ? throw(DomainError(n, "Index must be non-negative")) : chebyshev_t2(UInt(n), x, prec)

@doc raw"""
    chebyshev_u2(n::Int, x::RealFieldElem)

Return the tuple $(U_{n}(x), U_{n-1}(x))$
"""
chebyshev_u2(n::Int, x::RealFieldElem, prec::Int = precision(Balls)) = n < 0 ? throw(DomainError(n, "Index must be non-negative")) : chebyshev_u2(UInt(n), x, prec)

@doc raw"""
    bell(n::ZZRingElem, r::RealField)

Return the Bell number $B_n$ as an element of $r$.
"""
function bell(n::ZZRingElem, r::RealField, prec::Int = precision(Balls))
  z = r()
  @ccall libflint.arb_bell_fmpz(z::Ref{RealFieldElem}, n::Ref{ZZRingElem}, prec::Int)::Nothing
  return z
end

@doc raw"""
    bell(n::Int, r::RealField)

Return the Bell number $B_n$ as an element of $r$.
"""
bell(n::Int, r::RealField, prec::Int = precision(Balls)) = bell(ZZRingElem(n), r, prec)

@doc raw"""
    numpart(n::ZZRingElem, r::RealField)

Return the number of partitions $p(n)$ as an element of $r$.
"""
function numpart(n::ZZRingElem, r::RealField, prec::Int = precision(Balls))
  z = r()
  @ccall libflint.arb_partitions_fmpz(z::Ref{RealFieldElem}, n::Ref{ZZRingElem}, prec::Int)::Nothing
  return z
end

@doc raw"""
    numpart(n::Int, r::RealField)

Return the number of partitions $p(n)$ as an element of $r$.
"""
numpart(n::Int, r::RealField, prec::Int = precision(Balls)) = numpart(ZZRingElem(n), r, prec)

################################################################################
#
#  Hypergeometric and related functions
#
################################################################################

@doc raw"""
    airy_ai(x::RealFieldElem)

Return the Airy function $\operatorname{Ai}(x)$.
"""
function airy_ai(x::RealFieldElem, prec::Int = precision(Balls))
  ai = RealFieldElem()
  @ccall libflint.arb_hypgeom_airy(ai::Ref{RealFieldElem}, C_NULL::Ptr{Cvoid}, C_NULL::Ptr{Cvoid}, C_NULL::Ptr{Cvoid}, x::Ref{RealFieldElem}, prec::Int)::Nothing
  return ai
end

@doc raw"""
    airy_bi(x::RealFieldElem)

Return the Airy function $\operatorname{Bi}(x)$.
"""
function airy_bi(x::RealFieldElem, prec::Int = precision(Balls))
  bi = RealFieldElem()
  @ccall libflint.arb_hypgeom_airy(C_NULL::Ptr{Cvoid}, C_NULL::Ptr{Cvoid}, bi::Ref{RealFieldElem}, C_NULL::Ptr{Cvoid}, x::Ref{RealFieldElem}, prec::Int)::Nothing
  return bi
end

@doc raw"""
    airy_ai_prime(x::RealFieldElem)

Return the derivative of the Airy function $\operatorname{Ai}^\prime(x)$.
"""
function airy_ai_prime(x::RealFieldElem, prec::Int = precision(Balls))
  ai_prime = RealFieldElem()
  @ccall libflint.arb_hypgeom_airy(C_NULL::Ptr{Cvoid}, ai_prime::Ref{RealFieldElem}, C_NULL::Ptr{Cvoid}, C_NULL::Ptr{Cvoid}, x::Ref{RealFieldElem}, prec::Int)::Nothing
  return ai_prime
end

@doc raw"""
    airy_bi_prime(x::RealFieldElem)

Return the derivative of the Airy function $\operatorname{Bi}^\prime(x)$.
"""
function airy_bi_prime(x::RealFieldElem, prec::Int = precision(Balls))
  bi_prime = RealFieldElem()
  @ccall libflint.arb_hypgeom_airy(C_NULL::Ptr{Cvoid}, C_NULL::Ptr{Cvoid}, C_NULL::Ptr{Cvoid}, bi_prime::Ref{RealFieldElem}, x::Ref{RealFieldElem}, prec::Int)::Nothing
  return bi_prime
end

################################################################################
#
#  Linear dependence
#
################################################################################

@doc raw"""
    lindep(A::Vector{RealFieldElem}, bits::Int)

Find a small linear combination of the entries of the array $A$ that is small
(using LLL). The entries are first scaled by the given number of bits before
truncating to integers for use in LLL. This function can be used to find linear
dependence between a list of real numbers. The algorithm is heuristic only and
returns an array of Nemo integers representing the linear combination.

# Examples

```jldoctest
julia> RR = RealField()
Real field

julia> a = RR(-0.33198902958450931620250069492231652319)
[-0.33198902958450932088 +/- 4.15e-22]

julia> V = [RR(1), a, a^2, a^3, a^4, a^5]
6-element Vector{RealFieldElem}:
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
function lindep(A::Vector{RealFieldElem}, bits::Int)
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
      simplest_rational_inside(x::RealFieldElem)

Return the simplest fraction inside the ball $x$. A canonical fraction
$a_1/b_1$ is defined to be simpler than $a_2/b_2$ iff $b_1 < b_2$ or $b_1 =
b_2$ and $a_1 < a_2$.

# Examples

```jldoctest
julia> RR = RealField()
Real field

julia> simplest_rational_inside(const_pi(RR))
8717442233//2774848045
```
"""
function simplest_rational_inside(x::RealFieldElem)
  a = ZZRingElem()
  b = ZZRingElem()
  e = ZZRingElem()

  @ccall libflint.arb_get_interval_fmpz_2exp(a::Ref{ZZRingElem}, b::Ref{ZZRingElem}, e::Ref{ZZRingElem}, x::Ref{RealFieldElem})::Nothing
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

function zero!(z::RealFieldElemOrPtr)
  @ccall libflint.arb_zero(z::Ref{RealFieldElem})::Nothing
  return z
end

function one!(z::RealFieldElemOrPtr)
  @ccall libflint.arb_one(z::Ref{RealFieldElem})::Nothing
  return z
end

function neg!(z::RealFieldElemOrPtr, a::RealFieldElemOrPtr)
  @ccall libflint.arb_neg(z::Ref{RealFieldElem}, a::Ref{RealFieldElem})::Nothing
  return z
end

for (s,f) in (("add!","arb_add"), ("mul!","arb_mul"), ("div!", "arb_div"),
              ("sub!","arb_sub"))
  @eval begin
    function ($(Symbol(s)))(z::RealFieldElemOrPtr, x::RealFieldElemOrPtr, y::RealFieldElemOrPtr, prec::Int = precision(Balls))
      @ccall libflint.$f(z::Ref{RealFieldElem}, x::Ref{RealFieldElem}, y::Ref{RealFieldElem}, prec::Int)::Nothing
      return z
    end
  end
end

################################################################################
#
#  Unsafe setting
#
################################################################################

function _arb_set(x::RealFieldElemOrPtr, y::Int)
  @ccall libflint.arb_set_si(x::Ref{RealFieldElem}, y::Int)::Nothing
end

function _arb_set(x::RealFieldElemOrPtr, y::UInt)
  @ccall libflint.arb_set_ui(x::Ref{RealFieldElem}, y::UInt)::Nothing
end

function _arb_set(x::RealFieldElemOrPtr, y::Float64)
  @ccall libflint.arb_set_d(x::Ref{RealFieldElem}, y::Float64)::Nothing
end

function _arb_set(x::RealFieldElemOrPtr, y::Union{Int,UInt,Float64}, p::Int)
  _arb_set(x, y)
  @ccall libflint.arb_set_round(x::Ref{RealFieldElem}, x::Ref{RealFieldElem}, p::Int)::Nothing
end

function _arb_set(x::RealFieldElemOrPtr, y::ZZRingElem)
  @ccall libflint.arb_set_fmpz(x::Ref{RealFieldElem}, y::Ref{ZZRingElem})::Nothing
end

function _arb_set(x::RealFieldElemOrPtr, y::ZZRingElem, p::Int)
  @ccall libflint.arb_set_round_fmpz(x::Ref{RealFieldElem}, y::Ref{ZZRingElem}, p::Int)::Nothing
end

function _arb_set(x::RealFieldElemOrPtr, y::QQFieldElem, p::Int)
  @ccall libflint.arb_set_fmpq(x::Ref{RealFieldElem}, y::Ref{QQFieldElem}, p::Int)::Nothing
end

function _arb_set(x::RealFieldElemOrPtr, y::RealFieldElemOrPtr)
  @ccall libflint.arb_set(x::Ref{RealFieldElem}, y::Ref{RealFieldElem})::Nothing
end

function _arb_set(x::RealFieldElemOrPtr, y::Ptr{arb_struct})
  @ccall libflint.arb_set(x::Ref{RealFieldElem}, y::Ptr{arb_struct})::Nothing
end

function _arb_set(x::Ptr{arb_struct}, y::RealFieldElemOrPtr)
  @ccall libflint.arb_set(x::Ptr{arb_struct}, y::Ref{RealFieldElem})::Nothing
end

function _arb_set(x::RealFieldElemOrPtr, y::RealFieldElemOrPtr, p::Int)
  @ccall libflint.arb_set_round(x::Ref{RealFieldElem}, y::Ref{RealFieldElem}, p::Int)::Nothing
end

function _arb_set(x::RealFieldElemOrPtr, y::AbstractString, p::Int)
  s = string(y)
  err = @ccall libflint.arb_set_str(x::Ref{RealFieldElem}, s::Ptr{UInt8}, p::Int)::Int32
  err == 0 || error("Invalid real string: $(repr(s))")
end

function _arb_set(x::RealFieldElemOrPtr, y::BigFloat)
  m = _mid_ptr(x)
  r = _rad_ptr(x)
  @ccall libflint.arf_set_mpfr(m::Ptr{arf_struct}, y::Ref{BigFloat})::Nothing
  @ccall libflint.mag_zero(r::Ptr{mag_struct})::Nothing
end

function _arb_set(x::RealFieldElemOrPtr, y::BigFloat, p::Int)
  _arb_set(x, y)
  @ccall libflint.arb_set_round(x::Ref{RealFieldElem}, x::Ref{RealFieldElem}, p::Int)::Nothing
end

function _arb_set(x::RealFieldElemOrPtr, y::Integer)
  _arb_set(x, ZZRingElem(y))
end

function _arb_set(x::RealFieldElemOrPtr, y::Integer, p::Int)
  _arb_set(x, ZZRingElem(y), p)
end

function _arb_set(x::RealFieldElemOrPtr, y::Real)
  _arb_set(x, BigFloat(y))
end

function _arb_set(x::RealFieldElemOrPtr, y::Real, p::Int)
  _arb_set(x, BigFloat(y), p)
end

function _arb_set(x::RealFieldElemOrPtr, y::Irrational)
  _arb_set(x, y, precision(Balls))
end

function _arb_set(x::RealFieldElemOrPtr, y::Irrational, p::Int)
  if y == pi
    @ccall libflint.arb_const_pi(x::Ref{RealFieldElem}, p::Int)::Nothing
  elseif y == MathConstants.e
    @ccall libflint.arb_const_e(x::Ref{RealFieldElem}, p::Int)::Nothing
  elseif y == MathConstants.catalan
    @ccall libflint.arb_const_catalan(x::Ref{RealFieldElem}, p::Int)::Nothing
  elseif y == MathConstants.eulergamma
    @ccall libflint.arb_const_euler(x::Ref{RealFieldElem}, p::Int)::Nothing
  else
    _arb_set(x, BigFloat(y; precision=p), p)
  end
end

################################################################################
#
#  Parent object overloading
#
################################################################################

(r::RealField)() = RealFieldElem()

(r::RealField)(x::Any, prec::Int) = RealFieldElem(x, prec)

function (r::RealField)(x::Irrational, prec::Int)
  z = r()
  _arb_set(z, x, prec)
  return z
end

(r::RealField)(x::Any; precision::Int = precision(Balls)) = r(x, precision)

################################################################################
#
#  Arb real field constructor
#
################################################################################

# see inner constructor for RealField

################################################################################
#
#  Random generation
#
################################################################################

@doc raw"""
    rand(r::RealField; randtype::Symbol=:urandom)

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
function rand(r::RealField, prec::Int = precision(Balls); randtype::Symbol=:urandom)
  state = _flint_rand_states[Threads.threadid()]
  x = r()

  if randtype == :urandom
    @ccall libflint.arb_urandom(x::Ref{RealFieldElem}, state::Ref{rand_ctx}, prec::Int)::Nothing
  elseif randtype == :randtest
    @ccall libflint.arb_randtest(x::Ref{RealFieldElem}, state::Ref{rand_ctx}, prec::Int, 30::Int)::Nothing
  elseif randtype == :randtest_exact
    @ccall libflint.arb_randtest_exact(x::Ref{RealFieldElem}, state::Ref{rand_ctx}, prec::Int, 30::Int)::Nothing
  elseif randtype == :randtest_precise
    @ccall libflint.arb_randtest_precise(x::Ref{RealFieldElem}, state::Ref{rand_ctx}, prec::Int, 30::Int)::Nothing
  elseif randtype == :randtest_wide
    @ccall libflint.arb_randtest_wide(x::Ref{RealFieldElem}, state::Ref{rand_ctx}, prec::Int, 30::Int)::Nothing
  elseif randtype == :randtest_special
    @ccall libflint.arb_randtest_special(x::Ref{RealFieldElem}, state::Ref{rand_ctx}, prec::Int, 30::Int)::Nothing
  else
    error("Arb random generation `" * String(randtype) * "` is not defined")
  end

  return x
end
