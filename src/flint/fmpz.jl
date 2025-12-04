###############################################################################
#
#   ZZRingElem.jl : BigInts
#
###############################################################################

# Copyright (c) 2009-2014: Jeff Bezanson, Stefan Karpinski, Viral B. Shah,
# and other contributors:
#
# https://github.com/JuliaLang/julia/contributors
#
# Copyright (C) 2014, 2015 William Hart
# Copyright (C) 2015, Claus Fieker
#
# Permission is hereby granted, free of charge, to any person obtaining
# a copy of this software and associated documentation files (the
# "Software"), to deal in the Software without restriction, including
# without limitation the rights to use, copy, modify, merge, publish,
# distribute, sublicense, and/or sell copies of the Software, and to
# permit persons to whom the Software is furnished to do so, subject to
# the following conditions:
#
# The above copyright notice and this permission notice shall be
# included in all copies or substantial portions of the Software.
#
# THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND,
# EXPRESS OR IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF
# MERCHANTABILITY, FITNESS FOR A PARTICULAR PURPOSE AND
# NONINFRINGEMENT. IN NO EVENT SHALL THE AUTHORS OR COPYRIGHT HOLDERS BE
# LIABLE FOR ANY CLAIM, DAMAGES OR OTHER LIABILITY, WHETHER IN AN ACTION
# OF CONTRACT, TORT OR OTHERWISE, ARISING FROM, OUT OF OR IN CONNECTION
# WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS IN THE SOFTWARE.


###############################################################################
#
#   Data type and parent methods
#
###############################################################################

parent_type(::Type{ZZRingElem}) = ZZRing

@doc raw"""
    parent(a::ZZRingElem)

Returns the unique FLINT integer parent object `ZZ`.
"""
parent(a::ZZRingElem) = ZZ

elem_type(::Type{ZZRing}) = ZZRingElem

base_ring_type(::Type{ZZRing}) = Union{}

is_domain_type(::Type{ZZRingElem}) = true

###############################################################################
#
#   Internal data
#
###############################################################################

data(a::ZZRingElem) = a.d
data(a::Ref{ZZRingElem}) = a[].d
data(a::Ptr{ZZRingElem}) = unsafe_load(reinterpret(Ptr{Int}, a))

###############################################################################
#
#   Ranges
#
###############################################################################

# ZZRingElem needs to be iterable for
# Base.throw_boundserror(something, ::ZZRingElem) to print correctly
Base.iterate(x::ZZRingElem) = (x, nothing)
Base.iterate(x::ZZRingElem, i::Any) = nothing

# Note that we cannot get a UnitRange as this is only legal for subtypes of Real.
# So, we use an AbstractUnitRange here mostly copied from `base/range.jl`.
# `StepRange`s on the other hand work out of the box thanks to duck typing.

# See FlintTypes.jl for the struct ZZRingElemUnitRange

fmpz_unitrange_last(start::ZZRingElem, stop::ZZRingElem) =
ifelse(stop >= start, stop, start - one(ZZRingElem))

Base.:(:)(a::ZZRingElem, b::ZZRingElem) = ZZRingElemUnitRange(a, b)

Base.OneTo(n::ZZRingElem) = ZZRingElemUnitRange(parent(n)(1), n)

@inline function getindex(r::ZZRingElemUnitRange, i::ZZRingElem)
  val = r.start + (i - 1)
  @boundscheck _in_unit_range(r, val) || Base.throw_boundserror(r, i)
  val
end
_in_unit_range(r::ZZRingElemUnitRange, val::ZZRingElem) = r.start <= val <= r.stop

show(io::IO, r::ZZRingElemUnitRange) = print(io, repr(first(r)), ':', repr(last(r)))

in(x::IntegerUnion, r::ZZRingElemUnitRange) = first(r) <= x <= last(r)

mod(i::IntegerUnion, r::ZZRingElemUnitRange) = mod(i - first(r), length(r)) + first(r)

Base.:(:)(a::ZZRingElem, b::Integer) = (:)(promote(a, b)...)
Base.:(:)(a::Integer, b::ZZRingElem) = (:)(promote(a, b)...)

Base.:(:)(x::Int, y::ZZRingElem) = ZZRingElem(x):y
Base.:(:)(x::ZZRingElem, y::Int) = x:ZZRingElem(y)

# Construct StepRange{ZZRingElem, T} where +(::ZZRingElem, zero(::T)) must be defined
Base.:(:)(a::ZZRingElem, s, b::Integer) = ((a_, b_) = promote(a, b); a_:s:b_)
Base.:(:)(a::Integer, s, b::ZZRingElem) = ((a_, b_) = promote(a, b); a_:s:b_)

# `length` should return an Integer, so BigInt seems appropriate as ZZRingElem is not <: Integer
# this method is useful in particular to enable rand(ZZ(n):ZZ(m))
function Base.length(r::StepRange{ZZRingElem})
  n = div((last(r) - first(r)) + step(r), step(r))
  isempty(r) ? zero(BigInt) : BigInt(n)
end

function Base.in(x::IntegerUnion, r::AbstractRange{ZZRingElem})
  if isempty(r) || first(r) > x || x > last(r)
    return false
  end
  return mod(convert(ZZRingElem, x), step(r)) == mod(first(r), step(r))
end

function Base.getindex(a::StepRange{ZZRingElem}, i::ZZRingElem)
  res = first(a) + (i - 1) * Base.step(a)
  ok = false
  if step(a) > 0
    ok = res <= last(a) && res >= first(a)
  else
    ok = res >= last(a) && res <= first(a)
  end
  @boundscheck ((i > 0) && ok) || Base.throw_boundserror(a, i)
  return res
end

################################################################################
#
#   Low-level direct access
#
################################################################################

@inline function __fmpz_is_small(a::Int)
  return (unsigned(a) >> (Sys.WORD_SIZE - 2) != 1)
end

@inline function _fmpz_is_small(a::ZZRingElemOrPtr)
  return __fmpz_is_small(data(a))
end

# "views" a non-small ZZRingElem as a readonly BigInt
# the caller must ensure z is actually non-small, and
# call GC.@preserve appropriately
function _as_bigint(a::ZZRingElemOrPtr)
  unsafe_load(Ptr{BigInt}(data(a) << 2))
end

@inline function _fmpz_size(a::ZZRingElemOrPtr)
  return _fmpz_is_small(a) ? 1 : GC.@preserve a _as_bigint(a).size
end

################################################################################
#
#   Hashing
#
################################################################################

function hash_integer(a::ZZRingElem, h::UInt)
  return GC.@preserve a _hash_integer(data(a), h)
end

function hash(a::ZZRingElem, h::UInt)
  _fmpz_is_small(a) && return Base.hash(data(a), h)
  return hash_integer(a, h)
end

function _hash_integer(a::Int, h::UInt)
  __fmpz_is_small(a) && return Base.hash_integer(a, h)
  # view it as a BigInt
  z = unsafe_load(Ptr{BigInt}(unsigned(a) << 2))
  s = z.size  # number of active limbs
  p = z.d     # pointer to the limbs
  b = unsafe_load(p)
  h = xor(Base.hash_uint(xor(ifelse(s < 0, -b, b), h)), h)
  for k = 2:abs(s)
    h = xor(Base.hash_uint(xor(unsafe_load(p, k), h)), h)
  end
  return h
end

###############################################################################
#
#   Basic manipulation
#
###############################################################################

function deepcopy_internal(a::ZZRingElem, dict::IdDict)
  z = ZZRingElem()
  set!(z, a)
  return z
end

Base.copy(a::ZZRingElem) = deepcopy(a)

characteristic(R::ZZRing) = 0

one(::ZZRing) = ZZRingElem(1)

zero(::ZZRing) = ZZRingElem(0)

one(::Type{ZZRingElem}) = ZZRingElem(1)

zero(::Type{ZZRingElem}) = ZZRingElem(0)

krull_dim(::ZZRing) = 1

is_noetherian(::ZZRing) = true

@doc raw"""
    sign(a::ZZRingElem)

Return the sign of $a$, i.e. $+1$, $0$ or $-1$.
"""
sign(a::ZZRingElemOrPtr) = ZZRingElem(sign(Int, a))

sign(::Type{Int}, a::ZZRingElemOrPtr) = is_zero(a) ? 0 : (signbit(a) ? -1 : 1)

Base.signbit(a::ZZRingElemOrPtr) = _fmpz_is_small(a) ? signbit(data(a)) : signbit(_fmpz_size(a))

is_negative(n::ZZRingElemOrPtr) = sign(Int, n) < 0
is_positive(n::ZZRingElemOrPtr) = sign(Int, n) > 0

@doc raw"""
    fits(::Type{Int}, a::ZZRingElem)

Return `true` if $a$ fits into an `Int`, otherwise return `false`.
"""
function fits(::Type{Int}, a::ZZRingElem)
  _fmpz_is_small(a) && return true
  n = _fmpz_size(a)
  n == 1 && return rem(a, UInt) <= typemax(Int)
  n == -1 && return rem(a, UInt) >= reinterpret(UInt,typemin(Int))
  return false
end

@doc raw"""
    fits(::Type{UInt}, a::ZZRingElem)

Return `true` if $a$ fits into a `UInt`, otherwise return `false`.
"""
function fits(::Type{UInt}, a::ZZRingElem)
  return !is_negative(a) && size(a) == 1
end

function fits(::Type{T}, a::ZZRingElem) where {T <: Union{Int32,Int16,Int8,UInt32,UInt16,UInt8}}
  return _fmpz_is_small(a) && typemin(T) <= data(a) <= typemax(T)
end

function fits(::Type{T}, a::ZZRingElem) where {T <: Integer}
  return typemin(T) <= a <= typemax(T)
end

@doc raw"""
    size(a::ZZRingElem)

Return the number of limbs required to store the absolute value of $a$.
"""
size(a::ZZRingElem) = _fmpz_is_small(a) ? 1 : abs(_fmpz_size(a))

is_unit(a::ZZRingElemOrPtr) = data(a) == 1 || data(a) == -1

is_zero(a::ZZRingElemOrPtr) = data(a) == 0

is_one(a::ZZRingElemOrPtr) = data(a) == 1

isinteger(::ZZRingElem) = true

isfinite(::ZZRingElem) = true

isinf(::ZZRingElem) = false

@doc raw"""
    denominator(a::ZZRingElem)

Return the denominator of $a$ thought of as a rational. Always returns $1$.
"""
function denominator(a::ZZRingElem)
  return ZZRingElem(1)
end

@doc raw"""
    numerator(a::ZZRingElem)

Return the numerator of $a$ thought of as a rational. Always returns $a$.
"""
function numerator(a::ZZRingElem)
  return a
end

isodd(a::ZZRingElemOrPtr) = isodd(a % UInt)
iseven(a::ZZRingElemOrPtr) = !isodd(a)

###############################################################################
#
#   AbstractString I/O
#
###############################################################################

# ZZRingElem is allowed as a leaf, and the following code is needed by AA's api
expressify(x::ZZRingElem; context = nothing) = x

function AbstractAlgebra.get_syntactic_sign_abs(obj::ZZRingElem)
  return obj < 0 ? (-1, -obj) : (1, obj)
end

AbstractAlgebra.is_syntactic_one(x::ZZRingElem) = isone(x)

AbstractAlgebra.is_syntactic_zero(x::ZZRingElem) = iszero(x)

function AbstractAlgebra.print_obj(S::AbstractAlgebra.printer, mi::MIME,
    obj::ZZRingElem, left::Int, right::Int)
  AbstractAlgebra.print_integer_string(S, mi, string(obj), left, right)
end

string(x::ZZRingElem) = dec(x)

show(io::IO, x::ZZRingElem) = print(io, string(x))

function show(io::IO, a::ZZRing)
  # deliberately no @show_name or @show_special here as this is a singleton type
  if is_terse(io)
    print(pretty(io), LowercaseOff(), "ZZ")
  else
    print(io, "Integer ring")
  end
end



###############################################################################
#
#   Canonicalisation
#
###############################################################################

canonical_unit(x::ZZRingElem) = x < 0 ? ZZRingElem(-1) : ZZRingElem(1)

###############################################################################
#
#   Unary operators and functions, e.g. -ZZRingElem(12), ~ZZRingElem(12)
#
###############################################################################

function -(x::ZZRingElem)
  z = ZZRingElem()
  neg!(z, x)
  return z
end

function ~(x::ZZRingElem)
  z = ZZRingElem()
  @ccall libflint.fmpz_complement(z::Ref{ZZRingElem}, x::Ref{ZZRingElem})::Nothing
  return z
end

function abs(x::ZZRingElem)
  z = ZZRingElem()
  @ccall libflint.fmpz_abs(z::Ref{ZZRingElem}, x::Ref{ZZRingElem})::Nothing
  return z
end

function abs2(x::ZZRingElem)
  return x^2
end

floor(x::ZZRingElem) = x
ceil(x::ZZRingElem) = x
trunc(x::ZZRingElem) = x
round(x::ZZRingElem) = x

floor(::Type{ZZRingElem}, x::ZZRingElem) = x
ceil(::Type{ZZRingElem}, x::ZZRingElem) = x
trunc(::Type{ZZRingElem}, x::ZZRingElem) = x
round(::Type{ZZRingElem}, x::ZZRingElem) = x

conj(x::ZZRingElem) = x

###############################################################################
#
#   Binary operators and functions
#
###############################################################################

# Metaprogram to define functions +, -, *, gcd, lcm,
#                                 &, |, $ (xor)

for (fJ, fC) in ((:+, :add), (:-,:sub), (:*, :mul),
                 (:&, :and), (:|, :or), (:xor, :xor))
  @eval begin
    function ($fJ)(x::ZZRingElem, y::ZZRingElem)
      z = ZZRingElem()
      @ccall libflint.$("fmpz_$fC")(z::Ref{ZZRingElem}, x::Ref{ZZRingElem}, y::Ref{ZZRingElem})::Nothing
      return z
    end
  end
end

# Metaprogram to define functions fdiv, cdiv, tdiv, div

for (fJ, fC) in ((:fdiv, :fdiv_q), (:cdiv, :cdiv_q), (:tdiv, :tdiv_q),
                 (:div, :fdiv_q))
  @eval begin
    function ($fJ)(x::ZZRingElem, y::ZZRingElem)
      iszero(y) && throw(DivideError())
      z = ZZRingElem()
      @ccall libflint.$("fmpz_$fC")(z::Ref{ZZRingElem}, x::Ref{ZZRingElem}, y::Ref{ZZRingElem})::Nothing
      return z
    end
  end
end

# N.B. we do not export the internal definition of div
# which agrees with the internal definition of AbstractAlgebra
# Here we set Base.div to a version that agrees with Base
function Base.div(x::ZZRingElem, y::ZZRingElem)
  iszero(y) && throw(DivideError())
  z = ZZRingElem()
  @ccall libflint.fmpz_tdiv_q(z::Ref{ZZRingElem}, x::Ref{ZZRingElem}, y::Ref{ZZRingElem})::Nothing
  return z
end

Base.div(x::ZZRingElem, y::ZZRingElem, ::typeof(RoundToZero)) = tdiv(x, y)
Base.div(x::ZZRingElem, y::ZZRingElem, ::typeof(RoundUp)) = cdiv(x, y)
Base.div(x::ZZRingElem, y::ZZRingElem, ::typeof(RoundDown)) = fdiv(x, y)

function divexact(x::ZZRingElem, y::ZZRingElem; check::Bool=true)
  iszero(y) && throw(DivideError())
  if check
    z, r = tdivrem(x, y)
    is_zero(r) || throw(ArgumentError("Not an exact division"))
  else
    z = divexact!(ZZRingElem(), x, y)
  end
  return z
end

function AbstractAlgebra.divides!(z::ZZRingElem, x::ZZRingElem, y::ZZRingElem)
  res = @ccall libflint.fmpz_divides(z::Ref{ZZRingElem}, x::Ref{ZZRingElem}, y::Ref{ZZRingElem})::Bool
  return res, z
end

function AbstractAlgebra.divides(x::ZZRingElem, y::ZZRingElem)
  z = ZZRingElem()
  return divides!(z, x, y)
end

divides(x::ZZRingElem, y::Integer) = divides(x, ZZRingElem(y))

@doc raw"""
    is_divisible_by(x::ZZRingElem, y::ZZRingElem)

Return `true` if $x$ is divisible by $y$, otherwise return `false`.
"""
function is_divisible_by(x::ZZRingElem, y::ZZRingElem)
  if iszero(x)
    return true
  elseif iszero(y)
    return false
  elseif iseven(y) && isodd(x)
    return false
  elseif nbits(y) > nbits(x)
    return false
  else
    flag, q = divides(x, y)
    return flag
  end
end

@doc raw"""
    is_divisible_by(x::ZZRingElem, y::ZZRingElem)

Return `true` if $x$ is divisible by $y$, otherwise return `false`.
"""
function is_divisible_by(x::ZZRingElem, y::Integer)
  if iszero(x)
    return true
  elseif iszero(y)
    return false
  elseif iseven(y) && isodd(x)
    return false
  elseif ndigits(y, base=2) > nbits(x)
    return false
  else
    r = mod(x, y)
    return r == 0
  end
end

function is_divisible_by(x::Integer, y::ZZRingElem)
  return is_divisible_by(ZZRingElem(x), y)
end

function rem(x::ZZRingElemOrPtr, ::Type{UInt})
  return _fmpz_is_small(x) ? (data(x) % UInt) : GC.@preserve x (_as_bigint(x) % UInt)
end

function rem(x::ZZRingElem, c::ZZRingElem)
  # FIXME: it seems `rem` and `rem!` for `ZZRingElem` do different things?
  q, r = Base.divrem(x, c)
  return r
end

function rem(a::ZZRingElem, b::UInt)
  return @ccall libflint.fmpz_fdiv_ui(a::Ref{ZZRingElem}, b::UInt)::UInt
end

###############################################################################
#
#   Ad hoc binary operators
#
###############################################################################

for T in (Int, UInt, Integer)
  @eval begin
    +(a::ZZRingElem, b::$T) = add!(ZZRingElem(), a, b)
    +(a::$T, b::ZZRingElem) = add!(ZZRingElem(), a, b)

    -(a::ZZRingElem, b::$T) = sub!(ZZRingElem(), a, b)
    -(a::$T, b::ZZRingElem) = sub!(ZZRingElem(), a, b)

    *(a::ZZRingElem, b::$T) = mul!(ZZRingElem(), a, b)
    *(a::$T, b::ZZRingElem) = mul!(ZZRingElem(), a, b)
  end
end

*(a::ZZRingElem, b::AbstractFloat) = BigInt(a) * b
*(a::AbstractFloat, b::ZZRingElem) = a * BigInt(b)

/(a::AbstractFloat, b::ZZRingElem) = a / BigInt(b)

###############################################################################
#
#   Rounding
#
###############################################################################

for sym in (:trunc, :round, :ceil, :floor)
  @eval begin
    # support `trunc(ZZRingElem, 1.23)` etc. for arbitrary reals
    Base.$sym(::Type{ZZRingElem}, a::Real) = ZZRingElem(Base.$sym(BigInt, a))
    Base.$sym(::Type{ZZRingElem}, a::Rational) = ZZRingElem(Base.$sym(BigInt, a))
    Base.$sym(::Type{ZZRingElem}, a::Rational{T}) where T = ZZRingElem(Base.$sym(BigInt, a))
    Base.$sym(::Type{ZZRingElem}, a::Rational{Bool}) = ZZRingElem(Base.$sym(BigInt, a))

    # for integers we don't need to round in between
    Base.$sym(::Type{ZZRingElem}, a::Integer) = ZZRingElem(a)

    # support `trunc(ZZRingElem, m)` etc. where m is a matrix of reals
    function Base.$sym(::Type{ZZMatrix}, a::Matrix{<:Real})
      s = Base.size(a)
      m = zero_matrix(ZZ, s[1], s[2])
      for i = 1:s[1], j = 1:s[2]
        m[i, j] = Base.$sym(ZZRingElem, a[i, j])
      end
      return m
    end

    # rounding QQFieldElem to integer via ZZRingElem
    function Base.$sym(::Type{T}, a::QQFieldElem) where T <: Integer
      return T(Base.$sym(ZZRingElem, a))
    end
  end
end

###############################################################################
#
#   Ad hoc exact division
#
###############################################################################

divexact(x::ZZRingElem, y::Integer; check::Bool=true) = divexact(x, ZZRingElem(y); check=check)

divexact(x::Integer, y::ZZRingElem; check::Bool=true) = divexact(ZZRingElem(x), y; check=check)

###############################################################################
#
#   Ad hoc division
#
###############################################################################

function tdivpow2(x::ZZRingElem, c::Int)
  c < 0 && throw(DomainError(c, "Exponent must be non-negative"))
  z = ZZRingElem()
  @ccall libflint.fmpz_tdiv_q_2exp(z::Ref{ZZRingElem}, x::Ref{ZZRingElem}, c::Int)::Nothing
  return z
end

function fdivpow2(x::ZZRingElem, c::Int)
  c < 0 && throw(DomainError(c, "Exponent must be non-negative"))
  z = ZZRingElem()
  @ccall libflint.fmpz_fdiv_q_2exp(z::Ref{ZZRingElem}, x::Ref{ZZRingElem}, c::Int)::Nothing
  return z
end

function fmodpow2(x::ZZRingElem, c::Int)
  c < 0 && throw(DomainError(c, "Exponent must be non-negative"))
  z = ZZRingElem()
  @ccall libflint.fmpz_fdiv_r_2exp(z::Ref{ZZRingElem}, x::Ref{ZZRingElem}, c::Int)::Nothing
  return z
end

function cdivpow2(x::ZZRingElem, c::Int)
  c < 0 && throw(DomainError(c, "Exponent must be non-negative"))
  z = ZZRingElem()
  @ccall libflint.fmpz_cdiv_q_2exp(z::Ref{ZZRingElem}, x::Ref{ZZRingElem}, c::Int)::Nothing
  return z
end

function tdiv(x::ZZRingElem, c::Int)
  c == 0 && throw(DivideError())
  z = ZZRingElem()
  @ccall libflint.fmpz_tdiv_q_si(z::Ref{ZZRingElem}, x::Ref{ZZRingElem}, c::Int)::Nothing
  return z
end

function fdiv(x::ZZRingElem, c::Int)
  c == 0 && throw(DivideError())
  z = ZZRingElem()
  @ccall libflint.fmpz_fdiv_q_si(z::Ref{ZZRingElem}, x::Ref{ZZRingElem}, c::Int)::Nothing
  return z
end

function cdiv(x::ZZRingElem, c::Int)
  c == 0 && throw(DivideError())
  z = ZZRingElem()
  @ccall libflint.fmpz_cdiv_q_si(z::Ref{ZZRingElem}, x::Ref{ZZRingElem}, c::Int)::Nothing
  return z
end

rem(x::Integer, y::ZZRingElem) = rem(ZZRingElem(x), y)

rem(x::ZZRingElem, y::Integer) = rem(x, ZZRingElem(y))

mod(x::Integer, y::ZZRingElem) = mod(ZZRingElem(x), y)

@doc raw"""
    mod(x::ZZRingElem, y::Integer)

Return the remainder after division of $x$ by $y$. The remainder will be
closer to zero than $y$ and have the same sign, or it will be zero.
"""
mod(x::ZZRingElem, y::Integer) = mod(x, ZZRingElem(y))

div(x::Integer, y::ZZRingElem) = div(ZZRingElem(x), y)

# Note Base.div is different to Nemo.div
Base.div(x::Integer, y::ZZRingElem) = Base.div(ZZRingElem(x), y)
Base.div(x::Integer, y::ZZRingElem, r::RoundingMode) = Base.div(ZZ(x), y, r)

div(x::ZZRingElem, y::Integer) = div(x, ZZRingElem(y))

# Note Base.div is different to Nemo.div
Base.div(x::ZZRingElem, y::Integer) = Base.div(x, ZZRingElem(y))
Base.div(x::ZZRingElem, y::Integer, r::RoundingMode) = Base.div(x, ZZ(y), r)

divrem(x::ZZRingElem, y::Integer) = divrem(x, ZZRingElem(y))

divrem(x::Integer, y::ZZRingElem) = divrem(ZZRingElem(x), y)

# Without the functions below, Julia defaults to `(div(x, y), rem(x, y))`
Base.divrem(x::ZZRingElem, y::Integer) = Base.divrem(x, ZZ(y))
Base.divrem(x::ZZRingElem, y::Integer, r::RoundingMode) = Base.divrem(x, ZZ(y), r)
Base.divrem(x::Integer, y::ZZRingElem) = Base.divrem(ZZ(x), y)
Base.divrem(x::Integer, y::ZZRingElem, r::RoundingMode) = Base.divrem(ZZ(x), y, r)

###############################################################################
#
#   Division with remainder
#
###############################################################################

function divrem(x::ZZRingElem, y::ZZRingElem)
  return fdivrem(x, y)
end

# N.B. Base.divrem differs from Nemo.divrem
Base.divrem(x::ZZRingElem, y::ZZRingElem) = tdivrem(x, y)
Base.divrem(x::ZZRingElem, y::ZZRingElem, ::typeof(RoundToZero)) = tdivrem(x, y)
Base.divrem(x::ZZRingElem, y::ZZRingElem, ::typeof(RoundUp)) = cdivrem(x, y)
Base.divrem(x::ZZRingElem, y::ZZRingElem, ::typeof(RoundDown)) = fdivrem(x, y)

function tdivrem(x::ZZRingElem, y::ZZRingElem)
  iszero(y) && throw(DivideError())
  z1 = ZZRingElem()
  z2 = ZZRingElem()
  @ccall libflint.fmpz_tdiv_qr(z1::Ref{ZZRingElem}, z2::Ref{ZZRingElem}, x::Ref{ZZRingElem}, y::Ref{ZZRingElem})::Nothing
  z1, z2
end

function fdivrem(x::ZZRingElem, y::ZZRingElem)
  iszero(y) && throw(DivideError())
  z1 = ZZRingElem()
  z2 = ZZRingElem()
  @ccall libflint.fmpz_fdiv_qr(z1::Ref{ZZRingElem}, z2::Ref{ZZRingElem}, x::Ref{ZZRingElem}, y::Ref{ZZRingElem})::Nothing
  z1, z2
end

function cdivrem(x::ZZRingElem, y::ZZRingElem)
  iszero(y) && throw(DivideError())
  z1 = ZZRingElem()
  z2 = ZZRingElem()
  @ccall libflint.fmpz_cdiv_qr(z1::Ref{ZZRingElem}, z2::Ref{ZZRingElem}, x::Ref{ZZRingElem}, y::Ref{ZZRingElem})::Nothing
  z1, z2
end

function ntdivrem(x::ZZRingElem, y::ZZRingElem)
  # ties are the only possible remainders
  return ndivrem(x, y)
end

function nfdivrem(a::ZZRingElem, b::ZZRingElem)
  q, r = tdivrem(a, b)
  c = @ccall libflint.fmpz_cmp2abs(b::Ref{ZZRingElem}, r::Ref{ZZRingElem})::Cint
  if c <= 0
    if sign(Int, b) != sign(Int, r)
      sub!(q, q, UInt(1))
      add!(r, r, b)
    elseif c < 0
      add!(q, q, UInt(1))
      sub!(r, r, b)
    end
  end
  return (q, r)
end

function ncdivrem(a::ZZRingElem, b::ZZRingElem)
  q, r = tdivrem(a, b)
  c = @ccall libflint.fmpz_cmp2abs(b::Ref{ZZRingElem}, r::Ref{ZZRingElem})::Cint
  if c <= 0
    if sign(Int, b) == sign(Int, r)
      add!(q, q, UInt(1))
      sub!(r, r, b)
    elseif c < 0
      sub!(q, q, UInt(1))
      add!(r, r, b)
    end
  end
  return (q, r)
end

function ndivrem(x::ZZRingElem, y::ZZRingElem)
  iszero(y) && throw(DivideError())
  z1 = ZZRingElem()
  z2 = ZZRingElem()
  @ccall libflint.fmpz_ndiv_qr(z1::Ref{ZZRingElem}, z2::Ref{ZZRingElem}, x::Ref{ZZRingElem}, y::Ref{ZZRingElem})::Nothing
  z1, z2
end

###############################################################################
#
#   Inversion
#
###############################################################################

function inv(x::ZZRingElem)
  if isone(x)
    return ZZRingElem(1)
  elseif x == -1
    return ZZRingElem(-1)
  end
  iszero(x) && throw(DivideError())
  throw(ArgumentError("not a unit"))
end

###############################################################################
#
#   Powering
#
###############################################################################

# Cannot use IntegerUnion here to avoid ambiguity.

function ^(x::ZZRingElem, y::Int)
  if is_negative(y)
    if abs(x) == 1
      return isodd(y) ? deepcopy(x) : one(x)
    end
    throw(DomainError(y, "Exponent must be non-negative"))
  end
  return pow!(ZZRingElem(), x, y)
end

function ^(x::ZZRingElem, y::ZZRingElem)
  if is_negative(y)
    if abs(x) == 1
      return isodd(y) ? deepcopy(x) : one(x)
    end
    throw(DomainError(y, "Exponent must be non-negative"))
  end
  return pow!(ZZRingElem(), x, y)
end

###############################################################################
#
#  Generic powering by an ZZRingElem
#
###############################################################################

^(a::T, n::ZZRingElem) where {T<:RingElem} = _generic_power(a, n)

function _generic_power(a, n::ZZRingElem)
  fits(Int, n) && return a^Int(n)
  if is_negative(n)
    a = inv(a)
    n = -n
  end
  r = one(parent(a))
  for b = bits(n)
    r = mul!(r, r, r)
    if b
      r = mul!(r, r, a)
    end
  end
  return r
end

###############################################################################
#
#   Comparison
#
###############################################################################

function cmp(x::ZZRingElemOrPtr, y::ZZRingElemOrPtr)
  @ccall libflint.fmpz_cmp(x::Ref{ZZRingElem}, y::Ref{ZZRingElem})::Cint
end

==(x::ZZRingElem, y::ZZRingElem) = cmp(x,y) == 0

<=(x::ZZRingElem, y::ZZRingElem) = cmp(x,y) <= 0

<(x::ZZRingElem, y::ZZRingElem) = cmp(x,y) < 0

function cmpabs(x::ZZRingElemOrPtr, y::ZZRingElemOrPtr)
  Int(@ccall libflint.fmpz_cmpabs(x::Ref{ZZRingElem}, y::Ref{ZZRingElem})::Cint)
end

isless(x::ZZRingElem, y::ZZRingElem) = x < y

isless(x::ZZRingElem, y::Integer) = x < flintify(y)

isless(x::Integer, y::ZZRingElem) = flintify(x) < y

function Base.isapprox(x::ZZRingElem, y::ZZRingElem;
                       atol::Real=0, rtol::Real=0,
                       nans::Bool=false, norm::Function=abs)
  if norm === abs && atol < 1 && rtol == 0
    return x == y
  else
    return norm(x - y) <= max(atol, rtol*max(norm(x), norm(y)))
  end
end

###############################################################################
#
#   Ad hoc comparison
#
###############################################################################

function cmp(x::ZZRingElemOrPtr, y::Int)
  _fmpz_is_small(x) && return cmp(data(x), y)
  Int(@ccall libflint.fmpz_cmp_si(x::Ref{ZZRingElem}, y::Int)::Cint)
end

function cmp(x::ZZRingElemOrPtr, y::UInt)
  _fmpz_is_small(x) && return cmp(data(x), y)
  Int(@ccall libflint.fmpz_cmp_ui(x::Ref{ZZRingElem}, y::UInt)::Cint)
end

cmp(x::ZZRingElemOrPtr, y::Integer) = cmp(x, flintify(y))

for T in (Int, UInt, Integer)
  @eval begin
    ==(x::ZZRingElem, y::$T) = cmp(x,y) == 0
    <=(x::ZZRingElem, y::$T) = cmp(x,y) <= 0
    <(x::ZZRingElem, y::$T) = cmp(x,y) < 0

    ==(x::$T, y::ZZRingElem) = cmp(y,x) == 0
    <=(x::$T, y::ZZRingElem) = cmp(y,x) >= 0
    <(x::$T, y::ZZRingElem) = cmp(y,x) > 0
  end
end

==(x::ZZRingElem, y::Rational) = isinteger(y) && x == numerator(y)

<=(x::ZZRingElem, y::Rational) = x*denominator(y) <= numerator(y)

<(x::ZZRingElem, y::Rational) = x*denominator(y) < numerator(y)

==(x::Rational, y::ZZRingElem) = isinteger(x) && numerator(x) == y

<=(x::Rational, y::ZZRingElem) = numerator(x) <= y*denominator(x)

<(x::Rational, y::ZZRingElem) = numerator(x) < y*denominator(x)

function cmp(a::BigFloat, b::ZZRingElem)
  if _fmpz_is_small(b)
    return Int(@ccall :libmpfr.mpfr_cmp_si(a::Ref{BigFloat}, b.d::Int)::Cint)
  end
  return Int(@ccall :libmpfr.mpfr_cmp_z(a::Ref{BigFloat}, (unsigned(b.d) << 2)::UInt)::Cint)
end

==(x::ZZRingElem, y::BigFloat) = cmp(y, x) == 0

isless(x::ZZRingElem, y::BigFloat) = cmp(y, x) > 0

==(x::BigFloat, y::ZZRingElem) = cmp(x, y) == 0

isless(x::BigFloat, y::ZZRingElem) = cmp(x, y) < 0

isless(a::Float64, b::ZZRingElem) = isless(a, BigFloat(b))
isless(a::ZZRingElem, b::Float64) = isless(BigFloat(a), b)

###############################################################################
#
#   Shifting
#
###############################################################################

@doc raw"""
    <<(x::ZZRingElem, c::Int)

Return $2^cx$ where $c \geq 0$.
"""
function <<(x::ZZRingElem, c::Int)
  c < 0 && throw(DomainError(c, "Exponent must be non-negative"))
  c == 0 && return x
  z = ZZRingElem()
  @ccall libflint.fmpz_mul_2exp(z::Ref{ZZRingElem}, x::Ref{ZZRingElem}, c::Int)::Nothing
  return z
end

@doc raw"""
    >>(x::ZZRingElem, c::Int)

Return $x/2^c$, discarding any remainder, where $c \geq 0$.
"""
function >>(x::ZZRingElem, c::Int)
  c < 0 && throw(DomainError(c, "Exponent must be non-negative"))
  c == 0 && return x
  z = ZZRingElem()
  @ccall libflint.fmpz_fdiv_q_2exp(z::Ref{ZZRingElem}, x::Ref{ZZRingElem}, c::Int)::Nothing
  return z
end

###############################################################################
#
#   Modular arithmetic
#
###############################################################################

function mod!(r::ZZRingElemOrPtr, x::ZZRingElemOrPtr, y::ZZRingElemOrPtr)
  @ccall libflint.fmpz_fdiv_r(r::Ref{ZZRingElem}, x::Ref{ZZRingElem}, y::Ref{ZZRingElem})::Nothing
  return r
end

function mod(x::ZZRingElem, y::ZZRingElem)
  iszero(y) && throw(DivideError())
  r = ZZRingElem()
  return mod!(r, x, y)
end

function mod(x::ZZRingElem, c::UInt)
  c == 0 && throw(DivideError())
  @ccall libflint.fmpz_fdiv_ui(x::Ref{ZZRingElem}, c::UInt)::UInt
end

function mod_sym(a::ZZRingElem, b::ZZRingElem)
  return mod_sym!(deepcopy(a), b)
end

function mod_sym!(a::ZZRingElem, b::ZZRingElem)
  mod!(a, a, b)
  if (b > 0 && a > div(b, 2)) || (b < 0 && a < div(b, 2))
    sub!(a, a, b)
  end
  return a
end


@doc raw"""
    powermod(x::ZZRingElem, p::ZZRingElem, m::ZZRingElem)

Return $x^p (\mod m)$. The remainder will be in the range $[0, m)$
"""
function powermod(x::ZZRingElem, p::ZZRingElem, m::ZZRingElem)
  m <= 0 && throw(DomainError(m, "Exponent must be non-negative"))
  if p < 0
    x = invmod(x, m)
    p = -p
  end
  r = ZZRingElem()
  @ccall libflint.fmpz_powm(r::Ref{ZZRingElem}, x::Ref{ZZRingElem}, p::Ref{ZZRingElem}, m::Ref{ZZRingElem})::Nothing
  return r
end

@doc raw"""
    powermod(x::ZZRingElem, p::Int, m::ZZRingElem)

Return $x^p (\mod m)$. The remainder will be in the range $[0, m)$
"""
function powermod(x::ZZRingElem, p::Int, m::ZZRingElem)
  m <= 0 && throw(DomainError(m, "Exponent must be non-negative"))
  if p < 0
    x = invmod(x, m)
    p = -p
  end
  r = ZZRingElem()
  @ccall libflint.fmpz_powm_ui(r::Ref{ZZRingElem}, x::Ref{ZZRingElem}, p::Int, m::Ref{ZZRingElem})::Nothing
  return r
end

#square-and-multiply algorithm to compute f^e mod g
function powermod(f::T, e::ZZRingElem, g::T) where {T}
  #small exponent -> use powermod
  if nbits(e) <= 63
    return powermod(f, Int(e), g)
  else
    #go through binary representation of exponent and multiply with res
    #or (res and f)
    res = parent(f)(1)
    for b = bits(e)
      res = mod(res^2, g)
      if b
        res = mod(res * f, g)
      end
    end
    return res
  end
end

@doc raw"""
    invmod(x::ZZRingElem, m::ZZRingElem)

Return $x^{-1} (\mod m)$. The remainder will be in the range $[0, m)$
"""
function invmod(x::ZZRingElem, m::ZZRingElem)
  m <= 0 && throw(DomainError(m, "Modulus must be non-negative"))
  z = ZZRingElem()
  if isone(m)
    return ZZRingElem(0)
  end
  if (@ccall libflint.fmpz_invmod(z::Ref{ZZRingElem}, x::Ref{ZZRingElem}, m::Ref{ZZRingElem})::Cint) == 0
    error("Impossible inverse in invmod")
  end
  return z
end

@doc raw"""
    sqrtmod(x::ZZRingElem, m::ZZRingElem)

Return a square root of $x (\mod m)$ if one exists. The remainder will be in
the range $[0, m)$. We require that $m$ is prime, otherwise the algorithm may
not terminate.

# Examples

```jldoctest
julia> sqrtmod(ZZ(12), ZZ(13))
5
```
"""
function sqrtmod(x::ZZRingElem, m::ZZRingElem)
  m <= 0 && throw(DomainError(m, "Modulus must be non-negative"))
  z = ZZRingElem()
  success = @ccall libflint.fmpz_sqrtmod(z::Ref{ZZRingElem}, x::Ref{ZZRingElem}, m::Ref{ZZRingElem})::Cint
  if success == 0
    error("no square root exists or modulus is not prime")
  end
  return z
end

function sqrtmod_pk(a::IntegerUnion, p::IntegerUnion, k::Int)
  @req a != 0 && valuation(a, p) == 0 "Element must be a unit modulo p^k"
  return _sqrtmod_pk_big(ZZ(a), ZZ(p), k)
end

function _sqrtmod_pk_big(a::ZZRingElem, p::ZZRingElem, k::Int)
  # must be unit modulo p
  r = ZZ()
  fl = @ccall libflint._padic_sqrt(r::Ref{ZZRingElem}, a::Ref{ZZRingElem}, p::Ref{ZZRingElem}, k::Int)::Bool
  if !fl
    error("No square root exists")
  end
  return r
end

function _sqrtmod_pk_small(a::Int, p::Int, k::Int)
  # this is too expensive to check
  #@req k < @ccall Nemo.libflint.n_flog((UInt(1) << 63)::UInt, p::Int)::Int
  if a < 0
    a = mod(a, p)
  end
  # must be unit modulo p
  res = Ref(Ptr{UInt}())
  n = @ccall libflint.n_sqrtmod_primepow(res::Ref{Ptr{UInt}}, a::Int, p::Int, k::Int)::Int

  if n == 0
    error("No square root exists")
  end

  ptr = res[]
  b = unsafe_load(ptr, 1)
  @ccall libflint.flint_free(ptr::Ptr{UInt})::Nothing
  return b % Int
end

function _normalize_crt(r::ZZRingElem, m::ZZRingElem, signed)
  s = sign(Int, m)
  if s > 0
    return signed ? nfdivrem(r, m)[2] : fdivrem(r, m)[2]
  elseif s < 0
    return signed ? ncdivrem(r, m)[2] : cdivrem(r, m)[2]
  else
    return r
  end
end

function _normalize_crt_with_lcm(r::ZZRingElem, m::ZZRingElem, signed)
  s = sign(Int, m)
  if s == 0
    return (r, m)
  elseif s < 0
    m = -m
  end
  return (signed ? nfdivrem(r, m)[2] : fdivrem(r, m)[2], m)
end

@doc raw"""
    crt(r1::ZZRingElem, m1::ZZRingElem, r2::ZZRingElem, m2::ZZRingElem, signed=false; check::Bool=true)
    crt(r1::ZZRingElem, m1::ZZRingElem, r2::Union{Int, UInt}, m2::Union{Int, UInt}, signed=false; check::Bool=true)
    crt(r::Vector{ZZRingElem}, m::Vector{ZZRingElem}, signed=false; check::Bool=true)
    crt_with_lcm(r1::ZZRingElem, m1::ZZRingElem, r2::ZZRingElem, m2::ZZRingElem, signed=false; check::Bool=true)
    crt_with_lcm(r1::ZZRingElem, m1::ZZRingElem, r2::Union{Int, UInt}, m2::Union{Int, UInt}, signed=false; check::Bool=true)
    crt_with_lcm(r::Vector{ZZRingElem}, m::Vector{ZZRingElem}, signed=false; check::Bool=true)

As per the AbstractAlgebra `crt` interface, with the following option.
If `signed = true`, the solution is the range $(-m/2, m/2]$, otherwise it is in
the range $[0,m)$, where $m$ is the least common multiple of the moduli.

# Examples

```jldoctest
julia> crt(ZZ(5), ZZ(13), ZZ(7), ZZ(37), true)
44

julia> crt(ZZ(5), ZZ(13), 7, 37, true)
44
```
"""
function crt(r1::ZZRingElem, m1::ZZRingElem, r2::ZZRingElem, m2::ZZRingElem, signed=false; check::Bool=true)
  r, m = AbstractAlgebra._crt_with_lcm_stub(r1, m1, r2, m2; check=check)
  return _normalize_crt(r, m, signed)
end

function crt(r::Vector{ZZRingElem}, m::Vector{ZZRingElem}, signed=false; check::Bool=true)
  r, m = AbstractAlgebra._crt_with_lcm_stub(r, m; check=check)
  return _normalize_crt(r, m, signed)
end

function crt_with_lcm(r1::ZZRingElem, m1::ZZRingElem, r2::ZZRingElem, m2::ZZRingElem, signed=false; check::Bool=true)
  r, m = AbstractAlgebra._crt_with_lcm_stub(r1, m1, r2, m2; check=check)
  return _normalize_crt_with_lcm(r, m, signed)
end

function crt_with_lcm(r::Vector{ZZRingElem}, m::Vector{ZZRingElem}, signed=false; check::Bool=true)
  r, m = AbstractAlgebra._crt_with_lcm_stub(r, m; check=check)
  return _normalize_crt_with_lcm(r, m, signed)
end

# requires a < b
function _gcdinv(a::UInt, b::UInt)
  s = Ref{UInt}()
  g = @ccall libflint.n_gcdinv(s::Ptr{UInt}, a::UInt, b::UInt)::UInt
  return g, s[]
end

function submod(a::UInt, b::UInt, n::UInt)
  return a >= b ? a - b : a - b + n
end

function _crt_with_lcm(r1::ZZRingElem, m1::ZZRingElem, r2::UInt, m2::UInt; check::Bool=true)
  if iszero(m2)
    check && !is_divisible_by(r2 - r1, m1) && error("no crt solution")
    return (ZZRingElem(r2), ZZRingElem(m2))
  end
  if r2 >= m2
    r2 = mod(r2, m2)
  end
  if iszero(m1)
    check && mod(r1, m2) != r2 && error("no crt solution")
    return (r1, m1)
  end
  g, s = _gcdinv(mod(m1, m2), m2)
  diff = submod(r2, mod(r1, m2), m2)
  if isone(g)
    return (r1 + mulmod(diff, s, m2)*m1, m1*m2)
  else
    m2og = divexact(m2, g; check=false)
    diff = divexact(diff, g; check=check)
    return (r1 + mulmod(diff, s, m2og)*m1, m1*m2og)
  end
end

function _crt_with_lcm(r1::ZZRingElem, m1::ZZRingElem, r2::Union{Int, UInt},
    m2::Union{Int, UInt}; check::Bool=true)
  if iszero(m2)
    check && !is_divisible_by(r2 - r1, m1) && error("no crt solution")
    return (ZZRingElem(r2), ZZRingElem(m2))
  end
  m2 = abs(m2)%UInt
  r2 = r2 < 0 ? mod(r2, m2) : UInt(r2)
  return _crt_with_lcm(r1, m1, r2::UInt, m2; check=check)
end

function crt(r1::ZZRingElem, m1::ZZRingElem, r2::Union{Int, UInt},
    m2::Union{Int, UInt}, signed = false; check::Bool=true)
  r, m = _crt_with_lcm(r1, m1, r2, m2; check=check)
  return _normalize_crt(r, m, signed)
end

function crt_with_lcm(r1::ZZRingElem, m1::ZZRingElem, r2::Union{Int, UInt},
    m2::Union{Int, UInt}, signed = false; check::Bool=true)
  r, m = _crt_with_lcm(r1, m1, r2, m2; check=check)
  return _normalize_crt_with_lcm(r, m, signed)
end

###############################################################################
#
#   Integer logarithm
#
###############################################################################

@doc raw"""
    flog(x::ZZRingElem, c::ZZRingElem)
    flog(x::ZZRingElem, c::Int)

Return the floor of the logarithm of $x$ to base $c$.

# Examples

```jldoctest
julia> flog(ZZ(12), ZZ(2))
3

julia> flog(ZZ(12), 3)
2

```
"""
function flog(x::ZZRingElem, c::ZZRingElem)
  c <= 0 && throw(DomainError(c, "Base must be non-negative"))
  x <= 0 && throw(DomainError(x, "Argument must be non-negative"))
  return @ccall libflint.fmpz_flog(x::Ref{ZZRingElem}, c::Ref{ZZRingElem})::Int
end

function flog(x::ZZRingElem, c::Int)
  c <= 0 && throw(DomainError(c, "Base must be non-negative"))
  return @ccall libflint.fmpz_flog_ui(x::Ref{ZZRingElem}, c::Int)::Int
end

@doc raw"""
    clog(x::ZZRingElem, c::ZZRingElem)
    clog(x::ZZRingElem, c::Int)

Return the ceiling of the logarithm of $x$ to base $c$.

# Examples

```jldoctest
julia> clog(ZZ(12), ZZ(2))
4

julia> clog(ZZ(12), 3)
3

```
"""
function clog(x::ZZRingElem, c::ZZRingElem)
  c <= 0 && throw(DomainError(c, "Base must be non-negative"))
  x <= 0 && throw(DomainError(x, "Argument must be non-negative"))
  return @ccall libflint.fmpz_clog(x::Ref{ZZRingElem}, c::Ref{ZZRingElem})::Int
end

function clog(x::ZZRingElem, c::Int)
  c <= 0 && throw(DomainError(c, "Base must be non-negative"))
  return @ccall libflint.fmpz_clog_ui(x::Ref{ZZRingElem}, c::Int)::Int
end

###############################################################################
#
#   Natural logarithm
#
###############################################################################

log(a::ZZRingElem) = log(BigInt(a))
log(a::ZZRingElem, b::ZZRingElem) = log(b) / log(a)

###############################################################################
#
#   GCD and LCM
#
###############################################################################

@doc raw"""
    gcd(x::ZZRingElem, y::ZZRingElem, z::ZZRingElem...)

Return the greatest common divisor of $(x, y, ...)$. The returned result will
always be non-negative and will be zero iff all inputs are zero.
"""
function gcd(x::ZZRingElem, y::ZZRingElem, z::ZZRingElem...)
  d = ZZRingElem()
  d = gcd!(d, x, y)
  length(z) == 0 && return d

  for zi in z
    d = gcd!(d, zi)
  end
  return d
end

@doc raw"""
    gcd(x::Vector{ZZRingElem})

Return the greatest common divisor of the elements of $x$. The returned
result will always be non-negative and will be zero iff all elements of $x$
are zero.
"""
function gcd(x::Vector{ZZRingElem})
  if length(x) == 0
    error("Array must not be empty")
  elseif length(x) == 1
    return x[1]
  end

  z = ZZRingElem()
  z = gcd!(z, x[1], x[2])

  for i in 3:length(x)
    z = gcd!(z, x[i])
    if isone(z)
      return z
    end
  end

  return z
end

@doc raw"""
    lcm(x::ZZRingElem, y::ZZRingElem, z::ZZRingElem...)

Return the least common multiple of $(x, y, ...)$. The returned result will
always be non-negative and will be zero if any input is zero.
"""
function lcm(x::ZZRingElem, y::ZZRingElem, z::ZZRingElem...)
  m = ZZRingElem()
  m = lcm!(m, x, y)
  length(z) == 0 && return m

  for zi in z
    m = lcm!(m, zi)
  end
  return m
end

@doc raw"""
    lcm(x::Vector{ZZRingElem})

Return the least common multiple of the elements of $x$. The returned result
will always be non-negative and will be zero iff the elements of $x$ are zero.
"""
function lcm(x::Vector{ZZRingElem})
  if length(x) == 0
    error("Array must not be empty")
  elseif length(x) == 1
    return x[1]
  end

  z = ZZRingElem()
  z = lcm!(z, x[1], x[2])

  for i in 3:length(x)
    z = lcm!(z, x[i])
  end

  return z
end

gcd(a::ZZRingElem, b::Integer) = gcd(a, ZZRingElem(b))

gcd(a::Integer, b::ZZRingElem) = gcd(ZZRingElem(a), b)

lcm(a::ZZRingElem, b::Integer) = lcm(a, ZZRingElem(b))

lcm(a::Integer, b::ZZRingElem) = lcm(ZZRingElem(a), b)

###############################################################################
#
#   Extended GCD
#
###############################################################################

@doc raw"""
    gcdx(a::ZZRingElem, b::ZZRingElem)

Return a tuple $g, s, t$ such that $g$ is the greatest common divisor of $a$
and $b$ and integers $s$ and $t$ such that $g = as + bt$.
"""
function gcdx(a::ZZRingElem, b::ZZRingElem)
  @static if VERSION < v"1.12.0-DEV.410"
    # Just to conform with Julia's definition
    a == b == 0 && return zero(ZZ), one(ZZ), zero(ZZ)
  end

  d = ZZ()
  x = ZZ()
  y = ZZ()
  @ccall libflint.fmpz_xgcd_canonical_bezout(d::Ref{ZZRingElem}, x::Ref{ZZRingElem}, y::Ref{ZZRingElem}, a::Ref{ZZRingElem}, b::Ref{ZZRingElem})::Nothing
  return d, x, y
end

function gcdinv(a::ZZRingElem, b::ZZRingElem)
  a < 0 && throw(DomainError(a, "First argument must be non-negative"))
  b < a && throw(DomainError((a, b), "First argument must be smaller than second argument"))
  g = ZZRingElem()
  s = ZZRingElem()
  @ccall libflint.fmpz_gcdinv(g::Ref{ZZRingElem}, s::Ref{ZZRingElem}, a::Ref{ZZRingElem}, b::Ref{ZZRingElem})::Nothing
  return g, s
end

gcdx(a::ZZRingElem, b::Integer) = gcdx(a, ZZRingElem(b))

gcdx(a::Integer, b::ZZRingElem) = gcdx(ZZRingElem(a), b)

gcdinv(a::ZZRingElem, b::Integer) = gcdinv(a, ZZRingElem(b))

gcdinv(a::Integer, b::ZZRingElem) = gcdinv(ZZRingElem(a), b)

###############################################################################
#
#   Roots
#
###############################################################################

const sqrt_moduli = [3, 5, 7, 8]
const sqrt_residues = [[0, 1], [0, 1, 4], [0, 1, 2, 4], [0, 1, 4]]

@doc raw"""
    isqrt(x::ZZRingElem)

Return the floor of the square root of $x$.

# Examples

```jldoctest
julia> isqrt(ZZ(13))
3

```
"""
function isqrt(x::ZZRingElem)
  is_negative(x) && throw(DomainError(x, "Argument must be non-negative"))
  z = ZZRingElem()
  @ccall libflint.fmpz_sqrt(z::Ref{ZZRingElem}, x::Ref{ZZRingElem})::Nothing
  return z
end

@doc raw"""
    isqrtrem(x::ZZRingElem)

Return a tuple $s, r$ consisting of the floor $s$ of the square root of $x$
and the remainder $r$, i.e. such that $x = s^2 + r$. We require $x \geq 0$.

# Examples

```jldoctest
julia> isqrtrem(ZZ(13))
(3, 4)

```
"""
function isqrtrem(x::ZZRingElem)
  is_negative(x) && throw(DomainError(x, "Argument must be non-negative"))
  s = ZZRingElem()
  r = ZZRingElem()
  @ccall libflint.fmpz_sqrtrem(s::Ref{ZZRingElem}, r::Ref{ZZRingElem}, x::Ref{ZZRingElem})::Nothing
  return s, r
end

function Base.sqrt(x::ZZRingElem; check=true)
  is_negative(x) && throw(DomainError(x, "Argument must be non-negative"))
  if check
    for i = 1:length(sqrt_moduli)
      res = mod(x, sqrt_moduli[i])
      !(res in sqrt_residues[i]) && error("Not a square")
    end
    s, r = isqrtrem(x)
    !iszero(r) && error("Not a square")
  else
    s = isqrt(x)
  end
  return s
end

is_square(x::ZZRingElem) = Bool(@ccall libflint.fmpz_is_square(x::Ref{ZZRingElem})::Cint)

function is_square_with_sqrt(x::ZZRingElem)
  if is_negative(x)
    return false, zero(ZZRingElem)
  end
  for i = 1:length(sqrt_moduli)
    res = mod(x, sqrt_moduli[i])
    if !(res in sqrt_residues[i])
      return false, zero(ZZRingElem)
    end
  end
  s, r = isqrtrem(x)
  if !iszero(r)
    return false, zero(ZZRingElem)
  end
  return true, s
end

@doc raw"""
    root(x::ZZRingElem, n::Int; check::Bool=true)

Return the $n$-the root of $x$. We require $n > 0$ and that
$x \geq 0$ if $n$ is even. By default the function tests whether the input was
a perfect $n$-th power and if not raises an exception. If `check=false` this
check is omitted.

# Examples

```jldoctest
julia> root(ZZ(27), 3; check=true)
3
```
"""
function root(x::ZZRingElem, n::Int; check::Bool=true)
  x < 0 && iseven(n) && throw(DomainError((x, n), "Argument `x` must be positive if exponent `n` is even"))
  n <= 0 && throw(DomainError(n, "Exponent must be positive"))
  z = ZZRingElem()
  res = @ccall libflint.fmpz_root(z::Ref{ZZRingElem}, x::Ref{ZZRingElem}, n::Int)::Bool
  check && !res && error("Not a perfect n-th power (n = $n)")
  return z
end

@doc raw"""
    iroot(x::ZZRingElem, n::Int)

Return the integer truncation of the $n$-the root of $x$ (round towards zero).
We require $n > 0$ and that $x \geq 0$ if $n$ is even.

# Examples

```jldoctest
julia> iroot(ZZ(13), 3)
2
```
"""
function iroot(x::ZZRingElem, n::Int)
  x < 0 && iseven(n) && throw(DomainError((x, n), "Argument `x` must be positive if exponent `n` is even"))
  n <= 0 && throw(DomainError(n, "Exponent must be positive"))
  z = ZZRingElem()
  @ccall libflint.fmpz_root(z::Ref{ZZRingElem}, x::Ref{ZZRingElem}, n::Int)::Bool
  return z
end

#TODO (Hard): Implement this properly.
@doc raw"""
    is_squarefree(n::Union{Int, ZZRingElem}) -> Bool

Return `true` if $n$ is squarefree, `false` otherwise.
"""
function is_squarefree(n::Union{Int, ZZRingElem})
  is_divisible_by_four(n) && return false
  is_unit(n) && return true
  e, b = is_perfect_power_with_data(n)
  if e > 1
    return false
  end
  return isone(maximum(y for (_,y) in factor(n); init = 1))
end

is_divisible_by_four(n::Integer) = (n % 4) == 0
is_divisible_by_four(n::ZZRingElem) = ((n % UInt) % 4) == 0

pretty_lt(x::ZZRingElem, y::ZZRingElem) = isless(x, y)
pretty_eq(x::ZZRingElem, y::ZZRingElem) = (x == y)

################################################################################
#
#   Factor trial range
#
################################################################################

function _factor_trial_range(N::ZZRingElem, start::Int = 0, np::Int = 10^5)
  F = fmpz_factor()
  @ccall libflint.fmpz_factor_trial_range(F::Ref{fmpz_factor}, N::Ref{ZZRingElem}, start::UInt, np::UInt)::Nothing
  res = Dict{ZZRingElem, Int}()
  for i in 1:F.num
    z = ZZRingElem()
    @ccall libflint.fmpz_factor_get_fmpz(z::Ref{ZZRingElem}, F::Ref{fmpz_factor}, (i - 1)::Int)::Nothing
    res[z] = unsafe_load(F.exp, i)
  end
  return res, canonical_unit(N)
end

###############################################################################
#
#   Number theoretic/combinatorial
#
###############################################################################

@doc raw"""
    divisors(a::Union{Int, ZZRingElem})

Return the positive divisors of $a$ in an array, not necessarily in ascending
order. We require $a \neq 0$.
"""
function divisors end

function divisors(a::ZZRingElem)
  iszero(a) && throw(DomainError("Argument must be non-zero"))

  divs = ZZRingElem[one(ZZ)]
  isone(a) && return divs

  for (p,e) in factor(a)
    ndivs = copy(divs)
    for i = 1:e
      map!(d -> p*d, ndivs, ndivs)
      append!(divs, ndivs)
    end
  end

  return divs
end

divisors(a::Int) = Int.(divisors(ZZ(a)))

@doc raw"""
    prime_divisors(a::ZZRingElem)

Return the prime divisors of $a$ in an array. We require $a \neq 0$.
"""
function prime_divisors(a::ZZRingElem)
  iszero(a) && throw(DomainError("Argument must be non-zero"))
  ZZRingElem[p for (p, e) in factor(a)]
end

@doc raw"""
    prime_divisors(a::Int)

Return the prime divisors of $a$ in an array. We require $a \neq 0$.
"""
prime_divisors(a::Int) = Int.(prime_divisors(ZZ(a)))

is_prime(x::UInt) = Bool(@ccall libflint.n_is_prime(x::UInt)::Cint)

@doc raw"""
    is_prime(x::ZZRingElem)
    is_prime(x::Int)

Return `true` if $x$ is a prime number, otherwise return `false`.

# Examples

```jldoctest
julia> is_prime(ZZ(13))
true
```
"""
function is_prime(x::ZZRingElem)
  !is_probable_prime(x) && return false
  return Bool(@ccall libflint.fmpz_is_prime(x::Ref{ZZRingElem})::Cint)
end

function is_prime(n::Int)
  if n < 0
    return false
  end
  return is_prime(n % UInt)
end

@doc raw"""
    is_probable_prime(x::ZZRingElem)

Return `true` if $x$ is very probably a prime number, otherwise return
`false`. No counterexamples are known to this test, but it is conjectured
that infinitely many exist.
"""
is_probable_prime(x::ZZRingElem) = Bool(@ccall libflint.fmpz_is_probabprime(x::Ref{ZZRingElem})::Cint)

@doc raw"""
    next_prime(x::ZZRingElem, proved = true)

Return the smallest prime strictly greater than $x$.
If a second argument of `false` is specified, the return is only probably prime.
"""
function next_prime(x::ZZRingElem, proved::Bool = true)
  z = ZZRingElem()
  @ccall libflint.fmpz_nextprime(z::Ref{ZZRingElem}, x::Ref{ZZRingElem}, proved::Cint)::Nothing
  return z
end

function next_prime(x::UInt, proved::Bool = true)
  if (Base.GMP.BITS_PER_LIMB == 64 && x >= 0xffffffffffffffc5) ||
    (Base.GMP.BITS_PER_LIMB == 32 && x >= 0xfffffffb)
    error("No larger single-limb prime exists")
  end
  return @ccall libflint.n_nextprime(x::UInt, proved::Cint)::UInt
end

function next_prime(x::Int, proved::Bool = true)
  return x < 2 ? 2 : Int(next_prime(x % UInt, proved))
end

function remove!(z::ZZRingElemOrPtr, a::ZZRingElemOrPtr, b::ZZRingElemOrPtr)
  v = @ccall libflint.fmpz_remove(z::Ref{ZZRingElem}, a::Ref{ZZRingElem}, b::Ref{ZZRingElem})::Clong
  return Int(v), z
end

remove!(a::ZZRingElemOrPtr, b::ZZRingElemOrPtr) = remove!(a, a, b)

function remove(x::ZZRingElem, y::ZZRingElem)
  iszero(y) && throw(DivideError())
  y <= 1 && error("Factor <= 1")
  z = ZZRingElem()
  return remove!(z, x, y)
end

remove(x::ZZRingElem, y::Integer) = remove(x, ZZRingElem(y))

remove(x::Integer, y::ZZRingElem) = remove(ZZRingElem(x), y)

function remove(a::UInt, b::UInt)
  b <= 1 && error("Factor <= 1")
  a == 0 && error("Not yet implemented")
  q = Ref(a)
  binv = @ccall libflint.n_precompute_inverse(b::UInt)::Float64
  v = @ccall libflint.n_remove2_precomp(q::Ptr{UInt}, b::UInt, binv::Float64)::Cint
  return (Int(v), q[])
end

function remove(a::Int, b::Int)
  b <= 1 && error("Factor <= 1")
  v, q = remove(abs(a)%UInt, b%UInt)
  return (v, a < 0 ? -q%Int : q%Int)
end

function remove(a::BigInt, b::BigInt)
  b <= 1 && error("Factor <= 1")
  a == 0 && error("Not yet implemented")
  q = BigInt()
  v = @ccall :libgmp.__gmpz_remove(q::Ref{BigInt}, a::Ref{BigInt}, b::Ref{BigInt})::Culong
  return (Int(v), q)
end

function remove(x::Integer, y::Integer)
  v, q = remove(ZZRingElem(x), ZZRingElem(y))
  return (v, convert(promote_type(typeof(x), typeof(y)), q))
end

@doc raw"""
    valuation(x::ZZRingElem, y::ZZRingElem)

Return the largest $n$ such that $y^n$ divides $x$.
"""
function valuation(x::ZZRingElem, y::ZZRingElem)
  iszero(x) && error("Not yet implemented")
  n, _ = remove(x, y)
  return n
end

valuation(x::ZZRingElem, y::Integer) = valuation(x, ZZRingElem(y))

valuation(x::Integer, y::ZZRingElem) = valuation(ZZRingElem(x), y)

valuation(x::Integer, y::Integer) = valuation(ZZRingElem(x), ZZRingElem(y))

@doc raw"""
    divisor_lenstra(n::ZZRingElem, r::ZZRingElem, m::ZZRingElem)

If $n$ has a factor which lies in the residue class $r (\mod m)$ for
$0 < r < m < n$, this function returns such a factor. Otherwise it returns
$0$. This is only efficient if $m$ is at least the cube root of $n$. We
require gcd$(r, m) = 1$ and this condition is not checked.
"""
function divisor_lenstra(n::ZZRingElem, r::ZZRingElem, m::ZZRingElem)
  r <= 0 && throw(DomainError(r, "Residue class must be non-negative"))
  m <= r && throw(DomainError((m, r), "Modulus must be bigger than residue class"))
  n <= m && throw(DomainError((n, m), "Argument must be bigger than modulus"))
  z = ZZRingElem()
  if !Bool(@ccall libflint.fmpz_divisor_in_residue_class_lenstra(z::Ref{ZZRingElem}, n::Ref{ZZRingElem}, r::Ref{ZZRingElem}, m::Ref{ZZRingElem})::Cint)
    z = 0
  end
  return z
end

@doc raw"""
    factorial(x::ZZRingElem)

Return the factorial of $x$, i.e. $x! = 1.2.3\ldots x$. We require
$x \geq 0$.

# Examples

```jldoctest
julia> factorial(ZZ(100))
93326215443944152681699238856266700490715968264381621468592963895217599993229915608941463976156518286253697920827223758251185210916864000000000000000000000000
```
"""
function factorial(x::ZZRingElem)
  x < 0 && throw(DomainError(x, "Argument must be non-negative"))
  z = ZZRingElem()
  @ccall libflint.fmpz_fac_ui(z::Ref{ZZRingElem}, UInt(x)::UInt)::Nothing
  return z
end

@doc raw"""
    rising_factorial(x::ZZRingElem, n::Int)

Return the rising factorial of $x$, i.e. $x(x + 1)(x + 2)\ldots (x + n - 1)$.
If $n < 0$ we throw a `DomainError()`.
"""
function rising_factorial(x::ZZRingElem, n::Int)
  n < 0 && throw(DomainError(n, "Argument must be non-negative"))
  z = ZZRingElem()
  @ccall libflint.fmpz_rfac_ui(z::Ref{ZZRingElem}, x::Ref{ZZRingElem}, UInt(n)::UInt)::Nothing
  return z
end

@doc raw"""
    rising_factorial(x::ZZRingElem, n::ZZRingElem)

Return the rising factorial of $x$, i.e. $x(x + 1)(x + 2)\cdots (x + n - 1)$.
If $n < 0$ we throw a `DomainError()`.
"""
rising_factorial(x::ZZRingElem, n::ZZRingElem) = rising_factorial(x, Int(n))

@doc raw"""
    primorial(x::Int)

Return the primorial of $x$, i.e. the product of all primes less than or
equal to $x$. If $x < 0$ we throw a `DomainError()`.
"""
function primorial(x::Int)
  x < 0 && throw(DomainError(x, "Argument must be non-negative"))
  # Up to 28 is OK for Int32, up to 52 is OK for Int64, up to 100 is OK for Int128; beyond is too large
  if (Int == Int32 && x > 28) || (Int == Int64 && x > 52) || (Int == Int128 && x > 100)
    throw(OverflowError("primorial(::Int)"))
  end
  z = ZZRingElem()
  @ccall libflint.fmpz_primorial(z::Ref{ZZRingElem}, UInt(x)::UInt)::Nothing
  return Int(z)
end

@doc raw"""
    primorial(x::ZZRingElem)

Return the primorial of $x$, i.e. the product of all primes less than or
equal to $x$. If $x < 0$ we throw a `DomainError()`.
"""
function primorial(x::ZZRingElem)
  x < 0 && throw(DomainError(x, "Argument must be non-negative"))
  z = ZZRingElem()
  @ccall libflint.fmpz_primorial(z::Ref{ZZRingElem}, UInt(x)::UInt)::Nothing
  return z
end

@doc raw"""
    fibonacci(x::Int)

Return the $x$-th Fibonacci number $F_x$. We define $F_1 = 1$, $F_2 = 1$ and
$F_{i + 1} = F_i + F_{i - 1}$ for all integers $i$.
"""
function fibonacci(x::Int)
  # Up to 46 is OK for Int32; up to 92 is OK for Int64; up to 184 is OK for Int128; beyond is too large
  if (Int == Int32 && abs(x) > 46) || (Int == Int64 && abs(x) > 92) || (Int == Int128 && abs(x) > 184)
    throw(OverflowError("fibonacci(::Int)"))
  end
  z = ZZRingElem()
  @ccall libflint.fmpz_fib_ui(z::Ref{ZZRingElem}, UInt(abs(x))::UInt)::Nothing
  return x < 0 ? (iseven(x) ? -Int(z) : Int(z)) : Int(z)
end

@doc raw"""
    fibonacci(x::ZZRingElem)

Return the $x$-th Fibonacci number $F_x$. We define $F_1 = 1$, $F_2 = 1$ and
$F_{i + 1} = F_i + F_{i - 1}$ for all integers $i$.
"""
function fibonacci(x::ZZRingElem)
  z = ZZRingElem()
  @ccall libflint.fmpz_fib_ui(z::Ref{ZZRingElem}, UInt(abs(x))::UInt)::Nothing
  return x < 0 ? (iseven(x) ? -z : z) : z
end

@doc raw"""
    bell(x::Int)

Return the Bell number $B_x$.
"""
function bell(x::Int)
  x < 0 && throw(DomainError(x, "Argument must be non-negative"))
  z = ZZRingElem()
  @ccall libflint.arith_bell_number(z::Ref{ZZRingElem}, UInt(x)::UInt)::Nothing
  return Int(z)
end

@doc raw"""
    bell(x::ZZRingElem)

Return the Bell number $B_x$.
"""
function bell(x::ZZRingElem)
  x < 0 && throw(DomainError(x, "Argument must be non-negative"))
  z = ZZRingElem()
  @ccall libflint.arith_bell_number(z::Ref{ZZRingElem}, UInt(x)::UInt)::Nothing
  return z
end

# fmpz_bin_uiui doesn't always work on UInt input as it just wraps mpz_bin_uiui,
# which has silly gnu problems on windows
# TODO: fib_ui, pow_ui, fac_ui ditto
function _binomial!(z::ZZRingElem, n::Culong, k::Culong)
  @ccall libflint.fmpz_bin_uiui(z::Ref{ZZRingElem}, UInt(n)::UInt, UInt(k)::UInt)::Nothing
  return z
end

function _binomial(n::ZZRingElem, k::ZZRingElem)
  @assert k >= 0
  z = ZZRingElem(1)
  if fits(Culong, n) && fits(Culong, k)
    _binomial!(z, Culong(n), Culong(k))
  elseif fits(UInt, k)
    for K in UInt(1):UInt(k)
      mul!(z, z, n - (K - 1))
      divexact!(z, z, K)
    end
  else
    # if called with n >= k
    error("result of binomial($n, $k) probably is too large")
  end
  return z
end


@doc raw"""
    binomial(n::ZZRingElem, k::ZZRingElem)

Return the binomial coefficient $\frac{n (n-1) \cdots (n-k+1)}{k!}$.
If $k < 0$ we return $0$, and the identity
`binomial(n, k) == binomial(n - 1, k - 1) + binomial(n - 1, k)` always holds
for integers `n` and `k`.
"""
function binomial(n::ZZRingElem, k::ZZRingElem)
  ksgn = cmp(k, 0)
  ksgn > 0 || return ZZRingElem(ksgn == 0)
  # k > 0 now
  negz = false
  if n < 0
    n = k - n - 1
    negz = isodd(k)
  end
  if n < k
    z = ZZRingElem(0)
  elseif 2*k <= n
    z = _binomial(n, k)
  else
    z = _binomial(n, n - k)
  end
  return negz ? neg!(z) : z
end

@doc raw"""
    binomial(n::UInt, k::UInt, ::ZZRing)

Return the binomial coefficient $\frac{n!}{(n - k)!k!}$ as an `ZZRingElem`.
"""
function binomial(n::UInt, k::UInt, ::ZZRing)
  z = ZZRingElem()
  @ccall libflint.fmpz_bin_uiui(z::Ref{ZZRingElem}, n::UInt, k::UInt)::Nothing
  return z
end

@doc raw"""
    moebius_mu(x::ZZRingElem)

Return the Moebius mu function of $x$ as an `Int`. The value
returned is either $-1$, $0$ or $1$. If $x \leq 0$ we throw a `DomainError()`.
"""
function moebius_mu(x::ZZRingElem)
  x <= 0 && throw(DomainError(x, "Argument must be positive"))
  return Int(@ccall libflint.fmpz_moebius_mu(x::Ref{ZZRingElem})::Cint)
end

@doc raw"""
    moebius_mu(x::Int)

Return the Moebius mu function of $x$ as an `Int`. The value
returned is either $-1$, $0$ or $1$. If $x \leq 0$ we throw a `DomainError()`.
"""
moebius_mu(x::Int) = moebius_mu(ZZRingElem(x))

@doc raw"""
    jacobi_symbol(x::ZZRingElem, y::ZZRingElem)

Return the value of the Jacobi symbol $\left(\frac{x}{y}\right)$. The modulus
$y$ must be odd and positive, otherwise a `DomainError` is thrown.
"""
function jacobi_symbol(x::ZZRingElem, y::ZZRingElem)
  (y <= 0 || iseven(y)) && throw(DomainError(y, "Modulus must be odd and positive"))
  if x < 0 || x >= y
    x = mod(x, y)
  end
  return Int(@ccall libflint.fmpz_jacobi(x::Ref{ZZRingElem}, y::Ref{ZZRingElem})::Cint)
end

jacobi_symbol(x::IntegerUnion, y::IntegerUnion) = jacobi_symbol(ZZ(x), ZZ(y))

@doc raw"""
    jacobi_symbol(x::Int, y::Int)

Return the value of the Jacobi symbol $\left(\frac{x}{y}\right)$. The modulus
$y$ must be odd and positive, otherwise a `DomainError` is thrown.
"""
function jacobi_symbol(x::Int, y::Int)
  (y <= 0 || mod(y, 2) == 0) && throw(DomainError(y, "Modulus must be odd and positive"))
  if x < 0 || x >= y
    x = mod(x, y)
  end
  return Int(@ccall libflint.n_jacobi(x::Int, UInt(y)::UInt)::Cint)
end

@doc raw"""
    kronecker_symbol(x::ZZRingElem, y::ZZRingElem)
    kronecker_symbol(x::Int, y::Int)

Return the value of the Kronecker symbol $\left(\frac{x}{y}\right)$.
The definition is as per Henri Cohen's book, "A Course in Computational
Algebraic Number Theory", Definition 1.4.8.
"""
function kronecker_symbol(x::Int, y::Int)
  return Int(@ccall libflint.z_kronecker(x::Int, y::Int)::Cint)
end

function kronecker_symbol(x::ZZRingElem, y::ZZRingElem)
  return Int(@ccall libflint.fmpz_kronecker(x::Ref{ZZRingElem}, y::Ref{ZZRingElem})::Cint)
end

@doc raw"""
    divisor_sigma(x::ZZRingElem, y::Int)
    divisor_sigma(x::ZZRingElem, y::ZZRingElem)
    divisor_sigma(x::Int, y::Int)

Return the value of the sigma function, i.e. $\sum_{0 < d \;| x} d^y$. If
$x \leq 0$ or $y < 0$ we throw a `DomainError()`.

# Examples

```jldoctest
julia> divisor_sigma(ZZ(32), 10)
1127000493261825

julia> divisor_sigma(ZZ(32), ZZ(10))
1127000493261825

julia> divisor_sigma(32, 10)
1127000493261825
```
"""
function divisor_sigma(x::ZZRingElem, y::Int)
  x <= 0 && throw(DomainError(x, "Argument must be positive"))
  y < 0 && throw(DomainError(y, "Power must be non-negative"))
  z = ZZRingElem()
  @ccall libflint.fmpz_divisor_sigma(z::Ref{ZZRingElem}, UInt(y)::UInt, x::Ref{ZZRingElem})::Nothing
  return z
end

divisor_sigma(x::ZZRingElem, y::ZZRingElem) = divisor_sigma(x, Int(y))
divisor_sigma(x::Int, y::Int) = Int(divisor_sigma(ZZRingElem(x), y))

@doc raw"""
    euler_phi(x::ZZRingElem)
    euler_phi(x::Int)

Return the value of the Euler phi function at $x$, i.e. the number of
positive integers up to $x$ (inclusive) that are coprime with $x$. An
exception is raised if $x \leq 0$.

# Examples

```jldoctest
julia> euler_phi(ZZ(12480))
3072

julia> euler_phi(12480)
3072
```
"""
function euler_phi(x::ZZRingElem)
  x <= 0 && throw(DomainError(x, "Argument must be positive"))
  z = ZZRingElem()
  @ccall libflint.fmpz_euler_phi(z::Ref{ZZRingElem}, x::Ref{ZZRingElem})::Nothing
  return z
end

euler_phi(x::Int) = Int(euler_phi(ZZRingElem(x)))

function euler_phi(x::Fac{ZZRingElem})
  return prod((p - 1) * p^(v - 1) for (p, v) in x)
end

@doc raw"""
    number_of_partitions(x::Int)
    number_of_partitions(x::ZZRingElem)

Return the number of partitions of $x$.

# Examples

```jldoctest
julia> number_of_partitions(100)
190569292

julia> number_of_partitions(ZZ(1000))
24061467864032622473692149727991
```
"""
function number_of_partitions(x::Int)
  if x < 0
    return 0
  end
  z = ZZRingElem()
  @ccall libflint.partitions_fmpz_ui(z::Ref{ZZRingElem}, x::UInt)::Nothing
  return Int(z)
end

function number_of_partitions(x::ZZRingElem)
  z = ZZRingElem()
  if x < 0
    return z
  end
  @ccall libflint.partitions_fmpz_fmpz(z::Ref{ZZRingElem}, x::Ref{ZZRingElem}, 0::Int)::Nothing
  return z
end

###############################################################################
#
#   Number bases/digits
#
###############################################################################

@doc raw"""
    bin(n::ZZRingElem)

Return $n$ as a binary string.

# Examples

```jldoctest
julia> bin(ZZ(12))
"1100"
```
"""
bin(n::ZZRingElem) = base(n, 2)

@doc raw"""
    oct(n::ZZRingElem)

Return $n$ as a octal string.

# Examples

```jldoctest
julia> oct(ZZ(12))
"14"
```
"""
oct(n::ZZRingElem) = base(n, 8)

@doc raw"""
    dec(n::ZZRingElem)

Return $n$ as a decimal string.

# Examples

```jldoctest
julia> dec(ZZ(12))
"12"
```
"""
dec(n::ZZRingElem) = base(n, 10)

@doc raw"""
    hex(n::ZZRingElem) = base(n, 16)

Return $n$ as a hexadecimal string.

# Examples

```jldoctest
julia> hex(ZZ(12))
"c"
```
"""
hex(n::ZZRingElem) = base(n, 16)

@doc raw"""
    base(n::ZZRingElem, b::Integer)

Return $n$ as a string in base $b$. We require $2 \leq b \leq 62$.

# Examples

```jldoctest
julia> base(ZZ(12), 13)
"c"
```
"""
function base(n::ZZRingElem, b::Integer)
  2 <= b <= 62 || error("invalid base: $b")
  p = @ccall libflint.fmpz_get_str(C_NULL::Ptr{UInt8}, b::Cint, n::Ref{ZZRingElem})::Ptr{UInt8}
  s = unsafe_string(p)
  @ccall libflint.flint_free(p::Ptr{UInt8})::Nothing
  return s
end

@doc raw"""
    number_of_digits(x::ZZRingElem, b::Integer)

Return the number of digits of $x$ in the base $b$ (default is $b = 10$).

# Examples

```jldoctest
julia> number_of_digits(ZZ(12), 3)
3
```
"""
function number_of_digits(x::ZZRingElem, b::IntegerUnion)
  number_of_digits(x, base=b)::Int
end

function number_of_digits(a::ZZRingElem; base::IntegerUnion = 10, pad::Integer = 1)
  iszero(a) && return max(pad, 1)
  return max(pad, 1+flog(abs(a), ZZRingElem(abs(base))))
end

Base.digits(n::ZZRingElem; base::IntegerUnion = 10, pad::Integer = 1) =
digits(typeof(base), n, base = base, pad = pad)

function Base.digits(T::Type{<:IntegerUnion}, n::ZZRingElem; base::IntegerUnion = 10, pad::Integer = 1)
  digits!(zeros(T, ndigits(n, base=base, pad=pad)), n, base=base)
end

function Base.digits!(a::AbstractVector{T}, n::ZZRingElem; base::T = 10) where T<:IntegerUnion
  2 <= base || throw(DomainError(base, "base must be ≥ 2"))
  Base.hastypemax(T) && abs(base) - 1 > typemax(T) &&
  throw(ArgumentError("type $T too small for base $base"))
  isempty(a) && return a

  if nbits(n)/ndigits(base, base = 2) > 100
    c = div(div(nbits(n), ndigits(base, base = 2)), 2)
    nn = ZZRingElem(base)^c
    q, r = divrem(n, nn)

    digits!(view(a, 1:c), r, base = base)
    digits!(view(a, c+1:length(a)), q, base = base)
    return a
  end

  for i in eachindex(a)
    n, r = Base.divrem(n, base)
    a[i] = r
  end
  return a
end

@doc raw"""
    nbits(x::ZZRingElem)

Return the number of binary bits of $x$. We return zero if $x = 0$.

# Examples

```jldoctest
julia> nbits(ZZ(12))
4
```
"""
nbits(x::ZZRingElemOrPtr) = Int(@ccall libflint.fmpz_bits(x::Ref{ZZRingElem})::Culong)

@doc raw"""
    nbits(a::Integer) -> Int

Return the number of bits necessary to represent $a$.
"""
function nbits(a::Integer)
  return ndigits(a, base=2)
end

###############################################################################
#
#   Bit fiddling
#
###############################################################################

@doc raw"""
    popcount(x::ZZRingElem)

Return the number of ones in the binary representation of $x$.

# Examples

```jldoctest
julia> popcount(ZZ(12))
2
```
"""
popcount(x::ZZRingElem) = Int(@ccall libflint.fmpz_popcnt(x::Ref{ZZRingElem})::UInt)

@doc raw"""
    prevpow2(x::ZZRingElem)

Return the previous power of $2$ up to including $x$.
"""
prevpow2(x::ZZRingElem) = x < 0 ? -prevpow2(-x) :
(x <= 2 ? x : one(ZZ) << (ndigits(x, 2) - 1))

@doc raw"""
    nextpow2(x::ZZRingElem)

Return the next power of $2$ that is at least $x$.

# Examples

```jldoctest
julia> nextpow2(ZZ(12))
16
```
"""
nextpow2(x::ZZRingElem) = x < 0 ? -nextpow2(-x) :
(x <= 2 ? x : one(ZZ) << ndigits(x - 1, 2))

@doc raw"""
    trailing_zeros(x::ZZRingElem)

Return the number of trailing zeros in the binary representation of $x$.
"""
trailing_zeros(x::ZZRingElem) = @ccall libflint.fmpz_val2(x::Ref{ZZRingElem})::Int

###############################################################################
#
#   Bitwise operations (unsafe)
#
###############################################################################

@doc raw"""
    clrbit!(x::ZZRingElem, c::Int)

Clear bit $c$ of $x$, where the least significant bit is the $0$-th bit. Note
that this function modifies its input in-place.

# Examples

```jldoctest
julia> a = ZZ(12)
12

julia> clrbit!(a, 3)

julia> a
4
```
"""
function clrbit!(x::ZZRingElem, c::Int)
  c < 0 && throw(DomainError(c, "Second argument must be non-negative"))
  @ccall libflint.fmpz_clrbit(x::Ref{ZZRingElem}, c::UInt)::Nothing
end

@doc raw"""
    setbit!(x::ZZRingElem, c::Int)

Set bit $c$ of $x$, where the least significant bit is the $0$-th bit. Note
that this function modifies its input in-place.

# Examples

```jldoctest
julia> a = ZZ(12)
12

julia> setbit!(a, 0)

julia> a
13
```
"""
function setbit!(x::ZZRingElem, c::Int)
  c < 0 && throw(DomainError(c, "Second argument must be non-negative"))
  @ccall libflint.fmpz_setbit(x::Ref{ZZRingElem}, c::UInt)::Nothing
end

@doc raw"""
    combit!(x::ZZRingElem, c::Int)

Complement bit $c$ of $x$, where the least significant bit is the $0$-th bit.
Note that this function modifies its input in-place.

# Examples

```jldoctest
julia> a = ZZ(12)
12

julia> combit!(a, 2)

julia> a
8
```
"""
function combit!(x::ZZRingElem, c::Int)
  c < 0 && throw(DomainError(c, "Second argument must be non-negative"))
  @ccall libflint.fmpz_combit(x::Ref{ZZRingElem}, c::UInt)::Nothing
end

@doc raw"""
    tstbit(x::ZZRingElem, c::Int)

Return bit $i$ of x (numbered from 0) as `true` for 1 or `false` for 0.

# Examples

```jldoctest
julia> a = ZZ(12)
12

julia> tstbit(a, 0)
false

julia> tstbit(a, 2)
true
```
"""
function tstbit(x::ZZRingElem, c::Int)
  return c >= 0 && Bool(@ccall libflint.fmpz_tstbit(x::Ref{ZZRingElem}, c::UInt)::Cint)
end

###############################################################################
#
#   Unsafe operators
#
###############################################################################

function zero!(z::ZZRingElemOrPtr)
  @ccall libflint.fmpz_zero(z::Ref{ZZRingElem})::Nothing
  return z
end

function one!(z::ZZRingElemOrPtr)
  set!(z, UInt(1))
end

function neg!(z::ZZRingElemOrPtr, a::ZZRingElemOrPtr)
  @ccall libflint.fmpz_neg(z::Ref{ZZRingElem}, a::Ref{ZZRingElem})::Nothing
  return z
end

function set!(z::ZZRingElemOrPtr, a::ZZRingElemOrPtr)
  @ccall libflint.fmpz_set(z::Ref{ZZRingElem}, a::Ref{ZZRingElem})::Nothing
  return z
end

function set!(z::ZZRingElemOrPtr, a::Int)
  @ccall libflint.fmpz_set_si(z::Ref{ZZRingElem}, a::Int)::Nothing
  return z
end

function set!(z::ZZRingElemOrPtr, a::UInt)
  @ccall libflint.fmpz_set_ui(z::Ref{ZZRingElem}, a::UInt)::Nothing
  return z
end

set!(z::ZZRingElemOrPtr, a::Integer) = set!(z, flintify(a))

function swap!(a::ZZRingElemOrPtr, b::ZZRingElemOrPtr)
  @ccall libflint.fmpz_swap(a::Ref{ZZRingElem}, b::Ref{ZZRingElem})::Nothing
end

#

function add!(z::ZZRingElemOrPtr, x::ZZRingElemOrPtr, y::ZZRingElemOrPtr)
  @ccall libflint.fmpz_add(z::Ref{ZZRingElem}, x::Ref{ZZRingElem}, y::Ref{ZZRingElem})::Nothing
  return z
end

function add!(z::ZZRingElemOrPtr, x::ZZRingElemOrPtr, y::Int)
  @ccall libflint.fmpz_add_si(z::Ref{ZZRingElem}, x::Ref{ZZRingElem}, y::Int)::Nothing
  return z
end

function add!(z::ZZRingElemOrPtr, x::ZZRingElemOrPtr, y::UInt)
  @ccall libflint.fmpz_add_ui(z::Ref{ZZRingElem}, x::Ref{ZZRingElem}, y::UInt)::Nothing
  return z
end

function add!(a::ZZRingElemOrPtr, b::ZZRingElemOrPtr, c::Ptr{Int})
  @ccall libflint.fmpz_add(a::Ref{ZZRingElem}, b::Ref{ZZRingElem}, c::Ptr{Int})::Nothing
  return a
end

add!(z::ZZRingElemOrPtr, a::ZZRingElemOrPtr, b::Integer) = add!(z, a, flintify(b))
add!(z::ZZRingElemOrPtr, x::Integer, y::ZZRingElemOrPtr) = add!(z, y, x)

#

function sub!(z::ZZRingElemOrPtr, a::ZZRingElemOrPtr, b::ZZRingElemOrPtr)
  @ccall libflint.fmpz_sub(z::Ref{ZZRingElem}, a::Ref{ZZRingElem}, b::Ref{ZZRingElem})::Nothing
  return z
end

function sub!(z::ZZRingElemOrPtr, a::ZZRingElemOrPtr, b::Int)
  @ccall libflint.fmpz_sub_si(z::Ref{ZZRingElem}, a::Ref{ZZRingElem}, b::Int)::Nothing
  return z
end

function sub!(z::ZZRingElemOrPtr, a::ZZRingElemOrPtr, b::UInt)
  @ccall libflint.fmpz_sub_ui(z::Ref{ZZRingElem}, a::Ref{ZZRingElem}, b::UInt)::Nothing
  return z
end

sub!(z::ZZRingElemOrPtr, a::ZZRingElemOrPtr, b::Integer) = sub!(z, a, flintify(b))
sub!(z::ZZRingElemOrPtr, a::Integer, b::ZZRingElemOrPtr) = neg!(sub!(z, b, a))

#

function mul!(z::ZZRingElemOrPtr, x::ZZRingElemOrPtr, y::ZZRingElemOrPtr)
  @ccall libflint.fmpz_mul(z::Ref{ZZRingElem}, x::Ref{ZZRingElem}, y::Ref{ZZRingElem})::Nothing
  return z
end

function mul!(z::ZZRingElemOrPtr, x::ZZRingElemOrPtr, y::Int)
  @ccall libflint.fmpz_mul_si(z::Ref{ZZRingElem}, x::Ref{ZZRingElem}, y::Int)::Nothing
  return z
end

function mul!(z::ZZRingElemOrPtr, x::ZZRingElemOrPtr, y::UInt)
  @ccall libflint.fmpz_mul_ui(z::Ref{ZZRingElem}, x::Ref{ZZRingElem}, y::UInt)::Nothing
  return z
end

mul!(z::ZZRingElemOrPtr, a::ZZRingElemOrPtr, b::Integer) = mul!(z, a, flintify(b))
mul!(z::ZZRingElemOrPtr, x::Integer, y::ZZRingElemOrPtr) = mul!(z, y, x)

function mul!(a::ZZRingElemOrPtr, b::ZZRingElemOrPtr, c::Ptr{Int})
  @ccall libflint.fmpz_mul(a::Ref{ZZRingElem}, b::Ref{ZZRingElem}, c::Ptr{Int})::Nothing
  return a
end

#

function addmul!(z::ZZRingElemOrPtr, x::ZZRingElemOrPtr, y::ZZRingElemOrPtr)
  @ccall libflint.fmpz_addmul(z::Ref{ZZRingElem}, x::Ref{ZZRingElem}, y::Ref{ZZRingElem})::Nothing
  return z
end

function addmul!(z::ZZRingElemOrPtr, x::ZZRingElemOrPtr, y::Int)
  @ccall libflint.fmpz_addmul_si(z::Ref{ZZRingElem}, x::Ref{ZZRingElem}, y::Int)::Nothing
  return z
end

function addmul!(z::ZZRingElemOrPtr, x::ZZRingElemOrPtr, y::UInt)
  @ccall libflint.fmpz_addmul_ui(z::Ref{ZZRingElem}, x::Ref{ZZRingElem}, y::UInt)::Nothing
  return z
end

addmul!(z::ZZRingElemOrPtr, x::ZZRingElemOrPtr, y::Integer) = addmul!(z, x, flintify(y))
addmul!(z::ZZRingElemOrPtr, x::Integer, y::ZZRingElemOrPtr) = addmul!(z, y, x)

# ignore fourth argument
addmul!(z::ZZRingElemOrPtr, x::ZZRingElemOrPtr, y::Union{ZZRingElemOrPtr,Integer}, ::ZZRingElemOrPtr) = addmul!(z, x, y)
addmul!(z::ZZRingElemOrPtr, x::Integer, y::ZZRingElemOrPtr, ::ZZRingElemOrPtr) = addmul!(z, x, y)

function submul!(z::ZZRingElemOrPtr, x::ZZRingElemOrPtr, y::ZZRingElemOrPtr)
  @ccall libflint.fmpz_submul(z::Ref{ZZRingElem}, x::Ref{ZZRingElem}, y::Ref{ZZRingElem})::Nothing
  return z
end

function submul!(z::ZZRingElemOrPtr, x::ZZRingElemOrPtr, y::Int)
  @ccall libflint.fmpz_submul_si(z::Ref{ZZRingElem}, x::Ref{ZZRingElem}, y::Int)::Nothing
  return z
end

function submul!(z::ZZRingElemOrPtr, x::ZZRingElemOrPtr, y::UInt)
  @ccall libflint.fmpz_submul_ui(z::Ref{ZZRingElem}, x::Ref{ZZRingElem}, y::UInt)::Nothing
  return z
end

submul!(z::ZZRingElemOrPtr, x::ZZRingElemOrPtr, y::Integer) = submul!(z, x, flintify(y))

# ignore fourth argument
submul!(z::ZZRingElemOrPtr, x::ZZRingElemOrPtr, y::Union{ZZRingElemOrPtr,Integer}, ::ZZRingElemOrPtr) = submul!(z, x, y)
submul!(z::ZZRingElemOrPtr, x::Integer, y::ZZRingElemOrPtr, ::ZZRingElemOrPtr) = submul!(z, x, y)

@doc raw"""
    fmma!(r::ZZRingElem, a::ZZRingElem, b::ZZRingElem, c::ZZRingElem, d::ZZRingElem)

Return $r = a b + c d$, changing $r$ in-place.
"""
function fmma!(r::ZZRingElemOrPtr, a::ZZRingElemOrPtr, b::ZZRingElemOrPtr, c::ZZRingElemOrPtr, d::ZZRingElemOrPtr)
  @ccall libflint.fmpz_fmma(r::Ref{ZZRingElem}, a::Ref{ZZRingElem}, b::Ref{ZZRingElem}, c::Ref{ZZRingElem}, d::Ref{ZZRingElem})::Nothing
  return r
end

@doc raw"""
    fmms!(r::ZZRingElem, a::ZZRingElem, b::ZZRingElem, c::ZZRingElem, d::ZZRingElem)

Return $r = a b - c d$, changing $r$ in-place.
"""
function fmms!(r::ZZRingElemOrPtr, a::ZZRingElemOrPtr, b::ZZRingElemOrPtr, c::ZZRingElemOrPtr, d::ZZRingElemOrPtr)
  @ccall libflint.fmpz_fmms(r::Ref{ZZRingElem}, a::Ref{ZZRingElem}, b::Ref{ZZRingElem}, c::Ref{ZZRingElem}, d::Ref{ZZRingElem})::Nothing
  return r
end

#

function divexact!(z::ZZRingElemOrPtr, a::ZZRingElemOrPtr, b::ZZRingElemOrPtr)
  @ccall libflint.fmpz_divexact(z::Ref{ZZRingElem}, a::Ref{ZZRingElem}, b::Ref{ZZRingElem})::Nothing
  return z
end

function divexact!(z::ZZRingElemOrPtr, a::ZZRingElemOrPtr, b::Int)
  @ccall libflint.fmpz_divexact_si(z::Ref{ZZRingElem}, a::Ref{ZZRingElem}, b::Int)::Nothing
  return z
end

function divexact!(z::ZZRingElemOrPtr, a::ZZRingElemOrPtr, b::UInt)
  @ccall libflint.fmpz_divexact_ui(z::Ref{ZZRingElem}, a::Ref{ZZRingElem}, b::UInt)::Nothing
  return z
end

divexact!(z::ZZRingElemOrPtr, a::ZZRingElemOrPtr, b::Integer) = divexact!(z, a, flintify(b))

#

function rem!(a::ZZRingElem, b::ZZRingElem, c::ZZRingElem)
  # FIXME: it seems `rem` and `rem!` for `ZZRingElem` do different things?
  @ccall libflint.fmpz_mod(a::Ref{ZZRingElem}, b::Ref{ZZRingElem}, c::Ref{ZZRingElem})::Nothing
  return a
end

#

function tdiv_q!(a::ZZRingElem, b::ZZRingElem, c::ZZRingElem)
  @ccall libflint.fmpz_tdiv_q(a::Ref{ZZRingElem}, b::Ref{ZZRingElem}, c::Ref{ZZRingElem})::Cvoid
end

function shift_right!(a::ZZRingElem, b::ZZRingElem, i::Int)
  @ccall libflint.fmpz_fdiv_q_2exp(a::Ref{ZZRingElem}, b::Ref{ZZRingElem}, i::Int)::Nothing
end

#

function pow!(z::ZZRingElemOrPtr, x::ZZRingElemOrPtr, n::Integer)
  @ccall libflint.fmpz_pow_ui(z::Ref{ZZRingElem}, x::Ref{ZZRingElem}, UInt(n)::UInt)::Nothing
  return z
end

function pow!(z::ZZRingElemOrPtr, x::ZZRingElemOrPtr, n::ZZRingElemOrPtr)
  ok = Bool(@ccall libflint.fmpz_pow_fmpz(z::Ref{ZZRingElem}, x::Ref{ZZRingElem}, n::Ref{ZZRingElem})::Cint)
  if !ok
    error("unable to compute power")
  end
  return z
end

#

function lcm!(z::ZZRingElemOrPtr, x::ZZRingElemOrPtr, y::ZZRingElemOrPtr)
  @ccall libflint.fmpz_lcm(z::Ref{ZZRingElem}, x::Ref{ZZRingElem}, y::Ref{ZZRingElem})::Nothing
  return z
end

function gcd!(z::ZZRingElemOrPtr, x::ZZRingElemOrPtr, y::ZZRingElemOrPtr)
  @ccall libflint.fmpz_gcd(z::Ref{ZZRingElem}, x::Ref{ZZRingElem}, y::Ref{ZZRingElem})::Nothing
  return z
end

###############################################################################
#
#   Parent object overloads
#
###############################################################################

(::ZZRing)() = ZZRingElem()

(::ZZRing)(a::Integer) = ZZRingElem(a)

(::ZZRing)(a::AbstractString) = ZZRingElem(a)

(::ZZRing)(a::ZZRingElem) = a

(::ZZRing)(a::Float64) = ZZRingElem(a)

(::ZZRing)(a::Float32) = ZZRingElem(Float64(a))

(::ZZRing)(a::Float16) = ZZRingElem(Float64(a))

(::ZZRing)(a::BigFloat) = ZZRingElem(BigInt(a))

@doc raw"""
    (ZZ::ZZRing)(x)

Coerce `x` into an element of $\mathbb Z$. Note that `ZZ(x)` is equivalent to [`ZZRingElem(x)`](@ref).

# Examples

```jldoctest
julia> ZZ(2)
2

julia> ZZ(2)^100
1267650600228229401496703205376
```
"""
(::ZZRing)(x)

###############################################################################
#
#   String parser
#
###############################################################################

function parse(::Type{ZZRingElem}, s::AbstractString, base::Int = 10)
  if s[1] == '-'
    sgn = -1
    s2 = string(SubString(s, 2))
  else
    sgn = 1
    s2 = string(s)
  end
  z = ZZRingElem()
  err = @ccall libflint.fmpz_set_str(z::Ref{ZZRingElem}, s2::Ptr{UInt8}, base::Int32)::Int32
  err == 0 || error("Invalid big integer: $(repr(s))")
  return sgn < 0 ? -z : z
end

###############################################################################
#
#   Random generation
#
###############################################################################

RandomExtensions.maketype(R::ZZRing, _) = ZZRingElem

# define rand(make(ZZ, n:m))
rand(rng::AbstractRNG, sp::SamplerTrivial{<:Make2{ZZRingElem,ZZRing}}) =
sp[][1](rand(rng, sp[][2]))

rand(rng::AbstractRNG, R::ZZRing, n::AbstractArray) = R(rand(rng, n))

rand(R::ZZRing, n::AbstractArray) = rand(Random.default_rng(), R, n)

@doc raw"""
    rand_bits(::ZZRing, b::Int)

Return a random signed integer whose absolute value has $b$ bits.
"""
function rand_bits(::ZZRing, b::Int)
  b >= 0 || throw(DomainError(b, "Bit count must be non-negative"))
  z = ZZRingElem()
  @ccall libflint.fmpz_randbits(z::Ref{ZZRingElem}, _flint_rand_states[Threads.threadid()]::Ref{rand_ctx}, b::Int)::Nothing
  return z
end

@doc raw"""
    rand_bits_prime(::ZZRing, n::Int, proved::Bool=true)

Return a random prime number with the given number of bits. If only a
probable prime is required, one can pass `proved=false`.
"""
function rand_bits_prime(::ZZRing, n::Int, proved::Bool = true)
  n < 2 && throw(DomainError(n, "No primes with that many bits"))
  z = ZZRingElem()
  @ccall libflint.fmpz_randprime(z::Ref{ZZRingElem}, _flint_rand_states[Threads.threadid()]::Ref{rand_ctx}, n::Int, Cint(proved)::Cint)::Nothing
  return z
end

# rand in a range
# this mirrors the implementation for BigInt in the Random module

using Base.GMP: Limb, MPZ


Random.Sampler(::Type{<:AbstractRNG}, r::StepRange{ZZRingElem, ZZRingElem}, ::Random.Repetition) =
SamplerFmpz(r)

struct SamplerFmpz <: Random.Sampler{ZZRingElem}
  a::ZZRingElem           # first
  m::BigInt         # range length - 1
  nlimbs::Int       # number of limbs in generated BigInt's (z ∈ [0, m])
  nlimbsmax::Int    # max number of limbs for z+a
  mask::Limb        # applied to the highest limb

  ## diverges from Random.SamplerBigInt:
  step::ZZRingElem
end

function SamplerFmpz(r::StepRange{ZZRingElem, ZZRingElem})
  r1 = first(r)
  r2 = last(r)
  s = step(r)

  if isone(s)
    m = BigInt(r2 - r1)
  else
    m = length(r)::BigInt # type assertion in case length is changed to return an ZZRingElem
    MPZ.sub_ui!(m, 1)
  end

  m < 0 && throw(ArgumentError("range must be non-empty"))
  nd = ndigits(m, base=2)
  nlimbs, highbits = divrem(nd, 8*sizeof(Limb))
  highbits > 0 && (nlimbs += 1)
  mask = highbits == 0 ? ~zero(Limb) : one(Limb)<<highbits - one(Limb)
  nlimbsmax = max(nlimbs, size(r1), size(r2))
  return SamplerFmpz(r1, m, nlimbs, nlimbsmax, mask, s)
end

function rand(rng::AbstractRNG, sp::SamplerFmpz)
  z = ZZRingElem()
  # this make sure z is backed up by an mpz_t object:
  @ccall libflint.fmpz_init2(z::Ref{ZZRingElem}, sp.nlimbsmax::UInt)::Nothing
  @assert !_fmpz_is_small(z)
  GC.@preserve z begin
    x = _as_bigint(z)
    limbs = Random.UnsafeView(x.d, sp.nlimbs)
    while true
      rand!(rng, limbs)
      limbs[end] &= sp.mask
      MPZ.mpn_cmp(x, sp.m, sp.nlimbs) <= 0 && break
    end
    # adjust x.size (normally done by mpz_limbs_finish, in GMP version >= 6)
    sz = sp.nlimbs
    while sz > 0
      limbs[sz] != 0 && break
      sz -= 1
    end
    # write sz in the .size field of the mpz object
    unsafe_store!(Ptr{Cint}(z.d << 2) + sizeof(Cint), sz)
  end
  if !isone(sp.step)
    mul!(z, z, sp.step)
  end
  add!(z, z, sp.a)
end

#TODO
# need to be mapped onto proper FLINT primitives
# flints needs a proper interface to randomness - I think
# currently one simply cannot use it at all
#
# should be tied(?) to the Julia rng stuff?
# similarly, all the derived rand functions should probably also do this
#
# inspired by/copied from a former BigInt implementation from the stdlib in
# `Random/src/generation.jl`
#

function rand(rng::AbstractRNG, a::ZZRingElemUnitRange)
  m = Base.last(a) - Base.first(a)
  m < 0 && error("range empty")
  nd = ndigits(m, 2)
  nl, high = Base.divrem(nd, 8 * sizeof(Base.GMP.Limb))
  if high > 0
    mask = m >> (nl * 8 * sizeof(Base.GMP.Limb))
  end
  s = ZZRingElem(0)
  c = (8 * sizeof(Base.GMP.Limb))
  while true
    s = ZZRingElem(0)
    for i = 1:nl
      @ccall libflint.fmpz_mul_2exp(s::Ref{ZZRingElem}, s::Ref{ZZRingElem}, c::Int)::Nothing
      add!(s, s, rand(rng, Base.GMP.Limb))
    end
    if high > 0
      s = s << high
      s += rand(rng, 0:Base.GMP.Limb(mask))
    end
    if s <= m
      break
    end
  end
  return s + first(a)
end

struct RangeGeneratorfmpz# <: Base.Random.RangeGenerator
  a::StepRange{ZZRingElem,ZZRingElem}
end

function Random.RangeGenerator(r::StepRange{ZZRingElem,ZZRingElem})
  m = last(r) - first(r)
  m < 0 && throw(ArgumentError("range must be non-empty"))
  return RangeGeneratorfmpz(r)
end

function rand(rng::AbstractRNG, g::RangeGeneratorfmpz)
  return rand(rng, g.a)
end

function rand!(A::Vector{ZZRingElem}, v::StepRange{ZZRingElem,ZZRingElem})
  for i in 1:length(A)
    A[i] = rand(v)
  end
  return A
end

###############################################################################
#
#   Conformance test element generation
#
###############################################################################

function ConformanceTests.generate_element(R::ZZRing)
  return rand_bits(ZZ, rand(0:100))
end

###############################################################################
#
#   Constructors
#
###############################################################################

ZZRingElem(s::AbstractString) = parse(ZZRingElem, s)

ZZRingElem(z::Integer) = ZZRingElem(BigInt(z))

ZZRingElem(z::Float16) = ZZRingElem(Float64(z))

ZZRingElem(z::Float32) = ZZRingElem(Float64(z))

ZZRingElem(z::BigFloat) = ZZRingElem(BigInt(z))

###############################################################################
#
#   Conversions and promotions
#
###############################################################################

convert(::Type{ZZRingElem}, a::Integer) = ZZRingElem(a)

function (::Type{BigInt})(a::ZZRingElem)
  r = BigInt()
  @ccall libflint.fmpz_get_mpz(r::Ref{BigInt}, a::Ref{ZZRingElem})::Nothing
  return r
end

function (::Type{Int})(a::ZZRingElem)
  _fmpz_is_small(a) && return data(a)
  fits(Int, a) || throw(InexactError(:convert, Int, a))
  return @ccall libflint.fmpz_get_si(a::Ref{ZZRingElem})::Int
end

function (::Type{UInt})(a::ZZRingElem)
  fits(UInt, a) || throw(InexactError(:convert, UInt, a))
  return rem(a, UInt)
end

if Culong !== UInt
  function (::Type{Culong})(a::ZZRingElem)
    _fmpz_is_small(a) && return Culong(data(a))
    fits(Culong, a) || throw(InexactError(:convert, Culong, a))
    return (@ccall libflint.fmpz_get_ui(a::Ref{ZZRingElem})::UInt) % Culong
  end
end

(::Type{T})(a::ZZRingElem) where T <: Union{Int8, Int16, Int32} = T(Int(a))

(::Type{T})(a::ZZRingElem) where T <: Union{UInt8, UInt16, UInt32} = T(UInt(a))

function (::Type{T})(a::ZZRingElem) where T <: Union{Int128, UInt128}
  _fmpz_is_small(a) && return T(data(a))
  T(BigInt(a))
end

Base.Integer(a::ZZRingElem) = BigInt(a)

convert(::Type{T}, a::ZZRingElem) where T <: Integer = T(a)

function (::Type{Float64})(n::ZZRingElem)
  # rounds to zero
  @ccall libflint.fmpz_get_d(n::Ref{ZZRingElem})::Float64
end

convert(::Type{Float64}, n::ZZRingElem) = Float64(n)

(::Type{Float32})(n::ZZRingElem) = Float32(Float64(n))

convert(::Type{Float32}, n::ZZRingElem) = Float32(n)

(::Type{Float16})(n::ZZRingElem) = Float16(Float64(n))

convert(::Type{Float16}, n::ZZRingElem) = Float16(n)

(::Type{BigFloat})(n::ZZRingElem) = BigFloat(BigInt(n))

convert(::Type{BigFloat}, n::ZZRingElem) = BigFloat(n)

Base.promote_rule(::Type{ZZRingElem}, ::Type{T}) where {T <: Integer} = ZZRingElem

promote_rule(::Type{ZZRingElem}, ::Type{T}) where {T <: Integer} = ZZRingElem


#output sensitive rational_reconstruction, in particular if
#numerator is larger than den 
#used below in rational_reconstruction and, more seriously, in the 
#solvers in ZZMatrix-Linalg

function _ratrec!(n::ZZRingElem, d::ZZRingElem, a::ZZRingElem, b::ZZRingElem, N::ZZRingElem = ZZ(), D::ZZRingElem= ZZ())
  k = nbits(b)
  l = 1
  set!(N, b)
  set!(D, 2)

#  @assert 0<a<b
  done = false
  while !done && D <= N
    Nemo.mul!(D, D, D)
    tdiv_q!(N, b, D)
    shift_right!(N, N, 1)
    if D>N
      @ccall Nemo.libflint.fmpz_root(N::Ref{ZZRingElem}, b::Ref{ZZRingElem}, 2::Int)::Nothing
      shift_right!(D, N, 1)
      done = true
    end

#    @assert 2*N*D < b

    fl = @ccall libflint._fmpq_reconstruct_fmpz_2(n::Ref{ZZRingElem}, d::Ref{ZZRingElem}, a::Ref{ZZRingElem}, b::Ref{ZZRingElem}, N::Ref{ZZRingElem}, D::Ref{ZZRingElem})::Bool

    if fl && (nbits(n)+nbits(d) < max(k/2, k - 30) || D>N)
      return fl
    end
    l += 1
  end
  return false
end

#Note: this is not the best (fastest) algorithm, not the most general
#      signature, not the best (fastest) interface, ....
#However: for now it works.

@doc raw"""
    _rational_reconstruction(a::ZZRingElem, b::ZZRingElem)
    _rational_reconstruction(a::Integer, b::Integer)

Tries to solve $ay=x mod b$ for $x,y < sqrt(b/2)$. If possible, returns
  (`true`, $x$, $y$) or (`false`, garbage) if not possible.

If `unbalanced` is set to `true`, a solution is accepted if `nbits(x) + nbits(y) + 30 <= nbits(b)` or if the compined size of smaller than half of the modulus - this allows for the numberator or denominator to be much smaller
than the other one.

By default `y` and `b` have to be coprime for a valid solution. It is
well known that then the solution is unique.

If `error_tolerant` is set to `true`, then a solution is also accepted if
`x`, `y` and `b` have a common divisor `g` and if
  `a(y/g) = (x/g) mod (b/g)` is true and if the combined size is small enough.

The typical application are modular algorithms where
 - there are finitely many bad primes (ie. the `mod p` datum does
   not match the global solution modulo `p`)
 - that cannot be detected
In this case `g` will be the product of the bad primes.

See also [`reconstruct`](@ref).

"""
function _rational_reconstruction(a::ZZRingElem, b::ZZRingElem; error_tolerant::Bool = false, unbalanced::Bool = false)
  @req !error_tolerant || !unbalanced "only one of `error_tolerant` and `unbalanced` can be used at a time"

  if unbalanced
    n = ZZ()
    d = ZZ()
    fl = _ratrec!(n, d, a, b)
    return fl, n, d
  elseif error_tolerant
    m = matrix(ZZ, 2, 2, [a, ZZRingElem(1), b, ZZRingElem(0)])
    lll!(m)
    x = m[1,1]
    y = m[1,2]
    @assert (a*y-x) % b == 0
    g = gcd(x, y)
    divexact!(x, g)
    divexact!(y, g)
    return nbits(x)+nbits(y)+2*nbits(g) + 20 < nbits(b), x, y
  else
    res = QQFieldElem()
    a = mod(a, b)
    fl = @ccall libflint.fmpq_reconstruct_fmpz(res::Ref{QQFieldElem}, a::Ref{ZZRingElem}, b::Ref{ZZRingElem})::Cint
    return Bool(fl), numerator(res), denominator(res)
  end
end

@doc raw"""
    _rational_reconstruction(a::ZZRingElem, b::ZZRingElem, N::ZZRingElem, D::ZZRingElem) -> Bool, ZZRingElem, ZZRingElem

Given $a$ modulo $b$ and $N>0$, $D>0$ such that $2ND<b$, find $|x|\le N$, $0<y\le D$
satisfying $x/y \equiv a \bmod b$ or $a \equiv ya \bmod b$.
"""
function _rational_reconstruction(a::ZZRingElem, b::ZZRingElem, N::ZZRingElem, D::ZZRingElem)
  res = QQFieldElem()
  a = mod(a, b)
  fl = @ccall libflint.fmpq_reconstruct_fmpz_2(res::Ref{QQFieldElem}, a::Ref{ZZRingElem}, b::Ref{ZZRingElem}, N::Ref{ZZRingElem}, D::Ref{ZZRingElem})::Cint
  return Bool(fl), numerator(res), denominator(res)
end


###############################################################################
#
#  Perfect power detection
#
###############################################################################

# 1, 0, -1 are perfect powers
# ex is not guaranteed to be maximal
function _is_perfect_power(a::ZZRingElem)
  rt = ZZRingElem()
  ex = @ccall libflint.fmpz_is_perfect_power(rt::Ref{ZZRingElem}, a::Ref{ZZRingElem})::Int
  return rt, ex
end

@doc raw"""
    is_perfect_power(a::IntegerUnion)

Return whether $a$ is a perfect power, that is, whether $a = m^r$ for some
integer $m$ and $r > 1$. Neither $m$ nor $r$ is returned.
"""
function is_perfect_power(a::ZZRingElem)
  _, ex = _is_perfect_power(a)
  return ex > 0
end

is_perfect_power(a::Integer) = is_perfect_power(ZZRingElem(a))

#compare to Oscar/examples/PerfectPowers.jl which is, for large input,
#far superior over gmp/ fmpz_is_perfect_power

@doc raw"""
    is_perfect_power_with_data(a::ZZRingElem) -> Int, ZZRingElem
    is_perfect_power_with_data(a::Integer) -> Int, Integer

Return $e$, $r$ such that $a = r^e$ with $e$ maximal. Note: $1 = 1^0$.
"""
function is_perfect_power_with_data(a::ZZRingElem)
  if iszero(a)
    error("must not be zero")
  end
  if isone(a)
    return 0, a
  end
  if a < 0
    e, r = is_perfect_power_with_data(-a)
    if isone(e)
      return 1, a
    end
    v, s = iszero(e) ? (0, 0) : remove(e, 2)
    return s, -r^(2^v)
  end
  rt = ZZRingElem()
  e = 1
  while true
    rt, ex = _is_perfect_power(a)
    if ex == 1 || ex == 0
      return e, a
    end
    e *= ex
    a = rt
  end
end

function is_perfect_power_with_data(a::Integer)
  e, r = is_perfect_power_with_data(ZZRingElem(a))
  return e, typeof(a)(r)
end

@doc raw"""
    is_power(a::ZZRingElem, n::Int) -> Bool, ZZRingElem
    is_power(a::QQFieldElem, n::Int) -> Bool, QQFieldElem
    is_power(a::Integer, n::Int) -> Bool, Integer

Return `true` and the root if $a$ is an $n$-th power.
"""
function is_power(a::ZZRingElem, n::Int)
  if a < 0 && is_even(n)
    return false, a
  end
  b = iroot(a, n)
  return b^n == a, b
end

@doc raw"""
    is_prime_power(q::IntegerUnion) -> Bool

Returns whether $q$ is a prime power.
"""
is_prime_power(::IntegerUnion)

function is_prime_power(q::ZZRingElem)
  iszero(q) && return false
  e, a = is_perfect_power_with_data(q)
  return is_prime(a)
end

is_prime_power(q::Integer) = is_prime_power(ZZRingElem(q))

@doc raw"""
    is_prime_power_with_data(q::IntegerUnion) -> Bool, ZZRingElem, Int

Returns a flag indicating whether $q$ is a prime power and integers $e, p$ such
that $q = p^e$. If $q$ is a prime power, than $p$ is a prime.
"""
is_prime_power_with_data(::IntegerUnion)

function is_prime_power_with_data(q::ZZRingElem)
  iszero(q) && return false, 1, q
  e, a = is_perfect_power_with_data(q)
  return is_prime(a), e, a
end

function is_prime_power_with_data(q::Integer)
  e, a = is_perfect_power_with_data(ZZRingElem(q))
  return is_prime(a), e, typeof(q)(a)
end

###############################################################################
#
#   Convenience methods for arithmetics (since `ZZRingElem` is not a `Number` type)
#
###############################################################################

//(v::Vector{ZZRingElem}, x::ZZRingElem) = v .// x
*(x::ZZRingElem, v::Vector{ZZRingElem}) = x .* v
*(v::Vector{ZZRingElem}, x::ZZRingElem) = v .* x

###############################################################################
#
#   Bit iteration
#
###############################################################################

function bits end

module BitsMod

using ..Nemo

import Base: ^
import Base: eltype
import Base: getindex
import Base: iterate
import Base: length
import Base: show

export bits
export Limbs

const hb = UInt(1) << 63

struct Limbs
  a::ZZRingElem
  len::Int
  b::Ptr{UInt}
  function Limbs(a::ZZRingElem; MSW::Bool=true)
    if Nemo._fmpz_is_small(a)
      return new(a, 0, convert(Ptr{UInt}, 0))
    end
    z = convert(Ptr{Cint}, unsigned(a.d) << 2)
    len = unsafe_load(z, 2)
    d = convert(Ptr{Ptr{UInt}}, unsigned(a.d) << 2) + 2 * sizeof(Cint)
    p = unsafe_load(d)
    if !MSW
      new(a, -len, p)
    else
      new(a, len, p)
    end
  end
end

function show(io::IO, L::Limbs)
  print(io, "limb-access for: ", L.a)
end

@inline function getindex(L::Limbs, i::Int)
  if L.len == 0
    return UInt(abs(L.a.d)) #error???
  end
  @boundscheck @assert i <= abs(L.len)
  return unsafe_load(L.b, i)
end

function iterate(L::Limbs)
  is_zero(L.a) && return nothing # nbits(ZZ()) == 0
  L.len < 0 && return L[1], 1

  return L[L.len], L.len
end

function iterate(L::Limbs, i::Int)
  if L.len < 0
    i > -L.len && return nothing
    return L[i+1], i + 1
  end
  i == 0 && return nothing
  return L[i-1], i - 1
end

function length(L::Limbs)
  is_zero(L.a) && return 0 # nbits(ZZ()) == 0
  return L.len + 1
end

eltype(L::Limbs) = UInt

#=
#from https://github.com/JuliaLang/julia/issues/11592
#compiles directly down to the ror/rol in assembly
for T in Base.BitInteger_types
mask = UInt8(sizeof(T) << 3 - 1)
@eval begin
ror(x::$T, k::Integer) = (x >>> ($mask & k)) | (x <<  ($mask & -k))
rol(x::$T, k::Integer) = (x <<  ($mask & k)) | (x >>> ($mask & -k))
end
end
=#

struct BitsFmpz
  L::Limbs

  function BitsFmpz(b::ZZRingElem)
    return new(Limbs(b))
  end
end

function iterate(B::BitsFmpz)
  L = B.L
  is_zero(L.a) && return nothing
  a = L[L.len]
  b = UInt(1) << (nbits(a) - 1)
  return true, (b, L.len)
end

@inline function iterate(B::BitsFmpz, s::Tuple{UInt,Int})
  b = s[1] >> 1
  if b == 0
    l = s[2] - 1
    if l < 1
      return nothing
    end
    b = hb
    a = B.L[l]
    return a & b != 0, (b, l)
  end
  return B.L[s[2]] & b != 0, (b, s[2])
end

function show(io::IO, B::BitsFmpz)
  print(io, "bit iterator for: ", B.L.a)
end

length(B::BitsFmpz) = nbits(B.L.a)

eltype(B::BitsFmpz) = Bool

Nemo.bits(a::ZZRingElem) = BitsFmpz(a)
#= wrong order, thus disabled

function getindex(B::BitsFmpz, i::Int)
return @ccall libflint.fmpz_tstbit(B.L.a::Ref{ZZRingElem}, i::Int)::Int != 0
end
=#
end

using .BitsMod

###############################################################################
#
#   Resultant
#
###############################################################################

# From the flint docs:
# Computes the resultant of f and g divided by d [...]. It is assumed that the
# resultant is exactly divisible by d and the result res has at most nb bits.
# This bypasses the computation of general bounds.
function resultant(f::ZZPolyRingElem, g::ZZPolyRingElem, d::ZZRingElem, nb::Int)
  z = ZZRingElem()
  @ccall libflint.fmpz_poly_resultant_modular_div(z::Ref{ZZRingElem}, f::Ref{ZZPolyRingElem}, g::Ref{ZZPolyRingElem}, d::Ref{ZZRingElem}, nb::Int)::Nothing
  return z
end
