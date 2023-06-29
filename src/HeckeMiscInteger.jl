################################################################################
#
#  Integer functions
#
################################################################################

function isless(a::BigFloat, b::ZZRingElem)
    if _fmpz_is_small(b)
        c = ccall((:mpfr_cmp_si, :libmpfr), Int32, (Ref{BigFloat}, Int), a, b.d)
    else
        c = ccall((:mpfr_cmp_z, :libmpfr), Int32, (Ref{BigFloat}, UInt), a, unsigned(b.d) << 2)
    end
    return c < 0
end

function mulmod(a::UInt, b::UInt, n::UInt, ni::UInt)
    ccall((:n_mulmod2_preinv, libflint), UInt, (UInt, UInt, UInt, UInt), a, b, n, ni)
end

# TODO (CF):
# should be Bernstein'ed: this is slow for large valuations
# returns the maximal v s.th. z mod p^v == 0 and z div p^v
#   also useful if p is not prime....
#
# TODO: what happens to z = 0???

function remove(z::T, p::T) where {T<:Integer}
    z == 0 && return (0, z)
    v = 0
    @assert p > 1
    while mod(z, p) == 0
        z = Base.div(z, p)
        v += 1
    end
    return (v, z)
end

function remove(z::Rational{T}, p::T) where {T<:Integer}
    z == 0 && return (0, z)
    v, d = remove(denominator(z), p)
    w, n = remove(numerator(z), p)
    return w - v, n // d
end

function valuation(z::T, p::T) where {T<:Integer}
    iszero(z) && error("Not yet implemented")
    v = 0
    @assert p > 1
    while mod(z, p) == 0
        z = Base.div(z, p)
        v += 1
    end
    return v
end

function valuation(z::Rational{T}, p::T) where {T<:Integer}
    z == 0 && error("Not yet implemented")
    v = valuation(denominator(z), p)
    w = valuation(numerator(z), p)
    return w - v
end

@inline __get_rounding_mode() = Base.MPFR.rounding_raw(BigFloat)

function BigFloat(a::QQFieldElem)
    r = BigFloat(0)
    ccall((:fmpq_get_mpfr, libflint), Nothing, (Ref{BigFloat}, Ref{QQFieldElem}, Int32), r, a, __get_rounding_mode())
    return r
end

function isless(a::Float64, b::QQFieldElem)
    return a < BigFloat(b)
end
function isless(a::QQFieldElem, b::Float64)
    return BigFloat(a) < b
end

function isless(a::Float64, b::ZZRingElem)
    return a < BigFloat(b)
end
function isless(a::ZZRingElem, b::Float64)
    return BigFloat(a) < b
end

#function ^(a::ZZRingElem, k::ZZRingElem)
#  if a == 0
#    if k == 0
#      return ZZRingElem(1)
#    end
#    return ZZRingElem(0)
#  end
#
#  if a == 1
#    return ZZRingElem(1)
#  end
#  if a == -1
#    if isodd(k)
#      return ZZRingElem(-1)
#    else
#      return ZZRingElem(1)
#    end
#  end
#  return a^Int(k)
#end


function *(a::ZZRingElem, b::BigFloat)
    return BigInt(a) * b
end

function *(a::BigFloat, b::ZZRingElem)
    return BigInt(a) * b
end

function *(a::ZZRingElem, b::Float64)
    return BigInt(a) * b
end

function *(b::Float64, a::ZZRingElem)
    return BigInt(a) * b
end

function *(a::QQFieldElem, b::Float64)
    return Rational(a) * b
end

function *(b::Float64, a::QQFieldElem)
    return Rational(a) * b
end

function convert(R::Type{Rational{Base.GMP.BigInt}}, a::ZZRingElem)
    return R(BigInt(a))
end

log(a::ZZRingElem) = log(BigInt(a))
log(a::QQFieldElem) = log(numerator(a)) - log(denominator(a))

function log(a::ZZRingElem, b::ZZRingElem)
    log(b) / log(a)
end

function divisible(x::Integer, y::Integer)
    return iszero(rem(x, y))
end

Base.in(x::IntegerUnion, r::AbstractRange{ZZRingElem}) =
    !isempty(r) && first(r) <= x <= last(r) &&
    mod(convert(ZZRingElem, x), step(r)) == mod(first(r), step(r))

function Base.getindex(a::StepRange{ZZRingElem,ZZRingElem}, i::ZZRingElem)
    a.start + (i - 1) * Base.step(a)
end


################################################################################
#
#  power detection
#
################################################################################
#compare to Oscar/examples/PerfectPowers.jl which is, for large input,
#far superiour over gmp/ fmpz_is_perfect_power

@doc raw"""
    is_power(a::ZZRingElem) -> Int, ZZRingElem
    is_power(a::Integer) -> Int, Integer

Returns $e$, $r$ such that $a = r^e$ with $e$ maximal. Note: $1 = 1^0$.
"""
function is_power(a::ZZRingElem)
    if iszero(a)
        error("must not be zero")
    end
    if isone(a)
        return 0, a
    end
    if a < 0
        e, r = is_power(-a)
        if isone(e)
            return 1, a
        end
        v, s = iszero(e) ? (0, 0) : remove(e, 2)
        return s, -r^(2^v)
    end
    rt = ZZRingElem()
    e = 1
    while true
        ex = ccall((:fmpz_is_perfect_power, libflint), Int, (Ref{ZZRingElem}, Ref{ZZRingElem}), rt, a)
        if ex == 1 || ex == 0
            return e, a
        end
        e *= ex
        a = rt
    end
end

function is_power(a::Integer)
    e, r = is_power(ZZRingElem(a))
    return e, typeof(a)(r)
end

@doc raw"""
    is_power(a::QQFieldElem) -> Int, QQFieldElem
    is_power(a::Rational) -> Int, Rational

Writes $a = r^e$ with $e$ maximal. Note: $1 = 1^0$.
"""
function is_power(a::QQFieldElem)
    e, r = is_power(numerator(a))
    if e == 1
        return e, a
    end
    f, s = is_power(denominator(a))
    g = gcd(e, f)
    return g, r^Base.div(e, g) // s^Base.div(f, g)
end

function is_power(a::Rational)
    T = typeof(denominator(a))
    e, r = is_power(QQFieldElem(a))
    return e, T(numerator(r)) // T(denominator(r))
end

@doc raw"""
    is_power(a::ZZRingElem, n::Int) -> Bool, ZZRingElem
    is_power(a::QQFieldElem, n::Int) -> Bool, QQFieldElem
    is_power(a::Integer, n::Int) -> Bool, Integer

Tests if $a$ is an $n$-th power. Return `true` and the root if successful.
"""
function is_power(a::ZZRingElem, n::Int)
    if a < 0 && iseven(n)
        return false, a
    end
    b = iroot(a, n)
    return b^n == a, b
end

function is_power(a::QQFieldElem, n::Int)
    fl, nu = is_power(numerator(a), n)
    if !fl
        return fl, a
    end
    fl, de = is_power(denominator(a), n)
    return fl, QQFieldElem(nu, de)
end

@doc raw"""
    nbits(a::Integer) -> Int

Returns the number of bits necessary to represent $a$.
"""
function nbits(a::Integer)
    return ndigits(a, base=2)
end

@doc raw"""
    isinteger(a::QQFieldElem) -> Bool

Returns `true` iff the denominator of $a$ is one.
"""
function isinteger(a::QQFieldElem)
    return isone(denominator(a))
end

function (::ZZRing)(x::Rational{Int})
    @assert denominator(x) == 1
    return ZZRingElem(numerator(x))
end

function ceil(::Type{ZZRingElem}, a::BigFloat)
    return ZZRingElem(ceil(BigInt, a))
end

function ceil(::Type{Int}, a::QQFieldElem)
    return Int(ceil(ZZRingElem, a))
end

function floor(::Type{ZZRingElem}, a::BigFloat)
    return ZZRingElem(floor(BigInt, a))
end

function floor(::Type{Int}, a::QQFieldElem)
    return Int(floor(ZZRingElem, a))
end

function round(::Type{ZZRingElem}, a::BigFloat)
    return ZZRingElem(round(BigInt, a))
end

function round(::Type{Int}, a::BigFloat)
    return Int(round(ZZRingElem, a))
end

/(a::BigFloat, b::ZZRingElem) = a / BigInt(b)

is_negative(n::IntegerUnion) = cmp(n, 0) < 0
is_positive(n::IntegerUnion) = cmp(n, 0) > 0


################################################################################
#
#  is_squarefree
#
################################################################################

#TODO (Hard): Implement this properly.
@doc raw"""
    is_squarefree(n::Union{Int, ZZRingElem}) -> Bool

Returns true if $n$ is squarefree, false otherwise.
"""
function is_squarefree(n::Union{Int,ZZRingElem})
    if iszero(n)
        error("Argument must be non-zero")
    end
    if isone(abs(n))
        return true
    end
    e, b = is_power(n)
    if e > 1
        return false
    end
    return isone(maximum(values(factor(n).fac)))
end


################################################################################
#
#  Rounding and friends
#
################################################################################

Base.floor(::Type{ZZRingElem}, x::Int) = ZZRingElem(x)

Base.ceil(::Type{ZZRingElem}, x::Int) = ZZRingElem(x)

Base.floor(::Type{ZZRingElem}, x::QQFieldElem) = fdiv(numerator(x), denominator(x))

Base.ceil(::Type{ZZRingElem}, x::QQFieldElem) = cdiv(numerator(x), denominator(x))

Base.round(x::QQFieldElem, ::RoundingMode{:Up}) = ceil(x)

Base.round(::Type{ZZRingElem}, x::QQFieldElem, ::RoundingMode{:Up}) = ceil(ZZRingElem, x)

Base.round(x::QQFieldElem, ::RoundingMode{:Down}) = floor(x)

Base.round(::Type{ZZRingElem}, x::QQFieldElem, ::RoundingMode{:Down}) = floor(ZZRingElem, x)

function Base.round(x::QQFieldElem, ::RoundingMode{:Nearest})
    d = denominator(x)
    n = numerator(x)
    if d == 2
        if mod(n, 4) == 1
            if n > 0
                return Base.div(n, d)
            else
                return Base.div(n, d) - 1
            end
        else
            if n > 0
                return Base.div(n, d) + 1
            else
                return Base.div(n, d)
            end
        end
    end

    return floor(x + 1 // 2)
end

Base.round(x::QQFieldElem, ::RoundingMode{:NearestTiesAway}) = sign(x) * floor(abs(x) + 1 // 2)

Base.round(::Type{ZZRingElem}, x::QQFieldElem, ::RoundingMode{:NearestTiesAway}) = sign(x) == 1 ? floor(ZZRingElem, abs(x) + 1 // 2) : -floor(ZZRingElem, abs(x) + 1 // 2)

function Base.round(::Type{ZZRingElem}, a::QQFieldElem)
    return round(ZZRingElem, a, RoundNearestTiesAway)
end

function Base.round(a::QQFieldElem)
    return round(ZZRingElem, a)
end

clog(a::Int, b::Int) = clog(ZZRingElem(a), b)
