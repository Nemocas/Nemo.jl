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


function *(a::ZZRingElem, b::AbstractFloat)
    return BigInt(a) * b
end

function *(a::AbstractFloat, b::ZZRingElem)
    return a * BigInt(b)
end

function *(a::QQFieldElem, b::AbstractFloat)
    return Rational(a) * b
end

function *(a::AbstractFloat, b::QQFieldElem)
    return a * Rational(b)
end

function convert(R::Type{Rational{Base.GMP.BigInt}}, a::ZZRingElem)
    return R(BigInt(a))
end

log(a::ZZRingElem) = log(BigInt(a))
log(a::QQFieldElem) = log(numerator(a)) - log(denominator(a))

function log(a::ZZRingElem, b::ZZRingElem)
    log(b) / log(a)
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

function (::ZZRing)(x::Rational{Int})
    @assert denominator(x) == 1
    return ZZRingElem(numerator(x))
end

/(a::BigFloat, b::ZZRingElem) = a / BigInt(b)


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
    iszero(n) && return false
    is_unit(n) && return true
    e, b = is_power(n)
    if e > 1
        return false
    end
    return isone(maximum(values(factor(n).fac); init = 1))
end


################################################################################
#
#  Rounding and friends
#
################################################################################

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

clog(a::Int, b::Int) = clog(ZZRingElem(a), b)

function Float64(a::QQFieldElem)
    b = a * ZZRingElem(2)^53
    Float64(div(numerator(b), denominator(b))) / (Float64(2)^53) #CF 2^53 is bad in 32bit
end

function euler_phi(x::Fac{ZZRingElem})
    return prod((p - 1) * p^(v - 1) for (p, v) = x.fac)
end

Integer(a::ZZRingElem) = BigInt(a)

^(a::T, n::IntegerUnion) where {T<:RingElem} = _generic_power(a, n)

function _generic_power(a, n::IntegerUnion)
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

################################################################################
#
#  Modular reduction with symmetric residue system
#
################################################################################

function mod_sym(a::ZZRingElem, b::ZZRingElem)
    c = mod(a, b)
    @assert c >= 0
    if b > 0 && 2 * c > b
        return c - b
    elseif b < 0 && 2 * c > -b
        return c + b
    else
        return c
    end
end

##
## Ranges
##
# Note, we cannot get a UnitRange as this is only legal for subtypes of Real.
# So, we use an AbstractUnitRange here mostly copied from `base/range.jl`.
# `StepRange`s on the other hand work out of the box thanks to duck typing.

struct ZZRingElemUnitRange <: AbstractUnitRange{ZZRingElem}
    start::ZZRingElem
    stop::ZZRingElem
    ZZRingElemUnitRange(start, stop) = new(start, fmpz_unitrange_last(start, stop))
end
fmpz_unitrange_last(start::ZZRingElem, stop::ZZRingElem) =
    ifelse(stop >= start, stop, start - one(ZZRingElem))

Base.:(:)(a::ZZRingElem, b::ZZRingElem) = ZZRingElemUnitRange(a, b)

@inline function getindex(r::ZZRingElemUnitRange, i::ZZRingElem)
    val = r.start + (i - 1)
    @boundscheck _in_unit_range(r, val) || throw_boundserror(r, i)
    val
end
_in_unit_range(r::ZZRingElemUnitRange, val::ZZRingElem) = r.start <= val <= r.stop

show(io::IO, r::ZZRingElemUnitRange) = print(io, repr(first(r)), ':', repr(last(r)))

in(x::IntegerUnion, r::ZZRingElemUnitRange) = first(r) <= x <= last(r)

mod(i::IntegerUnion, r::ZZRingElemUnitRange) = mod(i - first(r), length(r)) + first(r)

Base.:(:)(a::ZZRingElem, b::Integer) = (:)(promote(a, b)...)
Base.:(:)(a::Integer, b::ZZRingElem) = (:)(promote(a, b)...)

Base.:(:)(x::Int, y::ZZRingElem) = ZZRingElem(x):y

# Construct StepRange{ZZRingElem, T} where +(::ZZRingElem, zero(::T)) must be defined
Base.:(:)(a::ZZRingElem, s, b::Integer) = ((a_, b_) = promote(a, b); a_:s:b_)
Base.:(:)(a::Integer, s, b::ZZRingElem) = ((a_, b_) = promote(a, b); a_:s:b_)

#TODO
# need to be mapped onto proper Flint primitives
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
            ccall((:fmpz_mul_2exp, libflint), Nothing,
              (Ref{ZZRingElem}, Ref{ZZRingElem}, Int), s, s, c)
            ccall((:fmpz_add_ui, libflint), Nothing, 
              (Ref{ZZRingElem}, Ref{ZZRingElem}, UInt), s, s, rand(rng, Base.GMP.Limb))
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


function bits end

module BitsMod

using ..Nemo

import Base: ^
import Base: getindex
import Base: iterate
import Base: length
import Base: show

export bits
export Limbs


const hb = UInt(1) << 63

#= not used - lacks length
struct BitsUInt
  a::UInt
end

function bits(a::UInt)
  l = nbits(a)
  return BitsUInt(a<<(sizeof(a)*8-l))
end


function Base.iterate(x::BitsUInt)
  return iterate(x, x.a)
end

@inline function Base.iterate(x::BitsUInt, u::UInt)
  iszero(u) && return nothing
  return (u&hb) != 0, u<<1
end
=#

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

length(L::Limbs) = L.len + 1

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
    print(io, "bit iterator for:", B.L.a)
end

length(B::BitsFmpz) = nbits(B.L.a)

Nemo.bits(a::ZZRingElem) = BitsFmpz(a)
#= wrong order, thus disabled

function getindex(B::BitsFmpz, i::Int)
  return ccall((:fmpz_tstbit, libflint), Int, (Ref{ZZRingElem}, Int), B.L.a, i) != 0
end
=#

end

using .BitsMod
