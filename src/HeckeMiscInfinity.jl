export PosInf, inf, IntExt, is_infinite

# This is a type for positive infinity for use in valuations.

struct PosInf
end

const inf = PosInf()

+(::Int, ::PosInf) = inf

+(::PosInf, ::Int) = inf

+(::PosInf, ::PosInf) = inf

-(::PosInf, ::Int) = inf

Base.max(::Int, ::PosInf) = inf

Base.max(::PosInf, ::Int) = inf

Base.isless(::Int, ::PosInf) = true

Base.isless(x::Rational{Int}, ::PosInf) = denominator(x) != 0

Base.isless(::PosInf, ::PosInf) = false

Base.isless(::PosInf, ::Int) = false

Base.isless(::PosInf, ::Rational{Int}) = false

Base.isfinite(::PosInf) = false

Base.isinf(::PosInf) = true

Base.isone(::PosInf) = false

Base.iszero(::PosInf) = false

Base.one(::PosInf) = 1

Base.zero(::PosInf) = 0

Base.isless(::PosInf, ::ZZRingElem) = false

Base.isless(::ZZRingElem, ::PosInf) = true

Base.isless(::PosInf, ::QQFieldElem) = false

Base.isless(::QQFieldElem, ::PosInf) = true

const IntExt = Union{Int,PosInf}

is_positive(::PosInf) = true

@doc raw"""
    is_infinite(x::Any) -> Bool

Tests whether $x$ is infinite, by returning `!isfinite(x)`.
"""
is_infinite(x::Any) = !isfinite(x)
