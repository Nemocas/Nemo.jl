export PosInf, NegInf, inf, IntExt, is_infinite

"""
    PosInf

This singleton type represents positive infinity, as in: a value larger
than any real number. For use in valuations and elsewhere.

See [`NegInf`](@ref).
"""
struct PosInf
end

"""
    NegInf

This singleton type represents negative infinity, as in: a value smaller
than any real number. For use in valuations and elsewhere.

See [`PosInf`](@ref).
"""
struct NegInf
end

# type union for convenience later on
const AnyInf = Union{PosInf,NegInf}

# another convenience type union
# TODO: maybe deprecate this one, or at least rename it; the current one seems
# somewhat arbitrary now that we also have negative infinity
const IntExt = Union{Int,PosInf}

const inf = PosInf() # TODO: for backwards compatibility; deprecate?

#const infinity = PosInf()  # FIXME: can't have this as we already have `infinity(C::CalciumField)`

########################################
#
# basics
#
########################################

# match the hash values of Inf and -Inf, as we also compare equal to them
Base.hash(::PosInf, h::UInt) = hash(Inf, h)
Base.hash(::NegInf, h::UInt) = hash(-Inf, h)

Base.show(io::IO, ::PosInf) = print(io, "infinity") # FIXME: if we can't have `infinity` as a global, maybe better print as `inf`???
Base.show(io::IO, ::NegInf) = print(io, "-infinity")

Base.one(::AnyInf) = 1
Base.zero(::AnyInf) = 0

########################################

Base.signbit(::PosInf) = false
Base.signbit(::NegInf) = true

########################################
#
# comparison
#
########################################

Base.:(==)(inf1::AnyInf, inf2::AnyInf) = signbit(inf1) == signbit(inf2)
Base.:(==)(x::AnyInf, y::Real) = isinf(y) && signbit(y) == signbit(x)
Base.:(==)(y::Real, x::AnyInf) = x == y


Base.isless(x::Real, ::PosInf) = isfinite(x) || signbit(x)
Base.isless(::PosInf, ::Real) = false

Base.isless(::Real, ::NegInf) = false
Base.isless(::NegInf, x::Real) = isfinite(x) || !signbit(x)

Base.isless(inf1::AnyInf, inf2::AnyInf) = signbit(inf1) && !signbit(inf2)


Base.isless(::PosInf, ::Union{ZZRingElem,QQFieldElem}) = false
Base.isless(::Union{ZZRingElem,QQFieldElem}, ::PosInf) = true

Base.isless(::NegInf, ::Union{ZZRingElem,QQFieldElem}) = true
Base.isless(::Union{ZZRingElem,QQFieldElem}, ::NegInf) = false

########################################
#
# other predicates
#
########################################

Base.isfinite(::AnyInf) = false
Base.isinf(::AnyInf) = true

Base.isone(::AnyInf) = false
Base.iszero(::AnyInf) = false

is_positive(::PosInf) = true
is_positive(::NegInf) = false

is_negative(::PosInf) = false
is_negative(::NegInf) = true

@doc raw"""
    is_infinite(x::Any) -> Bool

Test whether $x$ is infinite.
"""
is_infinite(x::Any) = !isfinite(x)
# TODO: should is_infinite become a synonym for `isinf` ???

########################################
#
# arithmetic
#
########################################

# unary minus
Base.:-(::PosInf) = NegInf()
Base.:-(::NegInf) = inf

# binary operations
Base.:+(::IntegerUnion, inf::AnyInf) = inf
Base.:+(inf::AnyInf, ::IntegerUnion) = inf
Base.:+(inf1::AnyInf, inf2::AnyInf) = signbit(inf1) == signbit(inf2) ? inf1 : error("inf - inf is undefined")

Base.:-(inf::AnyInf, ::IntegerUnion) = inf
Base.:-(::IntegerUnion, inf::AnyInf) = -inf
Base.:-(inf1::AnyInf, inf2::AnyInf) = inf1 + (-inf2)
