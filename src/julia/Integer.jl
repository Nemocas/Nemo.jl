function is_prime(x::Integer)
  return is_prime(ZZRingElem(x))
end

function next_prime(x::BigInt, proved::Bool=true)
  return BigInt(next_prime(ZZRingElem(x), proved))
end

function next_prime(x::T, proved::Bool=true) where {T<:Integer}
  return T(next_prime(BigInt(x), proved))
end

@inline function valuation(a::UInt, b::UInt)
  b <= 1 && error("Factor <= 1")
  return _remove(a, b)[1]
end

# (v, q) with a = b^v * q and b not dividing q, for b >= 2; for a == 0, v is 64 if b == 2
# and 0 otherwise, as with flint's n_remove
@inline function _remove(a::UInt, b::UInt)
  if b == 2
    v = trailing_zeros(a)
    return (v, a >> v)
  end

  a < b && return (0, a)
  q, r = divrem(a, b)
  iszero(r) || return (0, a)
  return _remove_divisible(q, b)
end

# _remove(a * b, b) for b > 2, given the quotient a: divide by b^2, b^4, ... while they
# divide, then try the same powers in reverse, as flint's n_remove does. 6 powers suffice
# since 3^(2^6 - 1) > 2^64. A power >= 2^32 is followed by typemax(UInt): its square
# would wrap, and the quotient after dividing by it is below 2^32, ending the ascent.
@noinline function _remove_divisible(a::UInt, b::UInt)::Tuple{Int, UInt}
  pows = (b, b, b, b, b, b)
  pows = Base.setindex(pows, b <= typemax(UInt32) ? b * b : typemax(UInt), 2)
  i = 1
  while i < 6
    p = pows[i + 1]
    a < p && break
    q, r = divrem(a, p)
    iszero(r) || break
    a = q
    i += 1
    i < 6 && (pows = Base.setindex(pows, p <= typemax(UInt32) ? p * p : typemax(UInt), i + 1))
  end

  v = (1 << i) - 1
  while i > 0
    i -= 1
    p = pows[i + 1]
    a < p && continue
    q, r = divrem(a, p)
    if iszero(r)
      v += 1 << i
      a = q
    end
  end
  return (v, a)
end

function fits(::Type{T}, a::Integer) where {T <: Integer}
  return typemin(T) <= a <= typemax(T)
end

clog(a::Int, b::Int) = clog(ZZRingElem(a), b)
