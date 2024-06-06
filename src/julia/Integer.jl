function is_prime(x::Integer)
  return is_prime(ZZRingElem(x))
end

function next_prime(x::BigInt, proved::Bool=true)
  return BigInt(next_prime(ZZRingElem(x), proved))
end

function next_prime(x::T, proved::Bool=true) where {T<:Integer}
  return T(next_prime(BigInt(x), proved))
end

function valuation(a::UInt, b::UInt)
  return ccall((:n_remove, libflint), Int, (Ref{UInt}, UInt), a, b)
end

fits(::Type{Int}, a::Int) = true

function fits(::Type{Int}, a::Integer)
  #TODO: possibly not optimal?
  return a % Int == a
end

clog(a::Int, b::Int) = clog(ZZRingElem(a), b)
