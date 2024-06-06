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
