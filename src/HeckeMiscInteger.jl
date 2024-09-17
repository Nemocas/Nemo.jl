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
