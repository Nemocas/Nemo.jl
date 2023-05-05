module Native

import ..Nemo: ZZRingElem, VarName, ZZModPolyRingElem, FpPolyRingElem, FqPolyRepField, Zmodn_poly, fpField, FpField, fqPolyRepField, is_prime, is_probable_prime, gen, characteristic

function GF(n::Int; cached::Bool=true)
  (n <= 0) && throw(DomainError(n, "Characteristic must be positive"))
  un = UInt(n)
  !is_prime(un) && throw(DomainError(n, "Characteristic must be prime"))
  return fpField(un, cached)
end

function GF(n::UInt; cached::Bool=true)
  un = UInt(n)
  !is_prime(un) && throw(DomainError(n, "Characteristic must be prime"))
  return fpField(un, cached)
end

function GF(n::ZZRingElem; cached::Bool=true)
  (n <= 0) && throw(DomainError(n, "Characteristic must be positive"))
  !is_probable_prime(n) && throw(DomainError(n, "Characteristic must be prime"))
  return FpField(n, cached)
end

function FiniteField(char::ZZRingElem, deg::Int, s::VarName = :o; cached = true)
  parent_obj = FqPolyRepField(char, deg, Symbol(s), cached)
  return parent_obj, gen(parent_obj)
end

function FiniteField(pol::Union{ZZModPolyRingElem, FpPolyRingElem},
    s::VarName = :o; cached = true, check::Bool=true)
  parent_obj = FqPolyRepField(pol, Symbol(s), cached, check=check)
  return parent_obj, gen(parent_obj)
end

function FiniteField(F::FqPolyRepField, deg::Int, s::VarName = :o; cached = true)
  return FqPolyRepField(characteristic(F), deg, Symbol(s), cached)
end

function FiniteField(char::Int, deg::Int, s::VarName = :o; cached = true)
   parent_obj = fqPolyRepField(ZZRingElem(char), deg, Symbol(s), cached)
   return parent_obj, gen(parent_obj)
end

function FiniteField(pol::Zmodn_poly, s::VarName = :o; cached = true, check::Bool=true)
   parent_obj = fqPolyRepField(pol, Symbol(s), cached, check=check)
   return parent_obj, gen(parent_obj)
end

function FiniteField(F::fqPolyRepField, deg::Int, s::VarName = :o; cached = true)
    return fqPolyRepField(characteristic(F), deg, Symbol(s), cached)
end

# Additional from Hecke
function FiniteField(p::Integer; cached::Bool = true)
  @assert is_prime(p)
  k = GF(p, cached=cached)
  return k, k(1)
end

function FiniteField(p::ZZRingElem; cached::Bool = true)
  @assert is_prime(p)
  k = GF(p, cached=cached)
  return k, k(1)
end

GF(p::Integer, k::Int, s::Union{AbstractString,Symbol}=:o; cached::Bool = true) = FiniteField(p, k, s, cached = cached)[1]

GF(p::ZZRingElem, k::Int, s::Union{AbstractString,Symbol}=:o; cached::Bool = true) = FiniteField(p, k, s, cached = cached)[1]

end # module

using .Native

export Native
