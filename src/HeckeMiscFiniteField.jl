function (k::fqPolyRepField)(a::Vector{fpFieldElem})
  return k(polynomial(Native.GF(Int(characteristic(k))), a))
end

function (A::fqPolyRepField)(x::Union{zzModPolyRingElem, fpPolyRingElem})
  u = A()
  set!(u, x)
  return u
end
