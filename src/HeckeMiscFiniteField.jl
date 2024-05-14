# As of 14 May 2024:
# This is used in OSCAR, but requires (::fqPolyRepField)(::fpPolyRingElem)
# which is in Hecke
function (k::fqPolyRepField)(a::Vector{fpFieldElem})
  return k(polynomial(Native.GF(Int(characteristic(k))), a))
end
