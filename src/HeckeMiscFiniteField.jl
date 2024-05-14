function (k::fqPolyRepField)(a::Vector)
  return k(polynomial(Native.GF(Int(characteristic(k))), a))
end

