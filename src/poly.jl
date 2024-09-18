function rem!(z::T, f::T, g::T) where {T<:PolyRingElem}
  z = rem(f, g)
  return z
end
