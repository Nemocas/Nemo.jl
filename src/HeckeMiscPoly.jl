function mulhigh_n(a::ZZPolyRingElem, b::ZZPolyRingElem, n::Int)
  c = parent(a)()
  #careful: as part of the interface, the coeffs 0 - (n-1) are random garbage
  ccall((:fmpz_poly_mulhigh_n, libflint), Nothing, (Ref{ZZPolyRingElem}, Ref{ZZPolyRingElem}, Ref{ZZPolyRingElem}, Cint), c, a, b, n)
  return c
end
function mulhigh(a::PolyRingElem{T}, b::PolyRingElem{T}, n::Int) where {T}
  return mulhigh_n(a, b, degree(a) + degree(b) - n)
end

normalise(f::ZZPolyRingElem, ::Int) = degree(f) + 1
set_length!(f::ZZPolyRingElem, ::Int) = nothing

################################################################################
#
#  Random polynomial
#
################################################################################

@doc raw"""
    Base.rand(Rt::PolyRing{T}, n::Int) where T <: ResElem{ZZRingElem} -> PolyRingElem{T}

Find a random polynomial of degree=$n$.
"""
function Base.rand(Rt::PolyRing{T}, n::Int) where {T<:ResElem{ZZRingElem}}
  f = Rt()
  R = base_ring(Rt)
  for i = 0:n
    setcoeff!(f, i, rand(R))
  end
  return f
end

################################################################################
#
#  Squarefreeness
#
################################################################################

function is_squarefree(f::PolyRingElem{<:FieldElement})
  R = coefficient_ring(f)

  iszero(f) && return false
  degree(f) == 0 && return true

  if !is_monic(f)
    g = divexact(f, leading_coefficient(f))
  else
    g = f
  end

  if characteristic(R) == 0 || R isa FinField
    return is_constant(gcd(g, derivative(g)))
  else
    fac = factor_squarefree(g)
    return all(e <= 1 for (_, e) in fac)
  end
end

function is_squarefree(f::PolyRingElem{<:RingElement})
  iszero(f) && return false
  degree(f) == 0 && return is_squarefree(leading_coefficient(f))::Bool
  fac = factor_squarefree(f)
  return all(e <= 1 for (_, e) in fac)
end
