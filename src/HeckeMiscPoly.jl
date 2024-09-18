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
