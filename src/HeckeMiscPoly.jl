function factor_equal_deg(x::fpPolyRingElem, d::Int)
  if degree(x) == d
    return fpPolyRingElem[x]
  end
  fac = gfp_poly_factor(x.mod_n)
  ccall((:nmod_poly_factor_equal_deg, libflint), UInt,
        (Ref{gfp_poly_factor}, Ref{fpPolyRingElem}, Int),
        fac, x, d)
  res = Vector{fpPolyRingElem}(undef, fac.num)
  for i in 1:fac.num
    f = parent(x)()
    ccall((:nmod_poly_factor_get_poly, libflint), Nothing,
          (Ref{fpPolyRingElem}, Ref{gfp_poly_factor}, Int), f, fac, i - 1)
    res[i] = f
  end
  return res
end

function factor_equal_deg(x::FpPolyRingElem, d::Int)
  if degree(x) == d
    return FpPolyRingElem[x]
  end
  fac = gfp_fmpz_poly_factor(base_ring(x))
  ccall((:fmpz_mod_poly_factor_equal_deg, libflint), UInt,
        (Ref{gfp_fmpz_poly_factor}, Ref{FpPolyRingElem}, Int, Ref{fmpz_mod_ctx_struct}),
        fac, x, d, x.parent.base_ring.ninv)
  res = Vector{FpPolyRingElem}(undef, fac.num)
  for i in 1:fac.num
    f = parent(x)()
    ccall((:fmpz_mod_poly_factor_get_fmpz_mod_poly, libflint), Nothing,
          (Ref{FpPolyRingElem}, Ref{gfp_fmpz_poly_factor}, Int, Ref{fmpz_mod_ctx_struct}), f, fac, i - 1, x.parent.base_ring.ninv)
    res[i] = f
  end
  return res
end

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
