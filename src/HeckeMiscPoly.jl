##############################################################
# all of this should be in Nemo/AbstractAlgebra
#
function is_power(a::Union{fpFieldElem, FpFieldElem, fqPolyRepFieldElem, FqPolyRepFieldElem, FqFieldElem}, m::Int)
  if iszero(a)
    return true, a
  end
  s = order(parent(a))
  if gcd(s - 1, m) == 1
    return true, a^invmod(ZZ(m), s - 1)
  end
  St, t = polynomial_ring(parent(a), "t", cached=false)
  f = t^m - a
  rt = roots(f)
  if length(rt) > 0
    return true, rt[1]
  else
    return false, a
  end
end

function setcoeff!(z::fqPolyRepPolyRingElem, n::Int, x::ZZRingElem)
  ccall((:fq_nmod_poly_set_coeff_fmpz, libflint), Nothing,
        (Ref{fqPolyRepPolyRingElem}, Int, Ref{ZZRingElem}, Ref{fqPolyRepField}),
        z, n, x, base_ring(parent(z)))
  return z
end

###############################################################################
#
#  Sturm sequence
#
###############################################################################

function _divide_by_content(f::ZZPolyRingElem)
  p = primpart(f)
  if sign(leading_coefficient(f)) == sign(leading_coefficient(p))
    return p
  else
    return -p
  end
end

function sturm_sequence(f::ZZPolyRingElem)
  g = f
  h = _divide_by_content(derivative(g))
  seq = ZZPolyRingElem[g, h]
  while true
    r = _divide_by_content(pseudorem(g, h))
    # r has the same sign as pseudorem(g, h)
    # To get a pseudo remainder sequence for the Sturm sequence,
    # we need r to be the pseudo remainder of |lc(b)|^(a - b + 1),
    # so we need some adjustment. See
    # https://en.wikipedia.org/wiki/Polynomial_greatest_common_divisor#Sturm_sequence_with_pseudo-remainders
    if leading_coefficient(h) < 0 && isodd(degree(g) - degree(h) + 1)
      r = -r
    end
    if r != 0
      push!(seq, -r)
      g, h = h, -r
    else
      break
    end
  end
  return seq
end

################################################################################
#
#  Squarefree factorization in characteristic 0
#
################################################################################

# TODO: Implement the things from
# "Square-Free Algorithms in Positive Characteristic" by Gianni--Trager
# This should avoid the full factorization for function fields
# (and/or finitely generated fields in general?!)

function factor_squarefree(f::PolyRingElem{<:FieldElement})
  R = coefficient_ring(f)
  if iszero(characteristic(R))
    return _factor_squarefree_char_0(f)
  else
    fac = factor(f)
    es = unique!([e for (p, e) in fac])
    facs = Vector{typeof(f)}(undef, length(es))
    for i in 1:length(facs)
      facs[i] = one(parent(f))
    end
    for (p, e) in fac
      i = findfirst(isequal(e), es)
      facs[i] *= p
    end
  end
  return Fac(unit(fac),
             Dict{typeof(f),Int}(facs[i] => es[i] for i in 1:length(es)))
end


# This is Musser's algorithm
function _factor_squarefree_char_0(f::PolyRingElem)
  @assert iszero(characteristic(base_ring(f)))
  res = Dict{typeof(f),Int}()
  if is_constant(f)
    return Fac(f, res)
  end
  c = leading_coefficient(f)
  f = divexact(f, c)
  di = gcd(f, derivative(f))
  if isone(di)
    res[f] = 1
    return Fac(parent(f)(c), res)
  end
  ei = divexact(f, di)
  i = 1
  while !is_constant(ei)
    eii = gcd(di, ei)
    dii = divexact(di, eii)
    if degree(eii) != degree(ei)
      res[divexact(ei, eii)] = i
    end
    i = i + 1
    di = dii
    ei = eii
  end
  return Fac(parent(f)(c), res)
end

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
