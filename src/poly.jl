function rem!(z::T, f::T, g::T) where {T<:PolyRingElem}
  z = rem(f, g)
  return z
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

################################################################################
#
#  Mulhigh
#
################################################################################

function mulhigh(a::PolyRingElem{T}, b::PolyRingElem{T}, n::Int) where {T}
  return mulhigh_n(a, b, degree(a) + degree(b) - n)
end
