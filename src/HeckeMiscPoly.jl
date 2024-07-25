function ZZRingElem(a::EuclideanRingResidueRingElem{ZZRingElem})
  return a.data
end

function ZZRingElem(a::zzModRingElem)
  return ZZRingElem(a.data)
end

function lift(::ZZRing, a::EuclideanRingResidueRingElem{ZZRingElem})
  return a.data
end

function (::ZZRing)(a::EuclideanRingResidueRingElem{ZZRingElem})
  return a.data
end

function lift(::ZZRing, a::zzModRingElem)
  return ZZRingElem(a.data)
end

function (::ZZRing)(a::zzModRingElem)
  return ZZRingElem(a.data)
end

function ZZRingElem(a::ZZModRingElem)
  return a.data
end

function lift(::ZZRing, a::ZZModRingElem)
  return a.data
end

function (::ZZRing)(a::ZZModRingElem)
  return a.data
end

function rem!(z::T, f::T, g::T) where {T<:PolyRingElem}
  z = rem(f, g)
  return z
end

function resultant(f::ZZPolyRingElem, g::ZZPolyRingElem, d::ZZRingElem, nb::Int)
  z = ZZRingElem()
  ccall((:fmpz_poly_resultant_modular_div, libflint), Nothing,
        (Ref{ZZRingElem}, Ref{ZZPolyRingElem}, Ref{ZZPolyRingElem}, Ref{ZZRingElem}, Int),
        z, f, g, d, nb)
  return z
end

function rem!(z::fqPolyRepPolyRingElem, x::fqPolyRepPolyRingElem, y::fqPolyRepPolyRingElem)
  ccall((:fq_nmod_poly_rem, libflint), Nothing, (Ref{fqPolyRepPolyRingElem}, Ref{fqPolyRepPolyRingElem}, Ref{fqPolyRepPolyRingElem}, Ref{fqPolyRepField}),
        z, x, y, base_ring(parent(x)))
  return z
end

function rem!(z::FqPolyRepPolyRingElem, x::FqPolyRepPolyRingElem, y::FqPolyRepPolyRingElem)
  ccall((:fq_poly_rem, libflint), Nothing, (Ref{FqPolyRepPolyRingElem}, Ref{FqPolyRepPolyRingElem}, Ref{FqPolyRepPolyRingElem}, Ref{FqPolyRepPolyRing}),
        z, x, y, parent(x))
  return z
end

function fmpq_poly_to_nmod_poly_raw!(r::zzModPolyRingElem, a::QQPolyRingElem)
  ccall((:fmpq_poly_get_nmod_poly, libflint), Nothing, (Ref{zzModPolyRingElem}, Ref{QQPolyRingElem}), r, a)
end

function fmpq_poly_to_gfp_poly_raw!(r::fpPolyRingElem, a::QQPolyRingElem)
  ccall((:fmpq_poly_get_nmod_poly, libflint), Nothing, (Ref{fpPolyRingElem}, Ref{QQPolyRingElem}), r, a)
end

function fmpq_poly_to_fq_default_poly_raw!(r::FqPolyRingElem, a::QQPolyRingElem, t1::ZZPolyRingElem=ZZPolyRingElem(), t2::ZZRingElem=ZZRingElem())
  ccall((:fmpq_poly_get_numerator, libflint), Nothing, (Ref{ZZPolyRingElem}, Ref{QQPolyRingElem}), t1, a)
  ccall((:fq_default_poly_set_fmpz_poly, libflint), Nothing, (Ref{FqPolyRingElem}, Ref{ZZPolyRingElem}, Ref{FqField}), r, t1, r.parent.base_ring)
  ccall((:fmpq_poly_get_denominator, libflint), Nothing, (Ref{ZZRingElem}, Ref{QQPolyRingElem}), t2, a)
  if !isone(t2)
    #res = ccall((:fmpz_invmod, libflint), Cint, (Ref{ZZRingElem}, Ref{ZZRingElem}, Ref{ZZRingElem}), t2, t2, characteristic(base_ring(r)))
    #res 
    #@assert res != 0
    ccall((:fq_default_poly_scalar_div_fq_default, libflint), Nothing, (Ref{FqPolyRingElem}, Ref{FqPolyRingElem}, Ref{FqFieldElem}, Ref{FqField}), r, r, coefficient_ring(r)(t2), coefficient_ring(r))
  end
end

function fmpq_poly_to_fmpz_mod_poly_raw!(r::ZZModPolyRingElem, a::QQPolyRingElem, t1::ZZPolyRingElem=ZZPolyRingElem(), t2::ZZRingElem=ZZRingElem())
  ccall((:fmpq_poly_get_numerator, libflint), Nothing, (Ref{ZZPolyRingElem}, Ref{QQPolyRingElem}), t1, a)
  ccall((:fmpz_mod_poly_set_fmpz_poly, libflint), Nothing, (Ref{ZZModPolyRingElem}, Ref{ZZPolyRingElem}, Ref{fmpz_mod_ctx_struct}), r, t1, r.parent.base_ring.ninv)
  ccall((:fmpq_poly_get_denominator, libflint), Nothing, (Ref{ZZRingElem}, Ref{QQPolyRingElem}), t2, a)
  if !isone(t2)
    res = ccall((:fmpz_invmod, libflint), Cint, (Ref{ZZRingElem}, Ref{ZZRingElem}, Ref{ZZRingElem}), t2, t2, modulus(base_ring(r)))
    @assert res != 0
    ccall((:fmpz_mod_poly_scalar_mul_fmpz, libflint), Nothing, (Ref{ZZModPolyRingElem}, Ref{ZZModPolyRingElem}, Ref{ZZRingElem}, Ref{fmpz_mod_ctx_struct}), r, r, t2, r.parent.base_ring.ninv)
  end
end

function fmpq_poly_to_gfp_fmpz_poly_raw!(r::FpPolyRingElem, a::QQPolyRingElem, t1::ZZPolyRingElem=ZZPolyRingElem(), t2::ZZRingElem=ZZRingElem())
  ccall((:fmpq_poly_get_numerator, libflint), Nothing, (Ref{ZZPolyRingElem}, Ref{QQPolyRingElem}), t1, a)
  ccall((:fmpz_mod_poly_set_fmpz_poly, libflint), Nothing, (Ref{FpPolyRingElem}, Ref{ZZPolyRingElem}, Ref{fmpz_mod_ctx_struct}), r, t1, r.parent.base_ring.ninv)
  ccall((:fmpq_poly_get_denominator, libflint), Nothing, (Ref{ZZRingElem}, Ref{QQPolyRingElem}), t2, a)
  if !isone(t2)
    res = ccall((:fmpz_invmod, libflint), Cint, (Ref{ZZRingElem}, Ref{ZZRingElem}, Ref{ZZRingElem}), t2, t2, modulus(base_ring(r)))
    @assert res != 0
    ccall((:fmpz_mod_poly_scalar_mul_fmpz, libflint), Nothing, (Ref{FpPolyRingElem}, Ref{FpPolyRingElem}, Ref{ZZRingElem}, Ref{fmpz_mod_ctx_struct}), r, r, t2, r.parent.base_ring.ninv)
  end
end

function fmpq_poly_to_nmod_poly(Rx::zzModPolyRing, f::QQPolyRingElem)
  g = Rx()
  fmpq_poly_to_nmod_poly_raw!(g, f)
  return g
end

function fmpq_poly_to_gfp_poly(Rx::fpPolyRing, f::QQPolyRingElem)
  g = Rx()
  fmpq_poly_to_gfp_poly_raw!(g, f)
  return g
end

function fmpz_poly_to_nmod_poly_raw!(r::zzModPolyRingElem, a::ZZPolyRingElem)
  ccall((:fmpz_poly_get_nmod_poly, libflint), Nothing,
        (Ref{zzModPolyRingElem}, Ref{ZZPolyRingElem}), r, a)

end

function fmpz_poly_to_gfp_poly_raw!(r::fpPolyRingElem, a::ZZPolyRingElem)
  ccall((:fmpz_poly_get_nmod_poly, libflint), Nothing,
        (Ref{fpPolyRingElem}, Ref{ZZPolyRingElem}), r, a)

end

function fmpz_poly_to_nmod_poly(Rx::zzModPolyRing, f::ZZPolyRingElem)
  g = Rx()
  fmpz_poly_to_nmod_poly_raw!(g, f)
  return g
end

function fmpq_poly_to_fmpz_mod_poly(Rx::ZZModPolyRing, f::QQPolyRingElem)
  g = Rx()
  fmpq_poly_to_fmpz_mod_poly_raw!(g, f)
  return g
end

function fmpq_poly_to_gfp_fmpz_poly(Rx::FpPolyRing, f::QQPolyRingElem)
  g = Rx()
  fmpq_poly_to_gfp_fmpz_poly_raw!(g, f)
  return g
end

function fmpq_poly_to_fq_default_poly(Rx::FqPolyRing, f::QQPolyRingElem)
  g = Rx()
  fmpq_poly_to_fq_default_poly_raw!(g, f)
  return g
end

function fmpz_poly_to_fmpz_mod_poly_raw!(r::ZZModPolyRingElem, a::ZZPolyRingElem)
  ccall((:fmpz_poly_get_fmpz_mod_poly, libflint), Nothing,
        (Ref{ZZModPolyRingElem}, Ref{ZZPolyRingElem}, Ref{fmpz_mod_ctx_struct}), r, a, r.parent.base_ring.ninv)

end

function fmpz_poly_to_fmpz_mod_poly(Rx::ZZModPolyRing, f::ZZPolyRingElem)
  g = Rx()
  fmpz_poly_to_fmpz_mod_poly_raw!(g, f)
  return g
end

modulus(F::EuclideanRingResidueRing{ZZRingElem}) = F.modulus

modulus(F::EuclideanRingResidueField{ZZRingElem}) = F.modulus

################################################################################
#
#  QQPolyRingElem with denominator 1 to ZZPolyRingElem
#
################################################################################

function (a::ZZPolyRing)(b::QQPolyRingElem)
  (!isone(denominator(b))) && error("Denominator has to be 1")
  z = a()
  ccall((:fmpq_poly_get_numerator, libflint), Nothing,
        (Ref{ZZPolyRingElem}, Ref{QQPolyRingElem}), z, b)
  return z
end

##############################################################
# all of this should be in Nemo/AbstractAlgebra
#
#TODO:
# expand systematically for all finite fields
# and for ZZRingElem/QQFieldElem poly
# for fun: is_power(a::AbsSimpleNumFieldElem)
#

function factor(R::T, f::QQPolyRingElem) where {T<:Union{fqPolyRepField,fpField}}
  Rt, t = polynomial_ring(R, "t", cached=false)
  return factor(Rt(f))
end

function roots(R::T, f::QQPolyRingElem) where {T<:Union{fqPolyRepField,fpField}}
  Rt, t = polynomial_ring(R, "t", cached=false)
  fp = polynomial_ring(ZZ, cached=false)[1](f * denominator(f))
  fpp = Rt(fp)
  return roots(fpp)
end

function roots(K::fqPolyRepField, f::fpPolyRingElem)
  @assert characteristic(K) == characteristic(base_ring(f))
  Kx = polynomial_ring(K, cached=false)[1]
  coeffsff = Vector{elem_type(K)}(undef, degree(f) + 1)
  for i = 0:degree(f)
    coeffsff[i+1] = K(lift(coeff(f, i)))
  end
  ff = Kx(coeffsff)
  return roots(ff)
end

function roots(K::FqPolyRepField, f::FpPolyRingElem)
  @assert characteristic(K) == characteristic(base_ring(f))
  Kx = polynomial_ring(K, cached=false)[1]
  coeffsff = Vector{FqPolyRepFieldElem}(undef, degree(f) + 1)
  for i = 0:degree(f)
    coeffsff[i+1] = K(lift(coeff(f, i)))
  end
  ff = Kx(coeffsff)
  return roots(ff)
end

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

function roots(f::Union{fqPolyRepPolyRingElem,FqPolyRepPolyRingElem}) # should be in Nemo and
  # made available for all finite fields I guess.
  q = size(base_ring(f))
  x = gen(parent(f))
  if degree(f) < q
    x = powermod(x, q, f) - x
  else
    x = x^Int(q) - x
  end
  f = gcd(f, x)
  l = factor(f).fac
  return elem_type(base_ring(f))[-divexact(constant_coefficient(x), leading_coefficient(x)) for x = keys(l) if degree(x) == 1]
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
