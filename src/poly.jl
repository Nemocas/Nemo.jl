_pretty_sort(D::Dict) = sort!(collect(D); by = x -> pretty_sort(x[1]))

_pretty_sort!(D::Vector) = sort!(D; by = x -> pretty_sort(x[1]))


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
             _pretty_sort!(Tuple{typeof(f),Int}[facs[i] => es[i] for i in 1:length(es)]))
end

# This is Musser's algorithm
function _factor_squarefree_char_0(f::PolyRingElem)
  @assert iszero(characteristic(base_ring(f)))
  res = Dict{typeof(f),Int}()
  if is_constant(f)
    return Fac(f, _pretty_sort(res))
  end
  c = leading_coefficient(f)
  f = divexact(f, c)
  di = gcd(f, derivative(f))
  if isone(di)
    res[f] = 1
    return Fac(parent(f)(c), _pretty_sort(res))
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
  return Fac(parent(f)(c), _pretty_sort(res))
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

###############################################################################
#
# Partial Fractions
#
###############################################################################

#for fmpz one should possibly use fdivrem to hopefully get the symmetric
#remainder, however that does not exist for Integer (and general euc)
#the assertion needs to be changed to use the euc function - but we don't
#have that generically
function _step1(n::T, a::T, b::T) where T 
  #needs to be euc....
  #decom for n//(ab) = f/a + e/b and a, b coprime, f<a, e<b in the euc sense
  g, e, f = gcdx(a, b)
#  @assert is_one(g)
  e *= n
  f *= n
#  @assert n == e*a + f*b
  q, e = divrem(e, b)
  f += q*a
#  @assert n == e*a + f*b
#  @assert is_zero(e) || degree(e) < degree(b)
#  @assert is_zero(f) || degree(f) < degree(a)
#  @assert n//(a*b) == f//a + e//b

  return f, e
end

@doc raw"""
    digits(n::T; base::T=gen(parent(n)), pad::Integer = 1) where T <: PolyRingElem{<:FieldElem}

Write $n = sum a_i base^i$ with `degree(a_i) < degree(base)`. Returns
the vector of `a_i`, starting with `a_0`. If `pad` is given, the vector
will have at least length `pad`, the tail being filled with 0.
"""
function Base.digits(n::T; base::T=gen(parent(n)), pad::Integer = 1) where T <: PolyRingElem{<:FieldElem}
  @assert parent(n) == parent(base)
  @assert degree(base) > 0
  return digits!([zero(n) for _ in 1:max(floor(Int, 1+degree(n)/degree(base)), pad)], n; base)
end

function Base.digits!(a::AbstractVector{T}, n::T; base::T) where T <: PolyRingElem{<:FieldElem}
  #essentially identical to the digits for ZZRingElem
  nd = degree(n)/degree(base)
  if nd > 10 #TODO: find a sensible cross over point
    po = ceil(Int, nd/2)
    q, r = divrem(n, base^po)
    digits!(view(a, 1:po), r; base)
    digits!(view(a, po+1:length(a)), q; base)
  else
    for i in eachindex(a)
      n, r = Base.divrem(n, base)
      a[i] = r
    end
  end
  return a
end

@doc raw"""
    partial_fractions(n::T, d::T) where T <: PolyRingElem{<:FieldElem}

Compute the partial fractions decomposition of n//d. Returns a vector
of rational functions with 
 - denominator 1 for the first entry begin the quotient
 - the denominators being powers of the irreducible factors of d

The decomposition is unique.

# Examples
```jldoctest
julia> Qx, x = QQ[:x];

julia> f = 3*x^4+1;

julia> g = (x+1)*(x-1)^2;

julia> partial_fractions(f, g)
4-element Vector{AbstractAlgebra.Generic.FracFieldElem{QQPolyRingElem}}:
 3*x + 3
 5//(x - 1)
 2//(x^2 - 2*x + 1)
 1//(x + 1)

julia> sum(ans) == f//g
true

```
"""
function partial_fractions(n::T, d::T) where T <: Union{Nemo.IntegerUnion, <:PolyRingElem{<:FieldElem}}
  n1, n = divrem(n, d)
  ld = collect(factor(d))
  res = [n1//one(parent(n))]
  #TODO: use a product tree to make this asymptotically fast - if anyone cares
  #here we're suboptimal: split d= prod p_i^n_i into
  # p_1^n_1 and the rest
  #then the rest into p_2^n_2 and the rest ...
  for i=1:length(ld)
    pp = ld[i][1]^ld[i][2]
    dd = divexact(d, pp)
    a, _n = _step1(n, pp, dd)
    n//(pp*dd) == a//pp + _n//dd
    n = _n

    b = digits(a; base = ld[i][1], pad = ld[i][2])
    @assert length(b) == ld[i][2] #can fail for integers if the "wrong" divrem
                                  #is used.
    pp = ld[i][1]
    for j=length(b):-1:1
      push!(res, b[j]//pp)
      pp *= ld[i][1]
    end
    d = dd
  end
  return res
end

@doc raw"""
    partial_fractions(a::Generic.FracFieldElem{<:PolyRingElem{<:FieldElem}})

Compute the partial fractions decomposition of `a=n//d`. 
"""
function partial_fractions(a::Generic.FracFieldElem{<:PolyRingElem{<:FieldElem}})
  return partial_fractions(numerator(a), denominator(a))
end  

