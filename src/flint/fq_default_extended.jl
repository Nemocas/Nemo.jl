export absolute_degree, absolute_norm, absolute_tr, absolute_frobenius

################################################################################
#
#  Additional predicate
#
################################################################################

is_absolute(F::FqDefaultFiniteField) = F.isabsolute

################################################################################
#
#  Base field
#
################################################################################

function base_field(F::FqDefaultFiniteField)
  # if it is relative, then the base_field will be set
  # otherwise, it is the prime field
  if !isdefined(F, :base_field)
    F.base_field = prime_field(F)
  end

  return F.base_field::FqDefaultFiniteField
end

################################################################################
#
#  Prime field
#
################################################################################

# Should be cached on the field
function prime_field(F::FqDefaultFiniteField)
  # We want them to be equal among all finite fields
  return FqDefaultFiniteField(characteristic(F), 1, Symbol("#"), true)
end

################################################################################
#
#  Internal coercion into base/prime field
#
################################################################################

# Need this for the trace and norm
# should be made faster
function _coerce_to_base_field(a::fq_default)
  L = parent(a)
  K = base_field(L)
  if is_absolute(L)
    return K(coeff(lift(ZZ["x"][1], a), 0))
  else
    return L.preimage_basefield(a)
  end
end

function _coerce_to_prime_field(a::fq_default)
  L = parent(a)
  K = prime_field(L)
  return K(coeff(lift(ZZ["x"][1], a), 0))
end

function defining_polynomial(L::FqDefaultFiniteField)
  if !isdefined(L, :defining_poly)
    @assert is_absolute(L)
    F = PolynomialRing(prime_field(L), "x", cached = false)[1]
    L.defining_poly = F(map(lift, collect(coefficients(modulus(L)))))
  end
  return L.defining_poly::fq_default_poly
end

################################################################################
#
#  Degree
#
################################################################################

function degree(a::FqDefaultFiniteField)
  if is_absolute(a)
    return _degree(a)
  else
    return degree(a.defining_poly)
  end
end

function absolute_degree(F::FqDefaultFiniteField)
  if is_absolute(F)
    return _degree(F)
  else
    return absolute_degree(base_field(F)) * degree(defining_polynomial(F))
  end
end

################################################################################
#
#  Algebra generator
#
################################################################################

# should not be cached (for in place stuff etc)
function gen(L::FqDefaultFiniteField)
  if is_absolute(L)
    return _gen(L)
  else
    L.forwardmap(gen(parent(defining_polynomial(L))))
  end
end

function is_gen(a::fq_default)
  L = parent(a)
  if is_absolute(L)
    return _is_gen(a)
  else
    return a == L.forwardmap(gen(parent(defining_polynomial(L))))
  end
end

################################################################################
#
#  Write element as polynomial
#
################################################################################

# assumes that we are not absolute, but we are not checking this
function _as_poly(a::fq_default)
  return parent(a).backwardmap(a)
end

################################################################################
#
#  Coeff
#
################################################################################

@doc Markdown.doc"""
    coeff(x::fq_default, n::Int)

Return the degree $n$ coefficient of the polynomial representing the given
finite field element.
"""
function coeff(x::fq_default, n::Int)
   if is_absolute(parent(x))
     return _coeff(x, n)
   end
   return coeff(_as_poly(x), n)
end

################################################################################
#
#  Frobenius
#
################################################################################

# adjust docstring
@doc Markdown.doc"""
    frobenius(x::fq_default, n = 1)

Return the iterated Frobenius $\sigma_p^n(x)$ where $\sigma_p$ is the
Frobenius map sending the element $a$ to $a^p$ in the finite field of
characteristic $p$. By default the Frobenius map is applied $n = 1$ times if
$n$ is not specified.
"""
function frobenius(x::fq_default, n = 1)
   # we want x -> x^#base_field
   z = parent(x)()
   if is_absolute(parent(x))
     m = n
   else
     m = n * absolute_degree(base_field(parent(x)))
   end
   return _frobenius(x, m)
end

function absolute_frobenius(x::fq_default, n = 1)
  return _frobenius(x, n)
end

################################################################################
#
#  Basis
#
################################################################################

# this is new
function basis(F::FqDefaultFiniteField)
  return powers(gen(F), degree(F) - 1)
end

# cache?
function absolute_basis(F::FqDefaultFiniteField)
  if is_absolute(F)
    return basis(F)
  else
    res = elem_type(F)[]
    kabs = absolute_basis(base_field(F))
    for b in basis(F)
      for bb in kabs
        push!(res, F(bb) * b)
      end
    end
    return res
  end
end

################################################################################
#
#  Minimal polynomial
#
################################################################################

function minpoly(a::fq_default)
  return minpoly(PolynomialRing(base_field(parent(a)), "x", cached = false)[1], a)
end

function minpoly(Rx::FqDefaultPolyRing, a::fq_default)
  @assert base_ring(Rx) === base_field(parent(a))
  c = [a]
  fa = frobenius(a)
  while !(fa in c)
    push!(c, fa)
    fa = frobenius(fa)
  end
  St = PolynomialRing(parent(a), "x", cached = false)[1]
  f = prod([gen(St) - x for x = c], init = one(St))
  g = Rx()
  for i = 0:degree(f)
    setcoeff!(g, i, _coerce_to_base_field(coeff(f, i)))
  end
  return g
end

function absolute_minpoly(a::fq_default)
  return absolute_minpoly(PolynomialRing(prime_field(parent(a)), "x", cached = false)[1], a)
end

function absolute_minpoly(Rx::FqDefaultPolyRing, a::fq_default)
  @assert base_ring(Rx) === prime_field(parent(a))
  c = [a]
  fa = absolute_frobenius(a)
  while !(fa in c)
    push!(c, fa)
    fa = absolute_frobenius(fa)
  end
  St = PolynomialRing(parent(a), "x", cached = false)[1]
  f = prod([gen(St) - x for x = c], init = one(St))
  g = Rx()
  for i = 0:degree(f)
    setcoeff!(g, i, _coerce_to_prime_field(coeff(f, i)))
  end
  return g
end

################################################################################
#
#  Characteristic polynomial
#
################################################################################

function charpoly(a::fq_default)
  return charpoly(PolynomialRing(base_field(parent(a)), "x", cached = false)[1], a)
end

function charpoly(Rx::FqDefaultPolyRing, a::fq_default)
  f = minpoly(Rx, a)
  d = divexact(degree(parent(a)), degree(f))
  return f^d
end

function absolute_charpoly(a::fq_default)
  return absolute_charpoly(PolynomialRing(prime_field(parent(a)), "x", cached = false)[1], a)
end

function absolute_charpoly(Rx::FqDefaultPolyRing, a::fq_default)
  f = absolute_minpoly(Rx, a)
  d = divexact(absolute_degree(parent(a)), degree(f))
  return f^d
end

################################################################################
#
#  Norm
#
################################################################################

# Should probably use resultant, but _as_poly is not that fast at the moment?
function norm(a::fq_default)
  if is_absolute(parent(a))
    return base_field(parent(a))(_norm(a))
  end
  d = degree(parent(a))
  f = charpoly(a)
  return isodd(d) ? -constant_coefficient(f) : constant_coefficient(f)
end

function absolute_norm(a::fq_default)
  return prime_field(parent(a))(_norm(a))
end

function tr(a::fq_default)
  if is_absolute(parent(a))
    return base_field(parent(a))(_tr(a))
  end
  d = degree(parent(a))
  f = charpoly(a)
  return -coeff(f, d - 1)
end

function absolute_tr(a::fq_default)
  return prime_field(parent(a))(_tr(a))
end

################################################################################
#
#  Embedding helper
#
################################################################################

# I just need one embedding which I fix one and for all
# This is used to embed K into L \cong K[x]/(f)
# This should be improved
function _embed(K::FqDefaultFiniteField, L::FqDefaultFiniteField)
  if absolute_degree(K) == 1
    return x -> begin
      y = L(coeff(lift(ZZ["x"][1], x), 0))
    end
  else
    # must be absolute minpoly
    g = absolute_minpoly(_gen(K))
    e = _embed(prime_field(K), L)
    a = roots(map_coefficients(e, g))[1]
    return x -> begin
      return sum(_coeff(x, i)*a^i for i in 0:(absolute_degree(K) - 1))
    end
  end
end

################################################################################
#
#  Print the internal presentation for debugging purposes
#
################################################################################

struct _fq_default_dummy
  a
end

function expressify(a::_fq_default_dummy; context = nothing)
   x = a.a.parent.var
   d = _degree(a.a.parent)

   sum = Expr(:call, :+)
   for k in (d - 1):-1:0
        c = _coeff(a.a, k)
        if !iszero(c)
            xk = k < 1 ? 1 : k == 1 ? x : Expr(:call, :^, x, k)
            if isone(c)
                push!(sum.args, Expr(:call, :*, xk))
            else
                push!(sum.args, Expr(:call, :*, expressify(c, context = context), xk))
            end
        end
    end
    return sum
end

show_raw(io::IO, a::fq_default) =
  println(io, AbstractAlgebra.obj_to_string(_fq_default_dummy(a), context = io))

show_raw(a::fq_default) = show_raw(stdout, a)

################################################################################
#
#  Constructor for relative extensions
#
################################################################################

function FqDefaultFiniteField(f::fq_default_poly, s::Symbol, cached::Bool = false)
  K = base_ring(f)
  p = characteristic(K)
  d = absolute_degree(K) * degree(f)
  L = FqDefaultFiniteField(p, d, s, cached)
  L.isabsolute = false
  L.defining_poly = f
  L.base_field = K
  # We also need to determine the map K[x]/(f) -> L
  # First embed K into L
  e = _embed(K, L)
  # Push f to L
  foverL = map_coefficients(e, f)
  a = roots(foverL)[1]
  # Found the map K[x]/(f) -> L
  forwardmap = y -> evaluate(map_coefficients(e, y), a)
  Kabs = absolute_basis(K)
  Fp = prime_field(K)
  # We have no natural coercion Fp -> K
  eabs = _embed(Fp, K)
  # Determine inverse of forwardmap using linear algebra
  # First determine the matrix representing forwardmap
  forwardmat = zero_matrix(Fp, d, d)
  l = 1
  x = gen(parent(f))
  xi = powers(x, degree(f) - 1)
  for i in 0:(degree(f) - 1)
    for b in Kabs
      v = forwardmap(b * xi[i + 1])
      for j in 1:_degree(L)
        forwardmat[l, j] = _coeff(v, j - 1)
      end
      l += 1
    end
  end
  forwardmatinv = inv(forwardmat)
  backwardmap = y -> begin
    w = matrix(Fp, 1, d, [_coeff(y, j - 1) for j in 1:d])
    ww = [Fp(_coeff(y, j - 1)) for j in 1:d]
    _abs_gen_rel = zero(parent(f))
    fl, vv = can_solve_with_solution(forwardmat, w, side = :left)
    vvv = ww * forwardmatinv
    @assert fl
    l = 1
    for i in 0:(degree(f) - 1)
      for b in Kabs
        _abs_gen_rel += eabs(vv[1, l]) * b * xi[i + 1]
        l += 1
      end
    end
    return _abs_gen_rel
  end
  backwardmap_basefield = y -> begin
    w = matrix(Fp, 1, d, [_coeff(y, j - 1) for j in 1:d])
    fl, vv = can_solve_with_solution(forwardmat, w, side = :left)
    @assert fl
    @assert all(iszero, (vv[1, i] for i in (absolute_degree(K) + 1):d))
    return sum(eabs(vv[1, i]) * Kabs[i] for i in 1:absolute_degree(K))
  end

  u = rand(K)
  @assert backwardmap_basefield(forwardmap(u*x^0)) == u

  L.forwardmap = forwardmap
  L.backwardmap = backwardmap
  L.image_basefield = e
  L.preimage_basefield = backwardmap_basefield

  return L
end

function NGFiniteField(f::fq_default_poly, s::AbstractString; cached::Bool = false, check::Bool = true)
  (check && isirreducible(f)) || error("Defining polynomial must be irreducible")
  # Should probably have its own cache
  F = FqDefaultFiniteField(f, Symbol(s), cached)
  return F, gen(F)
end

################################################################################
#
#  Fancy coercion
#
################################################################################

function (a::FqDefaultFiniteField)(b::fq_default)
   if parent(b) === a
     return b
   end

   if is_absolute(a)
      parent(b) != a && error("Coercion between finite fields not implemented")
   end

   if parent(b) === base_field(a)
     return (a.image_basefield)(b)
   end

   # To make it work in towers
   return a(base_field(a)(b))
end
