function round(::Type{Int}, a::QQFieldElem)
  return round(Int, Rational{BigInt}(a))
end

function prime_field(_::NumField)
  return QQField()
end

function prime_field(F::fqPolyRepField; cached::Bool=true)
  return Native.GF(Int(characteristic(F)), cached=cached)
end

function prime_field(F::FqPolyRepField; cached::Bool=true)
  return Native.GF(characteristic(F), cached=cached)
end

function prime_field(F::T; cached::Bool=true) where {T<:Union{fpField,FpField}}
  return F
end

function evaluate(f::ZZPolyRingElem, r::fqPolyRepFieldElem)
  #Horner - stolen from Claus

  if length(f) == 0
    return parent(r)()
  end

  l = f.length - 1
  s = parent(r)(coeff(f, l))
  for i = l-1:-1:0
    s = s * r + parent(r)(coeff(f, i))
  end
  return s
end

function evaluate!(z::fqPolyRepFieldElem, f::ZZPolyRingElem, r::fqPolyRepFieldElem)
  #Horner - stolen from Claus

  zero!(z)

  if length(f) == 0
    return z
  end

  l = f.length - 1
  set!(z, parent(r)(coeff(f, l)))
  #s = parent(r)(coeff(f, l))
  for i = l-1:-1:0
    mul!(z, z, r)
    add!(z, z, parent(r)(coeff(f, i)))
    #s = s*r + parent(r)(coeff(f, i))
  end
  return z
end

function real(tau::AcbMatrix)
  return map(real, tau)
end

function imag(tau::AcbMatrix)
  return map(imag, tau)
end

*(x::AcbFieldElem, y::ArbMatrix) = x * _acb_mat(y)
*(x::ArbMatrix, y::AcbFieldElem) = y * x
*(x::ArbMatrix, y::AcbMatrix) = _acb_mat(x) * y
*(x::AcbMatrix, y::ArbMatrix) = x * _acb_mat(y)
+(x::ArbMatrix, y::AcbMatrix) = _acb_mat(x) + y
+(x::AcbMatrix, y::ArbMatrix) = y + x
-(x::ArbMatrix, y::AcbMatrix) = x + (-y)
-(x::AcbMatrix, y::ArbMatrix) = x + (-y)
//(x::ArbMatrix, y::ArbFieldElem) = map(t -> t // y, x)


function _acb_mat(A::ArbMatrix)
  p = precision(base_ring(A))
  Cc = AcbField(p)
  return change_base_ring(Cc, A)
end

function mul!(z::AcbFieldElem, x::AcbFieldElem, y::ArbFieldElem)
  @ccall libflint.acb_mul_arb(z::Ref{AcbFieldElem}, x::Ref{AcbFieldElem}, y::Ref{ArbFieldElem}, parent(z).prec::Int)::Nothing
  return z
end

@doc raw"""
    valuation(G::QQMatrix, p)

Return the minimum valuation of the entries of `G`.
"""
function valuation(G::QQMatrix, p)
  return minimum(x == 0 ? inf : valuation(x, p) for x in G)
end

function roots(f::ZZModPolyRingElem, p::ZZRingElem, e::Int)
  F = Fac{ZZRingElem}()
  F.unit = one(ZZRingElem)
  F[p] = e
  return roots(f, F)
end
function roots(f::ZZModPolyRingElem, fac::Fac{ZZRingElem})
  res = fmpz_mod_poly_factor(base_ring(f))
  _fac = fmpz_factor()
  for (p, e) in fac
    @ccall libflint._fmpz_factor_append(_fac::Ref{fmpz_factor}, p::Ref{ZZRingElem}, UInt(e)::UInt)::Nothing
  end
  @ccall libflint.fmpz_mod_poly_roots_factored(res::Ref{fmpz_mod_poly_factor}, f::Ref{ZZModPolyRingElem}, 1::Cint, _fac::Ref{fmpz_factor}, base_ring(f).ninv::Ref{fmpz_mod_ctx_struct})::Nothing
  _res = Tuple{ZZModRingElem,Int}[]
  for i in 1:res.num
    g = parent(f)()
    @ccall libflint.fmpz_mod_poly_factor_get_fmpz_mod_poly(g::Ref{ZZModPolyRingElem}, res::Ref{fmpz_mod_poly_factor}, (i - 1)::Int, base_ring(f).ninv::Ref{fmpz_mod_ctx_struct})::Nothing
    e = unsafe_load(res.exp, i)
    push!(_res, (-coeff(g, 0), e))
  end
  return _res
end

ZZMatrix(M::Matrix{Int}) = matrix(ZZ, M)

order(::ZZRingElem) = ZZ

function sub!(z::Vector{QQFieldElem}, x::Vector{QQFieldElem}, y::Vector{ZZRingElem})
  for i in 1:length(z)
    sub!(z[i], x[i], y[i])
  end
  return z
end

function (Zx::ZZPolyRing)(a::AbsSimpleNumFieldElem)
  b = Zx()
  @assert denominator(a) == 1
  if degree(parent(a)) == 1
    # If the number field is linear, then a.elem_length is not properly
    # initialized, that is, it could be anything.
    setcoeff!(b, 0, numerator(coeff(a, 0)))
  elseif degree(parent(a)) == 2
    # ... or quadratic, then a.elem_length is not properly
    # initialized, that is, it could be anything.
    setcoeff!(b, 0, numerator(coeff(a, 0)))
    setcoeff!(b, 1, numerator(coeff(a, 1)))
  else
    for i = 0:a.elem_length
      setcoeff!(b, i, numerator(coeff(a, i)))
    end
  end
  return b
end

function Base.round(::Type{ZZRingElem}, x::ArbFieldElem)
  if radius(x) > 1e-1
    throw(InexactError(:round, ZZRingElem, x))
  end
  return setprecision(BigFloat, precision(parent(x))) do
    round(ZZRingElem, BigFloat(x))
  end
end

function Base.round(::Type{ZZMatrix}, C::ArbMatrix)
  v = zero_matrix(ZZ, nrows(C), ncols(C))

  for i = 1:nrows(C)
    for j = 1:ncols(C)
      v[i, j] = round(ZZRingElem, C[i, j])
    end
  end
  return v
end

function discriminant(K::QQField)
  return one(K)
end

gen(Q::QQField) = one(Q)

real(x::QQFieldElem) = x

norm(x::ZZRingElem) = abs(x)

number_field(::ZZRing) = QQ

function Base.hash(f::zzModMPolyRingElem, h::UInt)
  return h # TODO: enhance or throw error
end

function tdivpow2!(B::ZZMatrix, t::Int)
  @ccall libflint.fmpz_mat_scalar_tdiv_q_2exp(B::Ref{ZZMatrix}, B::Ref{ZZMatrix}, t::Cint)::Nothing
end

function tdivpow2(B::ZZMatrix, t::Int)
  C = similar(B)
  @ccall libflint.fmpz_mat_scalar_tdiv_q_2exp(C::Ref{ZZMatrix}, B::Ref{ZZMatrix}, t::Cint)::Nothing
  return C
end

@doc raw"""
    round(::Type{ZZRingElem}, a::ZZRingElem, b::ZZRingElem) -> ZZRingElem

Computes `round(a//b)`.
"""
function Base.round(::Type{ZZRingElem}, a::ZZRingElem, b::ZZRingElem)
  s = sign(a) * sign(b)
  bs = abs(b)
  as = abs(a)
  r = s * div(2 * as + bs, 2 * bs)
  #  @assert r == round(ZZRingElem, a//b)
  return r
end

function is_squarefree(x::Generic.Poly{AbsSimpleNumFieldElem})
  return isone(gcd(x, derivative(x), true))
end

################################################################################
#
#  Base case for dot products
#
################################################################################

dot(x::ZZRingElem, y::NumFieldElem) = x * y

dot(x::Integer, y::NumFieldElem) = x * y

dot(x::NumFieldElem, y::Integer) = x * y

function dot(a::Vector{<:NumFieldElem}, b::Vector{ZZRingElem})
  d = zero(parent(a[1]))
  t = zero(d)
  for i = 1:length(a)
    mul!(t, a[i], b[i])
    add!(d, d, t)
  end
  return d
end

function bits(x::ArbFieldElem)
  return @ccall libflint.arb_bits(x::Ref{ArbFieldElem})::Int
end

function Base.setprecision(x::BigFloat, p::Int)
  setprecision(BigFloat, p) do
    y = BigFloat()
    @ccall :libmpfr.mpfr_set(y::Ref{BigFloat}, x::Ref{BigFloat}, Base.MPFR.ROUNDING_MODE[]::Int32)::Nothing
    return y
  end
end

function evaluate(f::QQPolyRingElem, r::T) where {T<:RingElem}
  R = parent(r)
  if iszero(f)
    return zero(R)
  end
  l = length(f) - 1
  s = R(coeff(f, l))
  for i in l-1:-1:0
    s = s * r + R(coeff(f, i))
  end
  return s
end

function mod!(f::ZZPolyRingElem, p::ZZRingElem)
  for i = 0:degree(f)
    setcoeff!(f, i, mod(coeff(f, i), p))
  end
end

function mod(f::ZZPolyRingElem, p::ZZRingElem)
  g = parent(f)()
  for i = 0:degree(f)
    setcoeff!(g, i, mod(coeff(f, i), p))
  end
  return g
end

#Assuming that the denominator of a is one, reduces all the coefficients modulo p
# non-symmetric (positive) residue system
function mod!(a::AbsSimpleNumFieldElem, b::ZZRingElem)
  @ccall libflint.nf_elem_mod_fmpz(a::Ref{AbsSimpleNumFieldElem}, a::Ref{AbsSimpleNumFieldElem}, b::Ref{ZZRingElem}, parent(a)::Ref{AbsSimpleNumField})::Nothing
  return a
end

@doc raw"""
    numerator(a::AbsSimpleNumFieldElem) -> AbsSimpleNumFieldElem

For an element $a\in K = Q[t]/f$ write $a$ as $b/d$ with
$b\in Z[t]$, $\deg(a) = \deg(b)$ and $d>0$ minimal in $Z$.
This function returns $b$.
"""
function numerator(a::AbsSimpleNumFieldElem)
  _one = one(ZZ)
  z = deepcopy(a)
  @ccall libflint.nf_elem_set_den(z::Ref{AbsSimpleNumFieldElem}, _one::Ref{ZZRingElem}, a.parent::Ref{AbsSimpleNumField})::Nothing
  return z
end

function lift(R::ZZAbsPowerSeriesRing, f::ZZModAbsPowerSeriesRingElem)
  r = R()
  for i = 0:length(f)-1
    setcoeff!(r, i, lift(coeff(f, i)))
  end
  return r
end

function evaluate(f::fpPolyRingElem, v::Vector{fpFieldElem})
  F = base_ring(f)
  v1 = UInt[x.data for x in v]
  res = UInt[UInt(1) for x in v]
  @ccall libflint.nmod_poly_evaluate_nmod_vec(res::Ptr{UInt}, f::Ref{fpPolyRingElem}, v1::Ptr{UInt}, UInt(length(v))::UInt)::Nothing
  return fpFieldElem[fpFieldElem(x, F) for x in res]
end

function basis(K::fqPolyRepField)
  b = fqPolyRepFieldElem[]
  for i = 1:degree(K)
    x = K()
    setcoeff!(x, i - 1, UInt(1))
    push!(b, x)
  end
  return b
end

function gcd(a::ResElem{T}, b::ResElem{T}) where {T<:IntegerUnion}
  m = modulus(a)
  return parent(a)(gcd(gcd(a.data, m), b.data))
end

function gcdx(a::ResElem{T}, b::ResElem{T}) where {T<:IntegerUnion}
  m = modulus(a)
  R = parent(a)
  g, u, v = gcdx(ZZRingElem(a.data), ZZRingElem(b.data))
  G, U, V = gcdx(g, ZZRingElem(m))
  return R(G), R(U) * R(u), R(U) * R(v)
end

function inv(f::T) where {T<:Union{ZZModPolyRingElem,zzModPolyRingElem}}
  if !is_unit(f)
    error("impossible inverse")
  end
  Rx = parent(f)
  g = Rx(inv(constant_coefficient(f)))
  #lifting: to invert a, start with an inverse b mod m, then
  # then b -> b*(2-ab) is an inverse mod m^2
  # starting with this g, and using the fact that all coeffs are nilpotent
  # we have an inverse modulo s.th. nilpotent. Hence it works
  c = Rx()
  mul!(c, f, g)
  while !isone(c)
    mul!(g, g, 2 - c)
    mul!(c, f, g)
  end
  return g
end

function invmod(f::ZZModPolyRingElem, M::ZZModPolyRingElem)
  if !is_unit(f)
    r = parent(f)()
    ff = ZZ()
    i = @ccall libflint.fmpz_mod_poly_invmod_f(ff::Ref{ZZRingElem}, r::Ref{ZZModPolyRingElem}, f::Ref{ZZModPolyRingElem}, M::Ref{ZZModPolyRingElem}, f.parent.base_ring.ninv::Ref{fmpz_mod_ctx_struct})::Int
    if iszero(i)
      error("not yet implemented")
    else
      return r
    end
  end
  if !is_unit(leading_coefficient(M))
    error("not yet implemented")
  end
  g = parent(f)(inv(constant_coefficient(f)))
  #lifting: to invert a, start with an inverse b mod m, then
  # then b -> b*(2-ab) is an inverse mod m^2
  # starting with this g, and using the fact that all coeffs are nilpotent
  # we have an inverse modulo s.th. nilpotent. Hence it works
  c = f * g
  rem!(c, c, M)
  while !isone(c)
    mul!(g, g, 2 - c)
    rem!(g, g, M)
    mul!(c, f, g)
    rem!(c, c, M)
  end
  return g
end

function round!(z::ArbFieldElem, x::ArbFieldElem, p::Int)
  @ccall libflint.arb_set_round(z::Ref{ArbFieldElem}, x::Ref{ArbFieldElem}, p::Int)::Nothing
  z.parent = ArbField(p, cached=false)
  return z
end

function round!(z::AcbFieldElem, x::AcbFieldElem, p::Int)
  @ccall libflint.acb_set_round(z::Ref{AcbFieldElem}, x::Ref{AcbFieldElem}, p::Int)::Nothing
  z.parent = AcbField(p, cached=false)
  return z
end

function round(x::ArbFieldElem, p::Int)
  z = ArbField(p, cached=false)()
  @ccall libflint.arb_set_round(z::Ref{ArbFieldElem}, x::Ref{ArbFieldElem}, p::Int)::Nothing
  return z
end

function round(x::AcbFieldElem, p::Int)
  z = AcbField(p, cached=false)()
  @ccall libflint.acb_set_round(z::Ref{AcbFieldElem}, x::Ref{AcbFieldElem}, p::Int)::Nothing
  return z
end

function bits(x::AcbFieldElem)
  return @ccall libflint.acb_bits(x::Ref{AcbFieldElem})::Int
end

function order(x::EuclideanRingResidueRingElem{ZZRingElem}, fp::Dict{ZZRingElem,Int64})
  error("missing")
end

fit!(::QQRelPowerSeriesRingElem, Int) = nothing
fit!(::QQAbsPowerSeriesRingElem, Int) = nothing

function gen(R::Union{EuclideanRingResidueRing{fqPolyRepPolyRingElem},EuclideanRingResidueField{fqPolyRepPolyRingElem}}) ## this is not covered by above
  return R(gen(base_ring(R)))              ## and I don't know why
end

function gen(R::Union{EuclideanRingResidueRing{zzModPolyRingElem},EuclideanRingResidueField{zzModPolyRingElem}})
  return R(gen(base_ring(R)))
end

function characteristic(R::Union{EuclideanRingResidueRing{ZZRingElem},EuclideanRingResidueField{ZZRingElem}})
  return modulus(R)
end

function characteristic(R::Union{EuclideanRingResidueRing{zzModPolyRingElem},EuclideanRingResidueField{zzModPolyRingElem}})
  return characteristic(base_ring(base_ring(R)))
end

# discuss: size = order? order = size?
function size(R::Union{EuclideanRingResidueRing{zzModPolyRingElem},EuclideanRingResidueField{zzModPolyRingElem}})
  return characteristic(R)^degree(modulus(R))
end

function size(R::Union{EuclideanRingResidueRing{ZZRingElem},EuclideanRingResidueField{ZZRingElem}})
  return modulus(R)
end

function size(R::Union{EuclideanRingResidueRing{fqPolyRepPolyRingElem},EuclideanRingResidueField{fqPolyRepPolyRingElem}})
  return size(base_ring(base_ring(R)))^degree(R.modulus)
end

function size(R::FqPolyRepField)
  return order(R)
end

function size(R::fqPolyRepField)
  return order(R)
end

function size(F::fpField)
  return order(F)
end

function size(F::FpField)
  return order(F)
end

function size(F::FqField)
  return order(F)
end

function order(R::zzModRing)
  return ZZRingElem(R.n)
end

#################################################
# in triplicate.... and probably cases missing...
function elem_to_mat_row!(M::MatElem, i::Int, a::ResElem{T}) where {T<:PolyRingElem}
  z = zero(parent(M[1, 1]))
  for j = 0:degree(a.data)
    M[i, j+1] = coeff(a.data, j)
  end
  for j = degree(a.data)+2:ncols(M)
    M[i, j] = z
  end
end
function elem_to_mat_row!(M::MatElem, i::Int, a::ResElem{FqPolyRepPolyRingElem})
  z = zero(parent(M[1, 1]))
  for j = 0:degree(a.data)
    M[i, j+1] = coeff(a.data, j)
  end
  for j = degree(a.data)+2:ncols(M)
    M[i, j] = z
  end
end
function elem_to_mat_row!(M::MatElem, i::Int, a::ResElem{fqPolyRepPolyRingElem})
  z = zero(parent(M[1, 1]))
  for j = 0:degree(a.data)
    M[i, j+1] = coeff(a.data, j)
  end
  for j = degree(a.data)+2:ncols(M)
    M[i, j] = z
  end
end

function rand(R::Union{EuclideanRingResidueRing{ZZRingElem},EuclideanRingResidueField{ZZRingElem}})
  return R(rand(ZZRingElem(0):(size(R)-1)))
end

function rand(R::EuclideanRingResidueField{ZZRingElem})
  return R(rand(ZZRingElem(0):(order(R)-1)))
end

function rand(R::Union{EuclideanRingResidueRing{fqPolyRepPolyRingElem},EuclideanRingResidueField{fqPolyRepPolyRingElem}})
  r = rand(base_ring(base_ring(R)))
  g = gen(R)
  for i = 1:degree(R.modulus)
    r = r * g + rand(base_ring(base_ring(R)))
  end
  return r
end

function rand(R::Union{EuclideanRingResidueRing{FqPolyRepPolyRingElem},EuclideanRingResidueField{FqPolyRepPolyRingElem}})
  r = rand(base_ring(base_ring(R)))
  g = gen(R)
  for i = 1:degree(R.modulus)
    r = r * g + rand(base_ring(base_ring(R)))
  end
  return r
end

function rand(R::Union{EuclideanRingResidueRing{zzModPolyRingElem},EuclideanRingResidueField{zzModPolyRingElem}})
  r = rand(base_ring(base_ring(R)))
  g = gen(R)
  for i = 1:degree(R.modulus)
    r = r * g + rand(base_ring(base_ring(R)))
  end
  return r
end

function rem!(f::zzModPolyRingElem, g::zzModPolyRingElem, h::zzModPolyRingElem)
  @ccall libflint.nmod_poly_rem(f::Ref{zzModPolyRingElem}, g::Ref{zzModPolyRingElem}, h::Ref{zzModPolyRingElem})::Nothing
  return f
end

function gcd!(f::zzModPolyRingElem, g::zzModPolyRingElem, h::zzModPolyRingElem)
  @ccall libflint.nmod_poly_gcd(f::Ref{zzModPolyRingElem}, g::Ref{zzModPolyRingElem}, h::Ref{zzModPolyRingElem})::Nothing
  return f
end

function gcd!(f::fpPolyRingElem, g::fpPolyRingElem, h::fpPolyRingElem)
  @ccall libflint.nmod_poly_gcd(f::Ref{fpPolyRingElem}, g::Ref{fpPolyRingElem}, h::Ref{fpPolyRingElem})::Nothing
  return f
end

function divexact(a::ZZModRingElem, y::ZZRingElem; check::Bool=true)
  return divexact(a, parent(a)(y), check=check)
end

characteristic(F::EuclideanRingResidueField{ZZRingElem}) = abs(F.modulus)

#@doc raw"""
#    is_univariate(f::Generic.MPoly{T}) where T <: NumFieldElem -> Bool, PolyRingElem{T}
#
#Tests if $f$ involves only one variable. If so, return a corresponding univariate polynomial.
#"""
#function is_univariate(f::Generic.MPoly{T}) where T
#  kx, x = polynomial_ring(base_ring(f), "x", cached = false)
#  if ngens(parent(f)) == 1
#    f1 = kx()
#    for i = 1:f.length
#      setcoeff!(f1, Int(f.exps[1, i]), f.coeffs[i])
#    end
#    return true, f1
#  end
#  if f.length == 0
#    @assert iszero(f)
#    return true, kx(0)
#  end
#  n = ngens(parent(f))
#  i = 1
#  while i <= n && iszero(f.exps[i, :])
#    i += 1
#  end
#  j = n
#  while j >= 1 && iszero(f.exps[j, :])
#    j -= 1
#  end
#  if i != j
#    return false, x
#  end
#  f1 = kx()
#  for j = 1:f.length
#    setcoeff!(f1, Int(f.exps[i, j]), f.coeffs[j])
#  end
#  return true, f1
#end

function (R::ZZMPolyRing)(f::QQMPolyRingElem)
  return map_coefficients(ZZ, f, parent=R)
end

# mainly for testing
function rand(L::LocalizedEuclideanRing{T}, num_scale=(1:1000), den_scale=(1:1000)) where {T<:ZZRingElem}
  num = rand(num_scale)
  den = rand(den_scale)
  while gcd(den, prime(L)) != 1
    den = rand(den_scale)
  end
  return L(num // den)
end

function rand(L::LocalizedEuclideanRing{T}, num_scale::Vector, den_scale::Integer) where {T<:ZZRingElem}
  num = rand(num_scale)
  den = rand(den_scale)
  while gcd(den, prime(L)) != 1
    den = rand(den_scale)
  end
  return L(num // den)
end

function cmpabs(a::Int, b::Int)
  a = abs(a)
  b = abs(b)
  if a > b
    return 1
  elseif a == b
    return 0
  else
    return -1
  end
end

function evaluate(f::QQPolyRingElem, a::AbsSimpleNumFieldElem)
  #Base.show_backtrace(stdout, Base.stacktrace())
  R = parent(a)
  if iszero(f)
    return zero(R)
  end
  if a == gen(R) && parent(f) == parent(parent(a).pol)
    return R(f)
  end
  l = length(f) - 1
  s = R(coeff(f, l))
  for i in l-1:-1:0
    #s = s*a + R(coeff(f, i))
    mul!(s, s, a)
    add!(s, s, coeff(f, i))
  end
  return s
end

function preimage(M::Map{D,C}, a) where {D,C}
  if isdefined(M.header, :preimage)
    p = M.header.preimage(a)::elem_type(D)
    @assert parent(p) === domain(M)
    return p
  end
  error("No preimage function known")
end

function image(M::Map{D,C}, a) where {D,C}
  if isdefined(M, :header)
    if isdefined(M.header, :image)
      return M.header.image(a)::elem_type(C)
    else
      error("No image function known")
    end
  else
    return M(a)
  end
end

function setcoeff!(x::fqPolyRepFieldElem, n::Int, u::UInt)
  @ccall libflint.nmod_poly_set_coeff_ui(x::Ref{fqPolyRepFieldElem}, n::Int, u::UInt)::Nothing
end

function basis(k::fpField)
  return [k(1)]
end

function basis(k::fpField, l::fpField)
  @assert k == l
  return [k(1)]
end

function basis(K::fqPolyRepField, k::fpField)
  @assert characteristic(K) == characteristic(k)
  return basis(K)
end

function root(a::FinFieldElem, n::ZZRingElem)
  return root(a, Int(n))
end

function root(a::FinFieldElem, n::Integer)
  k = parent(a)
  kt, t = polynomial_ring(k, "t", cached=false)
  r = roots(t^n - a)
  return r[1]
end

function base_field(K::fqPolyRepField)
  return Native.GF(Int(characteristic(K)))
end

function gen(k::fpField)
  return k(1)
end

function defining_polynomial(k::fpField)
  kx, x = polynomial_ring(k, cached=false)
  return x - k(1)
end

@doc raw"""
    mod!(A::Generic.Mat{AbsSimpleNumFieldElem}, m::ZZRingElem)

Inplace: reduce all entries of $A$ modulo $m$, into the positive residue system.
"""
function mod!(A::Generic.Mat{AbsSimpleNumFieldElem}, m::ZZRingElem)
  for i = 1:nrows(A)
    for j = 1:ncols(A)
      mod!(A[i, j], m)
    end
  end
end

@doc raw"""
    divexact!(A::Generic.Mat{AbsSimpleNumFieldElem}, p::ZZRingElem)

Inplace: divide each entry of $A$ by $p$.
"""
function divexact!(A::Generic.Mat{AbsSimpleNumFieldElem}, p::ZZRingElem)
  for i = 1:nrows(A)
    for j = 1:ncols(A)
      A[i, j] = A[i, j] // p
    end
  end
  return A
end

#
#  Lifts a matrix from F_p to Z/p^nZ
#

function lift(M::fqPolyRepMatrix, R::zzModRing)
  @assert is_prime_power(modulus(R))
  N = zero_matrix(R, nrows(M), ncols(M))
  for i = 1:nrows(M)
    for j = 1:ncols(M)
      N[i, j] = ZZ(coeff(M[i, j], 0))
    end
  end
  return N
end

function lift(M::fpMatrix, R::zzModRing)
  @assert is_prime_power(modulus(R))
  N = zero_matrix(R, nrows(M), ncols(M))
  for i = 1:nrows(M)
    for j = 1:ncols(M)
      N[i, j] = R(lift(M[i, j]))
    end
  end
  return N
end

denominator(a::QQFieldElem, ::ZZRing) = denominator(a)

numerator(a::QQFieldElem, ::ZZRing) = numerator(a)

function (R::QQPolyRing)(a::Generic.RationalFunctionFieldElem{QQFieldElem})
  @assert isone(denominator(a))
  return R(numerator(a))
end

@doc raw"""
    zeros(f::ZZPolyRingElem) -> Vector{ZZRingElem}

Computes the integer zeros of a given polynomial $f$.
"""
function zeros(f::ZZPolyRingElem)

  fac = factor(f)
  zeros = ZZRingElem[]

  # check if there are monic linear factors <-> zeros
  for i in fac
    if degree(i[1]) == 1 && leading_coefficient(i[1]) == 1
      push!(zeros, -coeff(i[1], 0))
    end
  end

  return zeros
end

#This should probably go somewhere else. (Taking the nth derivative)
function derivative(x::AcbPolyRingElem, n::Int64)
  for i in (1:n)
    x = derivative(x)
  end
  return x
end

function lift!(x::fpFieldElem, z::ZZRingElem)
  set!(z, x.data)
  return z
end

function lift!(x::EuclideanRingResidueFieldElem{ZZRingElem}, z::ZZRingElem)
  set!(z, x.data)
  return z
end

degree(::EuclideanRingResidueField{ZZRingElem}) = 1
degree(::QQField) = 1

Base.:(*)(x::QQFieldElem, y::AbstractAlgebra.Generic.MatSpaceElem{AbsSimpleNumFieldElem}) = base_ring(y)(x) * y

function mod_sym(a::T, b::T) where {T}
  return mod(a, b)
end
function mod_sym!(a::T, b::T) where {T}
  return mod!(a, b)
end

Base.replace!(::typeof(-), m::ZZMatrix) = -m

function (A::AbsSimpleNumField)(a::ZZPolyRingElem)
  return A(parent(defining_polynomial(A))(a))
end

AbstractAlgebra.promote_rule(::Type{S}, ::Type{ZZRingElem}) where {S<:NumFieldElem} = S

AbstractAlgebra.promote_rule(::Type{ZZRingElem}, ::Type{S}) where {S<:NumFieldElem} = S

AbstractAlgebra.promote_rule(::Type{S}, ::Type{QQFieldElem}) where {S<:NumFieldElem} = S

AbstractAlgebra.promote_rule(::Type{QQFieldElem}, ::Type{S}) where {S<:NumFieldElem} = S

function is_positive(x::ZZRingElem, ::Union{PosInf,Vector{PosInf}})
  return sign(x) == 1
end

function is_positive(x::QQFieldElem, ::Union{PosInf,Vector{PosInf}})
  return sign(x) == 1
end

function is_negative(x::ZZRingElem, ::Union{PosInf,Vector{PosInf}})
  return sign(x) == -1
end

function is_negative(x::QQFieldElem, ::Union{PosInf,Vector{PosInf}})
  return sign(x) == -1
end

function (R::Generic.PolyRing{AbsSimpleNumFieldElem})(f::Generic.MPoly)
  if length(f) == 0
    return R()
  end
  j = 1
  c = 0
  while j <= ngens(parent(f))
    if f.exps[j, 1] != 0
      if c == 0
        c = j
      else
        error("poly is not univariate")
      end
    end
    j += 1
  end
  g = R()
  for i = 1:length(f)
    setcoeff!(g, Int(f.exps[c, i]), f.coeffs[i])
  end
  return g
end

function Base.map!(f, M::ZZMatrix)
  for i = 1:nrows(M)
    for j = 1:ncols(M)
      M[i, j] = f(M[i, j])
    end
  end
end

Base.log2(a::ZZRingElem) = log2(BigInt(a)) # stupid: there has to be faster way

is_cyclo_type(::NumField) = false


function nf_elem_to_fmpz_mod_poly!(r::ZZModPolyRingElem, a::AbsSimpleNumFieldElem, useden::Bool=true)
  @ccall libflint.nf_elem_get_fmpz_mod_poly_den(r::Ref{ZZModPolyRingElem}, a::Ref{AbsSimpleNumFieldElem}, a.parent::Ref{AbsSimpleNumField}, Cint(useden)::Cint, r.parent.base_ring.ninv::Ref{fmpz_mod_ctx_struct})::Nothing
  return nothing
end

function (R::ZZModPolyRing)(a::AbsSimpleNumFieldElem)
  r = R()
  nf_elem_to_fmpz_mod_poly!(r, a)
  return r
end

function nf_elem_to_gfp_poly!(r::fpPolyRingElem, a::AbsSimpleNumFieldElem, useden::Bool=true)
  @ccall libflint.nf_elem_get_nmod_poly_den(r::Ref{fpPolyRingElem}, a::Ref{AbsSimpleNumFieldElem}, a.parent::Ref{AbsSimpleNumField}, Cint(useden)::Cint)::Nothing
  return nothing
end

function (R::fpPolyRing)(a::AbsSimpleNumFieldElem)
  r = R()
  nf_elem_to_gfp_poly!(r, a)
  return r
end

function nf_elem_to_nmod_poly!(r::zzModPolyRingElem, a::AbsSimpleNumFieldElem, useden::Bool=true)
  @ccall libflint.nf_elem_get_nmod_poly_den(r::Ref{zzModPolyRingElem}, a::Ref{AbsSimpleNumFieldElem}, a.parent::Ref{AbsSimpleNumField}, Cint(useden)::Cint)::Nothing
  return nothing
end

function (R::zzModPolyRing)(a::AbsSimpleNumFieldElem)
  r = R()
  nf_elem_to_nmod_poly!(r, a)
  return r
end

function nf_elem_to_gfp_fmpz_poly!(r::FpPolyRingElem, a::AbsSimpleNumFieldElem, useden::Bool=true)
  @ccall libflint.nf_elem_get_fmpz_mod_poly_den(r::Ref{FpPolyRingElem}, a::Ref{AbsSimpleNumFieldElem}, a.parent::Ref{AbsSimpleNumField}, Cint(useden)::Cint, r.parent.base_ring.ninv::Ref{fmpz_mod_ctx_struct})::Nothing
  return nothing
end

function mod_sym!(f::ZZPolyRingElem, p::ZZRingElem)
  for i = 0:degree(f)
    setcoeff!(f, i, mod_sym(coeff(f, i), p))
  end
  return f
end

function mod_sym(a::AbsSimpleNumFieldElem, b::ZZRingElem, b2::ZZRingElem)
  # TODO: this is not correct
  return mod_sym(a, b)
  return z
end

function ^(x::NumFieldElem, y::ZZRingElem)
  if fits(Int, y)
    return x^Int(y)
  end

  return _power(x, y)
end

# We test once if it fits, otherwise we would have to check for every ^-call
function _power(x::NumFieldElem, y::ZZRingElem)
  res = parent(x)()
  if y < 0
    res = _power(inv(x), -y)
  elseif y == 0
    res = parent(x)(1)
  elseif y == 1
    res = deepcopy(x)
  elseif mod(y, 2) == 0
    z = _power(x, Base.div(y, 2))
    res = z * z
  else
    res = _power(x, y - 1) * x
  end
  return res
end

function (Rx::fpPolyRing)(a::fqPolyRepFieldElem)
  el = Rx()
  for i = 0:degree(parent(a))
    setcoeff!(el, i, base_ring(Rx)(coeff(a, i)))
  end
  return el
end

function (F::FpField)(a::FqPolyRepFieldElem)
  for i = 1:degree(parent(a))-1
    @assert iszero(coeff(a, i))
  end
  return F(coeff(a, 0))
end

function (F::fpField)(a::fqPolyRepFieldElem)
  for i = 1:degree(parent(a))-1
    @assert iszero(coeff(a, i))
  end
  return F(coeff(a, 0))
end


function (R::FqPolyRepField)(x::ZZModPolyRingElem)
  z = R()
  @ccall libflint.fq_set_fmpz_mod_poly(z::Ref{FqPolyRepFieldElem}, x::Ref{ZZModPolyRingElem}, R::Ref{FqPolyRepField})::Nothing
  #@ccall libflint.fq_reduce(z::Ref{FqPolyRepFieldElem}, R::Ref{FqPolyRepField})::Nothing
  return z
end

function (R::FqPolyRepField)(x::FpPolyRingElem)
  z = R()
  @ccall libflint.fq_set_fmpz_mod_poly(z::Ref{FqPolyRepFieldElem}, x::Ref{FpPolyRingElem}, R::Ref{FqPolyRepField})::Nothing
  @ccall libflint.fq_reduce(z::Ref{FqPolyRepFieldElem}, R::Ref{FqPolyRepField})::Nothing
  return z
end

function rem!(a::ZZModPolyRingElem, b::ZZModPolyRingElem, c::ZZModPolyRingElem)
  @ccall libflint.fmpz_mod_poly_rem(a::Ref{ZZModPolyRingElem}, b::Ref{ZZModPolyRingElem}, c::Ref{ZZModPolyRingElem}, a.parent.base_ring.ninv::Ref{fmpz_mod_ctx_struct})::Nothing
  return a
end

function rem!(a::FpPolyRingElem, b::FpPolyRingElem, c::FpPolyRingElem)
  @ccall libflint.fmpz_mod_poly_rem(a::Ref{FpPolyRingElem}, b::Ref{FpPolyRingElem}, c::Ref{FpPolyRingElem}, a.parent.base_ring.ninv::Ref{fmpz_mod_ctx_struct})::Nothing
  return a
end

########################################
#
# misc infinity changes that need to stay in Nemo (after moving the rest to AA) # TODO: move somewhere sensible
#
########################################

Base.isless(::PosInf, ::Union{ZZRingElem,QQFieldElem}) = false
Base.isless(::Union{ZZRingElem,QQFieldElem}, ::PosInf) = true

Base.isless(::NegInf, ::Union{ZZRingElem,QQFieldElem}) = true
Base.isless(::Union{ZZRingElem,QQFieldElem}, ::NegInf) = false

Base.:+(::ZZRingElem, inf::AnyInf) = inf
Base.:+(inf::AnyInf, ::ZZRingElem) = inf

Base.:-(inf::AnyInf, ::ZZRingElem) = inf
Base.:-(::ZZRingElem, inf::AnyInf) = -inf
