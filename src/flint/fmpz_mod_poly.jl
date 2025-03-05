################################################################################
#
#  ZZModPolyRingElem.jl : FLINT ZZModPolyRingElem (polynomials over Z/nZ, large modulus)
#
################################################################################

################################################################################
#
#  Type and parent object methods
#
################################################################################

parent(a::ZZModPolyRingElem) = a.parent

base_ring(R::ZZModPolyRing) = R.base_ring

elem_type(::Type{ZZModPolyRing}) = ZZModPolyRingElem

parent_type(::Type{ZZModPolyRingElem}) = ZZModPolyRing

dense_poly_type(::Type{ZZModRingElem}) = ZZModPolyRingElem

function _is_one_or_throw(f, y)
  R = base_ring(y)
  if !isone(f)
    throw(NotInvertibleError(R(f), R))
  end
end

################################################################################
#
#  Basic manipulation
#
################################################################################

length(x::T) where {T <: Zmodn_fmpz_poly} = x.length

function degree(x::T) where {T <: Zmodn_fmpz_poly}
  return x.length - 1
  #   return @ccall libflint.fmpz_mod_poly_degree(x::Ref{T}, x.parent.base_ring.ninv::Ref{fmpz_mod_ctx_struct})::Int
end

function coeff(x::T, n::Int) where {T <: Zmodn_fmpz_poly}
  n < 0 && throw(DomainError(n, "Index must be non-negative"))
  z = ZZRingElem()
  @ccall libflint.fmpz_mod_poly_get_coeff_fmpz(z::Ref{ZZRingElem}, x::Ref{T}, n::Int, x.parent.base_ring.ninv::Ref{fmpz_mod_ctx_struct})::Nothing
  return base_ring(x)(z)
end

zero(R::ZmodNFmpzPolyRing) = R(0)

one(R::ZmodNFmpzPolyRing) = R(1)

gen(R::ZmodNFmpzPolyRing) = R([ZZRingElem(0), ZZRingElem(1)])

is_gen(a::Zmodn_fmpz_poly) = (degree(a) == 1 &&
                              iszero(coeff(a,0)) && isone(coeff(a,1)))

var(R::ZmodNFmpzPolyRing) = R.S

modulus(a::Zmodn_fmpz_poly) = a.parent.n

modulus(R::ZmodNFmpzPolyRing) = R.n

function deepcopy_internal(a::T, dict::IdDict) where {T <: Zmodn_fmpz_poly}
  z = T(base_ring(parent(a)), a)
  z.parent = a.parent
  return z
end

###############################################################################
#
#   Similar
#
###############################################################################

function similar(f::PolyRingElem, R::ZZModRing, s::Symbol=var(parent(f)); cached::Bool=true)
  z = ZZModPolyRingElem(R)
  if base_ring(f) === R && s == var(parent(f)) && f isa ZZModPolyRingElem
    # steal parent in case it is not cached
    z.parent = parent(f)
  else
    z.parent = ZZModPolyRing(R, s, cached)
  end
  return z
end

###############################################################################
#
#   polynomial constructor
#
###############################################################################

function polynomial(R::ZZModRing, arr::Vector{T}, var::VarName=:x; cached::Bool=true) where T
  coeffs = map(R, arr)
  coeffs = length(coeffs) == 0 ? ZZModRingElem[] : coeffs
  z = ZZModPolyRingElem(R, coeffs)
  z.parent = ZZModPolyRing(R, Symbol(var), cached)
  return z
end

################################################################################
#
#  Canonicalization
#
################################################################################

canonical_unit(a::Zmodn_fmpz_poly) = canonical_unit(leading_coefficient(a))

################################################################################
#
#  Unary operations
#
################################################################################

-(x::T) where {T <: Zmodn_fmpz_poly} = neg!(parent(x)(), x)

################################################################################
#
#   Binary operations
#
################################################################################

function +(x::T, y::T) where {T <: Zmodn_fmpz_poly}
  check_parent(x,y)
  z = parent(x)()
  @ccall libflint.fmpz_mod_poly_add(z::Ref{T}, x::Ref{T}, y::Ref{T}, x.parent.base_ring.ninv::Ref{fmpz_mod_ctx_struct})::Nothing
  return z
end

function -(x::T, y::T) where {T <: Zmodn_fmpz_poly}
  check_parent(x,y)
  z = parent(x)()
  @ccall libflint.fmpz_mod_poly_sub(z::Ref{T}, x::Ref{T}, y::Ref{T}, x.parent.base_ring.ninv::Ref{fmpz_mod_ctx_struct})::Nothing
  return z
end

function *(x::T, y::T) where {T <: Zmodn_fmpz_poly}
  check_parent(x,y)
  z = parent(x)()
  @ccall libflint.fmpz_mod_poly_mul(z::Ref{T}, x::Ref{T}, y::Ref{T}, x.parent.base_ring.ninv::Ref{fmpz_mod_ctx_struct})::Nothing
  return z
end

###############################################################################
#
#  Ad hoc binary operations
#
###############################################################################

function *(x::ZZModPolyRingElem, y::ZZRingElem)
  z = parent(x)()
  @ccall libflint.fmpz_mod_poly_scalar_mul_fmpz(z::Ref{ZZModPolyRingElem}, x::Ref{ZZModPolyRingElem}, y::Ref{ZZRingElem}, x.parent.base_ring.ninv::Ref{fmpz_mod_ctx_struct})::Nothing
  return z
end

*(x::ZZRingElem, y::ZZModPolyRingElem) = y*x

*(x::ZZModPolyRingElem, y::Integer) = x*ZZRingElem(y)

*(x::Integer, y::ZZModPolyRingElem) = y*x

function *(x::ZZModPolyRingElem, y::ZZModRingElem)
  (base_ring(x) != parent(y)) && error("Must have same parent")
  return x*y.data
end

*(x::ZZModRingElem, y::ZZModPolyRingElem) = y*x

function +(x::ZZModPolyRingElem, y::Int)
  z = parent(x)()
  @ccall libflint.fmpz_mod_poly_add_si(z::Ref{ZZModPolyRingElem}, x::Ref{ZZModPolyRingElem}, y::Int, x.parent.base_ring.ninv::Ref{fmpz_mod_ctx_struct})::Nothing
  return z
end

+(x::Int, y::ZZModPolyRingElem) = +(y, x)

function +(x::ZZModPolyRingElem, y::ZZRingElem)
  z = parent(x)()
  @ccall libflint.fmpz_mod_poly_add_fmpz(z::Ref{ZZModPolyRingElem}, x::Ref{ZZModPolyRingElem}, y::Ref{ZZRingElem}, x.parent.base_ring.ninv::Ref{fmpz_mod_ctx_struct})::Nothing
  return z
end

+(x::ZZRingElem, y::ZZModPolyRingElem) = y + x

+(x::ZZModPolyRingElem, y::Integer) = x + ZZRingElem(y)

+(x::Integer, y::ZZModPolyRingElem) = ZZRingElem(x) + y

function +(x::ZZModPolyRingElem, y::ZZModRingElem)
  (base_ring(x) != parent(y)) && error("Elements must have same parent")
  return x + y.data
end

+(x::ZZModRingElem, y::ZZModPolyRingElem) = y + x

function -(x::ZZModPolyRingElem, y::Int)
  z = parent(x)()
  @ccall libflint.fmpz_mod_poly_sub_si(z::Ref{ZZModPolyRingElem}, x::Ref{ZZModPolyRingElem}, y::Int, x.parent.base_ring.ninv::Ref{fmpz_mod_ctx_struct})::Nothing
  return z
end

function -(x::Int, y::ZZModPolyRingElem)
  z = parent(y)()
  @ccall libflint.fmpz_mod_poly_si_sub(z::Ref{ZZModPolyRingElem}, x::Int, y::Ref{ZZModPolyRingElem}, y.parent.base_ring.ninv::Ref{fmpz_mod_ctx_struct})::Nothing
  return z
end

function -(x::ZZModPolyRingElem, y::ZZRingElem)
  z = parent(x)()
  @ccall libflint.fmpz_mod_poly_sub_fmpz(z::Ref{ZZModPolyRingElem}, x::Ref{ZZModPolyRingElem}, y::Ref{ZZRingElem}, x.parent.base_ring.ninv::Ref{fmpz_mod_ctx_struct})::Nothing
  return z
end

function -(x::ZZRingElem, y::ZZModPolyRingElem)
  z = parent(y)()
  @ccall libflint.fmpz_mod_poly_fmpz_sub(z::Ref{ZZModPolyRingElem}, x::Ref{ZZRingElem}, y::Ref{ZZModPolyRingElem}, y.parent.base_ring.ninv::Ref{fmpz_mod_ctx_struct})::Nothing
  return z
end

-(x::ZZModPolyRingElem, y::Integer) = x - ZZRingElem(y)

-(x::Integer, y::ZZModPolyRingElem) = ZZRingElem(x) - y

function -(x::ZZModPolyRingElem, y::ZZModRingElem)
  (base_ring(x) != parent(y)) && error("Elements must have same parent")
  return x - y.data
end

function -(x::ZZModRingElem, y::ZZModPolyRingElem)
  (parent(x) != base_ring(y)) && error("Elements must have same parent")
  return x.data - y
end

################################################################################
#
#  Powering
#
################################################################################

function ^(x::T, y::Int) where {T <: Zmodn_fmpz_poly}
  y < 0 && throw(DomainError(y, "Exponent must be non-negative"))
  z = parent(x)()
  @ccall libflint.fmpz_mod_poly_pow(z::Ref{T}, x::Ref{T}, y::UInt, x.parent.base_ring.ninv::Ref{fmpz_mod_ctx_struct})::Nothing
  return z
end

################################################################################
#
#  Comparison
#
################################################################################

function ==(x::T, y::T) where {T <: Zmodn_fmpz_poly}
  check_parent(x, y)
  return Bool(@ccall libflint.fmpz_mod_poly_equal(x::Ref{T}, y::Ref{T}, x.parent.base_ring.ninv::Ref{fmpz_mod_ctx_struct})::Cint)
end

################################################################################
#
#  Ad hoc comparisons
#
################################################################################

function ==(x::ZZModPolyRingElem, y::ZZModRingElem)
  base_ring(x) != parent(y) && error("Incompatible base rings in comparison")
  if length(x) > 1
    return false
  elseif length(x) == 1
    u = ZZRingElem()
    @ccall libflint.fmpz_mod_poly_get_coeff_fmpz(u::Ref{ZZRingElem}, x::Ref{ZZModPolyRingElem}, 0::Int, x.parent.base_ring.ninv::Ref{fmpz_mod_ctx_struct})::Nothing
    return u == y
  else
    return iszero(y)
  end
end

==(x::ZZModRingElem, y::ZZModPolyRingElem) = y == x

################################################################################
#
#  Truncation
#
################################################################################

function truncate(a::T, n::Int) where {T <: Zmodn_fmpz_poly}
  n < 0 && throw(DomainError(n, "Index must be non-negative"))

  z = deepcopy(a)

  if length(z) <= n
    return z
  end

  @ccall libflint.fmpz_mod_poly_truncate(z::Ref{T}, n::Int, z.parent.base_ring.ninv::Ref{fmpz_mod_ctx_struct})::Nothing
  return z
end

function mullow(x::T, y::T, n::Int) where {T <: Zmodn_fmpz_poly}
  check_parent(x, y)
  n < 0 && throw(DomainError(n, "Index must be non-negative"))

  z = parent(x)()
  @ccall libflint.fmpz_mod_poly_mullow(z::Ref{T}, x::Ref{T}, y::Ref{T}, n::Int, x.parent.base_ring.ninv::Ref{fmpz_mod_ctx_struct})::Nothing
  return z
end

###############################################################################
#
#   Reversal
#
###############################################################################

function reverse(x::T, len::Int) where {T <: Zmodn_fmpz_poly}
  len < 0 && throw(DomainError(len, "Length must be non-negative"))
  return reverse!(parent(x)(), x, len)
end

###############################################################################
#
#   Shifting
#
###############################################################################

function shift_left(x::T, len::Int) where {T <: Zmodn_fmpz_poly}
  len < 0 && throw(DomainError(len, "Shift must be non-negative"))
  z = parent(x)()
  @ccall libflint.fmpz_mod_poly_shift_left(z::Ref{T}, x::Ref{T}, len::Int, x.parent.base_ring.ninv::Ref{fmpz_mod_ctx_struct})::Nothing
  return z
end

function shift_right(x::T, len::Int) where {T <: Zmodn_fmpz_poly}
  len < 0 && throw(DomainError(len, "Shift must be non-negative"))
  z = parent(x)()
  @ccall libflint.fmpz_mod_poly_shift_right(z::Ref{T}, x::Ref{T}, len::Int, x.parent.base_ring.ninv::Ref{fmpz_mod_ctx_struct})::Nothing
  return z
end

###############################################################################
#
#   Exact division
#
###############################################################################

function divexact(x::T, y::T; check::Bool=true) where {T <: Zmodn_fmpz_poly}
  check_parent(x, y)
  iszero(y) && throw(DivideError())
  d = ZZRingElem()
  q = parent(x)()
  r = parent(x)()
  @ccall libflint.fmpz_mod_poly_divrem_f(d::Ref{ZZRingElem}, q::Ref{T}, r::Ref{T}, x::Ref{T}, y::Ref{T}, x.parent.base_ring.ninv::Ref{fmpz_mod_ctx_struct})::Nothing
  _is_one_or_throw(d, y)
  return q
end

Base.div(x::T, y::T) where {T <: Zmodn_fmpz_poly} = divexact(x,y)

################################################################################
#
#  Ad hoc exact division
#
################################################################################

function divexact(x::ZZModPolyRingElem, y::ZZModRingElem; check::Bool=true)
  base_ring(x) != parent(y) && error("Elements must have same parent")
  iszero(y) && throw(DivideError())
  q = parent(x)()
  @ccall libflint.fmpz_mod_poly_scalar_div_fmpz(q::Ref{ZZModPolyRingElem}, x::Ref{ZZModPolyRingElem}, y.data::Ref{ZZRingElem}, x.parent.base_ring.ninv::Ref{fmpz_mod_ctx_struct})::Nothing
  return q
end

function divexact(x::T, y::ZZRingElem; check::Bool=true) where {T <: Zmodn_fmpz_poly}
  iszero(y) && throw(DivideError())
  q = parent(x)()
  @ccall libflint.fmpz_mod_poly_scalar_div_fmpz(q::Ref{T}, x::Ref{T}, y::Ref{ZZRingElem}, x.parent.base_ring.ninv::Ref{fmpz_mod_ctx_struct})::Nothing
  return q
end

function divexact(x::T, y::Int; check::Bool=true) where {T <: Zmodn_fmpz_poly}
  y == 0 && throw(DivideError())
  q = parent(x)()
  @ccall libflint.fmpz_mod_poly_scalar_div_fmpz(q::Ref{T}, x::Ref{T}, ZZRingElem(y)::Ref{ZZRingElem}, x.parent.base_ring.ninv::Ref{fmpz_mod_ctx_struct})::Nothing
  return q
end

################################################################################
#
#  Division with remainder
#
################################################################################

function Base.divrem(x::T, y::T) where {T <: Zmodn_fmpz_poly}
  check_parent(x,y)
  iszero(y) && throw(DivideError())
  q = parent(x)()
  r = parent(x)()
  d = ZZRingElem()
  @ccall libflint.fmpz_mod_poly_divrem_f(d::Ref{ZZRingElem}, q::Ref{T}, r::Ref{T}, x::Ref{T}, y::Ref{T}, x.parent.base_ring.ninv::Ref{fmpz_mod_ctx_struct})::Nothing
  _is_one_or_throw(d, y)
  return q, r
end

################################################################################
#
#  Remainder
#
################################################################################

function rem(x::T, y::T) where {T <: Zmodn_fmpz_poly}
  q, r = divrem(x, y)
  return r
end

mod(x::T, y::T) where {T <: Zmodn_fmpz_poly} = rem(x, y)

################################################################################
#
#  Removal and valuation
#
################################################################################

function divides(z::T, x::T) where {T <: Zmodn_fmpz_poly}
  if iszero(z)
    return true, zero(parent(z))
  end
  if iszero(x)
    return false, zero(parent(z))
  end
  q, r = divrem(z, x)
  return iszero(r), q
end

################################################################################
#
#  GCD
#
################################################################################

function AbstractAlgebra.hgcd_prefers_basecase(a::ZZModPolyRingElem, b::ZZModPolyRingElem)
  return length(b) < 150
end

function AbstractAlgebra.mat22_mul_prefers_classical(
    a11::ZZModPolyRingElem, a12::ZZModPolyRingElem, a21::ZZModPolyRingElem, a22::ZZModPolyRingElem,
    b11::ZZModPolyRingElem, b12::ZZModPolyRingElem, b21::ZZModPolyRingElem, b22::ZZModPolyRingElem)
  return length(a11) + length(a22) < 30 || length(b11) + length(b22) < 30
end

function AbstractAlgebra.gcd_basecase(x::ZZModPolyRingElem, y::ZZModPolyRingElem)
  z = parent(x)()
  f = ZZRingElem()
  @ccall libflint.fmpz_mod_poly_gcd_euclidean_f(f::Ref{ZZRingElem}, z::Ref{ZZModPolyRingElem}, x::Ref{ZZModPolyRingElem}, y::Ref{ZZModPolyRingElem}, x.parent.base_ring.ninv::Ref{fmpz_mod_ctx_struct})::Nothing
  _is_one_or_throw(f, y)
  return z
end

function AbstractAlgebra.gcdx_basecase(x::ZZModPolyRingElem, y::ZZModPolyRingElem)
  check_parent(x, y)
  g = parent(x)()
  s = parent(x)()
  t = parent(x)()
  f = ZZRingElem()
  @ccall libflint.fmpz_mod_poly_xgcd_euclidean_f(f::Ref{ZZRingElem}, g::Ref{ZZModPolyRingElem}, s::Ref{ZZModPolyRingElem}, t::Ref{ZZModPolyRingElem}, x::Ref{ZZModPolyRingElem}, y::Ref{ZZModPolyRingElem}, x.parent.base_ring.ninv::Ref{fmpz_mod_ctx_struct})::Nothing
  _is_one_or_throw(f, y)
  return g, s, t
end

function AbstractAlgebra.gcdinv_basecase(x::ZZModPolyRingElem, y::ZZModPolyRingElem)
  check_parent(x, y)
  length(y) <= 1 && error("Length of second argument must be >= 2")
  g = parent(x)()
  s = parent(x)()
  f = ZZRingElem()
  @ccall libflint.fmpz_mod_poly_gcdinv_euclidean_f(f::Ref{ZZRingElem}, g::Ref{ZZModPolyRingElem}, s::Ref{ZZModPolyRingElem}, x::Ref{ZZModPolyRingElem}, y::Ref{ZZModPolyRingElem}, x.parent.base_ring.ninv::Ref{fmpz_mod_ctx_struct})::Nothing
  _is_one_or_throw(f, y)
  return g, s
end

# AA does gcd, gcdx, and gcdinv in general

################################################################################
#
#  Modular arithmetic
#
################################################################################

function invmod(x::T, y::T) where {T <: Zmodn_fmpz_poly}
  is_zero(y) && error("Second argument must not be 0")
  check_parent(x,y)
  if length(y) == 1
    t = evaluate(x, coeff(y, 0))
    if !is_zero(t)
      t = inv!(t)
    end
    return parent(x)(t)
  end
  z = parent(x)()
  r = @ccall libflint.fmpz_mod_poly_invmod(z::Ref{T}, x::Ref{T}, y::Ref{T}, x.parent.base_ring.ninv::Ref{fmpz_mod_ctx_struct})::Cint
  r == 0 ? error("Impossible inverse in invmod") : return z
end

function mulmod(x::T, y::T, z::T) where {T <: Zmodn_fmpz_poly}
  check_parent(x, y)
  check_parent(y, z)
  w = parent(x)()
  @ccall libflint.fmpz_mod_poly_mulmod(w::Ref{T}, x::Ref{T}, y::Ref{T}, z::Ref{T}, x.parent.base_ring.ninv::Ref{fmpz_mod_ctx_struct})::Nothing
  return w
end

function powermod(x::T, e::Int, y::T) where {T <: Zmodn_fmpz_poly}
  check_parent(x, y)
  z = parent(x)()

  if e < 0
    g, x = gcdinv(x, y)
    if g != 1
      error("Element not invertible")
    end
    e = -e
  end

  @ccall libflint.fmpz_mod_poly_powmod_ui_binexp(z::Ref{T}, x::Ref{T}, e::UInt, y::Ref{T}, x.parent.base_ring.ninv::Ref{fmpz_mod_ctx_struct})::Nothing

  return z
end

function powermod(x::T, e::ZZRingElem, y::T) where {T <: Zmodn_fmpz_poly}
  z = parent(x)()

  if e < 0
    g, x = gcdinv(x, y)
    if g != 1
      error("Element not invertible")
    end
    e = -e
  end

  @ccall libflint.fmpz_mod_poly_powmod_fmpz_binexp(z::Ref{T}, x::Ref{T}, e::Ref{ZZRingElem}, y::Ref{T}, x.parent.base_ring.ninv::Ref{fmpz_mod_ctx_struct})::Nothing
  return z
end

################################################################################
#
#  Resultant
#
################################################################################

function resultant(x::T, y::T) where {T <: Zmodn_fmpz_poly}
  check_parent(x,y)
  z = parent(x)()
  !is_probable_prime(modulus(x)) && error("Modulus not prime in resultant")
  r = ZZRingElem()
  @ccall libflint.fmpz_mod_poly_resultant(r::Ref{ZZRingElem}, x::Ref{T}, y::Ref{T}, x.parent.base_ring.ninv::Ref{fmpz_mod_ctx_struct})::Nothing
  return base_ring(x)(r)
end

################################################################################
#
#  Evaluation
#
################################################################################

function evaluate(x::ZZModPolyRingElem, y::ZZModRingElem)
  base_ring(x) != parent(y) && error("Elements must have same parent")
  z = ZZRingElem()
  @ccall libflint.fmpz_mod_poly_evaluate_fmpz(z::Ref{ZZRingElem}, x::Ref{ZZModPolyRingElem}, y.data::Ref{ZZRingElem}, x.parent.base_ring.ninv::Ref{fmpz_mod_ctx_struct})::Nothing
  return parent(y)(z)
end

################################################################################
#
#  Derivative
#
################################################################################

function derivative(x::T) where {T <: Zmodn_fmpz_poly}
  z = parent(x)()
  @ccall libflint.fmpz_mod_poly_derivative(z::Ref{T}, x::Ref{T}, x.parent.base_ring.ninv::Ref{fmpz_mod_ctx_struct})::Nothing
  return z
end

###############################################################################
#
#   Integral
#
###############################################################################

function integral(x::ZZModPolyRingElem)
  len = length(x)
  v = Vector{ZZModRingElem}(undef, len + 1)
  v[1] = zero(base_ring(x))
  for i = 1:len
    v[i + 1] = divexact(coeff(x, i - 1), base_ring(x)(i))
  end
  return parent(x)(v)
end

################################################################################
#
#  Composition
#
################################################################################

function AbstractAlgebra._compose_right(x::T, y::T) where {T <: Zmodn_fmpz_poly}
  check_parent(x, y)
  z = parent(x)()
  @ccall libflint.fmpz_mod_poly_compose(z::Ref{T}, x::Ref{T}, y::Ref{T}, x.parent.base_ring.ninv::Ref{fmpz_mod_ctx_struct})::Nothing
  return z
end

################################################################################
#
#  Lifting
#
################################################################################

@doc raw"""
    lift(R::ZZPolyRing, y::ZZModPolyRingElem)

Lift from a polynomial over $\mathbb{Z}/n\mathbb{Z}$ to a polynomial over
$\mathbb{Z}$ with minimal reduced non-negative coefficients. The ring `R`
specifies the ring to lift into.
"""
function lift(R::ZZPolyRing, y::ZZModPolyRingElem)
  z = ZZPolyRingElem()
  @ccall libflint.fmpz_mod_poly_get_fmpz_poly(z::Ref{ZZPolyRingElem}, y::Ref{ZZModPolyRingElem}, y.parent.base_ring.ninv::Ref{fmpz_mod_ctx_struct})::Nothing
  z.parent = R
  return z
end

################################################################################
#
#  Irreducibility
#
################################################################################

function is_irreducible(x::ZZModPolyRingElem)
  !is_probable_prime(modulus(x)) && error("Modulus not prime in is_irreducible")
  is_constant(x) && return false
  return Bool(@ccall libflint.fmpz_mod_poly_is_irreducible(x::Ref{ZZModPolyRingElem}, x.parent.base_ring.ninv::Ref{fmpz_mod_ctx_struct})::Cint)
end

################################################################################
#
#  Squarefree testing
#
################################################################################

function is_squarefree(x::ZZModPolyRingElem)
  !is_probable_prime(modulus(x)) && error("Modulus not prime in is_squarefree")
  return Bool(@ccall libflint.fmpz_mod_poly_is_squarefree(x::Ref{ZZModPolyRingElem}, x.parent.base_ring.ninv::Ref{fmpz_mod_ctx_struct})::Cint)
end

################################################################################
#
#  Factorization
#
################################################################################

function factor(x::ZZModPolyRingElem)
  iszero(x) && throw(ArgumentError("Argument must be non-zero"))
  !is_probable_prime(modulus(x)) && error("Modulus not prime in factor")
  fac = _factor(x)
  return Fac(parent(x)(leading_coefficient(x)), fac)
end

function _factor(x::ZZModPolyRingElem)
  n = x.parent.base_ring.ninv
  fac = fmpz_mod_poly_factor(n)
  @ccall libflint.fmpz_mod_poly_factor(fac::Ref{fmpz_mod_poly_factor}, x::Ref{ZZModPolyRingElem}, n::Ref{fmpz_mod_ctx_struct})::UInt
  res = Dict{ZZModPolyRingElem, Int}()
  for i in 1:fac.num
    f = parent(x)()
    @ccall libflint.fmpz_mod_poly_factor_get_fmpz_mod_poly(f::Ref{ZZModPolyRingElem}, fac::Ref{fmpz_mod_poly_factor}, (i - 1)::Int, n::Ref{fmpz_mod_ctx_struct})::Nothing
    e = unsafe_load(fac.exp, i)
    res[f] = e
  end
  return res
end

function factor_squarefree(x::ZZModPolyRingElem)
  iszero(x) && throw(ArgumentError("Argument must be non-zero"))
  !is_probable_prime(modulus(x)) && error("Modulus not prime in factor_squarefree")
  fac = _factor_squarefree(x)
  return Fac(parent(x)(leading_coefficient(x)), fac)
end

function _factor_squarefree(x::ZZModPolyRingElem)
  n = x.parent.base_ring.ninv
  fac = fmpz_mod_poly_factor(n)
  @ccall libflint.fmpz_mod_poly_factor_squarefree(fac::Ref{fmpz_mod_poly_factor}, x::Ref{ZZModPolyRingElem}, n::Ref{fmpz_mod_ctx_struct})::UInt
  res = Dict{ZZModPolyRingElem, Int}()
  for i in 1:fac.num
    f = parent(x)()
    @ccall libflint.fmpz_mod_poly_factor_get_fmpz_mod_poly(f::Ref{ZZModPolyRingElem}, fac::Ref{fmpz_mod_poly_factor}, (i - 1)::Int, n::Ref{fmpz_mod_ctx_struct})::Nothing
    e = unsafe_load(fac.exp, i)
    res[f] = e
  end
  return res
end

@doc raw"""
    factor_distinct_deg(x::ZZModPolyRingElem)

Return the distinct degree factorisation of a squarefree polynomial $x$.
"""
function factor_distinct_deg(x::ZZModPolyRingElem)
  !is_squarefree(x) && error("Polynomial must be squarefree")
  !is_probable_prime(modulus(x)) && error("Modulus not prime in factor_distinct_deg")
  degs = Vector{Int}(undef, degree(x))
  degss = [ pointer(degs) ]
  n = x.parent.base_ring.ninv
  fac = fmpz_mod_poly_factor(n)
  @ccall libflint.fmpz_mod_poly_factor_distinct_deg(fac::Ref{fmpz_mod_poly_factor}, x::Ref{ZZModPolyRingElem}, degss::Ptr{Ptr{Int}}, n::Ref{fmpz_mod_ctx_struct})::UInt
  res = Dict{Int, ZZModPolyRingElem}()
  for i in 1:fac.num
    f = parent(x)()
    @ccall libflint.fmpz_mod_poly_factor_get_fmpz_mod_poly(f::Ref{ZZModPolyRingElem}, fac::Ref{fmpz_mod_poly_factor}, (i - 1)::Int, n::Ref{fmpz_mod_ctx_struct})::Nothing
    res[degs[i]] = f
  end
  return res
end

function roots(a::ZZModPolyRingElem)
  R = parent(a)
  n = R.base_ring.ninv
  fac = fmpz_mod_poly_factor(n)
  if is_probable_prime(n.n)
    @ccall libflint.fmpz_mod_poly_roots(fac::Ref{fmpz_mod_poly_factor}, a::Ref{ZZModPolyRingElem}, 0::Cint, n::Ref{fmpz_mod_ctx_struct})::UInt
  else
    nfac = fmpz_factor()
    @ccall libflint.fmpz_factor(nfac::Ref{fmpz_factor}, R.base_ring.n::Ref{ZZRingElem})::Nothing
    @ccall libflint.fmpz_mod_poly_roots_factored(fac::Ref{fmpz_mod_poly_factor}, a::Ref{ZZModPolyRingElem}, 0::Cint, nfac::Ref{fmpz_factor}, n::Ref{fmpz_mod_ctx_struct})::UInt
  end
  f = R()
  res = ZZModRingElem[]
  for i in 1:fac.num
    @ccall libflint.fmpz_mod_poly_factor_get_fmpz_mod_poly(f::Ref{ZZModPolyRingElem}, fac::Ref{fmpz_mod_poly_factor}, (i - 1)::Int, n::Ref{fmpz_mod_ctx_struct})::Nothing
    @assert isone(coeff(f, 1))
    push!(res, -coeff(f, 0))
  end
  return res
end


################################################################################
#
#  Unsafe functions
#
################################################################################

function zero!(x::T) where {T <: Zmodn_fmpz_poly}
  @ccall libflint.fmpz_mod_poly_zero(x::Ref{T}, x.parent.base_ring.ninv::Ref{fmpz_mod_ctx_struct})::Nothing
  return x
end

function one!(x::T) where {T <: Zmodn_fmpz_poly}
  @ccall libflint.fmpz_mod_poly_one(x::Ref{T}, x.parent.base_ring.ninv::Ref{fmpz_mod_ctx_struct})::Nothing
  return x
end

function neg!(z::T, x::T) where {T <: Zmodn_fmpz_poly}
  @ccall libflint.fmpz_mod_poly_neg(z::Ref{T}, x::Ref{T}, x.parent.base_ring.ninv::Ref{fmpz_mod_ctx_struct})::Nothing
  return z
end

function fit!(x::T, n::Int) where {T <: Zmodn_fmpz_poly}
  @ccall libflint.fmpz_mod_poly_fit_length(x::Ref{T}, n::Int, x.parent.base_ring.ninv::Ref{fmpz_mod_ctx_struct})::Nothing
  return nothing
end

function setcoeff!(x::T, n::Int, y::UInt) where {T <: Zmodn_fmpz_poly}
  @ccall libflint.fmpz_mod_poly_set_coeff_ui(x::Ref{T}, n::Int, y::UInt, x.parent.base_ring.ninv::Ref{fmpz_mod_ctx_struct})::Nothing
  return x
end

function setcoeff!(x::T, n::Int, y::Int) where {T <: Zmodn_fmpz_poly}
  @ccall libflint.fmpz_mod_poly_set_coeff_si(x::Ref{T}, n::Int, y::UInt, x.parent.base_ring.ninv::Ref{fmpz_mod_ctx_struct})::Nothing
  return x
end

function setcoeff!(x::T, n::Int, y::ZZRingElem) where {T <: Zmodn_fmpz_poly}
  @ccall libflint.fmpz_mod_poly_set_coeff_fmpz(x::Ref{T}, n::Int, y::Ref{ZZRingElem}, x.parent.base_ring.ninv::Ref{fmpz_mod_ctx_struct})::Nothing
  return x
end

setcoeff!(x::T, n::Int, y::Integer) where {T <: Zmodn_fmpz_poly} = setcoeff!(x, n, ZZRingElem(y))

setcoeff!(x::ZZModPolyRingElem, n::Int, y::ZZModRingElem) = setcoeff!(x, n, y.data)

function add!(z::T, x::T, y::T) where {T <: Zmodn_fmpz_poly}
  @ccall libflint.fmpz_mod_poly_add(z::Ref{T}, x::Ref{T}, y::Ref{T}, x.parent.base_ring.ninv::Ref{fmpz_mod_ctx_struct})::Nothing
  return z
end

function sub!(z::T, x::T, y::T) where {T <: Zmodn_fmpz_poly}
  @ccall libflint.fmpz_mod_poly_sub(z::Ref{T}, x::Ref{T}, y::Ref{T}, x.parent.base_ring.ninv::Ref{fmpz_mod_ctx_struct})::Nothing
  return z
end

function mul!(z::T, x::T, y::T) where {T <: Zmodn_fmpz_poly}
  @ccall libflint.fmpz_mod_poly_mul(z::Ref{T}, x::Ref{T}, y::Ref{T}, x.parent.base_ring.ninv::Ref{fmpz_mod_ctx_struct})::Nothing
  return z
end

#

function reverse!(z::T, x::T, len::Int) where {T <: Zmodn_fmpz_poly}
  @ccall libflint.fmpz_mod_poly_reverse(z::Ref{T}, x::Ref{T}, len::Int, x.parent.base_ring.ninv::Ref{fmpz_mod_ctx_struct})::Nothing
  return z
end

################################################################################
#
#  Promotion rules
#
################################################################################

promote_rule(::Type{T}, ::Type{V}) where {T <: Zmodn_fmpz_poly, V <: Integer} = T

promote_rule(::Type{T}, ::Type{ZZRingElem}) where {T <: Zmodn_fmpz_poly} = T

promote_rule(::Type{ZZModPolyRingElem}, ::Type{ZZModRingElem}) = ZZModPolyRingElem

################################################################################
#
#  Parent object call overloads
#
################################################################################

function (R::ZZModPolyRing)()
  z = ZZModPolyRingElem(base_ring(R))
  z.parent = R
  return z
end

function (R::ZZModPolyRing)(x::ZZRingElem)
  z = ZZModPolyRingElem(base_ring(R), x)
  z.parent = R
  return z
end

function (R::ZZModPolyRing)(x::Integer)
  z = ZZModPolyRingElem(base_ring(R), ZZRingElem(x))
  z.parent = R
  return z
end

function (R::ZZModPolyRing)(x::ZZModRingElem)
  base_ring(R) != parent(x) && error("Wrong parents")
  z = ZZModPolyRingElem(base_ring(R), x.data)
  z.parent = R
  return z
end

function (R::ZZModPolyRing)(arr::Vector{ZZRingElem})
  z = ZZModPolyRingElem(base_ring(R), arr)
  z.parent = R
  return z
end

function (R::ZZModPolyRing)(arr::Vector{ZZModRingElem})
  if length(arr) > 0
    (base_ring(R) != parent(arr[1])) && error("Wrong parents")
  end
  z = ZZModPolyRingElem(base_ring(R), arr)
  z.parent = R
  return z
end

(R::ZZModPolyRing)(arr::Vector{T}) where {T <: Integer} = R(map(base_ring(R), arr))

function (R::ZZModPolyRing)(x::ZZPolyRingElem)
  z = ZZModPolyRingElem(base_ring(R), x)
  z.parent = R
  return z
end

function (R::ZZModPolyRing)(f::ZZModPolyRingElem)
  parent(f) != R && error("Unable to coerce polynomial")
  return f
end

################################################################################
#
#  Rand
#
################################################################################

@doc raw"""
    rand(Rt::PolyRing{T}, n::Int) where T <: ResElem{ZZRingElem} -> PolyRingElem{T}

Return a random polynomial of degree $n$.
"""
function Base.rand(Rt::PolyRing{T}, n::Int) where {T<:ResElem{ZZRingElem}}
  f = Rt()
  R = base_ring(Rt)
  for i = 0:n
    setcoeff!(f, i, rand(R))
  end
  return f
end
