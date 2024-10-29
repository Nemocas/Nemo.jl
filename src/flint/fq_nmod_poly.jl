################################################################################
#
#  fq_nmod_poly.jl: Flint fq_mod_poly (Polynomials over FqNmodFiniteField)
#
################################################################################

################################################################################
#
#  Type and parent object methods
#
################################################################################

parent_type(::Type{fqPolyRepPolyRingElem}) = fqPolyRepPolyRing

elem_type(::Type{fqPolyRepPolyRing}) = fqPolyRepPolyRingElem

dense_poly_type(::Type{fqPolyRepFieldElem}) = fqPolyRepPolyRingElem

base_ring(a::fqPolyRepPolyRing) = a.base_ring

parent(a::fqPolyRepPolyRingElem) = a.parent

var(a::fqPolyRepPolyRing) = a.S

################################################################################
#
#   Basic manipulation
#
################################################################################

length(x::fqPolyRepPolyRingElem) = @ccall libflint.fq_nmod_poly_length(x::Ref{fqPolyRepPolyRingElem})::Int

function set_length!(x::fqPolyRepPolyRingElem, n::Int)
  @ccall libflint._fq_nmod_poly_set_length(x::Ref{fqPolyRepPolyRingElem}, n::Int)::Nothing
  return x
end

function coeff(x::fqPolyRepPolyRingElem, n::Int)
  n < 0 && throw(DomainError(n, "Index must be non-negative"))
  F = (x.parent).base_ring
  temp = F(1)
  @ccall libflint.fq_nmod_poly_get_coeff(temp::Ref{fqPolyRepFieldElem}, x::Ref{fqPolyRepPolyRingElem}, n::Int, F::Ref{fqPolyRepField})::Nothing
  return temp
end

zero(a::fqPolyRepPolyRing) = a(zero(base_ring(a)))

one(a::fqPolyRepPolyRing) = a(one(base_ring(a)))

gen(a::fqPolyRepPolyRing) = a([zero(base_ring(a)), one(base_ring(a))])

iszero(x::fqPolyRepPolyRingElem) = @ccall libflint.fq_nmod_poly_is_zero(x::Ref{fqPolyRepPolyRingElem}, base_ring(x.parent)::Ref{fqPolyRepField})::Bool

isone(x::fqPolyRepPolyRingElem) = @ccall libflint.fq_nmod_poly_is_one(x::Ref{fqPolyRepPolyRingElem}, base_ring(x.parent)::Ref{fqPolyRepField})::Bool

is_gen(x::fqPolyRepPolyRingElem) = @ccall libflint.fq_nmod_poly_is_gen(x::Ref{fqPolyRepPolyRingElem}, base_ring(x.parent)::Ref{fqPolyRepField})::Bool

degree(f::fqPolyRepPolyRingElem) = f.length - 1

function deepcopy_internal(a::fqPolyRepPolyRingElem, dict::IdDict)
  z = fqPolyRepPolyRingElem(a)
  z.parent = a.parent
  return z
end

###############################################################################
#
#   Similar
#
###############################################################################

function similar(f::PolyRingElem, R::fqPolyRepField, s::Symbol=var(parent(f)); cached::Bool=true)
  z = fqPolyRepPolyRingElem()
  if base_ring(f) === R && s == var(parent(f)) && f isa fqPolyRepPolyRingElem
    # steal parent in case it is not cached
    z.parent = parent(f)
  else
    z.parent = fqPolyRepPolyRing(R, s, cached)
  end
  return z
end

###############################################################################
#
#   polynomial constructor
#
###############################################################################

function polynomial(R::fqPolyRepField, arr::Vector{T}, var::VarName=:x; cached::Bool=true) where T
  coeffs = map(R, arr)
  z = length(coeffs) == 0 ? fqPolyRepPolyRingElem() : fqPolyRepPolyRingElem(coeffs)
  z.parent = fqPolyRepPolyRing(R, Symbol(var), cached)
  return z
end

################################################################################
#
#   Canonicalisation
#
################################################################################

canonical_unit(a::fqPolyRepPolyRingElem) = canonical_unit(leading_coefficient(a))

################################################################################
#
#  Unary operations
#
################################################################################

-(x::fqPolyRepPolyRingElem) = neg!(parent(x)(), x)

################################################################################
#
#  Binary operations
#
################################################################################

function +(x::fqPolyRepPolyRingElem, y::fqPolyRepPolyRingElem)
  check_parent(x,y)
  z = parent(x)()
  @ccall libflint.fq_nmod_poly_add(z::Ref{fqPolyRepPolyRingElem}, x::Ref{fqPolyRepPolyRingElem}, y::Ref{fqPolyRepPolyRingElem}, base_ring(parent(x))::Ref{fqPolyRepField})::Nothing
  return z
end

function -(x::fqPolyRepPolyRingElem, y::fqPolyRepPolyRingElem)
  check_parent(x,y)
  z = parent(x)()
  @ccall libflint.fq_nmod_poly_sub(z::Ref{fqPolyRepPolyRingElem}, x::Ref{fqPolyRepPolyRingElem}, y::Ref{fqPolyRepPolyRingElem}, base_ring(parent(x))::Ref{fqPolyRepField})::Nothing
  return z
end

function *(x::fqPolyRepPolyRingElem, y::fqPolyRepPolyRingElem)
  check_parent(x,y)
  z = parent(x)()
  @ccall libflint.fq_nmod_poly_mul(z::Ref{fqPolyRepPolyRingElem}, x::Ref{fqPolyRepPolyRingElem}, y::Ref{fqPolyRepPolyRingElem}, base_ring(parent(x))::Ref{fqPolyRepField})::Nothing
  return z
end

################################################################################
#
#   Ad hoc binary operators
#
################################################################################

function *(x::fqPolyRepFieldElem, y::fqPolyRepPolyRingElem)
  parent(x) != base_ring(parent(y)) &&
  error("Coefficient rings must be equal")
  z = parent(y)()
  @ccall libflint.fq_nmod_poly_scalar_mul_fq_nmod(z::Ref{fqPolyRepPolyRingElem}, y::Ref{fqPolyRepPolyRingElem}, x::Ref{fqPolyRepFieldElem}, parent(x)::Ref{fqPolyRepField})::Nothing
  return z
end

*(x::fqPolyRepPolyRingElem, y::fqPolyRepFieldElem) = y*x

*(x::ZZRingElem, y::fqPolyRepPolyRingElem) = base_ring(parent(y))(x) * y

*(x::fqPolyRepPolyRingElem, y::ZZRingElem) = y*x

*(x::Integer, y::fqPolyRepPolyRingElem) = ZZRingElem(x)*y

*(x::fqPolyRepPolyRingElem, y::Integer) = y*x

+(x::fqPolyRepFieldElem, y::fqPolyRepPolyRingElem) = parent(y)(x) + y

+(x::fqPolyRepPolyRingElem, y::fqPolyRepFieldElem) = y + x

+(x::ZZRingElem, y::fqPolyRepPolyRingElem) = base_ring(parent(y))(x) + y

+(x::fqPolyRepPolyRingElem, y::ZZRingElem) = y + x

+(x::fqPolyRepPolyRingElem, y::Integer) = x + ZZRingElem(y)

+(x::Integer, y::fqPolyRepPolyRingElem) = y + x

-(x::fqPolyRepFieldElem, y::fqPolyRepPolyRingElem) = parent(y)(x) - y

-(x::fqPolyRepPolyRingElem, y::fqPolyRepFieldElem) = x - parent(x)(y)

-(x::ZZRingElem, y::fqPolyRepPolyRingElem) = base_ring(parent(y))(x) - y

-(x::fqPolyRepPolyRingElem, y::ZZRingElem) = x - base_ring(parent(x))(y)

-(x::fqPolyRepPolyRingElem, y::Integer) = x - ZZRingElem(y)

-(x::Integer, y::fqPolyRepPolyRingElem) = ZZRingElem(x) - y

################################################################################
#
#   Powering
#
################################################################################

function ^(x::fqPolyRepPolyRingElem, y::Int)
  y < 0 && throw(DomainError(y, "Exponent must be non-negative"))
  z = parent(x)()
  @ccall libflint.fq_nmod_poly_pow(z::Ref{fqPolyRepPolyRingElem}, x::Ref{fqPolyRepPolyRingElem}, y::Int, base_ring(parent(x))::Ref{fqPolyRepField})::Nothing
  return z
end

################################################################################
#
#   Comparisons
#
################################################################################

function ==(x::fqPolyRepPolyRingElem, y::fqPolyRepPolyRingElem)
  check_parent(x,y)
  r = @ccall libflint.fq_nmod_poly_equal(x::Ref{fqPolyRepPolyRingElem}, y::Ref{fqPolyRepPolyRingElem}, base_ring(parent(x))::Ref{fqPolyRepField})::Cint
  return Bool(r)
end

################################################################################
#
#   Ad hoc comparisons
#
################################################################################

function ==(x::fqPolyRepPolyRingElem, y::fqPolyRepFieldElem)
  base_ring(parent(x)) != parent(y) && return false
  if length(x) > 1
    return false
  elseif length(x) == 1
    r = @ccall libflint.fq_nmod_poly_equal_fq_nmod(x::Ref{fqPolyRepPolyRingElem}, y::Ref{fqPolyRepFieldElem}, base_ring(parent(x))::Ref{fqPolyRepField})::Cint
    return Bool(r)
  else
    return iszero(y)
  end
end

==(x::fqPolyRepFieldElem, y::fqPolyRepPolyRingElem) = y == x

==(x::fqPolyRepPolyRingElem, y::ZZRingElem) = x == base_ring(parent(x))(y)

==(x::ZZRingElem, y::fqPolyRepPolyRingElem) = y == x

==(x::fqPolyRepPolyRingElem, y::Integer) = x == ZZRingElem(y)

==(x::Integer, y::fqPolyRepPolyRingElem) = y == x

################################################################################
#
#   Truncation
#
################################################################################

function truncate(x::fqPolyRepPolyRingElem, n::Int)
  n < 0 && throw(DomainError(n, "Index must be non-negative"))
  if length(x) <= n
    return x
  end
  z = parent(x)()
  @ccall libflint.fq_nmod_poly_set_trunc(z::Ref{fqPolyRepPolyRingElem}, x::Ref{fqPolyRepPolyRingElem}, n::Int, base_ring(parent(x))::Ref{fqPolyRepField})::Nothing
  return z
end

function mullow(x::fqPolyRepPolyRingElem, y::fqPolyRepPolyRingElem, n::Int)
  check_parent(x,y)
  n < 0 && throw(DomainError(n, "Index must be non-negative"))
  z = parent(x)()
  @ccall libflint.fq_nmod_poly_mullow(z::Ref{fqPolyRepPolyRingElem}, x::Ref{fqPolyRepPolyRingElem}, y::Ref{fqPolyRepPolyRingElem}, n::Int, base_ring(parent(x))::Ref{fqPolyRepField})::Nothing
  return z
end

################################################################################
#
#   Reversal
#
################################################################################

function reverse(x::fqPolyRepPolyRingElem, len::Int)
  len < 0 && throw(DomainError(len, "Index must be non-negative"))
  z = parent(x)()
  @ccall libflint.fq_nmod_poly_reverse(z::Ref{fqPolyRepPolyRingElem}, x::Ref{fqPolyRepPolyRingElem}, len::Int, base_ring(parent(x))::Ref{fqPolyRepField})::Nothing
  return z
end

################################################################################
#
#   Shifting
#
################################################################################

function shift_left(x::fqPolyRepPolyRingElem, len::Int)
  len < 0 && throw(DomainError(len, "Shift must be non-negative"))
  z = parent(x)()
  @ccall libflint.fq_nmod_poly_shift_left(z::Ref{fqPolyRepPolyRingElem}, x::Ref{fqPolyRepPolyRingElem}, len::Int, base_ring(parent(x))::Ref{fqPolyRepField})::Nothing
  return z
end

function shift_right(x::fqPolyRepPolyRingElem, len::Int)
  len < 0 && throw(DomainError(len, "Shift must be non-negative"))
  z = parent(x)()
  @ccall libflint.fq_nmod_poly_shift_right(z::Ref{fqPolyRepPolyRingElem}, x::Ref{fqPolyRepPolyRingElem}, len::Int, base_ring(parent(x))::Ref{fqPolyRepField})::Nothing
  return z
end

################################################################################
#
#   Euclidean division
#
################################################################################

function Base.div(x::fqPolyRepPolyRingElem, y::fqPolyRepPolyRingElem)
  check_parent(x,y)
  z = parent(x)()
  @ccall libflint.fq_nmod_poly_div_basecase(z::Ref{fqPolyRepPolyRingElem}, x::Ref{fqPolyRepPolyRingElem}, y::Ref{fqPolyRepPolyRingElem}, base_ring(parent(x))::Ref{fqPolyRepField})::Nothing
  return z
end

function rem(x::fqPolyRepPolyRingElem, y::fqPolyRepPolyRingElem)
  check_parent(x,y)
  z = parent(x)()
  rem!(z, x, y)
  return z
end

function rem!(z::fqPolyRepPolyRingElem, x::fqPolyRepPolyRingElem, y::fqPolyRepPolyRingElem)
  @ccall libflint.fq_nmod_poly_rem(z::Ref{fqPolyRepPolyRingElem}, x::Ref{fqPolyRepPolyRingElem}, y::Ref{fqPolyRepPolyRingElem}, base_ring(parent(x))::Ref{fqPolyRepField})::Nothing
  return z
end

mod(x::fqPolyRepPolyRingElem, y::fqPolyRepPolyRingElem) = rem(x, y)

function Base.divrem(x::fqPolyRepPolyRingElem, y::fqPolyRepPolyRingElem)
  check_parent(x,y)
  z = parent(x)()
  r = parent(x)()
  @ccall libflint.fq_nmod_poly_divrem(z::Ref{fqPolyRepPolyRingElem}, r::Ref{fqPolyRepPolyRingElem}, x::Ref{fqPolyRepPolyRingElem}, y::Ref{fqPolyRepPolyRingElem}, base_ring(parent(x))::Ref{fqPolyRepField})::Nothing
  return z,r
end

################################################################################
#
#  Square root
#
################################################################################

function sqrt(x::fqPolyRepPolyRingElem; check::Bool=true)
  R = parent(x)
  s = R()
  flag = Bool(@ccall libflint.fq_nmod_poly_sqrt(s::Ref{fqPolyRepPolyRingElem}, x::Ref{fqPolyRepPolyRingElem}, base_ring(parent(x))::Ref{fqPolyRepField})::Cint)
  check && !flag && error("Not a square in sqrt")
  return s
end

function is_square(x::fqPolyRepPolyRingElem)
  if iszero(x)
    return true
  end
  if !iseven(degree(x))
    return false
  end
  R = parent(x)
  s = R()
  flag = Bool(@ccall libflint.fq_nmod_poly_sqrt(s::Ref{fqPolyRepPolyRingElem}, x::Ref{fqPolyRepPolyRingElem}, base_ring(parent(x))::Ref{fqPolyRepField})::Cint)
  return flag
end

function is_square_with_sqrt(x::fqPolyRepPolyRingElem)
  R = parent(x)
  if iszero(x)
    return true, zero(R)
  end
  if !iseven(degree(x))
    return false, zero(R)
  end
  s = R()
  flag = Bool(@ccall libflint.fq_nmod_poly_sqrt(s::Ref{fqPolyRepPolyRingElem}, x::Ref{fqPolyRepPolyRingElem}, base_ring(parent(x))::Ref{fqPolyRepField})::Cint)
  return flag, s
end

################################################################################
#
#   Remove and valuation
#
################################################################################

function remove(z::fqPolyRepPolyRingElem, p::fqPolyRepPolyRingElem)
  ok, v = _remove_check_simple_cases(z, p)
  ok && return v, zero(parent(z))
  z = deepcopy(z)
  v = @ccall libflint.fq_nmod_poly_remove(z::Ref{fqPolyRepPolyRingElem}, p::Ref{fqPolyRepPolyRingElem}, base_ring(parent(z))::Ref{fqPolyRepField})::Int
  return v, z
end

function divides(z::fqPolyRepPolyRingElem, x::fqPolyRepPolyRingElem)
  check_parent(z, x)
  if iszero(z)
    return true, zero(parent(z))
  end
  if iszero(x)
    return false, zero(parent(z))
  end
  q = parent(z)()
  v = Bool(@ccall libflint.fq_nmod_poly_divides(q::Ref{fqPolyRepPolyRingElem}, z::Ref{fqPolyRepPolyRingElem}, x::Ref{fqPolyRepPolyRingElem}, base_ring(parent(z))::Ref{fqPolyRepField})::Cint)
  return v, q
end

################################################################################
#
#   Modular arithmetic
#
################################################################################

function powermod(x::fqPolyRepPolyRingElem, n::Int, y::fqPolyRepPolyRingElem)
  check_parent(x,y)
  z = parent(x)()

  if n < 0
    g, x = gcdinv(x, y)
    if !isone(g)
      error("Element not invertible")
    end
    n = -n
  end

  @ccall libflint.fq_nmod_poly_powmod_ui_binexp(z::Ref{fqPolyRepPolyRingElem}, x::Ref{fqPolyRepPolyRingElem}, n::Int, y::Ref{fqPolyRepPolyRingElem}, base_ring(parent(x))::Ref{fqPolyRepField})::Nothing
  return z
end

function powermod(x::fqPolyRepPolyRingElem, n::ZZRingElem, y::fqPolyRepPolyRingElem)
  check_parent(x,y)
  z = parent(x)()

  if n < 0
    g, x = gcdinv(x, y)
    if !isone(g)
      error("Element not invertible")
    end
    n = -n
  end

  @ccall libflint.fq_nmod_poly_powmod_fmpz_binexp(z::Ref{fqPolyRepPolyRingElem}, x::Ref{fqPolyRepPolyRingElem}, n::Ref{ZZRingElem}, y::Ref{fqPolyRepPolyRingElem}, base_ring(parent(x))::Ref{fqPolyRepField})::Nothing
  return z
end

################################################################################
#
#   GCD
#
################################################################################

function gcd(x::fqPolyRepPolyRingElem, y::fqPolyRepPolyRingElem)
  check_parent(x,y)
  z = parent(x)()
  @ccall libflint.fq_nmod_poly_gcd(z::Ref{fqPolyRepPolyRingElem}, x::Ref{fqPolyRepPolyRingElem}, y::Ref{fqPolyRepPolyRingElem}, base_ring(parent(x))::Ref{fqPolyRepField})::Nothing
  return z
end

function gcdx(x::fqPolyRepPolyRingElem, y::fqPolyRepPolyRingElem)
  check_parent(x,y)
  z = parent(x)()
  s = parent(x)()
  t = parent(x)()
  @ccall libflint.fq_nmod_poly_xgcd(z::Ref{fqPolyRepPolyRingElem}, s::Ref{fqPolyRepPolyRingElem}, t::Ref{fqPolyRepPolyRingElem}, x::Ref{fqPolyRepPolyRingElem}, y::Ref{fqPolyRepPolyRingElem}, base_ring(parent(x))::Ref{fqPolyRepField})::Nothing
  return z, s, t
end

################################################################################
#
#   Evaluation
#
################################################################################

function evaluate(x::fqPolyRepPolyRingElem, y::fqPolyRepFieldElem)
  base_ring(parent(x)) != parent(y) && error("Incompatible coefficient rings")
  z = parent(y)()
  @ccall libflint.fq_nmod_poly_evaluate_fq_nmod(z::Ref{fqPolyRepFieldElem}, x::Ref{fqPolyRepPolyRingElem}, y::Ref{fqPolyRepFieldElem}, base_ring(parent(x))::Ref{fqPolyRepField})::Nothing
  return z
end

################################################################################
#
#   Composition
#
################################################################################

function AbstractAlgebra._compose_right(x::fqPolyRepPolyRingElem, y::fqPolyRepPolyRingElem)
  check_parent(x,y)
  z = parent(x)()
  @ccall libflint.fq_nmod_poly_compose(z::Ref{fqPolyRepPolyRingElem}, x::Ref{fqPolyRepPolyRingElem}, y::Ref{fqPolyRepPolyRingElem}, base_ring(parent(x))::Ref{fqPolyRepField})::Nothing
  return z
end

################################################################################
#
#   Derivative
#
################################################################################

function derivative(x::fqPolyRepPolyRingElem)
  z = parent(x)()
  @ccall libflint.fq_nmod_poly_derivative(z::Ref{fqPolyRepPolyRingElem}, x::Ref{fqPolyRepPolyRingElem}, base_ring(parent(x))::Ref{fqPolyRepField})::Nothing
  return z
end

################################################################################
#
#  Inflation and deflation
#
################################################################################

function inflate(x::fqPolyRepPolyRingElem, n::Int)
  z = parent(x)()
  @ccall libflint.fq_nmod_poly_inflate(z::Ref{fqPolyRepPolyRingElem}, x::Ref{fqPolyRepPolyRingElem}, UInt(n)::Culong, base_ring(parent(x))::Ref{fqPolyRepField})::Nothing
  return z
end

function deflate(x::fqPolyRepPolyRingElem, n::Int)
  z = parent(x)()
  @ccall libflint.fq_nmod_poly_deflate(z::Ref{fqPolyRepPolyRingElem}, x::Ref{fqPolyRepPolyRingElem}, UInt(n)::Culong, base_ring(parent(x))::Ref{fqPolyRepField})::Nothing
  return z
end

################################################################################
#
#  Irreducibility
#
################################################################################

function is_irreducible(x::fqPolyRepPolyRingElem)
  return Bool(@ccall libflint.fq_nmod_poly_is_irreducible(x::Ref{fqPolyRepPolyRingElem}, base_ring(parent(x))::Ref{fqPolyRepField})::Int32)
end

################################################################################
#
#  Squarefree testing
#
################################################################################

function is_squarefree(x::fqPolyRepPolyRingElem)
  return Bool(@ccall libflint.fq_nmod_poly_is_squarefree(x::Ref{fqPolyRepPolyRingElem}, base_ring(parent(x))::Ref{fqPolyRepField})::Int32)
end

################################################################################
#
#  Factorization
#
################################################################################

function factor(x::fqPolyRepPolyRingElem)
  iszero(x) && throw(ArgumentError("Argument must be non-zero"))
  res, z = _factor(x)
  return Fac(parent(x)(z), res)
end

function _factor(x::fqPolyRepPolyRingElem)
  R = parent(x)
  F = base_ring(R)
  a = F()
  fac = fq_nmod_poly_factor(F)
  @ccall libflint.fq_nmod_poly_factor(fac::Ref{fq_nmod_poly_factor}, a::Ref{fqPolyRepFieldElem}, x::Ref{fqPolyRepPolyRingElem}, F::Ref{fqPolyRepField})::Nothing
  res = Dict{fqPolyRepPolyRingElem,Int}()
  for i in 1:fac.num
    f = R()
    @ccall libflint.fq_nmod_poly_factor_get_poly(f::Ref{fqPolyRepPolyRingElem}, fac::Ref{fq_nmod_poly_factor}, (i-1)::Int, F::Ref{fqPolyRepField})::Nothing
    e = unsafe_load(fac.exp,i)
    res[f] = e
  end
  return res, a
end

function factor_squarefree(x::fqPolyRepPolyRingElem)
  iszero(x) && throw(ArgumentError("Argument must be non-zero"))
  # _factor_squareefree does weird things if the polynomial is not monic
  return Fac(parent(x)(leading_coefficient(x)),
             _factor_squarefree(divexact(x, leading_coefficient(x))))
end

function _factor_squarefree(x::fqPolyRepPolyRingElem)
  F = base_ring(parent(x))
  fac = fq_nmod_poly_factor(F)
  @ccall libflint.fq_nmod_poly_factor_squarefree(fac::Ref{fq_nmod_poly_factor}, x::Ref{fqPolyRepPolyRingElem}, F::Ref{fqPolyRepField})::UInt
  res = Dict{fqPolyRepPolyRingElem,Int}()
  for i in 1:fac.num
    f = parent(x)()
    @ccall libflint.fq_nmod_poly_factor_get_poly(f::Ref{fqPolyRepPolyRingElem}, fac::Ref{fq_nmod_poly_factor}, (i-1)::Int, F::Ref{fqPolyRepField})::Nothing
    e = unsafe_load(fac.exp, i)
    res[f] = e
  end
  return res
end

@doc raw"""
    factor_distinct_deg(x::fqPolyRepPolyRingElem)

Return the distinct degree factorisation of a squarefree polynomial $x$.
"""
function factor_distinct_deg(x::fqPolyRepPolyRingElem)
  R = parent(x)
  F = base_ring(R)
  fac = fq_nmod_poly_factor(F)
  degrees = Vector{Int}(undef, degree(x))
  @ccall libflint.fq_nmod_poly_factor_distinct_deg(fac::Ref{fq_nmod_poly_factor}, x::Ref{fqPolyRepPolyRingElem}, degrees::Ref{Vector{Int}}, F::Ref{fqPolyRepField})::Nothing
  res = Dict{Int, fqPolyRepPolyRingElem}()
  for i in 1:fac.num
    f = R()
    @ccall libflint.fq_nmod_poly_factor_get_poly(f::Ref{fqPolyRepPolyRingElem}, fac::Ref{fq_nmod_poly_factor}, (i-1)::Int, F::Ref{fqPolyRepField})::Nothing
    res[degrees[i]] = f
  end
  return res
end

function roots(x::fqPolyRepPolyRingElem)
  R = parent(x)
  F = base_ring(R)
  fac = fq_nmod_poly_factor(F)
  @ccall libflint.fq_nmod_poly_roots(fac::Ref{fq_nmod_poly_factor}, x::Ref{fqPolyRepPolyRingElem}, 0::Cint, F::Ref{fqPolyRepField})::Nothing
  res = fqPolyRepFieldElem[]
  for i in 1:fac.num
    f = R()
    @ccall libflint.fq_nmod_poly_factor_get_poly(f::Ref{fqPolyRepPolyRingElem}, fac::Ref{fq_nmod_poly_factor}, (i-1)::Int, F::Ref{fqPolyRepField})::Nothing
    @assert isone(coeff(f, 1))
    push!(res, -coeff(f, 0))
  end
  return res
end

################################################################################
#
#   Unsafe functions
#
################################################################################

function zero!(z::fqPolyRepPolyRingElem)
  @ccall libflint.fq_nmod_poly_zero(z::Ref{fqPolyRepPolyRingElem}, base_ring(parent(z))::Ref{fqPolyRepField})::Nothing
  return z
end

function one!(z::fqPolyRepPolyRingElem)
  @ccall libflint.fq_nmod_poly_one(z::Ref{fqPolyRepPolyRingElem}, base_ring(parent(z))::Ref{fqPolyRepField})::Nothing
  return z
end

function neg!(z::fqPolyRepPolyRingElem, a::fqPolyRepPolyRingElem)
  @ccall libflint.fq_nmod_poly_neg(z::Ref{fqPolyRepPolyRingElem}, a::Ref{fqPolyRepPolyRingElem}, base_ring(parent(a))::Ref{fqPolyRepField})::Nothing
  return z
end

function fit!(z::fqPolyRepPolyRingElem, n::Int)
  @ccall libflint.fq_nmod_poly_fit_length(z::Ref{fqPolyRepPolyRingElem}, n::Int, base_ring(parent(z))::Ref{fqPolyRepField})::Nothing
  return nothing
end

function setcoeff!(z::fqPolyRepPolyRingElem, n::Int, x::fqPolyRepFieldElem)
  @ccall libflint.fq_nmod_poly_set_coeff(z::Ref{fqPolyRepPolyRingElem}, n::Int, x::Ref{fqPolyRepFieldElem}, base_ring(parent(z))::Ref{fqPolyRepField})::Nothing
  return z
end

function setcoeff!(z::fqPolyRepPolyRingElem, n::Int, x::ZZRingElem)
  @ccall libflint.fq_nmod_poly_set_coeff_fmpz(z::Ref{fqPolyRepPolyRingElem}, n::Int, x::Ref{ZZRingElem}, base_ring(parent(z))::Ref{fqPolyRepField})::Nothing
  return z
end

function mul!(z::fqPolyRepPolyRingElem, x::fqPolyRepPolyRingElem, y::fqPolyRepPolyRingElem)
  @ccall libflint.fq_nmod_poly_mul(z::Ref{fqPolyRepPolyRingElem}, x::Ref{fqPolyRepPolyRingElem}, y::Ref{fqPolyRepPolyRingElem}, base_ring(parent(x))::Ref{fqPolyRepField})::Nothing
  return z
end

function add!(z::fqPolyRepPolyRingElem, x::fqPolyRepPolyRingElem, y::fqPolyRepPolyRingElem)
  @ccall libflint.fq_nmod_poly_add(z::Ref{fqPolyRepPolyRingElem}, x::Ref{fqPolyRepPolyRingElem}, y::Ref{fqPolyRepPolyRingElem}, base_ring(parent(x))::Ref{fqPolyRepField})::Nothing
  return z
end

function sub!(z::fqPolyRepPolyRingElem, x::fqPolyRepPolyRingElem, y::fqPolyRepPolyRingElem)
  @ccall libflint.fq_nmod_poly_sub(z::Ref{fqPolyRepPolyRingElem}, x::Ref{fqPolyRepPolyRingElem}, y::Ref{fqPolyRepPolyRingElem}, base_ring(parent(x))::Ref{fqPolyRepField})::Nothing
  return z
end

################################################################################
#
#  Promotion rules
#
################################################################################

promote_rule(::Type{fqPolyRepPolyRingElem}, ::Type{V}) where {V <: Integer} = fqPolyRepPolyRingElem

promote_rule(::Type{fqPolyRepPolyRingElem}, ::Type{ZZRingElem}) = fqPolyRepPolyRingElem

promote_rule(::Type{fqPolyRepPolyRingElem}, ::Type{fqPolyRepFieldElem}) = fqPolyRepPolyRingElem

###############################################################################
#
#   Polynomial substitution
#
###############################################################################

function (f::fqPolyRepPolyRingElem)(a::fqPolyRepFieldElem)
  if parent(a) != base_ring(f)
    return subst(f, a)
  end
  return evaluate(f, a)
end

################################################################################
#
#   Parent object call overloads
#
################################################################################

function (R::fqPolyRepPolyRing)()
  z = fqPolyRepPolyRingElem()
  z.parent = R
  return z
end

function (R::fqPolyRepPolyRing)(x::fqPolyRepFieldElem)
  parent(x) !== base_ring(R) && error("Element not contained in coefficient ring")
  z = fqPolyRepPolyRingElem(x)
  z.parent = R
  return z
end

function (R::fqPolyRepPolyRing)(x::ZZRingElem)
  return R(base_ring(R)(x))
end

function (R::fqPolyRepPolyRing)(x::Integer)
  return R(ZZRingElem(x))
end

function (R::fqPolyRepPolyRing)(x::Vector{fqPolyRepFieldElem})
  length(x) == 0 && return zero(R)
  base_ring(R) != parent(x[1]) && error("Coefficient rings must coincide")
  z = fqPolyRepPolyRingElem(x)
  z.parent = R
  return z
end

function (R::fqPolyRepPolyRing)(x::Vector{ZZRingElem})
  length(x) == 0 && return zero(R)
  z = fqPolyRepPolyRingElem(x, base_ring(R))
  z.parent = R
  return z
end

function (R::fqPolyRepPolyRing)(x::Vector{T}) where {T <: Integer}
  length(x) == 0 && return zero(R)
  return R(map(ZZRingElem, x))
end

function (R::fqPolyRepPolyRing)(x::ZZPolyRingElem)
  z = fqPolyRepPolyRingElem(x, base_ring(R))
  z.parent = R
  return z
end

function (R::fqPolyRepPolyRing)(x::fqPolyRepPolyRingElem)
  parent(x) != R && error("Unable to coerce to polynomial")
  return x
end
