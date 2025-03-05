################################################################################
#
#  fq_poly.jl: FLINT fq_poly (Polynomials over FqFiniteField)
#
################################################################################

################################################################################
#
#  Type and parent object methods
#
################################################################################

parent_type(::Type{FqPolyRepPolyRingElem}) = FqPolyRepPolyRing

elem_type(::Type{FqPolyRepPolyRing}) = FqPolyRepPolyRingElem

dense_poly_type(::Type{FqPolyRepFieldElem}) = FqPolyRepPolyRingElem

base_ring(a::FqPolyRepPolyRing) = a.base_ring

parent(a::FqPolyRepPolyRingElem) = a.parent

var(a::FqPolyRepPolyRing) = a.S

################################################################################
#
#   Basic manipulation
#
################################################################################

length(x::FqPolyRepPolyRingElem) = x.length

function coeff(x::FqPolyRepPolyRingElem, n::Int)
  n < 0 && throw(DomainError(n, "Index must be non-negative"))
  F = (x.parent).base_ring
  temp = F(1)
  @ccall libflint.fq_poly_get_coeff(temp::Ref{FqPolyRepFieldElem}, x::Ref{FqPolyRepPolyRingElem}, n::Int, F::Ref{FqPolyRepField})::Nothing
  return temp
end

function set_length!(x::FqPolyRepPolyRingElem, n::Int)
  @ccall libflint._fq_poly_set_length(x::Ref{FqPolyRepPolyRingElem}, n::Int)::Nothing
  return x
end

zero(a::FqPolyRepPolyRing) = a(zero(base_ring(a)))

one(a::FqPolyRepPolyRing) = a(one(base_ring(a)))

gen(a::FqPolyRepPolyRing) = a([zero(base_ring(a)), one(base_ring(a))])

is_gen(x::FqPolyRepPolyRingElem) = @ccall libflint.fq_poly_is_gen(x::Ref{FqPolyRepPolyRingElem}, base_ring(x.parent)::Ref{FqPolyRepField})::Bool

isone(x::FqPolyRepPolyRingElem) = @ccall libflint.fq_poly_is_one(x::Ref{FqPolyRepPolyRingElem}, base_ring(x.parent)::Ref{FqPolyRepField})::Bool

function deepcopy_internal(a::FqPolyRepPolyRingElem, dict::IdDict)
  z = FqPolyRepPolyRingElem(a)
  z.parent = a.parent
  return z
end

###############################################################################
#
#   Similar
#
###############################################################################

function similar(f::PolyRingElem, R::FqPolyRepField, s::Symbol=var(parent(f)); cached::Bool=true)
  z = FqPolyRepPolyRingElem()
  if base_ring(f) === R && s == var(parent(f)) && f isa FqPolyRepPolyRingElem
    # steal parent in case it is not cached
    z.parent = parent(f)
  else
    z.parent = FqPolyRepPolyRing(R, s, cached)
  end
  return z
end

###############################################################################
#
#   polynomial constructor
#
###############################################################################

function polynomial(R::FqPolyRepField, arr::Vector{T}, var::VarName=:x; cached::Bool=true) where T
  coeffs = map(R, arr)
  z = length(coeffs) == 0 ? FqPolyRepPolyRingElem() : FqPolyRepPolyRingElem(coeffs)
  z.parent = FqPolyRepPolyRing(R, Symbol(var), cached)
  return z
end

################################################################################
#
#   Canonicalisation
#
################################################################################

canonical_unit(a::FqPolyRepPolyRingElem) = canonical_unit(leading_coefficient(a))

################################################################################
#
#  Unary operations
#
################################################################################

-(x::FqPolyRepPolyRingElem) = neg!(parent(x)(), x)

################################################################################
#
#  Binary operations
#
################################################################################

function +(x::FqPolyRepPolyRingElem, y::FqPolyRepPolyRingElem)
  check_parent(x,y)
  z = parent(x)()
  @ccall libflint.fq_poly_add(z::Ref{FqPolyRepPolyRingElem}, x::Ref{FqPolyRepPolyRingElem}, y::Ref{FqPolyRepPolyRingElem}, base_ring(parent(x))::Ref{FqPolyRepField})::Nothing
  return z
end

function -(x::FqPolyRepPolyRingElem, y::FqPolyRepPolyRingElem)
  check_parent(x,y)
  z = parent(x)()
  @ccall libflint.fq_poly_sub(z::Ref{FqPolyRepPolyRingElem}, x::Ref{FqPolyRepPolyRingElem}, y::Ref{FqPolyRepPolyRingElem}, base_ring(parent(x))::Ref{FqPolyRepField})::Nothing
  return z
end

function *(x::FqPolyRepPolyRingElem, y::FqPolyRepPolyRingElem)
  check_parent(x,y)
  z = parent(x)()
  @ccall libflint.fq_poly_mul(z::Ref{FqPolyRepPolyRingElem}, x::Ref{FqPolyRepPolyRingElem}, y::Ref{FqPolyRepPolyRingElem}, base_ring(parent(x))::Ref{FqPolyRepField})::Nothing
  return z
end

################################################################################
#
#   Ad hoc binary operators
#
################################################################################

function *(x::FqPolyRepFieldElem, y::FqPolyRepPolyRingElem)
  parent(x) != base_ring(parent(y)) &&
  error("Coefficient rings must be equal")
  z = parent(y)()
  @ccall libflint.fq_poly_scalar_mul_fq(z::Ref{FqPolyRepPolyRingElem}, y::Ref{FqPolyRepPolyRingElem}, x::Ref{FqPolyRepFieldElem}, parent(x)::Ref{FqPolyRepField})::Nothing
  return z
end

*(x::FqPolyRepPolyRingElem, y::FqPolyRepFieldElem) = y*x

*(x::ZZRingElem, y::FqPolyRepPolyRingElem) = base_ring(parent(y))(x) * y

*(x::FqPolyRepPolyRingElem, y::ZZRingElem) = y*x

*(x::Integer, y::FqPolyRepPolyRingElem) = ZZRingElem(x)*y

*(x::FqPolyRepPolyRingElem, y::Integer) = y*x

+(x::FqPolyRepFieldElem, y::FqPolyRepPolyRingElem) = parent(y)(x) + y

+(x::FqPolyRepPolyRingElem, y::FqPolyRepFieldElem) = y + x

+(x::ZZRingElem, y::FqPolyRepPolyRingElem) = base_ring(parent(y))(x) + y

+(x::FqPolyRepPolyRingElem, y::ZZRingElem) = y + x

+(x::FqPolyRepPolyRingElem, y::Integer) = x + ZZRingElem(y)

+(x::Integer, y::FqPolyRepPolyRingElem) = y + x

-(x::FqPolyRepFieldElem, y::FqPolyRepPolyRingElem) = parent(y)(x) - y

-(x::FqPolyRepPolyRingElem, y::FqPolyRepFieldElem) = x - parent(x)(y)

-(x::ZZRingElem, y::FqPolyRepPolyRingElem) = base_ring(parent(y))(x) - y

-(x::FqPolyRepPolyRingElem, y::ZZRingElem) = x - base_ring(parent(x))(y)

-(x::FqPolyRepPolyRingElem, y::Integer) = x - ZZRingElem(y)

-(x::Integer, y::FqPolyRepPolyRingElem) = ZZRingElem(x) - y

################################################################################
#
#   Powering
#
################################################################################

function ^(x::FqPolyRepPolyRingElem, y::Int)
  y < 0 && throw(DomainError(y, "Exponent must be non-negative"))
  z = parent(x)()
  @ccall libflint.fq_poly_pow(z::Ref{FqPolyRepPolyRingElem}, x::Ref{FqPolyRepPolyRingElem}, y::Int, base_ring(parent(x))::Ref{FqPolyRepField})::Nothing
  return z
end

################################################################################
#
#   Comparisons
#
################################################################################

function ==(x::FqPolyRepPolyRingElem, y::FqPolyRepPolyRingElem)
  check_parent(x,y)
  r = @ccall libflint.fq_poly_equal(x::Ref{FqPolyRepPolyRingElem}, y::Ref{FqPolyRepPolyRingElem}, base_ring(parent(x))::Ref{FqPolyRepField})::Cint
  return Bool(r)
end

################################################################################
#
#   Ad hoc comparisons
#
################################################################################

function ==(x::FqPolyRepPolyRingElem, y::FqPolyRepFieldElem)
  base_ring(parent(x)) != parent(y) && return false
  if length(x) > 1
    return false
  elseif length(x) == 1
    r = @ccall libflint.fq_poly_equal_fq(x::Ref{FqPolyRepPolyRingElem}, y::Ref{FqPolyRepFieldElem}, base_ring(parent(x))::Ref{FqPolyRepField})::Cint
    return Bool(r)
  else
    return iszero(y)
  end
end

==(x::FqPolyRepFieldElem, y::FqPolyRepPolyRingElem) = y == x

==(x::FqPolyRepPolyRingElem, y::ZZRingElem) = x == base_ring(parent(x))(y)

==(x::ZZRingElem, y::FqPolyRepPolyRingElem) = y == x

==(x::FqPolyRepPolyRingElem, y::Integer) = x == ZZRingElem(y)

==(x::Integer, y::FqPolyRepPolyRingElem) = y == x

################################################################################
#
#   Truncation
#
################################################################################

function truncate(x::FqPolyRepPolyRingElem, n::Int)
  n < 0 && throw(DomainError(n, "Index must be non-negative"))
  if length(x) <= n
    return x
  end
  z = parent(x)()
  @ccall libflint.fq_poly_set_trunc(z::Ref{FqPolyRepPolyRingElem}, x::Ref{FqPolyRepPolyRingElem}, n::Int, base_ring(parent(x))::Ref{FqPolyRepField})::Nothing
  return z
end

function mullow(x::FqPolyRepPolyRingElem, y::FqPolyRepPolyRingElem, n::Int)
  check_parent(x,y)
  n < 0 && throw(DomainError(n, "Index must be non-negative"))
  z = parent(x)()
  @ccall libflint.fq_poly_mullow(z::Ref{FqPolyRepPolyRingElem}, x::Ref{FqPolyRepPolyRingElem}, y::Ref{FqPolyRepPolyRingElem}, n::Int, base_ring(parent(x))::Ref{FqPolyRepField})::Nothing
  return z
end

################################################################################
#
#   Reversal
#
################################################################################

function reverse(x::FqPolyRepPolyRingElem, len::Int)
  len < 0 && throw(DomainError(len, "Index must be non-negative"))
  return reverse!(parent(x)(), x, len)
end

################################################################################
#
#   Shifting
#
################################################################################

function shift_left(x::FqPolyRepPolyRingElem, len::Int)
  len < 0 && throw(DomainError(len, "Shift must be non-negative"))
  z = parent(x)()
  @ccall libflint.fq_poly_shift_left(z::Ref{FqPolyRepPolyRingElem}, x::Ref{FqPolyRepPolyRingElem}, len::Int, base_ring(parent(x))::Ref{FqPolyRepField})::Nothing
  return z
end

function shift_right(x::FqPolyRepPolyRingElem, len::Int)
  len < 0 && throw(DomainError(len, "Shift must be non-negative"))
  z = parent(x)()
  @ccall libflint.fq_poly_shift_right(z::Ref{FqPolyRepPolyRingElem}, x::Ref{FqPolyRepPolyRingElem}, len::Int, base_ring(parent(x))::Ref{FqPolyRepField})::Nothing
  return z
end

################################################################################
#
#   Euclidean division
#
################################################################################

function Base.div(x::FqPolyRepPolyRingElem, y::FqPolyRepPolyRingElem)
  check_parent(x,y)
  z = parent(x)()
  @ccall libflint.fq_poly_div_basecase(z::Ref{FqPolyRepPolyRingElem}, x::Ref{FqPolyRepPolyRingElem}, y::Ref{FqPolyRepPolyRingElem}, base_ring(parent(x))::Ref{FqPolyRepField})::Nothing
  return z
end

function rem(x::FqPolyRepPolyRingElem, y::FqPolyRepPolyRingElem)
  check_parent(x,y)
  z = parent(x)()
  rem!(z, x, y)
  return z
end

function rem!(z::FqPolyRepPolyRingElem, x::FqPolyRepPolyRingElem, y::FqPolyRepPolyRingElem)
  @ccall libflint.fq_poly_rem(z::Ref{FqPolyRepPolyRingElem}, x::Ref{FqPolyRepPolyRingElem}, y::Ref{FqPolyRepPolyRingElem}, base_ring(parent(x))::Ref{FqPolyRepField})::Nothing
  return z
end

mod(x::FqPolyRepPolyRingElem, y::FqPolyRepPolyRingElem) = rem(x, y)

function Base.divrem(x::FqPolyRepPolyRingElem, y::FqPolyRepPolyRingElem)
  check_parent(x,y)
  z = parent(x)()
  r = parent(x)()
  @ccall libflint.fq_poly_divrem(z::Ref{FqPolyRepPolyRingElem}, r::Ref{FqPolyRepPolyRingElem}, x::Ref{FqPolyRepPolyRingElem}, y::Ref{FqPolyRepPolyRingElem}, base_ring(parent(x))::Ref{FqPolyRepField})::Nothing
  return z, r
end

################################################################################
#
#  Square root
#
################################################################################

function sqrt(x::FqPolyRepPolyRingElem; check::Bool=true)
  R = parent(x)
  s = R()
  flag = Bool(@ccall libflint.fq_poly_sqrt(s::Ref{FqPolyRepPolyRingElem}, x::Ref{FqPolyRepPolyRingElem}, base_ring(parent(x))::Ref{FqPolyRepField})::Cint)
  check && !flag && error("Not a square in sqrt")
  return s
end

function is_square(x::FqPolyRepPolyRingElem)
  if iszero(x)
    return true
  end
  if !iseven(degree(x))
    return false
  end
  R = parent(x)
  s = R()
  flag = Bool(@ccall libflint.fq_poly_sqrt(s::Ref{FqPolyRepPolyRingElem}, x::Ref{FqPolyRepPolyRingElem}, base_ring(parent(x))::Ref{FqPolyRepField})::Cint)
  return flag
end

function is_square_with_sqrt(x::FqPolyRepPolyRingElem)
  R = parent(x)
  if iszero(x)
    return true, zero(R)
  end
  if !iseven(degree(x))
    return false, zero(R)
  end
  s = R()
  flag = Bool(@ccall libflint.fq_poly_sqrt(s::Ref{FqPolyRepPolyRingElem}, x::Ref{FqPolyRepPolyRingElem}, base_ring(parent(x))::Ref{FqPolyRepField})::Cint)
  return flag, s
end

################################################################################
#
#   Remove and valuation
#
################################################################################

function remove(z::FqPolyRepPolyRingElem, p::FqPolyRepPolyRingElem)
  ok, v = _remove_check_simple_cases(z, p)
  ok && return v, zero(parent(z))
  z = deepcopy(z)
  v = @ccall libflint.fq_poly_remove(z::Ref{FqPolyRepPolyRingElem}, p::Ref{FqPolyRepPolyRingElem}, base_ring(parent(z))::Ref{FqPolyRepField})::Int
  return v, z
end

function divides(z::FqPolyRepPolyRingElem, x::FqPolyRepPolyRingElem)
  if iszero(z)
    return true, zero(parent(z))
  end
  if iszero(x)
    return false, zero(parent(z))
  end
  check_parent(z, x)
  q = parent(z)()
  v = Bool(@ccall libflint.fq_poly_divides(q::Ref{FqPolyRepPolyRingElem}, z::Ref{FqPolyRepPolyRingElem}, x::Ref{FqPolyRepPolyRingElem}, base_ring(parent(z))::Ref{FqPolyRepField})::Cint)
  return v, q
end

################################################################################
#
#   Modular arithmetic
#
################################################################################

function powermod(x::FqPolyRepPolyRingElem, n::Int, y::FqPolyRepPolyRingElem)
  check_parent(x,y)
  z = parent(x)()

  if n < 0
    g, x = gcdinv(x, y)
    if !isone(g)
      error("Element not invertible")
    end
    n = -n
  end

  @ccall libflint.fq_poly_powmod_ui_binexp(z::Ref{FqPolyRepPolyRingElem}, x::Ref{FqPolyRepPolyRingElem}, n::Int, y::Ref{FqPolyRepPolyRingElem}, base_ring(parent(x))::Ref{FqPolyRepField})::Nothing
  return z
end

function powermod(x::FqPolyRepPolyRingElem, n::ZZRingElem, y::FqPolyRepPolyRingElem)
  check_parent(x,y)
  z = parent(x)()

  if n < 0
    g, x = gcdinv(x, y)
    if !isone(g)
      error("Element not invertible")
    end
    n = -n
  end

  @ccall libflint.fq_poly_powmod_fmpz_binexp(z::Ref{FqPolyRepPolyRingElem}, x::Ref{FqPolyRepPolyRingElem}, n::Ref{ZZRingElem}, y::Ref{FqPolyRepPolyRingElem}, base_ring(parent(x))::Ref{FqPolyRepField})::Nothing
  return z
end

################################################################################
#
#   GCD
#
################################################################################

function gcd(x::FqPolyRepPolyRingElem, y::FqPolyRepPolyRingElem)
  check_parent(x,y)
  z = parent(x)()
  @ccall libflint.fq_poly_gcd(z::Ref{FqPolyRepPolyRingElem}, x::Ref{FqPolyRepPolyRingElem}, y::Ref{FqPolyRepPolyRingElem}, base_ring(parent(x))::Ref{FqPolyRepField})::Nothing
  return z
end

function gcdx(x::FqPolyRepPolyRingElem, y::FqPolyRepPolyRingElem)
  check_parent(x,y)
  z = parent(x)()
  s = parent(x)()
  t = parent(x)()
  @ccall libflint.fq_poly_xgcd(z::Ref{FqPolyRepPolyRingElem}, s::Ref{FqPolyRepPolyRingElem}, t::Ref{FqPolyRepPolyRingElem}, x::Ref{FqPolyRepPolyRingElem}, y::Ref{FqPolyRepPolyRingElem}, base_ring(parent(x))::Ref{FqPolyRepField})::Nothing
  return z, s, t
end

################################################################################
#
#   Evaluation
#
################################################################################

function evaluate(x::FqPolyRepPolyRingElem, y::FqPolyRepFieldElem)
  base_ring(parent(x)) != parent(y) && error("Incompatible coefficient rings")
  z = parent(y)()
  @ccall libflint.fq_poly_evaluate_fq(z::Ref{FqPolyRepFieldElem}, x::Ref{FqPolyRepPolyRingElem}, y::Ref{FqPolyRepFieldElem}, base_ring(parent(x))::Ref{FqPolyRepField})::Nothing
  return z
end

################################################################################
#
#   Composition
#
################################################################################

function AbstractAlgebra._compose_right(x::FqPolyRepPolyRingElem, y::FqPolyRepPolyRingElem)
  check_parent(x,y)
  z = parent(x)()
  @ccall libflint.fq_poly_compose(z::Ref{FqPolyRepPolyRingElem}, x::Ref{FqPolyRepPolyRingElem}, y::Ref{FqPolyRepPolyRingElem}, base_ring(parent(x))::Ref{FqPolyRepField})::Nothing
  return z
end

################################################################################
#
#   Derivative
#
################################################################################

function derivative(x::FqPolyRepPolyRingElem)
  z = parent(x)()
  @ccall libflint.fq_poly_derivative(z::Ref{FqPolyRepPolyRingElem}, x::Ref{FqPolyRepPolyRingElem}, base_ring(parent(x))::Ref{FqPolyRepField})::Nothing
  return z
end

################################################################################
#
#  Inflation and deflation
#
################################################################################

function inflate(x::FqPolyRepPolyRingElem, n::Int)
  z = parent(x)()
  @ccall libflint.fq_poly_inflate(z::Ref{FqPolyRepPolyRingElem}, x::Ref{FqPolyRepPolyRingElem}, UInt(n)::Culong, base_ring(parent(x))::Ref{FqPolyRepField})::Nothing
  return z
end

function deflate(x::FqPolyRepPolyRingElem, n::Int)
  z = parent(x)()
  @ccall libflint.fq_poly_deflate(z::Ref{FqPolyRepPolyRingElem}, x::Ref{FqPolyRepPolyRingElem}, UInt(n)::Culong, base_ring(parent(x))::Ref{FqPolyRepField})::Nothing
  return z
end

################################################################################
#
#  Irreducibility
#
################################################################################

function is_irreducible(x::FqPolyRepPolyRingElem)
  is_constant(x) && return false
  return Bool(@ccall libflint.fq_poly_is_irreducible(x::Ref{FqPolyRepPolyRingElem}, base_ring(parent(x))::Ref{FqPolyRepField})::Int32)
end

################################################################################
#
#  Squarefree testing
#
################################################################################

function is_squarefree(x::FqPolyRepPolyRingElem)
  return Bool(@ccall libflint.fq_poly_is_squarefree(x::Ref{FqPolyRepPolyRingElem}, base_ring(parent(x))::Ref{FqPolyRepField})::Int32)
end

################################################################################
#
#  Factorization
#
################################################################################

function factor(x::FqPolyRepPolyRingElem)
  iszero(x) && throw(ArgumentError("Argument must be non-zero"))
  fac, z = _factor(x)
  return Fac(parent(x)(z), fac)
end

function _factor(x::FqPolyRepPolyRingElem)
  R = parent(x)
  F = base_ring(R)
  a = F()
  fac = fq_poly_factor(F)
  @ccall libflint.fq_poly_factor(fac::Ref{fq_poly_factor}, a::Ref{FqPolyRepFieldElem}, x::Ref{FqPolyRepPolyRingElem}, F::Ref{FqPolyRepField})::Nothing
  res = Dict{FqPolyRepPolyRingElem,Int}()
  for i in 1:fac.num
    f = R()
    @ccall libflint.fq_poly_factor_get_poly(f::Ref{FqPolyRepPolyRingElem}, fac::Ref{fq_poly_factor}, (i-1)::Int, F::Ref{FqPolyRepField})::Nothing
    e = unsafe_load(fac.exp,i)
    res[f] = e
  end
  return res, a
end

function factor_squarefree(x::FqPolyRepPolyRingElem)
  iszero(x) && throw(ArgumentError("Argument must be non-zero"))
  # _factor_squareefree does weird things if the polynomial is not monic
  return Fac(parent(x)(leading_coefficient(x)),
             _factor_squarefree(divexact(x, leading_coefficient(x))))
end

function _factor_squarefree(x::FqPolyRepPolyRingElem)
  F = base_ring(parent(x))
  fac = fq_poly_factor(F)
  @ccall libflint.fq_poly_factor_squarefree(fac::Ref{fq_poly_factor}, x::Ref{FqPolyRepPolyRingElem}, F::Ref{FqPolyRepField})::UInt
  res = Dict{FqPolyRepPolyRingElem,Int}()
  for i in 1:fac.num
    f = parent(x)()
    @ccall libflint.fq_poly_factor_get_poly(f::Ref{FqPolyRepPolyRingElem}, fac::Ref{fq_poly_factor}, (i-1)::Int, F::Ref{FqPolyRepField})::Nothing
    e = unsafe_load(fac.exp, i)
    res[f] = e
  end
  return res
end

@doc raw"""
    factor_distinct_deg(x::FqPolyRepPolyRingElem)

Return the distinct degree factorisation of a squarefree polynomial $x$.
"""
function factor_distinct_deg(x::FqPolyRepPolyRingElem)
  R = parent(x)
  F = base_ring(R)
  fac = fq_poly_factor(F)
  degrees = Vector{Int}(undef, degree(x))
  @ccall libflint.fq_poly_factor_distinct_deg(fac::Ref{fq_poly_factor}, x::Ref{FqPolyRepPolyRingElem}, degrees::Ref{Vector{Int}}, F::Ref{FqPolyRepField})::Nothing
  res = Dict{Int, FqPolyRepPolyRingElem}()
  for i in 1:fac.num
    f = R()
    @ccall libflint.fq_poly_factor_get_poly(f::Ref{FqPolyRepPolyRingElem}, fac::Ref{fq_poly_factor}, (i-1)::Int, F::Ref{FqPolyRepField})::Nothing
    res[degrees[i]] = f
  end
  return res
end

function roots(x::FqPolyRepPolyRingElem)
  R = parent(x)
  F = base_ring(R)
  fac = fq_poly_factor(F)
  @ccall libflint.fq_poly_roots(fac::Ref{fq_poly_factor}, x::Ref{FqPolyRepPolyRingElem}, 0::Cint, F::Ref{FqPolyRepField})::Nothing
  res = FqPolyRepFieldElem[]
  for i in 1:fac.num
    f = R()
    @ccall libflint.fq_poly_factor_get_poly(f::Ref{FqPolyRepPolyRingElem}, fac::Ref{fq_poly_factor}, (i-1)::Int, F::Ref{FqPolyRepField})::Nothing
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

function zero!(z::FqPolyRepPolyRingElem)
  @ccall libflint.fq_poly_zero(z::Ref{FqPolyRepPolyRingElem}, base_ring(parent(z))::Ref{FqPolyRepField})::Nothing
  return z
end

function one!(z::FqPolyRepPolyRingElem)
  @ccall libflint.fq_poly_one(z::Ref{FqPolyRepPolyRingElem}, base_ring(parent(z))::Ref{FqPolyRepField})::Nothing
  return z
end

function neg!(z::FqPolyRepPolyRingElem, a::FqPolyRepPolyRingElem)
  @ccall libflint.fq_poly_neg(z::Ref{FqPolyRepPolyRingElem}, a::Ref{FqPolyRepPolyRingElem}, base_ring(parent(a))::Ref{FqPolyRepField})::Nothing
  return z
end

function fit!(z::FqPolyRepPolyRingElem, n::Int)
  @ccall libflint.fq_poly_fit_length(z::Ref{FqPolyRepPolyRingElem}, n::Int, base_ring(parent(z))::Ref{FqPolyRepField})::Nothing
  return nothing
end

function setcoeff!(z::FqPolyRepPolyRingElem, n::Int, x::FqPolyRepFieldElem)
  @ccall libflint.fq_poly_set_coeff(z::Ref{FqPolyRepPolyRingElem}, n::Int, x::Ref{FqPolyRepFieldElem}, base_ring(parent(z))::Ref{FqPolyRepField})::Nothing
  return z
end

function mul!(z::FqPolyRepPolyRingElem, x::FqPolyRepPolyRingElem, y::FqPolyRepPolyRingElem)
  @ccall libflint.fq_poly_mul(z::Ref{FqPolyRepPolyRingElem}, x::Ref{FqPolyRepPolyRingElem}, y::Ref{FqPolyRepPolyRingElem}, base_ring(parent(x))::Ref{FqPolyRepField})::Nothing
  return z
end

function add!(z::FqPolyRepPolyRingElem, x::FqPolyRepPolyRingElem, y::FqPolyRepPolyRingElem)
  @ccall libflint.fq_poly_add(z::Ref{FqPolyRepPolyRingElem}, x::Ref{FqPolyRepPolyRingElem}, y::Ref{FqPolyRepPolyRingElem}, base_ring(parent(x))::Ref{FqPolyRepField})::Nothing
  return z
end

function sub!(z::FqPolyRepPolyRingElem, x::FqPolyRepPolyRingElem, y::FqPolyRepPolyRingElem)
  @ccall libflint.fq_poly_sub(z::Ref{FqPolyRepPolyRingElem}, x::Ref{FqPolyRepPolyRingElem}, y::Ref{FqPolyRepPolyRingElem}, base_ring(parent(x))::Ref{FqPolyRepField})::Nothing
  return z
end

function reverse!(z::FqPolyRepPolyRingElem, x::FqPolyRepPolyRingElem, len::Int)
  @ccall libflint.fq_poly_reverse(z::Ref{FqPolyRepPolyRingElem}, x::Ref{FqPolyRepPolyRingElem}, len::Int, base_ring(parent(x))::Ref{FqPolyRepField})::Nothing
  return z
end

################################################################################
#
#  Promotion rules
#
################################################################################

promote_rule(::Type{FqPolyRepPolyRingElem}, ::Type{V}) where {V <: Integer} = FqPolyRepPolyRingElem

promote_rule(::Type{FqPolyRepPolyRingElem}, ::Type{ZZRingElem}) = FqPolyRepPolyRingElem

promote_rule(::Type{FqPolyRepPolyRingElem}, ::Type{FqPolyRepFieldElem}) = FqPolyRepPolyRingElem

################################################################################
#
#   Parent object call overloads
#
################################################################################

function (R::FqPolyRepPolyRing)()
  z = FqPolyRepPolyRingElem()
  z.parent = R
  return z
end

function (R::FqPolyRepPolyRing)(x::FqPolyRepFieldElem)
  z = FqPolyRepPolyRingElem(x)
  z.parent = R
  return z
end

function (R::FqPolyRepPolyRing)(x::ZZRingElem)
  return R(base_ring(R)(x))
end

function (R::FqPolyRepPolyRing)(x::Integer)
  return R(ZZRingElem(x))
end

function (R::FqPolyRepPolyRing)(x::Vector{FqPolyRepFieldElem})
  length(x) == 0 && return zero(R)
  base_ring(R) != parent(x[1]) && error("Coefficient rings must coincide")
  z = FqPolyRepPolyRingElem(x)
  z.parent = R
  return z
end

function (R::FqPolyRepPolyRing)(x::Vector{ZZRingElem})
  length(x) == 0 && return zero(R)
  z = FqPolyRepPolyRingElem(x, base_ring(R))
  z.parent = R
  return z
end

function (R::FqPolyRepPolyRing)(x::Vector{T}) where {T <: Integer}
  length(x) == 0 && return zero(R)
  return R(map(ZZRingElem, x))
end

function (R::FqPolyRepPolyRing)(x::ZZPolyRingElem)
  z = FqPolyRepPolyRingElem(x, base_ring(R))
  z.parent = R
  return z
end

function (R::FqPolyRepPolyRing)(x::FqPolyRepPolyRingElem)
  parent(x) != R && error("Unable to coerce to polynomial")
  return x
end
