################################################################################
#
#  fq_default_poly.jl: FLINT fq_default_poly
#                      (Polynomials over FqDefaultFiniteField)
#
################################################################################

################################################################################
#
#  Type and parent object methods
#
################################################################################

parent_type(::Type{FqPolyRingElem}) = FqPolyRing

elem_type(::Type{FqPolyRing}) = FqPolyRingElem

dense_poly_type(::Type{FqFieldElem}) = FqPolyRingElem

base_ring(a::FqPolyRing) = a.base_ring

parent(a::FqPolyRingElem) = a.parent

var(a::FqPolyRing) = a.S

################################################################################
#
#   Basic manipulation
#
################################################################################

function length(x::FqPolyRingElem)
  F = (x.parent).base_ring
  @ccall libflint.fq_default_poly_length(x::Ref{FqPolyRingElem}, F::Ref{FqField})::Int
end

function coeff(x::FqPolyRingElem, n::Int)
  n < 0 && throw(DomainError(n, "Index must be non-negative"))
  F = (x.parent).base_ring
  temp = F(1)
  @ccall libflint.fq_default_poly_get_coeff(temp::Ref{FqFieldElem}, x::Ref{FqPolyRingElem}, n::Int, F::Ref{FqField})::Nothing
  return temp
end

function set_length!(x::FqPolyRingElem, n::Int)
  ctx = base_ring(x)
  @ccall libflint._fq_default_poly_set_length(x::Ref{FqPolyRingElem}, n::Int, ctx::Ref{FqField})::Nothing
  return x
end

zero(a::FqPolyRing) = a(zero(base_ring(a)))

one(a::FqPolyRing) = a(one(base_ring(a)))

gen(a::FqPolyRing) = a([zero(base_ring(a)), one(base_ring(a))])

is_gen(x::FqPolyRingElem) = @ccall libflint.fq_default_poly_is_gen(x::Ref{FqPolyRingElem}, base_ring(x.parent)::Ref{FqField})::Bool

isone(x::FqPolyRingElem) = @ccall libflint.fq_default_poly_is_one(x::Ref{FqPolyRingElem}, base_ring(x.parent)::Ref{FqField})::Bool

function deepcopy_internal(a::FqPolyRingElem, dict::IdDict)
  z = FqPolyRingElem(a, base_ring(a))
  z.parent = a.parent
  return z
end

###############################################################################
#
#   Similar
#
###############################################################################

function similar(f::PolyRingElem, R::FqField, s::Symbol=var(parent(f)); cached::Bool=true)
  z = FqPolyRingElem(R)
  if base_ring(f) === R && s == var(parent(f)) && f isa FqPolyRingElem
    # steal parent in case it is not cached
    z.parent = parent(f)
  else
    z.parent = FqPolyRing(R, s, cached)
  end
  return z
end

################################################################################
#
#  Unary operations
#
################################################################################

-(x::FqPolyRingElem) = neg!(parent(x)(), x)

################################################################################
#
#  Binary operations
#
################################################################################

function +(x::FqPolyRingElem, y::FqPolyRingElem)
  check_parent(x,y)
  z = parent(x)()
  @ccall libflint.fq_default_poly_add(z::Ref{FqPolyRingElem}, x::Ref{FqPolyRingElem}, y::Ref{FqPolyRingElem}, base_ring(parent(x))::Ref{FqField})::Nothing
  return z
end

function -(x::FqPolyRingElem, y::FqPolyRingElem)
  check_parent(x,y)
  z = parent(x)()
  @ccall libflint.fq_default_poly_sub(z::Ref{FqPolyRingElem}, x::Ref{FqPolyRingElem}, y::Ref{FqPolyRingElem}, base_ring(parent(x))::Ref{FqField})::Nothing
  return z
end

function *(x::FqPolyRingElem, y::FqPolyRingElem)
  check_parent(x,y)
  z = parent(x)()
  @ccall libflint.fq_default_poly_mul(z::Ref{FqPolyRingElem}, x::Ref{FqPolyRingElem}, y::Ref{FqPolyRingElem}, base_ring(parent(x))::Ref{FqField})::Nothing
  return z
end

################################################################################
#
#   Ad hoc binary operators
#
################################################################################

function *(x::FqFieldElem, y::FqPolyRingElem)
  parent(x) != base_ring(parent(y)) &&
  error("Coefficient rings must be equal")
  z = parent(y)()
  @ccall libflint.fq_default_poly_scalar_mul_fq_default(z::Ref{FqPolyRingElem}, y::Ref{FqPolyRingElem}, x::Ref{FqFieldElem}, parent(x)::Ref{FqField})::Nothing
  return z
end

*(x::FqPolyRingElem, y::FqFieldElem) = y*x

*(x::ZZRingElem, y::FqPolyRingElem) = base_ring(parent(y))(x) * y

*(x::FqPolyRingElem, y::ZZRingElem) = y*x

*(x::Integer, y::FqPolyRingElem) = ZZRingElem(x)*y

*(x::FqPolyRingElem, y::Integer) = y*x

+(x::FqFieldElem, y::FqPolyRingElem) = parent(y)(x) + y

+(x::FqPolyRingElem, y::FqFieldElem) = y + x

+(x::ZZRingElem, y::FqPolyRingElem) = base_ring(parent(y))(x) + y

+(x::FqPolyRingElem, y::ZZRingElem) = y + x

+(x::FqPolyRingElem, y::Integer) = x + ZZRingElem(y)

+(x::Integer, y::FqPolyRingElem) = y + x

-(x::FqFieldElem, y::FqPolyRingElem) = parent(y)(x) - y

-(x::FqPolyRingElem, y::FqFieldElem) = x - parent(x)(y)

-(x::ZZRingElem, y::FqPolyRingElem) = base_ring(parent(y))(x) - y

-(x::FqPolyRingElem, y::ZZRingElem) = x - base_ring(parent(x))(y)

-(x::FqPolyRingElem, y::Integer) = x - ZZRingElem(y)

-(x::Integer, y::FqPolyRingElem) = ZZRingElem(x) - y

################################################################################
#
#   Powering
#
################################################################################

function ^(x::FqPolyRingElem, y::Int)
  y < 0 && throw(DomainError(y, "Exponent must be non-negative"))
  z = parent(x)()
  @ccall libflint.fq_default_poly_pow(z::Ref{FqPolyRingElem}, x::Ref{FqPolyRingElem}, y::Int, base_ring(parent(x))::Ref{FqField})::Nothing
  return z
end

################################################################################
#
#   Comparisons
#
################################################################################

function ==(x::FqPolyRingElem, y::FqPolyRingElem)
  check_parent(x,y)
  r = @ccall libflint.fq_default_poly_equal(x::Ref{FqPolyRingElem}, y::Ref{FqPolyRingElem}, base_ring(parent(x))::Ref{FqField})::Cint
  return Bool(r)
end

################################################################################
#
#   Ad hoc comparisons
#
################################################################################

function ==(x::FqPolyRingElem, y::FqFieldElem)
  base_ring(parent(x)) != parent(y) && return false
  if length(x) > 1
    return false
  elseif length(x) == 1
    r = @ccall libflint.fq_default_poly_equal_fq_default(x::Ref{FqPolyRingElem}, y::Ref{FqFieldElem}, base_ring(parent(x))::Ref{FqField})::Cint
    return Bool(r)
  else
    return iszero(y)
  end
end

==(x::FqFieldElem, y::FqPolyRingElem) = y == x

==(x::FqPolyRingElem, y::ZZRingElem) = x == base_ring(parent(x))(y)

==(x::ZZRingElem, y::FqPolyRingElem) = y == x

==(x::FqPolyRingElem, y::Integer) = x == ZZRingElem(y)

==(x::Integer, y::FqPolyRingElem) = y == x

################################################################################
#
#   Truncation
#
################################################################################

function truncate(x::FqPolyRingElem, n::Int)
  n < 0 && throw(DomainError(n, "Index must be non-negative"))
  if length(x) <= n
    return x
  end
  z = parent(x)()
  @ccall libflint.fq_default_poly_set_trunc(z::Ref{FqPolyRingElem}, x::Ref{FqPolyRingElem}, n::Int, base_ring(parent(x))::Ref{FqField})::Nothing
  return z
end

function mullow(x::FqPolyRingElem, y::FqPolyRingElem, n::Int)
  check_parent(x,y)
  n < 0 && throw(DomainError(n, "Index must be non-negative"))
  z = parent(x)()
  @ccall libflint.fq_default_poly_mullow(z::Ref{FqPolyRingElem}, x::Ref{FqPolyRingElem}, y::Ref{FqPolyRingElem}, n::Int, base_ring(parent(x))::Ref{FqField})::Nothing
  return z
end

################################################################################
#
#   Reversal
#
################################################################################

function reverse(x::FqPolyRingElem, len::Int)
  len < 0 && throw(DomainError(len, "Index must be non-negative"))
  return reverse!(parent(x)(), x, len)
end

function reverse!(z::FqPolyRingElem, x::FqPolyRingElem, len::Int)
  @ccall libflint.fq_default_poly_reverse(z::Ref{FqPolyRingElem}, x::Ref{FqPolyRingElem}, len::Int, base_ring(parent(x))::Ref{FqField})::Nothing
  return z
end

################################################################################
#
#   Shifting
#
################################################################################

function shift_left(x::FqPolyRingElem, len::Int)
  len < 0 && throw(DomainError(len, "Shift must be non-negative"))
  z = parent(x)()
  @ccall libflint.fq_default_poly_shift_left(z::Ref{FqPolyRingElem}, x::Ref{FqPolyRingElem}, len::Int, base_ring(parent(x))::Ref{FqField})::Nothing
  return z
end

function shift_right(x::FqPolyRingElem, len::Int)
  len < 0 && throw(DomainError(len, "Shift must be non-negative"))
  z = parent(x)()
  @ccall libflint.fq_default_poly_shift_right(z::Ref{FqPolyRingElem}, x::Ref{FqPolyRingElem}, len::Int, base_ring(parent(x))::Ref{FqField})::Nothing
  return z
end

################################################################################
#
#   Euclidean division
#
################################################################################

function Base.div(x::FqPolyRingElem, y::FqPolyRingElem)
  check_parent(x,y)
  return divrem(x, y)[1]
  # The following function does not exist in flint
  #z = parent(x)()
  #@ccall libflint.fq_default_poly_div_basecase(z::Ref{FqPolyRingElem}, x::Ref{FqPolyRingElem}, y::Ref{FqPolyRingElem},
  #      base_ring(parent(x))::Ref{FqField})::Nothing
  #return z
end

function rem(x::FqPolyRingElem, y::FqPolyRingElem)
  check_parent(x,y)
  z = parent(x)()
  @ccall libflint.fq_default_poly_rem(z::Ref{FqPolyRingElem}, x::Ref{FqPolyRingElem}, y::Ref{FqPolyRingElem}, base_ring(parent(x))::Ref{FqField})::Nothing
  return z
end

mod(x::FqPolyRingElem, y::FqPolyRingElem) = rem(x, y)

function Base.divrem(x::FqPolyRingElem, y::FqPolyRingElem)
  check_parent(x,y)
  z = parent(x)()
  r = parent(x)()
  @ccall libflint.fq_default_poly_divrem(z::Ref{FqPolyRingElem}, r::Ref{FqPolyRingElem}, x::Ref{FqPolyRingElem}, y::Ref{FqPolyRingElem}, base_ring(parent(x))::Ref{FqField})::Nothing
  return z, r
end

################################################################################
#
#  Square root
#
################################################################################

function sqrt(x::FqPolyRingElem; check::Bool=true)
  R = parent(x)
  s = R()
  flag = Bool(@ccall libflint.fq_default_poly_sqrt(s::Ref{FqPolyRingElem}, x::Ref{FqPolyRingElem}, base_ring(parent(x))::Ref{FqField})::Cint)
  check && !flag && error("Not a square in sqrt")
  return s
end

function is_square(x::FqPolyRingElem)
  if iszero(x)
    return true
  end
  if !iseven(degree(x))
    return false
  end
  R = parent(x)
  s = R()
  flag = Bool(@ccall libflint.fq_default_poly_sqrt(s::Ref{FqPolyRingElem}, x::Ref{FqPolyRingElem}, base_ring(parent(x))::Ref{FqField})::Cint)
  return flag
end

function is_square_with_sqrt(x::FqPolyRingElem)
  R = parent(x)
  if iszero(x)
    return true, zero(R)
  end
  if !iseven(degree(x))
    return false, zero(R)
  end
  s = R()
  flag = Bool(@ccall libflint.fq_default_poly_sqrt(s::Ref{FqPolyRingElem}, x::Ref{FqPolyRingElem}, base_ring(parent(x))::Ref{FqField})::Cint)
  return flag, s
end

################################################################################
#
#   Remove and valuation
#
################################################################################

function remove(z::FqPolyRingElem, p::FqPolyRingElem)
  ok, v = _remove_check_simple_cases(z, p)
  ok && return v, zero(parent(z))
  z = deepcopy(z)
  v = @ccall libflint.fq_default_poly_remove(z::Ref{FqPolyRingElem}, p::Ref{FqPolyRingElem}, base_ring(parent(z))::Ref{FqField})::Int
  return v, z
end

function divides(z::FqPolyRingElem, x::FqPolyRingElem)
  if iszero(z)
    return true, zero(parent(z))
  end
  if iszero(x)
    return false, zero(parent(z))
  end
  check_parent(z, x)
  q = parent(z)()
  v = Bool(@ccall libflint.fq_default_poly_divides(q::Ref{FqPolyRingElem}, z::Ref{FqPolyRingElem}, x::Ref{FqPolyRingElem}, base_ring(parent(z))::Ref{FqField})::Cint)
  return v, q
end

################################################################################
#
#   Modular arithmetic
#
################################################################################

function powermod(x::FqPolyRingElem, n::Int, y::FqPolyRingElem)
  check_parent(x,y)
  z = parent(x)()

  if n < 0
    g, x = gcdinv(x, y)
    if !isone(g)
      error("Element not invertible")
    end
    n = -n
  end

  @ccall libflint.fq_default_poly_powmod_ui_binexp(z::Ref{FqPolyRingElem}, x::Ref{FqPolyRingElem}, n::Int, y::Ref{FqPolyRingElem}, base_ring(parent(x))::Ref{FqField})::Nothing
  return z
end

function powermod(x::FqPolyRingElem, n::ZZRingElem, y::FqPolyRingElem)
  check_parent(x,y)
  z = parent(x)()

  if n < 0
    g, x = gcdinv(x, y)
    if !isone(g)
      error("Element not invertible")
    end
    n = -n
  end
  @ccall libflint.fq_default_poly_powmod_fmpz_binexp(z::Ref{FqPolyRingElem}, x::Ref{FqPolyRingElem}, n::Ref{ZZRingElem}, y::Ref{FqPolyRingElem}, base_ring(parent(x))::Ref{FqField})::Nothing
  return z
end

################################################################################
#
#   GCD
#
################################################################################

function gcd(x::FqPolyRingElem, y::FqPolyRingElem)
  check_parent(x,y)
  z = parent(x)()
  @ccall libflint.fq_default_poly_gcd(z::Ref{FqPolyRingElem}, x::Ref{FqPolyRingElem}, y::Ref{FqPolyRingElem}, base_ring(parent(x))::Ref{FqField})::Nothing
  return z
end

function gcdx(x::FqPolyRingElem, y::FqPolyRingElem)
  check_parent(x,y)
  z = parent(x)()
  s = parent(x)()
  t = parent(x)()
  @ccall libflint.fq_default_poly_xgcd(z::Ref{FqPolyRingElem}, s::Ref{FqPolyRingElem}, t::Ref{FqPolyRingElem}, x::Ref{FqPolyRingElem}, y::Ref{FqPolyRingElem}, base_ring(parent(x))::Ref{FqField})::Nothing
  return z, s, t
end

################################################################################
#
#   Evaluation
#
################################################################################

function evaluate(x::FqPolyRingElem, y::FqFieldElem)
  base_ring(parent(x)) != parent(y) && error("Incompatible coefficient rings")
  z = parent(y)()
  @ccall libflint.fq_default_poly_evaluate_fq_default(z::Ref{FqFieldElem}, x::Ref{FqPolyRingElem}, y::Ref{FqFieldElem}, base_ring(parent(x))::Ref{FqField})::Nothing
  return z
end

################################################################################
#
#   Composition
#
################################################################################

function AbstractAlgebra._compose_right(x::FqPolyRingElem, y::FqPolyRingElem)
  check_parent(x,y)
  z = parent(x)()
  @ccall libflint.fq_default_poly_compose(z::Ref{FqPolyRingElem}, x::Ref{FqPolyRingElem}, y::Ref{FqPolyRingElem}, base_ring(parent(x))::Ref{FqField})::Nothing
  return z
end

################################################################################
#
#   Derivative
#
################################################################################

function derivative(x::FqPolyRingElem)
  z = parent(x)()
  @ccall libflint.fq_default_poly_derivative(z::Ref{FqPolyRingElem}, x::Ref{FqPolyRingElem}, base_ring(parent(x))::Ref{FqField})::Nothing
  return z
end

################################################################################
#
#  Inflation and deflation
#
################################################################################

function inflate(x::FqPolyRingElem, n::Int)
  z = parent(x)()
  @ccall libflint.fq_default_poly_inflate(z::Ref{FqPolyRingElem}, x::Ref{FqPolyRingElem}, UInt(n)::Culong, base_ring(parent(x))::Ref{FqField})::Nothing
  return z
end

function deflate(x::FqPolyRingElem, n::Int)
  z = parent(x)()
  @ccall libflint.fq_default_poly_deflate(z::Ref{FqPolyRingElem}, x::Ref{FqPolyRingElem}, UInt(n)::Culong, base_ring(parent(x))::Ref{FqField})::Nothing
  return z
end

################################################################################
#
#  Irreducibility
#
################################################################################

function is_irreducible(x::FqPolyRingElem)
  is_constant(x) && return false
  return Bool(@ccall libflint.fq_default_poly_is_irreducible(x::Ref{FqPolyRingElem}, base_ring(parent(x))::Ref{FqField})::Int32)
end

################################################################################
#
#  Squarefree testing
#
################################################################################

function is_squarefree(x::FqPolyRingElem)
  return Bool(@ccall libflint.fq_default_poly_is_squarefree(x::Ref{FqPolyRingElem}, base_ring(parent(x))::Ref{FqField})::Int32)
end

################################################################################
#
#  Factorization
#
################################################################################

function exponent(x::fq_default_poly_factor, i::Int)
  return @ccall libflint.fq_default_poly_factor_exp(x::Ref{fq_default_poly_factor}, i::Int, x.base_field::Ref{FqField})::Int
end

function length(x::fq_default_poly_factor)
  return @ccall libflint.fq_default_poly_factor_length(x::Ref{fq_default_poly_factor}, x.base_field::Ref{FqField})::Int
end   

function factor(x::FqPolyRingElem)
  iszero(x) && throw(ArgumentError("Argument must be non-zero"))
  fac, z = _factor(x)
  return Fac(parent(x)(z), fac)
end

function _factor(x::FqPolyRingElem)
  R = parent(x)
  F = base_ring(R)
  a = F()
  fac = fq_default_poly_factor(F)
  @ccall libflint.fq_default_poly_factor(fac::Ref{fq_default_poly_factor}, a::Ref{FqFieldElem}, x::Ref{FqPolyRingElem}, F::Ref{FqField})::Nothing
  res = Dict{FqPolyRingElem,Int}()
  for i in 1:length(fac)
    f = R()
    @ccall libflint.fq_default_poly_factor_get_poly(f::Ref{FqPolyRingElem}, fac::Ref{fq_default_poly_factor}, (i - 1)::Int, F::Ref{FqField})::Nothing
    e = exponent(fac, i - 1)
    res[f] = e
  end
  return res, a
end

function factor_squarefree(x::FqPolyRingElem)
  iszero(x) && throw(ArgumentError("Argument must be non-zero"))
  # _factor_squareefree does weird things if the polynomial is not monic
  return Fac(parent(x)(leading_coefficient(x)),
             _factor_squarefree(divexact(x, leading_coefficient(x))))
end

function _factor_squarefree(x::FqPolyRingElem)
  F = base_ring(parent(x))
  fac = fq_default_poly_factor(F)
  @ccall libflint.fq_default_poly_factor_squarefree(fac::Ref{fq_default_poly_factor}, x::Ref{FqPolyRingElem}, F::Ref{FqField})::UInt
  res = Dict{FqPolyRingElem,Int}()
  for i in 1:length(fac)
    f = parent(x)()
    @ccall libflint.fq_default_poly_factor_get_poly(f::Ref{FqPolyRingElem}, fac::Ref{fq_default_poly_factor}, (i-1)::Int, F::Ref{FqField})::Nothing
    e = exponent(fac, i - 1)
    res[f] = e
  end
  return res
end

@doc raw"""
    factor_distinct_deg(x::FqPolyRingElem)

Return the distinct degree factorisation of a squarefree polynomial $x$.
"""
function factor_distinct_deg(x::FqPolyRingElem)
  R = parent(x)
  F = base_ring(R)
  fac = fq_default_poly_factor(F)
  degrees = Vector{Int}(undef, degree(x))
  @ccall libflint.fq_default_poly_factor_distinct_deg(fac::Ref{fq_default_poly_factor}, x::Ref{FqPolyRingElem}, degrees::Ref{Vector{Int}}, F::Ref{FqField})::Nothing
  res = Dict{Int, FqPolyRingElem}()
  for i in 1:length(fac)
    f = R()
    @ccall libflint.fq_default_poly_factor_get_poly(f::Ref{FqPolyRingElem}, fac::Ref{fq_default_poly_factor}, (i-1)::Int, F::Ref{FqField})::Nothing
    res[degrees[i]] = f
  end
  return res
end

function roots(x::FqPolyRingElem)
  R = parent(x)
  F = base_ring(R)
  fac = fq_default_poly_factor(F)
  @ccall libflint.fq_default_poly_roots(fac::Ref{fq_default_poly_factor}, x::Ref{FqPolyRingElem}, 0::Cint, F::Ref{FqField})::Nothing
  res = FqFieldElem[]
  for i in 1:length(fac)
    f = R()
    @ccall libflint.fq_default_poly_factor_get_poly(f::Ref{FqPolyRingElem}, fac::Ref{fq_default_poly_factor}, (i-1)::Int, F::Ref{FqField})::Nothing
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

function zero!(z::FqPolyRingElem)
  @ccall libflint.fq_default_poly_zero(z::Ref{FqPolyRingElem}, base_ring(parent(z))::Ref{FqField})::Nothing
  return z
end

function one!(z::FqPolyRingElem)
  @ccall libflint.fq_default_poly_one(z::Ref{FqPolyRingElem}, base_ring(parent(z))::Ref{FqField})::Nothing
  return z
end

function neg!(z::FqPolyRingElem, a::FqPolyRingElem)
  @ccall libflint.fq_default_poly_neg(z::Ref{FqPolyRingElem}, a::Ref{FqPolyRingElem}, base_ring(parent(a))::Ref{FqField})::Nothing
  return z
end

function fit!(z::FqPolyRingElem, n::Int)
  @ccall libflint.fq_default_poly_fit_length(z::Ref{FqPolyRingElem}, n::Int, base_ring(parent(z))::Ref{FqField})::Nothing
  return nothing
end

function setcoeff!(z::FqPolyRingElem, n::Int, x::FqFieldElem)
  @ccall libflint.fq_default_poly_set_coeff(z::Ref{FqPolyRingElem}, n::Int, x::Ref{FqFieldElem}, base_ring(parent(z))::Ref{FqField})::Nothing
  return z
end

function mul!(z::FqPolyRingElem, x::FqPolyRingElem, y::FqPolyRingElem)
  @ccall libflint.fq_default_poly_mul(z::Ref{FqPolyRingElem}, x::Ref{FqPolyRingElem}, y::Ref{FqPolyRingElem}, base_ring(parent(x))::Ref{FqField})::Nothing
  return z
end

function add!(z::FqPolyRingElem, x::FqPolyRingElem, y::FqPolyRingElem)
  @ccall libflint.fq_default_poly_add(z::Ref{FqPolyRingElem}, x::Ref{FqPolyRingElem}, y::Ref{FqPolyRingElem}, base_ring(parent(x))::Ref{FqField})::Nothing
  return z
end

function sub!(z::FqPolyRingElem, x::FqPolyRingElem, y::FqPolyRingElem)
  @ccall libflint.fq_default_poly_sub(z::Ref{FqPolyRingElem}, x::Ref{FqPolyRingElem}, y::Ref{FqPolyRingElem}, base_ring(parent(x))::Ref{FqField})::Nothing
  return z
end

################################################################################
#
#  Promotion rules
#
################################################################################

promote_rule(::Type{FqPolyRingElem}, ::Type{V}) where {V <: Integer} = FqPolyRingElem

promote_rule(::Type{FqPolyRingElem}, ::Type{ZZRingElem}) = FqPolyRingElem

promote_rule(::Type{FqPolyRingElem}, ::Type{FqFieldElem}) = FqPolyRingElem

################################################################################
#
#   Parent object call overloads
#
################################################################################

function (R::FqPolyRing)()
  z = FqPolyRingElem(base_ring(R))
  z.parent = R
  return z
end

function (R::FqPolyRing)(x::FqFieldElem)
  parent(x) !== base_ring(R) && error("Element not contained in coefficient ring")
  z = FqPolyRingElem(x, base_ring(R))
  z.parent = R
  return z
end

function (R::FqPolyRing)(x::ZZRingElem)
  return R(base_ring(R)(x))
end

function (R::FqPolyRing)(x::Integer)
  return R(ZZRingElem(x))
end

function (R::FqPolyRing)(x::Vector{FqFieldElem})
  length(x) == 0 && return zero(R)
  base_ring(R) != parent(x[1]) && error("Coefficient rings must coincide")
  z = FqPolyRingElem(x, base_ring(R))
  z.parent = R
  return z
end

function (R::FqPolyRing)(x::Vector{ZZRingElem})
  length(x) == 0 && return zero(R)
  z = FqPolyRingElem(x, base_ring(R))
  z.parent = R
  return z
end

function (R::FqPolyRing)(x::Vector{T}) where {T <: Integer}
  length(x) == 0 && return zero(R)
  return R(map(ZZRingElem, x))
end

function (R::FqPolyRing)(x::ZZPolyRingElem)
  z = FqPolyRingElem(x, base_ring(R))
  z.parent = R
  return z
end

function (R::FqPolyRing)(x::Union{zzModPolyRingElem, fpPolyRingElem})
  characteristic(base_ring(x)) != characteristic(base_ring(R)) &&
  error("Incompatible characteristic")
  z = FqPolyRingElem(x, base_ring(R))
  z.parent = R
  return z
end

function (R::FqPolyRing)(x::Union{ZZModPolyRingElem, FpPolyRingElem})
  characteristic(base_ring(x)) != characteristic(base_ring(R)) &&
  error("Incompatible characteristic")
  z = FqPolyRingElem(x, base_ring(R))
  z.parent = R
  return z
end

function (R::FqPolyRing)(x::FqPolyRingElem)
  parent(x) != R && error("Unable to coerce to polynomial")
  return x
end

################################################################################
#
#  Lift
#
################################################################################

function lift(R::ZZPolyRing, f::FqPolyRingElem)
  F = coefficient_ring(f)
  absolute_degree(F) != 1 && error("Must be a prime field.")
  if _fq_default_ctx_type(F) == _FQ_DEFAULT_NMOD
    z = R()
    @ccall libflint.fmpz_poly_set_nmod_poly_unsigned(z::Ref{ZZPolyRingElem}, f::Ref{FqPolyRingElem})::Nothing
    return z
  else
    @assert _fq_default_ctx_type(F) == _FQ_DEFAULT_FMPZ_NMOD
    z = R()
    @ccall libflint.fmpz_mod_poly_get_fmpz_poly(z::Ref{ZZPolyRingElem}, f::Ref{FqPolyRingElem}, (pointer_from_objref(F) + 2 * sizeof(Cint))::Ptr{Nothing})::Nothing
    return z
  end
end
