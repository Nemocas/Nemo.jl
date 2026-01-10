################################################################################
#
#  nmod_poly.jl : FLINT nmod_poly (polynomials over Z/nZ, small modulus)
#
################################################################################

################################################################################
#
#  Type and parent object methods
#
################################################################################

parent(a::zzModPolyRingElem) = a.parent

base_ring(R::zzModPolyRing) = R.base_ring

parent_type(::Type{zzModPolyRingElem}) = zzModPolyRing

elem_type(::Type{zzModPolyRing}) = zzModPolyRingElem

poly_type(::Type{zzModRingElem}) = zzModPolyRingElem

################################################################################
#
#   Basic helper
#
################################################################################

function lead_is_unit_or_throw(a::zzModPolyRingElem)
  d = degree(a)
  u = @ccall libflint.nmod_poly_get_coeff_ui(a::Ref{zzModPolyRingElem}, d::Int)::UInt
  n = @ccall libflint.n_gcd(u::UInt, modulus(a)::UInt)::UInt
  if n != 1
    R = base_ring(a)
    throw(NotInvertibleError(R(n), R))
  end
end

function Base.hash(a::zzModPolyRingElem, h::UInt)
  b = 0x53dd43cd511044d1%UInt
  for i in 0:length(a) - 1
    u = @ccall libflint.nmod_poly_get_coeff_ui(a::Ref{zzModPolyRingElem}, i::Int)::UInt
    b = xor(b, xor(hash(u, h), h))
    b = (b << 1) | (b >> (sizeof(Int)*8 - 1))
  end
  return b
end

################################################################################
#
#  Basic manipulation
#
################################################################################

length(x::T) where T <: Zmodn_poly = x.length

function set_length!(x::T, n::Int) where {T <: Zmodn_poly}
  @ccall libflint._nmod_poly_set_length(x::Ref{T}, n::Int)::Nothing
  return x
end

degree(x::T) where T <: Zmodn_poly = @ccall libflint.nmod_poly_degree(x::Ref{T})::Int

function coeff(x::T, n::Int) where T <: Zmodn_poly
  n < 0 && throw(DomainError(n, "Index must be non-negative"))
  return base_ring(x)(@ccall libflint.nmod_poly_get_coeff_ui(x::Ref{T}, n::Int)::UInt)
end

function coeff_raw(x::T, n::Int) where T <: Zmodn_poly
  return @ccall libflint.nmod_poly_get_coeff_ui(x::Ref{T}, n::Int)::UInt
end

zero(R::zzModPolyRing) = R(UInt(0))

one(R::zzModPolyRing) = R(UInt(1))

gen(R::zzModPolyRing) = R([zero(base_ring(R)), one(base_ring(R))])

is_gen(a::T) where T <: Zmodn_poly = (degree(a) <= 1 &&
                                      iszero(coeff(a,0)) && isone(coeff(a,1)))

iszero(a::T) where T <: Zmodn_poly = Bool(@ccall libflint.nmod_poly_is_zero(a::Ref{T})::Int32)

modulus(a::T) where T <: Zmodn_poly = modulus(parent(a))

modulus(R::zzModPolyRing) = modulus(base_ring(R))

var(R::zzModPolyRing) = R.S

function deepcopy_internal(a::zzModPolyRingElem, dict::IdDict)
  z = zzModPolyRingElem(modulus(a), a)
  z.parent = a.parent
  return z
end

###############################################################################
#
#   Similar
#
###############################################################################

function similar(f::PolyRingElem, R::zzModRing, s::Symbol=var(parent(f)); cached::Bool=true)
  z = zzModPolyRingElem(R.n)
  if base_ring(f) === R && s == var(parent(f)) && f isa zzModPolyRingElem
    # steal parent in case it is not cached
    z.parent = parent(f)
  else
    z.parent = zzModPolyRing(R, s, cached)
  end
  return z
end

###############################################################################
#
#   polynomial constructor
#
###############################################################################

function polynomial(R::zzModRing, arr::Vector{T}, var::VarName=:x; cached::Bool=true) where T
  coeffs = map(R, arr)
  coeffs = length(coeffs) == 0 ? zzModRingElem[] : coeffs
  z = zzModPolyRingElem(R.n, coeffs)
  z.parent = zzModPolyRing(R, Symbol(var), cached)
  return z
end

################################################################################
#
#  Unary operations
#
################################################################################

-(x::T) where T <: Zmodn_poly = neg!(parent(x)(), x)

################################################################################
#
#   Binary operations
#
################################################################################

function +(x::T, y::T) where T <: Zmodn_poly
  check_parent(x,y)
  z = parent(x)()
  @ccall libflint.nmod_poly_add(z::Ref{T}, x::Ref{T}, y::Ref{T})::Nothing
  return z
end

function -(x::T, y::T) where T <: Zmodn_poly
  check_parent(x,y)
  z = parent(x)()
  @ccall libflint.nmod_poly_sub(z::Ref{T}, x::Ref{T}, y::Ref{T})::Nothing
  return z
end

function *(x::T, y::T) where T <: Zmodn_poly
  check_parent(x,y)
  z = parent(x)()
  @ccall libflint.nmod_poly_mul(z::Ref{T}, x::Ref{T}, y::Ref{T})::Nothing
  return z
end

###############################################################################
#
#  Ad hoc binary operations
#
###############################################################################

function *(x::T, y::UInt) where T <: Zmodn_poly
  z = parent(x)()
  return mul!(z, x, y % modulus(x))
end

*(x::UInt, y::T) where T <: Zmodn_poly = y*x

function *(x::T, y::ZZRingElem) where T <: Zmodn_poly
  z = parent(x)()
  t = ZZRingElem()
  tt = UInt(0)
  @ccall libflint.fmpz_mod_ui(t::Ref{ZZRingElem}, y::Ref{ZZRingElem}, modulus(x)::UInt)::UInt
  tt = @ccall libflint.fmpz_get_ui(t::Ref{ZZRingElem})::UInt
  return x*tt
end

*(x::ZZRingElem, y::T) where T <: Zmodn_poly = y*x

*(x::T, y::Integer) where T <: Zmodn_poly = x*ZZRingElem(y)

*(x::Integer, y::T) where T <: Zmodn_poly = y*x

function *(x::zzModPolyRingElem, y::zzModRingElem)
  (base_ring(x) != parent(y)) && error("Must have same parent")
  return x*y.data
end

*(x::zzModRingElem, y::zzModPolyRingElem) = y*x

function +(x::T, y::UInt) where T <: Zmodn_poly
  z = parent(x)()
  y %= modulus(x)
  @ccall libflint.nmod_poly_add_ui(z::Ref{T}, x::Ref{T}, y::UInt)::Nothing
  return z
end

+(x::UInt, y::T) where T <: Zmodn_poly = y + x

function +(x::T, y::ZZRingElem) where T <: Zmodn_poly
  z = parent(x)()
  t = ZZRingElem()
  tt = UInt(0)
  @ccall libflint.fmpz_mod_ui(t::Ref{ZZRingElem}, y::Ref{ZZRingElem}, modulus(x)::UInt)::UInt
  tt = @ccall libflint.fmpz_get_ui(t::Ref{ZZRingElem})::UInt
  return +(x,tt)
end

+(x::ZZRingElem, y::T) where T <: Zmodn_poly = y + x

+(x::T, y::Integer) where T <: Zmodn_poly = x + ZZRingElem(y)

+(x::Integer, y::T) where T <: Zmodn_poly = y + x

function +(x::zzModPolyRingElem, y::zzModRingElem)
  (base_ring(x) != parent(y)) && error("Elements must have same parent")
  return +(x, y.data)
end

+(x::zzModRingElem, y::zzModPolyRingElem) = y + x

function -(x::T, y::UInt) where T <: Zmodn_poly
  z = parent(x)()
  y %= modulus(x)
  @ccall libflint.nmod_poly_sub_ui(z::Ref{T}, x::Ref{T}, y::UInt)::Nothing
  return z
end

-(x::UInt, y::T) where T <: Zmodn_poly = -(y - x)

function -(x::T, y::ZZRingElem) where T <: Zmodn_poly
  z = parent(x)()
  t = ZZRingElem()
  tt = UInt(0)
  @ccall libflint.fmpz_mod_ui(t::Ref{ZZRingElem}, y::Ref{ZZRingElem}, modulus(x)::UInt)::UInt
  tt = @ccall libflint.fmpz_get_ui(t::Ref{ZZRingElem})::UInt
  return -(x,tt)
end

-(x::ZZRingElem, y::T) where T <: Zmodn_poly = -(y - x)

-(x::T, y::Integer) where T <: Zmodn_poly = x - ZZRingElem(y)

-(x::Integer, y::T) where T <: Zmodn_poly = -(y - x)

function -(x::zzModPolyRingElem, y::zzModRingElem)
  (base_ring(x) != parent(y)) && error("Elements must have same parent")
  return -(x,y.data)
end

-(x::zzModRingElem, y::zzModPolyRingElem) = -(y - x)

################################################################################
#
#  Powering
#
################################################################################

function ^(x::T, y::Int) where T <: Zmodn_poly
  y < 0 && throw(DomainError(y, "Exponent must be non-negative"))
  z = parent(x)()
  @ccall libflint.nmod_poly_pow(z::Ref{T}, x::Ref{T}, y::Int)::Nothing
  return z
end

################################################################################
#
#  Comparison
#
################################################################################

function ==(x::T, y::T) where T <: Zmodn_poly
  check_parent(x, y)
  return Bool(@ccall libflint.nmod_poly_equal(x::Ref{T}, y::Ref{T})::Int32)
end

isequal(x::T, y::T) where T <: Zmodn_poly = x == y

################################################################################
#
#  Ad hoc comparisons
#
################################################################################

function ==(x::zzModPolyRingElem, y::zzModRingElem)
  base_ring(x) != parent(y) && error("Incompatible base rings in comparison")
  if length(x) > 1
    return false
  elseif length(x) == 1
    u = @ccall libflint.nmod_poly_get_coeff_ui(x::Ref{zzModPolyRingElem}, 0::Int)::UInt
    return u == y
  else
    return iszero(y)
  end
end

==(x::zzModRingElem, y::zzModPolyRingElem) = y == x

################################################################################
#
#  Truncation
#
################################################################################

function truncate(a::T, n::Int) where T <: Zmodn_poly
  n < 0 && throw(DomainError(n, "Index must be non-negative"))
  z = deepcopy(a)
  if length(z) <= n
    return z
  end
  @ccall libflint.nmod_poly_truncate(z::Ref{T}, n::Int)::Nothing
  return z
end

function mullow(x::T, y::T, n::Int) where T <: Zmodn_poly
  check_parent(x, y)
  n < 0 && throw(DomainError(n, "Index must be non-negative"))
  z = parent(x)()
  @ccall libflint.nmod_poly_mullow(z::Ref{T}, x::Ref{T}, y::Ref{T}, n::Int)::Nothing
  return z
end

###############################################################################
#
#   Reversal
#
###############################################################################

function reverse(x::T, len::Int) where T <: Zmodn_poly
  len < 0 && throw(DomainError(len, "Index must be non-negative"))
  return reverse!(parent(x)(), x, len)
end

function reverse!(z::T, x::T, len::Int) where T <: Zmodn_poly
  @ccall libflint.nmod_poly_reverse(z::Ref{T}, x::Ref{T}, len::Int)::Nothing
  return z
end

###############################################################################
#
#   Shifting
#
###############################################################################

function shift_left(x::T, len::Int) where T <: Zmodn_poly
  len < 0 && throw(DomainError(len, "Shift must be non-negative"))
  z = parent(x)()
  @ccall libflint.nmod_poly_shift_left(z::Ref{T}, x::Ref{T}, len::Int)::Nothing
  return z
end

function shift_right(x::T, len::Int) where T <: Zmodn_poly
  len < 0 && throw(DomainError(len, "Shift must be non-negative"))
  z = parent(x)()
  @ccall libflint.nmod_poly_shift_right(z::Ref{T}, x::Ref{T}, len::Int)::Nothing
  return z
end

###############################################################################
#
#   Exact division
#
###############################################################################

function divexact(x::zzModPolyRingElem, y::zzModPolyRingElem; check::Bool=true)
  check_parent(x, y)
  iszero(y) && throw(DivideError())
  lead_is_unit_or_throw(y)
  z = parent(x)()
  @ccall libflint.nmod_poly_div(z::Ref{zzModPolyRingElem}, x::Ref{zzModPolyRingElem}, y::Ref{zzModPolyRingElem})::Nothing
  return z
end

################################################################################
#
#  Ad hoc exact division
#
################################################################################

function divexact(x::zzModPolyRingElem, y::zzModRingElem; check::Bool=true)
  base_ring(x) != parent(y) && error("Elements must have same parent")
  iszero(y) && throw(DivideError())
  return divexact(x, parent(x)(y); check=check)
end

function divexact(x::T, y::ZZRingElem; check::Bool=true) where T <: Zmodn_poly
  iszero(y) && throw(DivideError())
  return divexact(x, parent(x)(y); check=check)
end

function divexact(x::T, y::Int; check::Bool=true) where T <: Zmodn_poly
  y == 0 && throw(DivideError())
  return divexact(x, parent(x)(y); check=check)
end

################################################################################
#
#  Division with remainder
#
################################################################################

function Base.divrem(x::zzModPolyRingElem, y::zzModPolyRingElem)
  check_parent(x,y)
  iszero(y) && throw(DivideError())
  lead_is_unit_or_throw(y)
  q = parent(x)()
  r = parent(x)()
  @ccall libflint.nmod_poly_divrem(q::Ref{zzModPolyRingElem}, r::Ref{zzModPolyRingElem}, x::Ref{zzModPolyRingElem}, y::Ref{zzModPolyRingElem})::Nothing
  return q, r
end

function Base.div(x::zzModPolyRingElem, y::zzModPolyRingElem)
  check_parent(x,y)
  iszero(y) && throw(DivideError())
  lead_is_unit_or_throw(y)
  q = parent(x)()
  @ccall libflint.nmod_poly_div(q::Ref{zzModPolyRingElem}, x::Ref{zzModPolyRingElem}, y::Ref{zzModPolyRingElem})::Nothing
  return q
end

################################################################################
#
#  Remainder
#
################################################################################

function rem(x::zzModPolyRingElem, y::zzModPolyRingElem)
  check_parent(x,y)
  iszero(y) && throw(DivideError())
  lead_is_unit_or_throw(y)
  z = parent(x)()
  @ccall libflint.nmod_poly_rem(z::Ref{zzModPolyRingElem}, x::Ref{zzModPolyRingElem}, y::Ref{zzModPolyRingElem})::Nothing
  return z
end

mod(x::T, y::T) where T <: Zmodn_poly = rem(x, y)

################################################################################
#
#  GCD
#
################################################################################

function AbstractAlgebra.hgcd_prefers_basecase(a::zzModPolyRingElem, b::zzModPolyRingElem)
  return length(b) < 100
end

function AbstractAlgebra.mat22_mul_prefers_classical(
    a11::zzModPolyRingElem, a12::zzModPolyRingElem, a21::zzModPolyRingElem, a22::zzModPolyRingElem,
    b11::zzModPolyRingElem, b12::zzModPolyRingElem, b21::zzModPolyRingElem, b22::zzModPolyRingElem)
  return length(a11) + length(a22) < 30 || length(b11) + length(b22) < 30
end

# Let AA do the gcd, gcdx, and gcdinv

################################################################################
#
#  Modular arithmetic
#
################################################################################

function invmod(x::T, y::T) where T <: Zmodn_poly
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
  r = @ccall libflint.nmod_poly_invmod(z::Ref{T}, x::Ref{T}, y::Ref{T})::Int32
  r == 0 ? error("Impossible inverse in invmod") : return z
end

function mulmod(x::T, y::T, z::T) where T <: Zmodn_poly
  check_parent(x,y)
  check_parent(y,z)
  w = parent(x)()
  @ccall libflint.nmod_poly_mulmod(w::Ref{T}, x::Ref{T}, y::Ref{T}, z::Ref{T})::Nothing
  return w
end

function powermod(x::T, e::Int, y::T) where T <: Zmodn_poly
  check_parent(x,y)
  z = parent(x)()

  if e < 0
    g, x = gcdinv(x, y)
    if g != 1
      error("Element not invertible")
    end
    e = -e
  end

  @ccall libflint.nmod_poly_powmod_ui_binexp(z::Ref{T}, x::Ref{T}, e::Int, y::Ref{T})::Nothing

  return z
end

################################################################################
#
#  Resultant
#
################################################################################

function resultant(x::zzModPolyRingElem, y::zzModPolyRingElem,  check::Bool = true)
  if check
    check_parent(x,y)
    !is_prime(modulus(x)) && error("Modulus not prime in resultant")
  end
  r = @ccall libflint.nmod_poly_resultant(x::Ref{zzModPolyRingElem}, y::Ref{zzModPolyRingElem})::UInt
  return base_ring(x)(r)
end

################################################################################
#
#  Evaluation
#
################################################################################

function evaluate(x::zzModPolyRingElem, y::zzModRingElem)
  base_ring(x) != parent(y) && error("Elements must have same parent")
  z = @ccall libflint.nmod_poly_evaluate_nmod(x::Ref{zzModPolyRingElem}, y.data::UInt)::UInt
  return parent(y)(z)
end

################################################################################
#
#  Derivative
#
################################################################################

function derivative(x::T) where T <: Zmodn_poly
  z = parent(x)()
  @ccall libflint.nmod_poly_derivative(z::Ref{T}, x::Ref{T})::Nothing
  return z
end

################################################################################
#
#  Integral
#
################################################################################

function integral(x::T) where T <: Zmodn_poly
  z = parent(x)()
  @ccall libflint.nmod_poly_integral(z::Ref{T}, x::Ref{T})::Nothing
  return z
end

################################################################################
#
#  Composition
#
################################################################################

function AbstractAlgebra._compose_right(x::T, y::T) where T <: Zmodn_poly
  check_parent(x,y)
  z = parent(x)()
  @ccall libflint.nmod_poly_compose(z::Ref{T}, x::Ref{T}, y::Ref{T})::Nothing
  return z
end

################################################################################
#
#  Interpolation
#
################################################################################

function interpolate(R::zzModPolyRing, x::Vector{zzModRingElem},
    y::Vector{zzModRingElem})
  z = R()

  ax = Vector{UInt}(undef, length(x))
  ay = Vector{UInt}(undef, length(y))

  for i in 1:length(x)
    ax[i] = x[i].data

    ay[i] = y[i].data
  end
  @ccall libflint.nmod_poly_interpolate_nmod_vec(z::Ref{zzModPolyRingElem}, ax::Ptr{UInt}, ay::Ptr{UInt}, length(x)::Int)::Nothing
  return z
end

################################################################################
#
#  Inflation and Deflation
#
################################################################################

function inflate(x::T, n::Int) where T <: Zmodn_poly
  n < 0 && throw(DomainError(n, "Cannot inflate by a negative number."))
  z = parent(x)()
  @ccall libflint.nmod_poly_inflate(z::Ref{T}, x::Ref{T}, UInt(n)::UInt)::Nothing
  return z
end

function deflate(x::T, n::Int) where T <: Zmodn_poly
  n < 0 && throw(DomainError(n, "Cannot deflate by a negative number."))
  z = parent(x)()
  @ccall libflint.nmod_poly_deflate(z::Ref{T}, x::Ref{T}, UInt(n)::UInt)::Nothing
  return z
end

################################################################################
#
#  Lifting
#
################################################################################

@doc raw"""
    lift(R::ZZPolyRing, y::zzModPolyRingElem)

Lift from a polynomial over $\mathbb{Z}/n\mathbb{Z}$ to a polynomial over
$\mathbb{Z}$ with minimal reduced non-negative coefficients. The ring `R`
specifies the ring to lift into.
"""
function lift(R::ZZPolyRing, y::zzModPolyRingElem)
  z = ZZPolyRingElem()
  @ccall libflint.fmpz_poly_set_nmod_poly_unsigned(z::Ref{ZZPolyRingElem}, y::Ref{zzModPolyRingElem})::Nothing
  z.parent = R
  return z
end

################################################################################
#
#  Irreducibility
#
################################################################################

function is_irreducible(x::zzModPolyRingElem)
  !is_prime(modulus(x)) && error("Modulus not prime in is_irreducible")
  is_constant(x) && return false
  return Bool(@ccall libflint.nmod_poly_is_irreducible(x::Ref{zzModPolyRingElem})::Int32)
end

################################################################################
#
#  Squarefree testing
#
################################################################################

function is_squarefree(x::zzModPolyRingElem)
  !is_prime(modulus(x)) && error("Modulus not prime in is_squarefree")
  return Bool(@ccall libflint.nmod_poly_is_squarefree(x::Ref{zzModPolyRingElem})::Int32)
end

################################################################################
#
#  Factorization
#
################################################################################

function factor(x::zzModPolyRingElem)
  iszero(x) && throw(ArgumentError("Argument must be non-zero"))
  fac, z = _factor(x)
  return Fac(parent(x)(z), fac)
end

function _factor(x::zzModPolyRingElem)
  !is_prime(modulus(x)) && error("Modulus not prime in factor")
  fac = nmod_poly_factor(x.mod_n)
  z = @ccall libflint.nmod_poly_factor(fac::Ref{nmod_poly_factor}, x::Ref{zzModPolyRingElem})::UInt
  res = Dict{zzModPolyRingElem,Int}()
  for i in 1:fac.num
    f = parent(x)()
    @ccall libflint.nmod_poly_factor_get_poly(f::Ref{zzModPolyRingElem}, fac::Ref{nmod_poly_factor}, (i-1)::Int)::Nothing
    e = unsafe_load(fac.exp,i)
    res[f] = e
  end
  return res, base_ring(x)(z)
end

function factor_squarefree(x::zzModPolyRingElem)
  iszero(x) && throw(ArgumentError("Argument must be non-zero"))
  !is_prime(modulus(x)) && error("Modulus not prime in factor_squarefree")
  return Fac(parent(x)(leading_coefficient(x)), _factor_squarefree(x))
end

function _factor_squarefree(x::zzModPolyRingElem)
  fac = nmod_poly_factor(x.mod_n)
  @ccall libflint.nmod_poly_factor_squarefree(fac::Ref{nmod_poly_factor}, x::Ref{zzModPolyRingElem})::UInt
  res = Dict{zzModPolyRingElem,Int}()
  for i in 1:fac.num
    f = parent(x)()
    @ccall libflint.nmod_poly_factor_get_poly(f::Ref{zzModPolyRingElem}, fac::Ref{nmod_poly_factor}, (i-1)::Int)::Nothing
    e = unsafe_load(fac.exp,i)
    res[f] = e
  end
  return res
end

@doc raw"""
    factor_distinct_deg(x::zzModPolyRingElem)

Return the distinct degree factorisation of a squarefree polynomial $x$.
"""
function factor_distinct_deg(x::zzModPolyRingElem)
  !is_squarefree(x) && error("Polynomial must be squarefree")
  !is_prime(modulus(x)) && error("Modulus not prime in factor_distinct_deg")
  degs = Vector{Int}(undef, degree(x))
  degss = [ pointer(degs) ]
  fac = nmod_poly_factor(x.mod_n)
  @ccall libflint.nmod_poly_factor_distinct_deg(fac::Ref{nmod_poly_factor}, x::Ref{zzModPolyRingElem}, degss::Ptr{Ptr{Int}})::UInt
  res = Dict{Int,zzModPolyRingElem}()
  for i in 1:fac.num
    f = parent(x)()
    @ccall libflint.nmod_poly_factor_get_poly(f::Ref{zzModPolyRingElem}, fac::Ref{nmod_poly_factor}, (i-1)::Int)::Nothing
    res[degs[i]] = f
  end
  return res
end

function factor_shape(x::PolyRingElem{T}) where {T <: RingElem}
  res = Dict{Int, Int}()
  square_fac = factor_squarefree(x)
  for (f, i) in square_fac
    discdeg = factor_distinct_deg(f)
    for (j,g) in discdeg
      num = div(degree(g), j)*i
      if haskey(res, j)
        res[j] += num
      else
        res[j] = num
      end
    end
  end
  return res
end

function roots(a::zzModPolyRingElem)
  R = parent(a)
  n = modulus(R)
  fac = nmod_poly_factor(n)
  if is_prime(n)
    @ccall libflint.nmod_poly_roots(fac::Ref{nmod_poly_factor}, a::Ref{zzModPolyRingElem}, 0::Cint)::UInt
  else
    nfac = n_factor()
    @ccall libflint.n_factor(nfac::Ref{n_factor}, n::UInt)::Nothing
    @ccall libflint.nmod_poly_roots_factored(fac::Ref{nmod_poly_factor}, a::Ref{zzModPolyRingElem}, 0::Cint, nfac::Ref{n_factor})::UInt
  end
  f = R()
  res = zzModRingElem[]
  for i in 1:fac.num
    @ccall libflint.nmod_poly_factor_get_poly(f::Ref{zzModPolyRingElem}, fac::Ref{nmod_poly_factor}, (i - 1)::Int)::Nothing
    @assert isone(coeff(f, 1))
    push!(res, -coeff(f, 0))
  end
  return res
end

################################################################################
#
#   Remove and valuation
#
################################################################################

# currently returns Tuple{::Bool, ::Int}, but the Int is supposed to be replaced
# by a type that include infinity
function _remove_check_simple_cases(a, b)
  parent(a) == parent(b) || error("Incompatible parents")
  if (iszero(b) || is_unit(b))
    throw(ArgumentError("Second argument must be a non-zero non-unit"))
  end
  if iszero(a)
    error("Not yet implemented")
    return (true, 0) # TODO return infinity instead of 0
  end
  return (false, 0)
end

function remove(z::zzModPolyRingElem, p::zzModPolyRingElem)
  ok, v = _remove_check_simple_cases(z, p)
  ok && return v, zero(parent(z))
  z = deepcopy(z)
  v = @ccall libflint.nmod_poly_remove(z::Ref{zzModPolyRingElem}, p::Ref{zzModPolyRingElem})::Int
  return v, z
end

function divides(z::T, x::T) where T <: Zmodn_poly
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
#  Speedups for rings over zzModPolyRingElem
#
################################################################################

function det(M::Generic.Mat{zzModPolyRingElem})
  nrows(M) != ncols(M) && error("Not a square matrix in det")

  if is_prime(modulus(base_ring(M)))
    return det_popov(M)
  end

  try
    return AbstractAlgebra.det_fflu(M)
  catch
    return AbstractAlgebra.det_df(M)
  end
end

################################################################################
#
#  Unsafe functions
#
################################################################################

function zero!(x::T) where T <: Zmodn_poly
  @ccall libflint.nmod_poly_zero(x::Ref{T})::Nothing
  return x
end

function one!(a::T) where T <: Zmodn_poly
  @ccall libflint.nmod_poly_one(a::Ref{T})::Nothing
  return a
end

function neg!(z::T, a::T) where T <: Zmodn_poly
  @ccall libflint.nmod_poly_neg(z::Ref{T}, a::Ref{T})::Nothing
  return z
end

function fit!(x::T, n::Int) where T <: Zmodn_poly
  @ccall libflint.nmod_poly_fit_length(x::Ref{T}, n::Int)::Nothing
  return nothing
end

function setcoeff!(x::T, n::Int, y::UInt) where T <: Zmodn_poly
  @ccall libflint.nmod_poly_set_coeff_ui(x::Ref{T}, n::Int, y::UInt)::Nothing
  return x
end

function setcoeff!(x::T, n::Int, y::Int) where T <: Zmodn_poly
  @ccall libflint.nmod_poly_set_coeff_ui(x::Ref{T}, n::Int, mod(y, x.mod_n)::UInt)::Nothing
  return x
end

function setcoeff!(x::T, n::Int, y::ZZRingElem) where T <: Zmodn_poly
  r = @ccall libflint.fmpz_fdiv_ui(y::Ref{ZZRingElem}, x.mod_n::UInt)::UInt
  setcoeff!(x, n, r)
  return x
end

setcoeff!(x::T, n::Int, y::Integer) where T <: Zmodn_poly = setcoeff!(x, n, ZZRingElem(y))

setcoeff!(x::zzModPolyRingElem, n::Int, y::zzModRingElem) = setcoeff!(x, n, y.data)

function add!(z::T, x::T, y::T) where T <: Zmodn_poly
  @ccall libflint.nmod_poly_add(z::Ref{T}, x::Ref{T}, y::Ref{T})::Nothing
  return z
end

function sub!(z::T, x::T, y::T) where T <: Zmodn_poly
  @ccall libflint.nmod_poly_sub(z::Ref{T}, x::Ref{T}, y::Ref{T})::Nothing
  return z
end

function mul!(z::T, x::T, y::T) where T <: Zmodn_poly
  @ccall libflint.nmod_poly_mul(z::Ref{T}, x::Ref{T}, y::Ref{T})::Nothing
  return z
end

function mul!(z::T, x::T, y::UInt) where T <: Zmodn_poly
  @ccall libflint.nmod_poly_scalar_mul_nmod(z::Ref{T}, x::Ref{T}, y::UInt)::Nothing
  return z
end

################################################################################
#
#  Promotion rules
#
################################################################################

promote_rule(::Type{zzModPolyRingElem}, ::Type{V}) where {V <: Integer} = zzModPolyRingElem

promote_rule(::Type{zzModPolyRingElem}, ::Type{ZZRingElem}) = zzModPolyRingElem

promote_rule(::Type{zzModPolyRingElem}, ::Type{zzModRingElem}) = zzModPolyRingElem

################################################################################
#
#  Parent object call overloads
#
################################################################################

(R::zzModPolyRing)() = zzModPolyRingElem(R)

function (R::zzModPolyRing)(x::ZZRingElem)
  r = @ccall libflint.fmpz_fdiv_ui(x::Ref{ZZRingElem}, modulus(R)::UInt)::UInt
  return zzModPolyRingElem(R, r)
end

(R::zzModPolyRing)(x::Integer) = zzModPolyRingElem(R, x)

function (R::zzModPolyRing)(x::zzModPolyRingElem)
  R != parent(x) && error("Wrong parents")
  return x
end

function (R::zzModPolyRing)(x::zzModRingElem)
  base_ring(R) != parent(x) && error("Wrong parents")
  return zzModPolyRingElem(R, x.data)
end

function (R::zzModPolyRing)(arr::Vector{ZZRingElem})
  return zzModPolyRingElem(R, arr)
end

function (R::zzModPolyRing)(arr::Vector{UInt})
  return zzModPolyRingElem(R, arr)
end

(R::zzModPolyRing)(arr::Vector{T}) where {T <: Integer} = R(map(base_ring(R), arr))

function (R::zzModPolyRing)(arr::Vector{zzModRingElem})
  @req all(parent(e) == base_ring(R) for e in arr) "parents do not match"
  return zzModPolyRingElem(R, arr)
end
