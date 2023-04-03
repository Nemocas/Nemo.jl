################################################################################
#
#  nmod_poly.jl : Flint nmod_poly (polynomials over Z/nZ, small modulus)
#
################################################################################

export zzModPolyRing, zzModPolyRingElem, parent, base_ring, elem_type, length, zero,
       one, gen, is_gen, iszero, var, deepcopy, show, truncate, mullow, reverse,
       shift_left, shift_right, divexact, rem, gcd, resultant,
       evaluate, derivative, compose, interpolate, inflate, deflate, lift,
       is_irreducible, is_squarefree, factor, factor_squarefree,
       factor_distinct_deg, factor_shape, setcoeff!, canonical_unit,
       add!, sub!, mul!, polynomial_ring, check_parent, gcdx, mod,
       invmod, gcdinv, mulmod, powermod, zero!, one!

################################################################################
#
#  Type and parent object methods
#
################################################################################

parent(a::zzModPolyRingElem) = a.parent

base_ring(R::zzModPolyRing) = R.base_ring

base_ring(a::zzModPolyRingElem) = base_ring(parent(a))

parent_type(::Type{zzModPolyRingElem}) = zzModPolyRing

elem_type(::Type{zzModPolyRingElem}) = zzModPolyRingElem

elem_type(::Type{zzModPolyRing}) = zzModPolyRingElem

dense_poly_type(::Type{zzModRingElem}) = zzModPolyRingElem

function check_parent(x::T, y::T) where T <: Zmodn_poly
  parent(x) != parent(y) && error("Parents must coincide")
  nothing
end

################################################################################
#
#   Basic helper
#
################################################################################

function lead_is_unit_or_throw(a::zzModPolyRingElem)
   d = degree(a)
   u = ccall((:nmod_poly_get_coeff_ui, libflint), UInt, (Ref{zzModPolyRingElem}, Int), a, d)
   n = ccall((:n_gcd, libflint), UInt, (UInt, UInt), u, modulus(a))
   if n != 1
      R = base_ring(a)
      throw(NotInvertibleError(R(n), R))
   end
end

function Base.hash(a::zzModPolyRingElem, h::UInt)
   b = 0x53dd43cd511044d1%UInt
   for i in 0:length(a) - 1
      u = ccall((:nmod_poly_get_coeff_ui, libflint), UInt, (Ref{zzModPolyRingElem}, Int), a, i)
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

length(x::T) where T <: Zmodn_poly = ccall((:nmod_poly_length, libflint), Int,
                               (Ref{T}, ), x)

degree(x::T) where T <: Zmodn_poly = ccall((:nmod_poly_degree, libflint), Int,
                               (Ref{T}, ), x)

function coeff(x::T, n::Int) where T <: Zmodn_poly
  n < 0 && throw(DomainError(n, "Index must be non-negative"))
  return base_ring(x)(ccall((:nmod_poly_get_coeff_ui, libflint), UInt,
          (Ref{T}, Int), x, n))
end

function coeff_raw(x::T, n::Int) where T <: Zmodn_poly
  return ccall((:nmod_poly_get_coeff_ui, libflint), UInt,
                (Ref{T}, Int), x, n)
end

zero(R::zzModPolyRing) = R(UInt(0))

one(R::zzModPolyRing) = R(UInt(1))

gen(R::zzModPolyRing) = R([zero(base_ring(R)), one(base_ring(R))])

is_gen(a::T) where T <: Zmodn_poly = (degree(a) == 1 &&
                              iszero(coeff(a,0)) && isone(coeff(a,1)))

iszero(a::T) where T <: Zmodn_poly = Bool(ccall((:nmod_poly_is_zero, libflint), Int32,
                              (Ref{T}, ), a))

modulus(a::T) where T <: Zmodn_poly = a.parent.n

modulus(R::zzModPolyRing) = R.n

var(R::zzModPolyRing) = R.S

function deepcopy_internal(a::zzModPolyRingElem, dict::IdDict)
  z = zzModPolyRingElem(modulus(a), a)
  z.parent = a.parent
  return z
end

characteristic(R::zzModPolyRing) = modulus(R)

###############################################################################
#
#   Similar
#
###############################################################################

function similar(f::PolyRingElem, R::zzModRing, s::Symbol=var(parent(f)); cached::Bool=true)
   z = zzModPolyRingElem(R.n)
   if base_ring(f) === R && s == var(parent(f)) && typeof(f) == zzModPolyRingElem
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

function polynomial(R::zzModRing, arr::Vector{T}, var::String="x"; cached::Bool=true) where T
   coeffs = map(R, arr)
   coeffs = length(coeffs) == 0 ? zzModRingElem[] : coeffs
   z = zzModPolyRingElem(R.n, coeffs)
   z.parent = zzModPolyRing(R, Symbol(var), cached)
   return z
end

################################################################################
#
#  AbstractString I/O
#
################################################################################

function show(io::IO, R::zzModPolyRing)
  print(io, "Univariate Polynomial Ring in ")
  print(io, string(var(R)))
  print(io, " over ")
  print(io, base_ring(R))
end

################################################################################
#
#  Canonicalization
#
################################################################################

function canonical_unit(a::T) where T <: Zmodn_poly
  return canonical_unit(leading_coefficient(a))
end

################################################################################
#
#  Unary operations
#
################################################################################

function -(x::T) where T <: Zmodn_poly
  z = parent(x)()
  ccall((:nmod_poly_neg, libflint), Nothing,
          (Ref{T}, Ref{T}), z, x)
  return z
end

################################################################################
#
#   Binary operations
#
################################################################################

function +(x::T, y::T) where T <: Zmodn_poly
  check_parent(x,y)
  z = parent(x)()
  ccall((:nmod_poly_add, libflint), Nothing,
          (Ref{T}, Ref{T},  Ref{T}), z, x, y)
  return z
end

function -(x::T, y::T) where T <: Zmodn_poly
  check_parent(x,y)
  z = parent(x)()
  ccall((:nmod_poly_sub, libflint), Nothing,
          (Ref{T}, Ref{T},  Ref{T}), z, x, y)
  return z
end

function *(x::T, y::T) where T <: Zmodn_poly
  check_parent(x,y)
  z = parent(x)()
  ccall((:nmod_poly_mul, libflint), Nothing,
          (Ref{T}, Ref{T},  Ref{T}), z, x, y)
  return z
end

###############################################################################
#
#  Ad hoc binary operations
#
###############################################################################

function *(x::T, y::UInt) where T <: Zmodn_poly
  z = parent(x)()
  ccall((:nmod_poly_scalar_mul_nmod, libflint), Nothing,
          (Ref{T}, Ref{T}, UInt), z, x, y)
  return z
end

*(x::UInt, y::T) where T <: Zmodn_poly = y*x

function *(x::T, y::ZZRingElem) where T <: Zmodn_poly
  z = parent(x)()
  t = ZZRingElem()
  tt = UInt(0)
  ccall((:fmpz_mod_ui, libflint), UInt,
                (Ref{ZZRingElem}, Ref{ZZRingElem}, UInt), t, y, parent(x).n)
  tt = ccall((:fmpz_get_ui, libflint), UInt, (Ref{ZZRingElem}, ), t)
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
  ccall((:nmod_poly_add_ui, libflint), Nothing,
    (Ref{T}, Ref{T}, UInt), z, x, y)
  return z
end

+(x::UInt, y::T) where T <: Zmodn_poly = y + x

function +(x::T, y::ZZRingElem) where T <: Zmodn_poly
  z = parent(x)()
  t = ZZRingElem()
  tt = UInt(0)
  ccall((:fmpz_mod_ui, libflint), UInt,
                (Ref{ZZRingElem}, Ref{ZZRingElem}, UInt), t, y, parent(x).n)
  tt = ccall((:fmpz_get_ui, libflint), UInt, (Ref{ZZRingElem}, ), t)
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
  ccall((:nmod_poly_sub_ui, libflint), Nothing,
    (Ref{T}, Ref{T}, UInt), z, x, y)
  return z
end

-(x::UInt, y::T) where T <: Zmodn_poly = -(y - x)

function -(x::T, y::ZZRingElem) where T <: Zmodn_poly
  z = parent(x)()
  t = ZZRingElem()
  tt = UInt(0)
  ccall((:fmpz_mod_ui, libflint), UInt,
                (Ref{ZZRingElem}, Ref{ZZRingElem}, UInt), t, y, parent(x).n)
  tt = ccall((:fmpz_get_ui, libflint), UInt, (Ref{ZZRingElem}, ), t)
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
  y < 0 && throw(DomainError(y, "Exponent must be nonnegative"))
  z = parent(x)()
  ccall((:nmod_poly_pow, libflint), Nothing,
          (Ref{T}, Ref{T}, Int), z, x, y)
  return z
end

################################################################################
#
#  Comparison
#
################################################################################

function ==(x::T, y::T) where T <: Zmodn_poly
  check_parent(x, y)
  return Bool(ccall((:nmod_poly_equal, libflint), Int32,
          (Ref{T}, Ref{T}), x, y))
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
    u = ccall((:nmod_poly_get_coeff_ui, libflint), UInt,
            (Ref{zzModPolyRingElem}, Int), x, 0)
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
  ccall((:nmod_poly_truncate, libflint), Nothing,
          (Ref{T}, Int), z, n)
  return z
end

function mullow(x::T, y::T, n::Int) where T <: Zmodn_poly
  check_parent(x, y)
  n < 0 && throw(DomainError(n, "Index must be non-negative"))
  z = parent(x)()
  ccall((:nmod_poly_mullow, libflint), Nothing,
          (Ref{T}, Ref{T}, Ref{T}, Int), z, x, y, n)
  return z
end

###############################################################################
#
#   Reversal
#
###############################################################################

function reverse(x::T, len::Int) where T <: Zmodn_poly
  len < 0 && throw(DomainError(n, "Index must be non-negative"))
  z = parent(x)()
  ccall((:nmod_poly_reverse, libflint), Nothing,
          (Ref{T}, Ref{T}, Int), z, x, len)
  return z
end

###############################################################################
#
#   Shifting
#
###############################################################################

function shift_left(x::T, len::Int) where T <: Zmodn_poly
  len < 0 && throw(DomainError(len, "Shift must be nonnegative."))
  z = parent(x)()
  ccall((:nmod_poly_shift_left, libflint), Nothing,
          (Ref{T}, Ref{T}, Int), z, x, len)
  return z
end

function shift_right(x::T, len::Int) where T <: Zmodn_poly
  len < 0 && throw(DomainError(len, "Shift must be nonnegative."))
  z = parent(x)()
  ccall((:nmod_poly_shift_right, libflint), Nothing,
            (Ref{T}, Ref{T}, Int), z, x, len)
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
  ccall((:nmod_poly_div, libflint), Nothing,
          (Ref{zzModPolyRingElem}, Ref{zzModPolyRingElem}, Ref{zzModPolyRingElem}), z, x, y)
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
  ccall((:nmod_poly_divrem, libflint), Nothing,
          (Ref{zzModPolyRingElem}, Ref{zzModPolyRingElem}, Ref{zzModPolyRingElem}, Ref{zzModPolyRingElem}),
          q, r, x, y)
  return q, r
end

function Base.div(x::zzModPolyRingElem, y::zzModPolyRingElem)
  check_parent(x,y)
  iszero(y) && throw(DivideError())
  lead_is_unit_or_throw(y)
  q = parent(x)()
  ccall((:nmod_poly_div, libflint), Nothing,
          (Ref{zzModPolyRingElem}, Ref{zzModPolyRingElem}, Ref{zzModPolyRingElem}),
          q, x, y)
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
  ccall((:nmod_poly_rem, libflint), Nothing,
          (Ref{zzModPolyRingElem}, Ref{zzModPolyRingElem}, Ref{zzModPolyRingElem}), z, x, y)
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
  length(y) == 0 && error("Second argument must not be 0")
  check_parent(x,y)
  if length(y) == 1
    return parent(x)(inv(eval(x, coeff(y, 0))))
  end
  z = parent(x)()
  r = ccall((:nmod_poly_invmod, libflint), Int32,
          (Ref{T}, Ref{T}, Ref{T}), z, x, y)
  r == 0 ? error("Impossible inverse in invmod") : return z
end

function mulmod(x::T, y::T, z::T) where T <: Zmodn_poly
  check_parent(x,y)
  check_parent(y,z)
  w = parent(x)()
  ccall((:nmod_poly_mulmod, libflint), Nothing,
        (Ref{T}, Ref{T}, Ref{T}, Ref{T}),
        w, x, y, z)
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

  ccall((:nmod_poly_powmod_ui_binexp, libflint), Nothing,
        (Ref{T}, Ref{T}, Int, Ref{T}), z, x, e, y)

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
  r = ccall((:nmod_poly_resultant, libflint), UInt,
          (Ref{zzModPolyRingElem}, Ref{zzModPolyRingElem}), x, y)
  return base_ring(x)(r)
end

################################################################################
#
#  Evaluation
#
################################################################################

function evaluate(x::zzModPolyRingElem, y::zzModRingElem)
  base_ring(x) != parent(y) && error("Elements must have same parent")
  z = ccall((:nmod_poly_evaluate_nmod, libflint), UInt,
              (Ref{zzModPolyRingElem}, UInt), x, y.data)
  return parent(y)(z)
end

################################################################################
#
#  Derivative
#
################################################################################

function derivative(x::T) where T <: Zmodn_poly
  z = parent(x)()
  ccall((:nmod_poly_derivative, libflint), Nothing,
        (Ref{T}, Ref{T}), z, x)
  return z
end

################################################################################
#
#  Integral
#
################################################################################

function integral(x::T) where T <: Zmodn_poly
  z = parent(x)()
  ccall((:nmod_poly_integral, libflint), Nothing,
        (Ref{T}, Ref{T}), z, x)
  return z
end

################################################################################
#
#  Composition
#
################################################################################

function compose(x::T, y::T) where T <: Zmodn_poly
  check_parent(x,y)
  z = parent(x)()
  ccall((:nmod_poly_compose, libflint), Nothing,
          (Ref{T}, Ref{T}, Ref{T}), z, x, y)
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
  ccall((:nmod_poly_interpolate_nmod_vec, libflint), Nothing,
          (Ref{zzModPolyRingElem}, Ptr{UInt}, Ptr{UInt}, Int),
          z, ax, ay, length(x))
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
  ccall((:nmod_poly_inflate, libflint), Nothing,
          (Ref{T}, Ref{T}, UInt), z, x, UInt(n))
  return z
end

function deflate(x::T, n::Int) where T <: Zmodn_poly
  n < 0 && throw(DomainError(n, "Cannot deflate by a negative number."))
  z = parent(x)()
  ccall((:nmod_poly_deflate, libflint), Nothing,
          (Ref{T}, Ref{T}, UInt), z, x, UInt(n))
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
$\mathbb{Z}$ with minimal reduced nonnegative coefficients. The ring `R`
specifies the ring to lift into.
"""
function lift(R::ZZPolyRing, y::zzModPolyRingElem)
  z = ZZPolyRingElem()
  ccall((:fmpz_poly_set_nmod_poly_unsigned, libflint), Nothing,
          (Ref{ZZPolyRingElem}, Ref{zzModPolyRingElem}), z, y)
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
  return Bool(ccall((:nmod_poly_is_irreducible, libflint), Int32,
          (Ref{zzModPolyRingElem}, ), x))
end

################################################################################
#
#  Squarefree testing
#
################################################################################

function is_squarefree(x::zzModPolyRingElem)
   !is_prime(modulus(x)) && error("Modulus not prime in is_squarefree")
   return Bool(ccall((:nmod_poly_is_squarefree, libflint), Int32,
       (Ref{zzModPolyRingElem}, ), x))
end

################################################################################
#
#  Factorization
#
################################################################################

function factor(x::zzModPolyRingElem)
  fac, z = _factor(x)
  return Fac(parent(x)(z), fac)
end

function _factor(x::zzModPolyRingElem)
  !is_prime(modulus(x)) && error("Modulus not prime in factor")
  fac = nmod_poly_factor(x.mod_n)
  z = ccall((:nmod_poly_factor, libflint), UInt,
          (Ref{nmod_poly_factor}, Ref{zzModPolyRingElem}), fac, x)
  res = Dict{zzModPolyRingElem,Int}()
  for i in 1:fac.num
    f = parent(x)()
    ccall((:nmod_poly_factor_get_poly, libflint), Nothing,
            (Ref{zzModPolyRingElem}, Ref{nmod_poly_factor}, Int), f, fac, i-1)
    e = unsafe_load(fac.exp,i)
    res[f] = e
  end
  return res, base_ring(x)(z)
end

function factor_squarefree(x::zzModPolyRingElem)
  !is_prime(modulus(x)) && error("Modulus not prime in factor_squarefree")
  return Fac(parent(x)(leading_coefficient(x)), _factor_squarefree(x))
end

function _factor_squarefree(x::zzModPolyRingElem)
  fac = nmod_poly_factor(x.mod_n)
  ccall((:nmod_poly_factor_squarefree, libflint), UInt,
          (Ref{nmod_poly_factor}, Ref{zzModPolyRingElem}), fac, x)
  res = Dict{zzModPolyRingElem,Int}()
  for i in 1:fac.num
    f = parent(x)()
    ccall((:nmod_poly_factor_get_poly, libflint), Nothing,
            (Ref{zzModPolyRingElem}, Ref{nmod_poly_factor}, Int), f, fac, i-1)
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
  ccall((:nmod_poly_factor_distinct_deg, libflint), UInt,
          (Ref{nmod_poly_factor}, Ref{zzModPolyRingElem}, Ptr{Ptr{Int}}),
          fac, x, degss)
  res = Dict{Int,zzModPolyRingElem}()
  for i in 1:fac.num
    f = parent(x)()
    ccall((:nmod_poly_factor_get_poly, libflint), Nothing,
            (Ref{zzModPolyRingElem}, Ref{nmod_poly_factor}, Int), f, fac, i-1)
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
  n = R.n
  fac = nmod_poly_factor(n)
  if is_prime(n)
    ccall((:nmod_poly_roots, libflint), UInt,
            (Ref{nmod_poly_factor}, Ref{zzModPolyRingElem}, Cint),
            fac, a, 0)
  else
    nfac = n_factor()
    ccall((:n_factor, libflint), Nothing,
          (Ref{n_factor}, UInt),
          nfac, n)
    ccall((:nmod_poly_roots_factored, libflint), UInt,
            (Ref{nmod_poly_factor}, Ref{zzModPolyRingElem}, Cint, Ref{n_factor}),
            fac, a, 0, nfac)
  end
  f = R()
  res = zzModRingElem[]
  for i in 1:fac.num
    ccall((:nmod_poly_factor_get_poly, libflint), Nothing,
          (Ref{zzModPolyRingElem}, Ref{nmod_poly_factor}, Int),
          f, fac, i - 1)
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
   v = ccall((:nmod_poly_remove, libflint), Int,
               (Ref{zzModPolyRingElem}, Ref{zzModPolyRingElem}), z,  p)
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
      return det_fflu(M)
   catch
      return det_df(M)
   end
end

################################################################################
#
#  Unsafe functions
#
################################################################################

function zero!(x::T) where T <: Zmodn_poly
  ccall((:nmod_poly_zero, libflint), Nothing,
                   (Ref{T},), x)
  return x
end

function one!(a::T) where T <: Zmodn_poly
  ccall((:nmod_poly_one, libflint), Nothing, (Ref{T}, ), a)
  return a
end

function fit!(x::T, n::Int) where T <: Zmodn_poly
  ccall((:nmod_poly_fit_length, libflint), Nothing,
                   (Ref{T}, Int), x, n)
  return nothing
end

function setcoeff!(x::T, n::Int, y::UInt) where T <: Zmodn_poly
  ccall((:nmod_poly_set_coeff_ui, libflint), Nothing,
                   (Ref{T}, Int, UInt), x, n, y)
  return x
end

function setcoeff!(x::T, n::Int, y::Int) where T <: Zmodn_poly
  ccall((:nmod_poly_set_coeff_ui, libflint), Nothing,
                   (Ref{T}, Int, UInt), x, n, mod(y, x.mod_n))
  return x
end

function setcoeff!(x::T, n::Int, y::ZZRingElem) where T <: Zmodn_poly
  r = ccall((:fmpz_fdiv_ui, libflint), UInt, (Ref{ZZRingElem}, UInt), y, x.mod_n)
  ccall((:nmod_poly_set_coeff_ui, libflint), Nothing,
                   (Ref{T}, Int, UInt), x, n, r)
  return x
end

setcoeff!(x::T, n::Int, y::Integer) where T <: Zmodn_poly = setcoeff!(x, n, ZZRingElem(y))

setcoeff!(x::zzModPolyRingElem, n::Int, y::zzModRingElem) = setcoeff!(x, n, y.data)

function add!(z::T, x::T, y::T) where T <: Zmodn_poly
  ccall((:nmod_poly_add, libflint), Nothing,
          (Ref{T}, Ref{T},  Ref{T}), z, x, y)
  return z
end

function addeq!(z::T, y::T) where T <: Zmodn_poly
  ccall((:nmod_poly_add, libflint), Nothing,
          (Ref{T}, Ref{T},  Ref{T}), z, z, y)
  return z
end

function sub!(z::T, x::T, y::T) where T <: Zmodn_poly
  ccall((:nmod_poly_sub, libflint), Nothing,
          (Ref{T}, Ref{T},  Ref{T}), z, x, y)
  return z
end

function mul!(z::T, x::T, y::T) where T <: Zmodn_poly
  ccall((:nmod_poly_mul, libflint), Nothing,
          (Ref{T}, Ref{T},  Ref{T}), z, x, y)
  return z
end

function mul!(z::T, x::T, y::UInt) where T <: Zmodn_poly
  ccall((:nmod_poly_scalar_mul_nmod, libflint), Nothing,
            (Ref{T}, Ref{T}, UInt), z, x, y)
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

###############################################################################
#
#   Polynomial substitution
#
###############################################################################

function (f::zzModPolyRingElem)(a::zzModRingElem)
   if parent(a) != base_ring(f)
      return subst(f, a)
   end
   return evaluate(f, a)
end

################################################################################
#
#  Parent object call overloads
#
################################################################################

function (R::zzModPolyRing)()
  z = zzModPolyRingElem(R.n)
  z.parent = R
  return z
end

function (R::zzModPolyRing)(x::ZZRingElem)
  r = ccall((:fmpz_fdiv_ui, libflint), UInt, (Ref{ZZRingElem}, UInt), x, R.n)
  z = zzModPolyRingElem(R.n, r)
  z.parent = R
  return z
end

function (R::zzModPolyRing)(x::UInt)
  z = zzModPolyRingElem(R.n, x)
  z.parent = R
  return z
end

function (R::zzModPolyRing)(x::Integer)
  z = zzModPolyRingElem(R.n, x)
  z.parent = R
  return z
end

function (R::zzModPolyRing)(x::zzModPolyRingElem)
   R != parent(x) && error("Wrong parents")
   return x
end

function (R::zzModPolyRing)(x::zzModRingElem)
  base_ring(R) != parent(x) && error("Wrong parents")
  z = zzModPolyRingElem(R.n, x.data)
  z.parent = R
  return z
end

function (R::zzModPolyRing)(arr::Vector{ZZRingElem})
  z = zzModPolyRingElem(R.n, arr)
  z.parent = R
  return z
end

function (R::zzModPolyRing)(arr::Vector{UInt})
  z = zzModPolyRingElem(R.n, arr)
  z.parent = R
  return z
end

(R::zzModPolyRing)(arr::Vector{T}) where {T <: Integer} = R(map(base_ring(R), arr))

function (R::zzModPolyRing)(arr::Vector{zzModRingElem})
  if length(arr) > 0
     (base_ring(R) != parent(arr[1])) && error("Wrong parents")
  end
  z = zzModPolyRingElem(R.n, arr)
  z.parent = R
  return z
end

function (R::zzModPolyRing)(x::ZZPolyRingElem)
  z = zzModPolyRingElem(R.n, x)
  z.parent = R
  return z
end
