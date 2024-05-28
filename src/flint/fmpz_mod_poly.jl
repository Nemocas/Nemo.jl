################################################################################
#
#  ZZModPolyRingElem.jl : Flint ZZModPolyRingElem (polynomials over Z/nZ, large modulus)
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

function check_parent(x::T, y::T) where {T <: Zmodn_fmpz_poly}
  parent(x) != parent(y) && error("Parents must coincide")
  nothing
end

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

function length(x::T) where {T <: Zmodn_fmpz_poly}
  return x.length
  #   return ccall((:fmpz_mod_poly_length, libflint), Int, (Ref{T}, Ref{fmpz_mod_ctx_struct}), x, x.parent.base_ring.ninv)
end

function degree(x::T) where {T <: Zmodn_fmpz_poly}
  return x.length - 1
  #   return ccall((:fmpz_mod_poly_degree, libflint), Int, (Ref{T}, Ref{fmpz_mod_ctx_struct}), x, x.parent.base_ring.ninv)
end

function coeff(x::T, n::Int) where {T <: Zmodn_fmpz_poly}
  n < 0 && throw(DomainError(n, "Index must be non-negative"))
  z = ZZRingElem()
  ccall((:fmpz_mod_poly_get_coeff_fmpz, libflint), Nothing,
        (Ref{ZZRingElem}, Ref{T}, Int, Ref{fmpz_mod_ctx_struct}),
        z, x, n, x.parent.base_ring.ninv)
  return base_ring(x)(z)
end

zero(R::ZmodNFmpzPolyRing) = R(0)

one(R::ZmodNFmpzPolyRing) = R(1)

gen(R::ZmodNFmpzPolyRing) = R([ZZRingElem(0), ZZRingElem(1)])

is_gen(a::Zmodn_fmpz_poly) = (degree(a) == 1 &&
                              iszero(coeff(a,0)) && isone(coeff(a,1)))

function iszero(a::T) where {T <: Zmodn_fmpz_poly}
  return a.length == 0
  #  return Bool(ccall((:fmpz_mod_poly_is_zero, libflint), Cint,
  #                    (Ref{T}, Ref{fmpz_mod_ctx_struct}),
  #                    a, a.parent.base_ring.ninv))
end

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

function -(x::T) where {T <: Zmodn_fmpz_poly}
  z = parent(x)()
  ccall((:fmpz_mod_poly_neg, libflint), Nothing,
        (Ref{T}, Ref{T}, Ref{fmpz_mod_ctx_struct}),
        z, x, x.parent.base_ring.ninv)
  return z
end

################################################################################
#
#   Binary operations
#
################################################################################

function +(x::T, y::T) where {T <: Zmodn_fmpz_poly}
  check_parent(x,y)
  z = parent(x)()
  ccall((:fmpz_mod_poly_add, libflint), Nothing,
        (Ref{T}, Ref{T}, Ref{T}, Ref{fmpz_mod_ctx_struct}),
        z, x, y, x.parent.base_ring.ninv)
  return z
end

function -(x::T, y::T) where {T <: Zmodn_fmpz_poly}
  check_parent(x,y)
  z = parent(x)()
  ccall((:fmpz_mod_poly_sub, libflint), Nothing,
        (Ref{T}, Ref{T}, Ref{T}, Ref{fmpz_mod_ctx_struct}),
        z, x, y, x.parent.base_ring.ninv)
  return z
end

function *(x::T, y::T) where {T <: Zmodn_fmpz_poly}
  check_parent(x,y)
  z = parent(x)()
  ccall((:fmpz_mod_poly_mul, libflint), Nothing,
        (Ref{T}, Ref{T}, Ref{T}, Ref{fmpz_mod_ctx_struct}),
        z, x, y, x.parent.base_ring.ninv)
  return z
end

###############################################################################
#
#  Ad hoc binary operations
#
###############################################################################

function *(x::ZZModPolyRingElem, y::ZZRingElem)
  z = parent(x)()
  ccall((:fmpz_mod_poly_scalar_mul_fmpz, libflint), Nothing,
        (Ref{ZZModPolyRingElem}, Ref{ZZModPolyRingElem}, Ref{ZZRingElem}, Ref{fmpz_mod_ctx_struct}),
        z, x, y, x.parent.base_ring.ninv)
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
  ccall((:fmpz_mod_poly_add_si, libflint), Nothing,
        (Ref{ZZModPolyRingElem}, Ref{ZZModPolyRingElem}, Int, Ref{fmpz_mod_ctx_struct}),
        z, x, y, x.parent.base_ring.ninv)
  return z
end

+(x::Int, y::ZZModPolyRingElem) = +(y, x)

function +(x::ZZModPolyRingElem, y::ZZRingElem)
  z = parent(x)()
  ccall((:fmpz_mod_poly_add_fmpz, libflint), Nothing,
        (Ref{ZZModPolyRingElem}, Ref{ZZModPolyRingElem}, Ref{ZZRingElem},
         Ref{fmpz_mod_ctx_struct}),
        z, x, y, x.parent.base_ring.ninv)
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
  ccall((:fmpz_mod_poly_sub_si, libflint), Nothing,
        (Ref{ZZModPolyRingElem}, Ref{ZZModPolyRingElem}, Int, Ref{fmpz_mod_ctx_struct}),
        z, x, y, x.parent.base_ring.ninv)
  return z
end

function -(x::Int, y::ZZModPolyRingElem)
  z = parent(y)()
  ccall((:fmpz_mod_poly_si_sub, libflint), Nothing,
        (Ref{ZZModPolyRingElem}, Int, Ref{ZZModPolyRingElem}, Ref{fmpz_mod_ctx_struct}),
        z, x, y, y.parent.base_ring.ninv)
  return z
end

function -(x::ZZModPolyRingElem, y::ZZRingElem)
  z = parent(x)()
  ccall((:fmpz_mod_poly_sub_fmpz, libflint), Nothing,
        (Ref{ZZModPolyRingElem}, Ref{ZZModPolyRingElem}, Ref{ZZRingElem},
         Ref{fmpz_mod_ctx_struct}),
        z, x, y, x.parent.base_ring.ninv)
  return z
end

function -(x::ZZRingElem, y::ZZModPolyRingElem)
  z = parent(y)()
  ccall((:fmpz_mod_poly_fmpz_sub, libflint), Nothing,
        (Ref{ZZModPolyRingElem}, Ref{ZZRingElem}, Ref{ZZModPolyRingElem},
         Ref{fmpz_mod_ctx_struct}),
        z, x, y, y.parent.base_ring.ninv)
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
  ccall((:fmpz_mod_poly_pow, libflint), Nothing,
        (Ref{T}, Ref{T}, UInt, Ref{fmpz_mod_ctx_struct}),
        z, x, y, x.parent.base_ring.ninv)
  return z
end

################################################################################
#
#  Comparison
#
################################################################################

function ==(x::T, y::T) where {T <: Zmodn_fmpz_poly}
  check_parent(x, y)
  return Bool(ccall((:fmpz_mod_poly_equal, libflint), Cint,
                    (Ref{T}, Ref{T}, Ref{fmpz_mod_ctx_struct}),
                    x, y, x.parent.base_ring.ninv))
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
    ccall((:fmpz_mod_poly_get_coeff_fmpz, libflint), Nothing,
          (Ref{ZZRingElem}, Ref{ZZModPolyRingElem}, Int, Ref{fmpz_mod_ctx_struct}),
          u, x, 0, x.parent.base_ring.ninv)
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

  ccall((:fmpz_mod_poly_truncate, libflint), Nothing,
        (Ref{T}, Int, Ref{fmpz_mod_ctx_struct}),
        z, n, z.parent.base_ring.ninv)
  return z
end

function mullow(x::T, y::T, n::Int) where {T <: Zmodn_fmpz_poly}
  check_parent(x, y)
  n < 0 && throw(DomainError(n, "Index must be non-negative"))

  z = parent(x)()
  ccall((:fmpz_mod_poly_mullow, libflint), Nothing,
        (Ref{T}, Ref{T}, Ref{T}, Int, Ref{fmpz_mod_ctx_struct}),
        z, x, y, n, x.parent.base_ring.ninv)
  return z
end

###############################################################################
#
#   Reversal
#
###############################################################################

function reverse(x::T, len::Int) where {T <: Zmodn_fmpz_poly}
  len < 0 && throw(DomainError(len, "Length must be non-negative"))
  z = parent(x)()
  ccall((:fmpz_mod_poly_reverse, libflint), Nothing,
        (Ref{T}, Ref{T}, Int, Ref{fmpz_mod_ctx_struct}),
        z, x, len, x.parent.base_ring.ninv)
  return z
end

###############################################################################
#
#   Shifting
#
###############################################################################

function shift_left(x::T, len::Int) where {T <: Zmodn_fmpz_poly}
  len < 0 && throw(DomainError(len, "Shift must be non-negative"))
  z = parent(x)()
  ccall((:fmpz_mod_poly_shift_left, libflint), Nothing,
        (Ref{T}, Ref{T}, Int, Ref{fmpz_mod_ctx_struct}),
        z, x, len, x.parent.base_ring.ninv)
  return z
end

function shift_right(x::T, len::Int) where {T <: Zmodn_fmpz_poly}
  len < 0 && throw(DomainError(len, "Shift must be non-negative"))
  z = parent(x)()
  ccall((:fmpz_mod_poly_shift_right, libflint), Nothing,
        (Ref{T}, Ref{T}, Int, Ref{fmpz_mod_ctx_struct}),
        z, x, len, x.parent.base_ring.ninv)
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
  ccall((:fmpz_mod_poly_divrem_f, libflint), Nothing,
        (Ref{ZZRingElem}, Ref{T}, Ref{T}, Ref{T}, Ref{T}, Ref{fmpz_mod_ctx_struct}),
        d, q, r, x, y, x.parent.base_ring.ninv)
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
  ccall((:fmpz_mod_poly_scalar_div_fmpz, libflint), Nothing,
        (Ref{ZZModPolyRingElem}, Ref{ZZModPolyRingElem}, Ref{ZZRingElem},
         Ref{fmpz_mod_ctx_struct}),
        q, x, y.data, x.parent.base_ring.ninv)
  return q
end

function divexact(x::T, y::ZZRingElem; check::Bool=true) where {T <: Zmodn_fmpz_poly}
  iszero(y) && throw(DivideError())
  q = parent(x)()
  ccall((:fmpz_mod_poly_scalar_div_fmpz, libflint), Nothing,
        (Ref{T}, Ref{T}, Ref{ZZRingElem}, Ref{fmpz_mod_ctx_struct}),
        q, x, y, x.parent.base_ring.ninv)
  return q
end

function divexact(x::T, y::Int; check::Bool=true) where {T <: Zmodn_fmpz_poly}
  y == 0 && throw(DivideError())
  q = parent(x)()
  ccall((:fmpz_mod_poly_scalar_div_fmpz, libflint), Nothing,
        (Ref{T}, Ref{T}, Ref{ZZRingElem}, Ref{fmpz_mod_ctx_struct}),
        q, x, ZZRingElem(y), x.parent.base_ring.ninv)
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
  ccall((:fmpz_mod_poly_divrem_f, libflint), Nothing,
        (Ref{ZZRingElem}, Ref{T}, Ref{T}, Ref{T}, Ref{T}, Ref{fmpz_mod_ctx_struct}),
        d, q, r, x, y, x.parent.base_ring.ninv)
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
  ccall((:fmpz_mod_poly_gcd_euclidean_f, libflint), Nothing,
        (Ref{ZZRingElem}, Ref{ZZModPolyRingElem},
         Ref{ZZModPolyRingElem}, Ref{ZZModPolyRingElem}, Ref{fmpz_mod_ctx_struct}),
        f, z, x, y, x.parent.base_ring.ninv)
  _is_one_or_throw(f, y)
  return z
end

function AbstractAlgebra.gcdx_basecase(x::ZZModPolyRingElem, y::ZZModPolyRingElem)
  check_parent(x, y)
  g = parent(x)()
  s = parent(x)()
  t = parent(x)()
  f = ZZRingElem()
  ccall((:fmpz_mod_poly_xgcd_euclidean_f, libflint), Nothing,
        (Ref{ZZRingElem}, Ref{ZZModPolyRingElem}, Ref{ZZModPolyRingElem}, Ref{ZZModPolyRingElem},
         Ref{ZZModPolyRingElem}, Ref{ZZModPolyRingElem}, Ref{fmpz_mod_ctx_struct}),
        f, g, s, t, x, y, x.parent.base_ring.ninv)
  _is_one_or_throw(f, y)
  return g, s, t
end

function AbstractAlgebra.gcdinv_basecase(x::ZZModPolyRingElem, y::ZZModPolyRingElem)
  check_parent(x, y)
  length(y) <= 1 && error("Length of second argument must be >= 2")
  g = parent(x)()
  s = parent(x)()
  f = ZZRingElem()
  ccall((:fmpz_mod_poly_gcdinv_euclidean_f, libflint), Nothing,
        (Ref{ZZRingElem}, Ref{ZZModPolyRingElem}, Ref{ZZModPolyRingElem},
         Ref{ZZModPolyRingElem}, Ref{ZZModPolyRingElem}, Ref{fmpz_mod_ctx_struct}),
        f, g, s, x, y, x.parent.base_ring.ninv)
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
  length(y) == 0 && error("Second argument must not be 0")
  check_parent(x, y)
  if length(y) == 1
    return parent(x)(inv(evaluate(x, coeff(y, 0))))
  end
  z = parent(x)()
  r = ccall((:fmpz_mod_poly_invmod, libflint), Cint,
            (Ref{T}, Ref{T}, Ref{T}, Ref{fmpz_mod_ctx_struct}),
            z, x, y, x.parent.base_ring.ninv)
  r == 0 ? error("Impossible inverse in invmod") : return z
end

function mulmod(x::T, y::T, z::T) where {T <: Zmodn_fmpz_poly}
  check_parent(x, y)
  check_parent(y, z)
  w = parent(x)()
  ccall((:fmpz_mod_poly_mulmod, libflint), Nothing,
        (Ref{T}, Ref{T}, Ref{T}, Ref{T}, Ref{fmpz_mod_ctx_struct}),
        w, x, y, z, x.parent.base_ring.ninv)
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

  ccall((:fmpz_mod_poly_powmod_ui_binexp, libflint), Nothing,
        (Ref{T}, Ref{T}, UInt, Ref{T}, Ref{fmpz_mod_ctx_struct}),
        z, x, e, y, x.parent.base_ring.ninv)

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

  ccall((:fmpz_mod_poly_powmod_fmpz_binexp, libflint), Nothing,
        (Ref{T}, Ref{T}, Ref{ZZRingElem}, Ref{T}, Ref{fmpz_mod_ctx_struct}),
        z, x, e, y, x.parent.base_ring.ninv)
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
  ccall((:fmpz_mod_poly_resultant, libflint), Nothing,
        (Ref{ZZRingElem}, Ref{T}, Ref{T}, Ref{fmpz_mod_ctx_struct}),
        r, x, y, x.parent.base_ring.ninv)
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
  ccall((:fmpz_mod_poly_evaluate_fmpz, libflint), Nothing,
        (Ref{ZZRingElem}, Ref{ZZModPolyRingElem}, Ref{ZZRingElem}, Ref{fmpz_mod_ctx_struct}),
        z, x, y.data, x.parent.base_ring.ninv)
  return parent(y)(z)
end

################################################################################
#
#  Derivative
#
################################################################################

function derivative(x::T) where {T <: Zmodn_fmpz_poly}
  z = parent(x)()
  ccall((:fmpz_mod_poly_derivative, libflint), Nothing,
        (Ref{T}, Ref{T}, Ref{fmpz_mod_ctx_struct}),
        z, x, x.parent.base_ring.ninv)
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
  ccall((:fmpz_mod_poly_compose, libflint), Nothing,
        (Ref{T}, Ref{T}, Ref{T}, Ref{fmpz_mod_ctx_struct}),
        z, x, y, x.parent.base_ring.ninv)
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
  ccall((:fmpz_mod_poly_get_fmpz_poly, libflint), Nothing,
        (Ref{ZZPolyRingElem}, Ref{ZZModPolyRingElem}, Ref{fmpz_mod_ctx_struct}),
        z, y, y.parent.base_ring.ninv)
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
  return Bool(ccall((:fmpz_mod_poly_is_irreducible, libflint), Cint,
                    (Ref{ZZModPolyRingElem}, Ref{fmpz_mod_ctx_struct}),
                    x, x.parent.base_ring.ninv))
end

################################################################################
#
#  Squarefree testing
#
################################################################################

function is_squarefree(x::ZZModPolyRingElem)
  !is_probable_prime(modulus(x)) && error("Modulus not prime in is_squarefree")
  return Bool(ccall((:fmpz_mod_poly_is_squarefree, libflint), Cint,
                    (Ref{ZZModPolyRingElem}, Ref{fmpz_mod_ctx_struct}),
                    x, x.parent.base_ring.ninv))
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
  ccall((:fmpz_mod_poly_factor, libflint), UInt,
        (Ref{fmpz_mod_poly_factor}, Ref{ZZModPolyRingElem}, Ref{fmpz_mod_ctx_struct}),
        fac, x, n)
  res = Dict{ZZModPolyRingElem, Int}()
  for i in 1:fac.num
    f = parent(x)()
    ccall((:fmpz_mod_poly_factor_get_fmpz_mod_poly, libflint), Nothing,
          (Ref{ZZModPolyRingElem}, Ref{fmpz_mod_poly_factor}, Int,
           Ref{fmpz_mod_ctx_struct}),
          f, fac, i - 1, n)
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
  ccall((:fmpz_mod_poly_factor_squarefree, libflint), UInt,
        (Ref{fmpz_mod_poly_factor}, Ref{ZZModPolyRingElem}, Ref{fmpz_mod_ctx_struct}),
        fac, x, n)
  res = Dict{ZZModPolyRingElem, Int}()
  for i in 1:fac.num
    f = parent(x)()
    ccall((:fmpz_mod_poly_factor_get_fmpz_mod_poly, libflint), Nothing,
          (Ref{ZZModPolyRingElem}, Ref{fmpz_mod_poly_factor}, Int,
           Ref{fmpz_mod_ctx_struct}),
          f, fac, i - 1, n)
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
  ccall((:fmpz_mod_poly_factor_distinct_deg, libflint), UInt,
        (Ref{fmpz_mod_poly_factor}, Ref{ZZModPolyRingElem}, Ptr{Ptr{Int}},
         Ref{fmpz_mod_ctx_struct}),
        fac, x, degss, n)
  res = Dict{Int, ZZModPolyRingElem}()
  for i in 1:fac.num
    f = parent(x)()
    ccall((:fmpz_mod_poly_factor_get_fmpz_mod_poly, libflint), Nothing,
          (Ref{ZZModPolyRingElem}, Ref{fmpz_mod_poly_factor}, Int,
           Ref{fmpz_mod_ctx_struct}),
          f, fac, i - 1, n)
    res[degs[i]] = f
  end
  return res
end

function roots(a::ZZModPolyRingElem)
  R = parent(a)
  n = R.base_ring.ninv
  fac = fmpz_mod_poly_factor(n)
  if is_probable_prime(n.n)
    ccall((:fmpz_mod_poly_roots, libflint), UInt,
          (Ref{fmpz_mod_poly_factor}, Ref{ZZModPolyRingElem}, Cint, Ref{fmpz_mod_ctx_struct}),
          fac, a, 0, n)
  else
    nfac = fmpz_factor()
    ccall((:fmpz_factor, libflint), Nothing,
          (Ref{fmpz_factor}, Ref{ZZRingElem}),
          nfac, R.base_ring.n)
    ccall((:fmpz_mod_poly_roots_factored, libflint), UInt,
          (Ref{fmpz_mod_poly_factor}, Ref{ZZModPolyRingElem}, Cint, Ref{fmpz_factor}, Ref{fmpz_mod_ctx_struct}),
          fac, a, 0, nfac, n)
  end
  f = R()
  res = ZZModRingElem[]
  for i in 1:fac.num
    ccall((:fmpz_mod_poly_factor_get_fmpz_mod_poly, libflint), Nothing,
          (Ref{ZZModPolyRingElem}, Ref{fmpz_mod_poly_factor}, Int, Ref{fmpz_mod_ctx_struct}),
          f, fac, i - 1, n)
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
  ccall((:fmpz_mod_poly_zero, libflint), Nothing,
        (Ref{T}, Ref{fmpz_mod_ctx_struct}),
        x, x.parent.base_ring.ninv)
  return x
end

function fit!(x::T, n::Int) where {T <: Zmodn_fmpz_poly}
  ccall((:fmpz_mod_poly_fit_length, libflint), Nothing,
        (Ref{T}, Int, Ref{fmpz_mod_ctx_struct}),
        x, n, x.parent.base_ring.ninv)
  return nothing
end

function setcoeff!(x::T, n::Int, y::UInt) where {T <: Zmodn_fmpz_poly}
  ccall((:fmpz_mod_poly_set_coeff_ui, libflint), Nothing,
        (Ref{T}, Int, UInt, Ref{fmpz_mod_ctx_struct}),
        x, n, y, x.parent.base_ring.ninv)
  return x
end

function setcoeff!(x::T, n::Int, y::Int) where {T <: Zmodn_fmpz_poly}
  ccall((:fmpz_mod_poly_set_coeff_si, libflint), Nothing,
        (Ref{T}, Int, UInt, Ref{fmpz_mod_ctx_struct}),
        x, n, y, x.parent.base_ring.ninv)
  return x
end

function setcoeff!(x::T, n::Int, y::ZZRingElem) where {T <: Zmodn_fmpz_poly}
  ccall((:fmpz_mod_poly_set_coeff_fmpz, libflint), Nothing,
        (Ref{T}, Int, Ref{ZZRingElem}, Ref{fmpz_mod_ctx_struct}),
        x, n, y, x.parent.base_ring.ninv)
  return x
end

setcoeff!(x::T, n::Int, y::Integer) where {T <: Zmodn_fmpz_poly} = setcoeff!(x, n, ZZRingElem(y))

setcoeff!(x::ZZModPolyRingElem, n::Int, y::ZZModRingElem) = setcoeff!(x, n, y.data)

function add!(z::T, x::T, y::T) where {T <: Zmodn_fmpz_poly}
  ccall((:fmpz_mod_poly_add, libflint), Nothing,
        (Ref{T}, Ref{T}, Ref{T}, Ref{fmpz_mod_ctx_struct}),
        z, x, y, x.parent.base_ring.ninv)
  return z
end

function addeq!(z::T, y::T) where {T <: Zmodn_fmpz_poly}
  ccall((:fmpz_mod_poly_add, libflint), Nothing,
        (Ref{T}, Ref{T}, Ref{T}, Ref{fmpz_mod_ctx_struct}),
        z, z, y, z.parent.base_ring.ninv)
  return z
end

function sub!(z::T, x::T, y::T) where {T <: Zmodn_fmpz_poly}
  ccall((:fmpz_mod_poly_sub, libflint), Nothing,
        (Ref{T}, Ref{T}, Ref{T}, Ref{fmpz_mod_ctx_struct}),
        z, x, y, x.parent.base_ring.ninv)
  return z
end

function mul!(z::T, x::T, y::T) where {T <: Zmodn_fmpz_poly}
  ccall((:fmpz_mod_poly_mul, libflint), Nothing,
        (Ref{T}, Ref{T}, Ref{T}, Ref{fmpz_mod_ctx_struct}),
        z, x, y, x.parent.base_ring.ninv)
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

###############################################################################
#
#   Polynomial substitution
#
###############################################################################

function (f::ZZModPolyRingElem)(a::ZZModRingElem)
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
