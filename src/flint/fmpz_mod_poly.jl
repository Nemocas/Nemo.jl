################################################################################
#
#  fmpz_mod_poly.jl : Flint fmpz_mod_poly (polynomials over Z/nZ, large modulus)
#
################################################################################

export FmpzModPolyRing, fmpz_mod_poly, factor

################################################################################
#
#  Type and parent object methods
#
################################################################################

parent(a::fmpz_mod_poly) = a.parent

base_ring(R::FmpzModPolyRing) = R.base_ring

base_ring(a::fmpz_mod_poly) = base_ring(parent(a))

elem_type(::Type{fmpz_mod_poly}) = fmpz_mod_poly

elem_type(::Type{FmpzModPolyRing}) = fmpz_mod_poly

parent_type(::Type{fmpz_mod_poly}) = FmpzModPolyRing

function check_parent(x::T, y::T) where {T <: Zmodn_fmpz_poly}
  parent(x) != parent(y) && error("Parents must coincide")
  nothing
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
  z = fmpz()
  ccall((:fmpz_mod_poly_get_coeff_fmpz, libflint), Nothing,
        (Ref{fmpz}, Ref{T}, Int, Ref{fmpz_mod_ctx_struct}),
        z, x, n, x.parent.base_ring.ninv)
  return base_ring(x)(z)
end

zero(R::ZmodNFmpzPolyRing) = R(0)

one(R::ZmodNFmpzPolyRing) = R(1)

gen(R::ZmodNFmpzPolyRing) = R([fmpz(0), fmpz(1)])

isgen(a::Zmodn_fmpz_poly) = (degree(a) == 1 &&
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

characteristic(R::ZmodNFmpzPolyRing) = modulus(R)

################################################################################
#
#  AbstractString I/O
#
################################################################################

function show(io::IO, R::ZmodNFmpzPolyRing)
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

canonical_unit(a::Zmodn_fmpz_poly) = canonical_unit(lead(a))

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

function *(x::fmpz_mod_poly, y::fmpz)
  z = parent(x)()
  ccall((:fmpz_mod_poly_scalar_mul_fmpz, libflint), Nothing,
        (Ref{fmpz_mod_poly}, Ref{fmpz_mod_poly}, Ref{fmpz}, Ref{fmpz_mod_ctx_struct}),
        z, x, y, x.parent.base_ring.ninv)
  return z
end

*(x::fmpz, y::fmpz_mod_poly) = y*x

*(x::fmpz_mod_poly, y::Integer) = x*fmpz(y)

*(x::Integer, y::fmpz_mod_poly) = y*x

function *(x::fmpz_mod_poly, y::fmpz_mod)
  (base_ring(x) != parent(y)) && error("Must have same parent")
  return x*y.data
end

*(x::fmpz_mod, y::fmpz_mod_poly) = y*x

function +(x::fmpz_mod_poly, y::Int)
  z = parent(x)()
  ccall((:fmpz_mod_poly_add_si, libflint), Nothing,
        (Ref{fmpz_mod_poly}, Ref{fmpz_mod_poly}, Int, Ref{fmpz_mod_ctx_struct}),
        z, x, y, x.parent.base_ring.ninv)
  return z
end

+(x::Int, y::fmpz_mod_poly) = +(y, x)

function +(x::fmpz_mod_poly, y::fmpz)
  z = parent(x)()
  ccall((:fmpz_mod_poly_add_fmpz, libflint), Nothing,
        (Ref{fmpz_mod_poly}, Ref{fmpz_mod_poly}, Ref{fmpz},
         Ref{fmpz_mod_ctx_struct}),
        z, x, y, x.parent.base_ring.ninv)
  return z
end

+(x::fmpz, y::fmpz_mod_poly) = y + x

+(x::fmpz_mod_poly, y::Integer) = x + fmpz(y)

+(x::Integer, y::fmpz_mod_poly) = fmpz(y) + x

function +(x::fmpz_mod_poly, y::fmpz_mod)
  (base_ring(x) != parent(y)) && error("Elements must have same parent")
  return x + y.data
end

+(x::fmpz_mod, y::fmpz_mod_poly) = y + x

function -(x::fmpz_mod_poly, y::Int)
  z = parent(x)()
  ccall((:fmpz_mod_poly_sub_si, libflint), Nothing,
        (Ref{fmpz_mod_poly}, Ref{fmpz_mod_poly}, Int, Ref{fmpz_mod_ctx_struct}),
        z, x, y, x.parent.base_ring.ninv)
  return z
end

function -(x::Int, y::fmpz_mod_poly)
  z = parent(y)()
  ccall((:fmpz_mod_poly_si_sub, libflint), Nothing,
        (Ref{fmpz_mod_poly}, Int, Ref{fmpz_mod_poly}, Ref{fmpz_mod_ctx_struct}),
        z, x, y, y.parent.base_ring.ninv)
  return z
end

function -(x::fmpz_mod_poly, y::fmpz)
  z = parent(x)()
  ccall((:fmpz_mod_poly_sub_fmpz, libflint), Nothing,
        (Ref{fmpz_mod_poly}, Ref{fmpz_mod_poly}, Ref{fmpz},
         Ref{fmpz_mod_ctx_struct}),
        z, x, y, x.parent.base_ring.ninv)
  return z
end

function -(x::fmpz, y::fmpz_mod_poly)
  z = parent(y)()
  ccall((:fmpz_mod_poly_fmpz_sub, libflint), Nothing,
        (Ref{fmpz_mod_poly}, Ref{fmpz}, Ref{fmpz_mod_poly},
         Ref{fmpz_mod_ctx_struct}),
        z, x, y, y.parent.base_ring.ninv)
  return z
end

-(x::fmpz_mod_poly, y::Integer) = x - fmpz(y)

-(x::Integer, y::fmpz_mod_poly) = fmpz(x) - y

function -(x::fmpz_mod_poly, y::fmpz_mod)
  (base_ring(x) != parent(y)) && error("Elements must have same parent")
  return x - y.data
end

function -(x::fmpz_mod, y::fmpz_mod_poly)
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

function ==(x::fmpz_mod_poly, y::fmpz_mod)
  base_ring(x) != parent(y) && error("Incompatible base rings in comparison")
  if length(x) > 1
     return false
  elseif length(x) == 1
     u = fmpz()
     ccall((:fmpz_mod_poly_get_coeff_fmpz, libflint), Nothing,
           (Ref{fmpz}, Ref{fmpz_mod_poly}, Int, Ref{fmpz_mod_ctx_struct}),
           u, x, 0, x.parent.base_ring.ninv)
     return u == y
  else
    return iszero(y)
  end
end

==(x::fmpz_mod, y::fmpz_mod_poly) = y == x

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

function divexact(x::T, y::T) where {T <: Zmodn_fmpz_poly}
  check_parent(x, y)
  iszero(y) && throw(DivideError())
  d = fmpz()
  q = parent(x)()
  r = parent(x)()
  ccall((:fmpz_mod_poly_divrem_f, libflint), Nothing,
        (Ref{fmpz}, Ref{T}, Ref{T}, Ref{T}, Ref{T}, Ref{fmpz_mod_ctx_struct}),
        d, q, r, x, y, x.parent.base_ring.ninv)
  !isone(d) && error("Impossible inverse in divexact")
  return q
end

Base.div(x::T, y::T) where {T <: Zmodn_fmpz_poly} = divexact(x,y)

################################################################################
#
#  Ad hoc exact division
#
################################################################################

function divexact(x::fmpz_mod_poly, y::fmpz_mod)
  base_ring(x) != parent(y) && error("Elements must have same parent")
  iszero(y) && throw(DivideError())
  q = parent(x)()
  ccall((:fmpz_mod_poly_scalar_div_fmpz, libflint), Nothing,
        (Ref{fmpz_mod_poly}, Ref{fmpz_mod_poly}, Ref{fmpz},
         Ref{fmpz_mod_ctx_struct}),
        q, x, y.data, x.parent.base_ring.ninv)
  return q
end

function divexact(x::T, y::fmpz) where {T <: Zmodn_fmpz_poly}
  iszero(y) && throw(DivideError())
  q = parent(x)()
  ccall((:fmpz_mod_poly_scalar_div_fmpz, libflint), Nothing,
        (Ref{T}, Ref{T}, Ref{fmpz}, Ref{fmpz_mod_ctx_struct}),
        q, x, y, x.parent.base_ring.ninv)
  return q
end

function divexact(x::T, y::Int) where {T <: Zmodn_fmpz_poly}
  y == 0 && throw(DivideError())
  q = parent(x)()
  ccall((:fmpz_mod_poly_scalar_div_fmpz, libflint), Nothing,
        (Ref{T}, Ref{T}, Ref{fmpz}, Ref{fmpz_mod_ctx_struct}),
        q, x, fmpz(y), x.parent.base_ring.ninv)
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
  d = fmpz()
  ccall((:fmpz_mod_poly_divrem_f, libflint), Nothing,
        (Ref{fmpz}, Ref{T}, Ref{T}, Ref{T}, Ref{T}, Ref{fmpz_mod_ctx_struct}),
        d, q, r, x, y, x.parent.base_ring.ninv)
  !isone(d) && error("Impossible inverse in divrem")
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

function gcd(x::T, y::T) where {T <: Zmodn_fmpz_poly}
  check_parent(x, y)
  z = parent(x)()
  f = fmpz()
  ccall((:fmpz_mod_poly_gcd_f, libflint), Nothing,
        (Ref{fmpz}, Ref{T}, Ref{T}, Ref{T}, Ref{fmpz_mod_ctx_struct}),
        f, z, x, y, x.parent.base_ring.ninv)
  f > 1 && error("Impossible inverse: $(f) divides modulus")
  return z
end

function gcdx(x::T, y::T) where {T <: Zmodn_fmpz_poly}
  check_parent(x, y)
  g = parent(x)()
  s = parent(x)()
  t = parent(x)()
  f = fmpz()
  ccall((:fmpz_mod_poly_xgcd_f, libflint), Nothing,
        (Ref{fmpz}, Ref{T}, Ref{T}, Ref{T},
         Ref{T}, Ref{T}, Ref{fmpz_mod_ctx_struct}),
        f, g, s, t, x, y, x.parent.base_ring.ninv)
  f > 1 && error("Impossible inverse: $(f) divides modulus")
  return g, s, t
end

function gcdinv(x::T, y::T) where {T <: Zmodn_fmpz_poly}
  check_parent(x,y)
  length(y) <= 1 && error("Length of second argument must be >= 2")
  g = parent(x)()
  s = parent(x)()
  f = fmpz()
  ccall((:fmpz_mod_poly_gcdinv_f, libflint), Nothing,
        (Ref{fmpz}, Ref{T}, Ref{T}, Ref{T}, Ref{T}, Ref{fmpz_mod_ctx_struct}),
        f, g, s, x, y, x.parent.base_ring.ninv)
  f > 1 && error("Impossible inverse: $(f) divides modulus")
  return g, s
end

################################################################################
#
#  Modular arithmetic
#
################################################################################

function invmod(x::T, y::T) where {T <: Zmodn_fmpz_poly}
  length(y) == 0 && error("Second argument must not be 0")
  check_parent(x, y)
  if length(y) == 1
    return parent(x)(inv(eval(x, coeff(y, 0))))
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

@doc Markdown.doc"""
    powermod(x::T, e::fmpz, y::T) where {T <: Zmodn_fmpz_poly}

Return $x^e \pmod{y}$.
"""
function powermod(x::T, e::fmpz, y::T) where {T <: Zmodn_fmpz_poly}
  z = parent(x)()

  if e < 0
    g, x = gcdinv(x, y)
    if g != 1
      error("Element not invertible")
    end
    e = -e
  end

  ccall((:fmpz_mod_poly_powmod_fmpz_binexp, libflint), Nothing,
        (Ref{T}, Ref{T}, Ref{fmpz}, Ref{T}, Ref{fmpz_mod_ctx_struct}),
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
  !isprobable_prime(modulus(x)) && error("Modulus not prime in resultant")
  r = fmpz()
  ccall((:fmpz_mod_poly_resultant, libflint), Nothing,
        (Ref{fmpz}, Ref{T}, Ref{T}, Ref{fmpz_mod_ctx_struct}),
        r, x, y, x.parent.base_ring.ninv)
  return base_ring(x)(r)
end

################################################################################
#
#  Evaluation
#
################################################################################

function evaluate(x::fmpz_mod_poly, y::fmpz_mod)
  base_ring(x) != parent(y) && error("Elements must have same parent")
  z = fmpz()
  ccall((:fmpz_mod_poly_evaluate_fmpz, libflint), Nothing,
        (Ref{fmpz}, Ref{fmpz_mod_poly}, Ref{fmpz}, Ref{fmpz_mod_ctx_struct}),
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

function integral(x::fmpz_mod_poly)
   len = length(x)
   v = Vector{fmpz_mod}(undef, len + 1)
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

function compose(x::T, y::T) where {T <: Zmodn_fmpz_poly}
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

@doc Markdown.doc"""
    function lift(R::FmpzPolyRing, y::fmpz_mod_poly)

Lift from a polynomial over $\mathbb{Z}/n\mathbb{Z}$ to a polynomial over
$\mathbb{Z}$ with minimal reduced nonnegative coefficients. The ring `R`
specifies the ring to lift into.
"""
function lift(R::FmpzPolyRing, y::fmpz_mod_poly)
   z = fmpz_poly()
   ccall((:fmpz_mod_poly_get_fmpz_poly, libflint), Nothing,
         (Ref{fmpz_poly}, Ref{fmpz_mod_poly}, Ref{fmpz_mod_ctx_struct}),
         z, y, y.parent.base_ring.ninv)
   z.parent = R
   return z
end

################################################################################
#
#  Irreducibility
#
################################################################################

@doc Markdown.doc"""
    isirreducible(x::fmpz_mod_poly)

Return `true` if $x$ is irreducible, otherwise return `false`.
"""
function isirreducible(x::fmpz_mod_poly)
  !isprobable_prime(modulus(x)) && error("Modulus not prime in isirreducible")
  return Bool(ccall((:fmpz_mod_poly_is_irreducible, libflint), Cint,
                    (Ref{fmpz_mod_poly}, Ref{fmpz_mod_ctx_struct}),
                    x, x.parent.base_ring.ninv))
end

################################################################################
#
#  Squarefree testing
#
################################################################################

@doc Markdown.doc"""
    issquarefree(x::fmpz_mod_poly)

Return `true` if $x$ is squarefree, otherwise return `false`.
"""
function issquarefree(x::fmpz_mod_poly)
   !isprobable_prime(modulus(x)) && error("Modulus not prime in issquarefree")
   return Bool(ccall((:fmpz_mod_poly_is_squarefree, libflint), Cint,
                     (Ref{fmpz_mod_poly}, Ref{fmpz_mod_ctx_struct}),
                     x, x.parent.base_ring.ninv))
end

################################################################################
#
#  Factorization
#
################################################################################

@doc Markdown.doc"""
    factor(x::fmpz_mod_poly)

Return the factorisation of $x$.
"""
function factor(x::fmpz_mod_poly)
  !isprobable_prime(modulus(x)) && error("Modulus not prime in factor")
  fac = _factor(x)
  return Fac(parent(x)(lead(x)), fac)
end

function _factor(x::fmpz_mod_poly)
  n = x.parent.base_ring.ninv
  fac = fmpz_mod_poly_factor(n)
  ccall((:fmpz_mod_poly_factor, libflint), UInt,
        (Ref{fmpz_mod_poly_factor}, Ref{fmpz_mod_poly}, Ref{fmpz_mod_ctx_struct}),
        fac, x, n)
  res = Dict{fmpz_mod_poly, Int}()
  for i in 1:fac.num
    f = parent(x)()
    ccall((:fmpz_mod_poly_factor_get_fmpz_mod_poly, libflint), Nothing,
          (Ref{fmpz_mod_poly}, Ref{fmpz_mod_poly_factor}, Int,
           Ref{fmpz_mod_ctx_struct}),
          f, fac, i - 1, n)
    e = unsafe_load(fac.exp, i)
    res[f] = e
  end
  return res
end

@doc Markdown.doc"""
    factor_squarefree(x::fmpz_mod_poly)

Return the squarefree factorisation of $x$.
"""
function factor_squarefree(x::fmpz_mod_poly)
  !isprobable_prime(modulus(x)) && error("Modulus not prime in factor_squarefree")
  fac = _factor_squarefree(x)
  return Fac(parent(x)(lead(x)), fac)
end

function _factor_squarefree(x::fmpz_mod_poly)
  n = x.parent.base_ring.ninv
  fac = fmpz_mod_poly_factor(n)
  ccall((:fmpz_mod_poly_factor_squarefree, libflint), UInt,
        (Ref{fmpz_mod_poly_factor}, Ref{fmpz_mod_poly}, Ref{fmpz_mod_ctx_struct}),
        fac, x, n)
  res = Dict{fmpz_mod_poly, Int}()
  for i in 1:fac.num
    f = parent(x)()
    ccall((:fmpz_mod_poly_factor_get_fmpz_mod_poly, libflint), Nothing,
          (Ref{fmpz_mod_poly}, Ref{fmpz_mod_poly_factor}, Int,
           Ref{fmpz_mod_ctx_struct}),
          f, fac, i - 1, n)
    e = unsafe_load(fac.exp, i)
    res[f] = e
  end
  return res
end

@doc Markdown.doc"""
    factor_distinct_deg(x::fmpz_mod_poly)

Return the distinct degree factorisation of a squarefree polynomial $x$.
"""
function factor_distinct_deg(x::fmpz_mod_poly)
  !issquarefree(x) && error("Polynomial must be squarefree")
  !isprobable_prime(modulus(x)) && error("Modulus not prime in factor_distinct_deg")
  degs = Vector{Int}(undef, degree(x))
  degss = [ pointer(degs) ]
  n = x.parent.base_ring.ninv
  fac = fmpz_mod_poly_factor(n)
  ccall((:fmpz_mod_poly_factor_distinct_deg, libflint), UInt,
        (Ref{fmpz_mod_poly_factor}, Ref{fmpz_mod_poly}, Ptr{Ptr{Int}},
         Ref{fmpz_mod_ctx_struct}),
        fac, x, degss, n)
  res = Dict{Int, fmpz_mod_poly}()
  for i in 1:fac.num
    f = parent(x)()
    ccall((:fmpz_mod_poly_factor_get_fmpz_mod_poly, libflint), Nothing,
          (Ref{fmpz_mod_poly}, Ref{fmpz_mod_poly_factor}, Int,
           Ref{fmpz_mod_ctx_struct}),
          f, fac, i - 1, n)
    res[degs[i]] = f
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

function setcoeff!(x::T, n::Int, y::fmpz) where {T <: Zmodn_fmpz_poly}
  ccall((:fmpz_mod_poly_set_coeff_fmpz, libflint), Nothing,
        (Ref{T}, Int, Ref{fmpz}, Ref{fmpz_mod_ctx_struct}),
        x, n, y, x.parent.base_ring.ninv)
  return x
end

setcoeff!(x::T, n::Int, y::Integer) where {T <: Zmodn_fmpz_poly} = setcoeff!(x, n, fmpz(y))

setcoeff!(x::fmpz_mod_poly, n::Int, y::fmpz_mod) = setcoeff!(x, n, y.data)

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

promote_rule(::Type{T}, ::Type{fmpz}) where {T <: Zmodn_fmpz_poly} = T

promote_rule(::Type{fmpz_mod_poly}, ::Type{fmpz_mod}) = fmpz_mod_poly

###############################################################################
#
#   Polynomial substitution
#
###############################################################################

function (f::fmpz_mod_poly)(a::fmpz_mod)
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

function (R::FmpzModPolyRing)()
  z = fmpz_mod_poly(base_ring(R))
  z.parent = R
  return z
end

function (R::FmpzModPolyRing)(x::fmpz)
  z = fmpz_mod_poly(base_ring(R), x)
  z.parent = R
  return z
end

function (R::FmpzModPolyRing)(x::Integer)
  z = fmpz_mod_poly(base_ring(R), fmpz(x))
  z.parent = R
  return z
end

function (R::FmpzModPolyRing)(x::fmpz_mod)
  base_ring(R) != parent(x) && error("Wrong parents")
  z = fmpz_mod_poly(base_ring(R), x.data)
  z.parent = R
  return z
end

function (R::FmpzModPolyRing)(arr::Array{fmpz, 1})
  z = fmpz_mod_poly(base_ring(R), arr)
  z.parent = R
  return z
end

function (R::FmpzModPolyRing)(arr::Array{fmpz_mod, 1})
  if length(arr) > 0
     (base_ring(R) != parent(arr[1])) && error("Wrong parents")
  end
  z = fmpz_mod_poly(base_ring(R), arr)
  z.parent = R
  return z
end

(R::FmpzModPolyRing)(arr::Array{T, 1}) where {T <: Integer} = R(map(base_ring(R), arr))

function (R::FmpzModPolyRing)(x::fmpz_poly)
  z = fmpz_mod_poly(base_ring(R), x)
  z.parent = R
  return z
end

function (R::FmpzModPolyRing)(f::fmpz_mod_poly)
   parent(f) != R && error("Unable to coerce polynomial")
   return f
end

################################################################################
#
#  Polynomial ring constructor
#
################################################################################

function PolynomialRing(R::FmpzModRing, s::AbstractString; cached=true)
   parent_obj = FmpzModPolyRing(R, Symbol(s), cached)

   return parent_obj, parent_obj([R(0), R(1)])
end
