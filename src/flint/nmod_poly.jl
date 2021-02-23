################################################################################
#
#  nmod_poly.jl : Flint nmod_poly (polynomials over Z/nZ, small modulus)
#
################################################################################

export NmodPolyRing, nmod_poly, parent, base_ring, elem_type, length, zero,
       one, gen, isgen, iszero, var, deepcopy, show, truncate, mullow, reverse,
       shift_left, shift_right, divexact, rem, gcd, resultant,
       evaluate, derivative, compose, interpolate, inflate, deflate, lift,
       isirreducible, issquarefree, factor, factor_squarefree,
       factor_distinct_deg, factor_shape, setcoeff!, canonical_unit,
       add!, sub!, mul!, PolynomialRing, check_parent, gcdx, mod,
       invmod, gcdinv, mulmod, powermod, zero!, one!

################################################################################
#
#  Type and parent object methods
#
################################################################################

parent(a::nmod_poly) = a.parent

base_ring(R::NmodPolyRing) = R.base_ring

base_ring(a::nmod_poly) = base_ring(parent(a))

parent_type(::Type{nmod_poly}) = NmodPolyRing

elem_type(::Type{nmod_poly}) = nmod_poly

elem_type(::Type{NmodPolyRing}) = nmod_poly

function check_parent(x::T, y::T) where T <: Zmodn_poly
  parent(x) != parent(y) && error("Parents must coincide")
  nothing
end

################################################################################
#
#   Basic helper
#
################################################################################

function lead_isunit(a::nmod_poly)
  d = degree(a)
  u = ccall((:nmod_poly_get_coeff_ui, libflint), UInt, (Ref{nmod_poly}, Int), a, d)
  n = ccall((:n_gcd, libflint), UInt, (UInt, UInt), u, modulus(a))
  return n==1
end

function Base.hash(a::nmod_poly, h::UInt)
   b = 0x53dd43cd511044d1%UInt
   for i in 0:length(a) - 1
      u = ccall((:nmod_poly_get_coeff_ui, libflint), UInt, (Ref{nmod_poly}, Int), a, i)
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

zero(R::NmodPolyRing) = R(UInt(0))

one(R::NmodPolyRing) = R(UInt(1))

gen(R::NmodPolyRing) = R([zero(base_ring(R)), one(base_ring(R))])

isgen(a::T) where T <: Zmodn_poly = (degree(a) == 1 &&
                              iszero(coeff(a,0)) && isone(coeff(a,1)))

iszero(a::T) where T <: Zmodn_poly = Bool(ccall((:nmod_poly_is_zero, libflint), Int32,
                              (Ref{T}, ), a))

modulus(a::T) where T <: Zmodn_poly = a.parent.n

modulus(R::NmodPolyRing) = R.n

var(R::NmodPolyRing) = R.S

function deepcopy_internal(a::nmod_poly, dict::IdDict)
  z = nmod_poly(modulus(a), a)
  z.parent = a.parent
  return z
end

characteristic(R::NmodPolyRing) = modulus(R)

################################################################################
#
#  AbstractString I/O
#
################################################################################

function show(io::IO, R::NmodPolyRing)
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

canonical_unit(a::T) where T <: Zmodn_poly = canonical_unit(lead(a))

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

function *(x::T, y::fmpz) where T <: Zmodn_poly
  z = parent(x)()
  t = fmpz()
  tt = UInt(0)
  ccall((:fmpz_mod_ui, libflint), UInt,
                (Ref{fmpz}, Ref{fmpz}, UInt), t, y, parent(x).n)
  tt = ccall((:fmpz_get_ui, libflint), UInt, (Ref{fmpz}, ), t)
  return x*tt
end

*(x::fmpz, y::T) where T <: Zmodn_poly = y*x

*(x::T, y::Integer) where T <: Zmodn_poly = x*fmpz(y)

*(x::Integer, y::T) where T <: Zmodn_poly = y*x

function *(x::nmod_poly, y::nmod)
  (base_ring(x) != parent(y)) && error("Must have same parent")
  return x*y.data
end

*(x::nmod, y::nmod_poly) = y*x

function +(x::T, y::UInt) where T <: Zmodn_poly
  z = parent(x)()
  ccall((:nmod_poly_add_ui, libflint), Nothing,
    (Ref{T}, Ref{T}, UInt), z, x, y)
  return z
end

+(x::UInt, y::T) where T <: Zmodn_poly = y + x

function +(x::T, y::fmpz) where T <: Zmodn_poly
  z = parent(x)()
  t = fmpz()
  tt = UInt(0)
  ccall((:fmpz_mod_ui, libflint), UInt,
                (Ref{fmpz}, Ref{fmpz}, UInt), t, y, parent(x).n)
  tt = ccall((:fmpz_get_ui, libflint), UInt, (Ref{fmpz}, ), t)
  return +(x,tt)
end

+(x::fmpz, y::T) where T <: Zmodn_poly = y + x

+(x::T, y::Integer) where T <: Zmodn_poly = x + fmpz(y)

+(x::Integer, y::T) where T <: Zmodn_poly = y + x

function +(x::nmod_poly, y::nmod)
  (base_ring(x) != parent(y)) && error("Elements must have same parent")
  return +(x, y.data)
end

+(x::nmod, y::nmod_poly) = y + x

function -(x::T, y::UInt) where T <: Zmodn_poly
  z = parent(x)()
  ccall((:nmod_poly_sub_ui, libflint), Nothing,
    (Ref{T}, Ref{T}, UInt), z, x, y)
  return z
end

-(x::UInt, y::T) where T <: Zmodn_poly = -(y - x)

function -(x::T, y::fmpz) where T <: Zmodn_poly
  z = parent(x)()
  t = fmpz()
  tt = UInt(0)
  ccall((:fmpz_mod_ui, libflint), UInt,
                (Ref{fmpz}, Ref{fmpz}, UInt), t, y, parent(x).n)
  tt = ccall((:fmpz_get_ui, libflint), UInt, (Ref{fmpz}, ), t)
  return -(x,tt)
end

-(x::fmpz, y::T) where T <: Zmodn_poly = -(y - x)

-(x::T, y::Integer) where T <: Zmodn_poly = x - fmpz(y)

-(x::Integer, y::T) where T <: Zmodn_poly = -(y - x)

function -(x::nmod_poly, y::nmod)
  (base_ring(x) != parent(y)) && error("Elements must have same parent")
  return -(x,y.data)
end

-(x::nmod, y::nmod_poly) = -(y - x)

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

function ==(x::nmod_poly, y::nmod)
  base_ring(x) != parent(y) && error("Incompatible base rings in comparison")
  if length(x) > 1
    return false
  elseif length(x) == 1
    u = ccall((:nmod_poly_get_coeff_ui, libflint), UInt,
            (Ref{nmod_poly}, Int), x, 0)
    return u == y
  else
    return iszero(y)
  end
end

==(x::nmod, y::nmod_poly) = y == x

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

function divexact(x::nmod_poly, y::nmod_poly)
  check_parent(x, y)
  iszero(y) && throw(DivideError())
  !lead_isunit(y) && error("Impossible inverse in divexact")
  z = parent(x)()
  ccall((:nmod_poly_div, libflint), Nothing,
          (Ref{nmod_poly}, Ref{nmod_poly}, Ref{nmod_poly}), z, x, y)
  return z
end

################################################################################
#
#  Ad hoc exact division
#
################################################################################

function divexact(x::nmod_poly, y::nmod)
  base_ring(x) != parent(y) && error("Elements must have same parent")
  iszero(y) && throw(DivideError())
  return divexact(x, parent(x)(y))
end

function divexact(x::T, y::fmpz) where T <: Zmodn_poly
  iszero(y) && throw(DivideError())
  return divexact(x, parent(x)(y))
end

function divexact(x::T, y::Int) where T <: Zmodn_poly
  y == 0 && throw(DivideError())
  return divexact(x, parent(x)(y))
end

################################################################################
#
#  Division with remainder
#
################################################################################

function Base.divrem(x::nmod_poly, y::nmod_poly)
  check_parent(x,y)
  iszero(y) && throw(DivideError())
  !lead_isunit(y) && error("Impossible inverse in divrem")
  q = parent(x)()
  r = parent(x)()
  ccall((:nmod_poly_divrem, libflint), Nothing,
          (Ref{nmod_poly}, Ref{nmod_poly}, Ref{nmod_poly}, Ref{nmod_poly}),
          q, r, x, y)
  return q, r
end

function Base.div(x::nmod_poly, y::nmod_poly)
  check_parent(x,y)
  iszero(y) && throw(DivideError())
  !lead_isunit(y) && error("Impossible inverse in div")
  q = parent(x)()
  ccall((:nmod_poly_div, libflint), Nothing,
          (Ref{nmod_poly}, Ref{nmod_poly}, Ref{nmod_poly}),
          q, x, y)
  return q
end

################################################################################
#
#  Remainder
#
################################################################################

function rem(x::nmod_poly, y::nmod_poly)
  check_parent(x,y)
  iszero(y) && throw(DivideError())
  !lead_isunit(y) && error("Impossible inverse in rem")
  z = parent(x)()
  ccall((:nmod_poly_rem, libflint), Nothing,
          (Ref{nmod_poly}, Ref{nmod_poly}, Ref{nmod_poly}), z, x, y)
  return z
end

mod(x::T, y::T) where T <: Zmodn_poly = rem(x, y)

################################################################################
#
#  GCD
#
################################################################################

function gcd(x::nmod_poly, y::nmod_poly)
  check_parent(x,y)
  !isprime(modulus(x)) && error("Modulus not prime in gcd")
  z = parent(x)()
  ccall((:nmod_poly_gcd, libflint), Nothing,
          (Ref{nmod_poly}, Ref{nmod_poly}, Ref{nmod_poly}), z, x, y)
  return z
end

function gcdx(x::nmod_poly, y::nmod_poly)
  check_parent(x,y)
  !isprime(modulus(x)) && error("Modulus not prime in gcdx")
  g = parent(x)()
  s = parent(x)()
  t = parent(x)()
  ccall((:nmod_poly_xgcd, libflint), Nothing,
          (Ref{nmod_poly}, Ref{nmod_poly}, Ref{nmod_poly}, Ref{nmod_poly},
           Ref{nmod_poly}), g, s, t, x, y)
  return g,s,t
end

function gcdinv(x::nmod_poly, y::nmod_poly)
  check_parent(x,y)
  !isprime(modulus(x)) && error("Modulus not prime in gcdinv")
  length(y) <= 1 && error("Length of second argument must be >= 2")
  g = parent(x)()
  s = parent(x)()
  ccall((:nmod_poly_gcdinv, libflint), Nothing,
          (Ref{nmod_poly}, Ref{nmod_poly}, Ref{nmod_poly}, Ref{nmod_poly}),
          g, s, x, y)
  return g,s
end

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

function resultant(x::nmod_poly, y::nmod_poly,  check::Bool = true)
  if check
    check_parent(x,y)
    !isprime(modulus(x)) && error("Modulus not prime in resultant")
  end
  r = ccall((:nmod_poly_resultant, libflint), UInt,
          (Ref{nmod_poly}, Ref{nmod_poly}), x, y)
  return base_ring(x)(r)
end

################################################################################
#
#  Evaluation
#
################################################################################

function evaluate(x::nmod_poly, y::nmod)
  base_ring(x) != parent(y) && error("Elements must have same parent")
  z = ccall((:nmod_poly_evaluate_nmod, libflint), UInt,
              (Ref{nmod_poly}, UInt), x, y.data)
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

function interpolate(R::NmodPolyRing, x::Array{nmod, 1},
                                      y::Array{nmod, 1})
  z = R()

  ax = Vector{UInt}(undef, length(x))
  ay = Vector{UInt}(undef, length(y))

  for i in 1:length(x)
    ax[i] = x[i].data

    ay[i] = y[i].data
  end
  ccall((:nmod_poly_interpolate_nmod_vec, libflint), Nothing,
          (Ref{nmod_poly}, Ptr{UInt}, Ptr{UInt}, Int),
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

@doc Markdown.doc"""
    function lift(R::FmpzPolyRing, y::nmod_poly)

Lift from a polynomial over $\mathbb{Z}/n\mathbb{Z}$ to a polynomial over
$\mathbb{Z}$ with minimal reduced nonnegative coefficients. The ring `R`
specifies the ring to lift into.
"""
function lift(R::FmpzPolyRing, y::nmod_poly)
  z = fmpz_poly()
  ccall((:fmpz_poly_set_nmod_poly, libflint), Nothing,
          (Ref{fmpz_poly}, Ref{nmod_poly}), z, y)
  z.parent = R
  return z
end

################################################################################
#
#  Irreducibility
#
################################################################################

@doc Markdown.doc"""
    isirreducible(x::nmod_poly)

Return `true` if $x$ is irreducible, otherwise return `false`.
"""
function isirreducible(x::nmod_poly)
  !isprime(modulus(x)) && error("Modulus not prime in isirreducible")
  return Bool(ccall((:nmod_poly_is_irreducible, libflint), Int32,
          (Ref{nmod_poly}, ), x))
end

################################################################################
#
#  Squarefree testing
#
################################################################################

@doc Markdown.doc"""
    issquarefree(x::nmod_poly)

Return `true` if $x$ is squarefree, otherwise return `false`.
"""
function issquarefree(x::nmod_poly)
   !isprime(modulus(x)) && error("Modulus not prime in issquarefree")
   return Bool(ccall((:nmod_poly_is_squarefree, libflint), Int32,
       (Ref{nmod_poly}, ), x))
end

################################################################################
#
#  Factorization
#
################################################################################

@doc Markdown.doc"""
    factor(x::nmod_poly)

Return the factorisation of $x$.
"""
function factor(x::nmod_poly)
  fac, z = _factor(x)
  return Fac(parent(x)(z), fac)
end

function _factor(x::nmod_poly)
  !isprime(modulus(x)) && error("Modulus not prime in factor")
  fac = nmod_poly_factor(x.mod_n)
  z = ccall((:nmod_poly_factor, libflint), UInt,
          (Ref{nmod_poly_factor}, Ref{nmod_poly}), fac, x)
  res = Dict{nmod_poly,Int}()
  for i in 1:fac.num
    f = parent(x)()
    ccall((:nmod_poly_factor_get_nmod_poly, libflint), Nothing,
            (Ref{nmod_poly}, Ref{nmod_poly_factor}, Int), f, fac, i-1)
    e = unsafe_load(fac.exp,i)
    res[f] = e
  end
  return res, base_ring(x)(z)
end

@doc Markdown.doc"""
    factor_squarefree(x::nmod_poly)

Return the squarefree factorisation of $x$.
"""
function factor_squarefree(x::nmod_poly)
  !isprime(modulus(x)) && error("Modulus not prime in factor_squarefree")
  return Fac(parent(x)(lead(x)), _factor_squarefree(x))
end

function _factor_squarefree(x::nmod_poly)
  fac = nmod_poly_factor(x.mod_n)
  ccall((:nmod_poly_factor_squarefree, libflint), UInt,
          (Ref{nmod_poly_factor}, Ref{nmod_poly}), fac, x)
  res = Dict{nmod_poly,Int}()
  for i in 1:fac.num
    f = parent(x)()
    ccall((:nmod_poly_factor_get_nmod_poly, libflint), Nothing,
            (Ref{nmod_poly}, Ref{nmod_poly_factor}, Int), f, fac, i-1)
    e = unsafe_load(fac.exp,i)
    res[f] = e
  end
  return res
end

@doc Markdown.doc"""
    factor_distinct_deg(x::nmod_poly)

Return the distinct degree factorisation of a squarefree polynomial $x$.
"""
function factor_distinct_deg(x::nmod_poly)
  !issquarefree(x) && error("Polynomial must be squarefree")
  !isprime(modulus(x)) && error("Modulus not prime in factor_distinct_deg")
  degs = Vector{Int}(undef, degree(x))
  degss = [ pointer(degs) ]
  fac = nmod_poly_factor(x.mod_n)
  ccall((:nmod_poly_factor_distinct_deg, libflint), UInt,
          (Ref{nmod_poly_factor}, Ref{nmod_poly}, Ptr{Ptr{Int}}),
          fac, x, degss)
  res = Dict{Int,nmod_poly}()
  for i in 1:fac.num
    f = parent(x)()
    ccall((:nmod_poly_factor_get_nmod_poly, libflint), Nothing,
            (Ref{nmod_poly}, Ref{nmod_poly_factor}, Int), f, fac, i-1)
    res[degs[i]] = f
  end
  return res
end

function factor_shape(x::PolyElem{T}) where {T <: RingElem}
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

################################################################################
#
#   Remove and valuation
#
################################################################################

@doc Markdown.doc"""
    remove(z::nmod_poly, p::nmod_poly)

Computes the valuation of $z$ at $p$, that is, the largest $k$ such that
$p^k$ divides $z$. Additionally, $z/p^k$ is returned as well.

See also `valuation`, which only returns the valuation.
"""
function remove(z::nmod_poly, p::nmod_poly)
   check_parent(z,p)
   iszero(z) && error("Not yet implemented")
   z = deepcopy(z)
   v = ccall((:nmod_poly_remove, libflint), Int,
               (Ref{nmod_poly}, Ref{nmod_poly}), z,  p)
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
#  Speedups for rings over nmod_poly
#
################################################################################

function det(M::Generic.Mat{nmod_poly})
   nrows(M) != ncols(M) && error("Not a square matrix in det")

   if isprime(modulus(base_ring(M)))
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

function setcoeff!(x::T, n::Int, y::fmpz) where T <: Zmodn_poly
  r = ccall((:fmpz_fdiv_ui, libflint), UInt, (Ref{fmpz}, UInt), y, x.mod_n)
  ccall((:nmod_poly_set_coeff_ui, libflint), Nothing,
                   (Ref{T}, Int, UInt), x, n, r)
  return x
end

setcoeff!(x::T, n::Int, y::Integer) where T <: Zmodn_poly = setcoeff!(x, n, fmpz(y))

setcoeff!(x::nmod_poly, n::Int, y::nmod) = setcoeff!(x, n, y.data)

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

promote_rule(::Type{nmod_poly}, ::Type{V}) where {V <: Integer} = nmod_poly

promote_rule(::Type{nmod_poly}, ::Type{fmpz}) = nmod_poly

promote_rule(::Type{nmod_poly}, ::Type{nmod}) = nmod_poly

###############################################################################
#
#   Polynomial substitution
#
###############################################################################

function (f::nmod_poly)(a::nmod)
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

function (R::NmodPolyRing)()
  z = nmod_poly(R.n)
  z.parent = R
  return z
end

function (R::NmodPolyRing)(x::fmpz)
  r = ccall((:fmpz_fdiv_ui, libflint), UInt, (Ref{fmpz}, UInt), x, R.n)
  z = nmod_poly(R.n, r)
  z.parent = R
  return z
end

function (R::NmodPolyRing)(x::UInt)
  z = nmod_poly(R.n, x)
  z.parent = R
  return z
end

function (R::NmodPolyRing)(x::Integer)
  z = nmod_poly(R.n, x)
  z.parent = R
  return z
end

function (R::NmodPolyRing)(x::nmod_poly)
   R != parent(x) && error("Wrong parents")
   return x
end

function (R::NmodPolyRing)(x::nmod)
  base_ring(R) != parent(x) && error("Wrong parents")
  z = nmod_poly(R.n, x.data)
  z.parent = R
  return z
end

function (R::NmodPolyRing)(arr::Array{fmpz, 1})
  z = nmod_poly(R.n, arr)
  z.parent = R
  return z
end

function (R::NmodPolyRing)(arr::Array{UInt, 1})
  z = nmod_poly(R.n, arr)
  z.parent = R
  return z
end

(R::NmodPolyRing)(arr::Array{T, 1}) where {T <: Integer} = R(map(base_ring(R), arr))

function (R::NmodPolyRing)(arr::Array{nmod, 1})
  if length(arr) > 0
     (base_ring(R) != parent(arr[1])) && error("Wrong parents")
  end
  z = nmod_poly(R.n, arr)
  z.parent = R
  return z
end

function (R::NmodPolyRing)(x::fmpz_poly)
  z = nmod_poly(R.n, x)
  z.parent = R
  return z
end

################################################################################
#
#  Polynomial ring constructor
#
################################################################################

function PolynomialRing(R::NmodRing, s::AbstractString; cached=true)
   parent_obj = NmodPolyRing(R, Symbol(s), cached)

   return parent_obj, parent_obj([R(0), R(1)])
end
