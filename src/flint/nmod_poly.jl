################################################################################
#
#  nmod_poly.jl : Flint nmod_poly (polynomials over Z/nZ, small modulus)
#
################################################################################

export NmodPolyRing, nmod_poly, parent, base_ring, elem_type, length, zero, 
       one, gen, isgen, iszero, var, deepcopy, show, truncate, mullow, reverse,
       shift_left, shift_right, divexact, divrem, rem, gcd, xgcd, resultant,
       evaluate, derivative, compose, interpolate, inflate, deflate, lift,
       isirreducible, issquarefree, factor, factor_squarefree,
       factor_distinct_deg, factor_shape, setcoeff!, canonical_unit,
       add!, sub!, mul!, call, PolynomialRing, check_parent, gcdx, mod,
       invmod, gcdinv, mulmod, powmod, zero!, one!

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

function check_parent(x::nmod_poly, y::nmod_poly)
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
  u = ccall((:nmod_poly_get_coeff_ui, :libflint), UInt, (Ptr{nmod_poly}, Int), &a, d)
  n = ccall((:n_gcd, :libflint), UInt, (UInt, UInt), u, modulus(a))
  return n==1
end

function Base.hash(a::nmod_poly, h::UInt)
   b = 0x53dd43cd511044d1%UInt
   for i in 0:length(a) - 1
      u = ccall((:nmod_poly_get_coeff_ui, :libflint), UInt, (Ptr{nmod_poly}, Int), &a, i)
      b $= hash(u, h) $ h
      b = (b << 1) | (b >> (sizeof(Int)*8 - 1))
   end
   return b
end

################################################################################
#
#  Basic manipulation
#
################################################################################

length(x::nmod_poly) = ccall((:nmod_poly_length, :libflint), Int,
                               (Ptr{nmod_poly}, ), &x)

degree(x::nmod_poly) = ccall((:nmod_poly_degree, :libflint), Int,
                               (Ptr{nmod_poly}, ), &x)

function coeff(x::nmod_poly, n::Int)
  n < 0 && throw(DomainError())
  return base_ring(x)(ccall((:nmod_poly_get_coeff_ui, :libflint), UInt,
          (Ptr{nmod_poly}, Int), &x, n))
end

function coeff_raw(x::nmod_poly, n::Int)
  return ccall((:nmod_poly_get_coeff_ui, :libflint), UInt,
                (Ptr{nmod_poly}, Int), &x, n)
end

zero(R::NmodPolyRing) = R(UInt(0))

one(R::NmodPolyRing) = R(UInt(1))

gen(R::NmodPolyRing) = R([zero(base_ring(R)), one(base_ring(R))])

isgen(a::nmod_poly) = (degree(a) == 1 &&
                              iszero(coeff(a,0)) && isone(coeff(a,1)))

iszero(a::nmod_poly) = Bool(ccall((:nmod_poly_is_zero, :libflint), Int32,
                              (Ptr{nmod_poly}, ), &a))

modulus(a::nmod_poly) = a.parent.n

modulus(R::NmodPolyRing) = R.n

var(R::NmodPolyRing) = R.S

function deepcopy_internal(a::nmod_poly, dict::ObjectIdDict)
  z = nmod_poly(modulus(a), a)
  z.parent = a.parent
  return z
end

################################################################################
#
#  AbstractString I/O
#
################################################################################

function show(io::IO, x::nmod_poly)
  if length(x) == 0
    print(io, "0")
  else
    cstr = ccall((:nmod_poly_get_str_pretty, :libflint), Ptr{UInt8},
            (Ptr{nmod_poly}, Ptr{UInt8}), &x, string(var(parent(x))))
    print(io, unsafe_string(cstr))
    ccall((:flint_free, :libflint), Void, (Ptr{UInt8}, ), cstr)
  end
end

function show(io::IO, R::NmodPolyRing)
  print(io, "Univariate Polynomial Ring in ")
  print(io, string(var(R)))
  print(io, " over ")
  print(io, base_ring(R))
end

show_minus_one(::Type{nmod_poly}) = true

################################################################################
#
#  Canonicalization
#
################################################################################

canonical_unit(a::nmod_poly) = canonical_unit(lead(a))

################################################################################
#
#  Unary operations
#
################################################################################

function -(x::nmod_poly)
  z = parent(x)()
  ccall((:nmod_poly_neg, :libflint), Void,
          (Ptr{nmod_poly}, Ptr{nmod_poly}), &z, &x)
  return z
end

################################################################################
#
#   Binary operations
#
################################################################################

function +(x::nmod_poly, y::nmod_poly)
  check_parent(x,y)
  z = parent(x)()
  ccall((:nmod_poly_add, :libflint), Void, 
          (Ptr{nmod_poly}, Ptr{nmod_poly},  Ptr{nmod_poly}), &z, &x, &y)
  return z
end

function -(x::nmod_poly, y::nmod_poly)
  check_parent(x,y)
  z = parent(x)()
  ccall((:nmod_poly_sub, :libflint), Void, 
          (Ptr{nmod_poly}, Ptr{nmod_poly},  Ptr{nmod_poly}), &z, &x, &y)
  return z
end

function *(x::nmod_poly, y::nmod_poly)
  check_parent(x,y)
  z = parent(x)()
  ccall((:nmod_poly_mul, :libflint), Void, 
          (Ptr{nmod_poly}, Ptr{nmod_poly},  Ptr{nmod_poly}), &z, &x, &y)
  return z
end

###############################################################################
#
#  Ad hoc binary operations
#
###############################################################################

function *(x::nmod_poly, y::UInt)
  z = parent(x)()
  ccall((:nmod_poly_scalar_mul_nmod, :libflint), Void,
          (Ptr{nmod_poly}, Ptr{nmod_poly}, UInt), &z, &x, y)
  return z
end

*(x::UInt, y::nmod_poly) = y*x

function *(x::nmod_poly, y::fmpz)
  z = parent(x)()
  t = fmpz()
  tt = UInt(0)
  ccall((:fmpz_mod_ui, :libflint), UInt,
                (Ptr{fmpz}, Ptr{fmpz}, UInt), &t, &y, parent(x).n)
  tt = ccall((:fmpz_get_ui, :libflint), UInt, (Ptr{fmpz}, ), &t)
  return x*tt
end

*(x::fmpz, y::nmod_poly) = y*x

*(x::nmod_poly, y::Integer) = x*fmpz(y)

*(x::Integer, y::nmod_poly) = y*x

function *(x::nmod_poly, y::GenRes{fmpz})
  (base_ring(x) != parent(y)) && error("Must have same parent")
  return x*y.data
end

*(x::GenRes{fmpz}, y::nmod_poly) = y*x

function +(x::nmod_poly, y::UInt)
  z = parent(x)()
  ccall((:nmod_poly_add_ui, :libflint), Void,
    (Ptr{nmod_poly}, Ptr{nmod_poly}, UInt), &z, &x, y)
  return z
end

+(x::UInt, y::nmod_poly) = y + x

function +(x::nmod_poly, y::fmpz)
  z = parent(x)()
  t = fmpz()
  tt = UInt(0)
  ccall((:fmpz_mod_ui, :libflint), UInt,
                (Ptr{fmpz}, Ptr{fmpz}, UInt), &t, &y, parent(x).n)
  tt = ccall((:fmpz_get_ui, :libflint), UInt, (Ptr{fmpz}, ), &t)
  return +(x,tt)
end

+(x::fmpz, y::nmod_poly) = y + x

+(x::nmod_poly, y::Integer) = x + fmpz(y)

+(x::Integer, y::nmod_poly) = y + x 

function +(x::nmod_poly, y::GenRes{fmpz})
  (base_ring(x) != parent(y)) && error("Elements must have same parent")
  return +(x,y.data)
end

+(x::GenRes{fmpz}, y::nmod_poly) = y + x

function -(x::nmod_poly, y::UInt)
  z = parent(x)()
  ccall((:nmod_poly_sub_ui, :libflint), Void,
    (Ptr{nmod_poly}, Ptr{nmod_poly}, UInt), &z, &x, y)
  return z
end

-(x::UInt, y::nmod_poly) = -(y - x)

function -(x::nmod_poly, y::fmpz)
  z = parent(x)()
  t = fmpz()
  tt = UInt(0)
  ccall((:fmpz_mod_ui, :libflint), UInt,
                (Ptr{fmpz}, Ptr{fmpz}, UInt), &t, &y, parent(x).n)
  tt = ccall((:fmpz_get_ui, :libflint), UInt, (Ptr{fmpz}, ), &t)
  return -(x,tt)
end

-(x::fmpz, y::nmod_poly) = -(y - x)

-(x::nmod_poly, y::Integer) = x - fmpz(y)

-(x::Integer, y::nmod_poly) = -(y - x)

function -(x::nmod_poly, y::GenRes{fmpz})
  (base_ring(x) != parent(y)) && error("Elements must have same parent")
  return -(x,y.data)
end

-(x::GenRes{fmpz}, y::nmod_poly) = -(y - x)

################################################################################
#
#  Powering
#
################################################################################

function ^(x::nmod_poly, y::Int)
  y < 0 && throw(DomainError())
  z = parent(x)()
  ccall((:nmod_poly_pow, :libflint), Void,
          (Ptr{nmod_poly}, Ptr{nmod_poly}, Int), &z, &x, y)
  return z
end

################################################################################
#
#  Comparison
#
################################################################################

function ==(x::nmod_poly, y::nmod_poly)
  check_parent(x, y)
  return Bool(ccall((:nmod_poly_equal, :libflint), Int32,
          (Ptr{nmod_poly}, Ptr{nmod_poly}), &x, &y))
end

################################################################################
#
#  Ad hoc comparisons
#
################################################################################

function ==(x::nmod_poly, y::GenRes{fmpz})
  base_ring(x) != parent(y) && error("Incompatible base rings in comparison")
  if length(x) > 1
    return false
  elseif length(x) == 1 
    u = ccall((:nmod_poly_get_coeff_ui, :libflint), UInt, 
            (Ptr{nmod_poly}, Int), &x, 0)
    return u == y
  else
    return y == 0
  end 
end

isequal(x::nmod_poly, y::nmod_poly) = x == y

==(x::GenRes{fmpz}, y::nmod_poly) = y == x

################################################################################
#
#  Truncation
#
################################################################################

function truncate(a::nmod_poly, n::Int)
  n < 0 && throw(DomainError())
  z = deepcopy(a)
  if length(z) <= n
    return z
  end
  ccall((:nmod_poly_truncate, :libflint), Void,
          (Ptr{nmod_poly}, Int), &z, n)
  return z
end

function mullow(x::nmod_poly, y::nmod_poly, n::Int)
  check_parent(x, y)
  n < 0 && throw(DomainError())
  z = parent(x)()
  ccall((:nmod_poly_mullow, :libflint), Void,
          (Ptr{nmod_poly}, Ptr{nmod_poly}, Ptr{nmod_poly}, Int), &z, &x, &y, n)
  return z
end

###############################################################################
#
#   Reversal
#
###############################################################################

function reverse(x::nmod_poly, len::Int)
  len < 0 && throw(DomainError())
  z = parent(x)()
  ccall((:nmod_poly_reverse, :libflint), Void,
          (Ptr{nmod_poly}, Ptr{nmod_poly}, Int), &z, &x, len)
  return z
end

###############################################################################
#
#   Shifting
#
###############################################################################

function shift_left(x::nmod_poly, len::Int)
  len < 0 && throw(DomainError())
  z = parent(x)()
  ccall((:nmod_poly_shift_left, :libflint), Void,
          (Ptr{nmod_poly}, Ptr{nmod_poly}, Int), &z, &x, len)
  return z
end

function shift_right(x::nmod_poly, len::Int)
  len < 0 && throw(DomainError())
  z = parent(x)()
  ccall((:nmod_poly_shift_right, :libflint), Void,
            (Ptr{nmod_poly}, Ptr{nmod_poly}, Int), &z, &x, len)
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
  ccall((:nmod_poly_div, :libflint), Void, 
          (Ptr{nmod_poly}, Ptr{nmod_poly}, Ptr{nmod_poly}), &z, &x, &y)
  return z
end

################################################################################
#
#  Ad hoc exact division
#
################################################################################

function divexact(x::nmod_poly, y::GenRes{fmpz})
  base_ring(x) != parent(y) && error("Elements must have same parent")
  iszero(y) && throw(DivideError())
  return divexact(x, parent(x)(y))
end

div(x::nmod_poly, y::nmod_poly) = divexact(x,y)

function divexact(x::nmod_poly, y::Int)
  y == 0 && throw(DivideError())
  return divexact(x, parent(x)(y))
end


################################################################################
#
#  Division with remainder
#
################################################################################

function divrem(x::nmod_poly, y::nmod_poly)
  check_parent(x,y)
  iszero(y) && throw(DivideError())
  !lead_isunit(y) && error("Impossible inverse in divrem")
  q = parent(x)()
  r = parent(x)()
  ccall((:nmod_poly_divrem, :libflint), Void,
          (Ptr{nmod_poly}, Ptr{nmod_poly}, Ptr{nmod_poly}, Ptr{nmod_poly}),
          &q, &r, &x, &y)
  return q, r
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
  ccall((:nmod_poly_rem, :libflint), Void,
          (Ptr{nmod_poly}, Ptr{nmod_poly}, Ptr{nmod_poly}), &z, &x, &y)
  return z
end

mod(x::nmod_poly, y::nmod_poly) = rem(x,y)

################################################################################
#
#  GCD 
#
################################################################################

function gcd(x::nmod_poly, y::nmod_poly)
  check_parent(x,y)
  !is_prime(modulus(x)) && error("Modulus not prime in gcd")
  z = parent(x)()
  ccall((:nmod_poly_gcd, :libflint), Void,
          (Ptr{nmod_poly}, Ptr{nmod_poly}, Ptr{nmod_poly}), &z, &x, &y)
  return z
end 

function gcdx(x::nmod_poly, y::nmod_poly)
  check_parent(x,y)
  !is_prime(modulus(x)) && error("Modulus not prime in gcdx")
  g = parent(x)()
  s = parent(x)()
  t = parent(x)()
  ccall((:nmod_poly_xgcd, :libflint), Void,
          (Ptr{nmod_poly}, Ptr{nmod_poly}, Ptr{nmod_poly}, Ptr{nmod_poly},
           Ptr{nmod_poly}), &g, &s, &t, &x, &y)
  return g,s,t
end

function gcdinv(x::nmod_poly, y::nmod_poly)
  check_parent(x,y)
  !is_prime(modulus(x)) && error("Modulus not prime in gcdinv")
  length(y) <= 1 && error("Length of second argument must be >= 2")
  g = parent(x)()
  s = parent(x)()
  ccall((:nmod_poly_gcdinv, :libflint), Void,
          (Ptr{nmod_poly}, Ptr{nmod_poly}, Ptr{nmod_poly}, Ptr{nmod_poly}),
          &g, &s, &x, &y)
  return g,s
end 

################################################################################
#
#  Modular arithmetic
#
################################################################################

function invmod(x::nmod_poly, y::nmod_poly)
  length(y) == 0 && error("Second argument must not be 0")
  check_parent(x,y)
  if length(y) == 1 
    return parent(x)(inv(eval(x,coeff(y,0))))
  end
  z = parent(x)()
  r = ccall((:nmod_poly_invmod, :libflint), Int32,
          (Ptr{nmod_poly}, Ptr{nmod_poly}, Ptr{nmod_poly}), &z, &x, &y)
  r == 0 ? error("Impossible inverse in invmod") : return z
end

function mulmod(x::nmod_poly, y::nmod_poly, z::nmod_poly)
  check_parent(x,y)
  check_parent(y,z)
  w = parent(x)()
  ccall((:nmod_poly_mulmod, :libflint), Void,
        (Ptr{nmod_poly}, Ptr{nmod_poly}, Ptr{nmod_poly}, Ptr{nmod_poly}),
        &w, &x, &y, &z)
  return w
end

function powmod(x::nmod_poly, e::Int, y::nmod_poly)
  check_parent(x,y)
  z = parent(x)()

  if e < 0
    g, x = gcdinv(x, y)
    if g != 1
      error("Element not invertible")
    end
    e = -e
  end

  ccall((:nmod_poly_powmod_ui_binexp, :libflint), Void,
        (Ptr{nmod_poly}, Ptr{nmod_poly}, Int, Ptr{nmod_poly}), &z, &x, e, &y)

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
    !is_prime(modulus(x)) && error("Modulus not prime in resultant")
  end
  r = ccall((:nmod_poly_resultant, :libflint), UInt,
          (Ptr{nmod_poly}, Ptr{nmod_poly}), &x, &y)
  return base_ring(x)(r)
end

################################################################################
#
#  Evaluation
#
################################################################################

function evaluate(x::nmod_poly, y::GenRes{fmpz})
  base_ring(x) != parent(y) && error("Elements must have same parent")
  u = ccall((:fmpz_get_ui, :libflint), UInt, (Ptr{fmpz}, ), &y.data)
  z = ccall((:nmod_poly_evaluate_nmod, :libflint), UInt,
              (Ptr{nmod_poly}, UInt), &x, u)
  return parent(y)(z)
end

################################################################################
#
#  Derivative
#
################################################################################

function derivative(x::nmod_poly)
  z = parent(x)()
  ccall((:nmod_poly_derivative, :libflint), Void,
        (Ptr{nmod_poly}, Ptr{nmod_poly}), &z, &x)
  return z
end

################################################################################
#
#  Integral
#
################################################################################

function integral(x::nmod_poly)
  z = parent(x)()
  ccall((:nmod_poly_integral, :libflint), Void,
        (Ptr{nmod_poly}, Ptr{nmod_poly}), &z, &x)
  return z
end

################################################################################
#
#  Composition
#
################################################################################

function compose(x::nmod_poly, y::nmod_poly)
  check_parent(x,y)
  z = parent(x)()
  ccall((:nmod_poly_compose, :libflint), Void,
          (Ptr{nmod_poly}, Ptr{nmod_poly}, Ptr{nmod_poly}), &z, &x, &y)
  return z
end

################################################################################
#
#  Interpolation
#
################################################################################

function interpolate(R::NmodPolyRing, x::Array{GenRes{fmpz}, 1},
                                      y::Array{GenRes{fmpz}, 1})
  z = R()

  ax = Array{UInt}(length(x))
  ay = Array{UInt}(length(y))

  t = fmpz()

  for i in 1:length(x)
    ccall((:fmpz_mod_ui, :libflint), Void,
          (Ptr{fmpz}, Ptr{fmpz}, UInt), &t, &x[i].data, R.n)
    u = ccall((:fmpz_get_ui, :libflint), UInt, (Ptr{fmpz}, ), &t)
    ax[i] = u

    ccall((:fmpz_mod_ui, :libflint), Void,
          (Ptr{fmpz}, Ptr{fmpz}, UInt), &t, &y[i].data, R.n)
    u = ccall((:fmpz_get_ui, :libflint), UInt, (Ptr{fmpz}, ), &t)
    
    ay[i] = u
  end
  ccall((:nmod_poly_interpolate_nmod_vec, :libflint), Void,
          (Ptr{nmod_poly}, Ptr{UInt}, Ptr{UInt}, Int),
          &z, ax, ay, length(x))
  return z
end

################################################################################
#
#  Inflation and Deflation
#
################################################################################

function inflate(x::nmod_poly, n::Int)
  n < 0 && throw(DomainError())
  z = parent(x)()
  ccall((:nmod_poly_inflate, :libflint), Void,
          (Ptr{nmod_poly}, Ptr{nmod_poly}, UInt), &z, &x, UInt(n))
  return z
end

function deflate(x::nmod_poly, n::Int)
  n < 0 && throw(DomainError())
  z = parent(x)()
  ccall((:nmod_poly_deflate, :libflint), Void,
          (Ptr{nmod_poly}, Ptr{nmod_poly}, UInt), &z, &x, UInt(n))
  return z
end
 
################################################################################
#
#  Lifting
#
################################################################################

doc"""
    function lift(R::FmpzPolyRing, y::nmod_poly)
> Lift from a polynomial over $\mathbb{Z}/n\mathbb{Z}$ to a polynomial over
> $\mathbb{Z}$ with minimal reduced nonnegative coefficients. The ring `R`
> specifies the ring to lift into.
"""
function lift(R::FmpzPolyRing, y::nmod_poly)
  z = fmpz_poly()
  ccall((:fmpz_poly_set_nmod_poly, :libflint), Void,
          (Ptr{fmpz_poly}, Ptr{nmod_poly}), &z, &y)
  z.parent = R
  return z
end

################################################################################
#
#  Irreducibility
#
################################################################################

doc"""
    isirreducible(x::nmod_poly)
> Return `true` if $x$ is irreducible, otherwise return `false`.
"""
function isirreducible(x::nmod_poly)
  !is_prime(modulus(x)) && error("Modulus not prime in isirreducible")
  return Bool(ccall((:nmod_poly_is_irreducible, :libflint), Int32,
          (Ptr{nmod_poly}, ), &x))
end

################################################################################
#
#  Squarefree testing
#
################################################################################

doc"""
    issquarefree(x::nmod_poly)
> Return `true` if $x$ is squarefree, otherwise return `false`.
"""
function issquarefree(x::nmod_poly)
   !is_prime(modulus(x)) && error("Modulus not prime in issquarefree")
   return Bool(ccall((:nmod_poly_is_squarefree, :libflint), Int32,
       (Ptr{nmod_poly}, ), &x))
end

################################################################################
#
#  Factorization
#
################################################################################

doc"""
    factor(x::nmod_poly)
> Return the factorisation of $x$.
"""
function factor(x::nmod_poly)
  fac, z = _factor(x)
  return Fac(parent(x)(z), fac)
end

function _factor(x::nmod_poly)
  !is_prime(modulus(x)) && error("Modulus not prime in factor")
  fac = nmod_poly_factor(x.mod_n)
  z = ccall((:nmod_poly_factor, :libflint), UInt,
          (Ptr{nmod_poly_factor}, Ptr{nmod_poly}), &fac, &x)
  res = Dict{nmod_poly,Int}()
  for i in 1:fac.num
    f = parent(x)()
    ccall((:nmod_poly_factor_get_nmod_poly, :libflint), Void,
            (Ptr{nmod_poly}, Ptr{nmod_poly_factor}, Int), &f, &fac, i-1)
    e = unsafe_load(fac.exp,i)
    res[f] = e
  end
  return res, base_ring(x)(z)
end

doc"""
    factor_squarefree(x::nmod_poly)
> Return the squarefree factorisation of $x$.
"""
function factor_squarefree(x::nmod_poly)
  !is_prime(modulus(x)) && error("Modulus not prime in factor_squarefree")
  return Fac(parent(x)(lead(x)), _factor_squarefree(x))
end

function _factor_squarefree(x::nmod_poly)
  fac = nmod_poly_factor(x.mod_n)
  ccall((:nmod_poly_factor_squarefree, :libflint), UInt,
          (Ptr{nmod_poly_factor}, Ptr{nmod_poly}), &fac, &x)
  res = Dict{nmod_poly,Int}()
  for i in 1:fac.num
    f = parent(x)()
    ccall((:nmod_poly_factor_get_nmod_poly, :libflint), Void,
            (Ptr{nmod_poly}, Ptr{nmod_poly_factor}, Int), &f, &fac, i-1)
    e = unsafe_load(fac.exp,i)
    res[f] = e
  end
  return res
end

doc"""
    factor_distinct_deg(x::nmod_poly)
> Return the distinct degree factorisation of a squarefree polynomial $x$.
"""
function factor_distinct_deg(x::nmod_poly)
  !issquarefree(x) && error("Polynomial must be squarefree")
  !is_prime(modulus(x)) && error("Modulus not prime in factor_distinct_deg")
  degs = Array{Int}(degree(x))
  degss = [ pointer(degs) ]
  fac = nmod_poly_factor(x.mod_n)
  ccall((:nmod_poly_factor_distinct_deg, :libflint), UInt,
          (Ptr{nmod_poly_factor}, Ptr{nmod_poly}, Ptr{Ptr{Int}}),
          &fac, &x, degss)
  res = Dict{Int,nmod_poly}()
  for i in 1:fac.num
    f = parent(x)()
    ccall((:nmod_poly_factor_get_nmod_poly, :libflint), Void,
            (Ptr{nmod_poly}, Ptr{nmod_poly_factor}, Int), &f, &fac, i-1)
    res[degs[i]] = f
  end
  return res
end

function factor_shape{T <: RingElem}(x::PolyElem{T})
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

doc"""
    remove(z::nmod_poly, p::nmod_poly)
> Computes the valuation of $z$ at $p$, that is, the largest $k$ such that
> $p^k$ divides $z$. Additionally, $z/p^k$ is returned as well.
>
> See also `valuation`, which only returns the valuation.
"""
function remove(z::nmod_poly, p::nmod_poly)
   check_parent(z,p)
   z == 0 && error("Not yet implemented")
   z = deepcopy(z)
   v = ccall((:nmod_poly_remove, :libflint), Int,
               (Ptr{nmod_poly}, Ptr{nmod_poly}), &z,  &p)
   return v, z
end

function divides(z::nmod_poly, x::nmod_poly)
   q, r = divrem(z, x)
   return r == 0, q
end

################################################################################
#
#  Speedups for rings over nmod_poly
#
################################################################################

function det(M::GenMat{nmod_poly})
   rows(M) != cols(M) && error("Not a square matrix in det")

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

function zero!(x::nmod_poly)
  ccall((:nmod_poly_zero, :libflint), Void, 
                   (Ptr{nmod_poly},), &x)
  return x
end

function one!(a::nmod_poly)
  ccall((:nmod_poly_one, :libflint), Void, (Ptr{nmod_poly}, ), &a)
  return a
end

function fit!(x::nmod_poly, n::Int)
  ccall((:nmod_poly_fit_length, :libflint), Void, 
                   (Ptr{nmod_poly}, Int), &x, n)
  return nothing
end

function setcoeff!(x::nmod_poly, n::Int, y::UInt)
  ccall((:nmod_poly_set_coeff_ui, :libflint), Void, 
                   (Ptr{nmod_poly}, Int, UInt), &x, n, y)
  return x
end

function setcoeff!(x::nmod_poly, n::Int, y::Int)
  ccall((:nmod_poly_set_coeff_ui, :libflint), Void, 
                   (Ptr{nmod_poly}, Int, UInt), &x, n, mod(y, x.mod_n))
  return x
end
  
function setcoeff!(x::nmod_poly, n::Int, y::fmpz)
  r = ccall((:fmpz_fdiv_ui, :libflint), UInt, (Ptr{fmpz}, UInt), &y, x.mod_n)
  ccall((:nmod_poly_set_coeff_ui, :libflint), Void, 
                   (Ptr{nmod_poly}, Int, UInt), &x, n, r)
  return x
end
  
setcoeff!(x::nmod_poly, n::Int, y::Integer) = setcoeff!(x, n, fmpz(y))
  
setcoeff!(x::nmod_poly, n::Int, y::GenRes{fmpz}) = setcoeff!(x, n, y.data)

function add!(z::nmod_poly, x::nmod_poly, y::nmod_poly)
  ccall((:nmod_poly_add, :libflint), Void, 
          (Ptr{nmod_poly}, Ptr{nmod_poly},  Ptr{nmod_poly}), &z, &x, &y)
  return z        
end

function addeq!(z::nmod_poly, y::nmod_poly)
  ccall((:nmod_poly_add, :libflint), Void, 
          (Ptr{nmod_poly}, Ptr{nmod_poly},  Ptr{nmod_poly}), &z, &z, &y)
  return z        
end

function sub!(z::nmod_poly, x::nmod_poly, y::nmod_poly)
  ccall((:nmod_poly_sub, :libflint), Void, 
          (Ptr{nmod_poly}, Ptr{nmod_poly},  Ptr{nmod_poly}), &z, &x, &y)
  return z        
end

function mul!(z::nmod_poly, x::nmod_poly, y::nmod_poly)
  ccall((:nmod_poly_mul, :libflint), Void, 
          (Ptr{nmod_poly}, Ptr{nmod_poly},  Ptr{nmod_poly}), &z, &x, &y)
  return z        
end

function mul!(z::nmod_poly, x::nmod_poly, y::UInt)
  ccall((:nmod_poly_scalar_mul_nmod, :libflint), Void,
            (Ptr{nmod_poly}, Ptr{nmod_poly}, UInt), &z, &x, y)
  return z
end

################################################################################
#
#  Promotion rules
#
################################################################################

promote_rule{V <: Integer}(::Type{nmod_poly}, ::Type{V}) = nmod_poly

promote_rule(::Type{nmod_poly}, ::Type{fmpz}) = nmod_poly

promote_rule(::Type{nmod_poly}, ::Type{GenRes{fmpz}}) = nmod_poly

###############################################################################
#
#   Polynomial substitution
#
###############################################################################

function (f::nmod_poly)(a::GenRes{fmpz})
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
  r = ccall((:fmpz_fdiv_ui, :libflint), UInt, (Ptr{fmpz}, UInt), &x, R.n)
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

function (R::NmodPolyRing)(x::GenRes{fmpz})
  base_ring(R) != parent(x) && error("Wrong parents")
  z = nmod_poly(R.n, UInt(x.data))
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

(R::NmodPolyRing){T <: Integer}(arr::Array{T, 1}) = R(map(base_ring(R), arr))

function (R::NmodPolyRing)(arr::Array{GenRes{fmpz}, 1})
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

# see fmpz_mod_poly for constructor
