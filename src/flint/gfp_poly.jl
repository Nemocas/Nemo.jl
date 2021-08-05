################################################################################
#
#  gfp_poly.jl : Flint gfp_poly (polynomials over Z/pZ, small prime modulus)
#
################################################################################

export GFPPolyRing, gfp_poly

################################################################################
#
#  Type and parent object methods
#
################################################################################

parent(a::gfp_poly) = a.parent

base_ring(R::GFPPolyRing) = R.base_ring

base_ring(a::gfp_poly) = base_ring(parent(a))

parent_type(::Type{gfp_poly}) = GFPPolyRing

elem_type(::Type{gfp_poly}) = gfp_poly

elem_type(::Type{GFPPolyRing}) = gfp_poly

dense_poly_type(::Type{gfp_elem}) = gfp_poly

################################################################################
#
#   Basic helper
#
################################################################################

lead_isunit(a::gfp_poly) = !iszero(a)

function Base.hash(a::gfp_poly, h::UInt)
   b = 0x74cec61d2911ace3%UInt
   for i in 0:length(a) - 1
      u = ccall((:nmod_poly_get_coeff_ui, libflint), UInt, (Ref{gfp_poly}, Int), a, i)
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

zero(R::GFPPolyRing) = R(UInt(0))

one(R::GFPPolyRing) = R(UInt(1))

gen(R::GFPPolyRing) = R([zero(base_ring(R)), one(base_ring(R))])

modulus(R::GFPPolyRing) = R.n

var(R::GFPPolyRing) = R.S

function deepcopy_internal(a::gfp_poly, dict::IdDict)
  z = gfp_poly(modulus(a), a)
  z.parent = a.parent
  return z
end

characteristic(R::GFPPolyRing) = characteristic(base_ring(R))

###############################################################################
#
#   Similar
#
###############################################################################

function similar(f::PolyElem, R::GaloisField, s::Symbol=var(parent(f)); cached::Bool=true)
   z = gfp_poly(R.n)
   if base_ring(f) === R && s == var(parent(f)) && typeof(f) == gfp_poly
      # steal parent in case it is not cached
      z.parent = parent(f)
   else
      z.parent = GFPPolyRing(R, s, cached)
   end
   return z
end

###############################################################################
#
#   polynomial constructor
#
###############################################################################

function polynomial(R::GaloisField, arr::Vector{T}, var::String="x"; cached::Bool=true) where T
   coeffs = map(R, arr)
   coeffs = length(coeffs) == 0 ? gfp_elem[] : coeffs
   z = gfp_poly(R.n, coeffs)
   z.parent = GFPPolyRing(R, Symbol(var), cached)
   return z
end

################################################################################
#
#  AbstractString I/O
#
################################################################################

function show(io::IO, R::GFPPolyRing)
  print(io, "Univariate Polynomial Ring in ")
  print(io, string(var(R)))
  print(io, " over ")
  print(io, base_ring(R))
end

###############################################################################
#
#  Ad hoc binary operations
#
###############################################################################

function *(x::gfp_poly, y::gfp_elem)
  (base_ring(x) != parent(y)) && error("Must have same parent")
  return x*y.data
end

*(x::gfp_elem, y::gfp_poly) = y*x

function +(x::gfp_poly, y::gfp_elem)
  (base_ring(x) != parent(y)) && error("Elements must have same parent")
  return +(x, y.data)
end

+(x::gfp_elem, y::gfp_poly) = y + x

function -(x::gfp_poly, y::gfp_elem)
  (base_ring(x) != parent(y)) && error("Elements must have same parent")
  return -(x,y.data)
end

-(x::gfp_elem, y::gfp_poly) = -(y - x)

################################################################################
#
#  Ad hoc comparisons
#
################################################################################

function ==(x::gfp_poly, y::gfp_elem)
  base_ring(x) != parent(y) && error("Incompatible base rings in comparison")
  if length(x) > 1
    return false
  elseif length(x) == 1
    u = ccall((:nmod_poly_get_coeff_ui, libflint), UInt,
            (Ref{gfp_poly}, Int), x, 0)
    return u == y
  else
    return iszero(y)
  end
end

==(x::gfp_elem, y::gfp_poly) = y == x

###############################################################################
#
#   Exact division
#
###############################################################################

function divexact(x::gfp_poly, y::gfp_poly; check::Bool=true)
  check_parent(x, y)
  iszero(y) && throw(DivideError())
  z = parent(x)()
  ccall((:nmod_poly_div, libflint), Nothing,
          (Ref{gfp_poly}, Ref{gfp_poly}, Ref{gfp_poly}), z, x, y)
  return z
end

################################################################################
#
#  Ad hoc exact division
#
################################################################################

function divexact(x::gfp_poly, y::gfp_elem; check::Bool=true)
  base_ring(x) != parent(y) && error("Elements must have same parent")
  iszero(y) && throw(DivideError())
  return divexact(x, parent(x)(y))
end

################################################################################
#
#  Division with remainder
#
################################################################################

function Base.divrem(x::gfp_poly, y::gfp_poly)
  check_parent(x,y)
  iszero(y) && throw(DivideError())
  q = parent(x)()
  r = parent(x)()
  ccall((:nmod_poly_divrem, libflint), Nothing,
          (Ref{gfp_poly}, Ref{gfp_poly}, Ref{gfp_poly}, Ref{gfp_poly}),
          q, r, x, y)
  return q, r
end

function Base.div(x::gfp_poly, y::gfp_poly)
  check_parent(x,y)
  iszero(y) && throw(DivideError())
  q = parent(x)()
  ccall((:nmod_poly_div, libflint), Nothing,
          (Ref{gfp_poly}, Ref{gfp_poly}, Ref{gfp_poly}),
          q, x, y)
  return q
end

################################################################################
#
#  Remainder
#
################################################################################

function rem(x::gfp_poly, y::gfp_poly)
  check_parent(x,y)
  iszero(y) && throw(DivideError())
  z = parent(x)()
  ccall((:nmod_poly_rem, libflint), Nothing,
          (Ref{gfp_poly}, Ref{gfp_poly}, Ref{gfp_poly}), z, x, y)
  return z
end

################################################################################
#
#  GCD
#
################################################################################

function gcd(x::gfp_poly, y::gfp_poly)
  check_parent(x,y)
  z = parent(x)()
  ccall((:nmod_poly_gcd, libflint), Nothing,
          (Ref{gfp_poly}, Ref{gfp_poly}, Ref{gfp_poly}), z, x, y)
  return z
end

function gcdx(x::gfp_poly, y::gfp_poly)
  check_parent(x,y)
  g = parent(x)()
  s = parent(x)()
  t = parent(x)()
  ccall((:nmod_poly_xgcd, libflint), Nothing,
          (Ref{gfp_poly}, Ref{gfp_poly}, Ref{gfp_poly}, Ref{gfp_poly},
           Ref{gfp_poly}), g, s, t, x, y)
  return g,s,t
end

function gcdinv(x::gfp_poly, y::gfp_poly)
  check_parent(x,y)
  length(y) <= 1 && error("Length of second argument must be >= 2")
  g = parent(x)()
  s = parent(x)()
  ccall((:nmod_poly_gcdinv, libflint), Nothing,
          (Ref{gfp_poly}, Ref{gfp_poly}, Ref{gfp_poly}, Ref{gfp_poly}),
          g, s, x, y)
  return g,s
end

################################################################################
#
#  Resultant
#
################################################################################

function resultant(x::gfp_poly, y::gfp_poly,  check::Bool = true)
  if check
    check_parent(x,y)
  end
  r = ccall((:nmod_poly_resultant, libflint), UInt,
          (Ref{gfp_poly}, Ref{gfp_poly}), x, y)
  return base_ring(x)(r)
end

################################################################################
#
#  Evaluation
#
################################################################################

function evaluate(x::gfp_poly, y::gfp_elem)
  base_ring(x) != parent(y) && error("Elements must have same parent")
  z = ccall((:nmod_poly_evaluate_nmod, libflint), UInt,
              (Ref{gfp_poly}, UInt), x, y.data)
  return parent(y)(z)
end

################################################################################
#
#  Interpolation
#
################################################################################

function interpolate(R::GFPPolyRing, x::Vector{gfp_elem},
                                      y::Vector{gfp_elem})
  z = R()

  ax = Vector{UInt}(undef, length(x))
  ay = Vector{UInt}(undef, length(y))

  for i in 1:length(x)
    ax[i] = x[i].data

    ay[i] = y[i].data
  end
  ccall((:nmod_poly_interpolate_nmod_vec, libflint), Nothing,
          (Ref{gfp_poly}, Ptr{UInt}, Ptr{UInt}, Int),
          z, ax, ay, length(x))
  return z
end

################################################################################
#
#  Lifting
#
################################################################################

@doc Markdown.doc"""
    lift(R::FmpzPolyRing, y::gfp_poly)

Lift from a polynomial over $\mathbb{Z}/n\mathbb{Z}$ to a polynomial over
$\mathbb{Z}$ with minimal reduced nonnegative coefficients. The ring `R`
specifies the ring to lift into.
"""
function lift(R::FmpzPolyRing, y::gfp_poly)
  z = fmpz_poly()
  ccall((:fmpz_poly_set_nmod_poly, libflint), Nothing,
          (Ref{fmpz_poly}, Ref{gfp_poly}), z, y)
  z.parent = R
  return z
end

################################################################################
#
#  Irreducibility
#
################################################################################

@doc Markdown.doc"""
    isirreducible(x::gfp_poly)

Return `true` if $x$ is irreducible, otherwise return `false`.
"""
function isirreducible(x::gfp_poly)
  return Bool(ccall((:nmod_poly_is_irreducible, libflint), Int32,
          (Ref{gfp_poly}, ), x))
end

################################################################################
#
#  Squarefree testing
#
################################################################################

@doc Markdown.doc"""
    issquarefree(x::gfp_poly)

Return `true` if $x$ is squarefree, otherwise return `false`.
"""
function issquarefree(x::gfp_poly)
   return Bool(ccall((:nmod_poly_is_squarefree, libflint), Int32,
       (Ref{gfp_poly}, ), x))
end

################################################################################
#
#  Factorization
#
################################################################################

@doc Markdown.doc"""
    factor(x::gfp_poly)

Return the factorisation of $x$.
"""
function factor(x::gfp_poly)
  fac, z = _factor(x)
  return Fac(parent(x)(z), fac)
end

function _factor(x::gfp_poly)
  fac = gfp_poly_factor(x.mod_n)
  z = ccall((:nmod_poly_factor, libflint), UInt,
          (Ref{gfp_poly_factor}, Ref{gfp_poly}), fac, x)
  res = Dict{gfp_poly, Int}()
  for i in 1:fac.num
    f = parent(x)()
    ccall((:nmod_poly_factor_get_nmod_poly, libflint), Nothing,
            (Ref{gfp_poly}, Ref{gfp_poly_factor}, Int), f, fac, i-1)
    e = unsafe_load(fac.exp,i)
    res[f] = e
  end
  return res, base_ring(x)(z)
end

@doc Markdown.doc"""
    factor_squarefree(x::gfp_poly)

Return the squarefree factorisation of $x$.
"""
function factor_squarefree(x::gfp_poly)
  return Fac(parent(x)(leading_coefficient(x)), _factor_squarefree(x))
end

function _factor_squarefree(x::gfp_poly)
  fac = gfp_poly_factor(x.mod_n)
  ccall((:nmod_poly_factor_squarefree, libflint), UInt,
          (Ref{gfp_poly_factor}, Ref{gfp_poly}), fac, x)
  res = Dict{gfp_poly, Int}()
  for i in 1:fac.num
    f = parent(x)()
    ccall((:nmod_poly_factor_get_nmod_poly, libflint), Nothing,
            (Ref{gfp_poly}, Ref{gfp_poly_factor}, Int), f, fac, i-1)
    e = unsafe_load(fac.exp,i)
    res[f] = e
  end
  return res
end

@doc Markdown.doc"""
    factor_distinct_deg(x::gfp_poly)

Return the distinct degree factorisation of a squarefree polynomial $x$.
"""
function factor_distinct_deg(x::gfp_poly)
  !issquarefree(x) && error("Polynomial must be squarefree")
  degs = Vector{Int}(undef, degree(x))
  degss = [ pointer(degs) ]
  fac = gfp_poly_factor(x.mod_n)
  ccall((:nmod_poly_factor_distinct_deg, libflint), UInt,
          (Ref{gfp_poly_factor}, Ref{gfp_poly}, Ptr{Ptr{Int}}),
          fac, x, degss)
  res = Dict{Int, gfp_poly}()
  for i in 1:fac.num
    f = parent(x)()
    ccall((:nmod_poly_factor_get_nmod_poly, libflint), Nothing,
            (Ref{gfp_poly}, Ref{gfp_poly_factor}, Int), f, fac, i-1)
    res[degs[i]] = f
  end
  return res
end

################################################################################
#
#   Remove and valuation
#
################################################################################

function remove(z::gfp_poly, p::gfp_poly)
   check_parent(z,p)
   iszero(z) && error("Not yet implemented")
   z = deepcopy(z)
   v = ccall((:nmod_poly_remove, libflint), Int,
               (Ref{gfp_poly}, Ref{gfp_poly}), z,  p)
   return v, z
end

################################################################################
#
#  Unsafe functions
#
################################################################################

setcoeff!(x::gfp_poly, n::Int, y::gfp_elem) = setcoeff!(x, n, y.data)

################################################################################
#
#  Promotion rules
#
################################################################################

promote_rule(::Type{gfp_poly}, ::Type{V}) where {V <: Integer} = gfp_poly

promote_rule(::Type{gfp_poly}, ::Type{fmpz}) = gfp_poly

promote_rule(::Type{gfp_poly}, ::Type{gfp_elem}) = gfp_poly

###############################################################################
#
#   Polynomial substitution
#
###############################################################################

function (f::gfp_poly)(a::gfp_elem)
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

function (R::GFPPolyRing)()
  z = gfp_poly(R.n)
  z.parent = R
  return z
end

function (R::GFPPolyRing)(x::fmpz)
  r = ccall((:fmpz_fdiv_ui, libflint), UInt, (Ref{fmpz}, UInt), x, R.n)
  z = gfp_poly(R.n, r)
  z.parent = R
  return z
end

function (R::GFPPolyRing)(x::UInt)
  z = gfp_poly(R.n, x)
  z.parent = R
  return z
end

function (R::GFPPolyRing)(x::Integer)
  z = gfp_poly(R.n, x)
  z.parent = R
  return z
end

function (R::GFPPolyRing)(x::gfp_poly)
   R != parent(x) && error("Wrong parents")
   return x
end

function (R::GFPPolyRing)(x::gfp_elem)
  base_ring(R) != parent(x) && error("Wrong parents")
  z = gfp_poly(R.n, x.data)
  z.parent = R
  return z
end

function (R::GFPPolyRing)(arr::Vector{fmpz})
  z = gfp_poly(R.n, arr)
  z.parent = R
  return z
end

function (R::GFPPolyRing)(arr::Vector{UInt})
  z = gfp_poly(R.n, arr)
  z.parent = R
  return z
end

(R::GFPPolyRing)(arr::Vector{T}) where {T <: Integer} = R(map(base_ring(R), arr))

function (R::GFPPolyRing)(arr::Vector{gfp_elem})
  if length(arr) > 0
     (base_ring(R) != parent(arr[1])) && error("Wrong parents")
  end
  z = gfp_poly(R.n, arr)
  z.parent = R
  return z
end

function (R::GFPPolyRing)(x::fmpz_poly)
  z = gfp_poly(R.n, x)
  z.parent = R
  return z
end

################################################################################
#
#  Polynomial ring constructor
#
################################################################################

function PolynomialRing(R::GaloisField, s::Symbol; cached=true)
   parent_obj = GFPPolyRing(R, s, cached)

   return parent_obj, parent_obj([R(0), R(1)])
end

function PolynomialRing(R::GaloisField, s::AbstractString; cached = true)
   return PolynomialRing(R, Symbol(s); cached=cached)
end

function PolyRing(R::GaloisField)
   return GFPPolyRing(R, :x, false)
end
