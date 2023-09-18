################################################################################
#
#  gfp_poly.jl: Flint gfp_poly (polynomials over Z/pZ, small prime modulus)
#
################################################################################

export fpPolyRing, fpPolyRingElem

################################################################################
#
#  Type and parent object methods
#
################################################################################

parent(a::fpPolyRingElem) = a.parent

base_ring(R::fpPolyRing) = R.base_ring

base_ring(a::fpPolyRingElem) = base_ring(parent(a))

parent_type(::Type{fpPolyRingElem}) = fpPolyRing

elem_type(::Type{fpPolyRingElem}) = fpPolyRingElem

elem_type(::Type{fpPolyRing}) = fpPolyRingElem

dense_poly_type(::Type{fpFieldElem}) = fpPolyRingElem

################################################################################
#
#   Basic helper
#
################################################################################

lead_isunit(a::fpPolyRingElem) = !iszero(a)

function Base.hash(a::fpPolyRingElem, h::UInt)
   b = 0x74cec61d2911ace3%UInt
   for i in 0:length(a) - 1
      u = ccall((:nmod_poly_get_coeff_ui, libflint), UInt, (Ref{fpPolyRingElem}, Int), a, i)
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

zero(R::fpPolyRing) = R(UInt(0))

one(R::fpPolyRing) = R(UInt(1))

gen(R::fpPolyRing) = R([zero(base_ring(R)), one(base_ring(R))])

modulus(R::fpPolyRing) = R.n

var(R::fpPolyRing) = R.S

function deepcopy_internal(a::fpPolyRingElem, dict::IdDict)
  z = fpPolyRingElem(modulus(a), a)
  z.parent = a.parent
  return z
end

characteristic(R::fpPolyRing) = characteristic(base_ring(R))

###############################################################################
#
#   Similar
#
###############################################################################

function similar(f::PolyRingElem, R::fpField, s::Symbol=var(parent(f)); cached::Bool=true)
   z = fpPolyRingElem(R.n)
   if base_ring(f) === R && s == var(parent(f)) && f isa fpPolyRingElem
      # steal parent in case it is not cached
      z.parent = parent(f)
   else
      z.parent = fpPolyRing(R, s, cached)
   end
   return z
end

###############################################################################
#
#   polynomial constructor
#
###############################################################################

function polynomial(R::fpField, arr::Vector{T}, var::VarName=:x; cached::Bool=true) where T
   coeffs = map(R, arr)
   coeffs = length(coeffs) == 0 ? fpFieldElem[] : coeffs
   z = fpPolyRingElem(R.n, coeffs)
   z.parent = fpPolyRing(R, Symbol(var), cached)
   return z
end

###############################################################################
#
#  Ad hoc binary operations
#
###############################################################################

function *(x::fpPolyRingElem, y::fpFieldElem)
  (base_ring(x) != parent(y)) && error("Must have same parent")
  return x*y.data
end

*(x::fpFieldElem, y::fpPolyRingElem) = y*x

function +(x::fpPolyRingElem, y::fpFieldElem)
  (base_ring(x) != parent(y)) && error("Elements must have same parent")
  return +(x, y.data)
end

+(x::fpFieldElem, y::fpPolyRingElem) = y + x

function -(x::fpPolyRingElem, y::fpFieldElem)
  (base_ring(x) != parent(y)) && error("Elements must have same parent")
  return -(x,y.data)
end

-(x::fpFieldElem, y::fpPolyRingElem) = -(y - x)

################################################################################
#
#  Ad hoc comparisons
#
################################################################################

function ==(x::fpPolyRingElem, y::fpFieldElem)
  base_ring(x) != parent(y) && error("Incompatible base rings in comparison")
  if length(x) > 1
    return false
  elseif length(x) == 1
    u = ccall((:nmod_poly_get_coeff_ui, libflint), UInt,
            (Ref{fpPolyRingElem}, Int), x, 0)
    return u == y
  else
    return iszero(y)
  end
end

==(x::fpFieldElem, y::fpPolyRingElem) = y == x

###############################################################################
#
#   Exact division
#
###############################################################################

function divexact(x::fpPolyRingElem, y::fpPolyRingElem; check::Bool=true)
  check_parent(x, y)
  iszero(y) && throw(DivideError())
  z = parent(x)()
  ccall((:nmod_poly_div, libflint), Nothing,
          (Ref{fpPolyRingElem}, Ref{fpPolyRingElem}, Ref{fpPolyRingElem}), z, x, y)
  return z
end

################################################################################
#
#  Ad hoc exact division
#
################################################################################

function divexact(x::fpPolyRingElem, y::fpFieldElem; check::Bool=true)
  base_ring(x) != parent(y) && error("Elements must have same parent")
  iszero(y) && throw(DivideError())
  return divexact(x, parent(x)(y))
end

################################################################################
#
#  Division with remainder
#
################################################################################

function Base.divrem(x::fpPolyRingElem, y::fpPolyRingElem)
  check_parent(x,y)
  iszero(y) && throw(DivideError())
  q = parent(x)()
  r = parent(x)()
  ccall((:nmod_poly_divrem, libflint), Nothing,
          (Ref{fpPolyRingElem}, Ref{fpPolyRingElem}, Ref{fpPolyRingElem}, Ref{fpPolyRingElem}),
          q, r, x, y)
  return q, r
end

function Base.div(x::fpPolyRingElem, y::fpPolyRingElem)
  check_parent(x,y)
  iszero(y) && throw(DivideError())
  q = parent(x)()
  ccall((:nmod_poly_div, libflint), Nothing,
          (Ref{fpPolyRingElem}, Ref{fpPolyRingElem}, Ref{fpPolyRingElem}),
          q, x, y)
  return q
end

################################################################################
#
#  Remainder
#
################################################################################

function rem(x::fpPolyRingElem, y::fpPolyRingElem)
  check_parent(x,y)
  iszero(y) && throw(DivideError())
  z = parent(x)()
  ccall((:nmod_poly_rem, libflint), Nothing,
          (Ref{fpPolyRingElem}, Ref{fpPolyRingElem}, Ref{fpPolyRingElem}), z, x, y)
  return z
end

################################################################################
#
#  GCD
#
################################################################################

function gcd(x::fpPolyRingElem, y::fpPolyRingElem)
  check_parent(x,y)
  z = parent(x)()
  ccall((:nmod_poly_gcd, libflint), Nothing,
          (Ref{fpPolyRingElem}, Ref{fpPolyRingElem}, Ref{fpPolyRingElem}), z, x, y)
  return z
end

function gcdx(x::fpPolyRingElem, y::fpPolyRingElem)
  check_parent(x,y)
  g = parent(x)()
  s = parent(x)()
  t = parent(x)()
  ccall((:nmod_poly_xgcd, libflint), Nothing,
          (Ref{fpPolyRingElem}, Ref{fpPolyRingElem}, Ref{fpPolyRingElem}, Ref{fpPolyRingElem},
           Ref{fpPolyRingElem}), g, s, t, x, y)
  return g,s,t
end

function gcdinv(x::fpPolyRingElem, y::fpPolyRingElem)
  check_parent(x,y)
  length(y) <= 1 && error("Length of second argument must be >= 2")
  g = parent(x)()
  s = parent(x)()
  ccall((:nmod_poly_gcdinv, libflint), Nothing,
          (Ref{fpPolyRingElem}, Ref{fpPolyRingElem}, Ref{fpPolyRingElem}, Ref{fpPolyRingElem}),
          g, s, x, y)
  return g,s
end

################################################################################
#
#  Resultant
#
################################################################################

function resultant(x::fpPolyRingElem, y::fpPolyRingElem,  check::Bool = true)
  if check
    check_parent(x,y)
  end
  r = ccall((:nmod_poly_resultant, libflint), UInt,
          (Ref{fpPolyRingElem}, Ref{fpPolyRingElem}), x, y)
  return base_ring(x)(r)
end

################################################################################
#
#  Evaluation
#
################################################################################

function evaluate(x::fpPolyRingElem, y::fpFieldElem)
  base_ring(x) != parent(y) && error("Elements must have same parent")
  z = ccall((:nmod_poly_evaluate_nmod, libflint), UInt,
              (Ref{fpPolyRingElem}, UInt), x, y.data)
  return parent(y)(z)
end

################################################################################
#
#  Interpolation
#
################################################################################

function interpolate(R::fpPolyRing, x::Vector{fpFieldElem},
                                      y::Vector{fpFieldElem})
  z = R()

  ax = Vector{UInt}(undef, length(x))
  ay = Vector{UInt}(undef, length(y))

  for i in 1:length(x)
    ax[i] = x[i].data

    ay[i] = y[i].data
  end
  ccall((:nmod_poly_interpolate_nmod_vec, libflint), Nothing,
          (Ref{fpPolyRingElem}, Ptr{UInt}, Ptr{UInt}, Int),
          z, ax, ay, length(x))
  return z
end

################################################################################
#
#  Lifting
#
################################################################################

@doc raw"""
    lift(R::ZZPolyRing, y::fpPolyRingElem)

Lift from a polynomial over $\mathbb{Z}/n\mathbb{Z}$ to a polynomial over
$\mathbb{Z}$ with minimal reduced non-negative coefficients. The ring `R`
specifies the ring to lift into.
"""
function lift(R::ZZPolyRing, y::fpPolyRingElem)
  z = ZZPolyRingElem()
  ccall((:fmpz_poly_set_nmod_poly_unsigned, libflint), Nothing,
          (Ref{ZZPolyRingElem}, Ref{fpPolyRingElem}), z, y)
  z.parent = R
  return z
end

################################################################################
#
#  Irreducibility
#
################################################################################

function is_irreducible(x::fpPolyRingElem)
  return Bool(ccall((:nmod_poly_is_irreducible, libflint), Int32,
          (Ref{fpPolyRingElem}, ), x))
end

################################################################################
#
#  Squarefree testing
#
################################################################################

function is_squarefree(x::fpPolyRingElem)
   return Bool(ccall((:nmod_poly_is_squarefree, libflint), Int32,
       (Ref{fpPolyRingElem}, ), x))
end

################################################################################
#
#  Square root
#
################################################################################

function sqrt(x::fpPolyRingElem; check::Bool=true)
   R = parent(x)
   s = R()
   flag = Bool(ccall((:nmod_poly_sqrt, libflint), Cint,
                     (Ref{fpPolyRingElem}, Ref{fpPolyRingElem}), s, x))
   check && !flag && error("Not a square in sqrt")
   return s
end

function is_square(x::fpPolyRingElem)
   if iszero(x)
      return true
   end
   if !iseven(degree(x))
      return false
   end
   R = parent(x)
   s = R()
   flag = Bool(ccall((:nmod_poly_sqrt, libflint), Cint,
                     (Ref{fpPolyRingElem}, Ref{fpPolyRingElem}), s, x))
   return flag
end

function is_square_with_sqrt(x::fpPolyRingElem)
   R = parent(x)
   if iszero(x)
      return true, zero(R)
   end
   if !iseven(degree(x))
      return false, zero(R)
   end
   s = R()
   flag = Bool(ccall((:nmod_poly_sqrt, libflint), Cint,
                     (Ref{fpPolyRingElem}, Ref{fpPolyRingElem}), s, x))
   return flag, s
end

################################################################################
#
#  Factorization
#
################################################################################

function factor(x::fpPolyRingElem)
  fac, z = _factor(x)
  return Fac(parent(x)(z), fac)
end

function _factor(x::fpPolyRingElem)
  fac = gfp_poly_factor(x.mod_n)
  z = ccall((:nmod_poly_factor, libflint), UInt,
          (Ref{gfp_poly_factor}, Ref{fpPolyRingElem}), fac, x)
  res = Dict{fpPolyRingElem, Int}()
  for i in 1:fac.num
    f = parent(x)()
    ccall((:nmod_poly_factor_get_poly, libflint), Nothing,
            (Ref{fpPolyRingElem}, Ref{gfp_poly_factor}, Int), f, fac, i-1)
    e = unsafe_load(fac.exp,i)
    res[f] = e
  end
  return res, base_ring(x)(z)
end

function factor_squarefree(x::fpPolyRingElem)
  return Fac(parent(x)(leading_coefficient(x)), _factor_squarefree(x))
end

function _factor_squarefree(x::fpPolyRingElem)
  fac = gfp_poly_factor(x.mod_n)
  ccall((:nmod_poly_factor_squarefree, libflint), UInt,
          (Ref{gfp_poly_factor}, Ref{fpPolyRingElem}), fac, x)
  res = Dict{fpPolyRingElem, Int}()
  for i in 1:fac.num
    f = parent(x)()
    ccall((:nmod_poly_factor_get_poly, libflint), Nothing,
            (Ref{fpPolyRingElem}, Ref{gfp_poly_factor}, Int), f, fac, i-1)
    e = unsafe_load(fac.exp,i)
    res[f] = e
  end
  return res
end

@doc raw"""
    factor_distinct_deg(x::fpPolyRingElem)

Return the distinct degree factorisation of a squarefree polynomial $x$.
"""
function factor_distinct_deg(x::fpPolyRingElem)
  !is_squarefree(x) && error("Polynomial must be squarefree")
  degs = Vector{Int}(undef, degree(x))
  degss = [ pointer(degs) ]
  fac = gfp_poly_factor(x.mod_n)
  ccall((:nmod_poly_factor_distinct_deg, libflint), UInt,
          (Ref{gfp_poly_factor}, Ref{fpPolyRingElem}, Ptr{Ptr{Int}}),
          fac, x, degss)
  res = Dict{Int, fpPolyRingElem}()
  for i in 1:fac.num
    f = parent(x)()
    ccall((:nmod_poly_factor_get_poly, libflint), Nothing,
            (Ref{fpPolyRingElem}, Ref{gfp_poly_factor}, Int), f, fac, i-1)
    res[degs[i]] = f
  end
  return res
end

function roots(a::fpPolyRingElem)
  R = parent(a)
  n = R.n
  fac = nmod_poly_factor(n)
  ccall((:nmod_poly_roots, libflint), UInt,
          (Ref{nmod_poly_factor}, Ref{fpPolyRingElem}, Cint),
          fac, a, 0)
  f = R()
  res = fpFieldElem[]
  for i in 1:fac.num
    ccall((:nmod_poly_factor_get_poly, libflint), Nothing,
          (Ref{fpPolyRingElem}, Ref{nmod_poly_factor}, Int),
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

function remove(z::fpPolyRingElem, p::fpPolyRingElem)
   ok, v = _remove_check_simple_cases(z, p)
   ok && return v, zero(parent(z))
   z = deepcopy(z)
   v = ccall((:nmod_poly_remove, libflint), Int,
               (Ref{fpPolyRingElem}, Ref{fpPolyRingElem}), z,  p)
   return v, z
end

################################################################################
#
#  Unsafe functions
#
################################################################################

setcoeff!(x::fpPolyRingElem, n::Int, y::fpFieldElem) = setcoeff!(x, n, y.data)

################################################################################
#
#  Promotion rules
#
################################################################################

promote_rule(::Type{fpPolyRingElem}, ::Type{V}) where {V <: Integer} = fpPolyRingElem

promote_rule(::Type{fpPolyRingElem}, ::Type{ZZRingElem}) = fpPolyRingElem

promote_rule(::Type{fpPolyRingElem}, ::Type{fpFieldElem}) = fpPolyRingElem

###############################################################################
#
#   Polynomial substitution
#
###############################################################################

function (f::fpPolyRingElem)(a::fpFieldElem)
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

function (R::fpPolyRing)()
  z = fpPolyRingElem(R.n)
  z.parent = R
  return z
end

function (R::fpPolyRing)(x::ZZRingElem)
  r = ccall((:fmpz_fdiv_ui, libflint), UInt, (Ref{ZZRingElem}, UInt), x, R.n)
  z = fpPolyRingElem(R.n, r)
  z.parent = R
  return z
end

function (R::fpPolyRing)(x::UInt)
  z = fpPolyRingElem(R.n, x)
  z.parent = R
  return z
end

function (R::fpPolyRing)(x::Integer)
  z = fpPolyRingElem(R.n, x)
  z.parent = R
  return z
end

function (R::fpPolyRing)(x::fpPolyRingElem)
   R != parent(x) && error("Wrong parents")
   return x
end

function (R::fpPolyRing)(x::fpFieldElem)
  base_ring(R) != parent(x) && error("Wrong parents")
  z = fpPolyRingElem(R.n, x.data)
  z.parent = R
  return z
end

function (R::fpPolyRing)(arr::Vector{ZZRingElem})
  z = fpPolyRingElem(R.n, arr)
  z.parent = R
  return z
end

function (R::fpPolyRing)(arr::Vector{UInt})
  z = fpPolyRingElem(R.n, arr)
  z.parent = R
  return z
end

(R::fpPolyRing)(arr::Vector{T}) where {T <: Integer} = R(map(base_ring(R), arr))

function (R::fpPolyRing)(arr::Vector{fpFieldElem})
  if length(arr) > 0
     (base_ring(R) != parent(arr[1])) && error("Wrong parents")
  end
  z = fpPolyRingElem(R.n, arr)
  z.parent = R
  return z
end

function (R::fpPolyRing)(x::ZZPolyRingElem)
  z = fpPolyRingElem(R.n, x)
  z.parent = R
  return z
end
