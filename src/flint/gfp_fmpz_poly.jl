################################################################################
#
#  fmpz_mod_poly.jl: Flint fmpz_mod_poly (polynomials over Z/nZ, large modulus)
#
################################################################################

export FpPolyRing, FpPolyRingElem

################################################################################
#
#  Type and parent object methods
#
################################################################################

parent(a::FpPolyRingElem) = a.parent

base_ring(R::FpPolyRing) = R.base_ring

base_ring(a::FpPolyRingElem) = base_ring(parent(a))

elem_type(::Type{FpPolyRingElem}) = FpPolyRingElem

elem_type(::Type{FpPolyRing}) = FpPolyRingElem

parent_type(::Type{FpPolyRingElem}) = FpPolyRing

dense_poly_type(::Type{Generic.ResidueFieldElem{ZZRingElem}}) = FpPolyRingElem

characteristic(R::FpPolyRing) = characteristic(base_ring(R))

###############################################################################
#
#   Similar
#
###############################################################################

function similar(f::PolyElem, R::FpField, s::Symbol=var(parent(f)); cached::Bool=true)
   z = FpPolyRingElem(R)
   if base_ring(f) === R && s == var(parent(f)) && typeof(f) == FpPolyRingElem
      # steal parent in case it is not cached
      z.parent = parent(f)
   else
      z.parent = FpPolyRing(R, s, cached)
   end
   return z
end

###############################################################################
#
#   polynomial constructor
#
###############################################################################

function polynomial(R::FpField, arr::Vector{T}, var::String="x"; cached::Bool=true) where T
   coeffs = map(R, arr)
   coeffs = length(coeffs) == 0 ? FpFieldElem[] : coeffs
   z = FpPolyRingElem(R, coeffs)
   z.parent = FpPolyRing(R, Symbol(var), cached)
   return z
end

###############################################################################
#
#  Ad hoc binary operations
#
###############################################################################

function *(x::FpPolyRingElem, y::ZZRingElem)
  z = parent(x)()
  ccall((:fmpz_mod_poly_scalar_mul_fmpz, libflint), Nothing,
        (Ref{FpPolyRingElem}, Ref{FpPolyRingElem}, Ref{ZZRingElem},
         Ref{fmpz_mod_ctx_struct}),
        z, x, y, x.parent.base_ring.ninv)
  return z
end

*(x::ZZRingElem, y::FpPolyRingElem) = y*x

*(x::FpPolyRingElem, y::Integer) = x*ZZRingElem(y)

*(x::Integer, y::FpPolyRingElem) = y*x

function *(x::FpPolyRingElem, y::FpFieldElem)
  (base_ring(x) != parent(y)) && error("Must have same parent")
  return x*y.data
end

*(x::FpFieldElem, y::FpPolyRingElem) = y*x

function +(x::FpPolyRingElem, y::Int)
  z = parent(x)()
  ccall((:fmpz_mod_poly_add_si, libflint), Nothing,
        (Ref{FpPolyRingElem}, Ref{FpPolyRingElem}, Int, Ref{fmpz_mod_ctx_struct}),
        z, x, y, x.parent.base_ring.ninv)
  return z
end

+(x::Int, y::FpPolyRingElem) = +(y, x)

function +(x::FpPolyRingElem, y::ZZRingElem)
  z = parent(x)()
  ccall((:fmpz_mod_poly_add_fmpz, libflint), Nothing,
        (Ref{FpPolyRingElem}, Ref{FpPolyRingElem}, Ref{ZZRingElem},
         Ref{fmpz_mod_ctx_struct}),
        z, x, y, x.parent.base_ring.ninv)
  return z
end

+(x::ZZRingElem, y::FpPolyRingElem) = y + x

+(x::FpPolyRingElem, y::Integer) = x + ZZRingElem(y)

+(x::Integer, y::FpPolyRingElem) = ZZRingElem(y) + x 

function +(x::FpPolyRingElem, y::FpFieldElem)
  (base_ring(x) != parent(y)) && error("Elements must have same parent")
  return x + y.data
end

+(x::FpFieldElem, y::FpPolyRingElem) = y + x

function -(x::FpPolyRingElem, y::Int)
  z = parent(x)()
  ccall((:fmpz_mod_poly_sub_si, libflint), Nothing,
        (Ref{FpPolyRingElem}, Ref{FpPolyRingElem}, Int, Ref{fmpz_mod_ctx_struct}),
        z, x, y, x.parent.base_ring.ninv)
  return z
end

function -(x::Int, y::FpPolyRingElem)
  z = parent(y)()
  ccall((:fmpz_mod_poly_si_sub, libflint), Nothing,
        (Ref{FpPolyRingElem}, Int, Ref{FpPolyRingElem}, Ref{fmpz_mod_ctx_struct}),
        z, x, y, y.parent.base_ring.ninv)
  return z
end

function -(x::FpPolyRingElem, y::ZZRingElem)
  z = parent(x)()
  ccall((:fmpz_mod_poly_sub_fmpz, libflint), Nothing,
        (Ref{FpPolyRingElem}, Ref{FpPolyRingElem}, Ref{ZZRingElem},
         Ref{fmpz_mod_ctx_struct}),
        z, x, y, x.parent.base_ring.ninv)
  return z
end

function -(x::ZZRingElem, y::FpPolyRingElem)
  z = parent(y)()
  ccall((:fmpz_mod_poly_fmpz_sub, libflint), Nothing,
        (Ref{FpPolyRingElem}, Ref{ZZRingElem}, Ref{FpPolyRingElem},
         Ref{fmpz_mod_ctx_struct}),
        z, x, y, y.parent.base_ring.ninv)
  return z
end

-(x::FpPolyRingElem, y::Integer) = x - ZZRingElem(y)

-(x::Integer, y::FpPolyRingElem) = ZZRingElem(x) - y

function -(x::FpPolyRingElem, y::FpFieldElem)
  (base_ring(x) != parent(y)) && error("Elements must have same parent")
  return x - y.data
end

function -(x::FpFieldElem, y::FpPolyRingElem)
   (parent(x) != base_ring(y)) && error("Elements must have same parent")
   return x.data - y
end

################################################################################
#
#  Ad hoc comparisons
#
################################################################################

function ==(x::FpPolyRingElem, y::FpFieldElem)
  base_ring(x) != parent(y) && error("Incompatible base rings in comparison")
  if length(x) > 1
     return false
  elseif length(x) == 1 
     u = ZZRingElem()
     ccall((:fmpz_mod_poly_get_coeff_fmpz, libflint), Nothing, 
           (Ref{ZZRingElem}, Ref{FpPolyRingElem}, Int, Ref{fmpz_mod_ctx_struct}),
           u, x, 0, x.parent.base_ring.ninv)
     return u == y
  else
    return iszero(y)
  end 
end

==(x::FpFieldElem, y::FpPolyRingElem) = y == x

################################################################################
#
#  Ad hoc exact division
#
################################################################################

function divexact(x::FpPolyRingElem, y::FpFieldElem; check::Bool=true)
  base_ring(x) != parent(y) && error("Elements must have same parent")
  iszero(y) && throw(DivideError())
  q = parent(x)()
  ccall((:fmpz_mod_poly_scalar_div_fmpz, libflint), Nothing, 
        (Ref{FpPolyRingElem}, Ref{FpPolyRingElem}, Ref{ZZRingElem},
         Ref{fmpz_mod_ctx_struct}), 
        q, x, y.data, x.parent.base_ring.ninv)
  return q
end

###############################################################################
#
#   Integral
#
###############################################################################

function integral(x::FpPolyRingElem)
   len = length(x)
   v = Vector{FpFieldElem}(undef, len + 1)
   v[1] = zero(base_ring(x))
   for i = 1:len
      v[i + 1] = divexact(coeff(x, i - 1), base_ring(x)(i))
   end
   return parent(x)(v)
end

################################################################################
#
#  Lifting
#
################################################################################

@doc Markdown.doc"""
    lift(R::ZZPolyRing, y::FpPolyRingElem)

Lift from a polynomial over $\mathbb{Z}/n\mathbb{Z}$ to a polynomial over
$\mathbb{Z}$ with minimal reduced nonnegative coefficients. The ring `R`
specifies the ring to lift into.
"""
function lift(R::ZZPolyRing, y::FpPolyRingElem)
   z = ZZPolyRingElem()
   ccall((:fmpz_mod_poly_get_fmpz_poly, libflint), Nothing,
         (Ref{ZZPolyRingElem}, Ref{FpPolyRingElem}, Ref{fmpz_mod_ctx_struct}),
         z, y, y.parent.base_ring.ninv)
   z.parent = R
   return z
end

################################################################################
#
#   GCD
#
################################################################################

function gcd(x::FpPolyRingElem, y::FpPolyRingElem)
   check_parent(x, y)
   z = parent(x)()
   f = ZZRingElem()
   ccall((:fmpz_mod_poly_gcd, libflint), Nothing,
         (Ref{FpPolyRingElem},
          Ref{FpPolyRingElem}, Ref{FpPolyRingElem}, Ref{fmpz_mod_ctx_struct}),
         z, x, y, x.parent.base_ring.ninv)
   return z
end

function gcdx(x::FpPolyRingElem, y::FpPolyRingElem)
  check_parent(x, y)
  g = parent(x)()
  s = parent(x)()
  t = parent(x)()
  ccall((:fmpz_mod_poly_xgcd, libflint), Nothing,
        (Ref{FpPolyRingElem}, Ref{FpPolyRingElem}, Ref{FpPolyRingElem},
         Ref{FpPolyRingElem}, Ref{FpPolyRingElem}, Ref{fmpz_mod_ctx_struct}),
        g, s, t, x, y, x.parent.base_ring.ninv)
  return g, s, t
end

function gcdinv(x::FpPolyRingElem, y::FpPolyRingElem)
   check_parent(x,y)
   length(y) >= 2 || error("Length of second argument must be >= 2")
   g = parent(x)()
   s = parent(x)()
   ccall((:fmpz_mod_poly_gcdinv, libflint), Nothing,
         (Ref{FpPolyRingElem}, Ref{FpPolyRingElem},
          Ref{FpPolyRingElem}, Ref{FpPolyRingElem}, Ref{fmpz_mod_ctx_struct}),
         g, s, x, y, x.parent.base_ring.ninv)
   return g, s
end

################################################################################
#
#  Irreducibility
#
################################################################################

function is_irreducible(x::FpPolyRingElem)
  return Bool(ccall((:fmpz_mod_poly_is_irreducible, libflint), Cint,
                    (Ref{FpPolyRingElem}, Ref{fmpz_mod_ctx_struct}),
                    x, x.parent.base_ring.ninv))
end

################################################################################
#
#  Squarefree testing
#
################################################################################

function is_squarefree(x::FpPolyRingElem)
   return Bool(ccall((:fmpz_mod_poly_is_squarefree, libflint), Cint,
                     (Ref{FpPolyRingElem}, Ref{fmpz_mod_ctx_struct}),
                     x, x.parent.base_ring.ninv))
end

################################################################################
#
#  Square root
#
################################################################################

function Base.sqrt(x::FpPolyRingElem; check::Bool=true)
   s = parent(x)()
   flag = Bool(ccall((:fmpz_mod_poly_sqrt, libflint), Cint,
                     (Ref{FpPolyRingElem}, Ref{FpPolyRingElem}, Ref{fmpz_mod_ctx_struct}),
                      s, x, x.parent.base_ring.ninv))
   check && !flag && error("Not a square in sqrt")
   return s
end

function issquare(x::FpPolyRingElem)
   s = parent(x)()
   flag = Bool(ccall((:fmpz_mod_poly_sqrt, libflint), Cint,
                     (Ref{FpPolyRingElem}, Ref{FpPolyRingElem}, Ref{fmpz_mod_ctx_struct}),
                      s, x, x.parent.base_ring.ninv))
   return flag
end

function issquare_with_sqrt(x::FpPolyRingElem)
   s = parent(x)()
   flag = Bool(ccall((:fmpz_mod_poly_sqrt, libflint), Cint,
                     (Ref{FpPolyRingElem}, Ref{FpPolyRingElem}, Ref{fmpz_mod_ctx_struct}),
                      s, x, x.parent.base_ring.ninv))
   return flag, s
end

################################################################################
#
#  Factorization
#
################################################################################

function factor(x::FpPolyRingElem)
  fac = _factor(x)
  return Fac(parent(x)(leading_coefficient(x)), fac)
end

function _factor(x::FpPolyRingElem)
  n = x.parent.base_ring.ninv
  fac = gfp_fmpz_poly_factor(n)
  ccall((:fmpz_mod_poly_factor, libflint), Nothing,
        (Ref{gfp_fmpz_poly_factor}, Ref{FpPolyRingElem},
         Ref{fmpz_mod_ctx_struct}),
        fac, x, n)
  res = Dict{FpPolyRingElem, Int}()
  for i in 1:fac.num
    f = parent(x)()
    ccall((:fmpz_mod_poly_factor_get_fmpz_mod_poly, libflint), Nothing,
          (Ref{FpPolyRingElem}, Ref{gfp_fmpz_poly_factor}, Int,
           Ref{fmpz_mod_ctx_struct}),
          f, fac, i - 1, n)
    e = unsafe_load(fac.exp, i)
    res[f] = e
  end
  return res 
end  

function factor_squarefree(x::FpPolyRingElem)
  fac = _factor_squarefree(x)
  return Fac(parent(x)(leading_coefficient(x)), fac)
end

function _factor_squarefree(x::FpPolyRingElem)
  n = x.parent.base_ring.ninv
  fac = gfp_fmpz_poly_factor(n)
  ccall((:fmpz_mod_poly_factor_squarefree, libflint), UInt,
        (Ref{gfp_fmpz_poly_factor}, Ref{FpPolyRingElem},
         Ref{fmpz_mod_ctx_struct}),
        fac, x, n)
  res = Dict{FpPolyRingElem, Int}()
  for i in 1:fac.num
    f = parent(x)()
    ccall((:fmpz_mod_poly_factor_get_fmpz_mod_poly, libflint), Nothing,
          (Ref{FpPolyRingElem}, Ref{gfp_fmpz_poly_factor}, Int,
           Ref{fmpz_mod_ctx_struct}),
          f, fac, i - 1, n)
    e = unsafe_load(fac.exp, i)
    res[f] = e
  end
  return res 
end  

@doc Markdown.doc"""
    factor_distinct_deg(x::ZZModPolyRingElem)

Return the distinct degree factorisation of a squarefree polynomial $x$.
"""
function factor_distinct_deg(x::FpPolyRingElem)
  !is_squarefree(x) && error("Polynomial must be squarefree")
  degs = Vector{Int}(undef, degree(x))
  degss = [ pointer(degs) ]
  n = x.parent.base_ring.ninv
  fac = gfp_fmpz_poly_factor(n)
  ccall((:fmpz_mod_poly_factor_distinct_deg, libflint), UInt,
        (Ref{gfp_fmpz_poly_factor}, Ref{FpPolyRingElem}, Ptr{Ptr{Int}},
         Ref{fmpz_mod_ctx_struct}),
        fac, x, degss, n)
  res = Dict{Int, FpPolyRingElem}()
  for i in 1:fac.num
    f = parent(x)()
    ccall((:fmpz_mod_poly_factor_get_fmpz_mod_poly, libflint), Nothing,
          (Ref{FpPolyRingElem}, Ref{gfp_fmpz_poly_factor}, Int,
           Ref{fmpz_mod_ctx_struct}),
          f, fac, i - 1, n)
    res[degs[i]] = f
  end
  return res 
end  

function roots(a::FpPolyRingElem)
  R = parent(a)
  n = R.base_ring.ninv
  fac = fmpz_mod_poly_factor(n)
  ccall((:fmpz_mod_poly_roots, libflint), UInt,
          (Ref{fmpz_mod_poly_factor}, Ref{FpPolyRingElem}, Cint, Ref{fmpz_mod_ctx_struct}),
          fac, a, 0, n)
  f = R()
  res = FpFieldElem[]
  for i in 1:fac.num
    ccall((:fmpz_mod_poly_factor_get_fmpz_mod_poly, libflint), Nothing,
          (Ref{FpPolyRingElem}, Ref{fmpz_mod_poly_factor}, Int, Ref{fmpz_mod_ctx_struct}),
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

setcoeff!(x::FpPolyRingElem, n::Int, y::FpFieldElem) = setcoeff!(x, n, y.data)

################################################################################
#
#  Promotion rules
#
################################################################################

promote_rule(::Type{FpPolyRingElem}, ::Type{FpFieldElem}) = FpPolyRingElem

promote_rule(::Type{FpPolyRingElem}, ::Type{ZZRingElem}) = FpPolyRingElem

###############################################################################
#
#   Polynomial substitution
#
###############################################################################

function (f::FpPolyRingElem)(a::FpFieldElem)
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

function (R::FpPolyRing)()
  z = FpPolyRingElem(base_ring(R))
  z.parent = R
  return z
end

function (R::FpPolyRing)(x::ZZRingElem)
  z = FpPolyRingElem(base_ring(R), x)
  z.parent = R
  return z
end

function (R::FpPolyRing)(x::Integer)
  z = FpPolyRingElem(base_ring(R), ZZRingElem(x))
  z.parent = R
  return z
end

function (R::FpPolyRing)(x::FpFieldElem)
  base_ring(R) != parent(x) && error("Wrong parents")
  z = FpPolyRingElem(base_ring(R), x.data)
  z.parent = R
  return z
end

function (R::FpPolyRing)(arr::Vector{ZZRingElem})
  z = FpPolyRingElem(base_ring(R), arr)
  z.parent = R
  return z
end

function (R::FpPolyRing)(arr::Vector{FpFieldElem})
  if length(arr) > 0
     (base_ring(R) != parent(arr[1])) && error("Wrong parents")
  end
  z = FpPolyRingElem(base_ring(R), arr)
  z.parent = R
  return z
end

(R::FpPolyRing)(arr::Vector{T}) where {T <: Integer} = R(map(base_ring(R), arr))

function (R::FpPolyRing)(x::ZZPolyRingElem)
  z = FpPolyRingElem(base_ring(R), x)
  z.parent = R
  return z
end

function (R::FpPolyRing)(f::FpPolyRingElem)
   parent(f) != R && error("Unable to coerce polynomial")
   return f
end

################################################################################
#
#  Polynomial ring constructor
#
################################################################################

function polynomial_ring(R::FpField, s::Symbol; cached=true)
   parent_obj = FpPolyRing(R, s, cached)

   return parent_obj, parent_obj([R(0), R(1)])
end

function polynomial_ring(R::FpField, s::AbstractString; cached = true)
   return polynomial_ring(R, Symbol(s); cached=cached)
end

function PolyRing(R::FpField)
   return FpPolyRing(R, :x, false)
end
