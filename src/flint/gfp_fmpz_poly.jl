################################################################################
#
#  fmpz_mod_poly.jl : Flint fmpz_mod_poly (polynomials over Z/nZ, large modulus)
#
################################################################################

export GFPFmpzPolyRing, gfp_fmpz_poly

################################################################################
#
#  Type and parent object methods
#
################################################################################

parent(a::gfp_fmpz_poly) = a.parent

base_ring(R::GFPFmpzPolyRing) = R.base_ring

base_ring(a::gfp_fmpz_poly) = base_ring(parent(a))

elem_type(::Type{gfp_fmpz_poly}) = gfp_fmpz_poly

elem_type(::Type{GFPFmpzPolyRing}) = gfp_fmpz_poly

parent_type(::Type{gfp_fmpz_poly}) = GFPFmpzPolyRing

characteristic(R::GFPFmpzPolyRing) = characteristic(base_ring(R))

###############################################################################
#
#  Ad hoc binary operations
#
###############################################################################

function *(x::gfp_fmpz_poly, y::fmpz)
  z = parent(x)()
  ccall((:fmpz_mod_poly_scalar_mul_fmpz, libflint), Nothing,
        (Ref{gfp_fmpz_poly}, Ref{gfp_fmpz_poly}, Ref{fmpz},
         Ref{fmpz_mod_ctx_struct}),
        z, x, y, x.parent.base_ring.ninv)
  return z
end

*(x::fmpz, y::gfp_fmpz_poly) = y*x

*(x::gfp_fmpz_poly, y::Integer) = x*fmpz(y)

*(x::Integer, y::gfp_fmpz_poly) = y*x

function *(x::gfp_fmpz_poly, y::gfp_fmpz_elem)
  (base_ring(x) != parent(y)) && error("Must have same parent")
  return x*y.data
end

*(x::gfp_fmpz_elem, y::gfp_fmpz_poly) = y*x

function +(x::gfp_fmpz_poly, y::Int)
  z = parent(x)()
  ccall((:fmpz_mod_poly_add_si, libflint), Nothing,
        (Ref{gfp_fmpz_poly}, Ref{gfp_fmpz_poly}, Int, Ref{fmpz_mod_ctx_struct}),
        z, x, y, x.parent.base_ring.ninv)
  return z
end

+(x::Int, y::gfp_fmpz_poly) = +(y, x)

function +(x::gfp_fmpz_poly, y::fmpz)
  z = parent(x)()
  ccall((:fmpz_mod_poly_add_fmpz, libflint), Nothing,
        (Ref{gfp_fmpz_poly}, Ref{gfp_fmpz_poly}, Ref{fmpz},
         Ref{fmpz_mod_ctx_struct}),
        z, x, y, x.parent.base_ring.ninv)
  return z
end

+(x::fmpz, y::gfp_fmpz_poly) = y + x

+(x::gfp_fmpz_poly, y::Integer) = x + fmpz(y)

+(x::Integer, y::gfp_fmpz_poly) = fmpz(y) + x 

function +(x::gfp_fmpz_poly, y::gfp_fmpz_elem)
  (base_ring(x) != parent(y)) && error("Elements must have same parent")
  return x + y.data
end

+(x::gfp_fmpz_elem, y::gfp_fmpz_poly) = y + x

function -(x::gfp_fmpz_poly, y::Int)
  z = parent(x)()
  ccall((:fmpz_mod_poly_sub_si, libflint), Nothing,
        (Ref{gfp_fmpz_poly}, Ref{gfp_fmpz_poly}, Int, Ref{fmpz_mod_ctx_struct}),
        z, x, y, x.parent.base_ring.ninv)
  return z
end

function -(x::Int, y::gfp_fmpz_poly)
  z = parent(y)()
  ccall((:fmpz_mod_poly_si_sub, libflint), Nothing,
        (Ref{gfp_fmpz_poly}, Int, Ref{gfp_fmpz_poly}, Ref{fmpz_mod_ctx_struct}),
        z, x, y, y.parent.base_ring.ninv)
  return z
end

function -(x::gfp_fmpz_poly, y::fmpz)
  z = parent(x)()
  ccall((:fmpz_mod_poly_sub_fmpz, libflint), Nothing,
        (Ref{gfp_fmpz_poly}, Ref{gfp_fmpz_poly}, Ref{fmpz},
         Ref{fmpz_mod_ctx_struct}),
        z, x, y, x.parent.base_ring.ninv)
  return z
end

function -(x::fmpz, y::gfp_fmpz_poly)
  z = parent(y)()
  ccall((:fmpz_mod_poly_fmpz_sub, libflint), Nothing,
        (Ref{gfp_fmpz_poly}, Ref{fmpz}, Ref{gfp_fmpz_poly},
         Ref{fmpz_mod_ctx_struct}),
        z, x, y, y.parent.base_ring.ninv)
  return z
end

-(x::gfp_fmpz_poly, y::Integer) = x - fmpz(y)

-(x::Integer, y::gfp_fmpz_poly) = fmpz(x) - y

function -(x::gfp_fmpz_poly, y::gfp_fmpz_elem)
  (base_ring(x) != parent(y)) && error("Elements must have same parent")
  return x - y.data
end

function -(x::gfp_fmpz_elem, y::gfp_fmpz_poly)
   (parent(x) != base_ring(y)) && error("Elements must have same parent")
   return x.data - y
end

################################################################################
#
#  Ad hoc comparisons
#
################################################################################

function ==(x::gfp_fmpz_poly, y::gfp_fmpz_elem)
  base_ring(x) != parent(y) && error("Incompatible base rings in comparison")
  if length(x) > 1
     return false
  elseif length(x) == 1 
     u = fmpz()
     ccall((:fmpz_mod_poly_get_coeff_fmpz, libflint), Nothing, 
           (Ref{fmpz}, Ref{gfp_fmpz_poly}, Int, Ref{fmpz_mod_ctx_struct}),
           u, x, 0, x.parent.base_ring.ninv)
     return u == y
  else
    return iszero(y)
  end 
end

==(x::gfp_fmpz_elem, y::gfp_fmpz_poly) = y == x

################################################################################
#
#  Ad hoc exact division
#
################################################################################

function divexact(x::gfp_fmpz_poly, y::gfp_fmpz_elem)
  base_ring(x) != parent(y) && error("Elements must have same parent")
  iszero(y) && throw(DivideError())
  q = parent(x)()
  ccall((:fmpz_mod_poly_scalar_div_fmpz, libflint), Nothing, 
        (Ref{gfp_fmpz_poly}, Ref{gfp_fmpz_poly}, Ref{fmpz},
         Ref{fmpz_mod_ctx_struct}), 
        q, x, y.data, x.parent.base_ring.ninv)
  return q
end

###############################################################################
#
#   Integral
#
###############################################################################

function integral(x::gfp_fmpz_poly)
   len = length(x)
   v = Vector{gfp_fmpz_elem}(undef, len + 1)
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
    function lift(R::FmpzPolyRing, y::gfp_fmpz_poly)

Lift from a polynomial over $\mathbb{Z}/n\mathbb{Z}$ to a polynomial over
$\mathbb{Z}$ with minimal reduced nonnegative coefficients. The ring `R`
specifies the ring to lift into.
"""
function lift(R::FmpzPolyRing, y::gfp_fmpz_poly)
   z = fmpz_poly()
   ccall((:fmpz_mod_poly_get_fmpz_poly, libflint), Nothing,
         (Ref{fmpz_poly}, Ref{gfp_fmpz_poly}, Ref{fmpz_mod_ctx_struct}),
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
    isirreducible(x::gfp_fmpz_poly)

Return `true` if $x$ is irreducible, otherwise return `false`.
"""
function isirreducible(x::gfp_fmpz_poly)
  return Bool(ccall((:fmpz_mod_poly_is_irreducible, libflint), Cint,
                    (Ref{gfp_fmpz_poly}, Ref{fmpz_mod_ctx_struct}),
                    x, x.parent.base_ring.ninv))
end

################################################################################
#
#  Squarefree testing
#
################################################################################

@doc Markdown.doc"""
    issquarefree(x::gfp_fmpz_poly)

Return `true` if $x$ is squarefree, otherwise return `false`.
"""
function issquarefree(x::gfp_fmpz_poly)
   return Bool(ccall((:fmpz_mod_poly_is_squarefree, libflint), Cint,
                     (Ref{gfp_fmpz_poly}, Ref{fmpz_mod_ctx_struct}),
                     x, x.parent.base_ring.ninv))
end

################################################################################
#
#  Factorization
#
################################################################################

@doc Markdown.doc"""
    factor(x::gfp_fmpz_poly)

Return the factorisation of $x$.
"""
function factor(x::gfp_fmpz_poly)
  fac = _factor(x)
  return Fac(parent(x)(lead(x)), fac)
end

function _factor(x::gfp_fmpz_poly)
  n = x.parent.base_ring.ninv
  fac = gfp_fmpz_poly_factor(n)
  ccall((:fmpz_mod_poly_factor, libflint), Nothing,
        (Ref{gfp_fmpz_poly_factor}, Ref{gfp_fmpz_poly},
         Ref{fmpz_mod_ctx_struct}),
        fac, x, n)
  res = Dict{gfp_fmpz_poly, Int}()
  for i in 1:fac.num
    f = parent(x)()
    ccall((:fmpz_mod_poly_factor_get_fmpz_mod_poly, libflint), Nothing,
          (Ref{gfp_fmpz_poly}, Ref{gfp_fmpz_poly_factor}, Int,
           Ref{fmpz_mod_ctx_struct}),
          f, fac, i - 1, n)
    e = unsafe_load(fac.exp, i)
    res[f] = e
  end
  return res 
end  

@doc Markdown.doc"""
    factor_squarefree(x::gfp_fmpz_poly)

Return the squarefree factorisation of $x$.
"""
function factor_squarefree(x::gfp_fmpz_poly)
  fac = _factor_squarefree(x)
  return Fac(parent(x)(lead(x)), fac)
end

function _factor_squarefree(x::gfp_fmpz_poly)
  n = x.parent.base_ring.ninv
  fac = gfp_fmpz_poly_factor(n)
  ccall((:fmpz_mod_poly_factor_squarefree, libflint), UInt,
        (Ref{gfp_fmpz_poly_factor}, Ref{gfp_fmpz_poly},
         Ref{fmpz_mod_ctx_struct}),
        fac, x, n)
  res = Dict{gfp_fmpz_poly, Int}()
  for i in 1:fac.num
    f = parent(x)()
    ccall((:fmpz_mod_poly_factor_get_fmpz_mod_poly, libflint), Nothing,
          (Ref{gfp_fmpz_poly}, Ref{gfp_fmpz_poly_factor}, Int,
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
function factor_distinct_deg(x::gfp_fmpz_poly)
  !issquarefree(x) && error("Polynomial must be squarefree")
  degs = Vector{Int}(undef, degree(x))
  degss = [ pointer(degs) ]
  n = x.parent.base_ring.ninv
  fac = gfp_fmpz_poly_factor(n)
  ccall((:fmpz_mod_poly_factor_distinct_deg, libflint), UInt,
        (Ref{gfp_fmpz_poly_factor}, Ref{gfp_fmpz_poly}, Ptr{Ptr{Int}},
         Ref{fmpz_mod_ctx_struct}),
        fac, x, degss, n)
  res = Dict{Int, gfp_fmpz_poly}()
  for i in 1:fac.num
    f = parent(x)()
    ccall((:fmpz_mod_poly_factor_get_fmpz_mod_poly, libflint), Nothing,
          (Ref{gfp_fmpz_poly}, Ref{gfp_fmpz_poly_factor}, Int,
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

setcoeff!(x::gfp_fmpz_poly, n::Int, y::gfp_fmpz_elem) = setcoeff!(x, n, y.data)

################################################################################
#
#  Promotion rules
#
################################################################################

promote_rule(::Type{gfp_fmpz_poly}, ::Type{gfp_fmpz_elem}) = gfp_fmpz_poly

###############################################################################
#
#   Polynomial substitution
#
###############################################################################

function (f::gfp_fmpz_poly)(a::gfp_fmpz_elem)
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

function (R::GFPFmpzPolyRing)()
  z = gfp_fmpz_poly(R.n)
  z.parent = R
  return z
end

function (R::GFPFmpzPolyRing)(x::fmpz)
  z = gfp_fmpz_poly(R.n, x)
  z.parent = R
  return z
end

function (R::GFPFmpzPolyRing)(x::Integer)
  z = gfp_fmpz_poly(R.n, fmpz(x))
  z.parent = R
  return z
end

function (R::GFPFmpzPolyRing)(x::gfp_fmpz_elem)
  base_ring(R) != parent(x) && error("Wrong parents")
  z = gfp_fmpz_poly(R.n, x.data)
  z.parent = R
  return z
end

function (R::GFPFmpzPolyRing)(arr::Array{fmpz, 1})
  z = gfp_fmpz_poly(R.n, arr)
  z.parent = R
  return z
end

function (R::GFPFmpzPolyRing)(arr::Array{gfp_fmpz_elem, 1})
  if length(arr) > 0
     (base_ring(R) != parent(arr[1])) && error("Wrong parents")
  end
  z = gfp_fmpz_poly(R.n, arr)
  z.parent = R
  return z
end

(R::GFPFmpzPolyRing)(arr::Array{T, 1}) where {T <: Integer} = R(map(base_ring(R), arr))

function (R::GFPFmpzPolyRing)(x::fmpz_poly)
  z = gfp_fmpz_poly(R.n, x)
  z.parent = R
  return z
end

function (R::GFPFmpzPolyRing)(f::gfp_fmpz_poly)
   parent(f) != R && error("Unable to coerce polynomial")
   return f
end

################################################################################
#
#  Polynomial ring constructor
#
################################################################################

function PolynomialRing(R::GaloisFmpzField, s::AbstractString; cached=true)
   parent_obj = GFPFmpzPolyRing(R, Symbol(s), cached)

   return parent_obj, parent_obj([R(0), R(1)])
end
