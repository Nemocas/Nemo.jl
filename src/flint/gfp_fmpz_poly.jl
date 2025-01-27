################################################################################
#
#  fmpz_mod_poly.jl: FLINT fmpz_mod_poly (polynomials over Z/nZ, large modulus)
#
################################################################################

################################################################################
#
#  Type and parent object methods
#
################################################################################

parent(a::FpPolyRingElem) = a.parent

base_ring(R::FpPolyRing) = R.base_ring

elem_type(::Type{FpPolyRing}) = FpPolyRingElem

parent_type(::Type{FpPolyRingElem}) = FpPolyRing

dense_poly_type(::Type{FpFieldElem}) = FpPolyRingElem

###############################################################################
#
#   Similar
#
###############################################################################

function similar(f::PolyRingElem, R::FpField, s::Symbol=var(parent(f)); cached::Bool=true)
  z = FpPolyRingElem(R)
  if base_ring(f) === R && s == var(parent(f)) && f isa FpPolyRingElem
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

function polynomial(R::FpField, arr::Vector{T}, var::VarName=:x; cached::Bool=true) where T
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
  @ccall libflint.fmpz_mod_poly_scalar_mul_fmpz(z::Ref{FpPolyRingElem}, x::Ref{FpPolyRingElem}, y::Ref{ZZRingElem}, x.parent.base_ring.ninv::Ref{fmpz_mod_ctx_struct})::Nothing
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
  @ccall libflint.fmpz_mod_poly_add_si(z::Ref{FpPolyRingElem}, x::Ref{FpPolyRingElem}, y::Int, x.parent.base_ring.ninv::Ref{fmpz_mod_ctx_struct})::Nothing
  return z
end

+(x::Int, y::FpPolyRingElem) = +(y, x)

function +(x::FpPolyRingElem, y::ZZRingElem)
  z = parent(x)()
  @ccall libflint.fmpz_mod_poly_add_fmpz(z::Ref{FpPolyRingElem}, x::Ref{FpPolyRingElem}, y::Ref{ZZRingElem}, x.parent.base_ring.ninv::Ref{fmpz_mod_ctx_struct})::Nothing
  return z
end

+(x::ZZRingElem, y::FpPolyRingElem) = y + x

+(x::FpPolyRingElem, y::Integer) = x + ZZRingElem(y)

+(x::Integer, y::FpPolyRingElem) = ZZRingElem(x) + y

function +(x::FpPolyRingElem, y::FpFieldElem)
  (base_ring(x) != parent(y)) && error("Elements must have same parent")
  return x + y.data
end

+(x::FpFieldElem, y::FpPolyRingElem) = y + x

function -(x::FpPolyRingElem, y::Int)
  z = parent(x)()
  @ccall libflint.fmpz_mod_poly_sub_si(z::Ref{FpPolyRingElem}, x::Ref{FpPolyRingElem}, y::Int, x.parent.base_ring.ninv::Ref{fmpz_mod_ctx_struct})::Nothing
  return z
end

function -(x::Int, y::FpPolyRingElem)
  z = parent(y)()
  @ccall libflint.fmpz_mod_poly_si_sub(z::Ref{FpPolyRingElem}, x::Int, y::Ref{FpPolyRingElem}, y.parent.base_ring.ninv::Ref{fmpz_mod_ctx_struct})::Nothing
  return z
end

function -(x::FpPolyRingElem, y::ZZRingElem)
  z = parent(x)()
  @ccall libflint.fmpz_mod_poly_sub_fmpz(z::Ref{FpPolyRingElem}, x::Ref{FpPolyRingElem}, y::Ref{ZZRingElem}, x.parent.base_ring.ninv::Ref{fmpz_mod_ctx_struct})::Nothing
  return z
end

function -(x::ZZRingElem, y::FpPolyRingElem)
  z = parent(y)()
  @ccall libflint.fmpz_mod_poly_fmpz_sub(z::Ref{FpPolyRingElem}, x::Ref{ZZRingElem}, y::Ref{FpPolyRingElem}, y.parent.base_ring.ninv::Ref{fmpz_mod_ctx_struct})::Nothing
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
    @ccall libflint.fmpz_mod_poly_get_coeff_fmpz(u::Ref{ZZRingElem}, x::Ref{FpPolyRingElem}, 0::Int, x.parent.base_ring.ninv::Ref{fmpz_mod_ctx_struct})::Nothing
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
  @ccall libflint.fmpz_mod_poly_scalar_div_fmpz(q::Ref{FpPolyRingElem}, x::Ref{FpPolyRingElem}, y.data::Ref{ZZRingElem}, x.parent.base_ring.ninv::Ref{fmpz_mod_ctx_struct})::Nothing
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

@doc raw"""
    lift(R::ZZPolyRing, y::FpPolyRingElem)

Lift from a polynomial over $\mathbb{Z}/n\mathbb{Z}$ to a polynomial over
$\mathbb{Z}$ with minimal reduced non-negative coefficients. The ring `R`
specifies the ring to lift into.
"""
function lift(R::ZZPolyRing, y::FpPolyRingElem)
  z = ZZPolyRingElem()
  @ccall libflint.fmpz_mod_poly_get_fmpz_poly(z::Ref{ZZPolyRingElem}, y::Ref{FpPolyRingElem}, y.parent.base_ring.ninv::Ref{fmpz_mod_ctx_struct})::Nothing
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
  @ccall libflint.fmpz_mod_poly_gcd(z::Ref{FpPolyRingElem}, x::Ref{FpPolyRingElem}, y::Ref{FpPolyRingElem}, x.parent.base_ring.ninv::Ref{fmpz_mod_ctx_struct})::Nothing
  return z
end

function gcdx(x::FpPolyRingElem, y::FpPolyRingElem)
  check_parent(x, y)
  g = parent(x)()
  s = parent(x)()
  t = parent(x)()
  @ccall libflint.fmpz_mod_poly_xgcd(g::Ref{FpPolyRingElem}, s::Ref{FpPolyRingElem}, t::Ref{FpPolyRingElem}, x::Ref{FpPolyRingElem}, y::Ref{FpPolyRingElem}, x.parent.base_ring.ninv::Ref{fmpz_mod_ctx_struct})::Nothing
  return g, s, t
end

################################################################################
#
#  Irreducibility
#
################################################################################

function is_irreducible(x::FpPolyRingElem)
  is_constant(x) && return false
  return Bool(@ccall libflint.fmpz_mod_poly_is_irreducible(x::Ref{FpPolyRingElem}, x.parent.base_ring.ninv::Ref{fmpz_mod_ctx_struct})::Cint)
end

################################################################################
#
#  Squarefree testing
#
################################################################################

function is_squarefree(x::FpPolyRingElem)
  return Bool(@ccall libflint.fmpz_mod_poly_is_squarefree(x::Ref{FpPolyRingElem}, x.parent.base_ring.ninv::Ref{fmpz_mod_ctx_struct})::Cint)
end

################################################################################
#
#  Square root
#
################################################################################

function Base.sqrt(x::FpPolyRingElem; check::Bool=true)
  s = parent(x)()
  flag = Bool(@ccall libflint.fmpz_mod_poly_sqrt(s::Ref{FpPolyRingElem}, x::Ref{FpPolyRingElem}, x.parent.base_ring.ninv::Ref{fmpz_mod_ctx_struct})::Cint)
  check && !flag && error("Not a square in sqrt")
  return s
end

function is_square(x::FpPolyRingElem)
  s = parent(x)()
  flag = Bool(@ccall libflint.fmpz_mod_poly_sqrt(s::Ref{FpPolyRingElem}, x::Ref{FpPolyRingElem}, x.parent.base_ring.ninv::Ref{fmpz_mod_ctx_struct})::Cint)
  return flag
end

function is_square_with_sqrt(x::FpPolyRingElem)
  s = parent(x)()
  flag = Bool(@ccall libflint.fmpz_mod_poly_sqrt(s::Ref{FpPolyRingElem}, x::Ref{FpPolyRingElem}, x.parent.base_ring.ninv::Ref{fmpz_mod_ctx_struct})::Cint)
  return flag, s
end

################################################################################
#
#  Factorization
#
################################################################################

function factor(x::FpPolyRingElem)
  iszero(x) && throw(ArgumentError("Argument must be non-zero"))
  fac = _factor(x)
  return Fac(parent(x)(leading_coefficient(x)), fac)
end

function _factor(x::FpPolyRingElem)
  n = x.parent.base_ring.ninv
  fac = gfp_fmpz_poly_factor(n)
  @ccall libflint.fmpz_mod_poly_factor(fac::Ref{gfp_fmpz_poly_factor}, x::Ref{FpPolyRingElem}, n::Ref{fmpz_mod_ctx_struct})::Nothing
  res = Dict{FpPolyRingElem, Int}()
  for i in 1:fac.num
    f = parent(x)()
    @ccall libflint.fmpz_mod_poly_factor_get_fmpz_mod_poly(f::Ref{FpPolyRingElem}, fac::Ref{gfp_fmpz_poly_factor}, (i - 1)::Int, n::Ref{fmpz_mod_ctx_struct})::Nothing
    e = unsafe_load(fac.exp, i)
    res[f] = e
  end
  return res 
end  

function factor_squarefree(x::FpPolyRingElem)
  iszero(x) && throw(ArgumentError("Argument must be non-zero"))
  fac = _factor_squarefree(x)
  return Fac(parent(x)(leading_coefficient(x)), fac)
end

function _factor_squarefree(x::FpPolyRingElem)
  n = x.parent.base_ring.ninv
  fac = gfp_fmpz_poly_factor(n)
  @ccall libflint.fmpz_mod_poly_factor_squarefree(fac::Ref{gfp_fmpz_poly_factor}, x::Ref{FpPolyRingElem}, n::Ref{fmpz_mod_ctx_struct})::UInt
  res = Dict{FpPolyRingElem, Int}()
  for i in 1:fac.num
    f = parent(x)()
    @ccall libflint.fmpz_mod_poly_factor_get_fmpz_mod_poly(f::Ref{FpPolyRingElem}, fac::Ref{gfp_fmpz_poly_factor}, (i - 1)::Int, n::Ref{fmpz_mod_ctx_struct})::Nothing
    e = unsafe_load(fac.exp, i)
    res[f] = e
  end
  return res 
end  

@doc raw"""
    factor_distinct_deg(x::ZZModPolyRingElem)

Return the distinct degree factorisation of a squarefree polynomial $x$.
"""
function factor_distinct_deg(x::FpPolyRingElem)
  !is_squarefree(x) && error("Polynomial must be squarefree")
  degs = Vector{Int}(undef, degree(x))
  degss = [ pointer(degs) ]
  n = x.parent.base_ring.ninv
  fac = gfp_fmpz_poly_factor(n)
  @ccall libflint.fmpz_mod_poly_factor_distinct_deg(fac::Ref{gfp_fmpz_poly_factor}, x::Ref{FpPolyRingElem}, degss::Ptr{Ptr{Int}}, n::Ref{fmpz_mod_ctx_struct})::UInt
  res = Dict{Int, FpPolyRingElem}()
  for i in 1:fac.num
    f = parent(x)()
    @ccall libflint.fmpz_mod_poly_factor_get_fmpz_mod_poly(f::Ref{FpPolyRingElem}, fac::Ref{gfp_fmpz_poly_factor}, (i - 1)::Int, n::Ref{fmpz_mod_ctx_struct})::Nothing
    res[degs[i]] = f
  end
  return res 
end

# Factor x assuming that all irreducible factors are of degree d
function factor_equal_deg(x::FpPolyRingElem, d::Int)
  if degree(x) == d
    return FpPolyRingElem[x]
  end
  fac = gfp_fmpz_poly_factor(base_ring(x))
  @ccall libflint.fmpz_mod_poly_factor_equal_deg(fac::Ref{gfp_fmpz_poly_factor}, x::Ref{FpPolyRingElem}, d::Int, x.parent.base_ring.ninv::Ref{fmpz_mod_ctx_struct})::UInt
  res = Vector{FpPolyRingElem}(undef, fac.num)
  for i in 1:fac.num
    f = parent(x)()
    @ccall libflint.fmpz_mod_poly_factor_get_fmpz_mod_poly(f::Ref{FpPolyRingElem}, fac::Ref{gfp_fmpz_poly_factor}, (i - 1)::Int, x.parent.base_ring.ninv::Ref{fmpz_mod_ctx_struct})::Nothing
    res[i] = f
  end
  return res
end

function roots(a::FpPolyRingElem)
  R = parent(a)
  n = R.base_ring.ninv
  fac = fmpz_mod_poly_factor(n)
  @ccall libflint.fmpz_mod_poly_roots(fac::Ref{fmpz_mod_poly_factor}, a::Ref{FpPolyRingElem}, 0::Cint, n::Ref{fmpz_mod_ctx_struct})::UInt
  f = R()
  res = FpFieldElem[]
  for i in 1:fac.num
    @ccall libflint.fmpz_mod_poly_factor_get_fmpz_mod_poly(f::Ref{FpPolyRingElem}, fac::Ref{fmpz_mod_poly_factor}, (i - 1)::Int, n::Ref{fmpz_mod_ctx_struct})::Nothing
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
