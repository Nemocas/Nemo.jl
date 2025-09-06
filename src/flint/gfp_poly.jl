################################################################################
#
#  gfp_poly.jl: FLINT gfp_poly (polynomials over Z/pZ, small prime modulus)
#
################################################################################

################################################################################
#
#  Type and parent object methods
#
################################################################################

parent(a::fpPolyRingElem) = a.parent

base_ring(R::fpPolyRing) = R.base_ring

parent_type(::Type{fpPolyRingElem}) = fpPolyRing

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
    u = @ccall libflint.nmod_poly_get_coeff_ui(a::Ref{fpPolyRingElem}, i::Int)::UInt
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
    u = @ccall libflint.nmod_poly_get_coeff_ui(x::Ref{fpPolyRingElem}, 0::Int)::UInt
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
  @ccall libflint.nmod_poly_div(z::Ref{fpPolyRingElem}, x::Ref{fpPolyRingElem}, y::Ref{fpPolyRingElem})::Nothing
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
  @ccall libflint.nmod_poly_divrem(q::Ref{fpPolyRingElem}, r::Ref{fpPolyRingElem}, x::Ref{fpPolyRingElem}, y::Ref{fpPolyRingElem})::Nothing
  return q, r
end

function Base.div(x::fpPolyRingElem, y::fpPolyRingElem)
  check_parent(x,y)
  iszero(y) && throw(DivideError())
  q = parent(x)()
  @ccall libflint.nmod_poly_div(q::Ref{fpPolyRingElem}, x::Ref{fpPolyRingElem}, y::Ref{fpPolyRingElem})::Nothing
  return q
end

################################################################################
#
#  Remainder
#
################################################################################

function rem!(z::fpPolyRingElem, x::fpPolyRingElem, y::fpPolyRingElem)
  @ccall libflint.nmod_poly_rem(z::Ref{fpPolyRingElem}, x::Ref{fpPolyRingElem}, y::Ref{fpPolyRingElem})::Nothing
  return z
end

function rem(x::fpPolyRingElem, y::fpPolyRingElem)
  check_parent(x,y)
  iszero(y) && throw(DivideError())
  z = parent(x)()
  return rem!(z, x, y)
end

################################################################################
#
#  GCD
#
################################################################################

function gcd(x::fpPolyRingElem, y::fpPolyRingElem)
  check_parent(x,y)
  z = parent(x)()
  @ccall libflint.nmod_poly_gcd(z::Ref{fpPolyRingElem}, x::Ref{fpPolyRingElem}, y::Ref{fpPolyRingElem})::Nothing
  return z
end

function gcdx(x::fpPolyRingElem, y::fpPolyRingElem)
  check_parent(x,y)
  g = parent(x)()
  s = parent(x)()
  t = parent(x)()
  @ccall libflint.nmod_poly_xgcd(g::Ref{fpPolyRingElem}, s::Ref{fpPolyRingElem}, t::Ref{fpPolyRingElem}, x::Ref{fpPolyRingElem}, y::Ref{fpPolyRingElem})::Nothing
  return g,s,t
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
  r = @ccall libflint.nmod_poly_resultant(x::Ref{fpPolyRingElem}, y::Ref{fpPolyRingElem})::UInt
  return base_ring(x)(r)
end

################################################################################
#
#  Evaluation
#
################################################################################

function evaluate(x::fpPolyRingElem, y::fpFieldElem)
  base_ring(x) != parent(y) && error("Elements must have same parent")
  z = @ccall libflint.nmod_poly_evaluate_nmod(x::Ref{fpPolyRingElem}, y.data::UInt)::UInt
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
  @ccall libflint.nmod_poly_interpolate_nmod_vec(z::Ref{fpPolyRingElem}, ax::Ptr{UInt}, ay::Ptr{UInt}, length(x)::Int)::Nothing
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
  @ccall libflint.fmpz_poly_set_nmod_poly_unsigned(z::Ref{ZZPolyRingElem}, y::Ref{fpPolyRingElem})::Nothing
  z.parent = R
  return z
end

################################################################################
#
#  Irreducibility
#
################################################################################

function is_irreducible(x::fpPolyRingElem)
  is_constant(x) && return false
  return Bool(@ccall libflint.nmod_poly_is_irreducible(x::Ref{fpPolyRingElem})::Int32)
end

################################################################################
#
#  Squarefree testing
#
################################################################################

function is_squarefree(x::fpPolyRingElem)
  return Bool(@ccall libflint.nmod_poly_is_squarefree(x::Ref{fpPolyRingElem})::Int32)
end

################################################################################
#
#  Square root
#
################################################################################

function sqrt(x::fpPolyRingElem; check::Bool=true)
  R = parent(x)
  s = R()
  flag = Bool(@ccall libflint.nmod_poly_sqrt(s::Ref{fpPolyRingElem}, x::Ref{fpPolyRingElem})::Cint)
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
  flag = Bool(@ccall libflint.nmod_poly_sqrt(s::Ref{fpPolyRingElem}, x::Ref{fpPolyRingElem})::Cint)
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
  flag = Bool(@ccall libflint.nmod_poly_sqrt(s::Ref{fpPolyRingElem}, x::Ref{fpPolyRingElem})::Cint)
  return flag, s
end

################################################################################
#
#  Factorization
#
################################################################################

function factor(x::fpPolyRingElem)
  iszero(x) && throw(ArgumentError("Argument must be non-zero"))
  fac, z = _factor(x)
  return Fac(parent(x)(z), _pretty_sort(fac))
end

function _factor(x::fpPolyRingElem)
  fac = gfp_poly_factor(x.mod_n)
  z = @ccall libflint.nmod_poly_factor(fac::Ref{gfp_poly_factor}, x::Ref{fpPolyRingElem})::UInt
  res = Dict{fpPolyRingElem, Int}()
  for i in 1:fac.num
    f = parent(x)()
    @ccall libflint.nmod_poly_factor_get_poly(f::Ref{fpPolyRingElem}, fac::Ref{gfp_poly_factor}, (i-1)::Int)::Nothing
    e = unsafe_load(fac.exp,i)
    res[f] = e
  end
  return res, base_ring(x)(z)
end

function factor_squarefree(x::fpPolyRingElem)
  iszero(x) && throw(ArgumentError("Argument must be non-zero"))
  return Fac(parent(x)(leading_coefficient(x)), _pretty_sort(_factor_squarefree(x)))
end

function _factor_squarefree(x::fpPolyRingElem)
  fac = gfp_poly_factor(x.mod_n)
  @ccall libflint.nmod_poly_factor_squarefree(fac::Ref{gfp_poly_factor}, x::Ref{fpPolyRingElem})::UInt
  res = Dict{fpPolyRingElem, Int}()
  for i in 1:fac.num
    f = parent(x)()
    @ccall libflint.nmod_poly_factor_get_poly(f::Ref{fpPolyRingElem}, fac::Ref{gfp_poly_factor}, (i-1)::Int)::Nothing
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
  @ccall libflint.nmod_poly_factor_distinct_deg(fac::Ref{gfp_poly_factor}, x::Ref{fpPolyRingElem}, degss::Ptr{Ptr{Int}})::UInt
  res = Dict{Int, fpPolyRingElem}()
  for i in 1:fac.num
    f = parent(x)()
    @ccall libflint.nmod_poly_factor_get_poly(f::Ref{fpPolyRingElem}, fac::Ref{gfp_poly_factor}, (i-1)::Int)::Nothing
    res[degs[i]] = f
  end
  return res
end

# Factor x assuming that all irreducible factors are of degree d
function factor_equal_deg(x::fpPolyRingElem, d::Int)
  if degree(x) == d
    return fpPolyRingElem[x]
  end
  fac = gfp_poly_factor(x.mod_n)
  @ccall libflint.nmod_poly_factor_equal_deg(fac::Ref{gfp_poly_factor}, x::Ref{fpPolyRingElem}, d::Int)::UInt
  res = Vector{fpPolyRingElem}(undef, fac.num)
  for i in 1:fac.num
    f = parent(x)()
    @ccall libflint.nmod_poly_factor_get_poly(f::Ref{fpPolyRingElem}, fac::Ref{gfp_poly_factor}, (i - 1)::Int)::Nothing
    res[i] = f
  end
  return res
end

function roots(a::fpPolyRingElem)
  R = parent(a)
  n = R.n
  fac = nmod_poly_factor(n)
  @ccall libflint.nmod_poly_roots(fac::Ref{nmod_poly_factor}, a::Ref{fpPolyRingElem}, 0::Cint)::UInt
  f = R()
  res = fpFieldElem[]
  for i in 1:fac.num
    @ccall libflint.nmod_poly_factor_get_poly(f::Ref{fpPolyRingElem}, fac::Ref{nmod_poly_factor}, (i - 1)::Int)::Nothing
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
  v = @ccall libflint.nmod_poly_remove(z::Ref{fpPolyRingElem}, p::Ref{fpPolyRingElem})::Int
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
  r = @ccall libflint.fmpz_fdiv_ui(x::Ref{ZZRingElem}, R.n::UInt)::UInt
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
