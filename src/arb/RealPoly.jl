###############################################################################
#
#   RealPoly.jl : Polynomials over ArbFieldElem
#
###############################################################################

###############################################################################
#
#   Basic manipulation
#
###############################################################################

parent_type(::Type{RealPolyRingElem}) = RealPolyRing

elem_type(::Type{RealPolyRing}) = RealPolyRingElem

dense_poly_type(::Type{RealFieldElem}) = RealPolyRingElem

length(x::RealPolyRingElem) = @ccall libflint.arb_poly_length(x::Ref{RealPolyRingElem})::Int

function set_length!(x::RealPolyRingElem, n::Int)
  @ccall libflint._arb_poly_set_length(x::Ref{RealPolyRingElem}, n::Int)::Nothing
  return x
end

function coeff(a::RealPolyRingElem, n::Int)
  n < 0 && throw(DomainError(n, "Index must be non-negative"))
  t = base_ring(parent(a))()
  @ccall libflint.arb_poly_get_coeff_arb(t::Ref{RealFieldElem}, a::Ref{RealPolyRingElem}, n::Int)::Nothing
  return t
end

zero(a::RealPolyRing) = a()

one(a::RealPolyRing) = one!(a())

function gen(a::RealPolyRing)
  z = RealPolyRingElem()
  setcoeff!(z, 1, 1)
  z.parent = a
  return z
end

# todo: write a C function for this
function is_gen(a::RealPolyRingElem)
  return isequal(a, gen(parent(a)))
end

#function isone(a::RealPolyRingElem)
#   return strongequal(a, one(parent(a)))
#end

function deepcopy_internal(a::RealPolyRingElem, dict::IdDict)
  z = RealPolyRingElem(a)
  z.parent = parent(a)
  return z
end

###############################################################################
#
#   Similar
#
###############################################################################

function similar(f::PolyRingElem, R::RealField, var::VarName=var(parent(f)); cached::Bool=true)
  z = RealPolyRingElem()
  z.parent = RealPolyRing(R, Symbol(var), cached)
  return z
end

###############################################################################
#
#   polynomial constructor
#
###############################################################################

function polynomial(R::RealField, arr::Vector{T}, var::VarName=:x; cached::Bool=true) where T
  coeffs = map(R, arr)
  coeffs = length(coeffs) == 0 ? RealFieldElem[] : coeffs
  z = RealPolyRingElem(coeffs, precision(Balls))
  z.parent = RealPolyRing(R, Symbol(var), cached)
  return z
end

###############################################################################
#
#   Comparisons
#
###############################################################################

function isequal(x::RealPolyRingElem, y::RealPolyRingElem)
  return @ccall libflint.arb_poly_equal(x::Ref{RealPolyRingElem}, y::Ref{RealPolyRingElem})::Bool
end

@doc raw"""
    overlaps(x::RealPolyRingElem, y::RealPolyRingElem)

Return `true` if the coefficient balls of $x$ overlap the coefficient balls
of $y$, otherwise return `false`.
"""
function overlaps(x::RealPolyRingElem, y::RealPolyRingElem)
  return @ccall libflint.arb_poly_overlaps(x::Ref{RealPolyRingElem}, y::Ref{RealPolyRingElem})::Bool
end

@doc raw"""
    contains(x::RealPolyRingElem, y::RealPolyRingElem)

Return `true` if the coefficient balls of $x$ contain the corresponding
coefficient balls of $y$, otherwise return `false`.
"""
function contains(x::RealPolyRingElem, y::RealPolyRingElem)
  return @ccall libflint.arb_poly_contains(x::Ref{RealPolyRingElem}, y::Ref{RealPolyRingElem})::Bool
end

@doc raw"""
    contains(x::RealPolyRingElem, y::ZZPolyRingElem)

Return `true` if the coefficient balls of $x$ contain the corresponding
exact coefficients of $y$, otherwise return `false`.
"""
function contains(x::RealPolyRingElem, y::ZZPolyRingElem)
  return @ccall libflint.arb_poly_contains_fmpz_poly(x::Ref{RealPolyRingElem}, y::Ref{ZZPolyRingElem})::Bool
end

@doc raw"""
    contains(x::RealPolyRingElem, y::QQPolyRingElem)

Return `true` if the coefficient balls of $x$ contain the corresponding
exact coefficients of $y$, otherwise return `false`.
"""
function contains(x::RealPolyRingElem, y::QQPolyRingElem)
  return @ccall libflint.arb_poly_contains_fmpq_poly(x::Ref{RealPolyRingElem}, y::Ref{QQPolyRingElem})::Bool
end

function ==(x::RealPolyRingElem, y::RealPolyRingElem)
  if length(x) != length(y)
    return false
  end
  for i = 0:degree(x)
    if !(coeff(x, i) == coeff(y, i))
      return false
    end
  end
  return true
end

function !=(x::RealPolyRingElem, y::RealPolyRingElem)
  for i = 0:max(degree(x), degree(y))
    if coeff(x, i) != coeff(y, i)
      return true
    end
  end
  return false
end

@doc raw"""
    unique_integer(x::RealPolyRingElem)

Return a tuple `(t, z)` where $t$ is `true` if there is a unique integer
contained in each of the coefficients of $x$, otherwise sets $t$ to `false`.
In the former case, $z$ is set to the integer polynomial.
"""
function unique_integer(x::RealPolyRingElem)
  z = ZZPolyRing(ZZ, var(parent(x)))()
  unique = @ccall libflint.arb_poly_get_unique_fmpz_poly(z::Ref{ZZPolyRingElem}, x::Ref{RealPolyRingElem})::Int
  return (unique != 0, z)
end

###############################################################################
#
#   Shifting
#
###############################################################################

function shift_left(x::RealPolyRingElem, len::Int)
  len < 0 && throw(DomainError(len, "Shift must be non-negative"))
  z = parent(x)()
  @ccall libflint.arb_poly_shift_left(z::Ref{RealPolyRingElem}, x::Ref{RealPolyRingElem}, len::Int)::Nothing
  return z
end

function shift_right(x::RealPolyRingElem, len::Int)
  len < 0 && throw(DomainError(len, "Shift must be non-negative"))
  z = parent(x)()
  @ccall libflint.arb_poly_shift_right(z::Ref{RealPolyRingElem}, x::Ref{RealPolyRingElem}, len::Int)::Nothing
  return z
end

################################################################################
#
#  Unary operations
#
################################################################################

-(x::RealPolyRingElem) = neg!(parent(x)(), x)

################################################################################
#
#  Binary operations
#
################################################################################

function +(x::RealPolyRingElem, y::RealPolyRingElem)
  z = parent(x)()
  return add!(z, x, y)
end

function -(x::RealPolyRingElem, y::RealPolyRingElem)
  z = parent(x)()
  return sub!(z, x, y)
  return z
end

function *(x::RealPolyRingElem, y::RealPolyRingElem)
  z = parent(x)()
  return mul!(z, x, y)
end

function ^(x::RealPolyRingElem, y::Int)
  y < 0 && throw(DomainError(y, "Exponent must be non-negative"))
  z = parent(x)()
  @ccall libflint.arb_poly_pow_ui(z::Ref{RealPolyRingElem}, x::Ref{RealPolyRingElem}, y::UInt, precision(Balls)::Int)::Nothing
  return z
end

###############################################################################
#
#   Ad hoc binary operators
#
###############################################################################

# to avoid method ambiguity errors, include `AbstractFloat, Integer, Rational` in addition to `Real`
for T in [Union{AbstractFloat, Integer, Rational}, Union{Integer, Rational}, Real, ZZRingElem, QQFieldElem, RealFieldElem, ZZPolyRingElem, QQPolyRingElem]
  @eval begin
    +(x::RealPolyRingElem, y::$T) = x + parent(x)(y)

    +(x::$T, y::RealPolyRingElem) = y + x

    -(x::RealPolyRingElem, y::$T) = x - parent(x)(y)

    -(x::$T, y::RealPolyRingElem) = parent(y)(x) - y

    *(x::RealPolyRingElem, y::$T) = x * parent(x)(y)

    *(x::$T, y::RealPolyRingElem) = y * x
  end
end

###############################################################################
#
#   Scalar division
#
###############################################################################

# to avoid method ambiguity errors, include `AbstractFloat, Integer, Rational` in addition to `Real`
for T in [Union{AbstractFloat, Integer, Rational}, Union{Integer, Rational}, Real, ZZRingElem, QQFieldElem,  RealFieldElem]
  @eval begin
    divexact(x::RealPolyRingElem, y::$T; check::Bool=true) = x * inv(base_ring(parent(x))(y))

    //(x::RealPolyRingElem, y::$T) = divexact(x, y)

    /(x::RealPolyRingElem, y::$T) = divexact(x, y)
  end
end

###############################################################################
#
#   Euclidean division
#
###############################################################################

function Base.divrem(x::RealPolyRingElem, y::RealPolyRingElem)
  iszero(y) && throw(DivideError())
  q = parent(x)()
  r = parent(x)()
  success = @ccall libflint.arb_poly_divrem(q::Ref{RealPolyRingElem}, r::Ref{RealPolyRingElem}, x::Ref{RealPolyRingElem}, y::Ref{RealPolyRingElem}, precision(Balls)::Int)::Int
  if success == 1
    return (q, r)
  else
    throw(DivideError())
  end
end

function mod(x::RealPolyRingElem, y::RealPolyRingElem)
  return divrem(x, y)[2]
end

function divexact(x::RealPolyRingElem, y::RealPolyRingElem; check::Bool=true)
  return divrem(x, y)[1]
end

###############################################################################
#
#   Truncation
#
###############################################################################

function truncate(a::RealPolyRingElem, n::Int)
  n < 0 && throw(DomainError(n, "Index must be non-negative"))
  if length(a) <= n
    return a
  end
  # todo: implement set_trunc in ArbFieldElem
  z = deepcopy(a)
  @ccall libflint.arb_poly_truncate(z::Ref{RealPolyRingElem}, n::Int)::Nothing
  return z
end

function mullow(x::RealPolyRingElem, y::RealPolyRingElem, n::Int, prec::Int = precision(Balls))
  n < 0 && throw(DomainError(n, "Index must be non-negative"))
  z = parent(x)()
  @ccall libflint.arb_poly_mullow(z::Ref{RealPolyRingElem}, x::Ref{RealPolyRingElem}, y::Ref{RealPolyRingElem}, n::Int, prec::Int)::Nothing
  return z
end

###############################################################################
#
#   Reversal
#
###############################################################################

#function reverse(x::RealPolyRingElem, len::Int)
#   len < 0 && throw(DomainError())
#   z = parent(x)()
#   @ccall libflint.arb_poly_reverse(z::Ref{RealPolyRingElem}, x::Ref{RealPolyRingElem}, len::Int)::Nothing
#   return z
#end

###############################################################################
#
#   Evaluation
#
###############################################################################

function evaluate(x::RealPolyRingElem, y::RealFieldElem, prec::Int = precision(Balls))
  z = parent(y)()
  @ccall libflint.arb_poly_evaluate(z::Ref{RealFieldElem}, x::Ref{RealPolyRingElem}, y::Ref{RealFieldElem}, prec::Int)::Nothing
  return z
end

function evaluate(x::RealPolyRingElem, y::AcbFieldElem, prec::Int = precision(Balls))
  z = parent(y)()
  @ccall libflint.arb_poly_evaluate_acb(z::Ref{AcbFieldElem}, x::Ref{RealPolyRingElem}, y::Ref{AcbFieldElem}, prec::Int)::Nothing
  return z
end

evaluate(x::RealPolyRingElem, y::RingElement) = evaluate(x, base_ring(parent(x))(y))
evaluate(x::RealPolyRingElem, y::Integer) = evaluate(x, base_ring(parent(x))(y))
evaluate(x::RealPolyRingElem, y::Rational) = evaluate(x, base_ring(parent(x))(y))
evaluate(x::RealPolyRingElem, y::Float64) = evaluate(x, base_ring(parent(x))(y))
evaluate(x::RealPolyRingElem, y::Any) = evaluate(x, base_ring(parent(x))(y))

@doc raw"""
    evaluate2(x::RealPolyRingElem, y::RingElement)

Return a tuple $p, q$ consisting of the polynomial $x$ evaluated at $y$ and
its derivative evaluated at $y$.
"""
function evaluate2(x::RealPolyRingElem, y::RealFieldElem, prec::Int = precision(Balls))
  z = parent(y)()
  w = parent(y)()
  @ccall libflint.arb_poly_evaluate2(z::Ref{RealFieldElem}, w::Ref{RealFieldElem}, x::Ref{RealPolyRingElem}, y::Ref{RealFieldElem}, prec::Int)::Nothing
  return z, w
end

function evaluate2(x::RealPolyRingElem, y::ComplexFieldElem, prec::Int = precision(Balls))
  z = parent(y)()
  w = parent(y)()
  @ccall libflint.arb_poly_evaluate2_acb(z::Ref{AcbFieldElem}, w::Ref{AcbFieldElem}, x::Ref{RealPolyRingElem}, y::Ref{AcbFieldElem}, prec::Int)::Nothing
  return z, w
end

function evaluate2(x::RealPolyRingElem, y::RingElement)
  return evaluate2(x, base_ring(parent(x))(y))
end

###############################################################################
#
#   Composition
#
###############################################################################

function compose(x::RealPolyRingElem, y::RealPolyRingElem, prec::Int = precision(Balls); inner::Symbol)
  if inner == :first
    x, y = y, x
  end
  @assert inner == :second

  z = parent(x)()
  @ccall libflint.arb_poly_compose(z::Ref{RealPolyRingElem}, x::Ref{RealPolyRingElem}, y::Ref{RealPolyRingElem}, prec::Int)::Nothing
  return z
end

###############################################################################
#
#   Derivative and integral
#
###############################################################################

function derivative(x::RealPolyRingElem, prec::Int = precision(Balls))
  z = parent(x)()
  @ccall libflint.arb_poly_derivative(z::Ref{RealPolyRingElem}, x::Ref{RealPolyRingElem}, prec::Int)::Nothing
  return z
end

function integral(x::RealPolyRingElem, prec::Int = precision(Balls))
  z = parent(x)()
  @ccall libflint.arb_poly_integral(z::Ref{RealPolyRingElem}, x::Ref{RealPolyRingElem}, prec::Int)::Nothing
  return z
end

###############################################################################
#
#   Multipoint evaluation and interpolation
#
###############################################################################

function arb_vec(n::Int)
  return @ccall libflint._arb_vec_init(n::Int)::Ptr{arb_struct}
end

function arb_vec(b::Vector{RealFieldElem})
  v = @ccall libflint._arb_vec_init(length(b)::Int)::Ptr{arb_struct}
  for i in 1:length(b)
    _arb_set(v + (i-1)*sizeof(arb_struct), b[i])
  end
  return v
end

function array(R::RealField, v::Ptr{arb_struct}, n::Int)
  r = Vector{RealFieldElem}(undef, n)
  for i in 1:n
    r[i] = R()
    _arb_set(r[i], v + (i-1)*sizeof(arb_struct))
  end
  return r
end

function arb_vec_clear(v::Ptr{arb_struct}, n::Int)
  @ccall libflint._arb_vec_clear(v::Ptr{arb_struct}, n::Int)::Nothing
end

@doc raw"""
    from_roots(R::RealPolyRing, b::Vector{RealFieldElem})

Construct a polynomial in the given polynomial ring from a list of its roots.
"""
function from_roots(R::RealPolyRing, b::Vector{RealFieldElem}, prec::Int = precision(Balls))
  z = R()
  tmp = arb_vec(b)
  @ccall libflint.arb_poly_product_roots(z::Ref{RealPolyRingElem}, tmp::Ptr{arb_struct}, length(b)::Int, prec::Int)::Nothing
  arb_vec_clear(tmp, length(b))
  return z
end

function evaluate_iter(x::RealPolyRingElem, b::Vector{RealFieldElem}, prec::Int = precision(Balls))
  return RealFieldElem[evaluate(x, b[i]) for i=1:length(b)]
end

function evaluate_fast(x::RealPolyRingElem, b::Vector{RealFieldElem}, prec::Int = precision(Balls))
  tmp = arb_vec(b)
  @ccall libflint.arb_poly_evaluate_vec_fast(tmp::Ptr{arb_struct}, x::Ref{RealPolyRingElem}, tmp::Ptr{arb_struct}, length(b)::Int, prec::Int)::Nothing
  res = array(base_ring(parent(x)), tmp, length(b))
  arb_vec_clear(tmp, length(b))
  return res
end

function interpolate_newton(R::RealPolyRing, xs::Vector{RealFieldElem}, ys::Vector{RealFieldElem}, prec::Int = precision(Balls))
  length(xs) != length(ys) && error()
  z = R()
  xsv = arb_vec(xs)
  ysv = arb_vec(ys)
  @ccall libflint.arb_poly_interpolate_newton(z::Ref{RealPolyRingElem}, xsv::Ptr{arb_struct}, ysv::Ptr{arb_struct}, length(xs)::Int, prec::Int)::Nothing
  arb_vec_clear(xsv, length(xs))
  arb_vec_clear(ysv, length(ys))
  return z
end

function interpolate_barycentric(R::RealPolyRing, xs::Vector{RealFieldElem}, ys::Vector{RealFieldElem}, prec::Int = precision(Balls))
  length(xs) != length(ys) && error()
  z = R()
  xsv = arb_vec(xs)
  ysv = arb_vec(ys)
  @ccall libflint.arb_poly_interpolate_barycentric(z::Ref{RealPolyRingElem}, xsv::Ptr{arb_struct}, ysv::Ptr{arb_struct}, length(xs)::Int, prec::Int)::Nothing
  arb_vec_clear(xsv, length(xs))
  arb_vec_clear(ysv, length(ys))
  return z
end

function interpolate_fast(R::RealPolyRing, xs::Vector{RealFieldElem}, ys::Vector{RealFieldElem}, prec::Int = precision(Balls))
  length(xs) != length(ys) && error()
  z = R()
  xsv = arb_vec(xs)
  ysv = arb_vec(ys)
  @ccall libflint.arb_poly_interpolate_fast(z::Ref{RealPolyRingElem}, xsv::Ptr{arb_struct}, ysv::Ptr{arb_struct}, length(xs)::Int, prec::Int)::Nothing
  arb_vec_clear(xsv, length(xs))
  arb_vec_clear(ysv, length(ys))
  return z
end

# todo: cutoffs for fast algorithm
function interpolate(R::RealPolyRing, xs::Vector{RealFieldElem}, ys::Vector{RealFieldElem}, prec::Int = precision(Balls))
  return interpolate_newton(R, xs, ys, prec)
end

# todo: cutoffs for fast algorithm
function evaluate(x::RealPolyRingElem, b::Vector{RealFieldElem}, prec::Int = precision(Balls))
  return evaluate_iter(x, b, prec)
end

###############################################################################
#
#   Root bounds
#
###############################################################################

@doc raw"""
    roots_upper_bound(x::RealPolyRingElem) -> ArbFieldElem

Returns an upper bound for the absolute value of all complex roots of $x$.
"""
function roots_upper_bound(x::RealPolyRingElem)
  z = base_ring(x)()
  p = precision(Balls)
  GC.@preserve x z begin
    t = _rad_ptr(z)
    @ccall libflint.arb_poly_root_bound_fujiwara(t::Ptr{mag_struct}, x::Ref{RealPolyRingElem})::Nothing
    s = _mid_ptr(z)
    @ccall libflint.arf_set_mag(s::Ptr{arf_struct}, t::Ptr{mag_struct})::Nothing
    @ccall libflint.arf_set_round(s::Ptr{arf_struct}, s::Ptr{arf_struct}, p::Int, ARB_RND_CEIL::Cint)::Nothing
    @ccall libflint.mag_zero(t::Ptr{mag_struct})::Nothing
  end
  return z
end

###############################################################################
#
#   Unsafe functions
#
###############################################################################

function zero!(z::RealPolyRingElemOrPtr)
  @ccall libflint.arb_poly_zero(z::Ref{RealPolyRingElem})::Nothing
  return z
end

function one!(z::RealPolyRingElemOrPtr)
  @ccall libflint.arb_poly_one(z::Ref{RealPolyRingElem})::Nothing
  return z
end

function neg!(z::RealPolyRingElemOrPtr, a::RealPolyRingElemOrPtr)
  @ccall libflint.arb_poly_neg(z::Ref{RealPolyRingElem}, a::Ref{RealPolyRingElem})::Nothing
  return z
end

function fit!(z::RealPolyRingElem, n::Int)
  @ccall libflint.arb_poly_fit_length(z::Ref{RealPolyRingElem}, n::Int)::Nothing
  return nothing
end

function setcoeff!(z::RealPolyRingElem, n::Int, x::RealFieldElem)
  @ccall libflint.arb_poly_set_coeff_arb(z::Ref{RealPolyRingElem}, n::Int, x::Ref{RealFieldElem})::Nothing
  return z
end

function setcoeff!(z::RealPolyRingElem, n::Int, x::Int)
  @ccall libflint.arb_poly_set_coeff_si(z::Ref{RealPolyRingElem}, n::Int, x::Int)::Nothing
  return z
end

function setcoeff!(z::RealPolyRingElem, n::Int, x::ZZRingElem)
  return setcoeff!(z, n, base_ring(z)(x))
end

setcoeff!(z::RealPolyRingElem, n::Int, x::Integer) = setcoeff!(z, n, flintify(x))

#

function add!(z::RealPolyRingElem, x::RealPolyRingElem, y::RealPolyRingElem)
  @ccall libflint.arb_poly_add(z::Ref{RealPolyRingElem}, x::Ref{RealPolyRingElem}, y::Ref{RealPolyRingElem}, precision(Balls)::Int)::Nothing
  return z
end

function add!(z::RealPolyRingElem, x::RealPolyRingElem, y::Int)
  @ccall libflint.arb_poly_add_si(z::Ref{RealPolyRingElem}, x::Ref{RealPolyRingElem}, y::Int, precision(Balls)::Int)::Nothing
  return z
end

add!(z::RealPolyRingElem, x::RealPolyRingElem, y::RealFieldElem) = add!(z, x, parent(z)(y))

add!(z::RealPolyRingElem, x::RealPolyRingElem, y::ZZRingElem) = add!(z, x, parent(z)(y))

add!(z::RealPolyRingElem, x::RealPolyRingElem, y::Integer) = add!(z, x, flintify(y))

add!(z::RealPolyRingElem, x::Union{RealFieldElem,IntegerUnion}, y::RealPolyRingElem) = add!(z, y, x)

#

function sub!(z::RealPolyRingElem, x::RealPolyRingElem, y::RealPolyRingElem)
  @ccall libflint.arb_poly_sub(z::Ref{RealPolyRingElem}, x::Ref{RealPolyRingElem}, y::Ref{RealPolyRingElem}, precision(Balls)::Int)::Nothing
  return z
end

sub!(z::RealPolyRingElem, x::RealPolyRingElem, y::Union{RealFieldElem,IntegerUnion}) = sub!(z, x, parent(z)(y))

sub!(z::RealPolyRingElem, x::Union{RealFieldElem,IntegerUnion}, y::RealPolyRingElem) = sub!(z, parent(z)(x), y)

#

function mul!(z::RealPolyRingElem, x::RealPolyRingElem, y::RealPolyRingElem)
  @ccall libflint.arb_poly_mul(z::Ref{RealPolyRingElem}, x::Ref{RealPolyRingElem}, y::Ref{RealPolyRingElem}, precision(Balls)::Int)::Nothing
  return z
end

function mul!(z::RealPolyRingElem, x::RealPolyRingElem, y::RealFieldElem)
  @ccall libflint.arb_poly_scalar_mul(z::Ref{RealPolyRingElem}, x::Ref{RealPolyRingElem}, y::Ref{RealFieldElem}, precision(Balls)::Int)::Nothing
  return z
end

mul!(z::RealPolyRingElem, x::RealPolyRingElem, y::IntegerUnion) = mul!(z, x, base_ring(z)(y))

mul!(z::RealPolyRingElem, x::Union{RealFieldElem,IntegerUnion}, y::RealPolyRingElem) = mul!(z, y, x)

#

###############################################################################
#
#   Promotions
#
###############################################################################

promote_rule(::Type{RealPolyRingElem}, ::Type{Float64}) = RealPolyRingElem

promote_rule(::Type{RealPolyRingElem}, ::Type{BigFloat}) = RealPolyRingElem

promote_rule(::Type{RealPolyRingElem}, ::Type{ZZRingElem}) = RealPolyRingElem

promote_rule(::Type{RealPolyRingElem}, ::Type{QQFieldElem}) = RealPolyRingElem

promote_rule(::Type{RealPolyRingElem}, ::Type{RealFieldElem}) = RealPolyRingElem

promote_rule(::Type{RealPolyRingElem}, ::Type{ZZPolyRingElem}) = RealPolyRingElem

promote_rule(::Type{RealPolyRingElem}, ::Type{QQPolyRingElem}) = RealPolyRingElem

promote_rule(::Type{RealPolyRingElem}, ::Type{T}) where {T <: Integer} = RealPolyRingElem

promote_rule(::Type{RealPolyRingElem}, ::Type{Rational{T}}) where T <: Union{Int, BigInt} = RealPolyRingElem

################################################################################
#
#  Parent object call overloads
#
################################################################################

function (a::RealPolyRing)()
  z = RealPolyRingElem()
  z.parent = a
  return z
end

for T in [Real, ZZRingElem, QQFieldElem, RealFieldElem]
  @eval begin
    function (a::RealPolyRing)(b::$T)
      z = RealPolyRingElem(base_ring(a)(b), precision(Balls))
      z.parent = a
      return z
    end
  end
end

function (a::RealPolyRing)(b::Vector{RealFieldElem})
  z = RealPolyRingElem(b, precision(Balls))
  z.parent = a
  return z
end

for T in [Real, ZZRingElem, QQFieldElem, RealFieldElem]
  @eval begin
    (a::RealPolyRing)(b::AbstractVector{<:$T}) = a(map(base_ring(a), b))
  end
end

function (a::RealPolyRing)(b::ZZPolyRingElem)
  z = RealPolyRingElem(b, precision(Balls))
  z.parent = a
  return z
end

function (a::RealPolyRing)(b::QQPolyRingElem)
  z = RealPolyRingElem(b, precision(Balls))
  z.parent = a
  return z
end

function (a::RealPolyRing)(b::RealPolyRingElem)
  z = RealPolyRingElem(b, precision(Balls))
  z.parent = a
  return z
end

function (R::RealPolyRing)(p::AbstractAlgebra.Generic.Poly{RealFieldElem})
  return R(p.coeffs)
end
