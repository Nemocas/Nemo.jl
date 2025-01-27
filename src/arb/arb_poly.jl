###############################################################################
#
#   arb_poly.jl : Polynomials over ArbFieldElem
#
###############################################################################

###############################################################################
#
#   Basic manipulation
#
###############################################################################

parent_type(::Type{ArbPolyRingElem}) = ArbPolyRing

elem_type(::Type{ArbPolyRing}) = ArbPolyRingElem

dense_poly_type(::Type{ArbFieldElem}) = ArbPolyRingElem

length(x::ArbPolyRingElem) = x.length

function set_length!(x::ArbPolyRingElem, n::Int)
  @ccall libflint._arb_poly_set_length(x::Ref{ArbPolyRingElem}, n::Int)::Nothing
  return x
end

degree(x::ArbPolyRingElem) = length(x) - 1

function coeff(a::ArbPolyRingElem, n::Int)
  n < 0 && throw(DomainError(n, "Index must be non-negative"))
  t = parent(a).base_ring()
  @ccall libflint.arb_poly_get_coeff_arb(t::Ref{ArbFieldElem}, a::Ref{ArbPolyRingElem}, n::Int)::Nothing
  return t
end

zero(a::ArbPolyRing) = a()

one(a::ArbPolyRing) = one!(a())

function gen(a::ArbPolyRing)
  z = ArbPolyRingElem()
  setcoeff!(z, 1, 1)
  z.parent = a
  return z
end

# todo: write a C function for this
function is_gen(a::ArbPolyRingElem)
  return isequal(a, gen(parent(a)))
end

#function iszero(a::ArbPolyRingElem)
#   return length(a) == 0
#end

#function isone(a::ArbPolyRingElem)
#   return strongequal(a, one(parent(a)))
#end

function deepcopy_internal(a::ArbPolyRingElem, dict::IdDict)
  z = ArbPolyRingElem(a)
  z.parent = parent(a)
  return z
end

###############################################################################
#
#   AbstractString I/O
#
###############################################################################

function show(io::IO, x::ArbPolyRing)
  @show_name(io, x)
  @show_special(io, x)
  print(io, "Univariate Polynomial Ring in ")
  print(io, var(x))
  print(io, " over ")
  show(io, x.base_ring)
end

###############################################################################
#
#   Similar
#
###############################################################################

function similar(f::PolyRingElem, R::ArbField, var::VarName=var(parent(f)); cached::Bool=true)
  z = ArbPolyRingElem()
  z.parent = ArbPolyRing(R, Symbol(var), cached)
  return z
end

###############################################################################
#
#   polynomial constructor
#
###############################################################################

function polynomial(R::ArbField, arr::Vector{T}, var::VarName=:x; cached::Bool=true) where T
  coeffs = map(R, arr)
  coeffs = length(coeffs) == 0 ? ArbFieldElem[] : coeffs
  z = ArbPolyRingElem(coeffs, R.prec)
  z.parent = ArbPolyRing(R, Symbol(var), cached)
  return z
end

###############################################################################
#
#   Comparisons
#
###############################################################################

function isequal(x::ArbPolyRingElem, y::ArbPolyRingElem)
  return @ccall libflint.arb_poly_equal(x::Ref{ArbPolyRingElem}, y::Ref{ArbPolyRingElem})::Bool
end

@doc raw"""
    overlaps(x::ArbPolyRingElem, y::ArbPolyRingElem)

Return `true` if the coefficient balls of $x$ overlap the coefficient balls
of $y$, otherwise return `false`.
"""
function overlaps(x::ArbPolyRingElem, y::ArbPolyRingElem)
  return @ccall libflint.arb_poly_overlaps(x::Ref{ArbPolyRingElem}, y::Ref{ArbPolyRingElem})::Bool
end

@doc raw"""
    contains(x::ArbPolyRingElem, y::ArbPolyRingElem)

Return `true` if the coefficient balls of $x$ contain the corresponding
coefficient balls of $y$, otherwise return `false`.
"""
function contains(x::ArbPolyRingElem, y::ArbPolyRingElem)
  return @ccall libflint.arb_poly_contains(x::Ref{ArbPolyRingElem}, y::Ref{ArbPolyRingElem})::Bool
end

@doc raw"""
    contains(x::ArbPolyRingElem, y::ZZPolyRingElem)

Return `true` if the coefficient balls of $x$ contain the corresponding
exact coefficients of $y$, otherwise return `false`.
"""
function contains(x::ArbPolyRingElem, y::ZZPolyRingElem)
  return @ccall libflint.arb_poly_contains_fmpz_poly(x::Ref{ArbPolyRingElem}, y::Ref{ZZPolyRingElem})::Bool
end

@doc raw"""
    contains(x::ArbPolyRingElem, y::QQPolyRingElem)

Return `true` if the coefficient balls of $x$ contain the corresponding
exact coefficients of $y$, otherwise return `false`.
"""
function contains(x::ArbPolyRingElem, y::QQPolyRingElem)
  return @ccall libflint.arb_poly_contains_fmpq_poly(x::Ref{ArbPolyRingElem}, y::Ref{QQPolyRingElem})::Bool
end

function ==(x::ArbPolyRingElem, y::ArbPolyRingElem)
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

function !=(x::ArbPolyRingElem, y::ArbPolyRingElem)
  for i = 0:max(degree(x), degree(y))
    if coeff(x, i) != coeff(y, i)
      return true
    end
  end
  return false
end

@doc raw"""
    unique_integer(x::ArbPolyRingElem)

Return a tuple `(t, z)` where $t$ is `true` if there is a unique integer
contained in each of the coefficients of $x$, otherwise sets $t$ to `false`.
In the former case, $z$ is set to the integer polynomial.
"""
function unique_integer(x::ArbPolyRingElem)
  z = ZZPolyRing(ZZ, var(parent(x)))()
  unique = @ccall libflint.arb_poly_get_unique_fmpz_poly(z::Ref{ZZPolyRingElem}, x::Ref{ArbPolyRingElem})::Int
  return (unique != 0, z)
end

###############################################################################
#
#   Shifting
#
###############################################################################

function shift_left(x::ArbPolyRingElem, len::Int)
  len < 0 && throw(DomainError(len, "Shift must be non-negative"))
  z = parent(x)()
  @ccall libflint.arb_poly_shift_left(z::Ref{ArbPolyRingElem}, x::Ref{ArbPolyRingElem}, len::Int)::Nothing
  return z
end

function shift_right(x::ArbPolyRingElem, len::Int)
  len < 0 && throw(DomainError(len, "Shift must be non-negative"))
  z = parent(x)()
  @ccall libflint.arb_poly_shift_right(z::Ref{ArbPolyRingElem}, x::Ref{ArbPolyRingElem}, len::Int)::Nothing
  return z
end

################################################################################
#
#  Unary operations
#
################################################################################

-(x::ArbPolyRingElem) = neg!(parent(x)(), x)

################################################################################
#
#  Binary operations
#
################################################################################

function +(x::ArbPolyRingElem, y::ArbPolyRingElem)
  z = parent(x)()
  return add!(z, x, y)
end

function -(x::ArbPolyRingElem, y::ArbPolyRingElem)
  z = parent(x)()
  return sub!(z, x, y)
  return z
end

function *(x::ArbPolyRingElem, y::ArbPolyRingElem)
  z = parent(x)()
  return mul!(z, x, y)
end

function ^(x::ArbPolyRingElem, y::Int)
  y < 0 && throw(DomainError(y, "Exponent must be non-negative"))
  z = parent(x)()
  @ccall libflint.arb_poly_pow_ui(z::Ref{ArbPolyRingElem}, x::Ref{ArbPolyRingElem}, y::UInt, precision(parent(x))::Int)::Nothing
  return z
end

###############################################################################
#
#   Ad hoc binary operators
#
###############################################################################

# to avoid method ambiguity errors, include `AbstractFloat, Integer, Rational` in addition to `Real`
for T in [Union{AbstractFloat, Integer, Rational}, Union{Integer, Rational}, Real, ZZRingElem, QQFieldElem, ArbFieldElem, ZZPolyRingElem, QQPolyRingElem]
  @eval begin
    +(x::ArbPolyRingElem, y::$T) = x + parent(x)(y)

    +(x::$T, y::ArbPolyRingElem) = y + x

    -(x::ArbPolyRingElem, y::$T) = x - parent(x)(y)

    -(x::$T, y::ArbPolyRingElem) = parent(y)(x) - y

    *(x::ArbPolyRingElem, y::$T) = x * parent(x)(y)

    *(x::$T, y::ArbPolyRingElem) = y * x
  end
end

###############################################################################
#
#   Scalar division
#
###############################################################################

# to avoid method ambiguity errors, include `AbstractFloat, Integer, Rational` in addition to `Real`
for T in [Union{AbstractFloat, Integer, Rational}, Union{Integer, Rational}, Real, ZZRingElem, QQFieldElem, ArbFieldElem]
  @eval begin
    divexact(x::ArbPolyRingElem, y::$T; check::Bool=true) = x * inv(base_ring(parent(x))(y))

    //(x::ArbPolyRingElem, y::$T) = divexact(x, y)

    /(x::ArbPolyRingElem, y::$T) = divexact(x, y)
  end
end

###############################################################################
#
#   Euclidean division
#
###############################################################################

function Base.divrem(x::ArbPolyRingElem, y::ArbPolyRingElem)
  iszero(y) && throw(DivideError())
  q = parent(x)()
  r = parent(x)()
  success = @ccall libflint.arb_poly_divrem(q::Ref{ArbPolyRingElem}, r::Ref{ArbPolyRingElem}, x::Ref{ArbPolyRingElem}, y::Ref{ArbPolyRingElem}, precision(parent(x))::Int)::Int
  if success == 1
    return (q, r)
  else
    throw(DivideError())
  end
end

function mod(x::ArbPolyRingElem, y::ArbPolyRingElem)
  return divrem(x, y)[2]
end

function divexact(x::ArbPolyRingElem, y::ArbPolyRingElem; check::Bool=true)
  return divrem(x, y)[1]
end

###############################################################################
#
#   Truncation
#
###############################################################################

function truncate(a::ArbPolyRingElem, n::Int)
  n < 0 && throw(DomainError(n, "Index must be non-negative"))
  if length(a) <= n
    return a
  end
  # todo: implement set_trunc in ArbFieldElem
  z = deepcopy(a)
  @ccall libflint.arb_poly_truncate(z::Ref{ArbPolyRingElem}, n::Int)::Nothing
  return z
end

function mullow(x::ArbPolyRingElem, y::ArbPolyRingElem, n::Int)
  n < 0 && throw(DomainError(n, "Index must be non-negative"))
  z = parent(x)()
  @ccall libflint.arb_poly_mullow(z::Ref{ArbPolyRingElem}, x::Ref{ArbPolyRingElem}, y::Ref{ArbPolyRingElem}, n::Int, precision(parent(x))::Int)::Nothing
  return z
end

###############################################################################
#
#   Reversal
#
###############################################################################

#function reverse(x::ArbPolyRingElem, len::Int)
#   len < 0 && throw(DomainError())
#   z = parent(x)()
#   @ccall libflint.arb_poly_reverse(z::Ref{ArbPolyRingElem}, x::Ref{ArbPolyRingElem}, len::Int)::Nothing
#   return z
#end

###############################################################################
#
#   Evaluation
#
###############################################################################

function evaluate(x::ArbPolyRingElem, y::ArbFieldElem)
  z = parent(y)()
  @ccall libflint.arb_poly_evaluate(z::Ref{ArbFieldElem}, x::Ref{ArbPolyRingElem}, y::Ref{ArbFieldElem}, precision(parent(y))::Int)::Nothing
  return z
end

function evaluate(x::ArbPolyRingElem, y::AcbFieldElem)
  z = parent(y)()
  @ccall libflint.arb_poly_evaluate_acb(z::Ref{AcbFieldElem}, x::Ref{ArbPolyRingElem}, y::Ref{AcbFieldElem}, precision(parent(y))::Int)::Nothing
  return z
end

evaluate(x::ArbPolyRingElem, y::RingElement) = evaluate(x, base_ring(parent(x))(y))
evaluate(x::ArbPolyRingElem, y::Integer) = evaluate(x, base_ring(parent(x))(y))
evaluate(x::ArbPolyRingElem, y::Rational) = evaluate(x, base_ring(parent(x))(y))
evaluate(x::ArbPolyRingElem, y::Float64) = evaluate(x, base_ring(parent(x))(y))
evaluate(x::ArbPolyRingElem, y::Any) = evaluate(x, base_ring(parent(x))(y))

@doc raw"""
    evaluate2(x::ArbPolyRingElem, y::Any)

Return a tuple $p, q$ consisting of the polynomial $x$ evaluated at $y$ and
its derivative evaluated at $y$.
"""
function evaluate2(x::ArbPolyRingElem, y::ArbFieldElem)
  z = parent(y)()
  w = parent(y)()
  @ccall libflint.arb_poly_evaluate2(z::Ref{ArbFieldElem}, w::Ref{ArbFieldElem}, x::Ref{ArbPolyRingElem}, y::Ref{ArbFieldElem}, precision(parent(y))::Int)::Nothing
  return z, w
end

function evaluate2(x::ArbPolyRingElem, y::AcbFieldElem)
  z = parent(y)()
  w = parent(y)()
  @ccall libflint.arb_poly_evaluate2_acb(z::Ref{AcbFieldElem}, w::Ref{AcbFieldElem}, x::Ref{ArbPolyRingElem}, y::Ref{AcbFieldElem}, precision(parent(y))::Int)::Nothing
  return z, w
end

evaluate2(x::ArbPolyRingElem, y::Any) = evaluate2(x, base_ring(parent(x))(y))

###############################################################################
#
#   Composition
#
###############################################################################

function AbstractAlgebra._compose_right(x::ArbPolyRingElem, y::ArbPolyRingElem)
  z = parent(x)()
  @ccall libflint.arb_poly_compose(z::Ref{ArbPolyRingElem}, x::Ref{ArbPolyRingElem}, y::Ref{ArbPolyRingElem}, precision(parent(x))::Int)::Nothing
  return z
end

###############################################################################
#
#   Derivative and integral
#
###############################################################################

function derivative(x::ArbPolyRingElem)
  z = parent(x)()
  @ccall libflint.arb_poly_derivative(z::Ref{ArbPolyRingElem}, x::Ref{ArbPolyRingElem}, precision(parent(x))::Int)::Nothing
  return z
end

function integral(x::ArbPolyRingElem)
  z = parent(x)()
  @ccall libflint.arb_poly_integral(z::Ref{ArbPolyRingElem}, x::Ref{ArbPolyRingElem}, precision(parent(x))::Int)::Nothing
  return z
end

###############################################################################
#
#   Multipoint evaluation and interpolation
#
###############################################################################

function arb_vec(b::Vector{ArbFieldElem})
  v = @ccall libflint._arb_vec_init(length(b)::Int)::Ptr{arb_struct}
  for i in 1:length(b)
    _arb_set(v + (i-1)*sizeof(arb_struct), b[i])
  end
  return v
end

function array(R::ArbField, v::Ptr{arb_struct}, n::Int)
  r = Vector{ArbFieldElem}(undef, n)
  for i in 1:n
    r[i] = R()
    _arb_set(r[i], v + (i-1)*sizeof(arb_struct))
  end
  return r
end

@doc raw"""
    from_roots(R::ArbPolyRing, b::Vector{ArbFieldElem})

Construct a polynomial in the given polynomial ring from a list of its roots.
"""
function from_roots(R::ArbPolyRing, b::Vector{ArbFieldElem})
  z = R()
  tmp = arb_vec(b)
  @ccall libflint.arb_poly_product_roots(z::Ref{ArbPolyRingElem}, tmp::Ptr{arb_struct}, length(b)::Int, precision(R)::Int)::Nothing
  arb_vec_clear(tmp, length(b))
  return z
end

function evaluate_iter(x::ArbPolyRingElem, b::Vector{ArbFieldElem})
  return ArbFieldElem[evaluate(x, b[i]) for i=1:length(b)]
end

function evaluate_fast(x::ArbPolyRingElem, b::Vector{ArbFieldElem})
  tmp = arb_vec(b)
  @ccall libflint.arb_poly_evaluate_vec_fast(tmp::Ptr{arb_struct}, x::Ref{ArbPolyRingElem}, tmp::Ptr{arb_struct}, length(b)::Int, precision(parent(x))::Int)::Nothing
  res = array(base_ring(parent(x)), tmp, length(b))
  arb_vec_clear(tmp, length(b))
  return res
end

function interpolate_newton(R::ArbPolyRing, xs::Vector{ArbFieldElem}, ys::Vector{ArbFieldElem})
  length(xs) != length(ys) && error()
  z = R()
  xsv = arb_vec(xs)
  ysv = arb_vec(ys)
  @ccall libflint.arb_poly_interpolate_newton(z::Ref{ArbPolyRingElem}, xsv::Ptr{arb_struct}, ysv::Ptr{arb_struct}, length(xs)::Int, precision(R)::Int)::Nothing
  arb_vec_clear(xsv, length(xs))
  arb_vec_clear(ysv, length(ys))
  return z
end

function interpolate_barycentric(R::ArbPolyRing, xs::Vector{ArbFieldElem}, ys::Vector{ArbFieldElem})
  length(xs) != length(ys) && error()
  z = R()
  xsv = arb_vec(xs)
  ysv = arb_vec(ys)
  @ccall libflint.arb_poly_interpolate_barycentric(z::Ref{ArbPolyRingElem}, xsv::Ptr{arb_struct}, ysv::Ptr{arb_struct}, length(xs)::Int, precision(R)::Int)::Nothing
  arb_vec_clear(xsv, length(xs))
  arb_vec_clear(ysv, length(ys))
  return z
end

function interpolate_fast(R::ArbPolyRing, xs::Vector{ArbFieldElem}, ys::Vector{ArbFieldElem})
  length(xs) != length(ys) && error()
  z = R()
  xsv = arb_vec(xs)
  ysv = arb_vec(ys)
  @ccall libflint.arb_poly_interpolate_fast(z::Ref{ArbPolyRingElem}, xsv::Ptr{arb_struct}, ysv::Ptr{arb_struct}, length(xs)::Int, precision(R)::Int)::Nothing
  arb_vec_clear(xsv, length(xs))
  arb_vec_clear(ysv, length(ys))
  return z
end

# todo: cutoffs for fast algorithm
function interpolate(R::ArbPolyRing, xs::Vector{ArbFieldElem}, ys::Vector{ArbFieldElem})
  return interpolate_newton(R, xs, ys)
end

# todo: cutoffs for fast algorithm
function evaluate(x::ArbPolyRingElem, b::Vector{ArbFieldElem})
  return evaluate_iter(x, b)
end

###############################################################################
#
#   Root bounds
#
###############################################################################

@doc raw"""
    roots_upper_bound(x::ArbPolyRingElem) -> ArbFieldElem

Returns an upper bound for the absolute value of all complex roots of $x$.
"""
function roots_upper_bound(x::ArbPolyRingElem)
  z = base_ring(x)()
  p = precision(base_ring(x))
  GC.@preserve x z begin
    t = _rad_ptr(z)
    @ccall libflint.arb_poly_root_bound_fujiwara(t::Ptr{mag_struct}, x::Ref{ArbPolyRingElem})::Nothing
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

function zero!(z::ArbPolyRingElemOrPtr)
  @ccall libflint.arb_poly_zero(z::Ref{ArbPolyRingElem})::Nothing
  return z
end

function one!(z::ArbPolyRingElemOrPtr)
  @ccall libflint.arb_poly_one(z::Ref{ArbPolyRingElem})::Nothing
  return z
end

function neg!(z::ArbPolyRingElemOrPtr, a::ArbPolyRingElemOrPtr)
  @ccall libflint.arb_poly_neg(z::Ref{ArbPolyRingElem}, a::Ref{ArbPolyRingElem})::Nothing
  return z
end

function fit!(z::ArbPolyRingElem, n::Int)
  @ccall libflint.arb_poly_fit_length(z::Ref{ArbPolyRingElem}, n::Int)::Nothing
  return nothing
end

function setcoeff!(z::ArbPolyRingElem, n::Int, x::ArbFieldElem)
  @ccall libflint.arb_poly_set_coeff_arb(z::Ref{ArbPolyRingElem}, n::Int, x::Ref{ArbFieldElem})::Nothing
  return z
end

function setcoeff!(z::ArbPolyRingElem, n::Int, x::Int)
  @ccall libflint.arb_poly_set_coeff_si(z::Ref{ArbPolyRingElem}, n::Int, x::Int)::Nothing
  return z
end

function setcoeff!(z::ArbPolyRingElem, n::Int, x::ZZRingElem)
  return setcoeff!(z, n, base_ring(z)(x))
end

setcoeff!(z::ArbPolyRingElem, n::Int, x::Integer) = setcoeff!(z, n, flintify(x))

#

function add!(z::ArbPolyRingElem, x::ArbPolyRingElem, y::ArbPolyRingElem)
  @ccall libflint.arb_poly_add(z::Ref{ArbPolyRingElem}, x::Ref{ArbPolyRingElem}, y::Ref{ArbPolyRingElem}, precision(parent(z))::Int)::Nothing
  return z
end

function add!(z::ArbPolyRingElem, x::ArbPolyRingElem, y::Int)
  @ccall libflint.arb_poly_add_si(z::Ref{ArbPolyRingElem}, x::Ref{ArbPolyRingElem}, y::Int, precision(parent(z))::Int)::Nothing
  return z
end

add!(z::ArbPolyRingElem, x::ArbPolyRingElem, y::ArbFieldElem) = add!(z, x, parent(z)(y))

add!(z::ArbPolyRingElem, x::ArbPolyRingElem, y::ZZRingElem) = add!(z, x, parent(z)(y))

add!(z::ArbPolyRingElem, x::ArbPolyRingElem, y::Integer) = add!(z, x, flintify(y))

add!(z::ArbPolyRingElem, x::Union{ArbFieldElem,IntegerUnion}, y::ArbPolyRingElem) = add!(z, y, x)

#

function sub!(z::ArbPolyRingElem, x::ArbPolyRingElem, y::ArbPolyRingElem)
  @ccall libflint.arb_poly_sub(z::Ref{ArbPolyRingElem}, x::Ref{ArbPolyRingElem}, y::Ref{ArbPolyRingElem}, precision(parent(z))::Int)::Nothing
  return z
end

sub!(z::ArbPolyRingElem, x::ArbPolyRingElem, y::Union{ArbFieldElem,IntegerUnion}) = sub!(z, x, parent(z)(y))

sub!(z::ArbPolyRingElem, x::Union{ArbFieldElem,IntegerUnion}, y::ArbPolyRingElem) = sub!(z, parent(z)(x), y)

#

function mul!(z::ArbPolyRingElem, x::ArbPolyRingElem, y::ArbPolyRingElem)
  @ccall libflint.arb_poly_mul(z::Ref{ArbPolyRingElem}, x::Ref{ArbPolyRingElem}, y::Ref{ArbPolyRingElem}, precision(parent(z))::Int)::Nothing
  return z
end

function mul!(z::ArbPolyRingElem, x::ArbPolyRingElem, y::ArbFieldElem)
  @ccall libflint.arb_poly_scalar_mul(z::Ref{ArbPolyRingElem}, x::Ref{ArbPolyRingElem}, y::Ref{ArbFieldElem}, precision(parent(z))::Int)::Nothing
  return z
end

mul!(z::ArbPolyRingElem, x::ArbPolyRingElem, y::IntegerUnion) = mul!(z, x, base_ring(z)(y))

mul!(z::ArbPolyRingElem, x::Union{ArbFieldElem,IntegerUnion}, y::ArbPolyRingElem) = mul!(z, y, x)

#

###############################################################################
#
#   Promotions
#
###############################################################################

promote_rule(::Type{ArbPolyRingElem}, ::Type{Float64}) = ArbPolyRingElem

promote_rule(::Type{ArbPolyRingElem}, ::Type{BigFloat}) = ArbPolyRingElem

promote_rule(::Type{ArbPolyRingElem}, ::Type{ZZRingElem}) = ArbPolyRingElem

promote_rule(::Type{ArbPolyRingElem}, ::Type{QQFieldElem}) = ArbPolyRingElem

promote_rule(::Type{ArbPolyRingElem}, ::Type{ArbFieldElem}) = ArbPolyRingElem

promote_rule(::Type{ArbPolyRingElem}, ::Type{ZZPolyRingElem}) = ArbPolyRingElem

promote_rule(::Type{ArbPolyRingElem}, ::Type{QQPolyRingElem}) = ArbPolyRingElem

promote_rule(::Type{ArbPolyRingElem}, ::Type{T}) where {T <: Integer} = ArbPolyRingElem

promote_rule(::Type{ArbPolyRingElem}, ::Type{Rational{T}}) where T <: Union{Int, BigInt} = ArbPolyRingElem

################################################################################
#
#  Parent object call overloads
#
################################################################################

function (a::ArbPolyRing)()
  z = ArbPolyRingElem()
  z.parent = a
  return z
end

for T in [Real, ZZRingElem, QQFieldElem, ArbFieldElem]
  @eval begin
    function (a::ArbPolyRing)(b::$T)
      z = ArbPolyRingElem(base_ring(a)(b), a.base_ring.prec)
      z.parent = a
      return z
    end
  end
end

function (a::ArbPolyRing)(b::Vector{ArbFieldElem})
  z = ArbPolyRingElem(b, a.base_ring.prec)
  z.parent = a
  return z
end

for T in [Real, ZZRingElem, QQFieldElem, ArbFieldElem]
  @eval begin
    (a::ArbPolyRing)(b::AbstractVector{<:$T}) = a(map(base_ring(a), b))
  end
end

function (a::ArbPolyRing)(b::ZZPolyRingElem)
  z = ArbPolyRingElem(b, a.base_ring.prec)
  z.parent = a
  return z
end

function (a::ArbPolyRing)(b::QQPolyRingElem)
  z = ArbPolyRingElem(b, a.base_ring.prec)
  z.parent = a
  return z
end

function (a::ArbPolyRing)(b::ArbPolyRingElem)
  z = ArbPolyRingElem(b, a.base_ring.prec)
  z.parent = a
  return z
end

function (R::ArbPolyRing)(p::AbstractAlgebra.Generic.Poly{ArbFieldElem})
  return R(p.coeffs)
end
