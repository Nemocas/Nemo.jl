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

length(x::RealPolyRingElem) = ccall((:arb_poly_length, libflint), Int,
                            (Ref{RealPolyRingElem},), x)

function set_length!(x::RealPolyRingElem, n::Int)
  ccall((:_arb_poly_set_length, libflint), Nothing,
        (Ref{RealPolyRingElem}, Int), x, n)
  return x
end

degree(x::RealPolyRingElem) = length(x) - 1

function coeff(a::RealPolyRingElem, n::Int)
  n < 0 && throw(DomainError(n, "Index must be non-negative"))
  t = base_ring(parent(a))()
  ccall((:arb_poly_get_coeff_arb, libflint), Nothing,
        (Ref{RealFieldElem}, Ref{RealPolyRingElem}, Int), t, a, n)
  return t
end

zero(a::RealPolyRing) = a(0)

one(a::RealPolyRing) = a(1)

function gen(a::RealPolyRing)
  z = RealPolyRingElem()
  ccall((:arb_poly_set_coeff_si, libflint), Nothing,
        (Ref{RealPolyRingElem}, Int, Int), z, 1, 1)
  z.parent = a
  return z
end

# todo: write a C function for this
function is_gen(a::RealPolyRingElem)
  return isequal(a, gen(parent(a)))
end

#function iszero(a::RealPolyRingElem)
#   return length(a) == 0
#end

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
  return ccall((:arb_poly_equal, libflint), Bool,
               (Ref{RealPolyRingElem}, Ref{RealPolyRingElem}), x, y)
end

@doc raw"""
    overlaps(x::RealPolyRingElem, y::RealPolyRingElem)

Return `true` if the coefficient balls of $x$ overlap the coefficient balls
of $y$, otherwise return `false`.
"""
function overlaps(x::RealPolyRingElem, y::RealPolyRingElem)
  return ccall((:arb_poly_overlaps, libflint), Bool,
               (Ref{RealPolyRingElem}, Ref{RealPolyRingElem}), x, y)
end

@doc raw"""
    contains(x::RealPolyRingElem, y::RealPolyRingElem)

Return `true` if the coefficient balls of $x$ contain the corresponding
coefficient balls of $y$, otherwise return `false`.
"""
function contains(x::RealPolyRingElem, y::RealPolyRingElem)
  return ccall((:arb_poly_contains, libflint), Bool,
               (Ref{RealPolyRingElem}, Ref{RealPolyRingElem}), x, y)
end

@doc raw"""
    contains(x::RealPolyRingElem, y::ZZPolyRingElem)

Return `true` if the coefficient balls of $x$ contain the corresponding
exact coefficients of $y$, otherwise return `false`.
"""
function contains(x::RealPolyRingElem, y::ZZPolyRingElem)
  return ccall((:arb_poly_contains_fmpz_poly, libflint), Bool,
               (Ref{RealPolyRingElem}, Ref{ZZPolyRingElem}), x, y)
end

@doc raw"""
    contains(x::RealPolyRingElem, y::QQPolyRingElem)

Return `true` if the coefficient balls of $x$ contain the corresponding
exact coefficients of $y$, otherwise return `false`.
"""
function contains(x::RealPolyRingElem, y::QQPolyRingElem)
  return ccall((:arb_poly_contains_fmpq_poly, libflint), Bool,
               (Ref{RealPolyRingElem}, Ref{QQPolyRingElem}), x, y)
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
  unique = ccall((:arb_poly_get_unique_fmpz_poly, libflint), Int,
                 (Ref{ZZPolyRingElem}, Ref{RealPolyRingElem}), z, x)
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
  ccall((:arb_poly_shift_left, libflint), Nothing,
        (Ref{RealPolyRingElem}, Ref{RealPolyRingElem}, Int), z, x, len)
  return z
end

function shift_right(x::RealPolyRingElem, len::Int)
  len < 0 && throw(DomainError(len, "Shift must be non-negative"))
  z = parent(x)()
  ccall((:arb_poly_shift_right, libflint), Nothing,
        (Ref{RealPolyRingElem}, Ref{RealPolyRingElem}, Int), z, x, len)
  return z
end

################################################################################
#
#  Unary operations
#
################################################################################

function -(x::RealPolyRingElem)
  z = parent(x)()
  ccall((:arb_poly_neg, libflint), Nothing, (Ref{RealPolyRingElem}, Ref{RealPolyRingElem}), z, x)
  return z
end

################################################################################
#
#  Binary operations
#
################################################################################

function +(x::RealPolyRingElem, y::RealPolyRingElem)
  z = parent(x)()
  ccall((:arb_poly_add, libflint), Nothing,
        (Ref{RealPolyRingElem}, Ref{RealPolyRingElem}, Ref{RealPolyRingElem}, Int),
        z, x, y, precision(Balls))
  return z
end

function *(x::RealPolyRingElem, y::RealPolyRingElem)
  z = parent(x)()
  ccall((:arb_poly_mul, libflint), Nothing,
        (Ref{RealPolyRingElem}, Ref{RealPolyRingElem}, Ref{RealPolyRingElem}, Int),
        z, x, y, precision(Balls))
  return z
end

function -(x::RealPolyRingElem, y::RealPolyRingElem)
  z = parent(x)()
  ccall((:arb_poly_sub, libflint), Nothing,
        (Ref{RealPolyRingElem}, Ref{RealPolyRingElem}, Ref{RealPolyRingElem}, Int),
        z, x, y, precision(Balls))
  return z
end

function ^(x::RealPolyRingElem, y::Int)
  y < 0 && throw(DomainError(y, "Exponent must be non-negative"))
  z = parent(x)()
  ccall((:arb_poly_pow_ui, libflint), Nothing,
        (Ref{RealPolyRingElem}, Ref{RealPolyRingElem}, UInt, Int),
        z, x, y, precision(Balls))
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
  if (ccall((:arb_poly_divrem, libflint), Int,
            (Ref{RealPolyRingElem}, Ref{RealPolyRingElem}, Ref{RealPolyRingElem}, Ref{RealPolyRingElem}, Int),
            q, r, x, y, precision(Balls)) == 1)
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
  ccall((:arb_poly_truncate, libflint), Nothing,
        (Ref{RealPolyRingElem}, Int), z, n)
  return z
end

function mullow(x::RealPolyRingElem, y::RealPolyRingElem, n::Int, prec::Int = precision(Balls))
  n < 0 && throw(DomainError(n, "Index must be non-negative"))
  z = parent(x)()
  ccall((:arb_poly_mullow, libflint), Nothing,
        (Ref{RealPolyRingElem}, Ref{RealPolyRingElem}, Ref{RealPolyRingElem}, Int, Int),
        z, x, y, n, prec)
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
#   ccall((:arb_poly_reverse, libflint), Nothing,
#                (Ref{RealPolyRingElem}, Ref{RealPolyRingElem}, Int), z, x, len)
#   return z
#end

###############################################################################
#
#   Evaluation
#
###############################################################################

function evaluate(x::RealPolyRingElem, y::RealFieldElem, prec::Int = precision(Balls))
  z = parent(y)()
  ccall((:arb_poly_evaluate, libflint), Nothing,
        (Ref{RealFieldElem}, Ref{RealPolyRingElem}, Ref{RealFieldElem}, Int),
        z, x, y, prec)
  return z
end

function evaluate(x::RealPolyRingElem, y::AcbFieldElem, prec::Int = precision(Balls))
  z = parent(y)()
  ccall((:arb_poly_evaluate_acb, libflint), Nothing,
        (Ref{AcbFieldElem}, Ref{RealPolyRingElem}, Ref{AcbFieldElem}, Int),
        z, x, y, prec)
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
  ccall((:arb_poly_evaluate2, libflint), Nothing,
        (Ref{RealFieldElem}, Ref{RealFieldElem}, Ref{RealPolyRingElem}, Ref{RealFieldElem}, Int),
        z, w, x, y, prec)
  return z, w
end

function evaluate2(x::RealPolyRingElem, y::ComplexFieldElem, prec::Int = precision(Balls))
  z = parent(y)()
  w = parent(y)()
  ccall((:arb_poly_evaluate2_acb, libflint), Nothing,
        (Ref{AcbFieldElem}, Ref{AcbFieldElem}, Ref{RealPolyRingElem}, Ref{AcbFieldElem}, Int),
        z, w, x, y, prec)
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
  ccall((:arb_poly_compose, libflint), Nothing,
        (Ref{RealPolyRingElem}, Ref{RealPolyRingElem}, Ref{RealPolyRingElem}, Int),
        z, x, y, prec)
  return z
end

###############################################################################
#
#   Derivative and integral
#
###############################################################################

function derivative(x::RealPolyRingElem, prec::Int = precision(Balls))
  z = parent(x)()
  ccall((:arb_poly_derivative, libflint), Nothing,
        (Ref{RealPolyRingElem}, Ref{RealPolyRingElem}, Int), z, x, prec)
  return z
end

function integral(x::RealPolyRingElem, prec::Int = precision(Balls))
  z = parent(x)()
  ccall((:arb_poly_integral, libflint), Nothing,
        (Ref{RealPolyRingElem}, Ref{RealPolyRingElem}, Int), z, x, prec)
  return z
end

###############################################################################
#
#   Multipoint evaluation and interpolation
#
###############################################################################

function arb_vec(n::Int)
  return ccall((:_arb_vec_init, libflint), Ptr{arb_struct}, (Int,), n)
end

function arb_vec(b::Vector{RealFieldElem})
  v = ccall((:_arb_vec_init, libflint), Ptr{arb_struct}, (Int,), length(b))
  for i=1:length(b)
    ccall((:arb_set, libflint), Nothing, (Ptr{arb_struct}, Ref{RealFieldElem}),
          v + (i-1)*sizeof(arb_struct), b[i])
  end
  return v
end

function array(R::RealField, v::Ptr{arb_struct}, n::Int)
  r = Vector{RealFieldElem}(undef, n)
  for i=1:n
    r[i] = R()
    ccall((:arb_set, libflint), Nothing, (Ref{RealFieldElem}, Ptr{arb_struct}),
          r[i], v + (i-1)*sizeof(arb_struct))
  end
  return r
end

function arb_vec_clear(v::Ptr{arb_struct}, n::Int)
  ccall((:_arb_vec_clear, libflint), Nothing, (Ptr{arb_struct}, Int), v, n)
end

@doc raw"""
    from_roots(R::RealPolyRing, b::Vector{RealFieldElem})

Construct a polynomial in the given polynomial ring from a list of its roots.
"""
function from_roots(R::RealPolyRing, b::Vector{RealFieldElem}, prec::Int = precision(Balls))
  z = R()
  tmp = arb_vec(b)
  ccall((:arb_poly_product_roots, libflint), Nothing,
        (Ref{RealPolyRingElem}, Ptr{arb_struct}, Int, Int), z, tmp, length(b), prec)
  arb_vec_clear(tmp, length(b))
  return z
end

function evaluate_iter(x::RealPolyRingElem, b::Vector{RealFieldElem}, prec::Int = precision(Balls))
  return RealFieldElem[evaluate(x, b[i]) for i=1:length(b)]
end

function evaluate_fast(x::RealPolyRingElem, b::Vector{RealFieldElem}, prec::Int = precision(Balls))
  tmp = arb_vec(b)
  ccall((:arb_poly_evaluate_vec_fast, libflint), Nothing,
        (Ptr{arb_struct}, Ref{RealPolyRingElem}, Ptr{arb_struct}, Int, Int),
        tmp, x, tmp, length(b), prec)
  res = array(base_ring(parent(x)), tmp, length(b))
  arb_vec_clear(tmp, length(b))
  return res
end

function interpolate_newton(R::RealPolyRing, xs::Vector{RealFieldElem}, ys::Vector{RealFieldElem}, prec::Int = precision(Balls))
  length(xs) != length(ys) && error()
  z = R()
  xsv = arb_vec(xs)
  ysv = arb_vec(ys)
  ccall((:arb_poly_interpolate_newton, libflint), Nothing,
        (Ref{RealPolyRingElem}, Ptr{arb_struct}, Ptr{arb_struct}, Int, Int),
        z, xsv, ysv, length(xs), prec)
  arb_vec_clear(xsv, length(xs))
  arb_vec_clear(ysv, length(ys))
  return z
end

function interpolate_barycentric(R::RealPolyRing, xs::Vector{RealFieldElem}, ys::Vector{RealFieldElem}, prec::Int = precision(Balls))
  length(xs) != length(ys) && error()
  z = R()
  xsv = arb_vec(xs)
  ysv = arb_vec(ys)
  ccall((:arb_poly_interpolate_barycentric, libflint), Nothing,
        (Ref{RealPolyRingElem}, Ptr{arb_struct}, Ptr{arb_struct}, Int, Int),
        z, xsv, ysv, length(xs), prec)
  arb_vec_clear(xsv, length(xs))
  arb_vec_clear(ysv, length(ys))
  return z
end

function interpolate_fast(R::RealPolyRing, xs::Vector{RealFieldElem}, ys::Vector{RealFieldElem}, prec::Int = precision(Balls))
  length(xs) != length(ys) && error()
  z = R()
  xsv = arb_vec(xs)
  ysv = arb_vec(ys)
  ccall((:arb_poly_interpolate_fast, libflint), Nothing,
        (Ref{RealPolyRingElem}, Ptr{arb_struct}, Ptr{arb_struct}, Int, Int),
        z, xsv, ysv, length(xs), prec)
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
    t = ccall((:arb_rad_ptr, libflint), Ptr{mag_struct}, (Ref{RealFieldElem}, ), z)
    ccall((:arb_poly_root_bound_fujiwara, libflint), Nothing,
          (Ptr{mag_struct}, Ref{RealPolyRingElem}), t, x)
    s = ccall((:arb_mid_ptr, libflint), Ptr{arf_struct}, (Ref{RealFieldElem}, ), z)
    ccall((:arf_set_mag, libflint), Nothing, (Ptr{arf_struct}, Ptr{mag_struct}), s, t)
    ccall((:arf_set_round, libflint), Nothing,
          (Ptr{arf_struct}, Ptr{arf_struct}, Int, Cint), s, s, p, ARB_RND_CEIL)
    ccall((:mag_zero, libflint), Nothing, (Ptr{mag_struct},), t)
  end
  return z
end

###############################################################################
#
#   Unsafe functions
#
###############################################################################

function zero!(z::RealPolyRingElem)
  ccall((:arb_poly_zero, libflint), Nothing,
        (Ref{RealPolyRingElem}, ), z)
  return z
end

function one!(z::RealPolyRingElem)
  ccall((:arb_poly_one, libflint), Nothing,
        (Ref{RealPolyRingElem}, ), z)
  return z
end

function fit!(z::RealPolyRingElem, n::Int)
  ccall((:arb_poly_fit_length, libflint), Nothing,
        (Ref{RealPolyRingElem}, Int), z, n)
  return nothing
end

function setcoeff!(z::RealPolyRingElem, n::Int, x::ZZRingElem)
  ccall((:arb_poly_set_coeff_fmpz, libflint), Nothing,
        (Ref{RealPolyRingElem}, Int, Ref{ZZRingElem}), z, n, x)
  return z
end

function setcoeff!(z::RealPolyRingElem, n::Int, x::RealFieldElem)
  ccall((:arb_poly_set_coeff_arb, libflint), Nothing,
        (Ref{RealPolyRingElem}, Int, Ref{RealFieldElem}), z, n, x)
  return z
end

function mul!(z::RealPolyRingElem, x::RealPolyRingElem, y::RealPolyRingElem)
  ccall((:arb_poly_mul, libflint), Nothing,
        (Ref{RealPolyRingElem}, Ref{RealPolyRingElem}, Ref{RealPolyRingElem}, Int),
        z, x, y, precision(parent(z)))
  return z
end

function add!(z::RealPolyRingElem, x::RealPolyRingElem, y::RealPolyRingElem)
  ccall((:arb_poly_add, libflint), Nothing,
        (Ref{RealPolyRingElem}, Ref{RealPolyRingElem}, Ref{RealPolyRingElem}, Int),
        z, x, y, precision(parent(z)))
  return z
end

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
