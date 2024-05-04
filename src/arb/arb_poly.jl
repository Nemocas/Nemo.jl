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

length(x::ArbPolyRingElem) = ccall((:arb_poly_length, libflint), Int,
                                   (Ref{ArbPolyRingElem},), x)

function set_length!(x::ArbPolyRingElem, n::Int)
   ccall((:_arb_poly_set_length, libflint), Nothing,
                                   (Ref{ArbPolyRingElem}, Int), x, n)
   return x
end

degree(x::ArbPolyRingElem) = length(x) - 1

function coeff(a::ArbPolyRingElem, n::Int)
  n < 0 && throw(DomainError(n, "Index must be non-negative"))
  t = parent(a).base_ring()
  ccall((:arb_poly_get_coeff_arb, libflint), Nothing,
              (Ref{ArbFieldElem}, Ref{ArbPolyRingElem}, Int), t, a, n)
  return t
end

zero(a::ArbPolyRing) = a(0)

one(a::ArbPolyRing) = a(1)

function gen(a::ArbPolyRing)
   z = ArbPolyRingElem()
   ccall((:arb_poly_set_coeff_si, libflint), Nothing,
        (Ref{ArbPolyRingElem}, Int, Int), z, 1, 1)
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
   return ccall((:arb_poly_equal, libflint), Bool,
                                      (Ref{ArbPolyRingElem}, Ref{ArbPolyRingElem}), x, y)
end

@doc raw"""
    overlaps(x::ArbPolyRingElem, y::ArbPolyRingElem)

Return `true` if the coefficient balls of $x$ overlap the coefficient balls
of $y$, otherwise return `false`.
"""
function overlaps(x::ArbPolyRingElem, y::ArbPolyRingElem)
   return ccall((:arb_poly_overlaps, libflint), Bool,
                                      (Ref{ArbPolyRingElem}, Ref{ArbPolyRingElem}), x, y)
end

@doc raw"""
    contains(x::ArbPolyRingElem, y::ArbPolyRingElem)

Return `true` if the coefficient balls of $x$ contain the corresponding
coefficient balls of $y$, otherwise return `false`.
"""
function contains(x::ArbPolyRingElem, y::ArbPolyRingElem)
   return ccall((:arb_poly_contains, libflint), Bool,
                                      (Ref{ArbPolyRingElem}, Ref{ArbPolyRingElem}), x, y)
end

@doc raw"""
    contains(x::ArbPolyRingElem, y::ZZPolyRingElem)

Return `true` if the coefficient balls of $x$ contain the corresponding
exact coefficients of $y$, otherwise return `false`.
"""
function contains(x::ArbPolyRingElem, y::ZZPolyRingElem)
   return ccall((:arb_poly_contains_fmpz_poly, libflint), Bool,
                                      (Ref{ArbPolyRingElem}, Ref{ZZPolyRingElem}), x, y)
end

@doc raw"""
    contains(x::ArbPolyRingElem, y::QQPolyRingElem)

Return `true` if the coefficient balls of $x$ contain the corresponding
exact coefficients of $y$, otherwise return `false`.
"""
function contains(x::ArbPolyRingElem, y::QQPolyRingElem)
   return ccall((:arb_poly_contains_fmpq_poly, libflint), Bool,
                                      (Ref{ArbPolyRingElem}, Ref{QQPolyRingElem}), x, y)
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
  unique = ccall((:arb_poly_get_unique_fmpz_poly, libflint), Int,
    (Ref{ZZPolyRingElem}, Ref{ArbPolyRingElem}), z, x)
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
   ccall((:arb_poly_shift_left, libflint), Nothing,
      (Ref{ArbPolyRingElem}, Ref{ArbPolyRingElem}, Int), z, x, len)
   return z
end

function shift_right(x::ArbPolyRingElem, len::Int)
   len < 0 && throw(DomainError(len, "Shift must be non-negative"))
   z = parent(x)()
   ccall((:arb_poly_shift_right, libflint), Nothing,
       (Ref{ArbPolyRingElem}, Ref{ArbPolyRingElem}, Int), z, x, len)
   return z
end

################################################################################
#
#  Unary operations
#
################################################################################

function -(x::ArbPolyRingElem)
  z = parent(x)()
  ccall((:arb_poly_neg, libflint), Nothing, (Ref{ArbPolyRingElem}, Ref{ArbPolyRingElem}), z, x)
  return z
end

################################################################################
#
#  Binary operations
#
################################################################################

function +(x::ArbPolyRingElem, y::ArbPolyRingElem)
  z = parent(x)()
  ccall((:arb_poly_add, libflint), Nothing,
              (Ref{ArbPolyRingElem}, Ref{ArbPolyRingElem}, Ref{ArbPolyRingElem}, Int),
              z, x, y, precision(parent(x)))
  return z
end

function *(x::ArbPolyRingElem, y::ArbPolyRingElem)
  z = parent(x)()
  ccall((:arb_poly_mul, libflint), Nothing,
              (Ref{ArbPolyRingElem}, Ref{ArbPolyRingElem}, Ref{ArbPolyRingElem}, Int),
              z, x, y, precision(parent(x)))
  return z
end

function -(x::ArbPolyRingElem, y::ArbPolyRingElem)
  z = parent(x)()
  ccall((:arb_poly_sub, libflint), Nothing,
              (Ref{ArbPolyRingElem}, Ref{ArbPolyRingElem}, Ref{ArbPolyRingElem}, Int),
              z, x, y, precision(parent(x)))
  return z
end

function ^(x::ArbPolyRingElem, y::Int)
  y < 0 && throw(DomainError(y, "Exponent must be non-negative"))
  z = parent(x)()
  ccall((:arb_poly_pow_ui, libflint), Nothing,
              (Ref{ArbPolyRingElem}, Ref{ArbPolyRingElem}, UInt, Int),
              z, x, y, precision(parent(x)))
  return z
end

###############################################################################
#
#   Ad hoc binary operators
#
###############################################################################

# to avoid method ambiguity errors, include `AbstractFloat, Integer, Rational` in addition to `Real`
for T in [AbstractFloat, Integer, Rational, Real, ZZRingElem, QQFieldElem, ArbFieldElem, ZZPolyRingElem, QQPolyRingElem]
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
for T in [AbstractFloat, Integer, Rational, Real, ZZRingElem, QQFieldElem, ArbFieldElem]
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
   if (ccall((:arb_poly_divrem, libflint), Int,
         (Ref{ArbPolyRingElem}, Ref{ArbPolyRingElem}, Ref{ArbPolyRingElem}, Ref{ArbPolyRingElem}, Int),
               q, r, x, y, precision(parent(x))) == 1)
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
   ccall((:arb_poly_truncate, libflint), Nothing,
                (Ref{ArbPolyRingElem}, Int), z, n)
   return z
end

function mullow(x::ArbPolyRingElem, y::ArbPolyRingElem, n::Int)
   n < 0 && throw(DomainError(n, "Index must be non-negative"))
   z = parent(x)()
   ccall((:arb_poly_mullow, libflint), Nothing,
         (Ref{ArbPolyRingElem}, Ref{ArbPolyRingElem}, Ref{ArbPolyRingElem}, Int, Int),
            z, x, y, n, precision(parent(x)))
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
#   ccall((:arb_poly_reverse, libflint), Nothing,
#                (Ref{ArbPolyRingElem}, Ref{ArbPolyRingElem}, Int), z, x, len)
#   return z
#end

###############################################################################
#
#   Evaluation
#
###############################################################################

function evaluate(x::ArbPolyRingElem, y::ArbFieldElem)
   z = parent(y)()
   ccall((:arb_poly_evaluate, libflint), Nothing,
                (Ref{ArbFieldElem}, Ref{ArbPolyRingElem}, Ref{ArbFieldElem}, Int),
                z, x, y, precision(parent(y)))
   return z
end

function evaluate(x::ArbPolyRingElem, y::AcbFieldElem)
   z = parent(y)()
   ccall((:arb_poly_evaluate_acb, libflint), Nothing,
                (Ref{AcbFieldElem}, Ref{ArbPolyRingElem}, Ref{AcbFieldElem}, Int),
                z, x, y, precision(parent(y)))
   return z
end

evaluate(x::ArbPolyRingElem, y::RingElem) = evaluate(x, base_ring(parent(x))(y))
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
   ccall((:arb_poly_evaluate2, libflint), Nothing,
                (Ref{ArbFieldElem}, Ref{ArbFieldElem}, Ref{ArbPolyRingElem}, Ref{ArbFieldElem}, Int),
                z, w, x, y, precision(parent(y)))
   return z, w
end

function evaluate2(x::ArbPolyRingElem, y::AcbFieldElem)
   z = parent(y)()
   w = parent(y)()
   ccall((:arb_poly_evaluate2_acb, libflint), Nothing,
                (Ref{AcbFieldElem}, Ref{AcbFieldElem}, Ref{ArbPolyRingElem}, Ref{AcbFieldElem}, Int),
                z, w, x, y, precision(parent(y)))
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
   ccall((:arb_poly_compose, libflint), Nothing,
                (Ref{ArbPolyRingElem}, Ref{ArbPolyRingElem}, Ref{ArbPolyRingElem}, Int),
                z, x, y, precision(parent(x)))
   return z
end

###############################################################################
#
#   Derivative and integral
#
###############################################################################

function derivative(x::ArbPolyRingElem)
   z = parent(x)()
   ccall((:arb_poly_derivative, libflint), Nothing,
                (Ref{ArbPolyRingElem}, Ref{ArbPolyRingElem}, Int), z, x, precision(parent(x)))
   return z
end

function integral(x::ArbPolyRingElem)
   z = parent(x)()
   ccall((:arb_poly_integral, libflint), Nothing,
                (Ref{ArbPolyRingElem}, Ref{ArbPolyRingElem}, Int), z, x, precision(parent(x)))
   return z
end

###############################################################################
#
#   Multipoint evaluation and interpolation
#
###############################################################################

function arb_vec(b::Vector{ArbFieldElem})
   v = ccall((:_arb_vec_init, libflint), Ptr{arb_struct}, (Int,), length(b))
   for i=1:length(b)
       ccall((:arb_set, libflint), Nothing, (Ptr{arb_struct}, Ref{ArbFieldElem}),
           v + (i-1)*sizeof(arb_struct), b[i])
   end
   return v
end

function array(R::ArbField, v::Ptr{arb_struct}, n::Int)
   r = Vector{ArbFieldElem}(undef, n)
   for i=1:n
       r[i] = R()
       ccall((:arb_set, libflint), Nothing, (Ref{ArbFieldElem}, Ptr{arb_struct}),
           r[i], v + (i-1)*sizeof(arb_struct))
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
   ccall((:arb_poly_product_roots, libflint), Nothing,
                (Ref{ArbPolyRingElem}, Ptr{arb_struct}, Int, Int), z, tmp, length(b), precision(R))
   arb_vec_clear(tmp, length(b))
   return z
end

function evaluate_iter(x::ArbPolyRingElem, b::Vector{ArbFieldElem})
   return ArbFieldElem[evaluate(x, b[i]) for i=1:length(b)]
end

function evaluate_fast(x::ArbPolyRingElem, b::Vector{ArbFieldElem})
   tmp = arb_vec(b)
   ccall((:arb_poly_evaluate_vec_fast, libflint), Nothing,
                (Ptr{arb_struct}, Ref{ArbPolyRingElem}, Ptr{arb_struct}, Int, Int),
            tmp, x, tmp, length(b), precision(parent(x)))
   res = array(base_ring(parent(x)), tmp, length(b))
   arb_vec_clear(tmp, length(b))
   return res
end

function interpolate_newton(R::ArbPolyRing, xs::Vector{ArbFieldElem}, ys::Vector{ArbFieldElem})
   length(xs) != length(ys) && error()
   z = R()
   xsv = arb_vec(xs)
   ysv = arb_vec(ys)
   ccall((:arb_poly_interpolate_newton, libflint), Nothing,
                (Ref{ArbPolyRingElem}, Ptr{arb_struct}, Ptr{arb_struct}, Int, Int),
            z, xsv, ysv, length(xs), precision(R))
   arb_vec_clear(xsv, length(xs))
   arb_vec_clear(ysv, length(ys))
   return z
end

function interpolate_barycentric(R::ArbPolyRing, xs::Vector{ArbFieldElem}, ys::Vector{ArbFieldElem})
   length(xs) != length(ys) && error()
   z = R()
   xsv = arb_vec(xs)
   ysv = arb_vec(ys)
   ccall((:arb_poly_interpolate_barycentric, libflint), Nothing,
                (Ref{ArbPolyRingElem}, Ptr{arb_struct}, Ptr{arb_struct}, Int, Int),
            z, xsv, ysv, length(xs), precision(R))
   arb_vec_clear(xsv, length(xs))
   arb_vec_clear(ysv, length(ys))
   return z
end

function interpolate_fast(R::ArbPolyRing, xs::Vector{ArbFieldElem}, ys::Vector{ArbFieldElem})
   length(xs) != length(ys) && error()
   z = R()
   xsv = arb_vec(xs)
   ysv = arb_vec(ys)
   ccall((:arb_poly_interpolate_fast, libflint), Nothing,
                (Ref{ArbPolyRingElem}, Ptr{arb_struct}, Ptr{arb_struct}, Int, Int),
            z, xsv, ysv, length(xs), precision(R))
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
      t = ccall((:arb_rad_ptr, libflint), Ptr{mag_struct}, (Ref{ArbFieldElem}, ), z)
      ccall((:arb_poly_root_bound_fujiwara, libflint), Nothing,
            (Ptr{mag_struct}, Ref{ArbPolyRingElem}), t, x)
      s = ccall((:arb_mid_ptr, libflint), Ptr{arf_struct}, (Ref{ArbFieldElem}, ), z)
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

function zero!(z::ArbPolyRingElem)
   ccall((:arb_poly_zero, libflint), Nothing,
                    (Ref{ArbPolyRingElem}, ), z)
   return z
end

function fit!(z::ArbPolyRingElem, n::Int)
   ccall((:arb_poly_fit_length, libflint), Nothing,
                    (Ref{ArbPolyRingElem}, Int), z, n)
   return nothing
end

function setcoeff!(z::ArbPolyRingElem, n::Int, x::ZZRingElem)
   ccall((:arb_poly_set_coeff_fmpz, libflint), Nothing,
                    (Ref{ArbPolyRingElem}, Int, Ref{ZZRingElem}), z, n, x)
   return z
end

function setcoeff!(z::ArbPolyRingElem, n::Int, x::ArbFieldElem)
   ccall((:arb_poly_set_coeff_arb, libflint), Nothing,
                    (Ref{ArbPolyRingElem}, Int, Ref{ArbFieldElem}), z, n, x)
   return z
end

function mul!(z::ArbPolyRingElem, x::ArbPolyRingElem, y::ArbPolyRingElem)
   ccall((:arb_poly_mul, libflint), Nothing,
                (Ref{ArbPolyRingElem}, Ref{ArbPolyRingElem}, Ref{ArbPolyRingElem}, Int),
                    z, x, y, precision(parent(z)))
   return z
end

function addeq!(z::ArbPolyRingElem, x::ArbPolyRingElem)
   ccall((:arb_poly_add, libflint), Nothing,
                (Ref{ArbPolyRingElem}, Ref{ArbPolyRingElem}, Ref{ArbPolyRingElem}, Int),
                    z, z, x, precision(parent(z)))
   return z
end

function add!(z::ArbPolyRingElem, x::ArbPolyRingElem, y::ArbPolyRingElem)
   ccall((:arb_poly_add, libflint), Nothing,
                (Ref{ArbPolyRingElem}, Ref{ArbPolyRingElem}, Ref{ArbPolyRingElem}, Int),
                    z, x, y, precision(parent(z)))
   return z
end

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
