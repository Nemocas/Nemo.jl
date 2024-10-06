###############################################################################
#
#   ComplexPoly.jl : Polynomials over AcbFieldElem
#
###############################################################################

###############################################################################
#
#   Basic manipulation
#
###############################################################################

parent_type(::Type{ComplexPolyRingElem}) = ComplexPolyRing

elem_type(::Type{ComplexPolyRing}) = ComplexPolyRingElem

dense_poly_type(::Type{ComplexFieldElem}) = ComplexPolyRingElem

length(x::ComplexPolyRingElem) = ccall((:acb_poly_length, libflint), Int,
                               (Ref{ComplexPolyRingElem},), x)

function set_length!(x::ComplexPolyRingElem, n::Int)
  ccall((:_acb_poly_set_length, libflint), Nothing,
        (Ref{ComplexPolyRingElem}, Int), x, n)
  return x
end

degree(x::ComplexPolyRingElem) = length(x) - 1

function coeff(a::ComplexPolyRingElem, n::Int)
  n < 0 && throw(DomainError(n, "Index must be non-negative"))
  t = ComplexFieldElem()
  ccall((:acb_poly_get_coeff_acb, libflint), Nothing,
        (Ref{ComplexFieldElem}, Ref{ComplexPolyRingElem}, Int), t, a, n)
  return t
end

zero(a::ComplexPolyRing) = a(0)

one(a::ComplexPolyRing) = a(1)

function gen(a::ComplexPolyRing)
  z = ComplexPolyRingElem()
  ccall((:acb_poly_set_coeff_si, libflint), Nothing,
        (Ref{ComplexPolyRingElem}, Int, Int), z, 1, 1)
  z.parent = a
  return z
end

# todo: write a C function for this
function is_gen(a::ComplexPolyRingElem)
  return isequal(a, gen(parent(a)))
end

#function iszero(a::ComplexPolyRingElem)
#   return length(a) == 0
#end

#function isone(a::ComplexPolyRingElem)
#   return isequal(a, one(parent(a)))
#end

function deepcopy_internal(a::ComplexPolyRingElem, dict::IdDict)
  z = ComplexPolyRingElem(a)
  z.parent = parent(a)
  return z
end

###############################################################################
#
#   AbstractString I/O
#
###############################################################################

function Base.show(io::IO, a::ComplexPolyRingElem)
  print(io, AbstractAlgebra.obj_to_string(a, context = io))
end

###############################################################################
#
#   Similar
#
###############################################################################

function similar(f::PolyRingElem, R::ComplexField, var::VarName=var(parent(f)); cached::Bool=true)
  z = ComplexPolyRingElem()
  z.parent = ComplexPolyRing(R, Symbol(var), cached)
  return z
end

###############################################################################
#
#   polynomial constructor
#
###############################################################################

function polynomial(R::ComplexField, arr::Vector{T}, var::VarName=:x; cached::Bool=true) where T
  coeffs = map(R, arr)
  coeffs = length(coeffs) == 0 ? ComplexFieldElem[] : coeffs
  z = ComplexPolyRingElem(coeffs, R.prec)
  z.parent = ComplexPolyRing(R, Symbol(var), cached)
  return z
end

###############################################################################
#
#   Comparisons
#
###############################################################################

function isequal(x::ComplexPolyRingElem, y::ComplexPolyRingElem)
  return ccall((:acb_poly_equal, libflint), Bool,
               (Ref{ComplexPolyRingElem}, Ref{ComplexPolyRingElem}), x, y)
end

@doc raw"""
    overlaps(x::ComplexPolyRingElem, y::ComplexPolyRingElem)

Return `true` if the coefficient boxes of $x$ overlap the coefficient boxes
of $y$, otherwise return `false`.
"""
function overlaps(x::ComplexPolyRingElem, y::ComplexPolyRingElem)
  return ccall((:acb_poly_overlaps, libflint), Bool,
               (Ref{ComplexPolyRingElem}, Ref{ComplexPolyRingElem}), x, y)
end

@doc raw"""
    contains(x::ComplexPolyRingElem, y::ComplexPolyRingElem)

Return `true` if the coefficient boxes of $x$ contain the corresponding
coefficient boxes of $y$, otherwise return `false`.
"""
function contains(x::ComplexPolyRingElem, y::ComplexPolyRingElem)
  return ccall((:acb_poly_contains, libflint), Bool,
               (Ref{ComplexPolyRingElem}, Ref{ComplexPolyRingElem}), x, y)
end

@doc raw"""
    contains(x::ComplexPolyRingElem, y::ZZPolyRingElem)

Return `true` if the coefficient boxes of $x$ contain the corresponding
exact coefficients of $y$, otherwise return `false`.
"""
function contains(x::ComplexPolyRingElem, y::ZZPolyRingElem)
  return ccall((:acb_poly_contains_fmpz_poly, libflint), Bool,
               (Ref{ComplexPolyRingElem}, Ref{ZZPolyRingElem}), x, y)
end

@doc raw"""
    contains(x::ComplexPolyRingElem, y::QQPolyRingElem)

Return `true` if the coefficient boxes of $x$ contain the corresponding
exact coefficients of $y$, otherwise return `false`.
"""
function contains(x::ComplexPolyRingElem, y::QQPolyRingElem)
  return ccall((:acb_poly_contains_fmpq_poly, libflint), Bool,
               (Ref{ComplexPolyRingElem}, Ref{QQPolyRingElem}), x, y)
end

function ==(x::ComplexPolyRingElem, y::ComplexPolyRingElem)
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

function !=(x::ComplexPolyRingElem, y::ComplexPolyRingElem)
  for i = 0:max(degree(x), degree(y))
    if coeff(x, i) != coeff(y, i)
      return true
    end
  end
  return false
end

@doc raw"""
    unique_integer(x::ComplexPolyRingElem)

Return a tuple `(t, z)` where $t$ is `true` if there is a unique integer
contained in the (constant) polynomial $x$, along with that integer $z$
in case it is, otherwise sets $t$ to `false`.
"""
function unique_integer(x::ComplexPolyRingElem)
  z = ZZPolyRing(ZZ, var(parent(x)))()
  unique = ccall((:acb_poly_get_unique_fmpz_poly, libflint), Int,
                 (Ref{ZZPolyRingElem}, Ref{ComplexPolyRingElem}), z, x)
  return (unique != 0, z)
end

function isreal(x::ComplexPolyRingElem)
  return ccall((:acb_poly_is_real, libflint), Cint, (Ref{ComplexPolyRingElem}, ), x) != 0
end

###############################################################################
#
#   Shifting
#
###############################################################################

function shift_left(x::ComplexPolyRingElem, len::Int)
  len < 0 && throw(DomainError(len, "Shift must be non-negative"))
  z = parent(x)()
  ccall((:acb_poly_shift_left, libflint), Nothing,
        (Ref{ComplexPolyRingElem}, Ref{ComplexPolyRingElem}, Int), z, x, len)
  return z
end

function shift_right(x::ComplexPolyRingElem, len::Int)
  len < 0 && throw(DomainError(len, "Shift must be non-negative"))
  z = parent(x)()
  ccall((:acb_poly_shift_right, libflint), Nothing,
        (Ref{ComplexPolyRingElem}, Ref{ComplexPolyRingElem}, Int), z, x, len)
  return z
end

################################################################################
#
#  Unary operations
#
################################################################################

function -(x::ComplexPolyRingElem)
  z = parent(x)()
  ccall((:acb_poly_neg, libflint), Nothing, (Ref{ComplexPolyRingElem}, Ref{ComplexPolyRingElem}), z, x)
  return z
end

################################################################################
#
#  Binary operations
#
################################################################################

function +(x::ComplexPolyRingElem, y::ComplexPolyRingElem)
  z = parent(x)()
  ccall((:acb_poly_add, libflint), Nothing,
        (Ref{ComplexPolyRingElem}, Ref{ComplexPolyRingElem}, Ref{ComplexPolyRingElem}, Int),
        z, x, y, precision(Balls))
  return z
end

function *(x::ComplexPolyRingElem, y::ComplexPolyRingElem)
  z = parent(x)()
  ccall((:acb_poly_mul, libflint), Nothing,
        (Ref{ComplexPolyRingElem}, Ref{ComplexPolyRingElem}, Ref{ComplexPolyRingElem}, Int),
        z, x, y, precision(Balls))
  return z
end

function -(x::ComplexPolyRingElem, y::ComplexPolyRingElem)
  z = parent(x)()
  ccall((:acb_poly_sub, libflint), Nothing,
        (Ref{ComplexPolyRingElem}, Ref{ComplexPolyRingElem}, Ref{ComplexPolyRingElem}, Int),
        z, x, y, precision(Balls))
  return z
end

function ^(x::ComplexPolyRingElem, y::Int)
  y < 0 && throw(DomainError(y, "Exponent must be non-negative"))
  z = parent(x)()
  ccall((:acb_poly_pow_ui, libflint), Nothing,
        (Ref{ComplexPolyRingElem}, Ref{ComplexPolyRingElem}, UInt, Int),
        z, x, y, precision(Balls))
  return z
end

###############################################################################
#
#   Ad hoc binary operators
#
###############################################################################

# to avoid method ambiguity errors, include `AbstractFloat, Integer, Rational` in addition to `Real`
for T in [Union{AbstractFloat, Integer, Rational}, Union{Integer, Rational}, Real, Complex, ZZRingElem, QQFieldElem, RealFieldElem, ComplexFieldElem, ZZPolyRingElem, QQPolyRingElem]
  @eval begin
    +(x::ComplexPolyRingElem, y::$T) = x + parent(x)(y)

    +(x::$T, y::ComplexPolyRingElem) = y + x

    -(x::ComplexPolyRingElem, y::$T) = x - parent(x)(y)

    -(x::$T, y::ComplexPolyRingElem) = parent(y)(x) - y

    *(x::ComplexPolyRingElem, y::$T) = x * parent(x)(y)

    *(x::$T, y::ComplexPolyRingElem) = y * x
  end
end

###############################################################################
#
#   Scalar division
#
###############################################################################

# to avoid method ambiguity errors, include `AbstractFloat, Integer, Rational` in addition to `Real`
for T in [Union{AbstractFloat, Integer, Rational}, Union{Integer, Rational}, Real, Complex, ZZRingElem, QQFieldElem, RealFieldElem, ComplexFieldElem]
  @eval begin
    divexact(x::ComplexPolyRingElem, y::$T; check::Bool=true) = x * inv(base_ring(parent(x))(y))

    //(x::ComplexPolyRingElem, y::$T) = divexact(x, y)

    /(x::ComplexPolyRingElem, y::$T) = divexact(x, y)
  end
end

###############################################################################
#
#   Euclidean division
#
###############################################################################

function Base.divrem(x::ComplexPolyRingElem, y::ComplexPolyRingElem)
  iszero(y) && throw(DivideError())
  q = parent(x)()
  r = parent(x)()
  if (ccall((:acb_poly_divrem, libflint), Int,
            (Ref{ComplexPolyRingElem}, Ref{ComplexPolyRingElem}, Ref{ComplexPolyRingElem}, Ref{ComplexPolyRingElem}, Int),
            q, r, x, y, precision(Balls)) == 1)
    return (q, r)
  else
    throw(DivideError())
  end
end

function mod(x::ComplexPolyRingElem, y::ComplexPolyRingElem)
  return divrem(x, y)[2]
end

function divexact(x::ComplexPolyRingElem, y::ComplexPolyRingElem; check::Bool=true)
  return divrem(x, y)[1]
end

###############################################################################
#
#   Truncation
#
###############################################################################

function truncate(a::ComplexPolyRingElem, n::Int)
  n < 0 && throw(DomainError(n, "Index must be non-negative"))
  if length(a) <= n
    return a
  end
  # todo: implement set_trunc in ArbFieldElem
  z = deepcopy(a)
  ccall((:acb_poly_truncate, libflint), Nothing,
        (Ref{ComplexPolyRingElem}, Int), z, n)
  return z
end

function mullow(x::ComplexPolyRingElem, y::ComplexPolyRingElem, n::Int, prec::Int = precision(Balls))
  n < 0 && throw(DomainError(n, "Index must be non-negative"))
  z = parent(x)()
  ccall((:acb_poly_mullow, libflint), Nothing,
        (Ref{ComplexPolyRingElem}, Ref{ComplexPolyRingElem}, Ref{ComplexPolyRingElem}, Int, Int),
        z, x, y, n, prec)
  return z
end

###############################################################################
#
#   Reversal
#
###############################################################################

#function reverse(x::ComplexPolyRingElem, len::Int)
#   len < 0 && throw(DomainError())
#   z = parent(x)()
#   ccall((:acb_poly_reverse, libflint), Nothing,
#                (Ref{ComplexPolyRingElem}, Ref{ComplexPolyRingElem}, Int), z, x, len)
#   return z
#end

###############################################################################
#
#   Evaluation
#
###############################################################################

function evaluate(x::ComplexPolyRingElem, y::ComplexFieldElem, prec::Int = precision(Balls))
  z = parent(y)()
  ccall((:acb_poly_evaluate, libflint), Nothing,
        (Ref{ComplexFieldElem}, Ref{ComplexPolyRingElem}, Ref{ComplexFieldElem}, Int),
        z, x, y, prec)
  return z
end

evaluate(x::ComplexPolyRingElem, y::RingElement, prec::Int = precision(Balls)) = evaluate(x, base_ring(parent(x))(y), prec)
evaluate(x::ComplexPolyRingElem, y::Integer, prec::Int = precision(Balls)) = evaluate(x, base_ring(parent(x))(y), prec)
evaluate(x::ComplexPolyRingElem, y::Rational, prec::Int = precision(Balls)) = evaluate(x, base_ring(parent(x))(y), prec)
evaluate(x::ComplexPolyRingElem, y::Float64, prec::Int = precision(Balls)) = evaluate(x, base_ring(parent(x))(y), prec)
evaluate(x::ComplexPolyRingElem, y::Any, prec::Int = precision(Balls)) = evaluate(x, base_ring(parent(x))(y), prec)

@doc raw"""
    evaluate2(x::ComplexPolyRingElem, y::RingElement; prec::Int = precision(Balls))

Return a tuple $p, q$ consisting of the polynomial $x$ evaluated at $y$ and
its derivative evaluated at $y$.
"""
function evaluate2(x::ComplexPolyRingElem, y::ComplexFieldElem, prec::Int = precision(Balls))
  z = ComplexFieldElem()
  w = ComplexFieldElem()
  ccall((:acb_poly_evaluate2, libflint), Nothing,
        (Ref{ComplexFieldElem}, Ref{ComplexFieldElem}, Ref{ComplexPolyRingElem}, Ref{ComplexFieldElem}, Int),
        z, w, x, y, prec)
  return z, w
end

function evaluate2(x::ComplexPolyRingElem, y::RingElement, prec::Int = precision(Balls))
  return evaluate2(x, base_ring(parent(x))(y), prec)
end

###############################################################################
#
#   Composition
#
###############################################################################

function compose(x::ComplexPolyRingElem, y::ComplexPolyRingElem, prec::Int = precision(Balls); inner::Symbol)
  if inner == :first
    x, y = y, x
  end
  @assert inner == :second
  z = parent(x)()
  ccall((:acb_poly_compose, libflint), Nothing,
        (Ref{ComplexPolyRingElem}, Ref{ComplexPolyRingElem}, Ref{ComplexPolyRingElem}, Int),
        z, x, y, prec)
  return z
end

###############################################################################
#
#   Derivative and integral
#
###############################################################################

function derivative(x::ComplexPolyRingElem, prec::Int = precision(Balls))
  z = parent(x)()
  ccall((:acb_poly_derivative, libflint), Nothing,
        (Ref{ComplexPolyRingElem}, Ref{ComplexPolyRingElem}, Int), z, x, prec)
  return z
end

function integral(x::ComplexPolyRingElem, prec::Int = precision(Balls))
  z = parent(x)()
  ccall((:acb_poly_integral, libflint), Nothing,
        (Ref{ComplexPolyRingElem}, Ref{ComplexPolyRingElem}, Int), z, x, prec)
  return z
end

###############################################################################
#
#   Multipoint evaluation and interpolation
#
###############################################################################

function acb_vec(n::Int)
  return ccall((:_acb_vec_init, libflint), Ptr{acb_struct}, (Int,), n)
end

function acb_vec(b::Vector{ComplexFieldElem})
  v = ccall((:_acb_vec_init, libflint), Ptr{acb_struct}, (Int,), length(b))
  for i=1:length(b)
    ccall((:acb_set, libflint), Nothing, (Ptr{acb_struct}, Ref{ComplexFieldElem}),
          v + (i-1)*sizeof(acb_struct), b[i])
  end
  return v
end

function array(R::ComplexField, v::Ptr{acb_struct}, n::Int)
  r = Vector{ComplexFieldElem}(undef, n)
  for i=1:n
    r[i] = R()
    ccall((:acb_set, libflint), Nothing, (Ref{ComplexFieldElem}, Ptr{acb_struct}),
          r[i], v + (i-1)*sizeof(acb_struct))
  end
  return r
end

function acb_vec_clear(v::Ptr{acb_struct}, n::Int)
  ccall((:_acb_vec_clear, libflint), Nothing, (Ptr{acb_struct}, Int), v, n)
end

@doc raw"""
    from_roots(R::ComplexPolyRing, b::Vector{ComplexFieldElem})

Construct a polynomial in the given polynomial ring from a list of its roots.
"""
function from_roots(R::ComplexPolyRing, b::Vector{ComplexFieldElem}, prec::Int = precision(Balls))
  z = R()
  tmp = acb_vec(b)
  ccall((:acb_poly_product_roots, libflint), Nothing,
        (Ref{ComplexPolyRingElem}, Ptr{acb_struct}, Int, Int), z, tmp, length(b), prec)
  acb_vec_clear(tmp, length(b))
  return z
end

function evaluate_iter(x::ComplexPolyRingElem, b::Vector{ComplexFieldElem}, prec::Int = precision(Balls))
  return ComplexFieldElem[evaluate(x, b[i], prec) for i=1:length(b)]
end

function evaluate_fast(x::ComplexPolyRingElem, b::Vector{ComplexFieldElem}, prec::Int = precision(Balls))
  tmp = acb_vec(b)
  ccall((:acb_poly_evaluate_vec_fast, libflint), Nothing,
        (Ptr{acb_struct}, Ref{ComplexPolyRingElem}, Ptr{acb_struct}, Int, Int),
        tmp, x, tmp, length(b), prec)
  res = array(base_ring(parent(x)), tmp, length(b))
  acb_vec_clear(tmp, length(b))
  return res
end

function interpolate_newton(R::ComplexPolyRing, xs::Vector{ComplexFieldElem}, ys::Vector{ComplexFieldElem}, prec::Int = precision(Balls))
  length(xs) != length(ys) && error()
  z = R()
  xsv = acb_vec(xs)
  ysv = acb_vec(ys)
  ccall((:acb_poly_interpolate_newton, libflint), Nothing,
        (Ref{ComplexPolyRingElem}, Ptr{acb_struct}, Ptr{acb_struct}, Int, Int),
        z, xsv, ysv, length(xs), prec)
  acb_vec_clear(xsv, length(xs))
  acb_vec_clear(ysv, length(ys))
  return z
end

function interpolate_barycentric(R::ComplexPolyRing, xs::Vector{ComplexFieldElem}, ys::Vector{ComplexFieldElem}, prec::Int = precision(Balls))
  length(xs) != length(ys) && error()
  z = R()
  xsv = acb_vec(xs)
  ysv = acb_vec(ys)
  ccall((:acb_poly_interpolate_barycentric, libflint), Nothing,
        (Ref{ComplexPolyRingElem}, Ptr{acb_struct}, Ptr{acb_struct}, Int, Int),
        z, xsv, ysv, length(xs), prec)
  acb_vec_clear(xsv, length(xs))
  acb_vec_clear(ysv, length(ys))
  return z
end

function interpolate_fast(R::ComplexPolyRing, xs::Vector{ComplexFieldElem}, ys::Vector{ComplexFieldElem}, prec::Int = precision(Balls))
  length(xs) != length(ys) && error()
  z = R()
  xsv = acb_vec(xs)
  ysv = acb_vec(ys)
  ccall((:acb_poly_interpolate_fast, libflint), Nothing,
        (Ref{ComplexPolyRingElem}, Ptr{acb_struct}, Ptr{acb_struct}, Int, Int),
        z, xsv, ysv, length(xs), prec)
  acb_vec_clear(xsv, length(xs))
  acb_vec_clear(ysv, length(ys))
  return z
end

# todo: cutoffs for fast algorithm
function interpolate(R::ComplexPolyRing, xs::Vector{ComplexFieldElem}, ys::Vector{ComplexFieldElem}, prec::Int = precision(Balls))
  return interpolate_newton(R, xs, ys, prec)
end

# todo: cutoffs for fast algorithm
function evaluate(x::ComplexPolyRingElem, b::Vector{ComplexFieldElem}, prec::Int = precision(Balls))
  return evaluate_iter(x, b, prec)
end

###############################################################################
#
#   Root finding
#
###############################################################################

@doc raw"""
    roots(x::ComplexPolyRingElem; target=0, isolate_real=false, initial_prec=0, max_prec=0, max_iter=0)

Attempts to isolate the complex roots of the complex polynomial $x$ by
iteratively refining balls in which they lie.

This is done by increasing the working precision, starting at `initial_prec`.
The maximal number of iterations can be set using `max_iter` and the maximal
precision can be set using `max_prec`.

If `isolate_real` is set and $x$ is strictly real, then the real roots will
be isolated from the non-real roots. Every root will have either zero,
positive or negative real part.

It is assumed that $x$ is squarefree.
"""
function roots(x::ComplexPolyRingElem; target=0, isolate_real=false, initial_prec=0, max_prec=0, max_iter=0)
  deg = degree(x)
  if deg <= 0
    return ComplexFieldElem[]
  end

  initial_prec = (initial_prec >= 2) ? initial_prec : 32
  max_prec = (max_prec >= 2) ? max_prec : 3 * precision(Balls)

  isolated = 0
  wp = initial_prec
  roots = acb_vec(deg)

  while true
    in_roots = (wp == initial_prec) ? C_NULL : roots
    step_max_iter = (max_iter >= 1) ? max_iter : min(max(deg, 32), wp)
    isolated = ccall((:acb_poly_find_roots, libflint), Int,
                     (Ptr{acb_struct}, Ref{ComplexPolyRingElem}, Ptr{acb_struct}, Int, Int),
                     roots, x, in_roots, step_max_iter, wp)

    wp = wp * 2

    if isolated == deg
      ok = true
      if target > 0
        for i = 0 : deg-1
          re = ccall((:acb_real_ptr, libflint), Ptr{arb_struct},
                     (Ptr{acb_struct}, ), roots + i * sizeof(acb_struct))
          im = ccall((:acb_imag_ptr, libflint), Ptr{arb_struct},
                     (Ptr{acb_struct}, ), roots + i * sizeof(acb_struct))
          t = ccall((:arb_rad_ptr, libflint), Ptr{mag_struct}, (Ptr{ArbFieldElem}, ), re)
          u = ccall((:arb_rad_ptr, libflint), Ptr{mag_struct}, (Ptr{ArbFieldElem}, ), im)
          ok = ok && (ccall((:mag_cmp_2exp_si, libflint), Cint,
                            (Ptr{mag_struct}, Int), t, -target) <= 0)
          ok = ok && (ccall((:mag_cmp_2exp_si, libflint), Cint,
                            (Ptr{mag_struct}, Int), u, -target) <= 0)
        end
      end

      if isreal(x)
        real_ok = ccall((:acb_poly_validate_real_roots, libflint),
                        Bool, (Ptr{acb_struct}, Ref{ComplexPolyRingElem}, Int), roots, x, wp)

        if isolate_real && !real_ok
          ok = false
        end

        if real_ok
          for i = 0 : deg - 1
            im = ccall((:acb_imag_ptr, libflint), Ptr{arb_struct},
                       (Ptr{ComplexFieldElem}, ), roots + i * sizeof(acb_struct))
            if ccall((:arb_contains_zero, libflint), Bool, (Ptr{arb_struct}, ), im)
              ccall((:arb_zero, libflint), Nothing, (Ptr{arb_struct}, ), im)
            end
          end
        end
      end

      if ok
        break
      end
    end

    if wp > max_prec
      break
    end
  end

  if isolated == deg
    ccall((:_acb_vec_sort_pretty, libflint), Nothing,
          (Ptr{acb_struct}, Int), roots, deg)
    res = array(base_ring(parent(x)), roots, deg)
  end

  acb_vec_clear(roots, deg)

  if isolated == deg
    return res
  else
    error("unable to isolate all roots (insufficient precision, or there is a multiple root)")
  end
end

###############################################################################
#
#   Root bounds
#
###############################################################################

@doc raw"""
    roots_upper_bound(x::ComplexPolyRingElem) -> ArbFieldElem

Returns an upper bound for the absolute value of all complex roots of $x$.
"""
function roots_upper_bound(x::ComplexPolyRingElem)
  z = RealFieldElem()
  p = precision(Balls)
  GC.@preserve x z begin
    t = ccall((:arb_rad_ptr, libflint), Ptr{mag_struct}, (Ref{RealFieldElem}, ), z)
    ccall((:acb_poly_root_bound_fujiwara, libflint), Nothing,
          (Ptr{mag_struct}, Ref{ComplexPolyRingElem}), t, x)
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

function zero!(z::ComplexPolyRingElem)
  ccall((:acb_poly_zero, libflint), Nothing, (Ref{ComplexPolyRingElem},), z)
  return z
end

function fit!(z::ComplexPolyRingElem, n::Int)
  ccall((:acb_poly_fit_length, libflint), Nothing,
        (Ref{ComplexPolyRingElem}, Int), z, n)
  return nothing
end

function setcoeff!(z::ComplexPolyRingElem, n::Int, x::ZZRingElem)
  ccall((:acb_poly_set_coeff_fmpz, libflint), Nothing,
        (Ref{ComplexPolyRingElem}, Int, Ref{ZZRingElem}), z, n, x)
  return z
end

function setcoeff!(z::ComplexPolyRingElem, n::Int, x::ComplexFieldElem)
  ccall((:acb_poly_set_coeff_acb, libflint), Nothing,
        (Ref{ComplexPolyRingElem}, Int, Ref{ComplexFieldElem}), z, n, x)
  return z
end

function mul!(z::ComplexPolyRingElem, x::ComplexPolyRingElem, y::ComplexPolyRingElem)
  ccall((:acb_poly_mul, libflint), Nothing,
        (Ref{ComplexPolyRingElem}, Ref{ComplexPolyRingElem}, Ref{ComplexPolyRingElem}, Int),
        z, x, y, precision(parent(z)))
  return z
end

function add!(z::ComplexPolyRingElem, x::ComplexPolyRingElem, y::ComplexPolyRingElem)
  ccall((:acb_poly_add, libflint), Nothing,
        (Ref{ComplexPolyRingElem}, Ref{ComplexPolyRingElem}, Ref{ComplexPolyRingElem}, Int),
        z, x, y, precision(parent(z)))
  return z
end

###############################################################################
#
#   Promotions
#
###############################################################################

promote_rule(::Type{ComplexPolyRingElem}, ::Type{ZZPolyRingElem}) = ComplexPolyRingElem

promote_rule(::Type{ComplexPolyRingElem}, ::Type{QQPolyRingElem}) = ComplexPolyRingElem

promote_rule(::Type{ComplexPolyRingElem}, ::Type{ArbPolyRingElem}) = ComplexPolyRingElem

promote_rule(::Type{ComplexPolyRingElem}, ::Type{ComplexPolyRingElem}) = ComplexPolyRingElem

function promote_rule(::Type{ComplexPolyRingElem}, ::Type{T}) where {T}
  return promote_rule(ComplexFieldElem, T) === ComplexFieldElem ? ComplexPolyRingElem : Union{}
end

################################################################################
#
#  Parent object call overloads
#
################################################################################

function (a::ComplexPolyRing)()
  z = ComplexPolyRingElem()
  z.parent = a
  return z
end

for T in [Real, Complex, ZZRingElem, QQFieldElem, RealFieldElem, ComplexFieldElem]
  @eval begin
    function (a::ComplexPolyRing)(b::$T)
      z = ComplexPolyRingElem(base_ring(a)(b), precision(Balls))
      z.parent = a
      return z
    end
  end
end

function (a::ComplexPolyRing)(b::Vector{ComplexFieldElem})
  z = ComplexPolyRingElem(b, precision(Balls))
  z.parent = a
  return z
end

for T in [Real, Complex, ZZRingElem, QQFieldElem, RealFieldElem, ComplexFieldElem]
  @eval begin
    (a::ComplexPolyRing)(b::AbstractVector{<:$T}) = a(map(base_ring(a), b))
  end
end

function (a::ComplexPolyRing)(b::ZZPolyRingElem)
  z = ComplexPolyRingElem(b, precision(Balls))
  z.parent = a
  return z
end

function (a::ComplexPolyRing)(b::QQPolyRingElem)
  z = ComplexPolyRingElem(b, precision(Balls))
  z.parent = a
  return z
end

function (a::ComplexPolyRing)(b::RealPolyRingElem)
  z = ComplexPolyRingElem(b, precision(Balls))
  z.parent = a
  return z
end

function (a::ComplexPolyRing)(b::ComplexPolyRingElem)
  z = ComplexPolyRingElem(b, precision(Balls))
  z.parent = a
  return z
end
