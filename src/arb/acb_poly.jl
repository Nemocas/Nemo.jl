###############################################################################
#
#   acb_poly.jl : Polynomials over AcbFieldElem
#
###############################################################################

###############################################################################
#
#   Basic manipulation
#
###############################################################################

parent_type(::Type{AcbPolyRingElem}) = AcbPolyRing

elem_type(::Type{AcbPolyRing}) = AcbPolyRingElem

dense_poly_type(::Type{AcbFieldElem}) = AcbPolyRingElem

length(x::AcbPolyRingElem) = ccall((:acb_poly_length, libarb), Int,
                                   (Ref{AcbPolyRingElem},), x)

function set_length!(x::AcbPolyRingElem, n::Int)
   ccall((:_acb_poly_set_length, libarb), Nothing,
                                   (Ref{AcbPolyRingElem}, Int), x, n)
   return x
end

degree(x::AcbPolyRingElem) = length(x) - 1

function coeff(a::AcbPolyRingElem, n::Int)
  n < 0 && throw(DomainError(n, "Index must be non-negative"))
  t = parent(a).base_ring()
  ccall((:acb_poly_get_coeff_acb, libarb), Nothing,
              (Ref{AcbFieldElem}, Ref{AcbPolyRingElem}, Int), t, a, n)
  return t
end

zero(a::AcbPolyRing) = a(0)

one(a::AcbPolyRing) = a(1)

function gen(a::AcbPolyRing)
   z = AcbPolyRingElem()
   ccall((:acb_poly_set_coeff_si, libarb), Nothing,
        (Ref{AcbPolyRingElem}, Int, Int), z, 1, 1)
   z.parent = a
   return z
end

# todo: write a C function for this
function is_gen(a::AcbPolyRingElem)
   return isequal(a, gen(parent(a)))
end

#function iszero(a::AcbPolyRingElem)
#   return length(a) == 0
#end

#function isone(a::AcbPolyRingElem)
#   return isequal(a, one(parent(a)))
#end

function deepcopy_internal(a::AcbPolyRingElem, dict::IdDict)
   z = AcbPolyRingElem(a)
   z.parent = parent(a)
   return z
end

###############################################################################
#
#   AbstractString I/O
#
###############################################################################

function show(io::IO, x::AcbPolyRing)
  @show_name(io, x)
  @show_special(io, x)
  print(io, "Univariate Polynomial Ring in ")
  print(io, var(x))
  print(io, " over ")
  show(io, x.base_ring)
end

function Base.show(io::IO, a::AcbPolyRingElem)
   print(io, AbstractAlgebra.obj_to_string(a, context = io))
end

###############################################################################
#
#   Similar
#
###############################################################################

function similar(f::PolyRingElem, R::AcbField, var::VarName=var(parent(f)); cached::Bool=true)
   z = AcbPolyRingElem()
   z.parent = AcbPolyRing(R, Symbol(var), cached)
   return z
end

###############################################################################
#
#   polynomial constructor
#
###############################################################################

function polynomial(R::AcbField, arr::Vector{T}, var::VarName=:x; cached::Bool=true) where T
   coeffs = map(R, arr)
   coeffs = length(coeffs) == 0 ? AcbFieldElem[] : coeffs
   z = AcbPolyRingElem(coeffs, R.prec)
   z.parent = AcbPolyRing(R, Symbol(var), cached)
   return z
end

###############################################################################
#
#   Comparisons
#
###############################################################################

function isequal(x::AcbPolyRingElem, y::AcbPolyRingElem)
   return ccall((:acb_poly_equal, libarb), Bool,
                                      (Ref{AcbPolyRingElem}, Ref{AcbPolyRingElem}), x, y)
end

@doc raw"""
    overlaps(x::AcbPolyRingElem, y::AcbPolyRingElem)

Return `true` if the coefficient boxes of $x$ overlap the coefficient boxes
of $y$, otherwise return `false`.
"""
function overlaps(x::AcbPolyRingElem, y::AcbPolyRingElem)
   return ccall((:acb_poly_overlaps, libarb), Bool,
                                      (Ref{AcbPolyRingElem}, Ref{AcbPolyRingElem}), x, y)
end

@doc raw"""
    contains(x::AcbPolyRingElem, y::AcbPolyRingElem)

Return `true` if the coefficient boxes of $x$ contain the corresponding
coefficient boxes of $y$, otherwise return `false`.
"""
function contains(x::AcbPolyRingElem, y::AcbPolyRingElem)
   return ccall((:acb_poly_contains, libarb), Bool,
                                      (Ref{AcbPolyRingElem}, Ref{AcbPolyRingElem}), x, y)
end

@doc raw"""
    contains(x::AcbPolyRingElem, y::ZZPolyRingElem)

Return `true` if the coefficient boxes of $x$ contain the corresponding
exact coefficients of $y$, otherwise return `false`.
"""
function contains(x::AcbPolyRingElem, y::ZZPolyRingElem)
   return ccall((:acb_poly_contains_fmpz_poly, libarb), Bool,
                                      (Ref{AcbPolyRingElem}, Ref{ZZPolyRingElem}), x, y)
end

@doc raw"""
    contains(x::AcbPolyRingElem, y::QQPolyRingElem)

Return `true` if the coefficient boxes of $x$ contain the corresponding
exact coefficients of $y$, otherwise return `false`.
"""
function contains(x::AcbPolyRingElem, y::QQPolyRingElem)
   return ccall((:acb_poly_contains_fmpq_poly, libarb), Bool,
                                      (Ref{AcbPolyRingElem}, Ref{QQPolyRingElem}), x, y)
end

function ==(x::AcbPolyRingElem, y::AcbPolyRingElem)
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

function !=(x::AcbPolyRingElem, y::AcbPolyRingElem)
    for i = 0:max(degree(x), degree(y))
        if coeff(x, i) != coeff(y, i)
            return true
        end
    end
    return false
end

@doc raw"""
    unique_integer(x::AcbPolyRingElem)

Return a tuple `(t, z)` where $t$ is `true` if there is a unique integer
contained in the (constant) polynomial $x$, along with that integer $z$
in case it is, otherwise sets $t$ to `false`.
"""
function unique_integer(x::AcbPolyRingElem)
  z = ZZPolyRing(ZZ, var(parent(x)))()
  unique = ccall((:acb_poly_get_unique_fmpz_poly, libarb), Int,
    (Ref{ZZPolyRingElem}, Ref{AcbPolyRingElem}), z, x)
  return (unique != 0, z)
end

function isreal(x::AcbPolyRingElem)
  return ccall((:acb_poly_is_real, libarb), Cint, (Ref{AcbPolyRingElem}, ), x) != 0
end

###############################################################################
#
#   Shifting
#
###############################################################################

function shift_left(x::AcbPolyRingElem, len::Int)
   len < 0 && throw(DomainError(len, "Shift must be non-negative"))
   z = parent(x)()
   ccall((:acb_poly_shift_left, libarb), Nothing,
      (Ref{AcbPolyRingElem}, Ref{AcbPolyRingElem}, Int), z, x, len)
   return z
end

function shift_right(x::AcbPolyRingElem, len::Int)
   len < 0 && throw(DomainError(len, "Shift must be non-negative"))
   z = parent(x)()
   ccall((:acb_poly_shift_right, libarb), Nothing,
       (Ref{AcbPolyRingElem}, Ref{AcbPolyRingElem}, Int), z, x, len)
   return z
end

################################################################################
#
#  Unary operations
#
################################################################################

function -(x::AcbPolyRingElem)
  z = parent(x)()
  ccall((:acb_poly_neg, libarb), Nothing, (Ref{AcbPolyRingElem}, Ref{AcbPolyRingElem}), z, x)
  return z
end

################################################################################
#
#  Binary operations
#
################################################################################

function +(x::AcbPolyRingElem, y::AcbPolyRingElem)
  z = parent(x)()
  ccall((:acb_poly_add, libarb), Nothing,
              (Ref{AcbPolyRingElem}, Ref{AcbPolyRingElem}, Ref{AcbPolyRingElem}, Int),
              z, x, y, precision(parent(x)))
  return z
end

function *(x::AcbPolyRingElem, y::AcbPolyRingElem)
  z = parent(x)()
  ccall((:acb_poly_mul, libarb), Nothing,
              (Ref{AcbPolyRingElem}, Ref{AcbPolyRingElem}, Ref{AcbPolyRingElem}, Int),
              z, x, y, precision(parent(x)))
  return z
end

function -(x::AcbPolyRingElem, y::AcbPolyRingElem)
  z = parent(x)()
  ccall((:acb_poly_sub, libarb), Nothing,
              (Ref{AcbPolyRingElem}, Ref{AcbPolyRingElem}, Ref{AcbPolyRingElem}, Int),
              z, x, y, precision(parent(x)))
  return z
end

function ^(x::AcbPolyRingElem, y::Int)
  y < 0 && throw(DomainError(y, "Exponent must be non-negative"))
  z = parent(x)()
  ccall((:acb_poly_pow_ui, libarb), Nothing,
              (Ref{AcbPolyRingElem}, Ref{AcbPolyRingElem}, UInt, Int),
              z, x, y, precision(parent(x)))
  return z
end

###############################################################################
#
#   Ad hoc binary operators
#
###############################################################################

# to avoid method ambiguity errors, include `AbstractFloat, Integer, Rational` in addition to `Real`
for T in [AbstractFloat, Integer, Rational, Real, Complex, ZZRingElem, QQFieldElem, ArbFieldElem, AcbFieldElem, ZZPolyRingElem, QQPolyRingElem]
   @eval begin
      +(x::AcbPolyRingElem, y::$T) = x + parent(x)(y)

      +(x::$T, y::AcbPolyRingElem) = y + x

      -(x::AcbPolyRingElem, y::$T) = x - parent(x)(y)

      -(x::$T, y::AcbPolyRingElem) = parent(y)(x) - y

      *(x::AcbPolyRingElem, y::$T) = x * parent(x)(y)

      *(x::$T, y::AcbPolyRingElem) = y * x
   end
end

###############################################################################
#
#   Scalar division
#
###############################################################################

# to avoid method ambiguity errors, include `AbstractFloat, Integer, Rational` in addition to `Real`
for T in [AbstractFloat, Integer, Rational, Real, Complex, ZZRingElem, QQFieldElem, ArbFieldElem, AcbFieldElem]
   @eval begin
      divexact(x::AcbPolyRingElem, y::$T; check::Bool=true) = x * inv(base_ring(parent(x))(y))

      //(x::AcbPolyRingElem, y::$T) = divexact(x, y)

      /(x::AcbPolyRingElem, y::$T) = divexact(x, y)
   end
end

###############################################################################
#
#   Euclidean division
#
###############################################################################

function Base.divrem(x::AcbPolyRingElem, y::AcbPolyRingElem)
   iszero(y) && throw(DivideError())
   q = parent(x)()
   r = parent(x)()
   if (ccall((:acb_poly_divrem, libarb), Int,
         (Ref{AcbPolyRingElem}, Ref{AcbPolyRingElem}, Ref{AcbPolyRingElem}, Ref{AcbPolyRingElem}, Int),
               q, r, x, y, precision(parent(x))) == 1)
      return (q, r)
   else
      throw(DivideError())
   end
end

function mod(x::AcbPolyRingElem, y::AcbPolyRingElem)
   return divrem(x, y)[2]
end

function divexact(x::AcbPolyRingElem, y::AcbPolyRingElem; check::Bool=true)
   return divrem(x, y)[1]
end

###############################################################################
#
#   Truncation
#
###############################################################################

function truncate(a::AcbPolyRingElem, n::Int)
   n < 0 && throw(DomainError(n, "Index must be non-negative"))
   if length(a) <= n
      return a
   end
   # todo: implement set_trunc in ArbFieldElem
   z = deepcopy(a)
   ccall((:acb_poly_truncate, libarb), Nothing,
                (Ref{AcbPolyRingElem}, Int), z, n)
   return z
end

function mullow(x::AcbPolyRingElem, y::AcbPolyRingElem, n::Int)
   n < 0 && throw(DomainError(n, "Index must be non-negative"))
   z = parent(x)()
   ccall((:acb_poly_mullow, libarb), Nothing,
         (Ref{AcbPolyRingElem}, Ref{AcbPolyRingElem}, Ref{AcbPolyRingElem}, Int, Int),
            z, x, y, n, precision(parent(x)))
   return z
end

###############################################################################
#
#   Reversal
#
###############################################################################

#function reverse(x::AcbPolyRingElem, len::Int)
#   len < 0 && throw(DomainError())
#   z = parent(x)()
#   ccall((:acb_poly_reverse, libarb), Nothing,
#                (Ref{AcbPolyRingElem}, Ref{AcbPolyRingElem}, Int), z, x, len)
#   return z
#end

###############################################################################
#
#   Evaluation
#
###############################################################################

function evaluate(x::AcbPolyRingElem, y::AcbFieldElem)
   z = parent(y)()
   ccall((:acb_poly_evaluate, libarb), Nothing,
                (Ref{AcbFieldElem}, Ref{AcbPolyRingElem}, Ref{AcbFieldElem}, Int),
                z, x, y, precision(parent(y)))
   return z
end

evaluate(x::AcbPolyRingElem, y::RingElem) = evaluate(x, base_ring(parent(x))(y))
evaluate(x::AcbPolyRingElem, y::Integer) = evaluate(x, base_ring(parent(x))(y))
evaluate(x::AcbPolyRingElem, y::Rational) = evaluate(x, base_ring(parent(x))(y))
evaluate(x::AcbPolyRingElem, y::Float64) = evaluate(x, base_ring(parent(x))(y))
evaluate(x::AcbPolyRingElem, y::Any) = evaluate(x, base_ring(parent(x))(y))

@doc raw"""
    evaluate2(x::AcbPolyRingElem, y::RingElement)

Return a tuple $p, q$ consisting of the polynomial $x$ evaluated at $y$ and
its derivative evaluated at $y$.
"""
function evaluate2(x::AcbPolyRingElem, y::AcbFieldElem)
   z = parent(y)()
   w = parent(y)()
   ccall((:acb_poly_evaluate2, libarb), Nothing,
                (Ref{AcbFieldElem}, Ref{AcbFieldElem}, Ref{AcbPolyRingElem}, Ref{AcbFieldElem}, Int),
                z, w, x, y, precision(parent(y)))
   return z, w
end

evaluate2(x::AcbPolyRingElem, y::RingElement) = evaluate2(x, base_ring(parent(x))(y))

###############################################################################
#
#   Composition
#
###############################################################################

function AbstractAlgebra._compose_right(x::AcbPolyRingElem, y::AcbPolyRingElem)
   z = parent(x)()
   ccall((:acb_poly_compose, libarb), Nothing,
                (Ref{AcbPolyRingElem}, Ref{AcbPolyRingElem}, Ref{AcbPolyRingElem}, Int),
                z, x, y, precision(parent(x)))
   return z
end

###############################################################################
#
#   Derivative and integral
#
###############################################################################

function derivative(x::AcbPolyRingElem)
   z = parent(x)()
   ccall((:acb_poly_derivative, libarb), Nothing,
                (Ref{AcbPolyRingElem}, Ref{AcbPolyRingElem}, Int), z, x, precision(parent(x)))
   return z
end

function integral(x::AcbPolyRingElem)
   z = parent(x)()
   ccall((:acb_poly_integral, libarb), Nothing,
                (Ref{AcbPolyRingElem}, Ref{AcbPolyRingElem}, Int), z, x, precision(parent(x)))
   return z
end

###############################################################################
#
#   Multipoint evaluation and interpolation
#
###############################################################################

function acb_vec(b::Vector{AcbFieldElem})
   v = ccall((:_acb_vec_init, libarb), Ptr{acb_struct}, (Int,), length(b))
   for i=1:length(b)
       ccall((:acb_set, libarb), Nothing, (Ptr{acb_struct}, Ref{AcbFieldElem}),
           v + (i-1)*sizeof(acb_struct), b[i])
   end
   return v
end

function array(R::AcbField, v::Ptr{acb_struct}, n::Int)
   r = Vector{AcbFieldElem}(undef, n)
   for i=1:n
       r[i] = R()
       ccall((:acb_set, libarb), Nothing, (Ref{AcbFieldElem}, Ptr{acb_struct}),
           r[i], v + (i-1)*sizeof(acb_struct))
   end
   return r
end

@doc raw"""
    from_roots(R::AcbPolyRing, b::Vector{AcbFieldElem})

Construct a polynomial in the given polynomial ring from a list of its roots.
"""
function from_roots(R::AcbPolyRing, b::Vector{AcbFieldElem})
   z = R()
   tmp = acb_vec(b)
   ccall((:acb_poly_product_roots, libarb), Nothing,
                (Ref{AcbPolyRingElem}, Ptr{acb_struct}, Int, Int), z, tmp, length(b), precision(R))
   acb_vec_clear(tmp, length(b))
   return z
end

function evaluate_iter(x::AcbPolyRingElem, b::Vector{AcbFieldElem})
   return AcbFieldElem[evaluate(x, b[i]) for i=1:length(b)]
end

function evaluate_fast(x::AcbPolyRingElem, b::Vector{AcbFieldElem})
   tmp = acb_vec(b)
   ccall((:acb_poly_evaluate_vec_fast, libarb), Nothing,
                (Ptr{acb_struct}, Ref{AcbPolyRingElem}, Ptr{acb_struct}, Int, Int),
            tmp, x, tmp, length(b), precision(parent(x)))
   res = array(base_ring(parent(x)), tmp, length(b))
   acb_vec_clear(tmp, length(b))
   return res
end

function interpolate_newton(R::AcbPolyRing, xs::Vector{AcbFieldElem}, ys::Vector{AcbFieldElem})
   length(xs) != length(ys) && error()
   z = R()
   xsv = acb_vec(xs)
   ysv = acb_vec(ys)
   ccall((:acb_poly_interpolate_newton, libarb), Nothing,
                (Ref{AcbPolyRingElem}, Ptr{acb_struct}, Ptr{acb_struct}, Int, Int),
            z, xsv, ysv, length(xs), precision(R))
   acb_vec_clear(xsv, length(xs))
   acb_vec_clear(ysv, length(ys))
   return z
end

function interpolate_barycentric(R::AcbPolyRing, xs::Vector{AcbFieldElem}, ys::Vector{AcbFieldElem})
   length(xs) != length(ys) && error()
   z = R()
   xsv = acb_vec(xs)
   ysv = acb_vec(ys)
   ccall((:acb_poly_interpolate_barycentric, libarb), Nothing,
                (Ref{AcbPolyRingElem}, Ptr{acb_struct}, Ptr{acb_struct}, Int, Int),
            z, xsv, ysv, length(xs), precision(R))
   acb_vec_clear(xsv, length(xs))
   acb_vec_clear(ysv, length(ys))
   return z
end

function interpolate_fast(R::AcbPolyRing, xs::Vector{AcbFieldElem}, ys::Vector{AcbFieldElem})
   length(xs) != length(ys) && error()
   z = R()
   xsv = acb_vec(xs)
   ysv = acb_vec(ys)
   ccall((:acb_poly_interpolate_fast, libarb), Nothing,
                (Ref{AcbPolyRingElem}, Ptr{acb_struct}, Ptr{acb_struct}, Int, Int),
            z, xsv, ysv, length(xs), precision(R))
   acb_vec_clear(xsv, length(xs))
   acb_vec_clear(ysv, length(ys))
   return z
end

# todo: cutoffs for fast algorithm
function interpolate(R::AcbPolyRing, xs::Vector{AcbFieldElem}, ys::Vector{AcbFieldElem})
   return interpolate_newton(R, xs, ys)
end

# todo: cutoffs for fast algorithm
function evaluate(x::AcbPolyRingElem, b::Vector{AcbFieldElem})
   return evaluate_iter(x, b)
end

###############################################################################
#
#   Root finding
#
###############################################################################

@doc raw"""
    roots(x::AcbPolyRingElem; target=0, isolate_real=false, initial_prec=0, max_prec=0, max_iter=0)

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
function roots(x::AcbPolyRingElem; target=0, isolate_real=false, initial_prec=0, max_prec=0, max_iter=0)
    deg = degree(x)
    if deg <= 0
        return Array{AcbFieldElem}(undef, 0)
    end

    initial_prec = (initial_prec >= 2) ? initial_prec : 32
    max_prec = (max_prec >= 2) ? max_prec : 3 * precision(parent(x))

    isolated = 0
    wp = initial_prec
    roots = acb_vec(deg)

    while true
        in_roots = (wp == initial_prec) ? C_NULL : roots
        step_max_iter = (max_iter >= 1) ? max_iter : min(max(deg, 32), wp)
        isolated = ccall((:acb_poly_find_roots, libarb), Int,
            (Ptr{acb_struct}, Ref{AcbPolyRingElem}, Ptr{acb_struct}, Int, Int),
                roots, x, in_roots, step_max_iter, wp)

        wp = wp * 2

        if isolated == deg
            ok = true
            if target > 0
                for i = 0 : deg-1
                    re = ccall((:acb_real_ptr, libarb), Ptr{arb_struct},
                        (Ptr{AcbFieldElem}, ), roots + i * sizeof(acb_struct))
                    im = ccall((:acb_imag_ptr, libarb), Ptr{arb_struct},
                        (Ptr{AcbFieldElem}, ), roots + i * sizeof(acb_struct))
                    t = ccall((:arb_rad_ptr, libarb), Ptr{mag_struct}, (Ptr{ArbFieldElem}, ), re)
                    u = ccall((:arb_rad_ptr, libarb), Ptr{mag_struct}, (Ptr{ArbFieldElem}, ), im)
                    ok = ok && (ccall((:mag_cmp_2exp_si, libarb), Cint,
                        (Ptr{mag_struct}, Int), t, -target) <= 0)
                    ok = ok && (ccall((:mag_cmp_2exp_si, libarb), Cint,
                        (Ptr{mag_struct}, Int), u, -target) <= 0)
                end
            end

            if isreal(x)
                real_ok = ccall((:acb_poly_validate_real_roots, libarb),
                    Bool, (Ptr{acb_struct}, Ref{AcbPolyRingElem}, Int), roots, x, wp)

                if isolate_real && !real_ok
                    ok = false
                end

                if real_ok
                    for i = 0 : deg - 1
                        im = ccall((:acb_imag_ptr, libarb), Ptr{arb_struct},
                            (Ptr{AcbFieldElem}, ), roots + i * sizeof(acb_struct))
                        if ccall((:arb_contains_zero, libarb), Bool, (Ptr{arb_struct}, ), im)
                            ccall((:arb_zero, libarb), Nothing, (Ptr{arb_struct}, ), im)
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
        ccall((:_acb_vec_sort_pretty, libarb), Nothing,
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
    roots_upper_bound(x::AcbPolyRingElem) -> ArbFieldElem

Returns an upper bound for the absolute value of all complex roots of $x$.
"""
function roots_upper_bound(x::AcbPolyRingElem)
   z = ArbField(precision(base_ring(x)))()
   p = precision(base_ring(x))
   GC.@preserve x z begin
      t = ccall((:arb_rad_ptr, libarb), Ptr{mag_struct}, (Ref{ArbFieldElem}, ), z)
      ccall((:acb_poly_root_bound_fujiwara, libarb), Nothing,
            (Ptr{mag_struct}, Ref{AcbPolyRingElem}), t, x)
      s = ccall((:arb_mid_ptr, libarb), Ptr{arf_struct}, (Ref{ArbFieldElem}, ), z)
      ccall((:arf_set_mag, libarb), Nothing, (Ptr{arf_struct}, Ptr{mag_struct}), s, t)
      ccall((:arf_set_round, libarb), Nothing,
            (Ptr{arf_struct}, Ptr{arf_struct}, Int, Cint), s, s, p, ARB_RND_CEIL)
      ccall((:mag_zero, libarb), Nothing, (Ptr{mag_struct},), t)
   end
   return z
end

###############################################################################
#
#   Unsafe functions
#
###############################################################################

function zero!(z::AcbPolyRingElem)
   ccall((:acb_poly_zero, libarb), Nothing, (Ref{AcbPolyRingElem},), z)
   return z
end

function fit!(z::AcbPolyRingElem, n::Int)
   ccall((:acb_poly_fit_length, libarb), Nothing,
                    (Ref{AcbPolyRingElem}, Int), z, n)
   return nothing
end

function setcoeff!(z::AcbPolyRingElem, n::Int, x::ZZRingElem)
   ccall((:acb_poly_set_coeff_fmpz, libarb), Nothing,
                    (Ref{AcbPolyRingElem}, Int, Ref{ZZRingElem}), z, n, x)
   return z
end

function setcoeff!(z::AcbPolyRingElem, n::Int, x::AcbFieldElem)
   ccall((:acb_poly_set_coeff_acb, libarb), Nothing,
                    (Ref{AcbPolyRingElem}, Int, Ref{AcbFieldElem}), z, n, x)
   return z
end

function mul!(z::AcbPolyRingElem, x::AcbPolyRingElem, y::AcbPolyRingElem)
   ccall((:acb_poly_mul, libarb), Nothing,
                (Ref{AcbPolyRingElem}, Ref{AcbPolyRingElem}, Ref{AcbPolyRingElem}, Int),
                    z, x, y, precision(parent(z)))
   return z
end

function addeq!(z::AcbPolyRingElem, x::AcbPolyRingElem)
   ccall((:acb_poly_add, libarb), Nothing,
                (Ref{AcbPolyRingElem}, Ref{AcbPolyRingElem}, Ref{AcbPolyRingElem}, Int),
                    z, z, x, precision(parent(z)))
   return z
end

function add!(z::AcbPolyRingElem, x::AcbPolyRingElem, y::AcbPolyRingElem)
   ccall((:acb_poly_add, libarb), Nothing,
                (Ref{AcbPolyRingElem}, Ref{AcbPolyRingElem}, Ref{AcbPolyRingElem}, Int),
                    z, x, y, precision(parent(z)))
   return z
end

###############################################################################
#
#   Promotions
#
###############################################################################

promote_rule(::Type{AcbPolyRingElem}, ::Type{ZZPolyRingElem}) = AcbPolyRingElem

promote_rule(::Type{AcbPolyRingElem}, ::Type{QQPolyRingElem}) = AcbPolyRingElem

promote_rule(::Type{AcbPolyRingElem}, ::Type{ArbPolyRingElem}) = AcbPolyRingElem

promote_rule(::Type{AcbPolyRingElem}, ::Type{AcbPolyRingElem}) = AcbPolyRingElem

function promote_rule(::Type{AcbPolyRingElem}, ::Type{T}) where {T}
   return promote_rule(AcbFieldElem, T) === AcbFieldElem ? AcbPolyRingElem : Union{}
end

################################################################################
#
#  Parent object call overloads
#
################################################################################

function (a::AcbPolyRing)()
   z = AcbPolyRingElem()
   z.parent = a
   return z
end

for T in [Real, Complex, ZZRingElem, QQFieldElem, ArbFieldElem, AcbFieldElem]
  @eval begin
    function (a::AcbPolyRing)(b::$T)
      z = AcbPolyRingElem(base_ring(a)(b), a.base_ring.prec)
      z.parent = a
      return z
    end
  end
end

function (a::AcbPolyRing)(b::Vector{AcbFieldElem})
   z = AcbPolyRingElem(b, a.base_ring.prec)
   z.parent = a
   return z
end

for T in [Real, Complex, ZZRingElem, QQFieldElem, ArbFieldElem, AcbFieldElem]
  @eval begin
    (a::AcbPolyRing)(b::AbstractVector{<:$T}) = a(map(base_ring(a), b))
  end
end

function (a::AcbPolyRing)(b::ZZPolyRingElem)
   z = AcbPolyRingElem(b, a.base_ring.prec)
   z.parent = a
   return z
end

function (a::AcbPolyRing)(b::QQPolyRingElem)
   z = AcbPolyRingElem(b, a.base_ring.prec)
   z.parent = a
   return z
end

function (a::AcbPolyRing)(b::ArbPolyRingElem)
   z = AcbPolyRingElem(b, a.base_ring.prec)
   z.parent = a
   return z
end

function (a::AcbPolyRing)(b::AcbPolyRingElem)
   z = AcbPolyRingElem(b, a.base_ring.prec)
   z.parent = a
   return z
end
