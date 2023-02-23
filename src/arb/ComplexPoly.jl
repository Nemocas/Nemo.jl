###############################################################################
#
#   acb_poly.jl : Polynomials over arb
#
###############################################################################

export isreal, derivative, integral, evaluate,
       evaluate2, compose, from_roots, evaluate_iter, evaluate_fast, evaluate,
       interpolate_newton, interpolate_barycentric, interpolate_fast,
       interpolate, roots

###############################################################################
#
#   Basic manipulation
#
###############################################################################

parent_type(::Type{ComplexPoly}) = ComplexPolyRing

elem_type(::Type{ComplexPolyRing}) = ComplexPoly

dense_poly_type(::Type{ComplexFieldElem}) = ComplexPoly

length(x::ComplexPoly) = ccall((:acb_poly_length, libarb), Int,
                                   (Ref{ComplexPoly},), x)

function set_length!(x::ComplexPoly, n::Int)
   ccall((:_acb_poly_set_length, libarb), Nothing,
                                   (Ref{ComplexPoly}, Int), x, n)
   return x
end

degree(x::ComplexPoly) = length(x) - 1

function coeff(a::ComplexPoly, n::Int)
  n < 0 && throw(DomainError(n, "Index must be non-negative"))
  t = ComplexFieldElem()
  ccall((:acb_poly_get_coeff_acb, libarb), Nothing,
              (Ref{ComplexFieldElem}, Ref{ComplexPoly}, Int), t, a, n)
  return t
end

zero(a::ComplexPolyRing) = a(0)

one(a::ComplexPolyRing) = a(1)

function gen(a::ComplexPolyRing)
   z = ComplexPoly()
   ccall((:acb_poly_set_coeff_si, libarb), Nothing,
        (Ref{ComplexPoly}, Int, Int), z, 1, 1)
   z.parent = a
   return z
end

# todo: write a C function for this
function is_gen(a::ComplexPoly)
   return isequal(a, gen(parent(a)))
end

#function iszero(a::ComplexPoly)
#   return length(a) == 0
#end

#function isone(a::ComplexPoly)
#   return isequal(a, one(parent(a)))
#end

function deepcopy_internal(a::ComplexPoly, dict::IdDict)
   z = ComplexPoly(a)
   z.parent = parent(a)
   return z
end

characteristic(::ComplexPolyRing) = 0

###############################################################################
#
#   AbstractString I/O
#
###############################################################################

function show(io::IO, x::ComplexPolyRing)
  print(io, "Univariate Polynomial Ring in ")
  print(io, string(var(x)))
  print(io, " over ")
  show(io, base_ring(x))
end

function Base.show(io::IO, a::ComplexPoly)
   print(io, AbstractAlgebra.obj_to_string(a, context = io))
end

###############################################################################
#
#   Similar
#
###############################################################################

function similar(f::PolyElem, R::ComplexField, var::Symbol=var(parent(f)); cached::Bool=true)
   z = ComplexPoly()
   z.parent = ComplexPolyRing(R, var, cached)
   return z
end

###############################################################################
#
#   polynomial constructor
#
###############################################################################

function polynomial(R::ComplexField, arr::Vector{T}, var::String="x"; cached::Bool=true) where T
   coeffs = map(R, arr)
   coeffs = length(coeffs) == 0 ? ComplexFieldElem[] : coeffs
   z = ComplexPoly(coeffs, R.prec)
   z.parent = ComplexPolyRing(R, Symbol(var), cached)
   return z
end

###############################################################################
#
#   Comparisons
#
###############################################################################

function isequal(x::ComplexPoly, y::ComplexPoly)
   return ccall((:acb_poly_equal, libarb), Bool,
                                      (Ref{ComplexPoly}, Ref{ComplexPoly}), x, y)
end

@doc Markdown.doc"""
    overlaps(x::ComplexPoly, y::ComplexPoly)

Return `true` if the coefficient boxes of $x$ overlap the coefficient boxes
of $y$, otherwise return `false`.
"""
function overlaps(x::ComplexPoly, y::ComplexPoly)
   return ccall((:acb_poly_overlaps, libarb), Bool,
                                      (Ref{ComplexPoly}, Ref{ComplexPoly}), x, y)
end

@doc Markdown.doc"""
    contains(x::ComplexPoly, y::ComplexPoly)

Return `true` if the coefficient boxes of $x$ contain the corresponding
coefficient boxes of $y$, otherwise return `false`.
"""
function contains(x::ComplexPoly, y::ComplexPoly)
   return ccall((:acb_poly_contains, libarb), Bool,
                                      (Ref{ComplexPoly}, Ref{ComplexPoly}), x, y)
end

@doc Markdown.doc"""
    contains(x::ComplexPoly, y::ZZPolyRingElem)

Return `true` if the coefficient boxes of $x$ contain the corresponding
exact coefficients of $y$, otherwise return `false`.
"""
function contains(x::ComplexPoly, y::ZZPolyRingElem)
   return ccall((:acb_poly_contains_fmpz_poly, libarb), Bool,
                                      (Ref{ComplexPoly}, Ref{ZZPolyRingElem}), x, y)
end

@doc Markdown.doc"""
    contains(x::ComplexPoly, y::QQPolyRingElem)

Return `true` if the coefficient boxes of $x$ contain the corresponding
exact coefficients of $y$, otherwise return `false`.
"""
function contains(x::ComplexPoly, y::QQPolyRingElem)
   return ccall((:acb_poly_contains_fmpq_poly, libarb), Bool,
                                      (Ref{ComplexPoly}, Ref{QQPolyRingElem}), x, y)
end

function ==(x::ComplexPoly, y::ComplexPoly)
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

function !=(x::ComplexPoly, y::ComplexPoly)
    for i = 0:max(degree(x), degree(y))
        if coeff(x, i) != coeff(y, i)
            return true
        end
    end
    return false
end

@doc Markdown.doc"""
    unique_integer(x::ComplexPoly)

Return a tuple `(t, z)` where $t$ is `true` if there is a unique integer
contained in the (constant) polynomial $x$, along with that integer $z$
in case it is, otherwise sets $t$ to `false`.
"""
function unique_integer(x::ComplexPoly)
  z = ZZPolyRing(FlintZZ, var(parent(x)))()
  unique = ccall((:acb_poly_get_unique_fmpz_poly, libarb), Int,
    (Ref{ZZPolyRingElem}, Ref{ComplexPoly}), z, x)
  return (unique != 0, z)
end

function isreal(x::ComplexPoly)
  return ccall((:acb_poly_is_real, libarb), Cint, (Ref{ComplexPoly}, ), x) != 0
end

###############################################################################
#
#   Shifting
#
###############################################################################

function shift_left(x::ComplexPoly, len::Int)
   len < 0 && throw(DomainError(len, "Shift must be non-negative"))
   z = parent(x)()
   ccall((:acb_poly_shift_left, libarb), Nothing,
      (Ref{ComplexPoly}, Ref{ComplexPoly}, Int), z, x, len)
   return z
end

function shift_right(x::ComplexPoly, len::Int)
   len < 0 && throw(DomainError(len, "Shift must be non-negative"))
   z = parent(x)()
   ccall((:acb_poly_shift_right, libarb), Nothing,
       (Ref{ComplexPoly}, Ref{ComplexPoly}, Int), z, x, len)
   return z
end

################################################################################
#
#  Unary operations
#
################################################################################

function -(x::ComplexPoly)
  z = parent(x)()
  ccall((:acb_poly_neg, libarb), Nothing, (Ref{ComplexPoly}, Ref{ComplexPoly}), z, x)
  return z
end

################################################################################
#
#  Binary operations
#
################################################################################

function +(x::ComplexPoly, y::ComplexPoly)
  z = parent(x)()
  ccall((:acb_poly_add, libarb), Nothing,
              (Ref{ComplexPoly}, Ref{ComplexPoly}, Ref{ComplexPoly}, Int),
              z, x, y, precision(Balls))
  return z
end

function *(x::ComplexPoly, y::ComplexPoly)
  z = parent(x)()
  ccall((:acb_poly_mul, libarb), Nothing,
              (Ref{ComplexPoly}, Ref{ComplexPoly}, Ref{ComplexPoly}, Int),
              z, x, y, precision(Balls))
  return z
end

function -(x::ComplexPoly, y::ComplexPoly)
  z = parent(x)()
  ccall((:acb_poly_sub, libarb), Nothing,
              (Ref{ComplexPoly}, Ref{ComplexPoly}, Ref{ComplexPoly}, Int),
              z, x, y, precision(Balls))
  return z
end

function ^(x::ComplexPoly, y::Int)
  y < 0 && throw(DomainError(y, "Exponent must be non-negative"))
  z = parent(x)()
  ccall((:acb_poly_pow_ui, libarb), Nothing,
              (Ref{ComplexPoly}, Ref{ComplexPoly}, UInt, Int),
              z, x, y, precision(Balls))
  return z
end

###############################################################################
#
#   Ad hoc binary operators
#
###############################################################################

for T in [Integer, ZZRingElem, QQFieldElem, Float64, BigFloat, RealFieldElem, ComplexFieldElem, ZZPolyRingElem, QQPolyRingElem]
   @eval begin
      +(x::ComplexPoly, y::$T) = x + parent(x)(y)

      +(x::$T, y::ComplexPoly) = y + x

      -(x::ComplexPoly, y::$T) = x - parent(x)(y)

      -(x::$T, y::ComplexPoly) = parent(y)(x) - y

      *(x::ComplexPoly, y::$T) = x * parent(x)(y)

      *(x::$T, y::ComplexPoly) = y * x
   end
end

+(x::ComplexPoly, y::Rational{T}) where T <: Union{Int, BigInt} = x + parent(x)(y)

+(x::Rational{T}, y::ComplexPoly) where T <: Union{Int, BigInt} = y + x

-(x::ComplexPoly, y::Rational{T}) where T <: Union{Int, BigInt} = x - parent(x)(y)

-(x::Rational{T}, y::ComplexPoly) where T <: Union{Int, BigInt} = parent(y)(x) - y

*(x::ComplexPoly, y::Rational{T}) where T <: Union{Int, BigInt} = x * parent(x)(y)

*(x::Rational{T}, y::ComplexPoly) where T <: Union{Int, BigInt} = y * x

###############################################################################
#
#   Scalar division
#
###############################################################################

for T in [Integer, ZZRingElem, QQFieldElem, Float64, BigFloat, RealFieldElem, ComplexFieldElem]
   @eval begin
      divexact(x::ComplexPoly, y::$T; check::Bool=true) = x * inv(base_ring(parent(x))(y))

      //(x::ComplexPoly, y::$T) = divexact(x, y)

      /(x::ComplexPoly, y::$T) = divexact(x, y)
   end
end

divexact(x::ComplexPoly, y::Rational{T}; check::Bool=true) where {T <: Integer} = x * inv(base_ring(parent(x))(y))

//(x::ComplexPoly, y::Rational{T}) where {T <: Integer} = divexact(x, y)

/(x::ComplexPoly, y::Rational{T}) where {T <: Integer} = divexact(x, y)

###############################################################################
#
#   Euclidean division
#
###############################################################################

function Base.divrem(x::ComplexPoly, y::ComplexPoly)
   iszero(y) && throw(DivideError())
   q = parent(x)()
   r = parent(x)()
   if (ccall((:acb_poly_divrem, libarb), Int,
         (Ref{ComplexPoly}, Ref{ComplexPoly}, Ref{ComplexPoly}, Ref{ComplexPoly}, Int),
               q, r, x, y, precision(Balls)) == 1)
      return (q, r)
   else
      throw(DivideError())
   end
end

function mod(x::ComplexPoly, y::ComplexPoly)
   return divrem(x, y)[2]
end

function divexact(x::ComplexPoly, y::ComplexPoly; check::Bool=true)
   return divrem(x, y)[1]
end

###############################################################################
#
#   Truncation
#
###############################################################################

function truncate(a::ComplexPoly, n::Int)
   n < 0 && throw(DomainError(n, "Index must be non-negative"))
   if length(a) <= n
      return a
   end
   # todo: implement set_trunc in arb
   z = deepcopy(a)
   ccall((:acb_poly_truncate, libarb), Nothing,
                (Ref{ComplexPoly}, Int), z, n)
   return z
end

function mullow(x::ComplexPoly, y::ComplexPoly, n::Int, prec = precision(Balls))
   n < 0 && throw(DomainError(n, "Index must be non-negative"))
   z = parent(x)()
   ccall((:acb_poly_mullow, libarb), Nothing,
         (Ref{ComplexPoly}, Ref{ComplexPoly}, Ref{ComplexPoly}, Int, Int),
            z, x, y, n, prec)
   return z
end

###############################################################################
#
#   Reversal
#
###############################################################################

#function reverse(x::ComplexPoly, len::Int)
#   len < 0 && throw(DomainError())
#   z = parent(x)()
#   ccall((:acb_poly_reverse, libarb), Nothing,
#                (Ref{ComplexPoly}, Ref{ComplexPoly}, Int), z, x, len)
#   return z
#end

###############################################################################
#
#   Evaluation
#
###############################################################################

function evaluate(x::ComplexPoly, y::ComplexFieldElem, prec = precision(Balls))
   z = parent(y)()
   ccall((:acb_poly_evaluate, libarb), Nothing,
                (Ref{ComplexFieldElem}, Ref{ComplexPoly}, Ref{ComplexFieldElem}, Int),
                z, x, y, prec)
   return z
end

@doc Markdown.doc"""
    evaluate2(x::ComplexPoly, y::ComplexFieldElem)

Return a tuple $p, q$ consisting of the polynomial $x$ evaluated at $y$ and
its derivative evaluated at $y$.
"""
function evaluate2(x::ComplexPoly, y::ComplexFieldElem, prec = precision(Balls))
   z = ComplexFieldElem()
   w = ComplexFieldElem()
   ccall((:acb_poly_evaluate2, libarb), Nothing,
                (Ref{ComplexFieldElem}, Ref{ComplexFieldElem}, Ref{ComplexPoly}, Ref{ComplexFieldElem}, Int),
                z, w, x, y, prec)
   return z, w
end

function evaluate(x::ComplexPoly, y::Union{Int, Float64, ZZRingElem, QQFieldElem, RealFieldElem}, prec = precision(Balls))
    return evaluate(x, base_ring(parent(x))(y), prec)
end

function evaluate2(x::ComplexPoly, y::Union{Integer, Float64, ZZRingElem, QQFieldElem, RealFieldElem}, prec = precision(Balls))
    return evaluate2(x, base_ring(parent(x))(y), prec)
end

###############################################################################
#
#   Composition
#
###############################################################################

function compose(x::ComplexPoly, y::ComplexPoly, prec = precision(Balls))
   z = parent(x)()
   ccall((:acb_poly_compose, libarb), Nothing,
                (Ref{ComplexPoly}, Ref{ComplexPoly}, Ref{ComplexPoly}, Int),
                z, x, y, prec)
   return z
end

###############################################################################
#
#   Derivative and integral
#
###############################################################################

function derivative(x::ComplexPoly, prec = precision(Balls))
   z = parent(x)()
   ccall((:acb_poly_derivative, libarb), Nothing,
                (Ref{ComplexPoly}, Ref{ComplexPoly}, Int), z, x, prec)
   return z
end

function integral(x::ComplexPoly, prec = precision(Balls))
   z = parent(x)()
   ccall((:acb_poly_integral, libarb), Nothing,
                (Ref{ComplexPoly}, Ref{ComplexPoly}, Int), z, x, prec)
   return z
end

###############################################################################
#
#   Multipoint evaluation and interpolation
#
###############################################################################

function acb_vec(n::Int)
   return ccall((:_acb_vec_init, libarb), Ptr{acb_struct}, (Int,), n)
end

function acb_vec(b::Vector{ComplexFieldElem})
   v = ccall((:_acb_vec_init, libarb), Ptr{acb_struct}, (Int,), length(b))
   for i=1:length(b)
       ccall((:acb_set, libarb), Nothing, (Ptr{acb_struct}, Ref{ComplexFieldElem}),
           v + (i-1)*sizeof(acb_struct), b[i])
   end
   return v
end

function array(R::ComplexField, v::Ptr{acb_struct}, n::Int)
   r = Vector{ComplexFieldElem}(undef, n)
   for i=1:n
       r[i] = R()
       ccall((:acb_set, libarb), Nothing, (Ref{ComplexFieldElem}, Ptr{acb_struct}),
           r[i], v + (i-1)*sizeof(acb_struct))
   end
   return r
end

function acb_vec_clear(v::Ptr{acb_struct}, n::Int)
   ccall((:_acb_vec_clear, libarb), Nothing, (Ptr{acb_struct}, Int), v, n)
end

@doc Markdown.doc"""
    from_roots(R::ComplexPolyRing, b::Vector{ComplexFieldElem})

Construct a polynomial in the given polynomial ring from a list of its roots.
"""
function from_roots(R::ComplexPolyRing, b::Vector{ComplexFieldElem}, prec = precision(Balls))
   z = R()
   tmp = acb_vec(b)
   ccall((:acb_poly_product_roots, libarb), Nothing,
                (Ref{ComplexPoly}, Ptr{acb_struct}, Int, Int), z, tmp, length(b), prec)
   acb_vec_clear(tmp, length(b))
   return z
end

function evaluate_iter(x::ComplexPoly, b::Vector{ComplexFieldElem}, prec = precision(Balls))
   return ComplexFieldElem[evaluate(x, b[i], prec) for i=1:length(b)]
end

function evaluate_fast(x::ComplexPoly, b::Vector{ComplexFieldElem}, prec = precision(Balls))
   tmp = acb_vec(b)
   ccall((:acb_poly_evaluate_vec_fast, libarb), Nothing,
                (Ptr{acb_struct}, Ref{ComplexPoly}, Ptr{acb_struct}, Int, Int),
            tmp, x, tmp, length(b), prec)
   res = array(base_ring(parent(x)), tmp, length(b))
   acb_vec_clear(tmp, length(b))
   return res
end

function interpolate_newton(R::ComplexPolyRing, xs::Vector{ComplexFieldElem}, ys::Vector{ComplexFieldElem}, prec = precision(Balls))
   length(xs) != length(ys) && error()
   z = R()
   xsv = acb_vec(xs)
   ysv = acb_vec(ys)
   ccall((:acb_poly_interpolate_newton, libarb), Nothing,
                (Ref{ComplexPoly}, Ptr{acb_struct}, Ptr{acb_struct}, Int, Int),
            z, xsv, ysv, length(xs), prec)
   acb_vec_clear(xsv, length(xs))
   acb_vec_clear(ysv, length(ys))
   return z
end

function interpolate_barycentric(R::ComplexPolyRing, xs::Vector{ComplexFieldElem}, ys::Vector{ComplexFieldElem}, prec = precision(Balls))
   length(xs) != length(ys) && error()
   z = R()
   xsv = acb_vec(xs)
   ysv = acb_vec(ys)
   ccall((:acb_poly_interpolate_barycentric, libarb), Nothing,
                (Ref{ComplexPoly}, Ptr{acb_struct}, Ptr{acb_struct}, Int, Int),
            z, xsv, ysv, length(xs), prec)
   acb_vec_clear(xsv, length(xs))
   acb_vec_clear(ysv, length(ys))
   return z
end

function interpolate_fast(R::ComplexPolyRing, xs::Vector{ComplexFieldElem}, ys::Vector{ComplexFieldElem}, prec = precision(Balls))
   length(xs) != length(ys) && error()
   z = R()
   xsv = acb_vec(xs)
   ysv = acb_vec(ys)
   ccall((:acb_poly_interpolate_fast, libarb), Nothing,
                (Ref{ComplexPoly}, Ptr{acb_struct}, Ptr{acb_struct}, Int, Int),
            z, xsv, ysv, length(xs), prec)
   acb_vec_clear(xsv, length(xs))
   acb_vec_clear(ysv, length(ys))
   return z
end

# todo: cutoffs for fast algorithm
function interpolate(R::ComplexPolyRing, xs::Vector{ComplexFieldElem}, ys::Vector{ComplexFieldElem}, prec = precision(Balls))
   return interpolate_newton(R, xs, ys, prec)
end

# todo: cutoffs for fast algorithm
function evaluate(x::ComplexPoly, b::Vector{ComplexFieldElem}, prec = precision(Balls))
   return evaluate_iter(x, b, prec)
end

###############################################################################
#
#   Root finding
#
###############################################################################

@doc Markdown.doc"""
    roots(x::ComplexPoly; target=0, isolate_real=false, initial_prec=0, max_prec=0, max_iter=0)

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
function roots(x::ComplexPoly; target=0, isolate_real=false, initial_prec=0, max_prec=0, max_iter=0)
    deg = degree(x)
    if deg <= 0
        return Array{ComplexFieldElem}(undef, 0)
    end

    initial_prec = (initial_prec >= 2) ? initial_prec : 32
    max_prec = (max_prec >= 2) ? max_prec : 3 * precision(Balls)

    isolated = 0
    wp = initial_prec
    roots = acb_vec(deg)

    while true
        in_roots = (wp == initial_prec) ? C_NULL : roots
        step_max_iter = (max_iter >= 1) ? max_iter : min(max(deg, 32), wp)
        isolated = ccall((:acb_poly_find_roots, libarb), Int,
            (Ptr{acb_struct}, Ref{ComplexPoly}, Ptr{acb_struct}, Int, Int),
                roots, x, in_roots, step_max_iter, wp)

        wp = wp * 2

        if isolated == deg
            ok = true
            if target > 0
                for i = 0 : deg-1
                    re = ccall((:acb_real_ptr, libarb), Ptr{arb_struct},
                        (Ptr{acb_struct}, ), roots + i * sizeof(acb_struct))
                    im = ccall((:acb_imag_ptr, libarb), Ptr{arb_struct},
                        (Ptr{acb_struct}, ), roots + i * sizeof(acb_struct))
                    t = ccall((:arb_rad_ptr, libarb), Ptr{mag_struct}, (Ptr{arb}, ), re)
                    u = ccall((:arb_rad_ptr, libarb), Ptr{mag_struct}, (Ptr{arb}, ), im)
                    ok = ok && (ccall((:mag_cmp_2exp_si, libarb), Cint,
                        (Ptr{mag_struct}, Int), t, -target) <= 0)
                    ok = ok && (ccall((:mag_cmp_2exp_si, libarb), Cint,
                        (Ptr{mag_struct}, Int), u, -target) <= 0)
                end
            end

            if isreal(x)
                real_ok = ccall((:acb_poly_validate_real_roots, libarb),
                    Bool, (Ptr{acb_struct}, Ref{ComplexPoly}, Int), roots, x, wp)

                if isolate_real && !real_ok
                    ok = false
                end

                if real_ok
                    for i = 0 : deg - 1
                        im = ccall((:acb_imag_ptr, libarb), Ptr{arb_struct},
                            (Ptr{ComplexFieldElem}, ), roots + i * sizeof(acb_struct))
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

@doc Markdown.doc"""
    roots_upper_bound(x::ComplexPoly) -> arb

Returns an upper bound for the absolute value of all complex roots of $x$.
"""
function roots_upper_bound(x::ComplexPoly)
   z = RealFieldElem()
   p = precision(Balls)
   GC.@preserve x z begin
      t = ccall((:arb_rad_ptr, libarb), Ptr{mag_struct}, (Ref{RealFieldElem}, ), z)
      ccall((:acb_poly_root_bound_fujiwara, libarb), Nothing,
            (Ptr{mag_struct}, Ref{ComplexPoly}), t, x)
      s = ccall((:arb_mid_ptr, libarb), Ptr{arf_struct}, (Ref{RealFieldElem}, ), z)
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

function zero!(z::ComplexPoly)
   ccall((:acb_poly_zero, libarb), Nothing, (Ref{ComplexPoly},), z)
   return z
end

function fit!(z::ComplexPoly, n::Int)
   ccall((:acb_poly_fit_length, libarb), Nothing,
                    (Ref{ComplexPoly}, Int), z, n)
   return nothing
end

function setcoeff!(z::ComplexPoly, n::Int, x::ZZRingElem)
   ccall((:acb_poly_set_coeff_fmpz, libarb), Nothing,
                    (Ref{ComplexPoly}, Int, Ref{ZZRingElem}), z, n, x)
   return z
end

function setcoeff!(z::ComplexPoly, n::Int, x::ComplexFieldElem)
   ccall((:acb_poly_set_coeff_acb, libarb), Nothing,
                    (Ref{ComplexPoly}, Int, Ref{ComplexFieldElem}), z, n, x)
   return z
end

function mul!(z::ComplexPoly, x::ComplexPoly, y::ComplexPoly)
   ccall((:acb_poly_mul, libarb), Nothing,
                (Ref{ComplexPoly}, Ref{ComplexPoly}, Ref{ComplexPoly}, Int),
                    z, x, y, precision(parent(z)))
   return z
end

function addeq!(z::ComplexPoly, x::ComplexPoly)
   ccall((:acb_poly_add, libarb), Nothing,
                (Ref{ComplexPoly}, Ref{ComplexPoly}, Ref{ComplexPoly}, Int),
                    z, z, x, precision(parent(z)))
   return z
end

function add!(z::ComplexPoly, x::ComplexPoly, y::ComplexPoly)
   ccall((:acb_poly_add, libarb), Nothing,
                (Ref{ComplexPoly}, Ref{ComplexPoly}, Ref{ComplexPoly}, Int),
                    z, x, y, precision(parent(z)))
   return z
end

###############################################################################
#
#   Promotions
#
###############################################################################

promote_rule(::Type{ComplexPoly}, ::Type{ZZPolyRingElem}) = ComplexPoly

promote_rule(::Type{ComplexPoly}, ::Type{QQPolyRingElem}) = ComplexPoly

promote_rule(::Type{ComplexPoly}, ::Type{arb_poly}) = ComplexPoly

function promote_rule(::Type{ComplexPoly}, ::Type{T}) where {T}
   return promote_rule(ComplexFieldElem, T) === ComplexFieldElem ? ComplexPoly : Union{}
end

################################################################################
#
#  Parent object call overloads
#
################################################################################

function (a::ComplexPolyRing)()
   z = ComplexPoly()
   z.parent = a
   return z
end

for T in [Integer, ZZRingElem, QQFieldElem, Float64, Complex{Float64},
          Complex{Int}, RealFieldElem, ComplexFieldElem]
  @eval begin
    function (a::ComplexPolyRing)(b::$T)
      z = ComplexPoly(base_ring(a)(b), precision(Balls))
      z.parent = a
      return z
    end
  end
end

(a::ComplexPolyRing)(b::Rational{T}) where {T <: Integer} = a(QQFieldElem(b))

function (a::ComplexPolyRing)(b::Vector{ComplexFieldElem})
   z = ComplexPoly(b, precision(Balls))
   z.parent = a
   return z
end

for T in [ZZRingElem, QQFieldElem, Float64, Complex{Float64}, Complex{Int}, RealFieldElem]
  @eval begin
    (a::ComplexPolyRing)(b::Vector{$T}) = a(map(base_ring(a), b))
  end
end

(a::ComplexPolyRing)(b::Vector{T}) where {T <: Integer} = a(map(base_ring(a), b))

(a::ComplexPolyRing)(b::Vector{Rational{T}}) where {T <: Integer} = a(map(base_ring(a), b))

function (a::ComplexPolyRing)(b::ZZPolyRingElem)
   z = ComplexPoly(b, precision(Balls))
   z.parent = a
   return z
end

function (a::ComplexPolyRing)(b::QQPolyRingElem)
   z = ComplexPoly(b, precision(Balls))
   z.parent = a
   return z
end

function (a::ComplexPolyRing)(b::RealPoly)
   z = ComplexPoly(b, precision(Balls))
   z.parent = a
   return z
end

function (a::ComplexPolyRing)(b::ComplexPoly)
   z = ComplexPoly(b, precision(Balls))
   z.parent = a
   return z
end

################################################################################
#
#  polynomial_ring constructor
#
################################################################################

function polynomial_ring(R::ComplexField, s::Symbol; cached = true)
  parent_obj = ComplexPolyRing(R, s, cached)
  return parent_obj, parent_obj(ZZPolyRingElem([ZZRingElem(0), ZZRingElem(1)]))
end

function polynomial_ring(R::ComplexField, s::AbstractString; cached = true)
   return polynomial_ring(R, Symbol(s); cached=cached)
end

function PolyRing(R::ComplexField)
   return ComplexPolyRing(R, :x, false)
end
