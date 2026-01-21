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

poly_type(::Type{AcbFieldElem}) = AcbPolyRingElem

length(x::AcbPolyRingElem) = x.length

function set_length!(x::AcbPolyRingElem, n::Int)
  @ccall libflint._acb_poly_set_length(x::Ref{AcbPolyRingElem}, n::Int)::Nothing
  return x
end

function coeff(a::AcbPolyRingElem, n::Int)
  n < 0 && throw(DomainError(n, "Index must be non-negative"))
  t = parent(a).base_ring()
  @ccall libflint.acb_poly_get_coeff_acb(t::Ref{AcbFieldElem}, a::Ref{AcbPolyRingElem}, n::Int)::Nothing
  return t
end

zero(a::AcbPolyRing) = a()

one(a::AcbPolyRing) = one!(a())

function gen(a::AcbPolyRing)
  z = AcbPolyRingElem()
  setcoeff!(z, 1, 1)
  z.parent = a
  return z
end

# todo: write a C function for this
function is_gen(a::AcbPolyRingElem)
  return isequal(a, gen(parent(a)))
end

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
  return @ccall libflint.acb_poly_equal(x::Ref{AcbPolyRingElem}, y::Ref{AcbPolyRingElem})::Bool
end

@doc raw"""
    overlaps(x::AcbPolyRingElem, y::AcbPolyRingElem)

Return `true` if the coefficient boxes of $x$ overlap the coefficient boxes
of $y$, otherwise return `false`.
"""
function overlaps(x::AcbPolyRingElem, y::AcbPolyRingElem)
  return @ccall libflint.acb_poly_overlaps(x::Ref{AcbPolyRingElem}, y::Ref{AcbPolyRingElem})::Bool
end

@doc raw"""
    contains(x::AcbPolyRingElem, y::AcbPolyRingElem)

Return `true` if the coefficient boxes of $x$ contain the corresponding
coefficient boxes of $y$, otherwise return `false`.
"""
function contains(x::AcbPolyRingElem, y::AcbPolyRingElem)
  return @ccall libflint.acb_poly_contains(x::Ref{AcbPolyRingElem}, y::Ref{AcbPolyRingElem})::Bool
end

@doc raw"""
    contains(x::AcbPolyRingElem, y::ZZPolyRingElem)

Return `true` if the coefficient boxes of $x$ contain the corresponding
exact coefficients of $y$, otherwise return `false`.
"""
function contains(x::AcbPolyRingElem, y::ZZPolyRingElem)
  return @ccall libflint.acb_poly_contains_fmpz_poly(x::Ref{AcbPolyRingElem}, y::Ref{ZZPolyRingElem})::Bool
end

@doc raw"""
    contains(x::AcbPolyRingElem, y::QQPolyRingElem)

Return `true` if the coefficient boxes of $x$ contain the corresponding
exact coefficients of $y$, otherwise return `false`.
"""
function contains(x::AcbPolyRingElem, y::QQPolyRingElem)
  return @ccall libflint.acb_poly_contains_fmpq_poly(x::Ref{AcbPolyRingElem}, y::Ref{QQPolyRingElem})::Bool
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
  unique = @ccall libflint.acb_poly_get_unique_fmpz_poly(z::Ref{ZZPolyRingElem}, x::Ref{AcbPolyRingElem})::Int
  return (unique != 0, z)
end

function isreal(x::AcbPolyRingElem)
  return Bool(@ccall libflint.acb_poly_is_real(x::Ref{AcbPolyRingElem})::Cint)
end

###############################################################################
#
#   Shifting
#
###############################################################################

function shift_left(x::AcbPolyRingElem, len::Int)
  len < 0 && throw(DomainError(len, "Shift must be non-negative"))
  z = parent(x)()
  @ccall libflint.acb_poly_shift_left(z::Ref{AcbPolyRingElem}, x::Ref{AcbPolyRingElem}, len::Int)::Nothing
  return z
end

function shift_right(x::AcbPolyRingElem, len::Int)
  len < 0 && throw(DomainError(len, "Shift must be non-negative"))
  z = parent(x)()
  @ccall libflint.acb_poly_shift_right(z::Ref{AcbPolyRingElem}, x::Ref{AcbPolyRingElem}, len::Int)::Nothing
  return z
end

################################################################################
#
#  Unary operations
#
################################################################################

-(x::AcbPolyRingElem) = neg!(parent(x)(), x)

################################################################################
#
#  Binary operations
#
################################################################################

function +(x::AcbPolyRingElem, y::AcbPolyRingElem)
  z = parent(x)()
  return add!(z, x, y)
end

function -(x::AcbPolyRingElem, y::AcbPolyRingElem)
  z = parent(x)()
  return sub!(z, x, y)
  return z
end

function *(x::AcbPolyRingElem, y::AcbPolyRingElem)
  z = parent(x)()
  return mul!(z, x, y)
end

function ^(x::AcbPolyRingElem, y::Int)
  y < 0 && throw(DomainError(y, "Exponent must be non-negative"))
  z = parent(x)()
  @ccall libflint.acb_poly_pow_ui(z::Ref{AcbPolyRingElem}, x::Ref{AcbPolyRingElem}, y::UInt, precision(parent(x))::Int)::Nothing
  return z
end

###############################################################################
#
#   Ad hoc binary operators
#
###############################################################################

# to avoid method ambiguity errors, include `AbstractFloat, Integer, Rational` in addition to `Real`
for T in [Union{AbstractFloat, Integer, Rational}, Union{Integer, Rational}, Real, Complex, ZZRingElem, QQFieldElem, ArbFieldElem, AcbFieldElem, ZZPolyRingElem, QQPolyRingElem]
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
for T in [Union{AbstractFloat, Integer, Rational}, Union{Integer, Rational}, Real, Complex, ZZRingElem, QQFieldElem, ArbFieldElem, AcbFieldElem]
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
  success = @ccall libflint.acb_poly_divrem(q::Ref{AcbPolyRingElem}, r::Ref{AcbPolyRingElem}, x::Ref{AcbPolyRingElem}, y::Ref{AcbPolyRingElem}, precision(parent(x))::Int)::Int
  if success == 1
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
  @ccall libflint.acb_poly_truncate(z::Ref{AcbPolyRingElem}, n::Int)::Nothing
  return z
end

function mullow(x::AcbPolyRingElem, y::AcbPolyRingElem, n::Int)
  n < 0 && throw(DomainError(n, "Index must be non-negative"))
  z = parent(x)()
  @ccall libflint.acb_poly_mullow(z::Ref{AcbPolyRingElem}, x::Ref{AcbPolyRingElem}, y::Ref{AcbPolyRingElem}, n::Int, precision(parent(x))::Int)::Nothing
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
#   @ccall libflint.acb_poly_reverse(z::Ref{AcbPolyRingElem}, x::Ref{AcbPolyRingElem}, len::Int)::Nothing
#   return z
#end

###############################################################################
#
#   Evaluation
#
###############################################################################

function evaluate(x::AcbPolyRingElem, y::AcbFieldElem)
  z = parent(y)()
  @ccall libflint.acb_poly_evaluate(z::Ref{AcbFieldElem}, x::Ref{AcbPolyRingElem}, y::Ref{AcbFieldElem}, precision(parent(y))::Int)::Nothing
  return z
end

evaluate(x::AcbPolyRingElem, y::RingElement) = evaluate(x, base_ring(parent(x))(y))
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
  @ccall libflint.acb_poly_evaluate2(z::Ref{AcbFieldElem}, w::Ref{AcbFieldElem}, x::Ref{AcbPolyRingElem}, y::Ref{AcbFieldElem}, precision(parent(y))::Int)::Nothing
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
  @ccall libflint.acb_poly_compose(z::Ref{AcbPolyRingElem}, x::Ref{AcbPolyRingElem}, y::Ref{AcbPolyRingElem}, precision(parent(x))::Int)::Nothing
  return z
end

###############################################################################
#
#   Derivative and integral
#
###############################################################################

function derivative(x::AcbPolyRingElem)
  z = parent(x)()
  @ccall libflint.acb_poly_derivative(z::Ref{AcbPolyRingElem}, x::Ref{AcbPolyRingElem}, precision(parent(x))::Int)::Nothing
  return z
end

function derivative(x::AcbPolyRingElem, n::Int64)
  # TODO: reimplement using _acb_poly_nth_derivative
  # and then do the equivalent for {Complex,Arb,Real}PolyRingElem
  for i in (1:n)
    x = derivative(x)
  end
  return x
end

function integral(x::AcbPolyRingElem)
  z = parent(x)()
  @ccall libflint.acb_poly_integral(z::Ref{AcbPolyRingElem}, x::Ref{AcbPolyRingElem}, precision(parent(x))::Int)::Nothing
  return z
end

###############################################################################
#
#   Multipoint evaluation and interpolation
#
###############################################################################

function acb_vec(b::Vector{AcbFieldElem})
  v = @ccall libflint._acb_vec_init(length(b)::Int)::Ptr{acb_struct}
  for i in 1:length(b)
    _acb_set(v + (i-1)*sizeof(acb_struct), b[i])
  end
  return v
end

function array(R::AcbField, v::Ptr{acb_struct}, n::Int)
  r = Vector{AcbFieldElem}(undef, n)
  for i in 1:n
    r[i] = R()
    _acb_set(r[i], v + (i-1)*sizeof(acb_struct))
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
  @ccall libflint.acb_poly_product_roots(z::Ref{AcbPolyRingElem}, tmp::Ptr{acb_struct}, length(b)::Int, precision(R)::Int)::Nothing
  acb_vec_clear(tmp, length(b))
  return z
end

function evaluate_iter(x::AcbPolyRingElem, b::Vector{AcbFieldElem})
  return AcbFieldElem[evaluate(x, b[i]) for i=1:length(b)]
end

function evaluate_fast(x::AcbPolyRingElem, b::Vector{AcbFieldElem})
  tmp = acb_vec(b)
  @ccall libflint.acb_poly_evaluate_vec_fast(tmp::Ptr{acb_struct}, x::Ref{AcbPolyRingElem}, tmp::Ptr{acb_struct}, length(b)::Int, precision(parent(x))::Int)::Nothing
  res = array(base_ring(parent(x)), tmp, length(b))
  acb_vec_clear(tmp, length(b))
  return res
end

function interpolate_newton(R::AcbPolyRing, xs::Vector{AcbFieldElem}, ys::Vector{AcbFieldElem})
  length(xs) != length(ys) && error()
  z = R()
  xsv = acb_vec(xs)
  ysv = acb_vec(ys)
  @ccall libflint.acb_poly_interpolate_newton(z::Ref{AcbPolyRingElem}, xsv::Ptr{acb_struct}, ysv::Ptr{acb_struct}, length(xs)::Int, precision(R)::Int)::Nothing
  acb_vec_clear(xsv, length(xs))
  acb_vec_clear(ysv, length(ys))
  return z
end

function interpolate_barycentric(R::AcbPolyRing, xs::Vector{AcbFieldElem}, ys::Vector{AcbFieldElem})
  length(xs) != length(ys) && error()
  z = R()
  xsv = acb_vec(xs)
  ysv = acb_vec(ys)
  @ccall libflint.acb_poly_interpolate_barycentric(z::Ref{AcbPolyRingElem}, xsv::Ptr{acb_struct}, ysv::Ptr{acb_struct}, length(xs)::Int, precision(R)::Int)::Nothing
  acb_vec_clear(xsv, length(xs))
  acb_vec_clear(ysv, length(ys))
  return z
end

function interpolate_fast(R::AcbPolyRing, xs::Vector{AcbFieldElem}, ys::Vector{AcbFieldElem})
  length(xs) != length(ys) && error()
  z = R()
  xsv = acb_vec(xs)
  ysv = acb_vec(ys)
  @ccall libflint.acb_poly_interpolate_fast(z::Ref{AcbPolyRingElem}, xsv::Ptr{acb_struct}, ysv::Ptr{acb_struct}, length(xs)::Int, precision(R)::Int)::Nothing
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
    return AcbFieldElem[]
  end

  initial_prec = (initial_prec >= 2) ? initial_prec : 32
  max_prec = (max_prec >= 2) ? max_prec : 3 * precision(parent(x))

  isolated = 0
  wp = initial_prec
  roots = acb_vec(deg)

  while true
    in_roots = (wp == initial_prec) ? C_NULL : roots
    step_max_iter = (max_iter >= 1) ? max_iter : min(max(deg, 32), wp)
    isolated = @ccall libflint.acb_poly_find_roots(roots::Ptr{acb_struct}, x::Ref{AcbPolyRingElem}, in_roots::Ptr{acb_struct}, step_max_iter::Int, wp::Int)::Int

    wp = wp * 2

    if isolated == deg
      ok = true
      if target > 0
        for i = 0 : deg-1
          re = _real_ptr(roots + i * sizeof(acb_struct))
          im = _imag_ptr(roots + i * sizeof(acb_struct))
          t = _rad_ptr(re)
          u = _rad_ptr(im)
          ok = ok && (@ccall libflint.mag_cmp_2exp_si(t::Ptr{mag_struct}, (-target)::Int)::Cint) <= 0
          ok = ok && (@ccall libflint.mag_cmp_2exp_si(u::Ptr{mag_struct}, (-target)::Int)::Cint) <= 0
        end
      end

      if isreal(x)
        real_ok = @ccall libflint.acb_poly_validate_real_roots(roots::Ptr{acb_struct}, x::Ref{AcbPolyRingElem}, wp::Int)::Bool

        if isolate_real && !real_ok
          ok = false
        end

        if real_ok
          for i = 0 : deg - 1
            im = _imag_ptr(roots + i * sizeof(acb_struct))
            if @ccall libflint.arb_contains_zero(im::Ptr{arb_struct})::Bool
              @ccall libflint.arb_zero(im::Ptr{arb_struct})::Nothing
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
    @ccall libflint._acb_vec_sort_pretty(roots::Ptr{acb_struct}, deg::Int)::Nothing
    res = array(base_ring(parent(x)), roots, deg)
    acb_vec_clear(roots, deg)
    return res
  else
    acb_vec_clear(roots, deg)
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
    t = _rad_ptr(z)
    @ccall libflint.acb_poly_root_bound_fujiwara(t::Ptr{mag_struct}, x::Ref{AcbPolyRingElem})::Nothing
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

function zero!(z::AcbPolyRingElemOrPtr)
  @ccall libflint.acb_poly_zero(z::Ref{AcbPolyRingElem})::Nothing
  return z
end

function one!(z::AcbPolyRingElemOrPtr)
  @ccall libflint.acb_poly_one(z::Ref{AcbPolyRingElem})::Nothing
  return z
end

function neg!(z::AcbPolyRingElemOrPtr, a::AcbPolyRingElemOrPtr)
  @ccall libflint.acb_poly_neg(z::Ref{AcbPolyRingElem}, a::Ref{AcbPolyRingElem})::Nothing
  return z
end

function fit!(z::AcbPolyRingElem, n::Int)
  @ccall libflint.acb_poly_fit_length(z::Ref{AcbPolyRingElem}, n::Int)::Nothing
  return nothing
end

function setcoeff!(z::AcbPolyRingElem, n::Int, x::AcbFieldElem)
  @ccall libflint.acb_poly_set_coeff_acb(z::Ref{AcbPolyRingElem}, n::Int, x::Ref{AcbFieldElem})::Nothing
  return z
end

function setcoeff!(z::AcbPolyRingElem, n::Int, x::Int)
  @ccall libflint.acb_poly_set_coeff_si(z::Ref{AcbPolyRingElem}, n::Int, x::Int)::Nothing
  return z
end

function setcoeff!(z::AcbPolyRingElem, n::Int, x::ZZRingElem)
  return setcoeff!(z, n, base_ring(z)(x))
end

setcoeff!(z::AcbPolyRingElem, n::Int, x::Integer) = setcoeff!(z, n, flintify(x))

#

function add!(z::AcbPolyRingElem, x::AcbPolyRingElem, y::AcbPolyRingElem)
  @ccall libflint.acb_poly_add(z::Ref{AcbPolyRingElem}, x::Ref{AcbPolyRingElem}, y::Ref{AcbPolyRingElem}, precision(parent(z))::Int)::Nothing
  return z
end

function add!(z::AcbPolyRingElem, x::AcbPolyRingElem, y::Int)
  @ccall libflint.acb_poly_add_si(z::Ref{AcbPolyRingElem}, x::Ref{AcbPolyRingElem}, y::Int, precision(parent(z))::Int)::Nothing
  return z
end

add!(z::AcbPolyRingElem, x::AcbPolyRingElem, y::AcbFieldElem) = add!(z, x, parent(z)(y))

add!(z::AcbPolyRingElem, x::AcbPolyRingElem, y::ZZRingElem) = add!(z, x, parent(z)(y))

add!(z::AcbPolyRingElem, x::AcbPolyRingElem, y::Integer) = add!(z, x, flintify(y))

add!(z::AcbPolyRingElem, x::Union{AcbFieldElem,IntegerUnion}, y::AcbPolyRingElem) = add!(z, y, x)

#

function sub!(z::AcbPolyRingElem, x::AcbPolyRingElem, y::AcbPolyRingElem)
  @ccall libflint.acb_poly_sub(z::Ref{AcbPolyRingElem}, x::Ref{AcbPolyRingElem}, y::Ref{AcbPolyRingElem}, precision(parent(z))::Int)::Nothing
  return z
end

sub!(z::AcbPolyRingElem, x::AcbPolyRingElem, y::Union{AcbFieldElem,IntegerUnion}) = sub!(z, x, parent(z)(y))

sub!(z::AcbPolyRingElem, x::Union{AcbFieldElem,IntegerUnion}, y::AcbPolyRingElem) = sub!(z, parent(z)(x), y)

#

function mul!(z::AcbPolyRingElem, x::AcbPolyRingElem, y::AcbPolyRingElem)
  @ccall libflint.acb_poly_mul(z::Ref{AcbPolyRingElem}, x::Ref{AcbPolyRingElem}, y::Ref{AcbPolyRingElem}, precision(parent(z))::Int)::Nothing
  return z
end

function mul!(z::AcbPolyRingElem, x::AcbPolyRingElem, y::AcbFieldElem)
  @ccall libflint.acb_poly_scalar_mul(z::Ref{AcbPolyRingElem}, x::Ref{AcbPolyRingElem}, y::Ref{AcbFieldElem}, precision(parent(z))::Int)::Nothing
  return z
end

mul!(z::AcbPolyRingElem, x::AcbPolyRingElem, y::IntegerUnion) = mul!(z, x, base_ring(z)(y))

mul!(z::AcbPolyRingElem, x::Union{AcbFieldElem,IntegerUnion}, y::AcbPolyRingElem) = mul!(z, y, x)

#

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
