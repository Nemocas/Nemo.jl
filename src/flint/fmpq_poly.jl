###############################################################################
#
#   QQPolyRingElem.jl : Flint polynomials over QQFieldElem
#
###############################################################################

###############################################################################
#
#   Data type and parent methods
#
###############################################################################

parent_type(::Type{QQPolyRingElem}) = QQPolyRing

elem_type(::Type{QQPolyRing}) = QQPolyRingElem

dense_poly_type(::Type{QQFieldElem}) = QQPolyRingElem

base_ring(a::QQPolyRing) = QQ

parent(a::QQPolyRingElem) = a.parent

var(a::QQPolyRing) = a.S

###############################################################################
#
#   Basic manipulation
#
###############################################################################

@doc raw"""
    denominator(a::QQPolyRingElem)

Return the least common denominator of the coefficients of the polynomial
$a$.
"""
function denominator(a::QQPolyRingElem)
  z = ZZRingElem()
  ccall((:fmpq_poly_get_denominator, libflint), Nothing,
        (Ref{ZZRingElem}, Ref{QQPolyRingElem}), z, a)
  return z
end

length(x::QQPolyRingElem) = ccall((:fmpq_poly_length, libflint), Int,
                                  (Ref{QQPolyRingElem},), x)

function set_length!(x::QQPolyRingElem, n::Int)
  ccall((:_fmpq_poly_set_length, libflint), Nothing,
        (Ref{QQPolyRingElem}, Int), x, n)
  return x
end

function coeff(x::QQPolyRingElem, n::Int)
  n < 0 && throw(DomainError(n, "Index must be non-negative"))
  z = QQFieldElem()
  ccall((:fmpq_poly_get_coeff_fmpq, libflint), Nothing,
        (Ref{QQFieldElem}, Ref{QQPolyRingElem}, Int), z, x, n)
  return z
end

zero(a::QQPolyRing) = a(0)

one(a::QQPolyRing) = a(1)

gen(a::QQPolyRing) = a([zero(base_ring(a)), one(base_ring(a))])

is_gen(x::QQPolyRingElem) = ccall((:fmpq_poly_is_gen, libflint), Bool,
                                  (Ref{QQPolyRingElem},), x)

function deepcopy_internal(a::QQPolyRingElem, dict::IdDict)
  z = QQPolyRingElem(a)
  z.parent = parent(a)
  return z
end

Base.copy(f::QQPolyRingElem) = parent(f)(f)

###############################################################################
#
#   Similar
#
###############################################################################

function similar(f::PolyRingElem, R::QQField, s::Symbol=var(parent(f)); cached::Bool=true)
  z = QQPolyRingElem()
  if base_ring(f) === R && s == var(parent(f)) && f isa QQPolyRingElem
    # steal parent in case it is not cached
    z.parent = parent(f)
  else
    z.parent = QQPolyRing(R, s, cached)
  end
  return z
end

###############################################################################
#
#   polynomial constructor
#
###############################################################################

function polynomial(R::QQField, arr::Vector{T}, var::VarName=:x; cached::Bool=true) where T
  coeffs = T == QQFieldElem ? arr : map(R, arr)
  coeffs = length(coeffs) == 0 ? QQFieldElem[] : coeffs
  z = QQPolyRingElem(coeffs)
  z.parent = QQPolyRing(R, Symbol(var), cached)
  return z
end

###############################################################################
#
#   Canonicalisation
#
###############################################################################

canonical_unit(a::QQPolyRingElem) = canonical_unit(leading_coefficient(a))

###############################################################################
#
#   Unary operations
#
###############################################################################

-(x::QQPolyRingElem) = neg!(parent(x)(), x)

###############################################################################
#
#   Binary operations
#
###############################################################################

function +(x::QQPolyRingElem, y::QQPolyRingElem)
  check_parent(x, y)
  z = parent(x)()
  return add!(z, x, y)
end

function -(x::QQPolyRingElem, y::QQPolyRingElem)
  check_parent(x, y)
  z = parent(x)()
  return sub!(z, x, y)
end

function *(x::QQPolyRingElem, y::QQPolyRingElem)
  check_parent(x, y)
  z = parent(x)()
  return mul!(z, x, y)
end

###############################################################################
#
#   Ad hoc binary operators
#
###############################################################################

for T in [Integer, ZZRingElem, Rational, QQFieldElem]
  for (jop, cop) in ((:+,:add!), (:-,:sub!), (:*,:mul!))
    @eval begin
      $jop(a::QQPolyRingElem, b::$T) = $cop(similar(a), a, b)
      $jop(a::$T, b::QQPolyRingElem) = $cop(similar(b), a, b)
    end
  end
end

###############################################################################
#
#   Powering
#
###############################################################################

function ^(x::QQPolyRingElem, y::Int)
  y < 0 && throw(DomainError(y, "Exponent must be non-negative"))
  z = parent(x)()
  ccall((:fmpq_poly_pow, libflint), Nothing,
        (Ref{QQPolyRingElem}, Ref{QQPolyRingElem}, Int),
        z, x, y)
  return z
end

###############################################################################
#
#   Comparisons
#
###############################################################################

function ==(x::QQPolyRingElem, y::QQPolyRingElem)
  check_parent(x, y)
  return ccall((:fmpq_poly_equal, libflint), Bool,
               (Ref{QQPolyRingElem}, Ref{QQPolyRingElem}), x, y)
end

###############################################################################
#
#   Ad hoc comparisons
#
###############################################################################

function ==(x::QQPolyRingElem, y::QQFieldElem)
  if length(x) > 1
    return false
  elseif length(x) == 1
    z = QQFieldElem()
    ccall((:fmpq_poly_get_coeff_fmpq, libflint), Nothing,
          (Ref{QQFieldElem}, Ref{QQPolyRingElem}, Int), z, x, 0)
    return ccall((:fmpq_equal, libflint), Bool,
                 (Ref{QQFieldElem}, Ref{QQFieldElem}, Int), z, y, 0)
  else
    return iszero(y)
  end
end

==(x::QQFieldElem, y::QQPolyRingElem) = y == x

==(x::QQPolyRingElem, y::Rational{T}) where T <: Union{Int, BigInt} = x == QQFieldElem(y)

==(x::Rational{T}, y::QQPolyRingElem) where T <: Union{Int, BigInt} = y == x

###############################################################################
#
#   Truncation
#
###############################################################################

function truncate(a::QQPolyRingElem, n::Int)
  n < 0 && throw(DomainError(n, "Index must be non-negative"))

  if length(a) <= n
    return a
  end

  z = parent(a)()
  ccall((:fmpq_poly_set_trunc, libflint), Nothing,
        (Ref{QQPolyRingElem}, Ref{QQPolyRingElem}, Int), z, a, n)
  return z
end

function mullow(x::QQPolyRingElem, y::QQPolyRingElem, n::Int)
  check_parent(x, y)
  n < 0 && throw(DomainError(n, "Index must be non-negative"))

  z = parent(x)()
  ccall((:fmpq_poly_mullow, libflint), Nothing,
        (Ref{QQPolyRingElem}, Ref{QQPolyRingElem}, Ref{QQPolyRingElem}, Int), z, x, y, n)
  return z
end

###############################################################################
#
#   Reversal
#
###############################################################################

function reverse(x::QQPolyRingElem, len::Int)
  len < 0 && throw(DomainError(len, "Length must be non-negative"))
  z = parent(x)()
  ccall((:fmpq_poly_reverse, libflint), Nothing,
        (Ref{QQPolyRingElem}, Ref{QQPolyRingElem}, Int), z, x, len)
  return z
end

###############################################################################
#
#   Shifting
#
###############################################################################

function shift_left(x::QQPolyRingElem, len::Int)
  len < 0 && throw(DomainError(len, "Shift must be non-negative"))
  z = parent(x)()
  ccall((:fmpq_poly_shift_left, libflint), Nothing,
        (Ref{QQPolyRingElem}, Ref{QQPolyRingElem}, Int), z, x, len)
  return z
end

function shift_right(x::QQPolyRingElem, len::Int)
  len < 0 && throw(DomainError(len, "Shift must be non-negative"))
  z = parent(x)()
  ccall((:fmpq_poly_shift_right, libflint), Nothing,
        (Ref{QQPolyRingElem}, Ref{QQPolyRingElem}, Int), z, x, len)
  return z
end

###############################################################################
#
#   Euclidean division
#
###############################################################################

function mod(x::QQPolyRingElem, y::QQPolyRingElem)
  check_parent(x, y)
  iszero(y) && throw(DivideError())
  r = parent(x)()
  ccall((:fmpq_poly_rem, libflint), Nothing,
        (Ref{QQPolyRingElem}, Ref{QQPolyRingElem}, Ref{QQPolyRingElem}),
        r, x, y)
  return r
end

rem(x::QQPolyRingElem, y::QQPolyRingElem) = mod(x, y)

function Base.divrem(x::QQPolyRingElem, y::QQPolyRingElem)
  check_parent(x, y)
  iszero(y) && throw(DivideError())
  q = parent(x)()
  r = parent(x)()
  ccall((:fmpq_poly_divrem, libflint), Nothing,
        (Ref{QQPolyRingElem}, Ref{QQPolyRingElem}, Ref{QQPolyRingElem}, Ref{QQPolyRingElem}),
        q, r, x, y)
  return q, r
end

###############################################################################
#
#   Exact division
#
###############################################################################

function Base.div(x::QQPolyRingElem, y::QQPolyRingElem)
  check_parent(x, y)
  iszero(y) && throw(DivideError())
  z = parent(x)()
  ccall((:fmpq_poly_div, libflint), Nothing,
        (Ref{QQPolyRingElem}, Ref{QQPolyRingElem}, Ref{QQPolyRingElem}), z, x, y)
  return z
end

function divexact(x::QQPolyRingElem, y::QQPolyRingElem; check::Bool=true)
  if !check
    return div(x, y)
  else
    q, r = divrem(x, y)
    !iszero(r) && throw(ArgumentError("not an exact division"))
    return q
  end
end

###############################################################################
#
#   Ad hoc exact division
#
###############################################################################

function divexact(x::QQPolyRingElem, y::ZZRingElem; check::Bool=true)
  iszero(y) && throw(DivideError())
  z = parent(x)()
  ccall((:fmpq_poly_scalar_div_fmpz, libflint), Nothing,
        (Ref{QQPolyRingElem}, Ref{QQPolyRingElem}, Ref{ZZRingElem}), z, x, y)
  return z
end

function divexact(x::QQPolyRingElem, y::QQFieldElem; check::Bool=true)
  iszero(y) && throw(DivideError())
  z = parent(x)()
  ccall((:fmpq_poly_scalar_div_fmpq, libflint), Nothing,
        (Ref{QQPolyRingElem}, Ref{QQPolyRingElem}, Ref{QQFieldElem}), z, x, y)
  return z
end

function divexact(x::QQPolyRingElem, y::Int; check::Bool=true)
  y == 0 && throw(DivideError())
  z = parent(x)()
  ccall((:fmpq_poly_scalar_div_si, libflint), Nothing,
        (Ref{QQPolyRingElem}, Ref{QQPolyRingElem}, Int), z, x, y)
  return z
end

divexact(x::QQPolyRingElem, y::Integer; check::Bool=true) = divexact(x, flintify(y); check=check)

divexact(x::QQPolyRingElem, y::Rational{T}; check::Bool=true) where T <: Union{Int, BigInt} = divexact(x, QQFieldElem(y); check=check)

###############################################################################
#
#   Removal and valuation
#
###############################################################################

function divides(z::QQPolyRingElem, x::QQPolyRingElem)
  if iszero(z)
    return true, zero(parent(z))
  end
  if iszero(x)
    return false, zero(parent(z))
  end
  q, r = divrem(z, x)
  return iszero(r), q
end

###############################################################################
#
#   Content, primitive part, GCD and LCM
#
###############################################################################

function gcd(x::QQPolyRingElem, y::QQPolyRingElem)
  check_parent(x, y)
  z = parent(x)()
  ccall((:fmpq_poly_gcd, libflint), Nothing,
        (Ref{QQPolyRingElem}, Ref{QQPolyRingElem}, Ref{QQPolyRingElem}), z, x, y)
  return z
end

function content(x::QQPolyRingElem)
  z = QQFieldElem()
  ccall((:fmpq_poly_content, libflint), Nothing,
        (Ref{QQFieldElem}, Ref{QQPolyRingElem}), z, x)
  return z
end

function primpart(x::QQPolyRingElem)
  z = parent(x)()
  ccall((:fmpq_poly_primitive_part, libflint), Nothing,
        (Ref{QQPolyRingElem}, Ref{QQPolyRingElem}), z, x)
  return z
end

###############################################################################
#
#   Evaluation
#
###############################################################################

function evaluate(x::QQPolyRingElem, y::ZZRingElem)
  z = QQFieldElem()
  ccall((:fmpq_poly_evaluate_fmpz, libflint), Nothing,
        (Ref{QQFieldElem}, Ref{QQPolyRingElem}, Ref{ZZRingElem}), z, x, y)
  return z
end

function evaluate(x::QQPolyRingElem, y::QQFieldElem)
  z = QQFieldElem()
  ccall((:fmpq_poly_evaluate_fmpq, libflint), Nothing,
        (Ref{QQFieldElem}, Ref{QQPolyRingElem}, Ref{QQFieldElem}), z, x, y)
  return z
end

evaluate(x::QQPolyRingElem, y::Integer) = evaluate(x, ZZRingElem(y))

evaluate(x::QQPolyRingElem, y::Rational) = evaluate(x, QQFieldElem(y))

###############################################################################
#
#   Composition
#
###############################################################################

function AbstractAlgebra._compose_right(x::QQPolyRingElem, y::QQPolyRingElem)
  check_parent(x, y)
  z = parent(x)()
  ccall((:fmpq_poly_compose, libflint), Nothing,
        (Ref{QQPolyRingElem}, Ref{QQPolyRingElem}, Ref{QQPolyRingElem}), z, x, y)
  return z
end

###############################################################################
#
#   Derivative
#
###############################################################################

function derivative(x::QQPolyRingElem)
  z = parent(x)()
  ccall((:fmpq_poly_derivative, libflint), Nothing,
        (Ref{QQPolyRingElem}, Ref{QQPolyRingElem}), z, x)
  return z
end

###############################################################################
#
#   Integral
#
###############################################################################

function integral(x::QQPolyRingElem)
  z = parent(x)()
  ccall((:fmpq_poly_integral, libflint), Nothing,
        (Ref{QQPolyRingElem}, Ref{QQPolyRingElem}), z, x)
  return z
end

###############################################################################
#
#   Resultant
#
###############################################################################

function resultant(x::QQPolyRingElem, y::QQPolyRingElem)
  check_parent(x, y)
  z = QQFieldElem()
  ccall((:fmpq_poly_resultant, libflint), Nothing,
        (Ref{QQFieldElem}, Ref{QQPolyRingElem}, Ref{QQPolyRingElem}), z, x, y)
  return z
end

###############################################################################
#
#   GCDX
#
###############################################################################

function gcdx(x::QQPolyRingElem, y::QQPolyRingElem)
  check_parent(x, y)
  z = parent(x)()
  u = parent(x)()
  v = parent(x)()
  ccall((:fmpq_poly_xgcd, libflint), Nothing,
        (Ref{QQPolyRingElem}, Ref{QQPolyRingElem}, Ref{QQPolyRingElem}, Ref{QQPolyRingElem},
         Ref{QQPolyRingElem}), z, u, v, x, y)
  return (z, u, v)
end

###############################################################################
#
#   Square root
#
###############################################################################

function sqrt(x::QQPolyRingElem; check::Bool=true)
  R = parent(x)
  d = denominator(x)
  sd = sqrt(d; check=check)
  n = polynomial(ZZ, [], cached = false)
  ccall((:fmpq_poly_get_numerator, libflint), Nothing,
        (Ref{ZZPolyRingElem}, Ref{QQPolyRingElem}), n, x)
  sn = sqrt(n; check=check)
  s = R(sn)
  return divexact(s, sd)
end

function is_square(x::QQPolyRingElem)
  d = denominator(x)
  if !is_square(d)
    return false
  end
  n = polynomial(ZZ, [])
  ccall((:fmpq_poly_get_numerator, libflint), Nothing,
        (Ref{ZZPolyRingElem}, Ref{QQPolyRingElem}), n, x)
  if !is_square(n)
    return false
  end
  return true
end

function is_square_with_sqrt(x::QQPolyRingElem)
  R = parent(x)
  d = denominator(x)
  f1, s1 = is_square_with_sqrt(d)
  if !f1
    return false, zero(R)
  end
  n = polynomial(ZZ, [])
  ccall((:fmpq_poly_get_numerator, libflint), Nothing,
        (Ref{ZZPolyRingElem}, Ref{QQPolyRingElem}), n, x)
  f2, s2 = is_square_with_sqrt(n)
  if !f2
    return false, zero(R)
  end
  s = R(s2)
  return true, divexact(s, s1)
end

################################################################################
#
#   Factorization
#
################################################################################

for (factor_fn, factor_fn_inner, flint_fn) in
  [(:factor, :_factor, "fmpz_poly_factor"),
   (:factor_squarefree, :_factor_squarefree, "fmpz_poly_factor_squarefree")]
  eval(quote

         function $factor_fn(x::QQPolyRingElem)
           iszero(x) && throw(ArgumentError("Argument must be non-zero"))
           res, z = $factor_fn_inner(x)
           return Fac(parent(x)(z), res)
         end

         function $factor_fn_inner(x::QQPolyRingElem)
           res = Dict{QQPolyRingElem, Int}()
           y = ZZPolyRingElem()
           ccall((:fmpq_poly_get_numerator, libflint), Nothing,
                 (Ref{ZZPolyRingElem}, Ref{QQPolyRingElem}), y, x)
           fac = fmpz_poly_factor()
           ccall(($flint_fn, libflint), Nothing,
                 (Ref{fmpz_poly_factor}, Ref{ZZPolyRingElem}), fac, y)
           z = ZZRingElem()
           ccall((:fmpz_poly_factor_get_fmpz, libflint), Nothing,
                 (Ref{ZZRingElem}, Ref{fmpz_poly_factor}), z, fac)
           f = ZZPolyRingElem()
           for i in 1:fac.num
             ccall((:fmpz_poly_factor_get_fmpz_poly, libflint), Nothing,
                   (Ref{ZZPolyRingElem}, Ref{fmpz_poly_factor}, Int), f, fac, i - 1)
             e = unsafe_load(fac.exp, i)
             res[parent(x)(f)] = e
           end
           return res, QQFieldElem(z, denominator(x))
         end

       end)
end

function is_irreducible(x::QQPolyRingElem)
  res, _ = _factor(x)
  return length(res) == 1 && first(values(res)) == 1
end

###############################################################################
#
#   Signature
#
###############################################################################

@doc raw"""
    signature(f::QQPolyRingElem)

Return the signature of $f$, i.e. a tuple $(r, s)$ such that $r$ is the number of
real roots of $f$ and $s$ is half the number of complex roots.

# Examples

```jldoctest
julia> R, x = polynomial_ring(QQ, "x");

julia> signature(x^3 + 3x + 1)
(1, 1)
```
"""
function signature(f::QQPolyRingElem)
  z = ZZPolyRingElem()
  ccall((:fmpq_poly_get_numerator, libflint), Nothing,
        (Ref{ZZPolyRingElem}, Ref{QQPolyRingElem}), z, f)
  return signature(z)
end

###############################################################################
#
#   Speedups for polynomials over fmpq_polys
#
###############################################################################

function *(a::Generic.Poly{QQPolyRingElem}, b::Generic.Poly{QQPolyRingElem})
  check_parent(a, b)
  if min(length(a), length(b)) < 40
    return mul_classical(a, b)
  else
    return mul_ks(a, b)
  end
end

###############################################################################
#
#   Unsafe functions
#
###############################################################################

function zero!(z::QQPolyRingElemOrPtr)
  @ccall libflint.fmpq_poly_zero(z::Ref{QQPolyRingElem})::Nothing
  return z
end

function one!(z::QQPolyRingElemOrPtr)
  @ccall libflint.fmpq_poly_one(z::Ref{QQPolyRingElem})::Nothing
  return z
end

function neg!(z::QQPolyRingElemOrPtr, a::QQPolyRingElemOrPtr)
  @ccall libflint.fmpq_poly_neg(z::Ref{QQPolyRingElem}, a::Ref{QQPolyRingElem})::Nothing
  return z
end

function fit!(z::QQPolyRingElemOrPtr, n::Int)
  @ccall libflint.fmpq_poly_fit_length(z::Ref{QQPolyRingElem}, n::Int)::Nothing
  return nothing
end

#

function set!(z::QQPolyRingElemOrPtr, a::QQPolyRingElemOrPtr)
  @ccall libflint.fmpq_poly_set(z::Ref{QQPolyRingElem}, a::Ref{QQPolyRingElem})::Nothing
  return z
end

function set!(z::QQPolyRingElemOrPtr, a::ZZPolyRingElemOrPtr)
  @ccall libflint.fmpq_poly_set_fmpz_poly(z::Ref{QQPolyRingElem}, a::Ref{ZZPolyRingElem})::Nothing
  return z
end

function set!(z::QQPolyRingElemOrPtr, a::QQFieldElemOrPtr)
  @ccall libflint.fmpq_poly_set_fmpq(z::Ref{QQPolyRingElem}, a::Ref{QQFieldElem})::Nothing
  return z
end

function set!(z::QQPolyRingElemOrPtr, a::ZZRingElemOrPtr)
  @ccall libflint.fmpq_poly_set_fmpz(z::Ref{QQPolyRingElem}, a::Ref{ZZRingElem})::Nothing
  return z
end

function set!(z::QQPolyRingElemOrPtr, a::Int)
  @ccall libflint.fmpq_poly_set_si(z::Ref{QQPolyRingElem}, a::Int)::Nothing
  return z
end

function set!(z::QQPolyRingElemOrPtr, a::UInt)
  @ccall libflint.fmpq_poly_set_ui(z::Ref{QQPolyRingElem}, a::UInt)::Nothing
  return z
end

set!(z::QQPolyRingElemOrPtr, a::Union{Integer, Rational}) = set!(z, flintify(a))

#

function setcoeff!(z::QQPolyRingElemOrPtr, n::Int, x::ZZRingElemOrPtr)
  @ccall libflint.fmpq_poly_set_coeff_fmpz(z::Ref{QQPolyRingElem}, n::Int, x::Ref{ZZRingElem})::Nothing
  return z
end

function setcoeff!(z::QQPolyRingElemOrPtr, n::Int, x::QQFieldElemOrPtr)
  @ccall libflint.fmpq_poly_set_coeff_fmpq(z::Ref{QQPolyRingElem}, n::Int, x::Ref{QQFieldElem})::Nothing
  return z
end

function setcoeff!(z::QQPolyRingElemOrPtr, n::Int, x::UInt)
  @ccall libflint.fmpq_poly_set_coeff_ui(z::Ref{QQPolyRingElem}, n::Int, x::UInt)::Nothing
  return z
end

function setcoeff!(z::QQPolyRingElemOrPtr, n::Int, x::Int)
  @ccall libflint.fmpq_poly_set_coeff_si(z::Ref{QQPolyRingElem}, n::Int, x::Int)::Nothing
  return z
end

setcoeff!(z::QQPolyRingElemOrPtr, n::Int, x::Union{Integer, Rational}) = setcoeff!(z, n, flintify(x))

#

function add!(z::QQPolyRingElemOrPtr, x::QQPolyRingElemOrPtr, y::QQPolyRingElemOrPtr)
  @ccall libflint.fmpq_poly_add(z::Ref{QQPolyRingElem}, x::Ref{QQPolyRingElem}, y::Ref{QQPolyRingElem})::Nothing
  return z
end

function add!(z::QQPolyRingElemOrPtr, x::QQPolyRingElemOrPtr, y::QQFieldElemOrPtr)
  @ccall libflint.fmpq_poly_add_fmpq(z::Ref{QQPolyRingElem}, x::Ref{QQPolyRingElem}, y::Ref{QQFieldElem})::Nothing
  return z
end

function add!(z::QQPolyRingElemOrPtr, x::QQPolyRingElemOrPtr, y::ZZRingElemOrPtr)
  @ccall libflint.fmpq_poly_add_fmpz(z::Ref{QQPolyRingElem}, x::Ref{QQPolyRingElem}, y::Ref{ZZRingElem})::Nothing
  return z
end

function add!(z::QQPolyRingElemOrPtr, x::QQPolyRingElemOrPtr, y::Int)
  @ccall libflint.fmpq_poly_add_si(z::Ref{QQPolyRingElem}, x::Ref{QQPolyRingElem}, y::Int)::Nothing
  return z
end

add!(z::QQPolyRingElemOrPtr, x::QQPolyRingElemOrPtr, y::Union{Integer, Rational}) = add!(z, x, flintify(y))

add!(z::QQPolyRingElemOrPtr, x::RationalUnionOrPtr, y::QQPolyRingElemOrPtr) = add!(z, y, x)

#

function sub!(z::QQPolyRingElemOrPtr, x::QQPolyRingElemOrPtr, y::QQPolyRingElemOrPtr)
  @ccall libflint.fmpq_poly_sub(z::Ref{QQPolyRingElem}, x::Ref{QQPolyRingElem}, y::Ref{QQPolyRingElem})::Nothing
  return z
end


function sub!(z::QQPolyRingElemOrPtr, x::QQPolyRingElemOrPtr, y::QQFieldElemOrPtr)
  @ccall libflint.fmpq_poly_sub_fmpq(z::Ref{QQPolyRingElem}, x::Ref{QQPolyRingElem}, y::Ref{QQFieldElem})::Nothing
  return z
end

function sub!(z::QQPolyRingElemOrPtr, x::QQPolyRingElemOrPtr, y::ZZRingElemOrPtr)
  @ccall libflint.fmpq_poly_sub_fmpz(z::Ref{QQPolyRingElem}, x::Ref{QQPolyRingElem}, y::Ref{ZZRingElem})::Nothing
  return z
end

function sub!(z::QQPolyRingElemOrPtr, x::QQPolyRingElemOrPtr, y::Int)
  @ccall libflint.fmpq_poly_sub_si(z::Ref{QQPolyRingElem}, x::Ref{QQPolyRingElem}, y::Int)::Nothing
  return z
end

function sub!(z::QQPolyRingElemOrPtr, x::QQFieldElemOrPtr, y::QQPolyRingElemOrPtr)
  @ccall libflint.fmpq_poly_fmpq_sub(z::Ref{QQPolyRingElem}, x::Ref{QQFieldElem}, y::Ref{QQPolyRingElem})::Nothing
  return z
end

function sub!(z::QQPolyRingElemOrPtr, x::ZZRingElemOrPtr, y::QQPolyRingElemOrPtr)
  @ccall libflint.fmpq_poly_fmpz_sub(z::Ref{QQPolyRingElem}, x::Ref{ZZRingElem}, y::Ref{QQPolyRingElem})::Nothing
  return z
end

function sub!(z::QQPolyRingElemOrPtr, x::Int, y::QQPolyRingElemOrPtr)
  @ccall libflint.fmpq_poly_si_sub(z::Ref{QQPolyRingElem}, x::Int, y::Ref{QQPolyRingElem})::Nothing
  return z
end

sub!(z::QQPolyRingElemOrPtr, x::QQPolyRingElemOrPtr, y::Union{Integer, Rational}) = sub!(z, x, flintify(y))

sub!(z::QQPolyRingElemOrPtr, x::Union{Integer, Rational}, y::QQPolyRingElemOrPtr) = sub!(z, flintify(x), y)

#

function mul!(z::QQPolyRingElemOrPtr, x::QQPolyRingElemOrPtr, y::QQPolyRingElemOrPtr)
  @ccall libflint.fmpq_poly_mul(z::Ref{QQPolyRingElem}, x::Ref{QQPolyRingElem}, y::Ref{QQPolyRingElem})::Nothing
  return z
end

function mul!(z::QQPolyRingElemOrPtr, x::QQPolyRingElemOrPtr, y::QQFieldElemOrPtr)
  @ccall libflint.fmpq_poly_scalar_mul_fmpq(z::Ref{QQPolyRingElem}, x::Ref{QQPolyRingElem}, y::Ref{QQFieldElem})::Nothing
  return z
end

function mul!(z::QQPolyRingElemOrPtr, x::QQPolyRingElemOrPtr, y::ZZRingElemOrPtr)
  @ccall libflint.fmpq_poly_scalar_mul_fmpz(z::Ref{QQPolyRingElem}, x::Ref{QQPolyRingElem}, y::Ref{ZZRingElem})::Nothing
  return z
end

function mul!(z::QQPolyRingElemOrPtr, x::QQPolyRingElemOrPtr, y::Int)
  @ccall libflint.fmpq_poly_scalar_mul_si(z::Ref{QQPolyRingElem}, x::Ref{QQPolyRingElem}, y::Int)::Nothing
  return z
end

mul!(z::QQPolyRingElemOrPtr, x::QQPolyRingElemOrPtr, y::Union{Integer, Rational}) = mul!(z, x, flintify(y))

mul!(z::QQPolyRingElemOrPtr, x::RationalUnionOrPtr, y::QQPolyRingElemOrPtr) = mul!(z, y, x)

###############################################################################
#
#   Promotions
#
###############################################################################

promote_rule(::Type{QQPolyRingElem}, ::Type{T}) where {T <: Integer} = QQPolyRingElem

promote_rule(::Type{QQPolyRingElem}, ::Type{ZZRingElem}) = QQPolyRingElem

promote_rule(::Type{QQPolyRingElem}, ::Type{QQFieldElem}) = QQPolyRingElem

promote_rule(::Type{QQPolyRingElem}, ::Type{Rational{T}}) where T <: Union{Int, BigInt} = QQPolyRingElem

###############################################################################
#
#   Polynomial substitution
#
###############################################################################

(f::QQPolyRingElem)(a::QQFieldElem) = evaluate(f, a)

(f::QQPolyRingElem)(a::Rational) = evaluate(f, QQFieldElem(a))

###############################################################################
#
#   Conversion
#
###############################################################################

function fmpq_poly_to_nmod_poly_raw!(r::zzModPolyRingElem, a::QQPolyRingElem)
  ccall((:fmpq_poly_get_nmod_poly, libflint), Nothing,
        (Ref{zzModPolyRingElem}, Ref{QQPolyRingElem}), r, a)
  return r
end

function (R::zzModPolyRing)(g::QQPolyRingElem)
  r = R()
  fmpq_poly_to_nmod_poly_raw!(r, g)
  return r
end

function fmpq_poly_to_gfp_poly_raw!(r::fpPolyRingElem, a::QQPolyRingElem)
  ccall((:fmpq_poly_get_nmod_poly, libflint), Nothing,
        (Ref{fpPolyRingElem}, Ref{QQPolyRingElem}), r, a)
  return r
end

function (R::fpPolyRing)(g::QQPolyRingElem)
  r = R()
  fmpq_poly_to_gfp_poly_raw!(r, g)
  return r
end

function fmpq_poly_to_fq_default_poly_raw!(r::FqPolyRingElem, a::QQPolyRingElem, t1::ZZPolyRingElem=ZZPolyRingElem(), t2::ZZRingElem=ZZRingElem())
  ccall((:fmpq_poly_get_numerator, libflint), Nothing,
        (Ref{ZZPolyRingElem}, Ref{QQPolyRingElem}), t1, a)
  ccall((:fq_default_poly_set_fmpz_poly, libflint), Nothing,
        (Ref{FqPolyRingElem}, Ref{ZZPolyRingElem}, Ref{FqField}),
        r, t1, base_ring(parent(r)))
  ccall((:fmpq_poly_get_denominator, libflint), Nothing,
        (Ref{ZZRingElem}, Ref{QQPolyRingElem}), t2, a)
  if !isone(t2)
    ccall((:fq_default_poly_scalar_div_fq_default, libflint), Nothing,
          (Ref{FqPolyRingElem}, Ref{FqPolyRingElem}, Ref{FqFieldElem}, Ref{FqField}),
          r, r, coefficient_ring(r)(t2), coefficient_ring(r))
  end
  return r
end

function (R::FqPolyRing)(g::QQPolyRingElem)
  r = R()
  fmpq_poly_to_fq_default_poly_raw!(r, g)
  return r
end

function fmpq_poly_to_fmpz_mod_poly_raw!(r::ZZModPolyRingElem, a::QQPolyRingElem, t1::ZZPolyRingElem=ZZPolyRingElem(), t2::ZZRingElem=ZZRingElem())
  ccall((:fmpq_poly_get_numerator, libflint), Nothing,
        (Ref{ZZPolyRingElem}, Ref{QQPolyRingElem}), t1, a)
  ccall((:fmpz_mod_poly_set_fmpz_poly, libflint), Nothing,
        (Ref{ZZModPolyRingElem}, Ref{ZZPolyRingElem}, Ref{fmpz_mod_ctx_struct}), r, t1, base_ring(parent(r)).ninv)
  ccall((:fmpq_poly_get_denominator, libflint), Nothing,
        (Ref{ZZRingElem}, Ref{QQPolyRingElem}), t2, a)
  if !isone(t2)
    res = ccall((:fmpz_invmod, libflint), Cint,
                (Ref{ZZRingElem}, Ref{ZZRingElem}, Ref{ZZRingElem}),
                t2, t2, modulus(base_ring(r)))
    @assert res != 0
    ccall((:fmpz_mod_poly_scalar_mul_fmpz, libflint), Nothing,
          (Ref{ZZModPolyRingElem}, Ref{ZZModPolyRingElem}, Ref{ZZRingElem}, Ref{fmpz_mod_ctx_struct}),
          r, r, t2, base_ring(parent(r)).ninv)
  end
  return r
end

function (R::ZZModPolyRing)(g::QQPolyRingElem)
  r = R()
  fmpq_poly_to_fmpz_mod_poly_raw!(r, g)
  return r
end

function fmpq_poly_to_gfp_fmpz_poly_raw!(r::FpPolyRingElem, a::QQPolyRingElem, t1::ZZPolyRingElem=ZZPolyRingElem(), t2::ZZRingElem=ZZRingElem())
  ccall((:fmpq_poly_get_numerator, libflint), Nothing,
        (Ref{ZZPolyRingElem}, Ref{QQPolyRingElem}), t1, a)
  ccall((:fmpz_mod_poly_set_fmpz_poly, libflint), Nothing,
        (Ref{FpPolyRingElem}, Ref{ZZPolyRingElem}, Ref{fmpz_mod_ctx_struct}), r, t1, base_ring(parent(r)).ninv)
  ccall((:fmpq_poly_get_denominator, libflint), Nothing,
        (Ref{ZZRingElem}, Ref{QQPolyRingElem}), t2, a)
  if !isone(t2)
    res = ccall((:fmpz_invmod, libflint), Cint,
                (Ref{ZZRingElem}, Ref{ZZRingElem}, Ref{ZZRingElem}),
                t2, t2, modulus(base_ring(r)))
    @assert res != 0
    ccall((:fmpz_mod_poly_scalar_mul_fmpz, libflint), Nothing,
          (Ref{FpPolyRingElem}, Ref{FpPolyRingElem}, Ref{ZZRingElem}, Ref{fmpz_mod_ctx_struct}),
          r, r, t2, base_ring(parent(r)).ninv)
  end
  return r
end

function (R::FpPolyRing)(g::QQPolyRingElem)
  r = R()
  fmpq_poly_to_gfp_fmpz_poly_raw!(r, g)
  return r
end

function (a::ZZPolyRing)(b::QQPolyRingElem)
  (!isone(denominator(b))) && error("Denominator has to be 1")
  z = a()
  ccall((:fmpq_poly_get_numerator, libflint), Nothing,
        (Ref{ZZPolyRingElem}, Ref{QQPolyRingElem}), z, b)
  return z
end

###############################################################################
#
#   Parent object call overloads
#
###############################################################################

function (a::QQPolyRing)()
  z = QQPolyRingElem()
  z.parent = a
  return z
end

function (a::QQPolyRing)(b::Int)
  z = QQPolyRingElem(b)
  z.parent = a
  return z
end

function (a::QQPolyRing)(b::Integer)
  z = QQPolyRingElem(ZZRingElem(b))
  z.parent = a
  return z
end

function (a::QQPolyRing)(b::ZZRingElem)
  z = QQPolyRingElem(b)
  z.parent = a
  return z
end

function (a::QQPolyRing)(b::QQFieldElem)
  z = QQPolyRingElem(b)
  z.parent = a
  return z
end

function (a::QQPolyRing)(b::Vector{QQFieldElem})
  z = QQPolyRingElem(b)
  z.parent = a
  return z
end

(a::QQPolyRing)(b::Rational) = a(QQFieldElem(b))

(a::QQPolyRing)(b::Vector{T}, copy::Bool=true) where {T <: Integer} = a(map(QQFieldElem, b))

(a::QQPolyRing)(b::Vector{Rational{T}}, copy::Bool=true) where {T <: Integer} = a(map(QQFieldElem, b))

function (a::QQPolyRing)(b::Vector{ZZRingElem}, copy::Bool=true) 
  x = a()
  for i=1:length(b)
    ccall((:fmpq_poly_set_coeff_fmpz, libflint), Cvoid, (Ref{QQPolyRingElem}, Int, Ref{ZZRingElem}), x, i - 1, b[i])
  end
  return x
end

(a::QQPolyRing)(b::QQPolyRingElem) = b

function (a::QQPolyRing)(b::ZZPolyRingElem)
  z = QQPolyRingElem(b)
  z.parent = a
  return z
end

###############################################################################
#
#   Roots mod p
#
###############################################################################

#TODO:
# expand systematically for all finite fields
# and for ZZRingElem/QQFieldElem poly

# TODO: Can't we use the generic AbstractAlgebra `roots(::Field, ::PolyRingElem)`?
# It doesn't multiply by the denominator, but if the denominator is 0 in R, then
# then this function builds the zero polynomial and gives the root 0?
function roots(R::T, f::QQPolyRingElem) where {T<:Union{fqPolyRepField, fpField}}
  Rt, t = polynomial_ring(R, :t, cached=false)
  fp = polynomial_ring(ZZ, cached=false)[1](f * denominator(f))
  fpp = Rt(fp)
  return roots(fpp)
end
