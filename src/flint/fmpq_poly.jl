###############################################################################
#
#   QQPolyRingElem.jl : Flint polynomials over QQFieldElem
#
###############################################################################

export QQPolyRing, QQPolyRingElem

###############################################################################
#
#   Data type and parent methods
#
###############################################################################

parent_type(::Type{QQPolyRingElem}) = QQPolyRing

elem_type(::Type{QQPolyRing}) = QQPolyRingElem

dense_poly_type(::Type{QQFieldElem}) = QQPolyRingElem

base_ring(a::QQPolyRing) = FlintQQ

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

###############################################################################
#
#   Similar
#
###############################################################################

function similar(f::PolyRingElem, R::QQField, s::Symbol=var(parent(f)); cached::Bool=true)
   z = QQPolyRingElem()
   if base_ring(f) === R && s == var(parent(f)) && typeof(f) == QQPolyRingElem
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

function polynomial(R::QQField, arr::Vector{T}, var::String="x"; cached::Bool=true) where T
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
#   AbstractString I/O
#
###############################################################################

function show(io::IO, p::QQPolyRing)
   print(io, "Univariate Polynomial Ring in ")
   print(io, string(var(p)))
   print(io, " over ")
   show(io, base_ring(p))
end

###############################################################################
#
#   Unary operations
#
###############################################################################

function -(x::QQPolyRingElem)
   z = parent(x)()
   ccall((:fmpq_poly_neg, libflint), Nothing,
         (Ref{QQPolyRingElem}, Ref{QQPolyRingElem}), z, x)
   return z
end

###############################################################################
#
#   Binary operations
#
###############################################################################

function +(x::QQPolyRingElem, y::QQPolyRingElem)
   check_parent(x, y)
   z = parent(x)()
   ccall((:fmpq_poly_add, libflint), Nothing,
                (Ref{QQPolyRingElem}, Ref{QQPolyRingElem},  Ref{QQPolyRingElem}),
               z, x, y)
   return z
end

function -(x::QQPolyRingElem, y::QQPolyRingElem)
   check_parent(x, y)
   z = parent(x)()
   ccall((:fmpq_poly_sub, libflint), Nothing,
                (Ref{QQPolyRingElem}, Ref{QQPolyRingElem},  Ref{QQPolyRingElem}),
               z, x, y)
   return z
end

function *(x::QQPolyRingElem, y::QQPolyRingElem)
   check_parent(x, y)
   z = parent(x)()
   ccall((:fmpq_poly_mul, libflint), Nothing,
                (Ref{QQPolyRingElem}, Ref{QQPolyRingElem},  Ref{QQPolyRingElem}),
               z, x, y)
   return z
end

###############################################################################
#
#   Ad hoc binary operators
#
###############################################################################

function *(x::Int, y::QQPolyRingElem)
   z = parent(y)()
   ccall((:fmpq_poly_scalar_mul_si, libflint), Nothing,
                (Ref{QQPolyRingElem}, Ref{QQPolyRingElem}, Int), z, y, x)
   return z
end

function *(x::ZZRingElem, y::QQPolyRingElem)
   z = parent(y)()
   ccall((:fmpq_poly_scalar_mul_fmpz, libflint), Nothing,
         (Ref{QQPolyRingElem}, Ref{QQPolyRingElem}, Ref{ZZRingElem}), z, y, x)
   return z
end

function *(x::QQFieldElem, y::QQPolyRingElem)
   z = parent(y)()
   ccall((:fmpq_poly_scalar_mul_fmpq, libflint), Nothing,
                (Ref{QQPolyRingElem}, Ref{QQPolyRingElem}, Ref{QQFieldElem}), z, y, x)
   return z
end

function +(x::QQPolyRingElem, y::Int)
   z = parent(x)()
   ccall((:fmpq_poly_add_si, libflint), Nothing,
                (Ref{QQPolyRingElem}, Ref{QQPolyRingElem}, Int), z, x, y)
   return z
end

function +(x::QQPolyRingElem, y::ZZRingElem)
   z = parent(x)()
   ccall((:fmpq_poly_add_fmpz, libflint), Nothing,
                (Ref{QQPolyRingElem}, Ref{QQPolyRingElem}, Ref{ZZRingElem}), z, x, y)
   return z
end

function +(x::QQPolyRingElem, y::QQFieldElem)
   z = parent(x)()
   ccall((:fmpq_poly_add_fmpq, libflint), Nothing,
                (Ref{QQPolyRingElem}, Ref{QQPolyRingElem}, Ref{QQFieldElem}), z, x, y)
   return z
end

function -(x::QQPolyRingElem, y::Int)
   z = parent(x)()
   ccall((:fmpq_poly_sub_si, libflint), Nothing,
                (Ref{QQPolyRingElem}, Ref{QQPolyRingElem}, Int), z, x, y)
   return z
end

function -(x::QQPolyRingElem, y::ZZRingElem)
   z = parent(x)()
   ccall((:fmpq_poly_sub_fmpz, libflint), Nothing,
         (Ref{QQPolyRingElem}, Ref{QQPolyRingElem}, Ref{ZZRingElem}), z, x, y)
   return z
end

function -(x::QQPolyRingElem, y::QQFieldElem)
   z = parent(x)()
   ccall((:fmpq_poly_sub_fmpq, libflint), Nothing,
                (Ref{QQPolyRingElem}, Ref{QQPolyRingElem}, Ref{QQFieldElem}), z, x, y)
   return z
end

function -(x::Int, y::QQPolyRingElem)
   z = parent(y)()
   ccall((:fmpq_poly_si_sub, libflint), Nothing,
                (Ref{QQPolyRingElem}, Int, Ref{QQPolyRingElem}), z, x, y)
   return z
end

function -(x::ZZRingElem, y::QQPolyRingElem)
   z = parent(y)()
   ccall((:fmpq_poly_fmpz_sub, libflint), Nothing,
         (Ref{QQPolyRingElem}, Ref{ZZRingElem}, Ref{QQPolyRingElem}), z, x, y)
   return z
end

function -(x::QQFieldElem, y::QQPolyRingElem)
   z = parent(y)()
   ccall((:fmpq_poly_fmpq_sub, libflint), Nothing,
                (Ref{QQPolyRingElem}, Ref{QQFieldElem}, Ref{QQPolyRingElem}), z, x, y)
   return z
end

+(x::Int, y::QQPolyRingElem) = y + x

+(x::ZZRingElem, y::QQPolyRingElem) = y + x

+(x::QQFieldElem, y::QQPolyRingElem) = y + x

*(x::QQPolyRingElem, y::Int) = y*x

*(x::QQPolyRingElem, y::ZZRingElem) = y*x

*(x::QQPolyRingElem, y::QQFieldElem) = y*x

+(x::Integer, y::QQPolyRingElem) = y + ZZRingElem(x)

-(x::Integer, y::QQPolyRingElem) = ZZRingElem(x) - y

*(x::Integer, y::QQPolyRingElem) = ZZRingElem(x)*y

+(x::QQPolyRingElem, y::Integer) = x + ZZRingElem(y)

-(x::QQPolyRingElem, y::Integer) = x - ZZRingElem(y)

*(x::QQPolyRingElem, y::Integer) = ZZRingElem(y)*x

+(x::Rational, y::QQPolyRingElem) = QQFieldElem(x) + y

-(x::Rational, y::QQPolyRingElem) = QQFieldElem(x) - y

*(x::Rational, y::QQPolyRingElem) = QQFieldElem(x) * y

+(x::QQPolyRingElem, y::Rational) = x + QQFieldElem(y)

-(x::QQPolyRingElem, y::Rational) = x - QQFieldElem(y)

*(x::QQPolyRingElem, y::Rational) = x * QQFieldElem(y)

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

divexact(x::QQPolyRingElem, y::QQPolyRingElem; check::Bool=true) = div(x,y)

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

divexact(x::QQPolyRingElem, y::Integer; check::Bool=true) = divexact(x, ZZRingElem(y); check=check)

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

function compose(x::QQPolyRingElem, y::QQPolyRingElem)
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

function zero!(z::QQPolyRingElem)
   ccall((:fmpq_poly_zero, libflint), Nothing,
                    (Ref{QQPolyRingElem},), z)
   return z
end

function fit!(z::QQPolyRingElem, n::Int)
   ccall((:fmpq_poly_fit_length, libflint), Nothing,
                    (Ref{QQPolyRingElem}, Int), z, n)
   return nothing
end

function setcoeff!(z::QQPolyRingElem, n::Int, x::ZZRingElem)
   ccall((:fmpq_poly_set_coeff_fmpz, libflint), Nothing,
                    (Ref{QQPolyRingElem}, Int, Ref{ZZRingElem}), z, n, x)
   return z
end

function setcoeff!(z::QQPolyRingElem, n::Int, x::QQFieldElem)
   ccall((:fmpq_poly_set_coeff_fmpq, libflint), Nothing,
                    (Ref{QQPolyRingElem}, Int, Ref{QQFieldElem}), z, n, x)
   return z
end

function mul!(z::QQPolyRingElem, x::QQPolyRingElem, y::QQPolyRingElem)
   ccall((:fmpq_poly_mul, libflint), Nothing,
                (Ref{QQPolyRingElem}, Ref{QQPolyRingElem}, Ref{QQPolyRingElem}), z, x, y)
   return z
end

function addeq!(z::QQPolyRingElem, x::QQPolyRingElem)
   ccall((:fmpq_poly_add, libflint), Nothing,
                (Ref{QQPolyRingElem}, Ref{QQPolyRingElem}, Ref{QQPolyRingElem}), z, z, x)
   return z
end

function add!(z::QQPolyRingElem, x::QQPolyRingElem, y::QQPolyRingElem)
   ccall((:fmpq_poly_add, libflint), Nothing,
                (Ref{QQPolyRingElem}, Ref{QQPolyRingElem}, Ref{QQPolyRingElem}), z, x, y)
   return z
end

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

(a::QQPolyRing)(b::Vector{ZZRingElem}, copy::Bool=true) = a(map(QQFieldElem, b))

(a::QQPolyRing)(b::QQPolyRingElem) = b

function (a::QQPolyRing)(b::ZZPolyRingElem)
   z = QQPolyRingElem(b)
   z.parent = a
   return z
end
