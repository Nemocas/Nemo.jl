###############################################################################
#
#   fmpq_poly.jl : Flint polynomials over fmpq
#
###############################################################################

export FmpqPolyRing, fmpq_poly

###############################################################################
#
#   Data type and parent methods
#
###############################################################################

parent_type(::Type{fmpq_poly}) = FmpqPolyRing

elem_type(::Type{FmpqPolyRing}) = fmpq_poly

base_ring(a::FmpqPolyRing) = a.base_ring

var(a::FmpqPolyRing) = a.S

###############################################################################
#
#   Basic manipulation
#
###############################################################################

@doc Markdown.doc"""
    denominator(a::fmpq_poly)
> Return the least common denominator of the coefficients of the polynomial
> $a$.
"""
function denominator(a::fmpq_poly)
   z = fmpz()
   ccall((:fmpq_poly_get_denominator, libflint), Nothing,
         (Ref{fmpz}, Ref{fmpq_poly}), z, a)
   return z
end

length(x::fmpq_poly) = ccall((:fmpq_poly_length, libflint), Int,
                                   (Ref{fmpq_poly},), x)

set_length!(x::fmpq_poly, n::Int) = ccall((:_fmpq_poly_set_length, libflint), Nothing,
                                   (Ref{fmpq_poly}, Int), x, n)

function coeff(x::fmpq_poly, n::Int)
   n < 0 && throw(DomainError(n, "Index must be non-negative"))
   z = fmpq()
   ccall((:fmpq_poly_get_coeff_fmpq, libflint), Nothing,
               (Ref{fmpq}, Ref{fmpq_poly}, Int), z, x, n)
   return z
end

zero(a::FmpqPolyRing) = a(0)

one(a::FmpqPolyRing) = a(1)

gen(a::FmpqPolyRing) = a([zero(base_ring(a)), one(base_ring(a))])

isgen(x::fmpq_poly) = ccall((:fmpq_poly_is_gen, libflint), Bool,
                            (Ref{fmpq_poly},), x)

function deepcopy_internal(a::fmpq_poly, dict::IdDict)
   z = fmpq_poly(a)
   z.parent = parent(a)
   return z
end

###############################################################################
#
#   Canonicalisation
#
###############################################################################

canonical_unit(a::fmpq_poly) = canonical_unit(lead(a))

###############################################################################
#
#   AbstractString I/O
#
###############################################################################

#=
#use the generic one to print //
function show(io::IO, x::fmpq_poly)
   if length(x) == 0
      print(io, "0")
   else
      cstr = ccall((:fmpq_poly_get_str_pretty, libflint), Ptr{UInt8},
          (Ref{fmpq_poly}, Ptr{UInt8}), x, string(var(parent(x))))

      print(io, unsafe_string(cstr))

      ccall((:flint_free, libflint), Nothing, (Ptr{UInt8},), cstr)
   end
end
=#

function show(io::IO, p::FmpqPolyRing)
   print(io, "Univariate Polynomial Ring in ")
   print(io, string(var(p)))
   print(io, " over ")
   show(io, p.base_ring)
end

show_minus_one(::Type{fmpq_poly}) = show_minus_one(FracElem{fmpz})

###############################################################################
#
#   Unary operations
#
###############################################################################

function -(x::fmpq_poly)
   z = parent(x)()
   ccall((:fmpq_poly_neg, libflint), Nothing,
         (Ref{fmpq_poly}, Ref{fmpq_poly}), z, x)
   return z
end

###############################################################################
#
#   Binary operations
#
###############################################################################

function +(x::fmpq_poly, y::fmpq_poly)
   check_parent(x, y)
   z = parent(x)()
   ccall((:fmpq_poly_add, libflint), Nothing,
                (Ref{fmpq_poly}, Ref{fmpq_poly},  Ref{fmpq_poly}),
               z, x, y)
   return z
end

function -(x::fmpq_poly, y::fmpq_poly)
   check_parent(x, y)
   z = parent(x)()
   ccall((:fmpq_poly_sub, libflint), Nothing,
                (Ref{fmpq_poly}, Ref{fmpq_poly},  Ref{fmpq_poly}),
               z, x, y)
   return z
end

function *(x::fmpq_poly, y::fmpq_poly)
   check_parent(x, y)
   z = parent(x)()
   ccall((:fmpq_poly_mul, libflint), Nothing,
                (Ref{fmpq_poly}, Ref{fmpq_poly},  Ref{fmpq_poly}),
               z, x, y)
   return z
end

###############################################################################
#
#   Ad hoc binary operators
#
###############################################################################

function *(x::Int, y::fmpq_poly)
   z = parent(y)()
   ccall((:fmpq_poly_scalar_mul_si, libflint), Nothing,
                (Ref{fmpq_poly}, Ref{fmpq_poly}, Int), z, y, x)
   return z
end

function *(x::fmpz, y::fmpq_poly)
   z = parent(y)()
   ccall((:fmpq_poly_scalar_mul_fmpz, libflint), Nothing,
         (Ref{fmpq_poly}, Ref{fmpq_poly}, Ref{fmpz}), z, y, x)
   return z
end

function *(x::fmpq, y::fmpq_poly)
   z = parent(y)()
   ccall((:fmpq_poly_scalar_mul_fmpq, libflint), Nothing,
                (Ref{fmpq_poly}, Ref{fmpq_poly}, Ref{fmpq}), z, y, x)
   return z
end

function +(x::fmpq_poly, y::Int)
   z = parent(x)()
   ccall((:fmpq_poly_add_si, libflint), Nothing,
                (Ref{fmpq_poly}, Ref{fmpq_poly}, Int), z, x, y)
   return z
end

function +(x::fmpq_poly, y::fmpz)
   z = parent(x)()
   ccall((:fmpq_poly_add_fmpz, libflint), Nothing,
                (Ref{fmpq_poly}, Ref{fmpq_poly}, Ref{fmpz}), z, x, y)
   return z
end

function +(x::fmpq_poly, y::fmpq)
   z = parent(x)()
   ccall((:fmpq_poly_add_fmpq, libflint), Nothing,
                (Ref{fmpq_poly}, Ref{fmpq_poly}, Ref{fmpq}), z, x, y)
   return z
end

function -(x::fmpq_poly, y::Int)
   z = parent(x)()
   ccall((:fmpq_poly_sub_si, libflint), Nothing,
                (Ref{fmpq_poly}, Ref{fmpq_poly}, Int), z, x, y)
   return z
end

function -(x::fmpq_poly, y::fmpz)
   z = parent(x)()
   ccall((:fmpq_poly_sub_fmpz, libflint), Nothing,
         (Ref{fmpq_poly}, Ref{fmpq_poly}, Ref{fmpz}), z, x, y)
   return z
end

function -(x::fmpq_poly, y::fmpq)
   z = parent(x)()
   ccall((:fmpq_poly_sub_fmpq, libflint), Nothing,
                (Ref{fmpq_poly}, Ref{fmpq_poly}, Ref{fmpq}), z, x, y)
   return z
end

function -(x::Int, y::fmpq_poly)
   z = parent(y)()
   ccall((:fmpq_poly_si_sub, libflint), Nothing,
                (Ref{fmpq_poly}, Int, Ref{fmpq_poly}), z, x, y)
   return z
end

function -(x::fmpz, y::fmpq_poly)
   z = parent(y)()
   ccall((:fmpq_poly_fmpz_sub, libflint), Nothing,
         (Ref{fmpq_poly}, Ref{fmpz}, Ref{fmpq_poly}), z, x, y)
   return z
end

function -(x::fmpq, y::fmpq_poly)
   z = parent(y)()
   ccall((:fmpq_poly_fmpq_sub, libflint), Nothing,
                (Ref{fmpq_poly}, Ref{fmpq}, Ref{fmpq_poly}), z, x, y)
   return z
end

+(x::Int, y::fmpq_poly) = y + x

+(x::fmpz, y::fmpq_poly) = y + x

+(x::fmpq, y::fmpq_poly) = y + x

*(x::fmpq_poly, y::Int) = y*x

*(x::fmpq_poly, y::fmpz) = y*x

*(x::fmpq_poly, y::fmpq) = y*x

+(x::Integer, y::fmpq_poly) = y + fmpz(x)

-(x::Integer, y::fmpq_poly) = fmpz(x) - y

*(x::Integer, y::fmpq_poly) = fmpz(x)*y

+(x::fmpq_poly, y::Integer) = x + fmpz(y)

-(x::fmpq_poly, y::Integer) = x - fmpz(y)

*(x::fmpq_poly, y::Integer) = fmpz(y)*x

+(x::Rational, y::fmpq_poly) = fmpq(x) + y

-(x::Rational, y::fmpq_poly) = fmpq(x) - y

*(x::Rational, y::fmpq_poly) = fmpq(x) * y

+(x::fmpq_poly, y::Rational) = x + fmpq(y)

-(x::fmpq_poly, y::Rational) = x - fmpq(y)

*(x::fmpq_poly, y::Rational) = x * fmpq(y)

###############################################################################
#
#   Powering
#
###############################################################################

function ^(x::fmpq_poly, y::Int)
   y < 0 && throw(DomainError(y, "Exponent must be non-negative"))
   z = parent(x)()
   ccall((:fmpq_poly_pow, libflint), Nothing,
                (Ref{fmpq_poly}, Ref{fmpq_poly}, Int),
               z, x, y)
   return z
end

###############################################################################
#
#   Comparisons
#
###############################################################################

function ==(x::fmpq_poly, y::fmpq_poly)
   check_parent(x, y)
   return ccall((:fmpq_poly_equal, libflint), Bool,
                                      (Ref{fmpq_poly}, Ref{fmpq_poly}), x, y)
end

###############################################################################
#
#   Ad hoc comparisons
#
###############################################################################

function ==(x::fmpq_poly, y::fmpq)
   if length(x) > 1
      return false
   elseif length(x) == 1
      z = fmpq()
      ccall((:fmpq_poly_get_coeff_fmpq, libflint), Nothing,
                       (Ref{fmpq}, Ref{fmpq_poly}, Int), z, x, 0)
      return ccall((:fmpq_equal, libflint), Bool,
               (Ref{fmpq}, Ref{fmpq}, Int), z, y, 0)
   else
      return iszero(y)
   end
end

==(x::fmpq, y::fmpq_poly) = y == x

==(x::fmpq_poly, y::Rational{T}) where T <: Union{Int, BigInt} = x == fmpq(y)

==(x::Rational{T}, y::fmpq_poly) where T <: Union{Int, BigInt} = y == x

###############################################################################
#
#   Truncation
#
###############################################################################

function truncate(a::fmpq_poly, n::Int)
   n < 0 && throw(DomainError(n, "Index must be non-negative"))

   if length(a) <= n
      return a
   end

   z = parent(a)()
   ccall((:fmpq_poly_set_trunc, libflint), Nothing,
                (Ref{fmpq_poly}, Ref{fmpq_poly}, Int), z, a, n)
   return z
end

function mullow(x::fmpq_poly, y::fmpq_poly, n::Int)
   check_parent(x, y)
   n < 0 && throw(DomainError(n, "Index must be non-negative"))

   z = parent(x)()
   ccall((:fmpq_poly_mullow, libflint), Nothing,
         (Ref{fmpq_poly}, Ref{fmpq_poly}, Ref{fmpq_poly}, Int), z, x, y, n)
   return z
end

###############################################################################
#
#   Reversal
#
###############################################################################

function reverse(x::fmpq_poly, len::Int)
   len < 0 && throw(DomainError(len, "Length must be non-negative"))
   z = parent(x)()
   ccall((:fmpq_poly_reverse, libflint), Nothing,
                (Ref{fmpq_poly}, Ref{fmpq_poly}, Int), z, x, len)
   return z
end

###############################################################################
#
#   Shifting
#
###############################################################################

function shift_left(x::fmpq_poly, len::Int)
   len < 0 && throw(DomainError(len, "Shift must be non-negative"))
   z = parent(x)()
   ccall((:fmpq_poly_shift_left, libflint), Nothing,
      (Ref{fmpq_poly}, Ref{fmpq_poly}, Int), z, x, len)
   return z
end

function shift_right(x::fmpq_poly, len::Int)
   len < 0 && throw(DomainError(len, "Shift must be non-negative"))
   z = parent(x)()
   ccall((:fmpq_poly_shift_right, libflint), Nothing,
       (Ref{fmpq_poly}, Ref{fmpq_poly}, Int), z, x, len)
   return z
end

###############################################################################
#
#   Euclidean division
#
###############################################################################

function mod(x::fmpq_poly, y::fmpq_poly)
   check_parent(x, y)
   iszero(y) && throw(DivideError())
   r = parent(x)()
   ccall((:fmpq_poly_rem, libflint), Nothing,
                (Ref{fmpq_poly}, Ref{fmpq_poly}, Ref{fmpq_poly}),
               r, x, y)
   return r
end

rem(x::fmpq_poly, y::fmpq_poly) = mod(x, y)

function divrem(x::fmpq_poly, y::fmpq_poly)
   check_parent(x, y)
   iszero(y) && throw(DivideError())
   q = parent(x)()
   r = parent(x)()
   ccall((:fmpq_poly_divrem, libflint), Nothing,
         (Ref{fmpq_poly}, Ref{fmpq_poly}, Ref{fmpq_poly}, Ref{fmpq_poly}),
               q, r, x, y)
   return q, r
end

###############################################################################
#
#   Exact division
#
###############################################################################

function div(x::fmpq_poly, y::fmpq_poly)
   check_parent(x, y)
   iszero(y) && throw(DivideError())
   z = parent(x)()
   ccall((:fmpq_poly_div, libflint), Nothing,
            (Ref{fmpq_poly}, Ref{fmpq_poly}, Ref{fmpq_poly}), z, x, y)
   return z
end

divexact(x::fmpq_poly, y::fmpq_poly) = div(x,y)

###############################################################################
#
#   Ad hoc exact division
#
###############################################################################

function divexact(x::fmpq_poly, y::fmpz)
   iszero(y) && throw(DivideError())
   z = parent(x)()
   ccall((:fmpq_poly_scalar_div_fmpz, libflint), Nothing,
          (Ref{fmpq_poly}, Ref{fmpq_poly}, Ref{fmpz}), z, x, y)
   return z
end

function divexact(x::fmpq_poly, y::fmpq)
   iszero(y) && throw(DivideError())
   z = parent(x)()
   ccall((:fmpq_poly_scalar_div_fmpq, libflint), Nothing,
          (Ref{fmpq_poly}, Ref{fmpq_poly}, Ref{fmpq}), z, x, y)
   return z
end

function divexact(x::fmpq_poly, y::Int)
   y == 0 && throw(DivideError())
   z = parent(x)()
   ccall((:fmpq_poly_scalar_div_si, libflint), Nothing,
                        (Ref{fmpq_poly}, Ref{fmpq_poly}, Int), z, x, y)
   return z
end

divexact(x::fmpq_poly, y::Integer) = divexact(x, fmpz(y))

divexact(x::fmpq_poly, y::Rational{T}) where T <: Union{Int, BigInt} = divexact(x, fmpq(y))

###############################################################################
#
#   Removal and valuation
#
###############################################################################

function divides(z::fmpq_poly, x::fmpq_poly)
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

function gcd(x::fmpq_poly, y::fmpq_poly)
   check_parent(x, y)
   z = parent(x)()
   ccall((:fmpq_poly_gcd, libflint), Nothing,
                (Ref{fmpq_poly}, Ref{fmpq_poly}, Ref{fmpq_poly}), z, x, y)
   return z
end

function content(x::fmpq_poly)
   z = fmpq()
   ccall((:fmpq_poly_content, libflint), Nothing,
         (Ref{fmpq}, Ref{fmpq_poly}), z, x)
   return z
end

function primpart(x::fmpq_poly)
   z = parent(x)()
   ccall((:fmpq_poly_primitive_part, libflint), Nothing,
         (Ref{fmpq_poly}, Ref{fmpq_poly}), z, x)
   return z
end

###############################################################################
#
#   Evaluation
#
###############################################################################

function evaluate(x::fmpq_poly, y::fmpz)
   z = fmpq()
   ccall((:fmpq_poly_evaluate_fmpz, libflint), Nothing,
                (Ref{fmpq}, Ref{fmpq_poly}, Ref{fmpz}), z, x, y)
   return z
end

function evaluate(x::fmpq_poly, y::fmpq)
   z = fmpq()
   ccall((:fmpq_poly_evaluate_fmpq, libflint), Nothing,
                (Ref{fmpq}, Ref{fmpq_poly}, Ref{fmpq}), z, x, y)
   return z
end

evaluate(x::fmpq_poly, y::Integer) = evaluate(x, fmpz(y))

evaluate(x::fmpq_poly, y::Rational) = evaluate(x, fmpq(y))

###############################################################################
#
#   Composition
#
###############################################################################

function compose(x::fmpq_poly, y::fmpq_poly)
   check_parent(x, y)
   z = parent(x)()
   ccall((:fmpq_poly_compose, libflint), Nothing,
                (Ref{fmpq_poly}, Ref{fmpq_poly}, Ref{fmpq_poly}), z, x, y)
   return z
end

###############################################################################
#
#   Derivative
#
###############################################################################

function derivative(x::fmpq_poly)
   z = parent(x)()
   ccall((:fmpq_poly_derivative, libflint), Nothing,
                (Ref{fmpq_poly}, Ref{fmpq_poly}), z, x)
   return z
end

###############################################################################
#
#   Integral
#
###############################################################################

function integral(x::fmpq_poly)
   z = parent(x)()
   ccall((:fmpq_poly_integral, libflint), Nothing,
                (Ref{fmpq_poly}, Ref{fmpq_poly}), z, x)
   return z
end

###############################################################################
#
#   Resultant
#
###############################################################################

function resultant(x::fmpq_poly, y::fmpq_poly)
   check_parent(x, y)
   z = fmpq()
   ccall((:fmpq_poly_resultant, libflint), Nothing,
                (Ref{fmpq}, Ref{fmpq_poly}, Ref{fmpq_poly}), z, x, y)
   return z
end

###############################################################################
#
#   GCDX
#
###############################################################################

function gcdx(x::fmpq_poly, y::fmpq_poly)
   check_parent(x, y)
   z = parent(x)()
   u = parent(x)()
   v = parent(x)()
   ccall((:fmpq_poly_xgcd, libflint), Nothing,
        (Ref{fmpq_poly}, Ref{fmpq_poly}, Ref{fmpq_poly}, Ref{fmpq_poly},
                                     Ref{fmpq_poly}), z, u, v, x, y)
   return (z, u, v)
end

################################################################################
#
#   Factorization
#
################################################################################

@doc Markdown.doc"""
    factor(x::fmpq_poly)
> Returns the factorization of $x$.
"""
function factor(x::fmpq_poly)
   res, z = _factor(x)
   return Fac(parent(x)(z), res)
end

function _factor(x::fmpq_poly)
   res = Dict{fmpq_poly, Int}()
   y = fmpz_poly()
   ccall((:fmpq_poly_get_numerator, libflint), Nothing,
         (Ref{fmpz_poly}, Ref{fmpq_poly}), y, x)
   fac = fmpz_poly_factor()
   ccall((:fmpz_poly_factor, libflint), Nothing,
              (Ref{fmpz_poly_factor}, Ref{fmpz_poly}), fac, y)
   z = fmpz()
   ccall((:fmpz_poly_factor_get_fmpz, libflint), Nothing,
            (Ref{fmpz}, Ref{fmpz_poly_factor}), z, fac)
   f = fmpz_poly()
   for i in 1:fac.num
      ccall((:fmpz_poly_factor_get_fmpz_poly, libflint), Nothing,
            (Ref{fmpz_poly}, Ref{fmpz_poly_factor}, Int), f, fac, i - 1)
      e = unsafe_load(fac.exp, i)
      res[parent(x)(f)] = e
   end
   return res, fmpq(z, denominator(x))
end

@doc Markdown.doc"""
    isirreducible(x::fmpq_poly)
> Checks if $x$ is irreducible.
"""
function isirreducible(x::fmpq_poly)
   res, _ = _factor(x)
   return length(res) == 1 && first(values(res)) == 1
end

###############################################################################
#
#   Signature
#
###############################################################################

@doc Markdown.doc"""
    signature(f::fmpq_poly)
> Return the signature of $f$, i.e. a tuple $(r, s)$ where $r$ is the number of
> real roots of $f$ and $s$ is half the number of complex roots.
"""
function signature(f::fmpq_poly)
   r = Vector{Int}(undef, 1)
   s = Vector{Int}(undef, 1)
   z = fmpz_poly()
   ccall((:fmpq_poly_get_numerator, libflint), Nothing,
         (Ref{fmpz_poly}, Ref{fmpq_poly}), z, f)
   return signature(z)
end

###############################################################################
#
#   Speedups for polynomials over fmpq_polys
#
###############################################################################

function *(a::Generic.Poly{fmpq_poly}, b::Generic.Poly{fmpq_poly})
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

function zero!(z::fmpq_poly)
   ccall((:fmpq_poly_zero, libflint), Nothing,
                    (Ref{fmpq_poly},), z)
   return z
end

function fit!(z::fmpq_poly, n::Int)
   ccall((:fmpq_poly_fit_length, libflint), Nothing,
                    (Ref{fmpq_poly}, Int), z, n)
   return nothing
end

function setcoeff!(z::fmpq_poly, n::Int, x::fmpz)
   ccall((:fmpq_poly_set_coeff_fmpz, libflint), Nothing,
                    (Ref{fmpq_poly}, Int, Ref{fmpz}), z, n, x)
   return z
end

function setcoeff!(z::fmpq_poly, n::Int, x::fmpq)
   ccall((:fmpq_poly_set_coeff_fmpq, libflint), Nothing,
                    (Ref{fmpq_poly}, Int, Ref{fmpq}), z, n, x)
   return z
end

function mul!(z::fmpq_poly, x::fmpq_poly, y::fmpq_poly)
   ccall((:fmpq_poly_mul, libflint), Nothing,
                (Ref{fmpq_poly}, Ref{fmpq_poly}, Ref{fmpq_poly}), z, x, y)
   return z
end

function addeq!(z::fmpq_poly, x::fmpq_poly)
   ccall((:fmpq_poly_add, libflint), Nothing,
                (Ref{fmpq_poly}, Ref{fmpq_poly}, Ref{fmpq_poly}), z, z, x)
   return z
end

function add!(z::fmpq_poly, x::fmpq_poly, y::fmpq_poly)
   ccall((:fmpq_poly_add, libflint), Nothing,
                (Ref{fmpq_poly}, Ref{fmpq_poly}, Ref{fmpq_poly}), z, x, y)
   return z
end

###############################################################################
#
#   Promotions
#
###############################################################################

promote_rule(::Type{fmpq_poly}, ::Type{T}) where {T <: Integer} = fmpq_poly

promote_rule(::Type{fmpq_poly}, ::Type{fmpz}) = fmpq_poly

promote_rule(::Type{fmpq_poly}, ::Type{fmpq}) = fmpq_poly

promote_rule(::Type{fmpq_poly}, ::Type{Rational{T}}) where T <: Union{Int, BigInt} = fmpq_poly

###############################################################################
#
#   Polynomial substitution
#
###############################################################################

(f::fmpq_poly)(a::fmpq) = evaluate(f, a)

(f::fmpq_poly)(a::Rational) = evaluate(f, fmpq(a))

###############################################################################
#
#   Parent object call overloads
#
###############################################################################

function (a::FmpqPolyRing)()
   z = fmpq_poly()
   z.parent = a
   return z
end

function (a::FmpqPolyRing)(b::Int)
   z = fmpq_poly(b)
   z.parent = a
   return z
end

function (a::FmpqPolyRing)(b::Integer)
   z = fmpq_poly(fmpz(b))
   z.parent = a
   return z
end

function (a::FmpqPolyRing)(b::fmpz)
   z = fmpq_poly(b)
   z.parent = a
   return z
end

function (a::FmpqPolyRing)(b::fmpq)
   z = fmpq_poly(b)
   z.parent = a
   return z
end

function (a::FmpqPolyRing)(b::Array{fmpq, 1})
   z = fmpq_poly(b)
   z.parent = a
   return z
end

(a::FmpqPolyRing)(b::Rational) = a(fmpq(b))

(a::FmpqPolyRing)(b::Array{T, 1}, copy::Bool=true) where {T <: Integer} = a(map(fmpq, b))

(a::FmpqPolyRing)(b::Array{Rational{T}, 1}, copy::Bool=true) where {T <: Integer} = a(map(fmpq, b))

(a::FmpqPolyRing)(b::Array{fmpz, 1}, copy::Bool=true) = a(map(fmpq, b))

(a::FmpqPolyRing)(b::fmpq_poly) = b

function (a::FmpqPolyRing)(b::fmpz_poly)
   z = fmpq_poly(b)
   z.parent = a
   return z
end

###############################################################################
#
#   PolynomialRing constructor
#
###############################################################################

function PolynomialRing(R::FlintRationalField, s::AbstractString; cached = true)
   S = Symbol(s)

   parent_obj = FmpqPolyRing(R, S, cached)

   return parent_obj, parent_obj([fmpq(0), fmpq(1)])
end
