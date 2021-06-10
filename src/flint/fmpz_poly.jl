###############################################################################
#
#   fmpz_poly.jl : Flint polynomials over fmpz
#
###############################################################################

export FmpzPolyRing, fmpz_poly, cyclotomic, theta_qexp, eta_qexp, cos_minpoly,
       swinnerton_dyer, signature, height

###############################################################################
#
#   Data type and parent methods
#
###############################################################################

parent_type(::Type{fmpz_poly}) = FmpzPolyRing

elem_type(::Type{FmpzPolyRing}) = fmpz_poly

dense_poly_type(::Type{fmpz}) = fmpz_poly

base_ring(a::FmpzPolyRing) = a.base_ring

parent(a::fmpz_poly) = a.parent

var(a::FmpzPolyRing) = a.S

###############################################################################
#
#   Basic manipulation
#
###############################################################################

length(x::fmpz_poly) = ccall((:fmpz_poly_length, libflint), Int,
                             (Ref{fmpz_poly},), x)

function coeff(x::fmpz_poly, n::Int)
   n < 0 && throw(DomainError(n, "Index must be non-negative"))
   z = fmpz()
   ccall((:fmpz_poly_get_coeff_fmpz, libflint), Nothing,
               (Ref{fmpz}, Ref{fmpz_poly}, Int), z, x, n)
   return z
end

zero(a::FmpzPolyRing) = a(0)

one(a::FmpzPolyRing) = a(1)

gen(a::FmpzPolyRing) = a([zero(base_ring(a)), one(base_ring(a))])

isgen(x::fmpz_poly) = ccall((:fmpz_poly_is_gen, libflint), Bool,
                            (Ref{fmpz_poly},), x)

function deepcopy_internal(a::fmpz_poly, dict::IdDict)
   z = fmpz_poly(a)
   z.parent = parent(a)
   return z
end

characteristic(::FmpzPolyRing) = 0

@doc Markdown.doc"""
    height(a::fmpz_poly)

Return the largest of the absolute values of the coefficients of a.
"""
function height(a::fmpz_poly)
   z = fmpz()
   ccall((:fmpz_poly_height, libflint), Nothing,
         (Ref{fmpz}, Ref{fmpz_poly}), z, a)
   return z
end

###############################################################################
#
#   Similar
#
###############################################################################

function similar(f::PolyElem, R::FlintIntegerRing, var::Symbol=var(parent(f)); cached::Bool=true)
   z = fmpz_poly()
   z.parent = FmpzPolyRing(R, var, cached)
   return z
end

###############################################################################
#
#   polynomial constructor
#
###############################################################################

function polynomial(R::FlintIntegerRing, arr::Vector{T}, var::String="x"; cached::Bool=true) where T
   coeffs = T == fmpz ? arr : map(R, arr)
   coeffs = length(coeffs) == 0 ? fmpz[] : coeffs
   z = fmpz_poly(coeffs)
   z.parent = FmpzPolyRing(R, Symbol(var), cached)
   return z
end

###############################################################################
#
#   Canonicalisation
#
###############################################################################

canonical_unit(a::fmpz_poly) = canonical_unit(leading_coefficient(a))

###############################################################################
#
#   AbstractString I/O
#
###############################################################################

function show(io::IO, p::FmpzPolyRing)
   print(io, "Univariate Polynomial Ring in ")
   print(io, string(var(p)))
   print(io, " over ")
   show(io, p.base_ring)
end

###############################################################################
#
#   Unary operations
#
###############################################################################

function -(x::fmpz_poly)
   z = parent(x)()
   ccall((:fmpz_poly_neg, libflint), Nothing,
         (Ref{fmpz_poly}, Ref{fmpz_poly}), z, x)
   return z
end

###############################################################################
#
#   Binary operations
#
###############################################################################

function +(x::fmpz_poly, y::fmpz_poly)
   check_parent(x, y)
   z = parent(x)()
   ccall((:fmpz_poly_add, libflint), Nothing,
                (Ref{fmpz_poly}, Ref{fmpz_poly},  Ref{fmpz_poly}),
               z, x, y)
   return z
end

function -(x::fmpz_poly, y::fmpz_poly)
   check_parent(x, y)
   z = parent(x)()
   ccall((:fmpz_poly_sub, libflint), Nothing,
                (Ref{fmpz_poly}, Ref{fmpz_poly},  Ref{fmpz_poly}),
               z, x, y)
   return z
end

function *(x::fmpz_poly, y::fmpz_poly)
   check_parent(x, y)
   z = parent(x)()
   ccall((:fmpz_poly_mul, libflint), Nothing,
                (Ref{fmpz_poly}, Ref{fmpz_poly},  Ref{fmpz_poly}),
               z, x, y)
   return z
end

###############################################################################
#
#   Ad hoc binary operators
#
###############################################################################

function *(x::Int, y::fmpz_poly)
   z = parent(y)()
   ccall((:fmpz_poly_scalar_mul_si, libflint), Nothing,
                (Ref{fmpz_poly}, Ref{fmpz_poly}, Int), z, y, x)
   return z
end

function *(x::fmpz, y::fmpz_poly)
   z = parent(y)()
   ccall((:fmpz_poly_scalar_mul_fmpz, libflint), Nothing,
                (Ref{fmpz_poly}, Ref{fmpz_poly}, Ref{fmpz}), z, y, x)
   return z
end

function +(x::fmpz_poly, y::Int)
   z = parent(x)()
   ccall((:fmpz_poly_add_si, libflint), Nothing,
                (Ref{fmpz_poly}, Ref{fmpz_poly}, Int), z, x, y)
   return z
end

function +(x::fmpz_poly, y::fmpz)
   z = parent(x)()
   ccall((:fmpz_poly_add_fmpz, libflint), Nothing,
                (Ref{fmpz_poly}, Ref{fmpz_poly}, Ref{fmpz}), z, x, y)
   return z
end

function -(x::fmpz_poly, y::Int)
   z = parent(x)()
   ccall((:fmpz_poly_sub_si, libflint), Nothing,
                (Ref{fmpz_poly}, Ref{fmpz_poly}, Int), z, x, y)
   return z
end

function -(x::fmpz_poly, y::fmpz)
   z = parent(x)()
   ccall((:fmpz_poly_sub_fmpz, libflint), Nothing,
                (Ref{fmpz_poly}, Ref{fmpz_poly}, Ref{fmpz}), z, x, y)
   return z
end

function -(x::Int, y::fmpz_poly)
   z = parent(y)()
   ccall((:fmpz_poly_si_sub, libflint), Nothing,
                (Ref{fmpz_poly}, Int, Ref{fmpz_poly}), z, x, y)
   return z
end

function -(x::fmpz, y::fmpz_poly)
   z = parent(y)()
   ccall((:fmpz_poly_fmpz_sub, libflint), Nothing,
                (Ref{fmpz_poly}, Ref{fmpz}, Ref{fmpz_poly}), z, x, y)
   return z
end

+(x::Int, y::fmpz_poly) = y + x

+(x::fmpz, y::fmpz_poly) = y + x

*(x::fmpz_poly, y::Int) = y*x

*(x::fmpz_poly, y::fmpz) = y*x

+(x::Integer, y::fmpz_poly) = y + fmpz(x)

-(x::Integer, y::fmpz_poly) = fmpz(x) - y

*(x::Integer, y::fmpz_poly) = fmpz(x)*y

+(x::fmpz_poly, y::Integer) = x + fmpz(y)

-(x::fmpz_poly, y::Integer) = x - fmpz(y)

*(x::fmpz_poly, y::Integer) = fmpz(y)*x

###############################################################################
#
#   Powering
#
###############################################################################

function ^(x::fmpz_poly, y::Int)
   y < 0 && throw(DomainError(y, "Exponent must be non-negative"))
   z = parent(x)()
   ccall((:fmpz_poly_pow, libflint), Nothing,
                (Ref{fmpz_poly}, Ref{fmpz_poly}, Int),
               z, x, y)
   return z
end

###############################################################################
#
#   Comparisons
#
###############################################################################

function ==(x::fmpz_poly, y::fmpz_poly)
   check_parent(x, y)
   return ccall((:fmpz_poly_equal, libflint), Bool,
                (Ref{fmpz_poly}, Ref{fmpz_poly}), x, y)
end

###############################################################################
#
#   Ad hoc comparisons
#
###############################################################################

function ==(x::fmpz_poly, y::fmpz)
   if length(x) > 1
      return false
   elseif length(x) == 1
      z = fmpz()
      ccall((:fmpz_poly_get_coeff_fmpz, libflint), Nothing,
                       (Ref{fmpz}, Ref{fmpz_poly}, Int), z, x, 0)
      return ccall((:fmpz_equal, libflint), Bool,
               (Ref{fmpz}, Ref{fmpz}, Int), z, y, 0)
   else
      return iszero(y)
   end
end

==(x::fmpz, y::fmpz_poly) = y == x

==(x::fmpz_poly, y::Integer) = x == fmpz(y)

==(x::Integer, y::fmpz_poly) = y == x

###############################################################################
#
#   Truncation
#
###############################################################################

function truncate(a::fmpz_poly, n::Int)
   n < 0 && throw(DomainError(n, "Index must be non-negative"))

   if length(a) <= n
      return a
   end

   z = parent(a)()
   ccall((:fmpz_poly_set_trunc, libflint), Nothing,
                (Ref{fmpz_poly}, Ref{fmpz_poly}, Int), z, a, n)
   return z
end

function mullow(x::fmpz_poly, y::fmpz_poly, n::Int)
   check_parent(x, y)
   n < 0 && throw(DomainError(n, "Index must be non-negative"))

   z = parent(x)()
   ccall((:fmpz_poly_mullow, libflint), Nothing,
         (Ref{fmpz_poly}, Ref{fmpz_poly}, Ref{fmpz_poly}, Int), z, x, y, n)
   return z
end

###############################################################################
#
#   Reversal
#
###############################################################################

function reverse(x::fmpz_poly, len::Int)
   len < 0 && throw(DomainError(len, "Index must be non-negative"))
   z = parent(x)()
   ccall((:fmpz_poly_reverse, libflint), Nothing,
                (Ref{fmpz_poly}, Ref{fmpz_poly}, Int), z, x, len)
   return z
end

###############################################################################
#
#   Shifting
#
###############################################################################

function shift_left(x::fmpz_poly, len::Int)
   len < 0 && throw(DomainError(len, "Shift must be non-negative"))
   z = parent(x)()
   ccall((:fmpz_poly_shift_left, libflint), Nothing,
      (Ref{fmpz_poly}, Ref{fmpz_poly}, Int), z, x, len)
   return z
end

function shift_right(x::fmpz_poly, len::Int)
   len < 0 && throw(DomainError(len, "Shift must be non-negative"))
   z = parent(x)()
   ccall((:fmpz_poly_shift_right, libflint), Nothing,
       (Ref{fmpz_poly}, Ref{fmpz_poly}, Int), z, x, len)
   return z
end

###############################################################################
#
#   Exact division
#
###############################################################################

function divexact(x::fmpz_poly, y::fmpz_poly)
   check_parent(x, y)
   iszero(y) && throw(DivideError())
   z = parent(x)()
   ccall((:fmpz_poly_div, libflint), Nothing,
            (Ref{fmpz_poly}, Ref{fmpz_poly}, Ref{fmpz_poly}), z, x, y)
   return z
end

function Base.divrem(x::fmpz_poly, y::fmpz_poly)
   check_parent(x, y)
   iszero(y) && throw(DivideError())
   z = parent(x)()
   r = parent(x)()
   ccall((:fmpz_poly_divrem, libflint), Nothing,
            (Ref{fmpz_poly}, Ref{fmpz_poly}, Ref{fmpz_poly}, Ref{fmpz_poly}), z, r, x, y)
   return z, r
end

function divides(x::fmpz_poly, y::fmpz_poly)
   check_parent(x, y)
   iszero(y) && throw(DivideError())
   z = parent(x)()
   flag = Bool(ccall((:fmpz_poly_divides, libflint), Cint,
           (Ref{fmpz_poly}, Ref{fmpz_poly}, Ref{fmpz_poly}), z, x, y))
   return flag, z
end

###############################################################################
#
#   Ad hoc exact division
#
###############################################################################

function divexact(x::fmpz_poly, y::fmpz)
   iszero(y) && throw(DivideError())
   z = parent(x)()
   ccall((:fmpz_poly_scalar_divexact_fmpz, libflint), Nothing,
          (Ref{fmpz_poly}, Ref{fmpz_poly}, Ref{fmpz}), z, x, y)
   return z
end

function divexact(x::fmpz_poly, y::Int)
   y == 0 && throw(DivideError())
   z = parent(x)()
   ccall((:fmpz_poly_scalar_divexact_si, libflint), Nothing,
                        (Ref{fmpz_poly}, Ref{fmpz_poly}, Int), z, x, y)
   return z
end

divexact(x::fmpz_poly, y::Integer) = divexact(x, fmpz(y))

###############################################################################
#
#   Pseudodivision
#
###############################################################################

function pseudorem(x::fmpz_poly, y::fmpz_poly)
   check_parent(x, y)
   iszero(y) && throw(DivideError())
   diff = length(x) - length(y) + 1
   r = parent(x)()
   d = Vector{Int}(undef, 1)
   ccall((:fmpz_poly_pseudo_rem, libflint), Nothing,
     (Ref{fmpz_poly}, Ptr{Int}, Ref{fmpz_poly}, Ref{fmpz_poly}), r, d, x, y)
   if (diff > d[1])
      return leading_coefficient(y)^(diff - d[1])*r
   else
      return r
   end
end

function pseudodivrem(x::fmpz_poly, y::fmpz_poly)
   check_parent(x, y)
   iszero(y) && throw(DivideError())
   diff = length(x) - length(y) + 1
   q = parent(x)()
   r = parent(x)()
   d = Vector{Int}(undef, 1)
   ccall((:fmpz_poly_pseudo_divrem_divconquer, libflint), Nothing,
    (Ref{fmpz_poly}, Ref{fmpz_poly}, Ptr{Int}, Ref{fmpz_poly}, Ref{fmpz_poly}),
               q, r, d, x, y)
   if (diff > d[1])
      m = leading_coefficient(y)^(diff - d[1])
      return m*q, m*r
   else
      return q, r
   end
end

###############################################################################
#
#   Content, primitive part, GCD and LCM
#
###############################################################################

function gcd(x::fmpz_poly, y::fmpz_poly)
   check_parent(x, y)
   z = parent(x)()
   ccall((:fmpz_poly_gcd, libflint), Nothing,
                (Ref{fmpz_poly}, Ref{fmpz_poly}, Ref{fmpz_poly}), z, x, y)
   return z
end

function content(x::fmpz_poly)
   z = fmpz()
   ccall((:fmpz_poly_content, libflint), Nothing,
         (Ref{fmpz}, Ref{fmpz_poly}), z, x)
   return z
end

function primpart(x::fmpz_poly)
   z = parent(x)()
   ccall((:fmpz_poly_primitive_part, libflint), Nothing,
         (Ref{fmpz_poly}, Ref{fmpz_poly}), z, x)
   return z
end

###############################################################################
#
#   Square root
#
###############################################################################

function Base.sqrt(x::fmpz_poly)
    z = parent(x)()
    flag = Bool(ccall((:fmpz_poly_sqrt, libflint), Cint,
          (Ref{fmpz_poly}, Ref{fmpz_poly}), z, x))
    flag == false && error("Not a square in sqrt")
    return z
 end

###############################################################################
#
#   Evaluation
#
###############################################################################

function evaluate(x::fmpz_poly, y::fmpz)
   z = fmpz()
   ccall((:fmpz_poly_evaluate_fmpz, libflint), Nothing,
        (Ref{fmpz}, Ref{fmpz_poly}, Ref{fmpz}), z, x, y)
   return z
end

evaluate(x::fmpz_poly, y::Integer) = evaluate(x, fmpz(y))

###############################################################################
#
#   Composition
#
###############################################################################

function compose(x::fmpz_poly, y::fmpz_poly)
   check_parent(x, y)
   z = parent(x)()
   ccall((:fmpz_poly_compose, libflint), Nothing,
                (Ref{fmpz_poly}, Ref{fmpz_poly}, Ref{fmpz_poly}), z, x, y)
   return z
end

###############################################################################
#
#   Derivative
#
###############################################################################

function derivative(x::fmpz_poly)
   z = parent(x)()
   ccall((:fmpz_poly_derivative, libflint), Nothing,
                (Ref{fmpz_poly}, Ref{fmpz_poly}), z, x)
   return z
end

###############################################################################
#
#   Resultant
#
###############################################################################

function resultant(x::fmpz_poly, y::fmpz_poly)
   check_parent(x, y)
   z = fmpz()
   ccall((:fmpz_poly_resultant, libflint), Nothing,
                (Ref{fmpz}, Ref{fmpz_poly}, Ref{fmpz_poly}), z, x, y)
   return z
end

###############################################################################
#
#   Discriminant
#
###############################################################################

function discriminant(x::fmpz_poly)
   z = fmpz()
   ccall((:fmpz_poly_discriminant, libflint), Nothing,
                (Ref{fmpz}, Ref{fmpz_poly}), z, x)
   return z
end

###############################################################################
#
#   RESX
#
###############################################################################

function resx(a::fmpz_poly, b::fmpz_poly)
   check_parent(a, b)
   lena = length(a)
   lenb = length(b)
   if lena == 0 || lenb == 0
      return fmpz(), parent(a)(), parent(a)()
   end
   (lena <= 1 && lenb <= 1) && error("Constant polynomials in resx")
   z = fmpz()
   u = parent(a)()
   v = parent(a)()
   c1 = content(a)
   c2 = content(b)
   x = divexact(a, c1)
   y = divexact(b, c2)
   ccall((:fmpz_poly_xgcd_modular, libflint), Nothing,
   (Ref{fmpz}, Ref{fmpz_poly}, Ref{fmpz_poly}, Ref{fmpz_poly}, Ref{fmpz_poly}),
            z, u, v, x, y)
   r = z*c1^(lenb - 1)*c2^(lena - 1)
   if lenb > 1
      u *= c1^(lenb - 2)*c2^(lena - 1)
   else
      u *= c2^(lena - 1)
      u = divexact(u, c1)
   end
   if lena > 1
      v *= c1^(lenb - 1)*c2^(lena - 2)
   else
      v *= c1^(lenb - 1)
      v = divexact(v, c2)
   end
   return (r, u, v)
end

###############################################################################
#
#   Signature
#
###############################################################################

@doc Markdown.doc"""
    signature(f::fmpz_poly)

Return the signature of the polynomial $f$, i.e. a tuple $(r, s)$ such that
$r$ is the number of real roots of $f$ and $s$ is half the number of complex
roots.
"""
function signature(f::fmpz_poly)
   r = Vector{Int}(undef, 1)
   s = Vector{Int}(undef, 1)
   ccall((:fmpz_poly_signature, libflint), Nothing,
         (Ptr{Int}, Ptr{Int}, Ref{fmpz_poly}), r, s, f)
   return (r[1], s[1])
end

################################################################################
#
#  Interpolation
#
################################################################################

function interpolate(R::FmpzPolyRing, x::Array{fmpz, 1},
                                      y::Array{fmpz, 1})
  z = R()

  ax = Vector{Int}(undef, length(x))
  ay = Vector{Int}(undef, length(y))

  t = fmpz()

  for i in 1:length(x)
    ax[i] = x[i].d
    ay[i] = y[i].d
  end

  ccall((:fmpz_poly_interpolate_fmpz_vec, libflint), Nothing,
          (Ref{fmpz_poly}, Ptr{Int}, Ptr{Int}, Int),
          z, ax, ay, length(x))
  return z
end

################################################################################
#
#  Factorization
#
################################################################################

@doc Markdown.doc"""
    factor(x::fmpz_poly)

Returns the factorization of $x$.
"""
function factor(x::fmpz_poly)
  fac, z = _factor(x)
  ffac = factor(z)

  for (p, e) in ffac
    fac[parent(x)(p)] = e
  end

  return Fac(parent(x)(unit(ffac)), fac)
end

function _factor(x::fmpz_poly)
  fac = fmpz_poly_factor()
  ccall((:fmpz_poly_factor, libflint), Nothing,
              (Ref{fmpz_poly_factor}, Ref{fmpz_poly}), fac, x)
  res = Dict{fmpz_poly,Int}()
  z = fmpz()
  ccall((:fmpz_poly_factor_get_fmpz, libflint), Nothing,
            (Ref{fmpz}, Ref{fmpz_poly_factor}), z, fac)
  for i in 1:fac.num
    f = parent(x)()
    ccall((:fmpz_poly_factor_get_fmpz_poly, libflint), Nothing,
            (Ref{fmpz_poly}, Ref{fmpz_poly_factor}, Int), f, fac, i - 1)
    e = unsafe_load(fac.exp, i)
    res[f] = e
  end
  return res, z
end

@doc Markdown.doc"""
    isirreducible(x::fmpz_poly)

Checks if $x$ is irreducible.
"""
function isirreducible(x::fmpz_poly)
   if degree(x) == 0
     return isprime(coeff(x, 0))
   end
   res, z = _factor(x)
   if abs(z) == 1
     return length(res) == 1 && first(values(res)) == 1
   else
     return false
   end
end

###############################################################################
#
#   Special polynomials
#
###############################################################################

function chebyshev_t(n::Int, x::fmpz_poly)
   z = parent(x)()
   ccall((:fmpz_poly_chebyshev_t, libflint), Nothing,
                                                  (Ref{fmpz_poly}, Int), z, n)
   return isgen(x) ? z : compose(z, x)
end

function chebyshev_u(n::Int, x::fmpz_poly)
   z = parent(x)()
   ccall((:fmpz_poly_chebyshev_u, libflint), Nothing,
                                                  (Ref{fmpz_poly}, Int), z, n)
   return isgen(x) ? z : compose(z, x)
end

@doc Markdown.doc"""
    cyclotomic(n::Int, x::fmpz_poly)

Return the $n$th cyclotomic polynomial, defined as
$$\Phi_n(x) = \prod_{\omega} (x-\omega),$$ where $\omega$ runs over all the
$n$th primitive roots of unity.
"""
function cyclotomic(n::Int, x::fmpz_poly)
   z = parent(x)()
   ccall((:fmpz_poly_cyclotomic, libflint), Nothing,
                                                  (Ref{fmpz_poly}, Int), z, n)
   return isgen(x) ? z : compose(z, x)
end

@doc Markdown.doc"""
    swinnerton_dyer(n::Int, x::fmpz_poly)

Return the Swinnerton-Dyer polynomial $S_n$, defined as the integer
polynomial
$$S_n = \prod (x \pm \sqrt{2} \pm \sqrt{3} \pm \sqrt{5} \pm \ldots \pm \sqrt{p_n})$$
where $p_n$ denotes the $n$-th prime number and all combinations of signs are
taken. This polynomial has degree $2^n$ and is irreducible over the integers
(it is the minimal polynomial of $\sqrt{2} + \ldots + \sqrt{p_n}$).
"""
function swinnerton_dyer(n::Int, x::fmpz_poly)
   z = parent(x)()
   ccall((:fmpz_poly_swinnerton_dyer, libflint), Nothing,
                                                  (Ref{fmpz_poly}, Int), z, n)
   return isgen(x) ? z : compose(z, x)
end

@doc Markdown.doc"""
    cos_minpoly(n::Int, x::fmpz_poly)

Return the minimal polynomial of $2 \cos(2 \pi / n)$. For suitable choice of
$n$, this gives the minimal polynomial of $2 \cos(a \pi)$ or $2 \sin(a \pi)$ for any
rational $a$.
"""
function cos_minpoly(n::Int, x::fmpz_poly)
   z = parent(x)()
   ccall((:fmpz_poly_cos_minpoly, libflint), Nothing,
                                                  (Ref{fmpz_poly}, Int), z, n)
   return isgen(x) ? z : compose(z, x)
end

@doc Markdown.doc"""
    theta_qexp(e::Int, n::Int, x::fmpz_poly)

Return the $q$-expansion to length $n$ of the Jacobi theta function raised to
the power $r$, i.e. $\vartheta(q)^r$ where
$\vartheta(q) = 1 + \sum_{k=1}^{\infty} q^{k^2}$.
"""
function theta_qexp(e::Int, n::Int, x::fmpz_poly)
   z = parent(x)()
   ccall((:fmpz_poly_theta_qexp, libflint), Nothing,
                                          (Ref{fmpz_poly}, Int, Int), z, e, n)
   return isgen(x) ? z : compose(z, x)
end

@doc Markdown.doc"""
    eta_qexp(e::Int, n::Int, x::fmpz_poly)

Return the $q$-expansion to length $n$ of the Dedekind eta function (without
the leading factor $q^{1/24}$) raised to the power $r$, i.e.
$(q^{-1/24} \eta(q))^r = \prod_{k=1}^{\infty} (1 - q^k)^r$.
In particular, $r = -1$ gives the generating function of the partition
function $p(k)$, and $r = 24$ gives, after multiplication by $q$, the modular
discriminant $\Delta(q)$ which generates the Ramanujan tau function
$\tau(k)$.
"""
function eta_qexp(e::Int, n::Int, x::fmpz_poly)
   z = parent(x)()
   ccall((:fmpz_poly_eta_qexp, libflint), Nothing,
                                          (Ref{fmpz_poly}, Int, Int), z, e, n)
   return isgen(x) ? z : compose(z, x)
end

###############################################################################
#
#   Speedups for polynomials over fmpz_polys
#
###############################################################################

function *(a::Generic.Poly{fmpz_poly}, b::Generic.Poly{fmpz_poly})
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

function zero!(z::fmpz_poly)
   ccall((:fmpz_poly_zero, libflint), Nothing,
                    (Ref{fmpz_poly},), z)
   return z
end

function fit!(z::fmpz_poly, n::Int)
   ccall((:fmpz_poly_fit_length, libflint), Nothing,
                    (Ref{fmpz_poly}, Int), z, n)
   return nothing
end

function setcoeff!(z::fmpz_poly, n::Int, x::fmpz)
   ccall((:fmpz_poly_set_coeff_fmpz, libflint), Nothing,
                    (Ref{fmpz_poly}, Int, Ref{fmpz}), z, n, x)
   return z
end

function mul!(z::fmpz_poly, x::fmpz_poly, y::fmpz_poly)
   ccall((:fmpz_poly_mul, libflint), Nothing,
                (Ref{fmpz_poly}, Ref{fmpz_poly}, Ref{fmpz_poly}), z, x, y)
   return z
end

function addeq!(z::fmpz_poly, x::fmpz_poly)
   ccall((:fmpz_poly_add, libflint), Nothing,
                (Ref{fmpz_poly}, Ref{fmpz_poly}, Ref{fmpz_poly}), z, z, x)
   return z
end

function add!(z::fmpz_poly, x::fmpz_poly, y::fmpz_poly)
   ccall((:fmpz_poly_add, libflint), Nothing,
                (Ref{fmpz_poly}, Ref{fmpz_poly}, Ref{fmpz_poly}), z, x, y)
   return z
end

###############################################################################
#
#   Promotions
#
###############################################################################

promote_rule(::Type{fmpz_poly}, ::Type{T}) where {T <: Integer} = fmpz_poly

promote_rule(::Type{fmpz_poly}, ::Type{fmpz}) = fmpz_poly

###############################################################################
#
#   Parent object call overloads
#
###############################################################################

function (a::FmpzPolyRing)()
   z = fmpz_poly()
   z.parent = a
   return z
end

function (a::FmpzPolyRing)(b::Int)
   z = fmpz_poly(b)
   z.parent = a
   return z
end

function (a::FmpzPolyRing)(b::Integer)
   z = fmpz_poly(fmpz(b))
   z.parent = a
   return z
end

function (a::FmpzPolyRing)(b::fmpz)
   z = fmpz_poly(b)
   z.parent = a
   return z
end

function (a::FmpzPolyRing)(b::Array{fmpz, 1})
   z = fmpz_poly(b)
   z.parent = a
   return z
end

(a::FmpzPolyRing)(b::Array{T, 1}) where {T <: Integer} = a(map(fmpz, b))

(a::FmpzPolyRing)(b::fmpz_poly) = b

###############################################################################
#
#   PolynomialRing constructor
#
###############################################################################

function PolynomialRing(R::FlintIntegerRing, s::Symbol; cached = true)
   parent_obj = FmpzPolyRing(R, s, cached)

   return parent_obj, parent_obj([fmpz(0), fmpz(1)])
end

function PolynomialRing(R::FlintIntegerRing, s::AbstractString; cached = true)
   return PolynomialRing(R, Symbol(s); cached=cached)
end

function PolyRing(R::FlintIntegerRing)
   return FmpzPolyRing(R, :x, false)
end
