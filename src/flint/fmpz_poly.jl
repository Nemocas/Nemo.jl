###############################################################################
#
#   ZZPolyRingElem.jl : Flint polynomials over ZZRingElem
#
###############################################################################

###############################################################################
#
#   Data type and parent methods
#
###############################################################################

parent_type(::Type{ZZPolyRingElem}) = ZZPolyRing

elem_type(::Type{ZZPolyRing}) = ZZPolyRingElem

dense_poly_type(::Type{ZZRingElem}) = ZZPolyRingElem

base_ring(a::ZZPolyRing) = ZZ

parent(a::ZZPolyRingElem) = a.parent

var(a::ZZPolyRing) = a.S

###############################################################################
#
#   Basic manipulation
#
###############################################################################

length(x::ZZPolyRingElem) = ccall((:fmpz_poly_length, libflint), Int,
                                  (Ref{ZZPolyRingElem},), x)

function coeff(x::ZZPolyRingElem, n::Int)
  n < 0 && throw(DomainError(n, "Index must be non-negative"))
  z = ZZRingElem()
  ccall((:fmpz_poly_get_coeff_fmpz, libflint), Nothing,
        (Ref{ZZRingElem}, Ref{ZZPolyRingElem}, Int), z, x, n)
  return z
end

zero(a::ZZPolyRing) = a(0)

one(a::ZZPolyRing) = a(1)

gen(a::ZZPolyRing) = a([zero(base_ring(a)), one(base_ring(a))])

is_gen(x::ZZPolyRingElem) = ccall((:fmpz_poly_is_gen, libflint), Bool,
                                  (Ref{ZZPolyRingElem},), x)

function deepcopy_internal(a::ZZPolyRingElem, dict::IdDict)
  z = ZZPolyRingElem(a)
  z.parent = parent(a)
  return z
end

@doc raw"""
    height(a::ZZPolyRingElem)

Return the largest of the absolute values of the coefficients of a.
"""
function height(a::ZZPolyRingElem)
  z = ZZRingElem()
  ccall((:fmpz_poly_height, libflint), Nothing,
        (Ref{ZZRingElem}, Ref{ZZPolyRingElem}), z, a)
  return z
end

normalise(f::ZZPolyRingElem, ::Int) = degree(f) + 1

set_length!(f::ZZPolyRingElem, ::Int) = nothing

###############################################################################
#
#   Similar and zero
#
###############################################################################

function similar(f::PolyRingElem, R::ZZRing, s::Symbol=var(parent(f)); cached::Bool=true)
  z = ZZPolyRingElem()
  if base_ring(f) === R && s == var(parent(f)) && f isa ZZPolyRingElem
    # steal parent in case it is not cached
    z.parent = parent(f)
  else
    z.parent = ZZPolyRing(R, s, cached)
  end
  return z
end

###############################################################################
#
#   polynomial constructor
#
###############################################################################

function polynomial(R::ZZRing, arr::Vector{T}, var::VarName=:x; cached::Bool=true) where T
  coeffs = T == ZZRingElem ? arr : map(R, arr)
  coeffs = length(coeffs) == 0 ? ZZRingElem[] : coeffs
  z = ZZPolyRingElem(coeffs)
  z.parent = ZZPolyRing(R, Symbol(var), cached)
  return z
end

###############################################################################
#
#   Canonicalisation
#
###############################################################################

canonical_unit(a::ZZPolyRingElem) = canonical_unit(leading_coefficient(a))

###############################################################################
#
#   Unary operations
#
###############################################################################

-(x::ZZPolyRingElem) = neg!(parent(x)(), x)

###############################################################################
#
#   Binary operations
#
###############################################################################

function +(x::ZZPolyRingElem, y::ZZPolyRingElem)
  check_parent(x, y)
  z = parent(x)()
  return add!(z, x, y)
end

function -(x::ZZPolyRingElem, y::ZZPolyRingElem)
  check_parent(x, y)
  z = parent(x)()
  return sub!(z, x, y)
end

function *(x::ZZPolyRingElem, y::ZZPolyRingElem)
  check_parent(x, y)
  z = parent(x)()
  return mul!(z, x, y)
end

###############################################################################
#
#   Ad hoc binary operators
#
###############################################################################

for T in [Integer, ZZRingElem]
  for (jop, cop) in ((:+,:add!), (:-,:sub!), (:*,:mul!))
    @eval begin
      $jop(a::ZZPolyRingElem, b::$T) = $cop(similar(a), a, b)
      $jop(a::$T, b::ZZPolyRingElem) = $cop(similar(b), a, b)
    end
  end
end

###############################################################################
#
#   Powering
#
###############################################################################

function ^(x::ZZPolyRingElem, y::Int)
  y < 0 && throw(DomainError(y, "Exponent must be non-negative"))
  z = parent(x)()
  ccall((:fmpz_poly_pow, libflint), Nothing,
        (Ref{ZZPolyRingElem}, Ref{ZZPolyRingElem}, Int),
        z, x, y)
  return z
end

###############################################################################
#
#   Comparisons
#
###############################################################################

function ==(x::ZZPolyRingElem, y::ZZPolyRingElem)
  check_parent(x, y)
  return ccall((:fmpz_poly_equal, libflint), Bool,
               (Ref{ZZPolyRingElem}, Ref{ZZPolyRingElem}), x, y)
end

###############################################################################
#
#   Ad hoc comparisons
#
###############################################################################

function ==(x::ZZPolyRingElem, y::ZZRingElem)
  if length(x) > 1
    return false
  elseif length(x) == 1
    z = ZZRingElem()
    ccall((:fmpz_poly_get_coeff_fmpz, libflint), Nothing,
          (Ref{ZZRingElem}, Ref{ZZPolyRingElem}, Int), z, x, 0)
    return ccall((:fmpz_equal, libflint), Bool,
                 (Ref{ZZRingElem}, Ref{ZZRingElem}, Int), z, y, 0)
  else
    return iszero(y)
  end
end

==(x::ZZRingElem, y::ZZPolyRingElem) = y == x

==(x::ZZPolyRingElem, y::Integer) = x == ZZRingElem(y)

==(x::Integer, y::ZZPolyRingElem) = y == x

###############################################################################
#
#   Truncation
#
###############################################################################

function truncate(a::ZZPolyRingElem, n::Int)
  n < 0 && throw(DomainError(n, "Index must be non-negative"))

  if length(a) <= n
    return a
  end

  z = parent(a)()
  ccall((:fmpz_poly_set_trunc, libflint), Nothing,
        (Ref{ZZPolyRingElem}, Ref{ZZPolyRingElem}, Int), z, a, n)
  return z
end

function mullow(x::ZZPolyRingElem, y::ZZPolyRingElem, n::Int)
  check_parent(x, y)
  n < 0 && throw(DomainError(n, "Index must be non-negative"))

  z = parent(x)()
  ccall((:fmpz_poly_mullow, libflint), Nothing,
        (Ref{ZZPolyRingElem}, Ref{ZZPolyRingElem}, Ref{ZZPolyRingElem}, Int), z, x, y, n)
  return z
end

###############################################################################
#
#   Reversal
#
###############################################################################

function reverse(x::ZZPolyRingElem, len::Int)
  len < 0 && throw(DomainError(len, "Index must be non-negative"))
  z = parent(x)()
  ccall((:fmpz_poly_reverse, libflint), Nothing,
        (Ref{ZZPolyRingElem}, Ref{ZZPolyRingElem}, Int), z, x, len)
  return z
end

###############################################################################
#
#   Shifting
#
###############################################################################

function shift_left(x::ZZPolyRingElem, len::Int)
  len < 0 && throw(DomainError(len, "Shift must be non-negative"))
  z = parent(x)()
  ccall((:fmpz_poly_shift_left, libflint), Nothing,
        (Ref{ZZPolyRingElem}, Ref{ZZPolyRingElem}, Int), z, x, len)
  return z
end

function shift_right(x::ZZPolyRingElem, len::Int)
  len < 0 && throw(DomainError(len, "Shift must be non-negative"))
  z = parent(x)()
  ccall((:fmpz_poly_shift_right, libflint), Nothing,
        (Ref{ZZPolyRingElem}, Ref{ZZPolyRingElem}, Int), z, x, len)
  return z
end

###############################################################################
#
#   Exact division
#
###############################################################################

function divexact(x::ZZPolyRingElem, y::ZZPolyRingElem; check::Bool=true)
  check_parent(x, y)
  iszero(y) && throw(DivideError())
  z = parent(x)()
  if check
    r = parent(x)()
    ccall((:fmpz_poly_divrem, libflint), Nothing,
          (Ref{ZZPolyRingElem}, Ref{ZZPolyRingElem}, Ref{ZZPolyRingElem}, Ref{ZZPolyRingElem}),
          z, r, x, y)
    r != 0 && error("Not an exact division")
  else
    ccall((:fmpz_poly_div, libflint), Nothing,
          (Ref{ZZPolyRingElem}, Ref{ZZPolyRingElem}, Ref{ZZPolyRingElem}), z, x, y)
  end
  return z
end

function Base.divrem(x::ZZPolyRingElem, y::ZZPolyRingElem)
  check_parent(x, y)
  iszero(y) && throw(DivideError())
  z = parent(x)()
  r = parent(x)()
  ccall((:fmpz_poly_divrem, libflint), Nothing,
        (Ref{ZZPolyRingElem}, Ref{ZZPolyRingElem}, Ref{ZZPolyRingElem}, Ref{ZZPolyRingElem}), z, r, x, y)
  return z, r
end

mod(x::ZZPolyRingElem, y::ZZPolyRingElem) = divrem(x, y)[2]

function divides(x::ZZPolyRingElem, y::ZZPolyRingElem)
  check_parent(x, y)
  iszero(y) && throw(DivideError())
  z = parent(x)()
  flag = Bool(ccall((:fmpz_poly_divides, libflint), Cint,
                    (Ref{ZZPolyRingElem}, Ref{ZZPolyRingElem}, Ref{ZZPolyRingElem}), z, x, y))
  return flag, z
end

###############################################################################
#
#   Ad hoc exact division
#
###############################################################################

function divexact(x::ZZPolyRingElem, y::ZZRingElem; check::Bool=true)
  iszero(y) && throw(DivideError())
  z = parent(x)()
  ccall((:fmpz_poly_scalar_divexact_fmpz, libflint), Nothing,
        (Ref{ZZPolyRingElem}, Ref{ZZPolyRingElem}, Ref{ZZRingElem}), z, x, y)
  return z
end

function divexact(x::ZZPolyRingElem, y::Int; check::Bool=true)
  y == 0 && throw(DivideError())
  z = parent(x)()
  ccall((:fmpz_poly_scalar_divexact_si, libflint), Nothing,
        (Ref{ZZPolyRingElem}, Ref{ZZPolyRingElem}, Int), z, x, y)
  return z
end

divexact(x::ZZPolyRingElem, y::Integer; check::Bool=true) = divexact(x, flintify(y); check=check)

###############################################################################
#
#   Pseudodivision
#
###############################################################################

function pseudorem(x::ZZPolyRingElem, y::ZZPolyRingElem)
  check_parent(x, y)
  iszero(y) && throw(DivideError())
  diff = length(x) - length(y) + 1
  r = parent(x)()
  d = Vector{Int}(undef, 1)
  ccall((:fmpz_poly_pseudo_rem, libflint), Nothing,
        (Ref{ZZPolyRingElem}, Ptr{Int}, Ref{ZZPolyRingElem}, Ref{ZZPolyRingElem}), r, d, x, y)
  if (diff > d[1])
    return leading_coefficient(y)^(diff - d[1])*r
  else
    return r
  end
end

function pseudodivrem(x::ZZPolyRingElem, y::ZZPolyRingElem)
  check_parent(x, y)
  iszero(y) && throw(DivideError())
  diff = length(x) - length(y) + 1
  q = parent(x)()
  r = parent(x)()
  d = Vector{Int}(undef, 1)
  ccall((:fmpz_poly_pseudo_divrem_divconquer, libflint), Nothing,
        (Ref{ZZPolyRingElem}, Ref{ZZPolyRingElem}, Ptr{Int}, Ref{ZZPolyRingElem}, Ref{ZZPolyRingElem}),
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

function gcd(x::ZZPolyRingElem, y::ZZPolyRingElem)
  check_parent(x, y)
  z = parent(x)()
  ccall((:fmpz_poly_gcd, libflint), Nothing,
        (Ref{ZZPolyRingElem}, Ref{ZZPolyRingElem}, Ref{ZZPolyRingElem}), z, x, y)
  return z
end

function content(x::ZZPolyRingElem)
  z = ZZRingElem()
  ccall((:fmpz_poly_content, libflint), Nothing,
        (Ref{ZZRingElem}, Ref{ZZPolyRingElem}), z, x)
  return z
end

function primpart(x::ZZPolyRingElem)
  z = parent(x)()
  ccall((:fmpz_poly_primitive_part, libflint), Nothing,
        (Ref{ZZPolyRingElem}, Ref{ZZPolyRingElem}), z, x)
  return z
end

###############################################################################
#
#   Square root
#
###############################################################################

function Base.sqrt(x::ZZPolyRingElem; check::Bool=true)
  z = parent(x)()
  flag = Bool(ccall((:fmpz_poly_sqrt, libflint), Cint,
                    (Ref{ZZPolyRingElem}, Ref{ZZPolyRingElem}), z, x))
  check && flag == false && error("Not a square in sqrt")
  return z
end

function is_square(x::ZZPolyRingElem)
  z = parent(x)()
  flag = Bool(ccall((:fmpz_poly_sqrt, libflint), Cint,
                    (Ref{ZZPolyRingElem}, Ref{ZZPolyRingElem}), z, x))
  return flag
end

function is_square_with_sqrt(x::ZZPolyRingElem)
  R = parent(x)
  z = R()
  flag = Bool(ccall((:fmpz_poly_sqrt, libflint), Cint,
                    (Ref{ZZPolyRingElem}, Ref{ZZPolyRingElem}), z, x))
  if !flag
    return false, zero(R)
  end
  return true, z
end

###############################################################################
#
#   Evaluation
#
###############################################################################

function evaluate(x::ZZPolyRingElem, y::ZZRingElem)
  z = ZZRingElem()
  ccall((:fmpz_poly_evaluate_fmpz, libflint), Nothing,
        (Ref{ZZRingElem}, Ref{ZZPolyRingElem}, Ref{ZZRingElem}), z, x, y)
  return z
end

evaluate(x::ZZPolyRingElem, y::Integer) = evaluate(x, ZZRingElem(y))

###############################################################################
#
#   Composition
#
###############################################################################

function AbstractAlgebra._compose_right(x::ZZPolyRingElem, y::ZZPolyRingElem)
  check_parent(x, y)
  z = parent(x)()
  ccall((:fmpz_poly_compose, libflint), Nothing,
        (Ref{ZZPolyRingElem}, Ref{ZZPolyRingElem}, Ref{ZZPolyRingElem}), z, x, y)
  return z
end

###############################################################################
#
#   Derivative
#
###############################################################################

function derivative(x::ZZPolyRingElem)
  z = parent(x)()
  ccall((:fmpz_poly_derivative, libflint), Nothing,
        (Ref{ZZPolyRingElem}, Ref{ZZPolyRingElem}), z, x)
  return z
end

###############################################################################
#
#   Resultant
#
###############################################################################

function resultant(x::ZZPolyRingElem, y::ZZPolyRingElem)
  check_parent(x, y)
  z = ZZRingElem()
  ccall((:fmpz_poly_resultant, libflint), Nothing,
        (Ref{ZZRingElem}, Ref{ZZPolyRingElem}, Ref{ZZPolyRingElem}), z, x, y)
  return z
end

###############################################################################
#
#   Discriminant
#
###############################################################################

function discriminant(x::ZZPolyRingElem)
  z = ZZRingElem()
  ccall((:fmpz_poly_discriminant, libflint), Nothing,
        (Ref{ZZRingElem}, Ref{ZZPolyRingElem}), z, x)
  return z
end

###############################################################################
#
#   RESX
#
###############################################################################

function resx(a::ZZPolyRingElem, b::ZZPolyRingElem)
  check_parent(a, b)
  lena = length(a)
  lenb = length(b)
  if lena == 0 || lenb == 0
    return ZZRingElem(), parent(a)(), parent(a)()
  end
  (lena <= 1 && lenb <= 1) && error("Constant polynomials in resx")
  z = ZZRingElem()
  u = parent(a)()
  v = parent(a)()
  c1 = content(a)
  c2 = content(b)
  x = divexact(a, c1)
  y = divexact(b, c2)
  ccall((:fmpz_poly_xgcd_modular, libflint), Nothing,
        (Ref{ZZRingElem}, Ref{ZZPolyRingElem}, Ref{ZZPolyRingElem}, Ref{ZZPolyRingElem}, Ref{ZZPolyRingElem}),
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

@doc raw"""
    signature(f::ZZPolyRingElem)

Return the signature of $f$, i.e. a tuple $(r, s)$ such that $r$ is the number of
real roots of $f$ and $s$ is half the number of complex roots.

# Examples

```jldoctest
julia> R, x = polynomial_ring(ZZ, "x");

julia> signature(x^3 + 3x + 1)
(1, 1)
```
"""
function signature(f::ZZPolyRingElem)
  r = Vector{Int}(undef, 1)
  s = Vector{Int}(undef, 1)
  ccall((:fmpz_poly_signature, libflint), Nothing,
        (Ptr{Int}, Ptr{Int}, Ref{ZZPolyRingElem}), r, s, f)
  return (r[1], s[1])
end

################################################################################
#
#  Interpolation
#
################################################################################

function interpolate(R::ZZPolyRing, x::Vector{ZZRingElem},
    y::Vector{ZZRingElem})
  z = R()

  ax = Vector{Int}(undef, length(x))
  ay = Vector{Int}(undef, length(y))

  t = ZZRingElem()

  for i in 1:length(x)
    ax[i] = x[i].d
    ay[i] = y[i].d
  end

  ccall((:fmpz_poly_interpolate_fmpz_vec, libflint), Nothing,
        (Ref{ZZPolyRingElem}, Ptr{Int}, Ptr{Int}, Int),
        z, ax, ay, length(x))
  return z
end

################################################################################
#
#  Factorization
#
################################################################################

for (factor_fn, factor_fn_inner, flint_fn) in 
  [(:factor, :_factor, "fmpz_poly_factor"),
   (:factor_squarefree, :_factor_squarefree, "fmpz_poly_factor_squarefree")]
  eval(quote

         function $factor_fn(x::ZZPolyRingElem)
           iszero(x) && throw(ArgumentError("Argument must be non-zero"))
           fac, z = $factor_fn_inner(x)
           ffac = factor(z)

           for (p, e) in ffac
             fac[parent(x)(p)] = e
           end

           return Fac(parent(x)(unit(ffac)), fac)
         end

         function $factor_fn_inner(x::ZZPolyRingElem)
           fac = fmpz_poly_factor()
           ccall(($flint_fn, libflint), Nothing,
                 (Ref{fmpz_poly_factor}, Ref{ZZPolyRingElem}), fac, x)
           res = Dict{ZZPolyRingElem,Int}()
           z = ZZRingElem()
           ccall((:fmpz_poly_factor_get_fmpz, libflint), Nothing,
                 (Ref{ZZRingElem}, Ref{fmpz_poly_factor}), z, fac)
           for i in 1:fac.num
             f = parent(x)()
             ccall((:fmpz_poly_factor_get_fmpz_poly, libflint), Nothing,
                   (Ref{ZZPolyRingElem}, Ref{fmpz_poly_factor}, Int), f, fac, i - 1)
             e = unsafe_load(fac.exp, i)
             res[f] = e
           end
           return res, z
         end

       end)
end

function is_irreducible(x::ZZPolyRingElem)
  if degree(x) == 0
    return is_prime(coeff(x, 0))
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

function chebyshev_t(n::Int, x::ZZPolyRingElem)
  z = parent(x)()
  ccall((:fmpz_poly_chebyshev_t, libflint), Nothing,
        (Ref{ZZPolyRingElem}, Int), z, n)
  return is_gen(x) ? z : compose(z, x, inner = :second)
end

function chebyshev_u(n::Int, x::ZZPolyRingElem)
  z = parent(x)()
  ccall((:fmpz_poly_chebyshev_u, libflint), Nothing,
        (Ref{ZZPolyRingElem}, Int), z, n)
  return is_gen(x) ? z : compose(z, x, inner = :second)
end

@doc raw"""
    cyclotomic(n::Int, x::ZZPolyRingElem)

Return the $n$th cyclotomic polynomial, defined as
$$\Phi_n(x) = \prod_{\omega} (x-\omega),$$ where $\omega$ runs over all the
$n$th primitive roots of unity.
"""
function cyclotomic(n::Int, x::ZZPolyRingElem)
  z = parent(x)()
  ccall((:fmpz_poly_cyclotomic, libflint), Nothing,
        (Ref{ZZPolyRingElem}, Int), z, n)
  return is_gen(x) ? z : compose(z, x; inner = :second)
end

@doc raw"""
    swinnerton_dyer(n::Int, x::ZZPolyRingElem)

Return the Swinnerton-Dyer polynomial $S_n$, defined as the integer
polynomial
$$S_n = \prod (x \pm \sqrt{2} \pm \sqrt{3} \pm \sqrt{5} \pm \ldots \pm \sqrt{p_n})$$
where $p_n$ denotes the $n$-th prime number and all combinations of signs are
taken. This polynomial has degree $2^n$ and is irreducible over the integers
(it is the minimal polynomial of $\sqrt{2} + \ldots + \sqrt{p_n}$).
"""
function swinnerton_dyer(n::Int, x::ZZPolyRingElem)
  z = parent(x)()
  ccall((:fmpz_poly_swinnerton_dyer, libflint), Nothing,
        (Ref{ZZPolyRingElem}, Int), z, n)
  return is_gen(x) ? z : compose(z, x, inner = :second)
end

@doc raw"""
    cos_minpoly(n::Int, x::ZZPolyRingElem)

Return the minimal polynomial of $2 \cos(2 \pi / n)$. For suitable choice of
$n$, this gives the minimal polynomial of $2 \cos(a \pi)$ or $2 \sin(a \pi)$ for any
rational $a$.
"""
function cos_minpoly(n::Int, x::ZZPolyRingElem)
  z = parent(x)()
  ccall((:fmpz_poly_cos_minpoly, libflint), Nothing,
        (Ref{ZZPolyRingElem}, Int), z, n)
  return is_gen(x) ? z : compose(z, x, inner = :second)
end

@doc raw"""
    theta_qexp(e::Int, n::Int, x::ZZPolyRingElem)

Return the $q$-expansion to length $n$ of the Jacobi theta function raised to
the power $r$, i.e. $\vartheta(q)^r$ where
$\vartheta(q) = 1 + \sum_{k=1}^{\infty} q^{k^2}$.
"""
function theta_qexp(e::Int, n::Int, x::ZZPolyRingElem)
  z = parent(x)()
  ccall((:fmpz_poly_theta_qexp, libflint), Nothing,
        (Ref{ZZPolyRingElem}, Int, Int), z, e, n)
  return is_gen(x) ? z : compose(z, x, inner = :second)
end

@doc raw"""
    eta_qexp(e::Int, n::Int, x::ZZPolyRingElem)

Return the $q$-expansion to length $n$ of the Dedekind eta function (without
the leading factor $q^{1/24}$) raised to the power $r$, i.e.
$(q^{-1/24} \eta(q))^r = \prod_{k=1}^{\infty} (1 - q^k)^r$.
In particular, $r = -1$ gives the generating function of the partition
function $p(k)$, and $r = 24$ gives, after multiplication by $q$, the modular
discriminant $\Delta(q)$ which generates the Ramanujan tau function
$\tau(k)$.
"""
function eta_qexp(e::Int, n::Int, x::ZZPolyRingElem)
  z = parent(x)()
  ccall((:fmpz_poly_eta_qexp, libflint), Nothing,
        (Ref{ZZPolyRingElem}, Int, Int), z, e, n)
  return is_gen(x) ? z : compose(z, x, inner = :second)
end

###############################################################################
#
#   Speedups for polynomials over fmpz_polys
#
###############################################################################

function *(a::Generic.Poly{ZZPolyRingElem}, b::Generic.Poly{ZZPolyRingElem})
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

function zero!(z::ZZPolyRingElemOrPtr)
  ccall((:fmpz_poly_zero, libflint), Nothing,
        (Ref{ZZPolyRingElem},), z)
  return z
end

function one!(z::ZZPolyRingElemOrPtr)
  ccall((:fmpz_poly_one, libflint), Nothing,
        (Ref{ZZPolyRingElem},), z)
  return z
end

function neg!(z::ZZPolyRingElemOrPtr, a::ZZPolyRingElemOrPtr)
  ccall((:fmpz_poly_neg, libflint), Nothing,
        (Ref{ZZPolyRingElem}, Ref{ZZPolyRingElem}), z, a)
  return z
end

function fit!(z::ZZPolyRingElemOrPtr, n::Int)
  ccall((:fmpz_poly_fit_length, libflint), Nothing,
        (Ref{ZZPolyRingElem}, Int), z, n)
  return nothing
end

#

function set!(z::ZZPolyRingElemOrPtr, a::ZZPolyRingElemOrPtr)
  @ccall libflint.fmpz_poly_set(z::Ref{ZZPolyRingElem}, a::Ref{ZZPolyRingElem})::Nothing
  return z
end

function set!(z::ZZPolyRingElemOrPtr, a::ZZRingElemOrPtr)
  @ccall libflint.fmpz_poly_set_fmpz(z::Ref{ZZPolyRingElem}, a::Ref{ZZRingElem})::Nothing
  return z
end

function set!(z::ZZPolyRingElemOrPtr, a::Int)
  @ccall libflint.fmpz_poly_set_si(z::Ref{ZZPolyRingElem}, a::Int)::Nothing
  return z
end

function set!(z::ZZPolyRingElemOrPtr, a::UInt)
  @ccall libflint.fmpz_poly_set_ui(z::Ref{ZZPolyRingElem}, a::UInt)::Nothing
  return z
end

set!(z::ZZPolyRingElemOrPtr, a::Integer) = set!(z, flintify(a))

#

function setcoeff!(z::ZZPolyRingElemOrPtr, n::Int, x::ZZRingElemOrPtr)
  @ccall libflint.fmpz_poly_set_coeff_fmpz(z::Ref{ZZPolyRingElem}, n::Int, x::Ref{ZZRingElem})::Nothing
  return z
end

function setcoeff!(z::ZZPolyRingElemOrPtr, n::Int, x::Int)
  @ccall libflint.fmpz_poly_set_coeff_si(z::Ref{ZZPolyRingElem}, n::Int, x::Int)::Nothing
  return z
end

function setcoeff!(z::ZZPolyRingElemOrPtr, n::Int, x::UInt)
  @ccall libflint.fmpz_poly_set_coeff_ui(z::Ref{ZZPolyRingElem}, n::Int, x::UInt)::Nothing
  return z
end

setcoeff!(z::ZZPolyRingElemOrPtr, n::Int, x::Integer) = setcoeff!(z, n, flintify(x))

#

function add!(z::ZZPolyRingElemOrPtr, x::ZZPolyRingElemOrPtr, y::ZZPolyRingElemOrPtr)
  @ccall libflint.fmpz_poly_add(z::Ref{ZZPolyRingElem}, x::Ref{ZZPolyRingElem}, y::Ref{ZZPolyRingElem})::Nothing
  return z
end

function add!(z::ZZPolyRingElemOrPtr, x::ZZPolyRingElemOrPtr, y::ZZRingElemOrPtr)
  @ccall libflint.fmpz_poly_add_fmpz(z::Ref{ZZPolyRingElem}, x::Ref{ZZPolyRingElem}, y::Ref{ZZRingElem})::Nothing
  return z
end

function add!(z::ZZPolyRingElemOrPtr, x::ZZPolyRingElemOrPtr, y::Int)
  @ccall libflint.fmpz_poly_add_si(z::Ref{ZZPolyRingElem}, x::Ref{ZZPolyRingElem}, y::Int)::Nothing
  return z
end

add!(z::ZZPolyRingElemOrPtr, x::ZZPolyRingElemOrPtr, y::Integer) = add!(z, x, flintify(y))

add!(z::ZZPolyRingElemOrPtr, x::IntegerUnionOrPtr, y::ZZPolyRingElemOrPtr) = add!(z, y, x)

#

function sub!(z::ZZPolyRingElemOrPtr, x::ZZPolyRingElemOrPtr, y::ZZPolyRingElemOrPtr)
  @ccall libflint.fmpz_poly_sub(z::Ref{ZZPolyRingElem}, x::Ref{ZZPolyRingElem}, y::Ref{ZZPolyRingElem})::Nothing
  return z
end

function sub!(z::ZZPolyRingElemOrPtr, x::ZZPolyRingElemOrPtr, y::ZZRingElemOrPtr)
  if is_zero(y)
    # HACK HACK HACK: workaround a crash in fmpz_poly_sub_fmpz when subtracting
    # 0 from a zero polynomial; see https://github.com/flintlib/flint/pull/2102
    set!(z, x)
  else
    @ccall libflint.fmpz_poly_sub_fmpz(z::Ref{ZZPolyRingElem}, x::Ref{ZZPolyRingElem}, y::Ref{ZZRingElem})::Nothing
  end
  return z
end

function sub!(z::ZZPolyRingElemOrPtr, x::ZZPolyRingElemOrPtr, y::Int)
  @ccall libflint.fmpz_poly_sub_si(z::Ref{ZZPolyRingElem}, x::Ref{ZZPolyRingElem}, y::Int)::Nothing
  return z
end

function sub!(z::ZZPolyRingElemOrPtr, x::ZZRingElemOrPtr, y::ZZPolyRingElemOrPtr)
  @ccall libflint.fmpz_poly_fmpz_sub(z::Ref{ZZPolyRingElem}, x::Ref{ZZRingElem}, y::Ref{ZZPolyRingElem})::Nothing
  return z
end

function sub!(z::ZZPolyRingElemOrPtr, x::Int, y::ZZPolyRingElemOrPtr)
  @ccall libflint.fmpz_poly_si_sub(z::Ref{ZZPolyRingElem}, x::Int, y::Ref{ZZPolyRingElem})::Nothing
  return z
end

sub!(z::ZZPolyRingElemOrPtr, x::ZZPolyRingElemOrPtr, y::Integer) = sub!(z, x, flintify(y))

sub!(z::ZZPolyRingElemOrPtr, x::Integer, y::ZZPolyRingElemOrPtr) = sub!(z, flintify(x), y)

#

function mul!(z::ZZPolyRingElemOrPtr, x::ZZPolyRingElemOrPtr, y::ZZPolyRingElemOrPtr)
  @ccall libflint.fmpz_poly_mul(z::Ref{ZZPolyRingElem}, x::Ref{ZZPolyRingElem}, y::Ref{ZZPolyRingElem})::Nothing
  return z
end

function mul!(z::ZZPolyRingElemOrPtr, x::ZZPolyRingElemOrPtr, y::ZZRingElemOrPtr)
  @ccall libflint.fmpz_poly_scalar_mul_fmpz(z::Ref{ZZPolyRingElem}, x::Ref{ZZPolyRingElem}, y::Ref{ZZRingElem})::Nothing
  return z
end

function mul!(z::ZZPolyRingElemOrPtr, x::ZZPolyRingElemOrPtr, y::Int)
  @ccall libflint.fmpz_poly_scalar_mul_si(z::Ref{ZZPolyRingElem}, x::Ref{ZZPolyRingElem}, y::Int)::Nothing
  return z
end

function mul!(z::ZZPolyRingElemOrPtr, x::ZZPolyRingElemOrPtr, y::UInt)
  @ccall libflint.fmpz_poly_scalar_mul_ui(z::Ref{ZZPolyRingElem}, x::Ref{ZZPolyRingElem}, y::UInt)::Nothing
  return z
end

mul!(z::ZZPolyRingElemOrPtr, x::ZZPolyRingElemOrPtr, y::Integer) = mul!(z, x, flintify(y))

mul!(z::ZZPolyRingElemOrPtr, x::IntegerUnionOrPtr, y::ZZPolyRingElemOrPtr) = mul!(z, y, x)

###############################################################################
#
#   Promotions
#
###############################################################################

promote_rule(::Type{ZZPolyRingElem}, ::Type{T}) where {T <: Integer} = ZZPolyRingElem

promote_rule(::Type{ZZPolyRingElem}, ::Type{ZZRingElem}) = ZZPolyRingElem

###############################################################################
#
#   Conversion
#
###############################################################################

function fmpz_poly_to_nmod_poly_raw!(r::zzModPolyRingElem, a::ZZPolyRingElem)
  ccall((:fmpz_poly_get_nmod_poly, libflint), Nothing,
        (Ref{zzModPolyRingElem}, Ref{ZZPolyRingElem}), r, a)
  return r
end

function (Rx::zzModPolyRing)(f::ZZPolyRingElem)
  r = Rx()
  fmpz_poly_to_nmod_poly_raw!(r, f)
  return r
end

function fmpz_poly_to_gfp_poly_raw!(r::fpPolyRingElem, a::ZZPolyRingElem)
  ccall((:fmpz_poly_get_nmod_poly, libflint), Nothing,
        (Ref{fpPolyRingElem}, Ref{ZZPolyRingElem}), r, a)
  return r
end

function (Rx::fpPolyRing)(f::ZZPolyRingElem)
  r = Rx()
  fmpz_poly_to_gfp_poly_raw!(r, f)
  return r
end

###############################################################################
#
#   Parent object call overloads
#
###############################################################################

function (a::ZZPolyRing)()
  z = ZZPolyRingElem()
  z.parent = a
  return z
end

function (a::ZZPolyRing)(b::IntegerUnion)
  z = ZZPolyRingElem(flintify(b))
  z.parent = a
  return z
end

function (a::ZZPolyRing)(b::Vector{ZZRingElem})
  z = ZZPolyRingElem(b)
  z.parent = a
  return z
end

(a::ZZPolyRing)(b::Vector{T}) where {T <: Integer} = a(map(ZZRingElem, b))

(a::ZZPolyRing)(b::ZZPolyRingElem) = b

###############################################################################
#
#  Sturm sequence
#
###############################################################################

function _divide_by_content(f::ZZPolyRingElem)
  p = primpart(f)
  if sign(leading_coefficient(f)) == sign(leading_coefficient(p))
    return p
  else
    return -p
  end
end

function sturm_sequence(f::ZZPolyRingElem)
  g = f
  h = _divide_by_content(derivative(g))
  seq = ZZPolyRingElem[g, h]
  while true
    r = _divide_by_content(pseudorem(g, h))
    # r has the same sign as pseudorem(g, h)
    # To get a pseudo remainder sequence for the Sturm sequence,
    # we need r to be the pseudo remainder of |lc(b)|^(a - b + 1),
    # so we need some adjustment. See
    # https://en.wikipedia.org/wiki/Polynomial_greatest_common_divisor#Sturm_sequence_with_pseudo-remainders
    if leading_coefficient(h) < 0 && isodd(degree(g) - degree(h) + 1)
      r = -r
    end
    if r != 0
      push!(seq, -r)
      g, h = h, -r
    else
      break
    end
  end
  return seq
end

###############################################################################
#
#   Mulhigh
#
###############################################################################

function mulhigh_n(a::ZZPolyRingElem, b::ZZPolyRingElem, n::Int)
  c = parent(a)()
  #careful: as part of the interface, the coeffs 0 - (n-1) are random garbage
  ccall((:fmpz_poly_mulhigh_n, libflint), Nothing,
        (Ref{ZZPolyRingElem}, Ref{ZZPolyRingElem}, Ref{ZZPolyRingElem}, Cint),
        c, a, b, n)
  return c
end
