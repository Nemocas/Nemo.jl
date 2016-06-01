###############################################################################
#
#   fmpz_poly.jl : Flint polynomials over fmpz
#
###############################################################################

export FmpzPolyRing, fmpz_poly, cyclotomic, theta_qexp, eta_qexp, cos_minpoly,
       swinnerton_dyer, signature

###############################################################################
#
#   Data type and parent methods
#
###############################################################################

parent_type(::Type{fmpz_poly}) = FmpzPolyRing

elem_type(::FmpzPolyRing) = fmpz_poly

base_ring(a::FmpzPolyRing) = a.base_ring

parent(a::fmpz_poly) = a.parent

var(a::FmpzPolyRing) = a.S

###############################################################################
#
#   Basic manipulation
#
###############################################################################   
   
length(x::fmpz_poly) = ccall((:fmpz_poly_length, :libflint), Int, 
                             (Ptr{fmpz_poly},), &x)

function coeff(x::fmpz_poly, n::Int)
   n < 0 && throw(DomainError())
   z = fmpz()
   ccall((:fmpz_poly_get_coeff_fmpz, :libflint), Void, 
               (Ptr{fmpz}, Ptr{fmpz_poly}, Int), &z, &x, n)
   return z
end

zero(a::FmpzPolyRing) = a(0)

one(a::FmpzPolyRing) = a(1)

gen(a::FmpzPolyRing) = a([zero(base_ring(a)), one(base_ring(a))])

isgen(x::fmpz_poly) = ccall((:fmpz_poly_is_x, :libflint), Bool, 
                            (Ptr{fmpz_poly},), &x)

function deepcopy(a::fmpz_poly)
   z = fmpz_poly(a)
   z.parent = parent(a)
   return z
end

###############################################################################
#
#   Canonicalisation
#
###############################################################################

canonical_unit(a::fmpz_poly) = canonical_unit(lead(a))

###############################################################################
#
#   AbstractString{} I/O
#
###############################################################################

function show(io::IO, x::fmpz_poly)
   if length(x) == 0
      print(io, "0")
   else
      cstr = ccall((:fmpz_poly_get_str_pretty, :libflint), Ptr{UInt8}, 
          (Ptr{fmpz_poly}, Ptr{UInt8}), &x, bytestring(string(var(parent(x)))))

      print(io, bytestring(cstr))

      ccall((:flint_free, :libflint), Void, (Ptr{UInt8},), cstr)
   end
end

function show(io::IO, p::FmpzPolyRing)
   print(io, "Univariate Polynomial Ring in ")
   print(io, string(var(p)))
   print(io, " over ")
   show(io, p.base_ring)
end

show_minus_one(::Type{fmpz_poly}) = show_minus_one(fmpz)

###############################################################################
#
#   Unary operations
#
###############################################################################

function -(x::fmpz_poly)
   z = parent(x)()
   ccall((:fmpz_poly_neg, :libflint), Void, 
         (Ptr{fmpz_poly}, Ptr{fmpz_poly}), &z, &x)
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
   ccall((:fmpz_poly_add, :libflint), Void, 
                (Ptr{fmpz_poly}, Ptr{fmpz_poly},  Ptr{fmpz_poly}), 
               &z, &x, &y)
   return z
end

function -(x::fmpz_poly, y::fmpz_poly)
   check_parent(x, y)
   z = parent(x)()
   ccall((:fmpz_poly_sub, :libflint), Void, 
                (Ptr{fmpz_poly}, Ptr{fmpz_poly},  Ptr{fmpz_poly}), 
               &z, &x, &y)
   return z
end

function *(x::fmpz_poly, y::fmpz_poly)
   check_parent(x, y)
   z = parent(x)()
   ccall((:fmpz_poly_mul, :libflint), Void, 
                (Ptr{fmpz_poly}, Ptr{fmpz_poly},  Ptr{fmpz_poly}), 
               &z, &x, &y)
   return z
end

###############################################################################
#
#   Ad hoc binary operators
#
###############################################################################

function *(x::Int, y::fmpz_poly)
   z = parent(y)()
   ccall((:fmpz_poly_scalar_mul_si, :libflint), Void, 
                (Ptr{fmpz_poly}, Ptr{fmpz_poly}, Int), &z, &y, x)
   return z
end

function *(x::fmpz, y::fmpz_poly)
   z = parent(y)()
   ccall((:fmpz_poly_scalar_mul_fmpz, :libflint), Void, 
                (Ptr{fmpz_poly}, Ptr{fmpz_poly}, Ptr{fmpz}), &z, &y, &x)
   return z
end

function +(x::fmpz_poly, y::Int)
   z = parent(x)()
   ccall((:fmpz_poly_add_si, :libflint), Void, 
                (Ptr{fmpz_poly}, Ptr{fmpz_poly}, Int), &z, &x, y)
   return z
end

function +(x::fmpz_poly, y::fmpz)
   z = parent(x)()
   ccall((:fmpz_poly_add_fmpz, :libflint), Void, 
                (Ptr{fmpz_poly}, Ptr{fmpz_poly}, Ptr{fmpz}), &z, &x, &y)
   return z
end

function -(x::fmpz_poly, y::Int)
   z = parent(x)()
   ccall((:fmpz_poly_sub_si, :libflint), Void, 
                (Ptr{fmpz_poly}, Ptr{fmpz_poly}, Int), &z, &x, y)
   return z
end

function -(x::fmpz_poly, y::fmpz)
   z = parent(x)()
   ccall((:fmpz_poly_sub_fmpz, :libflint), Void, 
                (Ptr{fmpz_poly}, Ptr{fmpz_poly}, Ptr{fmpz}), &z, &x, &y)
   return z
end

function -(x::Int, y::fmpz_poly)
   z = parent(y)()
   ccall((:fmpz_poly_si_sub, :libflint), Void, 
                (Ptr{fmpz_poly}, Int, Ptr{fmpz_poly}), &z, x, &y)
   return z
end

function -(x::fmpz, y::fmpz_poly)
   z = parent(y)()
   ccall((:fmpz_poly_fmpz_sub, :libflint), Void, 
                (Ptr{fmpz_poly}, Ptr{fmpz}, Ptr{fmpz_poly}), &z, &x, &y)
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
   y < 0 && throw(DomainError())
   z = parent(x)()
   ccall((:fmpz_poly_pow, :libflint), Void, 
                (Ptr{fmpz_poly}, Ptr{fmpz_poly}, Int), 
               &z, &x, y)
   return z
end

###############################################################################
#
#   Comparisons
#
###############################################################################

function ==(x::fmpz_poly, y::fmpz_poly)
   check_parent(x, y)
   return ccall((:fmpz_poly_equal, :libflint), Bool, 
                (Ptr{fmpz_poly}, Ptr{fmpz_poly}), &x, &y)
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
      ccall((:fmpz_poly_get_coeff_fmpz, :libflint), Void, 
                       (Ptr{fmpz}, Ptr{fmpz_poly}, Int), &z, &x, 0)
      return ccall((:fmpz_equal, :libflint), Bool, 
               (Ptr{fmpz}, Ptr{fmpz}, Int), &z, &y, 0)
   else
      return y == 0
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
   n < 0 && throw(DomainError())
   
   if length(a) <= n
      return a
   end

   z = parent(a)()
   ccall((:fmpz_poly_set_trunc, :libflint), Void,
                (Ptr{fmpz_poly}, Ptr{fmpz_poly}, Int), &z, &a, n)
   return z
end

function mullow(x::fmpz_poly, y::fmpz_poly, n::Int)
   check_parent(x, y)
   n < 0 && throw(DomainError())
   
   z = parent(x)()
   ccall((:fmpz_poly_mullow, :libflint), Void,
         (Ptr{fmpz_poly}, Ptr{fmpz_poly}, Ptr{fmpz_poly}, Int), &z, &x, &y, n)
   return z
end

###############################################################################
#
#   Reversal
#
###############################################################################

function reverse(x::fmpz_poly, len::Int)
   len < 0 && throw(DomainError())
   z = parent(x)()
   ccall((:fmpz_poly_reverse, :libflint), Void,
                (Ptr{fmpz_poly}, Ptr{fmpz_poly}, Int), &z, &x, len)
   return z
end

###############################################################################
#
#   Shifting
#
###############################################################################

function shift_left(x::fmpz_poly, len::Int)
   len < 0 && throw(DomainError())
   z = parent(x)()
   ccall((:fmpz_poly_shift_left, :libflint), Void,
      (Ptr{fmpz_poly}, Ptr{fmpz_poly}, Int), &z, &x, len)
   return z
end

function shift_right(x::fmpz_poly, len::Int)
   len < 0 && throw(DomainError())
   z = parent(x)()
   ccall((:fmpz_poly_shift_right, :libflint), Void,
       (Ptr{fmpz_poly}, Ptr{fmpz_poly}, Int), &z, &x, len)
   return z
end

###############################################################################
#
#   Exact division
#
###############################################################################

function divexact(x::fmpz_poly, y::fmpz_poly)
   check_parent(x, y)
   y == 0 && throw(DivideError())
   z = parent(x)()
   ccall((:fmpz_poly_div, :libflint), Void, 
            (Ptr{fmpz_poly}, Ptr{fmpz_poly}, Ptr{fmpz_poly}), &z, &x, &y)
   return z
end

###############################################################################
#
#   Ad hoc exact division
#
###############################################################################

function divexact(x::fmpz_poly, y::fmpz)
   y == 0 && throw(DivideError())
   z = parent(x)()
   ccall((:fmpz_poly_scalar_divexact_fmpz, :libflint), Void, 
          (Ptr{fmpz_poly}, Ptr{fmpz_poly}, Ptr{fmpz}), &z, &x, &y)
   return z
end

function divexact(x::fmpz_poly, y::Int)
   y == 0 && throw(DivideError())
   z = parent(x)()
   ccall((:fmpz_poly_scalar_divexact_si, :libflint), Void, 
                        (Ptr{fmpz_poly}, Ptr{fmpz_poly}, Int), &z, &x, y)
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
   y == 0 && throw(DivideError())
   diff = length(x) - length(y)
   r = parent(x)()
   d = Array(Int, 1)
   ccall((:fmpz_poly_pseudo_rem, :libflint), Void, 
     (Ptr{fmpz_poly}, Ptr{Int}, Ptr{fmpz_poly}, Ptr{fmpz_poly}), &r, d, &x, &y)
   if (diff > d[1])
      return lead(y)^(diff - d[1])*r
   else
      return r
   end
end

function pseudodivrem(x::fmpz_poly, y::fmpz_poly)
   check_parent(x, y)
   y == 0 && throw(DivideError())
   diff = length(x) - length(y)
   q = parent(x)()
   r = parent(x)()
   d = Array(Int, 1)
   ccall((:fmpz_poly_pseudo_divrem_divconquer, :libflint), Void, 
    (Ptr{fmpz_poly}, Ptr{fmpz_poly}, Ptr{Int}, Ptr{fmpz_poly}, Ptr{fmpz_poly}),
               &q, &r, d, &x, &y)
   if (diff > d[1])
      m = lead(y)^(diff - d[1])
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
   ccall((:fmpz_poly_gcd, :libflint), Void, 
                (Ptr{fmpz_poly}, Ptr{fmpz_poly}, Ptr{fmpz_poly}), &z, &x, &y)
   return z
end

function content(x::fmpz_poly)
   z = fmpz()
   ccall((:fmpz_poly_content, :libflint), Void,
         (Ptr{fmpz}, Ptr{fmpz_poly}), &z, &x)
   return z
end

function primpart(x::fmpz_poly)
   z = parent(x)()
   ccall((:fmpz_poly_primitive_part, :libflint), Void, 
         (Ptr{fmpz_poly}, Ptr{fmpz_poly}), &z, &x)
   return z
end

###############################################################################
#
#   Evaluation
#
###############################################################################

function evaluate(x::fmpz_poly, y::fmpz)
   z = fmpz()
   ccall((:fmpz_poly_evaluate_fmpz, :libflint), Void, 
        (Ptr{fmpz}, Ptr{fmpz_poly}, Ptr{fmpz}), &z, &x, &y)
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
   ccall((:fmpz_poly_compose, :libflint), Void, 
                (Ptr{fmpz_poly}, Ptr{fmpz_poly}, Ptr{fmpz_poly}), &z, &x, &y)
   return z
end

###############################################################################
#
#   Derivative
#
###############################################################################

function derivative(x::fmpz_poly)
   z = parent(x)()
   ccall((:fmpz_poly_derivative, :libflint), Void, 
                (Ptr{fmpz_poly}, Ptr{fmpz_poly}), &z, &x)
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
   ccall((:fmpz_poly_resultant, :libflint), Void, 
                (Ptr{fmpz}, Ptr{fmpz_poly}, Ptr{fmpz_poly}), &z, &x, &y)
   return z
end

###############################################################################
#
#   Discriminant
#
###############################################################################

function discriminant(x::fmpz_poly)
   z = fmpz()
   ccall((:fmpz_poly_discriminant, :libflint), Void, 
                (Ptr{fmpz}, Ptr{fmpz_poly}), &z, &x)
   return z
end

###############################################################################
#
#   GCDX
#
###############################################################################

function gcdx(a::fmpz_poly, b::fmpz_poly)
   check_parent(a, b)
   lena = length(a)
   lenb = length(b)
   (lena <= 1 || lenb <= 1) && error("Constant polynomial in gcdx")  
   z = fmpz()
   u = parent(a)()
   v = parent(a)()
   c1 = content(a)
   c2 = content(b)
   x = divexact(a, c1)
   y = divexact(b, c2)
   ccall((:fmpz_poly_xgcd_modular, :libflint), Void, 
   (Ptr{fmpz}, Ptr{fmpz_poly}, Ptr{fmpz_poly}, Ptr{fmpz_poly}, Ptr{fmpz_poly}), 
            &z, &u, &v, &x, &y)
   r = z*c1^(lenb - 1)*c2^(lena - 1)
   u *= c1^(lenb - 2)*c2^(lena - 1)
   v *= c1^(lenb - 1)*c2^(lena - 2)   
   return (r, u, v)
end

###############################################################################
#
#   Signature
#
###############################################################################

doc"""
    signature(f::fmpz_poly)
> Return the signature of the polynomial $f$, i.e. a tuple $(r, s)$ such that
> $r$ is the number of real roots of $f$ and $s$ is half the number of complex
> roots.
"""
function signature(f::fmpz_poly)
   r = Array(Int, 1)
   s = Array(Int, 1)
   ccall((:fmpz_poly_signature, :libflint), Void,
         (Ptr{Int}, Ptr{Int}, Ptr{fmpz_poly}), r, s, &f)
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

  ax = Array(Int, length(x))
  ay = Array(Int, length(y))

  t = fmpz()

  for i in 1:length(x)
    ax[i] = x[i].d
    ay[i] = y[i].d
  end

  ccall((:fmpz_poly_interpolate_fmpz_vec, :libflint), Void,
          (Ptr{fmpz_poly}, Ptr{Int}, Ptr{Int}, Int),
          &z, ax, ay, length(x))
  return z
end

###############################################################################
#
#   Special polynomials
#
###############################################################################

function chebyshev_t(n::Int, x::fmpz_poly)
   z = parent(x)()
   ccall((:fmpz_poly_chebyshev_t, :libflint), Void, 
                                                  (Ptr{fmpz_poly}, Int), &z, n)
   return isgen(x) ? z : compose(z, x)
end
   
function chebyshev_u(n::Int, x::fmpz_poly)
   z = parent(x)()
   ccall((:fmpz_poly_chebyshev_u, :libflint), Void, 
                                                  (Ptr{fmpz_poly}, Int), &z, n)
   return isgen(x) ? z : compose(z, x)
end

doc"""
    cyclotomic(n::Int, x::fmpz_poly)
> Return the $n$th cyclotomic polynomial, defined as
> $$\Phi_n(x) = \prod_{\omega} (x-\omega),$$ where $\omega$ runs over all the 
> $n$th primitive roots of unity.
"""
function cyclotomic(n::Int, x::fmpz_poly)
   z = parent(x)()
   ccall((:fmpz_poly_cyclotomic, :libflint), Void, 
                                                  (Ptr{fmpz_poly}, Int), &z, n)
   return isgen(x) ? z : compose(z, x)
end
   
doc"""
    swinnerton_dyer(n::Int, x::fmpz_poly)
> Return the Swinnerton-Dyer polynomial $S_n$, defined as the integer 
> polynomial
> $$S_n = \prod (x \pm \sqrt{2} \pm \sqrt{3} \pm \sqrt{5} \pm \ldots \pm \sqrt{p_n})$$ 
> where $p_n$ denotes the $n$-th prime number and all combinations of signs are
> taken. This polynomial has degree $2^n$ and is irreducible over the integers
> (it is the minimal polynomial of $\sqrt{2} + \ldots + \sqrt{p_n}$).
"""
function swinnerton_dyer(n::Int, x::fmpz_poly)
   z = parent(x)()
   ccall((:fmpz_poly_swinnerton_dyer, :libflint), Void, 
                                                  (Ptr{fmpz_poly}, Int), &z, n)
   return isgen(x) ? z : compose(z, x)
end
   
doc"""
    cos_minpoly(n::Int, x::fmpz_poly)
> Return the minimal polynomial of $2 \cos(2 \pi / n)$. For suitable choice of 
> $n$, this gives the minimal polynomial of $2 \cos(a \pi)$ or $2 \sin(a \pi)$ for any
> rational $a$.
"""
function cos_minpoly(n::Int, x::fmpz_poly)
   z = parent(x)()
   ccall((:fmpz_poly_cos_minpoly, :libflint), Void, 
                                                  (Ptr{fmpz_poly}, Int), &z, n)
   return isgen(x) ? z : compose(z, x)
end
   
doc"""
    theta_qexp(e::Int, n::Int, x::fmpz_poly)
> Return the $q$-expansion to length $n$ of the Jacobi theta function raised to
> the power $r$, i.e. $\vartheta(q)^r$ where 
> $\vartheta(q) = 1 + \sum_{k=1}^{\infty} q^{k^2}$.
"""
function theta_qexp(e::Int, n::Int, x::fmpz_poly)
   z = parent(x)()
   ccall((:fmpz_poly_theta_qexp, :libflint), Void, 
                                          (Ptr{fmpz_poly}, Int, Int), &z, e, n)
   return isgen(x) ? z : compose(z, x)
end

doc"""
    eta_qexp(e::Int, n::Int, x::fmpz_poly)
> Return the $q$-expansion to length $n$ of the Dedekind eta function (without 
> the leading factor $q^{1/24}$) raised to the power $r$, i.e.
> $(q^{-1/24} \eta(q))^r = \prod_{k=1}^{\infty} (1 - q^k)^r$.
> In particular, $r = -1$ gives the generating function of the partition
> function $p(k)$, and $r = 24$ gives, after multiplication by $q$, the modular
> discriminant $\Delta(q)$ which generates the Ramanujan tau function
> $\tau(k)$.
"""
function eta_qexp(e::Int, n::Int, x::fmpz_poly)
   z = parent(x)()
   ccall((:fmpz_poly_eta_qexp, :libflint), Void, 
                                          (Ptr{fmpz_poly}, Int, Int), &z, e, n)
   return isgen(x) ? z : compose(z, x)
end

###############################################################################
#
#   Speedups for polynomials over fmpz_polys
#
###############################################################################

function *(a::GenPoly{fmpz_poly}, b::GenPoly{fmpz_poly})
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

function fit!(z::fmpz_poly, n::Int)
   ccall((:fmpz_poly_fit_length, :libflint), Void, 
                    (Ptr{fmpz_poly}, Int), &z, n)
end

function setcoeff!(z::fmpz_poly, n::Int, x::fmpz)
   ccall((:fmpz_poly_set_coeff_fmpz, :libflint), Void, 
                    (Ptr{fmpz_poly}, Int, Ptr{fmpz}), &z, n, &x)
end

function mul!(z::fmpz_poly, x::fmpz_poly, y::fmpz_poly)
   ccall((:fmpz_poly_mul, :libflint), Void, 
                (Ptr{fmpz_poly}, Ptr{fmpz_poly}, Ptr{fmpz_poly}), &z, &x, &y)
end

function addeq!(z::fmpz_poly, x::fmpz_poly)
   ccall((:fmpz_poly_add, :libflint), Void, 
                (Ptr{fmpz_poly}, Ptr{fmpz_poly}, Ptr{fmpz_poly}), &z, &z, &x)
end

###############################################################################
#
#   Promotions
#
###############################################################################

Base.promote_rule{T <: Integer}(::Type{fmpz_poly}, ::Type{T}) = fmpz_poly

Base.promote_rule(::Type{fmpz_poly}, ::Type{fmpz}) = fmpz_poly

###############################################################################
#
#   Parent object call overloads
#
###############################################################################

function Base.call(a::FmpzPolyRing)
   z = fmpz_poly()
   z.parent = a
   return z
end

function Base.call(a::FmpzPolyRing, b::Int)
   z = fmpz_poly(b)
   z.parent = a
   return z
end

function Base.call(a::FmpzPolyRing, b::Integer)
   z = fmpz_poly(fmpz(b))
   z.parent = a
   return z
end

function Base.call(a::FmpzPolyRing, b::fmpz)
   z = fmpz_poly(b)
   z.parent = a
   return z
end

function Base.call(a::FmpzPolyRing, b::Array{fmpz, 1})
   z = fmpz_poly(b)
   z.parent = a
   return z
end

Base.call(a::FmpzPolyRing, b::fmpz_poly) = b

###############################################################################
#
#   PolynomialRing constructor
#
###############################################################################

function PolynomialRing(R::FlintIntegerRing, s::AbstractString{})
   S = Symbol(s)

   parent_obj = FmpzPolyRing(S)
   
   return parent_obj, parent_obj([fmpz(0), fmpz(1)])
end
