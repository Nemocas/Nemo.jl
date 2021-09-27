################################################################################
#
#  fq_default_poly.jl: Flint fq_default_poly
#                      (Polynomials over FqDefaultFiniteField)
#
################################################################################

export fq_default_poly, FqDefaultPolyRing

################################################################################
#
#  Type and parent object methods
#
################################################################################

parent_type(::Type{fq_default_poly}) = FqDefaultPolyRing

elem_type(::Type{FqDefaultPolyRing}) = fq_default_poly

base_ring(a::FqDefaultPolyRing) = a.base_ring

parent(a::fq_default_poly) = a.parent

var(a::FqDefaultPolyRing) = a.S

function check_parent(a::fq_default_poly, b::fq_default_poly)
   a.parent != b.parent &&
         error("Operations on distinct polynomial rings not supported")
end

################################################################################
#
#   Basic manipulation
#
################################################################################

function length(x::fq_default_poly)
   F = (x.parent).base_ring
   ccall((:fq_default_poly_length, libflint), Int,
                        (Ref{fq_default_poly}, Ref{FqDefaultFiniteField}), x, F)
end

function coeff(x::fq_default_poly, n::Int)
   n < 0 && throw(DomainError(n, "Index must be non-negative"))
   F = (x.parent).base_ring
   temp = F(1)
   ccall((:fq_default_poly_get_coeff, libflint), Nothing,
         (Ref{fq_default}, Ref{fq_default_poly}, Int, Ref{FqDefaultFiniteField}),
         temp, x, n, F)
   return temp
end

function set_length!(x::fq_default_poly, n::Int)
   ctx = base_ring(x)
   ccall((:_fq_default_poly_set_length, libflint), Nothing,
         (Ref{fq_default_poly}, Int, Ref{FqDefaultFiniteField}), x, n, ctx)
   return x
end

zero(a::FqDefaultPolyRing) = a(zero(base_ring(a)))

one(a::FqDefaultPolyRing) = a(one(base_ring(a)))

gen(a::FqDefaultPolyRing) = a([zero(base_ring(a)), one(base_ring(a))])

isgen(x::fq_default_poly) = ccall((:fq_default_poly_is_gen, libflint), Bool,
                              (Ref{fq_default_poly}, Ref{FqDefaultFiniteField}),
                              x, base_ring(x.parent))

iszero(x::fq_default_poly) = ccall((:fq_default_poly_is_zero, libflint), Bool,
                              (Ref{fq_default_poly}, Ref{FqDefaultFiniteField}),
                              x, base_ring(x.parent))

isone(x::fq_default_poly) = ccall((:fq_default_poly_is_one, libflint), Bool,
                              (Ref{fq_default_poly}, Ref{FqDefaultFiniteField}),
                              x, base_ring(x.parent))

degree(f::fq_default_poly) = length(f) - 1

function deepcopy_internal(a::fq_default_poly, dict::IdDict)
   z = fq_default_poly(a, base_ring(a))
   z.parent = a.parent
   return z
end

characteristic(R::FqDefaultPolyRing) = characteristic(base_ring(R))

################################################################################
#
#   Canonicalisation
#
################################################################################

canonical_unit(a::fq_default_poly) = canonical_unit(leading_coefficient(a))

################################################################################
#
#  String I/O
#
################################################################################

function show(io::IO, R::FqDefaultPolyRing)
   print(io, "Univariate Polynomial Ring in ")
   print(io, string(var(R)))
   print(io, " over ")
   show(io, base_ring(R))
end

################################################################################
#
#  Unary operations
#
################################################################################

function -(x::fq_default_poly)
   z = parent(x)()
   ccall((:fq_default_poly_neg, libflint), Nothing,
         (Ref{fq_default_poly}, Ref{fq_default_poly}, Ref{FqDefaultFiniteField}),
         z, x, base_ring(parent(x)))
   return z
end

################################################################################
#
#  Binary operations
#
################################################################################

function +(x::fq_default_poly, y::fq_default_poly)
   check_parent(x,y)
   z = parent(x)()
   ccall((:fq_default_poly_add, libflint), Nothing,
         (Ref{fq_default_poly}, Ref{fq_default_poly},
         Ref{fq_default_poly}, Ref{FqDefaultFiniteField}),
         z, x, y, base_ring(parent(x)))
   return z
end

function -(x::fq_default_poly, y::fq_default_poly)
   check_parent(x,y)
   z = parent(x)()
   ccall((:fq_default_poly_sub, libflint), Nothing,
         (Ref{fq_default_poly}, Ref{fq_default_poly},
         Ref{fq_default_poly}, Ref{FqDefaultFiniteField}),
         z, x, y, base_ring(parent(x)))
   return z
end

function *(x::fq_default_poly, y::fq_default_poly)
   check_parent(x,y)
   z = parent(x)()
   ccall((:fq_default_poly_mul, libflint), Nothing,
         (Ref{fq_default_poly}, Ref{fq_default_poly},
         Ref{fq_default_poly}, Ref{FqDefaultFiniteField}),
         z, x, y, base_ring(parent(x)))
   return z
end

################################################################################
#
#   Ad hoc binary operators
#
################################################################################

function *(x::fq_default, y::fq_default_poly)
   parent(x) != base_ring(parent(y)) &&
         error("Coefficient rings must be equal")
   z = parent(y)()
   ccall((:fq_default_poly_scalar_mul_fq_default, libflint), Nothing,
         (Ref{fq_default_poly}, Ref{fq_default_poly},
         Ref{fq_default}, Ref{FqDefaultFiniteField}),
         z, y, x, parent(x))
  return z
end

*(x::fq_default_poly, y::fq_default) = y*x

*(x::fmpz, y::fq_default_poly) = base_ring(parent(y))(x) * y

*(x::fq_default_poly, y::fmpz) = y*x

*(x::Integer, y::fq_default_poly) = fmpz(x)*y

*(x::fq_default_poly, y::Integer) = y*x

+(x::fq_default, y::fq_default_poly) = parent(y)(x) + y

+(x::fq_default_poly, y::fq_default) = y + x

+(x::fmpz, y::fq_default_poly) = base_ring(parent(y))(x) + y

+(x::fq_default_poly, y::fmpz) = y + x

+(x::fq_default_poly, y::Integer) = x + fmpz(y)

+(x::Integer, y::fq_default_poly) = y + x

-(x::fq_default, y::fq_default_poly) = parent(y)(x) - y

-(x::fq_default_poly, y::fq_default) = x - parent(x)(y)

-(x::fmpz, y::fq_default_poly) = base_ring(parent(y))(x) - y

-(x::fq_default_poly, y::fmpz) = x - base_ring(parent(x))(y)

-(x::fq_default_poly, y::Integer) = x - fmpz(y)

-(x::Integer, y::fq_default_poly) = fmpz(x) - y

################################################################################
#
#   Powering
#
################################################################################

function ^(x::fq_default_poly, y::Int)
   y < 0 && throw(DomainError(y, "Exponent must be non-negative"))
   z = parent(x)()
   ccall((:fq_default_poly_pow, libflint), Nothing,
         (Ref{fq_default_poly}, Ref{fq_default_poly}, Int, Ref{FqDefaultFiniteField}),
         z, x, y, base_ring(parent(x)))
   return z
end

################################################################################
#
#   Comparisons
#
################################################################################

function ==(x::fq_default_poly, y::fq_default_poly)
   check_parent(x,y)
   r = ccall((:fq_default_poly_equal, libflint), Cint,
             (Ref{fq_default_poly}, Ref{fq_default_poly}, Ref{FqDefaultFiniteField}),
             x, y, base_ring(parent(x)))
   return Bool(r)
end

################################################################################
#
#   Ad hoc comparisons
#
################################################################################

function ==(x::fq_default_poly, y::fq_default)
   base_ring(parent(x)) != parent(y) && return false
   if length(x) > 1
      return false
   elseif length(x) == 1
      r = ccall((:fq_default_poly_equal_fq_default, libflint), Cint,
                (Ref{fq_default_poly}, Ref{fq_default}, Ref{FqDefaultFiniteField}),
                x, y, base_ring(parent(x)))
      return Bool(r)
   else
      return iszero(y)
  end
end

==(x::fq_default, y::fq_default_poly) = y == x

==(x::fq_default_poly, y::fmpz) = x == base_ring(parent(x))(y)

==(x::fmpz, y::fq_default_poly) = y == x

==(x::fq_default_poly, y::Integer) = x == fmpz(y)

==(x::Integer, y::fq_default_poly) = y == x

################################################################################
#
#   Truncation
#
################################################################################

function truncate(x::fq_default_poly, n::Int)
   n < 0 && throw(DomainError(n, "Index must be non-negative"))
   if length(x) <= n
      return x
   end
   z = parent(x)()
   ccall((:fq_default_poly_set_trunc, libflint), Nothing,
         (Ref{fq_default_poly}, Ref{fq_default_poly}, Int, Ref{FqDefaultFiniteField}),
         z, x, n, base_ring(parent(x)))
   return z
end

function mullow(x::fq_default_poly, y::fq_default_poly, n::Int)
   check_parent(x,y)
   n < 0 && throw(DomainError(n, "Index must be non-negative"))
   z = parent(x)()
   ccall((:fq_default_poly_mullow, libflint), Nothing,
         (Ref{fq_default_poly}, Ref{fq_default_poly}, Ref{fq_default_poly},
         Int, Ref{FqDefaultFiniteField}),
         z, x, y, n, base_ring(parent(x)))
   return z
end

################################################################################
#
#   Reversal
#
################################################################################

function reverse(x::fq_default_poly, len::Int)
   len < 0 && throw(DomainError(len, "Index must be non-negative"))
   z = parent(x)()
   ccall((:fq_default_poly_reverse, libflint), Nothing,
         (Ref{fq_default_poly}, Ref{fq_default_poly}, Int, Ref{FqDefaultFiniteField}),
         z, x, len, base_ring(parent(x)))
   return z
end

################################################################################
#
#   Shifting
#
################################################################################

function shift_left(x::fq_default_poly, len::Int)
   len < 0 && throw(DomainError(len, "Shift must be non-negative"))
   z = parent(x)()
   ccall((:fq_default_poly_shift_left, libflint), Nothing,
         (Ref{fq_default_poly}, Ref{fq_default_poly}, Int, Ref{FqDefaultFiniteField}),
         z, x, len, base_ring(parent(x)))
   return z
end

function shift_right(x::fq_default_poly, len::Int)
   len < 0 && throw(DomainError(len, "Shift must be non-negative"))
   z = parent(x)()
   ccall((:fq_default_poly_shift_right, libflint), Nothing,
         (Ref{fq_default_poly}, Ref{fq_default_poly}, Int, Ref{FqDefaultFiniteField}),
         z, x, len, base_ring(parent(x)))
   return z
end

################################################################################
#
#   Euclidean division
#
################################################################################

function Base.div(x::fq_default_poly, y::fq_default_poly)
   check_parent(x,y)
   z = parent(x)()
   ccall((:fq_default_poly_div_basecase, libflint), Nothing,
         (Ref{fq_default_poly}, Ref{fq_default_poly}, Ref{fq_default_poly},
         Ref{FqDefaultFiniteField}), z, x, y, base_ring(parent(x)))
  return z
end

function rem(x::fq_default_poly, y::fq_default_poly)
   check_parent(x,y)
   z = parent(x)()
   ccall((:fq_default_poly_rem, libflint), Nothing,
         (Ref{fq_default_poly}, Ref{fq_default_poly}, Ref{fq_default_poly},
         Ref{FqDefaultFiniteField}), z, x, y, base_ring(parent(x)))
  return z
end

mod(x::fq_default_poly, y::fq_default_poly) = rem(x, y)

function Base.divrem(x::fq_default_poly, y::fq_default_poly)
   check_parent(x,y)
   z = parent(x)()
   r = parent(x)()
   ccall((:fq_default_poly_divrem, libflint), Nothing, (Ref{fq_default_poly},
         Ref{fq_default_poly}, Ref{fq_default_poly}, Ref{fq_default_poly},
         Ref{FqDefaultFiniteField}), z, r, x, y, base_ring(parent(x)))
   return z, r
end

################################################################################
#
#   Remove and valuation
#
################################################################################

function remove(z::fq_default_poly, p::fq_default_poly)
   ok, v = _remove_check_simple_cases(z, p)
   ok && return v, zero(parent(z))
   z = deepcopy(z)
   v = ccall((:fq_default_poly_remove, libflint), Int,
            (Ref{fq_default_poly}, Ref{fq_default_poly}, Ref{FqDefaultFiniteField}),
             z, p, base_ring(parent(z)))
   return v, z
end

function divides(z::fq_default_poly, x::fq_default_poly)
   if iszero(z)
      return true, zero(parent(z))
   end
   if iszero(x)
      return false, zero(parent(z))
   end
   check_parent(z, x)
   q = parent(z)()
   v = Bool(ccall((:fq_default_poly_divides, libflint), Cint,
            (Ref{fq_default_poly}, Ref{fq_default_poly},
             Ref{fq_default_poly}, Ref{FqDefaultFiniteField}),
             q, z, x, base_ring(parent(z))))
   return v, q
end

################################################################################
#
#   Modular arithmetic
#
################################################################################

function powermod(x::fq_default_poly, n::Int, y::fq_default_poly)
   check_parent(x,y)
   z = parent(x)()

   if n < 0
      g, x = gcdinv(x, y)
      if !isone(g)
         error("Element not invertible")
      end
      n = -n
   end

   ccall((:fq_default_poly_powmod_ui_binexp, libflint), Nothing,
         (Ref{fq_default_poly}, Ref{fq_default_poly}, Int, Ref{fq_default_poly},
         Ref{FqDefaultFiniteField}), z, x, n, y, base_ring(parent(x)))
  return z
end

function powermod(x::fq_default_poly, n::fmpz, y::fq_default_poly)
   check_parent(x,y)
   z = parent(x)()

   if n < 0
      g, x = gcdinv(x, y)
      if !isone(g)
         error("Element not invertible")
      end
      n = -n
   end

   ccall((:fq_default_poly_powmod_fmpz_binexp, libflint), Nothing,
         (Ref{fq_default_poly}, Ref{fq_default_poly}, Ref{fmpz}, Ref{fq_default_poly},
         Ref{FqDefaultFiniteField}), z, x, n, y, base_ring(parent(x)))
  return z
end

################################################################################
#
#   GCD
#
################################################################################

function gcd(x::fq_default_poly, y::fq_default_poly)
   check_parent(x,y)
   z = parent(x)()
   ccall((:fq_default_poly_gcd, libflint), Nothing,
         (Ref{fq_default_poly}, Ref{fq_default_poly}, Ref{fq_default_poly},
         Ref{FqDefaultFiniteField}), z, x, y, base_ring(parent(x)))
   return z
end

function gcdinv(x::fq_default_poly, y::fq_default_poly)
   check_parent(x,y)
   z = parent(x)()
   s = parent(x)()
   t = parent(x)()
   ccall((:fq_default_poly_xgcd, libflint), Nothing,
         (Ref{fq_default_poly}, Ref{fq_default_poly}, Ref{fq_default_poly}, Ref{fq_default_poly}, Ref{fq_default_poly},
         Ref{FqDefaultFiniteField}), z, s, t, x, y, base_ring(parent(x)))
   return z, s
end

function gcdx(x::fq_default_poly, y::fq_default_poly)
   check_parent(x,y)
   z = parent(x)()
   s = parent(x)()
   t = parent(x)()
   ccall((:fq_default_poly_xgcd, libflint), Nothing,
         (Ref{fq_default_poly}, Ref{fq_default_poly}, Ref{fq_default_poly}, Ref{fq_default_poly}, Ref{fq_default_poly},
         Ref{FqDefaultFiniteField}), z, s, t, x, y, base_ring(parent(x)))
   return z, s, t
end

################################################################################
#
#   Evaluation
#
################################################################################

function evaluate(x::fq_default_poly, y::fq_default)
   base_ring(parent(x)) != parent(y) && error("Incompatible coefficient rings")
   z = parent(y)()
   ccall((:fq_default_poly_evaluate_fq_default, libflint), Nothing,
         (Ref{fq_default}, Ref{fq_default_poly}, Ref{fq_default},
         Ref{FqDefaultFiniteField}), z, x, y, base_ring(parent(x)))
   return z
end

################################################################################
#
#   Composition
#
################################################################################

function compose(x::fq_default_poly, y::fq_default_poly)
   check_parent(x,y)
   z = parent(x)()
   ccall((:fq_default_poly_compose, libflint), Nothing,
         (Ref{fq_default_poly}, Ref{fq_default_poly}, Ref{fq_default_poly},
         Ref{FqDefaultFiniteField}), z, x, y, base_ring(parent(x)))
   return z
end

################################################################################
#
#   Derivative
#
################################################################################

function derivative(x::fq_default_poly)
   z = parent(x)()
   ccall((:fq_default_poly_derivative, libflint), Nothing,
         (Ref{fq_default_poly}, Ref{fq_default_poly}, Ref{FqDefaultFiniteField}),
         z, x, base_ring(parent(x)))
   return z
end

################################################################################
#
#  Inflation and deflation
#
################################################################################

function inflate(x::fq_default_poly, n::Int)
   z = parent(x)()
   ccall((:fq_default_poly_inflate, libflint), Nothing, (Ref{fq_default_poly},
         Ref{fq_default_poly}, Culong, Ref{FqDefaultFiniteField}),
         z, x, UInt(n), base_ring(parent(x)))
   return z
end

function deflate(x::fq_default_poly, n::Int)
   z = parent(x)()
   ccall((:fq_default_poly_deflate, libflint), Nothing,
         (Ref{fq_default_poly}, Ref{fq_default_poly}, Culong, Ref{FqDefaultFiniteField}),
         z, x, UInt(n), base_ring(parent(x)))
  return z
end

################################################################################
#
#  Irreducibility
#
################################################################################

function isirreducible(x::fq_default_poly)
  return Bool(ccall((:fq_default_poly_is_irreducible, libflint), Int32,
                    (Ref{fq_default_poly}, Ref{FqDefaultFiniteField} ),
                    x, base_ring(parent(x))))
end

################################################################################
#
#  Squarefree testing
#
################################################################################

function issquarefree(x::fq_default_poly)
   return Bool(ccall((:fq_default_poly_is_squarefree, libflint), Int32,
       (Ref{fq_default_poly}, Ref{FqDefaultFiniteField}), x, base_ring(parent(x))))
end

################################################################################
#
#  Factorization
#
################################################################################

function exponent(x::fq_default_poly_factor, i::Int)
   return ccall((:fq_default_poly_factor_exp, libflint), Int,
                (Ref{fq_default_poly_factor}, Int, Ref{FqDefaultFiniteField}),
                 x, i, x.base_field)
end

function length(x::fq_default_poly_factor)
   return ccall((:fq_default_poly_factor_length, libflint), Int,
         (Ref{fq_default_poly_factor}, Ref{FqDefaultFiniteField}),
          x, x.base_field)
end   

function factor(x::fq_default_poly)
   fac, z = _factor(x)
   return Fac(parent(x)(z), fac)
end

function _factor(x::fq_default_poly)
   R = parent(x)
   F = base_ring(R)
   a = F()
   fac = fq_default_poly_factor(F)
   ccall((:fq_default_poly_factor, libflint), Nothing, (Ref{fq_default_poly_factor},
         Ref{fq_default}, Ref{fq_default_poly}, Ref{FqDefaultFiniteField}),
         fac, a, x, F)
   res = Dict{fq_default_poly,Int}()
   for i in 1:length(fac)
      f = R()
      ccall((:fq_default_poly_factor_get_poly, libflint), Nothing,
            (Ref{fq_default_poly}, Ref{fq_default_poly_factor}, Int,
            Ref{FqDefaultFiniteField}), f, fac, i - 1, F)
      e = exponent(fac, i - 1)
      res[f] = e
   end
   return res, a
end

function factor_squarefree(x::fq_default_poly)
  # _factor_squareefree does weird things if the polynomial is not monic
  return Fac(parent(x)(leading_coefficient(x)),
	      _factor_squarefree(divexact(x, leading_coefficient(x))))
end

function _factor_squarefree(x::fq_default_poly)
  F = base_ring(parent(x))
  fac = fq_default_poly_factor(F)
  ccall((:fq_default_poly_factor_squarefree, libflint), UInt,
        (Ref{fq_default_poly_factor}, Ref{fq_default_poly}, Ref{FqDefaultFiniteField}), fac, x, F)
  res = Dict{fq_default_poly,Int}()
  for i in 1:length(fac)
    f = parent(x)()
    ccall((:fq_default_poly_factor_get_poly, libflint), Nothing,
          (Ref{fq_default_poly}, Ref{fq_default_poly_factor}, Int,
          Ref{FqDefaultFiniteField}), f, fac, i-1, F)
    e = exponent(fac, i - 1)
    res[f] = e
  end
  return res
end

@doc Markdown.doc"""
    factor_distinct_deg(x::fq_default_poly)

Return the distinct degree factorisation of a squarefree polynomial $x$.
"""
function factor_distinct_deg(x::fq_default_poly)
   R = parent(x)
   F = base_ring(R)
   fac = fq_default_poly_factor(F)
   degrees = Vector{Int}(undef, degree(x))
   ccall((:fq_default_poly_factor_distinct_deg, libflint), Nothing,
         (Ref{fq_default_poly_factor}, Ref{fq_default_poly}, Ref{Vector{Int}},
         Ref{FqDefaultFiniteField}), fac, x, degrees, F)
   res = Dict{Int, fq_default_poly}()
   for i in 1:length(fac)
      f = R()
      ccall((:fq_default_poly_factor_get_poly, libflint), Nothing,
            (Ref{fq_default_poly}, Ref{fq_default_poly_factor}, Int,
            Ref{FqDefaultFiniteField}), f, fac, i-1, F)
      res[degrees[i]] = f
   end
   return res
end

################################################################################
#
#   Unsafe functions
#
################################################################################

function zero!(z::fq_default_poly)
   ccall((:fq_default_poly_zero, libflint), Nothing,
         (Ref{fq_default_poly}, Ref{FqDefaultFiniteField}),
         z, base_ring(parent(z)))
   return z
end

function fit!(z::fq_default_poly, n::Int)
   ccall((:fq_default_poly_fit_length, libflint), Nothing,
         (Ref{fq_default_poly}, Int, Ref{FqDefaultFiniteField}),
         z, n, base_ring(parent(z)))
   return nothing
end

function setcoeff!(z::fq_default_poly, n::Int, x::fq_default)
   ccall((:fq_default_poly_set_coeff, libflint), Nothing,
         (Ref{fq_default_poly}, Int, Ref{fq_default}, Ref{FqDefaultFiniteField}),
         z, n, x, base_ring(parent(z)))
   return z
end

function mul!(z::fq_default_poly, x::fq_default_poly, y::fq_default_poly)
   ccall((:fq_default_poly_mul, libflint), Nothing,
         (Ref{fq_default_poly}, Ref{fq_default_poly}, Ref{fq_default_poly},
         Ref{FqDefaultFiniteField}), z, x, y, base_ring(parent(x)))
   return z
end

function add!(z::fq_default_poly, x::fq_default_poly, y::fq_default_poly)
   ccall((:fq_default_poly_add, libflint), Nothing,
         (Ref{fq_default_poly}, Ref{fq_default_poly}, Ref{fq_default_poly},
         Ref{FqDefaultFiniteField}), z, x, y, base_ring(parent(x)))
   return z
end

function sub!(z::fq_default_poly, x::fq_default_poly, y::fq_default_poly)
   ccall((:fq_default_poly_sub, libflint), Nothing,
         (Ref{fq_default_poly}, Ref{fq_default_poly}, Ref{fq_default_poly},
         Ref{FqDefaultFiniteField}), z, x, y, base_ring(parent(x)))
   return z
end


function addeq!(z::fq_default_poly, x::fq_default_poly)
   ccall((:fq_default_poly_add, libflint), Nothing,
         (Ref{fq_default_poly}, Ref{fq_default_poly}, Ref{fq_default_poly},
         Ref{FqDefaultFiniteField}), z, z, x, base_ring(parent(x)))
   return z
end

################################################################################
#
#  Promotion rules
#
################################################################################

promote_rule(::Type{fq_default_poly}, ::Type{V}) where {V <: Integer} = fq_default_poly

promote_rule(::Type{fq_default_poly}, ::Type{fmpz}) = fq_default_poly

promote_rule(::Type{fq_default_poly}, ::Type{fq_default}) = fq_default_poly

###############################################################################
#
#   Polynomial substitution
#
###############################################################################

function (f::fq_default_poly)(a::fq_default)
   if parent(a) != base_ring(f)
      return subst(f, a)
   end
   return evaluate(f, a)
end

################################################################################
#
#   Parent object call overloads
#
################################################################################

function (R::FqDefaultPolyRing)()
   z = fq_default_poly(base_ring(R))
   z.parent = R
   return z
end

function (R::FqDefaultPolyRing)(x::fq_default)
  z = fq_default_poly(x, base_ring(R))
  z.parent = R
  return z
end

function (R::FqDefaultPolyRing)(x::fmpz)
   return R(base_ring(R)(x))
end

function (R::FqDefaultPolyRing)(x::Integer)
   return R(fmpz(x))
end

function (R::FqDefaultPolyRing)(x::Vector{fq_default})
   length(x) == 0 && return zero(R)
   base_ring(R) != parent(x[1]) && error("Coefficient rings must coincide")
   z = fq_default_poly(x, base_ring(R))
   z.parent = R
   return z
end

function (R::FqDefaultPolyRing)(x::Vector{fmpz})
   length(x) == 0 && return zero(R)
   z = fq_default_poly(x, base_ring(R))
   z.parent = R
   return z
end

function (R::FqDefaultPolyRing)(x::Vector{T}) where {T <: Integer}
   length(x) == 0 && return zero(R)
   return R(map(fmpz, x))
end

function (R::FqDefaultPolyRing)(x::fmpz_poly)
   z = fq_default_poly(x, base_ring(R))
   z.parent = R
   return z
end

function (R::FqDefaultPolyRing)(x::Union{nmod_poly, gfp_poly})
   characteristic(base_ring(x)) != characteristic(base_ring(R)) &&
                                   error("Incompatible characteristic")
   z = fq_default_poly(x, base_ring(R))
   z.parent = R
   return z
end

function (R::FqDefaultPolyRing)(x::Union{fmpz_mod_poly, gfp_fmpz_poly})
   characteristic(base_ring(x)) != characteristic(base_ring(R)) &&
                                   error("Incompatible characteristic")
   z = fq_default_poly(x, base_ring(R))
   z.parent = R
   return z
end

function (R::FqDefaultPolyRing)(x::fq_default_poly)
  parent(x) != R && error("Unable to coerce to polynomial")
  return x
end

################################################################################
#
#   PolynomialRing constructor
#
################################################################################

function PolynomialRing(R::FqDefaultFiniteField, s::AbstractString; cached = true)
   S = Symbol(s)
   parent_obj = FqDefaultPolyRing(R, S, cached)
   return parent_obj, parent_obj([R(0), R(1)])
end
