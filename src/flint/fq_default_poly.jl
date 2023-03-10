################################################################################
#
#  fq_default_poly.jl: Flint fq_default_poly
#                      (Polynomials over FqDefaultFiniteField)
#
################################################################################

export FqPolyRingElem, FqPolyRing

################################################################################
#
#  Type and parent object methods
#
################################################################################

parent_type(::Type{FqPolyRingElem}) = FqPolyRing

elem_type(::Type{FqPolyRing}) = FqPolyRingElem

base_ring(a::FqPolyRing) = a.base_ring

parent(a::FqPolyRingElem) = a.parent

var(a::FqPolyRing) = a.S

function check_parent(a::FqPolyRingElem, b::FqPolyRingElem)
   a.parent != b.parent &&
         error("Operations on distinct polynomial rings not supported")
end

################################################################################
#
#   Basic manipulation
#
################################################################################

function length(x::FqPolyRingElem)
   F = (x.parent).base_ring
   ccall((:fq_default_poly_length, libflint), Int,
                        (Ref{FqPolyRingElem}, Ref{FqField}), x, F)
end

function coeff(x::FqPolyRingElem, n::Int)
   n < 0 && throw(DomainError(n, "Index must be non-negative"))
   F = (x.parent).base_ring
   temp = F(1)
   ccall((:fq_default_poly_get_coeff, libflint), Nothing,
         (Ref{FqFieldElem}, Ref{FqPolyRingElem}, Int, Ref{FqField}),
         temp, x, n, F)
   return temp
end

function set_length!(x::FqPolyRingElem, n::Int)
   ctx = base_ring(x)
   ccall((:_fq_default_poly_set_length, libflint), Nothing,
         (Ref{FqPolyRingElem}, Int, Ref{FqField}), x, n, ctx)
   return x
end

zero(a::FqPolyRing) = a(zero(base_ring(a)))

one(a::FqPolyRing) = a(one(base_ring(a)))

gen(a::FqPolyRing) = a([zero(base_ring(a)), one(base_ring(a))])

is_gen(x::FqPolyRingElem) = ccall((:fq_default_poly_is_gen, libflint), Bool,
                              (Ref{FqPolyRingElem}, Ref{FqField}),
                              x, base_ring(x.parent))

iszero(x::FqPolyRingElem) = ccall((:fq_default_poly_is_zero, libflint), Bool,
                              (Ref{FqPolyRingElem}, Ref{FqField}),
                              x, base_ring(x.parent))

isone(x::FqPolyRingElem) = ccall((:fq_default_poly_is_one, libflint), Bool,
                              (Ref{FqPolyRingElem}, Ref{FqField}),
                              x, base_ring(x.parent))

degree(f::FqPolyRingElem) = length(f) - 1

function deepcopy_internal(a::FqPolyRingElem, dict::IdDict)
   z = FqPolyRingElem(a, base_ring(a))
   z.parent = a.parent
   return z
end

characteristic(R::FqPolyRing) = characteristic(base_ring(R))

###############################################################################
#
#   Similar
#
###############################################################################

function similar(f::PolyRingElem, R::FqField, s::Symbol=var(parent(f)); cached::Bool=true)
   z = FqPolyRingElem(R)
   if base_ring(f) === R && s == var(parent(f)) && typeof(f) == FqPolyRingElem
      # steal parent in case it is not cached
      z.parent = parent(f)
   else
      z.parent = FqPolyRing(R, s, cached)
   end
   return z
end

################################################################################
#
#   Canonicalisation
#
################################################################################

canonical_unit(a::FqPolyRingElem) = canonical_unit(leading_coefficient(a))

################################################################################
#
#  String I/O
#
################################################################################

function show(io::IO, R::FqPolyRing)
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

function -(x::FqPolyRingElem)
   z = parent(x)()
   ccall((:fq_default_poly_neg, libflint), Nothing,
         (Ref{FqPolyRingElem}, Ref{FqPolyRingElem}, Ref{FqField}),
         z, x, base_ring(parent(x)))
   return z
end

################################################################################
#
#  Binary operations
#
################################################################################

function +(x::FqPolyRingElem, y::FqPolyRingElem)
   check_parent(x,y)
   z = parent(x)()
   ccall((:fq_default_poly_add, libflint), Nothing,
         (Ref{FqPolyRingElem}, Ref{FqPolyRingElem},
         Ref{FqPolyRingElem}, Ref{FqField}),
         z, x, y, base_ring(parent(x)))
   return z
end

function -(x::FqPolyRingElem, y::FqPolyRingElem)
   check_parent(x,y)
   z = parent(x)()
   ccall((:fq_default_poly_sub, libflint), Nothing,
         (Ref{FqPolyRingElem}, Ref{FqPolyRingElem},
         Ref{FqPolyRingElem}, Ref{FqField}),
         z, x, y, base_ring(parent(x)))
   return z
end

function *(x::FqPolyRingElem, y::FqPolyRingElem)
   check_parent(x,y)
   z = parent(x)()
   ccall((:fq_default_poly_mul, libflint), Nothing,
         (Ref{FqPolyRingElem}, Ref{FqPolyRingElem},
         Ref{FqPolyRingElem}, Ref{FqField}),
         z, x, y, base_ring(parent(x)))
   return z
end

################################################################################
#
#   Ad hoc binary operators
#
################################################################################

function *(x::FqFieldElem, y::FqPolyRingElem)
   parent(x) != base_ring(parent(y)) &&
         error("Coefficient rings must be equal")
   z = parent(y)()
   ccall((:fq_default_poly_scalar_mul_fq_default, libflint), Nothing,
         (Ref{FqPolyRingElem}, Ref{FqPolyRingElem},
         Ref{FqFieldElem}, Ref{FqField}),
         z, y, x, parent(x))
  return z
end

*(x::FqPolyRingElem, y::FqFieldElem) = y*x

*(x::ZZRingElem, y::FqPolyRingElem) = base_ring(parent(y))(x) * y

*(x::FqPolyRingElem, y::ZZRingElem) = y*x

*(x::Integer, y::FqPolyRingElem) = ZZRingElem(x)*y

*(x::FqPolyRingElem, y::Integer) = y*x

+(x::FqFieldElem, y::FqPolyRingElem) = parent(y)(x) + y

+(x::FqPolyRingElem, y::FqFieldElem) = y + x

+(x::ZZRingElem, y::FqPolyRingElem) = base_ring(parent(y))(x) + y

+(x::FqPolyRingElem, y::ZZRingElem) = y + x

+(x::FqPolyRingElem, y::Integer) = x + ZZRingElem(y)

+(x::Integer, y::FqPolyRingElem) = y + x

-(x::FqFieldElem, y::FqPolyRingElem) = parent(y)(x) - y

-(x::FqPolyRingElem, y::FqFieldElem) = x - parent(x)(y)

-(x::ZZRingElem, y::FqPolyRingElem) = base_ring(parent(y))(x) - y

-(x::FqPolyRingElem, y::ZZRingElem) = x - base_ring(parent(x))(y)

-(x::FqPolyRingElem, y::Integer) = x - ZZRingElem(y)

-(x::Integer, y::FqPolyRingElem) = ZZRingElem(x) - y

################################################################################
#
#   Powering
#
################################################################################

function ^(x::FqPolyRingElem, y::Int)
   y < 0 && throw(DomainError(y, "Exponent must be non-negative"))
   z = parent(x)()
   ccall((:fq_default_poly_pow, libflint), Nothing,
         (Ref{FqPolyRingElem}, Ref{FqPolyRingElem}, Int, Ref{FqField}),
         z, x, y, base_ring(parent(x)))
   return z
end

################################################################################
#
#   Comparisons
#
################################################################################

function ==(x::FqPolyRingElem, y::FqPolyRingElem)
   check_parent(x,y)
   r = ccall((:fq_default_poly_equal, libflint), Cint,
             (Ref{FqPolyRingElem}, Ref{FqPolyRingElem}, Ref{FqField}),
             x, y, base_ring(parent(x)))
   return Bool(r)
end

################################################################################
#
#   Ad hoc comparisons
#
################################################################################

function ==(x::FqPolyRingElem, y::FqFieldElem)
   base_ring(parent(x)) != parent(y) && return false
   if length(x) > 1
      return false
   elseif length(x) == 1
      r = ccall((:fq_default_poly_equal_fq_default, libflint), Cint,
                (Ref{FqPolyRingElem}, Ref{FqFieldElem}, Ref{FqField}),
                x, y, base_ring(parent(x)))
      return Bool(r)
   else
      return iszero(y)
  end
end

==(x::FqFieldElem, y::FqPolyRingElem) = y == x

==(x::FqPolyRingElem, y::ZZRingElem) = x == base_ring(parent(x))(y)

==(x::ZZRingElem, y::FqPolyRingElem) = y == x

==(x::FqPolyRingElem, y::Integer) = x == ZZRingElem(y)

==(x::Integer, y::FqPolyRingElem) = y == x

################################################################################
#
#   Truncation
#
################################################################################

function truncate(x::FqPolyRingElem, n::Int)
   n < 0 && throw(DomainError(n, "Index must be non-negative"))
   if length(x) <= n
      return x
   end
   z = parent(x)()
   ccall((:fq_default_poly_set_trunc, libflint), Nothing,
         (Ref{FqPolyRingElem}, Ref{FqPolyRingElem}, Int, Ref{FqField}),
         z, x, n, base_ring(parent(x)))
   return z
end

function mullow(x::FqPolyRingElem, y::FqPolyRingElem, n::Int)
   check_parent(x,y)
   n < 0 && throw(DomainError(n, "Index must be non-negative"))
   z = parent(x)()
   ccall((:fq_default_poly_mullow, libflint), Nothing,
         (Ref{FqPolyRingElem}, Ref{FqPolyRingElem}, Ref{FqPolyRingElem},
         Int, Ref{FqField}),
         z, x, y, n, base_ring(parent(x)))
   return z
end

################################################################################
#
#   Reversal
#
################################################################################

function reverse(x::FqPolyRingElem, len::Int)
   len < 0 && throw(DomainError(len, "Index must be non-negative"))
   z = parent(x)()
   ccall((:fq_default_poly_reverse, libflint), Nothing,
         (Ref{FqPolyRingElem}, Ref{FqPolyRingElem}, Int, Ref{FqField}),
         z, x, len, base_ring(parent(x)))
   return z
end

################################################################################
#
#   Shifting
#
################################################################################

function shift_left(x::FqPolyRingElem, len::Int)
   len < 0 && throw(DomainError(len, "Shift must be non-negative"))
   z = parent(x)()
   ccall((:fq_default_poly_shift_left, libflint), Nothing,
         (Ref{FqPolyRingElem}, Ref{FqPolyRingElem}, Int, Ref{FqField}),
         z, x, len, base_ring(parent(x)))
   return z
end

function shift_right(x::FqPolyRingElem, len::Int)
   len < 0 && throw(DomainError(len, "Shift must be non-negative"))
   z = parent(x)()
   ccall((:fq_default_poly_shift_right, libflint), Nothing,
         (Ref{FqPolyRingElem}, Ref{FqPolyRingElem}, Int, Ref{FqField}),
         z, x, len, base_ring(parent(x)))
   return z
end

################################################################################
#
#   Euclidean division
#
################################################################################

function Base.div(x::FqPolyRingElem, y::FqPolyRingElem)
   check_parent(x,y)
   z = parent(x)()
   ccall((:fq_default_poly_div_basecase, libflint), Nothing,
         (Ref{FqPolyRingElem}, Ref{FqPolyRingElem}, Ref{FqPolyRingElem},
         Ref{FqField}), z, x, y, base_ring(parent(x)))
  return z
end

function rem(x::FqPolyRingElem, y::FqPolyRingElem)
   check_parent(x,y)
   z = parent(x)()
   ccall((:fq_default_poly_rem, libflint), Nothing,
         (Ref{FqPolyRingElem}, Ref{FqPolyRingElem}, Ref{FqPolyRingElem},
         Ref{FqField}), z, x, y, base_ring(parent(x)))
  return z
end

mod(x::FqPolyRingElem, y::FqPolyRingElem) = rem(x, y)

function Base.divrem(x::FqPolyRingElem, y::FqPolyRingElem)
   check_parent(x,y)
   z = parent(x)()
   r = parent(x)()
   ccall((:fq_default_poly_divrem, libflint), Nothing, (Ref{FqPolyRingElem},
         Ref{FqPolyRingElem}, Ref{FqPolyRingElem}, Ref{FqPolyRingElem},
         Ref{FqField}), z, r, x, y, base_ring(parent(x)))
   return z, r
end

################################################################################
#
#  Square root
#
################################################################################

function sqrt(x::FqPolyRingElem; check::Bool=true)
   R = parent(x)
   s = R()
   flag = Bool(ccall((:fq_default_poly_sqrt, libflint), Cint,
                     (Ref{FqPolyRingElem}, Ref{FqPolyRingElem}, Ref{FqField}),
                      s, x, base_ring(parent(x))))
   check && !flag && error("Not a square in sqrt")
   return s
end

function issquare(x::FqPolyRingElem)
   if iszero(x)
      return true
   end
   if !iseven(degree(x))
      return false
   end
   R = parent(x)
   s = R()
   flag = Bool(ccall((:fq_default_poly_sqrt, libflint), Cint,
                     (Ref{FqPolyRingElem}, Ref{FqPolyRingElem}, Ref{FqField}),
                      s, x, base_ring(parent(x))))
   return flag
end

function issquare_with_sqrt(x::FqPolyRingElem)
   R = parent(x)
   if iszero(x)
      return true, zero(R)
   end
   if !iseven(degree(x))
      return false, zero(R)
   end
   s = R()
   flag = Bool(ccall((:fq_default_poly_sqrt, libflint), Cint,
                     (Ref{FqPolyRingElem}, Ref{FqPolyRingElem}, Ref{FqField}),
                      s, x, base_ring(parent(x))))
   return flag, s
end

################################################################################
#
#   Remove and valuation
#
################################################################################

function remove(z::FqPolyRingElem, p::FqPolyRingElem)
   ok, v = _remove_check_simple_cases(z, p)
   ok && return v, zero(parent(z))
   z = deepcopy(z)
   v = ccall((:fq_default_poly_remove, libflint), Int,
            (Ref{FqPolyRingElem}, Ref{FqPolyRingElem}, Ref{FqField}),
             z, p, base_ring(parent(z)))
   return v, z
end

function divides(z::FqPolyRingElem, x::FqPolyRingElem)
   if iszero(z)
      return true, zero(parent(z))
   end
   if iszero(x)
      return false, zero(parent(z))
   end
   check_parent(z, x)
   q = parent(z)()
   v = Bool(ccall((:fq_default_poly_divides, libflint), Cint,
            (Ref{FqPolyRingElem}, Ref{FqPolyRingElem},
             Ref{FqPolyRingElem}, Ref{FqField}),
             q, z, x, base_ring(parent(z))))
   return v, q
end

################################################################################
#
#   Modular arithmetic
#
################################################################################

function powermod(x::FqPolyRingElem, n::Int, y::FqPolyRingElem)
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
         (Ref{FqPolyRingElem}, Ref{FqPolyRingElem}, Int, Ref{FqPolyRingElem},
         Ref{FqField}), z, x, n, y, base_ring(parent(x)))
  return z
end

function powermod(x::FqPolyRingElem, n::ZZRingElem, y::FqPolyRingElem)
   check_parent(x,y)
   z = parent(x)()

   if n < 0
      g, x = gcdinv(x, y)
      if !isone(g)
         error("Element not invertible")
      end
      n = -n
   end
   # https://github.com/flintlib/flint2/pull/1261
   if _fq_default_ctx_type(base_ring(parent(x))) == 4
      ccall((:nmod_poly_powmod_fmpz_binexp, libflint), Nothing,
            (Ref{FqPolyRingElem}, Ref{FqPolyRingElem}, Ref{ZZRingElem}, Ref{FqPolyRingElem}), z, x, n, y)
   else
     ccall((:fq_default_poly_powmod_fmpz_binexp, libflint), Nothing,
           (Ref{FqPolyRingElem}, Ref{FqPolyRingElem}, Ref{ZZRingElem}, Ref{FqPolyRingElem},
           Ref{FqField}), z, x, n, y, base_ring(parent(x)))
   end
   return z
end

################################################################################
#
#   GCD
#
################################################################################

function gcd(x::FqPolyRingElem, y::FqPolyRingElem)
   check_parent(x,y)
   z = parent(x)()
   ccall((:fq_default_poly_gcd, libflint), Nothing,
         (Ref{FqPolyRingElem}, Ref{FqPolyRingElem}, Ref{FqPolyRingElem},
         Ref{FqField}), z, x, y, base_ring(parent(x)))
   return z
end

function gcdinv(x::FqPolyRingElem, y::FqPolyRingElem)
   check_parent(x,y)
   z = parent(x)()
   s = parent(x)()
   t = parent(x)()
   ccall((:fq_default_poly_xgcd, libflint), Nothing,
         (Ref{FqPolyRingElem}, Ref{FqPolyRingElem}, Ref{FqPolyRingElem}, Ref{FqPolyRingElem}, Ref{FqPolyRingElem},
         Ref{FqField}), z, s, t, x, y, base_ring(parent(x)))
   return z, s
end

function gcdx(x::FqPolyRingElem, y::FqPolyRingElem)
   check_parent(x,y)
   z = parent(x)()
   s = parent(x)()
   t = parent(x)()
   ccall((:fq_default_poly_xgcd, libflint), Nothing,
         (Ref{FqPolyRingElem}, Ref{FqPolyRingElem}, Ref{FqPolyRingElem}, Ref{FqPolyRingElem}, Ref{FqPolyRingElem},
         Ref{FqField}), z, s, t, x, y, base_ring(parent(x)))
   return z, s, t
end

################################################################################
#
#   Evaluation
#
################################################################################

function evaluate(x::FqPolyRingElem, y::FqFieldElem)
   base_ring(parent(x)) != parent(y) && error("Incompatible coefficient rings")
   z = parent(y)()
   ccall((:fq_default_poly_evaluate_fq_default, libflint), Nothing,
         (Ref{FqFieldElem}, Ref{FqPolyRingElem}, Ref{FqFieldElem},
         Ref{FqField}), z, x, y, base_ring(parent(x)))
   return z
end

################################################################################
#
#   Composition
#
################################################################################

function compose(x::FqPolyRingElem, y::FqPolyRingElem)
   check_parent(x,y)
   z = parent(x)()
   ccall((:fq_default_poly_compose, libflint), Nothing,
         (Ref{FqPolyRingElem}, Ref{FqPolyRingElem}, Ref{FqPolyRingElem},
         Ref{FqField}), z, x, y, base_ring(parent(x)))
   return z
end

################################################################################
#
#   Derivative
#
################################################################################

function derivative(x::FqPolyRingElem)
   z = parent(x)()
   ccall((:fq_default_poly_derivative, libflint), Nothing,
         (Ref{FqPolyRingElem}, Ref{FqPolyRingElem}, Ref{FqField}),
         z, x, base_ring(parent(x)))
   return z
end

################################################################################
#
#  Inflation and deflation
#
################################################################################

function inflate(x::FqPolyRingElem, n::Int)
   z = parent(x)()
   ccall((:fq_default_poly_inflate, libflint), Nothing, (Ref{FqPolyRingElem},
         Ref{FqPolyRingElem}, Culong, Ref{FqField}),
         z, x, UInt(n), base_ring(parent(x)))
   return z
end

function deflate(x::FqPolyRingElem, n::Int)
   z = parent(x)()
   ccall((:fq_default_poly_deflate, libflint), Nothing,
         (Ref{FqPolyRingElem}, Ref{FqPolyRingElem}, Culong, Ref{FqField}),
         z, x, UInt(n), base_ring(parent(x)))
  return z
end

################################################################################
#
#  Irreducibility
#
################################################################################

function is_irreducible(x::FqPolyRingElem)
  return Bool(ccall((:fq_default_poly_is_irreducible, libflint), Int32,
                    (Ref{FqPolyRingElem}, Ref{FqField} ),
                    x, base_ring(parent(x))))
end

################################################################################
#
#  Squarefree testing
#
################################################################################

function is_squarefree(x::FqPolyRingElem)
   return Bool(ccall((:fq_default_poly_is_squarefree, libflint), Int32,
       (Ref{FqPolyRingElem}, Ref{FqField}), x, base_ring(parent(x))))
end

################################################################################
#
#  Factorization
#
################################################################################

function exponent(x::fq_default_poly_factor, i::Int)
   return ccall((:fq_default_poly_factor_exp, libflint), Int,
                (Ref{fq_default_poly_factor}, Int, Ref{FqField}),
                 x, i, x.base_field)
end

function length(x::fq_default_poly_factor)
   return ccall((:fq_default_poly_factor_length, libflint), Int,
         (Ref{fq_default_poly_factor}, Ref{FqField}),
          x, x.base_field)
end   

function factor(x::FqPolyRingElem)
   fac, z = _factor(x)
   return Fac(parent(x)(z), fac)
end

function _factor(x::FqPolyRingElem)
   R = parent(x)
   F = base_ring(R)
   a = F()
   fac = fq_default_poly_factor(F)
   ccall((:fq_default_poly_factor, libflint), Nothing, (Ref{fq_default_poly_factor},
         Ref{FqFieldElem}, Ref{FqPolyRingElem}, Ref{FqField}),
         fac, a, x, F)
   res = Dict{FqPolyRingElem,Int}()
   for i in 1:length(fac)
      f = R()
      ccall((:fq_default_poly_factor_get_poly, libflint), Nothing,
            (Ref{FqPolyRingElem}, Ref{fq_default_poly_factor}, Int,
            Ref{FqField}), f, fac, i - 1, F)
      e = exponent(fac, i - 1)
      res[f] = e
   end
   return res, a
end

function factor_squarefree(x::FqPolyRingElem)
  # _factor_squareefree does weird things if the polynomial is not monic
  return Fac(parent(x)(leading_coefficient(x)),
	      _factor_squarefree(divexact(x, leading_coefficient(x))))
end

function _factor_squarefree(x::FqPolyRingElem)
  F = base_ring(parent(x))
  fac = fq_default_poly_factor(F)
  ccall((:fq_default_poly_factor_squarefree, libflint), UInt,
        (Ref{fq_default_poly_factor}, Ref{FqPolyRingElem}, Ref{FqField}), fac, x, F)
  res = Dict{FqPolyRingElem,Int}()
  for i in 1:length(fac)
    f = parent(x)()
    ccall((:fq_default_poly_factor_get_poly, libflint), Nothing,
          (Ref{FqPolyRingElem}, Ref{fq_default_poly_factor}, Int,
          Ref{FqField}), f, fac, i-1, F)
    e = exponent(fac, i - 1)
    res[f] = e
  end
  return res
end

@doc Markdown.doc"""
    factor_distinct_deg(x::FqPolyRingElem)

Return the distinct degree factorisation of a squarefree polynomial $x$.
"""
function factor_distinct_deg(x::FqPolyRingElem)
   R = parent(x)
   F = base_ring(R)
   fac = fq_default_poly_factor(F)
   degrees = Vector{Int}(undef, degree(x))
   ccall((:fq_default_poly_factor_distinct_deg, libflint), Nothing,
         (Ref{fq_default_poly_factor}, Ref{FqPolyRingElem}, Ref{Vector{Int}},
         Ref{FqField}), fac, x, degrees, F)
   res = Dict{Int, FqPolyRingElem}()
   for i in 1:length(fac)
      f = R()
      ccall((:fq_default_poly_factor_get_poly, libflint), Nothing,
            (Ref{FqPolyRingElem}, Ref{fq_default_poly_factor}, Int,
            Ref{FqField}), f, fac, i-1, F)
      res[degrees[i]] = f
   end
   return res
end

function roots(x::FqPolyRingElem)
   R = parent(x)
   F = base_ring(R)
   fac = fq_default_poly_factor(F)
   ccall((:fq_default_poly_roots, libflint), Nothing,
         (Ref{fq_default_poly_factor}, Ref{FqPolyRingElem}, Cint,
         Ref{FqField}), fac, x, 0, F)
   res = FqFieldElem[]
   for i in 1:length(fac)
      f = R()
      ccall((:fq_default_poly_factor_get_poly, libflint), Nothing,
            (Ref{FqPolyRingElem}, Ref{fq_default_poly_factor}, Int,
            Ref{FqField}), f, fac, i-1, F)
      @assert isone(coeff(f, 1))
      push!(res, -coeff(f, 0))
   end
   return res
end

################################################################################
#
#   Unsafe functions
#
################################################################################

function zero!(z::FqPolyRingElem)
   ccall((:fq_default_poly_zero, libflint), Nothing,
         (Ref{FqPolyRingElem}, Ref{FqField}),
         z, base_ring(parent(z)))
   return z
end

function fit!(z::FqPolyRingElem, n::Int)
   ccall((:fq_default_poly_fit_length, libflint), Nothing,
         (Ref{FqPolyRingElem}, Int, Ref{FqField}),
         z, n, base_ring(parent(z)))
   return nothing
end

function setcoeff!(z::FqPolyRingElem, n::Int, x::FqFieldElem)
   ccall((:fq_default_poly_set_coeff, libflint), Nothing,
         (Ref{FqPolyRingElem}, Int, Ref{FqFieldElem}, Ref{FqField}),
         z, n, x, base_ring(parent(z)))
   return z
end

function mul!(z::FqPolyRingElem, x::FqPolyRingElem, y::FqPolyRingElem)
   ccall((:fq_default_poly_mul, libflint), Nothing,
         (Ref{FqPolyRingElem}, Ref{FqPolyRingElem}, Ref{FqPolyRingElem},
         Ref{FqField}), z, x, y, base_ring(parent(x)))
   return z
end

function add!(z::FqPolyRingElem, x::FqPolyRingElem, y::FqPolyRingElem)
   ccall((:fq_default_poly_add, libflint), Nothing,
         (Ref{FqPolyRingElem}, Ref{FqPolyRingElem}, Ref{FqPolyRingElem},
         Ref{FqField}), z, x, y, base_ring(parent(x)))
   return z
end

function sub!(z::FqPolyRingElem, x::FqPolyRingElem, y::FqPolyRingElem)
   ccall((:fq_default_poly_sub, libflint), Nothing,
         (Ref{FqPolyRingElem}, Ref{FqPolyRingElem}, Ref{FqPolyRingElem},
         Ref{FqField}), z, x, y, base_ring(parent(x)))
   return z
end


function addeq!(z::FqPolyRingElem, x::FqPolyRingElem)
   ccall((:fq_default_poly_add, libflint), Nothing,
         (Ref{FqPolyRingElem}, Ref{FqPolyRingElem}, Ref{FqPolyRingElem},
         Ref{FqField}), z, z, x, base_ring(parent(x)))
   return z
end

################################################################################
#
#  Promotion rules
#
################################################################################

promote_rule(::Type{FqPolyRingElem}, ::Type{V}) where {V <: Integer} = FqPolyRingElem

promote_rule(::Type{FqPolyRingElem}, ::Type{ZZRingElem}) = FqPolyRingElem

promote_rule(::Type{FqPolyRingElem}, ::Type{FqFieldElem}) = FqPolyRingElem

###############################################################################
#
#   Polynomial substitution
#
###############################################################################

function (f::FqPolyRingElem)(a::FqFieldElem)
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

function (R::FqPolyRing)()
   z = FqPolyRingElem(base_ring(R))
   z.parent = R
   return z
end

function (R::FqPolyRing)(x::FqFieldElem)
  parent(x) !== base_ring(R) && error("Element not contained in coefficient ring")
  z = FqPolyRingElem(x, base_ring(R))
  z.parent = R
  return z
end

function (R::FqPolyRing)(x::ZZRingElem)
   return R(base_ring(R)(x))
end

function (R::FqPolyRing)(x::Integer)
   return R(ZZRingElem(x))
end

function (R::FqPolyRing)(x::Vector{FqFieldElem})
   length(x) == 0 && return zero(R)
   base_ring(R) != parent(x[1]) && error("Coefficient rings must coincide")
   z = FqPolyRingElem(x, base_ring(R))
   z.parent = R
   return z
end

function (R::FqPolyRing)(x::Vector{ZZRingElem})
   length(x) == 0 && return zero(R)
   z = FqPolyRingElem(x, base_ring(R))
   z.parent = R
   return z
end

function (R::FqPolyRing)(x::Vector{T}) where {T <: Integer}
   length(x) == 0 && return zero(R)
   return R(map(ZZRingElem, x))
end

function (R::FqPolyRing)(x::ZZPolyRingElem)
   z = FqPolyRingElem(x, base_ring(R))
   z.parent = R
   return z
end

function (R::FqPolyRing)(x::Union{zzModPolyRingElem, fpPolyRingElem})
   characteristic(base_ring(x)) != characteristic(base_ring(R)) &&
                                   error("Incompatible characteristic")
   z = FqPolyRingElem(x, base_ring(R))
   z.parent = R
   return z
end

function (R::FqPolyRing)(x::Union{ZZModPolyRingElem, FpPolyRingElem})
   characteristic(base_ring(x)) != characteristic(base_ring(R)) &&
                                   error("Incompatible characteristic")
   z = FqPolyRingElem(x, base_ring(R))
   z.parent = R
   return z
end

function (R::FqPolyRing)(x::FqPolyRingElem)
  parent(x) != R && error("Unable to coerce to polynomial")
  return x
end

################################################################################
#
#   polynomial_ring constructor
#
################################################################################

function polynomial_ring(R::FqField, s::AbstractString; cached = true)
   S = Symbol(s)
   parent_obj = FqPolyRing(R, S, cached)
   return parent_obj, parent_obj([R(0), R(1)])
end
