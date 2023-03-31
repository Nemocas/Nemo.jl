################################################################################
#
#  fq_nmod_poly.jl: Flint fq_mod_poly (Polynomials over FqNmodFiniteField)
#
################################################################################

export fqPolyRepPolyRingElem, fqPolyRepPolyRing

################################################################################
#
#  Type and parent object methods
#
################################################################################

parent_type(::Type{fqPolyRepPolyRingElem}) = fqPolyRepPolyRing

elem_type(::Type{fqPolyRepPolyRing}) = fqPolyRepPolyRingElem

dense_poly_type(::Type{fqPolyRepFieldElem}) = fqPolyRepPolyRingElem

base_ring(a::fqPolyRepPolyRing) = a.base_ring

parent(a::fqPolyRepPolyRingElem) = a.parent

var(a::fqPolyRepPolyRing) = a.S

function check_parent(a::fqPolyRepPolyRingElem, b::fqPolyRepPolyRingElem)
   a.parent != b.parent &&
         error("Operations on distinct polynomial rings not supported")
end

################################################################################
#
#   Basic manipulation
#
################################################################################

length(x::fqPolyRepPolyRingElem) = ccall((:fq_nmod_poly_length, libflint), Int,
                                (Ref{fqPolyRepPolyRingElem},), x)

function set_length!(x::fqPolyRepPolyRingElem, n::Int)
   ccall((:_fq_nmod_poly_set_length, libflint), Nothing,
                              (Ref{fqPolyRepPolyRingElem}, Int), x, n)
   return x
end

function coeff(x::fqPolyRepPolyRingElem, n::Int)
   n < 0 && throw(DomainError(n, "Index must be non-negative"))
   F = (x.parent).base_ring
   temp = F(1)
   ccall((:fq_nmod_poly_get_coeff, libflint), Nothing,
         (Ref{fqPolyRepFieldElem}, Ref{fqPolyRepPolyRingElem}, Int, Ref{fqPolyRepField}),
         temp, x, n, F)
   return temp
end

zero(a::fqPolyRepPolyRing) = a(zero(base_ring(a)))

one(a::fqPolyRepPolyRing) = a(one(base_ring(a)))

gen(a::fqPolyRepPolyRing) = a([zero(base_ring(a)), one(base_ring(a))])

iszero(x::fqPolyRepPolyRingElem) = ccall((:fq_nmod_poly_is_zero, libflint), Bool,
                              (Ref{fqPolyRepPolyRingElem}, Ref{fqPolyRepField}),
                              x, base_ring(x.parent))

isone(x::fqPolyRepPolyRingElem) = ccall((:fq_nmod_poly_is_one, libflint), Bool,
                              (Ref{fqPolyRepPolyRingElem}, Ref{fqPolyRepField}),
                              x, base_ring(x.parent))

is_gen(x::fqPolyRepPolyRingElem) = ccall((:fq_nmod_poly_is_gen, libflint), Bool,
                              (Ref{fqPolyRepPolyRingElem}, Ref{fqPolyRepField}),
                              x, base_ring(x.parent))

degree(f::fqPolyRepPolyRingElem) = f.length - 1

function deepcopy_internal(a::fqPolyRepPolyRingElem, dict::IdDict)
   z = fqPolyRepPolyRingElem(a)
   z.parent = a.parent
   return z
end

characteristic(R::fqPolyRepPolyRing) = characteristic(base_ring(R))

###############################################################################
#
#   Similar
#
###############################################################################

function similar(f::PolyRingElem, R::fqPolyRepField, s::Symbol=var(parent(f)); cached::Bool=true)
   z = fqPolyRepPolyRingElem()
   if base_ring(f) === R && s == var(parent(f)) && f isa fqPolyRepPolyRingElem
      # steal parent in case it is not cached
      z.parent = parent(f)
   else
      z.parent = fqPolyRepPolyRing(R, s, cached)
   end
   return z
end

###############################################################################
#
#   polynomial constructor
#
###############################################################################

function polynomial(R::fqPolyRepField, arr::Vector{T}, var::VarName=:x; cached::Bool=true) where T
   coeffs = map(R, arr)
   z = length(coeffs) == 0 ? fqPolyRepPolyRingElem() : fqPolyRepPolyRingElem(coeffs)
   z.parent = fqPolyRepPolyRing(R, Symbol(var), cached)
   return z
end

################################################################################
#
#   Canonicalisation
#
################################################################################

canonical_unit(a::fqPolyRepPolyRingElem) = canonical_unit(leading_coefficient(a))

################################################################################
#
#  Unary operations
#
################################################################################

function -(x::fqPolyRepPolyRingElem)
   z = parent(x)()
   ccall((:fq_nmod_poly_neg, libflint), Nothing,
         (Ref{fqPolyRepPolyRingElem}, Ref{fqPolyRepPolyRingElem}, Ref{fqPolyRepField}),
         z, x, base_ring(parent(x)))
   return z
end

################################################################################
#
#  Binary operations
#
################################################################################

function +(x::fqPolyRepPolyRingElem, y::fqPolyRepPolyRingElem)
   check_parent(x,y)
   z = parent(x)()
   ccall((:fq_nmod_poly_add, libflint), Nothing,
         (Ref{fqPolyRepPolyRingElem}, Ref{fqPolyRepPolyRingElem},
         Ref{fqPolyRepPolyRingElem}, Ref{fqPolyRepField}),
         z, x, y, base_ring(parent(x)))
   return z
end

function -(x::fqPolyRepPolyRingElem, y::fqPolyRepPolyRingElem)
   check_parent(x,y)
   z = parent(x)()
   ccall((:fq_nmod_poly_sub, libflint), Nothing,
         (Ref{fqPolyRepPolyRingElem}, Ref{fqPolyRepPolyRingElem},
         Ref{fqPolyRepPolyRingElem}, Ref{fqPolyRepField}),
         z, x, y, base_ring(parent(x)))
   return z
end

function *(x::fqPolyRepPolyRingElem, y::fqPolyRepPolyRingElem)
   check_parent(x,y)
   z = parent(x)()
   ccall((:fq_nmod_poly_mul, libflint), Nothing,
         (Ref{fqPolyRepPolyRingElem}, Ref{fqPolyRepPolyRingElem},
         Ref{fqPolyRepPolyRingElem}, Ref{fqPolyRepField}),
         z, x, y, base_ring(parent(x)))
   return z
end

################################################################################
#
#   Ad hoc binary operators
#
################################################################################

function *(x::fqPolyRepFieldElem, y::fqPolyRepPolyRingElem)
   parent(x) != base_ring(parent(y)) &&
         error("Coefficient rings must be equal")
   z = parent(y)()
   ccall((:fq_nmod_poly_scalar_mul_fq_nmod, libflint), Nothing,
         (Ref{fqPolyRepPolyRingElem}, Ref{fqPolyRepPolyRingElem},
         Ref{fqPolyRepFieldElem}, Ref{fqPolyRepField}),
         z, y, x, parent(x))
  return z
end

*(x::fqPolyRepPolyRingElem, y::fqPolyRepFieldElem) = y*x

*(x::ZZRingElem, y::fqPolyRepPolyRingElem) = base_ring(parent(y))(x) * y

*(x::fqPolyRepPolyRingElem, y::ZZRingElem) = y*x

*(x::Integer, y::fqPolyRepPolyRingElem) = ZZRingElem(x)*y

*(x::fqPolyRepPolyRingElem, y::Integer) = y*x

+(x::fqPolyRepFieldElem, y::fqPolyRepPolyRingElem) = parent(y)(x) + y

+(x::fqPolyRepPolyRingElem, y::fqPolyRepFieldElem) = y + x

+(x::ZZRingElem, y::fqPolyRepPolyRingElem) = base_ring(parent(y))(x) + y

+(x::fqPolyRepPolyRingElem, y::ZZRingElem) = y + x

+(x::fqPolyRepPolyRingElem, y::Integer) = x + ZZRingElem(y)

+(x::Integer, y::fqPolyRepPolyRingElem) = y + x

-(x::fqPolyRepFieldElem, y::fqPolyRepPolyRingElem) = parent(y)(x) - y

-(x::fqPolyRepPolyRingElem, y::fqPolyRepFieldElem) = x - parent(x)(y)

-(x::ZZRingElem, y::fqPolyRepPolyRingElem) = base_ring(parent(y))(x) - y

-(x::fqPolyRepPolyRingElem, y::ZZRingElem) = x - base_ring(parent(x))(y)

-(x::fqPolyRepPolyRingElem, y::Integer) = x - ZZRingElem(y)

-(x::Integer, y::fqPolyRepPolyRingElem) = ZZRingElem(x) - y

################################################################################
#
#   Powering
#
################################################################################

function ^(x::fqPolyRepPolyRingElem, y::Int)
   y < 0 && throw(DomainError(y, "Exponent must be non-negative"))
   z = parent(x)()
   ccall((:fq_nmod_poly_pow, libflint), Nothing,
         (Ref{fqPolyRepPolyRingElem}, Ref{fqPolyRepPolyRingElem}, Int, Ref{fqPolyRepField}),
         z, x, y, base_ring(parent(x)))
   return z
end

################################################################################
#
#   Comparisons
#
################################################################################

function ==(x::fqPolyRepPolyRingElem, y::fqPolyRepPolyRingElem)
   check_parent(x,y)
   r = ccall((:fq_nmod_poly_equal, libflint), Cint,
             (Ref{fqPolyRepPolyRingElem}, Ref{fqPolyRepPolyRingElem}, Ref{fqPolyRepField}),
             x, y, base_ring(parent(x)))
   return Bool(r)
end

################################################################################
#
#   Ad hoc comparisons
#
################################################################################

function ==(x::fqPolyRepPolyRingElem, y::fqPolyRepFieldElem)
   base_ring(parent(x)) != parent(y) && return false
   if length(x) > 1
      return false
   elseif length(x) == 1
      r = ccall((:fq_nmod_poly_equal_fq_nmod, libflint), Cint,
                (Ref{fqPolyRepPolyRingElem}, Ref{fqPolyRepFieldElem}, Ref{fqPolyRepField}),
                x, y, base_ring(parent(x)))
      return Bool(r)
   else
      return iszero(y)
  end
end

==(x::fqPolyRepFieldElem, y::fqPolyRepPolyRingElem) = y == x

==(x::fqPolyRepPolyRingElem, y::ZZRingElem) = x == base_ring(parent(x))(y)

==(x::ZZRingElem, y::fqPolyRepPolyRingElem) = y == x

==(x::fqPolyRepPolyRingElem, y::Integer) = x == ZZRingElem(y)

==(x::Integer, y::fqPolyRepPolyRingElem) = y == x

################################################################################
#
#   Truncation
#
################################################################################

function truncate(x::fqPolyRepPolyRingElem, n::Int)
   n < 0 && throw(DomainError(n, "Index must be non-negative"))
   if length(x) <= n
      return x
   end
   z = parent(x)()
   ccall((:fq_nmod_poly_set_trunc, libflint), Nothing,
         (Ref{fqPolyRepPolyRingElem}, Ref{fqPolyRepPolyRingElem}, Int, Ref{fqPolyRepField}),
         z, x, n, base_ring(parent(x)))
   return z
end

function mullow(x::fqPolyRepPolyRingElem, y::fqPolyRepPolyRingElem, n::Int)
   check_parent(x,y)
   n < 0 && throw(DomainError(n, "Index must be non-negative"))
   z = parent(x)()
   ccall((:fq_nmod_poly_mullow, libflint), Nothing,
         (Ref{fqPolyRepPolyRingElem}, Ref{fqPolyRepPolyRingElem}, Ref{fqPolyRepPolyRingElem},
         Int, Ref{fqPolyRepField}),
         z, x, y, n, base_ring(parent(x)))
   return z
end

################################################################################
#
#   Reversal
#
################################################################################

function reverse(x::fqPolyRepPolyRingElem, len::Int)
   len < 0 && throw(DomainError(len, "Index must be non-negative"))
   z = parent(x)()
   ccall((:fq_nmod_poly_reverse, libflint), Nothing,
         (Ref{fqPolyRepPolyRingElem}, Ref{fqPolyRepPolyRingElem}, Int, Ref{fqPolyRepField}),
         z, x, len, base_ring(parent(x)))
   return z
end

################################################################################
#
#   Shifting
#
################################################################################

function shift_left(x::fqPolyRepPolyRingElem, len::Int)
   len < 0 && throw(DomainError(len, "Shift must be non-negative"))
   z = parent(x)()
   ccall((:fq_nmod_poly_shift_left, libflint), Nothing,
         (Ref{fqPolyRepPolyRingElem}, Ref{fqPolyRepPolyRingElem}, Int, Ref{fqPolyRepField}),
         z, x, len, base_ring(parent(x)))
   return z
end

function shift_right(x::fqPolyRepPolyRingElem, len::Int)
   len < 0 && throw(DomainError(len, "Shift must be non-negative"))
   z = parent(x)()
   ccall((:fq_nmod_poly_shift_right, libflint), Nothing,
         (Ref{fqPolyRepPolyRingElem}, Ref{fqPolyRepPolyRingElem}, Int, Ref{fqPolyRepField}),
         z, x, len, base_ring(parent(x)))
   return z
end

################################################################################
#
#   Euclidean division
#
################################################################################

function Base.div(x::fqPolyRepPolyRingElem, y::fqPolyRepPolyRingElem)
   check_parent(x,y)
   z = parent(x)()
   ccall((:fq_nmod_poly_div_basecase, libflint), Nothing,
         (Ref{fqPolyRepPolyRingElem}, Ref{fqPolyRepPolyRingElem}, Ref{fqPolyRepPolyRingElem},
         Ref{fqPolyRepField}), z, x, y, base_ring(parent(x)))
  return z
end

function rem(x::fqPolyRepPolyRingElem, y::fqPolyRepPolyRingElem)
   check_parent(x,y)
   z = parent(x)()
   ccall((:fq_nmod_poly_rem, libflint), Nothing,
         (Ref{fqPolyRepPolyRingElem}, Ref{fqPolyRepPolyRingElem}, Ref{fqPolyRepPolyRingElem},
         Ref{fqPolyRepField}), z, x, y, base_ring(parent(x)))
  return z
end

mod(x::fqPolyRepPolyRingElem, y::fqPolyRepPolyRingElem) = rem(x, y)

function Base.divrem(x::fqPolyRepPolyRingElem, y::fqPolyRepPolyRingElem)
   check_parent(x,y)
   z = parent(x)()
   r = parent(x)()
   ccall((:fq_nmod_poly_divrem, libflint), Nothing, (Ref{fqPolyRepPolyRingElem},
         Ref{fqPolyRepPolyRingElem}, Ref{fqPolyRepPolyRingElem}, Ref{fqPolyRepPolyRingElem},
         Ref{fqPolyRepField}), z, r, x, y, base_ring(parent(x)))
   return z,r
end

################################################################################
#
#  Square root
#
################################################################################

function sqrt(x::fqPolyRepPolyRingElem; check::Bool=true)
   R = parent(x)
   s = R()
   flag = Bool(ccall((:fq_nmod_poly_sqrt, libflint), Cint,
                     (Ref{fqPolyRepPolyRingElem}, Ref{fqPolyRepPolyRingElem}, Ref{fqPolyRepField}),
                      s, x, base_ring(parent(x))))
   check && !flag && error("Not a square in sqrt")
   return s
end

function issquare(x::fqPolyRepPolyRingElem)
   if iszero(x)
      return true
   end
   if !iseven(degree(x))
      return false
   end
   R = parent(x)
   s = R()
   flag = Bool(ccall((:fq_nmod_poly_sqrt, libflint), Cint,
                     (Ref{fqPolyRepPolyRingElem}, Ref{fqPolyRepPolyRingElem}, Ref{fqPolyRepField}),
                      s, x, base_ring(parent(x))))
   return flag
end

function issquare_with_sqrt(x::fqPolyRepPolyRingElem)
   R = parent(x)
   if iszero(x)
      return true, zero(R)
   end
   if !iseven(degree(x))
      return false, zero(R)
   end
   s = R()
   flag = Bool(ccall((:fq_nmod_poly_sqrt, libflint), Cint,
                     (Ref{fqPolyRepPolyRingElem}, Ref{fqPolyRepPolyRingElem}, Ref{fqPolyRepField}),
                      s, x, base_ring(parent(x))))
   return flag, s
end

################################################################################
#
#   Remove and valuation
#
################################################################################

function remove(z::fqPolyRepPolyRingElem, p::fqPolyRepPolyRingElem)
   ok, v = _remove_check_simple_cases(z, p)
   ok && return v, zero(parent(z))
   z = deepcopy(z)
   v = ccall((:fq_nmod_poly_remove, libflint), Int,
            (Ref{fqPolyRepPolyRingElem}, Ref{fqPolyRepPolyRingElem}, Ref{fqPolyRepField}),
             z,  p, base_ring(parent(z)))
   return v, z
end

function divides(z::fqPolyRepPolyRingElem, x::fqPolyRepPolyRingElem)
   check_parent(z, x)
   if iszero(z)
      return true, zero(parent(z))
   end
   if iszero(x)
      return false, zero(parent(z))
   end
   q = parent(z)()
   v = Bool(ccall((:fq_nmod_poly_divides, libflint), Cint,
            (Ref{fqPolyRepPolyRingElem}, Ref{fqPolyRepPolyRingElem},
             Ref{fqPolyRepPolyRingElem}, Ref{fqPolyRepField}),
             q, z, x, base_ring(parent(z))))
   return v, q
end

################################################################################
#
#   Modular arithmetic
#
################################################################################

function powermod(x::fqPolyRepPolyRingElem, n::Int, y::fqPolyRepPolyRingElem)
   check_parent(x,y)
   z = parent(x)()

   if n < 0
      g, x = gcdinv(x, y)
      if !isone(g)
         error("Element not invertible")
      end
      n = -n
   end

   ccall((:fq_nmod_poly_powmod_ui_binexp, libflint), Nothing,
         (Ref{fqPolyRepPolyRingElem}, Ref{fqPolyRepPolyRingElem}, Int, Ref{fqPolyRepPolyRingElem},
         Ref{fqPolyRepField}), z, x, n, y, base_ring(parent(x)))
  return z
end

function powermod(x::fqPolyRepPolyRingElem, n::ZZRingElem, y::fqPolyRepPolyRingElem)
   check_parent(x,y)
   z = parent(x)()

   if n < 0
      g, x = gcdinv(x, y)
      if !isone(g)
         error("Element not invertible")
      end
      n = -n
   end

   ccall((:fq_nmod_poly_powmod_fmpz_binexp, libflint), Nothing,
         (Ref{fqPolyRepPolyRingElem}, Ref{fqPolyRepPolyRingElem}, Ref{ZZRingElem}, Ref{fqPolyRepPolyRingElem},
         Ref{fqPolyRepField}), z, x, n, y, base_ring(parent(x)))
  return z
end

################################################################################
#
#   GCD
#
################################################################################

function gcd(x::fqPolyRepPolyRingElem, y::fqPolyRepPolyRingElem)
   check_parent(x,y)
   z = parent(x)()
   ccall((:fq_nmod_poly_gcd, libflint), Nothing,
         (Ref{fqPolyRepPolyRingElem}, Ref{fqPolyRepPolyRingElem}, Ref{fqPolyRepPolyRingElem},
         Ref{fqPolyRepField}), z, x, y, base_ring(parent(x)))
   return z
end

function gcdinv(x::fqPolyRepPolyRingElem, y::fqPolyRepPolyRingElem)
   check_parent(x,y)
   z = parent(x)()
   s = parent(x)()
   t = parent(x)()
   ccall((:fq_nmod_poly_xgcd, libflint), Nothing,
         (Ref{fqPolyRepPolyRingElem}, Ref{fqPolyRepPolyRingElem}, Ref{fqPolyRepPolyRingElem},
          Ref{fqPolyRepPolyRingElem}, Ref{fqPolyRepPolyRingElem},
           Ref{fqPolyRepField}), z, s, t, x, y, base_ring(parent(x)))
   return z, s
end

function gcdx(x::fqPolyRepPolyRingElem, y::fqPolyRepPolyRingElem)
   check_parent(x,y)
   z = parent(x)()
   s = parent(x)()
   t = parent(x)()
   ccall((:fq_nmod_poly_xgcd, libflint), Nothing,
         (Ref{fqPolyRepPolyRingElem}, Ref{fqPolyRepPolyRingElem}, Ref{fqPolyRepPolyRingElem},
          Ref{fqPolyRepPolyRingElem}, Ref{fqPolyRepPolyRingElem},
           Ref{fqPolyRepField}), z, s, t, x, y, base_ring(parent(x)))
   return z, s, t
end

################################################################################
#
#   Evaluation
#
################################################################################

function evaluate(x::fqPolyRepPolyRingElem, y::fqPolyRepFieldElem)
   base_ring(parent(x)) != parent(y) && error("Incompatible coefficient rings")
   z = parent(y)()
   ccall((:fq_nmod_poly_evaluate_fq_nmod, libflint), Nothing,
         (Ref{fqPolyRepFieldElem}, Ref{fqPolyRepPolyRingElem}, Ref{fqPolyRepFieldElem},
         Ref{fqPolyRepField}), z, x, y, base_ring(parent(x)))
   return z
end

################################################################################
#
#   Composition
#
################################################################################

function compose(x::fqPolyRepPolyRingElem, y::fqPolyRepPolyRingElem)
   check_parent(x,y)
   z = parent(x)()
   ccall((:fq_nmod_poly_compose, libflint), Nothing,
         (Ref{fqPolyRepPolyRingElem}, Ref{fqPolyRepPolyRingElem}, Ref{fqPolyRepPolyRingElem},
         Ref{fqPolyRepField}), z, x, y, base_ring(parent(x)))
   return z
end

################################################################################
#
#   Derivative
#
################################################################################

function derivative(x::fqPolyRepPolyRingElem)
   z = parent(x)()
   ccall((:fq_nmod_poly_derivative, libflint), Nothing,
         (Ref{fqPolyRepPolyRingElem}, Ref{fqPolyRepPolyRingElem}, Ref{fqPolyRepField}),
         z, x, base_ring(parent(x)))
   return z
end

################################################################################
#
#  Inflation and deflation
#
################################################################################

function inflate(x::fqPolyRepPolyRingElem, n::Int)
   z = parent(x)()
   ccall((:fq_nmod_poly_inflate, libflint), Nothing, (Ref{fqPolyRepPolyRingElem},
         Ref{fqPolyRepPolyRingElem}, Culong, Ref{fqPolyRepField}),
         z, x, UInt(n), base_ring(parent(x)))
   return z
end

function deflate(x::fqPolyRepPolyRingElem, n::Int)
   z = parent(x)()
   ccall((:fq_nmod_poly_deflate, libflint), Nothing,
         (Ref{fqPolyRepPolyRingElem}, Ref{fqPolyRepPolyRingElem}, Culong, Ref{fqPolyRepField}),
         z, x, UInt(n), base_ring(parent(x)))
  return z
end

################################################################################
#
#  Irreducibility
#
################################################################################

function is_irreducible(x::fqPolyRepPolyRingElem)
  return Bool(ccall((:fq_nmod_poly_is_irreducible, libflint), Int32,
                    (Ref{fqPolyRepPolyRingElem}, Ref{fqPolyRepField} ),
                    x, base_ring(parent(x))))
end

################################################################################
#
#  Squarefree testing
#
################################################################################

function is_squarefree(x::fqPolyRepPolyRingElem)
   return Bool(ccall((:fq_nmod_poly_is_squarefree, libflint), Int32,
       (Ref{fqPolyRepPolyRingElem}, Ref{fqPolyRepField}), x, base_ring(parent(x))))
end

################################################################################
#
#  Factorization
#
################################################################################

function factor(x::fqPolyRepPolyRingElem)
   res, z = _factor(x)
   return Fac(parent(x)(z), res)
end

function _factor(x::fqPolyRepPolyRingElem)
   R = parent(x)
   F = base_ring(R)
   a = F()
   fac = fq_nmod_poly_factor(F)
   ccall((:fq_nmod_poly_factor, libflint), Nothing, (Ref{fq_nmod_poly_factor},
         Ref{fqPolyRepFieldElem}, Ref{fqPolyRepPolyRingElem}, Ref{fqPolyRepField}),
         fac, a, x, F)
   res = Dict{fqPolyRepPolyRingElem,Int}()
   for i in 1:fac.num
      f = R()
      ccall((:fq_nmod_poly_factor_get_poly, libflint), Nothing,
            (Ref{fqPolyRepPolyRingElem}, Ref{fq_nmod_poly_factor}, Int,
            Ref{fqPolyRepField}), f, fac, i-1, F)
      e = unsafe_load(fac.exp,i)
      res[f] = e
   end
   return res, a
end

function factor_squarefree(x::fqPolyRepPolyRingElem)
  # _factor_squareefree does weird things if the polynomial is not monic
  return Fac(parent(x)(leading_coefficient(x)),
	     _factor_squarefree(divexact(x, leading_coefficient(x))))
end

function _factor_squarefree(x::fqPolyRepPolyRingElem)
  F = base_ring(parent(x))
  fac = fq_nmod_poly_factor(F)
  ccall((:fq_nmod_poly_factor_squarefree, libflint), UInt,
        (Ref{fq_nmod_poly_factor}, Ref{fqPolyRepPolyRingElem}, Ref{fqPolyRepField}), fac, x, F)
  res = Dict{fqPolyRepPolyRingElem,Int}()
  for i in 1:fac.num
    f = parent(x)()
    ccall((:fq_nmod_poly_factor_get_poly, libflint), Nothing,
          (Ref{fqPolyRepPolyRingElem}, Ref{fq_nmod_poly_factor}, Int,
           Ref{fqPolyRepField}), f, fac, i-1, F)
    e = unsafe_load(fac.exp, i)
    res[f] = e
  end
  return res
end

@doc raw"""
    factor_distinct_deg(x::fqPolyRepPolyRingElem)

Return the distinct degree factorisation of a squarefree polynomial $x$.
"""
function factor_distinct_deg(x::fqPolyRepPolyRingElem)
   R = parent(x)
   F = base_ring(R)
   fac = fq_nmod_poly_factor(F)
   degrees = Vector{Int}(undef, degree(x))
   ccall((:fq_nmod_poly_factor_distinct_deg, libflint), Nothing,
         (Ref{fq_nmod_poly_factor}, Ref{fqPolyRepPolyRingElem}, Ref{Vector{Int}},
         Ref{fqPolyRepField}), fac, x, degrees, F)
   res = Dict{Int, fqPolyRepPolyRingElem}()
   for i in 1:fac.num
      f = R()
      ccall((:fq_nmod_poly_factor_get_poly, libflint), Nothing,
            (Ref{fqPolyRepPolyRingElem}, Ref{fq_nmod_poly_factor}, Int,
            Ref{fqPolyRepField}), f, fac, i-1, F)
      res[degrees[i]] = f
   end
   return res
end

function roots(x::fqPolyRepPolyRingElem)
   R = parent(x)
   F = base_ring(R)
   fac = fq_nmod_poly_factor(F)
   ccall((:fq_nmod_poly_roots, libflint), Nothing,
         (Ref{fq_nmod_poly_factor}, Ref{fqPolyRepPolyRingElem}, Cint,
         Ref{fqPolyRepField}), fac, x, 0, F)
   res = fqPolyRepFieldElem[]
   for i in 1:fac.num
      f = R()
      ccall((:fq_nmod_poly_factor_get_poly, libflint), Nothing,
            (Ref{fqPolyRepPolyRingElem}, Ref{fq_nmod_poly_factor}, Int,
            Ref{fqPolyRepField}), f, fac, i-1, F)
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

function zero!(z::fqPolyRepPolyRingElem)
   ccall((:fq_nmod_poly_zero, libflint), Nothing,
         (Ref{fqPolyRepPolyRingElem}, Ref{fqPolyRepField}),
         z, base_ring(parent(z)))
   return z
end

function fit!(z::fqPolyRepPolyRingElem, n::Int)
   ccall((:fq_nmod_poly_fit_length, libflint), Nothing,
         (Ref{fqPolyRepPolyRingElem}, Int, Ref{fqPolyRepField}),
         z, n, base_ring(parent(z)))
   return nothing
end

function setcoeff!(z::fqPolyRepPolyRingElem, n::Int, x::fqPolyRepFieldElem)
   ccall((:fq_nmod_poly_set_coeff, libflint), Nothing,
         (Ref{fqPolyRepPolyRingElem}, Int, Ref{fqPolyRepFieldElem}, Ref{fqPolyRepField}),
         z, n, x, base_ring(parent(z)))
   return z
end

function mul!(z::fqPolyRepPolyRingElem, x::fqPolyRepPolyRingElem, y::fqPolyRepPolyRingElem)
   ccall((:fq_nmod_poly_mul, libflint), Nothing,
         (Ref{fqPolyRepPolyRingElem}, Ref{fqPolyRepPolyRingElem}, Ref{fqPolyRepPolyRingElem},
         Ref{fqPolyRepField}), z, x, y, base_ring(parent(x)))
   return z
end

function add!(z::fqPolyRepPolyRingElem, x::fqPolyRepPolyRingElem, y::fqPolyRepPolyRingElem)
   ccall((:fq_nmod_poly_add, libflint), Nothing,
         (Ref{fqPolyRepPolyRingElem}, Ref{fqPolyRepPolyRingElem}, Ref{fqPolyRepPolyRingElem},
         Ref{fqPolyRepField}), z, x, y, base_ring(parent(x)))
   return z
end

function sub!(z::fqPolyRepPolyRingElem, x::fqPolyRepPolyRingElem, y::fqPolyRepPolyRingElem)
   ccall((:fq_nmod_poly_sub, libflint), Nothing,
         (Ref{fqPolyRepPolyRingElem}, Ref{fqPolyRepPolyRingElem}, Ref{fqPolyRepPolyRingElem},
         Ref{fqPolyRepField}), z, x, y, base_ring(parent(x)))
   return z
end


function addeq!(z::fqPolyRepPolyRingElem, x::fqPolyRepPolyRingElem)
   ccall((:fq_nmod_poly_add, libflint), Nothing,
         (Ref{fqPolyRepPolyRingElem}, Ref{fqPolyRepPolyRingElem}, Ref{fqPolyRepPolyRingElem},
         Ref{fqPolyRepField}), z, z, x, base_ring(parent(x)))
   return z
end

################################################################################
#
#  Promotion rules
#
################################################################################

promote_rule(::Type{fqPolyRepPolyRingElem}, ::Type{V}) where {V <: Integer} = fqPolyRepPolyRingElem

promote_rule(::Type{fqPolyRepPolyRingElem}, ::Type{ZZRingElem}) = fqPolyRepPolyRingElem

promote_rule(::Type{fqPolyRepPolyRingElem}, ::Type{fqPolyRepFieldElem}) = fqPolyRepPolyRingElem

###############################################################################
#
#   Polynomial substitution
#
###############################################################################

function (f::fqPolyRepPolyRingElem)(a::fqPolyRepFieldElem)
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

function (R::fqPolyRepPolyRing)()
   z = fqPolyRepPolyRingElem()
   z.parent = R
   return z
end

function (R::fqPolyRepPolyRing)(x::fqPolyRepFieldElem)
  z = fqPolyRepPolyRingElem(x)
  z.parent = R
  return z
end

function (R::fqPolyRepPolyRing)(x::ZZRingElem)
   return R(base_ring(R)(x))
end

function (R::fqPolyRepPolyRing)(x::Integer)
   return R(ZZRingElem(x))
end

function (R::fqPolyRepPolyRing)(x::Vector{fqPolyRepFieldElem})
   length(x) == 0 && return zero(R)
   base_ring(R) != parent(x[1]) && error("Coefficient rings must coincide")
   z = fqPolyRepPolyRingElem(x)
   z.parent = R
   return z
end

function (R::fqPolyRepPolyRing)(x::Vector{ZZRingElem})
   length(x) == 0 && return zero(R)
   z = fqPolyRepPolyRingElem(x, base_ring(R))
   z.parent = R
   return z
end

function (R::fqPolyRepPolyRing)(x::Vector{T}) where {T <: Integer}
   length(x) == 0 && return zero(R)
   return R(map(ZZRingElem, x))
end

function (R::fqPolyRepPolyRing)(x::ZZPolyRingElem)
   z = fqPolyRepPolyRingElem(x, base_ring(R))
   z.parent = R
   return z
end

function (R::fqPolyRepPolyRing)(x::fqPolyRepPolyRingElem)
  parent(x) != R && error("Unable to coerce to polynomial")
  return x
end
