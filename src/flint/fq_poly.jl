################################################################################
#
#  fq_poly.jl: Flint fq_poly (Polynomials over FqFiniteField)
#
################################################################################

export FqPolyRepPolyRingElem, FqPolyRepPolyRing

################################################################################
#
#  Type and parent object methods
#
################################################################################

parent_type(::Type{FqPolyRepPolyRingElem}) = FqPolyRepPolyRing

elem_type(::Type{FqPolyRepPolyRing}) = FqPolyRepPolyRingElem

dense_poly_type(::Type{FqPolyRepFieldElem}) = FqPolyRepPolyRingElem

base_ring(a::FqPolyRepPolyRing) = a.base_ring

parent(a::FqPolyRepPolyRingElem) = a.parent

var(a::FqPolyRepPolyRing) = a.S

function check_parent(a::FqPolyRepPolyRingElem, b::FqPolyRepPolyRingElem)
   a.parent != b.parent &&
         error("Operations on distinct polynomial rings not supported")
end

################################################################################
#
#   Basic manipulation
#
################################################################################

length(x::FqPolyRepPolyRingElem) = ccall((:fq_poly_length, libflint), Int,
                                (Ref{FqPolyRepPolyRingElem},), x)

function coeff(x::FqPolyRepPolyRingElem, n::Int)
   n < 0 && throw(DomainError(n, "Index must be non-negative"))
   F = (x.parent).base_ring
   temp = F(1)
   ccall((:fq_poly_get_coeff, libflint), Nothing,
         (Ref{FqPolyRepFieldElem}, Ref{FqPolyRepPolyRingElem}, Int, Ref{FqPolyRepField}),
         temp, x, n, F)
   return temp
end

function set_length!(x::FqPolyRepPolyRingElem, n::Int)
   ccall((:_fq_poly_set_length, libflint), Nothing,
                              (Ref{FqPolyRepPolyRingElem}, Int), x, n)
   return x
end

zero(a::FqPolyRepPolyRing) = a(zero(base_ring(a)))

one(a::FqPolyRepPolyRing) = a(one(base_ring(a)))

gen(a::FqPolyRepPolyRing) = a([zero(base_ring(a)), one(base_ring(a))])

is_gen(x::FqPolyRepPolyRingElem) = ccall((:fq_poly_is_gen, libflint), Bool,
                              (Ref{FqPolyRepPolyRingElem}, Ref{FqPolyRepField}),
                              x, base_ring(x.parent))

iszero(x::FqPolyRepPolyRingElem) = ccall((:fq_poly_is_zero, libflint), Bool,
                              (Ref{FqPolyRepPolyRingElem}, Ref{FqPolyRepField}),
                              x, base_ring(x.parent))

isone(x::FqPolyRepPolyRingElem) = ccall((:fq_poly_is_one, libflint), Bool,
                              (Ref{FqPolyRepPolyRingElem}, Ref{FqPolyRepField}),
                              x, base_ring(x.parent))

degree(f::FqPolyRepPolyRingElem) = f.length - 1

function deepcopy_internal(a::FqPolyRepPolyRingElem, dict::IdDict)
   z = FqPolyRepPolyRingElem(a)
   z.parent = a.parent
   return z
end

characteristic(R::FqPolyRepPolyRing) = characteristic(base_ring(R))

###############################################################################
#
#   Similar
#
###############################################################################

function similar(f::PolyElem, R::FqPolyRepField, s::Symbol=var(parent(f)); cached::Bool=true)
   z = FqPolyRepPolyRingElem()
   if base_ring(f) === R && s == var(parent(f)) && typeof(f) == FqPolyRepPolyRingElem
      # steal parent in case it is not cached
      z.parent = parent(f)
   else
      z.parent = FqPolyRepPolyRing(R, s, cached)
   end
   return z
end

###############################################################################
#
#   polynomial constructor
#
###############################################################################

function polynomial(R::FqPolyRepField, arr::Vector{T}, var::String="x"; cached::Bool=true) where T
   coeffs = map(R, arr)
   z = length(coeffs) == 0 ? FqPolyRepPolyRingElem() : FqPolyRepPolyRingElem(coeffs)
   z.parent = FqPolyRepPolyRing(R, Symbol(var), cached)
   return z
end

################################################################################
#
#   Canonicalisation
#
################################################################################

canonical_unit(a::FqPolyRepPolyRingElem) = canonical_unit(leading_coefficient(a))

################################################################################
#
#  String I/O
#
################################################################################

function show(io::IO, R::FqPolyRepPolyRing)
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

function -(x::FqPolyRepPolyRingElem)
   z = parent(x)()
   ccall((:fq_poly_neg, libflint), Nothing,
         (Ref{FqPolyRepPolyRingElem}, Ref{FqPolyRepPolyRingElem}, Ref{FqPolyRepField}),
         z, x, base_ring(parent(x)))
   return z
end

################################################################################
#
#  Binary operations
#
################################################################################

function +(x::FqPolyRepPolyRingElem, y::FqPolyRepPolyRingElem)
   check_parent(x,y)
   z = parent(x)()
   ccall((:fq_poly_add, libflint), Nothing,
         (Ref{FqPolyRepPolyRingElem}, Ref{FqPolyRepPolyRingElem},
         Ref{FqPolyRepPolyRingElem}, Ref{FqPolyRepField}),
         z, x, y, base_ring(parent(x)))
   return z
end

function -(x::FqPolyRepPolyRingElem, y::FqPolyRepPolyRingElem)
   check_parent(x,y)
   z = parent(x)()
   ccall((:fq_poly_sub, libflint), Nothing,
         (Ref{FqPolyRepPolyRingElem}, Ref{FqPolyRepPolyRingElem},
         Ref{FqPolyRepPolyRingElem}, Ref{FqPolyRepField}),
         z, x, y, base_ring(parent(x)))
   return z
end

function *(x::FqPolyRepPolyRingElem, y::FqPolyRepPolyRingElem)
   check_parent(x,y)
   z = parent(x)()
   ccall((:fq_poly_mul, libflint), Nothing,
         (Ref{FqPolyRepPolyRingElem}, Ref{FqPolyRepPolyRingElem},
         Ref{FqPolyRepPolyRingElem}, Ref{FqPolyRepField}),
         z, x, y, base_ring(parent(x)))
   return z
end

################################################################################
#
#   Ad hoc binary operators
#
################################################################################

function *(x::FqPolyRepFieldElem, y::FqPolyRepPolyRingElem)
   parent(x) != base_ring(parent(y)) &&
         error("Coefficient rings must be equal")
   z = parent(y)()
   ccall((:fq_poly_scalar_mul_fq, libflint), Nothing,
         (Ref{FqPolyRepPolyRingElem}, Ref{FqPolyRepPolyRingElem},
         Ref{FqPolyRepFieldElem}, Ref{FqPolyRepField}),
         z, y, x, parent(x))
  return z
end

*(x::FqPolyRepPolyRingElem, y::FqPolyRepFieldElem) = y*x

*(x::ZZRingElem, y::FqPolyRepPolyRingElem) = base_ring(parent(y))(x) * y

*(x::FqPolyRepPolyRingElem, y::ZZRingElem) = y*x

*(x::Integer, y::FqPolyRepPolyRingElem) = ZZRingElem(x)*y

*(x::FqPolyRepPolyRingElem, y::Integer) = y*x

+(x::FqPolyRepFieldElem, y::FqPolyRepPolyRingElem) = parent(y)(x) + y

+(x::FqPolyRepPolyRingElem, y::FqPolyRepFieldElem) = y + x

+(x::ZZRingElem, y::FqPolyRepPolyRingElem) = base_ring(parent(y))(x) + y

+(x::FqPolyRepPolyRingElem, y::ZZRingElem) = y + x

+(x::FqPolyRepPolyRingElem, y::Integer) = x + ZZRingElem(y)

+(x::Integer, y::FqPolyRepPolyRingElem) = y + x

-(x::FqPolyRepFieldElem, y::FqPolyRepPolyRingElem) = parent(y)(x) - y

-(x::FqPolyRepPolyRingElem, y::FqPolyRepFieldElem) = x - parent(x)(y)

-(x::ZZRingElem, y::FqPolyRepPolyRingElem) = base_ring(parent(y))(x) - y

-(x::FqPolyRepPolyRingElem, y::ZZRingElem) = x - base_ring(parent(x))(y)

-(x::FqPolyRepPolyRingElem, y::Integer) = x - ZZRingElem(y)

-(x::Integer, y::FqPolyRepPolyRingElem) = ZZRingElem(x) - y

################################################################################
#
#   Powering
#
################################################################################

function ^(x::FqPolyRepPolyRingElem, y::Int)
   y < 0 && throw(DomainError(y, "Exponent must be non-negative"))
   z = parent(x)()
   ccall((:fq_poly_pow, libflint), Nothing,
         (Ref{FqPolyRepPolyRingElem}, Ref{FqPolyRepPolyRingElem}, Int, Ref{FqPolyRepField}),
         z, x, y, base_ring(parent(x)))
   return z
end

################################################################################
#
#   Comparisons
#
################################################################################

function ==(x::FqPolyRepPolyRingElem, y::FqPolyRepPolyRingElem)
   check_parent(x,y)
   r = ccall((:fq_poly_equal, libflint), Cint,
             (Ref{FqPolyRepPolyRingElem}, Ref{FqPolyRepPolyRingElem}, Ref{FqPolyRepField}),
             x, y, base_ring(parent(x)))
   return Bool(r)
end

################################################################################
#
#   Ad hoc comparisons
#
################################################################################

function ==(x::FqPolyRepPolyRingElem, y::FqPolyRepFieldElem)
   base_ring(parent(x)) != parent(y) && return false
   if length(x) > 1
      return false
   elseif length(x) == 1
      r = ccall((:fq_poly_equal_fq, libflint), Cint,
                (Ref{FqPolyRepPolyRingElem}, Ref{FqPolyRepFieldElem}, Ref{FqPolyRepField}),
                x, y, base_ring(parent(x)))
      return Bool(r)
   else
      return iszero(y)
  end
end

==(x::FqPolyRepFieldElem, y::FqPolyRepPolyRingElem) = y == x

==(x::FqPolyRepPolyRingElem, y::ZZRingElem) = x == base_ring(parent(x))(y)

==(x::ZZRingElem, y::FqPolyRepPolyRingElem) = y == x

==(x::FqPolyRepPolyRingElem, y::Integer) = x == ZZRingElem(y)

==(x::Integer, y::FqPolyRepPolyRingElem) = y == x

################################################################################
#
#   Truncation
#
################################################################################

function truncate(x::FqPolyRepPolyRingElem, n::Int)
   n < 0 && throw(DomainError(n, "Index must be non-negative"))
   if length(x) <= n
      return x
   end
   z = parent(x)()
   ccall((:fq_poly_set_trunc, libflint), Nothing,
         (Ref{FqPolyRepPolyRingElem}, Ref{FqPolyRepPolyRingElem}, Int, Ref{FqPolyRepField}),
         z, x, n, base_ring(parent(x)))
   return z
end

function mullow(x::FqPolyRepPolyRingElem, y::FqPolyRepPolyRingElem, n::Int)
   check_parent(x,y)
   n < 0 && throw(DomainError(n, "Index must be non-negative"))
   z = parent(x)()
   ccall((:fq_poly_mullow, libflint), Nothing,
         (Ref{FqPolyRepPolyRingElem}, Ref{FqPolyRepPolyRingElem}, Ref{FqPolyRepPolyRingElem},
         Int, Ref{FqPolyRepField}),
         z, x, y, n, base_ring(parent(x)))
   return z
end

################################################################################
#
#   Reversal
#
################################################################################

function reverse(x::FqPolyRepPolyRingElem, len::Int)
   len < 0 && throw(DomainError(len, "Index must be non-negative"))
   z = parent(x)()
   ccall((:fq_poly_reverse, libflint), Nothing,
         (Ref{FqPolyRepPolyRingElem}, Ref{FqPolyRepPolyRingElem}, Int, Ref{FqPolyRepField}),
         z, x, len, base_ring(parent(x)))
   return z
end

################################################################################
#
#   Shifting
#
################################################################################

function shift_left(x::FqPolyRepPolyRingElem, len::Int)
   len < 0 && throw(DomainError(len, "Shift must be non-negative"))
   z = parent(x)()
   ccall((:fq_poly_shift_left, libflint), Nothing,
         (Ref{FqPolyRepPolyRingElem}, Ref{FqPolyRepPolyRingElem}, Int, Ref{FqPolyRepField}),
         z, x, len, base_ring(parent(x)))
   return z
end

function shift_right(x::FqPolyRepPolyRingElem, len::Int)
   len < 0 && throw(DomainError(len, "Shift must be non-negative"))
   z = parent(x)()
   ccall((:fq_poly_shift_right, libflint), Nothing,
         (Ref{FqPolyRepPolyRingElem}, Ref{FqPolyRepPolyRingElem}, Int, Ref{FqPolyRepField}),
         z, x, len, base_ring(parent(x)))
   return z
end

################################################################################
#
#   Euclidean division
#
################################################################################

function Base.div(x::FqPolyRepPolyRingElem, y::FqPolyRepPolyRingElem)
   check_parent(x,y)
   z = parent(x)()
   ccall((:fq_poly_div_basecase, libflint), Nothing,
         (Ref{FqPolyRepPolyRingElem}, Ref{FqPolyRepPolyRingElem}, Ref{FqPolyRepPolyRingElem},
         Ref{FqPolyRepField}), z, x, y, base_ring(parent(x)))
  return z
end

function rem(x::FqPolyRepPolyRingElem, y::FqPolyRepPolyRingElem)
   check_parent(x,y)
   z = parent(x)()
   ccall((:fq_poly_rem, libflint), Nothing,
         (Ref{FqPolyRepPolyRingElem}, Ref{FqPolyRepPolyRingElem}, Ref{FqPolyRepPolyRingElem},
         Ref{FqPolyRepField}), z, x, y, base_ring(parent(x)))
  return z
end

mod(x::FqPolyRepPolyRingElem, y::FqPolyRepPolyRingElem) = rem(x, y)

function Base.divrem(x::FqPolyRepPolyRingElem, y::FqPolyRepPolyRingElem)
   check_parent(x,y)
   z = parent(x)()
   r = parent(x)()
   ccall((:fq_poly_divrem, libflint), Nothing, (Ref{FqPolyRepPolyRingElem},
         Ref{FqPolyRepPolyRingElem}, Ref{FqPolyRepPolyRingElem}, Ref{FqPolyRepPolyRingElem},
         Ref{FqPolyRepField}), z, r, x, y, base_ring(parent(x)))
   return z, r
end

################################################################################
#
#  Square root
#
################################################################################

function sqrt(x::FqPolyRepPolyRingElem; check::Bool=true)
   R = parent(x)
   s = R()
   flag = Bool(ccall((:fq_poly_sqrt, libflint), Cint,
                     (Ref{FqPolyRepPolyRingElem}, Ref{FqPolyRepPolyRingElem}, Ref{FqPolyRepField}),
                      s, x, base_ring(parent(x))))
   check && !flag && error("Not a square in sqrt")
   return s
end

function issquare(x::FqPolyRepPolyRingElem)
   if iszero(x)
      return true
   end
   if !iseven(degree(x))
      return false
   end
   R = parent(x)
   s = R()
   flag = Bool(ccall((:fq_poly_sqrt, libflint), Cint,
                     (Ref{FqPolyRepPolyRingElem}, Ref{FqPolyRepPolyRingElem}, Ref{FqPolyRepField}),
                      s, x, base_ring(parent(x))))
   return flag
end

function issquare_with_sqrt(x::FqPolyRepPolyRingElem)
   R = parent(x)
   if iszero(x)
      return true, zero(R)
   end
   if !iseven(degree(x))
      return false, zero(R)
   end
   s = R()
   flag = Bool(ccall((:fq_poly_sqrt, libflint), Cint,
                     (Ref{FqPolyRepPolyRingElem}, Ref{FqPolyRepPolyRingElem}, Ref{FqPolyRepField}),
                      s, x, base_ring(parent(x))))
   return flag, s
end

################################################################################
#
#   Remove and valuation
#
################################################################################

function remove(z::FqPolyRepPolyRingElem, p::FqPolyRepPolyRingElem)
   ok, v = _remove_check_simple_cases(z, p)
   ok && return v, zero(parent(z))
   z = deepcopy(z)
   v = ccall((:fq_poly_remove, libflint), Int,
            (Ref{FqPolyRepPolyRingElem}, Ref{FqPolyRepPolyRingElem}, Ref{FqPolyRepField}),
             z,  p, base_ring(parent(z)))
   return v, z
end

function divides(z::FqPolyRepPolyRingElem, x::FqPolyRepPolyRingElem)
   if iszero(z)
      return true, zero(parent(z))
   end
   if iszero(x)
      return false, zero(parent(z))
   end
   check_parent(z, x)
   q = parent(z)()
   v = Bool(ccall((:fq_poly_divides, libflint), Cint,
            (Ref{FqPolyRepPolyRingElem}, Ref{FqPolyRepPolyRingElem},
             Ref{FqPolyRepPolyRingElem}, Ref{FqPolyRepField}),
             q, z, x, base_ring(parent(z))))
   return v, q
end

################################################################################
#
#   Modular arithmetic
#
################################################################################

function powermod(x::FqPolyRepPolyRingElem, n::Int, y::FqPolyRepPolyRingElem)
   check_parent(x,y)
   z = parent(x)()

   if n < 0
      g, x = gcdinv(x, y)
      if !isone(g)
         error("Element not invertible")
      end
      n = -n
   end

   ccall((:fq_poly_powmod_ui_binexp, libflint), Nothing,
         (Ref{FqPolyRepPolyRingElem}, Ref{FqPolyRepPolyRingElem}, Int, Ref{FqPolyRepPolyRingElem},
         Ref{FqPolyRepField}), z, x, n, y, base_ring(parent(x)))
  return z
end

function powermod(x::FqPolyRepPolyRingElem, n::ZZRingElem, y::FqPolyRepPolyRingElem)
   check_parent(x,y)
   z = parent(x)()

   if n < 0
      g, x = gcdinv(x, y)
      if !isone(g)
         error("Element not invertible")
      end
      n = -n
   end

   ccall((:fq_poly_powmod_fmpz_binexp, libflint), Nothing,
         (Ref{FqPolyRepPolyRingElem}, Ref{FqPolyRepPolyRingElem}, Ref{ZZRingElem}, Ref{FqPolyRepPolyRingElem},
         Ref{FqPolyRepField}), z, x, n, y, base_ring(parent(x)))
  return z
end

################################################################################
#
#   GCD
#
################################################################################

function gcd(x::FqPolyRepPolyRingElem, y::FqPolyRepPolyRingElem)
   check_parent(x,y)
   z = parent(x)()
   ccall((:fq_poly_gcd, libflint), Nothing,
         (Ref{FqPolyRepPolyRingElem}, Ref{FqPolyRepPolyRingElem}, Ref{FqPolyRepPolyRingElem},
         Ref{FqPolyRepField}), z, x, y, base_ring(parent(x)))
   return z
end

function gcdinv(x::FqPolyRepPolyRingElem, y::FqPolyRepPolyRingElem)
   check_parent(x,y)
   z = parent(x)()
   s = parent(x)()
   t = parent(x)()
   ccall((:fq_poly_xgcd, libflint), Nothing,
         (Ref{FqPolyRepPolyRingElem}, Ref{FqPolyRepPolyRingElem}, Ref{FqPolyRepPolyRingElem}, Ref{FqPolyRepPolyRingElem}, Ref{FqPolyRepPolyRingElem},
         Ref{FqPolyRepField}), z, s, t, x, y, base_ring(parent(x)))
   return z, s
end

function gcdx(x::FqPolyRepPolyRingElem, y::FqPolyRepPolyRingElem)
   check_parent(x,y)
   z = parent(x)()
   s = parent(x)()
   t = parent(x)()
   ccall((:fq_poly_xgcd, libflint), Nothing,
         (Ref{FqPolyRepPolyRingElem}, Ref{FqPolyRepPolyRingElem}, Ref{FqPolyRepPolyRingElem}, Ref{FqPolyRepPolyRingElem}, Ref{FqPolyRepPolyRingElem},
         Ref{FqPolyRepField}), z, s, t, x, y, base_ring(parent(x)))
   return z, s, t
end

################################################################################
#
#   Evaluation
#
################################################################################

function evaluate(x::FqPolyRepPolyRingElem, y::FqPolyRepFieldElem)
   base_ring(parent(x)) != parent(y) && error("Incompatible coefficient rings")
   z = parent(y)()
   ccall((:fq_poly_evaluate_fq, libflint), Nothing,
         (Ref{FqPolyRepFieldElem}, Ref{FqPolyRepPolyRingElem}, Ref{FqPolyRepFieldElem},
         Ref{FqPolyRepField}), z, x, y, base_ring(parent(x)))
   return z
end

################################################################################
#
#   Composition
#
################################################################################

function compose(x::FqPolyRepPolyRingElem, y::FqPolyRepPolyRingElem)
   check_parent(x,y)
   z = parent(x)()
   ccall((:fq_poly_compose, libflint), Nothing,
         (Ref{FqPolyRepPolyRingElem}, Ref{FqPolyRepPolyRingElem}, Ref{FqPolyRepPolyRingElem},
         Ref{FqPolyRepField}), z, x, y, base_ring(parent(x)))
   return z
end

################################################################################
#
#   Derivative
#
################################################################################

function derivative(x::FqPolyRepPolyRingElem)
   z = parent(x)()
   ccall((:fq_poly_derivative, libflint), Nothing,
         (Ref{FqPolyRepPolyRingElem}, Ref{FqPolyRepPolyRingElem}, Ref{FqPolyRepField}),
         z, x, base_ring(parent(x)))
   return z
end

################################################################################
#
#  Inflation and deflation
#
################################################################################

function inflate(x::FqPolyRepPolyRingElem, n::Int)
   z = parent(x)()
   ccall((:fq_poly_inflate, libflint), Nothing, (Ref{FqPolyRepPolyRingElem},
         Ref{FqPolyRepPolyRingElem}, Culong, Ref{FqPolyRepField}),
         z, x, UInt(n), base_ring(parent(x)))
   return z
end

function deflate(x::FqPolyRepPolyRingElem, n::Int)
   z = parent(x)()
   ccall((:fq_poly_deflate, libflint), Nothing,
         (Ref{FqPolyRepPolyRingElem}, Ref{FqPolyRepPolyRingElem}, Culong, Ref{FqPolyRepField}),
         z, x, UInt(n), base_ring(parent(x)))
  return z
end

################################################################################
#
#  Irreducibility
#
################################################################################

function is_irreducible(x::FqPolyRepPolyRingElem)
  return Bool(ccall((:fq_poly_is_irreducible, libflint), Int32,
                    (Ref{FqPolyRepPolyRingElem}, Ref{FqPolyRepField} ),
                    x, base_ring(parent(x))))
end

################################################################################
#
#  Squarefree testing
#
################################################################################

function is_squarefree(x::FqPolyRepPolyRingElem)
   return Bool(ccall((:fq_poly_is_squarefree, libflint), Int32,
       (Ref{FqPolyRepPolyRingElem}, Ref{FqPolyRepField}), x, base_ring(parent(x))))
end

################################################################################
#
#  Factorization
#
################################################################################

function factor(x::FqPolyRepPolyRingElem)
   fac, z = _factor(x)
   return Fac(parent(x)(z), fac)
end

function _factor(x::FqPolyRepPolyRingElem)
   R = parent(x)
   F = base_ring(R)
   a = F()
   fac = fq_poly_factor(F)
   ccall((:fq_poly_factor, libflint), Nothing, (Ref{fq_poly_factor},
         Ref{FqPolyRepFieldElem}, Ref{FqPolyRepPolyRingElem}, Ref{FqPolyRepField}),
         fac, a, x, F)
   res = Dict{FqPolyRepPolyRingElem,Int}()
   for i in 1:fac.num
      f = R()
      ccall((:fq_poly_factor_get_poly, libflint), Nothing,
            (Ref{FqPolyRepPolyRingElem}, Ref{fq_poly_factor}, Int,
            Ref{FqPolyRepField}), f, fac, i-1, F)
      e = unsafe_load(fac.exp,i)
      res[f] = e
   end
   return res, a
end

function factor_squarefree(x::FqPolyRepPolyRingElem)
  # _factor_squareefree does weird things if the polynomial is not monic
  return Fac(parent(x)(leading_coefficient(x)),
	      _factor_squarefree(divexact(x, leading_coefficient(x))))
end

function _factor_squarefree(x::FqPolyRepPolyRingElem)
  F = base_ring(parent(x))
  fac = fq_poly_factor(F)
  ccall((:fq_poly_factor_squarefree, libflint), UInt,
        (Ref{fq_poly_factor}, Ref{FqPolyRepPolyRingElem}, Ref{FqPolyRepField}), fac, x, F)
  res = Dict{FqPolyRepPolyRingElem,Int}()
  for i in 1:fac.num
    f = parent(x)()
    ccall((:fq_poly_factor_get_poly, libflint), Nothing,
          (Ref{FqPolyRepPolyRingElem}, Ref{fq_poly_factor}, Int,
          Ref{FqPolyRepField}), f, fac, i-1, F)
    e = unsafe_load(fac.exp, i)
    res[f] = e
  end
  return res
end

@doc Markdown.doc"""
    factor_distinct_deg(x::FqPolyRepPolyRingElem)

Return the distinct degree factorisation of a squarefree polynomial $x$.
"""
function factor_distinct_deg(x::FqPolyRepPolyRingElem)
   R = parent(x)
   F = base_ring(R)
   fac = fq_poly_factor(F)
   degrees = Vector{Int}(undef, degree(x))
   ccall((:fq_poly_factor_distinct_deg, libflint), Nothing,
         (Ref{fq_poly_factor}, Ref{FqPolyRepPolyRingElem}, Ref{Vector{Int}},
         Ref{FqPolyRepField}), fac, x, degrees, F)
   res = Dict{Int, FqPolyRepPolyRingElem}()
   for i in 1:fac.num
      f = R()
      ccall((:fq_poly_factor_get_poly, libflint), Nothing,
            (Ref{FqPolyRepPolyRingElem}, Ref{fq_poly_factor}, Int,
            Ref{FqPolyRepField}), f, fac, i-1, F)
      res[degrees[i]] = f
   end
   return res
end

function roots(x::FqPolyRepPolyRingElem)
   R = parent(x)
   F = base_ring(R)
   fac = fq_poly_factor(F)
   ccall((:fq_poly_roots, libflint), Nothing,
         (Ref{fq_poly_factor}, Ref{FqPolyRepPolyRingElem}, Cint,
         Ref{FqPolyRepField}), fac, x, 0, F)
   res = FqPolyRepFieldElem[]
   for i in 1:fac.num
      f = R()
      ccall((:fq_poly_factor_get_poly, libflint), Nothing,
            (Ref{FqPolyRepPolyRingElem}, Ref{fq_poly_factor}, Int,
            Ref{FqPolyRepField}), f, fac, i-1, F)
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

function zero!(z::FqPolyRepPolyRingElem)
   ccall((:fq_poly_zero, libflint), Nothing,
         (Ref{FqPolyRepPolyRingElem}, Ref{FqPolyRepField}),
         z, base_ring(parent(z)))
   return z
end

function fit!(z::FqPolyRepPolyRingElem, n::Int)
   ccall((:fq_poly_fit_length, libflint), Nothing,
         (Ref{FqPolyRepPolyRingElem}, Int, Ref{FqPolyRepField}),
         z, n, base_ring(parent(z)))
   return nothing
end

function setcoeff!(z::FqPolyRepPolyRingElem, n::Int, x::FqPolyRepFieldElem)
   ccall((:fq_poly_set_coeff, libflint), Nothing,
         (Ref{FqPolyRepPolyRingElem}, Int, Ref{FqPolyRepFieldElem}, Ref{FqPolyRepField}),
         z, n, x, base_ring(parent(z)))
   return z
end

function mul!(z::FqPolyRepPolyRingElem, x::FqPolyRepPolyRingElem, y::FqPolyRepPolyRingElem)
   ccall((:fq_poly_mul, libflint), Nothing,
         (Ref{FqPolyRepPolyRingElem}, Ref{FqPolyRepPolyRingElem}, Ref{FqPolyRepPolyRingElem},
         Ref{FqPolyRepField}), z, x, y, base_ring(parent(x)))
   return z
end

function add!(z::FqPolyRepPolyRingElem, x::FqPolyRepPolyRingElem, y::FqPolyRepPolyRingElem)
   ccall((:fq_poly_add, libflint), Nothing,
         (Ref{FqPolyRepPolyRingElem}, Ref{FqPolyRepPolyRingElem}, Ref{FqPolyRepPolyRingElem},
         Ref{FqPolyRepField}), z, x, y, base_ring(parent(x)))
   return z
end

function sub!(z::FqPolyRepPolyRingElem, x::FqPolyRepPolyRingElem, y::FqPolyRepPolyRingElem)
   ccall((:fq_poly_sub, libflint), Nothing,
         (Ref{FqPolyRepPolyRingElem}, Ref{FqPolyRepPolyRingElem}, Ref{FqPolyRepPolyRingElem},
         Ref{FqPolyRepField}), z, x, y, base_ring(parent(x)))
   return z
end


function addeq!(z::FqPolyRepPolyRingElem, x::FqPolyRepPolyRingElem)
   ccall((:fq_poly_add, libflint), Nothing,
         (Ref{FqPolyRepPolyRingElem}, Ref{FqPolyRepPolyRingElem}, Ref{FqPolyRepPolyRingElem},
         Ref{FqPolyRepField}), z, z, x, base_ring(parent(x)))
   return z
end

################################################################################
#
#  Promotion rules
#
################################################################################

promote_rule(::Type{FqPolyRepPolyRingElem}, ::Type{V}) where {V <: Integer} = FqPolyRepPolyRingElem

promote_rule(::Type{FqPolyRepPolyRingElem}, ::Type{ZZRingElem}) = FqPolyRepPolyRingElem

promote_rule(::Type{FqPolyRepPolyRingElem}, ::Type{FqPolyRepFieldElem}) = FqPolyRepPolyRingElem

###############################################################################
#
#   Polynomial substitution
#
###############################################################################

function (f::FqPolyRepPolyRingElem)(a::FqPolyRepFieldElem)
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

function (R::FqPolyRepPolyRing)()
   z = FqPolyRepPolyRingElem()
   z.parent = R
   return z
end

function (R::FqPolyRepPolyRing)(x::FqPolyRepFieldElem)
  z = FqPolyRepPolyRingElem(x)
  z.parent = R
  return z
end

function (R::FqPolyRepPolyRing)(x::ZZRingElem)
   return R(base_ring(R)(x))
end

function (R::FqPolyRepPolyRing)(x::Integer)
   return R(ZZRingElem(x))
end

function (R::FqPolyRepPolyRing)(x::Vector{FqPolyRepFieldElem})
   length(x) == 0 && return zero(R)
   base_ring(R) != parent(x[1]) && error("Coefficient rings must coincide")
   z = FqPolyRepPolyRingElem(x)
   z.parent = R
   return z
end

function (R::FqPolyRepPolyRing)(x::Vector{ZZRingElem})
   length(x) == 0 && return zero(R)
   z = FqPolyRepPolyRingElem(x, base_ring(R))
   z.parent = R
   return z
end

function (R::FqPolyRepPolyRing)(x::Vector{T}) where {T <: Integer}
   length(x) == 0 && return zero(R)
   return R(map(ZZRingElem, x))
end

function (R::FqPolyRepPolyRing)(x::ZZPolyRingElem)
   z = FqPolyRepPolyRingElem(x, base_ring(R))
   z.parent = R
   return z
end

function (R::FqPolyRepPolyRing)(x::FqPolyRepPolyRingElem)
  parent(x) != R && error("Unable to coerce to polynomial")
  return x
end

################################################################################
#
#   PolynomialRing constructor
#
################################################################################

function PolynomialRing(R::FqPolyRepField, s::Symbol; cached = true)
   parent_obj = FqPolyRepPolyRing(R, s, cached)
   return parent_obj, parent_obj([R(0), R(1)])
end

function PolynomialRing(R::FqPolyRepField, s::AbstractString; cached = true)
   return PolynomialRing(R, Symbol(s); cached=cached)
end

function PolyRing(R::FqPolyRepField)
   return FqPolyRepPolyRing(R, :x, false)
end
