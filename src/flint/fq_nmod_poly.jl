################################################################################
#
#  fq_nmod_poly.jl: Flint fq_mod_poly (Polynomials over FqNmodFiniteField)
#
################################################################################

export fq_nmod_poly, FqNmodPolyRing

################################################################################
#
#  Type and parent object methods
#
################################################################################

parent_type(::Type{fq_nmod_poly}) = FqNmodPolyRing

elem_type(::Type{FqNmodPolyRing}) = fq_nmod_poly

base_ring(a::FqNmodPolyRing) = a.base_ring

parent(a::fq_nmod_poly) = a.parent

var(a::FqNmodPolyRing) = a.S

function check_parent(a::fq_nmod_poly, b::fq_nmod_poly)
   a.parent != b.parent &&
         error("Operations on distinct polynomial rings not supported")
end

################################################################################
#
#   Basic manipulation
#
################################################################################

length(x::fq_nmod_poly) = ccall((:fq_nmod_poly_length, libflint), Int,
                                (Ref{fq_nmod_poly},), x)

function set_length!(x::fq_nmod_poly, n::Int)
   ccall((:_fq_nmod_poly_set_length, libflint), Nothing,
                              (Ref{fq_nmod_poly}, Int), x, n)
   return x
end

function coeff(x::fq_nmod_poly, n::Int)
   n < 0 && throw(DomainError(n, "Index must be non-negative"))
   F = (x.parent).base_ring
   temp = F(1)
   ccall((:fq_nmod_poly_get_coeff, libflint), Nothing,
         (Ref{fq_nmod}, Ref{fq_nmod_poly}, Int, Ref{FqNmodFiniteField}),
         temp, x, n, F)
   return temp
end

zero(a::FqNmodPolyRing) = a(zero(base_ring(a)))

one(a::FqNmodPolyRing) = a(one(base_ring(a)))

gen(a::FqNmodPolyRing) = a([zero(base_ring(a)), one(base_ring(a))])

iszero(x::fq_nmod_poly) = ccall((:fq_nmod_poly_is_zero, libflint), Bool,
                              (Ref{fq_nmod_poly}, Ref{FqNmodFiniteField}),
                              x, base_ring(x.parent))

isone(x::fq_nmod_poly) = ccall((:fq_nmod_poly_is_one, libflint), Bool,
                              (Ref{fq_nmod_poly}, Ref{FqNmodFiniteField}),
                              x, base_ring(x.parent))

isgen(x::fq_nmod_poly) = ccall((:fq_nmod_poly_is_gen, libflint), Bool,
                              (Ref{fq_nmod_poly}, Ref{FqNmodFiniteField}),
                              x, base_ring(x.parent))

degree(f::fq_nmod_poly) = f.length - 1

function deepcopy_internal(a::fq_nmod_poly, dict::IdDict)
   z = fq_nmod_poly(a)
   z.parent = a.parent
   return z
end

characteristic(R::FqNmodPolyRing) = characteristic(base_ring(R))

################################################################################
#
#   Canonicalisation
#
################################################################################

canonical_unit(a::fq_nmod_poly) = canonical_unit(lead(a))

################################################################################
#
#  AbstractString I/O
#
################################################################################

function show(io::IO, x::fq_nmod_poly)
   if length(x) == 0
      print(io, "0")
   else
      cstr = ccall((:fq_nmod_poly_get_str_pretty, libflint), Ptr{UInt8},
                  (Ref{fq_nmod_poly}, Ptr{UInt8}, Ref{FqNmodFiniteField}),
                  x, string(var(parent(x))),
                  (x.parent).base_ring)
      print(io, unsafe_string(cstr))
      ccall((:flint_free, libflint), Nothing, (Ptr{UInt8},), cstr)
   end
end

function show(io::IO, R::FqNmodPolyRing)
  print(io, "Univariate Polynomial Ring in ")
  print(io, string(var(R)))
  print(io, " over ")
  show(io, base_ring(R))
end

show_minus_one(::Type{fq_nmod_poly}) = show_minus_one(fq_nmod)

################################################################################
#
#  Unary operations
#
################################################################################

function -(x::fq_nmod_poly)
   z = parent(x)()
   ccall((:fq_nmod_poly_neg, libflint), Nothing,
         (Ref{fq_nmod_poly}, Ref{fq_nmod_poly}, Ref{FqNmodFiniteField}),
         z, x, base_ring(parent(x)))
   return z
end

################################################################################
#
#  Binary operations
#
################################################################################

function +(x::fq_nmod_poly, y::fq_nmod_poly)
   check_parent(x,y)
   z = parent(x)()
   ccall((:fq_nmod_poly_add, libflint), Nothing,
         (Ref{fq_nmod_poly}, Ref{fq_nmod_poly},
         Ref{fq_nmod_poly}, Ref{FqNmodFiniteField}),
         z, x, y, base_ring(parent(x)))
   return z
end

function -(x::fq_nmod_poly, y::fq_nmod_poly)
   check_parent(x,y)
   z = parent(x)()
   ccall((:fq_nmod_poly_sub, libflint), Nothing,
         (Ref{fq_nmod_poly}, Ref{fq_nmod_poly},
         Ref{fq_nmod_poly}, Ref{FqNmodFiniteField}),
         z, x, y, base_ring(parent(x)))
   return z
end

function *(x::fq_nmod_poly, y::fq_nmod_poly)
   check_parent(x,y)
   z = parent(x)()
   ccall((:fq_nmod_poly_mul, libflint), Nothing,
         (Ref{fq_nmod_poly}, Ref{fq_nmod_poly},
         Ref{fq_nmod_poly}, Ref{FqNmodFiniteField}),
         z, x, y, base_ring(parent(x)))
   return z
end

################################################################################
#
#   Ad hoc binary operators
#
################################################################################

function *(x::fq_nmod, y::fq_nmod_poly)
   parent(x) != base_ring(parent(y)) &&
         error("Coefficient rings must be equal")
   z = parent(y)()
   ccall((:fq_nmod_poly_scalar_mul_fq_nmod, libflint), Nothing,
         (Ref{fq_nmod_poly}, Ref{fq_nmod_poly},
         Ref{fq_nmod}, Ref{FqNmodFiniteField}),
         z, y, x, parent(x))
  return z
end

*(x::fq_nmod_poly, y::fq_nmod) = y*x

*(x::fmpz, y::fq_nmod_poly) = base_ring(parent(y))(x) * y

*(x::fq_nmod_poly, y::fmpz) = y*x

*(x::Integer, y::fq_nmod_poly) = fmpz(x)*y

*(x::fq_nmod_poly, y::Integer) = y*x

+(x::fq_nmod, y::fq_nmod_poly) = parent(y)(x) + y

+(x::fq_nmod_poly, y::fq_nmod) = y + x

+(x::fmpz, y::fq_nmod_poly) = base_ring(parent(y))(x) + y

+(x::fq_nmod_poly, y::fmpz) = y + x

+(x::fq_nmod_poly, y::Integer) = x + fmpz(y)

+(x::Integer, y::fq_nmod_poly) = y + x

-(x::fq_nmod, y::fq_nmod_poly) = parent(y)(x) - y

-(x::fq_nmod_poly, y::fq_nmod) = x - parent(x)(y)

-(x::fmpz, y::fq_nmod_poly) = base_ring(parent(y))(x) - y

-(x::fq_nmod_poly, y::fmpz) = x - base_ring(parent(x))(y)

-(x::fq_nmod_poly, y::Integer) = x - fmpz(y)

-(x::Integer, y::fq_nmod_poly) = fmpz(x) - y

################################################################################
#
#   Powering
#
################################################################################

function ^(x::fq_nmod_poly, y::Int)
   y < 0 && throw(DomainError(y, "Exponent must be non-negative"))
   z = parent(x)()
   ccall((:fq_nmod_poly_pow, libflint), Nothing,
         (Ref{fq_nmod_poly}, Ref{fq_nmod_poly}, Int, Ref{FqNmodFiniteField}),
         z, x, y, base_ring(parent(x)))
   return z
end

################################################################################
#
#   Comparisons
#
################################################################################

function ==(x::fq_nmod_poly, y::fq_nmod_poly)
   check_parent(x,y)
   r = ccall((:fq_nmod_poly_equal, libflint), Cint,
             (Ref{fq_nmod_poly}, Ref{fq_nmod_poly}, Ref{FqNmodFiniteField}),
             x, y, base_ring(parent(x)))
   return Bool(r)
end

################################################################################
#
#   Ad hoc comparisons
#
################################################################################

function ==(x::fq_nmod_poly, y::fq_nmod)
   base_ring(parent(x)) != parent(y) && return false
   if length(x) > 1
      return false
   elseif length(x) == 1
      r = ccall((:fq_nmod_poly_equal_fq_nmod, libflint), Cint,
                (Ref{fq_nmod_poly}, Ref{fq_nmod}, Ref{FqNmodFiniteField}),
                x, y, base_ring(parent(x)))
      return Bool(r)
   else
      return iszero(y)
  end
end

==(x::fq_nmod, y::fq_nmod_poly) = y == x

==(x::fq_nmod_poly, y::fmpz) = x == base_ring(parent(x))(y)

==(x::fmpz, y::fq_nmod_poly) = y == x

==(x::fq_nmod_poly, y::Integer) = x == fmpz(y)

==(x::Integer, y::fq_nmod_poly) = y == x

################################################################################
#
#   Truncation
#
################################################################################

function truncate(x::fq_nmod_poly, n::Int)
   n < 0 && throw(DomainError(n, "Index must be non-negative"))
   if length(x) <= n
      return x
   end
   z = parent(x)()
   ccall((:fq_nmod_poly_set_trunc, libflint), Nothing,
         (Ref{fq_nmod_poly}, Ref{fq_nmod_poly}, Int, Ref{FqNmodFiniteField}),
         z, x, n, base_ring(parent(x)))
   return z
end

function mullow(x::fq_nmod_poly, y::fq_nmod_poly, n::Int)
   check_parent(x,y)
   n < 0 && throw(DomainError(n, "Index must be non-negative"))
   z = parent(x)()
   ccall((:fq_nmod_poly_mullow, libflint), Nothing,
         (Ref{fq_nmod_poly}, Ref{fq_nmod_poly}, Ref{fq_nmod_poly},
         Int, Ref{FqNmodFiniteField}),
         z, x, y, n, base_ring(parent(x)))
   return z
end

################################################################################
#
#   Reversal
#
################################################################################

function reverse(x::fq_nmod_poly, len::Int)
   len < 0 && throw(DomainError(len, "Index must be non-negative"))
   z = parent(x)()
   ccall((:fq_nmod_poly_reverse, libflint), Nothing,
         (Ref{fq_nmod_poly}, Ref{fq_nmod_poly}, Int, Ref{FqNmodFiniteField}),
         z, x, len, base_ring(parent(x)))
   return z
end

################################################################################
#
#   Shifting
#
################################################################################

function shift_left(x::fq_nmod_poly, len::Int)
   len < 0 && throw(DomainError(len, "Shift must be non-negative"))
   z = parent(x)()
   ccall((:fq_nmod_poly_shift_left, libflint), Nothing,
         (Ref{fq_nmod_poly}, Ref{fq_nmod_poly}, Int, Ref{FqNmodFiniteField}),
         z, x, len, base_ring(parent(x)))
   return z
end

function shift_right(x::fq_nmod_poly, len::Int)
   len < 0 && throw(DomainError(len, "Shift must be non-negative"))
   z = parent(x)()
   ccall((:fq_nmod_poly_shift_right, libflint), Nothing,
         (Ref{fq_nmod_poly}, Ref{fq_nmod_poly}, Int, Ref{FqNmodFiniteField}),
         z, x, len, base_ring(parent(x)))
   return z
end

################################################################################
#
#   Euclidean division
#
################################################################################

function div(x::fq_nmod_poly, y::fq_nmod_poly)
   check_parent(x,y)
   z = parent(x)()
   ccall((:fq_nmod_poly_div_basecase, libflint), Nothing,
         (Ref{fq_nmod_poly}, Ref{fq_nmod_poly}, Ref{fq_nmod_poly},
         Ref{FqNmodFiniteField}), z, x, y, base_ring(parent(x)))
  return z
end

function rem(x::fq_nmod_poly, y::fq_nmod_poly)
   check_parent(x,y)
   z = parent(x)()
   ccall((:fq_nmod_poly_rem, libflint), Nothing,
         (Ref{fq_nmod_poly}, Ref{fq_nmod_poly}, Ref{fq_nmod_poly},
         Ref{FqNmodFiniteField}), z, x, y, base_ring(parent(x)))
  return z
end

mod(x::fq_nmod_poly, y::fq_nmod_poly) = rem(x, y)

function divrem(x::fq_nmod_poly, y::fq_nmod_poly)
   check_parent(x,y)
   z = parent(x)()
   r = parent(x)()
   ccall((:fq_nmod_poly_divrem, libflint), Nothing, (Ref{fq_nmod_poly},
         Ref{fq_nmod_poly}, Ref{fq_nmod_poly}, Ref{fq_nmod_poly},
         Ref{FqNmodFiniteField}), z, r, x, y, base_ring(parent(x)))
   return z,r
end

################################################################################
#
#   Remove and valuation
#
################################################################################

@doc Markdown.doc"""
    remove(z::fq_nmod_poly, p::fq_nmod_poly)
> Computes the valuation of $z$ at $p$, that is, the largest $k$ such that
> $p^k$ divides $z$. Additionally, $z/p^k$ is returned as well.
>
> See also `valuation`, which only returns the valuation.
"""
function remove(z::fq_nmod_poly, p::fq_nmod_poly)
   check_parent(z,p)
   iszero(z) && error("Not yet implemented")
   z = deepcopy(z)
   v = ccall((:fq_nmod_poly_remove, libflint), Int,
            (Ref{fq_nmod_poly}, Ref{fq_nmod_poly}, Ref{FqNmodFiniteField}),
             z,  p, base_ring(parent(z)))
   return v, z
end

function divides(z::fq_nmod_poly, x::fq_nmod_poly)
   check_parent(z, x)
   if iszero(z)
      return true, zero(parent(z))
   end
   if iszero(x)
      return false, zero(parent(z))
   end
   q = parent(z)()
   v = Bool(ccall((:fq_nmod_poly_divides, libflint), Cint,
            (Ref{fq_nmod_poly}, Ref{fq_nmod_poly},
             Ref{fq_nmod_poly}, Ref{FqNmodFiniteField}),
             q, z, x, base_ring(parent(z))))
   return v, q
end

################################################################################
#
#   Modular arithmetic
#
################################################################################

function powmod(x::fq_nmod_poly, n::Int, y::fq_nmod_poly)
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
         (Ref{fq_nmod_poly}, Ref{fq_nmod_poly}, Int, Ref{fq_nmod_poly},
         Ref{FqNmodFiniteField}), z, x, n, y, base_ring(parent(x)))
  return z
end

function powmod(x::fq_nmod_poly, n::fmpz, y::fq_nmod_poly)
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
         (Ref{fq_nmod_poly}, Ref{fq_nmod_poly}, Ref{fmpz}, Ref{fq_nmod_poly},
         Ref{FqNmodFiniteField}), z, x, n, y, base_ring(parent(x)))
  return z
end

################################################################################
#
#   GCD
#
################################################################################

function gcd(x::fq_nmod_poly, y::fq_nmod_poly)
   check_parent(x,y)
   z = parent(x)()
   ccall((:fq_nmod_poly_gcd, libflint), Nothing,
         (Ref{fq_nmod_poly}, Ref{fq_nmod_poly}, Ref{fq_nmod_poly},
         Ref{FqNmodFiniteField}), z, x, y, base_ring(parent(x)))
   return z
end

function gcdinv(x::fq_nmod_poly, y::fq_nmod_poly)
   check_parent(x,y)
   z = parent(x)()
   s = parent(x)()
   t = parent(x)()
   ccall((:fq_nmod_poly_xgcd, libflint), Nothing,
         (Ref{fq_nmod_poly}, Ref{fq_nmod_poly}, Ref{fq_nmod_poly},
          Ref{fq_nmod_poly}, Ref{fq_nmod_poly},
           Ref{FqNmodFiniteField}), z, s, t, x, y, base_ring(parent(x)))
   return z, s
end

function gcdx(x::fq_nmod_poly, y::fq_nmod_poly)
   check_parent(x,y)
   z = parent(x)()
   s = parent(x)()
   t = parent(x)()
   ccall((:fq_nmod_poly_xgcd, libflint), Nothing,
         (Ref{fq_nmod_poly}, Ref{fq_nmod_poly}, Ref{fq_nmod_poly},
          Ref{fq_nmod_poly}, Ref{fq_nmod_poly},
           Ref{FqNmodFiniteField}), z, s, t, x, y, base_ring(parent(x)))
   return z, s, t
end

################################################################################
#
#   Evaluation
#
################################################################################

function evaluate(x::fq_nmod_poly, y::fq_nmod)
   base_ring(parent(x)) != parent(y) && error("Incompatible coefficient rings")
   z = parent(y)()
   ccall((:fq_nmod_poly_evaluate_fq_nmod, libflint), Nothing,
         (Ref{fq_nmod}, Ref{fq_nmod_poly}, Ref{fq_nmod},
         Ref{FqNmodFiniteField}), z, x, y, base_ring(parent(x)))
   return z
end

################################################################################
#
#   Composition
#
################################################################################

function compose(x::fq_nmod_poly, y::fq_nmod_poly)
   check_parent(x,y)
   z = parent(x)()
   ccall((:fq_nmod_poly_compose, libflint), Nothing,
         (Ref{fq_nmod_poly}, Ref{fq_nmod_poly}, Ref{fq_nmod_poly},
         Ref{FqNmodFiniteField}), z, x, y, base_ring(parent(x)))
   return z
end

################################################################################
#
#   Derivative
#
################################################################################

function derivative(x::fq_nmod_poly)
   z = parent(x)()
   ccall((:fq_nmod_poly_derivative, libflint), Nothing,
         (Ref{fq_nmod_poly}, Ref{fq_nmod_poly}, Ref{FqNmodFiniteField}),
         z, x, base_ring(parent(x)))
   return z
end

################################################################################
#
#  Inflation and deflation
#
################################################################################

function inflate(x::fq_nmod_poly, n::Int)
   z = parent(x)()
   ccall((:fq_nmod_poly_inflate, libflint), Nothing, (Ref{fq_nmod_poly},
         Ref{fq_nmod_poly}, Culong, Ref{FqNmodFiniteField}),
         z, x, UInt(n), base_ring(parent(x)))
   return z
end

function deflate(x::fq_nmod_poly, n::Int)
   z = parent(x)()
   ccall((:fq_nmod_poly_deflate, libflint), Nothing,
         (Ref{fq_nmod_poly}, Ref{fq_nmod_poly}, Culong, Ref{FqNmodFiniteField}),
         z, x, UInt(n), base_ring(parent(x)))
  return z
end

################################################################################
#
#  Irreducibility
#
################################################################################

@doc Markdown.doc"""
    isirreducible(x::fq_nmod_poly)
> Return `true` if $x$ is irreducible, otherwise return `false`.
"""
function isirreducible(x::fq_nmod_poly)
  return Bool(ccall((:fq_nmod_poly_is_irreducible, libflint), Int32,
                    (Ref{fq_nmod_poly}, Ref{FqNmodFiniteField} ),
                    x, base_ring(parent(x))))
end

################################################################################
#
#  Squarefree testing
#
################################################################################

@doc Markdown.doc"""
    issquarefree(x::fq_nmod_poly)
> Return `true` if $x$ is squarefree, otherwise return `false`.
"""
function issquarefree(x::fq_nmod_poly)
   return Bool(ccall((:fq_nmod_poly_is_squarefree, libflint), Int32,
       (Ref{fq_nmod_poly}, Ref{FqNmodFiniteField}), x, base_ring(parent(x))))
end

################################################################################
#
#  Factorization
#
################################################################################

@doc Markdown.doc"""
    factor(x::fq_nmod_poly)
> Return the factorisation of $x$.
"""
function factor(x::fq_nmod_poly)
   res, z = _factor(x)
   return Fac(parent(x)(z), res)
end

function _factor(x::fq_nmod_poly)
   R = parent(x)
   F = base_ring(R)
   a = F()
   fac = fq_nmod_poly_factor(F)
   ccall((:fq_nmod_poly_factor, libflint), Nothing, (Ref{fq_nmod_poly_factor},
         Ref{fq_nmod}, Ref{fq_nmod_poly}, Ref{FqNmodFiniteField}),
         fac, a, x, F)
   res = Dict{fq_nmod_poly,Int}()
   for i in 1:fac.num
      f = R()
      ccall((:fq_nmod_poly_factor_get_poly, libflint), Nothing,
            (Ref{fq_nmod_poly}, Ref{fq_nmod_poly_factor}, Int,
            Ref{FqNmodFiniteField}), f, fac, i-1, F)
      e = unsafe_load(fac.exp,i)
      res[f] = e
   end
   return res, a
end

@doc Markdown.doc"""
    factor_squarefree(x::fq_nmod_poly)
> Return the squarefree factorisation of $x$.
"""
function factor_squarefree(x::fq_nmod_poly)
  # _factor_squareefree does weird things if the polynomial is not monic
  return Fac(parent(x)(lead(x)), _factor_squarefree(divexact(x, lead(x))))
end

function _factor_squarefree(x::fq_nmod_poly)
  F = base_ring(parent(x))
  fac = fq_nmod_poly_factor(F)
  ccall((:fq_nmod_poly_factor_squarefree, libflint), UInt,
        (Ref{fq_nmod_poly_factor}, Ref{fq_nmod_poly}, Ref{FqNmodFiniteField}), fac, x, F)
  res = Dict{fq_nmod_poly,Int}()
  for i in 1:fac.num
    f = parent(x)()
    ccall((:fq_nmod_poly_factor_get_poly, libflint), Nothing,
          (Ref{fq_nmod_poly}, Ref{fq_nmod_poly_factor}, Int,
           Ref{FqNmodFiniteField}), f, fac, i-1, F)
    e = unsafe_load(fac.exp, i)
    res[f] = e
  end
  return res
end

@doc Markdown.doc"""
    factor_distinct_deg(x::fq_nmod_poly)
> Return the distinct degree factorisation of a squarefree polynomial $x$.
"""
function factor_distinct_deg(x::fq_nmod_poly)
   R = parent(x)
   F = base_ring(R)
   fac = fq_nmod_poly_factor(F)
   degrees = Vector{Int}(undef, degree(x))
   ccall((:fq_nmod_poly_factor_distinct_deg, libflint), Nothing,
         (Ref{fq_nmod_poly_factor}, Ref{fq_nmod_poly}, Ref{Vector{Int}},
         Ref{FqNmodFiniteField}), fac, x, degrees, F)
   res = Dict{Int, fq_nmod_poly}()
   for i in 1:fac.num
      f = R()
      ccall((:fq_nmod_poly_factor_get_poly, libflint), Nothing,
            (Ref{fq_nmod_poly}, Ref{fq_nmod_poly_factor}, Int,
            Ref{FqNmodFiniteField}), f, fac, i-1, F)
      res[degrees[i]] = f
   end
   return res
end

################################################################################
#
#   Unsafe functions
#
################################################################################

function zero!(z::fq_nmod_poly)
   ccall((:fq_nmod_poly_zero, libflint), Nothing,
         (Ref{fq_nmod_poly}, Ref{FqNmodFiniteField}),
         z, base_ring(parent(z)))
   return z
end

function fit!(z::fq_nmod_poly, n::Int)
   ccall((:fq_nmod_poly_fit_length, libflint), Nothing,
         (Ref{fq_nmod_poly}, Int, Ref{FqNmodFiniteField}),
         z, n, base_ring(parent(z)))
   return nothing
end

function setcoeff!(z::fq_nmod_poly, n::Int, x::fq_nmod)
   ccall((:fq_nmod_poly_set_coeff, libflint), Nothing,
         (Ref{fq_nmod_poly}, Int, Ref{fq_nmod}, Ref{FqNmodFiniteField}),
         z, n, x, base_ring(parent(z)))
   return z
end

function mul!(z::fq_nmod_poly, x::fq_nmod_poly, y::fq_nmod_poly)
   ccall((:fq_nmod_poly_mul, libflint), Nothing,
         (Ref{fq_nmod_poly}, Ref{fq_nmod_poly}, Ref{fq_nmod_poly},
         Ref{FqNmodFiniteField}), z, x, y, base_ring(parent(x)))
   return z
end

function add!(z::fq_nmod_poly, x::fq_nmod_poly, y::fq_nmod_poly)
   ccall((:fq_nmod_poly_add, libflint), Nothing,
         (Ref{fq_nmod_poly}, Ref{fq_nmod_poly}, Ref{fq_nmod_poly},
         Ref{FqNmodFiniteField}), z, x, y, base_ring(parent(x)))
   return z
end

function sub!(z::fq_nmod_poly, x::fq_nmod_poly, y::fq_nmod_poly)
   ccall((:fq_nmod_poly_sub, libflint), Nothing,
         (Ref{fq_nmod_poly}, Ref{fq_nmod_poly}, Ref{fq_nmod_poly},
         Ref{FqNmodFiniteField}), z, x, y, base_ring(parent(x)))
   return z
end


function addeq!(z::fq_nmod_poly, x::fq_nmod_poly)
   ccall((:fq_nmod_poly_add, libflint), Nothing,
         (Ref{fq_nmod_poly}, Ref{fq_nmod_poly}, Ref{fq_nmod_poly},
         Ref{FqNmodFiniteField}), z, z, x, base_ring(parent(x)))
   return z
end

################################################################################
#
#  Promotion rules
#
################################################################################

promote_rule(::Type{fq_nmod_poly}, ::Type{V}) where {V <: Integer} = fq_nmod_poly

promote_rule(::Type{fq_nmod_poly}, ::Type{fmpz}) = fq_nmod_poly

promote_rule(::Type{fq_nmod_poly}, ::Type{fq_nmod}) = fq_nmod_poly

###############################################################################
#
#   Polynomial substitution
#
###############################################################################

function (f::fq_nmod_poly)(a::fq_nmod)
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

function (R::FqNmodPolyRing)()
   z = fq_nmod_poly()
   z.parent = R
   return z
end

function (R::FqNmodPolyRing)(x::fq_nmod)
  z = fq_nmod_poly(x)
  z.parent = R
  return z
end

function (R::FqNmodPolyRing)(x::fmpz)
   return R(base_ring(R)(x))
end

function (R::FqNmodPolyRing)(x::Integer)
   return R(fmpz(x))
end

function (R::FqNmodPolyRing)(x::Array{fq_nmod, 1})
   length(x) == 0 && error("Array must be non-empty")
   base_ring(R) != parent(x[1]) && error("Coefficient rings must coincide")
   z = fq_nmod_poly(x)
   z.parent = R
   return z
end

function (R::FqNmodPolyRing)(x::Array{fmpz, 1})
   length(x) == 0 && error("Array must be non-empty")
   z = fq_nmod_poly(x, base_ring(R))
   z.parent = R
   return z
end

function (R::FqNmodPolyRing)(x::Array{T, 1}) where {T <: Integer}
   length(x) == 0 && error("Array must be non-empty")
   return R(map(fmpz, x))
end

function (R::FqNmodPolyRing)(x::fmpz_poly)
   z = fq_nmod_poly(x, base_ring(R))
   z.parent = R
   return z
end

function (R::FqNmodPolyRing)(x::fq_nmod_poly)
  parent(x) != R && error("Unable to coerce to polynomial")
  return x
end

################################################################################
#
#   PolynomialRing constructor
#
################################################################################

function PolynomialRing(R::FqNmodFiniteField, s::AbstractString; cached = true)
   S = Symbol(s)
   parent_obj = FqNmodPolyRing(R, S, cached)
   return parent_obj, parent_obj([R(0), R(1)])
end
