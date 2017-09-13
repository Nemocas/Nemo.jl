################################################################################
#
#  fq_poly.jl: Flint fq_poly (Polynomials over FqFiniteField)
#
################################################################################

export fq_poly, FqPolyRing

################################################################################
#
#  Type and parent object methods
#
################################################################################

parent_type(::Type{fq_poly}) = FqPolyRing

elem_type(::Type{FqPolyRing}) = fq_poly

base_ring(a::FqPolyRing) = a.base_ring

parent(a::fq_poly) = a.parent

var(a::FqPolyRing) = a.S

function check_parent(a::fq_poly, b::fq_poly) 
   a.parent != b.parent &&
         error("Operations on distinct polynomial rings not supported")
end

################################################################################
#
#   Basic manipulation
#
################################################################################
   
length(x::fq_poly) = ccall((:fq_poly_length, :libflint), Int,
                                (Ptr{fq_poly},), &x)

function coeff(x::fq_poly, n::Int)
   n < 0 && throw(DomainError())
   F = (x.parent).base_ring
   temp = F(1)
   ccall((:fq_poly_get_coeff, :libflint), Void, 
         (Ptr{fq}, Ptr{fq_poly}, Int, Ptr{FqFiniteField}),
         &temp, &x, n, &F)
   return temp
end

set_length!(x::fq_poly, n::Int) = ccall((:_fq_poly_set_length, :libflint), Void,
                              (Ptr{fq_poly}, Int), &x, n)

zero(a::FqPolyRing) = a(zero(base_ring(a)))

one(a::FqPolyRing) = a(one(base_ring(a)))

gen(a::FqPolyRing) = a([zero(base_ring(a)), one(base_ring(a))])

isgen(x::fq_poly) = ccall((:fq_poly_is_gen, :libflint), Bool,
                              (Ptr{fq_poly}, Ptr{FqFiniteField}),
                              &x, &base_ring(x.parent))

iszero(x::fq_poly) = ccall((:fq_poly_is_zero, :libflint), Bool,
                              (Ptr{fq_poly}, Ptr{FqFiniteField}),
                              &x, &base_ring(x.parent))

isone(x::fq_poly) = ccall((:fq_poly_is_one, :libflint), Bool,
                              (Ptr{fq_poly}, Ptr{FqFiniteField}),
                              &x, &base_ring(x.parent))

degree(f::fq_poly) = f.length - 1

function deepcopy_internal(a::fq_poly, dict::ObjectIdDict)
   z = fq_poly(a)
   z.parent = a.parent
   return z
end

################################################################################
#
#   Canonicalisation
#
################################################################################

canonical_unit(a::fq_poly) = canonical_unit(lead(a))
  
################################################################################
#
#  String I/O
#
################################################################################

function show(io::IO, x::fq_poly)
   if length(x) == 0
      print(io, "0")
   else
      cstr = ccall((:fq_poly_get_str_pretty, :libflint), Ptr{UInt8}, 
                  (Ptr{fq_poly}, Ptr{UInt8}, Ptr{FqFiniteField}),
                  &x, string(var(parent(x))),
                  &((x.parent).base_ring))
      print(io, unsafe_string(cstr))
      ccall((:flint_free, :libflint), Void, (Ptr{UInt8},), cstr)
   end
end

function show(io::IO, R::FqPolyRing)
   print(io, "Univariate Polynomial Ring in ")
   print(io, string(var(R)))
   print(io, " over ")
   show(io, base_ring(R))
end

show_minus_one(::Type{fq_poly}) = show_minus_one(fq)

################################################################################
#
#  Unary operations
#
################################################################################

function -(x::fq_poly)
   z = parent(x)()
   ccall((:fq_poly_neg, :libflint), Void,
         (Ptr{fq_poly}, Ptr{fq_poly}, Ptr{FqFiniteField}),
         &z, &x, &base_ring(parent(x)))
   return z
end

################################################################################
#
#  Binary operations
#
################################################################################

function +(x::fq_poly, y::fq_poly)
   check_parent(x,y)
   z = parent(x)()
   ccall((:fq_poly_add, :libflint), Void, 
         (Ptr{fq_poly}, Ptr{fq_poly},
         Ptr{fq_poly}, Ptr{FqFiniteField}),
         &z, &x, &y, &base_ring(parent(x)))
   return z
end

function -(x::fq_poly, y::fq_poly)
   check_parent(x,y)
   z = parent(x)()
   ccall((:fq_poly_sub, :libflint), Void,
         (Ptr{fq_poly}, Ptr{fq_poly},
         Ptr{fq_poly}, Ptr{FqFiniteField}),
         &z, &x, &y, &base_ring(parent(x)))
   return z
end

function *(x::fq_poly, y::fq_poly)
   check_parent(x,y)
   z = parent(x)()
   ccall((:fq_poly_mul, :libflint), Void,
         (Ptr{fq_poly}, Ptr{fq_poly},
         Ptr{fq_poly}, Ptr{FqFiniteField}),
         &z, &x, &y, &base_ring(parent(x)))
   return z
end

################################################################################
#
#   Ad hoc binary operators
#
################################################################################

function *(x::fq, y::fq_poly)
   parent(x) != base_ring(parent(y)) &&
         error("Coefficient rings must be equal")
   z = parent(y)()
   ccall((:fq_poly_scalar_mul_fq, :libflint), Void,
         (Ptr{fq_poly}, Ptr{fq_poly},
         Ptr{fq}, Ptr{FqFiniteField}),
         &z, &y, &x, &parent(x))
  return z
end

*(x::fq_poly, y::fq) = y*x

*(x::fmpz, y::fq_poly) = base_ring(parent(y))(x) * y

*(x::fq_poly, y::fmpz) = y*x

*(x::Integer, y::fq_poly) = fmpz(x)*y

*(x::fq_poly, y::Integer) = y*x

+(x::fq, y::fq_poly) = parent(y)(x) + y

+(x::fq_poly, y::fq) = y + x

+(x::fmpz, y::fq_poly) = base_ring(parent(y))(x) + y

+(x::fq_poly, y::fmpz) = y + x

+(x::fq_poly, y::Integer) = x + fmpz(y)

+(x::Integer, y::fq_poly) = y + x

-(x::fq, y::fq_poly) = parent(y)(x) - y

-(x::fq_poly, y::fq) = x - parent(x)(y)

-(x::fmpz, y::fq_poly) = base_ring(parent(y))(x) - y

-(x::fq_poly, y::fmpz) = x - base_ring(parent(x))(y)

-(x::fq_poly, y::Integer) = x - fmpz(y)

-(x::Integer, y::fq_poly) = fmpz(x) - y

################################################################################
#
#   Powering
#
################################################################################

function ^(x::fq_poly, y::Int)
   y < 0 && throw(DomainError())
   z = parent(x)()
   ccall((:fq_poly_pow, :libflint), Void,
         (Ptr{fq_poly}, Ptr{fq_poly}, Int, Ptr{FqFiniteField}), 
         &z, &x, y, &base_ring(parent(x)))
   return z
end

################################################################################
#
#   Comparisons
#
################################################################################

function ==(x::fq_poly, y::fq_poly)
   check_parent(x,y)
   r = ccall((:fq_poly_equal, :libflint), Cint,
             (Ptr{fq_poly}, Ptr{fq_poly}, Ptr{FqFiniteField}),
             &x, &y, &base_ring(parent(x)))
   return Bool(r)
end

################################################################################
#
#   Ad hoc comparisons
#
################################################################################

function ==(x::fq_poly, y::fq) 
   base_ring(parent(x)) != parent(y) && return false
   if length(x) > 1
      return false
   elseif length(x) == 1 
      r = ccall((:fq_poly_equal_fq, :libflint), Cint, 
                (Ptr{fq_poly}, Ptr{fq}, Ptr{FqFiniteField}),
                &x, &y, &base_ring(parent(x)))
      return Bool(r)
   else
      return iszero(y)
  end 
end

==(x::fq, y::fq_poly) = y == x

==(x::fq_poly, y::fmpz) = x == base_ring(parent(x))(y)

==(x::fmpz, y::fq_poly) = y == x

==(x::fq_poly, y::Integer) = x == fmpz(y)

==(x::Integer, y::fq_poly) = y == x

################################################################################
#
#   Truncation
#
################################################################################

function truncate(x::fq_poly, n::Int)
   n < 0 && throw(DomainError())
   if length(x) <= n
      return x
   end
   z = parent(x)()
   ccall((:fq_poly_set_trunc, :libflint), Void,
         (Ptr{fq_poly}, Ptr{fq_poly}, Int, Ptr{FqFiniteField}),
         &z, &x, n, &base_ring(parent(x)))
   return z
end

function mullow(x::fq_poly, y::fq_poly, n::Int)
   check_parent(x,y)
   n < 0 && throw(DomainError())
   z = parent(x)()
   ccall((:fq_poly_mullow, :libflint), Void,
         (Ptr{fq_poly}, Ptr{fq_poly}, Ptr{fq_poly},
         Int, Ptr{FqFiniteField}),
         &z, &x, &y, n, &base_ring(parent(x)))
   return z
end

################################################################################
#
#   Reversal
#
################################################################################

function reverse(x::fq_poly, len::Int)
   len < 0 && throw(DomainError())
   z = parent(x)()
   ccall((:fq_poly_reverse, :libflint), Void,
         (Ptr{fq_poly}, Ptr{fq_poly}, Int, Ptr{FqFiniteField}),
         &z, &x, len, &base_ring(parent(x)))
   return z
end

################################################################################
#
#   Shifting
#
################################################################################

function shift_left(x::fq_poly, len::Int)
   len < 0 && throw(DomainError())
   z = parent(x)()
   ccall((:fq_poly_shift_left, :libflint), Void,
         (Ptr{fq_poly}, Ptr{fq_poly}, Int, Ptr{FqFiniteField}),
         &z, &x, len, &base_ring(parent(x)))
   return z
end

function shift_right(x::fq_poly, len::Int)
   len < 0 && throw(DomainError())
   z = parent(x)()
   ccall((:fq_poly_shift_right, :libflint), Void,
         (Ptr{fq_poly}, Ptr{fq_poly}, Int, Ptr{FqFiniteField}),
         &z, &x, len, &base_ring(parent(x)))
   return z
end

################################################################################
#
#   Euclidean division
#
################################################################################

function div(x::fq_poly, y::fq_poly)
   check_parent(x,y)
   z = parent(x)()
   ccall((:fq_poly_div_basecase, :libflint), Void,
         (Ptr{fq_poly}, Ptr{fq_poly}, Ptr{fq_poly},
         Ptr{FqFiniteField}), &z, &x, &y, &base_ring(parent(x)))
  return z
end

function rem(x::fq_poly, y::fq_poly)
   check_parent(x,y)
   z = parent(x)()
   ccall((:fq_poly_rem, :libflint), Void,
         (Ptr{fq_poly}, Ptr{fq_poly}, Ptr{fq_poly},
         Ptr{FqFiniteField}), &z, &x, &y, &base_ring(parent(x)))
  return z
end

mod(x::fq_poly, y::fq_poly) = rem(x, y)

function divrem(x::fq_poly, y::fq_poly)
   check_parent(x,y)
   z = parent(x)()
   r = parent(x)()
   ccall((:fq_poly_divrem, :libflint), Void, (Ptr{fq_poly},
         Ptr{fq_poly}, Ptr{fq_poly}, Ptr{fq_poly},
         Ptr{FqFiniteField}), &z, &r, &x, &y, &base_ring(parent(x)))
   return z, r
end

################################################################################
#
#   Remove and valuation
#
################################################################################

doc"""
    remove(z::fq_poly, p::fq_poly)
> Computes the valuation of $z$ at $p$, that is, the largest $k$ such that
> $p^k$ divides $z$. Additionally, $z/p^k$ is returned as well.
>
> See also `valuation`, which only returns the valuation.
"""
function remove(z::fq_poly, p::fq_poly)
   check_parent(z,p)
   iszero(z) && error("Not yet implemented")
   z = deepcopy(z)
   v = ccall((:fq_poly_remove, :libflint), Int,
            (Ptr{fq_poly}, Ptr{fq_poly}, Ptr{FqFiniteField}),
             &z,  &p, &base_ring(parent(z)))
   return v, z
end

function divides(z::fq_poly, x::fq_poly)
   check_parent(z, x)
   q = parent(z)()
   v = Bool(ccall((:fq_poly_divides, :libflint), Cint,
            (Ptr{fq_poly}, Ptr{fq_poly},
             Ptr{fq_poly}, Ptr{FqFiniteField}),
             &q, &z, &x, &base_ring(parent(z))))
   return v, q
end

################################################################################
#
#   Modular arithmetic
#
################################################################################

function powmod(x::fq_poly, n::Int, y::fq_poly)
   check_parent(x,y)
   z = parent(x)()

   if n < 0
      g, x = gcdinv(x, y)
      if g != 1
         error("Element not invertible")
      end
      n = -n
   end

   ccall((:fq_poly_powmod_ui_binexp, :libflint), Void,
         (Ptr{fq_poly}, Ptr{fq_poly}, Int, Ptr{fq_poly},
         Ptr{FqFiniteField}), &z, &x, n, &y, &base_ring(parent(x)))
  return z
end

################################################################################
#
#   GCD
#
################################################################################

function gcd(x::fq_poly, y::fq_poly)
   check_parent(x,y)
   z = parent(x)()
   ccall((:fq_poly_gcd, :libflint), Void,
         (Ptr{fq_poly}, Ptr{fq_poly}, Ptr{fq_poly},
         Ptr{FqFiniteField}), &z, &x, &y, &base_ring(parent(x)))
   return z
end

function gcdinv(x::fq_poly, y::fq_poly)
   check_parent(x,y)
   z = parent(x)()
   s = parent(x)()
   t = parent(x)()
   ccall((:fq_poly_xgcd, :libflint), Void,
         (Ptr{fq_poly}, Ptr{fq_poly}, Ptr{fq_poly}, Ptr{fq_poly}, Ptr{fq_poly},
         Ptr{FqFiniteField}), &z, &s, &t, &x, &y, &base_ring(parent(x)))
   return z, s
end

function gcdx(x::fq_poly, y::fq_poly)
   check_parent(x,y)
   z = parent(x)()
   s = parent(x)()
   t = parent(x)()
   ccall((:fq_poly_xgcd, :libflint), Void,
         (Ptr{fq_poly}, Ptr{fq_poly}, Ptr{fq_poly}, Ptr{fq_poly}, Ptr{fq_poly},
         Ptr{FqFiniteField}), &z, &s, &t, &x, &y, &base_ring(parent(x)))
   return z, s, t
end

################################################################################
#
#   Evaluation
#
################################################################################

function evaluate(x::fq_poly, y::fq)
   base_ring(parent(x)) != parent(y) && error("Incompatible coefficient rings")
   z = parent(y)()
   ccall((:fq_poly_evaluate_fq, :libflint), Void,
         (Ptr{fq}, Ptr{fq_poly}, Ptr{fq},
         Ptr{FqFiniteField}), &z, &x, &y, &base_ring(parent(x)))
   return z
end

################################################################################
#
#   Composition
#
################################################################################

function compose(x::fq_poly, y::fq_poly)
   check_parent(x,y)
   z = parent(x)()
   ccall((:fq_poly_compose, :libflint), Void, 
         (Ptr{fq_poly}, Ptr{fq_poly}, Ptr{fq_poly},
         Ptr{FqFiniteField}), &z, &x, &y, &base_ring(parent(x)))
   return z
end

################################################################################
#
#   Derivative
#
################################################################################

function derivative(x::fq_poly)
   z = parent(x)()
   ccall((:fq_poly_derivative, :libflint), Void, 
         (Ptr{fq_poly}, Ptr{fq_poly}, Ptr{FqFiniteField}),
         &z, &x, &base_ring(parent(x)))
   return z
end

################################################################################
#
#  Inflation and deflation
#
################################################################################

function inflate(x::fq_poly, n::Int)
   z = parent(x)()
   ccall((:fq_poly_inflate, :libflint), Void, (Ptr{fq_poly},
         Ptr{fq_poly}, Culong, Ptr{FqFiniteField}),
         &z, &x, UInt(n), &base_ring(parent(x)))
   return z
end

function deflate(x::fq_poly, n::Int)
   z = parent(x)()
   ccall((:fq_poly_deflate, :libflint), Void,
         (Ptr{fq_poly}, Ptr{fq_poly}, Culong, Ptr{FqFiniteField}),
         &z, &x, UInt(n), &base_ring(parent(x)))
  return z
end

################################################################################
#
#  Irreducibility
#
################################################################################

doc"""
    isirreducible(x::fq_poly)
> Return `true` if $x$ is irreducible, otherwise return `false`.
"""
function isirreducible(x::fq_poly)
  return Bool(ccall((:fq_poly_is_irreducible, :libflint), Int32,
                    (Ptr{fq_poly}, Ptr{FqFiniteField} ),
                    &x, &base_ring(parent(x))))
end

################################################################################
#
#  Squarefree testing
#
################################################################################

doc"""
    issquarefree(x::fq_poly)
> Return `true` if $x$ is squarefree, otherwise return `false`.
"""
function issquarefree(x::fq_poly)
   return Bool(ccall((:fq_poly_is_squarefree, :libflint), Int32, 
       (Ptr{fq_poly}, Ptr{FqFiniteField}), &x, &base_ring(parent(x))))
end

################################################################################
#
#  Factorization
#
################################################################################

doc"""
    factor(x::fq_poly)
> Return the factorisation of $x$.
"""
function factor(x::fq_poly)
   fac, z = _factor(x)
   return Fac(parent(x)(z), fac)
end

function _factor(x::fq_poly)
   R = parent(x)
   F = base_ring(R)
   a = F()
   fac = fq_poly_factor(F)
   ccall((:fq_poly_factor, :libflint), Void, (Ptr{fq_poly_factor},
         Ptr{fq}, Ptr{fq_poly}, Ptr{FqFiniteField}),
         &fac, &a, &x, &F)
   res = Dict{fq_poly,Int}()
   for i in 1:fac.num
      f = R()
      ccall((:fq_poly_factor_get_poly, :libflint), Void,
            (Ptr{fq_poly}, Ptr{fq_poly_factor}, Int,
            Ptr{FqFiniteField}), &f, &fac, i-1, &F)
      e = unsafe_load(fac.exp,i)
      res[f] = e
   end
   return res, a
end

doc"""
    factor_squarefree(x::fq_poly)
> Return the squarefree factorisation of $x$.
"""
function factor_squarefree(x::fq_poly)
  # _factor_squareefree does weird things if the polynomial is not monic
  return Fac(parent(x)(lead(x)), _factor_squarefree(divexact(x, lead(x))))
end

function _factor_squarefree(x::fq_poly)
  F = base_ring(parent(x))
  fac = fq_poly_factor(F)
  ccall((:fq_poly_factor_squarefree, :libflint), UInt,
        (Ptr{fq_poly_factor}, Ptr{fq_poly}, Ptr{FqFiniteField}), &fac, &x, &F)
  res = Dict{fq_poly,Int}()
  for i in 1:fac.num
    f = parent(x)()
    ccall((:fq_poly_factor_get_poly, :libflint), Void,
          (Ptr{fq_poly}, Ptr{fq_poly_factor}, Int,
          Ptr{FqFiniteField}), &f, &fac, i-1, &F)
    e = unsafe_load(fac.exp, i)
    res[f] = e
  end
  return res
end

doc"""
    factor_distinct_deg(x::fq_poly)
> Return the distinct degree factorisation of a squarefree polynomial $x$.
"""
function factor_distinct_deg(x::fq_poly)
   R = parent(x)
   F = base_ring(R)
   fac = fq_poly_factor(F)
   tmp = fq_poly_factor(F)
   ccall((:fq_poly_factor_fit_length, :libflint), Void,
         (Ptr{fq_poly_factor}, Int, Ptr{FqFiniteField}),
         &tmp, degree(x), &F)
   ccall((:fq_poly_factor_distinct_deg, :libflint), Void, 
         (Ptr{fq_poly_factor}, Ptr{fq_poly}, Ptr{Int},
         Ptr{FqFiniteField}), &fac, &x, &tmp.exp, &F)
   res = Dict{Int, fq_poly}()
   for i in 1:fac.num
      f = R()
      ccall((:fq_poly_factor_get_poly, :libflint), Void,
            (Ptr{fq_poly}, Ptr{fq_poly_factor}, Int,
            Ptr{FqFiniteField}), &f, &fac, i-1, &F)
      d = unsafe_load(tmp.exp,i)
      res[d] = f
   end
   return res
end

################################################################################
#
#   Unsafe functions
#
################################################################################

function zero!(z::fq_poly)
   ccall((:fq_poly_zero, :libflint), Void, 
         (Ptr{fq_poly}, Ptr{FqFiniteField}),
         &z, &base_ring(parent(z)))
   return z
end

function fit!(z::fq_poly, n::Int)
   ccall((:fq_poly_fit_length, :libflint), Void, 
         (Ptr{fq_poly}, Int, Ptr{FqFiniteField}),
         &z, n, &base_ring(parent(z)))
   return nothing
end

function setcoeff!(z::fq_poly, n::Int, x::fq)
   ccall((:fq_poly_set_coeff, :libflint), Void, 
         (Ptr{fq_poly}, Int, Ptr{fq}, Ptr{FqFiniteField}),
         &z, n, &x, &base_ring(parent(z)))
   return z
end

function mul!(z::fq_poly, x::fq_poly, y::fq_poly)
   ccall((:fq_poly_mul, :libflint), Void, 
         (Ptr{fq_poly}, Ptr{fq_poly}, Ptr{fq_poly},
         Ptr{FqFiniteField}), &z, &x, &y, &base_ring(parent(x)))
   return z
end

function add!(z::fq_poly, x::fq_poly, y::fq_poly)
   ccall((:fq_poly_add, :libflint), Void, 
         (Ptr{fq_poly}, Ptr{fq_poly}, Ptr{fq_poly},
         Ptr{FqFiniteField}), &z, &x, &y, &base_ring(parent(x)))
   return z
end

function sub!(z::fq_poly, x::fq_poly, y::fq_poly)
   ccall((:fq_poly_sub, :libflint), Void, 
         (Ptr{fq_poly}, Ptr{fq_poly}, Ptr{fq_poly},
         Ptr{FqFiniteField}), &z, &x, &y, &base_ring(parent(x)))
   return z
end


function addeq!(z::fq_poly, x::fq_poly)
   ccall((:fq_poly_add, :libflint), Void, 
         (Ptr{fq_poly}, Ptr{fq_poly}, Ptr{fq_poly},
         Ptr{FqFiniteField}), &z, &z, &x, &base_ring(parent(x)))
   return z
end

################################################################################
#
#  Promotion rules
#
################################################################################

promote_rule(::Type{fq_poly}, ::Type{V}) where {V <: Integer} = fq_poly

promote_rule(::Type{fq_poly}, ::Type{fmpz}) = fq_poly

promote_rule(::Type{fq_poly}, ::Type{fq}) = fq_poly

###############################################################################
#
#   Polynomial substitution
#
###############################################################################

function (f::fq_poly)(a::fq)
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
   z = fq_poly()
   z.parent = R
   return z
end

function (R::FqPolyRing)(x::fq)
  z = fq_poly(x)
  z.parent = R
  return z
end

function (R::FqPolyRing)(x::fmpz)
   return R(base_ring(R)(x))
end

function (R::FqPolyRing)(x::Integer)
   return R(fmpz(x))
end

function (R::FqPolyRing)(x::Array{fq, 1})
   length(x) == 0 && error("Array must be non-empty")
   base_ring(R) != parent(x[1]) && error("Coefficient rings must coincide")
   z = fq_poly(x)
   z.parent = R
   return z
end

function (R::FqPolyRing)(x::Array{fmpz, 1})
   length(x) == 0 && error("Array must be non-empty")
   z = fq_poly(x, base_ring(R))
   z.parent = R
   return z
end

function (R::FqPolyRing)(x::Array{T, 1}) where {T <: Integer}
   length(x) == 0 && error("Array must be non-empty")
   return R(map(fmpz, x))
end

function (R::FqPolyRing)(x::fmpz_poly)
   z = fq_poly(x, base_ring(R))
   z.parent = R
   return z
end

function (R::FqPolyRing)(x::fq_poly)
  parent(x) != R && error("Unable to coerce to polynomial")
  return x
end

################################################################################
#
#   PolynomialRing constructor
#
################################################################################

function PolynomialRing(R::FqFiniteField, s::AbstractString; cached = true)
   S = Symbol(s)
   parent_obj = FqPolyRing(R, S, cached)
   return parent_obj, parent_obj([R(0), R(1)])
end

