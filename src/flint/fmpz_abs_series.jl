###############################################################################
#
#   ZZAbsPowerSeriesRingElem.jl : Power series over flint ZZRingElem integers
#
###############################################################################

export ZZAbsPowerSeriesRingElem, ZZAbsPowerSeriesRing, PowerSeriesRing

###############################################################################
#
#   Data type and parent object methods
#
###############################################################################

function O(a::ZZAbsPowerSeriesRingElem)
   if iszero(a)
      return deepcopy(a)    # 0 + O(x^n)
   end
   prec = length(a) - 1
   prec < 0 && throw(DomainError(prec, "Precision must be non-negative"))
   z = ZZAbsPowerSeriesRingElem(Vector{ZZRingElem}(undef, 0), 0, prec)
   z.parent = parent(a)
   return z
end

elem_type(::Type{ZZAbsPowerSeriesRing}) = ZZAbsPowerSeriesRingElem

parent_type(::Type{ZZAbsPowerSeriesRingElem}) = ZZAbsPowerSeriesRing

base_ring(R::ZZAbsPowerSeriesRing) = R.base_ring

abs_series_type(::Type{ZZRingElem}) = ZZAbsPowerSeriesRingElem

var(a::ZZAbsPowerSeriesRing) = a.S

###############################################################################
#
#   Basic manipulation
#
###############################################################################

max_precision(R::ZZAbsPowerSeriesRing) = R.prec_max

function normalise(a::ZZAbsPowerSeriesRingElem, len::Int)
   if len > 0
      c = ZZRingElem()
      ccall((:fmpz_poly_get_coeff_fmpz, libflint), Nothing,
         (Ref{ZZRingElem}, Ref{ZZAbsPowerSeriesRingElem}, Int), c, a, len - 1)
   end
   while len > 0 && iszero(c)
      len -= 1
      if len > 0
         ccall((:fmpz_poly_get_coeff_fmpz, libflint), Nothing,
            (Ref{ZZRingElem}, Ref{ZZAbsPowerSeriesRingElem}, Int), c, a, len - 1)
      end
   end

   return len
end

function length(x::ZZAbsPowerSeriesRingElem)
   return ccall((:fmpz_poly_length, libflint), Int, (Ref{ZZAbsPowerSeriesRingElem},), x)
end

precision(x::ZZAbsPowerSeriesRingElem) = x.prec

function coeff(x::ZZAbsPowerSeriesRingElem, n::Int)
   if n < 0
      return ZZRingElem(0)
   end
   z = ZZRingElem()
   ccall((:fmpz_poly_get_coeff_fmpz, libflint), Nothing,
         (Ref{ZZRingElem}, Ref{ZZAbsPowerSeriesRingElem}, Int), z, x, n)
   return z
end

zero(R::ZZAbsPowerSeriesRing) = R(0)

one(R::ZZAbsPowerSeriesRing) = R(1)

function gen(R::ZZAbsPowerSeriesRing)
   z = ZZAbsPowerSeriesRingElem([ZZRingElem(0), ZZRingElem(1)], 2, max_precision(R))
   z.parent = R
   return z
end

function deepcopy_internal(a::ZZAbsPowerSeriesRingElem, dict::IdDict)
   z = ZZAbsPowerSeriesRingElem(a)
   z.prec = a.prec
   z.parent = parent(a)
   return z
end

function is_gen(a::ZZAbsPowerSeriesRingElem)
   return precision(a) == 0 || ccall((:fmpz_poly_is_gen, libflint), Bool,
                            (Ref{ZZAbsPowerSeriesRingElem},), a)
end

iszero(a::ZZAbsPowerSeriesRingElem) = length(a) == 0

is_unit(a::ZZAbsPowerSeriesRingElem) = valuation(a) == 0 && is_unit(coeff(a, 0))

function isone(a::ZZAbsPowerSeriesRingElem)
   return precision(a) == 0 || ccall((:fmpz_poly_is_one, libflint), Bool,
                                (Ref{ZZAbsPowerSeriesRingElem},), a)
end

# todo: write an fmpz_poly_valuation
function valuation(a::ZZAbsPowerSeriesRingElem)
   for i = 1:length(a)
      if !iszero(coeff(a, i - 1))
         return i - 1
      end
   end
   return precision(a)
end

characteristic(::ZZAbsPowerSeriesRing) = 0

###############################################################################
#
#   Similar
#
###############################################################################

function similar(f::AbsSeriesElem, R::ZZRing, max_prec::Int,
                                   s::Symbol=var(parent(f)); cached::Bool=true)
   z = ZZAbsPowerSeriesRingElem()
   if base_ring(f) === R && s == var(parent(f)) &&
      typeof(f) == ZZAbsPowerSeriesRingElem && max_precision(parent(f)) == max_prec
      # steal parent in case it is not cached
      z.parent = parent(f)
   else
      z.parent = ZZAbsPowerSeriesRing(max_prec, s, cached)
   end
   z.prec = max_prec
   return z
end

###############################################################################
#
#   abs_series constructor
#
###############################################################################

function abs_series(R::ZZRing, arr::Vector{T},
                           len::Int, prec::Int, var::String="x";
                            max_precision::Int=prec, cached::Bool=true) where T
   prec < len && error("Precision too small for given data")
   coeffs = T == ZZRingElem ? arr : map(R, arr)
   coeffs = length(coeffs) == 0 ? ZZRingElem[] : coeffs
   z = ZZAbsPowerSeriesRingElem(coeffs, len, prec)
   z.parent = ZZAbsPowerSeriesRing(max_precision, Symbol(var), cached)
   return z
end

###############################################################################
#
#   AbstractString I/O
#
###############################################################################

function show(io::IO, a::ZZAbsPowerSeriesRing)
   print(io, "Univariate power series ring in ", var(a), " over ")
   show(io, base_ring(a))
end

###############################################################################
#
#   Unary operators
#
###############################################################################

function -(x::ZZAbsPowerSeriesRingElem)
   z = parent(x)()
   ccall((:fmpz_poly_neg, libflint), Nothing,
                (Ref{ZZAbsPowerSeriesRingElem}, Ref{ZZAbsPowerSeriesRingElem}),
               z, x)
   z.prec = x.prec
   return z
end

###############################################################################
#
#   Binary operators
#
###############################################################################

function +(a::ZZAbsPowerSeriesRingElem, b::ZZAbsPowerSeriesRingElem)
   check_parent(a, b)
   lena = length(a)
   lenb = length(b)

   prec = min(a.prec, b.prec)

   lena = min(lena, prec)
   lenb = min(lenb, prec)

   lenz = max(lena, lenb)
   z = parent(a)()
   z.prec = prec
   ccall((:fmpz_poly_add_series, libflint), Nothing,
                (Ref{ZZAbsPowerSeriesRingElem}, Ref{ZZAbsPowerSeriesRingElem}, Ref{ZZAbsPowerSeriesRingElem}, Int),
               z, a, b, lenz)
   return z
end

function -(a::ZZAbsPowerSeriesRingElem, b::ZZAbsPowerSeriesRingElem)
   check_parent(a, b)
   lena = length(a)
   lenb = length(b)

   prec = min(a.prec, b.prec)

   lena = min(lena, prec)
   lenb = min(lenb, prec)

   lenz = max(lena, lenb)
   z = parent(a)()
   z.prec = prec
   ccall((:fmpz_poly_sub_series, libflint), Nothing,
                (Ref{ZZAbsPowerSeriesRingElem}, Ref{ZZAbsPowerSeriesRingElem}, Ref{ZZAbsPowerSeriesRingElem}, Int),
               z, a, b, lenz)
   return z
end

function *(a::ZZAbsPowerSeriesRingElem, b::ZZAbsPowerSeriesRingElem)
   check_parent(a, b)
   lena = length(a)
   lenb = length(b)

   aval = valuation(a)
   bval = valuation(b)

   prec = min(a.prec + bval, b.prec + aval)
   prec = min(prec, max_precision(parent(a)))

   lena = min(lena, prec)
   lenb = min(lenb, prec)

   z = parent(a)()
   z.prec = prec

   if lena == 0 || lenb == 0
      return z
   end

   lenz = min(lena + lenb - 1, prec)

   ccall((:fmpz_poly_mullow, libflint), Nothing,
                (Ref{ZZAbsPowerSeriesRingElem}, Ref{ZZAbsPowerSeriesRingElem}, Ref{ZZAbsPowerSeriesRingElem}, Int),
               z, a, b, lenz)
   return z
end

###############################################################################
#
#   Ad hoc binary operators
#
###############################################################################

function *(x::Int, y::ZZAbsPowerSeriesRingElem)
   z = parent(y)()
   z.prec = y.prec
   ccall((:fmpz_poly_scalar_mul_si, libflint), Nothing,
                (Ref{ZZAbsPowerSeriesRingElem}, Ref{ZZAbsPowerSeriesRingElem}, Int),
               z, y, x)
   return z
end

*(x::ZZAbsPowerSeriesRingElem, y::Int) = y * x

function *(x::ZZRingElem, y::ZZAbsPowerSeriesRingElem)
   z = parent(y)()
   z.prec = y.prec
   ccall((:fmpz_poly_scalar_mul_fmpz, libflint), Nothing,
                (Ref{ZZAbsPowerSeriesRingElem}, Ref{ZZAbsPowerSeriesRingElem}, Ref{ZZRingElem}),
               z, y, x)
   return z
end

*(x::ZZAbsPowerSeriesRingElem, y::ZZRingElem) = y * x

*(x::Integer, y::ZZAbsPowerSeriesRingElem) = ZZRingElem(x)*y

*(x::ZZAbsPowerSeriesRingElem, y::Integer) = y*x

###############################################################################
#
#   Shifting
#
###############################################################################

function shift_left(x::ZZAbsPowerSeriesRingElem, len::Int)
   len < 0 && throw(DomainError(len, "Shift must be non-negative"))
   xlen = length(x)
   z = parent(x)()
   z.prec = x.prec + len
   z.prec = min(z.prec, max_precision(parent(x)))
   zlen = min(z.prec, xlen + len)
   ccall((:fmpz_poly_shift_left, libflint), Nothing,
                (Ref{ZZAbsPowerSeriesRingElem}, Ref{ZZAbsPowerSeriesRingElem}, Int),
               z, x, len)
   ccall((:fmpz_poly_set_trunc, libflint), Nothing,
                (Ref{ZZAbsPowerSeriesRingElem}, Ref{ZZAbsPowerSeriesRingElem}, Int),
               z, z, zlen)
   return z
end

function shift_right(x::ZZAbsPowerSeriesRingElem, len::Int)
   len < 0 && throw(DomainError(len, "Shift must be non-negative"))
   xlen = length(x)
   z = parent(x)()
   if len >= xlen
      z.prec = max(0, x.prec - len)
   else
      z.prec = x.prec - len
      ccall((:fmpz_poly_shift_right, libflint), Nothing,
                (Ref{ZZAbsPowerSeriesRingElem}, Ref{ZZAbsPowerSeriesRingElem}, Int),
               z, x, len)
   end
   return z
end

###############################################################################
#
#   Truncation
#
###############################################################################

function truncate(x::ZZAbsPowerSeriesRingElem, prec::Int)
   prec < 0 && throw(DomainError(prec, "Index must be non-negative"))
   if x.prec <= prec
      return x
   end
   z = parent(x)()
   z.prec = prec
   ccall((:fmpz_poly_set_trunc, libflint), Nothing,
                (Ref{ZZAbsPowerSeriesRingElem}, Ref{ZZAbsPowerSeriesRingElem}, Int),
               z, x, prec)
   return z
end

###############################################################################
#
#   Powering
#
###############################################################################

function ^(a::ZZAbsPowerSeriesRingElem, b::Int)
   b < 0 && throw(DomainError(b, "Exponent must be non-negative"))
   if precision(a) > 0 && is_gen(a) && b > 0
      return shift_left(a, b - 1)
   elseif length(a) == 1
      return parent(a)([coeff(a, 0)^b], 1, a.prec)
   elseif b == 0
      z = one(parent(a))
      z = set_precision!(z, precision(a))
      return z
   else
      z = parent(a)()
      z.prec = a.prec + (b - 1)*valuation(a)
      z.prec = min(z.prec, max_precision(parent(a)))
      ccall((:fmpz_poly_pow_trunc, libflint), Nothing,
                (Ref{ZZAbsPowerSeriesRingElem}, Ref{ZZAbsPowerSeriesRingElem}, Int, Int),
               z, a, b, z.prec)
   end
   return z
end

###############################################################################
#
#   Comparison
#
###############################################################################

function ==(x::ZZAbsPowerSeriesRingElem, y::ZZAbsPowerSeriesRingElem)
   check_parent(x, y)
   prec = min(x.prec, y.prec)

   n = max(length(x), length(y))
   n = min(n, prec)

   return Bool(ccall((:fmpz_poly_equal_trunc, libflint), Cint,
                (Ref{ZZAbsPowerSeriesRingElem}, Ref{ZZAbsPowerSeriesRingElem}, Int),
               x, y, n))
end

function isequal(x::ZZAbsPowerSeriesRingElem, y::ZZAbsPowerSeriesRingElem)
   if parent(x) != parent(y)
      return false
   end
   if x.prec != y.prec || length(x) != length(y)
      return false
   end
   return Bool(ccall((:fmpz_poly_equal, libflint), Cint,
                (Ref{ZZAbsPowerSeriesRingElem}, Ref{ZZAbsPowerSeriesRingElem}),
               x, y))
end

###############################################################################
#
#   Ad hoc comparisons
#
###############################################################################

function ==(x::ZZAbsPowerSeriesRingElem, y::ZZRingElem)
   if length(x) > 1
      return false
   elseif length(x) == 1
      z = ZZRingElem()
      ccall((:fmpz_poly_get_coeff_fmpz, libflint), Nothing,
                       (Ref{ZZRingElem}, Ref{ZZAbsPowerSeriesRingElem}, Int), z, x, 0)
      return ccall((:fmpz_equal, libflint), Bool,
               (Ref{ZZRingElem}, Ref{ZZRingElem}, Int), z, y, 0)
   else
      return precision(x) == 0 || iszero(y)
   end
end

==(x::ZZRingElem, y::ZZAbsPowerSeriesRingElem) = y == x

==(x::ZZAbsPowerSeriesRingElem, y::Integer) = x == ZZRingElem(y)

==(x::Integer, y::ZZAbsPowerSeriesRingElem) = y == x

###############################################################################
#
#   Exact division
#
###############################################################################

function divexact(x::ZZAbsPowerSeriesRingElem, y::ZZAbsPowerSeriesRingElem; check::Bool=true)
   check_parent(x, y)
   iszero(y) && throw(DivideError())
   v2 = valuation(y)
   v1 = valuation(x)
   if v2 != 0
      if check && v1 < v2
         error("Not an exact division")
      end
      x = shift_right(x, v2)
      y = shift_right(y, v2)
   end
   prec = min(x.prec, y.prec - v2 + v1)
   z = parent(x)()
   z.prec = prec
   ccall((:fmpz_poly_div_series, libflint), Nothing,
                (Ref{ZZAbsPowerSeriesRingElem}, Ref{ZZAbsPowerSeriesRingElem}, Ref{ZZAbsPowerSeriesRingElem}, Int),
               z, x, y, prec)
   return z
end

###############################################################################
#
#   Ad hoc exact division
#
###############################################################################

function divexact(x::ZZAbsPowerSeriesRingElem, y::Int; check::Bool=true)
   y == 0 && throw(DivideError())
   z = parent(x)()
   z.prec = x.prec
   ccall((:fmpz_poly_scalar_divexact_si, libflint), Nothing,
                (Ref{ZZAbsPowerSeriesRingElem}, Ref{ZZAbsPowerSeriesRingElem}, Int),
               z, x, y)
   return z
end

function divexact(x::ZZAbsPowerSeriesRingElem, y::ZZRingElem; check::Bool=true)
   iszero(y) && throw(DivideError())
   z = parent(x)()
   z.prec = x.prec
   ccall((:fmpz_poly_scalar_divexact_fmpz, libflint), Nothing,
                (Ref{ZZAbsPowerSeriesRingElem}, Ref{ZZAbsPowerSeriesRingElem}, Ref{ZZRingElem}),
               z, x, y)
   return z
end

divexact(x::ZZAbsPowerSeriesRingElem, y::Integer; check::Bool=true) = divexact(x, ZZRingElem(y); check=check)

###############################################################################
#
#   Inversion
#
###############################################################################

function inv(a::ZZAbsPowerSeriesRingElem)
    iszero(a) && throw(DivideError())
    !is_unit(a) && error("Unable to invert power series")
    ainv = parent(a)()
    ainv.prec = a.prec
    ccall((:fmpz_poly_inv_series, libflint), Nothing,
          (Ref{ZZAbsPowerSeriesRingElem}, Ref{ZZAbsPowerSeriesRingElem}, Int),
                  ainv, a, a.prec)
    return ainv
end

###############################################################################
#
#   Square root
#
###############################################################################

function Base.sqrt(a::ZZAbsPowerSeriesRingElem; check::Bool=true)
    asqrt = parent(a)()
    v = valuation(a)
    asqrt.prec = a.prec - div(v, 2)
    flag = Bool(ccall((:fmpz_poly_sqrt_series, libflint), Cint,
               (Ref{ZZAbsPowerSeriesRingElem}, Ref{ZZAbsPowerSeriesRingElem}, Int),
                  asqrt, a, a.prec))
    check && !flag && error("Not a square")
    return asqrt
end

###############################################################################
#
#   Unsafe functions
#
###############################################################################

function zero!(z::ZZAbsPowerSeriesRingElem)
   ccall((:fmpz_poly_zero, libflint), Nothing,
                (Ref{ZZAbsPowerSeriesRingElem},), z)
   z.prec = parent(z).prec_max
   return z
end

function fit!(z::ZZAbsPowerSeriesRingElem, n::Int)
   ccall((:fmpz_poly_fit_length, libflint), Nothing,
                 (Ref{ZZAbsPowerSeriesRingElem}, Int), z, n)
   return nothing
end

function setcoeff!(z::ZZAbsPowerSeriesRingElem, n::Int, x::ZZRingElem)
   ccall((:fmpz_poly_set_coeff_fmpz, libflint), Nothing,
                (Ref{ZZAbsPowerSeriesRingElem}, Int, Ref{ZZRingElem}),
               z, n, x)
   return z
end

function mul!(z::ZZAbsPowerSeriesRingElem, a::ZZAbsPowerSeriesRingElem, b::ZZAbsPowerSeriesRingElem)
   lena = length(a)
   lenb = length(b)

   aval = valuation(a)
   bval = valuation(b)

   prec = min(a.prec + bval, b.prec + aval)
   prec = min(prec, max_precision(parent(z)))

   lena = min(lena, prec)
   lenb = min(lenb, prec)

   lenz = min(lena + lenb - 1, prec)
   if lenz < 0
      lenz = 0
   end

   z.prec = prec
   ccall((:fmpz_poly_mullow, libflint), Nothing,
                (Ref{ZZAbsPowerSeriesRingElem}, Ref{ZZAbsPowerSeriesRingElem}, Ref{ZZAbsPowerSeriesRingElem}, Int),
               z, a, b, lenz)
   return z
end

function addeq!(a::ZZAbsPowerSeriesRingElem, b::ZZAbsPowerSeriesRingElem)
   lena = length(a)
   lenb = length(b)

   prec = min(a.prec, b.prec)

   lena = min(lena, prec)
   lenb = min(lenb, prec)

   lenz = max(lena, lenb)
   a.prec = prec
   ccall((:fmpz_poly_add_series, libflint), Nothing,
                (Ref{ZZAbsPowerSeriesRingElem}, Ref{ZZAbsPowerSeriesRingElem}, Ref{ZZAbsPowerSeriesRingElem}, Int),
               a, a, b, lenz)
   return a
end

function add!(c::ZZAbsPowerSeriesRingElem, a::ZZAbsPowerSeriesRingElem, b::ZZAbsPowerSeriesRingElem)
   lena = length(a)
   lenb = length(b)

   prec = min(a.prec, b.prec)

   lena = min(lena, prec)
   lenb = min(lenb, prec)

   lenc = max(lena, lenb)
   c.prec = prec
   ccall((:fmpz_poly_add_series, libflint), Nothing,
                (Ref{ZZAbsPowerSeriesRingElem}, Ref{ZZAbsPowerSeriesRingElem}, Ref{ZZAbsPowerSeriesRingElem}, Int),
               c, a, b, lenc)
   return c
end

function set_length!(a::ZZAbsPowerSeriesRingElem, n::Int)
   ccall((:_fmpz_poly_set_length, libflint), Nothing,
         (Ref{ZZAbsPowerSeriesRingElem}, Int), a, n)
   return a
end

###############################################################################
#
#   Promotion rules
#
###############################################################################

promote_rule(::Type{ZZAbsPowerSeriesRingElem}, ::Type{T}) where {T <: Integer} = ZZAbsPowerSeriesRingElem

promote_rule(::Type{ZZAbsPowerSeriesRingElem}, ::Type{ZZRingElem}) = ZZAbsPowerSeriesRingElem

###############################################################################
#
#   Parent object call overload
#
###############################################################################

function (a::ZZAbsPowerSeriesRing)()
   z = ZZAbsPowerSeriesRingElem()
   z.prec = a.prec_max
   z.parent = a
   return z
end

function (a::ZZAbsPowerSeriesRing)(b::Integer)
   if b == 0
      z = ZZAbsPowerSeriesRingElem()
      z.prec = a.prec_max
   else
      z = ZZAbsPowerSeriesRingElem([ZZRingElem(b)], 1, a.prec_max)
   end
   z.parent = a
   return z
end

function (a::ZZAbsPowerSeriesRing)(b::ZZRingElem)
   if iszero(b)
      z = ZZAbsPowerSeriesRingElem()
      z.prec = a.prec_max
   else
      z = ZZAbsPowerSeriesRingElem([b], 1, a.prec_max)
   end
   z.parent = a
   return z
end

function (a::ZZAbsPowerSeriesRing)(b::ZZAbsPowerSeriesRingElem)
   parent(b) != a && error("Unable to coerce power series")
   return b
end

function (a::ZZAbsPowerSeriesRing)(b::Vector{ZZRingElem}, len::Int, prec::Int)
   z = ZZAbsPowerSeriesRingElem(b, len, prec)
   z.parent = a
   return z
end

###############################################################################
#
#   PowerSeriesRing constructor
#
###############################################################################

function PowerSeriesRing(R::ZZRing, prec::Int, s::Symbol;  model=:capped_relative, cached = true)
   if model == :capped_relative
      parent_obj = ZZRelPowerSeriesRing(prec, s, cached)
   elseif model == :capped_absolute
      parent_obj = ZZAbsPowerSeriesRing(prec, s, cached)
   else
      error("Unknown model")
   end

   return parent_obj, gen(parent_obj)
end

function PowerSeriesRing(R::ZZRing, prec::Int, s::AbstractString; model=:capped_relative, cached = true)
   return PowerSeriesRing(R, prec, Symbol(s); model=model, cached=cached)
end

function AbsSeriesRing(R::ZZRing, prec::Int)
   return ZZAbsPowerSeriesRing(prec, :x, false)
end

function RelSeriesRing(R::ZZRing, prec::Int)
   return ZZRelPowerSeriesRing(prec, :x, false)
end
