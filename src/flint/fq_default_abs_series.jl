###############################################################################
#
#   fq_default_abs_series.jl : Power series over flint ZZRingElem integers
#
###############################################################################

export fq_default_abs_series, FqDefaultAbsSeriesRing

###############################################################################
#
#   Data type and parent object methods
#
###############################################################################

function O(a::fq_default_abs_series)
   if iszero(a)
      return deepcopy(a)    # 0 + O(x^n)
   end
   prec = length(a) - 1
   prec < 0 && throw(DomainError(prec, "Valuation must be non-negative"))
   z = fq_default_abs_series(base_ring(a), Vector{FqFieldElem}(undef, 0), 0, prec)
   z.parent = parent(a)
   return z
end

elem_type(::Type{FqDefaultAbsSeriesRing}) = fq_default_abs_series

parent_type(::Type{fq_default_abs_series}) = FqDefaultAbsSeriesRing

base_ring(R::FqDefaultAbsSeriesRing) = R.base_ring

abs_series_type(::Type{FqFieldElem}) = fq_default_abs_series

var(a::FqDefaultAbsSeriesRing) = a.S

###############################################################################
#
#   Basic manipulation
#
###############################################################################

max_precision(R::FqDefaultAbsSeriesRing) = R.prec_max

function normalise(a::fq_default_abs_series, len::Int)
   ctx = base_ring(a)
   if len > 0
      c = base_ring(a)()
      ccall((:fq_default_poly_get_coeff, libflint), Nothing,
         (Ref{FqFieldElem}, Ref{fq_default_abs_series}, Int, Ref{FqField}),
          c, a, len - 1, ctx)
   end
   while len > 0 && iszero(c)
      len -= 1
      if len > 0
         ccall((:fq_default_poly_get_coeff, libflint), Nothing,
            (Ref{FqFieldElem}, Ref{fq_default_abs_series}, Int, Ref{FqField}),
             c, a, len - 1, ctx)
      end
   end

   return len
end

function length(x::fq_default_abs_series)
   return ccall((:fq_default_poly_length, libflint), Int,
                (Ref{fq_default_abs_series}, Ref{FqField}), x, base_ring(x))
end

precision(x::fq_default_abs_series) = x.prec

function coeff(x::fq_default_abs_series, n::Int)
   if n < 0
      return base_ring(x)()
   end
   z = base_ring(x)()
   ccall((:fq_default_poly_get_coeff, libflint), Nothing,
         (Ref{FqFieldElem}, Ref{fq_default_abs_series}, Int, Ref{FqField}),
          z, x, n, base_ring(x))
   return z
end

zero(R::FqDefaultAbsSeriesRing) = R(0)

one(R::FqDefaultAbsSeriesRing) = R(1)

function gen(R::FqDefaultAbsSeriesRing)
   S = base_ring(R)
   z = fq_default_abs_series(S, [S(0), S(1)], 2, max_precision(R))
   z.parent = R
   return z
end

function deepcopy_internal(a::fq_default_abs_series, dict::IdDict)
   z = fq_default_abs_series(base_ring(a), a)
   z.prec = a.prec
   z.parent = parent(a)
   return z
end

function is_gen(a::fq_default_abs_series)
   return precision(a) == 0 || ccall((:fq_default_poly_is_gen, libflint), Bool,
                   (Ref{fq_default_abs_series}, Ref{FqField}), a, base_ring(a))
end

iszero(a::fq_default_abs_series) = length(a) == 0

is_unit(a::fq_default_abs_series) = valuation(a) == 0 && is_unit(coeff(a, 0))

function isone(a::fq_default_abs_series)
   return precision(a) == 0 || ccall((:fq_default_poly_is_one, libflint), Bool,
                   (Ref{fq_default_abs_series}, Ref{FqField}), a, base_ring(a))
end

# todo: write an fq_default_poly_valuation
function valuation(a::fq_default_abs_series)
   for i = 1:length(a)
      if !iszero(coeff(a, i - 1))
         return i - 1
      end
   end
   return precision(a)
end

characteristic(R::FqDefaultAbsSeriesRing) = characteristic(base_ring(R))

###############################################################################
#
#   Similar
#
###############################################################################

function similar(f::AbsSeriesElem, R::FqField, max_prec::Int,
                                 var::Symbol=var(parent(f)); cached::Bool=true)
   z = fq_default_abs_series(R)
   z.parent = FqDefaultAbsSeriesRing(R, max_prec, var, cached)
   z.prec = max_prec
   return z
end

###############################################################################
#
#   abs_series constructor
#
###############################################################################

function abs_series(R::FqField, arr::Vector{T},
                           len::Int, prec::Int, var::String="x";
                            max_precision::Int=prec, cached::Bool=true) where T
   prec < len && error("Precision too small for given data")
   coeffs = T == FqFieldElem ? arr : map(R, arr)
   coeffs = length(coeffs) == 0 ? FqFieldElem[] : coeffs
   z = fq_default_abs_series(R, coeffs, len, prec)
   z.parent = FqDefaultAbsSeriesRing(R, max_precision, Symbol(var), cached)
   return z
end

###############################################################################
#
#   AbstractString I/O
#
###############################################################################

function show(io::IO, a::FqDefaultAbsSeriesRing)
   print(io, "Univariate power series ring in ", var(a), " over ")
   show(io, base_ring(a))
end

###############################################################################
#
#   Unary operators
#
###############################################################################

function -(x::fq_default_abs_series)
   z = parent(x)()
   ccall((:fq_default_poly_neg, libflint), Nothing,
                (Ref{fq_default_abs_series}, Ref{fq_default_abs_series}, Ref{FqField}),
               z, x, base_ring(x))
   z.prec = x.prec
   return z
end

###############################################################################
#
#   Binary operators
#
###############################################################################

function +(a::fq_default_abs_series, b::fq_default_abs_series)
   check_parent(a, b)
   lena = length(a)
   lenb = length(b)
   prec = min(a.prec, b.prec)
   lena = min(lena, prec)
   lenb = min(lenb, prec)
   lenz = max(lena, lenb)
   z = parent(a)()
   z.prec = prec
   ccall((:fq_default_poly_add_series, libflint), Nothing,
         (Ref{fq_default_abs_series}, Ref{fq_default_abs_series},
          Ref{fq_default_abs_series}, Int, Ref{FqField}),
               z, a, b, lenz, base_ring(a))
   return z
end

function -(a::fq_default_abs_series, b::fq_default_abs_series)
   check_parent(a, b)
   lena = length(a)
   lenb = length(b)
   prec = min(a.prec, b.prec)
   lena = min(lena, prec)
   lenb = min(lenb, prec)
   lenz = max(lena, lenb)
   z = parent(a)()
   z.prec = prec
   ccall((:fq_default_poly_sub_series, libflint), Nothing,
         (Ref{fq_default_abs_series}, Ref{fq_default_abs_series},
          Ref{fq_default_abs_series}, Int, Ref{FqField}),
               z, a, b, lenz, base_ring(a))
   return z
end

function *(a::fq_default_abs_series, b::fq_default_abs_series)
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
   ccall((:fq_default_poly_mullow, libflint), Nothing,
         (Ref{fq_default_abs_series}, Ref{fq_default_abs_series},
          Ref{fq_default_abs_series}, Int, Ref{FqField}),
               z, a, b, lenz, base_ring(a))
   return z
end

###############################################################################
#
#   Ad hoc binary operators
#
###############################################################################

function *(x::FqFieldElem, y::fq_default_abs_series)
   z = parent(y)()
   z.prec = y.prec
   ccall((:fq_default_poly_scalar_mul_fq_default, libflint), Nothing,
         (Ref{fq_default_abs_series}, Ref{fq_default_abs_series}, Ref{FqFieldElem}, Ref{FqField}),
               z, y, x, base_ring(y))
   return z
end

*(x::fq_default_abs_series, y::FqFieldElem) = y * x

###############################################################################
#
#   Shifting
#
###############################################################################

function shift_left(x::fq_default_abs_series, len::Int)
   len < 0 && throw(DomainError(len, "Shift must be non-negative"))
   xlen = length(x)
   z = parent(x)()
   z.prec = x.prec + len
   z.prec = min(z.prec, max_precision(parent(x)))
   zlen = min(z.prec, xlen + len)
   ccall((:fq_default_poly_shift_left, libflint), Nothing,
         (Ref{fq_default_abs_series}, Ref{fq_default_abs_series}, Int, Ref{FqField}),
               z, x, len, base_ring(x))
   ccall((:fq_default_poly_set_trunc, libflint), Nothing,
                (Ref{fq_default_abs_series}, Ref{fq_default_abs_series}, Int, Ref{FqField}),
               z, z, zlen, base_ring(x))
   return z
end

function shift_right(x::fq_default_abs_series, len::Int)
   len < 0 && throw(DomainError(len, "Shift must be non-negative"))
   xlen = length(x)
   z = parent(x)()
   if len >= xlen
      z.prec = max(0, x.prec - len)
   else
      z.prec = x.prec - len
      ccall((:fq_default_poly_shift_right, libflint), Nothing,
            (Ref{fq_default_abs_series}, Ref{fq_default_abs_series}, Int, Ref{FqField}),
               z, x, len, base_ring(x))
   end
   return z
end

###############################################################################
#
#   Truncation
#
###############################################################################

function truncate(x::fq_default_abs_series, prec::Int)
   prec < 0 && throw(DomainError(prec, "Index must be non-negative"))
   if x.prec <= prec
      return x
   end
   z = parent(x)()
   z.prec = prec
   ccall((:fq_default_poly_set_trunc, libflint), Nothing,
         (Ref{fq_default_abs_series}, Ref{fq_default_abs_series}, Int, Ref{FqField}),
               z, x, prec, base_ring(x))
   return z
end

###############################################################################
#
#   Powering
#
###############################################################################

function ^(a::fq_default_abs_series, b::Int)
   b < 0 && throw(DomainError(b, "Exponent must be non-negative"))
   if precision(a) > 0 && is_gen(a) && b > 0
      return shift_left(a, b - 1)
   elseif length(a) == 1
      return parent(a)([coeff(a, 0)^b], 1, a.prec)
   elseif b == 0
      z = one(parent(a))
      z = set_precision!(z, precision(a))
   else
      bit = ~((~UInt(0)) >> 1)
      while (UInt(bit) & b) == 0
         bit >>= 1
      end
      z = a
      bit >>= 1
      while bit !=0
         z = z*z
         if (UInt(bit) & b) != 0
            z *= a
         end
         bit >>= 1
      end
   end
   return z
end

###############################################################################
#
#   Comparison
#
###############################################################################

function ==(x::fq_default_abs_series, y::fq_default_abs_series)
   check_parent(x, y)
   prec = min(x.prec, y.prec)
   n = max(length(x), length(y))
   n = min(n, prec)
   return Bool(ccall((:fq_default_poly_equal_trunc, libflint), Cint,
             (Ref{fq_default_abs_series}, Ref{fq_default_abs_series}, Int, Ref{FqField}),
               x, y, n, base_ring(x)))
end

function isequal(x::fq_default_abs_series, y::fq_default_abs_series)
   if parent(x) != parent(y)
      return false
   end
   if x.prec != y.prec || length(x) != length(y)
      return false
   end
   return Bool(ccall((:fq_default_poly_equal, libflint), Cint,
             (Ref{fq_default_abs_series}, Ref{fq_default_abs_series},
              Ref{FqField}),
              x, y, base_ring(x)))
end

###############################################################################
#
#   Ad hoc comparisons
#
###############################################################################

function ==(x::fq_default_abs_series, y::FqFieldElem)
   if length(x) > 1
      return false
   elseif length(x) == 1
      z = base_ring(x)()
      ccall((:fq_default_poly_get_coeff, libflint), Nothing,
            (Ref{FqFieldElem}, Ref{fq_default_abs_series}, Int, Ref{FqField}),
             z, x, 0, base_ring(x))
      return z == y
   else
      return precision(x) == 0 || iszero(y)
   end
end

==(x::FqFieldElem, y::fq_default_abs_series) = y == x

function ==(x::fq_default_abs_series, y::ZZRingElem)
   if length(x) > 1
      return false
   elseif length(x) == 1
      z = base_ring(x)()
      ccall((:fq_default_poly_get_coeff, libflint), Nothing,
            (Ref{FqFieldElem}, Ref{fq_default_abs_series}, Int, Ref{FqField}),
             z, x, 0, base_ring(x))
      return z == y
   else
      return precision(x) == 0 || iszero(y)
   end
end

==(x::ZZRingElem, y::fq_default_abs_series) = y == x

==(x::fq_default_abs_series, y::Integer) = x == ZZRingElem(y)

==(x::Integer, y::fq_default_abs_series) = y == x

###############################################################################
#
#   Exact division
#
###############################################################################

function divexact(x::fq_default_abs_series, y::fq_default_abs_series; check::Bool=true)
   check_parent(x, y)
   iszero(y) && throw(DivideError())
   v2 = valuation(y)
   v1 = valuation(x)
   if v2 != 0
      if v1 >= v2
         x = shift_right(x, v2)
         y = shift_right(y, v2)
      end
   end
   check && !is_unit(y) && error("Unable to invert power series")
   prec = min(x.prec, y.prec - v2 + v1)
   z = parent(x)()
   z.prec = prec
   ccall((:fq_default_poly_div_series, libflint), Nothing,
         (Ref{fq_default_abs_series}, Ref{fq_default_abs_series},
          Ref{fq_default_abs_series}, Int, Ref{FqField}),
               z, x, y, prec, base_ring(x))
   return z
end

###############################################################################
#
#   Ad hoc exact division
#
###############################################################################

function divexact(x::fq_default_abs_series, y::FqFieldElem; check::Bool=true)
   iszero(y) && throw(DivideError())
   z = parent(x)()
   z.prec = x.prec
   ccall((:fq_default_poly_scalar_div_fq_default, libflint), Nothing,
         (Ref{fq_default_abs_series}, Ref{fq_default_abs_series}, Ref{FqFieldElem}, Ref{FqField}),
               z, x, y, base_ring(x))
   return z
end

###############################################################################
#
#   Inversion
#
###############################################################################

function inv(a::fq_default_abs_series)
   iszero(a) && throw(DivideError())
   !is_unit(a) && error("Unable to invert power series")
   ainv = parent(a)()
   ainv.prec = a.prec
   ccall((:fq_default_poly_inv_series, libflint), Nothing,
         (Ref{fq_default_abs_series}, Ref{fq_default_abs_series}, Int, Ref{FqField}),
               ainv, a, a.prec, base_ring(a))
   return ainv
end

###############################################################################
#
#   Square root
#
###############################################################################

function sqrt_classical_char2(a::fq_default_abs_series; check::Bool=true)
   S = parent(a)
   asqrt = S()
   prec = div(precision(a) + 1, 2)
   asqrt = set_precision!(asqrt, prec)
   if check
      for i = 1:2:precision(a) - 1 # series must have even exponents
         if !iszero(coeff(a, i))
            return false, S()
         end
      end
   end
   for i = 0:prec - 1
      if check
         flag, c = issquare_with_sqrt(coeff(a, 2*i))
         !flag && error("Not a square")
      else
         # degree of finite field could be > 1 so sqrt necessary here
         c = sqrt(coeff(a, 2*i); check=check)
      end
      asqrt = setcoeff!(asqrt, i, c)
   end
   return true, asqrt
end
 
function sqrt_classical(a::fq_default_abs_series; check::Bool=true)
   R = base_ring(a)
   S = parent(a)
   if characteristic(R) == 2
      return sqrt_classical_char2(a; check=check)
   end
   v = valuation(a)
   z = S()
   z.prec = a.prec - div(v, 2)
   if iszero(a)
      return true, z
   end
   if check && !iseven(v)
      return false, S()
   end
   a = shift_right(a, v)
   c = coeff(a, 0)
   if check
      flag, s = issquare_with_sqrt(c)
      if !flag
         return false, S()
      end
   else
      s = sqrt(c; check=check)
   end
   a = divexact(a, c)
   z.prec = a.prec - div(v, 2)
   ccall((:fq_default_poly_sqrt_series, libflint), Nothing,
         (Ref{fq_default_abs_series}, Ref{fq_default_abs_series},
             Int, Ref{FqField}),
                z, a, a.prec, base_ring(a))
   if !isone(s)
      z *= s
   end
   if !iszero(v)
      z = shift_left(z, div(v, 2))
   end
   return true, z
end
 
function Base.sqrt(a::fq_default_abs_series; check::Bool=true)
   flag, s = sqrt_classical(a; check=check)
   check && !flag && error("Not a square")
   return s
end
  
function issquare(a::fq_default_abs_series)
   flag, s = sqrt_classical(a; check=true)
   return flag
end
 
function issquare_with_sqrt(a::fq_default_abs_series)
   return sqrt_classical(a; check=true)
end

###############################################################################
#
#   Unsafe functions
#
###############################################################################

function zero!(z::fq_default_abs_series)
   ccall((:fq_default_poly_zero, libflint), Nothing,
                (Ref{fq_default_abs_series}, Ref{FqField}), z, base_ring(z))
   z.prec = parent(z).prec_max
   return z
end

function fit!(z::fq_default_abs_series, n::Int)
   ccall((:fq_default_poly_fit_length, libflint), Nothing,
         (Ref{fq_default_abs_series}, Int, Ref{FqField}),
         z, n, base_ring(z))
   return nothing
end

function setcoeff!(z::fq_default_abs_series, n::Int, x::FqFieldElem)
   ccall((:fq_default_poly_set_coeff, libflint), Nothing,
                (Ref{fq_default_abs_series}, Int, Ref{FqFieldElem}, Ref{FqField}),
               z, n, x, base_ring(z))
   return z
end

function mul!(z::fq_default_abs_series, a::fq_default_abs_series, b::fq_default_abs_series)
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
   ccall((:fq_default_poly_mullow, libflint), Nothing,
         (Ref{fq_default_abs_series}, Ref{fq_default_abs_series},
          Ref{fq_default_abs_series}, Int, Ref{FqField}),
               z, a, b, lenz, base_ring(z))
   return z
end

function addeq!(a::fq_default_abs_series, b::fq_default_abs_series)
   lena = length(a)
   lenb = length(b)
   prec = min(a.prec, b.prec)
   lena = min(lena, prec)
   lenb = min(lenb, prec)
   lenz = max(lena, lenb)
   a.prec = prec
   ccall((:fq_default_poly_add_series, libflint), Nothing,
         (Ref{fq_default_abs_series}, Ref{fq_default_abs_series},
          Ref{fq_default_abs_series}, Int, Ref{FqField}),
               a, a, b, lenz, base_ring(a))
   return a
end

function add!(c::fq_default_abs_series, a::fq_default_abs_series, b::fq_default_abs_series)
   if c === a
      return addeq!(c, b)
   elseif c === b
      return addeq!(c, a)
   end
   lena = length(a)
   lenb = length(b)
   prec = min(a.prec, b.prec)
   lena = min(lena, prec)
   lenb = min(lenb, prec)
   lenc = max(lena, lenb)
   c.prec = prec
   ccall((:fq_default_poly_add_series, libflint), Nothing,
         (Ref{fq_default_abs_series}, Ref{fq_default_abs_series},
          Ref{fq_default_abs_series}, Int, Ref{FqField}),
               c, a, b, lenc, base_ring(a))
   return c
end

function set_length!(a::fq_default_abs_series, n::Int64)
   ccall((:_fq_default_poly_set_length, libflint), Nothing,
         (Ref{fq_default_abs_series}, Int, Ref{FqField}),
          a, n, base_ring(a))
   return a
end

###############################################################################
#
#   Promotion rules
#
###############################################################################

promote_rule(::Type{fq_default_abs_series}, ::Type{T}) where {T <: Integer} = fq_default_abs_series

promote_rule(::Type{fq_default_abs_series}, ::Type{FqFieldElem}) = fq_default_abs_series

promote_rule(::Type{fq_default_abs_series}, ::Type{ZZRingElem}) = fq_default_abs_series

###############################################################################
#
#   Parent object call overload
#
###############################################################################

function (a::FqDefaultAbsSeriesRing)()
   ctx = base_ring(a)
   z = fq_default_abs_series(ctx)
   z.prec = a.prec_max
   z.parent = a
   return z
end

function (a::FqDefaultAbsSeriesRing)(b::Integer)
   ctx = base_ring(a)
   if b == 0
      z = fq_default_abs_series(ctx)
      z.prec = a.prec_max
   else
      z = fq_default_abs_series(ctx, [base_ring(a)(b)], 1, a.prec_max)
   end
   z.parent = a
   return z
end

function (a::FqDefaultAbsSeriesRing)(b::ZZRingElem)
   ctx = base_ring(a)
   if iszero(b)
      z = fq_default_abs_series(ctx)
      z.prec = a.prec_max
   else
      z = fq_default_abs_series(ctx, [base_ring(a)(b)], 1, a.prec_max)
   end
   z.parent = a
   return z
end

function (a::FqDefaultAbsSeriesRing)(b::FqFieldElem)
   ctx = base_ring(a)
   if iszero(b)
      z = fq_default_abs_series(ctx)
      z.prec = a.prec_max
   else
      z = fq_default_abs_series(ctx, [b], 1, a.prec_max)
   end
   z.parent = a
   return z
end

function (a::FqDefaultAbsSeriesRing)(b::fq_default_abs_series)
   parent(b) != a && error("Unable to coerce power series")
   return b
end

function (a::FqDefaultAbsSeriesRing)(b::Vector{FqFieldElem}, len::Int, prec::Int)
   ctx = base_ring(a)
   z = fq_default_abs_series(ctx, b, len, prec)
   z.parent = a
   return z
end

###############################################################################
#
#   PowerSeriesRing constructor
#
###############################################################################

function PowerSeriesRing(R::FqField, prec::Int, s::AbstractString; model=:capped_relative, cached = true)
   S = Symbol(s)

   if model == :capped_relative
      parent_obj = FqDefaultRelSeriesRing(R, prec, S, cached)
   elseif model == :capped_absolute
      parent_obj = FqDefaultAbsSeriesRing(R, prec, S, cached)
   else
      error("Unknown model")
   end

   return parent_obj, gen(parent_obj)
end
