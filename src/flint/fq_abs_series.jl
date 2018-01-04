###############################################################################
#
#   fq_abs_series.jl : Power series over flint fmpz integers
#
###############################################################################

export fq_abs_series, FqAbsSeriesRing, PowerSeriesRing

###############################################################################
#
#   Data type and parent object methods
#
###############################################################################

function O(a::fq_abs_series)
   if iszero(a)
      return deepcopy(a)    # 0 + O(x^n)
   end
   prec = length(a) - 1
   prec < 0 && throw(DomainError())
   z = fq_abs_series(base_ring(a), Array{fq}(0), 0, prec)
   z.parent = parent(a)
   return z
end

elem_type(::Type{FqAbsSeriesRing}) = fq_abs_series

parent_type(::Type{fq_abs_series}) = FqAbsSeriesRing

base_ring(R::FqAbsSeriesRing) = R.base_ring

var(a::FqAbsSeriesRing) = a.S

###############################################################################
#
#   Basic manipulation
#
###############################################################################

max_precision(R::FqAbsSeriesRing) = R.prec_max

function normalise(a::fq_abs_series, len::Int)
   ctx = base_ring(a)
   if len > 0
      c = base_ring(a)()
      ccall((:fq_poly_get_coeff, :libflint), Void,
         (Ref{fq}, Ref{fq_abs_series}, Int, Ref{FqFiniteField}),
          c, a, len - 1, ctx)
   end
   while len > 0 && iszero(c)
      len -= 1
      if len > 0
         ccall((:fq_poly_get_coeff, :libflint), Void,
            (Ref{fq}, Ref{fq_abs_series}, Int, Ref{FqFiniteField}),
             c, a, len - 1, ctx)
      end
   end

   return len
end

function length(x::fq_abs_series)
   return ccall((:fq_poly_length, :libflint), Int,
                (Ref{fq_abs_series}, Ref{FqFiniteField}), x, base_ring(x))
end

precision(x::fq_abs_series) = x.prec

function coeff(x::fq_abs_series, n::Int)
   if n < 0
      return base_ring(x)()
   end
   z = base_ring(x)()
   ccall((:fq_poly_get_coeff, :libflint), Void,
         (Ref{fq}, Ref{fq_abs_series}, Int, Ref{FqFiniteField}),
          z, x, n, base_ring(x))
   return z
end

zero(R::FqAbsSeriesRing) = R(0)

one(R::FqAbsSeriesRing) = R(1)

function gen(R::FqAbsSeriesRing)
   S = base_ring(R)
   z = fq_abs_series(S, [S(0), S(1)], 2, max_precision(R))
   z.parent = R
   return z
end

function deepcopy_internal(a::fq_abs_series, dict::ObjectIdDict)
   z = fq_abs_series(base_ring(a), a)
   z.prec = a.prec
   z.parent = parent(a)
   return z
end

function isgen(a::fq_abs_series)
   return precision(a) == 0 || ccall((:fq_poly_is_gen, :libflint), Bool,
                   (Ref{fq_abs_series}, Ref{FqFiniteField}), a, base_ring(a))
end

iszero(a::fq_abs_series) = length(a) == 0

isunit(a::fq_abs_series) = valuation(a) == 0 && isunit(coeff(a, 0))

function isone(a::fq_abs_series)
   return precision(a) == 0 || ccall((:fq_poly_is_one, :libflint), Bool,
                   (Ref{fq_abs_series}, Ref{FqFiniteField}), a, base_ring(a))
end

# todo: write an fq_poly_valuation
function valuation(a::fq_abs_series)
   for i = 1:length(a)
      if !iszero(coeff(a, i - 1))
         return i - 1
      end
   end
   return precision(a)
end

###############################################################################
#
#   AbstractString I/O
#
###############################################################################

function show(io::IO, a::FqAbsSeriesRing)
   print(io, "Univariate power series ring in ", var(a), " over ")
   show(io, base_ring(a))
end

show_minus_one(::Type{fq_abs_series}) = show_minus_one(fq)

###############################################################################
#
#   Unary operators
#
###############################################################################

function -(x::fq_abs_series)
   z = parent(x)()
   ccall((:fq_poly_neg, :libflint), Void,
                (Ref{fq_abs_series}, Ref{fq_abs_series}, Ref{FqFiniteField}),
               z, x, base_ring(x))
   z.prec = x.prec
   return z
end

###############################################################################
#
#   Binary operators
#
###############################################################################

function +(a::fq_abs_series, b::fq_abs_series)
   check_parent(a, b)
   lena = length(a)
   lenb = length(b)
   prec = min(a.prec, b.prec)
   lena = min(lena, prec)
   lenb = min(lenb, prec)
   lenz = max(lena, lenb)
   z = parent(a)()
   z.prec = prec
   ccall((:fq_poly_add_series, :libflint), Void,
         (Ref{fq_abs_series}, Ref{fq_abs_series},
          Ref{fq_abs_series}, Int, Ref{FqFiniteField}),
               z, a, b, lenz, base_ring(a))
   return z
end

function -(a::fq_abs_series, b::fq_abs_series)
   check_parent(a, b)
   lena = length(a)
   lenb = length(b)
   prec = min(a.prec, b.prec)
   lena = min(lena, prec)
   lenb = min(lenb, prec)
   lenz = max(lena, lenb)
   z = parent(a)()
   z.prec = prec
   ccall((:fq_poly_sub_series, :libflint), Void,
         (Ref{fq_abs_series}, Ref{fq_abs_series},
          Ref{fq_abs_series}, Int, Ref{FqFiniteField}),
               z, a, b, lenz, base_ring(a))
   return z
end

function *(a::fq_abs_series, b::fq_abs_series)
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
   ccall((:fq_poly_mullow, :libflint), Void,
         (Ref{fq_abs_series}, Ref{fq_abs_series},
          Ref{fq_abs_series}, Int, Ref{FqFiniteField}),
               z, a, b, lenz, base_ring(a))
   return z
end

###############################################################################
#
#   Ad hoc binary operators
#
###############################################################################

function *(x::fq, y::fq_abs_series)
   z = parent(y)()
   z.prec = y.prec
   ccall((:fq_poly_scalar_mul_fq, :libflint), Void,
         (Ref{fq_abs_series}, Ref{fq_abs_series}, Ref{fq}, Ref{FqFiniteField}),
               z, y, x, base_ring(y))
   return z
end

*(x::fq_abs_series, y::fq) = y * x

###############################################################################
#
#   Shifting
#
###############################################################################

function shift_left(x::fq_abs_series, len::Int)
   len < 0 && throw(DomainError())
   xlen = length(x)
   z = parent(x)()
   z.prec = x.prec + len
   ccall((:fq_poly_shift_left, :libflint), Void,
         (Ref{fq_abs_series}, Ref{fq_abs_series}, Int, Ref{FqFiniteField}),
               z, x, len, base_ring(x))
   return z
end

function shift_right(x::fq_abs_series, len::Int)
   len < 0 && throw(DomainError())
   xlen = length(x)
   z = parent(x)()
   if len >= xlen
      z.prec = max(0, x.prec - len)
   else
      z.prec = x.prec - len
      ccall((:fq_poly_shift_right, :libflint), Void,
            (Ref{fq_abs_series}, Ref{fq_abs_series}, Int, Ref{FqFiniteField}),
               z, x, len, base_ring(x))
   end
   return z
end

###############################################################################
#
#   Truncation
#
###############################################################################

function truncate(x::fq_abs_series, prec::Int)
   prec < 0 && throw(DomainError())
   if x.prec <= prec
      return x
   end
   z = parent(x)()
   z.prec = prec
   ccall((:fq_poly_set_trunc, :libflint), Void,
         (Ref{fq_abs_series}, Ref{fq_abs_series}, Int, Ref{FqFiniteField}),
               z, x, prec, base_ring(x))
   return z
end

###############################################################################
#
#   Powering
#
###############################################################################

function ^(a::fq_abs_series, b::Int)
   b < 0 && throw(DomainError())
   if precision(a) > 0 && isgen(a) && b > 0
      return shift_left(a, b - 1)
   elseif length(a) == 1
      return parent(a)([coeff(a, 0)^b], 1, a.prec)
   elseif b == 0
      z = one(parent(a))
      set_prec!(z, precision(a))
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

function ==(x::fq_abs_series, y::fq_abs_series)
   check_parent(x, y)
   prec = min(x.prec, y.prec)
   n = max(length(x), length(y))
   n = min(n, prec)
   return Bool(ccall((:fq_poly_equal_trunc, :libflint), Cint,
             (Ref{fq_abs_series}, Ref{fq_abs_series}, Int, Ref{FqFiniteField}),
               x, y, n, base_ring(x)))
end

function isequal(x::fq_abs_series, y::fq_abs_series)
   if parent(x) != parent(y)
      return false
   end
   if x.prec != y.prec || length(x) != length(y)
      return false
   end
   return Bool(ccall((:fq_poly_equal, :libflint), Cint,
             (Ref{fq_abs_series}, Ref{fq_abs_series}, Int, Ref{FqFiniteField}),
               x, y, length(x), base_ring(x)))
end

###############################################################################
#
#   Ad hoc comparisons
#
###############################################################################

function ==(x::fq_abs_series, y::fq)
   if length(x) > 1
      return false
   elseif length(x) == 1
      z = base_ring(x)()
      ccall((:fq_poly_get_coeff, :libflint), Void,
            (Ref{fq}, Ref{fq_abs_series}, Int, Ref{FqFiniteField}),
             z, x, 0, base_ring(x))
      return z == y
   else
      return precision(x) == 0 || iszero(y)
   end
end

==(x::fq, y::fq_abs_series) = y == x

function ==(x::fq_abs_series, y::fmpz)
   if length(x) > 1
      return false
   elseif length(x) == 1
      z = base_ring(x)()
      ccall((:fq_poly_get_coeff, :libflint), Void,
            (Ref{fq}, Ref{fq_abs_series}, Int, Ref{FqFiniteField}),
             z, x, 0, base_ring(x))
      return z == y
   else
      return precision(x) == 0 || iszero(y)
   end
end

==(x::fmpz, y::fq_abs_series) = y == x

==(x::fq_abs_series, y::Integer) = x == fmpz(y)

==(x::Integer, y::fq_abs_series) = y == x

###############################################################################
#
#   Exact division
#
###############################################################################

function divexact(x::fq_abs_series, y::fq_abs_series)
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
   !isunit(y) && error("Unable to invert power series")
   prec = min(x.prec, y.prec - v2 + v1)
   z = parent(x)()
   z.prec = prec
   ccall((:fq_poly_div_series, :libflint), Void,
         (Ref{fq_abs_series}, Ref{fq_abs_series},
          Ref{fq_abs_series}, Int, Ref{FqFiniteField}),
               z, x, y, prec, base_ring(x))
   return z
end

###############################################################################
#
#   Ad hoc exact division
#
###############################################################################

function divexact(x::fq_abs_series, y::fq)
   iszero(y) && throw(DivideError())
   z = parent(x)()
   z.prec = x.prec
   ccall((:fq_poly_scalar_div_fq, :libflint), Void,
         (Ref{fq_abs_series}, Ref{fq_abs_series}, Ref{fq}, Ref{FqFiniteField}),
               z, x, y, base_ring(x))
   return z
end

###############################################################################
#
#   Inversion
#
###############################################################################

function inv(a::fq_abs_series)
   a == 0 && throw(DivideError())
   !isunit(a) && error("Unable to invert power series")
   ainv = parent(a)()
   ainv.prec = a.prec
   ccall((:fq_poly_inv_series, :libflint), Void,
         (Ref{fq_abs_series}, Ref{fq_abs_series}, Int, Ref{FqFiniteField}),
               ainv, a, a.prec, base_ring(a))
   return ainv
end

###############################################################################
#
#   Unsafe functions
#
###############################################################################

function fit!(z::fq_abs_series, n::Int)
   ccall((:fq_poly_fit_length, :libflint), Void,
         (Ref{fq_abs_series}, Int, Ref{FqFiniteField}),
         z, n, base_ring(z))
   return nothing
end

function setcoeff!(z::fq_abs_series, n::Int, x::fq)
   ccall((:fq_poly_set_coeff, :libflint), Void,
                (Ref{fq_abs_series}, Int, Ref{fq}, Ref{FqFiniteField}),
               z, n, x, base_ring(z))
   return z
end

function mul!(z::fq_abs_series, a::fq_abs_series, b::fq_abs_series)
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
   ccall((:fq_poly_mullow, :libflint), Void,
         (Ref{fq_abs_series}, Ref{fq_abs_series},
          Ref{fq_abs_series}, Int, Ref{FqFiniteField}),
               z, a, b, lenz, base_ring(z))
   return z
end

function addeq!(a::fq_abs_series, b::fq_abs_series)
   lena = length(a)
   lenb = length(b)
   prec = min(a.prec, b.prec)
   lena = min(lena, prec)
   lenb = min(lenb, prec)
   lenz = max(lena, lenb)
   a.prec = prec
   ccall((:fq_poly_add_series, :libflint), Void,
         (Ref{fq_abs_series}, Ref{fq_abs_series},
          Ref{fq_abs_series}, Int, Ref{FqFiniteField}),
               a, a, b, lenz, base_ring(a))
   return a
end

###############################################################################
#
#   Promotion rules
#
###############################################################################

promote_rule(::Type{fq_abs_series}, ::Type{T}) where {T <: Integer} = fq_abs_series

promote_rule(::Type{fq_abs_series}, ::Type{fq}) = fq_abs_series

promote_rule(::Type{fq_abs_series}, ::Type{fmpz}) = fq_abs_series

###############################################################################
#
#   Parent object call overload
#
###############################################################################

function (a::FqAbsSeriesRing)()
   ctx = base_ring(a)
   z = fq_abs_series(ctx)
   z.prec = a.prec_max
   z.parent = a
   return z
end

function (a::FqAbsSeriesRing)(b::Integer)
   ctx = base_ring(a)
   if b == 0
      z = fq_abs_series(ctx)
      z.prec = a.prec_max
   else
      z = fq_abs_series(ctx, [base_ring(a)(b)], 1, a.prec_max)
   end
   z.parent = a
   return z
end

function (a::FqAbsSeriesRing)(b::fmpz)
   ctx = base_ring(a)
   if b == 0
      z = fq_abs_series(ctx)
      z.prec = a.prec_max
   else
      z = fq_abs_series(ctx, [base_ring(a)(b)], 1, a.prec_max)
   end
   z.parent = a
   return z
end

function (a::FqAbsSeriesRing)(b::fq)
   ctx = base_ring(a)
   if b == 0
      z = fq_abs_series(ctx)
      z.prec = a.prec_max
   else
      z = fq_abs_series(ctx, [b], 1, a.prec_max)
   end
   z.parent = a
   return z
end

function (a::FqAbsSeriesRing)(b::fq_abs_series)
   parent(b) != a && error("Unable to coerce power series")
   return b
end

function (a::FqAbsSeriesRing)(b::Array{fq, 1}, len::Int, prec::Int)
   ctx = base_ring(a)
   z = fq_abs_series(ctx, b, len, prec)
   z.parent = a
   return z
end

###############################################################################
#
#   PowerSeriesRing constructor
#
###############################################################################

function PowerSeriesRing(R::FqFiniteField, prec::Int, s::AbstractString; model=:capped_relative, cached = true)
   S = Symbol(s)

   if model == :capped_relative
      parent_obj = FqRelSeriesRing(R, prec, S, cached)
   elseif model == :capped_absolute
      parent_obj = FqAbsSeriesRing(R, prec, S, cached)
   else
      error("Unknown model")
   end

   return parent_obj, gen(parent_obj)
end
