###############################################################################
#
#   QQRelPowerSeriesRingElem.jl : Power series over flint QQFieldElem rationals
#
###############################################################################

###############################################################################
#
#   Data type and parent object methods
#
###############################################################################

function O(a::QQRelPowerSeriesRingElem)
   val = pol_length(a) + valuation(a) - 1
   val < 0 && throw(DomainError(val, "Valuation must be non-negative"))
   z = QQRelPowerSeriesRingElem(Vector{QQFieldElem}(undef, 0), 0, val, val)
   z.parent = parent(a)
   return z
end

elem_type(::Type{QQRelPowerSeriesRing}) = QQRelPowerSeriesRingElem

parent_type(::Type{QQRelPowerSeriesRingElem}) = QQRelPowerSeriesRing

base_ring(R::QQRelPowerSeriesRing) = QQ

rel_series_type(::Type{QQFieldElem}) = QQRelPowerSeriesRingElem

var(a::QQRelPowerSeriesRing) = a.S

###############################################################################
#
#   Basic manipulation
#
###############################################################################

max_precision(R::QQRelPowerSeriesRing) = R.prec_max

function normalise(a::QQRelPowerSeriesRingElem, len::Int)
   if len > 0
      c = QQFieldElem()
      ccall((:fmpq_poly_get_coeff_fmpq, libflint), Nothing,
         (Ref{QQFieldElem}, Ref{QQRelPowerSeriesRingElem}, Int), c, a, len - 1)
   end
   while len > 0 && iszero(c)
      len -= 1
      if len > 0
         ccall((:fmpq_poly_get_coeff_fmpq, libflint), Nothing,
            (Ref{QQFieldElem}, Ref{QQRelPowerSeriesRingElem}, Int), c, a, len - 1)
      end
   end
   return len
end

function pol_length(x::QQRelPowerSeriesRingElem)
   return ccall((:fmpq_poly_length, libflint), Int, (Ref{QQRelPowerSeriesRingElem},), x)
end

precision(x::QQRelPowerSeriesRingElem) = x.prec

function polcoeff(x::QQRelPowerSeriesRingElem, n::Int)
   if n < 0
      return QQFieldElem(0)
   end
   z = QQFieldElem()
   ccall((:fmpq_poly_get_coeff_fmpq, libflint), Nothing,
         (Ref{QQFieldElem}, Ref{QQRelPowerSeriesRingElem}, Int), z, x, n)
   return z
end

zero(R::QQRelPowerSeriesRing) = R(0)

one(R::QQRelPowerSeriesRing) = R(1)

function gen(R::QQRelPowerSeriesRing)
   z = QQRelPowerSeriesRingElem([QQFieldElem(1)], 1, max_precision(R) + 1, 1)
   z.parent = R
   return z
end

function deepcopy_internal(a::QQRelPowerSeriesRingElem, dict::IdDict)
   z = QQRelPowerSeriesRingElem(a)
   z.prec = a.prec
   z.val = a.val
   z.parent = parent(a)
   return z
end

function renormalize!(z::QQRelPowerSeriesRingElem)
   i = 0
   zlen = pol_length(z)
   zval = valuation(z)
   zprec = precision(z)
   while i < zlen && iszero(polcoeff(z, i))
      i += 1
   end
   z.prec = zprec
   if i == zlen
      z.val = zprec
   else
      z.val = zval + i
      ccall((:fmpq_poly_shift_right, libflint), Nothing,
            (Ref{QQRelPowerSeriesRingElem}, Ref{QQRelPowerSeriesRingElem}, Int), z, z, i)
   end
   return nothing
end

characteristic(::QQRelPowerSeriesRing) = 0

###############################################################################
#
#   Similar
#
###############################################################################

function similar(f::RelPowerSeriesRingElem, R::QQField, max_prec::Int,
                                   s::Symbol=var(parent(f)); cached::Bool=true)
   z = QQRelPowerSeriesRingElem()
   if base_ring(f) === R && s == var(parent(f)) &&
      f isa QQRelPowerSeriesRingElem && max_precision(parent(f)) == max_prec
      # steal parent in case it is not cached
      z.parent = parent(f)
   else
      z.parent = QQRelPowerSeriesRing(max_prec, s, cached)
   end
   z.prec = max_prec
   z.val = max_prec
   return z
end

###############################################################################
#
#   rel_series constructor
#
###############################################################################

function rel_series(R::QQField, arr::Vector{T},
                   len::Int, prec::Int, val::Int, var::VarName=:x;
                            max_precision::Int=prec, cached::Bool=true) where T
   prec < len + val && error("Precision too small for given data")
   coeffs = T == QQFieldElem ? arr : map(R, arr)
   coeffs = length(coeffs) == 0 ? QQFieldElem[] : coeffs
   z = QQRelPowerSeriesRingElem(coeffs, len, prec, val)
   z.parent = QQRelPowerSeriesRing(max_precision, Symbol(var), cached)
   return z
end

###############################################################################
#
#   Unary operators
#
###############################################################################

function -(x::QQRelPowerSeriesRingElem)
   z = parent(x)()
   ccall((:fmpq_poly_neg, libflint), Nothing,
                (Ref{QQRelPowerSeriesRingElem}, Ref{QQRelPowerSeriesRingElem}),
               z, x)
   z.prec = x.prec
   z.val = x.val
   return z
end

###############################################################################
#
#   Binary operators
#
###############################################################################

function +(a::QQRelPowerSeriesRingElem, b::QQRelPowerSeriesRingElem)
   check_parent(a, b)
   lena = pol_length(a)
   lenb = pol_length(b)
   prec = min(a.prec, b.prec)
   val = min(a.val, b.val)
   lena = min(lena, prec - a.val)
   lenb = min(lenb, prec - b.val)
   z = parent(a)()
   if a.val < b.val
      lenz = max(lena, lenb + b.val - a.val)
      ccall((:fmpq_poly_set_trunc, libflint), Nothing,
            (Ref{QQRelPowerSeriesRingElem}, Ref{QQRelPowerSeriesRingElem}, Int),
            z, b, max(0, lenz - b.val + a.val))
      ccall((:fmpq_poly_shift_left, libflint), Nothing,
            (Ref{QQRelPowerSeriesRingElem}, Ref{QQRelPowerSeriesRingElem}, Int),
            z, z, b.val - a.val)
      ccall((:fmpq_poly_add_series, libflint), Nothing,
                (Ref{QQRelPowerSeriesRingElem}, Ref{QQRelPowerSeriesRingElem}, Ref{QQRelPowerSeriesRingElem}, Int),
               z, z, a, lenz)
   elseif b.val < a.val
      lenz = max(lena + a.val - b.val, lenb)
      ccall((:fmpq_poly_set_trunc, libflint), Nothing,
            (Ref{QQRelPowerSeriesRingElem}, Ref{QQRelPowerSeriesRingElem}, Int),
            z, a, max(0, lenz - a.val + b.val))
      ccall((:fmpq_poly_shift_left, libflint), Nothing,
            (Ref{QQRelPowerSeriesRingElem}, Ref{QQRelPowerSeriesRingElem}, Int),
            z, z, a.val - b.val)
      ccall((:fmpq_poly_add_series, libflint), Nothing,
                (Ref{QQRelPowerSeriesRingElem}, Ref{QQRelPowerSeriesRingElem}, Ref{QQRelPowerSeriesRingElem}, Int),
               z, z, b, lenz)
   else
      lenz = max(lena, lenb)
      ccall((:fmpq_poly_add_series, libflint), Nothing,
                (Ref{QQRelPowerSeriesRingElem}, Ref{QQRelPowerSeriesRingElem}, Ref{QQRelPowerSeriesRingElem}, Int),
               z, a, b, lenz)
   end
   z.prec = prec
   z.val = val
   renormalize!(z)
   return z
end

function -(a::QQRelPowerSeriesRingElem, b::QQRelPowerSeriesRingElem)
   check_parent(a, b)
   lena = pol_length(a)
   lenb = pol_length(b)
   prec = min(a.prec, b.prec)
   val = min(a.val, b.val)
   lena = min(lena, prec - a.val)
   lenb = min(lenb, prec - b.val)
   lenz = max(lena, lenb)
   z = parent(a)()
   if a.val < b.val
      lenz = max(lena, lenb + b.val - a.val)
      ccall((:fmpq_poly_set_trunc, libflint), Nothing,
            (Ref{QQRelPowerSeriesRingElem}, Ref{QQRelPowerSeriesRingElem}, Int),
            z, b, max(0, lenz - b.val + a.val))
      ccall((:fmpq_poly_shift_left, libflint), Nothing,
            (Ref{QQRelPowerSeriesRingElem}, Ref{QQRelPowerSeriesRingElem}, Int),
            z, z, b.val - a.val)
      ccall((:fmpq_poly_neg, libflint), Nothing,
            (Ref{QQRelPowerSeriesRingElem}, Ref{QQRelPowerSeriesRingElem}), z, z)
      ccall((:fmpq_poly_add_series, libflint), Nothing,
                (Ref{QQRelPowerSeriesRingElem}, Ref{QQRelPowerSeriesRingElem}, Ref{QQRelPowerSeriesRingElem}, Int),
               z, z, a, lenz)
   elseif b.val < a.val
      lenz = max(lena + a.val - b.val, lenb)
      ccall((:fmpq_poly_set_trunc, libflint), Nothing,
            (Ref{QQRelPowerSeriesRingElem}, Ref{QQRelPowerSeriesRingElem}, Int),
            z, a, max(0, lenz - a.val + b.val))
      ccall((:fmpq_poly_shift_left, libflint), Nothing,
            (Ref{QQRelPowerSeriesRingElem}, Ref{QQRelPowerSeriesRingElem}, Int),
            z, z, a.val - b.val)
      ccall((:fmpq_poly_sub_series, libflint), Nothing,
                (Ref{QQRelPowerSeriesRingElem}, Ref{QQRelPowerSeriesRingElem}, Ref{QQRelPowerSeriesRingElem}, Int),
               z, z, b, lenz)
   else
      lenz = max(lena, lenb)
      ccall((:fmpq_poly_sub_series, libflint), Nothing,
                (Ref{QQRelPowerSeriesRingElem}, Ref{QQRelPowerSeriesRingElem}, Ref{QQRelPowerSeriesRingElem}, Int),
               z, a, b, lenz)
   end
   z.prec = prec
   z.val = val
   renormalize!(z)
   return z
end

function *(a::QQRelPowerSeriesRingElem, b::QQRelPowerSeriesRingElem)
   check_parent(a, b)
   lena = pol_length(a)
   lenb = pol_length(b)
   aval = valuation(a)
   bval = valuation(b)
   prec = min(a.prec - aval, b.prec - bval)
   lena = min(lena, prec)
   lenb = min(lenb, prec)
   z = parent(a)()
   z.val = a.val + b.val
   z.prec = prec + z.val
   if lena == 0 || lenb == 0
      return z
   end
   lenz = min(lena + lenb - 1, prec)
   ccall((:fmpq_poly_mullow, libflint), Nothing,
                (Ref{QQRelPowerSeriesRingElem}, Ref{QQRelPowerSeriesRingElem}, Ref{QQRelPowerSeriesRingElem}, Int),
               z, a, b, lenz)
   return z
end

###############################################################################
#
#   Ad hoc binary operators
#
###############################################################################

function *(x::Int, y::QQRelPowerSeriesRingElem)
   z = parent(y)()
   z.prec = y.prec
   z.val = y.val
   ccall((:fmpq_poly_scalar_mul_si, libflint), Nothing,
                (Ref{QQRelPowerSeriesRingElem}, Ref{QQRelPowerSeriesRingElem}, Int),
               z, y, x)
   return z
end

*(x::QQRelPowerSeriesRingElem, y::Int) = y * x

function *(x::ZZRingElem, y::QQRelPowerSeriesRingElem)
   z = parent(y)()
   z.prec = y.prec
   z.val = y.val
   ccall((:fmpq_poly_scalar_mul_fmpz, libflint), Nothing,
                (Ref{QQRelPowerSeriesRingElem}, Ref{QQRelPowerSeriesRingElem}, Ref{ZZRingElem}),
               z, y, x)
   return z
end

function *(x::QQFieldElem, y::QQRelPowerSeriesRingElem)
   z = parent(y)()
   z.prec = y.prec
   z.val = y.val
   ccall((:fmpq_poly_scalar_mul_fmpq, libflint), Nothing,
                (Ref{QQRelPowerSeriesRingElem}, Ref{QQRelPowerSeriesRingElem}, Ref{QQFieldElem}),
               z, y, x)
   return z
end

*(x::QQRelPowerSeriesRingElem, y::ZZRingElem) = y * x

*(x::QQRelPowerSeriesRingElem, y::QQFieldElem) = y * x

*(x::QQRelPowerSeriesRingElem, y::Integer) = x * ZZRingElem(y)

*(x::Integer, y::QQRelPowerSeriesRingElem) = ZZRingElem(x) * y

*(x::QQRelPowerSeriesRingElem, y::Rational) = x * QQFieldElem(y)

*(x::Rational, y::QQRelPowerSeriesRingElem) = QQFieldElem(x) * y

+(x::QQRelPowerSeriesRingElem, y::Integer) = x + ZZRingElem(y)

+(x::Integer, y::QQRelPowerSeriesRingElem) = ZZRingElem(x) + y

+(x::QQRelPowerSeriesRingElem, y::Rational) = x + QQFieldElem(y)

+(x::Rational, y::QQRelPowerSeriesRingElem) = QQFieldElem(x) + y

-(x::QQRelPowerSeriesRingElem, y::Integer) = x - ZZRingElem(y)

-(x::Integer, y::QQRelPowerSeriesRingElem) = ZZRingElem(x) - y

-(x::QQRelPowerSeriesRingElem, y::Rational) = x - QQFieldElem(y)

-(x::Rational, y::QQRelPowerSeriesRingElem) = QQFieldElem(x) - y

###############################################################################
#
#   Shifting
#
###############################################################################

function shift_left(x::QQRelPowerSeriesRingElem, len::Int)
   len < 0 && throw(DomainError(len, "Shift must be non-negative"))
   xlen = pol_length(x)
   z = QQRelPowerSeriesRingElem(x)
   z.prec = x.prec + len
   z.val = x.val + len
   z.parent = parent(x)
   return z
end

function shift_right(x::QQRelPowerSeriesRingElem, len::Int)
   len < 0 && throw(DomainError(len, "Shift must be non-negative"))
   xlen = pol_length(x)
   xval = valuation(x)
   z = parent(x)()
   if len >= xlen + xval
      z.prec = max(0, x.prec - len)
      z.val = max(0, x.prec - len)
   else
      z.prec = max(0, x.prec - len)
      z.val = max(0, xval - len)
      zlen = min(xlen + xval - len, xlen)
      ccall((:fmpq_poly_shift_right, libflint), Nothing,
                (Ref{QQRelPowerSeriesRingElem}, Ref{QQRelPowerSeriesRingElem}, Int),
               z, x, xlen - zlen)
      renormalize!(z)
   end
   return z
end

###############################################################################
#
#   Truncation
#
###############################################################################

function truncate(x::QQRelPowerSeriesRingElem, prec::Int)
   prec < 0 && throw(DomainError(prec, "Index must be non-negative"))
   xlen = pol_length(x)
   xprec = precision(x)
   xval = valuation(x)
   if xprec + xval <= prec
      return x
   end
   z = parent(x)()
   z.prec = prec
   if prec <= xval
      z = parent(x)()
      z.val = prec
      z.prec = prec
   else
      z.val = xval
      ccall((:fmpq_poly_set_trunc, libflint), Nothing,
                (Ref{QQRelPowerSeriesRingElem}, Ref{QQRelPowerSeriesRingElem}, Int),
               z, x, min(prec - xval, xlen))
   end
   return z
end

###############################################################################
#
#   Powering
#
###############################################################################

function ^(a::QQRelPowerSeriesRingElem, b::Int)
   b < 0 && throw(DomainError(b, "Exponent must be non-negative"))
   if is_gen(a)
      z = parent(a)()
      z = setcoeff!(z, 0, QQFieldElem(1))
      z.prec = a.prec + b - 1
      z.val = b
   elseif pol_length(a) == 0
      z = parent(a)()
      z.prec = b*valuation(a)
      z.val = b*valuation(a)
   elseif pol_length(a) == 1
      return parent(a)([polcoeff(a, 0)^b], 1,
                           (b - 1)*valuation(a) + precision(a), b*valuation(a))
   elseif b == 0
      return one(parent(a))
   else
      bit = ~((~UInt(0)) >> 1)
      while (UInt(bit) & b) == 0
         bit >>= 1
      end
      z = a
      bit >>= 1
      while bit != 0
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

function ==(x::QQRelPowerSeriesRingElem, y::QQRelPowerSeriesRingElem)
   check_parent(x, y)
   prec = min(x.prec, y.prec)
   if prec <= x.val && prec <= y.val
      return true
   end
   if x.val != y.val
      return false
   end
   xlen = normalise(x, min(pol_length(x), prec - x.val))
   ylen = normalise(y, min(pol_length(y), prec - y.val))
   if xlen != ylen
      return false
   end
   return Bool(ccall((:fmpq_poly_equal_trunc, libflint), Cint,
                (Ref{QQRelPowerSeriesRingElem}, Ref{QQRelPowerSeriesRingElem}, Int),
               x, y, xlen))
end

function isequal(x::QQRelPowerSeriesRingElem, y::QQRelPowerSeriesRingElem)
   if parent(x) != parent(y)
      return false
   end
   if x.prec != y.prec || x.val != y.val || pol_length(x) != pol_length(y)
      return false
   end
   return Bool(ccall((:fmpq_poly_equal, libflint), Cint,
                (Ref{QQRelPowerSeriesRingElem}, Ref{QQRelPowerSeriesRingElem}),
               x, y))
end

###############################################################################
#
#   Ad hoc comparisons
#
###############################################################################

function ==(x::QQRelPowerSeriesRingElem, y::QQFieldElem)
   if precision(x) == 0
      return true
   elseif pol_length(x) > 1
      return false
   elseif pol_length(x) == 1
      if x.val == 0
         z = QQFieldElem()
         ccall((:fmpq_poly_get_coeff_fmpq, libflint), Nothing,
                       (Ref{QQFieldElem}, Ref{QQRelPowerSeriesRingElem}, Int), z, x, 0)
         return ccall((:fmpq_equal, libflint), Bool,
               (Ref{QQFieldElem}, Ref{QQFieldElem}, Int), z, y, 0)
      else
         return false
      end
   else
      return iszero(y)
   end
end

function ==(x::QQRelPowerSeriesRingElem, y::ZZRingElem)
   if precision(x) == 0
      return true
   elseif pol_length(x) > 1
      return false
   elseif pol_length(x) == 1
      if x.val == 0
         z = QQFieldElem()
         ccall((:fmpq_poly_get_coeff_fmpq, libflint), Nothing,
                       (Ref{QQFieldElem}, Ref{QQRelPowerSeriesRingElem}, Int), z, x, 0)
         return isone(denominator(z)) && ccall((:fmpz_equal, libflint), Bool,
               (Ref{ZZRingElem}, Ref{ZZRingElem}, Int), numerator(z), y, 0)
      else
         return false
      end
   else
      return iszero(y)
   end
end

==(x::ZZRingElem, y::QQRelPowerSeriesRingElem) = y == x

==(x::QQFieldElem, y::QQRelPowerSeriesRingElem) = y == x

==(x::QQRelPowerSeriesRingElem, y::Integer) = x == ZZRingElem(y)

==(x::Integer, y::QQRelPowerSeriesRingElem) = y == x

==(x::QQRelPowerSeriesRingElem, y::Rational{T}) where T <: Union{Int, BigInt} = x == QQFieldElem(y)

==(x::Rational{T}, y::QQRelPowerSeriesRingElem) where T <: Union{Int, BigInt} = y == x

###############################################################################
#
#   Exact division
#
###############################################################################

function divexact(x::QQRelPowerSeriesRingElem, y::QQRelPowerSeriesRingElem; check::Bool=true)
   check_parent(x, y)
   iszero(y) && throw(DivideError())
   yval = valuation(y)
   xval = valuation(x)
   if yval != 0
      if xval >= yval
         x = shift_right(x, yval)
         y = shift_right(y, yval)
      end
   end
   check && !is_unit(y) && error("Unable to invert power series")
   prec = min(x.prec - x.val, y.prec - y.val)
   z = parent(x)()
   z.val = xval - yval
   z.prec = prec + z.val
   if prec != 0
      ccall((:fmpq_poly_div_series, libflint), Nothing,
                (Ref{QQRelPowerSeriesRingElem}, Ref{QQRelPowerSeriesRingElem}, Ref{QQRelPowerSeriesRingElem}, Int),
               z, x, y, prec)
   end
   return z
end

###############################################################################
#
#   Ad hoc exact division
#
###############################################################################

function divexact(x::QQRelPowerSeriesRingElem, y::Int; check::Bool=true)
   y == 0 && throw(DivideError())
   z = parent(x)()
   z.prec = x.prec
   z.val = x.val
   ccall((:fmpq_poly_scalar_div_si, libflint), Nothing,
                (Ref{QQRelPowerSeriesRingElem}, Ref{QQRelPowerSeriesRingElem}, Int),
               z, x, y)
   return z
end

function divexact(x::QQRelPowerSeriesRingElem, y::ZZRingElem; check::Bool=true)
   iszero(y) && throw(DivideError())
   z = parent(x)()
   z.prec = x.prec
   z.prec = x.prec
   z.val = x.val
   ccall((:fmpq_poly_scalar_div_fmpz, libflint), Nothing,
                (Ref{QQRelPowerSeriesRingElem}, Ref{QQRelPowerSeriesRingElem}, Ref{ZZRingElem}),
               z, x, y)
   return z
end

function divexact(x::QQRelPowerSeriesRingElem, y::QQFieldElem; check::Bool=true)
   iszero(y) && throw(DivideError())
   z = parent(x)()
   z.prec = x.prec
   z.prec = x.prec
   z.val = x.val
   ccall((:fmpq_poly_scalar_div_fmpq, libflint), Nothing,
                (Ref{QQRelPowerSeriesRingElem}, Ref{QQRelPowerSeriesRingElem}, Ref{QQFieldElem}),
               z, x, y)
   return z
end

divexact(x::QQRelPowerSeriesRingElem, y::Integer; check::Bool=true) = divexact(x, ZZRingElem(y); check=check)

divexact(x::QQRelPowerSeriesRingElem, y::Rational{T}; check::Bool=true) where T <: Union{Int, BigInt} = divexact(x, QQFieldElem(y); check=check)

###############################################################################
#
#   Inversion
#
###############################################################################

function inv(a::QQRelPowerSeriesRingElem)
   iszero(a) && throw(DivideError())
   !is_unit(a) && error("Unable to invert power series")
   ainv = parent(a)()
   ainv.prec = a.prec
   ainv.val = 0
   ccall((:fmpq_poly_inv_series, libflint), Nothing,
                (Ref{QQRelPowerSeriesRingElem}, Ref{QQRelPowerSeriesRingElem}, Int),
               ainv, a, a.prec)
   return ainv
end

###############################################################################
#
#   Special functions
#
###############################################################################

function Base.exp(a::QQRelPowerSeriesRingElem)
   (a.val == 0 && pol_length(a) != 0) && error("Constant term not zero in exp")
   if pol_length(a) + valuation(a) == 0 || a.prec == 1
      return parent(a)([QQFieldElem(1)], 1, a.prec, 0)
   end
   z = parent(a)()
   z.prec = a.prec
   z.val = 0
   ccall((:fmpq_poly_shift_left, libflint), Nothing,
                (Ref{QQRelPowerSeriesRingElem}, Ref{QQRelPowerSeriesRingElem}, Int),
               z, a, a.val)
   ccall((:fmpq_poly_exp_series, libflint), Nothing,
                (Ref{QQRelPowerSeriesRingElem}, Ref{QQRelPowerSeriesRingElem}, Int),
               z, z, a.prec)
   renormalize!(z)
   return z
end

function log(a::QQRelPowerSeriesRingElem)
   (a.val != 0 || coeff(a, 0) != 1) && error("Constant term not one in log")
   if pol_length(a) + valuation(a) == 1 || a.prec < 2
      return parent(a)(QQFieldElem[], 0, a.prec, 0)
   end
   z = parent(a)()
   z.prec = a.prec
   z.val = 0
   ccall((:fmpq_poly_log_series, libflint), Nothing,
                (Ref{QQRelPowerSeriesRingElem}, Ref{QQRelPowerSeriesRingElem}, Int),
               z, a, a.prec)
   renormalize!(z)
   return z
end

function tan(a::QQRelPowerSeriesRingElem)
   (a.val == 0 && pol_length(a) != 0) && error("Constant term not zero in tan")
   if iszero(a) || a.prec < 2
      return parent(a)(QQFieldElem[], 0, a.prec, 0)
   end
   z = parent(a)()
   z.prec = a.prec
   z.val = 0
   ccall((:fmpq_poly_shift_left, libflint), Nothing,
                (Ref{QQRelPowerSeriesRingElem}, Ref{QQRelPowerSeriesRingElem}, Int),
               z, a, a.val)
   ccall((:fmpq_poly_tan_series, libflint), Nothing,
                (Ref{QQRelPowerSeriesRingElem}, Ref{QQRelPowerSeriesRingElem}, Int),
               z, z, a.prec)
   renormalize!(z)
   return z
end

function tanh(a::QQRelPowerSeriesRingElem)
   (a.val == 0 && pol_length(a) != 0) && error("Constant term not zero in tanh")
   if iszero(a) || a.prec < 2
      return parent(a)(QQFieldElem[], 0, a.prec, 0)
   end
   z = parent(a)()
   z.prec = a.prec
   z.val = 0
   ccall((:fmpq_poly_shift_left, libflint), Nothing,
                (Ref{QQRelPowerSeriesRingElem}, Ref{QQRelPowerSeriesRingElem}, Int),
               z, a, a.val)
   ccall((:fmpq_poly_tanh_series, libflint), Nothing,
                (Ref{QQRelPowerSeriesRingElem}, Ref{QQRelPowerSeriesRingElem}, Int),
               z, z, a.prec)
   renormalize!(z)
   return z
end

function sin(a::QQRelPowerSeriesRingElem)
   (a.val == 0 && pol_length(a) != 0) && error("Constant term not zero in sin")
   if iszero(a) || a.prec < 2
      return parent(a)(QQFieldElem[], 0, a.prec, 0)
   end
   z = parent(a)()
   z.prec = a.prec
   z.val = 0
   ccall((:fmpq_poly_shift_left, libflint), Nothing,
                (Ref{QQRelPowerSeriesRingElem}, Ref{QQRelPowerSeriesRingElem}, Int),
               z, a, a.val)
   ccall((:fmpq_poly_truncate, libflint), Nothing,
                (Ref{QQRelPowerSeriesRingElem}, Int),
               z, a.prec)
   ccall((:fmpq_poly_sin_series, libflint), Nothing,
                (Ref{QQRelPowerSeriesRingElem}, Ref{QQRelPowerSeriesRingElem}, Int),
               z, z, a.prec)
   renormalize!(z)
   return z
end

function sinh(a::QQRelPowerSeriesRingElem)
   (a.val == 0 && pol_length(a) != 0) && error("Constant term not zero in sinh")
   if iszero(a) || a.prec < 2
      return parent(a)(QQFieldElem[], 0, a.prec, 0)
   end
   z = parent(a)()
   z.prec = a.prec
   z.val = 0
   ccall((:fmpq_poly_shift_left, libflint), Nothing,
                (Ref{QQRelPowerSeriesRingElem}, Ref{QQRelPowerSeriesRingElem}, Int),
               z, a, a.val)
   ccall((:fmpq_poly_sinh_series, libflint), Nothing,
                (Ref{QQRelPowerSeriesRingElem}, Ref{QQRelPowerSeriesRingElem}, Int),
               z, z, a.prec)
   renormalize!(z)
   return z
end

function cos(a::QQRelPowerSeriesRingElem)
   (a.val == 0 && pol_length(a) != 0) && error("Constant term not zero in cos")
   if pol_length(a) + valuation(a) == 0 || a.prec == 1
      return parent(a)(QQFieldElem[1], 1, a.prec, 0)
   end
   z = parent(a)()
   z.prec = a.prec
   z.val = 0
   ccall((:fmpq_poly_shift_left, libflint), Nothing,
                (Ref{QQRelPowerSeriesRingElem}, Ref{QQRelPowerSeriesRingElem}, Int),
               z, a, a.val)
   ccall((:fmpq_poly_truncate, libflint), Nothing,
                (Ref{QQRelPowerSeriesRingElem}, Int),
               z, a.prec)
   ccall((:fmpq_poly_cos_series, libflint), Nothing,
                (Ref{QQRelPowerSeriesRingElem}, Ref{QQRelPowerSeriesRingElem}, Int),
               z, z, a.prec)
   renormalize!(z)
   return z
end

function cosh(a::QQRelPowerSeriesRingElem)
   (a.val == 0 && pol_length(a) != 0) && error("Constant term not zero in cosh")
   if pol_length(a) + valuation(a) == 0 || a.prec == 1
      return parent(a)(QQFieldElem[1], 1, a.prec, 0)
   end
   z = parent(a)()
   z.prec = a.prec
   z.val = 0
   ccall((:fmpq_poly_shift_left, libflint), Nothing,
                (Ref{QQRelPowerSeriesRingElem}, Ref{QQRelPowerSeriesRingElem}, Int),
               z, a, a.val)
   ccall((:fmpq_poly_cosh_series, libflint), Nothing,
                (Ref{QQRelPowerSeriesRingElem}, Ref{QQRelPowerSeriesRingElem}, Int),
               z, z, a.prec)
   renormalize!(z)
   return z
end

function asin(a::QQRelPowerSeriesRingElem)
   (a.val == 0 && pol_length(a) != 0) && error("Constant term not zero in asin")
   if iszero(a) || a.prec < 2
      return parent(a)(QQFieldElem[], 0, a.prec, 0)
   end
   z = parent(a)()
   z.prec = a.prec
   z.val = 0
   ccall((:fmpq_poly_shift_left, libflint), Nothing,
                (Ref{QQRelPowerSeriesRingElem}, Ref{QQRelPowerSeriesRingElem}, Int),
               z, a, a.val)
   ccall((:fmpq_poly_asin_series, libflint), Nothing,
                (Ref{QQRelPowerSeriesRingElem}, Ref{QQRelPowerSeriesRingElem}, Int),
               z, z, a.prec)
   renormalize!(z)
   return z
end

function asinh(a::QQRelPowerSeriesRingElem)
   (a.val == 0 && pol_length(a) != 0) && error("Constant term not zero in asinh")
   if iszero(a) || a.prec < 2
      return parent(a)(QQFieldElem[], 0, a.prec, 0)
   end
   z = parent(a)()
   z.prec = a.prec
   z.val = 0
   ccall((:fmpq_poly_shift_left, libflint), Nothing,
                (Ref{QQRelPowerSeriesRingElem}, Ref{QQRelPowerSeriesRingElem}, Int),
               z, a, a.val)
   ccall((:fmpq_poly_asinh_series, libflint), Nothing,
                (Ref{QQRelPowerSeriesRingElem}, Ref{QQRelPowerSeriesRingElem}, Int),
               z, z, a.prec)
   renormalize!(z)
   return z
end

function atan(a::QQRelPowerSeriesRingElem)
   (a.val == 0 && pol_length(a) != 0) && error("Constant term not zero in atan")
   if iszero(a) || a.prec < 2
      return parent(a)(QQFieldElem[], 0, a.prec, 0)
   end
   z = parent(a)()
   z.prec = a.prec
   z.val = 0
   ccall((:fmpq_poly_shift_left, libflint), Nothing,
                (Ref{QQRelPowerSeriesRingElem}, Ref{QQRelPowerSeriesRingElem}, Int),
               z, a, a.val)
   ccall((:fmpq_poly_atan_series, libflint), Nothing,
                (Ref{QQRelPowerSeriesRingElem}, Ref{QQRelPowerSeriesRingElem}, Int),
               z, z, a.prec)
   renormalize!(z)
   return z
end

function atanh(a::QQRelPowerSeriesRingElem)
   (a.val == 0 && pol_length(a) != 0) && error("Constant term not zero in atanh")
   if iszero(a) || a.prec < 2
      return parent(a)(QQFieldElem[], 0, a.prec, 0)
   end
   z = parent(a)()
   z.prec = a.prec
   z.val = 0
   ccall((:fmpq_poly_shift_left, libflint), Nothing,
                (Ref{QQRelPowerSeriesRingElem}, Ref{QQRelPowerSeriesRingElem}, Int),
               z, a, a.val)
   ccall((:fmpq_poly_atanh_series, libflint), Nothing,
                (Ref{QQRelPowerSeriesRingElem}, Ref{QQRelPowerSeriesRingElem}, Int),
               z, z, a.prec)
   renormalize!(z)
   return z
end

function Base.sqrt(a::QQRelPowerSeriesRingElem; check::Bool=true)
   v = valuation(a)
   z = parent(a)()
   v2 = div(v, 2)
   if iszero(a)
      z.prec = v2
      z.val = v2
      return z
   end
   check && !iseven(v) && error("Not a square")
   z.prec = a.prec - v2
   z.val = v2
   c = coeff(a, v)
   s = sqrt(c; check=check)
   a = divexact(a, c)
   ccall((:fmpq_poly_sqrt_series, libflint), Nothing,
                (Ref{QQRelPowerSeriesRingElem}, Ref{QQRelPowerSeriesRingElem}, Int),
               z, a, a.prec)
   if !isone(s)
      z *= s
   end
   return z
end

###############################################################################
#
#   Unsafe functions
#
###############################################################################

function zero!(z::QQRelPowerSeriesRingElem)
   ccall((:fmpq_poly_zero, libflint), Nothing,
                (Ref{QQRelPowerSeriesRingElem},), z)
   z.prec = parent(z).prec_max
   z.val = parent(z).prec_max
   return z
end

function fit!(z::QQRelPowerSeriesRingElem, n::Int)
   ccall((:fmpq_poly_fit_length, libflint), Nothing,
                 (Ref{QQRelPowerSeriesRingElem}, Int), z, n)
   return nothing
end

function setcoeff!(z::QQRelPowerSeriesRingElem, n::Int, x::QQFieldElem)
   ccall((:fmpq_poly_set_coeff_fmpq, libflint), Nothing,
                (Ref{QQRelPowerSeriesRingElem}, Int, Ref{QQFieldElem}),
               z, n, x)
   return z
end

function mul!(z::QQRelPowerSeriesRingElem, a::QQRelPowerSeriesRingElem, b::QQRelPowerSeriesRingElem)
   lena = pol_length(a)
   lenb = pol_length(b)
   aval = valuation(a)
   bval = valuation(b)
   prec = min(a.prec - aval, b.prec - bval)
   lena = min(lena, prec)
   lenb = min(lenb, prec)
   z.val = a.val + b.val
   z.prec = prec + z.val
   lenz = min(lena + lenb - 1, prec)
   if lena <= 0 || lenb <= 0
      lenz = 0
   end
   ccall((:fmpq_poly_mullow, libflint), Nothing,
                (Ref{QQRelPowerSeriesRingElem}, Ref{QQRelPowerSeriesRingElem}, Ref{QQRelPowerSeriesRingElem}, Int),
               z, a, b, lenz)
   return z
end

function addeq!(a::QQRelPowerSeriesRingElem, b::QQRelPowerSeriesRingElem)
   lena = pol_length(a)
   lenb = pol_length(b)
   prec = min(a.prec, b.prec)
   val = min(a.val, b.val)
   lena = min(lena, prec - a.val)
   lenb = min(lenb, prec - b.val)
   if a.val < b.val
      z = QQRelPowerSeriesRingElem()
      z.parent = parent(a)
      lenz = max(lena, lenb + b.val - a.val)
      ccall((:fmpq_poly_set_trunc, libflint), Nothing,
            (Ref{QQRelPowerSeriesRingElem}, Ref{QQRelPowerSeriesRingElem}, Int),
            z, b, max(0, lenz - b.val + a.val))
      ccall((:fmpq_poly_shift_left, libflint), Nothing,
            (Ref{QQRelPowerSeriesRingElem}, Ref{QQRelPowerSeriesRingElem}, Int),
            z, z, b.val - a.val)
      ccall((:fmpq_poly_add_series, libflint), Nothing,
                (Ref{QQRelPowerSeriesRingElem}, Ref{QQRelPowerSeriesRingElem}, Ref{QQRelPowerSeriesRingElem}, Int),
               a, a, z, lenz)
   elseif b.val < a.val
      lenz = max(lena + a.val - b.val, lenb)
      ccall((:fmpq_poly_truncate, libflint), Nothing,
            (Ref{QQRelPowerSeriesRingElem}, Int),
            a, max(0, lenz - a.val + b.val))
      ccall((:fmpq_poly_shift_left, libflint), Nothing,
            (Ref{QQRelPowerSeriesRingElem}, Ref{QQRelPowerSeriesRingElem}, Int),
            a, a, a.val - b.val)
      ccall((:fmpq_poly_add_series, libflint), Nothing,
                (Ref{QQRelPowerSeriesRingElem}, Ref{QQRelPowerSeriesRingElem}, Ref{QQRelPowerSeriesRingElem}, Int),
               a, a, b, lenz)
   else
      lenz = max(lena, lenb)
      ccall((:fmpq_poly_add_series, libflint), Nothing,
                (Ref{QQRelPowerSeriesRingElem}, Ref{QQRelPowerSeriesRingElem}, Ref{QQRelPowerSeriesRingElem}, Int),
               a, a, b, lenz)
   end
   a.prec = prec
   a.val = val
   renormalize!(a)
   return a
end

function add!(c::QQRelPowerSeriesRingElem, a::QQRelPowerSeriesRingElem, b::QQRelPowerSeriesRingElem)
   if c === a
      return addeq!(c, b)
   elseif c === b
      return addeq!(c, a)
   end
   lena = pol_length(a)
   lenb = pol_length(b)
   prec = min(a.prec, b.prec)
   val = min(a.val, b.val)
   lena = min(lena, prec - a.val)
   lenb = min(lenb, prec - b.val)
   if a.val < b.val
      lenc = max(lena, lenb + b.val - a.val)
      ccall((:fmpq_poly_set_trunc, libflint), Nothing,
            (Ref{QQRelPowerSeriesRingElem}, Ref{QQRelPowerSeriesRingElem}, Int),
            c, b, max(0, lenc - b.val + a.val))
      ccall((:fmpq_poly_shift_left, libflint), Nothing,
            (Ref{QQRelPowerSeriesRingElem}, Ref{QQRelPowerSeriesRingElem}, Int),
            c, c, b.val - a.val)
      ccall((:fmpq_poly_add_series, libflint), Nothing,
                (Ref{QQRelPowerSeriesRingElem}, Ref{QQRelPowerSeriesRingElem}, Ref{QQRelPowerSeriesRingElem}, Int),
               c, c, a, lenc)
   elseif b.val < a.val
      lenc = max(lena + a.val - b.val, lenb)
      ccall((:fmpq_poly_set_trunc, libflint), Nothing,
            (Ref{QQRelPowerSeriesRingElem}, Ref{QQRelPowerSeriesRingElem}, Int),
            c, a, max(0, lenc - a.val + b.val))
      ccall((:fmpq_poly_shift_left, libflint), Nothing,
            (Ref{QQRelPowerSeriesRingElem}, Ref{QQRelPowerSeriesRingElem}, Int),
            c, c, a.val - b.val)
      ccall((:fmpq_poly_add_series, libflint), Nothing,
                (Ref{QQRelPowerSeriesRingElem}, Ref{QQRelPowerSeriesRingElem}, Ref{QQRelPowerSeriesRingElem}, Int),
               c, c, b, lenc)
   else
      lenc = max(lena, lenb)
      ccall((:fmpq_poly_add_series, libflint), Nothing,
                (Ref{QQRelPowerSeriesRingElem}, Ref{QQRelPowerSeriesRingElem}, Ref{QQRelPowerSeriesRingElem}, Int),
               c, a, b, lenc)
   end
   c.prec = prec
   c.val = val
   renormalize!(c)
   return c
end

function set_length!(a::QQRelPowerSeriesRingElem, n::Int)
   ccall((:_fmpq_poly_set_length, libflint), Nothing,
         (Ref{QQRelPowerSeriesRingElem}, Int), a, n)
   return a
end

###############################################################################
#
#   Promotion rules
#
###############################################################################

promote_rule(::Type{QQRelPowerSeriesRingElem}, ::Type{T}) where {T <: Integer} = QQRelPowerSeriesRingElem

promote_rule(::Type{QQRelPowerSeriesRingElem}, ::Type{Rational{T}}) where T <: Union{Int, BigInt} = QQRelPowerSeriesRingElem

promote_rule(::Type{QQRelPowerSeriesRingElem}, ::Type{ZZRingElem}) = QQRelPowerSeriesRingElem

promote_rule(::Type{QQRelPowerSeriesRingElem}, ::Type{QQFieldElem}) = QQRelPowerSeriesRingElem

###############################################################################
#
#   Parent object call overload
#
###############################################################################

function (a::QQRelPowerSeriesRing)()
   z = QQRelPowerSeriesRingElem()
   z.prec = a.prec_max
   z.val = a.prec_max
   z.parent = a
   return z
end

function (a::QQRelPowerSeriesRing)(b::Integer)
   if b == 0
      z = QQRelPowerSeriesRingElem()
      z.prec = a.prec_max
      z.val = a.prec_max
   else
      z = QQRelPowerSeriesRingElem([QQFieldElem(b)], 1, a.prec_max, 0)
   end
   z.parent = a
   return z
end

function (a::QQRelPowerSeriesRing)(b::ZZRingElem)
   if iszero(b)
      z = QQRelPowerSeriesRingElem()
      z.prec = a.prec_max
      z.val = a.prec_max
   else
      z = QQRelPowerSeriesRingElem([QQFieldElem(b)], 1, a.prec_max, 0)
   end
   z.parent = a
   return z
end

function (a::QQRelPowerSeriesRing)(b::QQFieldElem)
   if iszero(b)
      z = QQRelPowerSeriesRingElem()
      z.prec = a.prec_max
      z.val = a.prec_max
   else
      z = QQRelPowerSeriesRingElem([b], 1, a.prec_max, 0)
   end
   z.parent = a
   return z
end

(a::QQRelPowerSeriesRing)(b::Rational{T}) where T <: Union{Int, BigInt} = a(QQFieldElem(b))

function (a::QQRelPowerSeriesRing)(b::QQRelPowerSeriesRingElem)
   parent(b) != a && error("Unable to coerce power series")
   return b
end

function (a::QQRelPowerSeriesRing)(b::Vector{QQFieldElem}, len::Int, prec::Int, val::Int)
   z = QQRelPowerSeriesRingElem(b, len, prec, val)
   z.parent = a
   return z
end

(a::QQRelPowerSeriesRing)(b::Vector{ZZRingElem}, len::Int, prec::Int, val::Int) =
    a(map(QQFieldElem, b), len, prec, val)

(a::QQRelPowerSeriesRing)(b::Vector{T}, len::Int, prec::Int, val::Int) where {T <: Integer} =
    a(map(QQFieldElem, b), len, prec, val)

(a::QQRelPowerSeriesRing)(b::Vector{Rational{T}}, len::Int, prec::Int, val::Int) where {T <: Integer} =
    a(map(QQFieldElem, b), len, prec, val)
