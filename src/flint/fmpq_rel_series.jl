###############################################################################
#
#   fmpq_rel_series.jl : Power series over flint fmpq rationals
#
###############################################################################

export fmpq_rel_series, FmpqRelSeriesRing

###############################################################################
#
#   Data type and parent object methods
#
###############################################################################

function O(a::fmpq_rel_series)
   val = pol_length(a) + valuation(a) - 1
   val < 0 && throw(DomainError(val, "Valuation must be non-negative"))
   z = fmpq_rel_series(Vector{fmpq}(undef, 0), 0, val, val)
   z.parent = parent(a)
   return z
end

elem_type(::Type{FmpqRelSeriesRing}) = fmpq_rel_series

parent_type(::Type{fmpq_rel_series}) = FmpqRelSeriesRing

base_ring(R::FmpqRelSeriesRing) = R.base_ring

var(a::FmpqRelSeriesRing) = a.S

###############################################################################
#
#   Basic manipulation
#
###############################################################################

max_precision(R::FmpqRelSeriesRing) = R.prec_max

function normalise(a::fmpq_rel_series, len::Int)
   if len > 0
      c = fmpq()
      ccall((:fmpq_poly_get_coeff_fmpq, libflint), Nothing,
         (Ref{fmpq}, Ref{fmpq_rel_series}, Int), c, a, len - 1)
   end
   while len > 0 && iszero(c)
      len -= 1
      if len > 0
         ccall((:fmpq_poly_get_coeff_fmpq, libflint), Nothing,
            (Ref{fmpq}, Ref{fmpq_rel_series}, Int), c, a, len - 1)
      end
   end
   return len
end

function pol_length(x::fmpq_rel_series)
   return ccall((:fmpq_poly_length, libflint), Int, (Ref{fmpq_rel_series},), x)
end

precision(x::fmpq_rel_series) = x.prec

function polcoeff(x::fmpq_rel_series, n::Int)
   if n < 0
      return fmpq(0)
   end
   z = fmpq()
   ccall((:fmpq_poly_get_coeff_fmpq, libflint), Nothing,
         (Ref{fmpq}, Ref{fmpq_rel_series}, Int), z, x, n)
   return z
end

zero(R::FmpqRelSeriesRing) = R(0)

one(R::FmpqRelSeriesRing) = R(1)

function gen(R::FmpqRelSeriesRing)
   z = fmpq_rel_series([fmpq(1)], 1, max_precision(R) + 1, 1)
   z.parent = R
   return z
end

function deepcopy_internal(a::fmpq_rel_series, dict::IdDict)
   z = fmpq_rel_series(a)
   z.prec = a.prec
   z.val = a.val
   z.parent = parent(a)
   return z
end

function renormalize!(z::fmpq_rel_series)
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
            (Ref{fmpq_rel_series}, Ref{fmpq_rel_series}, Int), z, z, i)
   end
   return nothing
end

characteristic(::FmpqRelSeriesRing) = 0

###############################################################################
#
#   String I/O
#
###############################################################################

function show(io::IO, a::FmpqRelSeriesRing)
   print(io, "Univariate power series ring in ", var(a), " over ")
   show(io, base_ring(a))
end

###############################################################################
#
#   Unary operators
#
###############################################################################

function -(x::fmpq_rel_series)
   z = parent(x)()
   ccall((:fmpq_poly_neg, libflint), Nothing,
                (Ref{fmpq_rel_series}, Ref{fmpq_rel_series}),
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

function +(a::fmpq_rel_series, b::fmpq_rel_series)
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
            (Ref{fmpq_rel_series}, Ref{fmpq_rel_series}, Int),
            z, b, max(0, lenz - b.val + a.val))
      ccall((:fmpq_poly_shift_left, libflint), Nothing,
            (Ref{fmpq_rel_series}, Ref{fmpq_rel_series}, Int),
            z, z, b.val - a.val)
      ccall((:fmpq_poly_add_series, libflint), Nothing,
                (Ref{fmpq_rel_series}, Ref{fmpq_rel_series}, Ref{fmpq_rel_series}, Int),
               z, z, a, lenz)
   elseif b.val < a.val
      lenz = max(lena + a.val - b.val, lenb)
      ccall((:fmpq_poly_set_trunc, libflint), Nothing,
            (Ref{fmpq_rel_series}, Ref{fmpq_rel_series}, Int),
            z, a, max(0, lenz - a.val + b.val))
      ccall((:fmpq_poly_shift_left, libflint), Nothing,
            (Ref{fmpq_rel_series}, Ref{fmpq_rel_series}, Int),
            z, z, a.val - b.val)
      ccall((:fmpq_poly_add_series, libflint), Nothing,
                (Ref{fmpq_rel_series}, Ref{fmpq_rel_series}, Ref{fmpq_rel_series}, Int),
               z, z, b, lenz)
   else
      lenz = max(lena, lenb)
      ccall((:fmpq_poly_add_series, libflint), Nothing,
                (Ref{fmpq_rel_series}, Ref{fmpq_rel_series}, Ref{fmpq_rel_series}, Int),
               z, a, b, lenz)
   end
   z.prec = prec
   z.val = val
   renormalize!(z)
   return z
end

function -(a::fmpq_rel_series, b::fmpq_rel_series)
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
            (Ref{fmpq_rel_series}, Ref{fmpq_rel_series}, Int),
            z, b, max(0, lenz - b.val + a.val))
      ccall((:fmpq_poly_shift_left, libflint), Nothing,
            (Ref{fmpq_rel_series}, Ref{fmpq_rel_series}, Int),
            z, z, b.val - a.val)
      ccall((:fmpq_poly_neg, libflint), Nothing,
            (Ref{fmpq_rel_series}, Ref{fmpq_rel_series}), z, z)
      ccall((:fmpq_poly_add_series, libflint), Nothing,
                (Ref{fmpq_rel_series}, Ref{fmpq_rel_series}, Ref{fmpq_rel_series}, Int),
               z, z, a, lenz)
   elseif b.val < a.val
      lenz = max(lena + a.val - b.val, lenb)
      ccall((:fmpq_poly_set_trunc, libflint), Nothing,
            (Ref{fmpq_rel_series}, Ref{fmpq_rel_series}, Int),
            z, a, max(0, lenz - a.val + b.val))
      ccall((:fmpq_poly_shift_left, libflint), Nothing,
            (Ref{fmpq_rel_series}, Ref{fmpq_rel_series}, Int),
            z, z, a.val - b.val)
      ccall((:fmpq_poly_sub_series, libflint), Nothing,
                (Ref{fmpq_rel_series}, Ref{fmpq_rel_series}, Ref{fmpq_rel_series}, Int),
               z, z, b, lenz)
   else
      lenz = max(lena, lenb)
      ccall((:fmpq_poly_sub_series, libflint), Nothing,
                (Ref{fmpq_rel_series}, Ref{fmpq_rel_series}, Ref{fmpq_rel_series}, Int),
               z, a, b, lenz)
   end
   z.prec = prec
   z.val = val
   renormalize!(z)
   return z
end

function *(a::fmpq_rel_series, b::fmpq_rel_series)
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
                (Ref{fmpq_rel_series}, Ref{fmpq_rel_series}, Ref{fmpq_rel_series}, Int),
               z, a, b, lenz)
   return z
end

###############################################################################
#
#   Ad hoc binary operators
#
###############################################################################

function *(x::Int, y::fmpq_rel_series)
   z = parent(y)()
   z.prec = y.prec
   z.val = y.val
   ccall((:fmpq_poly_scalar_mul_si, libflint), Nothing,
                (Ref{fmpq_rel_series}, Ref{fmpq_rel_series}, Int),
               z, y, x)
   return z
end

*(x::fmpq_rel_series, y::Int) = y * x

function *(x::fmpz, y::fmpq_rel_series)
   z = parent(y)()
   z.prec = y.prec
   z.val = y.val
   ccall((:fmpq_poly_scalar_mul_fmpz, libflint), Nothing,
                (Ref{fmpq_rel_series}, Ref{fmpq_rel_series}, Ref{fmpz}),
               z, y, x)
   return z
end

function *(x::fmpq, y::fmpq_rel_series)
   z = parent(y)()
   z.prec = y.prec
   z.val = y.val
   ccall((:fmpq_poly_scalar_mul_fmpq, libflint), Nothing,
                (Ref{fmpq_rel_series}, Ref{fmpq_rel_series}, Ref{fmpq}),
               z, y, x)
   return z
end

*(x::fmpq_rel_series, y::fmpz) = y * x

*(x::fmpq_rel_series, y::fmpq) = y * x

*(x::fmpq_rel_series, y::Integer) = x * fmpz(y)

*(x::Integer, y::fmpq_rel_series) = fmpz(x) * y

*(x::fmpq_rel_series, y::Rational) = x * fmpq(y)

*(x::Rational, y::fmpq_rel_series) = fmpq(x) * y

+(x::fmpq_rel_series, y::Integer) = x + fmpz(y)

+(x::Integer, y::fmpq_rel_series) = fmpz(x) + y

+(x::fmpq_rel_series, y::Rational) = x + fmpq(y)

+(x::Rational, y::fmpq_rel_series) = fmpq(x) + y

-(x::fmpq_rel_series, y::Integer) = x - fmpz(y)

-(x::Integer, y::fmpq_rel_series) = fmpz(x) - y

-(x::fmpq_rel_series, y::Rational) = x - fmpq(y)

-(x::Rational, y::fmpq_rel_series) = fmpq(x) - y

###############################################################################
#
#   Shifting
#
###############################################################################

function shift_left(x::fmpq_rel_series, len::Int)
   len < 0 && throw(DomainError(len, "Shift must be non-negative"))
   xlen = pol_length(x)
   z = fmpq_rel_series(x)
   z.prec = x.prec + len
   z.val = x.val + len
   z.parent = parent(x)
   return z
end

function shift_right(x::fmpq_rel_series, len::Int)
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
                (Ref{fmpq_rel_series}, Ref{fmpq_rel_series}, Int),
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

function truncate(x::fmpq_rel_series, prec::Int)
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
                (Ref{fmpq_rel_series}, Ref{fmpq_rel_series}, Int),
               z, x, min(prec - xval, xlen))
   end
   return z
end

###############################################################################
#
#   Powering
#
###############################################################################

function ^(a::fmpq_rel_series, b::Int)
   b < 0 && throw(DomainError(b, "Exponent must be non-negative"))
   if isgen(a)
      z = parent(a)()
      z = setcoeff!(z, 0, fmpq(1))
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

function ==(x::fmpq_rel_series, y::fmpq_rel_series)
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
                (Ref{fmpq_rel_series}, Ref{fmpq_rel_series}, Int),
               x, y, xlen))
end

function isequal(x::fmpq_rel_series, y::fmpq_rel_series)
   if parent(x) != parent(y)
      return false
   end
   if x.prec != y.prec || x.val != y.val || pol_length(x) != pol_length(y)
      return false
   end
   return Bool(ccall((:fmpq_poly_equal, libflint), Cint,
                (Ref{fmpq_rel_series}, Ref{fmpq_rel_series}, Int),
               x, y, pol_length(x)))
end

###############################################################################
#
#   Ad hoc comparisons
#
###############################################################################

function ==(x::fmpq_rel_series, y::fmpq)
   if precision(x) == 0
      return true
   elseif pol_length(x) > 1
      return false
   elseif pol_length(x) == 1
      if x.val == 0
         z = fmpq()
         ccall((:fmpq_poly_get_coeff_fmpq, libflint), Nothing,
                       (Ref{fmpq}, Ref{fmpq_rel_series}, Int), z, x, 0)
         return ccall((:fmpq_equal, libflint), Bool,
               (Ref{fmpq}, Ref{fmpq}, Int), z, y, 0)
      else
         return false
      end
   else
      return iszero(y)
   end
end

function ==(x::fmpq_rel_series, y::fmpz)
   if precision(x) == 0
      return true
   elseif pol_length(x) > 1
      return false
   elseif pol_length(x) == 1
      if x.val == 0
         z = fmpq()
         ccall((:fmpq_poly_get_coeff_fmpq, libflint), Nothing,
                       (Ref{fmpq}, Ref{fmpq_rel_series}, Int), z, x, 0)
         return isone(denominator(z)) && ccall((:fmpz_equal, libflint), Bool,
               (Ref{fmpz}, Ref{fmpz}, Int), numerator(z), y, 0)
      else
         return false
      end
   else
      return iszero(y)
   end
end

==(x::fmpz, y::fmpq_rel_series) = y == x

==(x::fmpq, y::fmpq_rel_series) = y == x

==(x::fmpq_rel_series, y::Integer) = x == fmpz(y)

==(x::Integer, y::fmpq_rel_series) = y == x

==(x::fmpq_rel_series, y::Rational{T}) where T <: Union{Int, BigInt} = x == fmpq(y)

==(x::Rational{T}, y::fmpq_rel_series) where T <: Union{Int, BigInt} = y == x

###############################################################################
#
#   Exact division
#
###############################################################################

function divexact(x::fmpq_rel_series, y::fmpq_rel_series)
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
   !isunit(y) && error("Unable to invert power series")
   prec = min(x.prec - x.val, y.prec - y.val)
   z = parent(x)()
   z.val = xval - yval
   z.prec = prec + z.val
   if prec != 0
      ccall((:fmpq_poly_div_series, libflint), Nothing,
                (Ref{fmpq_rel_series}, Ref{fmpq_rel_series}, Ref{fmpq_rel_series}, Int),
               z, x, y, prec)
   end
   return z
end

###############################################################################
#
#   Ad hoc exact division
#
###############################################################################

function divexact(x::fmpq_rel_series, y::Int)
   y == 0 && throw(DivideError())
   z = parent(x)()
   z.prec = x.prec
   z.val = x.val
   ccall((:fmpq_poly_scalar_div_si, libflint), Nothing,
                (Ref{fmpq_rel_series}, Ref{fmpq_rel_series}, Int),
               z, x, y)
   return z
end

function divexact(x::fmpq_rel_series, y::fmpz)
   iszero(y) && throw(DivideError())
   z = parent(x)()
   z.prec = x.prec
   z.prec = x.prec
   z.val = x.val
   ccall((:fmpq_poly_scalar_div_fmpz, libflint), Nothing,
                (Ref{fmpq_rel_series}, Ref{fmpq_rel_series}, Ref{fmpz}),
               z, x, y)
   return z
end

function divexact(x::fmpq_rel_series, y::fmpq)
   iszero(y) && throw(DivideError())
   z = parent(x)()
   z.prec = x.prec
   z.prec = x.prec
   z.val = x.val
   ccall((:fmpq_poly_scalar_div_fmpq, libflint), Nothing,
                (Ref{fmpq_rel_series}, Ref{fmpq_rel_series}, Ref{fmpq}),
               z, x, y)
   return z
end

divexact(x::fmpq_rel_series, y::Integer) = divexact(x, fmpz(y))

divexact(x::fmpq_rel_series, y::Rational{T}) where T <: Union{Int, BigInt} = divexact(x, fmpq(y))

###############################################################################
#
#   Inversion
#
###############################################################################

function inv(a::fmpq_rel_series)
   iszero(a) && throw(DivideError())
   !isunit(a) && error("Unable to invert power series")
   ainv = parent(a)()
   ainv.prec = a.prec
   ainv.val = 0
   ccall((:fmpq_poly_inv_series, libflint), Nothing,
                (Ref{fmpq_rel_series}, Ref{fmpq_rel_series}, Int),
               ainv, a, a.prec)
   return ainv
end

###############################################################################
#
#   Special functions
#
###############################################################################

function Base.exp(a::fmpq_rel_series)
   (a.val == 0 && pol_length(a) != 0) && error("Constant term not zero in exp")
   if pol_length(a) + valuation(a) == 0 || a.prec == 1
      return parent(a)([fmpq(1)], 1, a.prec, 0)
   end
   z = parent(a)()
   z.prec = a.prec
   z.val = 0
   ccall((:fmpq_poly_shift_left, libflint), Nothing,
                (Ref{fmpq_rel_series}, Ref{fmpq_rel_series}, Int),
               z, a, a.val)
   ccall((:fmpq_poly_exp_series, libflint), Nothing,
                (Ref{fmpq_rel_series}, Ref{fmpq_rel_series}, Int),
               z, z, a.prec)
   renormalize!(z)
   return z
end

@doc Markdown.doc"""
    log(a::fmpq_rel_series)

Return log$(a)$. Requires the constant term to be one.
"""
function log(a::fmpq_rel_series)
   (a.val != 0 || coeff(a, 0) != 1) && error("Constant term not one in log")
   if pol_length(a) + valuation(a) == 1 || a.prec < 2
      return parent(a)(fmpq[], 0, a.prec, 0)
   end
   z = parent(a)()
   z.prec = a.prec
   z.val = 0
   ccall((:fmpq_poly_log_series, libflint), Nothing,
                (Ref{fmpq_rel_series}, Ref{fmpq_rel_series}, Int),
               z, a, a.prec)
   renormalize!(z)
   return z
end

@doc Markdown.doc"""
    tan(a::fmpq_rel_series)

Return tan$(a)$. Requires a zero constant term.
"""
function tan(a::fmpq_rel_series)
   (a.val == 0 && pol_length(a) != 0) && error("Constant term not zero in tan")
   if iszero(a) || a.prec < 2
      return parent(a)(fmpq[], 0, a.prec, 0)
   end
   z = parent(a)()
   z.prec = a.prec
   z.val = 0
   ccall((:fmpq_poly_shift_left, libflint), Nothing,
                (Ref{fmpq_rel_series}, Ref{fmpq_rel_series}, Int),
               z, a, a.val)
   ccall((:fmpq_poly_tan_series, libflint), Nothing,
                (Ref{fmpq_rel_series}, Ref{fmpq_rel_series}, Int),
               z, z, a.prec)
   renormalize!(z)
   return z
end

@doc Markdown.doc"""
    tanh(a::fmpq_rel_series)

Return tanh$(a)$. Requires a zero constant term.
"""
function tanh(a::fmpq_rel_series)
   (a.val == 0 && pol_length(a) != 0) && error("Constant term not zero in tanh")
   if iszero(a) || a.prec < 2
      return parent(a)(fmpq[], 0, a.prec, 0)
   end
   z = parent(a)()
   z.prec = a.prec
   z.val = 0
   ccall((:fmpq_poly_shift_left, libflint), Nothing,
                (Ref{fmpq_rel_series}, Ref{fmpq_rel_series}, Int),
               z, a, a.val)
   ccall((:fmpq_poly_tanh_series, libflint), Nothing,
                (Ref{fmpq_rel_series}, Ref{fmpq_rel_series}, Int),
               z, z, a.prec)
   renormalize!(z)
   return z
end

@doc Markdown.doc"""
    sin(a::fmpq_rel_series)

Return sin$(a)$. Requires a zero constant term.
"""
function sin(a::fmpq_rel_series)
   (a.val == 0 && pol_length(a) != 0) && error("Constant term not zero in sin")
   if iszero(a) || a.prec < 2
      return parent(a)(fmpq[], 0, a.prec, 0)
   end
   z = parent(a)()
   z.prec = a.prec
   z.val = 0
   ccall((:fmpq_poly_shift_left, libflint), Nothing,
                (Ref{fmpq_rel_series}, Ref{fmpq_rel_series}, Int),
               z, a, a.val)
   ccall((:fmpq_poly_truncate, libflint), Nothing,
                (Ref{fmpq_rel_series}, Int),
               z, a.prec)
   ccall((:fmpq_poly_sin_series, libflint), Nothing,
                (Ref{fmpq_rel_series}, Ref{fmpq_rel_series}, Int),
               z, z, a.prec)
   renormalize!(z)
   return z
end

@doc Markdown.doc"""
    sinh(a::fmpq_rel_series)

Return sinh$(a)$. Requires a zero constant term.
"""
function sinh(a::fmpq_rel_series)
   (a.val == 0 && pol_length(a) != 0) && error("Constant term not zero in sinh")
   if iszero(a) || a.prec < 2
      return parent(a)(fmpq[], 0, a.prec, 0)
   end
   z = parent(a)()
   z.prec = a.prec
   z.val = 0
   ccall((:fmpq_poly_shift_left, libflint), Nothing,
                (Ref{fmpq_rel_series}, Ref{fmpq_rel_series}, Int),
               z, a, a.val)
   ccall((:fmpq_poly_sinh_series, libflint), Nothing,
                (Ref{fmpq_rel_series}, Ref{fmpq_rel_series}, Int),
               z, z, a.prec)
   renormalize!(z)
   return z
end

@doc Markdown.doc"""
    cos(a::fmpq_rel_series)

Return cos$(a)$. Requires a zero constant term.
"""
function cos(a::fmpq_rel_series)
   (a.val == 0 && pol_length(a) != 0) && error("Constant term not zero in cos")
   if pol_length(a) + valuation(a) == 0 || a.prec == 1
      return parent(a)(fmpq[1], 1, a.prec, 0)
   end
   z = parent(a)()
   z.prec = a.prec
   z.val = 0
   ccall((:fmpq_poly_shift_left, libflint), Nothing,
                (Ref{fmpq_rel_series}, Ref{fmpq_rel_series}, Int),
               z, a, a.val)
   ccall((:fmpq_poly_truncate, libflint), Nothing,
                (Ref{fmpq_rel_series}, Int),
               z, a.prec)
   ccall((:fmpq_poly_cos_series, libflint), Nothing,
                (Ref{fmpq_rel_series}, Ref{fmpq_rel_series}, Int),
               z, z, a.prec)
   renormalize!(z)
   return z
end

@doc Markdown.doc"""
    cosh(a::fmpq_rel_series)

Return cosh$(a)$. Requires a zero constant term.
"""
function cosh(a::fmpq_rel_series)
   (a.val == 0 && pol_length(a) != 0) && error("Constant term not zero in cosh")
   if pol_length(a) + valuation(a) == 0 || a.prec == 1
      return parent(a)(fmpq[1], 1, a.prec, 0)
   end
   z = parent(a)()
   z.prec = a.prec
   z.val = 0
   ccall((:fmpq_poly_shift_left, libflint), Nothing,
                (Ref{fmpq_rel_series}, Ref{fmpq_rel_series}, Int),
               z, a, a.val)
   ccall((:fmpq_poly_cosh_series, libflint), Nothing,
                (Ref{fmpq_rel_series}, Ref{fmpq_rel_series}, Int),
               z, z, a.prec)
   renormalize!(z)
   return z
end

@doc Markdown.doc"""
    asin(a::fmpq_rel_series)

Return asin$(a)$. Requires a zero constant term.
"""
function asin(a::fmpq_rel_series)
   (a.val == 0 && pol_length(a) != 0) && error("Constant term not zero in asin")
   if iszero(a) || a.prec < 2
      return parent(a)(fmpq[], 0, a.prec, 0)
   end
   z = parent(a)()
   z.prec = a.prec
   z.val = 0
   ccall((:fmpq_poly_shift_left, libflint), Nothing,
                (Ref{fmpq_rel_series}, Ref{fmpq_rel_series}, Int),
               z, a, a.val)
   ccall((:fmpq_poly_asin_series, libflint), Nothing,
                (Ref{fmpq_rel_series}, Ref{fmpq_rel_series}, Int),
               z, z, a.prec)
   renormalize!(z)
   return z
end

@doc Markdown.doc"""
    asinh(a::fmpq_rel_series)

Return asinh$(a)$. Requires a zero constant term.
"""
function asinh(a::fmpq_rel_series)
   (a.val == 0 && pol_length(a) != 0) && error("Constant term not zero in asinh")
   if iszero(a) || a.prec < 2
      return parent(a)(fmpq[], 0, a.prec, 0)
   end
   z = parent(a)()
   z.prec = a.prec
   z.val = 0
   ccall((:fmpq_poly_shift_left, libflint), Nothing,
                (Ref{fmpq_rel_series}, Ref{fmpq_rel_series}, Int),
               z, a, a.val)
   ccall((:fmpq_poly_asinh_series, libflint), Nothing,
                (Ref{fmpq_rel_series}, Ref{fmpq_rel_series}, Int),
               z, z, a.prec)
   renormalize!(z)
   return z
end

@doc Markdown.doc"""
    atan(a::fmpq_rel_series)

Return atan$(a)$. Requires a zero constant term.
"""
function atan(a::fmpq_rel_series)
   (a.val == 0 && pol_length(a) != 0) && error("Constant term not zero in atan")
   if iszero(a) || a.prec < 2
      return parent(a)(fmpq[], 0, a.prec, 0)
   end
   z = parent(a)()
   z.prec = a.prec
   z.val = 0
   ccall((:fmpq_poly_shift_left, libflint), Nothing,
                (Ref{fmpq_rel_series}, Ref{fmpq_rel_series}, Int),
               z, a, a.val)
   ccall((:fmpq_poly_atan_series, libflint), Nothing,
                (Ref{fmpq_rel_series}, Ref{fmpq_rel_series}, Int),
               z, z, a.prec)
   renormalize!(z)
   return z
end

@doc Markdown.doc"""
    atanh(a::fmpq_rel_series)

Return atanh$(a)$. Requires a zero constant term.
"""
function atanh(a::fmpq_rel_series)
   (a.val == 0 && pol_length(a) != 0) && error("Constant term not zero in atanh")
   if iszero(a) || a.prec < 2
      return parent(a)(fmpq[], 0, a.prec, 0)
   end
   z = parent(a)()
   z.prec = a.prec
   z.val = 0
   ccall((:fmpq_poly_shift_left, libflint), Nothing,
                (Ref{fmpq_rel_series}, Ref{fmpq_rel_series}, Int),
               z, a, a.val)
   ccall((:fmpq_poly_atanh_series, libflint), Nothing,
                (Ref{fmpq_rel_series}, Ref{fmpq_rel_series}, Int),
               z, z, a.prec)
   renormalize!(z)
   return z
end

@doc Markdown.doc"""
    sqrt(a::fmpq_rel_series)

Return the power series square root of $a$. Requires a constant term equal to
one.
"""
function Base.sqrt(a::fmpq_rel_series)
   (a.val != 0 || coeff(a, 0) != 1) && error("Constant term not one in sqrt")
   z = parent(a)()
   z.prec = a.prec
   z.val = 0
   ccall((:fmpq_poly_sqrt_series, libflint), Nothing,
                (Ref{fmpq_rel_series}, Ref{fmpq_rel_series}, Int),
               z, a, a.prec)
   return z
end

###############################################################################
#
#   Unsafe functions
#
###############################################################################

function zero!(z::fmpq_rel_series)
   ccall((:fmpq_poly_zero, libflint), Nothing,
                (Ref{fmpq_rel_series},), z)
   z.prec = parent(z).prec_max
   return z
end

function setcoeff!(z::fmpq_rel_series, n::Int, x::fmpq)
   ccall((:fmpq_poly_set_coeff_fmpq, libflint), Nothing,
                (Ref{fmpq_rel_series}, Int, Ref{fmpq}),
               z, n, x)
   return z
end

function mul!(z::fmpq_rel_series, a::fmpq_rel_series, b::fmpq_rel_series)
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
                (Ref{fmpq_rel_series}, Ref{fmpq_rel_series}, Ref{fmpq_rel_series}, Int),
               z, a, b, lenz)
   return z
end

function addeq!(a::fmpq_rel_series, b::fmpq_rel_series)
   lena = pol_length(a)
   lenb = pol_length(b)
   prec = min(a.prec, b.prec)
   val = min(a.val, b.val)
   lena = min(lena, prec - a.val)
   lenb = min(lenb, prec - b.val)
   if a.val < b.val
      z = fmpq_rel_series()
      lenz = max(lena, lenb + b.val - a.val)
      ccall((:fmpq_poly_set_trunc, libflint), Nothing,
            (Ref{fmpq_rel_series}, Ref{fmpq_rel_series}, Int),
            z, b, max(0, lenz - b.val + a.val))
      ccall((:fmpq_poly_shift_left, libflint), Nothing,
            (Ref{fmpq_rel_series}, Ref{fmpq_rel_series}, Int),
            z, z, b.val - a.val)
      ccall((:fmpq_poly_add_series, libflint), Nothing,
                (Ref{fmpq_rel_series}, Ref{fmpq_rel_series}, Ref{fmpq_rel_series}, Int),
               a, a, z, lenz)
   elseif b.val < a.val
      lenz = max(lena + a.val - b.val, lenb)
      ccall((:fmpq_poly_truncate, libflint), Nothing,
            (Ref{fmpq_rel_series}, Int),
            a, max(0, lenz - a.val + b.val))
      ccall((:fmpq_poly_shift_left, libflint), Nothing,
            (Ref{fmpq_rel_series}, Ref{fmpq_rel_series}, Int),
            a, a, a.val - b.val)
      ccall((:fmpq_poly_add_series, libflint), Nothing,
                (Ref{fmpq_rel_series}, Ref{fmpq_rel_series}, Ref{fmpq_rel_series}, Int),
               a, a, b, lenz)
   else
      lenz = max(lena, lenb)
      ccall((:fmpq_poly_add_series, libflint), Nothing,
                (Ref{fmpq_rel_series}, Ref{fmpq_rel_series}, Ref{fmpq_rel_series}, Int),
               a, a, b, lenz)
   end
   a.prec = prec
   a.val = val
   renormalize!(a)
   return a
end

function add!(c::fmpq_rel_series, a::fmpq_rel_series, b::fmpq_rel_series)
   lena = pol_length(a)
   lenb = pol_length(b)

   prec = min(a.prec, b.prec)

   lena = min(lena, prec)
   lenb = min(lenb, prec)

   lenc = max(lena, lenb)
   c.prec = prec
   ccall((:fmpq_poly_add_series, libflint), Nothing,
                (Ref{fmpq_rel_series}, Ref{fmpq_rel_series}, Ref{fmpq_rel_series}, Int),
               c, a, b, lenc)
   return c
end

###############################################################################
#
#   Promotion rules
#
###############################################################################

promote_rule(::Type{fmpq_rel_series}, ::Type{T}) where {T <: Integer} = fmpq_rel_series

promote_rule(::Type{fmpq_rel_series}, ::Type{Rational{T}}) where T <: Union{Int, BigInt} = fmpq_rel_series

promote_rule(::Type{fmpq_rel_series}, ::Type{fmpz}) = fmpq_rel_series

promote_rule(::Type{fmpq_rel_series}, ::Type{fmpq}) = fmpq_rel_series

###############################################################################
#
#   Parent object call overload
#
###############################################################################

function (a::FmpqRelSeriesRing)()
   z = fmpq_rel_series()
   z.prec = a.prec_max
   z.val = a.prec_max
   z.parent = a
   return z
end

function (a::FmpqRelSeriesRing)(b::Integer)
   if b == 0
      z = fmpq_rel_series()
      z.prec = a.prec_max
      z.val = a.prec_max
   else
      z = fmpq_rel_series([fmpq(b)], 1, a.prec_max, 0)
   end
   z.parent = a
   return z
end

function (a::FmpqRelSeriesRing)(b::fmpz)
   if iszero(b)
      z = fmpq_rel_series()
      z.prec = a.prec_max
      z.val = a.prec_max
   else
      z = fmpq_rel_series([fmpq(b)], 1, a.prec_max, 0)
   end
   z.parent = a
   return z
end

function (a::FmpqRelSeriesRing)(b::fmpq)
   if iszero(b)
      z = fmpq_rel_series()
      z.prec = a.prec_max
      z.val = a.prec_max
   else
      z = fmpq_rel_series([b], 1, a.prec_max, 0)
   end
   z.parent = a
   return z
end

(a::FmpqRelSeriesRing)(b::Rational{T}) where T <: Union{Int, BigInt} = a(fmpq(b))

function (a::FmpqRelSeriesRing)(b::fmpq_rel_series)
   parent(b) != a && error("Unable to coerce power series")
   return b
end

function (a::FmpqRelSeriesRing)(b::Array{fmpq, 1}, len::Int, prec::Int, val::Int)
   z = fmpq_rel_series(b, len, prec, val)
   z.parent = a
   return z
end

(a::FmpqRelSeriesRing)(b::Array{fmpz, 1}, len::Int, prec::Int, val::Int) =
    a(map(fmpq, b), len, prec, val)

(a::FmpqRelSeriesRing)(b::Array{T, 1}, len::Int, prec::Int, val::Int) where {T <: Integer} =
    a(map(fmpq, b), len, prec, val)

(a::FmpqRelSeriesRing)(b::Array{Rational{T}, 1}, len::Int, prec::Int, val::Int) where {T <: Integer} =
    a(map(fmpq, b), len, prec, val)
