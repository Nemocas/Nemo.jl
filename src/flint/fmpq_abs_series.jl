###############################################################################
#
#   fmpq_abs_series.jl : Power series over flint QQFieldElem rationals (using QQPolyRingElem)
#
###############################################################################

export fmpq_abs_series, FmpqAbsSeriesRing, tan, tanh, sin, sinh, asin, asinh,
       atan, atanh, log

###############################################################################
#
#   Data type and parent object methods
#
###############################################################################

function O(a::fmpq_abs_series)
   if iszero(a)
      return deepcopy(a)    # 0 + O(x^n)
   end
   prec = length(a) - 1
   prec < 0 && throw(DomainError(prec, "Precision must be non-negative"))
   z = parent(a)()
   z.prec = prec
   z.parent = parent(a)
   return z
end

elem_type(::Type{FmpqAbsSeriesRing}) = fmpq_abs_series

parent_type(::Type{fmpq_abs_series}) = FmpqAbsSeriesRing

base_ring(R::FmpqAbsSeriesRing) = R.base_ring

abs_series_type(::Type{QQFieldElem}) = fmpq_abs_series

var(a::FmpqAbsSeriesRing) = a.S

###############################################################################
#
#   Basic manipulation
#
###############################################################################

max_precision(R::FmpqAbsSeriesRing) = R.prec_max

function normalise(a::fmpq_abs_series, len::Int)
   if len > 0
      c = QQFieldElem()
      ccall((:fmpq_poly_get_coeff_fmpq, libflint), Nothing,
         (Ref{QQFieldElem}, Ref{fmpq_abs_series}, Int), c, a, len - 1)
   end
   while len > 0 && iszero(c)
      len -= 1
      if len > 0
         ccall((:fmpq_poly_get_coeff_fmpq, libflint), Nothing,
            (Ref{QQFieldElem}, Ref{fmpq_abs_series}, Int), c, a, len - 1)
      end
   end

   return len
end

function coeff(x::fmpq_abs_series, n::Int)
   if n < 0
      return QQFieldElem(0)
   end
   z = QQFieldElem()
   ccall((:fmpq_poly_get_coeff_fmpq, libflint), Nothing,
         (Ref{QQFieldElem}, Ref{fmpq_abs_series}, Int), z, x, n)
   return z
end

function length(x::fmpq_abs_series)
   return ccall((:fmpq_poly_length, libflint), Int, (Ref{fmpq_abs_series},), x)
end

precision(x::fmpq_abs_series) = x.prec

zero(R::FmpqAbsSeriesRing) = R(0)

one(R::FmpqAbsSeriesRing) = R(1)

function gen(R::FmpqAbsSeriesRing)
   z = fmpq_abs_series([QQFieldElem(0), QQFieldElem(1)], 2, max_precision(R))
   z.parent = R
   return z
end

function deepcopy_internal(a::fmpq_abs_series, dict::IdDict)
   z = fmpq_abs_series(a)
   z.prec = a.prec
   z.parent = parent(a)
   return z
end

function is_gen(a::fmpq_abs_series)
   return precision(a) == 0 || ccall((:fmpq_poly_is_gen, libflint), Bool,
                            (Ref{fmpq_abs_series},), a)
end

iszero(a::fmpq_abs_series) = length(a) == 0

is_unit(a::fmpq_abs_series) = valuation(a) == 0 && is_unit(coeff(a, 0))

function isone(a::fmpq_abs_series)
   return precision(a) == 0 || ccall((:fmpq_poly_is_one, libflint), Bool,
                                (Ref{fmpq_abs_series},), a)
end

# todo: write an fmpq_poly_valuation
function valuation(a::fmpq_abs_series)
   for i = 1:length(a)
      if !iszero(coeff(a, i - 1))
         return i - 1
      end
   end
   return precision(a)
end

characteristic(::FmpqAbsSeriesRing) = 0

###############################################################################
#
#   Similar
#
###############################################################################

function similar(f::AbsSeriesElem, R::QQField, max_prec::Int,
                                   s::Symbol=var(parent(f)); cached::Bool=true)
   z = fmpq_abs_series()
   if base_ring(f) === R && s == var(parent(f)) &&
      typeof(f) == fmpq_abs_series && max_precision(parent(f)) == max_prec
      # steal parent in case it is not cached
      z.parent = parent(f)
   else
      z.parent = FmpqAbsSeriesRing(max_prec, s, cached)
   end
   z.prec = max_prec
   return z
end

###############################################################################
#
#   abs_series constructor
#
###############################################################################

function abs_series(R::QQField, arr::Vector{T},
                           len::Int, prec::Int, var::String="x";
                            max_precision::Int=prec, cached::Bool=true) where T
   prec < len && error("Precision too small for given data")
   coeffs = T == QQFieldElem ? arr : map(R, arr)
   coeffs = length(coeffs) == 0 ? QQFieldElem[] : coeffs
   z = fmpq_abs_series(coeffs, len, prec)
   z.parent = FmpqAbsSeriesRing(max_precision, Symbol(var), cached)
   return z
end

###############################################################################
#
#   AbstractString I/O
#
###############################################################################

function show(io::IO, a::FmpqAbsSeriesRing)
   print(io, "Univariate power series ring in ", var(a), " over ")
   show(io, base_ring(a))
end

###############################################################################
#
#   Unary operators
#
###############################################################################

function -(x::fmpq_abs_series)
   z = parent(x)()
   ccall((:fmpq_poly_neg, libflint), Nothing,
                (Ref{fmpq_abs_series}, Ref{fmpq_abs_series}),
               z, x)
   z.prec = x.prec
   return z
end

###############################################################################
#
#   Binary operators
#
###############################################################################

function +(a::fmpq_abs_series, b::fmpq_abs_series)
   check_parent(a, b)
   lena = length(a)
   lenb = length(b)

   prec = min(a.prec, b.prec)

   lena = min(lena, prec)
   lenb = min(lenb, prec)

   lenz = max(lena, lenb)
   z = parent(a)()
   z.prec = prec
   ccall((:fmpq_poly_add_series, libflint), Nothing,
                (Ref{fmpq_abs_series}, Ref{fmpq_abs_series}, Ref{fmpq_abs_series}, Int),
               z, a, b, lenz)
   return z
end

function -(a::fmpq_abs_series, b::fmpq_abs_series)
   check_parent(a, b)
   lena = length(a)
   lenb = length(b)

   prec = min(a.prec, b.prec)

   lena = min(lena, prec)
   lenb = min(lenb, prec)

   lenz = max(lena, lenb)
   z = parent(a)()
   z.prec = prec
   ccall((:fmpq_poly_sub_series, libflint), Nothing,
                (Ref{fmpq_abs_series}, Ref{fmpq_abs_series}, Ref{fmpq_abs_series}, Int),
               z, a, b, lenz)
   return z
end

function *(a::fmpq_abs_series, b::fmpq_abs_series)
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

   ccall((:fmpq_poly_mullow, libflint), Nothing,
                (Ref{fmpq_abs_series}, Ref{fmpq_abs_series}, Ref{fmpq_abs_series}, Int),
               z, a, b, lenz)
   return z
end


###############################################################################
#
#   Ad hoc binary operators
#
###############################################################################

function *(x::Int, y::fmpq_abs_series)
   z = parent(y)()
   z.prec = y.prec
   ccall((:fmpq_poly_scalar_mul_si, libflint), Nothing,
                (Ref{fmpq_abs_series}, Ref{fmpq_abs_series}, Int),
               z, y, x)
   return z
end

function *(x::ZZRingElem, y::fmpq_abs_series)
   z = parent(y)()
   z.prec = y.prec
   ccall((:fmpq_poly_scalar_mul_fmpz, libflint), Nothing,
                (Ref{fmpq_abs_series}, Ref{fmpq_abs_series}, Ref{ZZRingElem}),
               z, y, x)
   return z
end

function *(x::QQFieldElem, y::fmpq_abs_series)
   z = parent(y)()
   z.prec = y.prec
   ccall((:fmpq_poly_scalar_mul_fmpq, libflint), Nothing,
                (Ref{fmpq_abs_series}, Ref{fmpq_abs_series}, Ref{QQFieldElem}),
               z, y, x)
   return z
end

*(x::fmpq_abs_series, y::Int) = y*x

*(x::fmpq_abs_series, y::ZZRingElem) = y*x

*(x::fmpq_abs_series, y::QQFieldElem) = y*x

*(x::fmpq_abs_series, y::Integer) = x*ZZRingElem(y)

*(x::Integer, y::fmpq_abs_series) = ZZRingElem(x)*y

*(x::fmpq_abs_series, y::Rational) = x*QQFieldElem(y)

*(x::Rational, y::fmpq_abs_series) = QQFieldElem(x)*y

+(x::fmpq_abs_series, y::Rational) = x + QQFieldElem(y)

+(x::Rational, y::fmpq_abs_series) = QQFieldElem(x) + y

-(x::fmpq_abs_series, y::Rational) = x - QQFieldElem(y)

-(x::Rational, y::fmpq_abs_series) = QQFieldElem(x) - y

###############################################################################
#
#   Shifting
#
###############################################################################

function shift_left(x::fmpq_abs_series, len::Int)
   len < 0 && throw(DomainError(len, "Shift must be non-negative"))
   xlen = length(x)
   z = parent(x)()
   z.prec = x.prec + len
   z.prec = min(z.prec, max_precision(parent(x)))
   zlen = min(z.prec, xlen + len)
   ccall((:fmpq_poly_shift_left, libflint), Nothing,
         (Ref{fmpq_abs_series}, Ref{fmpq_abs_series}, Int),
               z, x, len)
   ccall((:fmpq_poly_set_trunc, libflint), Nothing,
         (Ref{fmpq_abs_series}, Ref{fmpq_abs_series}, Int),
               z, z, zlen)
   return z
end

function shift_right(x::fmpq_abs_series, len::Int)
   len < 0 && throw(DomainError(len, "Shift must be non-negative"))
   xlen = length(x)
   z = parent(x)()
   if len >= xlen
      z.prec = max(0, x.prec - len)
   else
      z.prec = x.prec - len
      ccall((:fmpq_poly_shift_right, libflint), Nothing,
                (Ref{fmpq_abs_series}, Ref{fmpq_abs_series}, Int),
               z, x, len)
   end
   return z
end

###############################################################################
#
#   Truncation
#
###############################################################################

function truncate(x::fmpq_abs_series, prec::Int)
   prec < 0 && throw(DomainError(prec, "Precision must be non-negative"))
   if x.prec <= prec
      return x
   end
   z = parent(x)()
   z.prec = prec
   ccall((:fmpq_poly_set_trunc, libflint), Nothing,
                (Ref{fmpq_abs_series}, Ref{fmpq_abs_series}, Int),
               z, x, prec)
   return z
end

###############################################################################
#
#   Powering
#
###############################################################################

function ^(a::fmpq_abs_series, b::Int)
   b < 0 && throw(DomainError(b, "Exponent must be non-negative"))
   # special case powers of x for constructing power series efficiently
   if precision(a) > 0 && is_gen(a) && b > 0
      return shift_left(a, b - 1)
   elseif length(a) == 1
      z = parent(a)(coeff(a, 0)^b)
      z = set_precision!(z, precision(a))
      return z
   elseif b == 0
      z = one(parent(a))
      z = set_precision!(z, precision(a))
      return z
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
      return z
   end
end

###############################################################################
#
#   Comparison
#
###############################################################################

function ==(x::fmpq_abs_series, y::fmpq_abs_series)
   check_parent(x, y)
   prec = min(x.prec, y.prec)

   n = max(length(x), length(y))
   n = min(n, prec)

   return Bool(ccall((:fmpq_poly_equal_trunc, libflint), Cint,
                (Ref{fmpq_abs_series}, Ref{fmpq_abs_series}, Int),
               x, y, n))
end

function isequal(x::fmpq_abs_series, y::fmpq_abs_series)
   if parent(x) != parent(y)
      return false
   end
   if x.prec != y.prec || length(x) != length(y)
      return false
   end
   return Bool(ccall((:fmpq_poly_equal, libflint), Cint,
                (Ref{fmpq_abs_series}, Ref{fmpq_abs_series}),
               x, y))
end

###############################################################################
#
#   Ad hoc comparison
#
###############################################################################

==(x::fmpq_abs_series, y::Rational{T}) where T <: Union{Int, BigInt} = x == QQFieldElem(y)

==(x::fmpq_abs_series, y::Integer) = x == ZZRingElem(y)

==(x::Rational{T}, y::fmpq_abs_series) where T <: Union{Int, BigInt} = y == x

==(x::Integer, y::fmpq_abs_series) = y == x

###############################################################################
#
#   Exact division
#
###############################################################################

function divexact(x::fmpq_abs_series, y::fmpq_abs_series; check::Bool=true)
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
   ccall((:fmpq_poly_div_series, libflint), Nothing,
                (Ref{fmpq_abs_series}, Ref{fmpq_abs_series}, Ref{fmpq_abs_series}, Int),
               z, x, y, prec)
   return z
end

###############################################################################
#
#   Ad hoc exact division
#
###############################################################################

function divexact(x::fmpq_abs_series, y::Int; check::Bool=true)
   y == 0 && throw(DivideError())
   z = parent(x)()
   z.prec = x.prec
   ccall((:fmpq_poly_scalar_div_si, libflint), Nothing,
                (Ref{fmpq_abs_series}, Ref{fmpq_abs_series}, Int),
               z, x, y)
   return z
end

function divexact(x::fmpq_abs_series, y::ZZRingElem; check::Bool=true)
   iszero(y) && throw(DivideError())
   z = parent(x)()
   z.prec = x.prec
   ccall((:fmpq_poly_scalar_div_fmpz, libflint), Nothing,
                (Ref{fmpq_abs_series}, Ref{fmpq_abs_series}, Ref{ZZRingElem}),
               z, x, y)
   return z
end

function divexact(x::fmpq_abs_series, y::QQFieldElem; check::Bool=true)
   iszero(y) && throw(DivideError())
   z = parent(x)()
   z.prec = x.prec
   ccall((:fmpq_poly_scalar_div_fmpq, libflint), Nothing,
                (Ref{fmpq_abs_series}, Ref{fmpq_abs_series}, Ref{QQFieldElem}),
               z, x, y)
   return z
end

divexact(x::fmpq_abs_series, y::Integer; check::Bool=true) = divexact(x, ZZRingElem(y); check=check)

divexact(x::fmpq_abs_series, y::Rational{T}; check::Bool=true) where T <: Union{Int, BigInt} = divexact(x, QQFieldElem(y); check=check)

###############################################################################
#
#   Inversion
#
###############################################################################

function inv(a::fmpq_abs_series)
  iszero(a) && throw(DivideError())
   !is_unit(a) && error("Unable to invert power series")
   ainv = parent(a)()
   ainv.prec = a.prec
   ccall((:fmpq_poly_inv_series, libflint), Nothing,
                (Ref{fmpq_abs_series}, Ref{fmpq_abs_series}, Int),
               ainv, a, a.prec)
   return ainv
end

###############################################################################
#
#   Special functions
#
###############################################################################

function Base.exp(a::fmpq_abs_series)
   !iszero(coeff(a, 0)) && error("Constant term not zero in exp")
   if length(a) == 0 || a.prec == 1
      return parent(a)([QQFieldElem(1)], 1, a.prec)
   end
   z = parent(a)()
   z.prec = a.prec
   ccall((:fmpq_poly_exp_series, libflint), Nothing,
                (Ref{fmpq_abs_series}, Ref{fmpq_abs_series}, Int),
               z, a, a.prec)
   return z
end

function log(a::fmpq_abs_series)
   !isone(coeff(a, 0)) && error("Constant term not one in log")
   if length(a) == 1 || a.prec < 2
      return parent(a)()
   end
   z = parent(a)()
   z.prec = a.prec
   ccall((:fmpq_poly_log_series, libflint), Nothing,
                (Ref{fmpq_abs_series}, Ref{fmpq_abs_series}, Int),
               z, a, a.prec)
   return z
end

function tan(a::fmpq_abs_series)
   !iszero(coeff(a, 0)) && error("Constant term not zero in tan")
   if iszero(a) || a.prec < 2
      return parent(a)()
   end
   z = parent(a)()
   z.prec = a.prec
   ccall((:fmpq_poly_tan_series, libflint), Nothing,
                (Ref{fmpq_abs_series}, Ref{fmpq_abs_series}, Int),
               z, a, a.prec)
   return z
end

function tanh(a::fmpq_abs_series)
   !iszero(coeff(a, 0)) && error("Constant term not zero in tanh")
   if iszero(a) || a.prec < 2
      return parent(a)()
   end
   z = parent(a)()
   z.prec = a.prec
   ccall((:fmpq_poly_tanh_series, libflint), Nothing,
                (Ref{fmpq_abs_series}, Ref{fmpq_abs_series}, Int),
               z, a, a.prec)
   return z
end

function sin(a::fmpq_abs_series)
   !iszero(coeff(a, 0)) && error("Constant term not zero in sin")
   if iszero(a) || a.prec < 2
      return parent(a)()
   end
   z = parent(a)()
   z.prec = a.prec
   ccall((:fmpq_poly_sin_series, libflint), Nothing,
                (Ref{fmpq_abs_series}, Ref{fmpq_abs_series}, Int),
               z, a, a.prec)
   return z
end

function sinh(a::fmpq_abs_series)
   !iszero(coeff(a, 0)) && error("Constant term not zero in sinh")
   if iszero(a) || a.prec < 2
      return parent(a)()
   end
   z = parent(a)()
   z.prec = a.prec
   ccall((:fmpq_poly_sinh_series, libflint), Nothing,
                (Ref{fmpq_abs_series}, Ref{fmpq_abs_series}, Int),
               z, a, a.prec)
   return z
end

function cos(a::fmpq_abs_series)
   !iszero(coeff(a, 0)) && error("Constant term not zero in cos")
   if length(a) == 0 || a.prec == 1
      return one(parent(a))
   end
   z = parent(a)()
   z.prec = a.prec
   ccall((:fmpq_poly_cos_series, libflint), Nothing,
                (Ref{fmpq_abs_series}, Ref{fmpq_abs_series}, Int),
               z, a, a.prec)
   return z
end

function cosh(a::fmpq_abs_series)
   !iszero(coeff(a, 0)) && error("Constant term not zero in cosh")
   if length(a) == 0 || a.prec == 1
      return one(parent(a))
   end
   z = parent(a)()
   z.prec = a.prec
   ccall((:fmpq_poly_cosh_series, libflint), Nothing,
                (Ref{fmpq_abs_series}, Ref{fmpq_abs_series}, Int),
               z, a, a.prec)
   return z
end

function asin(a::fmpq_abs_series)
   !iszero(coeff(a, 0)) && error("Constant term not zero in asin")
   if iszero(a) || a.prec < 2
      return parent(a)()
   end
   z = parent(a)()
   z.prec = a.prec
   ccall((:fmpq_poly_asin_series, libflint), Nothing,
                (Ref{fmpq_abs_series}, Ref{fmpq_abs_series}, Int),
               z, a, a.prec)
   return z
end

function asinh(a::fmpq_abs_series)
   !iszero(coeff(a, 0)) && error("Constant term not zero in asinh")
   if iszero(a) || a.prec < 2
      return parent(a)()
   end
   z = parent(a)()
   z.prec = a.prec
   ccall((:fmpq_poly_asinh_series, libflint), Nothing,
                (Ref{fmpq_abs_series}, Ref{fmpq_abs_series}, Int),
               z, a, a.prec)
   return z
end

function atan(a::fmpq_abs_series)
   !iszero(coeff(a, 0)) && error("Constant term not zero in atan")
   if iszero(a) || a.prec < 2
      return parent(a)()
   end
   z = parent(a)()
   z.prec = a.prec
   ccall((:fmpq_poly_atan_series, libflint), Nothing,
                (Ref{fmpq_abs_series}, Ref{fmpq_abs_series}, Int),
               z, a, a.prec)
   return z
end

function atanh(a::fmpq_abs_series)
   !iszero(coeff(a, 0)) && error("Constant term not zero in atanh")
   if iszero(a) || a.prec < 2
      return parent(a)()
   end
   z = parent(a)()
   z.prec = a.prec
   ccall((:fmpq_poly_atanh_series, libflint), Nothing,
                (Ref{fmpq_abs_series}, Ref{fmpq_abs_series}, Int),
               z, a, a.prec)
   return z
end

function Base.sqrt(a::fmpq_abs_series; check::Bool=true)
   v = valuation(a)
   z = parent(a)()
   z.prec = a.prec - div(v, 2)
   if iszero(a)
      return z
   end
   check && !iseven(v) && error("Not a square")
   a = shift_right(a, v)
   c = coeff(a, 0)
   s = sqrt(c; check=check)
   a = divexact(a, c)
   z.prec = a.prec - div(v, 2)
   ccall((:fmpq_poly_sqrt_series, libflint), Nothing,
                (Ref{fmpq_abs_series}, Ref{fmpq_abs_series}, Int),
               z, a, a.prec)
   if !isone(s)
      z *= s
   end
   if !iszero(v)
      z = shift_left(z, div(v, 2))
   end
   return z
end

###############################################################################
#
#   Unsafe functions
#
###############################################################################

function zero!(z::fmpq_abs_series)
   ccall((:fmpq_poly_zero, libflint), Nothing,
                (Ref{fmpq_abs_series},), z)
   z.prec = parent(z).prec_max
   return z
end

function fit!(z::fmpq_abs_series, n::Int)
   ccall((:fmpq_poly_fit_length, libflint), Nothing,
                (Ref{fmpq_abs_series}, Int), z, n)
   return nothing
end

function setcoeff!(z::fmpq_abs_series, n::Int, x::QQFieldElem)
   ccall((:fmpq_poly_set_coeff_fmpq, libflint), Nothing,
                (Ref{fmpq_abs_series}, Int, Ref{QQFieldElem}),
               z, n, x)
   return z
end

function mul!(z::fmpq_abs_series, a::fmpq_abs_series, b::fmpq_abs_series)
   lena = length(a)
   lenb = length(b)

   aval = valuation(a)
   bval = valuation(b)

   prec = min(a.prec + bval, b.prec + aval)
   prec = min(prec, max_precision(parent(a)))

   lena = min(lena, prec)
   lenb = min(lenb, prec)

   lenz = min(lena + lenb - 1, prec)
   if lenz < 0
      lenz = 0
   end

   z.prec = prec
   ccall((:fmpq_poly_mullow, libflint), Nothing,
                (Ref{fmpq_abs_series}, Ref{fmpq_abs_series}, Ref{fmpq_abs_series}, Int),
               z, a, b, lenz)
   return z
end

function addeq!(a::fmpq_abs_series, b::fmpq_abs_series)
   lena = length(a)
   lenb = length(b)

   prec = min(a.prec, b.prec)

   lena = min(lena, prec)
   lenb = min(lenb, prec)

   lenz = max(lena, lenb)
   a.prec = prec
   ccall((:fmpq_poly_add_series, libflint), Nothing,
                (Ref{fmpq_abs_series}, Ref{fmpq_abs_series},
                 Ref{fmpq_abs_series}, Int),
               a, a, b, lenz)
   return a
end

function add!(c::fmpq_abs_series, a::fmpq_abs_series, b::fmpq_abs_series)
   lena = length(a)
   lenb = length(b)

   prec = min(a.prec, b.prec)

   lena = min(lena, prec)
   lenb = min(lenb, prec)

   lenc = max(lena, lenb)
   c.prec = prec
   ccall((:fmpq_poly_add_series, libflint), Nothing,
                (Ref{fmpq_abs_series}, Ref{fmpq_abs_series},
                 Ref{fmpq_abs_series}, Int),
               c, a, b, lenc)
   return c
end

function set_length!(a::fmpq_abs_series, n::Int)
   ccall((:_fmpq_poly_set_length, libflint), Nothing,
         (Ref{fmpq_abs_series}, Int), a, n)
   return a
end

###############################################################################
#
#   Promotion rules
#
###############################################################################

promote_rule(::Type{fmpq_abs_series}, ::Type{T}) where {T <: Integer} = fmpq_abs_series

promote_rule(::Type{fmpq_abs_series}, ::Type{Rational{T}}) where T <: Union{Int, BigInt} = fmpq_abs_series

promote_rule(::Type{fmpq_abs_series}, ::Type{ZZRingElem}) = fmpq_abs_series

promote_rule(::Type{fmpq_abs_series}, ::Type{QQFieldElem}) = fmpq_abs_series

###############################################################################
#
#   Parent object call overload
#
###############################################################################

function (a::FmpqAbsSeriesRing)()
   z = fmpq_abs_series()
   z.prec = a.prec_max
   z.parent = a
   return z
end

function (a::FmpqAbsSeriesRing)(b::Integer)
   if b == 0
      z = fmpq_abs_series()
      z.prec = a.prec_max
   else
      z = fmpq_abs_series([QQFieldElem(b)], 1, a.prec_max)
   end
   z.parent = a
   return z
end

function (a::FmpqAbsSeriesRing)(b::ZZRingElem)
   if iszero(b)
      z = fmpq_abs_series()
      z.prec = a.prec_max
   else
      z = fmpq_abs_series([QQFieldElem(b)], 1, a.prec_max)
   end
   z.parent = a
   return z
end

function (a::FmpqAbsSeriesRing)(b::QQFieldElem)
   if iszero(b)
      z = fmpq_abs_series()
      z.prec = a.prec_max
   else
      z = fmpq_abs_series([b], 1, a.prec_max)
   end
   z.parent = a
   return z
end

(a::FmpqAbsSeriesRing)(b::Rational{T}) where T <: Union{Int, BigInt} = a(QQFieldElem(b))

function (a::FmpqAbsSeriesRing)(b::fmpq_abs_series)
   parent(b) != a && error("Unable to coerce power series")
   return b
end

function (a::FmpqAbsSeriesRing)(b::Vector{QQFieldElem}, len::Int, prec::Int)
   z = fmpq_abs_series(b, len, prec)
   z.parent = a
   return z
end

###############################################################################
#
#   PowerSeriesRing constructor
#
###############################################################################

function PowerSeriesRing(R::QQField, prec::Int, s::Symbol; model=:capped_relative, cached = true)
   if model == :capped_relative
      parent_obj = FmpqRelSeriesRing(prec, s, cached)
   elseif model == :capped_absolute
      parent_obj = FmpqAbsSeriesRing(prec, s, cached)
   else
      error("Unknown model")
   end

   return parent_obj, gen(parent_obj)
end

function PowerSeriesRing(R::QQField, prec::Int, s::AbstractString; model=:capped_relative, cached = true)
   return PowerSeriesRing(R, prec, Symbol(s); model=model, cached=cached)
end

function AbsSeriesRing(R::QQField, prec::Int)
   return FmpqAbsSeriesRing(prec, :x, false)
end

function RelSeriesRing(R::QQField, prec::Int)
   return FmpqRelSeriesRing(prec, :x, false)
end
