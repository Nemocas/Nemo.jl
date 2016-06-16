###############################################################################
#
#   fmpq_rel_series.jl : Power series over flint fmpq rationals (using fmpq_poly)
#
###############################################################################

export fmpq_rel_series, FmpqRelSeriesRing, tan, tanh, sin, sinh, asin, asinh, atan,
       atanh, sqrt, log

###############################################################################
#
#   Data type and parent object methods
#
###############################################################################

function O(a::fmpq_rel_series)
   prec = length(a) - 1
   prec < 0 && throw(DomainError())
   z = parent(a)()
   z.prec = prec
   z.parent = parent(a)
   return z
end

elem_type(::FmpqRelSeriesRing) = fmpq_rel_series

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
      ccall((:fmpq_poly_get_coeff_fmpq, :libflint), Void, 
         (Ptr{fmpq}, Ptr{fmpq_rel_series}, Int), &c, &a, len - 1)
   end
   while len > 0 && iszero(c)
      len -= 1
      if len > 0
         ccall((:fmpq_poly_get_coeff_fmpq, :libflint), Void, 
            (Ptr{fmpq}, Ptr{fmpq_rel_series}, Int), &c, &a, len - 1)
      end
   end

   return len
end

function coeff(x::fmpq_rel_series, n::Int)
   if n < 0
      return fmpq(0)
   end
   z = fmpq()
   ccall((:fmpq_poly_get_coeff_fmpq, :libflint), Void, 
         (Ptr{fmpq}, Ptr{fmpq_rel_series}, Int), &z, &x, n)
   return z
end

function length(x::fmpq_rel_series)
   return ccall((:fmpq_poly_length, :libflint), Int, (Ptr{fmpq_rel_series},), &x)
end

precision(x::fmpq_rel_series) = x.prec

zero(R::FmpqRelSeriesRing) = R(0)

one(R::FmpqRelSeriesRing) = R(1)

function gen(R::FmpqRelSeriesRing)
   z = fmpq_rel_series([fmpq(0), fmpq(1)], 2, max_precision(R) + 1)
   z.parent = R
   return z
end

function deepcopy(a::fmpq_rel_series)
   z = fmpq_rel_series(a)
   z.prec = a.prec
   z.parent = parent(a)
   return z
end

###############################################################################
#
#   AbstractString{} I/O
#
###############################################################################

function show(io::IO, x::fmpq_rel_series)
   if length(x) == 0
      print(io, "0")
   else
      cstr = ccall((:fmpq_poly_get_str_pretty, :libflint), Ptr{UInt8}, 
        (Ptr{fmpq_rel_series}, Ptr{UInt8}), &x, bytestring(string(var(parent(x)))))

      print(io, bytestring(cstr))

      ccall((:flint_free, :libflint), Void, (Ptr{UInt8},), cstr)
   end
   print(io, "+O(", string(var(parent(x))), "^", x.prec, ")")
end

function show(io::IO, a::FmpqRelSeriesRing)
   print(io, "Univariate power series ring in ", var(a), " over ")
   show(io, base_ring(a))
end

show_minus_one(::Type{fmpq_rel_series}) = show_minus_one(fmpq)

###############################################################################
#
#   Unary operators
#
###############################################################################

function -(x::fmpq_rel_series)
   z = parent(x)()
   ccall((:fmpq_poly_neg, :libflint), Void, 
                (Ptr{fmpq_rel_series}, Ptr{fmpq_rel_series}), 
               &z, &x)
   z.prec = x.prec
   return z
end

###############################################################################
#
#   Binary operators
#
###############################################################################

function +(a::fmpq_rel_series, b::fmpq_rel_series)
   check_parent(a, b)
   lena = length(a)
   lenb = length(b)
         
   prec = min(a.prec, b.prec)
 
   lena = min(lena, prec)
   lenb = min(lenb, prec)

   lenz = max(lena, lenb)
   z = parent(a)()
   z.prec = prec
   ccall((:fmpq_poly_add_series, :libflint), Void, 
                (Ptr{fmpq_rel_series}, Ptr{fmpq_rel_series}, Ptr{fmpq_rel_series}, Int), 
               &z, &a, &b, lenz)
   return z
end

function -(a::fmpq_rel_series, b::fmpq_rel_series)
   check_parent(a, b)
   lena = length(a)
   lenb = length(b)
         
   prec = min(a.prec, b.prec)
 
   lena = min(lena, prec)
   lenb = min(lenb, prec)

   lenz = max(lena, lenb)
   z = parent(a)()
   z.prec = prec
   ccall((:fmpq_poly_sub_series, :libflint), Void, 
                (Ptr{fmpq_rel_series}, Ptr{fmpq_rel_series}, Ptr{fmpq_rel_series}, Int), 
               &z, &a, &b, lenz)
   return z
end

function *(a::fmpq_rel_series, b::fmpq_rel_series)
   check_parent(a, b)
   lena = length(a)
   lenb = length(b)
   
   aval = valuation(a)
   bval = valuation(b)

   prec = min(a.prec + bval, b.prec + aval)
   
   lena = min(lena, prec)
   lenb = min(lenb, prec)
   
   z = parent(a)()
   z.prec = prec
      
   if lena == 0 || lenb == 0
      return z
   end

   lenz = min(lena + lenb - 1, prec)

   ccall((:fmpq_poly_mullow, :libflint), Void, 
                (Ptr{fmpq_rel_series}, Ptr{fmpq_rel_series}, Ptr{fmpq_rel_series}, Int), 
               &z, &a, &b, lenz)
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
   ccall((:fmpq_poly_scalar_mul_si, :libflint), Void, 
                (Ptr{fmpq_rel_series}, Ptr{fmpq_rel_series}, Int), 
               &z, &y, x)
   return z
end

function *(x::fmpz, y::fmpq_rel_series)
   z = parent(y)()
   z.prec = y.prec
   ccall((:fmpq_poly_scalar_mul_fmpz, :libflint), Void, 
                (Ptr{fmpq_rel_series}, Ptr{fmpq_rel_series}, Ptr{fmpz}), 
               &z, &y, &x)
   return z
end

function *(x::fmpq, y::fmpq_rel_series)
   z = parent(y)()
   z.prec = y.prec
   ccall((:fmpq_poly_scalar_mul_fmpq, :libflint), Void, 
                (Ptr{fmpq_rel_series}, Ptr{fmpq_rel_series}, Ptr{fmpq}), 
               &z, &y, &x)
   return z
end

*(x::fmpq_rel_series, y::Int) = y*x

*(x::fmpq_rel_series, y::fmpz) = y*x

*(x::fmpq_rel_series, y::fmpq) = y*x

###############################################################################
#
#   Shifting
#
###############################################################################

function shift_left(x::fmpq_rel_series, len::Int)
   len < 0 && throw(DomainError())
   xlen = length(x)
   z = parent(x)()
   z.prec = x.prec + len
   ccall((:fmpq_poly_shift_left, :libflint), Void, 
                (Ptr{fmpq_rel_series}, Ptr{fmpq_rel_series}, Int), 
               &z, &x, len)
   return z
end

function shift_right(x::fmpq_rel_series, len::Int)
   len < 0 && throw(DomainError())
   xlen = length(x)
   z = parent(x)()
   if len >= xlen
      z.prec = max(0, x.prec - len)
   else
      z.prec = x.prec - len
      ccall((:fmpq_poly_shift_right, :libflint), Void, 
                (Ptr{fmpq_rel_series}, Ptr{fmpq_rel_series}, Int), 
               &z, &x, len)
   end
   return z
end

###############################################################################
#
#   Truncation
#
###############################################################################

function truncate(x::fmpq_rel_series, prec::Int)
   prec < 0 && throw(DomainError())
   if x.prec <= prec
      return x
   end
   z = parent(x)()
   z.prec = prec
   ccall((:fmpq_poly_set_trunc, :libflint), Void, 
                (Ptr{fmpq_rel_series}, Ptr{fmpq_rel_series}, Int), 
               &z, &x, prec)
   return z
end

###############################################################################
#
#   Powering
#
###############################################################################

function ^(a::fmpq_rel_series, b::Int)
   b < 0 && throw(DomainError())
   if isgen(a)
      z = parent(a)()
      setcoeff!(z, b, fmpq(1))
      z.prec = a.prec + b - 1
   elseif length(a) == 0
      z = parent(a)()
      z.prec = a.prec + (b - 1)*valuation(a)
   elseif length(a) == 1
      return parent(a)([coeff(a, 0)^b], 1, a.prec)
   elseif b == 0
      return parent(a)([fmpq(1)], 1, parent(a).prec_max)
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

function ==(x::fmpq_rel_series, y::fmpq_rel_series)
   check_parent(x, y)
   prec = min(x.prec, y.prec)
   
   n = max(length(x), length(y))
   n = min(n, prec)
   
   return Bool(ccall((:fmpq_poly_equal_trunc, :libflint), Cint, 
                (Ptr{fmpq_rel_series}, Ptr{fmpq_rel_series}, Int), 
               &x, &y, n))
end

function isequal(x::fmpq_rel_series, y::fmpq_rel_series)
   if parent(x) != parent(y)
      return false
   end
   if x.prec != y.prec || length(x) != length(y)
      return false
   end
   return Bool(ccall((:fmpq_poly_equal, :libflint), Cint, 
                (Ptr{fmpq_rel_series}, Ptr{fmpq_rel_series}, Int), 
               &x, &y, length(x)))
end

###############################################################################
#
#   Exact division
#
###############################################################################

function divexact(x::fmpq_rel_series, y::fmpq_rel_series)
   check_parent(x, y)
   y == 0 && throw(DivideError())
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
   ccall((:fmpq_poly_div_series, :libflint), Void, 
                (Ptr{fmpq_rel_series}, Ptr{fmpq_rel_series}, Ptr{fmpq_rel_series}, Int), 
               &z, &x, &y, prec)
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
   ccall((:fmpq_poly_scalar_div_si, :libflint), Void, 
                (Ptr{fmpq_rel_series}, Ptr{fmpq_rel_series}, Int), 
               &z, &x, y)
   return z
end

function divexact(x::fmpq_rel_series, y::fmpz)
   y == 0 && throw(DivideError())
   z = parent(x)()
   z.prec = x.prec
   ccall((:fmpq_poly_scalar_div_fmpz, :libflint), Void, 
                (Ptr{fmpq_rel_series}, Ptr{fmpq_rel_series}, Ptr{fmpz}), 
               &z, &x, &y)
   return z
end

function divexact(x::fmpq_rel_series, y::fmpq)
   y == 0 && throw(DivideError())
   z = parent(x)()
   z.prec = x.prec
   ccall((:fmpq_poly_scalar_div_fmpq, :libflint), Void, 
                (Ptr{fmpq_rel_series}, Ptr{fmpq_rel_series}, Ptr{fmpq}), 
               &z, &x, &y)
   return z
end

divexact(x::fmpq_rel_series, y::Integer) = divexact(x, fmpz(y))

###############################################################################
#
#   Inversion
#
###############################################################################

function inv(a::fmpq_rel_series)
   a == 0 && throw(DivideError())
   !isunit(a) && error("Unable to invert power series")
   ainv = parent(a)()
   ainv.prec = a.prec
   ccall((:fmpq_poly_inv_series, :libflint), Void, 
                (Ptr{fmpq_rel_series}, Ptr{fmpq_rel_series}, Int), 
               &ainv, &a, a.prec)
   return ainv
end

###############################################################################
#
#   Special functions
#
###############################################################################

function exp(a::fmpq_rel_series)
   coeff(a, 0) != 0 && error("Constant term not zero in exp")
   if length(a) == 0 || a.prec == 1
      return parent(a)([fmpq(1)], 1, a.prec)
   end
   z = parent(a)()
   z.prec = a.prec
   ccall((:fmpq_poly_exp_series, :libflint), Void, 
                (Ptr{fmpq_rel_series}, Ptr{fmpq_rel_series}, Int), 
               &z, &a, a.prec)
   return z
end

doc"""
   log(a::fmpq_rel_series)
> Return log$(a)$. Requires the constant term to be one.
"""
function log(a::fmpq_rel_series)
   coeff(a, 0) != 1 && error("Constant term not one in log")
   if length(a) == 1 || a.prec < 2
      return parent(a)()
   end
   z = parent(a)()
   z.prec = a.prec
   ccall((:fmpq_poly_log_series, :libflint), Void, 
                (Ptr{fmpq_rel_series}, Ptr{fmpq_rel_series}, Int), 
               &z, &a, a.prec)
   return z
end

doc"""
   tan(a::fmpq_rel_series)
> Return tan$(a)$. Requires a zero constant term.
"""
function tan(a::fmpq_rel_series)
   coeff(a, 0) != 0 && error("Constant term not zero in tan")
   if a == 0 || a.prec < 2
      return parent(a)()
   end
   z = parent(a)()
   z.prec = a.prec
   ccall((:fmpq_poly_tan_series, :libflint), Void, 
                (Ptr{fmpq_rel_series}, Ptr{fmpq_rel_series}, Int), 
               &z, &a, a.prec)
   return z
end

doc"""
   tanh(a::fmpq_rel_series)
> Return tanh$(a)$. Requires a zero constant term.
"""
function tanh(a::fmpq_rel_series)
   coeff(a, 0) != 0 && error("Constant term not zero in tanh")
   if a == 0 || a.prec < 2
      return parent(a)()
   end
   z = parent(a)()
   z.prec = a.prec
   ccall((:fmpq_poly_tanh_series, :libflint), Void, 
                (Ptr{fmpq_rel_series}, Ptr{fmpq_rel_series}, Int), 
               &z, &a, a.prec)
   return z
end

doc"""
   sin(a::fmpq_rel_series)
> Return sin$(a)$. Requires a zero constant term.
"""
function sin(a::fmpq_rel_series)
   coeff(a, 0) != 0 && error("Constant term not zero in sin")
   if a == 0 || a.prec < 2
      return parent(a)()
   end
   z = parent(a)()
   z.prec = a.prec
   ccall((:fmpq_poly_sin_series, :libflint), Void, 
                (Ptr{fmpq_rel_series}, Ptr{fmpq_rel_series}, Int), 
               &z, &a, a.prec)
   return z
end

doc"""
   sinh(a::fmpq_rel_series)
> Return sinh$(a)$. Requires a zero constant term.
"""
function sinh(a::fmpq_rel_series)
   coeff(a, 0) != 0 && error("Constant term not zero in sinh")
   if a == 0 || a.prec < 2
      return parent(a)()
   end
   z = parent(a)()
   z.prec = a.prec
   ccall((:fmpq_poly_sinh_series, :libflint), Void, 
                (Ptr{fmpq_rel_series}, Ptr{fmpq_rel_series}, Int), 
               &z, &a, a.prec)
   return z
end

doc"""
   cos(a::fmpq_rel_series)
> Return cos$(a)$. Requires a zero constant term.
"""
function cos(a::fmpq_rel_series)
   coeff(a, 0) != 0 && error("Constant term not zero in cos")
   if length(a) == 0 || a.prec == 1
      return one(parent(a))
   end
   z = parent(a)()
   z.prec = a.prec
   ccall((:fmpq_poly_cos_series, :libflint), Void, 
                (Ptr{fmpq_rel_series}, Ptr{fmpq_rel_series}, Int), 
               &z, &a, a.prec)
   return z
end

doc"""
   cosh(a::fmpq_rel_series)
> Return cosh$(a)$. Requires a zero constant term.
"""
function cosh(a::fmpq_rel_series)
   coeff(a, 0) != 0 && error("Constant term not zero in cosh")
   if length(a) == 0 || a.prec == 1
      return one(parent(a))
   end
   z = parent(a)()
   z.prec = a.prec
   ccall((:fmpq_poly_cosh_series, :libflint), Void, 
                (Ptr{fmpq_rel_series}, Ptr{fmpq_rel_series}, Int), 
               &z, &a, a.prec)
   return z
end

doc"""
   asin(a::fmpq_rel_series)
> Return asin$(a)$. Requires a zero constant term.
"""
function asin(a::fmpq_rel_series)
   coeff(a, 0) != 0 && error("Constant term not zero in asin")
   if a == 0 || a.prec < 2
      return parent(a)()
   end
   z = parent(a)()
   z.prec = a.prec
   ccall((:fmpq_poly_asin_series, :libflint), Void, 
                (Ptr{fmpq_rel_series}, Ptr{fmpq_rel_series}, Int), 
               &z, &a, a.prec)
   return z
end

doc"""
   asinh(a::fmpq_rel_series)
> Return asinh$(a)$. Requires a zero constant term.
"""
function asinh(a::fmpq_rel_series)
   coeff(a, 0) != 0 && error("Constant term not zero in asinh")
   if a == 0 || a.prec < 2
      return parent(a)()
   end
   z = parent(a)()
   z.prec = a.prec
   ccall((:fmpq_poly_asinh_series, :libflint), Void, 
                (Ptr{fmpq_rel_series}, Ptr{fmpq_rel_series}, Int), 
               &z, &a, a.prec)
   return z
end

doc"""
   atan(a::fmpq_rel_series)
> Return atan$(a)$. Requires a zero constant term.
"""
function atan(a::fmpq_rel_series)
   coeff(a, 0) != 0 && error("Constant term not zero in atan")
   if a == 0 || a.prec < 2
      return parent(a)()
   end
   z = parent(a)()
   z.prec = a.prec
   ccall((:fmpq_poly_atan_series, :libflint), Void, 
                (Ptr{fmpq_rel_series}, Ptr{fmpq_rel_series}, Int), 
               &z, &a, a.prec)
   return z
end

doc"""
   atanh(a::fmpq_rel_series)
> Return atanh$(a)$. Requires a zero constant term.
"""
function atanh(a::fmpq_rel_series)
   coeff(a, 0) != 0 && error("Constant term not zero in atanh")
   if a == 0 || a.prec < 2
      return parent(a)()
   end
   z = parent(a)()
   z.prec = a.prec
   ccall((:fmpq_poly_atanh_series, :libflint), Void, 
                (Ptr{fmpq_rel_series}, Ptr{fmpq_rel_series}, Int), 
               &z, &a, a.prec)
   return z
end

doc"""
   sqrt(a::fmpq_rel_series)
> Return the power series square root of $a$. Requires a constant term equal to
> one.
"""
function sqrt(a::fmpq_rel_series)
   coeff(a, 0) != 1 && error("Constant term not one in sqrt")
   z = parent(a)()
   z.prec = a.prec
   ccall((:fmpq_poly_sqrt_series, :libflint), Void, 
                (Ptr{fmpq_rel_series}, Ptr{fmpq_rel_series}, Int), 
               &z, &a, a.prec)
   return z
end

###############################################################################
#
#   Unsafe functions
#
###############################################################################

function setcoeff!(z::fmpq_rel_series, n::Int, x::fmpq)
   ccall((:fmpq_poly_set_coeff_fmpq, :libflint), Void, 
                (Ptr{fmpq_rel_series}, Int, Ptr{fmpq}), 
               &z, n, &x)
end

function mul!(z::fmpq_rel_series, a::fmpq_rel_series, b::fmpq_rel_series)
   lena = length(a)
   lenb = length(b)
   
   aval = valuation(a)
   bval = valuation(b)

   prec = min(a.prec + bval, b.prec + aval)
   
   lena = min(lena, prec)
   lenb = min(lenb, prec)
   
   lenz = min(lena + lenb - 1, prec)
   if lenz < 0
      lenz = 0
   end

   z.prec = prec
   ccall((:fmpq_poly_mullow, :libflint), Void, 
                (Ptr{fmpq_rel_series}, Ptr{fmpq_rel_series}, Ptr{fmpq_rel_series}, Int), 
               &z, &a, &b, lenz)
end

function addeq!(a::fmpq_rel_series, b::fmpq_rel_series)
   lena = length(a)
   lenb = length(b)
         
   prec = min(a.prec, b.prec)
 
   lena = min(lena, prec)
   lenb = min(lenb, prec)

   lenz = max(lena, lenb)
   a.prec = prec
   ccall((:fmpq_poly_add_series, :libflint), Void, 
                (Ptr{fmpq_rel_series}, Ptr{fmpq_rel_series}, Ptr{fmpq_rel_series}, Int), 
               &a, &a, &b, lenz)
end

###############################################################################
#
#   Promotion rules
#
###############################################################################

Base.promote_rule{T <: Integer}(::Type{fmpq_rel_series}, ::Type{T}) = fmpq_rel_series

Base.promote_rule(::Type{fmpq_rel_series}, ::Type{fmpz}) = fmpq_rel_series

Base.promote_rule(::Type{fmpq_rel_series}, ::Type{fmpq}) = fmpq_rel_series

###############################################################################
#
#   Parent object call overload
#
###############################################################################

function Base.call(a::FmpqRelSeriesRing)
   z = fmpq_rel_series()
   z.prec = a.prec_max
   z.parent = a
   return z
end

function Base.call(a::FmpqRelSeriesRing, b::Integer)
   if b == 0
      z = fmpq_rel_series()
      z.prec = a.prec_max
   else
      z = fmpq_rel_series([fmpq(b)], 1, a.prec_max)
   end
   z.parent = a
   return z
end

function Base.call(a::FmpqRelSeriesRing, b::fmpz)
   if b == 0
      z = fmpq_rel_series()
      z.prec = a.prec_max
   else
      z = fmpq_rel_series([fmpq(b)], 1, a.prec_max)
   end
   z.parent = a
   return z
end

function Base.call(a::FmpqRelSeriesRing, b::fmpq)
   if b == 0
      z = fmpq_rel_series()
      z.prec = a.prec_max
   else
      z = fmpq_rel_series([b], 1, a.prec_max)
   end
   z.parent = a
   return z
end

function Base.call(a::FmpqRelSeriesRing, b::fmpq_rel_series)
   parent(b) != a && error("Unable to coerce power series")
   return b
end

function Base.call(a::FmpqRelSeriesRing, b::Array{fmpq, 1}, len::Int, prec::Int)
   z = fmpq_rel_series(b, len, prec)
   z.parent = a
   return z
end

###############################################################################
#
#   PowerSeriesRing constructor
#
###############################################################################

function PowerSeriesRing(R::FlintRationalField, prec::Int, s::AbstractString{})
   S = Symbol(s)

   parent_obj = FmpqRelSeriesRing(prec, S)
   
   return parent_obj, parent_obj([fmpq(0), fmpq(1)], 2, prec + 1)
end

