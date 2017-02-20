###############################################################################
#
#   fmpz_rel_series.jl : Power series over flint fmpz integers
#
###############################################################################

export fmpz_rel_series, FmpzRelSeriesRing

###############################################################################
#
#   Data type and parent object methods
#
###############################################################################

function O(a::fmpz_rel_series)
   val = pol_length(a) + valuation(a) - 1
   val < 0 && throw(DomainError())
   z = fmpz_rel_series(Array{fmpz}(0), 0, val, val)
   z.parent = parent(a)
   return z
end

elem_type(::FmpzRelSeriesRing) = fmpz_rel_series

parent_type(::Type{fmpz_rel_series}) = FmpzRelSeriesRing

base_ring(R::FmpzRelSeriesRing) = R.base_ring

var(a::FmpzRelSeriesRing) = a.S

###############################################################################
#
#   Basic manipulation
#
###############################################################################    
   
max_precision(R::FmpzRelSeriesRing) = R.prec_max

function normalise(a::fmpz_rel_series, len::Int)
   if len > 0
      c = fmpz()
      ccall((:fmpz_poly_get_coeff_fmpz, :libflint), Void, 
         (Ptr{fmpz}, Ptr{fmpz_rel_series}, Int), &c, &a, len - 1)
   end
   while len > 0 && iszero(c)
      len -= 1
      if len > 0
         ccall((:fmpz_poly_get_coeff_fmpz, :libflint), Void, 
            (Ptr{fmpz}, Ptr{fmpz_rel_series}, Int), &c, &a, len - 1)
      end
   end
   return len
end

function pol_length(x::fmpz_rel_series)
   return ccall((:fmpz_poly_length, :libflint), Int, (Ptr{fmpz_rel_series},), &x)
end

precision(x::fmpz_rel_series) = x.prec

function polcoeff(x::fmpz_rel_series, n::Int)
   if n < 0
      return fmpz(0)
   end
   z = fmpz()
   ccall((:fmpz_poly_get_coeff_fmpz, :libflint), Void, 
         (Ptr{fmpz}, Ptr{fmpz_rel_series}, Int), &z, &x, n)
   return z
end

zero(R::FmpzRelSeriesRing) = R(0)

one(R::FmpzRelSeriesRing) = R(1)

function gen(R::FmpzRelSeriesRing)
   z = fmpz_rel_series([fmpz(1)], 1, max_precision(R) + 1, 1)
   z.parent = R
   return z
end

function deepcopy_internal(a::fmpz_rel_series, dict::ObjectIdDict)
   z = fmpz_rel_series(a)
   z.prec = a.prec
   z.val = a.val
   z.parent = parent(a)
   return z
end

function renormalize!(z::fmpz_rel_series)
   i = 0
   zlen = pol_length(z)
   zval = valuation(z)
   zprec = precision(z)
   while i < zlen && polcoeff(z, i) == 0
      i += 1
   end
   z.prec = zprec
   if i == zlen
      z.val = zprec
   else
      z.val = zval + i
      ccall((:fmpz_poly_shift_right, :libflint), Void,
            (Ptr{fmpz_rel_series}, Ptr{fmpz_rel_series}, Int), &z, &z, i)
   end
   return nothing
end

###############################################################################
#
#   String I/O
#
###############################################################################

function show(io::IO, a::FmpzRelSeriesRing)
   print(io, "Univariate power series ring in ", var(a), " over ")
   show(io, base_ring(a))
end

show_minus_one(::Type{fmpz_rel_series}) = show_minus_one(fmpz)

###############################################################################
#
#   Unary operators
#
###############################################################################

function -(x::fmpz_rel_series)
   z = parent(x)()
   ccall((:fmpz_poly_neg, :libflint), Void, 
                (Ptr{fmpz_rel_series}, Ptr{fmpz_rel_series}), 
               &z, &x)
   z.prec = x.prec
   z.val = x.val
   return z
end

###############################################################################
#
#   Binary operators
#
###############################################################################

function +(a::fmpz_rel_series, b::fmpz_rel_series)
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
      ccall((:fmpz_poly_set_trunc, :libflint), Void,
            (Ptr{fmpz_rel_series}, Ptr{fmpz_rel_series}, Int),
            &z, &b, max(0, lenz - b.val + a.val))
      ccall((:fmpz_poly_shift_left, :libflint), Void,
            (Ptr{fmpz_rel_series}, Ptr{fmpz_rel_series}, Int),
            &z, &z, b.val - a.val)
      ccall((:fmpz_poly_add_series, :libflint), Void, 
                (Ptr{fmpz_rel_series}, Ptr{fmpz_rel_series}, Ptr{fmpz_rel_series}, Int), 
               &z, &z, &a, lenz)
   elseif b.val < a.val
      lenz = max(lena + a.val - b.val, lenb)
      ccall((:fmpz_poly_set_trunc, :libflint), Void,
            (Ptr{fmpz_rel_series}, Ptr{fmpz_rel_series}, Int),
            &z, &a, max(0, lenz - a.val + b.val))
      ccall((:fmpz_poly_shift_left, :libflint), Void,
            (Ptr{fmpz_rel_series}, Ptr{fmpz_rel_series}, Int),
            &z, &z, a.val - b.val)
      ccall((:fmpz_poly_add_series, :libflint), Void, 
                (Ptr{fmpz_rel_series}, Ptr{fmpz_rel_series}, Ptr{fmpz_rel_series}, Int), 
               &z, &z, &b, lenz)
   else
      lenz = max(lena, lenb)
      ccall((:fmpz_poly_add_series, :libflint), Void, 
                (Ptr{fmpz_rel_series}, Ptr{fmpz_rel_series}, Ptr{fmpz_rel_series}, Int), 
               &z, &a, &b, lenz)
   end
   z.prec = prec
   z.val = val
   renormalize!(z)
   return z
end

function -(a::fmpz_rel_series, b::fmpz_rel_series)
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
      ccall((:fmpz_poly_set_trunc, :libflint), Void,
            (Ptr{fmpz_rel_series}, Ptr{fmpz_rel_series}, Int),
            &z, &b, max(0, lenz - b.val + a.val))
      ccall((:fmpz_poly_shift_left, :libflint), Void,
            (Ptr{fmpz_rel_series}, Ptr{fmpz_rel_series}, Int),
            &z, &z, b.val - a.val)
      ccall((:fmpz_poly_neg, :libflint), Void,
            (Ptr{fmpz_rel_series}, Ptr{fmpz_rel_series}), &z, &z)
      ccall((:fmpz_poly_add_series, :libflint), Void, 
                (Ptr{fmpz_rel_series}, Ptr{fmpz_rel_series}, Ptr{fmpz_rel_series}, Int), 
               &z, &z, &a, lenz)
   elseif b.val < a.val
      lenz = max(lena + a.val - b.val, lenb)
      ccall((:fmpz_poly_set_trunc, :libflint), Void,
            (Ptr{fmpz_rel_series}, Ptr{fmpz_rel_series}, Int),
            &z, &a, max(0, lenz - a.val + b.val))
      ccall((:fmpz_poly_shift_left, :libflint), Void,
            (Ptr{fmpz_rel_series}, Ptr{fmpz_rel_series}, Int),
            &z, &z, a.val - b.val)
      ccall((:fmpz_poly_sub_series, :libflint), Void, 
                (Ptr{fmpz_rel_series}, Ptr{fmpz_rel_series}, Ptr{fmpz_rel_series}, Int), 
               &z, &z, &b, lenz)
   else
      lenz = max(lena, lenb)
      ccall((:fmpz_poly_sub_series, :libflint), Void, 
                (Ptr{fmpz_rel_series}, Ptr{fmpz_rel_series}, Ptr{fmpz_rel_series}, Int), 
               &z, &a, &b, lenz)
   end
   z.prec = prec
   z.val = val
   renormalize!(z)
   return z
end

function *(a::fmpz_rel_series, b::fmpz_rel_series)
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
   ccall((:fmpz_poly_mullow, :libflint), Void, 
                (Ptr{fmpz_rel_series}, Ptr{fmpz_rel_series}, Ptr{fmpz_rel_series}, Int), 
               &z, &a, &b, lenz)

   return z
end

###############################################################################
#
#   Ad hoc binary operators
#
###############################################################################

function *(x::Int, y::fmpz_rel_series)
   z = parent(y)()
   z.prec = y.prec
   z.val = y.val
   ccall((:fmpz_poly_scalar_mul_si, :libflint), Void, 
                (Ptr{fmpz_rel_series}, Ptr{fmpz_rel_series}, Int), 
               &z, &y, x)
   return z
end

*(x::fmpz_rel_series, y::Int) = y * x

function *(x::fmpz, y::fmpz_rel_series)
   z = parent(y)()
   z.prec = y.prec
   z.val = y.val
   ccall((:fmpz_poly_scalar_mul_fmpz, :libflint), Void, 
                (Ptr{fmpz_rel_series}, Ptr{fmpz_rel_series}, Ptr{fmpz}), 
               &z, &y, &x)
   return z
end

*(x::fmpz_rel_series, y::fmpz) = y * x

###############################################################################
#
#   Shifting
#
###############################################################################

function shift_left(x::fmpz_rel_series, len::Int)
   len < 0 && throw(DomainError())
   xlen = pol_length(x)
   z = fmpz_rel_series(x)
   z.prec = x.prec + len
   z.val = x.val + len
   z.parent = parent(x)
   return z
end

function shift_right(x::fmpz_rel_series, len::Int)
   len < 0 && throw(DomainError())
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
      ccall((:fmpz_poly_shift_right, :libflint), Void, 
                (Ptr{fmpz_rel_series}, Ptr{fmpz_rel_series}, Int), 
               &z, &x, xlen - zlen)
      renormalize!(z)
   end
   return z
end

###############################################################################
#
#   Truncation
#
###############################################################################

function truncate(x::fmpz_rel_series, prec::Int)
   prec < 0 && throw(DomainError())
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
      ccall((:fmpz_poly_set_trunc, :libflint), Void, 
                (Ptr{fmpz_rel_series}, Ptr{fmpz_rel_series}, Int), 
               &z, &x, min(prec - xval, xlen))
   end
   return z
end

###############################################################################
#
#   Powering
#
###############################################################################

function ^(a::fmpz_rel_series, b::Int)
   b < 0 && throw(DomainError())
   if isgen(a)
      z = parent(a)()
      setcoeff!(z, 0, fmpz(1))
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
      return parent(a)([fmpz(1)], 1, precision(a) - valuation(a), 0)
   else
      z = parent(a)()
      z.prec = a.prec + (b - 1)*valuation(a)
      z.val = b*valuation(a)
      ccall((:fmpz_poly_pow_trunc, :libflint), Void, 
                (Ptr{fmpz_rel_series}, Ptr{fmpz_rel_series}, Int, Int), 
               &z, &a, b, z.prec - z.val)
   end
   return z
end

###############################################################################
#
#   Comparison
#
###############################################################################

function ==(x::fmpz_rel_series, y::fmpz_rel_series)
   check_parent(x, y)
   prec = min(x.prec, y.prec)
   if prec <= x.val && prec <= y.val
      return true
   end
   if x.val != y.val
      return false
   end
   xlen = min(pol_length(x), prec - x.val)
   ylen = min(pol_length(y), prec - y.val)
   if xlen != ylen
      return false
   end
   return Bool(ccall((:fmpz_poly_equal_trunc, :libflint), Cint, 
                (Ptr{fmpz_rel_series}, Ptr{fmpz_rel_series}, Int), 
               &x, &y, xlen))
end

function isequal(x::fmpz_rel_series, y::fmpz_rel_series)
   if parent(x) != parent(y)
      return false
   end
   if x.prec != y.prec || x.val != y.val || pol_length(x) != pol_length(y)
      return false
   end
   return Bool(ccall((:fmpz_poly_equal, :libflint), Cint, 
                (Ptr{fmpz_rel_series}, Ptr{fmpz_rel_series}, Int), 
               &x, &y, pol_length(x)))
end

###############################################################################
#
#   Ad hoc comparisons
#
###############################################################################

function ==(x::fmpz_rel_series, y::fmpz) 
   if precision(x) == 0
      return true
   elseif pol_length(x) > 1
      return false
   elseif pol_length(x) == 1 
      if x.val == 0
         z = fmpz()
         ccall((:fmpz_poly_get_coeff_fmpz, :libflint), Void, 
                       (Ptr{fmpz}, Ptr{fmpz_rel_series}, Int), &z, &x, 0)
         return ccall((:fmpz_equal, :libflint), Bool, 
               (Ptr{fmpz}, Ptr{fmpz}, Int), &z, &y, 0)
      else
         return false
      end
   else
      return y == 0
   end 
end

==(x::fmpz, y::fmpz_rel_series) = y == x

==(x::fmpz_rel_series, y::Integer) = x == fmpz(y)

==(x::Integer, y::fmpz_rel_series) = y == x

###############################################################################
#
#   Exact division
#
###############################################################################

function divexact(x::fmpz_rel_series, y::fmpz_rel_series)
   check_parent(x, y)
   y == 0 && throw(DivideError())
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
      ccall((:fmpz_poly_div_series, :libflint), Void, 
                (Ptr{fmpz_rel_series}, Ptr{fmpz_rel_series}, Ptr{fmpz_rel_series}, Int), 
               &z, &x, &y, prec)
   end
   return z
end

###############################################################################
#
#   Ad hoc exact division
#
###############################################################################

function divexact(x::fmpz_rel_series, y::Int)
   y == 0 && throw(DivideError())
   z = parent(x)()
   z.prec = x.prec
   z.val = x.val
   ccall((:fmpz_poly_scalar_divexact_si, :libflint), Void, 
                (Ptr{fmpz_rel_series}, Ptr{fmpz_rel_series}, Int), 
               &z, &x, y)
   return z
end

function divexact(x::fmpz_rel_series, y::fmpz)
   y == 0 && throw(DivideError())
   z = parent(x)()
   z.prec = x.prec
   z.prec = x.prec
   z.val = x.val
   ccall((:fmpz_poly_scalar_divexact_fmpz, :libflint), Void, 
                (Ptr{fmpz_rel_series}, Ptr{fmpz_rel_series}, Ptr{fmpz}), 
               &z, &x, &y)
   return z
end

divexact(x::fmpz_rel_series, y::Integer) = divexact(x, fmpz(y))

###############################################################################
#
#   Inversion
#
###############################################################################

function inv(a::fmpz_rel_series)
   a == 0 && throw(DivideError())
   !isunit(a) && error("Unable to invert power series")
   ainv = parent(a)()
   ainv.prec = a.prec
   ainv.val = 0
   ccall((:fmpz_poly_inv_series, :libflint), Void, 
                (Ptr{fmpz_rel_series}, Ptr{fmpz_rel_series}, Int), 
               &ainv, &a, a.prec)
   return ainv
end

###############################################################################
#
#   Unsafe functions
#
###############################################################################

function zero!(x::fmpz_rel_series)
  ccall((:fmpz_poly_zero, :libflint), Void, 
                   (Ptr{fmpz_rel_series},), &x)
  x.prec = parent(x).prec_max
end

function setcoeff!(z::fmpz_rel_series, n::Int, x::fmpz)
   ccall((:fmpz_poly_set_coeff_fmpz, :libflint), Void, 
                (Ptr{fmpz_rel_series}, Int, Ptr{fmpz}), 
               &z, n, &x)
end

function mul!(z::fmpz_rel_series, a::fmpz_rel_series, b::fmpz_rel_series)
   lena = pol_length(a)
   lenb = pol_length(b)
   aval = valuation(a)
   bval = valuation(b)
   prec = min(a.prec - aval, b.prec - bval)
   lena = min(lena, prec)
   lenb = min(lenb, prec)
   z.val = a.val + b.val
   z.prec = prec + c.val
   lenz = min(lena + lenb - 1, prec)
   if lena <= 0 || lenb <= 0
      lenz = 0
   end
   ccall((:fmpz_poly_mullow, :libflint), Void, 
                (Ptr{fmpz_rel_series}, Ptr{fmpz_rel_series}, Ptr{fmpz_rel_series}, Int), 
               &z, &a, &b, lenz)
   return nothing
end

function addeq!(a::fmpz_rel_series, b::fmpz_rel_series)
   lena = pol_length(a)
   lenb = pol_length(b)  
   prec = min(a.prec, b.prec)
   val = min(a.val, b.val)
   lena = min(lena, prec - a.val)
   lenb = min(lenb, prec - b.val)
   if a.val < b.val
      z = fmpz_rel_series()
      lenz = max(lena, lenb + b.val - a.val)
      ccall((:fmpz_poly_set_trunc, :libflint), Void,
            (Ptr{fmpz_rel_series}, Ptr{fmpz_rel_series}, Int),
            &z, &b, max(0, lenz - b.val + a.val))
      ccall((:fmpz_poly_shift_left, :libflint), Void,
            (Ptr{fmpz_rel_series}, Ptr{fmpz_rel_series}, Int),
            &z, &z, b.val - a.val)
      ccall((:fmpz_poly_add_series, :libflint), Void, 
                (Ptr{fmpz_rel_series}, Ptr{fmpz_rel_series}, Ptr{fmpz_rel_series}, Int), 
               &a, &a, &z, lenz)
   elseif b.val < a.val
      lenz = max(lena + a.val - b.val, lenb)
      ccall((:fmpz_poly_truncate, :libflint), Void,
            (Ptr{fmpz_rel_series}, Int),
            &a, max(0, lenz - a.val + b.val))
      ccall((:fmpz_poly_shift_left, :libflint), Void,
            (Ptr{fmpz_rel_series}, Ptr{fmpz_rel_series}, Int),
            &a, &a, a.val - b.val)
      ccall((:fmpz_poly_add_series, :libflint), Void, 
                (Ptr{fmpz_rel_series}, Ptr{fmpz_rel_series}, Ptr{fmpz_rel_series}, Int), 
               &a, &a, &b, lenz)
   else
      lenz = max(lena, lenb)
      ccall((:fmpz_poly_add_series, :libflint), Void, 
                (Ptr{fmpz_rel_series}, Ptr{fmpz_rel_series}, Ptr{fmpz_rel_series}, Int), 
               &a, &a, &b, lenz)
   end
   a.prec = prec
   a.val = val
   renormalize!(a)
   return nothing
end

function add!(c::fmpz_rel_series, a::fmpz_rel_series, b::fmpz_rel_series)
   lena = length(a)
   lenb = length(b)
         
   prec = min(a.prec, b.prec)
 
   lena = min(lena, prec)
   lenb = min(lenb, prec)

   lenc = max(lena, lenb)
   c.prec = prec
   ccall((:fmpz_poly_add_series, :libflint), Void, 
                (Ptr{fmpz_rel_series}, Ptr{fmpz_rel_series}, Ptr{fmpz_rel_series}, Int), 
               &c, &a, &b, lenc)
end

###############################################################################
#
#   Promotion rules
#
###############################################################################

Base.promote_rule{T <: Integer}(::Type{fmpz_rel_series}, ::Type{T}) = fmpz_rel_series

Base.promote_rule(::Type{fmpz_rel_series}, ::Type{fmpz}) = fmpz_rel_series

###############################################################################
#
#   Parent object call overload
#
###############################################################################

function (a::FmpzRelSeriesRing)()
   z = fmpz_rel_series()
   z.prec = a.prec_max
   z.val = a.prec_max
   z.parent = a
   return z
end

function (a::FmpzRelSeriesRing)(b::Integer)
   if b == 0
      z = fmpz_rel_series()
      z.prec = a.prec_max
   else
      z = fmpz_rel_series([fmpz(b)], 1, a.prec_max, 0)
   end
   z.parent = a
   return z
end

function (a::FmpzRelSeriesRing)(b::fmpz)
   if b == 0
      z = fmpz_rel_series()
      z.prec = a.prec_max
   else
      z = fmpz_rel_series([b], 1, a.prec_max, 0)
   end
   z.parent = a
   return z
end

function (a::FmpzRelSeriesRing)(b::fmpz_rel_series)
   parent(b) != a && error("Unable to coerce power series")
   return b
end

function (a::FmpzRelSeriesRing)(b::Array{fmpz, 1}, len::Int, prec::Int, val::Int)
   z = fmpz_rel_series(b, len, prec, val)
   z.parent = a
   return z
end

