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
   prec = length(a) - 1
   prec < 0 && throw(DomainError())
   z = fmpz_rel_series(Array(fmpz, 0), 0, prec)
   z.parent = parent(a)
   return z
end

elem_type(::FmpzRelSeriesRing) = fmpz_rel_series

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

function length(x::fmpz_rel_series)
   return ccall((:fmpz_poly_length, :libflint), Int, (Ptr{fmpz_rel_series},), &x)
end

precision(x::fmpz_rel_series) = x.prec

function coeff(x::fmpz_rel_series, n::Int)
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
   z = fmpz_rel_series([fmpz(0), fmpz(1)], 2, max_precision(R) + 1)
   z.parent = R
   return z
end

function deepcopy(a::fmpz_rel_series)
   z = fmpz_rel_series(a)
   z.prec = a.prec
   z.parent = parent(a)
   return z
end

###############################################################################
#
#   AbstractString{} I/O
#
###############################################################################

function show(io::IO, x::fmpz_rel_series)
   if length(x) == 0
      print(io, "0")
   else
      cstr = ccall((:fmpz_poly_get_str_pretty, :libflint), Ptr{UInt8}, 
        (Ptr{fmpz_rel_series}, Ptr{UInt8}), &x, bytestring(string(var(parent(x)))))

      print(io, bytestring(cstr))

      ccall((:flint_free, :libflint), Void, (Ptr{UInt8},), cstr)
   end
   print(io, "+O(", string(var(parent(x))), "^", x.prec, ")")
end

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
   return z
end

###############################################################################
#
#   Binary operators
#
###############################################################################

function +(a::fmpz_rel_series, b::fmpz_rel_series)
   check_parent(a, b)
   lena = length(a)
   lenb = length(b)
         
   prec = min(a.prec, b.prec)
 
   lena = min(lena, prec)
   lenb = min(lenb, prec)

   lenz = max(lena, lenb)
   z = parent(a)()
   z.prec = prec
   ccall((:fmpz_poly_add_series, :libflint), Void, 
                (Ptr{fmpz_rel_series}, Ptr{fmpz_rel_series}, Ptr{fmpz_rel_series}, Int), 
               &z, &a, &b, lenz)
   return z
end

function -(a::fmpz_rel_series, b::fmpz_rel_series)
   check_parent(a, b)
   lena = length(a)
   lenb = length(b)
         
   prec = min(a.prec, b.prec)
 
   lena = min(lena, prec)
   lenb = min(lenb, prec)

   lenz = max(lena, lenb)
   z = parent(a)()
   z.prec = prec
   ccall((:fmpz_poly_sub_series, :libflint), Void, 
                (Ptr{fmpz_rel_series}, Ptr{fmpz_rel_series}, Ptr{fmpz_rel_series}, Int), 
               &z, &a, &b, lenz)
   return z
end

function *(a::fmpz_rel_series, b::fmpz_rel_series)
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
   ccall((:fmpz_poly_scalar_mul_si, :libflint), Void, 
                (Ptr{fmpz_rel_series}, Ptr{fmpz_rel_series}, Int), 
               &z, &y, x)
   return z
end

function *(x::fmpz, y::fmpz_rel_series)
   z = parent(y)()
   z.prec = y.prec
   ccall((:fmpz_poly_scalar_mul_fmpz, :libflint), Void, 
                (Ptr{fmpz_rel_series}, Ptr{fmpz_rel_series}, Ptr{fmpz}), 
               &z, &y, &x)
   return z
end

###############################################################################
#
#   Shifting
#
###############################################################################

function shift_left(x::fmpz_rel_series, len::Int)
   len < 0 && throw(DomainError())
   xlen = length(x)
   z = parent(x)()
   z.prec = x.prec + len
   ccall((:fmpz_poly_shift_left, :libflint), Void, 
                (Ptr{fmpz_rel_series}, Ptr{fmpz_rel_series}, Int), 
               &z, &x, len)
   return z
end

function shift_right(x::fmpz_rel_series, len::Int)
   len < 0 && throw(DomainError())
   xlen = length(x)
   z = parent(x)()
   if len >= xlen
      z.prec = max(0, x.prec - len)
   else
      z.prec = x.prec - len
      ccall((:fmpz_poly_shift_right, :libflint), Void, 
                (Ptr{fmpz_rel_series}, Ptr{fmpz_rel_series}, Int), 
               &z, &x, len)
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
   if x.prec <= prec
      return x
   end
   z = parent(x)()
   z.prec = prec
   ccall((:fmpz_poly_set_trunc, :libflint), Void, 
                (Ptr{fmpz_rel_series}, Ptr{fmpz_rel_series}, Int), 
               &z, &x, prec)
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
      setcoeff!(z, b, fmpz(1))
      z.prec = a.prec + b - 1
   elseif length(a) == 0
      z = parent(a)()
      z.prec = a.prec + (b - 1)*valuation(a)
   elseif length(a) == 1
      return parent(a)([coeff(a, 0)^b], 1, a.prec)
   elseif b == 0
      return parent(a)([fmpz(1)], 1, parent(a).prec_max)
   else
      z = parent(a)()
      z.prec = a.prec + (b - 1)*valuation(a)
      ccall((:fmpz_poly_pow_trunc, :libflint), Void, 
                (Ptr{fmpz_rel_series}, Ptr{fmpz_rel_series}, Int, Int), 
               &z, &a, b, z.prec)
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
   
   n = max(length(x), length(y))
   n = min(n, prec)
   
   return Bool(ccall((:fmpz_poly_equal_trunc, :libflint), Cint, 
                (Ptr{fmpz_rel_series}, Ptr{fmpz_rel_series}, Int), 
               &x, &y, n))
end

function isequal(x::fmpz_rel_series, y::fmpz_rel_series)
   if parent(x) != parent(y)
      return false
   end
   if x.prec != y.prec || length(x) != length(y)
      return false
   end
   return Bool(ccall((:fmpz_poly_equal, :libflint), Cint, 
                (Ptr{fmpz_rel_series}, Ptr{fmpz_rel_series}, Int), 
               &x, &y, length(x)))
end

###############################################################################
#
#   Exact division
#
###############################################################################

function divexact(x::fmpz_rel_series, y::fmpz_rel_series)
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
   ccall((:fmpz_poly_div_series, :libflint), Void, 
                (Ptr{fmpz_rel_series}, Ptr{fmpz_rel_series}, Ptr{fmpz_rel_series}, Int), 
               &z, &x, &y, prec)
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
   ccall((:fmpz_poly_scalar_divexact_si, :libflint), Void, 
                (Ptr{fmpz_rel_series}, Ptr{fmpz_rel_series}, Int), 
               &z, &x, y)
   return z
end

function divexact(x::fmpz_rel_series, y::fmpz)
   y == 0 && throw(DivideError())
   z = parent(x)()
   z.prec = x.prec
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

function setcoeff!(z::fmpz_rel_series, n::Int, x::fmpz)
   ccall((:fmpz_poly_set_coeff_fmpz, :libflint), Void, 
                (Ptr{fmpz_rel_series}, Int, Ptr{fmpz}), 
               &z, n, &x)
end

function mul!(z::fmpz_rel_series, a::fmpz_rel_series, b::fmpz_rel_series)
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
   ccall((:fmpz_poly_mullow, :libflint), Void, 
                (Ptr{fmpz_rel_series}, Ptr{fmpz_rel_series}, Ptr{fmpz_rel_series}, Int), 
               &z, &a, &b, lenz)
end

function addeq!(a::fmpz_rel_series, b::fmpz_rel_series)
   lena = length(a)
   lenb = length(b)
         
   prec = min(a.prec, b.prec)
 
   lena = min(lena, prec)
   lenb = min(lenb, prec)

   lenz = max(lena, lenb)
   a.prec = prec
   ccall((:fmpz_poly_add_series, :libflint), Void, 
                (Ptr{fmpz_rel_series}, Ptr{fmpz_rel_series}, Ptr{fmpz_rel_series}, Int), 
               &a, &a, &b, lenz)
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

function Base.call(a::FmpzRelSeriesRing)
   z = fmpz_rel_series()
   z.prec = a.prec_max
   z.parent = a
   return z
end

function Base.call(a::FmpzRelSeriesRing, b::Integer)
   if b == 0
      z = fmpz_rel_series()
      z.prec = a.prec_max
   else
      z = fmpz_rel_series([fmpz(b)], 1, a.prec_max)
   end
   z.parent = a
   return z
end

function Base.call(a::FmpzRelSeriesRing, b::fmpz)
   if b == 0
      z = fmpz_rel_series()
      z.prec = a.prec_max
   else
      z = fmpz_rel_series([b], 1, a.prec_max)
   end
   z.parent = a
   return z
end

function Base.call(a::FmpzRelSeriesRing, b::fmpz_rel_series)
   parent(b) != a && error("Unable to coerce power series")
   return b
end

function Base.call(a::FmpzRelSeriesRing, b::Array{fmpz, 1}, len::Int, prec::Int)
   z = fmpz_rel_series(b, len, prec)
   z.parent = a
   return z
end

###############################################################################
#
#   PowerSeriesRing constructor
#
###############################################################################

function PowerSeriesRing(R::FlintIntegerRing, prec::Int, s::AbstractString{})
   S = Symbol(s)

   parent_obj = FmpzRelSeriesRing(prec, S)
   
   return parent_obj, parent_obj([fmpz(0), fmpz(1)], 2, prec + 1)
end

