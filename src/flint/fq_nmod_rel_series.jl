###############################################################################
#
#   fq_nmod_rel_series.jl : Power series over flint finite fields
#
###############################################################################

export fq_nmod_rel_series, FqNmodRelSeriesRing

###############################################################################
#
#   Data type and parent object methods
#
###############################################################################

function O(a::fq_nmod_rel_series)
   prec = length(a) - 1
   prec < 0 && throw(DomainError())
   z = fq_nmod_rel_series(base_ring(a), Array(fq_nmod, 0), 0, prec)
   z.parent = parent(a)
   return z
end

parent_type(::Type{fq_nmod_rel_series}) = FqNmodRelSeriesRing

elem_type(::FqNmodRelSeriesRing) = fq_nmod_rel_series

base_ring(R::FqNmodRelSeriesRing) = R.base_ring

var(a::FqNmodRelSeriesRing) = a.S

###############################################################################
#
#   Basic manipulation
#
###############################################################################    
   
max_precision(R::FqNmodRelSeriesRing) = R.prec_max

function normalise(a::fq_nmod_rel_series, len::Int)
   ctx = base_ring(a)
   if len > 0
      c = ctx()
      ccall((:fq_nmod_poly_get_coeff, :libflint), Void, 
         (Ptr{fq_nmod}, Ptr{fq_nmod_rel_series}, Int, Ptr{FqNmodFiniteField}), 
                                           &c, &a, len - 1, &ctx)
   end
   while len > 0 && iszero(c)
      len -= 1
      if len > 0
         ccall((:fq_nmod_poly_get_coeff, :libflint), Void, 
            (Ptr{fq_nmod}, Ptr{fq_nmod_rel_series}, Int, Ptr{FqNmodFiniteField}), 
                                           &c, &a, len - 1, &ctx)
      end
   end

   return len
end

function set_length!(a::fq_nmod_rel_series, len::Int)
   ccall((:_fq_nmod_poly_set_length, :libflint), Void,
      (Ptr{fq_nmod_rel_series}, Int), &a, len)
end

function coeff(x::fq_nmod_rel_series, n::Int)
   ctx = base_ring(x)
   if n < 0
      return ctx()
   end
   z = ctx()
   ccall((:fq_nmod_poly_get_coeff, :libflint), Void, 
         (Ptr{fq_nmod}, Ptr{fq_nmod_rel_series}, Int, Ptr{FqNmodFiniteField}), 
             &z, &x, n, &ctx)
   return z
end

function length(x::fq_nmod_rel_series)
   return ccall((:fq_nmod_poly_length, :libflint), Int, 
                (Ptr{fq_nmod_rel_series},), &x)
end

precision(x::fq_nmod_rel_series) = x.prec

zero(R::FqNmodRelSeriesRing) = R(0)

one(R::FqNmodRelSeriesRing) = R(1)

function gen(R::FqNmodRelSeriesRing)
   ctx = base_ring(R)
   z = fq_nmod_rel_series(ctx, [ctx(0), ctx(1)], 2, max_precision(R) + 1)
   z.parent = R
   return z
end

function deepcopy(a::fq_nmod_rel_series)
   z = fq_nmod_rel_series(base_ring(a), a)
   z.prec = a.prec
   z.parent = parent(a)
   return z
end

###############################################################################
#
#   AbstractString{} I/O
#
###############################################################################

function show(io::IO, x::fq_nmod_rel_series)
   if length(x) == 0
      print(io, "0")
   else
      ctx = base_ring(x)
      cstr = ccall((:fq_nmod_poly_get_str_pretty, :libflint), Ptr{UInt8}, 
        (Ptr{fq_nmod_rel_series}, Ptr{UInt8}, Ptr{FqNmodFiniteField}), 
                     &x, bytestring(string(var(parent(x)))), &ctx)

      print(io, bytestring(cstr))

      ccall((:flint_free, :libflint), Void, (Ptr{UInt8},), cstr)
   end
   print(io, "+O(", string(var(parent(x))), "^", x.prec, ")")
end

function show(io::IO, a::FqNmodRelSeriesRing)
   print(io, "Univariate power series ring in ", var(a), " over ")
   show(io, base_ring(a))
end

show_minus_one(::Type{fq_nmod_rel_series}) = show_minus_one(fq_nmod)

###############################################################################
#
#   Unary operators
#
###############################################################################

function -(x::fq_nmod_rel_series)
   ctx = base_ring(x)
   z = parent(x)()
   ccall((:fq_nmod_poly_neg, :libflint), Void, 
                (Ptr{fq_nmod_rel_series}, Ptr{fq_nmod_rel_series}, Ptr{FqNmodFiniteField}), 
               &z, &x, &ctx)
   z.prec = x.prec
   return z
end

###############################################################################
#
#   Binary operators
#
###############################################################################

function +(a::fq_nmod_rel_series, b::fq_nmod_rel_series)
   check_parent(a, b)
   ctx = base_ring(a)
   lena = length(a)
   lenb = length(b)
         
   prec = min(a.prec, b.prec)
 
   lena = min(lena, prec)
   lenb = min(lenb, prec)

   lenz = max(lena, lenb)
   z = parent(a)()
   z.prec = prec
   ccall((:fq_nmod_poly_add_series, :libflint), Void, 
     (Ptr{fq_nmod_rel_series}, Ptr{fq_nmod_rel_series}, Ptr{fq_nmod_rel_series}, Int, Ptr{FqNmodFiniteField}), 
               &z, &a, &b, lenz, &ctx)
   return z
end

function -(a::fq_nmod_rel_series, b::fq_nmod_rel_series)
   check_parent(a, b)
   ctx = base_ring(a)
   lena = length(a)
   lenb = length(b)
         
   prec = min(a.prec, b.prec)
 
   lena = min(lena, prec)
   lenb = min(lenb, prec)

   lenz = max(lena, lenb)
   z = parent(a)()
   z.prec = prec
   ccall((:fq_nmod_poly_sub_series, :libflint), Void, 
     (Ptr{fq_nmod_rel_series}, Ptr{fq_nmod_rel_series}, Ptr{fq_nmod_rel_series}, Int, Ptr{FqNmodFiniteField}), 
               &z, &a, &b, lenz, &ctx)
   return z
end

function *(a::fq_nmod_rel_series, b::fq_nmod_rel_series)
   check_parent(a, b)
   ctx = base_ring(a)
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

   ccall((:fq_nmod_poly_mullow, :libflint), Void, 
     (Ptr{fq_nmod_rel_series}, Ptr{fq_nmod_rel_series}, Ptr{fq_nmod_rel_series}, Int, Ptr{FqNmodFiniteField}),
               &z, &a, &b, lenz, &ctx)
   return z
end

###############################################################################
#
#   Ad hoc binary operators
#
###############################################################################

function *(x::fq_nmod, y::fq_nmod_rel_series)
   ctx = base_ring(y)
   z = parent(y)()
   z.prec = y.prec
   ccall((:fq_nmod_poly_scalar_mul_fq_nmod, :libflint), Void, 
         (Ptr{fq_nmod_rel_series}, Ptr{fq_nmod_rel_series}, Ptr{fq_nmod}, Ptr{FqNmodFiniteField}), 
               &z, &y, &x, &ctx)
   return z
end

*(x::fq_nmod_rel_series, y::fq_nmod) = y*x

###############################################################################
#
#   Shifting
#
###############################################################################

function shift_left(x::fq_nmod_rel_series, len::Int)
   len < 0 && throw(DomainError())
   ctx = base_ring(x)
   xlen = length(x)
   z = parent(x)()
   z.prec = x.prec + len
   ccall((:fq_nmod_poly_shift_left, :libflint), Void, 
                (Ptr{fq_nmod_rel_series}, Ptr{fq_nmod_rel_series}, Int, Ptr{FqNmodFiniteField}), 
               &z, &x, len, &ctx)
   return z
end

function shift_right(x::fq_nmod_rel_series, len::Int)
   len < 0 && throw(DomainError())
   ctx = base_ring(x)
   xlen = length(x)
   z = parent(x)()
   if len >= xlen
      z.prec = max(0, x.prec - len)
   else
      z.prec = x.prec - len
      ccall((:fq_nmod_poly_shift_right, :libflint), Void, 
                (Ptr{fq_nmod_rel_series}, Ptr{fq_nmod_rel_series}, Int, Ptr{FqNmodFiniteField}), 
               &z, &x, len, &ctx)
   end
   return z
end

###############################################################################
#
#   Truncation
#
###############################################################################

function truncate(x::fq_nmod_rel_series, prec::Int)
   prec < 0 && throw(DomainError())
   if x.prec <= prec
      return x
   end
   ctx = base_ring(x)
   z = parent(x)()
   z.prec = prec
   ccall((:fq_nmod_poly_set_trunc, :libflint), Void, 
                (Ptr{fq_nmod_rel_series}, Ptr{fq_nmod_rel_series}, Int, Ptr{FqNmodFiniteField}), 
               &z, &x, prec, &ctx)
   return z
end

###############################################################################
#
#   Powering
#
###############################################################################

function ^(a::fq_nmod_rel_series, b::Int)
   b < 0 && throw(DomainError())
   ctx = base_ring(a)
   if isgen(a)
      z = parent(a)()
      setcoeff!(z, b, ctx(1))
      z.prec = a.prec + b - 1
   elseif length(a) == 0
      z = parent(a)()
      z.prec = a.prec + (b - 1)*valuation(a)
   elseif length(a) == 1
      return parent(a)([coeff(a, 0)^b], 1, a.prec)
   elseif b == 0
      return parent(a)([ctx(1)], 1, parent(a).prec_max)
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

function ==(x::fq_nmod_rel_series, y::fq_nmod_rel_series)
   check_parent(x, y)
   ctx = base_ring(x)
   prec = min(x.prec, y.prec)
   
   n = max(length(x), length(y))
   n = min(n, prec)
   
   return Bool(ccall((:fq_nmod_poly_equal_trunc, :libflint), Cint, 
                (Ptr{fq_nmod_rel_series}, Ptr{fq_nmod_rel_series}, Int, Ptr{FqNmodFiniteField}), 
               &x, &y, n, &ctx))
end

function isequal(x::fq_nmod_rel_series, y::fq_nmod_rel_series)
   if parent(x) != parent(y)
      return false
   end
   ctx = base_ring(x)
   if x.prec != y.prec || length(x) != length(y)
      return false
   end
   return Bool(ccall((:fq_nmod_poly_equal, :libflint), Cint, 
                (Ptr{fq_nmod_rel_series}, Ptr{fq_nmod_rel_series}, Int, Ptr{FqNmodFiniteField}), 
               &x, &y, length(x), &ctx))
end

###############################################################################
#
#   Exact division
#
###############################################################################

function divexact(x::fq_nmod_rel_series, y::fq_nmod_rel_series)
   check_parent(x, y)
   ctx = base_ring(x)
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
   ccall((:fq_nmod_poly_div_series, :libflint), Void, 
     (Ptr{fq_nmod_rel_series}, Ptr{fq_nmod_rel_series}, Ptr{fq_nmod_rel_series}, Int, Ptr{FqNmodFiniteField}),
               &z, &x, &y, prec, &ctx)
   return z
end

###############################################################################
#
#   Ad hoc exact division
#
###############################################################################

function divexact(x::fq_nmod_rel_series, y::fq_nmod)
   y == 0 && throw(DivideError())
   ctx = base_ring(x)
   z = parent(x)()
   z.prec = x.prec
   ccall((:fq_nmod_poly_scalar_div_fq_nmod, :libflint), Void, 
                (Ptr{fq_nmod_rel_series}, Ptr{fq_nmod_rel_series}, Ptr{fq_nmod}, Ptr{FqNmodFiniteField}), 
               &z, &x, &y, &ctx)
   return z
end

###############################################################################
#
#   Inversion
#
###############################################################################

function inv(a::fq_nmod_rel_series)
   a == 0 && throw(DivideError())
   !isunit(a) && error("Unable to invert power series")
   ctx = base_ring(a)
   ainv = parent(a)()
   ainv.prec = a.prec
   ccall((:fq_nmod_poly_inv_series, :libflint), Void, 
                (Ptr{fq_nmod_rel_series}, Ptr{fq_nmod_rel_series}, Int, Ptr{FqNmodFiniteField}), 
               &ainv, &a, a.prec, &ctx)
   return ainv
end

###############################################################################
#
#   Unsafe functions
#
###############################################################################

function fit!(z::fq_nmod_rel_series, n::Int)
   ccall((:fq_nmod_poly_fit_length, :libflint), Void, 
         (Ptr{fq_nmod_rel_series}, Int, Ptr{FqNmodFiniteField}),
         &z, n, &base_ring(parent(z)))
end

function setcoeff!(z::fq_nmod_rel_series, n::Int, x::fq_nmod)
   ctx = base_ring(z)
   ccall((:fq_nmod_poly_set_coeff, :libflint), Void, 
                (Ptr{fq_nmod_rel_series}, Int, Ptr{fq_nmod}, Ptr{FqNmodFiniteField}), 
               &z, n, &x, &ctx)
end

function mul!(z::fq_nmod_rel_series, a::fq_nmod_rel_series, b::fq_nmod_rel_series)
   ctx = base_ring(z)
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
   ccall((:fq_nmod_poly_mullow, :libflint), Void, 
     (Ptr{fq_nmod_rel_series}, Ptr{fq_nmod_rel_series}, Ptr{fq_nmod_rel_series}, Int, Ptr{FqNmodFiniteField}),
               &z, &a, &b, lenz, &ctx)
end

function addeq!(a::fq_nmod_rel_series, b::fq_nmod_rel_series)
   ctx = base_ring(a)
   lena = length(a)
   lenb = length(b)
         
   prec = min(a.prec, b.prec)
 
   lena = min(lena, prec)
   lenb = min(lenb, prec)

   lenz = max(lena, lenb)
   a.prec = prec
   ccall((:fq_nmod_poly_add_series, :libflint), Void, 
     (Ptr{fq_nmod_rel_series}, Ptr{fq_nmod_rel_series}, Ptr{fq_nmod_rel_series}, Int, Ptr{FqNmodFiniteField}),
               &a, &a, &b, lenz, &ctx)
end

###############################################################################
#
#   Promotion rules
#
###############################################################################

Base.promote_rule{T <: Integer}(::Type{fq_nmod_rel_series}, ::Type{T}) = fq_nmod_rel_series

Base.promote_rule(::Type{fq_nmod_rel_series}, ::Type{fmpz}) = fq_nmod_rel_series

Base.promote_rule(::Type{fq_nmod_rel_series}, ::Type{fq_nmod}) = fq_nmod_rel_series

###############################################################################
#
#   Parent object call overload
#
###############################################################################

function Base.call(a::FqNmodRelSeriesRing)
   ctx = base_ring(a)
   z = fq_nmod_rel_series(ctx)
   z.prec = a.prec_max
   z.parent = a
   return z
end

function Base.call(a::FqNmodRelSeriesRing, b::Integer)
   ctx = base_ring(a)
   if b == 0
      z = fq_nmod_rel_series(ctx)
      z.prec = a.prec_max
   else
      z = fq_nmod_rel_series(ctx, [ctx(b)], 1, a.prec_max)
   end
   z.parent = a
   return z
end

function Base.call(a::FqNmodRelSeriesRing, b::fmpz)
   ctx = base_ring(a)
   if b == 0
      z = fq_nmod_rel_series(ctx)
      z.prec = a.prec_max
   else
      z = fq_nmod_rel_series(ctx, [ctx(b)], 1, a.prec_max)
   end
   z.parent = a
   return z
end

function Base.call(a::FqNmodRelSeriesRing, b::fq_nmod)
   ctx = base_ring(a)
   if b == 0
      z = fq_nmod_rel_series(ctx)
      z.prec = a.prec_max
   else
      z = fq_nmod_rel_series(ctx, [b], 1, a.prec_max)
   end
   z.parent = a
   return z
end

function Base.call(a::FqNmodRelSeriesRing, b::fq_nmod_rel_series)
   parent(b) != a && error("Unable to coerce power series")
   return b
end

function Base.call(a::FqNmodRelSeriesRing, b::Array{fq_nmod, 1}, len::Int, prec::Int)
   ctx = base_ring(a)
   z = fq_nmod_rel_series(ctx, b, len, prec)
   z.parent = a
   return z
end

###############################################################################
#
#   PowerSeriesRing constructor
#
###############################################################################

function PowerSeriesRing(R::FqNmodFiniteField, prec::Int, s::AbstractString{})
   S = Symbol(s)

   parent_obj = FqNmodRelSeriesRing(R, prec, S)

   return parent_obj, parent_obj([R(0), R(1)], 2, prec + 1)
end

