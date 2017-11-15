###############################################################################
#
#   fq_rel_series.jl : Power series over flint finite fields
#
###############################################################################

export fq_rel_series, FqRelSeriesRing

###############################################################################
#
#   Data type and parent object methods
#
###############################################################################

function O(a::fq_rel_series)
   val = pol_length(a) + valuation(a) - 1
   val < 0 && throw(DomainError())
   z = fq_rel_series(base_ring(a), Array{fq}(0), 0, val, val)
   z.parent = parent(a)
   return z
end

elem_type(::Type{FqRelSeriesRing}) = fq_rel_series

parent_type(::Type{fq_rel_series}) = FqRelSeriesRing

base_ring(R::FqRelSeriesRing) = R.base_ring

isexact(R::FqRelSeriesRing) = false

var(a::FqRelSeriesRing) = a.S

###############################################################################
#
#   Basic manipulation
#
###############################################################################

max_precision(R::FqRelSeriesRing) = R.prec_max

function normalise(a::fq_rel_series, len::Int)
   ctx = base_ring(a)
   if len > 0
      c = base_ring(a)()
      ccall((:fq_poly_get_coeff, :libflint), Void,
         (Ptr{fq}, Ptr{fq_rel_series}, Int, Ptr{FqFiniteField}),
                                                         &c, &a, len - 1, &ctx)
   end
   while len > 0 && iszero(c)
      len -= 1
      if len > 0
         ccall((:fq_poly_get_coeff, :libflint), Void,
            (Ptr{fq}, Ptr{fq_rel_series}, Int, Ptr{FqFiniteField}),
                                                         &c, &a, len - 1, &ctx)
      end
   end
   return len
end

function pol_length(x::fq_rel_series)
   return ccall((:fq_poly_length, :libflint), Int,
                (Ptr{fq_rel_series}, Ptr{FqFiniteField}), &x, &base_ring(x))
end

precision(x::fq_rel_series) = x.prec

function polcoeff(x::fq_rel_series, n::Int)
   z = base_ring(x)()
   if n < 0
      return z
   end
   ccall((:fq_poly_get_coeff, :libflint), Void,
         (Ptr{fq}, Ptr{fq_rel_series}, Int, Ptr{FqFiniteField}),
                                                      &z, &x, n, &base_ring(x))
   return z
end

zero(R::FqRelSeriesRing) = R(0)

one(R::FqRelSeriesRing) = R(1)

function gen(R::FqRelSeriesRing)
   z = fq_rel_series(base_ring(R), [base_ring(R)(1)], 1, max_precision(R) + 1, 1)
   z.parent = R
   return z
end

function deepcopy_internal(a::fq_rel_series, dict::ObjectIdDict)
   z = fq_rel_series(base_ring(a), a)
   z.prec = a.prec
   z.val = a.val
   z.parent = parent(a)
   return z
end

function renormalize!(z::fq_rel_series)
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
      ccall((:fq_poly_shift_right, :libflint), Void,
            (Ptr{fq_rel_series}, Ptr{fq_rel_series}, Int, Ptr{FqFiniteField}),
                                                      &z, &z, i, &base_ring(z))
   end
   return nothing
end

###############################################################################
#
#   AbstractString I/O
#
###############################################################################

function show(io::IO, a::FqRelSeriesRing)
   print(io, "Univariate power series ring in ", var(a), " over ")
   show(io, base_ring(a))
end

show_minus_one(::Type{fq_rel_series}) = show_minus_one(fq)

###############################################################################
#
#   Unary operators
#
###############################################################################

function -(x::fq_rel_series)
   z = parent(x)()
   ccall((:fq_poly_neg, :libflint), Void,
                (Ptr{fq_rel_series}, Ptr{fq_rel_series}, Ptr{FqFiniteField}),
               &z, &x, &base_ring(x))
   z.prec = x.prec
   z.val = x.val
   return z
end

###############################################################################
#
#   Binary operators
#
###############################################################################

function +(a::fq_rel_series, b::fq_rel_series)
   check_parent(a, b)
   lena = pol_length(a)
   lenb = pol_length(b)
   prec = min(a.prec, b.prec)
   val = min(a.val, b.val)
   lena = min(lena, prec - a.val)
   lenb = min(lenb, prec - b.val)
   z = parent(a)()
   ctx = base_ring(a)
   if a.val < b.val
      lenz = max(lena, lenb + b.val - a.val)
      ccall((:fq_poly_set_trunc, :libflint), Void,
            (Ptr{fq_rel_series}, Ptr{fq_rel_series}, Int, Ptr{FqFiniteField}),
            &z, &b, max(0, lenz - b.val + a.val), &ctx)
      ccall((:fq_poly_shift_left, :libflint), Void,
            (Ptr{fq_rel_series}, Ptr{fq_rel_series}, Int, Ptr{FqFiniteField}),
            &z, &z, b.val - a.val, &ctx)
      ccall((:fq_poly_add_series, :libflint), Void,
                (Ptr{fq_rel_series}, Ptr{fq_rel_series},
                 Ptr{fq_rel_series}, Int, Ptr{FqFiniteField}),
               &z, &z, &a, lenz, &ctx)
   elseif b.val < a.val
      lenz = max(lena + a.val - b.val, lenb)
      ccall((:fq_poly_set_trunc, :libflint), Void,
            (Ptr{fq_rel_series}, Ptr{fq_rel_series}, Int, Ptr{FqFiniteField}),
            &z, &a, max(0, lenz - a.val + b.val), &ctx)
      ccall((:fq_poly_shift_left, :libflint), Void,
            (Ptr{fq_rel_series}, Ptr{fq_rel_series}, Int, Ptr{FqFiniteField}),
            &z, &z, a.val - b.val, &ctx)
      ccall((:fq_poly_add_series, :libflint), Void,
                (Ptr{fq_rel_series}, Ptr{fq_rel_series},
                 Ptr{fq_rel_series}, Int, Ptr{FqFiniteField}),
               &z, &z, &b, lenz, &ctx)
   else
      lenz = max(lena, lenb)
      ccall((:fq_poly_add_series, :libflint), Void,
                (Ptr{fq_rel_series}, Ptr{fq_rel_series},
                 Ptr{fq_rel_series}, Int, Ptr{FqFiniteField}),
               &z, &a, &b, lenz, &ctx)
   end
   z.prec = prec
   z.val = val
   renormalize!(z)
   return z
end

function -(a::fq_rel_series, b::fq_rel_series)
   check_parent(a, b)
   lena = pol_length(a)
   lenb = pol_length(b)
   prec = min(a.prec, b.prec)
   val = min(a.val, b.val)
   lena = min(lena, prec - a.val)
   lenb = min(lenb, prec - b.val)
   lenz = max(lena, lenb)
   z = parent(a)()
   ctx = base_ring(a)
   if a.val < b.val
      lenz = max(lena, lenb + b.val - a.val)
      ccall((:fq_poly_set_trunc, :libflint), Void,
            (Ptr{fq_rel_series}, Ptr{fq_rel_series}, Int, Ptr{FqFiniteField}),
            &z, &b, max(0, lenz - b.val + a.val), &ctx)
      ccall((:fq_poly_shift_left, :libflint), Void,
            (Ptr{fq_rel_series}, Ptr{fq_rel_series}, Int, Ptr{FqFiniteField}),
            &z, &z, b.val - a.val, &ctx)
      ccall((:fq_poly_neg, :libflint), Void,
            (Ptr{fq_rel_series}, Ptr{fq_rel_series}, Ptr{FqFiniteField}),
            &z, &z, &ctx)
      ccall((:fq_poly_add_series, :libflint), Void,
                (Ptr{fq_rel_series}, Ptr{fq_rel_series},
                 Ptr{fq_rel_series}, Int, Ptr{FqFiniteField}),
               &z, &z, &a, lenz, &ctx)
   elseif b.val < a.val
      lenz = max(lena + a.val - b.val, lenb)
      ccall((:fq_poly_set_trunc, :libflint), Void,
            (Ptr{fq_rel_series}, Ptr{fq_rel_series}, Int, Ptr{FqFiniteField}),
            &z, &a, max(0, lenz - a.val + b.val), &ctx)
      ccall((:fq_poly_shift_left, :libflint), Void,
            (Ptr{fq_rel_series}, Ptr{fq_rel_series}, Int, Ptr{FqFiniteField}),
            &z, &z, a.val - b.val, &ctx)
      ccall((:fq_poly_sub_series, :libflint), Void,
                (Ptr{fq_rel_series}, Ptr{fq_rel_series},
                 Ptr{fq_rel_series}, Int, Ptr{FqFiniteField}),
               &z, &z, &b, lenz, &ctx)
   else
      lenz = max(lena, lenb)
      ccall((:fq_poly_sub_series, :libflint), Void,
                (Ptr{fq_rel_series}, Ptr{fq_rel_series},
                 Ptr{fq_rel_series}, Int, Ptr{FqFiniteField}),
               &z, &a, &b, lenz, &ctx)
   end
   z.prec = prec
   z.val = val
   renormalize!(z)
   return z
end

function *(a::fq_rel_series, b::fq_rel_series)
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
   ccall((:fq_poly_mullow, :libflint), Void,
                (Ptr{fq_rel_series}, Ptr{fq_rel_series},
                 Ptr{fq_rel_series}, Int, Ptr{FqFiniteField}),
               &z, &a, &b, lenz, &base_ring(a))
   return z
end

###############################################################################
#
#   Ad hoc binary operators
#
###############################################################################

function *(x::fq, y::fq_rel_series)
   z = parent(y)()
   z.prec = y.prec
   z.val = y.val
   ccall((:fq_poly_scalar_mul_fq, :libflint), Void,
                (Ptr{fq_rel_series}, Ptr{fq_rel_series},
                 Ptr{fq}, Ptr{FqFiniteField}),
               &z, &y, &x, &base_ring(y))
   return z
end

*(x::fq_rel_series, y::fq) = y * x

###############################################################################
#
#   Shifting
#
###############################################################################

function shift_left(x::fq_rel_series, len::Int)
   len < 0 && throw(DomainError())
   xlen = pol_length(x)
   z = fq_rel_series(base_ring(x), x)
   z.prec = x.prec + len
   z.val = x.val + len
   z.parent = parent(x)
   return z
end

function shift_right(x::fq_rel_series, len::Int)
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
      ccall((:fq_poly_shift_right, :libflint), Void,
                (Ptr{fq_rel_series}, Ptr{fq_rel_series},
                 Int, Ptr{FqFiniteField}),
               &z, &x, xlen - zlen, &base_ring(x))
      renormalize!(z)
   end
   return z
end

###############################################################################
#
#   Truncation
#
###############################################################################

function truncate(x::fq_rel_series, prec::Int)
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
      ccall((:fq_poly_set_trunc, :libflint), Void,
                (Ptr{fq_rel_series}, Ptr{fq_rel_series},
                 Int, Ptr{FqFiniteField}),
               &z, &x, min(prec - xval, xlen), &base_ring(x))
   end
   return z
end

###############################################################################
#
#   Powering
#
###############################################################################

function ^(a::fq_rel_series, b::Int)
   b < 0 && throw(DomainError())
   if isgen(a)
      z = parent(a)()
      z = setcoeff!(z, 0, base_ring(a)(1))
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

function ==(x::fq_rel_series, y::fq_rel_series)
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
   return Bool(ccall((:fq_poly_equal_trunc, :libflint), Cint,
                (Ptr{fq_rel_series}, Ptr{fq_rel_series},
                 Int, Ptr{FqFiniteField}),
               &x, &y, xlen, &base_ring(x)))
end

function isequal(x::fq_rel_series, y::fq_rel_series)
   if parent(x) != parent(y)
      return false
   end
   if x.prec != y.prec || x.val != y.val || pol_length(x) != pol_length(y)
      return false
   end
   return Bool(ccall((:fq_poly_equal, :libflint), Cint,
                (Ptr{fq_rel_series}, Ptr{fq_rel_series},
                 Int, Ptr{FqFiniteField}),
               &x, &y, pol_length(x), &base_ring(x)))
end

###############################################################################
#
#   Exact division
#
###############################################################################

function divexact(x::fq_rel_series, y::fq_rel_series)
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
      ccall((:fq_poly_div_series, :libflint), Void,
                (Ptr{fq_rel_series}, Ptr{fq_rel_series},
                 Ptr{fq_rel_series}, Int, Ptr{FqFiniteField}),
               &z, &x, &y, prec, &base_ring(x))
   end
   return z
end

###############################################################################
#
#   Ad hoc exact division
#
###############################################################################

function divexact(x::fq_rel_series, y::fq)
   iszero(y) && throw(DivideError())
   z = parent(x)()
   z.prec = x.prec
   z.prec = x.prec
   z.val = x.val
   ccall((:fq_poly_scalar_div_fq, :libflint), Void,
                (Ptr{fq_rel_series}, Ptr{fq_rel_series},
                 Ptr{fq}, Ptr{FqFiniteField}),
               &z, &x, &y, &base_ring(x))
   return z
end

###############################################################################
#
#   Inversion
#
###############################################################################

function inv(a::fq_rel_series)
   iszero(a) && throw(DivideError())
   !isunit(a) && error("Unable to invert power series")
   ainv = parent(a)()
   ainv.prec = a.prec
   ainv.val = 0
   ccall((:fq_poly_inv_series, :libflint), Void,
         (Ptr{fq_rel_series}, Ptr{fq_rel_series}, Int, Ptr{FqFiniteField}),
               &ainv, &a, a.prec, &base_ring(a))
   return ainv
end

###############################################################################
#
#   Unsafe functions
#
###############################################################################

function zero!(x::fq_rel_series)
  ccall((:fq_poly_zero, :libflint), Void,
                   (Ptr{fq_rel_series}, Ptr{FqFiniteField}), &x, &base_ring(x))
  x.prec = parent(x).prec_max
  return x
end

function fit!(z::fq_rel_series, n::Int)
   ccall((:fq_poly_fit_length, :libflint), Void,
         (Ptr{fq_rel_series}, Int, Ptr{FqFiniteField}),
         &z, n, &base_ring(z))
   return nothing
end

function setcoeff!(z::fq_rel_series, n::Int, x::fmpz)
   ccall((:fq_poly_set_coeff_fmpz, :libflint), Void,
                (Ptr{fq_rel_series}, Int, Ptr{fmpz}, Ptr{FqFiniteField}),
               &z, n, &x, &base_ring(z))
   return z
end

function setcoeff!(z::fq_rel_series, n::Int, x::fq)
   ccall((:fq_poly_set_coeff, :libflint), Void,
                (Ptr{fq_rel_series}, Int, Ptr{fq}, Ptr{FqFiniteField}),
               &z, n, &x, &base_ring(z))
   return z
end

function mul!(z::fq_rel_series, a::fq_rel_series, b::fq_rel_series)
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
   ccall((:fq_poly_mullow, :libflint), Void,
         (Ptr{fq_rel_series}, Ptr{fq_rel_series},
          Ptr{fq_rel_series}, Int, Ptr{FqFiniteField}),
               &z, &a, &b, lenz, &base_ring(z))
   return z
end

function addeq!(a::fq_rel_series, b::fq_rel_series)
   lena = pol_length(a)
   lenb = pol_length(b)
   prec = min(a.prec, b.prec)
   val = min(a.val, b.val)
   lena = min(lena, prec - a.val)
   lenb = min(lenb, prec - b.val)
   ctx = base_ring(a)
   if a.val < b.val
      z = fq_rel_series(base_ring(a))
      lenz = max(lena, lenb + b.val - a.val)
      ccall((:fq_poly_set_trunc, :libflint), Void,
            (Ptr{fq_rel_series}, Ptr{fq_rel_series}, Int, Ptr{FqFiniteField}),
            &z, &b, max(0, lenz - b.val + a.val), ctx)
      ccall((:fq_poly_shift_left, :libflint), Void,
            (Ptr{fq_rel_series}, Ptr{fq_rel_series}, Int, Ptr{FqFiniteField}),
            &z, &z, b.val - a.val, ctx)
      ccall((:fq_poly_add_series, :libflint), Void,
                (Ptr{fq_rel_series}, Ptr{fq_rel_series},
                 Ptr{fq_rel_series}, Int, Ptr{FqFiniteField}),
               &a, &a, &z, lenz, &ctx)
   elseif b.val < a.val
      lenz = max(lena + a.val - b.val, lenb)
      ccall((:fq_poly_truncate, :libflint), Void,
            (Ptr{fq_rel_series}, Int, Ptr{FqFiniteField}),
            &a, max(0, lenz - a.val + b.val), &ctx)
      ccall((:fq_poly_shift_left, :libflint), Void,
            (Ptr{fq_rel_series}, Ptr{fq_rel_series}, Int, Ptr{FqFiniteField}),
            &a, &a, a.val - b.val, &ctx)
      ccall((:fq_poly_add_series, :libflint), Void,
                (Ptr{fq_rel_series}, Ptr{fq_rel_series},
                 Ptr{fq_rel_series}, Int, Ptr{FqFiniteField}),
               &a, &a, &b, lenz, &ctx)
   else
      lenz = max(lena, lenb)
      ccall((:fq_poly_add_series, :libflint), Void,
                (Ptr{fq_rel_series}, Ptr{fq_rel_series},
                 Ptr{fq_rel_series}, Int, Ptr{FqFiniteField}),
               &a, &a, &b, lenz, &ctx)
   end
   a.prec = prec
   a.val = val
   renormalize!(a)
   return a
end

function add!(c::fq_rel_series, a::fq_rel_series, b::fq_rel_series)
   ctx = base_ring(a)
   lena = length(a)
   lenb = length(b)

   prec = min(a.prec, b.prec)

   lena = min(lena, prec)
   lenb = min(lenb, prec)

   lenc = max(lena, lenb)
   c.prec = prec
   ccall((:fq_poly_add_series, :libflint), Void,
     (Ptr{fq_rel_series}, Ptr{fq_rel_series}, Ptr{fq_rel_series}, Int, Ptr{FqFiniteField}),
               &c, &a, &b, lenc, &ctx)
   return c
end

###############################################################################
#
#   Promotion rules
#
###############################################################################

promote_rule(::Type{fq_rel_series}, ::Type{T}) where {T <: Integer} = fq_rel_series

promote_rule(::Type{fq_rel_series}, ::Type{fmpz}) = fq_rel_series

promote_rule(::Type{fq_rel_series}, ::Type{fq}) = fq_rel_series

###############################################################################
#
#   Parent object call overload
#
###############################################################################

function (a::FqRelSeriesRing)()
   ctx = base_ring(a)
   z = fq_rel_series(ctx)
   z.prec = a.prec_max
   z.val = a.prec_max
   z.parent = a
   return z
end

function (a::FqRelSeriesRing)(b::Integer)
   ctx = base_ring(a)
   if b == 0
      z = fq_rel_series(ctx)
      z.prec = a.prec_max
   else
      z = fq_rel_series(ctx, [ctx(b)], 1, a.prec_max, 0)
   end
   z.parent = a
   return z
end

function (a::FqRelSeriesRing)(b::fmpz)
   ctx = base_ring(a)
   if iszero(b)
      z = fq_rel_series(ctx)
      z.prec = a.prec_max
   else
      z = fq_rel_series(ctx, [ctx(b)], 1, a.prec_max, 0)
   end
   z.parent = a
   return z
end

function (a::FqRelSeriesRing)(b::fq)
   ctx = base_ring(a)
   if iszero(b)
      z = fq_rel_series(ctx)
      z.prec = a.prec_max
   else
      z = fq_rel_series(ctx, [b], 1, a.prec_max, 0)
   end
   z.parent = a
   return z
end

function (a::FqRelSeriesRing)(b::fq_rel_series)
   parent(b) != a && error("Unable to coerce power series")
   return b
end

function (a::FqRelSeriesRing)(b::Array{fq, 1}, len::Int, prec::Int, val::Int)
   ctx = base_ring(a)
   z = fq_rel_series(ctx, b, len, prec, val)
   z.parent = a
   return z
end
