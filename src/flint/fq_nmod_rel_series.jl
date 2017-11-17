###############################################################################
#
#   fq_nmod_rel_series.jl : Power series over flint finite fields (small char)
#
###############################################################################

export fq_nmod_rel_series, FqNmodRelSeriesRing

###############################################################################
#
#   Data type and parent object methods
#
###############################################################################

function O(a::fq_nmod_rel_series)
   val = pol_length(a) + valuation(a) - 1
   val < 0 && throw(DomainError())
   z = fq_nmod_rel_series(base_ring(a), Array{fq_nmod}(0), 0, val, val)
   z.parent = parent(a)
   return z
end

elem_type(::Type{FqNmodRelSeriesRing}) = fq_nmod_rel_series

parent_type(::Type{fq_nmod_rel_series}) = FqNmodRelSeriesRing

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
      c = base_ring(a)()
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

function pol_length(x::fq_nmod_rel_series)
   return ccall((:fq_nmod_poly_length, :libflint), Int,
                (Ptr{fq_nmod_rel_series}, Ptr{FqNmodFiniteField}), &x, &base_ring(x))
end

precision(x::fq_nmod_rel_series) = x.prec

function polcoeff(x::fq_nmod_rel_series, n::Int)
   z = base_ring(x)()
   if n < 0
      return z
   end
   ccall((:fq_nmod_poly_get_coeff, :libflint), Void,
         (Ptr{fq_nmod}, Ptr{fq_nmod_rel_series}, Int, Ptr{FqNmodFiniteField}),
                                                      &z, &x, n, &base_ring(x))
   return z
end

zero(R::FqNmodRelSeriesRing) = R(0)

one(R::FqNmodRelSeriesRing) = R(1)

function gen(R::FqNmodRelSeriesRing)
   z = fq_nmod_rel_series(base_ring(R), [base_ring(R)(1)], 1, max_precision(R) + 1, 1)
   z.parent = R
   return z
end

function deepcopy_internal(a::fq_nmod_rel_series, dict::ObjectIdDict)
   z = fq_nmod_rel_series(base_ring(a), a)
   z.prec = a.prec
   z.val = a.val
   z.parent = parent(a)
   return z
end

function renormalize!(z::fq_nmod_rel_series)
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
      ccall((:fq_nmod_poly_shift_right, :libflint), Void,
            (Ptr{fq_nmod_rel_series}, Ptr{fq_nmod_rel_series}, Int, Ptr{FqNmodFiniteField}),
                                                      &z, &z, i, &base_ring(z))
   end
   return nothing
end

###############################################################################
#
#   AbstractString I/O
#
###############################################################################

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
   z = parent(x)()
   ccall((:fq_nmod_poly_neg, :libflint), Void,
                (Ptr{fq_nmod_rel_series}, Ptr{fq_nmod_rel_series}, Ptr{FqNmodFiniteField}),
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

function +(a::fq_nmod_rel_series, b::fq_nmod_rel_series)
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
      ccall((:fq_nmod_poly_set_trunc, :libflint), Void,
            (Ptr{fq_nmod_rel_series}, Ptr{fq_nmod_rel_series}, Int, Ptr{FqNmodFiniteField}),
            &z, &b, max(0, lenz - b.val + a.val), &ctx)
      ccall((:fq_nmod_poly_shift_left, :libflint), Void,
            (Ptr{fq_nmod_rel_series}, Ptr{fq_nmod_rel_series}, Int, Ptr{FqNmodFiniteField}),
            &z, &z, b.val - a.val, &ctx)
      ccall((:fq_nmod_poly_add_series, :libflint), Void,
                (Ptr{fq_nmod_rel_series}, Ptr{fq_nmod_rel_series},
                 Ptr{fq_nmod_rel_series}, Int, Ptr{FqNmodFiniteField}),
               &z, &z, &a, lenz, &ctx)
   elseif b.val < a.val
      lenz = max(lena + a.val - b.val, lenb)
      ccall((:fq_nmod_poly_set_trunc, :libflint), Void,
            (Ptr{fq_nmod_rel_series}, Ptr{fq_nmod_rel_series}, Int, Ptr{FqNmodFiniteField}),
            &z, &a, max(0, lenz - a.val + b.val), &ctx)
      ccall((:fq_nmod_poly_shift_left, :libflint), Void,
            (Ptr{fq_nmod_rel_series}, Ptr{fq_nmod_rel_series}, Int, Ptr{FqNmodFiniteField}),
            &z, &z, a.val - b.val, &ctx)
      ccall((:fq_nmod_poly_add_series, :libflint), Void,
                (Ptr{fq_nmod_rel_series}, Ptr{fq_nmod_rel_series},
                 Ptr{fq_nmod_rel_series}, Int, Ptr{FqNmodFiniteField}),
               &z, &z, &b, lenz, &ctx)
   else
      lenz = max(lena, lenb)
      ccall((:fq_nmod_poly_add_series, :libflint), Void,
                (Ptr{fq_nmod_rel_series}, Ptr{fq_nmod_rel_series},
                 Ptr{fq_nmod_rel_series}, Int, Ptr{FqNmodFiniteField}),
               &z, &a, &b, lenz, &ctx)
   end
   z.prec = prec
   z.val = val
   renormalize!(z)
   return z
end

function -(a::fq_nmod_rel_series, b::fq_nmod_rel_series)
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
      ccall((:fq_nmod_poly_set_trunc, :libflint), Void,
            (Ptr{fq_nmod_rel_series}, Ptr{fq_nmod_rel_series}, Int, Ptr{FqNmodFiniteField}),
            &z, &b, max(0, lenz - b.val + a.val), &ctx)
      ccall((:fq_nmod_poly_shift_left, :libflint), Void,
            (Ptr{fq_nmod_rel_series}, Ptr{fq_nmod_rel_series}, Int, Ptr{FqNmodFiniteField}),
            &z, &z, b.val - a.val, &ctx)
      ccall((:fq_nmod_poly_neg, :libflint), Void,
            (Ptr{fq_nmod_rel_series}, Ptr{fq_nmod_rel_series}, Ptr{FqNmodFiniteField}),
            &z, &z, &ctx)
      ccall((:fq_nmod_poly_add_series, :libflint), Void,
                (Ptr{fq_nmod_rel_series}, Ptr{fq_nmod_rel_series},
                 Ptr{fq_nmod_rel_series}, Int, Ptr{FqNmodFiniteField}),
               &z, &z, &a, lenz, &ctx)
   elseif b.val < a.val
      lenz = max(lena + a.val - b.val, lenb)
      ccall((:fq_nmod_poly_set_trunc, :libflint), Void,
            (Ptr{fq_nmod_rel_series}, Ptr{fq_nmod_rel_series}, Int, Ptr{FqNmodFiniteField}),
            &z, &a, max(0, lenz - a.val + b.val), &ctx)
      ccall((:fq_nmod_poly_shift_left, :libflint), Void,
            (Ptr{fq_nmod_rel_series}, Ptr{fq_nmod_rel_series}, Int, Ptr{FqNmodFiniteField}),
            &z, &z, a.val - b.val, &ctx)
      ccall((:fq_nmod_poly_sub_series, :libflint), Void,
                (Ptr{fq_nmod_rel_series}, Ptr{fq_nmod_rel_series},
                 Ptr{fq_nmod_rel_series}, Int, Ptr{FqNmodFiniteField}),
               &z, &z, &b, lenz, &ctx)
   else
      lenz = max(lena, lenb)
      ccall((:fq_nmod_poly_sub_series, :libflint), Void,
                (Ptr{fq_nmod_rel_series}, Ptr{fq_nmod_rel_series},
                 Ptr{fq_nmod_rel_series}, Int, Ptr{FqNmodFiniteField}),
               &z, &a, &b, lenz, &ctx)
   end
   z.prec = prec
   z.val = val
   renormalize!(z)
   return z
end

function *(a::fq_nmod_rel_series, b::fq_nmod_rel_series)
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
   ccall((:fq_nmod_poly_mullow, :libflint), Void,
                (Ptr{fq_nmod_rel_series}, Ptr{fq_nmod_rel_series},
                 Ptr{fq_nmod_rel_series}, Int, Ptr{FqNmodFiniteField}),
               &z, &a, &b, lenz, &base_ring(a))
   return z
end

###############################################################################
#
#   Ad hoc binary operators
#
###############################################################################

function *(x::fq_nmod, y::fq_nmod_rel_series)
   z = parent(y)()
   z.prec = y.prec
   z.val = y.val
   ccall((:fq_nmod_poly_scalar_mul_fq_nmod, :libflint), Void,
                (Ptr{fq_nmod_rel_series}, Ptr{fq_nmod_rel_series},
                 Ptr{fq_nmod}, Ptr{FqNmodFiniteField}),
               &z, &y, &x, &base_ring(y))
   return z
end

*(x::fq_nmod_rel_series, y::fq_nmod) = y * x

###############################################################################
#
#   Shifting
#
###############################################################################

function shift_left(x::fq_nmod_rel_series, len::Int)
   len < 0 && throw(DomainError())
   xlen = pol_length(x)
   z = fq_nmod_rel_series(base_ring(x), x)
   z.prec = x.prec + len
   z.val = x.val + len
   z.parent = parent(x)
   return z
end

function shift_right(x::fq_nmod_rel_series, len::Int)
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
      ccall((:fq_nmod_poly_shift_right, :libflint), Void,
                (Ptr{fq_nmod_rel_series}, Ptr{fq_nmod_rel_series},
                 Int, Ptr{FqNmodFiniteField}),
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

function truncate(x::fq_nmod_rel_series, prec::Int)
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
      ccall((:fq_nmod_poly_set_trunc, :libflint), Void,
                (Ptr{fq_nmod_rel_series}, Ptr{fq_nmod_rel_series},
                 Int, Ptr{FqNmodFiniteField}),
               &z, &x, min(prec - xval, xlen), &base_ring(x))
   end
   return z
end

###############################################################################
#
#   Powering
#
###############################################################################

function ^(a::fq_nmod_rel_series, b::Int)
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

function ==(x::fq_nmod_rel_series, y::fq_nmod_rel_series)
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
   return Bool(ccall((:fq_nmod_poly_equal_trunc, :libflint), Cint,
                (Ptr{fq_nmod_rel_series}, Ptr{fq_nmod_rel_series},
                 Int, Ptr{FqNmodFiniteField}),
               &x, &y, xlen, &base_ring(x)))
end

function isequal(x::fq_nmod_rel_series, y::fq_nmod_rel_series)
   if parent(x) != parent(y)
      return false
   end
   if x.prec != y.prec || x.val != y.val || pol_length(x) != pol_length(y)
      return false
   end
   return Bool(ccall((:fq_nmod_poly_equal, :libflint), Cint,
                (Ptr{fq_nmod_rel_series}, Ptr{fq_nmod_rel_series},
                 Int, Ptr{FqNmodFiniteField}),
               &x, &y, pol_length(x), &base_ring(x)))
end

###############################################################################
#
#   Exact division
#
###############################################################################

function divexact(x::fq_nmod_rel_series, y::fq_nmod_rel_series)
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
      ccall((:fq_nmod_poly_div_series, :libflint), Void,
                (Ptr{fq_nmod_rel_series}, Ptr{fq_nmod_rel_series},
                 Ptr{fq_nmod_rel_series}, Int, Ptr{FqNmodFiniteField}),
               &z, &x, &y, prec, &base_ring(x))
   end
   return z
end

###############################################################################
#
#   Ad hoc exact division
#
###############################################################################

function divexact(x::fq_nmod_rel_series, y::fq_nmod)
   iszero(y) && throw(DivideError())
   z = parent(x)()
   z.prec = x.prec
   z.prec = x.prec
   z.val = x.val
   ccall((:fq_nmod_poly_scalar_div_fq_nmod, :libflint), Void,
                (Ptr{fq_nmod_rel_series}, Ptr{fq_nmod_rel_series},
                 Ptr{fq_nmod}, Ptr{FqNmodFiniteField}),
               &z, &x, &y, &base_ring(x))
   return z
end

###############################################################################
#
#   Inversion
#
###############################################################################

function inv(a::fq_nmod_rel_series)
   iszero(a) && throw(DivideError())
   !isunit(a) && error("Unable to invert power series")
   ainv = parent(a)()
   ainv.prec = a.prec
   ainv.val = 0
   ccall((:fq_nmod_poly_inv_series, :libflint), Void,
         (Ptr{fq_nmod_rel_series}, Ptr{fq_nmod_rel_series}, Int, Ptr{FqNmodFiniteField}),
               &ainv, &a, a.prec, &base_ring(a))
   return ainv
end

###############################################################################
#
#   Unsafe functions
#
###############################################################################

function zero!(x::fq_nmod_rel_series)
  ccall((:fq_nmod_poly_zero, :libflint), Void,
                   (Ptr{fq_nmod_rel_series}, Ptr{FqNmodFiniteField}), &x, &base_ring(x))
  x.prec = parent(x).prec_max
  return x
end

function fit!(z::fq_nmod_rel_series, n::Int)
   ccall((:fq_nmod_poly_fit_length, :libflint), Void,
         (Ptr{fq_nmod_rel_series}, Int, Ptr{FqNmodFiniteField}),
         &z, n, &base_ring(z))
   return nothing
end

function setcoeff!(z::fq_nmod_rel_series, n::Int, x::fmpz)
   ccall((:fq_nmod_poly_set_coeff_fmpz, :libflint), Void,
                (Ptr{fq_nmod_rel_series}, Int, Ptr{fmpz}, Ptr{FqNmodFiniteField}),
               &z, n, &x, &base_ring(z))
   return z
end

function setcoeff!(z::fq_nmod_rel_series, n::Int, x::fq_nmod)
   ccall((:fq_nmod_poly_set_coeff, :libflint), Void,
                (Ptr{fq_nmod_rel_series}, Int, Ptr{fq_nmod}, Ptr{FqNmodFiniteField}),
               &z, n, &x, &base_ring(z))
   return z
end

function mul!(z::fq_nmod_rel_series, a::fq_nmod_rel_series, b::fq_nmod_rel_series)
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
   ccall((:fq_nmod_poly_mullow, :libflint), Void,
         (Ptr{fq_nmod_rel_series}, Ptr{fq_nmod_rel_series},
          Ptr{fq_nmod_rel_series}, Int, Ptr{FqNmodFiniteField}),
               &z, &a, &b, lenz, &base_ring(z))
   return z
end

function addeq!(a::fq_nmod_rel_series, b::fq_nmod_rel_series)
   lena = pol_length(a)
   lenb = pol_length(b)
   prec = min(a.prec, b.prec)
   val = min(a.val, b.val)
   lena = min(lena, prec - a.val)
   lenb = min(lenb, prec - b.val)
   ctx = base_ring(a)
   if a.val < b.val
      z = fq_nmod_rel_series(base_ring(a))
      lenz = max(lena, lenb + b.val - a.val)
      ccall((:fq_nmod_poly_set_trunc, :libflint), Void,
            (Ptr{fq_nmod_rel_series}, Ptr{fq_nmod_rel_series}, Int, Ptr{FqNmodFiniteField}),
            &z, &b, max(0, lenz - b.val + a.val), ctx)
      ccall((:fq_nmod_poly_shift_left, :libflint), Void,
            (Ptr{fq_nmod_rel_series}, Ptr{fq_nmod_rel_series}, Int, Ptr{FqNmodFiniteField}),
            &z, &z, b.val - a.val, ctx)
      ccall((:fq_nmod_poly_add_series, :libflint), Void,
                (Ptr{fq_nmod_rel_series}, Ptr{fq_nmod_rel_series},
                 Ptr{fq_nmod_rel_series}, Int, Ptr{FqNmodFiniteField}),
               &a, &a, &z, lenz, &ctx)
   elseif b.val < a.val
      lenz = max(lena + a.val - b.val, lenb)
      ccall((:fq_nmod_poly_truncate, :libflint), Void,
            (Ptr{fq_nmod_rel_series}, Int, Ptr{FqNmodFiniteField}),
            &a, max(0, lenz - a.val + b.val), &ctx)
      ccall((:fq_nmod_poly_shift_left, :libflint), Void,
            (Ptr{fq_nmod_rel_series}, Ptr{fq_nmod_rel_series}, Int, Ptr{FqNmodFiniteField}),
            &a, &a, a.val - b.val, &ctx)
      ccall((:fq_nmod_poly_add_series, :libflint), Void,
                (Ptr{fq_nmod_rel_series}, Ptr{fq_nmod_rel_series},
                 Ptr{fq_nmod_rel_series}, Int, Ptr{FqNmodFiniteField}),
               &a, &a, &b, lenz, &ctx)
   else
      lenz = max(lena, lenb)
      ccall((:fq_nmod_poly_add_series, :libflint), Void,
                (Ptr{fq_nmod_rel_series}, Ptr{fq_nmod_rel_series},
                 Ptr{fq_nmod_rel_series}, Int, Ptr{FqNmodFiniteField}),
               &a, &a, &b, lenz, &ctx)
   end
   a.prec = prec
   a.val = val
   renormalize!(a)
   return a
end

function add!(c::fq_nmod_rel_series, a::fq_nmod_rel_series, b::fq_nmod_rel_series)
   ctx = base_ring(a)
   lena = length(a)
   lenb = length(b)

   prec = min(a.prec, b.prec)

   lena = min(lena, prec)
   lenb = min(lenb, prec)

   lenc = max(lena, lenb)
   c.prec = prec
   ccall((:fq_nmod_poly_add_series, :libflint), Void,
     (Ptr{fq_nmod_rel_series}, Ptr{fq_nmod_rel_series}, Ptr{fq_nmod_rel_series}, Int, Ptr{FqNmodFiniteField}),
               &c, &a, &b, lenc, &ctx)
   return c
end

###############################################################################
#
#   Promotion rules
#
###############################################################################

promote_rule(::Type{fq_nmod_rel_series}, ::Type{T}) where {T <: Integer} = fq_nmod_rel_series

promote_rule(::Type{fq_nmod_rel_series}, ::Type{fmpz}) = fq_nmod_rel_series

promote_rule(::Type{fq_nmod_rel_series}, ::Type{fq_nmod}) = fq_nmod_rel_series

###############################################################################
#
#   Parent object call overload
#
###############################################################################

function (a::FqNmodRelSeriesRing)()
   ctx = base_ring(a)
   z = fq_nmod_rel_series(ctx)
   z.prec = a.prec_max
   z.val = a.prec_max
   z.parent = a
   return z
end

function (a::FqNmodRelSeriesRing)(b::Integer)
   ctx = base_ring(a)
   if b == 0
      z = fq_nmod_rel_series(ctx)
      z.prec = a.prec_max
   else
      z = fq_nmod_rel_series(ctx, [ctx(b)], 1, a.prec_max, 0)
   end
   z.parent = a
   return z
end

function (a::FqNmodRelSeriesRing)(b::fmpz)
   ctx = base_ring(a)
   if iszero(b)
      z = fq_nmod_rel_series(ctx)
      z.prec = a.prec_max
   else
      z = fq_nmod_rel_series(ctx, [ctx(b)], 1, a.prec_max, 0)
   end
   z.parent = a
   return z
end

function (a::FqNmodRelSeriesRing)(b::fq_nmod)
   ctx = base_ring(a)
   if iszero(b)
      z = fq_nmod_rel_series(ctx)
      z.prec = a.prec_max
   else
      z = fq_nmod_rel_series(ctx, [b], 1, a.prec_max, 0)
   end
   z.parent = a
   return z
end

function (a::FqNmodRelSeriesRing)(b::fq_nmod_rel_series)
   parent(b) != a && error("Unable to coerce power series")
   return b
end

function (a::FqNmodRelSeriesRing)(b::Array{fq_nmod, 1}, len::Int, prec::Int, val::Int)
   ctx = base_ring(a)
   z = fq_nmod_rel_series(ctx, b, len, prec, val)
   z.parent = a
   return z
end
