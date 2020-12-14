###############################################################################
#
#   fmpz_mod_rel_series.jl : Power series over flint fmpz integers mod n
#
###############################################################################

export fmpz_mod_rel_series, FmpzModRelSeriesRing

###############################################################################
#
#   Data type and parent object methods
#
###############################################################################

function O(a::fmpz_mod_rel_series)
   val = pol_length(a) + valuation(a) - 1
   val < 0 && throw(DomainError(val, "Valuation must be non-negative"))
   z = fmpz_mod_rel_series(modulus(a), Vector{fmpz}(undef, 0), 0, val, val)
   z.parent = parent(a)
   return z
end

elem_type(::Type{FmpzModRelSeriesRing}) = fmpz_mod_rel_series

parent_type(::Type{fmpz_mod_rel_series}) = FmpzModRelSeriesRing

base_ring(R::FmpzModRelSeriesRing) = R.base_ring

var(a::FmpzModRelSeriesRing) = a.S

###############################################################################
#
#   Basic manipulation
#
###############################################################################

max_precision(R::FmpzModRelSeriesRing) = R.prec_max

function normalise(a::fmpz_mod_rel_series, len::Int)
   if len > 0
      c = fmpz()
      while len > 0
         ccall((:fmpz_mod_poly_get_coeff_fmpz, libflint), Nothing,
               (Ref{fmpz}, Ref{fmpz_mod_rel_series}, Int,
                Ref{fmpz_mod_ctx_struct}),
               c, a, len - 1, a.parent.base_ring.ninv)
         if !iszero(c)
            break
         end
         len -= 1
      end
   end
   return len
end

function pol_length(x::fmpz_mod_rel_series)
   return x.length
#   return ccall((:fmpz_mod_poly_length, libflint), Int,
#                (Ref{fmpz_mod_rel_series}, Ref{fmpz_mod_ctx_struct}),
#                x, x.parent.base_ring.ninv)
end

precision(x::fmpz_mod_rel_series) = x.prec

function polcoeff(x::fmpz_mod_rel_series, n::Int)
   R = base_ring(x)
   if n < 0
      return R(0)
   end
   z = fmpz()
   ccall((:fmpz_mod_poly_get_coeff_fmpz, libflint), Nothing,
         (Ref{fmpz}, Ref{fmpz_mod_rel_series}, Int, Ref{fmpz_mod_ctx_struct}),
         z, x, n, x.parent.base_ring.ninv)
   return R(z)
end

zero(R::FmpzModRelSeriesRing) = R(0)

one(R::FmpzModRelSeriesRing) = R(1)

function gen(R::FmpzModRelSeriesRing)
   z = fmpz_mod_rel_series(modulus(R), [fmpz(1)], 1, max_precision(R) + 1, 1)
   z.parent = R
   return z
end

modulus(R::FmpzModRelSeriesRing) = modulus(base_ring(R))

function deepcopy_internal(a::fmpz_mod_rel_series, dict::IdDict)
   z = fmpz_mod_rel_series(a)
   z.prec = a.prec
   z.val = a.val
   z.parent = parent(a)
   return z
end

function renormalize!(z::fmpz_mod_rel_series)
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
      ccall((:fmpz_mod_poly_shift_right, libflint), Nothing,
            (Ref{fmpz_mod_rel_series}, Ref{fmpz_mod_rel_series}, Int,
             Ref{fmpz_mod_ctx_struct}),
            z, z, i, z.parent.base_ring.ninv)
   end
   return nothing
end

characteristic(R::FmpzModRelSeriesRing) = modulus(R)

###############################################################################
#
#   AbstractString I/O
#
###############################################################################

function show(io::IO, a::FmpzModRelSeriesRing)
   print(io, "Univariate power series ring in ", var(a), " over ")
   show(io, base_ring(a))
end

show_minus_one(::Type{fmpz_mod_rel_series}) = show_minus_one(fmpz)

###############################################################################
#
#   Unary operators
#
###############################################################################

function -(x::fmpz_mod_rel_series)
   z = parent(x)()
   ccall((:fmpz_mod_poly_neg, libflint), Nothing,
         (Ref{fmpz_mod_rel_series}, Ref{fmpz_mod_rel_series},
          Ref{fmpz_mod_ctx_struct}),
         z, x, x.parent.base_ring.ninv)
   z.prec = x.prec
   z.val = x.val
   return z
end

###############################################################################
#
#   Binary operators
#
###############################################################################

function +(a::fmpz_mod_rel_series, b::fmpz_mod_rel_series)
   check_parent(a, b)
   lena = pol_length(a)
   lenb = pol_length(b)
   prec = min(a.prec, b.prec)
   val = min(a.val, b.val)
   lena = min(lena, prec - a.val)
   lenb = min(lenb, prec - b.val)
   z = parent(a)()
   p = a.parent.base_ring.ninv
   if a.val < b.val
      lenz = max(lena, lenb + b.val - a.val)
      ccall((:fmpz_mod_poly_set_trunc, libflint), Nothing,
            (Ref{fmpz_mod_rel_series}, Ref{fmpz_mod_rel_series}, Int,
             Ref{fmpz_mod_ctx_struct}),
            z, b, max(0, lenz - b.val + a.val), p)
      ccall((:fmpz_mod_poly_shift_left, libflint), Nothing,
            (Ref{fmpz_mod_rel_series}, Ref{fmpz_mod_rel_series}, Int,
             Ref{fmpz_mod_ctx_struct}),
            z, z, b.val - a.val, p)
      ccall((:fmpz_mod_poly_add_series, libflint), Nothing,
            (Ref{fmpz_mod_rel_series}, Ref{fmpz_mod_rel_series},
             Ref{fmpz_mod_rel_series}, Int, Ref{fmpz_mod_ctx_struct}),
            z, z, a, lenz, p)
   elseif b.val < a.val
      lenz = max(lena + a.val - b.val, lenb)
      ccall((:fmpz_mod_poly_set_trunc, libflint), Nothing,
            (Ref{fmpz_mod_rel_series}, Ref{fmpz_mod_rel_series}, Int,
             Ref{fmpz_mod_ctx_struct}),
            z, a, max(0, lenz - a.val + b.val), p)
      ccall((:fmpz_mod_poly_shift_left, libflint), Nothing,
            (Ref{fmpz_mod_rel_series}, Ref{fmpz_mod_rel_series}, Int,
             Ref{fmpz_mod_ctx_struct}),
            z, z, a.val - b.val, p)
      ccall((:fmpz_mod_poly_add_series, libflint), Nothing,
            (Ref{fmpz_mod_rel_series}, Ref{fmpz_mod_rel_series},
             Ref{fmpz_mod_rel_series}, Int, Ref{fmpz_mod_ctx_struct}),
            z, z, b, lenz, p)
   else
      lenz = max(lena, lenb)
      ccall((:fmpz_mod_poly_add_series, libflint), Nothing,
            (Ref{fmpz_mod_rel_series}, Ref{fmpz_mod_rel_series},
             Ref{fmpz_mod_rel_series}, Int, Ref{fmpz_mod_ctx_struct}),
            z, a, b, lenz, p)
   end
   z.prec = prec
   z.val = val
   renormalize!(z)
   return z
end

function -(a::fmpz_mod_rel_series, b::fmpz_mod_rel_series)
   check_parent(a, b)
   lena = pol_length(a)
   lenb = pol_length(b)
   prec = min(a.prec, b.prec)
   val = min(a.val, b.val)
   lena = min(lena, prec - a.val)
   lenb = min(lenb, prec - b.val)
   lenz = max(lena, lenb)
   z = parent(a)()
   p = a.parent.base_ring.ninv
   if a.val < b.val
      lenz = max(lena, lenb + b.val - a.val)
      ccall((:fmpz_mod_poly_set_trunc, libflint), Nothing,
            (Ref{fmpz_mod_rel_series}, Ref{fmpz_mod_rel_series}, Int,
             Ref{fmpz_mod_ctx_struct}),
            z, b, max(0, lenz - b.val + a.val), p)
      ccall((:fmpz_mod_poly_shift_left, libflint), Nothing,
            (Ref{fmpz_mod_rel_series}, Ref{fmpz_mod_rel_series}, Int,
             Ref{fmpz_mod_ctx_struct}),
            z, z, b.val - a.val, p)
      ccall((:fmpz_mod_poly_neg, libflint), Nothing,
            (Ref{fmpz_mod_rel_series}, Ref{fmpz_mod_rel_series},
             Ref{fmpz_mod_ctx_struct}),
            z, z, p)
      ccall((:fmpz_mod_poly_add_series, libflint), Nothing,
            (Ref{fmpz_mod_rel_series}, Ref{fmpz_mod_rel_series},
             Ref{fmpz_mod_rel_series}, Int, Ref{fmpz_mod_ctx_struct}),
            z, z, a, lenz, p)
   elseif b.val < a.val
      lenz = max(lena + a.val - b.val, lenb)
      ccall((:fmpz_mod_poly_set_trunc, libflint), Nothing,
            (Ref{fmpz_mod_rel_series}, Ref{fmpz_mod_rel_series}, Int,
             Ref{fmpz_mod_ctx_struct}),
            z, a, max(0, lenz - a.val + b.val), p)
      ccall((:fmpz_mod_poly_shift_left, libflint), Nothing,
            (Ref{fmpz_mod_rel_series}, Ref{fmpz_mod_rel_series}, Int,
             Ref{fmpz_mod_ctx_struct}),
            z, z, a.val - b.val, p)
      ccall((:fmpz_mod_poly_sub_series, libflint), Nothing,
            (Ref{fmpz_mod_rel_series}, Ref{fmpz_mod_rel_series},
             Ref{fmpz_mod_rel_series}, Int, Ref{fmpz_mod_ctx_struct}),
            z, z, b, lenz, p)
   else
      lenz = max(lena, lenb)
      ccall((:fmpz_mod_poly_sub_series, libflint), Nothing,
            (Ref{fmpz_mod_rel_series}, Ref{fmpz_mod_rel_series},
             Ref{fmpz_mod_rel_series}, Int, Ref{fmpz_mod_ctx_struct}),
            z, a, b, lenz, p)
   end
   z.prec = prec
   z.val = val
   renormalize!(z)
   return z
end

function *(a::fmpz_mod_rel_series, b::fmpz_mod_rel_series)
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
   ccall((:fmpz_mod_poly_mullow, libflint), Nothing,
         (Ref{fmpz_mod_rel_series}, Ref{fmpz_mod_rel_series},
          Ref{fmpz_mod_rel_series}, Int, Ref{fmpz_mod_ctx_struct}),
         z, a, b, lenz, a.parent.base_ring.ninv)
   renormalize!(z)
   return z
end

###############################################################################
#
#   Ad hoc binary operators
#
###############################################################################

function *(x::fmpz_mod, y::fmpz_mod_rel_series)
   z = parent(y)()
   z.prec = y.prec
   z.val = y.val
   ccall((:fmpz_mod_poly_scalar_mul_fmpz, libflint), Nothing,
         (Ref{fmpz_mod_rel_series}, Ref{fmpz_mod_rel_series}, Ref{fmpz},
          Ref{fmpz_mod_ctx_struct}),
         z, y, x.data, y.parent.base_ring.ninv)
   renormalize!(z)
   return z
end

*(x::fmpz_mod_rel_series, y::fmpz_mod) = y * x

function *(x::fmpz, y::fmpz_mod_rel_series)
   z = parent(y)()
   z.prec = y.prec
   z.val = y.val
   ccall((:fmpz_mod_poly_scalar_mul_fmpz, libflint), Nothing,
         (Ref{fmpz_mod_rel_series}, Ref{fmpz_mod_rel_series}, Ref{fmpz},
          Ref{fmpz_mod_ctx_struct}),
         z, y, x, y.parent.base_ring.ninv)
   renormalize!(z)
   return z
end

*(x::fmpz_mod_rel_series, y::fmpz) = y * x

*(x::Integer, y::fmpz_mod_rel_series) = fmpz(x)*y

*(x::fmpz_mod_rel_series, y::Integer) = y * x

###############################################################################
#
#   Shifting
#
###############################################################################

function shift_left(x::fmpz_mod_rel_series, len::Int)
   len < 0 && throw(DomainError(len, "Shift must be non-negative"))
   xlen = pol_length(x)
   z = fmpz_mod_rel_series(x)
   z.prec = x.prec + len
   z.val = x.val + len
   z.parent = parent(x)
   return z
end

function shift_right(x::fmpz_mod_rel_series, len::Int)
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
      ccall((:fmpz_mod_poly_shift_right, libflint), Nothing,
            (Ref{fmpz_mod_rel_series}, Ref{fmpz_mod_rel_series}, Int,
             Ref{fmpz_mod_ctx_struct}),
            z, x, xlen - zlen, x.parent.base_ring.ninv)
      renormalize!(z)
   end
   return z
end

###############################################################################
#
#   Truncation
#
###############################################################################

function truncate(x::fmpz_mod_rel_series, prec::Int)
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
      ccall((:fmpz_mod_poly_set_trunc, libflint), Nothing,
            (Ref{fmpz_mod_rel_series}, Ref{fmpz_mod_rel_series}, Int,
             Ref{fmpz_mod_ctx_struct}),
            z, x, min(prec - xval, xlen), x.parent.base_ring.ninv)
   end
   return z
end

###############################################################################
#
#   Powering
#
###############################################################################

function ^(a::fmpz_mod_rel_series, b::Int)
   b < 0 && throw(DomainError(b, "Exponent must be non-negative"))
   if isgen(a)
      z = parent(a)()
      z = setcoeff!(z, 0, fmpz(1))
      z.prec = a.prec + b - 1
      z.val = b
   elseif pol_length(a) == 0
      z = parent(a)()
      z.prec = b*valuation(a)
      z.val = b*valuation(a)
   elseif pol_length(a) == 1
      z = parent(a)([polcoeff(a, 0)^b], 1,
                           (b - 1)*valuation(a) + precision(a), b*valuation(a))
      renormalize!(z)
      return z
   elseif b == 0
      return one(parent(a))
   else
      z = parent(a)()
      z.prec = a.prec + (b - 1)*valuation(a)
      z.val = b*valuation(a)
      ccall((:fmpz_mod_poly_pow_trunc, libflint), Nothing,
            (Ref{fmpz_mod_rel_series}, Ref{fmpz_mod_rel_series}, UInt, Int,
             Ref{fmpz_mod_ctx_struct}),
            z, a, b, z.prec - z.val, a.parent.base_ring.ninv)
   end
   renormalize!(z)
   return z
end

###############################################################################
#
#   Comparison
#
###############################################################################

function ==(x::fmpz_mod_rel_series, y::fmpz_mod_rel_series)
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
   return Bool(ccall((:fmpz_mod_poly_equal_trunc, libflint), Cint,
                     (Ref{fmpz_mod_rel_series}, Ref{fmpz_mod_rel_series}, Int,
                      Ref{fmpz_mod_ctx_struct}),
                     x, y, xlen, y.parent.base_ring.ninv))
end

function isequal(x::fmpz_mod_rel_series, y::fmpz_mod_rel_series)
   if parent(x) != parent(y)
      return false
   end
   if x.prec != y.prec || x.val != y.val || pol_length(x) != pol_length(y)
      return false
   end
   return Bool(ccall((:fmpz_mod_poly_equal, libflint), Cint,
                     (Ref{fmpz_mod_rel_series}, Ref{fmpz_mod_rel_series}, Int,
                      Ref{fmpz_mod_ctx_struct}),
                     x, y, pol_length(x), x.parent.base_ring.ninv))
end

###############################################################################
#
#   Ad hoc comparisons
#
###############################################################################

function ==(x::fmpz_mod_rel_series, y::fmpz_mod)
   if precision(x) == 0
      return true
   elseif pol_length(x) > 1
      return false
   elseif pol_length(x) == 1
      if x.val == 0
         z = fmpz()
         ccall((:fmpz_mod_poly_get_coeff_fmpz, libflint), Nothing,
               (Ref{fmpz}, Ref{fmpz_mod_rel_series}, Int,
                Ref{fmpz_mod_ctx_struct}),
               z, x, 0, x.parent.base_ring.ninv)
         return Bool(ccall((:fmpz_equal, libflint), Cint,
                           (Ref{fmpz}, Ref{fmpz}),
                           z, y.data))
      else
         return false
      end
   else
      return iszero(y.data)
   end
end

==(x::fmpz_mod, y::fmpz_mod_rel_series) = y == x

function ==(x::fmpz_mod_rel_series, y::fmpz)
   if precision(x) == 0
      return true
   elseif pol_length(x) > 1
      return false
   elseif pol_length(x) == 1
      if x.val == 0
         z = fmpz()
         ccall((:fmpz_mod_poly_get_coeff_fmpz, libflint), Nothing,
               (Ref{fmpz}, Ref{fmpz_mod_rel_series}, Int,
                Ref{fmpz_mod_ctx_struct}),
               z, x, 0, x.parent.base_ring.ninv)
         r = mod(y, modulus(x))
         return Bool(ccall((:fmpz_equal, libflint), Cint,
                           (Ref{fmpz}, Ref{fmpz}),
                           z, r))
      else
         return false
      end
   else
      r = mod(y, modulus(x))
      return iszero(r)
   end
end

==(x::fmpz, y::fmpz_mod_rel_series) = y == x

==(x::fmpz_mod_rel_series, y::Integer) = x == fmpz(y)

==(x::Integer, y::fmpz_mod_rel_series) = y == x

###############################################################################
#
#   Exact division
#
###############################################################################

function divexact(x::fmpz_mod_rel_series, y::fmpz_mod_rel_series)
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
      ccall((:fmpz_mod_poly_div_series, libflint), Nothing,
            (Ref{fmpz_mod_rel_series}, Ref{fmpz_mod_rel_series},
             Ref{fmpz_mod_rel_series}, Int, Ref{fmpz_mod_ctx_struct}),
            z, x, y, prec, x.parent.base_ring.ninv)
   end
   return z
end

###############################################################################
#
#   Ad hoc exact division
#
###############################################################################

function divexact(x::fmpz_mod_rel_series, y::fmpz_mod)
   iszero(y) && throw(DivideError())
   z = parent(x)()
   z.prec = x.prec
   z.val = x.val
   ccall((:fmpz_mod_poly_scalar_div_fmpz, libflint), Nothing,
         (Ref{fmpz_mod_rel_series}, Ref{fmpz_mod_rel_series}, Ref{fmpz},
          Ref{fmpz_mod_ctx_struct}),
         z, x, y.data, x.parent.base_ring.ninv)
   return z
end

function divexact(x::fmpz_mod_rel_series, y::fmpz)
   iszero(y) && throw(DivideError())
   z = parent(x)()
   z.prec = x.prec
   z.prec = x.prec
   z.val = x.val
   r = mod(y, modulus(x))
   ccall((:fmpz_mod_poly_scalar_div_fmpz, libflint), Nothing,
         (Ref{fmpz_mod_rel_series}, Ref{fmpz_mod_rel_series}, Ref{fmpz},
          Ref{fmpz_mod_ctx_struct}),
         z, x, r, x.parent.base_ring.ninv)
   return z
end

divexact(x::fmpz_mod_rel_series, y::Integer) = divexact(x, fmpz(y))

###############################################################################
#
#   Inversion
#
###############################################################################

function inv(a::fmpz_mod_rel_series)
   iszero(a) && throw(DivideError())
   !isunit(a) && error("Unable to invert power series")
   ainv = parent(a)()
   ainv.prec = a.prec
   ainv.val = 0
   ccall((:fmpz_mod_poly_inv_series, libflint), Nothing,
         (Ref{fmpz_mod_rel_series}, Ref{fmpz_mod_rel_series}, Int,
          Ref{fmpz_mod_ctx_struct}),
         ainv, a, a.prec, a.parent.base_ring.ninv)
   return ainv
end

###############################################################################
#
#   Special functions
#
###############################################################################

function Base.exp(a::fmpz_mod_rel_series)
   if iszero(a)
      z = one(parent(a))
      z.prec = precision(a)
      z.val = valuation(a)
      return z
   end
   z = parent(a)()
   R = base_ring(a)
   vala = valuation(a)
   preca = precision(a)
   d = Vector{fmpz}(undef, preca)
   c = vala == 0 ? polcoeff(a, 0) : R()
   d[1] = exp(c).data
   len = pol_length(a) + vala
   z0 = fmpz()
   for k = 1 : preca - 1
      s = fmpz()
      for j = 1 : min(k + 1, len) - 1
         c = j >= vala ? polcoeff(a, j - vala).data : z0
         s += j * c * d[k - j + 1]
      end
      !isunit(base_ring(a)(k)) && error("Unable to divide in exp")
      d[k + 1] = divexact(base_ring(a)(s), k).data
   end
   z = parent(a)(d, preca, preca, 0)
   ccall((:_fmpz_mod_poly_set_length, libflint), Nothing,
         (Ref{fmpz_mod_rel_series}, Int, Ref{fmpz_mod_ctx_struct}),
         z, normalise(z, preca), a.parent.base_ring.ninv)
   return z
end

###############################################################################
#
#   Unsafe functions
#
###############################################################################

function zero!(x::fmpz_mod_rel_series)
  ccall((:fmpz_mod_poly_zero, libflint), Nothing,
        (Ref{fmpz_mod_rel_series}, Ref{fmpz_mod_ctx_struct}),
        x, x.parent.base_ring.ninv)
  x.prec = parent(x).prec_max
  return x
end

function fit!(x::fmpz_mod_rel_series, n::Int)
  ccall((:fmpz_mod_poly_fit_length, libflint), Nothing,
        (Ref{fmpz_mod_rel_series}, Int, Ref{fmpz_mod_ctx_struct}),
        x, n, x.parent.base_ring.ninv)
  return nothing
end

function setcoeff!(z::fmpz_mod_rel_series, n::Int, x::fmpz)
   ccall((:fmpz_mod_poly_set_coeff_fmpz, libflint), Nothing,
         (Ref{fmpz_mod_rel_series}, Int, Ref{fmpz}, Ref{fmpz_mod_ctx_struct}),
         z, n, x, z.parent.base_ring.ninv)
   return z
end

function setcoeff!(z::fmpz_mod_rel_series, n::Int, x::fmpz_mod)
   ccall((:fmpz_mod_poly_set_coeff_fmpz, libflint), Nothing,
         (Ref{fmpz_mod_rel_series}, Int, Ref{fmpz}, Ref{fmpz_mod_ctx_struct}),
         z, n, x.data, z.parent.base_ring.ninv)
   return z
end

function mul!(z::fmpz_mod_rel_series, a::fmpz_mod_rel_series, b::fmpz_mod_rel_series)
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
   ccall((:fmpz_mod_poly_mullow, libflint), Nothing,
         (Ref{fmpz_mod_rel_series}, Ref{fmpz_mod_rel_series},
          Ref{fmpz_mod_rel_series}, Int, Ref{fmpz_mod_ctx_struct}),
         z, a, b, lenz, z.parent.base_ring.ninv)
   renormalize!(z)
   return z
end

function addeq!(a::fmpz_mod_rel_series, b::fmpz_mod_rel_series)
   lena = pol_length(a)
   lenb = pol_length(b)
   prec = min(a.prec, b.prec)
   val = min(a.val, b.val)
   lena = min(lena, prec - a.val)
   lenb = min(lenb, prec - b.val)
   modulus = modulus(a)
   p = a.parent.base_ring.ninv
   if a.val < b.val
      z = fmpz_mod_rel_series(modulus)
      lenz = max(lena, lenb + b.val - a.val)
      ccall((:fmpz_mod_poly_set_trunc, libflint), Nothing,
            (Ref{fmpz_mod_rel_series}, Ref{fmpz_mod_rel_series}, Int,
             Ref{fmpz_mod_ctx_struct}),
            z, b, max(0, lenz - b.val + a.val), p)
      ccall((:fmpz_mod_poly_shift_left, libflint), Nothing,
            (Ref{fmpz_mod_rel_series}, Ref{fmpz_mod_rel_series}, Int,
             Ref{fmpz_mod_ctx_struct}),
            z, z, b.val - a.val, p)
      ccall((:fmpz_mod_poly_add_series, libflint), Nothing,
            (Ref{fmpz_mod_rel_series}, Ref{fmpz_mod_rel_series},
             Ref{fmpz_mod_rel_series}, Int, Ref{fmpz_mod_ctx_struct}),
            a, a, z, lenz, p)
   elseif b.val < a.val
      lenz = max(lena + a.val - b.val, lenb)
      ccall((:fmpz_mod_poly_truncate, libflint), Nothing,
            (Ref{fmpz_mod_rel_series}, Int, Ref{fmpz_mod_ctx_struct}),
            a, max(0, lenz - a.val + b.val), p)
      ccall((:fmpz_mod_poly_shift_left, libflint), Nothing,
            (Ref{fmpz_mod_rel_series}, Ref{fmpz_mod_rel_series}, Int,
             Ref{fmpz_mod_ctx_struct}),
            a, a, a.val - b.val, p)
      ccall((:fmpz_mod_poly_add_series, libflint), Nothing,
            (Ref{fmpz_mod_rel_series}, Ref{fmpz_mod_rel_series},
             Ref{fmpz_mod_rel_series}, Int, Ref{fmpz_mod_ctx_struct}),
            a, a, b, lenz, p)
   else
      lenz = max(lena, lenb)
      ccall((:fmpz_mod_poly_add_series, libflint), Nothing,
            (Ref{fmpz_mod_rel_series}, Ref{fmpz_mod_rel_series},
             Ref{fmpz_mod_rel_series}, Int, Ref{fmpz_mod_ctx_struct}),
            a, a, b, lenz, p)
   end
   a.prec = prec
   a.val = val
   renormalize!(a)
   return a
end

function add!(c::fmpz_mod_rel_series, a::fmpz_mod_rel_series, b::fmpz_mod_rel_series)
   lena = pol_length(a)
   lenb = pol_length(b)

   prec = min(a.prec, b.prec)

   lena = min(lena, prec)
   lenb = min(lenb, prec)

   lenc = max(lena, lenb)
   c.prec = prec
   ccall((:fmpz_mod_poly_add_series, libflint), Nothing,
         (Ref{fmpz_mod_rel_series}, Ref{fmpz_mod_rel_series},
          Ref{fmpz_mod_rel_series}, Int, Ref{fmpz_mod_ctx_struct}),
         c, a, b, lenc, a.parent.base_ring.ninv)
   return c
end

###############################################################################
#
#   Promotion rules
#
###############################################################################

promote_rule(::Type{fmpz_mod_rel_series}, ::Type{T}) where {T <: Integer} = fmpz_mod_rel_series

promote_rule(::Type{fmpz_mod_rel_series}, ::Type{fmpz}) = fmpz_mod_rel_series

promote_rule(::Type{fmpz_mod_rel_series}, ::Type{fmpz_mod}) = fmpz_mod_rel_series

###############################################################################
#
#   Parent object call overload
#
###############################################################################

function (a::FmpzModRelSeriesRing)()
   z = fmpz_mod_rel_series(modulus(a))
   z.prec = a.prec_max
   z.val = a.prec_max
   z.parent = a
   return z
end

function (a::FmpzModRelSeriesRing)(b::Integer)
   if b == 0
      z = fmpz_mod_rel_series(modulus(a))
      z.prec = a.prec_max
      z.val = a.prec_max
   else
      z = fmpz_mod_rel_series(modulus(a), [fmpz(b)], 1, a.prec_max, 0)
   end
   z.parent = a
   return z
end

function (a::FmpzModRelSeriesRing)(b::fmpz)
   if iszero(b)
      z = fmpz_mod_rel_series(modulus(a))
      z.prec = a.prec_max
      z.val = a.prec_max
   else
      z = fmpz_mod_rel_series(modulus(a), [b], 1, a.prec_max, 0)
   end
   z.parent = a
   return z
end

function (a::FmpzModRelSeriesRing)(b::fmpz_mod)
   if iszero(b)
      z = fmpz_mod_rel_series(modulus(a))
      z.prec = a.prec_max
      z.val = a.prec_max
   else
      z = fmpz_mod_rel_series(modulus(a), [b], 1, a.prec_max, 0)
   end
   z.parent = a
   return z
end

function (a::FmpzModRelSeriesRing)(b::fmpz_mod_rel_series)
   parent(b) != a && error("Unable to coerce power series")
   return b
end

function (a::FmpzModRelSeriesRing)(b::Array{fmpz, 1}, len::Int, prec::Int, val::Int)
   z = fmpz_mod_rel_series(modulus(a), b, len, prec, val)
   z.parent = a
   return z
end

function (a::FmpzModRelSeriesRing)(b::Array{fmpz_mod, 1}, len::Int, prec::Int, val::Int)
   z = fmpz_mod_rel_series(modulus(a), b, len, prec, val)
   z.parent = a
   return z
end
