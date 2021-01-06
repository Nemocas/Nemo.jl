###############################################################################
#
#   nmod_rel_series.jl : Power series over flint nmod integers mod n
#
###############################################################################

export nmod_rel_series, NmodRelSeriesRing

###############################################################################
#
#   Data type and parent object methods
#
###############################################################################

function O(a::nmod_rel_series)
   val = pol_length(a) + valuation(a) - 1
   val < 0 && throw(DomainError(val, "Valuation must be non-negative"))
   z = nmod_rel_series(modulus(a), Vector{UInt}(undef, 0), 0, val, val)
   z.parent = parent(a)
   return z
end

elem_type(::Type{NmodRelSeriesRing}) = nmod_rel_series

parent_type(::Type{nmod_rel_series}) = NmodRelSeriesRing

base_ring(R::NmodRelSeriesRing) = R.base_ring

var(a::NmodRelSeriesRing) = a.S

###############################################################################
#
#   Basic manipulation
#
###############################################################################

max_precision(R::NmodRelSeriesRing) = R.prec_max

function normalise(a::nmod_rel_series, len::Int)
   if len > 0
      c = ccall((:nmod_poly_get_coeff_ui, libflint), UInt,
         (Ref{nmod_rel_series}, Int), a, len - 1)
   end
   while len > 0 && iszero(c)
      len -= 1
      if len > 0
         c = ccall((:nmod_poly_get_coeff_ui, libflint), UInt,
            (Ref{nmod_rel_series}, Int), a, len - 1)
      end
   end
   return len
end

function pol_length(x::nmod_rel_series)
   return ccall((:nmod_poly_length, libflint), Int, (Ref{nmod_rel_series},), x)
end

precision(x::nmod_rel_series) = x.prec

function polcoeff(x::nmod_rel_series, n::Int)
   R = base_ring(x)
   if n < 0
      return R(0)
   end
   z = ccall((:nmod_poly_get_coeff_ui, libflint), UInt,
         (Ref{nmod_rel_series}, Int), x, n)
   return R(z)
end

zero(R::NmodRelSeriesRing) = R(0)

one(R::NmodRelSeriesRing) = R(1)

function gen(R::NmodRelSeriesRing)
   z = nmod_rel_series(modulus(R), [UInt(1)], 1, max_precision(R) + 1, 1)
   z.parent = R
   return z
end

modulus(R::NmodRelSeriesRing) = modulus(base_ring(R))

function deepcopy_internal(a::nmod_rel_series, dict::IdDict)
   z = nmod_rel_series(a)
   z.prec = a.prec
   z.val = a.val
   z.parent = parent(a)
   return z
end

function renormalize!(z::nmod_rel_series)
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
      ccall((:nmod_poly_shift_right, libflint), Nothing,
            (Ref{nmod_rel_series}, Ref{nmod_rel_series}, Int), z, z, i)
   end
   return nothing
end

characteristic(R::NmodRelSeriesRing) = modulus(R)

###############################################################################
#
#   AbstractString I/O
#
###############################################################################

function show(io::IO, a::NmodRelSeriesRing)
   print(io, "Univariate power series ring in ", var(a), " over ")
   show(io, base_ring(a))
end

###############################################################################
#
#   Unary operators
#
###############################################################################

function -(x::nmod_rel_series)
   z = parent(x)()
   ccall((:nmod_poly_neg, libflint), Nothing,
                (Ref{nmod_rel_series}, Ref{nmod_rel_series}),
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

function +(a::nmod_rel_series, b::nmod_rel_series)
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
      ccall((:nmod_poly_set_trunc, libflint), Nothing,
            (Ref{nmod_rel_series}, Ref{nmod_rel_series}, Int),
            z, b, max(0, lenz - b.val + a.val))
      ccall((:nmod_poly_shift_left, libflint), Nothing,
            (Ref{nmod_rel_series}, Ref{nmod_rel_series}, Int),
            z, z, b.val - a.val)
      ccall((:nmod_poly_add_series, libflint), Nothing,
                (Ref{nmod_rel_series}, Ref{nmod_rel_series}, Ref{nmod_rel_series}, Int),
               z, z, a, lenz)
   elseif b.val < a.val
      lenz = max(lena + a.val - b.val, lenb)
      ccall((:nmod_poly_set_trunc, libflint), Nothing,
            (Ref{nmod_rel_series}, Ref{nmod_rel_series}, Int),
            z, a, max(0, lenz - a.val + b.val))
      ccall((:nmod_poly_shift_left, libflint), Nothing,
            (Ref{nmod_rel_series}, Ref{nmod_rel_series}, Int),
            z, z, a.val - b.val)
      ccall((:nmod_poly_add_series, libflint), Nothing,
                (Ref{nmod_rel_series}, Ref{nmod_rel_series}, Ref{nmod_rel_series}, Int),
               z, z, b, lenz)
   else
      lenz = max(lena, lenb)
      ccall((:nmod_poly_add_series, libflint), Nothing,
                (Ref{nmod_rel_series}, Ref{nmod_rel_series}, Ref{nmod_rel_series}, Int),
               z, a, b, lenz)
   end
   z.prec = prec
   z.val = val
   renormalize!(z)
   return z
end

function -(a::nmod_rel_series, b::nmod_rel_series)
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
      ccall((:nmod_poly_set_trunc, libflint), Nothing,
            (Ref{nmod_rel_series}, Ref{nmod_rel_series}, Int),
            z, b, max(0, lenz - b.val + a.val))
      ccall((:nmod_poly_shift_left, libflint), Nothing,
            (Ref{nmod_rel_series}, Ref{nmod_rel_series}, Int),
            z, z, b.val - a.val)
      ccall((:nmod_poly_neg, libflint), Nothing,
            (Ref{nmod_rel_series}, Ref{nmod_rel_series}), z, z)
      ccall((:nmod_poly_add_series, libflint), Nothing,
                (Ref{nmod_rel_series}, Ref{nmod_rel_series}, Ref{nmod_rel_series}, Int),
               z, z, a, lenz)
   elseif b.val < a.val
      lenz = max(lena + a.val - b.val, lenb)
      ccall((:nmod_poly_set_trunc, libflint), Nothing,
            (Ref{nmod_rel_series}, Ref{nmod_rel_series}, Int),
            z, a, max(0, lenz - a.val + b.val))
      ccall((:nmod_poly_shift_left, libflint), Nothing,
            (Ref{nmod_rel_series}, Ref{nmod_rel_series}, Int),
            z, z, a.val - b.val)
      ccall((:nmod_poly_sub_series, libflint), Nothing,
                (Ref{nmod_rel_series}, Ref{nmod_rel_series}, Ref{nmod_rel_series}, Int),
               z, z, b, lenz)
   else
      lenz = max(lena, lenb)
      ccall((:nmod_poly_sub_series, libflint), Nothing,
                (Ref{nmod_rel_series}, Ref{nmod_rel_series}, Ref{nmod_rel_series}, Int),
               z, a, b, lenz)
   end
   z.prec = prec
   z.val = val
   renormalize!(z)
   return z
end

function *(a::nmod_rel_series, b::nmod_rel_series)
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
   ccall((:nmod_poly_mullow, libflint), Nothing,
                (Ref{nmod_rel_series}, Ref{nmod_rel_series}, Ref{nmod_rel_series}, Int),
               z, a, b, lenz)
   renormalize!(z)
   return z
end

###############################################################################
#
#   Ad hoc binary operators
#
###############################################################################

function *(x::nmod, y::nmod_rel_series)
   z = parent(y)()
   z.prec = y.prec
   z.val = y.val
   ccall((:nmod_poly_scalar_mul_nmod, libflint), Nothing,
                (Ref{nmod_rel_series}, Ref{nmod_rel_series}, UInt),
               z, y, data(x))
   renormalize!(z)
   return z
end

*(x::nmod_rel_series, y::nmod) = y * x

function *(x::fmpz, y::nmod_rel_series)
   z = parent(y)()
   z.prec = y.prec
   z.val = y.val
   r = ccall((:fmpz_fdiv_ui, libflint), UInt, (Ref{fmpz}, UInt), x, modulus(y))
   ccall((:nmod_poly_scalar_mul_nmod, libflint), Nothing,
                (Ref{nmod_rel_series}, Ref{nmod_rel_series}, UInt),
               z, y, r)
   renormalize!(z)
   return z
end

function *(x::UInt, y::nmod_rel_series)
   z = parent(y)()
   z.prec = y.prec
   z.val = y.val
   ccall((:nmod_poly_scalar_mul_nmod, libflint), Nothing,
                (Ref{nmod_rel_series}, Ref{nmod_rel_series}, UInt),
               z, y, mod(x, modulus(y)))
   renormalize!(z)
   return z
end

*(x::nmod_rel_series, y::fmpz) = y * x

*(x::nmod_rel_series, y::UInt) = y * x

*(x::Integer, y::nmod_rel_series) = fmpz(x)*y

*(x::nmod_rel_series, y::Integer) = y * x

###############################################################################
#
#   Shifting
#
###############################################################################

function shift_left(x::nmod_rel_series, len::Int)
   len < 0 && throw(DomainError(len, "Shift must be non-negative"))
   xlen = pol_length(x)
   z = nmod_rel_series(x)
   z.prec = x.prec + len
   z.val = x.val + len
   z.parent = parent(x)
   return z
end

function shift_right(x::nmod_rel_series, len::Int)
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
      ccall((:nmod_poly_shift_right, libflint), Nothing,
                (Ref{nmod_rel_series}, Ref{nmod_rel_series}, Int),
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

function truncate(x::nmod_rel_series, prec::Int)
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
      ccall((:nmod_poly_set_trunc, libflint), Nothing,
                (Ref{nmod_rel_series}, Ref{nmod_rel_series}, Int),
               z, x, min(prec - xval, xlen))
   end
   return z
end

###############################################################################
#
#   Powering
#
###############################################################################

function ^(a::nmod_rel_series, b::Int)
   b < 0 && throw(DomainError(b, "Exponent must be non-negative"))
   if isgen(a)
      z = parent(a)()
      z = setcoeff!(z, 0, UInt(1))
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
      ccall((:nmod_poly_pow_trunc, libflint), Nothing,
                (Ref{nmod_rel_series}, Ref{nmod_rel_series}, Int, Int),
               z, a, b, z.prec - z.val)
   end
   renormalize!(z)
   return z
end

###############################################################################
#
#   Comparison
#
###############################################################################

function ==(x::nmod_rel_series, y::nmod_rel_series)
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
   return Bool(ccall((:nmod_poly_equal_trunc, libflint), Cint,
                (Ref{nmod_rel_series}, Ref{nmod_rel_series}, Int),
               x, y, xlen))
end

function isequal(x::nmod_rel_series, y::nmod_rel_series)
   if parent(x) != parent(y)
      return false
   end
   if x.prec != y.prec || x.val != y.val || pol_length(x) != pol_length(y)
      return false
   end
   return Bool(ccall((:nmod_poly_equal, libflint), Cint,
                (Ref{nmod_rel_series}, Ref{nmod_rel_series}, Int),
               x, y, pol_length(x)))
end

###############################################################################
#
#   Ad hoc comparisons
#
###############################################################################

function ==(x::nmod_rel_series, y::nmod)
   if precision(x) == 0
      return true
   elseif pol_length(x) > 1
      return false
   elseif pol_length(x) == 1
      if x.val == 0
         z = ccall((:nmod_poly_get_coeff_ui, libflint), UInt,
                       (Ref{nmod_rel_series}, Int), x, 0)
         return data(y) == z
      else
         return false
      end
   else
      return iszero(data(y))
   end
end

==(x::nmod, y::nmod_rel_series) = y == x

function ==(x::nmod_rel_series, y::fmpz)
   if precision(x) == 0
      return true
   elseif pol_length(x) > 1
      return false
   elseif pol_length(x) == 1
      if x.val == 0
         r = ccall((:fmpz_fdiv_ui, libflint), UInt, (Ref{fmpz}, UInt), y, modulus(x))
         z = ccall((:nmod_poly_get_coeff_ui, libflint), UInt,
                       (Ref{nmod_rel_series}, Int), x, 0)
         return r == z
      else
         return false
      end
   else
      r = ccall((:fmpz_fdiv_ui, libflint), UInt, (Ref{fmpz}, UInt), y, modulus(x))
      return r == UInt(0)
   end
end

==(x::fmpz, y::nmod_rel_series) = y == x

function ==(x::nmod_rel_series, y::UInt)
   if precision(x) == 0
      return true
   elseif pol_length(x) > 1
      return false
   elseif pol_length(x) == 1
      if x.val == 0
         r = mod(y, modulus(x))
         z = ccall((:nmod_poly_get_coeff_ui, libflint), UInt,
                       (Ref{nmod_rel_series}, Int), x, 0)
         return r == z
      else
         return false
      end
   else
      r = mod(y, modulus(x))
      return r == UInt(0)
   end
end

==(x::UInt, y::nmod_rel_series) = y == x

==(x::nmod_rel_series, y::Integer) = x == fmpz(y)

==(x::Integer, y::nmod_rel_series) = y == x

###############################################################################
#
#   Exact division
#
###############################################################################

function divexact(x::nmod_rel_series, y::nmod_rel_series)
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
      ccall((:nmod_poly_div_series, libflint), Nothing,
                (Ref{nmod_rel_series}, Ref{nmod_rel_series}, Ref{nmod_rel_series}, Int),
               z, x, y, prec)
   end
   return z
end

###############################################################################
#
#   Ad hoc exact division
#
###############################################################################

function divexact(x::nmod_rel_series, y::nmod)
   iszero(y) && throw(DivideError())
   z = parent(x)()
   z.prec = x.prec
   z.val = x.val
   r = inv(y)
   ccall((:nmod_poly_scalar_mul_nmod, libflint), Nothing,
                (Ref{nmod_rel_series}, Ref{nmod_rel_series}, UInt),
               z, x, data(r))
   return z
end

function divexact(x::nmod_rel_series, y::fmpz)
   iszero(y) && throw(DivideError())
   z = parent(x)()
   z.prec = x.prec
   z.prec = x.prec
   z.val = x.val
   r = ccall((:fmpz_fdiv_ui, libflint), UInt, (Ref{fmpz}, UInt), y, modulus(x))
   rinv = inv(base_ring(x)(r))
   ccall((:nmod_poly_scalar_mul_nmod, libflint), Nothing,
                (Ref{nmod_rel_series}, Ref{nmod_rel_series}, UInt),
               z, x, data(rinv))
   return z
end

function divexact(x::nmod_rel_series, y::UInt)
   iszero(y) && throw(DivideError())
   z = parent(x)()
   z.prec = x.prec
   z.prec = x.prec
   z.val = x.val
   r = mod(y, modulus(x))
   rinv = inv(base_ring(x)(r))
   ccall((:nmod_poly_scalar_mul_nmod, libflint), Nothing,
                (Ref{nmod_rel_series}, Ref{nmod_rel_series}, UInt),
               z, x, data(rinv))
   return z
end

divexact(x::nmod_rel_series, y::Integer) = divexact(x, fmpz(y))

###############################################################################
#
#   Inversion
#
###############################################################################

function inv(a::nmod_rel_series)
   iszero(a) && throw(DivideError())
   !isunit(a) && error("Unable to invert power series")
   ainv = parent(a)()
   ainv.prec = a.prec
   ainv.val = 0
   ccall((:nmod_poly_inv_series, libflint), Nothing,
                (Ref{nmod_rel_series}, Ref{nmod_rel_series}, Int),
               ainv, a, a.prec)
   return ainv
end

###############################################################################
#
#   Special functions
#
###############################################################################

function Base.exp(a::nmod_rel_series)
   if iszero(a)
      precision(a) == 0 && return deepcopy(a)
      z = one(parent(a))
      z.prec = precision(a)
      return z
   end
   z = parent(a)()
   R = base_ring(a)
   vala = valuation(a)
   preca = precision(a)
   d = Vector{nmod}(undef, preca)
   c = vala == 0 ? polcoeff(a, 0) : R()
   d[1] = exp(c)
   len = pol_length(a) + vala
   z0 = R()
   for k = 1 : preca - 1
      s = R()
      for j = 1 : min(k + 1, len) - 1
         c = j >= vala ? polcoeff(a, j - vala) : z0
         s += j * c * d[k - j + 1]
      end
      !isunit(base_ring(a)(k)) && error("Unable to divide in exp")
      d[k + 1] = divexact(base_ring(a)(s), k)
   end
   z = parent(a)(d, preca, preca, 0)
   ccall((:_nmod_poly_set_length, libflint), Nothing,
         (Ref{nmod_rel_series}, Int), z, normalise(z, preca))
   return z
end

###############################################################################
#
#   Unsafe functions
#
###############################################################################

function zero!(x::nmod_rel_series)
  ccall((:nmod_poly_zero, libflint), Nothing,
                   (Ref{nmod_rel_series},), x)
  x.prec = parent(x).prec_max
  return x
end

function fit!(x::nmod_rel_series, n::Int)
  ccall((:nmod_poly_fit_length, libflint), Nothing,
                   (Ref{nmod_rel_series}, Int), x, n)
  return nothing
end

function setcoeff!(z::nmod_rel_series, n::Int, x::fmpz)
   r = ccall((:fmpz_fdiv_ui, libflint), UInt, (Ref{fmpz}, UInt), x, modulus(z))
   ccall((:nmod_poly_set_coeff_ui, libflint), Nothing,
                (Ref{nmod_rel_series}, Int, UInt),
               z, n, r)
   return z
end

function setcoeff!(z::nmod_rel_series, n::Int, x::UInt)
   r = mod(x, modulus(z))
   ccall((:nmod_poly_set_coeff_ui, libflint), Nothing,
                (Ref{nmod_rel_series}, Int, UInt),
               z, n, r)
   return z
end

function setcoeff!(z::nmod_rel_series, n::Int, x::nmod)
   ccall((:nmod_poly_set_coeff_ui, libflint), Nothing,
                (Ref{nmod_rel_series}, Int, UInt),
               z, n, data(x))
   return z
end

function mul!(z::nmod_rel_series, a::nmod_rel_series, b::nmod_rel_series)
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
   ccall((:nmod_poly_mullow, libflint), Nothing,
                (Ref{nmod_rel_series}, Ref{nmod_rel_series}, Ref{nmod_rel_series}, Int),
               z, a, b, lenz)
   renormalize!(z)
   return z
end

function addeq!(a::nmod_rel_series, b::nmod_rel_series)
   lena = pol_length(a)
   lenb = pol_length(b)
   prec = min(a.prec, b.prec)
   val = min(a.val, b.val)
   lena = min(lena, prec - a.val)
   lenb = min(lenb, prec - b.val)
   modulus = modulus(a)
   if a.val < b.val
      z = nmod_rel_series(modulus)
      lenz = max(lena, lenb + b.val - a.val)
      ccall((:nmod_poly_set_trunc, libflint), Nothing,
            (Ref{nmod_rel_series}, Ref{nmod_rel_series}, Int),
            z, b, max(0, lenz - b.val + a.val))
      ccall((:nmod_poly_shift_left, libflint), Nothing,
            (Ref{nmod_rel_series}, Ref{nmod_rel_series}, Int),
            z, z, b.val - a.val)
      ccall((:nmod_poly_add_series, libflint), Nothing,
                (Ref{nmod_rel_series}, Ref{nmod_rel_series}, Ref{nmod_rel_series}, Int),
               a, a, z, lenz)
   elseif b.val < a.val
      lenz = max(lena + a.val - b.val, lenb)
      ccall((:nmod_poly_truncate, libflint), Nothing,
            (Ref{nmod_rel_series}, Int),
            a, max(0, lenz - a.val + b.val))
      ccall((:nmod_poly_shift_left, libflint), Nothing,
            (Ref{nmod_rel_series}, Ref{nmod_rel_series}, Int),
            a, a, a.val - b.val)
      ccall((:nmod_poly_add_series, libflint), Nothing,
                (Ref{nmod_rel_series}, Ref{nmod_rel_series}, Ref{nmod_rel_series}, Int),
               a, a, b, lenz)
   else
      lenz = max(lena, lenb)
      ccall((:nmod_poly_add_series, libflint), Nothing,
                (Ref{nmod_rel_series}, Ref{nmod_rel_series}, Ref{nmod_rel_series}, Int),
               a, a, b, lenz)
   end
   a.prec = prec
   a.val = val
   renormalize!(a)
   return a
end

function add!(c::nmod_rel_series, a::nmod_rel_series, b::nmod_rel_series)
   lena = pol_length(a)
   lenb = pol_length(b)

   prec = min(a.prec, b.prec)

   lena = min(lena, prec)
   lenb = min(lenb, prec)

   lenc = max(lena, lenb)
   c.prec = prec
   ccall((:nmod_poly_add_series, libflint), Nothing,
                (Ref{nmod_rel_series}, Ref{nmod_rel_series}, Ref{nmod_rel_series}, Int),
               c, a, b, lenc)
   return c
end

###############################################################################
#
#   Promotion rules
#
###############################################################################

promote_rule(::Type{nmod_rel_series}, ::Type{T}) where {T <: Integer} = nmod_rel_series

promote_rule(::Type{nmod_rel_series}, ::Type{fmpz}) = nmod_rel_series

promote_rule(::Type{nmod_rel_series}, ::Type{nmod}) = nmod_rel_series

###############################################################################
#
#   Parent object call overload
#
###############################################################################

function (a::NmodRelSeriesRing)()
   z = nmod_rel_series(modulus(a))
   z.prec = a.prec_max
   z.val = a.prec_max
   z.parent = a
   return z
end

function (a::NmodRelSeriesRing)(b::Integer)
   if b == 0
      z = nmod_rel_series(modulus(a))
      z.prec = a.prec_max
      z.val = a.prec_max
   else
      z = nmod_rel_series(modulus(a), [fmpz(b)], 1, a.prec_max, 0)
   end
   z.parent = a
   return z
end

function (a::NmodRelSeriesRing)(b::fmpz)
   if iszero(b)
      z = nmod_rel_series(modulus(a))
      z.prec = a.prec_max
      z.val = a.prec_max
   else
      z = nmod_rel_series(modulus(a), [b], 1, a.prec_max, 0)
   end
   z.parent = a
   return z
end

function (a::NmodRelSeriesRing)(b::nmod)
   if iszero(b)
      z = nmod_rel_series(modulus(a))
      z.prec = a.prec_max
      z.val = a.prec_max
   else
      z = nmod_rel_series(modulus(a), [b], 1, a.prec_max, 0)
   end
   z.parent = a
   return z
end

function (a::NmodRelSeriesRing)(b::nmod_rel_series)
   parent(b) != a && error("Unable to coerce power series")
   return b
end

function (a::NmodRelSeriesRing)(b::Array{fmpz, 1}, len::Int, prec::Int, val::Int)
   z = nmod_rel_series(modulus(a), b, len, prec, val)
   z.parent = a
   return z
end

function (a::NmodRelSeriesRing)(b::Array{UInt, 1}, len::Int, prec::Int, val::Int)
   z = nmod_rel_series(modulus(a), b, len, prec, val)
   z.parent = a
   return z
end

function (a::NmodRelSeriesRing)(b::Array{nmod, 1}, len::Int, prec::Int, val::Int)
   z = nmod_rel_series(modulus(a), b, len, prec, val)
   z.parent = a
   return z
end

###############################################################################
#
#   PowerSeriesRing constructor
#
###############################################################################

function PowerSeriesRing(R::NmodRing, prec::Int, s::AbstractString; model=:capped_relative, cached = true)
   S = Symbol(s)

   if model == :capped_relative
      parent_obj = NmodRelSeriesRing(R, prec, S, cached)
   elseif model == :capped_absolute
      error("Not implemented yet")
      # parent_obj = NmodAbsSeriesRing(R, prec, S, cached)
   end
   return parent_obj, gen(parent_obj)
end
