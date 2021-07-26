###############################################################################
#
#   nmod_rel_series.jl: Relative series using nmod_poly
#
#   nmod_rel_series, gfp_rel_series
#
###############################################################################

export nmod_rel_series, NmodRelSeriesRing,
       gfp_rel_series, GFPRelSeriesRing

for (etype, rtype, mtype, brtype, flint_fn) in (
   (nmod_rel_series, NmodRelSeriesRing, nmod, NmodRing, "nmod_poly"),
   (gfp_rel_series, GFPRelSeriesRing, gfp_elem, GaloisField, "nmod_poly"))
@eval begin

###############################################################################
#
#   Data type and parent object methods
#
###############################################################################

function O(a::($etype))
   val = pol_length(a) + valuation(a) - 1
   val < 0 && throw(DomainError(val, "Valuation must be non-negative"))
   z = ($etype)(modulus(a), Vector{UInt}(undef, 0), 0, val, val)
   z.parent = parent(a)
   return z
end

elem_type(::Type{($rtype)}) = ($etype)

parent_type(::Type{($etype)}) = ($rtype)

base_ring(R::($rtype)) = R.base_ring

rel_series_type(::Type{($mtype)}) = ($etype)

var(a::($rtype)) = a.S

###############################################################################
#
#   Basic manipulation
#
###############################################################################

max_precision(R::($rtype)) = R.prec_max

function normalise(a::($etype), len::Int)
   if len > 0
      c = ccall(($(flint_fn*"_get_coeff_ui"), libflint), UInt,
         (Ref{($etype)}, Int), a, len - 1)
   end
   while len > 0 && iszero(c)
      len -= 1
      if len > 0
         c = ccall(($(flint_fn*"_get_coeff_ui"), libflint), UInt,
            (Ref{($etype)}, Int), a, len - 1)
      end
   end
   return len
end

function pol_length(x::($etype))
   return ccall(($(flint_fn*"_length"), libflint), Int, (Ref{($etype)},), x)
end

precision(x::($etype)) = x.prec

function polcoeff(x::($etype), n::Int)
   R = base_ring(x)
   if n < 0
      return R(0)
   end
   z = ccall(($(flint_fn*"_get_coeff_ui"), libflint), UInt,
         (Ref{($etype)}, Int), x, n)
   return R(z)
end

zero(R::($rtype)) = R(0)

one(R::($rtype)) = R(1)

function gen(R::($rtype))
   z = ($etype)(modulus(R), [UInt(1)], 1, max_precision(R) + 1, 1)
   z.parent = R
   return z
end

modulus(R::($rtype)) = modulus(base_ring(R))

function deepcopy_internal(a::($etype), dict::IdDict)
   z = ($etype)(a)
   z.prec = a.prec
   z.val = a.val
   z.parent = parent(a)
   return z
end

function renormalize!(z::($etype))
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
      ccall(($(flint_fn*"_shift_right"), libflint), Nothing,
            (Ref{($etype)}, Ref{($etype)}, Int), z, z, i)
   end
   return nothing
end

characteristic(R::($rtype)) = modulus(R)

###############################################################################
#
#   Similar
#
###############################################################################

function similar(f::RelSeriesElem, R::($brtype), max_prec::Int,
                                   s::Symbol=var(parent(f)); cached::Bool=true)
   par = ($rtype)(R, max_prec, s, cached)
   z = ($etype)(modulus(R))
   if base_ring(f) === R && s == var(parent(f)) &&
      typeof(f) == ($etype) && max_precision(parent(f)) == max_prec
      # steal parent in case it is not cached
      z.parent = parent(f)
   else
      z.parent = par
   end
   z.prec = max_prec
   z.val = max_prec
   return z
end

###############################################################################
#
#   rel_series constructor
#
###############################################################################

function rel_series(R::($brtype), arr::Vector{T},
                   len::Int, prec::Int, val::Int, var::String="x";
                            max_precision::Int=prec, cached::Bool=true) where T
   prec < len + val && error("Precision too small for given data")
   coeffs = T == ($mtype) ? arr : map(R, arr)
   coeffs = length(coeffs) == 0 ? ($mtype)[] : coeffs
   par = ($rtype)(R, max_precision, Symbol(var), cached)
   z = ($etype)(modulus(par), coeffs, len, prec, val)
   z.parent = par
   return z
end

###############################################################################
#
#   AbstractString I/O
#
###############################################################################

function show(io::IO, a::($rtype))
   print(io, "Univariate power series ring in ", var(a), " over ")
   show(io, base_ring(a))
end

###############################################################################
#
#   Unary operators
#
###############################################################################

function -(x::($etype))
   z = parent(x)()
   ccall(($(flint_fn*"_neg"), libflint), Nothing,
                (Ref{($etype)}, Ref{($etype)}),
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

function +(a::($etype), b::($etype))
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
      ccall(($(flint_fn*"_set_trunc"), libflint), Nothing,
            (Ref{($etype)}, Ref{($etype)}, Int),
            z, b, max(0, lenz - b.val + a.val))
      ccall(($(flint_fn*"_shift_left"), libflint), Nothing,
            (Ref{($etype)}, Ref{($etype)}, Int),
            z, z, b.val - a.val)
      ccall(($(flint_fn*"_add_series"), libflint), Nothing,
                (Ref{($etype)}, Ref{($etype)}, Ref{($etype)}, Int),
               z, z, a, lenz)
   elseif b.val < a.val
      lenz = max(lena + a.val - b.val, lenb)
      ccall(($(flint_fn*"_set_trunc"), libflint), Nothing,
            (Ref{($etype)}, Ref{($etype)}, Int),
            z, a, max(0, lenz - a.val + b.val))
      ccall(($(flint_fn*"_shift_left"), libflint), Nothing,
            (Ref{($etype)}, Ref{($etype)}, Int),
            z, z, a.val - b.val)
      ccall(($(flint_fn*"_add_series"), libflint), Nothing,
                (Ref{($etype)}, Ref{($etype)}, Ref{($etype)}, Int),
               z, z, b, lenz)
   else
      lenz = max(lena, lenb)
      ccall(($(flint_fn*"_add_series"), libflint), Nothing,
                (Ref{($etype)}, Ref{($etype)}, Ref{($etype)}, Int),
               z, a, b, lenz)
   end
   z.prec = prec
   z.val = val
   renormalize!(z)
   return z
end

function -(a::($etype), b::($etype))
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
      ccall(($(flint_fn*"_set_trunc"), libflint), Nothing,
            (Ref{($etype)}, Ref{($etype)}, Int),
            z, b, max(0, lenz - b.val + a.val))
      ccall(($(flint_fn*"_shift_left"), libflint), Nothing,
            (Ref{($etype)}, Ref{($etype)}, Int),
            z, z, b.val - a.val)
      ccall(($(flint_fn*"_neg"), libflint), Nothing,
            (Ref{($etype)}, Ref{($etype)}), z, z)
      ccall(($(flint_fn*"_add_series"), libflint), Nothing,
                (Ref{($etype)}, Ref{($etype)}, Ref{($etype)}, Int),
               z, z, a, lenz)
   elseif b.val < a.val
      lenz = max(lena + a.val - b.val, lenb)
      ccall(($(flint_fn*"_set_trunc"), libflint), Nothing,
            (Ref{($etype)}, Ref{($etype)}, Int),
            z, a, max(0, lenz - a.val + b.val))
      ccall(($(flint_fn*"_shift_left"), libflint), Nothing,
            (Ref{($etype)}, Ref{($etype)}, Int),
            z, z, a.val - b.val)
      ccall(($(flint_fn*"_sub_series"), libflint), Nothing,
                (Ref{($etype)}, Ref{($etype)}, Ref{($etype)}, Int),
               z, z, b, lenz)
   else
      lenz = max(lena, lenb)
      ccall(($(flint_fn*"_sub_series"), libflint), Nothing,
                (Ref{($etype)}, Ref{($etype)}, Ref{($etype)}, Int),
               z, a, b, lenz)
   end
   z.prec = prec
   z.val = val
   renormalize!(z)
   return z
end

function *(a::($etype), b::($etype))
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
   ccall(($(flint_fn*"_mullow"), libflint), Nothing,
                (Ref{($etype)}, Ref{($etype)}, Ref{($etype)}, Int),
               z, a, b, lenz)
   renormalize!(z)
   return z
end

###############################################################################
#
#   Ad hoc binary operators
#
###############################################################################

function *(x::($mtype), y::($etype))
   z = parent(y)()
   z.prec = y.prec
   z.val = y.val
   ccall(($(flint_fn*"_scalar_mul_nmod"), libflint), Nothing,
                (Ref{($etype)}, Ref{($etype)}, UInt),
               z, y, data(x))
   renormalize!(z)
   return z
end

*(x::($etype), y::($mtype)) = y * x

function *(x::fmpz, y::($etype))
   z = parent(y)()
   z.prec = y.prec
   z.val = y.val
   r = ccall((:fmpz_fdiv_ui, libflint), UInt, (Ref{fmpz}, UInt), x, modulus(y))
   ccall(($(flint_fn*"_scalar_mul_nmod"), libflint), Nothing,
                (Ref{($etype)}, Ref{($etype)}, UInt),
               z, y, r)
   renormalize!(z)
   return z
end

function *(x::UInt, y::($etype))
   z = parent(y)()
   z.prec = y.prec
   z.val = y.val
   ccall(($(flint_fn*"_scalar_mul_nmod"), libflint), Nothing,
                (Ref{($etype)}, Ref{($etype)}, UInt),
               z, y, mod(x, modulus(y)))
   renormalize!(z)
   return z
end

*(x::($etype), y::fmpz) = y * x

*(x::($etype), y::UInt) = y * x

*(x::Integer, y::($etype)) = fmpz(x)*y

*(x::($etype), y::Integer) = y * x

###############################################################################
#
#   Shifting
#
###############################################################################

function shift_left(x::($etype), len::Int)
   len < 0 && throw(DomainError(len, "Shift must be non-negative"))
   xlen = pol_length(x)
   z = ($etype)(x)
   z.prec = x.prec + len
   z.val = x.val + len
   z.parent = parent(x)
   return z
end

function shift_right(x::($etype), len::Int)
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
      ccall(($(flint_fn*"_shift_right"), libflint), Nothing,
                (Ref{($etype)}, Ref{($etype)}, Int),
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

function truncate(x::($etype), prec::Int)
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
      ccall(($(flint_fn*"_set_trunc"), libflint), Nothing,
                (Ref{($etype)}, Ref{($etype)}, Int),
               z, x, min(prec - xval, xlen))
   end
   return z
end

###############################################################################
#
#   Powering
#
###############################################################################

function ^(a::($etype), b::Int)
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
      ccall(($(flint_fn*"_pow_trunc"), libflint), Nothing,
                (Ref{($etype)}, Ref{($etype)}, Int, Int),
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

function ==(x::($etype), y::($etype))
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
   return Bool(ccall(($(flint_fn*"_equal_trunc"), libflint), Cint,
                (Ref{($etype)}, Ref{($etype)}, Int),
               x, y, xlen))
end

function isequal(x::($etype), y::($etype))
   if parent(x) != parent(y)
      return false
   end
   if x.prec != y.prec || x.val != y.val || pol_length(x) != pol_length(y)
      return false
   end
   return Bool(ccall(($(flint_fn*"_equal"), libflint), Cint,
                (Ref{($etype)}, Ref{($etype)}, Int),
               x, y, pol_length(x)))
end

###############################################################################
#
#   Ad hoc comparisons
#
###############################################################################

function ==(x::($etype), y::($mtype))
   if precision(x) == 0
      return true
   elseif pol_length(x) > 1
      return false
   elseif pol_length(x) == 1
      if x.val == 0
         z = ccall(($(flint_fn*"_get_coeff_ui"), libflint), UInt,
                       (Ref{($etype)}, Int), x, 0)
         return data(y) == z
      else
         return false
      end
   else
      return iszero(data(y))
   end
end

==(x::($mtype), y::($etype)) = y == x

function ==(x::($etype), y::fmpz)
   if precision(x) == 0
      return true
   elseif pol_length(x) > 1
      return false
   elseif pol_length(x) == 1
      if x.val == 0
         r = ccall((:fmpz_fdiv_ui, libflint), UInt, (Ref{fmpz}, UInt), y, modulus(x))
         z = ccall(($(flint_fn*"_get_coeff_ui"), libflint), UInt,
                       (Ref{($etype)}, Int), x, 0)
         return r == z
      else
         return false
      end
   else
      r = ccall((:fmpz_fdiv_ui, libflint), UInt, (Ref{fmpz}, UInt), y, modulus(x))
      return r == UInt(0)
   end
end

==(x::fmpz, y::($etype)) = y == x

function ==(x::($etype), y::UInt)
   if precision(x) == 0
      return true
   elseif pol_length(x) > 1
      return false
   elseif pol_length(x) == 1
      if x.val == 0
         r = mod(y, modulus(x))
         z = ccall(($(flint_fn*"_get_coeff_ui"), libflint), UInt,
                       (Ref{($etype)}, Int), x, 0)
         return r == z
      else
         return false
      end
   else
      r = mod(y, modulus(x))
      return r == UInt(0)
   end
end

==(x::UInt, y::($etype)) = y == x

==(x::($etype), y::Integer) = x == fmpz(y)

==(x::Integer, y::($etype)) = y == x

###############################################################################
#
#   Exact division
#
###############################################################################

function divexact(x::($etype), y::($etype))
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
      ccall(($(flint_fn*"_div_series"), libflint), Nothing,
                (Ref{($etype)}, Ref{($etype)}, Ref{($etype)}, Int),
               z, x, y, prec)
   end
   return z
end

###############################################################################
#
#   Ad hoc exact division
#
###############################################################################

function divexact(x::($etype), y::($mtype))
   iszero(y) && throw(DivideError())
   z = parent(x)()
   z.prec = x.prec
   z.val = x.val
   r = inv(y)
   ccall(($(flint_fn*"_scalar_mul_nmod"), libflint), Nothing,
                (Ref{($etype)}, Ref{($etype)}, UInt),
               z, x, data(r))
   return z
end

function divexact(x::($etype), y::fmpz)
   iszero(y) && throw(DivideError())
   z = parent(x)()
   z.prec = x.prec
   z.prec = x.prec
   z.val = x.val
   r = ccall((:fmpz_fdiv_ui, libflint), UInt, (Ref{fmpz}, UInt), y, modulus(x))
   rinv = inv(base_ring(x)(r))
   ccall(($(flint_fn*"_scalar_mul_nmod"), libflint), Nothing,
                (Ref{($etype)}, Ref{($etype)}, UInt),
               z, x, data(rinv))
   return z
end

function divexact(x::($etype), y::UInt)
   iszero(y) && throw(DivideError())
   z = parent(x)()
   z.prec = x.prec
   z.prec = x.prec
   z.val = x.val
   r = mod(y, modulus(x))
   rinv = inv(base_ring(x)(r))
   ccall(($(flint_fn*"_scalar_mul_nmod"), libflint), Nothing,
                (Ref{($etype)}, Ref{($etype)}, UInt),
               z, x, data(rinv))
   return z
end

divexact(x::($etype), y::Integer) = divexact(x, fmpz(y))

###############################################################################
#
#   Inversion
#
###############################################################################

function inv(a::($etype))
   iszero(a) && throw(DivideError())
   !isunit(a) && error("Unable to invert power series")
   ainv = parent(a)()
   ainv.prec = a.prec
   ainv.val = 0
   ccall(($(flint_fn*"_inv_series"), libflint), Nothing,
                (Ref{($etype)}, Ref{($etype)}, Int),
               ainv, a, a.prec)
   return ainv
end

###############################################################################
#
#   Special functions
#
###############################################################################

function Base.exp(a::($etype))
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
   d = Vector{($mtype)}(undef, preca)
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
   ccall(($("_"*flint_fn*"_set_length"), libflint), Nothing,
         (Ref{($etype)}, Int), z, normalise(z, preca))
   return z
end

###############################################################################
#
#   Unsafe functions
#
###############################################################################

function zero!(x::($etype))
  ccall(($(flint_fn*"_zero"), libflint), Nothing,
                   (Ref{($etype)},), x)
  x.prec = parent(x).prec_max
  x.val = parent(x).prec_max
  return x
end

function fit!(x::($etype), n::Int)
  ccall(($(flint_fn*"_fit_length"), libflint), Nothing,
                   (Ref{($etype)}, Int), x, n)
  return nothing
end

function setcoeff!(z::($etype), n::Int, x::fmpz)
   r = ccall((:fmpz_fdiv_ui, libflint), UInt, (Ref{fmpz}, UInt), x, modulus(z))
   ccall(($(flint_fn*"_set_coeff_ui"), libflint), Nothing,
                (Ref{($etype)}, Int, UInt),
               z, n, r)
   return z
end

function setcoeff!(z::($etype), n::Int, x::UInt)
   r = mod(x, modulus(z))
   ccall(($(flint_fn*"_set_coeff_ui"), libflint), Nothing,
                (Ref{($etype)}, Int, UInt),
               z, n, r)
   return z
end

function setcoeff!(z::($etype), n::Int, x::($mtype))
   ccall(($(flint_fn*"_set_coeff_ui"), libflint), Nothing,
                (Ref{($etype)}, Int, UInt),
               z, n, data(x))
   return z
end

function mul!(z::($etype), a::($etype), b::($etype))
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
   ccall(($(flint_fn*"_mullow"), libflint), Nothing,
                (Ref{($etype)}, Ref{($etype)}, Ref{($etype)}, Int),
               z, a, b, lenz)
   renormalize!(z)
   return z
end

function addeq!(a::($etype), b::($etype))
   lena = pol_length(a)
   lenb = pol_length(b)
   prec = min(a.prec, b.prec)
   val = min(a.val, b.val)
   lena = min(lena, prec - a.val)
   lenb = min(lenb, prec - b.val)
   n = modulus(parent(a))
   if a.val < b.val
      z = ($etype)(n)
      z.parent = parent(a)
      lenz = max(lena, lenb + b.val - a.val)
      ccall(($(flint_fn*"_set_trunc"), libflint), Nothing,
            (Ref{($etype)}, Ref{($etype)}, Int),
            z, b, max(0, lenz - b.val + a.val))
      ccall(($(flint_fn*"_shift_left"), libflint), Nothing,
            (Ref{($etype)}, Ref{($etype)}, Int),
            z, z, b.val - a.val)
      ccall(($(flint_fn*"_add_series"), libflint), Nothing,
                (Ref{($etype)}, Ref{($etype)}, Ref{($etype)}, Int),
               a, a, z, lenz)
   elseif b.val < a.val
      lenz = max(lena + a.val - b.val, lenb)
      ccall(($(flint_fn*"_truncate"), libflint), Nothing,
            (Ref{($etype)}, Int),
            a, max(0, lenz - a.val + b.val))
      ccall(($(flint_fn*"_shift_left"), libflint), Nothing,
            (Ref{($etype)}, Ref{($etype)}, Int),
            a, a, a.val - b.val)
      ccall(($(flint_fn*"_add_series"), libflint), Nothing,
                (Ref{($etype)}, Ref{($etype)}, Ref{($etype)}, Int),
               a, a, b, lenz)
   else
      lenz = max(lena, lenb)
      ccall(($(flint_fn*"_add_series"), libflint), Nothing,
                (Ref{($etype)}, Ref{($etype)}, Ref{($etype)}, Int),
               a, a, b, lenz)
   end
   a.prec = prec
   a.val = val
   renormalize!(a)
   return a
end

function add!(c::($etype), a::($etype), b::($etype))
   if c === a
      return addeq!(c, b)
   elseif c === b
      return addeq!(c, a)
   end
   lena = pol_length(a)
   lenb = pol_length(b)
   prec = min(a.prec, b.prec)
   val = min(a.val, b.val)
   lena = min(lena, prec - a.val)
   lenb = min(lenb, prec - b.val)
   if a.val < b.val
      lenc = max(lena, lenb + b.val - a.val)
      ccall(($(flint_fn*"_set_trunc"), libflint), Nothing,
            (Ref{($etype)}, Ref{($etype)}, Int),
            c, b, max(0, lenc - b.val + a.val))
      ccall(($(flint_fn*"_shift_left"), libflint), Nothing,
            (Ref{($etype)}, Ref{($etype)}, Int),
            c, c, b.val - a.val)
      ccall(($(flint_fn*"_add_series"), libflint), Nothing,
                (Ref{($etype)}, Ref{($etype)}, Ref{($etype)}, Int),
               c, c, a, lenc)
   elseif b.val < a.val
      lenc = max(lena + a.val - b.val, lenb)
      ccall(($(flint_fn*"_set_trunc"), libflint), Nothing,
            (Ref{($etype)}, Ref{($etype)}, Int),
            c, a, max(0, lenc - a.val + b.val))
      ccall(($(flint_fn*"_shift_left"), libflint), Nothing,
            (Ref{($etype)}, Ref{($etype)}, Int),
            c, c, a.val - b.val)
      ccall(($(flint_fn*"_add_series"), libflint), Nothing,
                (Ref{($etype)}, Ref{($etype)}, Ref{($etype)}, Int),
               c, c, b, lenc)
   else
      lenc = max(lena, lenb)
      ccall(($(flint_fn*"_add_series"), libflint), Nothing,
                (Ref{($etype)}, Ref{($etype)}, Ref{($etype)}, Int),
               c, a, b, lenc)
   end
   c.prec = prec
   c.val = val
   renormalize!(c)
   return c
end

###############################################################################
#
#   Promotion rules
#
###############################################################################

promote_rule(::Type{($etype)}, ::Type{T}) where {T <: Integer} = ($etype)

promote_rule(::Type{($etype)}, ::Type{fmpz}) = ($etype)

promote_rule(::Type{($etype)}, ::Type{($mtype)}) = ($etype)

###############################################################################
#
#   Parent object call overload
#
###############################################################################

function (a::($rtype))()
   z = ($etype)(modulus(a))
   z.prec = a.prec_max
   z.val = a.prec_max
   z.parent = a
   return z
end

function (a::($rtype))(b::Integer)
   if b == 0
      z = ($etype)(modulus(a))
      z.prec = a.prec_max
      z.val = a.prec_max
   else
      z = ($etype)(modulus(a), [fmpz(b)], 1, a.prec_max, 0)
   end
   z.parent = a
   return z
end

function (a::($rtype))(b::fmpz)
   if iszero(b)
      z = ($etype)(modulus(a))
      z.prec = a.prec_max
      z.val = a.prec_max
   else
      z = ($etype)(modulus(a), [b], 1, a.prec_max, 0)
   end
   z.parent = a
   return z
end

function (a::($rtype))(b::($mtype))
   if iszero(b)
      z = ($etype)(modulus(a))
      z.prec = a.prec_max
      z.val = a.prec_max
   else
      z = ($etype)(modulus(a), [b], 1, a.prec_max, 0)
   end
   z.parent = a
   return z
end

function (a::($rtype))(b::($etype))
   parent(b) != a && error("Unable to coerce power series")
   return b
end

function (a::($rtype))(b::Vector{fmpz}, len::Int, prec::Int, val::Int)
   z = ($etype)(modulus(a), b, len, prec, val)
   z.parent = a
   return z
end

function (a::($rtype))(b::Vector{UInt}, len::Int, prec::Int, val::Int)
   z = ($etype)(modulus(a), b, len, prec, val)
   z.parent = a
   return z
end

function (a::($rtype))(b::Vector{($mtype)}, len::Int, prec::Int, val::Int)
   z = ($etype)(modulus(a), b, len, prec, val)
   z.parent = a
   return z
end

end # eval
end # for
