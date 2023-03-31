###############################################################################
#
#   fmpz_mod_rel_series.jl: Relative series using fmpz_mod_poly
#
#   fmpz_mod_rel_series, gfp_fmpz_rel_series
#
###############################################################################

export ZZModRelPowerSeriesRingElem, ZZModRelPowerSeriesRing,
       FpRelPowerSeriesRingElem, FpRelPowerSeriesRing

for (etype, rtype, ctype, mtype, brtype, flint_fn) in (
   (ZZModRelPowerSeriesRingElem, ZZModRelPowerSeriesRing, fmpz_mod_ctx_struct, ZZModRingElem, ZZModRing, "fmpz_mod_poly"),
   (FpRelPowerSeriesRingElem, FpRelPowerSeriesRing, fmpz_mod_ctx_struct, FpFieldElem, FpField, "fmpz_mod_poly"))
@eval begin

###############################################################################
#
#   Data type and parent object methods
#
###############################################################################

function O(a::($etype))
   val = pol_length(a) + valuation(a) - 1
   val < 0 && throw(DomainError(val, "Valuation must be non-negative"))
   z = ($etype)(base_ring(a), Vector{ZZRingElem}(undef, 0), 0, val, val)
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
      c = ZZRingElem()
      while len > 0
         ccall(($(flint_fn*"_get_coeff_fmpz"), libflint), Nothing,
               (Ref{ZZRingElem}, Ref{($etype)}, Int,
                Ref{($ctype)}),
               c, a, len - 1, a.parent.base_ring.ninv)
         if !iszero(c)
            break
         end
         len -= 1
      end
   end
   return len
end

function pol_length(x::($etype))
   return x.length
#   return ccall(($(flint_fn*"_length"), libflint), Int,
#                (Ref{($etype)}, Ref{($ctype)}),
#                x, x.parent.base_ring.ninv)
end

precision(x::($etype)) = x.prec

function polcoeff(x::($etype), n::Int)
   R = base_ring(x)
   if n < 0
      return R(0)
   end
   z = ZZRingElem()
   ccall(($(flint_fn*"_get_coeff_fmpz"), libflint), Nothing,
         (Ref{ZZRingElem}, Ref{($etype)}, Int, Ref{($ctype)}),
         z, x, n, x.parent.base_ring.ninv)
   return R(z)
end

zero(R::($rtype)) = R(0)

one(R::($rtype)) = R(1)

function gen(R::($rtype))
   z = ($etype)(base_ring(R), [ZZRingElem(1)], 1, max_precision(R) + 1, 1)
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
            (Ref{($etype)}, Ref{($etype)}, Int,
             Ref{($ctype)}),
            z, z, i, z.parent.base_ring.ninv)
   end
   return nothing
end

characteristic(R::($rtype)) = modulus(R)

###############################################################################
#
#   Similar
#
###############################################################################

function similar(f::RelPowerSeriesRingElem, R::($brtype), max_prec::Int,
                                   s::Symbol=var(parent(f)); cached::Bool=true)
   par = ($rtype)(R, max_prec, s, cached)
   z = ($etype)(R)
   if base_ring(f) === R && s == var(parent(f)) &&
      f isa ($etype) && max_precision(parent(f)) == max_prec
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
                   len::Int, prec::Int, val::Int, var::VarName=:x;
                            max_precision::Int=prec, cached::Bool=true) where T
   prec < len + val && error("Precision too small for given data")
   coeffs = T == ($mtype) ? arr : map(R, arr)
   coeffs = length(coeffs) == 0 ? ($mtype)[] : coeffs
   par = ($rtype)(R, max_precision, Symbol(var), cached)
   z = ($etype)(R, coeffs, len, prec, val)
   z.parent = par
   return z
end

###############################################################################
#
#   Unary operators
#
###############################################################################

function -(x::($etype))
   z = parent(x)()
   ccall(($(flint_fn*"_neg"), libflint), Nothing,
         (Ref{($etype)}, Ref{($etype)},
          Ref{($ctype)}),
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

function +(a::($etype), b::($etype))
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
      ccall(($(flint_fn*"_set_trunc"), libflint), Nothing,
            (Ref{($etype)}, Ref{($etype)}, Int,
             Ref{($ctype)}),
            z, b, max(0, lenz - b.val + a.val), p)
      ccall(($(flint_fn*"_shift_left"), libflint), Nothing,
            (Ref{($etype)}, Ref{($etype)}, Int,
             Ref{($ctype)}),
            z, z, b.val - a.val, p)
      ccall(($(flint_fn*"_add_series"), libflint), Nothing,
            (Ref{($etype)}, Ref{($etype)},
             Ref{($etype)}, Int, Ref{($ctype)}),
            z, z, a, lenz, p)
   elseif b.val < a.val
      lenz = max(lena + a.val - b.val, lenb)
      ccall(($(flint_fn*"_set_trunc"), libflint), Nothing,
            (Ref{($etype)}, Ref{($etype)}, Int,
             Ref{($ctype)}),
            z, a, max(0, lenz - a.val + b.val), p)
      ccall(($(flint_fn*"_shift_left"), libflint), Nothing,
            (Ref{($etype)}, Ref{($etype)}, Int,
             Ref{($ctype)}),
            z, z, a.val - b.val, p)
      ccall(($(flint_fn*"_add_series"), libflint), Nothing,
            (Ref{($etype)}, Ref{($etype)},
             Ref{($etype)}, Int, Ref{($ctype)}),
            z, z, b, lenz, p)
   else
      lenz = max(lena, lenb)
      ccall(($(flint_fn*"_add_series"), libflint), Nothing,
            (Ref{($etype)}, Ref{($etype)},
             Ref{($etype)}, Int, Ref{($ctype)}),
            z, a, b, lenz, p)
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
   p = a.parent.base_ring.ninv
   if a.val < b.val
      lenz = max(lena, lenb + b.val - a.val)
      ccall(($(flint_fn*"_set_trunc"), libflint), Nothing,
            (Ref{($etype)}, Ref{($etype)}, Int,
             Ref{($ctype)}),
            z, b, max(0, lenz - b.val + a.val), p)
      ccall(($(flint_fn*"_shift_left"), libflint), Nothing,
            (Ref{($etype)}, Ref{($etype)}, Int,
             Ref{($ctype)}),
            z, z, b.val - a.val, p)
      ccall(($(flint_fn*"_neg"), libflint), Nothing,
            (Ref{($etype)}, Ref{($etype)},
             Ref{($ctype)}),
            z, z, p)
      ccall(($(flint_fn*"_add_series"), libflint), Nothing,
            (Ref{($etype)}, Ref{($etype)},
             Ref{($etype)}, Int, Ref{($ctype)}),
            z, z, a, lenz, p)
   elseif b.val < a.val
      lenz = max(lena + a.val - b.val, lenb)
      ccall(($(flint_fn*"_set_trunc"), libflint), Nothing,
            (Ref{($etype)}, Ref{($etype)}, Int,
             Ref{($ctype)}),
            z, a, max(0, lenz - a.val + b.val), p)
      ccall(($(flint_fn*"_shift_left"), libflint), Nothing,
            (Ref{($etype)}, Ref{($etype)}, Int,
             Ref{($ctype)}),
            z, z, a.val - b.val, p)
      ccall(($(flint_fn*"_sub_series"), libflint), Nothing,
            (Ref{($etype)}, Ref{($etype)},
             Ref{($etype)}, Int, Ref{($ctype)}),
            z, z, b, lenz, p)
   else
      lenz = max(lena, lenb)
      ccall(($(flint_fn*"_sub_series"), libflint), Nothing,
            (Ref{($etype)}, Ref{($etype)},
             Ref{($etype)}, Int, Ref{($ctype)}),
            z, a, b, lenz, p)
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
         (Ref{($etype)}, Ref{($etype)},
          Ref{($etype)}, Int, Ref{($ctype)}),
         z, a, b, lenz, a.parent.base_ring.ninv)
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
   ccall(($(flint_fn*"_scalar_mul_fmpz"), libflint), Nothing,
         (Ref{($etype)}, Ref{($etype)}, Ref{ZZRingElem},
          Ref{($ctype)}),
         z, y, x.data, y.parent.base_ring.ninv)
   renormalize!(z)
   return z
end

*(x::($etype), y::($mtype)) = y * x

function *(x::ZZRingElem, y::($etype))
   z = parent(y)()
   z.prec = y.prec
   z.val = y.val
   ccall(($(flint_fn*"_scalar_mul_fmpz"), libflint), Nothing,
         (Ref{($etype)}, Ref{($etype)}, Ref{ZZRingElem},
          Ref{($ctype)}),
         z, y, x, y.parent.base_ring.ninv)
   renormalize!(z)
   return z
end

*(x::($etype), y::ZZRingElem) = y * x

*(x::Integer, y::($etype)) = ZZRingElem(x)*y

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
            (Ref{($etype)}, Ref{($etype)}, Int,
             Ref{($ctype)}),
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
            (Ref{($etype)}, Ref{($etype)}, Int,
             Ref{($ctype)}),
            z, x, min(prec - xval, xlen), x.parent.base_ring.ninv)
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
   if is_gen(a)
      z = parent(a)()
      z = setcoeff!(z, 0, ZZRingElem(1))
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
            (Ref{($etype)}, Ref{($etype)}, UInt, Int,
             Ref{($ctype)}),
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
                     (Ref{($etype)}, Ref{($etype)}, Int,
                      Ref{($ctype)}),
                     x, y, xlen, y.parent.base_ring.ninv))
end

function isequal(x::($etype), y::($etype))
   if parent(x) != parent(y)
      return false
   end
   if x.prec != y.prec || x.val != y.val || pol_length(x) != pol_length(y)
      return false
   end
   return Bool(ccall(($(flint_fn*"_equal"), libflint), Cint,
                     (Ref{($etype)}, Ref{($etype)}, Ref{($ctype)}),
                     x, y, x.parent.base_ring.ninv))
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
         z = ZZRingElem()
         ccall(($(flint_fn*"_get_coeff_fmpz"), libflint), Nothing,
               (Ref{ZZRingElem}, Ref{($etype)}, Int,
                Ref{($ctype)}),
               z, x, 0, x.parent.base_ring.ninv)
         return Bool(ccall((:fmpz_equal, libflint), Cint,
                           (Ref{ZZRingElem}, Ref{ZZRingElem}),
                           z, y.data))
      else
         return false
      end
   else
      return iszero(y.data)
   end
end

==(x::($mtype), y::($etype)) = y == x

function ==(x::($etype), y::ZZRingElem)
   if precision(x) == 0
      return true
   elseif pol_length(x) > 1
      return false
   elseif pol_length(x) == 1
      if x.val == 0
         z = ZZRingElem()
         ccall(($(flint_fn*"_get_coeff_fmpz"), libflint), Nothing,
               (Ref{ZZRingElem}, Ref{($etype)}, Int,
                Ref{($ctype)}),
               z, x, 0, x.parent.base_ring.ninv)
         r = mod(y, modulus(x))
         return Bool(ccall((:fmpz_equal, libflint), Cint,
                           (Ref{ZZRingElem}, Ref{ZZRingElem}),
                           z, r))
      else
         return false
      end
   else
      r = mod(y, modulus(x))
      return iszero(r)
   end
end

==(x::ZZRingElem, y::($etype)) = y == x

==(x::($etype), y::Integer) = x == ZZRingElem(y)

==(x::Integer, y::($etype)) = y == x

###############################################################################
#
#   Exact division
#
###############################################################################

function divexact(x::($etype), y::($etype); check::Bool=true)
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
   check && !is_unit(y) && error("Unable to invert power series")
   prec = min(x.prec - x.val, y.prec - y.val)
   z = parent(x)()
   z.val = xval - yval
   z.prec = prec + z.val
   if prec != 0
      ccall(($(flint_fn*"_div_series"), libflint), Nothing,
            (Ref{($etype)}, Ref{($etype)},
             Ref{($etype)}, Int, Ref{($ctype)}),
            z, x, y, prec, x.parent.base_ring.ninv)
   end
   return z
end

###############################################################################
#
#   Ad hoc exact division
#
###############################################################################

function divexact(x::($etype), y::($mtype); check::Bool=true)
   iszero(y) && throw(DivideError())
   z = parent(x)()
   z.prec = x.prec
   z.val = x.val
   ccall(($(flint_fn*"_scalar_div_fmpz"), libflint), Nothing,
         (Ref{($etype)}, Ref{($etype)}, Ref{ZZRingElem},
          Ref{($ctype)}),
         z, x, y.data, x.parent.base_ring.ninv)
   return z
end

function divexact(x::($etype), y::ZZRingElem; check::Bool=true)
   iszero(y) && throw(DivideError())
   z = parent(x)()
   z.prec = x.prec
   z.prec = x.prec
   z.val = x.val
   r = mod(y, modulus(x))
   ccall(($(flint_fn*"_scalar_div_fmpz"), libflint), Nothing,
         (Ref{($etype)}, Ref{($etype)}, Ref{ZZRingElem},
          Ref{($ctype)}),
         z, x, r, x.parent.base_ring.ninv)
   return z
end

divexact(x::($etype), y::Integer; check::Bool=true) = divexact(x, ZZRingElem(y); check=check)

###############################################################################
#
#   Inversion
#
###############################################################################

function inv(a::($etype))
   iszero(a) && throw(DivideError())
   !is_unit(a) && error("Unable to invert power series")
   ainv = parent(a)()
   ainv.prec = a.prec
   ainv.val = 0
   ccall(($(flint_fn*"_inv_series"), libflint), Nothing,
         (Ref{($etype)}, Ref{($etype)}, Int,
          Ref{($ctype)}),
         ainv, a, a.prec, a.parent.base_ring.ninv)
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
   d = Vector{ZZRingElem}(undef, preca)
   c = vala == 0 ? polcoeff(a, 0) : R()
   d[1] = exp(c).data
   len = pol_length(a) + vala
   z0 = ZZRingElem()
   for k = 1 : preca - 1
      s = ZZRingElem()
      for j = 1 : min(k + 1, len) - 1
         c = j >= vala ? polcoeff(a, j - vala).data : z0
         s += j * c * d[k - j + 1]
      end
      !is_unit(base_ring(a)(k)) && error("Unable to divide in exp")
      d[k + 1] = divexact(base_ring(a)(s), k).data
   end
   z = parent(a)(d, preca, preca, 0)
   ccall(($("_"*flint_fn*"_set_length"), libflint), Nothing,
         (Ref{($etype)}, Int, Ref{($ctype)}),
         z, normalise(z, preca), a.parent.base_ring.ninv)
   return z
end

###############################################################################
#
#   Unsafe functions
#
###############################################################################

function zero!(x::($etype))
  ccall(($(flint_fn*"_zero"), libflint), Nothing,
        (Ref{($etype)}, Ref{($ctype)}),
        x, x.parent.base_ring.ninv)
  x.prec = parent(x).prec_max
  x.val = parent(x).prec_max
  return x
end

function fit!(x::($etype), n::Int)
  ccall(($(flint_fn*"_fit_length"), libflint), Nothing,
        (Ref{($etype)}, Int, Ref{($ctype)}),
        x, n, x.parent.base_ring.ninv)
  return nothing
end

function setcoeff!(z::($etype), n::Int, x::ZZRingElem)
   ccall(($(flint_fn*"_set_coeff_fmpz"), libflint), Nothing,
         (Ref{($etype)}, Int, Ref{ZZRingElem}, Ref{($ctype)}),
         z, n, x, z.parent.base_ring.ninv)
   return z
end

function setcoeff!(z::($etype), n::Int, x::($mtype))
   ccall(($(flint_fn*"_set_coeff_fmpz"), libflint), Nothing,
         (Ref{($etype)}, Int, Ref{ZZRingElem}, Ref{($ctype)}),
         z, n, x.data, z.parent.base_ring.ninv)
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
         (Ref{($etype)}, Ref{($etype)},
          Ref{($etype)}, Int, Ref{($ctype)}),
         z, a, b, lenz, z.parent.base_ring.ninv)
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
   p = a.parent.base_ring.ninv
   if a.val < b.val
      z = ($etype)(p)
      z.parent = parent(a)
      lenz = max(lena, lenb + b.val - a.val)
      ccall(($(flint_fn*"_set_trunc"), libflint), Nothing,
            (Ref{($etype)}, Ref{($etype)}, Int,
             Ref{($ctype)}),
            z, b, max(0, lenz - b.val + a.val), p)
      ccall(($(flint_fn*"_shift_left"), libflint), Nothing,
            (Ref{($etype)}, Ref{($etype)}, Int,
             Ref{($ctype)}),
            z, z, b.val - a.val, p)
      ccall(($(flint_fn*"_add_series"), libflint), Nothing,
            (Ref{($etype)}, Ref{($etype)},
             Ref{($etype)}, Int, Ref{($ctype)}),
            a, a, z, lenz, p)
   elseif b.val < a.val
      lenz = max(lena + a.val - b.val, lenb)
      ccall(($(flint_fn*"_truncate"), libflint), Nothing,
            (Ref{($etype)}, Int, Ref{($ctype)}),
            a, max(0, lenz - a.val + b.val), p)
      ccall(($(flint_fn*"_shift_left"), libflint), Nothing,
            (Ref{($etype)}, Ref{($etype)}, Int,
             Ref{($ctype)}),
            a, a, a.val - b.val, p)
      ccall(($(flint_fn*"_add_series"), libflint), Nothing,
            (Ref{($etype)}, Ref{($etype)},
             Ref{($etype)}, Int, Ref{($ctype)}),
            a, a, b, lenz, p)
   else
      lenz = max(lena, lenb)
      ccall(($(flint_fn*"_add_series"), libflint), Nothing,
            (Ref{($etype)}, Ref{($etype)},
             Ref{($etype)}, Int, Ref{($ctype)}),
            a, a, b, lenz, p)
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
   p = a.parent.base_ring.ninv
   if a.val < b.val
      lenc = max(lena, lenb + b.val - a.val)
      ccall(($(flint_fn*"_set_trunc"), libflint), Nothing,
            (Ref{($etype)}, Ref{($etype)}, Int,
             Ref{($ctype)}),
            c, b, max(0, lenc - b.val + a.val), p)
      ccall(($(flint_fn*"_shift_left"), libflint), Nothing,
            (Ref{($etype)}, Ref{($etype)}, Int,
             Ref{($ctype)}),
            c, c, b.val - a.val, p)
      ccall(($(flint_fn*"_add_series"), libflint), Nothing,
            (Ref{($etype)}, Ref{($etype)},
             Ref{($etype)}, Int, Ref{($ctype)}),
            c, c, a, lenc, p)
   elseif b.val < a.val
      lenc = max(lena + a.val - b.val, lenb)
      ccall(($(flint_fn*"_set_trunc"), libflint), Nothing,
            (Ref{($etype)}, Ref{($etype)}, Int,
             Ref{($ctype)}),
            c, a, max(0, lenc - a.val + b.val), p)
      ccall(($(flint_fn*"_shift_left"), libflint), Nothing,
            (Ref{($etype)}, Ref{($etype)}, Int,
             Ref{($ctype)}),
            c, c, a.val - b.val, p)
      ccall(($(flint_fn*"_add_series"), libflint), Nothing,
            (Ref{($etype)}, Ref{($etype)},
             Ref{($etype)}, Int, Ref{($ctype)}),
            c, c, b, lenc, p)
   else
      lenc = max(lena, lenb)
      ccall(($(flint_fn*"_add_series"), libflint), Nothing,
            (Ref{($etype)}, Ref{($etype)},
             Ref{($etype)}, Int, Ref{($ctype)}),
            c, a, b, lenc, p)
   end
   c.prec = prec
   c.val = val
   renormalize!(c)
   return c
end

function set_length!(a::($etype), n::Int)
   ccall(($("_"*flint_fn*"_set_length"), libflint), Nothing,
         (Ref{($etype)}, Int, Ref{$(ctype)}),
         a, n, a.parent.base_ring.ninv)
   return a
end

###############################################################################
#
#   Promotion rules
#
###############################################################################

promote_rule(::Type{($etype)}, ::Type{T}) where {T <: Integer} = ($etype)

promote_rule(::Type{($etype)}, ::Type{ZZRingElem}) = ($etype)

promote_rule(::Type{($etype)}, ::Type{($mtype)}) = ($etype)

###############################################################################
#
#   Parent object call overload
#
###############################################################################

function (a::($rtype))()
   z = ($etype)(base_ring(a))
   z.prec = a.prec_max
   z.val = a.prec_max
   z.parent = a
   return z
end

function (a::($rtype))(b::Integer)
   if b == 0
      z = ($etype)(base_ring(a))
      z.prec = a.prec_max
      z.val = a.prec_max
   else
      z = ($etype)(base_ring(a), [ZZRingElem(b)], 1, a.prec_max, 0)
   end
   z.parent = a
   return z
end

function (a::($rtype))(b::ZZRingElem)
   if iszero(b)
      z = ($etype)(base_ring(a))
      z.prec = a.prec_max
      z.val = a.prec_max
   else
      z = ($etype)(base_ring(a), [b], 1, a.prec_max, 0)
   end
   z.parent = a
   return z
end

function (a::($rtype))(b::($mtype))
   if iszero(b)
      z = ($etype)(base_ring(a))
      z.prec = a.prec_max
      z.val = a.prec_max
   else
      z = ($etype)(base_ring(a), [b], 1, a.prec_max, 0)
   end
   z.parent = a
   return z
end

function (a::($rtype))(b::($etype))
   parent(b) != a && error("Unable to coerce power series")
   return b
end

function (a::($rtype))(b::Vector{ZZRingElem}, len::Int, prec::Int, val::Int)
   z = ($etype)(base_ring(a), b, len, prec, val)
   z.parent = a
   return z
end

function (a::($rtype))(b::Vector{($mtype)}, len::Int, prec::Int, val::Int)
   z = ($etype)(base_ring(a), b, len, prec, val)
   z.parent = a
   return z
end

end # eval
end # for

###############################################################################
#
#   Square root
#
###############################################################################

function sqrt_classical_char2(a::FpRelPowerSeriesRingElem; check::Bool=true)
   S = parent(a)
   R = base_ring(a)
   prec = div(precision(a) + 1, 2)
   if iszero(a)
      asqrt = parent(a)()
      asqrt = set_precision!(asqrt, prec)
      asqrt = set_valuation!(asqrt, prec)
      return true, asqrt
   end
   aval = valuation(a)
   if check && !iseven(aval)
      return false, S()
   end
   aval2 = div(aval, 2)
   asqrt = parent(a)()
   asqrt = set_precision!(asqrt, prec)
   asqrt = set_valuation!(asqrt, aval2)
   if check
      for i = 1:2:precision(a) - aval - 1 # series must have even exponents
         if !iszero(polcoeff(a, i))
            return false, S()
         end
      end
   end
   for i = 0:prec - aval2 - 1
      asqrt = setcoeff!(asqrt, i, polcoeff(a, 2*i))
   end
   return true, asqrt
end

function sqrt_classical(a::FpRelPowerSeriesRingElem; check::Bool=true)
   S = parent(a)
   R = base_ring(a)
   v = valuation(a)
   z = S()
   v2 = div(v, 2)
   if iszero(a)
      z.prec = v2
      z.val = v2
      return true, z
   end
   if check && !iseven(v)
      return false, S()
   end
   if characteristic(R) == 2
      return sqrt_classical_char2(a; check=check)
   end
   z.prec = a.prec - v2
   z.val = v2
   c = coeff(a, v)
   if check
      flag, s = issquare_with_sqrt(c)
      if !flag
         return false, S()
      end
   else
      s = sqrt(c; check=check)
   end
   a = divexact(a, c)
   ccall((:fmpz_mod_poly_sqrt_series, libflint), Nothing,
                (Ref{FpRelPowerSeriesRingElem}, Ref{FpRelPowerSeriesRingElem},
                 Int, Ref{fmpz_mod_ctx_struct}),
               z, a, a.prec, a.parent.base_ring.ninv)
   if !isone(s)
      z *= s
   end
   return true, z
end

@doc raw"""
    sqrt(a::FpRelPowerSeriesRingElem)

Return the square root of the power series $a$. By default the function raises
an exception if the input is not a square. If `check=false` this check is
omitted.
"""
function Base.sqrt(a::FpRelPowerSeriesRingElem; check::Bool=true)
   flag, q = sqrt_classical(a; check=check)
   if check && !flag
      error("Not a square in sqrt")
   end
   return q
end

function issquare(a::FpRelPowerSeriesRingElem)
   flag, q = sqrt_classical(a; check=true)
   return flag
end

function issquare_with_sqrt(a::FpRelPowerSeriesRingElem)
   return sqrt_classical(a; check=true)
end
