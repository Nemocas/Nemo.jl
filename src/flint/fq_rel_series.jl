###############################################################################
#
#   fq_rel_series.jl: Relative series over finite fields
#
#   fq_rel_series, fq_nmod_rel_series
#
###############################################################################

export fq_rel_series, FqRelSeriesRing,
       fq_nmod_rel_series, FqNmodRelSeriesRing

for (etype, rtype, ctype, btype, flint_fn, flint_tail) in (
   (fq_rel_series, FqRelSeriesRing, FqFiniteField, fq, "fq_poly", "fq"),
   (fq_nmod_rel_series, FqNmodRelSeriesRing, FqNmodFiniteField, fq_nmod, "fq_nmod_poly", "fq_nmod"))
@eval begin

###############################################################################
#
#   Data type and parent object methods
#
###############################################################################

function O(a::($etype))
   val = pol_length(a) + valuation(a) - 1
   val < 0 && throw(DomainError(val, "Valuation must be non-negative"))
   z = ($etype)(base_ring(a), Vector{($btype)}(undef, 0), 0, val, val)
   z.parent = parent(a)
   return z
end

elem_type(::Type{($rtype)}) = ($etype)

parent_type(::Type{($etype)}) = ($rtype)

base_ring(R::($rtype)) = R.base_ring

rel_series_type(::Type{($btype)}) = ($etype)

var(a::($rtype)) = a.S

###############################################################################
#
#   Basic manipulation
#
###############################################################################

max_precision(R::($rtype)) = R.prec_max

function normalise(a::($etype), len::Int)
   ctx = base_ring(a)
   if len > 0
      c = base_ring(a)()
      ccall(($(flint_fn*"_get_coeff"), libflint), Nothing,
         (Ref{($btype)}, Ref{($etype)}, Int, Ref{($ctype)}),
                                                         c, a, len - 1, ctx)
   end
   while len > 0 && iszero(c)
      len -= 1
      if len > 0
         ccall(($(flint_fn*"_get_coeff"), libflint), Nothing,
            (Ref{($btype)}, Ref{($etype)}, Int, Ref{($ctype)}),
                                                         c, a, len - 1, ctx)
      end
   end
   return len
end

function pol_length(x::($etype))
   return ccall(($(flint_fn*"_length"), libflint), Int,
                (Ref{($etype)}, Ref{($ctype)}), x, base_ring(x))
end

precision(x::($etype)) = x.prec

function polcoeff(x::($etype), n::Int)
   z = base_ring(x)()
   if n < 0
      return z
   end
   ccall(($(flint_fn*"_get_coeff"), libflint), Nothing,
         (Ref{($btype)}, Ref{($etype)}, Int, Ref{($ctype)}),
                                                      z, x, n, base_ring(x))
   return z
end

zero(R::($rtype)) = R(0)

one(R::($rtype)) = R(1)

function gen(R::($rtype))
   z = ($etype)(base_ring(R), [base_ring(R)(1)], 1, max_precision(R) + 1, 1)
   z.parent = R
   return z
end

function deepcopy_internal(a::($etype), dict::IdDict)
   z = ($etype)(base_ring(a), a)
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
            (Ref{($etype)}, Ref{($etype)}, Int, Ref{($ctype)}),
                                                      z, z, i, base_ring(z))
   end
   return nothing
end

characteristic(R::($rtype)) = characteristic(base_ring(R))

###############################################################################
#
#   Similar
#
###############################################################################

function similar(f::RelSeriesElem, R::($ctype), max_prec::Int,
                                   s::Symbol=var(parent(f)); cached::Bool=true)
   par = ($rtype)(R, max_prec, s, cached)
   z = ($etype)(R)
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

function rel_series(R::($ctype), arr::Vector{T},
                   len::Int, prec::Int, val::Int, var::String="x";
                            max_precision::Int=prec, cached::Bool=true) where T
   prec < len + val && error("Precision too small for given data")
   coeffs = T == ($btype) ? arr : map(R, arr)
   coeffs = length(coeffs) == 0 ? ($btype)[] : coeffs
   par = ($rtype)(R, max_precision, Symbol(var), cached)
   z = ($etype)(R, coeffs, len, prec, val)
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
                (Ref{($etype)}, Ref{($etype)}, Ref{($ctype)}),
               z, x, base_ring(x))
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
   ctx = base_ring(a)
   if a.val < b.val
      lenz = max(lena, lenb + b.val - a.val)
      ccall(($(flint_fn*"_set_trunc"), libflint), Nothing,
            (Ref{($etype)}, Ref{($etype)}, Int, Ref{($ctype)}),
            z, b, max(0, lenz - b.val + a.val), ctx)
      ccall(($(flint_fn*"_shift_left"), libflint), Nothing,
            (Ref{($etype)}, Ref{($etype)}, Int, Ref{($ctype)}),
            z, z, b.val - a.val, ctx)
      ccall(($(flint_fn*"_add_series"), libflint), Nothing,
                (Ref{($etype)}, Ref{($etype)},
                 Ref{($etype)}, Int, Ref{($ctype)}),
               z, z, a, lenz, ctx)
   elseif b.val < a.val
      lenz = max(lena + a.val - b.val, lenb)
      ccall(($(flint_fn*"_set_trunc"), libflint), Nothing,
            (Ref{($etype)}, Ref{($etype)}, Int, Ref{($ctype)}),
            z, a, max(0, lenz - a.val + b.val), ctx)
      ccall(($(flint_fn*"_shift_left"), libflint), Nothing,
            (Ref{($etype)}, Ref{($etype)}, Int, Ref{($ctype)}),
            z, z, a.val - b.val, ctx)
      ccall(($(flint_fn*"_add_series"), libflint), Nothing,
                (Ref{($etype)}, Ref{($etype)},
                 Ref{($etype)}, Int, Ref{($ctype)}),
               z, z, b, lenz, ctx)
   else
      lenz = max(lena, lenb)
      ccall(($(flint_fn*"_add_series"), libflint), Nothing,
                (Ref{($etype)}, Ref{($etype)},
                 Ref{($etype)}, Int, Ref{($ctype)}),
               z, a, b, lenz, ctx)
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
   ctx = base_ring(a)
   if a.val < b.val
      lenz = max(lena, lenb + b.val - a.val)
      ccall(($(flint_fn*"_set_trunc"), libflint), Nothing,
            (Ref{($etype)}, Ref{($etype)}, Int, Ref{($ctype)}),
            z, b, max(0, lenz - b.val + a.val), ctx)
      ccall(($(flint_fn*"_shift_left"), libflint), Nothing,
            (Ref{($etype)}, Ref{($etype)}, Int, Ref{($ctype)}),
            z, z, b.val - a.val, ctx)
      ccall(($(flint_fn*"_neg"), libflint), Nothing,
            (Ref{($etype)}, Ref{($etype)}, Ref{($ctype)}),
            z, z, ctx)
      ccall(($(flint_fn*"_add_series"), libflint), Nothing,
                (Ref{($etype)}, Ref{($etype)},
                 Ref{($etype)}, Int, Ref{($ctype)}),
               z, z, a, lenz, ctx)
   elseif b.val < a.val
      lenz = max(lena + a.val - b.val, lenb)
      ccall(($(flint_fn*"_set_trunc"), libflint), Nothing,
            (Ref{($etype)}, Ref{($etype)}, Int, Ref{($ctype)}),
            z, a, max(0, lenz - a.val + b.val), ctx)
      ccall(($(flint_fn*"_shift_left"), libflint), Nothing,
            (Ref{($etype)}, Ref{($etype)}, Int, Ref{($ctype)}),
            z, z, a.val - b.val, ctx)
      ccall(($(flint_fn*"_sub_series"), libflint), Nothing,
                (Ref{($etype)}, Ref{($etype)},
                 Ref{($etype)}, Int, Ref{($ctype)}),
               z, z, b, lenz, ctx)
   else
      lenz = max(lena, lenb)
      ccall(($(flint_fn*"_sub_series"), libflint), Nothing,
                (Ref{($etype)}, Ref{($etype)},
                 Ref{($etype)}, Int, Ref{($ctype)}),
               z, a, b, lenz, ctx)
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
               z, a, b, lenz, base_ring(a))
   return z
end

###############################################################################
#
#   Ad hoc binary operators
#
###############################################################################

function *(x::($btype), y::($etype))
   z = parent(y)()
   z.prec = y.prec
   z.val = y.val
   ccall(($(flint_fn*"_scalar_mul_"*flint_tail), libflint), Nothing,
                (Ref{($etype)}, Ref{($etype)},
                 Ref{($btype)}, Ref{($ctype)}),
               z, y, x, base_ring(y))
   return z
end

*(x::($etype), y::($btype)) = y * x

###############################################################################
#
#   Shifting
#
###############################################################################

function shift_left(x::($etype), len::Int)
   len < 0 && throw(DomainError(len, "Shift must be non-negative"))
   xlen = pol_length(x)
   z = ($etype)(base_ring(x), x)
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
                (Ref{($etype)}, Ref{($etype)},
                 Int, Ref{($ctype)}),
               z, x, xlen - zlen, base_ring(x))
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
                (Ref{($etype)}, Ref{($etype)},
                 Int, Ref{($ctype)}),
               z, x, min(prec - xval, xlen), base_ring(x))
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
                (Ref{($etype)}, Ref{($etype)},
                 Int, Ref{($ctype)}),
               x, y, xlen, base_ring(x)))
end

function isequal(x::($etype), y::($etype))
   if parent(x) != parent(y)
      return false
   end
   if x.prec != y.prec || x.val != y.val || pol_length(x) != pol_length(y)
      return false
   end
   return Bool(ccall(($(flint_fn*"_equal"), libflint), Cint,
                (Ref{($etype)}, Ref{($etype)},
                 Int, Ref{($ctype)}),
               x, y, pol_length(x), base_ring(x)))
end

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
                (Ref{($etype)}, Ref{($etype)},
                 Ref{($etype)}, Int, Ref{($ctype)}),
               z, x, y, prec, base_ring(x))
   end
   return z
end

###############################################################################
#
#   Ad hoc exact division
#
###############################################################################

function divexact(x::($etype), y::fq)
   iszero(y) && throw(DivideError())
   z = parent(x)()
   z.prec = x.prec
   z.prec = x.prec
   z.val = x.val
   ccall(($(flint_fn*"_scalar_div_"*flint_tail), libflint), Nothing,
                (Ref{($etype)}, Ref{($etype)},
                 Ref{($btype)}, Ref{($ctype)}),
               z, x, y, base_ring(x))
   return z
end

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
         (Ref{($etype)}, Ref{($etype)}, Int, Ref{($ctype)}),
               ainv, a, a.prec, base_ring(a))
   return ainv
end

###############################################################################
#
#   Unsafe functions
#
###############################################################################

function zero!(x::($etype))
  ccall(($(flint_fn*"_zero"), libflint), Nothing,
                   (Ref{($etype)}, Ref{($ctype)}), x, base_ring(x))
  x.prec = parent(x).prec_max
  x.val = parent(x).prec_max
  return x
end

function fit!(z::($etype), n::Int)
   ccall(($(flint_fn*"_fit_length"), libflint), Nothing,
         (Ref{($etype)}, Int, Ref{($ctype)}),
         z, n, base_ring(z))
   return nothing
end

function setcoeff!(z::($etype), n::Int, x::fmpz)
   ccall(($(flint_fn*"_set_coeff_fmpz"), libflint), Nothing,
                (Ref{($etype)}, Int, Ref{fmpz}, Ref{($ctype)}),
               z, n, x, base_ring(z))
   return z
end

function setcoeff!(z::($etype), n::Int, x::($btype))
   ccall(($(flint_fn*"_set_coeff"), libflint), Nothing,
                (Ref{($etype)}, Int, Ref{($btype)}, Ref{($ctype)}),
               z, n, x, base_ring(z))
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
               z, a, b, lenz, base_ring(z))
   return z
end

function addeq!(a::($etype), b::($etype))
   lena = pol_length(a)
   lenb = pol_length(b)
   prec = min(a.prec, b.prec)
   val = min(a.val, b.val)
   lena = min(lena, prec - a.val)
   lenb = min(lenb, prec - b.val)
   ctx = base_ring(a)
   if a.val < b.val
      z = ($etype)(base_ring(a))
      z.parent = parent(a)
      lenz = max(lena, lenb + b.val - a.val)
      ccall(($(flint_fn*"_set_trunc"), libflint), Nothing,
            (Ref{($etype)}, Ref{($etype)}, Int, Ref{($ctype)}),
            z, b, max(0, lenz - b.val + a.val), ctx)
      ccall(($(flint_fn*"_shift_left"), libflint), Nothing,
            (Ref{($etype)}, Ref{($etype)}, Int, Ref{($ctype)}),
            z, z, b.val - a.val, ctx)
      ccall(($(flint_fn*"_add_series"), libflint), Nothing,
                (Ref{($etype)}, Ref{($etype)},
                 Ref{($etype)}, Int, Ref{($ctype)}),
               a, a, z, lenz, ctx)
   elseif b.val < a.val
      lenz = max(lena + a.val - b.val, lenb)
      ccall(($(flint_fn*"_truncate"), libflint), Nothing,
            (Ref{($etype)}, Int, Ref{($ctype)}),
            a, max(0, lenz - a.val + b.val), ctx)
      ccall(($(flint_fn*"_shift_left"), libflint), Nothing,
            (Ref{($etype)}, Ref{($etype)}, Int, Ref{($ctype)}),
            a, a, a.val - b.val, ctx)
      ccall(($(flint_fn*"_add_series"), libflint), Nothing,
                (Ref{($etype)}, Ref{($etype)},
                 Ref{($etype)}, Int, Ref{($ctype)}),
               a, a, b, lenz, ctx)
   else
      lenz = max(lena, lenb)
      ccall(($(flint_fn*"_add_series"), libflint), Nothing,
                (Ref{($etype)}, Ref{($etype)},
                 Ref{($etype)}, Int, Ref{($ctype)}),
               a, a, b, lenz, ctx)
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
   ctx = base_ring(a)
   if a.val < b.val
      lenc = max(lena, lenb + b.val - a.val)
      ccall(($(flint_fn*"_set_trunc"), libflint), Nothing,
            (Ref{($etype)}, Ref{($etype)}, Int, Ref{($ctype)}),
            c, b, max(0, lenc - b.val + a.val), ctx)
      ccall(($(flint_fn*"_shift_left"), libflint), Nothing,
            (Ref{($etype)}, Ref{($etype)}, Int, Ref{($ctype)}),
            c, c, b.val - a.val, ctx)
      ccall(($(flint_fn*"_add_series"), libflint), Nothing,
                (Ref{($etype)}, Ref{($etype)},
                 Ref{($etype)}, Int, Ref{($ctype)}),
               c, c, a, lenc, ctx)
   elseif b.val < a.val
      lenc = max(lena + a.val - b.val, lenb)
      ccall(($(flint_fn*"_set_trunc"), libflint), Nothing,
            (Ref{($etype)}, Ref{($etype)}, Int, Ref{($ctype)}),
            c, a, max(0, lenc - a.val + b.val), ctx)
      ccall(($(flint_fn*"_shift_left"), libflint), Nothing,
            (Ref{($etype)}, Ref{($etype)}, Int, Ref{($ctype)}),
            c, c, a.val - b.val, ctx)
      ccall(($(flint_fn*"_add_series"), libflint), Nothing,
                (Ref{($etype)}, Ref{($etype)},
                 Ref{($etype)}, Int, Ref{($ctype)}),
               c, c, b, lenc, ctx)
   else
      lenc = max(lena, lenb)
      ccall(($(flint_fn*"_add_series"), libflint), Nothing,
                (Ref{($etype)}, Ref{($etype)},
                 Ref{($etype)}, Int, Ref{($ctype)}),
               c, a, b, lenc, ctx)
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

promote_rule(::Type{($etype)}, ::Type{($btype)}) = ($etype)

###############################################################################
#
#   Parent object call overload
#
###############################################################################

function (a::($rtype))()
   ctx = base_ring(a)
   z = ($etype)(ctx)
   z.prec = a.prec_max
   z.val = a.prec_max
   z.parent = a
   return z
end

function (a::($rtype))(b::Integer)
   ctx = base_ring(a)
   if b == 0
      z = ($etype)(ctx)
      z.prec = a.prec_max
      z.val = a.prec_max
   else
      z = ($etype)(ctx, [ctx(b)], 1, a.prec_max, 0)
   end
   z.parent = a
   return z
end

function (a::($rtype))(b::fmpz)
   ctx = base_ring(a)
   if iszero(b)
      z = ($etype)(ctx)
      z.prec = a.prec_max
      z.val = a.prec_max
   else
      z = ($etype)(ctx, [ctx(b)], 1, a.prec_max, 0)
   end
   z.parent = a
   return z
end

function (a::($rtype))(b::($btype))
   ctx = base_ring(a)
   if iszero(b)
      z = ($etype)(ctx)
      z.prec = a.prec_max
      z.val = a.prec_max
   else
      z = ($etype)(ctx, [b], 1, a.prec_max, 0)
   end
   z.parent = a
   return z
end

function (a::($rtype))(b::($etype))
   parent(b) != a && error("Unable to coerce power series")
   return b
end

function (a::($rtype))(b::Vector{($btype)}, len::Int, prec::Int, val::Int)
   ctx = base_ring(a)
   z = ($etype)(ctx, b, len, prec, val)
   z.parent = a
   return z
end

end # eval
end # for
