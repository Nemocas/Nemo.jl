###############################################################################
#
#   fq_abs_series.jl: Absolute series over finite fields
#
#   fq_abs_series, fq_nmod_abs_series
#
###############################################################################

export fq_abs_series, FqAbsSeriesRing,
       fq_nmod_abs_series, FqNmodAbsSeriesRing,
       PowerSeriesRing

for (etype, rtype, ctype, btype, flint_fn, flint_tail) in (
   (fq_abs_series, FqAbsSeriesRing, FqFiniteField, fq, "fq_poly", "fq"),
   (fq_nmod_abs_series, FqNmodAbsSeriesRing, FqNmodFiniteField, fq_nmod, "fq_nmod_poly", "fq_nmod"))
@eval begin

###############################################################################
#
#   Data type and parent object methods
#
###############################################################################

function O(a::($etype))
   if iszero(a)
      return deepcopy(a)    # 0 + O(x^n)
   end
   prec = length(a) - 1
   prec < 0 && throw(DomainError(prec, "Valuation must be non-negative"))
   z = ($etype)(base_ring(a), Vector{$(btype)}(undef, 0), 0, prec)
   z.parent = parent(a)
   return z
end

elem_type(::Type{($rtype)}) = ($etype)

parent_type(::Type{($etype)}) = ($rtype)

base_ring(R::($rtype)) = R.base_ring

abs_series_type(::Type{($btype)}) = ($etype)

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

function length(x::($etype))
   return ccall(($(flint_fn*"_length"), libflint), Int,
                (Ref{($etype)}, Ref{($ctype)}), x, base_ring(x))
end

precision(x::($etype)) = x.prec

function coeff(x::($etype), n::Int)
   if n < 0
      return base_ring(x)()
   end
   z = base_ring(x)()
   ccall(($(flint_fn*"_get_coeff"), libflint), Nothing,
         (Ref{($btype)}, Ref{($etype)}, Int, Ref{($ctype)}),
          z, x, n, base_ring(x))
   return z
end

zero(R::($rtype)) = R(0)

one(R::($rtype)) = R(1)

function gen(R::($rtype))
   S = base_ring(R)
   z = ($etype)(S, [S(0), S(1)], 2, max_precision(R))
   z.parent = R
   return z
end

function deepcopy_internal(a::($etype), dict::IdDict)
   z = ($etype)(base_ring(a), a)
   z.prec = a.prec
   z.parent = parent(a)
   return z
end

function isgen(a::($etype))
   return precision(a) == 0 || ccall(($(flint_fn*"_is_gen"), libflint), Bool,
                   (Ref{($etype)}, Ref{($ctype)}), a, base_ring(a))
end

iszero(a::($etype)) = length(a) == 0

isunit(a::($etype)) = valuation(a) == 0 && isunit(coeff(a, 0))

function isone(a::($etype))
   return precision(a) == 0 || ccall(($(flint_fn*"_is_one"), libflint), Bool,
                   (Ref{($etype)}, Ref{($ctype)}), a, base_ring(a))
end

# todo: write an fq_poly_valuation
function valuation(a::($etype))
   for i = 1:length(a)
      if !iszero(coeff(a, i - 1))
         return i - 1
      end
   end
   return precision(a)
end

characteristic(R::($rtype)) = characteristic(base_ring(R))

###############################################################################
#
#   Similar
#
###############################################################################

function similar(f::AbsSeriesElem, R::($ctype), max_prec::Int,
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
   return z
end

###############################################################################
#
#   abs_series constructor
#
###############################################################################

function abs_series(R::($ctype), arr::Vector{T},
                           len::Int, prec::Int, var::String="x";
                            max_precision::Int=prec, cached::Bool=true) where T
   prec < len && error("Precision too small for given data")
   coeffs = T == ($btype) ? arr : map(R, arr)
   coeffs = length(coeffs) == 0 ? ($btype)[] : coeffs
   par = ($rtype)(R, max_precision, Symbol(var), cached)
   z = ($etype)(R, coeffs, len, prec)
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
   return z
end

###############################################################################
#
#   Binary operators
#
###############################################################################

function +(a::($etype), b::($etype))
   check_parent(a, b)
   lena = length(a)
   lenb = length(b)
   prec = min(a.prec, b.prec)
   lena = min(lena, prec)
   lenb = min(lenb, prec)
   lenz = max(lena, lenb)
   z = parent(a)()
   z.prec = prec
   ccall(($(flint_fn*"_add_series"), libflint), Nothing,
         (Ref{($etype)}, Ref{($etype)},
          Ref{($etype)}, Int, Ref{($ctype)}),
               z, a, b, lenz, base_ring(a))
   return z
end

function -(a::($etype), b::($etype))
   check_parent(a, b)
   lena = length(a)
   lenb = length(b)
   prec = min(a.prec, b.prec)
   lena = min(lena, prec)
   lenb = min(lenb, prec)
   lenz = max(lena, lenb)
   z = parent(a)()
   z.prec = prec
   ccall(($(flint_fn*"_sub_series"), libflint), Nothing,
         (Ref{($etype)}, Ref{($etype)},
          Ref{($etype)}, Int, Ref{($ctype)}),
               z, a, b, lenz, base_ring(a))
   return z
end

function *(a::($etype), b::($etype))
   check_parent(a, b)
   lena = length(a)
   lenb = length(b)
   aval = valuation(a)
   bval = valuation(b)
   prec = min(a.prec + bval, b.prec + aval)
   prec = min(prec, max_precision(parent(a)))
   lena = min(lena, prec)
   lenb = min(lenb, prec)
   z = parent(a)()
   z.prec = prec
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
   ccall(($(flint_fn*"_scalar_mul_"*flint_tail), libflint), Nothing,
         (Ref{($etype)}, Ref{($etype)}, Ref{($btype)}, Ref{($ctype)}),
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
   xlen = length(x)
   z = parent(x)()
   z.prec = x.prec + len
   z.prec = min(z.prec, max_precision(parent(x)))
   zlen = min(z.prec, xlen + len)
   ccall(($(flint_fn*"_shift_left"), libflint), Nothing,
         (Ref{($etype)}, Ref{($etype)}, Int, Ref{($ctype)}),
               z, x, len, base_ring(x))
   ccall(($(flint_fn*"_set_trunc"), libflint), Nothing,
                (Ref{($etype)}, Ref{($etype)}, Int, Ref{($ctype)}),
               z, z, zlen, base_ring(x))
   return z
end

function shift_right(x::($etype), len::Int)
   len < 0 && throw(DomainError(len, "Shift must be non-negative"))
   xlen = length(x)
   z = parent(x)()
   if len >= xlen
      z.prec = max(0, x.prec - len)
   else
      z.prec = x.prec - len
      ccall(($(flint_fn*"_shift_right"), libflint), Nothing,
            (Ref{($etype)}, Ref{($etype)}, Int, Ref{($ctype)}),
               z, x, len, base_ring(x))
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
   if x.prec <= prec
      return x
   end
   z = parent(x)()
   z.prec = prec
   ccall(($(flint_fn*"_set_trunc"), libflint), Nothing,
         (Ref{($etype)}, Ref{($etype)}, Int, Ref{($ctype)}),
               z, x, prec, base_ring(x))
   return z
end

###############################################################################
#
#   Powering
#
###############################################################################

function ^(a::($etype), b::Int)
   b < 0 && throw(DomainError(b, "Exponent must be non-negative"))
   if precision(a) > 0 && isgen(a) && b > 0
      return shift_left(a, b - 1)
   elseif length(a) == 1
      return parent(a)([coeff(a, 0)^b], 1, a.prec)
   elseif b == 0
      z = one(parent(a))
      z = set_precision!(z, precision(a))
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

function ==(x::($etype), y::($etype))
   check_parent(x, y)
   prec = min(x.prec, y.prec)
   n = max(length(x), length(y))
   n = min(n, prec)
   return Bool(ccall(($(flint_fn*"_equal_trunc"), libflint), Cint,
             (Ref{($etype)}, Ref{($etype)}, Int, Ref{($ctype)}),
               x, y, n, base_ring(x)))
end

function isequal(x::($etype), y::($etype))
   if parent(x) != parent(y)
      return false
   end
   if x.prec != y.prec || length(x) != length(y)
      return false
   end
   return Bool(ccall(($(flint_fn*"_equal"), libflint), Cint,
             (Ref{($etype)}, Ref{($etype)}, Int, Ref{($ctype)}),
               x, y, length(x), base_ring(x)))
end

###############################################################################
#
#   Ad hoc comparisons
#
###############################################################################

function ==(x::($etype), y::($btype))
   if length(x) > 1
      return false
   elseif length(x) == 1
      z = base_ring(x)()
      ccall(($(flint_fn*"_get_coeff"), libflint), Nothing,
            (Ref{($btype)}, Ref{($etype)}, Int, Ref{($ctype)}),
             z, x, 0, base_ring(x))
      return z == y
   else
      return precision(x) == 0 || iszero(y)
   end
end

==(x::($btype), y::($etype)) = y == x

function ==(x::($etype), y::fmpz)
   if length(x) > 1
      return false
   elseif length(x) == 1
      z = base_ring(x)()
      ccall(($(flint_fn*"_get_coeff"), libflint), Nothing,
            (Ref{($btype)}, Ref{($etype)}, Int, Ref{($ctype)}),
             z, x, 0, base_ring(x))
      return z == y
   else
      return precision(x) == 0 || iszero(y)
   end
end

==(x::fmpz, y::($etype)) = y == x

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
   ccall(($(flint_fn*"_div_series"), libflint), Nothing,
         (Ref{($etype)}, Ref{($etype)},
          Ref{($etype)}, Int, Ref{($ctype)}),
               z, x, y, prec, base_ring(x))
   return z
end

###############################################################################
#
#   Ad hoc exact division
#
###############################################################################

function divexact(x::($etype), y::($btype))
   iszero(y) && throw(DivideError())
   z = parent(x)()
   z.prec = x.prec
   ccall(($(flint_fn*"_scalar_div_"*flint_tail), libflint), Nothing,
         (Ref{($etype)}, Ref{($etype)}, Ref{($btype)}, Ref{($ctype)}),
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

function zero!(z::($etype))
   ccall(($(flint_fn*"_zero"), libflint), Nothing,
                (Ref{($etype)}, Ref{($ctype)}), z, base_ring(z))
   z.prec = parent(z).prec_max
   return z
end

function fit!(z::($etype), n::Int)
   ccall(($(flint_fn*"_fit_length"), libflint), Nothing,
         (Ref{($etype)}, Int, Ref{($ctype)}),
         z, n, base_ring(z))
   return nothing
end

function setcoeff!(z::($etype), n::Int, x::($btype))
   ccall(($(flint_fn*"_set_coeff"), libflint), Nothing,
                (Ref{($etype)}, Int, Ref{($btype)}, Ref{($ctype)}),
               z, n, x, base_ring(z))
   return z
end

function mul!(z::($etype), a::($etype), b::($etype))
   lena = length(a)
   lenb = length(b)
   aval = valuation(a)
   bval = valuation(b)
   prec = min(a.prec + bval, b.prec + aval)
   prec = min(prec, max_precision(parent(z)))
   lena = min(lena, prec)
   lenb = min(lenb, prec)
   lenz = min(lena + lenb - 1, prec)
   if lenz < 0
      lenz = 0
   end
   z.prec = prec
   ccall(($(flint_fn*"_mullow"), libflint), Nothing,
         (Ref{($etype)}, Ref{($etype)},
          Ref{($etype)}, Int, Ref{($ctype)}),
               z, a, b, lenz, base_ring(z))
   return z
end

function addeq!(a::($etype), b::($etype))
   lena = length(a)
   lenb = length(b)
   prec = min(a.prec, b.prec)
   lena = min(lena, prec)
   lenb = min(lenb, prec)
   lenz = max(lena, lenb)
   a.prec = prec
   ccall(($(flint_fn*"_add_series"), libflint), Nothing,
         (Ref{($etype)}, Ref{($etype)},
          Ref{($etype)}, Int, Ref{($ctype)}),
               a, a, b, lenz, base_ring(a))
   return a
end

function add!(c::($etype), a::($etype), b::($etype))
   lena = length(a)
   lenb = length(b)
   prec = min(a.prec, b.prec)
   lena = min(lena, prec)
   lenb = min(lenb, prec)
   lenc = max(lena, lenb)
   c.prec = prec
   ccall(($(flint_fn*"_add_series"), libflint), Nothing,
         (Ref{($etype)}, Ref{($etype)},
          Ref{($etype)}, Int, Ref{($ctype)}),
               c, a, b, lenc, base_ring(a))
   return c
end

###############################################################################
#
#   Promotion rules
#
###############################################################################

promote_rule(::Type{($etype)}, ::Type{T}) where {T <: Integer} = ($etype)

promote_rule(::Type{($etype)}, ::Type{$(btype)}) = ($etype)

promote_rule(::Type{($etype)}, ::Type{fmpz}) = ($etype)

###############################################################################
#
#   Parent object call overload
#
###############################################################################

function (a::($rtype))()
   ctx = base_ring(a)
   z = ($etype)(ctx)
   z.prec = a.prec_max
   z.parent = a
   return z
end

function (a::($rtype))(b::Integer)
   ctx = base_ring(a)
   if b == 0
      z = ($etype)(ctx)
      z.prec = a.prec_max
   else
      z = ($etype)(ctx, [base_ring(a)(b)], 1, a.prec_max)
   end
   z.parent = a
   return z
end

function (a::($rtype))(b::fmpz)
   ctx = base_ring(a)
   if iszero(b)
      z = ($etype)(ctx)
      z.prec = a.prec_max
   else
      z = ($etype)(ctx, [base_ring(a)(b)], 1, a.prec_max)
   end
   z.parent = a
   return z
end

function (a::($rtype))(b::($btype))
   ctx = base_ring(a)
   if iszero(b)
      z = ($etype)(ctx)
      z.prec = a.prec_max
   else
      z = ($etype)(ctx, [b], 1, a.prec_max)
   end
   z.parent = a
   return z
end

function (a::($rtype))(b::($etype))
   parent(b) != a && error("Unable to coerce power series")
   return b
end

function (a::($rtype))(b::Vector{$(btype)}, len::Int, prec::Int)
   ctx = base_ring(a)
   z = ($etype)(ctx, b, len, prec)
   z.parent = a
   return z
end

end # eval
end # for

###############################################################################
#
#   PowerSeriesRing constructor
#
###############################################################################

function PowerSeriesRing(R::FqFiniteField, prec::Int, s::Symbol; model=:capped_relative, cached = true)
   if model == :capped_relative
      parent_obj = FqRelSeriesRing(R, prec, s, cached)
   elseif model == :capped_absolute
      parent_obj = FqAbsSeriesRing(R, prec, s, cached)
   else
      error("Unknown model")
   end

   return parent_obj, gen(parent_obj)
end

function PowerSeriesRing(R::FqFiniteField, prec::Int, s::AbstractString; model=:capped_relative, cached = true)
   return PowerSeriesRing(R, prec, Symbol(s); model=model, cached=cached)
end

function AbsSeriesRing(R::FqFiniteField, prec::Int)
   return FqAbsSeriesRing(R, prec, :x, false)
end

function RelSeriesRing(R::FqFiniteField, prec::Int)
   return FqRelSeriesRing(R, prec, :x, false)
end

function PowerSeriesRing(R::FqNmodFiniteField, prec::Int, s::Symbol; model=:capped_relative, cached = true)
   if model == :capped_relative
      parent_obj = FqNmodRelSeriesRing(R, prec, s, cached)
   elseif model == :capped_absolute
      parent_obj = FqNmodAbsSeriesRing(R, prec, s, cached)
   else
      error("Unknown model")
   end

   return parent_obj, gen(parent_obj)
end

function PowerSeriesRing(R::FqNmodFiniteField, prec::Int, s::AbstractString; model=:capped_relative, cached = true)
   return PowerSeriesRing(R, prec, Symbol(s); model=model, cached=cached)
end

function AbsSeriesRing(R::FqNmodFiniteField, prec::Int)
   return FqNmodAbsSeriesRing(R, prec, :x, false)
end

function RelSeriesRing(R::FqNmodFiniteField, prec::Int)
   return FqNmodRelSeriesRing(R, prec, :x, false)
end
