###############################################################################
#
#   ZZModAbsPowerSeriesRingElem.jl: Absolute series using ZZModPolyRingElem
#
#   ZZModAbsPowerSeriesRingElem, FpAbsPowerSeriesRingElem
#
###############################################################################

export ZZModAbsPowerSeriesRingElem, ZZModAbsPowerSeriesRing,
       FpAbsPowerSeriesRingElem, FpAbsPowerSeriesRing

for (etype, rtype, ctype, mtype, brtype, flint_fn) in (
   (ZZModAbsPowerSeriesRingElem, ZZModAbsPowerSeriesRing, fmpz_mod_ctx_struct, ZZModRingElem, ZZModRing, "fmpz_mod_poly"),
   (FpAbsPowerSeriesRingElem, FpAbsPowerSeriesRing, fmpz_mod_ctx_struct, FpFieldElem, FpField, "fmpz_mod_poly"))
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
   prec < 0 && throw(DomainError(prec, "Precision must be non-negative"))
   z = ($etype)(base_ring(a), Vector{ZZRingElem}(undef, 0), 0, prec)
   z.parent = parent(a)
   return z
end

elem_type(::Type{($rtype)}) = ($etype)

parent_type(::Type{($etype)}) = ($rtype)

base_ring(R::($rtype)) = R.base_ring

abs_series_type(::Type{($mtype)}) = ($etype)

var(a::($rtype)) = a.S

###############################################################################
#
#   Basic manipulation
#
###############################################################################

max_precision(R::($rtype)) = R.prec_max

function normalise(a::($etype), len::Int)
   p = a.parent.base_ring.ninv
   if len > 0
      c = ZZRingElem()
      ccall(($(flint_fn*"_get_coeff_fmpz"), libflint), Nothing,
            (Ref{ZZRingElem}, Ref{($etype)}, Int,
             Ref{($ctype)}),
            c, a, len - 1, p)
   end
   while len > 0 && iszero(c)
      len -= 1
      if len > 0
         ccall(($(flint_fn*"_get_coeff_fmpz"), libflint), Nothing,
               (Ref{ZZRingElem}, Ref{($etype)}, Int,
                Ref{($ctype)}),
               c, a, len - 1, p)
      end
   end
   return len
end

function length(x::($etype))
   return x.length
#   return ccall(($(flint_fn*"_length"), libflint), Int,
#                (Ref{($etype)}, Ref{($ctype)}),
#                x, x.parent.base_ring.ninv)
end

precision(x::($etype)) = x.prec

function coeff(x::($etype), n::Int)
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
   z = ($etype)(base_ring(R), [ZZRingElem(0), ZZRingElem(1)], 2, max_precision(R))
   z.parent = R
   return z
end

function deepcopy_internal(a::($etype), dict::IdDict)
   z = ($etype)(a)
   z.prec = a.prec
   z.parent = parent(a)
   return z
end

function is_gen(a::($etype))
   return precision(a) == 0 ||
          Bool(ccall(($(flint_fn*"_is_gen"), libflint), Cint,
                     (Ref{($etype)}, Ref{($ctype)}),
                     a, a.parent.base_ring.ninv))
end

iszero(a::($etype)) = length(a) == 0

is_unit(a::($etype)) = valuation(a) == 0 && is_unit(coeff(a, 0))

function isone(a::($etype))
   return precision(a) == 0 ||
          Bool(ccall(($(flint_fn*"_is_one"), libflint), Cint,
                     (Ref{($etype)}, Ref{($ctype)}),
                     a, a.parent.base_ring.ninv))
end

# todo: write an fmpz_mod_poly_valuation
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

function similar(f::AbsSeriesElem, R::($brtype), max_prec::Int,
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

function abs_series(R::($brtype), arr::Vector{T},
                           len::Int, prec::Int, var::String="x";
                            max_precision::Int=prec, cached::Bool=true) where T
   prec < len && error("Precision too small for given data")
   coeffs = T == ($mtype) ? arr : map(R, arr)
   coeffs = length(coeffs) == 0 ? ($mtype)[] : coeffs
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
         (Ref{($etype)}, Ref{($etype)},
          Ref{($ctype)}),
         z, x, x.parent.base_ring.ninv)
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
         z, a, b, lenz, a.parent.base_ring.ninv)
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
         z, a, b, lenz, a.parent.base_ring.ninv)
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
         z, a, b, lenz, a.parent.base_ring.ninv)
   return z
end

###############################################################################
#
#   Ad hoc binary operators
#
###############################################################################

function *(x::$(mtype), y::($etype))
   z = parent(y)()
   z.prec = y.prec
   ccall(($(flint_fn*"_scalar_mul_fmpz"), libflint), Nothing,
         (Ref{($etype)}, Ref{($etype)}, Ref{ZZRingElem},
          Ref{($ctype)}),
         z, y, x.data, y.parent.base_ring.ninv)
   return z
end

*(x::($etype), y::ZZRingElem) = y * x

function *(x::ZZRingElem, y::($etype))
   z = parent(y)()
   z.prec = y.prec
   ccall(($(flint_fn*"_scalar_mul_fmpz"), libflint), Nothing,
         (Ref{($etype)}, Ref{($etype)}, Ref{ZZRingElem},
          Ref{($ctype)}),
         z, y, x, y.parent.base_ring.ninv)
   return z
end

*(x::Integer, y::($etype)) = ZZRingElem(x)*y

*(x::($etype), y::Integer) = y * x

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
   p = x.parent.base_ring.ninv
   ccall(($(flint_fn*"_shift_left"), libflint), Nothing,
         (Ref{($etype)}, Ref{($etype)}, Int,
          Ref{($ctype)}),
         z, x, len, p)
   ccall(($(flint_fn*"_set_trunc"), libflint), Nothing,
         (Ref{($etype)}, Ref{($etype)}, Int,
          Ref{($ctype)}),
         z, z, zlen, p)
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
            (Ref{($etype)}, Ref{($etype)}, Int,
             Ref{($ctype)}),
            z, x, len, x.parent.base_ring.ninv)
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
         (Ref{($etype)}, Ref{($etype)}, Int,
          Ref{($ctype)}),
         z, x, prec, x.parent.base_ring.ninv)
   return z
end

###############################################################################
#
#   Powering
#
###############################################################################

function ^(a::($etype), b::Int)
   b < 0 && throw(DomainError(b, "Exponent must be non-negative"))
   if precision(a) > 0 && is_gen(a) && b > 0
      return shift_left(a, b - 1)
   elseif length(a) == 1
      return parent(a)([coeff(a, 0)^b], 1, a.prec)
   elseif b == 0
      z = one(parent(a))
      z = set_precision!(z, precision(a))
      return z
   else
      z = parent(a)()
      z.prec = a.prec + (b - 1)*valuation(a)
      z.prec = min(z.prec, max_precision(parent(a)))
      ccall(($(flint_fn*"_pow_trunc"), libflint), Nothing,
            (Ref{($etype)}, Ref{($etype)}, UInt, Int,
             Ref{($ctype)}),
            z, a, b, z.prec, a.parent.base_ring.ninv)
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
                     (Ref{($etype)}, Ref{($etype)}, Int,
                      Ref{($ctype)}),
                     x, y, n, x.parent.base_ring.ninv))
end

function isequal(x::($etype), y::($etype))
   if parent(x) != parent(y)
      return false
   end
   if x.prec != y.prec || length(x) != length(y)
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

function ==(x::($etype), y::$(mtype))
   if length(x) > 1
      return false
   elseif length(x) == 1
      z = ZZRingElem()
      ccall(($(flint_fn*"_get_coeff_fmpz"), libflint), Nothing,
            (Ref{ZZRingElem}, Ref{($etype)}, Int,
             Ref{($ctype)}),
            z, x, 0, x.parent.base_ring.ninv)
      return ccall((:fmpz_equal, libflint), Bool,
               (Ref{ZZRingElem}, Ref{ZZRingElem}), z, y)
   else
      return precision(x) == 0 || iszero(y)
   end
end

==(x::$(mtype), y::($etype)) = y == x

function ==(x::($etype), y::ZZRingElem)
   if length(x) > 1
      return false
   elseif length(x) == 1
      z = ZZRingElem()
      r = mod(y, modulus(x))
      ccall(($(flint_fn*"_get_coeff_fmpz"), libflint), Nothing,
            (Ref{ZZRingElem}, Ref{($etype)}, Int, Ref{($ctype)}),
            z, x, 0, x.parent.base_ring.ninv)
      return Bool(ccall((:fmpz_equal, libflint), Cint,
                        (Ref{ZZRingElem}, Ref{ZZRingElem}),
                        z, r))
   else
      r = mod(y, modulus(x))
      return precision(x) == 0 || iszero(r)
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
   v2 = valuation(y)
   v1 = valuation(x)
   if v2 != 0
      if v1 >= v2
         x = shift_right(x, v2)
         y = shift_right(y, v2)
      end
   end
   check && !is_unit(y) && error("Unable to invert power series")
   prec = min(x.prec, y.prec - v2 + v1)
   z = parent(x)()
   z.prec = prec
   ccall(($(flint_fn*"_div_series"), libflint), Nothing,
         (Ref{($etype)}, Ref{($etype)},
          Ref{($etype)}, Int, Ref{($ctype)}),
         z, x, y, prec, x.parent.base_ring.ninv)
   return z
end

###############################################################################
#
#   Ad hoc exact division
#
###############################################################################

function divexact(x::($etype), y::$(mtype); check::Bool=true)
   iszero(y) && throw(DivideError())
   z = parent(x)()
   z.prec = x.prec
   ccall(($(flint_fn*"_scalar_div_fmpz"), libflint), Nothing,
         (Ref{($etype)}, Ref{($etype)}, Ref{ZZRingElem},
          Ref{($ctype)}),
         z, x, data(y), x.parent.base_ring.ninv)
   return z
end

function divexact(x::($etype), y::ZZRingElem; check::Bool=true)
   iszero(y) && throw(DivideError())
   z = parent(x)()
   z.prec = x.prec
   r = mod(y, modulus(x))
   ccall(($(flint_fn*"_scalar_div_fmpz"), libflint), Nothing,
         (Ref{($etype)}, Ref{($etype)}, Ref{ZZRingElem},
          Ref{($ctype)}),
         z, x, y, x.parent.base_ring.ninv)
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
   ccall(($(flint_fn*"_inv_series"), libflint), Nothing,
         (Ref{($etype)}, Ref{($etype)}, Int,
          Ref{($ctype)}),
         ainv, a, a.prec, a.parent.base_ring.ninv)
   return ainv
end

###############################################################################
#
#   Unsafe functions
#
###############################################################################

function zero!(z::($etype))
   ccall(($(flint_fn*"_zero"), libflint), Nothing,
         (Ref{($etype)}, Ref{($ctype)}),
         z, z.parent.base_ring.ninv)
   z.prec = parent(z).prec_max
   return z
end

function fit!(z::($etype), n::Int)
   ccall(($(flint_fn*"_fit_length"), libflint), Nothing,
         (Ref{($etype)}, Int, Ref{($ctype)}),
	 z, n, z.parent.base_ring.ninv)
   return nothing
end

function setcoeff!(z::($etype), n::Int, x::ZZRingElem)
   ccall(($(flint_fn*"_set_coeff_fmpz"), libflint), Nothing,
         (Ref{($etype)}, Int, Ref{ZZRingElem}, Ref{($ctype)}),
         z, n, x, z.parent.base_ring.ninv)
   return z
end

function setcoeff!(z::($etype), n::Int, x::$(mtype))
   return setcoeff!(z, n, data(x))
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
         z, a, b, lenz, a.parent.base_ring.ninv)
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
         a, a, b, lenz, a.parent.base_ring.ninv)
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
         c, a, b, lenc, a.parent.base_ring.ninv)
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

promote_rule(::Type{($etype)}, ::Type{$(mtype)}) = ($etype)

###############################################################################
#
#   Parent object call overload
#
###############################################################################

function (a::($rtype))()
   m = base_ring(a)
   z = ($etype)(m)
   z.prec = a.prec_max
   z.parent = a
   return z
end

function (a::($rtype))(b::Integer)
   m = base_ring(a)
   if b == 0
      z = ($etype)(m)
      z.prec = a.prec_max
   else
      z = ($etype)(m, [ZZRingElem(b)], 1, a.prec_max)
   end
   z.parent = a
   return z
end

function (a::($rtype))(b::ZZRingElem)
   m = base_ring(a)
   if iszero(b)
      z = ($etype)(m)
      z.prec = a.prec_max
   else
      z = ($etype)(m, [b], 1, a.prec_max)
   end
   z.parent = a
   return z
end

function (a::($rtype))(b::$(mtype))
   m = base_ring(a)
   if iszero(b)
      z = ($etype)(m)
      z.prec = a.prec_max
   else
      z = ($etype)(m, [b], 1, a.prec_max)
   end
   z.parent = a
   return z
end

function (a::($rtype))(b::($etype))
   parent(b) != a && error("Unable to coerce power series")
   return b
end

function (a::($rtype))(b::Vector{ZZRingElem}, len::Int, prec::Int)
   m = base_ring(a)
   z = ($etype)(m, b, len, prec)
   z.parent = a
   return z
end

function (a::($rtype))(b::Vector{($mtype)}, len::Int, prec::Int)
   if length(b) > 0
      (base_ring(a) != parent(b[1])) && error("Wrong parents")
   end
   z = ($etype)(base_ring(a), b, len, prec)
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

function sqrt_classical_char2(a::FpAbsPowerSeriesRingElem; check::Bool=true)
   S = parent(a)
   asqrt = S()
   prec = div(precision(a) + 1, 2)
   asqrt = set_precision!(asqrt, prec)
   if check
     for i = 1:2:precision(a) - 1 # series must have even exponents
       if !iszero(coeff(a, i))
         return false, S()
       end
     end
   end
   for i = 0:prec - 1
     asqrt = setcoeff!(asqrt, i, coeff(a, 2*i))
   end
   return true, asqrt
end
 
function sqrt_classical(a::FpAbsPowerSeriesRingElem; check::Bool=true)
   R = base_ring(a)
   S = parent(a)
   if characteristic(R) == 2
      return sqrt_classical_char2(a; check=check)
   end
   v = valuation(a)
   z = S()
   z.prec = a.prec - div(v, 2)
   if iszero(a)
      return true, z
   end
   if check && !iseven(v)
      return false, S()
   end
   a = shift_right(a, v)
   c = coeff(a, 0)
   if check
      flag, s = issquare_with_sqrt(c)
      if !flag
         return false, S()
      end
   else
      s = sqrt(c; check=check)
   end
   a = divexact(a, c)
   z.prec = a.prec - div(v, 2)
   ccall((:fmpz_mod_poly_sqrt_series, libflint), Nothing,
         (Ref{FpAbsPowerSeriesRingElem}, Ref{FpAbsPowerSeriesRingElem},
              Int, Ref{fmpz_mod_ctx_struct}),
                z, a, a.prec, a.parent.base_ring.ninv)
   if !isone(s)
      z *= s
   end
   if !iszero(v)
      z = shift_left(z, div(v, 2))
   end
   return true, z
end
 
@doc Markdown.doc"""
   sqrt(a::FpAbsPowerSeriesRingElem; check::Bool=true)
 
Return the power series square root of $a$.
"""
function Base.sqrt(a::FpAbsPowerSeriesRingElem; check::Bool=true)
   flag, s = sqrt_classical(a; check=check)
   check && !flag && error("Not a square")
   return s
end
  
function issquare(a::FpAbsPowerSeriesRingElem)
   flag, s = sqrt_classical(a; check=true)
   return flag
end
 
function issquare_with_sqrt(a::FpAbsPowerSeriesRingElem)
   return sqrt_classical(a; check=true)
end

###############################################################################
#
#   PowerSeriesRing constructor
#
###############################################################################

function PowerSeriesRing(R::ZZModRing, prec::Int, s::Symbol; model=:capped_relative, cached = true)
   if model == :capped_relative
      parent_obj = ZZModRelPowerSeriesRing(R, prec, s, cached)
   elseif model == :capped_absolute
      parent_obj = ZZModAbsPowerSeriesRing(R, prec, s, cached)
   else
      error("Unknown model")
   end
   return parent_obj, gen(parent_obj)
end

function PowerSeriesRing(R::ZZModRing, prec::Int, s::AbstractString; model=:capped_relative, cached = true)
   return PowerSeriesRing(R, prec, Symbol(s); model=model, cached=cached)
end

function AbsSeriesRing(R::ZZModRing, prec::Int)
   return ZZModAbsPowerSeriesRing(R, prec, :x, false)
end

function RelSeriesRing(R::ZZModRing, prec::Int)
   return ZZModRelPowerSeriesRing(R, prec, :x, false)
end

function PowerSeriesRing(R::FpField, prec::Int, s::Symbol; model=:capped_relative, cached = true)
   if model == :capped_relative
      parent_obj = FpRelPowerSeriesRing(R, prec, s, cached)
   elseif model == :capped_absolute
      parent_obj = FpAbsPowerSeriesRing(R, prec, s, cached)
   else
      error("Unknown model")
   end
   return parent_obj, gen(parent_obj)
end

function PowerSeriesRing(R::FpField, prec::Int, s::AbstractString; model=:capped_relative, cached = true)
   return PowerSeriesRing(R, prec, Symbol(s); model=model, cached=cached)
end

function AbsSeriesRing(R::FpField, prec::Int)
   return FpAbsPowerSeriesRing(R, prec, :x, false)
end

function RelSeriesRing(R::FpField, prec::Int)
   return FpRelPowerSeriesRing(R, prec, :x, false)
end
