###############################################################################
#
#   fmpz_mod_abs_series.jl: Absolute series using fmpz_mod_poly
#
#   fmpz_mod_abs_series, gfp_fmpz_abs_series
#
###############################################################################

export fmpz_mod_abs_series, FmpzModAbsSeriesRing,
       gfp_fmpz_abs_series, GFPFmpzAbsSeriesRing

for (etype, rtype, ctype, mtype, brtype, flint_fn) in (
   (fmpz_mod_abs_series, FmpzModAbsSeriesRing, fmpz_mod_ctx_struct, fmpz_mod, FmpzModRing, "fmpz_mod_poly"),
   (gfp_fmpz_abs_series, GFPFmpzAbsSeriesRing, fmpz_mod_ctx_struct, gfp_fmpz_elem, GaloisFmpzField, "fmpz_mod_poly"))
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
   z = ($etype)(base_ring(a), Vector{fmpz}(undef, 0), 0, prec)
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
      c = fmpz()
      ccall(($(flint_fn*"_get_coeff_fmpz"), libflint), Nothing,
            (Ref{fmpz}, Ref{($etype)}, Int,
             Ref{($ctype)}),
            c, a, len - 1, p)
   end
   while len > 0 && iszero(c)
      len -= 1
      if len > 0
         ccall(($(flint_fn*"_get_coeff_fmpz"), libflint), Nothing,
               (Ref{fmpz}, Ref{($etype)}, Int,
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
   z = fmpz()
   ccall(($(flint_fn*"_get_coeff_fmpz"), libflint), Nothing,
         (Ref{fmpz}, Ref{($etype)}, Int, Ref{($ctype)}),
         z, x, n, x.parent.base_ring.ninv)
   return R(z)
end

zero(R::($rtype)) = R(0)

one(R::($rtype)) = R(1)

function gen(R::($rtype))
   z = ($etype)(base_ring(R), [fmpz(0), fmpz(1)], 2, max_precision(R))
   z.parent = R
   return z
end

function deepcopy_internal(a::($etype), dict::IdDict)
   z = ($etype)(a)
   z.prec = a.prec
   z.parent = parent(a)
   return z
end

function isgen(a::($etype))
   return precision(a) == 0 ||
          Bool(ccall(($(flint_fn*"_is_gen"), libflint), Cint,
                     (Ref{($etype)}, Ref{($ctype)}),
                     a, a.parent.base_ring.ninv))
end

iszero(a::($etype)) = length(a) == 0

isunit(a::($etype)) = valuation(a) == 0 && isunit(coeff(a, 0))

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
                                 var::Symbol=var(parent(f)); cached::Bool=true)
   par = ($rtype)(R, max_prec, var, cached)
   z = ($etype)(R)
   z.parent = par
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
         (Ref{($etype)}, Ref{($etype)}, Ref{fmpz},
          Ref{($ctype)}),
         z, y, x.data, y.parent.base_ring.ninv)
   return z
end

*(x::($etype), y::fmpz) = y * x

function *(x::fmpz, y::($etype))
   z = parent(y)()
   z.prec = y.prec
   ccall(($(flint_fn*"_scalar_mul_fmpz"), libflint), Nothing,
         (Ref{($etype)}, Ref{($etype)}, Ref{fmpz},
          Ref{($ctype)}),
         z, y, x, y.parent.base_ring.ninv)
   return z
end

*(x::Integer, y::($etype)) = fmpz(x)*y

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
   if precision(a) > 0 && isgen(a) && b > 0
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
                     (Ref{($etype)}, Ref{($etype)}, Int,
                      Ref{($ctype)}),
                     x, y, length(x), x.parent.base_ring.ninv))
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
      z = fmpz()
      ccall(($(flint_fn*"_get_coeff_fmpz"), libflint), Nothing,
            (Ref{fmpz}, Ref{($etype)}, Int,
             Ref{($ctype)}),
            z, x, 0, x.parent.base_ring.ninv)
      return ccall((:fmpz_equal, libflint), Bool,
               (Ref{fmpz}, Ref{fmpz}), z, y)
   else
      return precision(x) == 0 || iszero(y)
   end
end

==(x::$(mtype), y::($etype)) = y == x

function ==(x::($etype), y::fmpz)
   if length(x) > 1
      return false
   elseif length(x) == 1
      z = fmpz()
      r = mod(y, modulus(x))
      ccall(($(flint_fn*"_get_coeff_fmpz"), libflint), Nothing,
            (Ref{fmpz}, Ref{($etype)}, Int, Ref{($ctype)}),
            z, x, 0, x.parent.base_ring.ninv)
      return Bool(ccall((:fmpz_equal, libflint), Cint,
                        (Ref{fmpz}, Ref{fmpz}),
                        z, r))
   else
      r = mod(y, modulus(x))
      return precision(x) == 0 || iszero(r)
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
         z, x, y, prec, x.parent.base_ring.ninv)
   return z
end

###############################################################################
#
#   Ad hoc exact division
#
###############################################################################

function divexact(x::($etype), y::$(mtype))
   iszero(y) && throw(DivideError())
   z = parent(x)()
   z.prec = x.prec
   ccall(($(flint_fn*"_scalar_div_fmpz"), libflint), Nothing,
         (Ref{($etype)}, Ref{($etype)}, Ref{fmpz},
          Ref{($ctype)}),
         z, x, y, x.parent.base_ring.ninv)
   return z
end

function divexact(x::($etype), y::fmpz)
   iszero(y) && throw(DivideError())
   z = parent(x)()
   z.prec = x.prec
   r = mod(y, modulus(x))
   ccall(($(flint_fn*"_scalar_div_fmpz"), libflint), Nothing,
         (Ref{($etype)}, Ref{($etype)}, Ref{fmpz},
          Ref{($ctype)}),
         z, x, y, x.parent.base_ring.ninv)
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

function setcoeff!(z::($etype), n::Int, x::fmpz)
   ccall(($(flint_fn*"_set_coeff_fmpz"), libflint), Nothing,
         (Ref{($etype)}, Int, Ref{fmpz}, Ref{($ctype)}),
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

###############################################################################
#
#   Promotion rules
#
###############################################################################

promote_rule(::Type{($etype)}, ::Type{T}) where {T <: Integer} = ($etype)

promote_rule(::Type{($etype)}, ::Type{fmpz}) = ($etype)

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
      z = ($etype)(m, [fmpz(b)], 1, a.prec_max)
   end
   z.parent = a
   return z
end

function (a::($rtype))(b::fmpz)
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

function (a::($rtype))(b::Array{fmpz, 1}, len::Int, prec::Int)
   m = base_ring(a)
   z = ($etype)(m, b, len, prec)
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

function PowerSeriesRing(R::FmpzModRing, prec::Int, s::Symbol; model=:capped_relative, cached = true)
   if model == :capped_relative
      parent_obj = FmpzModRelSeriesRing(R, prec, s, cached)
   elseif model == :capped_absolute
      parent_obj = FmpzModAbsSeriesRing(R, prec, s, cached)
   else
      error("Unknown model")
   end
   return parent_obj, gen(parent_obj)
end

function PowerSeriesRing(R::FmpzModRing, prec::Int, s::AbstractString; model=:capped_relative, cached = true)
   return PowerSeriesRing(R, prec, Symbol(s); model=model, cached=cached)
end

function AbsSeriesRing(R::FmpzModRing, prec::Int)
   return FmpzModAbsSeriesRing(R, prec, :x, false)
end

function RelSeriesRing(R::FmpzModRing, prec::Int)
   return FmpzModRelSeriesRing(R, prec, :x, false)
end

function PowerSeriesRing(R::GaloisFmpzField, prec::Int, s::Symbol; model=:capped_relative, cached = true)
   if model == :capped_relative
      parent_obj = GFPFmpzRelSeriesRing(R, prec, s, cached)
   elseif model == :capped_absolute
      parent_obj = GFPFmpzAbsSeriesRing(R, prec, s, cached)
   else
      error("Unknown model")
   end
   return parent_obj, gen(parent_obj)
end

function PowerSeriesRing(R::GaloisFmpzField, prec::Int, s::AbstractString; model=:capped_relative, cached = true)
   return PowerSeriesRing(R, prec, Symbol(s); model=model, cached=cached)
end

function AbsSeriesRing(R::GaloisFmpzField, prec::Int)
   return GFPFmpzAbsSeriesRing(R, prec, :x, false)
end

function RelSeriesRing(R::GaloisFmpzField, prec::Int)
   return GFPFmpzRelSeriesRing(R, prec, :x, false)
end
