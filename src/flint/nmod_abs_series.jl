###############################################################################
#
#   nmod_abs_series.jl: Absolute series using nmod_poly
#
#   nmod_abs_series, gfp_abs_series
#
###############################################################################

export nmod_abs_series, NmodAbsSeriesRing,
       gfp_abs_series, GFPAbsSeriesRing

for (etype, rtype, mtype, brtype, flint_fn) in (
   (nmod_abs_series, NmodAbsSeriesRing, nmod, NmodRing, "nmod_poly"),
   (gfp_abs_series, GFPAbsSeriesRing, gfp_elem, GaloisField, "nmod_poly"))
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
   z = ($etype)(base_ring(a).n, Vector{$(mtype)}(undef, 0), 0, prec)
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
   while len > 0
      c = ccall(($(flint_fn*"_get_coeff_ui"), libflint), UInt,
            (Ref{($etype)}, Int), a, len - 1)
      if !iszero(c)
         break
      end
      len -= 1
   end
   return len
end

function length(x::($etype))
   return x.length
end

precision(x::($etype)) = x.prec

function coeff_raw(x::($etype), n::Int)
   R = base_ring(x)
   if n < 0
      return zero(UInt)
   end
   return ccall(($(flint_fn*"_get_coeff_ui"), libflint), UInt,
         (Ref{($etype)}, Int), x, n)
end

function coeff(x::($etype), n::Int)
   R = base_ring(x)
   z = coeff_raw(x, n)
   return R(z)
end

zero(R::($rtype)) = R(0)

one(R::($rtype)) = R(1)

function gen(R::($rtype))
   z = ($etype)(R.n, [fmpz(0), fmpz(1)], 2, max_precision(R))
   z.parent = R
   return z
end

function deepcopy_internal(a::($etype), dict::IdDict)
   z = ($etype)(a)
   z.parent = parent(a)
   return z
end

function isgen(a::($etype))
   return precision(a) == 0 ||
          (length(a) == 2 && isone(coeff(a, 1)) && iszero(coeff(a, 0)))
end

iszero(a::($etype)) = length(a) == 0

isunit(a::($etype)) = valuation(a) == 0 && isunit(coeff(a, 0))

function isone(a::($etype))
   return precision(a) == 0 ||
          Bool(ccall(($(flint_fn*"_is_one"), libflint), Cint,
                     (Ref{($etype)},), a))
end

# todo: write an nmod_poly_valuation
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
   z = ($etype)(par.n)
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
   z = ($etype)(par.n, coeffs, len, prec)
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
         (Ref{($etype)}, Ref{($etype)}), z, x)
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
         (Ref{($etype)}, Ref{($etype)}, Ref{($etype)}, Int),
          z, a, b, lenz)
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
         (Ref{($etype)}, Ref{($etype)}, Ref{($etype)}, Int),
         z, a, b, lenz)
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
         (Ref{($etype)}, Ref{($etype)}, Ref{($etype)}, Int),
         z, a, b, lenz)
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
   ccall(($(flint_fn*"_scalar_mul_nmod"), libflint), Nothing,
         (Ref{($etype)}, Ref{($etype)}, UInt),
         z, y, x.data)
   return z
end

*(x::($etype), y::$(mtype)) = y*x

function *(x::fmpz, y::($etype))
   R = base_ring(y)
   xmod = ccall((:fmpz_fdiv_ui, libflint), UInt,
                (Ref{fmpz}, UInt),
                x, R.n)
   return R(xmod)*y
end

*(x::($etype), y::fmpz) = y*x

*(x::Integer, y::($etype)) = fmpz(x)*y

*(x::($etype), y::Integer) = y*x

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
         (Ref{($etype)}, Ref{($etype)}, Int),
         z, x, len)
   ccall(($(flint_fn*"_set_trunc"), libflint), Nothing,
         (Ref{($etype)}, Ref{($etype)}, Int),
         z, z, zlen)
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
            (Ref{($etype)}, Ref{($etype)}, Int),
            z, x, len)
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
         (Ref{($etype)}, Ref{($etype)}, Int),
         z, x, prec)
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
            (Ref{($etype)}, Ref{($etype)}, UInt, Int),
            z, a, b, z.prec)
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
                     (Ref{($etype)}, Ref{($etype)}, Int),
                     x, y, n))
end

function isequal(x::($etype), y::($etype))
   if parent(x) != parent(y)
      return false
   end
   if x.prec != y.prec || length(x) != length(y)
      return false
   end
   return Bool(ccall(($(flint_fn*"_equal"), libflint), Cint,
                     (Ref{($etype)}, Ref{($etype)}, Int),
                     x, y, length(x)))
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
      z = ccall(($(flint_fn*"_get_coeff_ui"), libflint), UInt,
            (Ref{($etype)}, Int), x, 0)
      return z == y.data
   else
      return precision(x) == 0 || iszero(y)
   end
end

==(x::$(mtype), y::($etype)) = y == x

function ==(x::($etype), y::fmpz)
   R = base_ring(x)
   ymod = ccall((:fmpz_fdiv_ui, libflint), UInt,
                (Ref{fmpz}, UInt),
                y, modulus(x))
   return x == R(ymod)
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
         (Ref{($etype)}, Ref{($etype)}, Ref{($etype)}, Int),
         z, x, y, prec)
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
   yinv = inv(y)
   ccall(($(flint_fn*"_scalar_mul_nmod"), libflint), Nothing,
         (Ref{($etype)}, Ref{($etype)}, UInt),
         z, x, y.data)
   return z
end

function divexact(x::($etype), y::fmpz)
   R = base_ring(x)
   return divexact(x, R(y))
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
         (Ref{($etype)}, Ref{($etype)}, Int),
         ainv, a, a.prec)
   return ainv
end

###############################################################################
#
#   Unsafe functions
#
###############################################################################

function zero!(z::($etype))
   ccall(($(flint_fn*"_zero"), libflint), Nothing,
         (Ref{($etype)},), z)
   z.prec = parent(z).prec_max
   return z
end

function fit!(z::($etype), n::Int)
   ccall(($(flint_fn*"_fit_length"), libflint), Nothing,
         (Ref{($etype)}, Int), z, n)
   return nothing
end

function setcoeff!(z::($etype), n::Int, x::($mtype))
   ccall(($(flint_fn*"_set_coeff_ui"), libflint), Nothing,
         (Ref{($etype)}, Int, UInt), z, n, x.data)
   return z
end

function setcoeff!(z::($etype), n::Int, x::fmpz)
   R = base_ring(z)
   return setcoeff!(z, n, R(x))
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
          Ref{($etype)}, Int), z, a, b, lenz)
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
          Ref{($etype)}, Int), a, a, b, lenz)
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
         (Ref{($etype)}, Ref{($etype)}, Ref{($etype)}, Int),
          c, a, b, lenc)
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
   z = ($etype)(a.n)
   z.prec = a.prec_max
   z.parent = a
   return z
end

function (a::($rtype))(b::$(mtype))
   if iszero(b)
      z = ($etype)(a.n)
      z.prec = a.prec_max
   else
      z = ($etype)(a.n, [b], 1, a.prec_max)
   end
   z.parent = a
   return z
end

function (a::($rtype))(b::fmpz)
   R = base_ring(a)
   return a(R(b))
end

function (a::($rtype))(b::Integer)
   return a(fmpz(b))
end

function (a::($rtype))(b::($etype))
   parent(b) != a && error("Unable to coerce power series")
   return b
end

function (a::($rtype))(b::Array{fmpz, 1}, len::Int, prec::Int)
   z = ($etype)(a.n, b, len, prec)
   z.parent = a
   return z
end

function (a::($rtype))(b::Array{UInt, 1}, len::Int, prec::Int)
   z = ($etype)(a.n, b, len, prec)
   z.parent = a
   return z
end

function (a::($rtype))(b::Array{($mtype), 1}, len::Int, prec::Int)
   if length(b) > 0
      (base_ring(a) != parent(b[1])) && error("Wrong parents")
   end
   z = ($etype)(a.n, b, len, prec)
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

function PowerSeriesRing(R::NmodRing, prec::Int, s::Symbol; model=:capped_relative, cached = true)
   if model == :capped_relative
      parent_obj = NmodRelSeriesRing(R, prec, s, cached)
   elseif model == :capped_absolute
      parent_obj = NmodAbsSeriesRing(R, prec, s, cached)
   else
      error("Unknown model")
   end
   return parent_obj, gen(parent_obj)
end

function PowerSeriesRing(R::NmodRing, prec::Int, s::AbstractString; model=:capped_relative, cached = true)
   return PowerSeriesRing(R, prec, Symbol(s); model=model, cached=cached)
end

function AbsSeriesRing(R::NmodRing, prec::Int)
   return NmodAbsSeriesRing(R, prec, :x, false)
end

function RelSeriesRing(R::NmodRing, prec::Int)
   return NmodRelSeriesRing(R, prec, :x, false)
end

function PowerSeriesRing(R::GaloisField, prec::Int, s::Symbol; model=:capped_relative, cached = true)
   if model == :capped_relative
      parent_obj = GFPRelSeriesRing(R, prec, s, cached)
   elseif model == :capped_absolute
      parent_obj = GFPAbsSeriesRing(R, prec, s, cached)
   else
      error("Unknown model")
   end
   return parent_obj, gen(parent_obj)
end

function PowerSeriesRing(R::GaloisField, prec::Int, s::AbstractString; model=:capped_relative, cached = true)
   return PowerSeriesRing(R, prec, Symbol(s); model=model, cached=cached)
end

function AbsSeriesRing(R::GaloisField, prec::Int)
   return GFPAbsSeriesRing(R, prec, :x, false)
end

function RelSeriesRing(R::GaloisField, prec::Int)
   return GFPRelSeriesRing(R, prec, :x, false)
end
