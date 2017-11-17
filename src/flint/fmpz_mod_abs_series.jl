###############################################################################
#
#   fmpz_mod_abs_series.jl : Power series over flint fmpz integers
#
###############################################################################

export fmpz_mod_abs_series, FmpzModAbsSeriesRing

###############################################################################
#
#   Data type and parent object methods
#
###############################################################################

function O(a::fmpz_mod_abs_series)
   if iszero(a)
      return deepcopy(a)    # 0 + O(x^n)
   end
   prec = length(a) - 1
   prec < 0 && throw(DomainError())
   z = fmpz_mod_abs_series(modulus(a), Array{fmpz}(0), 0, prec)
   z.parent = parent(a)
   return z
end

elem_type(::Type{FmpzModAbsSeriesRing}) = fmpz_mod_abs_series

parent_type(::Type{fmpz_mod_abs_series}) = FmpzModAbsSeriesRing

base_ring(R::FmpzModAbsSeriesRing) = R.base_ring

var(a::FmpzModAbsSeriesRing) = a.S

###############################################################################
#
#   Basic manipulation
#
###############################################################################

max_precision(R::FmpzModAbsSeriesRing) = R.prec_max

function normalise(a::fmpz_mod_abs_series, len::Int)
   if len > 0
      c = fmpz()
      ccall((:fmpz_mod_poly_get_coeff_fmpz, :libflint), Void,
         (Ptr{fmpz}, Ptr{fmpz_mod_abs_series}, Int), &c, &a, len - 1)
   end
   while len > 0 && iszero(c)
      len -= 1
      if len > 0
         ccall((:fmpz_mod_poly_get_coeff_fmpz, :libflint), Void,
            (Ptr{fmpz}, Ptr{fmpz_mod_abs_series}, Int), &c, &a, len - 1)
      end
   end
   return len
end

function length(x::fmpz_mod_abs_series)
   return ccall((:fmpz_mod_poly_length, :libflint), Int, (Ptr{fmpz_mod_abs_series},), &x)
end

precision(x::fmpz_mod_abs_series) = x.prec

function coeff(x::fmpz_mod_abs_series, n::Int)
   R = base_ring(x)
   if n < 0
      return R(0)
   end
   z = fmpz()
   ccall((:fmpz_mod_poly_get_coeff_fmpz, :libflint), Void,
         (Ptr{fmpz}, Ptr{fmpz_mod_abs_series}, Int), &z, &x, n)
   return R(z)
end

zero(R::FmpzModAbsSeriesRing) = R(0)

one(R::FmpzModAbsSeriesRing) = R(1)

function gen(R::FmpzModAbsSeriesRing)
   z = fmpz_mod_abs_series(modulus(base_ring(R)), [fmpz(0), fmpz(1)], 2, max_precision(R))
   z.parent = R
   return z
end

function deepcopy_internal(a::fmpz_mod_abs_series, dict::ObjectIdDict)
   z = fmpz_mod_abs_series(a)
   z.prec = a.prec
   z.parent = parent(a)
   return z
end

function isgen(a::fmpz_mod_abs_series)
   return precision(a) == 0 || ccall((:fmpz_mod_poly_is_x, :libflint), Bool,
                            (Ptr{fmpz_mod_abs_series},), &a)
end

iszero(a::fmpz_mod_abs_series) = length(a) == 0

isunit(a::fmpz_mod_abs_series) = valuation(a) == 0 && isunit(coeff(a, 0))

function isone(a::fmpz_mod_abs_series)
   return precision(a) == 0 || ccall((:fmpz_mod_poly_is_one, :libflint), Bool,
                                (Ptr{fmpz_mod_abs_series},), &a)
end

# todo: write an fmpz_mod_poly_valuation
function valuation(a::fmpz_mod_abs_series)
   for i = 1:length(a)
      if !iszero(coeff(a, i - 1))
         return i - 1
      end
   end
   return precision(a)
end

###############################################################################
#
#   AbstractString I/O
#
###############################################################################

function show(io::IO, a::FmpzModAbsSeriesRing)
   print(io, "Univariate power series ring in ", var(a), " over ")
   show(io, base_ring(a))
end

show_minus_one(::Type{fmpz_mod_abs_series}) = show_minus_one(fmpz)

###############################################################################
#
#   Unary operators
#
###############################################################################

function -(x::fmpz_mod_abs_series)
   z = parent(x)()
   ccall((:fmpz_mod_poly_neg, :libflint), Void,
                (Ptr{fmpz_mod_abs_series}, Ptr{fmpz_mod_abs_series}),
               &z, &x)
   z.prec = x.prec
   return z
end

###############################################################################
#
#   Binary operators
#
###############################################################################

function +(a::fmpz_mod_abs_series, b::fmpz_mod_abs_series)
   check_parent(a, b)
   lena = length(a)
   lenb = length(b)

   prec = min(a.prec, b.prec)

   lena = min(lena, prec)
   lenb = min(lenb, prec)

   lenz = max(lena, lenb)
   z = parent(a)()
   z.prec = prec
   ccall((:fmpz_mod_poly_add_series, :libflint), Void,
                (Ptr{fmpz_mod_abs_series}, Ptr{fmpz_mod_abs_series}, Ptr{fmpz_mod_abs_series}, Int),
               &z, &a, &b, lenz)
   return z
end

function -(a::fmpz_mod_abs_series, b::fmpz_mod_abs_series)
   check_parent(a, b)
   lena = length(a)
   lenb = length(b)

   prec = min(a.prec, b.prec)

   lena = min(lena, prec)
   lenb = min(lenb, prec)

   lenz = max(lena, lenb)
   z = parent(a)()
   z.prec = prec
   ccall((:fmpz_mod_poly_sub_series, :libflint), Void,
                (Ptr{fmpz_mod_abs_series}, Ptr{fmpz_mod_abs_series}, Ptr{fmpz_mod_abs_series}, Int),
               &z, &a, &b, lenz)
   return z
end

function *(a::fmpz_mod_abs_series, b::fmpz_mod_abs_series)
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

   ccall((:fmpz_mod_poly_mullow, :libflint), Void,
                (Ptr{fmpz_mod_abs_series}, Ptr{fmpz_mod_abs_series}, Ptr{fmpz_mod_abs_series}, Int),
               &z, &a, &b, lenz)
   return z
end

###############################################################################
#
#   Ad hoc binary operators
#
###############################################################################

function *(x::Generic.Res{fmpz}, y::fmpz_mod_abs_series)
   z = parent(y)()
   z.prec = y.prec
   ccall((:fmpz_mod_poly_scalar_mul_fmpz, :libflint), Void,
                (Ptr{fmpz_mod_abs_series}, Ptr{fmpz_mod_abs_series}, Ptr{fmpz}),
               &z, &y, &x.data)
   return z
end

*(x::fmpz_mod_abs_series, y::fmpz) = y * x

function *(x::fmpz, y::fmpz_mod_abs_series)
   z = parent(y)()
   z.prec = y.prec
   r = mod(x, modulus(y))
   ccall((:fmpz_mod_poly_scalar_mul_fmpz, :libflint), Void,
                (Ptr{fmpz_mod_abs_series}, Ptr{fmpz_mod_abs_series}, Ptr{fmpz}),
               &z, &y, &r)
   return z
end

*(x::Integer, y::fmpz_mod_abs_series) = fmpz(x)*y

*(x::fmpz_mod_abs_series, y::Integer) = y * x

###############################################################################
#
#   Shifting
#
###############################################################################

function shift_left(x::fmpz_mod_abs_series, len::Int)
   len < 0 && throw(DomainError())
   xlen = length(x)
   z = parent(x)()
   z.prec = x.prec + len
   ccall((:fmpz_mod_poly_shift_left, :libflint), Void,
                (Ptr{fmpz_mod_abs_series}, Ptr{fmpz_mod_abs_series}, Int),
               &z, &x, len)
   return z
end

function shift_right(x::fmpz_mod_abs_series, len::Int)
   len < 0 && throw(DomainError())
   xlen = length(x)
   z = parent(x)()
   if len >= xlen
      z.prec = max(0, x.prec - len)
   else
      z.prec = x.prec - len
      ccall((:fmpz_mod_poly_shift_right, :libflint), Void,
                (Ptr{fmpz_mod_abs_series}, Ptr{fmpz_mod_abs_series}, Int),
               &z, &x, len)
   end
   return z
end

###############################################################################
#
#   Truncation
#
###############################################################################

function truncate(x::fmpz_mod_abs_series, prec::Int)
   prec < 0 && throw(DomainError())
   if x.prec <= prec
      return x
   end
   z = parent(x)()
   z.prec = prec
   ccall((:fmpz_mod_poly_set_trunc, :libflint), Void,
                (Ptr{fmpz_mod_abs_series}, Ptr{fmpz_mod_abs_series}, Int),
               &z, &x, prec)
   return z
end

###############################################################################
#
#   Powering
#
###############################################################################

function ^(a::fmpz_mod_abs_series, b::Int)
   b < 0 && throw(DomainError())
   if precision(a) > 0 && isgen(a) && b > 0
      return shift_left(a, b - 1)
   elseif length(a) == 1
      return parent(a)([coeff(a, 0)^b], 1, a.prec)
   elseif b == 0
      z = one(parent(a))
      set_prec!(z, precision(a))
      return z
   else
      z = parent(a)()
      z.prec = a.prec + (b - 1)*valuation(a)
      z.prec = min(z.prec, max_precision(parent(a)))
      ccall((:fmpz_mod_poly_pow_trunc, :libflint), Void,
                (Ptr{fmpz_mod_abs_series}, Ptr{fmpz_mod_abs_series}, Int, Int),
               &z, &a, b, z.prec)
   end
   return z
end

###############################################################################
#
#   Comparison
#
###############################################################################

function ==(x::fmpz_mod_abs_series, y::fmpz_mod_abs_series)
   check_parent(x, y)
   prec = min(x.prec, y.prec)

   n = max(length(x), length(y))
   n = min(n, prec)

   return Bool(ccall((:fmpz_mod_poly_equal_trunc, :libflint), Cint,
                (Ptr{fmpz_mod_abs_series}, Ptr{fmpz_mod_abs_series}, Int),
               &x, &y, n))
end

function isequal(x::fmpz_mod_abs_series, y::fmpz_mod_abs_series)
   if parent(x) != parent(y)
      return false
   end
   if x.prec != y.prec || length(x) != length(y)
      return false
   end
   return Bool(ccall((:fmpz_mod_poly_equal, :libflint), Cint,
                (Ptr{fmpz_mod_abs_series}, Ptr{fmpz_mod_abs_series}, Int),
               &x, &y, length(x)))
end

###############################################################################
#
#   Ad hoc comparisons
#
###############################################################################

function ==(x::fmpz_mod_abs_series, y::Generic.Res{fmpz})
   if length(x) > 1
      return false
   elseif length(x) == 1
      z = fmpz()
      ccall((:fmpz_mod_poly_get_coeff_fmpz, :libflint), Void,
                       (Ptr{fmpz}, Ptr{fmpz_mod_abs_series}, Int), &z, &x, 0)
      return ccall((:fmpz_equal, :libflint), Bool,
               (Ptr{fmpz}, Ptr{fmpz}), &z, &y)
   else
      return precision(x) == 0 || iszero(y)
   end
end

==(x::Generic.Res{fmpz}, y::fmpz_mod_abs_series) = y == x

function ==(x::fmpz_mod_abs_series, y::fmpz)
   if length(x) > 1
      return false
   elseif length(x) == 1
      z = fmpz()
      r = mod(y, modulus(x))
      ccall((:fmpz_mod_poly_get_coeff_fmpz, :libflint), Void,
                       (Ptr{fmpz}, Ptr{fmpz_mod_abs_series}, Int), &z, &x, 0)
      return ccall((:fmpz_equal, :libflint), Bool,
               (Ptr{fmpz}, Ptr{fmpz}), &z, &r)
   else
      r = mod(y, modulus(x))
      return precision(x) == 0 || iszero(r)
   end
end

==(x::fmpz, y::fmpz_mod_abs_series) = y == x

==(x::fmpz_mod_abs_series, y::Integer) = x == fmpz(y)

==(x::Integer, y::fmpz_mod_abs_series) = y == x

###############################################################################
#
#   Exact division
#
###############################################################################

function divexact(x::fmpz_mod_abs_series, y::fmpz_mod_abs_series)
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
   ccall((:fmpz_mod_poly_div_series, :libflint), Void,
                (Ptr{fmpz_mod_abs_series}, Ptr{fmpz_mod_abs_series}, Ptr{fmpz_mod_abs_series}, Int),
               &z, &x, &y, prec)
   return z
end

###############################################################################
#
#   Ad hoc exact division
#
###############################################################################

function divexact(x::fmpz_mod_abs_series, y::Generic.Res{fmpz})
   iszero(y) && throw(DivideError())
   z = parent(x)()
   z.prec = x.prec
   ccall((:fmpz_mod_poly_scalar_div_fmpz, :libflint), Void,
                (Ptr{fmpz_mod_abs_series}, Ptr{fmpz_mod_abs_series}, Ptr{fmpz}),
               &z, &x, &y)
   return z
end

function divexact(x::fmpz_mod_abs_series, y::fmpz)
   iszero(y) && throw(DivideError())
   z = parent(x)()
   z.prec = x.prec
   r = mod(y, modulus(x))
   ccall((:fmpz_mod_poly_scalar_div_fmpz, :libflint), Void,
                (Ptr{fmpz_mod_abs_series}, Ptr{fmpz_mod_abs_series}, Ptr{fmpz}),
               &z, &x, &y)
   return z
end

divexact(x::fmpz_mod_abs_series, y::Integer) = divexact(x, fmpz(y))

###############################################################################
#
#   Inversion
#
###############################################################################

function inv(a::fmpz_mod_abs_series)
   iszero(a) && throw(DivideError())
   !isunit(a) && error("Unable to invert power series")
   ainv = parent(a)()
   ainv.prec = a.prec
   ccall((:fmpz_mod_poly_inv_series, :libflint), Void,
                (Ptr{fmpz_mod_abs_series}, Ptr{fmpz_mod_abs_series}, Int),
               &ainv, &a, a.prec)
   return ainv
end

###############################################################################
#
#   Unsafe functions
#
###############################################################################

function setcoeff!(z::fmpz_mod_abs_series, n::Int, x::fmpz)
   ccall((:fmpz_mod_poly_set_coeff_fmpz, :libflint), Void,
                (Ptr{fmpz_mod_abs_series}, Int, Ptr{fmpz}),
               &z, n, &x)
   return z
end

function mul!(z::fmpz_mod_abs_series, a::fmpz_mod_abs_series, b::fmpz_mod_abs_series)
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
   ccall((:fmpz_mod_poly_mullow, :libflint), Void,
                (Ptr{fmpz_mod_abs_series}, Ptr{fmpz_mod_abs_series}, Ptr{fmpz_mod_abs_series}, Int),
               &z, &a, &b, lenz)
   return z
end

function addeq!(a::fmpz_mod_abs_series, b::fmpz_mod_abs_series)
   lena = length(a)
   lenb = length(b)

   prec = min(a.prec, b.prec)

   lena = min(lena, prec)
   lenb = min(lenb, prec)

   lenz = max(lena, lenb)
   a.prec = prec
   ccall((:fmpz_mod_poly_add_series, :libflint), Void,
                (Ptr{fmpz_mod_abs_series}, Ptr{fmpz_mod_abs_series}, Ptr{fmpz_mod_abs_series}, Int),
               &a, &a, &b, lenz)
   return a
end

###############################################################################
#
#   Promotion rules
#
###############################################################################

promote_rule(::Type{fmpz_mod_abs_series}, ::Type{T}) where {T <: Integer} = fmpz_mod_abs_series

promote_rule(::Type{fmpz_mod_abs_series}, ::Type{fmpz}) = fmpz_mod_abs_series

promote_rule(::Type{fmpz_mod_abs_series}, ::Type{Generic.Res{fmpz}}) = fmpz_mod_abs_series

###############################################################################
#
#   Parent object call overload
#
###############################################################################

function (a::FmpzModAbsSeriesRing)()
   m = modulus(base_ring(a))
   z = fmpz_mod_abs_series(m)
   z.prec = a.prec_max
   z.parent = a
   return z
end

function (a::FmpzModAbsSeriesRing)(b::Integer)
   m = modulus(base_ring(a))
   if b == 0
      z = fmpz_mod_abs_series(m)
      z.prec = a.prec_max
   else
      z = fmpz_mod_abs_series(m, [fmpz(b)], 1, a.prec_max)
   end
   z.parent = a
   return z
end

function (a::FmpzModAbsSeriesRing)(b::fmpz)
   m = modulus(base_ring(a))
   if iszero(b)
      z = fmpz_mod_abs_series(m)
      z.prec = a.prec_max
   else
      z = fmpz_mod_abs_series(m, [b], 1, a.prec_max)
   end
   z.parent = a
   return z
end

function (a::FmpzModAbsSeriesRing)(b::Generic.Res{fmpz})
   m = modulus(base_ring(a))
   if iszero(b)
      z = fmpz_mod_abs_series(m)
      z.prec = a.prec_max
   else
      z = fmpz_mod_abs_series(m, [b], 1, a.prec_max)
   end
   z.parent = a
   return z
end

function (a::FmpzModAbsSeriesRing)(b::fmpz_mod_abs_series)
   parent(b) != a && error("Unable to coerce power series")
   return b
end

function (a::FmpzModAbsSeriesRing)(b::Array{fmpz, 1}, len::Int, prec::Int)
   m = modulus(base_ring(a))
   z = fmpz_mod_abs_series(m, b, len, prec)
   z.parent = a
   return z
end

###############################################################################
#
#   PowerSeriesRing constructor
#
###############################################################################

function PowerSeriesRing(R::Generic.ResRing{fmpz}, prec::Int, s::AbstractString; model=:capped_relative, cached = true)
   S = Symbol(s)

   if model == :capped_relative
      parent_obj = FmpzModRelSeriesRing(R, prec, S, cached)
   elseif model == :capped_absolute
      parent_obj = FmpzModAbsSeriesRing(R, prec, S, cached)
   end
   return parent_obj, gen(parent_obj)
end
