###############################################################################
#
#   ZZRelPowerSeriesRingElem.jl : Power series over flint ZZRingElem integers
#
###############################################################################

###############################################################################
#
#   Data type and parent object methods
#
###############################################################################

function O(a::ZZRelPowerSeriesRingElem)
  val = pol_length(a) + valuation(a) - 1
  val < 0 && throw(DomainError(val, "Valuation must be non-negative"))
  z = ZZRelPowerSeriesRingElem(Vector{ZZRingElem}(undef, 0), 0, val, val)
  z.parent = parent(a)
  return z
end

elem_type(::Type{ZZRelPowerSeriesRing}) = ZZRelPowerSeriesRingElem

parent_type(::Type{ZZRelPowerSeriesRingElem}) = ZZRelPowerSeriesRing

base_ring(R::ZZRelPowerSeriesRing) = ZZ

rel_series_type(::Type{ZZRingElem}) = ZZRelPowerSeriesRingElem

var(a::ZZRelPowerSeriesRing) = a.S

###############################################################################
#
#   Basic manipulation
#
###############################################################################

max_precision(R::ZZRelPowerSeriesRing) = R.prec_max

function normalise(a::ZZRelPowerSeriesRingElem, len::Int)
  if len > 0
    c = ZZRingElem()
    @ccall libflint.fmpz_poly_get_coeff_fmpz(c::Ref{ZZRingElem}, a::Ref{ZZRelPowerSeriesRingElem}, (len - 1)::Int)::Nothing
  end
  while len > 0 && iszero(c)
    len -= 1
    if len > 0
      @ccall libflint.fmpz_poly_get_coeff_fmpz(c::Ref{ZZRingElem}, a::Ref{ZZRelPowerSeriesRingElem}, (len - 1)::Int)::Nothing
    end
  end
  return len
end

pol_length(x::ZZRelPowerSeriesRingElem) = x.length

precision(x::ZZRelPowerSeriesRingElem) = x.prec

function polcoeff(x::ZZRelPowerSeriesRingElem, n::Int)
  if n < 0
    return ZZRingElem(0)
  end
  z = ZZRingElem()
  @ccall libflint.fmpz_poly_get_coeff_fmpz(z::Ref{ZZRingElem}, x::Ref{ZZRelPowerSeriesRingElem}, n::Int)::Nothing
  return z
end

zero(R::ZZRelPowerSeriesRing) = R(0)

one(R::ZZRelPowerSeriesRing) = R(1)

function gen(R::ZZRelPowerSeriesRing)
  z = ZZRelPowerSeriesRingElem([ZZRingElem(1)], 1, max_precision(R) + 1, 1)
  z.parent = R
  return z
end

function deepcopy_internal(a::ZZRelPowerSeriesRingElem, dict::IdDict)
  z = ZZRelPowerSeriesRingElem(a)
  z.prec = a.prec
  z.val = a.val
  z.parent = parent(a)
  return z
end

function renormalize!(z::ZZRelPowerSeriesRingElem)
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
    @ccall libflint.fmpz_poly_shift_right(z::Ref{ZZRelPowerSeriesRingElem}, z::Ref{ZZRelPowerSeriesRingElem}, i::Int)::Nothing
  end
  return nothing
end

characteristic(::ZZRelPowerSeriesRing) = 0

###############################################################################
#
#   Similar
#
###############################################################################

function similar(f::RelPowerSeriesRingElem, R::ZZRing, max_prec::Int,
    s::Symbol=var(parent(f)); cached::Bool=true)
  z = ZZRelPowerSeriesRingElem()
  if base_ring(f) === R && s == var(parent(f)) &&
    f isa ZZRelPowerSeriesRingElem && max_precision(parent(f)) == max_prec
    # steal parent in case it is not cached
    z.parent = parent(f)
  else
    z.parent = ZZRelPowerSeriesRing(max_prec, s, cached)
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

function rel_series(R::ZZRing, arr::Vector{T},
    len::Int, prec::Int, val::Int, var::VarName=:x;
    max_precision::Int=prec, cached::Bool=true) where T
  prec < len + val && error("Precision too small for given data")
  coeffs = T == ZZRingElem ? arr : map(R, arr)
  coeffs = length(coeffs) == 0 ? ZZRingElem[] : coeffs
  z = ZZRelPowerSeriesRingElem(coeffs, len, prec, val)
  z.parent = ZZRelPowerSeriesRing(max_precision, Symbol(var), cached)
  return z
end

###############################################################################
#
#   Unary operators
#
###############################################################################

function -(x::ZZRelPowerSeriesRingElem)
  z = parent(x)()
  @ccall libflint.fmpz_poly_neg(z::Ref{ZZRelPowerSeriesRingElem}, x::Ref{ZZRelPowerSeriesRingElem})::Nothing
  z.prec = x.prec
  z.val = x.val
  return z
end

###############################################################################
#
#   Binary operators
#
###############################################################################

function +(a::ZZRelPowerSeriesRingElem, b::ZZRelPowerSeriesRingElem)
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
    @ccall libflint.fmpz_poly_set_trunc(z::Ref{ZZRelPowerSeriesRingElem}, b::Ref{ZZRelPowerSeriesRingElem}, max(0, lenz - b.val + a.val)::Int)::Nothing
    @ccall libflint.fmpz_poly_shift_left(z::Ref{ZZRelPowerSeriesRingElem}, z::Ref{ZZRelPowerSeriesRingElem}, (b.val - a.val)::Int)::Nothing
    @ccall libflint.fmpz_poly_add_series(z::Ref{ZZRelPowerSeriesRingElem}, z::Ref{ZZRelPowerSeriesRingElem}, a::Ref{ZZRelPowerSeriesRingElem}, lenz::Int)::Nothing
  elseif b.val < a.val
    lenz = max(lena + a.val - b.val, lenb)
    @ccall libflint.fmpz_poly_set_trunc(z::Ref{ZZRelPowerSeriesRingElem}, a::Ref{ZZRelPowerSeriesRingElem}, max(0, lenz - a.val + b.val)::Int)::Nothing
    @ccall libflint.fmpz_poly_shift_left(z::Ref{ZZRelPowerSeriesRingElem}, z::Ref{ZZRelPowerSeriesRingElem}, (a.val - b.val)::Int)::Nothing
    @ccall libflint.fmpz_poly_add_series(z::Ref{ZZRelPowerSeriesRingElem}, z::Ref{ZZRelPowerSeriesRingElem}, b::Ref{ZZRelPowerSeriesRingElem}, lenz::Int)::Nothing
  else
    lenz = max(lena, lenb)
    @ccall libflint.fmpz_poly_add_series(z::Ref{ZZRelPowerSeriesRingElem}, a::Ref{ZZRelPowerSeriesRingElem}, b::Ref{ZZRelPowerSeriesRingElem}, lenz::Int)::Nothing
  end
  z.prec = prec
  z.val = val
  renormalize!(z)
  return z
end

function -(a::ZZRelPowerSeriesRingElem, b::ZZRelPowerSeriesRingElem)
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
    @ccall libflint.fmpz_poly_set_trunc(z::Ref{ZZRelPowerSeriesRingElem}, b::Ref{ZZRelPowerSeriesRingElem}, max(0, lenz - b.val + a.val)::Int)::Nothing
    @ccall libflint.fmpz_poly_shift_left(z::Ref{ZZRelPowerSeriesRingElem}, z::Ref{ZZRelPowerSeriesRingElem}, (b.val - a.val)::Int)::Nothing
    @ccall libflint.fmpz_poly_neg(z::Ref{ZZRelPowerSeriesRingElem}, z::Ref{ZZRelPowerSeriesRingElem})::Nothing
    @ccall libflint.fmpz_poly_add_series(z::Ref{ZZRelPowerSeriesRingElem}, z::Ref{ZZRelPowerSeriesRingElem}, a::Ref{ZZRelPowerSeriesRingElem}, lenz::Int)::Nothing
  elseif b.val < a.val
    lenz = max(lena + a.val - b.val, lenb)
    @ccall libflint.fmpz_poly_set_trunc(z::Ref{ZZRelPowerSeriesRingElem}, a::Ref{ZZRelPowerSeriesRingElem}, max(0, lenz - a.val + b.val)::Int)::Nothing
    @ccall libflint.fmpz_poly_shift_left(z::Ref{ZZRelPowerSeriesRingElem}, z::Ref{ZZRelPowerSeriesRingElem}, (a.val - b.val)::Int)::Nothing
    @ccall libflint.fmpz_poly_sub_series(z::Ref{ZZRelPowerSeriesRingElem}, z::Ref{ZZRelPowerSeriesRingElem}, b::Ref{ZZRelPowerSeriesRingElem}, lenz::Int)::Nothing
  else
    lenz = max(lena, lenb)
    @ccall libflint.fmpz_poly_sub_series(z::Ref{ZZRelPowerSeriesRingElem}, a::Ref{ZZRelPowerSeriesRingElem}, b::Ref{ZZRelPowerSeriesRingElem}, lenz::Int)::Nothing
  end
  z.prec = prec
  z.val = val
  renormalize!(z)
  return z
end

function *(a::ZZRelPowerSeriesRingElem, b::ZZRelPowerSeriesRingElem)
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
  @ccall libflint.fmpz_poly_mullow(z::Ref{ZZRelPowerSeriesRingElem}, a::Ref{ZZRelPowerSeriesRingElem}, b::Ref{ZZRelPowerSeriesRingElem}, lenz::Int)::Nothing

  return z
end

###############################################################################
#
#   Ad hoc binary operators
#
###############################################################################

function *(x::Int, y::ZZRelPowerSeriesRingElem)
  z = parent(y)()
  z.prec = y.prec
  z.val = y.val
  @ccall libflint.fmpz_poly_scalar_mul_si(z::Ref{ZZRelPowerSeriesRingElem}, y::Ref{ZZRelPowerSeriesRingElem}, x::Int)::Nothing
  return z
end

*(x::ZZRelPowerSeriesRingElem, y::Int) = y * x

function *(x::ZZRingElem, y::ZZRelPowerSeriesRingElem)
  z = parent(y)()
  z.prec = y.prec
  z.val = y.val
  @ccall libflint.fmpz_poly_scalar_mul_fmpz(z::Ref{ZZRelPowerSeriesRingElem}, y::Ref{ZZRelPowerSeriesRingElem}, x::Ref{ZZRingElem})::Nothing
  return z
end

*(x::ZZRelPowerSeriesRingElem, y::ZZRingElem) = y * x

*(x::Integer, y::ZZRelPowerSeriesRingElem) = ZZRingElem(x)*y

*(x::ZZRelPowerSeriesRingElem, y::Integer) = y*x

###############################################################################
#
#   Shifting
#
###############################################################################

function shift_left(x::ZZRelPowerSeriesRingElem, len::Int)
  len < 0 && throw(DomainError(len, "Shift must be non-negative"))
  xlen = pol_length(x)
  z = ZZRelPowerSeriesRingElem(x)
  z.prec = x.prec + len
  z.val = x.val + len
  z.parent = parent(x)
  return z
end

function shift_right(x::ZZRelPowerSeriesRingElem, len::Int)
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
    @ccall libflint.fmpz_poly_shift_right(z::Ref{ZZRelPowerSeriesRingElem}, x::Ref{ZZRelPowerSeriesRingElem}, (xlen - zlen)::Int)::Nothing
    renormalize!(z)
  end
  return z
end

###############################################################################
#
#   Truncation
#
###############################################################################

function truncate(x::ZZRelPowerSeriesRingElem, k::Int)
  return truncate!(deepcopy(x), k)
end

function truncate!(x::ZZRelPowerSeriesRingElem, k::Int)
  k < 0 && throw(DomainError(k, "Index must be non-negative"))
  if precision(x) <= k
    return x
  end
  if k <= valuation(x)
    x = zero!(x)
    x.val = k
  else
    @ccall libflint.fmpz_poly_truncate(x::Ref{ZZRelPowerSeriesRingElem}, (k - valuation(x))::Int)::Nothing
  end
  x.prec = k
  return x
end

###############################################################################
#
#   Powering
#
###############################################################################

function ^(a::ZZRelPowerSeriesRingElem, b::Int)
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
    return parent(a)([polcoeff(a, 0)^b], 1,
                     (b - 1)*valuation(a) + precision(a), b*valuation(a))
  elseif b == 0
    return one(parent(a))
  else
    z = parent(a)()
    z.prec = a.prec + (b - 1)*valuation(a)
    z.val = b*valuation(a)
    @ccall libflint.fmpz_poly_pow_trunc(z::Ref{ZZRelPowerSeriesRingElem}, a::Ref{ZZRelPowerSeriesRingElem}, b::Int, (z.prec - z.val)::Int)::Nothing
  end
  return z
end

###############################################################################
#
#   Comparison
#
###############################################################################

function ==(x::ZZRelPowerSeriesRingElem, y::ZZRelPowerSeriesRingElem)
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
  return Bool(@ccall libflint.fmpz_poly_equal_trunc(x::Ref{ZZRelPowerSeriesRingElem}, y::Ref{ZZRelPowerSeriesRingElem}, xlen::Int)::Cint)
end

function isequal(x::ZZRelPowerSeriesRingElem, y::ZZRelPowerSeriesRingElem)
  if parent(x) != parent(y)
    return false
  end
  if x.prec != y.prec || x.val != y.val || pol_length(x) != pol_length(y)
    return false
  end
  return Bool(@ccall libflint.fmpz_poly_equal(x::Ref{ZZRelPowerSeriesRingElem}, y::Ref{ZZRelPowerSeriesRingElem})::Cint)
end

###############################################################################
#
#   Ad hoc comparisons
#
###############################################################################

function ==(x::ZZRelPowerSeriesRingElem, y::ZZRingElem)
  if precision(x) == 0
    return true
  elseif pol_length(x) > 1
    return false
  elseif pol_length(x) == 1
    if x.val == 0
      z = ZZRingElem()
      @ccall libflint.fmpz_poly_get_coeff_fmpz(z::Ref{ZZRingElem}, x::Ref{ZZRelPowerSeriesRingElem}, 0::Int)::Nothing
      return @ccall libflint.fmpz_equal(z::Ref{ZZRingElem}, y::Ref{ZZRingElem}, 0::Int)::Bool
    else
      return false
    end
  else
    return iszero(y)
  end
end

==(x::ZZRingElem, y::ZZRelPowerSeriesRingElem) = y == x

==(x::ZZRelPowerSeriesRingElem, y::Integer) = x == ZZRingElem(y)

==(x::Integer, y::ZZRelPowerSeriesRingElem) = y == x

###############################################################################
#
#   Exact division
#
###############################################################################

function divexact(x::ZZRelPowerSeriesRingElem, y::ZZRelPowerSeriesRingElem; check::Bool=true)
  check_parent(x, y)
  iszero(y) && throw(DivideError())
  yval = valuation(y)
  xval = valuation(x)
  if yval != 0
    if check && xval < yval
      error("Not an exact division")
    end
    x = shift_right(x, yval)
    y = shift_right(y, yval)
  end
  prec = min(x.prec - x.val, y.prec - y.val)
  z = parent(x)()
  z.val = xval - yval
  z.prec = prec + z.val
  if prec != 0
    @ccall libflint.fmpz_poly_div_series(z::Ref{ZZRelPowerSeriesRingElem}, x::Ref{ZZRelPowerSeriesRingElem}, y::Ref{ZZRelPowerSeriesRingElem}, prec::Int)::Nothing
  end
  return z
end

###############################################################################
#
#   Ad hoc exact division
#
###############################################################################

function divexact(x::ZZRelPowerSeriesRingElem, y::Int; check::Bool=true)
  y == 0 && throw(DivideError())
  z = parent(x)()
  z.prec = x.prec
  z.val = x.val
  @ccall libflint.fmpz_poly_scalar_divexact_si(z::Ref{ZZRelPowerSeriesRingElem}, x::Ref{ZZRelPowerSeriesRingElem}, y::Int)::Nothing
  return z
end

function divexact(x::ZZRelPowerSeriesRingElem, y::ZZRingElem; check::Bool=true)
  iszero(y) && throw(DivideError())
  z = parent(x)()
  z.prec = x.prec
  z.prec = x.prec
  z.val = x.val
  @ccall libflint.fmpz_poly_scalar_divexact_fmpz(z::Ref{ZZRelPowerSeriesRingElem}, x::Ref{ZZRelPowerSeriesRingElem}, y::Ref{ZZRingElem})::Nothing
  return z
end

divexact(x::ZZRelPowerSeriesRingElem, y::Integer; check::Bool=true) = divexact(x, ZZRingElem(y); check=check)

###############################################################################
#
#   Inversion
#
###############################################################################

function inv(a::ZZRelPowerSeriesRingElem)
  iszero(a) && throw(DivideError())
  !is_unit(a) && error("Unable to invert power series")
  ainv = parent(a)()
  ainv.prec = a.prec
  ainv.val = 0
  @ccall libflint.fmpz_poly_inv_series(ainv::Ref{ZZRelPowerSeriesRingElem}, a::Ref{ZZRelPowerSeriesRingElem}, a.prec::Int)::Nothing
  return ainv
end

###############################################################################
#
#   Square root
#
###############################################################################

function Base.sqrt(a::ZZRelPowerSeriesRingElem; check::Bool=true)
  asqrt = parent(a)()
  val = div(valuation(a), 2)
  asqrt.prec = a.prec - val
  asqrt.val = val
  flag = Bool(@ccall libflint.fmpz_poly_sqrt_series(asqrt::Ref{ZZRelPowerSeriesRingElem}, a::Ref{ZZRelPowerSeriesRingElem}, (a.prec - 2*val)::Int)::Cint)
  check && flag == false && error("Not a square in sqrt")
  return asqrt
end

###############################################################################
#
#   Unsafe functions
#
###############################################################################

function zero!(x::ZZRelPowerSeriesRingElem)
  @ccall libflint.fmpz_poly_zero(x::Ref{ZZRelPowerSeriesRingElem})::Nothing
  x.prec = parent(x).prec_max
  x.val = parent(x).prec_max
  return x
end

function one!(x::ZZRelPowerSeriesRingElem)
  @ccall libflint.fmpz_poly_one(x::Ref{ZZRelPowerSeriesRingElem})::Nothing
  x.prec = parent(x).prec_max
  x.val = 0
  return x
end

function fit!(z::ZZRelPowerSeriesRingElem, n::Int)
  @ccall libflint.fmpz_poly_fit_length(z::Ref{ZZRelPowerSeriesRingElem}, n::Int)::Nothing
  return nothing
end

function setcoeff!(z::ZZRelPowerSeriesRingElem, n::Int, x::ZZRingElem)
  @ccall libflint.fmpz_poly_set_coeff_fmpz(z::Ref{ZZRelPowerSeriesRingElem}, n::Int, x::Ref{ZZRingElem})::Nothing
  return z
end

function mul!(z::ZZRelPowerSeriesRingElem, a::ZZRelPowerSeriesRingElem, b::ZZRelPowerSeriesRingElem)
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
  @ccall libflint.fmpz_poly_mullow(z::Ref{ZZRelPowerSeriesRingElem}, a::Ref{ZZRelPowerSeriesRingElem}, b::Ref{ZZRelPowerSeriesRingElem}, lenz::Int)::Nothing
  return z
end

function add!(a::ZZRelPowerSeriesRingElem, b::ZZRelPowerSeriesRingElem)
  lena = pol_length(a)
  lenb = pol_length(b)
  prec = min(a.prec, b.prec)
  val = min(a.val, b.val)
  lena = min(lena, prec - a.val)
  lenb = min(lenb, prec - b.val)
  if a.val < b.val
    z = ZZRelPowerSeriesRingElem()
    z.parent = parent(a)
    lenz = max(lena, lenb + b.val - a.val)
    @ccall libflint.fmpz_poly_set_trunc(z::Ref{ZZRelPowerSeriesRingElem}, b::Ref{ZZRelPowerSeriesRingElem}, max(0, lenz - b.val + a.val)::Int)::Nothing
    @ccall libflint.fmpz_poly_shift_left(z::Ref{ZZRelPowerSeriesRingElem}, z::Ref{ZZRelPowerSeriesRingElem}, (b.val - a.val)::Int)::Nothing
    @ccall libflint.fmpz_poly_add_series(a::Ref{ZZRelPowerSeriesRingElem}, a::Ref{ZZRelPowerSeriesRingElem}, z::Ref{ZZRelPowerSeriesRingElem}, lenz::Int)::Nothing
  elseif b.val < a.val
    lenz = max(lena + a.val - b.val, lenb)
    @ccall libflint.fmpz_poly_truncate(a::Ref{ZZRelPowerSeriesRingElem}, max(0, lenz - a.val + b.val)::Int)::Nothing
    @ccall libflint.fmpz_poly_shift_left(a::Ref{ZZRelPowerSeriesRingElem}, a::Ref{ZZRelPowerSeriesRingElem}, (a.val - b.val)::Int)::Nothing
    @ccall libflint.fmpz_poly_add_series(a::Ref{ZZRelPowerSeriesRingElem}, a::Ref{ZZRelPowerSeriesRingElem}, b::Ref{ZZRelPowerSeriesRingElem}, lenz::Int)::Nothing
  else
    lenz = max(lena, lenb)
    @ccall libflint.fmpz_poly_add_series(a::Ref{ZZRelPowerSeriesRingElem}, a::Ref{ZZRelPowerSeriesRingElem}, b::Ref{ZZRelPowerSeriesRingElem}, lenz::Int)::Nothing
  end
  a.prec = prec
  a.val = val
  renormalize!(a)
  return a
end

function add!(c::ZZRelPowerSeriesRingElem, a::ZZRelPowerSeriesRingElem, b::ZZRelPowerSeriesRingElem)
  if c === a
    return add!(c, b)
  elseif c === b
    return add!(c, a)
  end
  lena = pol_length(a)
  lenb = pol_length(b)
  prec = min(a.prec, b.prec)
  val = min(a.val, b.val)
  lena = min(lena, prec - a.val)
  lenb = min(lenb, prec - b.val)
  if a.val < b.val
    lenc = max(lena, lenb + b.val - a.val)
    @ccall libflint.fmpz_poly_set_trunc(c::Ref{ZZRelPowerSeriesRingElem}, b::Ref{ZZRelPowerSeriesRingElem}, max(0, lenc - b.val + a.val)::Int)::Nothing
    @ccall libflint.fmpz_poly_shift_left(c::Ref{ZZRelPowerSeriesRingElem}, c::Ref{ZZRelPowerSeriesRingElem}, (b.val - a.val)::Int)::Nothing
    @ccall libflint.fmpz_poly_add_series(c::Ref{ZZRelPowerSeriesRingElem}, c::Ref{ZZRelPowerSeriesRingElem}, a::Ref{ZZRelPowerSeriesRingElem}, lenc::Int)::Nothing
  elseif b.val < a.val
    lenc = max(lena + a.val - b.val, lenb)
    @ccall libflint.fmpz_poly_set_trunc(c::Ref{ZZRelPowerSeriesRingElem}, a::Ref{ZZRelPowerSeriesRingElem}, max(0, lenc - a.val + b.val)::Int)::Nothing
    @ccall libflint.fmpz_poly_shift_left(c::Ref{ZZRelPowerSeriesRingElem}, c::Ref{ZZRelPowerSeriesRingElem}, (a.val - b.val)::Int)::Nothing
    @ccall libflint.fmpz_poly_add_series(c::Ref{ZZRelPowerSeriesRingElem}, c::Ref{ZZRelPowerSeriesRingElem}, b::Ref{ZZRelPowerSeriesRingElem}, lenc::Int)::Nothing
  else
    lenc = max(lena, lenb)
    @ccall libflint.fmpz_poly_add_series(c::Ref{ZZRelPowerSeriesRingElem}, a::Ref{ZZRelPowerSeriesRingElem}, b::Ref{ZZRelPowerSeriesRingElem}, lenc::Int)::Nothing
  end
  c.prec = prec
  c.val = val
  renormalize!(c)
  return c
end

function set_length!(a::ZZRelPowerSeriesRingElem, n::Int)
  @ccall libflint._fmpz_poly_set_length(a::Ref{ZZRelPowerSeriesRingElem}, n::Int)::Nothing
  return a
end

###############################################################################
#
#   Promotion rules
#
###############################################################################

promote_rule(::Type{ZZRelPowerSeriesRingElem}, ::Type{T}) where {T <: Integer} = ZZRelPowerSeriesRingElem

promote_rule(::Type{ZZRelPowerSeriesRingElem}, ::Type{ZZRingElem}) = ZZRelPowerSeriesRingElem

###############################################################################
#
#   Parent object call overload
#
###############################################################################

function (a::ZZRelPowerSeriesRing)()
  z = ZZRelPowerSeriesRingElem()
  z.prec = a.prec_max
  z.val = a.prec_max
  z.parent = a
  return z
end

function (a::ZZRelPowerSeriesRing)(b::Integer)
  if b == 0
    z = ZZRelPowerSeriesRingElem()
    z.prec = a.prec_max
    z.val = a.prec_max
  else
    z = ZZRelPowerSeriesRingElem([ZZRingElem(b)], 1, a.prec_max, 0)
  end
  z.parent = a
  return z
end

function (a::ZZRelPowerSeriesRing)(b::ZZRingElem)
  if b == 0
    z = ZZRelPowerSeriesRingElem()
    z.prec = a.prec_max
    z.val = a.prec_max
  else
    z = ZZRelPowerSeriesRingElem([b], 1, a.prec_max, 0)
  end
  z.parent = a
  return z
end

function (a::ZZRelPowerSeriesRing)(b::ZZRelPowerSeriesRingElem)
  parent(b) != a && error("Unable to coerce power series")
  return b
end

function (a::ZZRelPowerSeriesRing)(b::Vector{ZZRingElem}, len::Int, prec::Int, val::Int)
  z = ZZRelPowerSeriesRingElem(b, len, prec, val)
  z.parent = a
  return z
end
