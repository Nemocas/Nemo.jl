###############################################################################
#
#   ZZAbsPowerSeriesRingElem.jl : Power series over flint ZZRingElem integers
#
###############################################################################

###############################################################################
#
#   Data type and parent object methods
#
###############################################################################

function O(a::ZZAbsPowerSeriesRingElem)
  if iszero(a)
    return deepcopy(a)    # 0 + O(x^n)
  end
  prec = length(a) - 1
  prec < 0 && throw(DomainError(prec, "Precision must be non-negative"))
  z = ZZAbsPowerSeriesRingElem(Vector{ZZRingElem}(undef, 0), 0, prec)
  z.parent = parent(a)
  return z
end

elem_type(::Type{ZZAbsPowerSeriesRing}) = ZZAbsPowerSeriesRingElem

parent_type(::Type{ZZAbsPowerSeriesRingElem}) = ZZAbsPowerSeriesRing

base_ring(R::ZZAbsPowerSeriesRing) = ZZ

abs_series_type(::Type{ZZRingElem}) = ZZAbsPowerSeriesRingElem

var(a::ZZAbsPowerSeriesRing) = a.S

###############################################################################
#
#   Basic manipulation
#
###############################################################################

max_precision(R::ZZAbsPowerSeriesRing) = R.prec_max

function normalise(a::ZZAbsPowerSeriesRingElem, len::Int)
  len <= 0 && return len
  c = ZZRingElem()
  @ccall libflint.fmpz_poly_get_coeff_fmpz(c::Ref{ZZRingElem}, a::Ref{ZZAbsPowerSeriesRingElem}, (len - 1)::Int)::Nothing
  while len > 0 && iszero(c)
    len -= 1
    if len > 0
      @ccall libflint.fmpz_poly_get_coeff_fmpz(c::Ref{ZZRingElem}, a::Ref{ZZAbsPowerSeriesRingElem}, (len - 1)::Int)::Nothing
    end
  end
  return len
end

length(x::ZZAbsPowerSeriesRingElem) = x.length

precision(x::ZZAbsPowerSeriesRingElem) = x.prec

function coeff(x::ZZAbsPowerSeriesRingElem, n::Int)
  if n < 0
    return ZZRingElem(0)
  end
  z = ZZRingElem()
  @ccall libflint.fmpz_poly_get_coeff_fmpz(z::Ref{ZZRingElem}, x::Ref{ZZAbsPowerSeriesRingElem}, n::Int)::Nothing
  return z
end

zero(R::ZZAbsPowerSeriesRing) = R(0)

one(R::ZZAbsPowerSeriesRing) = R(1)

function gen(R::ZZAbsPowerSeriesRing)
  z = ZZAbsPowerSeriesRingElem([ZZRingElem(0), ZZRingElem(1)], 2, max_precision(R))
  z.parent = R
  return z
end

function deepcopy_internal(a::ZZAbsPowerSeriesRingElem, dict::IdDict)
  z = ZZAbsPowerSeriesRingElem(a)
  z.prec = a.prec
  z.parent = parent(a)
  return z
end

function is_gen(a::ZZAbsPowerSeriesRingElem)
  return precision(a) == 0 || @ccall libflint.fmpz_poly_is_gen(a::Ref{ZZAbsPowerSeriesRingElem})::Bool
end

iszero(a::ZZAbsPowerSeriesRingElem) = length(a) == 0

is_unit(a::ZZAbsPowerSeriesRingElem) = valuation(a) == 0 && is_unit(coeff(a, 0))

function isone(a::ZZAbsPowerSeriesRingElem)
  return precision(a) == 0 || @ccall libflint.fmpz_poly_is_one(a::Ref{ZZAbsPowerSeriesRingElem})::Bool
end

# todo: write an fmpz_poly_valuation
function valuation(a::ZZAbsPowerSeriesRingElem)
  for i = 1:length(a)
    if !iszero(coeff(a, i - 1))
      return i - 1
    end
  end
  return precision(a)
end

characteristic(::ZZAbsPowerSeriesRing) = 0

###############################################################################
#
#   Similar
#
###############################################################################

function similar(f::AbsPowerSeriesRingElem, R::ZZRing, max_prec::Int,
    s::Symbol=var(parent(f)); cached::Bool=true)
  z = ZZAbsPowerSeriesRingElem()
  if base_ring(f) === R && s == var(parent(f)) &&
    f isa ZZAbsPowerSeriesRingElem && max_precision(parent(f)) == max_prec
    # steal parent in case it is not cached
    z.parent = parent(f)
  else
    z.parent = ZZAbsPowerSeriesRing(max_prec, s, cached)
  end
  z.prec = max_prec
  return z
end

###############################################################################
#
#   abs_series constructor
#
###############################################################################

function abs_series(R::ZZRing, arr::Vector{T},
    len::Int, prec::Int, var::VarName=:x;
    max_precision::Int=prec, cached::Bool=true) where T
  prec < len && error("Precision too small for given data")
  coeffs = T == ZZRingElem ? arr : map(R, arr)
  coeffs = length(coeffs) == 0 ? ZZRingElem[] : coeffs
  z = ZZAbsPowerSeriesRingElem(coeffs, len, prec)
  z.parent = ZZAbsPowerSeriesRing(max_precision, Symbol(var), cached)
  return z
end

###############################################################################
#
#   AbstractString I/O
#
###############################################################################

function show(io::IO, a::ZZAbsPowerSeriesRing)
  @show_name(io, a)
  @show_special(io, a)
  print(io, "Univariate power series ring in ", var(a), " over ")
  show(io, base_ring(a))
end

###############################################################################
#
#   Unary operators
#
###############################################################################

function -(x::ZZAbsPowerSeriesRingElem)
  z = parent(x)()
  @ccall libflint.fmpz_poly_neg(z::Ref{ZZAbsPowerSeriesRingElem}, x::Ref{ZZAbsPowerSeriesRingElem})::Nothing
  z.prec = x.prec
  return z
end

###############################################################################
#
#   Binary operators
#
###############################################################################

function +(a::ZZAbsPowerSeriesRingElem, b::ZZAbsPowerSeriesRingElem)
  check_parent(a, b)
  lena = length(a)
  lenb = length(b)

  prec = min(a.prec, b.prec)

  lena = min(lena, prec)
  lenb = min(lenb, prec)

  lenz = max(lena, lenb)
  z = parent(a)()
  z.prec = prec
  @ccall libflint.fmpz_poly_add_series(z::Ref{ZZAbsPowerSeriesRingElem}, a::Ref{ZZAbsPowerSeriesRingElem}, b::Ref{ZZAbsPowerSeriesRingElem}, lenz::Int)::Nothing
  return z
end

function -(a::ZZAbsPowerSeriesRingElem, b::ZZAbsPowerSeriesRingElem)
  check_parent(a, b)
  lena = length(a)
  lenb = length(b)

  prec = min(a.prec, b.prec)

  lena = min(lena, prec)
  lenb = min(lenb, prec)

  lenz = max(lena, lenb)
  z = parent(a)()
  z.prec = prec
  @ccall libflint.fmpz_poly_sub_series(z::Ref{ZZAbsPowerSeriesRingElem}, a::Ref{ZZAbsPowerSeriesRingElem}, b::Ref{ZZAbsPowerSeriesRingElem}, lenz::Int)::Nothing
  return z
end

function *(a::ZZAbsPowerSeriesRingElem, b::ZZAbsPowerSeriesRingElem)
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

  @ccall libflint.fmpz_poly_mullow(z::Ref{ZZAbsPowerSeriesRingElem}, a::Ref{ZZAbsPowerSeriesRingElem}, b::Ref{ZZAbsPowerSeriesRingElem}, lenz::Int)::Nothing
  return z
end

###############################################################################
#
#   Ad hoc binary operators
#
###############################################################################

function *(x::Int, y::ZZAbsPowerSeriesRingElem)
  z = parent(y)()
  z.prec = y.prec
  @ccall libflint.fmpz_poly_scalar_mul_si(z::Ref{ZZAbsPowerSeriesRingElem}, y::Ref{ZZAbsPowerSeriesRingElem}, x::Int)::Nothing
  return z
end

*(x::ZZAbsPowerSeriesRingElem, y::Int) = y * x

function *(x::ZZRingElem, y::ZZAbsPowerSeriesRingElem)
  z = parent(y)()
  z.prec = y.prec
  @ccall libflint.fmpz_poly_scalar_mul_fmpz(z::Ref{ZZAbsPowerSeriesRingElem}, y::Ref{ZZAbsPowerSeriesRingElem}, x::Ref{ZZRingElem})::Nothing
  return z
end

*(x::ZZAbsPowerSeriesRingElem, y::ZZRingElem) = y * x

*(x::Integer, y::ZZAbsPowerSeriesRingElem) = ZZRingElem(x)*y

*(x::ZZAbsPowerSeriesRingElem, y::Integer) = y*x

###############################################################################
#
#   Shifting
#
###############################################################################

function shift_left(x::ZZAbsPowerSeriesRingElem, len::Int)
  len < 0 && throw(DomainError(len, "Shift must be non-negative"))
  xlen = length(x)
  z = parent(x)()
  z.prec = x.prec + len
  z.prec = min(z.prec, max_precision(parent(x)))
  zlen = min(z.prec, xlen + len)
  @ccall libflint.fmpz_poly_shift_left(z::Ref{ZZAbsPowerSeriesRingElem}, x::Ref{ZZAbsPowerSeriesRingElem}, len::Int)::Nothing
  @ccall libflint.fmpz_poly_set_trunc(z::Ref{ZZAbsPowerSeriesRingElem}, z::Ref{ZZAbsPowerSeriesRingElem}, zlen::Int)::Nothing
  return z
end

function shift_right(x::ZZAbsPowerSeriesRingElem, len::Int)
  len < 0 && throw(DomainError(len, "Shift must be non-negative"))
  xlen = length(x)
  z = parent(x)()
  if len >= xlen
    z.prec = max(0, x.prec - len)
  else
    z.prec = x.prec - len
    @ccall libflint.fmpz_poly_shift_right(z::Ref{ZZAbsPowerSeriesRingElem}, x::Ref{ZZAbsPowerSeriesRingElem}, len::Int)::Nothing
  end
  return z
end

###############################################################################
#
#   Truncation
#
###############################################################################

function truncate(x::ZZAbsPowerSeriesRingElem, k::Int)
  return truncate!(deepcopy(x), k)
end

function truncate!(x::ZZAbsPowerSeriesRingElem, k::Int)
  k < 0 && throw(DomainError(k, "Index must be non-negative"))
  if precision(x) <= k
    return x
  end
  @ccall libflint.fmpz_poly_truncate(x::Ref{ZZAbsPowerSeriesRingElem}, k::Int)::Nothing
  x.prec = k
  return x
end

###############################################################################
#
#   Powering
#
###############################################################################

function ^(a::ZZAbsPowerSeriesRingElem, b::Int)
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
    @ccall libflint.fmpz_poly_pow_trunc(z::Ref{ZZAbsPowerSeriesRingElem}, a::Ref{ZZAbsPowerSeriesRingElem}, b::Int, z.prec::Int)::Nothing
  end
  return z
end

###############################################################################
#
#   Comparison
#
###############################################################################

function ==(x::ZZAbsPowerSeriesRingElem, y::ZZAbsPowerSeriesRingElem)
  check_parent(x, y)
  prec = min(x.prec, y.prec)

  n = max(length(x), length(y))
  n = min(n, prec)

  return Bool(@ccall libflint.fmpz_poly_equal_trunc(x::Ref{ZZAbsPowerSeriesRingElem}, y::Ref{ZZAbsPowerSeriesRingElem}, n::Int)::Cint)
end

function isequal(x::ZZAbsPowerSeriesRingElem, y::ZZAbsPowerSeriesRingElem)
  if parent(x) != parent(y)
    return false
  end
  if x.prec != y.prec || length(x) != length(y)
    return false
  end
  return Bool(@ccall libflint.fmpz_poly_equal(x::Ref{ZZAbsPowerSeriesRingElem}, y::Ref{ZZAbsPowerSeriesRingElem})::Cint)
end

###############################################################################
#
#   Ad hoc comparisons
#
###############################################################################

function ==(x::ZZAbsPowerSeriesRingElem, y::ZZRingElem)
  if length(x) > 1
    return false
  elseif length(x) == 1
    z = ZZRingElem()
    @ccall libflint.fmpz_poly_get_coeff_fmpz(z::Ref{ZZRingElem}, x::Ref{ZZAbsPowerSeriesRingElem}, 0::Int)::Nothing
    return @ccall libflint.fmpz_equal(z::Ref{ZZRingElem}, y::Ref{ZZRingElem}, 0::Int)::Bool
  else
    return precision(x) == 0 || iszero(y)
  end
end

==(x::ZZRingElem, y::ZZAbsPowerSeriesRingElem) = y == x

==(x::ZZAbsPowerSeriesRingElem, y::Integer) = x == ZZRingElem(y)

==(x::Integer, y::ZZAbsPowerSeriesRingElem) = y == x

###############################################################################
#
#   Exact division
#
###############################################################################

function divexact(x::ZZAbsPowerSeriesRingElem, y::ZZAbsPowerSeriesRingElem; check::Bool=true)
  check_parent(x, y)
  iszero(y) && throw(DivideError())
  v2 = valuation(y)
  v1 = valuation(x)
  if v2 != 0
    if check && v1 < v2
      error("Not an exact division")
    end
    x = shift_right(x, v2)
    y = shift_right(y, v2)
  end
  prec = min(x.prec, y.prec - v2 + v1)
  z = parent(x)()
  z.prec = prec
  @ccall libflint.fmpz_poly_div_series(z::Ref{ZZAbsPowerSeriesRingElem}, x::Ref{ZZAbsPowerSeriesRingElem}, y::Ref{ZZAbsPowerSeriesRingElem}, prec::Int)::Nothing
  return z
end

###############################################################################
#
#   Ad hoc exact division
#
###############################################################################

function divexact(x::ZZAbsPowerSeriesRingElem, y::Int; check::Bool=true)
  y == 0 && throw(DivideError())
  z = parent(x)()
  z.prec = x.prec
  @ccall libflint.fmpz_poly_scalar_divexact_si(z::Ref{ZZAbsPowerSeriesRingElem}, x::Ref{ZZAbsPowerSeriesRingElem}, y::Int)::Nothing
  return z
end

function divexact(x::ZZAbsPowerSeriesRingElem, y::ZZRingElem; check::Bool=true)
  iszero(y) && throw(DivideError())
  z = parent(x)()
  z.prec = x.prec
  @ccall libflint.fmpz_poly_scalar_divexact_fmpz(z::Ref{ZZAbsPowerSeriesRingElem}, x::Ref{ZZAbsPowerSeriesRingElem}, y::Ref{ZZRingElem})::Nothing
  return z
end

divexact(x::ZZAbsPowerSeriesRingElem, y::Integer; check::Bool=true) = divexact(x, ZZRingElem(y); check=check)

###############################################################################
#
#   Inversion
#
###############################################################################

function inv(a::ZZAbsPowerSeriesRingElem)
  iszero(a) && throw(DivideError())
  !is_unit(a) && error("Unable to invert power series")
  ainv = parent(a)()
  ainv.prec = a.prec
  @ccall libflint.fmpz_poly_inv_series(ainv::Ref{ZZAbsPowerSeriesRingElem}, a::Ref{ZZAbsPowerSeriesRingElem}, a.prec::Int)::Nothing
  return ainv
end

###############################################################################
#
#   Square root
#
###############################################################################

function Base.sqrt(a::ZZAbsPowerSeriesRingElem; check::Bool=true)
  asqrt = parent(a)()
  v = valuation(a)
  asqrt.prec = a.prec - div(v, 2)
  flag = Bool(@ccall libflint.fmpz_poly_sqrt_series(asqrt::Ref{ZZAbsPowerSeriesRingElem}, a::Ref{ZZAbsPowerSeriesRingElem}, a.prec::Int)::Cint)
  check && !flag && error("Not a square")
  return asqrt
end

###############################################################################
#
#   Unsafe functions
#
###############################################################################

function zero!(z::ZZAbsPowerSeriesRingElem)
  @ccall libflint.fmpz_poly_zero(z::Ref{ZZAbsPowerSeriesRingElem})::Nothing
  z.prec = parent(z).prec_max
  return z
end

function one!(z::ZZAbsPowerSeriesRingElem)
  @ccall libflint.fmpz_poly_one(z::Ref{ZZAbsPowerSeriesRingElem})::Nothing
  z.prec = parent(z).prec_max
  return z
end

function fit!(z::ZZAbsPowerSeriesRingElem, n::Int)
  @ccall libflint.fmpz_poly_fit_length(z::Ref{ZZAbsPowerSeriesRingElem}, n::Int)::Nothing
  return nothing
end

function setcoeff!(z::ZZAbsPowerSeriesRingElem, n::Int, x::ZZRingElem)
  @ccall libflint.fmpz_poly_set_coeff_fmpz(z::Ref{ZZAbsPowerSeriesRingElem}, n::Int, x::Ref{ZZRingElem})::Nothing
  return z
end

function mul!(z::ZZAbsPowerSeriesRingElem, a::ZZAbsPowerSeriesRingElem, b::ZZAbsPowerSeriesRingElem)
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
  @ccall libflint.fmpz_poly_mullow(z::Ref{ZZAbsPowerSeriesRingElem}, a::Ref{ZZAbsPowerSeriesRingElem}, b::Ref{ZZAbsPowerSeriesRingElem}, lenz::Int)::Nothing
  return z
end

function add!(c::ZZAbsPowerSeriesRingElem, a::ZZAbsPowerSeriesRingElem, b::ZZAbsPowerSeriesRingElem)
  lena = length(a)
  lenb = length(b)

  prec = min(a.prec, b.prec)

  lena = min(lena, prec)
  lenb = min(lenb, prec)

  lenc = max(lena, lenb)
  c.prec = prec
  @ccall libflint.fmpz_poly_add_series(c::Ref{ZZAbsPowerSeriesRingElem}, a::Ref{ZZAbsPowerSeriesRingElem}, b::Ref{ZZAbsPowerSeriesRingElem}, lenc::Int)::Nothing
  return c
end

function set_length!(a::ZZAbsPowerSeriesRingElem, n::Int)
  @ccall libflint._fmpz_poly_set_length(a::Ref{ZZAbsPowerSeriesRingElem}, n::Int)::Nothing
  return a
end

###############################################################################
#
#   Promotion rules
#
###############################################################################

promote_rule(::Type{ZZAbsPowerSeriesRingElem}, ::Type{T}) where {T <: Integer} = ZZAbsPowerSeriesRingElem

promote_rule(::Type{ZZAbsPowerSeriesRingElem}, ::Type{ZZRingElem}) = ZZAbsPowerSeriesRingElem

###############################################################################
#
#   Parent object call overload
#
###############################################################################

function (a::ZZAbsPowerSeriesRing)()
  z = ZZAbsPowerSeriesRingElem()
  z.prec = a.prec_max
  z.parent = a
  return z
end

function (a::ZZAbsPowerSeriesRing)(b::Integer)
  if b == 0
    z = ZZAbsPowerSeriesRingElem()
    z.prec = a.prec_max
  else
    z = ZZAbsPowerSeriesRingElem([ZZRingElem(b)], 1, a.prec_max)
  end
  z.parent = a
  return z
end

function (a::ZZAbsPowerSeriesRing)(b::ZZRingElem)
  if iszero(b)
    z = ZZAbsPowerSeriesRingElem()
    z.prec = a.prec_max
  else
    z = ZZAbsPowerSeriesRingElem([b], 1, a.prec_max)
  end
  z.parent = a
  return z
end

function (a::ZZAbsPowerSeriesRing)(b::ZZAbsPowerSeriesRingElem)
  parent(b) != a && error("Unable to coerce power series")
  return b
end

function (a::ZZAbsPowerSeriesRing)(b::Vector{ZZRingElem}, len::Int, prec::Int)
  z = ZZAbsPowerSeriesRingElem(b, len, prec)
  z.parent = a
  return z
end

###############################################################################
#
#   power_series_ring constructor
#
###############################################################################

function power_series_ring(R::ZZRing, prec::Int, s::VarName;  model::Symbol=:capped_relative, cached::Bool = true)
  if model == :capped_relative
    parent_obj = ZZRelPowerSeriesRing(prec, Symbol(s), cached)
  elseif model == :capped_absolute
    parent_obj = ZZAbsPowerSeriesRing(prec, Symbol(s), cached)
  else
    error("Unknown model")
  end

  return parent_obj, gen(parent_obj)
end

function AbsPowerSeriesRing(R::ZZRing, prec::Int)
  return ZZAbsPowerSeriesRing(prec, :x, false)
end

function RelPowerSeriesRing(R::ZZRing, prec::Int)
  return ZZRelPowerSeriesRing(prec, :x, false)
end
