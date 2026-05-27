###############################################################################
#
#   fq_abs_series.jl: Absolute series over finite fields
#
###############################################################################

###############################################################################
#
#   Data type and parent object methods
#
###############################################################################

function O(a::FqPolyRepAbsPowerSeriesRingElem)
  if iszero(a)
    return deepcopy(a)    # 0 + O(x^n)
  end
  prec = length(a) - 1
  prec < 0 && throw(DomainError(prec, "Valuation must be non-negative"))
  z = FqPolyRepAbsPowerSeriesRingElem(base_ring(a), Vector{FqPolyRepFieldElem}(undef, 0), 0, prec)
  z.parent = parent(a)
  return z
end

elem_type(::Type{FqPolyRepAbsPowerSeriesRing}) = FqPolyRepAbsPowerSeriesRingElem

parent_type(::Type{FqPolyRepAbsPowerSeriesRingElem}) = FqPolyRepAbsPowerSeriesRing

base_ring(R::FqPolyRepAbsPowerSeriesRing) = R.base_ring

abs_series_type(::Type{FqPolyRepFieldElem}) = FqPolyRepAbsPowerSeriesRingElem

var(a::FqPolyRepAbsPowerSeriesRing) = a.S

###############################################################################
#
#   Basic manipulation
#
###############################################################################

max_precision(R::FqPolyRepAbsPowerSeriesRing) = R.prec_max

function normalise(a::FqPolyRepAbsPowerSeriesRingElem, len::Int)
  len <= 0 && return len
  ctx = base_ring(a)
  c = base_ring(a)()
  @ccall libflint.fq_poly_get_coeff(c::Ref{FqPolyRepFieldElem}, a::Ref{FqPolyRepAbsPowerSeriesRingElem}, (len - 1)::Int, ctx::Ref{FqPolyRepField})::Nothing
  while len > 0 && iszero(c)
    len -= 1
    if len > 0
      @ccall libflint.fq_poly_get_coeff(c::Ref{FqPolyRepFieldElem}, a::Ref{FqPolyRepAbsPowerSeriesRingElem}, (len - 1)::Int, ctx::Ref{FqPolyRepField})::Nothing
    end
  end
  return len
end

length(x::FqPolyRepAbsPowerSeriesRingElem) = x.length

precision(x::FqPolyRepAbsPowerSeriesRingElem) = x.prec

function coeff(x::FqPolyRepAbsPowerSeriesRingElem, n::Int)
  if n < 0
    return base_ring(x)()
  end
  z = base_ring(x)()
  @ccall libflint.fq_poly_get_coeff(z::Ref{FqPolyRepFieldElem}, x::Ref{FqPolyRepAbsPowerSeriesRingElem}, n::Int, base_ring(x)::Ref{FqPolyRepField})::Nothing
  return z
end

zero(R::FqPolyRepAbsPowerSeriesRing) = R(0)

one(R::FqPolyRepAbsPowerSeriesRing) = R(1)

function gen(R::FqPolyRepAbsPowerSeriesRing)
  S = base_ring(R)
  z = FqPolyRepAbsPowerSeriesRingElem(S, [S(0), S(1)], 2, max_precision(R))
  z.parent = R
  return z
end

function deepcopy_internal(a::FqPolyRepAbsPowerSeriesRingElem, dict::IdDict)
  z = FqPolyRepAbsPowerSeriesRingElem(base_ring(a), a)
  z.prec = a.prec
  z.parent = parent(a)
  return z
end

function is_gen(a::FqPolyRepAbsPowerSeriesRingElem)
  return precision(a) == 0 || @ccall libflint.fq_poly_is_gen(a::Ref{FqPolyRepAbsPowerSeriesRingElem}, base_ring(a)::Ref{FqPolyRepField})::Bool
end

iszero(a::FqPolyRepAbsPowerSeriesRingElem) = length(a) == 0

is_unit(a::FqPolyRepAbsPowerSeriesRingElem) = valuation(a) == 0 && is_unit(coeff(a, 0))

function isone(a::FqPolyRepAbsPowerSeriesRingElem)
  return precision(a) == 0 || @ccall libflint.fq_poly_is_one(a::Ref{FqPolyRepAbsPowerSeriesRingElem}, base_ring(a)::Ref{FqPolyRepField})::Bool
end

# todo: write an fq_poly_valuation
function valuation(a::FqPolyRepAbsPowerSeriesRingElem)
  for i = 1:length(a)
    if !iszero(coeff(a, i - 1))
      return i - 1
    end
  end
  return precision(a)
end

characteristic(R::FqPolyRepAbsPowerSeriesRing) = characteristic(base_ring(R))

###############################################################################
#
#   Similar
#
###############################################################################

function similar(f::AbsPowerSeriesRingElem, R::FqPolyRepField, max_prec::Int,
    s::Symbol=var(parent(f)); cached::Bool=true)
  par = FqPolyRepAbsPowerSeriesRing(R, max_prec, s, cached)
  z = FqPolyRepAbsPowerSeriesRingElem(R)
  if base_ring(f) === R && s == var(parent(f)) &&
    f isa FqPolyRepAbsPowerSeriesRingElem && max_precision(parent(f)) == max_prec
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

function abs_series(R::FqPolyRepField, arr::Vector{T},
    len::Int, prec::Int, var::VarName=:x;
    max_precision::Int=prec, cached::Bool=true) where T
  prec < len && error("Precision too small for given data")
  coeffs = T == FqPolyRepFieldElem ? arr : map(R, arr)
  coeffs = length(coeffs) == 0 ? FqPolyRepFieldElem[] : coeffs
  par = FqPolyRepAbsPowerSeriesRing(R, max_precision, Symbol(var), cached)
  z = FqPolyRepAbsPowerSeriesRingElem(R, coeffs, len, prec)
  z.parent = par
  return z
end

###############################################################################
#
#   Unary operators
#
###############################################################################

function -(x::FqPolyRepAbsPowerSeriesRingElem)
  z = parent(x)()
  @ccall libflint.fq_poly_neg(z::Ref{FqPolyRepAbsPowerSeriesRingElem}, x::Ref{FqPolyRepAbsPowerSeriesRingElem}, base_ring(x)::Ref{FqPolyRepField})::Nothing
  z.prec = x.prec
  return z
end

###############################################################################
#
#   Binary operators
#
###############################################################################

function +(a::FqPolyRepAbsPowerSeriesRingElem, b::FqPolyRepAbsPowerSeriesRingElem)
  check_parent(a, b)
  lena = length(a)
  lenb = length(b)
  prec = min(a.prec, b.prec)
  lena = min(lena, prec)
  lenb = min(lenb, prec)
  lenz = max(lena, lenb)
  z = parent(a)()
  z.prec = prec
  @ccall libflint.fq_poly_add_series(z::Ref{FqPolyRepAbsPowerSeriesRingElem}, a::Ref{FqPolyRepAbsPowerSeriesRingElem}, b::Ref{FqPolyRepAbsPowerSeriesRingElem}, lenz::Int, base_ring(a)::Ref{FqPolyRepField})::Nothing
  return z
end

function -(a::FqPolyRepAbsPowerSeriesRingElem, b::FqPolyRepAbsPowerSeriesRingElem)
  check_parent(a, b)
  lena = length(a)
  lenb = length(b)
  prec = min(a.prec, b.prec)
  lena = min(lena, prec)
  lenb = min(lenb, prec)
  lenz = max(lena, lenb)
  z = parent(a)()
  z.prec = prec
  @ccall libflint.fq_poly_sub_series(z::Ref{FqPolyRepAbsPowerSeriesRingElem}, a::Ref{FqPolyRepAbsPowerSeriesRingElem}, b::Ref{FqPolyRepAbsPowerSeriesRingElem}, lenz::Int, base_ring(a)::Ref{FqPolyRepField})::Nothing
  return z
end

function *(a::FqPolyRepAbsPowerSeriesRingElem, b::FqPolyRepAbsPowerSeriesRingElem)
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
  @ccall libflint.fq_poly_mullow(z::Ref{FqPolyRepAbsPowerSeriesRingElem}, a::Ref{FqPolyRepAbsPowerSeriesRingElem}, b::Ref{FqPolyRepAbsPowerSeriesRingElem}, lenz::Int, base_ring(a)::Ref{FqPolyRepField})::Nothing
  return z
end

###############################################################################
#
#   Ad hoc binary operators
#
###############################################################################

function *(x::FqPolyRepFieldElem, y::FqPolyRepAbsPowerSeriesRingElem)
  z = parent(y)()
  z.prec = y.prec
  @ccall libflint.fq_poly_scalar_mul_fq(z::Ref{FqPolyRepAbsPowerSeriesRingElem}, y::Ref{FqPolyRepAbsPowerSeriesRingElem}, x::Ref{FqPolyRepFieldElem}, base_ring(y)::Ref{FqPolyRepField})::Nothing
  return z
end

*(x::FqPolyRepAbsPowerSeriesRingElem, y::FqPolyRepFieldElem) = y * x

###############################################################################
#
#   Shifting
#
###############################################################################

function shift_left(x::FqPolyRepAbsPowerSeriesRingElem, len::Int)
  len < 0 && throw(DomainError(len, "Shift must be non-negative"))
  xlen = length(x)
  z = parent(x)()
  z.prec = x.prec + len
  z.prec = min(z.prec, max_precision(parent(x)))
  zlen = min(z.prec, xlen + len)
  @ccall libflint.fq_poly_shift_left(z::Ref{FqPolyRepAbsPowerSeriesRingElem}, x::Ref{FqPolyRepAbsPowerSeriesRingElem}, len::Int, base_ring(x)::Ref{FqPolyRepField})::Nothing
  @ccall libflint.fq_poly_set_trunc(z::Ref{FqPolyRepAbsPowerSeriesRingElem}, z::Ref{FqPolyRepAbsPowerSeriesRingElem}, zlen::Int, base_ring(x)::Ref{FqPolyRepField})::Nothing
  return z
end

function shift_right(x::FqPolyRepAbsPowerSeriesRingElem, len::Int)
  len < 0 && throw(DomainError(len, "Shift must be non-negative"))
  xlen = length(x)
  z = parent(x)()
  if len >= xlen
    z.prec = max(0, x.prec - len)
  else
    z.prec = x.prec - len
    @ccall libflint.fq_poly_shift_right(z::Ref{FqPolyRepAbsPowerSeriesRingElem}, x::Ref{FqPolyRepAbsPowerSeriesRingElem}, len::Int, base_ring(x)::Ref{FqPolyRepField})::Nothing
  end
  return z
end

###############################################################################
#
#   Truncation
#
###############################################################################

function truncate(x::FqPolyRepAbsPowerSeriesRingElem, k::Int)
  return truncate!(deepcopy(x), k)
end

function truncate!(x::FqPolyRepAbsPowerSeriesRingElem, k::Int)
  k < 0 && throw(DomainError(k, "Index must be non-negative"))
  if precision(x) <= k
    return x
  end
  @ccall libflint.fq_poly_truncate(x::Ref{FqPolyRepAbsPowerSeriesRingElem}, k::Int, base_ring(x)::Ref{FqPolyRepField})::Nothing
  x.prec = k
  return x
end

###############################################################################
#
#   Powering
#
###############################################################################

function ^(a::FqPolyRepAbsPowerSeriesRingElem, b::Int)
  b < 0 && throw(DomainError(b, "Exponent must be non-negative"))
  if precision(a) > 0 && is_gen(a) && b > 0
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

function ==(x::FqPolyRepAbsPowerSeriesRingElem, y::FqPolyRepAbsPowerSeriesRingElem)
  check_parent(x, y)
  prec = min(x.prec, y.prec)
  n = max(length(x), length(y))
  n = min(n, prec)
  return Bool(@ccall libflint.fq_poly_equal_trunc(x::Ref{FqPolyRepAbsPowerSeriesRingElem}, y::Ref{FqPolyRepAbsPowerSeriesRingElem}, n::Int, base_ring(x)::Ref{FqPolyRepField})::Cint)
end

function isequal(x::FqPolyRepAbsPowerSeriesRingElem, y::FqPolyRepAbsPowerSeriesRingElem)
  if parent(x) != parent(y)
    return false
  end
  if x.prec != y.prec || length(x) != length(y)
    return false
  end
  return Bool(@ccall libflint.fq_poly_equal(x::Ref{FqPolyRepAbsPowerSeriesRingElem}, y::Ref{FqPolyRepAbsPowerSeriesRingElem}, base_ring(x)::Ref{FqPolyRepField})::Cint)
end

###############################################################################
#
#   Ad hoc comparisons
#
###############################################################################

function ==(x::FqPolyRepAbsPowerSeriesRingElem, y::FqPolyRepFieldElem)
  if length(x) > 1
    return false
  elseif length(x) == 1
    z = base_ring(x)()
    @ccall libflint.fq_poly_get_coeff(z::Ref{FqPolyRepFieldElem}, x::Ref{FqPolyRepAbsPowerSeriesRingElem}, 0::Int, base_ring(x)::Ref{FqPolyRepField})::Nothing
    return z == y
  else
    return precision(x) == 0 || iszero(y)
  end
end

==(x::FqPolyRepFieldElem, y::FqPolyRepAbsPowerSeriesRingElem) = y == x

function ==(x::FqPolyRepAbsPowerSeriesRingElem, y::ZZRingElem)
  if length(x) > 1
    return false
  elseif length(x) == 1
    z = base_ring(x)()
    @ccall libflint.fq_poly_get_coeff(z::Ref{FqPolyRepFieldElem}, x::Ref{FqPolyRepAbsPowerSeriesRingElem}, 0::Int, base_ring(x)::Ref{FqPolyRepField})::Nothing
    return z == y
  else
    return precision(x) == 0 || iszero(y)
  end
end

==(x::ZZRingElem, y::FqPolyRepAbsPowerSeriesRingElem) = y == x

==(x::FqPolyRepAbsPowerSeriesRingElem, y::Integer) = x == ZZRingElem(y)

==(x::Integer, y::FqPolyRepAbsPowerSeriesRingElem) = y == x

###############################################################################
#
#   Exact division
#
###############################################################################

function divexact(x::FqPolyRepAbsPowerSeriesRingElem, y::FqPolyRepAbsPowerSeriesRingElem; check::Bool=true)
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
  @ccall libflint.fq_poly_div_series(z::Ref{FqPolyRepAbsPowerSeriesRingElem}, x::Ref{FqPolyRepAbsPowerSeriesRingElem}, y::Ref{FqPolyRepAbsPowerSeriesRingElem}, prec::Int, base_ring(x)::Ref{FqPolyRepField})::Nothing
  return z
end

###############################################################################
#
#   Ad hoc exact division
#
###############################################################################

function divexact(x::FqPolyRepAbsPowerSeriesRingElem, y::FqPolyRepFieldElem; check::Bool=true)
  iszero(y) && throw(DivideError())
  z = parent(x)()
  z.prec = x.prec
  @ccall libflint.fq_poly_scalar_div_fq(z::Ref{FqPolyRepAbsPowerSeriesRingElem}, x::Ref{FqPolyRepAbsPowerSeriesRingElem}, y::Ref{FqPolyRepFieldElem}, base_ring(x)::Ref{FqPolyRepField})::Nothing
  return z
end

###############################################################################
#
#   Inversion
#
###############################################################################

function inv(a::FqPolyRepAbsPowerSeriesRingElem)
  iszero(a) && throw(DivideError())
  !is_unit(a) && error("Unable to invert power series")
  ainv = parent(a)()
  ainv.prec = a.prec
  @ccall libflint.fq_poly_inv_series(ainv::Ref{FqPolyRepAbsPowerSeriesRingElem}, a::Ref{FqPolyRepAbsPowerSeriesRingElem}, a.prec::Int, base_ring(a)::Ref{FqPolyRepField})::Nothing
  return ainv
end

###############################################################################
#
#   Square root
#
###############################################################################

function sqrt_classical_char2(a::FqPolyRepAbsPowerSeriesRingElem; check::Bool=true)
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
    if check
      flag, c = is_square_with_sqrt(coeff(a, 2*i))
      !flag && error("Not a square")
    else
      # degree of finite field could be > 1 so sqrt necessary here
      c = sqrt(coeff(a, 2*i); check=check)
    end
    asqrt = setcoeff!(asqrt, i, c)
  end
  return true, asqrt
end

function sqrt_classical(a::FqPolyRepAbsPowerSeriesRingElem; check::Bool=true)
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
    flag, s = is_square_with_sqrt(c)
    if !flag
      return false, S()
    end
  else
    s = sqrt(c; check=check)
  end
  a = divexact(a, c)
  z.prec = a.prec - div(v, 2)
  @ccall libflint.fq_poly_sqrt_series(z::Ref{FqPolyRepAbsPowerSeriesRingElem}, a::Ref{FqPolyRepAbsPowerSeriesRingElem}, a.prec::Int, base_ring(a)::Ref{FqPolyRepField})::Nothing
  if !isone(s)
    z *= s
  end
  if !iszero(v)
    z = shift_left(z, div(v, 2))
  end
  return true, z
end

function Base.sqrt(a::FqPolyRepAbsPowerSeriesRingElem; check::Bool=true)
  flag, s = sqrt_classical(a; check=check)
  check && !flag && error("Not a square")
  return s
end

function is_square(a::FqPolyRepAbsPowerSeriesRingElem)
  flag, s = sqrt_classical(a; check=true)
  return flag
end

function is_square_with_sqrt(a::FqPolyRepAbsPowerSeriesRingElem)
  return sqrt_classical(a; check=true)
end

###############################################################################
#
#   Unsafe functions
#
###############################################################################

function zero!(z::FqPolyRepAbsPowerSeriesRingElem)
  @ccall libflint.fq_poly_zero(z::Ref{FqPolyRepAbsPowerSeriesRingElem}, base_ring(z)::Ref{FqPolyRepField})::Nothing
  z.prec = parent(z).prec_max
  return z
end

function one!(z::FqPolyRepAbsPowerSeriesRingElem)
  @ccall libflint.fq_poly_one(z::Ref{FqPolyRepAbsPowerSeriesRingElem}, base_ring(z)::Ref{FqPolyRepField})::Nothing
  z.prec = parent(z).prec_max
  return z
end

function fit!(z::FqPolyRepAbsPowerSeriesRingElem, n::Int)
  @ccall libflint.fq_poly_fit_length(z::Ref{FqPolyRepAbsPowerSeriesRingElem}, n::Int, base_ring(z)::Ref{FqPolyRepField})::Nothing
  return nothing
end

function setcoeff!(z::FqPolyRepAbsPowerSeriesRingElem, n::Int, x::FqPolyRepFieldElem)
  @ccall libflint.fq_poly_set_coeff(z::Ref{FqPolyRepAbsPowerSeriesRingElem}, n::Int, x::Ref{FqPolyRepFieldElem}, base_ring(z)::Ref{FqPolyRepField})::Nothing
  return z
end

function mul!(z::FqPolyRepAbsPowerSeriesRingElem, a::FqPolyRepAbsPowerSeriesRingElem, b::FqPolyRepAbsPowerSeriesRingElem)
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
  @ccall libflint.fq_poly_mullow(z::Ref{FqPolyRepAbsPowerSeriesRingElem}, a::Ref{FqPolyRepAbsPowerSeriesRingElem}, b::Ref{FqPolyRepAbsPowerSeriesRingElem}, lenz::Int, base_ring(z)::Ref{FqPolyRepField})::Nothing
  return z
end

function add!(c::FqPolyRepAbsPowerSeriesRingElem, a::FqPolyRepAbsPowerSeriesRingElem, b::FqPolyRepAbsPowerSeriesRingElem)
  lena = length(a)
  lenb = length(b)
  prec = min(a.prec, b.prec)
  lena = min(lena, prec)
  lenb = min(lenb, prec)
  lenc = max(lena, lenb)
  c.prec = prec
  @ccall libflint.fq_poly_add_series(c::Ref{FqPolyRepAbsPowerSeriesRingElem}, a::Ref{FqPolyRepAbsPowerSeriesRingElem}, b::Ref{FqPolyRepAbsPowerSeriesRingElem}, lenc::Int, base_ring(a)::Ref{FqPolyRepField})::Nothing
  return c
end

function set_length!(a::FqPolyRepAbsPowerSeriesRingElem, n::Int)
  @ccall libflint._fq_poly_set_length(a::Ref{FqPolyRepAbsPowerSeriesRingElem}, n::Int, base_ring(a)::Ref{FqPolyRepField})::Nothing
  return a
end

###############################################################################
#
#   Promotion rules
#
###############################################################################

promote_rule(::Type{FqPolyRepAbsPowerSeriesRingElem}, ::Type{T}) where {T <: Integer} = FqPolyRepAbsPowerSeriesRingElem

promote_rule(::Type{FqPolyRepAbsPowerSeriesRingElem}, ::Type{FqPolyRepFieldElem}) = FqPolyRepAbsPowerSeriesRingElem

promote_rule(::Type{FqPolyRepAbsPowerSeriesRingElem}, ::Type{ZZRingElem}) = FqPolyRepAbsPowerSeriesRingElem

###############################################################################
#
#   Parent object call overload
#
###############################################################################

function (a::FqPolyRepAbsPowerSeriesRing)()
  ctx = base_ring(a)
  z = FqPolyRepAbsPowerSeriesRingElem(ctx)
  z.prec = a.prec_max
  z.parent = a
  return z
end

function (a::FqPolyRepAbsPowerSeriesRing)(b::Integer)
  return a(base_ring(a)(b))
end

function (a::FqPolyRepAbsPowerSeriesRing)(b::ZZRingElem)
  return a(base_ring(a)(b))
end

function (a::FqPolyRepAbsPowerSeriesRing)(b::FqPolyRepFieldElem)
  ctx = base_ring(a)
  if iszero(b)
    z = FqPolyRepAbsPowerSeriesRingElem(ctx)
    z.prec = a.prec_max
  else
    z = FqPolyRepAbsPowerSeriesRingElem(ctx, [b], 1, a.prec_max)
  end
  z.parent = a
  return z
end

function (a::FqPolyRepAbsPowerSeriesRing)(b::FqPolyRepAbsPowerSeriesRingElem)
  parent(b) != a && error("Unable to coerce power series")
  return b
end

function (a::FqPolyRepAbsPowerSeriesRing)(b::Vector{FqPolyRepFieldElem}, len::Int, prec::Int)
  ctx = base_ring(a)
  z = FqPolyRepAbsPowerSeriesRingElem(ctx, b, len, prec)
  z.parent = a
  return z
end


###############################################################################
#
#   power_series_ring constructor
#
###############################################################################

function power_series_ring(R::FqPolyRepField, prec::Int, s::VarName; model::Symbol=:capped_relative, cached::Bool = true)
  if model == :capped_relative
    parent_obj = FqPolyRepRelPowerSeriesRing(R, prec, Symbol(s), cached)
  elseif model == :capped_absolute
    parent_obj = FqPolyRepAbsPowerSeriesRing(R, prec, Symbol(s), cached)
  else
    error("Unknown model")
  end

  return parent_obj, gen(parent_obj)
end

function AbsPowerSeriesRing(R::FqPolyRepField, prec::Int)
  return FqPolyRepAbsPowerSeriesRing(R, prec, :x, false)
end

function RelPowerSeriesRing(R::FqPolyRepField, prec::Int)
  return FqPolyRepRelPowerSeriesRing(R, prec, :x, false)
end
