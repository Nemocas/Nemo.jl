###############################################################################
#
#   FqAbsPowerSeriesRingElem.jl : Power series over flint ZZRingElem integers
#
###############################################################################

###############################################################################
#
#   Data type and parent object methods
#
###############################################################################

function O(a::FqAbsPowerSeriesRingElem)
  if iszero(a)
    return deepcopy(a)    # 0 + O(x^n)
  end
  prec = length(a) - 1
  prec < 0 && throw(DomainError(prec, "Valuation must be non-negative"))
  z = FqAbsPowerSeriesRingElem(base_ring(a), Vector{FqFieldElem}(undef, 0), 0, prec)
  z.parent = parent(a)
  return z
end

elem_type(::Type{FqAbsPowerSeriesRing}) = FqAbsPowerSeriesRingElem

parent_type(::Type{FqAbsPowerSeriesRingElem}) = FqAbsPowerSeriesRing

base_ring(R::FqAbsPowerSeriesRing) = R.base_ring

abs_series_type(::Type{FqFieldElem}) = FqAbsPowerSeriesRingElem

var(a::FqAbsPowerSeriesRing) = a.S

###############################################################################
#
#   Basic manipulation
#
###############################################################################

max_precision(R::FqAbsPowerSeriesRing) = R.prec_max

function normalise(a::FqAbsPowerSeriesRingElem, len::Int)
  ctx = base_ring(a)
  if len > 0
    c = base_ring(a)()
    @ccall libflint.fq_default_poly_get_coeff(c::Ref{FqFieldElem}, a::Ref{FqAbsPowerSeriesRingElem}, (len - 1)::Int, ctx::Ref{FqField})::Nothing
  end
  while len > 0 && iszero(c)
    len -= 1
    if len > 0
      @ccall libflint.fq_default_poly_get_coeff(c::Ref{FqFieldElem}, a::Ref{FqAbsPowerSeriesRingElem}, (len - 1)::Int, ctx::Ref{FqField})::Nothing
    end
  end

  return len
end

function length(x::FqAbsPowerSeriesRingElem)
  return @ccall libflint.fq_default_poly_length(x::Ref{FqAbsPowerSeriesRingElem}, base_ring(x)::Ref{FqField})::Int
end

precision(x::FqAbsPowerSeriesRingElem) = x.prec

function coeff(x::FqAbsPowerSeriesRingElem, n::Int)
  if n < 0
    return base_ring(x)()
  end
  z = base_ring(x)()
  @ccall libflint.fq_default_poly_get_coeff(z::Ref{FqFieldElem}, x::Ref{FqAbsPowerSeriesRingElem}, n::Int, base_ring(x)::Ref{FqField})::Nothing
  return z
end

zero(R::FqAbsPowerSeriesRing) = R(0)

one(R::FqAbsPowerSeriesRing) = R(1)

function gen(R::FqAbsPowerSeriesRing)
  S = base_ring(R)
  z = FqAbsPowerSeriesRingElem(S, [S(0), S(1)], 2, max_precision(R))
  z.parent = R
  return z
end

function deepcopy_internal(a::FqAbsPowerSeriesRingElem, dict::IdDict)
  z = FqAbsPowerSeriesRingElem(base_ring(a), a)
  z.prec = a.prec
  z.parent = parent(a)
  return z
end

function is_gen(a::FqAbsPowerSeriesRingElem)
  return precision(a) == 0 || @ccall libflint.fq_default_poly_is_gen(a::Ref{FqAbsPowerSeriesRingElem}, base_ring(a)::Ref{FqField})::Bool
end

iszero(a::FqAbsPowerSeriesRingElem) = length(a) == 0

is_unit(a::FqAbsPowerSeriesRingElem) = valuation(a) == 0 && is_unit(coeff(a, 0))

function isone(a::FqAbsPowerSeriesRingElem)
  return precision(a) == 0 || @ccall libflint.fq_default_poly_is_one(a::Ref{FqAbsPowerSeriesRingElem}, base_ring(a)::Ref{FqField})::Bool
end

# todo: write an fq_default_poly_valuation
function valuation(a::FqAbsPowerSeriesRingElem)
  for i = 1:length(a)
    if !iszero(coeff(a, i - 1))
      return i - 1
    end
  end
  return precision(a)
end

characteristic(R::FqAbsPowerSeriesRing) = characteristic(base_ring(R))

function set_precision!(z::FqAbsPowerSeriesRingElem, k::Int)
  k < 0 && throw(DomainError(k, "Precision must be non-negative"))
  z = truncate!(z, k)
  z.prec = k
  return z
end

###############################################################################
#
#   Similar
#
###############################################################################

function similar(f::AbsPowerSeriesRingElem, R::FqField, max_prec::Int,
    var::Symbol=var(parent(f)); cached::Bool=true)
  z = FqAbsPowerSeriesRingElem(R)
  z.parent = FqAbsPowerSeriesRing(R, max_prec, var, cached)
  z.prec = max_prec
  return z
end

###############################################################################
#
#   abs_series constructor
#
###############################################################################

function abs_series(R::FqField, arr::Vector{T},
    len::Int, prec::Int, var::VarName=:x;
    max_precision::Int=prec, cached::Bool=true) where T
  prec < len && error("Precision too small for given data")
  coeffs = T == FqFieldElem ? arr : map(R, arr)
  coeffs = length(coeffs) == 0 ? FqFieldElem[] : coeffs
  z = FqAbsPowerSeriesRingElem(R, coeffs, len, prec)
  z.parent = FqAbsPowerSeriesRing(R, max_precision, Symbol(var), cached)
  return z
end

###############################################################################
#
#   Unary operators
#
###############################################################################

-(x::FqAbsPowerSeriesRingElem) = neg!(parent(x)(), x)

###############################################################################
#
#   Binary operators
#
###############################################################################

function +(a::FqAbsPowerSeriesRingElem, b::FqAbsPowerSeriesRingElem)
  check_parent(a, b)
  lena = length(a)
  lenb = length(b)
  prec = min(a.prec, b.prec)
  lena = min(lena, prec)
  lenb = min(lenb, prec)
  lenz = max(lena, lenb)
  z = parent(a)()
  z.prec = prec
  @ccall libflint.fq_default_poly_add_series(z::Ref{FqAbsPowerSeriesRingElem}, a::Ref{FqAbsPowerSeriesRingElem}, b::Ref{FqAbsPowerSeriesRingElem}, lenz::Int, base_ring(a)::Ref{FqField})::Nothing
  return z
end

function -(a::FqAbsPowerSeriesRingElem, b::FqAbsPowerSeriesRingElem)
  check_parent(a, b)
  lena = length(a)
  lenb = length(b)
  prec = min(a.prec, b.prec)
  lena = min(lena, prec)
  lenb = min(lenb, prec)
  lenz = max(lena, lenb)
  z = parent(a)()
  z.prec = prec
  @ccall libflint.fq_default_poly_sub_series(z::Ref{FqAbsPowerSeriesRingElem}, a::Ref{FqAbsPowerSeriesRingElem}, b::Ref{FqAbsPowerSeriesRingElem}, lenz::Int, base_ring(a)::Ref{FqField})::Nothing
  return z
end

function *(a::FqAbsPowerSeriesRingElem, b::FqAbsPowerSeriesRingElem)
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
  @ccall libflint.fq_default_poly_mullow(z::Ref{FqAbsPowerSeriesRingElem}, a::Ref{FqAbsPowerSeriesRingElem}, b::Ref{FqAbsPowerSeriesRingElem}, lenz::Int, base_ring(a)::Ref{FqField})::Nothing
  return z
end

###############################################################################
#
#   Ad hoc binary operators
#
###############################################################################

function *(x::FqFieldElem, y::FqAbsPowerSeriesRingElem)
  z = parent(y)()
  z.prec = y.prec
  @ccall libflint.fq_default_poly_scalar_mul_fq_default(z::Ref{FqAbsPowerSeriesRingElem}, y::Ref{FqAbsPowerSeriesRingElem}, x::Ref{FqFieldElem}, base_ring(y)::Ref{FqField})::Nothing
  return z
end

*(x::FqAbsPowerSeriesRingElem, y::FqFieldElem) = y * x

*(x::ZZRingElem, y::FqAbsPowerSeriesRingElem) = base_ring(parent(y))(x) * y

*(x::FqAbsPowerSeriesRingElem, y::ZZRingElem) = y * x

*(x::Integer, y::FqAbsPowerSeriesRingElem) = base_ring(parent(y))(x) * y

*(x::FqAbsPowerSeriesRingElem, y::Integer) = y * x

+(x::FqFieldElem, y::FqAbsPowerSeriesRingElem) = parent(y)(x) + y

+(x::FqAbsPowerSeriesRingElem, y::FqFieldElem) = y + x

+(x::ZZRingElem, y::FqAbsPowerSeriesRingElem) = base_ring(parent(y))(x) + y

+(x::FqAbsPowerSeriesRingElem, y::ZZRingElem) = y + x

+(x::FqAbsPowerSeriesRingElem, y::Integer) = x + base_ring(parent(x))(y)

+(x::Integer, y::FqAbsPowerSeriesRingElem) = y + x

-(x::FqFieldElem, y::FqAbsPowerSeriesRingElem) = parent(y)(x) - y

-(x::FqAbsPowerSeriesRingElem, y::FqFieldElem) = x - parent(x)(y)

-(x::ZZRingElem, y::FqAbsPowerSeriesRingElem) = base_ring(parent(y))(x) - y

-(x::FqAbsPowerSeriesRingElem, y::ZZRingElem) = x - base_ring(parent(x))(y)

-(x::FqAbsPowerSeriesRingElem, y::Integer) = x - base_ring(parent(x))(y)

-(x::Integer, y::FqAbsPowerSeriesRingElem) = base_ring(parent(y))(x) - y

###############################################################################
#
#   Shifting
#
###############################################################################

function shift_left(x::FqAbsPowerSeriesRingElem, len::Int)
  len < 0 && throw(DomainError(len, "Shift must be non-negative"))
  xlen = length(x)
  z = parent(x)()
  z.prec = x.prec + len
  z.prec = min(z.prec, max_precision(parent(x)))
  zlen = min(z.prec, xlen + len)
  @ccall libflint.fq_default_poly_shift_left(z::Ref{FqAbsPowerSeriesRingElem}, x::Ref{FqAbsPowerSeriesRingElem}, len::Int, base_ring(x)::Ref{FqField})::Nothing
  @ccall libflint.fq_default_poly_set_trunc(z::Ref{FqAbsPowerSeriesRingElem}, z::Ref{FqAbsPowerSeriesRingElem}, zlen::Int, base_ring(x)::Ref{FqField})::Nothing
  return z
end

function shift_right(x::FqAbsPowerSeriesRingElem, len::Int)
  len < 0 && throw(DomainError(len, "Shift must be non-negative"))
  xlen = length(x)
  z = parent(x)()
  if len >= xlen
    z.prec = max(0, x.prec - len)
  else
    z.prec = x.prec - len
    @ccall libflint.fq_default_poly_shift_right(z::Ref{FqAbsPowerSeriesRingElem}, x::Ref{FqAbsPowerSeriesRingElem}, len::Int, base_ring(x)::Ref{FqField})::Nothing
  end
  return z
end

###############################################################################
#
#   Truncation
#
###############################################################################

function truncate(x::FqAbsPowerSeriesRingElem, k::Int)
  return truncate!(deepcopy(x), k)
end

function truncate!(x::FqAbsPowerSeriesRingElem, k::Int)
  k < 0 && throw(DomainError(k, "Index must be non-negative"))
  if precision(x) <= k
    return x
  end
  @ccall libflint.fq_default_poly_truncate(x::Ref{FqAbsPowerSeriesRingElem}, k::Int, base_ring(x)::Ref{FqField})::Nothing
  x.prec = k
  return x
end

###############################################################################
#
#   Powering
#
###############################################################################

function ^(a::FqAbsPowerSeriesRingElem, b::Int)
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

function ==(x::FqAbsPowerSeriesRingElem, y::FqAbsPowerSeriesRingElem)
  check_parent(x, y)
  prec = min(x.prec, y.prec)
  n = max(length(x), length(y))
  n = min(n, prec)
  return Bool(@ccall libflint.fq_default_poly_equal_trunc(x::Ref{FqAbsPowerSeriesRingElem}, y::Ref{FqAbsPowerSeriesRingElem}, n::Int, base_ring(x)::Ref{FqField})::Cint)
end

function isequal(x::FqAbsPowerSeriesRingElem, y::FqAbsPowerSeriesRingElem)
  if parent(x) != parent(y)
    return false
  end
  if x.prec != y.prec || length(x) != length(y)
    return false
  end
  return Bool(@ccall libflint.fq_default_poly_equal(x::Ref{FqAbsPowerSeriesRingElem}, y::Ref{FqAbsPowerSeriesRingElem}, base_ring(x)::Ref{FqField})::Cint)
end

###############################################################################
#
#   Ad hoc comparisons
#
###############################################################################

function ==(x::FqAbsPowerSeriesRingElem, y::FqFieldElem)
  if length(x) > 1
    return false
  elseif length(x) == 1
    z = base_ring(x)()
    @ccall libflint.fq_default_poly_get_coeff(z::Ref{FqFieldElem}, x::Ref{FqAbsPowerSeriesRingElem}, 0::Int, base_ring(x)::Ref{FqField})::Nothing
    return z == y
  else
    return precision(x) == 0 || iszero(y)
  end
end

==(x::FqFieldElem, y::FqAbsPowerSeriesRingElem) = y == x

function ==(x::FqAbsPowerSeriesRingElem, y::ZZRingElem)
  if length(x) > 1
    return false
  elseif length(x) == 1
    z = base_ring(x)()
    @ccall libflint.fq_default_poly_get_coeff(z::Ref{FqFieldElem}, x::Ref{FqAbsPowerSeriesRingElem}, 0::Int, base_ring(x)::Ref{FqField})::Nothing
    return z == y
  else
    return precision(x) == 0 || iszero(y)
  end
end

==(x::ZZRingElem, y::FqAbsPowerSeriesRingElem) = y == x

==(x::FqAbsPowerSeriesRingElem, y::Integer) = x == ZZRingElem(y)

==(x::Integer, y::FqAbsPowerSeriesRingElem) = y == x

###############################################################################
#
#   Exact division
#
###############################################################################

function divexact(x::FqAbsPowerSeriesRingElem, y::FqAbsPowerSeriesRingElem; check::Bool=true)
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
  @ccall libflint.fq_default_poly_div_series(z::Ref{FqAbsPowerSeriesRingElem}, x::Ref{FqAbsPowerSeriesRingElem}, y::Ref{FqAbsPowerSeriesRingElem}, prec::Int, base_ring(x)::Ref{FqField})::Nothing
  return z
end

###############################################################################
#
#   Ad hoc exact division
#
###############################################################################

function divexact(x::FqAbsPowerSeriesRingElem, y::FqFieldElem; check::Bool=true)
  iszero(y) && throw(DivideError())
  z = parent(x)()
  z.prec = x.prec
  @ccall libflint.fq_default_poly_scalar_div_fq_default(z::Ref{FqAbsPowerSeriesRingElem}, x::Ref{FqAbsPowerSeriesRingElem}, y::Ref{FqFieldElem}, base_ring(x)::Ref{FqField})::Nothing
  return z
end

###############################################################################
#
#   Inversion
#
###############################################################################

function inv(a::FqAbsPowerSeriesRingElem)
  iszero(a) && throw(DivideError())
  !is_unit(a) && error("Unable to invert power series")
  ainv = parent(a)()
  ainv.prec = a.prec
  @ccall libflint.fq_default_poly_inv_series(ainv::Ref{FqAbsPowerSeriesRingElem}, a::Ref{FqAbsPowerSeriesRingElem}, a.prec::Int, base_ring(a)::Ref{FqField})::Nothing
  return ainv
end

###############################################################################
#
#   Square root
#
###############################################################################

function sqrt_classical_char2(a::FqAbsPowerSeriesRingElem; check::Bool=true)
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

function sqrt_classical(a::FqAbsPowerSeriesRingElem; check::Bool=true)
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
  @ccall libflint.fq_default_poly_sqrt_series(z::Ref{FqAbsPowerSeriesRingElem}, a::Ref{FqAbsPowerSeriesRingElem}, a.prec::Int, base_ring(a)::Ref{FqField})::Nothing
  if !isone(s)
    z *= s
  end
  if !iszero(v)
    z = shift_left(z, div(v, 2))
  end
  return true, z
end

function Base.sqrt(a::FqAbsPowerSeriesRingElem; check::Bool=true)
  flag, s = sqrt_classical(a; check=check)
  check && !flag && error("Not a square")
  return s
end

function is_square(a::FqAbsPowerSeriesRingElem)
  flag, s = sqrt_classical(a; check=true)
  return flag
end

function is_square_with_sqrt(a::FqAbsPowerSeriesRingElem)
  return sqrt_classical(a; check=true)
end

###############################################################################
#
#   Unsafe functions
#
###############################################################################

function zero!(z::FqAbsPowerSeriesRingElem)
  @ccall libflint.fq_default_poly_zero(z::Ref{FqAbsPowerSeriesRingElem}, base_ring(z)::Ref{FqField})::Nothing
  z.prec = parent(z).prec_max
  return z
end

function one!(z::FqAbsPowerSeriesRingElem)
  @ccall libflint.fq_default_poly_one(z::Ref{FqAbsPowerSeriesRingElem}, base_ring(z)::Ref{FqField})::Nothing
  z.prec = parent(z).prec_max
  return z
end

function neg!(z::FqAbsPowerSeriesRingElem, a::FqAbsPowerSeriesRingElem)
  @ccall libflint.fq_default_poly_neg(z::Ref{FqAbsPowerSeriesRingElem}, a::Ref{FqAbsPowerSeriesRingElem}, base_ring(a)::Ref{FqField})::Nothing
  z.prec = a.prec
  return z
end

function fit!(z::FqAbsPowerSeriesRingElem, n::Int)
  @ccall libflint.fq_default_poly_fit_length(z::Ref{FqAbsPowerSeriesRingElem}, n::Int, base_ring(z)::Ref{FqField})::Nothing
  return nothing
end

function setcoeff!(z::FqAbsPowerSeriesRingElem, n::Int, x::FqFieldElem)
  @ccall libflint.fq_default_poly_set_coeff(z::Ref{FqAbsPowerSeriesRingElem}, n::Int, x::Ref{FqFieldElem}, base_ring(z)::Ref{FqField})::Nothing
  return z
end

function mul!(z::FqAbsPowerSeriesRingElem, a::FqAbsPowerSeriesRingElem, b::FqAbsPowerSeriesRingElem)
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
  @ccall libflint.fq_default_poly_mullow(z::Ref{FqAbsPowerSeriesRingElem}, a::Ref{FqAbsPowerSeriesRingElem}, b::Ref{FqAbsPowerSeriesRingElem}, lenz::Int, base_ring(z)::Ref{FqField})::Nothing
  return z
end

function add!(a::FqAbsPowerSeriesRingElem, b::FqAbsPowerSeriesRingElem)
  lena = length(a)
  lenb = length(b)
  prec = min(a.prec, b.prec)
  lena = min(lena, prec)
  lenb = min(lenb, prec)
  lenz = max(lena, lenb)
  a.prec = prec
  @ccall libflint.fq_default_poly_add_series(a::Ref{FqAbsPowerSeriesRingElem}, a::Ref{FqAbsPowerSeriesRingElem}, b::Ref{FqAbsPowerSeriesRingElem}, lenz::Int, base_ring(a)::Ref{FqField})::Nothing
  return a
end

function add!(c::FqAbsPowerSeriesRingElem, a::FqAbsPowerSeriesRingElem, b::FqAbsPowerSeriesRingElem)
  if c === a
    return add!(c, b)
  elseif c === b
    return add!(c, a)
  end
  lena = length(a)
  lenb = length(b)
  prec = min(a.prec, b.prec)
  lena = min(lena, prec)
  lenb = min(lenb, prec)
  lenc = max(lena, lenb)
  c.prec = prec
  @ccall libflint.fq_default_poly_add_series(c::Ref{FqAbsPowerSeriesRingElem}, a::Ref{FqAbsPowerSeriesRingElem}, b::Ref{FqAbsPowerSeriesRingElem}, lenc::Int, base_ring(a)::Ref{FqField})::Nothing
  return c
end

function set_length!(a::FqAbsPowerSeriesRingElem, n::Int64)
  @ccall libflint._fq_default_poly_set_length(a::Ref{FqAbsPowerSeriesRingElem}, n::Int, base_ring(a)::Ref{FqField})::Nothing
  return a
end

###############################################################################
#
#   Promotion rules
#
###############################################################################

promote_rule(::Type{FqAbsPowerSeriesRingElem}, ::Type{T}) where {T <: Integer} = FqAbsPowerSeriesRingElem

promote_rule(::Type{FqAbsPowerSeriesRingElem}, ::Type{FqFieldElem}) = FqAbsPowerSeriesRingElem

promote_rule(::Type{FqAbsPowerSeriesRingElem}, ::Type{ZZRingElem}) = FqAbsPowerSeriesRingElem

###############################################################################
#
#   Parent object call overload
#
###############################################################################

function (a::FqAbsPowerSeriesRing)()
  ctx = base_ring(a)
  z = FqAbsPowerSeriesRingElem(ctx)
  z.prec = a.prec_max
  z.parent = a
  return z
end

function (a::FqAbsPowerSeriesRing)(b::Integer)
  return a(base_ring(a)(b))
end

function (a::FqAbsPowerSeriesRing)(b::ZZRingElem)
  return a(base_ring(a)(b))
end

function (a::FqAbsPowerSeriesRing)(b::FqFieldElem)
  ctx = base_ring(a)
  if iszero(b)
    z = FqAbsPowerSeriesRingElem(ctx)
    z.prec = a.prec_max
  else
    z = FqAbsPowerSeriesRingElem(ctx, [b], 1, a.prec_max)
  end
  z.parent = a
  return z
end

function (a::FqAbsPowerSeriesRing)(b::FqAbsPowerSeriesRingElem)
  parent(b) != a && error("Unable to coerce power series")
  return b
end

function (a::FqAbsPowerSeriesRing)(b::Vector{FqFieldElem}, len::Int, prec::Int)
  ctx = base_ring(a)
  z = FqAbsPowerSeriesRingElem(ctx, b, len, prec)
  z.parent = a
  return z
end

###############################################################################
#
#   power_series_ring constructor
#
###############################################################################

function power_series_ring(R::FqField, prec::Int, s::VarName; model::Symbol=:capped_relative, cached::Bool = true)
  S = Symbol(s)

  if model == :capped_relative
    parent_obj = FqRelPowerSeriesRing(R, prec, S, cached)
  elseif model == :capped_absolute
    parent_obj = FqAbsPowerSeriesRing(R, prec, S, cached)
  else
    error("Unknown model")
  end

  return parent_obj, gen(parent_obj)
end
