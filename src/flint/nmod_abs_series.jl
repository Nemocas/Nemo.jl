###############################################################################
#
#   nmod_abs_series.jl: Absolute series using nmod_poly
#
#   nmod_abs_series, gfp_abs_series
#
###############################################################################

for (etype, rtype, mtype, brtype) in (
                                      (zzModAbsPowerSeriesRingElem, zzModAbsPowerSeriesRing, zzModRingElem, zzModRing),
                                      (fpAbsPowerSeriesRingElem, fpAbsPowerSeriesRing, fpFieldElem, fpField))
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
        c = @ccall libflint.nmod_poly_get_coeff_ui(a::Ref{($etype)}, (len - 1)::Int)::UInt
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
      return @ccall libflint.nmod_poly_get_coeff_ui(x::Ref{($etype)}, n::Int)::UInt
    end

    function coeff(x::($etype), n::Int)
      R = base_ring(x)
      z = coeff_raw(x, n)
      return R(z)
    end

    zero(R::($rtype)) = R(0)

    one(R::($rtype)) = R(1)

    function gen(R::($rtype))
      z = ($etype)(R.n, [ZZRingElem(0), ZZRingElem(1)], 2, max_precision(R))
      z.parent = R
      return z
    end

    function deepcopy_internal(a::($etype), dict::IdDict)
      z = ($etype)(a)
      z.parent = parent(a)
      return z
    end

    function is_gen(a::($etype))
      return precision(a) == 0 ||
      (length(a) == 2 && isone(coeff(a, 1)) && iszero(coeff(a, 0)))
    end

    iszero(a::($etype)) = length(a) == 0

    is_unit(a::($etype)) = valuation(a) == 0 && is_unit(coeff(a, 0))

    function isone(a::($etype))
      return precision(a) == 0 ||
      Bool(@ccall libflint.nmod_poly_is_one(a::Ref{($etype)})::Cint)
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

    function similar(f::AbsPowerSeriesRingElem, R::($brtype), max_prec::Int,
        s::Symbol=var(parent(f)); cached::Bool=true)
      par = ($rtype)(R, max_prec, s, cached)
      z = ($etype)(par.n)
      if base_ring(f) === R && s == var(parent(f)) &&
        f isa ($etype) && max_precision(parent(f)) == max_prec
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
        len::Int, prec::Int, var::VarName=:x;
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
    #   Unary operators
    #
    ###############################################################################

    -(x::($etype)) = neg!(parent(x)(), x)

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
      @ccall libflint.nmod_poly_add_series(z::Ref{($etype)}, a::Ref{($etype)}, b::Ref{($etype)}, lenz::Int)::Nothing
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
      @ccall libflint.nmod_poly_sub_series(z::Ref{($etype)}, a::Ref{($etype)}, b::Ref{($etype)}, lenz::Int)::Nothing
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

      @ccall libflint.nmod_poly_mullow(z::Ref{($etype)}, a::Ref{($etype)}, b::Ref{($etype)}, lenz::Int)::Nothing
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
      @ccall libflint.nmod_poly_scalar_mul_nmod(z::Ref{($etype)}, y::Ref{($etype)}, x.data::UInt)::Nothing
      return z
    end

    *(x::($etype), y::$(mtype)) = y*x

    function *(x::ZZRingElem, y::($etype))
      R = base_ring(y)
      xmod = @ccall libflint.fmpz_fdiv_ui(x::Ref{ZZRingElem}, R.n::UInt)::UInt
      return R(xmod)*y
    end

    *(x::($etype), y::ZZRingElem) = y*x

    *(x::Integer, y::($etype)) = ZZRingElem(x)*y

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
      @ccall libflint.nmod_poly_shift_left(z::Ref{($etype)}, x::Ref{($etype)}, len::Int)::Nothing
      @ccall libflint.nmod_poly_set_trunc(z::Ref{($etype)}, z::Ref{($etype)}, zlen::Int)::Nothing
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
        @ccall libflint.nmod_poly_shift_right(z::Ref{($etype)}, x::Ref{($etype)}, len::Int)::Nothing
      end
      return z
    end

    ###############################################################################
    #
    #   Truncation
    #
    ###############################################################################

    function truncate(x::($etype), k::Int)
      return truncate!(deepcopy(x), k)
    end

    function truncate!(x::($etype), k::Int)
      k < 0 && throw(DomainError(k, "Index must be non-negative"))
      if precision(x) <= k
        return x
      end
      @ccall libflint.nmod_poly_truncate(x::Ref{($etype)}, k::Int)::Nothing
      x.prec = k
      return x
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
        @ccall libflint.nmod_poly_pow_trunc(z::Ref{($etype)}, a::Ref{($etype)}, b::UInt, z.prec::Int)::Nothing
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

      return Bool(@ccall libflint.nmod_poly_equal_trunc(x::Ref{($etype)}, y::Ref{($etype)}, n::Int)::Cint)
    end

    function isequal(x::($etype), y::($etype))
      if parent(x) != parent(y)
        return false
      end
      if x.prec != y.prec || length(x) != length(y)
        return false
      end
      return Bool(@ccall libflint.nmod_poly_equal(x::Ref{($etype)}, y::Ref{($etype)}, length(x)::Int)::Cint)
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
        z = @ccall libflint.nmod_poly_get_coeff_ui(x::Ref{($etype)}, 0::Int)::UInt
        return z == y.data
      else
        return precision(x) == 0 || iszero(y)
      end
    end

    ==(x::$(mtype), y::($etype)) = y == x

    function ==(x::($etype), y::ZZRingElem)
      R = base_ring(x)
      ymod = @ccall libflint.fmpz_fdiv_ui(y::Ref{ZZRingElem}, modulus(x)::UInt)::UInt
      return x == R(ymod)
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
      @ccall libflint.nmod_poly_div_series(z::Ref{($etype)}, x::Ref{($etype)}, y::Ref{($etype)}, prec::Int)::Nothing
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
      yinv = inv(y)
      @ccall libflint.nmod_poly_scalar_mul_nmod(z::Ref{($etype)}, x::Ref{($etype)}, yinv.data::UInt)::Nothing
      return z
    end

    function divexact(x::($etype), y::ZZRingElem; check::Bool=true)
      R = base_ring(x)
      return divexact(x, R(y))
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
      @ccall libflint.nmod_poly_inv_series(ainv::Ref{($etype)}, a::Ref{($etype)}, a.prec::Int)::Nothing
      return ainv
    end

    ###############################################################################
    #
    #   Unsafe functions
    #
    ###############################################################################

    function zero!(z::($etype))
      @ccall libflint.nmod_poly_zero(z::Ref{($etype)})::Nothing
      z.prec = parent(z).prec_max
      return z
    end

    function one!(z::($etype))
      @ccall libflint.nmod_poly_one(z::Ref{($etype)})::Nothing
      z.prec = parent(z).prec_max
      return z
    end

    function neg!(z::($etype), x::($etype))
      @ccall libflint.nmod_poly_neg(z::Ref{($etype)}, x::Ref{($etype)})::Nothing
      z.prec = x.prec
      return z
    end

    function fit!(z::($etype), n::Int)
      @ccall libflint.nmod_poly_fit_length(z::Ref{($etype)}, n::Int)::Nothing
      return nothing
    end

    function setcoeff!(z::($etype), n::Int, x::($mtype))
      @ccall libflint.nmod_poly_set_coeff_ui(z::Ref{($etype)}, n::Int, x.data::UInt)::Nothing
      return z
    end

    function setcoeff!(z::($etype), n::Int, x::ZZRingElem)
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
      @ccall libflint.nmod_poly_mullow(z::Ref{($etype)}, a::Ref{($etype)}, b::Ref{($etype)}, lenz::Int)::Nothing
      return z
    end

    function add!(c::($etype), a::($etype), b::($etype))
      lena = length(a)
      lenb = length(b)

      prec = min(a.prec, b.prec)

      lena = min(lena, prec)
      lenb = min(lenb, prec)

      lenc = max(lena, lenb)
      c.prec = prec
      @ccall libflint.nmod_poly_add_series(c::Ref{($etype)}, a::Ref{($etype)}, b::Ref{($etype)}, lenc::Int)::Nothing
      return c
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

    function (a::($rtype))(b::ZZRingElem)
      R = base_ring(a)
      return a(R(b))
    end

    function (a::($rtype))(b::Integer)
      return a(ZZRingElem(b))
    end

    function (a::($rtype))(b::($etype))
      parent(b) != a && error("Unable to coerce power series")
      return b
    end

    function (a::($rtype))(b::Vector{ZZRingElem}, len::Int, prec::Int)
      z = ($etype)(a.n, b, len, prec)
      z.parent = a
      return z
    end

    function (a::($rtype))(b::Vector{UInt}, len::Int, prec::Int)
      z = ($etype)(a.n, b, len, prec)
      z.parent = a
      return z
    end

    function (a::($rtype))(b::Vector{($mtype)}, len::Int, prec::Int)
      @req all(parent(e) == base_ring(a) for e in b) "parents do not match"
      z = ($etype)(a.n, b, len, prec)
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

function sqrt_classical_char2(a::fpAbsPowerSeriesRingElem; check::Bool=true)
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

function sqrt_classical(a::fpAbsPowerSeriesRingElem; check::Bool=true)
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
  @ccall libflint.nmod_poly_sqrt_series(z::Ref{fpAbsPowerSeriesRingElem}, a::Ref{fpAbsPowerSeriesRingElem}, a.prec::Int)::Nothing
  if !isone(s)
    z *= s
  end
  if !iszero(v)
    z = shift_left(z, div(v, 2))
  end
  return true, z
end

@doc raw"""
    sqrt(a::fpAbsPowerSeriesRingElem; check::Bool=true)

Return the power series square root of $a$.
"""
function Base.sqrt(a::fpAbsPowerSeriesRingElem; check::Bool=true)
  flag, s = sqrt_classical(a; check=check)
  check && !flag && error("Not a square")
  return s
end

function is_square(a::fpAbsPowerSeriesRingElem)
  flag, s = sqrt_classical(a; check=true)
  return flag
end

function is_square_with_sqrt(a::fpAbsPowerSeriesRingElem)
  return sqrt_classical(a; check=true)
end

###############################################################################
#
#   power_series_ring constructor
#
###############################################################################

function power_series_ring(R::zzModRing, prec::Int, s::VarName = :x; model::Symbol=:capped_relative, cached::Bool = true)
  if model == :capped_relative
    parent_obj = zzModRelPowerSeriesRing(R, prec, Symbol(s), cached)
  elseif model == :capped_absolute
    parent_obj = zzModAbsPowerSeriesRing(R, prec, Symbol(s), cached)
  else
    error("Unknown model")
  end
  return parent_obj, gen(parent_obj)
end

function AbsPowerSeriesRing(R::zzModRing, prec::Int)
  return zzModAbsPowerSeriesRing(R, prec, :x, false)
end

function RelPowerSeriesRing(R::zzModRing, prec::Int)
  return zzModRelPowerSeriesRing(R, prec, :x, false)
end

function power_series_ring(R::fpField, prec::Int, s::VarName = :x; model::Symbol=:capped_relative, cached::Bool = true)
  if model == :capped_relative
    parent_obj = fpRelPowerSeriesRing(R, prec, Symbol(s), cached)
  elseif model == :capped_absolute
    parent_obj = fpAbsPowerSeriesRing(R, prec, Symbol(s), cached)
  else
    error("Unknown model")
  end
  return parent_obj, gen(parent_obj)
end

function AbsPowerSeriesRing(R::fpField, prec::Int)
  return fpAbsPowerSeriesRing(R, prec, :x, false)
end

function RelPowerSeriesRing(R::fpField, prec::Int)
  return fpRelPowerSeriesRing(R, prec, :x, false)
end
