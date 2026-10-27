###############################################################################
#
#   nmod_rel_series.jl: Relative series using nmod_poly
#
#   nmod_rel_series, gfp_rel_series
#
###############################################################################

for (etype, rtype, mtype, brtype) in (
                                      (zzModRelPowerSeriesRingElem, zzModRelPowerSeriesRing, zzModRingElem, zzModRing),
                                      (fpRelPowerSeriesRingElem, fpRelPowerSeriesRing, fpFieldElem, fpField))
  @eval begin

    ###############################################################################
    #
    #   Data type and parent object methods
    #
    ###############################################################################

    function O(a::($etype))
      val = pol_length(a) + valuation(a) - 1
      val < 0 && throw(DomainError(val, "Valuation must be non-negative"))
      z = ($etype)(modulus(a), Vector{UInt}(undef, 0), 0, val, val)
      z.parent = parent(a)
      return z
    end

    elem_type(::Type{($rtype)}) = ($etype)

    parent_type(::Type{($etype)}) = ($rtype)

    base_ring(R::($rtype)) = R.base_ring

    rel_series_type(::Type{($mtype)}) = ($etype)

    var(a::($rtype)) = a.S

    ###############################################################################
    #
    #   Basic manipulation
    #
    ###############################################################################

    max_precision(R::($rtype)) = R.prec_max

    function normalise(a::($etype), len::Int)
      len <= 0 && return len
      c = @ccall libflint.nmod_poly_get_coeff_ui(a::Ref{($etype)}, (len - 1)::Int)::UInt
      while len > 0 && iszero(c)
        len -= 1
        if len > 0
          c = @ccall libflint.nmod_poly_get_coeff_ui(a::Ref{($etype)}, (len - 1)::Int)::UInt
        end
      end
      return len
    end

    pol_length(x::($etype)) = x.length

    precision(x::($etype)) = x.prec

    function polcoeff(x::($etype), n::Int)
      R = base_ring(x)
      if n < 0
        return R(0)
      end
      z = @ccall libflint.nmod_poly_get_coeff_ui(x::Ref{($etype)}, n::Int)::UInt
      return R(z)
    end

    zero(R::($rtype)) = R(0)

    one(R::($rtype)) = R(1)

    function gen(R::($rtype))
      z = ($etype)(modulus(R), [UInt(1)], 1, max_precision(R) + 1, 1)
      z.parent = R
      return z
    end

    modulus(R::($rtype)) = modulus(base_ring(R))

    function deepcopy_internal(a::($etype), dict::IdDict)
      z = ($etype)(a)
      z.prec = a.prec
      z.val = a.val
      z.parent = parent(a)
      return z
    end

    function renormalize!(z::($etype))
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
        @ccall libflint.nmod_poly_shift_right(z::Ref{($etype)}, z::Ref{($etype)}, i::Int)::Nothing
      end
      return nothing
    end

    characteristic(R::($rtype)) = modulus(R)

    ###############################################################################
    #
    #   Similar
    #
    ###############################################################################

    function similar(f::RelPowerSeriesRingElem, R::($brtype), max_prec::Int,
        s::Symbol=var(parent(f)); cached::Bool=true)
      par = ($rtype)(R, max_prec, s, cached)
      z = ($etype)(modulus(R))
      if base_ring(f) === R && s == var(parent(f)) &&
        f isa ($etype) && max_precision(parent(f)) == max_prec
        # steal parent in case it is not cached
        z.parent = parent(f)
      else
        z.parent = par
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

    function rel_series(R::($brtype), arr::Vector{T},
        len::Int, prec::Int, val::Int, var::VarName=:x;
        max_precision::Int=prec, cached::Bool=true) where T
      prec < len + val && error("Precision too small for given data")
      coeffs = T == ($mtype) ? arr : map(R, arr)
      coeffs = length(coeffs) == 0 ? ($mtype)[] : coeffs
      par = ($rtype)(R, max_precision, Symbol(var), cached)
      z = ($etype)(modulus(par), coeffs, len, prec, val)
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
      lena = pol_length(a)
      lenb = pol_length(b)
      prec = min(precision(a), precision(b))
      val = min(valuation(a), valuation(b))
      lena = min(lena, prec - valuation(a))
      lenb = min(lenb, prec - valuation(b))
      z = parent(a)()
      if valuation(a) < valuation(b)
        lenz = max(lena, lenb + valuation(b) - valuation(a))
        @ccall libflint.nmod_poly_set_trunc(z::Ref{($etype)}, b::Ref{($etype)}, max(0, lenz - valuation(b) + valuation(a))::Int)::Nothing
        @ccall libflint.nmod_poly_shift_left(z::Ref{($etype)}, z::Ref{($etype)}, (valuation(b) - valuation(a))::Int)::Nothing
        @ccall libflint.nmod_poly_add_series(z::Ref{($etype)}, z::Ref{($etype)}, a::Ref{($etype)}, lenz::Int)::Nothing
      elseif valuation(b) < valuation(a)
        lenz = max(lena + valuation(a) - valuation(b), lenb)
        @ccall libflint.nmod_poly_set_trunc(z::Ref{($etype)}, a::Ref{($etype)}, max(0, lenz - valuation(a) + valuation(b))::Int)::Nothing
        @ccall libflint.nmod_poly_shift_left(z::Ref{($etype)}, z::Ref{($etype)}, (valuation(a) - valuation(b))::Int)::Nothing
        @ccall libflint.nmod_poly_add_series(z::Ref{($etype)}, z::Ref{($etype)}, b::Ref{($etype)}, lenz::Int)::Nothing
      else
        lenz = max(lena, lenb)
        @ccall libflint.nmod_poly_add_series(z::Ref{($etype)}, a::Ref{($etype)}, b::Ref{($etype)}, lenz::Int)::Nothing
      end
      z.prec = prec
      z.val = val
      renormalize!(z)
      return z
    end

    function -(a::($etype), b::($etype))
      check_parent(a, b)
      lena = pol_length(a)
      lenb = pol_length(b)
      prec = min(precision(a), precision(b))
      val = min(valuation(a), valuation(b))
      lena = min(lena, prec - valuation(a))
      lenb = min(lenb, prec - valuation(b))
      lenz = max(lena, lenb)
      z = parent(a)()
      if valuation(a) < valuation(b)
        lenz = max(lena, lenb + valuation(b) - valuation(a))
        @ccall libflint.nmod_poly_set_trunc(z::Ref{($etype)}, b::Ref{($etype)}, max(0, lenz - valuation(b) + valuation(a))::Int)::Nothing
        @ccall libflint.nmod_poly_shift_left(z::Ref{($etype)}, z::Ref{($etype)}, (valuation(b) - valuation(a))::Int)::Nothing
        @ccall libflint.nmod_poly_neg(z::Ref{($etype)}, z::Ref{($etype)})::Nothing
        @ccall libflint.nmod_poly_add_series(z::Ref{($etype)}, z::Ref{($etype)}, a::Ref{($etype)}, lenz::Int)::Nothing
      elseif valuation(b) < valuation(a)
        lenz = max(lena + valuation(a) - valuation(b), lenb)
        @ccall libflint.nmod_poly_set_trunc(z::Ref{($etype)}, a::Ref{($etype)}, max(0, lenz - valuation(a) + valuation(b))::Int)::Nothing
        @ccall libflint.nmod_poly_shift_left(z::Ref{($etype)}, z::Ref{($etype)}, (valuation(a) - valuation(b))::Int)::Nothing
        @ccall libflint.nmod_poly_sub_series(z::Ref{($etype)}, z::Ref{($etype)}, b::Ref{($etype)}, lenz::Int)::Nothing
      else
        lenz = max(lena, lenb)
        @ccall libflint.nmod_poly_sub_series(z::Ref{($etype)}, a::Ref{($etype)}, b::Ref{($etype)}, lenz::Int)::Nothing
      end
      z.prec = prec
      z.val = val
      renormalize!(z)
      return z
    end

    function *(a::($etype), b::($etype))
      check_parent(a, b)
      lena = pol_length(a)
      lenb = pol_length(b)
      aval = valuation(a)
      bval = valuation(b)
      prec = min(precision(a) - aval, precision(b) - bval)
      lena = min(lena, prec)
      lenb = min(lenb, prec)
      z = parent(a)()
      z.val = valuation(a) + valuation(b)
      z.prec = prec + valuation(z)
      if lena == 0 || lenb == 0
        return z
      end
      lenz = min(lena + lenb - 1, prec)
      @ccall libflint.nmod_poly_mullow(z::Ref{($etype)}, a::Ref{($etype)}, b::Ref{($etype)}, lenz::Int)::Nothing
      renormalize!(z)
      return z
    end

    ###############################################################################
    #
    #   Ad hoc binary operators
    #
    ###############################################################################

    function *(x::($mtype), y::($etype))
      z = parent(y)()
      z.prec = y.prec
      z.val = y.val
      @ccall libflint.nmod_poly_scalar_mul_nmod(z::Ref{($etype)}, y::Ref{($etype)}, data(x)::UInt)::Nothing
      renormalize!(z)
      return z
    end

    *(x::($etype), y::($mtype)) = y * x

    function *(x::ZZRingElem, y::($etype))
      z = parent(y)()
      z.prec = y.prec
      z.val = y.val
      r = @ccall libflint.fmpz_fdiv_ui(x::Ref{ZZRingElem}, modulus(y)::UInt)::UInt
      @ccall libflint.nmod_poly_scalar_mul_nmod(z::Ref{($etype)}, y::Ref{($etype)}, r::UInt)::Nothing
      renormalize!(z)
      return z
    end

    function *(x::UInt, y::($etype))
      z = parent(y)()
      z.prec = y.prec
      z.val = y.val
      @ccall libflint.nmod_poly_scalar_mul_nmod(z::Ref{($etype)}, y::Ref{($etype)}, mod(x, modulus(y))::UInt)::Nothing
      renormalize!(z)
      return z
    end

    *(x::($etype), y::ZZRingElem) = y * x

    *(x::($etype), y::UInt) = y * x

    *(x::Integer, y::($etype)) = ZZRingElem(x)*y

    *(x::($etype), y::Integer) = y * x

    ###############################################################################
    #
    #   Shifting
    #
    ###############################################################################

    function shift_left(x::($etype), len::Int)
      len < 0 && throw(DomainError(len, "Shift must be non-negative"))
      xlen = pol_length(x)
      z = ($etype)(x)
      z.prec = precision(x) + len
      z.val = valuation(x) + len
      z.parent = parent(x)
      return z
    end

    function shift_right(x::($etype), len::Int)
      len < 0 && throw(DomainError(len, "Shift must be non-negative"))
      xlen = pol_length(x)
      xval = valuation(x)
      z = parent(x)()
      if len >= xlen + xval
        z.prec = max(0, precision(x) - len)
        z.val = max(0, precision(x) - len)
      else
        z.prec = max(0, precision(x) - len)
        z.val = max(0, xval - len)
        zlen = min(xlen + xval - len, xlen)
        @ccall libflint.nmod_poly_shift_right(z::Ref{($etype)}, x::Ref{($etype)}, (xlen - zlen)::Int)::Nothing
        renormalize!(z)
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
      if k <= valuation(x)
        x = zero!(x)
        x.val = k
      else
        @ccall libflint.nmod_poly_truncate(x::Ref{($etype)}, (k - valuation(x))::Int)::Nothing
      end
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
      if is_gen(a)
        z = parent(a)()
        z = setcoeff!(z, 0, UInt(1))
        z.prec = precision(a) + b - 1
        z.val = b
      elseif pol_length(a) == 0
        z = parent(a)()
        z.prec = b*valuation(a)
        z.val = b*valuation(a)
      elseif pol_length(a) == 1
        z = parent(a)([polcoeff(a, 0)^b], 1,
                      (b - 1)*valuation(a) + precision(a), b*valuation(a))
        renormalize!(z)
        return z
      elseif b == 0
        return one(parent(a))
      else
        z = parent(a)()
        z.prec = precision(a) + (b - 1)*valuation(a)
        z.val = b*valuation(a)
        @ccall libflint.nmod_poly_pow_trunc(z::Ref{($etype)}, a::Ref{($etype)}, b::Int, (precision(z) - valuation(z))::Int)::Nothing
      end
      renormalize!(z)
      return z
    end

    ###############################################################################
    #
    #   Comparison
    #
    ###############################################################################

    function ==(x::($etype), y::($etype))
      check_parent(x, y)
      prec = min(precision(x), precision(y))
      if prec <= valuation(x) && prec <= valuation(y)
        return true
      end
      if valuation(x) != valuation(y)
        return false
      end
      xlen = normalise(x, min(pol_length(x), prec - valuation(x)))
      ylen = normalise(y, min(pol_length(y), prec - valuation(y)))
      if xlen != ylen
        return false
      end
      return Bool(@ccall libflint.nmod_poly_equal_trunc(x::Ref{($etype)}, y::Ref{($etype)}, xlen::Int)::Cint)
    end

    function isequal(x::($etype), y::($etype))
      if parent(x) != parent(y)
        return false
      end
      if precision(x) != precision(y) || valuation(x) != valuation(y) || pol_length(x) != pol_length(y)
        return false
      end
      return Bool(@ccall libflint.nmod_poly_equal(x::Ref{($etype)}, y::Ref{($etype)})::Cint)
    end

    ###############################################################################
    #
    #   Ad hoc comparisons
    #
    ###############################################################################

    function ==(x::($etype), y::($mtype))
      if precision(x) == 0
        return true
      elseif pol_length(x) > 1
        return false
      elseif pol_length(x) == 1
        if valuation(x) == 0
          z = @ccall libflint.nmod_poly_get_coeff_ui(x::Ref{($etype)}, 0::Int)::UInt
          return data(y) == z
        else
          return false
        end
      else
        return iszero(y)
      end
    end

    ==(x::($mtype), y::($etype)) = y == x

    function ==(x::($etype), y::ZZRingElem)
      if precision(x) == 0
        return true
      elseif pol_length(x) > 1
        return false
      elseif pol_length(x) == 1
        if valuation(x) == 0
          r = @ccall libflint.fmpz_fdiv_ui(y::Ref{ZZRingElem}, modulus(x)::UInt)::UInt
          z = @ccall libflint.nmod_poly_get_coeff_ui(x::Ref{($etype)}, 0::Int)::UInt
          return r == z
        else
          return false
        end
      else
        r = @ccall libflint.fmpz_fdiv_ui(y::Ref{ZZRingElem}, modulus(x)::UInt)::UInt
        return r == UInt(0)
      end
    end

    ==(x::ZZRingElem, y::($etype)) = y == x

    function ==(x::($etype), y::UInt)
      if precision(x) == 0
        return true
      elseif pol_length(x) > 1
        return false
      elseif pol_length(x) == 1
        if valuation(x) == 0
          r = mod(y, modulus(x))
          z = @ccall libflint.nmod_poly_get_coeff_ui(x::Ref{($etype)}, 0::Int)::UInt
          return r == z
        else
          return false
        end
      else
        r = mod(y, modulus(x))
        return r == UInt(0)
      end
    end

    ==(x::UInt, y::($etype)) = y == x

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
      yval = valuation(y)
      xval = valuation(x)
      if yval != 0
        if xval >= yval
          x = shift_right(x, yval)
          y = shift_right(y, yval)
        end
      end
      check && !is_unit(y) && error("Unable to invert power series")
      prec = min(precision(x) - valuation(x), precision(y) - valuation(y))
      z = parent(x)()
      z.val = xval - yval
      z.prec = prec + valuation(z)
      if prec != 0
        @ccall libflint.nmod_poly_div_series(z::Ref{($etype)}, x::Ref{($etype)}, y::Ref{($etype)}, prec::Int)::Nothing
      end
      return z
    end

    ###############################################################################
    #
    #   Ad hoc exact division
    #
    ###############################################################################

    function divexact(x::($etype), y::($mtype); check::Bool=true)
      iszero(y) && throw(DivideError())
      z = parent(x)()
      z.prec = x.prec
      z.val = x.val
      r = inv(y)
      @ccall libflint.nmod_poly_scalar_mul_nmod(z::Ref{($etype)}, x::Ref{($etype)}, data(r)::UInt)::Nothing
      return z
    end

    function divexact(x::($etype), y::ZZRingElem; check::Bool=true)
      iszero(y) && throw(DivideError())
      z = parent(x)()
      z.prec = x.prec
      z.prec = x.prec
      z.val = x.val
      r = @ccall libflint.fmpz_fdiv_ui(y::Ref{ZZRingElem}, modulus(x)::UInt)::UInt
      rinv = inv(base_ring(x)(r))
      @ccall libflint.nmod_poly_scalar_mul_nmod(z::Ref{($etype)}, x::Ref{($etype)}, data(rinv)::UInt)::Nothing
      return z
    end

    function divexact(x::($etype), y::UInt; check::Bool=true)
      iszero(y) && throw(DivideError())
      z = parent(x)()
      z.prec = x.prec
      z.prec = x.prec
      z.val = x.val
      r = mod(y, modulus(x))
      rinv = inv(base_ring(x)(r))
      @ccall libflint.nmod_poly_scalar_mul_nmod(z::Ref{($etype)}, x::Ref{($etype)}, data(rinv)::UInt)::Nothing
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
      ainv.val = 0
      @ccall libflint.nmod_poly_inv_series(ainv::Ref{($etype)}, a::Ref{($etype)}, precision(a)::Int)::Nothing
      return ainv
    end

    ###############################################################################
    #
    #   Special functions
    #
    ###############################################################################

    function Base.exp(a::($etype))
      if iszero(a)
        precision(a) == 0 && return deepcopy(a)
        z = one(parent(a))
        z.prec = precision(a)
        return z
      end
      z = parent(a)()
      R = base_ring(a)
      vala = valuation(a)
      preca = precision(a)
      d = Vector{($mtype)}(undef, preca)
      c = vala == 0 ? polcoeff(a, 0) : R()
      d[1] = exp(c)
      len = pol_length(a) + vala
      z0 = R()
      for k = 1 : preca - 1
        s = R()
        for j = 1 : min(k + 1, len) - 1
          c = j >= vala ? polcoeff(a, j - vala) : z0
          s += j * c * d[k - j + 1]
        end
        !is_unit(base_ring(a)(k)) && error("Unable to divide in exp")
        d[k + 1] = divexact(base_ring(a)(s), k)
      end
      z = parent(a)(d, preca, preca, 0)
      @ccall libflint._nmod_poly_set_length(z::Ref{($etype)}, normalise(z, preca)::Int)::Nothing
      return z
    end

    ###############################################################################
    #
    #   Unsafe functions
    #
    ###############################################################################

    function zero!(x::($etype))
      @ccall libflint.nmod_poly_zero(x::Ref{($etype)})::Nothing
      x.prec = max_precision(parent(x))
      x.val = max_precision(parent(x))
      return x
    end

    function one!(x::($etype))
      @ccall libflint.nmod_poly_one(x::Ref{($etype)})::Nothing
      x.prec = max_precision(parent(x))
      x.val = 0
      return x
    end

    function neg!(z::($etype), x::($etype))
      @ccall libflint.nmod_poly_neg(z::Ref{($etype)}, x::Ref{($etype)})::Nothing
      z.prec = x.prec
      z.val = x.val
      return z
    end

    function fit!(x::($etype), n::Int)
      @ccall libflint.nmod_poly_fit_length(x::Ref{($etype)}, n::Int)::Nothing
      return nothing
    end

    function setcoeff!(z::($etype), n::Int, x::ZZRingElem)
      r = @ccall libflint.fmpz_fdiv_ui(x::Ref{ZZRingElem}, modulus(z)::UInt)::UInt
      @ccall libflint.nmod_poly_set_coeff_ui(z::Ref{($etype)}, n::Int, r::UInt)::Nothing
      return z
    end

    function setcoeff!(z::($etype), n::Int, x::UInt)
      r = mod(x, modulus(z))
      @ccall libflint.nmod_poly_set_coeff_ui(z::Ref{($etype)}, n::Int, r::UInt)::Nothing
      return z
    end

    function setcoeff!(z::($etype), n::Int, x::($mtype))
      @ccall libflint.nmod_poly_set_coeff_ui(z::Ref{($etype)}, n::Int, data(x)::UInt)::Nothing
      return z
    end

    function mul!(z::($etype), a::($etype), b::($etype))
      lena = pol_length(a)
      lenb = pol_length(b)
      aval = valuation(a)
      bval = valuation(b)
      prec = min(precision(a) - aval, precision(b) - bval)
      lena = min(lena, prec)
      lenb = min(lenb, prec)
      z.val = valuation(a) + valuation(b)
      z.prec = prec + valuation(z)
      lenz = min(lena + lenb - 1, prec)
      if lena <= 0 || lenb <= 0
        lenz = 0
      end
      @ccall libflint.nmod_poly_mullow(z::Ref{($etype)}, a::Ref{($etype)}, b::Ref{($etype)}, lenz::Int)::Nothing
      renormalize!(z)
      return z
    end

    function add!(a::($etype), b::($etype))
      lena = pol_length(a)
      lenb = pol_length(b)
      prec = min(precision(a), precision(b))
      val = min(valuation(a), valuation(b))
      lena = min(lena, prec - valuation(a))
      lenb = min(lenb, prec - valuation(b))
      n = modulus(parent(a))
      if valuation(a) < valuation(b)
        z = ($etype)(n)
        z.parent = parent(a)
        lenz = max(lena, lenb + valuation(b) - valuation(a))
        @ccall libflint.nmod_poly_set_trunc(z::Ref{($etype)}, b::Ref{($etype)}, max(0, lenz - valuation(b) + valuation(a))::Int)::Nothing
        @ccall libflint.nmod_poly_shift_left(z::Ref{($etype)}, z::Ref{($etype)}, (valuation(b) - valuation(a))::Int)::Nothing
        @ccall libflint.nmod_poly_add_series(a::Ref{($etype)}, a::Ref{($etype)}, z::Ref{($etype)}, lenz::Int)::Nothing
      elseif valuation(b) < valuation(a)
        lenz = max(lena + valuation(a) - valuation(b), lenb)
        @ccall libflint.nmod_poly_truncate(a::Ref{($etype)}, max(0, lenz - valuation(a) + valuation(b))::Int)::Nothing
        @ccall libflint.nmod_poly_shift_left(a::Ref{($etype)}, a::Ref{($etype)}, (valuation(a) - valuation(b))::Int)::Nothing
        @ccall libflint.nmod_poly_add_series(a::Ref{($etype)}, a::Ref{($etype)}, b::Ref{($etype)}, lenz::Int)::Nothing
      else
        lenz = max(lena, lenb)
        @ccall libflint.nmod_poly_add_series(a::Ref{($etype)}, a::Ref{($etype)}, b::Ref{($etype)}, lenz::Int)::Nothing
      end
      a.prec = prec
      a.val = val
      renormalize!(a)
      return a
    end

    function add!(c::($etype), a::($etype), b::($etype))
      if c === a
        return add!(c, b)
      elseif c === b
        return add!(c, a)
      end
      lena = pol_length(a)
      lenb = pol_length(b)
      prec = min(precision(a), precision(b))
      val = min(valuation(a), valuation(b))
      lena = min(lena, prec - valuation(a))
      lenb = min(lenb, prec - valuation(b))
      if valuation(a) < valuation(b)
        lenc = max(lena, lenb + valuation(b) - valuation(a))
        @ccall libflint.nmod_poly_set_trunc(c::Ref{($etype)}, b::Ref{($etype)}, max(0, lenc - valuation(b) + valuation(a))::Int)::Nothing
        @ccall libflint.nmod_poly_shift_left(c::Ref{($etype)}, c::Ref{($etype)}, (valuation(b) - valuation(a))::Int)::Nothing
        @ccall libflint.nmod_poly_add_series(c::Ref{($etype)}, c::Ref{($etype)}, a::Ref{($etype)}, lenc::Int)::Nothing
      elseif valuation(b) < valuation(a)
        lenc = max(lena + valuation(a) - valuation(b), lenb)
        @ccall libflint.nmod_poly_set_trunc(c::Ref{($etype)}, a::Ref{($etype)}, max(0, lenc - valuation(a) + valuation(b))::Int)::Nothing
        @ccall libflint.nmod_poly_shift_left(c::Ref{($etype)}, c::Ref{($etype)}, (valuation(a) - valuation(b))::Int)::Nothing
        @ccall libflint.nmod_poly_add_series(c::Ref{($etype)}, c::Ref{($etype)}, b::Ref{($etype)}, lenc::Int)::Nothing
      else
        lenc = max(lena, lenb)
        @ccall libflint.nmod_poly_add_series(c::Ref{($etype)}, a::Ref{($etype)}, b::Ref{($etype)}, lenc::Int)::Nothing
      end
      c.prec = prec
      c.val = val
      renormalize!(c)
      return c
    end

    function set_length!(a::($etype), n::Int)
      @ccall libflint._nmod_poly_set_length(a::Ref{($etype)}, n::Int)::Nothing
      return a
    end

    ###############################################################################
    #
    #   Promotion rules
    #
    ###############################################################################

    promote_rule(::Type{($etype)}, ::Type{T}) where {T <: Integer} = ($etype)

    promote_rule(::Type{($etype)}, ::Type{ZZRingElem}) = ($etype)

    promote_rule(::Type{($etype)}, ::Type{($mtype)}) = ($etype)

    ###############################################################################
    #
    #   Parent object call overload
    #
    ###############################################################################

    function (a::($rtype))()
      z = ($etype)(modulus(a))
      z.prec = max_precision(a)
      z.val = max_precision(a)
      z.parent = a
      return z
    end

    function (a::($rtype))(b::Integer)
      return a(base_ring(a)(b))
    end

    function (a::($rtype))(b::ZZRingElem)
      return a(base_ring(a)(b))
    end

    function (a::($rtype))(b::($mtype))
      if iszero(b)
        z = ($etype)(modulus(a))
        z.prec = max_precision(a)
        z.val = max_precision(a)
      else
        z = ($etype)(modulus(a), [b], 1, max_precision(a), 0)
      end
      z.parent = a
      return z
    end

    function (a::($rtype))(b::($etype))
      parent(b) != a && error("Unable to coerce power series")
      return b
    end

    function (a::($rtype))(b::Vector{ZZRingElem}, len::Int, prec::Int, val::Int)
      z = ($etype)(modulus(a), b, len, prec, val)
      z.parent = a
      return z
    end

    function (a::($rtype))(b::Vector{UInt}, len::Int, prec::Int, val::Int)
      z = ($etype)(modulus(a), b, len, prec, val)
      z.parent = a
      return z
    end

    function (a::($rtype))(b::Vector{($mtype)}, len::Int, prec::Int, val::Int)
      z = ($etype)(modulus(a), b, len, prec, val)
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

function sqrt_classical_char2(a::fpRelPowerSeriesRingElem; check::Bool=true)
  S = parent(a)
  R = base_ring(a)
  prec = div(precision(a) + 1, 2)
  if iszero(a)
    asqrt = parent(a)()
    asqrt = set_precision!(asqrt, prec)
    asqrt = set_valuation!(asqrt, prec)
    return true, asqrt
  end
  aval = valuation(a)
  if check && !iseven(aval)
    return false, S()
  end
  aval2 = div(aval, 2)
  asqrt = parent(a)()
  fit!(asqrt, prec)
  asqrt = set_precision!(asqrt, prec)
  asqrt = set_valuation!(asqrt, aval2)
  if check
    for i = 1:2:precision(a) - aval - 1 # series must have even exponents
      if !iszero(polcoeff(a, i))
        return false, S()
      end
    end
  end
  for i = 0:prec - aval2 - 1
    asqrt = setcoeff!(asqrt, i, polcoeff(a, 2*i))
  end
  return true, asqrt
end

function sqrt_classical(a::fpRelPowerSeriesRingElem; check::Bool=true)
  S = parent(a)
  if characteristic(S) == 2
    return sqrt_classical_char2(a; check=check)
  end
  v = valuation(a)
  z = S()
  v2 = div(v, 2)
  if iszero(a)
    z.prec = v2
    z.val = v2
    return true, z
  end
  if check && !iseven(v)
    return false, S()
  end
  z.prec = precision(a) - v2
  z.val = v2
  c = coeff(a, v)
  if check
    flag, s = is_square_with_sqrt(c)
    if !flag
      return false, S()
    end
  else
    s = sqrt(c; check=check)
  end
  a = divexact(a, c)
  @ccall libflint.nmod_poly_sqrt_series(z::Ref{fpRelPowerSeriesRingElem}, a::Ref{fpRelPowerSeriesRingElem}, precision(a)::Int)::Nothing
  if !isone(s)
    z *= s
  end
  return true, z
end

@doc raw"""
    sqrt(a::fpRelPowerSeriesRingElem)

Return the square root of the power series $a$. By default the function raises
an exception if the input is not a square. If `check=false` this check is
omitted.
"""
function Base.sqrt(a::fpRelPowerSeriesRingElem; check::Bool=true)
  flag, q = sqrt_classical(a; check=check)
  if check && !flag
    error("Not a square in sqrt")
  end
  return q
end

function is_square(a::fpRelPowerSeriesRingElem)
  flag, q = sqrt_classical(a; check=true)
  return flag
end

function is_square_with_sqrt(a::fpRelPowerSeriesRingElem)
  return sqrt_classical(a; check=true)
end

