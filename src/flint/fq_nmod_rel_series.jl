###############################################################################
#
#   fq_nmod_rel_series.jl: Relative series over finite fields
#
###############################################################################

    ###############################################################################
    #
    #   Data type and parent object methods
    #
    ###############################################################################

    function O(a::fqPolyRepRelPowerSeriesRingElem)
      val = pol_length(a) + valuation(a) - 1
      val < 0 && throw(DomainError(val, "Valuation must be non-negative"))
      z = fqPolyRepRelPowerSeriesRingElem(base_ring(a), Vector{fqPolyRepFieldElem}(undef, 0), 0, val, val)
      z.parent = parent(a)
      return z
    end

    elem_type(::Type{fqPolyRepRelPowerSeriesRing}) = fqPolyRepRelPowerSeriesRingElem

    parent_type(::Type{fqPolyRepRelPowerSeriesRingElem}) = fqPolyRepRelPowerSeriesRing

    base_ring(R::fqPolyRepRelPowerSeriesRing) = R.base_ring

    rel_series_type(::Type{fqPolyRepFieldElem}) = fqPolyRepRelPowerSeriesRingElem

    var(a::fqPolyRepRelPowerSeriesRing) = a.S

    ###############################################################################
    #
    #   Basic manipulation
    #
    ###############################################################################

    max_precision(R::fqPolyRepRelPowerSeriesRing) = R.prec_max

    function normalise(a::fqPolyRepRelPowerSeriesRingElem, len::Int)
      ctx = base_ring(a)
      if len > 0
        c = base_ring(a)()
        ccall((:fq_nmod_poly_get_coeff, libflint), Nothing,
              (Ref{fqPolyRepFieldElem}, Ref{fqPolyRepRelPowerSeriesRingElem}, Int, Ref{fqPolyRepField}),
              c, a, len - 1, ctx)
      end
      while len > 0 && iszero(c)
        len -= 1
        if len > 0
          ccall((:fq_nmod_poly_get_coeff, libflint), Nothing,
                (Ref{fqPolyRepFieldElem}, Ref{fqPolyRepRelPowerSeriesRingElem}, Int, Ref{fqPolyRepField}),
                c, a, len - 1, ctx)
        end
      end
      return len
    end

    function pol_length(x::fqPolyRepRelPowerSeriesRingElem)
      return ccall((:fq_nmod_poly_length, libflint), Int,
                   (Ref{fqPolyRepRelPowerSeriesRingElem}, Ref{fqPolyRepField}), x, base_ring(x))
    end

    precision(x::fqPolyRepRelPowerSeriesRingElem) = x.prec

    function polcoeff(x::fqPolyRepRelPowerSeriesRingElem, n::Int)
      z = base_ring(x)()
      if n < 0
        return z
      end
      ccall((:fq_nmod_poly_get_coeff, libflint), Nothing,
            (Ref{fqPolyRepFieldElem}, Ref{fqPolyRepRelPowerSeriesRingElem}, Int, Ref{fqPolyRepField}),
            z, x, n, base_ring(x))
      return z
    end

    zero(R::fqPolyRepRelPowerSeriesRing) = R(0)

    one(R::fqPolyRepRelPowerSeriesRing) = R(1)

    function gen(R::fqPolyRepRelPowerSeriesRing)
      z = fqPolyRepRelPowerSeriesRingElem(base_ring(R), [base_ring(R)(1)], 1, max_precision(R) + 1, 1)
      z.parent = R
      return z
    end

    function deepcopy_internal(a::fqPolyRepRelPowerSeriesRingElem, dict::IdDict)
      z = fqPolyRepRelPowerSeriesRingElem(base_ring(a), a)
      z.prec = a.prec
      z.val = a.val
      z.parent = parent(a)
      return z
    end

    function renormalize!(z::fqPolyRepRelPowerSeriesRingElem)
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
        ccall((:fq_nmod_poly_shift_right, libflint), Nothing,
              (Ref{fqPolyRepRelPowerSeriesRingElem}, Ref{fqPolyRepRelPowerSeriesRingElem}, Int, Ref{fqPolyRepField}),
              z, z, i, base_ring(z))
      end
      return nothing
    end

    characteristic(R::fqPolyRepRelPowerSeriesRing) = characteristic(base_ring(R))

    ###############################################################################
    #
    #   Similar
    #
    ###############################################################################

    function similar(f::RelPowerSeriesRingElem, R::fqPolyRepField, max_prec::Int,
        s::Symbol=var(parent(f)); cached::Bool=true)
      par = fqPolyRepRelPowerSeriesRing(R, max_prec, s, cached)
      z = fqPolyRepRelPowerSeriesRingElem(R)
      if base_ring(f) === R && s == var(parent(f)) &&
        f isa fqPolyRepRelPowerSeriesRingElem && max_precision(parent(f)) == max_prec
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

    function rel_series(R::fqPolyRepField, arr::Vector{T},
        len::Int, prec::Int, val::Int, var::VarName=:x;
        max_precision::Int=prec, cached::Bool=true) where T
      prec < len + val && error("Precision too small for given data")
      coeffs = T == fqPolyRepFieldElem ? arr : map(R, arr)
      coeffs = length(coeffs) == 0 ? fqPolyRepFieldElem[] : coeffs
      par = fqPolyRepRelPowerSeriesRing(R, max_precision, Symbol(var), cached)
      z = fqPolyRepRelPowerSeriesRingElem(R, coeffs, len, prec, val)
      z.parent = par
      return z
    end

    ###############################################################################
    #
    #   Unary operators
    #
    ###############################################################################

    -(x::fqPolyRepRelPowerSeriesRingElem) = neg!(parent(x)(), x)

    ###############################################################################
    #
    #   Binary operators
    #
    ###############################################################################

    function +(a::fqPolyRepRelPowerSeriesRingElem, b::fqPolyRepRelPowerSeriesRingElem)
      check_parent(a, b)
      lena = pol_length(a)
      lenb = pol_length(b)
      prec = min(a.prec, b.prec)
      val = min(a.val, b.val)
      lena = min(lena, prec - a.val)
      lenb = min(lenb, prec - b.val)
      z = parent(a)()
      ctx = base_ring(a)
      if a.val < b.val
        lenz = max(lena, lenb + b.val - a.val)
        ccall((:fq_nmod_poly_set_trunc, libflint), Nothing,
              (Ref{fqPolyRepRelPowerSeriesRingElem}, Ref{fqPolyRepRelPowerSeriesRingElem}, Int, Ref{fqPolyRepField}),
              z, b, max(0, lenz - b.val + a.val), ctx)
        ccall((:fq_nmod_poly_shift_left, libflint), Nothing,
              (Ref{fqPolyRepRelPowerSeriesRingElem}, Ref{fqPolyRepRelPowerSeriesRingElem}, Int, Ref{fqPolyRepField}),
              z, z, b.val - a.val, ctx)
        ccall((:fq_nmod_poly_add_series, libflint), Nothing,
              (Ref{fqPolyRepRelPowerSeriesRingElem}, Ref{fqPolyRepRelPowerSeriesRingElem},
               Ref{fqPolyRepRelPowerSeriesRingElem}, Int, Ref{fqPolyRepField}),
              z, z, a, lenz, ctx)
      elseif b.val < a.val
        lenz = max(lena + a.val - b.val, lenb)
        ccall((:fq_nmod_poly_set_trunc, libflint), Nothing,
              (Ref{fqPolyRepRelPowerSeriesRingElem}, Ref{fqPolyRepRelPowerSeriesRingElem}, Int, Ref{fqPolyRepField}),
              z, a, max(0, lenz - a.val + b.val), ctx)
        ccall((:fq_nmod_poly_shift_left, libflint), Nothing,
              (Ref{fqPolyRepRelPowerSeriesRingElem}, Ref{fqPolyRepRelPowerSeriesRingElem}, Int, Ref{fqPolyRepField}),
              z, z, a.val - b.val, ctx)
        ccall((:fq_nmod_poly_add_series, libflint), Nothing,
              (Ref{fqPolyRepRelPowerSeriesRingElem}, Ref{fqPolyRepRelPowerSeriesRingElem},
               Ref{fqPolyRepRelPowerSeriesRingElem}, Int, Ref{fqPolyRepField}),
              z, z, b, lenz, ctx)
      else
        lenz = max(lena, lenb)
        ccall((:fq_nmod_poly_add_series, libflint), Nothing,
              (Ref{fqPolyRepRelPowerSeriesRingElem}, Ref{fqPolyRepRelPowerSeriesRingElem},
               Ref{fqPolyRepRelPowerSeriesRingElem}, Int, Ref{fqPolyRepField}),
              z, a, b, lenz, ctx)
      end
      z.prec = prec
      z.val = val
      renormalize!(z)
      return z
    end

    function -(a::fqPolyRepRelPowerSeriesRingElem, b::fqPolyRepRelPowerSeriesRingElem)
      check_parent(a, b)
      lena = pol_length(a)
      lenb = pol_length(b)
      prec = min(a.prec, b.prec)
      val = min(a.val, b.val)
      lena = min(lena, prec - a.val)
      lenb = min(lenb, prec - b.val)
      lenz = max(lena, lenb)
      z = parent(a)()
      ctx = base_ring(a)
      if a.val < b.val
        lenz = max(lena, lenb + b.val - a.val)
        ccall((:fq_nmod_poly_set_trunc, libflint), Nothing,
              (Ref{fqPolyRepRelPowerSeriesRingElem}, Ref{fqPolyRepRelPowerSeriesRingElem}, Int, Ref{fqPolyRepField}),
              z, b, max(0, lenz - b.val + a.val), ctx)
        ccall((:fq_nmod_poly_shift_left, libflint), Nothing,
              (Ref{fqPolyRepRelPowerSeriesRingElem}, Ref{fqPolyRepRelPowerSeriesRingElem}, Int, Ref{fqPolyRepField}),
              z, z, b.val - a.val, ctx)
        ccall((:fq_nmod_poly_neg, libflint), Nothing,
              (Ref{fqPolyRepRelPowerSeriesRingElem}, Ref{fqPolyRepRelPowerSeriesRingElem}, Ref{fqPolyRepField}),
              z, z, ctx)
        ccall((:fq_nmod_poly_add_series, libflint), Nothing,
              (Ref{fqPolyRepRelPowerSeriesRingElem}, Ref{fqPolyRepRelPowerSeriesRingElem},
               Ref{fqPolyRepRelPowerSeriesRingElem}, Int, Ref{fqPolyRepField}),
              z, z, a, lenz, ctx)
      elseif b.val < a.val
        lenz = max(lena + a.val - b.val, lenb)
        ccall((:fq_nmod_poly_set_trunc, libflint), Nothing,
              (Ref{fqPolyRepRelPowerSeriesRingElem}, Ref{fqPolyRepRelPowerSeriesRingElem}, Int, Ref{fqPolyRepField}),
              z, a, max(0, lenz - a.val + b.val), ctx)
        ccall((:fq_nmod_poly_shift_left, libflint), Nothing,
              (Ref{fqPolyRepRelPowerSeriesRingElem}, Ref{fqPolyRepRelPowerSeriesRingElem}, Int, Ref{fqPolyRepField}),
              z, z, a.val - b.val, ctx)
        ccall((:fq_nmod_poly_sub_series, libflint), Nothing,
              (Ref{fqPolyRepRelPowerSeriesRingElem}, Ref{fqPolyRepRelPowerSeriesRingElem},
               Ref{fqPolyRepRelPowerSeriesRingElem}, Int, Ref{fqPolyRepField}),
              z, z, b, lenz, ctx)
      else
        lenz = max(lena, lenb)
        ccall((:fq_nmod_poly_sub_series, libflint), Nothing,
              (Ref{fqPolyRepRelPowerSeriesRingElem}, Ref{fqPolyRepRelPowerSeriesRingElem},
               Ref{fqPolyRepRelPowerSeriesRingElem}, Int, Ref{fqPolyRepField}),
              z, a, b, lenz, ctx)
      end
      z.prec = prec
      z.val = val
      renormalize!(z)
      return z
    end

    function *(a::fqPolyRepRelPowerSeriesRingElem, b::fqPolyRepRelPowerSeriesRingElem)
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
      ccall((:fq_nmod_poly_mullow, libflint), Nothing,
            (Ref{fqPolyRepRelPowerSeriesRingElem}, Ref{fqPolyRepRelPowerSeriesRingElem},
             Ref{fqPolyRepRelPowerSeriesRingElem}, Int, Ref{fqPolyRepField}),
            z, a, b, lenz, base_ring(a))
      return z
    end

    ###############################################################################
    #
    #   Ad hoc binary operators
    #
    ###############################################################################

    function *(x::fqPolyRepFieldElem, y::fqPolyRepRelPowerSeriesRingElem)
      z = parent(y)()
      z.prec = y.prec
      z.val = y.val
      ccall((:fq_nmod_poly_scalar_mul_fq_nmod, libflint), Nothing,
            (Ref{fqPolyRepRelPowerSeriesRingElem}, Ref{fqPolyRepRelPowerSeriesRingElem},
             Ref{fqPolyRepFieldElem}, Ref{fqPolyRepField}),
            z, y, x, base_ring(y))
      return z
    end

    *(x::fqPolyRepRelPowerSeriesRingElem, y::fqPolyRepFieldElem) = y * x

    ###############################################################################
    #
    #   Shifting
    #
    ###############################################################################

    function shift_left(x::fqPolyRepRelPowerSeriesRingElem, len::Int)
      len < 0 && throw(DomainError(len, "Shift must be non-negative"))
      xlen = pol_length(x)
      z = fqPolyRepRelPowerSeriesRingElem(base_ring(x), x)
      z.prec = x.prec + len
      z.val = x.val + len
      z.parent = parent(x)
      return z
    end

    function shift_right(x::fqPolyRepRelPowerSeriesRingElem, len::Int)
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
        ccall((:fq_nmod_poly_shift_right, libflint), Nothing,
              (Ref{fqPolyRepRelPowerSeriesRingElem}, Ref{fqPolyRepRelPowerSeriesRingElem},
               Int, Ref{fqPolyRepField}),
              z, x, xlen - zlen, base_ring(x))
        renormalize!(z)
      end
      return z
    end

    ###############################################################################
    #
    #   Truncation
    #
    ###############################################################################

    function truncate(x::fqPolyRepRelPowerSeriesRingElem, k::Int)
      return truncate!(deepcopy(x), k)
    end

    function truncate!(x::fqPolyRepRelPowerSeriesRingElem, k::Int)
      k < 0 && throw(DomainError(k, "Index must be non-negative"))
      if precision(x) <= k
        return x
      end
      if k <= valuation(x)
        x = zero!(x)
        x.val = k
      else
        ccall((:fq_nmod_poly_truncate, libflint), Nothing,
              (Ref{fqPolyRepRelPowerSeriesRingElem}, Int, Ref{fqPolyRepField}),
              x, k - valuation(x), base_ring(x))
      end
      x.prec = k
      return x
    end

    ###############################################################################
    #
    #   Powering
    #
    ###############################################################################

    function ^(a::fqPolyRepRelPowerSeriesRingElem, b::Int)
      b < 0 && throw(DomainError(b, "Exponent must be non-negative"))
      if is_gen(a)
        z = parent(a)()
        z = setcoeff!(z, 0, base_ring(a)(1))
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
        bit = ~((~UInt(0)) >> 1)
        while (UInt(bit) & b) == 0
          bit >>= 1
        end
        z = a
        bit >>= 1
        while bit != 0
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

    function ==(x::fqPolyRepRelPowerSeriesRingElem, y::fqPolyRepRelPowerSeriesRingElem)
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
      return Bool(ccall((:fq_nmod_poly_equal_trunc, libflint), Cint,
                        (Ref{fqPolyRepRelPowerSeriesRingElem}, Ref{fqPolyRepRelPowerSeriesRingElem},
                         Int, Ref{fqPolyRepField}),
                        x, y, xlen, base_ring(x)))
    end

    function isequal(x::fqPolyRepRelPowerSeriesRingElem, y::fqPolyRepRelPowerSeriesRingElem)
      if parent(x) != parent(y)
        return false
      end
      if x.prec != y.prec || x.val != y.val || pol_length(x) != pol_length(y)
        return false
      end
      return Bool(ccall((:fq_nmod_poly_equal, libflint), Cint,
                        (Ref{fqPolyRepRelPowerSeriesRingElem}, Ref{fqPolyRepRelPowerSeriesRingElem}, Ref{fqPolyRepField}),
                        x, y, base_ring(x)))
    end

    ###############################################################################
    #
    #   Exact division
    #
    ###############################################################################

    function divexact(x::fqPolyRepRelPowerSeriesRingElem, y::fqPolyRepRelPowerSeriesRingElem; check::Bool=true)
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
      prec = min(x.prec - x.val, y.prec - y.val)
      z = parent(x)()
      z.val = xval - yval
      z.prec = prec + z.val
      if prec != 0
        ccall((:fq_nmod_poly_div_series, libflint), Nothing,
              (Ref{fqPolyRepRelPowerSeriesRingElem}, Ref{fqPolyRepRelPowerSeriesRingElem},
               Ref{fqPolyRepRelPowerSeriesRingElem}, Int, Ref{fqPolyRepField}),
              z, x, y, prec, base_ring(x))
      end
      return z
    end

    ###############################################################################
    #
    #   Ad hoc exact division
    #
    ###############################################################################

    function divexact(x::fqPolyRepRelPowerSeriesRingElem, y::FqPolyRepFieldElem; check::Bool=true)
      iszero(y) && throw(DivideError())
      z = parent(x)()
      z.prec = x.prec
      z.prec = x.prec
      z.val = x.val
      ccall((:fq_nmod_poly_scalar_div_fq_nmod, libflint), Nothing,
            (Ref{fqPolyRepRelPowerSeriesRingElem}, Ref{fqPolyRepRelPowerSeriesRingElem},
             Ref{fqPolyRepFieldElem}, Ref{fqPolyRepField}),
            z, x, y, base_ring(x))
      return z
    end

    ###############################################################################
    #
    #   Inversion
    #
    ###############################################################################

    function inv(a::fqPolyRepRelPowerSeriesRingElem)
      iszero(a) && throw(DivideError())
      !is_unit(a) && error("Unable to invert power series")
      ainv = parent(a)()
      ainv.prec = a.prec
      ainv.val = 0
      ccall((:fq_nmod_poly_inv_series, libflint), Nothing,
            (Ref{fqPolyRepRelPowerSeriesRingElem}, Ref{fqPolyRepRelPowerSeriesRingElem}, Int, Ref{fqPolyRepField}),
            ainv, a, a.prec, base_ring(a))
      return ainv
    end

    ###############################################################################
    #
    #   Square root
    #
    ###############################################################################

    function sqrt_classical_char2(a::fqPolyRepRelPowerSeriesRingElem; check::Bool=true)
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
        c = polcoeff(a, 2*i)
        if check && !is_square(c)
          return false, S()
        end
        asqrt = setcoeff!(asqrt, i, sqrt(c; check=false))
      end
      return true, asqrt
    end

    function sqrt_classical(a::fqPolyRepRelPowerSeriesRingElem; check::Bool=true)
      S = parent(a)
      R = base_ring(a)
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
      if characteristic(R) == 2
        return sqrt_classical_char2(a; check=check)
      end
      z.prec = a.prec - v2
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
      ccall((:fq_nmod_poly_sqrt_series, libflint), Nothing,
            (Ref{fqPolyRepRelPowerSeriesRingElem}, Ref{fqPolyRepRelPowerSeriesRingElem},
             Int, Ref{fqPolyRepField}),
            z, a, a.prec, base_ring(a))
      if !isone(s)
        z *= s
      end
      return true, z
    end

    function Base.sqrt(a::fqPolyRepRelPowerSeriesRingElem; check::Bool=true)
      flag, q = sqrt_classical(a; check=check)
      if check && !flag
        error("Not a square in sqrt")
      end
      return q
    end

    function is_square(a::fqPolyRepRelPowerSeriesRingElem)
      flag, q = sqrt_classical(a; check=true)
      return flag
    end

    function is_square_with_sqrt(a::fqPolyRepRelPowerSeriesRingElem)
      return sqrt_classical(a; check=true)
    end

    ###############################################################################
    #
    #   Unsafe functions
    #
    ###############################################################################

    function zero!(x::fqPolyRepRelPowerSeriesRingElem)
      ccall((:fq_nmod_poly_zero, libflint), Nothing,
            (Ref{fqPolyRepRelPowerSeriesRingElem}, Ref{fqPolyRepField}), x, base_ring(x))
      x.prec = parent(x).prec_max
      x.val = parent(x).prec_max
      return x
    end

    function one!(x::fqPolyRepRelPowerSeriesRingElem)
      ccall((:fq_nmod_poly_one, libflint), Nothing,
            (Ref{fqPolyRepRelPowerSeriesRingElem}, Ref{fqPolyRepField}), x, base_ring(x))
      x.prec = parent(x).prec_max
      x.val = 0
      return x
    end

    function neg!(z::fqPolyRepRelPowerSeriesRingElem, x::fqPolyRepRelPowerSeriesRingElem)
      ccall((:fq_nmod_poly_neg, libflint), Nothing,
            (Ref{fqPolyRepRelPowerSeriesRingElem}, Ref{fqPolyRepRelPowerSeriesRingElem}, Ref{fqPolyRepField}), z, x, base_ring(x))
      z.prec = x.prec
      z.val = x.val
      return z
    end

    function fit!(z::fqPolyRepRelPowerSeriesRingElem, n::Int)
      ccall((:fq_nmod_poly_fit_length, libflint), Nothing,
            (Ref{fqPolyRepRelPowerSeriesRingElem}, Int, Ref{fqPolyRepField}),
            z, n, base_ring(z))
      return nothing
    end

    function setcoeff!(z::fqPolyRepRelPowerSeriesRingElem, n::Int, x::ZZRingElem)
      ccall((:fq_nmod_poly_set_coeff_fmpz, libflint), Nothing,
            (Ref{fqPolyRepRelPowerSeriesRingElem}, Int, Ref{ZZRingElem}, Ref{fqPolyRepField}),
            z, n, x, base_ring(z))
      return z
    end

    function setcoeff!(z::fqPolyRepRelPowerSeriesRingElem, n::Int, x::fqPolyRepFieldElem)
      ccall((:fq_nmod_poly_set_coeff, libflint), Nothing,
            (Ref{fqPolyRepRelPowerSeriesRingElem}, Int, Ref{fqPolyRepFieldElem}, Ref{fqPolyRepField}),
            z, n, x, base_ring(z))
      return z
    end

    function mul!(z::fqPolyRepRelPowerSeriesRingElem, a::fqPolyRepRelPowerSeriesRingElem, b::fqPolyRepRelPowerSeriesRingElem)
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
      ccall((:fq_nmod_poly_mullow, libflint), Nothing,
            (Ref{fqPolyRepRelPowerSeriesRingElem}, Ref{fqPolyRepRelPowerSeriesRingElem},
             Ref{fqPolyRepRelPowerSeriesRingElem}, Int, Ref{fqPolyRepField}),
            z, a, b, lenz, base_ring(z))
      return z
    end

    function add!(a::fqPolyRepRelPowerSeriesRingElem, b::fqPolyRepRelPowerSeriesRingElem)
      lena = pol_length(a)
      lenb = pol_length(b)
      prec = min(a.prec, b.prec)
      val = min(a.val, b.val)
      lena = min(lena, prec - a.val)
      lenb = min(lenb, prec - b.val)
      ctx = base_ring(a)
      if a.val < b.val
        z = fqPolyRepRelPowerSeriesRingElem(base_ring(a))
        z.parent = parent(a)
        lenz = max(lena, lenb + b.val - a.val)
        ccall((:fq_nmod_poly_set_trunc, libflint), Nothing,
              (Ref{fqPolyRepRelPowerSeriesRingElem}, Ref{fqPolyRepRelPowerSeriesRingElem}, Int, Ref{fqPolyRepField}),
              z, b, max(0, lenz - b.val + a.val), ctx)
        ccall((:fq_nmod_poly_shift_left, libflint), Nothing,
              (Ref{fqPolyRepRelPowerSeriesRingElem}, Ref{fqPolyRepRelPowerSeriesRingElem}, Int, Ref{fqPolyRepField}),
              z, z, b.val - a.val, ctx)
        ccall((:fq_nmod_poly_add_series, libflint), Nothing,
              (Ref{fqPolyRepRelPowerSeriesRingElem}, Ref{fqPolyRepRelPowerSeriesRingElem},
               Ref{fqPolyRepRelPowerSeriesRingElem}, Int, Ref{fqPolyRepField}),
              a, a, z, lenz, ctx)
      elseif b.val < a.val
        lenz = max(lena + a.val - b.val, lenb)
        ccall((:fq_nmod_poly_truncate, libflint), Nothing,
              (Ref{fqPolyRepRelPowerSeriesRingElem}, Int, Ref{fqPolyRepField}),
              a, max(0, lenz - a.val + b.val), ctx)
        ccall((:fq_nmod_poly_shift_left, libflint), Nothing,
              (Ref{fqPolyRepRelPowerSeriesRingElem}, Ref{fqPolyRepRelPowerSeriesRingElem}, Int, Ref{fqPolyRepField}),
              a, a, a.val - b.val, ctx)
        ccall((:fq_nmod_poly_add_series, libflint), Nothing,
              (Ref{fqPolyRepRelPowerSeriesRingElem}, Ref{fqPolyRepRelPowerSeriesRingElem},
               Ref{fqPolyRepRelPowerSeriesRingElem}, Int, Ref{fqPolyRepField}),
              a, a, b, lenz, ctx)
      else
        lenz = max(lena, lenb)
        ccall((:fq_nmod_poly_add_series, libflint), Nothing,
              (Ref{fqPolyRepRelPowerSeriesRingElem}, Ref{fqPolyRepRelPowerSeriesRingElem},
               Ref{fqPolyRepRelPowerSeriesRingElem}, Int, Ref{fqPolyRepField}),
              a, a, b, lenz, ctx)
      end
      a.prec = prec
      a.val = val
      renormalize!(a)
      return a
    end

    function add!(c::fqPolyRepRelPowerSeriesRingElem, a::fqPolyRepRelPowerSeriesRingElem, b::fqPolyRepRelPowerSeriesRingElem)
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
      ctx = base_ring(a)
      if a.val < b.val
        lenc = max(lena, lenb + b.val - a.val)
        ccall((:fq_nmod_poly_set_trunc, libflint), Nothing,
              (Ref{fqPolyRepRelPowerSeriesRingElem}, Ref{fqPolyRepRelPowerSeriesRingElem}, Int, Ref{fqPolyRepField}),
              c, b, max(0, lenc - b.val + a.val), ctx)
        ccall((:fq_nmod_poly_shift_left, libflint), Nothing,
              (Ref{fqPolyRepRelPowerSeriesRingElem}, Ref{fqPolyRepRelPowerSeriesRingElem}, Int, Ref{fqPolyRepField}),
              c, c, b.val - a.val, ctx)
        ccall((:fq_nmod_poly_add_series, libflint), Nothing,
              (Ref{fqPolyRepRelPowerSeriesRingElem}, Ref{fqPolyRepRelPowerSeriesRingElem},
               Ref{fqPolyRepRelPowerSeriesRingElem}, Int, Ref{fqPolyRepField}),
              c, c, a, lenc, ctx)
      elseif b.val < a.val
        lenc = max(lena + a.val - b.val, lenb)
        ccall((:fq_nmod_poly_set_trunc, libflint), Nothing,
              (Ref{fqPolyRepRelPowerSeriesRingElem}, Ref{fqPolyRepRelPowerSeriesRingElem}, Int, Ref{fqPolyRepField}),
              c, a, max(0, lenc - a.val + b.val), ctx)
        ccall((:fq_nmod_poly_shift_left, libflint), Nothing,
              (Ref{fqPolyRepRelPowerSeriesRingElem}, Ref{fqPolyRepRelPowerSeriesRingElem}, Int, Ref{fqPolyRepField}),
              c, c, a.val - b.val, ctx)
        ccall((:fq_nmod_poly_add_series, libflint), Nothing,
              (Ref{fqPolyRepRelPowerSeriesRingElem}, Ref{fqPolyRepRelPowerSeriesRingElem},
               Ref{fqPolyRepRelPowerSeriesRingElem}, Int, Ref{fqPolyRepField}),
              c, c, b, lenc, ctx)
      else
        lenc = max(lena, lenb)
        ccall((:fq_nmod_poly_add_series, libflint), Nothing,
              (Ref{fqPolyRepRelPowerSeriesRingElem}, Ref{fqPolyRepRelPowerSeriesRingElem},
               Ref{fqPolyRepRelPowerSeriesRingElem}, Int, Ref{fqPolyRepField}),
              c, a, b, lenc, ctx)
      end
      c.prec = prec
      c.val = val
      renormalize!(c)
      return c
    end

    function set_length!(a::fqPolyRepRelPowerSeriesRingElem, n::Int)
      ccall((:_fq_nmod_poly_set_length, libflint), Nothing,
            (Ref{fqPolyRepRelPowerSeriesRingElem}, Int, Ref{fqPolyRepField}),
            a, n, base_ring(a))
      return a
    end

    ###############################################################################
    #
    #   Promotion rules
    #
    ###############################################################################

    promote_rule(::Type{fqPolyRepRelPowerSeriesRingElem}, ::Type{T}) where {T <: Integer} = fqPolyRepRelPowerSeriesRingElem

    promote_rule(::Type{fqPolyRepRelPowerSeriesRingElem}, ::Type{ZZRingElem}) = fqPolyRepRelPowerSeriesRingElem

    promote_rule(::Type{fqPolyRepRelPowerSeriesRingElem}, ::Type{fqPolyRepFieldElem}) = fqPolyRepRelPowerSeriesRingElem

    ###############################################################################
    #
    #   Parent object call overload
    #
    ###############################################################################

    function (a::fqPolyRepRelPowerSeriesRing)()
      ctx = base_ring(a)
      z = fqPolyRepRelPowerSeriesRingElem(ctx)
      z.prec = a.prec_max
      z.val = a.prec_max
      z.parent = a
      return z
    end

    function (a::fqPolyRepRelPowerSeriesRing)(b::Integer)
      return a(base_ring(a)(b))
    end

    function (a::fqPolyRepRelPowerSeriesRing)(b::ZZRingElem)
      return a(base_ring(a)(b))
    end

    function (a::fqPolyRepRelPowerSeriesRing)(b::fqPolyRepFieldElem)
      ctx = base_ring(a)
      if iszero(b)
        z = fqPolyRepRelPowerSeriesRingElem(ctx)
        z.prec = a.prec_max
        z.val = a.prec_max
      else
        z = fqPolyRepRelPowerSeriesRingElem(ctx, [b], 1, a.prec_max, 0)
      end
      z.parent = a
      return z
    end

    function (a::fqPolyRepRelPowerSeriesRing)(b::fqPolyRepRelPowerSeriesRingElem)
      parent(b) != a && error("Unable to coerce power series")
      return b
    end

    function (a::fqPolyRepRelPowerSeriesRing)(b::Vector{fqPolyRepFieldElem}, len::Int, prec::Int, val::Int)
      ctx = base_ring(a)
      z = fqPolyRepRelPowerSeriesRingElem(ctx, b, len, prec, val)
      z.parent = a
      return z
    end
