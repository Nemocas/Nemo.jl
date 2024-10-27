###############################################################################
#
#   fq_nmod_abs_series.jl: Absolute series over finite fields
#
###############################################################################

    ###############################################################################
    #
    #   Data type and parent object methods
    #
    ###############################################################################

    function O(a::fqPolyRepAbsPowerSeriesRingElem)
      if iszero(a)
        return deepcopy(a)    # 0 + O(x^n)
      end
      prec = length(a) - 1
      prec < 0 && throw(DomainError(prec, "Valuation must be non-negative"))
      z = fqPolyRepAbsPowerSeriesRingElem(base_ring(a), Vector{fqPolyRepFieldElem}(undef, 0), 0, prec)
      z.parent = parent(a)
      return z
    end

    elem_type(::Type{fqPolyRepAbsPowerSeriesRing}) = fqPolyRepAbsPowerSeriesRingElem

    parent_type(::Type{fqPolyRepAbsPowerSeriesRingElem}) = fqPolyRepAbsPowerSeriesRing

    base_ring(R::fqPolyRepAbsPowerSeriesRing) = R.base_ring

    abs_series_type(::Type{fqPolyRepFieldElem}) = fqPolyRepAbsPowerSeriesRingElem

    var(a::fqPolyRepAbsPowerSeriesRing) = a.S

    ###############################################################################
    #
    #   Basic manipulation
    #
    ###############################################################################

    max_precision(R::fqPolyRepAbsPowerSeriesRing) = R.prec_max

    function normalise(a::fqPolyRepAbsPowerSeriesRingElem, len::Int)
      ctx = base_ring(a)
      if len > 0
        c = base_ring(a)()
        ccall((:fq_nmod_poly_get_coeff, libflint), Nothing,
              (Ref{fqPolyRepFieldElem}, Ref{fqPolyRepAbsPowerSeriesRingElem}, Int, Ref{fqPolyRepField}),
              c, a, len - 1, ctx)
      end
      while len > 0 && iszero(c)
        len -= 1
        if len > 0
          ccall((:fq_nmod_poly_get_coeff, libflint), Nothing,
                (Ref{fqPolyRepFieldElem}, Ref{fqPolyRepAbsPowerSeriesRingElem}, Int, Ref{fqPolyRepField}),
                c, a, len - 1, ctx)
        end
      end

      return len
    end

    function length(x::fqPolyRepAbsPowerSeriesRingElem)
      return ccall((:fq_nmod_poly_length, libflint), Int,
                   (Ref{fqPolyRepAbsPowerSeriesRingElem}, Ref{fqPolyRepField}), x, base_ring(x))
    end

    precision(x::fqPolyRepAbsPowerSeriesRingElem) = x.prec

    function coeff(x::fqPolyRepAbsPowerSeriesRingElem, n::Int)
      if n < 0
        return base_ring(x)()
      end
      z = base_ring(x)()
      ccall((:fq_nmod_poly_get_coeff, libflint), Nothing,
            (Ref{fqPolyRepFieldElem}, Ref{fqPolyRepAbsPowerSeriesRingElem}, Int, Ref{fqPolyRepField}),
            z, x, n, base_ring(x))
      return z
    end

    zero(R::fqPolyRepAbsPowerSeriesRing) = R(0)

    one(R::fqPolyRepAbsPowerSeriesRing) = R(1)

    function gen(R::fqPolyRepAbsPowerSeriesRing)
      S = base_ring(R)
      z = fqPolyRepAbsPowerSeriesRingElem(S, [S(0), S(1)], 2, max_precision(R))
      z.parent = R
      return z
    end

    function deepcopy_internal(a::fqPolyRepAbsPowerSeriesRingElem, dict::IdDict)
      z = fqPolyRepAbsPowerSeriesRingElem(base_ring(a), a)
      z.prec = a.prec
      z.parent = parent(a)
      return z
    end

    function is_gen(a::fqPolyRepAbsPowerSeriesRingElem)
      return precision(a) == 0 || ccall((:fq_nmod_poly_is_gen, libflint), Bool,
                                        (Ref{fqPolyRepAbsPowerSeriesRingElem}, Ref{fqPolyRepField}), a, base_ring(a))
    end

    iszero(a::fqPolyRepAbsPowerSeriesRingElem) = length(a) == 0

    is_unit(a::fqPolyRepAbsPowerSeriesRingElem) = valuation(a) == 0 && is_unit(coeff(a, 0))

    function isone(a::fqPolyRepAbsPowerSeriesRingElem)
      return precision(a) == 0 || ccall((:fq_nmod_poly_is_one, libflint), Bool,
                                        (Ref{fqPolyRepAbsPowerSeriesRingElem}, Ref{fqPolyRepField}), a, base_ring(a))
    end

    # todo: write an fq_poly_valuation
    function valuation(a::fqPolyRepAbsPowerSeriesRingElem)
      for i = 1:length(a)
        if !iszero(coeff(a, i - 1))
          return i - 1
        end
      end
      return precision(a)
    end

    characteristic(R::fqPolyRepAbsPowerSeriesRing) = characteristic(base_ring(R))

    ###############################################################################
    #
    #   Similar
    #
    ###############################################################################

    function similar(f::AbsPowerSeriesRingElem, R::fqPolyRepField, max_prec::Int,
        s::Symbol=var(parent(f)); cached::Bool=true)
      par = fqPolyRepAbsPowerSeriesRing(R, max_prec, s, cached)
      z = fqPolyRepAbsPowerSeriesRingElem(R)
      if base_ring(f) === R && s == var(parent(f)) &&
        f isa fqPolyRepAbsPowerSeriesRingElem && max_precision(parent(f)) == max_prec
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

    function abs_series(R::fqPolyRepField, arr::Vector{T},
        len::Int, prec::Int, var::VarName=:x;
        max_precision::Int=prec, cached::Bool=true) where T
      prec < len && error("Precision too small for given data")
      coeffs = T == fqPolyRepFieldElem ? arr : map(R, arr)
      coeffs = length(coeffs) == 0 ? fqPolyRepFieldElem[] : coeffs
      par = fqPolyRepAbsPowerSeriesRing(R, max_precision, Symbol(var), cached)
      z = fqPolyRepAbsPowerSeriesRingElem(R, coeffs, len, prec)
      z.parent = par
      return z
    end

    ###############################################################################
    #
    #   Unary operators
    #
    ###############################################################################

    function -(x::fqPolyRepAbsPowerSeriesRingElem)
      z = parent(x)()
      ccall((:fq_nmod_poly_neg, libflint), Nothing,
            (Ref{fqPolyRepAbsPowerSeriesRingElem}, Ref{fqPolyRepAbsPowerSeriesRingElem}, Ref{fqPolyRepField}),
            z, x, base_ring(x))
      z.prec = x.prec
      return z
    end

    ###############################################################################
    #
    #   Binary operators
    #
    ###############################################################################

    function +(a::fqPolyRepAbsPowerSeriesRingElem, b::fqPolyRepAbsPowerSeriesRingElem)
      check_parent(a, b)
      lena = length(a)
      lenb = length(b)
      prec = min(a.prec, b.prec)
      lena = min(lena, prec)
      lenb = min(lenb, prec)
      lenz = max(lena, lenb)
      z = parent(a)()
      z.prec = prec
      ccall((:fq_nmod_poly_add_series, libflint), Nothing,
            (Ref{fqPolyRepAbsPowerSeriesRingElem}, Ref{fqPolyRepAbsPowerSeriesRingElem},
             Ref{fqPolyRepAbsPowerSeriesRingElem}, Int, Ref{fqPolyRepField}),
            z, a, b, lenz, base_ring(a))
      return z
    end

    function -(a::fqPolyRepAbsPowerSeriesRingElem, b::fqPolyRepAbsPowerSeriesRingElem)
      check_parent(a, b)
      lena = length(a)
      lenb = length(b)
      prec = min(a.prec, b.prec)
      lena = min(lena, prec)
      lenb = min(lenb, prec)
      lenz = max(lena, lenb)
      z = parent(a)()
      z.prec = prec
      ccall((:fq_nmod_poly_sub_series, libflint), Nothing,
            (Ref{fqPolyRepAbsPowerSeriesRingElem}, Ref{fqPolyRepAbsPowerSeriesRingElem},
             Ref{fqPolyRepAbsPowerSeriesRingElem}, Int, Ref{fqPolyRepField}),
            z, a, b, lenz, base_ring(a))
      return z
    end

    function *(a::fqPolyRepAbsPowerSeriesRingElem, b::fqPolyRepAbsPowerSeriesRingElem)
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
      ccall((:fq_nmod_poly_mullow, libflint), Nothing,
            (Ref{fqPolyRepAbsPowerSeriesRingElem}, Ref{fqPolyRepAbsPowerSeriesRingElem},
             Ref{fqPolyRepAbsPowerSeriesRingElem}, Int, Ref{fqPolyRepField}),
            z, a, b, lenz, base_ring(a))
      return z
    end

    ###############################################################################
    #
    #   Ad hoc binary operators
    #
    ###############################################################################

    function *(x::fqPolyRepFieldElem, y::fqPolyRepAbsPowerSeriesRingElem)
      z = parent(y)()
      z.prec = y.prec
      ccall((:fq_nmod_poly_scalar_mul_fq_nmod, libflint), Nothing,
            (Ref{fqPolyRepAbsPowerSeriesRingElem}, Ref{fqPolyRepAbsPowerSeriesRingElem}, Ref{fqPolyRepFieldElem}, Ref{fqPolyRepField}),
            z, y, x, base_ring(y))
      return z
    end

    *(x::fqPolyRepAbsPowerSeriesRingElem, y::fqPolyRepFieldElem) = y * x

    ###############################################################################
    #
    #   Shifting
    #
    ###############################################################################

    function shift_left(x::fqPolyRepAbsPowerSeriesRingElem, len::Int)
      len < 0 && throw(DomainError(len, "Shift must be non-negative"))
      xlen = length(x)
      z = parent(x)()
      z.prec = x.prec + len
      z.prec = min(z.prec, max_precision(parent(x)))
      zlen = min(z.prec, xlen + len)
      ccall((:fq_nmod_poly_shift_left, libflint), Nothing,
            (Ref{fqPolyRepAbsPowerSeriesRingElem}, Ref{fqPolyRepAbsPowerSeriesRingElem}, Int, Ref{fqPolyRepField}),
            z, x, len, base_ring(x))
      ccall((:fq_nmod_poly_set_trunc, libflint), Nothing,
            (Ref{fqPolyRepAbsPowerSeriesRingElem}, Ref{fqPolyRepAbsPowerSeriesRingElem}, Int, Ref{fqPolyRepField}),
            z, z, zlen, base_ring(x))
      return z
    end

    function shift_right(x::fqPolyRepAbsPowerSeriesRingElem, len::Int)
      len < 0 && throw(DomainError(len, "Shift must be non-negative"))
      xlen = length(x)
      z = parent(x)()
      if len >= xlen
        z.prec = max(0, x.prec - len)
      else
        z.prec = x.prec - len
        ccall((:fq_nmod_poly_shift_right, libflint), Nothing,
              (Ref{fqPolyRepAbsPowerSeriesRingElem}, Ref{fqPolyRepAbsPowerSeriesRingElem}, Int, Ref{fqPolyRepField}),
              z, x, len, base_ring(x))
      end
      return z
    end

    ###############################################################################
    #
    #   Truncation
    #
    ###############################################################################

    function truncate(x::fqPolyRepAbsPowerSeriesRingElem, k::Int)
      return truncate!(deepcopy(x), k)
    end

    function truncate!(x::fqPolyRepAbsPowerSeriesRingElem, k::Int)
      k < 0 && throw(DomainError(k, "Index must be non-negative"))
      if precision(x) <= k
        return x
      end
      ccall((:fq_nmod_poly_truncate, libflint), Nothing,
            (Ref{fqPolyRepAbsPowerSeriesRingElem}, Int, Ref{fqPolyRepField}),
            x, k, base_ring(x))
      x.prec = k
      return x
    end

    ###############################################################################
    #
    #   Powering
    #
    ###############################################################################

    function ^(a::fqPolyRepAbsPowerSeriesRingElem, b::Int)
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

    function ==(x::fqPolyRepAbsPowerSeriesRingElem, y::fqPolyRepAbsPowerSeriesRingElem)
      check_parent(x, y)
      prec = min(x.prec, y.prec)
      n = max(length(x), length(y))
      n = min(n, prec)
      return Bool(ccall((:fq_nmod_poly_equal_trunc, libflint), Cint,
                        (Ref{fqPolyRepAbsPowerSeriesRingElem}, Ref{fqPolyRepAbsPowerSeriesRingElem}, Int, Ref{fqPolyRepField}),
                        x, y, n, base_ring(x)))
    end

    function isequal(x::fqPolyRepAbsPowerSeriesRingElem, y::fqPolyRepAbsPowerSeriesRingElem)
      if parent(x) != parent(y)
        return false
      end
      if x.prec != y.prec || length(x) != length(y)
        return false
      end
      return Bool(ccall((:fq_nmod_poly_equal, libflint), Cint,
                        (Ref{fqPolyRepAbsPowerSeriesRingElem}, Ref{fqPolyRepAbsPowerSeriesRingElem}, Ref{fqPolyRepField}),
                        x, y, base_ring(x)))
    end

    ###############################################################################
    #
    #   Ad hoc comparisons
    #
    ###############################################################################

    function ==(x::fqPolyRepAbsPowerSeriesRingElem, y::fqPolyRepFieldElem)
      if length(x) > 1
        return false
      elseif length(x) == 1
        z = base_ring(x)()
        ccall((:fq_nmod_poly_get_coeff, libflint), Nothing,
              (Ref{fqPolyRepFieldElem}, Ref{fqPolyRepAbsPowerSeriesRingElem}, Int, Ref{fqPolyRepField}),
              z, x, 0, base_ring(x))
        return z == y
      else
        return precision(x) == 0 || iszero(y)
      end
    end

    ==(x::fqPolyRepFieldElem, y::fqPolyRepAbsPowerSeriesRingElem) = y == x

    function ==(x::fqPolyRepAbsPowerSeriesRingElem, y::ZZRingElem)
      if length(x) > 1
        return false
      elseif length(x) == 1
        z = base_ring(x)()
        ccall((:fq_nmod_poly_get_coeff, libflint), Nothing,
              (Ref{fqPolyRepFieldElem}, Ref{fqPolyRepAbsPowerSeriesRingElem}, Int, Ref{fqPolyRepField}),
              z, x, 0, base_ring(x))
        return z == y
      else
        return precision(x) == 0 || iszero(y)
      end
    end

    ==(x::ZZRingElem, y::fqPolyRepAbsPowerSeriesRingElem) = y == x

    ==(x::fqPolyRepAbsPowerSeriesRingElem, y::Integer) = x == ZZRingElem(y)

    ==(x::Integer, y::fqPolyRepAbsPowerSeriesRingElem) = y == x

    ###############################################################################
    #
    #   Exact division
    #
    ###############################################################################

    function divexact(x::fqPolyRepAbsPowerSeriesRingElem, y::fqPolyRepAbsPowerSeriesRingElem; check::Bool=true)
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
      ccall((:fq_nmod_poly_div_series, libflint), Nothing,
            (Ref{fqPolyRepAbsPowerSeriesRingElem}, Ref{fqPolyRepAbsPowerSeriesRingElem},
             Ref{fqPolyRepAbsPowerSeriesRingElem}, Int, Ref{fqPolyRepField}),
            z, x, y, prec, base_ring(x))
      return z
    end

    ###############################################################################
    #
    #   Ad hoc exact division
    #
    ###############################################################################

    function divexact(x::fqPolyRepAbsPowerSeriesRingElem, y::fqPolyRepFieldElem; check::Bool=true)
      iszero(y) && throw(DivideError())
      z = parent(x)()
      z.prec = x.prec
      ccall((:fq_nmod_poly_scalar_div_fq_nmod, libflint), Nothing,
            (Ref{fqPolyRepAbsPowerSeriesRingElem}, Ref{fqPolyRepAbsPowerSeriesRingElem}, Ref{fqPolyRepFieldElem}, Ref{fqPolyRepField}),
            z, x, y, base_ring(x))
      return z
    end

    ###############################################################################
    #
    #   Inversion
    #
    ###############################################################################

    function inv(a::fqPolyRepAbsPowerSeriesRingElem)
      iszero(a) && throw(DivideError())
      !is_unit(a) && error("Unable to invert power series")
      ainv = parent(a)()
      ainv.prec = a.prec
      ccall((:fq_nmod_poly_inv_series, libflint), Nothing,
            (Ref{fqPolyRepAbsPowerSeriesRingElem}, Ref{fqPolyRepAbsPowerSeriesRingElem}, Int, Ref{fqPolyRepField}),
            ainv, a, a.prec, base_ring(a))
      return ainv
    end

    ###############################################################################
    #
    #   Square root
    #
    ###############################################################################

    function sqrt_classical_char2(a::fqPolyRepAbsPowerSeriesRingElem; check::Bool=true)
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

    function sqrt_classical(a::fqPolyRepAbsPowerSeriesRingElem; check::Bool=true)
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
      ccall((:fq_nmod_poly_sqrt_series, libflint), Nothing,
            (Ref{fqPolyRepAbsPowerSeriesRingElem}, Ref{fqPolyRepAbsPowerSeriesRingElem}, Int, Ref{fqPolyRepField}),
            z, a, a.prec, base_ring(a))
      if !isone(s)
        z *= s
      end
      if !iszero(v)
        z = shift_left(z, div(v, 2))
      end
      return true, z
    end

    function Base.sqrt(a::fqPolyRepAbsPowerSeriesRingElem; check::Bool=true)
      flag, s = sqrt_classical(a; check=check)
      check && !flag && error("Not a square")
      return s
    end

    function is_square(a::fqPolyRepAbsPowerSeriesRingElem)
      flag, s = sqrt_classical(a; check=true)
      return flag
    end

    function is_square_with_sqrt(a::fqPolyRepAbsPowerSeriesRingElem)
      return sqrt_classical(a; check=true)
    end

    ###############################################################################
    #
    #   Unsafe functions
    #
    ###############################################################################

    function zero!(z::fqPolyRepAbsPowerSeriesRingElem)
      ccall((:fq_nmod_poly_zero, libflint), Nothing,
            (Ref{fqPolyRepAbsPowerSeriesRingElem}, Ref{fqPolyRepField}), z, base_ring(z))
      z.prec = parent(z).prec_max
      return z
    end

    function one!(z::fqPolyRepAbsPowerSeriesRingElem)
      ccall((:fq_nmod_poly_one, libflint), Nothing,
            (Ref{fqPolyRepAbsPowerSeriesRingElem}, Ref{fqPolyRepField}), z, base_ring(z))
      z.prec = parent(z).prec_max
      return z
    end

    function fit!(z::fqPolyRepAbsPowerSeriesRingElem, n::Int)
      ccall((:fq_nmod_poly_fit_length, libflint), Nothing,
            (Ref{fqPolyRepAbsPowerSeriesRingElem}, Int, Ref{fqPolyRepField}),
            z, n, base_ring(z))
      return nothing
    end

    function setcoeff!(z::fqPolyRepAbsPowerSeriesRingElem, n::Int, x::fqPolyRepFieldElem)
      ccall((:fq_nmod_poly_set_coeff, libflint), Nothing,
            (Ref{fqPolyRepAbsPowerSeriesRingElem}, Int, Ref{fqPolyRepFieldElem}, Ref{fqPolyRepField}),
            z, n, x, base_ring(z))
      return z
    end

    function mul!(z::fqPolyRepAbsPowerSeriesRingElem, a::fqPolyRepAbsPowerSeriesRingElem, b::fqPolyRepAbsPowerSeriesRingElem)
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
      ccall((:fq_nmod_poly_mullow, libflint), Nothing,
            (Ref{fqPolyRepAbsPowerSeriesRingElem}, Ref{fqPolyRepAbsPowerSeriesRingElem},
             Ref{fqPolyRepAbsPowerSeriesRingElem}, Int, Ref{fqPolyRepField}),
            z, a, b, lenz, base_ring(z))
      return z
    end

    function add!(c::fqPolyRepAbsPowerSeriesRingElem, a::fqPolyRepAbsPowerSeriesRingElem, b::fqPolyRepAbsPowerSeriesRingElem)
      lena = length(a)
      lenb = length(b)
      prec = min(a.prec, b.prec)
      lena = min(lena, prec)
      lenb = min(lenb, prec)
      lenc = max(lena, lenb)
      c.prec = prec
      ccall((:fq_nmod_poly_add_series, libflint), Nothing,
            (Ref{fqPolyRepAbsPowerSeriesRingElem}, Ref{fqPolyRepAbsPowerSeriesRingElem},
             Ref{fqPolyRepAbsPowerSeriesRingElem}, Int, Ref{fqPolyRepField}),
            c, a, b, lenc, base_ring(a))
      return c
    end

    function set_length!(a::fqPolyRepAbsPowerSeriesRingElem, n::Int)
      ccall((:_fq_nmod_poly_set_length, libflint), Nothing,
            (Ref{fqPolyRepAbsPowerSeriesRingElem}, Int, Ref{fqPolyRepField}),
            a, n, base_ring(a))
      return a
    end

    ###############################################################################
    #
    #   Promotion rules
    #
    ###############################################################################

    promote_rule(::Type{fqPolyRepAbsPowerSeriesRingElem}, ::Type{T}) where {T <: Integer} = fqPolyRepAbsPowerSeriesRingElem

    promote_rule(::Type{fqPolyRepAbsPowerSeriesRingElem}, ::Type{fqPolyRepFieldElem}) = fqPolyRepAbsPowerSeriesRingElem

    promote_rule(::Type{fqPolyRepAbsPowerSeriesRingElem}, ::Type{ZZRingElem}) = fqPolyRepAbsPowerSeriesRingElem

    ###############################################################################
    #
    #   Parent object call overload
    #
    ###############################################################################

    function (a::fqPolyRepAbsPowerSeriesRing)()
      ctx = base_ring(a)
      z = fqPolyRepAbsPowerSeriesRingElem(ctx)
      z.prec = a.prec_max
      z.parent = a
      return z
    end

    function (a::fqPolyRepAbsPowerSeriesRing)(b::Integer)
      return a(base_ring(a)(b))
    end

    function (a::fqPolyRepAbsPowerSeriesRing)(b::ZZRingElem)
      return a(base_ring(a)(b))
    end

    function (a::fqPolyRepAbsPowerSeriesRing)(b::fqPolyRepFieldElem)
      ctx = base_ring(a)
      if iszero(b)
        z = fqPolyRepAbsPowerSeriesRingElem(ctx)
        z.prec = a.prec_max
      else
        z = fqPolyRepAbsPowerSeriesRingElem(ctx, [b], 1, a.prec_max)
      end
      z.parent = a
      return z
    end

    function (a::fqPolyRepAbsPowerSeriesRing)(b::fqPolyRepAbsPowerSeriesRingElem)
      parent(b) != a && error("Unable to coerce power series")
      return b
    end

    function (a::fqPolyRepAbsPowerSeriesRing)(b::Vector{fqPolyRepFieldElem}, len::Int, prec::Int)
      ctx = base_ring(a)
      z = fqPolyRepAbsPowerSeriesRingElem(ctx, b, len, prec)
      z.parent = a
      return z
    end


###############################################################################
#
#   power_series_ring constructor
#
###############################################################################

function power_series_ring(R::fqPolyRepField, prec::Int, s::VarName; model::Symbol=:capped_relative, cached::Bool = true)
  if model == :capped_relative
    parent_obj = fqPolyRepRelPowerSeriesRing(R, prec, Symbol(s), cached)
  elseif model == :capped_absolute
    parent_obj = fqPolyRepAbsPowerSeriesRing(R, prec, Symbol(s), cached)
  else
    error("Unknown model")
  end

  return parent_obj, gen(parent_obj)
end

function AbsPowerSeriesRing(R::fqPolyRepField, prec::Int)
  return fqPolyRepAbsPowerSeriesRing(R, prec, :x, false)
end

function RelPowerSeriesRing(R::fqPolyRepField, prec::Int)
  return fqPolyRepRelPowerSeriesRing(R, prec, :x, false)
end
