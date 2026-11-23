################################################################################
#
#   Square root
#
################################################################################

# `sqrt`, `is_square` and `is_square_with_sqrt` come from AbstractAlgebra and
# call `sqrt_classical`.

function sqrt_classical(a::FlintFieldRelPowerSeriesRingElem; check::Bool=true)
  S = parent(a)
  R = base_ring(a)
  v = valuation(a)
  z = S()
  v2 = div(v, 2)
  if iszero(a)
    z.prec = precision(a) - v2
    z.val = z.prec
    return true, z
  end
  if check && !iseven(v)
    return false, S()
  end
  if characteristic(R) == 2
    return sqrt_classical_char2(a; check=check)
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
  z = _sqrt_series!(z, a, precision(a) - v)
  if !isone(s)
    z *= s
  end
  return true, z
end

function sqrt_classical_char2(a::FlintFinFieldRelPowerSeriesRingElem; check::Bool=true)
  S = parent(a)
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
    for i in 1:2:precision(a) - aval - 1 # series must have even exponents
      if !iszero(polcoeff(a, i))
        return false, S()
      end
    end
  end
  for i in 0:prec - aval2 - 1
    asqrt = setcoeff!(asqrt, i, _sqrt_char2(polcoeff(a, 2*i)))
  end
  return true, asqrt
end

# Every element of a finite field of characteristic 2 is a square;
# over `GF(2)` it is its own root.
_sqrt_char2(c::Union{fpFieldElem, FpFieldElem}) = c
_sqrt_char2(c::FinFieldElem) = sqrt(c)
