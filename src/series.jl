################################################################################
#
#   Square root
#
################################################################################

@doc raw"""
    sqrt(a::FlintRelPowerSeriesRingElem)

Return the square root of the power series $a$. By default the function raises
an exception if the input is not a square. If `check=false` this check is
omitted.
"""
function Base.sqrt(a::FlintRelPowerSeriesRingElem; check::Bool=true)
  flag, q = sqrt_classical(a; check=check)
  if check && !flag
    error("Not a square in sqrt")
  end
  return q
end

function is_square(a::FlintRelPowerSeriesRingElem)
  flag, q = sqrt_classical(a; check=true)
  return flag
end

function is_square_with_sqrt(a::FlintRelPowerSeriesRingElem)
  return sqrt_classical(a; check=true)
end
