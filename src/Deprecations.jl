# Deprecated in 0.39.*

@deprecate divisible(x::Int, y::Int) is_divisible_by(x, y)
@deprecate divisible(x::ZZRingElem, y::Int) is_divisible_by(x, y)
@deprecate divisible(x::ZZRingElem, y::ZZRingElem) is_divisible_by(x, y)
