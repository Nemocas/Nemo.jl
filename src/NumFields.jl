prime_field(_::NumField) = QQField()

is_cyclo_type(::NumField) = false


AbstractAlgebra.promote_rule(::Type{S}, ::Type{ZZRingElem}) where {S<:NumFieldElem} = S

AbstractAlgebra.promote_rule(::Type{ZZRingElem}, ::Type{S}) where {S<:NumFieldElem} = S

AbstractAlgebra.promote_rule(::Type{S}, ::Type{QQFieldElem}) where {S<:NumFieldElem} = S

AbstractAlgebra.promote_rule(::Type{QQFieldElem}, ::Type{S}) where {S<:NumFieldElem} = S

################################################################################
#
#  Base case for dot products
#
################################################################################

dot(x::ZZRingElem, y::NumFieldElem) = x * y

dot(x::Integer, y::NumFieldElem) = x * y

dot(x::NumFieldElem, y::Integer) = x * y

function dot(a::Vector{<:NumFieldElem}, b::Vector{ZZRingElem})
  d = zero(parent(a[1]))
  t = zero(d)
  for i = 1:length(a)
    mul!(t, a[i], b[i])
    add!(d, d, t)
  end
  return d
end

function ^(x::NumFieldElem, y::ZZRingElem)
  if fits(Int, y)
    return x^Int(y)
  end

  return _power(x, y)
end

# We test once if it fits, otherwise we would have to check for every ^-call
function _power(x::NumFieldElem, y::ZZRingElem)
  res = parent(x)()
  if y < 0
    res = _power(inv(x), -y)
  elseif y == 0
    res = parent(x)(1)
  elseif y == 1
    res = deepcopy(x)
  elseif mod(y, 2) == 0
    z = _power(x, Base.div(y, 2))
    res = z * z
  else
    res = _power(x, y - 1) * x
  end
  return res
end

