###############################################################################
#
#   Specialized methods for universal polynomials
#
###############################################################################

# These methods speed up some computations with specific universal
# polynomial rings which would otherwise be handled by more generic
# code. This mainly concerns rings over the rationals.

denominator(f::UniversalPolyRingElem{QQFieldElem}) = denominator(data(f))

function +(p::Generic.UnivPoly{T}, n::ZZRingElem) where {T}
  S = parent(p)
  return Generic.UnivPoly{T}(data(p)+n, S)
end

+(n::ZZRingElem, p::Generic.UnivPoly) = p+n

function -(p::Generic.UnivPoly{T}, n::ZZRingElem) where {T}
  S = parent(p)
  return Generic.UnivPoly{T}(data(p)-n, S)
end

function -(n::ZZRingElem, p::Generic.UnivPoly{T}) where {T}
  S = parent(p)
  return Generic.UnivPoly{T}(n-data(p), S)
end

function *(p::Generic.UnivPoly{T}, n::ZZRingElem) where {T}
  S = parent(p)
  return Generic.UnivPoly{T}(data(p)*n, S)
end

*(n::ZZRingElem, p::Generic.UnivPoly) = p*n
