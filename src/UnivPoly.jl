###############################################################################
#
#   Specialized methods for universal polynomials
#
###############################################################################

# These methods speed up some computations with specific universal
# polynomial rings which would otherwise be handled by more generic
# code. This mainly concerns rings over the rationals.

denominator(f::UniversalPolyRingElem{QQFieldElem}) = denominator(data(f))

for op in (:+, :*, :-)
  @eval begin
    $op(a::T, b::ZZRingElem) where {T <: Generic.UnivPoly} = T($op(data(a), b), parent(a))
    $op(a::ZZRingElem, b::T) where {T <: Generic.UnivPoly} = T($op(a, data(b)), parent(b))

    # to avoid ambiguity
    $op(a::T, b::ZZRingElem) where {T <: Generic.UnivPoly{ZZRingElem}} = T($op(data(a), b), parent(a))
    $op(a::ZZRingElem, b::T) where {T <: Generic.UnivPoly{ZZRingElem}} = T($op(a, data(b)), parent(b))
  end
end
