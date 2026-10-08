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

    # to avoid ambiguity: the `n::U` argument of AbstractAlgebra's methods for
    # `UniversalRingElem{T, U}` is covariant, so a `ZZRingElem` there only
    # forces `ZZRingElem <: U`. Both bounds are needed to cover exactly that
    # overlap -- `Generic.UnivPoly{ZZRingElem}` alone leaves the ambiguity.
    $op(a::T, b::ZZRingElem) where {ZZRingElem <: U <: RingElem, T <: Generic.UnivPoly{U}} = T($op(data(a), b), parent(a))
    $op(a::ZZRingElem, b::T) where {ZZRingElem <: U <: RingElem, T <: Generic.UnivPoly{U}} = T($op(a, data(b)), parent(b))
  end
end
