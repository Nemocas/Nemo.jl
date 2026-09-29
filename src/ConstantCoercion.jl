###############################################################################
#
#   Coercion of constant polynomials and rational functions
#
###############################################################################

const _PolyLikeElem{T} = Union{PolyRingElem{T}, MPolyRingElem{T}, UniversalRingElem{<:MPolyRingElem{T}}}

_coerce_constant(R::Ring, x::RingElement) = R(x)

function _coerce_constant(R::Ring, x::_PolyLikeElem)
  T = elem_type(R)
  is_constant(x) || throw(InexactError(nameof(T), T, x))
  return R(constant_coefficient(x))
end

for P in (ZZRing, QQField, fpField, FpField, FqField, fqPolyRepField, FqPolyRepField,
          zzModRing, ZZModRing, QQBarField)
  T = elem_type(P)

  # `RationalFunctionFieldElem{T}` requires `T <: FieldElement`
  F = Generic.FracFieldElem{<:_PolyLikeElem{T}}
  T <: FieldElement && (F = Union{F, Generic.RationalFunctionFieldElem{T}})

  @eval begin
    (R::$P)(x::_PolyLikeElem{$T}) = _coerce_constant(R, x)

    (R::$P)(x::$F) =
      divexact(_coerce_constant(R, numerator(x)), _coerce_constant(R, denominator(x)))
  end
end
