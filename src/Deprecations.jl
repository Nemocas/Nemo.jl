###############################################################################
#
#   Aliases
#
###############################################################################

# ALL aliases here are only a temporary measure to allow for a smooth transition downstream.
# they will be replaced by deprecations eventually



###############################################################################
#
#   Deprecated bindings
#
###############################################################################

# Deprecated bindings don't get reexported automatically in Hecke/Oscar/etc.
# By calling this macro from the respective packages, we can ensure that the deprecated bindings are available there.
macro include_deprecated_bindings()
  return esc(quote
               # renamed and deprecated for 0.40.0
               Base.@deprecate_binding FlintPadicField PadicField
               Base.@deprecate_binding padic PadicFieldElem
               Base.@deprecate_binding FlintQadicField QadicField
               Base.@deprecate_binding qadic QadicFieldElem

               # renamed and deprecated for 0.41.0
               Base.@deprecate_binding arb_poly ArbPolyRingElem
               Base.@deprecate_binding arb_mat ArbMatrix
               Base.@deprecate_binding arb ArbFieldElem
               Base.@deprecate_binding acb_poly AcbPolyRingElem
               Base.@deprecate_binding acb_mat AcbMatrix
               Base.@deprecate_binding acb AcbFieldElem
               Base.@deprecate_binding ca CalciumFieldElem
               Base.@deprecate_binding Loc LocalizedEuclideanRing
               Base.@deprecate_binding LocElem LocalizedEuclideanRingElem
               Base.@deprecate_binding lll_ctx LLLContext
               Base.@deprecate_binding qqbar QQBarFieldElem
               Base.@deprecate_binding CalciumQQBarField QQBarField
               Base.@deprecate_binding FlintQQiField Nemo.QQiField false
               Base.@deprecate_binding fmpqi Nemo.QQiFieldElem false
               Base.@deprecate_binding FlintZZiRing Nemo.ZZiRing false
               Base.@deprecate_binding fmpzi Nemo.ZZiRingElem false
               Base.@deprecate_binding fmpzUnitRange ZZRingElemUnitRange
               Base.@deprecate_binding AnticNumberField AbsSimpleNumField
               Base.@deprecate_binding nf_elem AbsSimpleNumFieldElem

               # depreacted in 0.46
               Base.@deprecate_binding QQBar QQBarField()
               Base.@deprecate_binding CalciumQQBar QQBarField()

               Base.@deprecate_binding ComplexPoly ComplexPolyRingElem
               Base.@deprecate_binding RealPoly RealPolyRingElem
               Base.@deprecate_binding ComplexMat ComplexMatrix
               Base.@deprecate_binding RealMat RealMatrix
               Base.@deprecate_binding AcbMatSpace AcbMatrixSpace
               Base.@deprecate_binding ComplexMatSpace ComplexMatrixSpace
               Base.@deprecate_binding ArbMatSpace ArbMatrixSpace
               Base.@deprecate_binding RealMatSpace RealMatrixSpace
             end)
end

@include_deprecated_bindings()

###############################################################################
#
#   Deprecations
#
###############################################################################

# Deprecated in 0.39.*
@deprecate divisible(x::Int, y::Int) is_divisible_by(x, y)
@deprecate divisible(x::ZZRingElem, y::Int) is_divisible_by(x, y)
@deprecate divisible(x::ZZRingElem, y::ZZRingElem) is_divisible_by(x, y)

# Deprecated in 0.45.*
@deprecate defining_polynomial(Q::fqPolyRepField, P::Ring) defining_polynomial(P, Q)

# Deprecated in 0.46
@deprecate lift(a::PadicFieldElem) lift(ZZ, a)
@deprecate prime_field(k::PadicField) base_field(k)

function (R::QadicField)(n::ZZPolyRingElem, pr::Int)
  Base.depwarn("`(::QadicField)(::ZZPolyRingElem, ::Int)` is deprecated, use `(::QadicField)(::ZZPolyRingElem; precision::Int)` instead.", :QadicField)
  return (R::QadicField)(n::ZZPolyRingElem; precision=pr)
end

@deprecate is_power(x::IntegerUnion) is_perfect_power_with_data(x)
@deprecate is_power(x::QQFieldElem) is_perfect_power_with_data(x)
@deprecate is_power(x::Rational) is_perfect_power_with_data(x)

# Deprecated in 0.47
@deprecate coefficient_ring(k::PadicField) base_field(k)
@deprecate coefficient_ring(k::QadicField) base_field(k)
