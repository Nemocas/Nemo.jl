include(joinpath(pathof(AbstractAlgebra), "..", "Aliases.jl"))

# make some Julia names compatible with our naming conventions
@alias is_equal isequal
@alias is_finite isfinite
@alias is_inf isinf
@alias is_integer isinteger
@alias is_less isless
@alias is_real isreal

@alias eigvals_simple eigenvalues_simple # for consistency with eigvals/eigenvalues

# renamed for 0.40.0
Base.@deprecate_binding FlintPadicField PadicField
Base.@deprecate_binding padic PadicFieldElem
Base.@deprecate_binding FlintQadicField QadicField
Base.@deprecate_binding qadic QadicFieldElem

# renamed for 0.41.0
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
