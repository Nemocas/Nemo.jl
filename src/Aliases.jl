include(joinpath(pathof(AbstractAlgebra), "..", "Aliases.jl"))

# make some Julia names compatible with our naming conventions
@alias is_equal isequal
@alias is_finite isfinite
@alias is_inf isinf
@alias is_integer isinteger
@alias is_less isless
@alias is_real isreal

@alias eigvals_simple eigenvalues_simple # for consistency with eigvals/eigenvalues

# for backwards compatibility
Base.@deprecate_binding isalgebraic is_algebraic
Base.@deprecate_binding isalgebraic_integer is_algebraic_integer
Base.@deprecate_binding iscyclo_type is_cyclo_type
Base.@deprecate_binding isdiagonal is_diagonal
Base.@deprecate_binding isembedded is_embedded
Base.@deprecate_binding isequal_abs is_equal_abs
Base.@deprecate_binding isequal_abs_imag is_equal_abs_imag
Base.@deprecate_binding isequal_abs_real is_equal_abs_real
Base.@deprecate_binding isequal_imag is_equal_imag
Base.@deprecate_binding isequal_real is_equal_real
Base.@deprecate_binding isexact is_exact
Base.@deprecate_binding ishadamard is_hadamard
Base.@deprecate_binding isimaginary is_imaginary
Base.@deprecate_binding isless_abs is_less_abs
Base.@deprecate_binding isless_abs_imag is_less_abs_imag
Base.@deprecate_binding isless_abs_real is_less_abs_real
Base.@deprecate_binding isless_imag is_less_imag
Base.@deprecate_binding isless_real is_less_real
Base.@deprecate_binding isless_root_order is_less_root_order
Base.@deprecate_binding islower_triangular is_lower_triangular
Base.@deprecate_binding ismaxreal_type is_maxreal_type
Base.@deprecate_binding isnilpotent is_nilpotent
Base.@deprecate_binding isnonnegative is_nonnegative
Base.@deprecate_binding isnonpositive is_nonpositive
Base.@deprecate_binding isnonzero is_nonzero
Base.@deprecate_binding isnumber is_number
Base.@deprecate_binding ispositive is_positive
Base.@deprecate_binding ispositive_entry is_positive_entry
Base.@deprecate_binding isprime is_prime
Base.@deprecate_binding isrational is_rational
Base.@deprecate_binding isroot_of_unity is_root_of_unity
Base.@deprecate_binding issigned_inf is_signed_inf
Base.@deprecate_binding isuinf is_uinf
Base.@deprecate_binding isundefined is_undefined
Base.@deprecate_binding isunknown is_unknown
Base.@deprecate_binding isupper_triangular is_upper_triangular

# old type names from before 0.33
# may be remove at some point in the future
Base.@deprecate_binding fmpz ZZRingElem
Base.@deprecate_binding fmpz_mat ZZMatrix
Base.@deprecate_binding fmpz_mpoly ZZMPolyRingElem
Base.@deprecate_binding fmpz_poly ZZPolyRingElem

Base.@deprecate_binding fmpq QQFieldElem
Base.@deprecate_binding fmpq_mat QQMatrix
Base.@deprecate_binding fmpq_mpoly QQMPolyRingElem
Base.@deprecate_binding fmpq_poly QQPolyRingElem

Base.@deprecate_binding fmpz_mod ZZModRingElem
Base.@deprecate_binding fmpz_mod_mat ZZModMatrix
Base.@deprecate_binding fmpz_mod_poly ZZModPolyRingElem

Base.@deprecate_binding nmod zzModRingElem
Base.@deprecate_binding nmod_mat zzModMatrix
Base.@deprecate_binding nmod_mpoly zzModMPolyRingElem
Base.@deprecate_binding nmod_poly zzModPolyRingElem

Base.@deprecate_binding fq_default FqFieldElem
Base.@deprecate_binding fq_default_mat FqMatrix
Base.@deprecate_binding fq_default_mpoly FqMPolyRingElem
Base.@deprecate_binding fq_default_poly FqPolyRingElem

Base.@deprecate_binding gfp_fmpz_elem FpFieldElem
Base.@deprecate_binding gfp_fmpz_mat FpMatrix
Base.@deprecate_binding gfp_fmpz_mpoly FpMPolyRingElem
Base.@deprecate_binding gfp_fmpz_poly FpPolyRingElem

Base.@deprecate_binding gfp_elem fpFieldElem
Base.@deprecate_binding gfp_mat fpMatrix
Base.@deprecate_binding gfp_mpoly fpMPolyRingElem
Base.@deprecate_binding gfp_poly fpPolyRingElem

Base.@deprecate_binding fq FqPolyRepFieldElem
Base.@deprecate_binding fq_mat FqPolyRepMatrix
Base.@deprecate_binding fq_poly FqPolyRepPolyRingElem

Base.@deprecate_binding fq_nmod fqPolyRepFieldElem
Base.@deprecate_binding fq_nmod_mat fqPolyRepMatrix
Base.@deprecate_binding fq_nmod_mpoly fqPolyRepMPolyRingElem
Base.@deprecate_binding fq_nmod_poly fqPolyRepPolyRingElem

Base.@deprecate_binding FlintIntegerRing ZZRing
Base.@deprecate_binding FmpzMatSpace ZZMatrixSpace
Base.@deprecate_binding FmpzMPolyRing ZZMPolyRing
Base.@deprecate_binding FmpzPolyRing ZZPolyRing

Base.@deprecate_binding FlintRationalField QQField
Base.@deprecate_binding FmpqMatSpace QQMatrixSpace
Base.@deprecate_binding FmpqMPolyRing QQMPolyRing
Base.@deprecate_binding FmpqPolyRing QQPolyRing

Base.@deprecate_binding FmpzModRing ZZModRing
Base.@deprecate_binding FmpzModMatSpace ZZModMatrixSpace
Base.@deprecate_binding FmpzModPolyRing ZZModPolyRing

Base.@deprecate_binding NmodRing zzModRing
Base.@deprecate_binding NmodMatSpace zzModMatrixSpace
Base.@deprecate_binding NmodMPolyRing zzModMPolyRing
Base.@deprecate_binding NmodPolyRing zzModPolyRing

Base.@deprecate_binding FqDefaultFiniteField FqField
Base.@deprecate_binding FqDefaultMatSpace FqMatrixSpace
Base.@deprecate_binding FqDefaultMPolyRing FqMPolyRing
Base.@deprecate_binding FqDefaultPolyRing FqPolyRing

Base.@deprecate_binding GaloisFmpzField FpField
Base.@deprecate_binding GaloisFmpzMatSpace FpMatrixSpace
Base.@deprecate_binding GFPFmpzMPolyRing FpMPolyRing
Base.@deprecate_binding GFPFmpzPolyRing FpPolyRing

Base.@deprecate_binding GaloisField fpField
Base.@deprecate_binding GFPMatSpace fpMatrixSpace
Base.@deprecate_binding GFPMPolyRing fpMPolyRing
Base.@deprecate_binding GFPPolyRing fpPolyRing

Base.@deprecate_binding FqFiniteField FqPolyRepField
Base.@deprecate_binding FqMatSpace FqPolyRepMatrixSpace
#Base.@deprecate_binding FqPolyRing FqPolyRepPolyRing        # DISABLED because FqPolyRing also is new name

Base.@deprecate_binding FqNmodFiniteField fqPolyRepField
Base.@deprecate_binding FqNmodMatSpace fqPolyRepMatrixSpace
Base.@deprecate_binding FqNmodMPolyRing fqPolyRepMPolyRing
Base.@deprecate_binding FqNmodPolyRing fqPolyRepPolyRing

Base.@deprecate_binding fmpq_abs_series QQAbsPowerSeriesRingElem
Base.@deprecate_binding fmpq_rel_series QQRelPowerSeriesRingElem
Base.@deprecate_binding FmpqAbsSeriesRing QQAbsPowerSeriesRing
Base.@deprecate_binding FmpqRelSeriesRing QQRelPowerSeriesRing

Base.@deprecate_binding fmpz_abs_series ZZAbsPowerSeriesRingElem
Base.@deprecate_binding fmpz_rel_series ZZRelPowerSeriesRingElem
Base.@deprecate_binding FmpzAbsSeriesRing ZZAbsPowerSeriesRing
Base.@deprecate_binding FmpzRelSeriesRing ZZRelPowerSeriesRing

Base.@deprecate_binding fmpz_laurent_series ZZLaurentSeriesRingElem
Base.@deprecate_binding FmpzLaurentSeriesRing ZZLaurentSeriesRing

Base.@deprecate_binding fmpz_mod_abs_series ZZModAbsPowerSeriesRingElem
Base.@deprecate_binding fmpz_mod_rel_series ZZModRelPowerSeriesRingElem
Base.@deprecate_binding FmpzModAbsSeriesRing ZZModAbsPowerSeriesRing
Base.@deprecate_binding FmpzModRelSeriesRing ZZModRelPowerSeriesRing

Base.@deprecate_binding fq_default_abs_series FqAbsPowerSeriesRingElem
Base.@deprecate_binding fq_default_rel_series FqRelPowerSeriesRingElem
Base.@deprecate_binding FqDefaultAbsSeriesRing FqAbsPowerSeriesRing
Base.@deprecate_binding FqDefaultRelSeriesRing FqRelPowerSeriesRing

Base.@deprecate_binding fq_abs_series FqPolyRepAbsPowerSeriesRingElem
Base.@deprecate_binding fq_rel_series FqPolyRepRelPowerSeriesRingElem
Base.@deprecate_binding FqAbsSeriesRing FqPolyRepAbsPowerSeriesRing
Base.@deprecate_binding FqRelSeriesRing FqPolyRepRelPowerSeriesRing

Base.@deprecate_binding fq_nmod_abs_series fqPolyRepAbsPowerSeriesRingElem
Base.@deprecate_binding fq_nmod_rel_series fqPolyRepRelPowerSeriesRingElem
Base.@deprecate_binding FqNmodAbsSeriesRing fqPolyRepAbsPowerSeriesRing
Base.@deprecate_binding FqNmodRelSeriesRing fqPolyRepRelPowerSeriesRing

Base.@deprecate_binding gfp_abs_series fpAbsPowerSeriesRingElem
Base.@deprecate_binding gfp_rel_series fpRelPowerSeriesRingElem
Base.@deprecate_binding GFPAbsSeriesRing fpAbsPowerSeriesRing
Base.@deprecate_binding GFPRelSeriesRing fpRelPowerSeriesRing

Base.@deprecate_binding gfp_fmpz_abs_series FpAbsPowerSeriesRingElem
Base.@deprecate_binding gfp_fmpz_rel_series FpRelPowerSeriesRingElem
Base.@deprecate_binding GFPFmpzAbsSeriesRing FpAbsPowerSeriesRing
Base.@deprecate_binding GFPFmpzRelSeriesRing FpRelPowerSeriesRing

Base.@deprecate_binding nmod_abs_series zzModAbsPowerSeriesRingElem
Base.@deprecate_binding nmod_rel_series zzModRelPowerSeriesRingElem
Base.@deprecate_binding NmodAbsSeriesRing zzModAbsPowerSeriesRing
Base.@deprecate_binding NmodRelSeriesRing zzModRelPowerSeriesRing

Base.@deprecate_binding FiniteField finite_field

Base.@deprecate_binding NumberField number_field

Base.@deprecate_binding CyclotomicRealSubfield cyclotomic_real_subfield

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
Base.@deprecate_binding FlintQQiField QQiField
Base.@deprecate_binding fmpqi QQiFieldElem
Base.@deprecate_binding FlintZZiRing ZZiRing
Base.@deprecate_binding fmpzi ZZiRingElem
Base.@deprecate_binding fmpzUnitRange ZZRingElemUnitRange
Base.@deprecate_binding AnticNumberField AbsSimpleNumField
Base.@deprecate_binding nf_elem AbsSimpleNumFieldElem
