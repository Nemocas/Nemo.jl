# make some Julia names compatible with our naming conventions
@alias is_equal isequal
@alias is_finite isfinite
@alias is_inf isinf
@alias is_integer isinteger
@alias is_less isless
@alias is_real isreal

# for backwards compatibility
@alias isalgebraic is_algebraic
@alias isalgebraic_integer is_algebraic_integer
@alias iscyclo_type is_cyclo_type
@alias isdiagonal is_diagonal
@alias isembedded is_embedded
@alias isequal_abs is_equal_abs
@alias isequal_abs_imag is_equal_abs_imag
@alias isequal_abs_real is_equal_abs_real
@alias isequal_imag is_equal_imag
@alias isequal_real is_equal_real
@alias isexact is_exact
@alias ishadamard is_hadamard
@alias isimaginary is_imaginary
@alias isless_abs is_less_abs
@alias isless_abs_imag is_less_abs_imag
@alias isless_abs_real is_less_abs_real
@alias isless_imag is_less_imag
@alias isless_real is_less_real
@alias isless_root_order is_less_root_order
@alias islower_triangular is_lower_triangular
@alias ismaxreal_type is_maxreal_type
@alias isnilpotent is_nilpotent
@alias isnonnegative is_nonnegative
@alias isnonpositive is_nonpositive
@alias isnonzero is_nonzero
@alias isnumber is_number
@alias ispositive is_positive
@alias ispositive_entry is_positive_entry
@alias isprime is_prime
@alias isrational is_rational
@alias isroot_of_unity is_root_of_unity
@alias issigned_inf is_signed_inf
@alias isuinf is_uinf
@alias isundefined is_undefined
@alias isunknown is_unknown
@alias isupper_triangular is_upper_triangular
@alias iszero_row is_zero_row

# old type names from before 0.33
# may be remove at some point in the future
@alias fmpz ZZRingElem
@alias fmpz_mat ZZMatrix
@alias fmpz_mpoly ZZMPolyRingElem
@alias fmpz_poly ZZPolyRingElem

@alias fmpq QQFieldElem
@alias fmpq_mat QQMatrix
@alias fmpq_mpoly QQMPolyRingElem
@alias fmpq_poly QQPolyRingElem

@alias fmpz_mod ZZModRingElem
@alias fmpz_mod_mat ZZModMatrix
@alias fmpz_mod_poly ZZModPolyRingElem

@alias nmod zzModRingElem
@alias nmod_mat zzModMatrix
@alias nmod_mpoly zzModMPolyRingElem
@alias nmod_poly zzModPolyRingElem

@alias fq_default FqFieldElem
@alias fq_default_mat FqMatrix
@alias fq_default_mpoly FqMPolyRingElem
@alias fq_default_poly FqPolyRingElem

@alias gfp_fmpz_elem FpFieldElem
@alias gfp_fmpz_mat FpMatrix
@alias gfp_fmpz_mpoly FpMPolyRingElem
@alias gfp_fmpz_poly FpPolyRingElem

@alias gfp_elem fpFieldElem
@alias gfp_mat fpMatrix
@alias gfp_mpoly fpMPolyRingElem
@alias gfp_poly fpPolyRingElem

@alias fq FqPolyRepFieldElem
@alias fq_mat FqPolyRepMatrix
@alias fq_poly FqPolyRepPolyRingElem

@alias fq_nmod fqPolyRepFieldElem
@alias fq_nmod_mat fqPolyRepMatrix
@alias fq_nmod_mpoly fqPolyRepMPolyRingElem
@alias fq_nmod_poly fqPolyRepPolyRingElem

@alias FlintIntegerRing ZZRing
@alias FmpzMatSpace ZZMatrixSpace
@alias FmpzMPolyRing ZZMPolyRing
@alias FmpzPolyRing ZZPolyRing

@alias FlintRationalField QQField
@alias FmpqMatSpace QQMatrixSpace
@alias FmpqMPolyRing QQMPolyRing
@alias FmpqPolyRing QQPolyRing

@alias FmpzModRing ZZModRing
@alias FmpzModMatSpace ZZModMatrixSpace
@alias FmpzModPolyRing ZZModPolyRing

@alias NmodRing zzModRing
@alias NmodMatSpace zzModMatrixSpace
@alias NmodMPolyRing zzModMPolyRing
@alias NmodPolyRing zzModPolyRing

@alias FqDefaultFiniteField FqField
@alias FqDefaultMatSpace FqMatrixSpace
@alias FqDefaultMPolyRing FqMPolyRing
@alias FqDefaultPolyRing FqPolyRing

@alias GaloisFmpzField FpField
@alias GaloisFmpzMatSpace FpMatrixSpace
@alias GFPFmpzMPolyRing FpMPolyRing
@alias GFPFmpzPolyRing FpPolyRing

@alias GaloisField fpField
@alias GFPMatSpace fpMatrixSpace
@alias GFPMPolyRing fpMPolyRing
@alias GFPPolyRing fpPolyRing

@alias FqFiniteField FqPolyRepField
@alias FqMatSpace FqPolyRepMatrixSpace
#@alias FqPolyRing FqPolyRepPolyRing        # DISABLED because FqPolyRing also is new name

@alias FqNmodFiniteField fqPolyRepField
@alias FqNmodMatSpace fqPolyRepMatrixSpace
@alias FqNmodMPolyRing fqPolyRepMPolyRing
@alias FqNmodPolyRing fqPolyRepPolyRing

@alias fmpq_abs_series QQAbsPowerSeriesRingElem
@alias fmpq_rel_series QQRelPowerSeriesRingElem
@alias FmpqAbsSeriesRing QQAbsPowerSeriesRing
@alias FmpqRelSeriesRing QQRelPowerSeriesRing

@alias fmpz_abs_series ZZAbsPowerSeriesRingElem
@alias fmpz_rel_series ZZRelPowerSeriesRingElem
@alias FmpzAbsSeriesRing ZZAbsPowerSeriesRing
@alias FmpzRelSeriesRing ZZRelPowerSeriesRing

@alias fmpz_laurent_series ZZLaurentSeriesRingElem
@alias FmpzLaurentSeriesRing ZZLaurentSeriesRing

@alias fmpz_mod_abs_series ZZModAbsPowerSeriesRingElem
@alias fmpz_mod_rel_series ZZModRelPowerSeriesRingElem
@alias FmpzModAbsSeriesRing ZZModAbsPowerSeriesRing
@alias FmpzModRelSeriesRing ZZModRelPowerSeriesRing

@alias fq_default_abs_series FqAbsPowerSeriesRingElem
@alias fq_default_rel_series FqRelPowerSeriesRingElem
@alias FqDefaultAbsSeriesRing FqAbsPowerSeriesRing
@alias FqDefaultRelSeriesRing FqRelPowerSeriesRing

@alias fq_abs_series FqPolyRepAbsPowerSeriesRingElem
@alias fq_rel_series FqPolyRepRelPowerSeriesRingElem
@alias FqAbsSeriesRing FqPolyRepAbsPowerSeriesRing
@alias FqRelSeriesRing FqPolyRepRelPowerSeriesRing

@alias fq_nmod_abs_series fqPolyRepAbsPowerSeriesRingElem
@alias fq_nmod_rel_series fqPolyRepRelPowerSeriesRingElem
@alias FqNmodAbsSeriesRing fqPolyRepAbsPowerSeriesRing
@alias FqNmodRelSeriesRing fqPolyRepRelPowerSeriesRing

@alias gfp_abs_series fpAbsPowerSeriesRingElem
@alias gfp_rel_series fpRelPowerSeriesRingElem
@alias GFPAbsSeriesRing fpAbsPowerSeriesRing
@alias GFPRelSeriesRing fpRelPowerSeriesRing

@alias gfp_fmpz_abs_series FpAbsPowerSeriesRingElem
@alias gfp_fmpz_rel_series FpRelPowerSeriesRingElem
@alias GFPFmpzAbsSeriesRing FpAbsPowerSeriesRing
@alias GFPFmpzRelSeriesRing FpRelPowerSeriesRing

@alias nmod_abs_series zzModAbsPowerSeriesRingElem
@alias nmod_rel_series zzModRelPowerSeriesRingElem
@alias NmodAbsSeriesRing zzModAbsPowerSeriesRing
@alias NmodRelSeriesRing zzModRelPowerSeriesRing

@alias MPolyElem MPolyRingElem

@alias FiniteField finite_field
