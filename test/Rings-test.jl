const ring_to_mat = Dict(FlintZZ                         => ZZMatrix,
                         FlintQQ                         => QQMatrix,
                         residue_ring(ZZ, 9)[1]          => zzModMatrix,
                         GF(5)                           => fpMatrix,
                         finite_field(3, 2, "b")[1]      => fqPolyRepMatrix,
                         finite_field(ZZRingElem(3), 2, "b")[1] => FqPolyRepMatrix,
                         ArbField()                      => ArbMatrix,
                         AcbField()                      => AcbMatrix,
                         RealField()                     => RealMat,
                         ComplexField()                  => ComplexMat,
                         )

include("flint/fmpz-test.jl")
include("flint/fmpz_poly-test.jl")
include("flint/fmpz_mod_poly-test.jl")
include("flint/gfp_fmpz_poly-test.jl")
include("flint/nmod-test.jl")
include("flint/fmpz_mod-test.jl")
include("flint/nmod_poly-test.jl")
include("flint/gfp_poly-test.jl")
include("flint/fmpq_poly-test.jl")
include("flint/fq_poly-test.jl")
include("flint/fq_nmod_poly-test.jl")
include("flint/fq_default_poly-test.jl")
include("flint/fmpz_rel_series-test.jl")
include("flint/fmpz_abs_series-test.jl")
include("flint/fmpz_laurent_series-test.jl")
include("flint/fmpz_puiseux_series-test.jl")
include("flint/fmpq_rel_series-test.jl")
include("flint/fmpq_abs_series-test.jl")
include("flint/nmod_abs_series-test.jl")
include("flint/gfp_abs_series-test.jl")
include("flint/fmpz_mod_abs_series-test.jl")
include("flint/gfp_fmpz_abs_series-test.jl")
include("flint/nmod_rel_series-test.jl")
include("flint/gfp_rel_series-test.jl")
include("flint/fmpz_mod_rel_series-test.jl")
include("flint/gfp_fmpz_rel_series-test.jl")
include("flint/fq_rel_series-test.jl")
include("flint/fq_abs_series-test.jl")
include("flint/fq_nmod_rel_series-test.jl")
include("flint/fq_nmod_abs_series-test.jl")
include("flint/fq_default_abs_series-test.jl")
include("flint/fq_default_rel_series-test.jl")
include("flint/nmod_mat-test.jl")
include("flint/fmpz_mod_mat-test.jl")
include("flint/gfp_mat-test.jl")
include("flint/gfp_fmpz_mat-test.jl")
include("flint/fq_mat-test.jl")
include("flint/fq_nmod_mat-test.jl")
include("flint/fq_default_mat-test.jl")
include("flint/fmpz_mat-test.jl")
include("flint/fmpq_mat-test.jl")

include("ArbFieldElem/ArbPolyRingElem-test.jl")
include("ArbFieldElem/RealPoly-test.jl")
include("ArbFieldElem/AcbPolyRingElem-test.jl")
include("ArbFieldElem/ComplexPoly-test.jl")
include("ArbFieldElem/ArbMatrix-test.jl")
include("ArbFieldElem/RealMat-test.jl")
include("ArbFieldElem/AcbMatrix-test.jl")
include("ArbFieldElem/ComplexMat-test.jl")

include("flint/fmpz_mpoly-test.jl")
include("flint/fmpq_mpoly-test.jl")
include("flint/nmod_mpoly-test.jl")
include("flint/gfp_mpoly-test.jl")
include("flint/gfp_fmpz_mpoly-test.jl")
include("flint/fq_nmod_mpoly-test.jl")
include("flint/fq_default_mpoly-test.jl")

include("gaussiannumbers/ZZiRingElem-test.jl")
