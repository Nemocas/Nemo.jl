const ring_to_mat = Dict(FlintZZ                         => fmpz_mat,
                         FlintQQ                         => fmpq_mat,
                         ResidueRing(ZZ, 9)              => nmod_mat,
                         GF(5)                           => gfp_mat,
                         FiniteField(3, 2, "b")[1]       => fq_nmod_mat,
                         FiniteField(fmpz(3), 2, "b")[1] => fq_mat,
                         ArbField(64)                    => arb_mat,
                         AcbField(64)                    => acb_mat,
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
include("flint/fmpz_rel_series-test.jl")
include("flint/fmpz_abs_series-test.jl")
include("flint/fmpz_laurent_series-test.jl")
include("flint/fmpz_puiseux_series-test.jl")
include("flint/fmpq_rel_series-test.jl")
include("flint/fmpq_abs_series-test.jl")
include("flint/fmpz_mod_abs_series-test.jl")
include("flint/nmod_rel_series-test.jl")
include("flint/fmpz_mod_rel_series-test.jl")
include("flint/fq_rel_series-test.jl")
include("flint/fq_abs_series-test.jl")
include("flint/fq_nmod_rel_series-test.jl")
include("flint/fq_nmod_abs_series-test.jl")
include("flint/nmod_mat-test.jl")
include("flint/gfp_mat-test.jl")
include("flint/fq_mat-test.jl")
include("flint/fq_nmod_mat-test.jl")
include("flint/fmpz_mat-test.jl")
include("flint/fmpq_mat-test.jl")

include("arb/arb_poly-test.jl")
include("arb/acb_poly-test.jl")
include("arb/arb_mat-test.jl")
include("arb/acb_mat-test.jl")

include("flint/fmpz_mpoly-test.jl")
include("flint/fmpq_mpoly-test.jl")
include("flint/nmod_mpoly-test.jl")
