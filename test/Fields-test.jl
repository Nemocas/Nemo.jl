include("generic/Fraction-test.jl")

include("julia/gfelem-test.jl")
include("flint/fmpq-test.jl")
include("flint/fq-test.jl")
include("flint/fq_nmod-test.jl")
include("flint/padic-test.jl")

include("antic/nf_elem-test.jl")

include("arb/arb-test.jl")
include("arb/acb-test.jl")

function test_fields()
   test_gfelem()

   test_arb()
   test_acb()

   test_gen_frac()

   test_fmpq()
   test_fq()
   test_fq_nmod()
   test_padic()

   test_nf_elem()
end
