@testset "Factor squarefree" begin
  K, a = cyclotomic_field(3, "a")
  Kx, x = K["x"]
  f = (x - 1)^2 * x^2 * (x + 1)
  sf = factor_squarefree(f)
  @test length(sf) == 2
  # This is broken in AbstractAlgebra https://github.com/Nemocas/AbstractAlgebra.jl/pull/1799
  #@test all(is_squarefree, keys(sf.fac))
  @test evaluate(sf) == f
end
