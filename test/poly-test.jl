@testset "Factor squarefree" begin
  K, a = cyclotomic_field(3, "a")
  Kx, x = K["x"]
  f = (x - 1)^2 * x^2 * (x + 1)
  sf = factor_squarefree(f)
  @test length(sf) == 2
  @test all(is_squarefree, keys(sf.fac))
  @test evaluate(sf) == f

  sf = factor_squarefree(Kx(1))
  @test length(sf) == 0
  @test evaluate(sf) == Kx(1)
  sf = factor_squarefree(x)
  @test length(sf) == 1
  @test evaluate(sf) == x
end

@testset "Squarefreeness" begin
  R, x = QQ["x"]
  @test !is_squarefree(R(0))
  @test is_squarefree(R(3))
  @test is_squarefree(x)
  @test !is_squarefree(x^2)
  @test is_squarefree(2*x)
  @test !is_squarefree(2*x^2)

  R, x = ZZ["x"]
  @test !is_squarefree(R(0))
  @test is_squarefree(R(3))
  @test is_squarefree(x)
  @test !is_squarefree(x^2)
  @test is_squarefree(2*x)
  @test !is_squarefree(2*x^2)
end
