@testset "UnivPoly.specialized_methods" begin
  R = universal_polynomial_ring(QQ)
  x = gen(R, :x)

  @test denominator(1//2*x+1//3*x^2) == 6
  @test denominator(2//3*x) == 3
end
