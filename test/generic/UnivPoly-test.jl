@testset "UnivPoly.specialized_methods" begin
  R = universal_polynomial_ring(QQ)
  x = gen(R, :x)

  @test denominator(1//2*x+1//3*x^2) == 6
  @test denominator(2//3*x) == 3

  @test ZZ(1)+x == 1+x
  @test x+ZZ(1) == x+1

  @test x-ZZ(1) == x-1
  @test ZZ(1)-x == 1-x

  @test ZZ(2)*x == 2*x
  @test x*ZZ(2) == x*2
end
