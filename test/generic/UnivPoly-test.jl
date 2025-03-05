@testset "UnivPoly.denominator" begin
  R = universal_polynomial_ring(QQ)
  x = gen(R, :x)

  @test denominator(1//2*x+1//3*x^2) == 6
  @test denominator(2//3*x) == 3
end

@testset "UnivPoly.specialized_methods over $R" for R in (ZZ, QQ)
  S = universal_polynomial_ring(R)
  x = gen(S, :x)

  @test ZZ(1)+x == 1+x
  @test x+ZZ(1) == x+1

  @test x-ZZ(1) == x-1
  @test ZZ(1)-x == 1-x

  @test ZZ(2)*x == 2*x
  @test x*ZZ(2) == x*2
end
