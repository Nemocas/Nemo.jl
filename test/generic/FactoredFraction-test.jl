@testset "FactoredFracFieldElem.adhoc_binary" begin
  F = Generic.FactoredFractionField(ZZ)
  a = F(3)

  @test a*ZZ(2) == F(6)
  @test ZZ(2)*a == F(6)
  @test a + ZZ(2) == F(5)
  @test ZZ(2) + a == F(5)
  @test a - ZZ(2) == F(1)
  @test ZZ(2) - a == F(-1)
  @test divexact(a, ZZ(3)) == F(1)
  @test divexact(ZZ(6), a) == F(2)
end
