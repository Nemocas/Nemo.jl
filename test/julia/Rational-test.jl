@testset "Rational in-place operations" begin
  a = 1 // 3
  b = 3 // 2
  q = 0 // 1
  n = 0
  d = 0

  q = divexact!(q, a, b)
  n = numerator!(n, q)
  d = denominator!(d, q)

  @test q == 2 // 9 && typeof(q) == Rational{Int}
  @test n == 2 && typeof(n) == Int
  @test d == 9 && typeof(d) == Int
end
