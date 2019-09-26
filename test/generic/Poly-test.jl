@testset "Poly.binary_ops_delayed_reduction..." begin
   S, t = PolynomialRing(QQ, "t")
   K, a = NumberField(t^3 + 3t + 1, "a")
   R, x = PolynomialRing(K, "x")

   for iter = 1:100
      f = rand(R, 0:10, -10:10)
      g = rand(R, 0:10, -10:10)
      h = rand(R, 0:10, -10:10)

      @test f*g == g*f
      @test f*(g + h) == f*g + f*h
      @test (f + g)*(f - g) == f*f - g*g
   end
end

@testset "Poly.truncation_delayed_reduction..." begin
   S, t = PolynomialRing(QQ, "t")
   K, a = NumberField(t^3 + 3t + 1, "a")
   R, x = PolynomialRing(K, "x")

   for iter = 1:300
      f = rand(R, 0:10, -10:10)
      g = rand(R, 0:10, -10:10)
      n = rand(0:20)

      @test truncate(f*g, n) == mullow(f, g, n)
   end
end
