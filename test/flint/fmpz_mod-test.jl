@testset "fmpz_mod.gcdx..." begin
   for i = 1:100
      n = rand(1:24)
      R = ResidueRing(ZZ, ZZ(n))

      for iter = 1:100
         a = rand(R, 0:n-1)
         b = rand(R, 0:n-1)

         g, s, t = gcdx(a, b)

         @test g == s*a + t*b
      end
   end
end
