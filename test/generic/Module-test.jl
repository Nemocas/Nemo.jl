function rand_module(R::Ring, vals...)
   rk = rand(0:5)
   M = free_module(R, rk)
   levels = rand(0:3)
   for i = 1:levels
      if ngens(M) == 0
         break
      end
      G = [rand(M, vals...) for i in 1:rand(1:ngens(M))]
      S, f = sub(M, G)
      if rand(1:2) == 1
         M, f = quo(M, S)
      else
         M = S
      end
   end
   return M
end

@testset "Module.invariant_factors" begin
   for R in [ZZ, QQ]
      for iter = 1:100
         M = rand_module(R, -10:10)

         I, f = snf(M)

         K, g = kernel(f)

         @test length(invariant_factors(K)) == 0

         m = rand(I, -10:10)

         @test m == inv(f)(f(m))
      end
   end 
end
