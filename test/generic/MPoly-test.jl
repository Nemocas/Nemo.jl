@testset "MPoly.binary_ops_delayed_reduction..." begin
   S, t = PolynomialRing(QQ, "t")
   K, a = NumberField(t^3 + 3t + 1, "a")

   for num_vars = 1:10
      var_names = ["x$j" for j in 1:num_vars]
      ord = rand_ordering()

      S, varlist = PolynomialRing(K, var_names, ordering = ord)

      for iter = 1:10
         f = rand(S, 0:5, 0:100, -100:100)
         g = rand(S, 0:5, 0:100, -100:100)
         h = rand(S, 0:5, 0:100, -100:100)

         @test f*g == g*f
         @test f*g + f*h == f*(g + h)
         @test f*g - f*h == f*(g - h)

         @test f*g == Generic.mul_classical(f, g)
      end
   end
end

@testset "MPoly.powering_delayed_reduction..." begin
   S, t = PolynomialRing(QQ, "t")
   K, a = NumberField(t^3 + 3t + 1, "a")

   for num_vars = 1:10
      var_names = ["x$j" for j in 1:num_vars]
      ord = rand_ordering()

      S, varlist = PolynomialRing(K, var_names, ordering = ord)

      for iter = 1:10
         f = rand(S, 0:5, 0:100, -100:100)

         expn = rand(0:10)

         r = S(1)
         for i = 1:expn
            r *= f
         end

         @test (f == 0 && expn == 0 && f^expn == 0) || f^expn == r
      end
   end
end

@testset "MPoly.divides_delayed_reduction..." begin
   S, t = PolynomialRing(QQ, "t")
   K, a = NumberField(t^3 + 3t + 1, "a")

   for num_vars = 1:10
      var_names = ["x$j" for j in 1:num_vars]
      ord = rand_ordering()

      S, varlist = PolynomialRing(K, var_names, ordering = ord)

      for iter = 1:10
         f = S(0)
         f = rand(S, 0:5, 0:100, -100:100)
         g = rand(S, 0:5, 0:100, -100:100)

         p = f*g

         flag, q = divides(p, f)
         flag2, q2 = divides(f, p)

         @test flag == true

         @test q * f == p

         q1 = divexact(p, f)

         @test q1 * f == p

         if !iszero(p)
           @test q1 == g
         end
      end
   end
end

@testset "MPoly.euclidean_division_delayed_reduction..." begin
   S, t = PolynomialRing(QQ, "t")
   K, a = NumberField(t^3 + 3t + 1, "a")

   for num_vars = 1:10
      var_names = ["x$j" for j in 1:num_vars]
      ord = rand_ordering()

      S, varlist = PolynomialRing(K, var_names, ordering = ord)

      for iter = 1:10
         f = S(0)
         while iszero(f)
            f = rand(S, 0:5, 0:100, -100:100)
         end
         g = rand(S, 0:5, 0:100, -100:100)

         p = f*g

         q1, r = divrem(p, f)
         q2 = div(p, f)

         @test q1 == g
         @test q2 == g
         @test f*q1 + r == p

         q3, r3 = divrem(g, f)
         q4 = div(g, f)
         flag, q5 = divides(g, f)

         @test q3*f + r3 == g
         @test q3 == q4
         @test (r3 == 0 && flag == true && q5 == q3) || (r3 != 0 && flag == false)
      end
   end
end
