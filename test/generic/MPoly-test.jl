function test_gen_mpoly_constructors()
   print("Generic.MPoly.constructors...")

   R, x = JuliaZZ["y"]

   for num_vars = 1:10
      var_names = ["x$j" for j in 1:num_vars]
      ord = rand_ordering()

      S, varlist = PolynomialRing(R, var_names, ordering = ord)

      @test elem_type(S) == Generic.MPoly{elem_type(R)}
      @test elem_type(Generic.MPolyRing{elem_type(R)}) == Generic.MPoly{elem_type(R)}
      @test parent_type(Generic.MPoly{elem_type(R)}) == Generic.MPolyRing{elem_type(R)}

      @test typeof(S) <: Generic.MPolyRing

      isa(vars(S), Array{Symbol, 1})

      for j = 1:num_vars
         @test isa(varlist[j], MPolyElem)
         @test isa(gens(S)[j], MPolyElem)
      end

      f =  rand(S, 0:5, 0:100, 0:0, -100:100)

      @test isa(f, MPolyElem)

      @test isa(S(2), MPolyElem)

      @test isa(S(R(2)), MPolyElem)

      @test isa(S(f), MPolyElem)

      V = [R(rand(-100:100)) for i in 1:5]

      W0 = UInt[rand(0:100) for i in 1:5*num_vars]
      W = reshape(W0, num_vars, 5)

      @test isa(S(V, W), MPolyElem)
   end

   println("PASS")
end

function test_gen_mpoly_manipulation()
   print("Generic.MPoly.manipulation...")

   R, x = JuliaZZ["y"]

   for num_vars = 1:10
      var_names = ["x$j" for j in 1:num_vars]
      ord = rand_ordering()

      S, varlist = PolynomialRing(R, var_names, ordering = ord)
      g = gens(S)

      @test !isgen(S(1))

      for i = 1:num_vars
         @test isgen(varlist[i])
         @test isgen(g[i])
         @test !isgen(g[i] + 1)
      end

      f = rand(S, 0:5, 0:100, 0:0, -100:100)

      @test f == deepcopy(f)

      if length(f) > 0
         @test isa(coeff(f, rand(1:length(f)) - 1), elem_type(R))
      end

      max_degs, biggest = max_degrees(f)
      deg = isdegree(ordering(S))
      rev = isreverse(ordering(S))

      if deg
         @test max_degs[1] <= 100*num_vars
         @test max_degs[1] == biggest
         @test S.N == num_vars + 1
      else
         @test S.N == num_vars
      end

      if rev
         @test S.N == num_vars + 1
      end

      for j = Int(deg) + 1:num_vars + Int(deg)
         @test max_degs[j] <= 100
         @test max_degs[j] <= biggest
      end

      @test ord == ordering(S)

      @test nvars(f) == num_vars

      @test isone(one(S))

      @test iszero(zero(S))

      @test isconstant(S(rand(-100:100)))
      @test isconstant(S(zero(S)))

      g = rand(S, 1:1, 0:100, 0:0, 1:100)
      h = rand(S, 1:1, 0:100, 0:0, 1:1)

      @test isterm(g)
      @test ismonomial(h)
   end

   println("PASS")
end

function test_gen_mpoly_unary_ops()
   print("Generic.MPoly.unary_ops...")

   R, x = JuliaZZ["y"]

   for num_vars = 1:10
      var_names = ["x$j" for j in 1:num_vars]
      ord = rand_ordering()

      S, varlist = PolynomialRing(R, var_names, ordering = ord)

      for iter = 1:10
         f = rand(S, 0:5, 0:100, 0:0, -100:100)

         @test f == -(-f)
      end
   end

   println("PASS")
end

function test_gen_mpoly_binary_ops()
   print("Generic.MPoly.binary_ops...")

   R, x = JuliaZZ["y"]

   for num_vars = 1:10
      var_names = ["x$j" for j in 1:num_vars]
      ord = rand_ordering()

      S, varlist = PolynomialRing(R, var_names, ordering = ord)

      for iter = 1:10
         f = rand(S, 0:5, 0:100, 0:0, -100:100)
         g = rand(S, 0:5, 0:100, 0:0, -100:100)
         h = rand(S, 0:5, 0:100, 0:0, -100:100)

         @test f + g == g + f
         @test f - g == -(g - f)
         @test f*g == g*f
         @test f*g + f*h == f*(g + h)
         @test f*g - f*h == f*(g - h)

         @test f*g == Nemo.mul_classical(f, g)
      end
   end

   println("PASS")
end

function test_gen_mpoly_adhoc_binary()
   print("Generic.MPoly.adhoc_binary...")

   R, x = JuliaZZ["y"]

   for num_vars = 1:10
      var_names = ["x$j" for j in 1:num_vars]
      ord = rand_ordering()

      S, varlist = PolynomialRing(R, var_names, ordering = ord)

      for iter = 1:100
         f = rand(S, 0:5, 0:100, 0:0, -100:100)

         d1 = rand(-20:20)
         d2 = rand(-20:20)
         g1 = rand(R, 0:2, -10:10)
         g2 = rand(R, 0:2, -10:10)

         @test f*d1 + f*d2 == (d1 + d2)*f
         @test f*BigInt(d1) + f*BigInt(d2) == (BigInt(d1) + BigInt(d2))*f
         @test f*g1 + f*g2 == (g1 + g2)*f

         @test f + d1 + d2 == d1 + d2 + f
         @test f + BigInt(d1) + BigInt(d2) == BigInt(d1) + BigInt(d2) + f
         @test f + g1 + g2 == g1 + g2 + f

         @test f - d1 - d2 == -((d1 + d2) - f)
         @test f - BigInt(d1) - BigInt(d2) == -((BigInt(d1) + BigInt(d2)) - f)
         @test f - g1 - g2 == -((g1 + g2) - f)

         @test f + d1 - d1 == f
         @test f + BigInt(d1) - BigInt(d1) == f
         @test f + g1 - g1 == f
      end
   end

   println("PASS")
end

function test_gen_mpoly_adhoc_comparison()
   print("Generic.MPoly.adhoc_comparison...")

   R, x = JuliaZZ["y"]

   for num_vars = 1:10
      var_names = ["x$j" for j in 1:num_vars]
      ord = rand_ordering()

      S, varlist = PolynomialRing(R, var_names, ordering = ord)

      for iter = 1:100
         d = rand(-100:100)
         g = rand(R, 0:2, -10:10)

         @test S(d) == d
         @test d == S(d)
         @test S(d) == BigInt(d)
         @test BigInt(d) == S(d)
         @test S(g) == g
         @test g == S(g)
      end
   end

   println("PASS")
end

function test_gen_mpoly_powering()
   print("Generic.MPoly.powering...")

   R, x = JuliaZZ["y"]

   for num_vars = 1:10
      var_names = ["x$j" for j in 1:num_vars]
      ord = rand_ordering()

      S, varlist = PolynomialRing(R, var_names, ordering = ord)

      for iter = 1:10
         f = rand(S, 0:5, 0:100, 0:0, -100:100)

         expn = rand(0:10)

         r = S(1)
         for i = 1:expn
            r *= f
         end

         @test (f == 0 && expn == 0 && f^expn == 0) || f^expn == r
      end
   end

   println("PASS")
end

function test_gen_mpoly_divides()
   print("Generic.MPoly.divides...")

   R, x = JuliaZZ["y"]

   for num_vars = 1:10
      var_names = ["x$j" for j in 1:num_vars]
      ord = rand_ordering()

      S, varlist = PolynomialRing(R, var_names, ordering = ord)

      for iter = 1:10
         f = S(0)
         while iszero(f)
            f = rand(S, 0:5, 0:100, 0:0, -100:100)
         end
         g = rand(S, 0:5, 0:100, 0:0, -100:100)

         p = f*g

         flag, q = divides(p, f)

         @test flag == true
         @test q == g

         q = divexact(p, f)

         @test q == g
      end
   end

   println("PASS")
end

function test_gen_mpoly_euclidean_division()
   print("Generic.MPoly.euclidean_division...")

   R, x = JuliaQQ["y"]

   for num_vars = 1:10
      var_names = ["x$j" for j in 1:num_vars]
      ord = rand_ordering()

      S, varlist = PolynomialRing(R, var_names, ordering = ord)

      for iter = 1:10
         f = S(0)
         while iszero(f)
            f = rand(S, 0:5, 0:100, 0:0, -100:100)
         end
         g = rand(S, 0:5, 0:100, 0:0, -100:100)

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

   println("PASS")
end

function test_gen_mpoly_ideal_reduction()
   print("Generic.MPoly.ideal_reduction...")

   R, x = JuliaQQ["y"]

   for num_vars = 1:10
      var_names = ["x$j" for j in 1:num_vars]
      ord = rand_ordering()

      S, varlist = PolynomialRing(R, var_names, ordering = ord)

      for iter = 1:10
         f = S(0)
         while iszero(f)
            f = rand(S, 0:5, 0:100, 0:0, -100:100)
         end
         g = rand(S, 0:5, 0:100, 0:0, -100:100)

         p = f*g

         q1, r = divrem(p, [f])

         @test q1[1] == g
         @test r == 0
      end

      for iter = 1:10
         num = rand(1:5)

         V = Array{elem_type(S)}(num)

         for i = 1:num
            V[i] = S(0)
            while iszero(V[i])
               V[i] = rand(S, 0:5, 0:100, 0:0, -100:100)
            end
         end
         g = rand(S, 0:5, 0:100, 0:0, -100:100)

         q, r = divrem(g, V)

         p = r
         for i = 1:num
            p += q[i]*V[i]
         end

         @test p == g
      end
   end

   println("PASS")
end

function test_gen_mpoly_gcd()
   print("Generic.MPoly.gcd...")

   for num_vars = 1:4
      var_names = ["x$j" for j in 1:num_vars]
      ord = rand_ordering()

      S, varlist = PolynomialRing(JuliaZZ, var_names, ordering = ord)

      for iter = 1:10
         f = rand(S, 0:4, 0:5, -10:10)
         g = rand(S, 0:4, 0:5, -10:10)
         h = rand(S, 0:4, 0:5, -10:10)

         g1 = gcd(f, g)
         g2 = gcd(f*h, g*h)

         @test g2 == g1*h || g2 == -g1*h
      end
   end

   println("PASS")
end

function test_gen_mpoly_evaluation()
   print("Generic.MPoly.evaluation...")

   R, x = JuliaZZ["y"]

   for num_vars = 1:10
      var_names = ["x$j" for j in 1:num_vars]
      ord = rand_ordering()

      S, varlist = PolynomialRing(R, var_names, ordering = ord)

      for iter = 1:100
         f = rand(S, 0:5, 0:100, 0:0, -100:100)
         g = rand(S, 0:5, 0:100, 0:0, -100:100)

         V1 = [rand(-10:10) for i in 1:num_vars]

         r1 = evaluate(f, V1)
         r2 = evaluate(g, V1)
         r3 = evaluate(f + g, V1)

         @test r3 == r1 + r2

         V2 = [BigInt(rand(-10:10)) for i in 1:num_vars]

         r1 = evaluate(f, V2)
         r2 = evaluate(g, V2)
         r3 = evaluate(f + g, V2)

         @test r3 == r1 + r2

         V3 = [R(rand(-10:10)) for i in 1:num_vars]

         r1 = evaluate(f, V3)
         r2 = evaluate(g, V3)
         r3 = evaluate(f + g, V3)

         @test r3 == r1 + r2
      end
   end

   println("PASS")
end

function test_gen_mpoly_valuation()
   print("Generic.MPoly.valuation...")

   R, x = JuliaZZ["y"]

   for num_vars = 1:10
      var_names = ["x$j" for j in 1:num_vars]
      ord = rand_ordering()

      S, varlist = PolynomialRing(R, var_names, ordering = ord)

      for iter = 1:100
         f = S()
         g = S()
         while f == 0 || g == 0
            f = rand(S, 0:5, 0:100, 0:0, -100:100)
            g = rand(S, 0:5, 0:100, 0:0, -100:100)
         end

         d1 = valuation(f, g)

         expn = rand(1:5)

         d2 = valuation(f*g^expn, g)

         @test d2 == d1 + expn

         d3, q3 = remove(f, g)

         @test d3 == d1
         @test f == q3*g^d3

         d4, q4 = remove(q3*g^expn, g)

         @test d4 == expn
         @test q4 == q3
      end
   end

   println("PASS")
end

function test_gen_mpoly()
   test_gen_mpoly_constructors()
   test_gen_mpoly_manipulation()
   test_gen_mpoly_unary_ops()
   test_gen_mpoly_binary_ops()
   test_gen_mpoly_adhoc_binary()
   test_gen_mpoly_adhoc_comparison()
   test_gen_mpoly_powering()
   test_gen_mpoly_divides()
   test_gen_mpoly_euclidean_division()
   test_gen_mpoly_ideal_reduction()
   test_gen_mpoly_gcd()
   test_gen_mpoly_evaluation()
   test_gen_mpoly_valuation()

   println("")
end
