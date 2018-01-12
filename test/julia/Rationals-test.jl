function test_Rationals_constructors()
   print("Julia.Rationals.constructors...")

   R = qq
   S = JuliaQQ

   @test R == FractionField(zz)
   @test S == FractionField(JuliaZZ)

   @test elem_type(R) == Rational{Int}
   @test elem_type(S) == Rational{BigInt}
   @test elem_type(Nemo.Rationals{Int}) == Rational{Int}
   @test elem_type(Nemo.Rationals{BigInt}) == Rational{BigInt}
   @test parent_type(Rational{Int}) == Nemo.Rationals{Int}
   @test parent_type(Rational{BigInt}) == Nemo.Rationals{BigInt}
   
   @test isa(R, Nemo.Rationals)
   @test isa(S, Nemo.Rationals)

   @test isa(R(), Rational{Int})
   @test isa(S(), Rational{BigInt})

   @test isa(R(11), Rational{Int})
   @test isa(R(11//3), Rational{Int})
   @test isa(R(11, 3), Rational{Int})
   @test isa(S(BigInt(11)), Rational{BigInt})
   @test isa(S(BigInt(11)//BigInt(3)), Rational{BigInt})
   @test isa(S(Rational{BigInt}(11)), Rational{BigInt})
   @test isa(S(11), Rational{BigInt})
   @test isa(S(11, 3), Rational{BigInt})
   @test isa(S(BigInt(11), BigInt(3)), Rational{BigInt})

   a = R(11)
   b = S(11)

   @test isa(R(a), Rational{Int})
   @test isa(S(b), Rational{BigInt})

   println("PASS")
end

function test_Rationals_manipulation()
   print("Julia.Rationals.manipulation...")

   R = qq
   S = JuliaQQ

   @test iszero(zero(R))
   @test iszero(zero(S))

   @test isone(one(R))
   @test isone(one(S))

   @test !isunit(R())
   @test !isunit(S())
   @test isunit(R(3))
   @test isunit(S(3))

   println("PASS")
end

function test_Rationals_exact_division()
   print("Julia.Rationals.exact_division...")
  
   R = qq
   S = JuliaQQ

   for iter = 1:1000
      a1 = rand(R, -100:100)
      a2 = rand(R, -100:100)
      b1 = rand(S, -100:100)
      b2 = rand(S, -100:100)
      c1 = rand(zz, -100:100)
      c2 = rand(JuliaZZ, -100:100)

      @test a2 == 0 || divexact(a1, a2)*a2 == a1
      @test b2 == 0 || divexact(b1, b2)*b2 == b1

      if a1 != 0
         flagR, qR = divides(a2, a1)
      
         @test flagR
         @test qR*a1 == a2
      end

      if b1 != 0
         flagS, qS = divides(b2, b1)

         @test flagS
         @test qS*b1 == b2
      end   

      @test c1 == 0 || divexact(a1, c1)*c1 == a1
      @test c2 == 0 || divexact(b1, c2)*c2 == b1

      @test a1 == 0 || divexact(c1, a1)*a1 == c1
      @test b1 == 0 || divexact(c2, b1)*b1 == c2
   end

   println("PASS")
end

function test_Rationals_gcd()
   print("Julia.Rationals.gcd...")
  
   R = qq
   S = JuliaQQ

   for iter = 1:1000
      r1 = rand(R, -100:100)
      r2 = rand(R, -100:100)
      r3 = rand(R, -100:100)
      s1 = rand(S, -100:100)
      s2 = rand(S, -100:100)
      s3 = rand(S, -100:100)

      @test gcd(r1, gcd(r2, r3)) == gcd(gcd(r1, r2), r3)
      @test gcd(s1, gcd(s2, s3)) == gcd(gcd(s1, s2), s3)
   end

   println("PASS")
end

function test_Rationals_square_root()
   print("Julia.Rationals.square_root...")
  
   R = qq
   S = JuliaQQ

   for iter = 1:1000
      r = rand(R, -100:100)
      s = rand(R, -100:100)
      
      f = r^2
      g = s^2

      @test Nemo.sqrt(f)^2 == f
      @test Nemo.sqrt(g)^2 == g
   end

   println("PASS")
end


function test_Rationals()
   test_Rationals_constructors()
   test_Rationals_manipulation()
   test_Rationals_exact_division()
   test_Rationals_gcd()
   test_Rationals_square_root()
   
   println("")
end
