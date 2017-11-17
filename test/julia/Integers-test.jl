function test_Integers_constructors()
   print("Julia.Integers.constructors...")

   R = zz
   S = JuliaZZ

   @test elem_type(R) == Int
   @test elem_type(S) == BigInt
   @test elem_type(Nemo.Integers{Int}) == Int
   @test elem_type(Nemo.Integers{BigInt}) == BigInt
   @test parent_type(Int) == Nemo.Integers{Int}
   @test parent_type(BigInt) == Nemo.Integers{BigInt}
   
   @test isa(R, Nemo.Integers)
   @test isa(S, Nemo.Integers)

   @test isa(R(), Int)
   @test isa(S(), BigInt)

   @test isa(R(11), Int)
   @test isa(S(BigInt(11)), BigInt)
   @test isa(S(11), BigInt)

   a = R(11)
   b = S(11)

   @test isa(R(a), Int)
   @test isa(S(b), BigInt)

   println("PASS")
end

function test_Integers_manipulation()
   print("Julia.Integers.manipulation...")

   R = zz
   S = JuliaZZ

   @test iszero(zero(R))
   @test iszero(zero(S))

   @test isone(one(R))
   @test isone(one(S))

   @test !isunit(R())
   @test !isunit(S())
   @test !isunit(R(3))
   @test !isunit(S(3))
   @test isunit(R(1))
   @test isunit(S(1))
   @test isunit(R(-1))
   @test isunit(S(-1))

   println("PASS")
end

function test_Integers_modular_arithmetic()
   print("Julia.Integers.modular_arithmetic...")

   R = zz
   S = JuliaZZ

   for iter = 1:100
      a = R(1)
      b = S(1)
      modR = 23
      modS = BigInt(23)

      r = rand(R, 0:22)
      s = rand(S, 0:22)

      for n = 0:20
         @test r == 0 || a == powmod(r, n, modR)
         @test s == 0 || b == powmod(s, n, modS)

         a = mod(a*r, modR)
         b = mod(b*s, modS)
      end   
   end

   println("PASS")
end

function test_Integers_exact_division()
   print("Julia.Integers.exact_division...")
  
   R = zz
   S = JuliaZZ

   for iter = 1:1000
      a1 = rand(R, -100:100)
      a2 = rand(R, -100:100)
      b1 = rand(S, -100:100)
      b2 = rand(S, -100:100)

      @test a2 == 0 || divexact(a1*a2, a2) == a1
      @test b2 == 0 || divexact(b1*b2, b2) == b1

      if a1 != 0
         flagR, qR = divides(a1*a2, a1)
      
         @test flagR
         @test qR == a2
      end

      if b1 != 0
         flagS, qS = divides(b1*b2, b1)

         @test flagS
         @test qS == b2
      end   
   end

   println("PASS")
end

function test_Integers_gcd()
   print("Julia.Integers.gcd...")
  
   R = zz
   S = JuliaZZ

   n = 23*31

   modR = n
   modS = BigInt(n)

   for iter = 1:1000
      r = rand(R, 0:n - 1)
      s = rand(S, 0:n - 1)

      gR, sR = gcdinv(r, modR)
      gS, sS = gcdinv(s, modS)

      @test gR == gcd(r, modR)
      @test gR != 1 || mod(sR*r, modR) == 1
      @test gS == gcd(s, modS)
      @test gS != 1 || mod(sS*s, modS) == 1
   end

   println("PASS")
end


function test_Integers()
   test_Integers_constructors()
   test_Integers_manipulation()
   test_Integers_modular_arithmetic()
   test_Integers_exact_division()
   test_Integers_gcd()
   
   println("")
end
