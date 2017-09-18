function test_gfelem_constructors()
   print("Julia.gfelem.constructors...")

   R = GF(13)
   S = GF(BigInt(13))

   @test elem_type(R) == Nemo.gfelem{Int}
   @test elem_type(S) == Nemo.gfelem{BigInt}
   @test elem_type(Nemo.GFField{Int}) == Nemo.gfelem{Int}
   @test elem_type(Nemo.GFField{BigInt}) == Nemo.gfelem{BigInt}
   @test parent_type(Nemo.gfelem{Int}) == Nemo.GFField{Int}
   @test parent_type(Nemo.gfelem{BigInt}) == Nemo.GFField{BigInt}
   
   @test isa(R, Nemo.GFField)
   @test isa(S, Nemo.GFField)

   @test isa(R(), Nemo.gfelem)
   @test isa(S(), Nemo.gfelem)

   @test isa(R(11), Nemo.gfelem)
   @test isa(S(BigInt(11)), Nemo.gfelem)
   @test isa(S(11), Nemo.gfelem)
   @test isa(S(BigInt(11)), Nemo.gfelem)

   a = R(11)
   b = S(11)

   @test isa(R(a), Nemo.gfelem)
   @test isa(S(b), Nemo.gfelem)

   println("PASS")
end

function test_gfelem_printing()
   print("Julia.gfelem.printing...")

   R = GF(13)
   S = GF(BigInt(13))

   @test string(R(3)) == "3"
   @test string(R()) == "0"
   @test string(S(3)) == "3"
   @test string(S()) == "0"

   println("PASS")
end

function test_gfelem_manipulation()
   print("Julia.gfelem.manipulation...")

   R = GF(13)
   S = GF(BigInt(13))

   @test iszero(zero(R))
   @test iszero(zero(S))

   @test isone(one(R))
   @test isone(one(S))

   @test characteristic(R) == 13
   @test characteristic(S) == 13

   @test order(R) == 13
   @test order(S) == 13

   @test degree(R) == 1
   @test degree(S) == 1

   @test !isunit(R())
   @test !isunit(S())
   @test isunit(R(3))
   @test isunit(S(3))

   @test deepcopy(R(3)) == R(3)
   @test deepcopy(S(3)) == S(3)

   R1 = GF(13)
   S1 = GF(BigInt(13))

   @test R === R1
   @test S === S1

   println("PASS")
end

function test_gfelem_unary_ops()
   print("Julia.gfelem.unary_ops...")

   R = GF(13)
   S = GF(BigInt(13))

   for iter = 1:1000
      a = rand(R)
      b = rand(S)

      @test a == -(-a)
      @test b == -(-b)
   end

   println("PASS")
end

function test_gfelem_binary_ops()
   print("Julia.gfelem.binary_ops...")

   R = GF(13)
   S = GF(BigInt(13))

   for iter = 1:1000
      a1 = rand(R)
      a2 = rand(R)
      a3 = rand(R)

      @test a1 + a2 == a2 + a1
      @test a1 - a2 == -(a2 - a1)
      @test a1 + R() == a1
      @test a1 + (a2 + a3) == (a1 + a2) + a3
      @test a1*(a2 + a3) == a1*a2 + a1*a3
      @test a1*a2 == a2*a1
      @test a1*R(1) == a1
      @test R(1)*a1 == a1

      b1 = rand(S)
      b2 = rand(S)
      b3 = rand(S)

      @test b1 + b2 == b2 + b1
      @test b1 - b2 == -(b2 - b1)
      @test b1 + S() == b1
      @test b1 + (b2 + b3) == (b1 + b2) + b3
      @test b1*(b2 + b3) == b1*b2 + b1*b3
      @test b1*b2 == b2*b1
      @test b1*S(1) == b1
      @test S(1)*b1 == b1
   end

   println("PASS")
end

function test_gfelem_adhoc_binary()
   print("Julia.gfelem.adhoc_binary...")

   R = GF(13)
   S = GF(BigInt(13))

   for iter = 1:1000
      a = rand(R)

      c1 = rand(0:100)
      c2 = rand(0:100)
      d1 = rand(BigInt(0):BigInt(100))
      d2 = rand(BigInt(0):BigInt(100))

      @test a + c1 == c1 + a
      @test a + d1 == d1 + a
      @test a - c1 == -(c1 - a)
      @test a - d1 == -(d1 - a)
      @test a*c1 == c1*a
      @test a*d1 == d1*a
      @test a*c1 + a*c2 == a*(c1 + c2)
      @test a*d1 + a*d2 == a*(d1 + d2)

      b = rand(S)

      @test b + c1 == c1 + b
      @test b + d1 == d1 + b
      @test b - c1 == -(c1 - b)
      @test b - d1 == -(d1 - b)
      @test b*c1 == c1*b
      @test b*d1 == d1*b
      @test b*c1 + b*c2 == b*(c1 + c2)
      @test b*d1 + b*d2 == b*(d1 + d2)
   end

   println("PASS")
end

function test_gfelem_powering()
   print("Julia.gfelem.powering...")

   R = GF(13)
   S = GF(BigInt(13))

   for iter = 1:100
      a = R(1)
      b = S(1)

      r = rand(R)
      s = rand(S)

      for n = 0:20
         @test r == 0 || a == r^n
         @test s == 0 || b == s^n

         a *= r
         b *= s
      end   
   end

   for iter = 1:100
      a = R(1)
      b = S(1)

      r = rand(R)
      s = rand(S)

      rinv = r == 0 ? R(0) : inv(r)
      sinv = s == 0 ? S(0) : inv(s)

      for n = 0:20
         @test r == 0 || a == r^(-n)
         @test s == 0 || b == s^(-n)

         a *= rinv
         b *= sinv
      end   
   end

   println("PASS")
end

function test_gfelem_comparison()
   print("Julia.gfelem.comparison...")
  
   R = GF(13)
   S = GF(BigInt(13))

   for iter = 1:1000
      a = rand(R)
      b = rand(S)

      @test a != a + 1
      @test b != b + 1

      c = rand(0:100)
      d = rand(BigInt(0):BigInt(100))

      @test R(c) == R(c)
      @test S(c) == S(c)
      @test R(d) == R(d)
      @test S(d) == S(d)
   end

   println("PASS")
end

function test_gfelem_adhoc_comparison()
   print("Julia.gfelem.adhoc_comparison...")
  
   R = GF(13)
   S = GF(BigInt(13))

   for iter = 1:1000
      c = rand(0:100)
      d = rand(BigInt(0):BigInt(100))

      @test R(c) == c
      @test c == R(c)
      @test R(d) == d
      @test d == R(d)

      @test S(c) == c
      @test c == S(c)
      @test S(d) == d
      @test d == S(d)
   end

   println("PASS")
end

function test_gfelem_inversion()
   print("Julia.gfelem.inversion...")
  
   R = GF(13)
   S = GF(BigInt(13))

   for iter = 1:1000
      a = rand(R)
      b = rand(S)

      @test a == 0 || inv(inv(a)) == a
      @test b == 0 || inv(inv(b)) == b

      @test a == 0 || a*inv(a) == one(R)
      @test b == 0 || b*inv(b) == one(S)
   end

   println("PASS")
end

function test_gfelem_exact_division()
   print("Julia.gfelem.exact_division...")
  
   R = GF(13)
   S = GF(BigInt(13))

   for iter = 1:1000
      a1 = rand(R)
      a2 = rand(R)
      b1 = rand(S)
      b2 = rand(S)

      @test a2 == 0 || divexact(a1, a2)*a2 == a1
      @test b2 == 0 || divexact(b1, b2)*b2 == b1
   end

   println("PASS")
end

function test_gfelem()
   test_gfelem_constructors()
   test_gfelem_printing()
   test_gfelem_manipulation()
   test_gfelem_unary_ops()
   test_gfelem_binary_ops()
   test_gfelem_adhoc_binary()
   test_gfelem_powering()
   test_gfelem_comparison()
   test_gfelem_adhoc_comparison()
   test_gfelem_inversion()
   test_gfelem_exact_division()
   
   println("")
end
