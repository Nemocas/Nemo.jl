function test_nmod_constructors()
   print("nmod.constructors...")

   R = ResidueRing(ZZ, 13)

   @test elem_type(R) == Nemo.nmod
   @test elem_type(Nemo.NmodRing) == Nemo.nmod
   @test parent_type(Nemo.nmod) == Nemo.NmodRing
   
   @test isa(R, Nemo.NmodRing)

   @test isa(R(), Nemo.nmod)

   @test isa(R(11), Nemo.nmod)

   a = R(11)

   @test isa(R(a), Nemo.nmod)

   for i = 1:1000
      R = ResidueRing(ZZ, rand(UInt(1):typemax(UInt)))

      a = R(rand(Int))
      d = a.data

      @test a.data < R.n
   end 

   for i = 1:1000
      R = ResidueRing(ZZ, rand(1:24))

      a = R(rand(Int))
      d = a.data

      @test a.data < R.n
   end 

   println("PASS")
end

function test_nmod_printing()
   print("nmod.printing...")

   R = ResidueRing(ZZ, 13)

   @test string(R(3)) == "3"
   @test string(R()) == "0"

   println("PASS")
end

function test_nmod_manipulation()
   print("nmod.manipulation...")

   R = ResidueRing(ZZ, 13)

   @test iszero(zero(R))

   @test modulus(R) == UInt(13)

   @test !isunit(R())
   @test isunit(R(3))

   @test deepcopy(R(3)) == R(3)

   R1 = ResidueRing(ZZ, 13)

   @test R === R1

   S = ResidueRing(ZZ, 1)

   @test iszero(zero(S))

   @test modulus(S) == UInt(1)

   @test isunit(S())

   println("PASS")
end

function test_nmod_unary_ops()
   print("nmod.unary_ops...")

   for i = 1:100
      R = ResidueRing(ZZ, rand(UInt(1):typemax(UInt)))

      for iter = 1:100
         a = rand(R)

         @test a == -(-a)
      end
   end

   for i = 1:100
      R = ResidueRing(ZZ, rand(1:24))

      for iter = 1:100
         a = rand(R)

         @test a == -(-a)
      end
   end

   println("PASS")
end

function test_nmod_binary_ops()
   print("nmod.binary_ops...")

   for i = 1:100
      R = ResidueRing(ZZ, rand(1:24))

      for iter = 1:100
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
      end
   end

   for i = 1:100
      R = ResidueRing(ZZ, rand(UInt(1):typemax(UInt)))

      for iter = 1:100
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
      end
   end

   println("PASS")
end

function test_nmod_adhoc_binary()
   print("nmod.adhoc_binary...")

   for i = 1:100
      R = ResidueRing(ZZ, rand(1:24))

      for iter = 1:100
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
      end
   end

   for i = 1:100
      R = ResidueRing(ZZ, rand(UInt(1):typemax(UInt)))

      for iter = 1:100
         a = rand(R)

         c1 = rand(Int)
         c2 = rand(Int)
         d1 = rand(BigInt(0):BigInt(100))
         d2 = rand(BigInt(0):BigInt(100))

         @test a + c1 == c1 + a
         @test a + d1 == d1 + a
         @test a - c1 == -(c1 - a)
         @test a - d1 == -(d1 - a)
         @test a*c1 == c1*a
         @test a*d1 == d1*a
         @test a*c1 + a*c2 == a*(widen(c1) + widen(c2))
         @test a*d1 + a*d2 == a*(d1 + d2)
      end
   end

   println("PASS")
end

function test_nmod_powering()
   print("nmod.powering...")

   for i = 1:100
      R = ResidueRing(ZZ, rand(1:24))

      for iter = 1:100
         a = R(1)

         r = rand(R)

         for n = 0:20
            @test r == 0 || a == r^n

            a *= r
         end   
      end

      for iter = 1:100
         a = R(1)

         r = rand(R)
         while !isunit(r)
            r = rand(R)
         end

         rinv = r == 0 ? R(0) : inv(r)

         for n = 0:20
            @test r == 0 || a == r^(-n)

            a *= rinv
         end   
      end
   end

   for i = 1:100
      R = ResidueRing(ZZ, rand(UInt(1):typemax(UInt)))

      for iter = 1:100
         a = R(1)

         r = rand(R)

         for n = 0:20
            @test r == 0 || a == r^n

            a *= r
         end   
      end

      for iter = 1:100
         a = R(1)

         r = rand(R)
         while !isunit(r)
            r = rand(R)
         end

         rinv = r == 0 ? R(0) : inv(r)

         for n = 0:20
            @test r == 0 || a == r^(-n)

            a *= rinv
         end   
      end
   end

   println("PASS")
end

function test_nmod_comparison()
   print("nmod.comparison...")
  
   for i = 1:100
      R = ResidueRing(ZZ, rand(1:24))

      for iter = 1:100
         a = rand(R)

         @test (modulus(R) == 1 && a == a + 1) || a != a + 1

         c = rand(0:100)
         d = rand(BigInt(0):BigInt(100))

         @test R(c) == R(c)
         @test R(d) == R(d)
      end
   end

   for i = 1:100
      R = ResidueRing(ZZ, rand(UInt(1):typemax(UInt)))

      for iter = 1:100
         a = rand(R)

         @test (modulus(R) == 1 && a == a + 1) || a != a + 1

         c = rand(Int)
         d = rand(BigInt(0):BigInt(100))

         @test R(c) == R(c)
         @test R(d) == R(d)
      end
   end

   println("PASS")
end

function test_nmod_adhoc_comparison()
   print("nmod.adhoc_comparison...")
  
   for i = 1:100
      R = ResidueRing(ZZ, rand(1:24))

      for iter = 1:100
         c = rand(0:100)
         d = rand(BigInt(0):BigInt(100))

         @test R(c) == c
         @test c == R(c)
         @test R(d) == d
         @test d == R(d)
      end
   end

   for i = 1:100
      R = ResidueRing(ZZ, rand(UInt(1):typemax(UInt)))

      for iter = 1:100
         c = rand(Int)
         d = rand(BigInt(0):BigInt(100))

         @test R(c) == c
         @test c == R(c)
         @test R(d) == d
         @test d == R(d)
      end
   end

   println("PASS")
end

function test_nmod_inversion()
   print("nmod.inversion...")
  
   for i = 1:100
      R = ResidueRing(ZZ, rand(1:24))

      for iter = 1:100
         a = rand(R)
         
         @test !isunit(a) || inv(inv(a)) == a

         @test !isunit(a) || a*inv(a) == one(R)
      end
   end

   for i = 1:100
      R = ResidueRing(ZZ, rand(UInt(1):typemax(UInt)))

      for iter = 1:100
         a = rand(R)
         
         @test !isunit(a) || inv(inv(a)) == a

         @test !isunit(a) || a*inv(a) == one(R)
      end
   end

   println("PASS")
end

function test_nmod_exact_division()
   print("nmod.exact_division...")
  
   for i = 1:100
      R = ResidueRing(ZZ, rand(1:24))

      for iter = 1:100
         a1 = rand(R)
         a2 = rand(R)

         @test !isunit(a2) || divexact(a1, a2)*a2 == a1
      end
   end

   for i = 1:100
      R = ResidueRing(ZZ, rand(UInt(1):typemax(UInt)))

      for iter = 1:100
         a1 = rand(R)
         a2 = rand(R)

         @test !isunit(a2) || divexact(a1, a2)*a2 == a1
      end
   end

   println("PASS")
end

function test_nmod_gcd()
   print("nmod.gcd...")
  
   for i = 1:100
      R = ResidueRing(ZZ, rand(1:24))

      for iter = 1:100
         a = rand(R)
         b = rand(R)
         c = rand(R)

         @test gcd(c*a, c*b) == R(gcd(c.data*gcd(a, b).data, R.n))
      end
   end

   println("PASS")
end

function test_nmod()
   test_nmod_constructors()
   test_nmod_printing()
   test_nmod_manipulation()
   test_nmod_unary_ops()
   test_nmod_binary_ops()
   test_nmod_adhoc_binary()
   test_nmod_powering()
   test_nmod_comparison()
   test_nmod_adhoc_comparison()
   test_nmod_inversion()
   test_nmod_exact_division()
   test_nmod_gcd()
   
   println("")
end
