@testset "fmpz_mod.constructors" begin
   R = ResidueRing(ZZ, ZZ(13))

   @test_throws DomainError ResidueRing(ZZ, -ZZ(13))
   @test_throws DomainError ResidueRing(ZZ, ZZ(0))

   @test elem_type(R) == Nemo.fmpz_mod
   @test elem_type(Nemo.FmpzModRing) == Nemo.fmpz_mod
   @test parent_type(Nemo.fmpz_mod) == Nemo.FmpzModRing

   @test base_ring(R) == FlintZZ

   @test isa(R, Nemo.FmpzModRing)

   @test isa(R(), Nemo.fmpz_mod)

   @test isa(R(11), Nemo.fmpz_mod)

   a = R(11)

   @test isa(R(a), Nemo.fmpz_mod)

   for i = 1:1000
      R = ResidueRing(ZZ, ZZ(rand(1:56987432569869432769438752)))

      a = R(rand(Int))
      d = a.data

      @test a.data < R.n
   end

   for i = 1:1000
      R = ResidueRing(ZZ, ZZ(rand(1:24)))

      a = R(rand(Int))
      d = a.data

      @test a.data < R.n
   end
end

@testset "fmpz_mod.rand" begin
   R = ResidueRing(ZZ, ZZ(13))

   test_rand(R)
   test_rand(R, 1:9)
   test_rand(R, Int16(1):Int16(9))
   test_rand(R, big(1):big(9))
   test_rand(R, fmpz(1):fmpz(9))
   test_rand(R, [3,9,2])
   test_rand(R, Int16[3,9,2])
   test_rand(R, BigInt[3,9,2])
   test_rand(R, fmpz[3,9,2])
end

@testset "fmpz_mod.printing" begin
   R = ResidueRing(ZZ, ZZ(13))

   @test string(R(3)) == "3"
   @test string(R()) == "0"
end

@testset "fmpz_mod.manipulation" begin
   R = ResidueRing(ZZ, ZZ(13))

   @test iszero(zero(R))

   @test modulus(R) == UInt(13)

   @test !isunit(R())
   @test isunit(R(3))

   @test deepcopy(R(3)) == R(3)

   R1 = ResidueRing(ZZ, ZZ(13))

   @test R === R1

   S = ResidueRing(ZZ, ZZ(1))

   @test iszero(zero(S))

   @test modulus(S) == UInt(1)

   @test isunit(S())

   @test characteristic(R) == 13

   @test data(R(3)) == 3
   @test lift(R(3)) == 3
   @test isa(lift(R(3)), fmpz)
end

@testset "fmpz_mod.unary_ops" begin
   for i = 1:100
      R = ResidueRing(ZZ, ZZ(rand(1:56987432569869432769438752)))

      for iter = 1:100
         a = rand(R)

         @test a == -(-a)
      end
   end

   for i = 1:100
      R = ResidueRing(ZZ, ZZ(rand(1:24)))

      for iter = 1:100
         a = rand(R)

         @test a == -(-a)
      end
   end
end

@testset "fmpz_mod.binary_ops" begin
   for i = 1:100
      R = ResidueRing(ZZ, ZZ(rand(1:24)))

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
      R = ResidueRing(ZZ, ZZ(rand(1:56987432569869432769438752)))

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
end

@testset "fmpz_mod.adhoc_binary" begin
   for i = 1:100
      R = ResidueRing(ZZ, ZZ(rand(1:24)))

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
      R = ResidueRing(ZZ, ZZ(rand(1:56987432569869432769438752)))

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
end

@testset "fmpz_mod.powering" begin
   for i = 1:100
      R = ResidueRing(ZZ, ZZ(rand(1:56987432569869432769438752)))

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
      R = ResidueRing(ZZ, ZZ(rand(1:56987432569869432769438752)))

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
end

@testset "fmpz_mod.comparison" begin
   for i = 1:100
      R = ResidueRing(ZZ, ZZ(rand(1:24)))

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
      R = ResidueRing(ZZ, ZZ(rand(1:56987432569869432769438752)))

      for iter = 1:100
         a = rand(R)

         @test (modulus(R) == 1 && a == a + 1) || a != a + 1

         c = rand(Int)
         d = rand(BigInt(0):BigInt(100))

         @test R(c) == R(c)
         @test R(d) == R(d)
      end
   end
end

@testset "fmpz_mod.adhoc_comparison" begin
   for i = 1:100
      R = ResidueRing(ZZ, ZZ(rand(1:24)))

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
      R = ResidueRing(ZZ, ZZ(rand(1:56987432569869432769438752)))

      for iter = 1:100
         c = rand(Int)
         d = rand(BigInt(0):BigInt(100))

         @test R(c) == c
         @test c == R(c)
         @test R(d) == d
         @test d == R(d)
      end
   end
end

@testset "fmpz_mod.inversion" begin
   for i = 1:100
      R = ResidueRing(ZZ, ZZ(rand(1:24)))

      for iter = 1:100
         a = rand(R)

         @test !isunit(a) || inv(inv(a)) == a

         @test !isunit(a) || a*inv(a) == one(R)
      end
   end

   for i = 1:100
      R = ResidueRing(ZZ, ZZ(rand(1:56987432569869432769438752)))

      for iter = 1:100
         a = rand(R)

         @test !isunit(a) || inv(inv(a)) == a

         @test !isunit(a) || a*inv(a) == one(R)
      end
   end
end

@testset "fmpz_mod.exact_division" begin
   for i = 1:100
      R = ResidueRing(ZZ, ZZ(rand(1:24)))

      for iter = 1:100
         a1 = rand(R)
         a2 = rand(R)
         a2 += Int(a2 == 0) # still works mod 1
         p = a1*a2

         q = divexact(p, a2)

         @test q*a2 == p
      end
   end

   for i = 1:100
      R = ResidueRing(ZZ, ZZ(rand(1:56987432569869432769438752)))

      for iter = 1:100
         a1 = rand(R)
         a2 = rand(R)
         a2 += Int(a2 == 0) # still works mod 1
         p = a1*a2

         q = divexact(p, a2)

         @test q*a2 == p
      end
   end
end

@testset "fmpz_mod.gcd" begin
   for i = 1:100
      R = ResidueRing(ZZ, ZZ(rand(1:24)))

      for iter = 1:100
         a = rand(R)
         b = rand(R)
         c = rand(R)

         @test gcd(c*a, c*b) == R(gcd(c.data*gcd(a, b).data, R.n))
      end
   end
end

@testset "fmpz_mod.gcdx" begin
   for i = 1:100
      R = ResidueRing(ZZ, ZZ(rand(1:24)))

      for iter = 1:100
         a = rand(R)
         b = rand(R)

         g, s, t = gcdx(a, b)

         @test g == s*a + t*b
      end
   end
end
