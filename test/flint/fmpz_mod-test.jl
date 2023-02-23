@testset "ZZModRingElem.constructors" begin
   R = residue_ring(ZZ, ZZ(13))

   @test_throws DomainError residue_ring(ZZ, -ZZ(13))
   @test_throws DomainError residue_ring(ZZ, ZZ(0))

   @test elem_type(R) == Nemo.ZZModRingElem
   @test elem_type(Nemo.ZZModRing) == Nemo.ZZModRingElem
   @test parent_type(Nemo.ZZModRingElem) == Nemo.ZZModRing

   @test Nemo.promote_rule(elem_type(R), ZZRingElem) == elem_type(R)

   @test base_ring(R) == FlintZZ

   @test isa(R, Nemo.ZZModRing)

   @test isa(R(), Nemo.ZZModRingElem)

   @test isa(R(11), Nemo.ZZModRingElem)

   a = R(11)

   @test isa(R(a), Nemo.ZZModRingElem)

   for i = 1:1000
      R = residue_ring(ZZ, ZZ(rand(1:56987432569869432769438752)))

      a = R(rand(Int))
      d = a.data

      @test a.data < R.n
   end

   for i = 1:1000
      R = residue_ring(ZZ, ZZ(rand(1:24)))

      a = R(rand(Int))
      d = a.data

      @test a.data < R.n
   end
end

@testset "ZZModRingElem.rand" begin
   R = residue_ring(ZZ, ZZ(13))

   test_rand(R)
   test_rand(R, 1:9)
   test_rand(R, Int16(1):Int16(9))
   test_rand(R, big(1):big(9))
   test_rand(R, ZZRingElem(1):ZZRingElem(9))
   test_rand(R, [3,9,2])
   test_rand(R, Int16[3,9,2])
   test_rand(R, BigInt[3,9,2])
   test_rand(R, ZZRingElem[3,9,2])
end

@testset "ZZModRingElem.printing" begin
   R = residue_ring(ZZ, ZZ(13))

   @test string(R(3)) == "3"
   @test string(R()) == "0"
end

@testset "ZZModRingElem.manipulation" begin
   R = residue_ring(ZZ, ZZ(13))

   @test iszero(zero(R))

   @test modulus(R) == UInt(13)

   @test !is_unit(R())
   @test is_unit(R(3))

   @test deepcopy(R(3)) == R(3)

   R1 = residue_ring(ZZ, ZZ(13))

   @test R === R1

   S = residue_ring(ZZ, ZZ(1))

   @test iszero(zero(S))

   @test modulus(S) == UInt(1)

   @test is_unit(S())

   @test characteristic(R) == 13

   @test data(R(3)) == 3
   @test lift(R(3)) == 3
   @test isa(lift(R(3)), ZZRingElem)

   R2 = residue_ring(ZZ, ZZ(2))
   R22 = residue_ring(ZZ, 2)
   R3 = residue_ring(ZZ, ZZ(3))
   R6 = residue_ring(ZZ, ZZ(6))
   @test R2(R6(2)) == 2  && parent(R2(R6(2))) == R2
   @test R22(R6(2)) == 2 && parent(R22(R6(2))) == R22
   @test R3(R6(2)) == 2  && parent(R3(R6(2))) == R3
   @test_throws Exception R6(R3(1))
   @test_throws Exception R6(R2(1))
   @test_throws Exception R6(R22(1))
   @test_throws Exception R2(R3(1))
   @test_throws Exception R3(R2(1))
end

@testset "ZZModRingElem.unary_ops" begin
   for i = 1:100
      R = residue_ring(ZZ, ZZ(rand(1:56987432569869432769438752)))

      for iter = 1:100
         a = rand(R)

         @test a == -(-a)
      end
   end

   for i = 1:100
      R = residue_ring(ZZ, ZZ(rand(1:24)))

      for iter = 1:100
         a = rand(R)

         @test a == -(-a)
      end
   end
end

@testset "ZZModRingElem.binary_ops" begin
   for i = 1:100
      R = residue_ring(ZZ, ZZ(rand(1:24)))

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
      R = residue_ring(ZZ, ZZ(rand(1:56987432569869432769438752)))

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

@testset "ZZModRingElem.adhoc_binary" begin
   for i = 1:100
      R = residue_ring(ZZ, ZZ(rand(1:24)))

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
      R = residue_ring(ZZ, ZZ(rand(1:56987432569869432769438752)))

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

@testset "ZZModRingElem.powering" begin
   for i = 1:100
      R = residue_ring(ZZ, ZZ(rand(1:56987432569869432769438752)))

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
         while !is_unit(r)
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
      R = residue_ring(ZZ, ZZ(rand(1:56987432569869432769438752)))

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
         while !is_unit(r)
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

@testset "ZZModRingElem.comparison" begin
   for i = 1:100
      R = residue_ring(ZZ, ZZ(rand(1:24)))

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
      R = residue_ring(ZZ, ZZ(rand(1:56987432569869432769438752)))

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

@testset "ZZModRingElem.adhoc_comparison" begin
   for i = 1:100
      R = residue_ring(ZZ, ZZ(rand(1:24)))

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
      R = residue_ring(ZZ, ZZ(rand(1:56987432569869432769438752)))

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

@testset "ZZModRingElem.inversion" begin
   for i = 1:100
      R = residue_ring(ZZ, ZZ(rand(1:24)))

      for iter = 1:100
         a = rand(R)

         @test !is_unit(a) || inv(inv(a)) == a

         @test !is_unit(a) || a*inv(a) == one(R)
      end
   end

   for i = 1:100
      R = residue_ring(ZZ, ZZ(rand(1:56987432569869432769438752)))

      for iter = 1:100
         a = rand(R)

         @test !is_unit(a) || inv(inv(a)) == a

         @test !is_unit(a) || a*inv(a) == one(R)
      end
   end
end

@testset "ZZModRingElem.exact_division" begin
   for i = 1:100
      R = residue_ring(ZZ, ZZ(rand(1:24)))

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
      R = residue_ring(ZZ, ZZ(rand(1:56987432569869432769438752)))

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

@testset "ZZModRingElem.gcd" begin
   for i = 1:100
      R = residue_ring(ZZ, ZZ(rand(1:24)))

      for iter = 1:100
         a = rand(R)
         b = rand(R)
         c = rand(R)

         @test gcd(c*a, c*b) == R(gcd(c.data*gcd(a, b).data, R.n))
      end
   end
end

@testset "ZZModRingElem.gcdx" begin
   for i = 1:100
      R = residue_ring(ZZ, ZZ(rand(1:24)))

      for iter = 1:100
         a = rand(R)
         b = rand(R)

         g, s, t = gcdx(a, b)

         @test g == s*a + t*b
      end
   end
end
