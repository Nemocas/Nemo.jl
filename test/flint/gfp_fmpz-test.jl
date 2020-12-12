@testset "gfp_fmpz.constructors..." begin
   R = GF(ZZ(13))

   @test_throws DomainError GF(-ZZ(13))

   @test elem_type(R) == Nemo.gfp_fmpz_elem
   @test elem_type(Nemo.GaloisFmpzField) == Nemo.gfp_fmpz_elem
   @test parent_type(Nemo.gfp_fmpz_elem) == Nemo.GaloisFmpzField

   @test isa(R, Nemo.GaloisFmpzField)

   @test isa(R(), Nemo.gfp_fmpz_elem)

   @test isa(R(11), Nemo.gfp_fmpz_elem)

   a = R(11)

   @test isa(R(a), Nemo.gfp_fmpz_elem)

   for i = 1:1000
      p = rand(BigInt(1):BigInt(4273673264873254848326487))*6 + 1
      if Nemo.isprobable_prime(ZZ(p))
         R = GF(ZZ(p))

         a = R(rand(Int))
         d = a.data

         @test a.data < R.n
      end
   end

   for i = 1:1000
      p = rand(1:24)
      if Nemo.isprime(ZZ(p))
         R = GF(ZZ(p))

         a = R(rand(Int))
         d = a.data

         @test a.data < R.n
      end
   end

   S = GF(ZZ(17))
   T = GF(ZZ(17))
   @test T === S

   S = GF(ZZ(19), cached = false)
   T = GF(ZZ(19), cached = false)
   @test !(S === T)

   S = GF(fmpz(17))
   T = GF(fmpz(17))
   @test T === S

   S = GF(fmpz(19), cached = false)
   T = GF(fmpz(19), cached = false)
   @test !(S === T)
end

@testset "gfp_fmpz.rand..." begin
   R = GF(ZZ(13))

   test_rand(R)
   test_rand(R, 1:9)
end

@testset "gfp_fmpz.printing..." begin
   R = GF(ZZ(13))

   @test string(R(3)) == "3"
   @test string(R()) == "0"
end

@testset "gfp_fmpz.manipulation..." begin
   R = GF(ZZ(13))

   @test iszero(zero(R))

   @test modulus(R) == UInt(13)

   @test !isunit(R())
   @test isunit(R(3))

   @test deepcopy(R(3)) == R(3)

   R1 = GF(ZZ(13))

   @test R === R1

   @test characteristic(R) == 13
end

@testset "gfp_fmpz.unary_ops..." begin
   for i = 1:1000
      p = rand(BigInt(1):BigInt(4273673264873254848326487))*6 + 1
      if Nemo.isprobable_prime(ZZ(p))
         R = GF(ZZ(p))

         for iter = 1:1000
            a = rand(R)

            @test a == -(-a)
         end
      end
   end

   for i = 1:100
      p = rand(1:24)
      if Nemo.isprime(ZZ(p))
         R = GF(ZZ(p))

         for iter = 1:100
            a = rand(R)

            @test a == -(-a)
         end
      end
   end
end

@testset "gfp_fmpz.binary_ops..." begin
   for i = 1:100
      p = rand(1:24)
      if Nemo.isprime(ZZ(p))
         R = GF(ZZ(p))

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

   for i = 1:1000
      p = rand(BigInt(1):BigInt(4273673264873254848326487))*6 + 1
      if Nemo.isprobable_prime(ZZ(p))
         R = GF(ZZ(p))

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
end

@testset "gfp_fmpz.adhoc_binary..." begin
   for i = 1:100
      p = rand(1:24)
      if Nemo.isprime(ZZ(p))
         R = GF(ZZ(p))

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
   end

   for i = 1:1000
      p = rand(BigInt(1):BigInt(4273673264873254848326487))*6 + 1
      if Nemo.isprobable_prime(ZZ(p))
         R = GF(ZZ(p))

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
end

@testset "gfp_fmpz.powering..." begin
  for i = 1:100
      p = rand(1:24)
      if Nemo.isprime(ZZ(p))
         R = GF(ZZ(p))

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

            rinv = inv(r)

            for n = 0:20
               @test r == 0 || a == r^(-n)

               a *= rinv
            end
         end
      end
   end

   for i = 1:1000
      p = rand(BigInt(1):BigInt(4273673264873254848326487))*6 + 1
      if Nemo.isprobable_prime(ZZ(p))
         R = GF(ZZ(p))

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

            rinv = inv(r)

            for n = 0:20
               @test r == 0 || a == r^(-n)

               a *= rinv
            end
         end
      end
   end
end

@testset "gfp_fmpz.comparison..." begin
  for i = 1:100
      p = rand(1:24)
      if Nemo.isprime(ZZ(p))
         R = GF(ZZ(p))

         for iter = 1:100
            a = rand(R)

            @test (modulus(R) == 1 && a == a + 1) || a != a + 1

            c = rand(0:100)
            d = rand(BigInt(0):BigInt(100))

            @test R(c) == R(c)
            @test R(d) == R(d)
         end
      end
   end

   for i = 1:1000
      p = rand(BigInt(1):BigInt(4273673264873254848326487))*6 + 1
      if Nemo.isprobable_prime(ZZ(p))
         R = GF(ZZ(p))

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
end

@testset "gfp_fmpz.adhoc_comparison..." begin
  for i = 1:100
      p = rand(1:24)
      if Nemo.isprime(ZZ(p))
         R = GF(ZZ(p))

         for iter = 1:100
            c = rand(0:100)
            d = rand(BigInt(0):BigInt(100))

            @test R(c) == c
            @test c == R(c)
            @test R(d) == d
            @test d == R(d)
         end
      end
   end

   for i = 1:1000
      p = rand(BigInt(1):BigInt(4273673264873254848326487))*6 + 1
      if Nemo.isprobable_prime(ZZ(p))
         R = GF(ZZ(p))

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
end

@testset "gfp_fmpz.inversion..." begin
  for i = 1:100
      p = rand(1:24)
      if Nemo.isprime(ZZ(p))
         R = GF(ZZ(p))

         for iter = 1:100
            a = rand(R)

            @test !isunit(a) || inv(inv(a)) == a

            @test !isunit(a) || a*inv(a) == one(R)
         end
      end
   end

   for i = 1:1000
      p = rand(BigInt(1):BigInt(4273673264873254848326487))*6 + 1
      if Nemo.isprobable_prime(ZZ(p))
         R = GF(ZZ(p))

         for iter = 1:100
            a = rand(R)

            @test !isunit(a) || inv(inv(a)) == a

            @test !isunit(a) || a*inv(a) == one(R)
         end
      end
   end
end

@testset "gfp_fmpz.exact_division..." begin
  for i = 1:100
      p = rand(1:24)
      if Nemo.isprime(ZZ(p))
         R = GF(ZZ(p))

         for iter = 1:100
            a1 = rand(R)
            a2 = rand(R)
            a2 += Int(a2 == 0) # still works mod 1
            p = a1*a2

            q = divexact(p, a2)

            fl, y = divides(p, a2)
            @test fl

            @test q*a2 == p
         end
      end
   end


   for i = 1:1000
      p = rand(BigInt(1):BigInt(4273673264873254848326487))*6 + 1
      if Nemo.isprobable_prime(ZZ(p))
         R = GF(ZZ(p))

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
end
