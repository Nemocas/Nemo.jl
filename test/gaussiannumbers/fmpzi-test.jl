@testset "fmpzi.abstract_types" begin
   @test fmpzi <: RingElem
   @test FlintZZiRing <: Nemo.Ring
   @test elem_type(ZZi) == fmpzi
   @test parent_type(fmpzi) == FlintZZiRing
   @test base_ring(ZZi) == ZZ
   @test base_ring(ZZi()) == ZZ
end

@testset "fmpzi.hash" begin
   @test hash(ZZi(2, 3)) == hash(ZZi(2 + 3*im))
   @test hash(ZZi) == hash(deepcopy(ZZi))
   @test ZZi === Base.deepcopy_internal(ZZi, IdDict())
end

@testset "fmpzi.printing" begin
   @test string(zero(ZZi)) == "0"
   @test string(one(ZZi)) == "1"
   @test string(ZZi(2,-3)) == "2 - 3*im"
   @test string(ZZi) == "ZZ[im]"
end

@testset "fmpzi.constructors" begin
   for a in Any[true, false, 1, big(1), fmpz(1)]
      @test ZZi(a) == a
      @test ZZ(a) + im == ZZi(a, 1)
      @test ZZ(a) - im == ZZi(a, -1)
      @test im + ZZ(a) == ZZi(a, 1)
      @test im - ZZ(a) == ZZi(-a, 1)
      @test ZZ(a)*im == ZZi(0, a)
      @test im*ZZ(a) == ZZi(0, a)
   end
end

@testset "fmpzi.conversions" begin
   @test ZZ(ZZi(9)) == 9
   @test_throws Exception ZZ(ZZi(0,9))
   @test convert(Complex{BigInt}, ZZi(8,9)) == 8 + 9*im
   @test 8 + 9*im == convert(fmpzi, 8 + 9*im)
   @test 8 == convert(fmpzi, 8)
   @test convert(fmpzi, fmpz(8)) == 8
end

@testset "fmpzi.pow" begin
   @test_throws Exception ZZi(1,1)^-1
   @test ZZi(0,1)^-1 == -im
   @test ZZi(0,1)^2 == -1
end

@testset "fmpzi.canonical_mod" begin
   function test_nudivrem(a, b)
      @test Nemo.nudivrem(fmpz(a), fmpz(b)) == divrem(a, b, RoundNearestTiesUp)
   end
   for i in -7:7, j in 1:5
      test_nudivrem(i, -j)
      test_nudivrem(i, +j)
   end
   for i in 1:30
      m = rand_bits(ZZi, rand(2:6))
      if iszero(m)
         m = ZZi(1, 1)
      end
      n = abs2(m)
      s = Set{fmpzi}([zero(ZZi)])
      for j in 1:10*Int(n)
         a = rand_bits(ZZi, rand(1:15))
         push!(s, mod(a, m))
         @test length(s) <= n
      end
   end
end

@testset "fmpzi.Euclidean" begin
   @test_throws Exception invmod(ZZi(1,1), ZZi(2))
   m = ZZi(3)
   @test isdivisible_by(invmod(ZZi(1,1), m) - powermod(ZZi(1,1), -1, m), m)
   @test_throws Exception remove(ZZi(1), ZZi(0))
   @test remove(ZZi(0), ZZi(2)) == (0, ZZi(0))
   @test remove(ZZi(-10,-2), ZZi(1,1)) == (3, ZZi(2,3))
end

@testset "fmpzi.gcd" begin
   for a in (ZZi(0,0), ZZi(1,0), ZZi(2,1), ZZi(1,1), ZZi(1,2),
                       ZZi(0,1), ZZi(-1,2), ZZi(-1,1), ZZi(-2,1),
                       ZZi(1,-0), ZZi(2,-1), ZZi(1,-1), ZZi(1,-2),
                       ZZi(0,-1), ZZi(-1,-2), ZZi(-1,-1), ZZi(-2,-1))
      b = divexact(a, canonical_unit(a))
      @assert abs2(b) == abs2(a)
      @assert real(b) >= 0
      @assert abs(real(b)) > abs(imag(b)) || real(b) == imag(b)
   end

   @test gcd(2, ZZi(0,3)) == 1
   @test gcd(2, ZZi(0,4)) == 2
   @test gcd(ZZi(3,0), 2) == 1
   @test gcd(ZZi(4,0), 2) == 2
   @test gcd(ZZ(2), ZZi(1,1)) == ZZi(1,1)
   @test gcd(ZZi(1,1), ZZ(2)) == ZZi(1,1)
   @test gcdx(ZZi(0), ZZi(0)) == (0, 0, 0)
   @test gcdx(ZZi(1), ZZi(0)) == (1, 1, 0)
   @test gcdx(ZZi(0), ZZi(1)) == (1, 0, 1)

   let l = 1000
      for k in 1:200
         a = one(ZZi)
         b = zero(ZZi)
         for i in 1:rand(0:200)
            q = rand_bits(ZZi, rand(0:l))
            (a, b) = (a*q + b, a)
            nbits(a) < 10000 || break
         end
         g = rand_bits(ZZi, rand(0:l))
         (A, B) = (g*a, g*b)

         G = gcd(A, B)
         if iszero(G)
            @test iszero(A)
            @test iszero(B)
         else
            Abar = divexact(A, G)
            Bbar = divexact(B, G)
            @test Abar*G == A
            @test Bbar*G == B
            @test isone(gcd(Abar, Bbar))
            @test isone(canonical_unit(G))
         end

         (G1, U, V) = gcdx(A, B)
         @test G1 == G
         @test G1 == U*A + V*B
      end
   end
end

@testset "fmpzi.factor" begin
   let l = 26
      for k in 1:100
         a = one(ZZi)
         for i in 1:rand(0:20)
            a *= rand_bits(ZZi, rand(0:l))^rand(1:16)
            nbits(a) < 250 || break
         end
         if iszero(a)
            @test_throws Exception factor(a)
         else
            f = factor(a)
            b = unit(f)
            for (p, e) in f
               b *= p^e
            end
            @test a == b
         end
      end
   end
end

@testset "fmpzi.adhoc" begin
   @test ZZ(5) + im == ZZi(5, 1)
   @test im + ZZ(5) == ZZi(5, 1)
   @test ZZ(5) - im == ZZi(5, -1)
   @test im - ZZ(5) == ZZi(-5, 1)
   @test ZZ(5) * im == ZZi(0, 5)
   @test im * ZZ(5) == ZZi(0, 5)

   for (a, bs) in [[ZZi(1,1), [2, ZZ(2), 2*im, ZZi(2)]],
                   [ZZ(2),    [2*im]]]
      for b in bs
         @test Nemo.AbstractAlgebra.promote_rule(typeof(a), typeof(b)) == fmpzi
         @test Nemo.AbstractAlgebra.promote_rule(typeof(b), typeof(a)) == fmpzi
         @test ZZi == parent(a*b)
         @test ZZi == parent(b*a)
         @test ZZi == parent(a + b)
         @test ZZi == parent(b + a)
         @test ZZi == parent(a - b)
         @test ZZi == parent(b - a)
         @test ZZi(a)*ZZi(b) == a*b
         @test ZZi(b)*ZZi(a) == b*a
         @test ZZi(a) + ZZi(b) == a + b
         @test ZZi(b) + ZZi(a) == b + a
         @test ZZi(a) - ZZi(b) == a - b
         @test ZZi(b) - ZZi(a) == b - a
      end
   end
end

@testset "fmpzi.unsafe" begin
   a = rand_bits(ZZi, 600); A = deepcopy(a)
   b = rand_bits(ZZi, 600); B = deepcopy(b)
   t = rand_bits(ZZi, 600)
   @test isone(one!(t))
   @test iszero(zero!(t))
   @test mul!(t, a, b) == a*b
   @test mul!(t, t, b) == a*b^2
   @test mul!(t, a, t) == a^2*b^2
   @test mul!(t, t, t) == a^4*b^4
   @test 1 + 0*im == one!(t)
   @test addmul!(t, a, b) == 1 + a*b
   @test addmul!(t, a, b, fmpzi()) == 1 + 2*a*b
   @test Nemo.submul!(t, a, b) == 1 + a*b
   @test Nemo.submul!(t, a, b, fmpzi()) == 1
   @test addmul!(t, t, b) == 1 + b
   @test Nemo.submul!(t, t, a) == (1 + b)*(1 - a)
   @test Nemo.set!(t, a) == a
   @test Nemo.add!(t, t, UInt(1)) == 1 + a
   @test Nemo.sub!(t, t, UInt(1)) == a
   @test Nemo.add!(t, t, BigInt(1)) == a + 1
   @test Nemo.sub!(t, t, BigInt(1)) == a
   @test Nemo.mul!(t, t, BigInt(2)) == a*2
   @test Nemo.mul!(t, t, UInt(3)) == a*6
   Nemo.swap!(a, b)
   @test b == A && a == B
end

function test_elem(R::FlintZZiRing)
   return rand_bits(R, rand(0:200))
end

@testset "fmpzi.conformance_tests" begin
   test_Ring_interface(ZZi)
   test_EuclideanRing_interface(ZZi)
end

