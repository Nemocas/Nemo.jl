@testset "fmpz.abstract_types" begin
   @test fmpzi <: RingElem
   @test FlintZZiRing <: Nemo.Ring
   @test elem_type(ZZi) == fmpzi
   @test parent_type(fmpzi) == FlintZZiRing
end

@testset "ZZi.printing" begin
  @test string(zero(ZZi)) == "0"
  @test string(one(ZZi)) == "1"
  @test string(ZZi(2,-3)) == "2 - 3*im"
  @test string(ZZi) == "ZZ[im]"
end

@testset "ZZi.constructors" begin
  for a in Any[true, false, 1, big(1), fmpz(1)]
    @test ZZi(a) == a
    @test fmpz(a) + im == fmpzi(a, 1)
    @test fmpz(a) - im == fmpzi(a, -1)
    @test im + fmpz(a) == fmpzi(a, 1)
    @test im - fmpz(a) == fmpzi(-a, 1)
    @test fmpz(a)*im == fmpzi(0, a)
    @test im*fmpz(a) == fmpzi(0, a)
  end
end

@testset "ZZi.conversions" begin
  @test ZZ(ZZi(9)) == 9
  @test_throws Exception ZZ(ZZi(0,9))
  @test convert(Complex{BigInt}, ZZi(8,9)) == 8 + 9*im
  @test convert(fmpzi, 8 + 9*im) == 8 + 9*im
  @test convert(fmpzi, 8) == 8
end

@testset "ZZi.gcd" begin
  for a in (ZZi(0,0), ZZi(1,0), ZZi(2,1), ZZi(1,1), ZZi(1,2),
                      ZZi(0,1), ZZi(-1,2), ZZi(-1,1), ZZi(-2,1),
                      ZZi(1,-0), ZZi(2,-1), ZZi(1,-1), ZZi(1,-2),
                      ZZi(0,-1), ZZi(-1,-2), ZZi(-1,-1), ZZi(-2,-1))
    b = divexact(a, canonical_unit(a))
    @assert abs2(b) == abs2(a)
    @assert real(b) >= 0
    @assert abs(real(b)) > abs(imag(b)) || real(b) == imag(b)
  end

  let l = 1000
    for k in 1:200
      a = one(ZZi)
      b = zero(ZZi)
      for i in 1:rand(0:100)
        q = rand_bits(ZZ, rand(0:l)) + rand_bits(ZZ, rand(0:l))*im
        (a, b) = (a*q + b, a)
        nbits(a) < 10000 || break
      end
      g = rand_bits(ZZ, rand(0:l)) + rand_bits(ZZ, rand(0:l))*im
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

@testset "ZZi.factor" begin
  let l = 13
    for k in 1:500
      a = one(ZZi)
      for i in 1:rand(0:20)
        a *= (rand_bits(ZZ, rand(0:l)) + rand_bits(ZZ, rand(0:l))*im)^rand(1:16)
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

@testset "ZZi.unsafe" begin
  a = rand_bits(ZZi, 600); A = deepcopy(a)
  b = rand_bits(ZZi, 600); B = deepcopy(b)
  t = rand_bits(ZZi, 600)
  @test isone(one!(t))
  @test iszero(zero!(t))
  @test mul!(t, a, b) == a*b
  @test mul!(t, t, b) == a*b^2
  @test mul!(t, a, t) == a^2*b^2
  @test mul!(t, t, t) == a^4*b^4
  one!(t)
  @test addmul!(t, a, b) == 1 + a*b
  @test addmul!(t, a, b, fmpzi()) == 1 + 2*a*b
  @test Nemo.submul!(t, a, b) == 1 + a*b
  @test Nemo.submul!(t, a, b, fmpzi()) == 1
  @test addmul!(t, t, b) == 1 + b
  @test Nemo.submul!(t, t, a) == (1 + b)*(1 - a)
  @test set!(t, a) == a
  swap!(a, b)
  @test b == A && a == B
end

