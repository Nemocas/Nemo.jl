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
