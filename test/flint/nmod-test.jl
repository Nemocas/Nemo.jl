function test_elem(R::Nemo.zzModRing)
  return R(rand(Int))
end

@testset "zzModRingElem.conformance_tests" begin
  # TODO: using test_Ring_interface_recursive below fails because zzModPolyRingElem does
  # not support initialization from arbitrary Integer subtypes such as BigInt
  for i in [1, 6, 13, 2^8, 2^16, 2^32, next_prime(2^8), next_prime(2^16), next_prime(2^32)]
    test_Ring_interface(residue_ring(ZZ, i)[1])
  end
end

@testset "zzModRingElem.constructors" begin
  R, f = residue_ring(ZZ, 13)
  @test domain(f) === ZZ
  @test codomain(f) === R
  @test_throws ErrorException f(QQ(1//2))
  @test_throws ErrorException preimage(f, QQ(1//2))

  @test_throws DomainError residue_ring(ZZ, -13)
  @test_throws DomainError residue_ring(ZZ, 0)

  @test elem_type(R) == Nemo.zzModRingElem
  @test elem_type(Nemo.zzModRing) == Nemo.zzModRingElem
  @test parent_type(Nemo.zzModRingElem) == Nemo.zzModRing

  @test Nemo.promote_rule(elem_type(R), ZZRingElem) == elem_type(R)

  @test base_ring(R) == ZZ

  @test isa(R, Nemo.zzModRing)

  @test isa(R(), Nemo.zzModRingElem)

  @test isa(R(11), Nemo.zzModRingElem)

  a = R(11)

  @test isa(R(a), Nemo.zzModRingElem)

  for i = 1:1000
    R, = residue_ring(ZZ, rand(UInt(1):typemax(UInt)))

    a = R(rand(Int))
    d = a.data

    @test a.data < R.n
  end

  for i = 1:1000
    R, = residue_ring(ZZ, rand(1:24))

    a = R(rand(Int))
    d = a.data

    @test a.data < R.n
  end
end

@testset "zzModRingElem.rand" begin
  R, = residue_ring(ZZ, 13)

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

@testset "zzModRingElem.printing" begin
  R, = residue_ring(ZZ, 13)

  @test string(R(3)) == "3"
  @test string(R()) == "0"
end

@testset "zzModRingElem.manipulation" begin
  R, = residue_ring(ZZ, 13)

  @test iszero(zero(R))

  @test modulus(R) == UInt(13)

  @test !is_unit(R())
  @test is_unit(R(3))

  @test deepcopy(R(3)) == R(3)

  R1, = residue_ring(ZZ, 13)

  @test R === R1

  S, = residue_ring(ZZ, 1)

  @test iszero(zero(S))

  @test modulus(S) == UInt(1)

  @test is_unit(S())

  @test characteristic(S) == 1

  @test data(R(3)) == 3
  @test lift(R(3)) == 3
  @test isa(lift(R(3)), ZZRingElem)
  @test lift(ZZ, R(3)) == 3
  @test isa(lift(ZZ, R(3)), ZZRingElem)
  @test ZZ(R(3)) == 3
  @test isa(ZZ(R(3)), ZZRingElem)
  @test ZZRingElem(R(3)) == 3
  @test isa(ZZRingElem(R(3)), ZZRingElem)

  R2,  = residue_ring(ZZ, 2)
  R3,  = residue_ring(ZZ, 3)
  R6,  = residue_ring(ZZ, 6)
  R66, = residue_ring(ZZ, ZZ(6))
  @test R2(R6(2)) == 2  && parent(R2(R6(2))) == R2
  @test R3(R6(2)) == 2  && parent(R3(R6(2))) == R3
  @test R2(R66(2)) == 2 && parent(R2(R66(2))) == R2
  @test R3(R66(2)) == 2 && parent(R3(R66(2))) == R3
  @test_throws Exception R66(R3(1))
  @test_throws Exception R6(R3(1))
  @test_throws Exception R6(R2(1))
  @test_throws Exception R2(R3(1))
  @test_throws Exception R3(R2(1))
end

@testset "zzModRingElem.unary_ops" begin
  for i = 1:100
    R, = residue_ring(ZZ, rand(UInt(1):typemax(UInt)))

    for iter = 1:100
      a = rand(R)

      @test a == -(-a)
    end
  end

  for i = 1:100
    R, = residue_ring(ZZ, rand(1:24))

    for iter = 1:100
      a = rand(R)

      @test a == -(-a)
    end
  end
end

@testset "zzModRingElem.binary_ops" begin
  for i = 1:100
    R, = residue_ring(ZZ, rand(1:24))

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
    R, = residue_ring(ZZ, rand(UInt(1):typemax(UInt)))

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

@testset "zzModRingElem.adhoc_binary" begin
  for i = 1:100
    R, = residue_ring(ZZ, rand(1:24))

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
    R, = residue_ring(ZZ, rand(UInt(1):typemax(UInt)))

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

@testset "zzModRingElem.powering" begin
  for i = 1:100
    R, = residue_ring(ZZ, rand(1:24))

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
    R, = residue_ring(ZZ, rand(UInt(1):typemax(UInt)))

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

  R, _ = residue_ring(ZZ, 10)
  a = R(3)
  @test a^ZZ(2) == R(9)
  @test a^ZZ(-2) == R(9)
  @test a^(ZZ(2)^64) == R(1)
  @test a^(-ZZ(2)^64) == R(1)
end

@testset "zzModRingElem.comparison" begin
  for i = 1:100
    R, = residue_ring(ZZ, rand(1:24))

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
    R, = residue_ring(ZZ, rand(UInt(1):typemax(UInt)))

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

@testset "zzModRingElem.adhoc_comparison" begin
  for i = 1:100
    R, = residue_ring(ZZ, rand(1:24))

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
    R, = residue_ring(ZZ, rand(UInt(1):typemax(UInt)))

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

@testset "zzModRingElem.inversion" begin
  for i = 1:100
    R, = residue_ring(ZZ, rand(1:24))

    for iter = 1:100
      a = rand(R)

      @test !is_unit(a) || inv(inv(a)) == a

      @test !is_unit(a) || a*inv(a) == one(R)
    end
  end

  for i = 1:100
    R, = residue_ring(ZZ, rand(UInt(1):typemax(UInt)))

    for iter = 1:100
      a = rand(R)

      @test !is_unit(a) || inv(inv(a)) == a

      @test !is_unit(a) || a*inv(a) == one(R)
    end
  end
end

@testset "zzModRingElem.exact_division" begin
  for i = 1:100
    R, = residue_ring(ZZ, rand(1:24))

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
    R, = residue_ring(ZZ, rand(UInt(1):typemax(UInt)))

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

@testset "zzModRingElem.gcd" begin
  for i = 1:100
    R, = residue_ring(ZZ, rand(1:24))

    for iter = 1:100
      a = rand(R)
      b = rand(R)
      c = rand(R)

      @test gcd(c*a, c*b) == R(gcd(c.data*gcd(a, b).data, R.n))
    end
  end
end

@testset "zzModRingElem.gcdx" begin
  for i = 1:100
    R, = residue_ring(ZZ, rand(1:24))

    for iter = 1:100
      a = rand(R)
      b = rand(R)

      g, s, t = gcdx(a, b)

      @test g == s*a + t*b
    end
  end
end
