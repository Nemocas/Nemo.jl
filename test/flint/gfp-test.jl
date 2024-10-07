@testset "gfp.conformance_tests" begin
  # TODO: make this work with test_Field_interface_recursive
  for p in [13, next_prime(2^8), next_prime(2^16), next_prime(2^32)]
    test_Field_interface(Native.GF(p))
    #test_Field_interface_recursive(Native.GF(p))
  end
end

@testset "gfp.constructors" begin
  R = Native.GF(13)

  @test_throws DomainError Native.GF(-13)

  @test elem_type(R) == Nemo.fpFieldElem
  @test elem_type(Nemo.fpField) == Nemo.fpFieldElem
  @test parent_type(Nemo.fpFieldElem) == Nemo.fpField

  @test Nemo.promote_rule(elem_type(R), ZZRingElem) == elem_type(R)

  @test isa(R, Nemo.fpField)

  @test isa(R(), Nemo.fpFieldElem)

  @test isa(R(11), Nemo.fpFieldElem)

  a = R(11)

  @test isa(R(a), Nemo.fpFieldElem)

  @test isa(R(QQFieldElem(7, 11)), Nemo.fpFieldElem)
  @test R(QQFieldElem(7, 11))*11 == 7
  @test_throws ErrorException R(QQFieldElem(7, 13))

  for i = 1:1000
    p = rand(UInt(1):typemax(UInt))

    if Nemo.is_prime(ZZ(p))
      R = Native.GF(p)

      a = R(rand(Int))
      d = a.data

      @test a.data < R.n
    end
  end

  for i = 1:1000
    p = rand(1:24)
    if Nemo.is_prime(ZZ(p))
      R = Native.GF(p)

      a = R(rand(Int))
      d = a.data

      @test a.data < R.n
    end
  end

  S = Native.GF(17)
  T = Native.GF(17)
  @test T === S

  S = Native.GF(19, cached = false)
  T = Native.GF(19, cached = false)
  @test !(S === T)

  S = Native.GF(ZZRingElem(17))
  T = Native.GF(ZZRingElem(17))
  @test T === S

  S = Native.GF(ZZRingElem(19), cached = false)
  T = Native.GF(ZZRingElem(19), cached = false)
  @test !(S === T)

  @test_throws MethodError Native.GF(big(3))
  @test_throws MethodError Native.GF(0x3)
end

@testset "gfp.rand" begin
  R = Native.GF(13)

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

@testset "gfp.printing" begin
  R = Native.GF(13)

  @test string(R(3)) == "3"
  @test string(R()) == "0"
end

@testset "gfp.manipulation" begin
  R = Native.GF(13)

  @test iszero(zero(R))

  @test modulus(R) == UInt(13)

  @test !is_unit(R())
  @test is_unit(R(3))

  @test deepcopy(R(3)) == R(3)

  @test coeff(R(2), 0) == 2
  @test coeff(R(2), 1) == 0

  R1 = Native.GF(13)

  @test R === R1

  @test characteristic(R) == 13
  @test degree(R) == 1

  @test data(R(3)) == 3
  @test lift(R(3)) == 3
  @test isa(lift(R(3)), ZZRingElem)
  @test lift(ZZ, R(3)) == 3
  @test isa(lift(ZZ, R(3)), ZZRingElem)

  R2 = Native.GF(2)
  R3 = Native.GF(3)
  R6, = residue_ring(ZZ, 6)
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

@testset "gfp.unary_ops" begin
  for i = 1:100
    p = rand(UInt(1):typemax(UInt))
    if Nemo.is_prime(ZZ(p))
      R = Native.GF(p)

      for iter = 1:100
        a = rand(R)

        @test a == -(-a)
      end
    end
  end

  for i = 1:100
    p = rand(1:24)
    if Nemo.is_prime(ZZ(p))
      R = Native.GF(p)

      for iter = 1:100
        a = rand(R)

        @test a == -(-a)
      end
    end
  end
end

@testset "gfp.binary_ops" begin
  for i = 1:100
    p = rand(1:24)
    if Nemo.is_prime(ZZ(p))
      R = Native.GF(p)

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

  for i = 1:100
    p = rand(UInt(1):typemax(UInt))
    if Nemo.is_prime(ZZ(p))
      R = Native.GF(p)

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

@testset "gfp.adhoc_binary" begin
  for i = 1:100
    p = rand(1:24)
    if Nemo.is_prime(ZZ(p))
      R = Native.GF(p)

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

  for i = 1:100
    p = rand(UInt(1):typemax(UInt))
    if Nemo.is_prime(ZZ(p))
      R = Native.GF(p)

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

@testset "gfp.powering" begin
  for i = 1:100
    p = rand(1:24)
    if Nemo.is_prime(ZZ(p))
      R = Native.GF(p)

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

        rinv = inv(r)

        for n = 0:20
          @test r == 0 || a == r^(-n)

          a *= rinv
        end
      end
    end
  end

  for i = 1:100
    p = rand(UInt(1):typemax(UInt))
    if Nemo.is_prime(ZZ(p))
      R = Native.GF(p)

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

        rinv = inv(r)

        for n = 0:20
          @test r == 0 || a == r^(-n)

          a *= rinv
        end
      end
    end
  end
end

@testset "gfp.comparison" begin
  for i = 1:100
    p = rand(1:24)
    if Nemo.is_prime(ZZ(p))
      R = Native.GF(p)

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

  for i = 1:100
    p = rand(UInt(1):typemax(UInt))
    if Nemo.is_prime(ZZ(p))
      R = Native.GF(p)

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

@testset "gfp.adhoc_comparison" begin
  for i = 1:100
    p = rand(1:24)
    if Nemo.is_prime(ZZ(p))
      R = Native.GF(p)

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

  for i = 1:100
    p = rand(UInt(1):typemax(UInt))
    if Nemo.is_prime(ZZ(p))
      R = Native.GF(p)

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

@testset "gfp.inversion" begin
  for i = 1:100
    p = rand(1:24)
    if Nemo.is_prime(ZZ(p))
      R = Native.GF(p)

      for iter = 1:100
        a = rand(R)

        @test !is_unit(a) || inv(inv(a)) == a

        @test !is_unit(a) || a*inv(a) == one(R)
      end
    end
  end

  for i = 1:100
    p = rand(UInt(1):typemax(UInt))
    if Nemo.is_prime(ZZ(p))
      R = Native.GF(p)

      for iter = 1:100
        a = rand(R)

        @test !is_unit(a) || inv(inv(a)) == a

        @test !is_unit(a) || a*inv(a) == one(R)
      end
    end
  end
end

@testset "gfp.exact_division" begin
  for i = 1:100
    p = rand(1:24)
    if Nemo.is_prime(ZZ(p))
      R = Native.GF(p)

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


  for i = 1:100
    p = rand(UInt(1):typemax(UInt))
    if Nemo.is_prime(ZZ(p))
      R = Native.GF(p)

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

@testset "gfp.square_root" begin
  for i = 1:100
    p = rand(1:65537)
    if Nemo.is_prime(ZZ(p))
      R = Native.GF(p)

      z = rand(R)
      if p != 2
        while is_square(z)
          z = rand(R)
        end
      end

      for iter = 1:100
        a = rand(R)

        @test is_square(a^2)

        s = sqrt(a^2)

        @test s^2 == a^2

        f1, s1 = is_square_with_sqrt(a^2)

        @test f1 && s1^2 == a^2

        if p != 2 && !iszero(a)
          @test !is_square(z*a^2)

          f2, s2 = is_square_with_sqrt(z*a^2)

          @test !f2
        end
      end
    end
  end
end

@testset "gfp.overload" begin
  R = Native.GF(19)
  @test R([5]) == R(5)
end

@testset "gfp.representation_matrix" begin
  F = Native.GF(19)
  @test is_one(representation_matrix(one(F)))
end
