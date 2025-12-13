import Nemo: AbstractAlgebra.PrettyPrinting

@testset "ZZRingElem.conformance_tests" begin
  ConformanceTests.test_Ring_interface_recursive(ZZ)
end

@testset "ZZRingElem.issingletontype" begin
  @test Base.issingletontype(ZZRing)
end

@testset "ZZRingElem.abstract_types" begin
  @test ZZRingElem <: RingElem

  @test ZZRing <: Nemo.Ring

  @test elem_type(ZZRing()) == ZZRingElem
  @test elem_type(ZZRing) == ZZRingElem
  @test parent_type(ZZRingElem) == ZZRing
end

@testset "ZZRingElem.constructors" begin
  a = ZZRingElem(-123)
  @test isa(a, RingElem)

  b = ZZRingElem(12.0)
  @test isa(b, RingElem)

  c = ZZRingElem("-1234567876545678376545678900000000000000000000000000")
  @test isa(c, RingElem)

  @test a == ZZRingElem("-123")
  @test b == ZZRingElem("12")

  @test ZZRingElem(234) == ZZRingElem(SubString("12345",2,4))

  d = ZZRingElem(c)
  @test isa(d, RingElem)
  @test c == d

  e = deepcopy(c)
  @test isa(e, RingElem)
  @test c == e

  f = ZZRingElem(BigFloat(10)^100)
  @test isa(f, RingElem)
  @test f == ZZRingElem(10)^100

  g = ZZRingElem()
  @test isa(f, RingElem)
  @test g == 0
end

@testset "ZZRingElem.rand" begin
  test_rand(ZZ, 1:9)
  test_rand(ZZ, Int16(1):Int16(9))
  test_rand(ZZ, big(1):big(9))
  test_rand(ZZ, ZZRingElem(1):ZZRingElem(9))
  test_rand(ZZ, [3,9,2])
  test_rand(ZZ, Int16[3,9,2])
  test_rand(ZZ, BigInt[3,9,2])
  test_rand(ZZ, ZZRingElem[3,9,2])

  for bits in 0:100
    t = rand_bits(ZZ, bits)
    @test abs(t) < ZZRingElem(2)^bits
    @test bits < 1 || abs(t) >= ZZRingElem(2)^(bits - 1)
  end

  for i = 1:100
    nbits = rand(2:100)
    n = rand_bits_prime(ZZ, nbits)
    @test ndigits(n, 2) == nbits
    @test is_prime(n)
  end
  @test_throws DomainError rand_bits_prime(ZZ, -1)
  @test_throws DomainError rand_bits_prime(ZZ, 0)
  @test_throws DomainError rand_bits_prime(ZZ, 1)

  # in a range
  for e in [0, 1, 2, 3, 32, 64, 65, 100, 129, 500]
    for b in [ZZRingElem(2) .^ e ; ZZRingElem(2) .^ e .+ e;]
      for r in [ZZRingElem(1):ZZRingElem(1):b, ZZRingElem(3):ZZRingElem(1):b, ZZRingElem(1):ZZRingElem(3):b]
        if isempty(r)
          @test_throws ArgumentError rand(r)
        else
          rb = map(BigInt, r) # in(::ZZRingElem, StepRange{ZZRingElem}) no working
          test_rand(r) do x
            @test BigInt(x) in rb
          end
        end
      end
    end
  end

  @testset "Nemo seeding" begin
    for seed in (rand(UInt128), rand(Int8(0):typemax(Int8)))
      Nemo.randseed!(seed)
      a = [rand_bits(ZZ, i) for i = 1:99] # must test for i > 64, to exercise
      # both FLINT's RNGs
      Nemo.randseed!(seed)
      @test a == [rand_bits(ZZ, i) for i = 1:99]
    end
    @test_throws DomainError Nemo.randseed!(-rand(1:1234))
  end

  @testset "ZZRingElemUnitRange.rand" begin
    r = ZZ(-4):ZZ(2)^64
    for _ in 1:10
      s = @inferred rand(r)
      @test s >= -4 && s <= ZZ(2)^64
      @test s isa ZZRingElem
    end

    r = ZZ(-4):ZZ(2):ZZ(10)
    rg = Random.RangeGenerator(r)
    for _ in 1:10
      s = @inferred rand(rg)
      @test s >= -4 && s <= 10
      @test s isa ZZRingElem
      @test is_even(s)
    end

    a = [one(ZZ) for _ in 1:10]
    @inferred rand!(a, r)
    for s in a
      @test s >= -4 && s <= 10
      @test s isa ZZRingElem
      @test is_even(s)
    end
  end
end

@testset "ZZRingElem.printing" begin
  a = ZZRingElem(-123)

  @test string(a) == "-123"
end

@testset "ZZRingElem.convert" begin
  a = ZZRingElem(-123)
  b = ZZRingElem(12)

  @testset "ZZRingElem.convert for $T" for T in [Int8, Int16, Int32, Int64, Int128, BigInt, Float16, Float32, Float64, BigFloat]
    x = @inferred T(a)
    @test x isa T
    @test x == -123
  end

  x = @inferred Integer(a)
  @test x isa BigInt
  @test x == -123

  @testset "ZZRingElem.convert for $T and $f+2^$e" for
      T in (Int8, Int16, Int32, Int64, Int128, UInt8, UInt16, UInt32, UInt64, UInt128),
      e in 1:130, f in -1:1
    c = ZZ(2)^e + f
    if fits(T, c)
      x = @inferred T(c)
      @test x isa T
      @test x == T(big(2)^e + f)
    else
      @test_throws InexactError T(c)
    end
  end

  @test_throws InexactError Int(ZZRingElem(1234484735687346876324432764872))
  @test_throws InexactError UInt(ZZRingElem(typemin(Int)))
end

@testset "ZZRingElem.vector_arithmetics" begin
  @test ZZRingElem[1, 2, 3] // ZZRingElem(2) == QQFieldElem[1//2, 1, 3//2]
  @test ZZRingElem(2) * ZZRingElem[1, 2, 3] == ZZRingElem[2, 4, 6]
  @test ZZRingElem[1, 2, 3] * ZZRingElem(2) == ZZRingElem[2, 4, 6]
end

@testset "ZZRingElem.manipulation" begin
  a = one(ZZRing())
  b = zero(ZZRing())
  c = zero(ZZRingElem)

  @test isa(a, ZZRingElem)
  @test isa(b, ZZRingElem)
  @test isa(c, ZZRingElem)

  @test sign(a) == 1
  @test sign(a) isa ZZRingElem
  @test !signbit(a)

  @test sign(-a) == -1
  @test sign(-a) isa ZZRingElem
  @test signbit(-a)

  @test sign(b) == 0
  @test sign(b) isa ZZRingElem
  @test !signbit(b)

  @test fits(Int, a)

  @test fits(UInt, a)

  @test size(a) == 1

  @test canonical_unit(ZZRingElem(-12)) == -1

  @test is_unit(ZZRingElem(-1))

  @test !iszero(a)
  @test iszero(b)
  @test zero(ZZRing()) == zero(ZZRingElem)

  @test isone(a)
  @test !isone(b)
  @test one(ZZRing()) == one(ZZRingElem)

  @test numerator(ZZRingElem(12)) == ZZRingElem(12)

  @test denominator(ZZRingElem(12)) == ZZRingElem(1)

  @test iseven(ZZRingElem(12))
  @test isodd(ZZRingElem(13))
  b = big(2)
  x = rand(-b^rand(1:1000):b^rand(1:1000))
  y = ZZRingElem(x)
  @test iseven(x) == iseven(y)
  @test isodd(x) == isodd(y)

  @test isinteger(a)
  @test isinteger(b)
  @test isinteger(x)
  @test isinteger(y)

  @test isfinite(a)
  @test isfinite(b)
  @test isfinite(x)
  @test isfinite(y)

  @test !isinf(a)
  @test !isinf(b)
  @test !isinf(x)
  @test !isinf(y)

  @test characteristic(ZZ) == 0
end

@testset "ZZRingElem.rounding" begin
  @test floor(ZZRingElem(12)) == ZZRingElem(12)
  @test ceil(ZZRingElem(12)) == ZZRingElem(12)
  @test trunc(ZZRingElem(12)) == ZZRingElem(12)

  @test floor(ZZRingElem, ZZRingElem(12)) == ZZRingElem(12)
  @test ceil(ZZRingElem, ZZRingElem(12)) == ZZRingElem(12)
  @test trunc(ZZRingElem, ZZRingElem(12)) == ZZRingElem(12)

  @testset "$func" for func in (trunc, round, ceil, floor)
    for val in -5:5
      valZ = ZZRingElem(val)
      @test func(valZ) isa ZZRingElem
      @test func(valZ) == func(val)
      @test func(ZZRingElem, valZ) isa ZZRingElem
      @test func(ZZRingElem, valZ) == func(ZZRingElem, val)
    end

    for val in [3.4, 2//3, big(2)//3, 2, big(3), false//true]
      @test func(ZZRingElem, val) isa ZZRingElem
      @test func(ZZRingElem, val) == func(Int, val)
    end

    for T in [Int, BigInt]
      @test (@inferred func(T, QQ(2//3))) == func(T, 2//3)
    end
  end

  a = [1.2 3.4; -1.2 -3.4]
  @test trunc(ZZMatrix, a) == ZZ[1 3; -1 -3]
  @test round(ZZMatrix, a) == ZZ[1 3; -1 -3]
  @test ceil(ZZMatrix, a) == ZZ[2 4; -1 -3]
  @test floor(ZZMatrix, a) == ZZ[1 3; -2 -4]
end

@testset "ZZRingElem.binary_ops" begin
  a = ZZRingElem(12)
  b = ZZRingElem(26)

  @test a + b == 38

  @test a - b == -14

  @test a*b == 312

  @test b%a == 2

  @test b&a == 8

  @test b|a == 30

  @test xor(b, a) == 22
end

@testset "ZZRingElem.division" begin
  a = ZZRingElem(12)
  b = ZZRingElem(26)

  @test fdiv(b, a) == 2

  @test cdiv(b, a) == 3

  @test tdiv(b, a) == 2

  @test div(b, a) == 2

  @test div(-ZZRingElem(2), ZZRingElem(3)) == 0
  @test Nemo.div(-ZZRingElem(2), ZZRingElem(3)) == -1

  @test div(-2, ZZRingElem(3)) == 0
  @test Nemo.div(-2, ZZRingElem(3)) == -1

  @test div(-ZZRingElem(2), 3) == 0
  @test Nemo.div(-ZZRingElem(2), 3) == -1
end

@testset "ZZRingElem.remainder" begin
  a = ZZRingElem(12)
  b = ZZRingElem(26)

  @test mod(b, a) == 2

  @test mod(ZZRingElem(3), ZZRingElem(-2)) == ZZRingElem(-1)

  @test rem(b, a) == 2

  @test mod(b, 12) == 2

  @test rem(b, 12) == 2
end

@testset "ZZRingElem.exact_division" begin
  @test divexact(ZZRingElem(24), ZZRingElem(12)) == 2
  @test divexact(ZZRingElem(24), ZZRingElem(12); check=false) == 2
  @test_throws ArgumentError divexact(ZZRingElem(24), ZZRingElem(11))
end

@testset "ZZRingElem.inverse" begin
  @test inv(ZZRingElem(1)) == 1
  @test inv(-ZZRingElem(1)) == -1
  @test_throws DivideError inv(ZZRingElem(0))
  @test_throws ArgumentError inv(ZZRingElem(2))
end

@testset "ZZRingElem.divides" begin
  flag, q = divides(ZZRingElem(12), ZZRingElem(0))
  @test flag == false
  @test divides(ZZRingElem(12), ZZRingElem(6)) == (true, ZZRingElem(2))
  @test divides(ZZRingElem(0), ZZRingElem(0)) == (true, ZZRingElem(0))

  for iters = 1:1000
    a = rand(ZZ, -1000:1000)
    b = rand(ZZ, -1000:1000)

    flag, q = divides(a*b, b)

    @test flag && b == 0 || q == a
    @test is_divisible_by(a*b, b)
  end

  for iters = 1:1000
    b = rand(ZZ, -1000:1000)
    if b == 0 || b == 1
      b = ZZ(2)
    elseif b == -1
      b = -ZZ(2)
    end
    a = rand(-1000:1000)
    r = rand(1:Int(abs(b)) - 1)

    flag, q = divides(a*b + r, b)

    @test !flag && q == 0
    @test !is_divisible_by(a*b + r, b)
  end
end

@testset "ZZRingElem.gcd_lcm" begin
  a = ZZRingElem(12)
  b = ZZRingElem(26)

  @test gcd(a, b) == 2
  @test gcd(a, 26) == 2
  @test gcd(12, b) == 2

  c = ZZRingElem(2^2 * 3 * 5^2 * 7)
  zero = ZZRingElem(0)
  one = ZZRingElem(1)

  @test gcd(130 * c, 618 * c, 817 * c, 177 * c) == c
  @test gcd(one, one, one, one, one) == 1
  @test gcd(zero, zero, zero, zero) == 0

  @test_throws ErrorException gcd(ZZRingElem[])

  @test gcd(ZZRingElem[8]) == 8

  @test gcd([ZZRingElem(10), ZZRingElem(2)]) == 2

  @test gcd([ZZRingElem(1), ZZRingElem(2), ZZRingElem(3)]) == 1

  @test gcd([ZZRingElem(9), ZZRingElem(27), ZZRingElem(3)]) == 3

  @test lcm(a, b) == 156
  @test lcm(12, b) == 156
  @test lcm(a, 26) == 156

  c = ZZRingElem(2^2 * 3 * 5^2 * 7)
  zero = ZZRingElem(0)
  one = ZZRingElem(1)

  @test lcm(2 * c, 2 * c, 3 * c, 19 * c) == 114 * c
  @test lcm(one, one, one, one, one) == 1
  @test lcm(zero, one, one, one, one, one) == 0

  @test_throws ErrorException lcm(ZZRingElem[])

  @test lcm(ZZRingElem[2]) == 2

  @test lcm(ZZRingElem[2, 3]) == 6

  @test lcm(ZZRingElem[2, 2, 2]) == 2

  @test lcm(ZZRingElem[2, 3, 2]) == 6
end

@testset "ZZRingElem.logarithm" begin
  a = ZZRingElem(12)
  b = ZZRingElem(26)

  @test flog(b, a) == 1

  @test_throws DomainError flog(b, -a)

  @test flog(b, 12) == 1

  @test_throws DomainError flog(b, -12)

  @test clog(b, a) == 2

  @test_throws DomainError clog(b, -a)

  @test clog(b, 12) == 2

  @test_throws DomainError clog(b, -12)

  @test log(ZZ(2), ZZ(4)) == 2.0
  @test_throws DomainError log(ZZ(-2))
end

@testset "ZZRingElem.adhoc_binary" begin
  a = ZZRingElem(-12)

  @test 3 + a == -9

  @test a + 3 == -9

  @test a - 3 == -15

  @test 5 - a == 17

  @test a*5 == -60

  @test 5*a == -60

  @test a%5 == -2

  a = one(ZZ)
  @test a * 1.5 isa BigFloat
  @test isapprox(a * 1.5, 1.5)
  @test 1.5 * a isa BigFloat
  @test isapprox(1.5 * a, 1.5)
  @test a * big"1.5" isa BigFloat
  @test isapprox(a * big"1.5", big"1.5")
  @test big"1.5" * a isa BigFloat
  @test isapprox(big"1.5" * a, big"1.5")

  @test 1.5/a isa BigFloat
  @test isapprox(1.5/a, 1.5)
  @test big"1.5"/a isa BigFloat
  @test isapprox(big"1.5"/a, 1.5)
end

@testset "ZZRingElem.adhoc_division" begin
  a = ZZRingElem(-12)

  @test fdiv(a, 5) == -3

  @test tdiv(a, 7) == -1

  @test cdiv(a, 7) == -1

  @test div(a, 3) == -4

  @test div(-12, ZZRingElem(3)) == -4

  @test mod(-12, ZZRingElem(3)) == 0

  @test isa(mod(ZZRingElem(2), -3), ZZRingElem)

  @test mod(ZZRingElem(2), -3) == -1

  @test rem(-12, ZZRingElem(3)) == 0

  @test_throws ArgumentError divexact(ZZ(2), 3)

  b = ZZ(123)
  @test b % UInt == 123 % UInt
  @test -b % UInt == -123 % UInt
  @test b^29 % UInt == big(123)^29 % UInt
  @test -b^29 % UInt == -big(123)^29 % UInt
end

@testset "ZZRingElem.shift.." begin
  a = ZZRingElem(-12)

  @test a >> 3 == -2

  @test fdivpow2(a, 2) == -3

  @test_throws DomainError fdivpow2(a, -1)

  @test cdivpow2(a, 2) == -3

  @test_throws DomainError cdivpow2(a, -1)

  @test tdivpow2(a, 2) == -3

  @test_throws DomainError tdivpow2(a, -1)

  @test a << 4 == -192
end

@testset "ZZRingElem.powering" begin
  a = ZZRingElem(-12)

  @test a^5 == a^ZZRingElem(5) == -248832

  @test isone(a^0) && isone(a^ZZRingElem(0))

  a = ZZRingElem(2)
  @test_throws ErrorException a^(a^200)

  for a in ZZRingElem.(-5:5)
    for e = -5:-1
      if a != 1 && a != -1
        @test_throws DomainError a^e
        @test_throws DomainError a^ZZRingElem(e)
      end
    end
    @test a^1 == a^ZZRingElem(1) == a
    @test a^1 !== a^ZZRingElem(1) !== a
  end

  a = ZZRingElem(1)
  for e = -2:2
    @test isone(a^e) && isone(a^ZZRingElem(e))
    @test a^e !== a^ZZRingElem(e) !== a
  end

  a = ZZRingElem(-1)
  for e = [-3, -1, 1, 3, 5]
    @test a^e == a^ZZRingElem(e) == a
    @test a^e !== a^ZZRingElem(e) !== a
  end
  for e = [-2, 0, 2, 4]
    @test isone(a^e) && isone(a^ZZRingElem(e))
  end
end

@testset "ZZRingElem.comparison" begin
  a = ZZRingElem(-12)
  b = ZZRingElem(5)

  @test a < b

  @test b > a

  @test b >= a

  @test a <= b

  @test a == ZZRingElem(-12)

  @test a != b

  @test isequal(a, ZZRingElem(-12))

  @test cmpabs(a, b) == 1

  @test cmp(a, b) == -1

  @test ZZRingElem(2) < 47632748687326487326487326487326

  @test ZZRingElem(2) < 476327486873264873264873264873264837624982
end

@testset "ZZRingElem.adhoc_comparison" begin

  a = ZZRingElem(12)

  # values less than a
  lt = [-40, UInt(3), 3, 3//1, big(5)//big(3), Float64(-40), BigFloat(-40)]

  # values equal to a
  eq = [12, UInt(12), 12//1, big(12), big(12)//1, Float64(12), BigFloat(12)]

  # values greater than a
  gt = [40, UInt(40), 40//1, big(40)//big(3), Float64(40), BigFloat(40)]

  @testset "lt $b" for b in lt
    @test b < a
    @test b <= a
    @test !(b > a)
    @test !(b >= a)

    @test !(a < b)
    @test !(a <= b)
    @test a > b
    @test a >= b

    @test !(a == b)
    @test !(b == a)
    @test a != b
    @test b != a
  end

  @testset "eq $b" for b in eq
    @test !(b < a)
    @test b <= a
    @test !(b > a)
    @test b >= a

    @test !(a < b)
    @test a <= b
    @test !(a > b)
    @test a >= b

    @test a == b
    @test b == a
    @test !(a != b)
    @test !(b != a)
  end

  @testset "gt $b" for b in gt
    @test !(b < a)
    @test !(b <= a)
    @test b > a
    @test b >= a

    @test a < b
    @test a <= b
    @test !(a > b)
    @test !(a >= b)

    @test !(a == b)
    @test !(b == a)
    @test a != b
    @test b != a
  end

  # Additional test for non-small a
  a = ZZ(2)^200
  b = BigFloat(2)
  @test b < a
  @test !(b > a)
  @test b != a

  b = BigFloat(2)^200
  @test !(b < a)
  @test !(b > a)
  @test b == a

  b = BigFloat(2)^201
  @test b > a
  @test !(b < a)
  @test b != a
end

@testset "ZZRingElem.unary_ops" begin
  @test -ZZRingElem(12) == -12

  @test ~ZZRingElem(-5) == 4
end

@testset "ZZRingElem.abs" begin
  @test abs(ZZRingElem(-12)) == 12
  @test abs2(ZZRingElem(-12)) == 144
  @test abs2(ZZRingElem(0)) == 0
end

@testset "ZZRingElem.divrem" begin
  @test fdivrem(ZZRingElem(12), ZZRingElem(5)) == (ZZRingElem(2), ZZRingElem(2))

  @test tdivrem(ZZRingElem(12), ZZRingElem(5)) == (ZZRingElem(2), ZZRingElem(2))

  @test ndivrem(ZZRingElem(12), ZZRingElem(5)) == (ZZRingElem(2), ZZRingElem(2))
  @test ndivrem(ZZRingElem(13), ZZRingElem(5)) == (ZZRingElem(3), ZZRingElem(-2))
  @test ndivrem(ZZRingElem(6), ZZRingElem(-4)) == (ZZRingElem(-1), ZZRingElem(2))

  @test divrem(ZZRingElem(12), ZZRingElem(5)) == (ZZRingElem(2), ZZRingElem(2))

  @test divrem(-ZZRingElem(2), ZZRingElem(3)) == (ZZRingElem(0), -ZZRingElem(2))
  @test divrem(-2, ZZRingElem(3)) == (ZZRingElem(0), -ZZRingElem(2))
  @test divrem(-ZZRingElem(2), 3) == (ZZRingElem(0), -ZZRingElem(2))

  @test Nemo.divrem(-ZZRingElem(2), ZZRingElem(3)) == (-ZZRingElem(1), ZZRingElem(1))
  @test Nemo.divrem(-2, ZZRingElem(3)) == (-ZZRingElem(1), ZZRingElem(1))
  @test Nemo.divrem(-ZZRingElem(2), 3) == (-ZZRingElem(1), ZZRingElem(1))
end

@testset "divrem and div with other rings" begin
  for (x, y) in [(12, 5), (-12, 5)]
    for r in [RoundToZero, RoundUp, RoundDown]
      @test (
        ZZ(Base.div(x, y, r))
        == Base.div(ZZ(x), y, r)
        == Base.div(x, ZZ(y), r)
        == Base.div(ZZ(x), ZZ(y), r)
      )
      a, b = Base.divrem(x, y, r)
      @test (
        (ZZ(a), ZZ(b))
        == Base.divrem(ZZ(x), y, r)
        == Base.divrem(x, ZZ(y), r)
        == Base.divrem(ZZ(x), ZZ(y), r)
      )
    end
  end
end

@testset "ZZRingElem.roots" begin
  @test sqrt(ZZRingElem(16)) == 4
  @test sqrt(ZZRingElem()) == 0

  @test_throws DomainError sqrt(-ZZRingElem(1))
  @test_throws ErrorException sqrt(ZZRingElem(12))

  @test is_square_with_sqrt(ZZRingElem(5)) == (false, 0)
  @test is_square_with_sqrt(ZZRingElem(4)) == (true, 2)

  f1, s1 = is_square_with_sqrt(-ZZRingElem(1))

  @test !f1

  @test isqrt(ZZRingElem(12)) == 3

  @test_throws DomainError isqrt(-ZZRingElem(12))

  @test isqrtrem(ZZRingElem(12)) == (3, 3)

  @test_throws DomainError isqrtrem(-ZZRingElem(12))

  @test root(ZZRingElem(1000), 3) == 10
  @test root(-ZZRingElem(27), 3) == -3
  @test root(ZZRingElem(27), 3; check=true) == 3

  @test_throws DomainError root(-ZZRingElem(1000), 4)
  @test_throws DomainError root(ZZRingElem(1000), -3)

  @test_throws ErrorException root(ZZRingElem(1100), 3; check=true)
  @test_throws ErrorException root(-ZZRingElem(40), 3; check=true)

  @test iroot(ZZRingElem(1000), 3) == 10
  @test iroot(ZZRingElem(1100), 3) == 10
  @test iroot(-ZZRingElem(40), 3) == -3

  @test_throws DomainError iroot(-ZZRingElem(1000), 4)
  @test_throws DomainError iroot(ZZRingElem(1000), -3)
end

@testset "ZZRingElem.is_squarefree" begin
  for T in [Int, ZZRingElem]
    @test !is_squarefree(T(0))
    @test is_squarefree(T(1))
    @test is_squarefree(T(3))
    @test !is_squarefree(T(-4))
  end
end

@testset "ZZRingElem.extended_gcd" begin
  @test gcdx(ZZRingElem(12), ZZRingElem(5)) == (1, -2, 5)
  @test gcdx(ZZRingElem(12), 5) == (1, -2, 5)
  @test gcdx(12, ZZRingElem(5)) == (1, -2, 5)

  @test gcdinv(ZZRingElem(5), ZZRingElem(12)) == (1, 5)
  @test gcdinv(ZZRingElem(5), 12) == (1, 5)
  @test gcdinv(5, ZZRingElem(12)) == (1, 5)

  @test_throws DomainError gcdinv(-ZZRingElem(5), ZZRingElem(12))

  @test_throws DomainError gcdinv(ZZRingElem(13), ZZRingElem(12))

  for i = -10:10
    for j = -10:10
      @test gcdx(ZZRingElem(i), ZZRingElem(j)) == gcdx(i, j)
    end
  end
end

@testset "ZZRingElem.bit_twiddling" begin
  a = ZZRingElem(12)

  @test popcount(a) == 2

  @test nextpow2(a) == 16

  @test prevpow2(a) == 8

  @test trailing_zeros(a) == 2

  combit!(a, 2)

  @test a == 8

  @test tstbit(a, 3)
  @test !tstbit(a, 9)
  @test !tstbit(a, -1)

  @test_throws DomainError combit!(a, -1)

  setbit!(a, 0)

  @test a == 9

  @test_throws DomainError setbit!(a, -1)

  clrbit!(a, 0)

  @test a == 8

  @test_throws DomainError clrbit!(a, -1)
end

@testset "ZZRingElem.unsafe" begin
  a = ZZRingElem(32)
  b = ZZRingElem(23)
  c = one(ZZ)
  d = ZZRingElem(-3)
  r = ZZRingElem()
  b_copy = deepcopy(b)
  c_copy = deepcopy(c)

  a = zero!(a)
  @test iszero(a)
  a = mul!(a, a, b)
  @test iszero(a)

  a = add!(a, a, b)
  @test a == b
  a = add!(a, a, 2)
  @test a == b + 2
  @test add!(a, -1, a) == b + 1

  a = add!(a, b^2)
  @test a == 1 + b + b^2

  a = mul!(a, a, b)
  @test a == (1 + b + b^2) * b
  a = mul!(a, a, 3)
  @test a == (1 + b + b^2) * b * 3

  a = addmul!(a, a, c)
  @test a == 2 * (1 + b + b^2) * b * 3

  Nemo.fmma!(r, a, b, c, d)
  @test r == a * b + c * d

  Nemo.fmms!(r, a, b, c, d)
  @test r == a * b - c * d

  @test b_copy == b
  @test c_copy == c
end

@testset "ZZRingElem.bases" begin
  a = ZZRingElem(12)

  @test bin(a) == "1100"

  @test oct(a) == "14"

  @test dec(a) == "12"

  @test hex(a) == "c"

  @test base(a, 13) == "c"

  @test nbits(a) == 4
  @test nbits(12) == 4
  @test nbits(BigInt(12)) == 4

  @test ndigits(a, 3) == 3

  a = ZZRingElem(4611686837384281896) # must not be an "immediate" integer (but a GMP int)

  @test ndigits(a, 257) == 8
  @test ndigits(a, base = 257) == 8

  @test digits(a) == digits(BigInt(a))
  @test digits(a, base = 17) == digits(BigInt(a), base = 17)
  @test digits(a, base = 5, pad = 50) == digits(BigInt(a), base = 5, pad = 50)

  a = a^20

  @test ndigits(a, 257) == 155
  @test ndigits(a, base = 257) == 155

  @test digits(a) == digits(BigInt(a))
  @test digits(a, base = 17) == digits(BigInt(a), base = 17)
  @test digits(a, base = 5, pad = 50) == digits(BigInt(a), base = 5, pad = 50)

  a = zero(ZZ)

  @test ndigits(a, 257) == 1
  @test ndigits(a, base = 257) == 1

  @test digits(a) == digits(BigInt(a))
  @test digits(a, base = 17) == digits(BigInt(a), base = 17)
  @test digits(a, base = 5, pad = 50) == digits(BigInt(a), base = 5, pad = 50)

  a = -ZZRingElem(4611686837384281896)

  @test digits(a) == digits(BigInt(a))
  @test digits(a, base = 17) == digits(BigInt(a), base = 17)
  @test digits(a, base = 5, pad = 50) == digits(BigInt(a), base = 5, pad = 50)
end

@testset "ZZRingElem.string_io" begin
  a = ZZRingElem(12)

  @test string(a) == "12"
end

@testset "ZZRingElem.modular_arithmetic" begin
  @test powermod(ZZRingElem(12), ZZRingElem(110), ZZRingElem(13)) == 1

  @test_throws DomainError powermod(ZZRingElem(12), ZZRingElem(110), ZZRingElem(-1))

  @test powermod(ZZRingElem(12), 110, ZZRingElem(13)) == 1

  @test_throws DomainError powermod(ZZRingElem(12), 110, ZZRingElem(-1))

  @test invmod(ZZRingElem(12), ZZRingElem(13)) == 12

  @test_throws DomainError invmod(ZZRingElem(12), ZZRingElem(-13))

  @test sqrtmod(ZZRingElem(12), ZZRingElem(13)) == 5

  @test_throws DomainError sqrtmod(ZZRingElem(12), ZZRingElem(-13))

  @test_throws ErrorException sqrtmod(ZZRingElem(-7), ZZRingElem(1024))

  @test mod_sym(ZZ(0), ZZ(5)) == ZZ(0)
  @test mod_sym(ZZ(0), ZZ(-5)) == ZZ(0)
  @test mod_sym(ZZ(1), ZZ(5)) == ZZ(1)
  @test mod_sym(ZZ(1), ZZ(-5)) == ZZ(1)
  @test mod_sym(ZZ(-1), ZZ(5)) == ZZ(-1)
  @test mod_sym(ZZ(-1), ZZ(-5)) == ZZ(-1)
  @test mod_sym(ZZ(4), ZZ(5)) == ZZ(-1)
  @test mod_sym(ZZ(4), ZZ(-5)) == ZZ(-1)
  @test mod_sym(ZZ(-4), ZZ(5)) == ZZ(1)
  @test mod_sym(ZZ(-4), ZZ(-5)) == ZZ(1)

  for (p, k) in [(2, 4), (2, 5), (5, 1), (5, 5)]
    b = rand(1:p^k - 1) % p^k
    while gcd(b, p) != 1
      b = rand(1:p^k - 1) % p^k
    end
    a = powermod(b, 2, p^k)
    c = Nemo.sqrtmod_pk(a, p, k)
    @test c^2 % p^k == a
    c = Nemo._sqrtmod_pk_small(a, p, k)
    @test c^2 % p^k == a
  end

  @test_throws ArgumentError Nemo.sqrtmod_pk(2, 2, 5)
  @test_throws ErrorException Nemo.sqrtmod_pk(3, 5, 10)
end

@testset "ZZRingElem.crt" begin
  function testit(r, m, check=true)
    n = length(r)
    s = rand(Bool)
    a = @inferred crt(r, m, s; check=check)
    b, l = @inferred crt_with_lcm(r, m, !s; check=check)
    if !iszero(l)
      if s
        @test -l < 2*a <= l
        @test 0 <= b < l
      else
        @test 0 <= a < l
        @test -l < 2*b <= l
      end
    end
    for i in 1:length(r)
      @test is_divisible_by(l, m[i])
      @test is_divisible_by(a - r[i], m[i])
      @test is_divisible_by(b - r[i], m[i])
    end
  end

  testit([ZZ(1)], [ZZ(4)])
  testit([ZZ(1)], [ZZ(0)])

  testit([ZZ(1), ZZ(2)], [ZZ(4), ZZ(5)])
  testit([ZZ(1), ZZ(3)], [ZZ(4), ZZ(6)], false)
  testit([ZZ(1), ZZ(3)], [ZZ(4), ZZ(6)], true)
  testit([ZZ(-1), ZZ(2)], [ZZ(0), ZZ(3)])
  testit([ZZ(-1), ZZ(2)], [ZZ(3), ZZ(0)])
  @test_throws Exception crt([ZZ(1), ZZ(2)], [ZZ(0), ZZ(3)])
  @test_throws Exception crt([ZZ(1), ZZ(2)], [ZZ(3), ZZ(0)])
  @test_throws Exception crt([ZZ(1), ZZ(2)], [ZZ(4), ZZ(6)])
  @test parent(crt([ZZ(1), ZZ(2)], [ZZ(4), ZZ(6)]; check=false)) == ZZ # junk but no throw

  testit([ZZ(1), ZZ(2), ZZ(3)], [ZZ(4), ZZ(5), ZZ(7)])
  testit([ZZ(1), ZZ(2), ZZ(3)], [ZZ(4), ZZ(5), ZZ(-6)])
  testit([ZZ(-1), ZZ(2), ZZ(-1)], [ZZ(0), ZZ(3), ZZ(0)])
  @test_throws Exception crt([ZZ(1), ZZ(2), ZZ(2)], [ZZ(4), ZZ(5), ZZ(6)])
  @test_throws Exception crt([ZZ(-1), ZZ(2), ZZ(2)], [ZZ(0), ZZ(3), ZZ(0)])
  @test_throws Exception crt([ZZ(-1), ZZ(-1), ZZ(2)], [ZZ(0), ZZ(0), ZZ(4)])

  @test crt(ZZRingElem(5), ZZRingElem(13), ZZRingElem(7), ZZRingElem(37), true) == 44
  @test crt(ZZRingElem(1), ZZRingElem(2), ZZRingElem(2), ZZRingElem(-3), true) == -1
  @test crt(ZZRingElem(1), ZZRingElem(2), ZZRingElem(0), ZZRingElem(3), true) == 3
  @test crt(ZZRingElem(1), ZZRingElem(-2), ZZRingElem(2), ZZRingElem(3), false) == 5
  @test crt(ZZRingElem(1), ZZRingElem(2), ZZRingElem(0), ZZRingElem(3), false) == 3
  @test crt(ZZRingElem(11),ZZRingElem(30),ZZRingElem(41),ZZRingElem(85)) == 41
  @test crt(ZZRingElem(11), ZZRingElem(30), ZZRingElem(40), ZZRingElem(85); check=false) isa ZZRingElem
  @test_throws Exception crt(ZZRingElem(11), ZZRingElem(30), ZZRingElem(40), ZZRingElem(85)) isa ZZRingElem

  for s in (true, false)
    rr = ZZ(99)^150
    mm = ZZ(101)^100  # something prime to the typemax/mins
    for (r1, m1, r2, m2) in ((5, 13, 7, 37),
                             (5, 13, -7, 37),
                             (5, 13, 7, -37),
                             (5, 13, -7, -37),
                             (0, 0, 0, 0),
                             (2, 4, 0, 2),
                             (0, 2, 2, 4),
                             (1, 0, 1, 3),
                             (1, 3, 1, 0),
                             (1, 3, UInt(1), UInt(0)),
                             (rr, mm, 100, typemax(UInt)),
                             (rr, mm, -200, typemax(UInt)),
                             (rr, mm, typemin(Int), typemax(UInt)),
                             (rr, mm, typemin(Int), typemax(UInt)),
                             (rr, mm, typemax(Int), typemax(UInt)),
                             (rr, mm, typemin(Int), typemax(Int)),
                             (rr, mm, typemax(Int), typemax(Int)),
                             (rr, mm, typemin(Int), typemin(Int)),
                             (rr, mm, typemax(Int), typemin(Int)),
                             (rr, mm, typemax(UInt), typemin(Int)),
                             (rr, mm, typemax(UInt), typemin(Int)))

      @test crt(ZZ(r1), ZZ(m1), r2, m2, s) ==
      crt(ZZ(r1), ZZ(m1), ZZ(r2), ZZ(m2), s)

      @test crt_with_lcm(ZZ(r1), ZZ(m1), r2, m2, s) ==
      crt_with_lcm(ZZ(r1), ZZ(m1), ZZ(r2), ZZ(m2), s)
    end
  end

  @test_throws Exception crt(ZZRingElem(1), ZZRingElem(3), UInt(2), UInt(0))
  @test_throws Exception crt(ZZRingElem(1), ZZRingElem(3), 2, 0)
  @test_throws Exception crt(ZZRingElem(11), ZZRingElem(30), UInt(40), UInt(85))
  @test_throws Exception crt(ZZRingElem(11), ZZRingElem(30), 40, 85)
end

@testset "ZZRingElem.factor" begin
  a = ZZRingElem(-3*5*7*11*13^10)

  fact = factor(a)

  b = unit(fact)

  for (p, e) in fact
    b = b*p^e
  end

  @test b == a

  @test fact[ZZRingElem(3)] == 1
  @test fact[ZZRingElem(5)] == 1
  @test fact[ZZRingElem(7)] == 1
  @test fact[ZZRingElem(11)] == 1
  @test fact[ZZRingElem(13)] == 10
  @test 3 in fact
  @test !(2 in fact)

  fact = factor(ZZRingElem(-1))

  @test collect(fact) == Pair{ZZRingElem, Int}[]

  fact = factor(ZZRingElem(-2))

  @test occursin("2", sprint(show, "text/plain", fact))

  @test collect(fact) == [ZZRingElem(2) => 1]
  @test unit(fact) == -1

  @test_throws ArgumentError factor(ZZRingElem(0))

  for (T, a) in [(Int, -3*5*7*11*13^5), (UInt, UInt(3*5*7*11*13^5))]
    fact = factor(a)

    b = unit(fact)

    for (p, e) in fact
      b = b*p^e
    end

    @test b == a

    @test fact[T(3)] == 1
    @test fact[T(5)] == 1
    @test fact[T(7)] == 1
    @test fact[T(11)] == 1
    @test fact[T(13)] == 5
    @test T(3) in fact
    @test !(T(2) in fact)
  end

  fact = factor(-1)

  @test collect(fact) == Pair{Int, Int}[]

  fact = factor(-2)

  @test occursin("2", sprint(show, "text/plain", fact))

  @test collect(fact) == [2 => 1]
  @test unit(fact) == -1

  @test_throws ArgumentError factor(0)
  @test_throws ArgumentError factor(UInt(0))

  fact = factor(next_prime(3*UInt(2)^62))
  @test length(fact) == 1

  n = ZZRingElem(2 * 1125899906842679)
  b, f = Nemo.ecm(n)
  @test mod(n, f) == 0

  n = factorial(ZZ(50))
  d, u = Nemo._factor_trial_range(n, 0, 50)
  @test isone(u)
  @test prod(p^e for (p, e) in d) == n

  let
    a = ZZ(65261404486168272596469055321032207458900347428758158658012236487063743080177113810110282680249226184903475017526897956542827985941267684646505051975441057446672916412353515625)
    fact = factor(a)
    @test a == prod(p^e for (p, e) in fact)
  end
end

@testset "ZZRingElem.number_theoretic" begin
  @test is_prime(ZZRingElem(13))

  @test is_prime(13)

  @test is_probable_prime(ZZRingElem(13))

  @test is_divisible_by(ZZRingElem(12), ZZRingElem(6))

  n = ZZRingElem(2^2 * 3 * 13^2)
  d = ZZRingElem.([1, 2, 3, 4, 6, 12, 13, 26, 39, 52, 78, 156, 169, 338, 507, 676, 1014, 2028])
  p = ZZRingElem.([2, 3, 13])
  divsr = divisors(n)
  pdivsr = prime_divisors(n)
  @test all([k in divsr for k in d])
  @test all([k in pdivsr for k in p])

  @test next_prime(ZZ(-9)) == 2
  @test next_prime(ZZ(2)^30 - 1) == next_prime(ZZ(2)^30) == 1073741827
  @test next_prime(ZZ(2)^31 - 1) == next_prime(ZZ(2)^31) == 2147483659
  @test next_prime(ZZ(2)^32 - 1) == next_prime(ZZ(2)^32) == 4294967311
  @test next_prime(ZZ(2)^62 - 1) == next_prime(ZZ(2)^62) == 4611686018427388039
  @test next_prime(ZZ(2)^63 - 1) == next_prime(ZZ(2)^63) == 9223372036854775837
  @test next_prime(ZZ(2)^64 - 1) == next_prime(ZZ(2)^64) == 18446744073709551629
  @test next_prime(ZZ(10)^50, false) == ZZ(10)^50 + 151

  @test next_prime(-9) == 2
  @test next_prime(2) == 3
  @test next_prime(3, false) == 5
  @test_throws Exception next_prime(typemax(Int))

  @test next_prime(UInt(10)) == 11
  @test next_prime(UInt(11), false) == 13
  @test_throws Exception next_prime(typemax(UInt))

  @test factorial(ZZ(100)) == ZZRingElem("93326215443944152681699238856266700490715968264381621468592963895217599993229915608941463976156518286253697920827223758251185210916864000000000000000000000000")

  @test divisor_sigma(ZZRingElem(128), 10) == ZZRingElem("1181745669222511412225")

  @test_throws DomainError divisor_sigma(ZZRingElem(1), -1)

  @test euler_phi(ZZRingElem(12480)) == 3072
  @test euler_phi(factor(ZZ(12480))) == 3072

  @test fibonacci(2) == 1

  @test fibonacci(0) == 0

  @test fibonacci(-2) == -1

  @test fibonacci(ZZRingElem(2)) == 1

  @test fibonacci(ZZRingElem(-2)) == -1

  @test_throws OverflowError fibonacci(999)

  @test_throws DomainError  euler_phi(-ZZRingElem(12480))

  @test remove(ZZRingElem(12), ZZRingElem(2)) == (2, 3)
  @test remove(-3*6^3, 6) === (3, -3)
  @test remove(-4*6^0, 6) === (0, -4)
  @test remove(typemin(Int), 2) === (trailing_zeros(typemin(Int)), -1)
  @test remove(typemin(Int), 3) === (0, typemin(Int))
  @test remove(UInt(24), 6) === (1, UInt(4))

  for a in (UInt(1), UInt(8), UInt(12), typemax(UInt))
    n = trailing_zeros(a)
    @test remove(a, UInt(2)) === (n, a >> n)
    @test remove(a, 2) === (n, a >> n)
  end

  @test remove(-3*6^3, BigInt(6)) == (3, -3)
  @test remove(-3*6^3, BigInt(6)) isa Tuple{Int, BigInt}

  @test remove(BigInt(-3*6^3), BigInt(6)) == (3, -3)
  @test remove(BigInt(-3*6^3), BigInt(6)) isa Tuple{Int, BigInt}

  @test remove(BigInt(-3*6^3), 6) == (3, -3)
  @test remove(BigInt(-3*6^3), 6) isa Tuple{Int, BigInt}

  @test valuation(ZZRingElem(12), ZZRingElem(2)) == 2

  @test valuation(ZZRingElem(12), 2) == 2

  @test valuation(12, 2) == 2

  @test_throws ErrorException valuation(0, 2)

  @test divisor_lenstra(ZZRingElem(12), ZZRingElem(4), ZZRingElem(5)) == 4

  @test_throws DomainError divisor_lenstra(ZZRingElem(12), -ZZRingElem(4), ZZRingElem(5))
  @test_throws DomainError divisor_lenstra(ZZRingElem(1), ZZRingElem(4), ZZRingElem(5))
  @test_throws DomainError divisor_lenstra(ZZRingElem(10), ZZRingElem(4), ZZRingElem(3))

  @test rising_factorial(ZZRingElem(12), 5) == 524160

  @test_throws DomainError rising_factorial(ZZRingElem(12), -1)

  @test rising_factorial(12, 5) == 524160

  @test_throws DomainError rising_factorial(12, -1)

  @test primorial(7) == 210

  @test_throws OverflowError primorial(999)

  @test_throws DomainError primorial(-7)

  for a in [-ZZ(3)^100, ZZ(3)^100]
    @test binomial(a, ZZ(2)) == divexact(a*(a - 1), 2)
    @test binomial(a, ZZ(3)) == divexact(a*(a - 1)*(a - 2), 6)
  end
  for a in -9:9, b in -2:9
    @test binomial(ZZ(a), ZZ(b)) == binomial(big(a), big(b))
  end
  # ok for julia on windows
  n = typemax(Clong)
  for a in [0, 1, 2, n-2, n-1, n], b in [n-2, n-1, n]
    @test binomial(ZZ(a), ZZ(b)) == binomial(big(a), big(b))
  end
  # avoid julia on windows
  for n in [ZZ(typemax(Int)), ZZ(typemax(UInt)), ZZ(typemax(UInt)) + 10]
    @test binomial(n, ZZ(1)) == n
    @test binomial(n, n) == 1
    @test binomial(n, n - 1) == n
    @test binomial(n - 1, n) == 0
    @test binomial(ZZ(-1), n) == (-1)^isodd(n)
    @test binomial(ZZ(-2), n) == (-1)^isodd(n)*(n + 1)
  end
  @test_throws ErrorException binomial(ZZ(2)^101, ZZ(2)^100)

  @test binomial(ZZ(12), ZZ(5)) == 792
  @test binomial(UInt(12), UInt(5), ZZ) == 792

  @test bell(12) == 4213597

  @test_throws DomainError bell(-1)

  @test moebius_mu(ZZRingElem(13)) == -1

  @test_throws DomainError moebius_mu(-ZZRingElem(1))

  @test jacobi_symbol(ZZRingElem(2), ZZRingElem(5)) == -1

  @test_throws DomainError jacobi_symbol(ZZRingElem(5), ZZRingElem(-2))

  @test_throws DomainError jacobi_symbol(ZZRingElem(5), ZZRingElem(2))

  @test jacobi_symbol(2, 3) == -1

  for T in (Int, Integer, ZZRingElem)
    for S in (Int, Integer, ZZRingElem)
      @test jacobi_symbol(T(2), S(3)) == -1
    end
  end

  @test_throws DomainError jacobi_symbol(2, 0)

  @test_throws DomainError jacobi_symbol(-5, 4)

  for T in (Int, ZZRingElem)
    for iters = 1:1000
      m1 = T(rand(-100:100))
      n1 = T(rand(-100:100))
      m2 = T(rand(-100:100))
      n2 = T(rand(-100:100))

      @test m1 == -1 || n1 == -1 || m2 == -1 || n2 == -1 ||
      kronecker_symbol(m1*m2, n1*n2) ==
      kronecker_symbol(m1, n1)*kronecker_symbol(m1, n2)*
      kronecker_symbol(m2, n1)*kronecker_symbol(m2, n2)
    end

    @test kronecker_symbol(T(-5), T(-1)) == -1
    @test kronecker_symbol(T(5), T(-1)) == 1
    @test kronecker_symbol(T(4), T(10)) == 0
    @test kronecker_symbol(T(1), T(2)) == 1
    @test kronecker_symbol(T(7), T(2)) == 1
    @test kronecker_symbol(T(3), T(2)) == -1
    @test kronecker_symbol(T(5), T(2)) == -1
    @test kronecker_symbol(T(3), T(4)) == 1
    @test kronecker_symbol(T(5), T(4)) == 1
    @test kronecker_symbol(T(2), T(0)) == 0
    @test kronecker_symbol(T(-2), T(0)) == 0
    @test kronecker_symbol(T(0), T(0)) == 0
    @test kronecker_symbol(T(-1), T(0)) == 1
    @test kronecker_symbol(T(1), T(0)) == 1
  end

  if !(Sys.iswindows() && (Int == Int64))

    @test number_of_partitions(10) == 42

    @test number_of_partitions(ZZRingElem(1000)) == ZZRingElem("24061467864032622473692149727991")

    @test number_of_partitions(0) == 1

    @test number_of_partitions(-1) == 0

    @test number_of_partitions(ZZRingElem(-2)) == 0
  end

  # Perfect power
  for T in [Int, BigInt, ZZRingElem]
    @test @inferred is_perfect_power(T(4))
    @test is_perfect_power(T(36))
    @test is_perfect_power(T(-27))
    @test is_perfect_power(T(1))
    @test is_perfect_power(T(-1))
    @test is_perfect_power(T(0))
    @test !is_perfect_power(T(2))
    @test !is_perfect_power(T(-4))
    @test !is_perfect_power(T(6))
  end
  @test is_perfect_power(ZZRingElem(10940293781057873954324736))

  # Perfect power with data
  for T in [Int, BigInt, ZZRingElem]
    @test (@inferred is_perfect_power_with_data(T(5))) == (1, 5)
    @test (@inferred is_perfect_power_with_data(T(-5))) == (1, -5)
    @test (@inferred is_perfect_power_with_data(T(64))) == (6, 2)
    @test (@inferred is_perfect_power_with_data(T(-64))) == (3, -4)
    @test (@inferred is_perfect_power_with_data(T(1))) == (0, 1)
  end

  # is_power(::ZZRingElem, n::Int)
  @test is_power(ZZ(-64), 6)[1] == false
  @test is_power(ZZ(64), 6) == (true, 2)

  # Prime power
  for T in [Int, BigInt, ZZRingElem]
    @test @inferred Nemo.is_prime_power(T(2))
    @test (@inferred Nemo.is_prime_power_with_data(T(2))) == (true, 1, T(2))
    @test Nemo.is_prime_power(T(4))
    @test (@inferred Nemo.is_prime_power_with_data(T(4))) == (true, 2, T(2))
    @test Nemo.is_prime_power(T(27))
    @test (@inferred Nemo.is_prime_power_with_data(T(27))) == (true, 3, T(3))
    @test !Nemo.is_prime_power(T(1))
    @test !Nemo.is_prime_power(T(6))
    @test !Nemo.is_prime_power(T(-3))
    @test !Nemo.is_prime_power(-T(7)^4)
  end
  @test !Nemo.is_prime_power(ZZRingElem(10940293781057873954324736))
end

@testset "ZZRingElem.tdivrem" begin
  @test tdivrem(ZZ(-6), ZZ(+4)) == (-1, -2)
  @test tdivrem(ZZ(-5), ZZ(+4)) == (-1, -1)
  @test tdivrem(ZZ(-4), ZZ(+4)) == (-1,  0)
  @test tdivrem(ZZ(-3), ZZ(+4)) == ( 0, -3)
  @test tdivrem(ZZ(-2), ZZ(+4)) == ( 0, -2)
  @test tdivrem(ZZ(-1), ZZ(+4)) == ( 0, -1)
  @test tdivrem(ZZ( 0), ZZ(+4)) == ( 0,  0)
  @test tdivrem(ZZ(+1), ZZ(+4)) == ( 0, +1)
  @test tdivrem(ZZ(+2), ZZ(+4)) == ( 0, +2)
  @test tdivrem(ZZ(+3), ZZ(+4)) == ( 0, +3)
  @test tdivrem(ZZ(+4), ZZ(+4)) == (+1,  0)
  @test tdivrem(ZZ(+5), ZZ(+4)) == (+1, +1)
  @test tdivrem(ZZ(+6), ZZ(+4)) == (+1, +2)

  @test tdivrem(ZZ(+6), ZZ(-4)) == (-1, +2)
  @test tdivrem(ZZ(+5), ZZ(-4)) == (-1, +1)
  @test tdivrem(ZZ(+4), ZZ(-4)) == (-1,  0)
  @test tdivrem(ZZ(+3), ZZ(-4)) == ( 0, +3)
  @test tdivrem(ZZ(+2), ZZ(-4)) == ( 0, +2)
  @test tdivrem(ZZ(+1), ZZ(-4)) == ( 0, +1)
  @test tdivrem(ZZ( 0), ZZ(-4)) == ( 0,  0)
  @test tdivrem(ZZ(-1), ZZ(-4)) == ( 0, -1)
  @test tdivrem(ZZ(-2), ZZ(-4)) == ( 0, -2)
  @test tdivrem(ZZ(-3), ZZ(-4)) == ( 0, -3)
  @test tdivrem(ZZ(-4), ZZ(-4)) == (+1,  0)
  @test tdivrem(ZZ(-5), ZZ(-4)) == (+1, -1)
  @test tdivrem(ZZ(-6), ZZ(-4)) == (+1, -2)
end

@testset "ZZRingElem.fdivrem" begin
  @test fdivrem(ZZ(-6), ZZ(+4)) == (-2, +2)
  @test fdivrem(ZZ(-5), ZZ(+4)) == (-2, +3)
  @test fdivrem(ZZ(-4), ZZ(+4)) == (-1,  0)
  @test fdivrem(ZZ(-3), ZZ(+4)) == (-1, +1)
  @test fdivrem(ZZ(-2), ZZ(+4)) == (-1, +2)
  @test fdivrem(ZZ(-1), ZZ(+4)) == (-1, +3)
  @test fdivrem(ZZ( 0), ZZ(+4)) == ( 0,  0)
  @test fdivrem(ZZ(+1), ZZ(+4)) == ( 0, +1)
  @test fdivrem(ZZ(+2), ZZ(+4)) == ( 0, +2)
  @test fdivrem(ZZ(+3), ZZ(+4)) == ( 0, +3)
  @test fdivrem(ZZ(+4), ZZ(+4)) == (+1,  0)
  @test fdivrem(ZZ(+5), ZZ(+4)) == (+1, +1)
  @test fdivrem(ZZ(+6), ZZ(+4)) == (+1, +2)

  @test fdivrem(ZZ(+6), ZZ(-4)) == (-2, -2)
  @test fdivrem(ZZ(+5), ZZ(-4)) == (-2, -3)
  @test fdivrem(ZZ(+4), ZZ(-4)) == (-1,  0)
  @test fdivrem(ZZ(+3), ZZ(-4)) == (-1, -1)
  @test fdivrem(ZZ(+2), ZZ(-4)) == (-1, -2)
  @test fdivrem(ZZ(+1), ZZ(-4)) == (-1, -3)
  @test fdivrem(ZZ( 0), ZZ(-4)) == ( 0,  0)
  @test fdivrem(ZZ(-1), ZZ(-4)) == ( 0, -1)
  @test fdivrem(ZZ(-2), ZZ(-4)) == ( 0, -2)
  @test fdivrem(ZZ(-3), ZZ(-4)) == ( 0, -3)
  @test fdivrem(ZZ(-4), ZZ(-4)) == (+1,  0)
  @test fdivrem(ZZ(-5), ZZ(-4)) == (+1, -1)
  @test fdivrem(ZZ(-6), ZZ(-4)) == (+1, -2)
end

@testset "ZZRingElem.cdivrem" begin
  @test cdivrem(ZZ(-6), ZZ(+4)) == (-1, -2)
  @test cdivrem(ZZ(-5), ZZ(+4)) == (-1, -1)
  @test cdivrem(ZZ(-4), ZZ(+4)) == (-1,  0)
  @test cdivrem(ZZ(-3), ZZ(+4)) == ( 0, -3)
  @test cdivrem(ZZ(-2), ZZ(+4)) == ( 0, -2)
  @test cdivrem(ZZ(-1), ZZ(+4)) == ( 0, -1)
  @test cdivrem(ZZ( 0), ZZ(+4)) == ( 0,  0)
  @test cdivrem(ZZ(+1), ZZ(+4)) == (+1, -3)
  @test cdivrem(ZZ(+2), ZZ(+4)) == (+1, -2)
  @test cdivrem(ZZ(+3), ZZ(+4)) == (+1, -1)
  @test cdivrem(ZZ(+4), ZZ(+4)) == (+1,  0)
  @test cdivrem(ZZ(+5), ZZ(+4)) == (+2, -3)
  @test cdivrem(ZZ(+6), ZZ(+4)) == (+2, -2)

  @test cdivrem(ZZ(+6), ZZ(-4)) == (-1, +2)
  @test cdivrem(ZZ(+5), ZZ(-4)) == (-1, +1)
  @test cdivrem(ZZ(+4), ZZ(-4)) == (-1,  0)
  @test cdivrem(ZZ(+3), ZZ(-4)) == ( 0, +3)
  @test cdivrem(ZZ(+2), ZZ(-4)) == ( 0, +2)
  @test cdivrem(ZZ(+1), ZZ(-4)) == ( 0, +1)
  @test cdivrem(ZZ( 0), ZZ(-4)) == ( 0,  0)
  @test cdivrem(ZZ(-1), ZZ(-4)) == (+1, +3)
  @test cdivrem(ZZ(-2), ZZ(-4)) == (+1, +2)
  @test cdivrem(ZZ(-3), ZZ(-4)) == (+1, +1)
  @test cdivrem(ZZ(-4), ZZ(-4)) == (+1,  0)
  @test cdivrem(ZZ(-5), ZZ(-4)) == (+2, +3)
  @test cdivrem(ZZ(-6), ZZ(-4)) == (+2, +2)
end

@testset "ZZRingElem.ntdivrem" begin
  @test ntdivrem(ZZ(-6), ZZ(+4)) == (-1, -2)
  @test ntdivrem(ZZ(-5), ZZ(+4)) == (-1, -1)
  @test ntdivrem(ZZ(-4), ZZ(+4)) == (-1,  0)
  @test ntdivrem(ZZ(-3), ZZ(+4)) == (-1, +1)
  @test ntdivrem(ZZ(-2), ZZ(+4)) == ( 0, -2)
  @test ntdivrem(ZZ(-1), ZZ(+4)) == ( 0, -1)
  @test ntdivrem(ZZ( 0), ZZ(+4)) == ( 0,  0)
  @test ntdivrem(ZZ(+1), ZZ(+4)) == ( 0, +1)
  @test ntdivrem(ZZ(+2), ZZ(+4)) == (+0, +2)
  @test ntdivrem(ZZ(+3), ZZ(+4)) == (+1, -1)
  @test ntdivrem(ZZ(+4), ZZ(+4)) == (+1,  0)
  @test ntdivrem(ZZ(+5), ZZ(+4)) == (+1, +1)
  @test ntdivrem(ZZ(+6), ZZ(+4)) == (+1, +2)

  @test ntdivrem(ZZ(+6), ZZ(-4)) == (-1, +2)
  @test ntdivrem(ZZ(+5), ZZ(-4)) == (-1, +1)
  @test ntdivrem(ZZ(+4), ZZ(-4)) == (-1,  0)
  @test ntdivrem(ZZ(+3), ZZ(-4)) == (-1, -1)
  @test ntdivrem(ZZ(+2), ZZ(-4)) == ( 0, +2)
  @test ntdivrem(ZZ(+1), ZZ(-4)) == ( 0, +1)
  @test ntdivrem(ZZ( 0), ZZ(-4)) == ( 0,  0)
  @test ntdivrem(ZZ(-1), ZZ(-4)) == ( 0, -1)
  @test ntdivrem(ZZ(-2), ZZ(-4)) == (+0, -2)
  @test ntdivrem(ZZ(-3), ZZ(-4)) == (+1, +1)
  @test ntdivrem(ZZ(-4), ZZ(-4)) == (+1,  0)
  @test ntdivrem(ZZ(-5), ZZ(-4)) == (+1, -1)
  @test ntdivrem(ZZ(-6), ZZ(-4)) == (+1, -2)
end

@testset "ZZRingElem.nfdivrem" begin
  @test nfdivrem(ZZ(-6), ZZ(+4)) == (-2, +2)
  @test nfdivrem(ZZ(-5), ZZ(+4)) == (-1, -1)
  @test nfdivrem(ZZ(-4), ZZ(+4)) == (-1,  0)
  @test nfdivrem(ZZ(-3), ZZ(+4)) == (-1, +1)
  @test nfdivrem(ZZ(-2), ZZ(+4)) == (-1, +2)
  @test nfdivrem(ZZ(-1), ZZ(+4)) == ( 0, -1)
  @test nfdivrem(ZZ( 0), ZZ(+4)) == ( 0,  0)
  @test nfdivrem(ZZ(+1), ZZ(+4)) == ( 0, +1)
  @test nfdivrem(ZZ(+2), ZZ(+4)) == ( 0, +2)
  @test nfdivrem(ZZ(+3), ZZ(+4)) == (+1, -1)
  @test nfdivrem(ZZ(+4), ZZ(+4)) == (+1,  0)
  @test nfdivrem(ZZ(+5), ZZ(+4)) == (+1, +1)
  @test nfdivrem(ZZ(+6), ZZ(+4)) == (+1, +2)

  @test nfdivrem(ZZ(+6), ZZ(-4)) == (-2, -2)
  @test nfdivrem(ZZ(+5), ZZ(-4)) == (-1, +1)
  @test nfdivrem(ZZ(+4), ZZ(-4)) == (-1,  0)
  @test nfdivrem(ZZ(+3), ZZ(-4)) == (-1, -1)
  @test nfdivrem(ZZ(+2), ZZ(-4)) == (-1, -2)
  @test nfdivrem(ZZ(+1), ZZ(-4)) == ( 0, +1)
  @test nfdivrem(ZZ( 0), ZZ(-4)) == ( 0,  0)
  @test nfdivrem(ZZ(-1), ZZ(-4)) == ( 0, -1)
  @test nfdivrem(ZZ(-2), ZZ(-4)) == ( 0, -2)
  @test nfdivrem(ZZ(-3), ZZ(-4)) == (+1, +1)
  @test nfdivrem(ZZ(-4), ZZ(-4)) == (+1,  0)
  @test nfdivrem(ZZ(-5), ZZ(-4)) == (+1, -1)
  @test nfdivrem(ZZ(-6), ZZ(-4)) == (+1, -2)
end

@testset "ZZRingElem.ncdivrem" begin
  @test ncdivrem(ZZ(-6), ZZ(+4)) == (-1, -2)
  @test ncdivrem(ZZ(-5), ZZ(+4)) == (-1, -1)
  @test ncdivrem(ZZ(-4), ZZ(+4)) == (-1,  0)
  @test ncdivrem(ZZ(-3), ZZ(+4)) == (-1, +1)
  @test ncdivrem(ZZ(-2), ZZ(+4)) == ( 0, -2)
  @test ncdivrem(ZZ(-1), ZZ(+4)) == ( 0, -1)
  @test ncdivrem(ZZ( 0), ZZ(+4)) == ( 0,  0)
  @test ncdivrem(ZZ(+1), ZZ(+4)) == ( 0, +1)
  @test ncdivrem(ZZ(+2), ZZ(+4)) == (+1, -2)
  @test ncdivrem(ZZ(+3), ZZ(+4)) == (+1, -1)
  @test ncdivrem(ZZ(+4), ZZ(+4)) == (+1,  0)
  @test ncdivrem(ZZ(+5), ZZ(+4)) == (+1, +1)
  @test ncdivrem(ZZ(+6), ZZ(+4)) == (+2, -2)

  @test ncdivrem(ZZ(+6), ZZ(-4)) == (-1, +2)
  @test ncdivrem(ZZ(+5), ZZ(-4)) == (-1, +1)
  @test ncdivrem(ZZ(+4), ZZ(-4)) == (-1,  0)
  @test ncdivrem(ZZ(+3), ZZ(-4)) == (-1, -1)
  @test ncdivrem(ZZ(+2), ZZ(-4)) == ( 0, +2)
  @test ncdivrem(ZZ(+1), ZZ(-4)) == ( 0, +1)
  @test ncdivrem(ZZ( 0), ZZ(-4)) == ( 0,  0)
  @test ncdivrem(ZZ(-1), ZZ(-4)) == ( 0, -1)
  @test ncdivrem(ZZ(-2), ZZ(-4)) == (+1, +2)
  @test ncdivrem(ZZ(-3), ZZ(-4)) == (+1, +1)
  @test ncdivrem(ZZ(-4), ZZ(-4)) == (+1,  0)
  @test ncdivrem(ZZ(-5), ZZ(-4)) == (+1, -1)
  @test ncdivrem(ZZ(-6), ZZ(-4)) == (+2, +2)
end

@testset "ZZRingElem.printing" begin
  @test ZZ === integer_ring()
  @test PrettyPrinting.detailed(ZZ) == "Integer ring"
  @test PrettyPrinting.oneline(ZZ) == "Integer ring"
  @test PrettyPrinting.supercompact(ZZ) == "ZZ"

  # test LowercaseOff
  io = PrettyPrinting.pretty(IOBuffer())
  print(PrettyPrinting.terse(io), PrettyPrinting.Lowercase(), ZZ)
  @test String(take!(io)) == "ZZ"
end

@testset "ZZRingElem.range" begin
  # UnitRange
  r = ZZ(-2):ZZ(10)
  @test r isa ZZRingElemUnitRange
  @test r[1] == ZZ(-2)
  @test r[ZZ(3)] == ZZ(0)
  @test_throws BoundsError r[ZZ(100)]
  @test 1 in r
  @test !(-3 in r)
  @test ZZ(1) in r
  @test !(ZZ(-3) in r)
  @test length(r) == 13

  @test mod(ZZ(6), ZZ(1):ZZ(3)) == ZZ(3)
  @test mod(6, ZZ(1):ZZ(3)) == ZZ(3)

  @test 2:ZZ(3) isa ZZRingElemUnitRange
  @test BigInt(2):ZZ(3) isa ZZRingElemUnitRange
  @test ZZ(2):3 isa ZZRingElemUnitRange
  @test ZZ(2):BigInt(3) isa ZZRingElemUnitRange

  # StepRange
  r = ZZ(-2):ZZ(2):ZZ(10)
  @test length(r) == 7
  @test length(r) isa BigInt
  @test 2 in r
  @test !(3 in r)
  @test ZZ(2) in r
  @test !(ZZ(3) in r)
  @test r[ZZ(2)] == ZZ(0)
  @test_throws BoundsError r[ZZ(20)]

  r = ZZ(-2):ZZ(-2):ZZ(10)
  @test length(r) == 0
  @test length(r) isa BigInt
  @test !(1 in r)
  @test !(ZZ(1) in r)
  @test_throws BoundsError r[ZZ(2)]

  @test ZZ(-2):2:2 isa StepRange{ZZRingElem}
  @test BigInt(-2):2:ZZ(2) isa StepRange{ZZRingElem}
end

@testset "ZZRingElem.range" begin
  r04 = ZZRingElem(0):ZZRingElem(4)
  r026 = ZZRingElem(0):ZZRingElem(2):ZZRingElem(6)
  zlarge = ZZRingElem(13)^100
  zjump = ZZRingElem(11)^100
  rlarge = -zlarge:zlarge
  rlargejumps = -zlarge:zjump:zlarge

  @test ZZRingElem(0):4 == 0:ZZRingElem(4) == r04
  @test ZZRingElem(0):2:6 == 0:2:ZZRingElem(6) == 0:ZZRingElem(2):6 == r026
  @test typeof(ZZRingElem(0):4) == typeof(0:ZZRingElem(4)) == typeof(r04)
  @test typeof(ZZRingElem(0):ZZRingElem(2):6) == typeof(0:ZZRingElem(2):ZZRingElem(6)) == typeof(0:ZZRingElem(2):6) == typeof(r026)

  @test collect(r04) == ZZRingElem[0, 1, 2, 3, 4]
  @test collect(r026) == ZZRingElem[0, 2, 4, 6]
  @test collect(ZZRingElem(-2):ZZRingElem(5):ZZRingElem(21)) == ZZRingElem[-2, 3, 8, 13, 18]
  @test collect(ZZRingElem(3):ZZRingElem(-2):ZZRingElem(-4)) == ZZRingElem[3, 1, -1, -3]

  @test length(r04) == 5
  @test length(r026) == 4
  @test length(ZZRingElem(-2):ZZRingElem(5):ZZRingElem(21)) == 5
  @test length(ZZRingElem(3):ZZRingElem(-2):ZZRingElem(-4)) == 4

  # Julia assumes `length` to return an Integer, so if we want several base
  # functions to work, that should better be the case.
  @test length(r04) isa Integer
  @test length(r026) isa Integer
  @test length(rlarge) isa Integer
  @test length(rlargejumps) isa Integer

  @test r04[4] == r04[end-1] == 3
  @test r026[3] == r026[end-1] == 4
  @test rlarge[2zlarge] == rlarge[end-1] == rlarge[end-ZZRingElem(1)] == zlarge - 1
  @test rlargejumps[2] == -zlarge + zjump
  @test rlargejumps[end] == zlarge - 2zlarge % zjump

  @test range(ZZRingElem(0); step=2, stop=6) == ZZRingElem(0):ZZRingElem(2):ZZRingElem(6)
  @test range(0; step=ZZRingElem(2), stop=6) == ZZRingElem(0):ZZRingElem(2):ZZRingElem(6)
  @test range(ZZRingElem(0); step=2, length=4) == ZZRingElem(0):ZZRingElem(2):ZZRingElem(6)
  @test range(; stop=ZZRingElem(0), length=3) == ZZRingElem(-2):ZZRingElem(0)

  @test 1 .+ r04 == ZZRingElem(1):ZZRingElem(5)
  @test ZZRingElem(1) .+ r04 == ZZRingElem(1):ZZRingElem(5)
  @test ZZRingElem(1) .+ (0:4) == ZZRingElem(1):ZZRingElem(5)
  @test 2 .* r04 == ZZRingElem(0):ZZRingElem(2):ZZRingElem(8)
  @test ZZRingElem(2) .* r04 == ZZRingElem(0):ZZRingElem(2):ZZRingElem(8)
  @test 2 .* (0:4) == ZZRingElem(0):ZZRingElem(2):ZZRingElem(8)

  @test mod(ZZRingElem(7), r04) == ZZRingElem(2)
  @test mod(7, r04) == 2
  @test mod(7, ZZRingElem(10):ZZRingElem(14)) == 12

  @test all(x -> x in r04, rand(r04, 5))
  @test all(x -> x in ZZRingElem(0):ZZRingElem(10):ZZRingElem(100), rand(ZZRingElem(0):ZZRingElem(10):ZZRingElem(100), 5))
  @test all(x -> x in rlarge, rand(rlarge, 20))
  @test all(x -> x in rlargejumps, rand(rlargejumps, 20))
end

@testset "ZZRingElem.bits" begin
  @test (@inferred collect(bits(ZZ(-4)))) == [true, false, false]
  @test (@inferred collect(bits(ZZ(0)))) == Bool[]
  @test (@inferred collect(bits(ZZ(5)))) == Bool[true, false, true]
  @test (@inferred collect(bits(ZZ(2)^64))) == append!([true], falses(64))
end


@testset "Rings.ZZ.constructors" begin
   @test ZZ(123) isa ZZRingElem
   @test ZZ(2348732479832498732749823) isa ZZRingElem
   @test ZZ(49874359874359874359874387598374587984375897435) isa ZZRingElem
   @test ZZ() isa ZZRingElem
   @test ZZ(0) isa ZZRingElem
   @test ZZ(-123) isa ZZRingElem

   @test zero(ZZ) isa ZZRingElem
   @test zero(ZZ) == 0
   @test one(ZZ) isa ZZRingElem
   @test one(ZZ) == 1

   @test ZZ(typemin(Int)) == typemin(Int)
   @test ZZ(typemin(Int)) < 0
   @test ZZ(typemax(Int)) == typemax(Int)
   @test ZZ(typemax(Int)) > 0
end

@testset "Rings.ZZ.properties" begin
   @test iszero(zero(ZZ))
   @test isone(one(ZZ))
   @test is_unit(one(ZZ))
   @test is_unit(-one(ZZ))
   @test !is_unit(zero(ZZ))
   @test sign(ZZ(2)) == 1
   @test sign(ZZ(0)) == 0
   @test sign(-ZZ(2)) == -1
   @test sign(ZZ(2)) isa ZZRingElem
   @test sign(ZZ(0)) isa ZZRingElem
   @test sign(ZZ(-2)) isa ZZRingElem
   @test abs(ZZ(2)) == 2
   @test abs(ZZ(-2)) == 2
   @test abs(ZZ(0)) == 0
   @test isodd(ZZ(3))
   @test isodd(ZZ(-1))
   @test !isodd(ZZ(2))
   @test !isodd(ZZ(0))
   @test !isodd(ZZ(-2))
   @test iseven(ZZ(2))
   @test iseven(ZZ(0))
   @test iseven(ZZ(-2))
   @test !iseven(ZZ(-1))
   @test is_square(ZZ(0))
   @test is_square(ZZ(1))
   @test !is_square(ZZ(2))
   @test is_square(ZZ(4))
   @test !is_square(ZZ(-1))
   @test !is_square(ZZ(-4))
   @test is_prime(ZZ(2))
   @test !is_prime(ZZ(-2))
   @test !is_prime(ZZ(-1))
   @test !is_prime(ZZ(0))
   @test !is_prime(ZZ(1))
   @test is_probable_prime(ZZ(2))
   @test !is_probable_prime(ZZ(-2))
   @test !is_probable_prime(ZZ(-1))
   @test !is_probable_prime(ZZ(0))
   @test !is_probable_prime(ZZ(1))

   @test parent(ZZ(2)) == ZZ
   @test parent(ZZ()) == ZZ
end

@testset "Rings.ZZ.arithmetic" begin
   @test ZZ(2) + 3 isa ZZRingElem
   @test 3 + ZZ(2) isa ZZRingElem
   @test ZZ(2) - 3 isa ZZRingElem
   @test 3 - ZZ(2) isa ZZRingElem
   @test ZZ(2)*3 isa ZZRingElem
   @test 3*ZZ(2) isa ZZRingElem
   @test 0*ZZ(2) isa ZZRingElem
   @test ZZ(2)*0 isa ZZRingElem

   a = ZZ(2)

   @test a !== a + 0
   @test a !== a + BigInt(0)
   @test a !== a + ZZ(0)
   @test a !== 0 + a
   @test a !== BigInt(0) + a
   @test a !== ZZ(0) + a

   @test a !== a - 0
   @test a !== a - BigInt(0)
   @test a !== a - ZZ(0)

   @test a !== a*1
   @test a !== a*BigInt(1)
   @test a !== a*ZZ(1)
   @test a !== 1*a
   @test a !== BigInt(1)*a
   @test a !== ZZ(1)*a
end

@testset "Rings.ZZ.comparison" begin
   @test ZZ(2) == ZZ(2)
   @test ZZ(0) == ZZ(0)
   @test ZZ(-2) == ZZ(-2)
   @test ZZ(2) == 2
   @test ZZ(0) == 0
   @test ZZ(-2) == -2
   @test 2 == ZZ(2)
   @test 0 == ZZ(0)
   @test -2 == ZZ(-2)

   @test ZZ(3) > ZZ(2)
   @test ZZ(2) >= ZZ(2)
   @test ZZ(2) > ZZ(0)
   @test ZZ(2) < ZZ(3)
   @test ZZ(2) <= ZZ(2)
   @test ZZ(0) < ZZ(2)
   @test ZZ(2) > ZZ(-2)
   @test ZZ(-2) < ZZ(2)

   @test ZZ(3) > 2
   @test ZZ(2) >= 2
   @test ZZ(2) > 0
   @test ZZ(2) < 3
   @test ZZ(2) <= 2
   @test ZZ(0) < 2
   @test ZZ(2) > -2
   @test ZZ(-2) < 2

   @test 3 > ZZ(2)
   @test 2 >= ZZ(2)
   @test 2 > ZZ(0)
   @test 2 < ZZ(3)
   @test 2 <= ZZ(2)
   @test 0 < ZZ(2)
   @test 2 > ZZ(-2)
   @test -2 < ZZ(2)

   @test ZZ(2) < 32783627361723381726834
   @test ZZ(2) < 4243746873264873264873264876327486832468732
   @test ZZ(2) > -32783627361723381726834
   @test ZZ(2) > -4243746873264873264873264876327486832468732
   @test 32783627361723381726834 > ZZ(2)
   @test 4243746873264873264873264876327486832468732 > ZZ(2)
   @test -32783627361723381726834 < ZZ(2)
   @test -4243746873264873264873264876327486832468732 < ZZ(2)

   @test ZZ(32783627361723381726834) == 32783627361723381726834
   @test ZZ(4243746873264873264873264876327486832468732) == 4243746873264873264873264876327486832468732

   @test ZZ(2) != ZZ(3)
   @test ZZ(2) != 3
   @test 2 != ZZ(3)
   @test ZZ(2) != 32783627361723381726834
   @test ZZ(2) != 4243746873264873264873264876327486832468732
   @test 32783627361723381726834 != ZZ(2)
   @test 4243746873264873264873264876327486832468732 != ZZ(2)
end

@testset "Rings.ZZ.divexact" begin
   @test divexact(ZZ(6), ZZ(2)) == 3
   @test divexact(ZZ(6), ZZ(-2)) == -3
   @test divexact(ZZ(-6), ZZ(-2)) == 3
   @test divexact(ZZ(-6), ZZ(2)) == -3

   @test_throws DivideError divexact(ZZ(2), ZZ(0))
   @test_throws DivideError divexact(ZZ(0), ZZ(0))
   @test_throws DivideError divexact(ZZ(2), 0)
   @test_throws DivideError divexact(ZZ(0), 0)

   @test_throws ArgumentError divexact(ZZ(2), ZZ(3))
   @test_throws ArgumentError divexact(ZZ(2), 3)
   @test_throws ArgumentError divexact(2, ZZ(3))

   @test divexact(ZZ(6), 2) isa ZZRingElem
   @test divexact(6, ZZ(2)) isa ZZRingElem
   @test divexact(ZZ(32783627361723381726834), 32783627361723381726834) isa ZZRingElem
   @test divexact(32783627361723381726834, ZZ(32783627361723381726834)) isa ZZRingElem
   @test divexact(ZZ(4243746873264873264873264876327486832468732), 4243746873264873264873264876327486832468732) isa ZZRingElem
   @test divexact(32783627361723381726834, ZZ(32783627361723381726834)) isa ZZRingElem

   @test divexact(ZZ(6), 2) == 3
   @test divexact(6, ZZ(2)) == 3
   @test divexact(ZZ(32783627361723381726834), 32783627361723381726834) == 1
   @test divexact(32783627361723381726834, ZZ(32783627361723381726834)) == 1
   @test divexact(4243746873264873264873264876327486832468732, ZZ(4243746873264873264873264876327486832468732)) == 1

   @test divexact(ZZ(0), ZZ(2)) == 0
   @test divexact(0, ZZ(2)) isa ZZRingElem
   @test divexact(0, ZZ(2)) == 0
end

@testset "Rings.ZZ.powering" begin
   @test ZZ(2)^2 isa ZZRingElem
   @test ZZ(2)^1 isa ZZRingElem
   @test ZZ(2)^0 isa ZZRingElem
   @test ZZ(1)^-1 isa ZZRingElem
   @test ZZ(1)^-2 isa ZZRingElem
   @test ZZ(0)^2 isa ZZRingElem
   @test ZZ(0)^1 isa ZZRingElem
   @test ZZ(0)^0 isa ZZRingElem

   @test ZZ(2)^2 == 4
   @test ZZ(2)^1 == 2
   @test ZZ(2)^0 == 1
   @test ZZ(1)^-1 == 1
   @test ZZ(1)^-2 == 1
   @test ZZ(-1)^-1 == -1
   @test ZZ(-1)^-2 == 1
   @test ZZ(0)^2 == 0
   @test ZZ(0)^1 == 0

   @test ZZ(0)^0 == 1 # for convenience only

   @test_throws DomainError ZZ(0)^-1
   @test_throws DomainError ZZ(0)^-2
   @test_throws DomainError ZZ(-2)^-1
   @test_throws DomainError ZZ(2)^-1
   @test_throws DomainError ZZ(-2)^-2
   @test_throws DomainError ZZ(2)^-1

   @test ZZ(-3)^3 == -27
   @test ZZ(-1)^3 == -1
   @test ZZ(-1)^2 == 1
   @test ZZ(-1)^1 == -1

   @test ZZ(2)^62 isa ZZRingElem
   @test ZZ(2)^63 isa ZZRingElem
   @test ZZ(2)^64 isa ZZRingElem
   @test ZZ(2)^62 == BigInt(2)^62
   @test ZZ(2)^63 == BigInt(2)^63
   @test ZZ(2)^64 == BigInt(2)^64
end

@testset "Rings.ZZ.euclidean_division" begin
   @test mod(ZZ(2), ZZ(3)) == mod(2, 3)
   @test mod(ZZ(2), ZZ(-3)) == mod(2, -3)
   @test mod(ZZ(-2), ZZ(3)) == mod(-2, 3)
   @test mod(ZZ(-2), ZZ(-3)) == mod(-2, -3)

   @test rem(ZZ(2), ZZ(3)) == rem(2, 3)
   @test rem(ZZ(2), ZZ(-3)) == rem(2, -3)
   @test rem(ZZ(-2), ZZ(3)) == rem(-2, 3)
   @test rem(ZZ(-2), ZZ(-3)) == rem(-2, -3)

   @test div(ZZ(2), ZZ(3)) == div(2, 3)
   @test div(ZZ(2), ZZ(-3)) == div(2, -3)
   @test div(ZZ(-2), ZZ(3)) == div(-2, 3)
   @test div(ZZ(-2), ZZ(-3)) == div(-2, -3)

   @test divrem(ZZ(2), ZZ(3)) == divrem(2, 3)
   @test divrem(ZZ(2), ZZ(-3)) == divrem(2, -3)
   @test divrem(ZZ(-2), ZZ(3)) == divrem(-2, 3)
   @test divrem(ZZ(-2), ZZ(-3)) == divrem(-2, -3)

   @test mod(ZZ(2), 3) == mod(2, 3)
   @test mod(ZZ(2), -3) == mod(2, -3)
   @test mod(ZZ(-2), 3) == mod(-2, 3)
   @test mod(ZZ(-2), -3) == mod(-2, -3)

   @test rem(ZZ(2), 3) == rem(2, 3)
   @test rem(ZZ(2), -3) == rem(2, -3)
   @test rem(ZZ(-2), 3) == rem(-2, 3)
   @test rem(ZZ(-2), -3) == rem(-2, -3)

   @test div(ZZ(2), 3) == div(2, 3)
   @test div(ZZ(2), -3) == div(2, -3)
   @test div(ZZ(-2), 3) == div(-2, 3)
   @test div(ZZ(-2), -3) == div(-2, -3)

   @test divrem(ZZ(2), 3) == divrem(2, 3)
   @test divrem(ZZ(2), -3) == divrem(2, -3)
   @test divrem(ZZ(-2), 3) == divrem(-2, 3)
   @test divrem(ZZ(-2), -3) == divrem(-2, -3)

   @test mod(2, ZZ(3)) == mod(2, 3)
   @test mod(2, ZZ(-3)) == mod(2, -3)
   @test mod(-2, ZZ(3)) == mod(-2, 3)
   @test mod(-2, ZZ(-3)) == mod(-2, -3)

   @test rem(2, ZZ(3)) == rem(2, 3)
   @test rem(2, ZZ(-3)) == rem(2, -3)
   @test rem(-2, ZZ(3)) == rem(-2, 3)
   @test rem(-2, ZZ(-3)) == rem(-2, -3)

   @test div(2, ZZ(3)) == div(2, 3)
   @test div(2, ZZ(-3)) == div(2, -3)
   @test div(-2, ZZ(3)) == div(-2, 3)
   @test div(-2, ZZ(-3)) == div(-2, -3)

   @test divrem(2, ZZ(3)) == divrem(2, 3)
   @test divrem(2, ZZ(-3)) == divrem(2, -3)
   @test divrem(-2, ZZ(3)) == divrem(-2, 3)
   @test divrem(-2, ZZ(-3)) == divrem(-2, -3)

   @test mod(2, ZZ(3)) isa ZZRingElem
   @test rem(2, ZZ(3)) isa ZZRingElem
   @test div(2, ZZ(3)) isa ZZRingElem
   @test divrem(2, ZZ(3)) isa Tuple{ZZRingElem, ZZRingElem}

   @test mod(ZZ(2), 3) isa ZZRingElem
   @test rem(ZZ(2), 3) isa ZZRingElem
   @test div(ZZ(2), 3) isa ZZRingElem
   @test divrem(ZZ(2), 3) isa Tuple{ZZRingElem, ZZRingElem}

   @test mod(Int128(2), ZZ(3)) isa ZZRingElem
   @test rem(Int128(2), ZZ(3)) isa ZZRingElem
   @test div(Int128(2), ZZ(3)) isa ZZRingElem
   @test divrem(Int128(2), ZZ(3)) isa Tuple{ZZRingElem, ZZRingElem}

   @test mod(ZZ(2), Int128(3)) isa ZZRingElem
   @test rem(ZZ(2), Int128(3)) isa ZZRingElem
   @test div(ZZ(2), Int128(3)) isa ZZRingElem
   @test divrem(ZZ(2), Int128(3)) isa Tuple{ZZRingElem, ZZRingElem}

   @test mod(BigInt(2), ZZ(3)) isa ZZRingElem
   @test rem(BigInt(2), ZZ(3)) isa ZZRingElem
   @test div(BigInt(2), ZZ(3)) isa ZZRingElem
   @test divrem(BigInt(2), ZZ(3)) isa Tuple{ZZRingElem, ZZRingElem}

   @test mod(ZZ(2), BigInt(3)) isa ZZRingElem
   @test rem(ZZ(2), BigInt(3)) isa ZZRingElem
   @test div(ZZ(2), BigInt(3)) isa ZZRingElem
   @test divrem(ZZ(2), BigInt(3)) isa Tuple{ZZRingElem, ZZRingElem}

   @test_throws DivideError mod(ZZ(2), ZZ(0))
   @test_throws DivideError mod(ZZ(0), ZZ(0))
   @test_throws DivideError mod(ZZ(2), 0)
   @test_throws DivideError mod(ZZ(0), 0)
   @test_throws DivideError mod(2, ZZ(0))
   @test_throws DivideError mod(0, ZZ(0))

   @test_throws DivideError rem(ZZ(2), ZZ(0))
   @test_throws DivideError rem(ZZ(0), ZZ(0))
   @test_throws DivideError rem(ZZ(2), 0)
   @test_throws DivideError rem(ZZ(0), 0)
   @test_throws DivideError rem(2, ZZ(0))
   @test_throws DivideError rem(0, ZZ(0))

   @test_throws DivideError div(ZZ(2), ZZ(0))
   @test_throws DivideError div(ZZ(0), ZZ(0))
   @test_throws DivideError div(ZZ(2), 0)
   @test_throws DivideError div(ZZ(0), 0)
   @test_throws DivideError div(2, ZZ(0))
   @test_throws DivideError div(0, ZZ(0))

   @test_throws DivideError divrem(ZZ(2), ZZ(0))
   @test_throws DivideError divrem(ZZ(0), ZZ(0))
   @test_throws DivideError divrem(ZZ(2), 0)
   @test_throws DivideError divrem(ZZ(0), 0)
   @test_throws DivideError divrem(2, ZZ(0))
   @test_throws DivideError divrem(0, ZZ(0))
end

@testset "Rings.ZZ.conversion" begin
   @test Int(ZZ(123)) == 123
   @test Int(ZZ(0)) == 0
   @test Int(ZZ(-123)) == -123
   @test Int(ZZ(typemin(Int))) == typemin(Int)
   @test Int(ZZ(typemax(Int))) == typemax(Int)

   @test_throws InexactError Int(ZZ(typemin(Int)) - 1)
   @test_throws InexactError Int(ZZ(typemax(Int)) + 1)

   @test fits(Int, ZZ(123))
   @test fits(Int, ZZ(typemin(Int)))
   @test fits(Int, ZZ(typemax(Int)))

   @test Int(ZZ(123)) isa Int
   @test Int(ZZ(0)) isa Int

   @test BigInt(ZZ(123)) == 123
   @test BigInt(ZZ(0)) == 0
   @test BigInt(ZZ(1234498543743857934594389)) == 1234498543743857934594389

   @test BigInt(ZZ(123)) isa BigInt
   @test BigInt(ZZ(0)) isa BigInt
end

@testset "Rings.ZZ.gcd" begin
   @test gcd(ZZ(2), ZZ(3)) == 1
   @test gcd(ZZ(2), ZZ(-3)) == 1
   @test gcd(ZZ(-2), ZZ(3)) == 1
   @test gcd(ZZ(-2), ZZ(-3)) == 1

   @test gcd(ZZ(2), ZZ(0)) == 2
   @test gcd(ZZ(0), ZZ(2)) == 2
   @test gcd(ZZ(0), ZZ(0)) == 0

   @test gcd(ZZ(2), 0) == 2
   @test gcd(0, ZZ(2)) == 2
   @test gcd(ZZ(0), 0) == 0
   @test gcd(0, ZZ(0)) == 0

   @test gcd(ZZ(2), 3) isa ZZRingElem
   @test gcd(2, ZZ(3)) isa ZZRingElem
   @test gcd(ZZ(2), 0) isa ZZRingElem
   @test gcd(0, ZZ(2)) isa ZZRingElem
   @test gcd(ZZ(0), 0) isa ZZRingElem
   @test gcd(0, ZZ(0)) isa ZZRingElem

   @test gcd(ZZ(2), Int128(3)) isa ZZRingElem
   @test gcd(ZZ(2), BigInt(3)) isa ZZRingElem
   @test gcd(Int128(2), ZZ(3)) isa ZZRingElem
   @test gcd(BigInt(2), ZZ(3)) isa ZZRingElem

   @test lcm(ZZ(2), ZZ(3)) == 6
   @test lcm(ZZ(2), ZZ(-3)) == 6
   @test lcm(ZZ(-2), ZZ(3)) == 6
   @test lcm(ZZ(-2), ZZ(-3)) == 6

   @test lcm(ZZ(2), ZZ(0)) == 0
   @test lcm(ZZ(0), ZZ(2)) == 0
   @test lcm(ZZ(0), ZZ(0)) == 0

   @test lcm(ZZ(2), 0) == 0
   @test lcm(0, ZZ(2)) == 0
   @test lcm(ZZ(0), 0) == 0
   @test lcm(0, ZZ(0)) == 0

   @test lcm(ZZ(2), 3) isa ZZRingElem
   @test lcm(2, ZZ(3)) isa ZZRingElem
   @test lcm(ZZ(2), 0) isa ZZRingElem
   @test lcm(0, ZZ(2)) isa ZZRingElem
   @test lcm(ZZ(0), 0) isa ZZRingElem
   @test lcm(0, ZZ(0)) isa ZZRingElem

   for a = -10:10
      for b = -10:10
         g, s, t = gcdx(ZZ(a), ZZ(b))
         gcdx(ZZ(a), b) == g, s, t
         gcdx(a, ZZ(b)) == g, s, t
         @test g == a*s + b*t
         if abs(a) == abs(b)
            @test t == sign(b)
         elseif b == 0 || abs(b) == 2*g
            @test s == sign(a)
         elseif a == 0 || abs(a) == 2*g
            @test t == sign(b)
         else
            @test 2*g*abs(s) < abs(b)
            @test 2*g*abs(t) < abs(a)
         end
      end
   end

   @test gcdx(ZZ(2), ZZ(3)) isa Tuple{ZZRingElem, ZZRingElem, ZZRingElem}
   @test gcdx(ZZ(2), ZZ(0)) isa Tuple{ZZRingElem, ZZRingElem, ZZRingElem}
   @test gcdx(ZZ(0), ZZ(3)) isa Tuple{ZZRingElem, ZZRingElem, ZZRingElem}
   @test gcdx(ZZ(0), ZZ(0)) isa Tuple{ZZRingElem, ZZRingElem, ZZRingElem}
   @test gcdx(ZZ(2), ZZ(1)) isa Tuple{ZZRingElem, ZZRingElem, ZZRingElem}
   @test gcdx(ZZ(1), ZZ(3)) isa Tuple{ZZRingElem, ZZRingElem, ZZRingElem}
   @test gcdx(ZZ(-1), ZZ(-1)) isa Tuple{ZZRingElem, ZZRingElem, ZZRingElem}

   @test gcdx(ZZ(2), 3) isa Tuple{ZZRingElem, ZZRingElem, ZZRingElem}
   @test gcdx(ZZ(2), 0) isa Tuple{ZZRingElem, ZZRingElem, ZZRingElem}
   @test gcdx(ZZ(0), 3) isa Tuple{ZZRingElem, ZZRingElem, ZZRingElem}
   @test gcdx(ZZ(0), 0) isa Tuple{ZZRingElem, ZZRingElem, ZZRingElem}
   @test gcdx(ZZ(2), 1) isa Tuple{ZZRingElem, ZZRingElem, ZZRingElem}
   @test gcdx(ZZ(1), 3) isa Tuple{ZZRingElem, ZZRingElem, ZZRingElem}
   @test gcdx(ZZ(-1), -1) isa Tuple{ZZRingElem, ZZRingElem, ZZRingElem}

   @test gcdx(2, ZZ(3)) isa Tuple{ZZRingElem, ZZRingElem, ZZRingElem}
   @test gcdx(2, ZZ(0)) isa Tuple{ZZRingElem, ZZRingElem, ZZRingElem}
   @test gcdx(0, ZZ(3)) isa Tuple{ZZRingElem, ZZRingElem, ZZRingElem}
   @test gcdx(0, ZZ(0)) isa Tuple{ZZRingElem, ZZRingElem, ZZRingElem}
   @test gcdx(2, ZZ(1)) isa Tuple{ZZRingElem, ZZRingElem, ZZRingElem}
   @test gcdx(1, ZZ(3)) isa Tuple{ZZRingElem, ZZRingElem, ZZRingElem}
   @test gcdx(-1, ZZ(-1)) isa Tuple{ZZRingElem, ZZRingElem, ZZRingElem}
end

@testset "Rings.ZZ.roots" begin
   @test isqrt(ZZ(16)) == 4
   @test isqrt(ZZ(5)) == 2
   @test isqrt(ZZ(0)) == 0
   @test isqrt(ZZ(1)) == 1

   @test_throws DomainError isqrt(ZZ(-1))
   @test_throws DomainError isqrt(ZZ(-4))

   @test isqrtrem(ZZ(16)) == (4, 0)
   @test isqrtrem(ZZ(5)) == (2, 1)
   @test isqrtrem(ZZ(0)) == (0, 0)
   @test isqrtrem(ZZ(1)) == (1, 0)

   @test_throws DomainError isqrtrem(ZZ(-1))
   @test_throws DomainError isqrtrem(ZZ(-4))

   @test root(ZZ(16), 2) == 4
   @test root(ZZ(-8), 3) == -2
   @test root(ZZ(0), 2) == 0
   @test root(ZZ(0), 3) == 0
   @test root(ZZ(1), 1) == 1
   @test root(ZZ(-1), 1) == -1
   @test root(ZZ(1), 2) == 1

   @test_throws DomainError root(ZZ(2), -3)
   @test_throws DomainError root(ZZ(2), 0)
   @test_throws DomainError root(ZZ(1), -1)
   @test_throws DomainError root(ZZ(1), 0)
   @test_throws DomainError root(ZZ(0), 0)
   @test_throws DomainError root(ZZ(-2), 2)
   @test_throws DomainError root(ZZ(-2), 0)
end

@testset "Rings.ZZ.factorisation" begin
   F = factor(ZZ(-60))

   @test unit(F) == -1
   @test unit(F) isa ZZRingElem

   @test length(collect(F)) == 3
   @test 2 in F
   @test ZZ(3) in F
   @test F[2] == 2
   @test F[ZZ(3)] == 1
   @test !(ZZ(-1) in F)
   @test !(-1 in F)
   @test !(ZZ(0) in F)
   @test !(0 in F)
   @test !(7 in F)

   F = factor(ZZ(60))

   @test unit(F) == 1

   @test_throws ArgumentError factor(ZZ(0))
   @test_throws ErrorException F[0]
   @test_throws ErrorException F[7]
   @test_throws ErrorException F[ZZ(0)]
   @test_throws ErrorException F[ZZ(7)]
end

@testset "Rings.ZZ.combinatorial" begin
   @test factorial(ZZ(0)) == 1
   @test factorial(ZZ(1)) == 1
   @test factorial(ZZ(3)) == 6

   @test factorial(ZZ(0)) isa ZZRingElem
   @test factorial(ZZ(3)) isa ZZRingElem

   @test_throws DomainError factorial(ZZ(-1))

   @test rising_factorial(3, 2) == 12
   @test rising_factorial(3, 0) == 1
   @test rising_factorial(0, 3) == 0
   @test rising_factorial(-3, 3) == -6

   @test rising_factorial(2, 3) isa Int
   @test rising_factorial(2, 0) isa Int

   @test_throws DomainError rising_factorial(0, -1)
   @test_throws DomainError rising_factorial(-3, -5)

   @test rising_factorial(ZZ(3), 2) == 12
   @test rising_factorial(ZZ(3), 0) == 1
   @test rising_factorial(ZZ(0), 3) == 0
   @test rising_factorial(ZZ(-3), 3) == -6

   @test rising_factorial(ZZ(2), 3) isa ZZRingElem
   @test rising_factorial(ZZ(2), 0) isa ZZRingElem

   @test_throws DomainError rising_factorial(ZZ(0), -1)
   @test_throws DomainError rising_factorial(ZZ(-3), -5)

   @test rising_factorial(ZZ(3), ZZ(2)) == 12
   @test rising_factorial(ZZ(3), ZZ(0)) == 1
   @test rising_factorial(ZZ(0), ZZ(3)) == 0
   @test rising_factorial(ZZ(-3), ZZ(3)) == -6

   @test rising_factorial(ZZ(2), ZZ(3)) isa ZZRingElem
   @test rising_factorial(ZZ(2), ZZ(0)) isa ZZRingElem

   @test_throws DomainError rising_factorial(ZZ(0), ZZ(-1))
   @test_throws DomainError rising_factorial(ZZ(-3), ZZ(-5))

   @test primorial(6) == 30
   @test primorial(5) == 30
   @test primorial(2) == 2
   @test primorial(1) == 1
   @test primorial(0) == 1

   @test primorial(0) isa Int
   @test primorial(1) isa Int
   @test primorial(2) isa Int

   @test_throws DomainError primorial(-1)

   @test primorial(ZZ(6)) == 30
   @test primorial(ZZ(5)) == 30
   @test primorial(ZZ(2)) == 2
   @test primorial(ZZ(1)) == 1
   @test primorial(ZZ(0)) == 1

   @test primorial(ZZ(0)) isa ZZRingElem
   @test primorial(ZZ(1)) isa ZZRingElem
   @test primorial(ZZ(2)) isa ZZRingElem

   @test_throws DomainError primorial(ZZ(-1))

   @test bell(0) == 1
   @test bell(1) == 1
   @test bell(3) == 5

   @test bell(0) isa Int
   @test bell(1) isa Int
   @test bell(3) isa Int

   @test_throws DomainError bell(-1)

   @test bell(ZZ(0)) == 1
   @test bell(ZZ(1)) == 1
   @test bell(ZZ(3)) == 5

   @test bell(ZZ(0)) isa ZZRingElem
   @test bell(ZZ(1)) isa ZZRingElem
   @test bell(ZZ(3)) isa ZZRingElem

   @test_throws DomainError bell(ZZ(-1))

   @test binomial(ZZ(3), ZZ(2)) == 3
   @test binomial(ZZ(3), ZZ(0)) == 1
   @test binomial(ZZ(3), ZZ(3)) == 1
   @test binomial(ZZ(0), ZZ(0)) == 1
   @test binomial(ZZ(1), ZZ(0)) == 1
   @test binomial(ZZ(1), ZZ(1)) == 1

   @test binomial(ZZ(2), ZZ(-3)) == 0
   @test binomial(ZZ(0), ZZ(-3)) == 0
   @test binomial(ZZ(-1), ZZ(-1)) == 0
   @test binomial(ZZ(3), ZZ(5)) == 0

   @test binomial(ZZ(3), ZZ(2)) isa ZZRingElem
   @test binomial(ZZ(0), ZZ(0)) isa ZZRingElem
   @test binomial(ZZ(-3), ZZ(2)) isa ZZRingElem
   @test binomial(ZZ(2), ZZ(-3)) isa ZZRingElem
   @test binomial(ZZ(3), ZZ(5)) isa ZZRingElem

   @test number_of_partitions(0) == 1
   @test number_of_partitions(ZZ(0)) == 1
   @test number_of_partitions(3) == 3
   @test number_of_partitions(ZZ(3)) == 3
   @test number_of_partitions(-1) == 0
   @test number_of_partitions(ZZ(-1)) == 0
end

@testset "Rings.ZZ.number_theoretical" begin
   @test fibonacci(1) == 1
   @test fibonacci(2) == 1
   @test fibonacci(3) == 2
   @test fibonacci(0) == 0
   @test fibonacci(-1) == 1
   @test fibonacci(-2) == -1

   @test fibonacci(1) isa Int
   @test fibonacci(2) isa Int
   @test fibonacci(3) isa Int
   @test fibonacci(0) isa Int
   @test fibonacci(-1) isa Int

   @test fibonacci(ZZ(1)) == 1
   @test fibonacci(ZZ(2)) == 1
   @test fibonacci(ZZ(3)) == 2
   @test fibonacci(ZZ(0)) == 0
   @test fibonacci(ZZ(-1)) == 1
   @test fibonacci(ZZ(-2)) == -1

   @test fibonacci(ZZ(1)) isa ZZRingElem
   @test fibonacci(ZZ(2)) isa ZZRingElem
   @test fibonacci(ZZ(3)) isa ZZRingElem
   @test fibonacci(ZZ(0)) isa ZZRingElem
   @test fibonacci(ZZ(-1)) isa ZZRingElem

   @test moebius_mu(1) == 1
   @test moebius_mu(2) == -1
   @test moebius_mu(6) == 1

   @test moebius_mu(ZZ(1)) == 1
   @test moebius_mu(ZZ(2)) == -1
   @test moebius_mu(ZZ(6)) == 1

   @test moebius_mu(1) isa Int
   @test moebius_mu(2) isa Int

   @test moebius_mu(ZZ(1)) isa Int
   @test moebius_mu(ZZ(2)) isa Int

   @test_throws DomainError moebius_mu(0)
   @test_throws DomainError moebius_mu(ZZ(0))
   @test_throws DomainError moebius_mu(-1)
   @test_throws DomainError moebius_mu(ZZ(-1))

   @test jacobi_symbol(2, 3) == -1
   @test jacobi_symbol(1, 3) == 1
   @test jacobi_symbol(0, 3) == 0
   @test jacobi_symbol(-1, 3) == -1
   @test jacobi_symbol(14, 15) == -1
   @test jacobi_symbol(1, 15) == 1
   @test jacobi_symbol(5, 15) == 0
   @test jacobi_symbol(2, 45) == -1

   @test jacobi_symbol(ZZ(2), ZZ(3)) == -1
   @test jacobi_symbol(ZZ(1), ZZ(3)) == 1
   @test jacobi_symbol(ZZ(0), ZZ(3)) == 0
   @test jacobi_symbol(ZZ(-1), ZZ(3)) == -1
   @test jacobi_symbol(ZZ(14), ZZ(15)) == -1
   @test jacobi_symbol(ZZ(1), ZZ(15)) == 1
   @test jacobi_symbol(ZZ(5), ZZ(15)) == 0
   @test jacobi_symbol(ZZ(2), ZZ(45)) == -1

   @test jacobi_symbol(2, 3) isa Int
   @test jacobi_symbol(1, 3) isa Int
   @test jacobi_symbol(0, 3) isa Int
   @test jacobi_symbol(-1, 3) isa Int

   @test jacobi_symbol(ZZ(2), ZZ(3)) isa Int
   @test jacobi_symbol(ZZ(1), ZZ(3)) isa Int
   @test jacobi_symbol(ZZ(0), ZZ(3)) isa Int
   @test jacobi_symbol(ZZ(-1), ZZ(3)) isa Int

   @test_throws DomainError jacobi_symbol(2, 0)
   @test_throws DomainError jacobi_symbol(ZZ(2), ZZ(0))
   @test_throws DomainError jacobi_symbol(2, 2)
   @test_throws DomainError jacobi_symbol(ZZ(2), ZZ(2))
   @test_throws DomainError jacobi_symbol(2, -1)
   @test_throws DomainError jacobi_symbol(ZZ(2), ZZ(-1))

   @test divisor_sigma(6, 0) == 4
   @test divisor_sigma(6, 1) == 12
   @test divisor_sigma(6, 2) == 50

   @test divisor_sigma(ZZ(6), 0) == 4
   @test divisor_sigma(ZZ(6), 1) == 12
   @test divisor_sigma(ZZ(6), 2) == 50

   @test divisor_sigma(ZZ(6), ZZ(0)) == 4
   @test divisor_sigma(ZZ(6), ZZ(1)) == 12
   @test divisor_sigma(ZZ(6), ZZ(2)) == 50

   @test divisor_sigma(6, 0) isa Int
   @test divisor_sigma(6, 1) isa Int
   @test divisor_sigma(6, 2) isa Int

   @test divisor_sigma(ZZ(6), 0) isa ZZRingElem
   @test divisor_sigma(ZZ(6), 1) isa ZZRingElem
   @test divisor_sigma(ZZ(6), 2) isa ZZRingElem

   @test divisor_sigma(ZZ(6), ZZ(0)) isa ZZRingElem
   @test divisor_sigma(ZZ(6), ZZ(1)) isa ZZRingElem
   @test divisor_sigma(ZZ(6), ZZ(2)) isa ZZRingElem

   @test_throws DomainError divisor_sigma(0, 1)
   @test_throws DomainError divisor_sigma(ZZ(0), 1)
   @test_throws DomainError divisor_sigma(-1, 1)
   @test_throws DomainError divisor_sigma(ZZ(-1), 1)
   @test_throws DomainError divisor_sigma(6, -1)
   @test_throws DomainError divisor_sigma(ZZ(6), -1)
   @test_throws DomainError divisor_sigma(ZZ(6), ZZ(-1))

   @test euler_phi(1) == 1
   @test euler_phi(2) == 1

   @test euler_phi(ZZ(1)) == 1
   @test euler_phi(ZZ(2)) == 1

   @test euler_phi(1) isa Int
   @test euler_phi(2) isa Int

   @test euler_phi(ZZ(1)) isa ZZRingElem
   @test euler_phi(ZZ(2)) isa ZZRingElem

   @test_throws DomainError euler_phi(0)
   @test_throws DomainError euler_phi(ZZ(0))
   @test_throws DomainError euler_phi(-1)
   @test_throws DomainError euler_phi(ZZ(-1))
end

@testset "ZZOneTo" begin
  n = ZZ(5)

  r = Base.OneTo(n)

  @test r isa ZZOneTo
  @test first(r) == ZZ(1)
  @test last(r) == n
  @test length(r) == 5
  @test collect(r) == ZZRingElem[1, 2, 3, 4, 5]
  @test ZZ(3) in r
  @test !(ZZ(0) in r)
  @test sum(r) == sum(1:5)

  m = ZZ(20)
  r2 = Base.OneTo(m)

  @test r2 isa ZZOneTo
  @test first(r2) == ZZ(1)
  @test last(r2) == m
  @test length(r2) == 20
end
