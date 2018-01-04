function test_fmpz_abstract_types()
   print("fmpz.abstract_types...")

   @test fmpz <: RingElem

   @test FlintIntegerRing <: Nemo.Ring

   @test elem_type(FlintIntegerRing()) == fmpz
   @test elem_type(FlintIntegerRing) == fmpz
   @test parent_type(fmpz) == FlintIntegerRing

   println("PASS")
end

function test_fmpz_constructors()
   print("fmpz.constructors...")

   a = fmpz(-123)
   @test isa(a, RingElem)

   b = fmpz(12.0)
   @test isa(b, RingElem)

   c = fmpz("-1234567876545678376545678900000000000000000000000000")
   @test isa(c, RingElem)

   d = fmpz(c)
   @test isa(d, RingElem)

   e = deepcopy(c)
   @test isa(e, RingElem)

   f = fmpz(BigFloat(10)^100)
   @test isa(f, RingElem)

   g = fmpz()
   @test isa(f, RingElem)

   println("PASS")
end

function test_fmpz_printing()
   print("fmpz.printing...")

   a = fmpz(-123)

   @test string(a) == "-123"

   println("PASS")
end

function test_fmpz_convert()
   print("fmpz.convert...")

   a = fmpz(-123)
   b = fmpz(12)

   @test Int(a) == -123
   @test UInt(b) == UInt(12)
   @test BigInt(a) == BigInt(-123)
   @test Float64(a) == Float64(-123)
   @test Float32(a) == Float32(-123)
   @test Float16(a) == Float16(-123)
   @test BigFloat(a) == BigFloat(-123)

   println("PASS")
end

function test_fmpz_manipulation()
   print("fmpz.manipulation...")

   a = one(FlintIntegerRing())
   b = zero(FlintIntegerRing())

   @test isa(a, RingElem)

   @test isa(b, RingElem)

   @test sign(a) == 1

   @test fits(Int, a)

   @test fits(UInt, a)

   @test size(a) == 1

   @test canonical_unit(fmpz(-12)) == -1

   @test isunit(fmpz(-1))

   @test iszero(b)

   @test isone(a)

   @test numerator(fmpz(12)) == fmpz(12)

   @test denominator(fmpz(12)) == fmpz(1)

   println("PASS")
end

function test_fmpz_binary_ops()
   print("fmpz.binary_ops...")

   a = fmpz(12)
   b = fmpz(26)

   @test a + b == 38

   @test a - b == -14

   @test a*b == 312

   @test b%a == 2

   @test b&a == 8

   @test b|a == 30

   @test b$a == 22

   println("PASS")
end

function test_fmpz_division()
   print("fmpz.division...")

   a = fmpz(12)
   b = fmpz(26)

   @test fdiv(b, a) == 2

   @test cdiv(b, a) == 3

   @test tdiv(b, a) == 2

   @test div(b, a) == 2

   println("PASS")
end

function test_fmpz_remainder()
   print("fmpz.remainder...")

   a = fmpz(12)
   b = fmpz(26)

   @test mod(b, a) == 2

   @test rem(b, a) == 2

   @test mod(b, 12) == 2

   @test rem(b, 12) == 2

   println("PASS")
end

function test_fmpz_exact_division()
   print("fmpz.exact_division...")

   @test divexact(fmpz(24), fmpz(12)) == 2

   println("PASS")
end

function test_fmpz_gcd_lcm()
   print("fmpz.gcd_lcm...")

   a = fmpz(12)
   b = fmpz(26)

   @test gcd(a, b) == 2

   @test_throws ErrorException gcd(fmpz[])

   @test gcd(fmpz[8]) == 8

   @test gcd([fmpz(10), fmpz(2)]) == 2

   @test gcd([fmpz(1), fmpz(2), fmpz(3)]) == 1

   @test gcd([fmpz(9), fmpz(27), fmpz(3)]) == 3

   @test lcm(a, b) == 156

   @test_throws ErrorException lcm(fmpz[])

   @test lcm(fmpz[2]) == 2

   @test lcm(fmpz[2, 3]) == 6

   @test lcm(fmpz[2, 2, 2]) == 2

   @test lcm(fmpz[2, 3, 2]) == 6
   println("PASS")
end

function test_fmpz_logarithm()
   print("fmpz.logarithm...")

   a = fmpz(12)
   b = fmpz(26)

   @test flog(b, a) == 1

   @test flog(b, 12) == 1

   @test clog(b, a) == 2

   @test clog(b, 12) == 2

   println("PASS")
end

function test_fmpz_adhoc_binary()
   print("fmpz.adhoc_binary...")

   a = fmpz(-12)

   @test 3 + a == -9

   @test a + 3 == -9

   @test a - 3 == -15

   @test 5 - a == 17

   @test a*5 == -60

   @test 5*a == -60

   @test a%5 == -2

   println("PASS")
end

function test_fmpz_adhoc_division()
   print("fmpz.adhoc_division...")

   a = fmpz(-12)

   @test fdiv(a, 5) == -3

   @test tdiv(a, 7) == -1

   @test cdiv(a, 7) == -1

   @test div(a, 3) == -4

   println("PASS")
end

function test_fmpz_shift()
   print("fmpz.shift..")

   a = fmpz(-12)

   @test a >> 3 == -2

   @test fdivpow2(a, 2) == -3

   @test cdivpow2(a, 2) == -3

   @test tdivpow2(a, 2) == -3

   @test a << 4 == -192

   println("PASS")
end

function test_fmpz_powering()
   print("fmpz.powering...")

   a = fmpz(-12)

   @test a^5 == -248832

   @test a^UInt(5) == -248832

   println("PASS")
end

function test_fmpz_comparison()
   print("fmpz.comparison...")

   a = fmpz(-12)
   b = fmpz(5)

   @test a < b

   @test b > a

   @test b >= a

   @test a <= b

   @test a == fmpz(-12)

   @test a != b

   @test isequal(a, fmpz(-12))

   @test cmpabs(a, b) == 1

   @test cmp(a, b) == -1

   println("PASS")
end

function test_fmpz_adhoc_comparison()
   print("fmpz.adhoc_comparison...")

   a = fmpz(-12)

   @test a < 7

   @test a > -40

   @test 7 > a

   @test -40 < a

   @test a <= 7

   @test a >= -40

   @test 7 >= a

   @test -40 <= a

   @test a == -12

   @test a != 4

   @test -12 == a

   @test 4 != a

   a = fmpz(2)

   @test a < UInt(7)

   @test a > UInt(1)

   @test UInt(7) > a

   @test UInt(1) < a

   @test a <= UInt(7)

   @test a >= UInt(2)

   @test UInt(7) >= a

   @test UInt(1) <= a

   @test a == UInt(2)

   @test a != UInt(4)

   @test UInt(2) == a

   @test UInt(4) != a

   println("PASS")
end

function test_fmpz_unary_ops()
   print("fmpz.unary_ops...")

   @test -fmpz(12) == -12

   @test ~fmpz(-5) == 4

   println("PASS")
end

function test_fmpz_abs()
   print("fmpz.abs...")

   @test abs(fmpz(-12)) == 12

   println("PASS")
end

function test_fmpz_divrem()
   print("fmpz.divrem...")

   @test fdivrem(fmpz(12), fmpz(5)) == (fmpz(2), fmpz(2))

   @test tdivrem(fmpz(12), fmpz(5)) == (fmpz(2), fmpz(2))

   @test divrem(fmpz(12), fmpz(5)) == (fmpz(2), fmpz(2))

   println("PASS")
end

function test_fmpz_roots()
   print("fmpz.roots...")

   @test isqrt(fmpz(12)) == 3

   @test isqrtrem(fmpz(12)) == (3, 3)

   @test root(fmpz(1000), 3) == 10

   println("PASS")
end

function test_fmpz_extended_gcd()
   print("fmpz.extended_gcd...")

   @test gcdx(fmpz(12), fmpz(5)) == (1, -2, 5)

   @test gcdinv(fmpz(5), fmpz(12)) == (1, 5)

   println("PASS")
end

function test_fmpz_bit_twiddling()
   print("fmpz.bit_twiddling...")

   a = fmpz(12)

   @test popcount(a) == 2

   @test nextpow2(a) == 16

   @test prevpow2(a) == 8

   @test trailing_zeros(a) == 2

   combit!(a, 2)

   @test a == 8

   setbit!(a, 0)

   @test a == 9

   clrbit!(a, 0)

   @test a == 8

   println("PASS")
end

function test_fmpz_bases()
   print("fmpz.bases...")

   a = fmpz(12)

   @test bin(a) == "1100"

   @test oct(a) == "14"

   @test dec(a) == "12"

   @test hex(a) == "c"

   @test base(a, 13) == "c"

   @test nbits(a) == 4

   @test ndigits(a, 3) == 3

   println("PASS")
end

function test_fmpz_string_io()
   print("fmpz.string_io...")

   a = fmpz(12)

   @test string(a) == "12"

   println("PASS")
end

function test_fmpz_modular_arithmetic()
   print("fmpz.modular_arithmetic...")

   @test powmod(fmpz(12), fmpz(110), fmpz(13)) == 1

   @test powmod(fmpz(12), 110, fmpz(13)) == 1

   @test invmod(fmpz(12), fmpz(13)) == 12

   @test sqrtmod(fmpz(12), fmpz(13)) == 5

   @test crt(fmpz(5), fmpz(13), fmpz(7), fmpz(37), true) == 44

   @test crt(fmpz(5), fmpz(13), 7, 37, false) == 44

   println("PASS")
end

function test_fmpz_factor()
   print("fmpz.factor...")

   a = fmpz(fmpz(-3*5*7*11*13^10))

   fac = factor(a)

   b = unit(fac)

   for (p, e) in fac
      b = b*p^e
   end

   @test b == a

   @test fac[fmpz(3)] == 1
   @test fac[fmpz(5)] == 1
   @test fac[fmpz(7)] == 1
   @test fac[fmpz(11)] == 1
   @test fac[fmpz(13)] == 10
   @test 3 in fac
   @test !(2 in fac)

   fac = factor(fmpz(-1))

   @test fac.fac == Dict{fmpz, Int}()

   fac = factor(fmpz(-2))

   @test fac.fac == Dict(fmpz(2) => 1)
   @test unit(fac) == -1

   println("PASS")
end

function test_fmpz_number_theoretic()
   print("fmpz.number_theoretic...")

   @test isprime(fmpz(13))

   @test isprobabprime(fmpz(13))

   @test divisible(fmpz(12), fmpz(6))

   @test issquare(fmpz(36))

   @test fac(100) == fmpz("93326215443944152681699238856266700490715968264381621468592963895217599993229915608941463976156518286253697920827223758251185210916864000000000000000000000000")

   @test sigma(fmpz(128), 10) == fmpz("1181745669222511412225")

   @test eulerphi(fmpz(12480)) == 3072

   @test remove(fmpz(12), fmpz(2)) == (2, 3)

   @test valuation(fmpz(12), fmpz(2)) == 2

   @test valuation(fmpz(12), 2) == 2

   @test valuation(12, 2) == 2

   @test divisor_lenstra(fmpz(12), fmpz(4), fmpz(5)) == 4

   @test risingfac(fmpz(12), 5) == 524160

   @test risingfac(12, 5) == 524160

   @test primorial(7) == 210

   @test binom(12, 5) == 792

   @test bell(12) == 4213597

   @test moebiusmu(fmpz(13)) == -1

   @test jacobi(fmpz(2), fmpz(5)) == -1

   if !is_windows64()

      @test numpart(10) == 42

      @test numpart(fmpz(1000)) == fmpz("24061467864032622473692149727991")

   end

   println("PASS")
end

function test_fmpz_square_root()
   print("fmpz.square_root...")

   @test sqrt(fmpz(4)) == 2

   @test sqrt(fmpz(0)) == 0

   println("PASS")
end

function test_fmpz()
   test_fmpz_abstract_types()
   test_fmpz_constructors()
   test_fmpz_printing()
   test_fmpz_convert()
   test_fmpz_manipulation()
   test_fmpz_binary_ops()
   test_fmpz_division()
   test_fmpz_remainder()
   test_fmpz_exact_division()
   test_fmpz_gcd_lcm()
   test_fmpz_logarithm()
   test_fmpz_adhoc_binary()
   test_fmpz_adhoc_division()
   test_fmpz_shift()
   test_fmpz_powering()
   test_fmpz_comparison()
   test_fmpz_adhoc_comparison()
   test_fmpz_unary_ops()
   test_fmpz_abs()
   test_fmpz_divrem()
   test_fmpz_roots()
   test_fmpz_extended_gcd()
   test_fmpz_bit_twiddling()
   test_fmpz_bases()
   test_fmpz_string_io()
   test_fmpz_modular_arithmetic()
   test_fmpz_factor()
   test_fmpz_number_theoretic()
   test_fmpz_square_root()

   println("")
end
