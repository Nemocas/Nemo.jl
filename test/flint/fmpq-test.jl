function test_fmpq_constructors()
   print("fmpq.constructors...")

   R = FractionField(ZZ)

   @test elem_type(R) == fmpq
   @test elem_type(FlintRationalField) == fmpq
   @test parent_type(fmpq) == FlintRationalField

   @test isa(R, FlintRationalField)

   @test isa(R(2), fmpq)

   @test isa(R(), fmpq)

   @test isa(R(BigInt(1)//2), fmpq)

   @test isa(R(2, 3), fmpq)

   @test isa(R(fmpz(2), 3), fmpq)

   @test isa(R(2, fmpz(3)), fmpq)

   @test isa(R(fmpz(2), fmpz(3)), fmpq)

   @test isa(R(R(2)), fmpq)

   @test isa(fmpq(2), fmpq)

   @test isa(fmpq(), fmpq)

   @test isa(fmpq(BigInt(1)//2), fmpq)

   @test isa(fmpq(2, 3), fmpq)

   @test isa(fmpq(fmpz(2), 3), fmpq)

   @test isa(fmpq(2, fmpz(3)), fmpq)

   @test isa(fmpq(fmpz(2), fmpz(3)), fmpq)

   @test isa(fmpq(R(2)), fmpq)

   println("PASS")
end

function test_fmpq_printing()
   print("fmpq.constructors...")

   a = FlintQQ(1, 2)

   @test string(a) == "1//2"

   println("PASS")
end

function test_fmpq_conversions()
   print("fmpq.conversions...")

   @test Rational(fmpz(12)) == 12

   @test Rational(fmpq(3, 7)) == 3//7

   println("PASS")
end

function test_fmpq_manipulation()
   print("fmpq.manipulation...")

   R = FractionField(ZZ)

   a = -fmpz(2)//3
   b = fmpz(123)//234

   @test height(a) == 3

   @test height_bits(b) == 7

   @test abs(a) == fmpz(2)//3

   @test isone(one(R))

   @test iszero(zero(R))

   @test isunit(one(R))

   @test isunit(fmpq(1, 3))

   @test deepcopy(fmpq(2, 3)) == fmpq(2, 3)

   @test numerator(fmpq(2, 3)) == 2

   @test denominator(fmpq(2, 3)) == 3

   println("PASS")
end

function test_fmpq_unary_ops()
   print("fmpq.unary_ops...")

   a = fmpq(-2, 3)

   @test -a == fmpq(2, 3)

   println("PASS")
end

function test_fmpq_binary_ops()
   print("fmpq.binary_ops...")

   a = fmpq(-2, 3)
   b = fmpz(5)//7

   @test a + b == fmpq(1, 21)

   @test a - b == fmpq(-29, 21)

   @test a*b == fmpq(-10, 21)

   println("PASS")
end

function test_fmpq_adhoc_binary()
   print("fmpq.adhoc_binary...")

   a = fmpq(-2, 3)

   @test a + 3 == fmpq(7, 3)

   @test 3 + a == fmpq(7, 3)

   @test a - 3 == fmpq(-11, 3)

   @test 3 - a == fmpq(11, 3)

   @test a*3 == -2

   @test 3a == -2

   @test a + fmpz(3) == fmpq(7, 3)

   @test fmpz(3) + a == fmpq(7, 3)

   @test a - fmpz(3) == fmpq(-11, 3)

   @test fmpz(3) - a == fmpq(11, 3)

   @test a*fmpz(3) == -2

   @test fmpz(3)*a == -2

   @test fmpq(1, 2) + 1//2 == 1

   @test 1//2 + fmpq(1, 2) == 1

   @test BigInt(1)//BigInt(2) + fmpq(1, 2) == 1

   @test fmpq(1, 2) + BigInt(1)//BigInt(2) == 1

   @test fmpq(1, 2) - 1//2 == 0

   @test 1//2 - fmpq(1, 2) == 0

   @test BigInt(1)//BigInt(2) - fmpq(1, 2) == 0

   @test fmpq(1, 2) - BigInt(1)//BigInt(2) == 0

   @test fmpq(1, 2) * 1//2 == 1//4

   @test 1//2 * fmpq(1, 2) == 1//4

   @test BigInt(1)//BigInt(2) * fmpq(1, 2) == 1//4

   @test fmpq(1, 2) * BigInt(1)//BigInt(2) == 1//4

   @test fmpq(1, 2) // (BigInt(1)//BigInt(2)) == 1

   @test fmpq(1, 2) // (1//2) == 1

   println("PASS")
end

function test_fmpq_comparison()
   print("fmpq.comparison...")

   a = fmpq(-2, 3)
   b = fmpz(1)//2

   @test a < b

   @test b > a

   @test b >= a

   @test a <= b

   @test a == fmpz(-4)//6

   @test a != b

   println("PASS")
end

function test_fmpq_adhoc_comparison()
   print("fmpq.adhoc_comparison...")

   a = -fmpz(2)//3

   @test a < 1

   @test 1 > a

   @test a < fmpz(1)

   @test fmpz(1) > a

   @test a < 1//1

   @test 1//1 > a

   @test a < BigInt(1)//BigInt(1)

   @test BigInt(1)//BigInt(1) > a

   @test a <= 0

   @test 0 >= a

   @test a <= fmpz(0)

   @test fmpz(0) >= a

   @test a <= 0//1

   @test 0//1 >= a

   @test a <= BigInt(0)//BigInt(1)

   @test BigInt(0)//BigInt(1) >= a

   @test a != 1

   @test a != fmpz(1)

   @test 1 != a

   @test fmpz(1) != a

   @test a != 1//1

   @test a != BigInt(1)//1

   @test a == fmpq(-2, 3)

   @test fmpq(1, 2) == 1//2

   @test 1//2 == fmpq(1, 2)

   @test fmpq(1, 2) == BigInt(1)//BigInt(2)

   @test BigInt(1)//BigInt(2) == fmpq(1, 2)

   println("PASS")
end

function test_fmpq_shifting()
   print("fmpq.shifting...")

   a = -fmpz(2)//3
   b = fmpq(1, 2)

   @test a << 3 == -fmpz(16)//3

   @test b >> 5 == fmpz(1)//64

   println("PASS")
end

function test_fmpq_powering()
   print("fmpq.powering...")

   a = -fmpz(2)//3

   @test a^(-12) == fmpz(531441)//4096

   println("PASS")
end

function test_fmpq_inversion()
   print("fmpq.inversion...")

   a = -fmpz(2)//3

   @test inv(a) == fmpz(-3)//2

   println("PASS")
end

function test_fmpq_exact_division()
   print("fmpq.exact_division...")

   a = -fmpz(2)//3
   b = fmpz(1)//2

   @test divexact(a, b) == fmpz(-4)//3

   println("PASS")
end

function test_fmpq_adhoc_exact_division()
   print("fmpq.adhoc_exact_division...")

   a = -fmpz(2)//3

   @test divexact(a, 3) == fmpz(-2)//9

   @test divexact(a, fmpz(3)) == fmpz(-2)//9

   @test divexact(3, a) == fmpz(-9)//2

   @test divexact(fmpz(3), a) == fmpz(-9)//2

   @test divexact(a, 2//1) == -fmpz(2)//6

   @test divexact(a, BigInt(2)//BigInt(1)) == -fmpz(2)//6

   @test divexact(2//1, a) == -fmpz(6)//2

   @test divexact(BigInt(2)//BigInt(1), a) == -fmpz(6)//2

   println("PASS")
end

function test_fmpq_modular_arithmetic()
   print("fmpq.modular_arithmetic...")

   a = -fmpz(2)//3
   b = fmpz(1)//2

   @test mod(a, 7) == 4

   @test mod(b, fmpz(5)) == 3

   println("PASS")
end

function test_fmpq_gcd()
   print("fmpq.gcd...")

   a = -fmpz(2)//3
   b = fmpz(1)//2

   @test gcd(a, b) == fmpz(1)//6

   println("PASS")
end

function test_fmpq_rational_reconstruction()
   print("fmpq.rational_reconstruction...")

   @test reconstruct(7, 13) == fmpz(1)//2

   @test reconstruct(fmpz(15), 31) == -fmpz(1)//2

   @test reconstruct(fmpz(123), fmpz(237)) == fmpz(9)//2

   @test reconstruct(123, fmpz(237)) == fmpz(9)//2

   println("PASS")
end

function test_fmpq_rational_enumeration()
   print("fmpq.rational_enumeration...")

   @test next_minimal(fmpz(2)//3) == fmpz(3)//2

   @test next_signed_minimal(-fmpz(21)//31) == fmpz(31)//21

   @test next_calkin_wilf(fmpz(321)//113) == fmpz(113)//244

   @test next_signed_calkin_wilf(-fmpz(51)//17) == fmpz(1)//4

   println("PASS")
end

function test_fmpq_special_functions()
   print("fmpq.special_functions...")

   @test harmonic(12) == fmpz(86021)//27720

   @test dedekind_sum(12, 13) == -fmpz(11)//13

   @test dedekind_sum(fmpz(12), fmpz(13)) == -fmpz(11)//13

   @test dedekind_sum(-120, fmpz(1305)) == -fmpz(575)//522

   @test dedekind_sum(fmpz(-120), 1305) == -fmpz(575)//522

   println("PASS")
end

function test_fmpq_adhoc_remove_valuation()
   print("fmpq.adhoc_remove_valuation...")

   a = fmpq(2, 3)

   @test remove(a, 3) == (-1, fmpq(2, 1))
   @test valuation(a, 3) == -1

   println("PASS")
end

function test_fmpq()
   test_fmpq_constructors()
   test_fmpq_printing()
   test_fmpq_conversions()
   test_fmpq_manipulation()
   test_fmpq_unary_ops()
   test_fmpq_binary_ops()
   test_fmpq_adhoc_binary()
   test_fmpq_comparison()
   test_fmpq_adhoc_comparison()
   test_fmpq_shifting()
   test_fmpq_powering()
   test_fmpq_inversion()
   test_fmpq_exact_division()
   test_fmpq_adhoc_exact_division()
   test_fmpq_modular_arithmetic()
   test_fmpq_gcd()
   test_fmpq_rational_reconstruction()
   test_fmpq_rational_enumeration()
   test_fmpq_special_functions()
   test_fmpq_adhoc_remove_valuation()

   println("")
end
