function test_elem(R::FlintRationalField)
   return rand_bits(ZZ, rand(0:100))//rand_bits(ZZ, rand(1:100))
end

@testset "fmpq.conformance_tests" begin
   # TODO: make this work with test_Field_interface_recursive
   test_Field_interface_recursive(FlintQQ)
end

@testset "fmpq.constructors" begin
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

   @test fmpq(3, -5) == -fmpq(3, 5)

   @test FlintQQ(2//typemin(Int)) == 1//(div(typemin(Int), 2))

   @test fmpq(2^62, 1) == 2^62

   @test fmpq(typemin(Int), 1) == typemin(Int)

   @test fmpq(typemax(Int), 1) == typemax(Int)

   @test fmpq(typemax(Int)) == typemax(Int)

   @test fmpq(typemin(Int)) == typemin(Int)
end

@testset "fmpq.rand" begin
   for bits in 1:100
      t = rand_bits(FlintQQ, bits)
      @test height_bits(t) <= bits
   end

   # implemented in AbstractAlgebra
   test_rand(QQ, 1:9) do f
      @test 1 <= numerator(f) <= 9
      @test 1 <= denominator(f) <= 9
   end
end

@testset "fmpq.printing" begin
   a = FlintQQ(1, 2)

   @test string(a) == "1//2"
end

@testset "fmpq.conversions" begin
   @test Rational(fmpz(12)) == 12

   @test Rational(fmpq(3, 7)) == 3//7
end

@testset "fmpq.manipulation" begin
   R = FractionField(ZZ)

   @test zero(fmpq) == 0

   a = -fmpz(2)//3
   b = fmpz(123)//234

   @test height(a) == 3

   @test height_bits(b) == 7

   @test abs(a) == fmpz(2)//3

   @test sign(fmpq(-2, 3)) == -1
   @test sign(fmpq()) == 0
   @test sign(fmpq(1, 7)) == 1

   @test isone(one(R))

   @test iszero(zero(R))

   @test isunit(one(R))

   @test isunit(fmpq(1, 3))

   @test deepcopy(fmpq(2, 3)) == fmpq(2, 3)

   @test numerator(fmpq(2, 3)) == 2

   @test denominator(fmpq(2, 3)) == 3

   @test characteristic(R) == 0

   @test floor(fmpq(2, 3)) == 0
   @test floor(fmpq(-1, 3)) == -1
   @test floor(fmpq(2, 1)) == 2

   @test ceil(fmpq(2, 3)) == 1
   @test ceil(fmpq(-1, 3)) == 0
   @test ceil(fmpq(2, 1)) == 2
end

@testset "fmpq.unary_ops" begin
   a = fmpq(-2, 3)

   @test -a == fmpq(2, 3)
end

@testset "fmpq.binary_ops" begin
   a = fmpq(-2, 3)
   b = fmpz(5)//7

   @test a + b == fmpq(1, 21)

   @test a - b == fmpq(-29, 21)

   @test a*b == fmpq(-10, 21)
end

@testset "fmpq.adhoc_binary" begin
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

   @test 1//2 - fmpq(1, 4) == fmpq(1, 4)

   @test BigInt(1)//BigInt(2) - fmpq(1, 2) == 0

   @test fmpq(1, 2) - BigInt(1)//BigInt(2) == 0

   @test fmpq(1, 2) * 1//2 == 1//4

   @test 1//2 * fmpq(1, 2) == 1//4

   @test BigInt(1)//BigInt(2) * fmpq(1, 2) == 1//4

   @test fmpq(1, 2) * BigInt(1)//BigInt(2) == 1//4

   @test fmpq(1, 2) // (BigInt(1)//BigInt(2)) == 1

   @test fmpq(1, 2) // (1//2) == 1
end

@testset "fmpq.comparison" begin
   a = fmpq(-2, 3)
   b = fmpz(1)//2

   @test a < b

   @test b > a

   @test b >= a

   @test a <= b

   @test a == fmpz(-4)//6

   @test a != b
end

@testset "fmpq.adhoc_comparison" begin
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
end

@testset "fmpq.shifting" begin
   a = -fmpz(2)//3
   b = fmpq(1, 2)

   @test a << 3 == -fmpz(16)//3

   @test b >> 5 == fmpz(1)//64
end

@testset "fmpq.powering" begin
   a = -fmpz(2)//3

   @test a^(-12) == fmpz(531441)//4096

   @test_throws DivideError fmpq(0)^-1
end

@testset "fmpq.inversion" begin
   a = -fmpz(2)//3

   @test inv(a) == fmpz(-3)//2

   @test_throws ErrorException inv(fmpq(0))
end

@testset "fmpq.exact_division" begin
   a = -fmpz(2)//3
   b = fmpz(1)//2
   c = fmpz(0)//1

   @test divexact(a, b) == fmpz(-4)//3

   @test_throws DivideError divexact(a, c)
end

@testset "fmpq.adhoc_exact_division" begin
   a = -fmpz(2)//3

   @test divexact(a, 3) == fmpz(-2)//9

   @test divexact(a, fmpz(3)) == fmpz(-2)//9

   @test divexact(3, a) == fmpz(-9)//2

   @test divexact(fmpz(3), a) == fmpz(-9)//2

   @test divexact(a, 2//1) == -fmpz(2)//6

   @test divexact(a, BigInt(2)//BigInt(1)) == -fmpz(2)//6

   @test divexact(2//1, a) == -fmpz(6)//2

   @test divexact(BigInt(2)//BigInt(1), a) == -fmpz(6)//2

   @test_throws DivideError divexact(a, 0)

   @test_throws DivideError divexact(a, 0//1)

   @test_throws DivideError divexact(a, ZZ(0))

   @test_throws DivideError divexact(12, QQ(0))

   @test_throws DivideError divexact(ZZ(12), QQ(0))

   for T in [fmpz, Int, BigInt, Rational{Int}, Rational{BigInt}]
     @test divexact(a, T(3)) == fmpq(-2, 9)
     @test a//T(3) == fmpq(-2, 9)
   end
end

@testset "fmpq.modular_arithmetic" begin
   a = -fmpz(2)//3
   b = fmpz(1)//2

   @test mod(a, 7) == 4

   @test mod(b, fmpz(5)) == 3
end

@testset "fmpq.gcd" begin
   a = -fmpz(2)//3
   b = fmpz(1)//2

   @test gcd(a, b) == fmpz(1)//6
end

@testset "fmpq.sqrt" begin
   a = fmpz(4)//9
   b = fmpz(0)//1

   @test sqrt(a) == fmpz(2)//3
   @test sqrt(b) == 0
end

@testset "fmpq.rational_reconstruction" begin
   @test reconstruct(7, 13) == fmpz(1)//2

   @test reconstruct(fmpz(15), 31) == -fmpz(1)//2

   @test reconstruct(fmpz(123), fmpz(237)) == fmpz(9)//2

   @test reconstruct(123, fmpz(237)) == fmpz(9)//2
end

@testset "fmpq.rational_enumeration" begin
   @test next_minimal(fmpz(2)//3) == fmpz(3)//2

   @test_throws DomainError next_minimal(fmpz(-1)//1)

   @test next_signed_minimal(-fmpz(21)//31) == fmpz(31)//21

   @test next_calkin_wilf(fmpz(321)//113) == fmpz(113)//244

   @test_throws DomainError next_calkin_wilf(fmpz(-1)//1)

   @test next_signed_calkin_wilf(-fmpz(51)//17) == fmpz(1)//4
end

@testset "fmpq.special_functions" begin
   @test harmonic(12) == fmpz(86021)//27720

   @test_throws DomainError harmonic(-1)

   @test dedekind_sum(12, 13) == -fmpz(11)//13

   @test dedekind_sum(fmpz(12), fmpz(13)) == -fmpz(11)//13

   @test dedekind_sum(-120, fmpz(1305)) == -fmpz(575)//522

   @test dedekind_sum(fmpz(-120), 1305) == -fmpz(575)//522
end

@testset "fmpq.adhoc_remove_valuation" begin
   a = fmpq(2, 3)

   @test remove(a, 3) == (-1, fmpq(2, 1))
   @test valuation(a, 3) == -1
end

@testset "fmpq.simplest_between" begin
   @test @inferred simplest_between(fmpq(-2//2), fmpq(1)) == -1
   @test simplest_between(fmpq(1//10), fmpq(3//10)) == 1//4
   @test simplest_between(fmpq(11//10), fmpq(21//10)) == 2
end
