import Nemo: AbstractAlgebra.PrettyPrinting

function test_elem(R::QQField)
  return rand_bits(ZZ, rand(0:100))//rand_bits(ZZ, rand(1:100))
end

@testset "QQFieldElem.conformance_tests" begin
  test_Field_interface_recursive(QQ)
end

@testset "QQFieldElem.issingletontype" begin
  @test Base.issingletontype(QQField)
end

@testset "QQFieldElem.constructors" begin
  R = fraction_field(ZZ)

  @test elem_type(R) == QQFieldElem
  @test elem_type(QQField) == QQFieldElem
  @test parent_type(QQFieldElem) == QQField

  @test isa(R, QQField)

  @test isa(R(2), QQFieldElem)

  @test isa(R(), QQFieldElem)

  @test isa(R(BigInt(1)//2), QQFieldElem)

  @test isa(R(2, 3), QQFieldElem)

  @test isa(R(ZZRingElem(2), 3), QQFieldElem)

  @test isa(R(2, ZZRingElem(3)), QQFieldElem)

  @test isa(R(ZZRingElem(2), ZZRingElem(3)), QQFieldElem)

  @test isa(R(R(2)), QQFieldElem)

  @test isa(QQFieldElem(2), QQFieldElem)

  @test isa(QQFieldElem(), QQFieldElem)

  @test isa(QQFieldElem(BigInt(1)//2), QQFieldElem)

  @test isa(QQFieldElem(2, 3), QQFieldElem)

  @test isa(QQFieldElem(ZZRingElem(2), 3), QQFieldElem)

  @test isa(QQFieldElem(2, ZZRingElem(3)), QQFieldElem)

  @test isa(QQFieldElem(ZZRingElem(2), ZZRingElem(3)), QQFieldElem)

  @test isa(QQFieldElem(R(2)), QQFieldElem)

  @test QQFieldElem(3, -5) == -QQFieldElem(3, 5)

  @test QQ(2//typemin(Int)) == 1//(div(typemin(Int), 2))

  @test QQFieldElem(2^62, 1) == 2^62

  @test QQFieldElem(typemin(Int), 1) == typemin(Int)

  @test QQFieldElem(typemax(Int), 1) == typemax(Int)

  @test QQFieldElem(typemax(Int)) == typemax(Int)

  @test QQFieldElem(typemin(Int)) == typemin(Int)

  @test zero(R) == zero(QQFieldElem)

  @test one(R) == one(QQFieldElem)

  @test sprint(show, "text/plain", QQ) == "Rational field"

  for T in [ZZRingElem, Int, BigInt, Rational{Int}, Rational{BigInt}]
    @test Nemo.promote_rule(QQFieldElem, T) == QQFieldElem
  end
end

@testset "QQFieldElem.rand" begin
  for bits in 1:100
    t = rand_bits(QQ, bits)
    @test height_bits(t) <= bits
  end

  # implemented in AbstractAlgebra
  test_rand(QQ, 1:9) do f
    @test 1 <= numerator(f) <= 9
    @test 1 <= denominator(f) <= 9
  end
end

@testset "QQFieldElem.printing" begin
  a = QQ(1, 2)

  @test string(a) == "1//2"
end

@testset "QQFieldElem.conversions" begin
  @test convert(Rational{Int}, QQFieldElem(3, 7)) == 3//7
  @test convert(Rational{BigInt}, QQFieldElem(3, 7)) == 3//7

  @test convert(QQFieldElem, 3) == QQFieldElem(3)
  @test convert(QQFieldElem, 3//7) == QQFieldElem(3, 7)

  @test Rational(ZZRingElem(12)) == 12
  @test Rational{Int}(ZZRingElem(12)) == 12
  @test Rational{BigInt}(ZZRingElem(12)) == 12

  @test convert(Rational{Int}, ZZRingElem(12)) == 12
  @test convert(Rational{BigInt}, ZZRingElem(12)) == 12

  @test Rational(QQFieldElem(3, 7)) == 3//7
  @test Rational{Int}(QQFieldElem(3, 7)) == 3//7
  @test Rational{BigInt}(QQFieldElem(3, 7)) == 3//7

  @test ZZ(QQFieldElem(3)) isa ZZRingElem
  @test_throws Exception ZZ(QQFieldElem(3, 2))

  @test ZZ(3//1) isa ZZRingElem
  @test_throws Exception ZZ(3//2)

  @test ZZ(big(3)//1) isa ZZRingElem
  @test_throws Exception ZZ(big(3)//2)

  @test BigFloat(QQFieldElem(3, 4)) == BigFloat(0.75)
  @test Float64(QQFieldElem(3, 4)) == 0.75
end

@testset "QQFieldElem.vector_arithmetics" begin
  @test QQFieldElem[1, 2, 3] // QQFieldElem(2) == QQFieldElem[1//2, 1, 3//2]
  @test QQFieldElem[1, 2, 3] / QQFieldElem(2) == QQFieldElem[1//2, 1, 3//2]
  @test QQFieldElem(2) * QQFieldElem[1, 2, 3] == QQFieldElem[2, 4, 6]
  @test QQFieldElem[1, 2, 3] * QQFieldElem(2) == QQFieldElem[2, 4, 6]

  @test QQFieldElem[1, 2, 3] // ZZRingElem(2) == QQFieldElem[1//2, 1, 3//2]
  @test QQFieldElem[1, 2, 3] / ZZRingElem(2) == QQFieldElem[1//2, 1, 3//2]
  @test ZZRingElem(2) * QQFieldElem[1, 2, 3] == QQFieldElem[2, 4, 6]
  @test QQFieldElem[1, 2, 3] * ZZRingElem(2) == QQFieldElem[2, 4, 6]
end

@testset "QQFieldElem.manipulation" begin
  R = fraction_field(ZZ)

  @test zero(QQFieldElem) == 0

  a = -ZZRingElem(2)//3
  b = ZZRingElem(123)//234

  @test height(a) == 3

  @test height_bits(b) == 7

  @test abs(a) == ZZRingElem(2)//3

  @test sign(QQFieldElem(-2, 3)) == -1
  @test sign(QQFieldElem()) == 0
  @test sign(QQFieldElem(1, 7)) == 1
  @test sign(Int, QQFieldElem(-2, 3)) == -1
  @test sign(Int, QQFieldElem()) == 0
  @test sign(Int, QQFieldElem(1, 7)) == 1

  @test signbit(QQFieldElem(-2, 3))
  @test !signbit(QQFieldElem())
  @test !signbit(QQFieldElem(1, 7))

  @test !isone(zero(R))
  @test isone(one(R))

  @test iszero(zero(R))
  @test !iszero(one(R))

  @test is_unit(one(R))
  @test is_unit(QQFieldElem(1, 3))
  @test !is_unit(QQFieldElem(0, 3))

  @test isinteger(zero(R))
  @test isinteger(one(R))
  @test !isinteger(QQFieldElem(1, 3))

  @test isfinite(zero(R))
  @test isfinite(one(R))
  @test isfinite(QQFieldElem(1, 3))

  @test !isinf(zero(R))
  @test !isinf(one(R))
  @test !isinf(QQFieldElem(1, 3))

  @test deepcopy(QQFieldElem(2, 3)) == QQFieldElem(2, 3)

  @test numerator(QQFieldElem(2, 3)) == 2

  @test denominator(QQFieldElem(2, 3)) == 3

  @test characteristic(R) == 0

  @test nbits(QQFieldElem(12, 1)) == 5
  @test nbits(QQFieldElem(1, 3)) == 3
end

@testset "QQFieldElem.rounding" begin
  @test floor(QQFieldElem(2, 3)) == 0
  @test floor(QQFieldElem(-1, 3)) == -1
  @test floor(QQFieldElem(2, 1)) == 2

  @test ceil(QQFieldElem(2, 3)) == 1
  @test ceil(QQFieldElem(-1, 3)) == 0
  @test ceil(QQFieldElem(2, 1)) == 2

  @test trunc(QQFieldElem(2, 3)) == 0
  @test trunc(QQFieldElem(-1, 3)) == 0
  @test trunc(QQFieldElem(2, 1)) == 2

  vals = vcat(
              Any[d//3 for d in -15:15],
              Any[Rational{BigInt}(d//3) for d in -15:15],
              Any[Rational{Int16}(d//3) for d in -15:15],
              Any[true//true, false//true],
             )

  @testset "$func" for func in (trunc, round, ceil, floor)
    for val in vals
      valQ = QQFieldElem(val)
      @test func(valQ) isa QQFieldElem
      @test func(valQ) == func(val)

      @test func(QQFieldElem, valQ) isa QQFieldElem
      @test func(QQFieldElem, valQ) == func(QQFieldElem, val)

      @test func(ZZRingElem, valQ) isa ZZRingElem
      @test func(ZZRingElem, valQ) == func(ZZRingElem, val)

      @test func(BigInt, valQ) isa BigInt
      @test func(BigInt, valQ) == func(BigInt, val)

      @test func(Int, valQ) isa Int
      @test func(Int, valQ) == func(Int, val)
    end
  end

  @testset "$mode" for mode in (RoundUp, RoundDown, RoundNearest, RoundNearestTiesAway)
    for val in vals
      valQ = QQFieldElem(val)
      @test round(valQ, mode) isa QQFieldElem
      @test round(valQ, mode) == round(val, mode)

      @test round(QQFieldElem, valQ, mode) isa QQFieldElem
      @test round(QQFieldElem, valQ, mode) == round(val, mode)

      @test round(ZZRingElem, valQ, mode) isa ZZRingElem
      @test round(ZZRingElem, valQ, mode) == round(val, mode)

      @test round(BigInt, valQ, mode) isa BigInt
      @test round(BigInt, valQ, mode) == round(val, mode)

      @test round(Int, valQ, mode) isa Int
      @test round(Int, valQ, mode) == round(val, mode)
    end
  end
end

@testset "QQFieldElem.unary_ops" begin
  a = QQFieldElem(-2, 3)

  @test -a == QQFieldElem(2, 3)
end

@testset "QQFieldElem.binary_ops" begin
  a = QQFieldElem(-2, 3)
  b = ZZRingElem(5)//7

  @test a + b == QQFieldElem(1, 21)

  @test a - b == QQFieldElem(-29, 21)

  @test a*b == QQFieldElem(-10, 21)
end

@testset "QQFieldElem.adhoc_binary" begin
  a = QQFieldElem(-2, 3)

  @test a + 3 == QQFieldElem(7, 3)

  @test 3 + a == QQFieldElem(7, 3)

  @test a - 3 == QQFieldElem(-11, 3)

  @test 3 - a == QQFieldElem(11, 3)

  @test a*3 == -2

  @test 3*a == -2

  @test a + ZZRingElem(3) == QQFieldElem(7, 3)

  @test ZZRingElem(3) + a == QQFieldElem(7, 3)

  @test a - ZZRingElem(3) == QQFieldElem(-11, 3)

  @test ZZRingElem(3) - a == QQFieldElem(11, 3)

  @test a*ZZRingElem(3) == -2

  @test ZZRingElem(3)*a == -2

  @test QQFieldElem(1, 2) + 1//2 == 1

  @test 1//2 + QQFieldElem(1, 2) == 1

  @test BigInt(1)//BigInt(2) + QQFieldElem(1, 2) == 1

  @test QQFieldElem(1, 2) + BigInt(1)//BigInt(2) == 1

  @test QQFieldElem(1, 2) - 1//2 == 0

  @test 1//2 - QQFieldElem(1, 4) == QQFieldElem(1, 4)

  @test BigInt(1)//BigInt(2) - QQFieldElem(1, 2) == 0

  @test QQFieldElem(1, 2) - BigInt(1)//BigInt(2) == 0

  @test QQFieldElem(1, 2) * 1//2 == 1//4

  @test 1//2 * QQFieldElem(1, 2) == 1//4

  @test BigInt(1)//BigInt(2) * QQFieldElem(1, 2) == 1//4

  @test QQFieldElem(1, 2) * BigInt(1)//BigInt(2) == 1//4

  @test QQFieldElem(1, 2) // (BigInt(1)//BigInt(2)) == 1

  @test QQFieldElem(1, 2) // (1//2) == 1

  a = QQ(1//2)
  @test a * 1.5 isa BigFloat
  @test isapprox(a * 1.5, 0.75)
  @test 1.5 * a isa BigFloat
  @test isapprox(1.5 * a, 0.75)
  @test a * big"1.5" isa BigFloat
  @test isapprox(a * big"1.5", big"0.75")
  @test big"1.5" * a isa BigFloat
  @test isapprox(big"0.75", big"1.5" * a)

end

@testset "QQFieldElem.comparison" begin
  a = QQFieldElem(-2, 3)
  b = ZZRingElem(1)//2

  @test a < b

  @test b > a

  @test b >= a

  @test a <= b

  @test a == ZZRingElem(-4)//6

  @test a != b
end

@testset "QQFieldElem.adhoc_comparison" begin
  a = -ZZRingElem(2)//3

  @test a < 1

  @test 1 > a

  @test a < ZZRingElem(1)

  @test ZZRingElem(1) > a

  @test a < 1//1

  @test 1//1 > a

  @test a < BigInt(1)//BigInt(1)

  @test BigInt(1)//BigInt(1) > a

  @test a <= 0

  @test 0 >= a

  @test a <= ZZRingElem(0)

  @test ZZRingElem(0) >= a

  @test a <= 0//1

  @test 0//1 >= a

  @test a <= BigInt(0)//BigInt(1)

  @test BigInt(0)//BigInt(1) >= a

  @test a != 1

  @test a != ZZRingElem(1)

  @test 1 != a

  @test ZZRingElem(1) != a

  @test a != 1//1

  @test a != BigInt(1)//1

  @test a == QQFieldElem(-2, 3)

  @test QQFieldElem(1, 2) == 1//2

  @test 1//2 == QQFieldElem(1, 2)

  @test QQFieldElem(1, 2) == BigInt(1)//BigInt(2)

  @test BigInt(1)//BigInt(2) == QQFieldElem(1, 2)

  @test QQ(1) > 0.7
  @test 0.7 < QQ(1)
  @test QQ(3//4) < 1.0
  @test 1.0 > QQ(3//4)
end

@testset "QQFieldElem.shifting" begin
  a = -ZZRingElem(2)//3
  b = QQFieldElem(1, 2)

  @test a << 3 == -ZZRingElem(16)//3

  @test b >> 5 == ZZRingElem(1)//64
end

@testset "QQFieldElem.powering" begin
  a = -ZZRingElem(2)//3

  @test a^(-12) == ZZRingElem(531441)//4096

  @test_throws DivideError QQFieldElem(0)^-1
end

@testset "QQFieldElem.inversion" begin
  a = -ZZRingElem(2)//3

  @test inv(a) == ZZRingElem(-3)//2

  @test_throws ErrorException inv(QQFieldElem(0))
end

@testset "QQFieldElem.exact_division" begin
  a = -ZZRingElem(2)//3
  b = ZZRingElem(1)//2
  c = ZZRingElem(0)//1

  @test divexact(a, b) == ZZRingElem(-4)//3

  @test_throws DivideError divexact(a, c)
end

@testset "QQFieldElem.adhoc_exact_division" begin
  a = -ZZRingElem(2)//3

  @test divexact(a, 3) == ZZRingElem(-2)//9

  @test divexact(a, ZZRingElem(3)) == ZZRingElem(-2)//9

  @test divexact(3, a) == ZZRingElem(-9)//2

  @test divexact(ZZRingElem(3), a) == ZZRingElem(-9)//2

  @test divexact(a, 2//1) == -ZZRingElem(2)//6

  @test divexact(a, BigInt(2)//BigInt(1)) == -ZZRingElem(2)//6

  @test divexact(2//1, a) == -ZZRingElem(6)//2

  @test divexact(BigInt(2)//BigInt(1), a) == -ZZRingElem(6)//2

  @test_throws DivideError divexact(a, 0)

  @test_throws DivideError divexact(a, 0//1)

  @test_throws DivideError divexact(a, ZZ(0))

  @test_throws DivideError divexact(12, QQ(0))

  @test_throws DivideError divexact(ZZ(12), QQ(0))

  for T in [ZZRingElem, Int, BigInt, Rational{Int}, Rational{BigInt}]
    @test divexact(a, T(3)) == QQFieldElem(-2, 9)
    @test a//T(3) == QQFieldElem(-2, 9)
  end
end

@testset "QQFieldElem.modular_arithmetic" begin
  a = -ZZRingElem(2)//3
  b = ZZRingElem(1)//2

  @test mod(a, 7) == 4

  @test mod(b, ZZRingElem(5)) == 3
end

@testset "QQFieldElem.gcd" begin
  a = -ZZRingElem(2)//3
  b = ZZRingElem(1)//2

  @test gcd(a, b) == ZZRingElem(1)//6
end

@testset "QQFieldElem.sqrt" begin
  a = ZZRingElem(4)//9
  b = ZZRingElem(0)//1

  @test sqrt(a) == ZZRingElem(2)//3
  @test sqrt(b) == 0

  @test !is_square(ZZRingElem(2)//9)
  @test !is_square(ZZRingElem(9)//2)

  @test is_square(ZZRingElem(4)//9)

  f1, s1 = is_square_with_sqrt(ZZRingElem(2)//9)

  @test !f1

  f2, s2 = is_square_with_sqrt(ZZRingElem(9)//2)

  @test !f2

  f2, s3 = is_square_with_sqrt(ZZRingElem(4)//9)

  @test f2 && s3 == ZZRingElem(2)//3
end

@testset "QQFieldElem.roots" begin
  @test root(QQFieldElem(1000, 27), 3) == QQFieldElem(10, 3)
  @test root(-QQFieldElem(27, 8), 3) == -3//2
  @test root(QQFieldElem(27, 8), 3; check=true) == 3//2

  @test_throws DomainError root(-QQFieldElem(1000, 27), 4)
  @test_throws DomainError root(QQFieldElem(1000, 27), -3)

  @test_throws ErrorException root(QQFieldElem(1100, 27), 3; check=true)
  @test_throws ErrorException root(QQFieldElem(27, 7), 3; check=true)
  @test_throws ErrorException root(-QQFieldElem(40, 27), 3; check=true)
  @test_throws ErrorException root(-QQFieldElem(27, 7), 3; check=true)
end

@testset "QQFieldElem.rational_reconstruction" begin
  @test reconstruct(7, 13) == ZZRingElem(1)//2

  @test reconstruct(ZZRingElem(15), 31) == -ZZRingElem(1)//2

  @test reconstruct(ZZRingElem(123), ZZRingElem(237)) == ZZRingElem(9)//2

  a, m = ZZRingElem(397284476), ZZRingElem(2^30 + 3)
  N = D = isqrt((m >> 1) - 1)
  flag, nd = reconstruct(a, m, N, D)
  @test nd == ZZRingElem(1)//ZZRingElem(100)

  N = D = ZZRingElem(50)
  flag, nd = reconstruct(a, m, N, D)
  @test !flag

  @test reconstruct(123, ZZRingElem(237)) == ZZRingElem(9)//2

  flag, nd = Nemo.unsafe_reconstruct(ZZRingElem(123), ZZRingElem(237))
  @test flag && nd == ZZRingElem(9)//2

  a, m = ZZRingElem(643465418), ZZRingElem(2^31-1)
  @test_throws ErrorException reconstruct(a, m)
  flag, nd = Nemo.unsafe_reconstruct(a, m)
  @test !flag
end

@testset "QQFieldElem.rational_enumeration" begin
  @test next_minimal(ZZRingElem(2)//3) == ZZRingElem(3)//2

  @test_throws DomainError next_minimal(ZZRingElem(-1)//1)

  @test next_signed_minimal(-ZZRingElem(21)//31) == ZZRingElem(31)//21

  @test next_calkin_wilf(ZZRingElem(321)//113) == ZZRingElem(113)//244

  @test_throws DomainError next_calkin_wilf(ZZRingElem(-1)//1)

  @test next_signed_calkin_wilf(-ZZRingElem(51)//17) == ZZRingElem(1)//4
end

@testset "QQFieldElem.special_functions" begin
  @test harmonic(12) == ZZRingElem(86021)//27720

  @test_throws DomainError harmonic(-1)

  @test dedekind_sum(12, 13) == -ZZRingElem(11)//13

  @test dedekind_sum(ZZRingElem(12), ZZRingElem(13)) == -ZZRingElem(11)//13

  @test dedekind_sum(-120, ZZRingElem(1305)) == -ZZRingElem(575)//522

  @test dedekind_sum(ZZRingElem(-120), 1305) == -ZZRingElem(575)//522

  @test log(ZZ(2), QQ(1//4)) == -2.0
  @test_throws DomainError log(QQ(-2))
end

@testset "QQFieldElem.adhoc_remove_valuation" begin
  a = QQFieldElem(2, 3)

  @test remove(a, 3) == (-1, QQFieldElem(2, 1))
  @test valuation(a, 3) == -1
end

@testset "QQFieldElem.simplest_between" begin
  @test @inferred simplest_between(QQFieldElem(-2//2), QQFieldElem(1)) == -1
  @test simplest_between(QQFieldElem(1//10), QQFieldElem(3//10)) == 1//4
  @test simplest_between(QQFieldElem(11//10), QQFieldElem(21//10)) == 2
end


@testset "QQFieldElem.unsafe" begin
  a = QQFieldElem(32//17)
  b = QQFieldElem(23//11)
  c = one(QQ)
  b_copy = deepcopy(b)
  c_copy = deepcopy(c)

  a = zero!(a)
  @test iszero(a)
  a = mul!(a, a, b)
  @test iszero(a)

  a = add!(a, a, b)
  @test a == b
  a = add!(a, a, 1)
  @test a == b + 1
  a = add!(a, a, ZZRingElem(0))
  @test a == b + 1

  a = add!(a, b^2)
  @test a == 1 + b + b^2

  a = mul!(a, a, b)
  @test a == (1 + b + b^2) * b
  a = mul!(a, a, 3)
  @test a == (1 + b + b^2) * b * 3
  a = mul!(a, a, ZZRingElem(3))
  @test a == (1 + b + b^2) * b * 9

  a = addmul!(a, a, c)
  @test a == 2 * (1 + b + b^2) * b * 9

  @test b_copy == b
  @test c_copy == c
end

@testset "QQFieldElem.printing" begin
  @test QQ === rational_field()
  @test PrettyPrinting.detailed(QQ) == "Rational field"
  @test PrettyPrinting.oneline(QQ) == "Rational field"
  @test PrettyPrinting.supercompact(QQ) == "QQ"

  # test LowercaseOff
  io = PrettyPrinting.pretty(IOBuffer())
  print(PrettyPrinting.terse(io), PrettyPrinting.Lowercase(), QQ)
  @test String(take!(io)) == "QQ"
end

@testset "QQFieldElem.is_perfect_power_with_data" begin
  for T in [Rational{Int}, Rational{BigInt}, QQFieldElem]
    @test @inferred is_perfect_power_with_data(T(5//9)) == (1, 5//9)
    @test @inferred is_perfect_power_with_data(T(4//9)) == (2, 2//3)
  end

  @test is_power(QQ(2), 2)[1] == false
  @test is_power(QQ(1//2), 2)[1] == false
  @test is_power(QQ(4//9), 2) == (true, 2//3)
end
