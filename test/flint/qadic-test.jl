@testset "QadicFieldElem.constructors" begin
  R, _     = @inferred QadicField(7, 1, 30)
  K, _     = @inferred QadicField(7, 3, 30) 
  QX, x = polynomial_ring(QQ, "x") 

  @test elem_type(R) == QadicFieldElem
  @test elem_type(QadicField) == QadicFieldElem
  @test parent_type(QadicFieldElem) == QadicField

  @test isa(R, QadicField)

  S, _ = @inferred QadicField(ZZRingElem(1009), 1, 30)

  @test isa(S, QadicField)

  R, _ = qadic_field(7, 1)
  @test isa(R, QadicField)

  @test_throws DomainError qadic_field(4, 2)

  Qp = padic_field(7, cached = false)
  K, _ = unramified_extension(Qp, 2)
  @test isa(K, QadicField)
  @test base_field(K) === Qp

  R, _ = qadic_field(7, 1, precision = 30)

  @test isa(R, QadicField)
  @test precision(R) == 30

  @test isa(R(), QadicFieldElem)

  @test isa(R(1), QadicFieldElem)

  @test isa(R(ZZ(123)), QadicFieldElem)

  @test isa(R(ZZ(1)//7^2), QadicFieldElem)

  @test isa(1 + 2*7 + 4*7^2 + O(R, 7^3), QadicFieldElem)

  @test isa(13 + 357*ZZRingElem(1009) + O(S, ZZRingElem(1009)^12), QadicFieldElem)

  @test isa(ZZRingElem(1)//7^2 + ZZRingElem(2)//7 + 3 + 4*7 + O(R, 7^2), QadicFieldElem)

  @test precision( R(QQFieldElem(2//3)^100) ) == precision( R(QQFieldElem(2//3))^100 )

  @test precision( K(QQFieldElem(2//3)^100*(x+1)) ) == precision( K( QX(QQFieldElem(2//3))^100 )*K(x+1) )

  s = R()

  t = deepcopy(s)

  @test isa(t, QadicFieldElem)

  R, _ = QadicField(13, 1, 10)
  a = gen(R)
  @test a isa QadicFieldElem
  a = gen(R; precision = 20)
  @test a isa QadicFieldElem
  @test precision(a) == 20

  b = R(prime(R))

  Q, _ = QadicField(13, 3, 10)
  _, t = polynomial_ring(ZZ, "t")
  @test Q(t^4) == Q(t)^4

  R, _ = QadicField(13, 1, 10)
  a = gen(R)
  @test a isa QadicFieldElem
  b = R(prime(R))

  @test !iszero(b^20)
  @test !iszero(deepcopy(b^20))
end

@testset "QadicFieldElem.printing" begin
  R, _ = QadicField(7, 3, 30)

  @test string(zero(R)) isa String
  @test string(one(R)) isa String
  @test string(gen(R)) isa String
  @test string(gen(R)^10) isa String
end

@testset "QadicFieldElem.manipulation" begin
  R, _ = QadicField(7, 1, 30)

  a = 1 + 2*7 + 4*7^2 + O(R, 7^3)
  b = 7^2 + 3*7^3 + O(R, 7^5)
  c = R(2)

  @test isone(one(R))
  @test isone(one(R, precision = 60))
  @test precision(one(R, precision = 60)) == 60

  @test iszero(zero(R))
  @test iszero(zero(R, precision = 60))
  @test precision(zero(R, precision = 60)) == 60

  d = one(R)
  @test !iszero(d)
  zero!(d, precision = 60)
  @test iszero(d)
  @test precision(d) == 60

  @test precision(a) == 3

  @test prime(R) == 7

  @test valuation(b) == 2

  @test valuation(R(0)) == precision(R(0))

  @test characteristic(R) == 0

  @test shift_right(a, 2) == R(7)^-2 + 2*R(7)^-1 + 4*7^0 + O(R, 7^3)
  @test shift_left(a, 2) == 7^2 + 2*7^3 + 4*7^4 + O(R, 7^5)
end

@testset "QadicFieldElem.unary_ops" begin
  R, _ = QadicField(7, 1, 30)

  a = 1 + 2*7 + 4*7^2 + O(R, 7^3)
  b = R(0)

  @test -a == 6 + 4*7^1 + 2*7^2 + O(R, 7^3)

  @test iszero(-b)
end

@testset "QadicFieldElem.binary_ops" begin
  R, _ = QadicField(7, 1, 30)

  a = 1 + 2*7 + 4*7^2 + O(R, 7^3)
  b = 7^2 + 3*7^3 + O(R, 7^5)
  c = O(R, 7^3)
  d = R(2)

  @test a + b == 1 + 2*7^1 + 5*7^2 + O(R, 7^3)

  @test a - b == 1 + 2*7^1 + 3*7^2 + O(R, 7^3)

  @test a*b == 1*7^2 + 5*7^3 + 3*7^4 + O(R, 7^5)

  @test b*c == O(R, 7^5)

  @test a*d == 2 + 4*7^1 + 1*7^2 + O(R, 7^3)
end

@testset "QadicFieldElem.adhoc_binary" begin
  R, _ = QadicField(7, 1, 30)

  a = 1 + 2*7 + 4*7^2 + O(R, 7^3)
  b = 7^2 + 3*7^3 + O(R, 7^5)
  c = O(R, 7^3)
  d = R(2)

  @test a + 2 == 3 + 2*7^1 + 4*7^2 + O(R, 7^3)

  @test 3 - b == 3 + 6*7^2 + 3*7^3 + 6*7^4 + O(R, 7^5)

  @test a*ZZRingElem(5) == 5 + 3*7^1 + O(R, 7^3)

  @test ZZRingElem(3)*c == O(R, 7^3)

  @test 2*d == 4

  @test 2 + d == 4

  @test iszero(d - ZZRingElem(2))

  @test a + ZZRingElem(1)//7^2 == ZZRingElem(1)//7^2 + 1 + 2*7^1 + 4*7^2 + O(R, 7^3)

  @test (ZZRingElem(12)//11)*b == 3*7^2 + 3*7^3 + O(R, 7^5)

  @test c*(ZZRingElem(1)//7) == O(R, 7^2)
end

@testset "QadicFieldElem.comparison" begin
  R, _ = QadicField(7, 1, 30)

  a = 1 + 2*7 + 4*7^2 + O(R, 7^3)
  b = 3*7^3 + O(R, 7^5)
  c = O(R, 7^3)
  d = R(2)

  @test a == 1 + 2*7 + O(R, 7^2)

  @test b == c

  @test c == R(0)

  @test d == R(2)
end

@testset "QadicFieldElem.adhoc_comparison" begin
  R, _ = QadicField(7, 1, 30)

  a = 1 + O(R, 7^3)
  b = O(R, 7^5)
  c = R(2)

  @test a == 1

  @test b == ZZ(0)

  @test c == 2

  @test ZZRingElem(2) == c

  @test a == ZZRingElem(344)//1
end

@testset "QadicFieldElem.powering" begin
  R, _ = QadicField(7, 1, 30)

  a = 1 + 7 + 2*7^2 + O(R, 7^3)
  b = O(R, 7^5)
  c = R(2)

  @test a^5 == 1 + 5*7^1 + 6*7^2 + O(R, 7^3)

  @test b^3 == O(R, 7^5)

  @test c^7 == 2 + 4*7^1 + 2*7^2
end

@testset "QadicFieldElem.inversion" begin
  R, _ = QadicField(7, 1, 30)

  a = 1 + 7 + 2*7^2 + O(R, 7^3)
  b = 2 + 3*7 + O(R, 7^5)
  c = 7^2 + 2*7^3 + O(R, 7^4)
  d = 7 + 2*7^2 + O(R, 7^5)

  @test inv(a) == 1 + 6*7^1 + 5*7^2 + O(R, 7^3)

  @test inv(b) == 4 + 4*7^1 + 3*7^2 + 1*7^3 + 1*7^4 + O(R, 7^5)

  @test inv(c) == ZZRingElem(1)//7^2 + ZZRingElem(5)//7 + O(R, 7^0)

  @test inv(d) == ZZRingElem(1)//7 + 5 + 3*7^1 + 6*7^2 + O(R, 7^3)

  @test inv(R(1)) == 1
end

@testset "QadicFieldElem.exact_division" begin
  R, _ = QadicField(7, 1, 30)

  a = 1 + 7 + 2*7^2 + O(R, 7^3)
  b = 2 + 3*7 + O(R, 7^5)
  c = 7^2 + 2*7^3 + O(R, 7^4)
  d = 7 + 2*7^2 + O(R, 7^5)

  @test divexact(a, b) == 4 + 1*7^1 + 2*7^2 + O(R, 7^3)
  @test a//b == 4 + 1*7^1 + 2*7^2 + O(R, 7^3)

  @test divexact(c, d) == 1*7^1 + O(R, 7^3)

  @test divexact(d, R(7^3)) == ZZRingElem(1)//7^2 + ZZRingElem(2)//7 + O(R, 7^2)

  @test divexact(R(34), R(17)) == 2
end

@testset "QadicFieldElem.adhoc_exact_division" begin
  R, _ = QadicField(7, 1, 30)

  a = 1 + 7 + 2*7^2 + O(R, 7^3)
  b = 2 + 3*7 + O(R, 7^5)
  c = 7^2 + 2*7^3 + O(R, 7^4)
  d = 7 + 2*7^2 + O(R, 7^5)

  @test divexact(a, 2) == 4 + 1*7^2 + O(R, 7^3)

  @test divexact(b, ZZRingElem(7)) == ZZRingElem(2)//7 + 3 + O(R, 7^4)

  @test divexact(c, ZZRingElem(12)//7^2) == 3*7^4 + 5*7^5 + O(R, 7^6)

  @test divexact(2, d) == ZZRingElem(2)//7 + 3 + 6*7^2 + O(R, 7^3)

  @test divexact(R(3), 3) == 1

  @test divexact(ZZRingElem(5)//7, R(5)) == ZZRingElem(1)//7
end

@testset "QadicFieldElem.divides" begin
  R, _ = QadicField(7, 1, 30)

  a = 1 + 7 + 2*7^2 + O(R, 7^3)
  b = 2 + 3*7 + O(R, 7^5)

  flag, q = divides(a, b)

  @test flag
  @test q == divexact(a, b)
end

@testset "QadicFieldElem.adhoc_gcd" begin
  R, _ = QadicField(7, 1, 30)

  a = 1 + 7 + 2*7^2 + O(R, 7^3)
  b = 2 + 3*7 + O(R, 7^5)

  @test gcd(a, b) == 1

  @test gcd(zero(R), zero(R)) == 0
end

@testset "QadicFieldElem.square_root" begin
  R, _ = QadicField(7, 1, 30)

  a = 1 + 7 + 2*7^2 + O(R, 7^3)
  b = 2 + 3*7 + O(R, 7^5)
  c = 7^2 + 2*7^3 + O(R, 7^4)

  @test sqrt(a)^2 == a

  @test sqrt(b)^2 == b

  @test sqrt(c)^2 == c

  @test sqrt(R(121))^2 == R(121)
end

@testset "QadicFieldElem.special_functions" begin
  R, _ = QadicField(7, 1, 30)

  a = 1 + 7 + 2*7^2 + O(R, 7^3)
  b = 2 + 5*7 + 3*7^2 + O(R, 7^3)
  c = 3*7 + 2*7^2 + O(R, 7^5)

  @test exp(c) == 1 + 3*7^1 + 3*7^2 + 4*7^3 + 4*7^4 + O(R, 7^5)

  @test log(a) == 1*7^1 + 5*7^2 + O(R, 7^3)

  @test exp(R(0)) == 1

  @test log(R(1)) == 0

  @test teichmuller(b) == 2 + 4*7^1 + 6*7^2 + O(R, 7^3)

  @test 6 * log(R(2)) == log(R(2)^6)
end

@testset "QadicFieldElem.setcoeff!" begin
  R, _ = QadicField(7, 1, 30)
  a = 1 + 7 + 2*7^2 + O(R, 7^3)
  setcoeff!(a, 0, ZZ(2))
  @test a == 2
end

@testset "QadicFieldElem.parent_overloading" begin
  K, _ = qadic_field(7, 2)

  for a in [K(), K(0), K(ZZ(0)), K(QQ(0))]
    a = K()
    @test is_zero(a)
    @test precision(a) == precision(K)
  end
  for a in [K(precision = 30), K(UInt(0), precision = 30), K(0, precision = 30), K(ZZ(0), precision = 30), K(QQ(0), precision = 30)]
    @test is_zero(a)
    @test precision(a) == 30
  end

  for a in [K(UInt(1), precision = 30), K(1, precision = 30), K(ZZ(1), precision = 30), K(QQ(1), precision = 30)]
    @test is_one(a)
    @test precision(a) == 30
  end

  a = K(7, precision = 30)
  @test precision(a) == 31

  a = K(ZZRingElem(7), precision = 30)
  @test precision(a) == 31

  a = K(BigInt(7), precision = 30)
  @test precision(a) == 31

  a = K(QQ(1//7), precision = 30)
  @test precision(a) == 29

  a = K(1//7, precision = 30)
  @test precision(a) == 29

  ZZx, x = polynomial_ring(ZZ, "x", cached = false)
  z = gen(K)
  @test K(x + 1) == z + 1
  @test K(x + 1, precision = 30) == gen(K, precision = 30) + one(K, precision = 30)

  QQx, x = polynomial_ring(QQ, "x", cached = false)
  z = gen(K)
  @test K(x + 1) == z + 1
  @test K(x + 1, precision = 30) == gen(K, precision = 30) + one(K, precision = 30)
end

@testset "QadicFieldElem.base_field" begin
  L, _ = QadicField(7, 2, 10)
  @test base_field(L) isa PadicField
  @test prime(base_field(L)) == 7
end

@testset "QadicField.setprecision" begin
  K, _  = QadicField(2, 2, 10)
  @test precision(K) == 10
  setprecision!(K, 20)
  @test precision(K) == 20
  a = with_precision(K, 30) do
    zero(K)
  end
  @test precision(a) == 30
  @test precision(K) == 20
  a = with_precision(K, 10) do
    zero(K)
  end
  @test precision(a) == 10
  @test precision(K) == 20

  # Make sure the precision of the base field is set as well
  b = with_precision(K, 30) do
    a = one(K)
    return coeff(a, 0) + 1
  end
  @test parent(b) === base_field(K)
  @test precision(b) == 30

  a = 1 + 2 + 2^2 + O(K, 2^3)
  @test precision(a) == 3
  b = setprecision(a, 5)
  @test precision(b) == 5
  @test precision(a) == 3
  setprecision!(a, 5)
  @test precision(a) == 5

  Kx, x = K["x"]
  f = x^2 + 1
  @test all(x -> precision(x) == precision(K), coefficients(f))
  g = setprecision(f, 30)
  @test all(x -> precision(x) == precision(K), coefficients(f))
  @test all(x -> precision(x) == 30, coefficients(g))
  setprecision!(f, 30)
  @test all(x -> precision(x) == 30, coefficients(f))
end

@testset "QadicField.as_polynomial" begin
  L, _ = qadic_field(5, 4)
  K = base_field(L)
  Kx, x = K["x"]

  for i in 1:100
    a = L(rand(-1000:1000))
    f = map_coefficients(y -> lift(QQ, y), Kx(a))
    @test L(f) == a
  end

  a = 2*gen(L) + gen(L)
  setcoeff!(a, 1, K(1))
  @test a == gen(L)
  setcoeff!(a, 1, UInt(2))
  @test coeff(a, 1) == K(2)
end
