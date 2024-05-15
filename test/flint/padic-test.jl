function test_elem(R::PadicField)
  p = prime(R)
  prec = rand(1:R.prec_max)
  r = ZZRingElem(0):p-1
  return R(sum(rand(r)*p^i for i in 0:prec))
end

@testset "PadicFieldElem.conformance_tests" begin
  # TODO: make the following work; for now they fail because the conformance
  # tests want to use isapprox on PadicFieldElem elements, but no such method exists
  #   test_Field_interface_recursive(PadicField(7, 30))
  #   test_Field_interface_recursive(PadicField(ZZRingElem(65537), 30))
end

@testset "PadicFieldElem.constructors" begin
  R = PadicField(7, 30)

  @test elem_type(R) == PadicFieldElem
  @test elem_type(PadicField) == PadicFieldElem
  @test parent_type(PadicFieldElem) == PadicField

  @test isa(R, PadicField)

  S = PadicField(ZZRingElem(65537), 30)

  @test isa(S, PadicField)

  R = padic_field(7)
  @test isa(R, PadicField)

  @test_throws DomainError padic_field(4)

  R = padic_field(7, precision = 30)

  @test isa(R, PadicField)
  @test precision(R) == 30

  @test isa(R(), PadicFieldElem)

  @test isa(R(1), PadicFieldElem)

  @test isa(R(ZZ(123)), PadicFieldElem)

  @test isa(R(ZZ(1)//7^2), PadicFieldElem)

  @test isa(1 + 2*7 + 4*7^2 + O(R, 7^3), PadicFieldElem)

  @test isa(13 + 357*ZZRingElem(65537) + O(S, ZZRingElem(65537)^12), PadicFieldElem)

  @test isa(ZZRingElem(1)//7^2 + ZZRingElem(2)//7 + 3 + 4*7 + O(R, 7^2), PadicFieldElem)

  @test precision(R(QQFieldElem(2//3)^100)) == precision(R(QQFieldElem(2//3))^100)

  s = R()

  t = deepcopy(s)

  @test isa(t, PadicFieldElem)

  @test parent(t) === R
end

@testset "PadicFieldElem.printing" begin
  R = PadicField(7, 30)

  a = 1 + 2*7 + 4*7^2 + O(R, 7^3)

  set_printing_mode(PadicField, :series)
  @test get_printing_mode(PadicField) == :series

  @test string(a) == "7^0 + 2*7^1 + 4*7^2 + O(7^3)"

  set_printing_mode(PadicField, :terse)
  @test get_printing_mode(PadicField) == :terse

  a = 1 + 2*7 + 4*7^2 + O(R, 7^3)
  @test sprint(show, "text/plain", a) == "211 + O(7^3)"

  set_printing_mode(PadicField, :val_unit)
  @test get_printing_mode(PadicField) == :val_unit

  @test string(a) == "211*7^0 + O(7^3)"

  a = 7 + 2*7 + 4*7^2 + O(R, 7^3)

  @test string(a) == "31*7^1 + O(7^3)"
end

@testset "PadicFieldElem.manipulation" begin
  R = PadicField(7, 30)

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
  @test prime(R, 3) == 7^3

  @test valuation(b) == 2


  @test lift(ZZ, a) == 211
  @test is_zero(lift(ZZ, R()))

  @test lift(QQ, divexact(a, b)) == QQFieldElem(337, 49)

  @test characteristic(R) == 0
end

@testset "PadicFieldElem.unary_ops" begin
  R = PadicField(7, 30)

  a = 1 + 2*7 + 4*7^2 + O(R, 7^3)
  b = R(0)

  @test -a == 6 + 4*7^1 + 2*7^2 + O(R, 7^3)

  @test iszero(-b)
end

@testset "PadicFieldElem.binary_ops" begin
  R = PadicField(7, 30)

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

@testset "PadicFieldElem.adhoc_binary" begin
  R = PadicField(7, 30)

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

@testset "PadicFieldElem.comparison" begin
  R = PadicField(7, 30)

  a = 1 + 2*7 + 4*7^2 + O(R, 7^3)
  b = 3*7^3 + O(R, 7^5)
  c = O(R, 7^3)
  d = R(2)

  @test a == 1 + 2*7 + O(R, 7^2)

  @test b == c

  @test c == R(0)

  @test d == R(2)
end

@testset "PadicFieldElem.adhoc_comparison" begin
  R = PadicField(7, 30)

  a = 1 + O(R, 7^3)
  b = O(R, 7^5)
  c = R(2)

  @test a == 1

  @test b == ZZ(0)

  @test c == 2

  @test ZZRingElem(2) == c

  @test a == ZZRingElem(344)//1
end

@testset "PadicFieldElem.powering" begin
  R = PadicField(7, 30)

  a = 1 + 7 + 2*7^2 + O(R, 7^3)
  b = O(R, 7^5)
  c = R(2)

  @test a^5 == 1 + 5*7^1 + 6*7^2 + O(R, 7^3)

  @test b^3 == O(R, 7^5)

  @test c^7 == 2 + 4*7^1 + 2*7^2
end

@testset "PadicFieldElem.inversion" begin
  R = PadicField(7, 30)

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

@testset "PadicFieldElem.exact_division" begin
  R = PadicField(7, 30)

  a = 1 + 7 + 2*7^2 + O(R, 7^3)
  b = 2 + 3*7 + O(R, 7^5)
  c = 7^2 + 2*7^3 + O(R, 7^4)
  d = 7 + 2*7^2 + O(R, 7^5)

  @test divexact(a, b) == 4 + 1*7^1 + 2*7^2 + O(R, 7^3)

  @test divexact(c, d) == 1*7^1 + O(R, 7^3)

  @test divexact(d, R(7^3)) == ZZRingElem(1)//7^2 + ZZRingElem(2)//7 + O(R, 7^2)

  @test divexact(R(34), R(17)) == 2
end

@testset "PadicFieldElem.adhoc_exact_division" begin
  R = PadicField(7, 30)

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

@testset "PadicFieldElem.divides" begin
  R = PadicField(7, 30)

  a = 1 + 7 + 2*7^2 + O(R, 7^3)
  b = 2 + 3*7 + O(R, 7^5)

  flag, q = divides(a, b)

  @test flag
  @test q == divexact(a, b)
end

@testset "PadicFieldElem.adhoc_gcd" begin
  R = PadicField(7, 30)

  a = 1 + 7 + 2*7^2 + O(R, 7^3)
  b = 2 + 3*7 + O(R, 7^5)

  @test gcd(a, b) == 1

  @test gcd(zero(R), zero(R)) == 0
end

@testset "PadicFieldElem.square_root" begin
  R = PadicField(7, 30)

  a = 1 + 7 + 2*7^2 + O(R, 7^3)
  b = 2 + 3*7 + O(R, 7^5)
  c = 7^2 + 2*7^3 + O(R, 7^4)

  @test sqrt(a) == 1 + 4*7^1 + 3*7^2 + O(R, 7^3)

  @test sqrt(b) == 3 + 5*7^1 + 1*7^2 + 1*7^3 + O(R, 7^5)

  @test sqrt(c) == 1*7^1 + 1*7^2 + O(R, 7^3)

  @test sqrt(R(121)) == 3 + 5*7^1 + 6*7^2 + 6*7^3 + 6*7^4 + 6*7^5 + O(R, 7^6)

  @test is_square(a)
  @test is_square(b)
  @test is_square(c)

  @test_throws ErrorException sqrt(3*7 + 1*7^2 + O(R, 7^3))
  @test_throws ErrorException sqrt(3*7^2 + 1*7^3 + O(R, 7^4))

  @test !is_square(3*7 + 1*7^2 + O(R, 7^3))
  @test !is_square(3*7^2 + 1*7^3 + O(R, 7^4))

  f1, s1 = is_square_with_sqrt(a)

  @test f1 && s1^2 == a

  f2, s2 = is_square_with_sqrt(b)

  @test f2 && s2^2 == b

  f3, s3 = is_square_with_sqrt(c)

  @test f3 && s3^2 == c

  f4, s4 = is_square_with_sqrt(3*7 + 1*7^2 + O(R, 7^3))

  @test !f4

  f5, s5 = is_square_with_sqrt(3*7^2 + 1*7^3 + O(R, 7^4))

  @test !f5

  R = PadicField(2, 5)

  d = 1 + 1*2 + 1*2^3 + O(R, 2^5)

  @test !is_square(d)

  m = 1*2 + 1*2^2 + 1*2^3 + O(R, 2^5)

  @test !is_square(m)

  f6, s6 = is_square_with_sqrt(d)

  @test !f6

  f7, s7 = is_square_with_sqrt(d)

  @test !f7

  @test is_square(d^2)

  f8, s8 = is_square_with_sqrt(d^2)

  @test f8 && s8^2 == d^2 
end

@testset "PadicFieldElem.special_functions" begin
  R = PadicField(7, 30)

  a = 1 + 7 + 2*7^2 + O(R, 7^3)
  b = 2 + 5*7 + 3*7^2 + O(R, 7^3)
  c = 3*7 + 2*7^2 + O(R, 7^5)

  @test exp(c) == 1 + 3*7^1 + 3*7^2 + 4*7^3 + 4*7^4 + O(R, 7^5)

  @test_throws DomainError exp(R(7)^-1)

  @test log(a) == 1*7^1 + 5*7^2 + O(R, 7^3)

  @test_throws ErrorException log(c)

  @test exp(R(0)) == 1

  @test log(R(1)) == 0

  @test teichmuller(b) == 2 + 4*7^1 + 6*7^2 + O(R, 7^3)

  @test_throws DomainError teichmuller(R(7)^-1)
end

@testset "PadicFieldElem.parent_overloading" begin
  K = padic_field(7)

  for a in [K(), K(0), K(ZZ(0)), K(QQ(0))]
    a = K()
    @test is_zero(a)
    @test precision(a) == precision(K)
  end
  for a in [K(precision = 30), K(0, precision = 30), K(ZZ(0), precision = 30), K(QQ(0), precision = 30)]
    @test is_zero(a)
    @test precision(a) == 30
  end

  for a in [K(1, precision = 30), K(ZZ(1), precision = 30), K(QQ(1), precision = 30)]
    @test is_one(a)
    @test precision(a) == 30
  end

  a = K(7, precision = 30)
  @test precision(a) == 31

  a = K(QQ(1//7), precision = 30)
  @test precision(a) == 29
end

@testset "PadicField.feature_parity" begin
  R = PadicField(2, 10)
  @test degree(R) == 1
  @test base_field(R) === R
  @test gens(R, R) == [one(R)]
  @test_throws AssertionError gens(R, PadicField(3, 10))
end

@testset "PadicField.setprecision" begin
  K = PadicField(2, 10)
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
