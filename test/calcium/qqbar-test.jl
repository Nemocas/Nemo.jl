@testset "QQBarFieldElem.constructors" begin
  R = algebraic_closure(QQ)

  @test elem_type(R) == QQBarFieldElem
  @test elem_type(QQBarField) == QQBarFieldElem
  @test parent_type(QQBarFieldElem) == QQBarField
  @test is_domain_type(QQBarFieldElem) == true
  @test base_ring(R) == R
  @test base_ring(QQBarFieldElem(3)) == R

  @test isa(R, QQBarField)

  @test isa(R(), QQBarFieldElem)
  @test isa(R(2), QQBarFieldElem)
  @test isa(R(big(2)), QQBarFieldElem)
  @test isa(R(2+3im), QQBarFieldElem)
  @test isa(R(big(2)+3im), QQBarFieldElem)
  @test isa(R(ZZRingElem(2)), QQBarFieldElem)
  @test isa(R(QQFieldElem(2)), QQBarFieldElem)
  @test isa(R(QQBarFieldElem(2)), QQBarFieldElem)
  @test isa(R(1//2), QQBarFieldElem)
  @test isa(R(big(1)//2), QQBarFieldElem)
  @test R(1//2) == R(QQ(1//2))

  @test isa(QQBarFieldElem(), QQBarFieldElem)
  @test isa(QQBarFieldElem(2), QQBarFieldElem)
  @test isa(QQBarFieldElem(big(2)), QQBarFieldElem)
  @test isa(QQBarFieldElem(2+3im), QQBarFieldElem)
  @test isa(QQBarFieldElem(big(2)+3im), QQBarFieldElem)
  @test isa(QQBarFieldElem(ZZRingElem(2)), QQBarFieldElem)
  @test isa(QQBarFieldElem(QQFieldElem(2)), QQBarFieldElem)
  @test isa(QQBarFieldElem(1//2), QQBarFieldElem)
  @test isa(QQBarFieldElem(big(1)//2), QQBarFieldElem)
  @test QQBarFieldElem(1//2) == QQBarFieldElem(QQ(1//2))

  x = R(1)
  @test deepcopy(x) !== x

end

@testset "QQBarFieldElem.printing" begin
  QQBar = algebraic_closure(QQ)
  a = QQBar(1)

  @test string(a) == "Root 1.00000 of x - 1"
  @test string(parent(a)) == "Field of algebraic numbers"

  @test string(-(QQBarFieldElem(10) ^ 20)) == "Root -1.00000e+20 of x + 100000000000000000000"
  @test string(root_of_unity(QQBar, 3)) == "Root -0.500000 + 0.866025*im of x^2 + x + 1"
  @test string(sqrt(QQBarFieldElem(-1)) // 3) == "Root 0.333333*im of 9x^2 + 1"
end


@testset "QQBarFieldElem.manipulation" begin
  QQBar = algebraic_closure(QQ)

  @test zero(QQBar) == 0
  @test one(QQBar) == 1
  @test isa(zero(QQBar), QQBarFieldElem)
  @test isa(one(QQBar), QQBarFieldElem)
  @test zero(QQBar) == zero(QQBarFieldElem)
  @test one(QQBar) == one(QQBarFieldElem)

  @test iszero(QQBar(0))
  @test isone(QQBar(1))
  @test is_rational(QQBar(1))
  @test isreal(QQBar(1))
  @test degree(QQBar(1)) == 1

  u = sqrt(QQBar(2))
  i = sqrt(QQBar(-1))

  @test i == QQBar(0+1im)
  @test 3+4*i == QQBar(3+4im)

  @test canonical_unit(u) == u
  @test hash(u) != hash(i)

  @test degree(u) == 2
  @test !is_rational(u)
  @test isreal(u)
  @test !is_rational(i)
  @test !isreal(i)
  @test is_algebraic_integer(u)

  @test denominator(QQBarFieldElem(3+4im) // 5) == 5
  @test numerator(QQBarFieldElem(3+4im) // 5) == QQBarFieldElem(3+4im)
  @test height(QQBarFieldElem(1+10im)) == 101
  @test height_bits(QQBarFieldElem(1+10im)) == 7

  @test inv(u) == u // 2

  @test abs(-u) == u
  @test abs2(u) == 2
  @test u != i
  @test sign(2*i) == i
  @test conj(i) == -i
  @test real(3+4*i) == 3
  @test imag(3+4*i) == 4
  @test csgn(i) == 1
  @test sign_real(-3+4*i) == -1
  @test sign_imag(-3+4*i) == 1
  @test floor(u) == 1
  @test floor(ZZRingElem, u) == 1 && floor(ZZRingElem, u) isa ZZRingElem
  @test ceil(u) == 2
  @test ceil(ZZRingElem, u) == 2 && ceil(ZZRingElem, u) isa ZZRingElem

  @test (u >> 3) == u // 8
  @test (u << 3) == 8 * u

  ZZx, x = polynomial_ring(ZZ, "x")
  QQy, y = polynomial_ring(QQ, "x")

  @test minpoly(ZZx, u) == x^2 - 2
  @test minpoly(QQy, u) == y^2 - 2

  @test evaluate(x^2, u) == QQBarFieldElem(2)
  @test evaluate(y^2, u) == QQBarFieldElem(2)

  @test root(QQBarFieldElem(-1), 3) == root_of_unity(QQBar, 6)
  @test root_of_unity(QQBar, 4) == i
  @test root_of_unity(QQBar, 4, 3) == -i

  @test sinpi(QQBarFieldElem(1)//6) == QQBarFieldElem(1)//2
  @test cospi(QQBarFieldElem(1)//3) == QQBarFieldElem(1)//2
  @test sincospi(QQBarFieldElem(1)//3) == (sinpi(QQBarFieldElem(1)//3), cospi(QQBarFieldElem(1)//3))
  @test tanpi(QQBarFieldElem(1)//3) == sqrt(QQBarFieldElem(3))
  @test_throws DomainError tanpi(QQBarFieldElem(1)//2)

  @test atanpi(sqrt(QQBarFieldElem(3))) == QQBarFieldElem(1)//3
  @test asinpi(sqrt(QQBarFieldElem(2))//2) == QQBarFieldElem(1)//4
  @test acospi(sqrt(QQBarFieldElem(3))//2) == QQBarFieldElem(1)//6

  @test exp_pi_i(QQBarFieldElem(1)//2) == i
  @test log_pi_i(i) == QQBarFieldElem(1)//2

  @test_throws DomainError atanpi(QQBarFieldElem(2))
  @test_throws DomainError asinpi(QQBarFieldElem(2))
  @test_throws DomainError acospi(QQBarFieldElem(2))
  @test_throws DomainError log_pi_i(QQBarFieldElem(2))

  @test_throws DivideError (QQBar(1) // QQBar(0))
  @test_throws DomainError (QQBar(0) ^ QQBar(-1))
  @test_throws DomainError (root(QQBar(1), 0))
  @test_throws DomainError (u ^ u)
  @test_throws DomainError (root_of_unity(QQBar, 0))
  @test_throws DomainError (root_of_unity(QQBar, 0, 1))

  @test is_root_of_unity(i)
  @test !is_root_of_unity(QQBarFieldElem(2))
  @test root_of_unity_as_args(-i) == (4, 3)
  @test_throws DomainError root_of_unity_as_args(QQBarFieldElem(2))

  v = roots(QQBar, x^5-x-1)
  @test v[1]^5 - v[1] - 1 == 0

  v = roots(QQBar, y^2+1)
  @test v == [i, -i]

  @test roots(QQBar, ZZx(0)) == []
  @test roots(QQBar, ZZx(1)) == []
  @test roots(QQBar, QQy(0)) == []
  @test roots(QQBar, QQy(1)) == []

  @test eigenvalues(QQBar, zero(matrix_space(ZZ, 0, 0))) == []
  @test eigenvalues(QQBar, zero(matrix_space(QQ, 0, 0))) == []
  @test eigenvalues(QQBar, ZZ[1 1; 1 -1]) == [u, -u]
  @test eigenvalues_with_multiplicities(QQBar, ZZ[1 1; 1 -1]) == [(u, 1), (-u, 1)]
  @test eigenvalues(QQBar, diagonal_matrix(ZZ[1 1; 1 -1], ZZ[1 1; 1 -1])) == [u, -u]
  @test eigenvalues_with_multiplicities(QQBar, diagonal_matrix(ZZ[1 1; 1 -1], ZZ[1 1; 1 -1])) == [(u, 2), (-u, 2)]
  @test eigenvalues(QQBar, QQ[1 1; 1 -1]) == [u, -u]
  @test eigenvalues_with_multiplicities(QQBar, QQ[1 1; 1 -1]) == [(u, 1), (-u, 1)]
  @test eigenvalues(QQBar, diagonal_matrix(QQ[1 1; 1 -1], QQ[1 1; 1 -1])) == [u, -u]
  @test eigenvalues_with_multiplicities(QQBar, diagonal_matrix(QQ[1 1; 1 -1], QQ[1 1; 1 -1])) == [(u, 2), (-u, 2)]

  @test conjugates(QQBarFieldElem(3)) == [QQBarFieldElem(3)]
  @test conjugates(u) == [u, -u]

  @test ZZRingElem(QQBarFieldElem(3)) == 3
  @test QQFieldElem(QQBarFieldElem(3) // 2) == QQFieldElem(3,2)

  set_precision!(Balls, 128) do
    RR = ArbField()
    CC = AcbField()

    @test RR(QQBarFieldElem(3)) == 3
    @test CC(QQBarFieldElem(3)) == 3
    @test_throws DomainError (RR(i))

    v = sqrt(RR(2)) + sqrt(RR(3))
    @test guess(QQBar, v, 4) == sqrt(QQBarFieldElem(2)) + sqrt(QQBarFieldElem(3))
    @test guess(QQBar, v, 4, 10) == sqrt(QQBarFieldElem(2)) + sqrt(QQBarFieldElem(3))
    @test_throws ErrorException guess(QQBar, v, 2)

    @test guess(QQBar, CC(2+i), 2, 10) == 2+i

    Rx, x = polynomial_ring(QQBar, "x")
    @test gcd(x^4 - 4*x^2 + 4, x^2 + sqrt(QQBar(18))*x + 4) == x + sqrt(QQBar(2))
  end

  # floor, ceil, round
  a = sqrt(QQBar(2))
  test_data = [(a, 1, 2, 1, 1, 2, 1),
              (QQBar(1), 1, 1, 1, 1, 1, 1),
              (QQBar(0), 0, 0, 0, 0, 0, 0),
              (QQBar(1//2), 0, 1, 1, 0, 1, 0),
              (QQBar(3//2), 1, 2, 2, 1, 2, 2),
              (QQBar(-1//2), -1, 0, -1, -1, 0, 0),
              (sqrt(QQBar(3)), 1, 2, 2, 1, 2, 2),
              ]
  for (e, f, c, r, rd, ru, rn) in test_data
    @test floor(e) == f && parent(floor(e)) === QQBar
    @test floor(ZZRingElem, e) == f && floor(ZZRingElem, e) isa ZZRingElem
    @test ceil(e) == QQBar(c) && parent(ceil(e)) === QQBar
    @test ceil(ZZRingElem, e) == c && ceil(ZZRingElem, e) isa ZZRingElem
    @test round(e) == r && parent(round(e)) === QQBar
    @test round(ZZRingElem, e) == r && round(ZZRingElem, e) isa ZZRingElem
    @test round(e, RoundDown) == rd && parent(round(e, RoundDown)) === QQBar
    @test round(ZZRingElem, e, RoundDown) == rd && round(ZZRingElem, e, RoundDown) isa ZZRingElem
    @test round(e, RoundUp) == ru && parent(round(e, RoundUp)) === QQBar
    @test round(ZZRingElem, e, RoundUp) == ru && round(ZZRingElem, e, RoundUp) isa ZZRingElem
    @test round(e, RoundNearest) == rn && parent(round(e, RoundNearest)) === QQBar
    @test round(ZZRingElem, e, RoundNearest) == rn && round(ZZRingElem, e, RoundNearest) isa ZZRingElem
  end

  RR = RealField()
  CC = ComplexField()

  @test RR(QQBarFieldElem(3)) == 3
  @test CC(QQBarFieldElem(3)) == 3
  @test_throws DomainError (RR(i))

  v = sqrt(RR(2)) + sqrt(RR(3))
  @test guess(QQBar, v, 4) == sqrt(QQBarFieldElem(2)) + sqrt(QQBarFieldElem(3))
  @test guess(QQBar, v, 4, 10) == sqrt(QQBarFieldElem(2)) + sqrt(QQBarFieldElem(3))
  @test_throws ErrorException guess(QQBar, v, 2)

  @test guess(QQBar, CC(2+i), 2, 10) == 2+i

  Rx, x = polynomial_ring(QQBar, "x")
  @test gcd(x^4 - 4*x^2 + 4, x^2 + sqrt(QQBar(18))*x + 4) == x + sqrt(QQBar(2))

end

@testset "QQBarFieldElem.adhoc_operations" begin
  @test QQBarFieldElem(2) + QQBarFieldElem(3) == 5
  @test QQBarFieldElem(2) + 3 == 5
  @test QQBarFieldElem(2) + ZZRingElem(3) == 5
  @test QQBarFieldElem(2) + QQFieldElem(3) == 5
  @test 3 + QQBarFieldElem(2) == 5
  @test ZZRingElem(3) + QQBarFieldElem(2) == 5
  @test QQFieldElem(3) + QQBarFieldElem(2) == 5

  @test QQBarFieldElem(2) - QQBarFieldElem(3) == -1
  @test QQBarFieldElem(2) - 3 == -1
  @test QQBarFieldElem(2) - ZZRingElem(3) == -1
  @test QQBarFieldElem(2) - QQFieldElem(3) == -1
  @test 3 - QQBarFieldElem(2) == 1
  @test ZZRingElem(3) - QQBarFieldElem(2) == 1
  @test QQFieldElem(3) - QQBarFieldElem(2) == 1

  @test QQBarFieldElem(2) * QQBarFieldElem(3) == 6
  @test QQBarFieldElem(2) * 3 == 6
  @test QQBarFieldElem(2) * ZZRingElem(3) == 6
  @test QQBarFieldElem(2) * QQFieldElem(3) == 6
  @test 3 * QQBarFieldElem(2) == 6
  @test ZZRingElem(3) * QQBarFieldElem(2) == 6
  @test QQFieldElem(3) * QQBarFieldElem(2) == 6

  @test QQBarFieldElem(6) // QQBarFieldElem(2) == 3
  @test QQBarFieldElem(6) // 2 == 3
  @test QQBarFieldElem(6) // ZZRingElem(2) == 3
  @test QQBarFieldElem(6) // QQFieldElem(2) == 3
  @test 6 // QQBarFieldElem(2) == 3
  @test ZZRingElem(6) // QQBarFieldElem(2) == 3
  @test QQFieldElem(6) // QQBarFieldElem(2) == 3

  @test QQBarFieldElem(2) ^ QQBarFieldElem(3) == 8
  @test QQBarFieldElem(2) ^ 3 == 8
  @test QQBarFieldElem(2) ^ ZZRingElem(3) == 8
  @test QQBarFieldElem(2) ^ QQFieldElem(3) == 8
  @test 2 ^ QQBarFieldElem(3) == 8
  @test ZZRingElem(2) ^ QQBarFieldElem(3) == 8
  @test QQFieldElem(2) ^ QQBarFieldElem(3) == 8

  @test QQBarFieldElem(2) < QQBarFieldElem(3)
  @test QQBarFieldElem(2) < 3
  @test QQBarFieldElem(2) < ZZRingElem(3)
  @test QQBarFieldElem(2) < QQFieldElem(3)
  @test 2 < QQBarFieldElem(3)
  @test ZZRingElem(2) < QQBarFieldElem(3)
  @test QQFieldElem(2) < QQBarFieldElem(3)

end

@testset "QQBarFieldElem.comparison" begin
  R = algebraic_closure(QQ)

  u = R(3) // 2
  i = sqrt(R(-1))

  @test u < 2
  @test u > 1
  @test is_positive(u)
  @test !is_negative(u)
  @test !is_positive(R(0))
  @test !is_negative(R(0))
  @test_throws DomainError (i > 1)

  @test is_equal_abs(u, -u)
  @test !is_equal_abs(u, 1+u)

  @test is_equal_real(u, u+i)
  @test !is_equal_real(u, -u+i)

  @test is_equal_imag(i, 1+i)
  @test !is_equal_imag(i, 1-i)

  @test is_equal_abs_real(u, -u+i)
  @test !is_equal_abs_real(u, 1+u+i)

  @test is_equal_abs_imag(i, 1-i)
  @test !is_equal_abs_imag(i, 1-2*i)

  @test is_less_real(u, 1+u+i)
  @test !is_less_real(u, u+i)

  @test is_less_imag(i, 2*i)
  @test !is_less_imag(i, i)

  @test is_less_abs(u, -2*u)
  @test is_less_abs(u, 2*u)
  @test !is_less_abs(u, -u)

  @test is_less_abs_real(-u, -2*u)
  @test is_less_abs_real(-u, 2*u)
  @test !is_less_abs_real(-u, u)

  @test is_less_abs_imag(-u, -2*i)
  @test is_less_abs_imag(-u, 2*i)
  @test !is_less_abs_imag(-u, u)

  @test is_less_root_order(u, i)
  @test is_less_root_order(i, -i)

end


@testset "QQBarFieldElem.inplace" begin
  R = algebraic_closure(QQ)

  x = R(7)
  zero!(x)
  @test x == 0

  x = R(7)
  y = mul!(x, R(3), R(5))
  @test x == 15
  @test x === y

  x = R(7)
  y = addeq!(x, R(3))
  @test x == 10
  @test x === y

  x = R(7)
  y = add!(x, R(3), R(5))
  @test x == 8
  @test x === y

end

@testset "QQBarFieldElem.rand" begin
  QQBar = algebraic_closure(QQ)

  for i=1:10
    x = rand(QQBar, degree=5, bits=5)
    @test degree(x) <= 5
  end

  for i=1:10
    x = rand(QQBar, degree=5, bits=5, randtype=:real)
    @test isreal(x)
  end

  for i=1:10
    x = rand(QQBar, degree=5, bits=5, randtype=:nonreal)
    # todo: need to upgrade Calcium
    # @test !isreal(x)
  end

  @test_throws ErrorException rand(QQBar, degree=2, bits=5, randtype=:gollum)

end

function test_elem(R::QQBarField)
  return rand(R, degree=5, bits=5)
end

@testset "QQBarFieldElem.conformance_tests" begin
  test_Field_interface(algebraic_closure(QQ))
end

