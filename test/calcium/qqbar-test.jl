@testset "QQBarFieldElem.constructors" begin
   R = CalciumQQBar

   @test R == QQBar

   @test elem_type(R) == QQBarFieldElem
   @test elem_type(QQBarField) == QQBarFieldElem
   @test parent_type(QQBarFieldElem) == QQBarField
   @test is_domain_type(QQBarFieldElem) == true
   @test base_ring(CalciumQQBar) == CalciumQQBar
   @test base_ring(QQBarFieldElem(3)) == CalciumQQBar

   @test isa(R, QQBarField)

   @test isa(R(), QQBarFieldElem)
   @test isa(R(2), QQBarFieldElem)
   @test isa(R(2+3im), QQBarFieldElem)
   @test isa(R(ZZRingElem(2)), QQBarFieldElem)
   @test isa(R(QQFieldElem(2)), QQBarFieldElem)
   @test isa(R(QQBarFieldElem(2)), QQBarFieldElem)

   @test isa(QQBarFieldElem(), QQBarFieldElem)
   @test isa(QQBarFieldElem(2), QQBarFieldElem)
   @test isa(QQBarFieldElem(2+3im), QQBarFieldElem)
   @test isa(QQBarFieldElem(ZZRingElem(2)), QQBarFieldElem)
   @test isa(QQBarFieldElem(QQFieldElem(2)), QQBarFieldElem)

   x = R(1)
   @test deepcopy(x) !== x

end

@testset "QQBarFieldElem.printing" begin
   a = CalciumQQBar(1)

   @test string(a) == "Root 1.00000 of x - 1"
   @test string(parent(a)) == "Field of algebraic numbers"

   @test string(-(QQBarFieldElem(10) ^ 20)) == "Root -1.00000e+20 of x + 100000000000000000000"
   @test string(root_of_unity(CalciumQQBar, 3)) == "Root -0.500000 + 0.866025*im of x^2 + x + 1"
   @test string(sqrt(QQBarFieldElem(-1)) // 3) == "Root 0.333333*im of 9x^2 + 1"
end


@testset "QQBarFieldElem.manipulation" begin
   R = CalciumQQBar

   @test zero(R) == 0
   @test one(R) == 1
   @test isa(zero(R), QQBarFieldElem)
   @test isa(one(R), QQBarFieldElem)
   @test zero(R) == zero(QQBarFieldElem)
   @test one(R) == one(QQBarFieldElem)

   @test iszero(R(0))
   @test isone(R(1))
   @test is_rational(R(1))
   @test isreal(R(1))
   @test degree(R(1)) == 1

   u = sqrt(R(2))
   i = sqrt(R(-1))

   @test i == R(0+1im)
   @test 3+4*i == R(3+4im)

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
   @test ceil(u) == 2

   @test (u >> 3) == u // 8
   @test (u << 3) == 8 * u

   ZZx, x = polynomial_ring(FlintZZ, "x")
   QQy, y = polynomial_ring(FlintQQ, "x")

   @test minpoly(ZZx, u) == x^2 - 2
   @test minpoly(QQy, u) == y^2 - 2

   @test evaluate(x^2, u) == QQBarFieldElem(2)
   @test evaluate(y^2, u) == QQBarFieldElem(2)

   @test root(QQBarFieldElem(-1), 3) == root_of_unity(R, 6)
   @test root_of_unity(R, 4) == i
   @test root_of_unity(R, 4, 3) == -i

   @test sinpi(QQBarFieldElem(1)//6) == QQBarFieldElem(1)//2
   @test cospi(QQBarFieldElem(1)//3) == QQBarFieldElem(1)//2
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

   @test_throws DivideError (R(1) // R(0))
   @test_throws DomainError (R(0) ^ R(-1))
   @test_throws DomainError (root(R(1), 0))
   @test_throws DomainError (u ^ u)
   @test_throws DomainError (root_of_unity(R, 0))
   @test_throws DomainError (root_of_unity(R, 0, 1))

   @test is_root_of_unity(i)
   @test !is_root_of_unity(QQBarFieldElem(2))
   @test root_of_unity_as_args(-i) == (4, 3)
   @test_throws DomainError root_of_unity_as_args(QQBarFieldElem(2))

   v = roots(CalciumQQBar, x^5-x-1)
   @test v[1]^5 - v[1] - 1 == 0

   v = roots(CalciumQQBar, y^2+1)
   @test v == [i, -i]

   @test roots(CalciumQQBar, ZZx(0)) == []
   @test roots(CalciumQQBar, ZZx(1)) == []
   @test roots(CalciumQQBar, QQy(0)) == []
   @test roots(CalciumQQBar, QQy(1)) == []

   @test eigenvalues(CalciumQQBar, zero(matrix_space(ZZ, 0, 0))) == []
   @test eigenvalues(CalciumQQBar, zero(matrix_space(QQ, 0, 0))) == []
   @test eigenvalues(CalciumQQBar, ZZ[1 1; 1 -1]) == [u, -u]
   @test eigenvalues_with_multiplicities(CalciumQQBar, ZZ[1 1; 1 -1]) == [(u, 1), (-u, 1)]
   @test eigenvalues(CalciumQQBar, diagonal_matrix(ZZ[1 1; 1 -1], ZZ[1 1; 1 -1])) == [u, -u]
   @test eigenvalues_with_multiplicities(CalciumQQBar, diagonal_matrix(ZZ[1 1; 1 -1], ZZ[1 1; 1 -1])) == [(u, 2), (-u, 2)]
   @test eigenvalues(CalciumQQBar, QQ[1 1; 1 -1]) == [u, -u]
   @test eigenvalues_with_multiplicities(CalciumQQBar, QQ[1 1; 1 -1]) == [(u, 1), (-u, 1)]
   @test eigenvalues(CalciumQQBar, diagonal_matrix(QQ[1 1; 1 -1], QQ[1 1; 1 -1])) == [u, -u]
   @test eigenvalues_with_multiplicities(CalciumQQBar, diagonal_matrix(QQ[1 1; 1 -1], QQ[1 1; 1 -1])) == [(u, 2), (-u, 2)]

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
     @test guess(CalciumQQBar, v, 4) == sqrt(QQBarFieldElem(2)) + sqrt(QQBarFieldElem(3))
     @test guess(CalciumQQBar, v, 4, 10) == sqrt(QQBarFieldElem(2)) + sqrt(QQBarFieldElem(3))
     @test_throws ErrorException guess(CalciumQQBar, v, 2)

     @test guess(CalciumQQBar, CC(2+i), 2, 10) == 2+i

     Rx, x = polynomial_ring(QQBar, "x")
     @test gcd(x^4 - 4*x^2 + 4, x^2 + sqrt(QQBar(18))*x + 4) == x + sqrt(QQBar(2))
   end
end

@testset "QQBarFieldElem.adhoc_operations" begin
   R = CalciumQQBar

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
   R = CalciumQQBar

   u = R(3) // 2
   i = sqrt(R(-1))

   @test u < 2
   @test u > 1
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
   R = CalciumQQBar

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
   R = CalciumQQBar

   for i=1:10
      x = rand(CalciumQQBar, degree=5, bits=5)
      @test degree(x) <= 5
   end

   for i=1:10
      x = rand(CalciumQQBar, degree=5, bits=5, randtype=:real)
      @test isreal(x)
   end

   for i=1:10
      x = rand(CalciumQQBar, degree=5, bits=5, randtype=:nonreal)
      # todo: need to upgrade Calcium
      # @test !isreal(x)
   end

   @test_throws ErrorException rand(CalciumQQBar, degree=2, bits=5, randtype=:gollum)

end

function test_elem(R::QQBarField)
   return rand(R, degree=5, bits=5)
end

@testset "QQBarFieldElem.conformance_tests" begin
   test_Field_interface(CalciumQQBar)
end

