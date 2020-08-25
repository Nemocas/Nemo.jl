RR = ArbField(64)

@testset "arb.constructors..." begin
   @test isa(RR, ArbField)
   @test isa(RR(2), FieldElem)

   @test elem_type(RR) == arb
   @test elem_type(ArbField) == arb
   @test parent_type(arb) == ArbField
   @test base_ring(RR) == Union{}

   @test ArbField(10, cached = true) === ArbField(10, cached = true)
   @test ArbField(11, cached = false) !== ArbField(11, cached = false)
end

@testset "arb.printing..." begin
   a = RR(2)

   @test string(a) == "2.0000000000000000000"
end

@testset "arb.basic_ops..." begin
   @test one(RR) == 1
   @test zero(RR) == 0

   a = one(RR)
   @test RR(1) == a
   @test RR(ZZ(1)) == a
   @test RR(QQ(1)) == a
   @test RR(1.0) == a
   @test RR(UInt(1.0)) == a
   @test RR(RR(1)) == a
   @test RR("1.0") == a
   @test RR("1.0 +/- 0") == a
   @test RR("+1.00000e+0") == a
   @test RR(BigFloat(1)) == a
   @test RR(Rational{Int}(1)) == a
   @test RR(Rational{BigInt}(1)) == a

   @test contains(ball(RR(1), RR(0.0001)), 1)

   @test Float64(RR(0.5)) == 0.5
   @test convert(Float64, RR(0.5)) == 0.5
   @test abs(Float64(RR("2.3")) - 2.3) < 1e-10
   @test characteristic(RR) == 0
end

@testset "arb.comparison..." begin
   exact3 = RR(3)
   exact4 = RR(4)
   approx3 = RR("3 +/- 0.000001")
   approx4 = RR("4 +/- 0.000001")

   @test exact3 == exact3
   @test !(exact3 != exact3)
   @test exact4 > exact3
   @test exact4 >= exact3
   @test exact3 < exact4
   @test exact3 <= exact4

   @test exact4 > approx3
   @test exact4 >= approx3
   @test approx3 < exact4
   @test approx3 <= exact4

   @test !(exact3 == approx3)
   @test !(exact3 != approx3)
   @test !(exact3 > approx3)
   @test !(exact3 >= approx3)
   @test !(exact3 < approx3)
   @test !(exact3 <= approx3)

   @test isequal(approx3, approx3)
   @test !isequal(approx3, exact3)

   @test overlaps(approx3, exact3)
   @test overlaps(exact3, approx3)
   @test overlaps(approx3, approx3)
   @test !overlaps(approx3, approx4)

   @test contains(approx3, exact3)
   @test contains(approx3, approx3)
   @test !contains(exact3, approx3)

   @test contains(approx3, QQ(3))
   @test contains(approx3, ZZ(3))
   @test contains(approx3, 3)
   @test contains(approx3, BigFloat(3))

   @test !contains_zero(approx3)
   @test !contains_negative(approx3)
   @test contains_positive(approx3)
   @test !contains_nonpositive(approx3)
   @test contains_nonnegative(approx3)

   @test !contains_zero(-approx3)
   @test contains_negative(-approx3)
   @test !contains_positive(-approx3)
   @test contains_nonpositive(-approx3)
   @test !contains_nonnegative(-approx3)

   @test contains_zero(approx3 - 3)
   @test contains_negative(approx3 - 3)
   @test contains_positive(approx3 - 3)
   @test contains_nonpositive(approx3 - 3)
   @test contains_nonnegative(approx3 - 3)
end

@testset "arb.adhoc_comparison..." begin
   a = RR(3)

   for T in [fmpz, fmpq, Int, BigInt, Float64, BigFloat, Rational{Int}, Rational{BigInt}]
      @test a == T(3)
      @test !(a == T(4))
      @test a != T(4)
      @test !(a != T(3))
      @test a >= T(3)
      @test a >= T(2)
      @test !(a >= T(4))
      @test a > T(2)
      @test !(a > T(3))
      @test a <= T(3)
      @test a <= T(4)
      @test !(a <= T(2))
      @test a < T(4)
      @test !(a < T(3))
   end
end

@testset "arb.predicates..." begin
   @test iszero(RR(0))
   @test !iszero(RR(1))
   @test !iszero(RR("0 +/- 0.01"))

   @test !isnonzero(RR(0))
   @test isnonzero(RR(1))
   @test !isnonzero(RR("0 +/- 0.01"))

   @test isone(RR(1))
   @test !isone(RR(0))

   @test isfinite(RR(3))
   @test !isfinite(RR("0 +/- inf"))
   @test !isfinite(RR("nan"))

   @test isexact(RR(3))
   @test !isexact(RR("3 +/- 0.01"))
   @test isexact(RR(QQ(1,4)))
   @test !isexact(RR(QQ(1,3)))

   @test isint(RR(3))
   @test !isint(RR("3 +/- 0.01"))

   @test ispositive(RR(3))
   @test isnonnegative(RR(3))
   @test isnegative(RR(-3))
   @test isnonpositive(RR(-3))

   @test !ispositive(RR(0))
   @test isnonnegative(RR(0))
   @test !isnegative(RR(0))
   @test isnonpositive(RR(0))
end

@testset "arb.parts..." begin
   @test midpoint(RR(3)) == 3
   @test radius(RR(3)) == 0
   @test midpoint(RR("3 +/- 0.25")) == 3
   @test radius(RR("3 +/- 0.25")) >= 0.25
end

@testset "arb.unary_ops..." begin
   @test -RR(3) == RR(-3)
   @test abs(-RR(3)) == 3
   @test abs(RR(3)) == 3
   @test inv(RR(2)) == RR(0.5)
end

@testset "arb.binary_ops..." begin
   x = RR(2)
   y = RR(4)

   @test x + y == 6
   @test x - y == -2
   @test x * y == 8
   @test x // y == 0.5
   @test x ^ y == 16

   for T in [fmpz, fmpq, Int, BigInt, Rational{Int}, Rational{BigInt}]

      @test x + T(4) == 6
      @test x - T(4) == -2
      @test x * T(4) == 8
      @test x // T(4) == 0.5
      @test T(2) + y == 6
      @test T(2) - y == -2
      @test T(2) * y == 8
      @test T(2) // y == 0.5
      @test x ^ T(4) == 16
   end

   for T in [Float64, BigFloat]
      @test contains(x + T(4), 6)
      @test contains(x - T(4), -2)
      @test contains(x * T(4), 8)
      @test contains(x // T(4), fmpq(1, 2))
      @test contains(T(2) + y, 6)
      @test contains(T(2) - y, -2)
      @test contains(T(2) * y, 8)
      @test contains(T(2) // y, fmpq(1, 2))
      @test contains(x ^ T(4), 16)
   end
end

@testset "arb.misc_ops..." begin
   @test ldexp(RR(3), 2) == 12
   @test ldexp(RR(3), ZZ(2)) == 12
   @test contains(trim(RR("1.1 +/- 0.001")), RR("1.1"))

   @test accuracy_bits(RR(0)) == typemax(Int)
   @test accuracy_bits(RR("+/- inf")) == -typemax(Int)
   @test accuracy_bits(RR("0.1")) > precision(RR) - 4

   uniq, n = unique_integer(RR("3 +/- 0.001"))
   @test uniq
   @test n == 3

   uniq, n = unique_integer(RR("3 +/- 1.001"))
   @test !uniq

   @test contains(setunion(RR(3), RR(4)), 3)
   @test contains(setunion(RR(3), RR(4)), 4)

   # Issue #499
   RRR = ArbField(1000)
   b, i = unique_integer(RRR(2)^1000);
   b, i = unique_integer(RRR(2)^1000);
   b, i = unique_integer(RRR(2)^1000);
   b, i = unique_integer(RRR(2)^1000);
end

@testset "arb.unsafe_ops..." begin
   z = RR(1)
   x = RR(2)
   y = RR(3)

   add!(z, x, y)
   @test z == 5

   sub!(z, x, y)
   @test z == -1

   mul!(z, x, y)
   @test z == 6

   div!(z, y, x)
   @test z == 1.5
end

@testset "arb.constants..." begin
   @test overlaps(const_pi(RR), RR("3.141592653589793238462643 +/- 4.03e-25"))
   @test overlaps(const_e(RR), RR("2.718281828459045235360287 +/- 4.96e-25"))
   @test overlaps(const_log2(RR), RR("0.6931471805599453094172321 +/- 2.28e-26"))
   @test overlaps(const_log10(RR), RR("2.302585092994045684017991 +/- 4.79e-25"))
   @test overlaps(const_euler(RR), RR("0.5772156649015328606065121 +/- 3.42e-26"))
   @test overlaps(const_catalan(RR), RR("0.9159655941772190150546035 +/- 1.86e-26"))
   @test overlaps(const_khinchin(RR), RR("2.685452001065306445309715 +/- 2.28e-25"))
   @test overlaps(const_glaisher(RR), RR("1.282427129100622636875343 +/- 4.78e-25"))
end

@testset "arb.functions..." begin
   @test floor(RR(2.5)) == 2
   @test ceil(RR(2.5)) == 3
   @test sqrt(RR(4)) == 2
   @test rsqrt(RR(4)) == 0.5
   @test sqrt1pm1(RR(15)) == 3

   x = sqrtpos(sqrt(RR(2)) - sqrt(RR(2)))
   @test isfinite(x)
   @test contains(x, 0)

   x = sqrt(RR(2)) - 1
   y = sqrt(RR(3)) - 1

   @test overlaps(log(x), RR("-0.881373587019543025232609 +/- 4.18e-25"))
   @test overlaps(log1p(x), RR("0.3465735902799726547086160 +/- 7.33e-26"))
   @test overlaps(exp(x), RR("1.513180250744886829603548 +/- 2.95e-25"))
   @test overlaps(expm1(x), RR("0.5131802507448868296035478 +/- 8.15e-26"))
   @test overlaps(sin(x), RR("0.4024701238690976942014506 +/- 4.17e-26"))
   @test overlaps(cos(x), RR("0.9154331212015398552722359 +/- 5.39e-26"))
   @test overlaps(sinpi(x), RR("0.963902532849877330288337 +/- 2.68e-25"))
   @test overlaps(cospi(x), RR("0.266255342041415488608933 +/- 4.68e-25"))
   @test overlaps(tan(x), RR("0.4396499477109160739764696 +/- 7.24e-26"))
   @test overlaps(cot(x), RR("2.274536833693727714871270 +/- 5.23e-25"))
   @test overlaps(tanpi(x), RR("3.62021856710745059703047 +/- 2.21e-24"))
   @test overlaps(cotpi(x), RR("0.276226416019682080892189 +/- 1.62e-25"))
   @test overlaps(sinh(x), RR("0.4261602246690294501674754 +/- 9.44e-26"))
   @test overlaps(cosh(x), RR("1.087020026075857379436072 +/- 3.77e-25"))
   @test overlaps(tanh(x), RR("0.392044501891531832396677 +/- 1.49e-25"))
   @test overlaps(coth(x), RR("2.550730835849530928542115 +/- 9.57e-25"))
   @test overlaps(atan(x), RR("0.3926990816987241548078304 +/- 4.09e-26"))
   @test overlaps(asin(x), RR("0.4270785863924761254806469 +/- 9.08e-26"))
   @test overlaps(acos(x), RR("1.143717740402420493750675 +/- 3.10e-25"))
   @test overlaps(atanh(x), RR("0.4406867935097715126163046 +/- 8.42e-26"))
   @test overlaps(asinh(x), RR("0.4031997191615114958035310 +/- 5.55e-26"))
   @test overlaps(acosh(1+x), RR("0.8813735870195430252326093 +/- 4.46e-26"))
   @test overlaps(gamma(x), RR("2.14039690936215081177729 +/- 2.23e-24"))
   @test overlaps(lgamma(x), RR("0.760991283500573821902224 +/- 5.01e-25"))
   @test overlaps(rgamma(x), RR("0.467203066695702292350541 +/- 3.65e-25"))
   @test overlaps(digamma(x), RR("-2.46112318864250355875288 +/- 2.51e-24"))
   @test overlaps(zeta(x), RR("-1.17412759881491813598600 +/- 5.46e-24"))

   a, b = sincos(x)
   @test overlaps(a, RR("0.4024701238690976942014506 +/- 4.17e-26"))
   @test overlaps(b, RR("0.9154331212015398552722359 +/- 5.38e-26"))

   a, b = sincospi(x)
   @test overlaps(a, RR("0.963902532849877330288337 +/- 2.68e-25"))
   @test overlaps(b, RR("0.266255342041415488608933 +/- 4.68e-25"))
   @test overlaps(sinpi(x), RR("0.963902532849877330288337 +/- 2.68e-25"))
   @test overlaps(cospi(x), RR("0.266255342041415488608933 +/- 4.68e-25"))

   @test overlaps(sinpi(QQ(2,3), RR), RR("0.8660254037844386467637232 +/- 5.10e-26"))
   @test overlaps(cospi(QQ(2,3), RR), RR("-0.5000000000000000000000000"))
   a, b = sincospi(QQ(2,3), RR)
   @test overlaps(a, RR("0.8660254037844386467637232 +/- 5.10e-26"))
   @test overlaps(b, RR("-0.5000000000000000000000000"))

   a, b = sinhcosh(x)
   @test overlaps(a, RR("0.4261602246690294501674754 +/- 9.44e-26"))
   @test overlaps(b, RR("1.087020026075857379436072 +/- 3.77e-25"))

   @test overlaps(atan2(x,y), RR("0.514912633412022907863639 +/- 2.20e-25"))
   @test overlaps(agm(x,y), RR("0.561839426780398315452724 +/- 2.89e-25"))
   @test overlaps(zeta(x,y), RR("-0.80332051102108840234049 +/- 4.35e-24"))
   @test overlaps(hypot(x,y), RR("0.841113107801831438883282 +/- 2.34e-25"))

   @test overlaps(root(x, UInt(3)), RR("0.7454321246472561965628881 +/- 4.10e-26"))
   @test overlaps(root(x, 3), RR("0.7454321246472561965628881 +/- 4.10e-26"))

   @test_throws DomainError root(-x, 3)

   @test overlaps(fac(x), RR("0.886581428719259125080918 +/- 6.66e-25"))
   @test fac(UInt(10), RR) == 3628800
   @test fac(Int(10), RR) == 3628800

   @test overlaps(binom(x, UInt(3)), RR("0.0641293851417059390703256 +/- 4.37e-26"))
   @test overlaps(binom(UInt(10), UInt(3), RR), RR("120.0000000000000000000000"))

   @test fib(ZZ(10), RR) == 55
   @test fib(10, RR) == 55
   @test fib(-10, RR) == -55
   @test fib(UInt(10), RR) == 55

   @test gamma(ZZ(10), RR) == 362880
   @test overlaps(gamma(QQ(1,4), RR), RR("3.625609908221908311930685 +/- 1.75e-25"))

   @test overlaps(zeta(UInt(3), RR), RR("1.202056903159594285399738 +/- 1.74e-25"))
   @test overlaps(zeta(3, RR), RR("1.202056903159594285399738 +/- 1.74e-25"))
   @test overlaps(zeta(-3, RR), RR("0.008333333333333333333333333 +/- 5.22e-28"))

   @test overlaps(bernoulli(12, RR), RR("-0.2531135531135531135531136 +/- 5.36e-26"))
   @test overlaps(bernoulli(UInt(12), RR), RR("-0.2531135531135531135531136 +/- 5.36e-26"))

   @test_throws DomainError bernoulli(-1, RR)

   @test overlaps(risingfac(x, 4), RR("4.828427124746190097603377 +/- 7.35e-25"))
   @test overlaps(risingfac(QQ(2,3), 4, RR), RR("10.86419753086419753086420 +/- 2.74e-24"))

   @test_throws DomainError risingfac(x, -1)
   @test_throws DomainError risingfac(QQ(2, 3), -1, RR)

   a, b = risingfac2(x, 4)
   @test overlaps(a, RR("4.828427124746190097603377 +/- 7.35e-25"))
   @test overlaps(b, RR("18.48528137423857029281013 +/- 3.08e-24"))

   a, b = risingfac2(x, UInt(4))
   @test overlaps(a, RR("4.828427124746190097603377 +/- 7.35e-25"))
   @test overlaps(b, RR("18.48528137423857029281013 +/- 3.08e-24"))

   @test_throws DomainError risingfac(x, -1)

   @test overlaps(polylog(x,y), RR("1.89384268220168253175143 +/- 8.27e-24"))
   @test overlaps(polylog(3,y), RR("0.82112384129183065741 +/- 4.76e-21"))

   t3 = RR("-0.958369439657384170371292 +/- 5.13e-25")
   t2 = RR("-0.6568542494923801952067549 +/- 6.68e-26")
   u3 = RR("-1.088311754568578243139206 +/- 2.76e-25")
   u2 = RR("-0.313708498984760390413510 +/- 2.82e-25")

   @test overlaps(chebyshev_t(3,x), t3)
   @test overlaps(chebyshev_t(UInt(3),x), t3)
   @test overlaps(chebyshev_u(3,x), u3)
   @test overlaps(chebyshev_u(UInt(3),x), u3)

   @test_throws DomainError chebyshev_t(-1, x)
   @test_throws DomainError chebyshev_u(-1, x)

   a, b = chebyshev_t2(3,x)
   @test overlaps(a, t3)
   @test overlaps(b, t2)
   a, b = chebyshev_u2(3,x)
   @test overlaps(a, u3)
   @test overlaps(b, u2)

   a, b = chebyshev_t2(UInt(3),x)
   @test overlaps(a, t3)
   @test overlaps(b, t2)
   a, b = chebyshev_u2(UInt(3),x)
   @test overlaps(a, u3)
   @test overlaps(b, u2)

   @test_throws DomainError chebyshev_t2(-1, x)
   @test_throws DomainError chebyshev_u2(-1, x)

   @test overlaps(bell(ZZ(100), RR), RR("4.758539127676483365879077e+115 +/- 1.16e+90"))
   @test overlaps(bell(100, RR), RR("4.758539127676483365879077e+115 +/- 1.16e+90"))

   @test numpart(ZZ(10), RR) == 42
   @test numpart(10, RR) == 42
   @test numpart(ZZ(-10), RR) == 0
   @test numpart(-10, RR) == 0
   @test overlaps(numpart(ZZ(10)^20, RR), RR("1.8381765083448826436e+11140086259 +/- 4.69e+11140086239"))
end

@testset "fmpq.arb_special_functions..." begin
   @test bernoulli(10) == fmpz(5)//66

   b = bernoulli(100)

   bernoulli_cache(100)

   @test bernoulli(100) == b

   flint_cleanup()

   @test denominator(bernoulli(100)) == 33330
end

@testset "arb.lindep..." begin
   CC = ComplexField(64)

   tau = (1 + sqrt(CC(-23)))/2
   a = abs(modweber_f2(tau))^2
   C = lindep([RR(1), a, a^2, a^3, a^4, a^5], 20)

   @test C == fmpz[-1, 1, 1, 0, 1, 0]
end

@testset "arb.simplest_rational_inside" begin
   R = ArbField(64)
   @test @inferred simplest_rational_inside(R(1)) == 1
   @test simplest_rational_inside(R(1//2)) == 1//2
   @test simplest_rational_inside(R("0.1 +/- 0.01")) == 1//10
   @test simplest_rational_inside(const_pi(R)) == 8717442233//2774848045
end
