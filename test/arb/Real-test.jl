RR = RealField()

@testset "RealFieldElem.precision" begin
   old_prec = precision(RealField)

   set_precision!(RealField, 100) do
      RR = RealField()
      @test precision(RR) == 100
   end

   set_precision!(RealField(), 100) do
      RR = RealField()
      @test precision(RR) == 100
   end

   @test precision(RealField) == old_prec

   set_precision!(RealField, 200)
   @test precision(RealField) == 200

   set_precision!(RealField(), 300)
   @test precision(RealField) == 300

   set_precision!(RealField, old_prec)
end

@testset "RealFieldElem.constructors" begin
   @test isa(RR, RealField)
   @test isa(RR(2), FieldElem)

   @test elem_type(RR) == RealFieldElem
   @test elem_type(RealField) == RealFieldElem
   @test parent_type(RealFieldElem) == RealField
   @test base_ring(RR) == Union{}

   @test RealField() == RealField()
end

@testset "arf.hecke_semantics" begin
    x = Nemo.arf_struct(0, 0, 0, 0)
    y = ZZRingElem(3)^100
    ccall((:arf_set_fmpz, Nemo.libarb), Nothing,
          (Ref{Nemo.arf_struct}, Ref{ZZRingElem}),
          x, y)
    ccall((:arf_clear, Nemo.libarb), Nothing,
          (Ref{Nemo.arf_struct},),
          x)
end

@testset "RealFieldElem.printing" begin
   a = RR(2)

   @test string(a) == "2.0000000000000000000"
end

@testset "RealFieldElem.basic_ops" begin
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
   @test BigFloat(RR(0.5)) == 0.5
   @test convert(BigFloat, RR(0.5)) == 0.5

   a = 123
   b = -8162
   c = 0.12
   @test ZZRingElem(RR(a)) == a
   @test_throws ErrorException ZZRingElem(RR(c))
   @test ZZ(RR(a)) == a
   @test_throws ErrorException ZZ(RR(c))
   @test BigInt(RR(a)) == a
   @test_throws ErrorException BigInt(RR(c))
   @test Int(RR(a)) == a
   @test_throws ErrorException Int(RR(c))

   @test abs(Float64(RR("2.3")) - 2.3) < 1e-10
   @test setprecision(BigFloat, 1000) do
      set_precision!(Balls, 1000) do
         abs(BigFloat(RR("2.3")) - BigFloat("2.3")) < 1e-299
      end
   end

   for T in [Float64, BigFloat]
      x = RR(-1)/3
      y = T(-1)/3
      @test abs(T(x, RoundDown) - T(x, RoundUp)) < 1e-10
      for z in [T(x, RoundNearest), y]
         @test T(x, RoundDown) <= z <= T(x, RoundUp)
      end
   end

   @test characteristic(RR) == 0
end

@testset "RealFieldElem.comparison" begin
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

@testset "RealFieldElem.adhoc_comparison" begin
   a = RR(3)

   for T in [ZZRingElem, QQFieldElem, Int, BigInt, Float64, BigFloat, Rational{Int}, Rational{BigInt}]
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

@testset "RealFieldElem.predicates" begin
   @test iszero(RR(0))
   @test !iszero(RR(1))
   @test !iszero(RR("0 +/- 0.01"))

   @test !is_nonzero(RR(0))
   @test is_nonzero(RR(1))
   @test !is_nonzero(RR("0 +/- 0.01"))

   @test isone(RR(1))
   @test !isone(RR(0))

   @test isfinite(RR(3))
   @test !isfinite(RR("0 +/- inf"))
   @test !isfinite(RR("nan"))

   @test is_exact(RR(3))
   @test !is_exact(RR("3 +/- 0.01"))
   @test is_exact(RR(QQ(1,4)))
   @test !is_exact(RR(QQ(1,3)))

   @test isinteger(RR(3))
   @test !isinteger(RR("3 +/- 0.01"))

   @test is_positive(RR(3))
   @test is_nonnegative(RR(3))
   @test is_negative(RR(-3))
   @test is_nonpositive(RR(-3))

   @test !is_positive(RR(0))
   @test is_nonnegative(RR(0))
   @test !is_negative(RR(0))
   @test is_nonpositive(RR(0))
end

@testset "RealFieldElem.parts" begin
   @test midpoint(RR(3)) == 3
   @test radius(RR(3)) == 0
   @test midpoint(RR("3 +/- 0.25")) == 3
   @test radius(RR("3 +/- 0.25")) >= 0.25

   x = RR("4 +/- 1")
   add_error!(x, RR("2 +/- 1"))
   @test midpoint(x) == 4
   @test 3.999 <= radius(x) <= 4.001
end

@testset "RealFieldElem.unary_ops" begin
   @test -RR(3) == RR(-3)
   @test abs(-RR(3)) == 3
   @test abs(RR(3)) == 3
   @test inv(RR(2)) == RR(0.5)
end

@testset "RealFieldElem.binary_ops" begin
   x = RR(2)
   y = RR(4)

   @test x + y == 6
   @test x - y == -2
   @test x * y == 8
   @test x // y == 0.5
   @test x ^ y == 16

   for T in [ZZRingElem, QQFieldElem, Int, BigInt, Rational{Int}, Rational{BigInt}]

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
      @test contains(x // T(4), QQFieldElem(1, 2))
      @test contains(T(2) + y, 6)
      @test contains(T(2) - y, -2)
      @test contains(T(2) * y, 8)
      @test contains(T(2) // y, QQFieldElem(1, 2))
      @test contains(x ^ T(4), 16)
   end
end

@testset "RealFieldElem.misc_ops" begin
   @test ldexp(RR(3), 2) == 12
   @test ldexp(RR(3), ZZ(2)) == 12
   @test contains(trim(RR("1.1 +/- 0.001")), RR("1.1"))

   @test accuracy_bits(RR(0)) == typemax(Int)
   @test accuracy_bits(RR("+/- inf")) == -typemax(Int)
   @test accuracy_bits(RR("0.1")) > precision(Balls) - 4

   uniq, n = unique_integer(RR("3 +/- 0.001"))
   @test uniq
   @test n == 3

   uniq, n = unique_integer(RR("3 +/- 1.001"))
   @test !uniq

   @test contains(setunion(RR(3), RR(4)), 3)
   @test contains(setunion(RR(3), RR(4)), 4)

   # Issue #499
   set_precision!(Balls, 1000) do
     b, i = unique_integer(RR(2)^1000)
     b, i = unique_integer(RR(2)^1000)
     b, i = unique_integer(RR(2)^1000)
     b, i = unique_integer(RR(2)^1000)
   end
end

@testset "RealFieldElem.unsafe_ops" begin
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

@testset "RealFieldElem.constants" begin
   @test overlaps(const_pi(RR), RR("3.141592653589793238462643 +/- 4.03e-25"))
   @test overlaps(const_e(RR), RR("2.718281828459045235360287 +/- 4.96e-25"))
   @test overlaps(const_log2(RR), RR("0.6931471805599453094172321 +/- 2.28e-26"))
   @test overlaps(const_log10(RR), RR("2.302585092994045684017991 +/- 4.79e-25"))
   @test overlaps(const_euler(RR), RR("0.5772156649015328606065121 +/- 3.42e-26"))
   @test overlaps(const_catalan(RR), RR("0.9159655941772190150546035 +/- 1.86e-26"))
   @test overlaps(const_khinchin(RR), RR("2.685452001065306445309715 +/- 2.28e-25"))
   @test overlaps(const_glaisher(RR), RR("1.282427129100622636875343 +/- 4.78e-25"))
end

@testset "RealFieldElem.functions" begin
   @test floor(RR(2.5)) == 2
   @test ceil(RR(2.5)) == 3

   a = 2.3
   b = 2
   c = 3
   @test floor(ZZRingElem, RR(a)) == b
   @test ceil(ZZRingElem, RR(a)) == c
   @test floor(ZZRingElem, RR(-a)) == -c
   @test ceil(ZZRingElem, RR(-a)) == -b
   @test typeof(floor(ZZRingElem, RR(a))) == ZZRingElem
   @test typeof(ceil(ZZRingElem, RR(a))) == ZZRingElem

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
   @test overlaps(gamma(x,y), RR("0.390850361255613677231063 +/- 4.29e-25"))
   @test overlaps(gamma_regularized(x,y), gamma(x,y) / gamma(x))
   @test overlaps(gamma_lower(x,y), gamma(x) - gamma(x,y))
   @test overlaps(gamma_lower_regularized(x,y), gamma_lower(x,y) / gamma(x))
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

   @test overlaps(factorial(x), RR("0.886581428719259125080918 +/- 6.66e-25"))
   @test factorial(UInt(10), RR) == 3628800
   @test factorial(Int(10), RR) == 3628800

   @test overlaps(binomial(x, UInt(3)), RR("0.0641293851417059390703256 +/- 4.37e-26"))
   @test overlaps(binomial(UInt(10), UInt(3), RR), RR("120.0000000000000000000000"))

   @test fibonacci(ZZ(10), RR) == 55
   @test fibonacci(10, RR) == 55
   @test fibonacci(-10, RR) == -55
   @test fibonacci(UInt(10), RR) == 55

   @test gamma(ZZ(10), RR) == 362880
   @test overlaps(gamma(QQ(1,4), RR), RR("3.625609908221908311930685 +/- 1.75e-25"))

   @test overlaps(zeta(UInt(3), RR), RR("1.202056903159594285399738 +/- 1.74e-25"))
   @test overlaps(zeta(3, RR), RR("1.202056903159594285399738 +/- 1.74e-25"))
   @test overlaps(zeta(-3, RR), RR("0.008333333333333333333333333 +/- 5.22e-28"))

   @test overlaps(bernoulli(12, RR), RR("-0.2531135531135531135531136 +/- 5.36e-26"))
   @test overlaps(bernoulli(UInt(12), RR), RR("-0.2531135531135531135531136 +/- 5.36e-26"))

   @test_throws DomainError bernoulli(-1, RR)

   @test overlaps(rising_factorial(x, 4), RR("4.828427124746190097603377 +/- 7.35e-25"))
   @test overlaps(rising_factorial(QQ(2,3), 4, RR), RR("10.86419753086419753086420 +/- 2.74e-24"))

   @test_throws DomainError rising_factorial(x, -1)
   @test_throws DomainError rising_factorial(QQ(2, 3), -1, RR)

   a, b = rising_factorial2(x, 4)
   @test overlaps(a, RR("4.828427124746190097603377 +/- 7.35e-25"))
   @test overlaps(b, RR("18.48528137423857029281013 +/- 3.08e-24"))

   a, b = rising_factorial2(x, UInt(4))
   @test overlaps(a, RR("4.828427124746190097603377 +/- 7.35e-25"))
   @test overlaps(b, RR("18.48528137423857029281013 +/- 3.08e-24"))

   @test_throws DomainError rising_factorial(x, -1)

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

   x = RR(1)
   @test overlaps(airy_ai(x),       RR("[0.1352924163128814155 +/- 2.42e-20]"))
   @test overlaps(airy_ai_prime(x), RR("[-0.1591474412967932128 +/- 2.88e-20]"))
   @test overlaps(airy_bi(x),       RR("[1.207423594952871259 +/- 5.27e-19]"))
   @test overlaps(airy_bi_prime(x), RR("[0.9324359333927756329 +/- 7.75e-20]"))
end

@testset "QQFieldElem.RealElem_special_functions" begin
   @test bernoulli(10) == ZZRingElem(5)//66

   b = bernoulli(100)

   bernoulli_cache(100)

   @test bernoulli(100) == b

   flint_cleanup()

   @test denominator(bernoulli(100)) == 33330
end

@testset "RealFieldElem.lindep" begin
   CC = ComplexField()

   tau = (1 + sqrt(CC(-23)))/2
   a = abs(modular_weber_f2(tau))^2
   C = lindep([RR(1), a, a^2, a^3, a^4, a^5], 20)

   @test C == ZZRingElem[-1, 1, 1, 0, 1, 0]
end

@testset "RealFieldElem.simplest_rational_inside" begin
   R = RealField()
   @test @inferred simplest_rational_inside(R(1)) == 1
   @test simplest_rational_inside(R(1//2)) == 1//2
   @test simplest_rational_inside(R("0.1 +/- 0.01")) == 1//10
   @test simplest_rational_inside(const_pi(R)) == 8717442233//2774848045
end

@testset "RealFieldElem.rand" begin
   R = RealField()

   n = 100
   for _ in 1:n
      r_urandom = rand(R; randtype = :urandom)
      r_randtest = rand(R; randtype = :randtest)
      r_exact = rand(R; randtype = :randtest_exact)
      r_precise = rand(R; randtype = :randtest_precise)
      r_wide = rand(R; randtype = :randtest_wide)
      r_special = rand(R; randtype = :randtest_special)

      @test contains(R(".5 +/- .5"), r_urandom)
      @test isfinite(r_randtest)
      @test isfinite(r_exact) && is_exact(r_exact)
      @test isfinite(r_precise)
      # Does not work for small precisions (< 20) because of radius
      if midpoint(r_precise) != 0 != radius(r_precise)
         @test R(0.99) * R(2)^(-6 - precision(Balls)) <
               abs(radius(r_precise) / midpoint(r_precise)) <
               R(1.01) * R(2)^(3 - precision(Balls))
      end
      @test isfinite(r_wide)
      @test r_special isa RealFieldElem
   end
end
