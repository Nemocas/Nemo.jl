RR = ArbField(64)

function test_arb_constructors()
   print("arb.constructors()...")

   @test isa(RR, ArbField)
   @test isa(RR(2), FieldElem)

   @test elem_type(RR) == arb
   @test base_ring(RR) == Union{} 

   println("PASS")
end

function test_arb_basic_ops()
   print("arb.basic_ops()...")

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

   @test contains(ball(RR(1), RR(0.0001)), 1)

   @test Float64(RR(0.5)) == 0.5
   @test abs(Float64(RR("2.3")) - 2.3) < 1e-10

   println("PASS")
end

function test_arb_comparison()
   print("arb.comparison()...")

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

   println("PASS")
end

function test_arb_adhoc_comparison()
   print("arb.adhoc_comparison()...")

   a = RR(3)

   @test a == 3
   @test !(a == 4)
   @test a != 4
   @test !(a != 3)
   @test a >= 3
   @test a >= 2
   @test !(a >= 4)
   @test a > 2
   @test !(a > 3)
   @test a <= 3
   @test a <= 4
   @test !(a <= 2)
   @test a < 4
   @test !(a < 3)

   @test 3 == a
   @test !(4 == a)
   @test 4 != a
   @test !(3 != a)
   @test 3 <= a
   @test 2 <= a
   @test !(4 <= a)
   @test 2 < a
   @test !(3 < a)
   @test 3 >= a
   @test 4 >= a
   @test !(2 >= a)
   @test 4 > a
   @test !(3 > a)

   @test a == ZZ(3)
   @test !(a == ZZ(4))
   @test a != ZZ(4)
   @test !(a != ZZ(3))
   @test a >= ZZ(3)
   @test a >= ZZ(2)
   @test !(a >= ZZ(4))
   @test a > ZZ(2)
   @test !(a > ZZ(3))
   @test a <= ZZ(3)
   @test a <= ZZ(4)
   @test !(a <= ZZ(2))
   @test a < ZZ(4)
   @test !(a < ZZ(3))

   @test ZZ(3) == a
   @test !(ZZ(4) == a)
   @test ZZ(4) != a
   @test !(ZZ(3) != a)
   @test ZZ(3) <= a
   @test ZZ(2) <= a
   @test !(ZZ(4) <= a)
   @test ZZ(2) < a
   @test !(ZZ(3) < a)
   @test ZZ(3) >= a
   @test ZZ(4) >= a
   @test !(ZZ(2) >= a)
   @test ZZ(4) > a
   @test !(ZZ(3) > a)

   @test a == 3.0
   @test !(a == 4.0)
   @test a != 4.0
   @test !(a != 3.0)
   @test a >= 3.0
   @test a >= 2.0
   @test !(a >= 4.0)
   @test a > 2.0
   @test !(a > 3.0)
   @test a <= 3.0
   @test a <= 4.0
   @test !(a <= 2.0)
   @test a < 4.0
   @test !(a < 3.0)

   @test 3.0 == a
   @test !(4.0 == a)
   @test 4.0 != a
   @test !(3.0 != a)
   @test 3.0 <= a
   @test 2.0 <= a
   @test !(4.0 <= a)
   @test 2.0 < a
   @test !(3.0 < a)
   @test 3.0 >= a
   @test 4.0 >= a
   @test !(2.0 >= a)
   @test 4.0 > a
   @test !(3.0 > a)

   println("PASS")
end

function test_arb_predicates()
   print("arb.predicates()...")

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

   println("PASS")
end

function test_arb_parts()
   print("arb.parts()...")

   @test midpoint(RR(3)) == 3
   @test radius(RR(3)) == 0
   @test midpoint(RR("3 +/- 0.25")) == 3
   @test radius(RR("3 +/- 0.25")) >= 0.25

   println("PASS")
end

function test_arb_unary_ops()
   print("arb.unary_ops()...")

   @test -RR(3) == RR(-3)
   @test abs(-RR(3)) == 3
   @test abs(RR(3)) == 3
   @test inv(RR(2)) == RR(0.5)

   println("PASS")
end

function test_arb_binary_ops()
   print("arb.binary_ops()...")

   x = RR(2)
   y = RR(4)

   @test x + y == 6
   @test x - y == -2
   @test x * y == 8
   @test x // y == 0.5

   @test x + UInt(4) == 6
   @test x - UInt(4) == -2
   @test x * UInt(4) == 8
   @test x // UInt(4) == 0.5
   @test UInt(2) + y == 6
   @test UInt(2) - y == -2
   @test UInt(2) * y == 8
   @test UInt(2) // y == 0.5

   @test x + Int(4) == 6
   @test x - Int(4) == -2
   @test x * Int(4) == 8
   @test x // Int(4) == 0.5
   @test Int(2) + y == 6
   @test Int(2) - y == -2
   @test Int(2) * y == 8
   @test Int(2) // y == 0.5

   @test x + ZZ(4) == 6
   @test x - ZZ(4) == -2
   @test x * ZZ(4) == 8
   @test x // ZZ(4) == 0.5
   @test ZZ(2) + y == 6
   @test ZZ(2) - y == -2
   @test ZZ(2) * y == 8
   @test ZZ(2) // y == 0.5

   @test x ^ y == 16
   @test x ^ ZZ(4) == 16
   @test x ^ UInt(4) == 16
   @test x ^ Int(4) == 16
   @test x ^ QQ(4) == 16

   println("PASS")
end

function test_arb_misc_ops()
   print("arb.misc_ops()...")

   @test ldexp(RR(3), 2) == 12
   @test ldexp(RR(3), ZZ(2)) == 12
   @test contains(trim(RR("1.1 +/- 0.001")), RR("1.1"))

   @test accuracy_bits(RR(0)) == typemax(Int)
   @test accuracy_bits(RR("+/- inf")) == -typemax(Int)
   @test accuracy_bits(RR("0.1")) > prec(RR) - 4

   uniq, n = unique_integer(RR("3 +/- 0.001"))
   @test uniq
   @test n == 3

   uniq, n = unique_integer(RR("3 +/- 1.001"))
   @test !uniq

   @test contains(setunion(RR(3), RR(4)), 3)
   @test contains(setunion(RR(3), RR(4)), 4)

   println("PASS")
end

function test_arb_unsafe_ops()
   print("arb.unsafe_ops()...")

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

   println("PASS")
end

function test_arb_constants()
   print("arb.constants()...")

   @test overlaps(const_pi(RR), RR("3.141592653589793238462643 +/- 4.03e-25"))
   @test overlaps(const_e(RR), RR("2.718281828459045235360287 +/- 4.96e-25"))
   @test overlaps(const_log2(RR), RR("0.6931471805599453094172321 +/- 2.28e-26"))
   @test overlaps(const_log10(RR), RR("2.302585092994045684017991 +/- 4.79e-25"))
   @test overlaps(const_euler(RR), RR("0.5772156649015328606065121 +/- 3.42e-26"))
   @test overlaps(const_catalan(RR), RR("0.9159655941772190150546035 +/- 1.86e-26"))
   @test overlaps(const_khinchin(RR), RR("2.685452001065306445309715 +/- 2.28e-25"))
   @test overlaps(const_glaisher(RR), RR("1.282427129100622636875343 +/- 4.78e-25"))

   println("PASS")
end

function test_arb_functions()
   print("arb.functions()...")

   @test floor(RR(2.5)) == 2
   @test ceil(RR(2.5)) == 3
   @test sqrt(RR(4)) == 2
   @test rsqrt(RR(4)) == 0.5
   @test sqrt1pm1(RR(15)) == 3

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

   @test overlaps(risingfac(x, 4), RR("4.828427124746190097603377 +/- 7.35e-25"))
   @test overlaps(risingfac(QQ(2,3), 4, RR), RR("10.86419753086419753086420 +/- 2.74e-24"))

   a, b = risingfac2(x, 4)
   @test overlaps(a, RR("4.828427124746190097603377 +/- 7.35e-25"))
   @test overlaps(b, RR("18.48528137423857029281013 +/- 3.08e-24"))

   a, b = risingfac2(x, UInt(4))
   @test overlaps(a, RR("4.828427124746190097603377 +/- 7.35e-25"))
   @test overlaps(b, RR("18.48528137423857029281013 +/- 3.08e-24"))

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

   @test overlaps(bell(ZZ(100), RR), RR("4.758539127676483365879077e+115 +/- 1.16e+90"))
   @test overlaps(bell(100, RR), RR("4.758539127676483365879077e+115 +/- 1.16e+90"))

   println("PASS")
end

function test_fmpq_arb_special_functions()
   print("fmpq.arb_special_functions()...")

   @test bernoulli(10) == fmpz(5)//66

   b = bernoulli(100)

   bernoulli_cache(100)

   @test bernoulli(100) == b

   flint_cleanup()

   @test den(bernoulli(100)) == 33330

   println("PASS")
end

function test_arb()
   test_arb_constructors()
   test_arb_basic_ops()
   test_arb_comparison()
   test_arb_adhoc_comparison()
   test_arb_predicates()
   test_arb_parts()
   test_arb_unary_ops()
   test_arb_binary_ops()
   test_arb_misc_ops()
   test_arb_unsafe_ops()
   test_arb_constants()
   test_arb_functions()
   test_fmpq_arb_special_functions()

   println("")
end

