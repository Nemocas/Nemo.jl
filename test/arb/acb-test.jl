RR = ArbField(64)
CC = AcbField(64)

@testset "acb.constructors" begin
   @test isa(CC, AcbField)
   @test isa(CC(2), FieldElem)

   @test elem_type(CC) == acb
   @test base_ring(CC) == Union{}

   @test elem_type(AcbField) == acb
   @test parent_type(acb) == AcbField

   @test AcbField(10, cached = true) === AcbField(10, cached = true)
   @test AcbField(11, cached = false) !== AcbField(11, cached = false)
end

@testset "acb.printing" begin
   @test occursin(r"\A0\Z", string(CC(0)))
   @test occursin(r"\A1\.[0]+\Z", string(CC(1)))
   @test occursin(r"\A1\.[0]+[ ]*\*[ ]*?im\Z", string(onei(CC)))
   @test occursin(r"\A1\.[0]+[ ]*\+[ ]*1\.[0]+[ ]*?\*[ ]*im\Z",
                                                      string(CC(1) + onei(CC)))
end

@testset "acb.basic_ops" begin
   @test one(CC) == 1
   @test zero(CC) == 0

   a = one(CC)
   @test CC(1) == a
   @test CC(ZZ(1)) == a
   @test CC(QQ(1)) == a
   @test CC(RR(1)) == a
   @test CC(UInt(1)) == a
   @test CC(BigInt(1)) == a
   @test CC(Rational{Int}(1)) == a
   @test CC(Rational{BigInt}(1)) == a
   @test CC(RR(1)) == a
   @test CC("1.0") == a
   @test CC("1.0 +/- 0") == a
   @test CC("+1.00000e+0") == a
   @test CC(BigFloat(1)) == a

   b = CC(2,3)
   @test CC("2","3") == b
   @test CC(RR(2),RR(3)) == b
   @test CC(UInt(2), UInt(3)) == b
   @test CC(BigInt(2), BigInt(3)) == b
   @test CC(Rational{Int}(2), Rational{Int}(3)) == b
   @test CC(Rational{BigInt}(2), Rational{BigInt}(3)) == b
   @test CC(2.0, 3.0) == b
   @test CC(BigFloat(2), BigFloat(3)) == b
   @test real(b) == 2
   @test imag(b) == 3

   @test CC(UInt(4), Int(2)) == CC(4.0, ZZ(2))
   @test CC("4 +/- 0", BigFloat(2)) == CC(RR(4), QQ(2))
   @test CC(UInt(8)//UInt(2), BigInt(2)) == CC(4, 2)
@test CC(2, UInt(8)//UInt(2)) == CC(2, 4)

   @test characteristic(CC) == 0
end

@testset "acb.comparison" begin
   exact3 = CC(3)
   exact4 = CC(4)
   approx3 = CC("3 +/- 0.000001")
   approx4 = CC("4 +/- 0.000001")

   @test exact3 == exact3
   @test !(exact3 != exact3)

   @test !(exact3 == approx3)
   @test !(exact3 != approx3)

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

   @test !contains_zero(approx3)
   @test !contains_zero(-approx3)
   @test contains_zero(approx3 - 3)
end


@testset "acb.predicates" begin
   @test iszero(CC(0))
   @test !iszero(CC(1))
   @test !iszero(CC("0 +/- 0.01"))

   # @test !isnonzero(RR(0))
   # @test isnonzero(RR(1))
   # @test !isnonzero(RR("0 +/- 0.01"))

   @test isone(CC(1))
   @test !isone(CC(0))

   @test isfinite(CC(3))
   @test !isfinite(CC("0 +/- inf"))
   @test !isfinite(CC("nan"))

   @test isexact(CC(3))
   @test !isexact(CC("3 +/- 0.01"))
   @test isexact(CC(QQ(1,4)))
   @test !isexact(CC(QQ(1,3)))

   @test isint(CC(3))
   @test !isint(CC("3 +/- 0.01"))
end

@testset "acb.unary_ops" begin
   @test -CC(3) == CC(-3)
   @test abs(-CC(3)) == 3
   @test abs(CC(3)) == 3
   @test inv(CC(2)) == CC(QQ(1,2))
end

@testset "acb.binary_ops" begin
   x = CC(2)
   y = CC(4)

   @test x + y == 6
   @test x - y == -2
   @test x * y == 8
   @test x // y == 0.5

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

   for T in [Float64, BigFloat, arb]
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

@testset "acb.misc_ops" begin
   @test ldexp(CC(3), 2) == 12
   @test ldexp(CC(3), ZZ(2)) == 12
   @test contains(trim(CC("1.1 +/- 0.001")), CC("1.1"))

   @test accuracy_bits(CC(0)) == typemax(Int)
   @test accuracy_bits(CC("+/- inf")) == -typemax(Int)
   @test accuracy_bits(CC("0.1")) > precision(CC) - 4

   uniq, n = unique_integer(CC("3 +/- 0.001"))
   @test uniq
   @test n == 3

   uniq, n = unique_integer(CC("3 +/- 1.001"))
   @test !uniq

   uniq, n = unique_integer(CC("3", "0.1"))
   @test !uniq
end

@testset "acb.unsafe_ops" begin
   z = CC(1)
   x = CC(2)
   y = CC(3)

   add!(z, x, y)
   @test z == 5

   sub!(z, x, y)
   @test z == -1

   mul!(z, x, y)
   @test z == 6

   div!(z, y, x)
   @test z == 1.5
end

@testset "acb.constants" begin
   @test overlaps(const_pi(CC), CC("3.141592653589793238462643 +/- 4.03e-25"))
end

@testset "acb.functions" begin
   z = CC("0.2", "0.3")
   a = CC("0.3", "0.4")
   b = CC("0.4", "0.5")
   c = CC("0.5", "0.6")

   @test overlaps(sqrt(z), CC("0.5294124703604926084627418 +/- 6.58e-26",
                              "0.2833329556779434450121655 +/- 5.25e-26"))
   @test overlaps(rsqrt(z), CC("1.468326005965242732278446 +/- 5.21e-25",
                              "-0.7858242305581468733568419 +/- 6.40e-26"))
   @test overlaps(log(z), CC("-1.020110414263277315991248 +/- 3.16e-25",
                              "0.9827937232473290679857106 +/- 3.95e-26"))
   @test overlaps(log1p(z), CC("0.2126338677021720475020211 +/- 1.98e-26",
                              "0.2449786631268641541720825 +/- 3.64e-26"))
   @test overlaps(exp(z), CC("1.166850622789068287614508 +/- 2.54e-25",
                              "0.3609491955082235514294545 +/- 4.64e-26"))
   @test overlaps(expm1(z), CC("0.166850622789068287614508 +/- 2.54e-25",
                               "0.3609491955082235514294545 +/- 4.64e-26"))
   @test overlaps(cispi(z), CC("0.3152424821841265534507942 +/- 6.54e-26",
                              "0.2290370699407402465924600 +/- 7.08e-26"))
   @test overlaps(sin(z), CC("0.2076767030562843558332814 +/- 4.54e-26",
                              "0.2984501618819517453633022 +/- 5.06e-26"))
   @test overlaps(cos(z), CC("1.024501340227920709172021 +/- 1.78e-25",
                              "-0.06049884291265948916305985 +/- 8.58e-27"))
   @test overlaps(tan(z), CC("0.1848628040064145473517665 +/- 6.64e-26",
                              "0.3022291289077721469126452 +/- 9.03e-26"))
   @test overlaps(cot(z), CC("1.472814375144340834609492 +/- 5.49e-25",
                              "-2.407879768107776906367358 +/- 9.07e-25"))
   @test overlaps(sinpi(z), CC("0.868744702162250462386018 +/- 1.48e-25",
                              "0.880482019377109404605517 +/- 2.50e-25"))
   @test overlaps(cospi(z), CC("1.195724501561235958056311 +/- 4.82e-25",
                              "-0.639707632221510215793558 +/- 1.74e-25"))
   @test overlaps(tanpi(z), CC("0.258582202275679576275679 +/- 4.00e-25",
                              "0.874699001621106252447974 +/- 2.75e-25"))
   @test overlaps(cotpi(z), CC("0.310809701365069898900469 +/- 6.52e-25",
                              "-1.051367546125004636154355 +/- 3.73e-25"))

   @test overlaps(root_of_unity(CC, 2), CC(-1))
   @test_throws ArgumentError root_of_unity(CC, -1)
   @test_throws ArgumentError root_of_unity(CC, 0)
   @test overlaps(root_of_unity(CC, 4), onei(CC))
   @test overlaps(root_of_unity(CC, 5), CC("0.3090169943749474241 +/- 9.26e-20",
                                       "0.9510565162951535721 +/- 7.88e-20"))


   sval, cval = sincos(z)
   @test overlaps(sval, CC("0.2076767030562843558332814 +/- 4.54e-26",
                              "0.2984501618819517453633022 +/- 5.06e-26"))
   @test overlaps(cval, CC("1.024501340227920709172021 +/- 1.78e-25",
                              "-0.06049884291265948916305985 +/- 8.58e-27"))

   sval, cval = sincospi(z)
   @test overlaps(sval, CC("0.868744702162250462386018 +/- 1.48e-25",
                              "0.880482019377109404605517 +/- 2.50e-25"))
   @test overlaps(cval, CC("1.195724501561235958056311 +/- 4.82e-25",
                              "-0.639707632221510215793558 +/- 1.74e-25"))

   @test overlaps(sinh(z), CC("0.1923436298021928222471007 +/- 3.58e-26",
                              "0.3014503384289114663670543 +/- 5.95e-26"))
   @test overlaps(cosh(z), CC("0.9745069929868754653674075 +/- 6.93e-26",
                              "0.05949885707931208506240025 +/- 6.31e-27"))
   @test overlaps(tanh(z), CC("0.2154587730737840432610980 +/- 7.18e-26",
                              "0.2961813406783810497027117 +/- 5.25e-26"))
   @test overlaps(coth(z), CC("1.606152868815847688442798 +/- 4.94e-25",
                              "-2.207905035537343796489195 +/- 5.54e-25"))

   sval, cval = sinhcosh(z)
   @test overlaps(sval, CC("0.1923436298021928222471007 +/- 3.58e-26",
                              "0.3014503384289114663670543 +/- 5.95e-26"))
   @test overlaps(cval, CC("0.9745069929868754653674075 +/- 6.93e-26",
                              "0.05949885707931208506240025 +/- 6.31e-27"))


   @test overlaps(atan(z), CC("0.2154744937001882555778458 +/- 6.44e-26",
                              "0.2957499202364142781972578 +/- 4.56e-26"))

   @test overlaps(log_sinpi(z), CC("0.212622738160453236391712 +/- 3.07e-25",
                              "0.792108066076652209962478 +/- 3.84e-25"))

   @test overlaps(gamma(z), CC("1.17074211862417715153439 +/- 3.71e-24",
                              "-2.10413807786374340593296 +/- 2.55e-24"))
   @test overlaps(rgamma(z), CC("0.201920527977481935137338 +/- 2.50e-25",
                              "0.362905429693658450479200 +/- 2.66e-25"))
   @test overlaps(lgamma(z), CC("0.878759461001381725389280 +/- 5.88e-25",
                              "-1.063052882456422220552361 +/- 4.43e-25"))
   @test overlaps(digamma(z), CC("-1.76424192368129752832045 +/- 6.55e-24",
                              "2.67409284892388122018449 +/- 5.36e-24"))
   @test overlaps(rising_factorial(z,4), CC("0.362100000000000000000000 +/- 1.26e-25",
                              "3.162000000000000000000000 +/- 2.52e-25"))
   @test overlaps(rising_factorial(z,UInt(4)), CC("0.362100000000000000000000 +/- 1.26e-25",
                              "3.162000000000000000000000 +/- 2.52e-25"))

   @test_throws DomainError rising_factorial(z, -1)

   u, v = rising_factorial2(z,4)
   @test overlaps(u, CC("0.362100000000000000000000 +/- 1.21e-25",
                              "3.162000000000000000000000 +/- 1.87e-25"))
   @test overlaps(v, CC("9.316000000000000000000000 +/- 3.71e-25",
                              "8.796000000000000000000000 +/- 5.03e-25"))
   u, v = rising_factorial2(z,UInt(4))
   @test overlaps(u, CC("0.362100000000000000000000 +/- 1.21e-25",
                              "3.162000000000000000000000 +/- 1.87e-25"))
   @test overlaps(v, CC("9.316000000000000000000000 +/- 3.71e-25",
                              "8.796000000000000000000000 +/- 5.03e-25"))

   @test_throws DomainError rising_factorial2(z, -1)

   @test overlaps(polygamma(a,z), CC("-0.7483922021557882137094 +/- 6.32e-23",
                              "11.8258968574291607559455 +/- 4.05e-23"))
   @test overlaps(polylog(a,z), CC("0.149076595016851287862300 +/- 6.29e-25",
                              "0.417737740048930676709377 +/- 6.64e-25"))
   @test overlaps(polylog(3,z), CC("0.191881294823206392343013 +/- 7.59e-25",
                              "0.315096146289582929443521 +/- 5.44e-25"))
   @test overlaps(zeta(z), CC("-0.57948452849725094639168 +/- 3.30e-24",
                              "-0.38703035384397520079275 +/- 1.41e-24"))
   @test overlaps(zeta(z,a), CC("0.61155087453420024283540 +/- 5.02e-24",
                              "-0.82488469028124073728446 +/- 3.32e-24"))
   @test overlaps(barnes_g(z), CC("0.207890527664830899454035 +/- 8.15e-25",
                              "0.41000425789963963393056 +/- 2.21e-24"))
   @test overlaps(log_barnes_g(z), CC("-0.77718620877676355405122 +/- 6.03e-24",
                              "1.10152877228590925682947 +/- 1.86e-24"))
   @test overlaps(agm(a,b), CC("0.348259483785551624430965 +/- 5.98e-25",
                              "0.448649607194500405084686 +/- 2.99e-25"))
   @test overlaps(agm(a), CC("0.642470347997461360229908 +/- 8.30e-25",
                              "0.258011711262577345018833 +/- 7.89e-25"))
   @test overlaps(erf(z), CC("0.243097253707618155246101 +/- 3.70e-25",
                              "0.334443323443044934251369 +/- 3.24e-25"))
   @test overlaps(erfi(z), CC("0.2085288378827688631175979 +/- 9.12e-26",
                              "0.341237481472138596283589 +/- 3.83e-25"))
   @test overlaps(erfc(z), CC("0.756902746292381844753899 +/- 4.22e-25",
                              "-0.334443323443044934251369 +/- 3.70e-25"))
   @test overlaps(exp_integral_ei(z), CC("-0.258071740310124225105760 +/- 1.86e-25",
                              "1.313158595093461484954051 +/- 4.22e-25"))
   @test overlaps(sin_integral(z), CC("0.2025575702854160799774363 +/- 7.43e-26",
                              "0.299490037406755928146125 +/- 4.33e-25"))
   @test overlaps(cos_integral(z), CC("-0.430519178766360325396304 +/- 3.01e-25",
                              "0.952668915799813236516927 +/- 5.22e-25"))
   @test overlaps(sinh_integral(z), CC("0.1974464963284887729709696 +/- 7.58e-26",
                              "0.3004900626277844247301649 +/- 5.66e-26"))
   @test overlaps(cosh_integral(z), CC("-0.4555182366386129980767295 +/- 9.07e-26",
                              "1.012668532465677060223886 +/- 1.53e-25"))
   @test overlaps(log_integral(z), CC("-0.00563522578699947870932 +/- 5.04e-24",
                              "2.96647757239289013174985 +/- 1.08e-24"))
   @test overlaps(log_integral_offset(z), CC("-1.050799005904492263553913 +/- 9.59e-25",
                              "2.96647757239289013174985 +/- 1.08e-24"))
   @test overlaps(exp_integral_e(a,z), CC("0.15249323509272876700176 +/- 4.10e-24",
                              "-1.34436596014834977342501 +/- 2.02e-24"))
   @test overlaps(gamma(a,z), CC("0.52015862665033430896480 +/- 6.25e-24",
                              "-0.45171359572912367448392 +/- 5.94e-24"))
   @test overlaps(gamma_regularized(a,z), gamma(a,z) / gamma(a))
   @test overlaps(gamma_lower(a,z), gamma(a) - gamma(a,z))
   @test overlaps(gamma_lower_regularized(a,z), gamma_lower(a,z) / gamma(a))

   @test overlaps(airy_ai(z),       CC("[0.3008026290149675024 +/- 8.54e-20]",
                                       "[-0.0768563196244970504 +/- 6.98e-20]"))
   @test overlaps(airy_ai_prime(z), CC("[-0.2637194140647885059 +/- 8.11e-20]",
                                       "[0.0204576180267513109 +/- 4.42e-20]"))
   @test overlaps(airy_bi(z),       CC("[0.699432839434338965 +/- 1.26e-19]",
                                       "[0.1349581783797491604 +/- 7.70e-20]"))
   @test overlaps(airy_bi_prime(z), CC("[0.4260791047001445693 +/- 5.46e-20]",
                                       "[0.0381130644790292983 +/- 6.48e-20]"))

   @test overlaps(bessel_j(a,z), CC("0.46117305056699182297843 +/- 5.61e-24",
                              "-0.172953644007776862353437 +/- 6.92e-25"))
   @test overlaps(bessel_y(a,z), CC("-1.18656154996325105251999 +/- 4.72e-24",
                              "0.31443655250196831955353 +/- 8.03e-24"))
   @test overlaps(bessel_i(a,z), CC("0.466725582357895540843371 +/- 8.51e-25",
                              "-0.150139085652146628233083 +/- 7.46e-25"))
   @test overlaps(bessel_k(a,z), CC("1.38540911352254124513333 +/- 5.27e-24",
                              "-0.73450089723666866121649 +/- 8.89e-24"))
   @test overlaps(hypergeometric_1f1(a,b,z), CC("1.125983781196280931011114 +/- 7.61e-25",
                              "0.283048286141523623477498 +/- 6.79e-25"))
   @test overlaps(hypergeometric_1f1_regularized(a,b,z), CC("0.38425089179721272011261 +/- 4.66e-24",
                              "0.86691422812428398524918 +/- 1.89e-24"))
   @test overlaps(hypergeometric_u(a,b,z), CC("1.21934210138907906272586 +/- 8.19e-24",
                            "-0.00184089966759759298894 +/- 8.42e-24"))
   @test overlaps(hypergeometric_2f1(a,b,c,z), CC("0.913507440913606871706633 +/- 6.52e-25",
                            "0.127477171451557511736624 +/- 3.04e-25"))

   t1, t2, t3, t4 = jacobi_theta(z,a)
   @test overlaps(t1, CC("1.15342827918495425546807 +/- 7.18e-24",
                              "0.53226978187312777396054 +/- 4.41e-24"))
   @test overlaps(t2, CC("2.84687788483731660757883 +/- 3.56e-24",
                              "-0.25346724341130248214954 +/- 2.79e-24"))
   @test overlaps(t3, CC("2.84541084558501961355974 +/- 2.02e-24",
                              "-0.27710073395466035423707 +/- 6.30e-24"))
   @test overlaps(t4, CC("-0.66921558151467044848961 +/- 8.59e-24",
                              "0.81845615591876140504609 +/- 7.86e-24"))

   @test overlaps(dedekind_eta(z), CC("0.904816083881512590065651 +/- 9.09e-25",
                              "-0.098804163349957225969484 +/- 4.88e-25"))
   @test overlaps(j_invariant(z), CC("-1923742.4647359951069761 +/- 8.10e-17",
                              "-474343.2637224651049865 +/- 1.22e-17"))
   @test overlaps(modular_lambda(z), CC("0.99856752116671778730590 +/- 4.18e-24",
                              "-0.0112661792743731125728666 +/- 5.88e-26"))
   @test overlaps(modular_delta(z), CC("-0.09012304519443574525631 +/- 1.43e-24",
                              "-0.052947827926836557643152 +/- 8.29e-25"))
   @test overlaps(eisenstein_g(4, z),
                  CC("-90.182476999818759 +/- 7.47e-16",
                     "90.935041893602751 +/- 9.65e-16"))
   @test overlaps(eisenstein_g(6, z),
                  CC("858.06399824810497 +/- 7.27e-15",
                     "349.06790091288318 +/- 4.64e-15"))
   @test overlaps(eisenstein_g(6, onei(CC)),
                  CC("0 +/- 2.19e-17"))
   @test overlaps(eisenstein_g(8, onei(CC)),
                  CC("4.2557730353651895 +/- 3.20e-17"))
   @test overlaps(eisenstein_g(10, onei(CC)),
                  CC("0 +/- 3.14e-17"))

   @test overlaps(elliptic_k(z), CC("1.63015510394171138472863 +/- 3.00e-24",
                              "0.143703652492537358876625 +/- 4.78e-25"))
   @test overlaps(elliptic_e(z), CC("1.49751819287893089653527 +/- 4.93e-24",
                              "-0.12665473675024420800050 +/- 1.80e-24"))

   @test overlaps(weierstrass_p(z,a), CC("-1.35871985483098753373 +/- 1.89e-21",
                              "-51.93347883199212591376 +/- 6.19e-21"))
   @test overlaps(weierstrass_p_prime(z,a), CC("-462.546106875184 +/- 4.32e-13",
                              "-494.8014585693025 +/- 9.39e-14"))

   t = CC(rand(), abs(rand()) + eps())
   prod_sqr = (modular_weber_f(t)*modular_weber_f1(t)*modular_weber_f2(t))^2
   @test overlaps(prod_sqr, CC(2))
end

@testset "acb.fmpz_poly" begin
   R, x = PolynomialRing(ZZ, "x")
   @test hilbert_class_polynomial(-3, R) == x
   @test_throws ArgumentError hilbert_class_polynomial(2, R)
   @test_throws ArgumentError hilbert_class_polynomial(-2, R)
end

@testset "acb.lindep" begin
   CC = ComplexField(512)
   tau1 = CC(1//3, 8//7)
   tau2 = CC(1//5, 9//8)
   A1 = modular_weber_f1(tau1)^8; B1 = modular_weber_f1(2*tau1)^8

   vals1 = [A1^i*B1^j for i in 0:2 for j in 0:2];
   C = lindep(vals1, 150)

   @test_throws DomainError lindep(vals1, -1)

   R, (x, y) = PolynomialRing(ZZ, ["x", "y"])
   Phi = sum([C[3*i+j+1]*x^i*y^j for i in 0:2 for j in 0:2])

   @test Phi == x^2*y+16*x-y^2

   A2 = modular_weber_f1(tau2)^8; B2 = modular_weber_f1(2*tau2)^8
   vals2 = [A2^i*B2^j for i in 0:2 for j in 0:2]

   vals = permutedims([vals1 vals2])
   C = lindep(vals, 150)

   @test_throws DomainError lindep(vals, -1)

   Phi = sum([C[3*i+j+1]*x^i*y^j for i in 0:2 for j in 0:2])

   @test Phi == x^2*y+16*x-y^2

   CC = ComplexField(64)
end

@testset "acb.integration" begin
   res = Nemo.integrate(CC, x->x,  -1, 1)
   @test contains(res, CC(0))
   @test imag(res) == CC(0)
   @test radius(real(res)) < 3e-19

   res = Nemo.integrate(CC, x->x^2, -1, 1)
   @test contains(res, CC(2//3))
   @test imag(res) == CC(0)
   @test radius(real(res)) < 7e-19

   res = Nemo.integrate(CC, sin, 0, const_pi(CC))
   @test overlaps(res, CC(2))
   @test imag(res) == CC(0)
   @test radius(real(res)) < 4e-18

   res = Nemo.integrate(CC, exp, 0, 1, rel_tol = 1.0e-6, abs_tol = 1.0e-6)
   @test overlaps(res, CC(const_e(parent(real(zero(CC))))) - 1)
   @test radius(real(res)) < 1.0e-6
end
