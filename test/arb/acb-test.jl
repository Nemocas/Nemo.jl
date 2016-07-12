RR = ArbField(64)
CC = AcbField(64)

function test_acb_constructors()
   print("acb.constructors()...")

   @test isa(CC, AcbField)
   @test isa(CC(2), FieldElem)

   @test elem_type(CC) == acb
   @test base_ring(CC) == Union{} 

   println("PASS")
end

function test_acb_basic_ops()
   print("acb.basic_ops()...")

   @test one(CC) == 1
   @test zero(CC) == 0

   a = one(CC)
   @test CC(1) == a
   @test CC(ZZ(1)) == a
   @test CC(QQ(1)) == a
   @test CC(RR(1)) == a
   @test CC(UInt(1)) == a
   @test CC(RR(1)) == a
   @test CC("1.0") == a
   @test CC("1.0 +/- 0") == a
   @test CC("+1.00000e+0") == a
   @test CC(BigFloat(1)) == a

   b = CC(2,3)
   @test CC("2","3") == b
   @test CC(RR(2),RR(3)) == b
   @test CC(UInt(2), UInt(3)) == b
   @test CC(2.0, 3.0) == b
   @test CC(BigFloat(2), BigFloat(3)) == b
   @test real(b) == 2
   @test imag(b) == 3

   println("PASS")
end

function test_acb_comparison()
   print("acb.comparison()...")

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

   println("PASS")
end


function test_acb_predicates()
   print("acb.predicates()...")

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

   println("PASS")
end

function test_acb_unary_ops()
   print("acb.unary_ops()...")

   @test -CC(3) == CC(-3)
   @test abs(-CC(3)) == 3
   @test abs(CC(3)) == 3
   @test inv(CC(2)) == CC(QQ(1,2))

   println("PASS")
end

function test_acb_binary_ops()
   print("acb.binary_ops()...")

   x = CC(2)
   y = CC(4)

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

   @test x + QQ(4) == 6
   @test x - QQ(4) == -2
   @test x * QQ(4) == 8
   @test x // QQ(4) == 0.5
   @test QQ(2) + y == 6
   @test QQ(2) - y == -2
   @test QQ(2) * y == 8
   @test QQ(2) // y == 0.5

   @test x ^ y == 16
   @test x ^ ZZ(4) == 16
   @test x ^ UInt(4) == 16
   @test x ^ Int(4) == 16
   @test x ^ QQ(4) == 16

   @test ZZ(2) ^ y == 16
   @test UInt(2) ^ y == 16
   @test Int(2) ^ y == 16
   @test QQ(2) ^ y == 16


   println("PASS")
end

function test_acb_misc_ops()
   print("acb.misc_ops()...")

   @test ldexp(CC(3), 2) == 12
   @test ldexp(CC(3), ZZ(2)) == 12
   @test contains(trim(CC("1.1 +/- 0.001")), CC("1.1"))

   @test accuracy_bits(CC(0)) == typemax(Int)
   @test accuracy_bits(CC("+/- inf")) == -typemax(Int)
   @test accuracy_bits(CC("0.1")) > prec(CC) - 4

   uniq, n = unique_integer(CC("3 +/- 0.001"))
   @test uniq
   @test n == 3

   uniq, n = unique_integer(CC("3 +/- 1.001"))
   @test !uniq

   println("PASS")
end

function test_acb_unsafe_ops()
   print("acb.unsafe_ops()...")

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

   println("PASS")
end

function test_acb_constants()
   print("acb.constants()...")

   @test overlaps(const_pi(CC), CC("3.141592653589793238462643 +/- 4.03e-25"))

   println("PASS")
end

function test_acb_functions()
   print("acb.functions()...")

   z = CC("0.2", "0.3")
   a = CC("0.3", "0.4")
   b = CC("0.4", "0.5")

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
   @test overlaps(exppii(z), CC("0.3152424821841265534507942 +/- 6.54e-26",
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

   s, c = sincos(z)
   @test overlaps(s, CC("0.2076767030562843558332814 +/- 4.54e-26",
                              "0.2984501618819517453633022 +/- 5.06e-26"))
   @test overlaps(c, CC("1.024501340227920709172021 +/- 1.78e-25",
                              "-0.06049884291265948916305985 +/- 8.58e-27"))

   s, c = sincospi(z)
   @test overlaps(s, CC("0.868744702162250462386018 +/- 1.48e-25",
                              "0.880482019377109404605517 +/- 2.50e-25"))
   @test overlaps(c, CC("1.195724501561235958056311 +/- 4.82e-25",
                              "-0.639707632221510215793558 +/- 1.74e-25"))

   @test overlaps(sinh(z), CC("0.1923436298021928222471007 +/- 3.58e-26",
                              "0.3014503384289114663670543 +/- 5.95e-26"))
   @test overlaps(cosh(z), CC("0.9745069929868754653674075 +/- 6.93e-26",
                              "0.05949885707931208506240025 +/- 6.31e-27"))
   @test overlaps(tanh(z), CC("0.2154587730737840432610980 +/- 7.18e-26",
                              "0.2961813406783810497027117 +/- 5.25e-26"))
   @test overlaps(coth(z), CC("1.606152868815847688442798 +/- 4.94e-25",
                              "-2.207905035537343796489195 +/- 5.54e-25"))

   s, c = sinhcosh(z)
   @test overlaps(s, CC("0.1923436298021928222471007 +/- 3.58e-26",
                              "0.3014503384289114663670543 +/- 5.95e-26"))
   @test overlaps(c, CC("0.9745069929868754653674075 +/- 6.93e-26",
                              "0.05949885707931208506240025 +/- 6.31e-27"))


   @test overlaps(atan(z), CC("0.2154744937001882555778458 +/- 6.44e-26",
                              "0.2957499202364142781972578 +/- 4.56e-26"))

   @test overlaps(logsinpi(z), CC("0.212622738160453236391712 +/- 3.07e-25",
                              "0.792108066076652209962478 +/- 3.84e-25"))

   @test overlaps(gamma(z), CC("1.17074211862417715153439 +/- 3.71e-24",
                              "-2.10413807786374340593296 +/- 2.55e-24"))
   @test overlaps(rgamma(z), CC("0.201920527977481935137338 +/- 2.50e-25",
                              "0.362905429693658450479200 +/- 2.66e-25"))
   @test overlaps(lgamma(z), CC("0.878759461001381725389280 +/- 5.88e-25",
                              "-1.063052882456422220552361 +/- 4.43e-25"))
   @test overlaps(digamma(z), CC("-1.76424192368129752832045 +/- 6.55e-24",
                              "2.67409284892388122018449 +/- 5.36e-24"))
   @test overlaps(risingfac(z,4), CC("0.362100000000000000000000 +/- 1.26e-25",
                              "3.162000000000000000000000 +/- 2.52e-25"))
   @test overlaps(risingfac(z,UInt(4)), CC("0.362100000000000000000000 +/- 1.26e-25",
                              "3.162000000000000000000000 +/- 2.52e-25"))

   u, v = risingfac2(z,4)
   @test overlaps(u, CC("0.362100000000000000000000 +/- 1.21e-25",
                              "3.162000000000000000000000 +/- 1.87e-25"))
   @test overlaps(v, CC("9.316000000000000000000000 +/- 3.71e-25",
                              "8.796000000000000000000000 +/- 5.03e-25"))
   u, v = risingfac2(z,UInt(4))
   @test overlaps(u, CC("0.362100000000000000000000 +/- 1.21e-25",
                              "3.162000000000000000000000 +/- 1.87e-25"))
   @test overlaps(v, CC("9.316000000000000000000000 +/- 3.71e-25",
                              "8.796000000000000000000000 +/- 5.03e-25"))

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
   @test overlaps(barnesg(z), CC("0.207890527664830899454035 +/- 8.15e-25",
                              "0.41000425789963963393056 +/- 2.21e-24"))
   @test overlaps(logbarnesg(z), CC("-0.77718620877676355405122 +/- 6.03e-24",
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
   @test overlaps(ei(z), CC("-0.258071740310124225105760 +/- 1.86e-25",
                              "1.313158595093461484954051 +/- 4.22e-25"))
   @test overlaps(si(z), CC("0.2025575702854160799774363 +/- 7.43e-26",
                              "0.299490037406755928146125 +/- 4.33e-25"))
   @test overlaps(ci(z), CC("-0.430519178766360325396304 +/- 3.01e-25",
                              "0.952668915799813236516927 +/- 5.22e-25"))
   @test overlaps(shi(z), CC("0.1974464963284887729709696 +/- 7.58e-26",
                              "0.3004900626277844247301649 +/- 5.66e-26"))
   @test overlaps(chi(z), CC("-0.4555182366386129980767295 +/- 9.07e-26",
                              "1.012668532465677060223886 +/- 1.53e-25"))
   @test overlaps(li(z), CC("-0.00563522578699947870932 +/- 5.04e-24",
                              "2.96647757239289013174985 +/- 1.08e-24"))
   @test overlaps(lioffset(z), CC("-1.050799005904492263553913 +/- 9.59e-25",
                              "2.96647757239289013174985 +/- 1.08e-24"))
   @test overlaps(expint(a,z), CC("0.15249323509272876700176 +/- 4.10e-24",
                              "-1.34436596014834977342501 +/- 2.02e-24"))
   @test overlaps(gamma(a,z), CC("0.52015862665033430896480 +/- 6.25e-24",
                              "-0.45171359572912367448392 +/- 5.94e-24"))
   @test overlaps(besselj(a,z), CC("0.46117305056699182297843 +/- 5.61e-24",
                              "-0.172953644007776862353437 +/- 6.92e-25"))
   @test overlaps(bessely(a,z), CC("-1.18656154996325105251999 +/- 4.72e-24",
                              "0.31443655250196831955353 +/- 8.03e-24"))
   @test overlaps(besseli(a,z), CC("0.466725582357895540843371 +/- 8.51e-25",
                              "-0.150139085652146628233083 +/- 7.46e-25"))
   @test overlaps(besselk(a,z), CC("1.38540911352254124513333 +/- 5.27e-24",
                              "-0.73450089723666866121649 +/- 8.89e-24"))
   @test overlaps(hyp1f1(a,b,z), CC("1.125983781196280931011114 +/- 7.61e-25",
                              "0.283048286141523623477498 +/- 6.79e-25"))
   @test overlaps(hyp1f1r(a,b,z), CC("0.38425089179721272011261 +/- 4.66e-24",
                              "0.86691422812428398524918 +/- 1.89e-24"))

   t1, t2, t3, t4 = jtheta(z,a)
   @test overlaps(t1, CC("1.15342827918495425546807 +/- 7.18e-24",
                              "0.53226978187312777396054 +/- 4.41e-24"))
   @test overlaps(t2, CC("2.84687788483731660757883 +/- 3.56e-24",
                              "-0.25346724341130248214954 +/- 2.79e-24"))
   @test overlaps(t3, CC("2.84541084558501961355974 +/- 2.02e-24",
                              "-0.27710073395466035423707 +/- 6.30e-24"))
   @test overlaps(t4, CC("-0.66921558151467044848961 +/- 8.59e-24",
                              "0.81845615591876140504609 +/- 7.86e-24"))

   @test overlaps(modeta(z), CC("0.904816083881512590065651 +/- 9.09e-25",
                              "-0.098804163349957225969484 +/- 4.88e-25"))
   @test overlaps(modj(z), CC("-1923742.4647359951069761 +/- 8.10e-17",
                              "-474343.2637224651049865 +/- 1.22e-17"))
   @test overlaps(modlambda(z), CC("0.99856752116671778730590 +/- 4.18e-24",
                              "-0.0112661792743731125728666 +/- 5.88e-26"))
   @test overlaps(moddelta(z), CC("-0.09012304519443574525631 +/- 1.43e-24",
                              "-0.052947827926836557643152 +/- 8.29e-25"))

   @test overlaps(ellipk(z), CC("1.63015510394171138472863 +/- 3.00e-24",
                              "0.143703652492537358876625 +/- 4.78e-25"))
   @test overlaps(ellipe(z), CC("1.49751819287893089653527 +/- 4.93e-24",
                              "-0.12665473675024420800050 +/- 1.80e-24"))

   @test overlaps(ellipwp(z,a), CC("-1.35871985483098753373 +/- 1.89e-21",
                              "-51.93347883199212591376 +/- 6.19e-21"))

   println("PASS")
end

function test_acb()
   test_acb_constructors()
   test_acb_basic_ops()
   test_acb_comparison()
   test_acb_predicates()
   test_acb_unary_ops()
   test_acb_binary_ops()
   test_acb_misc_ops()
   test_acb_unsafe_ops()
   test_acb_constants()
   test_acb_functions()

   println("")
end
