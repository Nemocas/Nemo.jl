function test_fmpq_rel_series_constructors()
   print("fmpq_rel_series.constructors...")

   R, x = PowerSeriesRing(QQ, 30, "x")

   @test isa(R, FmpqRelSeriesRing)

   a = x^3 + 2x + 1
   b = x^2 + 3x + O(x^4)

   @test isa(R(a), SeriesElem)

   @test isa(R([fmpz(1), fmpz(2), fmpq(3)], 3, 5), SeriesElem)

   @test isa(R([fmpq(1), fmpq(2), fmpq(3)], 3, 3), SeriesElem)

   @test isa(R(1), SeriesElem)

   @test isa(R(fmpz(2)), SeriesElem)

   @test isa(R(fmpq(2)), SeriesElem)

   @test isa(R(), SeriesElem)

   println("PASS")
end

function test_fmpq_rel_series_manipulation()
   print("fmpq_rel_series.manipulation...")

   R, x = PowerSeriesRing(QQ, 30, "x")

   a = 2x + x^3
   b = O(x^4)

   @test isgen(gen(R))

   @test iszero(zero(R))

   @test isone(one(R))

   @test isunit(-1 + x + 2x^2)

   @test valuation(a) == 1

   @test valuation(b) == 4

   println("PASS")
end

function test_fmpq_rel_series_unary_ops()
   print("fmpq_rel_series.unary_ops...")

   R, x = PowerSeriesRing(QQ, 30, "x")

   a = 2x + x^3
   b = 1 + 2x + x^2 + O(x^3)

   @test -a == -2x - x^3
   
   @test -b == -1 - 2x - x^2 + O(x^3)

   println("PASS")
end

function test_fmpq_rel_series_binary_ops()
   print("fmpq_rel_series.binary_ops...")

   R, x = PowerSeriesRing(QQ, 30, "x")

   a = 2x + x^3
   b = O(x^4)
   c = 1 + x + 3x^2 + O(x^5)
   d = x^2 + 3x^3 - x^4

   @test a + b == x^3+2*x+O(x^4)

   @test a - c == x^3-3*x^2+x-1+O(x^5)

   @test b*c == O(x^4)

   @test a*c == 3*x^5+x^4+7*x^3+2*x^2+2*x+O(x^6)

   @test a*d == -x^7+3*x^6-x^5+6*x^4+2*x^3

   println("PASS")
end

function test_fmpq_rel_series_adhoc_binary_ops()
   print("fmpq_rel_series.adhoc_binary_ops...")

   R, x = PowerSeriesRing(QQ, 30, "x")

   a = 2x + x^3
   b = O(x^4)
   c = 1 + x + 3x^2 + O(x^5)
   d = x^2 + 3x^3 - x^4

   @test 2a == 4x + 2x^3

   @test ZZ(3)*b == O(x^4)

   @test c*2 == 2 + 2*x + 6*x^2 + O(x^5)

   @test d*ZZ(3) == 3x^2 + 9x^3 - 3x^4

   @test c*fmpq(2, 3) == 2*x^2 + fmpz(2)//3*x + fmpz(2)//3+O(x^5)

   @test fmpq(2, 3)*c == 2*x^2 + fmpz(2)//3*x + fmpz(2)//3+O(x^5)

   println("PASS")
end

function test_fmpq_rel_series_comparison()
   print("fmpq_rel_series.comparison...")

   R, x = PowerSeriesRing(QQ, 30, "x")

   a = 2x + x^3
   b = O(x^3)
   c = 1 + x + 3x^2 + O(x^5)
   d = 3x^3 - x^4

   @test a == 2x + x^3

   @test b == d

   @test c != d

   println("PASS")
end

function test_fmpq_rel_series_adhoc_comparison()
   print("fmpq_rel_series.adhoc_comparison...")

   R, x = PowerSeriesRing(QQ, 30, "x")

   a = 2x + x^3
   b = O(x^0)
   c = 1 + O(x^5)
   d = R(3)

   @test d == 3

   @test c == ZZ(1)

   @test ZZ(0) != a

   @test 2 == b

   @test ZZ(1) == c

   println("PASS")
end

function test_fmpq_rel_series_powering()
   print("fmpq_rel_series.powering...")

   R, x = PowerSeriesRing(QQ, 30, "x")

   a = 2x + x^3
   b = O(x^4)
   c = 1 + x + 2x^2 + O(x^5)
   d = 2x + x^3 + O(x^4)

   @test a^12 == x^36+24*x^34+264*x^32+1760*x^30+7920*x^28+25344*x^26+59136*x^24+101376*x^22+126720*x^20+112640*x^18+67584*x^16+24576*x^14+4096*x^12

   @test b^12 == O(x^48)

   @test c^12 == 2079*x^4+484*x^3+90*x^2+12*x+1+O(x^5)

   @test d^12 == 4096*x^12+24576*x^14+O(x^15)

   println("PASS")
end

function test_fmpq_rel_series_shift()
   print("fmpq_rel_series.shift...")

   R, x = PowerSeriesRing(QQ, 30, "x")

   a = 2x + x^3
   b = O(x^4)
   c = 1 + x + 2x^2 + O(x^5)
   d = 2x + x^3 + O(x^4)

   @test shift_left(a, 2) == 2*x^3+x^5

   @test shift_left(b, 2) == O(x^6)

   @test shift_right(c, 1) == 1+2*x+O(x^4)

   @test shift_right(d, 3) == 1+O(x^1)

   println("PASS")
end

function test_fmpq_rel_series_truncation()
   print("fmpq_rel_series.truncation...")

   R, x = PowerSeriesRing(QQ, 30, "x")

   a = 2x + x^3
   b = O(x^4)
   c = 1 + x + 2x^2 + O(x^5)
   d = 2x + x^3 + O(x^4)

   @test truncate(a, 3) == 2*x + O(x^3)

   @test truncate(b, 2) == O(x^2)

   @test truncate(c, 5) == 2*x^2+x+1+O(x^5)

   @test truncate(d, 5) == x^3+2*x+O(x^4)

   println("PASS")
end

function test_fmpq_rel_series_exact_division()
   print("fmpq_rel_series.exact_division...")

   R, x = PowerSeriesRing(QQ, 30, "x")

   a = x + x^3
   b = O(x^4)
   c = 1 + x + 2x^2 + O(x^5)
   d = x + x^3 + O(x^6)

   @test divexact(a, d) == 1+O(x^5)

   @test divexact(d, a) == 1+O(x^5)

   @test divexact(b, c) == O(x^4)

   @test divexact(d, c) == -2*x^5+2*x^4-x^2+x+O(x^6)

   println("PASS")
end

function test_fmpq_rel_series_adhoc_exact_division()
   print("fmpq_rel_series.adhoc_exact_division...")

   R, x = PolynomialRing(QQ, "x")
   
   a = x + x^3
   b = O(x^4)
   c = 1 + x + 2x^2 + O(x^5)
   d = x + x^3 + O(x^6)

   @test isequal(divexact(7a, 7), a)

   @test isequal(divexact(11b, fmpz(11)), b)

   @test isequal(divexact(2c, fmpz(2)), c)

   @test isequal(divexact(9d, 9), d)

   @test isequal(divexact(94872394861923874346987123694871329847a, 94872394861923874346987123694871329847), a)

   println("PASS")
end

function test_fmpq_rel_series_inversion()
   print("fmpq_rel_series.inversion...")

   R, x = PowerSeriesRing(QQ, 30, "x")

   a = 1 + x + 2x^2 + O(x^5)
   b = R(-1)

   @test inv(a) == -x^4+3*x^3-x^2-x+1+O(x^5)

   @test inv(b) == -1

   println("PASS")
end

function test_fmpq_rel_series_special()
   print("fmpq_rel_series.special...")
 
   R, x = PowerSeriesRing(QQ, 30, "x")

   a = 1 + x + 3x^2 + O(x^5)
   b = x + 2x^2 + 5x^3 + O(x^5)

   @test sqrt(a^2) == a
   @test log(exp(b)) == b
   @test asin(sin(b)) == b
   @test atan(tan(b)) == b
   @test sin(b)^2 + cos(b)^2 == 1 + O(x^5)
   @test asinh(sinh(b)) == b
   @test atanh(tanh(b)) == b
   @test cosh(b)^2 - sinh(b)^2 == 1 + O(x^5)

   println("PASS")
end

function test_fmpq_rel_series()
   test_fmpq_rel_series_constructors()
   test_fmpq_rel_series_manipulation()
   test_fmpq_rel_series_unary_ops()
   test_fmpq_rel_series_binary_ops()
   test_fmpq_rel_series_adhoc_binary_ops()
   test_fmpq_rel_series_comparison()
   test_fmpq_rel_series_adhoc_comparison()
   test_fmpq_rel_series_powering()
   test_fmpq_rel_series_shift()
   test_fmpq_rel_series_truncation()
   test_fmpq_rel_series_exact_division()
   test_fmpq_rel_series_inversion()
   test_fmpq_rel_series_special()

   println("")
end
