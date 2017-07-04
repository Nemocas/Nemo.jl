function test_fq_nmod_abs_series_constructors()
   print("fq_nmod_abs_series.constructors...")

   S, t = FiniteField(23, 5, "t")
   R, x = PowerSeriesRing(S, 30, "x", model=:capped_absolute)

   @test elem_type(R) == fq_nmod_abs_series
   @test elem_type(FqNmodAbsSeriesRing) == fq_nmod_abs_series
   @test parent_type(fq_nmod_abs_series) == FqNmodAbsSeriesRing

   @test isa(R, FqNmodAbsSeriesRing)

   a = x^3 + 2x + 1
   b = x^2 + 3x + O(x^4)

   @test isa(R(a), SeriesElem)

   @test isa(R([S(1), S(2), S(3)], 3, 5), SeriesElem)

   @test isa(R([S(1), S(2), S(3)], 3, 3), SeriesElem)

   @test isa(R(1), SeriesElem)

   @test isa(R(ZZ(2)), SeriesElem)

   @test isa(R(), SeriesElem)

   println("PASS")
end

function test_fq_nmod_abs_series_printing()
   print("fq_nmod_abs_series.printing...")

   S, t = FiniteField(23, 5, "t")
   R, x = PowerSeriesRing(S, 30, "x", model=:capped_absolute)

   b = x^2 + 3x + O(x^4)

   @test string(b) == "(3)*x+x^2+O(x^4)"

   println("PASS")
end

function test_fq_nmod_abs_series_manipulation()
   print("fq_nmod_abs_series.manipulation...")

   S, t = FiniteField(23, 5, "t")
   R, x = PowerSeriesRing(S, 30, "x", model=:capped_absolute)

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

function test_fq_nmod_abs_series_unary_ops()
   print("fq_nmod_abs_series.unary_ops...")

   S, t = FiniteField(23, 5, "t")
   R, x = PowerSeriesRing(S, 30, "x", model=:capped_absolute)

   a = 2x + x^3
   b = 1 + 2x + x^2 + O(x^3)

   @test -a == -2x - x^3
   
   @test -b == -1 - 2x - x^2 + O(x^3)

   println("PASS")
end

function test_fq_nmod_abs_series_binary_ops()
   print("fq_nmod_abs_series.binary_ops...")

   S, t = FiniteField(23, 5, "t")
   R, x = PowerSeriesRing(S, 30, "x", model=:capped_absolute)

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

function test_fq_nmod_abs_series_adhoc_binary_ops()
   print("fq_nmod_abs_series.adhoc_binary_ops...")

   S, t = FiniteField(23, 5, "t")
   R, x = PowerSeriesRing(S, 30, "x", model=:capped_absolute)

   a = 2x + x^3
   b = O(x^4)
   c = 1 + x + 3x^2 + O(x^5)
   d = x^2 + 3x^3 - x^4

   @test 2a == 4x + 2x^3

   @test ZZ(3)*b == O(x^4)

   @test c*2 == 2 + 2*x + 6*x^2 + O(x^5)

   @test d*ZZ(3) == 3x^2 + 9x^3 - 3x^4

   F, x = FiniteField(23, 1, "x")
   R, t = PowerSeriesRing(F, 6, "t", model=:capped_absolute)

   @test F(1) * t == t

   println("PASS")
end

function test_fq_nmod_abs_series_comparison()
   print("fq_nmod_abs_series.comparison...")

   S, t = FiniteField(23, 5, "t")
   R, x = PowerSeriesRing(S, 30, "x", model=:capped_absolute)

   a = 2x + x^3
   b = O(x^3)
   c = 1 + x + 3x^2 + O(x^5)
   d = 3x^3 - x^4

   @test a == 2x + x^3

   @test b == d

   @test c != d

   println("PASS")
end

function test_fq_nmod_abs_series_adhoc_comparison()
   print("fq_nmod_abs_series.adhoc_comparison...")

   S, t = FiniteField(23, 5, "t")
   R, x = PowerSeriesRing(S, 30, "x", model=:capped_absolute)

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

function test_fq_nmod_abs_series_powering()
   print("fq_nmod_abs_series.powering...")

   S, t = FiniteField(23, 5, "t")
   R, x = PowerSeriesRing(S, 30, "x", model=:capped_absolute)

   a = 2x + x^3
   b = O(x^4)
   c = 1 + x + 2x^2 + O(x^5)
   d = 2x + x^3 + O(x^4)

   @test a^12 == x^36+24*x^34+264*x^32+1760*x^30+7920*x^28+25344*x^26+59136*x^24+101376*x^22+126720*x^20+112640*x^18+67584*x^16+24576*x^14+4096*x^12 + O(x^30)

   @test b^12 == O(x^30)

   @test c^12 == 2079*x^4+484*x^3+90*x^2+12*x+1+O(x^5)

   @test d^12 == 4096*x^12+24576*x^14+O(x^15)

   println("PASS")
end

function test_fq_nmod_abs_series_shift()
   print("fq_nmod_abs_series.shift...")

   S, t = FiniteField(23, 5, "t")
   R, x = PowerSeriesRing(S, 30, "x", model=:capped_absolute)

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

function test_fq_nmod_abs_series_truncation()
   print("fq_nmod_abs_series.truncation...")

   S, t = FiniteField(23, 5, "t")
   R, x = PowerSeriesRing(S, 30, "x", model=:capped_absolute)

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

function test_fq_nmod_abs_series_exact_division()
   print("fq_nmod_abs_series.exact_division...")

   S, t = FiniteField(23, 5, "t")
   R, x = PowerSeriesRing(S, 30, "x", model=:capped_absolute)

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

function test_fq_nmod_abs_series_adhoc_exact_division()
   print("fq_nmod_abs_series.adhoc_exact_division...")

   S, t = FiniteField(23, 5, "t")
   R, x = PolynomialRing(ZZ, "x", model=:capped_absolute)
   
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

function test_fq_nmod_abs_series_inversion()
   print("fq_nmod_abs_series.inversion...")

   S, t = FiniteField(23, 5, "t")
   R, x = PowerSeriesRing(S, 30, "x", model=:capped_absolute)

   a = 1 + x + 2x^2 + O(x^5)
   b = R(-1)

   @test inv(a) == -x^4+3*x^3-x^2-x+1+O(x^5)

   @test inv(b) == -1

   println("PASS")
end

function test_fq_nmod_abs_series()
   test_fq_nmod_abs_series_constructors()
   test_fq_nmod_abs_series_printing()
   test_fq_nmod_abs_series_manipulation()
   test_fq_nmod_abs_series_unary_ops()
   test_fq_nmod_abs_series_binary_ops()
   test_fq_nmod_abs_series_adhoc_binary_ops()
   test_fq_nmod_abs_series_comparison()
   test_fq_nmod_abs_series_adhoc_comparison()
   test_fq_nmod_abs_series_powering()
   test_fq_nmod_abs_series_shift()
   test_fq_nmod_abs_series_truncation()
   test_fq_nmod_abs_series_exact_division()
   test_fq_nmod_abs_series_inversion()

   println("")
end
