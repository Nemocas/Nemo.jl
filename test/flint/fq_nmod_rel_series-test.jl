function test_fq_nmod_rel_series_constructors()
   print("fq_nmod_rel_series.constructors...")

   R, t = FiniteField(23, 5, "t")
   S, x = PowerSeriesRing(R, 30, "x")

   @test isa(S, FqNmodRelSeriesRing)

   a = x^3 + 2x + 1
   b = (t^2 + 1)*x^2 + (t + 3)x + O(x^4)

   @test isa(a, SeriesElem)
   @test isa(b, SeriesElem)

   c = S(a)
   d = S([t + 1, t, R(1)], 3, 5)

   @test isa(c, SeriesElem)
   @test isa(d, SeriesElem)

   g = S(1)
   h = S(fmpz(2))
   k = S()

   @test isa(g, SeriesElem)
   @test isa(h, SeriesElem)
   @test isa(k, SeriesElem)

   l = S(t)

   @test isa(l, SeriesElem)

   println("PASS")
end

function test_fq_nmod_rel_series_manipulation()
   print("fq_nmod_rel_series.manipulation...")

   R, t = FiniteField(23, 5, "t")
   S, x = PowerSeriesRing(R, 30, "x")
   
   @test max_precision(S) == 30

   a = 2x + x^3
   b = O(x^4)

   @test isgen(gen(S))

   @test iszero(zero(S))

   @test isone(one(S))

   @test isunit(-1 + x + 2x^2)

   @test valuation(a) == 1

   @test valuation(b) == 4

   @test precision(a) == 31

   @test precision(b) == 4

   @test isequal(deepcopy(a), a)

   @test isequal(deepcopy(b), b)

   @test length(a) == 4

   @test length(b) == 0

   @test normalise(a, 4) == 4

   @test coeff(a, 1) == 2

   @test coeff(b, 7) == 0

   println("PASS")
end

function test_fq_nmod_rel_series_unary_ops()
   print("fq_nmod_rel_series.unary_ops...")

   R, t = FiniteField(23, 5, "t")
   S, x = PowerSeriesRing(R, 30, "x")

   a = 2x + x^3
   b = 1 + 2x + x^2 + O(x^3)

   @test isequal(-a, -2x - x^3 + O(x^31))
   
   @test isequal(-b, -1 - 2x - x^2 + O(x^3))

   println("PASS")
end

function test_fq_nmod_rel_series_binary_ops()
   print("fq_nmod_rel_series.binary_ops...")

   R, t = FiniteField(23, 5, "t")
   S, x = PowerSeriesRing(R, 30, "x")

   a = 2x + x^3
   b = O(x^4)
   c = 1 + x + 3x^2 + O(x^5)
   d = x^2 + 3x^3 - x^4

   @test isequal(a + b, x^3+2*x+O(x^4))

   @test isequal(a - c, x^3-3*x^2+x-1+O(x^5))

   @test isequal(b*c, O(x^4))

   @test isequal(a*c, 3*x^5+x^4+7*x^3+2*x^2+2*x+O(x^6))

   @test isequal(a*d, -x^7+3*x^6-x^5+6*x^4+2*x^3+O(x^33))

   println("PASS")
end

function test_fq_nmod_rel_series_adhoc_binary_ops()
   print("fq_nmod_rel_series.adhoc_binary_ops...")

   R, t = FiniteField(23, 5, "t")
   S, x = PowerSeriesRing(R, 30, "x")

   a = 2x + x^3
   b = O(x^4)
   c = 1 + x + 3x^2 + O(x^5)
   d = x^2 + 3x^3 - x^4

   @test isequal(2a, 4x + 2x^3 + O(x^31))

   @test isequal(fmpz(3)*b, O(x^4))

   @test isequal(c*2, 2 + 2*x + 6*x^2 + O(x^5))

   @test isequal(d*fmpz(3), 3x^2 + 9x^3 - 3x^4 + O(x^32))

   println("PASS")
end

function test_fq_nmod_rel_series_comparison()
   print("fq_nmod_rel_series.comparison...")

   R, t = FiniteField(23, 5, "t")
   S, x = PowerSeriesRing(R, 30, "x")

   a = 2x + x^3
   b = O(x^3)
   c = 1 + x + 3x^2 + O(x^5)
   d = 3x^3 - x^4

   @test a == 2x + x^3

   @test b == d

   @test c != d

   @test isequal(a, 2x + x^3 + O(x^31))

   @test !isequal(b, d)

   println("PASS")
end

function test_fq_nmod_rel_series_adhoc_comparison()
   print("fq_nmod_rel_series.adhoc_comparison...")

   R, t = FiniteField(23, 5, "t")
   S, x = PowerSeriesRing(R, 30, "x")

   a = 2x + x^3
   b = O(x^0)
   c = 1 + O(x^5)
   d = S(3)

   @test d == 3

   @test c == fmpz(1)

   @test fmpz() != a

   @test 2 == b

   @test fmpz(1) == c

   println("PASS")
end

function test_fq_nmod_rel_series_powering()
   print("fq_nmod_rel_series.powering...")

   R, t = FiniteField(23, 5, "t")
   S, x = PowerSeriesRing(R, 30, "x")

   a = 2x + x^3
   b = O(x^4)
   c = 1 + x + 2x^2 + O(x^5)
   d = 2x + x^3 + O(x^4)

   @test isequal(a^12, x^36+24*x^34+264*x^32+1760*x^30+7920*x^28+25344*x^26+59136*x^24+101376*x^22+126720*x^20+112640*x^18+67584*x^16+24576*x^14+4096*x^12+O(x^42))

   @test isequal(b^12, O(x^48))

   @test isequal(c^12, 2079*x^4+484*x^3+90*x^2+12*x+1+O(x^5))

   @test isequal(d^12, 4096*x^12+24576*x^14+O(x^15))

   println("PASS")
end

function test_fq_nmod_rel_series_shift()
   print("fq_nmod_rel_series.shift...")

   R, t = FiniteField(23, 5, "t")
   S, x = PowerSeriesRing(R, 30, "x")

   a = 2x + x^3
   b = O(x^4)
   c = 1 + x + 2x^2 + O(x^5)
   d = 2x + x^3 + O(x^4)

   @test isequal(shift_left(a, 2), 2*x^3+x^5+O(x^33))

   @test isequal(shift_left(b, 2), O(x^6))

   @test isequal(shift_right(c, 1), 1+2*x+O(x^4))

   @test isequal(shift_right(d, 3), 1+O(x^1))

   println("PASS")
end

function test_fq_nmod_rel_series_truncation()
   print("fq_nmod_rel_series.truncation...")

   R, t = FiniteField(23, 5, "t")
   S, x = PowerSeriesRing(R, 30, "x")

   a = 2x + x^3
   b = O(x^4)
   c = 1 + x + 2x^2 + O(x^5)
   d = 2x + x^3 + O(x^4)

   @test isequal(truncate(a, 3), 2*x + O(x^3))

   @test isequal(truncate(b, 2), O(x^2))

   @test isequal(truncate(c, 5), 2*x^2+x+1+O(x^5))

   @test isequal(truncate(d, 5), x^3+2*x+O(x^4))

   println("PASS")
end

function test_fq_nmod_rel_series_inversion()
   print("fq_nmod_rel_series.inversion...")

   R, t = FiniteField(23, 5, "t")
   S, x = PowerSeriesRing(R, 30, "x")

   a = 1 + x + 2x^2 + O(x^5)
   b = S(-1)

   @test isequal(inv(a), -x^4+3*x^3-x^2-x+1+O(x^5))

   @test isequal(inv(b), -1+O(x^30))

   println("PASS")
end

function test_fq_nmod_rel_series_exact_division()
   print("fq_nmod_rel_series.exact_division...")

   R, t = FiniteField(23, 5, "t")
   S, x = PowerSeriesRing(R, 30, "x")

   a = x + x^3
   b = O(x^4)
   c = 1 + x + 2x^2 + O(x^5)
   d = x + x^3 + O(x^6)

   @test isequal(divexact(a, d), 1+O(x^5))

   @test isequal(divexact(d, a), 1+O(x^5))

   @test isequal(divexact(b, c), O(x^4))

   @test isequal(divexact(d, c), -2*x^5+2*x^4-x^2+x+O(x^6))

   println("PASS")
end

function test_fq_nmod_rel_series_adhoc_exact_division()
   print("fq_nmod_rel_series.adhoc_exact_division...")

   R, t = FiniteField(23, 5, "t")
   S, x = PowerSeriesRing(R, 30, "x")

   a = x + x^3
   b = O(x^4)
   c = 1 + x + 2x^2 + O(x^5)
   d = x + x^3 + O(x^6)

   @test isequal(divexact(a, 7), 10*x+10*x^3+O(x^31))

   @test isequal(divexact(b, fmpz(11)), 0+O(x^4))

   @test isequal(divexact(c, fmpz(2)), 12 + 12*x+x^2+O(x^5))

   @test isequal(divexact(d, 9), 18*x+18*x^3+O(x^6))

   @test isequal(divexact(94872394861923874346987123694871329847a, 94872394861923874346987123694871329847), a)

   @test isequal(divexact((t + 1)*a, t + 1), a)

   println("PASS")
end

function test_fq_nmod_rel_series_special_functions()
   print("fq_nmod_rel_series.special_functions...")

   R, t = FiniteField(23, 5, "t")
   S, x = PowerSeriesRing(R, 30, "x")

   @test isequal(exp(x + O(x^10)), 18*x^9+x^8+8*x^7+10*x^6+14*x^5+x^4+4*x^3+12*x^2+x+1+O(x^10))

   @test isequal(divexact(x, exp(x + O(x^10)) - 1), 13*x^8+9*x^6+13*x^4+2*x^2+11*x+1+O(x^9))

   println("PASS")
end

function test_fq_nmod_rel_series()
   test_fq_nmod_rel_series_constructors()
   test_fq_nmod_rel_series_manipulation()
   test_fq_nmod_rel_series_unary_ops()
   test_fq_nmod_rel_series_binary_ops()
   test_fq_nmod_rel_series_adhoc_binary_ops()
   test_fq_nmod_rel_series_comparison()
   test_fq_nmod_rel_series_adhoc_comparison()
   test_fq_nmod_rel_series_powering()
   test_fq_nmod_rel_series_shift()
   test_fq_nmod_rel_series_truncation()
   test_fq_nmod_rel_series_exact_division()
   test_fq_nmod_rel_series_adhoc_exact_division()
   test_fq_nmod_rel_series_inversion()
   test_fq_nmod_rel_series_special_functions()

   println("")
end
