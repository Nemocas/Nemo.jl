function test_abs_series_constructors()
   print("Generic.AbsSeries.constructors...")

   R, t = PolynomialRing(QQ, "t")
   S, x = PowerSeriesRing(R, 30, "x", model=:capped_absolute)

   @test elem_type(S) == Generic.AbsSeries{elem_type(R)}
   @test elem_type(Generic.AbsSeriesRing{elem_type(R)}) == Generic.AbsSeries{elem_type(R)}
   @test parent_type(Generic.AbsSeries{elem_type(R)}) == Generic.AbsSeriesRing{elem_type(R)}

   @test isa(S, Generic.AbsSeriesRing)

   a = x^3 + 2x + 1
   b = (t^2 + 1)*x^2 + (t + 3)x + O(x^4)

   @test isa(a, AbsSeriesElem)
   @test isa(b, AbsSeriesElem)

   c = S(a)
   d = S([t + 1, t, R(1)], 3, 5)

   @test isa(c, AbsSeriesElem)
   @test isa(d, AbsSeriesElem)

   g = S(1)
   h = S(fmpz(2))
   k = S()

   @test isa(g, AbsSeriesElem)
   @test isa(h, AbsSeriesElem)
   @test isa(k, AbsSeriesElem)

   l = S(t)

   @test isa(l, AbsSeriesElem)

   println("PASS")
end

function test_abs_series_manipulation()
   print("Generic.AbsSeries.manipulation...")

   R, t = PolynomialRing(QQ, "t")
   S, x = PowerSeriesRing(R, 30, "x", model=:capped_absolute)
   
   @test max_precision(S) == 30

   a = 2x + x^3
   b = O(x^4)

   @test isgen(gen(S))

   @test iszero(zero(S))

   @test isone(one(S))

   @test isunit(-1 + x + 2x^2)

   @test valuation(a) == 1

   @test valuation(b) == 4

   @test precision(a) == 30

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

function test_abs_series_unary_ops()
   print("Generic.AbsSeries.unary_ops...")

   R, t = PolynomialRing(QQ, "t")
   S, x = PowerSeriesRing(R, 30, "x", model=:capped_absolute)

   a = 2x + x^3
   b = 1 + 2x + x^2 + O(x^3)

   @test isequal(-a, -2x - x^3 + O(x^30))
   
   @test isequal(-b, -1 - 2x - x^2 + O(x^3))

   println("PASS")
end

function test_abs_series_binary_ops()
   print("Generic.AbsSeries.binary_ops...")

   R, t = PolynomialRing(QQ, "t")
   S, x = PowerSeriesRing(R, 30, "x", model=:capped_absolute)

   a = 2x + x^3
   b = O(x^4)
   c = 1 + x + 3x^2 + O(x^5)
   d = x^2 + 3x^3 - x^4

   @test isequal(a + b, x^3+2*x+O(x^4))

   @test isequal(a - c, x^3-3*x^2+x-1+O(x^5))

   @test isequal(b*c, O(x^4))

   @test isequal(a*c, 3*x^5+x^4+7*x^3+2*x^2+2*x+O(x^6))

   @test isequal(a*d, -x^7+3*x^6-x^5+6*x^4+2*x^3+O(x^30))

   println("PASS")
end

function test_abs_series_adhoc_binary_ops()
   print("Generic.AbsSeries.adhoc_binary_ops...")

   R, t = PolynomialRing(QQ, "t")
   S, x = PowerSeriesRing(R, 30, "x", model=:capped_absolute)

   a = 2x + x^3
   b = O(x^4)
   c = 1 + x + 3x^2 + O(x^5)
   d = x^2 + 3x^3 - x^4

   @test isequal(2a, 4x + 2x^3 + O(x^30))

   @test isequal(fmpz(3)*b, O(x^4))

   @test isequal(c*2, 2 + 2*x + 6*x^2 + O(x^5))

   @test isequal(d*fmpz(3), 3x^2 + 9x^3 - 3x^4 + O(x^30))

   println("PASS")
end

function test_abs_series_comparison()
   print("Generic.AbsSeries.comparison...")

   R, t = PolynomialRing(QQ, "t")
   S, x = PowerSeriesRing(R, 30, "x", model=:capped_absolute)

   a = 2x + x^3
   b = O(x^3)
   c = 1 + x + 3x^2 + O(x^5)
   d = 3x^3 - x^4

   @test a == 2x + x^3

   @test b == d

   @test c != d

   @test isequal(a, 2x + x^3 + O(x^30))

   @test !isequal(b, d)

   println("PASS")
end

function test_abs_series_adhoc_comparison()
   print("Generic.AbsSeries.adhoc_comparison...")

   R, t = PolynomialRing(QQ, "t")
   S, x = PowerSeriesRing(R, 30, "x", model=:capped_absolute)

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

function test_abs_series_powering()
   print("Generic.AbsSeries.powering...")

   R, t = PolynomialRing(QQ, "t")
   S, x = PowerSeriesRing(R, 30, "x", model=:capped_absolute)

   a = 2x + x^3
   b = O(x^4)
   c = 1 + x + 2x^2 + O(x^5)
   d = 2x + x^3 + O(x^4)

   @test isequal(a^12, x^36+24*x^34+264*x^32+1760*x^30+7920*x^28+25344*x^26+59136*x^24+101376*x^22+126720*x^20+112640*x^18+67584*x^16+24576*x^14+4096*x^12+O(x^30))

   @test isequal(b^12, O(x^30))

   @test isequal(c^12, 2079*x^4+484*x^3+90*x^2+12*x+1+O(x^5))

   @test isequal(d^12, 4096*x^12+24576*x^14+O(x^15))

   println("PASS")
end

function test_abs_series_shift()
   print("Generic.AbsSeries.shift...")

   R, t = PolynomialRing(QQ, "t")
   S, x = PowerSeriesRing(R, 30, "x", model=:capped_absolute)

   a = 2x + x^3
   b = O(x^4)
   c = 1 + x + 2x^2 + O(x^5)
   d = 2x + x^3 + O(x^4)

   @test isequal(shift_left(a, 2), 2*x^3+x^5+O(x^30))

   @test isequal(shift_left(b, 2), O(x^6))

   @test isequal(shift_right(c, 1), 1+2*x+O(x^4))

   @test isequal(shift_right(d, 3), 1+O(x^1))

   println("PASS")
end

function test_abs_series_truncation()
   print("Generic.AbsSeries.truncation...")

   R, t = PolynomialRing(QQ, "t")
   S, x = PowerSeriesRing(R, 30, "x", model=:capped_absolute)

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

function test_abs_series_inversion()
   print("Generic.AbsSeries.inversion...")

   R, t = PolynomialRing(QQ, "t")
   S, x = PowerSeriesRing(R, 30, "x", model=:capped_absolute)

   a = 1 + x + 2x^2 + O(x^5)
   b = S(-1)

   @test isequal(inv(a), -x^4+3*x^3-x^2-x+1+O(x^5))

   @test isequal(inv(b), -1+O(x^30))

   println("PASS")
end

function test_abs_series_exact_division()
   print("Generic.AbsSeries.exact_division...")

   R, t = PolynomialRing(QQ, "t")
   S, x = PowerSeriesRing(R, 30, "x", model=:capped_absolute)

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

function test_abs_series_adhoc_exact_division()
   print("Generic.AbsSeries.adhoc_exact_division...")

   R, t = PolynomialRing(QQ, "t")
   S, x = PowerSeriesRing(R, 30, "x", model=:capped_absolute)

   a = x + x^3
   b = O(x^4)
   c = 1 + x + 2x^2 + O(x^5)
   d = x + x^3 + O(x^6)

   @test isequal(divexact(a, 7), fmpz(1)//7*x+fmpz(1)//7*x^3+O(x^30))

   @test isequal(divexact(b, fmpz(11)), 0+O(x^4))

   @test isequal(divexact(c, fmpz(2)), fmpz(1)//2+fmpz(1)//2*x+x^2+O(x^5))

   @test isequal(divexact(d, 9), fmpz(1)//9*x+fmpz(1)//9*x^3+O(x^6))

   @test isequal(divexact(94872394861923874346987123694871329847a, 94872394861923874346987123694871329847), a)

   @test isequal(divexact((t + 1)*a, t + 1), a)

   println("PASS")
end

function test_abs_series_special_functions()
   print("Generic.AbsSeries.special_functions...")

   R = ResidueRing(ZZ, 17)
   T, t = PolynomialRing(R, "t")
   S, x = PowerSeriesRing(T, 30, "x", model=:capped_absolute)

   @test isequal(exp(x + O(x^10)),  8*x^9+4*x^8+15*x^7+3*x^6+x^5+5*x^4+3*x^3+9*x^2+x+1+O(x^10))

   @test isequal(divexact(x, exp(x + O(x^10)) - 1), x^8+11*x^6+14*x^4+10*x^2+8*x+1+O(x^9))

   println("PASS")
end

function test_gen_abs_series()
   test_abs_series_constructors()
   test_abs_series_manipulation()
   test_abs_series_unary_ops()
   test_abs_series_binary_ops()
   test_abs_series_adhoc_binary_ops()
   test_abs_series_comparison()
   test_abs_series_adhoc_comparison()
   test_abs_series_powering()
   test_abs_series_shift()
   test_abs_series_truncation()
   test_abs_series_exact_division()
   test_abs_series_adhoc_exact_division()
   test_abs_series_inversion()
   test_abs_series_special_functions()

   println("")
end
