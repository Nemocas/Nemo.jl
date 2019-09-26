@testset "fmpq_rel_series.constructors..." begin
   R, x = PowerSeriesRing(QQ, 30, "x")

   @test elem_type(R) == fmpq_rel_series
   @test elem_type(FmpqRelSeriesRing) == fmpq_rel_series
   @test parent_type(fmpq_rel_series) == FmpqRelSeriesRing

   @test isa(R, FmpqRelSeriesRing)

   a = x^3 + 2x + 1
   b = x^2 + 3x + O(x^4)

   @test isa(R(a), SeriesElem)

   @test isa(R([fmpz(1), fmpz(2), fmpq(3)], 3, 5, 0), SeriesElem)

   @test isa(R([fmpz(1), 2, 3], 3, 3, 0), SeriesElem)

   @test isa(R([BigInt(1), 2, 3], 3, 3, 0), SeriesElem)

   @test isa(R([1//1, 2, 3], 3, 3, 0), SeriesElem)

   @test isa(R([BigInt(1)//1, 2, 3], 3, 3, 0), SeriesElem)

   @test isa(R(1), SeriesElem)

   @test isa(R(fmpz(2)), SeriesElem)

   @test isa(R(BigInt(2)), SeriesElem)

   @test isa(R(fmpq(2)), SeriesElem)

   @test isa(R(2//1), SeriesElem)

   @test isa(R(BigInt(2)//1), SeriesElem)

   @test isa(R(), SeriesElem)
end

@testset "fmpq_rel_series.printing..." begin
   R, x = PowerSeriesRing(QQ, 30, "x")

   a = x^3 + 2x + 1

   @test string(a) == "1+2*x+x^3+O(x^30)"
end

@testset "fmpq_rel_series.manipulation..." begin
   R, x = PowerSeriesRing(QQ, 30, "x")

   a = 2x + x^3
   b = O(x^4)

   @test isgen(gen(R))

   @test iszero(zero(R))

   @test isone(one(R))

   @test isunit(-1 + x + 2x^2)

   @test valuation(a) == 1

   @test valuation(b) == 4
end

@testset "fmpq_rel_series.unary_ops..." begin
   R, x = PowerSeriesRing(QQ, 30, "x")

   a = 2x + x^3
   b = 1 + 2x + x^2 + O(x^3)

   @test -a == -2x - x^3

   @test -b == -1 - 2x - x^2 + O(x^3)
end

@testset "fmpq_rel_series.binary_ops..." begin
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

   f1 = 1 + x + x^2 + x^3
   f2 = x + x^2
   f3 = x + x^2 + x^3
   f4 = x^2 + x^3 + x^4 + x^5

   @test f1 + f1 == 2+2*x+2*x^2+2*x^3+O(x^30)

   @test f1 + f2 == 1+2*x+2*x^2+x^3+O(x^30)
   @test f2 + f1 == f1 + f2

   @test f1 + f3 == 1+2*x+2*x^2+2*x^3+O(x^30)
   @test f3 + f1 == f1 + f3

   @test f1 + f4 == 1+x+2*x^2+2*x^3+x^4+x^5+O(x^30)
   @test f4 + f1 == f1 + f4

   @test f1 - f1 == 0+O(x^30)

   @test f1 - f2 == 1+x^3+O(x^30)

   @test f1 - f3 == 1+O(x^30)

   @test f1 - f4 == 1+x-x^4-x^5+O(x^30)

   g1 = x^2*f1
   g2 = x^2*f2
   g3 = x^2*f3
   g4 = x^2*f4

   @test g1 + g1 == 2*x^2+2*x^3+2*x^4+2*x^5+O(x^32)

   @test g1 + g2 == x^2+2*x^3+2*x^4+x^5+O(x^32)
   @test g2 + g1 == g1 + g2

   @test g1 + g3 == x^2+2*x^3+2*x^4+2*x^5+O(x^32)
   @test g3 + g1 == g1 + g3

   @test g1 + g4 == x^2+x^3+2*x^4+2*x^5+x^6+x^7+O(x^32)
   @test g4 + g1 == g1 + g4

   @test g1 - g1 == 0+O(x^32)

   @test g1 - g2 == x^2+x^5+O(x^32)
   @test g2 - g1 == -(g1 - g2)

   @test g1 - g3 == x^2+O(x^32)
   @test g3 - g1 == -(g1 - g3)

   @test g1 - g4 == x^2+x^3-x^6-x^7+O(x^32)
   @test g4 - g1 == -(g1 - g4)

   h1 = f1
   h2 = -f2
   h3 = -f3
   h4 = -f4

   @test h1 + h2 == 1+x^3+O(x^30)
   @test h2 + h1 == h1 + h2

   @test h1 + h3 == 1+O(x^30)
   @test h3 + h1 == h1 + h3

   @test h1 + h4 == 1+x-x^4-x^5+O(x^30)
   @test h4 + h1 == h1 + h4

   @test h1 - h2 == 1+2*x+2*x^2+x^3+O(x^30)
   @test h2 - h1 == -(h1 - h2)

   @test h1 - h3 == 1+2*x+2*x^2+2*x^3+O(x^30)
   @test h3 - h1 == -(h1 - h3)

   @test h1 - h4 == 1+x+2*x^2+2*x^3+x^4+x^5+O(x^30)
   @test h4 - h1 == -(h1 - h4)

   k1 = g1
   k2 = -g2
   k3 = -g3
   k4 = -g4

   @test k1 + k2 == x^2+x^5+O(x^32)
   @test k2 + k1 == k1 + k2

   @test k1 + k3 == x^2+O(x^32)
   @test k3 + k1 == k1 + k3

   @test k1 + k4 == x^2+x^3-x^6-x^7+O(x^32)
   @test k4 + k1 == k1 + k4

   @test k1 - k2 == x^2+2*x^3+2*x^4+x^5+O(x^32)
   @test k2 - k1 == -(k1 - k2)

   @test k1 - k3 == x^2+2*x^3+2*x^4+2*x^5+O(x^32)
   @test k3 - k1 == -(k1 - k3)

   @test k1 - k4 == x^2+x^3+2*x^4+2*x^5+x^6+x^7+O(x^32)
   @test k4 - k1 == -(k1 - k4)

   m1 = 1 + x + x^2 + x^3 + O(x^4)
   m2 = x + x^2 + O(x^3)
   m3 = x + x^2 + x^3 + O(x^4)
   m4 = x^2 + x^3 + x^4 + x^5 + O(x^6)

   @test isequal(m1 + m1, 2+2*x+2*x^2+2*x^3+O(x^4))

   @test isequal(m1 + m2, 1+2*x+2*x^2+O(x^3))

   @test isequal(m1 + m3, 1+2*x+2*x^2+2*x^3+O(x^4))

   @test isequal(m1 + m4, 1+x+2*x^2+2*x^3+O(x^4))

   @test isequal(m1 - m1, 0+O(x^4))

   @test isequal(m1 - m2, 1+O(x^3))

   @test isequal(m1 - m3, 1+O(x^4))

   @test isequal(m1 - m4, 1+x+O(x^4))
end

@testset "fmpq_rel_series.adhoc_binary_ops..." begin
   R, x = PowerSeriesRing(QQ, 30, "x")

   a = 2x + x^3
   b = O(x^4)
   c = 1 + x + 3x^2 + O(x^5)
   d = x^2 + 3x^3 - x^4

   for T in [Int, BigInt, Rational{Int}, Rational{BigInt}, fmpz, fmpq]
      @test T(2) + a == 2x + x^3 + 2
      @test a + T(2) == 2x + x^3 + 2
   end

   for T in [Int, BigInt, Rational{Int}, Rational{BigInt}, fmpz, fmpq]
     @test T(2) * a == 4x + 2x^3
     @test a * T(2) == 4x + 2x^3
   end

   @test ZZ(3)*b == O(x^4)

   @test c*2 == 2 + 2*x + 6*x^2 + O(x^5)

   for T in [Int, BigInt, Rational{Int}, Rational{BigInt}, fmpz, fmpq]
     @test d * T(3) == 3x^2 + 9x^3 - 3x^4
     @test T(3) * d== 3x^2 + 9x^3 - 3x^4
   end

   @test c*fmpq(2, 3) == 2*x^2 + fmpz(2)//3*x + fmpz(2)//3+O(x^5)

   @test fmpq(2, 3)*c == 2*x^2 + fmpz(2)//3*x + fmpz(2)//3+O(x^5)
end

@testset "fmpq_rel_series.comparison..." begin
   R, x = PowerSeriesRing(QQ, 30, "x")

   a = 2x + x^3
   b = O(x^3)
   c = 1 + x + 3x^2 + O(x^5)
   d = 3x^3 - x^4

   @test a == 2x + x^3

   @test b == d

   @test c != d
end

@testset "fmpq_rel_series.adhoc_comparison..." begin
   R, x = PowerSeriesRing(QQ, 30, "x")

   a = 2x + x^3
   b = O(x^0)
   c = 1 + O(x^5)
   d = R(3)


   for T in [Int, BigInt, Rational{Int}, Rational{BigInt}, fmpz, fmpq]
      @test d == T(3)

      @test c == T(1)

      @test T(0) != a

      @test T(2) == b

      @test T(1) == c
   end
end

@testset "fmpq_rel_series.powering..." begin
   R, x = PowerSeriesRing(QQ, 30, "x")

   a = 2x + x^3
   b = O(x^4)
   c = 1 + x + 2x^2 + O(x^5)
   d = 2x + x^3 + O(x^4)

   @test a^12 == x^36+24*x^34+264*x^32+1760*x^30+7920*x^28+25344*x^26+59136*x^24+101376*x^22+126720*x^20+112640*x^18+67584*x^16+24576*x^14+4096*x^12

   @test b^12 == O(x^48)

   @test c^12 == 2079*x^4+484*x^3+90*x^2+12*x+1+O(x^5)

   @test d^12 == 4096*x^12+24576*x^14+O(x^15)

   @test_throws DomainError d^-1
end

@testset "fmpq_rel_series.shift..." begin
   R, x = PowerSeriesRing(QQ, 30, "x")

   a = 2x + x^3
   b = O(x^4)
   c = 1 + x + 2x^2 + O(x^5)
   d = 2x + x^3 + O(x^4)

   @test shift_left(a, 2) == 2*x^3+x^5

   @test_throws DomainError shift_left(a, -1)

   @test_throws DomainError shift_right(a, -1)

   @test shift_left(b, 2) == O(x^6)

   @test shift_right(c, 1) == 1+2*x+O(x^4)

   @test shift_right(d, 3) == 1+O(x^1)
end

@testset "fmpq_rel_series.truncation..." begin
   R, x = PowerSeriesRing(QQ, 30, "x")

   a = 2x + x^3
   b = O(x^4)
   c = 1 + x + 2x^2 + O(x^5)
   d = 2x + x^3 + O(x^4)

   @test truncate(a, 3) == 2*x + O(x^3)

   @test_throws DomainError truncate(a, -1)

   @test truncate(b, 2) == O(x^2)

   @test truncate(c, 5) == 2*x^2+x+1+O(x^5)

   @test truncate(d, 5) == x^3+2*x+O(x^4)
end

@testset "fmpq_rel_series.exact_division..." begin
   R, x = PowerSeriesRing(QQ, 30, "x")

   a = x + x^3
   b = O(x^4)
   c = 1 + x + 2x^2 + O(x^5)
   d = x + x^3 + O(x^6)

   @test divexact(a, d) == 1+O(x^5)

   @test divexact(d, a) == 1+O(x^5)

   @test divexact(b, c) == O(x^4)

   @test divexact(d, c) == -2*x^5+2*x^4-x^2+x+O(x^6)
end

@testset "fmpq_rel_series.adhoc_exact_division..." begin
   R, x = PowerSeriesRing(QQ, 30, "x")

   a = x + x^3
   b = O(x^4)
   c = 1 + x + 2x^2 + O(x^5)
   d = x + x^3 + O(x^6)

   for T in [Int, BigInt, Rational{Int}, Rational{BigInt}, fmpz, fmpq]
      @test isequal(divexact(7a, T(7)), a)

      @test isequal(divexact(11b, T(11)), b)

      @test isequal(divexact(2c, T(2)), c)

      @test isequal(divexact(9d, T(9)), d)
   end

   @test isequal(divexact(94872394861923874346987123694871329847a, 94872394861923874346987123694871329847), a)
end

@testset "fmpq_rel_series.inversion..." begin
   R, x = PowerSeriesRing(QQ, 30, "x")

   a = 1 + x + 2x^2 + O(x^5)
   b = R(-1)

   @test inv(a) == -x^4+3*x^3-x^2-x+1+O(x^5)

   @test inv(b) == -1
end

@testset "fmpq_rel_series.special..." begin
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
end
