@testset "QQAbsPowerSeriesRingElem.types" begin
  @test abs_series_type(QQFieldElem) == QQAbsPowerSeriesRingElem
end

@testset "QQAbsPowerSeriesRingElem.constructors" begin
  S1 = AbsPowerSeriesRing(QQ, 30)
  S2 = AbsPowerSeriesRing(QQ, 30)

  @test isa(S1, QQAbsPowerSeriesRing)
  @test S1 !== S2

  R, x = power_series_ring(QQ, 30, "x", model=:capped_absolute)

  @test elem_type(R) == QQAbsPowerSeriesRingElem
  @test elem_type(QQAbsPowerSeriesRing) == QQAbsPowerSeriesRingElem
  @test parent_type(QQAbsPowerSeriesRingElem) == QQAbsPowerSeriesRing

  @test isa(R, QQAbsPowerSeriesRing)

  a = x^3 + 2x + 1
  b = x^2 + 3x + O(x^4)

  @test isa(R(a), SeriesElem)

  @test isa(R([ZZRingElem(1), ZZRingElem(2), QQFieldElem(3)], 3, 5), SeriesElem)

  @test isa(R([QQFieldElem(1), QQFieldElem(2), QQFieldElem(3)], 3, 3), SeriesElem)

  @test isa(R(1), SeriesElem)

  @test isa(R(ZZRingElem(2)), SeriesElem)

  @test isa(R(QQFieldElem(2)), SeriesElem)

  @test isa(R(BigInt(2)), SeriesElem)

  @test isa(R(2//1), SeriesElem)

  @test isa(R(BigInt(2)//1), SeriesElem)

  @test isa(R(), SeriesElem)
end

@testset "QQAbsPowerSeriesRingElem.printing" begin
  R, x = power_series_ring(QQ, 30, "x", model=:capped_absolute)

  b = x^2 + 3x + O(x^4)

  @test sprint(show, "text/plain", b) == "3*x + x^2 + O(x^4)"
end

@testset "QQAbsPowerSeriesRingElem.manipulation" begin
  R, x = power_series_ring(QQ, 30, "x", model=:capped_absolute)

  a = 2x + x^3
  b = O(x^4)

  @test is_gen(gen(R))

  @test iszero(zero(R))

  @test isone(one(R))

  @test is_unit(-1 + x + 2x^2)

  @test valuation(a) == 1

  @test valuation(b) == 4

  @test characteristic(R) == 0
end

@testset "QQAbsPowerSeriesRingElem.similar" begin
  R, x = power_series_ring(QQ, 10, "x"; model=:capped_absolute)
  S, y = power_series_ring(ZZ, 10, "y"; model=:capped_absolute)

  for iters = 1:10
    f = rand(R, 0:10, -10:10)
    fz = rand(S, 0:10, -10:10)

    g = similar(fz, QQ, "y")
    h = similar(f, "y")
    k = similar(f)
    m = similar(fz, QQ, 5)
    n = similar(f, 5)

    @test isa(g, QQAbsPowerSeriesRingElem)
    @test isa(h, QQAbsPowerSeriesRingElem)
    @test isa(k, QQAbsPowerSeriesRingElem)
    @test isa(m, QQAbsPowerSeriesRingElem)
    @test isa(n, QQAbsPowerSeriesRingElem)

    @test parent(g).S == :y
    @test parent(h).S == :y

    @test iszero(g)
    @test iszero(h)
    @test iszero(k)
    @test iszero(m)
    @test iszero(n)

    @test parent(g) !== parent(f)
    @test parent(h) !== parent(f)
    @test parent(k) === parent(f)
    @test parent(m) !== parent(f)
    @test parent(n) !== parent(f)

    p = similar(f, cached=false)
    q = similar(f, "z", cached=false)
    r = similar(f, "z", cached=false)
    s = similar(f)
    t = similar(f)

    @test parent(p) === parent(f)
    @test parent(q) !== parent(r)
    @test parent(s) === parent(t)
  end
end

@testset "QQAbsPowerSeriesRingElem.abs_series" begin
  f = abs_series(QQ, [1, 2, 3], 3, 5, "y")

  @test isa(f, QQAbsPowerSeriesRingElem)
  @test base_ring(f) === QQ
  @test coeff(f, 0) == 1
  @test coeff(f, 2) == 3
  @test parent(f).S == :y

  g = abs_series(QQ, [1, 2, 3], 3, 5)

  @test isa(g, QQAbsPowerSeriesRingElem)
  @test base_ring(g) === QQ
  @test coeff(g, 0) == 1
  @test coeff(g, 2) == 3
  @test parent(g).S == :x

  h = abs_series(QQ, [1, 2, 3], 2, 5)
  k = abs_series(QQ, [1, 2, 3], 1, 6, cached=false)
  m = abs_series(QQ, [1, 2, 3], 3, 9, cached=false)

  @test parent(h) === parent(g)
  @test parent(k) !== parent(m)

  p = abs_series(QQ, QQFieldElem[], 0, 4)
  q = abs_series(QQ, [], 0, 6)

  @test isa(p, QQAbsPowerSeriesRingElem)
  @test isa(q, QQAbsPowerSeriesRingElem)

  @test length(p) == 0
  @test length(q) == 0

  r = abs_series(QQ, ZZRingElem[1, 2, 3], 3, 5)

  @test isa(r, QQAbsPowerSeriesRingElem)

  s = abs_series(QQ, [1, 2, 3], 3, 5; max_precision=10)

  @test max_precision(parent(s)) == 10
end

@testset "QQAbsPowerSeriesRingElem.unary_ops" begin
  R, x = power_series_ring(QQ, 30, "x", model=:capped_absolute)

  a = 2x + x^3
  b = 1 + 2x + x^2 + O(x^3)

  @test -a == -2x - x^3

  @test -b == -1 - 2x - x^2 + O(x^3)
end

@testset "QQAbsPowerSeriesRingElem.binary_ops" begin
  R, x = power_series_ring(QQ, 30, "x", model=:capped_absolute)

  a = 2x + x^3
  b = O(x^4)
  c = 1 + x + 3x^2 + O(x^5)
  d = x^2 + 3x^3 - x^4

  @test a + b == x^3+2*x+O(x^4)

  @test a - c == x^3-3*x^2+x-1+O(x^5)

  @test b*c == O(x^4)

  @test a*c == 3*x^5+x^4+7*x^3+2*x^2+2*x+O(x^6)

  @test a*d == -x^7+3*x^6-x^5+6*x^4+2*x^3
end

@testset "QQAbsPowerSeriesRingElem.adhoc_binary_ops" begin
  R, x = power_series_ring(QQ, 30, "x", model=:capped_absolute)

  a = 2x + x^3
  b = O(x^4)
  c = 1 + x + 3x^2 + O(x^5)
  d = x^2 + 3x^3 - x^4

  @test a + 2 == 2x + x^3 + 2

  @test 2 + a == 2x + x^3 + 2

  @test a + BigInt(2) == 2x + x^3 + 2

  @test BigInt(2) + a == 2x + x^3 + 2

  @test a + QQFieldElem(2) == 2x + x^3 + 2

  @test QQFieldElem(2) + a == 2x + x^3 + 2

  @test a + ZZRingElem(2) == 2x + x^3 + 2

  @test ZZRingElem(2) + a == 2x + x^3 + 2

  @test a + 2//1 == 2x + x^3 + 2

  @test 2//1 + a == 2x + x^3 + 2

  @test a + BigInt(2)//1 == 2x + x^3 + 2

  @test BigInt(2)//1 + a == 2x + x^3 + 2

  @test 2a == 4x + 2x^3

  @test a*2 == 4x + 2x^3

  @test ZZ(3)*b == O(x^4)

  @test b*ZZ(3) == O(x^4)

  @test 2c == 2 + 2*x + 6*x^2 + O(x^5)

  @test c*2 == 2 + 2*x + 6*x^2 + O(x^5)

  @test ZZ(3)*d == 3x^2 + 9x^3 - 3x^4

  @test d*ZZ(3) == 3x^2 + 9x^3 - 3x^4

  @test c*QQFieldElem(2, 3) == 2*x^2 + ZZRingElem(2)//3*x + ZZRingElem(2)//3+O(x^5)

  @test QQFieldElem(2, 3)*c == 2*x^2 + ZZRingElem(2)//3*x + ZZRingElem(2)//3+O(x^5)

  @test c*(2//3) == 2*x^2 + ZZRingElem(2)//3*x + ZZRingElem(2)//3+O(x^5)

  @test (2//3)*c == 2*x^2 + ZZRingElem(2)//3*x + ZZRingElem(2)//3+O(x^5)

  @test c*(BigInt(2)//3) == 2*x^2 + ZZRingElem(2)//3*x + ZZRingElem(2)//3+O(x^5)

  @test (BigInt(2)//3)*c == 2*x^2 + ZZRingElem(2)//3*x + ZZRingElem(2)//3+O(x^5)
end

@testset "QQAbsPowerSeriesRingElem.comparison" begin
  R, x = power_series_ring(QQ, 30, "x", model=:capped_absolute)

  a = 2x + x^3
  b = O(x^3)
  c = 1 + x + 3x^2 + O(x^5)
  d = 3x^3 - x^4

  @test a == 2x + x^3

  @test b == d

  @test c != d
end

@testset "QQAbsPowerSeriesRingElem.adhoc_comparison" begin
  R, x = power_series_ring(QQ, 30, "x", model=:capped_absolute)

  a = 2x + x^3
  b = O(x^0)
  c = 1 + O(x^5)
  d = R(3)

  @test d == 3
  @test 3 == d

  @test d == BigInt(3)
  @test BigInt(3) == d

  @test d == ZZRingElem(3)
  @test ZZRingElem(3) == d

  @test d == QQFieldElem(3)
  @test QQFieldElem(3) == d

  @test d == 3//1
  @test 3//1 == d

  @test d == BigInt(3)//1
  @test BigInt(3)//1 == d

  @test c == ZZ(1)

  @test ZZ(0) != a

  @test 2 == b

  @test ZZ(1) == c
end

@testset "QQAbsPowerSeriesRingElem.powering" begin
  R, x = power_series_ring(QQ, 30, "x", model=:capped_absolute)

  a = 2x + x^3
  b = O(x^4)
  c = 1 + x + 2x^2 + O(x^5)
  d = 2x + x^3 + O(x^4)

  @test a^12 == x^36+24*x^34+264*x^32+1760*x^30+7920*x^28+25344*x^26+59136*x^24+101376*x^22+126720*x^20+112640*x^18+67584*x^16+24576*x^14+4096*x^12 + O(x^30)

  @test b^12 == O(x^30)

  @test c^12 == 2079*x^4+484*x^3+90*x^2+12*x+1+O(x^5)

  @test d^12 == 4096*x^12+24576*x^14+O(x^15)

  @test_throws DomainError a^(-1)
end

@testset "QQAbsPowerSeriesRingElem.shift" begin
  R, x = power_series_ring(QQ, 30, "x", model=:capped_absolute)

  a = 2x + x^3
  b = O(x^4)
  c = 1 + x + 2x^2 + O(x^5)
  d = 2x + x^3 + O(x^4)

  @test shift_left(a, 2) == 2*x^3+x^5

  @test_throws DomainError shift_left(a, -1)

  @test shift_left(b, 2) == O(x^6)

  @test_throws DomainError shift_right(b, -1)

  @test shift_right(c, 1) == 1+2*x+O(x^4)

  @test shift_right(d, 3) == 1+O(x^1)
end

@testset "QQAbsPowerSeriesRingElem.truncation" begin
  R, x = power_series_ring(QQ, 30, "x", model=:capped_absolute)

  a = 2x + x^3
  b = O(x^4)
  c = 1 + x + 2x^2 + O(x^5)
  d = 2x + x^3 + O(x^4)
  e = x^3 + O(x^10)

  @test truncate(a, 3) == 2*x + O(x^3)

  @test_throws DomainError truncate(a, -1)

  @test truncate(b, 2) == O(x^2)

  @test truncate(c, 5) == 2*x^2+x+1+O(x^5)

  @test truncate(d, 5) == x^3+2*x+O(x^4)

  @test truncate(e, 2) == O(x^2)
end

@testset "QQAbsPowerSeriesRingElem.exact_division" begin
  R, x = power_series_ring(QQ, 30, "x", model=:capped_absolute)

  a = x + x^3
  b = O(x^4)
  c = 1 + x + 2x^2 + O(x^5)
  d = x + x^3 + O(x^6)

  @test divexact(a, d) == 1+O(x^5)

  @test divexact(d, a) == 1+O(x^5)

  @test divexact(b, c) == O(x^4)

  @test divexact(d, c) == -2*x^5+2*x^4-x^2+x+O(x^6)
end

@testset "QQAbsPowerSeriesRingElem.adhoc_exact_division" begin
  R, x = power_series_ring(QQ, 30, "x", model=:capped_absolute)

  a = x + x^3
  b = O(x^4)
  c = 1 + x + 2x^2 + O(x^5)
  d = x + x^3 + O(x^6)

  @test isequal(divexact(7a, 7), a)

  @test isequal(divexact(7a, BigInt(7)), a)

  @test isequal(divexact(11b, ZZRingElem(11)), b)

  @test isequal(divexact(2c, ZZRingElem(2)), c)

  @test isequal(divexact(9d, 9), d)

  @test isequal(divexact(94872394861923874346987123694871329847a, 94872394861923874346987123694871329847), a)

  @test isequal(divexact(9d, 9//1), d)

  @test isequal(divexact(9d, BigInt(9)//1), d)
end

@testset "QQAbsPowerSeriesRingElem.inversion" begin
  R, x = power_series_ring(QQ, 30, "x", model=:capped_absolute)

  a = 1 + x + 2x^2 + O(x^5)
  b = R(-1)

  @test inv(a) == -x^4+3*x^3-x^2-x+1+O(x^5)

  @test inv(b) == -1
end

@testset "QQAbsPowerSeriesRingElem.integral_derivative" begin
  R, x = power_series_ring(QQ, 10, "x"; model=:capped_absolute)

  for iter = 1:100
    f = rand(R, 0:0, -10:10)

    @test integral(derivative(f)) == f - coeff(f, 0)
  end
end

@testset "QQAbsPowerSeriesRingElem.special" begin
  R, x = power_series_ring(QQ, 30, "x", model=:capped_absolute)

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

  for iter = 1:300
    f = rand(R, 0:9, -10:10)

    @test sqrt(f^2) == f || sqrt(f^2) == -f
  end
end

@testset "QQAbsPowerSeriesRingElem.unsafe_operators" begin
  R, x = power_series_ring(QQ, 30, "x", model=:capped_absolute)

  for iter = 1:300
    f = rand(R, 0:9, -10:10)
    g = rand(R, 0:9, -10:10)
    f0 = deepcopy(f)
    g0 = deepcopy(g)

    h = rand(R, 0:9, -10:10)

    k = f + g
    h = add!(h, f, g)
    @test isequal(h, k)
    @test isequal(f, f0)
    @test isequal(g, g0)

    f1 = deepcopy(f)
    f1 = add!(f1, f1, g)
    @test isequal(f1, k)
    @test isequal(g, g0)

    g1 = deepcopy(g)
    g1 = add!(g1, f, g1)
    @test isequal(g1, k)
    @test isequal(f, f0)

    f1 = deepcopy(f)
    f1 = add!(f1, g)
    @test isequal(h, k)
    @test isequal(g, g0)

    k = f*g
    h = mul!(h, f, g)
    @test isequal(h, k)
    @test isequal(f, f0)
    @test isequal(g, g0)

    f1 = deepcopy(f)
    f1 = mul!(f1, f1, g)
    @test isequal(f1, k)
    @test isequal(g, g0)

    g1 = deepcopy(g)
    g1 = mul!(g1, f, g1)
    @test isequal(g1, k)
    @test isequal(f, f0)

    h = zero!(h)
    @test isequal(h, R())
  end
end

@testset "QQAbsPowerSeriesRingElem.set_precision" begin
  R, x = power_series_ring(QQ, 30, "x", model=:capped_absolute)

  a = 2x + x^3
  b = O(x^4)
  c = 1 + x + 2x^2 + O(x^5)
  d = 2x + x^3 + O(x^4)
  e = x^3 + O(x^10)

  @test set_precision(a, 3) == 2*x + O(x^3)
  @test set_precision(b, 2) == O(x^2)
  @test set_precision(b, 10) == O(x^10)
  @test set_precision(c, 5) == 2*x^2+x+1+O(x^5)
  @test set_precision(c, 10) == 2*x^2+x+1+O(x^10)
  @test set_precision(d, 5) == x^3+2*x+O(x^5)
  @test set_precision(e, 2) == O(x^2)

  @test_throws DomainError set_precision(a, -1)
end
