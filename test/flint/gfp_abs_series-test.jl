@testset "fpAbsPowerSeriesRingElem.types" begin
  @test abs_series_type(fpFieldElem) == fpAbsPowerSeriesRingElem
end

@testset "fpAbsPowerSeriesRingElem.constructors" begin
  S = Native.GF(23)

  R1 = AbsPowerSeriesRing(S, 30)
  R2 = AbsPowerSeriesRing(S, 30)

  @test isa(R1,fpAbsPowerSeriesRing)
  @test R1 !== R2

  R, x = power_series_ring(S, 30, "x", model=:capped_absolute)

  @test elem_type(R) == fpAbsPowerSeriesRingElem
  @test elem_type(fpAbsPowerSeriesRing) == fpAbsPowerSeriesRingElem
  @test parent_type(fpAbsPowerSeriesRingElem) == fpAbsPowerSeriesRing

  @test isa(R, fpAbsPowerSeriesRing)

  a = x^3 + 2x + 1
  b = x^2 + 3x + O(x^4)

  @test isa(R(a), SeriesElem)

  @test isa(R([S(1), S(2), S(3)], 3, 5), SeriesElem)

  @test isa(R([ZZ(1), ZZ(2), ZZ(3)], 3, 3), SeriesElem)

  @test isa(R(1), SeriesElem)

  @test isa(R(ZZ(2)), SeriesElem)

  @test isa(R(), SeriesElem)
end

@testset "fpAbsPowerSeriesRingElem.printing" begin
  S = Native.GF(23)
  R, x = power_series_ring(S, 30, "x", model=:capped_absolute)

  b = x^2 + 3x + O(x^4)

  @test sprint(show, "text/plain", b) == "3*x + x^2 + O(x^4)"
end

@testset "fpAbsPowerSeriesRingElem.manipulation" begin
  S = Native.GF(23)
  R, x = power_series_ring(S, 30, "x", model=:capped_absolute)

  a = 2x + x^3
  b = O(x^4)

  @test is_gen(gen(R))

  @test iszero(zero(R))

  @test isone(one(R))

  @test is_unit(-1 + x + 2x^2)

  @test valuation(a) == 1

  @test valuation(b) == 4

  @test characteristic(R) == 23
end

@testset "fpAbsPowerSeriesRingElem.similar" begin
  R0 = Native.GF(23)
  R, x = power_series_ring(R0, 10, "x"; model=:capped_absolute)
  S, y = power_series_ring(ZZ, 10, "y"; model=:capped_absolute)

  for iters = 1:10
    f = rand(R, 0:10)
    fz = rand(S, 0:10, -10:10)

    g = similar(fz, R0, "y")
    h = similar(f, "y")
    k = similar(f)
    m = similar(fz, R0, 5)
    n = similar(f, 5)

    @test isa(g, fpAbsPowerSeriesRingElem)
    @test isa(h, fpAbsPowerSeriesRingElem)
    @test isa(k, fpAbsPowerSeriesRingElem)
    @test isa(m, fpAbsPowerSeriesRingElem)
    @test isa(n, fpAbsPowerSeriesRingElem)

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

@testset "fpAbsPowerSeriesRingElem.abs_series" begin
  R = Native.GF(23)
  f = abs_series(R, [1, 2, 3], 3, 5, "y")

  @test isa(f, fpAbsPowerSeriesRingElem)
  @test base_ring(f) === R
  @test coeff(f, 0) == 1
  @test coeff(f, 2) == 3
  @test parent(f).S == :y

  g = abs_series(R, [1, 2, 3], 3, 5)

  @test isa(g, fpAbsPowerSeriesRingElem)
  @test base_ring(g) === R
  @test coeff(g, 0) == 1
  @test coeff(g, 2) == 3
  @test parent(g).S == :x

  h = abs_series(R, [1, 2, 3], 2, 5)
  k = abs_series(R, [1, 2, 3], 1, 6, cached=false)
  m = abs_series(R, [1, 2, 3], 3, 9, cached=false)

  @test parent(h) === parent(g)
  @test parent(k) !== parent(m)

  p = abs_series(R, fpFieldElem[], 0, 4)
  q = abs_series(R, [], 0, 6)

  @test isa(p, fpAbsPowerSeriesRingElem)
  @test isa(q, fpAbsPowerSeriesRingElem)

  @test length(p) == 0
  @test length(q) == 0

  r = abs_series(R, ZZRingElem[1, 2, 3], 3, 5)

  @test isa(r, fpAbsPowerSeriesRingElem)

  s = abs_series(R, [1, 2, 3], 3, 5; max_precision=10)

  @test max_precision(parent(s)) == 10
end

@testset "fpAbsPowerSeriesRingElem.unary_ops" begin
  S = Native.GF(23)
  R, x = power_series_ring(S, 30, "x", model=:capped_absolute)

  a = 2x + x^3
  b = 1 + 2x + x^2 + O(x^3)

  @test -a == -2x - x^3

  @test -b == -1 - 2x - x^2 + O(x^3)
end

@testset "fpAbsPowerSeriesRingElem.binary_ops" begin
  S = Native.GF(23)
  R, x = power_series_ring(S, 30, "x", model=:capped_absolute)

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

@testset "fpAbsPowerSeriesRingElem.adhoc_binary_ops" begin
  S = Native.GF(23)
  R, x = power_series_ring(S, 30, "x", model=:capped_absolute)

  a = 2x + x^3
  b = O(x^4)
  c = 1 + x + 3x^2 + O(x^5)
  d = x^2 + 3x^3 - x^4

  @test 2a == 4x + 2x^3

  @test ZZ(3)*b == O(x^4)

  @test c*2 == 2 + 2*x + 6*x^2 + O(x^5)

  @test d*ZZ(3) == 3x^2 + 9x^3 - 3x^4
end

@testset "fpAbsPowerSeriesRingElem.comparison" begin
  S = Native.GF(23)
  R, x = power_series_ring(S, 30, "x", model=:capped_absolute)

  a = 2x + x^3
  b = O(x^3)
  c = 1 + x + 3x^2 + O(x^5)
  d = 3x^3 - x^4

  @test a == 2x + x^3

  @test b == d

  @test c != d
end

@testset "fpAbsPowerSeriesRingElem.adhoc_comparison" begin
  S = Native.GF(23)
  R, x = power_series_ring(S, 30, "x", model=:capped_absolute)

  a = 2x + x^3
  b = O(x^0)
  c = 1 + O(x^5)
  d = R(3)

  @test d == 3

  @test c == ZZ(1)

  @test ZZ(0) != a

  @test 2 == b

  @test ZZ(1) == c
end

@testset "fpAbsPowerSeriesRingElem.powering" begin
  S = Native.GF(23)
  R, x = power_series_ring(S, 30, "x", model=:capped_absolute)

  a = 2x + x^3
  b = O(x^4)
  c = 1 + x + 2x^2 + O(x^5)
  d = 2x + x^3 + O(x^4)

  @test a^12 == 2*x^12 + 12*x^14 + 10*x^16 + 9*x^18 + 13*x^20 + 15*x^22 + 3*x^24 + 21*x^26 + 8*x^28 + O(x^30)

  @test b^12 == O(x^30)

  @test c^12 == 1 + 12*x + 21*x^2 + x^3 + 9*x^4 + O(x^5)

  @test d^12 == 2*x^12 + 12*x^14 + O(x^15)

  @test_throws DomainError a^-1
end

@testset "fpAbsPowerSeriesRingElem.shift" begin
  S = Native.GF(23)
  R, x = power_series_ring(S, 30, "x", model=:capped_absolute)

  a = 2x + x^3
  b = O(x^4)
  c = 1 + x + 2x^2 + O(x^5)
  d = 2x + x^3 + O(x^4)

  @test shift_left(a, 2) == 2*x^3+x^5

  @test shift_left(b, 2) == O(x^6)

  @test shift_right(c, 1) == 1+2*x+O(x^4)

  @test shift_right(d, 3) == 1+O(x^1)

  @test_throws DomainError shift_left(a, -1)

  @test_throws DomainError shift_right(a, -1)
end

@testset "fpAbsPowerSeriesRingElem.truncation" begin
  S = Native.GF(23)
  R, x = power_series_ring(S, 30, "x", model=:capped_absolute)

  a = 2x + x^3
  b = O(x^4)
  c = 1 + x + 2x^2 + O(x^5)
  d = 2x + x^3 + O(x^4)
  e = x^3 + O(x^10)

  @test truncate(a, 3) == 2*x + O(x^3)

  @test truncate(b, 2) == O(x^2)

  @test truncate(c, 5) == 2*x^2+x+1+O(x^5)

  @test truncate(d, 5) == x^3+2*x+O(x^4)

  @test truncate(e, 2) == O(x^2)

  @test_throws DomainError truncate(a, -1)

end

@testset "fpAbsPowerSeriesRingElem.exact_division" begin
  S = Native.GF(23)
  R, x = power_series_ring(S, 30, "x", model=:capped_absolute)

  a = x + x^3
  b = O(x^4)
  c = 1 + x + 2x^2 + O(x^5)
  d = x + x^3 + O(x^6)

  @test divexact(a, d) == 1+O(x^5)

  @test divexact(d, a) == 1+O(x^5)

  @test divexact(b, c) == O(x^4)

  @test divexact(d, c) == -2*x^5+2*x^4-x^2+x+O(x^6)
end

@testset "fpAbsPowerSeriesRingElem.adhoc_exact_division" begin
  S = Native.GF(23)
  R, x = power_series_ring(ZZ, 30, "x", model=:capped_absolute)

  a = x + x^3
  b = O(x^4)
  c = 1 + x + 2x^2 + O(x^5)
  d = x + x^3 + O(x^6)

  @test isequal(divexact(7a, 7), a)

  @test isequal(divexact(11b, ZZRingElem(11)), b)

  @test isequal(divexact(2c, ZZRingElem(2)), c)

  @test isequal(divexact(9d, 9), d)

  @test isequal(divexact(94872394861923874346987123694871329847a, 94872394861923874346987123694871329847), a)
end

@testset "fpAbsPowerSeriesRingElem.inversion" begin
  S = Native.GF(23)
  R, x = power_series_ring(S, 30, "x", model=:capped_absolute)

  a = 1 + x + 2x^2 + O(x^5)
  b = R(-1)

  @test inv(a) == -x^4+3*x^3-x^2-x+1+O(x^5)

  @test inv(b) == -1
end

@testset "fpAbsPowerSeriesRingElem.integral_derivative" begin
  S = Native.GF(31)
  R, x = power_series_ring(S, 10, "x"; model=:capped_absolute)

  for iter = 1:100
    f = rand(R, 0:0)

    @test integral(derivative(f)) == f - coeff(f, 0)
  end
end

@testset "fpAbsPowerSeriesRingElem.square_root" begin
  S = Native.GF(31)
  R, x = power_series_ring(S, 30, "x", model=:capped_absolute)

  for iter = 1:300
    f = rand(R, 0:9)

    @test sqrt(f^2) == f || sqrt(f^2) == -f
  end

  for p in [2, 7, 19, 65537]
    R = Native.GF(p)

    S, x = power_series_ring(R, 10, "x", model=:capped_absolute)

    for iter = 1:10
      f = rand(S, 0:10, 0:Int(p))

      s = f^2

      @test is_square(s)

      q = sqrt(s)

      @test q^2 == s

      q = sqrt(s; check=false)

      @test q^2 == s

      f1, s1 = is_square_with_sqrt(s)

      @test f1 && s1^2 == s

      if s*x != 0
        @test_throws ErrorException sqrt(s*x)
      end

      if mod(p, 4) == 3
        @test !is_square(-one(S))
      end
    end
  end
end

@testset "fpAbsPowerSeriesRingElem.unsafe_operators" begin
  S = Native.GF(31)
  R, x = power_series_ring(S, 30, "x", model=:capped_absolute)

  for iter = 1:300
    f = rand(R, 0:9)
    g = rand(R, 0:9)
    f0 = deepcopy(f)
    g0 = deepcopy(g)

    h = rand(R, 0:9)

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

@testset "fpAbsPowerSeriesRingElem.set_precision" begin
  S = Native.GF(23)
  R, x = power_series_ring(S, 30, "x", model=:capped_absolute)

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
