@testset "fmpz_puiseux_series.constructors..." begin
   R, x = PuiseuxSeriesRing(ZZ, 30, "x")

   @test elem_type(R) == FlintPuiseuxSeriesRingElem{fmpz_laurent_series}
   @test elem_type(FlintPuiseuxSeriesRing{fmpz_laurent_series}) == FlintPuiseuxSeriesRingElem{fmpz_laurent_series}
   @test parent_type(FlintPuiseuxSeriesRingElem{fmpz_laurent_series}) == FlintPuiseuxSeriesRing{fmpz_laurent_series}

   @test isa(R, FlintPuiseuxSeriesRing)

   a1 = x^3 + 2x + 1

   @test isa(a1, FlintPuiseuxSeriesElem)

   b1 = R(a1)

   @test isa(b1, FlintPuiseuxSeriesElem)

   g1 = R(1)
   h1 = R(ZZ(2))
   k1 = R()

   @test isa(g1, FlintPuiseuxSeriesElem)
   @test isa(h1, FlintPuiseuxSeriesElem)
   @test isa(k1, FlintPuiseuxSeriesElem)
end

@testset "fmpz_puiseux_series.rand..." begin
   R, x = PuiseuxSeriesRing(ZZ, 10, "x")
   test_rand(R, -12:12, 1:6, -10:10)
   test_rand(R, -12:12, 1:6, make(ZZ, -10:10))

   R, x = PuiseuxSeriesField(QQ, 10, "x")
   test_rand(R, -12:12, 1:6, -10:10)
   test_rand(R, -12:12, 1:6, make(ZZ, -10:10))
end

@testset "fmpz_puiseux_series.manipulation..." begin
   S, x = PuiseuxSeriesRing(ZZ, 30, "x")

   @test max_precision(S) == 30

   a = 2x + x^3
   b = O(x^4)

   @test valuation(a) == 1
   @test valuation(b) == 4

   @test precision(a) == 31
   @test precision(b) == 4

   a = 2x^(1//3) + x^(2//3) + 3x

   @test coeff(a, 1) == 3
   @test coeff(a, 1//3) == 2
   @test coeff(a, 1//4) == 0

   @test isgen(gen(S))

   @test iszero(zero(S))

   @test isone(one(S))

   @test isunit(-1 + x + 2x^2)

   @test isequal(deepcopy(a), a)
   @test isequal(deepcopy(b), b)

   @test characteristic(S) == 0

   d = Dict(x => 1, x^2 => 2)
   @test d[x] == 1
   @test d[x^2] == 2
end

@testset "fmpz_puiseux_series.unary_ops..." begin
   R, x = PuiseuxSeriesRing(ZZ, 10, "x")
   for iter = 1:300
      f = rand(R, -12:12, 1:6, -10:10)

      @test isequal(-(-f), f)
      @test iszero(f + (-f))
   end
end

@testset "fmpz_puiseux_series.binary_ops..." begin
   R, x = PuiseuxSeriesRing(ZZ, 10, "x")
   for iter = 1:100
      f = rand(R, -12:12, 1:6, -10:10)
      g = rand(R, -12:12, 1:6, -10:10)
      h = rand(R, -12:12, 1:6, -10:10)
      @test isequal(f + g, g + f)
      @test isequal(f + (g + h), (f + g) + h)
      @test isequal(f*g, g*f)
      @test isequal(f*(g*h), (f*g)*h)
      @test isequal(f - g, -(g - f))
      @test (f - h) + h == f
      @test f*(g + h) == f*g + f*h
      @test f*(g - h) == f*g - f*h
   end
end

@testset "fmpz_puiseux_series.adhoc_binary_ops..." begin
   R, x = PuiseuxSeriesRing(ZZ, 10, "x")
   for iter = 1:500
      f = rand(R, -12:12, 1:6, -10:10)
      c1 = rand(ZZ, -10:10)
      c2 = rand(ZZ, -10:10)
      d1 = rand(zz, -10:10)
      d2 = rand(zz, -10:10)

      @test isequal(c1*f - c2*f, (c1 - c2)*f)
      @test isequal(c1*f + c2*f, (c1 + c2)*f)
      @test isequal(d1*f - d2*f, (d1 - d2)*f)
      @test isequal(d1*f + d2*f, (d1 + d2)*f)

      @test isequal(f*c1 - f*c2, f*(c1 - c2))
      @test isequal(f*c1 + f*c2, f*(c1 + c2))
      @test isequal(f*d1 - f*d2, f*(d1 - d2))
      @test isequal(f*d1 + f*d2, f*(d1 + d2))
   end
end

@testset "fmpz_puiseux_series.comparison..." begin
   R, x = PuiseuxSeriesRing(ZZ, 10, "x")
   for iter = 1:500
      f = rand(R, -12:12, 1:6, -10:10)
      g = deepcopy(f)
      h = R()
      while iszero(h)
         h = rand(R, -12:12, 1:6, -10:10)
      end

      @test f == g
      @test isequal(f, g)
      @test (precision(h) > min(precision(f), precision(g)) || f != g + h)
      @test (precision(h) > min(precision(f), precision(g)) || !isequal(f, g + h))
   end
end

@testset "fmpz_puiseux_series.adhoc_comparison..." begin
   R, x = PuiseuxSeriesRing(ZZ, 10, "x")
   for iter = 1:500
      f = R()
      while f == 0
         f = rand(R, 0:0, 1:6, -10:10)
      end
      f += rand(R, 1:12, 1:6, -10:10)
      c1 = rand(ZZ, -10:10)
      d1 = rand(zz, -10:10)

      @test R(c1) == c1
      @test c1 == R(c1)
      @test R(d1) == d1
      @test d1 == R(d1)

      @test R(c1) != c1 + f
      @test c1 != R(c1) + f
      @test R(d1) != d1 + f
      @test d1 != R(d1) + f
   end
end

@testset "fmpz_puiseux_series.powering..." begin
   R, x = PuiseuxSeriesRing(ZZ, 10, "x")

   for iter = 1:100
      f = rand(R, -12:12, 1:6, -10:10)
      r2 = R(1)

      for expn = 0:10
         r1 = f^expn

         @test (f == 0 && expn == 0 && r1 == 0) || isequal(r1, r2)

         r2 *= f
      end
   end

   @test iszero((x-x)^1)
   @test isone((1-2*x+x^2)*(1-x)^-2)
end

@testset "fmpz_puiseux_series.inversion..." begin
   R, x = PuiseuxSeriesRing(ZZ, 10, "x")
   for iter = 1:300
      f = R()
      while iszero(f) || !isunit(coeff(f, valuation(f)))
         f = rand(R, -12:12, 1:6, -10:10)
      end

      @test f*inv(f) == 1
   end
end

@testset "fmpz_puiseux_series.square_root..." begin
   R, x = PuiseuxSeriesRing(ZZ, 10, "x")
   for iter = 1:300
      f = rand(R, -12:12, 1:6, -10:10)
      g = f^2

      @test isequal(sqrt(g)^2, g)
   end

   @test sqrt(1+4*x)*x^(1//2) == sqrt(x+4*x^2)
end

@testset "fmpz_puiseux_series.exact_division..." begin
   R, x = PuiseuxSeriesRing(ZZ, 10, "x")
   for iter = 1:300
      f = rand(R, -12:12, 1:6, -10:10)
      g = rand(R, -12:12, 1:6, -10:10)
      while iszero(g) || !isunit(coeff(g, valuation(g)))
         g = rand(R, -12:12, 1:6, -10:10)
      end

      @test divexact(f, g)*g == f
   end
end

@testset "fmpz_puiseux_series.adhoc_exact_division..." begin
   R, x = PuiseuxSeriesRing(ZZ, 10, "x")
   for iter = 1:300
      f = rand(R, -12:12, 1:6, -10:10)
      c = ZZ()
      while c == 0
         c = rand(ZZ, -10:10)
      end

      @test isequal(divexact(f*c, c), f)
   end
end

@testset "fmpz_puiseux_series.special_functions..." begin
   S, x = PuiseuxSeriesRing(ZZ, 100, "x")

   @test isequal(exp(2x - x^2 + O(x^3)), 1+2*x+x^2+O(x^3))

   @test isequal(eta_qexp(x), x^(1//24)-x^(25//24)-x^(49//24)+O(x^(101//24)))
end

@testset "fmpz_puiseux_series.unsafe..." begin
   R, x = PuiseuxSeriesRing(ZZ, 10, "x")
   a = x
   zero!(a)
   @test iszero(a)
   mul!(a, a, x)
   @test iszero(a)
   add!(a, a, x)
   @test a == x
   addeq!(a, x^2)
   @test a == x+x^2
   mul!(a, a, x)
   @test a == x*(x+x^2)
end
