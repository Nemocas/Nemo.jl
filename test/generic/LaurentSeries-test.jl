# In general we want to test over:
#    1) Exact rings, e.g. Z
#    2) Exact fields, e.g. Q or GFp
#    3) Inexact rings, e.g. polynomials over Julia RealField, or power series
#       over Z
#    4) Inexact fields, e.g. Julia RealField
#    5) A field of char p > 0, e.g. GF(p)
#    6) A ring of char p > 0, e.g. Z/pZ
#    7) Commutative ring, not an integral domain, e.g. Z/nZ or Z[x]/(f)
#       with reducible f
# In some cases, we may also wish to test over:
#    8) Polynomial rings, e.g. to test interpolation strategies
#    9) Fraction fields, such as Q, e.g. to test fraction free algorithms,
#       quasidivision, etc.
#   10) Generic towers, e.g. to test ad hoc functions
# Note: only useful to distinguish rings and fields for 1/2, 3/4, 5/6 if the
# algos differ, and 7 can often stand in for 5/6 if the algorithm supports it.

function test_laurent_series_constructors()
   print("Generic.LaurentSeries.constructors...")

   R, x = LaurentSeriesRing(JuliaZZ, 30, "x")

   S, t = PolynomialRing(JuliaQQ, "t")
   T, y = LaurentSeriesRing(S, 30, "y")

   U, z = LaurentSeriesField(JuliaQQ, 30, "z")

   @test elem_type(R) == Generic.LaurentSeriesRingElem{BigInt}
   @test elem_type(Generic.LaurentSeriesRing{BigInt}) == Generic.LaurentSeriesRingElem{BigInt}
   @test parent_type(Generic.LaurentSeriesRingElem{BigInt}) == Generic.LaurentSeriesRing{BigInt}

   @test elem_type(T) == Generic.LaurentSeriesRingElem{elem_type(S)}
   @test elem_type(Generic.LaurentSeriesRing{elem_type(S)}) == Generic.LaurentSeriesRingElem{elem_type(S)}
   @test parent_type(Generic.LaurentSeriesRingElem{elem_type(S)}) == Generic.LaurentSeriesRing{elem_type(S)}

   @test elem_type(U) == Generic.LaurentSeriesFieldElem{Rational{BigInt}}
   @test elem_type(Generic.LaurentSeriesField{Rational{BigInt}}) == Generic.LaurentSeriesFieldElem{Rational{BigInt}}
   @test parent_type(Generic.LaurentSeriesFieldElem{Rational{BigInt}}) == Generic.LaurentSeriesField{Rational{BigInt}}

   @test isa(R, Generic.LaurentSeriesRing)
   @test isa(T, Generic.LaurentSeriesRing)
   @test isa(U, Generic.LaurentSeriesField)

   a1 = x^3 + 2x + 1
   a2 = (t^2 + 1)*y^2 + (t + 3)*y + O(y^4)
   a3 = z^3 + 2z + 1

   @test isa(a1, Generic.LaurentSeriesElem)
   @test isa(a2, Generic.LaurentSeriesElem)
   @test isa(a3, Generic.LaurentSeriesElem)

   b1 = R(a1)
   b2 = T(a2)
   b3 = U(a3)

   @test isa(b1, Generic.LaurentSeriesElem)
   @test isa(b2, Generic.LaurentSeriesElem)
   @test isa(b3, Generic.LaurentSeriesElem)

   c1 = R(BigInt[1, 3, 5], 3, 5, 0)
   c2 = T([t + 1, t, S(1)], 3, 5, 0)
   c3 = U(Rational{BigInt}[1, 3, 5], 3, 5, 0)

   @test isa(c1, Generic.LaurentSeriesElem)
   @test isa(c2, Generic.LaurentSeriesElem)
   @test isa(c3, Generic.LaurentSeriesElem)

   g1 = R(1)
   h1 = R(JuliaZZ(2))
   k1 = R()

   g2 = T(1)
   h2 = T(JuliaZZ(2))
   h2 = T(JuliaQQ(2, 3))
   k2 = T()

   g3 = U(1)
   h3 = U(JuliaZZ(2))
   k3 = U()

   @test isa(g1, Generic.LaurentSeriesElem)
   @test isa(g2, Generic.LaurentSeriesElem)
   @test isa(g3, Generic.LaurentSeriesElem)
   @test isa(h1, Generic.LaurentSeriesElem)
   @test isa(h2, Generic.LaurentSeriesElem)
   @test isa(h3, Generic.LaurentSeriesElem)
   @test isa(k1, Generic.LaurentSeriesElem)
   @test isa(k2, Generic.LaurentSeriesElem)
   @test isa(k3, Generic.LaurentSeriesElem)

   l = T(t)

   @test isa(l, Generic.LaurentSeriesElem)

   println("PASS")
end

function test_laurent_series_manipulation()
   print("Generic.LaurentSeries.manipulation...")

   R, t = PolynomialRing(JuliaQQ, "t")
   S, x = LaurentSeriesRing(R, 30, "x")

   @test max_precision(S) == 30

   a = 2x + x^3
   b = O(x^4)

   @test pol_length(a) == 3
   @test pol_length(b) == 0

   @test valuation(a) == 1
   @test valuation(b) == 4

   @test precision(a) == 31
   @test precision(b) == 4

   @test isgen(gen(S))

   @test iszero(zero(S))

   @test isone(one(S))

   @test isunit(-1 + x + 2x^2)

   @test isequal(deepcopy(a), a)
   @test isequal(deepcopy(b), b)

   @test normalise(a, 3) == 3

   @test coeff(a, 1) == 2
   @test coeff(b, 7) == 0

   T = ResidueRing(JuliaZZ, 7)
   U, y = LaurentSeriesRing(T, 10, "y")

   @test modulus(T) == 7

   println("PASS")
end

function test_laurent_series_unary_ops()
   print("Generic.LaurentSeries.unary_ops...")

   #  Exact ring
   R, x = LaurentSeriesRing(JuliaZZ, 10, "x")
   for iter = 1:300
      f = rand(R, -12:12, -10:10)

      @test isequal(-(-f), f)
      @test iszero(f + (-f))
   end

   #  Inexact field
   R, x = LaurentSeriesField(JuliaRealField, 10, "x")
   for iter = 1:300
      f = rand(R, -12:12, -1:1)

      @test isequal(-(-f), f)
      @test iszero(f + (-f))
   end

   # Non-integral domain
   T = ResidueRing(JuliaZZ, 6)
   R, x = LaurentSeriesRing(T, 10, "x")
   for iter = 1:300
      f = rand(R, -12:12, 0:5)

      @test isequal(-(-f), f)
      @test iszero(f + (-f))
   end

   println("PASS")
end

function test_laurent_series_binary_ops()
   print("Generic.LaurentSeries.binary_ops...")

   #  Exact ring
   R, x = LaurentSeriesRing(JuliaZZ, 10, "x")
   for iter = 1:100
      f = rand(R, -12:12, -10:10)
      g = rand(R, -12:12, -10:10)
      h = rand(R, -12:12, -10:10)
      @test isequal(f + g, g + f)
      @test isequal(f + (g + h), (f + g) + h)
      @test isequal(f*g, g*f)
      @test isequal(f*(g*h), (f*g)*h)
      @test isequal(f - g, -(g - f))
      @test (f - h) + h == f
      @test f*(g + h) == f*g + f*h
      @test f*(g - h) == f*g - f*h
   end

   #  Inexact field
   R, x = LaurentSeriesField(JuliaRealField, 10, "x")
   for iter = 1:100
      f = rand(R, -12:12, -1:1)
      g = rand(R, -12:12, -1:1)
      h = rand(R, -12:12, -1:1)
      @test isapprox(f + (g + h), (f + g) + h)
      @test isapprox(f*g, g*f)
      @test isapprox(f*(g*h), (f*g)*h)
      @test isapprox(f - g, -(g - f))
      @test isapprox((f - h) + h, f)
      @test isapprox(f*(g + h), f*g + f*h)
      @test isapprox(f*(g - h), f*g - f*h)
   end

   # Non-integral domain
   T = ResidueRing(JuliaZZ, 6)
   R, x = LaurentSeriesRing(T, 10, "x")
   for iter = 1:100
      f = rand(R, -12:12, 0:5)
      g = rand(R, -12:12, 0:5)
      h = rand(R, -12:12, 0:5)
      @test isequal(f + (g + h), (f + g) + h)
      @test isequal(f*g, g*f)
      @test f*(g*h) == (f*g)*h
      @test isequal(f - g, -(g - f))
      @test (f - h) + h == f
      @test f*(g + h) == f*g + f*h
      @test f*(g - h) == f*g - f*h
   end

   println("PASS")
end

function test_laurent_series_adhoc_binary_ops()
   print("Generic.LaurentSeries.adhoc_binary_ops...")

   # Exact ring
   R, x = LaurentSeriesRing(JuliaZZ, 10, "x")
   for iter = 1:500
      f = rand(R, -12:12, -10:10)
      c1 = rand(JuliaZZ, -10:10)
      c2 = rand(JuliaZZ, -10:10)
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

   # Inexact field
   R, x = LaurentSeriesField(JuliaRealField, 10, "x")
   for iter = 1:500
      f = rand(R, -12:12, -1:1)
      c1 = rand(JuliaZZ, -10:10)
      c2 = rand(JuliaZZ, -10:10)
      d1 = rand(JuliaRealField, -1:1)
      d2 = rand(JuliaRealField, -1:1)

      @test isapprox(c1*f - c2*f, (c1 - c2)*f)
      @test isapprox(c1*f + c2*f, (c1 + c2)*f)
      @test isapprox(d1*f - d2*f, (d1 - d2)*f)
      @test isapprox(d1*f + d2*f, (d1 + d2)*f)

      @test isapprox(f*c1 - f*c2, f*(c1 - c2))
      @test isapprox(f*c1 + f*c2, f*(c1 + c2))
      @test isapprox(f*d1 - f*d2, f*(d1 - d2))
      @test isapprox(f*d1 + f*d2, f*(d1 + d2))
   end

   # Non-integral domain
   R = ResidueRing(JuliaZZ, 6)
   S, x = LaurentSeriesRing(R, 10, "x")
   for iter = 1:500
      f = rand(S, -12:12, 0:5)
      c1 = rand(JuliaZZ, -10:10)
      c2 = rand(JuliaZZ, -10:10)
      d1 = rand(zz, -10:10)
      d2 = rand(zz, -10:10)
      a1 = rand(R, 0:5)
      a2 = rand(R, 0:5)

      @test isequal(a1*f - a2*f, (a1 - a2)*f)
      @test isequal(a1*f + a2*f, (a1 + a2)*f)
      @test isequal(c1*f - c2*f, (c1 - c2)*f)
      @test isequal(c1*f + c2*f, (c1 + c2)*f)
      @test isequal(d1*f - d2*f, (d1 - d2)*f)
      @test isequal(d1*f + d2*f, (d1 + d2)*f)

      @test isequal(f*a1 - f*a2, f*(a1 - a2))
      @test isequal(f*a1 + f*a2, f*(a1 + a2))
      @test isequal(f*c1 - f*c2, f*(c1 - c2))
      @test isequal(f*c1 + f*c2, f*(c1 + c2))
      @test isequal(f*d1 - f*d2, f*(d1 - d2))
      @test isequal(f*d1 + f*d2, f*(d1 + d2))
   end

   # Generic tower
   R, x = JuliaZZ["x"]
   S, y = LaurentSeriesRing(R, 10, "y")
   for iter = 1:100
      f = rand(S, -12:12, 0:5, -10:10)
      c1 = rand(JuliaZZ, -10:10)
      c2 = rand(JuliaZZ, -10:10)
      d1 = rand(R, 0:5, -10:10)
      d2 = rand(R, 0:5, -10:10)

      @test isequal(c1*f - c2*f, (c1 - c2)*f)
      @test isequal(c1*f + c2*f, (c1 + c2)*f)
      @test isequal(d1*f - d2*f, (d1 - d2)*f)
      @test isequal(d1*f + d2*f, (d1 + d2)*f)

      @test isequal(f*c1 - f*c2, f*(c1 - c2))
      @test isequal(f*c1 + f*c2, f*(c1 + c2))
      @test isequal(f*d1 - f*d2, f*(d1 - d2))
      @test isequal(f*d1 + f*d2, f*(d1 + d2))
   end

   println("PASS")
end

function test_laurent_series_comparison()
   print("Generic.LaurentSeries.comparison...")

   # Exact ring
   R, x = LaurentSeriesRing(JuliaZZ, 10, "x")
   for iter = 1:500
      f = rand(R, -12:12, -10:10)
      g = deepcopy(f)
      h = R()
      while iszero(h)
         h = rand(R, -12:12, -10:10)
      end

      @test f == g
      @test isequal(f, g)
      @test (precision(h) > min(precision(f), precision(g)) || f != g + h)
      @test (precision(h) > min(precision(f), precision(g)) || !isequal(f, g + h))
   end

   # Inexact field
   R, x = LaurentSeriesField(JuliaRealField, 10, "x")
   for iter = 1:500
      f = rand(R, -12:12, -1:1)
      g = deepcopy(f)
      h = R()
      while iszero(h)
         h = rand(R, -12:12, -1:1)
      end

      @test f == g
      @test isequal(f,  g)
      @test (precision(h) > min(precision(f), precision(g)) || f != g + h)
      @test (precision(h) > min(precision(f), precision(g)) || !isequal(f, g + h))
   end

   # Non-integral domain
   R = ResidueRing(JuliaZZ, 6)
   S, x = LaurentSeriesRing(R, 10, "x")
   for iter = 1:500
      f = rand(S, -12:12, 0:5)
      g = deepcopy(f)
      h = R()
      while iszero(h)
         h = rand(S, -12:12, 0:5)
      end

      @test f == g
      @test isequal(f,  g)
      @test (precision(h) > min(precision(f), precision(g)) || f != g + h)
      @test (precision(h) > min(precision(f), precision(g)) || !isequal(f, g + h))
   end

   println("PASS")
end

function test_laurent_series_adhoc_comparison()
   print("Generic.LaurentSeries.adhoc_comparison...")

   # Exact ring
   R, x = LaurentSeriesRing(JuliaZZ, 10, "x")
   for iter = 1:500
      f = R()
      while f == 0
         f = rand(R, 0:0, -10:10)
      end
      f += rand(R, 1:12, -10:10)
      c1 = rand(JuliaZZ, -10:10)
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

   # Inexact field
   R, x = LaurentSeriesField(JuliaRealField, 10, "x")
   for iter = 1:500
      f = R()
      while isapprox(f, R())
         f = rand(R, 0:0, -1:1)
      end
      f += rand(R, 1:12, -1:1)
      c1 = rand(JuliaZZ, -10:10)
      d1 = rand(JuliaRealField, -1:1)

      @test R(c1) == c1
      @test c1 == R(c1)
      @test R(d1) == d1
      @test d1 == R(d1)

      @test R(c1) != c1 + f
      @test c1 != R(c1) + f
      @test R(d1) != d1 + f
      @test d1 != R(d1) + f
   end

   # Non-integral domain
   R = ResidueRing(JuliaZZ, 6)
   S, x = LaurentSeriesRing(R, 10, "x")
   for iter = 1:500
      f = S()
      while f == 0
         f = rand(S, 0:0, 0:5)
      end
      f += rand(S, 1:12, 0:5)
      c1 = rand(JuliaZZ, -10:10)
      d1 = rand(zz, -10:10)
      a1 = rand(R, 0:5)

      @test S(a1) == a1
      @test a1 == S(a1)
      @test S(c1) == c1
      @test c1 == S(c1)
      @test S(d1) == d1
      @test d1 == S(d1)

      @test S(a1) != a1 + f
      @test a1 != S(a1) + f
      @test S(c1) != c1 + f
      @test c1 != S(c1) + f
      @test S(d1) != d1 + f
      @test d1 != S(d1) + f
   end

   # Generic tower
   R, x = JuliaZZ["x"]
   S, y = LaurentSeriesRing(R, 10, "y")
   for iter = 1:100
      f = S()
      while f == 0
         f = rand(S, 0:0, 0:5, -10:10)
      end
      f += rand(S, 1:12, 0:5, -10:10)
      c1 = rand(JuliaZZ, -10:10)
      d1 = rand(R, 0:5, -10:10)

      @test S(c1) == c1
      @test c1 == S(c1)
      @test S(d1) == d1
      @test d1 == S(d1)

      @test S(c1) != c1 + f
      @test c1 != S(c1) + f
      @test S(d1) != d1 + f
      @test d1 != S(d1) + f
   end

   println("PASS")
end

function test_laurent_series_powering()
   print("Generic.LaurentSeries.powering...")

   # Exact ring
   R, x = LaurentSeriesRing(JuliaZZ, 10, "x")

   for iter = 1:100
      f = rand(R, -12:12, -10:10)
      r2 = R(1)

      for expn = 0:10
         r1 = f^expn

         @test (f == 0 && expn == 0 && r1 == 0) || isequal(r1, r2)

         r2 *= f
      end
   end

   # Inexact field
   R, x = LaurentSeriesField(JuliaRealField, 10, "x")

   for iter = 1:100
      f = rand(R, -12:12, -1:1)
      r2 = R(1)

      for expn = 0:4 # cannot set high power here
         r1 = f^expn

         @test (f == 0 && expn == 0 && r1 == 0) || isapprox(r1, r2)

         r2 *= f
      end
   end

   # Non-integral domain
   for iter = 1:100
      n = rand(2:26)

      Zn = ResidueRing(JuliaZZ, n)
      R, x = LaurentSeriesRing(Zn, 10, "x")

      f = rand(R, -12:12, 0:n - 1)
      r2 = R(1)

      for expn = 0:10
         r1 = f^expn

         @test (f == 0 && expn == 0 && r1 == 0) || isequal(r1, r2)

         r2 *= f
      end
   end

   println("PASS")
end

function test_laurent_series_shift()
   print("Generic.LaurentSeries.shift...")

   # Exact ring
   R, x = LaurentSeriesRing(JuliaZZ, 10, "x")
   for iter = 1:300
      f = rand(R, -12:12, -10:10)
      s = rand(0:12)

      @test isequal(shift_right(shift_left(f, s), s), f)
      @test isequal(shift_left(f, s), x^s*f)
      @test precision(shift_right(f, s)) == precision(f) - s
   end

   # Inexact field
   R, x = LaurentSeriesField(JuliaRealField, 10, "x")
   for iter = 1:300
      f = rand(R, -12:12, -1:1)
      s = rand(0:12)

      @test isapprox(shift_right(shift_left(f, s), s), f)
      @test isapprox(shift_left(f, s), x^s*f)
      @test precision(shift_right(f, s)) == precision(f) - s
   end

   # Non-integral domain
   T = ResidueRing(JuliaZZ, 6)
   R, x = LaurentSeriesRing(T, 10, "x")
   for iter = 1:300
      f = rand(R, -12:12, 0:5)
      s = rand(0:12)

      @test isequal(shift_right(shift_left(f, s), s), f)
      @test isequal(shift_left(f, s), x^s*f)
      @test precision(shift_right(f, s)) == precision(f) - s
   end

   println("PASS")
end

function test_laurent_series_truncation()
   print("Generic.LaurentSeries.truncation...")

   # Exact ring
   R, x = LaurentSeriesRing(JuliaZZ, 10, "x")
   for iter = 1:300
      f = rand(R, -12:12, -10:10)
      s = rand(-12:12)

      @test truncate(f, s) == f
      @test isequal(truncate(f, s), f + O(x^s))
      @test precision(truncate(f, s)) == min(precision(f), s)
   end

   # Inexact field
   R, x = LaurentSeriesField(JuliaRealField, 10, "x")
   for iter = 1:300
      f = rand(R, -12:12, -1:1)
      s = rand(-12:12)

      @test truncate(f, s) == f
      @test isapprox(truncate(f, s), f + O(x^s))
      @test precision(truncate(f, s)) == min(precision(f), s)
   end

   # Non-integral domain
   T = ResidueRing(JuliaZZ, 6)
   R, x = LaurentSeriesRing(T, 10, "x")
   for iter = 1:300
      f = rand(R, -12:12, 0:5)
      s = rand(-12:12)

      @test truncate(f, s) == f
      @test isequal(truncate(f, s), f + O(x^s))
      @test precision(truncate(f, s)) == min(precision(f), s)
   end

   println("PASS")
end

function test_laurent_series_inversion()
   print("Generic.LaurentSeries.inversion...")

   # Exact field
   R, x = LaurentSeriesRing(JuliaZZ, 10, "x")
   for iter = 1:300
      f = R()
      while iszero(f) || !isunit(polcoeff(f, 0))
         f = rand(R, -12:12, -10:10)
      end

      @test f*inv(f) == 1
   end

   # Inexact field
   R, x = LaurentSeriesField(JuliaRealField, 10, "x")
   for iter = 1:300
      f = R()
      while iszero(f)
         f = rand(R, -12:12, -1:1)
      end

      @test isapprox(f*inv(f), R(1))
   end

   # Non-integral domain
   T = ResidueRing(JuliaZZ, 6)
   R, x = LaurentSeriesRing(T, 10, "x")
   for iter = 1:300
      f = R()
      while iszero(f) || !isunit(polcoeff(f, 0))
         f = rand(R, -12:12, 0:5)
      end

      @test f*inv(f) == 1
   end

   println("PASS")
end

function test_laurent_series_exact_division()
   print("Generic.LaurentSeries.exact_division...")

   # Exact ring
   R, x = LaurentSeriesRing(JuliaZZ, 10, "x")
   for iter = 1:300
      f = rand(R, -12:12, -10:10)
      g = rand(R, -12:12, -10:10)
      while iszero(g) || !isunit(polcoeff(g, 0))
         g = rand(R, -12:12, -10:10)
      end

      @test divexact(f, g)*g == f
   end

   # Inexact field
   R, x = LaurentSeriesField(JuliaRealField, 10, "x")
   for iter = 1:300
      s = rand(0:12)
      f = rand(R, -12:12, -1:1)
      g = rand(R, -12:12, -1:1)
      while iszero(g) || iszero(polcoeff(g, 0))
         g = rand(R, -12:12, -1:1)
      end

      @test isapprox(divexact(f, g)*g, f)
   end

   # Non-integral domain
   T = ResidueRing(JuliaZZ, 6)
   R, x = LaurentSeriesRing(T, 10, "x")
   for iter = 1:300
      s = rand(0:12)
      f = rand(R, -12:12, 0:5)
      g = rand(R, -12:12, 0:5)
      while iszero(g) || !isunit(polcoeff(g, 0))
         g = rand(R, -12:12, 0:5)
      end

      @test divexact(f, g)*g == f
   end

   println("PASS")
end

function test_laurent_series_adhoc_exact_division()
   print("Generic.LaurentSeries.adhoc_exact_division...")

   # Exact field
   R, x = LaurentSeriesRing(JuliaZZ, 10, "x")
   for iter = 1:300
      f = rand(R, -12:12, -10:10)
      c = JuliaZZ()
      while c == 0
         c = rand(JuliaZZ, -10:10)
      end

      @test isequal(divexact(f*c, c), f)
   end

   # Inexact field
   R, x = LaurentSeriesField(JuliaRealField, 10, "x")
   for iter = 1:300
      f = rand(R, -12:12, -1:1)
      c = JuliaRealField()
      while isapprox(c, 0)
         c = rand(JuliaRealField, -1:1)
      end

      @test isapprox(divexact(f*c, c), f)
   end

   # Non-integral domain
   T = ResidueRing(JuliaZZ, 6)
   R, x = LaurentSeriesRing(T, 10, "x")
   for iter = 1:300
      f = rand(R, -12:12, 0:5)
      c = T()
      while !isunit(c)
         c = rand(T, 0:5)
      end

      @test isequal(divexact(f*c, c), f)
   end

   println("PASS")
end

function test_laurent_series_special_functions()
   print("Generic.LaurentSeries.special_functions...")

   # Exact field
   S, x = LaurentSeriesRing(JuliaQQ, 10, "x")

   for iter = 1:100
      @test exp(x + O(x^iter)) == exp(x + O(x^(iter - 1)))

      f = S()
      while !isunit(f)
         f = rand(S, 0:0, -10:10)
      end
      g = S()
      while !isunit(g) || !isunit(f + g)
         g = rand(S, 0:0, -10:10)
      end
      f *= x
      g *= x

      @test isequal(exp(f)*exp(g), exp(f + g))
   end

   # Inexact field
   S, x = LaurentSeriesField(JuliaRealField, 10, "x")

   for iter = 1:100
      @test isapprox(exp(x + O(x^iter)), exp(x + O(x^(iter - 1))))

      f = S()
      while coeff(f, 0) == 0
         f = rand(S, 0:0, -1:1)
      end
      g = S()
      while coeff(g, 0) == 0 || coeff(f + g, 0) == 0
         g = rand(S, 0:0, -1:1)
      end
      f *= x
      g *= x

      @test isapprox(exp(f)*exp(g), exp(f + g))
   end

   # Non-integral domain
   R = ResidueRing(JuliaZZ, 143)
   S, x = LaurentSeriesRing(R, 10, "x")

   for iter = 1:10
      @test exp(x + O(x^iter)) == exp(x + O(x^(iter - 1)))

      f = S()
      while !isunit(coeff(f, 0))
         f = rand(S, 0:0, -10:10)
      end
      g = S()
      while !isunit(coeff(g, 0)) || !isunit(coeff(f + g, 0))
         g = rand(S, 0:0, -10:10)
      end
      f *= x
      g *= x

      @test isequal(exp(f)*exp(g), exp(f + g))
   end

   println("PASS")
end

function test_gen_laurent_series()
   test_laurent_series_constructors()
   test_laurent_series_manipulation()
   test_laurent_series_unary_ops()
   test_laurent_series_binary_ops()
   test_laurent_series_adhoc_binary_ops()
   test_laurent_series_comparison()
   test_laurent_series_adhoc_comparison()
   test_laurent_series_powering()
   test_laurent_series_shift()
   test_laurent_series_truncation()
   test_laurent_series_exact_division()
   test_laurent_series_adhoc_exact_division()
   test_laurent_series_inversion()
   test_laurent_series_special_functions()

   println("")
end
