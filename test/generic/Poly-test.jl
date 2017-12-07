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

function test_gen_poly_constructors()
   print("Generic.Poly.constructors...")

   R, x = JuliaZZ["x"]
   S, y = R["y"]

   @test elem_type(S) == Generic.Poly{elem_type(R)}
   @test elem_type(Generic.PolyRing{elem_type(R)}) == Generic.Poly{elem_type(R)}
   @test parent_type(Generic.Poly{elem_type(R)}) == Generic.PolyRing{elem_type(R)}

   @test typeof(R) <: Nemo.Ring
   @test typeof(S) <: Generic.PolyRing

   @test isa(y, PolyElem)

   R, x = PolynomialRing(JuliaZZ, "x")
   S, y = PolynomialRing(R, "y")

   @test typeof(S) <: Generic.PolyRing

   @test isa(y, PolyElem)

   T, z = PolynomialRing(S, "z")

   @test typeof(T) <: Generic.PolyRing

   @test isa(z, PolyElem)

   f = x^2 + y^3 + z + 1

   @test isa(f, PolyElem)

   g = S(2)

   @test isa(g, PolyElem)

   h = S(x^2 + 2x + 1)

   @test isa(h, PolyElem)

   j = T(x + 2)

   @test isa(j, PolyElem)

   k = S([x, x + 2, x^2 + 3x + 1])

   @test isa(k, PolyElem)

   l = S(k)

   @test isa(l, PolyElem)

   m = S([1, 2, 3])

   @test isa(m, PolyElem)

   n = S([JuliaZZ(1), JuliaZZ(2), JuliaZZ(3)])

   @test isa(n, PolyElem)

   println("PASS")
end

function test_gen_poly_manipulation()
   print("Generic.Poly.manipulation...")

   R, x = PolynomialRing(JuliaZZ, "x")
   S, y = PolynomialRing(R, "y")

   @test iszero(zero(S))

   @test isone(one(S))

   @test isgen(gen(S))

   @test isunit(one(S))

   f = 2x*y + x^2 + 1

   @test lead(f) == 2x

   @test trail(2x*y + x^2) == x^2

   @test degree(f) == 1

   h = x*y^2 + (x + 1)*y + 3

   @test coeff(h, 2) == x

   @test length(h) == 3

   @test canonical_unit(-x*y + x + 1) == -1

   @test deepcopy(h) == h

   @test isterm(2*x*y^2)

   @test !ismonomial(2*x*y^2)

   @test ismonomial(x*y^2)

   @test !ismonomial(2*x*y^2 + y + 1)

   println("PASS")
end

function test_gen_poly_binary_ops()
   print("Generic.Poly.binary_ops...")

   #  Exact ring
   R, x = PolynomialRing(JuliaZZ, "x")
   for iter = 1:100
      f = rand(R, 0:10, -10:10)
      g = rand(R, 0:10, -10:10)
      h = rand(R, 0:10, -10:10)
      @test f + g == g + f
      @test f + (g + h) == (f + g) + h
      @test f*g == g*f
      @test f*(g + h) == f*g + f*h
      @test (f - h) + (g + h) == f + g
      @test (f + g)*(f - g) == f*f - g*g
      @test f - g == -(g - f)
   end

   #  Inexact field
   R, x = PolynomialRing(JuliaRealField, "x")
   for iter = 1:100
      f = rand(R, 0:10, -1:1)
      g = rand(R, 0:10, -1:1)
      h = rand(R, 0:10, -1:1)
      @test isapprox(f + (g + h), (f + g) + h)
      @test isapprox(f*g, g*f)
      @test isapprox(f*(g + h), f*g + f*h)
      @test isapprox((f - h) + (g + h), f + g)
      @test isapprox((f + g)*(f - g), f*f - g*g)
      @test isapprox(f - g, -(g - f))
   end

   # Non-integral domain
   T = ResidueRing(JuliaZZ, 6)
   R, x = T["x"]
   for iter = 1:100
      f = rand(R, 0:10, 0:5)
      g = rand(R, 0:10, 0:5)
      h = rand(R, 0:10, 0:5)
      @test f + (g + h) == (f + g) + h
      @test f*g == g*f
      @test f*(g + h) == f*g + f*h
      @test (f - h) + (g + h) == f + g
      @test (f + g)*(f - g) == f*f - g*g
      @test f - g == -(g - f)
   end

   println("PASS")
end

function test_gen_poly_adhoc_binary()
   print("Generic.Poly.adhoc_binary...")

   # Exact ring
   R, x = JuliaZZ["x"]
   for iter = 1:500
      f = rand(R, 0:10, -10:10)
      c1 = rand(JuliaZZ, -10:10)
      c2 = rand(JuliaZZ, -10:10)
      d1 = rand(zz, -10:10)
      d2 = rand(zz, -10:10)

      @test c1*f - c2*f == (c1 - c2)*f
      @test c1*f + c2*f == (c1 + c2)*f
      @test d1*f - d2*f == (d1 - d2)*f
      @test d1*f + d2*f == (d1 + d2)*f

      @test f*c1 - f*c2 == f*(c1 - c2)
      @test f*c1 + f*c2 == f*(c1 + c2)
      @test f*d1 - f*d2 == f*(d1 - d2)
      @test f*d1 + f*d2 == f*(d1 + d2)
   end

   # Inexact field
   R, x = JuliaRealField["x"]
   for iter = 1:500
      f = rand(R, 0:10, -1:1)
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
   S, x = R["x"]
   for iter = 1:500
      f = rand(S, 0:10, 0:5)
      c1 = rand(JuliaZZ, -10:10)
      c2 = rand(JuliaZZ, -10:10)
      d1 = rand(zz, -10:10)
      d2 = rand(zz, -10:10)
      a1 = rand(R, 0:5)
      a2 = rand(R, 0:5)

      @test a1*f - a2*f == (a1 - a2)*f
      @test a1*f + a2*f == (a1 + a2)*f
      @test c1*f - c2*f == (c1 - c2)*f
      @test c1*f + c2*f == (c1 + c2)*f
      @test d1*f - d2*f == (d1 - d2)*f
      @test d1*f + d2*f == (d1 + d2)*f

      @test f*a1 - f*a2 == f*(a1 - a2)
      @test f*a1 + f*a2 == f*(a1 + a2)
      @test f*c1 - f*c2 == f*(c1 - c2)
      @test f*c1 + f*c2 == f*(c1 + c2)
      @test f*d1 - f*d2 == f*(d1 - d2)
      @test f*d1 + f*d2 == f*(d1 + d2)
   end

   # Generic tower
   R, x = JuliaZZ["x"]
   S, y = R["y"]
   for iter = 1:100
      f = rand(S, 0:10, 0:5, -10:10)
      c1 = rand(JuliaZZ, -10:10)
      c2 = rand(JuliaZZ, -10:10)
      d1 = rand(R, 0:5, -10:10)
      d2 = rand(R, 0:5, -10:10)

      @test c1*f - c2*f == (c1 - c2)*f
      @test c1*f + c2*f == (c1 + c2)*f
      @test d1*f - d2*f == (d1 - d2)*f
      @test d1*f + d2*f == (d1 + d2)*f

      @test f*c1 - f*c2 == f*(c1 - c2)
      @test f*c1 + f*c2 == f*(c1 + c2)
      @test f*d1 - f*d2 == f*(d1 - d2)
      @test f*d1 + f*d2 == f*(d1 + d2)
   end

   println("PASS")
end

function test_gen_poly_comparison()
   print("Generic.Poly.comparison...")

   # Exact ring
   R, x = JuliaZZ["x"]
   for iter = 1:500
      f = rand(R, 0:10, -10:10)
      g = deepcopy(f)
      h = R()
      while iszero(h)
         h = rand(R, 0:10, -10:10)
      end

      @test f == g
      @test isequal(f, g)
      @test f != g + h
   end

   # Inexact field
   R, x = JuliaRealField["x"]
   for iter = 1:500
      f = rand(R, 0:10, -1:1)
      g = deepcopy(f)
      h = R()
      while iszero(h)
         h = rand(R, 0:10, -1:1)
      end

      @test f == g
      @test isequal(f, g)
      @test f != g + h
   end

   # Non-integral domain
   R = ResidueRing(JuliaZZ, 6)
   S, x = R["x"]
   for iter = 1:500
      f = rand(S, 0:10, 0:5)
      g = deepcopy(f)
      h = R()
      while iszero(h)
         h = rand(S, 0:10, 0:5)
      end

      @test f == g
      @test isequal(f, g)
      @test f != g + h
   end

   println("PASS")
end

function test_gen_poly_adhoc_comparison()
   print("Generic.Poly.adhoc_comparison...")

   # Exact ring
   R, x = JuliaZZ["x"]
   for iter = 1:500
      f = R()
      while iszero(f)
         f = rand(R, 0:10, -10:10)
      end
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
   R, x = JuliaRealField["x"]
   for iter = 1:500
      f = R()
      while iszero(f)
         f = rand(R, 0:10, -1:1)
      end
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
   S, x = R["x"]
   for iter = 1:500
      f = S()
      while iszero(f)
         f = rand(S, 0:10, 0:5)
      end
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
   S, y = R["y"]
   for iter = 1:100
      f = S()
      while iszero(f)
         f = rand(S, 0:10, 0:5, -10:10)
      end
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

function test_gen_poly_unary_ops()
   print("Generic.Poly.unary_ops...")

   #  Exact ring
   R, x = PolynomialRing(JuliaZZ, "x")
   for iter = 1:300
      f = rand(R, 0:10, -10:10)

      @test -(-f) == f
      @test iszero(f + (-f))
   end

   #  Inexact field
   R, x = PolynomialRing(JuliaRealField, "x")
   for iter = 1:300
      f = rand(R, 0:10, -1:1)

      @test -(-f) == f
      @test iszero(f + (-f))
   end

   # Non-integral domain
   T = ResidueRing(JuliaZZ, 6)
   R, x = T["x"]
   for iter = 1:300
      f = rand(R, 0:10, 0:5)

      @test -(-f) == f
      @test iszero(f + (-f))
   end

   println("PASS")
end

function test_gen_poly_truncation()
   print("Generic.Poly.truncation...")

   #  Exact ring
   R, x = PolynomialRing(JuliaZZ, "x")
   for iter = 1:300
      f = rand(R, 0:10, -10:10)
      g = rand(R, 0:10, -10:10)
      n = rand(0:20)

      @test truncate(f*g, n) == mullow(f, g, n)
   end

   #  Inexact field
   R, x = PolynomialRing(JuliaRealField, "x")
   for iter = 1:300
      f = rand(R, 0:10, -1:1)
      g = rand(R, 0:10, -1:1)
      n = rand(0:20)

      @test isapprox(truncate(f*g, n), mullow(f, g, n))
   end

   # Non-integral domain
   T = ResidueRing(JuliaZZ, 6)
   R, x = T["x"]
   for iter = 1:300
      f = rand(R, 0:10, 0:5)
      g = rand(R, 0:10, 0:5)
      n = rand(0:20)

      r = mullow(f, g, n)

      @test truncate(f*g, n) == r
      @test r == 0 || !iszero(lead(r))
   end

   println("PASS")
end

function test_gen_poly_reverse()
   print("Generic.Poly.reverse...")

   #  Exact ring
   R, x = JuliaZZ["x"]
   for iter = 1:300
      f = rand(R, 0:10, -10:10)
      len = rand(length(f):12)
      frev = reverse(f, len)

      shift = 0
      for i = 1:len
         if coeff(f, i - 1) != 0
            break
         end
         shift += 1
      end

      @test length(frev) == len - shift
      @test f == reverse(frev, len)
   end

   #  Inexact field
   R, x = PolynomialRing(JuliaRealField, "x")
   for iter = 1:300
      f = rand(R, 0:10, -1:1)
      len = rand(length(f):12)
      frev = reverse(f, len)

      shift = 0
      for i = 1:len
         if coeff(f, i - 1) != 0
            break
         end
         shift += 1
      end

      @test length(frev) == len - shift
      @test f == reverse(frev, len)
   end

   #  Non-integral domain
   T = ResidueRing(JuliaZZ, 6)
   R, x = T["x"]
   for iter = 1:300
      f = rand(R, 0:10, 0:5)
      len = rand(length(f):12)
      frev = reverse(f, len)

      shift = 0
      for i = 1:len
         if coeff(f, i - 1) != 0
            break
         end
         shift += 1
      end

      @test length(frev) == len - shift
      @test f == reverse(frev, len)
   end

   println("PASS")
end

function test_gen_poly_shift()
   print("Generic.Poly.shift...")

   # Exact ring
   R, x = JuliaZZ["x"]
   for iter = 1:300
      f = rand(R, 0:10, -10:10)
      s = rand(0:10)
      g = s == 0 ? R() : rand(R, 0:s - 1, -10:10)

      @test shift_right(shift_left(f, s) + g, s) == f
      @test shift_left(f, s) == x^s*f
      @test length(shift_right(f, s)) == max(0, length(f) - s)
   end

   # Inexact field
   R, x = PolynomialRing(JuliaRealField, "x")
   for iter = 1:300
      f = rand(R, 0:10, -1:1)
      s = rand(0:10)
      g = s == 0 ? R() : rand(R, 0:s - 1, -1:1)

      @test shift_right(shift_left(f, s) + g, s) == f
      @test shift_left(f, s) == x^s*f
      @test length(shift_right(f, s)) == max(0, length(f) - s)
   end

   # Non-integral domain
   T = ResidueRing(JuliaZZ, 6)
   R, x = T["x"]
   for iter = 1:300
      f = rand(R, 0:10, 0:5)
      s = rand(0:10)
      g = s == 0 ? R() : rand(R, 0:s - 1, 0:5)

      @test shift_right(shift_left(f, s) + g, s) == f
      @test shift_left(f, s) == x^s*f
      @test length(shift_right(f, s)) == max(0, length(f) - s)
   end

   println("PASS")
end

function test_gen_poly_powering()
   print("Generic.Poly.powering...")

   # Exact ring
   R, x = PolynomialRing(JuliaZZ, "x")

   for iter = 1:10
      f = rand(R, 0:10, -10:10)
      r2 = R(1)

      for expn = 0:10
         r1 = f^expn

         @test (f == 0 && expn == 0 && r1 == 0) || r1 == r2

         r2 *= f
      end
   end

   # Inexact field
   R, x = PolynomialRing(JuliaRealField, "x")

   for iter = 1:10
      f = rand(R, 0:10, -1:1)
      r2 = R(1)

      for expn = 0:4 # cannot set high power here
         r1 = f^expn

         @test (f == 0 && expn == 0 && r1 == 0) || isapprox(r1, r2)

         r2 *= f
      end
   end

   # Non-integral domain
   for iter = 1:10
      n = rand(2:26)

      Zn = ResidueRing(JuliaZZ, n)
      R, x = PolynomialRing(Zn, "x")

      f = rand(R, 0:10, 0:n - 1)
      r2 = R(1)

      for expn = 0:10
         r1 = f^expn

         @test (f == 0 && expn == 0 && r1 == 0) || r1 == r2

         r2 *= f
      end
   end

   println("PASS")
end

function test_gen_poly_modular_arithmetic()
   print("Generic.Poly.modular_arithmetic...")

   # Exact ring
   R = ResidueRing(JuliaZZ, 23)
   S, x = PolynomialRing(R, "x")

   for iter = 1:100
      f = rand(S, 0:5, 0:22)
      g = rand(S, 0:5, 0:22)
      h = rand(S, 0:5, 0:22)
      k = S()
      while k == 0
         k = rand(S, 0:5, 0:22)
      end

      @test mulmod(mulmod(f, g, k), h, k) == mulmod(f, mulmod(g, h, k), k)
   end

   for iter = 1:100
      f = S()
      g = S()
      while f == 0 || g == 0 || gcd(f, g) != 1
         f = rand(S, 0:5, 0:22)
         g = rand(S, 0:5, 0:22)
      end

      @test mulmod(invmod(f, g), f, g) == mod(S(1), g)
   end

   for iter = 1:100
      f = rand(S, 0:5, 0:22)
      g = S()
      while g == 0
         g = rand(S, 0:5, 0:22)
      end
      p = mod(S(1), g)

      for expn = 0:5
         r = powmod(f, expn, g)

         @test (f == 0 && expn == 0 && r == 0) || r == p

         p = mulmod(p, f, g)
      end
   end

   # Inexact field
   S, x = PolynomialRing(JuliaRealField, "x")

   for iter = 1:100
      f = rand(S, 0:5, -1:1)
      g = rand(S, 0:5, -1:1)
      h = rand(S, 0:5, -1:1)
      k = R()
      while k == 0
         k = rand(S, 0:5, -1:1)
      end

      @test isapprox(mulmod(mulmod(f, g, k), h, k), mulmod(f, mulmod(g, h, k), k))
   end

   for iter = 1:100
      f = S()
      g = S()
      while f == 0 || g == 0 || gcd(f, g) != 1
         f = rand(S, 0:5, -1:1)
         g = rand(S, 0:5, -1:1)
      end

      @test isapprox(mulmod(invmod(f, g), f, g), mod(S(1), g))
   end

   for iter = 1:100
      f = rand(S, 0:5, -1:1)
      g = S()
      while g == 0
         g = rand(S, 0:5, -1:1)
      end
      p = mod(S(1), g)

      for expn = 0:5
         r = powmod(f, expn, g)

         @test (f == 0 && expn == 0 && r == 0) || isapprox(r, p)

         p = mulmod(p, f, g)
      end
   end

   # Exact field
   R, x = PolynomialRing(JuliaQQ, "y")

   for iter = 1:10
      f = rand(R, 0:5, -10:10)
      g = rand(R, 0:5, -10:10)
      h = rand(R, 0:5, -10:10)
      k = R()
      while k == 0
         k = rand(R, 0:5, -10:10)
      end

      @test mulmod(mulmod(f, g, k), h, k) == mulmod(f, mulmod(g, h, k), k)
   end

   for iter = 1:10
      f = R()
      g = R()
      while f == 0 || g == 0 || gcd(f, g) != 1
         f = rand(R, 0:5, -10:10)
         g = rand(R, 0:5, -10:10)
      end

      @test mulmod(invmod(f, g), f, g) == mod(R(1), g)
   end

   for iter = 1:10
      f = rand(R, 0:5, -10:10)
      g = R()
      while g == 0
         g = rand(R, 0:5, -10:10)
      end
      p = mod(R(1), g)

      for expn = 0:5
         r = powmod(f, expn, g)

         @test (f == 0 && expn == 0 && r == 0) || r == p

         p = mulmod(p, f, g)
      end
   end

   println("PASS")
end

function test_gen_poly_exact_division()
   print("Generic.Poly.exact_division...")

   # Exact ring
   R, x = PolynomialRing(JuliaZZ, "x")

   for iter = 1:100
      f = rand(R, 0:10, -100:100)
      g = R()
      while g == 0
         g = rand(R, 0:10, -100:100)
      end

      @test divexact(f*g, g) == f
   end

   # Inexact field
   R, x = PolynomialRing(JuliaRealField, "x")

   for iter = 1:100
      f = rand(R, 0:10, -1:1)
      g = R()
      while g == 0
         g = rand(R, 0:10, -1:1)
      end

      @test isapprox(divexact(f*g, g), f)
   end

   # Characteristic p ring
   n = 23
   Zn = ResidueRing(JuliaZZ, n)
   R, x = PolynomialRing(Zn, "x")

   for iter = 1:100
      f = rand(R, 0:10, 0:n - 1)
      g = R()
      while g == 0
         g = rand(R, 0:10, 0:n - 1)
      end

      @test divexact(f*g, g) == f
   end

   println("PASS")
end

function test_gen_poly_adhoc_exact_division()
   print("Generic.Poly.adhoc_exact_division...")

   # Exact ring
   R, x = PolynomialRing(JuliaZZ, "x")

   for iter = 1:100
      f = rand(R, 0:10, -100:100)
      g = JuliaZZ()
      while g == 0
         g = rand(JuliaZZ, -10:10)
      end

      @test divexact(f*g, g) == f

      h = 0
      while h == 0
         h = rand(-10:10)
      end

      @test divexact(f*h, h) == f
   end

   # Inexact field
   R, x = PolynomialRing(JuliaRealField, "x")

   for iter = 1:100
      f = rand(R, 0:10, -1:1)
      g = JuliaZZ()
      while g == 0
         g = rand(JuliaRealField, -1:1)
      end

      @test isapprox(divexact(f*g, g), f)

      h = 0
      while h == 0
         h = rand(-10:10)
      end

      @test isapprox(divexact(f*h, h), f)
   end

   # Characteristic p ring
   n = 23
   Zn = ResidueRing(JuliaZZ, n)
   R, x = PolynomialRing(Zn, "x")

   for iter = 1:100
      f = rand(R, 0:10, 0:22)
      g = rand(Zn, 1:22)

      @test divexact(f*g, g) == f

      h = 0
      while (h % n) == 0
         h = rand(-100:100)
      end

      @test divexact(f*h, h) == f
   end

   # Generic tower
   R, x = PolynomialRing(JuliaZZ, "x")
   S, y = PolynomialRing(R, "y")

   for iter = 1:100
      f = rand(S, 0:10, 0:10, -100:100)
      g = R()
      while g == 0
         g = rand(R, 0:10, -100:100)
      end

      @test divexact(f*g, g) == f

      h = JuliaZZ()
      while h == 0
         h = rand(JuliaZZ, -10:10)
      end

      @test divexact(f*h, h) == f
   end

   println("PASS")
end

function test_gen_poly_euclidean_division()
   print("Generic.Poly.euclidean_division...")

   # Exact ring
   R = ResidueRing(JuliaZZ, 23)
   S, x = PolynomialRing(R, "x")

   for iter = 1:100
      f = rand(S, 0:5, 0:22)
      g = rand(S, 0:5, 0:22)
      h = S()
      while h == 0
         h = rand(S, 0:5, 0:22)
      end

      @test mod(f + g, h) == mod(f, h) + mod(g, h)
   end

   for iter = 1:10
      f = rand(S, 0:5, 0:22)
      g = S()
      while g == 0
         g = rand(S, 0:5, 0:22)
      end

      q, r = divrem(f, g)
      @test q*g + r == f

      @test mod(f, g) == r
   end

   # Inexact field
   R, x = PolynomialRing(JuliaRealField, "x")

   for iter = 1:100
      f = rand(R, 0:5, -1:1)
      g = rand(R, 0:5, -1:1)
      h = R()
      while h == 0
         h = rand(R, 0:5, -1:1)
      end

      @test isapprox(mod(f + g, h), mod(f, h) + mod(g, h))
   end

   for iter = 1:10
      f = rand(R, 0:5, -1:1)
      g = R()
      while g == 0
         g = rand(R, 0:5, -1:1)
      end

      q, r = divrem(f, g)
      @test isapprox(q*g + r, f)

      @test isapprox(mod(f, g), r)
   end

   # Exact field
   R, x = PolynomialRing(JuliaQQ, "x")

   for iter = 1:100
      f = rand(R, 0:5, -10:10)
      g = rand(R, 0:5, -10:10)
      h = R()
      while h == 0
         h = rand(R, 0:5, -10:10)
      end

      @test mod(f + g, h) == mod(f, h) + mod(g, h)
   end

   for iter = 1:10
      f = rand(R, 0:5, -10:10)
      g = R()
      while g == 0
         g = rand(R, 0:5, -10:10)
      end

      q, r = divrem(f, g)
      @test q*g + r == f

      @test mod(f, g) == r
   end

   println("PASS")
end

function test_gen_poly_pseudodivision()
   print("Generic.Poly.pseudodivision...")

   # Exact ring
   R, x = PolynomialRing(JuliaZZ, "x")

   for iter = 1:100
      f = rand(R, 0:5, -10:10)
      g = R()
      while g == 0
         g = rand(R, 0:5, -10:10)
      end

      q, r = pseudodivrem(f, g)

      if length(f) < length(g)
         @test f == r && q == 0
      else
         @test q*g + r == f*lead(g)^(length(f) - length(g) + 1)
      end

      @test pseudorem(f, g) == r
   end

   # Characteristic p ring
   Zn = ResidueRing(JuliaZZ, 23)
   R, x = PolynomialRing(Zn, "x")

   for iter = 1:100
      f = rand(R, 0:5, 0:22)
      g = R()
      while g == 0
         g = rand(R, 0:5, 0:22)
      end

      q, r = pseudodivrem(f, g)

      if length(f) < length(g)
         @test f == r && q == 0
      else
         @test q*g + r == f*lead(g)^(length(f) - length(g) + 1)
      end

      @test pseudorem(f, g) == r
   end

   println("PASS")
end

function test_gen_poly_content_primpart_gcd()
   print("Generic.Poly.content_primpart_gcd...")

   # Exact ring
   R, x = PolynomialRing(JuliaZZ, "x")

   for iter = 1:100
      f = rand(R, 0:10, -10:10)

      g = R()
      while g == 0
         g = rand(JuliaZZ, -10:10)
      end

      @test content(f*g) == divexact(g, canonical_unit(g))*content(f)

      @test primpart(f*g) == canonical_unit(g)*primpart(f)
   end

   for iter = 1:20
      f = rand(R, 0:10, -10:10)
      g = rand(R, 0:10, -10:10)
      h = R()
      while h == 0
         h = rand(R, 0:10, -10:10)
      end

      @test gcd(f*h, g*h) == divexact(h, canonical_unit(lead(h)))*gcd(f, g)

      @test lcm(f, h) == divexact(f*h, gcd(f, h))
   end

   # Exact field
   R, x = PolynomialRing(JuliaQQ, "x")

   for iter = 1:100
      f = rand(R, 0:5, -10:10)

      g = JuliaQQ()
      while g == 0
         g = rand(JuliaQQ, -10:10)
      end

      @test content(f*g) == content(f)

      @test primpart(f*g) == primpart(f)*g
   end

   for iter = 1:20
      f = rand(R, 0:5, -10:10)
      g = rand(R, 0:5, -10:10)
      h = R()
      while h == 0
         h = rand(R, 0:5, -10:10)
      end

      @test gcd(f*h, g*h) == inv(lead(h))*h*gcd(f, g)
   end

   for iter = 1:10
      f = R()
      g = R()
      while f == 0 || g == 0 || gcd(f, g) != 1
         f = rand(R, 0:5, -10:10)
         g = rand(R, 0:5, -10:10)
      end

      d, inv = gcdinv(f, g)

      @test d == gcd(f, g)

      @test mod(f*inv, g) == mod(R(1), g)
   end

   # Characteristic p ring
   R = ResidueRing(JuliaZZ, 23)
   S, x = PolynomialRing(R, "x")

   for iter = 1:100
      f = rand(S, 0:10, 0:22)
      g = rand(R, 1:22)

      @test content(f*g) == divexact(g, canonical_unit(g))*content(f)

      @test primpart(f*g) == canonical_unit(g)*primpart(f)
   end

   for iter = 1:100
      f = S()
      g = S()
      while f == 0 || g == 0 || gcd(f, g) != 1
         f = rand(S, 0:5, 0:22)
         g = rand(S, 0:5, 0:22)
      end

      d, inv = gcdinv(f, g)

      @test d == gcd(f, g)

      @test mod(f*inv, g) == mod(S(1), g)
   end

   # Characteristic p field
   R = GF(23)
   S, x = PolynomialRing(R, "x")

   for iter = 1:100
      f = rand(S, 0:10)
      g = R()
      while g == 0
         g = rand(R)
      end

      @test content(f*g) == divexact(g, canonical_unit(g))*content(f)

      @test primpart(f*g) == canonical_unit(g)*primpart(f)
   end

   for iter = 1:100
      f = S()
      g = S()
      while f == 0 || g == 0 || gcd(f, g) != 1
         f = rand(S, 0:5)
         g = rand(S, 0:5)
      end

      d, inv = gcdinv(f, g)

      @test d == gcd(f, g)

      @test mod(f*inv, g) == mod(S(1), g)
   end

   println("PASS")
end

function test_gen_poly_evaluation()
   print("Generic.Poly.evaluation...")

   # Exact ring
   R, x = PolynomialRing(JuliaZZ, "x")

   for iter in 1:10
      f = rand(R, 0:4, -10:10)
      g = rand(R, 0:4, -10:10)

      d = rand(JuliaZZ, -10:10)

      @test evaluate(g, evaluate(f, d)) == evaluate(subst(g, f), d)
   end

   for iter in 1:10
      f = rand(R, 0:4, -10:10)
      g = rand(R, 0:4, -10:10)

      d = rand(-10:10)

      @test evaluate(g, evaluate(f, d)) == evaluate(subst(g, f), d)
   end

   # Inexact field
   R, x = PolynomialRing(JuliaRealField, "x")

   for iter in 1:10
      f = rand(R, 0:4, 0:1)
      g = rand(R, 0:4, 0:1)

      d = rand(JuliaRealField, 0:1)

      @test isapprox(evaluate(g, evaluate(f, d)), evaluate(subst(g, f), d))
   end

   for iter in 1:10
      f = rand(R, 0:4, 0:1)
      g = rand(R, 0:4, 0:1)

      d = rand(-10:10)

      @test isapprox(evaluate(g, evaluate(f, d)), evaluate(subst(g, f), d))
   end

   # Non-integral domain
   Zn = ResidueRing(JuliaZZ, 23)
   R, x = PolynomialRing(Zn, "x")

   for iter in 1:10
      f = rand(R, 0:4, 0:22)
      g = rand(R, 0:4, 0:22)

      d = rand(Zn, 0:22)

      @test evaluate(g, evaluate(f, d)) == evaluate(subst(g, f), d)
   end

   for iter in 1:10
      f = rand(R, 0:4, 0:22)
      g = rand(R, 0:4, 0:22)

      d = rand(-100:100)

      @test evaluate(g, evaluate(f, d)) == evaluate(subst(g, f), d)
   end

   println("PASS")
end

function test_gen_poly_composition()
   print("Generic.Poly.composition...")

   # Exact ring
   R, x = PolynomialRing(JuliaZZ, "x")

   for iter in 1:10
      f = rand(R, 0:5, -10:10)
      g = rand(R, 0:5, -10:10)
      h = rand(R, 0:5, -10:10)

      @test compose(f, compose(g, h)) == compose(compose(f, g), h)
   end

   # Inexact field
   R, x = PolynomialRing(JuliaRealField, "x")

   for iter in 1:10
      f = rand(R, 0:5, 0:1)
      g = rand(R, 0:5, 0:1)
      h = rand(R, 0:5, 0:1)

      @test isapprox(compose(f, compose(g, h)), compose(compose(f, g), h))
   end

   # Non-integral domain
   Zn = ResidueRing(JuliaZZ, 6)
   R, x = PolynomialRing(Zn, "x")

   for iter in 1:10
      f = rand(R, 0:5, 0:5)
      g = rand(R, 0:5, 0:5)
      h = rand(R, 0:5, 0:5)

      @test compose(f, compose(g, h)) == compose(compose(f, g), h)
   end

   println("PASS")
end

function test_gen_poly_derivative()
   print("Generic.Poly.derivative...")

   # Exact ring
   R, x = PolynomialRing(JuliaZZ, "x")

   for iter in 1:10
      f = rand(R, 0:4, -100:100)
      g = rand(R, 0:4, -100:100)

      @test derivative(f + g) == derivative(g) + derivative(f)

      @test derivative(g*f) == derivative(g)*f + derivative(f)*g
   end

   # Inexact field
   R, x = PolynomialRing(JuliaRealField, "x")

   for iter in 1:10
      f = rand(R, 0:4, 0:1)
      g = rand(R, 0:4, 0:1)

      @test isapprox(derivative(f + g), derivative(g) + derivative(f))

      @test isapprox(derivative(g*f), derivative(g)*f + derivative(f)*g)
   end

   # Non-integral domain
   Zn = ResidueRing(JuliaZZ, 6)
   R, x = PolynomialRing(Zn, "x")

   for iter in 1:10
      f = rand(R, 0:4, 0:5)
      g = rand(R, 0:4, 0:5)

      @test derivative(f + g) == derivative(g) + derivative(f)

      @test derivative(g*f) == derivative(g)*f + derivative(f)*g
   end

   println("PASS")
end

function test_gen_poly_integral()
   print("Generic.Poly.integral...")

   # Exact field
   R, x = PolynomialRing(JuliaQQ, "x")

   for iter in 1:10
      f = rand(R, 0:10, -100:100)

      @test derivative(integral(f)) == f

      g = rand(R, 0:2, -100:100)

      @test integral(f + g) == integral(g) + integral(f)
      @test integral(f)*integral(g) == integral(integral(f)*g + integral(g)*f)
   end

   # Characteristic p ring
   Zn = ResidueRing(JuliaZZ, 23)
   R, x = PolynomialRing(Zn, "x")

   for iter in 1:10
      f = rand(R, 0:10, 0:22)

      @test derivative(integral(f)) == f

      g = rand(R, 0:10, 0:22)

      @test integral(f + g) == integral(g) + integral(f)
      @test integral(f)*integral(g) == integral(integral(f)*g + integral(g)*f)
   end

   # Inexact field
   R, x = PolynomialRing(JuliaRealField, "x")

   for iter in 1:10
      f = rand(R, 0:10, 0:1)

      @test isapprox(derivative(integral(f)), f)

      g = rand(R, 0:10, 0:1)

      @test isapprox(integral(f + g), integral(g) + integral(f))
      @test isapprox(integral(f)*integral(g), integral(integral(f)*g + integral(g)*f))
   end

   println("PASS")
end

function test_gen_poly_resultant()
   print("Generic.Poly.resultant...")

   # Exact ring
   R, x = PolynomialRing(JuliaZZ, "x")

   for iter in 1:10
      f = rand(R, 0:5, -10:10)
      g = rand(R, 0:5, -10:10)
      h = rand(R, 0:5, -10:10)

      @test resultant(f*g, h) == resultant(f, h) * resultant(g, h)
      @test resultant(f, g*h) == resultant(f, g) * resultant(f, h)

      @test resultant(f, g) == resultant_subresultant(f, g)
      @test resultant_ducos(f, g) == resultant_subresultant(f, g)
   end

   # Exact field
   R, x = PolynomialRing(JuliaQQ, "x")

   for iter in 1:10
      f = rand(R, 0:5, -10:10)
      g = rand(R, 0:5, -10:10)
      h = rand(R, 0:5, -10:10)

      @test resultant(f*g, h) == resultant(f, h) * resultant(g, h)
      @test resultant(f, g*h) == resultant(f, g) * resultant(f, h)

      @test resultant(f, g) == resultant_subresultant(f, g)
      @test resultant_ducos(f, g) == resultant_subresultant(f, g)
      @test resultant(f, g) == Generic.resultant_lehmer(f, g)
   end

   # Characteristic p ring
   Zn = ResidueRing(JuliaZZ, 23)
   R, x = PolynomialRing(Zn, "x")

   for iter in 1:10
      f = rand(R, 0:5, 0:22)
      g = rand(R, 0:5, 0:22)
      h = rand(R, 0:5, 0:22)

      @test resultant(f*g, h) == resultant(f, h)*resultant(g, h)
      @test resultant(f, g*h) == resultant(f, g)*resultant(f, h)

      @test resultant(f, g) == resultant_subresultant(f, g)
      @test resultant_ducos(f, g) == resultant_subresultant(f, g)
   end

   # Characteristic p field
   R, x = PolynomialRing(GF(23), "x")

   for iter in 1:10
      f = rand(R, 0:5)
      g = rand(R, 0:5)
      h = rand(R, 0:5)

      @test resultant(f*g, h) == resultant(f, h)*resultant(g, h)
      @test resultant(f, g*h) == resultant(f, g)*resultant(f, h)

      @test resultant(f, g) == resultant_subresultant(f, g)
      @test resultant_ducos(f, g) == resultant_subresultant(f, g)
   end

   # Inexact field
   R, x = PolynomialRing(JuliaRealField, "x")

   for iter in 1:10
      f = rand(R, 0:5, 0:1)
      g = rand(R, 0:5, 0:1)
      h = rand(R, 0:5, 0:1)

      @test isapprox(resultant(f*g, h), resultant(f, h)*resultant(g, h))
      @test isapprox(resultant(f, g*h), resultant(f, g)*resultant(f, h))

      @test isapprox(resultant(f, g), resultant_subresultant(f, g))
   end

   # Non-integral domain
   Zn = ResidueRing(JuliaZZ, 6)
   R, x = PolynomialRing(Zn, "x")

   for iter in 1:10
      f = rand(R, 0:5, 0:5)
      g = rand(R, 0:5, 0:5)
      h = rand(R, 0:5, 0:5)

      @test lead(f)*lead(g) == 0 || resultant(f*g, h) == resultant(f, h)*resultant(g, h)
      @test lead(g)*lead(h) == 0 || resultant(f, g*h) == resultant(f, g)*resultant(f, h)
   end

   println("PASS")
end

function test_gen_poly_discriminant()
   print("Generic.Poly.discriminant...")

   # Exact ring
   R, x = PolynomialRing(JuliaZZ, "x")

   for iter = 1:100
      f = R()
      g = R()
      while length(f) < 2 || length(g) < 2
         f = rand(R, 1:5, -10:10)
         g = rand(R, 1:5, -10:10)
      end

      # See http://www2.math.uu.se/~svante/papers/sjN5.pdf 3.10
      # The identity on Wikipedia is incorrect as of 07.10.2017
      @test discriminant(f*g) == discriminant(f)*discriminant(g)*resultant(g, f)^2
   end

   # Inexact field
   R, x = PolynomialRing(JuliaRealField, "x")

   for iter = 1:100
      f = R()
      g = R()
      while length(f) < 2 || length(g) < 2
         f = rand(R, 0:10, 0:1)
         g = rand(R, 0:10, 0:1)
      end

      # See http://www2.math.uu.se/~svante/papers/sjN5.pdf 3.10
      # The identity on Wikipedia is incorrect as of 07.10.2017
      @test isapprox(discriminant(f*g), discriminant(f)*discriminant(g)*resultant(g, f)^2)
   end

#   TODO: Fix issue #291
#   # Non-integral domain
#   Zn = ResidueRing(JuliaZZ, 6)
#   R, x = PolynomialRing(Zn, "x")
#
#   for iter = 1:100
#      f = R()
#      g = R()
#      while length(f) < 2 || length(g) < 2
#         f = rand(R, 1:5, 0:5)
#         g = rand(R, 1:5, 0:5)
#      end
#
#      # See http://www2.math.uu.se/~svante/papers/sjN5.pdf 3.10
#      # The identity on Wikipedia is incorrect as of 07.10.2017
#      @test discriminant(f*g) == discriminant(f)*discriminant(g)*resultant(g, f)^2
#   end

   println("PASS")
end

function test_gen_poly_resx()
   print("Generic.Poly.resx...")

   # Exact ring
   R, x = PolynomialRing(JuliaZZ, "x")

   for iter in 1:100
      f = R()
      g = R()
      while length(f) <= 1 && length(g) <= 1
         f = rand(R, 0:5, -10:10)
         g = rand(R, 0:5, -10:10)
      end
      r, u, v = resx(f, g)

      @test u*f + v*g == r
      @test r == resultant(f, g)

      h = rand(R, 0:5, -10:10)
      r, u, v = resx(f*h, g*h)

      @test (u*f + v*g)*h == r
   end

   # Exact field
   R, x = PolynomialRing(JuliaQQ, "x")

   for iter in 1:100
      f = R()
      g = R()
      while length(f) <= 1 && length(g) <= 1
         f = rand(R, 0:5, -10:10)
         g = rand(R, 0:5, -10:10)
      end
      r, u, v = resx(f, g)

      @test u*f + v*g == r
      @test r == resultant(f, g)

      h = rand(R, 0:5, -10:10)
      r, u, v = resx(f*h, g*h)

      @test (u*f + v*g)*h == r
   end

   # Inexact field
   R, x = PolynomialRing(JuliaRealField, "x")

   for iter in 1:100
      f = R()
      g = R()
      while length(f) <= 1 && length(g) <= 1
         f = rand(R, 0:5, 0:1)
         g = rand(R, 0:5, 0:1)
      end
      r, u, v = resx(f, g)

      @test isapprox(u*f + v*g, r)
      @test isapprox(r, resultant(f, g))

      h = rand(R, 0:5, 0:1)
      r, u, v = resx(f*h, g*h)

      @test isapprox((u*f + v*g)*h, r)
   end

   # Characteristic p ring
   Zn = ResidueRing(JuliaZZ, 23)
   R, x = PolynomialRing(Zn, "x")

   for iter in 1:100
      f = R()
      g = R()
      while length(f) <= 1 && length(g) <= 1
         f = rand(R, 0:5, 0:22)
         g = rand(R, 0:5, 0:22)
      end
      r, u, v = resx(f, g)

      @test u*f + v*g == r
      @test r == resultant(f, g)

      h = rand(R, 0:5, 0:22)
      r, u, v = resx(f*h, g*h)

      @test (u*f + v*g)*h == r
   end

   # Characteristic p field
   R, x = PolynomialRing(GF(23), "x")

   for iter in 1:100
      f = R()
      g = R()
      while length(f) <= 1 && length(g) <= 1
         f = rand(R, 0:5)
         g = rand(R, 0:5)
      end
      r, u, v = resx(f, g)

      @test u*f + v*g == r
      @test r == resultant(f, g)

      h = rand(R, 0:5)
      r, u, v = resx(f*h, g*h)

      @test (u*f + v*g)*h == r
   end

#   TODO: Fix issue #293
#   Test will cause impossible inverse in the mean time
#
#   # Non-integral domain
#   Zn = ResidueRing(JuliaZZ, 6)
#   R, x = PolynomialRing(Zn, "x")
#
#   for iter in 1:100
#      f = R()
#      g = R()
#      while length(f) <= 1 && length(g) <= 1
#         f = rand(R, 0:5, 0:5)
#         g = rand(R, 0:5, 0:5)
#      end
#      r, u, v = resx(f, g)
#
#      @test u*f + v*g == r
#      @test r == resultant(f, g)
#
#      h = R()
#      h = rand(R, 0:5, 0:5)
#      r, u, v = resx(f*h, g*h)
#
#      @test (u*f + v*g)*h == r
#   end

   println("PASS")
end

function test_gen_poly_gcdx()
   print("Generic.Poly.gcdx...")

   # Exact field
   R, x = PolynomialRing(JuliaQQ, "x")

   for iter in 1:100
      f = R()
      g = R()
      while length(f) <= 1 && length(g) <= 1
         f = rand(R, 0:5, -10:10)
         g = rand(R, 0:5, -10:10)
      end
      r, u, v = gcdx(f, g)

      @test u*f + v*g == r
      @test r == gcd(f, g)

      h = R()
      h = rand(R, 0:5, -10:10)
      r, u, v = gcdx(f*h, g*h)

      @test (u*f + v*g)*h == r
   end

   # Characteristic p ring
   Zn = ResidueRing(JuliaZZ, 23)
   R, x = PolynomialRing(Zn, "x")

   for iter in 1:100
      f = R()
      g = R()
      while length(f) <= 1 && length(g) <= 1
         f = rand(R, 0:5, 0:22)
         g = rand(R, 0:5, 0:22)
      end
      r, u, v = gcdx(f, g)

      @test u*f + v*g == r
      @test r == gcd(f, g)

      h = rand(R, 0:5, 0:22)
      r, u, v = gcdx(f*h, g*h)

      @test (u*f + v*g)*h == r
   end

   # Characteristic p field
   R, x = PolynomialRing(GF(23), "x")

   for iter in 1:100
      f = R()
      g = R()
      while length(f) <= 1 && length(g) <= 1
         f = rand(R, 0:5)
         g = rand(R, 0:5)
      end
      r, u, v = gcdx(f, g)

      @test u*f + v*g == r
      @test r == gcd(f, g)

      h = rand(R, 0:5)
      r, u, v = gcdx(f*h, g*h)

      @test (u*f + v*g)*h == r
   end

   println("PASS")
end

function test_gen_poly_newton_representation()
   print("Generic.Poly.newton_representation...")

   # Exact ring
   R, x = PolynomialRing(JuliaZZ, "x")

   for iter in 1:10
      f = rand(R, 0:10, -100:100)

      g = deepcopy(f)
      roots = BigInt[rand(JuliaZZ, -10:10) for i in 1:length(f)]
      monomial_to_newton!(g.coeffs, roots)
      newton_to_monomial!(g.coeffs, roots)

      @test f == g
   end

   # Exact field
   R, x = PolynomialRing(JuliaQQ, "x")

   for iter in 1:10
      f = rand(R, 0:10, -100:100)

      g = deepcopy(f)
      roots = Rational{BigInt}[rand(JuliaQQ, -10:10) for i in 1:length(f)]
      monomial_to_newton!(g.coeffs, roots)
      newton_to_monomial!(g.coeffs, roots)

      @test f == g
   end

   # Inexact field
   R, x = PolynomialRing(JuliaRealField, "x")

   for iter in 1:10
      f = rand(R, 0:10, 0:1)

      g = deepcopy(f)
      roots = BigFloat[rand(JuliaRealField, 0:1) for i in 1:length(f)]
      monomial_to_newton!(g.coeffs, roots)
      newton_to_monomial!(g.coeffs, roots)

      @test isapprox(f, g)
   end

   # Characteristic p ring
   Zn = ResidueRing(JuliaZZ, 23)
   R, x = PolynomialRing(Zn, "x")

   for iter in 1:10
      f = rand(R, 0:10, 0:22)

      g = deepcopy(f)
      roots = elem_type(Zn)[rand(Zn, 0:22) for i in 1:length(f)]
      monomial_to_newton!(g.coeffs, roots)
      newton_to_monomial!(g.coeffs, roots)

      @test f == g
   end

   # Characteristic p ring
   K = GF(23)
   R, x = PolynomialRing(K, "x")

   for iter in 1:10
      f = rand(R, 0:10)

      g = deepcopy(f)
      roots = elem_type(K)[rand(K) for i in 1:length(f)]
      monomial_to_newton!(g.coeffs, roots)
      newton_to_monomial!(g.coeffs, roots)

      @test f == g
   end

   # Non-integral domain
   Zn = ResidueRing(JuliaZZ, 6)
   R, x = PolynomialRing(Zn, "x")

   for iter in 1:10
      f = rand(R, 0:10, 0:5)

      g = deepcopy(f)
      roots = elem_type(Zn)[rand(Zn, 0:5) for i in 1:length(f)]
      monomial_to_newton!(g.coeffs, roots)
      newton_to_monomial!(g.coeffs, roots)

      @test f == g
   end

   println("PASS")
end

function test_gen_poly_interpolation()
   print("Generic.Poly.interpolation...")

   # Exact ring
   R, x = PolynomialRing(JuliaZZ, "x")

   for iter in 1:10
      p = R()
      while p == 0
         p = rand(R, 0:10, -10:10)
      end

      xs = BigInt[i for i in 1:length(p)]
      ys = [p(i) for i in 1:length(p)]

      f = interpolate(R, xs, ys)

      @test f == p
   end

   # Exact field
   R, x = PolynomialRing(JuliaQQ, "x")

   for iter in 1:10
      p = R()
      while p == 0
         p = rand(R, 0:10, -10:10)
      end

      xs = Rational{BigInt}[i for i in 1:length(p)]
      ys = [p(i) for i in 1:length(p)]

      f = interpolate(R, xs, ys)

      @test f == p
   end

   # Inexact field
   R, x = PolynomialRing(JuliaRealField, "x")

   for iter in 1:10
      p = R()
      while p == 0
         p = rand(R, 0:10, 0:1)
      end

      xs = BigFloat[i for i in 1:length(p)]
      ys = [p(i) for i in 1:length(p)]

      f = interpolate(R, xs, ys)

      @test isapprox(f, p)
   end

   # Characteristic p ring
   Zn = ResidueRing(JuliaZZ, 23)
   R, x = PolynomialRing(Zn, "x")

   for iter in 1:10
      p = R()
      while p == 0
         p = rand(R, 0:10, 0:22)
      end

      xs = elem_type(Zn)[Zn(i) for i in 1:length(p)]
      ys = [p(i) for i in 1:length(p)]

      f = interpolate(R, xs, ys)

      @test f == p
   end

   # Characteristic p field
   K = GF(23)
   R, x = PolynomialRing(K, "x")

   for iter in 1:10
      p = R()
      while p == 0
         p = rand(R, 0:10)
      end

      xs = elem_type(K)[K(i) for i in 1:length(p)]
      ys = [p(i) for i in 1:length(p)]

      f = interpolate(R, xs, ys)

      @test f == p
   end

#   TODO: Fix issue #294 (if possible)
#   # Non-integral domain
#   Zn = ResidueRing(JuliaZZ, 6)
#   R, x = PolynomialRing(Zn, "x")
#
#   for iter in 1:10
#      p = R()
#      while p == 0
#         p = rand(R, 0:10, 0:5)
#      end
#
#      xs = elem_type(Zn)[Zn(i) for i in 1:length(p)]
#      ys = [p(i) for i in 1:length(p)]
#
#      f = interpolate(R, xs, ys)
#
#      @test f == p
#   end

   println("PASS")
end

function test_gen_poly_special()
   print("Generic.Poly.special...")

   # Exact ring
   R, x = PolynomialRing(JuliaZZ, "x")

   for n in 10:20
      T = chebyshev_t(n, x)
      dT = derivative(T)
      ddT = derivative(dT)

      @test (1 - x^2)*ddT + n^2*T == x*dT

      U = chebyshev_u(n - 1, x)
      dU = derivative(U)
      ddU = derivative(dU)

      @test (1 - x^2)*ddU + (n-1)*(n+1)*U == 3*x*dU

      @test T^2 == 1 + (x^2 - 1)*U^2
   end

   # Exact field
   R, x = PolynomialRing(JuliaQQ, "x")

   for n in 10:20
      T = chebyshev_t(n, x)
      dT = derivative(T)
      ddT = derivative(dT)

      @test (1 - x^2)*ddT + n^2*T == x*dT

      U = chebyshev_u(n - 1, x)
      dU = derivative(U)
      ddU = derivative(dU)

      @test (1 - x^2)*ddU + (n-1)*(n+1)*U == 3*x*dU

      @test T^2 == 1 + (x^2 - 1)*U^2
   end

   # Inexact field
   R, x = PolynomialRing(JuliaRealField, "x")

   for n in 10:20
      T = chebyshev_t(n, x)
      dT = derivative(T)
      ddT = derivative(dT)

      @test (1 - x^2)*ddT + n^2*T == x*dT

      U = chebyshev_u(n - 1, x)
      dU = derivative(U)
      ddU = derivative(dU)

      @test (1 - x^2)*ddU + (n-1)*(n+1)*U == 3*x*dU

      @test T^2 == 1 + (x^2 - 1)*U^2
   end

   # Characteristic p ring
   Zn = ResidueRing(JuliaZZ, 23)
   R, x = PolynomialRing(Zn, "x")

   for n in 10:20
      T = chebyshev_t(n, x)
      dT = derivative(T)
      ddT = derivative(dT)

      @test (1 - x^2)*ddT + n^2*T == x*dT

      U = chebyshev_u(n - 1, x)
      dU = derivative(U)
      ddU = derivative(dU)

      @test (1 - x^2)*ddU + (n-1)*(n+1)*U == 3*x*dU

      @test T^2 == 1 + (x^2 - 1)*U^2
   end

   # Characteristic p field
   R, x = PolynomialRing(GF(23), "x")

   for n in 10:20
      T = chebyshev_t(n, x)
      dT = derivative(T)
      ddT = derivative(dT)

      @test (1 - x^2)*ddT + n^2*T == x*dT

      U = chebyshev_u(n - 1, x)
      dU = derivative(U)
      ddU = derivative(dU)

      @test (1 - x^2)*ddU + (n-1)*(n+1)*U == 3*x*dU

      @test T^2 == 1 + (x^2 - 1)*U^2
   end

   # Non-integral domain
   Zn = ResidueRing(JuliaZZ, 6)
   R, x = PolynomialRing(Zn, "x")

   for n in 10:20
      T = chebyshev_t(n, x)
      dT = derivative(T)
      ddT = derivative(dT)

      @test (1 - x^2)*ddT + n^2*T == x*dT

      U = chebyshev_u(n - 1, x)
      dU = derivative(U)
      ddU = derivative(dU)

      @test (1 - x^2)*ddU + (n-1)*(n+1)*U == 3*x*dU

      @test T^2 == 1 + (x^2 - 1)*U^2
   end

   println("PASS")
end

function test_gen_poly_mul_karatsuba()
   print("Generic.Poly.mul_karatsuba...")

   # Exact ring
   R, x = PolynomialRing(JuliaZZ, "x")
   S, y = PolynomialRing(R, "y")
   T, z = PolynomialRing(S, "z")

   f = x + y + 2z^2 + 1

   @test mul_karatsuba(f^10, f^10) == mul_classical(f^10, f^10)
   @test mul_karatsuba(f^10, f^30) == mul_classical(f^10, f^30)

   println("PASS")
end

function test_gen_poly_mul_ks()
   print("Generic.Poly.mul_ks...")

   # Exact ring
   R, x = PolynomialRing(JuliaZZ, "x")
   S, y = PolynomialRing(R, "y")
   T, z = PolynomialRing(S, "z")

   f = x + y + 2z^2 + 1

   @test mul_ks(f^10, f^10) == mul_classical(f^10, f^10)
   @test mul_ks(f^10, f^30) == mul_classical(f^10, f^30)

   println("PASS")
end

function test_gen_poly_remove_valuation()
   print("Generic.Poly.remove_valuation...")

   # Exact ring
   R, x = PolynomialRing(JuliaZZ, "x")

   for iter = 1:10
      d = true
      f = R()
      g = R()
      while d
         f = R()
         g = R()
         while f == 0 || g == 0
            f = rand(R, 0:10, -10:10)
            g = rand(R, 0:10, -10:10)
         end

         d, q = divides(f, g)
      end

      s = rand(0:10)

      v, q = remove(f*g^s, g)

      @test valuation(f*g^s, g) == s
      @test q == f
      @test v == s

      v, q = divides(f*g, f)

      @test v
      @test q == g

      if length(f) > 1
         v, q = divides(f*g + 1, f)

         @test !v
      end
   end

   # Exact field
   R, x = PolynomialRing(JuliaQQ, "x")

   for iter = 1:10
      d = true
      f = R()
      g = R()
      while d
         f = R()
         g = R()
         while f == 0 || g == 0
            f = rand(R, 0:10, -10:10)
            g = rand(R, 0:10, -10:10)
         end

         d, q = divides(f, g)
      end

      s = rand(0:10)

      v, q = remove(f*g^s, g)

      @test valuation(f*g^s, g) == s
      @test q == f
      @test v == s

      v, q = divides(f*g, f)

      @test v
      @test q == g

      if length(f) > 1
         v, q = divides(f*g + 1, f)

         @test !v
      end
   end

   # Characteristic p ring
   Zn = ResidueRing(JuliaZZ, 23)
   R, x = PolynomialRing(Zn, "x")

   for iter = 1:10
      d = true
      f = R()
      g = R()
      while d
         f = R()
         g = R()
         while f == 0 || g == 0
            f = rand(R, 0:10, 0:22)
            g = rand(R, 0:10, 0:22)
         end

         d, q = divides(f, g)
      end

      s = rand(0:10)

      v, q = remove(f*g^s, g)

      @test valuation(f*g^s, g) == s
      @test q == f
      @test v == s

      v, q = divides(f*g, f)

      @test v
      @test q == g

      if length(f) > 1
         v, q = divides(f*g + 1, f)

         @test !v
      end
   end

   # Characteristic p field
   R, x = PolynomialRing(GF(23), "x")

   for iter = 1:10
      d = true
      f = R()
      g = R()
      while d
         f = R()
         g = R()
         while f == 0 || g == 0
            f = rand(R, 0:10)
            g = rand(R, 0:10)
         end

         d, q = divides(f, g)
      end

      s = rand(0:10)

      v, q = remove(f*g^s, g)

      @test valuation(f*g^s, g) == s
      @test q == f
      @test v == s

      v, q = divides(f*g, f)

      @test v
      @test q == g

      if length(f) > 1
         v, q = divides(f*g + 1, f)

         @test !v
      end
   end

   println("PASS")
end

function test_gen_poly_generic_eval()
   print("Generic.Poly.generic_eval...")

   R, x = PolynomialRing(JuliaZZ, "x")

   for iter in 1:10
      f = rand(R, 0:2, -100:100)
      g = rand(R, 0:2, -100:100)
      h = rand(R, 0:2, -100:100)

      @test f(g(h)) == f(g)(h)
   end

   R, x = PolynomialRing(JuliaZZ, "x")

   f = x
   b = a = JuliaQQ(13)
   for i in 1:5
      g = x^2 + rand(R, 0:1, -1:1)
      f = g(f)
      b = g(b)

      @test b == f(a)
   end

   println("PASS")
end

function test_gen_poly()
   test_gen_poly_constructors()
   test_gen_poly_manipulation()
   test_gen_poly_binary_ops()
   test_gen_poly_adhoc_binary()
   test_gen_poly_comparison()
   test_gen_poly_adhoc_comparison()
   test_gen_poly_unary_ops()
   test_gen_poly_truncation()
   test_gen_poly_reverse()
   test_gen_poly_shift()
   test_gen_poly_powering()
   test_gen_poly_modular_arithmetic()
   test_gen_poly_exact_division()
   test_gen_poly_adhoc_exact_division()
   test_gen_poly_euclidean_division()
   test_gen_poly_pseudodivision()
   test_gen_poly_content_primpart_gcd()
   test_gen_poly_evaluation()
   test_gen_poly_composition()
   test_gen_poly_derivative()
   test_gen_poly_integral()
   test_gen_poly_resultant()
   test_gen_poly_discriminant()
   test_gen_poly_resx()
   test_gen_poly_gcdx()
   test_gen_poly_newton_representation()
   test_gen_poly_interpolation()
   test_gen_poly_special()
   test_gen_poly_mul_karatsuba()
   test_gen_poly_mul_ks()
   test_gen_poly_generic_eval()
   test_gen_poly_remove_valuation()


   println("")
end
