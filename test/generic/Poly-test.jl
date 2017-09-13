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

   R, x = PolynomialRing(JuliaZZ, "x")

#  Remi Imbach 31/07/17: begin
   maxIter = 100
   degmax  = 50
   bitsize = 20
   for iter = 1:maxIter
      f = rand(R, 0:degmax, -2^bitsize:2^bitsize)
      g = rand(R, 0:degmax, -2^bitsize:2^bitsize)
      h = rand(R, 0:degmax, -2^bitsize:2^bitsize)
      @test (f - h) + (g + h) == f + g
      @test (f + g)*(f - g) == f*f - g*g
   end
   #the same over Z/6Z
   T = ResidueRing(JuliaZZ, 6)
   R,x = T["x"]
   for iter = 1:maxIter
      f = rand(R, 0:degmax, -2^bitsize:2^bitsize)
      g = rand(R, 0:degmax, -2^bitsize:2^bitsize)
      h = rand(R, 0:degmax, -2^bitsize:2^bitsize)
      @test (f - h) + (g + h) == f + g
      @test (f + g)*(f - g) == f*f - g*g
   end
#  Remi Imbach 31/07/17: end
   println("PASS")
end

function test_gen_poly_adhoc_binary()
   print("Generic.Poly.adhoc_binary...")
#  Remi Imbach 31/07/17: begin
   R,x = JuliaZZ["x"]
   S,y = R["y"]
   U,z = S["z"]
   maxIter = 30
   degmax = 20
   bitsize = 20
   for iter = 1:maxIter
      f = rand(U, 0:degmax, 0:degmax, 0:degmax, -2^bitsize:2^bitsize)
      c1 = rand(JuliaZZ, -2^bitsize:2^bitsize)
      c2 = rand(JuliaZZ, -2^bitsize:2^bitsize)
      @test c1*f - c2*f == (c1 - c2)*f
   end
   #the same over Z/6Z
   T = ResidueRing(ZZ, 6)
   R,x = T["x"]
   S,y = R["y"]
   U,z = S["z"]
   for iter = 1:maxIter
      f = rand(U, 0:degmax, 0:degmax, 0:degmax, -2^bitsize:2^bitsize)
      c1 = rand(T, -2^bitsize:2^bitsize)
      c2 = rand(T, -2^bitsize:2^bitsize)
      @test c1*f - c2*f == (c1 - c2)*f
   end
#  Remi Imbach 31/07/17: end
   println("PASS")
end

function test_gen_poly_comparison()
   print("Generic.Poly.comparison...")
#  Remi Imbach 31/07/17: begin
   R, x = JuliaZZ["x"]
   maxIter = 100
   degmax = 100
   bitsize = 32
   for iter = 1:maxIter
      f = rand(R, 0:degmax, -2^bitsize:2^bitsize)
      g = f
      @test f == g
      @test isequal(f, g)
      g = f + rand(JuliaZZ, -2^bitsize:2^bitsize) + 1
      @test f != g
   end
   #the same over Z/6Z
   T = ResidueRing(JuliaZZ, 6)
   R, x = T["x"]
   for iter = 1:maxIter
      f = rand(R, 0:degmax, -2^bitsize:2^bitsize)
      g = f
      @test f == g
      @test isequal(f, g)
      g = f + rand(T, 0:4) + 1
      @test f != g
   end
#  Remi Imbach 31/07/17: end
   println("PASS")
end

function test_gen_poly_adhoc_comparison()
   print("Generic.Poly.adhoc_comparison...")

#  Remi Imbach 31/07/17: begin
   R,x = JuliaZZ["x"]
   S,y = R["y"]
   U,z = S["z"]
   maxIter = 100
   bitsize = 32
   for iter = 1:maxIter
      c1 = rand(JuliaZZ, -2^bitsize:2^bitsize)
      @test U(c1) == c1 
      @test U(c1) != y + rand(JuliaZZ, -2^bitsize:2^bitsize)*z
   end
   #the same over Z/6Z
   T = ResidueRing(JuliaZZ, 6)
   R,x = T["x"]
   S,y = R["y"]
   U,z = S["z"]
   for iter = 1:maxIter
      c1 = rand(T, -2^bitsize:2^bitsize)
      @test U(c1) == c1
      @test U(c1) != y + rand(T, 0:100)*z
   end
#  Remi Imbach 31/07/17: end
   println("PASS")
end

function test_gen_poly_unary_ops()
   print("Generic.Poly.unary_ops...")
#  Remi Imbach 31/07/17: begin
   R, x = JuliaZZ["x"]
   maxIter = 100
   degmax = 100
   bitsize = 32
   for iter = 1:maxIter
      f = rand(R, 0:degmax, -2^bitsize:2^bitsize)
      c1 = rand(JuliaZZ, -2^bitsize:2^bitsize)
      mf = -f
      @test -R(c1) == -c1
      @test degree(mf) == degree(f)
      for co = 1:degree(f) + 1
        @test coeff(mf, co) == -coeff(f, co)
      end
   end
   #the same over Z/6Z
   T = ResidueRing(JuliaZZ, 6)
   R, x = T["x"]
   for iter = 1:maxIter
      f = rand(R, 0:degmax, -2^bitsize:2^bitsize)
      c1 = rand(T, -2^bitsize:2^bitsize)
      mf = -f
      @test -R(c1) == -c1
      @test degree(mf) == degree(f)
      for co = 1:degree(f) + 1
        @test coeff(mf, co) == -coeff(f, co)
      end
   end
#  Remi Imbach 31/07/17: end
   println("PASS")
end

function test_gen_poly_truncation()
   print("Generic.Poly.truncation...")
#  Remi Imbach 31/07/17: begin
   R,x = JuliaZZ["x"]
   maxIter = 100
   degmax = 100
   bitsize = 32
   for iter = 1:maxIter
      f = rand(R, 0:degmax, -2^bitsize:2^bitsize)
      t = rand(1:degmax)
      h = truncate(f, t)
      @test degree(h) + 1 <= min(degree(f) + 1, t) # verify degree
      for co = 1:degree(h) + 1 # verify each coeff
         @test coeff(h, co - 1) == coeff(f, co - 1)
      end
   end
#  the same over Z/6Z
   T = ResidueRing(JuliaZZ, 6)
   R,x = T["x"]
   for iter = 1:maxIter
      f = rand(R, 0:degmax, -2^bitsize:2^bitsize)
      t = rand(1:degmax)
      h = truncate(f, t)
      @test degree(h) + 1 <= min(degree(f) + 1, t) # verify degree
      for co = 1:degree(h) + 1 # verify each coeff
         @test coeff(h, co - 1) == coeff(f, co - 1)
      end
   end
#  Remi Imbach 31/07/17: end
   println("PASS")
end

function test_gen_poly_reverse()
   print("Generic.Poly.reverse...")
   R, x = JuliaZZ["x"]
   maxIter = 100
   degmax = 50
   bitsize = 32
   for iter = 1:maxIter
      f = rand(R, 0:degmax, -2^bitsize:2^bitsize)
      len = rand(length(f):degmax + 1)
      f_rev = reverse(f, len)
      if f == 0
        @test f_rev == 0
      else # count the number of trailing 0 coeffs
        it = 0
        while coeff(f, it) == 0
            it += 1
        end
        @test length(f_rev) == len - it
        for co = 1:length(f_rev)
          @test coeff(f_rev, co - 1) == coeff(f, len - co)
        end
      end
   end
#  the same over Z/6Z
   T = ResidueRing(JuliaZZ, 6)
   R, x = T["x"]
   for iter = 1:maxIter
      f = rand(R, 0:degmax, -2^bitsize:2^bitsize)
      len = rand(length(f):degmax + 1)
      f_rev = reverse(f, len)
      if f == 0
        @test f_rev == 0
      else # count the number of trailing 0 coeffs
        it = 0
        while coeff(f, it) == 0
            it = it + 1
        end
        @test length(f_rev) == len - it
        for co = 1:length(f_rev)
          @test coeff(f_rev, co - 1) == coeff(f, len - co)
        end
      end
   end
#  Remi Imbach 31/07/17: end
   println("PASS")
end

function test_gen_poly_shift()
   print("Generic.Poly.shift...")
#  Remi Imbach 31/07/17: begin
   R, x = JuliaZZ["x"]
   maxIter = 100
   degmax = 50
   bitsize = 32
   for iter = 1:maxIter
      f = rand(R, 0:degmax, -2^bitsize:2^bitsize)
      s = rand(0:degmax)
      fsr = shift_right(f, s)
      fsl = shift_left(f, s)
      @test fsl == x^s*f
      f_rev = reverse(f, degree(f) + 1)
      f_rev_trun = truncate(f_rev, max(0, degree(f) + 1 - s))
      f_rev_trun_rev = reverse(f_rev_trun, max(0, degree(f) + 1 - s))
      @test fsr == f_rev_trun_rev
   end
#  the same over Z/6Z
   T = ResidueRing(JuliaZZ, 6)
   R, x = T["x"]
   for iter = 1:maxIter
      f = rand(R, 0:degmax, 0:5)
      s = rand(0:degmax)
      fsr = shift_right(f, s)
      fsl = shift_left(f, s)
      @test fsl == x^s*f
      f_rev = reverse(f, degree(f) + 1)
      f_rev_trun = truncate(f_rev, max(0, degree(f) + 1 - s))
      f_rev_trun_rev = reverse(f_rev_trun, max(0, degree(f) + 1 - s))
      @test fsr == f_rev_trun_rev
   end
#  Remi Imbach 31/07/17: end
   println("PASS")
end

function test_gen_poly_powering()
   print("Generic.Poly.powering...")

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

   R, x = PolynomialRing(JuliaZZ, "x")

   for iter = 1:100
      f = rand(R, 0:10, -100:100)
      g = R()
      while g == 0
         g = rand(R, 0:10, -100:100)
      end

      @test divexact(f*g, g) == f
   end

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

   R, x = PolynomialRing(JuliaZZ, "x")
   S, y = PolynomialRing(R, "y")

   for iter = 1:100
      f = rand(S, 0:10, 0:10, -100:100)
      g = R()
      while g == 0
         g = rand(R, 0:10, -100:100)
      end

      @test divexact(f*g, g) == f
   end

   n = 23
   Zn = ResidueRing(JuliaZZ, n)
   R, x = PolynomialRing(Zn, "x")
   S, y = PolynomialRing(R, "y")

   for iter = 1:100
      f = rand(S, 0:10, 0:10, -100:100)
      g = R()
      while g == 0
         g = rand(R, 0:10, -100:100)
      end

      @test divexact(f*g, g) == f
   end

   println("PASS")
end

function test_gen_poly_euclidean_division()
   print("Generic.Poly.euclidean_division...")

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
 
   println("PASS")
end

function test_gen_poly_pseudodivision()
   print("Generic.Poly.pseudodivision...")

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

   println("PASS")
end

function test_gen_poly_content_primpart_gcd()
   print("Generic.Poly.content_primpart_gcd...")

   R, x = PolynomialRing(JuliaZZ, "x")
   S, y = PolynomialRing(R, "y")

   for iter = 1:100
      f = rand(S, 0:10, 0:10, -10:10)

      g = R()
      while g == 0
         g = rand(R, 0:10, -10:10)
      end

      @test content(f*g) == divexact(g, canonical_unit(lead(g)))*content(f)

      @test primpart(f*g) == canonical_unit(lead(g))*primpart(f)
   end

   for iter = 1:20
      f = rand(R, 0:10, -10:10)
      g = rand(R, 0:10, -10:10)
      h = R()
      while h == 0
         h = rand(R, 0:10, -10:10)
      end

      @test gcd(f*h, g*h) == divexact(h, canonical_unit(lead(h)))*gcd(f, g)
   end

   R = ResidueRing(JuliaZZ, 23)
   S, x = PolynomialRing(R, "x")

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

   println("PASS")
end

function test_gen_poly_evaluation()
   print("Generic.Poly.evaluation...")

   R, x = PolynomialRing(JuliaZZ, "x")
   S, y = PolynomialRing(R, "y")

   g = x*y^2 + (x + 1)*y + 3

   for iter in 1:10
      f = rand(R, 0:4, -100:100)
      
      @test evaluate(g, f) == x*f^2 + (x + 1)*f + 3
      
      h = rand(S, 0:2, 0:2, -100:100)
      
      @test evaluate(h, f)^2 == evaluate(h^2, f)
   end

   println("PASS")
end

function test_gen_poly_composition()
   print("Generic.Poly.composition...")

   R, x = PolynomialRing(JuliaZZ, "x")
   S, y = PolynomialRing(R, "y")

   f = x*y^2 + (x + 1)*y + 3

   for d in 1:5
      g = rand(S, 0:d, 0:2, -10:10)

      @test compose(f, g) == x*g^2 + (x + 1)*g + 3

      h = rand(S, 0:d, 0:2, -10:10)

      @test compose(h, g)^2 == compose(h^2, g)

      k = rand(S, 0:d, 0:2, -10:10)

      @test compose(g, compose(h, k)) == compose(compose(g, h), k)
   end

   println("PASS")
end

function test_gen_poly_derivative()
   print("Generic.Poly.derivative...")

   R, x = PolynomialRing(JuliaZZ, "x")

   for iter in 1:10
      f = rand(R, 0:4, -100:100)
      g = rand(R, 0:4, -100:100)

      @test derivative(f + g) == derivative(g) + derivative(f)

      @test derivative(g*f) == derivative(g)*f + derivative(f)*g
   end

   println("PASS")
end

function test_gen_poly_integral()
   print("Generic.Poly.integral...")

   R, x = PolynomialRing(JuliaQQ, "x")

   for iter in 1:10
      f = rand(R, 0:2, -100:100)

      @test derivative(integral(f)) == f

      g = rand(R, 0:2, -100:100)

      @test integral(f + g) == integral(g) + integral(f)
      @test integral(f)*integral(g) == integral(integral(f)*g + integral(g)*f)
   end

   println("PASS")
end

function test_gen_poly_resultant()
   print("Generic.Poly.resultant...")

   R, x = PolynomialRing(JuliaZZ, "x")

   for iter in 1:10
      f = rand(R, 0:2, -100:100)
      g = rand(R, 0:2, -100:100)
      h = rand(R, 0:2, -100:100)

      @test resultant(f*g, h) == resultant(f, h) * resultant(g, h)
      @test resultant(f, g*h) == resultant(f, g) * resultant(f, h)
   end

   println("PASS")
end

function test_gen_poly_discriminant()
   print("Generic.Poly.discriminant...")

   R, x = PolynomialRing(JuliaZZ, "x")
   S, y = PolynomialRing(R, "y")

   f = x*y^2 + (x + 1)*y + 3

   @test discriminant(f) == x^2 - 10*x + 1

   println("PASS")
end

function test_gen_poly_resx()
   print("Generic.Poly.resx...")

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

      h = R()
      h = rand(R, 0:5, -10:10)
      r, u, v = resx(f*h, g*h)

      @test (u*f + v*g)*h == r
   end

   println("PASS")
end

function test_gen_poly_newton_representation()
   print("Generic.Poly.newton_representation...")

   R, x = PolynomialRing(JuliaZZ, "x")

   for iter in 1:10
      f = rand(R, 2:2, 1:100)

      g = deepcopy(f)
      roots = BigInt[1, 2, 3]
      monomial_to_newton!(g.coeffs, roots)
      newton_to_monomial!(g.coeffs, roots)

      @test f == g
   end

   println("PASS")
end

function test_gen_poly_interpolation()
   print("Generic.Poly.interpolation...")

   R, x = PolynomialRing(JuliaZZ, "x")

   xs = BigInt[1, 2, 3, 4]
   ys = BigInt[1, 4, 9, 16]

   f = interpolate(R, xs, ys)

   @test f == x^2

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

   println("PASS")
end

function test_gen_poly_special()
   print("Generic.Poly.special...")

   R, x = PolynomialRing(JuliaZZ, "x")
   S, y = PolynomialRing(R, "y")

   @test chebyshev_t(20, y) == 524288*y^20-2621440*y^18+5570560*y^16-6553600*y^14+4659200*y^12-2050048*y^10+549120*y^8-84480*y^6+6600*y^4-200*y^2+1

   @test chebyshev_u(15, y) == 32768*y^15-114688*y^13+159744*y^11-112640*y^9+42240*y^7-8064*y^5+672*y^3-16*y

   for n in 10:20
      T = chebyshev_t(n, y)
      dT = derivative(T)
      ddT = derivative(dT)

      @test (1 - y^2)*ddT + n^2*T == y*dT

      U = chebyshev_u(n - 1, y)
      dU = derivative(U)
      ddU = derivative(dU)

      @test (1 - y^2)*ddU + (n-1)*(n+1)*U == 3*y*dU

      @test T^2 == 1 + (y^2 - 1)*U^2
   end

   println("PASS")
end

function test_gen_poly_mul_karatsuba()
   print("Generic.Poly.mul_karatsuba...")

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
   test_gen_poly_newton_representation()
   test_gen_poly_interpolation()
   test_gen_poly_special()
   test_gen_poly_mul_karatsuba()
   test_gen_poly_mul_ks()
   test_gen_poly_generic_eval()
   test_gen_poly_remove_valuation()
   

   println("")
end
