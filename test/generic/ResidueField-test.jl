function test_gen_res_field_constructors()
   print("Generic.ResF.constructors...")

   B = JuliaZZ

   R = Generic.ResidueField(B, 16453889)

   @test elem_type(R) == Generic.ResF{elem_type(B)}
   @test elem_type(Generic.ResField{elem_type(B)}) == Generic.ResF{elem_type(B)}
   @test parent_type(Generic.ResF{elem_type(B)}) == Generic.ResField{elem_type(B)}

   @test isa(R, Generic.ResField)

   a = R(123)

   @test isa(a, Generic.ResF)

   b = R(a)

   @test isa(b, Generic.ResF)

   c = R(JuliaZZ(12))

   @test isa(c, Generic.ResF)

   d = R()

   @test isa(d, Generic.ResF)

   S, x = PolynomialRing(R, "x")
   T = ResidueField(S, x^3 + 3x + 1)

   @test isa(T, Generic.ResField)

   f = T(x^4)

   @test isa(f, Generic.ResF)

   g = T(f)

   @test isa(g, Generic.ResF)

   println("PASS")
end

function test_gen_res_field_manipulation()
   print("Generic.ResF.manipulation...")

   R = Generic.ResidueField(JuliaZZ, 16453889)

   @test modulus(R) == 16453889

   g = zero(R)

   @test iszero(g)

   @test modulus(g) == 16453889

   S, x = PolynomialRing(R, "x")
   T = ResidueField(S, x^3 + 3x + 1)

   h = one(T)

   @test isunit(h)

   @test isone(h)

   @test data(h) == 1

   @test canonical_unit(R(11)) == R(11)

   @test canonical_unit(T(x + 1)) == T(x + 1)

   @test deepcopy(h) == h

   println("PASS")
end

function test_gen_res_field_unary_ops()
   print("Generic.ResF.unary_ops...")

   R = Generic.ResidueField(JuliaZZ, 16453889)

   @test -R(12345) == R(16441544)

   S, x = PolynomialRing(R, "x")
   T = ResidueField(S, x^3 + 3x + 1)

   @test -T(x^5 + 1) == T(x^2+16453880*x+16453885)

   println("PASS")
end

function test_gen_res_field_binary_ops()
   print("Generic.ResF.binary_ops...")

   R = Generic.ResidueField(JuliaZZ, 13)

   f = R(4)
   g = R(6)

   @test f + g == R(10)

   @test f - g == R(11)

   @test f*g == R(11)

   Q = Generic.ResidueField(JuliaZZ, 7)
   S, x = PolynomialRing(Q, "x")
   T = ResidueField(S, x^3 + 3x + 1)

   n = T(x^5 + 1)
   p = T(x^2 + 2x + 1)

   @test n + p == T(4x + 5)

   @test n - p == T(5x^2 + 3)

   @test n*p == T(3x^2 + 4x + 4)

   println("PASS")
end

function test_gen_res_field_gcd()
   print("Generic.ResF.gcd...")

   R = Generic.ResidueField(JuliaZZ, 13)

   f = R(4)
   g = R(6)

   @test gcd(f, g) == R(1)

   Q = Generic.ResidueField(JuliaZZ, 7)
   S, x = PolynomialRing(Q, "x")
   T = ResidueField(S, x^3 + 3x + 1)

   n = T(x^5 + 1)
   p = T(x^2 + 2x + 1)

   @test gcd(n, p) == 1

   println("PASS")
end

function test_gen_res_field_adhoc_binary()
   print("Generic.ResF.adhoc_binary...")

   R = Generic.ResidueField(JuliaZZ, 7)

   a = R(3)

   @test a + 3 == R(6)

   @test 3 - a == R(0)

   @test 5a == R(1)

   S, x = PolynomialRing(R, "x")
   T = ResidueField(S, x^3 + 3x + 1)

   f = T(x^5 + 1)

   @test f + 4 == T(x^5 + 5)

   @test 4 - f == T(x^2+5*x)

   @test f*5 == T(2*x^2+3*x+6)

   println("PASS")
end

function test_gen_res_field_comparison()
   print("Generic.ResF.comparison...")

   R = Generic.ResidueField(JuliaZZ, 7)

   a = R(3)
   b = a
   c = R(2)

   @test b == a

   @test isequal(b, a)

   @test c != a

   S, x = PolynomialRing(R, "x")
   T = ResidueField(S, x^3 + 3x + 1)

   f = T(x^5 + 1)
   g = 8f
   h = f + g

   @test f == g
   @test h != g

   @test isequal(f, g)

   println("PASS")
end

function test_gen_res_field_adhoc_comparison()
   print("Generic.ResF.adhoc_comparison...")

   R = Generic.ResidueField(JuliaZZ, 7)

   a = R(3)

   @test a == 3
   @test 4 != a

   S, x = PolynomialRing(R, "x")
   T = ResidueField(S, x^3 + 3x + 1)

   f = T(x^5 + 1)

   @test f != 5

   println("PASS")
end

function test_gen_res_field_powering()
   print("Generic.ResF.powering...")

   R = Generic.ResidueField(JuliaZZ, 7)

   a = R(3)

   @test a^5 == 5

   S, x = PolynomialRing(R, "x")
   T = ResidueField(S, x^3 + 3x + 1)

   f = T(x^5 + 1)

   @test f^100 == T(x^2 + 2x + 1)

   println("PASS")
end

function test_gen_res_field_inversion()
   print("Generic.ResF.inversion...")

   R = Generic.ResidueField(JuliaZZ, 47)

   a = R(5)

   @test inv(a) == 19

   R = Generic.ResidueField(JuliaZZ, 41)
   S, x = PolynomialRing(R, "x")
   T = ResidueField(S, x^3 + 3x + 1)

   f = T(x^5 + 1)

   @test inv(f) == T(26*x^2+31*x+10)

   println("PASS")
end

function test_gen_res_field_exact_division()
   print("Generic.ResF.exact_division...")

   R = Generic.ResidueField(ZZ, 47)

   a = R(5)
   b = R(3)

   @test divexact(a, b) == 33

   R = Generic.ResidueField(JuliaZZ, 41)
   S, x = PolynomialRing(R, "x")
   T = ResidueField(S, x^3 + 3x + 1)

   f = T(x^5 + 1)
   g = T(x^4 + x + 2)

   @test divexact(f, g) == T(7*x^2+25*x+26)

   println("PASS")
end

function test_gen_res_field()
   test_gen_res_field_constructors()
   test_gen_res_field_manipulation()
   test_gen_res_field_unary_ops()
   test_gen_res_field_binary_ops()
   test_gen_res_field_gcd()
   test_gen_res_field_adhoc_binary()
   test_gen_res_field_comparison()
   test_gen_res_field_adhoc_comparison()
   test_gen_res_field_powering()
   test_gen_res_field_inversion()
   test_gen_res_field_exact_division()

   println("")
end
