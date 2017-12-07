function test_gen_frac_constructors()
   print("Generic.Frac.constructors...")

   S, x = PolynomialRing(ZZ, "x")
   T = FractionField(S)

   @test elem_type(T) == Generic.Frac{elem_type(S)}
   @test elem_type(Generic.FracField{elem_type(S)}) == Generic.Frac{elem_type(S)}
   @test parent_type(Generic.Frac{elem_type(S)}) == Generic.FracField{elem_type(S)}

   @test isa(T, Generic.FracField)

   @test isa(T(3), Generic.Frac)

   @test isa(T(fmpz(7)), Generic.Frac)

   @test isa(T(x + 2), Generic.Frac)

   @test isa(T(3, 7), Generic.Frac)

   @test isa(T(x + 2, x + 1), Generic.Frac)

   @test isa(T(x + 2, 4), Generic.Frac)

   @test isa(T(3, x + 1), Generic.Frac)

   @test isa(T(T(x + 2)), Generic.Frac)

   @test isa(T(), Generic.Frac)

   @test isa((x + 3)//(x^2 + 2), Generic.Frac)

   @test isa((x + 3)//12, Generic.Frac)

   @test isa(12//(x + 2), Generic.Frac)

   @test isa((x + 1)//T(x + 2, x + 1), Generic.Frac)

   @test isa(T(x + 2, x + 1)//(x + 1), Generic.Frac)

   @test isa(T(x + 2, x + 1)//T(x, x + 2), Generic.Frac) 

   println("PASS")
end

function test_gen_frac_manipulation()
   print("Generic.Frac.manipulation...")

   R = FractionField(ZZ)
   S, x = PolynomialRing(ZZ, "x")

   @test denominator((x + 1)//(-x^2 + 1)) == x - 1

   @test numerator((x + 1)//(-x^2 + 1)) == -1

   @test iszero(zero(R))

   @test isone(one(S))

   @test canonical_unit((x + 1)//(-x^2 + 1)) == -1//(x-1)

   @test isunit((x + 1)//(-x^2 + 1))

   @test deepcopy((x + 1)//(-x^2 + 1)) == (x + 1)//(-x^2 + 1)
   println("PASS")
end

function test_gen_frac_unary_ops()
   print("Generic.Frac.unary_ops...")

   S, x = PolynomialRing(ZZ, "x")

   @test -((x + 1)//(-x^2 + 1)) == 1//(x - 1)

   println("PASS")
end

function test_gen_frac_binary_ops()
   print("Generic.Frac.binary_ops...")

   S, x = PolynomialRing(ZZ, "x")

   a = -(x + 3)//(x + 1) + (2x + 3)//(x^2 + 4)
   b = (x + 1)//(-x^2 + 1) - x//(2x + 1)
   c = ((x^2 + 3x)//(5x))*((x + 1)//(2x^2 + 2))

   @test a == (-x^3-x^2+x-9)//(x^3+x^2+4*x+4)

   @test b == (-x^2-x-1)//(2*x^2-x-1)

   @test c == (x^2+4*x+3)//(10*x^2+10)

   println("PASS")
end

function test_gen_frac_adhoc_binary()
   print("Generic.Frac.adhoc_binary...")

   S, x = PolynomialRing(ZZ, "x")

   a = (-x + 1)//(2x^2 + 3)
   b = (x + 1)//(-x^2 + 1)

   @test a + 2 == (4*x^2-x+7)//(2*x^2+3)

   @test a - 2 == (-4*x^2-x-5)//(2*x^2+3)

   @test 3 + a == (6*x^2-x+10)//(2*x^2+3)

   @test 3 - a == (6*x^2+x+8)//(2*x^2+3)

   @test b*(x + 1) == (-x-1)//(x-1)

   @test (x + 1)*b == (-x-1)//(x-1)

   println("PASS")
end

function test_gen_frac_comparison()
   print("Generic.Frac.comparison...")

   S, x = PolynomialRing(ZZ, "x")

   a = -((x + 1)//(-x^2 + 1))

   @test a == 1//(x - 1)

   @test isequal(a, 1//(x - 1))

   println("PASS")
end

function test_gen_frac_adhoc_comparison()
   print("Generic.Frac.adhoc_comparison...")

   S, x = PolynomialRing(ZZ, "x")

   a = 1//(x - 1)

   @test 1//a == x - 1

   @test x - 1 == 1//a

   @test one(S) == 1

   @test 1 == one(S)

   println("PASS")
end

function test_gen_frac_powering()
   print("Generic.Frac.powering()...")

   S, x = PolynomialRing(ZZ, "x")

   a = (x + 1)//(-x^2 + 1)

   @test a^-12 == x^12-12*x^11+66*x^10-220*x^9+495*x^8-792*x^7+924*x^6-792*x^5+495*x^4-220*x^3+66*x^2-12*x+1

   println("PASS")
end

function test_gen_frac_inversion()
   print("Generic.Frac.inversion()...")

   S, x = PolynomialRing(ZZ, "x")

   a = (x + 1)//(-x^2 + 1)

   @test inv(a) == -x + 1

   println("PASS")
end

function test_gen_frac_exact_division()
   print("Generic.Frac.exact_division...")

   S, x = PolynomialRing(ZZ, "x")

   a = -(x + 3)//(x + 1) + (2x + 3)//(x^2 + 4)
   b = ((x^2 + 3x)//(5x))*((x + 1)//(2x^2 + 2))

   @test a//b == (-10*x^5-10*x^4-100*x^2+10*x-90)//(x^5+5*x^4+11*x^3+23*x^2+28*x+12)

   println("PASS")
end

function test_gen_frac_adhoc_exact_division()
   print("Generic.Frac.adhoc_exact_division...")

   S, x = PolynomialRing(ZZ, "x")

   a = (-x + 1)//(2x^2 + 3)
   b = (x + 1)//(-x^2 + 1)

   @test a//5 == (-x+1)//(10*x^2+15)

   @test a//(x + 1) == (-x+1)//(2*x^3+2*x^2+3*x+3)

   @test (x + 1)//b == -x^2+1

   @test 5//a == (-10*x^2-15)//(x-1)

   println("PASS")
end

function test_gen_frac_gcd()
   print("Generic.Frac.gcd...")

   S, x = PolynomialRing(ZZ, "x")

   a = (x + 1)//(-x^2 + 1) - x//(2x + 1)
   
   @test gcd(a, (x + 1)//(x - 1)) == 1//(2*x^2-x-1)

   println("PASS")
end

function test_gen_frac_remove_valuation()
   print("Generic.Frac.remove_valuation...")

   a = fmpq(2, 3)

   @test remove(a, fmpz(2)) == (1, fmpq(1, 3))
   @test valuation(a, fmpz(2)) == 1

   @test remove(a, fmpz(3)) == (-1, fmpq(2, 1))
   @test valuation(a, fmpz(3)) == 1

   println("PASS")
end

function test_gen_frac()
   test_gen_frac_constructors()
   test_gen_frac_manipulation()
   test_gen_frac_unary_ops()
   test_gen_frac_binary_ops()
   test_gen_frac_adhoc_binary()
   test_gen_frac_comparison()
   test_gen_frac_adhoc_comparison()
   test_gen_frac_powering()
   test_gen_frac_inversion()
   test_gen_frac_exact_division()
   test_gen_frac_adhoc_exact_division()
   test_gen_frac_gcd()

   println("")
end
