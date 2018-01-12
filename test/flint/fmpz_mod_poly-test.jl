function test_fmpz_mod_poly_constructors()
   print("fmpz_mod_poly.constructors...")
 
   R = ResidueRing(ZZ, 123456789012345678949)
   S, x = PolynomialRing(R, "x")

   @test elem_type(S) == fmpz_mod_poly
   @test elem_type(FmpzModPolyRing) == fmpz_mod_poly
   @test parent_type(fmpz_mod_poly) == FmpzModPolyRing

   @test typeof(S) <: FmpzModPolyRing

   @test isa(x, PolyElem)

   f = x^3 + 2x^2 + x + 1

   @test isa(f, PolyElem)

   g = S(2)

   @test isa(g, PolyElem)

   h = S(x^2 + 2x + 1)

   @test isa(h, PolyElem)

   k = S([R(1), R(0), R(3)])

   @test isa(k, PolyElem)

   l = S()

   @test isa(l, PolyElem)

   m = S(fmpz(123))

   @test isa(m, PolyElem)

   n = S([fmpz(1), fmpz(0), fmpz(3)])

   @test isa(n, PolyElem)

   T, y = PolynomialRing(ZZ, "y")

   p = 3y^3 + 2y - 1
   q = S(p)

   @test isa(q, PolyElem)

   r = S([1, 2, 3])

   @test isa(r, PolyElem)

   println("PASS")
end

function test_fmpz_mod_poly_printing()
   print("fmpz_mod_poly.printing...")
 
   R = ResidueRing(ZZ, 123456789012345678949)
   S, x = PolynomialRing(R, "x")
   f = x^3 + 2x^2 + x + 1

   @test string(f) == "x^3+2*x^2+x+1"

   println("PASS")
end

function test_fmpz_mod_poly_manipulation()
   print("fmpz_mod_poly.manipulation...")

   R = ResidueRing(ZZ, 123456789012345678949)
   S, x = PolynomialRing(R, "x")

   @test iszero(zero(S))
   
   @test isone(one(S))

   @test isgen(gen(S))
   
   @test isunit(one(S))

   f = x^2 + 2x + 1

   @test lead(f) == 1

   @test degree(f) == 2

   @test length(f) == 3

   @test coeff(f, 1) == 2

   @test canonical_unit(-x + 1) == -1

   @test deepcopy(f) == f

   setcoeff!(f, 1, UInt(2))

   @test coeff(f, 1) == 2

   setcoeff!(f, 1, 3)

   @test coeff(f, 1) == 3

   setcoeff!(f, 1, fmpz(2)^100)

   @test coeff(f, 1) == 32146634986640907030

   @test modulus(x) == 123456789012345678949

   @test modulus(R) == 123456789012345678949

   println("PASS")
end

function test_fmpz_mod_poly_binary_ops()
   print("fmpz_mod_poly.binary_ops...")

   R = ResidueRing(ZZ, 123456789012345678949)
   S, x = PolynomialRing(R, "x")

   f = x^2 + 2x + 1
   g = x^3 + 3x + 2

   @test f + g == x^3+x^2+5*x+3

   @test f*g == x^5+2*x^4+4*x^3+8*x^2+7*x+2

   @test f - g == 123456789012345678948*x^3+x^2+123456789012345678948*x+123456789012345678948

   println("PASS")
end

function test_fmpz_mod_poly_adhoc_binary()
   print("fmpz_mod_poly.adhoc_binary...")

   R = ResidueRing(ZZ, 123456789012345678949)
   S, x = PolynomialRing(R, "x")

   f = x^2 + 2x + 1
   g = x^3 + 3x + 2

   @test f*12 == 12*x^2+24*x+12

   @test 7*g == 7*x^3+21*x+14

   @test fmpz(3)*g == 3*x^3+9*x+6

   @test f*fmpz(2) == 2*x^2+4*x+2

   @test f + 12 == x^2+2*x+13

   @test f + fmpz(12) == x^2+2*x+13

   @test f - 12 == x^2+2*x+123456789012345678938

   @test f - fmpz(12) == x^2+2*x+123456789012345678938

   @test 12 + g == x^3+3*x+14

   @test fmpz(12) + g == x^3+3*x+14

   @test 12 - g == 123456789012345678948*x^3+123456789012345678946*x+10

   @test fmpz(12) - g == 123456789012345678948*x^3+123456789012345678946*x+10

   @test f + R(12) == x^2+2*x+13

   @test R(12) + g == x^3+3*x+14

   @test f - R(12) == x^2+2*x+123456789012345678938

   @test R(12) - g == 123456789012345678948*x^3+123456789012345678946*x+10

   @test R(7)*g == 7*x^3+21*x+14

   @test f*R(12) == 12*x^2+24*x+12

   println("PASS")
end

function test_fmpz_mod_poly_comparison()
   print("fmpz_mod_poly.comparison...")

   R = ResidueRing(ZZ, 123456789012345678949)
   S, x = PolynomialRing(R, "x")

   f = x^2 + 2x + 1
   g = x^3 + 3x + 2

   @test f != g

   @test isequal(f, deepcopy(f))

   println("PASS")
end

function test_fmpz_mod_poly_adhoc_comparison()
   print("fmpz_mod_poly.adhoc_comparison...")

   R = ResidueRing(ZZ, 123456789012345678949)
   S, x = PolynomialRing(R, "x")

   f = x^2 + 2x + 1

   @test f != 1 

   @test 1 != f 

   @test S(7) == fmpz(7)

   @test fmpz(7) != f

   @test S(7) == R(7)

   @test R(7) != x + 1

   println("PASS")
end

function test_fmpz_mod_poly_unary_ops()
   print("fmpz_mod_poly.unary_ops...")

   R = ResidueRing(ZZ, 123456789012345678949)
   S, x = PolynomialRing(R, "x")

   f = x^2 + 2x + 1

   @test -f == 123456789012345678948*x^2+123456789012345678947*x+123456789012345678948

   println("PASS")
end

function test_fmpz_mod_poly_truncation()
   print("fmpz_mod_poly.truncation...")

   R = ResidueRing(ZZ, 123456789012345678949)
   S, x = PolynomialRing(R, "x")

   f = x^2 + 2x + 1
   g = x^3 + 3x + 1

   @test truncate(f, 2) == 2*x+1

   @test mullow(f, g, 3) == 7*x^2+5*x+1

   println("PASS")
end

function test_fmpz_mod_poly_reverse()
   print("fmpz_mod_poly.reverse...")

   R = ResidueRing(ZZ, 123456789012345678949)
   S, x = PolynomialRing(R, "x")

   f = x^2 + 2x + 3

   @test reverse(f) == 3x^2 + 2x + 1

   println("PASS")
end

function test_fmpz_mod_poly_shift()
   print("fmpz_mod_poly.shift...")

   R = ResidueRing(ZZ, 123456789012345678949)
   S, x = PolynomialRing(R, "x")

   f = x^2 + 2x + 1

   @test shift_left(f, 3) == x^5 + 2x^4 + x^3

   @test shift_right(f, 1) == x + 2

   println("PASS")
end

function test_fmpz_mod_poly_powering()
   print("fmpz_mod_poly.powering...")

   R = ResidueRing(ZZ, 123456789012345678949)
   S, x = PolynomialRing(R, "x")

   f = x^2 + 2x + 1

   @test f^6 == x^12+12*x^11+66*x^10+220*x^9+495*x^8+792*x^7+924*x^6+792*x^5+495*x^4+220*x^3+66*x^2+12*x+1

   println("PASS")
end

function test_fmpz_mod_poly_exact_division()
   print("fmpz_mod_poly.exact_division...")

   R = ResidueRing(ZZ, 123456789012345678949)
   S, x = PolynomialRing(R, "x")

   f = x^2 + 2x + 1
   g = x^3 + 3x + 1

   @test divexact(f*g, f) == g

   println("PASS")
end

function test_fmpz_mod_poly_adhoc_exact_division()
   print("fmpz_mod_poly.adhoc_exact_division...")

   R = ResidueRing(ZZ, 123456789012345678949)
   S, x = PolynomialRing(R, "x")

   f = x^2 + 2x + 1
   
   @test divexact(3*f, fmpz(3)) == f

   @test divexact(3*f, 3) == f

   @test divexact(R(7)*f, R(7)) == f
   
   println("PASS")
end

function test_fmpz_mod_poly_modular_arithmetic()
   print("fmpz_mod_poly.modular_arithmetic...")

   R = ResidueRing(ZZ, 123456789012345678949)
   S, x = PolynomialRing(R, "x")

   f = 3*x^2 + x + 2
   g = 5*x^2 + 2*x + 1
   h = 3*x^3 + 2*x^2 + x + 7

   @test invmod(f, h) == 112883663504991137175*x^2+86761824016232857498*x+48511987621979662257

   @test mulmod(f, g, h) == 82304526008230452642*x^2+41152263004115226286*x+41152263004115226316

   @test powmod(f, 10, h) == 118470346535924950143*x^2+97790722831392543222*x+115967716915690326718

   @test powmod(f, fmpz(10), h) == 118470346535924950143*x^2+97790722831392543222*x+115967716915690326718

   @test powmod(f, -10, g) == 78305338116088931412*x+91239060941924718463

   @test powmod(f, -fmpz(10), g) == 78305338116088931412*x+91239060941924718463

   println("PASS")
end

function test_fmpz_mod_poly_euclidean_division()
   print("fmpz_mod_poly.euclidean_division...")

   R = ResidueRing(ZZ, 123456789012345678949)
   S, x = PolynomialRing(R, "x")

   f = x^2 + 2x + 1
   g = x^3 + 3x + 1

   @test mod(g, f) == 6*x+3

   @test divrem(g, f) == (x+123456789012345678947, 6*x+3)
 
   println("PASS")
end

function test_fmpz_mod_poly_gcd()
   print("fmpz_mod_poly.gcd...")

   R = ResidueRing(ZZ, 123456789012345678949)
   S, x = PolynomialRing(R, "x")

   f = x^2 + 2x + 1
   g = x^3 + 3x + 1
   h = x^2 + 1

   @test gcd(f*h, g*h) == x^2+1
 
   @test gcdx(f*h, g*h) == (x^2+1, 41152263004115226317*x^2+41152263004115226316*x+2,82304526008230452632*x+123456789012345678948)
   println("PASS")
end

function test_fmpz_mod_poly_gcdinv()
   print("fmpz_mod_poly.gcdinv...")

   R = ResidueRing(ZZ, 123456789012345678949)
   S, x = PolynomialRing(R, "x")

   f = x^2 + 2x + 1
   g = x^3 + 3x + 1

   @test gcdinv(f, g) == (1, 41152263004115226317*x^2+41152263004115226316*x+2)
 
   println("PASS")
end

function test_fmpz_mod_poly_evaluation()
   print("fmpz_mod_poly.evaluation...")

   R = ResidueRing(ZZ, 123456789012345678949)
   S, x = PolynomialRing(R, "x")

   f = x^2 + 2x + 1

   @test evaluate(f, 3) == 16
   
   @test evaluate(f, fmpz(10)) == 121

   @test evaluate(f, R(10)) == 121

if VERSION >= v"0.5.0-dev+3171"

   @test f(3) == 16

   @test f(fmpz(10)) == 121

   @test f(R(10)) == 121

end

   println("PASS")
end

function test_fmpz_mod_poly_composition()
   print("fmpz_mod_poly.composition...")

   R = ResidueRing(ZZ, 123456789012345678949)
   S, x = PolynomialRing(R, "x")

   f = x^2 + 2x + 1
   g = x^3 + 3x + 1

   @test compose(f, g) == x^6+6*x^4+4*x^3+9*x^2+12*x+4

   println("PASS")
end

function test_fmpz_mod_poly_derivative()
   print("fmpz_mod_poly.derivative...")

   R = ResidueRing(ZZ, 123456789012345678949)
   S, x = PolynomialRing(R, "x")

   f = x^2 + 2x + 1

   @test derivative(f) == 2x + 2

   println("PASS")
end

function test_fmpz_mod_poly_integral()
   print("fmpz_mod_poly.integral...")

   R = ResidueRing(ZZ, 123456789012345678949)
   S, x = PolynomialRing(R, "x")

   f = x^2 + 2x + 1
   
   @test integral(f) == 82304526008230452633*x^3+x^2+x

   println("PASS")
end

function test_fmpz_mod_poly_resultant()
   print("fmpz_mod_poly.resultant...")

   R = ResidueRing(ZZ, 123456789012345678949)
   S, x = PolynomialRing(R, "x")

   f = 5x^2 + 2x + 1
   g = 2x^3 + 3x + 1

   @test resultant(f, g) == 212

   println("PASS")
end

function test_fmpz_mod_poly_discriminant()
   print("fmpz_mod_poly.discriminant...")

   R = ResidueRing(ZZ, 123456789012345678949)
   S, x = PolynomialRing(R, "x")

   f = x^2 + 2x + 1

   @test discriminant(f) == 0

   println("PASS")
end

function test_fmpz_mod_poly_lift()
   print("fmpz_mod_poly.lift...")

   R = ResidueRing(ZZ, 123456789012345678949)
   S, x = PolynomialRing(R, "x")

   T, y = PolynomialRing(ZZ, "y")

   f = x^2 + 2x + 1

   @test lift(T, f) == y^2 + 2y + 1

   println("PASS")
end

function test_fmpz_mod_poly_isirreducible()
   print("fmpz_mod_poly.isirreducible...")

   R = ResidueRing(ZZ, 123456789012345678949)
   S, x = PolynomialRing(R, "x")

   f = x^2 + 2x + 1

   @test isirreducible(f) == false

   println("PASS")
end

function test_fmpz_mod_poly_issquarefree()
   print("fmpz_mod_poly.issquarefree...")

   R = ResidueRing(ZZ, 123456789012345678949)
   S, x = PolynomialRing(R, "x")

   f = x^2 + 2x + 1

   @test issquarefree(f) == false

   println("PASS")
end

function test_fmpz_mod_poly_factor()
   print("fmpz_mod_poly.factor...")

   R = ResidueRing(ZZ, 123456789012345678949)
   S, x = PolynomialRing(R, "x")

   f = 3*(x^2 + 2x + 1)
   g = x^3 + 3x + 1

   R = factor(f*g)

   @test f*g == unit(R) * prod([ p^e for (p, e) in R])

   R = factor_squarefree(f*g)

   @test f*g == unit(R) * prod([ p^e for (p, e) in R])

   R = factor_distinct_deg((x + 1)*g*(x^5+x+1))

   @test length(R) == 2
   @test R == Dict(1 => x^3+2*x^2+2*x+1,
                3 => x^6+123456789012345678948*x^5+3*x^4+123456789012345678948*x^3+123456789012345678948*x^2+3*x+1)

   R = factor_shape(f*g)
   
   @test length(R) == 2
   @test R == Dict(3=>1, 1=>2)

   println("PASS")
end

function test_fmpz_mod_poly_remove_valuation()
   print("fmpz_mod_poly.remove_valuation...")

   R = ResidueRing(ZZ, 123456789012345678949)
   S, y = PolynomialRing(R, "y")

   f = 7y^2 + 3y + 2
   g = f^5*(11y^3 - 2y^2 + 5)

   v, h = remove(g, f)

   @test valuation(g, f) == 5
   @test v == 5
   @test h == (11y^3 - 2y^2 + 5)

   v, q = divides(f*g, f)

   @test v
   @test q == g

   v, q = divides(f*g + 1, f)

   @test !v

   println("PASS")
end



function test_fmpz_mod_poly()
   test_fmpz_mod_poly_constructors()
   test_fmpz_mod_poly_printing()
   test_fmpz_mod_poly_manipulation()
   test_fmpz_mod_poly_binary_ops()
   test_fmpz_mod_poly_adhoc_binary()
   test_fmpz_mod_poly_comparison()
   test_fmpz_mod_poly_adhoc_comparison()
   test_fmpz_mod_poly_unary_ops()
   test_fmpz_mod_poly_truncation()
   test_fmpz_mod_poly_reverse()
   test_fmpz_mod_poly_shift()
   test_fmpz_mod_poly_powering()
   test_fmpz_mod_poly_exact_division()
   test_fmpz_mod_poly_adhoc_exact_division()
   test_fmpz_mod_poly_modular_arithmetic()
   test_fmpz_mod_poly_euclidean_division()
   test_fmpz_mod_poly_gcdinv()
   test_fmpz_mod_poly_evaluation()
   test_fmpz_mod_poly_composition()
   test_fmpz_mod_poly_derivative()
   test_fmpz_mod_poly_integral()
   test_fmpz_mod_poly_resultant()
   test_fmpz_mod_poly_discriminant()
   test_fmpz_mod_poly_lift()
   test_fmpz_mod_poly_isirreducible()
   test_fmpz_mod_poly_issquarefree()
   test_fmpz_mod_poly_factor()
   test_fmpz_mod_poly_remove_valuation()

   println("")
end

