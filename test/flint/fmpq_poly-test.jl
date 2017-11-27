function test_fmpq_poly_constructors()
   print("fmpq_poly.constructors...")
 
   S, y = PolynomialRing(QQ, "y")
 
   @test elem_type(S) == fmpq_poly
   @test elem_type(FmpqPolyRing) == fmpq_poly
   @test parent_type(fmpq_poly) == FmpqPolyRing

   @test isa(S, FmpqPolyRing)

   @test isa(y, PolyElem)

   T, z = PolynomialRing(S, "z")

   @test typeof(T) <: Generic.PolyRing

   @test isa(z, PolyElem)

   f = fmpz(12)//3 + y^3 + z + 1

   @test isa(f, PolyElem)

   g = S(2)

   @test isa(g, PolyElem)

   h = S(fmpz(12)//7 + 1)

   @test isa(h, PolyElem)

   j = T(fmpz(12)//7 + 2)

   @test isa(j, PolyElem)

   k = S([fmpz(12)//7, fmpz(12)//7 + 2, fmpz(3)//11 + 1])

   @test isa(k, PolyElem)

   l = S(k)

   @test isa(l, PolyElem)

   R, x = PolynomialRing(ZZ, "x")

   m = S(3x^3 + 2x + 1)

   @test isa(m, PolyElem)

   @test m == 3y^3 + 2y + 1

   n = S(fmpz(12))

   @test isa(n, PolyElem)

   n2 = S(12//1)

   @test isa(n2, PolyElem)

   n2 = S(BigInt(12)//BigInt(1))

   @test isa(n2, PolyElem)

   o = S([1, 2, 3])

   @test isa(o, PolyElem)

   o2 = S([1//1, 2//1, 3//1])

   @test isa(o2, PolyElem)

   o3 = S([BigInt(1)//BigInt(1), BigInt(2)//BigInt(1), BigInt(3)//BigInt(1)])

   @test isa(o3, PolyElem)

   p = S([ZZ(1), ZZ(2), ZZ(3)])

   @test isa(p, PolyElem)

   println("PASS")
end

function test_fmpq_poly_printing()
   print("fmpq_poly.printing...")
 
   S, y = PolynomialRing(QQ, "y")

   @test string(y + y^2) == "y^2+y"

   println("PASS")
end

function test_fmpq_poly_manipulation()
   print("fmpq_poly.manipulation...")

   S, y = PolynomialRing(QQ, "y")

   @test iszero(zero(S))
   
   @test isone(one(S))

   @test isgen(gen(S))
   
   @test isunit(one(S))

   f = 2y + fmpz(11)//7 + 1

   @test lead(f) == 2

   @test degree(f) == 1

   h = fmpz(12)//7*y^2 + 5*y + 3

   @test coeff(h, 2) == fmpz(12)//7

   @test length(h) == 3

   @test canonical_unit(-fmpz(12)//7*y + 1) == fmpz(-12)//7

   @test deepcopy(h) == h

   @test den(-fmpz(12)//7*y + 1) == 7

   println("PASS")
end

function test_fmpq_poly_binary_ops()
   print("fmpq_poly.binary_ops...")

   S, y = PolynomialRing(QQ, "y")

   f = 3*y^2 + 7*y + 3
   g = 2*y + 11
   
   @test f - g == 3*y^2 + 5*y - 8

   @test f + g == 3*y^2 + 9*y + 14

   @test f*g == 6*y^3 + 47*y^2 + 83*y + 33

   println("PASS")
end

function test_fmpq_poly_adhoc_binary()
   print("fmpq_poly.adhoc_binary...")

   S, y = PolynomialRing(QQ, "y")

   f = 3*y^2 + 7*y + 3
   g = 2*y + 11
   
   @test f*4 == 12*y^2 + 28*y + 12

   @test 7*f == 21*y^2 + 49*y + 21
   
   @test fmpz(5)*g == 10*y+55

   @test g*fmpz(3) == 6*y+33

   @test fmpq(5, 7)*g == fmpz(10)//7*y+fmpz(55)//7

   @test g*fmpq(5, 7) == fmpz(10)//7*y+fmpz(55)//7

   @test (5//7)*g == fmpz(10)//7*y+fmpz(55)//7

   @test g*(5//7) == fmpz(10)//7*y+fmpz(55)//7

   @test (BigInt(5)//BigInt(7))*g == fmpz(10)//7*y+fmpz(55)//7

   @test g*(BigInt(5)//BigInt(7)) == fmpz(10)//7*y+fmpz(55)//7
 
   @test f + 4 == 3*y^2 + 7*y + 7

   @test 7 + f == 3*y^2 + 7*y + 10
   
   @test fmpz(5) + g == 2*y+16

   @test g + fmpz(3) == 2*y+14

   @test fmpq(5, 7) + g == 2*y+fmpz(82)//7

   @test g + (5//7) == 2*y+fmpz(82)//7

   @test (5//7) + g == 2*y+fmpz(82)//7

   @test g + (BigInt(5)//BigInt(7)) == 2*y+fmpz(82)//7

   @test (BigInt(5)//BigInt(7)) + g == 2*y+fmpz(82)//7

   @test g + (BigInt(5)//BigInt(7)) == 2*y+fmpz(82)//7

   @test f - 4 == 3*y^2 + 7*y - 1

   @test 7 - f == -3*y^2 - 7*y + 4
   
   @test fmpz(5) - g == -2*y-6

   @test g - fmpz(3) == 2*y+8

   @test fmpq(5, 7) - g == -2*y-fmpz(72)//7

   @test g - fmpq(5, 7) == 2*y+fmpz(72)//7

   @test (5//7) - g == -2*y-fmpz(72)//7

   @test g - (5//7) == 2*y+fmpz(72)//7

   @test (BigInt(5)//BigInt(7)) - g == -2*y-fmpz(72)//7

   @test g - (BigInt(5)//BigInt(7)) == 2*y+fmpz(72)//7
 
   println("PASS")
end

function test_fmpq_poly_comparison()
   print("fmpq_poly.comparison...")

   S, y = PolynomialRing(QQ, "y")

   f = 3*y^2 + 7*y + 3
   g = 3*y^2 + 7*y + 3
   
   @test f == g

   @test isequal(f, g)

   println("PASS")
end

function test_fmpq_poly_adhoc_comparison()
   print("fmpq_poly.adhoc_comparison...")

   S, y = PolynomialRing(QQ, "y")

   @test S(1) == 1 

   @test S(1) == BigInt(1)

   @test S(1) == fmpz(1)

   @test S(1) == fmpq(1, 1)

   @test S(1) == 1//1

   @test S(1) == BigInt(1)//BigInt(1)

   @test 1 != fmpz(11)//7 + y

   @test S(fmpz(3)//5) == fmpq(3, 5)

   @test fmpq(3, 5) != y + 1

   @test (3//5) != y + 1

   @test BigInt(3)//BigInt(5) != y + 1

   println("PASS")
end

function test_fmpq_poly_unary_ops()
   print("fmpq_poly.unary_ops...")

   S, y = PolynomialRing(QQ, "y")

   f = 3*y^2 + 2*y + 3

   @test -f == -3*y^2 - 2*y - 3

   println("PASS")
end

function test_fmpq_poly_truncation()
   print("fmpq_poly.truncation...")

   S, y = PolynomialRing(QQ, "y")

   f = 3*y^2 + 7*y + 3
   g = 2*y^2 + 11*y + 1
   
   @test truncate(f, 1) == 3

   @test mullow(f, g, 4) == 47*y^3 + 86*y^2 + 40*y + 3

   println("PASS")
end

function test_fmpq_poly_reverse()
   print("fmpq_poly.reverse...")

   S, y = PolynomialRing(QQ, "y")

   f = 3*y^2 + 7*y + 3
   
   @test reverse(f, 7) == 3*y^6 + 7*y^5 + 3*y^4

   println("PASS")
end

function test_fmpq_poly_shift()
   print("fmpq_poly.shift...")

   S, y = PolynomialRing(QQ, "y")

   f = 3*y^2 + 7*y + 3
   
   @test shift_left(f, 7) == 3*y^9 + 7*y^8 + 3*y^7

   @test shift_right(f, 3) == 0

   println("PASS")
end

function test_fmpq_poly_powering()
   print("fmpq_poly.powering...")

   S, y = PolynomialRing(QQ, "y")

   f = 3*y^2 + 7*y + 3
   
   @test f^5 == 243*y^10 + 2835*y^9 + 14445*y^8 + 42210*y^7 + 78135*y^6 + 95557*y^5 + 78135*y^4 + 42210*y^3 + 14445*y^2 + 2835*y + 243

   println("PASS")
end

function test_fmpq_poly_modular_arithmetic()
   print("fmpq_poly.modular_arithmetic...")

   S, y = PolynomialRing(QQ, "y")
   
   f = 7y + 1
   g = 11y^2 + 12y + 21
   h = 17y^5 + 2y + 1

   @test invmod(f, g) == -fmpz(77)//956*y-fmpz(73)//956
   
   @test mulmod(f, g, h) == 77*y^3 + 95*y^2 + 159*y + 21
   
   @test powmod(f, 3, h) == 343*y^3 + 147*y^2 + 21*y + 1
   
   println("PASS")
end

function test_fmpq_poly_exact_division()
   print("fmpq_poly.exact_division...")

   S, y = PolynomialRing(QQ, "y")

   f = 3*y^2 + 7*y + 3
   g = 11*y^2 + 2*y + 3
   
   @test divexact(f*g, f) == g

   println("PASS")
end

function test_fmpq_poly_adhoc_exact_division()
   print("fmpq_poly.adhoc_exact_division...")

   S, y = PolynomialRing(QQ, "y")

   f = 3*y^2 + 7*y + 3
   
   @test divexact(3*f, 3) == f

   @test divexact(fmpz(3)*f, fmpz(3)) == f

   @test divexact(fmpz(12)//7*f, fmpz(12)//7) == f

   @test divexact(fmpz(12)//7*f, (12//7)) == f

   @test divexact(fmpz(12)//7*f, BigInt(12)//BigInt(7)) == f

   println("PASS")
end

function test_fmpq_poly_euclidean_division()
   print("fmpq_poly.euclidean_division...")

   S, y = PolynomialRing(QQ, "y")

   f = y^3 + 3*y^2 + 7*y + 3
   g = 11*y^2 + 2*y + 3
   
   @test mod(f, g) == fmpz(752)//121*y+fmpz(270)//121
   
   @test divrem(f, g) == (fmpz(1)//11*y+fmpz(31)//121, fmpz(752)//121*y+fmpz(270)//121)
 
   println("PASS")
end

function test_fmpq_poly_content_primpart_gcd()
   print("fmpq_poly.content_primpart_gcd...")

   S, y = PolynomialRing(QQ, "y")

   k = 3y^2 + 7y + 3
   l = 11y + 5
   m = y^2 + 17

   @test content(k) == 1

   @test primpart(k*fmpz(13)//6) == k

   @test gcd(k*m, l*m) == m

   @test lcm(k*m, l*m) == k*l*m

   println("PASS")
end

function test_fmpq_poly_evaluation()
   print("fmpq_poly.evaluation...")

   S, y = PolynomialRing(QQ, "y")

   f = fmpz(12)//7
   g = 3y^2 + 11*y + 3

   @test evaluate(g, 3) == 63

   @test evaluate(g, fmpz(3)) == 63

   @test evaluate(g, fmpq(3, 1)) == 63

   @test evaluate(g, 3//1) == 63

   @test evaluate(g, BigInt(3)//BigInt(1)) == 63

   @test evaluate(g, f) == fmpz(1503)//49

if VERSION >= v"0.5.0-dev+3171"

   @test g(3) == 63

   @test g(fmpz(3)) == 63

   @test g(fmpq(3, 1)) == 63

   @test g(3//1) == 63

   @test g(BigInt(3)//BigInt(1)) == 63

   @test g(f) == fmpz(1503)//49
end

   println("PASS")
end

function test_fmpq_poly_composition()
   print("fmpq_poly.composition...")

   S, y = PolynomialRing(QQ, "y")

   f = 7y^2 + 12y + 3
   g = 11y + 9

   @test compose(f, g) == 847*y^2 + 1518*y + 678

   println("PASS")
end

function test_fmpq_poly_derivative()
   print("fmpq_poly.derivative...")

   S, y = PolynomialRing(QQ, "y")

   h = 17y^2 + 2y + 3

   @test derivative(h) == 34y + 2

   println("PASS")
end

function test_fmpq_poly_integral()
   print("fmpq_poly.integral...")

   S, y = PolynomialRing(QQ, "y")
   
   f = 17y^2 + 2y - 11
   
   @test integral(f) == fmpz(17)//3*y^3 + y^2 - 11y

   println("PASS")
end

function test_fmpq_poly_resultant()
   print("fmpq_poly.resultant...")

   S, y = PolynomialRing(QQ, "y")

   f = 13y^2 + 7y + 3
   g = 6y + 11

   @test resultant(f, g) == 1219

   println("PASS")
end

function test_fmpq_poly_discriminant()
   print("fmpq_poly.discriminant...")

   S, y = PolynomialRing(QQ, "y")

   f = 17y^2 + 11y + 3

   @test discriminant(f) == -83

   println("PASS")
end

function test_fmpq_poly_gcdx()
   print("fmpq_poly.gcdx...")

   S, y = PolynomialRing(QQ, "y")

   f = 17y^2 + 11y + 3
   g = 61y - 9

   @test gcdx(f, g) == (1, fmpz(3721)//18579, -fmpz(1037)//18579*y-fmpz(824)//18579)

   println("PASS")
end

function test_fmpq_poly_factor()
   print("fmpq_poly.factor...")

   S, y = PolynomialRing(QQ, "y")

   f = (2y + 1)^10*(5*y^3 + 1)^100*(-fmpq(1,5))

   fac = factor(f)

   @test f == unit(fac) * prod([ p^e for (p, e) in fac])

   println("PASS")
end
function test_fmpq_poly_signature()
   print("fmpq_poly.signature...")

   R, x = PolynomialRing(QQ, "x")

   f = (x^3 + 3x + QQ(2)//QQ(3))

   @test signature(f) == (1, 1)

   println("PASS")
end

function test_fmpq_poly_special()
   print("fmpq_poly.special...")

   S, y = PolynomialRing(QQ, "y")

   @test chebyshev_t(20, y) == 524288*y^20-2621440*y^18+5570560*y^16-6553600*y^14+4659200*y^12-2050048*y^10+549120*y^8-84480*y^6+6600*y^4-200*y^2+1

   @test chebyshev_u(15, y) == 32768*y^15-114688*y^13+159744*y^11-112640*y^9+42240*y^7-8064*y^5+672*y^3-16*y

   println("PASS")
end

function test_fmpq_poly_Polynomials()
   print("fmpq_poly.Polynomials...")

   R, x = PolynomialRing(QQ, "x")
   S, y = PolynomialRing(R, "y")

   f = (3x^2 + 2x + 1)*y^3 + (2x^2 + 4)*y^2 + 4x*y + (2x^2 - x + 1)

   @test f^40*f^60 == f^50*f^50

   println("PASS")
end

function test_fmpq_poly_remove_valuation()
   print("fmpq_poly.remove_valuation...")

   S, y = PolynomialRing(FlintQQ, "y")

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

function test_fmpq_poly()
   test_fmpq_poly_constructors()
   test_fmpq_poly_printing()
   test_fmpq_poly_manipulation()
   test_fmpq_poly_binary_ops()
   test_fmpq_poly_adhoc_binary()
   test_fmpq_poly_comparison()
   test_fmpq_poly_adhoc_comparison()
   test_fmpq_poly_unary_ops()
   test_fmpq_poly_truncation()
   test_fmpq_poly_reverse()
   test_fmpq_poly_shift()
   test_fmpq_poly_powering()
   test_fmpq_poly_modular_arithmetic()
   test_fmpq_poly_exact_division()
   test_fmpq_poly_adhoc_exact_division()
   test_fmpq_poly_euclidean_division()
   test_fmpq_poly_content_primpart_gcd()
   test_fmpq_poly_evaluation()
   test_fmpq_poly_composition()
   test_fmpq_poly_derivative()
   test_fmpq_poly_integral()
   test_fmpq_poly_resultant()
   test_fmpq_poly_discriminant()
   test_fmpq_poly_gcdx()
   test_fmpq_poly_factor()
   test_fmpq_poly_signature()
   test_fmpq_poly_special()
   test_fmpq_poly_Polynomials()
   test_fmpq_poly_remove_valuation()

   println("")
end
