function test_fmpz_poly_constructors()
   print("fmpz_poly.constructors...")
 
   R, x = PolynomialRing(ZZ, "x")

   @test elem_type(R) == fmpz_poly
   @test elem_type(FmpzPolyRing) == fmpz_poly
   @test parent_type(fmpz_poly) == FmpzPolyRing

   @test typeof(R) <: FmpzPolyRing

   @test isa(x, PolyElem)

   f = x^3 + 2x^2 + x + 1

   @test isa(f, PolyElem)

   g = R(2)

   @test isa(g, PolyElem)

   h = R(x^2 + 2x + 1)

   @test isa(h, PolyElem)

   k = R([ZZ(1), ZZ(0), ZZ(3)])

   @test isa(k, PolyElem)

   l = R([1, 2, 3])

   @test isa(l, PolyElem)

   println("PASS")
end

function test_fmpz_poly_printing()
   print("fmpz_poly.printing...")
 
   R, x = PolynomialRing(ZZ, "x")
   f = x^3 + 2x^2 + x + 1

   @test string(f) == "x^3+2*x^2+x+1"

   println("PASS")
end

function test_fmpz_poly_manipulation()
   print("fmpz_poly.manipulation...")

   R, x = PolynomialRing(ZZ, "x")
   
   @test iszero(zero(R))
   
   @test isone(one(R))

   @test isgen(gen(R))
   
   @test isunit(one(R))

   f = x^2 + 2x + 1

   @test lead(f) == 1

   @test degree(f) == 2

   @test length(f) == 3

   @test coeff(f, 1) == 2

   @test canonical_unit(-x + 1) == -1

   @test deepcopy(f) == f

   println("PASS")
end

function test_fmpz_poly_binary_ops()
   print("fmpz_poly.binary_ops...")

   R, x = PolynomialRing(ZZ, "x")
   
   f = x^2 + 2x + 1
   g = x^3 + 3x + 2

   @test f + g == x^3+x^2+5*x+3

   @test f*g == x^5+2*x^4+4*x^3+8*x^2+7*x+2

   @test f - g == -x^3+x^2-x-1

   println("PASS")
end

function test_fmpz_poly_adhoc_binary()
   print("fmpz_poly.adhoc_binary...")

   R, x = PolynomialRing(ZZ, "x")
   
   f = x^2 + 2x + 1
   g = x^3 + 3x + 2

   @test f*12 == 12*x^2+24*x+12

   @test 7*g == 7*x^3+21*x+14

   @test fmpz(3)*g == 3*x^3+9*x+6

   @test f*fmpz(2) == 2*x^2+4*x+2

   @test f + 12 == x^2+2*x+13

   @test f + fmpz(12) == x^2+2*x+13

   @test f - 12 == x^2+2*x-11

   @test f - fmpz(12) == x^2+2*x-11

   @test 12 + g == x^3+3*x+14

   @test fmpz(12) + g == x^3+3*x+14

   @test 12 - g == -x^3-3*x+10

   @test fmpz(12) - g == -x^3-3*x+10

   println("PASS")
end

function test_fmpz_poly_comparison()
   print("fmpz_poly.comparison...")

   R, x = PolynomialRing(ZZ, "x")

   f = x^2 + 2x + 1
   g = x^3 + 3x + 2

   @test f != g

   @test isequal(f, deepcopy(f))

   println("PASS")
end

function test_fmpz_poly_adhoc_comparison()
   print("fmpz_poly.adhoc_comparison...")

   R, x = PolynomialRing(ZZ, "x")
   
   f = x^2 + 2x + 1

   @test f != 1 

   @test 1 != f 

   @test R(7) == fmpz(7)

   @test fmpz(7) != f

   println("PASS")
end

function test_fmpz_poly_unary_ops()
   print("fmpz_poly.unary_ops...")

   R, x = PolynomialRing(ZZ, "x")
   
   f = x^2 + 2x + 1

   @test -f == -x^2 - 2x - 1

   println("PASS")
end

function test_fmpz_poly_truncation()
   print("fmpz_poly.truncation...")

   R, x = PolynomialRing(ZZ, "x")
  
   f = x^2 + 2x + 1
   g = x^3 + 3x + 1

   @test truncate(f, 2) == 2*x+1

   @test mullow(f, g, 3) == 7*x^2+5*x+1

   println("PASS")
end

function test_fmpz_poly_reverse()
   print("fmpz_poly.reverse...")

   R, x = PolynomialRing(ZZ, "x")
   
   f = x^2 + 2x + 3

   @test reverse(f) == 3x^2 + 2x + 1

   println("PASS")
end

function test_fmpz_poly_shift()
   print("fmpz_poly.shift...")

   R, x = PolynomialRing(ZZ, "x")

   f = x^2 + 2x + 1

   @test shift_left(f, 3) == x^5 + 2x^4 + x^3

   @test shift_right(f, 1) == x + 2

   println("PASS")
end

function test_fmpz_poly_powering()
   print("fmpz_poly.powering...")

   R, x = PolynomialRing(ZZ, "x")
  
   f = x^2 + 2x + 1

   @test f^12 == x^24+24*x^23+276*x^22+2024*x^21+10626*x^20+42504*x^19+134596*x^18+346104*x^17+735471*x^16+1307504*x^15+1961256*x^14+2496144*x^13+2704156*x^12+2496144*x^11+1961256*x^10+1307504*x^9+735471*x^8+346104*x^7+134596*x^6+42504*x^5+10626*x^4+2024*x^3+276*x^2+24*x+1 

   println("PASS")
end

function test_fmpz_poly_exact_division()
   print("fmpz_poly.exact_division...")

   R, x = PolynomialRing(ZZ, "x")

   f = x^2 + 2x + 1
   g = x^3 + 3x + 1

   @test divexact(f*g, f) == g

   println("PASS")
end

function test_fmpz_poly_adhoc_exact_division()
   print("fmpz_poly.adhoc_exact_division...")

   R, x = PolynomialRing(ZZ, "x")

   f = x^2 + 2x + 1
   
   @test divexact(3*f, ZZ(3)) == f

   @test divexact(3*f, 3) == f

   println("PASS")
end

function test_fmpz_poly_pseudodivision()
   print("fmpz_poly.pseudodivision...")

   R, x = PolynomialRing(ZZ, "x")

   f = x^2 + 2x + 1
   g = x^3 + 3x + 1

   @test pseudorem(f, g) == x^2+2*x+1

   @test pseudodivrem(f, g) == (0, x^2+2*x+1)

   println("PASS")
end

function test_fmpz_poly_remove_valuation()
   print("fmpz_poly.remove_valuation...")

   R, x = PolynomialRing(ZZ, "x")

   f = x^2 + 2x + 1
   g = x^3 + 3x + 1

   v, q = divides(f*g, f)

   @test v
   @test q == g

   v, q = divides(f*g + 1, f)

   @test !v

   println("PASS")
end

function test_fmpz_poly_content_primpart_gcd()
   print("fmpz_poly.content_primpart_gcd...")

   R, x = PolynomialRing(ZZ, "x")

   f = x^2 + 2x + 1
   g = x^3 + 3x + 1
   h = x + 1

   @test content(3*f) == 3

   @test primpart(3*f) == f

   @test gcd(f*h, g*h) == h

   @test lcm(f*h, g*h) == f*g*h

   println("PASS")
end

function test_fmpz_poly_evaluation()
   print("fmpz_poly.evaluation...")

   R, x = PolynomialRing(ZZ, "x")
   
   f = x^2 + 2x + 1

   @test evaluate(f, 3) == 16
   
   @test evaluate(f, fmpz(10)) == 121

if VERSION >= v"0.5.0-dev+3171"

   @test f(3) == 16

   @test f(fmpz(10)) == 121

end

   println("PASS")
end

function test_fmpz_poly_composition()
   print("fmpz_poly.composition...")

   R, x = PolynomialRing(ZZ, "x")
   
   f = x^2 + 2x + 1
   g = x^3 + 3x + 1

   @test compose(f, g) == x^6+6*x^4+4*x^3+9*x^2+12*x+4

   println("PASS")
end

function test_fmpz_poly_derivative()
   print("fmpz_poly.derivative...")

   R, x = PolynomialRing(ZZ, "x")
   
   f = x^2 + 2x + 1

   @test derivative(f) == 2x + 2

   println("PASS")
end

function test_fmpz_poly_resultant()
   print("fmpz_poly.resultant...")

   R, x = PolynomialRing(ZZ, "x")
   
   f = 5x^2 + 2x + 1
   g = 2x^3 + 3x + 1

   @test resultant(f, g) == 212

   println("PASS")
end

function test_fmpz_poly_discriminant()
   print("fmpz_poly.discriminant...")

   R, x = PolynomialRing(ZZ, "x")
   
   f = x^2 + 2x + 1

   @test discriminant(f) == 0

   println("PASS")
end

function test_fmpz_poly_resx()
   print("fmpz_poly.resx...")

   R, x = PolynomialRing(ZZ, "x")
   
   f = 5x^2 + 2x + 1
   g = 2x^3 + 3x + 1

   @test resx(f, g) == (212, 146*x^2-58*x+213, -365*x-1)

   println("PASS")
end

function test_fmpz_poly_signature()
   print("fmpz_poly.signature...")

   R, x = PolynomialRing(ZZ, "x")

   f = x^3 + 3x + 1

   @test signature(f) == (1, 1)

   println("PASS")
end

function test_fmpz_poly_interpolate()
  print("fmpz_poly.interpolate...")
  
  Rx, x = PolynomialRing(ZZ, "x")

  xval = [ ZZ(0), ZZ(1), ZZ(2), ZZ(3) ]

  yval = [ ZZ(0), ZZ(1), ZZ(4), ZZ(9) ] 

  f = interpolate(Rx, xval, yval)

  @test parent(f) == Rx
  @test f == x^2

  println("PASS")
end

function test_fmpz_poly_factor()
  print("fmpz_poly.factor...")

  Rx, x = PolynomialRing(FlintZZ, "x")

  f = x^24 - x^23 + x^19 - x^18 + x^17 - x^16 + x^14 - x^13 + x^12 - x^11 + x^10 - x^8 + x^7 - x^6 + x^5 - x + 1
  g = x - 1

  fac = factor(-10*f^10 * g^20)

  @test -10*f^10 * g^20 == unit(fac) * prod([ p^e for (p, e) in fac])

  @test fac[f] == 10
  @test fac[g] == 20
  @test f in fac
  @test !(x in fac)

  println("PASS")
end

function test_fmpz_poly_square_root()
  print("fmpz_poly.square_root...")

  R, x = PolynomialRing(ZZ, "x")

  f = rand(R, 0:10, -10:10)
  g = f^2

  @test sqrt(g)^2 == g

  println("PASS")
end

function test_fmpz_poly_special()
   print("fmpz_poly.special...")

   R, x = PolynomialRing(ZZ, "x")

   @test chebyshev_t(20, x) == 524288*x^20-2621440*x^18+5570560*x^16-6553600*x^14+4659200*x^12-2050048*x^10+549120*x^8-84480*x^6+6600*x^4-200*x^2+1

   @test chebyshev_u(15, x) == 32768*x^15-114688*x^13+159744*x^11-112640*x^9+42240*x^7-8064*x^5+672*x^3-16*x
   
   @test cyclotomic(120, x) == x^32+x^28-x^20-x^16-x^12+x^4+1

   @test cyclotomic(10, 1+x+x^2) == x^8+4*x^7+9*x^6+13*x^5+14*x^4+11*x^3+6*x^2+2*x+1

   @test swinnerton_dyer(5, x) == x^32-448*x^30+84864*x^28-9028096*x^26+602397952*x^24-26625650688*x^22+801918722048*x^20-16665641517056*x^18+239210760462336*x^16-2349014746136576*x^14+15459151516270592*x^12-65892492886671360*x^10+172580952324702208*x^8-255690851718529024*x^6+183876928237731840*x^4-44660812492570624*x^2+2000989041197056

   @test cos_minpoly(30, x) == x^4+x^3-4*x^2-4*x+1

   @test theta_qexp(3, 30, x) == 72*x^29+32*x^27+72*x^26+30*x^25+24*x^24+24*x^22+48*x^21+24*x^20+24*x^19+36*x^18+48*x^17+6*x^16+48*x^14+24*x^13+8*x^12+24*x^11+24*x^10+30*x^9+12*x^8+24*x^6+24*x^5+6*x^4+8*x^3+12*x^2+6*x+1

   @test eta_qexp(24, 30, x) == -29211840*x^29+128406630*x^28+24647168*x^27-73279080*x^26+13865712*x^25-25499225*x^24+21288960*x^23+18643272*x^22-12830688*x^21-4219488*x^20-7109760*x^19+10661420*x^18+2727432*x^17-6905934*x^16+987136*x^15+1217160*x^14+401856*x^13-577738*x^12-370944*x^11+534612*x^10-115920*x^9-113643*x^8+84480*x^7-16744*x^6-6048*x^5+4830*x^4-1472*x^3+252*x^2-24*x+1

   println("PASS")
end

function test_fmpz_poly_Polynomials()
   print("fmpz_poly.Polynomials...")

   R, x = PolynomialRing(ZZ, "x")
   S, y = PolynomialRing(R, "y")

   f = (3x^2 + 2x + 1)*y^3 + (2x^2 + 4)*y^2 + 4x*y + (2x^2 - x + 1)

   @test f^40*f^60 == f^50*f^50

   println("PASS")
end

function test_fmpz_poly()
   test_fmpz_poly_constructors()
   test_fmpz_poly_printing()
   test_fmpz_poly_manipulation()
   test_fmpz_poly_binary_ops()
   test_fmpz_poly_adhoc_binary()
   test_fmpz_poly_comparison()
   test_fmpz_poly_adhoc_comparison()
   test_fmpz_poly_unary_ops()
   test_fmpz_poly_truncation()
   test_fmpz_poly_reverse()
   test_fmpz_poly_shift()
   test_fmpz_poly_powering()
   test_fmpz_poly_exact_division()
   test_fmpz_poly_adhoc_exact_division()
   test_fmpz_poly_pseudodivision()
   test_fmpz_poly_remove_valuation()
   test_fmpz_poly_content_primpart_gcd()
   test_fmpz_poly_evaluation()
   test_fmpz_poly_composition()
   test_fmpz_poly_derivative()
   test_fmpz_poly_resultant()
   test_fmpz_poly_discriminant()
   test_fmpz_poly_resx()
   test_fmpz_poly_signature()
   test_fmpz_poly_interpolate()
   test_fmpz_poly_factor()
   test_fmpz_poly_square_root()
   test_fmpz_poly_special()
   #test_fmpz_poly_Polynomials()

   println("")
end
