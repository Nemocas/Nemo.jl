function test_nf_accessor_functions()
   print("nf_elem.accessors...")
   t = create_accessors(AnticNumberField, fmpz, get_handle())
   get_test = t[1]
   set_test = t[2]

   R, x = PolynomialRing(QQ, "x")
   K, a = NumberField(x^3 + 3x + 1, "a")

   set_test(K, fmpz(2))
   @test 2 == get_test(K)
   println("PASS")
end

function test_nf_elem_constructors()
   print("nf_elem.constructors...")

   R, x = PolynomialRing(QQ, "x")
   K, a = NumberField(x^3 + 3x + 1, "a")

   @test elem_type(K) == nf_elem
   @test elem_type(AnticNumberField) == nf_elem
   @test parent_type(nf_elem) == AnticNumberField

   @test isa(K, AnticNumberField)

   a = K(123)

   @test isa(a, nf_elem)

   b = K(a)

   @test isa(b, nf_elem)

   c = K(fmpz(12))

   @test isa(c, nf_elem)

   d = K()

   @test isa(d, nf_elem)

   f = K(fmpq(2, 3))

   @test isa(f, nf_elem)

   g = K(x^2 + 2x - 7)

   @test isa(g, nf_elem)

   println("PASS")
end

function test_nf_elem_printing()
   print("nf_elem.constructors...")
 
   R, x = PolynomialRing(QQ, "x")
   K, a = NumberField(x^3 + 3x + 1, "a")
   g = K(x^2 + 2x - 7)

   @test string(g) == "a^2 + 2*a - 7"

   println("PASS")
end

function test_nf_elem_fmpz_mat_conversions()
   print("nf_elem.fmpz_mat_conversions...")

   R, x = PolynomialRing(QQ, "x")
   K, a = NumberField(x^3 + 3x + 1, "a")
   M = MatrixSpace(FlintZZ, 1, 3)(0)

   M[1, 1] = 1
   M[1, 2] = 2
   M[1, 3] = 3
   
   @test Nemo.elem_from_mat_row(K, M, 1, fmpz(5)) == (1 + 2*a + 3*a^2)//5

   b = (1 + a + 5*a^2)//3
   d = fmpz()

   Nemo.elem_to_mat_row!(M, 1, d, b)

   @test d == 3
   @test M == MatrixSpace(FlintZZ, 1, 3)([1 1 5])

   println("PASS")
end

function test_nf_elem_fmpq_poly_conversion()
   print("nf_elem.fmpq_poly_conversion...")

   R, x = PolynomialRing(QQ, "x")
   K, a = NumberField(x^3 + 3x + 1, "a")

   @test R(a^2 + a) == x^2 + x

   K, a = NumberField(x^2 - 7, "a")

   @test R(a + 1) == x + 1

   K, a = NumberField(x - 7, "a")

   @test R(a) == R(7)

   println("PASS")
end

function test_nf_elem_denominator()
   print("nf_elem.denominator...")

   R, x = PolynomialRing(QQ, "x")
   K, a = NumberField(x^3 + 3x + 1, "a")

   b = a//5

   @test den(b) == 5

   println("PASS")
end

function test_nf_elem_conversions()
   print("nf_elem.conversions...")
 
   R, x = PolynomialRing(QQ, "x")
   K, a = NumberField(x^3 + 3x + 1, "a")

   f = x^2 + 2x - 7

   @test R(K(f)) == f

   println("PASS")
end

function test_nf_elem_manipulation()
   print("nf_elem.manipulation...")
 
   R, x = PolynomialRing(QQ, "x")
   K, a = NumberField(x^3 + 3x + 1, "a")

   d = K(x^2 + 2x - 7)

   @test iszero(zero(K))
   @test isone(one(K))
   @test isgen(gen(K))

   @test deepcopy(d) == d

   @test coeff(d, 1) == 2
   @test coeff(d, 3) == 0

   @test degree(K) == 3

   @test signature(K) == (1, 1)

   println("PASS")
end

function test_nf_elem_unary_ops()
   print("nf_elem.unary_ops...")
 
   R, x = PolynomialRing(QQ, "x")
   K, a = NumberField(x^3 + 3x + 1, "a")

   d = a^2 + 2a - 7

   @test -d == -a^2 - 2a + 7

   println("PASS")
end

function test_nf_elem_binary_ops()
   print("nf_elem.binary_ops...")
 
   R, x = PolynomialRing(QQ, "x")
   K, a = NumberField(x^3 + 3x + 1, "a")

   c = a^2 + 2a - 7
   d = 3a^2 - a + 1

   @test c + d == 4a^2 + a - 6

   @test c - d == -2a^2 + 3a - 8

   @test c*d == -31*a^2 - 9*a - 12

   println("PASS")
end

function test_nf_elem_adhoc_binary()
   print("nf_elem.adhoc_binary...")
 
   R, x = PolynomialRing(QQ, "x")
   K, a = NumberField(x^3 + 3x + 1, "a")

   d = 3a^2 - a + 1

   @test d + 3 == 3 + d
   @test d + fmpz(3) == fmpz(3) + d
   @test d + fmpq(2, 3) == fmpq(2, 3) + d
   @test d - 3 == -(3 - d)
   @test d - fmpz(3) == -(fmpz(3) - d)
   @test d - fmpq(2, 3) == -(fmpq(2, 3) - d)
   @test d*3 == 3d
   @test d*fmpz(3) == fmpz(3)*d
   @test d*fmpq(2, 3) == fmpq(2, 3)*d
   
   println("PASS")
end

function test_nf_elem_powering()
   print("nf_elem.powering...")
 
   R, x = PolynomialRing(QQ, "x")
   K, a = NumberField(x^3 + 3x + 1, "a")

   d = a^2 + 2a - 7

   @test d^5 == -13195*a^2 + 72460*a + 336
   @test d^(-2) == fmpz(2773)//703921*a^2 + fmpz(1676)//703921*a + fmpz(12632)//703921
   @test d^0 == 1

   println("PASS")
end

function test_nf_elem_comparison()
   print("nf_elem.comparison...")
 
   R, x = PolynomialRing(QQ, "x")
   K, a = NumberField(x^3 + 3x + 1, "a")

   c = 3a^2 - a + 1
   d = a^2 + 2a - 7

   @test c != d
   @test c == 3a^2 - a + 1

   println("PASS")
end

function test_nf_elem_adhoc_comparison()
   print("nf_elem.adhoc_comparison...")
 
   R, x = PolynomialRing(QQ, "x")
   K, a = NumberField(x^3 + 3x + 1, "a")

   c = 3a^2 - a + 1

   @test c != 5
   @test K(5) == 5
   @test K(5) == fmpz(5)
   @test K(fmpq(2, 3)) == fmpq(2, 3)
   @test 5 == K(5)
   @test fmpz(5) == K(5)
   @test fmpq(2, 3) == K(fmpq(2, 3))

   println("PASS")
end

function test_nf_elem_inversion()
   print("nf_elem.inversion...")
 
   R, x = PolynomialRing(QQ, "x")
   K, a = NumberField(x^3 + 3x + 1, "a")

   c = 3a^2 - a + 1

   @test inv(c)*c == 1

   println("PASS")
end

function test_nf_elem_exact_division()
   print("nf_elem.exact_division...")
 
   R, x = PolynomialRing(QQ, "x")
   K, a = NumberField(x^3 + 3x + 1, "a")

   c = 3a^2 - a + 1
   d = a^2 + 2a - 7

   @test divexact(c, d) == c*inv(d)

   println("PASS")
end

function test_nf_elem_adhoc_exact_division()
   print("nf_elem.adhoc_exact_division...")
 
   R, x = PolynomialRing(QQ, "x")
   K, a = NumberField(x^3 + 3x + 1, "a")

   c = 3a^2 - a + 1
   
   @test divexact(7c, 7) == c
   @test divexact(7c, fmpz(7)) == c
   @test divexact(fmpq(2, 3)*c, fmpq(2, 3)) == c
  
   println("PASS")
end

function test_nf_elem_divides()
   print("nf_elem.divides...")
 
   R, x = PolynomialRing(QQ, "x")
   K, a = NumberField(x^3 + 3x + 1, "a")

   c = 3a^2 - a + 1
   d = a^2 + 2a - 7

   flag, q = divides(c, d)

   @test flag
   @test q == divexact(c, d)

   println("PASS")
end

function test_nf_elem_norm_trace()
   print("nf_elem.norm_trace...")
 
   R, x = PolynomialRing(QQ, "x")
   K, a = NumberField(x^3 + 3x + 1, "a")

   c = 3a^2 - a + 1
   
   @test norm(c) == 113
   @test trace(c) == -15
  
   println("PASS")
end

function test_nf_elem_Polynomials()
   print("nf_elem.Polynomials...")
 
   R, x = PolynomialRing(QQ, "x")
   K, a = NumberField(x^3 + 3x + 1, "a")
   S, y = PolynomialRing(K, "y")

   f = (3a^2 - a + 1)*y^2 + (3a - 1)*y + (2a^2 - a - 2)
   
   @test f^20*f^30 == f^25*f^25
   @test f^20*f^30 == mul_classical(f^20, f^30)
   @test f^20*f^30 == sqr_classical(f^25)
  
   println("PASS")
end

function test_nf_elem()
   test_nf_accessor_functions()
   test_nf_elem_constructors()
   test_nf_elem_printing()
   test_nf_elem_fmpz_mat_conversions()
   test_nf_elem_fmpq_poly_conversion()
   test_nf_elem_denominator()
   test_nf_elem_conversions()
   test_nf_elem_manipulation()
   test_nf_elem_unary_ops()
   test_nf_elem_binary_ops()
   test_nf_elem_adhoc_binary()
   test_nf_elem_powering()
   test_nf_elem_comparison()
   test_nf_elem_adhoc_comparison()
   test_nf_elem_inversion()
   test_nf_elem_exact_division()
   test_nf_elem_adhoc_exact_division()
   test_nf_elem_divides()
   test_nf_elem_norm_trace()
   test_nf_elem_Polynomials()

   println("")
end
