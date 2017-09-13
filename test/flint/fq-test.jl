function test_fq_constructors()
   print("fq.constructors...")

   R, x = FiniteField(fmpz(7), 5, "x")

   @test elem_type(R) == fq
   @test elem_type(FqFiniteField) == fq
   @test parent_type(fq) == FqFiniteField
   
   Sy, y = PolynomialRing(ResidueRing(FlintZZ, 36893488147419103363), "y")

   T, z = FiniteField(y^2 + 1, "z")

   @test isa(R, FqFiniteField)
   @test isa(T, FqFiniteField)

   @test isa(3x^4 + 2x^3 + 4x^2 + x + 1, fq)
   @test isa(z^2 + z + 1, fq)

   a = R()
   
   @test isa(a, fq)

   b = R(4)
   c = R(fmpz(7))

   @test isa(b, fq)

   @test isa(c, fq)

   d = R(c)

   @test isa(d, fq)

   println("PASS")
end

function test_fq_printing()
   print("fq.printing...")

   R, x = FiniteField(fmpz(7), 5, "x")

   a = 3x^4 + 2x^3 + 4x^2 + x + 1

   @test string(a) == "3*x^4+2*x^3+4*x^2+x+1"

   println("PASS")
end

function test_fq_manipulation()
   print("fq.manipulation...")

   R, x = FiniteField(fmpz(7), 5, "x")

   @test iszero(zero(R))

   @test isone(one(R))

   @test isgen(gen(R)) 

   @test characteristic(R) == 7

   @test order(R) == fmpz(7)^5

   @test degree(R) == 5

   @test isunit(x + 1)

   @test deepcopy(x + 1) == x + 1

   @test coeff(2x + 1, 1) == 2

   println("PASS")
end

function test_fq_unary_ops()
   print("fq.unary_ops...")

   R, x = FiniteField(fmpz(7), 5, "x")

   a = x^4 + 3x^2 + 6x + 1
   
   @test -a == 6*x^4+4*x^2+x+6

   println("PASS")
end

function test_fq_binary_ops()
   print("fq.binary_ops...")

   R, x = FiniteField(fmpz(7), 5, "x")

   a = x^4 + 3x^2 + 6x + 1
   b = 3x^4 + 2x^2 + x + 1

   @test a + b == 4*x^4+5*x^2+2

   @test a - b == 5*x^4+x^2+5*x

   @test a*b == 3*x^3+2

   println("PASS")
end

function test_fq_adhoc_binary()
   print("fq.adhoc_binary...")

   R, x = FiniteField(fmpz(7), 5, "x")

   a = x^4 + 3x^2 + 6x + 1

   @test 3a == 3*x^4+2*x^2+4*x+3

   @test a*3 == 3*x^4+2*x^2+4*x+3

   @test a*fmpz(5) == 5*x^4+x^2+2*x+5

   @test fmpz(5)*a == 5*x^4+x^2+2*x+5

   @test 12345678901234567890123*a == 3*x^4+2*x^2+4*x+3

   @test a*12345678901234567890123 == 3*x^4+2*x^2+4*x+3

   println("PASS")
end

function test_fq_powering()
   print("fq.powering...")

   R, x = FiniteField(fmpz(7), 5, "x")

   a = x^4 + 3x^2 + 6x + 1

   @test a^3 == x^4+6*x^3+5*x^2+5*x+6
   
   @test a^fmpz(-5) == x^4+4*x^3+6*x^2+6*x+2

   println("PASS")
end

function test_fq_comparison()
   print("fq.comparison...")
  
   R, x = FiniteField(fmpz(7), 5, "x")

   a = x^4 + 3x^2 + 6x + 1
   b = 3x^4 + 2x^2 + 2

   @test b != a
   @test R(3) == R(3)
   @test isequal(R(3), R(3))

   println("PASS")
end

function test_fq_inversion()
   print("fq.inversion...")
  
   R, x = FiniteField(fmpz(7), 5, "x")

   a = x^4 + 3x^2 + 6x + 1

   b = inv(a)

   @test b == x^4+5*x^3+4*x^2+5*x

   @test b == a^-1

   println("PASS")
end

function test_fq_exact_division()
   print("fq.exact_division...")
  
   R, x = FiniteField(fmpz(7), 5, "x")

   a = x^4 + 3x^2 + 6x + 1
   b = 3x^4 + 2x^2 + 2

   @test divexact(a, b) == 3*x^4+2*x^3+2*x^2+5*x

   @test b//a == 4*x^2+6*x+5

   println("PASS")
end

function test_fq_gcd()
   print("fq.gcd...")

   R, x = FiniteField(fmpz(7), 5, "x")

   a = x^4 + 3x^2 + 6x + 1
   b = 3x^4 + 2x^2 + x + 1

   @test gcd(a, b) == 1

   @test gcd(R(0), R(0)) == 0

   println("PASS")
end

function test_fq_special_functions()
   print("fq.special_functions...")
  
   R, x = FiniteField(fmpz(7), 5, "x")

   a = x^4 + 3x^2 + 6x + 1

   @test trace(a) == 1

   @test norm(a) == 4

   @test frobenius(a) == x^4+2*x^3+3*x^2+5*x+1

   @test frobenius(a, 3) == 3*x^4+3*x^3+3*x^2+x+4

   @test pth_root(a) == 4*x^4+3*x^3+4*x^2+5*x+2

   println("PASS")
end

function test_fq_rand()
   print("fq.rand...")
   
   R, x = FiniteField(fmpz(17), 3, "x")
   
   @inferred rand(R)
   
   println("PASS")
end

function test_fq()
   test_fq_constructors()
   test_fq_printing()
   test_fq_manipulation()
   test_fq_unary_ops()
   test_fq_binary_ops()
   test_fq_adhoc_binary()
   test_fq_powering()
   test_fq_comparison()
   test_fq_inversion()
   test_fq_exact_division()
   test_fq_gcd()
   test_fq_special_functions()
   test_fq_rand()
   
   println("")
end
