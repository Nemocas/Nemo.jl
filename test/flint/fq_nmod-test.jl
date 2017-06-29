function test_fq_nmod_constructors()
   print("fq_nmod.constructors...")

   R, x = FiniteField(7, 5, "x")

   @test elem_type(R) == fq_nmod
   @test elem_type(FqNmodFiniteField) == fq_nmod
   @test parent_type(fq_nmod) == FqNmodFiniteField

   Sy, y = PolynomialRing(ResidueRing(FlintZZ, 3), "y")

   T, z = FiniteField(y^2 + 1, "z")

   @test isa(R, FqNmodFiniteField)
   @test isa(T, FqNmodFiniteField)

   @test isa(3x^4 + 2x^3 + 4x^2 + x + 1, fq_nmod)
   @test isa(z^2 + z + 1, fq_nmod)

   a = R()
   
   @test isa(a, fq_nmod)

   b = R(4)
   c = R(fmpz(7))

   @test isa(b, fq_nmod)

   @test isa(c, fq_nmod)

   d = R(c)

   @test isa(d, fq_nmod)

   println("PASS")
end

function test_fq_nmod_printing()
   print("fq_nmod.printing...")

   R, x = FiniteField(7, 5, "x")

   a = 3x^4 + 2x^3 + 4x^2 + x + 1

   @test string(a) == "3*x^4+2*x^3+4*x^2+x+1"

   println("PASS")
end

function test_fq_nmod_manipulation()
   print("fq_nmod.manipulation...")

   R, x = FiniteField(7, 5, "x")

   @test iszero(zero(R))

   @test isone(one(R))

   @test isgen(gen(R)) 

   @test characteristic(R) == 7

   @test order(R) == ZZ(7)^5

   @test degree(R) == 5

   @test isunit(x + 1)

   @test deepcopy(x + 1) == x + 1

   @test coeff(2x + 1, 1) == 2

   println("PASS")
end

function test_fq_nmod_unary_ops()
   print("fq_nmod.unary_ops...")

   R, x = FiniteField(7, 5, "x")

   a = x^4 + 3x^2 + 6x + 1
   
   @test -a == 6*x^4+4*x^2+x+6

   println("PASS")
end

function test_fq_nmod_binary_ops()
   print("fq_nmod.binary_ops...")

   R, x = FiniteField(7, 5, "x")

   a = x^4 + 3x^2 + 6x + 1
   b = 3x^4 + 2x^2 + x + 1

   @test a + b == 4*x^4+5*x^2+2

   @test a - b == 5*x^4+x^2+5*x

   @test a*b == 3*x^3+2

   println("PASS")
end

function test_fq_nmod_adhoc_binary()
   print("fq_nmod.adhoc_binary...")

   R, x = FiniteField(7, 5, "x")

   a = x^4 + 3x^2 + 6x + 1

   @test 3a == 3*x^4+2*x^2+4*x+3

   @test a*3 == 3*x^4+2*x^2+4*x+3

   @test a*fmpz(5) == 5*x^4+x^2+2*x+5

   @test fmpz(5)*a == 5*x^4+x^2+2*x+5

   @test 12345678901234567890123*a == 3*x^4+2*x^2+4*x+3

   @test a*12345678901234567890123 == 3*x^4+2*x^2+4*x+3

   println("PASS")
end

function test_fq_nmod_powering()
   print("fq_nmod.powering...")

   R, x = FiniteField(7, 5, "x")

   a = x^4 + 3x^2 + 6x + 1

   @test a^3 == x^4+6*x^3+5*x^2+5*x+6
   
   @test a^fmpz(-5) == x^4+4*x^3+6*x^2+6*x+2

   println("PASS")
end

function test_fq_nmod_comparison()
   print("fq_nmod.comparison...")
  
   R, x = FiniteField(7, 5, "x")

   a = x^4 + 3x^2 + 6x + 1
   b = 3x^4 + 2x^2 + 2

   @test b != a
   @test R(3) == R(3)
   @test isequal(R(3), R(3))

   println("PASS")
end

function test_fq_nmod_inversion()
   print("fq_nmod.inversion...")
  
   R, x = FiniteField(7, 5, "x")

   a = x^4 + 3x^2 + 6x + 1

   b = inv(a)

   @test b == x^4+5*x^3+4*x^2+5*x

   @test b == a^-1

   println("PASS")
end

function test_fq_nmod_exact_division()
   print("fq_nmod.exact_division...")
  
   R, x = FiniteField(7, 5, "x")

   a = x^4 + 3x^2 + 6x + 1
   b = 3x^4 + 2x^2 + 2

   @test divexact(a, b) == 3*x^4+2*x^3+2*x^2+5*x

   @test b//a == 4*x^2+6*x+5

   println("PASS")
end

function test_fq_nmod_gcd()
   print("fq_nmod.gcd...")

   R, x = FiniteField(7, 5, "x")

   a = x^4 + 3x^2 + 6x + 1
   b = 3x^4 + 2x^2 + x + 1

   @test gcd(a, b) == 1

   @test gcd(R(0), R(0)) == 0

   println("PASS")
end

function test_fq_nmod_special_functions()
   print("fq_nmod.special_functions...")
  
   R, x = FiniteField(7, 5, "x")

   a = x^4 + 3x^2 + 6x + 1

   @test trace(a) == 1

   @test norm(a) == 4

   @test frobenius(a) == x^4+2*x^3+3*x^2+5*x+1

   @test frobenius(a, 3) == 3*x^4+3*x^3+3*x^2+x+4

   @test pth_root(a) == 4*x^4+3*x^3+4*x^2+5*x+2

   println("PASS")
end

function test_fq_nmod()
   test_fq_nmod_constructors()
   test_fq_nmod_printing()
   test_fq_nmod_manipulation()
   test_fq_nmod_unary_ops()
   test_fq_nmod_binary_ops()
   test_fq_nmod_adhoc_binary()
   test_fq_nmod_powering()
   test_fq_nmod_comparison()
   test_fq_nmod_inversion()
   test_fq_nmod_exact_division()
   test_fq_nmod_gcd()
   test_fq_nmod_special_functions()

   println("")
end
