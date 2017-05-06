function test_padic_constructors()
   print("padic.constructors()...")  

   R = PadicField(7, 30)
   
   @test isa(R, FlintPadicField)

   S = PadicField(fmpz(65537), 30)

   @test isa(S, FlintPadicField)

   @test isa(R(), padic)

   @test isa(R(1), padic)

   @test isa(R(ZZ(123)), padic)

   @test isa(R(ZZ(1)//7^2), padic)

   @test isa(1 + 2*7 + 4*7^2 + O(R, 7^3), padic)

   @test isa(13 + 357*fmpz(65537) + O(S, fmpz(65537)^12), padic)

   @test isa(fmpz(1)//7^2 + fmpz(2)//7 + 3 + 4*7 + O(R, 7^2), padic)

   println("PASS")
end

function test_padic_printing()
   print("padic.constructors()...")  

   R = PadicField(7, 30)
   
   a = 1 + 2*7 + 4*7^2 + O(R, 7^3)

   @test string(a) == "1 + 2*7^1 + 4*7^2 + O(7^3)"

   println("PASS")
end

function test_padic_manipulation()
   print("padic.manipulation()...")  

   R = PadicField(7, 30)

   a = 1 + 2*7 + 4*7^2 + O(R, 7^3)
   b = 7^2 + 3*7^3 + O(R, 7^5)
   c = R(2)

   @test isone(one(R))

   @test iszero(zero(R))

   @test precision(a) == 3

   @test prime(R) == 7

   @test valuation(b) == 2

   @test lift(FlintZZ, a) == 211

   @test lift(FlintQQ, divexact(a, b)) == fmpq(337, 49)

   println("PASS")
end

function test_padic_unary_ops()
   print("padic.unary_ops()...")  

   R = PadicField(7, 30)

   a = 1 + 2*7 + 4*7^2 + O(R, 7^3)
   b = R(0)

   @test -a == 6 + 4*7^1 + 2*7^2 + O(R, 7^3)
   
   @test iszero(-b)

   println("PASS")
end

function test_padic_binary_ops()
   print("padic.binary_ops()...")  

   R = PadicField(7, 30)

   a = 1 + 2*7 + 4*7^2 + O(R, 7^3)
   b = 7^2 + 3*7^3 + O(R, 7^5)
   c = O(R, 7^3)
   d = R(2)

   @test a + b == 1 + 2*7^1 + 5*7^2 + O(R, 7^3)

   @test a - b == 1 + 2*7^1 + 3*7^2 + O(R, 7^3)

   @test a*b == 1*7^2 + 5*7^3 + 3*7^4 + O(R, 7^5)

   @test b*c == O(R, 7^5)

   @test a*d == 2 + 4*7^1 + 1*7^2 + O(R, 7^3)

   println("PASS")
end

function test_padic_adhoc_binary()
   print("padic.adhoc_binary()...")  

   R = PadicField(7, 30)

   a = 1 + 2*7 + 4*7^2 + O(R, 7^3)
   b = 7^2 + 3*7^3 + O(R, 7^5)
   c = O(R, 7^3)
   d = R(2)

   @test a + 2 == 3 + 2*7^1 + 4*7^2 + O(R, 7^3)

   @test 3 - b == 3 + 6*7^2 + 3*7^3 + 6*7^4 + O(R, 7^5)

   @test a*fmpz(5) == 5 + 3*7^1 + O(R, 7^3)

   @test fmpz(3)*c == O(R, 7^3)

   @test 2*d == 4

   @test 2 + d == 4

   @test iszero(d - fmpz(2))

   @test a + fmpz(1)//7^2 == fmpz(1)//7^2 + 1 + 2*7^1 + 4*7^2 + O(R, 7^3)

   @test (fmpz(12)//11)*b == 3*7^2 + 3*7^3 + O(R, 7^5)

   @test c*(fmpz(1)//7) == O(R, 7^2)

   println("PASS")
end

function test_padic_comparison()
   print("padic.comparison()...")  

   R = PadicField(7, 30)

   a = 1 + 2*7 + 4*7^2 + O(R, 7^3)
   b = 3*7^3 + O(R, 7^5)
   c = O(R, 7^3)
   d = R(2)

   @test a == 1 + 2*7 + O(R, 7^2)

   @test b == c

   @test c == R(0)

   @test d == R(2)

   println("PASS")
end

function test_padic_adhoc_comparison()
   print("padic.adhoc_comparison()...")  

   R = PadicField(7, 30)

   a = 1 + O(R, 7^3)
   b = O(R, 7^5)
   c = R(2)

   @test a == 1

   @test b == ZZ(0)

   @test c == 2

   @test fmpz(2) == c

   @test a == fmpz(344)//1

   println("PASS")
end

function test_padic_powering()
   print("padic.powering()...")  

   R = PadicField(7, 30)

   a = 1 + 7 + 2*7^2 + O(R, 7^3)
   b = O(R, 7^5)
   c = R(2)

   @test a^5 == 1 + 5*7^1 + 6*7^2 + O(R, 7^3)

   @test b^3 == O(R, 7^5)

   @test c^7 == 2 + 4*7^1 + 2*7^2

   println("PASS")
end

function test_padic_inversion()
   print("padic.inversion()...")  

   R = PadicField(7, 30)

   a = 1 + 7 + 2*7^2 + O(R, 7^3)
   b = 2 + 3*7 + O(R, 7^5)
   c = 7^2 + 2*7^3 + O(R, 7^4)
   d = 7 + 2*7^2 + O(R, 7^5)

   @test inv(a) == 1 + 6*7^1 + 5*7^2 + O(R, 7^3)

   @test inv(b) == 4 + 4*7^1 + 3*7^2 + 1*7^3 + 1*7^4 + O(R, 7^5)

   @test inv(c) == fmpz(1)//7^2 + fmpz(5)//7 + O(R, 7^0)

   @test inv(d) == fmpz(1)//7 + 5 + 3*7^1 + 6*7^2 + O(R, 7^3)

   @test inv(R(1)) == 1

   println("PASS")
end

function test_padic_exact_division()
   print("padic.exact_division()...")  

   R = PadicField(7, 30)

   a = 1 + 7 + 2*7^2 + O(R, 7^3)
   b = 2 + 3*7 + O(R, 7^5)
   c = 7^2 + 2*7^3 + O(R, 7^4)
   d = 7 + 2*7^2 + O(R, 7^5)

   @test divexact(a, b) == 4 + 1*7^1 + 2*7^2 + O(R, 7^3)

   @test divexact(c, d) == 1*7^1 + O(R, 7^3)

   @test divexact(d, R(7^3)) == fmpz(1)//7^2 + fmpz(2)//7 + O(R, 7^2)

   @test divexact(R(34), R(17)) == 2

   println("PASS")
end

function test_padic_adhoc_exact_division()
   print("padic.adhoc_exact_division()...")  

   R = PadicField(7, 30)

   a = 1 + 7 + 2*7^2 + O(R, 7^3)
   b = 2 + 3*7 + O(R, 7^5)
   c = 7^2 + 2*7^3 + O(R, 7^4)
   d = 7 + 2*7^2 + O(R, 7^5)

   @test divexact(a, 2) == 4 + 1*7^2 + O(R, 7^3)

   @test divexact(b, fmpz(7)) == fmpz(2)//7 + 3 + O(R, 7^4)

   @test divexact(c, fmpz(12)//7^2) == 3*7^4 + 5*7^5 + O(R, 7^6)

   @test divexact(2, d) == fmpz(2)//7 + 3 + 6*7^2 + O(R, 7^3)

   @test divexact(R(3), 3) == 1

   @test divexact(fmpz(5)//7, R(5)) == fmpz(1)//7

   println("PASS")
end

function test_padic_divides()
   print("padic.divides()...")  

   R = PadicField(7, 30)

   a = 1 + 7 + 2*7^2 + O(R, 7^3)
   b = 2 + 3*7 + O(R, 7^5)
   
   flag, q = divides(a, b)

   @test flag
   @test q == divexact(a, b)

   println("PASS")
end

function test_padic_gcd()
   print("padic.adhoc_gcd()...")  

   R = PadicField(7, 30)

   a = 1 + 7 + 2*7^2 + O(R, 7^3)
   b = 2 + 3*7 + O(R, 7^5)
   
   @test gcd(a, b) == 1

   @test gcd(zero(R), zero(R)) == 0
   
   println("PASS")
end

function test_padic_square_root()
   print("padic.square_root()...")  

   R = PadicField(7, 30)

   a = 1 + 7 + 2*7^2 + O(R, 7^3)
   b = 2 + 3*7 + O(R, 7^5)
   c = 7^2 + 2*7^3 + O(R, 7^4)

   @test sqrt(a) == 1 + 4*7^1 + 3*7^2 + O(R, 7^3)

   @test sqrt(b) == 3 + 5*7^1 + 1*7^2 + 1*7^3 + O(R, 7^5)

   @test sqrt(c) == 1*7^1 + 1*7^2 + O(R, 7^3)

   @test sqrt(R(121)) == 3 + 5*7^1 + 6*7^2 + 6*7^3 + 6*7^4 + 6*7^5 + O(R, 7^6)

   println("PASS")
end

function test_padic_special_functions()
   print("padic.special_functions()...")  

   R = PadicField(7, 30)

   a = 1 + 7 + 2*7^2 + O(R, 7^3)
   b = 2 + 5*7 + 3*7^2 + O(R, 7^3)
   c = 3*7 + 2*7^2 + O(R, 7^5)

   @test exp(c) == 1 + 3*7^1 + 3*7^2 + 4*7^3 + 4*7^4 + O(R, 7^5)

   @test log(a) == 1*7^1 + 5*7^2 + O(R, 7^3)

   @test exp(R(0)) == 1

   @test log(R(1)) == 0

   @test teichmuller(b) == 2 + 4*7^1 + 6*7^2 + O(R, 7^3)

   println("PASS")
end

function test_padic()
   test_padic_constructors()
   test_padic_printing()
   test_padic_manipulation()
   test_padic_unary_ops()
   test_padic_binary_ops()
   test_padic_adhoc_binary()
   test_padic_comparison()
   test_padic_adhoc_comparison()
   test_padic_powering()
   test_padic_inversion()
   test_padic_exact_division()
   test_padic_adhoc_exact_division()
   test_padic_divides()
   test_padic_gcd()
   test_padic_square_root()
   test_padic_special_functions()

   println("")
end
