@testset "fqPolyRepFieldElem.constructors" begin
   R, x = finite_field(7, 5, "x")

   @test elem_type(R) == fqPolyRepFieldElem
   @test elem_type(fqPolyRepField) == fqPolyRepFieldElem
   @test parent_type(fqPolyRepFieldElem) == fqPolyRepField

   Sy, y = polynomial_ring(residue_ring(FlintZZ, 3), "y")
   Syy, yy = GF(3)["y"]

   T, z = finite_field(y^2 + 1, "z")
   T2, z2 = finite_field(yy^2 + 1, "z")

   # check that one can leave out the name for the generator, or specify it as a symbol
   @test finite_field(7, 5)[1] isa fqPolyRepField
   @test finite_field(7, 5, :x)[1] isa fqPolyRepField
   @test finite_field(y^2 + 1)[1] isa fqPolyRepField
   @test finite_field(y^2 + 1, :x)[1] isa fqPolyRepField

   @test isa(R, fqPolyRepField)
   @test isa(T, fqPolyRepField)
   @test isa(T2, fqPolyRepField)

   @test isa(3x^4 + 2x^3 + 4x^2 + x + 1, fqPolyRepFieldElem)
   @test isa(z^2 + z + 1, fqPolyRepFieldElem)
   @test isa(z2^2 + z2 + 1, fqPolyRepFieldElem)

   a = R()

   @test isa(a, fqPolyRepFieldElem)

   b = R(4)
   c = R(ZZRingElem(7))

   @test isa(b, fqPolyRepFieldElem)

   @test isa(c, fqPolyRepFieldElem)

   d = R(c)

   @test isa(d, fqPolyRepFieldElem)

   # check for primality
   T3, z3 = finite_field(yy^2 + 1, "z", check=false)
   @test isa(T2, fqPolyRepField)
   Syyy, yyy = polynomial_ring(residue_ring(FlintZZ, 4), "y")
   @test yyy isa zzModPolyRingElem
   @test_throws DomainError finite_field(yyy^2+1, "z")
end

@testset "fqPolyRepFieldElem.rand" begin
   R, x = finite_field(7, 5, "x")

   test_rand(R)
   test_rand(R, 1:9)
   test_rand(R, Int16(1):Int16(9))
   test_rand(R, big(1):big(9))
   test_rand(R, ZZRingElem(1):ZZRingElem(9))
   test_rand(R, [3,9,2])
   test_rand(R, Int16[3,9,2])
   test_rand(R, BigInt[3,9,2])
   test_rand(R, ZZRingElem[3,9,2])
end

@testset "fqPolyRepFieldElem.unsafe_coeffs" begin
   R, a = finite_field(23, 2, "a")

   b = R()

   @test Nemo.setindex_raw!(b, UInt(0), 0) == 0
   @test Nemo.setindex_raw!(b, UInt(0), 1) == 0
   @test Nemo.setindex_raw!(b, UInt(2), 1) == 2a
   @test Nemo.setindex_raw!(b, UInt(3), 0) == 2a + 3

   b = 2a + 3

   @test Nemo.setindex_raw!(b, UInt(0), 1) == 3
   @test Nemo.setindex_raw!(b, UInt(4), 0) == 4
   @test Nemo.setindex_raw!(b, UInt(0), 0) == 0

   @test fqPolyRepFieldElem(R, UInt[0, 0]) == 0
   @test fqPolyRepFieldElem(R, UInt[2, 0]) == 2
   @test fqPolyRepFieldElem(R, UInt[2, 3]) == 3a + 2

   @test coeffs_raw(2a + 3) == UInt[3, 2]
   @test coeffs_raw(R(3)) == UInt[3, 0]
   @test coeffs_raw(R()) == UInt[0, 0]
end

@testset "fqPolyRepFieldElem.printing" begin
   R, x = finite_field(7, 5, "x")

   a = 3x^4 + 2x^3 + 4x^2 + x + 1

   @test sprint(show, "text/plain", a) == "3*x^4 + 2*x^3 + 4*x^2 + x + 1"
end

@testset "fqPolyRepFieldElem.manipulation" begin
   R, x = finite_field(7, 5, "x")

   @test iszero(zero(R))

   @test isone(one(R))

   @test is_gen(gen(R))

   @test characteristic(R) == 7

   @test order(R) == ZZ(7)^5

   @test degree(R) == 5

   @test is_unit(x + 1)

   @test deepcopy(x + 1) == x + 1

   @test coeff(2x + 1, 1) == 2

   @test_throws DomainError coeff(2x + 1, -1)

   @test isa(modulus(R), fpPolyRingElem)

   #@test defining_polynomial(R) isa fpPolyRingElem
   #kt, t = GF(7)["t"]
   #@test parent(defining_polynomial(kt, R)) === kt
end

@testset "fqPolyRepFieldElem.unary_ops" begin
   R, x = finite_field(7, 5, "x")

   a = x^4 + 3x^2 + 6x + 1

   @test -a == 6*x^4+4*x^2+x+6
end

@testset "fqPolyRepFieldElem.binary_ops" begin
   R, x = finite_field(7, 5, "x")

   a = x^4 + 3x^2 + 6x + 1
   b = 3x^4 + 2x^2 + x + 1

   @test a + b == 4*x^4+5*x^2+2

   @test a - b == 5*x^4+x^2+5*x

   @test a*b == 3*x^3+2
end

@testset "fqPolyRepFieldElem.adhoc_binary" begin
   R, x = finite_field(7, 5, "x")

   a = x^4 + 3x^2 + 6x + 1

   @test 3a == 3*x^4+2*x^2+4*x+3

   @test a*3 == 3*x^4+2*x^2+4*x+3

   @test a*ZZRingElem(5) == 5*x^4+x^2+2*x+5

   @test ZZRingElem(5)*a == 5*x^4+x^2+2*x+5

   @test 12345678901234567890123*a == 3*x^4+2*x^2+4*x+3

   @test a*12345678901234567890123 == 3*x^4+2*x^2+4*x+3
end

@testset "fqPolyRepFieldElem.powering" begin
   R, x = finite_field(7, 5, "x")

   a = x^4 + 3x^2 + 6x + 1

   @test a^3 == x^4+6*x^3+5*x^2+5*x+6

   @test a^ZZRingElem(-5) == x^4+4*x^3+6*x^2+6*x+2
end

@testset "fqPolyRepFieldElem.comparison" begin
   R, x = finite_field(7, 5, "x")

   a = x^4 + 3x^2 + 6x + 1
   b = 3x^4 + 2x^2 + 2

   @test b != a
   @test R(3) == R(3)
   @test isequal(R(3), R(3))
end

@testset "fqPolyRepFieldElem.inversion" begin
   R, x = finite_field(7, 5, "x")

   a = x^4 + 3x^2 + 6x + 1

   b = inv(a)

   @test b == x^4+5*x^3+4*x^2+5*x

   @test b == a^-1
end

@testset "fqPolyRepFieldElem.exact_division" begin
   R, x = finite_field(7, 5, "x")

   a = x^4 + 3x^2 + 6x + 1
   b = 3x^4 + 2x^2 + 2

   @test divexact(a, b) == 3*x^4+2*x^3+2*x^2+5*x

   @test b//a == 4*x^2+6*x+5
end

@testset "fqPolyRepFieldElem.gcd" begin
   R, x = finite_field(7, 5, "x")

   a = x^4 + 3x^2 + 6x + 1
   b = 3x^4 + 2x^2 + x + 1

   @test gcd(a, b) == 1

   @test gcd(R(0), R(0)) == 0
end

@testset "fqPolyRepFieldElem.special_functions" begin
   R, x = finite_field(7, 5, "x")

   a = x^4 + 3x^2 + 6x + 1

   @test tr(a) == 1

   @test norm(a) == 4

   @test frobenius(a) == x^4+2*x^3+3*x^2+5*x+1

   @test frobenius(a, 3) == 3*x^4+3*x^3+3*x^2+x+4

   @test pth_root(a) == 4*x^4+3*x^3+4*x^2+5*x+2

   @test is_square(a^2)

   @test sqrt(a^2)^2 == a^2

   @test is_square_with_sqrt(a^2)[1]

   @test is_square_with_sqrt(a^2)[2]^2 == a^2

   @test !is_square(x*a^2)

   @test_throws ErrorException sqrt(x*a^2)

   @test !is_square_with_sqrt(x*a^2)[1]
end

@testset "fqPolyRepFieldElem.iteration" begin
   for n = [2, 3, 5, 13, 31]
      R, _ = finite_field(n, 1, "x")
      elts = Nemo.AbstractAlgebra.test_iterate(R)
      @test elts == R.(0:n-1)
      R, _ = finite_field(n, rand(2:9), "x")
      Nemo.AbstractAlgebra.test_iterate(R)
   end
end

@testset "fqPolyRepFieldElem.lift" begin
   R, x = finite_field(23, 2, "x")
   f = 8x + 9
   S, y = polynomial_ring(GF(23), "y")
   @test lift(S, f) == 8y + 9
end

@testset "fqPolyRepField.overload" begin
   R, x = finite_field(19, 3, "x")
   @test R([1, 0, 1]) == x^2 + 1
end

@testset "Nemo.jl#1493" begin
   @test_throws DomainError GF(4, 2)
   @test_throws DomainError finite_field(6, 2, "x")
end
