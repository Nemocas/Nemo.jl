@testset "fq_default.constructors" begin
   R, x = NGFiniteField(fmpz(7), 5, "x")

   @test elem_type(R) == fq_default
   @test elem_type(FqDefaultFiniteField) == fq_default
   @test parent_type(fq_default) == FqDefaultFiniteField

   Sy, y = PolynomialRing(ResidueRing(FlintZZ, 36893488147419103363), "y")
   Syy, yy = PolynomialRing(GF(fmpz(36893488147419103363)), "y")
   St, t = PolynomialRing(ResidueRing(FlintZZ, 23), "t")
   Stt, tt = PolynomialRing(GF(23), "y")

   T, z = NGFiniteField(y^2 + 1, "z")
   T2, z2 = NGFiniteField(yy^2 + 1, "z")
   T3, z3 = NGFiniteField(t^2 + 1, "z")
   T4, z4 = NGFiniteField(tt^2 + 1, "z")

   @test isa(R, FqDefaultFiniteField)
   @test isa(T, FqDefaultFiniteField)
   @test isa(T2, FqDefaultFiniteField)
   @test isa(T3, FqDefaultFiniteField)
   @test isa(T4, FqDefaultFiniteField)

   @test characteristic(R) == fmpz(7)
   @test characteristic(T) == fmpz(36893488147419103363)
   @test characteristic(T2) == fmpz(36893488147419103363)
   @test characteristic(T3) == 23
   @test characteristic(T4) == 23

   @test isa(3x^4 + 2x^3 + 4x^2 + x + 1, fq_default)
   @test isa(z^2 + z + 1, fq_default)
   @test isa(z2^2 + z2 + 1, fq_default)
   @test isa(z3^2 + z3 + 1, fq_default)
   @test isa(z4^2 + z4 + 1, fq_default)

   a = R()

   @test isa(a, fq_default)

   b = R(4)
   c = R(fmpz(7))

   @test isa(b, fq_default)

   @test isa(c, fq_default)

   d = R(c)

   @test isa(d, fq_default)

   # check for primality
   T3, z3 = NGFiniteField(yy^2 + 1, "z", check=false)
   @test isa(T2, FqDefaultFiniteField)
   Syyy, yyy = PolynomialRing(ResidueRing(FlintZZ, ZZ(4)), "y")
   @test yyy isa fmpz_mod_poly
   @test_throws DomainError NGFiniteField(yyy^2+1, "z")
end

@testset "fq_default.printing" begin
   R, x = NGFiniteField(fmpz(7), 5, "x")

   a = 3x^4 + 2x^3 + 4x^2 + x + 1

   @test sprint(show, "text/plain", a) == "3*x^4 + 2*x^3 + 4*x^2 + x + 1"
end

@testset "fq_default.manipulation" begin
   R, x = NGFiniteField(fmpz(7), 5, "x")

   @test iszero(zero(R))

   @test isone(one(R))

   @test is_gen(gen(R))

   @test characteristic(R) == 7

   @test order(R) == fmpz(7)^5

   @test degree(R) == 5

   @test is_unit(x + 1)

   @test deepcopy(x + 1) == x + 1

   @test coeff(2x + 1, 1) == 2

   @test_throws DomainError coeff(2x + 1, -1)
end

@testset "fq_default.conversions" begin
   U, a = NGFiniteField(fmpz(7), 5, "a")

   f = 3a^4 + 2a^3 + a + 5

   for R in [ResidueRing(FlintZZ, 7), ResidueRing(FlintZZ, ZZ(7)), GF(7), GF(ZZ(7))]
      S, y = PolynomialRing(R, "y")

      @test f == U(S(f))
   end

   S, y = PolynomialRing(ZZ, "y")

   @test f == U(lift(S, f))
end

@testset "fq_default.unary_ops" begin
   R, x = NGFiniteField(fmpz(7), 5, "x")

   a = x^4 + 3x^2 + 6x + 1

   @test -a == 6*x^4+4*x^2+x+6
end

@testset "fq_default.binary_ops" begin
   R, x = NGFiniteField(fmpz(7), 5, "x")

   a = x^4 + 3x^2 + 6x + 1
   b = 3x^4 + 2x^2 + x + 1

   @test a + b == 4*x^4+5*x^2+2

   @test a - b == 5*x^4+x^2+5*x

   @test a*b == 3*x^3+2
end

@testset "fq_default.adhoc_binary" begin
   R, x = NGFiniteField(fmpz(7), 5, "x")

   a = x^4 + 3x^2 + 6x + 1

   @test 3a == 3*x^4+2*x^2+4*x+3

   @test a*3 == 3*x^4+2*x^2+4*x+3

   @test a*fmpz(5) == 5*x^4+x^2+2*x+5

   @test fmpz(5)*a == 5*x^4+x^2+2*x+5

   @test 12345678901234567890123*a == 3*x^4+2*x^2+4*x+3

   @test a*12345678901234567890123 == 3*x^4+2*x^2+4*x+3
end

@testset "fq_default.powering" begin
   R, x = NGFiniteField(fmpz(7), 5, "x")

   a = x^4 + 3x^2 + 6x + 1

   @test a^3 == x^4+6*x^3+5*x^2+5*x+6

   @test a^fmpz(-5) == x^4+4*x^3+6*x^2+6*x+2
end

@testset "fq_default.comparison" begin
   R, x = NGFiniteField(fmpz(7), 5, "x")

   a = x^4 + 3x^2 + 6x + 1
   b = 3x^4 + 2x^2 + 2

   @test b != a
   @test R(3) == R(3)
   @test isequal(R(3), R(3))
end

@testset "fq_default.inversion" begin
   R, x = NGFiniteField(fmpz(7), 5, "x")

   a = x^4 + 3x^2 + 6x + 1

   b = inv(a)

   @test b == x^4+5*x^3+4*x^2+5*x

   @test b == a^-1
end

@testset "fq_default.exact_division" begin
   R, x = NGFiniteField(fmpz(7), 5, "x")

   a = x^4 + 3x^2 + 6x + 1
   b = 3x^4 + 2x^2 + 2

   @test divexact(a, b) == 3*x^4+2*x^3+2*x^2+5*x

   @test b//a == 4*x^2+6*x+5
end

@testset "fq_default.gcd" begin
   R, x = NGFiniteField(fmpz(7), 5, "x")

   a = x^4 + 3x^2 + 6x + 1
   b = 3x^4 + 2x^2 + x + 1

   @test gcd(a, b) == 1

   @test gcd(R(0), R(0)) == 0
end

@testset "fq_default.special_functions" begin
   R, x = NGFiniteField(fmpz(7), 5, "x")

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

@testset "fq_default.rand" begin
   R, x = NGFiniteField(fmpz(17), 3, "x")

   test_rand(R)
end

@testset "fq_default.iteration" begin
   for n = [2, 3, 5, 13, 31]
      R, _ = NGFiniteField(fmpz(n), 1, "x")
      elts = Nemo.AbstractAlgebra.test_iterate(R)
      @test elts == R.(0:n-1)
      R, _ = NGFiniteField(fmpz(n), rand(2:9), "x")
      Nemo.AbstractAlgebra.test_iterate(R)
   end
end

@testset "conversions" begin
  F1, = NGFiniteField(7, 1)
  F2, = NGFiniteField(ZZ(18446744073709551629), 1)
  F3, = NGFiniteField(7, 10) # avoid zech
  F4, = NGFiniteField(ZZ(18446744073709551629), 4)
  fields = [F1, F2, F3, F4]
  types = [Nemo.GaloisField, Nemo.GaloisFmpzField, FqNmodFiniteField, FqFiniteField]
  for (F, T) in zip(fields, types)
    f = Nemo.canonical_raw_type(T, F)
    @test codomain(f) isa T
    @test domain(f) === F
    for i in 1:10
      a = rand(F)
      @test preimage(f, image(f, a)) == a
      @test preimage(f, f(a)) == a
      b = rand(F)
      @test image(f, a * b) == image(f, a) * image(f, b)
    end

    g = inv(f)
    FF = domain(g)
    @test codomain(g) === F
    @test FF isa T
    for i in 1:10
      a = rand(FF)
      @test preimage(g, image(g, a)) == a
      @test preimage(g, g(a)) == a
      b = rand(FF)
      @test image(g, a * b) == image(g, a) * image(g, b)
    end
  end

  @test_throws AssertionError Nemo.canonical_raw_type(Nemo.GaloisFmpzField, F1)

  F1, = NGFiniteField(7, 1)
  F1w, = NGFiniteField(2, 1)
  f = Nemo.canonical_raw_type(Nemo.GaloisField, F1)
  @test_throws AssertionError f(rand(F1w))
  @test_throws AssertionError f(rand(F2))
  @test_throws AssertionError f(rand(F3))
  @test_throws AssertionError f(rand(F4))
end
