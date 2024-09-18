@testset "FqFieldElem.constructors" begin
  R, x = finite_field(ZZRingElem(7), 5, "x")

  @test elem_type(R) == FqFieldElem
  @test elem_type(FqField) == FqFieldElem
  @test parent_type(FqFieldElem) == FqField

  Sy, y = polynomial_ring(residue_ring(ZZ, 36893488147419103363)[1], "y")
  Syy, yy = polynomial_ring(Native.GF(ZZRingElem(36893488147419103363)), "y")
  St, t = polynomial_ring(residue_ring(ZZ, 23)[1], "t")
  Stt, tt = polynomial_ring(Native.GF(23), "y")

  T = FqField(y^2 + 1, :z)
  z = gen(T)
  T2 = FqField(yy^2 + 1, :z)
  z2 = gen(T2)
  T3 = FqField(t^2 + 1, :z)
  z3 = gen(T3)
  T4 = FqField(tt^2 + 1, :z)
  z4 = gen(T4)

  @test isa(R, FqField)
  @test isa(T, FqField)
  @test isa(T2, FqField)
  @test isa(T3, FqField)
  @test isa(T4, FqField)

  @test characteristic(R) == ZZRingElem(7)
  @test characteristic(T) == ZZRingElem(36893488147419103363)
  @test characteristic(T2) == ZZRingElem(36893488147419103363)
  @test characteristic(T3) == 23
  @test characteristic(T4) == 23

  @test isa(3x^4 + 2x^3 + 4x^2 + x + 1, FqFieldElem)
  @test isa(z^2 + z + 1, FqFieldElem)
  @test isa(z2^2 + z2 + 1, FqFieldElem)
  @test isa(z3^2 + z3 + 1, FqFieldElem)
  @test isa(z4^2 + z4 + 1, FqFieldElem)

  a = R()

  @test isa(a, FqFieldElem)

  b = R(4)
  c = R(ZZRingElem(7))

  @test isa(b, FqFieldElem)

  @test isa(c, FqFieldElem)

  d = R(c)

  @test isa(d, FqFieldElem)

  # check for primality
  T3, z3 = FqField(yy^2 + 1, :z, check=false)
  @test isa(T2, FqField)
  Syyy, yyy = polynomial_ring(residue_ring(ZZ, ZZ(4))[1], "y")
  @test yyy isa ZZModPolyRingElem
  @test_throws DomainError FqField(yyy^2+1, :z)
end

@testset "FqFieldElem.printing" begin
  R, x = finite_field(ZZRingElem(7), 5, "x")

  a = 3x^4 + 2x^3 + 4x^2 + x + 1

  @test sprint(show, "text/plain", a) == "3*x^4 + 2*x^3 + 4*x^2 + x + 1"
end

@testset "FqFieldElem.manipulation" begin
  R, x = finite_field(ZZRingElem(7), 5, "x")

  @test iszero(zero(R))

  @test isone(one(R))

  @test is_gen(gen(R))

  @test characteristic(R) == 7

  @test order(R) == ZZRingElem(7)^5

  @test degree(R) == 5

  @test is_unit(x + 1)

  @test deepcopy(x + 1) == x + 1

  @test (@inferred coeff(2x + 1, 1)) == 2

  @test_throws DomainError coeff(2x + 1, -1)
end

@testset "FqFieldElem.conversions" begin
  U, a = finite_field(ZZRingElem(7), 5, "a")

  for T in [Int, Int128, BigInt, ZZRingElem]
    @test isone(U(T(1)))
  end

  for T in [Int, Int128, BigInt, ZZRingElem]
    @test (U(1//T(2))) == inv(U(2))
  end

  @test_throws ErrorException U(1//7)

  f = 3a^4 + 2a^3 + a + 5

  for R in [residue_ring(ZZ, 7)[1], residue_ring(ZZ, ZZ(7))[1], Native.GF(7), Native.GF(ZZ(7))]
    S, y = polynomial_ring(R, "y")

    @test f == U(S(f))
  end

  S, y = polynomial_ring(ZZ, "y")

  @test f == U(lift(S, f))

  U, a = finite_field(ZZ(7), 5, "a")
  f = 3a^4 + 2a^3 + a + 5
  S, y = Nemo.GF(7)["y"]
  @test f == U(3y^4 + 2y^3 + y + 5)
  S, y = base_field(U)["y"]
  @test lift(S, f) == 3y^4 + 2y^3 + y + 5

  S, y = Nemo.GF(5)["y"]
  @test_throws ErrorException U(y)

  U, a = finite_field(ZZ(1180591620717411303449), 5, "a")
  f = 3a^4 + 2a^3 + a + 5
  S, y = Nemo.GF(ZZ(1180591620717411303449))["y"]
  @test f == U(3y^4 + 2y^3 + y + 5)
  S, y = base_field(U)["y"]
  @test lift(S, f) == 3y^4 + 2y^3 + y + 5

  S, y = Nemo.GF(5)["y"]
  @test_throws ErrorException U(y)
end

@testset "FqFieldElem.unary_ops" begin
  R, x = finite_field(ZZRingElem(7), 5, "x")

  a = x^4 + 3x^2 + 6x + 1

  @test -a == 6*x^4+4*x^2+x+6
end

@testset "FqFieldElem.binary_ops" begin
  R, x = finite_field(ZZRingElem(7), 5, "x")

  a = x^4 + 3x^2 + 6x + 1
  b = 3x^4 + 2x^2 + x + 1

  @test a + b == 4*x^4+5*x^2+2

  @test a - b == 5*x^4+x^2+5*x

  @test a*b == 3*x^3+2
end

@testset "FqFieldElem.adhoc_binary" begin
  R, x = finite_field(ZZRingElem(7), 5, "x")

  a = x^4 + 3x^2 + 6x + 1

  @test 3a == 3*x^4+2*x^2+4*x+3

  @test a*3 == 3*x^4+2*x^2+4*x+3

  @test a*ZZRingElem(5) == 5*x^4+x^2+2*x+5

  @test ZZRingElem(5)*a == 5*x^4+x^2+2*x+5

  @test 12345678901234567890123*a == 3*x^4+2*x^2+4*x+3

  @test a*12345678901234567890123 == 3*x^4+2*x^2+4*x+3
end

@testset "FqFieldElem.powering" begin
  R, x = finite_field(ZZRingElem(7), 5, "x")

  a = x^4 + 3x^2 + 6x + 1

  @test a^3 == x^4+6*x^3+5*x^2+5*x+6

  @test a^ZZRingElem(-5) == x^4+4*x^3+6*x^2+6*x+2
end

@testset "FqFieldElem.comparison" begin
  R, x = finite_field(ZZRingElem(7), 5, "x")

  a = x^4 + 3x^2 + 6x + 1
  b = 3x^4 + 2x^2 + 2

  @test b != a
  @test R(3) == R(3)
  @test isequal(R(3), R(3))
end

@testset "FqFieldElem.inversion" begin
  R, x = finite_field(ZZRingElem(7), 5, "x")

  a = x^4 + 3x^2 + 6x + 1

  b = inv(a)

  @test b == x^4+5*x^3+4*x^2+5*x

  @test b == a^-1
end

@testset "FqFieldElem.exact_division" begin
  R, x = finite_field(ZZRingElem(7), 5, "x")

  a = x^4 + 3x^2 + 6x + 1
  b = 3x^4 + 2x^2 + 2

  @test divexact(a, b) == 3*x^4+2*x^3+2*x^2+5*x

  @test b//a == 4*x^2+6*x+5
end

@testset "FqFieldElem.gcd" begin
  R, x = finite_field(ZZRingElem(7), 5, "x")

  a = x^4 + 3x^2 + 6x + 1
  b = 3x^4 + 2x^2 + x + 1

  @test gcd(a, b) == 1

  @test gcd(R(0), R(0)) == 0
end

@testset "FqFieldElem.special_functions" begin
  R, x = finite_field(ZZRingElem(7), 5, "x")

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

@testset "FqFieldElem.rand" begin
  R, x = finite_field(ZZRingElem(17), 3, "x")

  test_rand(R)
end

@testset "FqFieldElem.iteration" begin
  for n = [2, 3, 5, 13, 31]
    R, _ = finite_field(ZZRingElem(n), 1, "x")
    elts = Nemo.AbstractAlgebra.test_iterate(R)
    @test elts == R.(0:n-1)
    R, _ = finite_field(ZZRingElem(n), rand(2:9), "x")
    Nemo.AbstractAlgebra.test_iterate(R)
  end
end

@testset "conversions" begin
  F1, = finite_field(7, 1)
  F2, = finite_field(ZZ(18446744073709551629), 1)
  F3, = finite_field(7, 10) # avoid zech
  F4, = finite_field(ZZ(18446744073709551629), 4)
  fields = [F1, F2, F3, F4]
  types = [Nemo.fpField, Nemo.FpField, fqPolyRepField, FqPolyRepField]
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

  @test_throws AssertionError Nemo.canonical_raw_type(Nemo.FpField, F1)

  F1, = finite_field(7, 1)
  F1w, = finite_field(2, 1)
  f = Nemo.canonical_raw_type(Nemo.fpField, F1)
  @test_throws AssertionError f(rand(F1w))
  @test_throws AssertionError f(rand(F2))
  @test_throws AssertionError f(rand(F3))
  @test_throws AssertionError f(rand(F4))
end

@testset "residue field" begin
  R, f = residue_field(ZZ, 2)
  @test R isa FqField
  @test domain(f) === ZZ
  @test codomain(f) === R
  @test f(ZZ(1)) == R(1)
  x = preimage(f, R(1))
  @test x isa ZZRingElem && x == 1

  R, f = residue_field(ZZ, ZZ(2))
  @test R isa FqField
  @test domain(f) === ZZ
  @test codomain(f) === R
  @test f(ZZ(1)) == R(1)
  x = preimage(f, R(1))
  @test x isa ZZRingElem && x == 1

end

@testset "FqFieldElem.is_power" begin
  F = GF(11)
  a = F(0)
  @test is_power(a, 2) == (true, F(0))
  a = F(2)
  fl, b = is_power(a^3, 3)
  @test fl
  @test a^3 == b^3
  fl, b = is_power(a^2, 2)
  @test fl
  @test a^2 == b^2
  fl, b = is_power(a, 2)
  @test !fl
end
