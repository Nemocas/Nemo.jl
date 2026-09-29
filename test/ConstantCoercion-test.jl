@testset "Constant coercion" begin
  parents = [
    ZZ, QQ,
    Native.GF(5), Native.GF(ZZ(5)),
    finite_field(5, 2)[1],
    Native.finite_field(5, 2, "a")[1], Native.finite_field(ZZ(5), 2, "a")[1],
    residue_ring(ZZ, 7)[1], residue_ring(ZZ, ZZ(7))[1],
    algebraic_closure(QQ),
  ]

  for R in parents
    @testset "$R" begin
      Rx, x = R["x"]
      @test R(Rx(3)) == R(3)
      @test R(zero(Rx)) == zero(R)
      @test_throws InexactError R(x)

      Rxy, (y, z) = polynomial_ring(R, [:y, :z])
      @test R(Rxy(3)) == R(3)
      @test R(zero(Rxy)) == zero(R)
      @test_throws InexactError R(y)

      S = universal_polynomial_ring(R)
      t = gen(S, "t")
      @test R(S(3)) == R(3)
      @test R(zero(S)) == zero(R)
      @test_throws InexactError R(t)

      K = fraction_field(Rx)
      @test R(K(6) // K(2)) == R(3)
      @test R(zero(K)) == zero(R)
      @test_throws InexactError R(x // K(2))
      @test_throws InexactError R(K(2) // x)

      R isa Field || continue
      L, s = rational_function_field(R, "s")
      @test R(L(6) // L(2)) == R(3)
      @test R(zero(L)) == zero(R)
      @test_throws InexactError R(s // 2)
      @test_throws InexactError R(1 // s)
    end
  end

  # coefficients must already lie in the target ring
  K = fraction_field(QQ["x"][1])
  @test QQ(K(3) // K(2)) == 3 // 2
  @test QQ(rational_function_field(QQ, "s")[1](3 // 2)) == 3 // 2
  @test_throws MethodError ZZ(QQ["x"][1](3))

  # `F(u)` reduces `u` modulo the defining polynomial; a fraction must not
  F, o = finite_field(5, 2)
  ku, u = prime_field(F)["u"]
  @test F(u) == o
  L = fraction_field(ku)
  @test F(L(3) // L(2)) == F(3) // F(2)
  @test_throws InexactError F(L(u) // L(1))
  L, v = rational_function_field(prime_field(F), "v")
  @test F(L(3) // L(2)) == F(3) // F(2)
  @test_throws InexactError F(v // 1)
end
