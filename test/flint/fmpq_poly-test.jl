@testset "QQPolyRingElem.constructors" begin
  S1 = PolyRing(QQ)
  S2 = PolyRing(QQ)

  @test isa(S1, QQPolyRing)
  @test S1 !== S2

  S, y = polynomial_ring(QQ, "y")

  @test elem_type(S) == QQPolyRingElem
  @test elem_type(QQPolyRing) == QQPolyRingElem
  @test parent_type(QQPolyRingElem) == QQPolyRing
  @test dense_poly_type(QQFieldElem) == QQPolyRingElem

  @test isa(S, QQPolyRing)

  @test isa(y, PolyRingElem)

  T, z = polynomial_ring(S, "z")

  @test typeof(T) <: Generic.PolyRing

  @test isa(z, PolyRingElem)

  f = ZZRingElem(12)//3 + y^3 + z + 1

  @test isa(f, PolyRingElem)

  g = S(2)

  @test isa(g, PolyRingElem)

  h = S(ZZRingElem(12)//7 + 1)

  @test isa(h, PolyRingElem)

  j = T(ZZRingElem(12)//7 + 2)

  @test isa(j, PolyRingElem)

  k = S([ZZRingElem(12)//7, ZZRingElem(12)//7 + 2, ZZRingElem(3)//11 + 1])

  @test isa(k, PolyRingElem)

  l = S(k)

  @test isa(l, PolyRingElem)

  R, x = polynomial_ring(ZZ, "x")

  m = S(3x^3 + 2x + 1)

  @test isa(m, PolyRingElem)

  @test m == 3y^3 + 2y + 1

  n = S(ZZRingElem(12))

  @test isa(n, PolyRingElem)

  n2 = S(12//1)

  @test isa(n2, PolyRingElem)

  n2 = S(BigInt(12)//BigInt(1))

  @test isa(n2, PolyRingElem)

  o = S([1, 2, 3])

  @test isa(o, PolyRingElem)

  o2 = S([1//1, 2//1, 3//1])

  @test isa(o2, PolyRingElem)

  o3 = S([BigInt(1)//BigInt(1), BigInt(2)//BigInt(1), BigInt(3)//BigInt(1)])

  @test isa(o3, PolyRingElem)

  p = S([ZZ(1), ZZ(2), ZZ(3)])

  @test isa(p, PolyRingElem)

  @test polynomial_ring(QQField(), "x")[1] != polynomial_ring(QQField(), "y")[1]

  R = QQField()
  @test polynomial_ring(R, "x", cached = true)[1] === polynomial_ring(R, "x", cached = true)[1]
end

@testset "QQPolyRingElem.printing" begin
  S, y = polynomial_ring(QQ, "y")

  @test sprint(show, "text/plain", y + y^2) == "y^2 + y"
end

@testset "QQPolyRingElem.manipulation" begin
  S, y = polynomial_ring(QQ, "y")

  @test iszero(zero(S))

  @test isone(one(S))

  @test is_gen(gen(S))

  @test is_unit(one(S))

  f = 2y + ZZRingElem(11)//7 + 1

  @test leading_coefficient(f) == 2

  @test degree(f) == 1

  h = ZZRingElem(12)//7*y^2 + 5*y + 3

  @test coeff(h, 2) == ZZRingElem(12)//7

  @test_throws DomainError coeff(h, -1)

  @test length(h) == 3

  @test canonical_unit(-ZZRingElem(12)//7*y + 1) == ZZRingElem(-12)//7

  @test deepcopy(h) == h

  @test denominator(-ZZRingElem(12)//7*y + 1) == 7

  @test characteristic(S) == 0
end

@testset "QQPolyRingElem.polynomial" begin
  f = polynomial(QQ, [])
  g = polynomial(QQ, [1, 2, 3])
  h = polynomial(QQ, ZZRingElem[1, 2, 3])
  k = polynomial(QQ, QQFieldElem[1, 2, 3])
  p = polynomial(QQ, [1, 2, 3], "y")

  @test isa(f, QQPolyRingElem)
  @test isa(g, QQPolyRingElem)
  @test isa(h, QQPolyRingElem)
  @test isa(k, QQPolyRingElem)
  @test isa(p, QQPolyRingElem)

  q = polynomial(QQ, [1, 2, 3], cached=false)

  @test parent(g) !== parent(q)
end

@testset "QQPolyRingElem.similar" begin
  f = polynomial(QQ, [1, 2, 3])
  g = similar(f)
  h = similar(f, "y")

  @test isa(g, QQPolyRingElem)
  @test isa(h, QQPolyRingElem)

  q = similar(g, cached=false)

  @test parent(g) === parent(q)
end

@testset "QQPolyRingElem.binary_ops" begin
  S, y = polynomial_ring(QQ, "y")

  f = 3*y^2 + 7*y + 3
  g = 2*y + 11

  @test f - g == 3*y^2 + 5*y - 8

  @test f + g == 3*y^2 + 9*y + 14

  @test f*g == 6*y^3 + 47*y^2 + 83*y + 33
end

@testset "QQPolyRingElem.adhoc_binary" begin
  S, y = polynomial_ring(QQ, "y")

  f = 3*y^2 + 7*y + 3
  g = 2*y + 11

  @test f*4 == 12*y^2 + 28*y + 12

  @test 7*f == 21*y^2 + 49*y + 21

  @test ZZRingElem(5)*g == 10*y+55

  @test g*ZZRingElem(3) == 6*y+33

  @test QQFieldElem(5, 7)*g == ZZRingElem(10)//7*y+ZZRingElem(55)//7

  @test g*QQFieldElem(5, 7) == ZZRingElem(10)//7*y+ZZRingElem(55)//7

  @test (5//7)*g == ZZRingElem(10)//7*y+ZZRingElem(55)//7

  @test g*(5//7) == ZZRingElem(10)//7*y+ZZRingElem(55)//7

  @test (BigInt(5)//BigInt(7))*g == ZZRingElem(10)//7*y+ZZRingElem(55)//7

  @test g*(BigInt(5)//BigInt(7)) == ZZRingElem(10)//7*y+ZZRingElem(55)//7

  @test f + 4 == 3*y^2 + 7*y + 7

  @test 7 + f == 3*y^2 + 7*y + 10

  @test ZZRingElem(5) + g == 2*y+16

  @test g + ZZRingElem(3) == 2*y+14

  @test QQFieldElem(5, 7) + g == 2*y+ZZRingElem(82)//7

  @test g + (5//7) == 2*y+ZZRingElem(82)//7

  @test (5//7) + g == 2*y+ZZRingElem(82)//7

  @test g + (BigInt(5)//BigInt(7)) == 2*y+ZZRingElem(82)//7

  @test (BigInt(5)//BigInt(7)) + g == 2*y+ZZRingElem(82)//7

  @test g + (BigInt(5)//BigInt(7)) == 2*y+ZZRingElem(82)//7

  @test f - 4 == 3*y^2 + 7*y - 1

  @test 7 - f == -3*y^2 - 7*y + 4

  @test ZZRingElem(5) - g == -2*y-6

  @test g - ZZRingElem(3) == 2*y+8

  @test QQFieldElem(5, 7) - g == -2*y-ZZRingElem(72)//7

  @test g - QQFieldElem(5, 7) == 2*y+ZZRingElem(72)//7

  @test (5//7) - g == -2*y-ZZRingElem(72)//7

  @test g - (5//7) == 2*y+ZZRingElem(72)//7

  @test (BigInt(5)//BigInt(7)) - g == -2*y-ZZRingElem(72)//7

  @test g - (BigInt(5)//BigInt(7)) == 2*y+ZZRingElem(72)//7
end

@testset "QQPolyRingElem.comparison" begin
  S, y = polynomial_ring(QQ, "y")

  f = 3*y^2 + 7*y + 3
  g = 3*y^2 + 7*y + 3

  @test f == g

  @test isequal(f, g)
end

@testset "QQPolyRingElem.adhoc_comparison" begin
  S, y = polynomial_ring(QQ, "y")

  @test S(1) == 1

  @test S(1) == BigInt(1)

  @test S(1) == ZZRingElem(1)

  @test S(1) == QQFieldElem(1, 1)

  @test S(1) == 1//1

  @test S(1) == BigInt(1)//BigInt(1)

  @test 1 != ZZRingElem(11)//7 + y

  @test S(ZZRingElem(3)//5) == QQFieldElem(3, 5)

  @test QQFieldElem(3, 5) != y + 1

  @test (3//5) != y + 1

  @test BigInt(3)//BigInt(5) != y + 1
end

@testset "QQPolyRingElem.unary_ops" begin
  S, y = polynomial_ring(QQ, "y")

  f = 3*y^2 + 2*y + 3

  @test -f == -3*y^2 - 2*y - 3
end

@testset "QQPolyRingElem.truncation" begin
  S, y = polynomial_ring(QQ, "y")

  f = 3*y^2 + 7*y + 3
  g = 2*y^2 + 11*y + 1

  @test truncate(f, 1) == 3

  @test_throws DomainError truncate(f, -1)

  @test mullow(f, g, 4) == 47*y^3 + 86*y^2 + 40*y + 3

  @test_throws DomainError mullow(f, g, -1)
end

@testset "QQPolyRingElem.reverse" begin
  S, y = polynomial_ring(QQ, "y")

  f = 3*y^2 + 7*y + 3

  @test reverse(f, 7) == 3*y^6 + 7*y^5 + 3*y^4

  @test_throws DomainError reverse(f, -1)
end

@testset "QQPolyRingElem.shift" begin
  S, y = polynomial_ring(QQ, "y")

  f = 3*y^2 + 7*y + 3

  @test shift_left(f, 7) == 3*y^9 + 7*y^8 + 3*y^7

  @test_throws DomainError shift_left(f, -1)

  @test shift_right(f, 3) == 0

  @test_throws DomainError shift_right(f, -1)
end

@testset "QQPolyRingElem.powering" begin
  S, y = polynomial_ring(QQ, "y")

  f = 3*y^2 + 7*y + 3

  @test f^5 == 243*y^10 + 2835*y^9 + 14445*y^8 + 42210*y^7 + 78135*y^6 + 95557*y^5 + 78135*y^4 + 42210*y^3 + 14445*y^2 + 2835*y + 243
  @test f^ZZ(5) == 243*y^10 + 2835*y^9 + 14445*y^8 + 42210*y^7 + 78135*y^6 + 95557*y^5 + 78135*y^4 + 42210*y^3 + 14445*y^2 + 2835*y + 243

  @test_throws DomainError f^(-1)
end

@testset "QQPolyRingElem.modular_arithmetic" begin
  S, y = polynomial_ring(QQ, "y")

  f = 7y + 1
  g = 11y^2 + 12y + 21
  h = 17y^5 + 2y + 1

  @test invmod(f, g) == -ZZRingElem(77)//956*y-ZZRingElem(73)//956

  @test mulmod(f, g, h) == 77*y^3 + 95*y^2 + 159*y + 21

  @test powermod(f, 3, h) == 343*y^3 + 147*y^2 + 21*y + 1
end

@testset "QQPolyRingElem.exact_division" begin
  S, y = polynomial_ring(QQ, "y")

  f = 3*y^2 + 7*y + 3
  g = 11*y^2 + 2*y + 3

  @test divexact(f*g, f) == g
end

@testset "QQPolyRingElem.adhoc_exact_division" begin
  S, y = polynomial_ring(QQ, "y")

  f = 3*y^2 + 7*y + 3

  @test divexact(f, f) == one(S)

  @test_throws ArgumentError divexact(f, y)

  @test divexact(3*f, 3) == f

  @test divexact(ZZRingElem(3)*f, ZZRingElem(3)) == f

  @test divexact(ZZRingElem(12)//7*f, ZZRingElem(12)//7) == f

  @test divexact(ZZRingElem(12)//7*f, (12//7)) == f

  @test divexact(ZZRingElem(12)//7*f, BigInt(12)//BigInt(7)) == f
end

@testset "QQPolyRingElem.euclidean_division" begin
  S, y = polynomial_ring(QQ, "y")

  f = y^3 + 3*y^2 + 7*y + 3
  g = 11*y^2 + 2*y + 3

  @test mod(f, g) == ZZRingElem(752)//121*y+ZZRingElem(270)//121

  @test divrem(f, g) == (ZZRingElem(1)//11*y+ZZRingElem(31)//121, ZZRingElem(752)//121*y+ZZRingElem(270)//121)
end

@testset "QQPolyRingElem.content_primpart_gcd" begin
  S, y = polynomial_ring(QQ, "y")

  k = 3y^2 + 7y + 3
  l = 11y + 5
  m = y^2 + 17

  @test content(k) == 1

  @test primpart(k*ZZRingElem(13)//6) == k

  @test gcd(k*m, l*m) == m

  @test lcm(k*m, l*m) == k*l*m
end

@testset "QQPolyRingElem.evaluation" begin
  S, y = polynomial_ring(QQ, "y")

  f = ZZRingElem(12)//7
  g = 3y^2 + 11*y + 3

  @test evaluate(g, 3) == 63

  @test evaluate(g, ZZRingElem(3)) == 63

  @test evaluate(g, QQFieldElem(3, 1)) == 63

  @test evaluate(g, 3//1) == 63

  @test evaluate(g, BigInt(3)//BigInt(1)) == 63

  @test evaluate(g, f) == ZZRingElem(1503)//49

  @test g(3) == 63

  @test g(ZZRingElem(3)) == 63

  @test g(QQFieldElem(3, 1)) == 63

  @test g(3//1) == 63

  @test g(BigInt(3)//BigInt(1)) == 63

  @test g(f) == ZZRingElem(1503)//49
end

@testset "QQPolyRingElem.composition" begin
  S, y = polynomial_ring(QQ, "y")

  f = 7y^2 + 12y + 3
  g = 11y + 9

  @test compose(f, g; inner = :second) == 847*y^2 + 1518*y + 678
end

@testset "QQPolyRingElem.derivative" begin
  S, y = polynomial_ring(QQ, "y")

  h = 17y^2 + 2y + 3

  @test derivative(h) == 34y + 2
end

@testset "QQPolyRingElem.integral" begin
  S, y = polynomial_ring(QQ, "y")

  f = 17y^2 + 2y - 11

  @test integral(f) == ZZRingElem(17)//3*y^3 + y^2 - 11y
end

@testset "QQPolyRingElem.resultant" begin
  S, y = polynomial_ring(QQ, "y")

  f = 13y^2 + 7y + 3
  g = 6y + 11

  @test resultant(f, g) == 1219
end

@testset "QQPolyRingElem.discriminant" begin
  S, y = polynomial_ring(QQ, "y")

  f = 17y^2 + 11y + 3

  @test discriminant(f) == -83
end

@testset "QQPolyRingElem.gcdx" begin
  S, y = polynomial_ring(QQ, "y")

  f = 17y^2 + 11y + 3
  g = 61y - 9

  @test gcdx(f, g) == (1, ZZRingElem(3721)//18579, -ZZRingElem(1037)//18579*y-ZZRingElem(824)//18579)
end

@testset "QQPolyRingElem.factor" begin
  S, y = polynomial_ring(QQ, "y")

  @test_throws ArgumentError factor(S(0))
  @test_throws ArgumentError factor_squarefree(S(0))

  f = (2y + 1)^10*(5*y^3 + 1)^100*(-QQFieldElem(1,5))

  fac = factor(f)

  @test f == unit(fac) * prod([ p^e for (p, e) in fac])
  @test occursin("y", sprint(show, "text/plain", fac))

  fac = factor_squarefree(f)

  @test f == unit(fac) * prod([ p^e for (p, e) in fac])

  @test_throws ArgumentError factor(S(0))
  @test_throws ArgumentError factor_squarefree(S(0))

  @test !is_irreducible(S(0))
  @test !is_irreducible(S(1))
  @test !is_irreducible(S(2))
  @test is_irreducible(y^4 + 1)
  @test is_irreducible(y + 1)
  @test !is_irreducible(S(4))
  @test is_irreducible(2y + 2)
  @test !is_irreducible(y^2)

  @test !is_squarefree(S(0))
  @test is_squarefree(S(1))
  @test is_squarefree(S(2))
  @test is_squarefree(S(4))
  @test is_squarefree(7*y^2 + 2)
  @test is_squarefree(2*y)
  @test is_squarefree(4*y)
  @test !is_squarefree(2*y^2)
end

@testset "QQPolyRingElem.signature" begin
  R, x = polynomial_ring(QQ, "x")

  f = (x^3 + 3x + QQ(2)//QQ(3))

  @test signature(f) == (1, 1)
end

@testset "QQPolyRingElem.square_root"  begin
  R, x = polynomial_ring(QQ, "x")

  for i = 1:1000
    f = rand(R, -1:4, -5:5)
    while is_square(f)
      f = rand(R, -1:4, -5:5)
    end

    g0 = rand(R, -1:4, -5:5)
    g = g0^2

    @test sqrt(g)^2 == g

    if !iszero(g)
      @test_throws ErrorException sqrt(-g)
      @test_throws ErrorException sqrt(f*g)
    end

    @test is_square(g)

    f0, s0 = is_square_with_sqrt(g)

    @test f0 && s0^2 == g

    @test iszero(g) || !is_square(-g)
    @test iszero(g) || !is_square(f*g)

    f1, s1 = is_square_with_sqrt(-g)

    @test iszero(g) || !f1

    f2, s2 = is_square_with_sqrt(f*g)

    @test iszero(g) || !f2
  end
end

@testset "QQPolyRingElem.special" begin
  S, y = polynomial_ring(QQ, "y")

  @test chebyshev_t(20, y) == 524288*y^20-2621440*y^18+5570560*y^16-6553600*y^14+4659200*y^12-2050048*y^10+549120*y^8-84480*y^6+6600*y^4-200*y^2+1

  @test chebyshev_u(15, y) == 32768*y^15-114688*y^13+159744*y^11-112640*y^9+42240*y^7-8064*y^5+672*y^3-16*y
end

@testset "QQPolyRingElem.Polynomials" begin
  R, x = polynomial_ring(QQ, "x")
  S, y = polynomial_ring(R, "y")

  f = (3x^2 + 2x + 1)*y^3 + (2x^2 + 4)*y^2 + 4x*y + (2x^2 - x + 1)

  @test f^40*f^60 == f^50*f^50
end

@testset "QQPolyRingElem.remove_valuation" begin
  S, y = polynomial_ring(QQ, "y")

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
end

@testset "QQPolyRingElem.conversion" begin
  S, y = polynomial_ring(QQ, "y")
  f = 7//5*y^2 + 3//2*y + 2
  for R in [residue_ring(ZZ, 13)[1],
            residue_ring(ZZ, ZZ(13))[1],
            Native.GF(13),
            Native.GF(ZZ(13)),
            GF(13)]
    Rx, x = R["x"]
    g = @inferred Rx(f)
    @test g == 4*x^2 + 8*x + 2
  end

  R, x = polynomial_ring(ZZ, "x")
  @test_throws ErrorException R(f)
  f = 7*y^2 + 3*y + 2
  @test @inferred R(f) == 7*x^2 + 3*x + 2
end
