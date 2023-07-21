@testset "ZZPolyRingElem.constructors" begin
   S1 = PolyRing(ZZ)
   S2 = PolyRing(ZZ)

   @test isa(S1, ZZPolyRing)
   @test S1 !== S2

   R, x = polynomial_ring(ZZ, "x")

   @test elem_type(R) == ZZPolyRingElem
   @test elem_type(ZZPolyRing) == ZZPolyRingElem
   @test parent_type(ZZPolyRingElem) == ZZPolyRing
   @test dense_poly_type(ZZRingElem) == ZZPolyRingElem

   @test typeof(R) <: ZZPolyRing

   @test isa(x, PolyRingElem)

   f = x^3 + 2x^2 + x + 1

   @test isa(f, PolyRingElem)

   g = R(2)

   @test isa(g, PolyRingElem)

   h = R(x^2 + 2x + 1)

   @test isa(h, PolyRingElem)

   k = R([ZZ(1), ZZ(0), ZZ(3)])

   @test isa(k, PolyRingElem)

   l = R([1, 2, 3])

   @test isa(l, PolyRingElem)

   @test polynomial_ring(ZZRing(), "x")[1] != polynomial_ring(ZZRing(), "y")[1]

   R = ZZRing()
   @test polynomial_ring(R, "x", cached = true)[1] === polynomial_ring(R, "x", cached = true)[1]
end

@testset "ZZPolyRingElem.printing" begin
   R, x = polynomial_ring(ZZ, "x")
   f = x^3 + 2x^2 + x + 1

   @test sprint(show, "text/plain", f) == "x^3 + 2*x^2 + x + 1"
end

@testset "ZZPolyRingElem.manipulation" begin
   R, x = polynomial_ring(ZZ, "x")

   @test iszero(zero(R))

   @test isone(one(R))

   @test is_gen(gen(R))

   @test is_unit(one(R))

   f = x^2 + 2x + 1

   @test leading_coefficient(f) == 1

   @test degree(f) == 2

   @test length(f) == 3

   @test coeff(f, 1) == 2

   @test_throws DomainError coeff(f, -1)

   @test canonical_unit(-x + 1) == -1

   @test deepcopy(f) == f

   @test characteristic(R) == 0

   @test height(2*x^2 - 7*x + 1) == 7
end

@testset "ZZPolyRingElem.polynomial" begin
   f = polynomial(ZZ, [])
   g = polynomial(ZZ, [1, 2, 3])
   h = polynomial(ZZ, ZZRingElem[1, 2, 3])
   p = polynomial(ZZ, [1, 2, 3], "y")

   @test isa(f, ZZPolyRingElem)
   @test isa(g, ZZPolyRingElem)
   @test isa(h, ZZPolyRingElem)
   @test isa(p, ZZPolyRingElem)

   q = polynomial(ZZ, [1, 2, 3], cached=false)

   @test parent(g) !== parent(q)
end

@testset "ZZPolyRingElem.similar" begin
   f = polynomial(ZZ, [1, 2, 3])
   g = similar(f)
   h = similar(f, "y")

   @test isa(g, ZZPolyRingElem)
   @test isa(h, ZZPolyRingElem)

   q = similar(g, cached=false)

   @test parent(g) === parent(q)
end

@testset "ZZPolyRingElem.binary_ops" begin
   R, x = polynomial_ring(ZZ, "x")

   f = x^2 + 2x + 1
   g = x^3 + 3x + 2

   @test f + g == x^3+x^2+5*x+3

   @test f*g == x^5+2*x^4+4*x^3+8*x^2+7*x+2

   @test f - g == -x^3+x^2-x-1
end

@testset "ZZPolyRingElem.adhoc_binary" begin
   R, x = polynomial_ring(ZZ, "x")

   f = x^2 + 2x + 1
   g = x^3 + 3x + 2

   @test f*12 == 12*x^2+24*x+12

   @test 7*g == 7*x^3+21*x+14

   @test ZZRingElem(3)*g == 3*x^3+9*x+6

   @test f*ZZRingElem(2) == 2*x^2+4*x+2

   @test f + 12 == x^2+2*x+13

   @test f + ZZRingElem(12) == x^2+2*x+13

   @test f - 12 == x^2+2*x-11

   @test f - ZZRingElem(12) == x^2+2*x-11

   @test 12 + g == x^3+3*x+14

   @test ZZRingElem(12) + g == x^3+3*x+14

   @test 12 - g == -x^3-3*x+10

   @test ZZRingElem(12) - g == -x^3-3*x+10
end

@testset "ZZPolyRingElem.comparison" begin
   R, x = polynomial_ring(ZZ, "x")

   f = x^2 + 2x + 1
   g = x^3 + 3x + 2

   @test f != g

   @test isequal(f, deepcopy(f))
end

@testset "ZZPolyRingElem.adhoc_comparison" begin
   R, x = polynomial_ring(ZZ, "x")

   f = x^2 + 2x + 1

   @test f != 1

   @test 1 != f

   @test R(7) == ZZRingElem(7)

   @test ZZRingElem(7) != f
end

@testset "ZZPolyRingElem.unary_ops" begin
   R, x = polynomial_ring(ZZ, "x")

   f = x^2 + 2x + 1

   @test -f == -x^2 - 2x - 1
end

@testset "ZZPolyRingElem.truncation" begin
   R, x = polynomial_ring(ZZ, "x")

   f = x^2 + 2x + 1
   g = x^3 + 3x + 1

   @test truncate(f, 2) == 2*x+1

   @test_throws DomainError truncate(f, -1)

   @test mullow(f, g, 3) == 7*x^2+5*x+1

   @test_throws DomainError mullow(f, g, -1)
end

@testset "ZZPolyRingElem.reverse" begin
   R, x = polynomial_ring(ZZ, "x")

   f = x^2 + 2x + 3

   @test reverse(f) == 3x^2 + 2x + 1
end

@testset "ZZPolyRingElem.shift" begin
   R, x = polynomial_ring(ZZ, "x")

   f = x^2 + 2x + 1

   @test shift_left(f, 3) == x^5 + 2x^4 + x^3

   @test_throws DomainError shift_left(f, -1)

   @test shift_right(f, 1) == x + 2

   @test_throws DomainError shift_right(f, -1)
end

@testset "ZZPolyRingElem.powering" begin
   R, x = polynomial_ring(ZZ, "x")

   f = x^2 + 2x + 1

   @test f^12 == x^24+24*x^23+276*x^22+2024*x^21+10626*x^20+42504*x^19+134596*x^18+346104*x^17+735471*x^16+1307504*x^15+1961256*x^14+2496144*x^13+2704156*x^12+2496144*x^11+1961256*x^10+1307504*x^9+735471*x^8+346104*x^7+134596*x^6+42504*x^5+10626*x^4+2024*x^3+276*x^2+24*x+1
end

@testset "ZZPolyRingElem.exact_division" begin
   R, x = polynomial_ring(ZZ, "x")

   f = x^2 + 2x + 1
   g = x^3 + 3x + 1

   @test divexact(f*g, f) == g

   @test_throws ErrorException divexact(1, R(2))
end

@testset "ZZPolyRingElem.adhoc_exact_division" begin
   R, x = polynomial_ring(ZZ, "x")

   f = x^2 + 2x + 1

   @test divexact(3*f, ZZ(3)) == f

   @test divexact(3*f, 3) == f
end

@testset "ZZPolyRingElem.divrem" begin
   R, x = polynomial_ring(ZZ, "x")

   f = x^2 + 2x + 1
   g = x + 1

   @test divrem(f, g) == (x+1, 0)

   @test mod(x + 1, x - 1) == 2
end

@testset "ZZPolyRingElem.pseudodivision" begin
   R, x = polynomial_ring(ZZ, "x")

   f = x^2 + 2x + 1
   g = x^3 + 3x + 1

   @test pseudorem(f, g) == x^2+2*x+1

   @test pseudodivrem(f, g) == (0, x^2+2*x+1)
end

@testset "ZZPolyRingElem.remove_valuation" begin
   R, x = polynomial_ring(ZZ, "x")

   f = x^2 + 2x + 1
   g = x^3 + 3x + 1

   v, q = divides(f*g, f)

   @test v
   @test q == g

   v, q = divides(f*g + 1, f)

   @test !v
end

@testset "ZZPolyRingElem.content_primpart_gcd" begin
   R, x = polynomial_ring(ZZ, "x")

   f = x^2 + 2x + 1
   g = x^3 + 3x + 1
   h = x + 1

   @test content(3*f) == 3

   @test primpart(3*f) == f

   @test gcd(f*h, g*h) == h

   @test lcm(f*h, g*h) == f*g*h
end

@testset "ZZPolyRingElem.evaluation" begin
   R, x = polynomial_ring(ZZ, "x")

   f = x^2 + 2x + 1

   @test evaluate(f, 3) == 16

   @test evaluate(f, ZZRingElem(10)) == 121

   @test f(3) == 16

   @test f(ZZRingElem(10)) == 121

end

@testset "ZZPolyRingElem.composition" begin
   R, x = polynomial_ring(ZZ, "x")

   f = x^2 + 2x + 1
   g = x^3 + 3x + 1

   @test compose(f, g) == x^6+6*x^4+4*x^3+9*x^2+12*x+4
end

@testset "ZZPolyRingElem.derivative" begin
   R, x = polynomial_ring(ZZ, "x")

   f = x^2 + 2x + 1

   @test derivative(f) == 2x + 2
end

@testset "ZZPolyRingElem.resultant" begin
   R, x = polynomial_ring(ZZ, "x")

   f = 5x^2 + 2x + 1
   g = 2x^3 + 3x + 1

   @test resultant(f, g) == 212
end

@testset "ZZPolyRingElem.discriminant" begin
   R, x = polynomial_ring(ZZ, "x")

   f = x^2 + 2x + 1

   @test discriminant(f) == 0
end

@testset "ZZPolyRingElem.resx" begin
   R, x = polynomial_ring(ZZ, "x")

   f = 5x^2 + 2x + 1
   g = 2x^3 + 3x + 1

   @test resx(f, g) == (212, 146*x^2-58*x+213, -365*x-1)
end

@testset "ZZPolyRingElem.signature" begin
   R, x = polynomial_ring(ZZ, "x")

   f = x^3 + 3x + 1

   @test signature(f) == (1, 1)
end

@testset "ZZPolyRingElem.interpolate" begin
  Rx, x = polynomial_ring(ZZ, "x")

  xval = [ ZZ(0), ZZ(1), ZZ(2), ZZ(3) ]

  yval = [ ZZ(0), ZZ(1), ZZ(4), ZZ(9) ]

  f = interpolate(Rx, xval, yval)

  @test parent(f) == Rx
  @test f == x^2
end

@testset "ZZPolyRingElem.factor" begin
  Rx, x = polynomial_ring(FlintZZ, "x")

  f = x^24 - x^23 + x^19 - x^18 + x^17 - x^16 + x^14 - x^13 + x^12 - x^11 + x^10 - x^8 + x^7 - x^6 + x^5 - x + 1
  g = x - 1

  fac = factor(-10*f^10 * g^20)

  @test -10*f^10 * g^20 == unit(fac) * prod([ p^e for (p, e) in fac])

  @test fac[f] == 10
  @test fac[g] == 20
  @test f in fac
  @test !(x in fac)

  fac = factor_squarefree(-10*f^10 * g^20)

  @test -10*f^10 * g^20 == unit(fac) * prod([ p^e for (p, e) in fac])
  @test length(fac.fac) == 4

  @test is_irreducible(Rx(2))
  @test is_irreducible(x^4 + 1)
  @test is_irreducible(x + 1)
  @test !is_irreducible(Rx(4))
  @test !is_irreducible(2x + 2)
  @test !is_irreducible(x^2)

  g = x^30 + 73*x^28 + 2504*x^27 - 1359*x^26 - 4680*x^25 - 148311*x^24 - 159120*x^23 - 2615547*x^22 - 9732528*x^21 - 2087859*x^20 - 28418280*x^19 - 19101979*x^18 - 80724312*x^17 - 252445155*x^16 - 549998688*x^15 - 1003657917*x^14 - 2740409376*x^13 - 3199593893*x^12 - 2678402184*x^11 - 5168185293*x^10 - 2855108728*x^9 - 984488613*x^8 - 364079376*x^7 + 2870497527*x^6 + 5561735376*x^5 + 6329635407*x^4 + 3459410600*x^3 + 601207031*x^2 + 822759704*x + 294114975
  @test length(factor(g)) == 6
  @test length(factor(4 * g)) == 7

  @test is_squarefree(7*x^2 + 2)
  @test is_squarefree(2*x)
  @test !is_squarefree(4*x)
  @test !is_squarefree(2*x^2)
end

@testset "ZZPolyRingElem.square_root" begin
  R, x = polynomial_ring(ZZ, "x")

  f = rand(R, 0:10, -10:10)
  g = f^2

  @test sqrt(g)^2 == g

  f = 2*x^4 + x^3 + 2*x^2 - 6*x - 9

  @test is_square(f^2)

  for i = 1:5000
    f = rand(R, -1:5, -10:10)
    while is_square(f)
      f = rand(R, -1:5, -10:10)
    end

    g0 = rand(R, -1:5, -10:10)
    g = g0^2

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

@testset "ZZPolyRingElem.special" begin
   R, x = polynomial_ring(ZZ, "x")

   @test chebyshev_t(20, x) == 524288*x^20-2621440*x^18+5570560*x^16-6553600*x^14+4659200*x^12-2050048*x^10+549120*x^8-84480*x^6+6600*x^4-200*x^2+1

   @test chebyshev_u(15, x) == 32768*x^15-114688*x^13+159744*x^11-112640*x^9+42240*x^7-8064*x^5+672*x^3-16*x

   @test cyclotomic(120, x) == x^32+x^28-x^20-x^16-x^12+x^4+1

   @test cyclotomic(10, 1+x+x^2) == x^8+4*x^7+9*x^6+13*x^5+14*x^4+11*x^3+6*x^2+2*x+1

   @test swinnerton_dyer(5, x) == x^32-448*x^30+84864*x^28-9028096*x^26+602397952*x^24-26625650688*x^22+801918722048*x^20-16665641517056*x^18+239210760462336*x^16-2349014746136576*x^14+15459151516270592*x^12-65892492886671360*x^10+172580952324702208*x^8-255690851718529024*x^6+183876928237731840*x^4-44660812492570624*x^2+2000989041197056

   @test cos_minpoly(30, x) == x^4+x^3-4*x^2-4*x+1

   @test theta_qexp(3, 30, x) == 72*x^29+32*x^27+72*x^26+30*x^25+24*x^24+24*x^22+48*x^21+24*x^20+24*x^19+36*x^18+48*x^17+6*x^16+48*x^14+24*x^13+8*x^12+24*x^11+24*x^10+30*x^9+12*x^8+24*x^6+24*x^5+6*x^4+8*x^3+12*x^2+6*x+1

   @test eta_qexp(24, 30, x) == -29211840*x^29+128406630*x^28+24647168*x^27-73279080*x^26+13865712*x^25-25499225*x^24+21288960*x^23+18643272*x^22-12830688*x^21-4219488*x^20-7109760*x^19+10661420*x^18+2727432*x^17-6905934*x^16+987136*x^15+1217160*x^14+401856*x^13-577738*x^12-370944*x^11+534612*x^10-115920*x^9-113643*x^8+84480*x^7-16744*x^6-6048*x^5+4830*x^4-1472*x^3+252*x^2-24*x+1
end

@testset "ZZPolyRingElem.Polynomials" begin
   R, x = polynomial_ring(ZZ, "x")
   S, y = polynomial_ring(R, "y")

   f = (3x^2 + 2x + 1)*y^3 + (2x^2 + 4)*y^2 + 4x*y + (2x^2 - x + 1)

   @test f^40*f^60 == f^50*f^50
end
