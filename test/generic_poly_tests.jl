function test_poly_constructors(R)

  @testset "poly.constructors for $(R) of type $(typeof(R))" begin
    S1 = PolyRing(R)
    S2 = PolyRing(R)

    #@test isa(S1, GFPPolyRing)
    @test S1 !== S2

    Rx, x = polynomial_ring(R, "x")

    #@test elem_type(Rx) == gfp_poly
    #@test elem_type(GFPPolyRing) == gfp_poly
    #@test parent_type(gfp_poly) == GFPPolyRing
    #@test dense_poly_type(gfp_elem) == gfp_poly

    #S = GF(19)
    #Sy, y = polynomial_ring(R, "y")

    #RRx, xx = polynomial_ring(R, "x")
    #RRRx, xxx = polynomial_ring(GF(17), "xx")

    @test var(Rx) == Symbol("x")

    #@test RRx != RRRx

    #@test RRx == Rx

    #@test S != R

    #@test isa(Rx, GFPPolyRing)
    @test isa(x, PolyElem)

    a = Rx()

    @test isa(a, PolyElem)
    @test parent(a) == Rx

    b = Rx(2)

    @test isa(b, PolyElem)
    @test parent(b) == Rx

    c = Rx(UInt(3))

    @test isa(c, PolyElem)
    @test parent(c) == Rx

    d = Rx(fmpz(3))

    @test isa(d, PolyElem)
    @test parent(d) == Rx

    e = Rx(R(16))

    @test isa(e, PolyElem)
    @test parent(e) == Rx

    m = Rx([1, 2, 3])

    @test isa(m, PolyElem)
    @test parent(e) == Rx

    f = Rx([UInt(1), UInt(2), UInt(3)])

    @test isa(f, PolyElem)
    @test parent(f) == Rx

    g = Rx([fmpz(1), fmpz(2), fmpz(3)])

    @test isa(g, PolyElem)
    @test parent(g) == Rx

    h = Rx([R(1), R(2), R(3)])

    @test isa(h, PolyElem)
    @test parent(h) == Rx

    m = Rx(1:3)

    @test isa(m, PolyElem)
    @test parent(e) == Rx

    n = Rx(fmpz(1):fmpz(3))

    @test isa(n, PolyElem)
    @test parent(e) == Rx

    _a = polynomial_ring(ZZ, "y")[1]([fmpz(1),fmpz(2),fmpz(3)])

    k = Rx(_a)

    @test isa(k, PolyElem)
    @test parent(k) == Rx

    l = x^2 + x^2 + x^2 + x^1 + x^1 + R(1)

    @test isa(l, PolyElem)
    @test parent(l) == Rx

    @test f == g
    @test g == h
    @test h == k
    @test k == l
    @test l == m
    @test m == n
  end

end
