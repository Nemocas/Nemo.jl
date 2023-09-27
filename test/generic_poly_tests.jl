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
    @test isa(x, PolyRingElem)

    a = Rx()

    @test isa(a, PolyRingElem)
    @test parent(a) == Rx

    b = Rx(2)

    @test isa(b, PolyRingElem)
    @test parent(b) == Rx

    c = Rx(UInt(3))

    @test isa(c, PolyRingElem)
    @test parent(c) == Rx

    d = Rx(ZZ(3))

    @test isa(d, PolyRingElem)
    @test parent(d) == Rx

    e = Rx(R(16))

    @test isa(e, PolyRingElem)
    @test parent(e) == Rx

    m = Rx([1, 2, 3])

    @test isa(m, PolyRingElem)
    @test parent(e) == Rx

    f = Rx([UInt(1), UInt(2), UInt(3)])

    @test isa(f, PolyRingElem)
    @test parent(f) == Rx

    g = Rx([ZZ(1), ZZ(2), ZZ(3)])

    @test isa(g, PolyRingElem)
    @test parent(g) == Rx

    h = Rx([R(1), R(2), R(3)])

    @test isa(h, PolyRingElem)
    @test parent(h) == Rx

    m = Rx(1:3)

    @test isa(m, PolyRingElem)
    @test parent(e) == Rx

    n = Rx(ZZ(1):ZZ(3))

    @test isa(n, PolyRingElem)
    @test parent(e) == Rx

    _a = polynomial_ring(ZZ, "y")[1]([ZZ(1),ZZ(2),ZZ(3)])

    k = Rx(_a)

    @test isa(k, PolyRingElem)
    @test parent(k) == Rx

    l = x^2 + x^2 + x^2 + x^1 + x^1 + R(1)

    @test isa(l, PolyRingElem)
    @test parent(l) == Rx

    @test f == g
    @test g == h
    @test h == k
    @test k == l
    @test l == m
    @test m == n
  end

end
