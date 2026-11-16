@testset "Native" begin
  @test Native.GF(2) isa Nemo.fpField
  @test Native.GF(2, cached = false) isa Nemo.fpField
  @test Native.GF(UInt(2)) isa Nemo.fpField
  @test Native.GF(UInt(2), cached = false) isa Nemo.fpField
  @test Native.GF(ZZ(2)) isa Nemo.FpField
  @test Native.GF(ZZ(2), cached = false) isa Nemo.FpField
  @test Native.finite_field(ZZ(2), 2, :oo) isa Tuple{FqPolyRepField, FqPolyRepFieldElem}
  @test Native.finite_field(ZZ(2), 2, :oo, cached = false) isa Tuple{FqPolyRepField, FqPolyRepFieldElem}
  @test Native.finite_field(2, 2, :oo) isa Tuple{fqPolyRepField, fqPolyRepFieldElem}
  @test Native.finite_field(2, 2, :oo, cached = false) isa Tuple{fqPolyRepField, fqPolyRepFieldElem}

  F, x = Native.GF(2)["x"]
  @test Native.finite_field(x - 1, :oo, cached = false) isa Tuple{fqPolyRepField, fqPolyRepFieldElem}
  F, x = Native.GF(ZZ(2))["x"]
  @test Native.finite_field(x - 1, :oo, cached = false) isa Tuple{FqPolyRepField, FqPolyRepFieldElem}
  F, = Native.finite_field(2, 2)
  @test Native.finite_field(F, 2)  isa fqPolyRepField
  F, = Native.finite_field(ZZ(2), 2)
  @test Native.finite_field(F, 2)  isa FqPolyRepField

  @test Native.finite_field(2) isa Tuple{Nemo.fpField, Nemo.fpFieldElem}
  @test Native.finite_field(2, cached = false) isa Tuple{Nemo.fpField, Nemo.fpFieldElem}
  @test Native.finite_field(ZZ(2)) isa Tuple{Nemo.FpField, Nemo.FpFieldElem}
  @test Native.finite_field(ZZ(2), cached = false) isa Tuple{Nemo.FpField, Nemo.FpFieldElem}

  @test Native.GF(2, 2) isa fqPolyRepField
  @test Native.GF(ZZ(2), 2) isa FqPolyRepField

  F2 = Native.GF(2, cached = false)
  @test F2([1]) == one(F2)
  F4 = Native.GF(2, 2)
  @test F4([1, 0]) == one(F4)
end

@testset "Native ext_of_degree" begin
  p = next_prime(UInt(typemax(Int)))
  for (K, T) in ((Native.GF(p), fqPolyRepField), (Native.GF(ZZ(p)), FqPolyRepField))
    L, f = AbstractAlgebra.ext_of_degree(K, 2)
    @test L isa T
    @test domain(f) === K
    @test codomain(f) === L
    @test order(L) == order(K)^2
    @test f(-one(K)) == -one(L)
    @test preimage(f, -one(L)) == -one(K)

    M, g = AbstractAlgebra.ext_of_degree(L, 2)
    @test M isa T
    @test domain(g) === L
    @test codomain(g) === M
    @test order(M) == order(L)^2
    a = gen(L) - 1
    @test g(a^2) == g(a)^2
    @test g(-one(L)) == -one(M)
    @test preimage(g, g(a)) == a

    N, i = AbstractAlgebra.ext_of_degree(L, 1)
    @test order(N) == order(L)
    @test domain(i) === L
    @test codomain(i) === N
    @test i(one(L)) == one(N)
    @test_throws DomainError AbstractAlgebra.ext_of_degree(L, 0)
    @test_throws OverflowError AbstractAlgebra.ext_of_degree(L, typemax(Int))
  end

  K = Native.GF(p, 2; cached = false)
  L = Native.GF(p, 2; cached = false)
  f = embed(K, L)
  @test f(-one(K)) == -one(L)
  @test preimage(f, f(gen(K))) == gen(K)
end
