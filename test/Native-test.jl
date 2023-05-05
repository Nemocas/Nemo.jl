@testset "Native" begin
  @test Native.GF(2) isa Nemo.fpField
  @test Native.GF(2, cached = false) isa Nemo.fpField
  @test Native.GF(UInt(2)) isa Nemo.fpField
  @test Native.GF(UInt(2), cached = false) isa Nemo.fpField
  @test Native.GF(ZZ(2)) isa Nemo.FpField
  @test Native.GF(ZZ(2), cached = false) isa Nemo.FpField
  @test Native.FiniteField(ZZ(2), 2, :oo) isa Tuple{FqPolyRepField, FqPolyRepFieldElem}
  @test Native.FiniteField(ZZ(2), 2, :oo, cached = false) isa Tuple{FqPolyRepField, FqPolyRepFieldElem}
  @test Native.FiniteField(2, 2, :oo) isa Tuple{fqPolyRepField, fqPolyRepFieldElem}
  @test Native.FiniteField(2, 2, :oo, cached = false) isa Tuple{fqPolyRepField, fqPolyRepFieldElem}

  F, x = Native.GF(2)["x"]
  @test Native.FiniteField(x - 1, :oo, cached = false) isa Tuple{fqPolyRepField, fqPolyRepFieldElem}
  F, x = Native.GF(ZZ(2))["x"]
  @test Native.FiniteField(x - 1, :oo, cached = false) isa Tuple{FqPolyRepField, FqPolyRepFieldElem}
  F, = Native.FiniteField(2, 2)
  @test Native.FiniteField(F, 2)  isa fqPolyRepField
  F, = Native.FiniteField(ZZ(2), 2)
  @test Native.FiniteField(F, 2)  isa FqPolyRepField

  @test Native.FiniteField(2) isa Tuple{Nemo.fpField, Nemo.fpFieldElem}
  @test Native.FiniteField(2, cached = false) isa Tuple{Nemo.fpField, Nemo.fpFieldElem}
  @test Native.FiniteField(ZZ(2)) isa Tuple{Nemo.FpField, Nemo.FpFieldElem}
  @test Native.FiniteField(ZZ(2), cached = false) isa Tuple{Nemo.FpField, Nemo.FpFieldElem}

  @test Native.GF(2, 2) isa fqPolyRepField
  @test Native.GF(ZZ(2), 2) isa FqPolyRepField
end
