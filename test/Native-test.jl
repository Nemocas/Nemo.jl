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
end
