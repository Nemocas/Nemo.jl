@testset "fqPolyRepField.conversions" begin
  p = 11
  K, g = Native.finite_field(p, 2, "g")
  Fx, t = polynomial_ring(Native.GF(p), "t")
  Rx, s = polynomial_ring(residue_ring(ZZ, p)[1], "s")

  # length exceeds 2*degree(K), which forces division by the modulus
  @test K(t^7 + 3*t + 1) == g^7 + 3*g + 1
  @test K(s^7 + 3*s + 1) == g^7 + 3*g + 1
end
