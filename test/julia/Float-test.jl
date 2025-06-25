@testset "BigFloat manipulation" begin
  a = BigFloat(5) / BigFloat(3)
  b = BigFloat(1) / BigFloat(7)
  def_prec = precision(BigFloat)
  new_prec = 127

  @test precision(a) == def_prec
  setprecision!(a, new_prec)
  @test precision(a) == new_prec
  setprecision!(b, new_prec)
  @test precision(b) == new_prec
  mul!(a, a, b) # Does not work with a *= b
  @test precision(a) == new_prec
end
