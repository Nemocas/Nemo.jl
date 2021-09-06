@testset "fmpqi.abstract_types" begin
   @test fmpqi <: FieldElem
   @test FlintQQiField <: Nemo.Field
   @test elem_type(QQi) == fmpqi
   @test parent_type(fmpqi) == FlintQQiField
end

@testset "fmpqi.printing" begin
  @test string(zero(QQi)) == "0"
  @test string(one(QQi)) == "1"
  @test string(QQi(2//5,-3)) == "2//5 - 3*im"
  @test string(QQi) == "QQ[im]"
end

@testset "fmpqi.constructors" begin
  for a in Any[true, false, 1, big(1), fmpz(1)]
    @test QQi(a) == a
    @test QQi(a) + im == QQi(a, 1)
    @test QQi(a) - im == QQi(a, -1)
    @test im + QQi(a) == QQi(a, 1)
    @test im - QQi(a) == QQi(-a, 1)
    @test QQi(a)*im == QQi(0, a)
    @test im*QQi(a) == QQi(0, a)
  end
end

@testset "fmpqi.conversions" begin
  @test QQ(QQi(9)) == 9
  @test_throws Exception QQ(QQi(0,9))
  @test convert(Complex{Rational{BigInt}}, QQi(8,9)) == 8 + 9*im
  @test convert(fmpqi, 8//5 + 9*im) == 8//5 + 9*im
  @test convert(fmpqi, 8 + 9*im) == 8 + 9*im
  @test convert(fmpqi, 8) == 8
end

@testset "fmpqi.adhoc" begin
  @test ZZ(5) + im//2 == QQi(5, 1//2)
  @test im//2 + ZZ(5) == QQi(5, 1//2)
  @test ZZ(5) - im//2 == QQi(5, -1//2)
  @test im//2 - ZZ(5) == QQi(-5, 1//2)
  @test ZZ(5) * im//2 == QQi(0, 5//2)
  @test im//2 * ZZ(5) == QQi(0, 5//2)
end

