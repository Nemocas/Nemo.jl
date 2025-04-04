@testset "Prime wrappers" begin
  a = UInt8(3)
  @test is_prime(a)
  b = next_prime(a)
  @test b == UInt8(5)
  @test typeof(b) == UInt8
end

@testset "UInt valuation" begin
  for (a, b) in [(1, 2), (4, 2), (9, 3), (12, 2), (12, 3), (12, 4), (12, 5)]
    @test valuation(UInt(a), UInt(b)) == valuation(a, b)
  end
end

@testset "Integer fits" begin
  @test fits(Int, typemin(Int))
  @test fits(Int, typemax(Int))
  @test !fits(Int, typemax(UInt))
  @test fits(Int, typemin(UInt))
  @test fits(Int, UInt(typemax(Int)))
end

@testset "Integer clog" begin
  @test clog(7, 2) == 3
  @test clog(8, 2) == 3
  @test clog(25, 5) == 2
  @test clog(27, 5) == 3
end
