@testset "Prime wrappers" begin
  a = UInt8(3)
  @test is_prime(a)
  b = next_prime(a)
  @test b == UInt8(5)
  @test typeof(b) == UInt8
end

@testset "UInt valuation and remove" begin
  for (a, b) in [(1, 2), (4, 2), (9, 3), (12, 2), (12, 3), (12, 4), (12, 5)]
    @test valuation(UInt(a), UInt(b)) == valuation(a, b)
  end

  # a power of b wraps modulo 2^64 when squared
  for (a, b, v) in [(typemax(UInt), typemax(UInt), 1),
                    (UInt(2)^63 + 1, UInt(2)^63 + 1, 1),
                    (UInt(3) << 32, UInt(1) << 32, 1),
                    (UInt(1) << 48, UInt(1) << 16, 3)]
    @test valuation(a, b) == v
    @test remove(a, b) == (v, a ÷ b^v)
  end

  @test valuation(UInt(3)^40, UInt(3)) == 40
  @test_throws ErrorException valuation(UInt(5), UInt(0))
  @test_throws ErrorException valuation(UInt(5), UInt(1))

  for _ in 1:10_000
    b = rand((rand(UInt(2):UInt(100)), rand(UInt(2):typemax(UInt)), UInt(1) << rand(1:63)))
    a = rand(UInt(1):UInt(100))
    while rand(Bool) && a <= typemax(UInt) ÷ b
      a *= b
    end

    v, q = 0, a
    while q % b == 0
      q ÷= b
      v += 1
    end
    @test valuation(a, b) == v
    @test remove(a, b) == (v, q)
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
