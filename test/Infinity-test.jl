@testset "Inf basics" begin

  @test inf === PosInf()
  @test -inf === NegInf()
  @test -(-inf) === inf

  @test zero(inf) === 0
  @test one(inf) === 1

  @test zero(-inf) === 0
  @test one(-inf) === 1

  @test !signbit(inf)
  @test signbit(-inf)

end

@testset "Inf arithmetic" begin

  @test inf + inf === inf
  @test inf - -inf === inf
  @test -inf + -inf === -inf
  @test -inf - inf === -inf

  @test_throws ErrorException inf - inf
  @test_throws ErrorException (-inf) - (-inf)

  @testset "... with type $(typeof(x))" for x in [1, ZZ(1)]
    @test inf + x === inf
    @test x + inf === inf
    @test inf - x === inf
    @test x - inf === -inf

    @test -inf + x === -inf
    @test x + -inf === -inf
    @test -inf - x === -inf
    @test x - (-inf) === inf
  end
end

@testset "Inf comparisons" begin

  @testset "... against $x" for x in (1, ZZ(1), 1//2, QQ(1//2))
    # we verify the following three objects are in the correct
    # order with respect to isless and various other operations
    ord = [ -inf, x, inf ]
    for i in 1:length(ord), j in 1:length(ord)
      @test min(ord[i], ord[j]) === ord[min(i, j)]
      @test max(ord[i], ord[j]) === ord[max(i, j)]
      @test isless(ord[i], ord[j]) === isless(i, j)
      @test (ord[i] < ord[j]) === (i < j)
      @test (ord[i] <= ord[j]) === (i <= j)
      @test (ord[i] == ord[j]) === (i == j)
    end
  end

  # verify that no positive infinity is less than any other
  eqs = [ Inf, inf, 1//0 ]
  for i in 1:length(eqs), j in 1:length(eqs)
    @test !(eqs[i] < eqs[j])
  end

  # verify that no negative infinity is less than any other
  eqs = [ -Inf, -inf, -1//0 ]
  for i in 1:length(eqs), j in 1:length(eqs)
    @test !(eqs[i] < eqs[j])
  end

end

@testset "Inf predicates for $x" for x in [inf, -inf]

  @test !isone(x)
  @test !isfinite(x)
  @test !iszero(x)

  @test isinf(x)
  @test is_infinite(x)
  @test is_positive(x) == !signbit(x)
  @test is_positive(x) == (x > 0)
  @test is_negative(x) == signbit(x)
  @test is_negative(x) == (x < 0)

end

@testset "is_infinite for other kinds of infinity" begin

  @test !is_infinite(0)
  @test !is_infinite(0.0)

  @test is_infinite(Inf)
  @test is_infinite(-Inf)

  @test is_infinite(1 // 0)
  @test is_infinite(-1 // 0)

  @test is_infinite(big(1) // 0)
  @test is_infinite(big(-1) // 0)

end
