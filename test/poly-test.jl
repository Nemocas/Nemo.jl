@testset "Factor squarefree" begin
  K, a = cyclotomic_field(3, "a")
  Kx, x = K["x"]
  f = (x - 1)^2 * x^2 * (x + 1)
  sf = factor_squarefree(f)
  @test length(sf) == 2
  @test all(is_squarefree, (x for (x, _) in sf))
  @test evaluate(sf) == f

  sf = factor_squarefree(Kx(1))
  @test length(sf) == 0
  @test evaluate(sf) == Kx(1)
  sf = factor_squarefree(x)
  @test length(sf) == 1
  @test evaluate(sf) == x
end

@testset "Squarefreeness" begin
  R, x = QQ["x"]
  @test !is_squarefree(R(0))
  @test is_squarefree(R(3))
  @test is_squarefree(x)
  @test !is_squarefree(x^2)
  @test is_squarefree(2*x)
  @test !is_squarefree(2*x^2)

  R, x = ZZ["x"]
  @test !is_squarefree(R(0))
  @test is_squarefree(R(3))
  @test is_squarefree(x)
  @test !is_squarefree(x^2)
  @test is_squarefree(2*x)
  @test !is_squarefree(2*x^2)
end

@testset "partial_fractions" begin
  R, x = QQ["x"]
  f = 3*x^4+1
  g = (1-x)*(1-x^2)
  p = partial_fractions(f, g)
  #order is depending on factoring, hence not deterministic
  @test Set(p) == Set([3*x + 3//x^0, 5//(x - 1), 2//(x^2 - 2*x + 1), 1//(x + 1)])
  @test isone(denominator(first(p)))
  @test sum(p) == f//g
end

@testset "convolution" begin
  @test convolution([3, 6, 1, 5, 3, 2], [4, 3, 6, 7, -1]) == [12, 33, 40, 80, 72, 48, 58, 28, 11, -2]
  @test_throws "InexactError" convolution([39876897139871313, 123984703947], [3579071, 937482163461])
  @test convolution([3987689713987131353139879087135136313, 123984703947321513039870918730739086731876131], [35790710975387503812123512354123413, 93748216346112313136363613261321357132905791382758135]) == BigInt[142722250012839078095325803216475195931099886254439608855910119379796269, 373838798032469822997047496153015604232769072820405310413333311355166162352122914537611358, 11623344849262182502643316753569132935892798154610516217818229817660636539661629113670773952575685]
  @test convolution(ZZ.([1, 3, 5, 7]), ZZ.([8, 2, 4, 6])) == ZZRingElem[8, 26, 50, 84, 52, 58, 42]
end
