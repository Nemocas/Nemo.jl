@testset "AbsMSeries.constructors" begin
    R, (x, y) = power_series_ring(QQ, [5, 5], ["x", "y"])

    @test R() == 0
    @test R(1) == 1
    @test R(QQ(2, 1)) == 2
    @test R(y) == y
end

@testset "AbsMSeries.manipulation" begin
   R, (x, y) = power_series_ring(QQ, [5, 5], ["x", "y"])

   @test gens(R) == [x, y]
   @test isone(one(R))
   @test iszero(zero(R))
   @test is_gen(gen(R, 1))

   f = 2x^2*y^3 + 3x^2*y + y + 1

   @test truncate(f, [3, 2]) == 3x^2*y + y + 1

   @test length(f) == 4

   @test nvars(R) == 2

   @test precision(f) == [5, 5]
   @test precision(f + O(y^4) + O(x^4)) == [4, 4]

   @test max_precision(R) == [5, 5]

   @test valuation(f) == [0, 0]

   @test coeff(f, 3) == 3

   @test is_unit(f)

   @test parent(f) == R

   @test base_ring(R) == QQ

   @test symbols(R) == [:x, :y]

   @test deepcopy(f) == f
end

@testset "AbsMSeries.unary/binary_ops" begin
   R, (x, y) = power_series_ring(QQ, [5, 5], ["x", "y"])

   f = 2x^2*y^3 + 3x^2*y + y + 1

   @test f + (-f) == 0

   @test f + f == 2f

   @test f - f == 0

   @test f*f == f^2

   @test f == f

   @test isequal(f, f)
end

@testset "AbsMSeries.inv/divexact" begin
   R, (x, y) = power_series_ring(QQ, [5, 5], ["x", "y"])

   f = 2x^2*y^3 + 3x^2*y + y + 1

   @test isone(f*inv(f))

   @test isone(divexact(f, f))
end

@testset "AbsMSeries.evaluate" begin
   R, (x, y) = power_series_ring(QQ, [5, 5], ["x", "y"])

   f = 2x^2*y^3 + 3x^2*y + y + 1

   @test evaluate(f, [x + 1, y + 1]) == evaluate(evaluate(f, [x], [x + 1]), [y], [y + 1])
end

