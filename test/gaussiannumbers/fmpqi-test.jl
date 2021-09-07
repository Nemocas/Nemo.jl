@testset "fmpqi.abstract_types" begin
  @test fmpqi <: FieldElem
  @test FlintQQiField <: Nemo.Field
  @test elem_type(QQi) == fmpqi
  @test parent_type(fmpqi) == FlintQQiField
  @test base_ring(QQi) == ZZi
  @test base_ring(QQi()) == ZZi
end

@testset "fmpqi.hash" begin
  @test hash(QQi(2, 3)//5) == hash(ZZi(2, 3)//5)
  @test hash(QQi) == hash(deepcopy(QQi))
  @test QQi === Base.deepcopy_internal(QQi, IdDict())
end

@testset "fmpqi.printing" begin
  @test string(zero(QQi)) == "0"
  @test string(one(QQi)) == "1"
  @test string(QQi(2//5,-3)) == "2//5 - 3*im"
  @test string(QQi) == "QQ[im]"
end

@testset "fmpqi.constructors" begin
  for a in Any[true, false, 1, big(1), fmpz(1), fmpq(2,3)]
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
  @test ZZ(QQi(9)) == 9
  @test_throws Exception ZZ(QQi(2//3,0))
  @test_throws Exception ZZ(QQi(0,9))
  @test ZZi(QQi(8,9)) == 8 + 9*im
  @test_throws Exception ZZi(QQi(8//3,9))
  @test convert(Complex{Rational{BigInt}}, QQi(8,9)) == 8 + 9*im
  @test convert(Complex{Rational{Int}}, QQi(8,9)) == 8 + 9*im
  @test convert(fmpqi, 8//5 + 9*im) == 8//5 + 9*im
  @test convert(fmpqi, 8 + 9*im) == 8 + 9*im
  @test convert(fmpqi, 8) == 8
end

@testset "fmpqi.basic manipulation" begin
  a = QQi(1//2, 2//3)
  @test abs2(a) == real(a)^2 + imag(a)^2
  @test nbits(a) < 100
  @test parent(canonical_unit(a)) == QQi
end

@testset "fmpqi.adhoc" begin
  @test ZZ(5) + im//2 == QQi(5, 1//2)
  @test im//2 + ZZ(5) == QQi(5, 1//2)
  @test ZZ(5) - im//2 == QQi(5, -1//2)
  @test im//2 - ZZ(5) == QQi(-5, 1//2)
  @test ZZ(5) * im//2 == QQi(0, 5//2)
  @test im//2 * ZZ(5) == QQi(0, 5//2)

  @test ZZi(4 + 6*im)//ZZ(10) == QQi(4//10, 6//10)
  @test (4 + 6*im)//ZZ(10) == QQi(4//10, 6//10)
  @test ZZ(10)//(4 + 6*im) == QQi(10//13, -15//13)
  @test (4 + 6*im)//(ZZ(10)*im) == QQi(4//10, 6//10)//im
  @test ZZ(10)*im//(4 + 6*im) == QQi(10//13, -15//13)*im

  for (a, bs) in [[QQi(1,1), [2, ZZ(2), 2*im, ZZi(2), 2//3, QQ(2//3), 2*im//3]],
                  [2*im//3,  [ZZ(2), ZZi(1,1), QQ(2//3), QQi(1,1)]],
                  [QQ(2//3), [2*im, ZZi(1,1), 2*im//3, QQi(1,1)]],
                  [2//3,     [ZZi(1,1), QQi(1,1)]],
                  [ZZi(1,1), [2//3, QQ(2//3), 2*im//3, QQi(1,1)]],
                  [1+2*im,   [QQ(2//3), QQi(1,1)]],
                  [ZZ(2),    [2*im//3, QQi(1,1)]],
                  [2,        [QQi(1,1)]]]
    for b in bs
      @test Nemo.AbstractAlgebra.promote_rule(typeof(a), typeof(b)) == fmpqi
      @test QQi == parent(a*b)
      @test QQi == parent(b*a)
      @test QQi == parent(a + b)
      @test QQi == parent(b + a)
      @test QQi == parent(a - b)
      @test QQi == parent(b - a)
      @test QQi == parent(a//b)
      @test QQi == parent(b//a)
      @test QQi(a)*QQi(b) == a*b
      @test QQi(b)*QQi(a) == b*a
      @test QQi(a) + QQi(b) == a + b
      @test QQi(b) + QQi(a) == b + a
      @test QQi(a) - QQi(b) == a - b
      @test QQi(b) - QQi(a) == b - a
    end
  end
end

@testset "fmpqi.unsafe" begin
  for i in 1:1
    a = rand_bits(QQi, 600); A = deepcopy(a)
    b = rand_bits(QQi, 600); B = deepcopy(b)
    t = rand_bits(QQi, 600)
    @test isone(one!(t))
    @test iszero(zero!(t))
    @test mul!(t, a, b) == a*b
    @test mul!(t, t, b) == a*b^2
    @test mul!(t, a, t) == a^2*b^2
    @test mul!(t, t, t) == a^4*b^4
    @test mul!(t, t, -2) == -2*a^4*b^4
    @test add!(t, t, t) == -4*a^4*b^4
    @test one!(t) == 1 + 0*im
    @test addmul!(t, a, b) == 1 + a*b
    @test addmul!(t, a, b, fmpqi()) == 1 + 2*a*b
    @test Nemo.submul!(t, a, b) == 1 + a*b
    @test Nemo.submul!(t, a, b, fmpqi()) == 1
    @test addmul!(t, t, b) == 1 + b
    @test Nemo.submul!(t, t, a) == (1 + b)*(1 - a)
    @test Nemo.set!(t, a) == a
    @test Nemo.inv!(t, t) == a^-1
    @test sub!(t, a, t) == a - a^-1
    @test 0 + 0*im == sub!(t, t, t)
    Nemo.swap!(a, b)
    @test b == A && a == B
  end
end

function test_elem(R::FlintQQiField)
  return rand_bits(R, rand(0:200))
end

@testset "fmpqi.conformance_tests" begin
  test_Field_interface(QQi)
end
