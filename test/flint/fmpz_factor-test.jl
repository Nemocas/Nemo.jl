
@testset "Integer factorization" begin

  @test_throws ArgumentError factor(0)
  @test_throws ArgumentError factor(Int128(0))
  @test_throws ArgumentError factor(UInt128(0))
  @test_throws ArgumentError factor(BigInt(0))
  @test_throws ArgumentError factor(ZZ(0))


  @test_throws DomainError factor(true)
  @test_throws DomainError factor(false)


  # trivial case: input is 1
  F1 = factor(1)
  F1_ZZ = factor(ZZ(1))
  @test length(F1) == 0
  @test length(F1_ZZ) == 0
  @test unit(F1) == 1
  @test unit(F1_ZZ) == 1


  # Test different integer types
  F = factor(123456789)
  F_Int128 = factor(Int128(123456789))
  F_UInt128 = factor(UInt128(123456789))
  F_BigInt = factor(BigInt(123456789))
  F_ZZ = factor(ZZ(123456789))

  @test length(F) == 3
  @test length(F_Int128) == 3
  @test length(F_UInt128) == 3
  @test length(F_BigInt) == 3
  @test length(F_ZZ) == 3

  @test unit(F) == 1
  @test unit(F_Int128) == 1
  @test unit(F_UInt128) == 1
  @test unit(F_BigInt) == 1
  @test unit(F_ZZ) == 1


  # an example with two "large" factors
  repunit37 = ZZ(10)^37 -1
  repunit41 = ZZ(10)^41 -1
  FF = factor(repunit37*repunit41) # trickier but still quick
  @test length(FF) == 8


  # negative input
  F_minus1 = factor(-1)
  F_minus1_ZZ = factor(ZZ(-1))
  @test length(F_minus1) == 0
  @test length(F_minus1_ZZ) == 0
  @test unit(F_minus1) == -1
  @test unit(F_minus1_ZZ) == -1

  for T in [ Int8,  Int16,  Int32,  Int64,  UInt8,  UInt16,  UInt32,  UInt64 ]
    @test_throws ArgumentError factor(T(0))
    fac99 = factor(T(99))
    @test  unit(fac99) == 1
    @test  typeof(unit(fac99)) == T
    @test  length(fac99) == 2
    @test  issetequal([a  for (a,_) in fac99], [3, 11])
    @test  issetequal([e  for (_,e) in fac99], [2, 1])
    for (a,_) in fac99
      @test  typeof(a) == T
    end
  end

end
