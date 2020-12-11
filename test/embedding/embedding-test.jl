@testset "embedding-morphism" begin

  F = FiniteField(101, 2, "a")[1]
  K = FiniteField(101, 4, "b")[1]
  Kt = PolynomialRing(K, "t")[1]
  f = modulus(F)
  p = Vector{fq_nmod}(undef, degree(f)+1)
  for i = 1:length(f)
    p[i] = K(coeff(f, i-1).data)
  end
  rt = Nemo.any_root(Kt(p))
  mp = @inferred hom(F, K, rt)

  @test mp(gen(F)) == rt
  @test domain(mp) == F
  @test codomain(mp) == K
  @test isa(mp, Nemo.FinFieldMorphism{FqNmodFiniteField, FqNmodFiniteField})

  @test_throws ArgumentError preimage(mp, gen(K))
  for i = 1:10
    x = rand(F)
    g = @inferred mp(x)
    h = @inferred preimage(mp, g)
    @test x == h
  end

end

@testset "embedding-lattice" begin

  F = FiniteField(2, 2^3*3^3, "a")[1]
  F1 = FiniteField(2, 2^2*3^2, "b")[1]
  mpF = Nemo.FinFieldMorphism{FqNmodFiniteField, FqNmodFiniteField}[]
  mpF1 = Nemo.FinFieldMorphism{FqNmodFiniteField, FqNmodFiniteField}[]
  for i = 1:degree(F1)-1
    if iszero(mod(degree(F1), i))
      Fi = FiniteField(2, i, "ai")[1]
      push!(mpF, embed(Fi, F))
      push!(mpF1, embed(Fi, F1))
    end
  end
  mp = embed(F1, F)
  for i = 1:length(mpF)
    g = gen(domain(mpF[i]))
    @test mp(mpF1[i](g)) == mpF[i](g)
  end


end