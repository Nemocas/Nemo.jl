@testset "Spectrum and eigenspaces" begin
  K = GF(5)
  M = matrix(K, 4, 4, [ 0, 0, 1, 0, 1, 0, 0, 0, 0, 1, 0, 0, 0, 0, 0, 1 ])
  l = @inferred eigenvalues(M)
  @test length(l) == 1
  @test l[1] == one(K)
  l_m = @inferred eigenvalues_with_multiplicities(M)
  @test length(l_m) == 1
  @test l_m[1] == (one(K), 2)

  L = GF(5, 2)
  a = 2*gen(L) + 1 # third root of 1
  lL = @inferred eigenvalues(L, M)
  @test length(lL) == 3
  @test one(L) in lL
  @test a in lL
  @test a^-1 in lL
  lL_m = @inferred eigenvalues_with_multiplicities(L, M)
  @test Set(lL_m) == Set([ (one(L), 2), (a, 1), (a^-1, 1) ])

  M = matrix(L, 4, 4, [ 0, 0, 1, 0, 1, 0, 0, 0, 0, 1, 0, 0, 0, 0, 0, 1 ])
  lL2 = @inferred eigenvalues(M)
  @test lL == lL2
  lL_m2 = @inferred eigenvalues_with_multiplicities(M)
  @test lL_m == lL_m2

  Eig = @inferred eigenspaces(M, side = :right)
  V = zero_matrix(L, 4, 0)
  for (e, v) in Eig
    @test (e, ncols(v)) in lL_m2
    @test M*v == e*v
    V = hcat(V, v)
  end
  @test rref!(V) == 4

  Eig = @inferred eigenspaces(M, side = :left)
  V = zero_matrix(L, 0, 4)
  for (e, v) in Eig
    @test (e, nrows(v)) in lL_m2
    @test v*M == e*v
    V = vcat(V, v)
  end
  @test rref!(V) == 4
end

@testset "Diagonal" begin
  A = QQ[1 2 3; 4 5 6]
  @test @inferred diagonal(A) == [QQ(1), QQ(5)]
  A = transpose(A)
  @test @inferred diagonal(A) == [QQ(1), QQ(5)]

  @test prod_diagonal(A) == QQ(5)
  @test prod_diagonal(zero_matrix(QQ, 0, 0)) == QQ(1)
end

@testset "Reduce mod RREF" begin
  A = QQ[1 2 3; 4 5 6]
  B = echelon_form(A)
  @test is_zero(reduce_mod(A, B))
  @test is_zero(reduce_mod(A, 2*B))
end
