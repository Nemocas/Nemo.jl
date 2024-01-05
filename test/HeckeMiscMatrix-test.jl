@testset "Spectrum and eigenspaces" begin
  K = GF(5)
  M = matrix(K, 4, 4, [ 0, 0, 1, 0, 1, 0, 0, 0, 0, 1, 0, 0, 0, 0, 0, 1 ])
  l = eigenvalues(M)
  @test length(l) == 1
  @test l[1] == one(K)
  l_m = eigenvalues_with_multiplicities(M)
  @test length(l_m) == 1
  @test l_m[1] == (one(K), 2)

  L = GF(5, 2)
  a = 2*gen(L) + 1 # third root of 1
  lL = eigenvalues(L, M)
  @test length(lL) == 3
  @test one(L) in lL && a in lL && a^-1 in lL
  lL_m = eigenvalues_with_multiplicities(L, M)
  @test Set(lL_m) == Set([ (one(L), 2), (a, 1), (a^-1, 1) ])

  M = matrix(L, 4, 4, [ 0, 0, 1, 0, 1, 0, 0, 0, 0, 1, 0, 0, 0, 0, 0, 1 ])
  lL2 = eigenvalues(M)
  @test lL == lL2
  lL_m2 = eigenvalues_with_multiplicities(M)
  @test lL_m == lL_m2

  Eig = eigenspaces(M, side = :right)
  V = zero_matrix(L, 4, 0)
  for (e, v) in Eig
    @test (e, ncols(v)) in lL_m2
    @test M*v == e*v
    V = hcat(V, v)
  end
  @test rref!(V) == 4

  Eig = eigenspaces(M, side = :left)
  V = zero_matrix(L, 0, 4)
  for (e, v) in Eig
    @test (e, nrows(v)) in lL_m2
    @test v*M == e*v
    V = vcat(V, v)
  end
  @test rref!(V) == 4
end
