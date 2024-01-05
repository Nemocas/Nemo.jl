@testset "Spectrum and eigenspaces" begin
  K = GF(5)
  M = matrix(K, 4, 4, [ 0, 0, 1, 0, 1, 0, 0, 0, 0, 1, 0, 0, 0, 0, 0, 1 ])
  l = eigvals(M)
  @test length(keys(l)) == 1
  @test haskey(l, one(K))
  @test l[one(K)] == 2

  L = GF(5, 2)
  a = 2*gen(L) + 1 # third root of 1
  lL = eigvals(M, L)
  @test length(keys(lL)) == 3
  @test haskey(lL, one(L)) && haskey(lL, a) && haskey(lL, a^-1)
  @test lL[one(L)] == 2
  @test lL[a] == 1
  @test lL[a^-1] == 1

  M = matrix(L, 4, 4, [ 0, 0, 1, 0, 1, 0, 0, 0, 0, 1, 0, 0, 0, 0, 0, 1 ])
  lL2 = eigvals(M)
  @test lL == lL2

  Eig = eigenspaces(M, side = :right)
  V = zero_matrix(L, 4, 0)
  for (e, v) in Eig
    @test haskey(lL2, e)
    @test lL2[e] == ncols(v)
    @test M*v == e*v
    V = hcat(V, v)
  end
  @test rref!(V) == 4

  Eig = eigenspaces(M, side = :left)
  V = zero_matrix(L, 0, 4)
  for (e, v) in Eig
    @test haskey(lL2, e)
    @test lL2[e] == nrows(v)
    @test v*M == e*v
    V = vcat(V, v)
  end
  @test rref!(V) == 4
end
