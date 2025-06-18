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

@testset "Eigenvalues of ZZMatrix" begin
  A4 = matrix(ZZ,4,4, [6,-8,9,5,  5,-6,5,0,  0,6,5,4,  -6,8,-1,7])
  E4 = eigenvalues(A4)
  @test length(E4) == 2
  @test issetequal(E4, [2,4])
  E4m = eigenvalues_with_multiplicities(A4)
  @test length(E4m) == 2
  @test issetequal(E4m, [(ZZ(2),1), (ZZ(4),1)])


  A5 = matrix(ZZ,5,5, [0,0,1,-1,-1,  -1,-1,1,1,-1,  0,0,1,-1,-1,  -1,1,1,-1,-1,  1,1,-1,-1,1])
  E5 = eigenvalues(A5)
  @test length(E5) == 3
  @test issetequal(E5, [0,1,-2])

  E5m = eigenvalues_with_multiplicities(A5)
  @test length(E5m) == 3
  @test issetequal(E5m, [(ZZ(0),2), (ZZ(1),2), (ZZ(-2),1)])


  A6 = matrix(ZZ,6,6, [0,0,-1,0,1,0,  0,0,0,1,-1,0,  -1,0,0,0,1,0,  0,1,1,1,-1,0,  1,-1,1,1,-1,0,  1,-1,0,0,-1,-1])
  E6 = eigenvalues(A6)
  @test length(E6) == 2
  @test issetequal(E6, [1,-1])

  E6m = eigenvalues_with_multiplicities(A6)
  @test length(E6m) == 2
  @test issetequal(E6m, [(ZZ(-1),2), (ZZ(1),2)])


  A6b = matrix(ZZ,6,6, [0,1,0,-1,1,0,  1,1,-2,0,-2,0,  -1,0,1,1,1,0,  1,-1,2,1,0,0,  1,1,-2,-2,-1,-2,  2,-2,0,2,-2,0])
  E6b = eigenvalues(A6b)
  @test length(E6b) == 2
  @test issetequal(E6b, [1,-1])

  E6bm = eigenvalues_with_multiplicities(A6b)
  @test length(E6bm) == 2
  @test issetequal(E6bm, [(ZZ(-1),1), (ZZ(1),2)])

  M_2x2 = matrix(ZZ, [0 1; -1 0])
  @test length(eigenvalues(M_2x2)) == 0
  K = algebraic_closure(QQ)
  lambda1, lambda2 = eigenvalues(K, M_2x2)
  MK_2x2 = matrix(K, M_2x2)
  @test length(eigenspaces(MK_2x2)) == 2
  @test size(eigenspace(MK_2x2, lambda1)) == (1,2)

  @test length(eigenspaces(M_2x2)) == 0
  @test length(eigenspaces(K, M_2x2)) == 2
  @test size(eigenspace(M_2x2, lambda1)) == (1,2)

  M_3x3 = matrix(ZZ, [[5, -2, -2],  [2, 1, -2],  [2, -2, 1]])
  @test length(eigenvalues(M_3x3)) == 2
  @test length(eigenspaces(M_3x3)) == 2
  @test size(eigenspace(M_3x3, ZZ(1))) == (1,3)
  @test size(eigenspace(M_3x3, ZZ(2))) == (0,3)
  @test size(eigenspace(M_3x3, ZZ(3))) == (2,3)

  # eigenvalues_simple not defined for integer matrices, so not tested.
end


@testset "is_unimodular" begin

  # Some trivial inputs
  @test is_unimodular(matrix(ZZ,0,0,[]))
  @test is_unimodular(matrix(ZZ,1,1,[1]))
  @test is_unimodular(matrix(ZZ,1,1,[-1]))
  @test !is_unimodular(matrix(ZZ,1,1,[0]))
  
  @test is_unimodular(identity_matrix(ZZ, 11))
  @test !is_unimodular(2*identity_matrix(ZZ, 11))
  @test !is_unimodular(zero_matrix(ZZ, 11,11))

  # This test increases coverage slightly
  N = 1+prod(filter(is_prime, ZZ(2)^20:(ZZ(2)^20+ZZ(2)^11)))
  @test !is_unimodular(N*identity_matrix(ZZ, 11))


  @test_throws ArgumentError is_unimodular(matrix(ZZ,0,1,[]))
  @test_throws ArgumentError is_unimodular(matrix(ZZ,0,0,[]); algorithm=:WRONG)

  U = matrix(ZZ, 3,3,  [ 3, 22, 46, 5, 35, 73, 4, 27, 56])
  M = matrix(ZZ, 3,3,  [-3, 22, 46, 5, 35, 73, 4, 27, 56])

  # U6 is "small" and triggers the 2nd loop in P-S method
  U6 = matrix(ZZ, 6,6,
              [1,      -76,       87,       -57,      -41,   999950,
               0,        1,        0,         0,        0,        0,
               0,   999942,        1,         0,        0,        0,
               0,      -48,   999963,         1,        0,        0,
               0,       91,      -24,   1000012,        1,        0,
               0,       -2,       71,        77,   999925,        1])


  # Need larger values of k to increase coverage
  for k in 0:25
    @test is_unimodular(U^k)
    @test is_unimodular(U^k; algorithm=:CRT)
    @test is_unimodular(U^k; algorithm=:pauderis_storjohann)
    @test is_unimodular(U^k; algorithm=:auto)
    @test is_unimodular(-U^k)
    @test is_unimodular(-U^k; algorithm=:CRT)
    @test is_unimodular(-U^k; algorithm=:pauderis_storjohann)
    @test is_unimodular(-U^k; algorithm=:auto)
  end

  @test is_unimodular(U6)
  @test is_unimodular(U6; algorithm=:CRT)
  @test is_unimodular(U6; algorithm=:pauderis_storjohann)
  @test is_unimodular(U6; algorithm=:auto)

  @test !is_unimodular(M)
  @test !is_unimodular(M; algorithm=:CRT)
  @test !is_unimodular(M; algorithm=:pauderis_storjohann)
  @test !is_unimodular(M; algorithm=:auto)
end
