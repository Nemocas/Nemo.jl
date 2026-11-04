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
  @test (@inferred diagonal(A)) == [QQ(1), QQ(5)]
  A = transpose(A)
  @test (@inferred diagonal(A)) == [QQ(1), QQ(5)]

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
  E4 = eigenvalues(QQ, A4)
  @test length(E4) == 2
  @test issetequal(E4, [QQ(2),QQ(4)])
  E4m = eigenvalues_with_multiplicities(QQ,A4)
  @test length(E4m) == 2
  @test issetequal(E4m, [(QQ(2),1), (QQ(4),1)])


  A5 = matrix(ZZ,5,5, [0,0,1,-1,-1,  -1,-1,1,1,-1,  0,0,1,-1,-1,  -1,1,1,-1,-1,  1,1,-1,-1,1])
  E5 = eigenvalues(QQ,A5)
  @test length(E5) == 3
  @test issetequal(E5, [QQ(0),QQ(1),QQ(-2)])

  E5m = eigenvalues_with_multiplicities(QQ,A5)
  @test length(E5m) == 3
  @test issetequal(E5m, [(QQ(0),2), (QQ(1),2), (QQ(-2),1)])


  A6 = matrix(ZZ,6,6, [0,0,-1,0,1,0,  0,0,0,1,-1,0,  -1,0,0,0,1,0,  0,1,1,1,-1,0,  1,-1,1,1,-1,0,  1,-1,0,0,-1,-1])
  E6 = eigenvalues(QQ,A6)
  @test length(E6) == 2
  @test issetequal(E6, [QQ(1),QQ(-1)])

  E6m = eigenvalues_with_multiplicities(QQ,A6)
  @test length(E6m) == 2
  @test issetequal(E6m, [(QQ(-1),2), (QQ(1),2)])


  A6b = matrix(ZZ,6,6, [0,1,0,-1,1,0,  1,1,-2,0,-2,0,  -1,0,1,1,1,0,  1,-1,2,1,0,0,  1,1,-2,-2,-1,-2,  2,-2,0,2,-2,0])
  E6b = eigenvalues(QQ,A6b)
  @test length(E6b) == 2
  @test issetequal(E6b, [QQ(1),QQ(-1)])

  E6bm = eigenvalues_with_multiplicities(QQ,A6b)
  @test length(E6bm) == 2
  @test issetequal(E6bm, [(QQ(-1),1), (QQ(1),2)])

  # See OSCAR issue #4388 (and subsequent PR #2108)
  M_2x2 = matrix(ZZ, [0 1; -1 0])  # important here: ZZ is *not* a field
  @test length(eigenvalues(M_2x2)) == 0
  @test length(eigenvalues(QQ, M_2x2)) == 0
  @test_throws  ArgumentError  eigenvalues_with_multiplicities(M_2x2)
  @test  length(eigenvalues_with_multiplicities(QQ,M_2x2)) == 0
  MQQ_2x2 = change_base_ring(QQ, M_2x2)
  @test length(eigenvalues(MQQ_2x2)) == 0
  @test  length(eigenvalues_with_multiplicities(MQQ_2x2)) == 0
  K = algebraic_closure(QQ)
  lambda1, lambda2 = eigenvalues(K, M_2x2)
  MK_2x2 = matrix(K, M_2x2)
  @test_broken length(eigenvalues(MK_2x2)) == 2  # requires factor which is outside Nemo

  @test_throws  ArgumentError  eigenspaces(M_2x2)
  @test  length(eigenspaces(QQ,M_2x2)) == 0
  @test  length(eigenspaces(MQQ_2x2)) == 0
  @test length(eigenspaces(K, M_2x2)) == 2
  @test size(eigenspace(M_2x2, lambda1)) == (1,2)

  M_3x3 = matrix(ZZ, [[5, -2, -2],  [2, 1, -2],  [2, -2, 1]])
  MQQ_3x3 = change_base_ring(QQ, M_3x3)
  @test length(eigenvalues(QQ, M_3x3)) == 2
  @test length(eigenvalues(MQQ_3x3)) == 2
  @test length(eigenspaces(QQ, M_3x3)) == 2
  @test length(eigenspaces(MQQ_3x3)) == 2
  @test_throws ArgumentError  eigenspace(M_3x3, 1)
  @test_throws ArgumentError  eigenspace(M_3x3, ZZ(1))
  @test_throws ArgumentError  eigenspace(M_3x3, 1.0)
  @test size(eigenspace(M_3x3, 1//1)) == (1,3)
  @test size(eigenspace(M_3x3, QQ(1))) == (1,3)
  @test size(eigenspace(M_3x3, QQ(2))) == (0,3)
  @test size(eigenspace(M_3x3, QQ(3))) == (2,3)
  @test size(eigenspace(MQQ_3x3, ZZ(1))) == (1,3)
  @test size(eigenspace(MQQ_3x3, 1)) == (1,3)
  @test size(eigenspace(MQQ_3x3, 2)) == (0,3)
  @test size(eigenspace(MQQ_3x3, 3)) == (2,3)
  @test size(eigenspace(MQQ_3x3, 3//1)) == (2,3)
  @test size(eigenspace(MQQ_3x3, 3.0)) == (2,3)

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


  @test_throws DomainError is_unimodular(matrix(ZZ,0,1,[]))
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

# A 3x4 matrix of every FlintMatElem type, FqMatrix once for each kind of
# fq_default context.
function flint_mat_examples()
  Fqs = [GF(3, 2), GF(1048583, 2), GF(ZZ(10)^30 + 57, 2), GF(7), GF(ZZ(10)^30 + 57)]
  @test allunique(Nemo._fq_default_ctx_type.(Fqs))

  rings = [ZZ, QQ, residue_ring(ZZ, 12)[1], residue_ring(ZZ, ZZ(12))[1],
           Native.GF(7), Native.GF(ZZ(7)),
           Native.finite_field(7, 2, :a)[1], Native.finite_field(ZZ(7), 2, :a)[1],
           RealField(), ArbField(64), ComplexField(), AcbField(64), Fqs...]
  mats = Any[matrix(R, [1 2 3 4; 5 6 7 8; 9 10 11 12]) for R in rings]
  push!(mats, Nemo.ZZPolyRingMatrix(matrix(ZZ[:x][1], [1 2 3 4; 5 6 7 8; 9 10 11 12])))
  return mats
end

@testset "Methods shared by all FlintMatElem" begin
  P = Perm([2, 3, 1])
  Q = Perm([2, 3, 4, 1])
  for A in flint_mat_examples()
    @test Nemo.Solve.lazy_transpose(A) isa typeof(A)
    @test sprint(summary, A) == "3x4 $(typeof(A))"
    @test getindex!(base_ring(A)(), A, 2, 3) == A[2, 3]

    PA = P * A
    AQ = A * Q
    @test PA isa typeof(A) && AQ isa typeof(A)
    @test all(PA[P[i], j] == A[i, j] for i in 1:3, j in 1:4)
    @test all(AQ[i, Q[j]] == A[i, j] for i in 1:3, j in 1:4)

    # the degree of the permutation has to match
    @test_throws DimensionMismatch Q * A
    @test_throws DimensionMismatch A * P
  end
end

@testset "Access through Ref and Ptr" begin
  mats = flint_mat_examples()
  # the stride of a view differs from its number of columns
  views = [view(A, 2:3, 2:3) for A in mats]
  @test all(A -> size(A) == (3, 4), mats)
  @test all(A -> size(A) == (2, 2), views)

  for A in vcat(mats, views)
    GC.@preserve A begin
      for a in (Ref(A), Ptr{typeof(A)}(pointer_from_objref(A)))
        @test nrows(a) == nrows(A)
        @test ncols(a) == ncols(A)
        @test is_square(a) == is_square(A)
        A isa FqMatrix && continue
        @test Nemo.mat_entry_ptr(a, 2, 2) == Nemo.mat_entry_ptr(A, 2, 2)
      end
    end
  end
end

@testset "Views of FlintMatElem" begin
  @testset "$(typeof(A))" for A in flint_mat_examples()
    T = typeof(A)
    R = base_ring(A)
    A0 = deepcopy(A)

    V = @inferred view(A, 2:3, 2:4)
    @test V isa T
    @test size(V) == (2, 3)
    @test base_ring(V) == R
    @test all(V[i, j] == A[i + 1, j + 1] for i in 1:2, j in 1:3)

    # a view of a view; the stride of neither is its number of columns
    W = @inferred view(V, 2:2, 2:3)
    @test W isa T
    @test size(W) == (1, 2)
    @test W[1, 1] == A[3, 3] && W[1, 2] == A[3, 4]

    # FLINT takes a view wherever it takes a matrix; nmod_mat_neg is what
    # reads the modulus of its argument
    U = view(A, 1:3, 2:3)
    @test -V == -A0[2:3, 2:4]
    @test V + V == A0[2:3, 2:4] + A0[2:3, 2:4]
    @test V * U == A0[2:3, 2:4] * A0[1:3, 2:3]

    # a write through a view is seen in the parent
    V[1, 2] = R(42)
    @test A[2, 3] == R(42)
    W[1, 1] = R(43)
    @test A[3, 3] == R(43)
    @test V[2, 2] == R(43)

    # so is a write through a row or a column; of a non-integer where the
    # ring has one
    z = R isa Field ? inv(R(2)) : R(44)
    view(A, 2, :)[4] = z
    @test A[2, 4] == z
    view(A, :, 3)[1] = z + 1
    @test A[1, 3] == z + 1

    # `sub` and indexing with ranges copy
    A1 = deepcopy(A)
    for S in (sub(A, 2:3, 2:4), sub(A, 2, 2, 3, 4), A[2:3, 2:4])
      @test S isa T
      @test S == V
      S[1, 2] = R(5)
      @test A == A1
    end

    @test view(A, :, :) == A
    @test view(A, 2:2, :) == A[2:2, :]
    @test view(A, :, 3:4) == A[:, 3:4]
    @test collect(view(A, 2, :)) == [A[2, j] for j in 1:4]
    @test collect(view(A, :, 3)) == [A[i, 3] for i in 1:3]

    @test size(view(A, 2:1, 1:4)) == (0, 4)
    @test size(view(A, 1:3, 3:2)) == (3, 0)
    @test size(view(A, 5:4, 6:5)) == (0, 0)
    @test_throws BoundsError view(A, 1:4, 1:4)
    @test_throws BoundsError view(A, 0:1, 1:4)
    @test_throws BoundsError view(A, 1:3, 2:5)

    Z = zero(A, 0, 0)
    @test size(view(Z, 1:0, 1:0)) == (0, 0)
    @test size(view(Z, :, :)) == (0, 0)
    @test_throws BoundsError view(Z, 1:1, 1:0)

    # a view keeps its parent alive
    V = view(deepcopy(A0), 2:3, 2:4)
    GC.gc()
    @test V == A0[2:3, 2:4]
  end
end

# FLINT's `*_mat_window_init`, zero based as in C
for (T, f) in ((ZZMatrix, :fmpz_mat_window_init), (QQMatrix, :fmpq_mat_window_init),
               (zzModMatrix, :nmod_mat_window_init), (fpMatrix, :nmod_mat_window_init),
               (RealMatrix, :arb_mat_window_init), (ArbMatrix, :arb_mat_window_init),
               (ComplexMatrix, :acb_mat_window_init), (AcbMatrix, :acb_mat_window_init),
               (Nemo.ZZPolyRingMatrix, :fmpz_poly_mat_window_init))
  @eval function flint_window_init!(W::$T, A::$T, r1::Int, c1::Int, r2::Int, c2::Int)
    @ccall Nemo.libflint.$f(W::Ref{$T}, A::Ref{$T}, r1::Int, c1::Int, r2::Int, c2::Int)::Nothing
  end
end

for (T, f, C, ctx) in ((ZZModMatrix, :fmpz_mod_mat_window_init, Nemo.fmpz_mod_ctx_struct, :(base_ring(A).ninv)),
                       (FpMatrix, :fmpz_mod_mat_window_init, Nemo.fmpz_mod_ctx_struct, :(base_ring(A).ninv)),
                       (fqPolyRepMatrix, :fq_nmod_mat_window_init, fqPolyRepField, :(base_ring(A))),
                       (FqPolyRepMatrix, :fq_mat_window_init, FqPolyRepField, :(base_ring(A))),
                       (FqMatrix, :fq_default_mat_window_init, FqField, :(base_ring(A))))
  @eval function flint_window_init!(W::$T, A::$T, r1::Int, c1::Int, r2::Int, c2::Int)
    @ccall Nemo.libflint.$f(W::Ref{$T}, A::Ref{$T}, r1::Int, c1::Int, r2::Int, c2::Int, $ctx::Ref{$C})::Nothing
  end
end

# the first `k` words of the FLINT struct of `A`
struct_words(A, k) = GC.@preserve A [unsafe_load(Ptr{UInt}(pointer_from_objref(A)), i) for i in 1:k]

# Nemo builds a view without calling FLINT and gives it no finalizer. This test
# guards against a future FLINT whose `*_mat_window_init` sets up a different
# struct, for example one owning memory that `*_mat_window_clear` has to free.
@testset "Views are FLINT windows" begin
  for A in flint_mat_examples()
    # an nmod_mat has its modulus after `entries, r, c, stride`
    nmod = A isa Union{zzModMatrix, fpMatrix} ||
           (A isa FqMatrix && Nemo._fq_default_ctx_type(base_ring(A)) == Nemo.FlintC.FQ_DEFAULT_NMOD)
    k = nmod ? 7 : 4

    # the stride of `B` differs from its number of columns
    B = view(A, 2:3, 2:4)
    for (M, r, c) in ((A, 2:3, 2:4), (A, 1:3, 1:4), (B, 2:2, 2:3))
      W = typeof(M)()
      flint_window_init!(W, M, first(r) - 1, first(c) - 1, last(r), last(c))
      @test struct_words(W, k) == struct_words(view(M, r, c), k)
    end
  end
end
