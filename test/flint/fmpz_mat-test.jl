@testset "ZZMatrix.constructors" begin
   S = matrix_space(FlintZZ, 3, 3)

   @test elem_type(S) == ZZMatrix
   @test elem_type(ZZMatrixSpace) == ZZMatrix
   @test parent_type(ZZMatrix) == ZZMatrixSpace
   @test base_ring(S) == FlintZZ
   @test nrows(S) == 3
   @test ncols(S) == 3

   @test isa(S, ZZMatrixSpace)

   f = S(ZZRingElem(3))

   @test isa(f, MatElem)

   g = S(2)

   @test isa(g, MatElem)

   k = S([ZZRingElem(2) 3 5; 1 4 7; 9 6 3])

   @test isa(k, MatElem)

   k = S([2 3 5; 1 4 7; 9 6 3]')

   @test isa(k, MatElem)

   l = S(k)

   @test isa(l, MatElem)

   m = S()

   @test isa(m, MatElem)

   @test_throws ErrorConstrDimMismatch (S([ZZRingElem(1) 2; 3 4]))
   @test_throws ErrorConstrDimMismatch (S([ZZRingElem(1), 2, 3, 4]))
   @test_throws ErrorConstrDimMismatch (S([ZZRingElem(1) 2 3 4; 5 6 7 8; 1 2 3 4]))
   @test_throws ErrorConstrDimMismatch (S([ZZRingElem(1), 2, 3, 4, 5, 6, 7, 8, 1, 2, 3, 4]))

   arr = [1 2; 3 4]
   arr2 = [1, 2, 3, 4, 5, 6]

   for T in [ZZRingElem, Int, BigInt]
      M = matrix(FlintZZ, map(T, arr))
      @test isa(M, ZZMatrix)
      @test base_ring(M) == FlintZZ

      M2 = matrix(FlintZZ, 2, 3, map(T, arr2))
      @test isa(M2, ZZMatrix)
      @test base_ring(M2) == FlintZZ
      @test nrows(M2) == 2
      @test ncols(M2) == 3
      @test_throws ErrorConstrDimMismatch matrix(FlintZZ, 2, 2, map(T, arr2))
      @test_throws ErrorConstrDimMismatch matrix(FlintZZ, 2, 4, map(T, arr2))
   end

   M3 = zero_matrix(FlintZZ, 2, 3)

   @test isa(M3, ZZMatrix)
   @test base_ring(M3) == FlintZZ

   M4 = identity_matrix(FlintZZ, 3)

   @test isa(M4, ZZMatrix)
   @test base_ring(M4) == FlintZZ

   a = zero_matrix(FlintZZ, 2, 2)
   b = zero_matrix(FlintZZ, 2, 3)
   @test a in [a, b]
   @test a in [b, a]
   @test !(a in [b])
   @test a in keys(Dict(a => 1))
   @test !(a in keys(Dict(b => 1)))
end

@testset "ZZMatrix.$sim_zero" for sim_zero in (similar, zero)
   S = matrix_space(FlintZZ, 3, 3)
   s = S(ZZRingElem(3))

   t = sim_zero(s)
   @test t isa ZZMatrix
   @test size(t) == size(s)
   @test iszero(t)

   t = sim_zero(s, 2, 3)
   @test t isa ZZMatrix
   @test size(t) == (2, 3)
   @test iszero(t)

   for (R, M) in ring_to_mat
      t = sim_zero(s, R)
      @test size(t) == size(s)
      if sim_zero == zero
         @test iszero(t)
      end

      t = sim_zero(s, R, 2, 3)
      @test size(t) == (2, 3)
      if sim_zero == zero
         @test iszero(t)
      end
   end

   # issue #651
   m = one(Generic.MatSpace{ZZRingElem}(ZZ, 2, 2))
   for n = (m, -m, m*m, m+m, 2m)
      @test n isa Generic.MatSpaceElem{ZZRingElem}
   end
end

@testset "ZZMatrix.is_zero_entry" begin
   M = matrix(FlintZZ, [1 2 3;4 0 6;0 8 9])
   for i in 1:3, j in 1:3
      @test is_zero_entry(M, i, j) == (M[i, j] == 0)
   end
end

@testset "ZZMatrix.printing" begin
   S = matrix_space(FlintZZ, 3, 3)
   f = S(ZZRingElem(3))

   # test that default Julia printing is not used
   @test !occursin(string(typeof(f)), string(f))
end

@testset "ZZMatrix.convert" begin
   # Basic tests.
   A = [[1 2 3]; [4 5 6]]
   Abig = BigInt[[1 2 3]; [4 5 6]]
   S = matrix_space(FlintZZ, 2, 3)
   B = S(A)

   @test Matrix{Int}(B) == A
   @test Matrix{BigInt}(B) == Abig

   # Tests when elements do not fit a simple Int.
   B[1, 1] = 10000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000
   @test_throws ErrorException Matrix{Int}(B)
end

@testset "ZZMatrix.manipulation" begin
   S = matrix_space(FlintZZ, 3, 3)
   A = S([ZZRingElem(2) 3 5; 1 4 7; 9 6 3])
   B = S([ZZRingElem(1) 4 7; 9 6 7; 4 3 3])

   @test iszero(zero(S))
   @test isone(one(S))

   B[1, 1] = ZZRingElem(3)

   @test B[1, 1] == ZZRingElem(3)

   @test nrows(B) == 3
   @test ncols(B) == 3

   @test deepcopy(A) == A

   a = matrix(FlintZZ, 4, 4, [-1 ZZRingElem(2)^100 3 -4; 5 -1 ZZRingElem(2)^100 6; 7 5 -1 8; 9 10 11 12])
   @test hash(a, UInt(5)) == hash(deepcopy(a), UInt(5))
   @test hash(view(a, 1,1, 2,2)) == hash(view(a, 2,2, 3,3))
end

@testset "ZZMatrix.view" begin
   S = matrix_space(FlintZZ, 3, 3)

   A = S([1 2 3; 4 5 6; 7 8 9])

   B = @inferred view(A, 1, 1, 2, 2)

   @test typeof(B) == ZZMatrix
   @test B == matrix_space(FlintZZ, 2, 2)([1 2; 4 5])

   B[1, 1] = 10
   @test A[1, 1] == 10

   C = @inferred view(B, 1:2, 1:2)

   @test typeof(C) == ZZMatrix
   @test C == matrix_space(FlintZZ, 2, 2)([10 2; 4 5])

   C[1, 1] = 20
   @test B[1, 1] == 20
   @test A[1, 1] == 20

   A = 0
   GC.gc()

   @test B[1, 1] == 20

   A = S([1 2 3; 4 5 6; 7 8 9])
   v = @view A[2, :]
   @test length(v) == 3
   @test v[1] == 4
   v[2] = 7
   @test A == S([1 2 3; 4 7 6; 7 8 9])
   A = S([1 2 3; 4 5 6; 7 8 9])
   v = @view A[:, 3]
   @test v[3] == 9
   @test length(v) == 3
   v[1] = 1
   @test A == S([1 2 1; 4 5 6; 7 8 9])
end

@testset "ZZMatrix.sub" begin
   S = matrix_space(FlintZZ, 3, 3)

   A = S([1 2 3; 4 5 6; 7 8 9])

   B = @inferred sub(A, 1, 1, 2, 2)

   @test typeof(B) == ZZMatrix
   @test B == matrix_space(FlintZZ, 2, 2)([1 2; 4 5])

   B[1, 1] = 10
   @test A == S([1 2 3; 4 5 6; 7 8 9])

   C = @inferred sub(B, 1:2, 1:2)

   @test typeof(C) == ZZMatrix
   @test C == matrix_space(FlintZZ, 2, 2)([10 2; 4 5])

   C[1, 1] = 20
   @test B == matrix_space(FlintZZ, 2, 2)([10 2; 4 5])
   @test A == S([1 2 3; 4 5 6; 7 8 9])
end

@testset "ZZMatrix.unary_ops" begin
   S = matrix_space(FlintZZ, 3, 3)

   A = S([ZZRingElem(2) 3 5; 1 4 7; 9 6 3])
   B = S([ZZRingElem(-2) (-3) (-5); (-1) (-4) (-7); (-9) (-6) (-3)])

   @test -A == B
end

@testset "ZZMatrix.binary_ops" begin
   S = matrix_space(FlintZZ, 3, 3)

   A = S([ZZRingElem(2) 3 5; 1 4 7; 9 6 3])
   B = S([ZZRingElem(1) 4 7; 9 6 7; 4 3 3])

   @test A + B == S([3 7 12; 10 10 14; 13 9 6])
   @test A - B == S([1 (-1) (-2); (-8) (-2) 0; 5 3 0])
   @test A*B == S([49 41 50; 65 49 56; 75 81 114])

   @test A*ZZRingElem[1, 2, 3] == ZZRingElem[23, 30, 30]
   @test ZZRingElem[1, 2, 3]*A == ZZRingElem[31, 29, 28]
end

@testset "ZZMatrix.adhoc_binary" begin
   S = matrix_space(FlintZZ, 3, 3)

   A = S([ZZRingElem(2) 3 5; 1 4 7; 9 6 3])

   @test 12 + A == A + 12
   @test ZZRingElem(11) + A == A + ZZRingElem(11)
   @test A - 3 == -(3 - A)
   @test A - ZZRingElem(7) == -(ZZRingElem(7) - A)
   @test 3*A == A*3
   @test ZZRingElem(3)*A == A*ZZRingElem(3)
end

@testset "ZZMatrix.kronecker_product" begin
   S = matrix_space(ZZ, 2, 3)
   S2 = matrix_space(ZZ, 2, 2)
   S3 = matrix_space(ZZ, 3, 3)

   A = S(ZZRingElem[2 3 5; 9 6 3])
   B = S2(ZZRingElem[2 3; 1 4])
   C = S3(ZZRingElem[2 3 5; 1 4 7; 9 6 3])

   @test size(kronecker_product(A, A)) == (4,9)
   @test kronecker_product(B*A,A*C) == kronecker_product(B,A) * kronecker_product(A,C)
end

@testset "ZZMatrix.comparison" begin
   S = matrix_space(FlintZZ, 3, 3)

   A = S([ZZRingElem(2) 3 5; 1 4 7; 9 6 3])
   B = S([ZZRingElem(2) 3 5; 1 4 7; 9 6 3])

   @test A == B

   @test A != one(S)
end

@testset "ZZMatrix.adhoc_comparison" begin
   S = matrix_space(FlintZZ, 3, 3)

   A = S([ZZRingElem(2) 3 5; 1 4 7; 9 6 3])

   @test S(12) == 12
   @test S(5) == ZZRingElem(5)
   @test 12 == S(12)
   @test ZZRingElem(5) == S(5)
   @test A != one(S)
   @test one(S) == one(S)
end

@testset "ZZMatrix.powering" begin
   S = matrix_space(FlintZZ, 3, 3)

   A = S([ZZRingElem(2) 3 5; 1 4 7; 9 6 3])

   @test A^5 == A^2*A^3

   @test A^0 == one(S)

   @test_throws DomainError A^-1
end

@testset "ZZMatrix.adhoc_exact_division" begin
   S = matrix_space(FlintZZ, 3, 3)

   A = S([ZZRingElem(2) 3 5; 1 4 7; 9 6 3])

   @test divexact(5*A, 5) == A
   @test divexact(12*A, ZZRingElem(12)) == A
end

@testset "ZZMatrix.gram" begin
   S = matrix_space(FlintZZ, 3, 3)

   A = S([ZZRingElem(2) 3 5; 1 4 7; 9 6 3])

   @test gram(A) == S([38 49 51; 49 66 54; 51 54 126])
end

@testset "ZZMatrix.trace" begin
   S = matrix_space(FlintZZ, 3, 3)

   A = S([ZZRingElem(2) 3 5; 1 4 7; 9 6 3])

   @test tr(A) == 9
end

@testset "ZZMatrix.content" begin
   S = matrix_space(FlintZZ, 3, 3)

   A = S([ZZRingElem(2) 3 5; 1 4 7; 9 6 3])

   @test content(17*A) == 17
end

@testset "ZZMatrix.transpose" begin
   S = matrix_space(FlintZZ, 3, 3)

   A = S([ZZRingElem(2) 3 5; 1 4 7; 9 6 3])

   B = transpose(A) + A

   @test B == transpose(B)

   C = transpose(A)*A

   @test transpose(C) == C
end

@testset "ZZMatrix.row_col_swapping" begin
   a = matrix(FlintZZ, [1 2; 3 4; 5 6])

   @test swap_rows(a, 1, 3) == matrix(FlintZZ, [5 6; 3 4; 1 2])

   swap_rows!(a, 2, 3)

   @test a == matrix(FlintZZ, [1 2; 5 6; 3 4])

   @test swap_cols(a, 1, 2) == matrix(FlintZZ, [2 1; 6 5; 4 3])

   swap_cols!(a, 2, 1)

   @test a == matrix(FlintZZ, [2 1; 6 5; 4 3])

   a = matrix(FlintZZ, [1 2; 3 4])
   @test reverse_rows(a) == matrix(FlintZZ, [3 4; 1 2])
   reverse_rows!(a)
   @test a == matrix(FlintZZ, [3 4; 1 2])

   a = matrix(FlintZZ, [1 2; 3 4])
   @test reverse_cols(a) == matrix(FlintZZ, [2 1; 4 3])
   reverse_cols!(a)
   @test a == matrix(FlintZZ, [2 1; 4 3])

   a = matrix(FlintZZ, [1 2 3; 3 4 5; 5 6 7])

   @test reverse_rows(a) == matrix(FlintZZ, [5 6 7; 3 4 5; 1 2 3])
   reverse_rows!(a)
   @test a == matrix(FlintZZ, [5 6 7; 3 4 5; 1 2 3])

   a = matrix(FlintZZ, [1 2 3; 3 4 5; 5 6 7])
   @test reverse_cols(a) == matrix(FlintZZ, [3 2 1; 5 4 3; 7 6 5])
   reverse_cols!(a)
   @test a == matrix(FlintZZ, [3 2 1; 5 4 3; 7 6 5])
end

@testset "ZZMatrix.scaling" begin
   S = matrix_space(FlintZZ, 3, 3)

   A = S([ZZRingElem(2) 3 5; 1 4 7; 9 6 3])

   @test (A<<5)>>5 == A

   @test_throws DomainError (A<<-1)
   @test_throws DomainError (A>>-1)
end

@testset "ZZMatrix.inversion" begin
   S = matrix_space(FlintZZ, 3, 3)

   A = S([ZZRingElem(2) 3 5; 1 4 7; 9 2 2])
   B = S([-6 4 1; 61 (-41) (-9); -34 23 5])

   @test inv(inv(A)) == A

   @test inv(A) == B

   @test inv(A)*A == one(S)

   a = ZZ[1 1;]
   @test_throws ErrorException inv(a)
   b = ZZ[1 0; 0 2]
   @test_throws ErrorException inv(b)
   c = ZZ[1 1; 1 1]
   @test_throws ErrorException inv(c)
end

@testset "ZZMatrix.pseudo_inversion" begin
   S = matrix_space(FlintZZ, 3, 3)

   A = S([1 2 3; 1 2 3; 1 2 3])
   B = S([1 0 1; 2 3 1; 5 6 7])

   @test_throws ErrorException pseudo_inv(A)

   C, d = pseudo_inv(B)
   @test B*C == S(d)
end

@testset "ZZMatrix.exact_division" begin
   S = matrix_space(FlintZZ, 3, 3)

   A = S([ZZRingElem(2) 3 5; 1 4 7; 9 2 2])
   B = S([2 3 4; 7 9 1; 5 4 5])

   @test divexact(B*A, A) == B
end

@testset "ZZMatrix.modular_reduction" begin
   S = matrix_space(FlintZZ, 3, 3)

   A = S([ZZRingElem(2) 3 5; 1 4 7; 9 2 2])
   B = S([2 0 2; 1 1 1; 0 2 2])

   @test reduce_mod(A, 3) == B

   @test reduce_mod(A, ZZRingElem(3)) == B
end

@testset "ZZMatrix.det" begin
   S = matrix_space(FlintZZ, 3, 3)

   A = S([ZZRingElem(2) 3 5; 1 4 7; 19 3 7])

   @test det(A) == 27

   @test det_divisor(A) == 27

   @test det_given_divisor(A, 9) == 27

   @test det_given_divisor(A, ZZRingElem(9)) == 27
end

@testset "ZZMatrix.hadamard" begin
   S = matrix_space(FlintZZ, 4, 4)

   @test is_hadamard(hadamard(S))
end

@testset "ZZMatrix.fflu" begin
   for iters = 1:100
      m = rand(0:20)
      n = rand(0:20)

      rank = rand(0:min(m, n))
      S = matrix_space(ZZ, m, n)
      A = S()
      for i = 1:m
         for j = 1:n
            A[i, j] = rand(-10:10)
         end
      end

      r, d, P, L, U = fflu(A)

      V = matrix_space(QQ, m, m)
      D = V()
      if m >= 1
          D[1, 1] = 1//L[1, 1]
      end
      if m >= 2
         for j = 1:m - 1
            D[j + 1, j + 1] = (1//L[j, j])*(1//L[j + 1, j + 1])
         end
      end
      L2 = change_base_ring(QQ, L)
      U2 = change_base_ring(QQ, U)
      @test change_base_ring(QQ, P*A) == L2*D*U2
   end
end

@testset "ZZMatrix.hnf" begin
   S = matrix_space(FlintZZ, 3, 3)

   A = S([ZZRingElem(2) 3 5; 1 4 7; 19 3 7])

   B = S([1 0 0; 10 2 0; 0 0 4])

   @test hnf(A) == S([1 0 16; 0 1 18; 0 0 27])

   H, T = hnf_with_transform(A)

   @test T*A == H

   M = hnf_modular(A, ZZRingElem(27))

   @test is_hnf(M)

   MM = hnf_modular_eldiv(B, ZZRingElem(4))

   @test is_hnf(MM)
   @test S([1 0 0; 0 2 0; 0 0 4]) == MM
end

@testset "ZZMatrix.lll" begin
   S = matrix_space(FlintZZ, 3, 3)

   A = S([ZZRingElem(2) 3 5; 1 4 7; 19 3 7])

   @test lll(A) == S([-1 1 2; -1 (-2) 2; 4 1 1])

   L, T = lll_with_transform(A)

   @test T*A == L

   @test gram(L) == lll_gram(gram(A))

   G, T = lll_gram_with_transform(gram(A))

   @test G == gram(T*A)

   @test lll_with_removal(A, ZZRingElem(100)) == (3, S([-1 1 2; -1 (-2) 2; 4 1 1]))

   r, L, T = lll_with_removal_transform(A, ZZRingElem(100))

   @test T*A == L

   B = deepcopy(A)
   lll!(B)
   @test B == lll(A)

   B = gram(A)
   lll_gram!(B)
   @test B == lll_gram(gram(A))
end

@testset "ZZMatrix.nullspace" begin
   S = matrix_space(FlintZZ, 3, 3)
   T = matrix_space(FlintZZ, 3, 1)

   A = S([ZZRingElem(2) 3 5; 1 4 7; 4 1 1])

   @test nullspace(A) == (1, T([1; -9; 5]))

   r, N = nullspace(A)

   @test iszero(A*N)

   B = S([0 0 0; 0 0 0; 0 0 0])
   I = S([1 0 0; 0 1 0; 0 0 1])

   r, N = nullspace(B)
   @test r == 3 && N == I
end

@testset "ZZMatrix.rank" begin
   S = matrix_space(FlintZZ, 3, 3)

   A = S([ZZRingElem(2) 3 5; 1 4 7; 4 1 1])

   @test rank(A) == 2
end

@testset "ZZMatrix.rref" begin
   for iters = 1:100
      m = rand(0:100)
      n = rand(0:100)
      S = matrix_space(FlintZZ, m, n)
      M = rand(S, -100:100)
      r, N, d = rref_rational(M)

      @test is_rref(N)

      N2 = change_base_ring(QQ, N)
      N2 = divexact(N2, d)

      @test is_rref(N2)
   end 

   S = matrix_space(FlintZZ, 3, 3)

   A = S([ZZRingElem(2) 3 5; 1 4 7; 4 1 1])

   r, B, d = rref_rational(A)

   @test (B, d) == (S([5 0 (-1); 0 5 9; 0 0 0]), 5)
   @test r == 2
end

@testset "ZZMatrix.snf" begin
   S = matrix_space(FlintZZ, 3, 3)

   A = S([ZZRingElem(2) 3 5; 1 4 7; 19 3 7])

   @test snf(A) == S([1 0 0; 0 1 0; 0 0 27])

   @test is_snf(snf(A))

   B = S([ZZRingElem(2) 0 0; 0 4 0; 0 0 7])

   @test is_snf(snf_diagonal(B))
end

@testset "ZZMatrix._solve_rational" begin
   S = matrix_space(FlintZZ, 3, 3)

   A = S([ZZRingElem(2) 3 5; 1 4 7; 9 2 2])

   T = matrix_space(FlintZZ, 3, 1)

   B = T([ZZRingElem(4), 5, 7])

   X, d = Nemo._solve_rational(A, B)

   @test (X, d) == (T([3, -24, 14]), 1)

   @test d == 1

   @test A*X == B

   (Y, k) = Nemo._solve_dixon(A, B)

   @test reduce_mod(Y, k) == reduce_mod(X, k)
end

@testset "ZZMatrix.solve" begin
   A = matrix(ZZ, 2, 2, [1,2,3,4])
   b = matrix(ZZ, 1, 2, [1, 6])
   @test Nemo._solve_triu_left(A, b) == matrix(ZZ, 1, 2, [1, 1])
   b = matrix(ZZ, 2, 1, [3, 4])
   @test Nemo._solve_triu(A, b) == matrix(ZZ, 2, 1, [1, 1])
   b = matrix(ZZ, 2, 1, [1, 7])
   c = similar(b)
   AbstractAlgebra._solve_tril!(c, A, b)
   @test c == matrix(ZZ, 2, 1, [1, 1])

   S = matrix_space(FlintZZ, 3, 3)

   A = S([ZZRingElem(2) 3 5; 1 4 7; 9 2 2])

   T = matrix_space(FlintZZ, 3, 1)

   B = T([ZZRingElem(4), 5, 7])

   X = solve(A, B, side = :right)

   @test X == T([3, -24, 14])
   @test A*X == B

   fl, X = can_solve_with_solution(A, B, side = :right)
   @test fl && A * X == B

   A = matrix(ZZ, 2, 2, [1, 0, 0, 0])
   B = matrix(ZZ, 2, 2, [0, 0, 0, 1])
   fl, X = can_solve_with_solution(A, B, side = :right)
   @test !fl
   fl, X = can_solve_with_solution(A, B)
   @test !fl
   fl, X, K = can_solve_with_solution_and_kernel(A, B, side = :right)
   @test !fl

   A = matrix(ZZ, 2, 2, [1, 0, 0, 0])
   B = matrix(ZZ, 2, 2, [0, 1, 0, 0])
   fl, X, Z = can_solve_with_solution_and_kernel(A, B, side = :right)
   @test fl && A*X == B && iszero(A * Z)

   # Non-square example
   A = matrix(ZZ, [1 2 3; 4 5 6])
   B = matrix(ZZ, 2, 1, [1, 1])
   fl, x, K = can_solve_with_solution_and_kernel(A, B, side = :right)
   @test fl
   @test A*x == B
   @test is_zero(A*K)
   @test ncols(K) + rank(A) == ncols(A)

   B = matrix(ZZ, 1, 3, [1, 2, 3])
   fl, x, K = can_solve_with_solution_and_kernel(A, B)
   @test fl
   @test x*A == B
   @test is_zero(K*A)
   @test nrows(K) + rank(A) == nrows(A)

   C = solve_init(A)
   B = matrix(ZZ, 2, 1, [1, 1])
   fl, x, K = can_solve_with_solution_and_kernel(C, B, side = :right)
   @test fl
   @test A*x == B
   @test is_zero(A*K)
   @test ncols(K) + rank(A) == ncols(A)

   B = matrix(ZZ, 1, 3, [1, 2, 3])
   fl, x, K = can_solve_with_solution_and_kernel(C, B)
   @test fl
   @test x*A == B
   @test is_zero(K*A)
   @test nrows(K) + rank(A) == nrows(A)
end

@testset "ZZMatrix.kernel" begin
   A = matrix(ZZ, [ 1 2 3 ; 4 5 6 ])
   K = @inferred kernel(A, side = :right)
   @test is_zero(A*K)
   @test ncols(K) == 1

   K = @inferred kernel(A)
   @test is_zero(K*A)
   @test nrows(K) == 0

   A = transpose(A)
   K = @inferred kernel(A)
   @test is_zero(K*A)
   @test nrows(K) == 1

   K = @inferred kernel(zero_matrix(ZZ, 2, 2), side = :right)
   @test ncols(K) == 2
   @test hnf(K) == identity_matrix(ZZ, 2)

   K = @inferred kernel(zero_matrix(ZZ, 2, 2))
   @test nrows(K) == 2
   @test hnf(K) == identity_matrix(ZZ, 2)
end

@testset "ZZMatrix.concat" begin
   S = matrix_space(FlintZZ, 3, 3)
   T = matrix_space(FlintZZ, 3, 6)
   U = matrix_space(FlintZZ, 6, 3)

   A = S([ZZRingElem(2) 3 5; 1 4 7; 9 6 3])
   B = S([ZZRingElem(1) 4 7; 9 6 7; 4 3 3])
   C = matrix(ZZ, 2, 2, [1, 2, 3, 4])

   @test hcat(A, B) == T([2 3 5 1 4 7; 1 4 7 9 6 7; 9 6 3 4 3 3])

   @test vcat(A, B) == U([2 3 5; 1 4 7; 9 6 3; 1 4 7; 9 6 7; 4 3 3])

   @test [A B] == hcat(A, B)
   @test [A A A] == hcat(A, hcat(A, A))
   @test_throws ErrorException [A C]
   @test_throws ErrorException [A A C]
   @test [A; B] == vcat(A, B)
   @test [A; B; A] == vcat(A, vcat(B, A))
   @test_throws ErrorException [A; A; C]
   @test cat(A, A, dims = (1, 2)) == block_diagonal_matrix([A, A])
   @test cat(A, A, dims = 1) == hcat(A, A)
   @test reduce(hcat, [A, A]) == hcat(A, A) # -> _hcat
   @test reduce(vcat, [A, A]) == vcat(A, A) # -> _vcat
   @test cat(A, A, dims = 2) == vcat(A, A)
   @test_throws ErrorException cat(A, A, dims = 3)
end

@testset "ZZMatrix.rand" begin
   S = matrix_space(FlintZZ, 3, 3)
   M = rand(S, 1:9)
   @test parent(M) == S
   for i=1:3, j=1:3
      @test M[i, j] in 1:9
   end
end
