@testset "fmpz_mat.constructors..." begin
   S = MatrixSpace(FlintZZ, 3, 3)

   @test elem_type(S) == fmpz_mat
   @test elem_type(FmpzMatSpace) == fmpz_mat
   @test parent_type(fmpz_mat) == FmpzMatSpace
   @test base_ring(S) == FlintZZ
   @test nrows(S) == 3
   @test ncols(S) == 3

   @test isa(S, FmpzMatSpace)

   f = S(fmpz(3))

   @test isa(f, MatElem)

   g = S(2)

   @test isa(g, MatElem)

   k = S([fmpz(2) 3 5; 1 4 7; 9 6 3])

   @test isa(k, MatElem)

   k = S([2 3 5; 1 4 7; 9 6 3]')

   @test isa(k, MatElem)

   l = S(k)

   @test isa(l, MatElem)

   m = S()

   @test isa(m, MatElem)

   @test_throws ErrorConstrDimMismatch (S([fmpz(1) 2; 3 4]))
   @test_throws ErrorConstrDimMismatch (S([fmpz(1), 2, 3, 4]))
   @test_throws ErrorConstrDimMismatch (S([fmpz(1) 2 3 4; 5 6 7 8; 1 2 3 4]))
   @test_throws ErrorConstrDimMismatch (S([fmpz(1), 2, 3, 4, 5, 6, 7, 8, 1, 2, 3, 4]))

   arr = [1 2; 3 4]
   arr2 = [1, 2, 3, 4, 5, 6]

   for T in [fmpz, Int, BigInt]
      M = matrix(FlintZZ, map(T, arr))
      @test isa(M, fmpz_mat)
      @test M.base_ring == FlintZZ

      M2 = matrix(FlintZZ, 2, 3, map(T, arr2))
      @test isa(M2, fmpz_mat)
      @test M2.base_ring == FlintZZ
      @test nrows(M2) == 2
      @test ncols(M2) == 3
      @test_throws ErrorConstrDimMismatch matrix(FlintZZ, 2, 2, map(T, arr2))
      @test_throws ErrorConstrDimMismatch matrix(FlintZZ, 2, 4, map(T, arr2))
   end

   M3 = zero_matrix(FlintZZ, 2, 3)

   @test isa(M3, fmpz_mat)
   @test M3.base_ring == FlintZZ

   M4 = identity_matrix(FlintZZ, 3)

   @test isa(M4, fmpz_mat)
   @test M4.base_ring == FlintZZ

   a = zero_matrix(FlintZZ, 2, 2)
   b = zero_matrix(FlintZZ, 2, 3)
   @test a in [a, b]
   @test a in [b, a]
   @test !(a in [b])
   @test a in keys(Dict(a => 1))
   @test !(a in keys(Dict(b => 1)))
end

@testset "fmpz_mat.similar..." begin
   S = MatrixSpace(FlintZZ, 3, 3)
   s = S(fmpz(3))

   t = similar(s)
   @test t isa fmpz_mat
   @test size(t) == size(s)

   t = similar(s, 2, 3)
   @test t isa fmpz_mat
   @test size(t) == (2, 3)

   for (R, M) in ring_to_mat
      t = similar(s, R)
      @test t isa M
      @test size(t) == size(s)

      t = similar(s, R, 2, 3)
      @test t isa M
      @test size(t) == (2, 3)
   end
end

@testset "fmpz_mat.printing..." begin
   S = MatrixSpace(FlintZZ, 3, 3)
   f = S(fmpz(3))

   # test that default Julia printing is not used
   @test !occursin(string(typeof(f)), string(f))
end

@testset "fmpz_mat.convert..." begin
   # Basic tests.
   A = [[1 2 3]; [4 5 6]]
   Abig = BigInt[[1 2 3]; [4 5 6]]
   S = MatrixSpace(FlintZZ, 2, 3)
   B = S(A)

   @test Matrix{Int}(B) == A
   @test Matrix{BigInt}(B) == Abig

   # Tests when elements do not fit a simple Int.
   B[1, 1] = 10000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000
   @test_throws ErrorException Matrix{Int}(B)
end

@testset "fmpz_mat.manipulation..." begin
   S = MatrixSpace(FlintZZ, 3, 3)
   A = S([fmpz(2) 3 5; 1 4 7; 9 6 3])
   B = S([fmpz(1) 4 7; 9 6 7; 4 3 3])

   @test iszero(zero(S))
   @test isone(one(S))

   B[1, 1] = fmpz(3)

   @test B[1, 1] == fmpz(3)

   @test nrows(B) == 3
   @test ncols(B) == 3

   @test deepcopy(A) == A
end

@testset "fmpz_mat.view..." begin
   S = MatrixSpace(FlintZZ, 3, 3)

   A = S([1 2 3; 4 5 6; 7 8 9])

   B = @inferred view(A, 1, 1, 2, 2)

   @test typeof(B) == fmpz_mat
   @test B == MatrixSpace(FlintZZ, 2, 2)([1 2; 4 5])

   B[1, 1] = 10
   @test A[1, 1] == 10

   C = @inferred view(B, 1:2, 1:2)

   @test typeof(C) == fmpz_mat
   @test C == MatrixSpace(FlintZZ, 2, 2)([10 2; 4 5])

   C[1, 1] = 20
   @test B[1, 1] == 20
   @test A[1, 1] == 20

   A = 0
   GC.gc()

   @test B[1, 1] == 20
end

@testset "fmpz_mat.sub..." begin
   S = MatrixSpace(FlintZZ, 3, 3)

   A = S([1 2 3; 4 5 6; 7 8 9])

   B = @inferred sub(A, 1, 1, 2, 2)

   @test typeof(B) == fmpz_mat
   @test B == MatrixSpace(FlintZZ, 2, 2)([1 2; 4 5])

   B[1, 1] = 10
   @test A == S([1 2 3; 4 5 6; 7 8 9])

   C = @inferred sub(B, 1:2, 1:2)

   @test typeof(C) == fmpz_mat
   @test C == MatrixSpace(FlintZZ, 2, 2)([10 2; 4 5])

   C[1, 1] = 20
   @test B == MatrixSpace(FlintZZ, 2, 2)([10 2; 4 5])
   @test A == S([1 2 3; 4 5 6; 7 8 9])
end

@testset "fmpz_mat.unary_ops..." begin
   S = MatrixSpace(FlintZZ, 3, 3)

   A = S([fmpz(2) 3 5; 1 4 7; 9 6 3])
   B = S([fmpz(-2) (-3) (-5); (-1) (-4) (-7); (-9) (-6) (-3)])

   @test -A == B
end

@testset "fmpz_mat.binary_ops..." begin
   S = MatrixSpace(FlintZZ, 3, 3)

   A = S([fmpz(2) 3 5; 1 4 7; 9 6 3])
   B = S([fmpz(1) 4 7; 9 6 7; 4 3 3])

   @test A + B == S([3 7 12; 10 10 14; 13 9 6])

   @test A - B == S([1 (-1) (-2); (-8) (-2) 0; 5 3 0])

   @test A*B == S([49 41 50; 65 49 56; 75 81 114])
end

@testset "fmpz_mat.adhoc_binary..." begin
   S = MatrixSpace(FlintZZ, 3, 3)

   A = S([fmpz(2) 3 5; 1 4 7; 9 6 3])

   @test 12 + A == A + 12
   @test fmpz(11) + A == A + fmpz(11)
   @test A - 3 == -(3 - A)
   @test A - fmpz(7) == -(fmpz(7) - A)
   @test 3*A == A*3
   @test fmpz(3)*A == A*fmpz(3)
end

@testset "fmpz_mat.kronecker_product..." begin
   S = MatrixSpace(ZZ, 2, 3)
   S2 = MatrixSpace(ZZ, 2, 2)
   S3 = MatrixSpace(ZZ, 3, 3)

   A = S(fmpz[2 3 5; 9 6 3])
   B = S2(fmpz[2 3; 1 4])
   C = S3(fmpz[2 3 5; 1 4 7; 9 6 3])

   @test size(kronecker_product(A, A)) == (4,9)
   @test kronecker_product(B*A,A*C) == kronecker_product(B,A) * kronecker_product(A,C)
end

@testset "fmpz_mat.comparison..." begin
   S = MatrixSpace(FlintZZ, 3, 3)

   A = S([fmpz(2) 3 5; 1 4 7; 9 6 3])
   B = S([fmpz(2) 3 5; 1 4 7; 9 6 3])

   @test A == B

   @test A != one(S)
end

@testset "fmpz_mat.adhoc_comparison..." begin
   S = MatrixSpace(FlintZZ, 3, 3)

   A = S([fmpz(2) 3 5; 1 4 7; 9 6 3])

   @test S(12) == 12
   @test S(5) == fmpz(5)
   @test 12 == S(12)
   @test fmpz(5) == S(5)
   @test A != one(S)
   @test one(S) == one(S)
end

@testset "fmpz_mat.powering..." begin
   S = MatrixSpace(FlintZZ, 3, 3)

   A = S([fmpz(2) 3 5; 1 4 7; 9 6 3])

   @test A^5 == A^2*A^3

   @test A^0 == one(S)

   @test_throws DomainError A^-1
end

@testset "fmpz_mat.adhoc_exact_division..." begin
   S = MatrixSpace(FlintZZ, 3, 3)

   A = S([fmpz(2) 3 5; 1 4 7; 9 6 3])

   @test divexact(5*A, 5) == A
   @test divexact(12*A, fmpz(12)) == A
end

@testset "fmpz_mat.gram..." begin
   S = MatrixSpace(FlintZZ, 3, 3)

   A = S([fmpz(2) 3 5; 1 4 7; 9 6 3])

   @test gram(A) == S([38 49 51; 49 66 54; 51 54 126])
end

@testset "fmpz_mat.trace..." begin
   S = MatrixSpace(FlintZZ, 3, 3)

   A = S([fmpz(2) 3 5; 1 4 7; 9 6 3])

   @test tr(A) == 9
end

@testset "fmpz_mat.content..." begin
   S = MatrixSpace(FlintZZ, 3, 3)

   A = S([fmpz(2) 3 5; 1 4 7; 9 6 3])

   @test content(17*A) == 17
end

@testset "fmpz_mat.transpose..." begin
   S = MatrixSpace(FlintZZ, 3, 3)

   A = S([fmpz(2) 3 5; 1 4 7; 9 6 3])

   B = transpose(A) + A

   @test B == transpose(B)

   C = transpose(A)*A

   @test transpose(C) == C
end

@testset "fmpz_mat.row_col_swapping..." begin
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

@testset "fmpz_mat.scaling..." begin
   S = MatrixSpace(FlintZZ, 3, 3)

   A = S([fmpz(2) 3 5; 1 4 7; 9 6 3])

   @test (A<<5)>>5 == A

   @test_throws DomainError (A<<-1)
   @test_throws DomainError (A>>-1)
end

@testset "fmpz_mat.inversion..." begin
   S = MatrixSpace(FlintZZ, 3, 3)

   A = S([fmpz(2) 3 5; 1 4 7; 9 2 2])
   B = S([-6 4 1; 61 (-41) (-9); -34 23 5])

   @test inv(inv(A)) == A

   @test inv(A) == B

   @test inv(A)*A == one(S)
end

@testset "fmpz_mat.pseudo_inversion..." begin
   S = MatrixSpace(FlintZZ, 3, 3)

   A = S([1 2 3; 1 2 3; 1 2 3])
   B = S([1 0 1; 2 3 1; 5 6 7])

   @test_throws ErrorException pseudo_inv(A)

   C, d = pseudo_inv(B)
   @test B*C == S(d)
end

@testset "fmpz_mat.exact_division..." begin
   S = MatrixSpace(FlintZZ, 3, 3)

   A = S([fmpz(2) 3 5; 1 4 7; 9 2 2])
   B = S([2 3 4; 7 9 1; 5 4 5])

   @test divexact(B*A, A) == B
end

@testset "fmpz_mat.modular_reduction..." begin
   S = MatrixSpace(FlintZZ, 3, 3)

   A = S([fmpz(2) 3 5; 1 4 7; 9 2 2])
   B = S([2 0 2; 1 1 1; 0 2 2])

   @test reduce_mod(A, 3) == B

   @test reduce_mod(A, fmpz(3)) == B
end

@testset "fmpz_mat.det..." begin
   S = MatrixSpace(FlintZZ, 3, 3)

   A = S([fmpz(2) 3 5; 1 4 7; 19 3 7])

   @test det(A) == 27

   @test det_divisor(A) == 27

   @test det_given_divisor(A, 9) == 27

   @test det_given_divisor(A, fmpz(9)) == 27
end

@testset "fmpz_mat.hadamard..." begin
   S = MatrixSpace(FlintZZ, 4, 4)

   @test ishadamard(hadamard(S))
end

@testset "fmpz_mat.hnf..." begin
   S = MatrixSpace(FlintZZ, 3, 3)

   A = S([fmpz(2) 3 5; 1 4 7; 19 3 7])

   B = S([1 0 0; 10 2 0; 0 0 4])

   @test hnf(A) == S([1 0 16; 0 1 18; 0 0 27])

   H, T = hnf_with_transform(A)

   @test T*A == H

   M = hnf_modular(A, fmpz(27))

   @test ishnf(M)

   MM = hnf_modular_eldiv(B, fmpz(4))

   @test ishnf(MM)
   @test S([1 0 0; 0 2 0; 0 0 4]) == MM
end

@testset "fmpz_mat.lll..." begin
   S = MatrixSpace(FlintZZ, 3, 3)

   A = S([fmpz(2) 3 5; 1 4 7; 19 3 7])

   @test lll(A) == S([-1 1 2; -1 (-2) 2; 4 1 1])

   L, T = lll_with_transform(A)

   @test T*A == L

   @test gram(L) == lll_gram(gram(A))

   G, T = lll_gram_with_transform(gram(A))

   @test G == gram(T*A)

   @test lll_with_removal(A, fmpz(100)) == (3, S([-1 1 2; -1 (-2) 2; 4 1 1]))

   r, L, T = lll_with_removal_transform(A, fmpz(100))

   @test T*A == L

   B = deepcopy(A)
   lll!(B)
   @test B == lll(A)

   B = gram(A)
   lll_gram!(B)
   @test B == lll_gram(gram(A))
end

@testset "fmpz_mat.nullspace..." begin
   S = MatrixSpace(FlintZZ, 3, 3)
   T = MatrixSpace(FlintZZ, 3, 1)

   A = S([fmpz(2) 3 5; 1 4 7; 4 1 1])

   @test nullspace(A) == (1, T([1; -9; 5]))

   r, N = nullspace(A)

   @test iszero(A*N)
end

@testset "fmpz_mat.rank..." begin
   S = MatrixSpace(FlintZZ, 3, 3)

   A = S([fmpz(2) 3 5; 1 4 7; 4 1 1])

   @test rank(A) == 2
end

@testset "fmpz_mat.rref..." begin
   S = MatrixSpace(FlintZZ, 3, 3)

   A = S([fmpz(2) 3 5; 1 4 7; 4 1 1])

   r, B, d = rref(A)

   @test (B, d) == (S([5 0 (-1); 0 5 9; 0 0 0]), 5)
   @test r == 2
end

@testset "fmpz_mat.snf..." begin
   S = MatrixSpace(FlintZZ, 3, 3)

   A = S([fmpz(2) 3 5; 1 4 7; 19 3 7])

   @test snf(A) == S([1 0 0; 0 1 0; 0 0 27])

   @test issnf(snf(A))

   B = S([fmpz(2) 0 0; 0 4 0; 0 0 7])

   @test issnf(snf_diagonal(B))
end

@testset "fmpz_mat.solve_rational..." begin
   S = MatrixSpace(FlintZZ, 3, 3)

   A = S([fmpz(2) 3 5; 1 4 7; 9 2 2])

   T = MatrixSpace(FlintZZ, 3, 1)

   B = T([fmpz(4), 5, 7])

   X, d = solve_rational(A, B)

   @test (X, d) == (T([3, -24, 14]), 1)

   @test d == 1

   @test A*X == B

   (Y, k) = solve_dixon(A, B)

   @test reduce_mod(Y, k) == reduce_mod(X, k)
end

@testset "fmpz_mat.solve..." begin
   S = MatrixSpace(FlintZZ, 3, 3)

   A = S([fmpz(2) 3 5; 1 4 7; 9 2 2])

   T = MatrixSpace(FlintZZ, 3, 1)

   B = T([fmpz(4), 5, 7])

   X = solve(A, B)

   @test X == T([3, -24, 14])
   @test A*X == B
end


@testset "fmpz_mat.concat..." begin
   S = MatrixSpace(FlintZZ, 3, 3)
   T = MatrixSpace(FlintZZ, 3, 6)
   U = MatrixSpace(FlintZZ, 6, 3)

   A = S([fmpz(2) 3 5; 1 4 7; 9 6 3])
   B = S([fmpz(1) 4 7; 9 6 7; 4 3 3])

   @test hcat(A, B) == T([2 3 5 1 4 7; 1 4 7 9 6 7; 9 6 3 4 3 3])

   @test vcat(A, B) == U([2 3 5; 1 4 7; 9 6 3; 1 4 7; 9 6 7; 4 3 3])
end
