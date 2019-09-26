@testset "fmpq_mat.constructors..." begin
   S = MatrixSpace(QQ, 3, 3)

   @test elem_type(S) == fmpq_mat
   @test elem_type(FmpqMatSpace) == fmpq_mat
   @test parent_type(fmpq_mat) == FmpqMatSpace
   @test base_ring(S) == FlintQQ
   @test nrows(S) == 3
   @test ncols(S) == 3

   @test isa(S, FmpqMatSpace)

   f = S(fmpq(3))

   @test isa(f, MatElem)

   g = S(2)

   @test isa(g, MatElem)

   h = S(fmpz(5))

   @test isa(h, MatElem)

   k = S([fmpq(2) 3 5; 1 4 7; 9 6 3])

   @test isa(k, MatElem)

   k = S([2 3 5; 1 4 7; 9 6 3]')

   @test isa(k, MatElem)

   k = S([fmpz(2) 3 5; 1 4 7; 9 6 3])

   @test isa(k, MatElem)

   l = S(k)

   @test isa(l, MatElem)

   m = S()

   @test isa(m, MatElem)

   n = S([1 2 3; 4 5 6; 7 8 9])

   @test isa(n, MatElem)

   o = S([1//1 2 3; 4 5 6; 7 8 9])

   @test isa(o, MatElem)

   p = S([BigInt(1)//BigInt(1) 2 3; 4 5 6; 7 8 9])

   @test isa(p, MatElem)

   o = S([1, 2, 3, 4, 5, 6, 7, 8, 9])

   @test isa(o, MatElem)

   o = S([fmpz(1), 2, 3, 4, 5, 6, 7, 8, 9])

   @test isa(o, MatElem)

   o = S([BigInt(1), 2, 3, 4, 5, 6, 7, 8, 9])

   @test isa(o, MatElem)

   o = S([fmpq(1), 2, 3, 4, 5, 6, 7, 8, 9])

   @test isa(o, MatElem)

   o = S([1//1, 2, 3, 4, 5, 6, 7, 8, 9])

   @test isa(o, MatElem)

   o = S([BigInt(1)//BigInt(1), 2, 3, 4, 5, 6, 7, 8, 9])

   @test isa(o, MatElem)

   @test_throws ErrorConstrDimMismatch (S([fmpq(1) 2; 3 4]))
   @test_throws ErrorConstrDimMismatch (S([fmpq(1), 2, 3, 4]))
   @test_throws ErrorConstrDimMismatch (S([fmpq(1) 2 3 4; 5 6 7 8; 1 2 3 4]))
   @test_throws ErrorConstrDimMismatch (S([fmpq(1), 2, 3, 4, 5, 6, 7, 8, 1, 2, 3, 4]))

   arr = [1 2; 3 4]
   arr2 = [1, 2, 3, 4, 5, 6]

   for T in [fmpz, Int, BigInt, Rational{Int}, Rational{BigInt}]
      M = matrix(FlintQQ, map(T, arr))
      @test isa(M, fmpq_mat)
      @test M.base_ring == FlintQQ

      M2 = matrix(FlintQQ, 2, 3, map(T, arr2))
      @test isa(M2, fmpq_mat)
      @test M2.base_ring == FlintQQ
      @test nrows(M2) == 2
      @test ncols(M2) == 3
      @test_throws ErrorConstrDimMismatch matrix(FlintQQ, 2, 2, map(T, arr2))
      @test_throws ErrorConstrDimMismatch matrix(FlintQQ, 2, 4, map(T, arr2))
   end

   M3 = zero_matrix(FlintQQ, 2, 3)

   @test isa(M3, fmpq_mat)
   @test M3.base_ring == FlintQQ

   M4 = identity_matrix(FlintQQ, 3)

   @test isa(M4, fmpq_mat)
   @test M4.base_ring == FlintQQ

   a = zero_matrix(FlintQQ, 2, 2)
   b = zero_matrix(FlintQQ, 2, 3)
   @test a in [a, b]
   @test a in [b, a]
   @test !(a in [b])
   @test a in keys(Dict(a => 1))
   @test !(a in keys(Dict(b => 1)))
end

@testset "fmpq_mat.similar..." begin
   S = MatrixSpace(QQ, 3, 3)
   s = S(fmpz(3))

   t = similar(s)
   @test t isa fmpq_mat
   @test size(t) == size(s)

   t = similar(s, 2, 3)
   @test t isa fmpq_mat
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

@testset "fmpq_mat.printing..." begin
   a = MatrixSpace(QQ, 2, 2)(1)

  # test that default Julia printing is not used
  @test !occursin(string(typeof(a)), string(a))
end

@testset "fmpq_mat.manipulation..." begin
   S = MatrixSpace(QQ, 3, 3)
   A = S([fmpq(2) 3 5; 1 4 7; 9 6 3])
   B = S([fmpq(1) 4 7; 9 6 7; 4 3 3])

   @test iszero(zero(S))
   @test isone(one(S))

   B[1, 1] = fmpz(3)

   @test B[1, 1] == fmpz(3)

   B[1, 1] = fmpq(4)

   @test B[1, 1] == fmpq(4)

   B[1, 1] = BigInt(5)

   @test B[1, 1] == BigInt(5)

   B[1, 1] = 4//1

   @test B[1, 1] == 4//1

   B[1, 1] = BigInt(5)//1

   @test B[1, 1] == BigInt(5)//1

   @test nrows(B) == 3
   @test ncols(B) == 3

   @test deepcopy(A) == A
end

@testset "fmpq_mat.view..." begin
   S = MatrixSpace(QQ, 3, 3)

   A = S([1 2 3; 4 5 6; 7 8 9])

   B = @inferred view(A, 1, 1, 2, 2)

   @test typeof(B) == fmpq_mat
   @test B == MatrixSpace(QQ, 2, 2)([1 2; 4 5])

   B[1, 1] = 10
   @test A[1, 1] == 10

   C = @inferred view(B, 1:2, 1:2)

   @test typeof(C) == fmpq_mat
   @test C == MatrixSpace(QQ, 2, 2)([10 2; 4 5])

   C[1, 1] = 20
   @test B[1, 1] == 20
   @test A[1, 1] == 20

   A = 0
   GC.gc()

   @test B[1, 1] == 20
end

@testset "fmpq_mat.sub..." begin
   S = MatrixSpace(FlintQQ, 3, 3)

   A = S([1 2 3; 4 5 6; 7 8 9])

   B = @inferred sub(A, 1, 1, 2, 2)

   @test typeof(B) == fmpq_mat
   @test B == MatrixSpace(FlintQQ, 2, 2)([1 2; 4 5])

   B[1, 1] = 10
   @test A == S([1 2 3; 4 5 6; 7 8 9])

   C = @inferred sub(B, 1:2, 1:2)

   @test typeof(C) == fmpq_mat
   @test C == MatrixSpace(FlintQQ, 2, 2)([10 2; 4 5])

   C[1, 1] = 20
   @test B == MatrixSpace(FlintQQ, 2, 2)([10 2; 4 5])
   @test A == S([1 2 3; 4 5 6; 7 8 9])
end

@testset "fmpq_mat.unary_ops..." begin
   S = MatrixSpace(QQ, 3, 3)

   A = S([fmpq(2) 3 5; 1 4 7; 9 6 3])
   B = S([fmpq(-2) (-3) (-5); (-1) (-4) (-7); (-9) (-6) (-3)])

   @test -A == B
end

@testset "fmpq_mat.binary_ops..." begin
   S = MatrixSpace(QQ, 3, 3)

   A = S([fmpq(2) 3 5; 1 4 7; 9 6 3])
   B = S([fmpq(1) 4 7; 9 6 7; 4 3 3])

   @test A + B == S([3 7 12; 10 10 14; 13 9 6])

   @test A - B == S([1 (-1) (-2); (-8) (-2) 0; 5 3 0])

   @test A*B == S([49 41 50; 65 49 56; 75 81 114])
end

@testset "fmpq_mat.adhoc_binary..." begin
   S = MatrixSpace(QQ, 3, 3)

   A = S([fmpq(2) 3 5; 1 4 7; 9 6 3])

   @test 12 + A == A + 12
   @test BigInt(12) + A == A + 12
   @test fmpz(11) + A == A + fmpz(11)
   @test fmpq(11) + A == A + fmpq(11)
   @test 11//1 + A == A + fmpq(11)
   @test BigInt(11)//1 + A == A + fmpq(11)
   @test A - 3 == -(3 - A)
   @test A - BigInt(3) == -(3 - A)
   @test A - fmpz(7) == -(fmpz(7) - A)
   @test A - fmpq(7) == -(fmpq(7) - A)
   @test A - 7//1 == -(fmpq(7) - A)
   @test A - BigInt(7)//1 == -(fmpq(7) - A)
   @test 3*A == A*3
   @test BigInt(3)*A == A*3
   @test fmpz(3)*A == A*fmpz(3)
   @test fmpq(3)*A == A*fmpq(3)
   @test (3//1)*A == A*fmpq(3)
   @test (BigInt(3)//1)*A == A*fmpq(3)
end

@testset "fmpq_mat.kronecker_product..." begin
   S = MatrixSpace(QQ, 2, 3)
   S2 = MatrixSpace(QQ, 2, 2)
   S3 = MatrixSpace(QQ, 3, 3)

   A = S(fmpq[2 3 5; 9 6 3])
   B = S2(fmpq[2 3; 1 4])
   C = S3(fmpq[2 3 5; 1 4 7; 9 6 3])

   @test size(kronecker_product(A, A)) == (4,9)
   @test kronecker_product(B*A,A*C) == kronecker_product(B,A) * kronecker_product(A,C)
end

@testset "fmpq_mat.comparison..." begin
   S = MatrixSpace(QQ, 3, 3)

   A = S([fmpq(2) 3 5; 1 4 7; 9 6 3])
   B = S([fmpq(2) 3 5; 1 4 7; 9 6 3])

   @test A == B

   @test A != one(S)
end

@testset "fmpq_mat.adhoc_comparison..." begin
   S = MatrixSpace(QQ, 3, 3)

   A = S([fmpq(2) 3 5; 1 4 7; 9 6 3])

   @test S(12) == 12
   @test S(12) == BigInt(12)
   @test S(5) == fmpz(5)
   @test S(5) == fmpq(5)
   @test S(5) == 5//1
   @test S(5) == BigInt(5)//1
   @test 12 == S(12)
   @test BigInt(12) == S(12)
   @test fmpz(5) == S(5)
   @test fmpq(5) == S(5)
   @test 5//1 == S(5)
   @test BigInt(5)//1 == S(5)
   @test A != one(S)
   @test one(S) == one(S)
end

@testset "fmpq_mat.powering..." begin
   S = MatrixSpace(QQ, 3, 3)

   A = S([fmpq(2) 3 5; 1 4 7; 9 6 3])

   @test A^5 == A^2*A^3

   @test A^0 == one(S)
end

@testset "fmpq_mat.adhoc_exact_division..." begin
   S = MatrixSpace(QQ, 3, 3)

   A = S([fmpq(2) 3 5; 1 4 7; 9 6 3])

   @test divexact(5*A, 5) == A
   @test divexact(12*A, fmpz(12)) == A
   @test divexact(3*A, fmpq(3)) == A
   @test divexact(3*A, BigInt(3)) == A
   @test divexact(3*A, 3//1) == A
   @test divexact(3*A, BigInt(3)//1) == A
end

@testset "fmpq_mat.gso..." begin
   S = MatrixSpace(QQ, 3, 3)

   A = S([fmpq(2) 3 5; 1 4 7; 9 6 3])

   @test gso(A) == S([fmpq(2) fmpq(65, 43) fmpq(18, 23);
                      fmpq(1) fmpq(140, 43) fmpq(-9, 23);
                      fmpq(9) fmpq(-30, 43) fmpq(-3, 23)])
end

@testset "fmpq_mat.trace..." begin
   S = MatrixSpace(QQ, 3, 3)

   A = S([fmpq(2) 3 5; 1 4 7; 9 6 3])

   @test tr(A) == 9
end

@testset "fmpq_mat.transpose..." begin
   S = MatrixSpace(QQ, 3, 3)

   A = S([fmpq(2) 3 5; 1 4 7; 9 6 3])

   B = transpose(A) + A

   @test B == transpose(B)

   C = transpose(A)*A

   @test transpose(C) == C
end

@testset "fmpq_mat.row_col_swapping..." begin
   a = matrix(FlintQQ, [1 2; 3 4; 5 6])

   @test swap_rows(a, 1, 3) == matrix(FlintQQ, [5 6; 3 4; 1 2])

   swap_rows!(a, 2, 3)

   @test a == matrix(FlintQQ, [1 2; 5 6; 3 4])

   @test swap_cols(a, 1, 2) == matrix(FlintQQ, [2 1; 6 5; 4 3])

   swap_cols!(a, 2, 1)

   @test a == matrix(FlintQQ, [2 1; 6 5; 4 3])

   a = matrix(FlintQQ, [1 2; 3 4])
   @test reverse_rows(a) == matrix(FlintQQ, [3 4; 1 2])
   reverse_rows!(a)
   @test a == matrix(FlintQQ, [3 4; 1 2])

   a = matrix(FlintQQ, [1 2; 3 4])
   @test reverse_cols(a) == matrix(FlintQQ, [2 1; 4 3])
   reverse_cols!(a)
   @test a == matrix(FlintQQ, [2 1; 4 3])

   a = matrix(FlintQQ, [1 2 3; 3 4 5; 5 6 7])

   @test reverse_rows(a) == matrix(FlintQQ, [5 6 7; 3 4 5; 1 2 3])
   reverse_rows!(a)
   @test a == matrix(FlintQQ, [5 6 7; 3 4 5; 1 2 3])

   a = matrix(FlintQQ, [1 2 3; 3 4 5; 5 6 7])
   @test reverse_cols(a) == matrix(FlintQQ, [3 2 1; 5 4 3; 7 6 5])
   reverse_cols!(a)
   @test a == matrix(FlintQQ, [3 2 1; 5 4 3; 7 6 5])
end

@testset "fmpq_mat.inversion..." begin
   S = MatrixSpace(QQ, 3, 3)

   A = S([fmpq(2) 3 5; 1 4 7; 9 2 2])
   B = S([-6 4 1; 61 (-41) (-9); -34 23 5])
   C = S([fmpq(3) 1 2; 1 5 1; 4 8 0])

   @test inv(inv(A)) == A

   @test inv(A) == B

   @test inv(A)*A == one(S)

   @test inv(C)*C == one(S)

   @test C*inv(C) == one(S)
end

@testset "fmpq_mat.exact_division..." begin
   S = MatrixSpace(QQ, 3, 3)

   A = S([fmpq(2) 3 5; 1 4 7; 9 2 2])
   B = S([2 3 4; 7 9 1; 5 4 5])

   @test divexact(B*A, A) == B
end

@testset "fmpq_mat.det..." begin
   S = MatrixSpace(QQ, 3, 3)

   A = S([fmpq(2) 3 5; 1 4 7; 19 3 7])

   @test det(A) == 27
end

@testset "fmpq_mat.hilbert..." begin
   S = MatrixSpace(QQ, 4, 4)

   c4 = fmpz(2)^2*fmpz(3)

   c8 = fmpz(2)^6*fmpz(3)^5*fmpz(4)^4*fmpz(5)^3*fmpz(6)^2*fmpz(7)

   @test det(hilbert(S)) == c4^4//c8
end

@testset "fmpq_mat.nullspace..." begin
   S = MatrixSpace(QQ, 3, 3)
   T = MatrixSpace(QQ, 3, 1)

   A = S([fmpq(2) 3 5; 1 4 7; 4 1 1])

   @test nullspace(A) == (1, T([fmpq(1, 5); fmpq(-9, 5); fmpq(1)]))

   r, N = nullspace(A)

   @test iszero(A*N)
end

@testset "fmpq_mat.rank..." begin
   S = MatrixSpace(QQ, 3, 3)

   A = S([fmpq(2) 3 5; 1 4 7; 4 1 1])

   @test rank(A) == 2
end

@testset "fmpq_mat.rref..." begin
   S = MatrixSpace(QQ, 3, 3)

   A = S([fmpq(2) 3 5; 1 4 7; 4 1 1])

   @test rref(A) == (2, S([1 0 fmpq(-1, 5); 0 1 fmpq(9, 5); 0 0 0]))
end

@testset "fmpq_mat.solve..." begin
   S = MatrixSpace(QQ, 3, 3)

   A = S([fmpq(2) 3 5; 1 4 7; 9 2 2])

   T = MatrixSpace(QQ, 3, 1)

   B = T([fmpq(4), 5, 7])

   X = solve(A, B)

   @test X == T([3, -24, 14])

   @test A*X == B

   Y = solve_dixon(A, B)

   @test X == Y
end

@testset "fmpq_mat.concat..." begin
   S = MatrixSpace(QQ, 3, 3)
   T = MatrixSpace(QQ, 3, 6)
   U = MatrixSpace(QQ, 6, 3)

   A = S([fmpq(2) 3 5; 1 4 7; 9 6 3])
   B = S([fmpq(1) 4 7; 9 6 7; 4 3 3])

   @test hcat(A, B) == T([fmpq(2) 3 5 1 4 7; 1 4 7 9 6 7; 9 6 3 4 3 3])

   @test vcat(A, B) == U([fmpq(2) 3 5; 1 4 7; 9 6 3; 1 4 7; 9 6 7; 4 3 3])
end

@testset "fmpq_mat.charpoly..." begin
   S = MatrixSpace(QQ, 3, 3)
   R, x = PolynomialRing(QQ, "x")

   A = S([fmpq(2) 3 5; 1 4 7; 9 6 3])

   @test charpoly(R, A) == x^3 - 9*x^2 - 64*x + 30
end

@testset "fmpq_mat.minpoly..." begin
   S = MatrixSpace(QQ, 10, 10)
   R, x = PolynomialRing(QQ, "x")
   M = S()

   for i in 1:5
      for j in 1:5
         r = rand(-10:10)
         M[i, j] = r
         M[5 + i, 5 + j] = r
      end
   end

   for i in 1:5
      similarity!(M, rand(1:10), fmpq(rand(-3:3)))
   end

   @test degree(minpoly(R, M)) == 5
end
