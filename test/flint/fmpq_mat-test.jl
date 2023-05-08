@testset "QQMatrix.constructors" begin
   S = matrix_space(QQ, 3, 3)

   @test elem_type(S) == QQMatrix
   @test elem_type(QQMatrixSpace) == QQMatrix
   @test parent_type(QQMatrix) == QQMatrixSpace
   @test base_ring(S) == FlintQQ
   @test nrows(S) == 3
   @test ncols(S) == 3

   @test isa(S, QQMatrixSpace)

   f = S(QQFieldElem(3))

   @test isa(f, MatElem)

   g = S(2)

   @test isa(g, MatElem)

   h = S(ZZRingElem(5))

   @test isa(h, MatElem)

   k = S([QQFieldElem(2) 3 5; 1 4 7; 9 6 3])

   @test isa(k, MatElem)

   k = S([2 3 5; 1 4 7; 9 6 3]')

   @test isa(k, MatElem)

   k = S([ZZRingElem(2) 3 5; 1 4 7; 9 6 3])

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

   o = S([ZZRingElem(1), 2, 3, 4, 5, 6, 7, 8, 9])

   @test isa(o, MatElem)

   o = S([BigInt(1), 2, 3, 4, 5, 6, 7, 8, 9])

   @test isa(o, MatElem)

   o = S([QQFieldElem(1), 2, 3, 4, 5, 6, 7, 8, 9])

   @test isa(o, MatElem)

   o = S([1//1, 2, 3, 4, 5, 6, 7, 8, 9])

   @test isa(o, MatElem)

   o = S([BigInt(1)//BigInt(1), 2, 3, 4, 5, 6, 7, 8, 9])

   @test isa(o, MatElem)

   @test_throws ErrorConstrDimMismatch (S([QQFieldElem(1) 2; 3 4]))
   @test_throws ErrorConstrDimMismatch (S([QQFieldElem(1), 2, 3, 4]))
   @test_throws ErrorConstrDimMismatch (S([QQFieldElem(1) 2 3 4; 5 6 7 8; 1 2 3 4]))
   @test_throws ErrorConstrDimMismatch (S([QQFieldElem(1), 2, 3, 4, 5, 6, 7, 8, 1, 2, 3, 4]))

   arr = [1 2; 3 4]
   arr2 = [1, 2, 3, 4, 5, 6]

   for T in [ZZRingElem, Int, BigInt, Rational{Int}, Rational{BigInt}]
      M = matrix(FlintQQ, map(T, arr))
      @test isa(M, QQMatrix)
      @test base_ring(M) == FlintQQ

      M2 = matrix(FlintQQ, 2, 3, map(T, arr2))
      @test isa(M2, QQMatrix)
      @test base_ring(M2) == FlintQQ
      @test nrows(M2) == 2
      @test ncols(M2) == 3
      @test_throws ErrorConstrDimMismatch matrix(FlintQQ, 2, 2, map(T, arr2))
      @test_throws ErrorConstrDimMismatch matrix(FlintQQ, 2, 4, map(T, arr2))
   end

   M3 = zero_matrix(FlintQQ, 2, 3)

   @test isa(M3, QQMatrix)
   @test base_ring(M3) == FlintQQ

   M4 = identity_matrix(FlintQQ, 3)

   @test isa(M4, QQMatrix)
   @test base_ring(M4) == FlintQQ

   a = zero_matrix(FlintQQ, 2, 2)
   b = zero_matrix(FlintQQ, 2, 3)
   @test a in [a, b]
   @test a in [b, a]
   @test !(a in [b])
   @test a in keys(Dict(a => 1))
   @test !(a in keys(Dict(b => 1)))
end

@testset "QQMatrix.similar" begin
   S = matrix_space(QQ, 3, 3)
   s = S(ZZRingElem(3))

   t = similar(s)
   @test t isa QQMatrix
   @test size(t) == size(s)

   t = similar(s, 2, 3)
   @test t isa QQMatrix
   @test size(t) == (2, 3)

   for (R, M) in ring_to_mat
      t = similar(s, R)
      @test size(t) == size(s)

      t = similar(s, R, 2, 3)
      @test size(t) == (2, 3)
   end

   # issue #651
   m = one(Generic.MatSpace{QQFieldElem}(QQ, 2, 2))
   for n = (m, -m, m*m, m+m, 2m)
      @test n isa Generic.MatSpaceElem{QQFieldElem}
   end
end

@testset "QQMatrix.is_zero_entry" begin
   M = matrix(FlintQQ, [1 2 3;4 0 6;0 8 9])
   for i in 1:3, j in 1:3
      @test is_zero_entry(M, i, j) == (M[i, j] == 0)
   end
end

@testset "QQMatrix.printing" begin
   a = matrix_space(QQ, 2, 2)(1)

  # test that default Julia printing is not used
  @test !occursin(string(typeof(a)), string(a))
end

@testset "QQMatrix.manipulation" begin
   S = matrix_space(QQ, 3, 3)
   A = S([QQFieldElem(2) 3 5; 1 4 7; 9 6 3])
   B = S([QQFieldElem(1) 4 7; 9 6 7; 4 3 3])

   @test iszero(zero(S))
   @test isone(one(S))

   B[1, 1] = ZZRingElem(3)

   @test B[1, 1] == ZZRingElem(3)

   B[1, 1] = QQFieldElem(4)

   @test B[1, 1] == QQFieldElem(4)

   B[1, 1] = BigInt(5)

   @test B[1, 1] == BigInt(5)

   B[1, 1] = 4//1

   @test B[1, 1] == 4//1

   B[1, 1] = BigInt(5)//1

   @test B[1, 1] == BigInt(5)//1

   @test nrows(B) == 3
   @test ncols(B) == 3

   @test deepcopy(A) == A

   a = matrix(FlintQQ, 4, 4, [-1//2 ZZRingElem(2)^100 3 -4; 5 -1//2 ZZRingElem(2)^100 6; 7 5 -1//2 8; 9 10 11 12])
   @test hash(a, UInt(5)) == hash(deepcopy(a), UInt(5))
   @test hash(view(a, 1,1, 2,2)) == hash(view(a, 1,1, 2,2))
end

@testset "QQMatrix.view" begin
   S = matrix_space(QQ, 3, 3)

   A = S([1 2 3; 4 5 6; 7 8 9])

   B = @inferred view(A, 1, 1, 2, 2)

   @test typeof(B) == QQMatrix
   @test B == matrix_space(QQ, 2, 2)([1 2; 4 5])

   B[1, 1] = 10
   @test A[1, 1] == 10

   C = @inferred view(B, 1:2, 1:2)

   @test typeof(C) == QQMatrix
   @test C == matrix_space(QQ, 2, 2)([10 2; 4 5])

   C[1, 1] = 20
   @test B[1, 1] == 20
   @test A[1, 1] == 20

   A = 0
   GC.gc()

   @test B[1, 1] == 20
end

@testset "QQMatrix.sub" begin
   S = matrix_space(FlintQQ, 3, 3)

   A = S([1 2 3; 4 5 6; 7 8 9])

   B = @inferred sub(A, 1, 1, 2, 2)

   @test typeof(B) == QQMatrix
   @test B == matrix_space(FlintQQ, 2, 2)([1 2; 4 5])

   B[1, 1] = 10
   @test A == S([1 2 3; 4 5 6; 7 8 9])

   C = @inferred sub(B, 1:2, 1:2)

   @test typeof(C) == QQMatrix
   @test C == matrix_space(FlintQQ, 2, 2)([10 2; 4 5])

   C[1, 1] = 20
   @test B == matrix_space(FlintQQ, 2, 2)([10 2; 4 5])
   @test A == S([1 2 3; 4 5 6; 7 8 9])
end

@testset "QQMatrix.unary_ops" begin
   S = matrix_space(QQ, 3, 3)

   A = S([QQFieldElem(2) 3 5; 1 4 7; 9 6 3])
   B = S([QQFieldElem(-2) (-3) (-5); (-1) (-4) (-7); (-9) (-6) (-3)])

   @test -A == B
end

@testset "QQMatrix.binary_ops" begin
   S = matrix_space(QQ, 3, 3)

   A = S([QQFieldElem(2) 3 5; 1 4 7; 9 6 3])
   B = S([QQFieldElem(1) 4 7; 9 6 7; 4 3 3])

   @test A + B == S([3 7 12; 10 10 14; 13 9 6])

   @test A - B == S([1 (-1) (-2); (-8) (-2) 0; 5 3 0])

   @test A*B == S([49 41 50; 65 49 56; 75 81 114])
end

@testset "QQMatrix.adhoc_binary" begin
   S = matrix_space(QQ, 3, 3)

   A = S([QQFieldElem(2) 3 5; 1 4 7; 9 6 3])

   @test 12 + A == A + 12
   @test BigInt(12) + A == A + 12
   @test ZZRingElem(11) + A == A + ZZRingElem(11)
   @test QQFieldElem(11) + A == A + QQFieldElem(11)
   @test 11//1 + A == A + QQFieldElem(11)
   @test BigInt(11)//1 + A == A + QQFieldElem(11)
   @test A - 3 == -(3 - A)
   @test A - BigInt(3) == -(3 - A)
   @test A - ZZRingElem(7) == -(ZZRingElem(7) - A)
   @test A - QQFieldElem(7) == -(QQFieldElem(7) - A)
   @test A - 7//1 == -(QQFieldElem(7) - A)
   @test A - BigInt(7)//1 == -(QQFieldElem(7) - A)
   @test 3*A == A*3
   @test BigInt(3)*A == A*3
   @test ZZRingElem(3)*A == A*ZZRingElem(3)
   @test QQFieldElem(3)*A == A*QQFieldElem(3)
   @test (3//1)*A == A*QQFieldElem(3)
   @test (BigInt(3)//1)*A == A*QQFieldElem(3)
end

@testset "QQMatrix.kronecker_product" begin
   S = matrix_space(QQ, 2, 3)
   S2 = matrix_space(QQ, 2, 2)
   S3 = matrix_space(QQ, 3, 3)

   A = S(QQFieldElem[2 3 5; 9 6 3])
   B = S2(QQFieldElem[2 3; 1 4])
   C = S3(QQFieldElem[2 3 5; 1 4 7; 9 6 3])

   @test size(kronecker_product(A, A)) == (4,9)
   @test kronecker_product(B*A,A*C) == kronecker_product(B,A) * kronecker_product(A,C)
end

@testset "QQMatrix.comparison" begin
   S = matrix_space(QQ, 3, 3)

   A = S([QQFieldElem(2) 3 5; 1 4 7; 9 6 3])
   B = S([QQFieldElem(2) 3 5; 1 4 7; 9 6 3])

   @test A == B

   @test A != one(S)
end

@testset "QQMatrix.adhoc_comparison" begin
   S = matrix_space(QQ, 3, 3)

   A = S([QQFieldElem(2) 3 5; 1 4 7; 9 6 3])

   @test S(12) == 12
   @test S(12) == BigInt(12)
   @test S(5) == ZZRingElem(5)
   @test S(5) == QQFieldElem(5)
   @test S(5) == 5//1
   @test S(5) == BigInt(5)//1
   @test 12 == S(12)
   @test BigInt(12) == S(12)
   @test ZZRingElem(5) == S(5)
   @test QQFieldElem(5) == S(5)
   @test 5//1 == S(5)
   @test BigInt(5)//1 == S(5)
   @test A != one(S)
   @test one(S) == one(S)
end

@testset "QQMatrix.powering" begin
   S = matrix_space(QQ, 3, 3)

   A = S([QQFieldElem(2) 3 5; 1 4 7; 9 6 3])

   @test A^5 == A^2*A^3

   @test A^0 == one(S)
end

@testset "QQMatrix.adhoc_exact_division" begin
   S = matrix_space(QQ, 3, 3)

   A = S([QQFieldElem(2) 3 5; 1 4 7; 9 6 3])

   @test divexact(5*A, 5) == A
   @test divexact(12*A, ZZRingElem(12)) == A
   @test divexact(3*A, QQFieldElem(3)) == A
   @test divexact(3*A, BigInt(3)) == A
   @test divexact(3*A, 3//1) == A
   @test divexact(3*A, BigInt(3)//1) == A
end

@testset "QQMatrix.gso" begin
   S = matrix_space(QQ, 3, 3)

   A = S([QQFieldElem(2) 3 5; 1 4 7; 9 6 3])

   @test gso(A) == S([QQFieldElem(2) QQFieldElem(65, 43) QQFieldElem(18, 23);
                      QQFieldElem(1) QQFieldElem(140, 43) QQFieldElem(-9, 23);
                      QQFieldElem(9) QQFieldElem(-30, 43) QQFieldElem(-3, 23)])
end

@testset "QQMatrix.trace" begin
   S = matrix_space(QQ, 3, 3)

   A = S([QQFieldElem(2) 3 5; 1 4 7; 9 6 3])

   @test tr(A) == 9
end

@testset "QQMatrix.transpose" begin
   S = matrix_space(QQ, 3, 3)

   A = S([QQFieldElem(2) 3 5; 1 4 7; 9 6 3])

   B = transpose(A) + A

   @test B == transpose(B)

   C = transpose(A)*A

   @test transpose(C) == C
end

@testset "QQMatrix.row_col_swapping" begin
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

@testset "QQMatrix.inversion" begin
   S = matrix_space(QQ, 3, 3)

   A = S([QQFieldElem(2) 3 5; 1 4 7; 9 2 2])
   B = S([-6 4 1; 61 (-41) (-9); -34 23 5])
   C = S([QQFieldElem(3) 1 2; 1 5 1; 4 8 0])

   @test inv(inv(A)) == A

   @test inv(A) == B

   @test inv(A)*A == one(S)

   @test inv(C)*C == one(S)

   @test C*inv(C) == one(S)

   a = QQ[1 1;]
   @test_throws ErrorException inv(a)
   a = QQ[1 1; 1 1]
   @test_throws ErrorException inv(a)
end

@testset "QQMatrix.exact_division" begin
   S = matrix_space(QQ, 3, 3)

   A = S([QQFieldElem(2) 3 5; 1 4 7; 9 2 2])
   B = S([2 3 4; 7 9 1; 5 4 5])

   @test divexact(B*A, A) == B
end

@testset "QQMatrix.det" begin
   S = matrix_space(QQ, 3, 3)

   A = S([QQFieldElem(2) 3 5; 1 4 7; 19 3 7])

   @test det(A) == 27
end

@testset "QQMatrix.hilbert" begin
   S = matrix_space(QQ, 4, 4)

   c4 = ZZRingElem(2)^2*ZZRingElem(3)

   c8 = ZZRingElem(2)^6*ZZRingElem(3)^5*ZZRingElem(4)^4*ZZRingElem(5)^3*ZZRingElem(6)^2*ZZRingElem(7)

   @test det(hilbert(S)) == c4^4//c8
end

@testset "QQMatrix.nullspace" begin
   S = matrix_space(QQ, 3, 3)
   T = matrix_space(QQ, 3, 1)

   A = S([QQFieldElem(2) 3 5; 1 4 7; 4 1 1])

   @test nullspace(A) == (1, T([QQFieldElem(1, 5); QQFieldElem(-9, 5); QQFieldElem(1)]))

   r, N = nullspace(A)

   @test iszero(A*N)
end

@testset "QQMatrix.rank" begin
   S = matrix_space(QQ, 3, 3)

   A = S([QQFieldElem(2) 3 5; 1 4 7; 4 1 1])

   @test rank(A) == 2
end

@testset "QQMatrix.rref" begin
   for iters = 1:50
      m = rand(0:50)
      n = rand(0:50)
      S = matrix_space(QQ, m, n)
      M = rand(S, -100:100)
      r, N = rref(M)

      @test is_rref(N)
      MM = deepcopy(M)
      rr = rref!(MM)
      @test (r, N) == (rr, MM)
   end
 
   S = matrix_space(QQ, 3, 3)

   A = S([QQFieldElem(2) 3 5; 1 4 7; 4 1 1])

   @test rref(A) == (2, S([1 0 QQFieldElem(-1, 5); 0 1 QQFieldElem(9, 5); 0 0 0]))
end

@testset "QQMatrix.solve" begin
   S = matrix_space(QQ, 3, 3)

   A = S([QQFieldElem(2) 3 5; 1 4 7; 9 2 2])

   T = matrix_space(QQ, 3, 1)

   B = T([QQFieldElem(4), 5, 7])

   X = solve(A, B)

   @test X == T([3, -24, 14])

   @test A*X == B

   Y = solve_dixon(A, B)

   @test X == Y

   m1 = [1 1; 1 0; 1 0]
   m2 = [ 1 0 ; 0 1; 0 1 ]
   m1Q = matrix(QQ, m1)
   m2Q = matrix(QQ, m2);
   
   N = solve(m1Q, m2Q)

   @test N == matrix(QQ, 2, 2, [0 1; 1 -1])

   for i in 1:10
      m = rand(0:10)
      n = rand(0:10)
      k = rand(0:10)

      M = matrix_space(QQ, n, k)
      N = matrix_space(QQ, n, m)

      A = rand(M, -10:10)
      B = rand(N, -10:10)

      fl, X = can_solve_with_solution(A, B)

      if fl
         @test A * X == B
      end
   end

   A = matrix(QQ, 2, 2, [1, 2, 2, 5])
   B = matrix(QQ, 2, 1, [1, 2])
   fl, X = can_solve_with_solution(A, B)
   @test fl
   @test A * X == B
   @test can_solve(A, B)

   A = matrix(QQ, 2, 2, [1, 2, 2, 4])
   B = matrix(QQ, 2, 1, [1, 2])
   fl, X = can_solve_with_solution(A, B)
   @test fl
   @test A * X == B
   @test can_solve(A, B)

   A = matrix(QQ, 2, 2, [1, 2, 2, 4])
   B = matrix(QQ, 2, 1, [1, 3])
   fl, X = can_solve_with_solution(A, B)
   @test !fl
   @test !can_solve(A, B)

   A = zero_matrix(QQ, 2, 3)
   B = identity_matrix(QQ, 3)
   @test_throws ErrorException can_solve_with_solution(A, B)

   # Transpose
   A = transpose(matrix(QQ, 2, 2, [1, 2, 2, 5]))
   B = transpose(matrix(QQ, 2, 1, [1, 2]))
   fl, X = can_solve_with_solution(A, B, side = :left)
   @test fl
   @test X * A == B
   @test can_solve(A, B, side = :left)

   A = transpose(matrix(QQ, 2, 2, [1, 2, 2, 4]))
   B = transpose(matrix(QQ, 2, 1, [1, 2]))
   fl, X = can_solve_with_solution(A, B, side = :left)
   @test fl
   @test X * A == B
   @test can_solve(A, B, side = :left)

   A = transpose(matrix(QQ, 2, 2, [1, 2, 2, 4]))
   B = transpose(matrix(QQ, 2, 1, [1, 3]))
   fl, X = can_solve_with_solution(A, B, side = :left)
   @test !fl
   @test !can_solve(A, B, side = :left)

   A = transpose(zero_matrix(QQ, 2, 3))
   B = transpose(identity_matrix(QQ, 3))
   @test_throws ErrorException can_solve_with_solution(A, B, side = :left)

   @test_throws ErrorException can_solve_with_solution(A, B, side = :garbage)
   @test_throws ErrorException can_solve(A, B, side = :garbage)
end

@testset "QQMatrix.concat" begin
   S = matrix_space(QQ, 3, 3)
   T = matrix_space(QQ, 3, 6)
   U = matrix_space(QQ, 6, 3)

   A = S([QQFieldElem(2) 3 5; 1 4 7; 9 6 3])
   B = S([QQFieldElem(1) 4 7; 9 6 7; 4 3 3])

   @test hcat(A, B) == T([QQFieldElem(2) 3 5 1 4 7; 1 4 7 9 6 7; 9 6 3 4 3 3])

   @test vcat(A, B) == U([QQFieldElem(2) 3 5; 1 4 7; 9 6 3; 1 4 7; 9 6 7; 4 3 3])
end

@testset "QQMatrix.charpoly" begin
   S = matrix_space(QQ, 3, 3)
   R, x = polynomial_ring(QQ, "x")

   A = S([QQFieldElem(2) 3 5; 1 4 7; 9 6 3])

   @test charpoly(R, A) == x^3 - 9*x^2 - 64*x + 30
end

@testset "QQMatrix.minpoly" begin
   S = matrix_space(QQ, 10, 10)
   R, x = polynomial_ring(QQ, "x")
   M = S()

   for i in 1:5
      for j in 1:5
         r = rand(-10:10)
         M[i, j] = r
         M[5 + i, 5 + j] = r
      end
   end

   for i in 1:5
      similarity!(M, rand(1:10), QQFieldElem(rand(-3:3)))
   end

   @test degree(minpoly(R, M)) == 5
end

@testset "QQMatrix.rand" begin
   S = matrix_space(QQ, 10, 10)
   M = rand(S, 1:9)
   @test parent(M) == S
   for i=1:10, j=1:10
      x = M[i, j]
      @test numerator(x) in 1:9
      @test denominator(x) in 1:9
   end
end

@testset "QQMatrix.unsafe" begin
   A = matrix(QQ, 2, 3, [1//2 3//4 5//6; 7//8 9//10 11//12])
   @test mul!([QQ(), QQ()], A, [QQ(1), QQ(2), QQ(3)]) == [9//2, 217//40]
   @test mul!([QQ(), QQ()], A, [ZZ(1), ZZ(2), ZZ(3)]) == [9//2, 217//40]
   @test mul!([QQ(), QQ(), QQ()], [QQ(1), QQ(2)], A) == [9//4, 51//20, 8//3]
   @test mul!([QQ(), QQ(), QQ()], [ZZ(1), ZZ(2)], A) == [9//4, 51//20, 8//3]
end
