RR = ArbField(64)

@testset "ArbMatrix.constructors" begin
   @test_throws ErrorException matrix_space(RR, -1, 5)
   @test_throws ErrorException matrix_space(RR, 0, -2)
   @test_throws ErrorException matrix_space(RR, -3, -4)
   @test_throws ErrorException ArbMatSpace(RR, 2, -1)
   @test_throws ErrorException ArbMatSpace(RR, -1, 2)
   @test_throws ErrorException ArbMatSpace(RR, -1, -1)

   S = matrix_space(RR, 3, 3)
   R = matrix_space(ZZ, 3, 3)

   @test elem_type(S) == ArbMatrix
   @test elem_type(ArbMatSpace) == ArbMatrix
   @test parent_type(ArbMatrix) == ArbMatSpace
   @test nrows(S) == 3
   @test ncols(S) == 3

   @test isa(S, ArbMatSpace)

   f = S(ZZRingElem(3))

   @test isa(f, MatElem)

   g = S(2)

   @test isa(g, MatElem)

   k = S([ZZRingElem(2) 3 5; 1 4 7; 9 6 3])

   @test isa(g, MatElem)

   k = S(Int[2 3 5; 1 4 7; 9 6 3]')

   @test isa(k, MatElem)

   l = S(k)

   @test isa(l, MatElem)

   m = S()

   @test isa(m, MatElem)

   o = S([1.0 2.0 3.0; 1.0 1.0 1.0; 2.0 3.1 4.1])

   @test isa(o, MatElem)

   p = S(BigFloat[1.0 2.0 3.0; 1.0 1.0 1.0; 2.0 3.1 4.1])

   @test isa(p, MatElem)

   q = S(["1.0" "2.0" "3.0"; "1.0" "1.0" "1.0"; "2.0" "3.1" "4.1"])

   @test isa(p, MatElem)

   r = S(R([ZZRingElem(2) 3 5; 1 4 7; 9 6 3]))

   @test isa(r, MatElem)

   @test_throws ErrorConstrDimMismatch S([1 2])
   @test_throws ErrorConstrDimMismatch S([1, 2])
   @test_throws ErrorConstrDimMismatch S([1 2 3; 4 5 6; 7 8 9; 10 11 12])
   @test_throws ErrorConstrDimMismatch S([1, 2, 3, 4, 5, 6, 7, 8, 9, 10])

   arr = [1 2; 3 4]
   arr2 = [1, 2, 3, 4, 5, 6]

   for T in [ZZRingElem, QQFieldElem, Int, BigInt, Float64, BigFloat, RR, string, Rational{Int}, Rational{BigInt}]
      M = matrix(RR, map(T, arr))
      @test isa(M, ArbMatrix)
      @test base_ring(M) == RR
      @test nrows(M) == 2
      @test ncols(M) == 2

      M2 = matrix(RR, 2, 3, map(T, arr2))
      @test isa(M2, ArbMatrix)
      @test base_ring(M2) == RR
      @test nrows(M2) == 2
      @test ncols(M2) == 3
      @test_throws ErrorConstrDimMismatch matrix(RR, 2, 2, map(T, arr2))
      @test_throws ErrorConstrDimMismatch matrix(RR, 2, 4, map(T, arr2))
   end

   M3 = zero_matrix(RR, 2, 3)

   @test isa(M3, ArbMatrix)
   @test base_ring(M3) == RR

   M4 = identity_matrix(RR, 3)

   @test isa(M4, ArbMatrix)
   @test base_ring(M4) == RR

   a = zero_matrix(RR, 2, 2)
   b = zero_matrix(RR, 2, 3)
   @test a in [a, b]
   @test a in [b, a]
   @test !(a in [b])
end

@testset "ArbMatrix.similar" begin
   S = matrix_space(RR, 3, 3)
   s = S(ZZRingElem(3))

   t = similar(s)
   @test t isa ArbMatrix
   @test size(t) == size(s)

   t = similar(s, 2, 3)
   @test t isa ArbMatrix
   @test size(t) == (2, 3)

   for (R, M) in ring_to_mat
      t = similar(s, R)
      @test size(t) == size(s)

      t = similar(s, R, 2, 3)
      @test size(t) == (2, 3)
   end

   # issue #651
   m = one(Generic.MatSpace{ArbFieldElem}(RR, 2, 2))
   for n = (m, -m, m*m, m+m, 2m)
      @test n isa Generic.MatSpaceElem{ArbFieldElem}
   end
end

@testset "ArbMatrix.printing" begin
   S = matrix_space(RR, 3, 3)
   f = S(ZZRingElem(3))

   # test that default Julia printing is not used
   @test !occursin(string(typeof(f)), string(f))
end

@testset "ArbMatrix.manipulation" begin
   S = matrix_space(RR, 3, 3)
   A = S([ZZRingElem(2) 3 5; 1 4 7; 9 6 3])
   B = S([ZZRingElem(1) 4 7; 9 6 7; 4 3 3])

   @test iszero(zero(S))
   @test isone(one(S))

   i = 0
   for T in [Int, UInt, ZZRingElem, QQFieldElem, Float64, BigFloat, ZZRingElem, QQFieldElem]
      i += 1
      B[1, 1] = T(i)

      @test B[1, 1] == RR(i)
   end

   @test nrows(B) == 3
   @test ncols(B) == 3

   @test deepcopy(A) == A
end

@testset "ArbMatrix.unary_ops" begin
   S = matrix_space(RR, 3, 3)
   R = matrix_space(ZZ, 3, 3)

   A = S([2 3 5; 1 4 7; 9 6 3])
   B = R([(-2) (-3) (-5); (-1) (-4) (-7); (-9) (-6) (-3)])

   @test contains(-A, B)
end

@testset "ArbMatrix.transpose" begin
   S = matrix_space(RR, 3, 3)
   T = matrix_space(ZZ, 3, 3)

   A = S([2 3 5; 1 4 7; 9 6 3])

   B = transpose(A) + A

   @test overlaps(transpose(B), B)

   C = transpose(A)*A

   @test overlaps(transpose(C), C)
end

@testset "ArbMatrix.binary_ops" begin
   S = matrix_space(RR, 3, 3)
   R = matrix_space(ZZ, 3, 3)

   A = S([2 3 5; 1 4 7; 9 6 3])
   B = S([1 4 7; 9 6 7; 4 3 3])

   @test contains(A + B, R([3 7 12; 10 10 14; 13 9 6]))

   @test contains(A - B, R([1 (-1) (-2); (-8) (-2) 0; 5 3 0]))

   @test contains(A*B, R([49 41 50; 65 49 56; 75 81 114]))
end

@testset "ArbMatrix.adhoc_binary" begin
   S = matrix_space(RR, 3, 3)
   R = matrix_space(ZZ, 3, 3)
   T = matrix_space(QQ, 3, 3)

   A = S([ZZRingElem(2) 3 5; 1 4 7; 9 6 3])
   B = R([ZZRingElem(2) 3 5; 1 4 7; 9 6 3])
   C = T([QQ(2) 3 5; 1 4 7; 9 6 3])
   q = QQ(1)//QQ(3)

   @test contains(12 + A, B + 12)
   @test contains(A + 12, B + 12)

   @test contains(ZZRingElem(11) + A, B + ZZRingElem(11))
   @test contains(A + ZZRingElem(11), B + ZZRingElem(11))

   @test contains(A - 3, -(3 - B))
   @test contains(3 - A, 3 - B)

   @test contains(A - ZZRingElem(7), -(ZZRingElem(7) - B))
   @test contains(ZZRingElem(7) - A, ZZRingElem(7) - B)

   @test contains(3*A, B*3)
   @test contains(A*3, B*3)

   @test contains(ZZRingElem(3)*A, B*ZZRingElem(3))
   @test contains(A*ZZRingElem(3), B*ZZRingElem(3))

   @test contains(q + A, C + q)
   @test contains(A + q, C + q)

   @test contains(A - q, -(q - C))
   @test contains(q - A, q - C)

   @test contains(q*A, C*q)
   @test contains(A*q, C*q)
end

@testset "ArbMatrix.shifting" begin
   S = matrix_space(RR, 3, 3)
   R = matrix_space(ZZ, 3, 3)

   A = S([2 3 5; 1 4 7; 9 6 3])
   B = R([2 3 5; 1 4 7; 9 6 3])

   C = ldexp(A, 4)

   @test overlaps(16*A, C)
   @test contains(C, 16*B)
end

@testset "ArbMatrix.comparison" begin
   S = matrix_space(RR, 3, 3)
   R = matrix_space(ZZ, 3, 3)

   A = S([2 3 5; 1 4 7; 9 6 3])
   AZZ = R([2 3 5; 1 4 7; 9 6 3])
   B = S([2.1 3 5; 1 4 7; 9 6 3])
   C = S(["2.0 +/- 0.5" "3.0 +/- 0.5" "5.0 +/- 0.5";
          "1.0 +/- 0.5" "4.0 +/- 0.5" "7.0 +/- 0.5";
          "9.0 +/- 0.5" "6.0 +/- 0.5" "3.0 +/- 0.5"])

   @test isequal(A, A)

   @test A == A

   @test A != B

   @test overlaps(A, C)

   @test contains(C, A)
end

@testset "ArbMatrix.adhoc_comparison" begin
   S = matrix_space(RR, 3, 3)
   R = matrix_space(ZZ, 3, 3)
   T = matrix_space(QQ, 3, 3)

   A = S([2 3 5; 1 4 7; 9 6 3])
   B = R([2 3 5; 1 4 7; 9 6 3])
   C = T(QQFieldElem[2 3 5; 1 4 7; 9 6 3])

   @test contains(A, B)
   @test contains(A, C)

   @test S(12) == 12
   @test 12 == S(12)
   @test S(5) == ZZRingElem(5)
   @test ZZRingElem(5) == S(5)

   @test A == B
   @test B == A
end

@testset "ArbMatrix.inversion" begin
   S = matrix_space(RR, 3, 3)
   R = matrix_space(ZZ, 3, 3)

   A = S([1 2 1000; 0 3 1; 0 2 1])
   B = R([1 1998 -2998; 0 1 -1; 0 -2 3])

   C = inv(A)

   @test overlaps(A*C, one(S))
   @test contains(C, B)

   fl, C = is_invertible_with_inverse(A)
   @test fl && contains(C, B)

   A = RR[1 1; 1 1] 
   fl, C = is_invertible_with_inverse(A)
   @test !fl
   @test_throws ErrorException inv(A)
end

@testset "ArbMatrix.divexact" begin
   S = matrix_space(RR, 3, 3)
   R = matrix_space(ZZ, 3, 3)

   A = S([1 2 1001; 0 3 1; 0 2 1])
   B = R([1 2000 -3001; 0 1 -1; 0 -2 3])

   @test overlaps(divexact(A, A), one(S))
   @test contains(divexact(one(S), A), B)
end

@testset "ArbMatrix.adhoc_divexact" begin
   S = matrix_space(RR, 3, 3)
   R = matrix_space(ZZ, 3, 3)

   A = S([3 0 0; 0 3 0; 0 0 3])
   B = one(R)

   @test contains(divexact(A, 3), B)
   @test contains(divexact(A, ZZRingElem(3)), B)
   @test contains(divexact(A, RR("3.0 +/- 0.5")), B)
end

@testset "ArbMatrix.charpoly" begin
   S = matrix_space(RR, 3, 3)
   R, x = polynomial_ring(RR, "x")
   ZZy, y = polynomial_ring(ZZ, "y")

   A = S(["2.0 +/- 0.1" "3.0 +/- 0.1" "5.0 +/- 0.1";
          "0.0 +/- 0.1" "4.0 +/- 0.1" "7.0 +/- 0.1";
          "0.0 +/- 0.1" "0.0 +/- 0.1" "3.0 +/- 0.1"])

   f = (y - 2)*(y - 4)*(y - 3)

   g = charpoly(R, A)

   @test contains(g, f)
end

@testset "ArbMatrix.det" begin
   S = matrix_space(RR, 3, 3)

   A = S(["2.0 +/- 0.1" "3.0 +/- 0.1" "5.0 +/- 0.1";
          "0.0 +/- 0.1" "4.0 +/- 0.1" "7.0 +/- 0.1";
          "0.0 +/- 0.1" "0.0 +/- 0.1" "3.0 +/- 0.1"])

   d = det(A)

   @test contains(d, 24)
end

@testset "ArbMatrix.exp" begin
   S = matrix_space(RR, 3, 3)

   A = S(["2.0 +/- 0.1" "0.0 +/- 0.1" "0.0 +/- 0.1";
          "0.0 +/- 0.1" "4.0 +/- 0.1" "0.0 +/- 0.1";
          "0.0 +/- 0.1" "0.0 +/- 0.1" "3.0 +/- 0.1"])

   B = RR[ exp(RR(2)) 0 0; 0 exp(RR(4)) 0; 0 0 exp(RR(3)) ]

   C = exp(A)

   @test overlaps(B, C)
end

@testset "ArbMatrix.lu_nonsquare" begin
   S = matrix_space(RR, 2, 3)

   A = S(["1.0 +/- 0.01" "1.0 +/- 0.01" "1.0 +/- 0.01";
          "1.0 +/- 0.01" "0.0 +/- 0.01" "-1.0 +/- 0.01"])

    r, p, L, U = lu(A)

    @test overlaps(L*U, p*A)
    @test r == 2
end

@testset "ArbMatrix.cholesky_solving" begin
   S = matrix_space(RR, 3, 3)

   A = S(["4.0 +/- 0.01" "1.0 +/- 0.01" "1.0 +/- 0.01";
          "1.0 +/- 0.01" "4.0 +/- 0.01" "-1.0 +/- 0.01";
          "1.0 +/- 0.01" "-1.0 +/- 0.01" "4.0 +/- 0.01"])

   cho = cholesky(A)

   @test overlaps(cho * transpose(cho), A)

   b = RR["6.0 +/- 0.1" "4.0 +/- 0.1" "4.0 +/- 0.1"]

   y = Nemo._solve_cholesky_precomp(cho, transpose(b))

   @test overlaps(A*y, transpose(b))

   @test contains(transpose(y), ZZ[1 1 1])
end

@testset "ArbMatrix.linear_solving" begin
   S = matrix_space(RR, 3, 3)
   T = matrix_space(ZZ, 3, 3)

   A = S(["1.0 +/- 0.01" "2.0 +/- 0.01" "3.0 +/- 0.01";
          "4.0 +/- 0.01" "5.0 +/- 0.01" "6.0 +/- 0.01";
          "8.0 +/- 0.01" "8.0 +/- 0.01" "9.0 +/- 0.01"])

   B = deepcopy(A)

   b = RR["6.0 +/- 0.1" "15.0 +/- 0.1" "25.0 +/- 0.1"]

   r, p, L, U = lu(A)

   @test overlaps(L*U, p*A)
   @test r == 3

   Nemo.lu!(p, A)

   y = Nemo._solve_lu_precomp(p, A, transpose(b))

   @test overlaps(B*y, transpose(b))

   @test contains(transpose(y), ZZ[1 1 1])
end

@testset "ArbMatrix.Solve.solve" begin
   S = matrix_space(RR, 3, 3)

   A = S(["1.0 +/- 0.01" "2.0 +/- 0.01" "3.0 +/- 0.01";
          "4.0 +/- 0.01" "5.0 +/- 0.01" "6.0 +/- 0.01";
          "8.0 +/- 0.01" "8.0 +/- 0.01" "9.0 +/- 0.01"])

   b = transpose(RR["6.0 +/- 0.1" "15.0 +/- 0.1" "25.0 +/- 0.1"])
   b2 = 2*b

   fl, y, K = can_solve_with_solution_and_kernel(A, b, side = :right)
   @test fl
   @test overlaps(A*y, b)
   @test contains(transpose(y), ZZ[1 1 1])
   @test ncols(K) == 0

   fl, y, K = can_solve_with_solution_and_kernel(A, transpose(b))
   @test fl
   @test overlaps(y*A, transpose(b))
   @test nrows(K) == 0

   y = solve(A, b, side = :right)
   @test fl
   @test overlaps(A*y, b)
   @test contains(transpose(y), ZZ[1 1 1])

   C = solve_init(A)
   fl, y, K = can_solve_with_solution_and_kernel(C, b, side = :right)
   @test fl
   @test overlaps(A*y, b)
   @test contains(transpose(y), ZZ[1 1 1])
   @test ncols(K) == 0

   fl, y, K = can_solve_with_solution_and_kernel(C, transpose(b))
   @test fl
   @test overlaps(y*A, transpose(b))
   @test nrows(K) == 0

   y = solve(C, b, side = :right)
   @test fl
   @test overlaps(A*y, b)
   @test contains(transpose(y), ZZ[1 1 1])

   fl, y, K = can_solve_with_solution_and_kernel(C, b2, side = :right)
   @test fl
   @test overlaps(A*y, b2)
   @test contains(transpose(y), ZZ[2 2 2])
   @test ncols(K) == 0

   fl, y, K = can_solve_with_solution_and_kernel(C, transpose(b2))
   @test fl
   @test overlaps(y*A, transpose(b2))
   @test nrows(K) == 0

   y = solve(C, b2, side = :right)
   @test fl
   @test overlaps(A*y, b2)
   @test contains(transpose(y), ZZ[2 2 2])
end

@testset "ArbMatrix.bound_inf_norm" begin
   S = matrix_space(RR, 3, 3)

   A = S([2 3 5; 1 4 7; 9 6 3])

   c = bound_inf_norm(A)

   for i in 1:3
     for j in 1:3
       @test A[i, j] <= c
     end
   end
end
