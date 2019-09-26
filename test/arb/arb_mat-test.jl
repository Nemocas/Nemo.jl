RR = ArbField(64)

@testset "arb_mat.constructors..." begin
   S = MatrixSpace(RR, 3, 3)
   R = MatrixSpace(ZZ, 3, 3)

   @test elem_type(S) == arb_mat
   @test elem_type(ArbMatSpace) == arb_mat
   @test parent_type(arb_mat) == ArbMatSpace
   @test nrows(S) == 3
   @test ncols(S) == 3

   @test isa(S, ArbMatSpace)

   f = S(fmpz(3))

   @test isa(f, MatElem)

   g = S(2)

   @test isa(g, MatElem)

   k = S([fmpz(2) 3 5; 1 4 7; 9 6 3])

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

   r = S(R([fmpz(2) 3 5; 1 4 7; 9 6 3]))

   @test isa(r, MatElem)

   @test_throws ErrorConstrDimMismatch S([1 2])
   @test_throws ErrorConstrDimMismatch S([1, 2])
   @test_throws ErrorConstrDimMismatch S([1 2 3; 4 5 6; 7 8 9; 10 11 12])
   @test_throws ErrorConstrDimMismatch S([1, 2, 3, 4, 5, 6, 7, 8, 9, 10])

   arr = [1 2; 3 4]
   arr2 = [1, 2, 3, 4, 5, 6]

   for T in [fmpz, fmpq, Int, BigInt, Float64, BigFloat, RR, string, Rational{Int}, Rational{BigInt}]
      M = matrix(RR, map(T, arr))
      @test isa(M, arb_mat)
      @test M.base_ring == RR
      @test nrows(M) == 2
      @test ncols(M) == 2

      M2 = matrix(RR, 2, 3, map(T, arr2))
      @test isa(M2, arb_mat)
      @test M2.base_ring == RR
      @test nrows(M2) == 2
      @test ncols(M2) == 3
      @test_throws ErrorConstrDimMismatch matrix(RR, 2, 2, map(T, arr2))
      @test_throws ErrorConstrDimMismatch matrix(RR, 2, 4, map(T, arr2))
   end

   M3 = zero_matrix(RR, 2, 3)

   @test isa(M3, arb_mat)
   @test M3.base_ring == RR

   M4 = identity_matrix(RR, 3)

   @test isa(M4, arb_mat)
   @test M4.base_ring == RR

   a = zero_matrix(RR, 2, 2)
   b = zero_matrix(RR, 2, 3)
   @test a in [a, b]
   @test a in [b, a]
   @test !(a in [b])
end

@testset "arb_mat.similar..." begin
   S = MatrixSpace(RR, 3, 3)
   s = S(fmpz(3))

   t = similar(s)
   @test t isa arb_mat
   @test size(t) == size(s)
   t = similar(s, RR)
   @test t isa arb_mat
   @test size(t) == size(s)

   t = similar(s, 2, 3)
   @test t isa arb_mat
   @test size(t) == (2, 3)
   t = similar(s, RR, 2, 3)
   @test t isa arb_mat
   @test size(t) == (2, 3)
end

@testset "arb_mat.printing..." begin
   S = MatrixSpace(RR, 3, 3)
   f = S(fmpz(3))

   # test that default Julia printing is not used
   @test !occursin(string(typeof(f)), string(f))
end

@testset "arb_mat.manipulation..." begin
   S = MatrixSpace(RR, 3, 3)
   A = S([fmpz(2) 3 5; 1 4 7; 9 6 3])
   B = S([fmpz(1) 4 7; 9 6 7; 4 3 3])

   @test iszero(zero(S))
   @test isone(one(S))

   i = 0
   for T in [Int, UInt, fmpz, fmpq, Float64, BigFloat, fmpz, fmpq]
      i += 1
      B[1, 1] = T(i)

      @test B[1, 1] == RR(i)
   end

   @test nrows(B) == 3
   @test ncols(B) == 3

   @test deepcopy(A) == A
end

@testset "arb_mat.unary_ops..." begin
   S = MatrixSpace(RR, 3, 3)
   R = MatrixSpace(ZZ, 3, 3)

   A = S([2 3 5; 1 4 7; 9 6 3])
   B = R([(-2) (-3) (-5); (-1) (-4) (-7); (-9) (-6) (-3)])

   @test contains(-A, B)
end

@testset "arb_mat.transpose..." begin
   S = MatrixSpace(RR, 3, 3)
   T = MatrixSpace(ZZ, 3, 3)

   A = S([2 3 5; 1 4 7; 9 6 3])

   B = transpose(A) + A

   @test overlaps(transpose(B), B)

   C = transpose(A)*A

   @test overlaps(transpose(C), C)
end

@testset "arb_mat.binary_ops..." begin
   S = MatrixSpace(RR, 3, 3)
   R = MatrixSpace(ZZ, 3, 3)

   A = S([2 3 5; 1 4 7; 9 6 3])
   B = S([1 4 7; 9 6 7; 4 3 3])

   @test contains(A + B, R([3 7 12; 10 10 14; 13 9 6]))

   @test contains(A - B, R([1 (-1) (-2); (-8) (-2) 0; 5 3 0]))

   @test contains(A*B, R([49 41 50; 65 49 56; 75 81 114]))
end

@testset "arb_mat.adhoc_binary..." begin
   S = MatrixSpace(RR, 3, 3)
   R = MatrixSpace(ZZ, 3, 3)
   T = MatrixSpace(QQ, 3, 3)

   A = S([fmpz(2) 3 5; 1 4 7; 9 6 3])
   B = R([fmpz(2) 3 5; 1 4 7; 9 6 3])
   C = T([QQ(2) 3 5; 1 4 7; 9 6 3])
   q = QQ(1)//QQ(3)

   @test contains(12 + A, B + 12)
   @test contains(A + 12, B + 12)

   @test contains(fmpz(11) + A, B + fmpz(11))
   @test contains(A + fmpz(11), B + fmpz(11))

   @test contains(A - 3, -(3 - B))
   @test contains(3 - A, 3 - B)

   @test contains(A - fmpz(7), -(fmpz(7) - B))
   @test contains(fmpz(7) - A, fmpz(7) - B)

   @test contains(3*A, B*3)
   @test contains(A*3, B*3)

   @test contains(fmpz(3)*A, B*fmpz(3))
   @test contains(A*fmpz(3), B*fmpz(3))

   @test contains(q + A, C + q)
   @test contains(A + q, C + q)

   @test contains(A - q, -(q - C))
   @test contains(q - A, q - C)

   @test contains(q*A, C*q)
   @test contains(A*q, C*q)
end

@testset "arb_mat.shifting..." begin
   S = MatrixSpace(RR, 3, 3)
   R = MatrixSpace(ZZ, 3, 3)

   A = S([2 3 5; 1 4 7; 9 6 3])
   B = R([2 3 5; 1 4 7; 9 6 3])

   C = ldexp(A, 4)

   @test overlaps(16*A, C)
   @test contains(C, 16*B)
end

@testset "arb_mat.comparison..." begin
   S = MatrixSpace(RR, 3, 3)
   R = MatrixSpace(ZZ, 3, 3)

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

@testset "arb_mat.adhoc_comparison..." begin
   S = MatrixSpace(RR, 3, 3)
   R = MatrixSpace(ZZ, 3, 3)
   T = MatrixSpace(QQ, 3, 3)

   A = S([2 3 5; 1 4 7; 9 6 3])
   B = R([2 3 5; 1 4 7; 9 6 3])
   C = T(fmpq[2 3 5; 1 4 7; 9 6 3])

   @test contains(A, B)
   @test contains(A, C)

   @test S(12) == 12
   @test 12 == S(12)
   @test S(5) == fmpz(5)
   @test fmpz(5) == S(5)

   @test A == B
   @test B == A
end

@testset "arb_mat.inversion..." begin
   S = MatrixSpace(RR, 3, 3)
   R = MatrixSpace(ZZ, 3, 3)

   A = S([1 2 1000; 0 3 1; 0 2 1])
   B = R([1 1998 -2998; 0 1 -1; 0 -2 3])

   C = inv(A)

   @test overlaps(A*C, one(S))
   @test contains(C, B)
end

@testset "arb_mat.divexact..." begin
   S = MatrixSpace(RR, 3, 3)
   R = MatrixSpace(ZZ, 3, 3)

   A = S([1 2 1001; 0 3 1; 0 2 1])
   B = R([1 2000 -3001; 0 1 -1; 0 -2 3])

   @test overlaps(divexact(A, A), one(S))
   @test contains(divexact(one(S), A), B)
end

@testset "arb_mat.adhoc_divexact..." begin
   S = MatrixSpace(RR, 3, 3)
   R = MatrixSpace(ZZ, 3, 3)

   A = S([3 0 0; 0 3 0; 0 0 3])
   B = one(R)

   @test contains(divexact(A, 3), B)
   @test contains(divexact(A, fmpz(3)), B)
   @test contains(divexact(A, RR("3.0 +/- 0.5")), B)
end

@testset "arb_mat.charpoly..." begin
   S = MatrixSpace(RR, 3, 3)
   R, x = PolynomialRing(RR, "x")
   ZZy, y = PolynomialRing(ZZ, "y")

   A = S(["2.0 +/- 0.1" "3.0 +/- 0.1" "5.0 +/- 0.1";
          "0.0 +/- 0.1" "4.0 +/- 0.1" "7.0 +/- 0.1";
          "0.0 +/- 0.1" "0.0 +/- 0.1" "3.0 +/- 0.1"])

   f = (y - 2)*(y - 4)*(y - 3)

   g = charpoly(R, A)

   @test contains(g, f)
end

@testset "arb_mat.det..." begin
   S = MatrixSpace(RR, 3, 3)

   A = S(["2.0 +/- 0.1" "3.0 +/- 0.1" "5.0 +/- 0.1";
          "0.0 +/- 0.1" "4.0 +/- 0.1" "7.0 +/- 0.1";
          "0.0 +/- 0.1" "0.0 +/- 0.1" "3.0 +/- 0.1"])

   d = det(A)

   @test contains(d, 24)
end

@testset "arb_mat.exp..." begin
   S = MatrixSpace(RR, 3, 3)

   A = S(["2.0 +/- 0.1" "0.0 +/- 0.1" "0.0 +/- 0.1";
          "0.0 +/- 0.1" "4.0 +/- 0.1" "0.0 +/- 0.1";
          "0.0 +/- 0.1" "0.0 +/- 0.1" "3.0 +/- 0.1"])

   B = RR[ exp(RR(2)) 0 0; 0 exp(RR(4)) 0; 0 0 exp(RR(3)) ]

   C = exp(A)

   @test overlaps(B, C)
end

@testset "arb_mat.linear_solving..." begin
   S = MatrixSpace(RR, 3, 3)
   T = MatrixSpace(ZZ, 3, 3)

   A = S(["1.0 +/- 0.01" "2.0 +/- 0.01" "3.0 +/- 0.01";
          "4.0 +/- 0.01" "5.0 +/- 0.01" "6.0 +/- 0.01";
          "8.0 +/- 0.01" "8.0 +/- 0.01" "9.0 +/- 0.01"])

   B = deepcopy(A)

   b = RR["6.0 +/- 0.1" "15.0 +/- 0.1" "25.0 +/- 0.1"]

   r, p, L, U = lu(A)

   @test overlaps(L*U, p*A)
   @test r == 3

   y = solve(A, transpose(b))

   @test overlaps(A*y, transpose(b))

   @test contains(transpose(y), ZZ[1 1 1])

   Nemo.lu!(p, A)

   y = solve_lu_precomp(p, A, transpose(b))

   @test overlaps(B*y, transpose(b))

   @test contains(transpose(y), ZZ[1 1 1])
end

@testset "arb_mat.bound_inf_norm..." begin
   S = MatrixSpace(RR, 3, 3)

   A = S([2 3 5; 1 4 7; 9 6 3])

   c = bound_inf_norm(A)

   for i in 1:3
     for j in 1:3
       @test A[i, j] <= c
     end
   end
end
