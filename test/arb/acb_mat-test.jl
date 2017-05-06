CC = AcbField(64)
RR = ArbField(64)

function test_acb_mat_constructors()
   print("acb_mat.constructors...")
 
   S = MatrixSpace(CC, 3, 3)
   R = MatrixSpace(ZZ, 3, 3)

   @test isa(S, AcbMatSpace)

   f = S(fmpz(3))

   @test isa(f, MatElem)

   g = S(2)

   @test isa(g, MatElem)

   k = S([fmpz(2) 3 5; 1 4 7; 9 6 3])

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
   @test_throws ErrorConstrDimMismatch S([(1,1) (2,2)])
   @test_throws ErrorConstrDimMismatch S([(1,1), (2,2)])
   @test_throws ErrorConstrDimMismatch S([(1,1) (2,2) (3,3); (4,4) (5,5) (6,6);
                                          (7,7) (8,8) (9,9); (10,10) (11,11) (12,12)])
   @test_throws ErrorConstrDimMismatch S([(1,1), (2,2), (3,3), (4,4), (5,5), (6,6), (7,7),
                                          (8,8), (9,9), (10,10)])

   println("PASS")
end

function test_acb_mat_printing()
   print("acb_mat.constructors...")
 
   S = MatrixSpace(CC, 3, 3)
   f = S(fmpz(3))

   @test string(f) == "[3.0000000000000000000 + i*0 0 + i*0 0 + i*0]\n[0 + i*0 3.0000000000000000000 + i*0 0 + i*0]\n[0 + i*0 0 + i*0 3.0000000000000000000 + i*0]"

   println("PASS")
end

function test_acb_mat_manipulation()
   print("acb_mat.manipulation...")

   S = MatrixSpace(CC, 3, 3)
   A = S([fmpz(2) 3 5; 1 4 7; 9 6 3])
   B = S([fmpz(1) 4 7; 9 6 7; 4 3 3])

   @test iszero(zero(S))
   @test isone(one(S))

   B[1, 1] = fmpz(3)

   @test B[1, 1] == CC(3)

   @test rows(B) == 3
   @test cols(B) == 3

   @test deepcopy(A) == A

   println("PASS")
end

function test_acb_mat_unary_ops()
   print("acb_mat.unary_ops...")

   S = MatrixSpace(CC, 3, 3)
   R = MatrixSpace(ZZ, 3, 3)

   A = S([2 3 5; 1 4 7; 9 6 3])
   B = R([(-2) (-3) (-5); (-1) (-4) (-7); (-9) (-6) (-3)])

   @test contains(-A, B)

   println("PASS")
end

function test_acb_mat_transpose()
   print("acb_mat.transpose...")

   S = MatrixSpace(CC, 3, 3)
   T = MatrixSpace(ZZ, 3, 3)

   A = S([2 3 5; 1 4 7; 9 6 3])

   B = transpose(A) + A

   @test overlaps(transpose(B), B)

   C = transpose(A)*A

   @test overlaps(C', C)

   println("PASS")
end

function test_acb_mat_binary_ops()
   print("acb_mat.binary_ops...")

   S = MatrixSpace(CC, 3, 3)
   R = MatrixSpace(ZZ, 3, 3)

   A = S([2 3 5; 1 4 7; 9 6 3])
   B = S([1 4 7; 9 6 7; 4 3 3])

   @test contains(A + B, R([3 7 12; 10 10 14; 13 9 6]))

   @test contains(A - B, R([1 (-1) (-2); (-8) (-2) 0; 5 3 0]))

   @test contains(A*B, R([49 41 50; 65 49 56; 75 81 114]))

   println("PASS")
end

function test_acb_mat_adhoc_binary()
   print("acb_mat.adhoc_binary...")

   S = MatrixSpace(CC, 3, 3)
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
 
   println("PASS")
end

function test_acb_mat_shifting()
   print("acb_mat.shifting...")

   S = MatrixSpace(CC, 3, 3)
   R = MatrixSpace(ZZ, 3, 3)

   A = S([2 3 5; 1 4 7; 9 6 3])
   B = R([2 3 5; 1 4 7; 9 6 3])

   C = ldexp(A, 4)

   @test overlaps(16*A, C)
   @test contains(C, 16*B)

   println("PASS")
end

function test_acb_mat_comparison()
   print("acb_mat.comparison...")

   S = MatrixSpace(CC, 3, 3)
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
   
   println("PASS")
end

function test_acb_mat_adhoc_comparison()
   print("acb_mat.adhoc_comparison...")

   S = MatrixSpace(CC, 3, 3)
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

   println("PASS")
end

function test_acb_mat_predicates()
   print("acb_mat.predicates...")

   S = MatrixSpace(CC, 3, 3)
   A = S([1 2 1000; 0 3 1; 0 2 1])

   @test isreal(A)

   A = onei(CC) * A

   @test !isreal(A)
   
   println("PASS")
end

function test_acb_mat_inversion()
   print("acb_mat.inversion...")

   S = MatrixSpace(CC, 3, 3)
   R = MatrixSpace(ZZ, 3, 3)

   A = S([1 2 1000; 0 3 1; 0 2 1])
   B = R([1 1998 -2998; 0 1 -1; 0 -2 3])

   C = inv(A)

   @test overlaps(A*C, one(S))
   @test contains(C, B)

   println("PASS")
end

function test_acb_mat_divexact()
   print("acb_mat.divexact...")

   S = MatrixSpace(CC, 3, 3)
   R = MatrixSpace(ZZ, 3, 3)

   A = S([1 2 1001; 0 3 1; 0 2 1])
   B = R([1 2000 -3001; 0 1 -1; 0 -2 3])

   @test overlaps(divexact(A, A), one(S))
   @test contains(divexact(one(S), A), B)

   println("PASS")
end

function test_acb_mat_adhoc_divexact()
   print("acb_mat.adhoc_divexact...")

   S = MatrixSpace(CC, 3, 3)
   R = MatrixSpace(ZZ, 3, 3)

   A = S([3 0 0; 0 3 0; 0 0 3])
   B = one(R)

   @test contains(divexact(A, 3), B)
   @test contains(divexact(A, fmpz(3)), B)
   @test contains(divexact(A, CC("3.0 +/- 0.5")), B)

   println("PASS")
end

function test_acb_mat_charpoly()
   print("acb_mat.charpoly...")

   S = MatrixSpace(CC, 3, 3)
   R, x = PolynomialRing(CC, "x")
   ZZy, y = PolynomialRing(ZZ, "y")

   A = S(["2.0 +/- 0.1" "3.0 +/- 0.1" "5.0 +/- 0.1";
          "0.0 +/- 0.1" "4.0 +/- 0.1" "7.0 +/- 0.1";
          "0.0 +/- 0.1" "0.0 +/- 0.1" "3.0 +/- 0.1"])

   f = (y - 2)*(y - 4)*(y - 3)

   g = charpoly(R, A)

   @test contains(g, f)

   println("PASS")
end

function test_acb_mat_det()
   print("acb_mat.det...")

   S = MatrixSpace(CC, 3, 3)

   A = S(["2.0 +/- 0.1" "3.0 +/- 0.1" "5.0 +/- 0.1";
          "0.0 +/- 0.1" "4.0 +/- 0.1" "7.0 +/- 0.1";
          "0.0 +/- 0.1" "0.0 +/- 0.1" "3.0 +/- 0.1"])

   d = det(A)

   @test contains(d, 24)

   println("PASS")
end

function test_acb_mat_exp()
   print("acb_mat.exp...")

   S = MatrixSpace(CC, 3, 3)

   A = S(["2.0 +/- 0.1" "0.0 +/- 0.1" "0.0 +/- 0.1";
          "0.0 +/- 0.1" "4.0 +/- 0.1" "0.0 +/- 0.1";
          "0.0 +/- 0.1" "0.0 +/- 0.1" "3.0 +/- 0.1"])

   B = CC[ exp(RR(2)) 0 0; 0 exp(RR(4)) 0; 0 0 exp(RR(3)) ]

   C = exp(A)

   @test overlaps(B, C)

   println("PASS")
end

function test_acb_mat_linear_solving()
   print("acb_mat.linear_solving...")

   S = MatrixSpace(CC, 3, 3)
   T = MatrixSpace(ZZ, 3, 3)

   A = S(["1.0 +/- 0.01" "2.0 +/- 0.01" "3.0 +/- 0.01";
          "4.0 +/- 0.01" "5.0 +/- 0.01" "6.0 +/- 0.01";
          "8.0 +/- 0.01" "8.0 +/- 0.01" "9.0 +/- 0.01"])

   B = deepcopy(A)

   b = CC["6.0 +/- 0.1" "15.0 +/- 0.1" "25.0 +/- 0.1"]

   r, p, L, U = lufact(A)

   @test overlaps(L*U, p*A)
   @test r == 3

   y = solve(A, b')

   @test overlaps(A * y, b')

   @test contains(y', ZZ[1 1 1])

   Nemo.lufact!(p, A)

   y = solve_lu_precomp(p, A, b')

   @test overlaps(B*y, b')

   @test contains(y', ZZ[1 1 1])

   println("PASS")
end

function test_acb_mat_bound_inf_norm()
   print("acb_mat.bound_inf_norm...")

   S = MatrixSpace(CC, 3, 3)

   A = S([2 3 5; 1 4 7; 9 6 3])

   c = bound_inf_norm(A)

   for i in 1:3
     for j in 1:3
       @test abs(A[i, j]) <= c
     end
   end

   println("PASS")
end

function test_acb_mat()
   test_acb_mat_constructors()
   test_acb_mat_printing()
   test_acb_mat_manipulation()
   test_acb_mat_unary_ops()
   test_acb_mat_transpose()
   test_acb_mat_binary_ops()
   test_acb_mat_adhoc_binary()
   test_acb_mat_shifting()
   test_acb_mat_comparison()
   test_acb_mat_adhoc_comparison()
   test_acb_mat_predicates()
   test_acb_mat_inversion()
   test_acb_mat_divexact()
   test_acb_mat_adhoc_divexact()
   test_acb_mat_charpoly()
   test_acb_mat_det()
   test_acb_mat_exp()
   test_acb_mat_linear_solving()
   test_acb_mat_bound_inf_norm()

   println("")
end
