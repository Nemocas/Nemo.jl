function test_fmpz_mat_constructors()
   print("fmpz_mat.constructors...")

   S = MatrixSpace(ZZ, 3, 3)

   @test elem_type(S) == fmpz_mat
   @test elem_type(FmpzMatSpace) == fmpz_mat
   @test parent_type(fmpz_mat) == FmpzMatSpace

   @test isa(S, FmpzMatSpace)

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

   @test_throws ErrorConstrDimMismatch (S([fmpz(1) 2; 3 4]))
   @test_throws ErrorConstrDimMismatch (S([fmpz(1), 2, 3, 4]))
   @test_throws ErrorConstrDimMismatch (S([fmpz(1) 2 3 4; 5 6 7 8; 1 2 3 4]))
   @test_throws ErrorConstrDimMismatch (S([fmpz(1), 2, 3, 4, 5, 6, 7, 8, 1, 2, 3, 4]))

   println("PASS")
end

function test_fmpz_mat_printing()
   print("fmpz_mat.printing...")

   S = MatrixSpace(ZZ, 3, 3)
   f = S(fmpz(3))

   @test string(f) == "[3 0 0]\n[0 3 0]\n[0 0 3]"

   println("PASS")
end

function test_fmpz_mat_convert()
   print("fmpz_mat.convert...")

   # Basic tests.
   A = [[1 2 3]; [4 5 6]]
   Abig = BigInt[[1 2 3]; [4 5 6]]
   S = MatrixSpace(ZZ, 2, 3)
   B = S(A)

   @test Matrix{Int}(B) == A
   @test Matrix{BigInt}(B) == Abig

   # Tests when elements do not fit a simple Int.
   B[1, 1] = 10000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000
   @test_throws InexactError Matrix{Int}(B)

   println("PASS")
end

function test_fmpz_mat_manipulation()
   print("fmpz_mat.manipulation...")

   S = MatrixSpace(ZZ, 3, 3)
   A = S([fmpz(2) 3 5; 1 4 7; 9 6 3])
   B = S([fmpz(1) 4 7; 9 6 7; 4 3 3])

   @test iszero(zero(S))
   @test isone(one(S))

   B[1, 1] = fmpz(3)

   @test B[1, 1] == fmpz(3)

   @test rows(B) == 3
   @test cols(B) == 3

   @test deepcopy(A) == A

   println("PASS")
end

function test_fmpz_mat_view()
   print("fmpz_mat.view...")

   S = MatrixSpace(ZZ, 3, 3)

   A = S([1 2 3; 4 5 6; 7 8 9])

   B = @inferred view(A, 1, 1, 2, 2)

   @test typeof(B) == fmpz_mat
   @test B == MatrixSpace(ZZ, 2, 2)([1 2; 4 5])

   B[1, 1] = 10
   @test A[1, 1] == 10

   C = @inferred view(B, 1:2, 1:2)

   @test typeof(C) == fmpz_mat
   @test C == MatrixSpace(ZZ, 2, 2)([10 2; 4 5])

   C[1, 1] = 20
   @test B[1, 1] == 20
   @test A[1, 1] == 20

   println("PASS")
end

function test_fmpz_mat_unary_ops()
   print("fmpz_mat.unary_ops...")

   S = MatrixSpace(ZZ, 3, 3)

   A = S([fmpz(2) 3 5; 1 4 7; 9 6 3])
   B = S([fmpz(-2) (-3) (-5); (-1) (-4) (-7); (-9) (-6) (-3)])

   @test -A == B

   println("PASS")
end

function test_fmpz_mat_binary_ops()
   print("fmpz_mat.binary_ops...")

   S = MatrixSpace(ZZ, 3, 3)

   A = S([fmpz(2) 3 5; 1 4 7; 9 6 3])
   B = S([fmpz(1) 4 7; 9 6 7; 4 3 3])

   @test A + B == S([3 7 12; 10 10 14; 13 9 6])

   @test A - B == S([1 (-1) (-2); (-8) (-2) 0; 5 3 0])

   @test A*B == S([49 41 50; 65 49 56; 75 81 114])

   println("PASS")
end

function test_fmpz_mat_adhoc_binary()
   print("fmpz_mat.adhoc_binary...")

   S = MatrixSpace(ZZ, 3, 3)

   A = S([fmpz(2) 3 5; 1 4 7; 9 6 3])

   @test 12 + A == A + 12
   @test fmpz(11) + A == A + fmpz(11)
   @test A - 3 == -(3 - A)
   @test A - fmpz(7) == -(fmpz(7) - A)
   @test 3*A == A*3
   @test fmpz(3)*A == A*fmpz(3)

   println("PASS")
end

function test_fmpz_mat_comparison()
   print("fmpz_mat.comparison...")

   S = MatrixSpace(ZZ, 3, 3)

   A = S([fmpz(2) 3 5; 1 4 7; 9 6 3])
   B = S([fmpz(2) 3 5; 1 4 7; 9 6 3])

   @test A == B

   @test A != one(S)

   println("PASS")
end

function test_fmpz_mat_adhoc_comparison()
   print("fmpz_mat.adhoc_comparison...")

   S = MatrixSpace(ZZ, 3, 3)

   A = S([fmpz(2) 3 5; 1 4 7; 9 6 3])

   @test S(12) == 12
   @test S(5) == fmpz(5)
   @test 12 == S(12)
   @test fmpz(5) == S(5)
   @test A != one(S)
   @test one(S) == one(S)

   println("PASS")
end

function test_fmpz_mat_powering()
   print("fmpz_mat.powering...")

   S = MatrixSpace(ZZ, 3, 3)

   A = S([fmpz(2) 3 5; 1 4 7; 9 6 3])

   @test A^5 == A^2*A^3

   @test A^0 == one(S)

   println("PASS")
end

function test_fmpz_mat_adhoc_exact_division()
   print("fmpz_mat.adhoc_exact_division...")

   S = MatrixSpace(ZZ, 3, 3)

   A = S([fmpz(2) 3 5; 1 4 7; 9 6 3])

   @test divexact(5*A, 5) == A
   @test divexact(12*A, fmpz(12)) == A

   println("PASS")
end

function test_fmpz_mat_gram()
   print("fmpz_mat.gram...")

   S = MatrixSpace(ZZ, 3, 3)

   A = S([fmpz(2) 3 5; 1 4 7; 9 6 3])

   @test gram(A) == S([38 49 51; 49 66 54; 51 54 126])

   println("PASS")
end

function test_fmpz_mat_trace()
   print("fmpz_mat.trace...")

   S = MatrixSpace(ZZ, 3, 3)

   A = S([fmpz(2) 3 5; 1 4 7; 9 6 3])

   @test trace(A) == 9

   println("PASS")
end

function test_fmpz_mat_content()
   print("fmpz_mat.content...")

   S = MatrixSpace(ZZ, 3, 3)

   A = S([fmpz(2) 3 5; 1 4 7; 9 6 3])

   @test content(17*A) == 17
   println("PASS")
end

function test_fmpz_mat_transpose()
   print("fmpz_mat.transpose...")

   S = MatrixSpace(ZZ, 3, 3)

   A = S([fmpz(2) 3 5; 1 4 7; 9 6 3])

   B = transpose(A) + A

   @test B == transpose(B)

   C = transpose(A)*A

   @test C' == C

   println("PASS")
end

function test_fmpz_mat_scaling()
   print("fmpz_mat.scaling...")

   S = MatrixSpace(ZZ, 3, 3)

   A = S([fmpz(2) 3 5; 1 4 7; 9 6 3])

   @test (A<<5)>>5 == A

   println("PASS")
end

function test_fmpz_mat_inversion()
   print("fmpz_mat.inversion...")

   S = MatrixSpace(ZZ, 3, 3)

   A = S([fmpz(2) 3 5; 1 4 7; 9 2 2])
   B = S([-6 4 1; 61 (-41) (-9); -34 23 5])

   @test inv(inv(A)) == A

   @test inv(A) == B

   @test inv(A)*A == one(S)

   println("PASS")
end

function test_fmpz_mat_pseudo_inversion()
   print("fmpz_mat.pseudo_inversion...")

   S = MatrixSpace(ZZ, 3, 3)

   A = S([1 2 3; 1 2 3; 1 2 3])
   B = S([1 0 1; 2 3 1; 5 6 7])

   @test_throws ErrorException pseudo_inv(A)

   C, d = pseudo_inv(B)
   @test B*C == S(d)

   println("PASS")
end

function test_fmpz_mat_exact_division()
   print("fmpz_mat.exact_division...")

   S = MatrixSpace(ZZ, 3, 3)

   A = S([fmpz(2) 3 5; 1 4 7; 9 2 2])
   B = S([2 3 4; 7 9 1; 5 4 5])

   @test divexact(B*A, A) == B

   println("PASS")
end

function test_fmpz_mat_modular_reduction()
   print("fmpz_mat.modular_reduction...")

   S = MatrixSpace(ZZ, 3, 3)

   A = S([fmpz(2) 3 5; 1 4 7; 9 2 2])
   B = S([2 0 2; 1 1 1; 0 2 2])

   @test reduce_mod(A, 3) == B

   @test reduce_mod(A, fmpz(3)) == B

   println("PASS")
end

function test_fmpz_mat_det()
   print("fmpz_mat.det...")

   S = MatrixSpace(ZZ, 3, 3)

   A = S([fmpz(2) 3 5; 1 4 7; 19 3 7])

   @test det(A) == 27

   @test det_divisor(A) == 27

   @test det_given_divisor(A, 9) == 27

   @test det_given_divisor(A, fmpz(9)) == 27

   println("PASS")
end

function test_fmpz_mat_hadamard()
   print("fmpz_mat.hadamard...")

   S = MatrixSpace(ZZ, 4, 4)

   @test ishadamard(hadamard(S))

   println("PASS")
end

function test_fmpz_mat_hnf()
   print("fmpz_mat.hnf...")

   S = MatrixSpace(ZZ, 3, 3)

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

   println("PASS")
end

function test_fmpz_mat_lll()
   print("fmpz_mat.lll...")

   S = MatrixSpace(ZZ, 3, 3)

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

   println("PASS")
end

function test_fmpz_mat_nullspace()
   print("fmpz_mat.nullspace...")

   S = MatrixSpace(ZZ, 3, 3)
   T = MatrixSpace(ZZ, 3, 1)

   A = S([fmpz(2) 3 5; 1 4 7; 4 1 1])

   @test nullspace(A) == (T([1; -9; 5]), 1)

   N, r = nullspace(A)

   @test iszero(A*N)

   println("PASS")
end

function test_fmpz_mat_rank()
   print("fmpz_mat.rank...")

   S = MatrixSpace(ZZ, 3, 3)

   A = S([fmpz(2) 3 5; 1 4 7; 4 1 1])

   @test rank(A) == 2

   println("PASS")
end

function test_fmpz_mat_rref()
   print("fmpz_mat.rref...")

   S = MatrixSpace(ZZ, 3, 3)

   A = S([fmpz(2) 3 5; 1 4 7; 4 1 1])

   @test rref(A) == (S([5 0 (-1); 0 5 9; 0 0 0]), 5)

   println("PASS")
end

function test_fmpz_mat_snf()
   print("fmpz_mat.snf...")

   S = MatrixSpace(ZZ, 3, 3)

   A = S([fmpz(2) 3 5; 1 4 7; 19 3 7])

   @test snf(A) == S([1 0 0; 0 1 0; 0 0 27])

   @test issnf(snf(A))

   B = S([fmpz(2) 0 0; 0 4 0; 0 0 7])

   @test issnf(snf_diagonal(B))

   println("PASS")
end

function test_fmpz_mat_solve_rational()
   print("fmpz_mat.solve_rational...")

   S = MatrixSpace(ZZ, 3, 3)

   A = S([fmpz(2) 3 5; 1 4 7; 9 2 2])

   T = MatrixSpace(ZZ, 3, 1)

   B = T([fmpz(4), 5, 7])

   X, d = solve_rational(A, B)

   @test (X, d) == (T([3, -24, 14]), 1)

   @test d == 1

   @test A*X == B

   (Y, k) = solve_dixon(A, B)

   @test reduce_mod(Y, k) == reduce_mod(X, k)

   println("PASS")
end

function test_fmpz_mat_solve()
   print("fmpz_mat.solve...")

   S = MatrixSpace(ZZ, 3, 3)

   A = S([fmpz(2) 3 5; 1 4 7; 9 2 2])

   T = MatrixSpace(ZZ, 3, 1)

   B = T([fmpz(4), 5, 7])

   X = solve(A, B)

   @test X == T([3, -24, 14])
   @test A*X == B

   println("PASS")
end


function test_fmpz_mat_concat()
   print("fmpz_mat.concat...")

   S = MatrixSpace(ZZ, 3, 3)
   T = MatrixSpace(ZZ, 3, 6)
   U = MatrixSpace(ZZ, 6, 3)

   A = S([fmpz(2) 3 5; 1 4 7; 9 6 3])
   B = S([fmpz(1) 4 7; 9 6 7; 4 3 3])

   @test hcat(A, B) == T([2 3 5 1 4 7; 1 4 7 9 6 7; 9 6 3 4 3 3])

   @test vcat(A, B) == U([2 3 5; 1 4 7; 9 6 3; 1 4 7; 9 6 7; 4 3 3])

   println("PASS")
end

function test_fmpz_mat()
   test_fmpz_mat_constructors()
   test_fmpz_mat_printing()
   test_fmpz_mat_convert()
   test_fmpz_mat_manipulation()
   test_fmpz_mat_view()
   test_fmpz_mat_unary_ops()
   test_fmpz_mat_binary_ops()
   test_fmpz_mat_adhoc_binary()
   test_fmpz_mat_comparison()
   test_fmpz_mat_adhoc_comparison()
   test_fmpz_mat_powering()
   test_fmpz_mat_adhoc_exact_division()
   test_fmpz_mat_gram()
   test_fmpz_mat_trace()
   test_fmpz_mat_content()

   test_fmpz_mat_transpose()
   test_fmpz_mat_scaling()
   test_fmpz_mat_inversion()
   test_fmpz_mat_pseudo_inversion()
   test_fmpz_mat_exact_division()
   test_fmpz_mat_modular_reduction()
   test_fmpz_mat_det()
   test_fmpz_mat_hadamard()
   test_fmpz_mat_hnf()
   test_fmpz_mat_lll()
   test_fmpz_mat_nullspace()
   test_fmpz_mat_rank()
   test_fmpz_mat_rref()
   test_fmpz_mat_snf()
   test_fmpz_mat_solve_rational()
   test_fmpz_mat_solve()
   test_fmpz_mat_concat()

   println("")
end
