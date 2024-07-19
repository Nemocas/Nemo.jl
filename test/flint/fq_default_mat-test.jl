@testset "FqMatrix.constructors" begin
  F4, a = finite_field(ZZRingElem(2), 2, "a")
  F9, b = finite_field(ZZRingElem(3), 2, "b")

  @test_throws ErrorException matrix_space(F4, -1, 5)
  @test_throws ErrorException matrix_space(F4, 0, -2)
  @test_throws ErrorException matrix_space(F4, -3, -4)
  @test_throws ErrorException FqMatrixSpace(F4, 2, -1)
  @test_throws ErrorException FqMatrixSpace(F4, -1, 2)
  @test_throws ErrorException FqMatrixSpace(F4, -1, -1)

  R = FqMatrixSpace(F4, 2, 2)

  @test elem_type(R) == FqMatrix
  @test elem_type(FqMatrixSpace) == FqMatrix
  @test parent_type(FqMatrix) == FqMatrixSpace
  @test nrows(R) == 2
  @test ncols(R) == 2

  @test isa(R, FqMatrixSpace)

  @test base_ring(R) == F4

  S = FqMatrixSpace(F9, 2, 2)

  @test isa(S, FqMatrixSpace)

  RR = FqMatrixSpace(F4, 2, 2)

  @test isa(RR, FqMatrixSpace)

  @test R == RR

  a = R()

  @test isa(a, FqMatrix)
  @test parent(a) == R

  ar = [ BigInt(1) BigInt(1); BigInt(1) BigInt(1) ]

  b = R(ar)

  @test isa(b, FqMatrix)
  @test parent(b) == R
  @test nrows(b) == 2 && ncols(b) == 2
  @test_throws ErrorConstrDimMismatch R(reshape(ar,1,4))
  @test b == R([BigInt(1), BigInt(1), BigInt(1), BigInt(1)])
  @test_throws ErrorConstrDimMismatch R([BigInt(1) BigInt(1)])
  @test_throws ErrorConstrDimMismatch R([BigInt(1) BigInt(1) ; BigInt(1) BigInt(1) ;
                                         BigInt(1) BigInt(1)])
  @test_throws ErrorConstrDimMismatch R([BigInt(1), BigInt(1), BigInt(1)])
  @test_throws ErrorConstrDimMismatch R([BigInt(1), BigInt(1),
                                         BigInt(1), BigInt(1), BigInt(1)])

  ar = [ ZZ(1) ZZ(1); ZZ(1) ZZ(1) ]

  c = R(ar)
  @test isa(c, FqMatrix)
  @test parent(c) == R
  @test nrows(c) == 2 && ncols(c) == 2
  @test_throws ErrorConstrDimMismatch R(reshape(ar,4,1))
  @test c == R([ ZZ(1), ZZ(1), ZZ(1), ZZ(1)])
  @test_throws ErrorConstrDimMismatch R([ZZ(1) ZZ(1)])
  @test_throws ErrorConstrDimMismatch R([ZZ(1) ZZ(1) ; ZZ(1) ZZ(1) ; ZZ(1) ZZ(1)])
  @test_throws ErrorConstrDimMismatch R([ZZ(1), ZZ(1), ZZ(1)])
  @test_throws ErrorConstrDimMismatch R([ZZ(1), ZZ(1), ZZ(1), ZZ(1), ZZ(1)])

  ar = [ 1 1; 1 1]

  d = R(ar)

  @test isa(d, FqMatrix)
  @test parent(d) == R
  @test nrows(d) == 2 && ncols(d) == 2
  @test_throws ErrorConstrDimMismatch R(reshape(ar,1,4))
  @test d == R([1,1,1,1])
  @test_throws ErrorConstrDimMismatch R([1 1 ])
  @test_throws ErrorConstrDimMismatch R([1 1 ; 1 1 ; 1 1 ])
  @test_throws ErrorConstrDimMismatch R([1, 1, 1])
  @test_throws ErrorConstrDimMismatch R([1, 1, 1, 1, 1])

  ar = [ 1 1; 1 1]'

  d = R(ar)

  @test isa(d, FqMatrix)

  ar = matrix_space(ZZ, 2, 2)([ 1 1; 1 1])

  e = R(ar)

  @test isa(e, FqMatrix)
  @test parent(e) == R
  @test nrows(e) == 2 && ncols(e) == 2

  ar = matrix(ZZ, [ 1 1 1 ; 1 1 1; 1 1 1])

  @test_throws ErrorException R(ar)

  ar = [ F4(1) F4(1); F4(1) F4(1) ]

  f = R(ar)

  @test isa(f, FqMatrix)
  @test parent(f) == R
  @test nrows(f) == 2 && ncols(f) == 2
  @test_throws ErrorConstrDimMismatch R(reshape(ar,4,1))
  @test f == R([F4(1), F4(1), F4(1), F4(1)])
  @test_throws ErrorConstrDimMismatch R([F4(1) F4(1) ])
  @test_throws ErrorConstrDimMismatch R([F4(1) F4(1) ; F4(1) F4(1) ; F4(1) F4(1) ])
  @test_throws ErrorConstrDimMismatch R([F4(1), F4(1), F4(1)])
  @test_throws ErrorConstrDimMismatch R([F4(1), F4(1), F4(1), F4(1), F4(1)])

  @test isa(S(1), FqMatrix)

  @test isa(S(ZZRingElem(1)), FqMatrix)

  @test isa(S(F9(1)), FqMatrix)

  g = deepcopy(e)

  @test b == c
  @test c == d
  @test d == e
  @test e == f
  @test g == e

  arr = [1 2; 3 4]
  arr2 = [1, 2, 3, 4, 5, 6]

  for T in [F9, ZZRingElem, Int, BigInt]
    M = matrix(F9, map(T, arr))
    @test isa(M, FqMatrix)
    @test base_ring(M) == F9

    M2 = matrix(F9, 2, 3, map(T, arr2))
    @test isa(M2, FqMatrix)
    @test base_ring(M2) == F9
    @test nrows(M2) == 2
    @test ncols(M2) == 3
    @test_throws ErrorConstrDimMismatch matrix(F9, 2, 2, map(T, arr2))
    @test_throws ErrorConstrDimMismatch matrix(F9, 2, 4, map(T, arr2))
  end

  M3 = zero_matrix(F9, 2, 3)

  @test isa(M3, FqMatrix)
  @test base_ring(M3) == F9

  M4 = identity_matrix(F9, 3)

  @test isa(M4, FqMatrix)
  @test base_ring(M4) == F9

  a = zero_matrix(F9, 2, 2)
  b = zero_matrix(F9, 2, 3)
  @test a in [a, b]
  @test a in [b, a]
  @test !(a in [b])
  @test a in keys(Dict(a => 1))
  @test !(a in keys(Dict(b => 1)))

  a = zero_matrix(F4, 2, 2)
  b = zero_matrix(F9, 2, 2)
  @test a in [a, b]
  @test a in [b, a]
  @test !(a in [b])
  @test a in keys(Dict(a => 1))
  @test !(a in keys(Dict(b => 1)))

  R, x = finite_field(ZZRingElem(23), 5, "x")
  S = matrix_space(R, 2, 2)

  for R in [ZZ, residue_ring(ZZ, 23)[1], residue_ring(ZZ, ZZ(23))[1], Native.GF(23)]
    M = matrix(R, 2, 2, [1, 2, 3, 4])

    @test isa(S(M), MatElem)
  end
end

@testset "FqMatrix.similar" begin
  F9, b = finite_field(ZZRingElem(3), 2, "b")
  S = matrix_space(F9, 3, 3)
  s = S(ZZRingElem(3))

  t = similar(s)
  @test t isa FqMatrix
  @test size(t) == size(s)

  t = similar(s, 2, 3)
  @test t isa FqMatrix
  @test size(t) == (2, 3)

  for (R, M) in ring_to_mat
    t = similar(s, R)
    @test size(t) == size(s)

    t = similar(s, R, 2, 3)
    @test size(t) == (2, 3)
  end

  # issue #651
  m = one(Generic.MatSpaceElem{FqFieldElem}(F9, 2, 2))
  for n = (m, -m, m*m, m+m, 2m)
    @test n isa Generic.MatSpaceElem{FqFieldElem}
  end
end

@testset "FqMatrix.printing" begin
  F4, _  = finite_field(ZZRingElem(2), 2, "a")
  R = FqMatrixSpace(F4, 2, 2)

  a = R(1)

  # test that default Julia printing is not used
  @test !occursin(string(typeof(a)), string(a))
end

@testset "FqMatrix.manipulation" begin
  F4, _ = finite_field(ZZRingElem(2), 2, "a")
  R = FqMatrixSpace(F4, 2, 2)
  F9, _ = finite_field(ZZRingElem(3), 2, "b")
  S = FqMatrixSpace(F9, 2, 2)

  ar = [ 1 2; 3 4]

  a = R(ar)
  aa = S(ar)

  @test nrows(a) == 2
  @test ncols(a) == 2

  b = deepcopy(a)

  c = R([ 1 3; 2 4])

  @test a[1,1] == F4(1)

  a[1,1] = UInt(2)

  @test a[1,1] == F4(2)
  @test_throws BoundsError a[0,-1] = F4(2)

  a[2,1] = ZZ(3)

  @test a[2,1] == F4(ZZ(3))
  @test_throws BoundsError a[-10,-10] = ZZ(3)

  a[2,2] = F4(4)

  @test a[2,2] == F4(4)
  @test_throws BoundsError a[-2,2] = F4(4)

  a[1,2] = 5

  @test a[1,2] == F4(5)
  @test_throws BoundsError a[-2,2] = 5

  @test a != b

  d = one(R)

  @test isa(d, FqMatrix)

  e = zero(R)

  @test isa(e, FqMatrix)

  @test iszero(e)

  @test_throws ErrorException one(matrix_space(F9, 1, 2))

  @test is_square(a)

  @test a == a
  @test a == deepcopy(a)
  @test a != aa

  @test transpose(b) == c

  @test transpose(matrix_space(F4,1,2)([ 1 2; ])) ==
  matrix_space(F4,2,1)(reshape([ 1 ; 2],2,1))

  @test_throws ErrorConstrDimMismatch transpose!(R([ 1 2 ;]))

  C = F9[1 2 3; 4 5 6; 7 8 9]
  C[3, :] = F9[7 7 7]
  @test C == F9[1 2 3; 4 5 6; 7 7 7]

  C[:, 3] = F9[5; 5; 5]
  @test C == F9[1 2 5; 4 5 5; 7 7 5]

  C[1:2, 2:3] = F9[3 3; 3 3]
  @test C == F9[1 3 3; 4 3 3; 7 7 5]

  @test_throws DimensionMismatch C[1:2, 2:3] = F9[3 3]
  @test_throws BoundsError C[1:2, 3:4] = F9[3 3; 3 3]
end

@testset "FqMatrix.unary_ops" begin
  F17, _ = finite_field(ZZRingElem(17), 1, "a")

  R = matrix_space(F17, 3, 4)
  RR = matrix_space(F17, 4, 3)

  a = R([ 1 2 3 1; 3 2 1 2; 1 3 2 0])

  b = R([ 2 1 0 1; 0 0 0 0; 0 1 2 0 ])

  c = R()

  d = -b

  @test d == R([ 15 16 0 16; 0 0 0 0; 0 16 15 0])
end

@testset "FqMatrix.binary_ops" begin
  F17, _ = finite_field(ZZRingElem(17), 1, "a")

  R = matrix_space(F17, 3, 4)

  a = R([ 1 2 3 1; 3 2 1 2; 1 3 2 0])

  b = R([ 2 1 0 1; 0 0 0 0; 0 1 2 0 ])

  c = R()

  d = a + b

  @test d == R([3 3 3 2; 3 2 1 2; 1 4 4 0])

  d = a - b

  @test d == R([ 16 1 3 0; 3 2 1 2; 1 2 0 0 ])

  d = a*transpose(a)

  @test d == matrix_space(F17, 3, 3)([15 12 13; 12 1 11; 13 11 14])

  d = transpose(a)*a

  @test d == matrix_space(F17, 4, 4)([11 11 8 7; 11 0 14 6; 8 14 14 5; 7 6 5 5])

  a = F17[1 2 3 1; 3 2 1 2; 1 3 2 0]
  z = F17(2)
  b = similar(a)
  @test mul!(b, a, z) == a * z
  b = similar(a)
  @test mul!(b, z, a) == a * z

  F, _ = finite_field(ZZRingElem(17), 2, "a")
  a = F[1 2 3 1; 3 2 1 2; 1 3 2 0]
  z = F(2)
  b = similar(a)
  @test mul!(b, a, z) == a * z
  b = similar(a)
  @test mul!(b, z, a) == a * z
end

@testset "FqMatrix.row_col_swapping" begin
  R, _ = finite_field(ZZRingElem(17), 1, "a")

  a = matrix(R, [1 2; 3 4; 5 6])

  @test swap_rows(a, 1, 3) == matrix(R, [5 6; 3 4; 1 2])

  swap_rows!(a, 2, 3)

  @test a == matrix(R, [1 2; 5 6; 3 4])

  @test swap_cols(a, 1, 2) == matrix(R, [2 1; 6 5; 4 3])

  swap_cols!(a, 2, 1)

  @test a == matrix(R, [2 1; 6 5; 4 3])

  a = matrix(R, [1 2; 3 4])
  @test reverse_rows(a) == matrix(R, [3 4; 1 2])
  reverse_rows!(a)
  @test a == matrix(R, [3 4; 1 2])

  a = matrix(R, [1 2; 3 4])
  @test reverse_cols(a) == matrix(R, [2 1; 4 3])
  reverse_cols!(a)
  @test a == matrix(R, [2 1; 4 3])

  a = matrix(R, [1 2 3; 3 4 5; 5 6 7])

  @test reverse_rows(a) == matrix(R, [5 6 7; 3 4 5; 1 2 3])
  reverse_rows!(a)
  @test a == matrix(R, [5 6 7; 3 4 5; 1 2 3])

  a = matrix(R, [1 2 3; 3 4 5; 5 6 7])
  @test reverse_cols(a) == matrix(R, [3 2 1; 5 4 3; 7 6 5])
  reverse_cols!(a)
  @test a == matrix(R, [3 2 1; 5 4 3; 7 6 5])
end

@testset "FqMatrix.adhoc_binary" begin
  F17, _ = finite_field(ZZRingElem(17), 1, "a")

  R = matrix_space(F17, 3, 4)
  F2, _ = finite_field(ZZRingElem(2), 1, "a")

  a = R([ 1 2 3 1; 3 2 1 2; 1 3 2 0])

  b = R([ 2 1 0 1; 0 0 0 0; 0 1 2 0 ])

  c = R()
  d = BigInt(2)*a
  dd = a*BigInt(2)

  @test d == R([ 2 4 6 2; 6 4 2 4; 2 6 4 0])
  @test dd == R([ 2 4 6 2; 6 4 2 4; 2 6 4 0])


  d = 2*a
  dd = a*2

  @test d == R([ 2 4 6 2; 6 4 2 4; 2 6 4 0])
  @test dd == R([ 2 4 6 2; 6 4 2 4; 2 6 4 0])

  d = ZZ(2)*a
  dd = a*ZZ(2)

  @test d == R([ 2 4 6 2; 6 4 2 4; 2 6 4 0])
  @test dd == R([ 2 4 6 2; 6 4 2 4; 2 6 4 0])

  d = F17(2)*a
  dd = a*F17(2)

  @test d == R([ 2 4 6 2; 6 4 2 4; 2 6 4 0])
  @test dd == R([ 2 4 6 2; 6 4 2 4; 2 6 4 0])

  @test_throws ErrorException F2(1)*a
end

@testset "FqMatrix.comparison" begin
  F17, _ = finite_field(ZZRingElem(17), 1, "a")

  R = matrix_space(F17, 3, 4)

  a = R([ 1 2 3 1; 3 2 1 2; 1 3 2 0])

  @test a == a

  @test deepcopy(a) == a

  @test a != R([0 1 3 1; 2 1 4 2; 1 1 1 1])
end

@testset "FqMatrix.adhoc_comparison" begin
  F17, _ = finite_field(ZZRingElem(17), 1, "a")

  R = matrix_space(F17, 3, 4)

  @test R(5) == 5
  @test R(5) == ZZRingElem(5)
  @test R(5) == F17(5)

  @test 5 == R(5)
  @test ZZRingElem(5) == R(5)
  @test F17(5) == R(5)
end

@testset "FqMatrix.powering" begin
  F17, _ = finite_field(ZZRingElem(17), 1, "a")

  R = matrix_space(F17, 3, 4)

  a = R([ 1 2 3 1; 3 2 1 2; 1 3 2 0])

  f = a*transpose(a)

  g = f^1000

  @test g == matrix_space(F17, 3, 3)([1 2 2; 2 13 12; 2 12 15])
end

@testset "FqMatrix.row_echelon_form" begin
  F17, _ = finite_field(ZZRingElem(17), 1, "a")
  R = matrix_space(F17, 3, 4)
  RR = matrix_space(F17, 4, 3)

  a = R([ 1 2 3 1; 3 2 1 2; 1 3 2 0])

  b = R([ 2 1 0 1; 0 0 0 0; 0 1 2 0 ])

  b = transpose(b)

  c = a*transpose(a)

  r, d = rref(a)

  @test d == R([ 1 0 0 8; 0 1 0 15; 0 0 1 16])
  @test r == 3

  r = rref!(a)

  @test a == R([ 1 0 0 8; 0 1 0 15; 0 0 1 16])
  @test r == 3

  r, d = rref(b)

  @test d == parent(b)([ 1 0 0 ; 0 0 1; 0 0 0; 0 0 0])
  @test r == 2
end

@testset "FqMatrix.trace_det" begin
  F17, _ = finite_field(ZZRingElem(17), 1, "a")
  R = matrix_space(F17, 3, 4)
  RR = matrix_space(F17, 4, 3)

  a = R([ 1 2 3 1; 3 2 1 2; 1 3 2 0])

  aa = matrix_space(F17,3,3)([ 1 2 3; 3 2 1; 1 1 2])

  b = R([ 2 1 0 1; 0 0 0 0; 0 1 2 0 ])

  a = transpose(a)*a

  c = tr(a)

  @test c == F17(13)

  @test_throws ErrorException tr(b)

  c = det(a)

  @test c == zero(F17)

  @test_throws ErrorException det(b)

  c = det(aa)

  @test c == F17(13)

  a = R([ 1 2 3 1; 3 2 1 2; 1 3 2 0])

  @test is_one(zero_matrix(F17, 0, 0))
end

@testset "FqMatrix.rank" begin
  F17, _ = finite_field(ZZRingElem(17), 1, "a")
  R = matrix_space(F17, 3, 4)
  RR = matrix_space(F17, 4, 3)

  a = R([ 1 2 3 1; 3 2 1 2; 1 3 2 0])

  aa = matrix_space(F17,3,3)([ 1 2 3; 3 2 1; 1 1 2])

  b = R([ 2 1 0 1; 0 0 0 0; 0 1 2 0 ])

  c = rank(a)

  @test c == 3

  c = rank(aa)

  @test c == 3

  c = rank(b)

  @test c == 2
end

@testset "FqMatrix.inv" begin
  F17, _ = finite_field(ZZRingElem(17), 1, "a")
  R = matrix_space(F17, 3, 4)
  RR = matrix_space(F17, 4, 3)

  a = R([ 1 2 3 1; 3 2 1 2; 1 3 2 0])

  aa = matrix_space(F17,3,3)([ 1 2 3; 3 2 1; 1 1 2])

  b = R([ 2 1 0 1; 0 0 0 0; 0 1 2 0 ])

  c = inv(aa)

  @test c == parent(aa)([12 13 1; 14 13 15; 4 4 1])

  @test_throws ErrorException inv(a)

  @test_throws ErrorException inv(transpose(a)*a)
end

@testset "FqMatrix.solve" begin
  F17, _ = finite_field(ZZRingElem(17), 1, "a")
  R = matrix_space(F17, 3, 3)
  S = matrix_space(F17, 3, 4)

  a = R([ 1 2 3 ; 3 2 1 ; 0 0 2 ])

  @test AbstractAlgebra.Solve.matrix_normal_form_type(F17) === AbstractAlgebra.Solve.LUTrait()
  @test AbstractAlgebra.Solve.matrix_normal_form_type(a) === AbstractAlgebra.Solve.LUTrait()

  b = S([ 2 1 0 1; 0 0 0 0; 0 1 2 0 ])

  c = a*b

  d = solve(a, c, side = :right)

  @test d == b

  a = zero(R)

  @test_throws ArgumentError solve(a, c, side = :right)

  for i in 1:10
    m = rand(0:10)
    n = rand(0:10)
    k = rand(0:10)

    M = matrix_space(F17, n, k)
    N = matrix_space(F17, n, m)

    A = rand(M)
    B = rand(N)

    fl, X, K = can_solve_with_solution_and_kernel(A, B, side = :right)

    if fl
      @test A * X == B
      @test is_zero(A*K)
      @test rank(A) + ncols(K) == ncols(A)
    end
  end

  A = matrix(F17, 2, 2, [1, 2, 2, 5])
  B = matrix(F17, 2, 1, [1, 2])
  fl, X = can_solve_with_solution(A, B, side = :right)
  @test fl
  @test A * X == B
  @test can_solve(A, B, side = :right)

  A = matrix(F17, 2, 2, [1, 2, 2, 4])
  B = matrix(F17, 2, 1, [1, 2])
  fl, X = can_solve_with_solution(A, B, side = :right)
  @test fl
  @test A * X == B
  @test can_solve(A, B, side = :right)

  A = matrix(F17, 2, 2, [1, 2, 2, 4])
  B = matrix(F17, 2, 1, [1, 3])
  fl, X = can_solve_with_solution(A, B, side = :right)
  @test !fl
  @test !can_solve(A, B, side = :right)

  A = zero_matrix(F17, 2, 3)
  B = identity_matrix(F17, 3)
  @test_throws ErrorException can_solve_with_solution(A, B, side = :right)

  A = transpose(matrix(F17, 2, 2, [1, 2, 2, 5]))
  B = transpose(matrix(F17, 2, 1, [1, 2]))
  fl, X = can_solve_with_solution(A, B)
  @test fl
  @test X * A == B
  @test can_solve(A, B)

  A = transpose(matrix(F17, 2, 2, [1, 2, 2, 4]))
  B = transpose(matrix(F17, 2, 1, [1, 2]))
  fl, X = can_solve_with_solution(A, B)
  @test fl
  @test X * A == B
  @test can_solve(A, B)

  A = transpose(matrix(F17, 2, 2, [1, 2, 2, 4]))
  B = transpose(matrix(F17, 2, 1, [1, 3]))
  fl, X = can_solve_with_solution(A, B)
  @test !fl
  @test !can_solve(A, B)

  A = transpose(zero_matrix(F17, 2, 3))
  B = transpose(identity_matrix(F17, 3))
  @test_throws ErrorException can_solve_with_solution(A, B)

  @test_throws ArgumentError can_solve_with_solution(A, B, side = :garbage)
  @test_throws ArgumentError can_solve(A, B, side = :garbage)
end

@testset "FqMatrix.solve_context" begin
  F = GF(101)
  A = matrix(F, [1 2 3 4 5; 0 0 8 9 10; 0 0 0 14 15])
  C = solve_init(A)
  @test C isa AbstractAlgebra.solve_context_type(F)
  @test C isa AbstractAlgebra.solve_context_type(A)

  @test AbstractAlgebra.Solve.matrix_normal_form_type(C) === AbstractAlgebra.Solve.LUTrait()
  @test C isa AbstractAlgebra.solve_context_type(AbstractAlgebra.Solve.LUTrait(), FqFieldElem)
  @test C isa AbstractAlgebra.solve_context_type(AbstractAlgebra.Solve.LUTrait(), F())
  @test C isa AbstractAlgebra.solve_context_type(AbstractAlgebra.Solve.LUTrait(), FqField)
  @test C isa AbstractAlgebra.solve_context_type(AbstractAlgebra.Solve.LUTrait(), F)
  @test C isa AbstractAlgebra.solve_context_type(AbstractAlgebra.Solve.LUTrait(), typeof(A))
  @test C isa AbstractAlgebra.solve_context_type(AbstractAlgebra.Solve.LUTrait(), A)

  @test_throws ErrorException solve(C, [ F(1) ])
  @test_throws ErrorException solve(C, [ F(1) ], side = :right)
  @test_throws ErrorException solve(C, matrix(F, 1, 1, [ F(1) ]))
  @test_throws ErrorException solve(C, matrix(F, 1, 1, [ F(1) ]), side = :right)
  @test_throws ArgumentError solve(C, [ F(1), F(2), F(3) ], side = :test)
  @test_throws ArgumentError solve(C, matrix(F, 3, 1, [ F(1), F(2), F(3) ]), side = :test)

  for b in [ [ F(1), F(2), F(3) ],
            matrix(F, 3, 1, [ F(1), F(2), F(3) ]),
            matrix(F, 3, 2, [ F(1), F(2), F(3), F(4), F(5), F(6) ]) ]
    @test @inferred can_solve(C, b, side = :right)
    x = @inferred solve(C, b, side = :right)
    @test A*x == b
    fl, x = @inferred can_solve_with_solution(C, b, side = :right)
    @test fl
    @test A*x == b
    fl, x, K = @inferred can_solve_with_solution_and_kernel(C, b, side = :right)
    @test fl
    @test A*x == b
    @test is_zero(A*K)
    @test ncols(K) == 2
    K = @inferred kernel(C, side = :right)
    @test is_zero(A*K)
    @test ncols(K) == 2
  end

  for b in [ [ F(1), F(1), F(1), F(1), F(1) ],
            matrix(F, 1, 5, [ F(1), F(1), F(1), F(1), F(1) ]),
            matrix(F, 2, 5, [ F(1), F(1), F(1), F(1), F(1),
                             F(1), F(1), F(1), F(1), F(1) ]) ]
    @test_throws ArgumentError solve(C, b)
    @test @inferred !can_solve(C, b)
    fl, x = @inferred can_solve_with_solution(C, b)
    @test !fl
    fl, x, K = @inferred can_solve_with_solution_and_kernel(C, b)
    @test !fl
  end

  for b in [ [ F(1), F(2), F(3), F(4), F(5) ],
            matrix(F, 1, 5, [ F(1), F(2), F(3), F(4), F(5)]),
            matrix(F, 2, 5, [ F(1), F(2), F(3), F(4), F(5),
                             F(0), F(0), F(8), F(9), F(10) ]) ]
    @test @inferred can_solve(C, b)
    x = @inferred solve(C, b)
    @test x*A == b
    fl, x = @inferred can_solve_with_solution(C, b)
    @test fl
    @test x*A == b
    fl, x, K = @inferred can_solve_with_solution_and_kernel(C, b)
    @test fl
    @test x*A == b
    @test is_zero(K*A)
    @test nrows(K) == 0
    K = @inferred kernel(C)
    @test is_zero(K*A)
    @test nrows(K) == 0
  end

  N = zero_matrix(F, 2, 1)
  C = solve_init(N)
  b = zeros(F, 2)
  fl, x, K = @inferred can_solve_with_solution_and_kernel(C, b, side = :right)
  @test fl
  @test N*x == b
  @test K == identity_matrix(F, 1)
  K = @inferred kernel(C, side = :right)
  @test K == identity_matrix(F, 1)

  N = zero_matrix(F, 1, 2)
  C = solve_init(N)
  b = zeros(F, 1)
  fl, x, K = @inferred can_solve_with_solution_and_kernel(C, b, side = :right)
  @test fl
  @test N*x == b
  @test K == identity_matrix(F, 2) || K == swap_cols!(identity_matrix(F, 2), 1, 2)
  K = @inferred kernel(C, side = :right)
  @test K == identity_matrix(F, 2) || K == swap_cols!(identity_matrix(F, 2), 1, 2)
end

@testset "FqMatrix.kernel" begin
  F17, _ = finite_field(ZZRingElem(17), 1, "a")
  A = matrix(F17, [ 1 2 3 ; 4 5 6 ])
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
end

@testset "FqMatrix.lu" begin

  F17, _ = finite_field(ZZRingElem(17), 1, "a")
  R = matrix_space(F17, 3, 3)
  S = matrix_space(F17, 3, 4)

  a = R([ 1 2 3 ; 3 2 1 ; 0 0 2 ])

  b = S([ 2 1 0 1; 0 0 0 0; 0 1 2 0 ])

  r, P, l, u = lu(a)

  @test l*u == P*a

  r, P, l, u = lu(b)

  @test l*u == S([ 2 1 0 1; 0 1 2 0; 0 0 0 0])

  @test l*u == P*b

  c = matrix(F17, 6, 3, [0, 0, 1, 0, 0, 0, 0, 0, 0, 1, 0, 0, 0, 1, 0, 0, 0, 1])

  r, P, l, u = lu(c)

  @test r == 3
  @test l*u == P*c
end

@testset "FqMatrix.view" begin
  F17, _ = finite_field(ZZRingElem(17), 1, "a")
  R = matrix_space(F17, 3, 3)
  S = matrix_space(F17, 3, 4)

  a = R([ 1 2 3 ; 3 2 1 ; 0 0 2 ])

  b = S([ 2 1 0 1; 0 0 0 0; 0 1 2 0 ])

  t = view(a, 1, 1, 3, 3)

  @test t == a

  @test view(a, 1, 1, 3, 3) == view(a, 1:3, 1:3)
  @test view(a, 1, 1, 3, 3) == sub(a, 1, 1, 3, 3)
  @test view(a, 1, 1, 3, 3) == sub(a, 1:3, 1:3)

  t = view(a, 1, 1, 2, 2)

  @test t == F17[1 2; 3 2]

  t = view(a, 2, 2, 3, 2)

  @test t == transpose(F17[2 0;])

  @test view(a, 2, 2, 3, 2) == view(a, 2:3,  2:2)
  @test view(a, 2, 2, 3, 2) == sub(a, 2, 2, 3, 2)
  @test view(a, 2, 2, 3, 2) == sub(a, 2:3, 2:2)

  @test_throws BoundsError view(a, 2, 2, 5, 5)

  a = 0
  GC.gc()
  @test t[1, 1] == 2

  S = matrix_space(F17, 3, 3)
  A = S([1 2 3; 4 5 6; 7 8 9])
  v = @view A[2, :]
  @test v isa AbstractVector{elem_type(F17)}
  @test length(v) == 3
  @test v[1] == 4
  @test collect(v) == [4, 5, 6]
  v[2] = 7
  @test A == S([1 2 3; 4 7 6; 7 8 9])
  A = S([1 2 3; 4 5 6; 7 8 9])
  v = @view A[:, 3]
  @test v isa AbstractVector{elem_type(F17)}
  @test length(v) == 3
  @test v[3] == 9
  @test collect(v) == [3, 6, 9]
  v[1] = 1
  @test A == S([1 2 1; 4 5 6; 7 8 9])
end

@testset "FqMatrix.sub" begin
  F17, _ = finite_field(ZZRingElem(17), 1, "a")
  S = matrix_space(F17, 3, 3)

  A = S([1 2 3; 4 5 6; 7 8 9])

  B = @inferred sub(A, 1, 1, 2, 2)

  @test typeof(B) == FqMatrix
  @test B == matrix_space(F17, 2, 2)([1 2; 4 5])

  B[1, 1] = 10
  @test A == S([1 2 3; 4 5 6; 7 8 9])

  C = @inferred sub(B, 1:2, 1:2)

  @test typeof(C) == FqMatrix
  @test C == matrix_space(F17, 2, 2)([10 2; 4 5])

  C[1, 1] = 20
  @test B == matrix_space(F17, 2, 2)([10 2; 4 5])
  @test A == S([1 2 3; 4 5 6; 7 8 9])
end

@testset "FqMatrix.concatenation" begin
  F17, _ = finite_field(ZZRingElem(17), 1, "a")
  R = matrix_space(F17, 3, 3)
  S = matrix_space(F17, 3, 4)

  a = R([ 1 2 3 ; 3 2 1 ; 0 0 2 ])

  b = S([ 2 1 0 1; 0 0 0 0; 0 1 2 0 ])

  c = hcat(a,a)

  @test c == matrix_space(F17, 3, 6)([1, 2, 3, 1, 2, 3,
                                      3, 2, 1, 3, 2, 1,
                                      0, 0, 2, 0, 0, 2])

  c = hcat(a,b)

  @test c == matrix_space(F17, 3, 7)([1, 2, 3, 2, 1, 0, 1,
                                      3, 2, 1, 0, 0, 0, 0,
                                      0, 0, 2, 0, 1, 2, 0])

  @test_throws ErrorException c = hcat(a,transpose(b))

  c = vcat(a,transpose(b))

  @test c == matrix_space(F17, 7, 3)([1, 2, 3,
                                      3, 2, 1,
                                      0, 0, 2,
                                      2, 0, 0,
                                      1, 0, 1,
                                      0, 0, 2,
                                      1, 0, 0])

  @test_throws ErrorException vcat(a,b)
end

@testset "FqMatrix.conversion" begin
  F17, _ = finite_field(ZZRingElem(17), 1, "a")
  R = matrix_space(F17, 3, 3)

  a = R([ 1 2 3 ; 3 2 1 ; 0 0 2 ])

  @test Array(a) == [F17(1) F17(2) F17(3);
                     F17(3) F17(2) F17(1);
                     F17(0) F17(0) F17(2) ]
end

@testset "FqMatrix.charpoly" begin
  F17, _ = finite_field(ZZRingElem(17), 1, "a")

  for dim = 0:5
    S = matrix_space(F17, dim, dim)
    U, x = polynomial_ring(F17, "x")

    for i = 1:10
      M = rand(S)
      N = deepcopy(M)

      p1 = charpoly(U, M)
      p2 = charpoly_danilevsky!(U, M)

      @test p1 == p2
      @test iszero(subst(p1, N))
    end
  end
end

@testset "FqMatrix.rand" begin
  F17, _ = finite_field(ZZRingElem(17), 1, "a")
  S = matrix_space(F17, 3, 3)
  M = rand(S)
  @test parent(M) == S
end

@testset "FqMatrix.pointer_fun" begin
  ps = ZZRingElem[2, 1073741827, 1180591620717411303449]
  degs = [1, 2]
  for p in ps
    for d in degs
      F, = finite_field(p, d)
      A = matrix(F, 2, 2, [rand(F) for i in 1:4])
      GC.@preserve A begin
        for i in 1:2
          for j in 1:2
            z = Nemo.fq_default_mat_entry_ptr(A, i, j)
            zz = unsafe_load(z)
            zz.parent = F
            @test zz == A[i, j]
          end
        end
      end
    end
  end
end

@testset "FqMatrix.add_one!" begin
  # This is accessing the matrix via pointers, so test all different
  # flint "backends"
  for p in ZZRingElem[2, 1073741827, 1180591620717411303449]
    for d in [1, 2]
      F = GF(p, d)
      A = F[p - 2 0; 0 0]
      Generic.add_one!(A, 1, 1)
      @test A == F[p - 1 0; 0 0]
      # Make sure reduction works
      Generic.add_one!(A, 1, 1)
      @test A == F[0 0; 0 0]
      @test_throws BoundsError Generic.add_one!(A, 3, 1)
    end
  end
end
