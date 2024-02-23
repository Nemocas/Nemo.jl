@testset "FqMatrix.constructors" begin
  F4, a = finite_field(ZZRingElem(2), 2, "a")
  F9, b = finite_field(ZZRingElem(3), 2, "b")

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

  @test_throws ErrorException FqMatrixSpace(F4, 2, -1)
  @test_throws ErrorException FqMatrixSpace(F4, -1, 2)
  @test_throws ErrorException FqMatrixSpace(F4, -1, -1)

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

  ar = matrix(FlintZZ, [ 1 1 1 ; 1 1 1; 1 1 1])

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

   for R in [FlintZZ, residue_ring(FlintZZ, 23)[1], residue_ring(FlintZZ, ZZ(23))[1], Native.GF(23)]
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
   m = one(Generic.MatSpace{FqFieldElem}(F9, 2, 2))
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

   A = matrix(F17, [1 2 3; 4 5 6])
   C = solve_init(A)
   B = matrix(F17, 2, 1, [1, 1])
   fl, x, K = can_solve_with_solution_and_kernel(C, B, side = :right)
   @test fl
   @test A*x == B
   @test is_zero(A*K)
   @test ncols(K) + rank(A) == ncols(A)

   B = matrix(F17, 1, 3, [1, 2, 3])
   fl, x, K = can_solve_with_solution_and_kernel(C, B)
   @test fl
   @test x*A == B
   @test is_zero(K*A)
   @test nrows(K) + rank(A) == nrows(A)
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
  @test length(v) == 3
  v[2] = 7
  @test A == S([1 2 3; 4 7 6; 7 8 9])
  A = S([1 2 3; 4 5 6; 7 8 9])
  v = @view A[:, 3]
  @test length(v) == 3
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
