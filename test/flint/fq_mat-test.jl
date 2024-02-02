@testset "FqPolyRepMatrix.constructors" begin
  F4, a = Native.finite_field(ZZRingElem(2), 2, "a")
  F9, b = Native.finite_field(ZZRingElem(3), 2, "b")

  R = FqPolyRepMatrixSpace(F4, 2, 2)

  @test elem_type(R) == FqPolyRepMatrix
  @test elem_type(FqPolyRepMatrixSpace) == FqPolyRepMatrix
  @test parent_type(FqPolyRepMatrix) == FqPolyRepMatrixSpace
  @test nrows(R) == 2
  @test ncols(R) == 2

  @test isa(R, FqPolyRepMatrixSpace)

  @test base_ring(R) == F4

  S = FqPolyRepMatrixSpace(F9, 2, 2)

  @test isa(S, FqPolyRepMatrixSpace)

  RR = FqPolyRepMatrixSpace(F4, 2, 2)

  @test isa(RR, FqPolyRepMatrixSpace)

  @test R == RR

  @test_throws ErrorException FqPolyRepMatrixSpace(F4, 2, -1)
  @test_throws ErrorException FqPolyRepMatrixSpace(F4, -1, 2)
  @test_throws ErrorException FqPolyRepMatrixSpace(F4, -1, -1)

  a = R()

  @test isa(a, FqPolyRepMatrix)
  @test parent(a) == R

  ar = [ BigInt(1) BigInt(1); BigInt(1) BigInt(1) ]

  b = R(ar)

  @test isa(b, FqPolyRepMatrix)
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
  @test isa(c, FqPolyRepMatrix)
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

  @test isa(d, FqPolyRepMatrix)
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

  @test isa(d, FqPolyRepMatrix)

  ar = matrix_space(ZZ, 2, 2)([ 1 1; 1 1])

  e = R(ar)

  @test isa(e, FqPolyRepMatrix)
  @test parent(e) == R
  @test nrows(e) == 2 && ncols(e) == 2

  ar = matrix(FlintZZ, [ 1 1 1 ; 1 1 1; 1 1 1])

  @test_throws ErrorException R(ar)

  ar = [ F4(1) F4(1); F4(1) F4(1) ]

  f = R(ar)

  @test isa(f, FqPolyRepMatrix)
  @test parent(f) == R
  @test nrows(f) == 2 && ncols(f) == 2
  @test_throws ErrorConstrDimMismatch R(reshape(ar,4,1))
  @test f == R([F4(1), F4(1), F4(1), F4(1)])
  @test_throws ErrorConstrDimMismatch R([F4(1) F4(1) ])
  @test_throws ErrorConstrDimMismatch R([F4(1) F4(1) ; F4(1) F4(1) ; F4(1) F4(1) ])
  @test_throws ErrorConstrDimMismatch R([F4(1), F4(1), F4(1)])
  @test_throws ErrorConstrDimMismatch R([F4(1), F4(1), F4(1), F4(1), F4(1)])

  @test isa(S(1), FqPolyRepMatrix)

  @test isa(S(ZZRingElem(1)), FqPolyRepMatrix)

  @test isa(S(F9(1)), FqPolyRepMatrix)

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
      @test isa(M, FqPolyRepMatrix)
      @test base_ring(M) == F9

      M2 = matrix(F9, 2, 3, map(T, arr2))
      @test isa(M2, FqPolyRepMatrix)
      @test base_ring(M2) == F9
      @test nrows(M2) == 2
      @test ncols(M2) == 3
      @test_throws ErrorConstrDimMismatch matrix(F9, 2, 2, map(T, arr2))
      @test_throws ErrorConstrDimMismatch matrix(F9, 2, 4, map(T, arr2))
   end

   M3 = zero_matrix(F9, 2, 3)

   @test isa(M3, FqPolyRepMatrix)
   @test base_ring(M3) == F9

   M4 = identity_matrix(F9, 3)

   @test isa(M4, FqPolyRepMatrix)
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
end

@testset "FqPolyRepMatrix.similar" begin
   F9, b = Native.finite_field(ZZRingElem(3), 2, "b")
   S = matrix_space(F9, 3, 3)
   s = S(ZZRingElem(3))

   t = similar(s)
   @test t isa FqPolyRepMatrix
   @test size(t) == size(s)

   t = similar(s, 2, 3)
   @test t isa FqPolyRepMatrix
   @test size(t) == (2, 3)

   for (R, M) in ring_to_mat
      t = similar(s, R)
      @test size(t) == size(s)

      t = similar(s, R, 2, 3)
      @test size(t) == (2, 3)
   end

   # issue #651
   m = one(Generic.MatSpace{FqPolyRepFieldElem}(F9, 2, 2))
   for n = (m, -m, m*m, m+m, 2m)
      @test n isa Generic.MatSpaceElem{FqPolyRepFieldElem}
   end
end

@testset "FqPolyRepMatrix.printing" begin
  F4, _  = Native.finite_field(ZZRingElem(2), 2, "a")
  R = FqPolyRepMatrixSpace(F4, 2, 2)

  a = R(1)

  # test that default Julia printing is not used
  @test !occursin(string(typeof(a)), string(a))
end

@testset "FqPolyRepMatrix.manipulation" begin
  F4, _ = Native.finite_field(ZZRingElem(2), 2, "a")
  R = FqPolyRepMatrixSpace(F4, 2, 2)
  F9, _ = Native.finite_field(ZZRingElem(3), 2, "b")
  S = FqPolyRepMatrixSpace(F9, 2, 2)

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

  @test isa(d, FqPolyRepMatrix)

  e = zero(R)

  @test isa(e, FqPolyRepMatrix)

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

@testset "FqPolyRepMatrix.unary_ops" begin
  F17, _ = Native.finite_field(ZZRingElem(17), 1, "a")

  R = matrix_space(F17, 3, 4)
  RR = matrix_space(F17, 4, 3)

  a = R([ 1 2 3 1; 3 2 1 2; 1 3 2 0])

  b = R([ 2 1 0 1; 0 0 0 0; 0 1 2 0 ])

  c = R()

  d = -b

  @test d == R([ 15 16 0 16; 0 0 0 0; 0 16 15 0])
end

@testset "FqPolyRepMatrix.binary_ops" begin
  F17, _ = Native.finite_field(ZZRingElem(17), 1, "a")
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

  S = matrix_space(F17, 3, 3)
  A = S([2 3 5; 1 4 7; 9 6 3])
  v = map(F17, [1, 2, 3])

  @test A*v == [23, 30, 30]
  @test v*A == [31, 29, 28]
end

@testset "FqPolyRepMatrix.row_col_swapping" begin
   R, _ = Native.finite_field(ZZRingElem(17), 1, "a")

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

@testset "FqPolyRepMatrix.adhoc_binary" begin
  F17, _ = Native.finite_field(ZZRingElem(17), 1, "a")

  R = matrix_space(F17, 3, 4)
  F2, _ = Native.finite_field(ZZRingElem(2), 1, "a")

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

@testset "FqPolyRepMatrix.comparison" begin
  F17, _ = Native.finite_field(ZZRingElem(17), 1, "a")

  R = matrix_space(F17, 3, 4)

  a = R([ 1 2 3 1; 3 2 1 2; 1 3 2 0])

  @test a == a

  @test deepcopy(a) == a

  @test a != R([0 1 3 1; 2 1 4 2; 1 1 1 1])
end

@testset "FqPolyRepMatrix.adhoc_comparison" begin
  F17, _ = Native.finite_field(ZZRingElem(17), 1, "a")

  R = matrix_space(F17, 3, 4)

  @test R(5) == 5
  @test R(5) == ZZRingElem(5)
  @test R(5) == F17(5)

  @test 5 == R(5)
  @test ZZRingElem(5) == R(5)
  @test F17(5) == R(5)
end

@testset "FqPolyRepMatrix.powering" begin
  F17, _ = Native.finite_field(ZZRingElem(17), 1, "a")

  R = matrix_space(F17, 3, 4)

  a = R([ 1 2 3 1; 3 2 1 2; 1 3 2 0])

  f = a*transpose(a)

  g = f^1000

  @test g == matrix_space(F17, 3, 3)([1 2 2; 2 13 12; 2 12 15])
end

@testset "FqPolyRepMatrix.row_echelon_form" begin
  F17, _ = Native.finite_field(ZZRingElem(17), 1, "a")
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

@testset "FqPolyRepMatrix.trace_det" begin
  F17, _ = Native.finite_field(ZZRingElem(17), 1, "a")
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
end

@testset "FqPolyRepMatrix.rank" begin
  F17, _ = Native.finite_field(ZZRingElem(17), 1, "a")
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

@testset "FqPolyRepMatrix.inv" begin
  F17, _ = Native.finite_field(ZZRingElem(17), 1, "a")
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

@testset "FqPolyRepMatrix.solve" begin
  F17, _ = Native.finite_field(ZZRingElem(17), 1, "a")
  R = matrix_space(F17, 3, 3)
  S = matrix_space(F17, 3, 4)

  a = R([ 1 2 3 ; 3 2 1 ; 0 0 2 ])

  b = S([ 2 1 0 1; 0 0 0 0; 0 1 2 0 ])

  c = a*b

  d = solve(a,c)

  @test d == b

  a = zero(R)

  @test_throws ErrorException  solve(a,c)

   for i in 1:10
      m = rand(0:10)
      n = rand(0:10)
      k = rand(0:10)

      M = matrix_space(F17, n, k)
      N = matrix_space(F17, n, m)

      A = rand(M)
      B = rand(N)

      fl, X = can_solve_with_solution(A, B)

      if fl
         @test A * X == B
      end
   end

   A = matrix(F17, 2, 2, [1, 2, 2, 5])
   B = matrix(F17, 2, 1, [1, 2])
   fl, X = can_solve_with_solution(A, B)
   @test fl
   @test A * X == B
   @test can_solve(A, B)

   A = matrix(F17, 2, 2, [1, 2, 2, 4])
   B = matrix(F17, 2, 1, [1, 2])
   fl, X = can_solve_with_solution(A, B)
   @test fl
   @test A * X == B
   @test can_solve(A, B)

   A = matrix(F17, 2, 2, [1, 2, 2, 4])
   B = matrix(F17, 2, 1, [1, 3])
   fl, X = can_solve_with_solution(A, B)
   @test !fl
   @test !can_solve(A, B)

   A = zero_matrix(F17, 2, 3)
   B = identity_matrix(F17, 3)
   @test_throws ErrorException can_solve_with_solution(A, B)

   A = transpose(matrix(F17, 2, 2, [1, 2, 2, 5]))
   B = transpose(matrix(F17, 2, 1, [1, 2]))
   fl, X = can_solve_with_solution(A, B, side = :left)
   @test fl
   @test X * A == B
   @test can_solve(A, B, side = :left)

   A = transpose(matrix(F17, 2, 2, [1, 2, 2, 4]))
   B = transpose(matrix(F17, 2, 1, [1, 2]))
   fl, X = can_solve_with_solution(A, B, side = :left)
   @test fl
   @test X * A == B
   @test can_solve(A, B, side = :left)

   A = transpose(matrix(F17, 2, 2, [1, 2, 2, 4]))
   B = transpose(matrix(F17, 2, 1, [1, 3]))
   fl, X = can_solve_with_solution(A, B, side = :left)
   @test !fl
   @test !can_solve(A, B, side = :left)

   A = transpose(zero_matrix(F17, 2, 3))
   B = transpose(identity_matrix(F17, 3))
   @test_throws ErrorException can_solve_with_solution(A, B, side = :left)

   @test_throws ErrorException can_solve_with_solution(A, B, side = :garbage)
   @test_throws ErrorException can_solve(A, B, side = :garbage)
end

@testset "FqPolyRepMatrix.lu" begin

  F17, _ = Native.finite_field(ZZRingElem(17), 1, "a")
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

@testset "FqPolyRepMatrix.view" begin
  F17, _ = Native.finite_field(ZZRingElem(17), 1, "a")
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
end

@testset "FqPolyRepMatrix.sub" begin
   F17, _ = Native.finite_field(ZZRingElem(17), 1, "a")
   S = matrix_space(F17, 3, 3)

   A = S([1 2 3; 4 5 6; 7 8 9])

   B = @inferred sub(A, 1, 1, 2, 2)

   @test typeof(B) == FqPolyRepMatrix
   @test B == matrix_space(F17, 2, 2)([1 2; 4 5])

   B[1, 1] = 10
   @test A == S([1 2 3; 4 5 6; 7 8 9])

   C = @inferred sub(B, 1:2, 1:2)

   @test typeof(C) == FqPolyRepMatrix
   @test C == matrix_space(F17, 2, 2)([10 2; 4 5])

   C[1, 1] = 20
   @test B == matrix_space(F17, 2, 2)([10 2; 4 5])
   @test A == S([1 2 3; 4 5 6; 7 8 9])
end

@testset "FqPolyRepMatrix.concatenation" begin
  F17, _ = Native.finite_field(ZZRingElem(17), 1, "a")
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

@testset "FqPolyRepMatrix.conversion" begin
  F17, _ = Native.finite_field(ZZRingElem(17), 1, "a")
  R = matrix_space(F17, 3, 3)

  a = R([ 1 2 3 ; 3 2 1 ; 0 0 2 ])

  @test Array(a) == [F17(1) F17(2) F17(3);
                     F17(3) F17(2) F17(1);
                     F17(0) F17(0) F17(2) ]
end

@testset "FqPolyRepMatrix.charpoly" begin
   F17, _ = Native.finite_field(ZZRingElem(17), 1, "a")

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

@testset "FqPolyRepMatrix.rand" begin
   F17, _ = Native.finite_field(ZZRingElem(17), 1, "a")
   S = matrix_space(F17, 3, 3)
   M = rand(S)
   @test parent(M) == S
end
