@testset "FpMatrix.constructors" begin
  Z2 = Native.GF(ZZ(2))
  Z3 = Native.GF(ZZ(3))

  R = FpMatrixSpace(Z2, 2, 2)

  @test elem_type(R) == FpMatrix
  @test elem_type(FpMatrixSpace) == FpMatrix
  @test parent_type(FpMatrix) == FpMatrixSpace
  @test dense_matrix_type(elem_type(Z2)) == elem_type(R)
  @test nrows(R) == 2
  @test ncols(R) == 2

  @test isa(R, FpMatrixSpace)

  @test base_ring(R) == Z2

  S = FpMatrixSpace(Z3, 2, 2)

  @test isa(S, FpMatrixSpace)

  RR = FpMatrixSpace(Z2, 2, 2)

  @test isa(RR, FpMatrixSpace)

  @test R == RR

  @test_throws ErrorException FpMatrixSpace(Z2, 2, -1)
  @test_throws ErrorException FpMatrixSpace(Z2, -1, 2)
  @test_throws ErrorException FpMatrixSpace(Z2, -1, -1)

  a = R()

  @test isa(a, FpMatrix)
  @test parent(a) == R

  ar = [ BigInt(1) BigInt(1); BigInt(1) BigInt(1) ]

  b = R(ar)

  @test isa(b, FpMatrix)
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
  @test isa(c, FpMatrix)
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

  @test isa(d, FpMatrix)
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

  @test isa(d, FpMatrix)

  @test_throws ErrorConstrDimMismatch R([Z2(1) Z2(1) ])
  @test_throws ErrorConstrDimMismatch R([Z2(1) Z2(1) ; Z2(1) Z2(1) ; Z2(1) Z2(1) ])
  @test_throws ErrorConstrDimMismatch R([Z2(1), Z2(1), Z2(1)])
  @test_throws ErrorConstrDimMismatch R([Z2(1), Z2(1), Z2(1), Z2(1), Z2(1)])

  @test isa(S(1), FpMatrix)

  @test isa(S(ZZRingElem(1)), FpMatrix)

  @test isa(S(Z3(1)), FpMatrix)

  @test b == c
  @test c == d

   arr = [1 2; 3 4]
   arr2 = [1, 2, 3, 4, 5, 6]

   for T in [Z3, ZZRingElem, Int, BigInt]
      M = matrix(Z3, map(T, arr))
      @test isa(M, FpMatrix)
      @test base_ring(M) == Z3

      M2 = matrix(Z3, 2, 3, map(T, arr2))
      @test isa(M2, FpMatrix)
      @test base_ring(M2) == Z3
      @test nrows(M2) == 2
      @test ncols(M2) == 3
      @test_throws ErrorConstrDimMismatch matrix(Z3, 2, 2, map(T, arr2))
      @test_throws ErrorConstrDimMismatch matrix(Z3, 2, 4, map(T, arr2))
   end

   M3 = zero_matrix(Z3, 2, 3)

   @test isa(M3, FpMatrix)
   @test base_ring(M3) == Z3

   M4 = identity_matrix(Z3, 3)

   @test isa(M4, FpMatrix)
   @test base_ring(M4) == Z3

   a = zero_matrix(Z2, 2, 2)
   b = zero_matrix(Z2, 2, 3)
   @test a in [a, b]
   @test a in [b, a]
   @test !(a in [b])
   @test a in keys(Dict(a => 1))
   @test !(a in keys(Dict(b => 1)))

   a = zero_matrix(Z2, 2, 2)
   b = zero_matrix(Z3, 2, 2)
   @test a in [a, b]
   @test a in [b, a]
   @test !(a in [b])
   @test a in keys(Dict(a => 1))
   @test !(a in keys(Dict(b => 1)))
end

@testset "FpMatrix.similar" begin
   Z13 = Native.GF(ZZ(13))
   S = FpMatrixSpace(Z13, 2, 2)
   s = S(ZZRingElem(3))

   t = similar(s)
   @test t isa FpMatrix
   @test size(t) == size(s)

   t = similar(s, 2, 3)
   @test t isa FpMatrix
   @test size(t) == (2, 3)

   for (R, M) in ring_to_mat
      t = similar(s, R)
      @test size(t) == size(s)

      t = similar(s, R, 2, 3)
      @test size(t) == (2, 3)
   end
end

@testset "FpMatrix.printing" begin
  Z2 = Native.GF(ZZ(2))
  R = FpMatrixSpace(Z2, 2, 2)

  a = R(1)

  # test that default Julia printing is not used
  @test !occursin(string(typeof(a)), string(a))
end

@testset "FpMatrix.manipulation" begin
  Z11 = Native.GF(ZZ(11))
  R = FpMatrixSpace(Z11, 2, 2)
  Z23 = Native.GF(ZZ(23))
  S = FpMatrixSpace(Z23, 2, 2)

  ar = [ 1 2; 3 4]

  a = R(ar)
  aa = S(ar)

  @test nrows(a) == 2
  @test ncols(a) == 2

  b = deepcopy(a)

  c = R([ 1 3; 2 4])

  @test a[1,1] == Z11(1)

  a[1,1] = UInt(2)

  @test a[1,1] == Z11(2)
  @test_throws BoundsError a[0,-1] = Z11(2)

  a[2,1] = ZZ(3)

  @test a[2,1] == Z11(ZZ(3))
  @test_throws BoundsError a[-10,-10] = ZZ(3)

  a[2,2] = Z11(4)

  @test a[2,2] == Z11(4)
  @test_throws BoundsError a[-2,2] = Z11(4)

  a[1,2] = 5

  @test a[1,2] == Z11(5)
  @test_throws BoundsError a[-2,2] = 5

  @test a != b

  d = one(R)

  @test isa(d, FpMatrix)

  e = zero(R)

  @test isa(e, FpMatrix)

  @test iszero(e)

  @test_throws ErrorException one(matrix_space(Native.GF(ZZ(2)), 1, 2))

  @test is_square(a)

  @test a == a
  @test a == deepcopy(a)
  @test a != aa

  @test transpose(b) == c

  transpose!(b)

  @test b == c

  @test transpose(matrix_space(Z11,1,2)([ 1 2; ])) ==
          matrix_space(Z11,2,1)(reshape([ 1 ; 2],2,1))

  @test_throws ErrorConstrDimMismatch transpose!(R([ 1 2 ;]))

  R = matrix_space(Z11, 4, 4)
  m = [1 2 3 4; 2 4 6 2; 6 4 2 4; 2 6 4 0]
  @test R(m, true) == R(transpose(m))
  m1 = Matrix{BigInt}(m)
  @test R(m1, true) == R(transpose(m1))
  m2 = Matrix{ZZRingElem}(m)
  @test R(m2, true) == R(transpose(m2))
  m3 = map(Z11, m)
  @test R(m3, true) == R(transpose(m3))
end

@testset "FpMatrix.unary_ops" begin
  Z17 = Native.GF(ZZ(17))

  R = matrix_space(Z17, 3, 4)
  RR = matrix_space(Z17, 4, 3)
  Z2 = Native.GF(ZZ(2))
  S = matrix_space(Z2, 3, 4)

  a = R([ 1 2 3 1; 3 2 1 2; 1 3 2 0])

  b = R([ 2 1 0 1; 0 0 0 0; 0 1 2 0 ])

  c = R()

  d = -b

  @test d == R([ 15 16 0 16; 0 0 0 0; 0 16 15 0])
end

@testset "FpMatrix.binary_ops" begin
  Z17 = Native.GF(ZZ(17))

  R = matrix_space(Z17, 3, 4)
  a = R([ 1 2 3 1; 3 2 1 2; 1 3 2 0])
  b = R([ 2 1 0 1; 0 0 0 0; 0 1 2 0 ])
  c = R()
  d = a + b
  @test d == R([3 3 3 2; 3 2 1 2; 1 4 4 0])

  d = a - b
  @test d == R([ 16 1 3 0; 3 2 1 2; 1 2 0 0 ])

  d = a*transpose(a)
  @test d == matrix_space(Z17, 3, 3)([15 12 13; 12 1 11; 13 11 14])

  d = transpose(a)*a
  @test d == matrix_space(Z17, 4, 4)([11 11 8 7; 11 0 14 6; 8 14 14 5; 7 6 5 5])

  a = matrix(Z17, [1 2; 3 4])
  @test mul!([ZZRingElem(), ZZRingElem()], a, ZZRingElem[1, 2]) == [Z17(5), Z17(11)]
  @test mul!([ZZRingElem(), ZZRingElem()], ZZRingElem[1, 2], a) == [Z17(7), Z17(10)]
end

@testset "FpMatrix.row_col_swapping" begin
   R = Native.GF(ZZ(17))

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

@testset "FpMatrix.adhoc_binary" begin
  Z17 = Native.GF(ZZ(17))

  R = matrix_space(Z17, 3, 4)
  Z2 = Native.GF(ZZ(2))

  a = R([ 1 2 3 1; 3 2 1 2; 1 3 2 0])

  b = R([ 2 1 0 1; 0 0 0 0; 0 1 2 0 ])

  c = R()
  d = UInt(2)*a
  dd = a*UInt(2)

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

  d = Z17(2)*a
  dd = a*Z17(2)

  @test d == R([ 2 4 6 2; 6 4 2 4; 2 6 4 0])
  @test dd == R([ 2 4 6 2; 6 4 2 4; 2 6 4 0])

  @test_throws ErrorException Z2(1)*a
end

@testset "FpMatrix.comparison" begin
  Z17 = Native.GF(ZZ(17))

  R = matrix_space(Z17, 3, 4)

  a = R([ 1 2 3 1; 3 2 1 2; 1 3 2 0])

  @test a == a

  @test deepcopy(a) == a

  @test a != R([0 1 3 1; 2 1 4 2; 1 1 1 1])
end

@testset "FpMatrix.adhoc_comparison" begin
  Z17 = Native.GF(ZZ(17))

  R = matrix_space(Z17, 3, 4)

  @test R(5) == 5
  @test R(5) == ZZRingElem(5)
  @test R(5) == Z17(5)

  @test 5 == R(5)
  @test ZZRingElem(5) == R(5)
  @test Z17(5) == R(5)
end

@testset "FpMatrix.powering" begin
  Z17 = Native.GF(ZZ(17))

  R = matrix_space(Z17, 3, 4)

  a = R([ 1 2 3 1; 3 2 1 2; 1 3 2 0])

  f = a*transpose(a)

  g = f^1000

  @test g == matrix_space(Z17, 3, 3)([1 2 2; 2 13 12; 2 12 15])

  g = f^ZZ(1000)

  @test g == matrix_space(Z17, 3, 3)([1 2 2; 2 13 12; 2 12 15])

  @test_throws ErrorException f^(ZZ(2)^1000)
end

@testset "FpMatrix.row_echelon_form" begin
  Z17 = Native.GF(ZZ(17))
  R = matrix_space(Z17, 3, 4)
  RR = matrix_space(Z17, 4, 3)
  Z2 = Native.GF(ZZ(2))
  S = matrix_space(Z2, 3, 4)

  a = R([ 1 2 3 1; 3 2 1 2; 1 3 2 0])

  b = R([ 2 1 0 1; 0 0 0 0; 0 1 2 0 ])

  b = transpose(b)

  c = a*transpose(a)

  r, d, den = rref_rational(a)

  @test d == R([ 12 0 0 11; 0 12 0 10; 0 0 12 5])
  @test r == 3
  @test den == Z17(12)

  r, den = rref_rational!(a)

  @test a == R([ 12 0 0 11; 0 12 0 10; 0 0 12 5])
  @test r == 3
  @test den == Z17(12)

  r, d, den = rref_rational(b)

  @test d == parent(b)([ 2 0 0 ; 0 0 2; 0 0 0; 0 0 0])
  @test r == 2
  @test den == Z17(2)
end

@testset "FpMatrix.trace_det" begin
  Z17 = Native.GF(ZZ(17))
  R = matrix_space(Z17, 3, 4)
  RR = matrix_space(Z17, 4, 3)
  Z2 = Native.GF(ZZ(2))
  S = matrix_space(Z2, 3, 4)

  a = R([ 1 2 3 1; 3 2 1 2; 1 3 2 0])

  aa = matrix_space(Z17,3,3)([ 1 2 3; 3 2 1; 1 1 2])

  b = R([ 2 1 0 1; 0 0 0 0; 0 1 2 0 ])

  a = transpose(a)*a

  c = tr(a)

  @test c == Z17(13)

  @test_throws ErrorException tr(b)

  c = det(a)

  @test c == zero(Z17)

  @test_throws ErrorException det(b)

  c = det(aa)

  @test c == Z17(13)
end

@testset "FpMatrix.rank" begin
  Z17 = Native.GF(ZZ(17))
  R = matrix_space(Z17, 3, 4)
  RR = matrix_space(Z17, 4, 3)
  Z2 = Native.GF(ZZ(2))
  S = matrix_space(Z2, 3, 4)

  a = R([ 1 2 3 1; 3 2 1 2; 1 3 2 0])

  aa = matrix_space(Z17,3,3)([ 1 2 3; 3 2 1; 1 1 2])

  b = R([ 2 1 0 1; 0 0 0 0; 0 1 2 0 ])

  c = rank(a)

  @test c == 3

  c = rank(aa)

  @test c == 3

  c = rank(b)

  @test c == 2
end

@testset "FpMatrix.inv" begin
  Z17 = Native.GF(ZZ(17))
  R = matrix_space(Z17, 3, 4)
  RR = matrix_space(Z17, 4, 3)
  Z2 = Native.GF(ZZ(2))
  S = matrix_space(Z2, 3, 4)

  a = R([ 1 2 3 1; 3 2 1 2; 1 3 2 0])

  aa = matrix_space(Z17,3,3)([ 1 2 3; 3 2 1; 1 1 2])

  b = R([ 2 1 0 1; 0 0 0 0; 0 1 2 0 ])

  c = inv(aa)

  @test c == parent(aa)([12 13 1; 14 13 15; 4 4 1])

  @test_throws ErrorException inv(a)

  @test_throws ErrorException inv(transpose(a)*a)
end


@testset "FpMatrix.swap_rows" begin
  Z17 = Native.GF(ZZ(17))

  A = matrix(Z17, 5, 1, [1, 2, 3, 4, 5])

  B = swap_rows(A, 3, 4)
  @test B == matrix(Z17, 5, 1, [1, 2, 4, 3, 5])

  swap_rows!(A, 3, 4)
  @test A == matrix(Z17, 5, 1, [1, 2, 4, 3, 5])

  @test_throws BoundsError swap_rows(A, 0, 5)
  @test_throws BoundsError swap_rows(A, 4, 6)
end

@testset "FpMatrix.view" begin
  Z17 = Native.GF(ZZ(17))
  R = matrix_space(Z17, 3, 3)
  S = matrix_space(Z17, 3, 4)

  a = R([ 1 2 3 ; 3 2 1 ; 0 0 2 ])

  b = S([ 2 1 0 1; 0 0 0 0; 0 1 2 0 ])

  t = view(a, 1, 1, 3, 3)

  @test t == a

  @test view(a, 1, 1, 3, 3) == view(a, 1:3, 1:3)
  @test view(a, 1, 1, 3, 3) == sub(a, 1, 1, 3, 3)
  @test view(a, 1, 1, 3, 3) == sub(a, 1:3, 1:3)

  @test isempty(view(a, 2, 2, 1, 1))

  t = view(a, 1, 1, 2, 2)

  @test t == Z17[1 2; 3 2]

  t = view(a, 2, 2, 3, 2)

  @test t == transpose(Z17[2 0])

  @test view(a, 2, 2, 3, 2) == view(a, 2:3,  2:2)
  @test view(a, 2, 2, 3, 2) == sub(a, 2, 2, 3, 2)
  @test view(a, 2, 2, 3, 2) == sub(a, 2:3, 2:2)

  @test_throws BoundsError view(a, 3, 3, 5, 5)

  S = matrix_space(Z17, 3, 3)

  A = S([1 2 3; 4 5 6; 7 8 9])

  B = @inferred view(A, 1, 1, 2, 2)

  @test typeof(B) == FpMatrix
  @test B == matrix_space(Z17, 2, 2)([1 2; 4 5])

  B[1, 1] = 10
  @test A[1, 1] == 10

  C = @inferred view(B, 1:2, 1:2)

  @test typeof(C) == FpMatrix
  @test C == matrix_space(Z17, 2, 2)([10 2; 4 5])

  C[1, 1] = 23
  @test B[1, 1] == 23
  @test A[1, 1] == 23

  A = 0
  GC.gc()
  @test B[1, 1] == 23

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

@testset "FpMatrix.sub" begin
   Z17 = Native.GF(ZZ(17))
   S = matrix_space(Z17, 3, 3)

   A = S([1 2 3; 4 5 6; 7 8 9])

   B = @inferred sub(A, 1, 1, 2, 2)

   @test typeof(B) == FpMatrix
   @test B == matrix_space(Z17, 2, 2)([1 2; 4 5])

   B[1, 1] = 10
   @test A == S([1 2 3; 4 5 6; 7 8 9])

   C = @inferred sub(B, 1:2, 1:2)

   @test typeof(C) == FpMatrix
   @test C == matrix_space(Z17, 2, 2)([10 2; 4 5])

   C[1, 1] = 20
   @test B == matrix_space(Z17, 2, 2)([10 2; 4 5])
   @test A == S([1 2 3; 4 5 6; 7 8 9])
end

@testset "FpMatrix.concatenation" begin
  Z17 = Native.GF(ZZ(17))
  R = matrix_space(Z17, 3, 3)
  S = matrix_space(Z17, 3, 4)

  a = R([ 1 2 3 ; 3 2 1 ; 0 0 2 ])

  b = S([ 2 1 0 1; 0 0 0 0; 0 1 2 0 ])

  c = hcat(a,a)

  @test c == matrix_space(Z17, 3, 6)([1, 2, 3, 1, 2, 3,
                                     3, 2, 1, 3, 2, 1,
                                     0, 0, 2, 0, 0, 2])

  c = hcat(a,b)

  @test c == matrix_space(Z17, 3, 7)([1, 2, 3, 2, 1, 0, 1,
                                     3, 2, 1, 0, 0, 0, 0,
                                     0, 0, 2, 0, 1, 2, 0])

  @test_throws ErrorException c = hcat(a,transpose(b))

  c = vcat(a,transpose(b))

  @test c == matrix_space(Z17, 7, 3)([1, 2, 3,
                                     3, 2, 1,
                                     0, 0, 2,
                                     2, 0, 0,
                                     1, 0, 1,
                                     0, 0, 2,
                                     1, 0, 0])

  @test_throws ErrorException vcat(a,b)
end

@testset "FpMatrix.conversion" begin
  Z17 = Native.GF(ZZ(17))
  R = matrix_space(Z17, 3, 3)
  S = matrix_space(ZZ, 3, 3)

  c = S()

  a = R([ 1 2 3 ; 3 2 1 ; 0 0 2 ])

  @test Array(a) == [Z17(1) Z17(2) Z17(3);
                     Z17(3) Z17(2) Z17(1);
                     Z17(0) Z17(0) Z17(2) ]

#= Not implemented in Flint yet
  b = lift(a)

  @test b == S([ 1 2 3; 3 2 1; 0 0 2])
  @test parent(b) == S

  lift!(c,a)

  @test c == S([ 1 2 3; 3 2 1; 0 0 2])
=#

end

@testset "FpMatrix.charpoly" begin
   R = Native.GF(ZZ(17))

   for dim = 0:5
      S = matrix_space(R, dim, dim)
      U, x = polynomial_ring(R, "x")

      for i = 1:10
         M = rand(S, -5:5)

         p1 = charpoly(U, M)
         p2 = charpoly_danilevsky!(U, M)

         @test p1 == p2
      end
   end
end

@testset "FpMatrix.rand" begin
   R = Native.GF(ZZ(17))
   S = matrix_space(R, 3, 3)

   M = rand(S, 1:5)
   @test parent(M) == S

   for i=1:3, j=1:3
      @test M[i, j] in 1:5
   end

   M = rand(S)
   @test parent(M) == S
end
