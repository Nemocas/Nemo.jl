import Nemo: ZZPolyRingMatrix

@testset "ZZPolyRingMatrix.constructors" begin
  R, x = ZZ["x"]

  # Constructor from Matrix{ZZPolyRingElem}
  m = [x+1 x; x^2 2*x+3]
  A = ZZPolyRingMatrix(m)
  @test isa(A, ZZPolyRingMatrix)
  @test A isa MatElem{ZZPolyRingElem}
  @test base_ring(A) == R
  @test nrows(A) == 2
  @test ncols(A) == 2
  @test A[1, 1] == x + 1
  @test A[1, 2] == x
  @test A[2, 1] == x^2
  @test A[2, 2] == 2*x + 3

  # Constructor from MatElem{ZZPolyRingElem}
  M = matrix(R, [x+1 x; x^2 2*x+3])
  B = ZZPolyRingMatrix(M)
  @test isa(B, ZZPolyRingMatrix)
  @test base_ring(B) == R
  @test nrows(B) == 2
  @test ncols(B) == 2
  @test B == A

  # Non-square matrix
  C = ZZPolyRingMatrix(matrix(R, [x 1 x+2; x^2+1 x-1 3]))
  @test nrows(C) == 2
  @test ncols(C) == 3

  # 1x1 matrix
  D = ZZPolyRingMatrix(matrix(R, reshape([x^3 + x], 1, 1)))
  @test nrows(D) == 1
  @test ncols(D) == 1
  @test D[1, 1] == x^3 + x

  # deepcopy preserves values and is independent
  E = deepcopy(A)
  @test E == A
  E[1, 1] = R(0)
  @test A[1, 1] == x + 1

  # Constant polynomials
  F = ZZPolyRingMatrix(matrix(R, [R(1) R(0); R(0) R(1)]))
  @test isone(F)
end

@testset "ZZPolyRingMatrix.manipulation" begin
  R, x = ZZ["x"]

  A = ZZPolyRingMatrix(matrix(R, [x+1 x; x^2 2*x+3]))

  @test nrows(A) == 2
  @test ncols(A) == 2
  @test base_ring(A) == R

  # getindex / setindex!
  @test A[1, 1] == x + 1
  A[2, 2] = x^3
  @test A[2, 2] == x^3

  # iszero / isone
  Z = ZZPolyRingMatrix(zero_matrix(R, 2, 2))
  @test iszero(Z)
  @test !iszero(A)

  I = ZZPolyRingMatrix(identity_matrix(R, 3))
  @test isone(I)
  @test !isone(A)
end

@testset "ZZPolyRingMatrix.similar" begin
  R, x = ZZ["x"]
  A = ZZPolyRingMatrix(matrix(R, [x+1 x; x^2 2*x+3]))

  S = similar(A)
  @test S isa ZZPolyRingMatrix
  @test nrows(S) == 2
  @test ncols(S) == 2
  @test base_ring(S) == R

  S2 = similar(A, 3, 4)
  @test S2 isa ZZPolyRingMatrix
  @test nrows(S2) == 3
  @test ncols(S2) == 4
end

@testset "ZZPolyRingMatrix.printing" begin
  R, x = ZZ["x"]
  A = ZZPolyRingMatrix(matrix(R, [x+1 x; x^2 2*x+3]))

  # test that default Julia printing is not used
  @test !occursin(string(typeof(A)), string(A))
end

@testset "ZZPolyRingMatrix.unary_ops" begin
  R, x = ZZ["x"]
  A = ZZPolyRingMatrix(matrix(R, [x+1 x; x^2 2*x+3]))
  B = ZZPolyRingMatrix(matrix(R, [-(x+1) -x; -x^2 -(2*x+3)]))

  @test -A == B
  @test -(-A) == A
end

@testset "ZZPolyRingMatrix.binary_ops" begin
  R, x = ZZ["x"]
  A = ZZPolyRingMatrix(matrix(R, [x+1 x; x^2 2*x+3]))
  B = ZZPolyRingMatrix(matrix(R, [x 1; x+1 x^2]))

  # addition
  @test A + B == ZZPolyRingMatrix(matrix(R, [2*x+1 x+1; x^2+x+1 x^2+2*x+3]))

  # subtraction
  @test A - B == ZZPolyRingMatrix(matrix(R, [1 x-1; x^2-x-1 -x^2+2*x+3]))

  # multiplication
  prod_AB = A * B
  @test prod_AB[1, 1] == (x+1)*x + x*(x+1)
  @test prod_AB[1, 2] == (x+1)*1 + x*x^2
  @test prod_AB[2, 1] == x^2*x + (2*x+3)*(x+1)
  @test prod_AB[2, 2] == x^2*1 + (2*x+3)*x^2

  # non-square multiplication
  C = ZZPolyRingMatrix(matrix(R, [x 1 x+2; x^2+1 x-1 3]))
  D = ZZPolyRingMatrix(matrix(R, [x 1; x^2 2; x+1 x+3]))
  E = C * D
  @test nrows(E) == 2
  @test ncols(E) == 2

  # identity element
  I2 = ZZPolyRingMatrix(identity_matrix(R, 2))
  @test A * I2 == A
  @test I2 * A == A
end

@testset "ZZPolyRingMatrix.comparison" begin
  R, x = ZZ["x"]
  A = ZZPolyRingMatrix(matrix(R, [x+1 x; x^2 2*x+3]))
  B = ZZPolyRingMatrix(matrix(R, [x+1 x; x^2 2*x+3]))
  C = ZZPolyRingMatrix(matrix(R, [x 1; x+1 x^2]))

  @test A == A
  @test A == B
  @test !(A == C)
end

@testset "ZZPolyRingMatrix.transpose" begin
  R, x = ZZ["x"]
  A = ZZPolyRingMatrix(matrix(R, [x+1 x; x^2 2*x+3]))
  At = transpose(A)

  @test nrows(At) == 2
  @test ncols(At) == 2
  @test At[1, 1] == A[1, 1]
  @test At[1, 2] == A[2, 1]
  @test At[2, 1] == A[1, 2]
  @test At[2, 2] == A[2, 2]

  # non-square transpose
  B = ZZPolyRingMatrix(matrix(R, [x 1 x+2; x^2+1 x-1 3]))
  Bt = transpose(B)
  @test nrows(Bt) == 3
  @test ncols(Bt) == 2
  for i in 1:2, j in 1:3
    @test Bt[j, i] == B[i, j]
  end
end

@testset "ZZPolyRingMatrix.det" begin
  R, x = ZZ["x"]

  # 1x1
  A = ZZPolyRingMatrix(matrix(R, reshape([x^2 + x + 1], 1, 1)))
  @test det(A) == x^2 + x + 1

  # 2x2
  B = ZZPolyRingMatrix(matrix(R, [x+1 x; x^2 2*x+3]))
  d = det(B)
  @test d == (x+1)*(2*x+3) - x*x^2

  # identity matrix has det 1
  I3 = ZZPolyRingMatrix(identity_matrix(R, 3))
  @test det(I3) == R(1)

  # zero matrix has det 0
  Z3 = ZZPolyRingMatrix(zero_matrix(R, 3, 3))
  @test det(Z3) == R(0)

  # non-square throws
  C = ZZPolyRingMatrix(matrix(R, [x 1; x^2+1 x-1; R(1) x]))
  @test_throws ArgumentError det(C)
end

@testset "ZZPolyRingMatrix.tr" begin
  R, x = ZZ["x"]

  A = ZZPolyRingMatrix(matrix(R, [x+1 x; x^2 2*x+3]))
  @test tr(A) == (x+1) + (2*x+3)

  I3 = ZZPolyRingMatrix(identity_matrix(R, 3))
  @test tr(I3) == R(3)

  # non-square throws
  C = ZZPolyRingMatrix(matrix(R, [x 1 x+2; x^2+1 x-1 3]))
  @test_throws ArgumentError tr(C)
end

@testset "ZZPolyRingMatrix.rank" begin
  R, x = ZZ["x"]

  # full rank square
  A = ZZPolyRingMatrix(matrix(R, [x+1 x; x^2 2*x+3]))
  @test rank(A) == 2

  # rank-deficient (rows are proportional)
  B = ZZPolyRingMatrix(matrix(R, [x 2*x; 1 2]))
  @test rank(B) == 1

  # zero matrix
  Z = ZZPolyRingMatrix(zero_matrix(R, 2, 2))
  @test rank(Z) == 0

  # identity
  I3 = ZZPolyRingMatrix(identity_matrix(R, 3))
  @test rank(I3) == 3
end
