function randelem(R::Nemo.NmodRing, n)
   return rand(R, -n:n)
end

function randmat(R::NmodMatSpace, d::Int)
   m = nrows(R)
   n = ncols(R)
   r = R()
   for i = 1:m
      for j = 1:n
         r[i, j] = randelem(base_ring(R), d)
      end
   end
   return r
end

@testset "nmod_mat.constructors..." begin
  Z2 = ResidueRing(ZZ, 2)
  Z3 = ResidueRing(ZZ, 3)

  R = NmodMatSpace(Z2, 2, 2)

  @test elem_type(R) == nmod_mat
  @test elem_type(NmodMatSpace) == nmod_mat
  @test parent_type(nmod_mat) == NmodMatSpace
  @test nrows(R) == 2
  @test ncols(R) == 2

  @test isa(R, NmodMatSpace)

  @test base_ring(R) == Z2

  S = NmodMatSpace(Z3, 2, 2)

  @test isa(S, NmodMatSpace)

  RR = NmodMatSpace(Z2, 2, 2)

  @test isa(RR, NmodMatSpace)

  @test R == RR

  @test_throws ErrorException NmodMatSpace(Z2, 2, -1)
  @test_throws ErrorException NmodMatSpace(Z2, -1, 2)
  @test_throws ErrorException NmodMatSpace(Z2, -1, -1)

  a = R()

  @test isa(a, nmod_mat)
  @test parent(a) == R

  ar = [ BigInt(1) BigInt(1); BigInt(1) BigInt(1) ]

  b = R(ar)

  @test isa(b, nmod_mat)
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
  @test isa(c, nmod_mat)
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

  @test isa(d, nmod_mat)
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

  @test isa(d, nmod_mat)

  ar = MatrixSpace(ZZ, 2, 2)([ 1 1; 1 1])

  e = R(ar)

  @test isa(e, nmod_mat)
  @test parent(e) == R
  @test nrows(e) == 2 && ncols(e) == 2

  ar = MatrixSpace(ZZ, 3, 3)([ 1 1 1 ; 1 1 1; 1 1 1])

  @test_throws ErrorException R(ar)

  ar = [ Z2(1) Z2(1); Z2(1) Z2(1) ]

  f = R(ar)

  @test isa(f, nmod_mat)
  @test parent(f) == R
  @test nrows(f) == 2 && ncols(f) == 2
  @test_throws ErrorConstrDimMismatch R(reshape(ar,4,1))
  @test f == R([Z2(1), Z2(1), Z2(1), Z2(1)])
  @test_throws ErrorConstrDimMismatch R([Z2(1) Z2(1) ])
  @test_throws ErrorConstrDimMismatch R([Z2(1) Z2(1) ; Z2(1) Z2(1) ; Z2(1) Z2(1) ])
  @test_throws ErrorConstrDimMismatch R([Z2(1), Z2(1), Z2(1)])
  @test_throws ErrorConstrDimMismatch R([Z2(1), Z2(1), Z2(1), Z2(1), Z2(1)])

  @test isa(S(1), nmod_mat)

  @test isa(S(fmpz(1)), nmod_mat)

  @test isa(S(Z3(1)), nmod_mat)

  g = deepcopy(e)

  @test b == c
  @test c == d
  @test d == e
  @test e == f
  @test g == e

   arr = [1 2; 3 4]
   arr2 = [1, 2, 3, 4, 5, 6]

   for T in [Z3, fmpz, Int, BigInt]
      M = matrix(Z3, map(T, arr))
      @test isa(M, nmod_mat)
      @test M.base_ring == Z3

      M2 = matrix(Z3, 2, 3, map(T, arr2))
      @test isa(M2, nmod_mat)
      @test M2.base_ring == Z3
      @test nrows(M2) == 2
      @test ncols(M2) == 3
      @test_throws ErrorConstrDimMismatch matrix(Z3, 2, 2, map(T, arr2))
      @test_throws ErrorConstrDimMismatch matrix(Z3, 2, 4, map(T, arr2))
   end

   M3 = zero_matrix(Z3, 2, 3)

   @test isa(M3, nmod_mat)
   @test M3.base_ring == Z3

   M4 = identity_matrix(Z3, 3)

   @test isa(M4, nmod_mat)
   @test M4.base_ring == Z3

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

@testset "nmod_mat.similar..." begin
   Z13 = ResidueRing(ZZ, 13)
   S = NmodMatSpace(Z13, 2, 2)
   s = S(fmpz(3))

   t = similar(s)
   @test t isa nmod_mat
   @test size(t) == size(s)
   t = similar(s, Z13)
   @test t isa nmod_mat
   @test size(t) == size(s)

   t = similar(s, 2, 3)
   @test t isa nmod_mat
   @test size(t) == (2, 3)
   t = similar(s, Z13, 2, 3)
   @test t isa nmod_mat
   @test size(t) == (2, 3)
end

@testset "nmod_mat.printing..." begin
  Z2 = ResidueRing(ZZ, 2)
  R = NmodMatSpace(Z2, 2, 2)

  a = R(1)

  # test that default Julia printing is not used
  @test !occursin(string(typeof(a)), string(a))
end

@testset "nmod_mat.manipulation..." begin
  Z10 = ResidueRing(ZZ, 10)
  R = NmodMatSpace(Z10, 2, 2)
  Z20 = ResidueRing(ZZ, 20)
  S = NmodMatSpace(Z20, 2, 2)

  ar = [ 1 2; 3 4]

  a = R(ar)
  aa = S(ar)

  @test nrows(a) == 2
  @test ncols(a) == 2

  b = deepcopy(a)

  c = R([ 1 3; 2 4])

  @test a[1,1] == Z10(1)

  a[1,1] = UInt(2)

  @test a[1,1] == Z10(2)
  @test_throws BoundsError a[0,-1] = Z10(2)

  a[2,1] = ZZ(3)

  @test a[2,1] == Z10(ZZ(3))
  @test_throws BoundsError a[-10,-10] = ZZ(3)

  a[2,2] = Z10(4)

  @test a[2,2] == Z10(4)
  @test_throws BoundsError a[-2,2] = Z10(4)

  a[1,2] = 5

  @test a[1,2] == Z10(5)
  @test_throws BoundsError a[-2,2] = 5

  @test a != b

  d = one(R)

  @test isa(d, nmod_mat)

  e = zero(R)

  @test isa(e, nmod_mat)

  @test iszero(e)

  @test_throws ErrorException one(MatrixSpace(ResidueRing(ZZ, 2), 1, 2))

  @test issquare(a)

  @test a == a
  @test a == deepcopy(a)
  @test a != aa

  @test transpose(b) == c

  transpose!(b)

  @test b == c

  @test transpose(MatrixSpace(Z10,1,2)([ 1 2; ])) ==
          MatrixSpace(Z10,2,1)(reshape([ 1 ; 2],2,1))

  @test_throws ErrorConstrDimMismatch transpose!(R([ 1 2 ;]))
end

@testset "nmod_mat.unary_ops..." begin
  Z17 = ResidueRing(ZZ,17)

  R = MatrixSpace(Z17, 3, 4)
  RR = MatrixSpace(Z17, 4, 3)
  Z2 = ResidueRing(ZZ,2)
  S = MatrixSpace(Z2, 3, 4)

  a = R([ 1 2 3 1; 3 2 1 2; 1 3 2 0])

  b = R([ 2 1 0 1; 0 0 0 0; 0 1 2 0 ])

  c = R()

  d = -b

  @test d == R([ 15 16 0 16; 0 0 0 0; 0 16 15 0])
end

@testset "nmod_mat.binary_ops..." begin
  Z17 = ResidueRing(ZZ,17)

  R = MatrixSpace(Z17, 3, 4)

  a = R([ 1 2 3 1; 3 2 1 2; 1 3 2 0])

  b = R([ 2 1 0 1; 0 0 0 0; 0 1 2 0 ])

  c = R()

  d = a + b

  @test d == R([3 3 3 2; 3 2 1 2; 1 4 4 0])

  d = a - b

  @test d == R([ 16 1 3 0; 3 2 1 2; 1 2 0 0 ])

  d = a*transpose(a)

  @test d == MatrixSpace(Z17, 3, 3)([15 12 13; 12 1 11; 13 11 14])

  d = transpose(a)*a

  @test d == MatrixSpace(Z17, 4, 4)([11 11 8 7; 11 0 14 6; 8 14 14 5; 7 6 5 5])
end

@testset "nmod_mat.row_col_swapping..." begin
   R = ResidueRing(FlintZZ, 17)

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

@testset "nmod_mat.adhoc_binary..." begin
  Z17 = ResidueRing(ZZ,17)

  R = MatrixSpace(Z17, 3, 4)
  Z2 = ResidueRing(ZZ,2)

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

@testset "nmod_mat.comparison..." begin
  Z17 = ResidueRing(ZZ,17)

  R = MatrixSpace(Z17, 3, 4)

  a = R([ 1 2 3 1; 3 2 1 2; 1 3 2 0])

  @test a == a

  @test deepcopy(a) == a

  @test a != R([0 1 3 1; 2 1 4 2; 1 1 1 1])
end

@testset "nmod_mat.adhoc_comparison..." begin
  Z17 = ResidueRing(ZZ,17)

  R = MatrixSpace(Z17, 3, 4)

  @test R(5) == 5
  @test R(5) == fmpz(5)
  @test R(5) == Z17(5)

  @test 5 == R(5)
  @test fmpz(5) == R(5)
  @test Z17(5) == R(5)
end

@testset "nmod_mat.powering..." begin
  Z17 = ResidueRing(ZZ,17)

  R = MatrixSpace(Z17, 3, 4)

  a = R([ 1 2 3 1; 3 2 1 2; 1 3 2 0])

  f = a*transpose(a)

  g = f^1000

  @test g == MatrixSpace(Z17, 3, 3)([1 2 2; 2 13 12; 2 12 15])

  g = f^ZZ(1000)

  @test g == MatrixSpace(Z17, 3, 3)([1 2 2; 2 13 12; 2 12 15])

  @test_throws ErrorException f^(ZZ(2)^1000)
end

@testset "nmod_mat.row_echelon_form..." begin
  Z17 = ResidueRing(ZZ,17)
  R = MatrixSpace(Z17, 3, 4)
  RR = MatrixSpace(Z17, 4, 3)
  Z2 = ResidueRing(ZZ,2)
  S = MatrixSpace(Z2, 3, 4)

  a = R([ 1 2 3 1; 3 2 1 2; 1 3 2 0])

  b = R([ 2 1 0 1; 0 0 0 0; 0 1 2 0 ])

  b = transpose(b)

  c = a*transpose(a)

  r, den, d = rref(a)

  @test d == R([ 12 0 0 11; 0 12 0 10; 0 0 12 5])
  @test r == 3
  @test den == Z17(12)

  r, den = rref!(a)

  @test a == R([ 12 0 0 11; 0 12 0 10; 0 0 12 5])
  @test r == 3
  @test den == Z17(12)

  r, den, d = rref(b)

  @test d == parent(b)([ 2 0 0 ; 0 0 2; 0 0 0; 0 0 0])
  @test r == 2
  @test den == Z17(2)
end

@testset "nmod_mat.howell_form..." begin
  Z17 = ResidueRing(ZZ, 12)
  R = MatrixSpace(Z17, 3, 3)

  a = R([4 1 0; 0 0 5; 0 0 0 ])

  b = R([8 5 5; 0 9 8; 0 0 10])

  c = R([4 1 0; 0 3 0; 0 0 1])

  d = R([4 0 0; 0 0 1; 0 0 0])

  @test howell_form(a) == c
  @test howell_form(b) == c
  @test strong_echelon_form(d) == R([4 0 0; 0 0 0; 0 0 1])
end

@testset "nmod_mat.trace_det..." begin
  Z17 = ResidueRing(ZZ,17)
  R = MatrixSpace(Z17, 3, 4)
  RR = MatrixSpace(Z17, 4, 3)
  Z2 = ResidueRing(ZZ,2)
  S = MatrixSpace(Z2, 3, 4)

  a = R([ 1 2 3 1; 3 2 1 2; 1 3 2 0])

  aa = MatrixSpace(Z17,3,3)([ 1 2 3; 3 2 1; 1 1 2])

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

  a = R([ 1 2 3 1; 3 2 1 2; 1 3 2 0])
end

@testset "nmod_mat.rank..." begin
  Z17 = ResidueRing(ZZ,17)
  R = MatrixSpace(Z17, 3, 4)
  RR = MatrixSpace(Z17, 4, 3)
  Z2 = ResidueRing(ZZ,2)
  S = MatrixSpace(Z2, 3, 4)

  a = R([ 1 2 3 1; 3 2 1 2; 1 3 2 0])

  aa = MatrixSpace(Z17,3,3)([ 1 2 3; 3 2 1; 1 1 2])

  b = R([ 2 1 0 1; 0 0 0 0; 0 1 2 0 ])

  c = rank(a)

  @test c == 3

  c = rank(aa)

  @test c == 3

  c = rank(b)

  @test c == 2
end

@testset "nmod_mat.inv..." begin
  Z17 = ResidueRing(ZZ,17)
  R = MatrixSpace(Z17, 3, 4)
  RR = MatrixSpace(Z17, 4, 3)
  Z2 = ResidueRing(ZZ,2)
  S = MatrixSpace(Z2, 3, 4)

  a = R([ 1 2 3 1; 3 2 1 2; 1 3 2 0])

  aa = MatrixSpace(Z17,3,3)([ 1 2 3; 3 2 1; 1 1 2])

  b = R([ 2 1 0 1; 0 0 0 0; 0 1 2 0 ])

  c = inv(aa)

  @test c == parent(aa)([12 13 1; 14 13 15; 4 4 1])

  @test_throws ErrorException inv(a)

  @test_throws ErrorException inv(transpose(a)*a)
end

@testset "nmod_mat.solve..." begin
  Z17 = ResidueRing(ZZ,17)
  R = MatrixSpace(Z17, 3, 3)
  S = MatrixSpace(Z17, 3, 4)

  a = R([ 1 2 3 ; 3 2 1 ; 0 0 2 ])

  b = S([ 2 1 0 1; 0 0 0 0; 0 1 2 0 ])

  c = a*b

  d = solve(a,c)

  @test d == b

  a = zero(R)

  @test_throws ErrorException  solve(a,c)
end

@testset "nmod_mat.lu..." begin

  Z17 = ResidueRing(ZZ,17)
  R = MatrixSpace(Z17, 3, 3)
  S = MatrixSpace(Z17, 3, 4)

  a = R([ 1 2 3 ; 3 2 1 ; 0 0 2 ])

  b = S([ 2 1 0 1; 0 0 0 0; 0 1 2 0 ])

  r, P, l, u = lu(a)

  @test l*u == P*a

  r, P, l, u = lu(b)

  @test l*u == S([ 2 1 0 1; 0 1 2 0; 0 0 0 0])

  @test l*u == P*b

  c = matrix(Z17, 6, 3, [0, 0, 1, 0, 0, 0, 0, 0, 0, 1, 0, 0, 0, 1, 0, 0, 0, 1])

  r, P, l, u = lu(c)

  @test r == 3
  @test l*u == P*c
end

@testset "nmod_mat.swap_rows..." begin
  Z17 = ResidueRing(ZZ, 17)

  A = matrix(Z17, 5, 1, [1, 2, 3, 4, 5])

  B = swap_rows(A, 3, 4)
  @test B == matrix(Z17, 5, 1, [1, 2, 4, 3, 5])

  swap_rows!(A, 3, 4)
  @test A == matrix(Z17, 5, 1, [1, 2, 4, 3, 5])

  @test_throws BoundsError swap_rows(A, 0, 5)
  @test_throws BoundsError swap_rows(A, 4, 6)
end

@testset "nmod_mat.view..." begin
  Z17 = ResidueRing(ZZ, 17)
  R = MatrixSpace(Z17, 3, 3)
  S = MatrixSpace(Z17, 3, 4)

  a = R([ 1 2 3 ; 3 2 1 ; 0 0 2 ])

  b = S([ 2 1 0 1; 0 0 0 0; 0 1 2 0 ])

  t = view(a, 1, 1, 3, 3)

  @test t == a

  @test view(a, 1, 1, 3, 3) == view(a, 1:3, 1:3)
  @test view(a, 1, 1, 3, 3) == sub(a, 1, 1, 3, 3)
  @test view(a, 1, 1, 3, 3) == sub(a, 1:3, 1:3)

  t = view(a, 1, 1, 2, 2)

  @test t == Z17[1 2; 3 2]

  t = view(a, 2, 2, 3, 2)

  @test t == transpose(Z17[2 0])

  @test view(a, 2, 2, 3, 2) == view(a, 2:3,  2:2)
  @test view(a, 2, 2, 3, 2) == sub(a, 2, 2, 3, 2)
  @test view(a, 2, 2, 3, 2) == sub(a, 2:3, 2:2)

  @test_throws BoundsError view(a, 3, 3, 5, 5)

  S = MatrixSpace(Z17, 3, 3)

  A = S([1 2 3; 4 5 6; 7 8 9])

  B = @inferred view(A, 1, 1, 2, 2)

  @test typeof(B) == nmod_mat
  @test B == MatrixSpace(Z17, 2, 2)([1 2; 4 5])

  B[1, 1] = 10
  @test A[1, 1] == 10

  C = @inferred view(B, 1:2, 1:2)

  @test typeof(C) == nmod_mat
  @test C == MatrixSpace(Z17, 2, 2)([10 2; 4 5])

  C[1, 1] = 20
  @test B[1, 1] == 20
  @test A[1, 1] == 20

  A = 0
  GC.gc()
  @test B[1, 1] == 20
end

@testset "nmod_mat.sub..." begin
   Z17 = ResidueRing(ZZ, 17)
   S = MatrixSpace(Z17, 3, 3)

   A = S([1 2 3; 4 5 6; 7 8 9])

   B = @inferred sub(A, 1, 1, 2, 2)

   @test typeof(B) == nmod_mat
   @test B == MatrixSpace(Z17, 2, 2)([1 2; 4 5])

   B[1, 1] = 10
   @test A == S([1 2 3; 4 5 6; 7 8 9])

   C = @inferred sub(B, 1:2, 1:2)

   @test typeof(C) == nmod_mat
   @test C == MatrixSpace(Z17, 2, 2)([10 2; 4 5])

   C[1, 1] = 20
   @test B == MatrixSpace(Z17, 2, 2)([10 2; 4 5])
   @test A == S([1 2 3; 4 5 6; 7 8 9])
end

@testset "nmod_mat.concatenation..." begin
  Z17 = ResidueRing(ZZ,17)
  R = MatrixSpace(Z17, 3, 3)
  S = MatrixSpace(Z17, 3, 4)

  a = R([ 1 2 3 ; 3 2 1 ; 0 0 2 ])

  b = S([ 2 1 0 1; 0 0 0 0; 0 1 2 0 ])

  c = hcat(a,a)

  @test c == MatrixSpace(Z17, 3, 6)([1, 2, 3, 1, 2, 3,
                                     3, 2, 1, 3, 2, 1,
                                     0, 0, 2, 0, 0, 2])

  c = hcat(a,b)

  @test c == MatrixSpace(Z17, 3, 7)([1, 2, 3, 2, 1, 0, 1,
                                     3, 2, 1, 0, 0, 0, 0,
                                     0, 0, 2, 0, 1, 2, 0])

  @test_throws ErrorException c = hcat(a,transpose(b))

  c = vcat(a,transpose(b))

  @test c == MatrixSpace(Z17, 7, 3)([1, 2, 3,
                                     3, 2, 1,
                                     0, 0, 2,
                                     2, 0, 0,
                                     1, 0, 1,
                                     0, 0, 2,
                                     1, 0, 0])

  @test_throws ErrorException vcat(a,b)
end

@testset "nmod_mat.conversion..." begin
  Z17 = ResidueRing(ZZ, 17)
  R = MatrixSpace(Z17, 3, 3)
  S = MatrixSpace(ZZ, 3, 3)

  c = S()

  a = R([ 1 2 3 ; 3 2 1 ; 0 0 2 ])

  @test Array(a) == [Z17(1) Z17(2) Z17(3);
                     Z17(3) Z17(2) Z17(1);
                     Z17(0) Z17(0) Z17(2) ]

  b = lift(a)

  @test b == S([ 1 2 3; 3 2 1; 0 0 2])
  @test parent(b) == S

  lift!(c,a)

  @test c == S([ 1 2 3; 3 2 1; 0 0 2])
end

@testset "nmod_mat.charpoly..." begin
   R = ResidueRing(ZZ, 17)

   for dim = 0:5
      S = MatrixSpace(R, dim, dim)
      U, x = PolynomialRing(R, "x")

      for i = 1:10
         M = randmat(S, 5)

         p1 = charpoly(U, M)
         p2 = charpoly_danilevsky!(U, M)

         @test p1 == p2
      end
   end
end
