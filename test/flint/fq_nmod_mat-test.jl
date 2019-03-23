function randmat(R::FqNmodMatSpace)
   m = nrows(R)
   n = ncols(R)
   r = R()
   for i = 1:m
      for j = 1:n
         r[i, j] = rand(base_ring(R))
      end
   end
   return r
end

function test_fq_nmod_mat_constructors()
  print("fq_nmod_mat.constructors...")

  F4, a = FlintFiniteField(2, 2, "a")
  F9, b = FlintFiniteField(3, 2, "b") 

  R = FqNmodMatSpace(F4, 2, 2)

  @test elem_type(R) == fq_nmod_mat
  @test elem_type(FqNmodMatSpace) == fq_nmod_mat
  @test parent_type(fq_nmod_mat) == FqNmodMatSpace
  @test nrows(R) == 2
  @test ncols(R) == 2

  @test isa(R, FqNmodMatSpace)

  @test base_ring(R) == F4

  S = FqNmodMatSpace(F9, 2, 2)

  @test isa(S, FqNmodMatSpace)

  RR = FqNmodMatSpace(F4, 2, 2)

  @test isa(RR, FqNmodMatSpace)

  @test R == RR

  @test_throws ErrorException FqNmodMatSpace(F4, 2, -1)
  @test_throws ErrorException FqNmodMatSpace(F4, -1, 2)
  @test_throws ErrorException FqNmodMatSpace(F4, -1, -1)

  a = R()

  @test isa(a, fq_nmod_mat)
  @test parent(a) == R

  ar = [ BigInt(1) BigInt(1); BigInt(1) BigInt(1) ]

  b = R(ar)

  @test isa(b, fq_nmod_mat)
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
  @test isa(c, fq_nmod_mat)
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

  @test isa(d, fq_nmod_mat)
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

  @test isa(d, fq_nmod_mat)

  ar = MatrixSpace(ZZ, 2, 2)([ 1 1; 1 1])

  e = R(ar)

  @test isa(e, fq_nmod_mat)
  @test parent(e) == R
  @test nrows(e) == 2 && ncols(e) == 2

  ar = matrix(FlintZZ, [ 1 1 1 ; 1 1 1; 1 1 1])

  @test_throws ErrorException R(ar)

  ar = [ F4(1) F4(1); F4(1) F4(1) ]

  f = R(ar)

  @test isa(f, fq_nmod_mat)
  @test parent(f) == R
  @test nrows(f) == 2 && ncols(f) == 2
  @test_throws ErrorConstrDimMismatch R(reshape(ar,4,1))
  @test f == R([F4(1), F4(1), F4(1), F4(1)])
  @test_throws ErrorConstrDimMismatch R([F4(1) F4(1) ])
  @test_throws ErrorConstrDimMismatch R([F4(1) F4(1) ; F4(1) F4(1) ; F4(1) F4(1) ])
  @test_throws ErrorConstrDimMismatch R([F4(1), F4(1), F4(1)])
  @test_throws ErrorConstrDimMismatch R([F4(1), F4(1), F4(1), F4(1), F4(1)])

  @test isa(S(1), fq_nmod_mat)

  @test isa(S(fmpz(1)), fq_nmod_mat)

  @test isa(S(F9(1)), fq_nmod_mat)

  g = deepcopy(e)

  @test b == c
  @test c == d
  @test d == e
  @test e == f
  @test g == e

   arr = [1 2; 3 4]
   arr2 = [1, 2, 3, 4, 5, 6]

   for T in [F9, fmpz, Int, BigInt]
      M = matrix(F9, map(T, arr))
      @test isa(M, fq_nmod_mat)
      @test M.base_ring == F9

      M2 = matrix(F9, 2, 3, map(T, arr2))
      @test isa(M2, fq_nmod_mat)
      @test M2.base_ring == F9
      @test nrows(M2) == 2
      @test ncols(M2) == 3
      @test_throws ErrorConstrDimMismatch matrix(F9, 2, 2, map(T, arr2))
      @test_throws ErrorConstrDimMismatch matrix(F9, 2, 4, map(T, arr2))
   end

   M3 = zero_matrix(F9, 2, 3)

   @test isa(M3, fq_nmod_mat)
   @test M3.base_ring == F9

   M4 = identity_matrix(F9, 3)

   @test isa(M4, fq_nmod_mat)
   @test M4.base_ring == F9

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

  println("PASS")
end

function test_fq_nmod_mat_printing()
  print("fq_nmod_mat.printing...")

  F4, _  = FlintFiniteField(2, 2, "a")
  R = FqNmodMatSpace(F4, 2, 2)

  a = R(1)

  @test string(a) == "[1 0]\n[0 1]"

  println("PASS")
end

function test_fq_nmod_mat_manipulation()
  print("fq_nmod_mat.manipulation...")
  F4, _ = FlintFiniteField(2, 2, "a")
  R = FqNmodMatSpace(F4, 2, 2)
  F9, _ = FlintFiniteField(3, 2, "b")
  S = FqNmodMatSpace(F9, 2, 2)

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

  @test isa(d, fq_nmod_mat)

  e = zero(R)

  @test isa(e, fq_nmod_mat)

  @test iszero(e)

  @test_throws ErrorException one(MatrixSpace(F9, 1, 2))

  @test issquare(a)

  @test a == a
  @test a == deepcopy(a)
  @test a != aa

  @test transpose(b) == c

  @test transpose(MatrixSpace(F4,1,2)([ 1 2; ])) ==
          MatrixSpace(F4,2,1)(reshape([ 1 ; 2],2,1))

  @test_throws ErrorConstrDimMismatch transpose!(R([ 1 2 ;]))

  println("PASS")
end

function test_fq_nmod_mat_unary_ops()
  print("fq_nmod_mat.unary_ops...")

  F17, _ = FlintFiniteField(17, 1, "a")

  R = MatrixSpace(F17, 3, 4)
  RR = MatrixSpace(F17, 4, 3)

  a = R([ 1 2 3 1; 3 2 1 2; 1 3 2 0])

  b = R([ 2 1 0 1; 0 0 0 0; 0 1 2 0 ])

  c = R()

  d = -b

  @test d == R([ 15 16 0 16; 0 0 0 0; 0 16 15 0])

  println("PASS")
end

function test_fq_nmod_mat_binary_ops()
  print("fq_nmod_mat.binary_ops...")

  F17, _ = FlintFiniteField(17, 1, "a")

  R = MatrixSpace(F17, 3, 4)

  a = R([ 1 2 3 1; 3 2 1 2; 1 3 2 0])

  b = R([ 2 1 0 1; 0 0 0 0; 0 1 2 0 ])

  c = R()

  d = a + b

  @test d == R([3 3 3 2; 3 2 1 2; 1 4 4 0])

  d = a - b

  @test d == R([ 16 1 3 0; 3 2 1 2; 1 2 0 0 ])

  d = a*transpose(a)

  @test d == MatrixSpace(F17, 3, 3)([15 12 13; 12 1 11; 13 11 14])

  d = transpose(a)*a

  @test d == MatrixSpace(F17, 4, 4)([11 11 8 7; 11 0 14 6; 8 14 14 5; 7 6 5 5])

  println("PASS")
end

function test_fq_nmod_mat_adhoc_binary()
  print("fq_nmod_mat.adhoc_binary...")

  F17, _ = FlintFiniteField(17, 1, "a")

  R = MatrixSpace(F17, 3, 4)
  F2, _ = FlintFiniteField(2, 1, "a")

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

  println("PASS")
end

function test_fq_nmod_mat_comparison()
  print("fq_nmod_mat.comparison...")

  F17, _ = FlintFiniteField(17, 1, "a")

  R = MatrixSpace(F17, 3, 4)

  a = R([ 1 2 3 1; 3 2 1 2; 1 3 2 0])

  @test a == a

  @test deepcopy(a) == a

  @test a != R([0 1 3 1; 2 1 4 2; 1 1 1 1])

  println("PASS")
end

function test_fq_nmod_mat_adhoc_comparison()
  print("fq_nmod_mat.comparison...")

  F17, _ = FlintFiniteField(17, 1, "a")

  R = MatrixSpace(F17, 3, 4)

  @test R(5) == 5
  @test R(5) == fmpz(5)
  @test R(5) == F17(5)

  @test 5 == R(5)
  @test fmpz(5) == R(5)
  @test F17(5) == R(5)

  println("PASS")
end

function test_fq_nmod_mat_powering()
  print("fq_nmod_mat.powering...")

  F17, _ = FlintFiniteField(17, 1, "a")

  R = MatrixSpace(F17, 3, 4)

  a = R([ 1 2 3 1; 3 2 1 2; 1 3 2 0])

  f = a*transpose(a)

  g = f^1000

  @test g == MatrixSpace(F17, 3, 3)([1 2 2; 2 13 12; 2 12 15])

  println("PASS")
end

function test_fq_nmod_mat_row_echelon_form()
  print("fq_nmod_mat.row_echelon_form...")

  F17, _ = FlintFiniteField(17, 1, "a")
  R = MatrixSpace(F17, 3, 4)
  RR = MatrixSpace(F17, 4, 3)

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

  println("PASS")
end

function test_fq_nmod_mat_trace_det()
  print("fq_nmod_mat.trace_det...")

  F17, _ = FlintFiniteField(17, 1, "a")
  R = MatrixSpace(F17, 3, 4)
  RR = MatrixSpace(F17, 4, 3)

  a = R([ 1 2 3 1; 3 2 1 2; 1 3 2 0])

  aa = MatrixSpace(F17,3,3)([ 1 2 3; 3 2 1; 1 1 2])

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

  println("PASS")
end

function test_fq_nmod_mat_rank()
  print("fq_nmod_mat.rank...")

  F17, _ = FlintFiniteField(17, 1, "a")
  R = MatrixSpace(F17, 3, 4)
  RR = MatrixSpace(F17, 4, 3)

  a = R([ 1 2 3 1; 3 2 1 2; 1 3 2 0])

  aa = MatrixSpace(F17,3,3)([ 1 2 3; 3 2 1; 1 1 2])

  b = R([ 2 1 0 1; 0 0 0 0; 0 1 2 0 ])

  c = rank(a)

  @test c == 3

  c = rank(aa)

  @test c == 3

  c = rank(b)

  @test c == 2

  println("PASS")
end

function test_fq_nmod_mat_inv()
  print("fq_nmod_mat.inv...")

  F17, _ = FlintFiniteField(17, 1, "a")
  R = MatrixSpace(F17, 3, 4)
  RR = MatrixSpace(F17, 4, 3)

  a = R([ 1 2 3 1; 3 2 1 2; 1 3 2 0])

  aa = MatrixSpace(F17,3,3)([ 1 2 3; 3 2 1; 1 1 2])

  b = R([ 2 1 0 1; 0 0 0 0; 0 1 2 0 ])

  c = inv(aa)

  @test c == parent(aa)([12 13 1; 14 13 15; 4 4 1])

  @test_throws ErrorException inv(a)

  @test_throws ErrorException inv(transpose(a)*a)

  println("PASS")
end

function test_fq_nmod_mat_solve()
  print("fq_nmod_mat.solve...")

  F17, _ = FlintFiniteField(17, 1, "a")
  R = MatrixSpace(F17, 3, 3)
  S = MatrixSpace(F17, 3, 4)

  a = R([ 1 2 3 ; 3 2 1 ; 0 0 2 ])

  b = S([ 2 1 0 1; 0 0 0 0; 0 1 2 0 ])

  c = a*b

  d = solve(a,c)

  @test d == b

  a = zero(R)

  @test_throws ErrorException  solve(a,c)

  println("PASS")
end

function test_fq_nmod_mat_lu()
  print("fq_nmod_mat.lu...")


  F17, _ = FlintFiniteField(17, 1, "a")
  R = MatrixSpace(F17, 3, 3)
  S = MatrixSpace(F17, 3, 4)

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

  println("PASS")
end

function test_fq_nmod_mat_view()
  print("fq_nmod_mat.view...")

  F17, _ = FlintFiniteField(17, 1, "a")
  R = MatrixSpace(F17, 3, 3)
  S = MatrixSpace(F17, 3, 4)

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

  println("PASS")
end

function test_fq_nmod_mat_sub()
   print("fq_nmod_mat.sub...")

   F17, _ = FlintFiniteField(17, 1, "a")
   S = MatrixSpace(F17, 3, 3)

   A = S([1 2 3; 4 5 6; 7 8 9])

   B = @inferred sub(A, 1, 1, 2, 2)

   @test typeof(B) == nmod_mat
   @test B == MatrixSpace(F17, 2, 2)([1 2; 4 5])

   B[1, 1] = 10
   @test A == S([1 2 3; 4 5 6; 7 8 9])

   C = @inferred sub(B, 1:2, 1:2)

   @test typeof(C) == nmod_mat
   @test C == MatrixSpace(F17, 2, 2)([10 2; 4 5])

   C[1, 1] = 20
   @test B == MatrixSpace(F17, 2, 2)([10 2; 4 5])
   @test A == S([1 2 3; 4 5 6; 7 8 9])

   println("PASS")
end

function test_fq_nmod_mat_concatenation()
  print("fq_nmod_mat.concatenation...")

  F17, _ = FlintFiniteField(17, 1, "a")
  R = MatrixSpace(F17, 3, 3)
  S = MatrixSpace(F17, 3, 4)

  a = R([ 1 2 3 ; 3 2 1 ; 0 0 2 ])

  b = S([ 2 1 0 1; 0 0 0 0; 0 1 2 0 ])

  c = hcat(a,a)

  @test c == MatrixSpace(F17, 3, 6)([1, 2, 3, 1, 2, 3,
                                     3, 2, 1, 3, 2, 1,
                                     0, 0, 2, 0, 0, 2])

  c = hcat(a,b)

  @test c == MatrixSpace(F17, 3, 7)([1, 2, 3, 2, 1, 0, 1,
                                     3, 2, 1, 0, 0, 0, 0,
                                     0, 0, 2, 0, 1, 2, 0])

  @test_throws ErrorException c = hcat(a,transpose(b))

  c = vcat(a,transpose(b))

  @test c == MatrixSpace(F17, 7, 3)([1, 2, 3,
                                     3, 2, 1,
                                     0, 0, 2,
                                     2, 0, 0,
                                     1, 0, 1,
                                     0, 0, 2,
                                     1, 0, 0])

  @test_throws ErrorException vcat(a,b)

  println("PASS")
end

function test_fq_nmod_mat_conversion()
  print("fq_nmod_mat.conversion...")

  F17, _ = FlintFiniteField(17, 1, "a")
  R = MatrixSpace(F17, 3, 3)

  a = R([ 1 2 3 ; 3 2 1 ; 0 0 2 ])

  @test Array(a) == [F17(1) F17(2) F17(3);
                     F17(3) F17(2) F17(1);
                     F17(0) F17(0) F17(2) ]

  println("PASS")
end

function test_fq_nmod_mat_charpoly()
   print("fq_nmod_mat.charpoly...")

   F17, _ = FlintFiniteField(17, 1, "a")

   for dim = 0:5
      S = MatrixSpace(F17, dim, dim)
      U, x = PolynomialRing(F17, "x")

      for i = 1:10
         M = randmat(S)
         N = deepcopy(M)

         p1 = charpoly(U, M)
         p2 = charpoly_danilevsky!(U, M)

         @test p1 == p2
         @test iszero(subst(p1, N))
      end
   end

   println("PASS")
end

function test_fq_nmod_mat()
  test_fq_nmod_mat_constructors()
  test_fq_nmod_mat_printing()
  test_fq_nmod_mat_manipulation()
  test_fq_nmod_mat_unary_ops()
  test_fq_nmod_mat_binary_ops()
  test_fq_nmod_mat_adhoc_binary()
  test_fq_nmod_mat_comparison()
  test_fq_nmod_mat_adhoc_comparison()
  test_fq_nmod_mat_powering()
  test_fq_nmod_mat_row_echelon_form()
  test_fq_nmod_mat_trace_det()
  test_fq_nmod_mat_rank()
  test_fq_nmod_mat_inv()
  test_fq_nmod_mat_lu()
  test_fq_nmod_mat_view()
  test_fq_nmod_mat_concatenation()
  test_fq_nmod_mat_conversion()
  test_fq_nmod_mat_charpoly()

  println("")
end
