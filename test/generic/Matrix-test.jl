function randpoly(S, d::Int, n::Int)
   r = S()
   x = gen(S)
   for i = 0:rand(0:d)
      r += rand(-n:n)*x^i
   end
   return r
end

function randelem(K::AnticNumberField, n)
   s = K(0)
   a = gen(K)
   for i = 1:3
      s += rand(-n:n)*a^(i-1)
   end
   return s
end

function randelem(R::FmpzPolyRing, n)
   s = R(0)
   x = gen(R)
   for i = 1:3
      s += rand(-n:n)*x^(i-1)
   end
   return s
end

function randelem(R::GenResRing{fmpz}, n)
   return R(rand(-n:n))
end

function randelem(R, n)
   s = R(0)
   x = gen(R)
   for i = 1:3
      s += randelem(base_ring(R), n)*x^(i-1)
   end
   return s
end

function randmat{T <: RingElem}(R::GenMatSpace{T}, d::Int, c::Int)
   m = R.rows
   n = R.cols
   r = R()
   for i = 1:m
      for j = 1:n
         r[i, j] = randpoly(base_ring(R), d, c)
      end
   end
   return r
end

function randmat{T <: RingElem}(S::GenMatSpace{T}, c::Int)
   M = S()
   m = rows(M)
   n = cols(M)
   for i = 1:m
      for j = 1:n
         M[i, j] = randelem(base_ring(S), c)
      end
   end
   return M
end

function randmat_triu{T <: RingElem}(R::GenMatSpace{T}, d::Int, c::Int)
   m = R.rows
   n = R.cols
   r = R()
   for i = 1:m
      for j = 1:i - 1
         r[i, j] = base_ring(R)()
      end
      for j = i:n
         r[i, j] = randpoly(base_ring(R), d, c)
      end
      while r[i, i] == 0
         r[i, i] = randpoly(base_ring(R), d, c)
      end
   end
   return r
end

function randmat_triu{T <: RingElem}(S::GenMatSpace{T}, c::Int)
   M = S()
   m = rows(M)
   n = cols(M)
   for i = 1:m
      for j = 1:i - 1
         M[i, j] = base_ring(S)()
      end
      for j = i:n
         M[i, j] = randelem(base_ring(S), c)
      end
      while M[i, i] == 0
         M[i, i] = randelem(base_ring(S), c)
      end
   end
   return M
end

function randmat_with_rank{T <: RingElem}(R::GenMatSpace{T}, d::Int, c::Int, rank::Int)
   m = R.rows
   n = R.cols
   r = R()
   for i = 1:rank
      for j = 1:i - 1
         r[i, j] = base_ring(R)()
      end
      r[i, i] = randpoly(base_ring(R), d, c)
      while r[i, i] == 0
         r[i, i] = randpoly(base_ring(R), d, c)
      end
      for j = i + 1:n
         r[i, j] = randpoly(base_ring(R), d, c)
      end
   end
   for i = rank + 1:m
      for j = 1:n
         r[i, j] = base_ring(R)()
      end
   end
   if m > 1
      for i = 1:4m
         r1 = rand(1:m)
         r2 = rand(1:m - 1)
         r2 = r2 >= r1 ? r2 + 1 : r2
         d = rand(-5:5)
         for j = 1:n
            r[r1, j] = r[r1, j] + d*r[r2, j]
         end
      end
   end
   return r
end

function randmat_with_rank{T <: RingElem}(S::GenMatSpace{T}, c::Int, rank::Int)
   M = S()
   m = rows(M)
   n = cols(M)
   for i = 1:rank
      for j = 1:i - 1
         M[i, j] = base_ring(S)()
      end
      M[i, i] = randelem(base_ring(S), c)
      while M[i, i] == 0
         M[i, i] = randelem(base_ring(S), c)
      end
      for j = i + 1:n
         M[i, j] = randelem(base_ring(S), c)
      end
   end
   for i = rank + 1:m
      for j = 1:n
         M[i, j] = base_ring(S)()
      end
   end
   if m > 1
      for i = 1:4m
         r1 = rand(1:m)
         r2 = rand(1:m - 1)
         r2 = r2 >= r1 ? r2 + 1 : r2
         d = rand(-5:5)
         for j = 1:n
            M[r1, j] = M[r1, j] + d*M[r2, j]
         end
      end
   end
   return M
end

function test_gen_mat_constructors()
   print("GenMat.constructors...")
 
   R, t = PolynomialRing(QQ, "t")
   S = MatrixSpace(R, 3, 3)

   @test typeof(S) <: GenMatSpace

   f = S(t^2 + 1)

   @test isa(f, MatElem)

   g = S(2)

   @test isa(g, MatElem)

   h = S(fmpz(23))

   @test isa(h, MatElem)

   k = S([t t + 2 t^2 + 3t + 1; 2t R(2) t + 1; t^2 + 2 t + 1 R(0)])

   @test isa(k, MatElem)

   l = S(k)

   @test isa(l, MatElem)

   m = S()

   @test isa(m, MatElem)

   println("PASS")
end

function test_gen_mat_manipulation()
   print("GenMat.manipulation...")

   R, t = PolynomialRing(QQ, "t")
   S = MatrixSpace(R, 3, 3)

   A = S([t + 1 t R(1); t^2 t t; R(-2) t + 2 t^2 + t + 1])
   B = S([R(2) R(3) R(1); t t + 1 t + 2; R(-1) t^2 t^3])

   @test iszero(zero(S))
   @test isone(one(S))

   B[1, 1] = R(3)

   @test B[1, 1] == R(3)

   @test rows(B) == 3
   @test cols(B) == 3

   @test deepcopy(A) == A

   println("PASS")
end

function test_gen_mat_unary_ops()
   print("GenMat.unary_ops...")

   R, t = PolynomialRing(QQ, "t")
   S = MatrixSpace(R, 3, 3)

   A = S([t + 1 t R(1); t^2 t t; R(-2) t + 2 t^2 + t + 1])
   B = S([-t - 1 (-t) -R(1); -t^2 (-t) (-t); -R(-2) (-t - 2) (-t^2 - t - 1)])

   @test -A == B

   println("PASS")
end

function test_gen_mat_binary_ops()
   print("GenMat.binary_ops...")

   R, t = PolynomialRing(QQ, "t")
   S = MatrixSpace(R, 3, 3)

   A = S([t + 1 t R(1); t^2 t t; R(-2) t + 2 t^2 + t + 1])
   B = S([R(2) R(3) R(1); t t + 1 t + 2; R(-1) t^2 t^3])

   @test A + B == S([t+3 t+3 R(2); t^2 + t 2*t+1 2*t+2; R(-3) t^2 + t + 2 t^3 + 1*t^2 + t + 1])

   @test A - B == S([t-1 t-3 R(0); t^2 - t R(-1) R(-2); R(-1) (-t^2 + t + 2) (-t^3 + t^2 + t + 1)])

   @test A*B == S([t^2 + 2*t + 1 2*t^2 + 4*t + 3 t^3 + t^2 + 3*t + 1; 3*t^2 - t (t^3 + 4*t^2 + t) t^4 + 2*t^2 + 2*t; t-5 t^4 + t^3 + 2*t^2 + 3*t - 4 t^5 + 1*t^4 + t^3 + t^2 + 4*t + 2])

   println("PASS")
end

function test_gen_mat_adhoc_binary()
   print("GenMat.adhoc_binary...")

   R, t = PolynomialRing(QQ, "t")
   S = MatrixSpace(R, 3, 3)

   A = S([t + 1 t R(1); t^2 t t; R(-2) t + 2 t^2 + t + 1])

   @test 12 + A == A + 12
   @test fmpz(11) + A == A + fmpz(11)
   @test (t + 1) + A == A + (t + 1)
   @test A - (t + 1) == -((t + 1) - A)
   @test A - 3 == -(3 - A)
   @test A - fmpz(7) == -(fmpz(7) - A)
   @test 3*A == A*3
   @test fmpz(3)*A == A*fmpz(3)
   @test (t - 1)*A == A*(t - 1)

   println("PASS")
end

function test_gen_mat_permutation()
   print("GenMat.permutation...")

   R, t = PolynomialRing(QQ, "t")
   S = MatrixSpace(R, 3, 3)

   A = S([t + 1 t R(1); t^2 t t; R(-2) t + 2 t^2 + t + 1])

   T = PermutationGroup(3)
   P = T([2, 3, 1])

   @test A == inv(P)*(P*A)

   println("PASS")
end

function test_gen_mat_comparison()
   print("GenMat.comparison...")

   R, t = PolynomialRing(QQ, "t")
   S = MatrixSpace(R, 3, 3)

   A = S([t + 1 t R(1); t^2 t t; R(-2) t + 2 t^2 + t + 1])
   B = S([t + 1 t R(1); t^2 t t; R(-2) t + 2 t^2 + t + 1])

   @test A == B

   @test A != one(S)

   println("PASS")
end

function test_gen_mat_adhoc_comparison()
   print("GenMat.adhoc_comparison...")

   R, t = PolynomialRing(QQ, "t")
   S = MatrixSpace(R, 3, 3)

   A = S([t + 1 t R(1); t^2 t t; R(-2) t + 2 t^2 + t + 1])

   @test S(12) == 12
   @test S(5) == fmpz(5)
   @test S(t + 1) == t + 1
   @test 12 == S(12)
   @test fmpz(5) == S(5)
   @test t + 1 == S(t + 1)
   @test A != one(S)
   @test one(S) == one(S)

   println("PASS")
end

function test_gen_mat_powering()
   print("GenMat.powering...")

   R, t = PolynomialRing(QQ, "t")
   S = MatrixSpace(R, 3, 3)

   A = S([t + 1 t R(1); t^2 t t; R(-2) t + 2 t^2 + t + 1])

   @test A^5 == A^2*A^3

   @test A^0 == one(S)

   println("PASS")
end

function test_gen_mat_adhoc_exact_division()
   print("GenMat.adhoc_exact_division...")

   R, t = PolynomialRing(QQ, "t")
   S = MatrixSpace(R, 3, 3)

   A = S([t + 1 t R(1); t^2 t t; R(-2) t + 2 t^2 + t + 1])

   @test divexact(5*A, 5) == A
   @test divexact(12*A, fmpz(12)) == A
   @test divexact((1 + t)*A, 1 + t) == A

   println("PASS")
end

function test_gen_mat_gram()
   print("GenMat.gram...")

   R, t = PolynomialRing(QQ, "t")
   S = MatrixSpace(R, 3, 3)

   A = S([t + 1 t R(1); t^2 t t; R(-2) t + 2 t^2 + t + 1])

   @test gram(A) == S([2*t^2 + 2*t + 2 t^3 + 2*t^2 + t 2*t^2 + t - 1; t^3 + 2*t^2 + t t^4 + 2*t^2 t^3 + 3*t; 2*t^2 + t - 1 t^3 + 3*t t^4 + 2*t^3 + 4*t^2 + 6*t + 9])

   println("PASS")
end

function test_gen_mat_trace()
   print("GenMat.trace...")

   R, t = PolynomialRing(QQ, "t")
   S = MatrixSpace(R, 3, 3)

   A = S([t + 1 t R(1); t^2 t t; R(-2) t + 2 t^2 + t + 1])

   @test trace(A) == t^2 + 3t + 2

   println("PASS")
end

function test_gen_mat_content()
   print("GenMat.content...")

   R, t = PolynomialRing(QQ, "t")
   S = MatrixSpace(R, 3, 3)

   A = S([t + 1 t R(1); t^2 t t; R(-2) t + 2 t^2 + t + 1])

   @test content((1 + t)*A) == 1 + t 
   println("PASS")
end

function test_gen_mat_lufact()
   print("GenMat.lufact...")

   R, x = PolynomialRing(QQ, "x")
   K, a = NumberField(x^3 + 3x + 1, "a")
   S = MatrixSpace(K, 3, 3)
   
   A = S([a + 1 2a + 3 a^2 + 1; 2a^2 - 1 a - 1 2a; a^2 + 3a + 1 2a K(1)])

   r, P, L, U = lufact(A)

   @test r == 3
   @test P*A == L*U

   A = S([K(0) 2a + 3 a^2 + 1; a^2 - 2 a - 1 2a; a^2 + 3a + 1 2a K(1)])

   r, P, L, U = lufact(A)

   @test r == 3
   @test P*A == L*U

   A = S([K(0) 2a + 3 a^2 + 1; a^2 - 2 a - 1 2a; a^2 - 2 a - 1 2a])

   r, P, L, U = lufact(A)

   @test r == 2
   @test P*A == L*U

   println("PASS")
end

function test_gen_mat_fflu()
   print("GenMat.fflu...")

   R, x = PolynomialRing(QQ, "x")
   K, a = NumberField(x^3 + 3x + 1, "a")
   S = MatrixSpace(K, 3, 3)
   
   A = S([a + 1 2a + 3 a^2 + 1; 2a^2 - 1 a - 1 2a; a^2 + 3a + 1 2a K(1)])

   r, d, P, L, U = fflu(A)

   D = S()
   D[1, 1] = inv(U[1, 1])
   D[2, 2] = inv(U[1, 1]*U[2, 2])
   D[3, 3] = inv(U[2, 2])
   
   @test r == 3
   @test P*A == L*D*U

   A = S([K(0) 2a + 3 a^2 + 1; a^2 - 2 a - 1 2a; a^2 + 3a + 1 2a K(1)])

   r, d, P, L, U = fflu(A)

   D = S()
   D[1, 1] = inv(U[1, 1])
   D[2, 2] = inv(U[1, 1]*U[2, 2])
   D[3, 3] = inv(U[2, 2])
   
   @test r == 3
   @test P*A == L*D*U

   A = S([K(0) 2a + 3 a^2 + 1; a^2 - 2 a - 1 2a; a^2 - 2 a - 1 2a])

   r, d, P, L, U = fflu(A)

   D = S()
   D[1, 1] = inv(U[1, 1])
   D[2, 2] = inv(U[1, 1]*U[2, 2])
   D[3, 3] = inv(U[2, 2])
   
   @test r == 2
   @test P*A == L*D*U

   println("PASS")
end

function test_gen_mat_det()
   print("GenMat.det...")

   S, x = PolynomialRing(ResidueRing(ZZ, 1009*2003), "x")

   for dim = 0:10
      R = MatrixSpace(S, dim, dim)

      M = randmat(R, 5, 100);

      @test det(M) == Nemo.det_clow(M)
   end

   S, z = PolynomialRing(ZZ, "z")

   for dim = 0:10
      R = MatrixSpace(S, dim, dim)

      M = randmat(R, 3, 20);

      @test det(M) == Nemo.det_clow(M)
   end

   R, x = PolynomialRing(QQ, "x")
   K, a = NumberField(x^3 + 3x + 1, "a")
   
   for dim = 0:10
      S = MatrixSpace(K, dim, dim)

      M = randmat(S, 100);

      @test det(M) == Nemo.det_clow(M)
   end

   R, x = PolynomialRing(ZZ, "x")
   S, y = PolynomialRing(R, "y")
   
   for dim = 0:10
      T = MatrixSpace(S, dim, dim)
      M = randmat(T, 20)
      
      @test det(M) == Nemo.det_clow(M)
   end

   println("PASS")
end

function test_gen_mat_rank()
   print("GenMat.rank...")

   S, x = PolynomialRing(ResidueRing(ZZ, 1009*2003), "x")
   R = MatrixSpace(S, 3, 3)

   M = R([S(3) S(2) S(1); S(2021024) S(2021025) S(2021026); 3*x^2+5*x+2021024 2021022*x^2+4*x+5 S(2021025)])

   @test rank(M) == 2

   S, x = PolynomialRing(ResidueRing(ZZ, 20011*10007), "x")
   R = MatrixSpace(S, 5, 5)

   for i = 0:5
      M = randmat_with_rank(R, 5, 100, i)

      @test rank(M) == i
   end

   S, z = PolynomialRing(ZZ, "z")
   R = MatrixSpace(S, 4, 4)

   M = R([S(-2) S(0) S(5) S(3); 5*z^2+5*z-5 S(0) S(-z^2+z) 5*z^2+5*z+1; 2*z-1 S(0) z^2+3*z+2 S(-4*z); 3*z-5 S(0) S(-5*z+5) S(1)])

   @test rank(M) == 3

   R = MatrixSpace(S, 5, 5)

   for i = 0:5
      M = randmat_with_rank(R, 3, 20, i)

      @test rank(M) == i
   end

   R, x = PolynomialRing(QQ, "x")
   K, a = NumberField(x^3 + 3x + 1, "a")
   S = MatrixSpace(K, 3, 3)

   M = S([a a^2 + 2*a - 1 2*a^2 - 1*a; 2*a+2 2*a^2 + 2*a (-2*a^2 - 2*a); (-a) (-a^2) a^2])

   @test rank(M) == 2

   S = MatrixSpace(K, 5, 5)

   for i = 0:5
      M = randmat_with_rank(S, 100, i)

      @test rank(M) == i
   end

   R, x = PolynomialRing(ZZ, "x")
   S, y = PolynomialRing(R, "y")
   T = MatrixSpace(S, 3, 3)

   M = T([(2*x^2)*y^2+(-2*x^2-2*x)*y+(-x^2+2*x) S(0) (-x^2-2)*y^2+(x^2+2*x+2)*y+(2*x^2-x-1); 
    (-x)*y^2+(-x^2+x-1)*y+(x^2-2*x+2) S(0) (2*x^2+x-1)*y^2+(-2*x^2-2*x-2)*y+(x^2-x);
    (-x+2)*y^2+(x^2+x+1)*y+(-x^2+x-1) S(0) (-x^2-x+2)*y^2+(-x-1)*y+(-x-1)])

   @test rank(M) == 2

   T = MatrixSpace(S, 5, 5)

   for i = 0:5
      M = randmat_with_rank(T, 20, i)

      @test rank(M) == i
   end

   println("PASS")   
end

function test_gen_mat_solve()
   print("GenMat.solve...")

   S, x = PolynomialRing(ResidueRing(ZZ, 20011*10007), "x")

   for dim = 0:10
      R = MatrixSpace(S, dim, dim)
      U = MatrixSpace(S, dim, rand(1:5))

      M = randmat_with_rank(R, 5, 100, dim);
      b = randmat(U, 5, 100);

      x, d = solve(M, b)

      @test M*x == d*b
   end

   S, z = PolynomialRing(ZZ, "z")

   for dim = 0:10
      R = MatrixSpace(S, dim, dim)
      U = MatrixSpace(S, dim, rand(1:5))

      M = randmat_with_rank(R, 3, 20, dim);
      b = randmat(U, 3, 20);

      x, d = solve(M, b)

      @test M*x == d*b
   end

   R, x = PolynomialRing(QQ, "x")
   K, a = NumberField(x^3 + 3x + 1, "a")
   
   for dim = 0:10
      S = MatrixSpace(K, dim, dim)
      U = MatrixSpace(K, dim, rand(1:5))

      M = randmat_with_rank(S, 100, dim);
      b = randmat(U, 100);

      x = solve(M, b)

      @test M*x == b
   end

   R, x = PolynomialRing(ZZ, "x")
   S, y = PolynomialRing(R, "y")
   
   for dim = 0:10
      T = MatrixSpace(S, dim, dim)
      U = MatrixSpace(S, dim, rand(1:5))
     
      M = randmat_with_rank(T, 20, dim)
      b = randmat(U, 20)
 
      x, d = solve(M, b)

      @test M*x == d*b
   end

   R, t = PolynomialRing(QQ, "t")
   K, a = NumberField(t^3 + 3t + 1, "a")
   S, y = PolynomialRing(K, "y")
   T = MatrixSpace(S, 3, 3)
   U = MatrixSpace(S, 3, 1)

   M = T([3y*a^2 + (y + 1)*a + 2y (5y+1)*a^2 + 2a + y - 1 a^2 + (-a) + 2y; (y + 1)*a^2 + 2y - 4 3y*a^2 + (2y - 1)*a + y (4y - 1)*a^2 + (y - 1)*a + 5; 2a + y + 1 (2y + 2)*a^2 + 3y*a + 3y a^2 + (-y-1)*a + (-y - 3)])
   b = U([4y*a^2 + 4y*a + 2y + 1 5y*a^2 + (2y + 1)*a + 6y + 1 (y + 1)*a^2 + 3y*a + 2y + 4]')

   x, d = solve(M, b)

   @test M*x == d*b

   println("PASS")
end

function test_gen_mat_solve_triu()
   print("GenMat.solve_triu...")

   R, x = PolynomialRing(QQ, "x")
   K, a = NumberField(x^3 + 3x + 1, "a")
   
   for dim = 0:10
      S = MatrixSpace(K, dim, dim)
      U = MatrixSpace(K, dim, rand(1:5))

      M = randmat_triu(S, 100);
      b = randmat(U, 100);

      x = solve_triu(M, b, false)

      @test M*x == b
   end

   println("PASS")
end

function test_gen_mat_rref()
   print("GenMat.rref...")

   S, x = PolynomialRing(ResidueRing(ZZ, 20011*10007), "x")
   R = MatrixSpace(S, 5, 5)

   for i = 0:5
      M = randmat_with_rank(R, 5, 100, i)

      r, d, A = rref(M)

      @test r == i
      @test is_rref(A)
   end

   S, z = PolynomialRing(ZZ, "z")
   R = MatrixSpace(S, 5, 5)

   for i = 0:5
      M = randmat_with_rank(R, 3, 20, i)

      r, d, A = rref(M)

      @test r == i
      @test is_rref(A)
   end

   R, x = PolynomialRing(QQ, "x")
   K, a = NumberField(x^3 + 3x + 1, "a")
   S = MatrixSpace(K, 5, 5)

   for i = 0:5
      M = randmat_with_rank(S, 100, i)

      r, A = rref(M)

      @test r == i
      @test is_rref(A)
   end

   R, x = PolynomialRing(ZZ, "x")
   S, y = PolynomialRing(R, "y")
   T = MatrixSpace(S, 5, 5)

   for i = 0:5
      M = randmat_with_rank(T, 20, i)

      r, d, A = rref(M)

      @test r == i
      @test is_rref(A)
   end

   println("PASS")   
end

function test_gen_mat_nullspace()
   print("GenMat.nullspace...")

   S, x = PolynomialRing(ResidueRing(ZZ, 20011*10007), "x")
   R = MatrixSpace(S, 5, 5)

   for i = 0:5
      M = randmat_with_rank(R, 5, 100, i)

      n, N = nullspace(M)

      @test n == 5 - i
      @test rank(N) == n
      @test iszero(M*N)
   end

   S, z = PolynomialRing(ZZ, "z")
   R = MatrixSpace(S, 5, 5)

   for i = 0:5
      M = randmat_with_rank(R, 3, 20, i)

      n, N = nullspace(M)

      @test n == 5 - i
      @test rank(N) == n
      @test iszero(M*N)
   end

   R, x = PolynomialRing(QQ, "x")
   K, a = NumberField(x^3 + 3x + 1, "a")
   S = MatrixSpace(K, 5, 5)

   for i = 0:5
      M = randmat_with_rank(S, 100, i)

      n, N = nullspace(M)

      @test n == 5 - i
      @test rank(N) == n
      @test iszero(M*N)
   end

   R, x = PolynomialRing(ZZ, "x")
   S, y = PolynomialRing(R, "y")
   T = MatrixSpace(S, 5, 5)

   for i = 0:5
      M = randmat_with_rank(T, 20, i)

      n, N = nullspace(M)

      @test n == 5 - i
      @test rank(N) == n
      @test iszero(M*N)
   end

   println("PASS")   
end

function test_gen_mat_inversion()
   print("GenMat.inversion...")

   S, x = PolynomialRing(ResidueRing(ZZ, 20011*10007), "x")

   for dim = 1:10
      R = MatrixSpace(S, dim, dim)
      
      M = randmat_with_rank(R, 5, 100, dim);
      
      X, d = inv(M)

      @test M*X == d*one(R)
   end

   S, z = PolynomialRing(ZZ, "z")

   for dim = 1:10
      R = MatrixSpace(S, dim, dim)
      
      M = randmat_with_rank(R, 3, 20, dim);
      
      X, d = inv(M)

      @test M*X == d*one(R)
   end

   R, x = PolynomialRing(QQ, "x")
   K, a = NumberField(x^3 + 3x + 1, "a")
   
   for dim = 1:10
      S = MatrixSpace(K, dim, dim)

      M = randmat_with_rank(S, 100, dim);

      X = inv(M)

      @test isone(M*X)
   end

   R, x = PolynomialRing(ZZ, "x")
   S, y = PolynomialRing(R, "y")
   
   for dim = 1:10
      T = MatrixSpace(S, dim, dim)
     
      M = randmat_with_rank(T, 20, dim)
  
      X, d = inv(M)

      @test M*X == d*one(T)
   end

   println("PASS")   
end

function test_gen_mat_hessenberg()
   print("GenMat.hessenberg...")

   R = ResidueRing(ZZ, 18446744073709551629)

   for dim = 0:5
      S = MatrixSpace(R, dim, dim)
      U, x = PolynomialRing(R, "x")

      for i = 1:10
         M = randmat(S, 5)

         A = hessenberg(M)

         @test is_hessenberg(A)
      end
   end

   println("PASS")   
end

function test_gen_mat_charpoly()
   print("GenMat.charpoly...")

   R = ResidueRing(ZZ, 18446744073709551629)

   for dim = 0:5
      S = MatrixSpace(R, dim, dim)
      U, x = PolynomialRing(R, "x")

      for i = 1:10
         M = randmat(S, 5)

         p1 = charpoly(U, M)
         p2 = charpoly_danilevsky!(U, M)

         @test p1 == p2
      end

      for i = 1:10
         M = randmat(S, 5)

         p1 = charpoly(U, M)
         p2 = charpoly_danilevsky_ff!(U, M)

         @test p1 == p2
      end

      for i = 1:10
         M = randmat(S, 5)

         p1 = charpoly(U, M)
         p2 = charpoly_hessenberg!(U, M)

         @test p1 == p2
      end
   end

   R, x = PolynomialRing(ZZ, "x")
   U, z = PolynomialRing(R, "z")
   T = MatrixSpace(R, 6, 6)

   M = T()
   for i = 1:3
      for j = 1:3
         M[i, j] = randelem(R, 10)
         M[i + 3, j + 3] = deepcopy(M[i, j])
      end
   end

   p1 = charpoly(U, M)

   for i = 1:10
      similarity!(M, rand(1:6), R(randelem(R, 3)))
   end

   p2 = charpoly(U, M)

   @test p1 == p2

   println("PASS")   
end

function test_gen_mat_minpoly()
   print("GenMat.minpoly...")

   R, x = FiniteField(103, 1, "x")
   T, y = PolynomialRing(R, "y")

   M = R[92 97 8;
          0 5 13;
          0 16 2]

   @test minpoly(T, M) == y^2+96*y+8

   R, x = FiniteField(3, 1, "x")
   T, y = PolynomialRing(R, "y")

   M = R[1 2 0 2;
         1 2 1 0;
         1 2 2 1;
         2 1 2 0]

   @test minpoly(T, M) == y^2 + 2y

   R, x = FiniteField(13, 1, "x")
   T, y = PolynomialRing(R, "y")

   M = R[7 6 1;
         7 7 5;
         8 12 5]

   @test minpoly(T, M) == y^2+10*y

   M = R[4 0 9 5;
         1 0 1 9;
         0 0 7 6;
         0 0 3 10]

   @test minpoly(T, M) == y^2 + 9y

   M = R[2 7 0 0 0 0;
         1 0 0 0 0 0;
         0 0 2 7 0 0;
         0 0 1 0 0 0;
         0 0 0 0 4 3;
         0 0 0 0 1 0]

   @test minpoly(T, M) == (y^2+9*y+10)*(y^2+11*y+6)

   M = R[2 7 0 0 0 0;
         1 0 1 0 0 0;
         0 0 2 7 0 0;
         0 0 1 0 0 0;
         0 0 0 0 4 3;
         0 0 0 0 1 0]

   @test minpoly(T, M) == (y^2+9*y+10)*(y^2+11*y+6)^2

   S = MatrixSpace(R, 1, 1)
   M = S()

   @test minpoly(T, M) == y

   S = MatrixSpace(R, 0, 0)
   M = S()

   @test minpoly(T, M) == 1

   R, x = PolynomialRing(ZZ, "x")
   S, y = PolynomialRing(R, "y")
   U, z = PolynomialRing(S, "z")
   T = MatrixSpace(S, 6, 6)

   M = T()
   for i = 1:3
      for j = 1:3
         M[i, j] = randelem(S, 10)
         M[i + 3, j + 3] = deepcopy(M[i, j])
      end
   end

   f = minpoly(U, M)

   @test degree(f) <= 3

   R, x = PolynomialRing(ZZ, "x")
   U, z = PolynomialRing(R, "z")
   T = MatrixSpace(R, 6, 6)

   M = T()
   for i = 1:3
      for j = 1:3
         M[i, j] = randelem(R, 10)
         M[i + 3, j + 3] = deepcopy(M[i, j])
      end
   end

   p1 = minpoly(U, M)

   for i = 1:10
      similarity!(M, rand(1:6), R(randelem(R, 3)))
   end

   p2 = minpoly(U, M)

   @test p1 == p2

   println("PASS")   
end

function test_gen_concat()
   print("GenMat.concat...")

   R, x = PolynomialRing(ZZ, "x")
      
   for i = 1:10
      r = rand(0:10)
      c1 = rand(0:10)
      c2 = rand(0:10)

      S1 = MatrixSpace(R, r, c1)
      S2 = MatrixSpace(R, r, c2)

      M1 = randmat(S1, 3, 100)
      M2 = randmat(S2, 3, 100)

      @test vcat(transpose(M1), transpose(M2)) == transpose(hcat(M1, M2))
   end

   println("PASS")   
end

function test_gen_mat()
   test_gen_mat_constructors()
   test_gen_mat_manipulation()
   test_gen_mat_unary_ops()
   test_gen_mat_binary_ops()
   test_gen_mat_adhoc_binary()
   test_gen_mat_permutation()
   test_gen_mat_comparison()
   test_gen_mat_adhoc_comparison()
   test_gen_mat_powering()
   test_gen_mat_adhoc_exact_division()
   test_gen_mat_gram()
   test_gen_mat_trace()
   test_gen_mat_content()
   test_gen_mat_lufact()
   test_gen_mat_fflu()
   test_gen_mat_det()
   test_gen_mat_rank()
   test_gen_mat_solve()
   test_gen_mat_solve_triu()
   test_gen_mat_rref()
   test_gen_mat_nullspace()
   test_gen_mat_inversion()
   test_gen_mat_hessenberg()
   test_gen_mat_charpoly()
   test_gen_mat_minpoly()
   test_gen_concat()

   println("")
end
