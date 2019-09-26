function istriu(A::Generic.Mat)
   m = nrows(A)
   n = ncols(A)
   d = 0
   for c = 1:n
      for r = m:-1:1
         if !iszero(A[r,c])
            if r < d
               return false
            end
            d = r
            break
         end
      end
   end
   return true
end

function is_snf(A::Generic.Mat)
   m = nrows(A)
   n = ncols(A)
   a = A[1,1]
   for i = 2:min(m,n)
      q, r = divrem(A[i,i], a)
      if !iszero(r)
         return false
      end
      a = A[i,i]
   end
   for i = 1:n
      for j = 1:m
         if i == j
            continue
         end
         if !iszero(A[j,i])
            return false
         end
      end
   end
   return true
end

@testset "Matrix.binary_ops_delayed_reduction..." begin
   R, t = PolynomialRing(QQ, "t")
   K, a = NumberField(t^3 + 3t + 1, "a")
   S = MatrixSpace(K, 5, 5)

   for iter = 1:10
      f = rand(S, -10:10)
      g = rand(S, -10:10)
      h = rand(S, -10:10)

      @test f*(g + h) == f*g + f*h
      @test f*(g - h) == f*g - f*h
   end
end

@testset "Matrix.lu_delayed_reduction..." begin
   R, t = PolynomialRing(QQ, "t")
   K, a = NumberField(t^3 + 3t + 1, "a")
   S = MatrixSpace(K, 5, 5)

   for iter = 1:10
      rk = rand(0:5)
      A = randmat_with_rank(S, rk, -10:10)

      r, P, L, U = lu(A)

      @test r == rk
      @test P*A == L*U
   end
end

@testset "Matrix.fflu_delayed_reduction..." begin
   R, t = PolynomialRing(QQ, "t")
   K, a = NumberField(t^3 + 3t + 1, "a")
   S = MatrixSpace(K, 5, 5)

   for iter = 1:20
      rk = rand(0:5)
      A = randmat_with_rank(S, rk, -10:10)

      r, d, P, L, U = fflu(A)

      if r == 5
         D = S()
         D[1, 1] = inv(U[1, 1])
         D[2, 2] = inv(U[1, 1]*U[2, 2])
         D[3, 3] = inv(U[2, 2]*U[3, 3])
         D[4, 4] = inv(U[3, 3]*U[4, 4])
         D[5, 5] = inv(U[4, 4])
      end

      @test r == rk && (r < 5 || P*A == L*D*U)
   end
end

@testset "Matrix.minpoly_delayed_reduction..." begin
   # Tests reduce_row!

   R, t = PolynomialRing(QQ, "t")
   K, a = NumberField(t^3 + 3t + 1, "a")
   S = MatrixSpace(K, 6, 6)
   U, z = PolynomialRing(K, "z")

   M = S()
   for i = 1:3
      for j = 1:3
         M[i, j] = rand(K, -10:10)
         M[i + 3, j + 3] = deepcopy(M[i, j])
      end
   end

   p1 = minpoly(U, M)

   for i = 1:10
      similarity!(M, rand(1:6), rand(K, -3:3))
   end

   p2 = minpoly(U, M)

   @test p1 == p2
end

@testset "Matrix.solve_fflu_delayed_reduction..." begin
   R, t = PolynomialRing(QQ, "t")
   K, a = NumberField(t^3 + 3t + 1, "a")

   for dim = 0:5
      S = MatrixSpace(K, dim, dim)
      T = MatrixSpace(K, dim, rand(1:5))

      M = randmat_with_rank(S, dim, -100:100)
      b = rand(T, -100:100)

      x, d = Generic.solve_fflu(M, b)

      @test divexact(M, d)*x == b
   end
end

@testset "Matrix.solve_lu_delayed_reduction..." begin
   R, t = PolynomialRing(QQ, "t")
   K, a = NumberField(t^3 + 3t + 1, "a")

   for dim = 0:5
      S = MatrixSpace(K, dim, dim)
      T = MatrixSpace(K, dim, rand(1:5))

      M = randmat_with_rank(S, dim, -100:100)
      b = rand(T, -100:100)

      x = Generic.solve_lu(M, b)

      @test M*x == b
   end
end

@testset "Matrix.solve_triu_delayed_reduction..." begin
   R, x = PolynomialRing(QQ, "x")
   K, a = NumberField(x^3 + 3x + 1, "a")

   for dim = 0:10
      S = MatrixSpace(K, dim, dim)
      U = MatrixSpace(K, dim, rand(1:5))

      M = randmat_triu(S, -100:100)
      b = rand(U, -100:100)

      x = solve_triu(M, b, false)

      @test M*x == b
   end
end

@testset "Matrix.charpoly_delayed_reduction..." begin
   R, x = PolynomialRing(QQ, "x")
   K, a = NumberField(x^3 + 3x + 1, "a")

   for dim = 0:5
      S = MatrixSpace(K, dim, dim)
      U, x = PolynomialRing(K, "x")

      for i = 1:10
         M = rand(S, -5:5)

         p1 = charpoly_danilevsky_ff!(U, deepcopy(M))
         p2 = charpoly_danilevsky!(U, deepcopy(M))
         p3 = charpoly(U, M)

         @test p1 == p2
         @test p1 == p3
      end
   end
end

@testset "Matrix.hnf_delayed_reduction..." begin
   R, x = PolynomialRing(QQ, "x")
   K, a = NumberField(x^3 + 3x + 1, "a")
   S = MatrixSpace(K, 6, 6)

   for iter = 1:10
      A = rand(S, -10:10)

      H, U = hnf_cohen_with_transform(A)

      @test istriu(H)
      @test isunit(det(U))
      @test U*A == H
   end

   for iter = 1:10
      A = rand(S, -10:10)

      H, U = hnf_minors_with_transform(A)

      @test istriu(H)
      @test isunit(det(U))
      @test U*A == H
   end

   for iter = 1:10
      A = rand(S, -10:10)

      H, U = hnf_kb_with_transform(A)

      @test istriu(H)
      @test isunit(det(U))
      @test U*A == H
   end
end

@testset "Matrix.snf_delayed_reduction..." begin
   R, x = PolynomialRing(QQ, "x")
   K, a = NumberField(x^3 + 3x + 1, "a")
   S = MatrixSpace(K, 6, 6)

   for iter = 1:10
      A = rand(S, -10:10)

      T, U, K = Generic.snf_kb_with_transform(A)

      @test is_snf(T)
      @test isunit(det(U))
      @test isunit(det(K))
      @test U*A*K == T
   end
end

#=
   TODO: Add tests for the following when there are rings that are not fields
         that have delayed reduction
     - fflu over a ring
     - rref over a ring
     - minpoly over an integrally closed domain

   Note: backsolve! is also not tested as it appears to be unused.
=#
