@testset "Matrix.binary_ops_delayed_reduction" begin
   R, t = polynomial_ring(QQ, "t")
   K, a = number_field(t^3 + 3t + 1, "a")
   S = matrix_space(K, 5, 5)

   for iter = 1:10
      f = rand(S, -10:10)
      g = rand(S, -10:10)
      h = rand(S, -10:10)

      @test f*(g + h) == f*g + f*h
      @test f*(g - h) == f*g - f*h
   end
end

@testset "Matrix.lu_delayed_reduction" begin
   R, t = polynomial_ring(QQ, "t")
   K, a = number_field(t^3 + 3t + 1, "a")
   S = matrix_space(K, 5, 5)

   for iter = 1:10
      rk = rand(0:5)
      A = randmat_with_rank(S, rk, -10:10)

      r, P, L, U = lu(A)

      @test r == rk
      @test P*A == L*U
   end
end

@testset "Matrix.fflu_delayed_reduction" begin
   R, t = polynomial_ring(QQ, "t")
   K, a = number_field(t^3 + 3t + 1, "a")
   S = matrix_space(K, 5, 5)

   for iter = 1:20
      rk = rand(0:5)
      A = randmat_with_rank(S, rk, -10:10)

      r, d, P, L, U = fflu(A)

      if r == 5
         D = S()
         D[1, 1] = inv(L[1, 1])
         D[2, 2] = inv(L[1, 1]*L[2, 2])
         D[3, 3] = inv(L[2, 2]*L[3, 3])
         D[4, 4] = inv(L[3, 3]*L[4, 4])
         D[5, 5] = inv(L[4, 4]*L[5, 5])
      end

      @test r == rk && (r < 5 || P*A == L*D*U)
   end
end

@testset "Matrix.minpoly_delayed_reduction" begin
   # Tests reduce_row!

   R, t = polynomial_ring(QQ, "t")
   K, a = number_field(t^3 + 3t + 1, "a")
   S = matrix_space(K, 6, 6)
   U, z = polynomial_ring(K, "z")

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

@testset "Matrix.solve_fflu_delayed_reduction" begin
   R, t = polynomial_ring(QQ, "t")
   K, a = number_field(t^3 + 3t + 1, "a")

   for dim = 0:5
      S = matrix_space(K, dim, dim)
      T = matrix_space(K, dim, rand(1:5))

      M = randmat_with_rank(S, dim, -100:100)
      b = rand(T, -100:100)

      if isdefined(Generic, :can_solve_with_solution_fflu) 
         flag, x, d = Generic.can_solve_with_solution_fflu(M, b)
         @test flag
      else
         x, d = Generic.solve_fflu(M, b)
      end       

      @test divexact(M, d)*x == b
   end
end

@testset "Matrix.solve_lu_delayed_reduction" begin
   R, t = polynomial_ring(QQ, "t")
   K, a = number_field(t^3 + 3t + 1, "a")

   for dim = 0:5
      S = matrix_space(K, dim, dim)
      T = matrix_space(K, dim, rand(1:5))

      M = randmat_with_rank(S, dim, -100:100)
      b = rand(T, -100:100)

      if isdefined(Generic, :can_solve_with_solution_lu)
         flag, x = Generic.can_solve_with_solution_lu(M, b)
         @test flag
      else
         x = Generic.solve_lu(M, b)
      end     

      @test M*x == b
   end
end

@testset "Matrix.solve_triu_delayed_reduction" begin
   R, x = polynomial_ring(QQ, "x")
   K, a = number_field(x^3 + 3x + 1, "a")

   for dim = 0:10
      S = matrix_space(K, dim, dim)
      U = matrix_space(K, dim, rand(1:5))

      M = randmat_triu(S, -100:100)
      b = rand(U, -100:100)

      x = solve_triu(M, b, false)

      @test M*x == b
   end
end

@testset "Matrix.charpoly_delayed_reduction" begin
   R, x = polynomial_ring(QQ, "x")
   K, a = number_field(x^3 + 3x + 1, "a")

   for dim = 0:5
      S = matrix_space(K, dim, dim)
      U, x = polynomial_ring(K, "x")

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

@testset "Matrix.hnf_delayed_reduction" begin
   R, x = polynomial_ring(QQ, "x")
   K, a = number_field(x^3 + 3x + 1, "a")
   S = matrix_space(K, 6, 6)

   for iter = 1:10
      A = rand(S, -10:10)

      H, U = hnf_cohen_with_transform(A)

      @test is_upper_triangular(H)
      @test is_unit(det(U))
      @test U*A == H
   end

   for iter = 1:10
      A = rand(S, -10:10)

      H, U = hnf_minors_with_transform(A)

      @test is_upper_triangular(H)
      @test is_unit(det(U))
      @test U*A == H
   end

   for iter = 1:10
      A = rand(S, -10:10)

      H, U = hnf_kb_with_transform(A)

      @test is_upper_triangular(H)
      @test is_unit(det(U))
      @test U*A == H
   end
end

@testset "Matrix.snf_delayed_reduction" begin
   R, x = polynomial_ring(QQ, "x")
   K, a = number_field(x^3 + 3x + 1, "a")
   S = matrix_space(K, 6, 6)

   for iter = 1:10
      A = rand(S, -10:10)

      T, U, K = Generic.snf_kb_with_transform(A)

      @test is_snf(T)
      @test is_unit(det(U))
      @test is_unit(det(K))
      @test U*A*K == T
   end
end

@testset "Matrix.rounding" begin
    m = [ -1.2 -1.0 -0.7 -0.5 -0.3 0.0  ; 0.0 0.3 0.5 0.7 1.0 1.2 ]
    @test m isa Matrix{Float64}

    @test trunc(ZZMatrix, m) == ZZMatrix(map(x->trunc(Int,x), m))
    @test round(ZZMatrix, m) == ZZMatrix(map(x->round(Int,x), m))
    @test ceil(ZZMatrix, m) == ZZMatrix(map(x->ceil(Int,x), m))
    @test floor(ZZMatrix, m) == ZZMatrix(map(x->floor(Int,x), m))

    M = map(big, m)
    @test M isa Matrix{BigFloat}

    @test trunc(ZZMatrix, M) == ZZMatrix(map(x->trunc(Int,x), M))
    @test round(ZZMatrix, M) == ZZMatrix(map(x->round(Int,x), M))
    @test ceil(ZZMatrix, M) == ZZMatrix(map(x->ceil(Int,x), M))
    @test floor(ZZMatrix, M) == ZZMatrix(map(x->floor(Int,x), M))
end

@testset "Modular reduction of ZZMatrix" begin
  S0 = matrix_space(ZZ,2,2)
  M0 = S0(1)
  M0[1,2] = 99
  M0[2,1] = 199
  M0[2,2] = -499
  # First call the ZZModMatrix constructor directly: (3 cases)
  ignore = ZZModMatrix(UInt(81), M0)
  ignore = ZZModMatrix(Int(81), M0)
  ignore = ZZModMatrix(ZZ(81), M0)
  # Now check conversion via matrix_space: first use a modulus of type Int/UInt
  m = 81
  ZZmodm, = residue_ring(ZZ,m)
  S81 = matrix_space(ZZmodm,2,2)
  M81 = S81(M0)
  @test M81[1,1] == 1 && M81[1,2] == 18 && M81[2,1] == 37 && M81[2,2] == -13;
  # Now use matrix_space with modulus of type ZZRingElem (see GitHub issue #2947)
  ZZmodM, = residue_ring(ZZ,ZZ(m))
  SZZ81 = matrix_space(ZZmodM,2,2)
  MZZ81 = SZZ81(M0)
  @test MZZ81[1,1] == 1 && MZZ81[1,2] == 18 && MZZ81[2,1] == 37 && MZZ81[2,2] == -13;
end

#=
   TODO: Add tests for the following when there are rings that are not fields
         that have delayed reduction
     - fflu over a ring
     - rref over a ring
     - minpoly over an integrally closed domain

   Note: backsolve! is also not tested as it appears to be unused.
=#
