@testset "dixon solver" begin
  A = matrix(ZZ, 4, 4, [1, 2, 7, 4, 15, 6, 7, 9, 19, 10, 11, 11, 53, 14, 15, 16])
  b = matrix(ZZ, 4, 1, [-1, -4, 5, 7])
  bb = matrix(ZZ, 4, 2, [-1, -2, -3, 5, 11, 2, 13, 17])
  for i = [1,5,10]
    s, d = Nemo.dixon_solve(A^i, b)
    @test A^i*s == d*b
    s, d = Nemo.dixon_solve(A^i, transpose(b); side = :left)
    @test s*A^i == d*transpose(b)

    s, d = Nemo.dixon_solve(A^i, bb)
    @test A^i*s == d*bb
    s, d = Nemo.dixon_solve(A^i, transpose(bb); side = :left)
    @test s*A^i == d*transpose(bb)
  end
end

@testset "DoublePlusOne" begin
  A = matrix(ZZ, 4, 4, [1, 2, 7, 4, 15, 6, 7, 9, 19, 10, 11, 11, 53, 14, 15, 16])
  b = matrix(ZZ, 1, 4, [-1, -4, 5, 7])
  bb = matrix(ZZ, 2, 4, [-1, -2, -3, 5, 11, 2, 13, 17])
  for i = [1,5,10]
    s, d = Nemo.UniCertSolve(A^i, b)
    @test s*A^i == d*b

    s, d = Nemo.UniCertSolve(A^i, bb)
    @test s*A^i == d*bb
  end
end

