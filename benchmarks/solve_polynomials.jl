function benchmark_solve_poly()
   print("benchmark_solve_poly ... ")
   R, x = PolynomialRing(FlintZZ, "x")
   S, y = PolynomialRing(R, "y")
   M = MatrixSpace(S, 20, 20)()

   for i in 1:20
     for j in 1:20
       for k in 0:2
         M[i, j] = M[i,j] + y^k * (rand(-20:20) + rand(-20:20)*x + rand(-20:20)*x^2)
       end
     end
   end

   b = MatrixSpace(S, 20, 1)()

   for j in 1:20
     for k in 0:2
       b[j, 1] = b[j,1] + y^k * (rand(-20:20) + rand(-20:20)*x + rand(-20:20)*x^2)
     end
   end

   tt = @elapsed solve_rational(M, b)
   println("$tt")
end

