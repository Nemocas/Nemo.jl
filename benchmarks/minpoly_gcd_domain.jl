function benchmark_minpoly_gcd_domain()
   print("benchmark_minpoly_gcd_domain ... ")
   ZZx, x = polynomial_ring(ZZ, "x")

   M = matrix_space(ZZx, 20, 20)()

   for i in 1:10
      for j in 1:10
         r = ZZx(map(ZZRingElem, rand(-20:20, 3)))
         M[i, j] = r
         M[10 + i, 10 + j] = deepcopy(r)
      end
   end

   for i in 1:10
      similarity!(M, rand(1:20), ZZx(rand(-3:3)))
   end

   tt = @elapsed minpoly(polynomial_ring(ZZx, "y")[1], M)
   println("$tt")
end
