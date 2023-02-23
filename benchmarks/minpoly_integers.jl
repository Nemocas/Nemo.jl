function benchmark_minpoly_integers()
   print("benchmark_minpoly_integers ... ")
   M = matrix_space(FlintZZ, 80, 80)()
   
   for i in 1:40
      for j in 1:40
         r = rand(-20:20)
         M[i, j] = r
         M[40 + i, 40 + j] = deepcopy(r)
      end
   end

   for i in 1:10
      similarity!(M, rand(1:80), ZZRingElem(rand(-3:3)))
   end

   tt = @elapsed minpoly(polynomial_ring(FlintZZ, "x")[1], M)
   println("$tt")
end

