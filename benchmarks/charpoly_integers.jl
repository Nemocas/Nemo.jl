function benchmark_charpoly_int()
   print("benchmark_charpoly_int ... ")
   M = matrix_space(FlintZZ, 80, 80)()

   for i in 1:80
     for j in 1:80
       M[i, j] = rand(-20:20)
     end
   end

   tt = @elapsed charpoly(polynomial_ring(FlintZZ, "x")[1], M)
   println("$tt")
end
