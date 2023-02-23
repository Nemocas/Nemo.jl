function benchmark_det_poly_ring()
   print("benchmark_det_poly_ring ... ")
   ZZx, x = polynomial_ring(FlintZZ, "x")
   M = matrix_space(ZZx, 40, 40)()

   for i in 1:40
      for j in 1:40
         M[i, j] = ZZx(map(ZZRingElem, rand(-20:20, 3)))
      end
   end

   tt = @elapsed det(M)
   println("$tt")
end
