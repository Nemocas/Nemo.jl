function benchmark_pearce()
   print("benchmark_pearce ... ")
   R, x = polynomial_ring(ZZ, "x")
   S, y = polynomial_ring(R, "y")
   T, z = polynomial_ring(S, "z")
   U, t = polynomial_ring(T, "t")
   V, u = polynomial_ring(U, "u")

   f = (x + y + 2z^2 + 3t^3 + 5u^5 + 1)^10
   g = (u + t + 2z^2 + 3y^3 + 5x^5 + 1)^10
   
   tt = @elapsed f*g
   println("$tt")
end

