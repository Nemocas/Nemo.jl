function benchmark_resultant()
   print("benchmark_resultant ... ")
   R, x = finite_field(7, 11, "x")
   S, y = polynomial_ring(R, "y")
   T = residue_ring(S, y^3 + 3x*y + 1)
   U, z = polynomial_ring(T, "z")

   f = (3y^2 + y + x)*z^2 + ((x + 2)*y^2 + x + 1)*z + 4x*y + 3
   g = (7y^2 - y + 2x + 7)*z^2 + (3y^2 + 4x + 1)*z + (2x + 1)*y + 1

   s = f^12
   t = (s + g)^12
   
   tt = @elapsed resultant(s, t)
   println("$tt")
end

