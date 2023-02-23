function benchmark_fateman()
   print("benchmark_fateman ... ")
   R, x = polynomial_ring(FlintZZ, "x")
   S, y = polynomial_ring(R, "y")
   T, z = polynomial_ring(S, "z")
   U, t = polynomial_ring(T, "t")

   p = (x + y + z + t + 1)^20
   
   tt = @elapsed p*(p + 1)
   println("$tt")
end

