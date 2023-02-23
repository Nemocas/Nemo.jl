function benchmark_poly_nf_elem()
   print("benchmark_poly_nf_elem ... ")
   R, x = CyclotomicField(20, "x")
   S, y = polynomial_ring(R, "y")

   f = (3x^7 + x^4 - 3x + 1)*y^3 + (2x^6-x^5+4x^4-x^3+x^2-1)*y +(-3x^7+2x^6-x^5+3x^3-2x^2+x)

   tt = @elapsed f^300
   println("$tt")
end

