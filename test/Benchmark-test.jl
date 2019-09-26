@testset "Benchmark.fateman..." begin
   R, x = PolynomialRing(ZZ, "x")
   S, y = PolynomialRing(R, "y")
   T, z = PolynomialRing(S, "z")
   U, t = PolynomialRing(T, "t")

   p = (x + y + z + t + 1)^20

   q = p*(p + 1)

   @test length(q) == 41
end

@testset "Benchmark.pearce..." begin
   R, x = PolynomialRing(ZZ, "x")
   S, y = PolynomialRing(R, "y")
   T, z = PolynomialRing(S, "z")
   U, t = PolynomialRing(T, "t")
   V, u = PolynomialRing(U, "u")

   f = (x + y + 2z^2 + 3t^3 + 5u^5 + 1)^10
   g = (u + t + 2z^2 + 3y^3 + 5x^5 + 1)^10

   q = f*g

   @test length(q) == 61
end

@testset "Benchmark.resultant..." begin
   R, x = FlintFiniteField(7, 11, "x")
   S, y = PolynomialRing(R, "y")
   T = ResidueRing(S, y^3 + 3x*y + 1)
   U, z = PolynomialRing(T, "z")

   f = (3y^2 + y + x)*z^2 + ((x + 2)*y^2 + x + 1)*z + 4x*y + 3
   g = (7y^2 - y + 2x + 7)*z^2 + (3y^2 + 4x + 1)*z + (2x + 1)*y + 1

   s = f^12
   t = (s + g)^12

   r = resultant(s, t)

   @test r == (x^10+4*x^8+6*x^7+3*x^6+4*x^5+x^4+6*x^3+5*x^2+x)*y^2+(5*x^10+x^8+4*x^7+3*x^5+5*x^4+3*x^3+x^2+x+6)*y+(2*x^10+6*x^9+5*x^8+5*x^7+x^6+6*x^5+5*x^4+4*x^3+x+3)
end

@testset "Benchmark.poly_nf_elem..." begin
   R, x = CyclotomicField(20, "x")
   S, y = PolynomialRing(R, "y")

   f = (3x^7 + x^4 - 3x + 1)*y^3 + (2x^6-x^5+4x^4-x^3+x^2-1)*y +(-3x^7+2x^6-x^5+3x^3-2x^2+x)

   @test f^300 == f^100*f^200
end
