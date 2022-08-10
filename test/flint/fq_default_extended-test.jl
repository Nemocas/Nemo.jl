@testset "fq_default.constructors" begin
   R, a = NGFiniteField(fmpz(7), 5, "a")
   Rx, x = R["x"]
   f = x^2 + (2*a^4 + 5*a^3 + 5*a^2 + 3*a + 6)*x + a^4 + a^2 + 5*a + 5
   F, b = NGFiniteField(f, "b")
   @test defining_polynomial(F) == f

   @test F isa FqDefaultFiniteField
   @test base_field(F) === R

   Fy, y = F["y"]
   g = y^3 + 2*y + 1

   FF, bb = NGFiniteField(g, "bb")
   @test defining_polynomial(FF) == g

   @test FF isa FqDefaultFiniteField
   @test base_field(FF) === F

   a = F()

   @test isa(a, fq_default)

   b = F(4)
   c = F(fmpz(7))

   @test isa(b, fq_default)
   @test isa(c, fq_default)

   d = F(c)

   @test isa(d, fq_default)

   # check for irreducibility
   @test_throws ErrorException NGFiniteField(x^2-1, "z")
end

@testset "fq_default.printing" begin
   R, a = NGFiniteField(fmpz(7), 5, "a")
   Rx, x = R["x"]
   f = x^2 + (2*a^4 + 5*a^3 + 5*a^2 + 3*a + 6)*x + a^4 + a^2 + 5*a + 5
   F, b = NGFiniteField(f, "b")
   c = 2 * b + F(a)
   @test sprint(show, "text/plain", c) == "2*b + a"
end

@testset "fq_default.manipulation" begin
   R, a = NGFiniteField(fmpz(7), 5, "a")
   Rx, x = R["x"]
   f = x^2 + (2*a^4 + 5*a^3 + 5*a^2 + 3*a + 6)*x + a^4 + a^2 + 5*a + 5
   F, b = NGFiniteField(f, "b")

   @test iszero(zero(F))
   @test isone(one(F))
   @test is_gen(gen(F))
   @test characteristic(F) == 7
   @test order(F) == fmpz(7)^10
   @test degree(F) == 2
   @test absolute_degree(F) == 10
   @test is_unit(b + 1)
   @test deepcopy(b + 1) == b + 1
   @test coeff(2b + 1, 1) == 2
   @test_throws DomainError coeff(2b + 1, -1)
end

@testset "fq_default.special_functions" begin
   R, a = NGFiniteField(fmpz(7), 5, "a")

   @test degree(minpoly(a)) == degree(R)
   @test degree(defining_polynomial(R)) == degree(R)
   @test degree(absolute_charpoly(a)) == degree(R)
   @test !iszero(absolute_norm(a))
   @test_throws ErrorException NGFiniteField(fmpz(11), 2, "a")[1](a)

   Rx, x = R["x"]
   f = x^2 + (2*a^4 + 5*a^3 + 5*a^2 + 3*a + 6)*x + a^4 + a^2 + 5*a + 5
   F, b = NGFiniteField(f, "b")

   @test (@inferred tr(b)) == 5*a^4 + 2*a^3 + 2*a^2 + 4*a + 1
   @test (@inferred tr(tr(b))) == 6
   @test (@inferred absolute_tr(b)) == 6
   @test (@inferred norm(b)) == a^4 + a^2 + 5*a + 5
   @test (@inferred norm(norm(b))) == 6
   @test (@inferred absolute_tr(b)) == 6
   @test (@inferred frobenius(b)) == b^(7^5)
   @test (@inferred frobenius(b, 3)) == b^(7^(5*3))
   @test (@inferred absolute_frobenius(b)) == b^7
   @test (@inferred pth_root(b)) == F(6*a^4 + 5*a^3 + 3*a^2 + 4*a + 5)*b + F(a^4 + 4*a^3 + 4*a^2 + 3*a) + 2
   @test is_square(b^2)
   @test sqrt(b^2)^2 == b^2
   @test is_square_with_sqrt(b^2)[1]
   @test is_square_with_sqrt(b^2)[2]^2 == b^2
end

# TODO: Iteration is broken because AA uses degree instead of absolute_degree
#@testset "fq_default.iteration" begin
#   for n = [2, 3, 5, 13, 31]
#      R, _ = NGFiniteField(fmpz(n), 1, "x")
#      elts = Nemo.AbstractAlgebra.test_iterate(R)
#      @test elts == R.(0:n-1)
#      R, _ = NGFiniteField(fmpz(n), rand(2:9), "x")
#      Nemo.AbstractAlgebra.test_iterate(R)
#   end
#end
