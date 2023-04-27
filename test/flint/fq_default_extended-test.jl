@testset "FqFieldElem.constructors" begin
   R, a = NGFiniteField(ZZRingElem(7), 5, "a")
   Rx, x = R["x"]
   f = x^2 + (2*a^4 + 5*a^3 + 5*a^2 + 3*a + 6)*x + a^4 + a^2 + 5*a + 5
   F, b = NGFiniteField(f, "b")
   @test defining_polynomial(F) == f

   @test F(x) == b
   @test_throws ErrorException F(Nemo._GF(5)["x"][2])

   ff = defining_polynomial(Rx, F)
   @test ff == f

   Rt, t = R["t"]
   ff = defining_polynomial(Rt, F)
   @test parent(ff) === Rt

   Rt, t = F["t"]
   @test_throws ErrorException defining_polynomial(Rt, F)


   @test F isa FqField
   @test base_field(F) === R

   Fy, y = F["y"]
   g = y^3 + 2*y + 1

   FF, c = NGFiniteField(g, "c")
   @test defining_polynomial(FF) == g

   @test FF isa FqField
   @test base_field(FF) === F

   @test sprint(show, "text/plain", R) isa String
   @test sprint(show, "text/plain", F) isa String
   @test sprint(show, "text/plain", FF) isa String

   a = F()

   @test isa(a, FqFieldElem)

   b = F(4)
   c = F(ZZRingElem(7))

   @test isa(b, FqFieldElem)
   @test isa(c, FqFieldElem)

   d = F(c)

   @test isa(d, FqFieldElem)

   @test FF(one(R)) == one(FF)

   # check for irreducibility
   @test_throws ErrorException NGFiniteField(x^2-1, "z")

   F, = NGFiniteField(9)
   @test order(F) == 9
   @test NGFiniteField(9)[1] === NGFiniteField(9)[1]
   @test NGFiniteField(9)[1] !== NGFiniteField(9, cached = false)[1]
   @test_throws ErrorException NGFiniteField(6)
   @test Nemo._GF(2, 1) === Nemo._GF(2)
   @test Nemo._GF(6, 1, check = false) isa FqField
   @test Nemo._FiniteField(2, 1)[1] isa FqField

   # check that the check is correct
   R, a = NGFiniteField(3, 1, "a")
   Rx, x = R["x"]
   f = x^2 + 1
   F, b = NGFiniteField(f, "b")
   @test F isa FqField

   # check caching
   R, a = NGFiniteField(3, 1, "a")
   Rx, x = R["x"]
   f = x^2 + 2x + 2
   F, b = NGFiniteField(f, "b", cached = false)
   FF, bb = NGFiniteField(f, "b", cached = false)
   @test F !== FF
   F, b = NGFiniteField(f, "b")
   FF, bb = NGFiniteField(f, "b")
   @test F === FF
end

@testset "FqFieldElem.printing" begin
   R, a = NGFiniteField(ZZRingElem(7), 5, "a")
   Rx, x = R["x"]
   f = x^2 + (2*a^4 + 5*a^3 + 5*a^2 + 3*a + 6)*x + a^4 + a^2 + 5*a + 5
   F, b = NGFiniteField(f, "b")
   c = 2 * b + F(a)
   @test sprint(show, "text/plain", c) == "2*b + a"

   @test sprint(Nemo.show_raw, c) isa String
end

@testset "FqFieldElem.manipulation" begin
   R, a = NGFiniteField(ZZRingElem(7), 5, "a")
   Rx, x = R["x"]
   f = x^2 + (2*a^4 + 5*a^3 + 5*a^2 + 3*a + 6)*x + a^4 + a^2 + 5*a + 5
   F, b = NGFiniteField(f, "b")

   @test iszero(zero(F))
   @test isone(one(F))
   @test is_gen(gen(F))
   @test characteristic(F) == 7
   @test order(F) == ZZRingElem(7)^10
   @test degree(F) == 2
   @test absolute_degree(F) == 10
   @test is_unit(b + 1)
   @test deepcopy(b + 1) == b + 1
   @test (@inferred coeff(2b + 1, 1)) == 2
   @test_throws DomainError coeff(2b + 1, -1)

   u = a
   v = a^2
   w = R()
   @test coeff(w, 3) == 0
   @test coeff(w, 3) == 0
   mul!(w, u, v)
   @test coeff(w, 3) == 1
   add!(w, u, v)
   @test coeff(w, 1) == 1
   zero!(w)
   @test coeff(w, 1) == 0
   addeq!(w, u)
   @test coeff(w, 1) == 1

   @test basis(R) == [a^i for i in 0:4]
   @test basis(F) == [b^0, b^1]

   RR, aa = NGFiniteField(ZZRingElem(7), 2, "a")
   @test iszero(tr(zero(R)) + tr(zero(R)))
   @test isone(norm(one(R)) * norm(one(R)))
   @test prime_field(R) === prime_field(RR)
end

@testset "FqFieldElem.special_functions" begin
   R, a = NGFiniteField(ZZRingElem(7), 5, "a")

   @test degree(minpoly(a)) == degree(R)
   @test degree(defining_polynomial(R)) == degree(R)
   @test degree(absolute_charpoly(a)) == degree(R)
   @test !iszero(absolute_norm(a))
   @test_throws ErrorException NGFiniteField(ZZRingElem(11), 2, "a")[1](a)

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

@testset "FqFieldElem.iteration" begin
   R, a = NGFiniteField(ZZRingElem(2), 2, "a")
   Rx, x = R["x"]
   f = x^3 + x + 1
   F, _ = NGFiniteField(f, "b")
   AbstractAlgebra.test_iterate(F)
   @test length(collect(F)) == order(F)
end

@testset "FqFieldElem._residue_field" begin
   R, a = NGFiniteField(ZZRingElem(2), 2, "a")
   Rx, x = R["x"]
   f = x^3 + x + 1
   F, RxtoF = Nemo._residue_field(f)
   @test (@inferred domain(RxtoF)) === Rx
   @test (@inferred codomain(RxtoF)) === F
   @test (@inferred degree(F)) == 3
   @test (@inferred base_field(F)) === R
   @test @inferred isone(RxtoF(one(Rx)))
   @test @inferred iszero(RxtoF(zero(Rx)))
   for i in 1:10
      a = rand(Rx, 1:10)
      b = rand(Rx, 1:10)
      @test RxtoF(a * b) == RxtoF(a) * RxtoF(b)
      @test RxtoF(a + b) == RxtoF(a) + RxtoF(b)
      c = rand(F)
      @test RxtoF(preimage(RxtoF, c)) == c
   end

   F, RxtoF = Nemo._residue_field(f, absolute = true)
   @test degree(F) == 6
   @test base_field(F) === R
   @test isone(RxtoF(one(Rx)))
   @test iszero(RxtoF(zero(Rx)))
   for i in 1:10
      a = rand(Rx, 1:10)
      b = rand(Rx, 1:10)
      @test RxtoF(a * b) == RxtoF(a) * RxtoF(b)
      @test RxtoF(a + b) == RxtoF(a) + RxtoF(b)
      c = rand(F)
      @test RxtoF(preimage(RxtoF, c)) == c
   end

   R, a = NGFiniteField(ZZRingElem(7), 1, "a")
   Rx, x = R["x"]
   f = x + 2
   F, RxtoF = Nemo._residue_field(f)
   @test defining_polynomial(Rx, F) == x + 2

   R, a = NGFiniteField(ZZRingElem(7), 1, "a")
   Rx, x = R["x"]
   f = x + 2
   F, RxtoF = Nemo._residue_field(f)
   @test defining_polynomial(Rx, F) == x + 2

   R, a = NGFiniteField(ZZRingElem(18446744073709551629), 1, "a")
   Rx, x = R["x"]
   f = x + 2
   F, RxtoF = Nemo._residue_field(f)
   @test defining_polynomial(Rx, F) == x + 2
end
