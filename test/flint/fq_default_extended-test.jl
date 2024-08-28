@testset "FqFieldElem.constructors" begin
  R, a = finite_field(ZZRingElem(7), 5, "a")
  Rx, x = R["x"]
  f = x^2 + (2*a^4 + 5*a^3 + 5*a^2 + 3*a + 6)*x + a^4 + a^2 + 5*a + 5
  F, b = finite_field(f, "b")
  @test defining_polynomial(F) == f

  @test F(x) == b
  @test_throws ErrorException F(GF(5)["x"][2])

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

  FF, c = finite_field(g, "c")
  @test defining_polynomial(FF) == g

  @test FF isa FqField
  @test base_field(FF) === F

  @test sprint(show, "text/plain", R) isa String
  @test sprint(show, "text/plain", F) isa String
  @test sprint(show, "text/plain", FF) isa String
  @test PrettyPrinting.supercompact(FF) == "GF(7, 2*3)"

  # test LowercaseOff
  io = PrettyPrinting.pretty(IOBuffer())
  print(PrettyPrinting.terse(io), PrettyPrinting.Lowercase(), FF)
  @test String(take!(io)) == "GF(7, 2*3)"

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
  @test_throws ErrorException finite_field(x^2-1, "z")

  F, = finite_field(9)
  @test order(F) == 9
  @test finite_field(9)[1] === finite_field(9)[1]
  @test finite_field(9)[1] !== finite_field(9, cached = false)[1]
  @test_throws ErrorException finite_field(6)
  @test GF(2, 1) === GF(2)
  @test GF(6, 1, check = false) isa FqField
  @test finite_field(2, 1)[1] isa FqField

  # check that the check is correct
  R, a = finite_field(3, 1, "a")
  Rx, x = R["x"]
  f = x^2 + 1
  F, b = finite_field(f, "b")
  @test F isa FqField

  # check caching
  R, a = finite_field(3, 1, "a")
  Rx, x = R["x"]
  f = x^2 + 2x + 2
  F, b = finite_field(f, "b", cached = false)
  FF, bb = finite_field(f, "b", cached = false)
  @test F !== FF
  F, b = finite_field(f, "b")
  FF, bb = finite_field(f, "b")
  @test F === FF

  R, a = finite_field(3, 2, "a")
  Rx, x = R["x"]
  f = x^3 + 2x + 1
  F, b = finite_field(f, "b", cached = false)
  @test_throws ErrorException lift(ZZ["x"][1], b)
  @test F(ZZ["x"][2] + 1) == b + 1
  c = 2b^2 + 2b + 1
  @test lift(Rx, c) == 2x^2 + 2x + 1

  R, a = finite_field(ZZ(1180591620717411303449), 2, "a")
  Rx, x = R["x"]
  f = x^3 + 8*x^2 + 3*x + 5
  F, b = finite_field(f, "b", cached = false)
  @test_throws ErrorException lift(ZZ["x"][1], b)
  @test F(ZZ["x"][2] + 1) == b + 1
  c = 2b^2 + 2b + 1
  @test lift(Rx, c) == 2x^2 + 2x + 1
end

@testset "FqFieldElem.printing" begin
  R, a = finite_field(ZZRingElem(7), 5, "a")
  Rx, x = R["x"]
  f = x^2 + (2*a^4 + 5*a^3 + 5*a^2 + 3*a + 6)*x + a^4 + a^2 + 5*a + 5
  F, b = finite_field(f, "b")
  c = 2 * b + F(a)
  @test sprint(show, "text/plain", c) == "2*b + a"

  @test sprint(Nemo.show_raw, c) isa String
end

@testset "FqFieldElem.manipulation" begin
  R, a = finite_field(ZZRingElem(7), 5, "a")
  Rx, x = R["x"]
  f = x^2 + (2*a^4 + 5*a^3 + 5*a^2 + 3*a + 6)*x + a^4 + a^2 + 5*a + 5
  F, b = finite_field(f, "b")

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

  RR, aa = finite_field(ZZRingElem(7), 2, "a")
  @test iszero(tr(zero(R)) + tr(zero(R)))
  @test isone(norm(one(R)) * norm(one(R)))
  @test prime_field(R) === prime_field(RR)
end

@testset "FqFieldElem.special_functions" begin
  R, a = finite_field(ZZRingElem(7), 5, "a")

  @test degree(minpoly(a)) == degree(R)
  @test degree(defining_polynomial(R)) == degree(R)
  @test degree(absolute_charpoly(a)) == degree(R)
  @test !iszero(absolute_norm(a))
  @test_throws ErrorException finite_field(ZZRingElem(11), 2, "a")[1](a)

  Rx, x = R["x"]
  f = x^2 + (2*a^4 + 5*a^3 + 5*a^2 + 3*a + 6)*x + a^4 + a^2 + 5*a + 5
  F, b = finite_field(f, "b")

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
  R, a = finite_field(ZZRingElem(2), 2, "a")
  Rx, x = R["x"]
  f = x^3 + x + 1
  F, _ = finite_field(f, "b")
  AbstractAlgebra.test_iterate(F)
  @test length(collect(F)) == order(F)
end

@testset "FqFieldElem._residue_field" begin
  R, a = finite_field(ZZRingElem(2), 2, "a")
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

  R, a = finite_field(ZZRingElem(7), 1, "a")
  Rx, x = R["x"]
  f = x + 2
  F, RxtoF = Nemo._residue_field(f)
  @test defining_polynomial(Rx, F) == x + 2

  R, a = finite_field(ZZRingElem(7), 1, "a")
  Rx, x = R["x"]
  f = x + 2
  F, RxtoF = Nemo._residue_field(f)
  @test defining_polynomial(Rx, F) == x + 2

  R, a = finite_field(ZZRingElem(18446744073709551629), 1, "a")
  Rx, x = R["x"]
  f = x + 2
  F, RxtoF = Nemo._residue_field(f)
  @test defining_polynomial(Rx, F) == x + 2
end

@testset "Conversions" begin
  F = GF(2)
  Fx, x = F["x"]
  FF, = finite_field(x)
  @test iszero(FF(x))

  F = GF(ZZ(1180591620717411303449))
  Fx, x = F["x"]
  FF, = finite_field(x)
  @test iszero(FF(x))
end

@testset "Implicit promotions" begin
  F = GF(2, 2)
  FF = GF(2, 3)
  fl, = Nemo._try_promote(F, one(FF))
  @test !fl
  fl, = Nemo._try_promote(FF, one(F))
  @test !fl
  fl, = Nemo._try_promote(one(F), one(FF))
  @test !fl

  Fx, x = F["x"]
  f = x^2 + gen(F)*x + 1
  FF, = finite_field(f)
  for i in 1:10
    a, b = rand(F), rand(FF)
    @test a * b == FF(a) * b
    @test b * a == FF(a) * b
    @test a + b == FF(a) + b
    @test b + a == FF(a) + b
    @test a - b == FF(a) - b
    @test b - a == -(FF(a) - b)
    @test divides(a, b) == divides(FF(a), b)
    @test divides(b, a) == divides(b, FF(a))
    if !iszero(a)
      @test divexact(b, a) == divexact(b, FF(a))
    end
    @test divexact(b, one(F)) == divexact(b, one(FF))
    if !iszero(b)
      @test divexact(a, b) == divexact(FF(a), b)
    end
    @test divexact(a, one(FF)) == divexact(FF(a), one(FF))
  end

  F = GF(3)
  Fx, x = F["x"]
  FF, = Nemo._residue_field(x - 2)
  a = one(F) * one(FF)
  @test parent(a) === FF && is_one(a)
  a = one(FF) * one(F)
  @test parent(a) === FF && is_one(a)

  # some issue with recursion
  L = GF(5);
  Lx, x = L["x"];
  LL, = finite_field(x - 2);
  LLx, x = LL["x"];
  LLL, = finite_field(x - 2);
  @test one(LLL) + one(prime_field(LL)) == 2
end
