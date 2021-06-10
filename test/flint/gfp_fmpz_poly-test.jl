@testset "gfp_fmpz_poly.constructors" begin
   R = GF(fmpz(123456789012345678949))
   
   S1 = PolyRing(R)
   S2 = PolyRing(R)

   @test isa(S1, GFPFmpzPolyRing)
   @test S1 !== S2

   S, x = PolynomialRing(R, "x")

   @test elem_type(S) == gfp_fmpz_poly
   @test elem_type(GFPFmpzPolyRing) == gfp_fmpz_poly
   @test parent_type(gfp_fmpz_poly) == GFPFmpzPolyRing
   @test dense_poly_type(Generic.ResF{fmpz}) == gfp_fmpz_poly

   @test typeof(S) <: GFPFmpzPolyRing

   @test isa(x, PolyElem{Nemo.gfp_fmpz_elem})

   f = x^3 + 2x^2 + x + 1

   @test isa(f, PolyElem)

   g = S(2)

   @test isa(g, PolyElem)

   h = S(x^2 + 2x + 1)

   @test isa(h, PolyElem)

   k = S([R(1), R(0), R(3)])

   @test isa(k, PolyElem)

   l = S()

   @test isa(l, PolyElem)

   m = S(fmpz(123))

   @test isa(m, PolyElem)

   n = S([fmpz(1), fmpz(0), fmpz(3)])

   @test isa(n, PolyElem)

   T, y = PolynomialRing(ZZ, "y")

   p = 3y^3 + 2y - 1
   q = S(p)

   @test isa(q, PolyElem)

   r = S([1, 2, 3])

   @test isa(r, PolyElem)
end

@testset "gfp_fmpz_poly.printing" begin
   R = GF(fmpz(123456789012345678949))
   S, x = PolynomialRing(R, "x")
   f = x^3 + 2x^2 + x + 1

   @test sprint(show, "text/plain", f) == "x^3 + 2*x^2 + x + 1"
end

@testset "gfp_fmpz_poly.manipulation" begin
   R = GF(fmpz(123456789012345678949))
   S, x = PolynomialRing(R, "x")

   @test iszero(zero(S))

   @test isone(one(S))

   @test isgen(gen(S))

   @test isunit(one(S))

   f = x^2 + 2x + 1

   @test leading_coefficient(f) == 1

   @test degree(f) == 2

   @test length(f) == 3

   @test coeff(f, 1) == 2

   @test_throws DomainError coeff(f, -1)

   @test canonical_unit(-x + 1) == -1

   @test deepcopy(f) == f

   setcoeff!(f, 1, UInt(2))

   @test coeff(f, 1) == 2

   setcoeff!(f, 1, 3)

   @test coeff(f, 1) == 3

   setcoeff!(f, 1, fmpz(2)^100)

   @test coeff(f, 1) == 32146634986640907030

   @test modulus(x) == 123456789012345678949

   @test modulus(R) == 123456789012345678949

   @test characteristic(S) == 123456789012345678949
end

@testset "gfp_fmpz_poly.polynomial" begin
   R = GF(ZZ(23))

   f = polynomial(R, [])
   g = polynomial(R, [1, 2, 3])
   h = polynomial(R, fmpz[1, 2, 3])
   k = polynomial(R, [R(1), R(2), R(3)])
   p = polynomial(R, [1, 2, 3], "y")

   @test isa(f, gfp_fmpz_poly)
   @test isa(g, gfp_fmpz_poly)
   @test isa(h, gfp_fmpz_poly)
   @test isa(k, gfp_fmpz_poly)
   @test isa(p, gfp_fmpz_poly)

   q = polynomial(R, [1, 2, 3], cached=false)

   @test parent(g) != parent(q)
end

@testset "gfp_fmpz_poly.similar" begin
   R = GF(ZZ(23))

   f = polynomial(R, [1, 2, 3])
   g = similar(f)
   h = similar(f, "y")

   @test isa(g, gfp_fmpz_poly)
   @test isa(h, gfp_fmpz_poly)

   q = similar(g, cached=false)

   @test parent(g) != parent(q)
end

@testset "gfp_fmpz_poly.binary_ops" begin
   R = GF(fmpz(123456789012345678949))
   S, x = PolynomialRing(R, "x")

   f = x^2 + 2x + 1
   g = x^3 + 3x + 2

   @test f + g == x^3+x^2+5*x+3

   @test f*g == x^5+2*x^4+4*x^3+8*x^2+7*x+2

   @test f - g == 123456789012345678948*x^3+x^2+123456789012345678948*x+123456789012345678948
end

@testset "gfp_fmpz_poly.adhoc_binary" begin
   R = GF(fmpz(123456789012345678949))
   S, x = PolynomialRing(R, "x")

   f = x^2 + 2x + 1
   g = x^3 + 3x + 2

   @test f*12 == 12*x^2+24*x+12

   @test 7*g == 7*x^3+21*x+14

   @test fmpz(3)*g == 3*x^3+9*x+6

   @test f*fmpz(2) == 2*x^2+4*x+2

   @test f + 12 == x^2+2*x+13

   @test f + fmpz(12) == x^2+2*x+13

   @test f - 12 == x^2+2*x+123456789012345678938

   @test f - fmpz(12) == x^2+2*x+123456789012345678938

   @test 12 + g == x^3+3*x+14

   @test fmpz(12) + g == x^3+3*x+14

   @test 12 - g == 123456789012345678948*x^3+123456789012345678946*x+10

   @test fmpz(12) - g == 123456789012345678948*x^3+123456789012345678946*x+10

   @test f + R(12) == x^2+2*x+13

   @test R(12) + g == x^3+3*x+14

   @test f - R(12) == x^2+2*x+123456789012345678938

   @test R(12) - g == 123456789012345678948*x^3+123456789012345678946*x+10

   @test R(7)*g == 7*x^3+21*x+14

   @test f*R(12) == 12*x^2+24*x+12
end

@testset "gfp_fmpz_poly.comparison" begin
   R = GF(fmpz(123456789012345678949))
   S, x = PolynomialRing(R, "x")

   f = x^2 + 2x + 1
   g = x^3 + 3x + 2

   @test f != g

   @test isequal(f, deepcopy(f))
end

@testset "gfp_fmpz_poly.adhoc_comparison" begin
   R = GF(fmpz(123456789012345678949))
   S, x = PolynomialRing(R, "x")

   f = x^2 + 2x + 1

   @test f != 1

   @test 1 != f

   @test S(7) == fmpz(7)

   @test fmpz(7) != f

   @test S(7) == R(7)

   @test R(7) != x + 1
end

@testset "gfp_fmpz_poly.unary_ops" begin
   R = GF(fmpz(123456789012345678949))
   S, x = PolynomialRing(R, "x")

   f = x^2 + 2x + 1

   @test -f == 123456789012345678948*x^2+123456789012345678947*x+123456789012345678948
end

@testset "gfp_fmpz_poly.truncation" begin
   R = GF(fmpz(123456789012345678949))
   S, x = PolynomialRing(R, "x")

   f = x^2 + 2x + 1
   g = x^3 + 3x + 1

   @test truncate(f, 2) == 2*x+1

   @test_throws DomainError truncate(f, -1)

   @test mullow(f, g, 3) == 7*x^2+5*x+1

   @test_throws DomainError mullow(f, g, -1)
end

@testset "gfp_fmpz_poly.reverse" begin
   R = GF(fmpz(123456789012345678949))
   S, x = PolynomialRing(R, "x")

   f = x^2 + 2x + 3

   @test reverse(f) == 3x^2 + 2x + 1
end

@testset "gfp_fmpz_poly.shift" begin
   R = GF(fmpz(123456789012345678949))
   S, x = PolynomialRing(R, "x")

   f = x^2 + 2x + 1

   @test shift_left(f, 3) == x^5 + 2x^4 + x^3

   @test_throws DomainError shift_left(f, -1)

   @test shift_right(f, 1) == x + 2

   @test_throws DomainError shift_right(f, -1)
end

@testset "gfp_fmpz_poly.powering" begin
   R = GF(fmpz(123456789012345678949))
   S, x = PolynomialRing(R, "x")

   f = x^2 + 2x + 1

   @test f^6 == x^12+12*x^11+66*x^10+220*x^9+495*x^8+792*x^7+924*x^6+792*x^5+495*x^4+220*x^3+66*x^2+12*x+1

   @test_throws DomainError f^-1
end

@testset "gfp_fmpz_poly.exact_division" begin
   R = GF(fmpz(123456789012345678949))
   S, x = PolynomialRing(R, "x")

   f = x^2 + 2x + 1
   g = x^3 + 3x + 1

   @test divexact(f*g, f) == g
end

@testset "gfp_fmpz_poly.adhoc_exact_division" begin
   R = GF(fmpz(123456789012345678949))
   S, x = PolynomialRing(R, "x")

   f = x^2 + 2x + 1

   @test divexact(3*f, fmpz(3)) == f

   @test divexact(3*f, 3) == f

   @test divexact(R(7)*f, R(7)) == f
end

@testset "gfp_fmpz_poly.modular_arithmetic" begin
   R = GF(fmpz(123456789012345678949))
   S, x = PolynomialRing(R, "x")

   f = 3*x^2 + x + 2
   g = 5*x^2 + 2*x + 1
   h = 3*x^3 + 2*x^2 + x + 7

   @test invmod(f, h) == 112883663504991137175*x^2+86761824016232857498*x+48511987621979662257

   @test mulmod(f, g, h) == 82304526008230452642*x^2+41152263004115226286*x+41152263004115226316

   @test powermod(f, 10, h) == 118470346535924950143*x^2+97790722831392543222*x+115967716915690326718

   @test powermod(f, fmpz(10), h) == 118470346535924950143*x^2+97790722831392543222*x+115967716915690326718

   @test powermod(f, -10, g) == 78305338116088931412*x+91239060941924718463

   @test powermod(f, -fmpz(10), g) == 78305338116088931412*x+91239060941924718463
end

@testset "gfp_fmpz_poly.euclidean_division" begin
   R = GF(fmpz(123456789012345678949))
   S, x = PolynomialRing(R, "x")

   f = x^2 + 2x + 1
   g = x^3 + 3x + 1

   @test mod(g, f) == 6*x+3

   @test divrem(g, f) == (x+123456789012345678947, 6*x+3)
end

@testset "gfp_fmpz_poly.gcd" begin
   R = GF(fmpz(123456789012345678949))
   S, x = PolynomialRing(R, "x")

   f = x^2 + 2x + 1
   g = x^3 + 3x + 1
   h = x^2 + 1

   @test gcd(f*h, g*h) == x^2+1

   @test gcdx(f*h, g*h) == (x^2+1, 41152263004115226317*x^2+41152263004115226316*x+2,82304526008230452632*x+123456789012345678948)
end

@testset "gfp_fmpz_poly.gcdinv" begin
   R = GF(fmpz(123456789012345678949))
   S, x = PolynomialRing(R, "x")

   f = x^2 + 2x + 1
   g = x^3 + 3x + 1

   @test gcdinv(f, g) == (1, 41152263004115226317*x^2+41152263004115226316*x+2)
end

@testset "gfp_fmpz_poly.evaluation" begin
   R = GF(fmpz(123456789012345678949))
   S, x = PolynomialRing(R, "x")

   f = x^2 + 2x + 1

   @test evaluate(f, 3) == 16

   @test evaluate(f, fmpz(10)) == 121

   @test evaluate(f, R(10)) == 121

if VERSION >= v"0.5.0-dev+3171"

   @test f(3) == 16

   @test f(fmpz(10)) == 121

   @test f(R(10)) == 121

end
end

@testset "gfp_fmpz_poly.composition" begin
   R = GF(fmpz(123456789012345678949))
   S, x = PolynomialRing(R, "x")

   f = x^2 + 2x + 1
   g = x^3 + 3x + 1

   @test compose(f, g) == x^6+6*x^4+4*x^3+9*x^2+12*x+4
end

@testset "gfp_fmpz_poly.derivative" begin
   R = GF(fmpz(123456789012345678949))
   S, x = PolynomialRing(R, "x")

   f = x^2 + 2x + 1

   @test derivative(f) == 2x + 2
end

@testset "gfp_fmpz_poly.integral" begin
   R = GF(fmpz(123456789012345678949))
   S, x = PolynomialRing(R, "x")

   f = x^2 + 2x + 1

   @test integral(f) == 82304526008230452633*x^3+x^2+x
end

@testset "gfp_fmpz_poly.resultant" begin
   R = GF(fmpz(123456789012345678949))
   S, x = PolynomialRing(R, "x")

   f = 5x^2 + 2x + 1
   g = 2x^3 + 3x + 1

   @test resultant(f, g) == 212
end

@testset "gfp_fmpz_poly.discriminant" begin
   R = GF(fmpz(123456789012345678949))
   S, x = PolynomialRing(R, "x")

   f = x^2 + 2x + 1

   @test discriminant(f) == 0
end

@testset "gfp_fmpz_poly.lift" begin
   R = GF(fmpz(123456789012345678949))
   S, x = PolynomialRing(R, "x")

   T, y = PolynomialRing(ZZ, "y")

   f = x^2 + 2x + 1

   @test lift(T, f) == y^2 + 2y + 1
end

@testset "gfp_fmpz_poly.isirreducible" begin
   R = GF(fmpz(123456789012345678949))
   S, x = PolynomialRing(R, "x")

   f = x^2 + 2x + 1

   @test isirreducible(f) == false
end

@testset "gfp_fmpz_poly.issquarefree" begin
   R = GF(fmpz(123456789012345678949))
   S, x = PolynomialRing(R, "x")

   f = x^2 + 2x + 1

   @test issquarefree(f) == false
end

@testset "gfp_fmpz_poly.factor" begin
   R = GF(fmpz(123456789012345678949))
   S, x = PolynomialRing(R, "x")

   f = 3*(x^2 + 2x + 1)
   g = x^3 + 3x + 1

   R = factor(f*g)

   @test occursin("x", sprint(show, "text/plain", R))

   @test f*g == unit(R) * prod([ p^e for (p, e) in R])

   R = factor_squarefree(f*g)

   @test f*g == unit(R) * prod([ p^e for (p, e) in R])

   R = factor_distinct_deg((x + 1)*g*(x^5+x+1))

   @test length(R) == 2
   @test R == Dict(1 => x^3+2*x^2+2*x+1,
                3 => x^6+123456789012345678948*x^5+3*x^4+123456789012345678948*x^3+123456789012345678948*x^2+3*x+1)

   R = factor_shape(f*g)

   @test length(R) == 2
   @test R == Dict(3=>1, 1=>2)
end

@testset "gfp_fmpz_poly.remove_valuation" begin
   R = GF(fmpz(123456789012345678949))
   S, y = PolynomialRing(R, "y")

   f = 7y^2 + 3y + 2
   g = f^5*(11y^3 - 2y^2 + 5)

   v, h = remove(g, f)

   @test valuation(g, f) == 5
   @test v == 5
   @test h == (11y^3 - 2y^2 + 5)

   v, q = divides(f*g, f)

   @test v
   @test q == g

   v, q = divides(f*g + 1, f)

   @test !v
end
