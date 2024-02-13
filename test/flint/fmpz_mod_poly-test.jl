@testset "ZZModPolyRingElem.constructors" begin
   R, = residue_ring(ZZ, 123456789012345678949)
   
   S1 = PolyRing(R)
   S2 = PolyRing(R)

   @test isa(S1, ZZModPolyRing)
   @test S1 !== S2

   S, x = polynomial_ring(R, "x")

   @test elem_type(S) == ZZModPolyRingElem
   @test elem_type(ZZModPolyRing) == ZZModPolyRingElem
   @test parent_type(ZZModPolyRingElem) == ZZModPolyRing
   @test dense_poly_type(ZZModRingElem) == ZZModPolyRingElem

   @test typeof(S) <: ZZModPolyRing

   @test isa(x, PolyRingElem)

   f = x^3 + 2x^2 + x + 1

   @test isa(f, PolyRingElem)

   g = S(2)

   @test isa(g, PolyRingElem)

   h = S(x^2 + 2x + 1)

   @test isa(h, PolyRingElem)

   k = S([R(1), R(0), R(3)])

   @test isa(k, PolyRingElem)

   l = S()

   @test isa(l, PolyRingElem)

   m = S(ZZRingElem(123))

   @test isa(m, PolyRingElem)

   n = S([ZZRingElem(1), ZZRingElem(0), ZZRingElem(3)])

   @test isa(n, PolyRingElem)

   T, y = polynomial_ring(ZZ, "y")

   p = 3y^3 + 2y - 1
   q = S(p)

   @test isa(q, PolyRingElem)

   r = S([1, 2, 3])

   @test isa(r, PolyRingElem)

   @test characteristic(S) == 123456789012345678949

    R, = residue_ring(ZZ, ZZRingElem(132))
    Rx,  = polynomial_ring(R, "x")
    @test base_ring(Rx) === R
    @test Rx === polynomial_ring(R, "x")[1]

    R, = residue_ring(ZZ, ZZRingElem(132), cached = false)
    Rx,  = polynomial_ring(R, "x")
    @test base_ring(Rx) === R
    @test Rx === polynomial_ring(R, "x")[1]
end

@testset "ZZModPolyRingElem.printing" begin
   R, = residue_ring(ZZ, 123456789012345678949)
   S, x = polynomial_ring(R, "x")
   f = x^3 + 2x^2 + x + 1

   @test sprint(show, "text/plain", f) == "x^3 + 2*x^2 + x + 1"
end

@testset "ZZModPolyRingElem.manipulation" begin
   R, = residue_ring(ZZ, 123456789012345678949)
   S, x = polynomial_ring(R, "x")

   @test iszero(zero(S))

   @test isone(one(S))

   @test is_gen(gen(S))

   @test is_unit(one(S))

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

   setcoeff!(f, 1, ZZRingElem(2)^100)

   @test coeff(f, 1) == 32146634986640907030

   @test modulus(x) == 123456789012345678949

   @test modulus(R) == 123456789012345678949
end

@testset "ZZModPolyRingElem.polynomial" begin
   R, = residue_ring(ZZ, ZZ(23))

   f = polynomial(R, [])
   g = polynomial(R, [1, 2, 3])
   h = polynomial(R, ZZRingElem[1, 2, 3])
   k = polynomial(R, [R(1), R(2), R(3)])
   p = polynomial(R, [1, 2, 3], "y")

   @test isa(f, ZZModPolyRingElem)
   @test isa(g, ZZModPolyRingElem)
   @test isa(h, ZZModPolyRingElem)
   @test isa(k, ZZModPolyRingElem)
   @test isa(p, ZZModPolyRingElem)

   q = polynomial(R, [1, 2, 3], cached=false)

   @test parent(g) !== parent(q)
end

@testset "ZZModPolyRingElem.similar" begin
   R, = residue_ring(ZZ, ZZ(23))

   f = polynomial(R, [1, 2, 3])
   g = similar(f)
   h = similar(f, "y")

   @test isa(g, ZZModPolyRingElem)
   @test isa(h, ZZModPolyRingElem)

   q = similar(g, cached=false)

   @test parent(g) === parent(q)
end

@testset "ZZModPolyRingElem.binary_ops" begin
   R, = residue_ring(ZZ, 123456789012345678949)
   S, x = polynomial_ring(R, "x")

   f = x^2 + 2x + 1
   g = x^3 + 3x + 2

   @test f + g == x^3+x^2+5*x+3

   @test f*g == x^5+2*x^4+4*x^3+8*x^2+7*x+2

   @test f - g == 123456789012345678948*x^3+x^2+123456789012345678948*x+123456789012345678948
end

@testset "ZZModPolyRingElem.adhoc_binary" begin
   R, = residue_ring(ZZ, 123456789012345678949)
   S, x = polynomial_ring(R, "x")

   f = x^2 + 2x + 1
   g = x^3 + 3x + 2

   @test f*12 == 12*x^2+24*x+12

   @test 7*g == 7*x^3+21*x+14

   @test ZZRingElem(3)*g == 3*x^3+9*x+6

   @test f*ZZRingElem(2) == 2*x^2+4*x+2

   @test f + 12 == x^2+2*x+13

   @test f + ZZRingElem(12) == x^2+2*x+13

   @test f - 12 == x^2+2*x+123456789012345678938

   @test f - ZZRingElem(12) == x^2+2*x+123456789012345678938

   @test 12 + g == x^3+3*x+14

   @test ZZRingElem(12) + g == x^3+3*x+14

   @test 12 - g == 123456789012345678948*x^3+123456789012345678946*x+10

   @test ZZRingElem(12) - g == 123456789012345678948*x^3+123456789012345678946*x+10

   @test f + R(12) == x^2+2*x+13

   @test R(12) + g == x^3+3*x+14

   @test f - R(12) == x^2+2*x+123456789012345678938

   @test R(12) - g == 123456789012345678948*x^3+123456789012345678946*x+10

   @test R(7)*g == 7*x^3+21*x+14

   @test f*R(12) == 12*x^2+24*x+12
end

@testset "ZZModPolyRingElem.comparison" begin
   R, = residue_ring(ZZ, 123456789012345678949)
   S, x = polynomial_ring(R, "x")

   f = x^2 + 2x + 1
   g = x^3 + 3x + 2

   @test f != g

   @test isequal(f, deepcopy(f))
end

@testset "ZZModPolyRingElem.adhoc_comparison" begin
   R, = residue_ring(ZZ, 123456789012345678949)
   S, x = polynomial_ring(R, "x")

   f = x^2 + 2x + 1

   @test f != 1

   @test 1 != f

   @test S(7) == ZZRingElem(7)

   @test ZZRingElem(7) != f

   @test S(7) == R(7)

   @test R(7) != x + 1
end

@testset "ZZModPolyRingElem.unary_ops" begin
   R, = residue_ring(ZZ, 123456789012345678949)
   S, x = polynomial_ring(R, "x")

   f = x^2 + 2x + 1

   @test -f == 123456789012345678948*x^2+123456789012345678947*x+123456789012345678948
end

@testset "ZZModPolyRingElem.truncation" begin
   R, = residue_ring(ZZ, 123456789012345678949)
   S, x = polynomial_ring(R, "x")

   f = x^2 + 2x + 1
   g = x^3 + 3x + 1

   @test truncate(f, 2) == 2*x+1

   @test_throws DomainError truncate(f, -1)

   @test mullow(f, g, 3) == 7*x^2+5*x+1

   @test_throws DomainError mullow(f, g, -1)
end

@testset "ZZModPolyRingElem.reverse" begin
   R, = residue_ring(ZZ, 123456789012345678949)
   S, x = polynomial_ring(R, "x")

   f = x^2 + 2x + 3

   @test reverse(f) == 3x^2 + 2x + 1
end

@testset "ZZModPolyRingElem.shift" begin
   R, = residue_ring(ZZ, 123456789012345678949)
   S, x = polynomial_ring(R, "x")

   f = x^2 + 2x + 1

   @test shift_left(f, 3) == x^5 + 2x^4 + x^3

   @test_throws DomainError shift_left(f, -1)

   @test shift_right(f, 1) == x + 2

   @test_throws DomainError shift_right(f, -1)
end

@testset "ZZModPolyRingElem.powering" begin
   R, = residue_ring(ZZ, 123456789012345678949)
   S, x = polynomial_ring(R, "x")

   f = x^2 + 2x + 1

   @test f^6 == x^12+12*x^11+66*x^10+220*x^9+495*x^8+792*x^7+924*x^6+792*x^5+495*x^4+220*x^3+66*x^2+12*x+1

   @test_throws DomainError f^-1
end

@testset "ZZModPolyRingElem.exact_division" begin
   R, = residue_ring(ZZ, 123456789012345678949)
   S, x = polynomial_ring(R, "x")

   f = x^2 + 2x + 1
   g = x^3 + 3x + 1

   @test divexact(f*g, f) == g
end

@testset "ZZModPolyRingElem.adhoc_exact_division" begin
   R, = residue_ring(ZZ, 123456789012345678949)
   S, x = polynomial_ring(R, "x")

   f = x^2 + 2x + 1

   @test divexact(3*f, ZZRingElem(3)) == f

   @test divexact(3*f, 3) == f

   @test divexact(R(7)*f, R(7)) == f
end

@testset "ZZModPolyRingElem.modular_arithmetic" begin
   R, = residue_ring(ZZ, 123456789012345678949)
   S, x = polynomial_ring(R, "x")

   f = 3*x^2 + x + 2
   g = 5*x^2 + 2*x + 1
   h = 3*x^3 + 2*x^2 + x + 7

   @test invmod(f, h) == 112883663504991137175*x^2+86761824016232857498*x+48511987621979662257

   @test mulmod(f, g, h) == 82304526008230452642*x^2+41152263004115226286*x+41152263004115226316

   @test powermod(f, 10, h) == 118470346535924950143*x^2+97790722831392543222*x+115967716915690326718

   @test powermod(f, ZZRingElem(10), h) == 118470346535924950143*x^2+97790722831392543222*x+115967716915690326718

   @test powermod(f, -10, g) == 78305338116088931412*x+91239060941924718463

   @test powermod(f, -ZZRingElem(10), g) == 78305338116088931412*x+91239060941924718463
end

@testset "ZZModPolyRingElem.euclidean_division" begin
   R, = residue_ring(ZZ, 123456789012345678949)
   S, x = polynomial_ring(R, "x")

   f = x^2 + 2x + 1
   g = x^3 + 3x + 1

   @test mod(g, f) == 6*x+3

   @test divrem(g, f) == (x+123456789012345678947, 6*x+3)

   R, = residue_ring(ZZ, ZZRingElem(24))
   Rx, x = polynomial_ring(R, "x")
   a = 2*x+1
   try
      q = divexact(a^2, a)
      @test q*a = a^2
   catch e
      @test e isa NotInvertibleError
   end
end

@testset "ZZModPolyRingElem.hgcd" begin
   R, = residue_ring(ZZ, next_prime(ZZRingElem(2)^100))
   Rx, x = polynomial_ring(R, "x")
   a = rand(Rx, 501:501)
   b = rand(Rx, 500:500)
   try
      (A, B, m11, m12, m21, m22, s) = hgcd(a, b)
      @test degree(A) >= cld(degree(a), 2) > degree(B)
      @test m11*A + m12*B == a
      @test m21*A + m22*B == b
      @test m11*m22 - m21*m12 == s
      @test s^2 == 1
   catch e
      @test e isa NotInvertibleError
   end
end

@testset "ZZModPolyRingElem.gcd" begin
   R, = residue_ring(ZZ, 123456789012345678949)
   S, x = polynomial_ring(R, "x")

   f = x^2 + 2x + 1
   g = x^3 + 3x + 1
   h = x^2 + 1

   @test gcd(f*h, g*h) == x^2+1

   @test gcdx(f*h, g*h) == (x^2+1, 41152263004115226317*x^2+41152263004115226316*x+2,82304526008230452632*x+123456789012345678948)
end

@testset "ZZModPolyRingElem.gcdinv" begin
   R, = residue_ring(ZZ, 123456789012345678949)
   S, x = polynomial_ring(R, "x")

   f = x^2 + 2x + 1
   g = x^3 + 3x + 1

   @test gcdinv(f, g) == (1, 41152263004115226317*x^2+41152263004115226316*x+2)
end

@testset "ZZModPolyRingElem.evaluation" begin
   R, = residue_ring(ZZ, 123456789012345678949)
   S, x = polynomial_ring(R, "x")

   f = x^2 + 2x + 1

   @test evaluate(f, 3) == 16

   @test evaluate(f, ZZRingElem(10)) == 121

   @test evaluate(f, R(10)) == 121

   @test f(3) == 16

   @test f(ZZRingElem(10)) == 121

   @test f(R(10)) == 121

end

@testset "ZZModPolyRingElem.composition" begin
   R, = residue_ring(ZZ, 123456789012345678949)
   S, x = polynomial_ring(R, "x")

   f = x^2 + 2x + 1
   g = x^3 + 3x + 1

   @test compose(f, g; inner = :second) == x^6+6*x^4+4*x^3+9*x^2+12*x+4
end

@testset "ZZModPolyRingElem.derivative" begin
   R, = residue_ring(ZZ, 123456789012345678949)
   S, x = polynomial_ring(R, "x")

   f = x^2 + 2x + 1

   @test derivative(f) == 2x + 2
end

@testset "ZZModPolyRingElem.integral" begin
   R, = residue_ring(ZZ, 123456789012345678949)
   S, x = polynomial_ring(R, "x")

   f = x^2 + 2x + 1

   @test integral(f) == 82304526008230452633*x^3+x^2+x
end

@testset "ZZModPolyRingElem.resultant" begin
   R, = residue_ring(ZZ, 123456789012345678949)
   S, x = polynomial_ring(R, "x")

   f = 5x^2 + 2x + 1
   g = 2x^3 + 3x + 1

   @test resultant(f, g) == 212
end

@testset "ZZModPolyRingElem.discriminant" begin
   R, = residue_ring(ZZ, 123456789012345678949)
   S, x = polynomial_ring(R, "x")

   f = x^2 + 2x + 1

   @test discriminant(f) == 0
end

@testset "ZZModPolyRingElem.lift" begin
   R, = residue_ring(ZZ, 123456789012345678949)
   S, x = polynomial_ring(R, "x")

   T, y = polynomial_ring(ZZ, "y")

   f = x^2 + 2x + 1

   @test lift(T, f) == y^2 + 2y + 1
end

@testset "ZZModPolyRingElem.is_irreducible" begin
   R, = residue_ring(ZZ, 123456789012345678949)
   S, x = polynomial_ring(R, "x")

   f = x^2 + 2x + 1

   @test is_irreducible(f) == false
end

@testset "ZZModPolyRingElem.is_squarefree" begin
   R, = residue_ring(ZZ, 123456789012345678949)
   Rx, x = polynomial_ring(R, "x")

   f = x^2 + 2x + 1

   @test is_squarefree(f) == false

   @test !is_squarefree(Rx(0))
   @test is_squarefree(Rx(1))
   @test is_squarefree(Rx(2))
   @test is_squarefree(Rx(4))
end

@testset "ZZModPolyRingElem.factor" begin
   R, = residue_ring(ZZ, 123456789012345678949)
   S, x = polynomial_ring(R, "x")

   @test_throws ArgumentError factor(S(0))
   @test_throws ArgumentError factor_squarefree(S(0))

   f = 3*(x^2 + 2x + 1)
   g = x^3 + 3x + 1

   R = factor(f*g)

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

@testset "ZZModPolyRingElem.roots" begin
  _, x = polynomial_ring(residue_ring(ZZ, ZZ(1024))[1], "x")
  @test length(roots(x^2+7)) == 4

  _, x = polynomial_ring(residue_ring(ZZ, ZZ(1031))[1], "x")
  @test length(roots(x^2+7)) == 2

  for n in (17, 18, 19, 20)
    _, x = polynomial_ring(residue_ring(ZZ, ZZ(10)^n)[1], "x")
    @test length(roots(10*x)) == 10
  end
end

@testset "ZZModPolyRingElem.remove_valuation" begin
   R, = residue_ring(ZZ, 123456789012345678949)
   S, y = polynomial_ring(R, "y")

   f = 7y^2 + 3y + 2
   g = f^5*(11y^3 - 2y^2 + 5)

   @test_throws Exception remove(f, zero(S))
   @test_throws Exception remove(f, one(S))
   @test_throws Exception remove(zero(S), f)

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
