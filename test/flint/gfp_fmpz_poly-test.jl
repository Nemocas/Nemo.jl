@testset "FpPolyRingElem.constructors" begin
   R = Native.GF(ZZRingElem(123456789012345678949))
   
   S1 = PolyRing(R)
   S2 = PolyRing(R)

   @test isa(S1, FpPolyRing)
   @test S1 !== S2

   S, x = polynomial_ring(R, "x")

   @test elem_type(S) == FpPolyRingElem
   @test elem_type(FpPolyRing) == FpPolyRingElem
   @test parent_type(FpPolyRingElem) == FpPolyRing
   @test dense_poly_type(FpFieldElem) == FpPolyRingElem

   @test Nemo.promote_rule(elem_type(S), ZZRingElem) == elem_type(S)

   @test typeof(S) <: FpPolyRing

   @test isa(x, PolyRingElem{Nemo.FpFieldElem})

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
end

@testset "FpPolyRingElem.printing" begin
   R = Native.GF(ZZRingElem(123456789012345678949))
   S, x = polynomial_ring(R, "x")
   f = x^3 + 2x^2 + x + 1

   @test sprint(show, "text/plain", f) == "x^3 + 2*x^2 + x + 1"
end

@testset "FpPolyRingElem.manipulation" begin
   R = Native.GF(ZZRingElem(123456789012345678949))
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

   @test characteristic(S) == 123456789012345678949
end

@testset "FpPolyRingElem.polynomial" begin
   R = Native.GF(ZZ(23))

   f = polynomial(R, [])
   g = polynomial(R, [1, 2, 3])
   h = polynomial(R, ZZRingElem[1, 2, 3])
   k = polynomial(R, [R(1), R(2), R(3)])
   p = polynomial(R, [1, 2, 3], "y")

   @test isa(f, FpPolyRingElem)
   @test isa(g, FpPolyRingElem)
   @test isa(h, FpPolyRingElem)
   @test isa(k, FpPolyRingElem)
   @test isa(p, FpPolyRingElem)

   q = polynomial(R, [1, 2, 3], cached=false)

   @test parent(g) !== parent(q)
end

@testset "FpPolyRingElem.similar" begin
   R = Native.GF(ZZ(23))

   f = polynomial(R, [1, 2, 3])
   g = similar(f)
   h = similar(f, "y")

   @test isa(g, FpPolyRingElem)
   @test isa(h, FpPolyRingElem)

   q = similar(g, cached=false)

   @test parent(g) === parent(q)
end

@testset "FpPolyRingElem.binary_ops" begin
   R = Native.GF(ZZRingElem(123456789012345678949))
   S, x = polynomial_ring(R, "x")

   f = x^2 + 2x + 1
   g = x^3 + 3x + 2

   @test f + g == x^3+x^2+5*x+3

   @test f*g == x^5+2*x^4+4*x^3+8*x^2+7*x+2

   @test f - g == 123456789012345678948*x^3+x^2+123456789012345678948*x+123456789012345678948
end

@testset "FpPolyRingElem.adhoc_binary" begin
   R = Native.GF(ZZRingElem(123456789012345678949))
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

@testset "FpPolyRingElem.comparison" begin
   R = Native.GF(ZZRingElem(123456789012345678949))
   S, x = polynomial_ring(R, "x")

   f = x^2 + 2x + 1
   g = x^3 + 3x + 2

   @test f != g

   @test isequal(f, deepcopy(f))
end

@testset "FpPolyRingElem.adhoc_comparison" begin
   R = Native.GF(ZZRingElem(123456789012345678949))
   S, x = polynomial_ring(R, "x")

   f = x^2 + 2x + 1

   @test f != 1

   @test 1 != f

   @test S(7) == ZZRingElem(7)

   @test ZZRingElem(7) != f

   @test S(7) == R(7)

   @test R(7) != x + 1
end

@testset "FpPolyRingElem.unary_ops" begin
   R = Native.GF(ZZRingElem(123456789012345678949))
   S, x = polynomial_ring(R, "x")

   f = x^2 + 2x + 1

   @test -f == 123456789012345678948*x^2+123456789012345678947*x+123456789012345678948
end

@testset "FpPolyRingElem.truncation" begin
   R = Native.GF(ZZRingElem(123456789012345678949))
   S, x = polynomial_ring(R, "x")

   f = x^2 + 2x + 1
   g = x^3 + 3x + 1

   @test truncate(f, 2) == 2*x+1

   @test_throws DomainError truncate(f, -1)

   @test mullow(f, g, 3) == 7*x^2+5*x+1

   @test_throws DomainError mullow(f, g, -1)
end

@testset "FpPolyRingElem.reverse" begin
   R = Native.GF(ZZRingElem(123456789012345678949))
   S, x = polynomial_ring(R, "x")

   f = x^2 + 2x + 3

   @test reverse(f) == 3x^2 + 2x + 1
end

@testset "FpPolyRingElem.shift" begin
   R = Native.GF(ZZRingElem(123456789012345678949))
   S, x = polynomial_ring(R, "x")

   f = x^2 + 2x + 1

   @test shift_left(f, 3) == x^5 + 2x^4 + x^3

   @test_throws DomainError shift_left(f, -1)

   @test shift_right(f, 1) == x + 2

   @test_throws DomainError shift_right(f, -1)
end

@testset "FpPolyRingElem.powering" begin
   R = Native.GF(ZZRingElem(123456789012345678949))
   S, x = polynomial_ring(R, "x")

   f = x^2 + 2x + 1

   @test f^6 == x^12+12*x^11+66*x^10+220*x^9+495*x^8+792*x^7+924*x^6+792*x^5+495*x^4+220*x^3+66*x^2+12*x+1

   @test_throws DomainError f^-1
end

@testset "FpPolyRingElem.exact_division" begin
   R = Native.GF(ZZRingElem(123456789012345678949))
   S, x = polynomial_ring(R, "x")

   f = x^2 + 2x + 1
   g = x^3 + 3x + 1

   @test divexact(f*g, f) == g
end

@testset "FpPolyRingElem.adhoc_exact_division" begin
   R = Native.GF(ZZRingElem(123456789012345678949))
   S, x = polynomial_ring(R, "x")

   f = x^2 + 2x + 1

   @test divexact(3*f, ZZRingElem(3)) == f

   @test divexact(3*f, 3) == f

   @test divexact(R(7)*f, R(7)) == f
end

@testset "FpPolyRingElem.modular_arithmetic" begin
   R = Native.GF(ZZRingElem(123456789012345678949))
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

@testset "FpPolyRingElem.euclidean_division" begin
   R = Native.GF(ZZRingElem(123456789012345678949))
   S, x = polynomial_ring(R, "x")

   f = x^2 + 2x + 1
   g = x^3 + 3x + 1

   @test mod(g, f) == 6*x+3

   @test divrem(g, f) == (x+123456789012345678947, 6*x+3)
end

@testset "FpPolyRingElem.gcd" begin
   R = Native.GF(ZZRingElem(123456789012345678949))
   S, x = polynomial_ring(R, "x")

   f = x^2 + 2x + 1
   g = x^3 + 3x + 1
   h = x^2 + 1

   @test gcd(f*h, g*h) == x^2+1

   @test gcdx(f*h, g*h) == (x^2+1, 41152263004115226317*x^2+41152263004115226316*x+2,82304526008230452632*x+123456789012345678948)
end

@testset "FpPolyRingElem.gcdinv" begin
   R = Native.GF(ZZRingElem(123456789012345678949))
   S, x = polynomial_ring(R, "x")

   f = x^2 + 2x + 1
   g = x^3 + 3x + 1

   @test gcdinv(f, g) == (1, 41152263004115226317*x^2+41152263004115226316*x+2)
end

@testset "FpPolyRingElem.square_root" begin
   for R in [Native.GF(ZZ(2)), Native.GF(ZZ(23))]
      S, x = polynomial_ring(R, "x")

      for iter in 1:1000
         f = rand(S, -1:10)
         while is_square(f)
            f = rand(S, -1:10)
         end

         g0 = rand(S, -1:10)
         g = g0^2

         @test is_square(g)
         @test sqrt(g)^2 == g

         if !iszero(g)
            @test !is_square(f*g)
            @test_throws ErrorException sqrt(f*g)
         end

         f1, s1 = is_square_with_sqrt(g)

         @test f1 && s1^2 == g

         if !iszero(g)
            f2, s2 = is_square_with_sqrt(f*g)

            @test !f2
         end
      end
   end
end

@testset "FpPolyRingElem.evaluation" begin
   R = Native.GF(ZZRingElem(123456789012345678949))
   S, x = polynomial_ring(R, "x")

   f = x^2 + 2x + 1

   @test evaluate(f, 3) == 16

   @test evaluate(f, ZZRingElem(10)) == 121

   @test evaluate(f, R(10)) == 121

   @test f(3) == 16

   @test f(ZZRingElem(10)) == 121

   @test f(R(10)) == 121

end

@testset "FpPolyRingElem.composition" begin
   R = Native.GF(ZZRingElem(123456789012345678949))
   S, x = polynomial_ring(R, "x")

   f = x^2 + 2x + 1
   g = x^3 + 3x + 1

   @test compose(f, g; inner = :second) == x^6+6*x^4+4*x^3+9*x^2+12*x+4
end

@testset "FpPolyRingElem.derivative" begin
   R = Native.GF(ZZRingElem(123456789012345678949))
   S, x = polynomial_ring(R, "x")

   f = x^2 + 2x + 1

   @test derivative(f) == 2x + 2
end

@testset "FpPolyRingElem.integral" begin
   R = Native.GF(ZZRingElem(123456789012345678949))
   S, x = polynomial_ring(R, "x")

   f = x^2 + 2x + 1

   @test integral(f) == 82304526008230452633*x^3+x^2+x
end

@testset "FpPolyRingElem.resultant" begin
   R = Native.GF(ZZRingElem(123456789012345678949))
   S, x = polynomial_ring(R, "x")

   f = 5x^2 + 2x + 1
   g = 2x^3 + 3x + 1

   @test resultant(f, g) == 212
end

@testset "FpPolyRingElem.discriminant" begin
   R = Native.GF(ZZRingElem(123456789012345678949))
   S, x = polynomial_ring(R, "x")

   f = x^2 + 2x + 1

   @test discriminant(f) == 0
end

@testset "FpPolyRingElem.lift" begin
   R = Native.GF(ZZRingElem(123456789012345678949))
   S, x = polynomial_ring(R, "x")

   T, y = polynomial_ring(ZZ, "y")

   f = x^2 + 2x + 1

   @test lift(T, f) == y^2 + 2y + 1
end

@testset "FpPolyRingElem.is_irreducible" begin
   R = Native.GF(ZZRingElem(123456789012345678949))
   S, x = polynomial_ring(R, "x")

   f = x^2 + 2x + 1

   @test is_irreducible(f) == false
end

@testset "FpPolyRingElem.is_squarefree" begin
   R = Native.GF(ZZRingElem(123456789012345678949))
   S, x = polynomial_ring(R, "x")

   f = x^2 + 2x + 1

   @test is_squarefree(f) == false
end

@testset "FpPolyRingElem.factor" begin
   R = Native.GF(ZZRingElem(123456789012345678949))
   S, x = polynomial_ring(R, "x")
   F = R

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

   @test issetequal(roots(5 * x * (x^2 + 1)*(x^2 + 2)*(x+1)^2), F.([0, 123456789012345678948, 32539196700765078531, 90917592311580600418]))
end

@testset "FpPolyRingElem.remove_valuation" begin
   R = Native.GF(ZZRingElem(123456789012345678949))
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
