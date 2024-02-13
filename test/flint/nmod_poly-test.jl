@testset "zzModPolyRingElem.constructors" begin
  R, = residue_ring(ZZ, 17)
  
  S1 = PolyRing(R)
  S2 = PolyRing(R)

  @test isa(S1, zzModPolyRing)
  @test S1 !== S2

  Rx, x = polynomial_ring(R, "x")

  @test elem_type(Rx) == zzModPolyRingElem
  @test elem_type(zzModPolyRing) == zzModPolyRingElem
  @test parent_type(zzModPolyRingElem) == zzModPolyRing
  @test dense_poly_type(zzModRingElem) == zzModPolyRingElem

  S, = residue_ring(ZZ, 19)
  Sy, y = polynomial_ring(R, "y")

  RRx, xx = polynomial_ring(R, "x")
  RRRx, xxx = polynomial_ring(residue_ring(ZZ, 17)[1], "xx")

  @test var(Rx) == Symbol("x")

  @test RRx != RRRx

  @test RRx == Rx

  @test S != R

  @test isa(Rx, zzModPolyRing)
  @test isa(x, PolyRingElem)

  a = Rx()

  @test isa(a, PolyRingElem)
  @test parent(a) == Rx

  b = Rx(2)

  @test isa(b, PolyRingElem)
  @test parent(b) == Rx

  c = Rx(UInt(3))

  @test isa(c, PolyRingElem)
  @test parent(c) == Rx

  d = Rx(ZZRingElem(3))

  @test isa(d, PolyRingElem)
  @test parent(d) == Rx

  e = Rx(R(16))

  @test isa(e, PolyRingElem)
  @test parent(e) == Rx

  f = Rx([UInt(1), UInt(2), UInt(3)])

  @test isa(f, PolyRingElem)
  @test parent(f) == Rx

  g = Rx([ZZRingElem(1), ZZRingElem(2), ZZRingElem(3)])

  @test isa(g, PolyRingElem)
  @test parent(g) == Rx

  h = Rx([R(1), R(2), R(3)])

  @test isa(h, PolyRingElem)
  @test parent(h) == Rx

  _a = polynomial_ring(ZZ, "y")[1]([ZZRingElem(1),ZZRingElem(2),ZZRingElem(3)])

  k = Rx(_a)

  @test isa(k, PolyRingElem)
  @test parent(k) == Rx

  l = x^2 + x^2 + x^2 + x^1 + x^1 + R(1)

  @test isa(l, PolyRingElem)
  @test parent(l) == Rx

  @test f == g
  @test g == h
  @test h == k
  @test k == l

  m = Rx([1, 2, 3])

  @test isa(m, PolyRingElem)
end

@testset "zzModPolyRingElem.printing" begin
  R, = residue_ring(ZZ, 17)
  Rx, x = polynomial_ring(R, "x")

  a = x^3 + x + 1

  @test sprint(show, "text/plain", a) == "x^3 + x + 1"
end

@testset "zzModPolyRingElem.manipulation" begin
  R, = residue_ring(ZZ, 17)
  Rx, x = polynomial_ring(R, "x")

  @test isone(one(Rx))
  @test iszero(zero(Rx))
  @test is_gen(gen(Rx))
  @test is_unit(one(Rx))

  @test !is_unit(gen(Rx))

  @test degree(x) == 1
  @test degree(x^10) == 10

  @test length(x^10) == 11

  @test coeff(x^6 + R(2)*x^5, 5) == R(2)

  @test leading_coefficient(R(3)*x^2 + x) == R(3)

  @test canonical_unit(-x + 1) == R(-1)

  @test deepcopy(one(Rx)) == one(Rx)

  @test var(Rx) == :x

  @test modulus(x) == 17

  @test modulus(R) == 17

  @test characteristic(Rx) == 17
end

@testset "zzModPolyRingElem.polynomial" begin
   R, = residue_ring(ZZ, 23)

   f = polynomial(R, [])
   g = polynomial(R, [1, 2, 3])
   h = polynomial(R, ZZRingElem[1, 2, 3])
   k = polynomial(R, [R(1), R(2), R(3)])
   p = polynomial(R, [1, 2, 3], "y")

   @test isa(f, zzModPolyRingElem)
   @test isa(g, zzModPolyRingElem)
   @test isa(h, zzModPolyRingElem)
   @test isa(k, zzModPolyRingElem)
   @test isa(p, zzModPolyRingElem)

   q = polynomial(R, [1, 2, 3], cached=false)

   @test parent(g) !== parent(q)
end

@testset "zzModPolyRingElem.similar" begin
   R, = residue_ring(ZZ, 23)

   f = polynomial(R, [1, 2, 3])
   g = similar(f)
   h = similar(f, "y")

   @test isa(g, zzModPolyRingElem)
   @test isa(h, zzModPolyRingElem)

   q = similar(g, cached=false)

   @test parent(g) === parent(q)
end

@testset "zzModPolyRingElem.unary_ops" begin
  R, = residue_ring(ZZ, 23)
  Rx, x = polynomial_ring(R, "x")

  f = x^2 + R(13)*x + R(5)

  @test -f ==  R(22)*x^2 + R(10)*x + R(18)
end

@testset "zzModPolyRingElem.binary_ops" begin
  R, = residue_ring(ZZ, 23)
  Rx, x = polynomial_ring(R, "x")

  f = x^2 + R(13)*x + R(5)
  g = x^3 + R(11)*x^2 + R(10)*x + R(18)
  h = x + R(10)

  @test f + g == x^3 + R(12)*x^2

  @test f*g == x^5 + x^4 + R(20)*x^3 + R(19)*x^2 + R(8)*x + R(21);

  @test f - g == R(22)*x^3 + R(13)*x^2 + R(3)*x + R(10)

  @test h*(f+g) == x^4 + R(22)*x^3 + R(5)*x^2
end

@testset "zzModPolyRingElem.adhoc_binary" begin
  R, = residue_ring(ZZ, 113)
  Rx, x = polynomial_ring(R, "x")

  S, = residue_ring(ZZ,112)

  f = x^2 + R(2)x + R(1)
  g = x^3 + R(3)x^2 + x

  @test ZZRingElem(2)*f == R(2)x^2 + R(4)x + R(2)
  @test ZZRingElem(2)*f == f*ZZRingElem(2)
  @test 2*f == ZZRingElem(2)*f
  @test f*2 == 2*f
  @test R(2)*f == ZZRingElem(2)*f
  @test f*R(2) == R(2)*f

  @test_throws ErrorException S(1)*f

  @test f + 112 == x^2 + R(2)x
  @test 112 + f == f + 112
  @test R(112) + f == f + 112
  @test 112 + f == f + R(112)
  @test f + ZZRingElem(112) == f + 112
  @test ZZRingElem(112) + f == f + ZZRingElem(112)

  @test_throws ErrorException S(1)+f

  @test f - 1 == x^2 + R(2)x
  @test ZZRingElem(1) - f == -(f - ZZRingElem(1))
  @test ZZRingElem(1) - f == R(112)*x^2 + R(111)*x
  @test f - R(1) == f - 1
  @test R(1) - f == -(f - R(1))

  @test_throws ErrorException f - S(1)
end

@testset "zzModPolyRingElem.powering" begin
  R, = residue_ring(ZZ, 23)
  Rx, x = polynomial_ring(R, "x")

  f = x^2 + R(13)x + R(5)

  @test f^3 == x^6 + 16x^5 + 16x^4 + 11x^3 + 11x^2 + 9x + 10

  @test f^23 == x^46 + 13*x^23 + 5

  @test_throws DomainError f^(-1)
end

@testset "zzModPolyRingElem.comparison" begin
  R, = residue_ring(ZZ, 23)
  Rx, x = polynomial_ring(R, "x")
  Ry, y = polynomial_ring(R, "y")

  @test x^2 + x == x^2 + x
  @test_throws ErrorException x^2 + x != y^2 + y
end

@testset "zzModPolyRingElem.adhoc_comparison" begin
   R, = residue_ring(ZZ, 7)
   S, x = polynomial_ring(R, "x")

   f = x^2 + 2x + 1

   @test f != 1

   @test 1 != f

   @test S(5) == ZZRingElem(5)

   @test ZZRingElem(5) != f

   @test S(5) == R(5)

   @test R(5) != x + 1
end

@testset "zzModPolyRingElem.truncation" begin
  R, = residue_ring(ZZ, 23)
  Rx, x = polynomial_ring(R, "x")

  f = x^5 + x^4 + 2 *x^2

  g = truncate(f,4)

  @test parent(g) == parent(f)

  @test truncate(f,2) == R(0)
  @test truncate(f,5) == x^4 + 2*x^2
  @test truncate(f,10) == f
end

@testset "zzModPolyRingElem.mullow" begin
  R, = residue_ring(ZZ, 23)
  Rx, x = polynomial_ring(R, "x")

  f = x^5 + x^4 + 2 *x^2
  g = x^4 + x^3

  @test mullow(f,g,2) == truncate(f*g,2)
  @test mullow(f,g,7) == truncate(f*g,7)
end

@testset "zzModPolyRingElem.reverse" begin
  R, = residue_ring(ZZ, 23)
  Rx, x = polynomial_ring(R, "x")

  f = x^5 + x^4 + 2 *x^2 + 1

  g = reverse(f)

  @test parent(g) == parent(f)
  @test g == x^5 + 2*x^3 + x + 1
  @test isone(reverse(x))
end

@testset "zzModPolyRingElem.shift" begin
  R, = residue_ring(ZZ, 23)
  Rx, x = polynomial_ring(R, "x")

  f = x^5 + x^4 + 2 *x^2 + 1

  g = shift_left(f,3)
  h = shift_right(f,3)

  @test parent(g) == parent(f)
  @test parent(h) == parent(f)

  @test g == x^3*f

  @test h == x^2 + x

  @test f == shift_right(shift_left(f,20),20)

  @test_throws DomainError shift_left(f,-1)
  @test_throws DomainError shift_right(f,-1)
end

@testset "zzModPolyRingElem.division" begin
  R, = residue_ring(ZZ, 23)
  Rx, x = polynomial_ring(R, "x")

  f = x^5 + x^4 + 2 *x^2 + x

  g = divexact(f,x)

  @test parent(g) == parent(f)
  @test g == x^4 + x^3 + 2*x + 1

  g = divexact(f,x+1)

  @test g == x^4 + 2*x + 22

  h = x^1235+x^23

  @test divexact(f*h,h) == f

  @test_throws DivideError divexact(f,zero(Rx))

  @test f - divexact(f,Rx(2))*2 == zero(Rx)

  (q,r) = divrem(f,x+2)

  @test parent(q) == parent(f)
  @test parent(r) == parent(f)

  @test (q,r) == (x^4 + 22*x^3 + 2*x^2 + 21*x + 5, Rx(13))
  @test f == q*(x+2) + r
  @test_throws DivideError divrem(f,zero(Rx))

  r = rem(f,x+3)
  @test parent(r) == parent(f)
  @test r == Rx(14)

   R, = residue_ring(ZZ, 24)
   Rx, x = polynomial_ring(R, "x")
   a = 2*x+1
   try
      q = div(a^2, a)
      @test q*a = a^2
   catch e
      @test e isa NotInvertibleError
   end
end

@testset "zzModPolyRingElem.hgcd" begin
   R, = residue_ring(ZZ, next_prime(2^30))
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

@testset "zzModPolyRingElem.adhoc_exact_division" begin
   R, = residue_ring(ZZ, 23)
   S, x = polynomial_ring(R, "x")

   f = x^2 + 2x + 1

   @test divexact(3*f, ZZRingElem(3)) == f

   @test divexact(3*f, 3) == f

   @test divexact(R(7)*f, R(7)) == f
end

@testset "zzModPolyRingElem.gcd" begin
  R, = residue_ring(ZZ, 23)
  Rx, x = polynomial_ring(R, "x")

  f = x^5 + x^4 + 2 *x^2 + x
  g = x^3 + x^2 + x

  k = gcd(f,g)

  @test parent(k) == parent(x)
  @test k == x

  k, s, t = gcdx(f,g)

  @test k == s*f + t*g
end

@testset "zzModPolyRingElem.modular_arithmetic" begin
  R, = residue_ring(ZZ, 487326487)
  S, x = polynomial_ring(R, "x")

  f = 3*x^2 + x + 2
  g = 5*x^2 + 2*x + 1
  h = 3*x^3 + 2*x^2 + x + 7

  @test gcdinv(f, g) == (1,84344969*x+234291581)

  @test invmod(f, h) == 40508247*x^2+341251293*x+416130174

  @test mulmod(f, g, h) == 324884334*x^2+162442132*x+162442162

  @test powermod(f, 10, h) == 485924368*x^2+380106591*x+302530457

  @test powermod(f, -10, g) == 484381224*x+14566177
end

@testset "zzModPolyRingElem.resultant" begin
  R, = residue_ring(ZZ, 23)
  Rx, x = polynomial_ring(R, "x")

  f = x^5 + x^4 + 2 *x^2 + x

  @test resultant(f,x) == Rx(0)

  g = resultant(f,x^2 +2)

  @test parent(g) == R

  @test g == 4
end

@testset "zzModPolyRingElem.evaluate" begin
  R, = residue_ring(ZZ, 23)
  Rx, x = polynomial_ring(R, "x")

  f = x^5 + x^4 + 2 *x^2 + x

  r = evaluate(f, R(20))
  s = evaluate(f, 20)
  t = evaluate(f, ZZRingElem(20))

  @test r == R(14)

  @test s == R(14)

  @test t == R(14)

  @test f(R(20)) == R(14)

  @test f(20) == R(14)

  @test f(ZZRingElem(20)) == R(14)
end

@testset "zzModPolyRingElem.derivative" begin
  R, = residue_ring(ZZ, 23)
  Rx, x = polynomial_ring(R, "x")

  f = x^5 + x^4 + 2 *x^2 + x

  ff = derivative(f)

  @test parent(ff) == parent(f)

  @test ff == 5*x^4 + 4*x^3 + 4*x + 1
end

@testset "zzModPolyRingElem.integral" begin
  R, = residue_ring(ZZ, 7)
  S, x = polynomial_ring(R, "x")

  f = x^2 + 2x + 1

  @test integral(f) == 5x^3 + x^2 + x
end

@testset "zzModPolyRingElem.compose" begin
  R, = residue_ring(ZZ, 23)
  Rx, x = polynomial_ring(R, "x")

  f = x^5 + x^4 + 2 *x^2 + x
  g = x+1

  ff = compose(f, g; inner = :second)

  @test parent(ff) == parent(f)

  @test ff == x^5 + 6*x^4 + 14*x^3 + 18*x^2 + 14*x + 5
end

@testset "zzModPolyRingElem.interpolate" begin
  R, = residue_ring(ZZ, 23)
  Rx, x = polynomial_ring(R, "x")

  xval = [ R(0), R(1), R(2), R(3) ]

  yval = [ R(0), R(1), R(4), R(9) ]

  f = interpolate(Rx,xval,yval)

  @test parent(f) == Rx
  @test f == x^2
end

@testset "zzModPolyRingElem.inflate" begin
  R, = residue_ring(ZZ, 23)
  Rx, x = polynomial_ring(R, "x")

  f = x^6 + x^4 + 2 *x^2

  g = inflate(f,2)

  @test parent(g) == parent(f)
  @test g == x^12 + x^8 + 2*x^4

  @test_throws DomainError inflate(f,-1)
end

@testset "zzModPolyRingElem.deflate" begin
  R, = residue_ring(ZZ, 23)
  Rx, x = polynomial_ring(R, "x")

  f = x^6 + x^4 + 2 *x^2

  g = deflate(f,2)

  @test parent(g) == parent(f)
  @test g == x^3 + x^2 + 2*x

  @test_throws DomainError deflate(f,-1)
end

@testset "zzModPolyRingElem.lifting" begin
  R, = residue_ring(ZZ, 23)
  Rx, x = polynomial_ring(R, "x")
  Zy,y = polynomial_ring(ZZ, "y")

  f = x^6 + x^4 + 22*x^2

  Zf = lift(Zy, f)

  @test Zf == y^6 + y^4 + 22*y^2
  @test Rx(Zf) == f
end

@testset "zzModPolyRingElem.is_irreducible" begin
  R, = residue_ring(ZZ, 23)
  Rx, x = polynomial_ring(R, "x")

  f = x^6 + x^4 + 2 *x^2

  @test !is_irreducible(f)

  @test is_irreducible(x)

  @test is_irreducible(x^16+2*x^9+x^8+x^2+x+1)
end

@testset "zzModPolyRingElem.is_squarefree" begin
  R, = residue_ring(ZZ, 23)
  Rx, x = polynomial_ring(R, "x")

  f = x^6 + x^4 + 2 *x^2

  @test !is_squarefree(f)

  @test is_squarefree((x+1)*(x+2)*(x+3))

  @test !is_squarefree(Rx(0))
  @test is_squarefree(Rx(1))
  @test is_squarefree(Rx(2))
  @test is_squarefree(Rx(4))
end

@testset "zzModPolyRingElem.factor" begin
  R, = residue_ring(ZZ, 23)
  Rx, x = polynomial_ring(R, "x")

  @test_throws ArgumentError factor(Rx(0))
  @test_throws ArgumentError factor_squarefree(Rx(0))

  f = 2*((x^6 + x^4 + 2 *x^2 )^10 + x - 1)

  fac = factor(f)

  @test f == unit(fac)*prod([ p^e for (p, e) in fac])
  p = unit(fac)

  sh = factor_shape(f)

  @test sh == Dict(36=>1,1=>1,4=>1,19=>1)

  f = 5*(x^6 + x^4 + 2 *x^2 )^10

  fac = factor_squarefree(f)

  @test f == unit(fac) * prod([ p^e for (p, e) in fac])

  @test_throws ErrorException factor_distinct_deg(f)

  fac = factor_distinct_deg(x* (x^2 + 1)*(x^2 + 2)*(x+1))

  @test fac == Dict(2=>x^4+3*x^2+2,1=>x^2 + x)
end

@testset "zzModPolyRingElem.roots" begin
  _, x = polynomial_ring(residue_ring(ZZ, 1024)[1], "x")
  @test length(roots(x^2+7)) == 4

  _, x = polynomial_ring(residue_ring(ZZ, 1031)[1], "x")
  @test length(roots(x^2+7)) == 2
end

@testset "zzModPolyRingElem.canonicalization" begin
  R, = residue_ring(ZZ, 23)
  Rx, x = polynomial_ring(R, "x")

  @test canonical_unit(5*x) == R(5)
end

@testset "zzModPolyRingElem.remove_valuation" begin
  R, = residue_ring(ZZ, 23)
  Rx, x = polynomial_ring(R, "x")

  f = (x + 1)^10 * (x + 2) * (x + 3)
  g = x + 1

  @test_throws Exception remove(f, zero(Rx))
  @test_throws Exception remove(f, one(Rx))
  @test_throws Exception remove(zero(Rx), g)

  n, p = remove(f, g)

  @test n == valuation(f, g)
  @test n == 10
  @test p == (x+2)*(x+3)

   v, q = divides(f*g, f)

   @test v
   @test q == g

   v, q = divides(f*g + 1, f)

   @test !v
end
