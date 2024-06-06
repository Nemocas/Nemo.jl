@testset "CalciumFieldElem.constructors" begin
  C = CalciumField()

  @test elem_type(C) == CalciumFieldElem
  @test elem_type(CalciumField) == CalciumFieldElem
  @test parent_type(CalciumFieldElem) == CalciumField
  @test is_domain_type(CalciumFieldElem) == true
  @test base_ring(C) == Union{}      # ?
  @test base_ring(C(3)) == Union{}      # ?

  @test isa(C, CalciumField)

  @test isa(C(), CalciumFieldElem)
  @test isa(C(2), CalciumFieldElem)
  @test isa(C(2+3im), CalciumFieldElem)
  @test isa(C(ZZRingElem(2)), CalciumFieldElem)
  @test isa(C(QQFieldElem(2)), CalciumFieldElem)
  @test isa(C(QQBarFieldElem(2)), CalciumFieldElem)
  @test isa(C(C(2)), CalciumFieldElem)

  C2 = CalciumField()

  a = C(3)
  a2 = C2(3)

  @test parent(a) == C
  @test parent(a2) == C2
  @test parent(parent(a)(a2)) == C
  @test parent(parent(a2)(a)) == C2

  t = C(1)
  @test deepcopy(t) !== t
  @test deepcopy(t).parent === t.parent

end

@testset "CalciumFieldElem.options" begin
  C = CalciumField(options=Dict(:prec_limit => 256))
  @test options(C)[:prec_limit] == 256
end

@testset "CalciumFieldElem.printing" begin
  C = CalciumField()
  Cext = CalciumField(extended=true)

  @test string(C) == "Exact complex field"
  @test string(Cext) == "Exact complex field (extended)"

  @test string(C(1)) == "1"
  @test string(Cext(1)) == "1"

  @test string(C(pi)) == "3.14159 {a where a = 3.14159 [Pi]}"

end

@testset "CalciumFieldElem.manipulation" begin
  C = CalciumField()
  Cext = CalciumField(extended=true)

  @test zero(C) == 0
  @test one(C) == 1
  @test isa(zero(C), CalciumFieldElem)
  @test isa(one(C), CalciumFieldElem)

  @test iszero(C(0))
  @test isone(C(1))
  @test isinteger(C(1))
  @test is_rational(C(1))
  @test isreal(C(1))
  @test is_number(C(1))

  u = sqrt(C(2))
  i = sqrt(C(-1))

  @test is_algebraic(u)

  @test i == C(0+1im)
  @test 3+4*i == C(3+4im)

  @test u == Cext(u)
  @test u == C(Cext(u))
  u_i = u + i
  @test u_i == Cext(u_i)
  @test u_i == C(Cext(u_i))

  @test canonical_unit(u) == u
  @test isa(hash(u), UInt)

  @test !isinteger(u)
  @test !is_rational(u)
  @test isreal(u)
  @test !is_rational(i)
  @test !isreal(i)
  @test is_imaginary(i)
  @test !is_imaginary(u)

  @test inv(u) == u // 2

  @test abs(-u) == u
  @test u != i
  @test sign(2*i) == i
  @test conj(i) == -i
  @test real(3+4*i) == 3
  @test imag(3+4*i) == 4
  @test csgn(i) == 1
  #@test sign_real(-3+4*i) == -1
  #@test sign_imag(-3+4*i) == 1
  @test floor(u) == 1
  @test ceil(u) == 2

  @test_throws DomainError infinity(C)
  @test_throws DomainError unsigned_infinity(C)
  @test_throws DomainError undefined(C)
  @test_throws DomainError unknown(C)

  inf = infinity(Cext)
  uinf = unsigned_infinity(Cext)
  und = undefined(Cext)
  unk = unknown(Cext)

  @test_throws DomainError C(inf)

  @test_throws DomainError C(1) // 0
  @test Cext(1) // 0 == uinf

  @test_throws DomainError log(C(0))
  @test log(Cext(0)) == -inf

  @test_throws DomainError C(0) // 0
  @test Cext(0) // 0 == undefined(Cext)

  @test -2*Cext(i)*inf == infinity(Cext(-i))

  @test is_signed_inf(inf)
  @test !is_signed_inf(uinf)
  @test isinf(inf)
  @test isinf(uinf)
  @test !is_uinf(inf)
  @test is_uinf(uinf)

  @test is_undefined(und)
  @test !is_unknown(und)

  @test is_unknown(unk)
  @test_throws ErrorException isreal(unk)
  @test_throws ErrorException is_number(unk)
  @test_throws ErrorException is_undefined(unk)

  @test !isinf(C(1))
  @test !is_uinf(C(1))
  @test !is_signed_inf(C(1))
  @test !is_undefined(C(1))
  @test !is_unknown(C(1))

  @test und == und
  @test_throws ErrorException (unk == unk)

  Rx, x = polynomial_ring(C, "x")
  @test gcd(x^4 - 4*x^2 + 4, x^2 + sqrt(C(18))*x + 4) == x + sqrt(C(2))

end

@testset "CalciumFieldElem.adhoc_operations" begin
  C = CalciumField()

  @test C(2) + C(3) == 5
  @test C(2) + 3 == 5
  @test C(2) + ZZRingElem(3) == 5
  @test C(2) + QQFieldElem(3) == 5
  @test C(2) + QQBarFieldElem(3) == 5
  @test 3 + C(2) == 5
  @test ZZRingElem(3) + C(2) == 5
  @test QQFieldElem(3) + C(2) == 5
  @test QQBarFieldElem(3) + C(2) == 5

  @test C(2) - C(3) == -1
  @test C(2) - 3 == -1
  @test C(2) - ZZRingElem(3) == -1
  @test C(2) - QQFieldElem(3) == -1
  @test C(2) - QQBarFieldElem(3) == -1
  @test 3 - C(2) == 1
  @test ZZRingElem(3) - C(2) == 1
  @test QQFieldElem(3) - C(2) == 1
  @test QQBarFieldElem(3) - C(2) == 1

  @test C(2) * C(3) == 6
  @test C(2) * 3 == 6
  @test C(2) * ZZRingElem(3) == 6
  @test C(2) * QQFieldElem(3) == 6
  @test C(2) * QQBarFieldElem(3) == 6
  @test 3 * C(2) == 6
  @test ZZRingElem(3) * C(2) == 6
  @test QQFieldElem(3) * C(2) == 6
  @test QQBarFieldElem(3) * C(2) == 6

  @test C(6) // C(2) == 3
  @test C(6) // 2 == 3
  @test C(6) // ZZRingElem(2) == 3
  @test C(6) // QQFieldElem(2) == 3
  @test C(6) // QQBarFieldElem(2) == 3
  @test 6 // C(2) == 3
  @test ZZRingElem(6) // C(2) == 3
  @test QQFieldElem(6) // C(2) == 3
  @test QQBarFieldElem(6) // C(2) == 3

  @test divexact(C(6), C(2)) == 3
  @test divexact(C(6), 2) == 3
  @test divexact(C(6), ZZRingElem(2)) == 3
  @test divexact(C(6), QQFieldElem(2)) == 3
  @test divexact(C(6), QQBarFieldElem(2)) == 3
  @test divexact(6, C(2)) == 3
  @test divexact(ZZRingElem(6), C(2)) == 3
  @test divexact(QQFieldElem(6), C(2)) == 3
  @test divexact(QQBarFieldElem(6), C(2)) == 3

  @test C(2) ^ C(3) == 8
  @test C(2) ^ 3 == 8
  @test C(2) ^ ZZRingElem(3) == 8
  @test C(2) ^ QQFieldElem(3) == 8
  @test C(2) ^ QQBarFieldElem(3) == 8
  @test 2 ^ C(3) == 8
  @test ZZRingElem(2) ^ C(3) == 8
  @test QQFieldElem(2) ^ C(3) == 8
  @test QQBarFieldElem(2) ^ C(3) == 8

  @test C(2) < C(3)
  @test C(2) < 3
  @test C(2) < ZZRingElem(3)
  @test C(2) < QQFieldElem(3)
  @test C(2) < QQBarFieldElem(3)
  @test 2 < C(3)
  @test ZZRingElem(2) < C(3)
  @test QQFieldElem(2) < C(3)
  @test QQBarFieldElem(2) < C(3)

end

@testset "CalciumFieldElem.conversions" begin
  C = CalciumField()
  R = algebraic_closure(QQ)

  n = C(3)
  h = C(1) // 2
  c = C(1+2im)
  t = C(pi)

  @test ZZ(n) == 3

  @test QQ(h) == QQFieldElem(1) // 2
  @test_throws ErrorException ZZ(h)

  @test R(h) == QQBarFieldElem(1) // 2
  @test R(c) == QQBarFieldElem(1+2im)
  @test_throws ErrorException R(t)

  RR = ArbField(64)
  CC = AcbField(64)

  @test RR(h) == 0.5
  @test CC(h) == 0.5
  @test CC(c) == CC(1,2)
  @test overlaps(RR(t), RR(pi))
  @test overlaps(CC(t), CC(RR(pi)))

  @test_throws ErrorException RR(c)
  @test RR(c, check=false) == 1.0

  s = sin(C(1), form=:exponential)

  @test isreal(CC(s, parts=true))

  @test overlaps(RR(s), sin(RR(1)))
  @test overlaps(RR(s, check=false), sin(RR(1)))

  @test contains(RR(C(1im)*s, check=false), 0)

  s = 1 * one(C) + 2 * onei(C)
  @test ComplexF64(s) == 1 + 2 * im

  # verify precision bug https://github.com/Nemocas/Nemo.jl/issues/1580 is fixed
  x = C(pi)
  F = ArbField(333)
  y = F(x)
  @test radius(y) < 1e-113
  z = F("3.1415926535897932384626433832795028841971693993751058209749445923078164062862089986280348253421170680 +/- 1.79e-101")
  @test contains(z,y)
end

@testset "CalciumFieldElem.inplace" begin
  C = CalciumField()
  C2 = CalciumField()

  x = C(7)
  zero!(x)
  @test x == 0

  x = C(7)
  y = mul!(x, C(3), C(5))
  @test x == 15
  @test x === y

  @test_throws ErrorException mul!(x, C2(3), C(5))
  @test_throws ErrorException mul!(x, C(3), C2(5))

  x = C(7)
  y = addeq!(x, C(3))
  @test x == 10
  @test x === y

  @test_throws ErrorException addeq!(x, C2(3))

  x = C(7)
  y = add!(x, C(3), C(5))
  @test x == 8
  @test x === y

  @test_throws ErrorException add!(x, C2(3), C(5))
  @test_throws ErrorException add!(x, C(3), C2(5))

end

Base.@irrational mynumber 1.0 BigFloat("1")

@testset "CalciumFieldElem.functions" begin
  C = CalciumField()

  u = sqrt(C(2))
  i = sqrt(C(-1))

  @test const_pi(C) == C(pi)
  @test onei(C) == C(1im)
  @test C(1)//2 < const_euler(C)  + C(3)//5

  @test_throws ErrorException C(mynumber)

  @test real(3+4*i) == 3
  @test imag(3+4*i) == 4
  @test angle(2+2*i) == C(pi) // 4
  @test csgn(-i) == -1
  @test sign(2*i) == i
  @test abs(1+i) == u
  @test conj(1+i) == 1-i
  @test conj(1+C(pi)*i, form=:deep) == 1-C(pi)*i
  @test conj(1+C(pi)*i, form=:shallow) == 1-C(pi)*i
  @test_throws ErrorException conj(1+C(pi)*i, form=:gollum)

  @test floor(u) == 1
  @test ceil(u) == 2
  @test sqrt(i) == (1+i)*u//2
  @test exp(C(pi) * i) == -1
  @test log(exp(u)) == u

  @test pow(1 + C(pi), 25) == pow(1 + C(pi), 25, form=:arithmetic)
  @test_throws ErrorException pow(1 + C(pi), 25, form=:gollum)

  @test sin(C(1)) == sin(C(1), form=:exponential)
  @test sin(C(1)) == sin(C(1), form=:tangent)
  @test sin(C(1)) == sin(C(1), form=:direct)
  @test_throws ErrorException sin(C(1), form=:gollum)

  @test cos(C(1)) == cos(C(1), form=:exponential)
  @test cos(C(1)) == cos(C(1), form=:tangent)
  @test cos(C(1)) == cos(C(1), form=:direct)
  @test_throws ErrorException cos(C(1), form=:gollum)

  @test cos(u)^2 + sin(u)^2 == 1

  @test tan(C(1)) == tan(C(1), form=:exponential)
  @test tan(C(1)) == tan(C(1), form=:sine_cosine)
  @test tan(C(1)) == tan(C(1), form=:direct)
  @test_throws ErrorException tan(C(1), form=:gollum)

  @test atan(C(1)) == C(pi)//4
  @test atan(C(2)) == atan(C(2), form=:logarithm)
  @test atan(C(2)) == atan(C(2), form=:arctangent)
  @test atan(C(2)) == atan(C(2), form=:direct)
  @test_throws ErrorException atan(C(2), form=:gollum)

  @test asin(C(1)) == C(pi)//2
  @test asin(C(2)) == asin(C(2), form=:logarithm)
  @test asin(C(2)) == asin(C(2), form=:direct)
  @test_throws ErrorException asin(C(2), form=:gollum)

  @test acos(C(-1)) == C(pi)
  @test acos(C(2)) == acos(C(2), form=:logarithm)
  @test acos(C(2)) == acos(C(2), form=:direct)
  @test_throws ErrorException acos(C(2), form=:gollum)

  @test gamma(C(5)) == 24
  @test erf(C(1)) == 1 - erfc(C(1))
  @test erfi(C(1)) == -i*erf(i)

  @test string(complex_normal_form(sin(C(1), form=:direct)) + C(1im)) ==
  "0.841471 + 1.00000*I {(-a^2*b+2*a*b+b)/(2*a) where a = 0.540302 + 0.841471*I [Exp(1.00000*I {b})], b = I [b^2+1=0]}"

end

@testset "CalciumFieldElem.rand" begin
  C = CalciumField()
  Cext = CalciumField(extended=true)

  for i=1:10
    x = rand(C, depth=5, bits=5)
    @test is_number(x)
  end

  for i=1:10
    x = rand(C, depth=5, bits=5, randtype=:rational)
    @test is_rational(x)
  end

  @test_throws DomainError [rand(C, depth=1, bits=1, randtype=:special) for i=1:100]

  for i=1:10
    x = rand(Cext, depth=1, bits=1, randtype=:special)
    @test parent(x) == Cext
  end

  @test_throws ErrorException rand(C, depth=2, bits=5, randtype=:gollum)

end

