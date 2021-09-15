@testset "continued_fraction._shortest_l_infinity" begin
   for k in 1:1000
      l = 2 + rand(1:300)
      b = abs(rand_bits(ZZ, rand(0:l)))
      a = b + abs(rand_bits(ZZ, rand(1:l)))
      c = abs(rand_bits(ZZ, rand(1:l)))
      (v1, v2), (t1, t2) = Nemo._shortest_l_infinity(c, b, a)
      @test (v1, v2) == (t1*c, t1*b + t2*a)
      m = max(abs(v1), abs(v2))
      for x1 in -10:10, x2 in -10:10
         @test iszero(t1+x1) && iszero(t2+x2) ||
                 max(abs((t1+x1)*c), abs((t1+x1)*b+(t2+x2)*a)) >= m
      end
   end
end

@testset "continued_fraction.shortest_l_infinity_with_transform" begin
   # TODO implement matrix-vector product and use it

   m = matrix(ZZ, 0, 2, [])
   (v, t) = Nemo.shortest_l_infinity_with_transform(m)
   @test v == [ZZ(0), ZZ(0)]
   @test length(t) == 0

   m = matrix(ZZ, 1, 2, [0, 0])
   (v, t) = Nemo.shortest_l_infinity_with_transform(m)
   @test v == [ZZ(0), ZZ(0)]
   @test length(t) == 1
   @test v == [t[1]*m[1,i] for i in 1:2]

   m = matrix(ZZ, 1, 2, [1, 0])
   (v, t) = Nemo.shortest_l_infinity_with_transform(m)
   @test v == [ZZ(1), ZZ(0)] || v == [ZZ(-1), ZZ(0)]
   @test length(t) == 1
   @test v == [t[1]*m[1,i] for i in 1:2]

   m = matrix(ZZ, 2, 2, [0, 0, 0, 0])
   (v, t) = Nemo.shortest_l_infinity_with_transform(m)
   @test v == [ZZ(0), ZZ(0)]
   @test length(t) == 2
   @test v == [t[1]*m[1,i] + t[2]*m[2,i] for i in 1:2]

   m = matrix(ZZ, 2, 2, [0, -4, 0, 6])
   (v, t) = Nemo.shortest_l_infinity_with_transform(m)
   @test v == [ZZ(0), ZZ(2)] || v == [ZZ(0), ZZ(-2)]
   @test length(t) == 2
   @test v == [t[1]*m[1,i] + t[2]*m[2,i] for i in 1:2]

   m = matrix(ZZ, 2, 2, [0, 0, -3, 0])
   (v, t) = Nemo.shortest_l_infinity_with_transform(m)
   @test v == [ZZ(3), ZZ(0)] || v == [ZZ(-3), ZZ(0)]
   @test length(t) == 2
   @test v == [t[1]*m[1,i] + t[2]*m[2,i] for i in 1:2]

   m = matrix(ZZ, 2, 3, [0, 1, 3, 4, 5, 6])
   @test_throws Exception Nemo.shortest_l_infinity_with_transform(m)
end

@testset "continued_fraction.fmpq" begin
   for k in 1:100
      x = zero(QQ)
      for i in 1:rand(0:15)
         x = inv(abs(rand_bits(ZZ, rand(1:80))) + x)
      end
      x = rand_bits(ZZ, rand(0:10)) + x

      cf1, m1 = continued_fraction_with_matrix(x)
      @test x == m1[1,1]//m1[2,1]
      @test x == last(convergents(cf1))
      @test isunit(det(m1))

      @test cf1 == continued_fraction(x)

      cnvgts1 = convergents(cf1)
      cnvgts2 = collect(cnvgts1)

      @test eltype(cnvgts1) == fmpq
      @test eltype(typeof(cnvgts1)) == fmpq

      m = identity_matrix(ZZ, 2)
      cf = fmpz[]
      while !iszero(m[1,1] - m[2,1]*x)
         y = divexact(m[2,2]*x - m[1,2], m[1,1] - m[2,1]*x)
         cf2, m2 = continued_fraction_with_matrix(y, limit = rand(1:4))
         cf = vcat(cf, cf2)
         m = m*m2
         @test cnvgts1[length(cf)] == m[1,1]//m[2,1]
         @test cnvgts2[length(cf)] == m[1,1]//m[2,1]
      end
      @test cf == cf1
      @test m == m1
   end
end

@testset "continued_fraction.arb" begin
   CC = RealField(100)
   @test continued_fraction(CC(11//8)) == fmpz[1, 2, 1, 2]
   @test continued_fraction(CC("1.375")) == fmpz[1, 2, 1, 2]
   @test continued_fraction(CC("1.375 +/- 0.0000001")) == fmpz[1, 2, 1]
   @test continued_fraction(CC("543.5 +/- 0.4")) == fmpz[543]
   @test continued_fraction(CC("543.5 +/- 0.5")) == fmpz[]
   @test continued_fraction(-1/const_pi(CC), limit = 4) == fmpz[-1, 1, 2, 7]
   @test continued_fraction_with_matrix(-1/const_pi(CC), limit = 4) ==
                                 (fmpz[-1, 1, 2, 7], matrix(ZZ, [-7 -1; 22 3]))

   z = CC()
   ccall((:arb_zero_pm_one, Nemo.libarb), Nothing, (Ref{arb},), z)
   @test_throws Exception continued_fraction(inv(z))
   # need exact intervals [-1, 1], [543//512, 17/16], [542//512, 17//16] here
   @test continued_fraction(z) == fmpz[]
   @test continued_fraction(ldexp(1087+z, -10)) == fmpz[1, 16]
   @test continued_fraction(ldexp(543+z, -9)) == fmpz[1]

end
