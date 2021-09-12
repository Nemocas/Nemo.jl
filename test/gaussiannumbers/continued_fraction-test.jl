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

@testset "continuedFraction.shortest_l_infinity_with_transform" begin
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
