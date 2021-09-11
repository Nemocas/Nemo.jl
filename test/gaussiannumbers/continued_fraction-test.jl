@testset "continued_fraction._shortest_l_infinity" begin
  let l = 1000
    for k in 1:1000
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
end

