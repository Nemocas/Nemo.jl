
begin
local test_fields = (finite_field(23, 1, "a"),
                     finite_field(23, 5, "a"),
                     finite_field(next_prime(ZZRingElem(2)^100), 2, "a"))

@testset "FqMPolyRingElem.constructors" begin
   for (R, a) in test_fields
      for num_vars = 1:10
         var_names = ["x$j" for j in 1:num_vars]
         ord = rand_ordering()

         S, varlist = polynomial_ring(R, var_names, ordering = ord)

         SS, varlist = polynomial_ring(R, var_names, ordering = ord)

         @test S === SS

         SSS, varlist = polynomial_ring(R, var_names, ordering = ord, cached = false)
         SSSS, varlist = polynomial_ring(R, var_names, ordering = ord, cached = false)

         @test !(SSS === SSSS)

         @test !occursin("{", string(S))

         @test nvars(S) == num_vars

         @test elem_type(S) == FqMPolyRingElem
         @test elem_type(FqMPolyRing) == FqMPolyRingElem
         @test parent_type(FqMPolyRingElem) == FqMPolyRing

         @test Nemo.promote_rule(elem_type(S), elem_type(R)) == elem_type(S)
         @test Nemo.promote_rule(elem_type(S), Int) == elem_type(S)
         @test Nemo.promote_rule(elem_type(S), ZZRingElem) == elem_type(S)

         @test typeof(S) <: FqMPolyRing

         isa(symbols(S), Vector{Symbol})

         for j = 1:num_vars
            @test isa(varlist[j], FqMPolyRingElem)
            @test isa(gens(S)[j], FqMPolyRingElem)
         end

         f =  rand(S, 0:5, 0:100)

         @test isa(f, FqMPolyRingElem)
         @test isa(S(2), FqMPolyRingElem)
         @test isa(S(R(2)), FqMPolyRingElem)
         @test isa(S(f), FqMPolyRingElem)
         @test degree(modulus(S)) == degree(modulus(R))
         @test degree(modulus(f)) == degree(modulus(R))

         V = [R(rand(-100:100)) for i in 1:5]

         W0 = [Int[rand(0:100) for i in 1:num_vars] for j in 1:5]

         @test isa(S(V, W0), FqMPolyRingElem)

         for i in 1:num_vars
           f = gen(S, i)
           @test is_gen(f)
           @test !is_gen(f + 1)
         end
      end
   end
end

@testset "FqMPolyRingElem.printing" begin
   for (R, a) in test_fields
      S, (x, y) = polynomial_ring(R, ["x", "y"])

      @test !occursin(r"{", string(S))

      @test string(zero(S)) == "0"
      @test string(one(S)) == "1"
      @test degree(R) == 1 || string(S(a)) == "a"
      @test string(x) == "x"
      @test string(y) == "y"

      a = 3
      b = 2
      @test string(x^a + y^b) == "x^$a + y^$b"
   end
end

@testset "FqMPolyRingElem.hash" begin
   for (R, a) in test_fields
      S, (x, y) = polynomial_ring(R, ["x", "y"])

      p = y^2

      @test hash(x) == hash((x + y) - y)
      @test hash(x) == hash((x + p) - p)
   end
end

@testset "FqMPolyRingElem.inside_other_stuff" begin
   for (R, a) in test_fields
      S, (x, y, z, w) = polynomial_ring(R, ["x", "y", "z", "w"])
      @test det(matrix(S, [x y; z w])) == x*w - y*z

      SX, X = polynomial_ring(S, "X")
      @test add!(SX(), X + x, X + y) == x + y + 2*X
      @test iszero(zero!(X))
   end
end

@testset "FqMPolyRingElem.dont_mix_the_parents" begin
   S = FqMPolyRing[polynomial_ring(Ri[1], ["x"])[1] for Ri in test_fields]
   cs = [one(base_ring(Si)) for Si in S]
   exps = Vector{Int}[Int[i] for i in 1:length(S)]
   @test_throws ErrorException S[1](cs, exps)
end

@testset "FqMPolyRingElem.manipulation" begin
   for (R, a) in test_fields
      for num_vars = 1:10
         var_names = ["x$j" for j in 1:num_vars]
         ord = rand_ordering()

         S, varlist = polynomial_ring(R, var_names, ordering = ord)
         g = gens(S)

         @test characteristic(S) == characteristic(R)

         @test !is_gen(S(1))

         @test S(1) == R(1)
         @test R(0) != S(1)
         @test S(1) == 1
         @test 0 != S(1)

         for i = 1:num_vars
            @test is_gen(varlist[i])
            @test is_gen(g[i])
            @test !is_gen(g[i] + 1)
         end

         @test setcoeff!(gen(S, 1), 1, R(2)) == 2*gen(S, 1)

         f = rand(S, 0:5, 0:100)

         @test f == deepcopy(f)

         if length(f) > 0
            i = rand(1:(length(f)))
            @test isa(coeff(f, i), elem_type(R))
         end

         r = S()
         m = S()
         for i = 1:length(f)
            m = monomial!(m, f, i)
            @test m == monomial(f, i)
            @test term(f, i) == coeff(f, i)*monomial(f, i)
            r += coeff(f, i)*monomial(f, i)
         end
         @test f == r

         for i = 1:length(f)
            i1 = rand(1:length(f))
            i2 = rand(1:length(f))
            @test (i1 < i2) == (monomial(f, i1) > monomial(f, i2))
            @test (i1 > i2) == (monomial(f, i1) < monomial(f, i2))
            @test (i1 == i2) == (monomial(f, i1) == monomial(f, i2))
         end

         f = rand(S, 1:5, 0:100)

         if length(f) > 0
           @test f == sum((coeff(f, i) * S([R(1)], [Nemo.exponent_vector(f, i)])  for i in 1:length(f)))
         end

         deg = is_degree(ordering(S))
         rev = is_reverse(ordering(S))

         @test ord == ordering(S)

         @test isone(one(S))

         @test iszero(zero(S))

         @test is_constant(S(rand(-100:100)))
         @test is_constant(S(zero(S)))

         g = S()
         while g == 0
            g = rand(S, 1:1, 0:100)
         end
         h = S()
         while h == 0
            h = rand(S, 1:1, 0:100)
         end
         h = setcoeff!(h, 1, base_ring(h)(1))
         h2 = S()
         while length(h2) < 2
            h2 = rand(S, 2:2, 1:100)
         end

         @test is_term(h)
         @test !is_term(h2 + 1 + gen(S, 1))

         @test is_unit(S(1))
         @test !is_unit(gen(S, 1))

         @test is_monomial(gen(S, 1)*gen(S, num_vars))
         @test !is_monomial(2*gen(S, 1)*gen(S, num_vars))

         monomialexp = unique([Int[rand(0:10) for j in 1:num_vars] for k in 1:10])
         coeffs = [rand(R) for k in 1:length(monomialexp)]
         for i = 1:length(coeffs)
            while coeffs[i] == 0
               coeffs[i] = rand(R)
            end
         end
         h = S(coeffs, monomialexp)
         @test length(h) == length(monomialexp)
         for k in 1:length(h)
           @test coeff(h, S([R(1)], [monomialexp[k]])) == coeffs[k]
         end

         max_degs = max.(monomialexp...)
         for k = 1:num_vars
            @test (degree(h, k) == max_degs[k]) || (h == 0 && degree(h, k) == -1)
            @test degrees(h)[k] == degree(h, k)
         end

         @test (total_degree(h) == max(sum.(monomialexp)...)) || (h == 0 && total_degree(h) == -1)
      end

      S, (x, y) = polynomial_ring(R, ["x", "y"])

      @test trailing_coefficient(3x^2*y^2 + 2x*y + 5x + y + 7) == 7
      @test trailing_coefficient(3x^2*y^2 + 2x*y + 5x) == 5
      @test trailing_coefficient(x) == 1
      @test trailing_coefficient(S(2)) == 2
      @test trailing_coefficient(S()) == 0
   end
end

@testset "FqMPolyRingElem.multivariate_coeff" begin
   for (R, a) in test_fields
      for ord in Nemo.flint_orderings
         S, (x, y, z) = polynomial_ring(R, ["x", "y", "z"]; ordering=ord)

         f = 15*x^5*y^3*z^5+9*x^5*y^2*z^3+15*x^4*y^5*z^4+13*x^4*y^3*z^2+8*x^3*y^2*z+13*x*y^3*z^4+19*a*x*y+13*x*z^2+8*y^2*z^5+14*y^2*z^3

         @test coeff(f, [1], [1]) == 13*y^3*z^4+19*a*y+13*z^2
         @test coeff(f, [2, 3], [3, 2]) == 13*x^4
         @test coeff(f, [1, 3], [4, 5]) == 0

         @test coeff(f, [x], [1]) == 13*y^3*z^4+19*a*y+13*z^2
         @test coeff(f, [y, z], [3, 2]) == 13*x^4
         @test coeff(f, [x, z], [4, 5]) == 0
      end
   end
end

@testset "FqMPolyRingElem.unary_ops" begin
   for (R, a) in test_fields
      for num_vars = 1:10
         var_names = ["x$j" for j in 1:num_vars]
         ord = rand_ordering()

         S, varlist = polynomial_ring(R, var_names, ordering = ord)

         for iter = 1:10
            f = rand(S, 0:5, 0:100)

            @test f == -(-f)
         end
      end
   end
end

@testset "FqMPolyRingElem.binary_ops" begin
   for (R, a) in test_fields
      for num_vars = 1:10
         var_names = ["x$j" for j in 1:num_vars]
         ord = rand_ordering()

         S, varlist = polynomial_ring(R, var_names, ordering = ord)

         for iter = 1:10
            f = rand(S, 0:5, 0:100)
            g = rand(S, 0:5, 0:100)
            h = rand(S, 0:5, 0:100)

            @test f + g == g + f
            @test f - g == -(g - f)
            @test f*g == g*f
            @test f*g + f*h == f*(g + h)
            @test f*g - f*h == f*(g - h)
         end
      end
   end
end

@testset "FqMPolyRingElem.adhoc_binary" begin
   for (R, a) in test_fields
      for num_vars = 1:10
         var_names = ["x$j" for j in 1:num_vars]
         ord = rand_ordering()

         S, varlist = polynomial_ring(R, var_names, ordering = ord)

         for iter = 1:100
            f = rand(S, 0:5, 0:100)

            d1 = rand(-20:20)
            d2 = rand(-20:20)
            g1 = rand(R, -10:10)
            g2 = rand(R, -10:10)

            @test f*d1 + f*d2 == (d1 + d2)*f
            @test f*BigInt(d1) + f*BigInt(d2) == (BigInt(d1) + BigInt(d2))*f
            @test f*g1 + f*g2 == (g1 + g2)*f

            @test f + d1 + d2 == d1 + d2 + f
            @test f + BigInt(d1) + BigInt(d2) == BigInt(d1) + BigInt(d2) + f
            @test f + g1 + g2 == g1 + g2 + f

            @test f - d1 - d2 == -((d1 + d2) - f)
            @test f - BigInt(d1) - BigInt(d2) == -((BigInt(d1) + BigInt(d2)) - f)
            @test f - g1 - g2 == -((g1 + g2) - f)

            @test f + d1 - d1 == f
            @test f + BigInt(d1) - BigInt(d1) == f
            @test f + g1 - g1 == f
         end
      end
   end
end

@testset "FqMPolyRingElem.adhoc_comparison" begin
   for (R, a) in test_fields
      for num_vars = 1:10
         var_names = ["x$j" for j in 1:num_vars]
         ord = rand_ordering()

         S, varlist = polynomial_ring(R, var_names, ordering = ord)

         for iter = 1:100
            d = rand(-100:100)

            @test S(d) == d
            @test d == S(d)
            @test S(ZZRingElem(d)) == ZZRingElem(d)
            @test ZZRingElem(d) == S(ZZRingElem(d))
            @test S(d) == BigInt(d)
            @test BigInt(d) == S(d)
         end
      end
   end
end

@testset "FqMPolyRingElem.powering" begin
   for (R, a) in test_fields
      for num_vars = 1:10
         var_names = ["x$j" for j in 1:num_vars]
         ord = rand_ordering()

         S, varlist = polynomial_ring(R, var_names, ordering = ord)

         for iter = 1:10
            f = rand(S, 0:5, 0:100)

            expn = rand(0:10)

            r = S(1)
            for i = 1:expn
               r *= f
            end

            @test (f == 0 && expn == 0 && f^expn == 0) || f^expn == r

            @test_throws DomainError f^-1
         end
      end
   end
end

@testset "FqMPolyRingElem.divides" begin
   for (R, a) in test_fields
      for num_vars = 1:10
         var_names = ["x$j" for j in 1:num_vars]
         ord = rand_ordering()

         S, varlist = polynomial_ring(R, var_names, ordering = ord)

         @test divexact(2*gen(S, 1), 2) == gen(S, 1)
         @test divexact(2*gen(S, 1), R(2)) == gen(S, 1)

         for iter = 1:10
            f = rand(S, 0:5, 0:100)
            g = rand(S, 0:5, 0:100)

            p = f*g

            flag, q = divides(p, f)

            if flag
              @test q * f == p
            end

            if !iszero(f)
              q = divexact(p, f)
              @test q == g
            end
         end
      end
   end
end

@testset "FqMPolyRingElem.euclidean_division" begin
   for (R, a) in test_fields
      for num_vars = 1:10
         var_names = ["x$j" for j in 1:num_vars]
         ord = rand_ordering()

         S, varlist = polynomial_ring(R, var_names, ordering = ord)

         for iter = 1:10
            f = S(0)
            while iszero(f)
               f = rand(S, 0:5, 0:100)
            end
            g = rand(S, 0:5, 0:100)

            p = f*g

            q1, r = divrem(p, f)
            q2 = div(p, f)

            @test q1 == g
            @test q2 == g
            @test f*q1 + r == p

            q3, r3 = divrem(g, f)
            q4 = div(g, f)
            flag, q5 = divides(g, f)

            @test q3*f + r3 == g
            @test q3 == q4
            @test (r3 == 0 && flag == true && q5 == q3) || (r3 != 0 && flag == false)
         end
      end
   end
end

@testset "FqMPolyRingElem.ideal_reduction" begin
   for (R, a) in test_fields
      for num_vars = 1:10
         var_names = ["x$j" for j in 1:num_vars]
         ord = rand_ordering()

         S, varlist = polynomial_ring(R, var_names, ordering = ord)

         for iter = 1:10
            f = S(0)
            while iszero(f)
               f = rand(S, 0:5, 0:100)
            end
            g = rand(S, 0:5, 0:100)

            p = f*g

            q1, r = divrem(p, [f])

            @test q1[1] == g
            @test r == 0
         end

         for iter = 1:10
            num = rand(1:5)

            V = Vector{elem_type(S)}(undef, num)

            for i = 1:num
               V[i] = S(0)
               while iszero(V[i])
                  V[i] = rand(S, 0:5, 0:100)
               end
            end
            g = rand(S, 0:5, 0:100)

            q, r = divrem(g, V)

            p = r
            for i = 1:num
               p += q[i]*V[i]
            end

            @test p == g
         end
      end
   end
end

@testset "FqMPolyRingElem.gcd" begin
   for (R, a) in test_fields
      for num_vars = 1:4
         var_names = ["x$j" for j in 1:num_vars]
         ord = rand_ordering()

         S, varlist = polynomial_ring(R, var_names, ordering = ord)

         for iter = 1:10
            f = rand(S, 0:4, 0:5)
            g = rand(S, 0:4, 0:5)
            h = rand(S, 0:4, 0:5)

            g1 = gcd(f, g)
            g2 = gcd(f*h, g*h)

            if !iszero(h) && !iszero(g1)
               b, q = divides(g2, g1 * h)
               @test b
               @test is_constant(q)
            end
         end
      end
   end
end

@testset "fqPolyRepMPolyRingElem.factor" begin
   function check_factor(a, esum)
      f = factor(a)
      @test a == unit(f) * prod([p^e for (p, e) in f])
      @test esum == sum(e for (p, e) in f)

      f = factor_squarefree(a)
      @test a == unit(f) * prod([p^e for (p, e) in f])
   end

   for (R, a) in test_fields
      characteristic(R) == 23 || continue
      S, (x, y, z) = polynomial_ring(R, ["x", "y", "z"])

      @test_throws ArgumentError factor(S(0))
      @test_throws ArgumentError factor_squarefree(S(0))

      a = iszero(a) ? one(R) : a
      check_factor(3*a^3*x^23+2*a^2*y^23+a*z^23, 23)
      check_factor(x^99-a^33*y^99*z^33, 22)
   end
end

@testset "FqMPolyRingElem.sqrt" begin
   for (R, a) in test_fields
      for num_vars = 1:4
         var_names = ["x$j" for j in 1:num_vars]
         ord = rand_ordering()

         S, varlist = polynomial_ring(R, var_names, ordering = ord)

         for iter = 1:10
            f = rand(S, 0:4, 0:5, -10:10)

            g = sqrt(f^2)

            @test g^2 == f^2
            @test is_square(f^2)

            b, sf = is_square_with_sqrt(f^2)
            @test b && (sf == f || sf == -f)

            if f != 0
               x = varlist[rand(1:num_vars)]
               @test_throws ErrorException sqrt(f^2*(x^2 - x))
               @test !is_square(f^2*(x^2 - x))
            end
         end
      end
   end
end

@testset "FqMPolyRingElem.evaluation" begin
   for (R, a) in test_fields
      for num_vars = 1:10
         var_names = ["x$j" for j in 1:num_vars]
         ord = rand_ordering()

         S, varlist = polynomial_ring(R, var_names, ordering = ord)

         for iter = 1:100
            f = rand(S, 0:5, 0:100)
            g = rand(S, 0:5, 0:100)

            V1 = [rand(-10:10) for i in 1:num_vars]

            r1 = evaluate(f, V1)
            r2 = evaluate(g, V1)
            r3 = evaluate(f + g, V1)

            @test r3 == r1 + r2
            @test (f + g)(V1...) == f(V1...) + g(V1...)

            V2 = [BigInt(rand(-10:10)) for i in 1:num_vars]

            r1 = evaluate(f, V2)
            r2 = evaluate(g, V2)
            r3 = evaluate(f + g, V2)

            @test r3 == r1 + r2
            @test (f + g)(V2...) == f(V2...) + g(V2...)

            V3 = [R(rand(-10:10)) for i in 1:num_vars]

            r1 = evaluate(f, V3)
            r2 = evaluate(g, V3)
            r3 = evaluate(f + g, V3)

            @test r3 == r1 + r2
            @test (f + g)(V3...) == f(V3...) + g(V3...)
         end
      end

      # Individual tests
      S, (x, y) = polynomial_ring(R, ["x", "y"])
      T = MatrixAlgebra(R, 2)

      f = x^2*y^2+2*x+1

      M1 = T([1 2; 3 4])
      M2 = T([1 1; 2 4])

      @test f(M1, M2) == T([124 219; 271 480])
   end
end

@testset "FqMPolyRingElem.valuation" begin
   for (R, a) in test_fields
      for num_vars = 1:10
         var_names = ["x$j" for j in 1:num_vars]
         ord = rand_ordering()

         S, varlist = polynomial_ring(R, var_names, ordering = ord)

         for iter = 1:100
            f = S()
            g = S()
            while f == 0 || g == 0 || is_constant(g)
               f = rand(S, 0:5, 0:100)
               g = rand(S, 0:5, 0:100)
            end

            d1 = valuation(f, g)

            expn = rand(1:5)

            d2 = valuation(f*g^expn, g)

            @test d2 == d1 + expn

            d3, q3 = remove(f, g)

            @test d3 == d1
            @test f == q3*g^d3

            d4, q4 = remove(q3*g^expn, g)

            @test d4 == expn
            @test q4 == q3
         end
      end
   end
end

@testset "FqMPolyRingElem.derivative" begin
   for (R, a) in test_fields
      for num_vars = 1:10
         var_names = ["x$j" for j in 1:num_vars]
         ord = rand_ordering()

         S, varlist = polynomial_ring(R, var_names, ordering = ord)

         for j in 1:100
            f = rand(S, 0:5, 0:100)
            g = rand(S, 0:5, 0:100)

            for i in 1:num_vars
               @test derivative(f*g, i) == f*derivative(g, i) + derivative(f, i)*g
            end
         end
      end
   end
end

@testset "FqMPolyRingElem.combine_like_terms" begin
   for (R, a) in test_fields
     for num_vars in 1:10
        var_names = ["x$j" for j in 1:num_vars]
        ord = rand_ordering()

        S, _ = polynomial_ring(R, var_names; ordering=ord)

        for iter in 1:10
           f = S()
           while f == 0
              f = rand(S, 5:10, 1:10)
           end

           lenf = length(f)
           f = setcoeff!(f, rand(1:lenf), base_ring(f)(0))
           f = combine_like_terms!(f)

           @test length(f) == lenf - 1

           while length(f) < 2
              f = rand(S, 5:10, 1:10)
           end

           lenf = length(f)
           nrand = rand(1:lenf - 1)
           v = exponent_vector(f, nrand)
           f = set_exponent_vector!(f, nrand + 1, v)
           terms_cancel = coeff(f, nrand) == -coeff(f, nrand + 1)
           f = combine_like_terms!(f)
           @test length(f) == lenf - 1 - terms_cancel
        end
     end
   end
end

@testset "FqMPolyRingElem.exponents" begin
   for (R, a) in test_fields
     for num_vars = 1:10
        var_names = ["x$j" for j in 1:num_vars]
        ord = rand_ordering()

        S, _ = polynomial_ring(R, var_names; ordering=ord)

        for iter in 1:10
           f = S()
           while f == 0
              f = rand(S, 5:10, 1:10)
           end

           nrand = rand(1:length(f))
           v = exponent_vector(f, nrand)
           c = coeff(f, v)

           @test c == coeff(f, nrand)
           for ind = 1:length(v)
              @test v[ind] == exponent(f, nrand, ind)
           end
        end

        for iter in 1:10
           num_vars = nvars(S)

           f = S()
           rand_len = rand(0:10)

           for i = 1:rand_len
              f = set_exponent_vector!(f, i, [rand(0:10) for j in 1:num_vars])
              f = setcoeff!(f, i, base_ring(f)(rand(ZZ, -10:10)))
           end

           f = sort_terms!(f)
           f = combine_like_terms!(f)

           for i = 1:length(f) - 1
              @test exponent_vector(f, i) != exponent_vector(f, i + 1)
              @test coeff(f, i) != 0
           end
           if length(f) > 0
              @test coeff(f, length(f)) != 0
           end
        end
     end
   end
end

@testset "FqMPolyRingElem.gcd_with_cofactors" begin
   # TODO replace by "for (F, t) in test_fields" once gcd_with_cofactors is in AA
   for (F, t) in (test_fields[1], test_fields[2])
      R, (x, y, z) = polynomial_ring(F, [:x, :y, :z])

      @test gcd_with_cofactors(x, y) == (1, x, y)

      F = FactoredFractionField(R)
      (x, y, z) = map(F, (x, y, z))
      a = divexact(x, (x+2y+3z+1))
      b = divexact(y, (x+2y+3z+2))
      c = divexact(z, (x+2y+3z+1)^2)
      ab = a + b
      abc = a + b + c
      @test is_unit(denominator((x+2y+3z+1)^2*(x+2y+3z+2)*abc))
      @test abc - a - b == c
      @test abc - ab == c
   end
end

end # begin
