function test_Floats_constructors()
   print("Julia.Floats.constructors...")

   R = RDF
   S = JuliaRealField

   @test elem_type(R) == Float64
   @test elem_type(S) == BigFloat
   @test elem_type(Nemo.Floats{Float64}) == Float64
   @test elem_type(Nemo.Floats{BigFloat}) == BigFloat
   @test parent_type(Float64) == Nemo.Floats{Float64}
   @test parent_type(BigFloat) == Nemo.Floats{BigFloat}
   
   @test isa(R, Nemo.Floats)
   @test isa(S, Nemo.Floats)

   @test isa(R(), Float64)
   @test isa(S(), BigFloat)

   @test isa(R(11), Float64)
   @test isa(R(11//3), Float64)
   @test isa(R(1.2), Float64)
   @test isa(S(BigInt(11)), BigFloat)
   @test isa(S(1.2), BigFloat)
   @test isa(S(BigFloat(1.2)), BigFloat)
   @test isa(S(BigInt(11)//BigInt(3)), BigFloat)
   @test isa(S(Rational{BigInt}(11)), BigFloat)
   @test isa(S(11), BigFloat)
   
   a = R(11)
   b = S(11)

   @test isa(R(a), Float64)
   @test isa(S(b), BigFloat)

   println("PASS")
end

function test_Floats_manipulation()
   print("Julia.Floats.manipulation...")

   R = RDF
   S = JuliaRealField

   @test iszero(zero(R))
   @test iszero(zero(S))

   @test isone(one(R))
   @test isone(one(S))

   @test !isunit(R())
   @test !isunit(S())
   @test isunit(R(3))
   @test isunit(S(3))

   println("PASS")
end

function test_Floats_exact_division()
   print("Julia.Floats.exact_division...")
  
   R = RDF
   S = JuliaRealField

   for iter = 1:1000
      a1 = rand(R, -10:10)
      a2 = rand(R, -10:10)
      b1 = rand(S, -10:10)
      b2 = rand(S, -10:10)
      c1 = rand(zz, -10:10)
      c2 = rand(JuliaZZ, -10:10)

      @test a2 == 0 || isapprox(divexact(a1*a2, a2), a1)
      @test b2 == 0 || isapprox(divexact(b1*b2, b2), b1)

      @test c1 == 0 || isapprox(divexact(a1*c1, c1), a1)
      @test c2 == 0 || isapprox(divexact(b1*c2, c2), b1)

      @test c1 == 0 || isapprox(divexact(c1, R(1)*c1), R(1))
      @test c2 == 0 || isapprox(divexact(c2, S(1)*c2), S(1))
   end

   println("PASS")
end

function test_Floats_gcd()
   print("Julia.Floats.gcd...")
  
   R = RDF
   S = JuliaRealField

   for iter = 1:1000
      r1 = rand(R, -100:100)
      r2 = rand(R, -100:100)
      s1 = rand(S, -100:100)
      s2 = rand(S, -100:100)

      @test (r1 == 0 && r2 == 0) || gcd(r1, r2) == 1
      @test (s1 == 0 && s2 == 0) || gcd(s1, s2) == 1
   end

   println("PASS")
end


function test_Floats()
   test_Floats_constructors()
   test_Floats_manipulation()
   test_Floats_exact_division()
   test_Floats_gcd()
   
   println("")
end
