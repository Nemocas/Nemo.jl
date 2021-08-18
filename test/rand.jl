using Random
using RandomExtensions: make

const rng = MersenneTwister()

# test if rand(rng, R, args...) gives reproducible results
# also check that the Random API works as expected
# the type keyword argument specifies what the return type
# of rand(R, args...) must be (by default, elem_type(x)
# or eltype(x))
# A function can be passed as the first argument, which will
# be called on the result of the `rand` call (it should itself
# contain tests, i.e. its output is not checked)

test_rand(@nospecialize(R), @nospecialize(args...); type=nothing) =
   test_rand(nothing, R, args...; type=type)

function test_rand(@nospecialize(test::Union{Nothing,Function}),
                   @nospecialize(R), @nospecialize(args...); type=nothing)
   @nospecialize
   if type === nothing
      if R isa Nemo.Set
         type = elem_type(R)
      else
         type = eltype(R)
      end
   end

   _test(x) = test !== nothing && test(x)

   M = make(R, args...)
   if VERSION >= v"1.3"
      x = @inferred rand(R, args...)
   else
      x = rand(R, args...)
   end
   @test x isa type
   _test(x)
   if VERSION >= v"1.3"
      x = @inferred rand(make(R, args...))
   else
      x = rand(make(R, args...))
   end
   @test x isa type
   _test(x)

   if isempty(args)
      v = rand(R, 2)
      @test v isa Vector{type}
      @test size(v) == (2,)
      foreach(_test, v)
      m = rand(R, 2, 3)
      @test m isa Matrix{type}
      @test size(m) == (2, 3)
      foreach(_test, m)
   end

   vM = rand(M, 2)
   @test vM isa Vector{type}
   @test size(vM) == (2,)
   foreach(_test, vM)
   mM = rand(M, 2, 3)
   @test mM isa Matrix{type}
   @test size(mM) == (2, 3)
   foreach(_test, mM)

   Random.seed!(rng, rng.seed)
   x = rand(rng, R, args...)
   @test x isa type
   _test(x)
   y = rand(rng, M)
   @test y isa type
   _test(y)

   if isempty(args)
      v = rand(rng, R, 2)
      @test v isa Vector{type}
      @test size(v) == (2,)
      foreach(_test, v)
      m = rand(rng, R, 2, 3)
      @test m isa Matrix{type}
      @test size(m) == (2, 3)
      foreach(_test, m)
   end
   vM = rand(rng, M, 2)
   @test vM isa Vector{type}
   @test size(vM) == (2,)
   foreach(_test, vM)

   mM = rand(rng, M, 2, 3)
   @test mM isa Matrix{type}
   @test size(mM) == (2, 3)
   foreach(_test, mM)

   Random.seed!(rng, rng.seed)
   @test x == rand(rng, R, args...)
   @test y == rand(rng, M)
   if isempty(args)
      @test v == rand(rng, R, 2)
      @test m == rand(rng, R, 2, 3)
   end
   @test vM == rand(rng, M, 2)
   @test mM == rand(rng, M, 2, 3)
end
