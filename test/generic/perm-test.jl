function test_perm_abstract_types()
   print("perm.abstract_types...")

   @test perm <: GroupElem

   @test PermGroup <: Nemo.Group

   println("PASS")
end

function test_perm_constructors()
   print("perm.constructors...")

   R = PermutationGroup(10)

   a = R()
   b = R([2, 3, 5, 4, 6, 7, 1, 9, 10, 8])
   c = R(a)

   @test isa(a, GroupElem)
   @test isa(b, GroupElem)
   @test isa(c, GroupElem)

   println("PASS")
end

function test_perm_printing()
   print("perm.printing...")

   R = PermutationGroup(10)

   b = R([2, 3, 5, 4, 6, 7, 1, 9, 10, 8])

   Nemo.setpermstyle(:array);
   @test string(b) == "[2, 3, 5, 4, 6, 7, 1, 9, 10, 8]"

   Nemo.setpermstyle(:cycles);
   @test string(b) == "(1,2,3,5,6,7)(8,9,10)"

   println("PASS")
end

function test_perm_basic_manipulation()
   print("perm.basic_manipulation...")

   R = PermutationGroup(10)

   a = R()
   b = deepcopy(a)
   c = R([2, 3, 5, 4, 6, 7, 1, 9, 10, 8])

   @test a == b

   @test parity(a) == 0
   @test parity(c) == 1

   @test length(unique([c,deepcopy(c)])) == 1

   a[1] = 5
   @test a[1] == 5

   println("PASS")
end

function test_perm_iteration()
   print("perm.iteration...")

   G = PermutationGroup(5)
   @test length(Nemo.AllPerms(5)) == 120
   @test length(unique(Nemo.AllPerms(5))) == 120

   @test collect(elements(G))[1] == G()

   @test length(collect(elements(G))) == 120
   @test length(unique(elements(G))) == 120

   println("PASS")
end

function test_perm_binary_ops()
   print("perm.binary_ops...")

   G = PermutationGroup(3)

   a = G([2,1,3])
   b = G([2,3,1])

   @test a*b == G([1,3,2])
   @test b*a == G([3,2,1])
   @test a*a == G()
   @test b*b*b == G()

   for a in elements(G), b in elements(G)
      @test parity(a*b) == (parity(b)+parity(a)) % 2
   end

   println("PASS")
end

function test_perm_inversion()
   print("perm.inversion...")

   R = PermutationGroup(10)

   a = R()
   b = R([2, 3, 5, 4, 6, 7, 1, 9, 10, 8])

   @test a == inv(a)
   @test a == b*inv(b)

   G = PermutationGroup(3)
   for a in elements(G), b in elements(G)
      @test inv(a*b) == inv(b)*inv(a)
   end

   println("PASS")
end

function test_misc_functions()
   print("perm.misc...")

   G = PermutationGroup(10)
   a = G([2, 3, 5, 4, 6, 7, 1, 9, 10, 8])

   @test cycles(a) == [[1,2,3,5,6,7], [4], [8,9,10]]
   @test cycles(G()) == [[i] for i in 1:10]

   M = Nemo.matrix_repr(a)
   for (i,j) in enumerate(a.d)
      @test M[i,j] == 1
   end

   println("PASS")
end

function test_perm()
   test_perm_abstract_types()
   test_perm_constructors()
   test_perm_printing()
   test_perm_basic_manipulation()
   test_perm_iteration()
   test_perm_binary_ops()
   test_perm_inversion()
   test_misc_functions()
   println("")
end
