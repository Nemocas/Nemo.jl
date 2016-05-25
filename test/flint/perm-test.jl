function test_perm_abstract_types()
   print("perm.abstract_types...")

   @test perm <: GroupElem

   @test FlintPermGroup <: Nemo.Group

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

function test_perm_basic_manipulation()
   print("perm.basic_manipulation...")

   R = PermutationGroup(10)

   a = R()
   b = deepcopy(a)

   @test parity(a) == 0
   @test a == b   

   println("PASS")
end

function test_perm_comparison()
   print("perm.comparison...")

   R = PermutationGroup(10)

   a = R()
   b = deepcopy(a)

   @test a == a
   @test a == b   

   println("PASS")
end

function test_perm_binary_ops()
   print("perm.binary_ops...")

   R = PermutationGroup(10)

   a = R()
   b = deepcopy(a)

   @test a == a*a
   @test a == b*a   

   println("PASS")
end

function test_perm_inversion()
   print("perm.inversion...")

   R = PermutationGroup(10)

   a = R()
   b = R([2, 3, 5, 4, 6, 7, 1, 9, 10, 8])

   @test a == inv(a)
   @test a == b*inv(b)

   println("PASS")
end

function test_perm()
   test_perm_abstract_types()
   test_perm_constructors()
   test_perm_basic_manipulation()
   test_perm_comparison()
   test_perm_binary_ops()
   test_perm_inversion()

   println("")
end
