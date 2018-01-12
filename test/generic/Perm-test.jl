function test_perm_abstract_types()
   print("perm.abstract_types...")

   @test Generic.perm <: GroupElem

   @test Generic.PermGroup <: Nemo.Group

   println("PASS")
end

function test_perm_constructors()
   print("perm.constructors...")

   @test elem_type(Generic.PermGroup{Int}) == Generic.perm{Int}
   @test parent_type(Generic.perm{Int}) == Generic.PermGroup{Int}

   R = PermutationGroup(10)
   @test typeof(R) == Generic.PermGroup{Int}
   @test elem_type(R) == Generic.perm{Int}

   a = R()
   @test typeof(a) == Generic.perm{Int}
   @test parent_type(typeof(a)) == Generic.PermGroup{Int}

   b = R([2, 3, 5, 4, 6, 7, 1, 9, 10, 8])
   @test typeof(b) == Generic.perm{Int}
   @test parent_type(typeof(b)) == Generic.PermGroup{Int}


   R = PermutationGroup(Int16(10))
   @test typeof(R) == Generic.PermGroup{Int16}
   @test elem_type(R) == Generic.perm{Int16}

   a = R()
   @test typeof(a) == Generic.perm{Int16}
   @test parent_type(typeof(a)) == Generic.PermGroup{Int16}

   b = R(Int16[2, 3, 5, 4, 6, 7, 1, 9, 10, 8])
   @test typeof(b) == Generic.perm{Int16}
   @test parent_type(typeof(b)) == Generic.PermGroup{Int16}

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

   setpermstyle(:array);
   @test string(b) == "[2, 3, 5, 4, 6, 7, 1, 9, 10, 8]"

   setpermstyle(:cycles);
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
   @test length(AllPerms(5)) == 120
   @test length(unique(AllPerms(5))) == 120

   @test collect(elements(G))[1] == G()

   @test length(collect(elements(G))) == 120
   @test length(unique(elements(G))) == 120

   G = PermutationGroup(Int8(5))
   @test length(AllPerms(Int8(5))) == 120
   @test length(unique(AllPerms(Int8(5)))) == 120

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

   G = PermutationGroup(10)
   p = G([9,5,4,7,3,8,2,10,1,6]) # (1,9)(2,5,3,4,7)(6,8,10)
   @test p^0 == G()
   @test p^1 == p
   @test p^-1 == inv(p)
   @test p^5 == p*p*p*p*p
   @test p^-4 == inv(p)*inv(p)*inv(p)*inv(p)
   @test p^2 * p^-2 == G()
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

   @test cycles(G()) == [[i] for i in 1:10]
   @test order(G()) == 1
   @test cycles(a) == [[1,2,3,5,6,7], [4], [8,9,10]]
   @test order(a) == 6
   @test a^6 == G()

   p = G([9,5,4,7,3,8,2,10,1,6])

   @test cycles(p) == [[1,9],[2,5,3,4,7],[6,8,10]]
   @test order(p) == 30
   @test cycles(p^2) == [[1], [2,3,7,5,4], [6,10,8], [9]]
   @test order(p^2) == 15
   @test cycles(p^3) == [[1,9], [2,4,5,7,3], [6], [8], [10]]
   @test order(p^3) == 10
   @test cycles(p^5) == [[1,9], [2], [3], [4], [5], [6,10,8], [7]]
   @test order(p^5) == 6

   M = matrix_repr(a)
   for (idx, val) in enumerate(a.d)
      @test M[idx, val] == 1
   end

   println("PASS")
end

function test_characters()
   print("perm.characters...")

   N = 8
   G = PermutationGroup(N)
   @test all(character(p)(G()) == dim(YoungTableau(p)) for p=AllParts(N))

   @test character(Partition([2,2,2,2]), Partition([8])) == 0

   N = 3
   G = PermutationGroup(N)
   ps = Partition.([[1,1,1], [2,1], [3]])
   l = Partition([1,1,1])
   @test [character(l, m) for m in ps] == [ 1,-1, 1]
   l = Partition([2,1])
   @test [character(l, m) for m in ps] == [ 2, 0,-1]
   l = Partition([3])
   @test [character(l, m) for m in ps] == [ 1, 1, 1]

   N = 4
   G = PermutationGroup(N)
   ps = Partition.([[1,1,1,1], [2,1,1], [2,2], [3,1], [4]])
   l = Partition([1,1,1,1])
   @test [character(l, m) for m in ps] == [ 1,-1, 1, 1,-1]
   l = Partition([2,1,1])
   @test [character(l, m) for m in ps] == [ 3,-1,-1, 0, 1]
   l = Partition([2,2])
   @test [character(l, m) for m in ps] == [ 2, 0, 2,-1, 0]
   l = Partition([3,1])
   @test [character(l, m) for m in ps] == [ 3, 1,-1, 0,-1]
   l = Partition([4])
   @test [character(l, m) for m in ps] == [ 1, 1, 1, 1, 1]

   # values taken from GAP; note that we specify the order of partitions to be
   # compatible with GAP numbering of conjugacy classes. This is NOT the order
   # of partitions given by AllParts.
   N = 5
   G = PermutationGroup(N)
   ps = Partition.([[1,1,1,1,1], [2,1,1,1], [2,2,1], [3,1,1], [3,2], [4,1], [5]])
   l = Partition([1,1,1,1,1])
   @test [character(l, m) for m in ps] == [   1,  -1,   1,   1,  -1,  -1,   1 ]
   l = Partition([2,1,1,1])
   @test [character(l, m) for m in ps] == [   4,  -2,   0,   1,   1,   0,  -1 ]
   l = Partition([2,2,1])
   @test [character(l, m) for m in ps] == [   5,  -1,   1,  -1,  -1,   1,   0 ]
   l = Partition([3,1,1])
   @test [character(l, m) for m in ps] == [   6,   0,  -2,   0,   0,   0,   1 ]
   l = Partition([3,2])
   @test [character(l, m) for m in ps] == [   5,   1,   1,  -1,   1,  -1,   0 ]
   l = Partition([4,1])
   @test [character(l, m) for m in ps] == [   4,   2,   0,   1,  -1,   0,  -1 ]
   l = Partition([5])
   @test [character(l, m) for m in ps] == [   1,   1,   1,   1,   1,   1,   1 ]

   # test for overflow
   p = Partition(collect(10:-1:1))
   @test character(p, PermutationGroup(55)()) == 44261486084874072183645699204710400

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
   test_characters()
   println("")
end
