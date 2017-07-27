function test_partition_type()
   print("youngtabs.partition_type...")

   @test isa(Partition([4,3,1]), Partition)
   @test_throws String Partition([3,4,1])
   @test_throws String Partition([4,3,0])

   p = Partition([4,3,1])

   @test size(p) == (3,)
   @test p[1] == 4
   @test p[3] == 1

   @test length(p) == 3
   @test sum(p) == 8

   @test_throws String p[1] = 2
   @test_throws String p[2] = 5
   @test_throws String p[2] = 0

   p[3] = 2
   @test p[3] == 2
   @test p == Partition([4,3,2])

   println("PASS")
end

function test_youngtabs()
   print("youngtabs.YoungTableau...")

   lambda = [4,3,1]
   Y = YoungTableau(lambda)

   for i in 1:sum(lambda)
       @test Y[findfirst(Y, i)...] == i
   end

   @test [hooklength(Y,i,j) for i in 1:size(Y,1), j in 1:size(Y,2)] ==
      [6 4 3 1; 4 2 1 0; 1 0 0 0]

   @test dim(Y) == 70
   @test dim(YoungTableau([2,2])) == 2
   @test dim(YoungTableau([3,1])) == 3
   @test dim(YoungTableau([2,1,1])) == 3
   @test dim(YoungTableau([1,1,1,1])) == 1

   @test dim(YoungTableau([4])) == 1

   @test dim(YoungTableau(collect(10:-1:1))) == 44261486084874072183645699204710400

   println("PASS")
end

function test_skewdiags()
   print("youngtabs.skewdiags...")

   l = [5,3,2,2,2,1,1]
   m = [2,2,1]
   lambda = Partition(l)
   mu = Partition(m)
   xi = lambda/mu
   @test isa(xi, SkewDiagram)
   psi = SkewDiagram(l,m)
   @test isa(psi, SkewDiagram)
   @test xi == psi

   # 7×5 Array{Int64,2}:
   #  0  0  1  1  1
   #  0  0  1  0  0
   #  0  1  0  0  0
   #  1  1  0  0  0
   #  1  1  0  0  0
   #  1  0  0  0  0
   #  1  0  0  0  0

   @test Nemo.inskewdiag(xi, 1, 5) == true
   @test Nemo.inskewdiag(xi, 1, 3) == true
   @test Nemo.inskewdiag(xi, 2, 3) == true
   @test Nemo.inskewdiag(xi, 2, 2) == false
   @test Nemo.inskewdiag(xi, 5, 2) == true
   @test Nemo.inskewdiag(xi, 6, 2) == false

   @test Nemo.inskewdiag(xi, 8, 9) == false
   @test Nemo.inskewdiag(xi, -1, 1) == false
   @test Nemo.inskewdiag(xi, 1, -1) == false
   @test Nemo.inskewdiag(xi, -1, -1) == false

   @test Nemo.inskewdiag(SkewDiagram([4,3,1], [2]), 1, 4) == true

   @test Nemo.matrix_repr(Partition([1], false)/Partition(Int[], false)) == ones(Int, 1,1)
   @test Nemo.matrix_repr(xi) == [(Nemo.inskewdiag(xi, i,j)? 1:0) for i in 1:size(xi.lam,1), j in 1:maximum(xi.lam)]

   @test Nemo.has_left_neighbor(xi, 1, 5) == true
   @test Nemo.has_left_neighbor(xi, 1, 3) == false
   @test Nemo.has_left_neighbor(xi, 2, 3) == false
   @test Nemo.has_left_neighbor(xi, 3, 3) == false
   @test Nemo.has_left_neighbor(xi, 4, 2) == true
   @test Nemo.has_left_neighbor(xi, 5, 2) == true
   @test Nemo.has_left_neighbor(xi, 6, 1) == false
   @test Nemo.has_left_neighbor(xi, 7, 1) == false

   @test Nemo.has_bottom_neighbor(xi, 1, 5) == false
   @test Nemo.has_bottom_neighbor(xi, 1, 3) == true
   @test Nemo.has_bottom_neighbor(xi, 2, 3) == false
   @test Nemo.has_bottom_neighbor(xi, 3, 3) == false
   @test Nemo.has_bottom_neighbor(xi, 4, 2) == true
   @test Nemo.has_bottom_neighbor(xi, 5, 2) == false
   @test Nemo.has_bottom_neighbor(xi, 6, 1) == true
   @test Nemo.has_bottom_neighbor(xi, 7, 1) == false

   xi = Partition([4,3,2,1])/Partition([2,2,2,1])
   @test isrimhook(xi) == true

   println("PASS")
end

function test_rimhooks()
   print("youngtabs.rimhooks...")

   xi = Partition([2,1])/Partition(Int[], false)
   @test isrimhook(xi) == true

   xi = SkewDiagram([2,1], [1])
   @test isrimhook(xi) == false

   xi = Partition([2,2])/Partition(Int[], false)
   @test isrimhook(xi) == false

   xi = SkewDiagram([2,2], [1])
   @test isrimhook(xi) == true

   xi = SkewDiagram([4,3,1], [2,2])
   @test isrimhook(xi) == false

   xi = SkewDiagram([4,3,1], [2,1])
   @test isrimhook(xi) == false

   xi = SkewDiagram([4,3,1], [1])
   @test isrimhook(xi) == false

   xi = SkewDiagram([4,3,1], [2])
   @test isrimhook(xi) == true

   println("PASS")
end

function test_partitionseqs()
   print("youngtabs.partitionseqs...")
   @test Nemo.partitionseq(Partition([1])) == BitVector([true, false])
   @test Nemo.partitionseq([1]) == BitVector([true, false])
   @test Nemo.partitionseq(Partition([1,1])) == BitVector([true, false, false])
   @test Nemo.partitionseq(Partition([2,1])) == BitVector([true, false, true, false])

   t = Nemo.partitionseq(Partition([5,4,2,1]))
   @test t == Nemo.partitionseq([5,4,2,1])
   @test length(t) == 9
   @test t == BitVector([true, false, true, false, true, true, false, true, false])
   R = Nemo.partitionseq(t)
   R[1] = false
   R[end] = true
   @test Nemo.partitionseq(R) == BitVector([true,false,true,true,false])

   println("PASS")
end

function test_ytabs()
   test_partition_type()
   test_youngtabs()
   test_skewdiags()
   test_rimhooks()
   test_partitionseqs()
end
