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

function test_intpartitions()
   print("youngtabs.intpartitions...")
   prts =  collect(IntPartitions(4))
   @test length(prts) == 5
   @test all(sum.(prts) .== 4)

   prts = collect(IntPartitions(1))
   @test prts == [[1]]

   @test length(collect(IntPartitions(0))) == 1

   @test length(IntPartitions(10)) == 42
   @test all(sum.([p for p in IntPartitions(10)]) .== 10)

   println("PASS")
end

function test_youngtabs()
   print("youngtabs.YoungTableau...")

   λ = [4,3,1]
   Y = YoungTableau(λ)

   for i in 1:sum(λ)
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

   println("PASS")
end

function test_auts()
   print("youngtabs.automorphisms...")

   λ = [4,3,1]
   Y = YoungTableau(λ)

   @test length(colauts(Y)) == 4
   @test length(rowauts(Y)) == 3
   @test length(colauts(Y)[1]) == 6
   @test length(rowauts(Y)[1]) == 24

   @test length(automorphisms(Y, :rows)) == 144
   @test length(automorphisms(Y, :columns)) == 24

   println("PASS")
end


function test_skewdiags()
   print("youngtabs.skewdiags...")

   l = [5,3,2,2,2,1,1]
   m = [2,2,1]
   λ = Partition(l)
   μ = Partition(m)
   ξ = λ/μ
   @test isa(ξ, SkewDiagram)
   ψ = SkewDiagram(l,m)
   @test isa(ψ, SkewDiagram)
   @test ξ == ψ

   # 7×5 Array{Int64,2}:
   #  0  0  1  1  1
   #  0  0  1  0  0
   #  0  1  0  0  0
   #  1  1  0  0  0
   #  1  1  0  0  0
   #  1  0  0  0  0
   #  1  0  0  0  0

   @test Nemo.inskewdiag(ξ, 1, 5) == true
   @test Nemo.inskewdiag(ξ, 1, 3) == true
   @test Nemo.inskewdiag(ξ, 2, 3) == true
   @test Nemo.inskewdiag(ξ, 2, 2) == false
   @test Nemo.inskewdiag(ξ, 5, 2) == true
   @test Nemo.inskewdiag(ξ, 6, 2) == false

   @test Nemo.inskewdiag(ξ, 8, 9) == false
   @test Nemo.inskewdiag(ξ, -1, 1) == false
   @test Nemo.inskewdiag(ξ, 1, -1) == false
   @test Nemo.inskewdiag(ξ, -1, -1) == false

   @test Nemo.inskewdiag(SkewDiagram([4,3,1], [2]), 1, 4) == true

   @test Nemo.matrix_repr(Partition([1], false)/Partition(Int[], false)) == ones(Int, 1,1)
   @test Nemo.matrix_repr(ξ) == [(Nemo.inskewdiag(ξ, i,j)? 1:0) for i in 1:size(ξ.λ,1), j in 1:maximum(ξ.λ)]

   @test Nemo.haslneigh(ξ, 1, 5) == true
   @test Nemo.haslneigh(ξ, 1, 3) == false
   @test Nemo.haslneigh(ξ, 2, 3) == false
   @test Nemo.haslneigh(ξ, 3, 3) == false
   @test Nemo.haslneigh(ξ, 4, 2) == true
   @test Nemo.haslneigh(ξ, 5, 2) == true
   @test Nemo.haslneigh(ξ, 6, 1) == false
   @test Nemo.haslneigh(ξ, 7, 1) == false

   @test Nemo.hasdneigh(ξ, 1, 5) == false
   @test Nemo.hasdneigh(ξ, 1, 3) == true
   @test Nemo.hasdneigh(ξ, 2, 3) == false
   @test Nemo.hasdneigh(ξ, 3, 3) == false
   @test Nemo.hasdneigh(ξ, 4, 2) == true
   @test Nemo.hasdneigh(ξ, 5, 2) == false
   @test Nemo.hasdneigh(ξ, 6, 1) == true
   @test Nemo.hasdneigh(ξ, 7, 1) == false
   println("PASS")
end

function test_rimhooks()
   print("youngtabs.rimhooks...")

   ξ = Partition([2,1])/Partition(Int[], false)
   @test isrimhook(ξ) == true

   ξ = SkewDiagram([2,1], [1])
   @test isrimhook(ξ) == false

   ξ = Partition([2,2])/Partition(Int[], false)
   @test isrimhook(ξ) == false

   ξ = SkewDiagram([2,2], [1])
   @test isrimhook(ξ) == true

   ξ = SkewDiagram([4,3,1], [2,2])
   @test isrimhook(ξ) == false

   ξ = SkewDiagram([4,3,1], [2,1])
   @test isrimhook(ξ) == false

   ξ = SkewDiagram([4,3,1], [1])
   @test isrimhook(ξ) == false

   ξ = SkewDiagram([4,3,1], [2])
   @test isrimhook(ξ) == true

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
   test_intpartitions()
   test_youngtabs()
   # test_auts()
   test_skewdiags()
   test_rimhooks()
   test_partitionseqs()
end
