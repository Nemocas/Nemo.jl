##############################################################################
#
#   Partition type, AbstractVector interface
#
##############################################################################

doc"""
    Partition(part::Vector{Int}, check=true)
> Partition represents integer partition into numbers in non-increasing order.
> It is a thin wrapper over `Vector{Int}`
"""
immutable Partition <: AbstractVector{Int}
   part::Vector{Int}

   function Partition(part::Vector{Int}, check=true)
      if check
         all(diff(part) .<= 0) || throw("Partition must be decreasing!")
         if length(part) > 0
            part[end] >=1 || throw("Found non-positive entry in partition!")
         end
      end
      return new(part)
   end
end

length(p::Partition) = length(p.part)
size(p::Partition) = size(p.part)
Base.linearindexing{T<:Partition}(::Type{T}) = Base.LinearFast()
getindex(p::Partition, i::Int) = p.part[i]
function setindex!(p::Partition, v::Int, i::Int)
   prev = Inf
   nex = 1
   if i == length(p)
      prev = p[i-1]
   elseif i == 1
      nex = p[2]
   elseif 1 < i < length(p)
      prev = p[i-1]
      nex = p[i+1]
   end
   nex <= v <= prev || throw("Partition must be positive and non-increasing")
   p.part[i] = v
   return p
end

==(p::Partition, m::Partition) = p.part == m.part
hash(p::Partition, h::UInt) = hash(p.part, hash(Partition, h))

convert(::Type{Partition}, p::Vector{Int}) = Partition(p)

##############################################################################
#
#   Iterator interface for Integer Partitions
#
##############################################################################

# Implemented following RuleAsc (Algorithm 3.1) from
#    "Generating All Partitions: A Comparison Of Two Encodings"
# by Jerome Kelleher and Barry O’Sullivan, ArXiv:0909.2331

doc"""
   IntPartitions(n::Int)
> Returns an iterator over all integer `Partition`s of `n`. They come in
> ascending order. See also `Combinatorics.partitions(n)`.
"""
immutable IntPartitions
    n::Int
end

function Base.start(parts::IntPartitions)
    if parts.n < 1
        return (Int[], 0)
    elseif parts.n == 1
        return ([1], 0)
    else
        p = zeros(Int, parts.n)
        p[2] = parts.n
        return (p, 2)
    end
end

Base.next(parts::IntPartitions, state) = nextpart_asc(state...)
Base.done(parts::IntPartitions, state) = state[2] == 1
Base.eltype(::Type{IntPartitions}) = Partition
length(parts::IntPartitions) = numpart(parts.n)

function nextpart_asc(part, k)
    if k == 0
        return Partition(part, false), (part, 1)
    end
    y = part[k] - 1
    k -= 1
    x = part[k] + 1
    while x <= y
        part[k] = x
        y -= x
        k += 1
    end
    part[k] = x + y
    return Partition(reverse(part[1:k]), false), (part, k)
end

doc"""
    conj(part::Partition)
> Returns the conjugated partition of `part`, i.e. the partition corresponding
> to the Young tableau of `part` reflected through the main diagonal.
"""
function conj(part::Partition)
    p = Int[]
    for i in 1:sum(part)
        n = sum(part .>= i)
        n == 0 && break
        push!(p, n)
    end
    return Partition(p, false)
end

##############################################################################
#
#   Partition sequences and Murnaghan-Nakayama formula
#
##############################################################################

doc"""
    partitionseq(lambda::Partition)
> Returns a sequence (as `BitVector`) of `false`s and `trues`s constructed from
> `lambda`: tracing the lower contour of the Young Diagram associated to `lambda` from
> left to right a `true` is inserted for every horizontal and `false` for every
> vertical step. The sequence always starts with `true` and ends with `false`.
"""
function partitionseq(lambda::Partition)
   seq = trues(maximum(lambda) + length(lambda))
   j = lambda[end]
   for i in (length(lambda)-1):-1:1
      seq[j+1] = false
      j += lambda[i] - lambda[i+1] + 1
   end
   seq[j+1] = false
   return seq
end

partitionseq(v::Vector{Int}) = partitionseq(Partition(v))

doc"""
    partitionseq(seq::BitVector)
> Returns the essential part of the sequence `seq`, i.e. a subsequence starting
> at first `true` and ending at last `false`.
"""
partitionseq(seq::BitVector) = seq[findfirst(seq, true):findlast(seq, false)]

doc"""
    isrimhook(R::BitVector, idx::Int, len::Int)
> `R[idx:idx+len]` forms a rim hook in the Young Diagram of parition
> corresponding to `R` iff `R[idx] == true` and `R[idx+len] == false`.
"""
function isrimhook(R::BitVector, idx::Int, len::Int)
   return (R[idx+len] == false) && (R[idx] == true)
end

const _charvalsTable = Dict{Tuple{BitVector,Vector{Int}}, Int}()

doc"""
    MN1inner(R::BitVector, mu::Partition, t::Int, [charvals])
> Returns the value of $\lambda$-th irreducible character on conjugacy class of
> permutations represented by partition `mu`, where `R` is the (binary)
> partition sequence representing $\lambda$. Values already computed are stored in
> `charvals::Dict{Tuple{BitVector,Vector{Int}}, Int}`.
> This is an implementation (with slight modifications) of the
> Murnaghan-Nakayama formula as described in
>
>     Dan Bernstein,
>     "The computational complexity of rules for the character table of Sn"
>     _Journal of Symbolic Computation_, 37(6), 2004, p. 727-748.
"""
function MN1inner(R::BitVector, mu::Partition, t::Int,
        charvals=Dict{Tuple{BitVector,Vector{Int}}, Int}())
    if t > length(mu)
        chi = 1
    elseif mu[t] > length(R)
        chi = 0
    else
        chi = 0
        sgn = false

        for j in 1:mu[t]-1
            if R[j] == false
                sgn = !sgn
            end
        end
        for i in 1:length(R)-mu[t]
            if R[i] != R[i+mu[t]-1]
                sgn = !sgn
            end
            if isrimhook(R, i, mu[t])
                R[i], R[i+mu[t]] = R[i+mu[t]], R[i]
                essR = (partitionseq(R), mu[t+1:end])
                if !haskey(charvals, essR)
                    charvals[essR] = MN1inner(R, mu, t+1, charvals)
                end
                chi += (-1)^Int(sgn)*charvals[essR]
                R[i], R[i+mu[t]] = R[i+mu[t]], R[i]
            end
        end
    end
    return chi
end

##############################################################################
#
#   YoungTableau type, AbstractVector interface
#
##############################################################################

doc"""
    YoungTableau(part::Partition, fill::Vector{Int}=collect(1:sum(part)))
> Returns the Young tableaux of partition `part`, filled linearly (row-major)
> by `fill` vector.
"""
immutable YoungTableau <: AbstractArray{Int, 2}
   n::Int
   part::Partition
   tab::Array{Int,2}
end

function YoungTableau(part::Partition, fill=collect(1:sum(part)))
   sum(part) == length(fill) || throw("Can't fill Young digaram of $part with $fill: different number of elemnets.")
   n = sum(part)
   tab = zeros(Int, length(part), maximum(part))
   k=1
   for (idx, p) in enumerate(part)
      tab[idx, 1:p] = fill[k:k+p-1]
      k += p
   end
   return YoungTableau(n, part, tab)
end

YoungTableau(p::Vector{Int}) = YoungTableau(Partition(p))

size(Y::YoungTableau) = size(Y.tab)
Base.linearindexing{T<:YoungTableau}(::Type{T}) = Base.LinearFast()
getindex(Y::YoungTableau, i::Int) = Y.tab[i]

function ==(Y1::YoungTableau,Y2::YoungTableau)
   Y1.n == Y2.n || return false
   Y1.part == Y2.part || return false
   Y1.tab == Y2.tab || return false
   return true
end

hash(Y::YoungTableau, h::UInt) = hash(Y.n, hash(Y.part, hash(Y.tab, hash(YoungTableau, h))))

doc"""
    conj(Y::YoungTableau)
> Returns the conjugated tableau, i.e. the tableau reflected through the main
> diagonal.
"""
conj(Y::YoungTableau) = YoungTableau(Y.n, conj(Y.part), transpose(Y.tab))

##############################################################################
#
#   Misc functions for YoungTableaux
#
##############################################################################

rowlen(Y::YoungTableau, i, j) = sum(Y[i, j:end] .> 0)
collen(Y::YoungTableau, i, j) = sum(Y[i:end, j] .> 0)

doc"""
    hooklength(Y::YoungTableau, i, j)
> Returns the hooklength of an element in `Y` at position `(i,j)`. `hooklength`
> will return `0` for `(i,j)` not in the tableau `Y`.
"""
function hooklength(Y::YoungTableau, i, j)
   if Y[i,j] == 0
      return 0
   else
      return rowlen(Y, i, j) + collen(Y, i, j) - 1
   end
end

doc"""
    dim(Y::YoungTableau)
> Returns the dimension of the irreducible representation of
> `PermutationGroup(sum(Y))` associated to `Y`.
"""
function dim(Y::YoungTableau)
   n, m = size(Y)
   num = factorial(maximum(Y))
   den = reduce(*, 1, hooklength(Y,i,j) for i in 1:n, j in 1:m if j <= Y.part[i])
   return Int(num/den)
end

##############################################################################
#
#   SkewDiagrams
#
##############################################################################

doc"""
    SkewDiagram(lambda::Partition, mu::Partition)
> Implements a skew diagram, i.e. a difference of two Young diagrams
> represented by partitions `lambda` and `mu`.
"""
immutable SkewDiagram
   lam::Partition
   mu::Partition

   function SkewDiagram(lambda, mu)
      sum(lambda) >= sum(mu) || throw("Can't create SkewDiagram: $mu is partition of  $(sum(mu)) > $(sum(lambda)).")
      length(lambda) >= length(mu) || throw("Can't create SkewDiagram: $mu is longer than $(lambda)!")
      for (l, m) in zip(lambda, mu)
         l >= m || throw("a row of $mu is longer than a row of $lambda")
      end
      return new(lambda, mu)
   end
end

SkewDiagram(lambda::Vector{Int}, mu::Vector{Int}) = SkewDiagram(Partition(lambda), Partition(mu))

/(lambda::Partition, mu::Partition) = SkewDiagram(lambda, mu)

==(xi::SkewDiagram, psi::SkewDiagram) = xi.lam == psi.lam && xi.mu == psi.mu
hash(xi::SkewDiagram, h::UInt) = hash(xi.lam, hash(xi.mu, hash(SkewDiagram, h)))

###############################################################################
#
#   String I/O
#
###############################################################################

doc"""
    matrix_repr(xi::SkewDiagram)
> Returns a binary representation of the diagram `xi`, i.e. a binary array `A`
> where `A[i,j] == 1` if and only if `(i,j)` is in `xi.lam` but not in `xi.mu`.
"""
function matrix_repr(xi::SkewDiagram)
   ydiag = zeros(Int,length(xi.lam), maximum(xi.lam))
   for i in 1:length(xi.mu)
      ydiag[i, xi.mu[i]+1:xi.lam[i]] .= 1
   end
   for i in length(xi.mu)+1:length(xi.lam)
      ydiag[i,1:xi.lam[i]] .= 1
   end
   return ydiag
end

show(io::IO, xi::SkewDiagram) = show(io, MIME("text/plain"), matrix_repr(xi))

##############################################################################
#
#   Misc functions for SkewDiagrams
#
##############################################################################

doc"""
    inskewdiag(xi::SkewDiagram, i::Int, j::Int)
> Checks if box at position `(i,j)` belongs to the skew diagram `xi`.
"""
function inskewdiag(xi::SkewDiagram, i::Int, j::Int)
   if i <= 0 || j <= 0
      return false
   elseif i > length(xi.lam) || j > maximum(xi.lam)
      return false
   elseif length(xi.mu) >= i
      return xi.mu[i] < j ≤ xi.lam[i]
   else
      return j ≤ xi.lam[i]
   end
end

doc"""
    haslneigh(xi::SkewDiagram, i::Int, j::Int)
> Checks if box at position `(i,j)` has neighbour in `xi` to the left.
"""
function haslneigh(xi::SkewDiagram, i::Int, j::Int)
   if j == 1
      return false
   else
      return inskewdiag(xi, i, j) && inskewdiag(xi, i, j-1)
   end
end

doc"""
    hasdneigh(xi::SkewDiagram, i::Int, j::Int)
> Checks if box at position `(i,j)` has neighbour in `xi` below.
"""
function hasdneigh(xi::SkewDiagram, i::Int, j::Int)
   if i == length(xi.lam)
      return false
   else
      return inskewdiag(xi, i, j) && inskewdiag(xi, i+1, j)
   end
end

doc"""
    isrimhook(xi::SkewDiagram)
> Checks if `xi` represents a rim-hook diagram, i.e. its diagram is
> edge-connected and contains no $2\times 2$ squares.
"""
function isrimhook(xi::SkewDiagram)
   i = 1
   j = xi.lam[1]
   while i ≠ length(xi.lam) && j ≠ 1
      left = haslneigh(xi, i,j)
      down = hasdneigh(xi, i,j)
      if left && down # there is 2×2 square in xi
         return false
      elseif left
         j -= 1
      elseif down
         i += 1
      else # xi is disconnected
         return false
      end
   end
   return true
end

doc"""
    leglength(xi::SkewDiagram, check=true)
> Computes the leglength of a rim-hook `xi`, i.e. the number of rows with
> non-zero entries minus one. If `check` is `false` function will not check
> whether `xi` is actually a rim-hook.
"""
function leglength(xi::SkewDiagram, check=true)
   if check
      isrimhook(xi) || throw("$xi is not a rimhook. leglength is defined only for rim hooks")
   end
   m = zeros(length(xi.lam))
   m[1:length(xi.mu)] = xi.mu
   return sum((xi.lam .- m) .> 0) - 1
end

export Partition, IntPartitions, YoungTableau, SkewDiagram
export dim, noPartitions, isrimhook, hooklength, leglength
