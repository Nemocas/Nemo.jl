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

const _noPartsTable = Dict{Int, Int}(0 => 1, 1 => 1, 2 => 2)
const _noPartsTableBig = Dict{Int, BigInt}()

doc"""
    noPartitions(n::Int)
> Returns the number of all distinct integer partitions of `n`. The function
> uses Euler pentagonal number theorem for recursive formula. For more details
> see OEIS sequence [A000041](http://oeis.org/A000041). Note that
> `noPartitions(0) = 1` by convention.
"""
function noPartitions(n::Int)
   if n < 0
      return 0
   end
   if n < 395
      lookuptable = _noPartsTable
      s = 0
   else
      lookuptable = _noPartsTableBig
      s = big(0)
   end

   if !haskey(lookuptable, n)
      for j in 1:floor(Int, (1 + sqrt(1+24n))/6)
         p1 = noPartitions(n - div(j*(3j-1),2))
         p2 = noPartitions(n - div(j*(3j+1),2))
         s += (-1)^(j-1)*(p1 + p2)
      end
      lookuptable[n] = s
   end
   return lookuptable[n]
end

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
length(parts::IntPartitions) = noPartitions(parts.n)

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
    partitionseq(λ::Partition)
> Returns a sequence (as `BitVector`) of `false`s and `trues`s constructed from
> `λ`: tracing the lower contour of the Young Diagram associated to `λ` from
> left to right a `true` is inserted for every horizontal and `false` for every
> vertical step. The sequence always starts with `true` and ends with `false`.
"""
function partitionseq(λ::Partition)
   seq = trues(maximum(λ) + length(λ))
   j = λ[end]
   for i in (length(λ)-1):-1:1
      seq[j+1] = false
      j += λ[i] - λ[i+1] + 1
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
    MN1inner(R::BitVector, μ::Partition, t::Int, [charvals])
> Returns the value of `λ`-th irreducible character on conjugacy class of
> permutations represented by partition `μ`, where `R` is the (binary)
> partition sequence representing `λ`. Values already computed are stored in
> `charvals::Dict{Tuple{BitVector,Vector{Int}}, Int}`.
> This is an implementation (with slight modifications) of the
> Murnaghan-Nakayama formula as described in
>
>     Dan Bernstein,
>     "The computational complexity of rules for the character table of Sn"
>     _Journal of Symbolic Computation_, 37(6), 2004, p. 727-748.
"""
function MN1inner(R::BitVector, μ::Partition, t::Int,
        charvals=Dict{Tuple{BitVector,Vector{Int}}, Int}())
    if t > length(μ)
        chi = 1
    elseif μ[t] > length(R)
        chi = 0
    else
        chi = 0
        sgn = false

        for j in 1:μ[t]-1
            if R[j] == false
                sgn = !sgn
            end
        end
        for i in 1:length(R)-μ[t]
            if R[i] != R[i+μ[t]-1]
                sgn = !sgn
            end
            if isrimhook(R, i, μ[t])
                R[i], R[i+μ[t]] = R[i+μ[t]], R[i]
                essR = (partitionseq(R), μ[t+1:end])
                if !haskey(charvals, essR)
                    charvals[essR] = MN1inner(R, μ, t+1, charvals)
                end
                chi += (-1)^Int(sgn)*charvals[essR]
                R[i], R[i+μ[t]] = R[i+μ[t]], R[i]
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
    SkewDiagram(λ::Partition, μ::Partition)
> Implements a skew diagram, i.e. a difference of two Young diagrams
> represented by partitions `λ` and `μ`.
"""
immutable SkewDiagram
   λ::Partition
   μ::Partition

   function SkewDiagram(λ, μ)
      sum(λ) >= sum(μ) || throw("Can't create SkewDiagram: μ is partition of  $(sum(μ)) > $(sum(λ)).")
      length(λ) >= length(μ) || throw("Can't create SkewDiagram: $μ is longer than $(λ)!")
      for (l,m) in zip(λ, μ)
         l >= m || throw("a row of $μ is longer than a row of $λ")
      end
      return new(λ, μ)
   end
end

SkewDiagram(λ::Vector{Int}, μ::Vector{Int}) = SkewDiagram(Partition(λ), Partition(μ))

/(λ::Partition, μ::Partition) = SkewDiagram(λ, μ)

==(ξ::SkewDiagram, ψ::SkewDiagram) = ξ.λ == ψ.λ && ξ.μ == ψ.μ
hash(ξ::SkewDiagram, h::UInt) = hash(ξ.λ, hash(ξ.μ, hash(SkewDiagram, h)))

###############################################################################
#
#   String I/O
#
###############################################################################

doc"""
    matrix_repr(ξ::SkewDiagram)
> Returns a binary representation of the diagram `ξ`, i.e. a binary array `A`
> where `A[i,j] == 1` if and only if `(i,j)` is in `ξ.λ` but not in `ξ.μ`.
"""
function matrix_repr(ξ::SkewDiagram)
   ydiag = zeros(Int,length(ξ.λ), maximum(ξ.λ))
   for i in 1:length(ξ.μ)
      ydiag[i, ξ.μ[i]+1:ξ.λ[i]] .= 1
   end
   for i in length(ξ.μ)+1:length(ξ.λ)
      ydiag[i,1:ξ.λ[i]] .= 1
   end
   return ydiag
end

show(io::IO, ξ::SkewDiagram) = show(io, MIME("text/plain"), matrix_repr(ξ))

##############################################################################
#
#   Misc functions for SkewDiagrams
#
##############################################################################

doc"""
    inskewdiag(ξ::SkewDiagram, i::Int, j::Int)
> Checks if box at position `(i,j)` belongs to the skew diagram `ξ`.
"""
function inskewdiag(ξ::SkewDiagram, i::Int, j::Int)
   if i <= 0 || j <= 0
      return false
   elseif i > length(ξ.λ) || j > maximum(ξ.λ)
      return false
   elseif length(ξ.μ) >= i
      return ξ.μ[i] < j ≤ ξ.λ[i]
   else
      return j ≤ ξ.λ[i]
   end
end

doc"""
    haslneigh(ξ::SkewDiagram, i::Int, j::Int)
> Checks if box at position `(i,j)` has neighbour in `ξ` to the left.
"""
function haslneigh(ξ::SkewDiagram, i::Int, j::Int)
   if j == 1
      return false
   else
      return inskewdiag(ξ, i, j) && inskewdiag(ξ, i, j-1)
   end
end

doc"""
    hasdneig(ξ::SkewDiagram, i::Int, j::Int)
> Checks if box at position `(i,j)` has neighbour in `ξ` below.
"""
function hasdneigh(ξ::SkewDiagram, i::Int, j::Int)
   if i == length(ξ.λ)
      return false
   else
      return inskewdiag(ξ, i, j) && inskewdiag(ξ, i+1, j)
   end
end

doc"""
    isrimhook(ξ::SkewDiagram)
> Checks if `ξ` represents a rim-hook diagram, i.e. its diagram is
> edge-connected and contains no $2\times 2$ squares.
"""
function isrimhook(ξ::SkewDiagram)
   i = 1
   j = ξ.λ[1]
   while i ≠ length(ξ.λ) && j ≠ 1
      left = haslneigh(ξ, i,j)
      down = hasdneigh(ξ, i,j)
      if left && down # there is 2×2 square in ξ
         return false
      elseif left
         j -= 1
      elseif down
         i += 1
      else # ξ is disconnected
         return false
      end
   end
   return true
end

doc"""
    leglength(ξ::SkewDiagram, check=true)
> Computes the leglength of a rim-hook `ξ`, i.e. the number of rows with
> non-zero entries minus one. If `check` is `false` function will not check
> whether `ξ` is actually a rim-hook.
"""
function leglength(ξ::SkewDiagram, check=true)
   if check
      isrimhook(ξ) || throw("$ξ is not a rimhook. leglength is defined only for rim hooks")
   end
   m = zeros(length(ξ.λ))
   m[1:length(ξ.μ)] = ξ.μ
   return sum((ξ.λ .- m) .> 0)
end

export Partition, IntPartitions, YoungTableau, SkewDiagram
export dim, noPartitions, isrimhook, hooklength, leglength
