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
linearindexing{T<:Partition}(::Type{T}) = Base.LinearFast()
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

function start(parts::IntPartitions)
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

next(parts::IntPartitions, state) = nextpart_asc(state...)
done(parts::IntPartitions, state) = state[2] == 1
eltype(::Type{IntPartitions}) = Partition
length(parts::IntPartitions) = noPartitions(parts.n)
