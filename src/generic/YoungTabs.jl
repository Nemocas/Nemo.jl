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
