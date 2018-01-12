###############################################################################
#
#   GenericTypes.jl : Generic Types
#
###############################################################################

###############################################################################
#
#   PermGroup / perm
#
###############################################################################

const PermID = ObjectIdDict()

mutable struct PermGroup <: Nemo.Group
   n::Int

   function PermGroup(n::Int, cached=true)
      if haskey(PermID, n)
         return PermID[n]::PermGroup
      else
         z = new(n)
         if cached
            PermID[n] = z
         end
         return z
      end
   end
end

mutable struct perm <: Nemo.GroupElem
   d::Array{Int, 1}
   cycles::Vector{Vector{Int}}
   parent::PermGroup

   function perm(n::Int)
      return new(collect(1:n))
   end

   function perm(a::Array{Int, 1})
      return new(a)
   end
end

doc"""
    AllPerms(n::Int)
> Returns an iterator over arrays representing all permutations of `1:n`.
> Similar to `Combinatorics.permutations(1:n)`
"""
struct AllPerms
   n::Int
   all::Int
   AllPerms(n::Int) = new(n, factorial(n))
end

###############################################################################
#
#   Partition
#
###############################################################################

doc"""
    Partition(part::Vector{Int}, check::Bool=true)
> Partition represents integer partition into numbers in non-increasing order.
> It is a thin wrapper over `Vector{Int}`
"""
mutable struct Partition <: AbstractVector{Int}
   n::Int
   part::Vector{Int}

   function Partition(part::Vector{Int}, check::Bool=true)
      if check
         all(diff(part) .<= 0) || throw("Partition must be decreasing!")
         if length(part) > 0
            part[end] >=1 || throw("Found non-positive entry in partition!")
         end
      end
      return new(sum(part), part)
   end
end

doc"""
    AllParts(n::Int)
> Returns an iterator over all integer `Partition`s of `n`. They come in
> ascending order. See also `Combinatorics.partitions(n)`.
"""
struct AllParts{T}
    n::T
end

###############################################################################
#
#   SkewDiagram
#
###############################################################################

doc"""
    SkewDiagram(lambda::Partition, mu::Partition)
> Implements a skew diagram, i.e. a difference of two Young diagrams
> represented by partitions `lambda` and `mu`.
"""
struct SkewDiagram
   lam::Partition
   mu::Partition

   function SkewDiagram(lambda, mu)
      lambda.n >= mu.n || throw("Can't create SkewDiagram: $mu is partition of  $(mu.n) > $(lambda.n).")
      length(lambda) >= length(mu) || throw("Can't create SkewDiagram: $mu is longer than $(lambda)!")
      for (l, m) in zip(lambda, mu)
         l >= m || throw("a row of $mu is longer than a row of $lambda")
      end
      return new(lambda, mu)
   end
end

###############################################################################
#
#   Young Tableau
#
###############################################################################

doc"""
    YoungTableau(part::Partition, tab::Array{Int, 2})
> Returns the Young tableaux of partition `part` of `n`, filled linearly
> (row-major) by `fill` vector.
"""
struct YoungTableau <: AbstractArray{Int, 2}
   part::Partition
   tab::Array{Int,2}
end

###############################################################################
#
#   PolyRing / Poly
#
###############################################################################

mutable struct PolyRing{T <: RingElement} <: Nemo.PolyRing{T}
   base_ring::Ring
   S::Symbol

   function PolyRing{T}(R::Ring, s::Symbol, cached::Bool = true) where T <: RingElement
      if haskey(PolyID, (R, s))
         return PolyID[R, s]::PolyRing{T}
      else
         z = new{T}(R, s)
         if cached
           PolyID[R, s] = z
         end
         return z
      end
   end
end

const PolyID = Dict{Tuple{Ring, Symbol}, Ring}()

mutable struct Poly{T <: RingElement} <: Nemo.PolyElem{T}
   coeffs::Array{T, 1}
   length::Int
   parent::PolyRing{T}

   Poly{T}() where T <: RingElement = new{T}(Array{T}(0), 0)

   function Poly{T}(b::Array{T, 1}) where T <: RingElement
      z = new{T}(b)
      z.length = normalise(z, length(b))
      return z
   end

   Poly{T}(a::T) where T <: RingElement = iszero(a) ? new{T}(Array{T}(0), 0) : new{T}([a], 1)
end

###############################################################################
#
#   MPolyRing / MPoly
#
###############################################################################

# S is a Symbol which can take the values:
# :lex
# :revlex
# :deglex
# :degrevlex
#
# T is an Int which is the number of variables
# (plus one if ordered by total degree)

mutable struct MPolyRing{T <: RingElement} <: Nemo.MPolyRing{T}
   base_ring::Ring
   S::Array{Symbol, 1}
   ord::Symbol
   num_vars::Int
   N::Int

   function MPolyRing{T}(R::Ring, s::Array{Symbol, 1}, ord::Symbol, N::Int,
                         cached::Bool = true) where T <: RingElement
      if haskey(MPolyID, (R, s, ord, N))
         return MPolyID[R, s, ord, N]::MPolyRing{T}
      else
         z = new{T}(R, s, ord, length(s), N)
         if cached
           MPolyID[R, s, ord, N] = z
         end
         return z
      end
   end
end

const MPolyID = Dict{Tuple{Ring, Array{Symbol, 1}, Symbol, Int}, Ring}()

mutable struct MPoly{T <: RingElement} <: Nemo.MPolyElem{T}
   coeffs::Array{T, 1}
   exps::Array{UInt, 2}
   length::Int
   parent::MPolyRing{T}

   function MPoly{T}(R::MPolyRing) where T <: RingElement
      N = R.N
      return new{T}(Array{T}(0), Array{UInt}(N, 0), 0, R)
   end

   MPoly{T}(R::MPolyRing, a::Array{T, 1}, b::Array{UInt, 2}) where T <: RingElement = new{T}(a, b, length(a), R)

   function MPoly{T}(R::MPolyRing, a::T) where T <: RingElement
      N = R.N
      return iszero(a) ? new{T}(Array{T}(0), Array{UInt}(N, 0), 0, R) :
                                          new{T}([a], zeros(UInt, N, 1), 1, R)
   end
end

###############################################################################
#
#   SparsePolyRing / SparsePoly
#
###############################################################################

mutable struct SparsePolyRing{T <: RingElement} <: Nemo.Ring
   base_ring::Ring
   S::Symbol
   num_vars::Int

   function SparsePolyRing{T}(R::Ring, s::Symbol, cached::Bool = true) where T <: RingElement
      if haskey(SparsePolyID, (R, s))
         return SparsePolyID[R, s]::SparsePolyRing{T}
      else
         z = new{T}(R, s)
         if cached
           SparsePolyID[R, s] = z
         end
         return z
      end
   end
end

const SparsePolyID = Dict{Tuple{Ring, Symbol}, SparsePolyRing}()

mutable struct SparsePoly{T <: RingElement} <: Nemo.RingElem
   coeffs::Array{T, 1}
   exps::Array{UInt}
   length::Int
   parent::SparsePolyRing{T}

   SparsePoly{T}() where T <: RingElement = new{T}(Array{T}(0), Array{UInt}(0), 0)

   SparsePoly{T}(a::Array{T, 1}, b::Array{UInt, 1}) where T <: RingElement = new{T}(a, b, length(a))

   SparsePoly{T}(a::T) where T <: RingElement = iszero(a) ? new{T}(Array{T}(0), Array{UInt}(0), 0) :
                                               new{T}([a], [UInt(0)], 1)
end

###############################################################################
#
#   ResRing / Res
#
###############################################################################

mutable struct ResRing{T <: RingElement} <: Nemo.ResRing{T}
   base_ring::Ring
   modulus::T

   function ResRing{T}(modulus::T, cached::Bool = true) where T <: RingElement
      c = canonical_unit(modulus)
      if !isone(c)
        modulus = divexact(modulus, c)
      end
      if haskey(ModulusDict, (parent(modulus), modulus))
         return ModulusDict[parent(modulus), modulus]::ResRing{T}
      else
         z = new{T}(parent(modulus), modulus)
         if cached
            ModulusDict[parent(modulus), modulus] = z
         end
         return z
      end
   end
end

const ModulusDict = Dict{Tuple{Ring, RingElement}, Ring}()

mutable struct Res{T <: RingElement} <: Nemo.ResElem{T}
   data::T
   parent::ResRing{T}

   Res{T}(a::T) where T <: RingElement = new{T}(a)
end

###############################################################################
#
#   RelSeriesRing / RelSeries
#
###############################################################################

mutable struct RelSeriesRing{T <: RingElement} <: Nemo.SeriesRing{T}
   base_ring::Ring
   prec_max::Int
   S::Symbol

   function RelSeriesRing{T}(R::Ring, prec::Int, s::Symbol, cached::Bool = true) where T <: RingElement
      if haskey(RelSeriesID, (R, prec, s))
         return RelSeriesID[R, prec, s]::RelSeriesRing{T}
      else
         z = new{T}(R, prec, s)
         if cached
            RelSeriesID[R, prec, s] = z
         end
         return z
      end
   end
end

const RelSeriesID = Dict{Tuple{Ring, Int, Symbol}, Ring}()

mutable struct RelSeries{T <: RingElement} <: Nemo.RelSeriesElem{T}
   coeffs::Array{T, 1}
   length::Int
   prec::Int
   val::Int
   parent::RelSeriesRing{T}

   function RelSeries{T}(a::Array{T, 1}, length::Int, prec::Int, val::Int) where T <: RingElement
      new{T}(a, length, prec, val)
   end

   RelSeries{T}(a::RelSeries{T}) where T <: RingElement = a
end

###############################################################################
#
#   AbsSeriesRing / AbsSeries
#
###############################################################################

mutable struct AbsSeriesRing{T <: RingElement} <: Nemo.SeriesRing{T}
   base_ring::Ring
   prec_max::Int
   S::Symbol

   function AbsSeriesRing{T}(R::Ring, prec::Int, s::Symbol, cached::Bool = true) where T <: RingElement
      if haskey(AbsSeriesID, (R, prec, s))
         return AbsSeriesID[R, prec, s]::AbsSeriesRing{T}
      else
         z = new{T}(R, prec, s)
         if cached
            AbsSeriesID[R, prec, s] = z
         end
         return z
      end
   end
end

const AbsSeriesID = Dict{Tuple{Ring, Int, Symbol}, Ring}()

mutable struct AbsSeries{T <: RingElement} <: Nemo.AbsSeriesElem{T}
   coeffs::Array{T, 1}
   length::Int
   prec::Int
   parent::AbsSeriesRing{T}

   AbsSeries{T}(a::Array{T, 1}, length::Int, prec::Int) where T <: RingElement = new{T}(a, length, prec)
   AbsSeries{T}(a::AbsSeries{T}) where T <: RingElement = a
end

###############################################################################
#
#   LaurentSeriesRing / LarentSeriesRingElem
#
###############################################################################

mutable struct LaurentSeriesRing{T <: RingElement} <: Nemo.Ring
   base_ring::Ring
   prec_max::Int
   S::Symbol

   function LaurentSeriesRing{T}(R::Ring, prec::Int, s::Symbol, cached::Bool = true) where T <: RingElement
      if haskey(LaurentSeriesID, (R, prec, s))
         return LaurentSeriesID[R, prec, s]::LaurentSeriesRing{T}
      else
         z = new{T}(R, prec, s)
         if cached
            LaurentSeriesID[R, prec, s] = z
         end
         return z
      end
   end
end

const LaurentSeriesID = Dict{Tuple{Ring, Int, Symbol}, Ring}()

mutable struct LaurentSeriesRingElem{T <: RingElement} <: Nemo.RingElem
   coeffs::Array{T, 1}
   length::Int
   prec::Int
   val::Int
   parent::LaurentSeriesRing{T}

   function LaurentSeriesRingElem{T}(a::Array{T, 1}, length::Int, prec::Int, val::Int) where T <: RingElement
      new{T}(a, length, prec, val)
   end

   LaurentSeriesRingElem{T}(a::LaurentSeriesRingElem{T}) where T <: RingElement = a
end

###############################################################################
#
#   LaurentSeriesField / LarentSeriesFieldElem
#
###############################################################################

mutable struct LaurentSeriesField{T <: FieldElement} <: Nemo.Field
   base_ring::Field
   prec_max::Int
   S::Symbol

   function LaurentSeriesField{T}(R::Field, prec::Int, s::Symbol, cached::Bool = true) where T <: FieldElement
      if haskey(LaurentSeriesID, (R, prec, s))
         return LaurentSeriesID[R, prec, s]::LaurentSeriesField{T}
      else
         z = new{T}(R, prec, s)
         if cached
            LaurentSeriesID[R, prec, s] = z
         end
         return z
      end
   end
end

mutable struct LaurentSeriesFieldElem{T <: FieldElement} <: Nemo.FieldElem
   coeffs::Array{T, 1}
   length::Int
   prec::Int
   val::Int
   parent::LaurentSeriesField{T}

   function LaurentSeriesFieldElem{T}(a::Array{T, 1}, length::Int, prec::Int, val::Int) where T <: FieldElement
      new{T}(a, length, prec, val)
   end

   LaurentSeriesFieldElem{T}(a::LaurentSeriesFieldElem{T}) where T <: FieldElement = a
end

const LaurentSeriesElem{T} = Union{LaurentSeriesRingElem{T}, LaurentSeriesFieldElem{T}} where T <: RingElement

###############################################################################
#
#   FracField / Frac
#
###############################################################################

mutable struct FracField{T <: RingElem} <: Nemo.FracField{T}
   base_ring::Ring

   function FracField{T}(R::Ring, cached::Bool = true) where T <: RingElem
      if haskey(FracDict, R)
         return FracDict[R]::FracField{T}
      else
         z = new{T}(R)
         if cached
            FracDict[R] = z
         end
         return z
      end
   end
end

const FracDict = Dict{Ring, Ring}()

mutable struct Frac{T <: RingElem} <: Nemo.FracElem{T}
   num::T
   den::T
   parent::FracField{T}

   Frac{T}(num::T, den::T) where T <: RingElem = new{T}(num, den)
end

###############################################################################
#
#   MatSpace / Mat
#
###############################################################################

# not really a mathematical ring
mutable struct MatSpace{T <: RingElement} <: Nemo.MatSpace{T}
   rows::Int
   cols::Int
   base_ring::Ring

   function MatSpace{T}(R::Ring, r::Int, c::Int, cached::Bool = true) where T <: RingElement
      if haskey(MatDict, (R, r, c))
         return MatDict[R, r, c]::MatSpace{T}
      else
         z = new{T}(r, c, R)
         if cached
            MatDict[R, r, c] = z
         end
         return z
      end
   end
end

const MatDict = Dict{Tuple{Ring, Int, Int}, Ring}()

mutable struct Mat{T <: RingElement} <: Nemo.MatElem{T}
   entries::Array{T, 2}
   base_ring::Ring

   function Mat{T}(A::Array{T, 2}) where T <: RingElement
      return new{T}(A)
   end

   function Mat{T}(r::Int, c::Int, A::Array{T, 1}) where T <: RingElement
      t = Array{T}(r, c)
      for i = 1:r
         for j = 1:c
            t[i, j] = A[(i - 1) * c + j]
         end
      end
      return new{T}(t)
   end
end
