###############################################################################
#
#   GenericTypes.jl : Generic Types
#
###############################################################################

###############################################################################
#
#   GenPolyRing / GenPoly
#
###############################################################################

const GenPolyID = ObjectIdDict()

type GenPolyRing{T <: RingElem} <: PolyRing{T}
   base_ring :: Ring
   S::Symbol

   function GenPolyRing(R::Ring, s::Symbol, cached=true)
      if haskey(GenPolyID, (R, s))
         return GenPolyID[R, s]::GenPolyRing{T}
      else 
         z = new{T}(R, s)
         if cached
           GenPolyID[R, s] = z
         end
         return z
      end
   end
end

type GenPoly{T <: RingElem} <: PolyElem{T}
   coeffs::Array{T, 1}
   length::Int
   parent::GenPolyRing{T}

   GenPoly() = new(Array(T, 0), 0)
   
   GenPoly(a::Array{T, 1}) = new(a, length(a))

   GenPoly(a::T) = a == 0 ? new(Array(T, 0), 0) : new([a], 1)
end

###############################################################################
#
#   GenResRing / GenRes
#
###############################################################################

const ModulusDict = Dict{Tuple{Ring, RingElem}, Ring}()

type GenResRing{T <: RingElem} <: ResRing{T}
   base_ring::Ring
   modulus::T

   function GenResRing(modulus::T, cached=true)
      if haskey(ModulusDict, (parent(modulus), modulus))
         return ModulusDict[parent(modulus), modulus]::GenResRing{T}
      else
         z = new{T}(parent(modulus), modulus)
         if cached
            ModulusDict[parent(modulus), modulus] = z
         end
         return z
      end
   end
end

type GenRes{T <: RingElem} <: ResElem{T}
   data::T
   parent::GenResRing{T}

   GenRes(a::T) = new(a)
end

###############################################################################
#
#   GenRelSeriesRing / GenRelSeries
#
###############################################################################

const GenRelSeriesID = ObjectIdDict()

type GenRelSeriesRing{T <: RingElem} <: SeriesRing{T}
   base_ring::Ring
   prec_max::Int
   S::Symbol

   function GenRelSeriesRing(R::Ring, prec::Int, s::Symbol, cached=true)
      if haskey(GenRelSeriesID, (R, prec, s))
         return GenRelSeriesID[R, prec, s]::GenRelSeriesRing{T}
      else
         z = new{T}(R, prec, s)
         if cached
            GenRelSeriesID[R, prec, s] = z
         end
         return z
      end
   end
end

type GenRelSeries{T <: RingElem} <: SeriesElem{T}
   coeffs::Array{T, 1}
   length::Int
   prec::Int
   parent::GenRelSeriesRing{T}

   GenRelSeries(a::Array{T, 1}, length::Int, prec::Int) = new(a, length, prec)   
   GenRelSeries(a::GenRelSeries{T}) = a
end

###############################################################################
#
#   GenFracField / GenFrac
#
###############################################################################

const GenFracDict = ObjectIdDict()

type GenFracField{T <: RingElem} <: FracField{T}
   base_ring::Ring

   function GenFracField(R::Ring, cached=true)
      if haskey(GenFracDict, R)
         return GenFracDict[R]::GenFracField{T}
      else
         z = new{T}(R)
         if cached
            GenFracDict[R] = z
         end
         return z
      end
   end
end

type GenFrac{T <: RingElem} <: FracElem{T}
   num::T
   den::T
   parent::GenFracField{T}

   GenFrac(num::T, den::T) = new(num, den) 
end

###############################################################################
#
#   GenMatSpace / GenMat
#
###############################################################################

const GenMatDict = ObjectIdDict()

# not really a mathematical ring
type GenMatSpace{T <: RingElem} <: MatSpace{T}
   rows::Int
   cols::Int
   base_ring::Ring

   function GenMatSpace(R::Ring, r::Int, c::Int, cached=true)
      if haskey(GenMatDict, (R, r, c))
         return GenMatDict[R, r, c]::GenMatSpace{T}
      else
         z = new{T}(r, c, R)
         if cached
            GenMatDict[R, r, c] = z
         end
         return z
      end
   end
end

type GenMat{T <: RingElem} <: MatElem{T}
   entries::Array{T, 2}
   parent::GenMatSpace{T}

   GenMat(a::Array{T, 2}) = new(a) 
end

