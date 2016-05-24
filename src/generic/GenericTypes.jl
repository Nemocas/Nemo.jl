###############################################################################
#
#   GenericTypes.jl : Generic Types
#
###############################################################################

###############################################################################
#
#   GenPolynomialRing / GenPoly
#
###############################################################################

const GenPolyID = ObjectIdDict()

type GenPolynomialRing{T <: RingElem} <: PolyRing{T}
   base_ring :: Ring
   S::Symbol

   function GenPolynomialRing(R::Ring, s::Symbol, cached=true)
      if haskey(GenPolyID, (R, s))
         return GenPolyID[R, s]::GenPolynomialRing{T}
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
   parent::GenPolynomialRing{T}

   GenPoly() = new(Array(T, 0), 0)
   
   GenPoly(a::Array{T, 1}) = new(a, length(a))

   GenPoly(a::T) = a == 0 ? new(Array(T, 0), 0) : new([a], 1)
end

###############################################################################
#
#   GenResidueRing / GenResidue
#
###############################################################################

const ModulusDict = Dict{Tuple{Ring, RingElem}, Ring}()

type GenResidueRing{T <: RingElem} <: ResRing{T}
   base_ring::Ring
   modulus::T

   function GenResidueRing(modulus::T, cached=true)
      if haskey(ModulusDict, (parent(modulus), modulus))
         return ModulusDict[parent(modulus), modulus]::GenResidueRing{T}
      else
         z = new{T}(parent(modulus), modulus)
         if cached
            ModulusDict[parent(modulus), modulus] = z
         end
         return z
      end
   end
end

type GenResidue{T <: RingElem} <: ResidueElem{T}
   data::T
   parent::GenResidueRing{T}

   GenResidue(a::T) = new(a)
end

###############################################################################
#
#   GenCapRelPowerSeriesRing / GenCapRelSeries
#
###############################################################################

const GenCapRelSeriesID = ObjectIdDict()

type GenCapRelPowerSeriesRing{T <: RingElem} <: SeriesRing{T}
   base_ring::Ring
   prec_max::Int
   S::Symbol

   function GenCapRelPowerSeriesRing(R::Ring, prec::Int, s::Symbol, cached=true)
      if haskey(GenCapRelSeriesID, (R, prec, s))
         return GenCapRelSeriesID[R, prec, s]::GenCapRelPowerSeriesRing{T}
      else
         z = new{T}(R, prec, s)
         if cached
            GenCapRelSeriesID[R, prec, s] = z
         end
         return z
      end
   end
end

type GenCapRelSeries{T <: RingElem} <: SeriesElem{T}
   coeffs::Array{T, 1}
   length::Int
   prec::Int
   parent::GenCapRelPowerSeriesRing{T}

   GenCapRelSeries(a::Array{T, 1}, length::Int, prec::Int) = new(a, length, prec)   
   GenCapRelSeries(a::GenCapRelSeries{T}) = a
end

###############################################################################
#
#   GenFractionField / GenFraction
#
###############################################################################

const GenFractionDict = ObjectIdDict()

type GenFractionField{T <: RingElem} <: FracField{T}
   base_ring::Ring

   function GenFractionField(R::Ring, cached=true)
      if haskey(GenFractionDict, R)
         return GenFractionDict[R]::GenFractionField{T}
      else
         z = new{T}(R)
         if cached
            GenFractionDict[R] = z
         end
         return z
      end
   end
end

type GenFraction{T <: RingElem} <: FractionElem{T}
   num::T
   den::T
   parent::GenFractionField{T}

   GenFraction(num::T, den::T) = new(num, den) 
end

###############################################################################
#
#   GenMatrixSpace / GenMatrix
#
###############################################################################

const GenMatrixDict = ObjectIdDict()

# not really a mathematical ring
type GenMatrixSpace{T <: RingElem} <: MatSpace{T}
   rows::Int
   cols::Int
   base_ring::Ring

   function GenMatrixSpace(R::Ring, r::Int, c::Int, cached=true)
      if haskey(GenMatrixDict, (R, r, c))
         return GenMatrixDict[R, r, c]::GenMatrixSpace{T}
      else
         z = new{T}(r, c, R)
         if cached
            GenMatrixDict[R, r, c] = z
         end
         return z
      end
   end
end

type GenMat{T <: RingElem} <: MatElem{T}
   entries::Array{T, 2}
   parent::GenMatrixSpace{T}

   GenMat(a::Array{T, 2}) = new(a) 
end

