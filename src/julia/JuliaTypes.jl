###############################################################################
#
#   Integers / BigInt
#
###############################################################################

mutable struct Integers{T <: Integer} <: Ring
   function Integers{T}() where T <: Integer
      if haskey(IntegersID, T)
         z = IntegersID[T]::Integers{T}
      else 
         z = new{T}()
         IntegersID[T] = z
      end
      return z
   end
end

const IntegersID = Dict{DataType, Ring}()

###############################################################################
#
#   Rationals / Rational
#
###############################################################################

mutable struct Rationals{T <: Integer} <: Field
   function Rationals{T}() where T <: Integer
      if haskey(RationalsID, T)
         z = RationalsID[T]::Rationals{T}
      else 
         z = new{T}()
         RationalsID[T] = z
      end
      return z
   end
end

const RationalsID = Dict{DataType, Ring}()

###############################################################################
#
#   GFField/gfelem
#
###############################################################################

mutable struct GFField{T <: Integer} <: FinField
   p::T

   function GFField{T}(p::T) where T <: Integer
      if haskey(GFFieldID, (T, p))
         z = GFFieldID[T, p]::GFField{T}
      else
         z = new{T}(p)
         GFFieldID[T, p] = z
      end
      return z
   end
end

const GFFieldID = Dict{Tuple{DataType, Integer}, Field}()

struct gfelem{T <: Integer} <: FinFieldElem
   d::T
   parent::GFField{T}
end

###############################################################################
#
#   Unions of Nemo abstract types and Julia types
#
###############################################################################

const RingElement = Union{RingElem, Integer, Rational}

const FieldElement = Union{FieldElem, Rational}

