###############################################################################
#
#   Abstract types
#
###############################################################################

# broad mathematical domains
# these contain the type classes of parent objects

abstract type Set end

abstract type Group <: Set end

abstract type Ring <: Group end

abstract type Field <: Ring end

# elements of mathematical domains

abstract type SetElem end

abstract type GroupElem <: SetElem end

abstract type RingElem <: GroupElem end

abstract type FieldElem <: RingElem end

# parameterized domains

abstract type Module{T <: RingElem} <: Group end

# elements of parameterised domains

abstract type ModuleElem{T <: RingElem} <: GroupElem end

# rings, fields etc, parameterised by an element type
# these are the type classes of different kinds of
# mathematical rings/fields/etc, which have a base ring,
# and for which a generic implementation is possible
# over that base ring

abstract type PolyRing{T} <: Ring end

abstract type MPolyRing{T} <: Ring end

abstract type SeriesRing{T} <: Ring end

abstract type ResRing{T} <: Ring end

abstract type ResField{T} <: Field end

abstract type FracField{T} <: Field end

# not always really mathematical rings
# later we'll distinguish matrix algebras
# from the generic case
abstract type MatSpace{T} <: Ring end

# mathematical objects parameterised by an element type
# these are the type classes of mathematical objects
# that have some kind of base ring, and a generic
# implementation is meaningful over that base ring

abstract type PolyElem{T} <: RingElem end

abstract type MPolyElem{T} <: RingElem end

abstract type ResElem{T} <: RingElem end

abstract type ResFieldElem{T} <: FieldElem end

abstract type FracElem{T} <: FieldElem end

abstract type SeriesElem{T} <: RingElem end

abstract type RelSeriesElem{T} <: SeriesElem{T} end

abstract type AbsSeriesElem{T} <: SeriesElem{T} end

   # not always mathematical ring elements
   # later we'll maybe distinguish MatAlgebraElem, MatModuleElem
abstract type MatElem{T} <: RingElem end

# additional abstract types for parents, added ad hoc to form
# collections of types as needed by applications

abstract type FinField <: Field end    # for fq, fq_nmod, etc

# additional abstract types for elements, added ad hoc to form
# collections of types as needed by applications

abstract type FinFieldElem <: FieldElem end # for fq, fq_nmod, etc

################################################################################
#
#   Promotion system
#
# The promote_rule functions are not extending Base.promote_rule. The Nemo
# promotion system is orthogonal to the built-in julia promotion system. The
# julia system assumes that whenever you have a method signature of the form
# Base.promote_rule(::Type{T}, ::Type{S}) = R, then there is also a
# corresponding Base.convert(::Type{R}, ::T) and similar for S. Since we
# cannot use the julia convert system (we need an instance of the type and not
# the type), we cannot use the julia promotion system.
#
# The Nemo promotion system is used to define catch all functions for
# arithmetic between arbitrary ring elements.
#
################################################################################

promote_rule(T, U) = Union{}
