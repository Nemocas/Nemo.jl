###############################################################################
#
#   Abstract types
#
###############################################################################

# broad mathematical domains
# these contain the type classes of parent objects

   abstract Set

   abstract Group <: Set

   abstract Ring <: Group

   abstract Field <: Ring

# elements of mathematical domains

   abstract SetElem

   abstract GroupElem <: SetElem

   abstract RingElem <: GroupElem

   abstract FieldElem <: RingElem

# rings, fields etc, parameterised by an element type
# these are the type classes of different kinds of
# mathematical rings/fields/etc, which have a base ring,
# and for which a generic implementation is possible
# over that base ring

   abstract PolyRing{T} <: Ring

   abstract SeriesRing{T} <: Ring

   abstract ResRing{T} <: Ring

   abstract FracField{T} <: Field

# not always really mathematical rings
# later we'll distinguish matrix algebras
# from the generic case
   abstract MatSpace{T} <: Ring

# mathematical objects parameterised by an element type
# these are the type classes of mathematical objects
# that have some kind of base ring, and a generic 
# implementation is meaningful over that base ring

   abstract PolyElem{T} <: RingElem

   abstract ResElem{T} <: RingElem

   abstract FracElem{T} <: FieldElem

   abstract SeriesElem{T} <: RingElem

   # not always mathematical ring elements
   # later we'll maybe distinguish MatAlgebraElem, MatModuleElem
   abstract MatElem{T} <: RingElem

# additional abstract types for parents, added ad hoc to form
# collections of types as needed by applications

   abstract FinField <: Field     # for fq, fq_nmod, etc
   
# additional abstract types for elements, added ad hoc to form
# collections of types as needed by applications

   abstract FinFieldElem <: FieldElem # for fq, fq_nmod, etc
  