```@meta
CurrentModule = Nemo
```

# Interfaces

## Functionality for Generic and Abstract Types

As previously mentioned, Nemo provides various generic types, e.g. `Poly{T}`
for generic univariate polynomials and `Mat{T}` for generic matrices over a
base ring. These and other polynomial and matrix types belong in turn to
abstract types or unions thereof, e.g. `PolyRingElem{T}` is an abstract type
representing all univariate polynomial types and `MatrixElem{T}` is a union
of all Nemo matrix types.

When implementing generic functionality, one should usually implement it for
the abstract types and unions thereof, since the new functionality will then
work for all types of the specified kind, instead of just the generic types.

In order for this to work in practice, such implementations can only use
functions in the relevant official interface. These are the functions required
to be implemented by all types of that kind. For example, matrix
implementations make heavy use of `add!` and `mul!` to accumulate entries, but
they cannot make use of functions such as `subeq!` as it is not part of the
official interface.

In addition to implementations for abstract types and their unions, one may
also like to provide specialised implementations for the generic types
e.g. `Poly{T}` and `Mat{T}` as one would for other specialised types. The
generic types are based on Julia arrays internally, and so it makes perfect
sense to implement lower level functionality for these types specifically, as
this may lead to performance gains. Such specialised implementations can make
use of any functions provided for the generic types, whether in the interface
or not.

For convenience we list the most important abstract types and their unions
for which one should usually prefer to write generic implementations.

* `PolyRingElem{T}` : all univariate polynomial types
* `MPolyRingElem{T}` : all multivariate polynomial types (see note below)
* `MatrixElem{T}` : union of all matrix types including matrix algebras
* `MatElem{T}` : all matrix types not including matrix algebras
* `AbsPowerSeriesRingElem{T}` : all abstract series types
* `RelPowerSeriesRingElem{T}` : all relative series types
* `LaurentSeriesElem{T}` : union of all Laurent series over rings and fields
* `PuiseuxSeriesElem{T}` : union of all Puiseux series over rings and fields
* `FPModule{T}` : all finitely presented modules over a Euclidean domain
* `FPModuleElem{T}` : all elems of fin. presented modules over a Euc. domain
* `FracElem{T}` : all fractions
* `ResElem{T}` : all elements of a residue ring
* `ResFieldElem{T}` : all elements of a residue field
* `Map{D, C}` : all maps (see Maps developer docs for a description)

N.B: inside the `Generic` submodule of AbstractAlgebra some abstract types
`Blah` are only accessible by writing `AbstractAlgebra.Blah`. The unions are
directly accessible. There may be generic types and abstract types with the
same name, so this is more than just a convention.

Note that multivariate polynomials tend to require very specialised
implementations depending heavily on implementation details of the specific
multivariate type. Therefore it is rare to write implementations for the
abstract type `MPolyRingElem{T}`. Instead, implementations tend to be done for each
concrete multivariate type separately.

## Generic interfaces

As mentioned above, the generic implementations in Nemo depend on carefully
written interfaces for each of the abstract types provided by the system.

These interfaces are spelled out in the AbstractAlgebra documentation. Note
that a generic implementation may depend on functions in both the required and
optional interfaces as the optional functions are all implemented with generic
fallbacks in terms of the required functions.

For convenience we provide here a list of interfaces that can be relied on in
generic implementations, along with a description.

* Ring : all commutative rings in the system
* Field : all fields in the system
* NCRing : all rings in the system (not necessarily commutative)
* Euclidean Ring : Euclidean rings (see notes below)
* Univariate Polynomial Ring : all dense univariate polynomials
* Multivariate Polynomial Ring : all sparse distributed multivariate polys.
* Series Ring : all series, relative and absolute
* Residue Ring : all quotients of gcd domains with `gcdx` by a principal ideal
* Fraction Field : all fractions over a gcd domain with `gcdx`
* Module : all finitely presented modules over a Euclidean domain
* Matrix : all matrices over a commutative ring
* Map : all (set) maps in the system

Although we allow `Z/nZ` in our definition of Euclidean ring, much of the
functionality in Nemo can be expected to misbehave (impossible inverses, etc.)
when working with Euclidean rings that are not domains. In some cases the
algorithms just don't exist, and in other cases we simply haven't implemented
the required functionality to support all Euclidean rings for which
computations can be done.

Whether a ring is a Euclidean domain or not cannot be encoded in the type. Thus
there is no abstract type for Euclidean domains or their elements. Instead,
generic functions rely on the existence of certain functions such as `gcdx` to
implement functionality for Euclidean domains.

There is also currently no way to define a Euclidean function for a given ring
(which is known to be Euclidean) and have the system recognise the ring as
such. This kind of Euclidean interface may be provided in a future version of
Nemo.


## Julia interfaces we support

Many Julia interfaces rely on being able to create zero and one elements given
the type only. As we use the parent/element model (see developer notes on this
topic) we cannot support all Julia interfaces fully.

We do however partially implement some Julia interfaces.

* Iteration : iterators are currently provided for multivariate polynomials to
  iterate over the coefficients, terms and monomials. Nemo matrices can also be
  iterated over. Iteration proceeds down each column in turn. One can also
  iterate over all permutations and partitions. Finally, all finite field types
  can be iterated over.

* Views : because C libraries cannot be expected to implement the full range
  of Julia view types, views of matrices in Nemo can only be constructed for
  submatrices consisting of contiguous blocks in the original matrix.

* `map` and `similar` : we implement the map and similar interfaces with the
  caveat that we generally use parent objects where Julia would use types. See
  the specific documentation for the module of interest to see details.

* `zero` and `one` : these are implemented for parent types, which is not what
  Julia typically expects. Exceptions include the Flint `ZZRingElem` and `QQFieldElem` types,
  as their parents are not parameterised, which makes it possible to implement
  these functions for the types as well as the parents.

* `rand` : we have a Nemo specific `rand` interface, which passes the tail of
  a given `rand` invocation to the `rand` function for the base ring, e.g. to
  create random matrix elements or polynomial coefficients and so on. In addition
  to this custom `rand` interface, we also support much of the Julia `rand`
  interface, with the usual caveat that we use parent objects instead of types
  where necessary.

* serialisation : unfortunately this is currently NOT implemented by Nemo, but
  we would certainly like to see that done in the future. It's not automatic
  because of the C objects that underly many of our constructions.

* `Number` : Nemo number types do NOT belong to Julia's `Number` hierarchy, as
  we must make all our ring element types belong to our `RingElem` abstract
  type. To make some Julia `Number` types cooperate with Nemo, we define the
  unions `RingElement` and `FieldElement` which include some Julia types, such
  as `BigInt` and `Rational{BigInt}`, etc. Note that fixed precision integer
  types cannot be expected to be well-behaved when they overflow. We recommend
  using Nemo integer types if one wants good performance for small machine
  word sized integers, but no overflow when the integer becomes large (Nemo
  integers are based on Flint's multiprecision `ZZRingElem` type).

* `hash` : we implement hash functions for all major element types in Nemo.

* `getindex`/`setindex!`/`typed_hvcat` : we implement these to access elements
  of Nemo matrices, however see the note below on row major representation. In 
  addition, we allow creation of matrices using the notation `R[a b; c d]` etc.
  This is done by overloading `typed_hvcat` for the parent object `R` instead of
  a type as Julia would normally expect. This produces a Nemo matrix rather than
  a Julia one. Note that when passed a type, Julia's `typed_hvcat` can only
  construct Julia matrices for Nemo types such as `ZZRingElem` and `QQFieldElem` where
  elements can be constructed from types alone.

Many other Julia interfaces are either not yet implemented or only very
partially implemented.

## Column major vs row major matrices

Whereas Julia uses column major representation for its matrices, Nemo follows
the convention of the C libraries it wraps and uses row major representation.
Although Julia 2-D arrays are used internally in Nemo's generic matrix type,
the interface from the perspective of the user is still the Nemo row major
convention, not the Julia column major convention.

In row major representation, some row operations may be able to be performed
more cheaply than similar column operations. In column major representation
the converse is true. This may mean that some Julia matrix implementations may
perform more slowly if naively ported to Nemo matrices, unless suitably
modified.

