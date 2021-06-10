```@meta
CurrentModule = Nemo
```

# The type system

## Use of Julia types in Nemo

### Concrete and abstract types

Julia does not provide a traditional class/inheritance approach to programming.
Instead, the basic unit of its object oriented approach is the type definition
(`struct` and `mutable struct`) and inheritance exists only on the function
side of the language rather than data side. Julia provides a rich system of
abstract types and unions on the data side and multimethods on the function
side to effect this.

For example Julia's `Number` type is an abstract type containing all concrete
types that behave like numbers, e.g. `Int64`, `Float64`, and so on.

Abstract types can also belong to other abstract types, forming a tree of
abstract types.

In Nemo the most important abstract types are `Ring` and `Field`, with the
latter belonging to the former so that all fields are rings, and the abstract
types `RingElem` and `FieldElem` for the objects that represent elements of
rings and fields, again with the latter abstract type belonging to the former.

Because this hierarchy of abstract types must form a tree, Julia is strictly
speaking single inheritance, as each concrete and abstract type can belong to
at most one other abstract type. For example, one could not have a diamond of
abstract types with `ExactField` belonging to both `Field` and `ExactRing`.

### Recovering aspects of multiple inheritance in Nemo

Various possibilities exist to get around the limitation that abstract types
must form a 'tree' in Nemo and AbstractAlgebra.

One such possibility is union types. If a function should accept one of a
number of concrete or abstract types that can't all be made to belong to a
single abstract type due to this limitation then one can use a union type.

For example, Nemo defines `RingElement` to be a union of `RingElem` and all
the Julia standard types which behave like ring elements, e.g. all `Integer`
types and types of rationals with `Integer` components.

Other union types are defined in `src/AbstractAlgebra.jl` in AbstractAlgebra.

A second feature we make use of in Nemo is parameterised types. Each concrete
and abstract type can take one or more parameters. These parameter can be any
other type, either concrete or abstract. For example, in Julia `Rational{T}`
is for rationals with numerator and denominator of type `T`.

A great deal of control over parameterised types is possible, e.g. one can
restrict the type parameter `T` using a `where` clause, e.g. to write a
function that accepts all rational types with integer components of the same
type one can use the type `Rational{T} where T <: Integer`.

Nemo makes use of such parameterised types for generic ring constructions
such as generic polynomial rings and matrices over a given base ring. The type
of the elements of the base ring is substituted for the parameter `T` in any
concrete instantiation of the types `Poly{T}` and `Mat{T}`, which are defined
in AbstractAlgebra in `src/generic/GenericTypes.jl`.

The totality of all univariate polynomial types, including those of generic
`Poly{T}` types and those coming from C libraries (such as `fmpz_poly`), is
represented by the abstract type `PolyElem{T}` which in turn belongs to
`RingElem`, both defined in AbstractAlgebra in `src/AbstractTypes.jl`.

Similarly, the totality of all matrix types, including explicit C types
like `fmpz_mat` and the generic `Mat{T}` types is given by the abstract type
`MatElem{T}`, again defined in AbstractAlgebra in `src/AbstractTypes.jl`.

This hierarchy of types allows one to write functions at any level, e.g. for
all univariate polynomial types, just those with a given base type `T`, or
for a specific concrete type corresponding to just one kind of univariate
polynomial.

A third possibility to get around the single inheritance limitation of Julia is
type traits. There is currently no explicit compiler/language support for
traits, however various implementations exist that make use of type parameters
in tricky ways. This allows one to add 'traits' to types, so long as those
traits can be expressed as types. In this way, types can have multiple
'properties' at the same time, instead of belonging to just a single abstract
type.

Nemo does not currently use type traits, though the map types in Nemo do make
use of a custom analogue of this.

Note that unlike class based systems that dispatch on the type of a (sometimes
implicit) `this` or `self` parameter, Julia methods dispatch on the type of all
arguments. This is a natural fit for mathematics where all sorts of ad hoc left
and right operations may be required.

### Encapsulation, maps and runtime flags

One limitation of the Julia approach is that the type of an object cannot be
changed at runtime. For example one might like to insist that a given ring is
in fact a field. There are three standard ways to handle this in Julia.

The first approach is to encapsulate the object in another object which does
have the desired type. The second approach is to map the object to a different
one of the required type (e.g. by applying a morphism). The third approach is
to introduce data fields in the original type which can be changed at runtime,
unlike its type. All three approaches come with downsides. 

Encapsulation can be time consuming for the developer as methods which applied
to the original object do not automatically apply to the encapsulated object.
One can write methods which do, but this is not automatic.

Application of a map may come with a performance penalty and may be difficult
for the user to navigate. Moreover, mutation of the resulting object does not
result in mutation of the original object.

The third option of adding runtime data fields essentially takes one back to
writing a (possibly bug ridden) interpreter. It relies on the developer 
implementing outer methods that make use of hand written control statements
to determine which of a range of inner methods should be applied to the object.
This misses the benefits of one of the main defining features of Julia, namely
its multimethod system and can also make introspection more difficult.

Nemo does not apply any of these three approaches widely at present, though
information which can only be known at runtime such as whether a ring is
Euclidean will eventually have to be encoded using one of these three
methods.

### Nemo's custom map types

It makes sense that map types in Nemo should be parameterised by the element
types of both the domain and codomain of the map, and of course all maps in
the system should somehow belong to an abstract type `Map`.

This leads one to consider a two parameter system of types `Map{D, C}` where
`D` and `C` are the domain and codomain types respectively.

One may also wish to implement various types of map, e.g. linear maps (where
the map contains a matrix representing the map) or functional maps (where the
map is implemented by a Julia function) and so on. Notionally one imagines
doing this with a hierarchy of two parameter abstract types all ultimately
belonging to `Map{D, C}` as the root of the tree.

This approach begins to break down when constructions from homological algebra
begin to be applied to maps. In such cases, the maps themselves are the object
of study and functions may be applied to maps to produce other maps.

The simplest such function is composition. In a system where composition of
maps always results in a map of the same type, no problem arises with the
straightforward approach outlined above.

However, for various reasons (including performance) it may not be desirable or
even possible to construct a composition of two given maps using the same
representation as the original maps. This means that the result of composing
two maps of the same type may be a map of a different type, e.g. in the worst
case a general composition type.

This problem makes many homological and category theoretic operations on maps
difficult or impossible to implement.

Other operations which may be desirable to implement are caching of maps (e.g.
where the map is extremely time consuming to compute, such as discrete
logarithms) and attaching category theoretic information to maps. Such
operations can be effected by encapsulating existing maps in objects containing
the extra information, e.g. a cache or a category. However all the methods that
applied to the original map objects now no longer apply to the encapsulated
objects.

To work around these limitations Nemo implements a four parameter `Map` type,
`Map{D, C, T, U}`.

The first two parameters are the domain and codomain types as discussed above.

The parameter `T` is a "map class" which is itself an abstract type existing in
a hierarchy of abstract types. This parameter is best thought of as a trait,
independent of the hierarchy of abstract types belonging to `Map`, giving
additional flexibility to the map types in the system.

For example, `T` may be set to `LinearMap` or `FunctionalMap`. This may be
useful if one wishes to distinguish maps in other ways, e.g. whether they are
homomorphisms, isomorphisms, maps with section or retraction etc. As usual,
offering traits partially gets around the single inheritance problem.

The final parameter `U` is used to allow maps of a given type `U` to be
composed and still result in a map of type `U`, even though the concrete type
of the composition is different to that of the original maps. Methods can
be written for all maps of type `U` by matching this parameter, rather than
matching on the concrete type `U` of the original maps.

For example, two maps with concrete type `MyRingHomomorphism` would belong to
`Map{D, C, T, MyRingHomomorphism}` as would any composition of such maps, even
if the concrete type of the composition was not a `MyRingHomomorphism`.

Naturally four parameter types are rather unwieldy and so various helper
functions are provided to compute four parameter map types. In the first
instance one still has the type `Map{D, C}` which will give the union of all
map types whose first two parameters are `D` and `C`, and where the remaining
two parameters are arbitrary.

However one can also pass a map class or a concrete type `U` to a `Map`
function to compute the class of all maps of the given map class or type.

For example, to write a function which accepts all maps of "type"
`MyRingHomomorphism`, including all compositions of such maps, one inserts
`Map(MyRingHomomorphism)` in place of the type, e.g.

```julia
function myfun(f::Map(MyRingHomomorphism))
```

Note the parentheses here, rather than curly braces; it's a function to
compute a type! Now the function `myfun` will accept any map type whose
fourth parameter `U` is set to `MyRingHomomorphism`.

This four parameter system is flexible, but may need to be expanded in the
future. For example it may be useful to have more than one trait `T`. This
could be achieved either by making `T` a tuple of traits or by introducing a
parameterised `MapTrait` type which can be placed at that location. Naturally
the `Map` functions for computing the four parameter types will have to be
similarly expanded to make it easier for the user.

The map type system is currently considered experimental and our observation so
far is that it is not intuitive for developers.

## Type hierarchy diagram

The most important abstract types in the system are the element types. Their
hierarchy is shown in the following diagram.

![alt text](img/types.svg)

Most of the element types have a corresponding parent abstract type. These are
shown in the following diagram.

![alt text](img/types2.svg)
