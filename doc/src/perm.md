```@meta
CurrentModule = Nemo
```

## Introduction

Nemo provides rudimentary support for permutation groups via Flint. These are
mainly used for permutations of rows of matrices.

Permutation groups are created using the `FlintPermGroup` (inner) constructor.
However, for convenience we define

```
PermutationGroup = FlintPermGroup
```

so that permutation groups can be created using `PermutationGroup` instead of
`FlintPermGroup`.

The types of permutations in Nemo are given by the following table, along with
the libraries that provide them and the associated types of the parent objects.

 Library | Group                          | Element type  | Parent type
---------|--------------------------------|---------------|---------------------
Flint    | $S_n$                          | `perm`        | `FlintPermGroup`

All the permutation group types belong to the `Group` abstract type and the
corresponding permutation element types belong to the `GroupElem` abstract
type.

## Permutation group constructors

In order to construct permutations in Nemo, one must first construct the
permutation group they belong to. This is accomplished with the following
constructor.

```
FlintPermGroup(n::Int)
```

Construct the permutation group on $n$ points. The function returns the parent
object representing the group.

Here are some examples of creating a permutation group and using the parent
object to create a permutation (the identity permutation).

```
G = PermutationGroup(5)

p = G()
```

## Permutation constructors

Once a permutation group is constructed, there are various ways to construct
permutations in that group.

Apart from the identity permutation coercion as above, we offer the following
functions.

```@docs
eye(::FlintPermGroup)
```

Note that permutations consist of lists of $n$ integers numbered from $1$ to
$n$. If the $i$-th entry of a permuation is $j$, this corresponds to sending
$i \to j$ in the permutation.

Here are some examples of creating permutations.

```
G = PermutationGroup(5)

p = eye(G)
```

## Basic functionality

The following basic functionality is provided by the default permutation group
implementation in Nemo, to support construction of other generic constructions
over permutation groups. Any custom permutation group implementation in Nemo
should provide these  functions along with the usual group element arithmetic.

```
parent_type(::Type{perm})
```

Gives the type of the parent object of a Flint permutation group element.

```
elem_type(R::FlintPermGroup)
```

Given the parent object for a permutation group, return the type of elements
of the group.

```
Base.hash(a::perm, h::UInt)
```

Return a `UInt` hexadecimal hash of the permutation element $a$. This should
be xor'd with a fixed random hexadecimal specific to the permutation group
type. The hash of the entries of the permutation should be xor'd with the
supplied parameter `h` as part of computing the hash.

```
deepcopy(a::perm)
```

Construct a copy of the given permutation group element and return it. This
function must recursively construct copies of all of the internal data in the
given element. Nemo permutation group elements are mutable and so returning
shallow copies is not sufficient.

```
getindex(a::perm, n::Int)
```

Allows access to entry $n$ of the given permutation via the syntas `a[n]`. Note
that entries are $1$-indexed.

```
setindex!(a::perm, d::Int, n::Int)
```

Set the $n$-th entry of the given permutation to $d$. This allows Julia to
provide the syntax $a[n] = d$ for setting entries of a permuation. Note that
entries are $1$-indexed.

Given the parent object `G` for a permutation group, the following coercion
functions are provided to coerce various elements into the permutation group.
Developers provide these by overloading the `call` operator for the permutation
group parent objects.

```
R()
```

Return the identity permutation.

```
R(A::Array{Int, 1})
```

Return the permutation whose entries are given by the elements of the supplied
vector.

```
R(p::perm)
```

Take a permutation that is already in the permutation group and simply return
it. A copy of the original is not made.

In addition to the above, developers of custom permutation group types must
ensure that each permutation element contains a field `parent` specifying the
parent object of the permutation group element, or at least supply the
equivalent of the function `parent(a::perm)` to return the parent object of a
permutation group element.

## Basic manipulation

Numerous functions are provided to manipulate permutation group elements. Also
see the section on basic functionality above.

```@docs
parent(::perm)
```

```@docs
parity(::perm)
```

Here are some examples of basic manipulation of permutations.

```
G = PermutationGroup(5)

p = G([1, 3, 5, 2, 4])

R = parent(p)
a = parity(p)
```

## Arithmetic operators

```@docs
*(::perm, ::perm)
```

Here are some examples of arithmetic operations.

```
G = PermutationGroup(5)

p = G([1, 3, 5, 2, 4])
q = G([5, 4, 1, 3, 2])

a = p*q
```

## Comparison

``@docs
==(::perm, ::perm)
```

Here are some examples of comparison.

```
G = PermutationGroup(5)

p = G([1, 3, 5, 2, 4])
q = G([5, 4, 1, 3, 2])

p == q
```

## Inversion

```@docs
inv(::perm)
```

Here are some examples of inversion.

```
G = PermutationGroup(5)

p = G([1, 3, 5, 2, 4])

a = inv(p)
```
