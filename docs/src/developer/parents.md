```@meta
CurrentModule = Nemo
```

# Parent objects

## The use of parent objects in Nemo

### The parent/element model

As for other major computer algebra projects such as Sage and Magma, Nemo uses
the parent/element model to manage its mathematical objects.

As explained in the appendix to the AbstractAlgebra documentation, the standard
type/object model used in most programming languages is insufficient for much
of mathematics which often requires mathematical structures parameterised by
other objects.

For example a quotient ring by an ideal would be parameterised by the ideal.
The ideal is an object in the system and not a type and so parameterised types
are not sufficient to represent such quotient rings.

This means that each mathematical "domain" in the system (set, group, ring,
field, module, etc.) must be represented by an object in the system, rather
than a type. Such objects are called parent objects.

Just as one would write `typeof(a)` to get the type of an object `a` in an
object/type system of a standard programming language, we write `parent(a)`
to return the parent of the object `a`.

When talked about with reference to a parent in this way, the object `a` is
referred to as an `element` of the parent. Thus the system is divided into
elements and parents. For example a polynomial would be an element of a
polynomial ring, the latter being the parent of the former.

Naturally the parent/element system leads to some issues in a programming
language not built around this model. We discuss some of these issues below.

### Types in the parent/element model

As all elements and parents in Nemo are objects, those objects have types
which we refer to as the element type and parent type respectively.

For example, Flint integers have type `fmpz` and the parent object they all
belong to, `FlintZZ` has type `FlintIntegerRing`.

More complex parents and elements are parameterised. For example, generic
univariate polynomials over a base ring `R` are parameterised by `R`. The
base ring of a ring `S` can be obtained by the call `base_ring(S)`.

We have found it extremely useful to parameterise the type of both the parent
and element objects of such a ring by the type of the elements of the base
ring. Thus for example, a generic polynomial with Flint integer coefficients
would have type `Poly{fmpz}`.

In practice Flint already implements univariate polynomials over Flint
integers, and these have type `fmpz_poly`. But both `fmpz_poly` and the
generic polynomials `Poly{fmpz}` belong to the abstract type `PolyElem{fmpz}`
making it possible to write functions for all univariate polynomials over
Flint integers.

Given a specific element type or parent type it is possible to compute one
from the other with the functions `elem_type` and `parent_type`. For example
`parent_type(fmpz_poly)` returns `FmpzPolyRing` and `elem_type(FmpzPolyRing)`
returns `fmpz_poly`. Similarly `parent_type(Generic.Poly{fmpz})` returns
`Generic.PolyRing{fmpz}` and so on.

These functions are especially useful when writing type assertions or
constructing arrays of elements insides function where only the parent object
was passed.

### Other functions for computing types

Sometimes one needs to know the type of a polynomial or matrix one would
obtain if it were constructed over a given ring or with coefficients/entries
of a given element type.

This is especially important in generic code where it may not even be known
which Julia package is being used. The user may be expecting an
AbstractAlgebra object, a Nemo object or even some other kind of object to be
constructed, depending on which package they are using.

The function for returning the correct type for a dense matrix is
`dense_matrix_type` to which one can pass either a base ring or an element
type. For example, if AbstractAlgebra is being used, `dense_matrix_type(ZZ)`
will return `Mat{BigInt}` whereas if Nemo is being used it will return
`fmpz_mat`.

We also have `dense_poly_type` for univariate polynomials, `abs_series_type`
for absolute series and `rel_series_type` for relative series.

In theory such functions should exist for all major object types, however they
have in most cases not been implemented yet.

### Functions for creating objects of a similar type

A slightly more consistent interface for creating objects of a type that is
suitable for the package currently in use is the `similar` interface.

For example, given a matrix `M` one can create one with the same dimensions
but over a different ring `R` by calling `similar(M, R)`. Likewise one can
create one over the same ring with different dimensions `r x c` by calling
`similar(M, r, c)`.

The `similar` system is sophisticated enough to know that there is no native
type provided by Flint/Antic for matrices and polynomials over a number field.
The system knows that in such cases it must create a generic matrix or
polynomial over the given number field.

A great deal of thought went into the design of the `similar` system so that
developers would not be required to implement similar for every pair of types
in the package.

Again this interface should exist for all major Nemo domains, but the
functionality is still being implemented in some cases.

### Changing base rings and map

Given a polynomial, matrix or other composite object over a base ring, it is
often convenient to create a similar object but with all the entries or
coefficients coerced into a different ring.

For this purpose the function `change_base_ring` is provided.

Similarly it may be useful to create the matrix or polynomial that results by
applying a given map/function/lambda to each of the entries or coefficients.

For this purpose Julia's `map` function is overloaded. There are also functions
specific to polynomials and matrices called `map_coefficients` and
`map_entries` respectively, which essentially do the same thing.

Note that the implementation of such functions must make use of the functions
discussed above to ensure that a matrix/polynomial of the right type is output.

### Parent checking

When applying binary operations to a pair of elements of a given ring, it is
useful to check that they are in fact elements of the same ring. This is not
possible by checking the types alone. For example elements of $Z/7Z$ and
$Z/3Z$ would have the same type but different parents (one parameterised by
the integer 7, the other by the integer 3).

In order to perform such a check in a function one uses `check_parent(a, b)`
where `a` and `b` are the objects one wishes to assert must have the same
parent. If not, an exception is raised by `parent_check`.

### Parent object constructors

Various functions are provided for constructing parent objects. For example
a polynomial ring is constructed by calling a `PolynomialRing` function.
Such functions are called parent object constructors.

In general parent object constructors are intended for the user and should not
be used in library code. There are a number of reasons for this.

Firstly, inside the Generic submodule of AbstractAlgebra the only parent object
constructors that are directly accessible are the ones inside Generic. Thus if
a Nemo function calls a function inside Generic and it creates a parent object
using one of the parent object constructors, it will create a parent object for
a generic ring rather than a Nemo one.

One can work around this by calling `AbstractAlgebra.PolynomialRing` instead of
simply `PolynomialRing` inside Generic, but even safer would be to find another
way to construct the polynomials required.

A second issue is that parent objects are allowed to be as large as one likes
and they are cached by the system. They can also perform arbitrary
precomputations for the ring/field/module etc. that is being constructed. Over
time they tend to accumulate such precomputations, slowing down all generic
code which made use of them. Both memory usage and performance may blow out in
previously working code.

Thirdly, parent objects must be unique across the system for a given set of
parameters. This means they must be cached globally. This is problematic for
any future attempts to parallelise library code and in the worst case memory
usage can balloon due to swelling caches.

Most parent object constructors take a `cached` keyword which specifies whether
the parent object should be cached or not, but again it is better overall to
simply eschew the use of parent object constructors in library code.

Instead, it is recommended to use functions such as `similar`, `zero`,
`zero_matrix`, `identity_matrix`, `change_base_ring`, `map`, etc. for
constructing polynomials and matrices directly.

There are also functions that provide alternative ways of constructing objects,
e.g. `matrix` provides a means of creating a matrix over a given ring with
given dimensions. The constructor `polynomial` allows creation of a polynomial
over a given base ring with given coefficients and `abs_series` and
`rel_series` do similar things for absolute and relative series. These should
be used in preference to parent object constructors where possible. Additional
functions of this type should be added in future.

However even when using these functions in library code, it is important to
remember to pass `cached=false` so that the cache is not filled up by calls
to the library code. But this creates an additional problem, namely that if one
uses `polynomial` say, to construct two polynomials over the same base ring,
they will not be compatible in the sense that they will have different parents.

When one wishes to construct multiple elements in the same group/ring/field,
it is convenient to be able to construct a parent just as a user would. For
this purpose various light-weight and very safe parent constructors are
provided for use in library code.

For example there are the constructors `PolyRing`, `AbsSeriesRing` and
`RelSeriesRing`. These functions return the parent ring $R$ only and no
generator (it can be obtained by calling `gen(R)`). They also set the
variable for printing to a default (usually `x`). Moreover, these parents
are not cached, so they are completely safe to use in library code. They
can be thousands of times faster than the full parent constructors intended
for users.

Here is an example of their use:

```julia
R = PolyRing(ZZ)
p = R([1, 2, 3])
q = R([2, 3, 4])
s = p + q
```

Naturally functions like `polynomial` and `matrix` and the light-weight parent
constructors are missing for other modules in Nemo at present and it is hoped
that developers will fill in such infrastructure rather than simply push the
can down the road for someone else to fix. Forcing the creating of full parent
objects into as few bottlenecks as possible will make it much easier for
developers to remove problems associated with such calls when they arise in
future.

