```@meta
CurrentModule = Nemo
```

# Specific topics

## Julia arithmetic

At the console, Julia arithmetic is often defined in a way that a numerical
person would expect. For example, `3/1` returns a floating point number `3.0`,
`sqrt(4)` returns the floating point number `2.0` and `exp(0)` returns the
floating point number `1.0`.

In each case the ring is changed from the input to the output of the function.
Whilst this is often what one expects to happen in a computer algebra system,
these are not the definitions one would want for algebraic operations.

In this section we describe the alternatives we have implemented to allow
algebraic computations, particularly for rings and fields.

### `divexact` and `divides`

Nemo implements numerous kinds of division:

* floating point division using the `/` operator as per Julia
* exact division in a ring using `divexact` and `divides`
* quotient field element construction using `//` as per Julia
* Euclidean division using `div`, `rem`, `divrem`, `mod` and `%`

The expression `divexact(a, b)` for `a` and `b` in a ring `R` returns a value
`c` in `R` such that `a = bc`. If such an element of `R` does not exist, an
exception is raised.

To instead test whether such an element exists, `divides(a, b)` returns a tuple
`(flag, q)` where `flag` is a boolean saying whether such an exact quotient
exists in the ring and if so `q` is such a quotient.

### Euclidean division

Nemo must provide Euclidean division, i.e. given `a` and `b` in a Euclidean
ring `R` it must be able to find `q` and `r` such that `a = bq + r` with `r`
smaller than `a` with respect to some fixed Euclidean function on `R`.
There are some restrictions imposed by Julia however.

Firstly, `%` is a constant alias of `rem` in Julia, so these are not actually
two independent functions but the same function.

Julia defines `div`, `rem` and `divrem` for integers as a triple of functions
that return Euclidean quotient and remainder, where the remainder has the same
sign as the dividend, e.g. `rem(1, 3) == 1` but `rem(-2, 3) == -2`. In other
words, this triple of functions gives Euclidean division, but without a
consistent set of representatives.

When using Nemo at the console (or indeed inside any other package without
importing the internal Nemo definitions) `div`, `rem` and `divrem` return the
same values as Julia and these functions follows the Julia convention of making
the sign of the remainder the same as the dividend over `ZZ`, e.g.
`rem(ZZ(1), ZZ(3)) == 1` but `rem(ZZ(-2), ZZ(3)) == -2`.

Internally to Nemo however, this is not convenient. For example, Hermite normal
form over `ZZ` will only return a unique result if there is a consistent choice
of representatives for the Euclidean division. This applies to the generic HNF
code in AbstractAlgebra, but similar problems exist for the generic finitely
presented module code in AbstractAlgebra, even when used over Nemo integers.
Thus the Julia definition of `rem` will not suffice.

Furthermore, as Nemo wraps Flint, it is convenient that Euclidean division
inside Nemo should operate the way Flint operates. This is critical if for
example one wants the result of a Hermite normal form coming from Flint to be
reduced using the same definition of Euclidean remainder as used elsewhere
throughout the Nemo module and to return the same answers as the generic HNF
code in AbstractAlgebra for example.

In particular, Flint defines Euclidean remainder over the integers in line with
the Julia function `mod`, namely by returning the smallest remainder with the
same sign as the *divisor*, i.e. `mod(1, 3) == 1` but `mod(1, -3) == -2`.

Therefore internally, Nemo chooses `div`, `mod` and `divrem` to be a consistent
triple of functions for Euclidean division, with `mod` defined as per Julia.
Thus in particular, `div` and `divrem` behave differently to Julia inside of
Nemo itself, viz. `Nemo.divrem(-1, 3) == (-1, 2)`.

The same definitions for `div`, `mod` and `divrem` are used internally to
AbstractAlgebra as well, even for Julia integers, so that AbstractAlgebra and
Nemo are both consistent internally. However, both AbstractAlgebra and Nemo
export definitions in line with Julia so that behaviour at the console is
consistent.

The Nemo developers have given considerable thought to this compromise and the
current situation has evolved over many iterations to the current state. We do
not consider this to be a situation that needs 'fixing', though we are acutely
aware that many tickets will be opened complaining about some inconsistency.

When reflecting on the choice we have made, one must consider the following:

* Nemo must internally behave as Flint does for consistency
* There are also functions such as `powmod`, `invmod` that reduce as per mod
* HNF requires a consistent set of representatives for uniqueness over `ZZ`

Also note that Julia's `rem` does not provide symmetric mod, a misconception
that often arises. The issues here are independent of the decision to use
positive remainder (for positive modulus) in Flint, rather than symmetric mod.

We are aware that the conventions we have chosen have inconsistencies with
Julia and do not have the nice property that `div`, `rem` and `divrem` are a
triple of Euclidean functions inside Nemo. However, we are sure that the
convention we have chosen is one of only two sensible possibilities, and
switching to the other convention (apart from being a huge amount of effort)
would only succeed in replacing one kind of inconsistency with another.

As a consequence of these choices, `div`, `mod` and `divrem` are a triple of
functions for *all* Euclidean division across Nemo, not just for the integers.
As generic code must use a consistent set of functions, we ask that developers
respect this choice by using these three functions in all generic code. The
functions `rem` and `%` should only be used for Julia integers, and only when
one specifically wants the Julia definition.

### `sqrt`, `inv` and `exp`

As mentioned above, Julia does not perform computations within a given ring,
but often returns a numerical result when given an exact input.

Whilst this is often what a user expects, it makes operations such as power
series square root, inversion or exponentiation more tricky over an exact ring.

Therefore, AbstractAlgebra defines `sqrt`, `inv` and `exp` internally in a
strictly algebraic way, returning a result only if it exists in the ring of the
input and otherwise raising an exception.

For example, `AbstractAlgebra.sqrt(4) == 2`, `AbstractAlgebra.inv(-1) == -1`
and `AbstractAlgebra.exp(0) == 1`.

Naturally these definitions are not so terribly useful to a user and are only
needed for internal consistency. Therefore, of course these definitions are not
exported by `AbstractAlgebra` so that the behaviour at the console is not
affected by these definitions.

There is currently some inconsistency in that Nemo follows the Julia numerical
definitions internally rather than following the algebraic definitions provided
internally in AbstractAlgebra. This may or may not change in future.

It is worth recalling that Julia provides `isqrt` for integer square root. This
is not sufficient to solve our problem as we require square root for all rings,
not just integers. We don't feel that developers will want to type `isqrt`
rather than `sqrt` internally for all rings.

A number of changes are expected to be made with regard to the behaviour of
root taking and division functions, including the ability to specify high
performance alternatives that do not check the exactness of the computation.
These changes are being discussed on the Nemo ticket
https://github.com/Nemocas/Nemo.jl/issues/862
In particular, the table given there by thofma represents the current consensus
on the changes that will be made in the future.

Note that many of the above issues with exact computations in rings exist for
all the Julia transcendental functions, `sin`, `cos`, `log`, etc., of which
there are many. If we ever add some kind of generic power series functions for
these, we may extend the internal definitions to include exact algebraic
versions of all these functions. At least for now this is not a pressing issue.

The way that AbstractAlgebra deals with functions which must have a different
definition inside the module than what it exports is as follows. Firstly, we
do not import the functions from Base or export the functions at all.
Internally we make our definitions as we want them, but then we overload the
Base version explicitly to do what the console version of the function should
do. This is done by explicitly defining `Base.sqrt(::fmpz)` for example without
explicitly importing `sqrt` from Base, etc.

In the Generic module discussed below, we import the definitions from
AbstractAlgebra rather than Base.

### Determinant

Another function which Nemo handles differently to Julia is `det` for
determinant of matrices. If the input is an integer matrix, Nemo outputs an
integer rather than a floating point number for the determinant.

However, this is not such an acute problem as Julia's `det` has now been placed
in `LinearAlgebra` rather than `Base`. Moreover, Nemo has its own matrices and
so does not conflict with the definition of `det` for Julia matrices.

It is important for developers to understand this difference however. It is not
generally wise to use the Julia linear algebra functionality on the Julia
matrices underlying generic Nemo matrices for this reason.

## The Generic submodule

In AbstractAlgebra we define a submodule called Generic. The purpose of this
module is to allow generic constructions over a given base ring. For example
in Nemo, `R, x = Generic.PolynomialRing(ZZ, "x")` will construct a generic
polynomial ring over Nemo integers instead of constructing a Flint polynomial
ring.

In other words `x` will have the type `Generic.Poly{fmpz}` instead of the
usual `fmpz_poly`.

The ability to construct generic polynomials and matrices and the like is
useful for test code and for tracking down bugs in basic arithmetic. It is
also useful for performance comparison of arithmetic defined for generic ring
constructions vs the specialised implementations provided by C libraries like
Flint.

Whilst most developers will not need to use the Generic module specifically,
unless they have such needs, all Nemo developers need to understand how to
define new generic ring constructions and functions for them. They also need
to understand some subtleties that arise because of this mechanism.

Firstly, a generic construction like `PolynomialRing` must be defined inside
the Generic submodule of AbstractAlgebra. All files inside the `src/generic`
directory of AbstractAlgebra exist for this purpose. However, exporting from
that submodule will not export the functionality to the Nemo user.

To do this, one must add a function `PolynomialRing` for example, in
`src/Poly.jl`, say, which calls `Generic.PolynomialRing`. Then one needs
to export `PolynomialRing` from AbstractAlgebra (also in that file).

Similarly, all functions provided for generic polynomial rings are not
automatically available, even when exported from the Generic submodule.
Two additional things are required, namely an import from Generic into
AbstractAlgebra and then an export from AbstractAlgebra to the user.

An exception to this is if there is a function with the same name in
AbstractAlgebra (i.e. in the top level `src` directory). In this case
it is sufficient to simply import that function into `Generic` in the
file `src/Generic.jl`.

In the former case, two large lists exist in `src/AbstractAlgebra.jl` with
these imports and exports. These are kept in alphabetical order to prevent
duplicate imports/exports being added over time.

If one wishes to extend a definition provided by Base, one can simply overload
`Base.blah` inside the Generic submodule directly. Exceptions to this include
the `div`, `mod`, `divrem`, `sqrt`, `inv` and `exp` functions mentioned above.

For AbstractAlgebra types, one still defines these exceptions `blah` by
overloading `Base.blah` directly inside Generic. However, for the versions that
would conflict with the Julia definition (e.g. the definition for `Int`), we
instead define `AbstractAlgebra.blah` for that specific type and a fallback
`AbstractAlgebra.blah(a) = Base.blah(a)` which calls the Base version of the
function for all other types. Of course we do not export `blah` from
AbstractAlgebra.

In order to make the AbstractAlgebra version available in Generic (rather than
the Base version), we do not import `blah` from Base inside Generic, but
instead import it from AbstractAlgebra. One can see these imports for the
exceptional functions `blah` in the file `src/Generic.jl`.

## Unsafe operations and aliasing

As with most object oriented languages that overload arithmetic operators,
Julia creates new objects when doing an arithmetic operation. For example,
`BigInt(3) + BigInt(5)` creates a new `BigInt` object to return the value
`BigInt(8)`. This can be problematic when accumulating many such operations
in a single coefficient of a polynomial or entry of a matrix due to the large
number of temporary objects the garbage collector must allocate and clean up.

To speed up such accumulations, Nemo provides numerous unsafe operators, which
mutate the existing elements of the polynomial, matrix, etc. These include
functions such as `add!`, `addeq!`, `mul!`, `zero!` and `addmul!`.

These functions take as their first argument the object that should be modified
with the return value.

Note that functions such as `sub!`, `submul!` and `subeq!` are not in the
official interface and not provided consistently, thus generic code cannot
rely on them existing. So far it has always been the case that when doing
accumulation where subtraction is needed rather than addition, that a single
negation can be performed outside the accumulation loop and then the additive
versions of the functions can be called inside the loop where the performance
matters.

If we encounter cases in future where this is not the case, it may be necessary
to add the versions that do subtraction to the interface. However, this can
only be done if all rings in Nemo support it. One cannot define a fallback
which turns a subtraction into a negation and an addition, as then the old
performance characteristics of a new object being created per operation will
result, meaning that the developer will not be able to reason about the likely
performance of unsafe operators.

### Interaction of unsafe operators and immutable types

Because not all objects in Nemo are mutable, the unsafe operators somehow have
to support immutable objects. This is done by also returning the "modified"
return value from the unsafe operators. Naturally, this return value is not a
mutated version of the original value, as that is not possible. However, it
does allow the unsafe operators to accept immutable values in their first
argument. Instead of modifying this value, the old value is replaced with the
return value of the unsafe operator.

In order to make this work correctly, every single call to an unsafe operator
must assign the return value to the original location. This requires discipline
on the part of the developer using unsafe operators.

For example, to set the existing value `a` to `a + b` one must write

```julia
a = addeq!(a, b)
```

i.e. one must have an explicit assignment to the left of the `addeq!` call and
indeed all the unsafe operator calls.

In the case of a mutable type, `addeq!` will simply modify the original `a`.
The modified object will be returned and assigned to the exact same variable,
which has no effect.

In the case of an immutable type, `addeq!` does not modify the original object
`a` as this is impossible, but it still returns the new value and assigns it
to `a` which is what one wants.

## Aliasing rules and mutation

One must be incredibly careful when mutating an existing value that one owns
the value. If the user passes an object to a generic function for example and
it changes the object without the user knowing, this can result in incorrect
results in user code due to the value of their objects changing from under
them.

In the first instance, functions should never modify their inputs. But further
problems can also occur if the output of an unsafe operator happens to alias
one of the other inputs. Such cases need to be handled exceptionally carefully.

A second issue arises as Nemo is based on Flint, which has its own aliasing
rules which are distinct from the default expectation in Julia. This leads to
some interesting corner cases.

In particularly, Flint always allows aliasing of inputs and outputs in its
polynomial functions but expects matrix functions to have output matrices that
are distinct from their inputs, except in a handful of functions that are
specially documented to be inplace operations.

Moreover, when assigning an element to a coefficient of a polynomial or entry
of a matrix Flint always makes a copy of the element being assigned to that
location. In Julia however, if one assigns an element to some index of an
array, the existing object at that location is replaced with the new object.
This means that inplace modification of Julia array elements is not safe as it
would modify the original object that was assigned to that location, whereas in
Flint inplace modification is highly desirable for performance reasons and is
completely safe due to the fact that a copy was made when the value was
assigned to that location.

We have developed over a period of many years a set of rules that maximise the
performance benefit we get from our unsafe operators, whilst keeping the
burden imposed on the programmer to a minimum. It has been a *very* difficult
task to arrive at the set of rules we have whilst respecting correctness of our
code, and it would be extremely hard to change any of them.

### Arithmetic operations return a new object

In order to make it easy for the Nemo developer to create a completely new
object when one is needed, e.g. for accumulating values using unsafe operators,
we developed the following rules.

Whenever an arithmetic operation is used, i.e. `+`, `-`, `*`, unary minus and
`^`, Nemo always returns a new object, in line with Julia. Naturally,
`deepcopy` also makes a copy of an object which can be used in unsafe
functions.

Note that if `R` is a type and an element `a` of that type is passed to it,
e.g. `R(a)` then, the Julia convention is that the original object `a` will be
returned rather than a copy of `a`. This convention ensures there is not an
additional cost when coercing values that are already of the right type, e.g
in generic code where coercion may or may not be needed depending on the type.

We extend this convention to parent objects `R` and elements `a` of that
parent. In particular, `R(a)` cannot be used to make a copy of `a` for use
in an unsafe function if `R` is the parent of `a`.

All other functions *may* also return the input object if they wish. In other
words, the return value of all other functions is not suitable for use in
an unsafe function. Only return values of arithmetic operations and `deepcopy`
or objects freshly created using inner constructors will be suitable for such
use.

This convention has been chosen to maximise performance of Nemo. Low level
operations (where performance matters) make a new object, even if the result
is the same arithmetically as one of the inputs. But higher level functions
will not necessarily make a new object, meaning that they cannot be used with
unsafe functions.

### Aliasing rules

We now summarise the aliasing rules used by Nemo and AbstractAlgebra. We are
relatively confident by now that following these rules will result in correct
code given the constraints mentioned above.

* matrices are viewed as containers which may contain elements that alias one
  another. Other objects, e.g. polynomials, series, etc., are constructed from
  objects that do not alias one another, *even in part*

* standard unsafe operators, addeq!, mul!, addmul!, zero!, add! which mutate
  their outputs are allow to be used iff that output is entirely under the
  control of the caller, i.e. it was created for the purpose of accumulation,
  but otherwise must not be used

* all arithmetic functions i.e. unary minus, `+`, `-`, `*`, `^`, and deepcopy
  must return new objects and cannot return one of their inputs

* all other functions are allowed to return their inputs as outputs

* matrix functions with an exclamation mark should not mutate the objects that
  occur as entries of the output matrix, though should be allowed to
  arbitrarily replace/swap the entries that appear in the matrix. In other
  words, these functions should be interpreted as inplace operations, rather
  than operations that are allowed to mutate the actual entries themselves

* `R(a)` where `R` is the parent of `a`, always just returns `a` and not a copy

* `setcoeff!` and `setindex!` and `getcoeff` and `getindex` should not make
  copies. Note that this implies that setcoeff! should not be passed an element
  that aliases another somewhere else, even in part

* Constructors for polynomials, series and similar ring element objects (that
  are not matrices) that take an array as input, must ensure that the
  coefficients being placed into the object do not alias, even in part

## The SparsePoly module

The SparsePoly module in AbstractAlgebra is a generic module for sparse
univariate polynomials over a given base ring.

This module is used internally, e.g. in the generic multivariate gcd code,
however it is not particularly suitable for general use.

Firstly, whilst the representation is sparse (recursive) the algorithms
used generally are not. This is because the amount of time taken by the
Jit in Julia is simply too large (upwards of 6s for the first multivariate
gcd).

Secondly, the order of terms in that representation is not the one which a
developer would expect for a sparse univariate format.

If the Julia Jit is ever made orders of magnitude faster, it may be worth
cleaning up this module and making it generally available. But for now, it
should be considered internal and heavily incomplete.

## Parent object caching

Parent objects in Nemo must be unique given the data that is used to create
them. For this purpose most parent objects are cached globally and looked
up upon creation. If a parent object with that data already exists, it is
returned from the cache instead of creating a new one.

There are two situations where this can be problematic however.

The first situation is if one is doing some parallel programming. Here global
objects are a blight and it may be necessary to turn off caching and simply
ensure that that same data is only ever used once when creating parent objects.

The second situation is when doing multimodular algorithms, where many similar
parent objects with different moduli are created. The cache can become
overwhelmed slowing the code down or even grinding to a halt.

In both these situations one can pass `false` as an additional argument
to a parent constructor to avoid caching the parent object it creates. This
parameter normally has a default value of `true` and under normal circumstances
doesn't need to be supplied.

Note that special light-weight parent constructors, `PolyRing`, `AbsSeriesRing`,
`RelSeriesRing`, etc. are also provided which do not cache.

## Throw/nothrow for `check_parent`

By default the `check_parent` functions throw an exception if parents do not
match. However sometimes one would like to know if they match without throwing.

For this purpose one can pass an additional `false` argument to `check_parent`.
This supresses the exception that would be thrown if the parent objects didn't
match. Instead the function simply returns `true` or `false` to indicate
whether they matched or not.

## Delayed reduction

When working in residue rings, various functions will perform an arithmetic
operation followed by a reduction modulo the modulus of the residue ring.

Some accumulations, e.g. in linear algebra or polynomial arithmetic, can be
dramatically sped up if one can delay the reductions that would happen after
each operation in the accumulation.

Some of the Generic code in Nemo is designed to allow such delayed reduction
if the ring supports it and to simply use fallbacks that do the reduction
after every intermediate operation if they don't.

To support delayed reduction, a ring must support the delayed reduction
interface which we describe here.

Two additional functions must be supplied for the element type. We give
examples for the Nemo `nf_elem` type:

```julia
mul_red!(z::nf_elem, x::nf_elem, y::nf_elem, red::Bool)
```

This function behaves as per `mul!` but only performs reduction if the
additional boolean argument `red` is set to `true`. This function can
assume that both the inputs are reduced.

```julia
reduce!(x::nf_elem)
```

This function must perform reduction on an unreduced element (mutating it).
Note that it must return the mutated value as per all unsafe operators.

Finally, the `add!` and `addeq!` operators must be able to add nonreduced
values.

If one wishes to speed up generic code for rings that provide delayed
reduction, one makes use of the function `addmul_delayed_reduction!` in the
accumulation loop. Here is an example for accumulation into a two dimensional
matrix element in Generic in a matrix multiplication routine:

```julia
A[i, j] = base_ring(X)()
for k = 1:ncols(X)
    A[i, j] = addmul_delayed_reduction!(A[i, j], x[i, k], y[k, j], C)
end
A[i, j] = reduce!(A[i, j])
```

Here `C` is a temporary element of the same type as the other inputs which is
used internally in `addmul_delayed_reduction!` if needed.

Notice the final call to `reduce!` to reduce the accumulated value after the
accumulation loop has finished.

Note that `mul_red!` is never called directly but is called inside the generic
implementation of `addmul_delayed_reduction!` for rings that support delayed
reduction. That generic code falls back to a call to `addmul!` which in turn
falls back to `mul!` and `addeq!` where delayed reduction or `addmul!` are not
available.

