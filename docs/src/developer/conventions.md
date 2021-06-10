```@meta
CurrentModule = Nemo
```

# Conventions

AbstractAlgebra and Nemo have adopted a number of conventions to help maintain
a uniform codebase.

## Code conventions

### Function and type names

Names of types in Julia follow the convention of CamelCase where the first
letter of each word is capitalised, e.g. `Int64` and `AbstractString`.

Function/method names in Julia use all lowercase with underscores between
the words, e.g. `zip` and `jacobi_symbol`.

We follow these conventions in Nemo with some exceptions:

* When interfacing C libraries the types use the same spelling and
  capitalisation in Nemo as they do in C, e.g. the Flint library's `fmpz_poly`
  remains uncapitalised in Nemo.

* Types such as `gfp_poly` which don't exist under that name on the C side
  also use the lowercase convention as they wrap an actual C type which must be
  split into more than one type on the Julia side. For example `nmod_poly` and
  `gfp_poly` on the Julia side both represent Flint `nmod_poly`'s on the C side.

* Types of rings and fields, modules, maps, etc. are capitalised whether they
  correspond to a C type or not, e.g. `FqNmodFiniteField` for the type of an
  object representing the field that `fq_nmod`'s belong to.
.
* We omit an underscore if the first word of a method is "is" or "has", e.g.
  `iseven`.

* Underscores are omitted if the method name is already well established
  without an underscore in Julia itself, e.g. `setindex`.

* Constructors with the same name as a type use the same spelling and
  capitalisation as that type, e.g. `fmpz(1)`.

* Functions for creating rings, fields, modules, maps, etc. (rather than the
  elements thereof) use CamelCase, e.g. `PolynomialRing`. We refer to these 
  functions as parent constructors. Note that we do not follow the Julia
  convention here, e.g. `PolynomialRing` is a function and not a type constructor
  (in fact we often return a tuple consisting of a parent object and other
  objects such as generators with this type of function) yet we capitalise it.

* We prefer words to not be abbreviated, e.g. `denominator` instead of `den`.

* Exceptions always exist where the result would be offensive in any major
  spoken language (example omitted).

It is easy to find counterexamples to virtually all these rules. However we
have been making efforts to remove the most egregious cases from our codebase
over time. As perfect consistency is not possible, work on this has to at
times take a back seat.

### Use of ASCII characters

All code and printed output in Nemo should use ASCII characters only. This is
because we have developers who are using versions of the WSL that cannot
correctly display non-ASCII characters.

This extends to function and operator names, which saves people having to
learn how to enter them to use the system.

### Spacing and tabs

All function bodies and control blocks should be indented using spaces.

A survey of existing code shows 2, 3 or 4 space indenting commonly used in our
files. Values outside this range should not be used.

When contributing to an existing file, follow the majority convention in that
file. Consistency within a file is valued highly.

If you are new to Nemo development and do not already have a very strong
preference, new files should be started with 3 space indenting. This maximises
the likelihood that copy and paste between files will be straightforward, though
modern editors ease this to some degree.

Function signatures in docstrings should have four spaces before them.

Where possible, line lengths should not exceed 80 characters.

We use a term/factor convention for spacing. This means that all (additive)
terms have spaces before and after them, (multiplicative) factors usually do
not.

In practice this means that `+`, `-`, `=`, `==`, `!=`, `<`, `>`, `<=`, `>=` all
have spaces before and after them. The operators `*`, `/`, `^` and unary minus
do not.

As per English, commas are followed by a single space in expressions. This
applies for example to function arguments and tuples.

We do not put spaces immediately inside or before parentheses.

Colons used for ranges do not have spaces before or after them.

Logical operators, `&`, `|`, `&&`, etc. usually have spaces before and after
them.

### Comments

Despite appearances to the contrary, we now prefer code comments explaining the
algorithm as it proceeds.

The hash when used for a comment should always be followed by a space. Full
sentences are preferred.

We do not generally use comments in Nemo for questions, complaints or
proposals for future improvement. These are better off in a ticket on GitHub
with a discussion that will be brought to the attention of all relevant
parties.

Any (necessary) limitations of the implementation should be noted in
docstrings.

### Layout of files

In Nemo, all types are places in special files with the word "Types" in their
name, e.g. `FlintTypes.jl`. This is because Julia must be aware of all types
before they are used. Separation of types from implementations makes it easy
to ensure this happens.

Abstract types should be put in the file called `AbstractTypes.jl` at the top
level of the `src` directory.

Most implementation files present functions in a particular order, which is as
follows:

* A header stating what the file is for, and if needed, any copyright notices

* Functions applying to any "types" used in the file, e.g. `parent_type`,
  `elem_type`, `base_ring`, `parent`, `check_parent`.

* Basic manipulation, including hashes, predicates, getters/setters, functions
  for creating special values (e.g. `one`, `zero` and the like),
  `deepcopy_internal`. These are usually fairly short functions, often a single
  line.

* Indexing (`getindex`, `setindex`), iteration, views.

* String I/O (`expressify` and file access, etc.)

* Arithmetic operations, usually in multiple sections, such as unary
  operations, binary operations, ad hoc binary operations (e.g. multiplication
  of a complex object by a scalar), comparisons, ad hoc comparisons, division,
  etc.

* More complex functionality separated into sections based on functionality
  provided, e.g. gcd, interpolation, special functions, solving, etc.

* Functions for mapping between different types, coercion, changing base ring,
  etc.

* Unsafe operators, e.g. `mul!`, `add!`, `addeq!` etc.

* Random generation

* Promotion rules

* Parent object call overload (e.g. for implementing `R(2)` where `R` is an
  object representing a ring or field, etc.)

* Additional constructors, e.g. `matrix`, which might be used instead of a
  parent object to construct elements.

* Parent object constructors, e.g. `PolynomialRing`, etc.

The exact order within the file is less important than generally following
something like the above. This aids in finding functions in a file since all
files are more or less set out the same way.

For an example to follow, see the `src/Poly.jl` and `src/generic/Poly.jl` files
in AbstractAlgebra which form the oldest and most canonical example.

Headings for sections should be 80 characters wide and formed of hashes in the
style that can be seen in each Nemo file.

