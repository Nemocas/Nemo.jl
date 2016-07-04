```@meta
CurrentModule = Nemo
```

## Introduction

Finite fields are provided in Nemo by Flint. This allows construction of finite
fields of any characteristic and degree for which there are Conway polynomials.
It is also possible for the user to specify their own irreducible polynomial
generating a finite field.

Finite fields are constructed using the `FlintFiniteField` function. However,
for convenience we define

```
FiniteField = FlintFiniteField
```

so that finite fields can be constructed using `FiniteField` rather than
`FlintFiniteField`. Note that this is the name of the constructor, but not of
finite field type.

The types of finite field elements in Nemo are given in the following table,
along with the libraries that provide them and the associated types of the
parent objects.

 Library | Field                          | Element type  | Parent type
---------|--------------------------------|---------------|---------------------
Flint    | $\mathbb{F}_{p^n}$ (small $p$) | `fq_nmod`     | `FqNmodFiniteField`
Flint    | $\mathbb{F}_{p^n}$ (large $p$) | `fq`          | `FqFiniteField`

The only difference between the `fq` and `fq_nmod` types is the representation.
The former is for finite fields with multiprecision characteristic and the
latter is for characteristics that fit into a single unsigned machine word. The
`FlintFiniteField` constructor automatically picks the correct representation
for the user, and so the average user doesn't need to know about the actual
types.

All the finite field types belong to the `FinField` abstract type and the
finite field element types belong to the `FinFieldElem` abstract type.

Since all the functionality for the `fq` finite field type is identical to that
provided for the `fq_nmod` finite field type, we simply document the former.

## Finite field constructors

In order to construct finite field elements in Nemo, one must first construct
the finite field itself. This is accomplished with one of the following
constructors.

```@docs
FlintFiniteField(::fmpz, ::Int, ::AbstractString{})
FlintFiniteField(::Integer, ::Int, ::AbstractString{})
```

```@docs
FlintFiniteField(::fmpz_mod_poly, ::AbstractString{})
```

Here are some examples of creating finite fields and making use of the
resulting parent objects to coerce various elements into those fields.

```
R, x = FiniteField(7, 3, "x")
S, y = FiniteField(ZZ(12431351431561), 2, "y")
T, t = PolynomialRing(ResidueRing(ZZ, 12431351431561), "t")
U, z = FiniteField(t^2 + 7, "z")

a = R(5)
b = R(x)
c = S(ZZ(11))
d = U(7)
```

## Finite field element constructors

Once a finite field is constructed, there are various ways to construct
elements in that field.

Apart from coercing elements into the finite field as above, we offer the
following functions.

```@docs
zero(::FqFiniteField)
```

```@docs
one(::FqFiniteField)
```

```@docs
gen(::FqFiniteField)
```

Here are some examples of constructing finite field elements.

```
R, x = FiniteField(ZZ(7), 5, "x")

a = zero(R)
b = one(R)
c = gen(R)
```

## Basic functionality

The following basic functionality is provided by the default finite field
implementation in Nemo, to support construction of generic rings over finite
fields. Any custom finite field implementation in Nemo should provide these 
functions along with the usual arithmetic operations.

```
parent_type(::Type{fq})
```

Gives the type of the parent object of a Flint finite field element.

```
elem_type(R::FqFiniteField)
```

Given the parent object for a finite field, return the type of elements
of the field.

```
Base.hash(a::fq, h::UInt)
```

Return a `UInt` hexadecimal hash of the finite field element $a$. This should
be xor'd with a fixed random hexadecimal specific to the finite field type. The
hash of the coefficients of the finite field representation should be xor'd
with the supplied parameter `h` as part of computing the hash.

```
deepcopy(a::fq)
```

Construct a copy of the given finite field element and return it. This function
must recursively construct copies of all of the internal data in the given
element. Nemo finite field elements are mutable and so returning shallow copies
is not sufficient.

```
mul!(c::fq, a::fq, b::fq)
```

Multiply $a$ by $b$ and set the existing finite field element $c$ to the
result. This function is provided for performance reasons as it saves
allocating a new object for the result and eliminates associated garbage
collection.

```
addeq!(c::fq, a::fq)
```

In-place addition. Adds $a$ to $c$ and sets $c$ to the result. This function
is provided for performance reasons as it saves allocating a new object for
the result and eliminates associated garbage collection.

Given the parent object `R` for a finite field, the following coercion
functions are provided to coerce various elements into the finite field.
Developers provide these by overloading the `call` operator for the finite
field parent objects.

```
R()
```

Coerce zero into the finite field.

```
R(n::Integer)
R(f::fmpz)
```

Coerce an integer value into the finite field.

```
R(f::fq)
```

Take a finite field element that is already in the finite field and simply
return it. A copy of the original is not made.

In addition to the above, developers of custom finite field types must ensure
that they provide the equivalent of the function `base_ring(R::FqFiniteField)`
which should return `Union{}`. In addition to this they should ensure that
each finite field element contains a field `parent` specifying the parent
object of the finite field element, or at least supply the equivalent of the
function `parent(a::fq)` to return the parent object of a finite field element.

## Basic manipulation

Numerous functions are provided to manipulate finite field elements. Also see
the section on basic functionality above.

```@docs
base_ring(::FqFiniteField)
```

```@docs
base_ring(::fq)
```

```@docs
parent(::fq)
```

```@docs
iszero(::fq)
```

```@docs
isone(::fq)
```

```@docs
isunit(::fq)
```

```@docs
isgen(::fq)
```

```@docs
coeff(::fq, ::Int)
```

```@docs
degree(::FqFiniteField)
```

```@docs
characteristic(::FqFiniteField)
```

```@docs
order(::FqFiniteField)
```

Here are some examples of basic manipulation of finite field elements.

```
R, x = FiniteField(ZZ(7), 5, "x")

a = zero(R)
b = one(R)
c = gen(R)

d = characteristic(R)
f = order(R)
g = degree(R)
h = iszero(a)
k = isone(b)
m = isunit(x + 1)
n = isgen(x)
U = parent(x + 1)
V = base_ring(R)
```

## Arithmetic operations

Nemo provides all the standard field operations for finite field elements, as
follows.

Function               | Operation
-----------------------|----------------
-(a::fq)               | unary minus
+(a::fq, b::fq)        | addition
-(a::fq, b::fq)        | subtraction
*(a::fq, b::fq)        | multiplication
divexact(a::fq, b::fq) | exact division

In addition, the following ad hoc field operations are defined.

Function                    | Operation
----------------------------|----------------
+(a::fq, b::Integer)        | addition
+(a::Integer, b::fq)        | addition
+(a::fq, b::fmpz)           | addition
+(a::fmpz, b::fq)           | addition
-(a::fq, b::Integer)        | subtraction
-(a::Integer, b::fq)        | subtraction
-(a::fq, b::fmpz)           | subtraction
-(a::fmpz, b::fq)           | subtraction
*(a::fq, b::Integer)        | multiplication
*(a::Integer, b::fq)        | multiplication
*(a::fq, b::fmpz)           | multiplication
*(a::fmpz, b::fq)           | multiplication
divexact(a::fq, b::Integer) | exact division
divexact(a::fq, b::fmpz)    | exact division
divexact(a::Integer, b::fq) | exact division
divexact(a::fmpz, b::fq) | exact division
^(a::fq, b::Int)            | powering
^(a::fq, b::fmpz)           | powering

Here are some examples of arithmetic operations on finite fields.

```
R, x = FiniteField(ZZ(7), 5, "x")

a = x^4 + 3x^2 + 6x + 1
b = 3x^4 + 2x^2 + x + 1

c = a + b
d = a - b
f = a*b
g = 3a
h = b*ZZ(5)
k = divexact(a, b)
m = divexact(1, b)
n = divexact(a, ZZ(2))
p = a^3
```

## Comparison

Nemo provides the comparison operation `==` for finite field elements. Julia
then automatically provides the corresponding `!=` operation. Here are the
functions provided.

Function
-----------------
==(a::fq, b::fq)

In addition, the following ad hoc comparisons are provided, Julia again
providing the corresponding `!=` operators.

Function
----------------------
==(a::fq, b::Integer)
==(a::fq, b::fmpz)
==(a::Integer, b::fq)
==(a::fmpz, b::fq)

Here are some examples of comparisons.

```
R, x = FiniteField(ZZ(7), 5, "x")

a = x^4 + 3x^2 + 6x + 1
b = 3x^4 + 2x^2 + 2

b != a
a == 3
ZZ(5) == b
```

## Inversion

```@docs
inv(::fq)
```

Here are some examples of inversion.

```
R, x = FiniteField(ZZ(7), 5, "x")

a = x^4 + 3x^2 + 6x + 1

b = inv(a)
```

## Special functions


Various special functions with finite field specific behaviour are defined.

```@docs
trace(::fq)
```

```@docs
norm(::fq)
```

```@docs
frobenius(::fq, ::Int)
```

```@docs
pth_root(::fq)
```

Here are some examples of special functions.

```
R, x = FiniteField(ZZ(7), 5, "x")

a = x^4 + 3x^2 + 6x + 1

b = trace(a)
c = norm(a)
d = frobenius(a)
f = frobenius(a, 3)
g = pth_root(a)
```
