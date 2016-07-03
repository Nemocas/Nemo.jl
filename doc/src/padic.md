```@meta
CurrentModule = Nemo
```

## Introduction

P-adic fields are provided in Nemo by Flint. This allows construction of
$p$-adic fields for any prime $p$.

P-adic fields are constructed using the `FlintPadicField` function. However,
for convenience we define

```
PadicField = FlintPadicField
```

so that $p$-adic fields can be constructed using `PadicField` rather than
`FlintPadicField`. Note that this is the name of the constructor, but not of
padic field type.

The types of $p$-adic fields in Nemo are given in the following table, along
with the libraries that provide them and the associated types of the parent
objects.

 Library | Field            | Element type | Parent type
---------|----------------|----------------|---------------------
Flint    | $\mathbb{Q}_p$ | `padic`        | `PadicField`

All the $p$-adic field types belong to the `Field` abstract type and the
$p$-adic field element types belong to the `FieldElem` abstract type.

## P-adic field constructors

In order to construct $p$-adic field elements in Nemo, one must first construct
the $p$-adic field itself. This is accomplished with one of the following
constructors.

```@docs
FlintPadicField(::Integer, ::Int)
```

It is also possible to call the inner constructor directly. It has the following
form.

```
FlintPadicField(p::fmpz, prec::Int)
```

Returns the parent object for the $p$-adic field for given prime $p$, where
the default absolute precision of elements of the field is given by `prec`.

Here are some examples of creating $p$-adic fields and making use of the
resulting parent objects to coerce various elements into those fields.

```
R = PadicField(7, 30)
S = PadicField(ZZ(65537), 30)

a = R()
b = S(1)
c = S(ZZ(123))
d = R(ZZ(1)//7^2)
```

## P-adic field element constructors

Once a $p$-adic field is constructed, there are various ways to construct
elements in that field.

Apart from coercing elements into the $p$-adic field as above, we offer the
following functions.

```@docs
zero(::FlintPadicField)
```

```@docs
one(::FlintPadicField)
```

Elements can also be constructed using the big-oh notation. For this purpose we
define the following functions.

```@docs
O(::FlintPadicField, ::Integer)
O(::FlintPadicField, ::fmpz)
O(::FlintPadicField, ::fmpq)
```

The $O(p^n)$ construction can be used to construct $p$-adic values of precision
$n$ by adding it to integer values representing the $p$-adic value modulo
$p^n$ as in the examples.

Here are some examples of constructing $p$-adic field elements.

```
R = PadicField(7, 30)
S = PadicField(fmpz(65537), 30)

a = one(R)
b = zero(S)
c = 1 + 2*7 + 4*7^2 + O(R, 7^3)
d = 13 + 357*ZZ(65537) + O(S, ZZ(65537)^12)
f = ZZ(1)//7^2 + ZZ(2)//7 + 3 + 4*7 + O(R, 7^2)
```

Beware that the expression `1 + 2*p + 3*p^2 + O(R, p^n)` is actually computed
as a normal Julia expression. Therefore if \code{Int} values are used instead
of Flint integers or Julia bignums, overflow may result in evaluating the
value.

## Basic functionality

The following basic functionality is provided by the default $p$-adic field
implementation in Nemo, to support construction of generic rings over $p$-adic
fields. Any custom $p$-adic field implementation in Nemo should provide these 
functions along with the usual arithmetic operations.

```
parent_type(::Type{padic})
```

Gives the type of the parent object of a Flint $p$-adic field element.

```
elem_type(R::FlintPadicField)
```

Given the parent object for a $p$-adic field, return the type of elements
of the field.

```
Base.hash(a::padic, h::UInt)
```

Return a `UInt` hexadecimal hash of the $p$-adic field element $a$. This should
be xor'd with a fixed random hexadecimal specific to the $p$-adic field type. The
hash of the representative of the $p$-adic field element (lifted to $\mathbb{Q}$
and the prime $p$ for the field, should be xor'd with the supplied parameter `h`
as part of computing the hash.

```
deepcopy(a::padic)
```

Construct a copy of the given $p$-adic field element and return it. This function
must recursively construct copies of all of the internal data in the given
element. Nemo $p$-adic field elements are mutable and so returning shallow copies
is not sufficient.

```
mul!(c::padic, a::padic, b::padic)
```

Multiply $a$ by $b$ and set the existing $p$-adic field element $c$ to the
result. This function is provided for performance reasons as it saves
allocating a new object for the result and eliminates associated garbage
collection.

```
addeq!(c::padic, a::padic)
```

In-place addition. Adds $a$ to $c$ and sets $c$ to the result. This function
is provided for performance reasons as it saves allocating a new object for
the result and eliminates associated garbage collection.

Given the parent object `R` for a $p$-adic field, the following coercion
functions are provided to coerce various elements into the $p$-adic field.
Developers provide these by overloading the `call` operator for the $p$-adic
field parent objects.

```
R()
```

Coerce zero into the $p$-adic field.

```
R(n::Integer)
R(f::fmpz)
R(f::fmpq)
```

Coerce an integer or rational value into the $p$-adic field.

```
R(f::padic)
```

Take a $p$-adic field element that is already in the $p$-adic field and simply
return it. A copy of the original is not made.

In addition to the above, developers of custom $p$-adic field types must ensure
that they provide the equivalent of the function
`base_ring(R::FlintPadicField)` which should return `Union{}`. In addition to
this they should ensure that each $p$-adic field element contains a field
`parent` specifying the parent object of the $p$-adic field element, or at
least supply the equivalent of the function `parent(a::padic)` to return the
parent object of a $p$-adic field element.

## Basic manipulation

Numerous functions are provided to manipulate $p$-adic field elements. Also see
the section on basic functionality above.

```@docs
base_ring(::FlintPadicField)
```

```@docs
base_ring(::padic)
```

```@docs
parent(::padic)
```

```@docs
iszero(::padic)
```

```@docs
isone(::padic)
```

```@docs
isunit(::padic)
```

```@docs
prime(::FlintPadicField)
```

```@docs
precision(::padic)
```

```@docs
valuation(::padic)
```

```@docs
lift(::FlintIntegerRing, ::padic)
lift(::FlintRationalField, ::padic)
```

Here are some examples of basic manipulation of $p$-adic field elements.

```
R = PadicField(7, 30)

a = 1 + 2*7 + 4*7^2 + O(R, 7^3)
b = 7^2 + 3*7^3 + O(R, 7^5)
c = R(2)

d = one(R)
f = zero(R)
g = isone(d)
h = iszero(f)
k = precision(a)
m = prime(R)
n = valuation(b)
p = lift(FlintZZ, a)
q = lift(FlintQQ, divexact(a, b))
```

## Arithmetic operations

Nemo provides all the standard field operations for $p$-adic field elements, as
follows.

Function                     | Operation
-----------------------------|----------------
-(a::padic)                  | unary minus
+(a::padic, b::padic)        | addition
-(a::padic, b::padic)        | subtraction
*(a::padic, b::padic)        | multiplication
divexact(a::padic, b::padic) | exact division

In addition, the following ad hoc field operations are defined.

Function                       | Operation
-------------------------------|----------------
+(a::padic, b::Integer)        | addition
+(a::Integer, b::padic)        | addition
+(a::padic, b::fmpz)           | addition
+(a::fmpz, b::padic)           | addition
+(a::padic, b::fmpq)           | addition
+(a::fmpq, b::padic)           | addition
-(a::padic, b::Integer)        | subtraction
-(a::Integer, b::padic)        | subtraction
-(a::padic, b::fmpz)           | subtraction
-(a::fmpz, b::padic)           | subtraction
-(a::padic, b::fmpq)           | subtraction
-(a::fmpq, b::padic)           | subtraction
*(a::padic, b::Integer)        | multiplication
*(a::Integer, b::padic)        | multiplication
*(a::padic, b::fmpz)           | multiplication
*(a::fmpz, b::padic)           | multiplication
*(a::padic, b::fmpq)           | multiplication
*(a::fmpq, b::padic)           | multiplication
divexact(a::padic, b::Integer) | exact division
divexact(a::padic, b::fmpz)    | exact division
divexact(a::padic, b::fmpq)    | exact division
divexact(a::Integer, b::padic) | exact division
divexact(a::fmpz, b::padic)    | exact division
divexact(a::fmpq, b::padic)    | exact division
^(a::padic, b::Int)            | powering

Here are some examples of arithmetic operations on $p$-adic field elements.

```
R = PadicField(7, 30)

a = 1 + 2*7 + 4*7^2 + O(R, 7^3)
b = 7^2 + 3*7^3 + O(R, 7^5)
c = O(R, 7^3)
d = R(2)

f = a + b
g = a - b
h = a*b
j = b*c
k = a*d
m = a + 2
n = 3 - b
p = a*ZZ(5)
q = ZZ(3)*c
r = 2*d
s = 2 + d
t = d - ZZ(2)
u = a + ZZ(1)//7^2
v = (ZZ(12)//11)*b
w = c*(ZZ(1)//7)
```

## Comparison

Nemo provides the comparison operation `==` for $p$-adic field elements. Julia
then automatically provides the corresponding `!=` operation. Here are the
functions provided.

Function
----------------------------
==(a::padic, b::padic)
isequal(a::padic, b::padic)

Note that `==` returns `true` if its arguments are arithmetically equal to the
minimum of the two precisions. The `isequal` function requires them to both be
the same precision, as for power series.

In addition, the following ad hoc comparisons are provided, Julia again
providing the corresponding `!=` operators.

Function
----------------------
==(a::padic, b::Integer)
==(a::padic, b::fmpz)
==(a::padic, b::fmpq)
==(a::Integer, b::padic)
==(a::fmpz, b::padic)
==(a::fmpq, b::padic)

Here are some examples of comparisons.

```
R = PadicField(7, 30)

a = 1 + 2*7 + 4*7^2 + O(R, 7^3)
b = 3*7^3 + O(R, 7^5)
c = O(R, 7^3)
d = R(2)

a == 1 + 2*7 + O(R, 7^2)
b == c
isequal(a, b)
c == R(0)
d == R(2)
ZZ(3) == d
ZZ(3)//7 == c
```

## Inversion

```@docs
inv(::padic)
```

Here are some examples of inversion.

```
R = PadicField(7, 30)

a = 1 + 7 + 2*7^2 + O(R, 7^3)
b = 2 + 3*7 + O(R, 7^5)
c = 7^2 + 2*7^3 + O(R, 7^4)
d = 7 + 2*7^2 + O(R, 7^5)

f = inv(a)
g = inv(b)
h = inv(c)
k = inv(d)
l = inv(R(1))
```

## GCD

```@docs
gcd(::padic, ::padic)
```

Here are some examples of greatest common divisor.

```
R = PadicField(7, 30)

a = 1 + 7 + 2*7^2 + O(R, 7^3)
b = 2 + 3*7 + O(R, 7^5)

d = gcd(a, b)
f = gcd(R(0), R(0))
```

## Square root

```@docs
sqrt(::padic)
```

Here are some examples of taking the square root.

```
R = PadicField(7, 30)

a = 1 + 7 + 2*7^2 + O(R, 7^3)
b = 2 + 3*7 + O(R, 7^5)
c = 7^2 + 2*7^3 + O(R, 7^4)

d = sqrt(a)
f = sqrt(b)
f = sqrt(c)
g = sqrt(R(121))
```

## Special functions

```@docs
exp(::padic)
```

```@docs
log(::padic)
```

```@docs
teichmuller(::padic)
```

R = PadicField(7, 30)

a = 1 + 7 + 2*7^2 + O(R, 7^3)
b = 2 + 5*7 + 3*7^2 + O(R, 7^3)
c = 3*7 + 2*7^2 + O(R, 7^5)

c = exp(c)
d = log(a)
c = exp(R(0))
d = log(R(1))
f = teichmuller(b) 
