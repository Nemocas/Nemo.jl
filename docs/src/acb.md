```@meta
CurrentModule = Nemo
```

## Introduction

Arbitrary precision complex ball arithmetic is supplied by Arb which provides a
ball representation which tracks error bounds rigorously. Complex numbers are 
represented in in rectangular form $a+bi$ where $a,b$ are `arb` balls.

The Arb complex field is constructed using the `AcbField` constructor. This
constructs the parent object for the Arb complex field.

We define

```
ComplexField = AcbField
```

so that one can construct the Arb complex field parent using `ComplexField`
instead of `AcbField`.

The types of complex boxes in Nemo are given in the following table, along with
the libraries that provide them and the associated types of the parent objects.

 Library | Field                | Element type  | Parent type
---------|----------------------|---------------|--------------
Arb      | $\mathbb{C}$ (boxes) | `acb`         | `AcbField`

All the complex field types belong to the `Field` abstract type and the types of
elements in this field, i.e. complex boxes in this case, belong to the
`FieldElem` abstract type.

## Complex field constructors

In order to construct complex boxes in Nemo, one must first construct the Arb
complex field itself. This is accomplished with the following constructor.

```
AcbField(prec::Int)
```

Return the Arb complex field with precision in bits `prec` used for operations
on interval midpoints. The precision used for interval radii is a fixed
implementation-defined constant (30 bits).

We define

```
ComplexField = AcbField
```

so that one can use `ComplexField` instead of `AcbField`.

Here is an example of creating an Arb complex field and using the resulting
parent object to coerce values into the resulting field.

```
CC = ComplexField(64)

a = CC("0.25")
b = CC("0.1")
c = CC(0.5)
d = CC(12)
```

Note that whilst one can coerce double precision floating point values into an
Arb complex field, unless those values can be represented exactly in double
precision, the resulting ball can't be any more precise than the double
precision supplied.

If instead, values can be represented precisely using decimal arithmetic then
one can supply them to Arb using a string. In this case, Arb will store them to
the precision specified when creating the Arb complex field.

If the values can be stored precisely as a binary floating point number, Arb
will store the values exactly. See the function `isexact` below for more
information.

## Complex ball constructors

Once an Arb complex field is constructed, there are various ways to construct
boxes in that field.

Apart from coercing elements into the Arb complex field as above, we offer the
following functions.

```@docs
zero(::AcbField)
```

```@docs
one(::AcbField)
```

```@docs
onei(::AcbField)
```

Here are some examples of constructing complex boxes.

```
CC = ComplexField(64)

a = zero(CC)
b = one(CC)
c = onei(CC)
```

## Basic functionality

The following basic functionality is provided by the default Arb complex field
implementation in Nemo, to support construction of generic rings over complex
fields. Any custom complex field implementation in Nemo should provide analogues
of these functions along with the usual arithmetic operations.

```
parent_type(::Type{acb})
```

Gives the type of the parent object of an Arb complex field element.

```
elem_type(R::AcbField)
```

Given the parent object for an Arb complex field, return the type of elements
of the field.

```
mul!(c::acb, a::acb, b::acb)
```

Multiply $a$ by $b$ and set the existing Arb complex field element $c$ to the
result. This function is provided for performance reasons as it saves
allocating a new object for the result and eliminates associated garbage
collection.

```
addeq!(c::acb, a::acb)
```

In-place addition. Adds $a$ to $c$ and sets $c$ to the result. This function
is provided for performance reasons as it saves allocating a new object for
the result and eliminates associated garbage collection.

```
deepcopy(a::acb)
```

Return a copy of the Arb complex field element $a$, recursively copying the
internal data. Arb complex field elements are mutable in Nemo so a shallow
copy is not sufficient.

Given the parent object `R` for an Arb complex field, the following coercion
functions are provided to coerce various elements into the Arb complex field.
Developers provide these by overloading the `call` operator for the complex
field parent objects.

```
R()
```

Coerce zero into the Arb complex field.

```
R(n::Integer)
R(f::fmpz)
R(q::fmpq)
```

Coerce an integer or rational value into the Arb complex field.

```
R(f::Float64)
R(f::BigFloat)
```

Coerce the given floating point number into the Arb complex field.

```
R(f::AbstractString)
R(f::AbstractString, g::AbstractString)
```

Coerce the decimal number, given as a string, into the Arb complex field. In
each case $f$ is the real part and $g$ is the imaginary part.

```
R(f::arb)
```

Coerce the given Arb real ball into the Arb complex field.

```
R(f::acb)
```

Take an Arb complex field element that is already in an Arb field and simply
return it. A copy of the original is not made.

Here are some examples of coercing elements into the Arb complex field.

```
RR = RealField(64)
CC = ComplexField(64)

a = CC(3)
b = CC(QQ(2,3))
c = CC("3 +/- 0.0001")
d = CC("-1.24e+12345")
f = CC("nan +/- inf")
g = CC(RR(3))
```

In addition to the above, developers of custom complex field types must ensure
that they provide the equivalent of the function `base_ring(R::AcbField)`
which should return `Union{}`. In addition to this they should ensure that
each complex field element contains a field `parent` specifying the parent
object of the complex field element, or at least supply the equivalent of the
function `parent(a::acb)` to return the parent object of a complex field
element.

## Basic manipulation

Numerous functions are provided to manipulate Arb complex field elements. Also
see the section on basic functionality above.

```@docs
base_ring(::AcbField)
```

```@docs
base_ring(::acb)
```

```@docs
parent(::acb)
```

```@docs
iszero(::acb)
```

```@docs
isone(::acb)
```

```@docs
isfinite(::acb)
```

```@docs
isexact(::acb)
```

```@docs
isint(::acb)
```

```@docs
isreal(::acb)
```

```@docs
real(::acb)
```

```@docs
imag(::acb)
```

```@docs
accuracy_bits(::acb)
```

Here are some examples of basic manipulation of Arb complex boxes.

```
CC = ComplexField(64)

a = CC("1.2 +/- 0.001")
b = CC(3)

iszero(a)
isone(b)
isreal(a)
isfinite(b)
isint(b)
c = real(a)
d = imag(b)
f = accuracy_bits(a)
S = parent(a)
T = base_ring(CC)
```

## Arithmetic operations

Nemo provides all the standard field operations for Arb complex field elements,
as follows. Note that division is represented by `//` since a field is its own
fraction field and since exact division is not generally possible in an
inexact field.

Function                 | Operation
-------------------------|----------------
-(a::acb)                | unary minus
+(a::acb, b::acb)        | addition
-(a::acb, b::acb)        | subtraction
*(a::acb, b::acb)        | multiplication
//(a::acb, b::acb)       | division
^(a::acb, b::acb)        | powering

In addition, the following ad hoc field operations are defined.

Function               | Operation
-----------------------|----------------
+(a::acb, b::Integer)  | addition
+(a::Integer, b::acb)  | addition
+(a::acb, b::fmpz)     | addition
+(a::fmpz, b::acb)     | addition
+(a::acb, b::fmpq)     | addition
+(a::fmpq, b::acb)     | addition
+(a::acb, b::arb)      | addition
+(a::arb, b::acb)      | addition
-(a::acb, b::Integer)  | subtraction
-(a::Integer, b::acb)  | subtraction
-(a::acb, b::fmpz)     | subtraction
-(a::fmpz, b::acb)     | subtraction
-(a::acb, b::fmpq)     | subtraction
-(a::fmpq, b::acb)     | subtraction
-(a::acb, b::arb)      | subtraction
-(a::arb, b::acb)      | subtraction
*(a::acb, b::Integer)  | multiplication
*(a::Integer, b::acb)  | multiplication
*(a::acb, b::fmpz)     | multiplication
*(a::fmpz, b::acb)     | multiplication
*(a::acb, b::fmpq)     | multiplication
*(a::fmpq, b::acb)     | multiplication
*(a::acb, b::arb)      | multiplication
*(a::arb, b::acb)      | multiplication
//(a::acb, b::Integer) | division
//(a::acb, b::fmpz)    | division
//(a::acb, b::fmpq)    | division
//(a::Integer, b::acb) | division
//(a::fmpz, b::acb)    | division
//(a::fmpq, b::acb)    | division
//(a::arb, b::acb)     | division
^(a::acb, b::fmpq)     | powering
^(a::Integer, b::acb)  | powering
^(a::fmpz, b::acb)     | powering
^(a::fmpq, b::acb)     | powering
^(a::arb, b::acb)      | powering

Here are some examples of arithmetic operations on Arb complex boxes.

```
CC = ComplexField(64)

x = CC(3)
y = CC(QQ(2,3))

a = x + y
b = x*y
d = 1//y - x
d = 3x + ZZ(100)//y
f = (x^2 + y^2) ^ QQ(1,2)
```

## Containment

It is often necessary to determine whether a given exact value or box is
contained in a given complex box or whether two boxes overlap. The following
functions are provided for this purpose.

```@docs
overlaps(::acb, ::acb)
```

```@docs
contains(::acb, ::acb)
```

```@docs
contains(::acb, ::Integer)
contains(::acb, ::fmpz)
contains(::acb, ::fmpq)
```

The following functions are also provided for determining if a box intersects
a certain part of the complex number plane.

```@docs
contains_zero(::acb)
```

Here are some examples of testing containment.

```
CC = ComplexField(64)
x = CC("1 +/- 0.001")
y = CC("3")

overlaps(x, y)
contains(x, y)
contains(y, 3)
contains(x, ZZ(1)//2)
contains_zero(x)
```

## Comparison

Nemo provides a full range of comparison operations for Arb complex boxes. 

Firstly, we introduce an exact equality which is distinct from arithmetic
equality. This is distinct from arithmetic equality implemented by `==`, which
merely compares up to the minimum of the precisions of its operands.

```@docs
isequal(::acb, ::acb)
```

A full range of functions is available for comparing boxes, i.e. `==` and `!=`.
In fact, these are implemented directly in C. In the table below we document
these as though only `==` had been provided to Julia.

Function                
-------------------------
`==(x::acb, y::acb)`

As well as these, we provide a full range of ad hoc comparison operators.
Again, these are implemented directly in Julia, but we document them as though
only `==` were provided.

Function
-----------------------------
`==(x::acb, y::Integer)`
`==(x::Integer, y::acb)`
`==(x::acb, y::fmpz)`
`==(x::fmpz, y::acb)`
`==(x::arb, y::fmpz)`
`==(x::fmpz, y::arb)`
`==(x::acb, y::Float64)`
`==(x::Float64, y::acb)`

Here are some examples of comparison.

```
CC = ComplexField(64)
x = CC("1 +/- 0.001")
y = CC("3")
z = CC("4")

isequal(y, z)
x != z
x == 3
ZZ(3) == z
x != 1.23
```

## Absolute value

```@docs
abs(::acb)
```

Here are some examples of taking the absolute value.

```
CC = ComplexField(64)
x = CC("-1 +/- 0.001")

a = abs(x)
```

## Inverse

```@docs
inv(::acb)
```

Here are some examples of taking the inverse.

```
CC = ComplexField(64)
x = CC("-3 +/- 0.001")

a = inv(x)
```

## Shifting

```@docs
ldexp(x::acb, y::Int)
ldexp(x::acb, y::fmpz)
```

Here are some examples of shifting.

```
CC = ComplexField(64)
x = CC("-3 +/- 0.001")

a = ldexp(x, 23)
b = ldexp(x, -ZZ(15))
```

## Miscellaneous operations

```@docs
trim(::acb)
```

```@docs
unique_integer(::acb)
```

```@docs
conj(::acb)
```

```@docs
angle(::acb)
```

Here are some examples of miscellaneous operations.

```
CC = ComplexField(64)
x = CC("-3 +/- 0.001", "0.1")

a = trim(x)
b, c = unique_integer(x)
d = conj(x)
f = angle(x)
```

## Constants

```@docs
const_pi(::AcbField)
```


Here are some examples of computing complex constants.

```
CC = ComplexField(200)

a = const_pi(CC)
```

## Mathematical functions

```@docs
sqrt(::acb)
```

```@docs
rsqrt(::acb)
```

```@docs
log(::acb)
```

```@docs
log1p(::acb)
```

```@docs
exp(::acb)
```

```@docs
exppii(::acb)
```

```@docs
sin(::acb)
```

```@docs
cos(::acb)
```

```@docs
sinpi(::acb)
```

```@docs
cospi(::acb)
```

```@docs
tan(::acb)
```

```@docs
cot(::acb)
```

```@docs
tanpi(::acb)
```

```@docs
cotpi(::acb)
```

```@docs
sinh(::acb)
```

```@docs
cosh(::acb)
```

```@docs
tanh(::acb)
```

```@docs
coth(::acb)
```

```@docs
atan(::acb)
```

```@docs
logsinpi(::acb)
```

```@docs
gamma(::acb)
```

```@docs
lgamma(::acb)
```

```@docs
rgamma(::acb)
```

```@docs
digamma(::acb)
```

```@docs
zeta(::acb)
```

```@docs
barnesg(::acb)
```

```@docs
logbarnesg(::acb)
```

```@docs
erf(::acb)
```

```@docs
erfi(::acb)
```

```@docs
ei(::acb)
```

```@docs
si(::acb)
```

```@docs
ci(::acb)
```

```@docs
shi(::acb)
```

```@docs
chi(::acb)
```

```@docs
modeta(::acb)
```

```@docs
modj(::acb)
```

```@docs
modlambda(::acb)
```

```@docs
moddelta(::acb)
```

```@docs
ellipk(::acb)
```

```@docs
ellipe(::acb)
```

```@docs
sincos(::acb)
```

```@docs
sincospi(::acb)
```

```@docs
sinhcosh(::acb)
```

```@docs
agm(::acb)
agm(::acb, ::acb)
```

```@docs
polygamma(::acb, ::acb)
```

```@docs
zeta(::acb, ::acb)
```

```@docs
risingfac(::acb, ::Int)
```

```@docs
risingfac2(::acb, ::Int)
```

```@docs
polylog(::acb, ::acb)
```

```@docs
polylog(::Int, ::acb)
```

```@docs
li(::acb)
```

```@docs
lioffset(::acb)
```

```@docs
expint(::acb)
```

```@docs
gamma(::acb, ::acb)
```

```@docs
besselj(::acb, ::acb)
```

```@docs
bessely(::acb, ::acb)
```

```@docs
besseli(::acb, ::acb)
```

```@docs
besselk(::acb, ::acb)
```

```@docs
hyp1f1(::acb, ::acb, ::acb)
```

```@docs
hyp1f1r(::acb, ::acb, ::acb)
```

```@docs
hyperu(::acb, ::acb, ::acb)
```

```@docs
hyp2f1(::acb, ::acb, ::acb, ::acb)
```

```@docs
jtheta(::acb, ::acb)
```

```@docs
ellipwp(::acb, ::acb)
```

Here are some examples of complex valued mathematical functions.

```
CC = ComplexField(64)

s = CC(1, 2)
z = CC("1.23", "3.45")

a = sin(z)^2 + cos(z)^2
b = zeta(z)
c = besselj(s, z)
d = hyp1f1(s, s+1, z)
```
