```@meta
CurrentModule = Nemo
```

## Introduction

Arbitrary precision real ball arithmetic is supplied by Arb which provides a
ball representation which tracks error bounds rigorously. Real numbers are 
represented in mid-rad interval form $[m \pm r] = [m-r, m+r]$.

The Arb real field is constructed using the `ArbField` constructor. This
constructs the parent object for the Arb real field.

The types of real balls in Nemo are given in the following table, along with
the libraries that provide them and the associated types of the parent objects.

 Library | Field                | Element type  | Parent type
---------|----------------------|---------------|--------------
Arb      | $\mathbb{R}$ (balls) | `arb`         | `ArbField`

All the real field types belong to the `Field` abstract type and the types of
elements in this field, i.e. balls in this case, belong to the `FieldElem`
abstract type.

## Real field constructors

In order to construct real balls in Nemo, one must first construct the Arb
real field itself. This is accomplished with the following constructor.

```
ArbField(prec::Int)
```

Return the Arb field with precision in bits `prec` used for operations on
interval midpoints. The precision used for interval radii is a fixed
implementation-defined constant (30 bits).

Here is an example of creating an Arb real field and using the resulting
parent object to coerce values into the resulting field.

```
RR = ArbField(64)

a = RR("0.25")
b = RR("0.1")
c = RR(0.5)
d = RR(12)
```

Note that whilst one can coerce double precision floating point values into an
Arb real field, unless those values can be represented exactly in double
precision, the resulting ball can't be any more precise than the double
precision supplied.

If instead, values can be represented precisely using decimal arithmetic then
one can supply them to Arb using a string. In this case, Arb will store them to
the precision specified when creating the Arb field.

If the values can be stored precisely as a binary floating point number, Arb
will store the values exactly. See the function `isexact` below for more
information.

## Real ball constructors

Once an Arb real field is constructed, there are various ways to construct
balls in that field.

Apart from coercing elements into the Arb real field as above, we offer the
following functions.

```@docs
zero(::ArbField)
```

```@docs
one(::ArbField)
```

```@docs
ball(::arb, ::arb)
```

Here are some examples of constructing balls.

```
RR = ArbField(64)

a = zero(RR)
b = one(RR)
c = ball(RR(3), RR("0.0001"))
```

## Basic functionality

The following basic functionality is provided by the default Arb real field
implementation in Nemo, to support construction of generic rings over real
fields. Any custom real field implementation in Nemo should provide analogues
of these functions along with the usual arithmetic operations.

```
parent_type(::Type{arb})
```

Gives the type of the parent object of an Arb real field element.

```
elem_type(R::ArbField)
```

Given the parent object for a Arb field, return the type of elements
of the field.

```
mul!(c::arb, a::arb, b::arb)
```

Multiply $a$ by $b$ and set the existing Arb field element $c$ to the
result. This function is provided for performance reasons as it saves
allocating a new object for the result and eliminates associated garbage
collection.

```
addeq!(c::arb, a::arb)
```

In-place addition. Adds $a$ to $c$ and sets $c$ to the result. This function
is provided for performance reasons as it saves allocating a new object for
the result and eliminates associated garbage collection.

Given the parent object `R` for an Arb real field, the following coercion
functions are provided to coerce various elements into the Arb field.
Developers provide these by overloading the `call` operator for the real
field parent objects.

```
R()
```

Coerce zero into the Arb field.

```
R(n::Integer)
R(f::fmpz)
R(q::fmpq)
```

Coerce an integer or rational value into the Arb field.

```
R(f::Float64)
R(f::BigFloat)
```

Coerce the given floating point number into the Arb field.

```
R(f::AbstractString)
```

Coerce the decimal number, given as a string, into the Arb field.

```
R(f::arb)
```

Take an Arb field element that is already in the Arb field and simply
return it. A copy of the original is not made.

Here are some examples of coercing elements into the Arb field.

```
RR = ArbField(64)

a = RR(3)
b = RR(QQ(2,3))
c = ball(RR(3), RR("0.0001"))
d = RR("3 +/- 0.0001")
f = RR("-1.24e+12345")
g = RR("nan +/- inf")
```

In addition to the above, developers of custom real field types must ensure
that they provide the equivalent of the function `base_ring(R::ArbField)`
which should return `Union{}`. In addition to this they should ensure that
each real field element contains a field `parent` specifying the parent
object of the real field element, or at least supply the equivalent of the
function `parent(a::arb)` to return the parent object of a real field element.

## Conversions

```@docs
convert(::Type{Float64}, ::arb)
```

## Basic manipulation

Numerous functions are provided to manipulate Arb field elements. Also see
the section on basic functionality above.

```@docs
base_ring(::ArbField)
```

```@docs
base_ring(::arb)
```

```@docs
parent(::arb)
```

```@docs
iszero(::arb)
```

```@docs
isnonzero(::arb)
```

```@docs
isone(::arb)
```

```@docs
isfinite(::arb)
```

```@docs
isexact(::arb)
```

```@docs
isint(::arb)
```

```@docs
ispositive(::arb)
```

```@docs
isnonnegative(::arb)
```

```@docs
isnegative(::arb)
```

```@docs
isnonpositive(::arb)
```

```@docs
midpoint(::arb)
```

```@docs
radius(::arb)
```

```@docs
accuracy_bits(::arb)
```

Here are some examples of basic manipulation of Arb balls.

```
RR = ArbField(64)

a = RR("1.2 +/- 0.001")
b = RR(3)

iszero(a)
isone(b)
ispositive(a)
isfinite(b)
isint(b)
isnegative(a)
c = radius(a)
d = midpoint(b)
f = accuracy_bits(a)
S = parent(a)
T = base_ring(RR)
```

## Arithmetic operations

Nemo provides all the standard field operations for Arb field elements, as
follows. Note that division is represented by `//` since a field is its own
fraction field and since exact division is not generally possible in an
inexact field.

Function                 | Operation
-------------------------|----------------
-(a::arb)                | unary minus
+(a::arb, b::arb)        | addition
-(a::arb, b::arb)        | subtraction
*(a::arb, b::arb)        | multiplication
//(a::arb, b::arb)       | division
^(a::arb, b::arb)        | powering

In addition, the following ad hoc field operations are defined.

Function               | Operation
-----------------------|----------------
+(a::arb, b::Integer)  | addition
+(a::Integer, b::arb)  | addition
+(a::arb, b::fmpz)     | addition
+(a::fmpz, b::arb)     | addition
-(a::arb, b::Integer)  | subtraction
-(a::Integer, b::arb)  | subtraction
-(a::arb, b::fmpz)     | subtraction
-(a::fmpz, b::arb)     | subtraction
*(a::arb, b::Integer)  | multiplication
*(a::Integer, b::arb)  | multiplication
*(a::arb, b::fmpz)     | multiplication
*(a::fmpz, b::arb)     | multiplication
//(a::arb, b::Integer) | division
//(a::arb, b::fmpz)    | division
//(a::Integer, b::arb) | division
//(a::fmpz, b::arb)    | division
^(a::arb, b::Integer)  | powering
^(a::arb, b::fmpz)     | powering
^(a::arb, b::fmpq)     | powering


Here are some examples of arithmetic operations on Arb balls.

```
RR = ArbField(64)

x = RR(3)
y = RR(QQ(2,3))

a = x + y
b = x*y
d = 1//y - x
d = 3x + ZZ(100)//y
f = (x^2 + y^2) ^ QQ(1,2)
```

## Containment

It is often necessary to determine whether a given exact value or ball is
contained in a given real ball or whether two balls overlap. The following
functions are provided for this purpose.

```@docs
overlaps(::arb, ::arb)
```

```@docs
contains(::arb, ::arb)
```

```@docs
contains(::arb, ::Integer)
contains(::arb, ::fmpz)
contains(::arb, ::fmpq)
contains(::arb, ::BigFloat)
```

The following functions are also provided for determining if a ball intersects
a certain part of the real number line.

```@docs
contains_zero(::arb)
```

```@docs
contains_negative(::arb)
```

```@docs
contains_positive(::arb)
```

```@docs
contains_nonnegative(::arb)
```

```@docs
contains_nonpositive(::arb)
```

Here are some examples of testing containment.

```
RR = ArbField(64)
x = RR("1 +/- 0.001")
y = RR("3")

overlaps(x, y)
contains(x, y)
contains(y, 3)
contains(x, ZZ(1)//2)
contains_zero(x)
contains_positive(y)
```

## Comparison

Nemo provides a full range of comparison operations for Arb balls. Note that a
ball is considered less than another ball if every value in the first ball is
less than every value in the second ball, etc.

Firstly, we introduce an exact equality which is distinct from arithmetic
equality. This is distinct from arithmetic equality implemented by `==`, which
merely compares up to the minimum of the precisions of its operands.

```@docs
isequal(::arb, ::arb)
```

A full range of functions is available for comparing balls, i.e. `==`, `!=`,
`<`, `<=`, `>=`, `>`. In fact, all these are implemented directly in C. In the
table below we document these as though only `==` and `isless` had been
provided to Julia.

Function                
-------------------------
`isless(x::arb, y::arb)` 
`==(x::arb, y::arb)`

As well as these, we provide a full range of ad hoc comparison operators.
Again, these are implemented directly in Julia, but we document them as though
`isless` and `==` were provided.

Function
-----------------------------
`isless(x::arb, y::Integer)`
`isless(x::Integer, y::arb)`
`isless(x::arb, y::fmpz)`
`isless(x::fmpz, y::arb)`
`isless(x::arb, y::Float64)`
`isless(x::Float64, y::arb)`

Here are some examples of comparison.

```
RR = ArbField(64)
x = RR("1 +/- 0.001")
y = RR("3")
z = RR("4")

isequal(y, z)
x <= z
x == 3
ZZ(3) < z
x != 1.23
```

## Absolute value

```@docs
abs(::arb)
```

Here are some examples of taking the absolute value.

```
RR = ArbField(64)
x = RR("-1 +/- 0.001")

a = abs(x)
```

## Inverse

```@docs
inv(::arb)
```

Here are some examples of taking the inverse.

```
RR = ArbField(64)
x = RR("-3 +/- 0.001")

a = inv(x)
```

## Shifting

```@docs
ldexp(x::arb, y::Int)
ldexp(x::arb, y::fmpz)
```

Here are some examples of shifting.

```
RR = ArbField(64)
x = RR("-3 +/- 0.001")

a = ldexp(x, 23)
b = ldexp(x, -ZZ(15))
```

## Miscellaneous operations

```@docs
trim(::arb)
```

```@docs
unique_integer(::arb)
```

```@docs
setunion(::arb, ::arb)
```

Here are some examples of miscellaneous operations.

```
RR = ArbField(64)
x = RR("-3 +/- 0.001")
y = RR("2 +/- 0.5")

a = trim(x)
b, c = unique_integer(x)
d = setunion(x, y)
```

## Constants

```@docs
const_pi(::ArbField)
```

```@docs
const_e(::ArbField)
```

```@docs
const_log2(::ArbField)
```

```@docs
const_log10(::ArbField)
```

```@docs
const_euler(::ArbField)
```

```@docs
const_catalan(::ArbField)
```

```@docs
const_khinchin(::ArbField)
```

```@docs
const_glaisher(::ArbField)
```

Here are some examples of computing real constants.

```
RR = ArbField(200)

a = const_pi(RR)
b = const_e(RR)
c = const_euler(RR)
d = const_glaisher(RR)
```

## Mathematical functions

```@docs
floor(::arb)
```

```@docs
ceil(::arb)
```

```@docs
sqrt(::arb)
```

```@docs
rsqrt(::arb)
```

```@docs
sqrt1pm1(::arb)
```

```@docs
log(::arb)
```

```@docs
log1p(::arb)
```

```@docs
exp(::arb)
```

```@docs
expm1(::arb)
```

```@docs
sin(::arb)
```

```@docs
cos(::arb)
```

```@docs
sinpi(::arb)
```

```@docs
cospi(::arb)
```

```@docs
tan(::arb)
```

```@docs
cot(::arb)
```

```@docs
tanpi(::arb)
```

```@docs
cotpi(::arb)
```

```@docs
sinh(::arb)
```

```@docs
cosh(::arb)
```

```@docs
tanh(::arb)
```

```@docs
coth(::arb)
```

```@docs
atan(::arb)
```

```@docs
asin(::arb)
```

```@docs
acos(::arb)
```

```@docs
atanh(::arb)
```

```@docs
asinh(::arb)
```

```@docs
acosh(::arb)
```

```@docs
gamma(::arb)
```

```@docs
lgamma(::arb)
```

```@docs
rgamma(::arb)
```

```@docs
digamma(::arb)
```

```@docs
zeta(::arb)
```

```@docs
sincos(::arb)
```

```@docs
sincospi(::arb)
```

```@docs
sinpi(::fmpq, ::ArbField)
```

```@docs
cospi(::fmpq, ::ArbField)
```

```@docs
sincospi(::fmpq, ::ArbField)
```

```@docs
sinhcosh(::arb)
```

```@docs
atan2(::arb, ::arb)
```

```@docs
agm(::arb, ::arb)
```

```@docs
zeta(::arb, ::arb)
```

```@docs
hypot(::arb, ::arb)
```

```@docs
root(::arb, ::Int)
```

```@docs
fac(::arb)
```

```@docs
fac(::Int, ::ArbField)
```

```@docs
binom(::arb, ::UInt)
```

```@docs
binom(::UInt, ::UInt, ::ArbField)
```

```@docs
fib(::fmpz, ::ArbField)
```

```@docs
fib(::Int, ::ArbField)
```

```@docs
gamma(::fmpz, ::ArbField)
```

```@docs
gamma(::fmpq, ::ArbField)
```

```@docs
zeta(::Int, ::ArbField)
```

```@docs
bernoulli(::Int, ::ArbField)
```

```@docs
risingfac(::arb, ::Int)
```

```@docs
risingfac(::fmpq, ::Int, ::ArbField)
```

```@docs
risingfac2(::arb, ::Int)
```

```@docs
polylog(::arb, ::arb)
```

```@docs
polylog(::Int, ::arb)
```

```@docs
chebyshev_t(::Int, ::arb)
```

```@docs
chebyshev_u(::Int, ::arb)
```

```@docs
chebyshev_t2(::Int, ::arb)
```

```@docs
chebyshev_u2(::Int, ::arb)
```

```@docs
bell(::fmpz, ::ArbField)
```

```@docs
bell(::Int, ::ArbField)
```

Here are some examples of real valued mathematical functions.

```
RR = ArbField(64)

a = floor(exp(RR(1)))
b = sinpi(QQ(5,6), RR)
c = gamma(QQ(1,3), ArbField(256))
d = bernoulli(1000, ArbField(53))
f = polylog(3, RR(-10))
```
