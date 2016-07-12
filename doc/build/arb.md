


<a id='Introduction-1'></a>

## Introduction


Arbitrary precision real ball arithmetic is supplied by Arb which provides a ball representation which tracks error bounds rigorously. Real numbers are  represented in mid-rad interval form $[m \pm r] = [m-r, m+r]$.


The Arb real field is constructed using the `ArbField` constructor. This constructs the parent object for the Arb real field.


The types of real balls in Nemo are given in the following table, along with the libraries that provide them and the associated types of the parent objects.


Library |                Field | Element type | Parent type
------: | -------------------: | -----------: | ----------:
    Arb | $\mathbb{R}$ (balls) |        `arb` |  `ArbField`


All the real field types belong to the `Field` abstract type and the types of elements in this field, i.e. balls in this case, belong to the `FieldElem` abstract type.


<a id='Real-field-constructors-1'></a>

## Real field constructors


In order to construct real balls in Nemo, one must first construct the Arb real field itself. This is accomplished with the following constructor.


```
ArbField(prec::Int)
```


Return the Arb field with precision in bits `prec` used for operations on interval midpoints. The precision used for interval radii is a fixed implementation-defined constant (30 bits).


Here is an example of creating an Arb real field and using the resulting parent object to coerce values into the resulting field.


```
RR = ArbField(64)

a = RR("0.25")
b = RR("0.1")
c = RR(0.5)
d = RR(12)
```


Note that whilst one can coerce double precision floating point values into an Arb real field, unless those values can be represented exactly in double precision, the resulting ball can't be any more precise than the double precision supplied.


If instead, values can be represented precisely using decimal arithmetic then one can supply them to Arb using a string. In this case, Arb will store them to the precision specified when creating the Arb field.


If the values can be stored precisely as a binary floating point number, Arb will store the values exactly. See the function `isexact` below for more information.


<a id='Real-ball-constructors-1'></a>

## Real ball constructors


Once an Arb real field is constructed, there are various ways to construct balls in that field.


Apart from coercing elements into the Arb real field as above, we offer the following functions.

<a id='Base.zero-Tuple{Nemo.ArbField}' href='#Base.zero-Tuple{Nemo.ArbField}'>#</a>
**`Base.zero`** &mdash; *Method*.



```
zero(R::ArbField)
```

> Return exact zero in the given Arb field.


<a id='Base.one-Tuple{Nemo.ArbField}' href='#Base.one-Tuple{Nemo.ArbField}'>#</a>
**`Base.one`** &mdash; *Method*.



```
one(R::ArbField)
```

> Return exact one in the given Arb field.


<a id='Nemo.ball-Tuple{Nemo.arb,Nemo.arb}' href='#Nemo.ball-Tuple{Nemo.arb,Nemo.arb}'>#</a>
**`Nemo.ball`** &mdash; *Method*.



```
ball(mid::arb, rad::arb)
```

> Constructs an `arb` enclosing the range $[m-|r|, m+|r|]$, given the pair $(m, r)$.



Here are some examples of constructing balls.


```
RR = ArbField(64)

a = zero(RR)
b = one(RR)
c = ball(RR(3), RR("0.0001"))
```


<a id='Basic-functionality-1'></a>

## Basic functionality


The following basic functionality is provided by the default Arb real field implementation in Nemo, to support construction of generic rings over real fields. Any custom real field implementation in Nemo should provide analogues of these functions along with the usual arithmetic operations.


```
parent_type(::Type{arb})
```


Gives the type of the parent object of an Arb real field element.


```
elem_type(R::ArbField)
```


Given the parent object for a Arb field, return the type of elements of the field.


```
mul!(c::arb, a::arb, b::arb)
```


Multiply $a$ by $b$ and set the existing Arb field element $c$ to the result. This function is provided for performance reasons as it saves allocating a new object for the result and eliminates associated garbage collection.


```
addeq!(c::arb, a::arb)
```


In-place addition. Adds $a$ to $c$ and sets $c$ to the result. This function is provided for performance reasons as it saves allocating a new object for the result and eliminates associated garbage collection.


Given the parent object `R` for an Arb real field, the following coercion functions are provided to coerce various elements into the Arb field. Developers provide these by overloading the `call` operator for the real field parent objects.


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


Take an Arb field element that is already in the Arb field and simply return it. A copy of the original is not made.


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


In addition to the above, developers of custom real field types must ensure that they provide the equivalent of the function `base_ring(R::ArbField)` which should return `Union{}`. In addition to this they should ensure that each real field element contains a field `parent` specifying the parent object of the real field element, or at least supply the equivalent of the function `parent(a::arb)` to return the parent object of a real field element.


<a id='Conversions-1'></a>

## Conversions

<a id='Base.convert-Tuple{Type{Float64},Nemo.arb}' href='#Base.convert-Tuple{Type{Float64},Nemo.arb}'>#</a>
**`Base.convert`** &mdash; *Method*.



```
convert(::Type{Float64}, x::arb)
```

> Return the midpoint of $x$ rounded down to a machine double.



<a id='Basic-manipulation-1'></a>

## Basic manipulation


Numerous functions are provided to manipulate Arb field elements. Also see the section on basic functionality above.

<a id='Nemo.base_ring-Tuple{Nemo.ArbField}' href='#Nemo.base_ring-Tuple{Nemo.ArbField}'>#</a>
**`Nemo.base_ring`** &mdash; *Method*.



```
base_ring(R::ArbField)
```

> Returns `Union{}` since an Arb field does not depend on any other ring.


<a id='Nemo.base_ring-Tuple{Nemo.arb}' href='#Nemo.base_ring-Tuple{Nemo.arb}'>#</a>
**`Nemo.base_ring`** &mdash; *Method*.



```
base_ring(x::arb)
```

> Returns `Union{}` since an Arb field does not depend on any other ring.


<a id='Base.parent-Tuple{Nemo.arb}' href='#Base.parent-Tuple{Nemo.arb}'>#</a>
**`Base.parent`** &mdash; *Method*.



```
parent(A)
```

Returns the "parent array" of an array view type (e.g., `SubArray`), or the array itself if it is not a view

```
parent(a::padic)
```

> Returns the parent of the given p-adic field element.


```
parent(a::nf_elem)
```

> Return the parent of the given number field element.


```
parent(a::fq)
```

> Returns the parent of the given finite field element.


```
parent(a::FracElem)
```

> Return the parent object of the given fraction element.


```
parent(a::MatElem)
```

> Return the parent object of the given matrix.


```
parent(a::SeriesElem)
```

> Return the parent of the given power series.


```
parent(a::PolyElem)
```

> Return the parent of the given polynomial.


```
parent(a::ResElem)
```

> Return the parent object of the given residue element.


```
parent(a::fmpz)
```

> Returns the unique Flint integer parent object `FlintZZ`.


```
parent(a::perm)
```

> Return the parent of the given permutation group element.


<a id='Nemo.iszero-Tuple{Nemo.arb}' href='#Nemo.iszero-Tuple{Nemo.arb}'>#</a>
**`Nemo.iszero`** &mdash; *Method*.



```
iszero(x::arb)
```

> Return `true` if $x$ is certainly zero, otherwise return `false`.


<a id='Nemo.isnonzero-Tuple{Nemo.arb}' href='#Nemo.isnonzero-Tuple{Nemo.arb}'>#</a>
**`Nemo.isnonzero`** &mdash; *Method*.



```
isnonzero(x::arb)
```

> Return `true` if $x$ is certainly not equal to zero, otherwise return `false`.


<a id='Nemo.isone-Tuple{Nemo.arb}' href='#Nemo.isone-Tuple{Nemo.arb}'>#</a>
**`Nemo.isone`** &mdash; *Method*.



```
isone(x::arb)
```

> Return `true` if $x$ is certainly not equal to oneo, otherwise return `false`.


<a id='Base.isfinite-Tuple{Nemo.arb}' href='#Base.isfinite-Tuple{Nemo.arb}'>#</a>
**`Base.isfinite`** &mdash; *Method*.



```
isfinite(x::arb)
```

> Return `true` if $x$ is finite, i.e. having finite midpoint and radius, otherwise return `false`.


<a id='Nemo.isexact-Tuple{Nemo.arb}' href='#Nemo.isexact-Tuple{Nemo.arb}'>#</a>
**`Nemo.isexact`** &mdash; *Method*.



```
isexact(x::arb)
```

> Return `true` if $x$ is exact, i.e. has zero radius, otherwise return `false`.


<a id='Nemo.isint-Tuple{Nemo.arb}' href='#Nemo.isint-Tuple{Nemo.arb}'>#</a>
**`Nemo.isint`** &mdash; *Method*.



```
isint(x::arb)
```

> Return `true` if $x$ is an exact integer, otherwise return `false`.


<a id='Nemo.ispositive-Tuple{Nemo.arb}' href='#Nemo.ispositive-Tuple{Nemo.arb}'>#</a>
**`Nemo.ispositive`** &mdash; *Method*.



```
ispositive(x::arb)
```

> Return `true` if $x$ is certainly positive, otherwise return `false`.


<a id='Nemo.isnonnegative-Tuple{Nemo.arb}' href='#Nemo.isnonnegative-Tuple{Nemo.arb}'>#</a>
**`Nemo.isnonnegative`** &mdash; *Method*.



```
isnonnegative(x::arb)
```

> Return `true` if $x$ is certainly nonnegative, otherwise return `false`.


<a id='Nemo.isnegative-Tuple{Nemo.arb}' href='#Nemo.isnegative-Tuple{Nemo.arb}'>#</a>
**`Nemo.isnegative`** &mdash; *Method*.



```
isnegative(x::arb)
```

> Return `true` if $x$ is certainly negative, otherwise return `false`.


<a id='Nemo.isnonpositive-Tuple{Nemo.arb}' href='#Nemo.isnonpositive-Tuple{Nemo.arb}'>#</a>
**`Nemo.isnonpositive`** &mdash; *Method*.



```
isnonpositive(x::arb)
```

> Return `true` if $x$ is certainly nonpositive, otherwise return `false`.


<a id='Nemo.midpoint-Tuple{Nemo.arb}' href='#Nemo.midpoint-Tuple{Nemo.arb}'>#</a>
**`Nemo.midpoint`** &mdash; *Method*.



```
midpoint(x::arb)
```

> Return the midpoint of the ball $x$ as an Arb ball.


<a id='Nemo.radius-Tuple{Nemo.arb}' href='#Nemo.radius-Tuple{Nemo.arb}'>#</a>
**`Nemo.radius`** &mdash; *Method*.



```
radius(x::arb)
```

> Return the radius of the ball $x$ as an Arb ball.


<a id='Nemo.accuracy_bits-Tuple{Nemo.arb}' href='#Nemo.accuracy_bits-Tuple{Nemo.arb}'>#</a>
**`Nemo.accuracy_bits`** &mdash; *Method*.



```
accuracy_bits(x::arb)
```

> Return the relative accuracy of $x$ measured in bits, capped between `typemax(Int)` and `-typemax(Int)`.



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


<a id='Arithmetic-operations-1'></a>

## Arithmetic operations


Nemo provides all the standard field operations for Arb field elements, as follows. Note that division is represented by `//` since a field is its own fraction field and since exact division is not generally possible in an inexact field.


          Function |      Operation
-----------------: | -------------:
         -(a::arb) |    unary minus
 +(a::arb, b::arb) |       addition
 -(a::arb, b::arb) |    subtraction
 *(a::arb, b::arb) | multiplication
//(a::arb, b::arb) |       division
 ^(a::arb, b::arb) |       powering


In addition, the following ad hoc field operations are defined.


              Function |      Operation
---------------------: | -------------:
 +(a::arb, b::Integer) |       addition
 +(a::Integer, b::arb) |       addition
    +(a::arb, b::fmpz) |       addition
    +(a::fmpz, b::arb) |       addition
 -(a::arb, b::Integer) |    subtraction
 -(a::Integer, b::arb) |    subtraction
    -(a::arb, b::fmpz) |    subtraction
    -(a::fmpz, b::arb) |    subtraction
 *(a::arb, b::Integer) | multiplication
 *(a::Integer, b::arb) | multiplication
    *(a::arb, b::fmpz) | multiplication
    *(a::fmpz, b::arb) | multiplication
//(a::arb, b::Integer) |       division
   //(a::arb, b::fmpz) |       division
//(a::Integer, b::arb) |       division
   //(a::fmpz, b::arb) |       division
 ^(a::arb, b::Integer) |       powering
    ^(a::arb, b::fmpz) |       powering
    ^(a::arb, b::fmpq) |       powering


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


<a id='Containment-1'></a>

## Containment


It is often necessary to determine whether a given exact value or ball is contained in a given real ball or whether two balls overlap. The following functions are provided for this purpose.

<a id='Nemo.overlaps-Tuple{Nemo.arb,Nemo.arb}' href='#Nemo.overlaps-Tuple{Nemo.arb,Nemo.arb}'>#</a>
**`Nemo.overlaps`** &mdash; *Method*.



```
overlaps(x::arb, y::arb)
```

> Returns `true` if any part of the ball $x$ overlaps any part of the ball $y$, otherwise return `false`.


<a id='Base.contains-Tuple{Nemo.arb,Nemo.arb}' href='#Base.contains-Tuple{Nemo.arb,Nemo.arb}'>#</a>
**`Base.contains`** &mdash; *Method*.



```
contains(x::arb, y::arb)
```

> Returns `true` if the ball $x$ contains the ball $y$, otherwise return `false`.


<a id='Base.contains-Tuple{Nemo.arb,Integer}' href='#Base.contains-Tuple{Nemo.arb,Integer}'>#</a>
**`Base.contains`** &mdash; *Method*.



```
contains(x::arb, y::Integer)
```

> Returns `true` if the ball $x$ contains the given integer value, otherwise return `false`.


<a id='Base.contains-Tuple{Nemo.arb,Nemo.fmpz}' href='#Base.contains-Tuple{Nemo.arb,Nemo.fmpz}'>#</a>
**`Base.contains`** &mdash; *Method*.



```
contains(x::arb, y::fmpz)
```

> Returns `true` if the ball $x$ contains the given integer value, otherwise return `false`.


<a id='Base.contains-Tuple{Nemo.arb,Nemo.fmpq}' href='#Base.contains-Tuple{Nemo.arb,Nemo.fmpq}'>#</a>
**`Base.contains`** &mdash; *Method*.



```
contains(x::arb, y::fmpq)
```

> Returns `true` if the ball $x$ contains the given rational value, otherwise return `false`.


<a id='Base.contains-Tuple{Nemo.arb,BigFloat}' href='#Base.contains-Tuple{Nemo.arb,BigFloat}'>#</a>
**`Base.contains`** &mdash; *Method*.



```
contains(x::arb, y::BigFloat)
```

> Returns `true` if the ball $x$ contains the given floating point value,  otherwise return `false`.



The following functions are also provided for determining if a ball intersects a certain part of the real number line.

<a id='Nemo.contains_zero-Tuple{Nemo.arb}' href='#Nemo.contains_zero-Tuple{Nemo.arb}'>#</a>
**`Nemo.contains_zero`** &mdash; *Method*.



```
contains_zero(x::arb)
```

> Returns `true` if the ball $x$ contains zero, otherwise return `false`.


<a id='Nemo.contains_negative-Tuple{Nemo.arb}' href='#Nemo.contains_negative-Tuple{Nemo.arb}'>#</a>
**`Nemo.contains_negative`** &mdash; *Method*.



```
contains_negative(x::arb)
```

> Returns `true` if the ball $x$ contains any negative value, otherwise return `false`.


<a id='Nemo.contains_positive-Tuple{Nemo.arb}' href='#Nemo.contains_positive-Tuple{Nemo.arb}'>#</a>
**`Nemo.contains_positive`** &mdash; *Method*.



```
contains_positive(x::arb)
```

> Returns `true` if the ball $x$ contains any positive value, otherwise return `false`.


<a id='Nemo.contains_nonnegative-Tuple{Nemo.arb}' href='#Nemo.contains_nonnegative-Tuple{Nemo.arb}'>#</a>
**`Nemo.contains_nonnegative`** &mdash; *Method*.



```
contains_nonnegative(x::arb)
```

> Returns `true` if the ball $x$ contains any nonnegative value, otherwise return `false`.


<a id='Nemo.contains_nonpositive-Tuple{Nemo.arb}' href='#Nemo.contains_nonpositive-Tuple{Nemo.arb}'>#</a>
**`Nemo.contains_nonpositive`** &mdash; *Method*.



```
contains_nonpositive(x::arb)
```

> Returns `true` if the ball $x$ contains any nonpositive value, otherwise return `false`.



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


<a id='Comparison-1'></a>

## Comparison


Nemo provides a full range of comparison operations for Arb balls. Note that a ball is considered less than another ball if every value in the first ball is less than every value in the second ball, etc.


Firstly, we introduce an exact equality which is distinct from arithmetic equality. This is distinct from arithmetic equality implemented by `==`, which merely compares up to the minimum of the precisions of its operands.

<a id='Base.isequal-Tuple{Nemo.arb,Nemo.arb}' href='#Base.isequal-Tuple{Nemo.arb,Nemo.arb}'>#</a>
**`Base.isequal`** &mdash; *Method*.



```
isequal(x::arb, y::arb)
```

> Return `true` if the balls $x$ and $y$ are precisely equal, i.e. have the same midpoints and radii.



A full range of functions is available for comparing balls, i.e. `==`, `!=`, `<`, `<=`, `>=`, `>`. In fact, all these are implemented directly in C. In the table below we document these as though only `==` and `isless` had been provided to Julia.


<a id='Function-1'></a>

## Function


`isless(x::arb, y::arb)`  `==(x::arb, y::arb)`


As well as these, we provide a full range of ad hoc comparison operators. Again, these are implemented directly in Julia, but we document them as though `isless` and `==` were provided.


<a id='Function-2'></a>

## Function


`isless(x::arb, y::Integer)` `isless(x::Integer, y::arb)` `isless(x::arb, y::fmpz)` `isless(x::fmpz, y::arb)` `isless(x::arb, y::Float64)` `isless(x::Float64, y::arb)`


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


<a id='Absolute-value-1'></a>

## Absolute value

<a id='Base.abs-Tuple{Nemo.arb}' href='#Base.abs-Tuple{Nemo.arb}'>#</a>
**`Base.abs`** &mdash; *Method*.



```
abs(x::arb)
```

> Return the absolute value of $x$.



Here are some examples of taking the absolute value.


```
RR = ArbField(64)
x = RR("-1 +/- 0.001")

a = abs(x)
```


<a id='Inverse-1'></a>

## Inverse

<a id='Base.inv-Tuple{Nemo.arb}' href='#Base.inv-Tuple{Nemo.arb}'>#</a>
**`Base.inv`** &mdash; *Method*.



```
inv(x::arb)
```

> Return the multiplicative inverse of $x$, i.e. $1/x$.



Here are some examples of taking the inverse.


```
RR = ArbField(64)
x = RR("-3 +/- 0.001")

a = inv(x)
```


<a id='Shifting-1'></a>

## Shifting

<a id='Base.Math.ldexp-Tuple{Nemo.arb,Int64}' href='#Base.Math.ldexp-Tuple{Nemo.arb,Int64}'>#</a>
**`Base.Math.ldexp`** &mdash; *Method*.



```
ldexp(x::arb, y::Int)
```

> Return $2^yx$. Note that $y$ can be positive, zero or negative.


<a id='Base.Math.ldexp-Tuple{Nemo.arb,Nemo.fmpz}' href='#Base.Math.ldexp-Tuple{Nemo.arb,Nemo.fmpz}'>#</a>
**`Base.Math.ldexp`** &mdash; *Method*.



```
ldexp(x::arb, y::fmpz)
```

> Return $2^yx$. Note that $y$ can be positive, zero or negative.



Here are some examples of shifting.


```
RR = ArbField(64)
x = RR("-3 +/- 0.001")

a = ldexp(x, 23)
b = ldexp(x, -ZZ(15))
```


<a id='Miscellaneous-operations-1'></a>

## Miscellaneous operations

<a id='Nemo.trim-Tuple{Nemo.arb}' href='#Nemo.trim-Tuple{Nemo.arb}'>#</a>
**`Nemo.trim`** &mdash; *Method*.



```
trim(x::arb)
```

> Return an `arb` interval containing $x$ but which may be more economical, by rounding off insignificant bits from the midpoint.


<a id='Nemo.unique_integer-Tuple{Nemo.arb}' href='#Nemo.unique_integer-Tuple{Nemo.arb}'>#</a>
**`Nemo.unique_integer`** &mdash; *Method*.



```
unique_integer(x::arb)
```

> Return a pair where the first value is a boolean and the second is an `fmpz` integer. The boolean indicates whether the interval $x$ contains a unique integer. If this is the case, the second return value is set to this unique integer.


<a id='Nemo.setunion-Tuple{Nemo.arb,Nemo.arb}' href='#Nemo.setunion-Tuple{Nemo.arb,Nemo.arb}'>#</a>
**`Nemo.setunion`** &mdash; *Method*.



```
setunion(x::arb, y::arb)
```

> Return an `arb` containing the union of the intervals represented by $x$ and $y$.



Here are some examples of miscellaneous operations.


```
RR = ArbField(64)
x = RR("-3 +/- 0.001")
y = RR("2 +/- 0.5")

a = trim(x)
b, c = unique_integer(x)
d = setunion(x, y)
```


<a id='Constants-1'></a>

## Constants

<a id='Nemo.const_pi-Tuple{Nemo.ArbField}' href='#Nemo.const_pi-Tuple{Nemo.ArbField}'>#</a>
**`Nemo.const_pi`** &mdash; *Method*.



```
const_pi(r::ArbField)
```

> Return $\pi = 3.14159\ldots$ as an element of $r$.


<a id='Nemo.const_e-Tuple{Nemo.ArbField}' href='#Nemo.const_e-Tuple{Nemo.ArbField}'>#</a>
**`Nemo.const_e`** &mdash; *Method*.



```
const_e(r::ArbField)
```

> Return $e = 2.71828\ldots$ as an element of $r$.


<a id='Nemo.const_log2-Tuple{Nemo.ArbField}' href='#Nemo.const_log2-Tuple{Nemo.ArbField}'>#</a>
**`Nemo.const_log2`** &mdash; *Method*.



```
const_log2(r::ArbField)
```

> Return $\log(2) = 0.69314\ldots$ as an element of $r$.


<a id='Nemo.const_log10-Tuple{Nemo.ArbField}' href='#Nemo.const_log10-Tuple{Nemo.ArbField}'>#</a>
**`Nemo.const_log10`** &mdash; *Method*.



```
const_log10(r::ArbField)
```

> Return $\log(10) = 2.302585\ldots$ as an element of $r$.


<a id='Nemo.const_euler-Tuple{Nemo.ArbField}' href='#Nemo.const_euler-Tuple{Nemo.ArbField}'>#</a>
**`Nemo.const_euler`** &mdash; *Method*.



```
const_euler(r::ArbField)
```

> Return Euler's constant $\gamma = 0.577215\ldots$ as an element of $r$.


<a id='Nemo.const_catalan-Tuple{Nemo.ArbField}' href='#Nemo.const_catalan-Tuple{Nemo.ArbField}'>#</a>
**`Nemo.const_catalan`** &mdash; *Method*.



```
const_catalan(r::ArbField)
```

> Return Catalan's constant $C = 0.915965\ldots$ as an element of $r$.


<a id='Nemo.const_khinchin-Tuple{Nemo.ArbField}' href='#Nemo.const_khinchin-Tuple{Nemo.ArbField}'>#</a>
**`Nemo.const_khinchin`** &mdash; *Method*.



```
const_khinchin(r::ArbField)
```

> Return Khinchin's constant $K = 2.685452\ldots$ as an element of $r$.


<a id='Nemo.const_glaisher-Tuple{Nemo.ArbField}' href='#Nemo.const_glaisher-Tuple{Nemo.ArbField}'>#</a>
**`Nemo.const_glaisher`** &mdash; *Method*.



```
const_glaisher(r::ArbField)
```

> Return Glaisher's constant $A = 1.282427\ldots$ as an element of $r$.



Here are some examples of computing real constants.


```
RR = ArbField(200)

a = const_pi(RR)
b = const_e(RR)
c = const_euler(RR)
d = const_glaisher(RR)
```


<a id='Mathematical-functions-1'></a>

## Mathematical functions

<a id='Base.floor-Tuple{Nemo.arb}' href='#Base.floor-Tuple{Nemo.arb}'>#</a>
**`Base.floor`** &mdash; *Method*.



```
floor(x::arb)
```

> Compute the floor of $x$, i.e. the greatest integer not exceeding $x$, as an Arb.


<a id='Base.ceil-Tuple{Nemo.arb}' href='#Base.ceil-Tuple{Nemo.arb}'>#</a>
**`Base.ceil`** &mdash; *Method*.



```
ceil(x::arb)
```

> Return the ceiling of $x$, i.e. the least integer not less than $x$, as an Arb.


<a id='Base.sqrt-Tuple{Nemo.arb}' href='#Base.sqrt-Tuple{Nemo.arb}'>#</a>
**`Base.sqrt`** &mdash; *Method*.



```
sqrt(x::arb)
```

> Return the square root of $x$.


<a id='Nemo.rsqrt-Tuple{Nemo.arb}' href='#Nemo.rsqrt-Tuple{Nemo.arb}'>#</a>
**`Nemo.rsqrt`** &mdash; *Method*.



```
rsqrt(x::arb)
```

> Return the inverse of the square root of $x$, i.e. $1/\sqrt{x}$.


<a id='Nemo.sqrt1pm1-Tuple{Nemo.arb}' href='#Nemo.sqrt1pm1-Tuple{Nemo.arb}'>#</a>
**`Nemo.sqrt1pm1`** &mdash; *Method*.



```
sqrt1pm1(x::arb)
```

> Return $\sqrt{1+x}-1$, evaluated accurately for small $x$.


<a id='Base.log-Tuple{Nemo.arb}' href='#Base.log-Tuple{Nemo.arb}'>#</a>
**`Base.log`** &mdash; *Method*.



```
log(x::arb)
```

> Return the principal branch of the logarithm of $x$.


```
log(x)
```

Compute the natural logarithm of `x`. Throws `DomainError` for negative `Real` arguments. Use complex negative arguments to obtain complex results.

There is an experimental variant in the `Base.Math.JuliaLibm` module, which is typically faster and more accurate.

<a id='Base.log1p-Tuple{Nemo.arb}' href='#Base.log1p-Tuple{Nemo.arb}'>#</a>
**`Base.log1p`** &mdash; *Method*.



```
log1p(x::arb)
```

> Return $\log(1+x)$, evaluated accurately for small $x$.


<a id='Base.exp-Tuple{Nemo.arb}' href='#Base.exp-Tuple{Nemo.arb}'>#</a>
**`Base.exp`** &mdash; *Method*.



```
exp(x::arb)
```

> Return the exponential of $x$.


<a id='Base.expm1-Tuple{Nemo.arb}' href='#Base.expm1-Tuple{Nemo.arb}'>#</a>
**`Base.expm1`** &mdash; *Method*.



```
expm1(x::arb)
```

> Return $\exp(x)-1$, evaluated accurately for small $x$.


<a id='Base.sin-Tuple{Nemo.arb}' href='#Base.sin-Tuple{Nemo.arb}'>#</a>
**`Base.sin`** &mdash; *Method*.



```
sin(x::arb)
```

> Return the sine of $x$.


<a id='Base.cos-Tuple{Nemo.arb}' href='#Base.cos-Tuple{Nemo.arb}'>#</a>
**`Base.cos`** &mdash; *Method*.



```
cos(x::arb)
```

> Return the cosine of $x$.


<a id='Base.Math.sinpi-Tuple{Nemo.arb}' href='#Base.Math.sinpi-Tuple{Nemo.arb}'>#</a>
**`Base.Math.sinpi`** &mdash; *Method*.



```
sinpi(x::arb)
```

> Return the sine of $\pi x$.


<a id='Base.Math.cospi-Tuple{Nemo.arb}' href='#Base.Math.cospi-Tuple{Nemo.arb}'>#</a>
**`Base.Math.cospi`** &mdash; *Method*.



```
cospi(x::arb)
```

> Return the cosine of $\pi x$.


<a id='Base.tan-Tuple{Nemo.arb}' href='#Base.tan-Tuple{Nemo.arb}'>#</a>
**`Base.tan`** &mdash; *Method*.



```
tan(x::arb)
```

> Return the tangent of $x$.


<a id='Base.Math.cot-Tuple{Nemo.arb}' href='#Base.Math.cot-Tuple{Nemo.arb}'>#</a>
**`Base.Math.cot`** &mdash; *Method*.



```
cot(x::arb)
```

> Return the cotangent of $x$.


<a id='Nemo.tanpi-Tuple{Nemo.arb}' href='#Nemo.tanpi-Tuple{Nemo.arb}'>#</a>
**`Nemo.tanpi`** &mdash; *Method*.



```
tanpi(x::arb)
```

> Return the tangent of $\pi x$.


<a id='Nemo.cotpi-Tuple{Nemo.arb}' href='#Nemo.cotpi-Tuple{Nemo.arb}'>#</a>
**`Nemo.cotpi`** &mdash; *Method*.



```
cotpi(x::arb)
```

> Return the cotangent of $\pi x$.


<a id='Base.sinh-Tuple{Nemo.arb}' href='#Base.sinh-Tuple{Nemo.arb}'>#</a>
**`Base.sinh`** &mdash; *Method*.



```
sinh(x::arb)
```

> Return the hyperbolic sine of $x$.


<a id='Base.cosh-Tuple{Nemo.arb}' href='#Base.cosh-Tuple{Nemo.arb}'>#</a>
**`Base.cosh`** &mdash; *Method*.



```
cosh(x::arb)
```

> Return the hyperbolic cosine of $x$.


<a id='Base.tanh-Tuple{Nemo.arb}' href='#Base.tanh-Tuple{Nemo.arb}'>#</a>
**`Base.tanh`** &mdash; *Method*.



```
tanh(x::arb)
```

> Return the hyperbolic tangent of $x$.


<a id='Base.Math.coth-Tuple{Nemo.arb}' href='#Base.Math.coth-Tuple{Nemo.arb}'>#</a>
**`Base.Math.coth`** &mdash; *Method*.



```
coth(x::arb)
```

> Return the hyperbolic cotangent of $x$.


<a id='Base.atan-Tuple{Nemo.arb}' href='#Base.atan-Tuple{Nemo.arb}'>#</a>
**`Base.atan`** &mdash; *Method*.



```
atan(x::arb)
```

> Return the arctangent of $x$.


<a id='Base.asin-Tuple{Nemo.arb}' href='#Base.asin-Tuple{Nemo.arb}'>#</a>
**`Base.asin`** &mdash; *Method*.



```
asin(x::arb)
```

> Return the arcsine of $x$.


<a id='Base.acos-Tuple{Nemo.arb}' href='#Base.acos-Tuple{Nemo.arb}'>#</a>
**`Base.acos`** &mdash; *Method*.



```
acos(x::arb)
```

> Return the arccosine of $x$.


<a id='Base.atanh-Tuple{Nemo.arb}' href='#Base.atanh-Tuple{Nemo.arb}'>#</a>
**`Base.atanh`** &mdash; *Method*.



```
atanh(x::arb)
```

> Return the hyperbolic arctangent of $x$.


<a id='Base.asinh-Tuple{Nemo.arb}' href='#Base.asinh-Tuple{Nemo.arb}'>#</a>
**`Base.asinh`** &mdash; *Method*.



```
asinh(x::arb)
```

> Return the hyperbolic arcsine of $x$.


<a id='Base.acosh-Tuple{Nemo.arb}' href='#Base.acosh-Tuple{Nemo.arb}'>#</a>
**`Base.acosh`** &mdash; *Method*.



```
acosh(x::arb)
```

> Return the hyperbolic arccosine of $x$.


<a id='Base.Math.gamma-Tuple{Nemo.arb}' href='#Base.Math.gamma-Tuple{Nemo.arb}'>#</a>
**`Base.Math.gamma`** &mdash; *Method*.



```
gamma(x::arb)
```

> Return the Gamma function evaluated at $x$.


<a id='Base.Math.lgamma-Tuple{Nemo.arb}' href='#Base.Math.lgamma-Tuple{Nemo.arb}'>#</a>
**`Base.Math.lgamma`** &mdash; *Method*.



```
lgamma(x::arb)
```

> Return the logarithm of the Gamma function evaluated at $x$.


<a id='Nemo.rgamma-Tuple{Nemo.arb}' href='#Nemo.rgamma-Tuple{Nemo.arb}'>#</a>
**`Nemo.rgamma`** &mdash; *Method*.



```
rgamma(x::arb)
```

> Return the reciprocal of the Gamma function evaluated at $x$.


<a id='Base.Math.digamma-Tuple{Nemo.arb}' href='#Base.Math.digamma-Tuple{Nemo.arb}'>#</a>
**`Base.Math.digamma`** &mdash; *Method*.



```
digamma(x::arb)
```

> Return the  logarithmic derivative of the gamma function evaluated at $x$, i.e. $\psi(x)$.


<a id='Base.Math.zeta-Tuple{Nemo.arb}' href='#Base.Math.zeta-Tuple{Nemo.arb}'>#</a>
**`Base.Math.zeta`** &mdash; *Method*.



```
zeta(x::arb)
```

> Return the Riemann zeta function evaluated at $x$.


```
zeta(s)
```

Riemann zeta function $\zeta(s)$.

<a id='Nemo.sincos-Tuple{Nemo.arb}' href='#Nemo.sincos-Tuple{Nemo.arb}'>#</a>
**`Nemo.sincos`** &mdash; *Method*.



```
sincos(x::arb)
```

> Return a tuple $s, c$ consisting of the sine $s$ and cosine $c$ of $x$.


<a id='Nemo.sincospi-Tuple{Nemo.arb}' href='#Nemo.sincospi-Tuple{Nemo.arb}'>#</a>
**`Nemo.sincospi`** &mdash; *Method*.



```
sincospi(x::arb)
```

> Return a tuple $s, c$ consisting of the sine $s$ and cosine $c$ of $\pi x$.


<a id='Base.Math.sinpi-Tuple{Nemo.fmpq,Nemo.ArbField}' href='#Base.Math.sinpi-Tuple{Nemo.fmpq,Nemo.ArbField}'>#</a>
**`Base.Math.sinpi`** &mdash; *Method*.



```
sinpi(x::fmpq, r::ArbField)
```

> Return the sine of $\pi x$ in the given Arb field.


<a id='Base.Math.cospi-Tuple{Nemo.fmpq,Nemo.ArbField}' href='#Base.Math.cospi-Tuple{Nemo.fmpq,Nemo.ArbField}'>#</a>
**`Base.Math.cospi`** &mdash; *Method*.



```
cospi(x::fmpq, r::ArbField)
```

> Return the cosine of $\pi x$ in the given Arb field.


<a id='Nemo.sincospi-Tuple{Nemo.fmpq,Nemo.ArbField}' href='#Nemo.sincospi-Tuple{Nemo.fmpq,Nemo.ArbField}'>#</a>
**`Nemo.sincospi`** &mdash; *Method*.



```
sincospi(x::fmpq, r::ArbField)
```

> Return a tuple $s, c$ consisting of the sine and cosine of $\pi x$ in the given Arb field.


<a id='Nemo.sinhcosh-Tuple{Nemo.arb}' href='#Nemo.sinhcosh-Tuple{Nemo.arb}'>#</a>
**`Nemo.sinhcosh`** &mdash; *Method*.



```
sinhcosh(x::arb)
```

> Return a tuple $s, c$ consisting of the hyperbolic sine and cosine of $x$.


<a id='Base.Math.atan2-Tuple{Nemo.arb,Nemo.arb}' href='#Base.Math.atan2-Tuple{Nemo.arb,Nemo.arb}'>#</a>
**`Base.Math.atan2`** &mdash; *Method*.



```
atan2(x::arb, y::arb)
```

> Return atan2$(b,a) = \arg(a+bi)$.


<a id='Nemo.agm-Tuple{Nemo.arb,Nemo.arb}' href='#Nemo.agm-Tuple{Nemo.arb,Nemo.arb}'>#</a>
**`Nemo.agm`** &mdash; *Method*.



```
agm(x::arb, y::arb)
```

> Return the arithmetic-geometric mean of $x$ and $y$


<a id='Base.Math.zeta-Tuple{Nemo.arb,Nemo.arb}' href='#Base.Math.zeta-Tuple{Nemo.arb,Nemo.arb}'>#</a>
**`Base.Math.zeta`** &mdash; *Method*.



```
zeta(s::arb, a::arb)
```

> Return the Hurwitz zeta function $\zeta(s,a)$..


```
zeta(s, z)
```

Hurwitz zeta function $\zeta(s, z)$.  (This is equivalent to the Riemann zeta function $\zeta(s)$ for the case of `z=1`.)

<a id='Base.Math.hypot-Tuple{Nemo.arb,Nemo.arb}' href='#Base.Math.hypot-Tuple{Nemo.arb,Nemo.arb}'>#</a>
**`Base.Math.hypot`** &mdash; *Method*.



```
hypot(x::arb, y::arb)
```

> Return $\sqrt{x^2 + y^2}$.


<a id='Nemo.root-Tuple{Nemo.arb,Int64}' href='#Nemo.root-Tuple{Nemo.arb,Int64}'>#</a>
**`Nemo.root`** &mdash; *Method*.



```
root(x::arb, n::Int)
```

> Return the $n$-th root of $x$. We require $x \geq 0$.


<a id='Nemo.fac-Tuple{Nemo.arb}' href='#Nemo.fac-Tuple{Nemo.arb}'>#</a>
**`Nemo.fac`** &mdash; *Method*.



```
fac(x::arb)
```

> Return the factorial of $x$.


<a id='Nemo.fac-Tuple{Int64,Nemo.ArbField}' href='#Nemo.fac-Tuple{Int64,Nemo.ArbField}'>#</a>
**`Nemo.fac`** &mdash; *Method*.



```
fac(n::Int, r::ArbField)
```

> Return the factorial of $n$ in the given Arb field.


<a id='Nemo.binom-Tuple{Nemo.arb,UInt64}' href='#Nemo.binom-Tuple{Nemo.arb,UInt64}'>#</a>
**`Nemo.binom`** &mdash; *Method*.



```
binom(x::arb, n::UInt)
```

> Return the binomial coefficient ${x \choose n}$.


<a id='Nemo.binom-Tuple{UInt64,UInt64,Nemo.ArbField}' href='#Nemo.binom-Tuple{UInt64,UInt64,Nemo.ArbField}'>#</a>
**`Nemo.binom`** &mdash; *Method*.



```
binom(n::UInt, k::UInt, r::ArbField)
```

> Return the binomial coefficient ${n \choose k}$ in the given Arb field.


<a id='Nemo.fib-Tuple{Nemo.fmpz,Nemo.ArbField}' href='#Nemo.fib-Tuple{Nemo.fmpz,Nemo.ArbField}'>#</a>
**`Nemo.fib`** &mdash; *Method*.



```
fib(n::fmpz, r::ArbField)
```

> Return the $n$-th Fibonacci number in the given Arb field.


<a id='Nemo.fib-Tuple{Int64,Nemo.ArbField}' href='#Nemo.fib-Tuple{Int64,Nemo.ArbField}'>#</a>
**`Nemo.fib`** &mdash; *Method*.



```
fib(n::Int, r::ArbField)
```

> Return the $n$-th Fibonacci number in the given Arb field.


<a id='Base.Math.gamma-Tuple{Nemo.fmpz,Nemo.ArbField}' href='#Base.Math.gamma-Tuple{Nemo.fmpz,Nemo.ArbField}'>#</a>
**`Base.Math.gamma`** &mdash; *Method*.



```
gamma(x::fmpz, r::ArbField)
```

> Return the Gamma function evaluated at $x$ in the given Arb field.


<a id='Base.Math.gamma-Tuple{Nemo.fmpq,Nemo.ArbField}' href='#Base.Math.gamma-Tuple{Nemo.fmpq,Nemo.ArbField}'>#</a>
**`Base.Math.gamma`** &mdash; *Method*.



```
gamma(x::fmpq, r::ArbField)
```

> Return the Gamma function evaluated at $x$ in the given Arb field.


<a id='Base.Math.zeta-Tuple{Int64,Nemo.ArbField}' href='#Base.Math.zeta-Tuple{Int64,Nemo.ArbField}'>#</a>
**`Base.Math.zeta`** &mdash; *Method*.



```
zeta(n::Int, r::ArbField)
```

> Return the Riemann zeta function $\zeta(n)$ as an element of the given Arb field.


```
zeta(s, z)
```

Hurwitz zeta function $\zeta(s, z)$.  (This is equivalent to the Riemann zeta function $\zeta(s)$ for the case of `z=1`.)

<a id='Nemo.bernoulli-Tuple{Int64,Nemo.ArbField}' href='#Nemo.bernoulli-Tuple{Int64,Nemo.ArbField}'>#</a>
**`Nemo.bernoulli`** &mdash; *Method*.



```
bernoulli(n::Int, r::ArbField)
```

> Return the $n$-th Bernoulli number as an element of the given Arb field.


<a id='Nemo.risingfac-Tuple{Nemo.arb,Int64}' href='#Nemo.risingfac-Tuple{Nemo.arb,Int64}'>#</a>
**`Nemo.risingfac`** &mdash; *Method*.



```
risingfac(x::arb, n::Int)
```

> Return the rising factorial $x(x + 1)\ldots (x + n - 1)$ as an Arb.


<a id='Nemo.risingfac-Tuple{Nemo.fmpq,Int64,Nemo.ArbField}' href='#Nemo.risingfac-Tuple{Nemo.fmpq,Int64,Nemo.ArbField}'>#</a>
**`Nemo.risingfac`** &mdash; *Method*.



```
risingfac(x::fmpq, n::Int, r::ArbField)
```

> Return the rising factorial $x(x + 1)\ldots (x + n - 1)$ as an element of the given Arb field.


<a id='Nemo.risingfac2-Tuple{Nemo.arb,Int64}' href='#Nemo.risingfac2-Tuple{Nemo.arb,Int64}'>#</a>
**`Nemo.risingfac2`** &mdash; *Method*.



```
risingfac2(x::arb, n::Int)
```

> Return a tuple containing the rising factorial $x(x + 1)\ldots (x + n - 1)$ and its derivative.


<a id='Nemo.polylog-Tuple{Nemo.arb,Nemo.arb}' href='#Nemo.polylog-Tuple{Nemo.arb,Nemo.arb}'>#</a>
**`Nemo.polylog`** &mdash; *Method*.



```
polylog(s::arb, a::arb)
```

> Return the polylogarithm Li$_s(a)$.


<a id='Nemo.polylog-Tuple{Int64,Nemo.arb}' href='#Nemo.polylog-Tuple{Int64,Nemo.arb}'>#</a>
**`Nemo.polylog`** &mdash; *Method*.



```
polylog(s::Int, a::arb)
```

> Return the polylogarithm Li$_s(a)$.


<a id='Nemo.chebyshev_t-Tuple{Int64,Nemo.arb}' href='#Nemo.chebyshev_t-Tuple{Int64,Nemo.arb}'>#</a>
**`Nemo.chebyshev_t`** &mdash; *Method*.



```
chebyshev_t(n::Int, x::arb)
```

> Return the value of the Chebyshev polynomial $T_n(x)$.


<a id='Nemo.chebyshev_u-Tuple{Int64,Nemo.arb}' href='#Nemo.chebyshev_u-Tuple{Int64,Nemo.arb}'>#</a>
**`Nemo.chebyshev_u`** &mdash; *Method*.



```
chebyshev_u(n::Int, x::arb)
```

> Return the value of the Chebyshev polynomial $U_n(x)$.


<a id='Nemo.chebyshev_t2-Tuple{Int64,Nemo.arb}' href='#Nemo.chebyshev_t2-Tuple{Int64,Nemo.arb}'>#</a>
**`Nemo.chebyshev_t2`** &mdash; *Method*.



```
chebyshev_t2(n::Int, x::arb)
```

> Return the tuple $(T_{n}(x), T_{n-1}(x))$.


<a id='Nemo.chebyshev_u2-Tuple{Int64,Nemo.arb}' href='#Nemo.chebyshev_u2-Tuple{Int64,Nemo.arb}'>#</a>
**`Nemo.chebyshev_u2`** &mdash; *Method*.



```
chebyshev_u2(n::Int, x::arb)
```

> Return the tuple $(U_{n}(x), U_{n-1}(x))$


<a id='Nemo.bell-Tuple{Nemo.fmpz,Nemo.ArbField}' href='#Nemo.bell-Tuple{Nemo.fmpz,Nemo.ArbField}'>#</a>
**`Nemo.bell`** &mdash; *Method*.



```
bell(n::fmpz, r::ArbField)
```

> Return the Bell number $B_n$ as an element of $r$.


<a id='Nemo.bell-Tuple{Int64,Nemo.ArbField}' href='#Nemo.bell-Tuple{Int64,Nemo.ArbField}'>#</a>
**`Nemo.bell`** &mdash; *Method*.



```
bell(n::Int, r::ArbField)
```

> Return the Bell number $B_n$ as an element of $r$.



Here are some examples of real valued mathematical functions.


```
RR = ArbField(64)

a = floor(exp(RR(1)))
b = sinpi(QQ(5,6), RR)
c = gamma(QQ(1,3), ArbField(256))
d = bernoulli(1000, ArbField(53))
f = polylog(3, RR(-10))
```

