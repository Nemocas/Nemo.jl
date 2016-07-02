```@meta
CurrentModule = Nemo
```

## Introduction

The default integer type in Nemo is provided by Flint. The associated ring of
integers is represented by the constant parent object called `FlintZZ`.

For convenience we define

```
ZZ = FlintZZ
```

so that integers can be constructed using `ZZ` instead of `FlintZZ`. Note that
this is the name of a specific parent object, not the name of its type.

The types of the integer ring parent objects and elements of the asociated
rings of integers are given in the following table according to the library
provding them.

 Library        | Element type  | Parent type
----------------|---------------|--------------------
Flint           | `fmpz`        | `FlintIntegerRing`

All integer element types belong directly to the abstract type `RingElem` and
all the integer ring parent object types belong to the abstract type `Ring`.

## Integer element constructors

There are various ways to construct integers given an integer ring parent
object such as `ZZ`. As usual, one can coerce various elements into the
ring of integers using the parent object. Here are some examples.

```
ZZ(123)
ZZ("123")
ZZ(123.0)
```

Note that when coercing from floating point numbers of various kinds, the
input must be exactly an integer without fractional part.

Apart from coercing elements into the ring of integers, we offer the
following functions.

```@docs
zero(::FlintIntegerRing)
```

```@docs
one(::FlintIntegerRing)
```

Here are some examples of constructing integers.

```
a = ZZ(123)
b = one(ZZ)
c = zero(ZZ)
```

## Basic functionality

The following basic functionality is provided by the default integer
implementation in Nemo, to support construction of generic rings over the
integers. Any custom integer implementation in Nemo should provide these 
functions along with the usual arithmetic operations and greatest common
divisor.

```
parent_type(::Type{fmpz})
```

Gives the type of the parent object of a Flint integer.

```
elem_type(R::FlintIntegerRing)
```

Given the parent object for the integer ring, return the type of elements
of the integer ring.

```
Base.hash(a::fmpz, h::UInt)
```

Return a `UInt` hexadecimal hash of the integer $a$. This should be xor'd
with a fixed random hexadecimal specific to the integer type. The hash of
the machine words used to store the integer should be xor'd with the supplied
parameter `h` as part of computing the hash.

```
deepcopy(a::fmpz)
```

Construct a copy of the given integer and return it. This function must
recursively construct copies of all of the internal data in the given
integer. Nemo integers are mutable and so returning shallow copies is not
sufficient.

```
mul!(c::fmpz, a::fmpz, b::fmpz)
```

Multiply $a$ by $b$ and set the existing integer $c$ to the result. This
function is provided for performance reasons as it saves allocating a new
object for the result and eliminates associated garbage collection.

```
addeq!(c::fmpz, a::fmpz)
```

In-place addition. Adds $a$ to $c$ and sets $c$ to the result. This function
is provided for performance reasons as it saves allocating a new object for
the result and eliminates associated garbage collection.

Given the parent object `ZZ` for the integer ring, the following coercion
functions are provided to coerce various elements into the integer
ring. Developers provide these by overloading the `call` operator for the
integer ring parent objects.

```
ZZ()
```

Coerce zero into the integer ring.

```
ZZ(n::Integer)
```

Coerce an integer value into the integer ring.

```
ZZ(n::String)
```

Parse the given string as an integer.

```
ZZ(n::Float64)
ZZ(n::Float32)
ZZ(n::Float16)
ZZ(n::BigFloat)
```

Coerce the given floating point number into the integer ring, assuming that it
can be exactly represented as an integer.

```
ZZ(f::fmpz)
```

Take an integer that is already in the Flint integer ring and simply return it.
A copy of the original is not made.

In addition to the above, developers of custom integer types must ensure that
they provide the equivalent of the function `base_ring(R::FlintIntegerRing)`
which should return `Union{}`. In addition to this they should ensure that
each integer element contains a field `parent` specifying the parent object of
the integer, or at least supply the equivalent of the function 
`parent(a::fmpz)` to return the parent object of an integer.

## Basic manipulation

Numerous functions are provided to manipulate integers. Also see the section on
basic functionality above.

```@docs
base_ring(::FlintIntegerRing)
```

```@docs
base_ring(::fmpz)
```

```@docs
parent(::fmpz)
```

```@docs
iszero(::fmpz)
```

```@docs
isone(::fmpz)
```

```@docs
isunit(::fmpz)
```

```@docs
sign(::fmpz)
```

```@docs
size(::fmpz)
```

```@docs
fits(::Type{UInt}, ::fmpz)
fits(::Type{Int}, ::fmpz)
```

```@docs
den(::fmpz)
```

```@docs
num(::fmpz)
```

Here are some examples of basic manipulation of integers.

```
a = ZZ(12)

R = base_ring(ZZ)
S = base_ring(a)
T = parent(a)
iszero(a)
isone(a)
isunit(a)
sign(a)
s = size(a)
fits(Int, a)
n = num(a)
d = den(a)
```

## Arithmetic operations

Nemo provides all the standard ring operations for integers, as follows.

Function                   | Operation
---------------------------|----------------
-(a::fmpz)                 | unary minus
+(a::fmpz, b::fmpz)        | addition
-(a::fmpz, b::fmpz)        | subtraction
*(a::fmpz, b::fmpz)        | multiplication
divexact(a::fmpz, b::fmpz) | exact division

In addition, the following ad hoc ring operations are defined.

Function                      | Operation
------------------------------|----------------
+(a::fmpz, b::Integer)        | addition
+(a::Integer, b::fmpz)        | addition
-(a::fmpz, b::Integer)        | subtraction
-(a::Integer, b::fmpz)        | subtraction
*(a::fmpz, b::Integer)        | multiplication
*(a::Integer, b::fmpz)        | multiplication
divexact(a::fmpz, b::Integer) | exact division
divexact(a::Integer, b::fmpz) | exact division
^(a::fmpz, b::Int)            | powering

Here are some examples of arithmetic operations on integers.

```
a = fmpz(12)
b = fmpz(3)

c = a + b
d = divexact(a, b)
f = 3a
g = a*ZZ(7)
h = 3 - a
k = divexact(a, 4)
m = a^7
```

## Euclidean division

Nemo also provides a large number of Euclidean division operations. Recall that
for a dividend $a$ and divisor $b$, we can write $a = bq + r$ with
$0 \leq |r| < |b|$. We call $q$ the quotient and $r$ the remainder.

We distinguish three cases. If $q$ is rounded towards zero, $r$ will have the
same sign as $a$. If $q$ is rounded towards plus infinity, $r$ will have the
opposite sign to $b$. Finally, if $q$ is rounded towards minus infinity, $r$
will have the same sign as $b$.

In the following table we list the division functions and their rounding
behaviour. We also give the return value of the function, with $q$ representing
return of the quotient and $r$ representing return of the remainder.

Function                  | Return | Rounding
------------------------------------------------------------
divrem(a::fmpz, b::fmpz)  | q, r   | towards zero 
tdivrem(a::fmpz, b::fmpz) | q, r   | towards zero
fdivrem(a::fmpz, b::fmpz) | q, r   | towards minus infinity 

Nemo also offers the following ad hoc division operators. The notation and
description is as for the other Euclidean division functions.

Function                  | Return | Rounding
------------------------------------------------------------
rem(a::fmpz, b::Int)      | r      | towards zero
div(a::fmpz, b::Int)      | q      | towards zero
tdiv(a::fmpz, b::Int)     | q      | towards zero
fdiv(a::fmpz, b::Int)     | q      | towards minus infinity
cdiv(a::fmpz, b::Int)     | q      | towards plus infinity

The following functions are also available, for the case where one is dividing
by a power of $2$. In other words, for Euclidean division of the form
$a = b2^{d} + r$. These are useful for bit twiddling.

Function                  | Return | Rounding
------------------------------------------------------------
tdivpow2(a::fmpz, d::Int) | q      | towards zero
fdivpow2(a::fmpz, d::Int) | q      | towards minus infinity
fmodpow2(a::fmpz, d::Int) | r      | towards minus infinity
cdivpow2(a::fmpz, d::Int) | q      | towards plus infinity

Here are some examples of Euclidean division.

```
a = fmpz(12)
b = fmpz(5)

q, r = divrem(a, b)
c = cdiv(a, b)
d = fdiv(a, b)
f = tdivpow2(a, 2)
g = fmodpow2(a, 3)
```

## Comparison

Nemo provides a full complement of comparison operators for integers. This
includes the usual $<, >, \leq, \geq, ==, !=$ operators. These are usually
provided via Julia once Nemo provides the `isless` function and the `==`
function. However, to avoid two calls to Flint for such comparisons we
implement them differently.

Instead of `isless` we implement a function `cmp(a, b)` which returns a
positive value if $a > b$, zero if $a == b$ and a negative value if $a < b$.
We then implement all the other operators, including `==` in terms of `cmp`.

For convenience we also implement a `cmpabs(a, b)` function which returns
a positive value if $|a| > |b|$, zero if $|a| == |b|$ and a negative value if
$|a| < |b|$. This can be slightly faster than a call to `cmp` or one of the
comparison operators when comparing nonnegative values for example.

Here is a list of the comparison functions implemented, with the understanding
that `cmp` provides all of the comparison operators listed above.

Function
-------------------------
cmp(a::fmpz, b::fmpz)
cmpabs(a::fmpz, b::fmpz)

We also provide the following ad hoc comparisons which again provide all of the
comparison operators mentioned above.

Function
-------------------------
cmp(a::fmpz, b::Int)
cmp(a::Int, b::fmpz)
cmp(a::fmpz, b::UInt)
cmp(a::UInt, b::fmpz)

Here are some examples of comparisons.

```
a = ZZ(12)
b = ZZ(3)

a < b
a != b
a > 4
5 <= b
cmpabs(a, b)
```

## Shifting

```@docs
<<(::fmpz, ::Int)
```

```@docs
>>(::fmpz, ::Int)
```

Here are some examples of shifting.

```
a = fmpz(12)

a << 3
a >> 5
```

## Modular arithmetic

```@docs
mod(::fmpz, ::fmpz)
mod(::fmpz, ::Int)
```

```@docs
powmod(::fmpz, ::fmpz, ::fmpz)
powmod(::fmpz, ::Int, ::fmpz)
```

```@docs
invmod(::fmpz, ::fmpz)
```

```@docs
sqrtmod(::fmpz, ::fmpz)
```

```@docs
crt(::fmpz, ::fmpz, ::fmpz, m::fmpz, ::Bool)
crt(::fmpz, ::fmpz, ::Int, ::Int, ::Bool)
```

Here are some examples of modular arithmetic.

```
a = powmod(ZZ(12), ZZ(110), ZZ(13))
a = powmod(ZZ(12), 110, ZZ(13))
b = invmod(ZZ(12), ZZ(13))
c = sqrtmod(ZZ(12), ZZ(13))
d = crt(ZZ(5), ZZ(13), ZZ(7), ZZ(37), true)
```

## Integer logarithm

```@docs
flog(::fmpz, ::fmpz)
flog(::fmpz, ::Int)
```

```@docs
clog(::fmpz, ::fmpz)
clog(::fmpz, ::Int)
```

Here are some examples of computing integer logarithms.

```
a = fmpz(12)
b = fmpz(2)

c = flog(a, b)
d = clog(a, 3)
```

## GCD and LCM

```@docs
gcd(::fmpz, ::fmpz)
```

```@docs
lcm(::fmpz, ::fmpz)
```

```@docs
gcdx(::fmpz, ::fmpz)
```

```@docs
gcdinv(::fmpz, ::fmpz)
```

Here are some examples of GCD and LCM.

```
a = ZZ(3)
b = ZZ(12)

c = gcd(a, b)
d = lcm(a, b)
g, s, t = gcdx(a, b)
g, s = gcdinv(a, b)
```

## Integer roots

```@docs
isqrt(::fmpz)
```

```@docs
isqrtrem(::fmpz)
```

```@docs
root(::fmpz, ::Int)
```

Here are some examples of integer roots.

```
a = ZZ(13)

b = sqrt(a)
s, r = sqrtrem(a)
c = root(a, 3)
```

## Number theoretic functionality

```@docs
divisible(::fmpz, ::Int)
divisible(::fmpz, ::fmpz)
```

```@docs
issquare(::fmpz)
```

```@docs
is_prime(::UInt)
isprime(::fmpz)
```

```@docs
isprobabprime(::fmpz)
```

```@docs
remove(::fmpz, y:fmpz)
```

```@docs
divisor_lenstra(::fmpz, ::fmpz, ::fmpz)
```

```@docs
fac(::Int)
```

```@docs
risingfac(::fmpz, ::Int)
risingfac(::Int, ::Int)
```

```@docs
primorial(::Int)
```

```@docs
fib(::Int)
```

```@docs
bell(::Int)
```

```@docs
binom(::Int, ::Int)
```

```@docs
moebiusmu(::fmpz)
```

```@docs
jacobi(::fmpz, ::fmpz)
```

```@docs
sigma(::fmpz, ::Int)
```

```@docs
eulerphi(::fmpz)
```

```@docs
numpart(::Int)
numpart(::fmpz) 
```

Here are some examples of number theoretic functionality.

```
isprime(ZZ(13))
n = fac(100)
s = sigma(ZZ(128), 10)
a = eulerphi(ZZ(12480))
p = numpart(1000)
```

## Number digits and bases

```@docs
bin(::fmpz)
```

```@docs
oct(::fmpz)
```

```@docs
dec(::fmpz)
```

```@docs
hex(::fmpz)
```

```@docs
base(::fmpz, ::Integer)
```

```@docs
ndigits(::fmpz, ::Integer)
```

```@docs
nbits(::fmpz)
```

Here are some examples of writing numbers in various bases.

```
a = fmpz(12)

s1 = bin(a)
s2 = base(a, 13)
n1 = nbits(a)
n2 = ndigits(a, 3)
```

### Bit twiddling

```@docs
popcount(::fmpz)
```

```@docs
prevpow2(::fmpz)
```

```@docs
nextpow2(::fmpz)
```

```@docs
trailing_zeros(::fmpz)
```

```@docs
clrbit!(::fmpz, ::Int)
```

```@docs
setbit!(::fmpz, ::Int)
```

```@docs
combit!(::fmpz, ::Int)
```

Here are some examples of bit twiddling.

```
a = fmpz(12)

p = popcount(a)
b = nextpow2(a)
combit!(a, 2)
```
