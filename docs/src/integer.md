```@meta
CurrentModule = Nemo
```

# Integers

The default integer type in Nemo is provided by Flint. The associated ring of
integers is represented by the constant parent object called `FlintZZ`.

For convenience we define

```
ZZ = FlintZZ
```

so that integers can be constructed using `ZZ` instead of `FlintZZ`. Note that
this is the name of a specific parent object, not the name of its type.

The types of the integer ring parent objects and elements of the associated
rings of integers are given in the following table according to the library
provding them.

 Library        | Element type  | Parent type
----------------|---------------|--------------------
Flint           | `fmpz`        | `FlintIntegerRing`

All integer element types belong directly to the abstract type `RingElem` and
all the integer ring parent object types belong to the abstract type `Ring`.

A lot of code will want to accept both `fmpz` integers and Julia integers,
that is, subtypes of `Base.Integer`. Thus for convenience we define

```
IntegerUnion = Union{Integer,fmpz}
```

## Integer functionality

Nemo integers provide all of the ring and Euclidean ring functionality of
AbstractAlgebra.jl.

<https://nemocas.github.io/AbstractAlgebra.jl/latest/ring>

<https://nemocas.github.io/AbstractAlgebra.jl/latest/euclidean_interface>

Below, we describe the functionality that is specific to the Nemo/Flint integer ring.

### Constructors

```julia
ZZ(n::Integer)
```

Coerce a Julia integer value into the integer ring.

```julia
ZZ(n::String)
```

Parse the given string as an integer.

```julia
ZZ(n::Float64)
ZZ(n::Float32)
ZZ(n::Float16)
ZZ(n::BigFloat)
```

Coerce the given floating point number into the integer ring, assuming that it
can be exactly represented as an integer.

### Basic manipulation

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
denominator(::fmpz)
```

```@docs
numerator(::fmpz)
```

**Examples**

```
a = ZZ(12)

isunit(a)
sign(a)
s = size(a)
fits(Int, a)
n = numerator(a)
d = denominator(a)
```

### Euclidean division

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

Function                    | Return | Rounding
----------------------------|--------|------------------------
`mod`                       | r      | towards minus infinity
`rem`                       | r      | towards zero
`div`                       | q      | towards minus infinity
`divrem(a::fmpz, b::fmpz)`  | q, r   | towards minus infinity 
`tdivrem(a::fmpz, b::fmpz)` | q, r   | towards zero
`fdivrem(a::fmpz, b::fmpz)` | q, r   | towards minus infinity 
`ndivrem(a::fmpz, b::fmpz)` | q, r   | nearest integer, ties rounds toward zero

N.B: the internal definition of `Nemo.div` and `Nemo.divrem` are the same as
`fdiv` and `fdivrem`. The definitions in the table are of `Base.div` and
`Base.divrem` which agree with Julia's definitions of `div` and `divrem`.

Nemo also offers the following ad hoc division operators. The notation and
description is as for the other Euclidean division functions.

Function                    | Return | Rounding
----------------------------|--------|------------------------
`mod(a::fmpz, b::Int)`      | r      | towards minus infinity
`rem(a::fmpz, b::Int)`      | r      | towards zero
`div(a::fmpz, b::Int)`      | q      | towards zero
`tdiv(a::fmpz, b::Int)`     | q      | towards zero
`fdiv(a::fmpz, b::Int)`     | q      | towards minus infinity
`cdiv(a::fmpz, b::Int)`     | q      | towards plus infinity

N.B: the internal definition of `Nemo.div` is the same as `fdiv`. The
definition in the table is `Base.div` which agrees with Julia's
definition of `div`.

The following functions are also available, for the case where one is dividing
by a power of $2$. In other words, for Euclidean division of the form
$a = b2^{d} + r$. These are useful for bit twiddling.

Function                    | Return | Rounding
----------------------------|--------|------------------------
`tdivpow2(a::fmpz, d::Int)` | q      | towards zero
`fdivpow2(a::fmpz, d::Int)` | q      | towards minus infinity
`fmodpow2(a::fmpz, d::Int)` | r      | towards minus infinity
`cdivpow2(a::fmpz, d::Int)` | q      | towards plus infinity

**Examples**

```julia
a = fmpz(12)
b = fmpz(5)

q, r = divrem(a, b)
c = cdiv(a, b)
d = fdiv(a, b)
f = tdivpow2(a, 2)
g = fmodpow2(a, 3)
```

### Comparison

Instead of `isless` we implement a function `cmp(a, b)` which returns a
positive value if $a > b$, zero if $a == b$ and a negative value if $a < b$.
We then implement all the other operators, including `==` in terms of `cmp`.

For convenience we also implement a `cmpabs(a, b)` function which returns
a positive value if $|a| > |b|$, zero if $|a| == |b|$ and a negative value if
$|a| < |b|$. This can be slightly faster than a call to `cmp` or one of the
comparison operators when comparing nonnegative values for example.

Here is a list of the comparison functions implemented, with the understanding
that `cmp` provides all of the comparison operators listed above.

Function                   |
---------------------------|
`cmp(a::fmpz, b::fmpz)`    |
`cmpabs(a::fmpz, b::fmpz)` |

We also provide the following ad hoc comparisons which again provide all of the
comparison operators mentioned above.

Function                   |
---------------------------|
`cmp(a::fmpz, b::Int)`     |
`cmp(a::Int, b::fmpz)`     |
`cmp(a::fmpz, b::UInt)`    |
`cmp(a::UInt, b::fmpz)`    |

**Examples**

```julia
a = ZZ(12)
b = ZZ(3)

a < b
a != b
a > 4
5 <= b
cmpabs(a, b)
```

### Shifting

```@docs
<<(::fmpz, ::Int)
```

```@docs
>>(::fmpz, ::Int)
```

**Examples**

```julia
a = fmpz(12)

a << 3
a >> 5
```

### Modular arithmetic

```@docs
sqrtmod(::fmpz, ::fmpz)
```

```@docs
crt(::fmpz, ::fmpz, ::fmpz, m::fmpz, ::Bool)
crt(::fmpz, ::fmpz, ::Int, ::Int, ::Bool)
```

**Examples**

```julia
c = sqrtmod(ZZ(12), ZZ(13))
d = crt(ZZ(5), ZZ(13), ZZ(7), ZZ(37), true)
```

### Integer logarithm

```@docs
flog(::fmpz, ::fmpz)
flog(::fmpz, ::Int)
```

```@docs
clog(::fmpz, ::fmpz)
clog(::fmpz, ::Int)
```

**Examples**

```julia
a = fmpz(12)
b = fmpz(2)

c = flog(a, b)
d = clog(a, 3)
```

### Integer roots

```@docs
isqrt(::fmpz)
```

```@docs
isqrtrem(::fmpz)
```

```@docs
root(::fmpz, ::Int)
```

```@docs
iroot(::fmpz, ::Int)
```

**Examples**

```julia
a = ZZ(13)
b = ZZ(27)

c = isqrt(a)
s, r = isqrtrem(a)
d = iroot(a, 3)
k = root(b, 3; check=true)
```

### Number theoretic functionality

```@docs
divisible(::fmpz, ::Int)
divisible(::fmpz, ::fmpz)
```

```@docs
issquare(::fmpz)
```

```@docs
isprime(::fmpz)
```

```@docs
isprobable_prime(::fmpz)
```

```@docs
factor(::fmpz)
```

```@docs
divisor_lenstra(::fmpz, ::fmpz, ::fmpz)
```

```@docs
factorial(::fmpz)
```

```@docs
rising_factorial(::fmpz, ::fmpz)
rising_factorial(::fmpz, ::Int)
rising_factorial(::Int, ::Int)
```

```@docs
primorial(::fmpz)
primorial(::Int)
```

```@docs
fibonacci(::Int)
fibonacci(::fmpz)
```

```@docs
bell(::fmpz)
bell(::Int)
```

```@docs
binomial(::fmpz, ::fmpz)
```

```@docs
moebius_mu(::Int)
moebius_mu(::fmpz)
```

```@docs
jacobi_symbol(::Int, ::Int)
jacobi_symbol(::fmpz, ::fmpz)
kronecker_symbol(::Int, ::Int)
```

```@docs
divisor_sigma(::Int, ::Int)
divisor_sigma(::fmpz, ::Int)
divisor_sigma(::fmpz, ::fmpz)
```

```@docs
euler_phi(::Int)
euler_phi(::fmpz)
```

```@docs
number_of_partitions(::Int)
number_of_partitions(::fmpz) 
```

**Examples**

```julia
isprime(ZZ(13))
n = factorial(ZZ(100))
s = divisor_sigma(ZZ(128), 10)
a = euler_phi(ZZ(12480))
p = number_of_partitions(ZZ(1000))
f = factor(ZZ(12))
```

### Digits and bases

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

**Examples**

```julia
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

**Examples**

```julia
a = fmpz(12)

p = popcount(a)
b = nextpow2(a)
combit!(a, 2)
```

### Random generation

```@docs
rand_bits(::FlintIntegerRing, ::Int)
```

```@docs
rand_bits_prime(::FlintIntegerRing, ::Int, ::Bool)
```

**Examples**

```julia
a = rand_bits(ZZ, 23)
b = rand_bits_prime(ZZ, 7)
```

# Complex Integers

The Gaussian integer type in Nemo is provided by a pair of Flint integers.
The associated ring of integers is represented by the constant parent object
called `FlintZZi` or `ZZi`, and the fraction field is called `FlintQQi` or
`QQi`.

**Examples**

```julia
a = ZZ(5)*im
b = ZZi(3, 4)

isunit(a)
factor(a)
a//b
abs2(a//b)
```
