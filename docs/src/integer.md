```@meta
CurrentModule = Nemo
DocTestSetup = quote
    using Nemo
end
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
providing them.

 Library        | Element type  | Parent type
----------------|---------------|--------------------
Flint           | `ZZRingElem`        | `ZZRing`

All integer element types belong directly to the abstract type `RingElem` and
all the integer ring parent object types belong to the abstract type `Ring`.

A lot of code will want to accept both `ZZRingElem` integers and Julia integers,
that is, subtypes of `Base.Integer`. Thus for convenience we define

```
IntegerUnion = Union{Integer,ZZRingElem}
```

## Integer functionality

Nemo integers provide all of the ring and Euclidean ring functionality of
AbstractAlgebra.jl.

<https://nemocas.github.io/AbstractAlgebra.jl/stable/ring>

<https://nemocas.github.io/AbstractAlgebra.jl/stable/euclidean_interface>

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
sign(::ZZRingElem)
```

```@docs
size(::ZZRingElem)
```

```@docs
fits(::Type{UInt}, ::ZZRingElem)
fits(::Type{Int}, ::ZZRingElem)
```

```@docs
denominator(::ZZRingElem)
```

```@docs
numerator(::ZZRingElem)
```

**Examples**

```jldoctest
julia> a = ZZ(12)
12

julia> is_unit(a)
false

julia> sign(a)
1

julia> s = size(a)
1

julia> fits(Int, a)
true

julia> n = numerator(a)
12

julia> d = denominator(a)
1
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

Function                     | Return | Rounding of the quotient
-----------------------------|--------|--------------------------------------------
`mod`                        | r      | towards minus infinity
`rem`                        | r      | towards zero
`div`                        | q      | towards minus infinity
`divrem(a::ZZRingElem, b::ZZRingElem)`   | q, r   | towards minus infinity
`tdivrem(a::ZZRingElem, b::ZZRingElem)`  | q, r   | towards zero
`fdivrem(a::ZZRingElem, b::ZZRingElem)`  | q, r   | towards minus infinity
`cdivrem(a::ZZRingElem, b::ZZRingElem)`  | q, r   | towards plus infinity
`ntdivrem(a::ZZRingElem, b::ZZRingElem)` | q, r   | nearest integer, ties toward zero
`nfdivrem(a::ZZRingElem, b::ZZRingElem)` | q, r   | nearest integer, ties toward minus infinity
`ncdivrem(a::ZZRingElem, b::ZZRingElem)` | q, r   | nearest integer, ties toward plus infinity

N.B: the internal definition of `Nemo.div` and `Nemo.divrem` are the same as
`fdiv` and `fdivrem`. The definitions in the table are of `Base.div` and
`Base.divrem` which agree with Julia's definitions of `div` and `divrem`.

Nemo also offers the following ad hoc division operators. The notation and
description is as for the other Euclidean division functions.

Function                    | Return | Rounding
----------------------------|--------|------------------------
`mod(a::ZZRingElem, b::Int)`      | r      | towards minus infinity
`rem(a::ZZRingElem, b::Int)`      | r      | towards zero
`div(a::ZZRingElem, b::Int)`      | q      | towards zero
`tdiv(a::ZZRingElem, b::Int)`     | q      | towards zero
`fdiv(a::ZZRingElem, b::Int)`     | q      | towards minus infinity
`cdiv(a::ZZRingElem, b::Int)`     | q      | towards plus infinity

N.B: the internal definition of `Nemo.div` is the same as `fdiv`. The
definition in the table is `Base.div` which agrees with Julia's
definition of `div`.

The following functions are also available, for the case where one is dividing
by a power of $2$. In other words, for Euclidean division of the form
$a = b2^{d} + r$. These are useful for bit twiddling.

Function                    | Return | Rounding
----------------------------|--------|------------------------
`tdivpow2(a::ZZRingElem, d::Int)` | q      | towards zero
`fdivpow2(a::ZZRingElem, d::Int)` | q      | towards minus infinity
`fmodpow2(a::ZZRingElem, d::Int)` | r      | towards minus infinity
`cdivpow2(a::ZZRingElem, d::Int)` | q      | towards plus infinity

**Examples**

```jldoctest
julia> a = ZZ(12)
12

julia> b = ZZ(5)
5

julia> q, r = divrem(a, b)
(2, 2)

julia> c = cdiv(a, b)
3

julia> d = fdiv(a, b)
2

julia> f = tdivpow2(a, 2)
3

julia> g = fmodpow2(a, 3)
4
```

### Comparison

Instead of `isless` we implement a function `cmp(a, b)` which returns a
positive value if $a > b$, zero if $a == b$ and a negative value if $a < b$.
We then implement all the other operators, including `==` in terms of `cmp`.

For convenience we also implement a `cmpabs(a, b)` function which returns
a positive value if $|a| > |b|$, zero if $|a| == |b|$ and a negative value if
$|a| < |b|$. This can be slightly faster than a call to `cmp` or one of the
comparison operators when comparing non-negative values for example.

Here is a list of the comparison functions implemented, with the understanding
that `cmp` provides all of the comparison operators listed above.

Function                   |
---------------------------|
`cmp(a::ZZRingElem, b::ZZRingElem)`    |
`cmpabs(a::ZZRingElem, b::ZZRingElem)` |

We also provide the following ad hoc comparisons which again provide all of the
comparison operators mentioned above.

Function                   |
---------------------------|
`cmp(a::ZZRingElem, b::Int)`     |
`cmp(a::Int, b::ZZRingElem)`     |
`cmp(a::ZZRingElem, b::UInt)`    |
`cmp(a::UInt, b::ZZRingElem)`    |

**Examples**

```jldoctest
julia> a = ZZ(12)
12

julia> b = ZZ(3)
3

julia> a < b
false

julia> a != b
true

julia> a > 4
true

julia> 5 <= b
false

julia> cmpabs(a, b)
1
```

### Shifting

```@docs
<<(::ZZRingElem, ::Int)
```

```@docs
>>(::ZZRingElem, ::Int)
```

**Examples**

```jldoctest
julia> a = ZZ(12)
12

julia> a << 3
96

julia> a >> 5
0
```

### Modular arithmetic

```@docs
sqrtmod(::ZZRingElem, ::ZZRingElem)
```

```@docs
crt(r1::ZZRingElem, m1::ZZRingElem, r2::ZZRingElem, m2::ZZRingElem, signed=false; check::Bool=true)
```

### Integer logarithm

```@docs
flog(::ZZRingElem, ::ZZRingElem)
```

```@docs
clog(::ZZRingElem, ::ZZRingElem)
```

### Integer roots

```@docs
isqrt(::ZZRingElem)
```

```@docs
isqrtrem(::ZZRingElem)
```

```@docs
root(::ZZRingElem, ::Int)
```

```@docs
iroot(::ZZRingElem, ::Int)
```

### Number theoretic functionality

```@docs
divisible(::ZZRingElem, ::Int)
divisible(::ZZRingElem, ::ZZRingElem)
```

```@docs
is_square(::ZZRingElem)
```

```@docs
is_prime(::ZZRingElem)
```

```@docs
is_probable_prime(::ZZRingElem)
```

```@docs
factor(::ZZRingElem)
```

```@docs
divisor_lenstra(::ZZRingElem, ::ZZRingElem, ::ZZRingElem)
```

```@docs
factorial(::ZZRingElem)
```

```@docs
rising_factorial(::ZZRingElem, ::ZZRingElem)
rising_factorial(::ZZRingElem, ::Int)
rising_factorial(::Int, ::Int)
```

```@docs
primorial(::ZZRingElem)
primorial(::Int)
```

```@docs
fibonacci(::Int)
fibonacci(::ZZRingElem)
```

```@docs
bell(::ZZRingElem)
bell(::Int)
```

```@docs
binomial(::ZZRingElem, ::ZZRingElem)
binomial(::UInt, ::UInt, ::ZZRing)
```

```@docs
moebius_mu(::Int)
moebius_mu(::ZZRingElem)
```

```@docs
jacobi_symbol(::Int, ::Int)
jacobi_symbol(::ZZRingElem, ::ZZRingElem)
kronecker_symbol(::Int, ::Int)
```

```@docs
divisor_sigma(::ZZRingElem, ::Int)
```

```@docs
euler_phi(::ZZRingElem)
```

```@docs
number_of_partitions(::Int)
```

```@docs
is_perfect_power(::ZZRingElem)
Nemo.is_prime_power(::ZZRingElem)
is_prime_power_with_data(::ZZRingElem)
```

### Digits and bases

```@docs
bin(::ZZRingElem)
```

```@docs
oct(::ZZRingElem)
```

```@docs
dec(::ZZRingElem)
```

```@docs
hex(::ZZRingElem)
```

```@docs
base(::ZZRingElem, ::Integer)
```

```@docs
ndigits(::ZZRingElem, ::Integer)
```

```@docs
nbits(::ZZRingElem)
```

### Bit twiddling

```@docs
popcount(::ZZRingElem)
```

```@docs
prevpow2(::ZZRingElem)
```

```@docs
nextpow2(::ZZRingElem)
```

```@docs
trailing_zeros(::ZZRingElem)
```

```@docs
clrbit!(::ZZRingElem, ::Int)
setbit!(::ZZRingElem, ::Int)
combit!(::ZZRingElem, ::Int)
tstbit(::ZZRingElem, ::Int)
```

### Random generation

```@docs
rand_bits(::ZZRing, ::Int)
```

```@docs
rand_bits_prime(::ZZRing, ::Int, ::Bool)
```

**Examples**

```julia
a = rand_bits(ZZ, 23)
b = rand_bits_prime(ZZ, 7)
```

# Complex Integers

The Gaussian integer type in Nemo is provided by a pair of Flint integers.
The associated ring of integers and the fraction field can be retrieved by
`Nemo.GaussianIntegers()` and `Nemo.GaussianRationals()`.

**Examples**

```jldoctest
julia> ZZi = Nemo.GaussianIntegers()
Gaussian integer ring

julia> a = ZZ(5)*im
5*im

julia> b = ZZi(3, 4)
3 + 4*im

julia> is_unit(a)
false

julia> factor(a)
im * (2 - im) * (2 + im)

julia> a//b
4//5 + 3//5*im

julia> abs2(a//b)
1
```
