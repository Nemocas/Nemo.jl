


<a id='Introduction-1'></a>

## Introduction


The default integer type in Nemo is provided by Flint. The associated ring of integers is represented by the constant parent object called `FlintZZ`.


For convenience we define


```
ZZ = FlintZZ
```


so that integers can be constructed using `ZZ` instead of `FlintZZ`. Note that this is the name of a specific parent object, not the name of its type.


The types of the integer ring parent objects and elements of the asociated rings of integers are given in the following table according to the library provding them.


Library | Element type |        Parent type
------: | -----------: | -----------------:
  Flint |       `fmpz` | `FlintIntegerRing`


All integer element types belong directly to the abstract type `RingElem` and all the integer ring parent object types belong to the abstract type `Ring`.


<a id='Integer-element-constructors-1'></a>

## Integer element constructors


There are various ways to construct integers given an integer ring parent object such as `ZZ`. As usual, one can coerce various elements into the ring of integers using the parent object. Here are some examples.


```
ZZ(123)
ZZ("123")
ZZ(123.0)
```


Note that when coercing from floating point numbers of various kinds, the input must be exactly an integer without fractional part.


Apart from coercing elements into the ring of integers, we offer the following functions.

<a id='Base.zero-Tuple{Nemo.FlintIntegerRing}' href='#Base.zero-Tuple{Nemo.FlintIntegerRing}'>#</a>
**`Base.zero`** &mdash; *Method*.



```
zero(R::FlintIntegerRing)
```

> Return the integer $1$.


<a id='Base.one-Tuple{Nemo.FlintIntegerRing}' href='#Base.one-Tuple{Nemo.FlintIntegerRing}'>#</a>
**`Base.one`** &mdash; *Method*.



```
one(R::FlintIntegerRing)
```

> Return the integer $1$.



Here are some examples of constructing integers.


```
a = ZZ(123)
b = one(ZZ)
c = zero(ZZ)
```


<a id='Basic-functionality-1'></a>

## Basic functionality


The following basic functionality is provided by the default integer implementation in Nemo, to support construction of generic rings over the integers. Any custom integer implementation in Nemo should provide these  functions along with the usual arithmetic operations and greatest common divisor.


```
parent_type(::Type{fmpz})
```


Gives the type of the parent object of a Flint integer.


```
elem_type(R::FlintIntegerRing)
```


Given the parent object for the integer ring, return the type of elements of the integer ring.


```
Base.hash(a::fmpz, h::UInt)
```


Return a `UInt` hexadecimal hash of the integer $a$. This should be xor'd with a fixed random hexadecimal specific to the integer type. The hash of the machine words used to store the integer should be xor'd with the supplied parameter `h` as part of computing the hash.


```
deepcopy(a::fmpz)
```


Construct a copy of the given integer and return it. This function must recursively construct copies of all of the internal data in the given integer. Nemo integers are mutable and so returning shallow copies is not sufficient.


```
mul!(c::fmpz, a::fmpz, b::fmpz)
```


Multiply $a$ by $b$ and set the existing integer $c$ to the result. This function is provided for performance reasons as it saves allocating a new object for the result and eliminates associated garbage collection.


```
addeq!(c::fmpz, a::fmpz)
```


In-place addition. Adds $a$ to $c$ and sets $c$ to the result. This function is provided for performance reasons as it saves allocating a new object for the result and eliminates associated garbage collection.


Given the parent object `ZZ` for the integer ring, the following coercion functions are provided to coerce various elements into the integer ring. Developers provide these by overloading the `call` operator for the integer ring parent objects.


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


Coerce the given floating point number into the integer ring, assuming that it can be exactly represented as an integer.


```
ZZ(f::fmpz)
```


Take an integer that is already in the Flint integer ring and simply return it. A copy of the original is not made.


In addition to the above, developers of custom integer types must ensure that they provide the equivalent of the function `base_ring(R::FlintIntegerRing)` which should return `Union{}`. In addition to this they should ensure that each integer element contains a field `parent` specifying the parent object of the integer, or at least supply the equivalent of the function  `parent(a::fmpz)` to return the parent object of an integer.


<a id='Basic-manipulation-1'></a>

## Basic manipulation


Numerous functions are provided to manipulate integers. Also see the section on basic functionality above.

<a id='Nemo.base_ring-Tuple{Nemo.FlintIntegerRing}' href='#Nemo.base_ring-Tuple{Nemo.FlintIntegerRing}'>#</a>
**`Nemo.base_ring`** &mdash; *Method*.



```
base_ring(a::FlintIntegerRing)
```

> Returns `Union{}` as this ring is not dependent on another ring.


<a id='Nemo.base_ring-Tuple{Nemo.fmpz}' href='#Nemo.base_ring-Tuple{Nemo.fmpz}'>#</a>
**`Nemo.base_ring`** &mdash; *Method*.



```
base_ring(a::fmpz)
```

> Returns `Union{}` as the parent ring is not dependent on another ring.


<a id='Base.parent-Tuple{Nemo.fmpz}' href='#Base.parent-Tuple{Nemo.fmpz}'>#</a>
**`Base.parent`** &mdash; *Method*.



```
parent(a::fmpz)
```

> Returns the unique Flint integer parent object `FlintZZ`.


<a id='Nemo.iszero-Tuple{Nemo.fmpz}' href='#Nemo.iszero-Tuple{Nemo.fmpz}'>#</a>
**`Nemo.iszero`** &mdash; *Method*.



```
iszero(a::fmpz)
```

> Return `true` if the given integer is zero, otherwise return `false`.


<a id='Nemo.isone-Tuple{Nemo.fmpz}' href='#Nemo.isone-Tuple{Nemo.fmpz}'>#</a>
**`Nemo.isone`** &mdash; *Method*.



```
isone(a::fmpz)
```

> Return `true` if the given integer is one, otherwise return `false`.


<a id='Nemo.isunit-Tuple{Nemo.fmpz}' href='#Nemo.isunit-Tuple{Nemo.fmpz}'>#</a>
**`Nemo.isunit`** &mdash; *Method*.



```
isunit(a::fmpz)
```

> Return `true` if the given integer is a unit, i.e. $\pm 1$, otherwise return `false`.


<a id='Base.sign-Tuple{Nemo.fmpz}' href='#Base.sign-Tuple{Nemo.fmpz}'>#</a>
**`Base.sign`** &mdash; *Method*.



```
sign(a::fmpz)
```

> Returns the sign of $a$, i.e. $+1$, $0$ or $-1$.


<a id='Base.size-Tuple{Nemo.fmpz}' href='#Base.size-Tuple{Nemo.fmpz}'>#</a>
**`Base.size`** &mdash; *Method*.



```
size(a::fmpz)
```

> Returns the number of limbs required to store the absolute value of $a$.


<a id='Nemo.fits-Tuple{Type{UInt64},Nemo.fmpz}' href='#Nemo.fits-Tuple{Type{UInt64},Nemo.fmpz}'>#</a>
**`Nemo.fits`** &mdash; *Method*.



```
fits(::Type{UInt}, a::fmpz)
```

> Returns `true` if the given integer fits into a `UInt`, otherwise returns `false`.


<a id='Nemo.fits-Tuple{Type{Int64},Nemo.fmpz}' href='#Nemo.fits-Tuple{Type{Int64},Nemo.fmpz}'>#</a>
**`Nemo.fits`** &mdash; *Method*.



```
fits(::Type{Int}, a::fmpz)
```

> Returns `true` if the given integer fits into an `Int`, otherwise returns `false`.


<a id='Base.den-Tuple{Nemo.fmpz}' href='#Base.den-Tuple{Nemo.fmpz}'>#</a>
**`Base.den`** &mdash; *Method*.



```
den(a::fmpz)
```

> Returns the denominator of $a$ thought of as a rational. Always returns $1$.


<a id='Base.num-Tuple{Nemo.fmpz}' href='#Base.num-Tuple{Nemo.fmpz}'>#</a>
**`Base.num`** &mdash; *Method*.



```
num(a::fmpz)
```

> Returns the numerator of $a$ thought of as a rational. Always returns $a$.



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


<a id='Arithmetic-operations-1'></a>

## Arithmetic operations


Nemo provides all the standard ring operations for integers, as follows.


                  Function |      Operation
-------------------------: | -------------:
                -(a::fmpz) |    unary minus
       +(a::fmpz, b::fmpz) |       addition
       -(a::fmpz, b::fmpz) |    subtraction
       *(a::fmpz, b::fmpz) | multiplication
divexact(a::fmpz, b::fmpz) | exact division


In addition, the following ad hoc ring operations are defined.


                     Function |      Operation
----------------------------: | -------------:
       +(a::fmpz, b::Integer) |       addition
       +(a::Integer, b::fmpz) |       addition
       -(a::fmpz, b::Integer) |    subtraction
       -(a::Integer, b::fmpz) |    subtraction
       *(a::fmpz, b::Integer) | multiplication
       *(a::Integer, b::fmpz) | multiplication
divexact(a::fmpz, b::Integer) | exact division
divexact(a::Integer, b::fmpz) | exact division
           ^(a::fmpz, b::Int) |       powering


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


<a id='Euclidean-division-1'></a>

## Euclidean division


Nemo also provides a large number of Euclidean division operations. Recall that for a dividend $a$ and divisor $b$, we can write $a = bq + r$ with $0 \leq |r| < |b|$. We call $q$ the quotient and $r$ the remainder.


We distinguish three cases. If $q$ is rounded towards zero, $r$ will have the same sign as $a$. If $q$ is rounded towards plus infinity, $r$ will have the opposite sign to $b$. Finally, if $q$ is rounded towards minus infinity, $r$ will have the same sign as $b$.


In the following table we list the division functions and their rounding behaviour. We also give the return value of the function, with $q$ representing return of the quotient and $r$ representing return of the remainder.


<a id='Function-Return-Rounding-1'></a>

## Function                  | Return | Rounding


divrem(a::fmpz, b::fmpz)  | q, r   | towards zero  tdivrem(a::fmpz, b::fmpz) | q, r   | towards zero fdivrem(a::fmpz, b::fmpz) | q, r   | towards minus infinity 


Nemo also offers the following ad hoc division operators. The notation and description is as for the other Euclidean division functions.


<a id='Function-Return-Rounding-2'></a>

## Function                  | Return | Rounding


rem(a::fmpz, b::Int)      | r      | towards zero div(a::fmpz, b::Int)      | q      | towards zero tdiv(a::fmpz, b::Int)     | q      | towards zero fdiv(a::fmpz, b::Int)     | q      | towards minus infinity cdiv(a::fmpz, b::Int)     | q      | towards plus infinity


The following functions are also available, for the case where one is dividing by a power of $2$. In other words, for Euclidean division of the form $a = b2^{d} + r$. These are useful for bit twiddling.


<a id='Function-Return-Rounding-3'></a>

## Function                  | Return | Rounding


tdivpow2(a::fmpz, d::Int) | q      | towards zero fdivpow2(a::fmpz, d::Int) | q      | towards minus infinity fmodpow2(a::fmpz, d::Int) | r      | towards minus infinity cdivpow2(a::fmpz, d::Int) | q      | towards plus infinity


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


<a id='Comparison-1'></a>

## Comparison


Nemo provides a full complement of comparison operators for integers. This includes the usual $<, >, \leq, \geq, ==, !=$ operators. These are usually provided via Julia once Nemo provides the `isless` function and the `==` function. However, to avoid two calls to Flint for such comparisons we implement them differently.


Instead of `isless` we implement a function `cmp(a, b)` which returns a positive value if $a > b$, zero if $a == b$ and a negative value if $a < b$. We then implement all the other operators, including `==` in terms of `cmp`.


For convenience we also implement a `cmpabs(a, b)` function which returns a positive value if $|a| > |b|$, zero if $|a| == |b|$ and a negative value if $|a| < |b|$. This can be slightly faster than a call to `cmp` or one of the comparison operators when comparing nonnegative values for example.


Here is a list of the comparison functions implemented, with the understanding that `cmp` provides all of the comparison operators listed above.


<a id='Function-1'></a>

## Function


cmp(a::fmpz, b::fmpz) cmpabs(a::fmpz, b::fmpz)


We also provide the following ad hoc comparisons which again provide all of the comparison operators mentioned above.


<a id='Function-2'></a>

## Function


cmp(a::fmpz, b::Int) cmp(a::Int, b::fmpz) cmp(a::fmpz, b::UInt) cmp(a::UInt, b::fmpz)


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


<a id='Shifting-1'></a>

## Shifting

<a id='Base.<<-Tuple{Nemo.fmpz,Int64}' href='#Base.<<-Tuple{Nemo.fmpz,Int64}'>#</a>
**`Base.<<`** &mdash; *Method*.



```
<<(x::fmpz, c::Int)
```

> Return $2^cx$ where $c \geq 0$.


<a id='Base.>>-Tuple{Nemo.fmpz,Int64}' href='#Base.>>-Tuple{Nemo.fmpz,Int64}'>#</a>
**`Base.>>`** &mdash; *Method*.



```
>>(x::fmpz, c::Int)
```

> Return $x/2^c$, discarding any remainder, where $c \geq 0$.



Here are some examples of shifting.


```
a = fmpz(12)

a << 3
a >> 5
```


<a id='Modular-arithmetic-1'></a>

## Modular arithmetic

<a id='Base.mod-Tuple{Nemo.fmpz,Nemo.fmpz}' href='#Base.mod-Tuple{Nemo.fmpz,Nemo.fmpz}'>#</a>
**`Base.mod`** &mdash; *Method*.



```
mod(x::fmpz, y::fmpz)
```

> Return the remainder after division of $x$ by $y$. The remainder will be the least nonnegative remainder.


<a id='Base.mod-Tuple{Nemo.fmpz,Int64}' href='#Base.mod-Tuple{Nemo.fmpz,Int64}'>#</a>
**`Base.mod`** &mdash; *Method*.



```
mod(x::fmpz, y::Int)
```

> Return the remainder after division of $x$ by $y$. The remainder will be the least nonnegative remainder.


<a id='Nemo.powmod-Tuple{Nemo.fmpz,Nemo.fmpz,Nemo.fmpz}' href='#Nemo.powmod-Tuple{Nemo.fmpz,Nemo.fmpz,Nemo.fmpz}'>#</a>
**`Nemo.powmod`** &mdash; *Method*.



```
powmod(x::fmpz, p::fmpz, m::fmpz)
```

> Return $x^p (\mod m)$. The remainder will be in the range $[0, m)$


<a id='Nemo.powmod-Tuple{Nemo.fmpz,Int64,Nemo.fmpz}' href='#Nemo.powmod-Tuple{Nemo.fmpz,Int64,Nemo.fmpz}'>#</a>
**`Nemo.powmod`** &mdash; *Method*.



```
powmod(x::fmpz, p::Int, m::fmpz)
```

> Return $x^p (\mod m)$. The remainder will be in the range $[0, m)$


<a id='Base.invmod-Tuple{Nemo.fmpz,Nemo.fmpz}' href='#Base.invmod-Tuple{Nemo.fmpz,Nemo.fmpz}'>#</a>
**`Base.invmod`** &mdash; *Method*.



```
invmod(x::fmpz, m::fmpz)
```

> Return $x^{-1} (\mod m)$. The remainder will be in the range $[0, m)$


<a id='Nemo.sqrtmod-Tuple{Nemo.fmpz,Nemo.fmpz}' href='#Nemo.sqrtmod-Tuple{Nemo.fmpz,Nemo.fmpz}'>#</a>
**`Nemo.sqrtmod`** &mdash; *Method*.



```
sqrtmod(x::fmpz, m::fmpz)
```

> Return a square root of $x (\mod m)$ if one exists. The remainder will be in the range $[0, m)$. We require that $m$ is prime, otherwise the algorithm may not terminate.


<a id='Nemo.crt-Tuple{Nemo.fmpz,Nemo.fmpz,Nemo.fmpz,Nemo.fmpz,Bool}' href='#Nemo.crt-Tuple{Nemo.fmpz,Nemo.fmpz,Nemo.fmpz,Nemo.fmpz,Bool}'>#</a>
**`Nemo.crt`** &mdash; *Method*.



```
crt(r1::fmpz, m1::fmpz, r2::fmpz, m2::fmpz, signed=false)
```

> Find $r$ such that $r \equiv r_1 (\mod m_1)$ and $r \equiv r_2 (\mod m_2)$. If `signed = true`, $r$ will be in the range $-m_1m_2/2 < r \leq m_1m_2/2$. If `signed = false` the value will be in the range $0 \leq r < m_1m_2$.


<a id='Nemo.crt-Tuple{Nemo.fmpz,Nemo.fmpz,Int64,Int64,Bool}' href='#Nemo.crt-Tuple{Nemo.fmpz,Nemo.fmpz,Int64,Int64,Bool}'>#</a>
**`Nemo.crt`** &mdash; *Method*.



```
crt(r1::fmpz, m1::fmpz, r2::Int, m2::Int, signed=false)
```

> Find $r$ such that $r \equiv r_1 (\mod m_1)$ and $r \equiv r_2 (\mod m_2)$. If `signed = true`, $r$ will be in the range $-m_1m_2/2 < r \leq m_1m_2/2$. If `signed = false` the value will be in the range $0 \leq r < m_1m_2$.



Here are some examples of modular arithmetic.


```
a = powmod(ZZ(12), ZZ(110), ZZ(13))
a = powmod(ZZ(12), 110, ZZ(13))
b = invmod(ZZ(12), ZZ(13))
c = sqrtmod(ZZ(12), ZZ(13))
d = crt(ZZ(5), ZZ(13), ZZ(7), ZZ(37), true)
```


<a id='Integer-logarithm-1'></a>

## Integer logarithm

<a id='Nemo.flog-Tuple{Nemo.fmpz,Nemo.fmpz}' href='#Nemo.flog-Tuple{Nemo.fmpz,Nemo.fmpz}'>#</a>
**`Nemo.flog`** &mdash; *Method*.



```
flog(x::fmpz, c::fmpz)
```

> Return the floor of the logarithm of $x$ to base $c$.


<a id='Nemo.flog-Tuple{Nemo.fmpz,Int64}' href='#Nemo.flog-Tuple{Nemo.fmpz,Int64}'>#</a>
**`Nemo.flog`** &mdash; *Method*.



```
flog(x::fmpz, c::Int)
```

> Return the floor of the logarithm of $x$ to base $c$.


<a id='Nemo.clog-Tuple{Nemo.fmpz,Nemo.fmpz}' href='#Nemo.clog-Tuple{Nemo.fmpz,Nemo.fmpz}'>#</a>
**`Nemo.clog`** &mdash; *Method*.



```
clog(x::fmpz, c::fmpz)
```

> Return the ceiling of the logarithm of $x$ to base $c$.


<a id='Nemo.clog-Tuple{Nemo.fmpz,Int64}' href='#Nemo.clog-Tuple{Nemo.fmpz,Int64}'>#</a>
**`Nemo.clog`** &mdash; *Method*.



```
clog(x::fmpz, c::Int)
```

> Return the ceiling of the logarithm of $x$ to base $c$.



Here are some examples of computing integer logarithms.


```
a = fmpz(12)
b = fmpz(2)

c = flog(a, b)
d = clog(a, 3)
```


<a id='GCD-and-LCM-1'></a>

## GCD and LCM

<a id='Base.gcd-Tuple{Nemo.fmpz,Nemo.fmpz}' href='#Base.gcd-Tuple{Nemo.fmpz,Nemo.fmpz}'>#</a>
**`Base.gcd`** &mdash; *Method*.



```
gcd(x::fmpz, y::fmpz)
```

> Return the greatest common divisor of $x$ and $y$. The returned result will always be nonnegative and will be zero iff $x$ and $y$ are zero.


<a id='Base.lcm-Tuple{Nemo.fmpz,Nemo.fmpz}' href='#Base.lcm-Tuple{Nemo.fmpz,Nemo.fmpz}'>#</a>
**`Base.lcm`** &mdash; *Method*.



```
lcm(x::fmpz, y::fmpz)
```

> Return the least common multiple of $x$ and $y$. The returned result will always be nonnegative and will be zero iff $x$ and $y$ are zero.


<a id='Base.gcdx-Tuple{Nemo.fmpz,Nemo.fmpz}' href='#Base.gcdx-Tuple{Nemo.fmpz,Nemo.fmpz}'>#</a>
**`Base.gcdx`** &mdash; *Method*.



```
gcdx(a::fmpz, b::fmpz)
```

> Return a tuple $g, s, t$ such that $g$ is the greatest common divisor of $a$ and $b$ and integers $s$ and $t$ such that $g = as + bt$.


<a id='Nemo.gcdinv-Tuple{Nemo.fmpz,Nemo.fmpz}' href='#Nemo.gcdinv-Tuple{Nemo.fmpz,Nemo.fmpz}'>#</a>
**`Nemo.gcdinv`** &mdash; *Method*.



```
gcdinv(a::fmpz, b::fmpz)
```

> Return a tuple $g, s$ where $g$ is the greatest common divisor of $a$ and $b$ and where $s$ is the inverse of $a$ modulo $b$ if $g = 1$. This function can be used to detect impossible inverses, i.e. where $a$ and $b$ are not coprime, and to yield the common factor of $a$ and $b$ if they are not coprime. We require $b \geq a \geq 0$.



Here are some examples of GCD and LCM.


```
a = ZZ(3)
b = ZZ(12)

c = gcd(a, b)
d = lcm(a, b)
g, s, t = gcdx(a, b)
g, s = gcdinv(a, b)
```


<a id='Integer-roots-1'></a>

## Integer roots

<a id='Base.isqrt-Tuple{Nemo.fmpz}' href='#Base.isqrt-Tuple{Nemo.fmpz}'>#</a>
**`Base.isqrt`** &mdash; *Method*.



```
isqrt(x::fmpz)
```

> Return the floor of the square root of $x$.


<a id='Nemo.isqrtrem-Tuple{Nemo.fmpz}' href='#Nemo.isqrtrem-Tuple{Nemo.fmpz}'>#</a>
**`Nemo.isqrtrem`** &mdash; *Method*.



```
isqrtrem(x::fmpz)
```

> Return a tuple $s, r$ consisting of the floor $s$ of the square root of $x$ and the remainder $r$, i.e. such that $x = s^2 + r$. We require $x \geq 0$.


<a id='Nemo.root-Tuple{Nemo.fmpz,Int64}' href='#Nemo.root-Tuple{Nemo.fmpz,Int64}'>#</a>
**`Nemo.root`** &mdash; *Method*.



```
root(x::fmpz, n::Int)
```

> Return the floor of the $n$-the root of $x$. We require $n > 0$ and that $x \geq 0$ if $n$ is even.



Here are some examples of integer roots.


```
a = ZZ(13)

b = sqrt(a)
s, r = sqrtrem(a)
c = root(a, 3)
```


<a id='Number-theoretic-functionality-1'></a>

## Number theoretic functionality

<a id='Nemo.divisible-Tuple{Nemo.fmpz,Int64}' href='#Nemo.divisible-Tuple{Nemo.fmpz,Int64}'>#</a>
**`Nemo.divisible`** &mdash; *Method*.



```
divisible(x::fmpz, y::Int)
```

> Return `true` if $x$ is divisible by $y$, otherwise return `false`. We require $x \neq 0$.


<a id='Nemo.divisible-Tuple{Nemo.fmpz,Nemo.fmpz}' href='#Nemo.divisible-Tuple{Nemo.fmpz,Nemo.fmpz}'>#</a>
**`Nemo.divisible`** &mdash; *Method*.



```
divisible(x::fmpz, y::fmpz)
```

> Return `true` if $x$ is divisible by $y$, otherwise return `false`. We require $x \neq 0$.


<a id='Nemo.issquare-Tuple{Nemo.fmpz}' href='#Nemo.issquare-Tuple{Nemo.fmpz}'>#</a>
**`Nemo.issquare`** &mdash; *Method*.



```
issquare(x::fmpz)
```

> Return `true` if $x$ is a square, otherwise return `false`.


<a id='Nemo.is_prime-Tuple{UInt64}' href='#Nemo.is_prime-Tuple{UInt64}'>#</a>
**`Nemo.is_prime`** &mdash; *Method*.



```
is_prime(x::UInt)
```

> Return `true` if $x$ is a prime number, otherwise return `false`.


<a id='Base.isprime-Tuple{Nemo.fmpz}' href='#Base.isprime-Tuple{Nemo.fmpz}'>#</a>
**`Base.isprime`** &mdash; *Method*.



```
isprime(x::fmpz)
```

> Return `true` if $x$ is a prime number, otherwise return `false`.


<a id='Nemo.isprobabprime-Tuple{Nemo.fmpz}' href='#Nemo.isprobabprime-Tuple{Nemo.fmpz}'>#</a>
**`Nemo.isprobabprime`** &mdash; *Method*.



```
isprobabprime(x::fmpz)
```

> Return `true` if $x$ is a very probably a prime number, otherwise return `false`. No counterexamples are known to this test, but it is conjectured that infinitely many exist.


<a id='Nemo.remove-Tuple{Nemo.fmpz,Any}' href='#Nemo.remove-Tuple{Nemo.fmpz,Any}'>#</a>
**`Nemo.remove`** &mdash; *Method*.



```
remove(x::fmpz, y::fmpz)
```

> Return the tuple $n, z$ such that $x = y^nz$ where $y$ and $z$ are coprime.


<a id='Nemo.divisor_lenstra-Tuple{Nemo.fmpz,Nemo.fmpz,Nemo.fmpz}' href='#Nemo.divisor_lenstra-Tuple{Nemo.fmpz,Nemo.fmpz,Nemo.fmpz}'>#</a>
**`Nemo.divisor_lenstra`** &mdash; *Method*.



```
divisor_lenstra(n::fmpz, r::fmpz, m::fmpz)
```

> If $n$ has a factor which lies in the residue class $r (\mod m)$ for $0 < r < m < n$, this function returns such a factor. Otherwise it returns $0$. This is only efficient if $m$ is at least the cube root of $n$. We require gcd$(r, m) = 1$ and this condition is not checked.


<a id='Nemo.fac-Tuple{Int64}' href='#Nemo.fac-Tuple{Int64}'>#</a>
**`Nemo.fac`** &mdash; *Method*.



```
fac(x::Int)
```

> Return the factorial of $x$, i.e. $x! = 1.2.3\ldots x$. We require $x \geq 0$.


<a id='Nemo.risingfac-Tuple{Nemo.fmpz,Int64}' href='#Nemo.risingfac-Tuple{Nemo.fmpz,Int64}'>#</a>
**`Nemo.risingfac`** &mdash; *Method*.



```
risingfac(x::fmpz, y::Int)
```

> Return the rising factorial of $x$, i.e. $x(x + 1)(x + 2)\ldots (x + n - 1)$. If $n < 0$ we throw a `DomainError()`.


<a id='Nemo.risingfac-Tuple{Int64,Int64}' href='#Nemo.risingfac-Tuple{Int64,Int64}'>#</a>
**`Nemo.risingfac`** &mdash; *Method*.



```
risingfac(x::Int, y::Int)
```

> Return the rising factorial of $x$, i.e. $x(x + 1)(x + 2)\ldots (x + n - 1)$. If $n < 0$ we throw a `DomainError()`.


<a id='Nemo.primorial-Tuple{Int64}' href='#Nemo.primorial-Tuple{Int64}'>#</a>
**`Nemo.primorial`** &mdash; *Method*.



```
primorial(x::Int)
```

> Return the primorial of $n$, i.e. the product of all primes less than or equal to $n$. If $n < 0$ we throw a `DomainError()`.


<a id='Nemo.fib-Tuple{Int64}' href='#Nemo.fib-Tuple{Int64}'>#</a>
**`Nemo.fib`** &mdash; *Method*.



```
fib(x::Int)
```

> Return the $n$-th Fibonacci number $F_n$. We define $F_1 = 1$, $F_2 = 1$ and $F_{i + 1} = F_i + F_{i - 1}$ for all $i > 2$. We require $n \geq 0$. For convenience, we define $F_0 = 0$.


<a id='Nemo.bell-Tuple{Int64}' href='#Nemo.bell-Tuple{Int64}'>#</a>
**`Nemo.bell`** &mdash; *Method*.



```
bell(x::Int)
```

> Return the Bell number $B_n$.


<a id='Nemo.binom-Tuple{Int64,Int64}' href='#Nemo.binom-Tuple{Int64,Int64}'>#</a>
**`Nemo.binom`** &mdash; *Method*.



```
binom(n::Int, k::Int)
```

> Return the binomial coefficient $\frac{n!}{(n - k)!k!}$. If $n, k < 0$ or $k > n$ we return $0$.


<a id='Nemo.moebiusmu-Tuple{Nemo.fmpz}' href='#Nemo.moebiusmu-Tuple{Nemo.fmpz}'>#</a>
**`Nemo.moebiusmu`** &mdash; *Method*.



```
moebiusmu(x::fmpz)
```

> Returns the Moebius mu function of $x$ as an \code{Int}. The value returned is either $-1$, $0$ or $1$. If $x < 0$ we throw a `DomainError()`.


<a id='Nemo.jacobi-Tuple{Nemo.fmpz,Nemo.fmpz}' href='#Nemo.jacobi-Tuple{Nemo.fmpz,Nemo.fmpz}'>#</a>
**`Nemo.jacobi`** &mdash; *Method*.



```
jacobi(x::fmpz, y::fmpz)
```

> Return the value of the Jacobi symbol $\left(\frac{x}{y}\right)$. If $y \leq x$ or $x < 0$, we throw a `DomainError()`.


<a id='Nemo.sigma-Tuple{Nemo.fmpz,Int64}' href='#Nemo.sigma-Tuple{Nemo.fmpz,Int64}'>#</a>
**`Nemo.sigma`** &mdash; *Method*.



```
sigma(x::fmpz, y::Int)
```

> Return the value of the sigma function, i.e. $\sum_{0 < d \;| x} d^y$. If $y < 0$ we throw a `DomainError()`.


<a id='Nemo.eulerphi-Tuple{Nemo.fmpz}' href='#Nemo.eulerphi-Tuple{Nemo.fmpz}'>#</a>
**`Nemo.eulerphi`** &mdash; *Method*.



```
eulerphi(x::fmpz)
```

> Return the value of the Euler phi function at $x$, i.e. the number of positive integers less than $x$ that are coprime with $x$.


<a id='Nemo.numpart-Tuple{Int64}' href='#Nemo.numpart-Tuple{Int64}'>#</a>
**`Nemo.numpart`** &mdash; *Method*.



```
numpart(x::Int)
```

> Return the number of partitions of $x$. This function is not available on Windows 64.


<a id='Nemo.numpart-Tuple{Nemo.fmpz}' href='#Nemo.numpart-Tuple{Nemo.fmpz}'>#</a>
**`Nemo.numpart`** &mdash; *Method*.



```
numpart(x::fmpz)
```

> Return the number of partitions of $x$. This function is not available on Windows 64.



Here are some examples of number theoretic functionality.


```
isprime(ZZ(13))
n = fac(100)
s = sigma(ZZ(128), 10)
a = eulerphi(ZZ(12480))
p = numpart(1000)
```


<a id='Number-digits-and-bases-1'></a>

## Number digits and bases

<a id='Base.bin-Tuple{Nemo.fmpz}' href='#Base.bin-Tuple{Nemo.fmpz}'>#</a>
**`Base.bin`** &mdash; *Method*.



```
bin(n::fmpz)
```

> Return $n$ as a binary string.


<a id='Base.oct-Tuple{Nemo.fmpz}' href='#Base.oct-Tuple{Nemo.fmpz}'>#</a>
**`Base.oct`** &mdash; *Method*.



```
oct(n::fmpz)
```

> Return $n$ as a octal string.


<a id='Base.dec-Tuple{Nemo.fmpz}' href='#Base.dec-Tuple{Nemo.fmpz}'>#</a>
**`Base.dec`** &mdash; *Method*.



```
dec(n::fmpz)
```

> Return $n$ as a decimal string.


<a id='Base.hex-Tuple{Nemo.fmpz}' href='#Base.hex-Tuple{Nemo.fmpz}'>#</a>
**`Base.hex`** &mdash; *Method*.



```
hex(n::fmpz) = base(n, 16)
```

> Return $n$ as a hexadecimal string.


<a id='Base.base-Tuple{Nemo.fmpz,Integer}' href='#Base.base-Tuple{Nemo.fmpz,Integer}'>#</a>
**`Base.base`** &mdash; *Method*.



```
base(n::fmpz, b::Integer)
```

> Return $n$ as a string in base $b$. We require $2 \leq b \leq 62$.


<a id='Base.ndigits-Tuple{Nemo.fmpz,Integer}' href='#Base.ndigits-Tuple{Nemo.fmpz,Integer}'>#</a>
**`Base.ndigits`** &mdash; *Method*.



```
ndigits(x::fmpz, b::Integer = 10)
```

> Return the number of digits of $x$ in the base $b$ (default is $b = 10$).


<a id='Nemo.nbits-Tuple{Nemo.fmpz}' href='#Nemo.nbits-Tuple{Nemo.fmpz}'>#</a>
**`Nemo.nbits`** &mdash; *Method*.



```
nbits(x::fmpz)
```

> Return the number of binary bits of $x$. We return zero if $x = 0$.



Here are some examples of writing numbers in various bases.


```
a = fmpz(12)

s1 = bin(a)
s2 = base(a, 13)
n1 = nbits(a)
n2 = ndigits(a, 3)
```


<a id='Bit-twiddling-1'></a>

### Bit twiddling

<a id='Nemo.popcount-Tuple{Nemo.fmpz}' href='#Nemo.popcount-Tuple{Nemo.fmpz}'>#</a>
**`Nemo.popcount`** &mdash; *Method*.



```
popcount(x::fmpz)
```

> Return the number of ones in the binary representation of $x$.


<a id='Base.prevpow2-Tuple{Nemo.fmpz}' href='#Base.prevpow2-Tuple{Nemo.fmpz}'>#</a>
**`Base.prevpow2`** &mdash; *Method*.



```
prevpow2(x::fmpz)
```

> Return the previous power of $2$ up to including $x$.


<a id='Base.nextpow2-Tuple{Nemo.fmpz}' href='#Base.nextpow2-Tuple{Nemo.fmpz}'>#</a>
**`Base.nextpow2`** &mdash; *Method*.



```
nextpow2(x::fmpz)
```

> Return the next power of $2$ that is at least $x$.


<a id='Base.trailing_zeros-Tuple{Nemo.fmpz}' href='#Base.trailing_zeros-Tuple{Nemo.fmpz}'>#</a>
**`Base.trailing_zeros`** &mdash; *Method*.



```
trailing_zeros(x::fmpz)
```

> Count the trailing zeros in the binary representation of $x$.


<a id='Nemo.clrbit!-Tuple{Nemo.fmpz,Int64}' href='#Nemo.clrbit!-Tuple{Nemo.fmpz,Int64}'>#</a>
**`Nemo.clrbit!`** &mdash; *Method*.



```
clrbit!(x::fmpz, c::Int)
```

> Clear bit $c$ of $x$, where the least significant bit is the $0$-th bit. Note that this function modifies its input in-place.


<a id='Nemo.setbit!-Tuple{Nemo.fmpz,Int64}' href='#Nemo.setbit!-Tuple{Nemo.fmpz,Int64}'>#</a>
**`Nemo.setbit!`** &mdash; *Method*.



```
setbit!(x::fmpz, c::Int)
```

> Set bit $c$ of $x$, where the least significant bit is the $0$-th bit. Note that this function modifies its input in-place.


<a id='Nemo.combit!-Tuple{Nemo.fmpz,Int64}' href='#Nemo.combit!-Tuple{Nemo.fmpz,Int64}'>#</a>
**`Nemo.combit!`** &mdash; *Method*.



```
combit!(x::fmpz, c::Int)
```

> Complement bit $c$ of $x$, where the least significant bit is the $0$-th bit. Note that this function modifies its input in-place.



Here are some examples of bit twiddling.


```
a = fmpz(12)

p = popcount(a)
b = nextpow2(a)
combit!(a, 2)
```

