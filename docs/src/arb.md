```@meta
CurrentModule = Nemo
```

# Real balls

Arbitrary precision real ball arithmetic is supplied by Arb which provides a
ball representation which tracks error bounds rigorously. Real numbers are 
represented in mid-rad interval form $[m \pm r] = [m-r, m+r]$.

The Arb real field is constructed using the `ArbField` constructor. This
constructs the parent object for the Arb real field.

However, we define

```
RealField = ArbField
```

so that one can construct the Arb real field parent object using `RealField`
instead of `ArbField`.

The types of real balls in Nemo are given in the following table, along with
the libraries that provide them and the associated types of the parent objects.

 Library | Field                | Element type  | Parent type
---------|----------------------|---------------|--------------
Arb      | $\mathbb{R}$ (balls) | `arb`         | `ArbField`

All the real field types belong to the `Field` abstract type and the types of
elements in this field, i.e. balls in this case, belong to the `FieldElem`
abstract type.

## Real ball functionality

Real balls in Nemo implement the full AbstractAlgebra.jl field interface.

[https://nemocas.github.io/AbstractAlgebra.jl/latest/fields](https://nemocas.github.io/AbstractAlgebra.jl/latest/fields)

Below, we document the additional functionality provided for real balls.

### Constructors

In order to construct real balls in Nemo, one must first construct the Arb
real field itself. This is accomplished with the following constructor.

```
ArbField(prec::Int)
```

Return the Arb field with precision in bits `prec` used for operations on
interval midpoints. The precision used for interval radii is a fixed
implementation-defined constant (30 bits).

We define

```
RealField = ArbField
```

so that one can use `RealField` in place of `ArbField`.

Here is an example of creating an Arb real field and using the resulting
parent object to coerce values into the resulting field.

**Examples**

```julia
RR = RealField(64)

a = RR("0.25")
b = RR("0.1")
c = RR(0.5)
d = RR(12)
```

Note that whilst one can coerce double precision floating point values into an
Arb real field, unless those values can be represented exactly in double
precision the resulting ball can't be any more precise than the double
precision supplied.

If instead, values can be represented precisely using decimal arithmetic then
one can supply them to Arb using a string. In this case, Arb will store them to
the precision specified when creating the Arb field.

If the values can be stored precisely as a binary floating point number, Arb
will store the values exactly. See the function `isexact` below for more
information.

### Real ball constructors

```@docs
ball(::arb, ::arb)
```

**Examples**

```julia
RR = RealField(64)

c = ball(RR(3), RR("0.0001"))
```

### Conversions

```@docs
convert(::Type{Float64}, ::arb)
```

### Basic manipulation

```@docs
isnonzero(::arb)
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

**Examples**

```julia
RR = RealField(64)

a = RR("1.2 +/- 0.001")
b = RR(3)

ispositive(a)
isfinite(b)
isint(b)
isnegative(a)
c = radius(a)
d = midpoint(b)
f = accuracy_bits(a)
```

### Containment

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
contains{T <: Integer}(::arb, ::Rational{T})
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

**Examples**

```julia
RR = RealField(64)
x = RR("1 +/- 0.001")
y = RR("3")

overlaps(x, y)
contains(x, y)
contains(y, 3)
contains(x, ZZ(1)//2)
contains_zero(x)
contains_positive(y)
```

### Comparison

Nemo provides a full range of comparison operations for Arb balls. Note that a
ball is considered less than another ball if every value in the first ball is
less than every value in the second ball, etc.

In addition to the standard comparison operators, we introduce an exact equality. This
is distinct from arithmetic equality implemented by `==`, which merely compares up to
the minimum of the precisions of its operands.

```@docs
isequal(::arb, ::arb)
```

We also provide a full range of ad hoc comparison operators. These are implemented
directly in Julia, but we document them as though `isless` and `==` were provided.

Function                      |
------------------------------|
`==(x::arb, y::Integer)`      |
`==(x::Integer, y::arb)`      |
`==(x::arb, y::fmpz)`         |
`==(x::fmpz, y::arb)`         |
`==(x::arb, y::Float64)`      |
`==(x::Float64, y::arb)`      |
`isless(x::arb, y::Integer)`  |
`isless(x::Integer, y::arb)`  |
`isless(x::arb, y::fmpz)`     |
`isless(x::fmpz, y::arb)`     |
`isless(x::arb, y::Float64)`  |
`isless(x::Float64, y::arb)`  |
`isless(x::arb, y::BigFloat)` |
`isless(x::BigFloat, y::arb)` |
`isless(x::arb, y::fmpq)`     |
`isless(x::fmpq, y::arb)`     |

**Examples**

```julia
RR = RealField(64)
x = RR("1 +/- 0.001")
y = RR("3")
z = RR("4")

isequal(x, deepcopy(x))
x == 3
ZZ(3) < z
x != 1.23
```

### Absolute value

```@docs
abs(::arb)
```

**Examples**

```julia
RR = RealField(64)
x = RR("-1 +/- 0.001")

a = abs(x)
```

### Shifting

```@docs
ldexp(x::arb, y::Int)
ldexp(x::arb, y::fmpz)
```

**Examples**

```julia
RR = RealField(64)
x = RR("-3 +/- 0.001")

a = ldexp(x, 23)
b = ldexp(x, -ZZ(15))
```

### Miscellaneous operations

```@docs
trim(::arb)
```

```@docs
unique_integer(::arb)
```

```@docs
setunion(::arb, ::arb)
```

**Examples**

```julia
RR = RealField(64)
x = RR("-3 +/- 0.001")
y = RR("2 +/- 0.5")

a = trim(x)
b, c = unique_integer(x)
d = setunion(x, y)
```

### Constants

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

**Examples**

```julia
RR = RealField(200)

a = const_pi(RR)
b = const_e(RR)
c = const_euler(RR)
d = const_glaisher(RR)
```

### Mathematical and special functions

```@docs
floor(::arb)
```

```@docs
ceil(::arb)
```

```@docs
Base.sqrt(::arb)
```

```@docs
rsqrt(::arb)
```

```@docs
sqrt1pm1(::arb)
```

```@docs
sqrtpos(::arb)
```

```@docs
log(::arb)
```

```@docs
log1p(::arb)
```

```@docs
Base.exp(::arb)
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

```@docs
numpart(::fmpz, ::ArbField)
```

```@docs
numpart(::Int, ::ArbField)
```

**Examples**

```julia
RR = RealField(64)

a = floor(exp(RR(1)))
b = sinpi(QQ(5,6), RR)
c = gamma(QQ(1,3), RealField(256))
d = bernoulli(1000, RealField(53))
f = polylog(3, RR(-10))
```

### Linear dependence

```@docs
lindep(::Array{arb, 1}, n::Int)
```

**Examples**

```julia
RR = RealField(128)

a = RR(-0.33198902958450931620250069492231652319)

V = [RR(1), a, a^2, a^3, a^4, a^5]
W = lindep(V, 20)
```

```@docs
simplest_rational_inside(::arb)
```

**Examples**

```julia
RR = RealField(64)
simplest_rational_inside(const_pi(R))
```
