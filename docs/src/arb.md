```@meta
CurrentModule = Nemo
DocTestSetup = quote
    using Nemo
end
```

# Fixed precision real balls

Fixed precision real ball arithmetic is supplied by Arb which provides a
ball representation which tracks error bounds rigorously. Real numbers are 
represented in mid-rad interval form $[m \pm r] = [m-r, m+r]$.

The Arb real field is constructed using the `ArbField` constructor. This
constructs the parent object for the Arb real field.

The types of real balls in Nemo are given in the following table, along with
the libraries that provide them and the associated types of the parent objects.

 Library | Field                | Element type  | Parent type
---------|----------------------|---------------|--------------
Arb      | $\mathbb{R}$ (balls) | `ArbFieldElem`         | `ArbField`

All the real field types belong to the `Field` abstract type and the types of
elements in this field, i.e. balls in this case, belong to the `FieldElem`
abstract type.

## Real ball functionality

Real balls in Nemo provide all the field functionality described in AbstractAlgebra:

<https://nemocas.github.io/AbstractAlgebra.jl/stable/field>

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

Here is an example of creating an Arb real field and using the resulting
parent object to coerce values into the resulting field.

**Examples**

```julia
RR = ArbField(64)

a = RR("0.25")
b = RR("0.1 +/- 0.001")
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
will store the values exactly. See the function `is_exact` below for more
information.

### Real ball constructors

```@docs
ball(::ArbFieldElem, ::ArbFieldElem)
```

**Examples**

```julia
RR = ArbField(64)

c = ball(RR(3), RR("0.0001"))
```

### Conversions

```julia
RR = ArbField(64)

convert(Float64, RR(1//3))
```

### Basic manipulation

```@docs
is_nonzero(::ArbFieldElem)
```

```@docs
isfinite(::ArbFieldElem)
```

```@docs
is_exact(::ArbFieldElem)
```

```@docs
isinteger(::ArbFieldElem)
```

```@docs
is_positive(::ArbFieldElem)
```

```@docs
is_nonnegative(::ArbFieldElem)
```

```@docs
is_negative(::ArbFieldElem)
```

```@docs
is_nonpositive(::ArbFieldElem)
```

```@docs
midpoint(::ArbFieldElem)
```

```@docs
radius(::ArbFieldElem)
```

```@docs
accuracy_bits(::ArbFieldElem)
```

**Examples**

```julia
RR = ArbField(64)

a = RR("1.2 +/- 0.001")
b = RR(3)

is_positive(a)
isfinite(b)
isinteger(b)
is_negative(a)
c = radius(a)
d = midpoint(b)
f = accuracy_bits(a)
```

### Printing

Printing real balls can at first sight be confusing. Lets look at the following
example:

```julia
RR = ArbField(64)

a = RR(1)
b = RR(2)
c = RR(12)

x = ball(a, b)
y = ball(c, b)

mid = midpoint(x)
rad = radius(x)

print(x, "\n", y, "\n", mid, "\n", rad)
```

which generates

```
[+/- 3.01]
[1e+1 +/- 4.01]
1.0000000000000000000
[2.0000000037252902985 +/- 3.81e-20]
```

The first reason that `c` is not printed as `[1 +/- 2]` is that the
midpoint does not have a greater exponent than the radius in its scientific
notation. For similar reasons `y` is not printed as `[12 +/- 2]`.

The second reason is that we get an additional error term after our addition. As we
see, `radius(c)` is not equal to $2$, which when printed rounds it up to a
reasonable decimal place. This is because real balls keep track of
rounding errors of basic arithmetic.

### Containment

It is often necessary to determine whether a given exact value or ball is
contained in a given real ball or whether two balls overlap. The following
functions are provided for this purpose.

```@docs
overlaps(::ArbFieldElem, ::ArbFieldElem)
```

```@docs
contains(::ArbFieldElem, ::ArbFieldElem)
```

```@docs
contains(::ArbFieldElem, ::Integer)
contains(::ArbFieldElem, ::ZZRingElem)
contains(::ArbFieldElem, ::QQFieldElem)
contains{T <: Integer}(::ArbFieldElem, ::Rational{T})
contains(::ArbFieldElem, ::BigFloat)
```

The following functions are also provided for determining if a ball intersects
a certain part of the real number line.

```@docs
contains_zero(::ArbFieldElem)
```

```@docs
contains_negative(::ArbFieldElem)
```

```@docs
contains_positive(::ArbFieldElem)
```

```@docs
contains_nonnegative(::ArbFieldElem)
```

```@docs
contains_nonpositive(::ArbFieldElem)
```

**Examples**

```julia
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

### Comparison

Nemo provides a full range of comparison operations for Arb balls. Note that a
ball is considered less than another ball if every value in the first ball is
less than every value in the second ball, etc.

In addition to the standard comparison operators, we introduce an exact equality. This
is distinct from arithmetic equality implemented by `==`, which merely compares up to
the minimum of the precisions of its operands.

```@docs
isequal(::ArbFieldElem, ::ArbFieldElem)
```

We also provide a full range of ad hoc comparison operators. These are implemented
directly in Julia, but we document them as though `isless` and `==` were provided.

Function                      |
------------------------------|
`==(x::ArbFieldElem, y::Integer)`      |
`==(x::Integer, y::ArbFieldElem)`      |
`==(x::ArbFieldElem, y::ZZRingElem)`         |
`==(x::ZZRingElem, y::ArbFieldElem)`         |
`==(x::ArbFieldElem, y::Float64)`      |
`==(x::Float64, y::ArbFieldElem)`      |
`isless(x::ArbFieldElem, y::Integer)`  |
`isless(x::Integer, y::ArbFieldElem)`  |
`isless(x::ArbFieldElem, y::ZZRingElem)`     |
`isless(x::ZZRingElem, y::ArbFieldElem)`     |
`isless(x::ArbFieldElem, y::Float64)`  |
`isless(x::Float64, y::ArbFieldElem)`  |
`isless(x::ArbFieldElem, y::BigFloat)` |
`isless(x::BigFloat, y::ArbFieldElem)` |
`isless(x::ArbFieldElem, y::QQFieldElem)`     |
`isless(x::QQFieldElem, y::ArbFieldElem)`     |

**Examples**

```julia
RR = ArbField(64)
x = RR("1 +/- 0.001")
y = RR("3")
z = RR("4")

isequal(x, deepcopy(x))
x == 3
ZZ(3) < z
x != 1.23
```

### Absolute value

**Examples**

```julia
RR = ArbField(64)
x = RR("-1 +/- 0.001")

a = abs(x)
```

### Shifting

**Examples**

```julia
RR = ArbField(64)
x = RR("-3 +/- 0.001")

a = ldexp(x, 23)
b = ldexp(x, -ZZ(15))
```

### Miscellaneous operations

```@docs
add_error!(::ArbFieldElem, ::ArbFieldElem)
```

```@docs
trim(::ArbFieldElem)
```

```@docs
unique_integer(::ArbFieldElem)
```

```@docs
setunion(::ArbFieldElem, ::ArbFieldElem)
```

**Examples**

```julia
RR = ArbField(64)
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
RR = ArbField(200)

a = const_pi(RR)
b = const_e(RR)
c = const_euler(RR)
d = const_glaisher(RR)
```

### Mathematical and special functions

```@docs
rsqrt(::ArbFieldElem)
```

```@docs
sqrt1pm1(::ArbFieldElem)
```

```@docs
sqrtpos(::ArbFieldElem)
```

```@docs
gamma(::ArbFieldElem)
```

```@docs
lgamma(::ArbFieldElem)
```

```@docs
rgamma(::ArbFieldElem)
```

```@docs
digamma(::ArbFieldElem)
```

```@docs
gamma(::ArbFieldElem, ::ArbFieldElem)
```

```@docs
gamma_regularized(::ArbFieldElem, ::ArbFieldElem)
```

```@docs
gamma_lower(::ArbFieldElem, ::ArbFieldElem)
```

```@docs
gamma_lower_regularized(::ArbFieldElem, ::ArbFieldElem)
```

```@docs
zeta(::ArbFieldElem)
```

```@docs
atan2(::ArbFieldElem, ::ArbFieldElem)
```

```@docs
agm(::ArbFieldElem, ::ArbFieldElem)
```

```@docs
zeta(::ArbFieldElem, ::ArbFieldElem)
```

```@docs
root(::ArbFieldElem, ::Int)
```

```@docs
factorial(::ArbFieldElem)
```

```@docs
factorial(::Int, ::ArbField)
```

```@docs
binomial(::ArbFieldElem, ::UInt)
```

```@docs
binomial(::UInt, ::UInt, ::ArbField)
```

```@docs
fibonacci(::ZZRingElem, ::ArbField)
```

```@docs
fibonacci(::Int, ::ArbField)
```

```@docs
gamma(::ZZRingElem, ::ArbField)
```

```@docs
gamma(::QQFieldElem, ::ArbField)
```

```@docs
zeta(::Int, ::ArbField)
```

```@docs
bernoulli(::Int, ::ArbField)
```

```@docs
rising_factorial(::ArbFieldElem, ::Int)
```

```@docs
rising_factorial(::QQFieldElem, ::Int, ::ArbField)
```

```@docs
rising_factorial2(::ArbFieldElem, ::Int)
```

```@docs
polylog(::Union{ArbFieldElem,Int}, ::ArbFieldElem)
```

```@docs
chebyshev_t(::Int, ::ArbFieldElem)
```

```@docs
chebyshev_u(::Int, ::ArbFieldElem)
```

```@docs
chebyshev_t2(::Int, ::ArbFieldElem)
```

```@docs
chebyshev_u2(::Int, ::ArbFieldElem)
```

```@docs
bell(::ZZRingElem, ::ArbField)
```

```@docs
bell(::Int, ::ArbField)
```

```@docs
numpart(::ZZRingElem, ::ArbField)
```

```@docs
numpart(::Int, ::ArbField)
```

```@docs
airy_ai(::ArbFieldElem)
```

```@docs
airy_ai_prime(::ArbFieldElem)
```

```@docs
airy_bi(::ArbFieldElem)
```

```@docs
airy_bi_prime(::ArbFieldElem)
```

**Examples**

```julia
RR = ArbField(64)

a = floor(exp(RR(1)))
b = sinpi(QQ(5,6), RR)
c = gamma(QQ(1,3), ArbField(256))
d = bernoulli(1000, ArbField(53))
f = polylog(3, RR(-10))
```

### Linear dependence

```@docs
lindep(::Vector{ArbFieldElem}, n::Int)
```

**Examples**

```julia
RR = ArbField(128)

a = RR(-0.33198902958450931620250069492231652319)

V = [RR(1), a, a^2, a^3, a^4, a^5]
W = lindep(V, 20)
```

```@docs
simplest_rational_inside(::ArbFieldElem)
```

**Examples**

```julia
RR = ArbField(64)
simplest_rational_inside(const_pi(RR))
```

### Random generation

```@docs
rand(::ArbField)
```

**Examples**

```julia
RR = ArbField(100)

a = rand(RR)
b = rand(RR; randtype = :null_exact)
c = rand(RR; randtype = :exact)
d = rand(RR; randtype = :special)
```
