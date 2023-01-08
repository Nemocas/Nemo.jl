```@meta
CurrentModule = Nemo
```

# Arbitrary precision real balls

Arbitrary precision real ball arithmetic is supplied by Arb which provides a
ball representation which tracks error bounds rigorously. Real numbers are 
represented in mid-rad interval form $[m \pm r] = [m-r, m+r]$.

The types of real balls in Nemo are given in the following table, along with
the libraries that provide them and the associated types of the parent objects.

 Library | Field                | Element type  | Parent type
---------|----------------------|---------------|--------------
Arb      | $\mathbb{R}$ (balls) | `RealElem`    | `RealField`

The real field types belong to the `Field` abstract type and the types of
elements in this field, i.e. balls in this case, belong to the `FieldElem`
abstract type.

## Real ball functionality

Real balls in Nemo provide all the field functionality described in AbstractAlgebra:

<https://nemocas.github.io/AbstractAlgebra.jl/stable/field>

Below, we document the additional functionality provided for real balls.

### [Precision management](@id precision_management)

Precision for ball arithmetic and creation of elements can be controlled
using the functions:

```@docs
precision(::Type{Balls})
set_precision!(::Type{Balls}, n::Int)
set_precision!(f::Any, ::Type{Balls}, n::Int)
```

!!! info
    This functions are not thread-safe.

### Constructors

In order to construct real balls in Nemo, one must first construct the Arb
real field itself. This is accomplished with the following constructor.

```
RealField()
```

Here is an example of creating the real field and using the resulting
parent object to coerce values into the resulting field.

**Examples**

```julia
RR = RealField()

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

Using coercion into the real field, new elements can be created.

**Examples**

```julia
RR = RealField()

c = RR(1)
d = RR(1//2)
```

Note that for the construction, also the precision can be supplied:

```julia
RR = RealField()

c = RR(1, precision = 100)
d = RR(1//2, precision = 4)
```

### Conversions

```julia
RR = RealField()

convert(Float64, RR(1//3))
```

### Basic manipulation

```@docs
is_nonzero(::RealElem)
```

```@docs
isfinite(::RealElem)
```

```@docs
is_exact(::RealElem)
```

```@docs
isinteger(::RealElem)
```

```@docs
is_positive(::RealElem)
```

```@docs
is_nonnegative(::RealElem)
```

```@docs
is_negative(::RealElem)
```

```@docs
is_nonpositive(::RealElem)
```

```@docs
midpoint(::RealElem)
```

```@docs
radius(::RealElem)
```

```@docs
accuracy_bits(::RealElem)
```

**Examples**

```julia
RR = RealField()

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
RR = RealField()

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
overlaps(::RealElem, ::RealElem)
```

```@docs
contains(::RealElem, ::RealElem)
```

```@docs
contains(::RealElem, ::Integer)
contains(::RealElem, ::fmpz)
contains(::RealElem, ::fmpq)
contains{T <: Integer}(::RealElem, ::Rational{T})
contains(::RealElem, ::BigFloat)
```

The following functions are also provided for determining if a ball intersects
a certain part of the real number line.

```@docs
contains_zero(::RealElem)
```

```@docs
contains_negative(::RealElem)
```

```@docs
contains_positive(::RealElem)
```

```@docs
contains_nonnegative(::RealElem)
```

```@docs
contains_nonpositive(::RealElem)
```

**Examples**

```julia
RR = RealField()
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
isequal(::RealElem, ::RealElem)
```

We also provide a full range of ad hoc comparison operators. These are implemented
directly in Julia, but we document them as though `isless` and `==` were provided.

Function                      |
------------------------------|
`==(x::RealElem, y::Integer)`      |
`==(x::Integer, y::RealElem)`      |
`==(x::RealElem, y::fmpz)`         |
`==(x::fmpz, y::RealElem)`         |
`==(x::RealElem, y::Float64)`      |
`==(x::Float64, y::RealElem)`      |
`isless(x::RealElem, y::Integer)`  |
`isless(x::Integer, y::RealElem)`  |
`isless(x::RealElem, y::fmpz)`     |
`isless(x::fmpz, y::RealElem)`     |
`isless(x::RealElem, y::Float64)`  |
`isless(x::Float64, y::RealElem)`  |
`isless(x::RealElem, y::BigFloat)` |
`isless(x::BigFloat, y::RealElem)` |
`isless(x::RealElem, y::fmpq)`     |
`isless(x::fmpq, y::RealElem)`     |

**Examples**

```julia
RR = RealField()
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
RR = RealField()
x = RR("-1 +/- 0.001")

a = abs(x)
```

### Shifting

**Examples**

```julia
RR = RealField()
x = RR("-3 +/- 0.001")

a = ldexp(x, 23)
b = ldexp(x, -ZZ(15))
```

### Miscellaneous operations

```@docs
add_error!(::RealElem, ::RealElem)
```

```@docs
trim(::RealElem)
```

```@docs
unique_integer(::RealElem)
```

```@docs
setunion(::RealElem, ::RealElem)
```

**Examples**

```julia
RR = RealField()
x = RR("-3 +/- 0.001")
y = RR("2 +/- 0.5")

a = trim(x)
b, c = unique_integer(x)
d = setunion(x, y)
```

### Constants

```@docs
const_pi(::RealField)
```

```@docs
const_e(::RealField)
```

```@docs
const_log2(::RealField)
```

```@docs
const_log10(::RealField)
```

```@docs
const_euler(::RealField)
```

```@docs
const_catalan(::RealField)
```

```@docs
const_khinchin(::RealField)
```

```@docs
const_glaisher(::RealField)
```

**Examples**

```julia
RR = RealField()

a = const_pi(RR)
b = const_e(RR)
c = const_euler(RR)
d = const_glaisher(RR)
```

### Mathematical and special functions

```@docs
rsqrt(::RealElem)
```

```@docs
sqrt1pm1(::RealElem)
```

```@docs
sqrtpos(::RealElem)
```

```@docs
gamma(::RealElem)
```

```@docs
lgamma(::RealElem)
```

```@docs
rgamma(::RealElem)
```

```@docs
digamma(::RealElem)
```

```@docs
gamma(::RealElem, ::RealElem)
```

```@docs
gamma_regularized(::RealElem, ::RealElem)
```

```@docs
gamma_lower(::RealElem, ::RealElem)
```

```@docs
gamma_lower_regularized(::RealElem, ::RealElem)
```

```@docs
zeta(::RealElem)
```

```@docs
atan2(::RealElem, ::RealElem)
```

```@docs
agm(::RealElem, ::RealElem)
```

```@docs
zeta(::RealElem, ::RealElem)
```

```@docs
root(::RealElem, ::Int)
```

```@docs
factorial(::RealElem)
```

```@docs
factorial(::Int, ::RealField)
```

```@docs
binomial(::RealElem, ::UInt)
```

```@docs
binomial(::UInt, ::UInt, ::RealField)
```

```@docs
fibonacci(::fmpz, ::RealField)
```

```@docs
fibonacci(::Int, ::RealField)
```

```@docs
gamma(::fmpz, ::RealField)
```

```@docs
gamma(::fmpq, ::RealField)
```

```@docs
zeta(::Int, ::RealField)
```

```@docs
bernoulli(::Int, ::RealField)
```

```@docs
rising_factorial(::RealElem, ::Int)
```

```@docs
rising_factorial(::fmpq, ::Int, ::RealField)
```

```@docs
rising_factorial2(::RealElem, ::Int)
```

```@docs
polylog(::Union{RealElem,Int}, ::RealElem)
```

```@docs
chebyshev_t(::Int, ::RealElem)
```

```@docs
chebyshev_u(::Int, ::RealElem)
```

```@docs
chebyshev_t2(::Int, ::RealElem)
```

```@docs
chebyshev_u2(::Int, ::RealElem)
```

```@docs
bell(::fmpz, ::RealField)
```

```@docs
bell(::Int, ::RealField)
```

```@docs
numpart(::fmpz, ::RealField)
```

```@docs
numpart(::Int, ::RealField)
```

```@docs
airy_ai(::RealElem)
```

```@docs
airy_ai_prime(::RealElem)
```

```@docs
airy_bi(::RealElem)
```

```@docs
airy_bi_prime(::RealElem)
```

**Examples**

```julia
RR = RealField()

a = floor(exp(RR(1)))
b = sinpi(QQ(5,6), RR)
c = gamma(QQ(1,3), RealField()
d = bernoulli(1000, RealField()
f = polylog(3, RR(-10))
```

### Linear dependence

```@docs
lindep(::Vector{RealElem}, n::Int)
```

**Examples**

```julia
RR = RealField()

a = RR(-0.33198902958450931620250069492231652319)

V = [RR(1), a, a^2, a^3, a^4, a^5]
W = lindep(V, 20)
```

```@docs
simplest_rational_inside(::RealElem)
```

**Examples**

```julia
RR = RealField()
simplest_rational_inside(const_pi(RR))
```

### Random generation

```@docs
rand(::RealField)
```

**Examples**

```julia
RR = RealField()

a = rand(RR)
b = rand(RR; randtype = :null_exact)
c = rand(RR; randtype = :exact)
d = rand(RR; randtype = :special)
```
