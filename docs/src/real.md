```@meta
CurrentModule = Nemo
DocTestSetup = quote
    using Nemo
end
```

# Arbitrary precision real balls

Arbitrary precision real ball arithmetic is supplied by Arb which provides a
ball representation which tracks error bounds rigorously. Real numbers are 
represented in mid-rad interval form $[m \pm r] = [m-r, m+r]$.

The types of real balls in Nemo are given in the following table, along with
the libraries that provide them and the associated types of the parent objects.

 Library | Field                | Element type       | Parent type
---------|----------------------|--------------------|--------------
Arb      | $\mathbb{R}$ (balls) | `RealFieldElem`    | `RealField`

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

```julia
RealField()
```

Here is an example of creating the real field and using the resulting
parent object to coerce values into the resulting field.

**Examples**

```jldoctest
julia> RR = RealField()
Real field

julia> a = RR("0.25")
0.25000000000000000000

julia> b = RR("0.1 +/- 0.001")
[0.1 +/- 1.01e-3]

julia> c = RR(0.5)
0.50000000000000000000

julia> d = RR(12)
12.000000000000000000
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

```jldoctest; setup = :(RR = RealField())
julia> c = RR(1)
1.0000000000000000000

julia> d = RR(1//2)
0.50000000000000000000
```

Note that for the construction, also the precision can be supplied:

```jldoctest; setup = :(RR = RealField())
julia> c = RR(1//3, precision=100)
[0.33333333333333333333 +/- 3.34e-21]

julia> d = RR(1//3, precision=4)
[0.3 +/- 0.0438]
```

### Conversions

```jldoctest; setup = :(RR = RealField())
julia> convert(Float64, RR(1//3))
0.3333333333333333
```

### Basic manipulation

```@docs
is_nonzero(::RealFieldElem)
```

```@docs
isfinite(::RealFieldElem)
```

```@docs
is_exact(::RealFieldElem)
```

```@docs
isinteger(::RealFieldElem)
```

```@docs
is_positive(::RealFieldElem)
```

```@docs
is_nonnegative(::RealFieldElem)
```

```@docs
is_negative(::RealFieldElem)
```

```@docs
is_nonpositive(::RealFieldElem)
```

```@docs
midpoint(::RealFieldElem)
```

```@docs
radius(::RealFieldElem)
```

```@docs
accuracy_bits(::RealFieldElem)
```

**Examples**

```jldoctest
julia> RR = RealField()
Real field

julia> a = RR("1.2 +/- 0.001")
[1.20 +/- 1.01e-3]

julia> b = RR(3)
3.0000000000000000000

julia> is_positive(a)
true

julia> isfinite(b)
true

julia> isinteger(b)
true

julia> is_negative(a)
false

julia> c = radius(a)
[0.0010000000038417056203 +/- 1.12e-23]

julia> d = midpoint(b)
3.0000000000000000000

julia> f = accuracy_bits(a)
9
```

### Printing

Printing real balls can at first sight be confusing. Lets look at the following
example:

```jldoctest; setup = :(RR = RealField())
julia> a = RR(1)
1.0000000000000000000

julia> b = RR(2)
2.0000000000000000000

julia> c = RR(12)
12.000000000000000000

julia> x = ball(a, b)
[+/- 3.01]

julia> y = ball(c, b)
[1e+1 +/- 4.01]

julia> mid = midpoint(x)
1.0000000000000000000

julia> rad = radius(x)
[2.0000000037252902985 +/- 3.81e-20]

julia> print(x, "\n", y, "\n", mid, "\n", rad)
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
overlaps(::RealFieldElem, ::RealFieldElem)
```

```@docs
contains(::RealFieldElem, ::RealFieldElem)
```

```@docs
contains(::RealFieldElem, ::Integer)
contains(::RealFieldElem, ::ZZRingElem)
contains(::RealFieldElem, ::QQFieldElem)
contains{T <: Integer}(::RealFieldElem, ::Rational{T})
contains(::RealFieldElem, ::BigFloat)
```

The following functions are also provided for determining if a ball intersects
a certain part of the real number line.

```@docs
contains_zero(::RealFieldElem)
```

```@docs
contains_negative(::RealFieldElem)
```

```@docs
contains_positive(::RealFieldElem)
```

```@docs
contains_nonnegative(::RealFieldElem)
```

```@docs
contains_nonpositive(::RealFieldElem)
```

**Examples**

```jldoctest; setup = :(RR = RealField())
julia> x = RR("1 +/- 0.001")
[1.00 +/- 1.01e-3]

julia> y = RR("3")
3.0000000000000000000

julia> overlaps(x, y)
false

julia> contains(x, y)
false

julia> contains(y, 3)
true

julia> contains(x, ZZ(1)//2)
false

julia> contains_zero(x)
false

julia> contains_positive(y)
true
```

### Comparison

Nemo provides a full range of comparison operations for Arb balls. Note that a
ball is considered less than another ball if every value in the first ball is
less than every value in the second ball, etc.

In addition to the standard comparison operators, we introduce an exact equality. This
is distinct from arithmetic equality implemented by `==`, which merely compares up to
the minimum of the precisions of its operands.

```@docs
isequal(::RealFieldElem, ::RealFieldElem)
```

We also provide a full range of ad hoc comparison operators. These are implemented
directly in Julia, but we document them as though `isless` and `==` were provided.

Function                      |
------------------------------|
`==(x::RealFieldElem, y::Integer)`      |
`==(x::Integer, y::RealFieldElem)`      |
`==(x::RealFieldElem, y::ZZRingElem)`         |
`==(x::ZZRingElem, y::RealFieldElem)`         |
`==(x::RealFieldElem, y::Float64)`      |
`==(x::Float64, y::RealFieldElem)`      |
`isless(x::RealFieldElem, y::Integer)`  |
`isless(x::Integer, y::RealFieldElem)`  |
`isless(x::RealFieldElem, y::ZZRingElem)`     |
`isless(x::ZZRingElem, y::RealFieldElem)`     |
`isless(x::RealFieldElem, y::Float64)`  |
`isless(x::Float64, y::RealFieldElem)`  |
`isless(x::RealFieldElem, y::BigFloat)` |
`isless(x::BigFloat, y::RealFieldElem)` |
`isless(x::RealFieldElem, y::QQFieldElem)`     |
`isless(x::QQFieldElem, y::RealFieldElem)`     |

**Examples**

```jldoctest; setup = :(RR = RealField())
julia> x = RR("1 +/- 0.001")
[1.00 +/- 1.01e-3]

julia> y = RR("3")
3.0000000000000000000

julia> z = RR("4")
4.0000000000000000000

julia> isequal(x, deepcopy(x))
true

julia> x == 3
false

julia> ZZ(3) < z
true

julia> x != 1.23
true
```

### Absolute value

**Examples**

```jldoctest; setup = :(RR = RealField())
julia> x = RR("-1 +/- 0.001")
[-1.00 +/- 1.01e-3]

julia> a = abs(x)
[1.00 +/- 1.01e-3]
```

### Shifting

**Examples**

```jldoctest; setup = :(RR = RealField())
julia> x = RR("-3 +/- 0.001")
[-3.00 +/- 1.01e-3]

julia> a = ldexp(x, 23)
[-2.52e+7 +/- 4.26e+4]

julia> b = ldexp(x, -ZZ(15))
[-9.16e-5 +/- 7.78e-8]
```

### Miscellaneous operations

```@docs
add_error!(::RealFieldElem, ::RealFieldElem)
```

```@docs
trim(::RealFieldElem)
```

```@docs
unique_integer(::RealFieldElem)
```

```@docs
setunion(::RealFieldElem, ::RealFieldElem)
```

**Examples**

```jldoctest; setup = :(RR = RealField())
julia> x = RR("-3 +/- 0.001")
[-3.00 +/- 1.01e-3]

julia> y = RR("2 +/- 0.5")
[2e+0 +/- 0.501]

julia> a = trim(x)
[-3.00 +/- 1.01e-3]

julia> b, c = unique_integer(x)
(true, -3)

julia> d = setunion(x, y)
[+/- 3.01]
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

```jldoctest; setup = :(RR = RealField())
julia> a = const_pi(RR)
[3.141592653589793239 +/- 5.96e-19]

julia> b = const_e(RR)
[2.718281828459045235 +/- 4.29e-19]

julia> c = const_euler(RR)
[0.5772156649015328606 +/- 4.35e-20]

julia> d = const_glaisher(RR)
[1.282427129100622637 +/- 3.01e-19]
```

### Mathematical and special functions

```@docs
rsqrt(::RealFieldElem)
```

```@docs
sqrt1pm1(::RealFieldElem)
```

```@docs
sqrtpos(::RealFieldElem)
```

```@docs
gamma(::RealFieldElem)
```

```@docs
lgamma(::RealFieldElem)
```

```@docs
rgamma(::RealFieldElem)
```

```@docs
digamma(::RealFieldElem)
```

```@docs
gamma(::RealFieldElem, ::RealFieldElem)
```

```@docs
gamma_regularized(::RealFieldElem, ::RealFieldElem)
```

```@docs
gamma_lower(::RealFieldElem, ::RealFieldElem)
```

```@docs
gamma_lower_regularized(::RealFieldElem, ::RealFieldElem)
```

```@docs
zeta(::RealFieldElem)
```

```@docs
atan2(::RealFieldElem, ::RealFieldElem)
```

```@docs
agm(::RealFieldElem, ::RealFieldElem)
```

```@docs
zeta(::RealFieldElem, ::RealFieldElem)
```

```@docs
root(::RealFieldElem, ::Int)
```

```@docs
factorial(::RealFieldElem)
```

```@docs
factorial(::Int, ::RealField)
```

```@docs
binomial(::RealFieldElem, ::UInt)
```

```@docs
binomial(::UInt, ::UInt, ::RealField)
```

```@docs
fibonacci(::ZZRingElem, ::RealField)
```

```@docs
fibonacci(::Int, ::RealField)
```

```@docs
gamma(::ZZRingElem, ::RealField)
```

```@docs
gamma(::QQFieldElem, ::RealField)
```

```@docs
zeta(::Int, ::RealField)
```

```@docs
bernoulli(::Int, ::RealField)
```

```@docs
rising_factorial(::RealFieldElem, ::Int)
```

```@docs
rising_factorial(::QQFieldElem, ::Int, ::RealField)
```

```@docs
rising_factorial2(::RealFieldElem, ::Int)
```

```@docs
polylog(::Union{RealFieldElem,Int}, ::RealFieldElem)
```

```@docs
chebyshev_t(::Int, ::RealFieldElem)
```

```@docs
chebyshev_u(::Int, ::RealFieldElem)
```

```@docs
chebyshev_t2(::Int, ::RealFieldElem)
```

```@docs
chebyshev_u2(::Int, ::RealFieldElem)
```

```@docs
bell(::ZZRingElem, ::RealField)
```

```@docs
bell(::Int, ::RealField)
```

```@docs
numpart(::ZZRingElem, ::RealField)
```

```@docs
numpart(::Int, ::RealField)
```

```@docs
airy_ai(::RealFieldElem)
```

```@docs
airy_ai_prime(::RealFieldElem)
```

```@docs
airy_bi(::RealFieldElem)
```

```@docs
airy_bi_prime(::RealFieldElem)
```

**Examples**

```jldoctest; setup = :(RR = RealField())
julia> a = floor(exp(RR(1)))
2.0000000000000000000

julia> b = sinpi(QQ(5,6), RR)
0.50000000000000000000

julia> c = gamma(QQ(1,3), RR)
[2.678938534707747634 +/- 7.13e-19]

julia> d = bernoulli(1000, RR)
[-5.318704469415522036e+1769 +/- 6.61e+1750]

julia> f = polylog(3, RR(-10))
[-5.92106480375697 +/- 6.68e-15]
```

### Linear dependence

```@docs
lindep(::Vector{RealFieldElem}, n::Int)
```

```@docs
simplest_rational_inside(::RealFieldElem)
```

### Random generation

```@docs
rand(::RealField)
```

**Examples**

```julia
a = rand(RR)
b = rand(RR; randtype = :null_exact)
c = rand(RR; randtype = :exact)
d = rand(RR; randtype = :special)
```
