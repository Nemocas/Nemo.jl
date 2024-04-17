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

```jldoctest
julia> RR = ArbField(64)
Real Field with 64 bits of precision and error bounds

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

```@docs
ball(::ArbFieldElem, ::ArbFieldElem)
```

**Examples**

```jldoctest
julia> RR = ArbField(64)
Real Field with 64 bits of precision and error bounds

julia> c = ball(RR(3), RR("0.0001"))
[3.000 +/- 1.01e-4]
```

### Conversions

```jldoctest
julia> RR = ArbField(64)
Real Field with 64 bits of precision and error bounds

julia> convert(Float64, RR(1//3))
0.3333333333333333
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

```jldoctest
julia> RR = ArbField(64)
Real Field with 64 bits of precision and error bounds

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

```jldoctest
julia> RR = ArbField(64)
Real Field with 64 bits of precision and error bounds

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

```jldoctest
julia> RR = ArbField(64)
Real Field with 64 bits of precision and error bounds

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

```jldoctest
julia> RR = ArbField(64)
Real Field with 64 bits of precision and error bounds

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

julia> 3 == y
true
```

### Absolute value

**Examples**

```jldoctest
julia> RR = ArbField(64)
Real Field with 64 bits of precision and error bounds

julia> x = RR("-1 +/- 0.001")
[-1.00 +/- 1.01e-3]

julia> a = abs(x)
[1.00 +/- 1.01e-3]
```

### Shifting

**Examples**

```jldoctest
julia> RR = ArbField(64)
Real Field with 64 bits of precision and error bounds

julia> x = RR("-3 +/- 0.001")
[-3.00 +/- 1.01e-3]

julia> a = ldexp(x, 23)
[-2.52e+7 +/- 4.26e+4]

julia> b = ldexp(x, -ZZ(15))
[-9.16e-5 +/- 7.78e-8]
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

```jldoctest
julia> RR = ArbField(64)
Real Field with 64 bits of precision and error bounds

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

```jldoctest
julia> RR = ArbField(200)
Real Field with 200 bits of precision and error bounds

julia> a = const_pi(RR)
[3.14159265358979323846264338327950288419716939937510582097494 +/- 5.73e-60]

julia> b = const_e(RR)
[2.71828182845904523536028747135266249775724709369995957496697 +/- 7.06e-60]

julia> c = const_euler(RR)
[0.577215664901532860606512090082402431042159335939923598805767 +/- 5.37e-61]

julia> d = const_glaisher(RR)
[1.28242712910062263687534256886979172776768892732500119206374 +/- 2.18e-60]
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

```jldoctest
julia> RR = ArbField(64)
Real Field with 64 bits of precision and error bounds

julia> a = floor(exp(RR(1)))
2.0000000000000000000

julia> b = sinpi(QQ(5,6), RR)
0.50000000000000000000

julia> c = gamma(QQ(1,3), ArbField(256))
[2.6789385347077476336556929409746776441286893779573011009504283275904176101677 +/- 6.71e-77]

julia> d = bernoulli(1000, ArbField(53))
[-5.318704469415522e+1769 +/- 8.20e+1753]

julia> f = polylog(3, RR(-10))
[-5.92106480375697 +/- 6.68e-15]
```

### Linear dependence

```@docs
lindep(::Vector{ArbFieldElem}, n::Int)
```

**Examples**

```jldoctest
julia> RR = ArbField(128)
Real Field with 128 bits of precision and error bounds

julia> a = RR(-0.33198902958450931620250069492231652319) # real root of x^5 + 3x + 1
[-0.331989029584509320880414406929048709571 +/- 3.62e-40]

julia> V = [RR(1), a, a^2, a^3, a^4, a^5]
6-element Vector{ArbFieldElem}:
 1.00000000000000000000000000000000000000
 [-0.331989029584509320880414406929048709571 +/- 3.62e-40]
 [0.110216715764464205102727554344054759368 +/- 3.32e-40]
 [-0.0365907405106361618384680031506015710184 +/- 8.30e-41]
 [0.0121477244339046924274232580429164920524 +/- 2.83e-41]
 [-0.00403291124647205167662794872826031818905 +/- 7.87e-42]

julia> W = lindep(V, 20)
6-element Vector{ZZRingElem}:
 1
 3
 0
 0
 0
 1
```

```@docs
simplest_rational_inside(::ArbFieldElem)
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
