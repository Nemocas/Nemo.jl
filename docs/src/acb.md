```@meta
CurrentModule = Nemo
DocTestSetup = quote
    using Nemo
end
```

# Fixed precisioncomplex balls

Arbitrary precision complex ball arithmetic is supplied by Arb which provides a
ball representation which tracks error bounds rigorously. Complex numbers are 
represented in rectangular form $a+bi$ where $a,b$ are `arb` balls.

The Arb complex field is constructed using the `AcbField` constructor. This
constructs the parent object for the Arb complex field.

The types of complex boxes in Nemo are given in the following table, along with
the libraries that provide them and the associated types of the parent objects.

 Library | Field                | Element type  | Parent type
---------|----------------------|---------------|--------------
Arb      | $\mathbb{C}$ (boxes) | `acb`         | `AcbField`

All the complex field types belong to the `Field` abstract type and the types of
elements in this field, i.e. complex boxes in this case, belong to the
`FieldElem` abstract type.

## Complex ball functionality

The complex balls in Nemo provide all the field functionality defined by AbstractAlgebra:.

<https://nemocas.github.io/AbstractAlgebra.jl/stable/field>

Below, we document the additional functionality provided for complex balls.

### Complex field constructors

In order to construct complex boxes in Nemo, one must first construct the Arb
complex field itself. This is accomplished with the following constructor.

```
AcbField(prec::Int)
```

Return the Arb complex field with precision in bits `prec` used for operations
on interval midpoints. The precision used for interval radii is a fixed
implementation-defined constant (30 bits).

Here is an example of creating an Arb complex field and using the resulting
parent object to coerce values into the resulting field.

**Examples**

```jldoctest
julia> CC = AcbField(64)
Complex Field with 64 bits of precision and error bounds

julia> a = CC("0.25")
0.25000000000000000000

julia> b = CC("0.1")
[0.100000000000000000 +/- 1.22e-20]

julia> c = CC(0.5)
0.50000000000000000000

julia> d = CC(12)
12.000000000000000000
```

Note that whilst one can coerce double precision floating point values into an
Arb complex field, unless those values can be represented exactly in double
precision the resulting ball can't be any more precise than the double
precision supplied.

If instead, values can be represented precisely using decimal arithmetic then
one can supply them to Arb using a string. In this case, Arb will store them to
the precision specified when creating the Arb complex field.

If the values can be stored precisely as a binary floating point number, Arb
will store the values exactly. See the function `is_exact` below for more
information.

### Constructors

```@docs
onei(::AcbField)
```

**Examples**

```jldoctest
julia> CC = AcbField(64)
Complex Field with 64 bits of precision and error bounds

julia> c = onei(CC)
1.0000000000000000000*im
```

## Basic functionality

The following basic functionality is provided by the default Arb complex field
implementation in Nemo, to support construction of generic rings over complex
fields. Any custom complex field implementation in Nemo should provide analogues
of these functions along with the usual arithmetic operations.

```
parent_type(::Type{acb})
```

Gives the type of the parent object of an Arb complex field element.

```
elem_type(R::AcbField)
```

Given the parent object for an Arb complex field, return the type of elements
of the field.

```
mul!(c::acb, a::acb, b::acb)
```

Multiply $a$ by $b$ and set the existing Arb complex field element $c$ to the
result. This function is provided for performance reasons as it saves
allocating a new object for the result and eliminates associated garbage
collection.

```
addeq!(c::acb, a::acb)
```

In-place addition adds $a$ to $c$ and sets $c$ to the result. This function
is provided for performance reasons as it saves allocating a new object for
the result and eliminates associated garbage collection.

```
deepcopy(a::acb)
```

Return a copy of the Arb complex field element $a$, recursively copying the
internal data. Arb complex field elements are mutable in Nemo so a shallow
copy is not sufficient.

Given the parent object `R` for an Arb complex field, the following coercion
functions are provided to coerce various elements into the Arb complex field.
Developers provide these by overloading the `call` operator for the complex
field parent objects.

```
R()
```

Coerce zero into the Arb complex field.

```
R(n::Integer)
R(f::ZZRingElem)
R(q::QQFieldElem)
```

Coerce an integer or rational value into the Arb complex field.

```
R(f::Float64)
R(f::BigFloat)
```

Coerce the given floating point number into the Arb complex field.

```
R(f::AbstractString)
R(f::AbstractString, g::AbstractString)
```

Coerce the decimal number, given as a string, into the Arb complex field. In
each case $f$ is the real part and $g$ is the imaginary part.

```
R(f::arb)
```

Coerce the given Arb real ball into the Arb complex field.

```
R(f::acb)
```

Take an Arb complex field element that is already in an Arb field and simply
return it. A copy of the original is not made.

Here are some examples of coercing elements into the Arb complex field.

```jldoctest
julia> RR = ArbField(64)
Real Field with 64 bits of precision and error bounds

julia> CC = AcbField(64)
Complex Field with 64 bits of precision and error bounds

julia> a = CC(3)
3.0000000000000000000

julia> b = CC(QQ(2,3))
[0.6666666666666666666 +/- 8.48e-20]

julia> c = CC("3 +/- 0.0001")
[3.000 +/- 1.01e-4]

julia> d = CC("-1.24e+12345")
[-1.240000000000000000e+12345 +/- 1.16e+12326]

julia> f = CC("nan +/- inf")
nan

julia> g = CC(RR(3))
3.0000000000000000000
```

In addition to the above, developers of custom complex field types must ensure
that they provide the equivalent of the function `base_ring(R::AcbField)`
which should return `Union{}`. In addition to this they should ensure that
each complex field element contains a field `parent` specifying the parent
object of the complex field element, or at least supply the equivalent of the
function `parent(a::acb)` to return the parent object of a complex field
element.

### Basic manipulation

```@docs
isfinite(::acb)
```

```@docs
is_exact(::acb)
```

```@docs
isinteger(::acb)
```

```@docs
accuracy_bits(::acb)
```

**Examples**

```jldoctest
julia> CC = AcbField(64)
Complex Field with 64 bits of precision and error bounds

julia> a = CC("1.2 +/- 0.001")
[1.20 +/- 1.01e-3]

julia> b = CC(3)
3.0000000000000000000

julia> isreal(a)
true

julia> isfinite(b)
true

julia> isinteger(b)
true

julia> c = real(a)
[1.20 +/- 1.01e-3]

julia> d = imag(b)
0

julia> f = accuracy_bits(a)
9

```

### Containment

It is often necessary to determine whether a given exact value or box is
contained in a given complex box or whether two boxes overlap. The following
functions are provided for this purpose.

```@docs
overlaps(::acb, ::acb)
```

```@docs
contains(::acb, ::acb)
```

```@docs
contains(::acb, ::Integer)
contains(::acb, ::ZZRingElem)
contains(::acb, ::QQFieldElem)
```

The following functions are also provided for determining if a box intersects
a certain part of the complex number plane.

```@docs
contains_zero(::acb)
```

**Examples**

```jldoctest
julia> CC = AcbField(64)
Complex Field with 64 bits of precision and error bounds

julia> x = CC("1 +/- 0.001")
[1.00 +/- 1.01e-3]

julia> y = CC("3")
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
```

### Comparison

Nemo provides a full range of comparison operations for Arb complex boxes. 

In addition to the standard comparisons, we introduce an exact equality. This is
distinct from arithmetic equality implemented by `==`, which merely compares up to the
minimum of the precisions of its operands.

```@docs
isequal(::acb, ::acb)
```

A full range of ad hoc comparison operators is provided. These are implemented directly
in Julia, but we document them as though only `==` were provided.

Function                     |
-----------------------------|
`==(x::acb, y::Integer)`     |
`==(x::Integer, y::acb)`     |
`==(x::acb, y::ZZRingElem)`        |
`==(x::ZZRingElem, y::acb)`        |
`==(x::arb, y::ZZRingElem)`        |
`==(x::ZZRingElem, y::arb)`        |
`==(x::acb, y::Float64)`     |
`==(x::Float64, y::acb)`     |

**Examples**

```jldoctest
julia> CC = AcbField(64)
Complex Field with 64 bits of precision and error bounds

julia> x = CC("1 +/- 0.001")
[1.00 +/- 1.01e-3]

julia> y = CC("3")
3.0000000000000000000

julia> z = CC("4")
4.0000000000000000000

julia> isequal(x, deepcopy(x))
true

julia> x == 3
false

julia> ZZ(3) == z
false

julia> x != 1.23
true
```

### Absolute value

**Examples**

```jldoctest
julia> CC = AcbField(64)
Complex Field with 64 bits of precision and error bounds

julia> x = CC("-1 +/- 0.001")
[-1.00 +/- 1.01e-3]

julia> a = abs(x)
[1.00 +/- 1.01e-3]
```

### Shifting

**Examples**

```jldoctest
julia> CC = AcbField(64)
Complex Field with 64 bits of precision and error bounds

julia> x = CC("-3 +/- 0.001")
[-3.00 +/- 1.01e-3]

julia> a = ldexp(x, 23)
[-2.52e+7 +/- 4.26e+4]

julia> b = ldexp(x, -ZZ(15))
[-9.16e-5 +/- 7.78e-8]
```

### Miscellaneous operations

```@docs
trim(::acb)
```

```@docs
unique_integer(::acb)
```

**Examples**

```jldoctest
julia> CC = AcbField(64)
Complex Field with 64 bits of precision and error bounds

julia> x = CC("-3 +/- 0.001", "0.1")
[-3.00 +/- 1.01e-3] + [0.100000000000000000 +/- 1.22e-20]*im

julia> a = trim(x)
[-3.00 +/- 1.01e-3] + [0.100000000000000000 +/- 1.22e-20]*im

julia> b, c = unique_integer(x)
(false, 0)

julia> d = conj(x)
[-3.00 +/- 1.01e-3] + [-0.100000000000000000 +/- 1.22e-20]*im

julia> f = angle(x)
[3.1083 +/- 3.95e-5]
```

### Constants

```@docs
const_pi(::AcbField)
```


**Examples**

```jldoctest
julia> CC = AcbField(200)
Complex Field with 200 bits of precision and error bounds

julia> a = const_pi(CC)
[3.14159265358979323846264338327950288419716939937510582097494 +/- 5.73e-60]
```

### Mathematical and special functions

```@docs
rsqrt(::acb)
```

```@docs
cispi(::acb)
```

```@docs
root_of_unity(::AcbField, k::Int)
```

```@docs
log_sinpi(::acb)
```

```@docs
gamma(::acb)
```

```@docs
lgamma(::acb)
```

```@docs
rgamma(::acb)
```

```@docs
digamma(::acb)
```

```@docs
zeta(::acb)
```

```@docs
barnes_g(::acb)
```

```@docs
log_barnes_g(::acb)
```

```@docs
erf(::acb)
```

```@docs
erfi(::acb)
```

```@docs
exp_integral_ei(::acb)
```

```@docs
sin_integral(::acb)
```

```@docs
cos_integral(::acb)
```

```@docs
sinh_integral(::acb)
```

```@docs
cosh_integral(::acb)
```

```@docs
dedekind_eta(::acb)
```

```@docs
modular_weber_f(::acb)
```

```@docs
modular_weber_f1(::acb)
```

```@docs
modular_weber_f2(::acb)
```

```@docs
j_invariant(::acb)
```

```@docs
modular_lambda(::acb)
```

```@docs
modular_delta(::acb)
```

```@docs
eisenstein_g(::Int, ::acb)
```

```@docs
elliptic_k(::acb)
```

```@docs
elliptic_e(::acb)
```

```@docs
agm(::acb)
agm(::acb, ::acb)
```

```@docs
polygamma(::acb, ::acb)
```

```@docs
zeta(::acb, ::acb)
```

```@docs
rising_factorial(::acb, ::Int)
```

```@docs
rising_factorial2(::acb, ::Int)
```

```@docs
polylog(::Union{acb,Int}, ::acb)
```

```@docs
log_integral(::acb)
```

```@docs
log_integral_offset(::acb)
```

```@docs
exp_integral_e(::acb, ::acb)
```

```@docs
gamma(::acb, ::acb)
```

```@docs
gamma_regularized(::acb, ::acb)
```

```@docs
gamma_lower(::acb, ::acb)
```

```@docs
gamma_lower_regularized(::acb, ::acb)
```

```@docs
airy_ai(::acb)
```

```@docs
airy_ai_prime(::acb)
```

```@docs
airy_bi(::acb)
```

```@docs
airy_bi_prime(::acb)
```

```@docs
bessel_j(::acb, ::acb)
```

```@docs
bessel_y(::acb, ::acb)
```

```@docs
bessel_i(::acb, ::acb)
```

```@docs
bessel_k(::acb, ::acb)
```

```@docs
hypergeometric_1f1(::acb, ::acb, ::acb)
```

```@docs
hypergeometric_1f1_regularized(::acb, ::acb, ::acb)
```

```@docs
hypergeometric_u(::acb, ::acb, ::acb)
```

```@docs
hypergeometric_2f1(::acb, ::acb, ::acb, ::acb)
```

```@docs
jacobi_theta(::acb, ::acb)
```

```@docs
weierstrass_p(::acb, ::acb)
```

**Examples**

```jldoctest
julia> CC = AcbField(64)
Complex Field with 64 bits of precision and error bounds

julia> s = CC(1, 2)
1.0000000000000000000 + 2.0000000000000000000*im

julia> z = CC("1.23", "3.45")
[1.230000000000000000 +/- 2.00e-19] + [3.450000000000000000 +/- 3.91e-19]*im

julia> a = sin(z)^2 + cos(z)^2
[1.000000000000000 +/- 4.92e-16] + [+/- 4.12e-16]*im

julia> b = zeta(z)
[0.685803329024164062 +/- 6.30e-19] + [-0.038574782404586856 +/- 7.54e-19]*im

julia> c = bessel_j(s, z)
[0.63189634741402481 +/- 4.85e-18] + [0.00970090757446076 +/- 4.66e-18]*im

julia> d = hypergeometric_1f1(s, s+1, z)
[-1.3355297330012291 +/- 5.83e-17] + [-0.1715020340928697 +/- 4.97e-17]*im
```

### Linear dependence

```@docs
lindep(::Vector{acb}, n::Int)
```

```@docs
lindep(A::Matrix{acb}, bits::Int)
```

**Examples**

```julia
CC = AcbField(128)

# These are two of the roots of x^5 + 3x + 1
a = CC(1.0050669478588622428791051888364775253, - 0.93725915669289182697903585868761513585)
b = CC(-0.33198902958450931620250069492231652319)

# We recover the polynomial from one root....
V1 = [CC(1), a, a^2, a^3, a^4, a^5];
W = lindep(V1, 20)

# ...or from two
V2 = [CC(1), b, b^2, b^3, b^4, b^5];
Vs = [V1 V2]
X = lindep(Vs, 20)
```
