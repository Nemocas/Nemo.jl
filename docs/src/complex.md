```@meta
CurrentModule = Nemo
```

# Arbitrary precision complex balls

Arbitrary precision complex ball arithmetic is supplied by Arb which provides a
ball representation which tracks error bounds rigorously. Complex numbers are 
represented in rectangular form $a+bi$ where $a,b$ are `arb` balls.

The corresponding field is constructed using the `ComplexField` constructor. This
constructs the parent object for the Arb complex field.

The types of complex boxes in Nemo are given in the following table, along with
the libraries that provide them and the associated types of the parent objects.

 Library | Field                | Element type  | Parent type
---------|----------------------|---------------|--------------
Arb      | $\mathbb{C}$ (boxes) | `ComplexElem` | `ComplexField`

All the complex field types belong to the `Field` abstract type and the types of
elements in this field, i.e. complex boxes in this case, belong to the
`FieldElem` abstract type.

## Complex ball functionality

The complex balls in Nemo provide all the field functionality defined by AbstractAlgebra:.

<https://nemocas.github.io/AbstractAlgebra.jl/stable/field>

Below, we document the additional functionality provided for complex balls.

### Precision management

See [Precision management](@ref precision_management).

### Complex field constructors

In order to construct complex boxes in Nemo, one must first construct the Arb
complex field itself. This is accomplished with the following constructor.

```
ComplexField(prec::Int)
```

Here is an example of creating an Arb complex field and using the resulting
parent object to coerce values into the resulting field.

**Examples**

```julia
CC = ComplexField(64)

a = CC("0.25")
b = CC("0.1")
c = CC(0.5)
d = CC(12)
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
onei(::ComplexField)
```

**Examples**

```julia
CC = ComplexField(64)

c = onei(CC)
```

## Basic functionality

The following basic functionality is provided by the default Arb complex field
implementation in Nemo, to support construction of generic rings over complex
fields. Any custom complex field implementation in Nemo should provide analogues
of these functions along with the usual arithmetic operations.

```
parent_type(::Type{ComplexElem})
```

Gives the type of the parent object of an Arb complex field element.

```
elem_type(R::ComplexField)
```

Given the parent object for an Arb complex field, return the type of elements
of the field.

```
mul!(c::ComplexElem, a::ComplexElem, b::ComplexElem)
```

Multiply $a$ by $b$ and set the existing Arb complex field element $c$ to the
result. This function is provided for performance reasons as it saves
allocating a new object for the result and eliminates associated garbage
collection.

```
addeq!(c::ComplexElem, a::ComplexElem)
```

In-place addition adds $a$ to $c$ and sets $c$ to the result. This function
is provided for performance reasons as it saves allocating a new object for
the result and eliminates associated garbage collection.

```
deepcopy(a::ComplexElem)
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
R(f::ComplexElem)
```

Take an Arb complex field element that is already in an Arb field and simply
return it. A copy of the original is not made.

Here are some examples of coercing elements into the Arb complex field.

```
RR = RealField(64)
CC = ComplexField(64)

a = CC(3)
b = CC(QQ(2,3))
c = CC("3 +/- 0.0001")
d = CC("-1.24e+12345")
f = CC("nan +/- inf")
g = CC(RR(3))
```

In addition to the above, developers of custom complex field types must ensure
that they provide the equivalent of the function `base_ring(R::ComplexField)`
which should return `Union{}`. In addition to this they should ensure that
each complex field element contains a field `parent` specifying the parent
object of the complex field element, or at least supply the equivalent of the
function `parent(a::ComplexElem)` to return the parent object of a complex field
element.

### Basic manipulation

```@docs
isfinite(::ComplexElem)
```

```@docs
is_exact(::ComplexElem)
```

```@docs
isinteger(::ComplexElem)
```

```@docs
accuracy_bits(::ComplexElem)
```

**Examples**

```julia
CC = ComplexField(64)

a = CC("1.2 +/- 0.001")
b = CC(3)

isreal(a)
isfinite(b)
isinteger(b)
c = real(a)
d = imag(b)
f = accuracy_bits(a)
```

### Containment

It is often necessary to determine whether a given exact value or box is
contained in a given complex box or whether two boxes overlap. The following
functions are provided for this purpose.

```@docs
overlaps(::ComplexElem, ::ComplexElem)
```

```@docs
contains(::ComplexElem, ::ComplexElem)
```

```@docs
contains(::ComplexElem, ::Integer)
contains(::ComplexElem, ::ZZRingElem)
contains(::ComplexElem, ::QQFieldElem)
```

The following functions are also provided for determining if a box intersects
a certain part of the complex number plane.

```@docs
contains_zero(::ComplexElem)
```

**Examples**

```julia
CC = ComplexField(64)
x = CC("1 +/- 0.001")
y = CC("3")

overlaps(x, y)
contains(x, y)
contains(y, 3)
contains(x, ZZ(1)//2)
contains_zero(x)
```

### Comparison

Nemo provides a full range of comparison operations for Arb complex boxes. 

In addition to the standard comparisons, we introduce an exact equality. This is
distinct from arithmetic equality implemented by `==`, which merely compares up to the
minimum of the precisions of its operands.

```@docs
isequal(::ComplexElem, ::ComplexElem)
```

A full range of ad hoc comparison operators is provided. These are implemented directly
in Julia, but we document them as though only `==` were provided.

Function                     |
-----------------------------|
`==(x::ComplexElem, y::Integer)`     |
`==(x::Integer, y::ComplexElem)`     |
`==(x::ComplexElem, y::ZZRingElem)`        |
`==(x::ZZRingElem, y::ComplexElem)`        |
`==(x::arb, y::ZZRingElem)`        |
`==(x::ZZRingElem, y::arb)`        |
`==(x::ComplexElem, y::Float64)`     |
`==(x::Float64, y::ComplexElem)`     |

**Examples**

```julia
CC = ComplexField(64)
x = CC("1 +/- 0.001")
y = CC("3")
z = CC("4")

isequal(x, deepcopy(x))
x == 3
ZZ(3) == z
x != 1.23
```

### Absolute value

**Examples**

```julia
CC = ComplexField(64)
x = CC("-1 +/- 0.001")

a = abs(x)
```

### Shifting

**Examples**

```julia
CC = ComplexField(64)
x = CC("-3 +/- 0.001")

a = ldexp(x, 23)
b = ldexp(x, -ZZ(15))
```

### Miscellaneous operations

```@docs
trim(::ComplexElem)
```

```@docs
unique_integer(::ComplexElem)
```

**Examples**

```julia
CC = ComplexField(64)
x = CC("-3 +/- 0.001", "0.1")

a = trim(x)
b, c = unique_integer(x)
d = conj(x)
f = angle(x)
```

### Constants

```@docs
const_pi(::ComplexField)
```


**Examples**

```julia
CC = ComplexField(200)

a = const_pi(CC)
```

### Mathematical and special functions

```@docs
rsqrt(::ComplexElem)
```

```@docs
cispi(::ComplexElem)
```

```@docs
root_of_unity(::ComplexField, k::Int)
```

```@docs
log_sinpi(::ComplexElem)
```

```@docs
gamma(::ComplexElem)
```

```@docs
lgamma(::ComplexElem)
```

```@docs
rgamma(::ComplexElem)
```

```@docs
digamma(::ComplexElem)
```

```@docs
zeta(::ComplexElem)
```

```@docs
barnes_g(::ComplexElem)
```

```@docs
log_barnes_g(::ComplexElem)
```

```@docs
erf(::ComplexElem)
```

```@docs
erfi(::ComplexElem)
```

```@docs
exp_integral_ei(::ComplexElem)
```

```@docs
sin_integral(::ComplexElem)
```

```@docs
cos_integral(::ComplexElem)
```

```@docs
sinh_integral(::ComplexElem)
```

```@docs
cosh_integral(::ComplexElem)
```

```@docs
dedekind_eta(::ComplexElem)
```

```@docs
modular_weber_f(::ComplexElem)
```

```@docs
modular_weber_f1(::ComplexElem)
```

```@docs
modular_weber_f2(::ComplexElem)
```

```@docs
j_invariant(::ComplexElem)
```

```@docs
modular_lambda(::ComplexElem)
```

```@docs
modular_delta(::ComplexElem)
```

```@docs
eisenstein_g(::Int, ::ComplexElem)
```

```@docs
hilbert_class_polynomial(::Int, ::ZZPolyRing)
```

```@docs
elliptic_k(::ComplexElem)
```

```@docs
elliptic_e(::ComplexElem)
```

```@docs
agm(::ComplexElem)
agm(::ComplexElem, ::ComplexElem)
```

```@docs
polygamma(::ComplexElem, ::ComplexElem)
```

```@docs
zeta(::ComplexElem, ::ComplexElem)
```

```@docs
rising_factorial(::ComplexElem, ::Int)
```

```@docs
rising_factorial2(::ComplexElem, ::Int)
```

```@docs
polylog(::Union{ComplexElem,Int}, ::ComplexElem)
```

```@docs
log_integral(::ComplexElem)
```

```@docs
log_integral_offset(::ComplexElem)
```

```@docs
exp_integral_e(::ComplexElem, ::ComplexElem)
```

```@docs
gamma(::ComplexElem, ::ComplexElem)
```

```@docs
gamma_regularized(::ComplexElem, ::ComplexElem)
```

```@docs
gamma_lower(::ComplexElem, ::ComplexElem)
```

```@docs
gamma_lower_regularized(::ComplexElem, ::ComplexElem)
```

```@docs
airy_ai(::ComplexElem)
```

```@docs
airy_ai_prime(::ComplexElem)
```

```@docs
airy_bi(::ComplexElem)
```

```@docs
airy_bi_prime(::ComplexElem)
```

```@docs
bessel_j(::ComplexElem, ::ComplexElem)
```

```@docs
bessel_y(::ComplexElem, ::ComplexElem)
```

```@docs
bessel_i(::ComplexElem, ::ComplexElem)
```

```@docs
bessel_k(::ComplexElem, ::ComplexElem)
```

```@docs
hypergeometric_1f1(::ComplexElem, ::ComplexElem, ::ComplexElem)
```

```@docs
hypergeometric_1f1_regularized(::ComplexElem, ::ComplexElem, ::ComplexElem)
```

```@docs
hypergeometric_u(::ComplexElem, ::ComplexElem, ::ComplexElem)
```

```@docs
hypergeometric_2f1(::ComplexElem, ::ComplexElem, ::ComplexElem, ::ComplexElem)
```

```@docs
jacobi_theta(::ComplexElem, ::ComplexElem)
```

```@docs
weierstrass_p(::ComplexElem, ::ComplexElem)
```

**Examples**

```julia
CC = ComplexField(64)

s = CC(1, 2)
z = CC("1.23", "3.45")

a = sin(z)^2 + cos(z)^2
b = zeta(z)
c = bessel_j(s, z)
d = hypergeometric_1f1(s, s+1, z)
```

### Linear dependence

```@docs
lindep(::Vector{ComplexElem}, n::Int)
```

```@docs
lindep(A::Matrix{ComplexElem}, bits::Int)
```

**Examples**

```julia
CC = ComplexField(128)

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

