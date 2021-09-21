```@meta
CurrentModule = Nemo
```

# Complex balls

Arbitrary precision complex ball arithmetic is supplied by Arb which provides a
ball representation which tracks error bounds rigorously. Complex numbers are 
represented in rectangular form $a+bi$ where $a,b$ are `arb` balls.

The Arb complex field is constructed using the `AcbField` constructor. This
constructs the parent object for the Arb complex field.

We define

```
ComplexField = AcbField
```

so that one can construct the Arb complex field parent using `ComplexField`
instead of `AcbField`.

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

<https://nemocas.github.io/AbstractAlgebra.jl/latest/field>

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
will store the values exactly. See the function `isexact` below for more
information.

### Constructors

```@docs
onei(::AcbField)
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
R(f::fmpz)
R(q::fmpq)
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
isexact(::acb)
```

```@docs
isinteger(::acb)
```

```@docs
accuracy_bits(::acb)
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
overlaps(::acb, ::acb)
```

```@docs
contains(::acb, ::acb)
```

```@docs
contains(::acb, ::Integer)
contains(::acb, ::fmpz)
contains(::acb, ::fmpq)
```

The following functions are also provided for determining if a box intersects
a certain part of the complex number plane.

```@docs
contains_zero(::acb)
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
isequal(::acb, ::acb)
```

A full range of ad hoc comparison operators is provided. These are implemented directly
in Julia, but we document them as though only `==` were provided.

Function                     |
-----------------------------|
`==(x::acb, y::Integer)`     |
`==(x::Integer, y::acb)`     |
`==(x::acb, y::fmpz)`        |
`==(x::fmpz, y::acb)`        |
`==(x::arb, y::fmpz)`        |
`==(x::fmpz, y::arb)`        |
`==(x::acb, y::Float64)`     |
`==(x::Float64, y::acb)`     |

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
trim(::acb)
```

```@docs
unique_integer(::acb)
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
const_pi(::AcbField)
```


**Examples**

```julia
CC = ComplexField(200)

a = const_pi(CC)
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
hilbert_class_polynomial(::Int, ::FmpzPolyRing)
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
lindep(::Vector{acb}, n::Int)
```

```@docs
lindep(A::Matrix{acb}, bits::Int)
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

