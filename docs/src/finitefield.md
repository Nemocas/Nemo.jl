```@meta
CurrentModule = Nemo
```

# Finite fields

Finite fields are provided in Nemo by Flint. This allows construction of finite
fields of any characteristic and degree for which there are Conway polynomials.
It is also possible for the user to specify their own irreducible polynomial
generating a finite field.

Finite fields are constructed using the `FlintFiniteField` function. However,
for convenience we define

```
FiniteField = FlintFiniteField
```

so that finite fields can be constructed using `FiniteField` rather than
`FlintFiniteField`. Note that this is the name of the constructor, but not of
finite field type.

The types of finite field elements in Nemo are given in the following table,
along with the libraries that provide them and the associated types of the
parent objects.

 Library | Field                          | Element type  | Parent type
---------|--------------------------------|---------------|---------------------
Flint    | $\mathbb{F}_{p^n}$ (small $p$) | `fq_nmod`     | `FqNmodFiniteField`
Flint    | $\mathbb{F}_{p^n}$ (large $p$) | `fq`          | `FqFiniteField`

The only difference between the `fq` and `fq_nmod` types is the representation.
The former is for finite fields with multiprecision characteristic and the
latter is for characteristics that fit into a single unsigned machine word. The
`FlintFiniteField` constructor automatically picks the correct representation
for the user, and so the average user doesn't need to know about the actual
types.

All the finite field types belong to the `FinField` abstract type and the
finite field element types belong to the `FinFieldElem` abstract type.

Since all the functionality for the `fq` finite field type is identical to that
provided for the `fq_nmod` finite field type, we simply document the former.

## Finite field functionality

Finite fields in Nemo provide all the field functionality described in AbstractAlgebra:

<https://nemocas.github.io/AbstractAlgebra.jl/latest/field>

Below we describe the functionality that is provided in addition to this.

### Constructors

In order to construct finite field elements in Nemo, one must first construct
the finite field itself. This is accomplished with one of the following
constructors.

```@docs
FlintFiniteField
```

Here are some examples of creating finite fields and making use of the
resulting parent objects to coerce various elements into those fields.

**Examples**

```julia
R, x = FiniteField(7, 3, "x")
S, y = FiniteField(ZZ(12431351431561), 2, "y")
T, t = PolynomialRing(ResidueRing(ZZ, 12431351431561), "t")
U, z = FiniteField(t^2 + 7, "z")

a = R(5)
b = R(x)
c = S(ZZ(11))
d = U(7)
```

### Basic manipulation

```@docs
gen(::FqFiniteField)
```

```@docs
isgen(::fq)
```

```@docs
coeff(::fq, ::Int)
```

```@docs
degree(::FqFiniteField)
```

```@docs
modulus(::FqFiniteField)
```

**Examples**

```julia
R, x = FiniteField(ZZ(7), 5, "x")

c = gen(R)
d = characteristic(R)
f = order(R)
g = degree(R)
n = isgen(x)
```

### Special functions

Various special functions with finite field specific behaviour are defined.

```@docs
tr(::fq)
```

```@docs
norm(::fq)
```

```@docs
frobenius(::fq, ::Int)
```

```@docs
pth_root(::fq)
```

**Examples**

```julia
R, x = FiniteField(ZZ(7), 5, "x")

a = x^4 + 3x^2 + 6x + 1

b = tr(a)
c = norm(a)
d = frobenius(a)
f = frobenius(a, 3)
g = pth_root(a)
```

### Lift

```@docs
lift(::GFPFmpzPolyRing, ::fq)
```

**Examples**

```julia
R, x = FiniteField(23, 2, "x")
S, y = PolynomialRing(GF(23), "y")

f = 8x + 9

lift(S, f)
```

```
