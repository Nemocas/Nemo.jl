```@meta
CurrentModule = Nemo
DocTestSetup = quote
    using Nemo
end
```

# Finite fields

Finite fields are provided in Nemo by Flint. This allows construction of finite
fields of any characteristic and degree for which there are Conway polynomials.
It is also possible for the user to specify their own irreducible polynomial
generating a finite field.

Finite fields are constructed using the `FlintFiniteField` function. However,
for convenience we define

```
finite_field = FlintFiniteField
```

so that finite fields can be constructed using `finite_field` rather than
`FlintFiniteField`. Note that this is the name of the constructor, but not of
finite field type.

The types of finite field elements in Nemo are given in the following table,
along with the libraries that provide them and the associated types of the
parent objects.

 Library | Field                          | Element type  | Parent type
---------|--------------------------------|---------------|---------------------
Flint    | $\mathbb{F}_{p^n}$ (small $p$) | `fqPolyRepFieldElem`     | `fqPolyRepField`
Flint    | $\mathbb{F}_{p^n}$ (large $p$) | `FqPolyRepFieldElem`          | `FqPolyRepField`

The only difference between the `FqPolyRepFieldElem` and `fqPolyRepFieldElem` types is the representation.
The former is for finite fields with multiprecision characteristic and the
latter is for characteristics that fit into a single unsigned machine word. The
`FlintFiniteField` constructor automatically picks the correct representation
for the user, and so the average user doesn't need to know about the actual
types.

All the finite field types belong to the `FinField` abstract type and the
finite field element types belong to the `FinFieldElem` abstract type.

Since all the functionality for the `FqPolyRepFieldElem` finite field type is identical to that
provided for the `fqPolyRepFieldElem` finite field type, we simply document the former.

## Finite field functionality

Finite fields in Nemo provide all the field functionality described in AbstractAlgebra:

<https://nemocas.github.io/AbstractAlgebra.jl/stable/field>

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

```jldoctest
julia> R, x = finite_field(7, 3, "x")
(Finite field of degree 3 over GF(7), x)

julia> S, y = finite_field(ZZ(12431351431561), 2, "y")
(Finite field of degree 2 over GF(12431351431561), y)

julia> T, t = polynomial_ring(residue_ring(ZZ, 12431351431561), "t")
(Univariate polynomial ring in t over ZZ/(12431351431561), t)

julia> U, z = finite_field(t^2 + 7, "z")
(Finite field of degree 2 over GF(12431351431561), z)

julia> a = R(5)
5

julia> b = R(x)
x

julia> c = S(ZZ(11))
11

julia> d = U(7)
7
```

### Basic manipulation

```@docs
gen(::FqPolyRepField)
```

```@docs
is_gen(::FqPolyRepFieldElem)
```

```@docs
coeff(::FqPolyRepFieldElem, ::Int)
```

```@docs
degree(::FqPolyRepField)
```

```@docs
modulus(::FqPolyRepField)
```

**Examples**

```jldoctest
julia> R, x = finite_field(ZZ(7), 5, "x")
(Finite field of degree 5 over GF(7), x)

julia> c = gen(R)
x

julia> d = characteristic(R)
7

julia> f = order(R)
16807

julia> g = degree(R)
5

julia> n = is_gen(x)
true
```

### Special functions

Various special functions with finite field specific behaviour are defined.

```@docs
tr(::FqPolyRepFieldElem)
```

```@docs
norm(::FqPolyRepFieldElem)
```

```@docs
frobenius(::FqPolyRepFieldElem, ::Int)
```

```@docs
pth_root(::FqPolyRepFieldElem)
```

**Examples**

```jldoctest
julia> R, x = finite_field(ZZ(7), 5, "x")
(Finite field of degree 5 over GF(7), x)

julia> a = x^4 + 3x^2 + 6x + 1
x^4 + 3*x^2 + 6*x + 1

julia> b = tr(a)
1

julia> c = norm(a)
4

julia> d = frobenius(a)
x^4 + 2*x^3 + 3*x^2 + 5*x + 1

julia> f = frobenius(a, 3)
3*x^4 + 3*x^3 + 3*x^2 + x + 4

julia> g = pth_root(a)
4*x^4 + 3*x^3 + 4*x^2 + 5*x + 2
```

### Lift

```@docs
lift(::FpPolyRing, ::FqPolyRepFieldElem)
```

**Examples**

```jldoctest
julia> R, x = finite_field(23, 2, "x")
(Finite field of degree 2 over GF(23), x)

julia> S, y = polynomial_ring(GF(23), "y")
(Univariate polynomial ring in y over GF(23), y)

julia> f = 8x + 9
8*x + 9

julia> lift(S, f)
8*y + 9
```

# Uniform finite fields

An (experimental) uniform finite field interface is provided by the type `FqField`.
Such a finite field can be constructed as an extension of a prime field
$\mathbf{F}_p$ (an absolute extension) or of another finite field (a relative
extension). The field over which the extension is constructed is referred to as the *base field*
and field theoretic properties like the degree of an extension or the trace of an element are understood with respect to the base field.
The corresponding functionality for the implicit absolute extension over the prime field is available
by methods with the prefix `absolute_`.

Note that all finite fields are simple extension $k[t]/(f)$ of their base field $k$.
The irreducible polynomial $f \in k[t]$ is the *defining polynomial* and the class of $t$ is referred
to as the *generator* of the extension.

## Construction of finite fields

```@docs
Nemo._FiniteField
Nemo._GF
```

## Field properties

```@docs
base_field(::FqField)
prime_field(::FqField)
degree(::FqField)
absolute_degree(::FqField)
is_absolute(::FqField)
defining_polynomial(::FqPolyRing, ::FqField)
```

## Element properties

```@docs
tr(::FqFieldElem)
absolute_tr(::FqFieldElem)
norm(::FqFieldElem)
absolute_norm(::FqFieldElem)
lift(::FqPolyRing, ::FqFieldElem)
```
