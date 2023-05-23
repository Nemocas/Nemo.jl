```@meta
CurrentModule = Nemo
DocTestSetup = quote
    using Nemo
end
```

# Finite fields

A finite field $K$ is represented as simple extension $K = k(\alpha) = k[x]/(f)$, where $k$ can
be
- a prime field $\mathbf{F}_p$ ($K$ is then an *absolute finite field*), or
- an arbitrary finite field $k$ ($K$ is then a *relative finite field*).

In both cases, we call $k$ the *base field* of $K$, $\alpha$ a *generator* and $f$ the *defining polynomial* of $K$.

Note that all field theoretic properties (like basis, degree or trace) are defined with respect to the base field.
Methods with prefix `absolute_` return 

## Finite field functionality

Finite fields in Nemo provide all the field functionality described in AbstractAlgebra:

<https://nemocas.github.io/AbstractAlgebra.jl/stable/field>

Below we describe the functionality that is provided in addition to this.

## Constructors

```@docs
finite_field
GF
```

## Field functionality

```@docs
base_field(::FqField)
prime_field(::FqField)
degree(::FqField)
absolute_degree(::FqField)
is_absolute(::FqField)
defining_polynomial(::FqPolyRing, ::FqField)
```

## Element functionality

```@docs
gen(::FqField)
is_gen(::FqFieldElem)
tr(::FqFieldElem)
absolute_tr(::FqFieldElem)
norm(::FqFieldElem)
absolute_norm(::FqFieldElem)
lift(::FqPolyRing, ::FqFieldElem)
lift(::ZZRing, ::FqFieldElem)
```
