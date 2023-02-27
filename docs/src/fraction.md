```@meta
CurrentModule = Nemo
DocTestSetup = quote
    using Nemo
end
```

# Fraction fields

Nemo allows the creation of fraction fields over any ring $R$. We don't require
$R$ to be an integral domain, however no attempt is made to deal with the
general case. Two fractions $a/b$ and $c/d$ are equal in Nemo iff $ad = bc$.
Thus, in practice, a greatest common divisor function is currently required for
the ring $R$.

In order to make the representation $a/b$ unique for printing, we have a notion
of canonical unit for elements of a ring $R$. When canonicalising $a/b$, each
of the elements $a$ and $b$ is first divided by the canonical unit of $b$.

The `canonical_unit` function is defined for elements of every Nemo ring. It
must have the properties

```julia
canonical_unit(u) == u
canonical_unit(a*b) == canonical_unit(a)*canonical_unit(b)
```

for any unit $u$ of the ring in question, and $a$ and $b$ arbitrary elements
of the ring.

For example, the canonical unit of an integer is its sign. Thus a fraction of
integers always has positive denominator after canonicalisation.

The canonical unit of a polynomial is the canonical unit of its leading
coefficient, etc.

There are two different kinds of implementation of fraction fields in Nemo: a
generic one for the case where no specific implementation exists (provided by
AbstractAlgebra.jl), and efficient implementations of fractions over specific rings,
usually provided by C/C++ libraries.

The following table shows each of the fraction types available in Nemo, the
base ring $R$, and the Julia/Nemo types for that kind of fraction (the type
information is mainly of concern to developers).

Base ring         | Library             | Element type        | Parent type
------------------|---------------------|---------------------|----------------------
Generic ring $R$  | AbstractAlgebra.jl  | `Generic.Frac{T}`   | `Generic.FracField{T}`
$\mathbb{Z}$      | Flint               | `QQFieldElem`       | `QQField`

All fraction element types belong to the abstract type `FracElem` and all of
the fraction field types belong to the abstract type `FracField`. This enables
one to write generic functions that can accept any Nemo fraction type.

## Fraction functionality

All fraction types in Nemo provide functionality for fields described in
AbstractAlgebra.jl:

<https://nemocas.github.io/AbstractAlgebra.jl/stable/field>

In addition all the fraction field functionality of AbstractAlgebra.jl is provided,
along with generic fractions fields as described here:

<https://nemocas.github.io/AbstractAlgebra.jl/stable/fraction>

### Basic manipulation

```@docs
sign(::QQFieldElem)
```

```@docs
height(::QQFieldElem)
```

```@docs
height_bits(::QQFieldElem)
```

```@docs
<<(::QQFieldElem, ::Int)
```

```@docs
>>(::QQFieldElem, ::Int)
```

```@docs
floor(::QQFieldElem)
ceil(::QQFieldElem)
```

**Examples**

```julia
julia> d = abs(ZZ(11)//3)
11//3

julia> 4 <= ZZ(7)//ZZ(3)
false
```

### Modular arithmetic

The following functions are available for rationals.

```@docs
mod(a::QQFieldElem, b::ZZRingElem)
```

### Rational Reconstruction

Rational reconstruction is available for rational numbers.

```@docs
reconstruct(::ZZRingElem, ::ZZRingElem)
```

## Rational enumeration

Various methods exist to enumerate rationals.

```@docs
next_minimal(::QQFieldElem)
```

```@docs
next_signed_minimal(::QQFieldElem)
```

```@docs
next_calkin_wilf(::QQFieldElem)
```

```@docs
next_signed_calkin_wilf(::QQFieldElem)
```

### Random generation

```@docs
rand_bits(::QQField, b::Int)
```

### Special functions

The following special functions are available for specific rings in Nemo.

```@docs
harmonic(::Int)
```

```@docs
bernoulli(::Int)
```

```@docs
bernoulli_cache(::Int)
```

```@docs
dedekind_sum(::ZZRingElem, ::ZZRingElem)
```

```@docs
simplest_between(::QQFieldElem, ::QQFieldElem)
```
