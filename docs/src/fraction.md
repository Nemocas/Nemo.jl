```@meta
CurrentModule = Nemo
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

```
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

Base ring                             | Library             | Element type        | Parent type
--------------------------------------|---------------------|---------------------|----------------------
Generic ring $R$                      | AbstractAlgebra.jl  | `Generic.Frac{T}`   | `Generic.FracField{T}`
$\mathbb{Z}$                          | Flint               | `fmpq`              | `FlintRationalField`

All fraction element types belong to the abstract type `FracElem` and all of
the fraction field types belong to the abstract type `FracField`. This enables
one to write generic functions that can accept any Nemo fraction type.

## Fraction functionality

All fraction types in Nemo implement the AbstractAlgebra.jl fraction field interface:

<https://nemocas.github.io/AbstractAlgebra.jl/latest/fraction_fields>

In addition, generic fractions fields are implemented in AbstractAlgebra.jl, with the
following functionality:

<https://nemocas.github.io/AbstractAlgebra.jl/latest/fraction>

All fraction types in Nemo also implement this generic functionality.

### Basic manipulation

```@docs
abs(::fmpq)
```

```@docs
sign(::fmpq)
```

```@docs
height(::fmpq)
```

```@docs
height_bits(::fmpq)
```

```@docs
<<(::fmpq, ::Int)
```

```@docs
>>(::fmpq, ::Int)
```

Rational fractions can be compared with each other and with integers. Julia
provides the full range of operators $<, >, \leq, \geq$ which depend on the
following functions.

```@docs
isless(::fmpq, ::fmpq)
isless(::Integer, ::fmpq)
isless(::fmpq, ::Integer)
isless(::fmpq, ::fmpz)
isless(::fmpz, ::fmpq)
```

```@docs
floor(::fmpq)
ceil(::fmpq)
```

**Examples**

```julia
d = abs(ZZ(11)//3)
4 <= ZZ(7)//ZZ(3)
```

### Modular arithmetic

The following functions are available for rationals.

```@docs
mod(a::fmpq, b::fmpz)
```

```@docs
mod(a::fmpq, b::Integer)
```

**Examples**

```julia
a = -fmpz(2)//3
b = fmpz(1)//2

c = mod(a, 7)
d = mod(b, fmpz(5))
```

### Rational Reconstruction

Rational reconstruction is available for rational numbers.

```@docs
reconstruct(::fmpz, ::fmpz)
reconstruct(::fmpz, ::Integer)
reconstruct(::Integer, ::fmpz)
reconstruct(::Integer, ::Integer)
```

**Examples**

```julia
a = reconstruct(7, 13)
b = reconstruct(fmpz(15), 31)
c = reconstruct(fmpz(123), fmpz(237))
```

## Rational enumeration

Various methods exist to enumerate rationals.

```@docs
next_minimal(::fmpq)
```

```@docs
next_signed_minimal(::fmpq)
```

```@docs
next_calkin_wilf(::fmpq)
```

```@docs
next_signed_calkin_wilf(::fmpq)
```

**Examples**

```julia
next_minimal(fmpz(2)//3)
next_signed_minimal(-fmpz(21)//31)
next_calkin_wilf(fmpz(321)//113)
next_signed_calkin_wilf(-fmpz(51)//(17))
```

### Random generation

```@docs
rand_bits(::FlintRationalField, b::Int)
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
dedekind_sum(::fmpz, ::fmpz)
dedekind_sum(::fmpz, ::Integer)
dedekind_sum(::Integer, ::fmpz)
dedekind_sum(::Integer, ::Integer)
```

**Examples**

```julia
a = harmonic(12)

b = dedekind_sum(12, 13)
c = dedekind_sum(-120, fmpz(1305))

d = bernoulli(12)

bernoulli_cache(100)
e = bernoulli(100)
```

```@docs
simplest_between(::fmpq, ::fmpq)
```

**Examples**

```julia
simplest_between(fmpq(1//10), fmpq(3//10))
```

