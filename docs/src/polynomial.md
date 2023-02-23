```@meta
CurrentModule = Nemo
```

# Univariate polynomials

## Introduction

Nemo allow the creation of dense, univariate polynomials over any computable
ring $R$. There are two different kinds of implementation: a generic one for
the case where no specific implementation exists (provided by AbstractAlgebra.jl), and
efficient implementations of polynomials over numerous specific rings, usually provided
by C/C++ libraries.

The following table shows each of the polynomial types available in Nemo, the
base ring $R$, and the Julia/Nemo types for that kind of polynomial (the type
information is mainly of concern to developers).

Base ring                                   | Library             | Element type            | Parent type
--------------------------------------------|---------------------|-------------------------|----------------------
Generic ring $R$                            | AbstractAlgebra.jl  | `Generic.Poly{T}`       | `Generic.PolyRing{T}`
$\mathbb{Z}$                                | Flint               | `ZZPolyRingElem`        | `ZZPolyRing`
$\mathbb{Z}/n\mathbb{Z}$ (small $n$)        | Flint               | `zzModPolyRingElem`     | `zzModPolyRing`
$\mathbb{Z}/n\mathbb{Z}$ (large $n$)        | Flint               | `ZZModPolyRingElem`     | `ZZModPolyRing`
$\mathbb{Q}$                                | Flint               | `QQPolyRingElem`        | `QQPolyRing`
$\mathbb{Z}/p\mathbb{Z}$ (small prime $p$)  | Flint               | `fpPolyRingElem`        | `fpPolyRing`
$\mathbb{Z}/p\mathbb{Z}$ (large prime $p$)  | Flint               | `FpPolyRingElem`        | `FpPolyRing`
$\mathbb{F}_{p^n}$ (small $p$)              | Flint               | `fqPolyRepPolyRingElem` | `fqPolyRepPolyRing`
$\mathbb{F}_{p^n}$ (large $p$)              | Flint               | `FqPolyRepPolyRingElem` | `FqPolyRepPolyRing`
$\mathbb{R}$ (arbitrary precision)          | Arb                 | `RealPoly`              | `RealPolyRing`
$\mathbb{C}$ (arbitrary precision)          | Arb                 | `ComplexPoly`           | `ComplexPolyRing`
$\mathbb{R}$ (fixed precision)              | Arb                 | `arb_poly`              | `ArbPolyRing`
$\mathbb{C}$ (fixed precision)              | Arb                 | `acb_poly`              | `AcbPolyRing`

The string representation of the variable and the base ring $R$ of a generic
polynomial is stored in its parent object. 

All polynomial element types belong to the abstract type `PolyRingElem` and all of
the polynomial ring types belong to the abstract type `PolyRing`. This enables
one to write generic functions that can accept any Nemo univariate polynomial type.

## Polynomial functionality

All univariate polynomial types in Nemo provide the AbstractAlgebra univariate
polynomial functionality:

<https://nemocas.github.io/AbstractAlgebra.jl/stable/polynomial>

Generic polynomials are also available.

We describe here only functions that are in addition to that guaranteed by
AbstractAlgebra.jl, for specific coefficient rings.

### Remove and valuation

```@docs
evaluate2(::RealPoly, ::RealFieldElem)
```

```@docs
evaluate2(::ComplexPoly, ::ComplexFieldElem)
```

**Examples**

```julia
RR = RealField(64)
T, z = polynomial_ring(RR, "z")
   
h = z^2 + 2z + 1

s, t = evaluate2(h, RR("2.0 +/- 0.1"))
```

### Signature

```@docs
signature(::ZZPolyRingElem)
signature(::QQPolyRingElem)
```

**Examples**

```julia
R, x = polynomial_ring(ZZ, "x")

f = x^3 + 3x + 1

(r, s) = signature(f)
```

### Root finding

```@docs
roots(::ComplexPoly)
```

**Examples**

```julia
CC = ComplexField(64)
C, y = polynomial_ring(CC, "y")

m = y^2 + 2y + 3
n = m + CC("0 +/- 0.0001", "0 +/- 0.0001")

r = roots(n)

p = y^7 - 1

r = roots(n, isolate_real = true)
```

### Construction from roots

```@docs
from_roots(::ArbPolyRing, ::Vector{arb})
from_roots(::AcbPolyRing, ::Vector{acb})
```

**Examples**

```julia
RR = RealField(64)
R, x = polynomial_ring(RR, "x")

xs = arb[inv(RR(i)) for i=1:5]
f = from_roots(R, xs)
```

### Bounding absolute values of roots

```@docs
roots_upper_bound(::RealPoly)
roots_upper_bound(::ComplexPoly)
```

### Lifting

When working over a residue ring it is useful to be able to lift to the base
ring of the residue ring, e.g. from $\mathbb{Z}/n\mathbb{Z}$ to $\mathbb{Z}$.

```@docs
lift(::ZZPolyRing, ::zzModPolyRingElem)
lift(::ZZPolyRing, ::fpPolyRingElem)
lift(::ZZPolyRing, ::ZZModPolyRingElem)
lift(::ZZPolyRing, ::FpPolyRingElem)
```

**Examples**

```julia
R = residue_ring(ZZ, 123456789012345678949)
S, x = polynomial_ring(R, "x")
T, y = polynomial_ring(ZZ, "y")

f = x^2 + 2x + 1

a = lift(T, f)
```

### Overlapping and containment

Occasionally it is useful to be able to tell when inexact polynomials overlap
or contain other exact or inexact polynomials. The following functions are
provided for this purpose.

```@docs
overlaps(::RealPoly, ::RealPoly)
overlaps(::ComplexPoly, ::ComplexPoly)
```

```@docs
contains(::RealPoly, ::RealPoly)
contains(::ComplexPoly, ::ComplexPoly)
```

```@docs
contains(::RealPoly, ::ZZPolyRingElem)
contains(::RealPoly, ::QQPolyRingElem)
contains(::ComplexPoly, ::ZZPolyRingElem)
contains(::ComplexPoly, ::QQPolyRingElem)
```

It is sometimes also useful to be able to determine if there is a unique
integer contained in the coefficient of an inexact constant polynomial.

```@docs
unique_integer(::RealPoly)
unique_integer(::ComplexPoly)
```

**Examples**

```julia
RR = RealField(64)
CC = ComplexField(64)
R, x = polynomial_ring(RR, "x")
C, y = polynomial_ring(CC, "y")
Zx, zx = polynomial_ring(ZZ, "x")
Qx, qx = polynomial_ring(QQ, "x")

f = x^2 + 2x + 1
h = f + RR("0 +/- 0.0001")
k = f + RR("0 +/- 0.0001") * x^4
m = y^2 + 2y + 1
n = m + CC("0 +/- 0.0001", "0 +/- 0.0001")

contains(h, f)
overlaps(f, k)
contains(n, m)
t, z = unique_integer(k)
isreal(n)
```
### Factorisation

Certain polynomials can be factored (`ZZPolyRingElem', `zzModPolyRingElem`, `fpPolyRingElem`,
`ZZModPolyRingElem`, `FpPolyRingElem`, `FqPolyRepPolyRingElem`, `fqPolyRepPolyRingElem`) and the interface
follows the specification in AbstractAlgebra.jl. The following additional
functions are available.

```@docs
factor_distinct_deg(::zzModPolyRingElem)
factor_distinct_deg(::fpPolyRingElem)
factor_distinct_deg(::ZZModPolyRingElem)
factor_distinct_deg(::FpPolyRingElem)
factor_distinct_deg(::FqPolyRepPolyRingElem)
factor_distinct_deg(::fqPolyRepPolyRingElem)
```

**Examples**

```
R = residue_ring(ZZ, 23)
S, x = polynomial_ring(R, "x")

f = x^2 + 2x + 1
g = x^3 + 3x + 1

R = factor(f*g)
S = factor_squarefree(f*g)
T = factor_distinct_deg((x + 1)*g*(x^5+x^3+x+1))
```

### Special functions

```@docs
cyclotomic(::Int, ::ZZPolyRingElem)
```

```@docs
swinnerton_dyer(::Int, ::ZZPolyRingElem)
```

```@docs
cos_minpoly(::Int, ::ZZPolyRingElem)
```

```@docs
theta_qexp(::Int, ::Int, ::ZZPolyRingElem)
```

```@docs
eta_qexp(::Int, ::Int, ::ZZPolyRingElem)
```

**Examples**

```julia
R, x = polynomial_ring(ZZ, "x")
S, y = polynomial_ring(R, "y")

h = cyclotomic(120, x)
j = swinnerton_dyer(5, x)
k = cos_minpoly(30, x)
l = theta_qexp(3, 30, x)
m = eta_qexp(24, 30, x)
o = cyclotomic(10, 1 + x + x^2)
```
