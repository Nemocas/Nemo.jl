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

Base ring                                   | Library             | Element type        | Parent type
--------------------------------------------|---------------------|---------------------|----------------------
Generic ring $R$                            | AbstractAlgebra.jl  | `Generic.Poly{T}`   | `Generic.PolyRing{T}`
$\mathbb{Z}$                                | Flint               | `fmpz_poly`         | `FmpzPolyRing`
$\mathbb{Z}/n\mathbb{Z}$ (small $n$)        | Flint               | `nmod_poly`         | `NmodPolyRing`
$\mathbb{Z}/n\mathbb{Z}$ (large $n$)        | Flint               | `fmpz_mod_poly`     | `FmpzModPolyRing`
$\mathbb{Q}$                                | Flint               | `fmpq_poly`         | `FmpqPolyRing`
$\mathbb{Z}/p\mathbb{Z}$ (small prime $p$)  | Flint               | `gfp_poly`          | `GFPPolyRing`
$\mathbb{Z}/p\mathbb{Z}$ (large prime $p$)  | Flint               | `gfp_fmpz_poly`     | `GFPFmpzPolyRing`
$\mathbb{F}_{p^n}$ (small $p$)              | Flint               | `fq_nmod_poly`      | `FqNmodPolyRing`
$\mathbb{F}_{p^n}$ (large $p$)              | Flint               | `fq_poly`           | `FqPolyRing`
$\mathbb{R}$                                | Arb                 | `arb_poly`          | `ArbPolyRing`
$\mathbb{C}$                                | Arb                 | `acb_poly`          | `AcbPolyRing`

The string representation of the variable and the base ring $R$ of a generic
polynomial is stored in its parent object. 

All polynomial element types belong to the abstract type `PolyElem` and all of
the polynomial ring types belong to the abstract type `PolyRing`. This enables
one to write generic functions that can accept any Nemo univariate polynomial type.

## Polynomial functionality

All univariate polynomial types in Nemo provide the AbstractAlgebra univariate
polynomial functionality:

<https://nemocas.github.io/AbstractAlgebra.jl/latest/polynomial>

Generic polynomials are also available.

We describe here only functions that are in addition to that guaranteed by
AbstractAlgebra.jl, for specific coefficient rings.

### Remove and valuation

```@docs
evaluate2(::arb_poly, ::Integer)
evaluate2(::arb_poly, ::Float64)
evaluate2(::arb_poly, ::fmpz)
evaluate2(::arb_poly, ::fmpq)
evaluate2(::arb_poly, ::arb)
evaluate2(::arb_poly, ::acb)
```

```@docs
evaluate2(::acb_poly, ::Integer)
evaluate2(::acb_poly, ::Float64)
evaluate2(::acb_poly, ::fmpz)
evaluate2(::acb_poly, ::fmpq)
evaluate2(::acb_poly, ::arb)
evaluate2(::acb_poly, ::acb)
```

**Examples**

```julia
RR = RealField(64)
T, z = PolynomialRing(RR, "z")
   
h = z^2 + 2z + 1

s, t = evaluate2(h, RR("2.0 +/- 0.1"))
```

### Signature

```@docs
signature(::fmpz_poly)
signature(::fmpq_poly)
```

**Examples**

```julia
R, x = PolynomialRing(ZZ, "x")

f = x^3 + 3x + 1

(r, s) = signature(f)
```

### Root finding

```@docs
roots(::acb_poly)
```

**Examples**

```julia
CC = ComplexField(64)
C, y = PolynomialRing(CC, "y")

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
R, x = PolynomialRing(RR, "x")

xs = arb[inv(RR(i)) for i=1:5]
f = from_roots(R, xs)
```

### Bounding absolute values of roots

```@docs
roots_upper_bound(::arb_poly)
roots_upper_bound(::acb_poly)
```

### Lifting

When working over a residue ring it is useful to be able to lift to the base
ring of the residue ring, e.g. from $\mathbb{Z}/n\mathbb{Z}$ to $\mathbb{Z}$.

```@docs
lift(::FmpzPolyRing, ::nmod_poly)
lift(::FmpzPolyRing, ::gfp_poly)
lift(::FmpzPolyRing, ::fmpz_mod_poly)
lift(::FmpzPolyRing, ::gfp_fmpz_poly)
```

**Examples**

```julia
R = ResidueRing(ZZ, 123456789012345678949)
S, x = PolynomialRing(R, "x")
T, y = PolynomialRing(ZZ, "y")

f = x^2 + 2x + 1

a = lift(T, f)
```

### Overlapping and containment

Occasionally it is useful to be able to tell when inexact polynomials overlap
or contain other exact or inexact polynomials. The following functions are
provided for this purpose.

```@docs
overlaps(::arb_poly, ::arb_poly)
overlaps(::acb_poly, ::acb_poly)
```

```@docs
contains(::arb_poly, ::arb_poly)
contains(::acb_poly, ::acb_poly)
```

```@docs
contains(::arb_poly, ::fmpz_poly)
contains(::arb_poly, ::fmpq_poly)
contains(::acb_poly, ::fmpz_poly)
contains(::acb_poly, ::fmpq_poly)
```

It is sometimes also useful to be able to determine if there is a unique
integer contained in the coefficient of an inexact constant polynomial.

```@docs
unique_integer(::arb_poly)
unique_integer(::acb_poly)
```

We also have the following functions.

```@docs
isreal(::acb_poly)
```

**Examples**

```julia
RR = RealField(64)
CC = ComplexField(64)
R, x = PolynomialRing(RR, "x")
C, y = PolynomialRing(CC, "y")
Zx, zx = PolynomialRing(ZZ, "x")
Qx, qx = PolynomialRing(QQ, "x")

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

Polynomials can be factorised over certain rings. In general we use the
same format for the output as the Julia factorisation function, namely an
associative array with polynomial factors as keys and exponents as values.

```@docs
isirreducible(::nmod_poly)
isirreducible(::gfp_poly)
isirreducible(::fmpz_mod_poly)
isirreducible(::gfp_fmpz_poly)
isirreducible(::fq_poly)
isirreducible(::fq_nmod_poly)
```

```@docs
issquarefree(::nmod_poly)
issquarefree(::gfp_poly)
issquarefree(::fmpz_mod_poly)
issquarefree(::gfp_fmpz_poly)
issquarefree(::fq_poly)
issquarefree(::fq_nmod_poly)
```

```@docs
factor(::fmpz_poly)
factor(::nmod_poly)
factor(::gfp_poly)
factor(::fmpz_mod_poly)
factor(::gfp_fmpz_poly)
factor(::fq_poly)
factor(::fq_nmod_poly)
```

```@docs
factor_squarefree(::nmod_poly)
factor_squarefree(::gfp_poly)
factor_squarefree(::fmpz_mod_poly)
factor_squarefree(::gfp_fmpz_poly)
factor_squarefree(::fq_poly)
factor_squarefree(::fq_nmod_poly)
```

```@docs
factor_distinct_deg(::nmod_poly)
factor_distinct_deg(::gfp_poly)
factor_distinct_deg(::fmpz_mod_poly)
factor_distinct_deg(::gfp_fmpz_poly)
factor_distinct_deg(::fq_poly)
factor_distinct_deg(::fq_nmod_poly)
```

**Examples**

```
R = ResidueRing(ZZ, 23)
S, x = PolynomialRing(R, "x")

f = x^2 + 2x + 1
g = x^3 + 3x + 1

R = factor(f*g)
S = factor_squarefree(f*g)
T = factor_distinct_deg((x + 1)*g*(x^5+x^3+x+1))
```

### Special functions

```@docs
cyclotomic(::Int, ::fmpz_poly)
```

```@docs
swinnerton_dyer(::Int, ::fmpz_poly)
```

```@docs
cos_minpoly(::Int, ::fmpz_poly)
```

```@docs
theta_qexp(::Int, ::Int, ::fmpz_poly)
```

```@docs
eta_qexp(::Int, ::Int, ::fmpz_poly)
```

**Examples**

```julia
R, x = PolynomialRing(ZZ, "x")
S, y = PolynomialRing(R, "y")

h = cyclotomic(120, x)
j = swinnerton_dyer(5, x)
k = cos_minpoly(30, x)
l = theta_qexp(3, 30, x)
m = eta_qexp(24, 30, x)
o = cyclotomic(10, 1 + x + x^2)
```
