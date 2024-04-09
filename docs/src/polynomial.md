```@meta
CurrentModule = Nemo
DocTestSetup = quote
    using Nemo
end
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
$\mathbb{R}$ (fixed precision)              | Arb                 | `ArbPolyRingElem`       | `ArbPolyRing`
$\mathbb{C}$ (fixed precision)              | Arb                 | `AcbPolyRingElem`       | `AcbPolyRing`

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

```jldoctest
julia> RR = RealField()
Real field

julia> T, z = polynomial_ring(RR, "z")
(Univariate polynomial ring in z over RR, z)

julia> h = z^2 + 2z + 1
z^2 + 2.0000000000000000000*z + 1

julia> s, t = evaluate2(h, RR("2.0 +/- 0.1"))
([9e+0 +/- 0.611], [6e+0 +/- 0.201])
```

### Signature

```@docs
signature(::ZZPolyRingElem)
signature(::QQPolyRingElem)
```

### Root finding

```@docs
roots(::ComplexPoly)
```

**Examples**

```jldoctest
julia> CC = ComplexField()
Complex field

julia> C, y = polynomial_ring(CC, "y")
(Univariate polynomial ring in y over CC, y)

julia> m = y^2 + 2y + 3
y^2 + 2.0000000000000000000*y + 3.0000000000000000000

julia> n = m + CC("0 +/- 0.0001", "0 +/- 0.0001")
y^2 + 2.0000000000000000000*y + [3.000 +/- 1.01e-4] + [+/- 1.01e-4]*im

julia> r = roots(n);

julia> sort(r; by=x->(real(x), imag(x))) # sort roots to make printing consistent
2-element Vector{ComplexFieldElem}:
 [-1.00 +/- 1.01e-4] + [-1.414 +/- 3.14e-4]*im
 [-1.00 +/- 1.01e-4] + [1.414 +/- 3.14e-4]*im

julia> p = y^7 - 1
y^7 - 1.0000000000000000000

julia> r = roots(n, isolate_real = true);

julia> sort(r; by=x->(real(x), imag(x))) # sort roots to make printing consistent
2-element Vector{ComplexFieldElem}:
 [-1.00 +/- 1.01e-4] + [-1.414 +/- 3.14e-4]*im
 [-1.00 +/- 1.01e-4] + [1.414 +/- 3.14e-4]*im
```

### Construction from roots

```@docs
from_roots(::ArbPolyRing, ::Vector{ArbFieldElem})
from_roots(::AcbPolyRing, ::Vector{AcbFieldElem})
```

**Examples**

```jldoctest
julia> RR = RealField()
Real field

julia> R, x = polynomial_ring(RR, "x")
(Univariate polynomial ring in x over RR, x)

julia> xs = [inv(RR(i)) for i=1:5]
5-element Vector{RealFieldElem}:
 1.0000000000000000000
 0.50000000000000000000
 [0.3333333333333333333 +/- 4.24e-20]
 0.25000000000000000000
 [0.2000000000000000000 +/- 2.44e-20]

julia> f = from_roots(R, xs)
x^5 + [-2.283333333333333333 +/- 4.54e-19]*x^4 + [1.875000000000000000 +/- 5.10e-19]*x^3 + [-0.708333333333333333 +/- 3.99e-19]*x^2 + [0.1250000000000000000 +/- 3.69e-20]*x + [-0.00833333333333333333 +/- 4.13e-21]

julia> all(x -> contains_zero(evaluate(f, x)), xs)
true
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

```jldoctest
julia> R, = residue_ring(ZZ, 123456789012345678949)
(Integers modulo 123456789012345678949, Map: ZZ -> ZZ/(123456789012345678949))

julia> S, x = polynomial_ring(R, "x")
(Univariate polynomial ring in x over ZZ/(123456789012345678949), x)

julia> T, y = polynomial_ring(ZZ, "y")
(Univariate polynomial ring in y over ZZ, y)

julia> f = x^2 + 2x + 1
x^2 + 2*x + 1

julia> a = lift(T, f)
y^2 + 2*y + 1
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

```jldoctest
julia> RR = RealField()
Real field

julia> R, x = polynomial_ring(RR, "x")
(Univariate polynomial ring in x over RR, x)

julia> f = x^2 + 2x + 1
x^2 + 2.0000000000000000000*x + 1

julia> h = f + RR("0 +/- 0.0001")
x^2 + 2.0000000000000000000*x + [1.000 +/- 1.01e-4]

julia> k = f + RR("0 +/- 0.0001") * x^4
[+/- 1.01e-4]*x^4 + x^2 + 2.0000000000000000000*x + 1

julia> contains(h, f)
true

julia> overlaps(f, k)
true

julia> t, z = unique_integer(k)
(true, x^2 + 2*x + 1)
```

```jldoctest
julia> CC = ComplexField()
Complex field

julia> C, y = polynomial_ring(CC, "y")
(Univariate polynomial ring in y over CC, y)

julia> m = y^2 + 2y + 1
y^2 + 2.0000000000000000000*y + 1

julia> n = m + CC("0 +/- 0.0001", "0 +/- 0.0001")
y^2 + 2.0000000000000000000*y + [1.000 +/- 1.01e-4] + [+/- 1.01e-4]*im

julia> contains(n, m)
true

julia> isreal(n)
false

julia> isreal(m)
true
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

```jldoctest
julia> R, = residue_ring(ZZ, 23)
(Integers modulo 23, Map: ZZ -> ZZ/(23))

julia> S, x = polynomial_ring(R, "x")
(Univariate polynomial ring in x over ZZ/(23), x)

julia> f = x^2 + 2x + 1
x^2 + 2*x + 1

julia> g = x^3 + 3x + 1
x^3 + 3*x + 1

julia> R = factor(f*g)
1 * (x + 1)^2 * (x^3 + 3*x + 1)

julia> S = factor_squarefree(f*g)
1 * (x + 1)^2 * (x^3 + 3*x + 1)

julia> T = factor_distinct_deg((x + 1)*g*(x^5+x^3+x+1))
Dict{Int64, zzModPolyRingElem} with 3 entries:
  4 => x^4 + 7*x^3 + 4*x^2 + 5*x + 13
  3 => x^3 + 3*x + 1
  1 => x^2 + 17*x + 16
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

```jldoctest
julia> R, x = polynomial_ring(ZZ, "x")
(Univariate polynomial ring in x over ZZ, x)

julia> h = cyclotomic(120, x)
x^32 + x^28 - x^20 - x^16 - x^12 + x^4 + 1

julia> j = swinnerton_dyer(5, x)
x^32 - 448*x^30 + 84864*x^28 - 9028096*x^26 + 602397952*x^24 - 26625650688*x^22 + 801918722048*x^20 - 16665641517056*x^18 + 239210760462336*x^16 - 2349014746136576*x^14 + 15459151516270592*x^12 - 65892492886671360*x^10 + 172580952324702208*x^8 - 255690851718529024*x^6 + 183876928237731840*x^4 - 44660812492570624*x^2 + 2000989041197056

julia> k = cos_minpoly(30, x)
x^4 + x^3 - 4*x^2 - 4*x + 1

julia> l = theta_qexp(3, 30, x)
72*x^29 + 32*x^27 + 72*x^26 + 30*x^25 + 24*x^24 + 24*x^22 + 48*x^21 + 24*x^20 + 24*x^19 + 36*x^18 + 48*x^17 + 6*x^16 + 48*x^14 + 24*x^13 + 8*x^12 + 24*x^11 + 24*x^10 + 30*x^9 + 12*x^8 + 24*x^6 + 24*x^5 + 6*x^4 + 8*x^3 + 12*x^2 + 6*x + 1

julia> m = eta_qexp(24, 30, x)
-29211840*x^29 + 128406630*x^28 + 24647168*x^27 - 73279080*x^26 + 13865712*x^25 - 25499225*x^24 + 21288960*x^23 + 18643272*x^22 - 12830688*x^21 - 4219488*x^20 - 7109760*x^19 + 10661420*x^18 + 2727432*x^17 - 6905934*x^16 + 987136*x^15 + 1217160*x^14 + 401856*x^13 - 577738*x^12 - 370944*x^11 + 534612*x^10 - 115920*x^9 - 113643*x^8 + 84480*x^7 - 16744*x^6 - 6048*x^5 + 4830*x^4 - 1472*x^3 + 252*x^2 - 24*x + 1

julia> o = cyclotomic(10, 1 + x + x^2)
x^8 + 4*x^7 + 9*x^6 + 13*x^5 + 14*x^4 + 11*x^3 + 6*x^2 + 2*x + 1
```
