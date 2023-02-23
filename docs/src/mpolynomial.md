```@meta
CurrentModule = Nemo
```

# Multivariate polynomials

## Introduction

Nemo allow the creation of sparse, distributed multivariate polynomials over any
computable ring $R$. There are two different kinds of implementation: a generic one for
the case where no specific implementation exists (provided by AbstractAlgebra.jl), and
efficient implementations of polynomials over numerous specific rings, usually provided
by C/C++ libraries.

The following table shows each of the polynomial types available in Nemo, the
base ring $R$, and the Julia/Nemo types for that kind of polynomial (the type
information is mainly of concern to developers).

Base ring                                   | Library             | Element type             | Parent type
--------------------------------------------|---------------------|--------------------------|----------------------
Generic ring $R$                            | AbstractAlgebra.jl  | `Generic.MPoly{T}`       | `Generic.MPolyRing{T}`
$\mathbb{Z}$                                | Flint               | `ZZMPolyRingElem`        | `ZZMPolyRing`
$\mathbb{Z}/n\mathbb{Z}$ (small $n$)        | Flint               | `zzModMPolyRingElem`     | `zzModMPolyRing`
$\mathbb{Q}$                                | Flint               | `QQMPolyRingElem`        | `QQMPolyRing`
$\mathbb{Z}/p\mathbb{Z}$ (small prime $p$)  | Flint               | `fpMPolyRingElem`        | `fpMPolyRing`
$\mathbb{F}_{p^n}$ (small $p$)              | Flint               | `fqPolyRepMPolyRingElem` | `fqPolyRepMPolyRing`

The string representation of the variables and the base ring $R$ of a generic
polynomial is stored in its parent object. 

All polynomial element types belong to the abstract type `MPolyRingElem` and all of
the polynomial ring types belong to the abstract type `MPolyRing`. This enables
one to write generic functions that can accept any Nemo multivariate polynomial type.

## Polynomial functionality

All multivariate polynomial types in Nemo provide the multivariate polynomial
functionality described by AbstractAlgebra:

<https://nemocas.github.io/AbstractAlgebra.jl/stable/mpolynomial>

Generic multivariate polynomials are also available.

We describe here only functions that are in addition to that guaranteed by
AbstractAlgebra.jl, for specific coefficient rings.
