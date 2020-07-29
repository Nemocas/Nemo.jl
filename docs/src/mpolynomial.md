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

Base ring                                   | Library             | Element type        | Parent type
--------------------------------------------|---------------------|---------------------|----------------------
Generic ring $R$                            | AbstractAlgebra.jl  | `Generic.MPoly{T}`  | `Generic.MPolyRing{T}`
$\mathbb{Z}$                                | Flint               | `fmpz_mpoly`        | `FmpzMPolyRing`
$\mathbb{Z}/n\mathbb{Z}$ (small $n$)        | Flint               | `nmod_mpoly`        | `NmodMPolyRing`
$\mathbb{Q}$                                | Flint               | `fmpq_mpoly`        | `FmpqMPolyRing`

The following are not implemented yet, but will be available soon:

Base ring                                   | Library             | Element type        | Parent type
--------------------------------------------|---------------------|---------------------|----------------------
$\mathbb{Z}/p\mathbb{Z}$ (small prime $p$)  | Flint               | `gfp_mpoly`         | `GFPMPolyRing`
$\mathbb{F}_{p^n}$ (small $p$)              | Flint               | `fq_nmod_mpoly`     | `FqNmodMPolyRing`

The string representation of the variables and the base ring $R$ of a generic
polynomial is stored in its parent object. 

All polynomial element types belong to the abstract type `MPolyElem` and all of
the polynomial ring types belong to the abstract type `MPolyRing`. This enables
one to write generic functions that can accept any Nemo multivariate polynomial type.

## Polynomial functionality

All multivariate polynomial types in Nemo follow the AbstractAlgebra.jl multivariate
polynomial interface:

[https://nemocas.github.io/AbstractAlgebra.jl/latest/mpolynomial_rings](https://nemocas.github.io/AbstractAlgebra.jl/latest/mpolynomial_rings)

Generic multivariate polynomials are also available, and Nemo multivariate polynomial
types also implement all of the same functionality.

[https://nemocas.github.io/AbstractAlgebra.jl/latest/mpolynomial](https://nemocas.github.io/AbstractAlgebra.jl/latest/mpolynomial).

We describe here only functions that are in addition to that guaranteed by
AbstractAlgebra.jl, for specific coefficient rings.
