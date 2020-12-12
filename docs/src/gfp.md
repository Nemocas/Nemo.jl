```@meta
CurrentModule = Nemo
```

# Galois fields

Nemo allows the creation of Galois fields of the form $\mathbb{Z}/p\mathbb{Z}$ for a
prime $p$. Note that these are not the same as finite fields of degree 1, as Conway
polynomials are not used and no generator is given.

For convenience, the following constructors are provided.

```julia
GF(n::UInt)
GF(n::Int)
GF(n::fmpz)
```

For example, one can create the Galois field of characteristic $7$ as follows.

```julia
R = GF(7)
```

Elements of the field are then created in the usual way.

```julia
a = R(3)
```

Elements of Galois fields have type `gfp_elem` when $p$ is given to the
constructor as an `Int` or `UInt`, and of type `gfp_fmpz_elem` if $p$ is
given as an `fmpz`, and the type of the parent objects is
`GaloisField` or `GaloisFmpzField` respectively.

The modulus $p$ of an element of a Galois field is stored in its parent object.

The `gfp_elem` and `gfp_fmpz_elem` types belong to the abstract type
`FinFieldElem` and the `GaloisField` and `GaloisFmpzField` parent object types
belong to the abstract type `FinField`.

## Galois field functionality

Galois fields in Nemo implement the residue ring interface of AbstractAlgebra.jl:

<https://nemocas.github.io/AbstractAlgebra.jl/latest/residue_rings>

In addition, all the functionality for generic residue rings is available:

<https://nemocas.github.io/AbstractAlgebra.jl/latest/residue>

Below we describe the functionality that is provided in addition to this interface.

## Basic manipulation

```@docs
characteristic(::GaloisField)
characteristic(::GaloisFmpzField)
```

```@docs
order(::GaloisField)
order(::GaloisFmpzField)
```

**Examples**

```julia
F = GF(3)

a = characteristic(F)
b = order(F)
```
