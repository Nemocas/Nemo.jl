```@meta
CurrentModule = Nemo
DocTestSetup = quote
    using Nemo
end
```

# Galois fields

Nemo allows the creation of Galois fields of the form $\mathbb{Z}/p\mathbb{Z}$ for a
prime $p$. Note that these are not the same as finite fields of degree 1, as Conway
polynomials are not used and no generator is given.

For convenience, the following constructors are provided.

```julia
GF(n::UInt)
GF(n::Int)
GF(n::ZZRingElem)
```

For example, one can create the Galois field of characteristic $7$ as follows.

```julia
R = GF(7)
```

Elements of the field are then created in the usual way.

```julia
a = R(3)
```

Elements of Galois fields have type `fpFieldElem` when $p$ is given to the
constructor as an `Int` or `UInt`, and of type `FpFieldElem` if $p$ is
given as an `ZZRingElem`, and the type of the parent objects is
`fpField` or `FpField` respectively.

The modulus $p$ of an element of a Galois field is stored in its parent object.

The `fpFieldElem` and `FpFieldElem` types belong to the abstract type
`FinFieldElem` and the `fpField` and `FpField` parent object types
belong to the abstract type `FinField`.

## Galois field functionality

Galois fields in Nemo provide all the residue ring functionality of AbstractAlgebra.jl:

<https://nemocas.github.io/AbstractAlgebra.jl/stable/residue>

In addition, all the functionality for rings is available:

<https://nemocas.github.io/AbstractAlgebra.jl/stable/ring>

Below we describe the functionality that is provided in addition to these.

## Basic manipulation

**Examples**

```jldoctest
julia> F = GF(3)
Finite field of characteristic 3

julia> a = characteristic(F)
3

julia> b = order(F)
3
```
