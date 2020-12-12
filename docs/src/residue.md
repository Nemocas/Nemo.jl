```@meta
CurrentModule = Nemo
```

# Residue rings

Nemo allows the creation of residue rings of the form $R/(a)$ for an element
$a$ of a ring $R$.

We don't require $(a)$ to be a prime or maximal ideal. Instead, we allow the
creation of the residue ring $R/(a)$ for any nonzero $a$ and simply raise an
exception if an impossible inverse is encountered during computations 
involving elements of $R/(a)$. Of course, a GCD function must be available for the
base ring $R$.

There is a generic implementation of residue rings of this form in AbstractAlgebra.jl,
which accepts any ring $R$ as base ring.

The associated types of parent object and elements for each kind of residue rings in
Nemo are given in the following table.

Base ring                   | Library            | Element type    | Parent type
----------------------------|--------------------|-----------------|--------------------
Generic ring $R$            | AbstractAlgebra.jl | `Generic.Res{T}`| `Generic.ResRing{T}`
$\mathbb{Z}$ (Int modulus)  | Flint              | `nmod`          | `NmodRing`
$\mathbb{Z}$ (ZZ modulus)   | Flint              | `fmpz_mod`      | `FmpzModRing`

The modulus $a$ of a residue ring is stored in its parent object.

All residue element types belong to the abstract type `ResElem` and all the
residue ring parent object types belong to the abstract type `ResRing`.
This enables one to write generic functions that accept any Nemo residue type.

## Residue functionality

All the residue rings in Nemo implement the residue ring interface of AbstractAlgebra.jl:

<https://nemocas.github.io/AbstractAlgebra.jl/latest/residue_rings>

In addition, functionality for generic residue rings is available:

<https://nemocas.github.io/AbstractAlgebra.jl/latest/residue>

The other residue types in Nemo also implement this functionality.

### GCD

```@docs
gcdx(::nmod, ::nmod)
gcdx(::fmpz_mod, ::fmpz_mod)
```

**Examples**

```julia
R = ResidueRing(ZZ, 123456789012345678949)

g, s, t = gcdx(R(123), R(456))
```
