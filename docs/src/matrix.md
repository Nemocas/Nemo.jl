```@meta
CurrentModule = Nemo
DocTestSetup = quote
    using Nemo
end
```

# Matrices

Nemo allow the creation of dense matrices over any computable ring $R$. There
are two different kinds of implementation: a generic one for the case where no
specific implementation exists (provided by AbstractAlgebra.jl), and efficient
implementations of matrices over numerous specific rings, usually provided by C/C++
libraries.

The following table shows each of the matrix types available in Nemo, the
base ring $R$, and the Julia/Nemo types for that kind of matrix (the type
information is mainly of concern to developers).

Base ring                             | Library             | Element type        | Parent type
--------------------------------------|---------------------|---------------------|----------------------
Generic ring $R$                      | AbstractAlgebra.jl  | `Generic.Mat{T}`    | `Generic.MatSpace{T}`
$\mathbb{Z}$                          | Flint               | `ZZMatrix`          | `ZZMatrixSpace`
$\mathbb{Z}/n\mathbb{Z}$ (small $n$)  | Flint               | `zzModMatrix`       | `zzModMatrixSpace`
$\mathbb{Z}/n\mathbb{Z}$ (large $n$)  | Flint               | `ZZModMatrix`       | `ZZModMatrixSpace`
$\mathbb{Q}$                          | Flint               | `QQMatrix`          | `QQMatrixSpace`
$\mathbb{Z}/p\mathbb{Z}$ (small $p$)  | Flint               | `fpMatrix`          | `fpMatrixSpace`
$\mathbb{F}_{p^n}$ (small $p$)        | Flint               | `fqPolyRepMatrix`   | `fqPolyRepMatrixSpace`
$\mathbb{F}_{p^n}$ (large $p$)        | Flint               | `FqPolyRepMatrix`   | `FqPolyRepMatrixSpace
$\mathbb{R}$ (arbitrary precision)    | Arb                 | `RealMat`           | `RealMatSpace`
$\mathbb{C}$ (arbitrary precision)    | Arb                 | `ComplexMat`        | `ComplexMatSpace`
$\mathbb{R}$ (fixed precision)        | Arb                 | `arb_mat`           | `ArbMatSpace`
$\mathbb{C}$ (fixed precision)        | Arb                 | `acb_mat`           | `AcbMatSpace`

The dimensions and base ring $R$ of a generic matrix are stored in its parent
object.

All matrix element types belong to the abstract type `MatElem` and all of
the matrix space types belong to the abstract type `MatSpace`. This enables
one to write generic functions that can accept any Nemo matrix type.

Note that the preferred way to create matrices is not to use the type
constructors but to use the `matrix` function, see also the
[Matrix element constructors](https://nemocas.github.io/AbstractAlgebra.jl/stable/matrix/#Matrix-element-constructors)
section of the AbstractAlgebra manual.

## Matrix functionality

All matrix spaces in Nemo provide the matrix functionality of AbstractAlgebra:

<https://nemocas.github.io/AbstractAlgebra.jl/stable/matrix>

Some of this functionality is provided in Nemo by C libraries, such as Flint,
for various specific rings.

In the following, we list the functionality which is provided in addition to the generic
matrix functionality, for specific rings in Nemo.

### Comparison operators

```@docs
overlaps(::RealMat, ::RealMat)
```

```@docs
overlaps(::ComplexMat, ::ComplexMat)
```

```@docs
contains(::RealMat, ::RealMat)
```

```@docs
contains(::ComplexMat, ::ComplexMat)
```

In addition we have the following ad hoc comparison operators.

**Examples**

```julia
C = RR[1 2; 3 4]
D = RR["1 +/- 0.1" "2 +/- 0.1"; "3 +/- 0.1" "4 +/- 0.1"]
overlaps(C, D)
contains(D, C)
```

### Scaling

```@docs
<<(::ZZMatrix, ::Int)
```

```@docs
>>(::ZZMatrix, ::Int)
```

**Examples**

```julia
S = matrix_space(ZZ, 3, 3)

A = S([ZZ(2) 3 5; 1 4 7; 9 6 3])

B = A<<5
C = B>>2
```

### Determinant

```@docs
det_divisor(::ZZMatrix)
```

```@docs
det_given_divisor(::ZZMatrix, ::Integer, ::Bool)
det_given_divisor(::ZZMatrix, ::ZZRingElem, ::Bool)
```

**Examples**

```julia
S = matrix_space(ZZ, 3, 3)

A = S([ZZ(2) 3 5; 1 4 7; 9 6 3])

c = det_divisor(A)
d = det_given_divisor(A, c)
```

### Linear solving

```@docs
cansolve(::ZZMatrix, ::ZZMatrix)
```

```@docs
solve_dixon(::ZZMatrix, ::ZZMatrix)
solve_dixon(::QQMatrix, ::QQMatrix)
```

**Examples**

```julia
S = matrix_space(ZZ, 3, 3)
T = matrix_space(ZZ, 3, 1)

A = S([ZZ(2) 3 5; 1 4 7; 9 2 2])
B = T([ZZ(4), 5, 7])

X, m = solve_dixon(A, B)
```

### Pseudo inverse

```@docs
pseudo_inv(::ZZMatrix)
```

**Examples**

```julia
S = matrix_space(ZZ, 3, 3)

A = S([1 0 1; 2 3 1; 5 6 7])

B, d = pseudo_inv(A)
```

### Nullspace

```@docs
nullspace_right_rational(x::ZZMatrix)
```

### Modular reduction

```@docs
reduce_mod(::ZZMatrix, ::Integer)
reduce_mod(::ZZMatrix, ::ZZRingElem)
```

**Examples**

```julia
S = matrix_space(ZZ, 3, 3)

A = S([ZZ(2) 3 5; 1 4 7; 9 2 2])

reduce_mod(A, ZZ(5))
reduce_mod(A, 2)
```

### Lifting

```@docs
lift(::zzModMatrix)
lift(::fpMatrix)
```

**Examples**

```julia
R, = residue_ring(ZZ, 7)
S = matrix_space(R, 3, 3)

a = S([4 5 6; 7 3 2; 1 4 5])

 b = lift(a)
```

### Special matrices

```@docs
hadamard(::ZZMatrixSpace)
```

```@docs
is_hadamard(::ZZMatrix)
```

```@docs
hilbert(::QQMatrixSpace)
```

**Examples**

```julia
R = matrix_space(ZZ, 3, 3)
S = matrix_space(QQ, 3, 3)

A = hadamard(R)
is_hadamard(A)
B = hilbert(R)
```

### Hermite Normal Form

```@docs
hnf(::ZZMatrix)
```

```@docs
hnf_with_transform(::ZZMatrix)
```

```@docs
hnf_modular(::ZZMatrix, ::ZZRingElem)
```

```@docs
hnf_modular_eldiv(::ZZMatrix, ::ZZRingElem)
```

```@docs
is_hnf(::ZZMatrix)
```

**Examples**

```julia
S = matrix_space(ZZ, 3, 3)

A = S([ZZ(2) 3 5; 1 4 7; 19 3 7])

B = hnf(A)
H, T = hnf_with_transform(A)
M = hnf_modular(A, ZZ(27))
N = hnf_modular_eldiv(A, ZZ(27))
is_hnf(M)
```

### Lattice basis reduction

Nemo provides LLL lattice basis reduction. Optionally one can specify the setup
using a context object created by the following function.

```
lll_ctx(delta::Float64, eta::Float64, rep=:zbasis, gram=:approx)
```

Return a LLL context object specifying LLL parameters $\delta$ and $\eta$ and
specifying the representation as either `:zbasis` or `:gram` and the Gram type
as either `:approx` or `:exact`.

```@docs
lll(::ZZMatrix, ::lll_ctx)
```

```@docs
lll_with_transform(::ZZMatrix, ::lll_ctx)
```

```@docs
lll_gram(::ZZMatrix, ::lll_ctx)
```

```@docs
lll_gram_with_transform(::ZZMatrix, ::lll_ctx)
```

```@docs
lll_with_removal(::ZZMatrix, ::ZZRingElem, ::lll_ctx)
```

```@docs
lll_with_removal_transform(::ZZMatrix, ::ZZRingElem, ::lll_ctx)
```

```@docs
lll!(::ZZMatrix, ::lll_ctx)
```

```@docs
lll_gram!(::ZZMatrix, ::lll_ctx)
```

**Examples**

```julia
S = matrix_space(ZZ, 3, 3)

A = S([ZZ(2) 3 5; 1 4 7; 19 3 7])

L = lll(A, lll_ctx(0.95, 0.55, :zbasis, :approx)
L, T = lll_with_transform(A)

G == lll_gram(gram(A))
G, T = lll_gram_with_transform(gram(A))

r, L = lll_with_removal(A, ZZ(100))
r, L, T = lll_with_removal_transform(A, ZZ(100))
```

### Smith Normal Form

```@docs
snf(::ZZMatrix)
```

```@docs
snf_diagonal(::ZZMatrix)
```

```@docs
is_snf(::ZZMatrix)
```

**Examples**

```julia
S = matrix_space(ZZ, 3, 3)

A = S([ZZ(2) 3 5; 1 4 7; 19 3 7])

B = snf(A)
is_snf(B) == true

B = S([ZZ(2) 0 0; 0 4 0; 0 0 7])

C = snf_diagonal(B)
```

### Strong Echelon Form

```@docs
strong_echelon_form(::zzModMatrix)
strong_echelon_form(::fpMatrix)
```

**Examples**

```julia
R, = residue_ring(ZZ, 12)
S = matrix_space(R, 3, 3)

A = S([4 1 0; 0 0 5; 0 0 0 ])

B = strong_echelon_form(A)
```

### Howell Form

```@docs
howell_form(::zzModMatrix)
howell_form(::fpMatrix)
```

**Examples**

```julia
R, = residue_ring(ZZ, 12)
S = matrix_space(R, 3, 3)

A = S([4 1 0; 0 0 5; 0 0 0 ])

B = howell_form(A)
```

### Gram-Schmidt Orthogonalisation

```@docs
gram_schmidt_orthogonalisation(::QQMatrix)
```

### Exponential

**Examples**

```julia
A = RR[2 0 0; 0 3 0; 0 0 1]

B = exp(A)
```

### Norm

```@docs
bound_inf_norm(::RealMat)
```

```@docs
bound_inf_norm(::ComplexMat)
```

**Examples**

```julia
A = RR[1 2 3; 4 5 6; 7 8 9]

d = bound_inf_norm(A)
```

### Shifting

**Examples**

```julia
A = RR[1 2 3; 4 5 6; 7 8 9]

B = ldexp(A, 4)

overlaps(16*A, B)
```

### Predicates

**Examples**

```julia
A = CC[1 2 3; 4 5 6; 7 8 9]

isreal(A)

isreal(onei(CC)*A)
```

### Conversion to Julia matrices

Julia matrices use a different data structure than Nemo matrices. Conversion to Julia matrices is usually only required for interfacing with other packages. It isn't necessary to convert Nemo matrices to Julia matrices in order to manipulate them.

This conversion can be performed with standard Julia syntax, such as the following, where `A` is an `ZZMatrix`:

```julia
Matrix{Int}(A)
Matrix{BigInt}(A)
```

In case the matrix cannot be converted without loss, an `InexactError` is thrown: in this case, cast to a matrix of `BigInt`s rather than `Int`s.

### Eigenvalues and Eigenvectors (experimental)

```@docs
eigenvalues(::ComplexMat)
eigenvalues_with_multiplicities(::ComplexMat)
eigenvalues_simple(a::ComplexMat)
```

```julia
A = CC[1 2 3; 0 4 5; 0 0 6]
eigenvalues_simple(A)
A = CC[2 2 3; 0 2 5; 0 0 2])
eigenvalues(A)
eigenvalues_with_multiplicities(A)
```
