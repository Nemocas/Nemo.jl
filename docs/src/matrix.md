```@meta
CurrentModule = Nemo
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
$\mathbb{Z}$                          | Flint               | `fmpz_mat`          | `FmpzMatSpace`
$\mathbb{Z}/n\mathbb{Z}$ (small $n$)  | Flint               | `nmod_mat`          | `NmodMatSpace`
$\mathbb{Z}/n\mathbb{Z}$ (large $n$)  | Flint               | `fmpz_mod_mat`      | `FmpzModMatSpace`
$\mathbb{Q}$                          | Flint               | `fmpq_mat`          | `FmpqMatSpace`
$\mathbb{Z}/p\mathbb{Z}$ (small $p$)  | Flint               | `gfp_mat`           | `GFPMatSpace`
$\mathbb{F}_{p^n}$ (small $p$)        | Flint               | `fq_nmod_mat`       | `FqNmodMatSpace`
$\mathbb{F}_{p^n}$ (large $p$)        | Flint               | `fq_mat`            | `FqMatSpace
$\mathbb{R}$                          | Arb                 | `arb_mat`           | `ArbMatSpace`
$\mathbb{C}$                          | Arb                 | `acb_mat`           | `AcbMatSpace`

The dimensions and base ring $R$ of a generic matrix are stored in its parent
object.

All matrix element types belong to the abstract type `MatElem` and all of
the matrix space types belong to the abstract type `MatSpace`. This enables
one to write generic functions that can accept any Nemo matrix type.

Note that the preferred way to create matrices is not to use the type
constructors but to use the `matrix` function, see also the
[Constructors](https://nemocas.github.io/AbstractAlgebra.jl/latest/latest/matrix_spaces/#Constructors-1)
section of the AbstractAlgebra manual.

## Matrix functionality

All matrix spaces in Nemo follow the AbstractAlgebra.jl matrix interface:

<https://nemocas.github.io/AbstractAlgebra.jl/latest/matrix_spaces>

In addition, AbstractAlgebra.jl provides a great deal of generic functionality for
matrices. Some of this functionality is also provided by C libraries, such as Flint,
for various specific rings.

<https://nemocas.github.io/AbstractAlgebra.jl/latest/matrix>

In the following, we list the functionality which is provided in addition to the generic
matrix functionality, for specific rings in Nemo.

### Comparison operators

```@docs
overlaps(::arb_mat, ::arb_mat)
```

```@docs
overlaps(::acb_mat, ::acb_mat)
```

```@docs
contains(::arb_mat, ::arb_mat)
```

```@docs
contains(::acb_mat, ::acb_mat)
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
<<(::fmpz_mat, ::Int)
```

```@docs
>>(::fmpz_mat, ::Int)
```

**Examples**

```julia
S = MatrixSpace(ZZ, 3, 3)

A = S([fmpz(2) 3 5; 1 4 7; 9 6 3])

B = A<<5
C = B>>2
```

### Determinant

```@docs
det_divisor(::fmpz_mat)
```

```@docs
det_given_divisor(::fmpz_mat, ::Integer, ::Bool)
det_given_divisor(::fmpz_mat, ::fmpz, ::Bool)
```

**Examples**

```julia
S = MatrixSpace(ZZ, 3, 3)

A = S([fmpz(2) 3 5; 1 4 7; 9 6 3])

c = det_divisor(A)
d = det_given_divisor(A, c)
```

### Linear solving

```@docs
cansolve(::fmpz_mat, ::fmpz_mat)
```

```@docs
solve_dixon(::fmpz_mat, ::fmpz_mat)
solve_dixon(::fmpq_mat, ::fmpq_mat)
```

**Examples**

```julia
S = MatrixSpace(ZZ, 3, 3)
T = MatrixSpace(ZZ, 3, 1)

A = S([fmpz(2) 3 5; 1 4 7; 9 2 2])
B = T([fmpz(4), 5, 7])

X, m = solve_dixon(A, B)
```

### Pseudo inverse

```@docs
pseudo_inv(::fmpz_mat)
```

**Examples**

```julia
S = MatrixSpace(ZZ, 3, 3)

A = S([1 0 1; 2 3 1; 5 6 7])

B, d = pseudo_inv(A)
```

### Nullspace

```@docs
nullspace_right_rational(x::fmpz_mat)
```

### Modular reduction

```@docs
reduce_mod(::fmpz_mat, ::Integer)
reduce_mod(::fmpz_mat, ::fmpz)
```

**Examples**

```julia
S = MatrixSpace(ZZ, 3, 3)

A = S([fmpz(2) 3 5; 1 4 7; 9 2 2])

reduce_mod(A, ZZ(5))
reduce_mod(A, 2)
```

### Lifting

```@docs
lift(::nmod_mat)
lift(::gfp_mat)
```

**Examples**

```julia
R = ResidueRing(ZZ, 7)
S = MatrixSpace(R, 3, 3)

a = S([4 5 6; 7 3 2; 1 4 5])

 b = lift(a)
```

### Special matrices

```@docs
hadamard(::FmpzMatSpace)
```

```@docs
ishadamard(::fmpz_mat)
```

```@docs
hilbert(::FmpqMatSpace)
```

**Examples**

```julia
R = MatrixSpace(ZZ, 3, 3)
S = MatrixSpace(QQ, 3, 3)

A = hadamard(R)
ishadamard(A)
B = hilbert(R)
```

### Hermite Normal Form

```@docs
hnf(::fmpz_mat)
```

```@docs
hnf_with_transform(::fmpz_mat)
```

```@docs
hnf_modular(::fmpz_mat, ::fmpz)
```

```@docs
hnf_modular_eldiv(::fmpz_mat, ::fmpz)
```

```@docs
ishnf(::fmpz_mat)
```

**Examples**

```julia
S = MatrixSpace(ZZ, 3, 3)

A = S([fmpz(2) 3 5; 1 4 7; 19 3 7])

B = hnf(A)
H, T = hnf_with_transform(A)
M = hnf_modular(A, fmpz(27))
N = hnf_modular_eldiv(A, fmpz(27))
ishnf(M)
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
lll(::fmpz_mat, ::lll_ctx)
```

```@docs
lll_with_transform(::fmpz_mat, ::lll_ctx)
```

```@docs
lll_gram(::fmpz_mat, ::lll_ctx)
```

```@docs
lll_gram_with_transform(::fmpz_mat, ::lll_ctx)
```

```@docs
lll_with_removal(::fmpz_mat, ::fmpz, ::lll_ctx)
```

```@docs
lll_with_removal_transform(::fmpz_mat, ::fmpz, ::lll_ctx)
```

```@docs
lll!(::fmpz_mat, ::lll_ctx)
```

```@docs
lll_gram!(::fmpz_mat, ::lll_ctx)
```

**Examples**

```julia
S = MatrixSpace(ZZ, 3, 3)

A = S([fmpz(2) 3 5; 1 4 7; 19 3 7])

L = lll(A, lll_ctx(0.95, 0.55, :zbasis, :approx)
L, T = lll_with_transform(A)

G == lll_gram(gram(A))
G, T = lll_gram_with_transform(gram(A))

r, L = lll_with_removal(A, fmpz(100))
r, L, T = lll_with_removal_transform(A, fmpz(100))
```

### Smith Normal Form

```@docs
snf(::fmpz_mat)
```

```@docs
snf_diagonal(::fmpz_mat)
```

```@docs
issnf(::fmpz_mat)
```

**Examples**

```julia
S = MatrixSpace(ZZ, 3, 3)

A = S([fmpz(2) 3 5; 1 4 7; 19 3 7])

B = snf(A)
issnf(B) == true

B = S([fmpz(2) 0 0; 0 4 0; 0 0 7])

C = snf_diagonal(B)
```

### Strong Echelon Form

```@docs
strong_echelon_form(::nmod_mat)
strong_echelon_form(::gfp_mat)
```

**Examples**

```julia
R = ResidueRing(ZZ, 12)
S = MatrixSpace(R, 3, 3)

A = S([4 1 0; 0 0 5; 0 0 0 ])

B = strong_echelon_form(A)
```

### Howell Form

```@docs
howell_form(::nmod_mat)
howell_form(::gfp_mat)
```

**Examples**

```julia
R = ResidueRing(ZZ, 12)
S = MatrixSpace(R, 3, 3)

A = S([4 1 0; 0 0 5; 0 0 0 ])

B = howell_form(A)
```

### Gram-Schmidt Orthogonalisation

```@docs
gso(::fmpq_mat)
```

**Examples**

```julia
S = MatrixSpace(QQ, 3, 3)

A = S([4 7 3; 2 9 1; 0 5 3])

B = gso(A)
```

### Exponential

```@docs
Base.exp(::arb_mat)
```

```@docs
Base.exp(::acb_mat)
```

**Examples**

```julia
A = RR[2 0 0; 0 3 0; 0 0 1]

B = exp(A)
```

### Norm

```@docs
bound_inf_norm(::arb_mat)
```

```@docs
bound_inf_norm(::acb_mat)
```

**Examples**

```julia
A = RR[1 2 3; 4 5 6; 7 8 9]

d = bound_inf_norm(A)
```

### Shifting

```@docs
ldexp(::arb_mat, ::Int)
```

```@docs
ldexp(::acb_mat, ::Int)
```

**Examples**

```julia
A = RR[1 2 3; 4 5 6; 7 8 9]

B = ldexp(A, 4)

overlaps(16*A, B)
```

### Predicates

```@docs
isreal(::acb_mat)
```

**Examples**

```julia
A = CC[1 2 3; 4 5 6; 7 8 9]

isreal(A)

isreal(onei(CC)*A)
```

### Conversion to Julia matrices

Julia matrices use a different data structure than Nemo matrices. Conversion to Julia matrices is usually only required for interfacing with other packages. It isn't necessary to convert Nemo matrices to Julia matrices in order to manipulate them.

This conversion can be performed with standard Julia syntax, such as the following, where `A` is an `fmpz_mat`:

```julia
Matrix{Int}(A)
Matrix{BigInt}(A)
```

In case the matrix cannot be converted without loss, an `InexactError` is thrown: in this case, cast to a matrix of `BigInt`s rather than `Int`s.

### Eigenvalues and Eigenvectors (experimental)

```@docs
eigvals(::acb_mat)
eigvals_simple(a::acb_mat)
```

```julia
A = CC[1 2 3; 0 4 5; 0 0 6]
eigvals_simple(A)
A = CC[2 2 3; 0 2 5; 0 0 2])
eigvals(A)
```
