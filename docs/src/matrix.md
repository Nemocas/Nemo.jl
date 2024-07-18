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
$\mathbb{F}_{p^n}$ (large $p$)        | Flint               | `FqPolyRepMatrix`   | `FqPolyRepMatrixSpace`
$\mathbb{R}$ (arbitrary precision)    | Arb                 | `RealMatrix`        | `RealMatrixSpace`
$\mathbb{C}$ (arbitrary precision)    | Arb                 | `ComplexMatrix`     | `ComplexMatrixSpace`
$\mathbb{R}$ (fixed precision)        | Arb                 | `ArbMatrix`         | `ArbMatrixSpace`
$\mathbb{C}$ (fixed precision)        | Arb                 | `AcbMatrix`         | `AcbMatrixSpace`

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
overlaps(::RealMatrix, ::RealMatrix)
```

```@docs
overlaps(::ComplexMatrix, ::ComplexMatrix)
```

```@docs
contains(::RealMatrix, ::RealMatrix)
```

```@docs
contains(::ComplexMatrix, ::ComplexMatrix)
```

In addition we have the following ad hoc comparison operators.

**Examples**

```jldoctest; setup = :(RR = RealField())
julia> C = RR[1 2; 3 4]
[1.0000000000000000000   2.0000000000000000000]
[3.0000000000000000000   4.0000000000000000000]

julia> D = RR["1 +/- 0.1" "2 +/- 0.1"; "3 +/- 0.1" "4 +/- 0.1"]
[[1e+0 +/- 0.101]   [2e+0 +/- 0.101]]
[[3e+0 +/- 0.101]   [4e+0 +/- 0.101]]

julia> overlaps(C, D)
true

julia> contains(D, C)
true
```

### Scaling

```@docs
<<(::ZZMatrix, ::Int)
```

```@docs
>>(::ZZMatrix, ::Int)
```

**Examples**

```jldoctest
julia> A = ZZ[2 3 5; 1 4 7; 9 6 3]
[2   3   5]
[1   4   7]
[9   6   3]

julia> B = A<<5
[ 64    96   160]
[ 32   128   224]
[288   192    96]

julia> C = B>>2
[16   24   40]
[ 8   32   56]
[72   48   24]
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

```jldoctest
julia> A = ZZ[2 3 5; 1 4 7; 9 6 3]
[2   3   5]
[1   4   7]
[9   6   3]

julia> c = det_divisor(A)
3

julia> d = det_given_divisor(A, c)
-30
```

### Pseudo inverse

```@docs
pseudo_inv(::ZZMatrix)
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

```jldoctest
julia> A = ZZ[2 3 5; 1 4 7; 9 2 2]
[2   3   5]
[1   4   7]
[9   2   2]

julia> reduce_mod(A, ZZ(5))
[2   3   0]
[1   4   2]
[4   2   2]

julia> reduce_mod(A, 2)
[0   1   1]
[1   0   1]
[1   0   0]
```

### Lifting

```@docs
lift(::zzModMatrix)
lift(::fpMatrix)
```

**Examples**

```jldoctest
julia> R, = residue_ring(ZZ, 7)
(Integers modulo 7, Map: ZZ -> ZZ/(7))

julia> a = R[4 5 6; 7 3 2; 1 4 5]
[4   5   6]
[0   3   2]
[1   4   5]

julia> b = lift(a)
[-3   -2   -1]
[ 0    3    2]
[ 1   -3   -2]
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

```jldoctest
julia> hadamard(matrix_space(ZZ, 3, 3))
ERROR: Unable to create Hadamard matrix
[...]

julia> A = hadamard(matrix_space(ZZ, 4, 4))
[1    1    1    1]
[1   -1    1   -1]
[1    1   -1   -1]
[1   -1   -1    1]

julia> is_hadamard(A)
true

julia> B = hilbert(matrix_space(QQ, 3, 3))
[   1   1//2   1//3]
[1//2   1//3   1//4]
[1//3   1//4   1//5]
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

```jldoctest
julia> A = ZZ[2 3 5; 1 4 7; 19 3 7]
[ 2   3   5]
[ 1   4   7]
[19   3   7]

julia> B = hnf(A)
[1   0   16]
[0   1   18]
[0   0   27]

julia> H, T = hnf_with_transform(A)
([1 0 16; 0 1 18; 0 0 27], [-43 30 3; -44 31 3; -73 51 5])

julia> M = hnf_modular(A, ZZ(27))
[1   0   16]
[0   1   18]
[0   0   27]

julia> N = hnf_modular_eldiv(A, ZZ(27))
[1   0   16]
[0   1   18]
[0   0   27]

julia> is_hnf(M)
true
```

### Lattice basis reduction

Nemo provides LLL lattice basis reduction. Optionally one can specify the setup
using a context object created by the following function.

```
LLLContext(delta::Float64, eta::Float64, rep=:zbasis, gram=:approx)
```

Return a LLL context object specifying LLL parameters $\delta$ and $\eta$ and
specifying the representation as either `:zbasis` or `:gram` and the Gram type
as either `:approx` or `:exact`.

```@docs
lll(::ZZMatrix, ::LLLContext)
```

```@docs
lll_with_transform(::ZZMatrix, ::LLLContext)
```

```@docs
lll_gram(::ZZMatrix, ::LLLContext)
```

```@docs
lll_gram_with_transform(::ZZMatrix, ::LLLContext)
```

```@docs
lll_with_removal(::ZZMatrix, ::ZZRingElem, ::LLLContext)
```

```@docs
lll_with_removal_transform(::ZZMatrix, ::ZZRingElem, ::LLLContext)
```

```@docs
lll!(::ZZMatrix, ::LLLContext)
```

```@docs
lll_gram!(::ZZMatrix, ::LLLContext)
```

**Examples**

```jldoctest
julia> A = ZZ[2 3 5; 1 4 7; 19 3 7]
[ 2   3   5]
[ 1   4   7]
[19   3   7]

julia> L = lll(A, LLLContext(0.95, 0.55, :zbasis, :approx))
[-1    1   2]
[-1   -2   2]
[ 4    1   1]

julia> L, T = lll_with_transform(A)
([-1 1 2; -1 -2 2; 4 1 1], [-1 1 0; -15 10 1; 3 -2 0])

julia> G = lll_gram(gram(A))
[ 6    3   -1]
[ 3    9   -4]
[-1   -4   18]

julia> G, T = lll_gram_with_transform(gram(A))
([6 3 -1; 3 9 -4; -1 -4 18], [-1 1 0; -15 10 1; 3 -2 0])

julia> r, L = lll_with_removal(A, ZZ(100))
(3, [-1 1 2; -1 -2 2; 4 1 1])

julia> r, L, T = lll_with_removal_transform(A, ZZ(100))
(3, [-1 1 2; -1 -2 2; 4 1 1], [-1 1 0; -15 10 1; 3 -2 0])
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

```jldoctest
julia> A = ZZ[2 3 5; 1 4 7; 19 3 7]
[ 2   3   5]
[ 1   4   7]
[19   3   7]

julia> B = snf(A)
[1   0    0]
[0   1    0]
[0   0   27]

julia> is_snf(B) == true
true

julia> B = ZZ[2 0 0; 0 4 0; 0 0 7]
[2   0   0]
[0   4   0]
[0   0   7]

julia> C = snf_diagonal(B)
[1   0    0]
[0   2    0]
[0   0   28]
```

### Strong Echelon Form

```@docs
strong_echelon_form(::zzModMatrix)
strong_echelon_form(::fpMatrix)
```

**Examples**

```jldoctest
julia> R, = residue_ring(ZZ, 12);

julia> A = R[4 1 0; 0 0 5; 0 0 0 ]
[4   1   0]
[0   0   5]
[0   0   0]

julia> B = strong_echelon_form(A)
[4   1   0]
[0   3   0]
[0   0   1]
```

### Howell Form

```@docs
howell_form(::zzModMatrix)
howell_form(::fpMatrix)
```

**Examples**

```jldoctest
julia> R, = residue_ring(ZZ, 12);

julia> A = R[4 1 0; 0 0 5; 0 0 0 ]
[4   1   0]
[0   0   5]
[0   0   0]

julia> B = howell_form(A)
[4   1   0]
[0   3   0]
[0   0   1]
```

### Gram-Schmidt Orthogonalisation

```@docs
gram_schmidt_orthogonalisation(::QQMatrix)
```

### Exponential

**Examples**

```jldoctest; setup = :(RR = RealField())
julia> A = RR[2 0 0; 0 3 0; 0 0 1]
[2.0000000000000000000                       0                       0]
[                    0   3.0000000000000000000                       0]
[                    0                       0   1.0000000000000000000]

julia> B = exp(A)
[[7.389056098930650227 +/- 4.72e-19]                                     0                                     0]
[                                  0   [20.08553692318766774 +/- 1.94e-18]                                     0]
[                                  0                                     0   [2.718281828459045235 +/- 4.30e-19]]
```

### Norm

```@docs
bound_inf_norm(::RealMatrix)
```

```@docs
bound_inf_norm(::ComplexMatrix)
```

**Examples**

```jldoctest; setup = :(RR = RealField())
julia> A = RR[1 2 3; 4 5 6; 7 8 9]
[1.0000000000000000000   2.0000000000000000000   3.0000000000000000000]
[4.0000000000000000000   5.0000000000000000000   6.0000000000000000000]
[7.0000000000000000000   8.0000000000000000000   9.0000000000000000000]

julia> d = bound_inf_norm(A)
[24.000000059604644775 +/- 3.91e-19]
```

### Shifting

**Examples**

```jldoctest; setup = :(RR = RealField())
julia> A = RR[1 2 3; 4 5 6; 7 8 9]
[1.0000000000000000000   2.0000000000000000000   3.0000000000000000000]
[4.0000000000000000000   5.0000000000000000000   6.0000000000000000000]
[7.0000000000000000000   8.0000000000000000000   9.0000000000000000000]

julia> B = ldexp(A, 4)
[16.000000000000000000   32.000000000000000000   48.000000000000000000]
[64.000000000000000000   80.000000000000000000   96.000000000000000000]
[112.00000000000000000   128.00000000000000000   144.00000000000000000]

julia> overlaps(16*A, B)
true
```

### Predicates

**Examples**

```jldoctest; setup = :(CC = ComplexField())
julia> A = CC[1 2 3; 4 5 6; 7 8 9]
[1.0000000000000000000   2.0000000000000000000   3.0000000000000000000]
[4.0000000000000000000   5.0000000000000000000   6.0000000000000000000]
[7.0000000000000000000   8.0000000000000000000   9.0000000000000000000]

julia> isreal(A)
true

julia> isreal(onei(CC)*A)
false
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
eigenvalues(::ComplexMatrix)
eigenvalues_with_multiplicities(::ComplexMatrix)
eigenvalues_simple(a::ComplexMatrix)
```

```jldoctest; setup = :(CC = ComplexField())
julia> A = CC[1 2 3; 0 4 5; 0 0 6]
[1.0000000000000000000   2.0000000000000000000   3.0000000000000000000]
[                    0   4.0000000000000000000   5.0000000000000000000]
[                    0                       0   6.0000000000000000000]

julia> eigenvalues_simple(A)
3-element Vector{ComplexFieldElem}:
 1.0000000000000000000
 4.0000000000000000000
 6.0000000000000000000

julia> A = CC[2 2 3; 0 2 5; 0 0 2]
[2.0000000000000000000   2.0000000000000000000   3.0000000000000000000]
[                    0   2.0000000000000000000   5.0000000000000000000]
[                    0                       0   2.0000000000000000000]

julia> eigenvalues(A)
1-element Vector{ComplexFieldElem}:
 2.0000000000000000000

julia> eigenvalues_with_multiplicities(A)
1-element Vector{Tuple{ComplexFieldElem, Int64}}:
 (2.0000000000000000000, 3)
```
