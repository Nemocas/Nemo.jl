```@meta
CurrentModule = Nemo
```

## Introduction

Nemo allow the creation of dense matricses over any computable ring $R$. There
are two different kinds of implementation: a generic one for the case where no
specific implementation exists, and efficient implementations of matrices over
numerous specific rings, usually provided by C/C++ libraries.

The following table shows each of the matrix types available in Nemo, the
base ring $R$, and the Julia/Nemo types for that kind of matrix (the type
information is mainly of concern to developers).

Base ring                             | Library             | Element type        | Parent type
--------------------------------------|---------------------|---------------------|----------------------
Generic ring $R$                      | Nemo                | `GenMat{T}`         | `GenMatSpace{T}`
$\mathbb{Z}$                          | Flint               | `fmpz_mat`          | `FmpzMatSpace`
$\mathbb{Z}/n\mathbb{Z}$ (small $n$)  | Flint               | `nmod_mat`          | `NmodMatSpace`
$\mathbb{Q}$                          | Flint               | `fmpq_mat`          | `FmpqMatSpace`

The dimensions and base ring $R$ of a generic matrix are stored in its parent
object. 

All matrix element types belong to the abstract type `MatElem` and all of
the matrix space types belong to the abstract type `MatSpace`. This enables
one to write generic functions that can accept any Nemo matrix type.

## Matrix space constructors

In Nemo we have the concept of a matrix space. This is the collection of
matrices with specified dimensions and base ring.

In order to construct matrices in Nemo, one usually first constructs the
matrix space itself. This is accomplished with the following constructor.

```@docs
MatrixSpace(::Ring, ::Int, ::Int, ::Bool)
```

We also allow matrices over a given base ring to be constructed directly.
In such cases, Nemo automatically constructs the matrix space internally.
See the matrix element constructors below for examples. However, note that
there may be a small peformance disadvantage to doing it that way, since
the matrix space needs to be looked up internally every time a matrix is
constructed.

Here are some examples of creating matrix spaces and making use of the
resulting parent objects to coerce various elements into the matrix space.

```
R, t = PolynomialRing(QQ, "t")
S = MatrixSpace(R, 3, 3)

A = S()
B = S(12)
C = S(R(11))
```

## Matrix element constructors

Once a matrix space is constructed, there are various ways to construct
matrices in that space.

In addition to coercing elements into the matrix space as above, we provide the
following functions for constructing certain useful matrices.

```@docs
zero(::MatSpace)
```

```@docs
one(::MatSpace)
```

In addition, there are various shorthand notations for constructing matrices
over a given base ring without first constructing the matrix space parent
object.

```
R[a b c...;...]
```

Create the matrix over the base ring $R$ consisting of the given rows
(separated by semicolons). Each entry is coerced into $R$ 
automatically. Note that parentheses may be placed around individual entries
if the lists would otherwise be ambiguous, e.g. `R[1 2; 2 (-3)]`.

Beware that this syntax does not support the creation of column vectors. See
the notation below for creating those.

```
R[a b c...]
```

Create the row vector with entries in $R$ consisting of the given entries
(separated by spaces). Each entry is coerced into $R$ automatically. Note that
parentheses may be placed around individual entries if the list would otherwise
be ambiguous, e.g. `R[1 2 (-3)]`.

```
R[a b c...]'
```

Create the column vector with entries in $R$ consisting of the given entries
(separated by spaces). Each entry is coerced into $R$ automatically. Observe
the dash that is used to transpose the row vector notation (for free) to turn
it into a column vector. Note that parentheses may be placed around individual
entries if the list would otherwise be ambiguous, e.g. `R[1 2 (-3)]'`.

Here are some examples of constructing matrices.

```
R, t = PolynomialRing(QQ, "t")
S = MatrixSpace(R, 3, 3)

A = zero(S)
B = one(S)

C = R[t + 1 1; t^2 0]
D = R[t + 1 2 t]
F = R[1 2 t]'
```

## Basic functionality

All matric modules in Nemo must provide the functionality listed in this
section. (Note that only some of these functions are useful to a user.)

Developers who are writing their own matrix module, whether as an interface
to a C library, or as some kind of generic module, must provide all of these
functions for custom matrix types in Nemo. 

We write `U` for the type of the matrices in the matrix space and `T` for
the type of elements of the base ring.

All of these functions are provided for all existing matrix types in Nemo.

```
parent_type{U <: MatElem}(::Type{U})
```

Given the type of matrix elements, should return the type of the
corresponding parent object.

```
elem_type(R::MatSpace)
```

Given a parent object for the matrix space, return the type of elements
of the matrix space.

```
Base.hash(a::MatElem, h::UInt)
```

Return a `UInt` hexadecimal hash of the matrix $a$. This should be xor'd
with a fixed random hexadecimal specific to the matrix type. The hash of
each entry should be xor'd with the supplied parameter `h` as part of
computing the hash.

```
deepcopy(a::MatElem)
```

Construct a copy of the given matrix and return it. This function must
recursively construct copies of all of the internal data in the given
matrix. Nemo matricess are mutable and so returning shallow copies is not
sufficient.

To access entries of a Nemo matrix, we overload the square bracket notation.

```
M[r::Int, c::Int]
```

One can both assign to and access a given entry at row $r$ and column $c$ of a
matrix $M$ with this notation. Note that Julia and Nemo matrices are
$1$-indexed, i.e. the first row has index $1$, not $0$, etc. This is in
accordance with many papers on matrices and with systems such as Pari/GP.
 
Given a parent object `S` for a matrix space, the following coercion
functions are provided to coerce various elements into the matrix space.
Developers provide these by overloading the `call` operator for the
matrix parent objects.

```
S()
```

Coerce zero into the space $S$.

```
S(n::Integer)
S(n::fmpz)
```

Return the diagonal matrix with the given integer along the diagonal and zeroes
elsewhere.

```
S(n::T)
```

Coerces an element of the base ring, of type `T` into $S$.

```
S(A::Array{T, 2})
```

Take a Julia two dimensional array of elements in the base ring, of type `T`
and construct the matrix with those entries.

```
S(f::MatElem)
```

Take a matrix that is already in the space $S$ and simply return it. A copy
of the original is not made.

```
S(c::RingElem)
```

Try to coerce the given ring element into the matrix space (as a diagonal
matrix). This only succeeds if $c$ can be coerced into the base ring.

In addition to the above, developers of custom matrices must ensure the
parent object of a matrix type constains a field `base_ring` specifying the
base ring, and fields `rows` and `cols` to specify the dimensions. They must
also ensure that each matrix element contains a field `parent` specifying the
parent object of the matrix, or that there is at least a function
`parent(a::MatElem)` which returns the parent of the given matrix.

Typically a developer will also overload the `MatrixSpace` generic function
to create matrices of the custom type they are implementing.

## Basic manipulation

Numerous functions are provided to manipulate matricess and to set and
retrieve entries and other basic data associated with the matrices.
Also see the section on basic functionality above.

```@docs
base_ring(::MatSpace)
```

```@docs
base_ring(::MatElem)
```

```@docs
parent(::MatElem)
```

```@docs
rows(::MatElem)
```

```@docs
cols(::MatElem)
```

```@docs
iszero(::MatElem)
```

```@docs
isone(::MatElem)
```

Here are some examples of basic manipulation of matrices.

```
R, t = PolynomialRing(QQ, "t")
S = MatrixSpace(R, 3, 3)

A = S([t + 1 t R(1); t^2 t t; R(-2) t + 2 t^2 + t + 1])
B = S([R(2) R(3) R(1); t t + 1 t + 2; R(-1) t^2 t^3])

C = zero(S)
D = one(S)

f = iszero(C)
g = isone(D)
r = rows(B)
c = cols(B)
U = base_ring(C)
V = base_ring(S)
W = parent(D)
```

## Arithmetic operators

All the usual arithmetic operators are overloaded for Nemo matrices. Note
that Julia uses the single slash for floating point division. Therefore to
perform exact division by a constant we use `divexact`. 

```@docs
-(::MatElem)
```

```@docs
+{T <: RingElem}(::MatElem{T}, ::MatElem{T})
```

```@docs
-{T <: RingElem}(::MatElem{T}, ::MatElem{T})
```

```@docs
*{T <: RingElem}(::MatElem{T}, ::MatElem{T})
```

```@docs
divexact{T <: RingElem}(::MatElem{T}, ::MatElem{T})
```

The following ad hoc operators are also provided.

```@docs
+(::Integer, ::MatElem)
+(::MatElem, ::Integer)
+(::fmpz, ::MatElem)
+(::MatElem, ::fmpz)
+{T <: RingElem}(::T, ::MatElem{T})
+{T <: RingElem}(::MatElem{T}, ::T)
```

```@docs
-(::Integer, ::MatElem)
-(::MatElem, ::Integer)
-(::fmpz, ::MatElem)
-(::MatElem, ::fmpz)
-{T <: RingElem}(::T, ::MatElem{T})
-{T <: RingElem}(::MatElem{T}, ::T)
``` 

```@docs
*(::Integer, ::MatElem)
*(::MatElem, ::Integer)
*(::fmpz, ::MatElem)
*(::MatElem, ::fmpz)
*{T <: RingElem}(::T, ::MatElem{T})
*{T <: RingElem}(::MatElem{T}, ::T)
``` 

```@docs
divexact(::MatElem, ::Integer)
divexact(::MatElem, ::fmpz)
divexact{T <: RingElem}(::MatElem{T}, ::T)
```

```@docs
^(::MatElem, ::Int)
```

```@docs
powers{T <: RingElem}(a::MatElem{T}, d::Int)
```

If the appropriate `promote_rule` and coercion exists, these operators can also
be used with elements of other rings. Nemo will try to coerce the operands to
the dominating type and then apply the operator.

Here are some examples of arithmetic operations on matrices.

```
R, t = PolynomialRing(QQ, "t")
S = MatrixSpace(R, 3, 3)

A = S([t + 1 t R(1); t^2 t t; R(-2) t + 2 t^2 + t + 1])
B = S([R(2) R(3) R(1); t t + 1 t + 2; R(-1) t^2 t^3])

C = -A
D = A + B
F = A - B
G = A*B
H = 3*A
K = B + 2
M = fmpz(3) - B
N = t - A
P = A^3
Q = powers(A, 3)
R = divexact(A*3, 3)
```

## Comparison operators

The following comparison operators are implemented for matrices in Nemo.

```@docs
=={T <: RingElem}(::MatElem{T}, ::MatElem{T})
```

```@docs
isequal{T <: RingElem}(::MatElem{T}, ::MatElem{T})
```

In addition we have the following ad hoc comparison operators.

```@docs
=={T <: RingElem}(::MatElem{T}, ::T)
=={T <: RingElem}(::T, ::MatElem{T})
==(::MatElem, ::Integer)
==(::Integer, ::MatElem)
==(::MatElem, ::fmpz)
==(::fmpz, ::MatElem)
```

Here are some examples of comparisons.

```
R, t = PolynomialRing(QQ, "t")
S = MatrixSpace(R, 3, 3)

A = S([t + 1 t R(1); t^2 t t; R(-2) t + 2 t^2 + t + 1])
B = S([R(2) R(3) R(1); t t + 1 t + 2; R(-1) t^2 t^3])

A != B
A == deepcopy(A)
A != 12
fmpz(11) != A
B != t
S(11) == 11
```

## Scaling

```@docs
<<(::fmpz_mat, ::Int)
```

```@docs
>>(::fmpz_mat, ::Int)
```

Here are some examples of scaling matrices.

```
S = MatrixSpace(ZZ, 3, 3)

A = S([fmpz(2) 3 5; 1 4 7; 9 6 3])
 
B = A<<5
C = B>>2
```

## Transpose

```@docs
transpose(::MatElem)
```

Here is an example of transposing a matrix.

```
R, t = PolynomialRing(QQ, "t")
S = MatrixSpace(R, 3, 3)

A = S([t + 1 t R(1); t^2 t t; R(-2) t + 2 t^2 + t + 1])

B = transpose(A)
```

## Gram matrix

```@docs
gram(::MatElem)
```

Here is an example of computing the Gram matrix.

```
R, t = PolynomialRing(QQ, "t")
S = MatrixSpace(R, 3, 3)

A = S([t + 1 t R(1); t^2 t t; R(-2) t + 2 t^2 + t + 1])

B = gram(A)
```
## Trace

```@docs
trace(::MatElem)
```

Here is an example of computing the trace.

```
R, t = PolynomialRing(QQ, "t")
S = MatrixSpace(R, 3, 3)

A = S([t + 1 t R(1); t^2 t t; R(-2) t + 2 t^2 + t + 1])

b = trace(A)
```

## Content

```@docs
content(::MatElem)
```

Here is an example of computing the content of a matrix.

```
R, t = PolynomialRing(QQ, "t")
S = MatrixSpace(R, 3, 3)

A = S([t + 1 t R(1); t^2 t t; R(-2) t + 2 t^2 + t + 1])

b = content(A)
```

## Concatenation

```@docs
hcat(::MatElem, ::MatElem)
```

```@docs
vcat(::MatElem, ::MatElem)
```

Here are some examples of concatenation.

```
R, t = PolynomialRing(QQ, "t")
S = MatrixSpace(R, 3, 3)

A = S([t + 1 t R(1); t^2 t t; R(-2) t + 2 t^2 + t + 1])
B = S([R(2) R(3) R(1); t t + 1 t + 2; R(-1) t^2 t^3])

hcat(A, B)
vcat(A, B)
```

## Permutation

```@docs
*(::perm, ::MatElem)
```

Here is an example of applying a permutation to a matrix.

```
R, t = PolynomialRing(QQ, "t")
S = MatrixSpace(R, 3, 3)
G = FlintPermGroup(3)

A = S([t + 1 t R(1); t^2 t t; R(-2) t + 2 t^2 + t + 1])
P = G([1, 3, 2])

B = P*A
```

## LU factorisation

```@docs
lufact{T <: FieldElem}(::MatElem{T}, ::FlintPermGroup)
```

```@docs
fflu{T <: RingElem}(::MatElem{T}, ::FlintPermGroup)
```

Here are some examples of LU factorisation.

```
R, x = PolynomialRing(QQ, "x")
K, a = NumberField(x^3 + 3x + 1, "a")
S = MatrixSpace(K, 3, 3)
   
A = S([K(0) 2a + 3 a^2 + 1; a^2 - 2 a - 1 2a; a^2 - 2 a - 1 2a])

r, P, L, U = lufact(A)
r, d, P, L, U = fflu(A)
```

## Reduced row-echelon form

```@docs
rref{T <: RingElem}(::MatElem{T})
rref{T <: FieldElem}(::MatElem{T})
```

```@docs
is_rref{T <: RingElem}(::MatElem{T})
is_rref{T <: FieldElem}(::MatElem{T})
```

Here are some examples of computing reduced row echelon form.

```
R, x = PolynomialRing(QQ, "x")
K, a = NumberField(x^3 + 3x + 1, "a")
S = MatrixSpace(K, 3, 3)
   
M = S([K(0) 2a + 3 a^2 + 1; a^2 - 2 a - 1 2a; a^2 + 3a + 1 2a K(1)])
   
r, d, A = rref(M)
is_rref(A)

R, x = PolynomialRing(ZZ, "x")
S = MatrixSpace(R, 3, 3)
U = MatrixSpace(R, 3, 2)

M = S([R(0) 2x + 3 x^2 + 1; x^2 - 2 x - 1 2x; x^2 + 3x + 1 2x R(1)])

r, A = rref(M)
is_rref(A)
```

## Determinant

```@docs
det{T <: RingElem}(::MatElem{T})
det{T <: FieldElem}(::MatElem{T})
```

```@docs
det_divisor(::fmpz_mat)
```

```@docs
det_given_divisor(::fmpz_mat, ::Integer, ::Bool)
det_given_divisor(::fmpz_mat, ::fmpz, ::Bool)
```

Here are some examples of computing the determinant.

```
R, x = PolynomialRing(QQ, "x")
K, a = NumberField(x^3 + 3x + 1, "a")
S = MatrixSpace(K, 3, 3)
   
A = S([K(0) 2a + 3 a^2 + 1; a^2 - 2 a - 1 2a; a^2 + 3a + 1 2a K(1)])

d = det(A)

S = MatrixSpace(ZZ, 3, 3)

A = S([fmpz(2) 3 5; 1 4 7; 9 6 3])
 
c = det_divisor(A)
d = det_given_divisor(A, c)
```

## Rank

```@docs
rank{T <: RingElem}(::MatElem{T})
rank{T <: FieldElem}(::MatElem{T})
```

Here are some examples of computing the rank of a matrix.

```
R, x = PolynomialRing(QQ, "x")
K, a = NumberField(x^3 + 3x + 1, "a")
S = MatrixSpace(K, 3, 3)
   
A = S([K(0) 2a + 3 a^2 + 1; a^2 - 2 a - 1 2a; a^2 + 3a + 1 2a K(1)])

d = rank(A)
```

## Linear solving

```@docs
solve{T <: RingElem}(::MatElem{T}, ::MatElem{T})
```

```@docs
solve_triu{T <: FieldElem}(::MatElem{T}, ::MatElem{T}, ::Bool)
```

```@docs
solve_dixon(::fmpz_mat, ::fmpz_mat)
solve_dixon(::fmpq_mat, ::fmpq_mat)
```

Here are some examples of linear solving.

```
R, x = PolynomialRing(QQ, "x")
K, a = NumberField(x^3 + 3x + 1, "a")
S = MatrixSpace(K, 3, 3)
U = MatrixSpace(K, 3, 1)

A = S([K(0) 2a + 3 a^2 + 1; a^2 - 2 a - 1 2a; a^2 + 3a + 1 2a K(1)])
b = U([2a a + 1 (-a - 1)]')

x = solve(A, b)

A = S([a + 1 2a + 3 a^2 + 1; K(0) a^2 - 1 2a; K(0) K(0) a])
b = U([2a a + 1 (-a - 1)]')

x = solve_triu(A, b, false)

R, x = PolynomialRing(ZZ, "x")
S = MatrixSpace(R, 3, 3)
U = MatrixSpace(R, 3, 2)

A = S([R(0) 2x + 3 x^2 + 1; x^2 - 2 x - 1 2x; x^2 + 3x + 1 2x R(1)])
b = U([2x x + 1 (-x - 1); x + 1 (-x) x^2]')

x, d = solve(A, b)

S = MatrixSpace(ZZ, 3, 3)
T = MatrixSpace(ZZ, 3, 1)

A = S([fmpz(2) 3 5; 1 4 7; 9 2 2])   
B = T([fmpz(4), 5, 7])

X, d = solve(A, B)
X, m = solve_dixon(A, B)
```

## Inverse

```@docs
inv{T <: RingElem}(::MatElem{T})
```

```@docs
inv{T <: FieldElem}(::MatElem{T})
```

```@docs
pseudo_inv(::fmpz_mat)
```

Here are some examples of taking the inverse of a matrix.

```
R, x = PolynomialRing(QQ, "x")
K, a = NumberField(x^3 + 3x + 1, "a")
S = MatrixSpace(K, 3, 3)

A = S([K(0) 2a + 3 a^2 + 1; a^2 - 2 a - 1 2a; a^2 + 3a + 1 2a K(1)])

X = inv(A)

R, x = PolynomialRing(ZZ, "x")
S = MatrixSpace(R, 3, 3)

A = S([R(0) 2x + 3 x^2 + 1; x^2 - 2 x - 1 2x; x^2 + 3x + 1 2x R(1)])
    
X, d = inv(A)

S = MatrixSpace(ZZ, 3, 3)

A = S([1 0 1; 2 3 1; 5 6 7])
  
B, d = pseudo_inv(A)
```

## Nullspace

```@docs
nullspace{T <: RingElem}(::MatElem{T})
nullspace{T <: FieldElem}(::MatElem{T})
```

```@docs
nullspace_right_rational(x::fmpz_mat)
```

Here are some examples of computing the nullspace of a matrix.

```
R, x = PolynomialRing(ZZ, "x")
S = MatrixSpace(R, 4, 4)
   
M = S([-6*x^2+6*x+12 -12*x^2-21*x-15 -15*x^2+21*x+33 -21*x^2-9*x-9;
       -8*x^2+8*x+16 -16*x^2+38*x-20 90*x^2-82*x-44 60*x^2+54*x-34;
       -4*x^2+4*x+8 -8*x^2+13*x-10 35*x^2-31*x-14 22*x^2+21*x-15;
       -10*x^2+10*x+20 -20*x^2+70*x-25 150*x^2-140*x-85 105*x^2+90*x-50])
   
n, N = nullspace(M)
```

## Hessenberg form

```@docs
hessenberg{T <: RingElem}(::MatElem{T})
```

```@docs
is_hessenberg{T <: RingElem}(::MatElem{T})
```

Here are some examples of computing the Hessenberg form.

```
R = ResidueRing(ZZ, 7)
S = MatrixSpace(R, 4, 4)
   
M = S([R(1) R(2) R(4) R(3); R(2) R(5) R(1) R(0);
       R(6) R(1) R(3) R(2); R(1) R(1) R(3) R(5)])
   
A = hessenberg(M)
is_hessenberg(A) == true
```

## Characteristic polynomial

```@docs
charpoly{T <: RingElem}(::Ring, ::MatElem{T})
```

Here are some examples of computing the characteristic polynomial.

```
R = ResidueRing(ZZ, 7)
S = MatrixSpace(R, 4, 4)
T, x = PolynomialRing(R, "x")

M = S([R(1) R(2) R(4) R(3); R(2) R(5) R(1) R(0);
       R(6) R(1) R(3) R(2); R(1) R(1) R(3) R(5)])
   
A = charpoly(T, M)
```

## Minimal polynomial

```@docs
minpoly{T <: RingElem}(::Ring, ::MatElem{T}, ::Bool)
minpoly{T <: FieldElem}(::Ring, ::MatElem{T}, ::Bool)
```

Here are some examples of computing the minimal polynomial of a matrix.

```
R, x = FiniteField(13, 1, "x")
T, y = PolynomialRing(R, "y")
   
M = R[7 6 1;
      7 7 5;
      8 12 5]

A = minpoly(T, M)
```

## Transforms

```@docs
similarity!{T <: RingElem}(::MatElem{T}, ::Int, ::T)
```

Here is an example of applying a similarity transform to a matrix.

```
R = ResidueRing(ZZ, 7)
S = MatrixSpace(R, 4, 4)
   
M = S([R(1) R(2) R(4) R(3); R(2) R(5) R(1) R(0);
       R(6) R(1) R(3) R(2); R(1) R(1) R(3) R(5)])
   
similarity!(M, 1, R(3))
```

## Modular reduction

```@docs
reduce_mod(::fmpz_mat, ::Integer)
reduce_mod(::fmpz_mat, ::fmpz)
```

Here are some examples of modular reduction.

```
S = MatrixSpace(ZZ, 3, 3)

A = S([fmpz(2) 3 5; 1 4 7; 9 2 2])
   
reduce_mod(A, ZZ(5))
reduce_mod(A, 2)
```

## Lifting

```@docs
lift(::nmod_mat)
```

Here are some examples of lifting.

```
R = ResidueRing(ZZ, 7)
S = MatrixSpace(R, 3, 3)

a = S([4 5 6; 7 3 2; 1 4 5])
  
 b = lift(a)
```

## Special matrices

```@docs
hadamard(::FmpzMatSpace)
```

```@docs
is_hadamard(::fmpz_mat)
```

```@docs
hilbert(::FmpqMatSpace)
```

Here are some examples of computing special matrices.

```
R = MatrixSpace(ZZ, 3, 3)
S = MatrixSpace(QQ, 3, 3)

A = hadamard(R)
is_hadamard(A)
B = hilbert(R)
```

## Hermite Normal Form

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
is_hnf(::fmpz_mat)
```

Here are some examples of computing the Hermite Normal Form.

```
S = MatrixSpace(ZZ, 3, 3)

A = S([fmpz(2) 3 5; 1 4 7; 19 3 7])
   
B = hnf(A)
H, T = hnf_with_transform(A)
M = hnf_modular(A, fmpz(27))
N = hnf_modular_eldiv(A, fmpz(27))
is_hnf(M)
```

## Lattice basis reduction

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

Here are some examples of lattice basis reduction.

```
S = MatrixSpace(ZZ, 3, 3)

A = S([fmpz(2) 3 5; 1 4 7; 19 3 7])
   
L = lll(A, lll_ctx(0.95, 0.55, :zbasis, :approx)
L, T = lll_with_transform(A)

G == lll_gram(gram(A))
G, T = lll_gram_with_transform(gram(A))

r, L = lll_with_removal(A, fmpz(100))
r, L, T = lll_with_removal_transform(A, fmpz(100))
```

## Smith Normal Form

```@docs
snf(::fmpz_mat)
```

```@docs
snf_diagonal(::fmpz_mat)
```

```@docs
is_snf(::fmpz_mat)
```

Here are some examples of computing the Smith Normal Form.

```
S = MatrixSpace(ZZ, 3, 3)

A = S([fmpz(2) 3 5; 1 4 7; 19 3 7])
   
B = snf(A)
is_snf(B) == true

B = S([fmpz(2) 0 0; 0 4 0; 0 0 7])

C = snf_diagonal(B)
```

## Strong Echelon Form

```@docs
strong_echelon_form(::nmod_mat)
```

Here is an example of computing the strong echelon form.

```
R = ResidueRing(ZZ, 12)
S = MatrixSpace(R, 3, 3)

A = S([4 1 0; 0 0 5; 0 0 0 ])

B = strong_echelon_form(A)
```

## Howell Form

```@docs
howell_form(::nmod_mat)
```

Here is an example of computing the Howell form.

```
R = ResidueRing(ZZ, 12)
S = MatrixSpace(R, 3, 3)

A = S([4 1 0; 0 0 5; 0 0 0 ])

B = howell_form(A)
```

## Gram-Schmidt Orthogonoalisation

```@docs
gso(::fmpq_mat)
```

Here are some examples of computing the Gram-Schmidt Orthogonalisation.

```
S = MatrixSpace(QQ, 3, 3)

A = S([4 7 3; 2 9 1; 0 5 3])

B = gso(A)
```