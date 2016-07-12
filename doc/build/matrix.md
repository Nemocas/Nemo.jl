


<a id='Introduction-1'></a>

## Introduction


Nemo allow the creation of dense matricses over any computable ring $R$. There are two different kinds of implementation: a generic one for the case where no specific implementation exists, and efficient implementations of matrices over numerous specific rings, usually provided by C/C++ libraries.


The following table shows each of the matrix types available in Nemo, the base ring $R$, and the Julia/Nemo types for that kind of matrix (the type information is mainly of concern to developers).


                           Base ring | Library | Element type |      Parent type
-----------------------------------: | ------: | -----------: | ---------------:
                    Generic ring $R$ |    Nemo |  `GenMat{T}` | `GenMatSpace{T}`
                        $\mathbb{Z}$ |   Flint |   `fmpz_mat` |   `FmpzMatSpace`
$\mathbb{Z}/n\mathbb{Z}$ (small $n$) |   Flint |   `nmod_mat` |   `NmodMatSpace`
                        $\mathbb{Q}$ |   Flint |   `fmpq_mat` |   `FmpqMatSpace`


The dimensions and base ring $R$ of a generic matrix are stored in its parent object. 


All matrix element types belong to the abstract type `MatElem` and all of the matrix space types belong to the abstract type `MatSpace`. This enables one to write generic functions that can accept any Nemo matrix type.


<a id='Matrix-space-constructors-1'></a>

## Matrix space constructors


In Nemo we have the concept of a matrix space. This is the collection of matrices with specified dimensions and base ring.


In order to construct matrices in Nemo, one usually first constructs the matrix space itself. This is accomplished with the following constructor.

<a id='Nemo.MatrixSpace-Tuple{Nemo.Ring,Int64,Int64,Bool}' href='#Nemo.MatrixSpace-Tuple{Nemo.Ring,Int64,Int64,Bool}'>#</a>
**`Nemo.MatrixSpace`** &mdash; *Method*.



nothing

We also allow matrices over a given base ring to be constructed directly. In such cases, Nemo automatically constructs the matrix space internally. See the matrix element constructors below for examples. However, note that there may be a small peformance disadvantage to doing it that way, since the matrix space needs to be looked up internally every time a matrix is constructed.


Here are some examples of creating matrix spaces and making use of the resulting parent objects to coerce various elements into the matrix space.


```
R, t = PolynomialRing(QQ, "t")
S = MatrixSpace(R, 3, 3)

A = S()
B = S(12)
C = S(R(11))
```


<a id='Matrix-element-constructors-1'></a>

## Matrix element constructors


Once a matrix space is constructed, there are various ways to construct matrices in that space.


In addition to coercing elements into the matrix space as above, we provide the following functions for constructing certain useful matrices.

<a id='Base.zero-Tuple{Nemo.MatSpace{T}}' href='#Base.zero-Tuple{Nemo.MatSpace{T}}'>#</a>
**`Base.zero`** &mdash; *Method*.



```
zero(a::MatSpace)
```

> Construct the zero matrix in the given matrix space.


<a id='Base.one-Tuple{Nemo.MatSpace{T}}' href='#Base.one-Tuple{Nemo.MatSpace{T}}'>#</a>
**`Base.one`** &mdash; *Method*.



```
one(a::MatSpace)
```

> Construct the matrix in the given matrix space with ones down the diagonal and zeroes elsewhere.



In addition, there are various shorthand notations for constructing matrices over a given base ring without first constructing the matrix space parent object.


```
R[a b c...;...]
```


Create the matrix over the base ring $R$ consisting of the given rows (separated by semicolons). Each entry is coerced into $R$  automatically. Note that parentheses may be placed around individual entries if the lists would otherwise be ambiguous, e.g. `R[1 2; 2 (-3)]`.


Beware that this syntax does not support the creation of column vectors. See the notation below for creating those.


```
R[a b c...]
```


Create the row vector with entries in $R$ consisting of the given entries (separated by spaces). Each entry is coerced into $R$ automatically. Note that parentheses may be placed around individual entries if the list would otherwise be ambiguous, e.g. `R[1 2 (-3)]`.


```
R[a b c...]'
```


Create the column vector with entries in $R$ consisting of the given entries (separated by spaces). Each entry is coerced into $R$ automatically. Observe the dash that is used to transpose the row vector notation (for free) to turn it into a column vector. Note that parentheses may be placed around individual entries if the list would otherwise be ambiguous, e.g. `R[1 2 (-3)]'`.


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


<a id='Basic-functionality-1'></a>

## Basic functionality


All matric modules in Nemo must provide the functionality listed in this section. (Note that only some of these functions are useful to a user.)


Developers who are writing their own matrix module, whether as an interface to a C library, or as some kind of generic module, must provide all of these functions for custom matrix types in Nemo. 


We write `U` for the type of the matrices in the matrix space and `T` for the type of elements of the base ring.


All of these functions are provided for all existing matrix types in Nemo.


```
parent_type{U <: MatElem}(::Type{U})
```


Given the type of matrix elements, should return the type of the corresponding parent object.


```
elem_type(R::MatSpace)
```


Given a parent object for the matrix space, return the type of elements of the matrix space.


```
Base.hash(a::MatElem, h::UInt)
```


Return a `UInt` hexadecimal hash of the matrix $a$. This should be xor'd with a fixed random hexadecimal specific to the matrix type. The hash of each entry should be xor'd with the supplied parameter `h` as part of computing the hash.


```
deepcopy(a::MatElem)
```


Construct a copy of the given matrix and return it. This function must recursively construct copies of all of the internal data in the given matrix. Nemo matricess are mutable and so returning shallow copies is not sufficient.


To access entries of a Nemo matrix, we overload the square bracket notation.


```
M[r::Int, c::Int]
```


One can both assign to and access a given entry at row $r$ and column $c$ of a matrix $M$ with this notation. Note that Julia and Nemo matrices are $1$-indexed, i.e. the first row has index $1$, not $0$, etc. This is in accordance with many papers on matrices and with systems such as Pari/GP.


Given a parent object `S` for a matrix space, the following coercion functions are provided to coerce various elements into the matrix space. Developers provide these by overloading the `call` operator for the matrix parent objects.


```
S()
```


Coerce zero into the space $S$.


```
S(n::Integer)
S(n::fmpz)
```


Return the diagonal matrix with the given integer along the diagonal and zeroes elsewhere.


```
S(n::T)
```


Coerces an element of the base ring, of type `T` into $S$.


```
S(A::Array{T, 2})
```


Take a Julia two dimensional array of elements in the base ring, of type `T` and construct the matrix with those entries.


```
S(f::MatElem)
```


Take a matrix that is already in the space $S$ and simply return it. A copy of the original is not made.


```
S(c::RingElem)
```


Try to coerce the given ring element into the matrix space (as a diagonal matrix). This only succeeds if $c$ can be coerced into the base ring.


In addition to the above, developers of custom matrices must ensure the parent object of a matrix type constains a field `base_ring` specifying the base ring, and fields `rows` and `cols` to specify the dimensions. They must also ensure that each matrix element contains a field `parent` specifying the parent object of the matrix, or that there is at least a function `parent(a::MatElem)` which returns the parent of the given matrix.


Typically a developer will also overload the `MatrixSpace` generic function to create matrices of the custom type they are implementing.


<a id='Basic-manipulation-1'></a>

## Basic manipulation


Numerous functions are provided to manipulate matricess and to set and retrieve entries and other basic data associated with the matrices. Also see the section on basic functionality above.

<a id='Nemo.base_ring-Tuple{Nemo.MatSpace{T}}' href='#Nemo.base_ring-Tuple{Nemo.MatSpace{T}}'>#</a>
**`Nemo.base_ring`** &mdash; *Method*.



```
base_ring(a::padic)
```

> Returns `Union{}` as this field is not dependent on another field.


```
base_ring(a::FlintPadicField)
```

> Returns `Union{}` as this field is not dependent on another field.


```
base_ring(x::arb)
```

> Returns `Union{}` since an Arb field does not depend on any other ring.


```
base_ring(R::ArbField)
```

> Returns `Union{}` since an Arb field does not depend on any other ring.


```
base_ring(a::nf_elem)
```

> Returns `Union{}` since a number field doesn't depend on any ring.


```
base_ring(a::AnticNumberField)
```

> Returns `Union{}` since a number field doesn't depend on any ring.


```
base_ring(a::fq)
```

> Returns `Union{}` as this field is not dependent on another field.


```
base_ring(a::FqFiniteField)
```

> Returns `Union{}` as this field is not dependent on another field.


```
base_ring{T}(r::FracElem)
```

> Return the base ring $R$ of the fraction field that the supplied element $a$ belongs to.


```
base_ring{T}(S::FracField{T})
```

> Return the base ring $R$ of the given fraction field.


```
base_ring(r::MatElem)
```

> Return the base ring $R$ of the matrix space that the supplied matrix $r$ belongs to.


```
base_ring{T <: RingElem}(S::MatSpace{T})
```

> Return the base ring $R$ of the given matrix space.


```
base_ring(a::SeriesElem)
```

> Return the base ring of the power series ring of the given power series.


```
base_ring(R::SeriesRing)
```

> Return the base ring of the given power series ring.


```
base_ring(a::PolyElem)
```

> Return the base ring of the polynomial ring of the given polynomial.


```
base_ring(R::PolyRing)
```

> Return the base ring of the given polynomial ring.


```
base_ring(r::ResElem)
```

> Return the base ring $R$ of the residue ring $R/(a)$ that the supplied element $r$ belongs to.


```
base_ring{T <: RingElem}(S::ResRing{T})
```

> Return the base ring $R$ of the given residue ring $S = R/(a)$.


```
base_ring(a::fmpz)
```

> Returns `Union{}` as the parent ring is not dependent on another ring.


```
base_ring(a::FlintIntegerRing)
```

> Returns `Union{}` as this ring is not dependent on another ring.


<a id='Nemo.base_ring-Tuple{Nemo.MatElem{T}}' href='#Nemo.base_ring-Tuple{Nemo.MatElem{T}}'>#</a>
**`Nemo.base_ring`** &mdash; *Method*.



```
base_ring(r::MatElem)
```

> Return the base ring $R$ of the matrix space that the supplied matrix $r$ belongs to.


<a id='Base.parent-Tuple{Nemo.MatElem{T}}' href='#Base.parent-Tuple{Nemo.MatElem{T}}'>#</a>
**`Base.parent`** &mdash; *Method*.



```
parent(a::MatElem)
```

> Return the parent object of the given matrix.


<a id='Nemo.rows-Tuple{Nemo.MatElem{T}}' href='#Nemo.rows-Tuple{Nemo.MatElem{T}}'>#</a>
**`Nemo.rows`** &mdash; *Method*.



```
rows(a::MatElem)
```

> Return the number of rows of the given matrix.


<a id='Nemo.cols-Tuple{Nemo.MatElem{T}}' href='#Nemo.cols-Tuple{Nemo.MatElem{T}}'>#</a>
**`Nemo.cols`** &mdash; *Method*.



```
cols(a::MatElem)
```

> Return the number of columns of the given matrix.


<a id='Nemo.iszero-Tuple{Nemo.MatElem{T}}' href='#Nemo.iszero-Tuple{Nemo.MatElem{T}}'>#</a>
**`Nemo.iszero`** &mdash; *Method*.



```
iszero(a::MatElem)
```

> Return `true` if the supplied matrix $a$ is the zero matrix, otherwise return `false`.


<a id='Nemo.isone-Tuple{Nemo.MatElem{T}}' href='#Nemo.isone-Tuple{Nemo.MatElem{T}}'>#</a>
**`Nemo.isone`** &mdash; *Method*.



```
isone(a::MatElem)
```

> Return `true` if the supplied matrix $a$ is diagonal with ones along the diagonal, otherwise return `false`.



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


<a id='Arithmetic-operators-1'></a>

## Arithmetic operators


All the usual arithmetic operators are overloaded for Nemo matrices. Note that Julia uses the single slash for floating point division. Therefore to perform exact division by a constant we use `divexact`. 

<a id='Base.--Tuple{Nemo.MatElem{T}}' href='#Base.--Tuple{Nemo.MatElem{T}}'>#</a>
**`Base.-`** &mdash; *Method*.



```
-(a::MatElem)
```

> Return $-a$.


```
-(x)
```

Unary minus operator.

<a id='Base.+-Tuple{Nemo.MatElem{T<:Nemo.RingElem},Nemo.MatElem{T<:Nemo.RingElem}}' href='#Base.+-Tuple{Nemo.MatElem{T<:Nemo.RingElem},Nemo.MatElem{T<:Nemo.RingElem}}'>#</a>
**`Base.+`** &mdash; *Method*.



```
+{T <: RingElem}(a::MatElem{T}, b::MatElem{T})
```

> Return $a + b$.


```
+{T <: RingElem}(x::T, y::MatElem{T})
```

> Return $S(x) + y$ where $S$ is the parent of $y$.


```
+{T <: RingElem}(x::MatElem{T}, y::T)
```

> Return $x + S(y)$ where $S$ is the parent of $x$.


<a id='Base.--Tuple{Nemo.MatElem{T<:Nemo.RingElem},Nemo.MatElem{T<:Nemo.RingElem}}' href='#Base.--Tuple{Nemo.MatElem{T<:Nemo.RingElem},Nemo.MatElem{T<:Nemo.RingElem}}'>#</a>
**`Base.-`** &mdash; *Method*.



```
-{T <: RingElem}(a::MatElem{T}, b::MatElem{T})
```

> Return $a - b$.


```
-{T <: RingElem}(x::T, y::MatElem{T})
```

> Return $S(x) - y$ where $S$ is the parent of $y$.


```
-{T <: RingElem}(x::MatElem{T}, y::T)
```

> Return $x - S(y)$, where $S$ is the parent of $a$.


```
-(x, y)
```

Subtraction operator.

<a id='Base.*-Tuple{Nemo.MatElem{T<:Nemo.RingElem},Nemo.MatElem{T<:Nemo.RingElem}}' href='#Base.*-Tuple{Nemo.MatElem{T<:Nemo.RingElem},Nemo.MatElem{T<:Nemo.RingElem}}'>#</a>
**`Base.*`** &mdash; *Method*.



```
*{T <: RingElem}(a::MatElem{T}, b::MatElem{T})
```

> Return $a\times b$.


```
*{T <: RingElem}(x::T, y::MatElem{T})
```

> Return $x\times y$.


```
*{T <: RingElem}(x::MatElem{T}, y::T)
```

> Return $x\times y$.


```
*(x, y...)
```

Multiplication operator. `x*y*z*...` calls this function with all arguments, i.e. `*(x, y, z, ...)`.

<a id='Nemo.divexact-Tuple{Nemo.MatElem{T<:Nemo.RingElem},Nemo.MatElem{T<:Nemo.RingElem}}' href='#Nemo.divexact-Tuple{Nemo.MatElem{T<:Nemo.RingElem},Nemo.MatElem{T<:Nemo.RingElem}}'>#</a>
**`Nemo.divexact`** &mdash; *Method*.



```
divexact{T <: RingElem}(x::MatElem{T}, y::T)
```

> Return $x/y$, i.e. the matrix where each of the entries has been divided by $y$. Each division is expected to be exact.



The following ad hoc operators are also provided.

<a id='Base.+-Tuple{Integer,Nemo.MatElem{T}}' href='#Base.+-Tuple{Integer,Nemo.MatElem{T}}'>#</a>
**`Base.+`** &mdash; *Method*.



```
+(x::Integer, y::MatElem)
```

> Return $S(x) + y$ where $S$ is the parent of $y$.


<a id='Base.+-Tuple{Nemo.MatElem{T},Integer}' href='#Base.+-Tuple{Nemo.MatElem{T},Integer}'>#</a>
**`Base.+`** &mdash; *Method*.



```
+(x::MatElem, y::Integer)
```

> Return $x + S(y)$ where $S$ is the parent of $x$.


<a id='Base.+-Tuple{Nemo.fmpz,Nemo.MatElem{T}}' href='#Base.+-Tuple{Nemo.fmpz,Nemo.MatElem{T}}'>#</a>
**`Base.+`** &mdash; *Method*.



```
+(x::fmpz, y::MatElem)
```

> Return $S(x) + y$ where $S$ is the parent of $y$.


<a id='Base.+-Tuple{Nemo.MatElem{T},Nemo.fmpz}' href='#Base.+-Tuple{Nemo.MatElem{T},Nemo.fmpz}'>#</a>
**`Base.+`** &mdash; *Method*.



```
+(x::MatElem, y::fmpz)
```

> Return $x + S(y)$ where $S$ is the parent of $x$.


<a id='Base.+-Tuple{T<:Nemo.RingElem,Nemo.MatElem{T<:Nemo.RingElem}}' href='#Base.+-Tuple{T<:Nemo.RingElem,Nemo.MatElem{T<:Nemo.RingElem}}'>#</a>
**`Base.+`** &mdash; *Method*.



```
+{T <: RingElem}(x::T, y::MatElem{T})
```

> Return $S(x) + y$ where $S$ is the parent of $y$.


<a id='Base.+-Tuple{Nemo.MatElem{T<:Nemo.RingElem},T<:Nemo.RingElem}' href='#Base.+-Tuple{Nemo.MatElem{T<:Nemo.RingElem},T<:Nemo.RingElem}'>#</a>
**`Base.+`** &mdash; *Method*.



```
+{T <: RingElem}(x::MatElem{T}, y::T)
```

> Return $x + S(y)$ where $S$ is the parent of $x$.


<a id='Base.--Tuple{Integer,Nemo.MatElem{T}}' href='#Base.--Tuple{Integer,Nemo.MatElem{T}}'>#</a>
**`Base.-`** &mdash; *Method*.



```
-(x::Integer, y::MatElem)
```

> Return $S(x) - y$ where $S$ is the parent of $y$.


```
-(x, y)
```

Subtraction operator.

<a id='Base.--Tuple{Nemo.MatElem{T},Integer}' href='#Base.--Tuple{Nemo.MatElem{T},Integer}'>#</a>
**`Base.-`** &mdash; *Method*.



```
-(x::MatElem, y::Integer)
```

> Return $x - S(y)$, where $S$ is the parent of $x$


```
-(x, y)
```

Subtraction operator.

<a id='Base.--Tuple{Nemo.fmpz,Nemo.MatElem{T}}' href='#Base.--Tuple{Nemo.fmpz,Nemo.MatElem{T}}'>#</a>
**`Base.-`** &mdash; *Method*.



```
-(x::fmpz, y::MatElem)
```

> Return $S(x) - y$ where $S$ is the parent of $y$.


```
-(x, y)
```

Subtraction operator.

<a id='Base.--Tuple{Nemo.MatElem{T},Nemo.fmpz}' href='#Base.--Tuple{Nemo.MatElem{T},Nemo.fmpz}'>#</a>
**`Base.-`** &mdash; *Method*.



```
-(x::MatElem, y::fmpz)
```

> Return $x - S(y)$, where $S$ is the parent of $x$


```
-(x, y)
```

Subtraction operator.

<a id='Base.--Tuple{T<:Nemo.RingElem,Nemo.MatElem{T<:Nemo.RingElem}}' href='#Base.--Tuple{T<:Nemo.RingElem,Nemo.MatElem{T<:Nemo.RingElem}}'>#</a>
**`Base.-`** &mdash; *Method*.



```
-{T <: RingElem}(x::T, y::MatElem{T})
```

> Return $S(x) - y$ where $S$ is the parent of $y$.


```
-(x, y)
```

Subtraction operator.

<a id='Base.--Tuple{Nemo.MatElem{T<:Nemo.RingElem},T<:Nemo.RingElem}' href='#Base.--Tuple{Nemo.MatElem{T<:Nemo.RingElem},T<:Nemo.RingElem}'>#</a>
**`Base.-`** &mdash; *Method*.



```
-{T <: RingElem}(x::MatElem{T}, y::T)
```

> Return $x - S(y)$, where $S$ is the parent of $a$.


```
-(x, y)
```

Subtraction operator.

<a id='Base.*-Tuple{Integer,Nemo.MatElem{T}}' href='#Base.*-Tuple{Integer,Nemo.MatElem{T}}'>#</a>
**`Base.*`** &mdash; *Method*.



```
*(x::Integer, y::MatElem)
```

> Return $x\times y$.


```
*(x, y...)
```

Multiplication operator. `x*y*z*...` calls this function with all arguments, i.e. `*(x, y, z, ...)`.

<a id='Base.*-Tuple{Nemo.MatElem{T},Integer}' href='#Base.*-Tuple{Nemo.MatElem{T},Integer}'>#</a>
**`Base.*`** &mdash; *Method*.



```
*(x::MatElem, y::Integer)
```

> Return $x\times y$.


```
*(x, y...)
```

Multiplication operator. `x*y*z*...` calls this function with all arguments, i.e. `*(x, y, z, ...)`.

<a id='Base.*-Tuple{Nemo.fmpz,Nemo.MatElem{T}}' href='#Base.*-Tuple{Nemo.fmpz,Nemo.MatElem{T}}'>#</a>
**`Base.*`** &mdash; *Method*.



```
*(x::fmpz, y::MatElem)
```

> Return $x\times y$.


```
*(x, y...)
```

Multiplication operator. `x*y*z*...` calls this function with all arguments, i.e. `*(x, y, z, ...)`.

<a id='Base.*-Tuple{Nemo.MatElem{T},Nemo.fmpz}' href='#Base.*-Tuple{Nemo.MatElem{T},Nemo.fmpz}'>#</a>
**`Base.*`** &mdash; *Method*.



```
*(x::MatElem, y::fmpz)
```

> Return $x\times y$.


```
*(x, y...)
```

Multiplication operator. `x*y*z*...` calls this function with all arguments, i.e. `*(x, y, z, ...)`.

<a id='Base.*-Tuple{T<:Nemo.RingElem,Nemo.MatElem{T<:Nemo.RingElem}}' href='#Base.*-Tuple{T<:Nemo.RingElem,Nemo.MatElem{T<:Nemo.RingElem}}'>#</a>
**`Base.*`** &mdash; *Method*.



```
*{T <: RingElem}(x::T, y::MatElem{T})
```

> Return $x\times y$.


```
*(x, y...)
```

Multiplication operator. `x*y*z*...` calls this function with all arguments, i.e. `*(x, y, z, ...)`.

<a id='Base.*-Tuple{Nemo.MatElem{T<:Nemo.RingElem},T<:Nemo.RingElem}' href='#Base.*-Tuple{Nemo.MatElem{T<:Nemo.RingElem},T<:Nemo.RingElem}'>#</a>
**`Base.*`** &mdash; *Method*.



```
*{T <: RingElem}(x::MatElem{T}, y::T)
```

> Return $x\times y$.


```
*(x, y...)
```

Multiplication operator. `x*y*z*...` calls this function with all arguments, i.e. `*(x, y, z, ...)`.

<a id='Nemo.divexact-Tuple{Nemo.MatElem{T},Integer}' href='#Nemo.divexact-Tuple{Nemo.MatElem{T},Integer}'>#</a>
**`Nemo.divexact`** &mdash; *Method*.



```
divexact(x::MatElem, y::Integer)
```

> Return $x/y$, i.e. the matrix where each of the entries has been divided by $y$. Each division is expected to be exact.


<a id='Nemo.divexact-Tuple{Nemo.MatElem{T},Nemo.fmpz}' href='#Nemo.divexact-Tuple{Nemo.MatElem{T},Nemo.fmpz}'>#</a>
**`Nemo.divexact`** &mdash; *Method*.



```
divexact(x::MatElem, y::fmpz)
```

> Return $x/y$, i.e. the matrix where each of the entries has been divided by $y$. Each division is expected to be exact.


<a id='Nemo.divexact-Tuple{Nemo.MatElem{T<:Nemo.RingElem},T<:Nemo.RingElem}' href='#Nemo.divexact-Tuple{Nemo.MatElem{T<:Nemo.RingElem},T<:Nemo.RingElem}'>#</a>
**`Nemo.divexact`** &mdash; *Method*.



```
divexact{T <: RingElem}(x::MatElem{T}, y::T)
```

> Return $x/y$, i.e. the matrix where each of the entries has been divided by $y$. Each division is expected to be exact.


<a id='Base.^-Tuple{Nemo.MatElem{T},Int64}' href='#Base.^-Tuple{Nemo.MatElem{T},Int64}'>#</a>
**`Base.^`** &mdash; *Method*.



```
^(x, y)
```

Exponentiation operator.

<a id='Nemo.powers-Tuple{Nemo.MatElem{T<:Nemo.RingElem},Int64}' href='#Nemo.powers-Tuple{Nemo.MatElem{T<:Nemo.RingElem},Int64}'>#</a>
**`Nemo.powers`** &mdash; *Method*.



```
powers{T <: RingElem}(a::MatElem{T}, d::Int)
```

> Return an array of matrices $M$ wher $M[i + 1] = a^i$ for $i = 0..d$



If the appropriate `promote_rule` and coercion exists, these operators can also be used with elements of other rings. Nemo will try to coerce the operands to the dominating type and then apply the operator.


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


<a id='Comparison-operators-1'></a>

## Comparison operators


The following comparison operators are implemented for matrices in Nemo.

<a id='Base.==-Tuple{Nemo.MatElem{T<:Nemo.RingElem},Nemo.MatElem{T<:Nemo.RingElem}}' href='#Base.==-Tuple{Nemo.MatElem{T<:Nemo.RingElem},Nemo.MatElem{T<:Nemo.RingElem}}'>#</a>
**`Base.==`** &mdash; *Method*.



```
=={T <: RingElem}(x::MatElem{T}, y::MatElem{T})
```

> Return `true` if $x == y$ arithmetically, otherwise return `false`. Recall that power series to different precisions may still be arithmetically equal to the minimum of the two precisions.


```
=={T <: RingElem}(x::MatElem{T}, y::T)
```

> Return `true` if $x == S(y)$ arithmetically, where $S$ is the parent of $x$, otherwise return `false`.


```
=={T <: RingElem}(x::T, y::MatElem{T})
```

> Return `true` if $S(x) == y$ arithmetically, where $S$ is the parent of $y$, otherwise return `false`.


<a id='Base.isequal-Tuple{Nemo.MatElem{T<:Nemo.RingElem},Nemo.MatElem{T<:Nemo.RingElem}}' href='#Base.isequal-Tuple{Nemo.MatElem{T<:Nemo.RingElem},Nemo.MatElem{T<:Nemo.RingElem}}'>#</a>
**`Base.isequal`** &mdash; *Method*.



```
isequal{T <: RingElem}(x::MatElem{T}, y::MatElem{T})
```

> Return `true` if $x == y$ exactly, otherwise return `false`. This function is useful in cases where the entries of the matrices are inexact, e.g. power series. Only if the power series are precisely the same, to the same precision, are they declared equal by this function.



In addition we have the following ad hoc comparison operators.

<a id='Base.==-Tuple{Nemo.MatElem{T<:Nemo.RingElem},T<:Nemo.RingElem}' href='#Base.==-Tuple{Nemo.MatElem{T<:Nemo.RingElem},T<:Nemo.RingElem}'>#</a>
**`Base.==`** &mdash; *Method*.



```
=={T <: RingElem}(x::MatElem{T}, y::T)
```

> Return `true` if $x == S(y)$ arithmetically, where $S$ is the parent of $x$, otherwise return `false`.


<a id='Base.==-Tuple{T<:Nemo.RingElem,Nemo.MatElem{T<:Nemo.RingElem}}' href='#Base.==-Tuple{T<:Nemo.RingElem,Nemo.MatElem{T<:Nemo.RingElem}}'>#</a>
**`Base.==`** &mdash; *Method*.



```
=={T <: RingElem}(x::T, y::MatElem{T})
```

> Return `true` if $S(x) == y$ arithmetically, where $S$ is the parent of $y$, otherwise return `false`.


<a id='Base.==-Tuple{Nemo.MatElem{T},Integer}' href='#Base.==-Tuple{Nemo.MatElem{T},Integer}'>#</a>
**`Base.==`** &mdash; *Method*.



```
==(x::MatElem, y::Integer)
```

> Return `true` if $x == S(y)$ arithmetically, where $S$ is the parent of $x$, otherwise return `false`.


<a id='Base.==-Tuple{Integer,Nemo.MatElem{T}}' href='#Base.==-Tuple{Integer,Nemo.MatElem{T}}'>#</a>
**`Base.==`** &mdash; *Method*.



```
==(x::Integer, y::MatElem)
```

> Return `true` if $S(x) == y$ arithmetically, where $S$ is the parent of $y$, otherwise return `false`.


<a id='Base.==-Tuple{Nemo.MatElem{T},Nemo.fmpz}' href='#Base.==-Tuple{Nemo.MatElem{T},Nemo.fmpz}'>#</a>
**`Base.==`** &mdash; *Method*.



```
==(x::MatElem, y::fmpz)
```

> Return `true` if $x == S(y)$ arithmetically, where $S$ is the parent of $x$, otherwise return `false`.


<a id='Base.==-Tuple{Nemo.fmpz,Nemo.MatElem{T}}' href='#Base.==-Tuple{Nemo.fmpz,Nemo.MatElem{T}}'>#</a>
**`Base.==`** &mdash; *Method*.



```
==(x, y)
```

Generic equality operator, giving a single `Bool` result. Falls back to `===`. Should be implemented for all types with a notion of equality, based on the abstract value that an instance represents. For example, all numeric types are compared by numeric value, ignoring type. Strings are compared as sequences of characters, ignoring encoding.

Follows IEEE semantics for floating-point numbers.

Collections should generally implement `==` by calling `==` recursively on all contents.

New numeric types should implement this function for two arguments of the new type, and handle comparison to other types via promotion rules where possible.

```
=={T <: RingElem}(x::T, y::FracElem{T})
```

> Return `true` if $x == y$ arithmetically, otherwise return `false`.


```
=={T <: RingElem}(x::FracElem{T}, y::T)
```

> Return `true` if $x == y$ arithmetically, otherwise return `false`.


```
==(x::fmpz, y::FracElem)
```

> Return `true` if $x == y$ arithmetically, otherwise return `false`.


```
==(x::FracElem, y::fmpz)
```

> Return `true` if $x == y$ arithmetically, otherwise return `false`.


```
==(x::Integer, y::FracElem)
```

> Return `true` if $x == y$ arithmetically, otherwise return `false`.


```
==(x::FracElem, y::Integer)
```

> Return `true` if $x == y$ arithmetically, otherwise return `false`.


```
=={T <: RingElem}(x::FracElem{T}, y::FracElem{T})
```

> Return `true` if $x == y$ arithmetically, otherwise return `false`. Recall that power series to different precisions may still be arithmetically equal to the minimum of the two precisions.


```
=={T <: RingElem}(x::T, y::MatElem{T})
```

> Return `true` if $S(x) == y$ arithmetically, where $S$ is the parent of $y$, otherwise return `false`.


```
=={T <: RingElem}(x::MatElem{T}, y::T)
```

> Return `true` if $x == S(y)$ arithmetically, where $S$ is the parent of $x$, otherwise return `false`.


```
==(x::fmpz, y::MatElem)
```

> Return `true` if $S(x) == y$ arithmetically, where $S$ is the parent of $y$, otherwise return `false`.


```
==(x::MatElem, y::fmpz)
```

> Return `true` if $x == S(y)$ arithmetically, where $S$ is the parent of $x$, otherwise return `false`.


```
==(x::Integer, y::MatElem)
```

> Return `true` if $S(x) == y$ arithmetically, where $S$ is the parent of $y$, otherwise return `false`.


```
==(x::MatElem, y::Integer)
```

> Return `true` if $x == S(y)$ arithmetically, where $S$ is the parent of $x$, otherwise return `false`.


```
=={T <: RingElem}(x::MatElem{T}, y::MatElem{T})
```

> Return `true` if $x == y$ arithmetically, otherwise return `false`. Recall that power series to different precisions may still be arithmetically equal to the minimum of the two precisions.


```
==(x::fmpz, y::SeriesElem)
```

> Return `true` if $x == y$ arithmetically, otherwise return `false`.


```
==(x::Integer, y::SeriesElem)
```

> Return `true` if $x == y$ arithmetically, otherwise return `false`.


```
==(x::SeriesElem, y::fmpz)
```

> Return `true` if $x == y$ arithmetically, otherwise return `false`.


```
==(x::SeriesElem, y::Integer)
```

> Return `true` if $x == y$ arithmetically, otherwise return `false`.


```
=={T <: RingElem}(x::T, y::SeriesElem{T})
```

> Return `true` if $x == y$ arithmetically, otherwise return `false`.


```
=={T <: RingElem}(x::SeriesElem{T}, y::T)
```

> Return `true` if $x == y$ arithmetically, otherwise return `false`.


```
=={T <: RingElem}(x::SeriesElem{T}, y::SeriesElem{T})
```

> Return `true` if $x == y$ arithmetically, otherwise return `false`. Recall that power series to different precisions may still be arithmetically equal to the minimum of the two precisions.


```
==(x::fmpz, y::PolyElem)
```

> Return `true` if $x == y$ arithmetically, otherwise return `false`.


```
==(x::PolyElem, y::fmpz)
```

> Return `true` if $x == y$ arithmetically, otherwise return `false`.


```
==(x::Integer, y::PolyElem)
```

> Return `true` if $x == y$ arithmetically, otherwise return `false`.


```
==(x::PolyElem, y::Integer)
```

> Return `true` if $x == y$ arithmetically, otherwise return `false`.


```
=={T <: RingElem}(x::T, y::PolyElem{T})
```

> Return `true` if $x == y$ arithmetically, otherwise return `false`.


```
=={T <: RingElem}(x::PolyElem{T}, y::T)
```

> Return `true` if $x == y$ arithmetically, otherwise return `false`.


```
=={T <: RingElem}(x::PolyElem{T}, y::PolyElem{T})
```

> Return `true` if $x == y$ arithmetically, otherwise return `false`. Recall that power series to different precisions may still be arithmetically equal to the minimum of the two precisions.


```
=={T <: RingElem}(x::T, y::ResElem{T})
```

> Return `true` if $x == y$ arithmetically, otherwise return `false`.


```
=={T <: RingElem}(x::ResElem{T}, y::T)
```

> Return `true` if $x == y$ arithmetically, otherwise return `false`.


```
==(x::fmpz, y::ResElem)
```

> Return `true` if $x == y$ arithmetically, otherwise return `false`.


```
==(x::ResElem, y::fmpz)
```

> Return `true` if $x == y$ arithmetically, otherwise return `false`.


```
==(x::Integer, y::ResElem)
```

> Return `true` if $x == y$ arithmetically, otherwise return `false`.


```
==(x::ResElem, y::Integer)
```

> Return `true` if $x == y$ arithmetically, otherwise return `false`.


```
=={T <: RingElem}(x::ResElem{T}, y::ResElem{T})
```

> Return `true` if $x == y$ arithmetically, otherwise return `false`. Recall that power series to different precisions may still be arithmetically equal to the minimum of the two precisions.


```
==(a::perm, b::perm)
```

> Return `true` if the given permutations are equal, otherwise return `false`.



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


<a id='Scaling-1'></a>

## Scaling

<a id='Base.<<-Tuple{Nemo.fmpz_mat,Int64}' href='#Base.<<-Tuple{Nemo.fmpz_mat,Int64}'>#</a>
**`Base.<<`** &mdash; *Method*.



```
<<(x::fmpz_mat, y::Int)
```

> Return $2^yx$.


<a id='Base.>>-Tuple{Nemo.fmpz_mat,Int64}' href='#Base.>>-Tuple{Nemo.fmpz_mat,Int64}'>#</a>
**`Base.>>`** &mdash; *Method*.



```
>>(x::fmpz_mat, y::Int)
```

> Return $x/2^y$ where rounding is towards zero.



Here are some examples of scaling matrices.


```
S = MatrixSpace(ZZ, 3, 3)

A = S([fmpz(2) 3 5; 1 4 7; 9 6 3])
 
B = A<<5
C = B>>2
```


<a id='Transpose-1'></a>

## Transpose

<a id='Base.transpose-Tuple{Nemo.MatElem{T}}' href='#Base.transpose-Tuple{Nemo.MatElem{T}}'>#</a>
**`Base.transpose`** &mdash; *Method*.



```
transpose(x::MatElem)
```

> Return the transpose of the given matrix.



Here is an example of transposing a matrix.


```
R, t = PolynomialRing(QQ, "t")
S = MatrixSpace(R, 3, 3)

A = S([t + 1 t R(1); t^2 t t; R(-2) t + 2 t^2 + t + 1])

B = transpose(A)
```


<a id='Gram-matrix-1'></a>

## Gram matrix

<a id='Nemo.gram-Tuple{Nemo.MatElem{T}}' href='#Nemo.gram-Tuple{Nemo.MatElem{T}}'>#</a>
**`Nemo.gram`** &mdash; *Method*.



```
gram(x::MatElem)
```

> Return the Gram matrix of $x$, i.e. if $x$ is an $r\times c$ matrix return the $r\times r$ matrix whose entries $i, j$ are the dot products of the $i$-th and $j$-th rows, respectively.



Here is an example of computing the Gram matrix.


```
R, t = PolynomialRing(QQ, "t")
S = MatrixSpace(R, 3, 3)

A = S([t + 1 t R(1); t^2 t t; R(-2) t + 2 t^2 + t + 1])

B = gram(A)
```


<a id='Trace-1'></a>

## Trace

<a id='Base.LinAlg.trace-Tuple{Nemo.MatElem{T}}' href='#Base.LinAlg.trace-Tuple{Nemo.MatElem{T}}'>#</a>
**`Base.LinAlg.trace`** &mdash; *Method*.



```
trace(x::MatElem)
```

> Return the trace of the matrix $a$, i.e. the sum of the diagonal elements. We require the matrix to be square.



Here is an example of computing the trace.


```
R, t = PolynomialRing(QQ, "t")
S = MatrixSpace(R, 3, 3)

A = S([t + 1 t R(1); t^2 t t; R(-2) t + 2 t^2 + t + 1])

b = trace(A)
```


<a id='Content-1'></a>

## Content

<a id='Nemo.content-Tuple{Nemo.MatElem{T}}' href='#Nemo.content-Tuple{Nemo.MatElem{T}}'>#</a>
**`Nemo.content`** &mdash; *Method*.



```
content(x::MatElem)
```

> Return the content of the matrix $a$, i.e. the greatest common divisor of all its entries, assuming it exists.



Here is an example of computing the content of a matrix.


```
R, t = PolynomialRing(QQ, "t")
S = MatrixSpace(R, 3, 3)

A = S([t + 1 t R(1); t^2 t t; R(-2) t + 2 t^2 + t + 1])

b = content(A)
```


<a id='Concatenation-1'></a>

## Concatenation

<a id='Base.hcat-Tuple{Nemo.MatElem{T},Nemo.MatElem{T}}' href='#Base.hcat-Tuple{Nemo.MatElem{T},Nemo.MatElem{T}}'>#</a>
**`Base.hcat`** &mdash; *Method*.



```
hcat(a::MatElem, b::MatElem)
```

> Return the horizontal concatenation of $a$ and $b$. Assumes that the number of rows is the same in $a$ and $b$.


<a id='Base.vcat-Tuple{Nemo.MatElem{T},Nemo.MatElem{T}}' href='#Base.vcat-Tuple{Nemo.MatElem{T},Nemo.MatElem{T}}'>#</a>
**`Base.vcat`** &mdash; *Method*.



```
vcat(a::MatElem, b::MatElem)
```

> Return the vertical concatenation of $a$ and $b$. Assumes that the number of columns is the same in $a$ and $b$.



Here are some examples of concatenation.


```
R, t = PolynomialRing(QQ, "t")
S = MatrixSpace(R, 3, 3)

A = S([t + 1 t R(1); t^2 t t; R(-2) t + 2 t^2 + t + 1])
B = S([R(2) R(3) R(1); t t + 1 t + 2; R(-1) t^2 t^3])

hcat(A, B)
vcat(A, B)
```


<a id='Permutation-1'></a>

## Permutation

<a id='Base.*-Tuple{Nemo.perm,Nemo.MatElem{T}}' href='#Base.*-Tuple{Nemo.perm,Nemo.MatElem{T}}'>#</a>
**`Base.*`** &mdash; *Method*.



```
*(P::perm, x::MatElem)
```

> Apply the pemutation $P$ to the rows of the matrix $x$ and return the result.


```
*(x, y...)
```

Multiplication operator. `x*y*z*...` calls this function with all arguments, i.e. `*(x, y, z, ...)`.


Here is an example of applying a permutation to a matrix.


```
R, t = PolynomialRing(QQ, "t")
S = MatrixSpace(R, 3, 3)
G = FlintPermGroup(3)

A = S([t + 1 t R(1); t^2 t t; R(-2) t + 2 t^2 + t + 1])
P = G([1, 3, 2])

B = P*A
```


<a id='LU-factorisation-1'></a>

## LU factorisation

<a id='Base.LinAlg.lufact-Tuple{Nemo.MatElem{T<:Nemo.FieldElem},Nemo.FlintPermGroup}' href='#Base.LinAlg.lufact-Tuple{Nemo.MatElem{T<:Nemo.FieldElem},Nemo.FlintPermGroup}'>#</a>
**`Base.LinAlg.lufact`** &mdash; *Method*.



```
lufact{T <: FieldElem}(A::MatElem{T}, P = FlintPermGroup(rows(A)))
```

> Return a tuple $r, p, L, U$ consisting of the rank of $A$, a permutation $p$ of $A$ belonging to $P$, a lower triangular matrix $L$ and an upper triangular matrix $U$ such that $p(A) = LU$, where $p(A)$ stands for the matrix whose rows are the given permutation $p$ of the rows of $A$.


<a id='Nemo.fflu-Tuple{Nemo.MatElem{T<:Nemo.RingElem},Nemo.FlintPermGroup}' href='#Nemo.fflu-Tuple{Nemo.MatElem{T<:Nemo.RingElem},Nemo.FlintPermGroup}'>#</a>
**`Nemo.fflu`** &mdash; *Method*.



```
fflu{T <: RingElem}(A::MatElem{T}, P = FlintPermGroup(rows(A)))
```

> Return a tuple $r, d, p, L, U$ consisting of the rank of $A$, a denominator $d$, a permutation $p$ of $A$ belonging to $P$, a lower triangular matrix $L$ and an upper triangular matrix $U$ such that $p(A) = LD^1U$, where $p(A)$ stands for the matrix whose rows are the given permutation $p$ of the rows of $A$ and such that $D$ is the diagonal matrix diag$(p_1, p_1p_2, \ldots, p_{n-2}p_{n-1}, p_{n-1}$ where the $p_i$ are the inverses of the diagonal entries of $U$. The denominator $d$ is set to $\pm \mbox{det}(S)$ where $S$ is an appropriate submatrix of $A$ ($S = A$ if $A$ is square) and the sign is decided by the parity of the permutation.



Here are some examples of LU factorisation.


```
R, x = PolynomialRing(QQ, "x")
K, a = NumberField(x^3 + 3x + 1, "a")
S = MatrixSpace(K, 3, 3)
   
A = S([K(0) 2a + 3 a^2 + 1; a^2 - 2 a - 1 2a; a^2 - 2 a - 1 2a])

r, P, L, U = lufact(A)
r, d, P, L, U = fflu(A)
```


<a id='Reduced-row-echelon-form-1'></a>

## Reduced row-echelon form

<a id='Nemo.rref-Tuple{Nemo.MatElem{T<:Nemo.RingElem}}' href='#Nemo.rref-Tuple{Nemo.MatElem{T<:Nemo.RingElem}}'>#</a>
**`Nemo.rref`** &mdash; *Method*.



```
rref{T <: RingElem}(M::MatElem{T})
```

> Returns a tuple $(r, d, A)$ consisting of the rank $r$ of $M$ and a denominator $d$ in the base ring of $M$ and a matrix $A$ such that $A/d$ is the reduced row echelon form of $M$. Note that the denominator is not usually minimal.


<a id='Nemo.rref-Tuple{Nemo.MatElem{T<:Nemo.FieldElem}}' href='#Nemo.rref-Tuple{Nemo.MatElem{T<:Nemo.FieldElem}}'>#</a>
**`Nemo.rref`** &mdash; *Method*.



```
rref{T <: FieldElem}(M::MatElem{T})
```

> Returns a tuple $(r, A)$ consisting of the rank $r$ of $M$ and a reduced row echelon form $A$ of $M$.


```
rref{T <: RingElem}(M::MatElem{T})
```

> Returns a tuple $(r, d, A)$ consisting of the rank $r$ of $M$ and a denominator $d$ in the base ring of $M$ and a matrix $A$ such that $A/d$ is the reduced row echelon form of $M$. Note that the denominator is not usually minimal.


<a id='Nemo.is_rref-Tuple{Nemo.MatElem{T<:Nemo.RingElem}}' href='#Nemo.is_rref-Tuple{Nemo.MatElem{T<:Nemo.RingElem}}'>#</a>
**`Nemo.is_rref`** &mdash; *Method*.



```
is_rref{T <: RingElem}(M::MatElem{T})
```

> Return `true` if $M$ is in reduced row echelon form, otherwise return `false`.


<a id='Nemo.is_rref-Tuple{Nemo.MatElem{T<:Nemo.FieldElem}}' href='#Nemo.is_rref-Tuple{Nemo.MatElem{T<:Nemo.FieldElem}}'>#</a>
**`Nemo.is_rref`** &mdash; *Method*.



```
is_rref{T <: FieldElem}(M::MatElem{T})
```

> Return `true` if $M$ is in reduced row echelon form, otherwise return `false`.


```
is_rref{T <: RingElem}(M::MatElem{T})
```

> Return `true` if $M$ is in reduced row echelon form, otherwise return `false`.



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


<a id='Determinant-1'></a>

## Determinant

<a id='Base.LinAlg.det-Tuple{Nemo.MatElem{T<:Nemo.RingElem}}' href='#Base.LinAlg.det-Tuple{Nemo.MatElem{T<:Nemo.RingElem}}'>#</a>
**`Base.LinAlg.det`** &mdash; *Method*.



```
det{T <: RingElem}(M::MatElem{T})
```

> Return the determinant of the matrix $M$. We assume $M$ is square.


<a id='Base.LinAlg.det-Tuple{Nemo.MatElem{T<:Nemo.FieldElem}}' href='#Base.LinAlg.det-Tuple{Nemo.MatElem{T<:Nemo.FieldElem}}'>#</a>
**`Base.LinAlg.det`** &mdash; *Method*.



```
det{T <: FieldElem}(M::MatElem{T})
```

> Return the determinant of the matrix $M$. We assume $M$ is square.


```
det{T <: RingElem}(M::MatElem{T})
```

> Return the determinant of the matrix $M$. We assume $M$ is square.


<a id='Nemo.det_divisor-Tuple{Nemo.fmpz_mat}' href='#Nemo.det_divisor-Tuple{Nemo.fmpz_mat}'>#</a>
**`Nemo.det_divisor`** &mdash; *Method*.



```
det_divisor(x::fmpz_mat)
```

> Return some positive divisor of the determinant of $x$, if the determinant is nonzero, otherwise return zero.


<a id='Nemo.det_given_divisor-Tuple{Nemo.fmpz_mat,Integer,Bool}' href='#Nemo.det_given_divisor-Tuple{Nemo.fmpz_mat,Integer,Bool}'>#</a>
**`Nemo.det_given_divisor`** &mdash; *Method*.



```
det_given_divisor(x::fmpz_mat, d::Integer, proved=true)
```

> Return the determinant of $x$ given a positive divisor of its determinant. If `proved == true` (the default), the output is guaranteed to be correct, otherwise a heuristic algorithm is used.


<a id='Nemo.det_given_divisor-Tuple{Nemo.fmpz_mat,Nemo.fmpz,Bool}' href='#Nemo.det_given_divisor-Tuple{Nemo.fmpz_mat,Nemo.fmpz,Bool}'>#</a>
**`Nemo.det_given_divisor`** &mdash; *Method*.



```
det_given_divisor(x::fmpz_mat, d::fmpz, proved=true)
```

> Return the determinant of $x$ given a positive divisor of its determinant. If `proved == true` (the default), the output is guaranteed to be correct, otherwise a heuristic algorithm is used.



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


<a id='Rank-1'></a>

## Rank

<a id='Base.LinAlg.rank-Tuple{Nemo.MatElem{T<:Nemo.RingElem}}' href='#Base.LinAlg.rank-Tuple{Nemo.MatElem{T<:Nemo.RingElem}}'>#</a>
**`Base.LinAlg.rank`** &mdash; *Method*.



```
rank{T <: RingElem}(M::MatElem{T})
```

> Return the rank of the matrix $M$.


<a id='Base.LinAlg.rank-Tuple{Nemo.MatElem{T<:Nemo.FieldElem}}' href='#Base.LinAlg.rank-Tuple{Nemo.MatElem{T<:Nemo.FieldElem}}'>#</a>
**`Base.LinAlg.rank`** &mdash; *Method*.



```
rank{T <: FieldElem}(M::MatElem{T})
```

> Return the rank of the matrix $M$.


```
rank{T <: RingElem}(M::MatElem{T})
```

> Return the rank of the matrix $M$.



Here are some examples of computing the rank of a matrix.


```
R, x = PolynomialRing(QQ, "x")
K, a = NumberField(x^3 + 3x + 1, "a")
S = MatrixSpace(K, 3, 3)
   
A = S([K(0) 2a + 3 a^2 + 1; a^2 - 2 a - 1 2a; a^2 + 3a + 1 2a K(1)])

d = rank(A)
```


<a id='Linear-solving-1'></a>

## Linear solving

<a id='Nemo.solve-Tuple{Nemo.MatElem{T<:Nemo.RingElem},Nemo.MatElem{T<:Nemo.RingElem}}' href='#Nemo.solve-Tuple{Nemo.MatElem{T<:Nemo.RingElem},Nemo.MatElem{T<:Nemo.RingElem}}'>#</a>
**`Nemo.solve`** &mdash; *Method*.



```
solve{T <: RingElem}(M::MatElem{T}, b::MatElem{T})
```

> Given a non-singular $n\times n$ matrix over a ring and an $n\times m$ matrix over the same ring, return a tuple $x, d$ consisting of an $n\times m$ matrix $x$ and a denominator $d$ such that $Ax = db$. The denominator will be the determinant of $A$ up to sign. If $A$ is singular an exception is raised.


<a id='Nemo.solve_triu-Tuple{Nemo.MatElem{T<:Nemo.FieldElem},Nemo.MatElem{T<:Nemo.FieldElem},Bool}' href='#Nemo.solve_triu-Tuple{Nemo.MatElem{T<:Nemo.FieldElem},Nemo.MatElem{T<:Nemo.FieldElem},Bool}'>#</a>
**`Nemo.solve_triu`** &mdash; *Method*.



```
solve_triu{T <: FieldElem}(U::MatElem{T}, b::MatElem{T}, unit=false)
```

> Given a non-singular $n\times n$ matrix over a field which is upper triangular, and an $n\times m$ matrix over the same field, return an $n\times m$ matrix $x$ such that $Ax = b$. If $A$ is singular an exception is raised. If unit is true then $U$ is assumed to have ones on its diagonal, and the diagonal will not be read.


<a id='Nemo.solve_dixon-Tuple{Nemo.fmpz_mat,Nemo.fmpz_mat}' href='#Nemo.solve_dixon-Tuple{Nemo.fmpz_mat,Nemo.fmpz_mat}'>#</a>
**`Nemo.solve_dixon`** &mdash; *Method*.



```
solve_dixon(a::fmpz_mat, b::fmpz_mat)
```

> Return a tuple $(x, m)$ consisting of the column vector $x$ such that $ax = b \pmod{m}$ where $x$ and $b$ are column vectors with the same number of rows as the $a$. Note that $a$ must be a square matrix. If these conditions are not met, an exception is raised.


<a id='Nemo.solve_dixon-Tuple{Nemo.fmpq_mat,Nemo.fmpq_mat}' href='#Nemo.solve_dixon-Tuple{Nemo.fmpq_mat,Nemo.fmpq_mat}'>#</a>
**`Nemo.solve_dixon`** &mdash; *Method*.



```
solve_dixon(a::fmpq_mat, b::fmpq_mat)
```

> Solve $ax = b$ by clearing denominators and using Dixon's algorithm. This is usually faster for large systems.



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


<a id='Inverse-1'></a>

## Inverse

<a id='Base.inv-Tuple{Nemo.MatElem{T<:Nemo.RingElem}}' href='#Base.inv-Tuple{Nemo.MatElem{T<:Nemo.RingElem}}'>#</a>
**`Base.inv`** &mdash; *Method*.



```
inv{T <: RingElem}(M::MatElem{T})
```

> Given a non-singular $n\times n$ matrix over a ring the tuple $X, d$ consisting of an $n\times n$ matrix $X$ and a denominator $d$ such that $AX = dI_n$, where $I_n$ is the $n\times n$ identity matrix. The denominator will be the determinant of $A$ up to sign. If $A$ is singular an exception  is raised.


<a id='Base.inv-Tuple{Nemo.MatElem{T<:Nemo.FieldElem}}' href='#Base.inv-Tuple{Nemo.MatElem{T<:Nemo.FieldElem}}'>#</a>
**`Base.inv`** &mdash; *Method*.



```
inv{T <: FieldElem}(M::MatElem{T})
```

> Given a non-singular $n\times n$ matrix over a field, return an $n\times n$ matrix $X$ such that $AX = I_n$ where $I_n$ is the $n\times n$ identity matrix. If $A$ is singular an exception is raised.


```
inv{T <: RingElem}(M::MatElem{T})
```

> Given a non-singular $n\times n$ matrix over a ring the tuple $X, d$ consisting of an $n\times n$ matrix $X$ and a denominator $d$ such that $AX = dI_n$, where $I_n$ is the $n\times n$ identity matrix. The denominator will be the determinant of $A$ up to sign. If $A$ is singular an exception  is raised.


<a id='Nemo.pseudo_inv-Tuple{Nemo.fmpz_mat}' href='#Nemo.pseudo_inv-Tuple{Nemo.fmpz_mat}'>#</a>
**`Nemo.pseudo_inv`** &mdash; *Method*.



```
pseudo_inv(x::fmpz_mat)
```

> Return a tuple $(z, d)$ consisting of a matrix $z$ and denominator $d$ such that $z/d$ is the inverse of $x$.



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


<a id='Nullspace-1'></a>

## Nullspace

<a id='Base.LinAlg.nullspace-Tuple{Nemo.MatElem{T<:Nemo.RingElem}}' href='#Base.LinAlg.nullspace-Tuple{Nemo.MatElem{T<:Nemo.RingElem}}'>#</a>
**`Base.LinAlg.nullspace`** &mdash; *Method*.



```
nullspace{T <: RingElem}(M::MatElem{T})
```

> Returns a tuple $(\nu, N)$ consisting of the nullity $\nu$ of $M$ and a basis $N$ (consisting of column vectors) for the right nullspace of $M$, i.e. such that $MN$ is the zero matrix. If $M$ is an $m\times n$ matrix $N$ will be an $n\times \nu$ matrix. Note that the nullspace is taken to be the vector space kernel over the fraction field of the base ring if the latter is not a field. In Nemo we use the name ``kernel'' for a function to compute an integral kernel.


<a id='Base.LinAlg.nullspace-Tuple{Nemo.MatElem{T<:Nemo.FieldElem}}' href='#Base.LinAlg.nullspace-Tuple{Nemo.MatElem{T<:Nemo.FieldElem}}'>#</a>
**`Base.LinAlg.nullspace`** &mdash; *Method*.



```
nullspace{T <: FieldElem}(M::MatElem{T})
```

> Returns a tuple $(\nu, N)$ consisting of the nullity $\nu$ of $M$ and a basis $N$ (consisting of column vectors) for the right nullspace of $M$, i.e. such that $MN$ is the zero matrix. If $M$ is an $m\times n$ matrix $N$ will be an $n\times \nu$ matrix. Note that the nullspace is taken to be the vector space kernel over the fraction field of the base ring if the latter is not a field. In Nemo we use the name ``kernel'' for a function to compute an integral kernel.


```
nullspace{T <: RingElem}(M::MatElem{T})
```

> Returns a tuple $(\nu, N)$ consisting of the nullity $\nu$ of $M$ and a basis $N$ (consisting of column vectors) for the right nullspace of $M$, i.e. such that $MN$ is the zero matrix. If $M$ is an $m\times n$ matrix $N$ will be an $n\times \nu$ matrix. Note that the nullspace is taken to be the vector space kernel over the fraction field of the base ring if the latter is not a field. In Nemo we use the name ``kernel'' for a function to compute an integral kernel.


<a id='Nemo.nullspace_right_rational-Tuple{Nemo.fmpz_mat}' href='#Nemo.nullspace_right_rational-Tuple{Nemo.fmpz_mat}'>#</a>
**`Nemo.nullspace_right_rational`** &mdash; *Method*.



```
nullspace_right_rational(x::fmpz_mat)
```

> Return the right rational nullspace of $x$, i.e. a set of vectors over $\mathbb{Z}$ giving a $\mathbb{Q}$-basis for the nullspace of $x$ considered as a matrix over $\mathbb{Q}$.



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


<a id='Hessenberg-form-1'></a>

## Hessenberg form

<a id='Nemo.hessenberg-Tuple{Nemo.MatElem{T<:Nemo.RingElem}}' href='#Nemo.hessenberg-Tuple{Nemo.MatElem{T<:Nemo.RingElem}}'>#</a>
**`Nemo.hessenberg`** &mdash; *Method*.



```
hessenberg{T <: RingElem}(A::MatElem{T})
```

> Returns the Hessenberg form of $M$, i.e. an upper Hessenberg matrix which is similar to $M$. The upper Hessenberg form has nonzero entries above and on the diagonal and in the diagonal line immediately below the diagonal.


<a id='Nemo.is_hessenberg-Tuple{Nemo.MatElem{T<:Nemo.RingElem}}' href='#Nemo.is_hessenberg-Tuple{Nemo.MatElem{T<:Nemo.RingElem}}'>#</a>
**`Nemo.is_hessenberg`** &mdash; *Method*.



```
is_hessenberg{T <: RingElem}(A::MatElem{T})
```

> Returns `true` if $M$ is in Hessenberg form, otherwise returns `false`.



Here are some examples of computing the Hessenberg form.


```
R = ResidueRing(ZZ, 7)
S = MatrixSpace(R, 4, 4)
   
M = S([R(1) R(2) R(4) R(3); R(2) R(5) R(1) R(0);
       R(6) R(1) R(3) R(2); R(1) R(1) R(3) R(5)])
   
A = hessenberg(M)
is_hessenberg(A) == true
```


<a id='Characteristic-polynomial-1'></a>

## Characteristic polynomial

<a id='Nemo.charpoly-Tuple{Nemo.Ring,Nemo.MatElem{T<:Nemo.RingElem}}' href='#Nemo.charpoly-Tuple{Nemo.Ring,Nemo.MatElem{T<:Nemo.RingElem}}'>#</a>
**`Nemo.charpoly`** &mdash; *Method*.



```
charpoly{T <: RingElem}(V::Ring, Y::MatElem{T})
```

> Returns the characteristic polynomial $p$ of the matrix $M$. The polynomial ring $R$ of the resulting polynomial must be supplied and the matrix is assumed to be square.



Here are some examples of computing the characteristic polynomial.


```
R = ResidueRing(ZZ, 7)
S = MatrixSpace(R, 4, 4)
T, x = PolynomialRing(R, "x")

M = S([R(1) R(2) R(4) R(3); R(2) R(5) R(1) R(0);
       R(6) R(1) R(3) R(2); R(1) R(1) R(3) R(5)])
   
A = charpoly(T, M)
```


<a id='Minimal-polynomial-1'></a>

## Minimal polynomial

<a id='Nemo.minpoly-Tuple{Nemo.Ring,Nemo.MatElem{T<:Nemo.RingElem},Bool}' href='#Nemo.minpoly-Tuple{Nemo.Ring,Nemo.MatElem{T<:Nemo.RingElem},Bool}'>#</a>
**`Nemo.minpoly`** &mdash; *Method*.



```
minpoly{T <: RingElem}(S::Ring, M::MatElem{T}, charpoly_only = false)
```

> Returns the minimal polynomial $p$ of the matrix $M$. The polynomial ring $R$ of the resulting polynomial must be supplied and the matrix must be square.


<a id='Nemo.minpoly-Tuple{Nemo.Ring,Nemo.MatElem{T<:Nemo.FieldElem},Bool}' href='#Nemo.minpoly-Tuple{Nemo.Ring,Nemo.MatElem{T<:Nemo.FieldElem},Bool}'>#</a>
**`Nemo.minpoly`** &mdash; *Method*.



```
minpoly{T <: FieldElem}(S::Ring, M::MatElem{T}, charpoly_only = false)
```

> Returns the minimal polynomial $p$ of the matrix $M$. The polynomial ring $R$ of the resulting polynomial must be supplied and the matrix must be square.


```
minpoly{T <: RingElem}(S::Ring, M::MatElem{T}, charpoly_only = false)
```

> Returns the minimal polynomial $p$ of the matrix $M$. The polynomial ring $R$ of the resulting polynomial must be supplied and the matrix must be square.



Here are some examples of computing the minimal polynomial of a matrix.


```
R, x = FiniteField(13, 1, "x")
T, y = PolynomialRing(R, "y")
   
M = R[7 6 1;
      7 7 5;
      8 12 5]

A = minpoly(T, M)
```


<a id='Transforms-1'></a>

## Transforms

<a id='Nemo.similarity!-Tuple{Nemo.MatElem{T<:Nemo.RingElem},Int64,T<:Nemo.RingElem}' href='#Nemo.similarity!-Tuple{Nemo.MatElem{T<:Nemo.RingElem},Int64,T<:Nemo.RingElem}'>#</a>
**`Nemo.similarity!`** &mdash; *Method*.



```
similarity!{T <: RingElem}(A::MatElem{T}, r::Int, d::T)
```

> Applies a similarity transform to the $n\times n$ matrix $M$ in-place. Let $P$ be the $n\times n$ identity matrix that has had all zero entries of row $r$ replaced with $d$, then the transform applied is equivalent to $M = P^{-1}MP$. We require $M$ to be a square matrix. A similarity transform preserves the minimal and characteristic polynomials of a matrix.



Here is an example of applying a similarity transform to a matrix.


```
R = ResidueRing(ZZ, 7)
S = MatrixSpace(R, 4, 4)
   
M = S([R(1) R(2) R(4) R(3); R(2) R(5) R(1) R(0);
       R(6) R(1) R(3) R(2); R(1) R(1) R(3) R(5)])
   
similarity!(M, 1, R(3))
```


<a id='Modular-reduction-1'></a>

## Modular reduction

<a id='Nemo.reduce_mod-Tuple{Nemo.fmpz_mat,Integer}' href='#Nemo.reduce_mod-Tuple{Nemo.fmpz_mat,Integer}'>#</a>
**`Nemo.reduce_mod`** &mdash; *Method*.



```
reduce_mod(x::fmpz_mat, y::Integer)
```

> Reduce the entries of $x$ modulo $y$ and return the result.


<a id='Nemo.reduce_mod-Tuple{Nemo.fmpz_mat,Nemo.fmpz}' href='#Nemo.reduce_mod-Tuple{Nemo.fmpz_mat,Nemo.fmpz}'>#</a>
**`Nemo.reduce_mod`** &mdash; *Method*.



```
reduce_mod(x::fmpz_mat, y::fmpz)
```

> Reduce the entries of $x$ modulo $y$ and return the result.



Here are some examples of modular reduction.


```
S = MatrixSpace(ZZ, 3, 3)

A = S([fmpz(2) 3 5; 1 4 7; 9 2 2])
   
reduce_mod(A, ZZ(5))
reduce_mod(A, 2)
```


<a id='Lifting-1'></a>

## Lifting

<a id='Nemo.lift-Tuple{Nemo.nmod_mat}' href='#Nemo.lift-Tuple{Nemo.nmod_mat}'>#</a>
**`Nemo.lift`** &mdash; *Method*.



```
lift(a::nmod_mat)
```

> Return a lift of the matrix $a$ to a matrix over $\mathbb{Z}$, i.e. where the entries of the returned matrix are those of $a$ lifted to $\mathbb{Z}$.



Here are some examples of lifting.


```
R = ResidueRing(ZZ, 7)
S = MatrixSpace(R, 3, 3)

a = S([4 5 6; 7 3 2; 1 4 5])
  
 b = lift(a)
```


<a id='Special-matrices-1'></a>

## Special matrices

<a id='Nemo.hadamard-Tuple{Nemo.FmpzMatSpace}' href='#Nemo.hadamard-Tuple{Nemo.FmpzMatSpace}'>#</a>
**`Nemo.hadamard`** &mdash; *Method*.



```
hadamard(R::FmpzMatSpace)
```

> Return the Hadamard matrix for the given matrix space. The number of rows and columns must be equal.


<a id='Nemo.is_hadamard-Tuple{Nemo.fmpz_mat}' href='#Nemo.is_hadamard-Tuple{Nemo.fmpz_mat}'>#</a>
**`Nemo.is_hadamard`** &mdash; *Method*.



```
is_hadamard(x::fmpz_mat)
```

> Return `true` if the given matrix is Hadamard, otherwise return `false`.


<a id='Nemo.hilbert-Tuple{Nemo.FmpqMatSpace}' href='#Nemo.hilbert-Tuple{Nemo.FmpqMatSpace}'>#</a>
**`Nemo.hilbert`** &mdash; *Method*.



```
hilbert(R::FmpqMatSpace)
```

> Return the Hilbert matrix in the given matrix space. This is the matrix with entries $H_{i,j} = 1/(i + j - 1)$.



Here are some examples of computing special matrices.


```
R = MatrixSpace(ZZ, 3, 3)
S = MatrixSpace(QQ, 3, 3)

A = hadamard(R)
is_hadamard(A)
B = hilbert(R)
```


<a id='Hermite-Normal-Form-1'></a>

## Hermite Normal Form

<a id='Nemo.hnf-Tuple{Nemo.fmpz_mat}' href='#Nemo.hnf-Tuple{Nemo.fmpz_mat}'>#</a>
**`Nemo.hnf`** &mdash; *Method*.



```
hnf(x::fmpz_mat)
```

> Return the Hermite Normal Form of $x$.


<a id='Nemo.hnf_with_transform-Tuple{Nemo.fmpz_mat}' href='#Nemo.hnf_with_transform-Tuple{Nemo.fmpz_mat}'>#</a>
**`Nemo.hnf_with_transform`** &mdash; *Method*.



```
hnf_with_transform(x::fmpz_mat)
```

> Compute a tuple $(H, T)$ where $H$ is the Hermite normal form of $x$ and $T$ is a transformation matrix so that $H = Tx$.


<a id='Nemo.hnf_modular-Tuple{Nemo.fmpz_mat,Nemo.fmpz}' href='#Nemo.hnf_modular-Tuple{Nemo.fmpz_mat,Nemo.fmpz}'>#</a>
**`Nemo.hnf_modular`** &mdash; *Method*.



```
hnf_modular(x::fmpz_mat, d::fmpz)
```

> Compute the Hermite normal form of $x$ given that $d$ is a multiple of the determinant of the nonzero rows of $x$.


<a id='Nemo.hnf_modular_eldiv-Tuple{Nemo.fmpz_mat,Nemo.fmpz}' href='#Nemo.hnf_modular_eldiv-Tuple{Nemo.fmpz_mat,Nemo.fmpz}'>#</a>
**`Nemo.hnf_modular_eldiv`** &mdash; *Method*.



```
hnf_modular_eldiv(x::fmpz_mat, d::fmpz)
```

> Compute the Hermite normal form of $x$ given that $d$ is a multiple of the largest elementary divisor of $x$. The matrix $x$ must have full rank.


<a id='Nemo.is_hnf-Tuple{Nemo.fmpz_mat}' href='#Nemo.is_hnf-Tuple{Nemo.fmpz_mat}'>#</a>
**`Nemo.is_hnf`** &mdash; *Method*.



```
is_hnf(x::fmpz_mat)
```

> Return `true` if the given matrix is in Hermite Normal Form, otherwise return `false`.



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


<a id='Lattice-basis-reduction-1'></a>

## Lattice basis reduction


Nemo provides LLL lattice basis reduction. Optionally one can specify the setup using a context object created by the following function.


```
lll_ctx(delta::Float64, eta::Float64, rep=:zbasis, gram=:approx)
```


Return a LLL context object specifying LLL parameters $\delta$ and $\eta$ and specifying the representation as either `:zbasis` or `:gram` and the Gram type as either `:approx` or `:exact`.

<a id='Nemo.lll-Tuple{Nemo.fmpz_mat,Nemo.lll_ctx}' href='#Nemo.lll-Tuple{Nemo.fmpz_mat,Nemo.lll_ctx}'>#</a>
**`Nemo.lll`** &mdash; *Method*.



```
lll(x::fmpz_mat, ctx=lll_ctx(0.99, 0.51))
```

> Return the LLL reduction of the matrix $x$. By default the matrix $x$ is a $\mathbb{Z}$-basis and the Gram matrix is maintained throughout in approximate form. The LLL is performed with reduction parameters $\delta = 0.99$ and $\eta = 0.51$. All of these defaults can be overridden by specifying an optional context object.


<a id='Nemo.lll_with_transform-Tuple{Nemo.fmpz_mat,Nemo.lll_ctx}' href='#Nemo.lll_with_transform-Tuple{Nemo.fmpz_mat,Nemo.lll_ctx}'>#</a>
**`Nemo.lll_with_transform`** &mdash; *Method*.



> Compute a tuple $(L, T)$ where $L$ is the LLL reduction of $a$ and $T$ is a transformation matrix so that $L = Ta$. All the default parameters can be overridden by supplying an optional context object.


<a id='Nemo.lll_gram-Tuple{Nemo.fmpz_mat,Nemo.lll_ctx}' href='#Nemo.lll_gram-Tuple{Nemo.fmpz_mat,Nemo.lll_ctx}'>#</a>
**`Nemo.lll_gram`** &mdash; *Method*.



```
lll_gram(x::fmpz_mat, ctx=lll_ctx(0.99, 0.51, :gram))
```

> Given the Gram matrix $x$ of a matrix, compute the Gram matrix of its LLL reduction.


<a id='Nemo.lll_gram_with_transform-Tuple{Nemo.fmpz_mat,Nemo.lll_ctx}' href='#Nemo.lll_gram_with_transform-Tuple{Nemo.fmpz_mat,Nemo.lll_ctx}'>#</a>
**`Nemo.lll_gram_with_transform`** &mdash; *Method*.



```
lll_gram_with_transform(x::fmpz_mat, ctx=lll_ctx(0.99, 0.51, :gram))
```

> Given the Gram matrix $x$ of a matrix $M$, compute a tuple $(L, T)$ where $L$ is the gram matrix of the LLL reduction of the matrix and $T$ is a transformation matrix so that $L = TM$.


<a id='Nemo.lll_with_removal-Tuple{Nemo.fmpz_mat,Nemo.fmpz,Nemo.lll_ctx}' href='#Nemo.lll_with_removal-Tuple{Nemo.fmpz_mat,Nemo.fmpz,Nemo.lll_ctx}'>#</a>
**`Nemo.lll_with_removal`** &mdash; *Method*.



```
lll_with_removal(x::fmpz_mat, b::fmpz, ctx=lll_ctx(0.99, 0.51))
```

> Compute the LLL reduction of $x$ and throw away rows whose norm exceeds the given bound $b$. Return a tuple $(r, L)$ where the first $r$ rows of $L$ are the rows remaining after removal.


<a id='Nemo.lll_with_removal_transform-Tuple{Nemo.fmpz_mat,Nemo.fmpz,Nemo.lll_ctx}' href='#Nemo.lll_with_removal_transform-Tuple{Nemo.fmpz_mat,Nemo.fmpz,Nemo.lll_ctx}'>#</a>
**`Nemo.lll_with_removal_transform`** &mdash; *Method*.



```
lll_with_removal_transform(x::fmpz_mat, b::fmpz, ctx=lll_ctx(0.99, 0.51))
```

> Compute a tuple $(r, L, T)$ where the first $r$ rows of $L$ are those remaining from the LLL reduction after removal of vectors with norm exceeding the bound $b$ and $T$ is a transformation matrix so that $L = Tx$.



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


<a id='Smith-Normal-Form-1'></a>

## Smith Normal Form

<a id='Nemo.snf-Tuple{Nemo.fmpz_mat}' href='#Nemo.snf-Tuple{Nemo.fmpz_mat}'>#</a>
**`Nemo.snf`** &mdash; *Method*.



```
snf(x::fmpz_mat)
```

> Compute the Smith normal form of $x$.


<a id='Nemo.snf_diagonal-Tuple{Nemo.fmpz_mat}' href='#Nemo.snf_diagonal-Tuple{Nemo.fmpz_mat}'>#</a>
**`Nemo.snf_diagonal`** &mdash; *Method*.



```
snf_diagonal(x::fmpz_mat)
```

> Given a diagonal matrix $x$ compute the Smith normal form of $x$.


<a id='Nemo.is_snf-Tuple{Nemo.fmpz_mat}' href='#Nemo.is_snf-Tuple{Nemo.fmpz_mat}'>#</a>
**`Nemo.is_snf`** &mdash; *Method*.



```
is_snf(x::fmpz_mat)
```

> Return `true` if $x$ is in Smith normal form, otherwise return `false`.



Here are some examples of computing the Smith Normal Form.


```
S = MatrixSpace(ZZ, 3, 3)

A = S([fmpz(2) 3 5; 1 4 7; 19 3 7])
   
B = snf(A)
is_snf(B) == true

B = S([fmpz(2) 0 0; 0 4 0; 0 0 7])

C = snf_diagonal(B)
```


<a id='Strong-Echelon-Form-1'></a>

## Strong Echelon Form

<a id='Nemo.strong_echelon_form-Tuple{Nemo.nmod_mat}' href='#Nemo.strong_echelon_form-Tuple{Nemo.nmod_mat}'>#</a>
**`Nemo.strong_echelon_form`** &mdash; *Method*.



```
strong_echelon_form(a::nmod_mat)
```

> Return the strong echeleon form of $a$. The matrix $a$ must have at least as many rows as columns.



Here is an example of computing the strong echelon form.


```
R = ResidueRing(ZZ, 12)
S = MatrixSpace(R, 3, 3)

A = S([4 1 0; 0 0 5; 0 0 0 ])

B = strong_echelon_form(A)
```


<a id='Howell-Form-1'></a>

## Howell Form

<a id='Nemo.howell_form-Tuple{Nemo.nmod_mat}' href='#Nemo.howell_form-Tuple{Nemo.nmod_mat}'>#</a>
**`Nemo.howell_form`** &mdash; *Method*.



```
howell_form(a::nmod_mat)
```

> Return the Howell normal form of $a$. The matrix $a$ must have at least as many rows as columns.



Here is an example of computing the Howell form.


```
R = ResidueRing(ZZ, 12)
S = MatrixSpace(R, 3, 3)

A = S([4 1 0; 0 0 5; 0 0 0 ])

B = howell_form(A)
```


<a id='Gram-Schmidt-Orthogonoalisation-1'></a>

## Gram-Schmidt Orthogonoalisation

<a id='Nemo.gso-Tuple{Nemo.fmpq_mat}' href='#Nemo.gso-Tuple{Nemo.fmpq_mat}'>#</a>
**`Nemo.gso`** &mdash; *Method*.



```
gso(x::fmpq_mat)
```

> Return the Gram-Schmidt Orthogonalisation of the matrix $x$.



Here are some examples of computing the Gram-Schmidt Orthogonalisation.


```
S = MatrixSpace(QQ, 3, 3)

A = S([4 7 3; 2 9 1; 0 5 3])

B = gso(A)
```

