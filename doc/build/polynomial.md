


<a id='Introduction-1'></a>

## Introduction


Nemo allow the creation of dense, univariate polynomials over any computable ring $R$. There are two different kinds of implementation: a generic one for the case where no specific implementation exists, and efficient implementations of polynomials over numerous specific rings, usually provided by C/C++ libraries.


The following table shows each of the polynomial types available in Nemo, the base ring $R$, and the Julia/Nemo types for that kind of polynomial (the type information is mainly of concern to developers).


                           Base ring | Library |    Element type |       Parent type
-----------------------------------: | ------: | --------------: | ----------------:
                    Generic ring $R$ |    Nemo |    `GenPoly{T}` |  `GenPolyRing{T}`
                        $\mathbb{Z}$ |   Flint |     `fmpz_poly` |    `FmpzPolyRing`
$\mathbb{Z}/n\mathbb{Z}$ (small $n$) |   Flint |     `nmod_poly` |    `NmodPolyRing`
$\mathbb{Z}/n\mathbb{Z}$ (large $n$) |   Flint | `fmpz_mod_poly` | `FmpzModPolyRing`
                        $\mathbb{Q}$ |   Flint |     `fmpq_poly` |    `FmpqPolyRing`
      $\mathbb{F}_{p^n}$ (small $n$) |   Flint |  `fq_nmod_poly` |  `FqNmodPolyRing`
      $\mathbb{F}_{p^n}$ (large $n$) |   Flint |       `fq_poly` |      `FqPolyRing`


The string representation of the variable and the base ring $R$ of a generic polynomial is stored in its parent object. 


All polynomial element types belong to the abstract type `PolyElem` and all of the polynomial ring types belong to the abstract type `PolyRing`. This enables one to write generic functions that can accept any Nemo polynomial type.


<a id='Polynomial-ring-constructors-1'></a>

## Polynomial ring constructors


In order to construct polynomials in Nemo, one must first construct the polynomial ring itself. This is accomplished with the following constructor.

<a id='Nemo.PolynomialRing-Tuple{Nemo.Ring,AbstractString,Bool}' href='#Nemo.PolynomialRing-Tuple{Nemo.Ring,AbstractString,Bool}'>#</a>
**`Nemo.PolynomialRing`** &mdash; *Method*.



nothing

A shorthand version of this function is provided: given a base ring `R`, we abbreviate the constructor as follows.


```
R["x"]
```


Here are some examples of creating polynomial rings and making use of the resulting parent objects to coerce various elements into the polynomial ring.


```
R, x = PolynomialRing(ZZ, "x")
S, y = PolynomialRing(R, "y")
T, z = QQ["z"]

f = R()
g = R(123)
h = S(ZZ(1234))
k = S(x + 1)
m = T(z + 1)
```


<a id='Polynomial-element-constructors-1'></a>

## Polynomial element constructors


Once a polynomial ring is constructed, there are various ways to construct polynomials in that ring.


The easiest way is simply using the generator returned by the `PolynomialRing` constructor and and build up the polynomial using basic arithmetic. Julia has quite flexible notation for the construction of polynomials in this way.


In addition we provide the following functions for constructing certain useful polynomials.

<a id='Base.zero-Tuple{Nemo.PolyRing{T}}' href='#Base.zero-Tuple{Nemo.PolyRing{T}}'>#</a>
**`Base.zero`** &mdash; *Method*.



```
zero(R::PolyRing)
```

> Return the zero polynomial in the given polynomial ring.


<a id='Base.one-Tuple{Nemo.PolyRing{T}}' href='#Base.one-Tuple{Nemo.PolyRing{T}}'>#</a>
**`Base.one`** &mdash; *Method*.



```
one(R::PolyRing)
```

> Return the constant polynomial $1$ in the given polynomial ring.


<a id='Nemo.gen-Tuple{Nemo.PolyRing{T}}' href='#Nemo.gen-Tuple{Nemo.PolyRing{T}}'>#</a>
**`Nemo.gen`** &mdash; *Method*.



```
gen(R::PolyRing)
```

> Return the generator of the given polynomial ring.



Here are some examples of constructing polynomials.


```
R, x = PolynomialRing(ZZ, "x")
S, y = PolynomialRing(R, "y")

f = x^3 + 3x + 21
g = (x + 1)*y^2 + 2x + 1

h = zero(S)
k = one(R)
m = gen(S)
```


<a id='Basic-functionality-1'></a>

## Basic functionality


All univariate polynomial modules in Nemo must provide the functionality listed in this section. (Note that only some of these functions are useful to a user.)


Developers who are writing their own polynomial module, whether as an interface to a C library, or as some kind of generic module, must provide all of these functions for custom univariate polynomial types in Nemo. 


We write `U` for the type of the polynomials in the polynomial ring and `T` for the type of elements of the coefficient ring.


All of these functions are provided for all existing polynomial types in Nemo.


```
parent_type{U <: PolyElem}(::Type{U})
```


Given the type of polynomial elements, should return the type of the corresponding parent object.


```
elem_type(R::PolyRing)
```


Given a parent object for the polynomial ring, return the type of elements of the polynomial ring.


```
Base.hash(a::PolyElem, h::UInt)
```


Return a `UInt` hexadecimal hash of the polynomial $a$. This should be xor'd with a fixed random hexadecimal specific to the polynomial type. The hash of each coefficient should be xor'd with the supplied parameter `h` as part of computing the hash.


```
fit!(a::PolyElem, n::Int)
```


By reallocating if necessary, ensure that the given polynomial has space for at least $n$ coefficients. This function does not change the length of the polynomial and will only ever increase the number of allocated coefficients. Any coefficients added by this function are initialised to zero.


```
normalise(a::PolyElem, n::Int)
```


Return the normalised length of the given polynomial, assuming its current length is $n$. Its normalised length is such that it either has nonzero leading term or is the zero polynomial. Note that this function doesn't normalise the polynomial. That can be done with a subsequent call to `set_length!` using the length returned by `normalise`.


```
set_length!(a::PolyElem, n::Int)
```


Set the length of an existing polynomial that has sufficient space allocated, i.e. a polynomial for which no reallocation is needed. Note that if the Julia type definition for a custom polynomial type has a field, `length`, which corresponds to the current length of the polynomial, then the developer doesn't need to supply this function, as the supplied generic implementation will work. Note that it can change the length to any value from zero to the number of coefficients currently allocated and initialised.


```
length(a::PolyElem)
```


Return the current length (not the number of allocated coefficients), of the given polynomial. Note that this function only needs to be provided by a developer for a custom polynomial type if the Julia type definition for polynomial elements doesn't contain a field `length` corresponding to the current length of the polynomial. Otherwise the supplied generic implementation will work.


```
coeff(a::PolyElem, n::Int)
```


Return the degree `n` coefficient of the given polynomial. Note coefficients are numbered from `n = 0` for the constant coefficient. If $n$ is bigger then the degree of the polynomial, the function returns a zero coefficient. We require $n \geq 0$. 


```
setcoeff!{T <: RingElem}(a::PolyElem{T}, n::Int, c::T)
```


Set the coefficient of the degree $n$ term of the given polynomial to the given value `a`. The polynomial is not normalised automatically after this operation, however the polynomial is automatically resized if there is not sufficient allocated space.


```
deepcopy(a::PolyElem)
```


Construct a copy of the given polynomial and return it. This function must recursively construct copies of all of the internal data in the given polynomial. Nemo polynomials are mutable and so returning shallow copies is not sufficient.


```
mul!(c::PolyElem, a::PolyElem, b::PolyElem)
```


Multiply $a$ by $b$ and set the existing polynomial $c$ to the result. This function is provided for performance reasons as it saves allocating a new object for the result and eliminates associated garbage collection.


```
addeq!(c::PolyElem, a::PolyElem)
```


In-place addition. Adds $a$ to $c$ and sets $c$ to the result. This function is provided for performance reasons as it saves allocating a new object for the result and eliminates associated garbage collection.


Given a parent object `S` for a polynomial ring, the following coercion functions are provided to coerce various elements into the polynomial ring. Developers provide these by overloading the `call` operator for the polynomial parent objects.


```
S()
```


Coerce zero into the ring $S$.


```
S(n::Integer)
S(n::fmpz)
```


Coerce an integer value or Flint integer into the polynomial ring $S$.


```
S(n::T)
```


Coerces an element of the base ring, of type `T` into $S$.


```
S(A::Array{T, 1})
```


Take an array of elements in the base ring, of type `T` and construct the polynomial with those coefficients, starting with the constant coefficient.


```
S(f::PolyElem)
```


Take a polynomial that is already in the ring $S$ and simply return it. A copy of the original is not made.


```
S(c::RingElem)
```


Try to coerce the given ring element into the polynomial ring. This only succeeds if $c$ can be coerced into the base ring.


In addition to the above, developers of custom polynomials must ensure the parent object of a polynomial type constains a field `base_ring` specifying the base ring, a field `S` containing a symbol (not a string) representing the variable name of the polynomial ring. They must also ensure that each polynomial element contains a field `parent` specifying the parent object of the polynomial.


Typically a developer will also overload the `PolynomialRing` generic function to create polynomials of the custom type they are implementing.


<a id='Basic-manipulation-1'></a>

## Basic manipulation


Numerous functions are provided to manipulate polynomials and to set and retrieve coefficients and other basic data associated with the polynomials. Also see the section on basic functionality above.

<a id='Nemo.base_ring-Tuple{Nemo.PolyRing{T}}' href='#Nemo.base_ring-Tuple{Nemo.PolyRing{T}}'>#</a>
**`Nemo.base_ring`** &mdash; *Method*.



```
base_ring(R::PolyRing)
```

> Return the base ring of the given polynomial ring.


<a id='Nemo.base_ring-Tuple{Nemo.PolyElem{T}}' href='#Nemo.base_ring-Tuple{Nemo.PolyElem{T}}'>#</a>
**`Nemo.base_ring`** &mdash; *Method*.



```
base_ring(a::PolyElem)
```

> Return the base ring of the polynomial ring of the given polynomial.


<a id='Base.parent-Tuple{Nemo.PolyElem{T}}' href='#Base.parent-Tuple{Nemo.PolyElem{T}}'>#</a>
**`Base.parent`** &mdash; *Method*.



```
parent(a::PolyElem)
```

> Return the parent of the given polynomial.


<a id='Base.var-Tuple{Nemo.PolyRing{T}}' href='#Base.var-Tuple{Nemo.PolyRing{T}}'>#</a>
**`Base.var`** &mdash; *Method*.



```
var(a::PolyRing)
```

> Return the internal name of the generator of the polynomial ring. Note that this is returned as a `Symbol` not a `String`.


<a id='Nemo.degree-Tuple{Nemo.PolyElem{T}}' href='#Nemo.degree-Tuple{Nemo.PolyElem{T}}'>#</a>
**`Nemo.degree`** &mdash; *Method*.



```
degree(a::PolyElem)
```

> Return the degree of the given polynomial. This is defined to be one less than the length, even for constant polynomials.


<a id='Nemo.modulus-Tuple{Nemo.PolyElem{T<:Nemo.ResElem{T}}}' href='#Nemo.modulus-Tuple{Nemo.PolyElem{T<:Nemo.ResElem{T}}}'>#</a>
**`Nemo.modulus`** &mdash; *Method*.



```
modulus{T <: ResElem}(a::PolyElem{T})
```

> Return the modulus of the coefficients of the given polynomial.


<a id='Nemo.lead-Tuple{Nemo.PolyElem{T}}' href='#Nemo.lead-Tuple{Nemo.PolyElem{T}}'>#</a>
**`Nemo.lead`** &mdash; *Method*.



```
lead(x::PolyElem)
```

> Return the leading coefficient of the given polynomial. This will be the nonzero coefficient of the term with highest degree unless the polynomial in the zero polynomial, in which case a zero coefficient is returned.


<a id='Nemo.iszero-Tuple{Nemo.PolyElem{T}}' href='#Nemo.iszero-Tuple{Nemo.PolyElem{T}}'>#</a>
**`Nemo.iszero`** &mdash; *Method*.



```
iszero(a::PolyElem)
```

> Return `true` if the given polynomial is zero, otherwise return `false`.


<a id='Nemo.isone-Tuple{Nemo.PolyElem{T}}' href='#Nemo.isone-Tuple{Nemo.PolyElem{T}}'>#</a>
**`Nemo.isone`** &mdash; *Method*.



```
isone(a::PolyElem)
```

> Return `true` if the given polynomial is the constant polynomial $1$, otherwise return `false`.


<a id='Nemo.isgen-Tuple{Nemo.PolyElem{T}}' href='#Nemo.isgen-Tuple{Nemo.PolyElem{T}}'>#</a>
**`Nemo.isgen`** &mdash; *Method*.



```
isgen(a::PolyElem)
```

> Return `true` if the given polynomial is the constant generator of its polynomial ring, otherwise return `false`.


<a id='Nemo.isunit-Tuple{Nemo.PolyElem{T}}' href='#Nemo.isunit-Tuple{Nemo.PolyElem{T}}'>#</a>
**`Nemo.isunit`** &mdash; *Method*.



```
isunit(a::PolyElem)
```

> Return `true` if the given polynomial is a unit in its polynomial ring, otherwise return `false`.


<a id='Base.den-Tuple{Nemo.fmpq_poly}' href='#Base.den-Tuple{Nemo.fmpq_poly}'>#</a>
**`Base.den`** &mdash; *Method*.



```
den(a::fmpq_poly)
```

> Return the least common denominator of the coefficients of the polynomial $a$.



Here are some examples of basic manipulation of polynomials.


```
R, x = PolynomialRing(ZZ, "x")
S, y = PolynomialRing(R, "y")
T, z = PolynomialRing(QQ, "z")

a = zero(S)
b = one(S)

c = ZZ(1)//2*z^2 + ZZ(1)//3
d = x*y^2 + (x + 1)*y + 3

U = base_ring(S)
V = base_ring(y + 1)
v = var(S)
T = parent(y + 1)

f = lead(d)

g = isgen(y)
h = isone(b)
k = iszero(a)
m = isunit(b)
n = degree(d)
p = length(b)
q = den(c)
```


<a id='Arithmetic-operators-1'></a>

## Arithmetic operators


All the usual arithmetic operators are overloaded for Nemo polynomials. Note that Julia uses the single slash for floating point division. Therefore to perform exact division in a ring we use `divexact`. To construct an element of a fraction field one can use the double slash operator `//`.

<a id='Base.--Tuple{Nemo.PolyElem{T}}' href='#Base.--Tuple{Nemo.PolyElem{T}}'>#</a>
**`Base.-`** &mdash; *Method*.



```
-(a::PolyElem)
```

> Return $-a$.


```
-(x)
```

Unary minus operator.

<a id='Base.+-Tuple{Nemo.PolyElem{T<:Nemo.RingElem},Nemo.PolyElem{T<:Nemo.RingElem}}' href='#Base.+-Tuple{Nemo.PolyElem{T<:Nemo.RingElem},Nemo.PolyElem{T<:Nemo.RingElem}}'>#</a>
**`Base.+`** &mdash; *Method*.



```
+{T <: RingElem}(a::PolyElem{T}, b::PolyElem{T})
```

> Return $a + b$.


```
+{T <: RingElem}(a::T, b::PolyElem{T})
```

> Return $a + b$.


```
+{T <: RingElem}(a::PolyElem{T}, b::T)
```

> Return $a + b$.


<a id='Base.--Tuple{Nemo.PolyElem{T<:Nemo.RingElem},Nemo.PolyElem{T<:Nemo.RingElem}}' href='#Base.--Tuple{Nemo.PolyElem{T<:Nemo.RingElem},Nemo.PolyElem{T<:Nemo.RingElem}}'>#</a>
**`Base.-`** &mdash; *Method*.



```
-{T <: RingElem}(a::PolyElem{T}, b::PolyElem{T})
```

> Return $a - b$.


```
-{T <: RingElem}(a::PolyElem{T}, b::T)
```

> Return $a - b$.


```
-{T <: RingElem}(a::T, b::PolyElem{T})
```

> Return $a - b$.


```
-(x, y)
```

Subtraction operator.

<a id='Base.*-Tuple{Nemo.PolyElem{T<:Nemo.RingElem},Nemo.PolyElem{T<:Nemo.RingElem}}' href='#Base.*-Tuple{Nemo.PolyElem{T<:Nemo.RingElem},Nemo.PolyElem{T<:Nemo.RingElem}}'>#</a>
**`Base.*`** &mdash; *Method*.



```
*{T <: RingElem}(a::PolyElem{T}, b::PolyElem{T})
```

> Return $a\times b$.


```
*{T <: RingElem}(a::T, b::PolyElem{T})
```

> Return $a\times b$.


```
*{T <: RingElem}(a::PolyElem{T}, b::T)
```

> Return $a\times b$.


```
*(x, y...)
```

Multiplication operator. `x*y*z*...` calls this function with all arguments, i.e. `*(x, y, z, ...)`.

<a id='Nemo.divexact-Tuple{Nemo.PolyElem{T<:Nemo.RingElem},Nemo.PolyElem{T<:Nemo.RingElem}}' href='#Nemo.divexact-Tuple{Nemo.PolyElem{T<:Nemo.RingElem},Nemo.PolyElem{T<:Nemo.RingElem}}'>#</a>
**`Nemo.divexact`** &mdash; *Method*.



```
divexact{T <: RingElem}(a::PolyElem{T}, b::PolyElem{T})
```

> Return $a/b$ where the quotient is expected to be exact.


```
divexact{T <: RingElem}(a::PolyElem{T}, b::T)
```

> Return $a/b$ where the quotient is expected to be exact.



The following ad hoc operators are also provided.

<a id='Base.+-Tuple{Integer,Nemo.PolyElem{T}}' href='#Base.+-Tuple{Integer,Nemo.PolyElem{T}}'>#</a>
**`Base.+`** &mdash; *Method*.



```
+(a::Integer, b::PolyElem)
```

> Return $a + b$.


<a id='Base.+-Tuple{Nemo.PolyElem{T},Integer}' href='#Base.+-Tuple{Nemo.PolyElem{T},Integer}'>#</a>
**`Base.+`** &mdash; *Method*.



```
+(a::PolyElem, b::Integer)
```

> Return $a + b$.


<a id='Base.+-Tuple{Nemo.fmpz,Nemo.PolyElem{T}}' href='#Base.+-Tuple{Nemo.fmpz,Nemo.PolyElem{T}}'>#</a>
**`Base.+`** &mdash; *Method*.



```
+(a::fmpz, b::PolyElem)
```

> Return $a + b$.


<a id='Base.+-Tuple{Nemo.PolyElem{T},Nemo.fmpz}' href='#Base.+-Tuple{Nemo.PolyElem{T},Nemo.fmpz}'>#</a>
**`Base.+`** &mdash; *Method*.



```
+(a::PolyElem, b::fmpz)
```

> Return $a + b$.


<a id='Base.+-Tuple{T<:Nemo.RingElem,Nemo.PolyElem{T<:Nemo.RingElem}}' href='#Base.+-Tuple{T<:Nemo.RingElem,Nemo.PolyElem{T<:Nemo.RingElem}}'>#</a>
**`Base.+`** &mdash; *Method*.



```
+{T <: RingElem}(a::T, b::PolyElem{T})
```

> Return $a + b$.


<a id='Base.+-Tuple{Nemo.PolyElem{T<:Nemo.RingElem},T<:Nemo.RingElem}' href='#Base.+-Tuple{Nemo.PolyElem{T<:Nemo.RingElem},T<:Nemo.RingElem}'>#</a>
**`Base.+`** &mdash; *Method*.



```
+{T <: RingElem}(a::PolyElem{T}, b::T)
```

> Return $a + b$.


<a id='Base.--Tuple{Integer,Nemo.PolyElem{T}}' href='#Base.--Tuple{Integer,Nemo.PolyElem{T}}'>#</a>
**`Base.-`** &mdash; *Method*.



```
-(a::Integer, b::PolyElem)
```

> Return $a - b$.


```
-(x, y)
```

Subtraction operator.

<a id='Base.--Tuple{Nemo.PolyElem{T},Integer}' href='#Base.--Tuple{Nemo.PolyElem{T},Integer}'>#</a>
**`Base.-`** &mdash; *Method*.



```
-(a::PolyElem, b::Integer)
```

> Return $a - b$.


```
-(x, y)
```

Subtraction operator.

<a id='Base.--Tuple{Nemo.fmpz,Nemo.PolyElem{T}}' href='#Base.--Tuple{Nemo.fmpz,Nemo.PolyElem{T}}'>#</a>
**`Base.-`** &mdash; *Method*.



```
-(a::fmpz, b::PolyElem)
```

> Return $a - b$.


```
-(x, y)
```

Subtraction operator.

<a id='Base.--Tuple{Nemo.PolyElem{T},Nemo.fmpz}' href='#Base.--Tuple{Nemo.PolyElem{T},Nemo.fmpz}'>#</a>
**`Base.-`** &mdash; *Method*.



```
-(a::PolyElem, b::fmpz)
```

> Return $a - b$.


```
-(x, y)
```

Subtraction operator.

<a id='Base.--Tuple{T<:Nemo.RingElem,Nemo.PolyElem{T<:Nemo.RingElem}}' href='#Base.--Tuple{T<:Nemo.RingElem,Nemo.PolyElem{T<:Nemo.RingElem}}'>#</a>
**`Base.-`** &mdash; *Method*.



```
-{T <: RingElem}(a::T, b::PolyElem{T})
```

> Return $a - b$.


```
-(x, y)
```

Subtraction operator.

<a id='Base.--Tuple{Nemo.PolyElem{T<:Nemo.RingElem},T<:Nemo.RingElem}' href='#Base.--Tuple{Nemo.PolyElem{T<:Nemo.RingElem},T<:Nemo.RingElem}'>#</a>
**`Base.-`** &mdash; *Method*.



```
-{T <: RingElem}(a::PolyElem{T}, b::T)
```

> Return $a - b$.


```
-(x, y)
```

Subtraction operator.

<a id='Base.*-Tuple{Integer,Nemo.PolyElem{T}}' href='#Base.*-Tuple{Integer,Nemo.PolyElem{T}}'>#</a>
**`Base.*`** &mdash; *Method*.



```
*(a::Integer, b::PolyElem)
```

> Return $a\times b$.


```
*(x, y...)
```

Multiplication operator. `x*y*z*...` calls this function with all arguments, i.e. `*(x, y, z, ...)`.

<a id='Base.*-Tuple{Nemo.PolyElem{T},Integer}' href='#Base.*-Tuple{Nemo.PolyElem{T},Integer}'>#</a>
**`Base.*`** &mdash; *Method*.



```
*(a::PolyElem, b::Integer)
```

> Return $a\times b$.


```
*(x, y...)
```

Multiplication operator. `x*y*z*...` calls this function with all arguments, i.e. `*(x, y, z, ...)`.

<a id='Base.*-Tuple{Nemo.fmpz,Nemo.PolyElem{T}}' href='#Base.*-Tuple{Nemo.fmpz,Nemo.PolyElem{T}}'>#</a>
**`Base.*`** &mdash; *Method*.



```
*(a::fmpz, b::PolyElem)
```

> Return $a\times b$.


```
*(x, y...)
```

Multiplication operator. `x*y*z*...` calls this function with all arguments, i.e. `*(x, y, z, ...)`.

<a id='Base.*-Tuple{Nemo.PolyElem{T},Nemo.fmpz}' href='#Base.*-Tuple{Nemo.PolyElem{T},Nemo.fmpz}'>#</a>
**`Base.*`** &mdash; *Method*.



```
*(a::PolyElem, b::fmpz)
```

> Return $a\times b$.


```
*(x, y...)
```

Multiplication operator. `x*y*z*...` calls this function with all arguments, i.e. `*(x, y, z, ...)`.

<a id='Base.*-Tuple{T<:Nemo.RingElem,Nemo.PolyElem{T<:Nemo.RingElem}}' href='#Base.*-Tuple{T<:Nemo.RingElem,Nemo.PolyElem{T<:Nemo.RingElem}}'>#</a>
**`Base.*`** &mdash; *Method*.



```
*{T <: RingElem}(a::T, b::PolyElem{T})
```

> Return $a\times b$.


```
*(x, y...)
```

Multiplication operator. `x*y*z*...` calls this function with all arguments, i.e. `*(x, y, z, ...)`.

<a id='Base.*-Tuple{Nemo.PolyElem{T<:Nemo.RingElem},T<:Nemo.RingElem}' href='#Base.*-Tuple{Nemo.PolyElem{T<:Nemo.RingElem},T<:Nemo.RingElem}'>#</a>
**`Base.*`** &mdash; *Method*.



```
*{T <: RingElem}(a::PolyElem{T}, b::T)
```

> Return $a\times b$.


```
*(x, y...)
```

Multiplication operator. `x*y*z*...` calls this function with all arguments, i.e. `*(x, y, z, ...)`.

<a id='Nemo.divexact-Tuple{Nemo.PolyElem{T},Integer}' href='#Nemo.divexact-Tuple{Nemo.PolyElem{T},Integer}'>#</a>
**`Nemo.divexact`** &mdash; *Method*.



```
divexact(a::PolyElem, b::Integer)
```

> Return $a/b$ where the quotient is expected to be exact.


<a id='Nemo.divexact-Tuple{Nemo.PolyElem{T},Nemo.fmpz}' href='#Nemo.divexact-Tuple{Nemo.PolyElem{T},Nemo.fmpz}'>#</a>
**`Nemo.divexact`** &mdash; *Method*.



```
divexact(a::PolyElem, b::fmpz)
```

> Return $a/b$ where the quotient is expected to be exact.


<a id='Nemo.divexact-Tuple{Nemo.PolyElem{T<:Nemo.RingElem},T<:Nemo.RingElem}' href='#Nemo.divexact-Tuple{Nemo.PolyElem{T<:Nemo.RingElem},T<:Nemo.RingElem}'>#</a>
**`Nemo.divexact`** &mdash; *Method*.



```
divexact{T <: RingElem}(a::PolyElem{T}, b::T)
```

> Return $a/b$ where the quotient is expected to be exact.


<a id='Base.^-Tuple{Nemo.PolyElem{T},Int64}' href='#Base.^-Tuple{Nemo.PolyElem{T},Int64}'>#</a>
**`Base.^`** &mdash; *Method*.



```
^(x, y)
```

Exponentiation operator.


If the appropriate `promote_rule` and coercion exists, these operators can also be used with elements of other rings. Nemo will try to coerce the operands to the dominating type and then apply the operator.


Here are some examples of arithmetic operations on polynomials.


```
R, x = PolynomialRing(ZZ, "x")
S, y = PolynomialRing(R, "y")

f = x*y^2 + (x + 1)*y + 3
g = (x + 1)*y + (x^3 + 2x + 2)

h = f - g
k = f*g
m = f + g
n = g - 4
p = fmpz(5) - g
q = f*7
r = divexact(f, -1)
s = divexact(g*(x + 1), x + 1)
t = f^3
```


<a id='Comparison-operators-1'></a>

## Comparison operators


The following comparison operators are implemented for polynomials in Nemo.

<a id='Base.==-Tuple{Nemo.PolyElem{T<:Nemo.RingElem},Nemo.PolyElem{T<:Nemo.RingElem}}' href='#Base.==-Tuple{Nemo.PolyElem{T<:Nemo.RingElem},Nemo.PolyElem{T<:Nemo.RingElem}}'>#</a>
**`Base.==`** &mdash; *Method*.



```
=={T <: RingElem}(x::PolyElem{T}, y::PolyElem{T})
```

> Return `true` if $x == y$ arithmetically, otherwise return `false`. Recall that power series to different precisions may still be arithmetically equal to the minimum of the two precisions.


```
=={T <: RingElem}(x::PolyElem{T}, y::T)
```

> Return `true` if $x == y$ arithmetically, otherwise return `false`.


```
=={T <: RingElem}(x::T, y::PolyElem{T})
```

> Return `true` if $x == y$ arithmetically, otherwise return `false`.


<a id='Base.isequal-Tuple{Nemo.PolyElem{T<:Nemo.RingElem},Nemo.PolyElem{T<:Nemo.RingElem}}' href='#Base.isequal-Tuple{Nemo.PolyElem{T<:Nemo.RingElem},Nemo.PolyElem{T<:Nemo.RingElem}}'>#</a>
**`Base.isequal`** &mdash; *Method*.



```
isequal{T <: RingElem}(x::PolyElem{T}, y::PolyElem{T})
```

> Return `true` if $x == y$ exactly, otherwise return `false`. This function is useful in cases where the coefficients of the polynomial are inexact, e.g. power series. Only if the power series are precisely the same, to the same precision, are they declared equal by this function.



In addition we have the following ad hoc comparison operators.

<a id='Base.==-Tuple{Nemo.PolyElem{T<:Nemo.RingElem},T<:Nemo.RingElem}' href='#Base.==-Tuple{Nemo.PolyElem{T<:Nemo.RingElem},T<:Nemo.RingElem}'>#</a>
**`Base.==`** &mdash; *Method*.



```
=={T <: RingElem}(x::PolyElem{T}, y::T)
```

> Return `true` if $x == y$ arithmetically, otherwise return `false`.


<a id='Base.==-Tuple{T<:Nemo.RingElem,Nemo.PolyElem{T<:Nemo.RingElem}}' href='#Base.==-Tuple{T<:Nemo.RingElem,Nemo.PolyElem{T<:Nemo.RingElem}}'>#</a>
**`Base.==`** &mdash; *Method*.



```
=={T <: RingElem}(x::T, y::PolyElem{T})
```

> Return `true` if $x == y$ arithmetically, otherwise return `false`.


<a id='Base.==-Tuple{Nemo.PolyElem{T},Integer}' href='#Base.==-Tuple{Nemo.PolyElem{T},Integer}'>#</a>
**`Base.==`** &mdash; *Method*.



```
==(x::PolyElem, y::Integer)
```

> Return `true` if $x == y$ arithmetically, otherwise return `false`.


<a id='Base.==-Tuple{Integer,Nemo.PolyElem{T}}' href='#Base.==-Tuple{Integer,Nemo.PolyElem{T}}'>#</a>
**`Base.==`** &mdash; *Method*.



```
==(x::Integer, y::PolyElem)
```

> Return `true` if $x == y$ arithmetically, otherwise return `false`.


<a id='Base.==-Tuple{Nemo.PolyElem{T},Nemo.fmpz}' href='#Base.==-Tuple{Nemo.PolyElem{T},Nemo.fmpz}'>#</a>
**`Base.==`** &mdash; *Method*.



```
==(x::PolyElem, y::fmpz)
```

> Return `true` if $x == y$ arithmetically, otherwise return `false`.


<a id='Base.==-Tuple{Nemo.fmpz,Nemo.PolyElem{T}}' href='#Base.==-Tuple{Nemo.fmpz,Nemo.PolyElem{T}}'>#</a>
**`Base.==`** &mdash; *Method*.



```
==(x::fmpz, y::PolyElem)
```

> Return `true` if $x == y$ arithmetically, otherwise return `false`.



Here are some examples of comparisons.


```
R, x = PolynomialRing(ZZ, "x")
S, y = PolynomialRing(R, "y")

f = x*y^2 + (x + 1)*y + 3
g = x*y^2 + (x + 1)*y + 3
h = S(3)

f == g
isequal(f, g)
f != 3
g != x
h == fmpz(3)
```


<a id='Truncation-1'></a>

## Truncation

<a id='Base.truncate-Tuple{Nemo.PolyElem{T},Int64}' href='#Base.truncate-Tuple{Nemo.PolyElem{T},Int64}'>#</a>
**`Base.truncate`** &mdash; *Method*.



```
truncate(a::PolyElem, n::Int)
```

> Return $a$ truncated to $n$ terms.


<a id='Nemo.mullow-Tuple{Nemo.PolyElem{T<:Nemo.RingElem},Nemo.PolyElem{T<:Nemo.RingElem},Int64}' href='#Nemo.mullow-Tuple{Nemo.PolyElem{T<:Nemo.RingElem},Nemo.PolyElem{T<:Nemo.RingElem},Int64}'>#</a>
**`Nemo.mullow`** &mdash; *Method*.



```
mullow{T <: RingElem}(a::PolyElem{T}, b::PolyElem{T}, n::Int)
```

> Return $a\times b$ truncated to $n$ terms.



Here are some examples of truncated operations.


```
R, x = PolynomialRing(ZZ, "x")
S, y = PolynomialRing(R, "y")

f = x*y^2 + (x + 1)*y + 3
g = (x + 1)*y + (x^3 + 2x + 2)

h = truncate(f, 1)
k = mullow(f, g, 4)
```


<a id='Reversal-1'></a>

## Reversal

<a id='Base.reverse-Tuple{Nemo.PolyElem{T},Int64}' href='#Base.reverse-Tuple{Nemo.PolyElem{T},Int64}'>#</a>
**`Base.reverse`** &mdash; *Method*.



```
reverse(x::PolyElem, len::Int)
```

> Return the reverse of the polynomial $x$, thought of as a polynomial of the given length (the polynomial will be notionally truncated or padded with zeroes before the leading term if necessary to match the specified length).  The resulting polynomial is normalised. If `len` is negative we throw a `DomainError()`.


<a id='Base.reverse-Tuple{Nemo.PolyElem{T}}' href='#Base.reverse-Tuple{Nemo.PolyElem{T}}'>#</a>
**`Base.reverse`** &mdash; *Method*.



```
reverse(x::PolyElem)
```

> Return the reverse of the polynomial $x$, i.e. the leading coefficient of $x$ becomes the constant coefficient of the result, etc. The resulting polynomial is normalised.



Here are some examples of reversal.


```
R, x = PolynomialRing(ZZ, "x")
S, y = PolynomialRing(R, "y")

f = x*y^2 + (x + 1)*y + 3

g = reverse(f, 7)
h = reverse(f)
```


<a id='Shifting-1'></a>

## Shifting

<a id='Nemo.shift_left-Tuple{Nemo.PolyElem{T},Int64}' href='#Nemo.shift_left-Tuple{Nemo.PolyElem{T},Int64}'>#</a>
**`Nemo.shift_left`** &mdash; *Method*.



```
shift_left(x::PolyElem, n::Int)
```

> Return the polynomial $f$ shifted left by $n$ terms, i.e. multiplied by $x^n$.


<a id='Nemo.shift_right-Tuple{Nemo.PolyElem{T},Int64}' href='#Nemo.shift_right-Tuple{Nemo.PolyElem{T},Int64}'>#</a>
**`Nemo.shift_right`** &mdash; *Method*.



```
shift_right(f::PolyElem, n::Int)
```

> Return the polynomial $f$ shifted right by $n$ terms, i.e. divided by $x^n$.



Here are some examples of shifting.


```
R, x = PolynomialRing(ZZ, "x")
S, y = PolynomialRing(R, "y")

f = x*y^2 + (x + 1)*y + 3

g = shift_left(f, 7)
h = shift_right(f, 2)
```


<a id='Modulo-arithmetic-1'></a>

## Modulo arithmetic


For polynomials over a field or residue ring, we can reduce modulo a given polynomial. This isn't always well-defined in the case of a residue ring, but when it is well-defined, we obtain the correct result. If Nemo encounters an impossible inverse, an exception will be raised.

<a id='Nemo.mulmod-Tuple{Nemo.PolyElem{T<:Union{Nemo.FieldElem,Nemo.ResElem{T}}},Nemo.PolyElem{T<:Union{Nemo.FieldElem,Nemo.ResElem{T}}},Nemo.PolyElem{T<:Union{Nemo.FieldElem,Nemo.ResElem{T}}}}' href='#Nemo.mulmod-Tuple{Nemo.PolyElem{T<:Union{Nemo.FieldElem,Nemo.ResElem{T}}},Nemo.PolyElem{T<:Union{Nemo.FieldElem,Nemo.ResElem{T}}},Nemo.PolyElem{T<:Union{Nemo.FieldElem,Nemo.ResElem{T}}}}'>#</a>
**`Nemo.mulmod`** &mdash; *Method*.



```
mulmod{T <: Union{ResElem, FieldElem}}(a::PolyElem{T}, b::PolyElem{T}, d::PolyElem{T})
```

> Return $a\times b \pmod{d}$.


<a id='Nemo.powmod-Tuple{Nemo.PolyElem{T<:Union{Nemo.FieldElem,Nemo.ResElem{T}}},Int64,Nemo.PolyElem{T<:Union{Nemo.FieldElem,Nemo.ResElem{T}}}}' href='#Nemo.powmod-Tuple{Nemo.PolyElem{T<:Union{Nemo.FieldElem,Nemo.ResElem{T}}},Int64,Nemo.PolyElem{T<:Union{Nemo.FieldElem,Nemo.ResElem{T}}}}'>#</a>
**`Nemo.powmod`** &mdash; *Method*.



```
powmod{T <: Union{ResElem, FieldElem}}(a::PolyElem{T}, b::Int, d::PolyElem{T})
```

> Return $a^b \pmod{d}$. There are no restrictions on $b$.


<a id='Nemo.powmod-Tuple{Nemo.fmpz_mod_poly,Nemo.fmpz,Nemo.fmpz_mod_poly}' href='#Nemo.powmod-Tuple{Nemo.fmpz_mod_poly,Nemo.fmpz,Nemo.fmpz_mod_poly}'>#</a>
**`Nemo.powmod`** &mdash; *Method*.



```
powmod(x::fmpz_mod_poly, e::fmpz, y::fmpz_mod_poly)
```

> Return $x^e \pmod{y}$.


<a id='Base.invmod-Tuple{Nemo.PolyElem{T<:Union{Nemo.FieldElem,Nemo.ResElem{T}}},Nemo.PolyElem{T<:Union{Nemo.FieldElem,Nemo.ResElem{T}}}}' href='#Base.invmod-Tuple{Nemo.PolyElem{T<:Union{Nemo.FieldElem,Nemo.ResElem{T}}},Nemo.PolyElem{T<:Union{Nemo.FieldElem,Nemo.ResElem{T}}}}'>#</a>
**`Base.invmod`** &mdash; *Method*.



```
invmod{T <: Union{ResElem, FieldElem}}(a::PolyElem{T}, b::PolyElem{T})
```

> Return $a^{-1} \pmod{d}$.



Here are some examples of modular arithmetic.


```
R, x = PolynomialRing(QQ, "x")
S = ResidueRing(R, x^3 + 3x + 1)
T, y = PolynomialRing(S, "y")

f = (3*x^2 + x + 2)*y + x^2 + 1
g = (5*x^2 + 2*x + 1)*y^2 + 2x*y + x + 1
h = (3*x^3 + 2*x^2 + x + 7)*y^5 + 2x*y + 1

invmod(f, g)
mulmod(f, g, h)
powmod(f, 3, h)
```


<a id='Euclidean-division-1'></a>

## Euclidean division


For polynomials over a field, we have a euclidean domain, and in many cases for polynomials over a residue ring things behave as though we had a euclidean domain so long as we don't hit an impossible inverse. For such rings we define euclidean division of polynomials. If an impossible inverse is hit, we raise an exception.

<a id='Base.mod-Tuple{Nemo.PolyElem{T<:Union{Nemo.FieldElem,Nemo.ResElem{T}}},Nemo.PolyElem{T<:Union{Nemo.FieldElem,Nemo.ResElem{T}}}}' href='#Base.mod-Tuple{Nemo.PolyElem{T<:Union{Nemo.FieldElem,Nemo.ResElem{T}}},Nemo.PolyElem{T<:Union{Nemo.FieldElem,Nemo.ResElem{T}}}}'>#</a>
**`Base.mod`** &mdash; *Method*.



```
mod{T <: Union{ResElem, FieldElem}}(f::PolyElem{T}, g::PolyElem{T})
```

> Return $f \pmod{g}$.


<a id='Base.divrem-Tuple{Nemo.PolyElem{T<:Union{Nemo.FieldElem,Nemo.ResElem{T}}},Nemo.PolyElem{T<:Union{Nemo.FieldElem,Nemo.ResElem{T}}}}' href='#Base.divrem-Tuple{Nemo.PolyElem{T<:Union{Nemo.FieldElem,Nemo.ResElem{T}}},Nemo.PolyElem{T<:Union{Nemo.FieldElem,Nemo.ResElem{T}}}}'>#</a>
**`Base.divrem`** &mdash; *Method*.



```
divrem{T <: Union{ResElem, FieldElem}}(f::PolyElem{T}, g::PolyElem{T})
```

> Return a tuple $(q, r)$ such that $f = qg + r$ where $q$ is the euclidean quotient of $f$ by $g$.



Here are some examples of euclidean division.


```
R = ResidueRing(ZZ, 7)
S, x = PolynomialRing(R, "x")
T = ResidueRing(S, x^3 + 3x + 1)
U, y = PolynomialRing(T, "y")

f = y^3 + x*y^2 + (x + 1)*y + 3
g = (x + 1)*y^2 + (x^3 + 2x + 2)

h = mod(f, g)
q, r = divrem(f, g)
```


<a id='Pseudodivision-1'></a>

## Pseudodivision


Given two polynomials $a, b$, pseudodivision computes polynomials $q$ and $r$ with length$(r) <$ length$(b)$ such that $L^d a = bq + r,$ where $d =$ length$(a) -$ length$(b) + 1$ and $L$ is the leading coefficient of $b$.


We call $q$ the pseudoquotient and $r$ the pseudoremainder.

<a id='Nemo.pseudorem-Tuple{Nemo.PolyElem{T<:Nemo.RingElem},Nemo.PolyElem{T<:Nemo.RingElem}}' href='#Nemo.pseudorem-Tuple{Nemo.PolyElem{T<:Nemo.RingElem},Nemo.PolyElem{T<:Nemo.RingElem}}'>#</a>
**`Nemo.pseudorem`** &mdash; *Method*.



```
pseudorem{T <: RingElem}(f::PolyElem{T}, g::PolyElem{T})
```

> Return the pseudoremainder of $a$ divided by $b$. If $b = 0$ we throw a  `DivideError()`.


<a id='Nemo.pseudodivrem-Tuple{Nemo.PolyElem{T<:Nemo.RingElem},Nemo.PolyElem{T<:Nemo.RingElem}}' href='#Nemo.pseudodivrem-Tuple{Nemo.PolyElem{T<:Nemo.RingElem},Nemo.PolyElem{T<:Nemo.RingElem}}'>#</a>
**`Nemo.pseudodivrem`** &mdash; *Method*.



```
pseudodivrem{T <: RingElem}(f::PolyElem{T}, g::PolyElem{T})
```

> Return a tuple $(q, r)$ consisting of the pseudoquotient and pseudoremainder  of $a$ divided by $b$. If $b = 0$ we throw a `DivideError()`.



Here are some examples of pseudodivision.


```
R, x = PolynomialRing(ZZ, "x")
S, y = PolynomialRing(R, "y")

f = x*y^2 + (x + 1)*y + 3
g = (x + 1)*y + (x^3 + 2x + 2)

h = pseudorem(f, g)
q, r = pseudodivrem(f, g)
```


<a id='Content,-primitive-part,-GCD-and-LCM-1'></a>

## Content, primitive part, GCD and LCM


In Nemo, we allow computation of the greatest common divisor of polynomials over any ring. This is enabled by making use of pseudoremainders when we aren't working over a euclidean domain or something mimicking such a domain. In certain cases this allows us to return a greatest common divisor when it otherwise wouldn't be possible. However, a greatest common divisor is not necessarily unique, or even well-defined.


If an impossible inverse is encountered whilst computing the greatest common divisor, an exception is thrown.

<a id='Base.gcd-Tuple{Nemo.PolyElem{T<:Nemo.RingElem},Nemo.PolyElem{T<:Nemo.RingElem}}' href='#Base.gcd-Tuple{Nemo.PolyElem{T<:Nemo.RingElem},Nemo.PolyElem{T<:Nemo.RingElem}}'>#</a>
**`Base.gcd`** &mdash; *Method*.



```
gcd{T <: RingElem}(a::PolyElem{T}, b::PolyElem{T})
```

> Return a greatest common divisor of $a$ and $b$ if it exists.


<a id='Base.lcm-Tuple{Nemo.PolyElem{T<:Nemo.RingElem},Nemo.PolyElem{T<:Nemo.RingElem}}' href='#Base.lcm-Tuple{Nemo.PolyElem{T<:Nemo.RingElem},Nemo.PolyElem{T<:Nemo.RingElem}}'>#</a>
**`Base.lcm`** &mdash; *Method*.



```
lcm{T <: RingElem}(a::PolyElem{T}, b::PolyElem{T})
```

> Return a least common multiple of $a$ and $b$ if it exists.


<a id='Nemo.content-Tuple{Nemo.PolyElem{T}}' href='#Nemo.content-Tuple{Nemo.PolyElem{T}}'>#</a>
**`Nemo.content`** &mdash; *Method*.



```
content(a::PolyElem)
```

> Return the content of $a$, i.e. the greatest common divisor of its coefficients.


<a id='Nemo.primpart-Tuple{Nemo.PolyElem{T}}' href='#Nemo.primpart-Tuple{Nemo.PolyElem{T}}'>#</a>
**`Nemo.primpart`** &mdash; *Method*.



```
primpart(a::PolyElem)
```

> Return the primitive part of $a$, i.e. the polynomial divided by its content.


<a id='Base.gcdx-Tuple{Nemo.PolyElem{T<:Nemo.RingElem},Nemo.PolyElem{T<:Nemo.RingElem}}' href='#Base.gcdx-Tuple{Nemo.PolyElem{T<:Nemo.RingElem},Nemo.PolyElem{T<:Nemo.RingElem}}'>#</a>
**`Base.gcdx`** &mdash; *Method*.



```
gcdx{T <: RingElem}(a::PolyElem{T}, b::PolyElem{T})
```

> Return a tuple $(r, s, t)$ such that $r$ is the resultant of $a$ and $b$ and such that $r = a\times s + b\times t$.


<a id='Base.gcdx-Tuple{Nemo.PolyElem{T<:Union{Nemo.FieldElem,Nemo.ResElem{T}}},Nemo.PolyElem{T<:Union{Nemo.FieldElem,Nemo.ResElem{T}}}}' href='#Base.gcdx-Tuple{Nemo.PolyElem{T<:Union{Nemo.FieldElem,Nemo.ResElem{T}}},Nemo.PolyElem{T<:Union{Nemo.FieldElem,Nemo.ResElem{T}}}}'>#</a>
**`Base.gcdx`** &mdash; *Method*.



```
gcdx{T <: Union{ResElem, FieldElem}}(a::PolyElem{T}, b::PolyElem{T})
```

> Return a tuple $(g, s, t)$ such that $g$ is the greatest common divisor of $a$ and $b$ and such that $r = a\times s + b\times t$.


```
gcdx{T <: RingElem}(a::PolyElem{T}, b::PolyElem{T})
```

> Return a tuple $(r, s, t)$ such that $r$ is the resultant of $a$ and $b$ and such that $r = a\times s + b\times t$.


<a id='Nemo.gcdinv-Tuple{Nemo.PolyElem{T<:Union{Nemo.FieldElem,Nemo.ResElem{T}}},Nemo.PolyElem{T<:Union{Nemo.FieldElem,Nemo.ResElem{T}}}}' href='#Nemo.gcdinv-Tuple{Nemo.PolyElem{T<:Union{Nemo.FieldElem,Nemo.ResElem{T}}},Nemo.PolyElem{T<:Union{Nemo.FieldElem,Nemo.ResElem{T}}}}'>#</a>
**`Nemo.gcdinv`** &mdash; *Method*.



```
gcdinv{T <: Union{ResElem, FieldElem}}(a::PolyElem{T}, b::PolyElem{T})
```

> Return a tuple $(g, s)$ such that $g$ is the greatest common divisor of $a$ and $b$ and such that $s = a^{-1} \pmod{b}$. This function is useful for inverting modulo a polynomial and checking that it really was invertible.



Here are some examples of content, primitive part and GCD.


```
R, x = PolynomialRing(ZZ, "x")
S, y = PolynomialRing(R, "y")

k = x*y^2 + (x + 1)*y + 3
l = (x + 1)*y + (x^3 + 2x + 2)
m = y^2 + x + 1

n = content(k)
p = primpart(k*(x^2 + 1))
q = gcd(k*m, l*m)
r = lcm(k*m, l*m)

R, x = PolynomialRing(QQ, "x")
T = ResidueRing(R, x^3 + 3x + 1)
U, z = PolynomialRing(T, "z")

g = z^3 + 2z + 1
h = z^5 + 1

r, s, t = gcdx(g, h)
u, v = gcdinv(g, h)
```


<a id='Evaluation,-composition-and-substitution-1'></a>

## Evaluation, composition and substitution

<a id='Nemo.evaluate-Tuple{Nemo.PolyElem{T<:Nemo.RingElem},T<:Nemo.RingElem}' href='#Nemo.evaluate-Tuple{Nemo.PolyElem{T<:Nemo.RingElem},T<:Nemo.RingElem}'>#</a>
**`Nemo.evaluate`** &mdash; *Method*.



```
evaluate{T <: RingElem}(a::PolyElem{T}, b::T)
```

> Evaluate the polynomial $a$ at the value $b$ and return the result.


<a id='Nemo.evaluate-Tuple{Nemo.PolyElem{T},Integer}' href='#Nemo.evaluate-Tuple{Nemo.PolyElem{T},Integer}'>#</a>
**`Nemo.evaluate`** &mdash; *Method*.



```
evaluate(a::PolyElem, b::Integer)
```

> Evaluate the polynomial $a$ at the value $b$ and return the result.


<a id='Nemo.evaluate-Tuple{Nemo.PolyElem{T},Nemo.fmpz}' href='#Nemo.evaluate-Tuple{Nemo.PolyElem{T},Nemo.fmpz}'>#</a>
**`Nemo.evaluate`** &mdash; *Method*.



```
evaluate(a::PolyElem, b::fmpz)
```

> Evaluate the polynomial $a$ at the value $b$ and return the result.


<a id='Nemo.compose-Tuple{Nemo.PolyElem{T},Nemo.PolyElem{T}}' href='#Nemo.compose-Tuple{Nemo.PolyElem{T},Nemo.PolyElem{T}}'>#</a>
**`Nemo.compose`** &mdash; *Method*.



```
compose(a::PolyElem, b::PolyElem)
```

> Compose the polynomial $a$ with the polynomial $b$ and return the result, i.e. return $a\circ b$.


<a id='Nemo.subst-Tuple{Nemo.PolyElem{T<:Nemo.RingElem},Any}' href='#Nemo.subst-Tuple{Nemo.PolyElem{T<:Nemo.RingElem},Any}'>#</a>
**`Nemo.subst`** &mdash; *Method*.



```
subst{T <: RingElem}(f::PolyElem{T}, a::Any)
```

> Evaluate the polynomial $f$ at $a$. Note that $a$ can be anything, whether a ring element or not.



We also overload the functional notation so that the polynomial $f$ can be evaluated at $a$ by writing $f(a)$. This feature is only available with  Julia 0.5 however.


Here are some examples of polynomial evaluation, composition and substitution.


```
R, x = PolynomialRing(ZZ, "x")
S, y = PolynomialRing(R, "y")

f = x*y^2 + (x + 1)*y + 3
g = (x + 1)*y + (x^3 + 2x + 2)
M = R[x + 1 2x; x - 3 2x - 1]

h = evaluate(f, 3)
k = evaluate(f, x^2 + 2x + 1)
m = compose(f, g)
n = subst(f, M)
p = f(M)
k = f(23)
```


<a id='Derivative-and-integral-1'></a>

## Derivative and integral

<a id='Nemo.derivative-Tuple{Nemo.PolyElem{T}}' href='#Nemo.derivative-Tuple{Nemo.PolyElem{T}}'>#</a>
**`Nemo.derivative`** &mdash; *Method*.



```
derivative(a::PolyElem)
```

> Return the derivative of the polynomial $a$.


<a id='Nemo.integral-Tuple{Nemo.PolyElem{T<:Union{Nemo.FieldElem,Nemo.ResElem{T}}}}' href='#Nemo.integral-Tuple{Nemo.PolyElem{T<:Union{Nemo.FieldElem,Nemo.ResElem{T}}}}'>#</a>
**`Nemo.integral`** &mdash; *Method*.



```
integral{T <: Union{ResElem, FieldElem}}(x::PolyElem{T})
```

> Return the integral of the polynomial $a$.



Here are some examples of integral and derivative.


```
R, x = PolynomialRing(ZZ, "x")
S, y = PolynomialRing(R, "y")
T, z = PolynomialRing(QQ, "z")
U = ResidueRing(T, z^3 + 3z + 1)
V, w = PolynomialRing(U, "w")

f = x*y^2 + (x + 1)*y + 3
g = (z^2 + 2z + 1)*w^2 + (z + 1)*w - 2z + 4

h = derivative(f)
k = integral(g)   
```


<a id='Resultant-and-discriminant-1'></a>

## Resultant and discriminant

<a id='Nemo.resultant-Tuple{Nemo.PolyElem{T<:Nemo.RingElem},Nemo.PolyElem{T<:Nemo.RingElem}}' href='#Nemo.resultant-Tuple{Nemo.PolyElem{T<:Nemo.RingElem},Nemo.PolyElem{T<:Nemo.RingElem}}'>#</a>
**`Nemo.resultant`** &mdash; *Method*.



```
resultant{T <: RingElem}(a::PolyElem{T}, b::PolyElem{T})
```

> Return the resultant of the $a$ and $b$.


<a id='Nemo.discriminant-Tuple{Nemo.PolyElem{T}}' href='#Nemo.discriminant-Tuple{Nemo.PolyElem{T}}'>#</a>
**`Nemo.discriminant`** &mdash; *Method*.



```
discriminant(a::PolyElem)
```

> Return the discrimnant of the given polynomial.



Here are some examples of computing the resultant and discriminant.


```
R, x = PolynomialRing(ZZ, "x")
S, y = PolynomialRing(R, "y")

f = 3x*y^2 + (x + 1)*y + 3
g = 6(x + 1)*y + (x^3 + 2x + 2)

h = resultant(f, g)
k = discriminant(f)
```


<a id='Newton-representation-1'></a>

## Newton representation

<a id='Nemo.monomial_to_newton!-Tuple{Array{T<:Nemo.RingElem,1},Array{T<:Nemo.RingElem,1}}' href='#Nemo.monomial_to_newton!-Tuple{Array{T<:Nemo.RingElem,1},Array{T<:Nemo.RingElem,1}}'>#</a>
**`Nemo.monomial_to_newton!`** &mdash; *Method*.



```
monomial_to_newton!{T <: RingElem}(P::Array{T, 1}, roots::Array{T, 1})
```

> Converts a polynomial $p$, given as an array of coefficients, in-place from its coefficients given in the standard monomial basis to the Newton basis for the roots $r_0, r_1, \ldots, r_{n-2}$. In other words, this determines output coefficients $c_i$ such that $c_0 + c_1(x-r_0) + c_2(x-r_0)(x-r_1) + \ldots + c_{n-1}(x-r_0)(x-r_1)\cdots(x-r_{n-2})$ is equal to the input polynomial.


<a id='Nemo.newton_to_monomial!-Tuple{Array{T<:Nemo.RingElem,1},Array{T<:Nemo.RingElem,1}}' href='#Nemo.newton_to_monomial!-Tuple{Array{T<:Nemo.RingElem,1},Array{T<:Nemo.RingElem,1}}'>#</a>
**`Nemo.newton_to_monomial!`** &mdash; *Method*.



```
newton_to_monomial!{T <: RingElem}(P::Array{T, 1}, roots::Array{T, 1})
```

> Converts a polynomial $p$, given as an array of coefficients, in-place from its coefficients given in the Newton basis for the roots $r_0, r_1, \ldots, r_{n-2}$ to the standard monomial basis. In other words, this evaluates $c_0 + c_1(x-r_0) + c_2(x-r_0)(x-r_1) + \ldots + c_{n-1}(x-r_0)(x-r_1)\cdots(x-r_{n-2})$ where $c_i$ are the input coefficients given by $p$.



Here are some examples of conversion to and from Newton representation.


```
R, x = PolynomialRing(ZZ, "x")
S, y = PolynomialRing(R, "y")

f = 3x*y^2 + (x + 1)*y + 3
g = deepcopy(f)
roots = [R(1), R(2), R(3)]

monomial_to_newton!(g.coeffs, roots)
newton_to_monomial!(g.coeffs, roots)
```


<a id='Interpolation-1'></a>

## Interpolation

<a id='Nemo.interpolate-Tuple{Nemo.PolyRing{T},Array{T<:Nemo.RingElem,1},Array{T<:Nemo.RingElem,1}}' href='#Nemo.interpolate-Tuple{Nemo.PolyRing{T},Array{T<:Nemo.RingElem,1},Array{T<:Nemo.RingElem,1}}'>#</a>
**`Nemo.interpolate`** &mdash; *Method*.



```
interpolate{T <: RingElem}(S::PolyRing, x::Array{T, 1}, y::Array{T, 1})
```

> Given two arrays of values $xs$ and $ys$ of the same length $n$, find the polynomial $f$ in the polynomial ring $R$ of length at most $n$ such that $f$ has the value $ys$ at the points $xs$. The values in the arrays $xs$ and $ys$ must belong to the base ring of the polynomial ring $R$.



Here is an example of interpolation.


```
R, x = PolynomialRing(ZZ, "x")
S, y = PolynomialRing(R, "y")

xs = [R(1), R(2), R(3), R(4)]
ys = [R(1), R(4), R(9), R(16)]

f = interpolate(S, xs, ys)
```


<a id='Signature-1'></a>

## Signature


Signature is only available for certain coefficient rings.

<a id='Nemo.signature-Tuple{Nemo.fmpz_poly}' href='#Nemo.signature-Tuple{Nemo.fmpz_poly}'>#</a>
**`Nemo.signature`** &mdash; *Method*.



```
signature(f::fmpz_poly)
```

> Return the signature of the polynomial $f$, i.e. a tuple $(r, s)$ such that $r$ is the number of real roots of $f$ and $s$ is half the number of complex roots.


<a id='Nemo.signature-Tuple{Nemo.fmpq_poly}' href='#Nemo.signature-Tuple{Nemo.fmpq_poly}'>#</a>
**`Nemo.signature`** &mdash; *Method*.



```
signature(f::fmpq_poly)
```

> Return the signature of $f$, i.e. a tuple $(r, s)$ where $r$ is the number of real roots of $f$ and $s$ is half the number of complex roots.



Here is an example of signature.


```
R, x = PolynomialRing(ZZ, "x")

f = x^3 + 3x + 1

(r, s) = signature(f)
```


<a id='Lifting-1'></a>

## Lifting


When working over a residue ring it is useful to be able to lift to the base ring of the residue ring, e.g. from $\mathbb{Z}/n\mathbb{Z}$ to $\mathbb{Z}$.

<a id='Nemo.lift-Tuple{Nemo.FmpzPolyRing,Nemo.nmod_poly}' href='#Nemo.lift-Tuple{Nemo.FmpzPolyRing,Nemo.nmod_poly}'>#</a>
**`Nemo.lift`** &mdash; *Method*.



```
function lift(R::FmpzPolyRing, y::nmod_poly)
```

> Lift from a polynomial over $\mathbb{Z}/n\mathbb{Z}$ to a polynomial over $\mathbb{Z}$ with minimal reduced nonnegative coefficients. The ring `R` specifies the ring to lift into.


<a id='Nemo.lift-Tuple{Nemo.FmpzPolyRing,Nemo.fmpz_mod_poly}' href='#Nemo.lift-Tuple{Nemo.FmpzPolyRing,Nemo.fmpz_mod_poly}'>#</a>
**`Nemo.lift`** &mdash; *Method*.



```
function lift(R::FmpzPolyRing, y::fmpz_mod_poly)
```

> Lift from a polynomial over $\mathbb{Z}/n\mathbb{Z}$ to a polynomial over $\mathbb{Z}$ with minimal reduced nonnegative coefficients. The ring `R` specifies the ring to lift into.



Here is an example of lifting.


```
R = ResidueRing(ZZ, 123456789012345678949)
S, x = PolynomialRing(R, "x")
T, y = PolynomialRing(ZZ, "y")

f = x^2 + 2x + 1

a = lift(T, f)
```


<a id='Factorisation-1'></a>

## Factorisation


Polynomials can only be factorised over certain rings. In general we use the same format for the output as the Julia factorisation function, namely an associative array with polynomial factors as keys and exponents as values.

<a id='Nemo.isirreducible-Tuple{Nemo.nmod_poly}' href='#Nemo.isirreducible-Tuple{Nemo.nmod_poly}'>#</a>
**`Nemo.isirreducible`** &mdash; *Method*.



```
isirreducible(x::nmod_poly)
```

> Return `true` if $x$ is irreducible, otherwise return `false`.


<a id='Nemo.isirreducible-Tuple{Nemo.fmpz_mod_poly}' href='#Nemo.isirreducible-Tuple{Nemo.fmpz_mod_poly}'>#</a>
**`Nemo.isirreducible`** &mdash; *Method*.



```
isirreducible(x::fmpz_mod_poly)
```

> Return `true` if $x$ is irreducible, otherwise return `false`.


<a id='Nemo.issquarefree-Tuple{Nemo.nmod_poly}' href='#Nemo.issquarefree-Tuple{Nemo.nmod_poly}'>#</a>
**`Nemo.issquarefree`** &mdash; *Method*.



```
issquarefree(x::nmod_poly)
```

> Return `true` if $x$ is squarefree, otherwise return `false`.


<a id='Nemo.issquarefree-Tuple{Nemo.fmpz_mod_poly}' href='#Nemo.issquarefree-Tuple{Nemo.fmpz_mod_poly}'>#</a>
**`Nemo.issquarefree`** &mdash; *Method*.



```
issquarefree(x::fmpz_mod_poly)
```

> Return `true` if $x$ is squarefree, otherwise return `false`.


<a id='Base.factor-Tuple{Nemo.nmod_poly}' href='#Base.factor-Tuple{Nemo.nmod_poly}'>#</a>
**`Base.factor`** &mdash; *Method*.



```
factor(x::nmod_poly)
```

> Return the factorisation of $x$.


<a id='Base.factor-Tuple{Nemo.fmpz_mod_poly}' href='#Base.factor-Tuple{Nemo.fmpz_mod_poly}'>#</a>
**`Base.factor`** &mdash; *Method*.



```
factor(x::fmpz_mod_poly)
```

> Return the factorisation of $x$.


<a id='Nemo.factor_squarefree-Tuple{Nemo.nmod_poly}' href='#Nemo.factor_squarefree-Tuple{Nemo.nmod_poly}'>#</a>
**`Nemo.factor_squarefree`** &mdash; *Method*.



```
factor_squarefree(x::nmod_poly)
```

> Return the squarefree factorisation of $x$.


<a id='Nemo.factor_squarefree-Tuple{Nemo.fmpz_mod_poly}' href='#Nemo.factor_squarefree-Tuple{Nemo.fmpz_mod_poly}'>#</a>
**`Nemo.factor_squarefree`** &mdash; *Method*.



```
factor_squarefree(x::fmpz_mod_poly)
```

> Return the squarefree factorisation of $x$.


<a id='Nemo.factor_distinct_deg-Tuple{Nemo.nmod_poly}' href='#Nemo.factor_distinct_deg-Tuple{Nemo.nmod_poly}'>#</a>
**`Nemo.factor_distinct_deg`** &mdash; *Method*.



```
factor_distinct_deg(x::nmod_poly)
```

> Return the distinct degree factorisation of a squarefree polynomial $x$.


<a id='Nemo.factor_distinct_deg-Tuple{Nemo.fmpz_mod_poly}' href='#Nemo.factor_distinct_deg-Tuple{Nemo.fmpz_mod_poly}'>#</a>
**`Nemo.factor_distinct_deg`** &mdash; *Method*.



```
factor_distinct_deg(x::fmpz_mod_poly)
```

> Return the distinct degree factorisation of a squarefree polynomial $x$.



Here are some examples of factorisation.


```
R = ResidueRing(ZZ, 23)
S, x = PolynomialRing(R, "x")

f = x^2 + 2x + 1
g = x^3 + 3x + 1

R = factor(f*g)
S = factor_squarefree(f*g)
T = factor_distinct_deg((x + 1)*g*(x^5+x^3+x+1))
```


<a id='Special-functions-1'></a>

## Special functions


The following special functions can be computed for any polynomial ring. Typically one uses the generator $x$ of a polynomial ring to get the respective special polynomials expressed in terms of that generator.

<a id='Nemo.chebyshev_t-Tuple{Int64,Nemo.PolyElem{T}}' href='#Nemo.chebyshev_t-Tuple{Int64,Nemo.PolyElem{T}}'>#</a>
**`Nemo.chebyshev_t`** &mdash; *Method*.



```
chebyshev_t(n::Int, x::PolyElem)
```

> Return the Chebyshev polynomial of the first kind $T_n(x)$, defined by  $T_n(x) = \cos(n \cos^{-1}(x))$.


<a id='Nemo.chebyshev_u-Tuple{Int64,Nemo.PolyElem{T}}' href='#Nemo.chebyshev_u-Tuple{Int64,Nemo.PolyElem{T}}'>#</a>
**`Nemo.chebyshev_u`** &mdash; *Method*.



```
chebyshev_u(n::Int, x::PolyElem)
```

> Return the Chebyshev polynomial of the first kind $U_n(x)$, defined by  $(n+1) U_n(x) = T'_{n+1}(x)$.



The following special polynomials are only available for certain base rings.

<a id='Nemo.cyclotomic-Tuple{Int64,Nemo.fmpz_poly}' href='#Nemo.cyclotomic-Tuple{Int64,Nemo.fmpz_poly}'>#</a>
**`Nemo.cyclotomic`** &mdash; *Method*.



```
cyclotomic(n::Int, x::fmpz_poly)
```

> Return the $n$th cyclotomic polynomial, defined as $\Phi_n(x) = \prod_{\omega} (x-\omega),$ where $\omega$ runs over all the  $n$th primitive roots of unity.


<a id='Nemo.swinnerton_dyer-Tuple{Int64,Nemo.fmpz_poly}' href='#Nemo.swinnerton_dyer-Tuple{Int64,Nemo.fmpz_poly}'>#</a>
**`Nemo.swinnerton_dyer`** &mdash; *Method*.



```
swinnerton_dyer(n::Int, x::fmpz_poly)
```

> Return the Swinnerton-Dyer polynomial $S_n$, defined as the integer  polynomial $S_n = \prod (x \pm \sqrt{2} \pm \sqrt{3} \pm \sqrt{5} \pm \ldots \pm \sqrt{p_n})$  where $p_n$ denotes the $n$-th prime number and all combinations of signs are taken. This polynomial has degree $2^n$ and is irreducible over the integers (it is the minimal polynomial of $\sqrt{2} + \ldots + \sqrt{p_n}$).


<a id='Nemo.cos_minpoly-Tuple{Int64,Nemo.fmpz_poly}' href='#Nemo.cos_minpoly-Tuple{Int64,Nemo.fmpz_poly}'>#</a>
**`Nemo.cos_minpoly`** &mdash; *Method*.



```
cos_minpoly(n::Int, x::fmpz_poly)
```

> Return the minimal polynomial of $2 \cos(2 \pi / n)$. For suitable choice of  $n$, this gives the minimal polynomial of $2 \cos(a \pi)$ or $2 \sin(a \pi)$ for any rational $a$.


<a id='Nemo.theta_qexp-Tuple{Int64,Int64,Nemo.fmpz_poly}' href='#Nemo.theta_qexp-Tuple{Int64,Int64,Nemo.fmpz_poly}'>#</a>
**`Nemo.theta_qexp`** &mdash; *Method*.



```
theta_qexp(e::Int, n::Int, x::fmpz_poly)
```

> Return the $q$-expansion to length $n$ of the Jacobi theta function raised to the power $r$, i.e. $\vartheta(q)^r$ where  $\vartheta(q) = 1 + \sum_{k=1}^{\infty} q^{k^2}$.


<a id='Nemo.eta_qexp-Tuple{Int64,Int64,Nemo.fmpz_poly}' href='#Nemo.eta_qexp-Tuple{Int64,Int64,Nemo.fmpz_poly}'>#</a>
**`Nemo.eta_qexp`** &mdash; *Method*.



```
eta_qexp(e::Int, n::Int, x::fmpz_poly)
```

> Return the $q$-expansion to length $n$ of the Dedekind eta function (without  the leading factor $q^{1/24}$) raised to the power $r$, i.e. $(q^{-1/24} \eta(q))^r = \prod_{k=1}^{\infty} (1 - q^k)^r$. In particular, $r = -1$ gives the generating function of the partition function $p(k)$, and $r = 24$ gives, after multiplication by $q$, the modular discriminant $\Delta(q)$ which generates the Ramanujan tau function $\tau(k)$.



Here are some examples of special functions.


```
R, x = PolynomialRing(ZZ, "x")
S, y = PolynomialRing(R, "y")

f = chebyshev_t(20, y)
g = chebyshev_u(15, y)
h = cyclotomic(120, x)
j = swinnerton_dyer(5, x)
k = cos_minpoly(30, x)
l = theta_qexp(3, 30, x)
m = eta_qexp(24, 30, x)
o = cyclotomic(10, 1 + x + x^2)
```

