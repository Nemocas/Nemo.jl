


<a id='Introduction-1'></a>

## Introduction


Number fields are provided in Nemo by Antic. This allows construction of absolute number fields and basic arithmetic computations therein.


Number fields are constructed using the `AnticNumberField` function. However, for convenience we define


```
NumberField = AnticNumberField
```


so that number fields can be constructed using `NumberField` rather than `AnticNumberField`. 


The types of number field elements in Nemo are given in the following table, along with the libraries that provide them and the associated types of the parent objects.


Library |               Field | Element type |        Parent type
------: | ------------------: | -----------: | -----------------:
  Antic | $\mathbb{Q}[x]/(f)$ |    `nf_elem` | `AnticNumberField`


All the number field types belong to the `Field` abstract type and the number field element types belong to the `FieldElem` abstract type.


<a id='Number-field-constructors-1'></a>

## Number field constructors


In order to construct number field elements in Nemo, one must first construct the number field itself. This is accomplished with one of the following constructors.

<a id='Nemo.AnticNumberField-Tuple{Nemo.fmpq_poly,AbstractString}' href='#Nemo.AnticNumberField-Tuple{Nemo.fmpq_poly,AbstractString}'>#</a>
**`Nemo.AnticNumberField`** &mdash; *Method*.



```
AnticNumberField(f::fmpq_poly, s::AbstractString{})
```

> Return a tuple $R, x$ consisting of the parent object $R$ and generator $x$ of the number field $\mathbb{Q}/(f)$ where $f$ is the supplied polynomial. The supplied string `s` specifies how the generator of the number field should be printed.


<a id='Nemo.AnticCyclotomicField-Tuple{Int64,AbstractString,Any}' href='#Nemo.AnticCyclotomicField-Tuple{Int64,AbstractString,Any}'>#</a>
**`Nemo.AnticCyclotomicField`** &mdash; *Method*.



```
AnticCyclotomicField(n::Int, s::AbstractString{}, t = "\$")
```

> Return a tuple $R, x$ consisting of the parent object $R$ and generator $x$ of the $n$-th cyclotomic field, $\mathbb{Q}(\zeta_n)$. The supplied string `s` specifies how the generator of the number field should be printed. If provided, the string `t` specifies how the generator of the polynomial ring from which the number field is constructed, should be printed. If it is not supplied, a default dollar sign will be used to represent the variable.


<a id='Nemo.AnticMaximalRealSubfield-Tuple{Int64,AbstractString,AbstractString}' href='#Nemo.AnticMaximalRealSubfield-Tuple{Int64,AbstractString,AbstractString}'>#</a>
**`Nemo.AnticMaximalRealSubfield`** &mdash; *Method*.



```
AnticMaximalRealSubfield(n::Int, s::AbstractString{}, t = "\$")
```

> Return a tuple $R, x$ consisting of the parent object $R$ and generator $x$ of the totally real subfield of the $n$-th cyclotomic field, $\mathbb{Q}(\zeta_n)$. The supplied string `s` specifies how the generator of the number field should be printed. If provided, the string `t` specifies how the generator of the polynomial ring from which the number field is constructed, should be printed. If it is not supplied, a default dollar sign will be used to represent the variable.



For convenience we define


```
NumberField = AnticNumberField
CyclotomicField = AnticCyclotomicField
MaximalRealSubfield = AnticMaximalRealSubfield
```


so that one can use the names on the left instead of those on the right.


Here are some examples of creating number fields and making use of the resulting parent objects to coerce various elements into those fields.


```
R, x = PolynomialRing(QQ, "x")
K, a = NumberField(x^3 + 3x + 1, "a")
L, b = CyclotomicField(5, "b")
M, c = MaximalRealSubfield(5, "c", "y")

d = K(3)
f = L(b)
g = L(ZZ(11))
h = L(ZZ(11)//3)
k = M(x)
```


<a id='Number-field-element-constructors-1'></a>

## Number field element constructors


Once a number field is constructed, there are various ways to construct elements in that field.


Apart from coercing elements into the number field as above, we offer the following functions.

<a id='Base.zero-Tuple{Nemo.AnticNumberField}' href='#Base.zero-Tuple{Nemo.AnticNumberField}'>#</a>
**`Base.zero`** &mdash; *Method*.



```
zero(a::AnticNumberField)
```

> Return the multiplicative identity, i.e. one, in the given number field.


<a id='Base.one-Tuple{Nemo.AnticNumberField}' href='#Base.one-Tuple{Nemo.AnticNumberField}'>#</a>
**`Base.one`** &mdash; *Method*.



```
one(a::AnticNumberField)
```

> Return the multiplicative identity, i.e. one, in the given number field.


<a id='Nemo.gen-Tuple{Nemo.AnticNumberField}' href='#Nemo.gen-Tuple{Nemo.AnticNumberField}'>#</a>
**`Nemo.gen`** &mdash; *Method*.



```
gen(a::AnticNumberField)
```

> Return the generator of the given number field.



The easiest way of constructing number field elements is to use element arithmetic with the generator, to construct the desired element by its representation as a polynomial. See the following examples for how to do this.


Here are some examples of constructing number field elements.


```
R, x = PolynomialRing(QQ, "x")
K, a = NumberField(x^3 + 3x + 1, "a")

b = zero(K)
c = one(K)
d = gen(K)
f = a^2 + 2a - 7
```


<a id='Basic-functionality-1'></a>

## Basic functionality


The following basic functionality is provided by the default number field implementation in Nemo, to support construction of generic rings over number fields. Any custom number field implementation in Nemo should provide these  functions along with the usual arithmetic operations.


```
parent_type(::Type{nf_elem})
```


Gives the type of the parent object of an Antic number field element.


```
elem_type(R::AnticNumberField)
```


Given the parent object for a number field, return the type of elements of the field.


```
Base.hash(a::nf_elem, h::UInt)
```


Return a `UInt` hexadecimal hash of the number field element $a$. This should be xor'd with a fixed random hexadecimal specific to the number field type. The hash of the coefficients of the number field element representation should be xor'd with the supplied parameter `h` as part of computing the hash.


```
deepcopy(a::nf_elem)
```


Construct a copy of the given number field element and return it. This function must recursively construct copies of all of the internal data in the given element. Nemo number field elements are mutable and so returning shallow copies is not sufficient.


```
mul!(c::nf_elem, a::nf_elem, b::nf_elem)
```


Multiply $a$ by $b$ and set the existing number field element $c$ to the result. This function is provided for performance reasons as it saves allocating a new object for the result and eliminates associated garbage collection.


```
mul_red!(c::nf_elem, a::nf_elem, b::nf_elem, red::Bool)
```


Multiply $a$ by $b$ and set the existing number field element $c$ to the result. Reduction modulo the defining polynomial is only performed if `red` is set to `true`. Note that $a$ and $b$ must be reduced. This function is provided for performance reasons as it saves allocating a new object for the result and eliminates associated garbage collection.


```
reduce!(x::nf_elem)
```


Reduce the given number field element by the defining polynomial, in-place. This only needs to be done after accumulating values computed by `mul_red!` where reduction has not been performed. All standard Nemo number field functions automatically reduce their outputs.


```
addeq!(c::nf_elem, a::nf_elem)
```


In-place addition. Adds $a$ to $c$ and sets $c$ to the result. This function is provided for performance reasons as it saves allocating a new object for the result and eliminates associated garbage collection.


Given the parent object `R` for a number field, the following coercion functions are provided to coerce various elements into the number field. Developers provide these by overloading the `call` operator for the number field parent objects.


```
R()
```


Coerce zero into the number field.


```
R(n::Integer)
R(f::fmpz)
R(f::fmpq)
```


Coerce an integer or rational value into the number field.


```
R(f::fmpq_poly)
```


Coerce the given rational polynomial into the number field, i.e. consider the polynomial to be the representation of a number field element and return it.


```
R(f::nf_elem)
```


Take a number field element that is already in the number field and simply return it. A copy of the original is not made.


In addition to the above, developers of custom number field types must ensure that they provide the equivalent of the function `base_ring(R::AnticNumberField)` which should return `Union{}`. In addition to this they should ensure that each number field element contains a field `parent` specifying the parent object of the number field element, or at least supply the equivalent of the function `parent(a::nf_elem)` to return the parent object of a number field element.


<a id='Conversions-1'></a>

## Conversions


If $R$ is the polynomial ring to which the generating polynomial of a number field belongs, then we can coerce number field elements into the ring $R$ using the following function.


```
R(b::nf_elem)
```


Coerce the given number field element into the polynomial ring $R$ of which the number field is a quotient.


Here are some examples of conversions from number field elements.


```
R, x = PolynomialRing(QQ, "x")
K, a = NumberField(x^3 + 3x + 1, "a")

f = R(a^2 + 2a + 3)
```


<a id='Basic-manipulation-1'></a>

## Basic manipulation


Numerous functions are provided to manipulate number field elements. Also see the section on basic functionality above.

<a id='Nemo.base_ring-Tuple{Nemo.AnticNumberField}' href='#Nemo.base_ring-Tuple{Nemo.AnticNumberField}'>#</a>
**`Nemo.base_ring`** &mdash; *Method*.



```
base_ring(a::AnticNumberField)
```

> Returns `Union{}` since a number field doesn't depend on any ring.


<a id='Nemo.base_ring-Tuple{Nemo.nf_elem}' href='#Nemo.base_ring-Tuple{Nemo.nf_elem}'>#</a>
**`Nemo.base_ring`** &mdash; *Method*.



```
base_ring(a::nf_elem)
```

> Returns `Union{}` since a number field doesn't depend on any ring.


<a id='Base.parent-Tuple{Nemo.nf_elem}' href='#Base.parent-Tuple{Nemo.nf_elem}'>#</a>
**`Base.parent`** &mdash; *Method*.



```
parent(a::nf_elem)
```

> Return the parent of the given number field element.


<a id='Base.var-Tuple{Nemo.AnticNumberField}' href='#Base.var-Tuple{Nemo.AnticNumberField}'>#</a>
**`Base.var`** &mdash; *Method*.



```
var(a::AnticNumberField)
```

> Returns the identifier (as a symbol, not a string), that is used for printing the generator of the given number field.


<a id='Nemo.iszero-Tuple{Nemo.nf_elem}' href='#Nemo.iszero-Tuple{Nemo.nf_elem}'>#</a>
**`Nemo.iszero`** &mdash; *Method*.



```
iszero(a::nf_elem)
```

> Return `true` if the given number field element is the additive identity of the number field, i.e. zero, otherwise return `false`.


<a id='Nemo.isone-Tuple{Nemo.nf_elem}' href='#Nemo.isone-Tuple{Nemo.nf_elem}'>#</a>
**`Nemo.isone`** &mdash; *Method*.



```
isone(a::nf_elem)
```

> Return `true` if the given number field element is the multiplicative identity of the number field, i.e. one, otherwise return `false`.


<a id='Nemo.isunit-Tuple{Nemo.nf_elem}' href='#Nemo.isunit-Tuple{Nemo.nf_elem}'>#</a>
**`Nemo.isunit`** &mdash; *Method*.



```
isunit(a::nf_elem)
```

> Return `true` if the given number field element is invertible, i.e. nonzero, otherwise return `false`.


<a id='Nemo.isgen-Tuple{Nemo.nf_elem}' href='#Nemo.isgen-Tuple{Nemo.nf_elem}'>#</a>
**`Nemo.isgen`** &mdash; *Method*.



```
isgen(a::nf_elem)
```

> Return `true` if the given number field element is the generator of the number field, otherwise return `false`.


<a id='Nemo.coeff-Tuple{Nemo.nf_elem,Int64}' href='#Nemo.coeff-Tuple{Nemo.nf_elem,Int64}'>#</a>
**`Nemo.coeff`** &mdash; *Method*.



```
coeff(x::nf_elem, n::Int)
```

> Return the $n$-th coefficient of the polynomial representation of the given number field element. Coefficients are numbered from $0$, starting with the constant coefficient.


<a id='Base.den-Tuple{Nemo.nf_elem}' href='#Base.den-Tuple{Nemo.nf_elem}'>#</a>
**`Base.den`** &mdash; *Method*.



```
den(a::nf_elem)
```

> Return the denominator of the polynomial representation of the given number field element.


<a id='Nemo.degree-Tuple{Nemo.AnticNumberField}' href='#Nemo.degree-Tuple{Nemo.AnticNumberField}'>#</a>
**`Nemo.degree`** &mdash; *Method*.



```
degree(a::AnticNumberField)
```

> Return the degree of the given number field, i.e. the degree of its defining polynomial.


<a id='Nemo.signature-Tuple{Nemo.AnticNumberField}' href='#Nemo.signature-Tuple{Nemo.AnticNumberField}'>#</a>
**`Nemo.signature`** &mdash; *Method*.



```
signature(a::AnticNumberField)
```

> Return the signature of the given number field, i.e. a tuple $r, s$ consisting of $r$, the number of real embeddings and $s$, half the number of complex embeddings.



Here are some examples of basic manipulation of number field elements.


```
R, x = PolynomialRing(QQ, "x")
K, a = NumberField(x^3 + 3x + 1, "a")

d = a^2 + 2a - 7
k = zero(K)
l = one(K)
m = gen(K)

c = coeff(d, 1)
isgen(m)
isone(l)
q = degree(K)
r, s = signature(K)
v = var(R)
S = parent(a + 1)
T = base_ring(K)
```


<a id='Arithmetic-operations-1'></a>

## Arithmetic operations


Nemo provides all the standard field operations for number field elements, as follows.


                        Function |      Operation
-------------------------------: | -------------:
                   -(a::nf_elem) |    unary minus
       +(a::nf_elem, b::nf_elem) |       addition
       -(a::nf_elem, b::nf_elem) |    subtraction
       *(a::nf_elem, b::nf_elem) | multiplication
divexact(a::nf_elem, b::nf_elem) | exact division


In addition, the following ad hoc field operations are defined.


                        Function |      Operation
-------------------------------: | -------------:
       +(a::nf_elem, b::Integer) |       addition
       +(a::Integer, b::nf_elem) |       addition
          +(a::nf_elem, b::fmpz) |       addition
          +(a::fmpz, b::nf_elem) |       addition
          +(a::nf_elem, b::fmpq) |       addition
          +(a::fmpq, b::nf_elem) |       addition
       -(a::nf_elem, b::Integer) |    subtraction
       -(a::Integer, b::nf_elem) |    subtraction
          -(a::nf_elem, b::fmpz) |    subtraction
          -(a::fmpz, b::nf_elem) |    subtraction
          -(a::nf_elem, b::fmpq) |    subtraction
          -(a::fmpq, b::nf_elem) |    subtraction
       *(a::nf_elem, b::Integer) | multiplication
       *(a::Integer, b::nf_elem) | multiplication
          *(a::nf_elem, b::fmpz) | multiplication
          *(a::fmpz, b::nf_elem) | multiplication
          *(a::nf_elem, b::fmpq) | multiplication
          *(a::fmpq, b::nf_elem) | multiplication
divexact(a::nf_elem, b::Integer) | exact division
   divexact(a::nf_elem, b::fmpz) | exact division
   divexact(a::nf_elem, b::fmpq) | exact division
divexact(a::Integer, b::nf_elem) | exact division
   divexact(a::fmpz, b::nf_elem) | exact division
   divexact(a::fmpq, b::nf_elem) | exact division
           ^(a::nf_elem, b::Int) |       powering


Here are some examples of arithmetic operations on number fields.


```
R, x = PolynomialRing(QQ, "x")
K, a = NumberField(x^3 + 3x + 1, "a")

c = a^2 + 2a - 7
d = 3a^2 - a + 1

f = c + d
g = c - d
h = c*d
k = 3*c
l = d*ZZ(4)
m = d*(ZZ(4)//3)
n = 4 - c
p = divexact(c, 5)
q = divexact(ZZ(4)//5, d)
r = c^4
```


<a id='Comparison-1'></a>

## Comparison


Nemo provides the comparison operation `==` for number field elements. Julia then automatically provides the corresponding `!=` operation. Here are the functions provided.


<a id='Function-1'></a>

## Function


==(a::nf_elem, b::nf_elem)


In addition, the following ad hoc comparisons are provided, Julia again providing the corresponding `!=` operators.


<a id='Function-2'></a>

## Function


==(a::nf_elem, b::Integer) ==(a::nf_elem, b::fmpz) ==(a::nf_elem, b::fmpq) ==(a::Integer, b::nf_elem) ==(a::fmpz, b::nf_elem) ==(a::fmpq, b::nf_elem)


Here are some examples of comparisons.


```
R, x = PolynomialRing(QQ, "x")
K, a = NumberField(x^3 + 3x + 1, "a")

c = 3a^2 - a + 1
d = a^2 + 2a - 7

c != d
c == 3a^2 - a + 1
c == 3
ZZ(4) == d
ZZ(5)//7 != c
```


<a id='Inversion-1'></a>

## Inversion

<a id='Base.inv-Tuple{Nemo.nf_elem}' href='#Base.inv-Tuple{Nemo.nf_elem}'>#</a>
**`Base.inv`** &mdash; *Method*.



```
inv(a::nf_elem)
```

> Return $a^{-1}$. Requires $a \neq 0$.



Here are some examples of inversion.


```
R, x = PolynomialRing(QQ, "x")
K, a = NumberField(x^3 + 3x + 1, "a")

c = 3a^2 - a + 1

d = inv(c)
```


<a id='Norm-and-trace-1'></a>

## Norm and trace

<a id='Base.LinAlg.norm-Tuple{Nemo.nf_elem}' href='#Base.LinAlg.norm-Tuple{Nemo.nf_elem}'>#</a>
**`Base.LinAlg.norm`** &mdash; *Method*.



```
norm(a::nf_elem)
```

> Return the absolute norm of $a$. The result will be a rational number.


<a id='Base.LinAlg.trace-Tuple{Nemo.nf_elem}' href='#Base.LinAlg.trace-Tuple{Nemo.nf_elem}'>#</a>
**`Base.LinAlg.trace`** &mdash; *Method*.



```
norm(a::nf_elem)
```

> Return the absolute trace of $a$. The result will be a rational number.



Here are some examples of computing the norm and trace.


```
R, x = PolynomialRing(QQ, "x")
K, a = NumberField(x^3 + 3x + 1, "a")

c = 3a^2 - a + 1

d = norm(c)
f = trace(c)
```

