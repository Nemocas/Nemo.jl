```@meta
CurrentModule = Nemo
CollapsedDocStrings = true
DocTestSetup = Nemo.doctestsetup()
```

# Number field arithmetic

Number fields are provided in Nemo by Antic. This allows construction of
absolute number fields and basic arithmetic computations therein.

Number fields are constructed using the `number_field` function.

The types of number field elements in Nemo are given in the following table,
along with the libraries that provide them and the associated types of the
parent objects.

 Library | Field                          | Element type  | Parent type
---------|--------------------------------|---------------|---------------------
Antic    | $\mathbb{Q}[x]/(f)$            | `AbsSimpleNumFieldElem`     | `AbsSimpleNumField`

All the number field types belong to the `Field` abstract type and the number
field element types belong to the `FieldElem` abstract type.

The Hecke.jl library radically expands on number field functionality, providing
ideals, orders, class groups, relative extensions, class field theory, etc.

The basic number field element type used in Hecke is the Nemo/antic number field
element type, making the two libraries tightly integrated.

<https://thofma.github.io/Hecke.jl/stable/>

## Number field functionality

The number fields in Nemo provide all of the AbstractAlgebra field functionality:

<https://nemocas.github.io/AbstractAlgebra.jl/stable/field>

Below, we document the additional functionality provided for number field elements.

### Constructors

In order to construct number field elements in Nemo, one must first construct
the number field itself. This is accomplished with one of the following
constructors.

```@docs
number_field(::QQPolyRingElem, ::VarName)
cyclotomic_field(::Int, ::VarName)
cyclotomic_real_subfield(::Int, ::VarName)
```

Here are some examples of creating number fields and making use of the
resulting parent objects to coerce various elements into those fields.

**Examples**

```jldoctest
julia> R, x = polynomial_ring(QQ, "x")
(Univariate polynomial ring in x over QQ, x)

julia> K, a = number_field(x^3 + 3x + 1, "a")
(Number field of degree 3 over QQ, a)

julia> L, b = cyclotomic_field(5, "b")
(Cyclotomic field of order 5, b)

julia> M, c = cyclotomic_real_subfield(5, "c")
(Maximal real subfield of cyclotomic field of order 5, c)

julia> d = K(3)
3

julia> f = L(b)
b

julia> g = L(ZZ(11))
11

julia> h = L(ZZ(11)//3)
11//3
```

### Number field element constructors

```@docs
gen(::AbsSimpleNumField)
```

The easiest way of constructing number field elements is to use element
arithmetic with the generator, to construct the desired element by its
representation as a polynomial. See the following examples for how to do this.

**Examples**

```jldoctest
julia> R, x = polynomial_ring(QQ, "x")
(Univariate polynomial ring in x over QQ, x)

julia> K, a = number_field(x^3 + 3x + 1, "a")
(Number field of degree 3 over QQ, a)

julia> d = gen(K)
a

julia> f = a^2 + 2a - 7
a^2 + 2*a - 7
```

### Basic functionality

```@docs
mul_red!(::AbsSimpleNumFieldElem, ::AbsSimpleNumFieldElem, ::AbsSimpleNumFieldElem, ::Bool)
```

```@docs
reduce!(::AbsSimpleNumFieldElem)
```

The following coercion function is provided for a number field $R$.

```julia
R(f::QQPolyRingElem)
```

Coerce the given rational polynomial into the number field $R$, i.e. consider the
polynomial to be the representation of a number field element and return it.

Conversely, if $R$ is the polynomial ring to which the generating polynomial of a number
field belongs, then we can coerce number field elements into the ring $R$ using
the following function.

```julia
R(b::AbsSimpleNumFieldElem)
```

Coerce the given number field element into the polynomial ring $R$ of which the
number field is a quotient.

**Examples**

```jldoctest
julia> R, x = polynomial_ring(QQ, "x")
(Univariate polynomial ring in x over QQ, x)

julia> K, a = number_field(x^3 + 3x + 1, "a")
(Number field of degree 3 over QQ, a)

julia> f = R(a^2 + 2a + 3)
x^2 + 2*x + 3

julia> g = K(x^2 + 2x + 1)
a^2 + 2*a + 1
```

### Basic manipulation

```@docs
var(::AbsSimpleNumField)
```

```@docs
is_gen(::AbsSimpleNumFieldElem)
```

```@docs
coeff(::AbsSimpleNumFieldElem, ::Int)
```

```@docs
denominator(::AbsSimpleNumFieldElem)
```

```@docs
degree(::AbsSimpleNumField)
```

**Examples**

```jldoctest
julia> R, x = polynomial_ring(QQ, "x")
(Univariate polynomial ring in x over QQ, x)

julia> K, a = number_field(x^3 + 3x + 1, "a")
(Number field of degree 3 over QQ, a)

julia> d = a^2 + 2a - 7
a^2 + 2*a - 7

julia> m = gen(K)
a

julia> c = coeff(d, 1)
2

julia> is_gen(m)
true

julia> q = degree(K)
3
```

### Norm and trace

```@docs
norm(::AbsSimpleNumFieldElem)
```

```@docs
tr(::AbsSimpleNumFieldElem)
```

**Examples**

```jldoctest
julia> R, x = polynomial_ring(QQ, "x")
(Univariate polynomial ring in x over QQ, x)

julia> K, a = number_field(x^3 + 3x + 1, "a")
(Number field of degree 3 over QQ, a)

julia> c = 3a^2 - a + 1
3*a^2 - a + 1

julia> d = norm(c)
113

julia> f = tr(c)
-15
```
