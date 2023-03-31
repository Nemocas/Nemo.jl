```@meta
CurrentModule = Nemo
DocTestSetup = quote
    using Nemo
end
```

# Number field arithmetic

Number fields are provided in Nemo by Antic. This allows construction of
absolute number fields and basic arithmetic computations therein.

Number fields are constructed using the `AnticNumberField` function. However,
for convenience we define

```
number_field = AnticNumberField
```

so that number fields can be constructed using `number_field` rather than
`AnticNumberField`. 

The types of number field elements in Nemo are given in the following table,
along with the libraries that provide them and the associated types of the
parent objects.

 Library | Field                          | Element type  | Parent type
---------|--------------------------------|---------------|---------------------
Antic    | $\mathbb{Q}[x]/(f)$            | `nf_elem`     | `AnticNumberField`

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

```julia
R, x = polynomial_ring(QQ, "x")
K, a = number_field(x^3 + 3x + 1, "a")
L, b = CyclotomicField(5, "b")
M, c = CyclotomicRealField(5, "c")

d = K(3)
f = L(b)
g = L(ZZ(11))
h = L(ZZ(11)//3)
k = M(x)
```

### Number field element constructors

```@docs
gen(::AnticNumberField)
```

The easiest way of constructing number field elements is to use element
arithmetic with the generator, to construct the desired element by its
representation as a polynomial. See the following examples for how to do this.

**Examples**

```
R, x = polynomial_ring(QQ, "x")
K, a = number_field(x^3 + 3x + 1, "a")

d = gen(K)
f = a^2 + 2a - 7
```

### Basic functionality

```@docs
mul_red!(::nf_elem, ::nf_elem, ::nf_elem, ::Bool)
```

```@docs
reduce!(::nf_elem)
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
R(b::nf_elem)
```

Coerce the given number field element into the polynomial ring $R$ of which the
number field is a quotient.

**Examples**

```
R, x = polynomial_ring(QQ, "x")
K, a = number_field(x^3 + 3x + 1, "a")

f = R(a^2 + 2a + 3)
g = K(x^2 + 2x + 1)
```

### Basic manipulation

```@docs
var(::AnticNumberField)
```

```@docs
is_gen(::nf_elem)
```

```@docs
coeff(::nf_elem, ::Int)
```

```@docs
denominator(::nf_elem)
```

```@docs
degree(::AnticNumberField)
```

**Examples**

```julia
R, x = polynomial_ring(QQ, "x")
K, a = number_field(x^3 + 3x + 1, "a")

d = a^2 + 2a - 7
m = gen(K)

c = coeff(d, 1)
is_gen(m)
q = degree(K)
r, s = signature(K)
v = var(R)
```

### Norm and trace

```@docs
norm(::nf_elem)
```

```@docs
tr(::nf_elem)
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
