```@meta
CurrentModule = Nemo
```

# Number field arithmetic

Number fields are provided in Nemo by Antic. This allows construction of
absolute number fields and basic arithmetic computations therein.

Number fields are constructed using the `AnticNumberField` function. However,
for convenience we define

```
NumberField = AnticNumberField
```

so that number fields can be constructed using `NumberField` rather than
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

<https://thofma.github.io/Hecke.jl/latest/>

## Number field functionality

The number fields in Nemo implement the full AbstractAlgebra.jl field interface.

<https://nemocas.github.io/AbstractAlgebra.jl/latest/fields>

Below, we document the additional functionality provided for number field elements.

### Constructors

In order to construct number field elements in Nemo, one must first construct
the number field itself. This is accomplished with one of the following
constructors.

```julia
AnticNumberField(::fmpq_poly, ::AbstractString; cached = true)
```

Return a tuple `K, a` consisting of the number field parent object $K$ and generator
`a`. The generator will be printed as per the supplied string. By default number field
parents are cached based on generator string and generating polynomial. If this is
not desired, the optional argument `cached` can be set to false.

```julia
AnticCyclotomicField(::Int, ::AbstractString, AbstractString; cached = true)
```

Return a tuple `K, a` consisting of a parent object $K$ for the $n$-th cyclotomic
field, and a generator $a$. By default number field parents are cached based on
generator string and generating polynomial. If this is not desired, the optional
argument `cached` can be set to false.

```julia
AnticMaximalRealSubfield(::Int, ::AbstractString, ::AbstractString; cached = true)
```

Return a tuple `K, a` consisting of a parent object $K$ for the real subfield of the
$n$-th cyclotomic field, and a generator $a$. By default number field parents are
cached based on generator string and generating polynomial. If this is not desired, the
optional argument `cached` can be set to false.

For convenience we define

```
NumberField = AnticNumberField
CyclotomicField = AnticCyclotomicField
MaximalRealSubfield = AnticMaximalRealSubfield
```

so that one can use the names on the left instead of those on the right.

Here are some examples of creating number fields and making use of the
resulting parent objects to coerce various elements into those fields.

**Examples**

```julia
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

### Number field element constructors

```@docs
gen(::AnticNumberField)
```

The easiest way of constructing number field elements is to use element
arithmetic with the generator, to construct the desired element by its
representation as a polynomial. See the following examples for how to do this.

**Examples**

```
R, x = PolynomialRing(QQ, "x")
K, a = NumberField(x^3 + 3x + 1, "a")

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
R(f::fmpq_poly)
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
R, x = PolynomialRing(QQ, "x")
K, a = NumberField(x^3 + 3x + 1, "a")

f = R(a^2 + 2a + 3)
g = K(x^2 + 2x + 1)
```

### Basic manipulation

```@docs
var(::AnticNumberField)
```

```@docs
isgen(::nf_elem)
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
R, x = PolynomialRing(QQ, "x")
K, a = NumberField(x^3 + 3x + 1, "a")

d = a^2 + 2a - 7
m = gen(K)

c = coeff(d, 1)
isgen(m)
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

```julia
R, x = PolynomialRing(QQ, "x")
K, a = NumberField(x^3 + 3x + 1, "a")

c = 3a^2 - a + 1

d = norm(c)
f = tr(c)
```
