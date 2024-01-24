```@meta
CurrentModule = Nemo
DocTestSetup = quote
    using Nemo
end
```

# Padics

P-adic fields are provided in Nemo by Flint. This allows construction of
$p$-adic fields for any prime $p$.

P-adic fields are constructed using the `PadicField` function. 

The types of $p$-adic fields in Nemo are given in the following table, along
with the libraries that provide them and the associated types of the parent
objects.

 Library | Field            | Element type | Parent type
---------|----------------|----------------|---------------------
Flint    | $\mathbb{Q}_p$ | `PadicFieldElem`        | `PadicField`

All the $p$-adic field types belong to the `Field` abstract type and the
$p$-adic field element types belong to the `FieldElem` abstract type.

## P-adic functionality

P-adic fields in Nemo implement all the AbstractAlgebra field functionality:.

<https://nemocas.github.io/AbstractAlgebra.jl/stable/field>

Below, we document all the additional function that is provide by Nemo for p-adic
fields.

### Constructors

In order to construct $p$-adic field elements in Nemo, one must first construct
the $p$-adic field itself. This is accomplished with one of the following
constructors.

```@docs
PadicField(::Integer, ::Int)
```

It is also possible to call the inner constructor directly. It has the following
form.

```
PadicField(p::ZZRingElem, prec::Int)
```

Returns the parent object for the $p$-adic field for given prime $p$, where
the default absolute precision of elements of the field is given by `prec`.

Here are some examples of creating $p$-adic fields and making use of the
resulting parent objects to coerce various elements into those fields.

**Examples**

```julia
R = PadicField(7, 30)
S = PadicField(ZZ(65537), 30)

a = R()
b = S(1)
c = S(ZZ(123))
d = R(ZZ(1)//7^2)
```

### Big-oh notation

Elements of p-adic fields can  be constructed using the big-oh notation. For this
purpose we define the following functions.

```@docs
O(::PadicField, ::Integer)
O(::PadicField, ::ZZRingElem)
O(::PadicField, ::QQFieldElem)
```

The $O(p^n)$ construction can be used to construct $p$-adic values of precision
$n$ by adding it to integer values representing the $p$-adic value modulo
$p^n$ as in the examples.

**Examples**

```julia
R = PadicField(7, 30)
S = PadicField(ZZ(65537), 30)

c = 1 + 2*7 + 4*7^2 + O(R, 7^3)
d = 13 + 357*ZZ(65537) + O(S, ZZ(65537)^12)
f = ZZ(1)//7^2 + ZZ(2)//7 + 3 + 4*7 + O(R, 7^2)
```

Beware that the expression `1 + 2*p + 3*p^2 + O(R, p^n)` is actually computed
as a normal Julia expression. Therefore if `{Int}` values are used instead
of Flint integers or Julia bignums, overflow may result in evaluating the
value.

### Basic manipulation

```@docs
prime(::PadicField)
```

```@docs
precision(::PadicFieldElem)
```

```@docs
valuation(::PadicFieldElem)
```

```@docs
lift(::ZZRing, ::PadicFieldElem)
lift(::QQField, ::PadicFieldElem)
```

**Examples**

```julia
R = PadicField(7, 30)

a = 1 + 2*7 + 4*7^2 + O(R, 7^3)
b = 7^2 + 3*7^3 + O(R, 7^5)
c = R(2)

k = precision(a)
m = prime(R)
n = valuation(b)
p = lift(FlintZZ, a)
q = lift(FlintQQ, divexact(a, b))
```

### Square root

```@docs
Base.sqrt(::PadicFieldElem)
```

**Examples**

```julia
R = PadicField(7, 30)

a = 1 + 7 + 2*7^2 + O(R, 7^3)
b = 2 + 3*7 + O(R, 7^5)
c = 7^2 + 2*7^3 + O(R, 7^4)

d = sqrt(a)
f = sqrt(b)
f = sqrt(c)
g = sqrt(R(121))
```

### Special functions

```@docs
Base.exp(::PadicFieldElem)
```

```@docs
log(::PadicFieldElem)
```

```@docs
teichmuller(::PadicFieldElem)
```

**Examples**

```julia
R = PadicField(7, 30)

a = 1 + 7 + 2*7^2 + O(R, 7^3)
b = 2 + 5*7 + 3*7^2 + O(R, 7^3)
c = 3*7 + 2*7^2 + O(R, 7^5)

c = exp(c)
d = log(a)
c = exp(R(0))
d = log(R(1))
f = teichmuller(b)
``` 
