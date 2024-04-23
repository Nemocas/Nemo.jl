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

```jldoctest
julia> R = PadicField(7, 30)
Field of 7-adic numbers

julia> S = PadicField(ZZ(65537), 30)
Field of 65537-adic numbers

julia> a = R()
O(7^30)

julia> b = S(1)
65537^0 + O(65537^30)

julia> c = S(ZZ(123))
123*65537^0 + O(65537^30)

julia> d = R(ZZ(1)//7^2)
7^-2 + O(7^28)
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

```jldoctest
julia> R = PadicField(7, 30)
Field of 7-adic numbers

julia> S = PadicField(ZZ(65537), 30)
Field of 65537-adic numbers

julia> c = 1 + 2*7 + 4*7^2 + O(R, 7^3)
7^0 + 2*7^1 + 4*7^2 + O(7^3)

julia> d = 13 + 357*ZZ(65537) + O(S, ZZ(65537)^12)
13*65537^0 + 357*65537^1 + O(65537^12)

julia> f = ZZ(1)//7^2 + ZZ(2)//7 + 3 + 4*7 + O(R, 7^2)
7^-2 + 2*7^-1 + 3*7^0 + 4*7^1 + O(7^2)
```

Beware that the expression `1 + 2*p + 3*p^2 + O(R, p^n)` is actually computed
as a normal Julia expression. Therefore if `Int` values are used instead
of `ZZRingElem`s or Julia `BigInt`s, overflow may result in evaluating the
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

```jldoctest
julia> R = PadicField(7, 30)
Field of 7-adic numbers

julia> a = 1 + 2*7 + 4*7^2 + O(R, 7^3)
7^0 + 2*7^1 + 4*7^2 + O(7^3)

julia> b = 7^2 + 3*7^3 + O(R, 7^5)
7^2 + 3*7^3 + O(7^5)

julia> c = R(2)
2*7^0 + O(7^30)

julia> k = precision(a)
3

julia> m = prime(R)
7

julia> n = valuation(b)
2

julia> p = lift(ZZ, a)
211

julia> q = lift(QQ, divexact(a, b))
337//49
```

### Square root

```@docs
Base.sqrt(::PadicFieldElem)
```

**Examples**

```jldoctest
julia> R = PadicField(7, 30)
Field of 7-adic numbers

julia> a = 1 + 7 + 2*7^2 + O(R, 7^3)
7^0 + 7^1 + 2*7^2 + O(7^3)

julia> b = 2 + 3*7 + O(R, 7^5)
2*7^0 + 3*7^1 + O(7^5)

julia> c = 7^2 + 2*7^3 + O(R, 7^4)
7^2 + 2*7^3 + O(7^4)

julia> d = sqrt(a)
7^0 + 4*7^1 + 3*7^2 + O(7^3)

julia> f = sqrt(b)
3*7^0 + 5*7^1 + 7^2 + 7^3 + O(7^5)

julia> f = sqrt(c)
7^1 + 7^2 + O(7^3)

julia> g = sqrt(R(121))
3*7^0 + 5*7^1 + 6*7^2 + 6*7^3 + 6*7^4 + 6*7^5 + 6*7^6 + 6*7^7 + 6*7^8 + 6*7^9 + 6*7^10 + 6*7^11 + 6*7^12 + 6*7^13 + 6*7^14 + 6*7^15 + 6*7^16 + 6*7^17 + 6*7^18 + 6*7^19 + 6*7^20 + 6*7^21 + 6*7^22 + 6*7^23 + 6*7^24 + 6*7^25 + 6*7^26 + 6*7^27 + 6*7^28 + 6*7^29 + O(7^30)

julia> g^2 == R(121)
true
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

```jldoctest
julia> R = PadicField(7, 30)
Field of 7-adic numbers

julia> a = 1 + 7 + 2*7^2 + O(R, 7^3)
7^0 + 7^1 + 2*7^2 + O(7^3)

julia> b = 2 + 5*7 + 3*7^2 + O(R, 7^3)
2*7^0 + 5*7^1 + 3*7^2 + O(7^3)

julia> c = 3*7 + 2*7^2 + O(R, 7^5)
3*7^1 + 2*7^2 + O(7^5)

julia> c = exp(c)
7^0 + 3*7^1 + 3*7^2 + 4*7^3 + 4*7^4 + O(7^5)

julia> d = log(a)
7^1 + 5*7^2 + O(7^3)

julia> c = exp(R(0))
7^0 + O(7^30)

julia> d = log(R(1))
O(7^30)

julia> f = teichmuller(b)
2*7^0 + 4*7^1 + 6*7^2 + O(7^3)
``` 
