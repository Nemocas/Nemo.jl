```@meta
CurrentModule = Nemo
```

## Introduction

Nemo allows the creation of fraction fields over any ring $R$. We don't require
$R$ to be an integral domain, however no attempt is made to deal with the
general case. Two fractions $a/b$ and $c/d$ are equal in Nemo iff $ad = bc$.
Thus, in practice, a greatest common divisor function is currently required for
the ring $R$.

In order to make the representation $a/b$ unique for printing, we have a notion
of canonical unit for elements of a ring $R$. When canonicalising $a/b$, each
of the elements $a$ and $b$ is first divided by the canonical unit of $b$.

The `canonical_unit` function is defined for elements of every Nemo ring. It
must have the properties

```
canonical_unit(u) == u
canonical_unit(a*b) == canonical_unit(a)*canonical_unit(b)
```

for any unit $u$ of the ring in question, and $a$ and $b$ arbitrary elements
of the ring.

For example, the canonical unit of an integer is its sign. Thus a fraction of
integers always has positive denominator after canonicalisation.

The canonical unit of a polynomial is the canonical unit of its leading
coefficient, etc.

There are two different kinds of implementation of fraction fields in Nemo: a
generic one for the case where no specific implementation exists, and efficient
implementations of fractions over specific rings, usually provided by C/C++
libraries.

The following table shows each of the fraction types available in Nemo, the
base ring $R$, and the Julia/Nemo types for that kind of fraction (the type
information is mainly of concern to developers).

Base ring                             | Library             | Element type        | Parent type
--------------------------------------|---------------------|---------------------|----------------------
Generic ring $R$                      | Nemo                | `GenFrac{T}`        | `GenFracField{T}`
$\mathbb{Z}$                          | Flint               | `fmpq`              | `FlintRationalField`

All fraction element types belong to the abstract type `FracElem` and all of
the fraction field types belong to the abstract type `FracField`. This enables
one to write generic functions that can accept any Nemo fraction type.

## Fraction field constructors

In order to construct fractions in Nemo, one must first construct the fraction
field itself. This can be accomplished manually with the following constructor.

```@docs
FractionField(::Ring, ::Bool)
```

Note: it is also possible to construct fractions directly in Nemo without
manually constructing the fraction field. In such cases, Nemo creates the
relevant fraction field internally.

For convenience, the rational fraction field is constructed automatically
in Nemo. We have the definition

```
QQ = FractionField(ZZ)
```

Here are some examples of creating fraction fields and making use of the
resulting parent objects to coerce various elements into those fields.

```
R = FractionField(ZZ)
S, x = PolynomialRing(ZZ, "x")
T = FractionField(S)

a = R(1)
b = T(fmpz(3))
c = T(x)
d = QQ(11)
```

## Fraction constructors

Once a fraction field is constructed, there are various ways to construct
fractions in that field.

Apart from coercing elements into the fraction field as above, we offer the
following functions.

```@docs
zero(::FracField)
```

```@docs
one(::FracField)
```

It is also possible to construct fractions directly in Nemo, without first
manually constructing the relevant fraction field. For this purpose we overload
Julia's fraction operator `//`.

Here are some examples of constructing fractions.

```
S, x = PolynomialRing(ZZ, "x")
T = FractionField(S)

a = zero(T)
b = one(T)
c = (x + 3)//(x^2 + 2)
```

## Basic functionality

All fraction field modules in Nemo must provide the functionality listed
in this section. (Note that only some of these functions are useful to a user.)

Developers who are writing their own fraction field module, whether as an
interface to a C library, or as some kind of generic module, must provide all
of these functions for custom fraction field types in Nemo. 

We write `U` for the type of fraction elements in the fraction field and `T`
for the type of elements of the base ring.

All of these functions are provided for all existing fraction types in Nemo.

```
parent_type{U <: FracElem}(::Type{U})
```

Given the type of fraction elements, should return the type of the corresponding
parent object.

```
elem_type(R::FracField)
```

Given a parent object for the fraction field, return the type of elements
of the fraction field.

```
Base.hash(a::FracElem, h::UInt)
```

Return a `UInt` hexadecimal hash of the fraction $a$. This should be xor'd
with a fixed random hexadecimal specific to the fraction type. The hash of
the numerator and denominator of a fraction should be xor'd with the supplied
parameter `h` as part of computing the hash.

```
num(a::FracElem)
```

Return the numerator of the given fraction element, i.e. for $a = b/c$ return
$b$. The returned numerator will be divided by the canonical unit of the
denominator.

```
den(a::FracElem)
```

Return the denominator of the given fraction element, i.e. for $a = b/c$ return
$c$. The returned denominator will be divided by the canonical unit of the
denominator.

```
deepcopy(a::FracElem)
```

Construct a copy of the given fraction and return it. This function must
recursively construct copies of all of the internal data in the given
fraction. Nemo fractions are mutable and so returning shallow copies is not
sufficient.

```
mul!(c::FracElem, a::FracElem, b::FracElem)
```

Multiply $a$ by $b$ and set the existing fraction $c$ to the result. This
function is provided for performance reasons as it saves allocating a new
object for the result and eliminates associated garbage collection.

```
addeq!(c::FracElem, a::FracElem)
```

In-place addition. Adds $a$ to $c$ and sets $c$ to the result. This function
is provided for performance reasons as it saves allocating a new object for
the result and eliminates associated garbage collection.

Given a parent object `S` for a fraction field, the following coercion
functions are provided to coerce various elements into the fraction field. 
Developers provide these by overloading the `call` operator for the fraction
field parent objects.

```
S()
```

Coerce zero into the field $S$.

```
S(n::Integer)
S(n::fmpz)
```

Coerce an integer value or Flint integer into the fraction field $S$.

```
S(n::T)
```

Coerces an element of the base ring, of type `T` into $S$.

```
S(f::FracElem)
```

Take a fraction that is already in the field $S$ and simply return it. A copy
of the original is not made.

```
S(c::RingElem)
```

Try to coerce the given ring element into the fraction field. This only
succeeds if $c$ can be coerced into the base ring.

There are also the followin constructors for creatinf fracions from a numerator
and denominator.

```
S(n::T, d::T)
S(n::Integer, d::T)
S(n::T, d::Integer)
```

Create the fraction $n/d$ in the fraction field $S$.

In addition to the above, developers of custom fractions must ensure the
parent object of a fraction type constains a field `base_ring` specifying the
base ring. They must also ensure that each fraction element contains a field
`parent` specifying the parent object of the fraction.

Typically a developer will also overload the `FractionField` generic function
to create fractions of the custom type they are implementing.

## Basic manipulation

Numerous functions are provided to manipulate fractions. Also see the section on
basic functionality above.

```@docs
base_ring(::FracField)
```

```@docs
base_ring(::FracElem)
```

```@docs
parent(::FracElem)
```

```@docs
iszero(::FracElem)
```

```@docs
isone(::FracElem)
```

```@docs
isunit(::FracElem)
```

Some functions are only available for certain rings.

```@docs
abs(::fmpq)
```

```@docs
height(::fmpq)
```

```@docs
height_bits(::fmpq)
```

```@docs
<<(::fmpq, ::Int)
```

```@docs
>>(::fmpq, ::Int)
```

Rational fractions can be compared with each other and with integers. Julia
provides the full range of operators $<, >, \leq, \geq$ which depend on the
following functions.

```@docs
isless(::fmpq, ::fmpq)
isless(::Integer, ::fmpq)
isless(::fmpq, ::Integer)
isless(::fmpq, ::fmpz)
isless(::fmpz, ::fmpq)
```

Here are some examples of basic manipulation of fractions.

```
S, x = PolynomialRing(ZZ, "x")

f = zero(S)
g = one(S)

a = isunit((x + 1)//(-x^2 + 1))
b = iszero(f)
c = isone(g)
d = abs(ZZ(11)//3)
U = base_ring(S)
V = base_ring((x + 1)//(-x^2 + 1))
W = parent(S(x))
4 <= ZZ(7)//ZZ(3)
```

## Arithmetic operators

All the usual arithmetic operators are overloaded for Nemo fractions. Note
that Julia uses the single slash for floating point division. Therefore to
perform exact division in a ring we use `divexact`. To construct an element
of a fraction field one can use the double slash operator `//`.

```@docs
-(::FracElem)
```

```@docs
+{T <: RingElem}(::FracElem{T}, ::FracElem{T})
```

```@docs
-{T <: RingElem}(::FracElem{T}, ::FracElem{T})
```

```@docs
*{T <: RingElem}(::FracElem{T}, ::FracElem{T})
```

```@docs
divexact{T <: RingElem}(::FracElem{T}, ::FracElem{T})
```

The following ad hoc operators are also provided.

```@docs
+(::Integer, ::FracElem)
+(::FracElem, ::Integer)
+(::fmpz, ::FracElem)
+(::FracElem, ::fmpz)
+{T <: RingElem}(::T, ::FracElem{T})
+{T <: RingElem}(::FracElem{T}, ::T)
```

```@docs
-(::Integer, ::FracElem)
-(::FracElem, ::Integer)
-(::fmpz, ::FracElem)
-(::FracElem, ::fmpz)
-{T <: RingElem}(::T, ::FracElem{T})
-{T <: RingElem}(::FracElem{T}, ::T)
``` 

```@docs
*(::Integer, ::FracElem)
*(::FracElem, ::Integer)
*(::fmpz, ::FracElem)
*(::FracElem, ::fmpz)
*{T <: RingElem}(::T, ::FracElem{T})
*{T <: RingElem}(::FracElem{T}, ::T)
``` 

```@docs
divexact(::Integer, ::FracElem)
divexact(::FracElem, ::Integer)
divexact(::fmpz, ::FracElem)
divexact(::FracElem, ::fmpz)
divexact{T <: RingElem}(::T, ::FracElem{T})
divexact{T <: RingElem}(::FracElem{T}, ::T)
``` 

```@docs
^(::FracElem, ::Int)
```

If the appropriate `promote_rule` and coercion exists, these operators can also
be used with elements of other rings. Nemo will try to coerce the operands to
the dominating type and then apply the operator.

Here are some examples of arithmetic operations on fractions.

```
S, x = PolynomialRing(ZZ, "x")

a = -((x + 1)//(-x^2 + 1))
b = -(x + 3)//(x + 1) + (2x + 3)//(x^2 + 4)
c = (x + 1)//(-x^2 + 1) - x//(2x + 1)
d = ((x^2 + 3x)//(5x))*((x + 1)//(2x^2 + 2))
f = a + 2
g = 3 - a
h = b*(x + 1)
k = divexact(a, b)
m = divexact(a, x + 1)
n = divexact(b, 23)
p = divexact(ZZ(2), b)
q = a^3
```

## Comparison operators

The following comparison operators are implemented for fractions in Nemo.

```@docs
=={T <: RingElem}(::FracElem{T}, ::FracElem{T})
```

```@docs
isequal{T <: RingElem}(::FracElem{T}, ::FracElem{T})
```

In addition we have the following ad hoc comparison operators.

```@docs
=={T <: RingElem}(::FracElem{T}, ::T)
=={T <: RingElem}(::T, ::FracElem{T})
==(::FracElem, ::Integer)
==(::Integer, ::FracElem)
==(::FracElem, ::fmpz)
==(::fmpz, ::FracElem)
```

Here are some examples of comparisons.

```
S, x = PolynomialRing(ZZ, "x")

a = -(ZZ(4)//ZZ(6))
b = -((x + 1)//(-x^2 + 1))

a == -ZZ(2)//ZZ(3)
b == 1//(x - 1)
a == 4
ZZ(2) == b
b == x + 1
```

## Inversion

```@docs
inv(a::FracElem)
```

Here are some examples of computing inverses.

```
S, x = PolynomialRing(ZZ, "x")

a = (x + 1)//(-x^2 + 1)

b = inv(a)
```

## Greatest common divisor

```@docs
gcd{T <: RingElem}(a::FracElem{T}, b::FracElem{T})
```

Here are some examples of computing a greatest common divisor.

```
S, x = PolynomialRing(ZZ, "x")

a = -x//(2x + 1)

f = gcd(a, (x + 1)//(x - 1))
```

## Modular arithmetic

The following functions are available for rationals.

```@docs
mod(a::fmpq, b::fmpz)
```

```@docs
mod(a::fmpq, b::Integer)
```

Here are some examples of modular arithmetic.

```
a = -fmpz(2)//3
b = fmpz(1)//2

c = mod(a, 7)
d = mod(b, fmpz(5))
```

### Rational Reconstruction

Rational reconstruction is available for rational numbers.

```@docs
reconstruct(::fmpz, ::fmpz)
reconstruct(::fmpz, ::Integer)
reconstruct(::Integer, ::fmpz)
reconstruct(::Integer, ::Integer)
```

Here are some examples of rational reconstruction.

```
a = reconstruct(7, 13)
b = reconstruct(fmpz(15), 31)
c = reconstruct(fmpz(123), fmpz(237))
```

## Rational enumeration

Various methods exist to enumerator rationals.

```@docs
next_minimal(::fmpq)
```

```@docs
next_signed_minimal(::fmpq)
```

```@docs
next_calkin_wilf(::fmpq)
```

```@docs
next_signed_calkin_wilf(::fmpq)
```

Here are some examples of rational enumeration.

```
next_minimal(fmpz(2)//3)
next_signed_minimal(-fmpz(21)//31)
next_calkin_wilf(fmpz(321)//113)
next_signed_calkin_wilf(-fmpz(51)//(17))
```

## Special functions

The following special functions are available for specific rings in Nemo.

```@docs
harmonic(::Int)
```

```@docs
bernoulli(::Int)
```

```@docs
bernoulli_cache(::Int)
```

```@docs
dedekind_sum(::fmpz, ::fmpz)
dedekind_sum(::fmpz, ::Integer)
dedekind_sum(::Integer, ::fmpz)
dedekind_sum(::Integer, ::Integer)
```

Here are some examples of special functions.

```
a = harmonic(12)

b = dedekind_sum(12, 13)
c = dedekind_sum(-120, fmpz(1305))

d = bernoulli(12)

bernoulli_cache(100)
e = bernoulli(100)
```
