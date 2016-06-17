```@meta
CurrentModule = Nemo
```

## Introduction

Nemo allows the creation of residue rings of the form $R/(a)$ for an element
$a$ of a ring $R$.

We don't require $(a)$ to be a prime or maximal ideal. Instead, we allow the
creation of the residue ring $R/(a)$ for any nonzero $a$ and simply raise an
exception if an impossible inverse is encountered during computations 
involving elements of $R/(a)$.

There is a generic implementation of residue rings of this form in Nemo, which
accepts any ring $R$ as base ring. The associated types of parent object and
elements in such residue rings are given in the following table.

Base ring                   | Library         | Element type  | Parent type
----------------------------|-----------------|---------------|----------------
Generic ring $R$            | Nemo            | `GenRes{T}`   | `GeResRing{T}`

The modulus $a$ of a residue ring is stored in its parent object.

All residue element types belong to the abstract type `ResElem` and all the
residue ring parent object types belong to the abstract type `ResRing`.
This enables one to write generic functions that accept any Nemo residue type.

## Residue ring constructors

In order to construct residues in Nemo, one must first construct the residue
ring itself. This is accomplished with the following constructor.

```@docs
ResidueRing{T <: RingElem}(::Ring, ::T; ::Bool)
```

Here are some examples of creating residue rings and making use of the
resulting parent objects to coerce various elements into those rings.

```
R = ResidueRing(ZZ, 17)
S, x = PolynomialRing(QQ, "x")
T = ResidueRing(S, x^2 + 1)

a = R(11)
b = T(x)
c = T(ZZ(3))
```

## Residue element constructors

Once a residue ring is constructed, there are various ways to construct
residues in that ring.

Apart from coercing elements into the residue ring as above, we offer the
following functions.

```@docs
zero(::ResRing)
```

```@docs
one(::ResRing)
```

Here are some examples of constructing residues.

```
R = ResidueRing(ZZ, 16453889)
S, x = PolynomialRing(R, "x")
T = ResidueRing(S, x^3 + 3x + 1)

g = zero(R)
h = one(T)
```

## Basic functionality

All residue ring modules in Nemo must provide the functionality listed
in this section. (Note that only some of these functions are useful to a user.)

Developers who are writing their own residue ring module, whether as an
interface to a C library, or as some kind of generic module, must provide all of
these functions for custom residue ring types in Nemo. 

We write `U` for the type of residue elements in the residue ring and `T` for
the type of elements of the base ring.

All of these functions are provided for all existing residue types in Nemo.

```
parent_type{U <: ResElem}(::Type{U})
```

Given the type of residue elements, should return the type of the corresponding
parent object.

```
elem_type(R::ResRing)
```

Given a parent object for the residue ring, return the type of elements
of the residue ring.

```
Base.hash(a::ResElem, h::UInt)
```

Return a `UInt` hexadecimal hash of the residue $a$. This should be xor'd
with a fixed random hexadecimal specific to the residue type. The hash of
the data associated with a residue should be xor'd with the supplied
parameter `h` as part of computing the hash.

```
data(a::ResElem)
```

Return the data of the given residue element, i.e. for $c \pmod{a}$ in the ring
$R/(a)$ return the value $c$ as an element of the ring $R$.

```
deepcopy(a::ResElem)
```

Construct a copy of the given residue and return it. This function must
recursively construct copies of all of the internal data in the given
residue. Nemo residues are mutable and so returning shallow copies is not
sufficient.

```
mul!(c::ResElem, a::ResElem, b::ResElem)
```

Multiply $a$ by $b$ and set the existing residue $c$ to the result. This
function is provided for performance reasons as it saves allocating a new
object for the result and eliminates associated garbage collection.

```
addeq!(c::ResElem, a::ResElem)
```

In-place addition. Adds $a$ to $c$ and sets $c$ to the result. This function
is provided for performance reasons as it saves allocating a new object for
the result and eliminates associated garbage collection.

Given a parent object `S` for a residue ring, the following coercion
functions are provided to coerce various elements into the residue
ring. Developers provide these by overloading the `call` operator for the
residue ring parent objects.

```
S()
```

Coerce zero into the ring $S$.

```
S(n::Integer)
S(n::fmpz)
```

Coerce an integer value or Flint integer into the residue ring $S$.

```
S(n::T)
```

Coerces an element of the base ring, of type `T` into $S$.

```
S(f::ResElem)
```

Take a residue that is already in the ring $S$ and simply return it. A copy
of the original is not made.

```
S(c::RingElem)
```

Try to coerce the given ring element into the residue ring. This only
succeeds if $c$ can be coerced into the base ring.

In addition to the above, developers of custom residues must ensure the
parent object of a residue type constains a field `base_ring` specifying the
base ring, a field `modulus` containing the modulus $a$ of the residue ring
$R/(a)$. They must also ensure that each residue element contains a field
`parent` specifying the parent object of the residue.

Typically a developer will also overload the `ResidueRing` generic function
to create residues of the custom type they are implementing.

## Basic manipulation

Numerous functions are provided to manipulate residues. Also see the section on
basic functionality above.

```@docs
base_ring(::ResRing)
```

```@docs
base_ring(::ResElem)
```

```@docs
parent(::ResElem)
```

```@docs
modulus(::ResRing)
```

```@docs
modulus(::ResElem)
```

```@docs
iszero(::ResElem)
```

```@docs
isone(::ResElem)
```

```@docs
isunit(::ResElem)
```

Here are some examples of basic manipulation of residues.

```
R = ResidueRing(ZZ, 16453889)
S, x = PolynomialRing(R, "x")
T = ResidueRing(S, x^3 + 3x + 1)

g = zero(R)
h = one(T)

a = modulus(R)
b = modulus(T(x + 1))
c = isunit(h)
d = isone(h)
f = iszero(g)
U = parent(g)
V = base_ring(R)
W = base_ring(T(x + 1))
```

## Arithmetic operators

All the usual arithmetic operators are overloaded for Nemo residues. Note
that Julia uses the single slash for floating point division. Therefore to
perform exact division in a ring we use `divexact`. To construct an element
of a fraction field one can use the double slash operator `//`.

```@docs
-(::ResElem)
```

```@docs
+{T <: RingElem}(::ResElem{T}, ::ResElem{T})
```

```@docs
-{T <: RingElem}(::ResElem{T}, ::ResElem{T})
```

```@docs
*{T <: RingElem}(::ResElem{T}, ::ResElem{T})
```

```@docs
divexact{T <: RingElem}(::ResElem{T}, ::ResElem{T})
```

The following ad hoc operators are also provided.

```@docs
+(::Integer, ::ResElem)
+(::ResElem, ::Integer)
+(::fmpz, ::ResElem)
+(::ResElem, ::fmpz)
+{T <: RingElem}(::T, ::ResElem{T})
+{T <: RingElem}(::ResElem{T}, ::T)
```

```@docs
-(::Integer, ::ResElem)
-(::ResElem, ::Integer)
-(::fmpz, ::ResElem)
-(::ResElem, ::fmpz)
-{T <: RingElem}(::T, ::ResElem{T})
-{T <: RingElem}(::ResElem{T}, ::T)
``` 

```@docs
*(::Integer, ::ResElem)
*(::ResElem, ::Integer)
*(::fmpz, ::ResElem)
*(::ResElem, ::fmpz)
*{T <: RingElem}(::T, ::ResElem{T})
*{T <: RingElem}(::ResElem{T}, ::T)
``` 

```@docs
^(::ResElem, ::Int)
```

If the appropriate `promote_rule` and coercion exists, these operators can also
be used with elements of other rings. Nemo will try to coerce the operands to
the dominating type and then apply the operator.

Here are some examples of arithmetic operations on residues.

```
R = ResidueRing(ZZ, 16453889)

S, x = PolynomialRing(R, "x")
T = ResidueRing(S, x^3 + 3x + 1)

f = R(4)
g = R(6)
n = T(x^5 + 1)
p = T(x^2 + 2x + 1)

a = -R(12345)
b = -T(x^5 + 1)
c = f + g
d = f - g
h = f*g
k = n + p
l = n - p
m = n*p
q = divexact(a*b, a)
r = divexact(n*p, p)
s = f + 3
t = 3 - g
u = 5g
v = n + 4
w = 4 - p
y = n*5
z = p^12
```

## Comparison operators

The following comparison operators are implemented for residues in Nemo.

```@docs
=={T <: RingElem}(::ResElem{T}, ::ResElem{T})
```

```@docs
isequal{T <: RingElem}(::ResElem{T}, ::ResElem{T})
```

In addition we have the following ad hoc comparison operators.

```@docs
=={T <: RingElem}(::ResElem{T}, ::T)
=={T <: RingElem}(::T, ::ResElem{T})
==(::ResElem, ::Integer)
==(::Integer, ::ResElem)
==(::ResElem, ::fmpz)
==(::fmpz, ::ResElem)
```

Here are some examples of comparisons.

```
R = ResidueRing(ZZ, 7)
S, x = PolynomialRing(R, "x")
T = ResidueRing(S, x^3 + 3x + 1)

a = R(3)
b = a
c = R(2)
f = T(x^5 + 1)
g = 8f
h = f + g

b == a
c != a
f == g
h != g
isequal(f, g)
a == 3
4 != a
f != 5
```

## Inversion

```@docs
inv(a::ResElem)
```

Here are some examples of computing inverses.

```
R = ResidueRing(ZZ, 49)
S, x = PolynomialRing(R, "x")
T = ResidueRing(S, x^3 + 3x + 1)

a = R(5)
f = T(x^5 + 1)

b = inv(a)
g = inv(f)
```

## Greatest common divisor

```@docs
gcd{T <: RingElem}(a::ResElem{T}, b::ResElem{T})
```

Here are some examples of computing a greatest common divisor.

```
R = ResidueRing(ZZ, 43)
S, x = PolynomialRing(R, "x")
T = ResidueRing(S, x^3 - 2x + 1)

a = R(5)
b = R(7)
c = T(x^5 + 1)
d = T(x^2 + x)

f = gcd(a, b)
g = gcd(c, d)
```