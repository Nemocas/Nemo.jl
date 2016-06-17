```@meta
CurrentModule = Nemo
```

## Introduction

Nemo allow the creation of dense, univariate polynomials over any computable
ring $R$. There are two different kinds of implementation: a generic one for
the case where no specific implementation exists, and efficient implementations
of polynomials over numerous specific rings, usually provided by C/C++
libraries.

The following table shows each of the polynomial types available in Nemo, the
base ring $R$, and the Julia/Nemo types for that kind of polynomial (the type
information is mainly of concern to developers).

Base ring                             | Library             | Element type        | Parent type
--------------------------------------|---------------------|---------------------|----------------------
Generic ring $R$                      | Nemo                | `GenPoly{T}`        | `GenPolyRing{T}`
$\mathbb{Z}$                          | Flint               | `fmpz_poly`         | `FmpzPolyRing`
$\mathbb{Z}/n\mathbb{Z}$ (small $n$)  | Flint               | `nmod_poly`         | `NmodPolyRing`
$\mathbb{Z}/n\mathbb{Z}$ (large $n$)  | Flint               | `fmpz_mod_poly`     | `FmpzModPolyRing`
$\mathbb{Q}$                          | Flint               | `fmpq_poly`         | `FmpqPolyRing`
$\mathbb{F}_{p^n}$ (small $n$)        | Flint               | `fq_nmod_poly`      | `FqNmodPolyRing`
$\mathbb{F}_{p^n}$ (large $n$)        | Flint               | `fq_poly`           | `FqPolyRing`

The string representation of the variable and the base ring $R$ of a generic
polynomial is stored in its parent object. 

All polynomial element types belong to the abstract type `PolyElem` and all of
the polynomial ring types belong to the abstract type `PolyRing`. This enables
one to write generic functions that can accept any Nemo polynomial type.

## Polynomial ring constructors

In order to construct polynomials in Nemo, one must first construct the
polynomial ring itself. This is accomplished with the following constructor.

```@docs
PolynomialRing(::Ring, ::AbstractString{}, ::Bool)
```

A shorthand version of this function is provided: given a base ring `R`, we
abbreviate the constructor as follows.

```
R["x"]
```

Here are some examples of creating polynomial rings and making use of the
resulting parent objects to coerce various elements into the polynomial ring.

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

## Polynomial element constructors

Once a polynomial ring is constructed, there are various ways to construct
polynomials in that ring.

The easiest way is simply using the generator returned by the `PolynomialRing`
constructor and and build up the polynomial using basic arithmetic. Julia has
quite flexible notation for the construction of polynomials in this way.

In addition we provide the following functions for constructing certain useful
polynomials.

```@docs
zero(::PolyRing)
```

```@docs
one(::PolyRing)
```

```@docs
gen(::PolyRing)
```

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

## Basic functionality

All univariate polynomial modules in Nemo must provide the functionality listed
in this section. (Note that only some of these functions are useful to a user.)

Developers who are writing their own polynomial module, whether as an interface
to a C library, or as some kind of generic module, must provide all of these
functions for custom univariate polynomial types in Nemo. 

We write `U` for the type of the polynomials in the polynomial ring and `T` for
the type of elements of the coefficient ring.

All of these functions are provided for all existing polynomial types in Nemo.

```
parent_type{U <: PolyElem}(::Type{U})
```

Given the type of polynomial elements, should return the type of the
corresponding parent object.

```
elem_type(R::PolyRing)
```

Given a parent object for the polynomial ring, return the type of elements
of the polynomial ring.

```
Base.hash(a::PolyElem, h::UInt)
```

Return a `UInt` hexadecimal hash of the polynomial $a$. This should be xor'd
with a fixed random hexadecimal specific to the polynomial type. The hash of
each coefficient should be xor'd with the supplied parameter `h` as part of
computing the hash.

```
fit!(a::PolyElem, n::Int)
```

By reallocating if necessary, ensure that the given polynomial has space for at
least $n$ coefficients. This function does not change the length of the
polynomial and will only ever increase the number of allocated coefficients.
Any coefficients added by this function are initialised to zero.
 
```
normalise(a::PolyElem, n::Int)
```

Return the normalised length of the given polynomial, assuming its current
length is $n$. Its normalised length is such that it either has nonzero
leading term or is the zero polynomial. Note that this function doesn't
normalise the polynomial. That can be done with a subsequent call to
`set_length!` using the length returned by `normalise`.

```
set_length!(a::PolyElem, n::Int)
```

Set the length of an existing polynomial that has sufficient space allocated,
i.e. a polynomial for which no reallocation is needed. Note that if the Julia
type definition for a custom polynomial type has a field, `length`, which
corresponds to the current length of the polynomial, then the developer doesn't
need to supply this function, as the supplied generic implementation will work.
Note that it can change the length to any value from zero to the number of
coefficients currently allocated and initialised.

```
length(a::PolyElem)
```

Return the current length (not the number of allocated coefficients), of the
given polynomial. Note that this function only needs to be provided by a
developer for a custom polynomial type if the Julia type definition for
polynomial elements doesn't contain a field `length` corresponding to the
current length of the polynomial. Otherwise the supplied generic implementation
will work.

```
coeff(a::PolyElem, n::Int)
```

Return the degree `n` coefficient of the given polynomial. Note coefficients
are numbered from `n = 0` for the constant coefficient. If $n$ is bigger
then the degree of the polynomial, the function returns a zero coefficient.
We require $n \geq 0$. 

```
setcoeff!{T <: RingElem}(a::PolyElem{T}, n::Int, c::T)
```

Set the coefficient of the degree $n$ term of the given polynomial to the given
value `a`. The polynomial is not normalised automatically after this operation,
however the polynomial is automatically resized if there is not sufficient
allocated space.

```
deepcopy(a::PolyElem)
```

Construct a copy of the given polynomial and return it. This function must
recursively construct copies of all of the internal data in the given
polynomial. Nemo polynomials are mutable and so returning shallow copies is not
sufficient.

```
mul!(c::PolyElem, a::PolyElem, b::PolyElem)
```

Multiply $a$ by $b$ and set the existing polynomial $c$ to the result. This
function is provided for performance reasons as it saves allocating a new
object for the result and eliminates associated garbage collection.

```
addeq!(c::PolyElem, a::PolyElem)
```

In-place addition. Adds $a$ to $c$ and sets $c$ to the result. This function
is provided for performance reasons as it saves allocating a new object for
the result and eliminates associated garbage collection.

Given a parent object `S` for a polynomial ring, the following coercion
functions are provided to coerce various elements into the polynomial
ring. Developers provide these by overloading the `call` operator for the
polynomial parent objects.

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

Take an array of elements in the base ring, of type `T` and construct the
polynomial with those coefficients, starting with the constant coefficient.

```
S(f::PolyElem)
```

Take a polynomial that is already in the ring $S$ and simply return it. A copy
of the original is not made.

```
S(c::RingElem)
```

Try to coerce the given ring element into the polynomial ring. This only
succeeds if $c$ can be coerced into the base ring.

In addition to the above, developers of custom polynomials must ensure the
parent object of a polynomial type constains a field `base_ring` specifying the
base ring, a field `S` containing a symbol (not a string) representing the
variable name of the polynomial ring. They must also ensure that each
polynomial element contains a field `parent` specifying the parent object of
the polynomial.

Typically a developer will also overload the `PolynomialRing` generic function
to create polynomials of the custom type they are implementing.

## Basic manipulation

Numerous functions are provided to manipulate polynomials and to set and
retrieve coefficients and other basic data associated with the polynomials.
Also see the section on basic functionality above.

```@docs
base_ring(::PolyRing)
```

```@docs
base_ring(::PolyElem)
```

```@docs
parent(::PolyElem)
```

```@docs
var(::PolyRing)
```

```@docs
degree(::PolyElem)
```

```@docs
modulus{T <: ResElem}(::PolyElem{T})
```

```@docs
lead(::PolyElem)
```

```@docs
iszero(::PolyElem)
```

```@docs
isone(::PolyElem)
```

```@docs
isgen(::PolyElem)
```

```@docs
isunit(::PolyElem)
```

```@docs
den(::fmpq_poly)
```

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

## Arithmetic operators

All the usual arithmetic operators are overloaded for Nemo polynomials. Note
that Julia uses the single slash for floating point division. Therefore to
perform exact division in a ring we use `divexact`. To construct an element
of a fraction field one can use the double slash operator `//`.

```@docs
-(::PolyElem)
```

```@docs
+{T <: RingElem}(::PolyElem{T}, ::PolyElem{T})
```

```@docs
-{T <: RingElem}(::PolyElem{T}, ::PolyElem{T})
```

```@docs
*{T <: RingElem}(::PolyElem{T}, ::PolyElem{T})
```

```@docs
divexact{T <: RingElem}(::PolyElem{T}, ::PolyElem{T})
```

The following ad hoc operators are also provided.

```@docs
+(::Integer, ::PolyElem)
+(::PolyElem, ::Integer)
+(::fmpz, ::PolyElem)
+(::PolyElem, ::fmpz)
+{T <: RingElem}(::T, ::PolyElem{T})
+{T <: RingElem}(::PolyElem{T}, ::T)
```

```@docs
-(::Integer, ::PolyElem)
-(::PolyElem, ::Integer)
-(::fmpz, ::PolyElem)
-(::PolyElem, ::fmpz)
-{T <: RingElem}(::T, ::PolyElem{T})
-{T <: RingElem}(::PolyElem{T}, ::T)
``` 

```@docs
*(::Integer, ::PolyElem)
*(::PolyElem, ::Integer)
*(::fmpz, ::PolyElem)
*(::PolyElem, ::fmpz)
*{T <: RingElem}(::T, ::PolyElem{T})
*{T <: RingElem}(::PolyElem{T}, ::T)
``` 

```@docs
divexact(::PolyElem, ::Integer)
divexact(::PolyElem, ::fmpz)
divexact{T <: RingElem}(::PolyElem{T}, ::T)
```

```@docs
^(::PolyElem, ::Int)
```

If the appropriate `promote_rule` and coercion exists, these operators can also
be used with elements of other rings. Nemo will try to coerce the operands to
the dominating type and then apply the operator.

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

## Comparison operators

The following comparison operators are implemented for polynomials in Nemo.

```@docs
=={T <: RingElem}(::PolyElem{T}, ::PolyElem{T})
```

```@docs
isequal{T <: RingElem}(::PolyElem{T}, ::PolyElem{T})
```

In addition we have the following ad hoc comparison operators.

```@docs
=={T <: RingElem}(::PolyElem{T}, ::T)
=={T <: RingElem}(::T, ::PolyElem{T})
==(::PolyElem, ::Integer)
==(::Integer, ::PolyElem)
==(::PolyElem, ::fmpz)
==(::fmpz, ::PolyElem)
```

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

## Truncation

```@docs
truncate(::PolyElem, ::Int)
```

```@docs
mullow{T <: RingElem}(::PolyElem{T}, ::PolyElem{T}, ::Int)
```

Here are some examples of truncated operations.

```
R, x = PolynomialRing(ZZ, "x")
S, y = PolynomialRing(R, "y")

f = x*y^2 + (x + 1)*y + 3
g = (x + 1)*y + (x^3 + 2x + 2)

h = truncate(f, 1)
k = mullow(f, g, 4)
```

## Reversal

```@docs
reverse(::PolyElem, ::Int)
reverse(::PolyElem)
```

Here are some examples of reversal.

```
R, x = PolynomialRing(ZZ, "x")
S, y = PolynomialRing(R, "y")

f = x*y^2 + (x + 1)*y + 3

g = reverse(f, 7)
h = reverse(f)
```

## Shifting

```@docs
shift_left(::PolyElem, ::Int)
```

```@docs
shift_right(::PolyElem, ::Int)
```

Here are some examples of shifting.

```
R, x = PolynomialRing(ZZ, "x")
S, y = PolynomialRing(R, "y")

f = x*y^2 + (x + 1)*y + 3

g = shift_left(f, 7)
h = shift_right(f, 2)
```

## Modulo arithmetic

For polynomials over a field or residue ring, we can reduce modulo a given
polynomial. This isn't always well-defined in the case of a residue ring,
but when it is well-defined, we obtain the correct result. If Nemo encounters
an impossible inverse, an exception will be raised.

```@docs
mulmod{T <: Union{ResElem, FieldElem}}(::PolyElem{T}, ::PolyElem{T}, ::PolyElem{T})
```

```@docs
powmod{T <: Union{ResElem, FieldElem}}(::PolyElem{T}, ::Int, ::PolyElem{T})
```

```@docs
powmod(::fmpz_mod_poly, ::fmpz, ::fmpz_mod_poly)
```

```@docs
invmod{T <: Union{ResElem, FieldElem}}(::PolyElem{T}, ::PolyElem{T})
```

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

## Euclidean division

For polynomials over a field, we have a euclidean domain, and in many cases for
polynomials over a residue ring things behave as though we had a euclidean
domain so long as we don't hit an impossible inverse. For such rings we define
euclidean division of polynomials. If an impossible inverse is hit, we raise an
exception.

```@docs
mod{T <: Union{ResElem, FieldElem}}(::PolyElem{T}, ::PolyElem{T})
```

```@docs
divrem{T <: Union{ResElem, FieldElem}}(::PolyElem{T}, ::PolyElem{T})
```

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

## Pseudodivision

Given two polynomials $a, b$, pseudodivision computes polynomials $q$ and $r$
with length$(r) <$ length$(b)$ such that
$$L^d a = bq + r,$$
where $d =$ length$(a) -$ length$(b) + 1$ and $L$ is the leading coefficient
of $b$.

We call $q$ the pseudoquotient and $r$ the pseudoremainder.

```@docs
pseudorem{T <: RingElem}(::PolyElem{T}, ::PolyElem{T})
```

```@docs
pseudodivrem{T <: RingElem}(::PolyElem{T}, ::PolyElem{T})
```

Here are some examples of pseudodivision.

```
R, x = PolynomialRing(ZZ, "x")
S, y = PolynomialRing(R, "y")

f = x*y^2 + (x + 1)*y + 3
g = (x + 1)*y + (x^3 + 2x + 2)

h = pseudorem(f, g)
q, r = pseudodivrem(f, g)
```

## Content, primitive part, GCD and LCM

In Nemo, we allow computation of the greatest common divisor of polynomials
over any ring. This is enabled by making use of pseudoremainders when we
aren't working over a euclidean domain or something mimicking such a domain.
In certain cases this allows us to return a greatest common divisor when it
otherwise wouldn't be possible. However, a greatest common divisor is not
necessarily unique, or even well-defined.

If an impossible inverse is encountered whilst computing the greatest common
divisor, an exception is thrown.

```@docs
gcd{T <: RingElem}(::PolyElem{T}, ::PolyElem{T})
```

```@docs
lcm{T <: RingElem}(::PolyElem{T}, ::PolyElem{T})
```

```@docs
content(::PolyElem)
```

```@docs
primpart(::PolyElem)
```

```@docs
gcdx{T <: RingElem}(::PolyElem{T}, ::PolyElem{T})
gcdx{T <: Union{ResElem, FieldElem}}(::PolyElem{T}, ::PolyElem{T})
```

```@docs
gcdinv{T <: Union{ResElem, FieldElem}}(::PolyElem{T}, ::PolyElem{T})
```

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

## Evaluation, composition and substitution

```@docs
evaluate{T <: RingElem}(::PolyElem{T}, ::T)
evaluate(::PolyElem, ::Integer)
evaluate(::PolyElem, ::fmpz)
```

```@docs
compose(::PolyElem, ::PolyElem)
```

```@docs
subst{T <: RingElem}(::PolyElem{T}, ::Any)
```

We also overload the functional notation so that the polynomial $f$ can be
evaluated at $a$ by writing $f(a)$. This feature is only available with 
Julia 0.5 however.

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

## Derivative and integral

```@docs
derivative(::PolyElem)
```

```@docs
integral{T <: Union{ResElem, FieldElem}}(::PolyElem{T})
```

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

## Resultant and discriminant

```@docs
resultant{T <: RingElem}(::PolyElem{T}, ::PolyElem{T})
```

```@docs
discriminant(a::PolyElem)
```

Here are some examples of computing the resultant and discriminant.

```
R, x = PolynomialRing(ZZ, "x")
S, y = PolynomialRing(R, "y")

f = 3x*y^2 + (x + 1)*y + 3
g = 6(x + 1)*y + (x^3 + 2x + 2)

h = resultant(f, g)
k = discriminant(f)
```

## Newton representation

```@docs
monomial_to_newton!{T <: RingElem}(::Array{T, 1}, ::Array{T, 1})
```

```@docs
newton_to_monomial!{T <: RingElem}(::Array{T, 1}, ::Array{T, 1})
```

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

## Interpolation

```@docs
interpolate{T <: RingElem}(::PolyRing, ::Array{T, 1}, ::Array{T, 1})
```

Here is an example of interpolation.

```
R, x = PolynomialRing(ZZ, "x")
S, y = PolynomialRing(R, "y")

xs = [R(1), R(2), R(3), R(4)]
ys = [R(1), R(4), R(9), R(16)]

f = interpolate(S, xs, ys)
```

## Signature

Signature is only available for certain coefficient rings.

```@docs
signature(::fmpz_poly)
signature(::fmpq_poly)
```

Here is an example of signature.

```
R, x = PolynomialRing(ZZ, "x")

f = x^3 + 3x + 1

(r, s) = signature(f)
```

## Lifting

When working over a residue ring it is useful to be able to lift to the base
ring of the residue ring, e.g. from $\mathbb{Z}/n\mathbb{Z}$ to $\mathbb{Z}$.

```@docs
lift(::FmpzPolyRing, ::nmod_poly)
lift(::FmpzPolyRing, ::fmpz_mod_poly)
```

Here is an example of lifting.

```
R = ResidueRing(ZZ, 123456789012345678949)
S, x = PolynomialRing(R, "x")
T, y = PolynomialRing(ZZ, "y")

f = x^2 + 2x + 1

a = lift(T, f)
```

## Factorisation

Polynomials can only be factorised over certain rings. In general we use the
same format for the output as the Julia factorisation function, namely an
associative array with polynomial factors as keys and exponents as values.

```@docs
isirreducible(::nmod_poly)
isirreducible(::fmpz_mod_poly)
```

```@docs
issquarefree(::nmod_poly)
issquarefree(::fmpz_mod_poly)
```

```@docs
factor(::nmod_poly)
factor(::fmpz_mod_poly)
```

```@docs
factor_squarefree(::nmod_poly)
factor_squarefree(::fmpz_mod_poly)
```

```@docs
factor_distinct_deg(::nmod_poly)
factor_distinct_deg(::fmpz_mod_poly)
```

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

## Special functions

The following special functions can be computed for any polynomial ring.
Typically one uses the generator $x$ of a polynomial ring to get the respective
special polynomials expressed in terms of that generator.

```@docs
chebyshev_t(::Int, ::PolyElem)
```

```@docs
chebyshev_u(::Int, ::PolyElem)
```

The following special polynomials are only available for certain base rings.

```@docs
cyclotomic(::Int, ::fmpz_poly)
```

```@docs
swinnerton_dyer(::Int, ::fmpz_poly)
```

```@docs
cos_minpoly(::Int, ::fmpz_poly)
```

```@docs
theta_qexp(::Int, ::Int, ::fmpz_poly)
```

```@docs
eta_qexp(::Int, ::Int, ::fmpz_poly)
```

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
