```@meta
CurrentModule = Nemo
```

## Introduction

Nemo allows the creation of capped relative power series over any computable
ring $R$. These are power series of the form
$a_jx^j + a_{j+1}x^{j+1} + \cdots + a_{k-1}x^{k-1} + O(x^k)$
where $i \geq 0$, $a_i \in R$ and the relative precision $k - j$ is at most
equal to some specified precision $n$.

There are two different kinds of implementation: a generic one for
the case where no specific implementation exists, and efficient implementations
of power series over numerous specific rings, usually provided by C/C++
libraries.

The following table shows each of the relative power series types available in
Nemo, the base ring $R$, and the Julia/Nemo types for that kind of series (the
type information is mainly of concern to developers).

Base ring                             | Library         | Element type          | Parent type
--------------------------------------|-----------------|-----------------------|----------------------
Generic ring $R$                      | Nemo            | `GenRelSeries{T}`     | `GenRelSeriesRing{T}`
$\mathbb{Z}$                          | Flint           | `fmpz_rel_series`     | `FmpzRelSeriesRing`
$\mathbb{Z}/n\mathbb{Z}$              | Flint           | `fmpz_mod_rel_series` | `FmpzModRelSeriesRing`
$\mathbb{Q}$                          | Flint           | `fmpq_rel_series`     | `FmpqRelSerieRing`
$\mathbb{F}_{p^n}$ (small $n$)        | Flint           | `fq_nmod_rel_series`  | `FqNmodRelSeriesRing`
$\mathbb{F}_{p^n}$ (large $n$)        | Flint           | `fq_rel_series`       | `FqRelSeriesRing`

The maximum relative precision, the string representation of the variable and
the base ring $R$ of a generic power series are stored in its parent object. 

All power series element types belong to the abstract type `SeriesElem` and all
of the power series ring types belong to the abstract type `SeriesRing`. This
enables one to write generic functions that can accept any Nemo power series
type.

## Capped relative power series

Capped relative power series have their maximum relative precision capped at
some value `prec_max`. This means that if the leading term of a nonzero
power series element is $c_ax^a$ and the precision is $b$ then the power series
is of the form  $c_ax^a + c_{a+1}x^{a+1} + \ldots + O(x^{a + b})$.

The zero power series is simply taken to be $0 + O(x^b)$.

The capped relative model has the advantage that power series are stable
multiplicatively. In other words, for nonzero power series $f$ and $g$ we
have that `divexact(f*g), g) == f`.

However, capped relative power series are not additively stable, i.e. we
do not always have $(f + g) - g = f$.

In the capped relative model we say that two power series are equal if they
agree up to the minimum *absolute* precision of the two power series.
Thus, for example, $x^5 + O(x^10) == 0 + O(x^5)$, since the minimum absolute
precision is $5$.

During computations, it is possible for power series to lose relative
precision due to cancellation. For example if $f = x^3 + x^5 + O(x^8)$ and
$g = x^3 + x^6 + O(x^8)$ then $f - g = x^5 - x^6 + O(x^8)$ which now has
relative precision $3$ instead of relative precision $5$.

Amongst other things, this means that equality is not transitive. For example
$x^6 + O(x^11) == 0 + O(x^5)$ and $x^7 + O(x^12) == 0 + O(x^5)$ but
$x^6 + O(x^11) \neq x^7 + O(x^12)$.

Sometimes it is necessary to compare power series not just for arithmetic
equality, as above, but to see if they have precisely the same precision and
terms. For this purpose we introduce the `isequal` function.

For example, if $f = x^2 + O(x^7)$ and $g = x^2 + O(x^8)$ and $h = 0 + O(x^2)$
then $f == g$, $f == h$ and $g == h$, but `isequal(f, g)`, `isequal(f, h)` and
`isequal(g, h)` would all return `false`. However, if $k = x^2 + O(x^7)$ then
`isequal(f, k)` would return `true`.

There are further difficulties if we construct polynomial over power series.
For example, consider the polynomial in $y$ over the power series ring in $x$
over the rationals. Normalisation of such polynomials is problematic. For
instance, what is the leading coefficient of $(0 + O(x^10))y + (1 + O(x^10))$?

If one takes it to be $(0 + O(x^10))$ then some functions may not terminate
due to the fact that algorithms may require the degree of polynomials to
decrease with each iteration. Instead, the degree may remain constant and
simply accumulate leading terms which are arithmetically zero but not
identically zero.

On the other hand, when constructing power series over other power series, if
we simply throw away terms which are arithmetically equal to zero, our
computations may have different output depending on the order in which the
power series are added!

One should be aware of these difficulties when working with power series.
Power series, as represented on a computer, simply don't satisfy the axioms
of a ring. They must be used with care in order to approximate operations in
a mathematical power series ring.

Simply increasing the precision will not necessarily give a "more correct"
answer and some computations may not even terminate due to the presence of
arithmetic zeroes!

## Power series ring constructors

In order to construct power series in Nemo, one must first construct the
power series ring itself. This is accomplished with the following constructor.

```@docs
PowerSeriesRing(::Ring, ::Int, ::AbstractString{}, ::Bool)
```

Here are some examples of creating a power series ring using the constructor
and using the resulting parent object to coerce various elements into the power
series ring.

```
R, t = PolynomialRing(QQ, "t")
S, x = PowerSeriesRing(R, 30, "x")

a = S(x)
b = S(t + 1)
c = S(1)
d = S(ZZ(2))
f = S()
```

## Power series element constructors

Once a power series ring is constructed, there are various ways to construct
power series in that ring.

The easiest way is simply using the generator returned by the `PowerSeriesRing`
constructor and and build up the power series using basic arithmetic. The
absolute precision of a power series can be set using the following function.

```@docs
O{T}(a::SeriesElem{T})
```

In addition we provide the following functions for constructing certain useful
polynomials.

```@docs
zero(::SeriesRing)
```

```@docs
one(::SeriesRing)
```

```@docs
gen(::SeriesRing)
```

Here are some examples of constructing power series.

```
R, t = PolynomialRing(QQ, "t")
S, x = PowerSeriesRing(R, 30, "x")

a = x^3 + 2x + 1
b = (t^2 + 1)*x^2 + (t + 3)x + O(x^4)
c = zero(S)
d = one(S)
f = gen(S)
```

## Basic functionality

All power series modules in Nemo must provide the functionality listed
in this section. (Note that only some of these functions are useful to a user.)

Developers who are writing their own power series module, whether as an
interface to a C library, or as some kind of generic module, must provide all
of these functions for custom power series types in Nemo. 

We write `U` for the type of the power series in the power series ring and `T`
for the type of elements of the coefficient ring.

All of these functions are provided for all existing power series types in
Nemo.

```
parent_type{U <: SeriesElem}(::Type{U})
```

Given the type of power series elements, should return the type of the
corresponding parent object.

```
elem_type(R::SeriesRing)
```

Given a parent object for the power series ring, return the type of elements
of the power series ring.

```
Base.hash(a::SeriesElem, h::UInt)
```

Return a `UInt` hexadecimal hash of the power series $a$. This should be xor'd
with a fixed random hexadecimal specific to the power series type. The hash of
each coefficient should be xor'd with the supplied parameter `h` as part of
computing the hash.

```
fit!(a::SeriesElem, n::Int)
```

By reallocating if necessary, ensure that the polynomial underlying the given
power series has space for at least $n$ coefficients. This function does not
change the length of the power series and will only ever increase the number
of allocated coefficients. Any coefficients added by this function are
initialised to zero.
 
```
normalise(a::SeriesElem, n::Int)
```

Return the normalised length of the polynomial underlying the given power
series, assuming its current length is $n$. Its normalised length is such that
it either has nonzero leading term or is the zero polynomial. Note that this
function doesn't normalise the power series. That can be done with a subsequent
call to `set_length!` using the length returned by `normalise`.

```
set_length!(a::SeriesElem, n::Int)
```

Set the length of the polynomial underlying a power series assuming it has
sufficient space allocated, i.e. a power series for which no reallocation is
needed. Note that if the Julia type definition for a custom polynomial power
series type has a field, `length`, which corresponds to the current length of
the polynomial underlying a power series, then the developer doesn't need to
supply this function, as the supplied generic implementation will work. Note
that it can change the length to any value from zero to the number of
coefficients currently allocated and initialised.

```
length(a::SeriesElem)
```

Return the current length (not the number of allocated coefficients), of the
polynomial underlying the given power series. Note that this function only
needs to be provided by a developer for a custom power series type if the Julia
type definition the power series type doesn't contain a field `length`
corresponding to the current length of the polynomial underlying the power
series. Otherwise the supplied generic implementation will work.

```
set_prec!(a::SeriesElem, n::Int)
```

Set the precision of the given power series to $n$. Note this function only
needs to be provided by a developer for a custom power series type if the Julia
type definition of the power series type doesn't contain a field `prec`
corresponding to the current precision of the power series. Otherwise the
supplied generic implementation will work.

```
precision(a::SeriesElem)
```

Return the current precision of the given power series. This function does not
have to be provided by a developer of a custom power series type if the Julia
type definition of the power series type contains a field `prec` corresponding
to the current precision of the power series. In this case the supplied generic
implementation will work. Note that for convenience, the precision is stored as
an absolute precision.

```
coeff(a::SeriesElem, n::Int)
```

Return the degree $n$ coefficient of the given power series. Note coefficients
are numbered from $n = 0$ for the constant coefficient. If $n$ exceeds the
current precision of the power series, the function returns a zero coefficient.
We require $n \geq 0$. 

```
setcoeff!{T <: RingElem}(a::SeriesElem{T}, n::Int, c::T)
```

Set the coefficient of the degree $n$ term of the given power series to
the given value $a$. The polynomial underlying the power series is not
normalised automatically after this operation, however the polynomial is
automatically resized if there is not sufficient allocated space.

```
deepcopy(a::SeriesElem)
```

Construct a copy of the given power series and return it. This function must
recursively construct copies of all of the internal data in the given
polynomial. Nemo power series are mutable and so returning shallow copies is
not sufficient.

```
mul!(c::SeriesElem, a::SeriesElem, b::SeriesElem)
```

Multiply $a$ by $b$ and set the existing power series $c$ to the result. This
function is provided for performance reasons as it saves allocating a new
object for the result and eliminates associated garbage collection.

```
addeq!(c::SeriesElem, a::SeriesElem)
```

In-place addition. Adds $a$ to $c$ and sets $c$ to the result. This function
is provided for performance reasons as it saves allocating a new object for
the result and eliminates associated garbage collection.

Given a parent object `S` for a power series ring, the following coercion
functions are provided to coerce various elements into the power series
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

Coerce an integer value or Flint integer into the power series ring $S$.

```
S(n::T)
```

Coerces an element of the base ring, of type `T` into $S$.

```
S(A::Array{T, 1}, len::Int, prec::Int)
```

Take an array of elements in the base ring, of type `T` and construct the
power series with those coefficients. The length of the underlying polynomial
and the precision of the power series will be set to the given values.

```
S(f::SeriesElem)
```

Take a power series that is already in the ring $S$ and simply return it. A
copy of the original is not made.

```
S(c::RingElem)
```

Try to coerce the given ring element into the power series ring. This only
succeeds if $c$ can be coerced into the base ring.

In addition to the above, developers of custom power series must ensure the
parent object of a power series type constains a field `base_ring` specifying
the base ring, a field `S` containing a symbol (not a string) representing the
variable name of the power series ring and a field `max_prec` specifying the
maximum relative precision of the power series. They must also ensure that each
power series element contains a field `parent` specifying the parent object of
the power series.

Typically a developer will also overload the `PowerSeriesRing` generic function
to create power series of the custom type they are implementing.

## Basic manipulation

Numerous functions are provided to manipulate polynomials and to set and
retrieve coefficients and other basic data associated with the polynomials.
Also see the section on basic functionality above.

```@docs
base_ring(::SeriesRing)
```

```@docs
base_ring(::SeriesElem)
```

```@docs
parent(::SeriesElem)
```

```@docs
var(::SeriesRing)
```

```@docs
valuation(::SeriesElem)
```

```@docs
max_precision(R::SeriesRing)
```

```@docs
modulus{T <: ResElem}(::SeriesElem{T})
```

```@docs
iszero(::SeriesElem)
```

```@docs
isone(::SeriesElem)
```

```@docs
isgen(::SeriesElem)
```

```@docs
isunit(::SeriesElem)
```

Here are some examples of basic manipulation of power series.

```
R, t = PowerSeriesRing(QQ, 10, "t")
S, x = PowerSeriesRing(R, 30, "x")

a = O(x^4)
b = (t^2 + 1)*x^2 + (t + 3)x + O(x^4)

c = gen(R)
d = zero(R)
f = one(S)

g = iszero(d)
h = isone(f)
k = isgen(c)
m = isunit(-1 + x + 2x^2)
n = valuation(a)
p = valuation(b)
s = var(S)
U = base_ring(S)
V = base_ring(t)
W = parent(t + 1)
```

## Arithmetic operators

All the usual arithmetic operators are overloaded for Nemo power series. Note
that Julia uses the single slash for floating point division. Therefore to
perform exact division in a ring we use `divexact`. To construct an element
of a fraction field one can use the double slash operator `//`.

```@docs
-(::SeriesElem)
```

```@docs
+{T <: RingElem}(::SeriesElem{T}, ::SeriesElem{T})
```

```@docs
-{T <: RingElem}(::SeriesElem{T}, ::SeriesElem{T})
```

```@docs
*{T <: RingElem}(::SeriesElem{T}, ::SeriesElem{T})
```

```@docs
divexact{T <: RingElem}(::SeriesElem{T}, ::SeriesElem{T})
```

The following ad hoc operators are also provided.

```@docs
+(::Integer, ::SeriesElem)
+(::SeriesElem, ::Integer)
+(::fmpz, ::SeriesElem)
+(::SeriesElem, ::fmpz)
+{T <: RingElem}(::T, ::SeriesElem{T})
+{T <: RingElem}(::SeriesElem{T}, ::T)
```

```@docs
-(::Integer, ::SeriesElem)
-(::SeriesElem, ::Integer)
-(::fmpz, ::SeriesElem)
-(::SeriesElem, ::fmpz)
-{T <: RingElem}(::T, ::SeriesElem{T})
-{T <: RingElem}(::SeriesElem{T}, ::T)
``` 

```@docs
*(::Integer, ::SeriesElem)
*(::SeriesElem, ::Integer)
*(::fmpz, ::SeriesElem)
*(::SeriesElem, ::fmpz)
*{T <: RingElem}(::T, ::SeriesElem{T})
*{T <: RingElem}(::SeriesElem{T}, ::T)
``` 

```@docs
divexact(::SeriesElem, ::Integer)
divexact(::SeriesElem, ::fmpz)
divexact{T <: RingElem}(::SeriesElem{T}, ::T)
```

```@docs
^(::SeriesElem, ::Int)
```

If the appropriate `promote_rule` and coercion exists, these operators can also
be used with elements of other rings. Nemo will try to coerce the operands to
the dominating type and then apply the operator.

Here are some examples of arithmetic operations on power series.

```
R, t = PolynomialRing(QQ, "t")
S, x = PowerSeriesRing(R, 30, "x")

a = 2x + x^3
b = O(x^4)
c = 1 + x + 2x^2 + O(x^5)
d = x^2 + 3x^3 - x^4

f = -a
g = a + b
h = a - c
k = b*c
m = a*c
n = a*d
p = 2a
q = fmpz(3)*b
r = c*2
s = d*fmpz(3)
t = a^12
u = divexact(b, c)
v = divexact(a, 7)
w = divexact(b, fmpz(11))
```

## Comparison operators

The following comparison operators are implemented for power series in Nemo.

```@docs
=={T <: RingElem}(::SeriesElem{T}, ::SeriesElem{T})
```

```@docs
isequal{T <: RingElem}(::SeriesElem{T}, ::SeriesElem{T})
```

In addition we have the following ad hoc comparison operators.

```@docs
=={T <: RingElem}(::SeriesElem{T}, ::T)
=={T <: RingElem}(::T, ::SeriesElem{T})
==(::SeriesElem, ::Integer)
==(::Integer, ::SeriesElem)
==(::SeriesElem, ::fmpz)
==(::fmpz, ::SeriesElem)
```

Here are some examples of comparisons.

```
R, t = PolynomialRing(QQ, "t")
S, x = PowerSeriesRing(R, 30, "x")

a = 2x + x^3
b = O(x^3)
c = 1 + x + 3x^2 + O(x^5)
d = 3x^3 - x^4

a == 2x + x^3
b == d
c != d
isequal(b, d)
d == 3
c == fmpz(1)
fmpz(0) != a
2 == b
fmpz(1) == c
```

## Shifting

```@docs
shift_left(::SeriesElem, ::Int)
```

```@docs
shift_right(::SeriesElem, ::Int)
```

Here are some examples of shifting.

```
R, t = PolynomialRing(QQ, "t")
S, x = PowerSeriesRing(R, 30, "x")

a = 2x + x^3
b = O(x^4)
c = 1 + x + 2x^2 + O(x^5)
d = 2x + x^3 + O(x^4)

f = shift_left(a, 2)
g = shift_left(b, 2)
h = shift_right(c, 1)
k = shift_right(d, 3)
```

## Truncation

```@docs
truncate(::SeriesElem, ::Int)
```

Here are some examples of truncation.

```
R, t = PolynomialRing(QQ, "t")
S, x = PowerSeriesRing(R, 30, "x")

a = 2x + x^3
b = O(x^4)
c = 1 + x + 2x^2 + O(x^5)
d = 2x + x^3 + O(x^4)

f = truncate(a, 3)
g = truncate(b, 2)
h = truncate(c, 7)
k = truncate(d, 5)
```

## Inverse

```@docs
inv(a::SeriesElem)
```

Here are some examples of taking the inverse.

```
R, t = PolynomialRing(QQ, "t")
S, x = PowerSeriesRing(R, 30, "x")

a = 1 + x + 2x^2 + O(x^5)
b = S(-1)

c = inv(a)
d = inv(b)
```

## Special functions

```@docs
exp(a::SeriesElem)
```

The following special functions are only available for certain rings.

```@docs
log(a::fmpq_rel_series)
```

```@docs
sqrt(a::fmpq_rel_series)
```

```@docs
tan(a::fmpq_rel_series)
tanh(a::fmpq_rel_series)
sin(a::fmpq_rel_series)
sinh(a::fmpq_rel_series)
cos(a::fmpq_rel_series)
cosh(a::fmpq_rel_series)
```

```@docs
asin(a::fmpq_rel_series)
asinh(a::fmpq_rel_series)
atan(a::fmpq_rel_series)
atanh(a::fmpq_rel_series)
```

Here are some examples of special functions.

```
R, t = PolynomialRing(QQ, "t")
S, x = PowerSeriesRing(R, 30, "x")
T, z = PowerSeriesRing(QQ, 30, "z")

a = 1 + z + 3z^2 + O(z^5)
b = z + 2z^2 + 5z^3 + O(z^5)

c = exp(x + O(x^40))
d = divexact(x, exp(x + O(x^40)) - 1)
f = exp(b)
g = log(a)
h = sqrt(a)
k = sin(b)
m = atanh(b)
```