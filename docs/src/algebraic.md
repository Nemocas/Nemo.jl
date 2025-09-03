```@meta
CurrentModule = Nemo
CollapsedDocStrings = true
DocTestSetup = quote
    using Nemo
end
```

# Algebraic closure of the rational numbers

An implementation of the field of all algebraic numbers, that is, an algebraic closure of the field of rational numbers, is provided through the type `QQBarField` and the corresponding element type `QQBarFieldElem`.

Note that the field of algebraic closure is implemented as an ordered field;
see [Comparing algebraic numbers](@ref).

## Constructing the field of algebraic numbers

```jldoctest
julia> algebraic_closure(QQ)
Algebraic closure of rational field
```

### Printing

Before looking at examples, we comment on the printing of algebraic numbers.
As an illustration, consider the algebraic number $\sqrt[3]{2}$ with minimal polynomial $X^3 - 2$. This is printed as follows:

```jldoctest
julia> Qbar = algebraic_closure(QQ);

julia> z = root(Qbar(2), 3)
{a3: 1.25992}

julia> Qx, x = QQ[:x]
(Univariate polynomial ring in x over QQ, x)

julia> minpoly(Qx, z) # to see the minimal polynomial
x^3 - 2
```
The first part `a3` indicates that this is an algebraic number of degree $3$, which is followed by `1.25992`, the approximation of the root.

## Algebraic number functionality

Methods to construct algebraic numbers include:

* Conversion from other numbers and through arithmetic operations
* Computing the roots of a given polynomial
* Computing the eigenvalues of a given matrix
* Random generation
* Exact trigonometric functions (see later section)
* Guessing (see later section)

**Examples**

Arithmetic:

```jldoctest
julia> Qb = algebraic_closure(QQ);

julia> ZZRingElem(Qb(3))
3

julia> QQFieldElem(Qb(3) // 2)
3//2

julia> Qb(-1) ^ (Qb(1) // 3)
{a2: 0.500000 + 0.866025*im}
```

Solving the quintic equation:

```jldoctest
julia> Qb = algebraic_closure(QQ);

julia> R, x = polynomial_ring(QQ, "x")
(Univariate polynomial ring in x over QQ, x)

julia> v = roots(Qb, x^5-x-1)
5-element Vector{QQBarFieldElem}:
 {a5: 1.16730}
 {a5: 0.181232 + 1.08395*im}
 {a5: 0.181232 - 1.08395*im}
 {a5: -0.764884 + 0.352472*im}
 {a5: -0.764884 - 0.352472*im}

julia> v[1]^5 - v[1] - 1 == 0
true
```

Computing exact eigenvalues of a matrix:

```jldoctest
julia> Qb = algebraic_closure(QQ);

julia> eigenvalues(Qb, ZZ[1 1 0; 0 1 1; 1 0 1])
3-element Vector{QQBarFieldElem}:
 {a1: 2.00000}
 {a2: 0.500000 + 0.866025*im}
 {a2: 0.500000 - 0.866025*im}
```

**Interface**

```@docs
roots(R::QQBarField, f::ZZPolyRingElem)
roots(R::QQBarField, f::QQPolyRingElem)
eigenvalues(R::QQBarField, A::ZZMatrix)
eigenvalues_with_multiplicities(R::QQBarField, A::ZZMatrix)
eigenvalues(R::QQBarField, A::QQMatrix)
eigenvalues_with_multiplicities(R::QQBarField, A::QQMatrix)
rand(R::QQBarField; degree::Int, bits::Int, randtype::Symbol=:null)
```

### Numerical evaluation

**Examples**

Algebraic numbers can be evaluated
numerically to arbitrary precision by converting
to real or complex Arb fields:

```jldoctest
julia> Qb = algebraic_closure(QQ);

julia> RR = ArbField(64); RR(sqrt(Qb(2)))
[1.414213562373095049 +/- 3.45e-19]

julia> CC = AcbField(32); CC(Qb(-1) ^ (Qb(1) // 4))
[0.707106781 +/- 2.74e-10] + [0.707106781 +/- 2.74e-10]*im
```

### Minimal polynomials, conjugates, and properties

**Examples**

Retrieving the minimal polynomial and algebraic conjugates
of a given algebraic number:

```jldoctest
julia> Qb = algebraic_closure(QQ);

julia> minpoly(polynomial_ring(ZZ, "x")[1], Qb(1+2im))
x^2 - 2*x + 5

julia> conjugates(Qb(1+2im))
2-element Vector{QQBarFieldElem}:
 {a2: 1.00000 + 2.00000*im}
 {a2: 1.00000 - 2.00000*im}
```

**Interface**

```@docs
iszero(x::QQBarFieldElem)
isone(x::QQBarFieldElem)
isinteger(x::QQBarFieldElem)
is_rational(x::QQBarFieldElem)
isreal(x::QQBarFieldElem)
degree(x::QQBarFieldElem)
is_algebraic_integer(x::QQBarFieldElem)
minpoly(R::ZZPolyRing, x::QQBarFieldElem)
minpoly(R::QQPolyRing, x::QQBarFieldElem)
conjugates(a::QQBarFieldElem)
denominator(x::QQBarFieldElem)
numerator(x::QQBarFieldElem)
height(x::QQBarFieldElem)
height_bits(x::QQBarFieldElem)
```

### Complex parts

**Examples**

```jldoctest
julia> Qb = algebraic_closure(QQ);

julia> real(sqrt(Qb(1im)))
{a2: 0.707107}

julia> abs(sqrt(Qb(1im)))
{a1: 1.00000}

julia> floor(sqrt(Qb(1000)))
{a1: 31.0000}

julia> sign(Qb(-10-20im))
{a4: -0.447214 - 0.894427*im}
```

**Interface**

```@docs
real(a::QQBarFieldElem)
imag(a::QQBarFieldElem)
abs(a::QQBarFieldElem)
abs2(a::QQBarFieldElem)
conj(a::QQBarFieldElem)
sign(a::QQBarFieldElem)
csgn(a::QQBarFieldElem)
sign_real(a::QQBarFieldElem)
sign_imag(a::QQBarFieldElem)
```

### Comparing algebraic numbers

The operators `==` and `!=` check exactly for equality.

We provide various comparison functions for ordering algebraic numbers:

* Standard comparison for real numbers (`<`, `isless`)
* Real parts
* Imaginary parts
* Absolute values
* Absolute values of real or imaginary parts
* Root sort order 

The standard comparison will throw if either argument is nonreal.

The various comparisons for complex parts are provided as separate operations
since these functions are far more efficient than explicitly computing the
complex parts and then doing real comparisons.

The root sort order is a total order for complex algebraic numbers
used to order the output of `roots` and `conjugates` canonically.
We define this order as follows: real roots come first, in descending order.
Nonreal roots are subsequently ordered first by real part in descending order,
then in ascending order by the absolute value of the imaginary part, and then
in descending order of the sign of the imaginary part. This implies that
complex conjugate roots are adjacent, with the root in the upper half plane
first.

**Examples**

```jldoctest
julia> Qb = algebraic_closure(QQ);

julia> 1 < sqrt(Qb(2)) < Qb(3)//2
true

julia> x = Qb(3+4im)
{a2: 3.00000 + 4.00000*im}

julia> is_equal_abs(x, -x)
true

julia> is_equal_abs_imag(x, 2-x)
true

julia> is_less_real(x, x // 2)
false
```

**Interface**

```@docs
is_equal_real(a::QQBarFieldElem, b::QQBarFieldElem)
is_equal_imag(a::QQBarFieldElem, b::QQBarFieldElem)
is_equal_abs(a::QQBarFieldElem, b::QQBarFieldElem)
is_equal_abs_real(a::QQBarFieldElem, b::QQBarFieldElem)
is_equal_abs_imag(a::QQBarFieldElem, b::QQBarFieldElem)
is_less_real(a::QQBarFieldElem, b::QQBarFieldElem)
is_less_imag(a::QQBarFieldElem, b::QQBarFieldElem)
is_less_abs(a::QQBarFieldElem, b::QQBarFieldElem)
is_less_abs_real(a::QQBarFieldElem, b::QQBarFieldElem)
is_less_abs_imag(a::QQBarFieldElem, b::QQBarFieldElem)
is_less_root_order(a::QQBarFieldElem, b::QQBarFieldElem)
```

### Roots and trigonometric functions

**Examples**

```jldoctest
julia> Qb =  algebraic_closure(QQ);

julia> root(Qb(2), 5)
{a5: 1.14870}

julia> sinpi(Qb(7) // 13)
{a12: 0.992709}

julia> tanpi(atanpi(sqrt(Qb(2)) + 1))
{a2: 2.41421}

julia> root_of_unity(Qb, 5)
{a4: 0.309017 + 0.951057*im}

julia> root_of_unity(Qb, 5, 4)
{a4: 0.309017 - 0.951057*im}

julia> w = (1 - sqrt(Qb(-3)))//2
{a2: 0.500000 - 0.866025*im}

julia> is_root_of_unity(w)
true

julia> is_root_of_unity(w + 1)
false

julia> root_of_unity_as_args(w)
(6, 5)
```

**Interface**

```@docs
sqrt(a::QQBarFieldElem)
root(a::QQBarFieldElem, n::Int)
root_of_unity(C::QQBarField, n::Int)
root_of_unity(C::QQBarField, n::Int, k::Int)
is_root_of_unity(a::QQBarFieldElem)
root_of_unity_as_args(a::QQBarFieldElem)
exp_pi_i(a::QQBarFieldElem)
log_pi_i(a::QQBarFieldElem)
sinpi(a::QQBarFieldElem)
cospi(a::QQBarFieldElem)
sincospi(a::QQBarFieldElem)
tanpi(a::QQBarFieldElem)
asinpi(a::QQBarFieldElem)
acospi(a::QQBarFieldElem)
atanpi(a::QQBarFieldElem)
```

### Guessing

**Examples**

An algebraic number can be recovered from a numerical value:

```jldoctest
julia> Qb = algebraic_closure(QQ);

julia> RR = real_field(); guess(Qb, RR("1.41421356 +/- 1e-6"), 2)
{a2: 1.41421}
```

Warning: the input should be an enclosure. If you have a floating-point
approximation, you should add an error estimate; otherwise, at best the only
algebraic number that can be guessed is the binary floating-point number
itself, at worst no guess is possible.

```jldoctest
julia> Qb = algebraic_closure(QQ);

julia> RR = real_field();

julia> x = RR(0.1)       # note: 53-bit binary approximation of 1//10 without radius
[0.10000000000000000555 +/- 1.12e-21]

julia> guess(Qb, x, 1)
ERROR: No suitable algebraic number found

julia> guess(Qb, x + RR("+/- 1e-10"), 1)
{a1: 0.100000}
```

**Interface**

```@docs
guess
```

# Important note on performance

The default algebraic number type represents algebraic numbers
in canonical form using minimal polynomials. This works well for representing
individual algebraic numbers, but it does not provide the best
performance for field arithmetic.
For fast calculation in $\overline{\mathbb{Q}}$,
`CalciumField` should typically be used instead (see the section
on *Exact real and complex numbers*).
Alternatively, to compute in a fixed subfield of $\overline{\mathbb{Q}}$,
you may fix a generator $a$ and construct a number field to represent $\mathbb{Q}(a)$.

