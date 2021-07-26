```@meta
CurrentModule = Nemo
```

# Algebraic numbers

Nemo allows working with exact real and complex algebraic numbers.

The default algebraic number type in Nemo is provided by Calcium. The
associated field of algebraic numbers is represented by the constant
parent object called `CalciumQQBar`.

For convenience we define

```
QQBar = CalciumQQBar
```

so that algebraic numbers can be constructed using `QQBar` instead of
`CalciumQQBar`. Note that this is the name of a specific parent object,
not the name of its type.


 Library        | Element type  | Parent type
----------------|---------------|--------------------
Calcium         | `qqbar`       | `CalciumQQBarField`

**Important note on performance**

The default algebraic number type represents algebraic numbers
in canonical form using minimal polynomials. This works well for representing
individual algebraic numbers, but it does not provide the best
performance for field arithmetic.
For fast calculation in $\overline{\mathbb{Q}}$,
`CalciumField` should typically be used instead (see the section
on *Exact real and complex numbers*).
Alternatively, to compute in a fixed subfield of $\overline{\mathbb{Q}}$,
you may fix a generator $a$ and construct an
Antic number field to represent $\mathbb{Q}(a)$.

## Algebraic number functionality

### Constructing algebraic numbers

Methods to construct algebraic numbers include:

* Conversion from other numbers and through arithmetic operations
* Computing the roots of a given polynomial
* Computing the eigenvalues of a given matrix
* Random generation
* Exact trigonometric functions (see later section)
* Guessing (see later section)

**Examples**

Arithmetic:

```julia
julia> fmpz(QQBar(3))
3

julia> fmpq(QQBar(3) // 2)
3//2

julia> QQBar(-1) ^ (QQBar(1) // 3)
Root 0.500000 + 0.866025*im of x^2 - x + 1
```

Solving the quintic equation:

```julia
julia> R, x = PolynomialRing(QQ, "x")
(Univariate Polynomial Ring in x over Rational Field, x)

julia> v = roots(x^5-x-1, QQBar)
5-element Vector{qqbar}:
 Root 1.16730 of x^5 - x - 1
 Root 0.181232 + 1.08395*im of x^5 - x - 1
 Root 0.181232 - 1.08395*im of x^5 - x - 1
 Root -0.764884 + 0.352472*im of x^5 - x - 1
 Root -0.764884 - 0.352472*im of x^5 - x - 1

julia> v[1]^5 - v[1] - 1 == 0
true
```

Computing exact eigenvalues of a matrix:

```julia
julia> eigenvalues(ZZ[1 1 0; 0 1 1; 1 0 1], QQBar)
3-element Vector{qqbar}:
 Root 2.00000 of x - 2
 Root 0.500000 + 0.866025*im of x^2 - x + 1
 Root 0.500000 - 0.866025*im of x^2 - x + 1
```

**Interface**

```@docs
roots(f::fmpz_poly, R::CalciumQQBarField)
roots(f::fmpq_poly, R::CalciumQQBarField)
eigenvalues(A::fmpz_mat, R::CalciumQQBarField)
eigenvalues(A::fmpq_mat, R::CalciumQQBarField)
rand(R::CalciumQQBarField; degree::Int, bits::Int, randtype::Symbol=:null)
```

### Numerical evaluation

**Examples**

Algebraic numbers can be evaluated
numerically to arbitrary precision by converting
to real or complex Arb fields:

```julia
julia> RR = ArbField(64); RR(sqrt(QQBar(2)))
[1.414213562373095049 +/- 3.45e-19]

julia> CC = AcbField(32); CC(QQBar(-1) ^ (QQBar(1) // 4))
[0.707106781 +/- 2.74e-10] + [0.707106781 +/- 2.74e-10]*im
```

### Minimal polynomials, conjugates, and properties

**Examples**

Retrieving the minimal polynomial and algebraic conjugates
of a given algebraic number:

```julia
julia> minpoly(PolynomialRing(ZZ, "x")[1], QQBar(1+2im))
x^2 - 2*x + 5

julia> conjugates(QQBar(1+2im))
2-element Vector{qqbar}:
 Root 1.00000 + 2.00000*im of x^2 - 2x + 5
 Root 1.00000 - 2.00000*im of x^2 - 2x + 5
```

**Interface**

```@docs
iszero(x::qqbar)
isone(x::qqbar)
isinteger(x::qqbar)
isrational(x::qqbar)
isreal(x::qqbar)
degree(x::qqbar)
isalgebraic_integer(x::qqbar)
minpoly(R::FmpzPolyRing, x::qqbar)
minpoly(R::FmpqPolyRing, x::qqbar)
conjugates(a::qqbar)
denominator(x::qqbar)
numerator(x::qqbar)
height(x::qqbar)
height_bits(x::qqbar)
```

### Complex parts

**Examples**

```julia
julia> real(sqrt(QQBar(1im)))
Root 0.707107 of 2x^2 - 1

julia> abs(sqrt(QQBar(1im)))
Root 1.00000 of x - 1

julia> floor(sqrt(QQBar(1000)))
Root 31.0000 of x - 31

julia> sign(QQBar(-10-20im))
Root -0.447214 - 0.894427*im of 5x^4 + 6x^2 + 5
```

**Interface**

```@docs
real(a::qqbar)
imag(a::qqbar)
abs(a::qqbar)
abs2(a::qqbar)
conj(a::qqbar)
sign(a::qqbar)
csgn(a::qqbar)
sign_real(a::qqbar)
sign_imag(a::qqbar)
floor(a::qqbar)
ceil(a::qqbar)
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

```julia
julia> 1 < sqrt(QQBar(2)) < QQBar(3)//2
true

julia> x = QQBar(3+4im)
Root 3.00000 + 4.00000*im of x^2 - 6x + 25

julia> isequal_abs(x, -x)
true

julia> isequal_abs_imag(x, 2-x)
true

julia> isless_real(x, x // 2)
false
```

**Interface**

```@docs
isequal_real(a::qqbar, b::qqbar)
isequal_imag(a::qqbar, b::qqbar)
isequal_abs(a::qqbar, b::qqbar)
isequal_abs_real(a::qqbar, b::qqbar)
isequal_abs_imag(a::qqbar, b::qqbar)
isless_real(a::qqbar, b::qqbar)
isless_imag(a::qqbar, b::qqbar)
isless_abs(a::qqbar, b::qqbar)
isless_abs_real(a::qqbar, b::qqbar)
isless_abs_imag(a::qqbar, b::qqbar)
isless_root_order(a::qqbar, b::qqbar)
```

### Roots and trigonometric functions

**Examples**

```julia
julia> root(QQBar(2), 5)
Root 1.14870 of x^5 - 2

julia> sinpi(QQBar(7) // 13)
Root 0.992709 of 4096x^12 - 13312x^10 + 16640x^8 - 9984x^6 + 2912x^4 - 364x^2 + 13

julia> tanpi(atanpi(sqrt(QQBar(2)) + 1))
Root 2.41421 of x^2 - 2x - 1

julia> root_of_unity(QQBar, 5)
Root 0.309017 + 0.951057*im of x^4 + x^3 + x^2 + x + 1

julia> root_of_unity(QQBar, 5, 4)
Root 0.309017 - 0.951057*im of x^4 + x^3 + x^2 + x + 1

julia> w = (1 - sqrt(QQBar(-3)))//2
Root 0.500000 - 0.866025*im of x^2 - x + 1

julia> isroot_of_unity(w)
true

julia> isroot_of_unity(w + 1)
false

julia> root_of_unity_as_args(w)
(6, 5)
```

**Interface**

```@docs
sqrt(a::qqbar)
root(a::qqbar, n::Int)
root_of_unity(C::CalciumQQBarField, n::Int)
root_of_unity(C::CalciumQQBarField, n::Int, k::Int)
isroot_of_unity(a::qqbar)
root_of_unity_as_args(a::qqbar)
exp_pi_i(a::qqbar)
log_pi_i(a::qqbar)
sinpi(a::qqbar)
cospi(a::qqbar)
tanpi(a::qqbar)
asinpi(a::qqbar)
acospi(a::qqbar)
atanpi(a::qqbar)
```

### Guessing

**Examples**

An algebraic number can be recovered from a numerical value:

```julia
julia> RR = ArbField(53); guess(QQBar, RR("1.41421356 +/- 1e-6"), 2)
Root 1.41421 of x^2 - 2
```

Warning: the input should be an enclosure. If you have a floating-point
approximation, you should add an error estimate; otherwise, the only
algebraic number that can be guessed is the binary floating-point number
itself.

```julia
julia> RR = ArbField(128);

julia> x = RR(0.1);       # note: 53-bit binary approximation of 1//10 without radius

julia> guess(QQBar, x, 1)
Root 0.100000 of 36028797018963968x - 3602879701896397

julia> guess(QQBar, x + RR("+/- 1e-10"), 1)
Root 0.100000 of 10x - 1
```

**Interface**

```@docs
guess(R::CalciumQQBarField, x::arb, maxdeg::Int, maxbits::Int=0)
guess(R::CalciumQQBarField, x::acb, maxdeg::Int, maxbits::Int=0)
```

