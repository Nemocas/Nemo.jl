```@meta
CurrentModule = Nemo
```

# Power series and Laurent series

Nemo allows the creation of capped relative and absolute power series over any computable
ring $R$. Capped relative power series are power series of the form
$a_jx^j + a_{j+1}x^{j+1} + \cdots + a_{k-1}x^{k-1} + O(x^k)$
where $j \geq 0$, $a_j \in R$ and the relative precision $k - j$ is at most
equal to some specified precision $n$.
On the other hand capped absolute power series are power series of the form
$a_jx^j + a_{j+1}x^{j+1} + \cdots + a_{n-1}x^{n-1} + O(x^n)$
where $j \geq 0$, $a_j \in R$ and the precision $n$ is fixed.

There are two different kinds of implementation: a generic one for
the case where no specific implementation exists (provided by AbstractAlgebra.jl), and
efficient implementations of power series over numerous specific rings, usually
provided by C/C++ libraries.

The following table shows each of the relative power series types available in
Nemo, the base ring $R$, and the Julia/Nemo types for that kind of series (the
type information is mainly of concern to developers).

Base ring                             | Library            | Element type          | Parent type
--------------------------------------|--------------------|-----------------------|----------------------
Generic ring $R$                      | AbstractAlgebra.jl | `Generic.RelSeries{T} | `Generic.RelSeriesRing{T}`
$\mathbb{Z}$                          | Flint              | `fmpz_rel_series`     | `FmpzRelSeriesRing`
$\mathbb{Z}/n\mathbb{Z}$ (small $n$)  | Flint              | `nmod_rel_series`     |  NmodRelSeriesRing
$\mathbb{Z}/n\mathbb{Z}$              | Flint              | `fmpz_mod_rel_series` | `FmpzModRelSeriesRing`
$\mathbb{Q}$                          | Flint              | `fmpq_rel_series`     | `FmpqRelSeriesRing`
$\mathbb{F}_{p^n}$ (small $p$)        | Flint              | `fq_nmod_rel_series`  | `FqNmodRelSeriesRing`
$\mathbb{F}_{p^n}$ (large $p$)        | Flint              | `fq_rel_series`       | `FqRelSeriesRing`

All relative power series elements belong to the abstract type `RelSeriesElem` and all
of the relative power series ring types belong to the abstract type `RelSeriesRing`.

The maximum relative precision, the string representation of the variable and
the base ring $R$ of a generic power series are stored in its parent object. 

Here is the corresponding table for the absolute power series types.

Base ring                             | Library            | Element type          | Parent type
--------------------------------------|--------------------|-----------------------|----------------------
Generic ring $R$                      | AbstractAlgebra.jl | `Generic.AbsSeries{T}`| `Generic.AbsSeriesRing{T}`
$\mathbb{Z}$                          | Flint              | `fmpz_abs_series`     | `FmpzAbsSeriesRing`
$\mathbb{Z}/n\mathbb{Z}$              | Flint              | `fmpz_mod_abs_series` | `FmpzModAbsSeriesRing`
$\mathbb{Q}$                          | Flint              | `fmpq_abs_series`     | `FmpqAbsSeriesRing`
$\mathbb{F}_{p^n}$ (small $n$)        | Flint              | `fq_nmod_abs_series`  | `FqNmodAbsSeriesRing`
$\mathbb{F}_{p^n}$ (large $n$)        | Flint              | `fq_abs_series`       | `FqAbsSeriesRing`

All absolute power series elements belong to the abstract type `AbsSeriesElem` and all
of the absolute power series ring types belong to the abstract type `AbsSeriesRing`.

The absolute precision, the string representation of the variable and
the base ring $R$ of a generic power series are stored in its parent object. 

All power series element types belong to the abstract type `SeriesElem` and all
of the power series ring types belong to the abstract type `SeriesRing`. This
enables one to write generic functions that can accept any Nemo power series
type.

AbstractAlgebra.jl also provides Nemo with a generic implementation of Laurent series
over a given ring $R$. For completeness, we list it here.

Base ring                             | Library            | Element type              | Parent type
--------------------------------------|--------------------|---------------------------|----------------------
Generic ring $R$                      | AbstractAlgebra.jl | `Generic.LaurentSeriesRingElem{T}`| `Generic.LaurentSeriesRing{T}`
Generic field $K$                     | AbstractAlgebra.jl | `Generic.LaurentSeriesFieldElem{T}`|
`Generic.LaurentSeriesField{T}`

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
Thus, for example, $x^5 + O(x^{10}) == 0 + O(x^5)$, since the minimum absolute
precision is $5$.

During computations, it is possible for power series to lose relative
precision due to cancellation. For example if $f = x^3 + x^5 + O(x^8)$ and
$g = x^3 + x^6 + O(x^8)$ then $f - g = x^5 - x^6 + O(x^8)$ which now has
relative precision $3$ instead of relative precision $5$.

Amongst other things, this means that equality is not transitive. For example
$x^6 + O(x^{11}) == 0 + O(x^5)$ and $x^7 + O(x^{12}) == 0 + O(x^5)$ but
$x^6 + O(x^{11}) \neq x^7 + O(x^{12})$.

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
instance, what is the leading coefficient of $(0 + O(x^{10}))y + (1 + O(x^{10}))$?

If one takes it to be $(0 + O(x^{10}))$ then some functions may not terminate
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

## Capped absolute power series

An absolute power series ring over a ring $R$ with precision $p$ behaves 
very much like the quotient $R[x]/(x^p)$ of the polynomial ring over $R$.

## Power series functionality

Power series rings in Nemo implement the AbstractAlgebra.jl series interface:

<https://nemocas.github.io/AbstractAlgebra.jl/latest/series_rings>

In addition, generic power series and Laurent series are provided by AbstractAlgebra.jl:

<https://nemocas.github.io/AbstractAlgebra.jl/latest/series>

Power series rings in Nemo also implement this generic functionality. We list below only
the functionality that differs from this generic functionality, for specific rings
provided by Nemo.

### Special functions

```@docs
log(a::fmpq_rel_series)
```

```@docs
Base.sqrt(a::fmpq_rel_series)
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

**Examples**

```julia
S, x = PowerSeriesRing(R, 30, "x")
T, z = PowerSeriesRing(QQ, 30, "z")

a = 1 + z + 3z^2 + O(z^5)
b = z + 2z^2 + 5z^3 + O(z^5)

d = divexact(x, exp(x + O(x^40)) - 1)
f = exp(b)
g = log(a)
h = sqrt(a)
k = sin(b)
m = atanh(b)
```
