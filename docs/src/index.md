# Getting Started

Nemo is a computer algebra package for the Julia programming language, maintained by William Hart, 
Tommy Hofmann, Claus Fieker, Fredrik Johansson with additional code by Oleksandr Motsak, Marek Kaluba and other contributors.

- [http://nemocas.org](http://nemocas.org) (Website)
- [https://github.com/Nemocas/Nemo.jl](https://github.com/Nemocas/Nemo.jl) (Source code)
- [http://nemocas.github.io/Nemo.jl/latest/](http://nemocas.github.io/Nemo.jl/latest/) (Online documentation)

The features of Nemo so far include:

  - Multiprecision integers and rationals
  - Integers modulo n
  - p-adic numbers
  - Finite fields (prime and non-prime order)
  - Number field arithmetic
  - Arbitrary precision real and complex balls
  - Univariate polynomials and matrices over the above

Nemo depends on AbstractAlgebra.jl which provides Nemo with generic routines for:

  - Univariate and multivariate polynomials
  - Absolute and relative power series
  - Laurent series
  - Fraction fields
  - Residue rings
  - Matrices and linear algebra
  - Young Tableaux
  - Permutation groups
  - Characters

## Installation

To use Nemo we require Julia 1.0 or higher. Please see
[http://julialang.org/downloads](http://julialang.org/downloads/) for instructions on
how to obtain julia for your system.

At the Julia prompt simply type

```
julia> using Pkg; Pkg.add("Nemo")
```

## Quick start

Here are some examples of using Nemo.

This example computes recursive univariate polynomials.

```julia
julia> using Nemo

julia> R, x = PolynomialRing(ZZ, "x")
(Univariate Polynomial Ring in x over Integer Ring,x)

julia> S, y = PolynomialRing(R, "y")
(Univariate Polynomial Ring in y over Univariate Polynomial Ring in x over Integer Ring,y)

julia> T, z = PolynomialRing(S, "z")
(Univariate Polynomial Ring in z over Univariate Polynomial Ring in y over Univariate Polynomial Ring in x over Integer Ring,z)

julia> f = x + y + z + 1
z+(y+(x+1))

julia> p = f^30; # semicolon supresses output

julia> @time q = p*(p+1);
  0.325521 seconds (140.64 k allocations: 3.313 MB)
```

Here is an example using generic recursive ring constructions.

```
julia> using Nemo

julia> R, x = FiniteField(7, 11, "x")
(Finite field of degree 11 over F_7,x)

julia> S, y = PolynomialRing(R, "y")
(Univariate Polynomial Ring in y over Finite field of degree 11 over F_7,y)

julia> T = ResidueRing(S, y^3 + 3x*y + 1)
Residue ring of Univariate Polynomial Ring in y over Finite field of degree 11 over F_7 modulo y^3+(3*x)*y+(1)

julia> U, z = PolynomialRing(T, "z")
(Univariate Polynomial Ring in z over Residue ring of Univariate Polynomial Ring in y over Finite field of degree 11 over F_7 modulo y^3+(3*x)*y+(1),z)

julia> f = (3y^2 + y + x)*z^2 + ((x + 2)*y^2 + x + 1)*z + 4x*y + 3;

julia> g = (7y^2 - y + 2x + 7)*z^2 + (3y^2 + 4x + 1)*z + (2x + 1)*y + 1;

julia> s = f^12;

julia> t = (s + g)^12;

julia> @time resultant(s, t)
  0.426612 seconds (705.88 k allocations: 52.346 MB, 2.79% gc time)
(x^10+4*x^8+6*x^7+3*x^6+4*x^5+x^4+6*x^3+5*x^2+x)*y^2+(5*x^10+x^8+4*x^7+3*x^5+5*x^4+3*x^3+x^2+x+6)*y+(2*x^10+6*x^9+5*x^8+5*x^7+x^6+6*x^5+5*x^4+4*x^3+x+3)
```

Here is an example using matrices.

```
julia> using Nemo

julia> R, x = PolynomialRing(ZZ, "x")
(Univariate Polynomial Ring in x over Integer Ring,x)

julia> S = MatrixSpace(R, 40, 40)
Matrix Space of 40 rows and 40 columns over Univariate Polynomial Ring in x over Integer Ring

julia> M = rand(S, 2:2, -20:20)

julia> @time det(M);
  0.131212 seconds (1.12 M allocations: 39.331 MiB, 4.77% gc time)
```

And here is an example with power series.

```
julia> using Nemo

julia> R, x = QQ["x"]
(Univariate Polynomial Ring in x over Rational Field,x)

julia> S, t = PowerSeriesRing(R, 100, "t")
(Univariate power series ring in t over Univariate Polynomial Ring in x over Rational Field,t+O(t^101))

julia> u = t + O(t^100)
t+O(t^100)

julia> @time divexact((u*exp(x*u)), (exp(u)-1));
  0.042663 seconds (64.01 k allocations: 1.999 MB, 15.40% gc time)
```

## Building dependencies from source

Nemo depends on various C libraries which are installed using binaries by default.
Building from source can be enabled by setting the environment variable `NEMO_SOURCE_BUILD=1`
and then doing `Pkg.build("Nemo")` or `Pkg.add("Nemo")` depending on whether Nemo
was already installed.
