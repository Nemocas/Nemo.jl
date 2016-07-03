# Nemo

## Introduction

Nemo is a computer algebra package for the Julia programming language, maintained by William Hart, 
Tommy Hofmann, Claus Fieker, Fredrik Johansson with additional code by Oleksandr Motsak and other
contributors.

- [http://nemocas.org](http://nemocas.org) (Website)
- [https://github.com/Nemocas/Nemo.jl](https://github.com/Nemocas/Nemo.jl) (Source code)
- [http://nemocas.github.io/Nemo.jl/latest/](http://nemocas.github.io/Nemo.jl/latest/) (Online documentation)

The features of Nemo so far include:

  - Multiprecision integers and rationals
  - Integers modulo n
  - p-adic numbers
  - Finite fields (prime and non-prime order)
  - Number field arithmetic
  - Maximal orders of number fields
  - Arithmetic of ideals in maximal orders
  - Arbitrary precision real and complex balls
  - Univariate polynomials and matrices over the above
  - Generic polynomials, power series, fraction fields, residue rings and matrices

## Installation

To use Nemo we require Julia 0.4 or higher. Please see
[http://julialang.org/downloads](http://julialang.org/downloads/) for instructions on how to obtain
julia for your system.

At the Julia prompt simply type

```
julia> Pkg.add("Nemo")
julia> Pkg.build("Nemo")
```

Alternatively, if you don't want to set Julia up yourself, Julia and Nemo are available on
[https://cloud.sagemath.com/] (https://cloud.sagemath.com/).

## Quick start

Here are some examples of using Nemo.

This example computes recursive univariate polynomials.

```
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

julia> M = MatrixSpace(R, 40, 40)();

julia> R, x = PolynomialRing(ZZ, "x")
(Univariate Polynomial Ring in x over Integer Ring,x)

julia> M = MatrixSpace(R, 40, 40)();

julia> for i in 1:40
          for j in 1:40
             M[i, j] = R(map(fmpz, rand(-20:20, 3)))
          end
       end

julia> @time det(M);
  0.174888 seconds (268.40 k allocations: 26.537 MB, 4.47% gc time)
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