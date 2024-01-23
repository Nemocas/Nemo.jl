```@meta
CurrentModule = Nemo
DocTestFilters = r"[0-9\.]+ seconds \(.*\)"
DocTestSetup = quote
    using Nemo
end
```

# Getting Started

Nemo is a computer algebra package for the Julia programming language, maintained by William Hart, 
Tommy Hofmann, Claus Fieker, Fredrik Johansson with additional code by Oleksandr Motsak, Marek Kaluba and other contributors.

- <https://github.com/Nemocas/Nemo.jl> (Source code)
- <https://nemocas.github.io/Nemo.jl/stable/> (Online documentation)

The features of Nemo so far include:

  - Multiprecision integers and rationals
  - Integers modulo n
  - p-adic numbers
  - Finite fields (prime and non-prime order)
  - Number field arithmetic
  - Algebraic numbers
  - Exact real and complex numbers
  - Arbitrary precision real and complex balls
  - Univariate and multivariate polynomials and matrices over the above

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

To use Nemo we require Julia 1.6 or higher. Please see
<https://julialang.org/downloads/> for instructions on
how to obtain julia for your system.

At the Julia prompt simply type

```
julia> using Pkg; Pkg.add("Nemo")
```

## Quick start

Here are some examples of using Nemo.

This example computes recursive univariate polynomials.

```jldoctest
julia> using Nemo

julia> R, x = polynomial_ring(ZZ, "x")
(Univariate polynomial ring in x over ZZ, x)

julia> S, y = polynomial_ring(R, "y")
(Univariate polynomial ring in y over univariate polynomial ring, y)

julia> T, z = polynomial_ring(S, "z")
(Univariate polynomial ring in z over univariate polynomial ring, z)

julia> f = x + y + z + 1
z + y + x + 1

julia> p = f^30; # semicolon suppresses output

julia> @time q = p*(p+1);
  0.161733 seconds (79.42 k allocations: 2.409 MiB)
```

Here is an example using generic recursive ring constructions.

```jldoctest
julia> using Nemo

julia> R, x = finite_field(7, 11, "x")
(Finite field of degree 11 over GF(7), x)

julia> S, y = polynomial_ring(R, "y")
(Univariate polynomial ring in y over GF(7^11), y)

julia> T, _ = residue_ring(S, y^3 + 3x*y + 1)
(Residue ring of univariate polynomial ring modulo y^3 + 3*x*y + 1, Map: univariate polynomial ring -> residue ring)

julia> U, z = polynomial_ring(T, "z")
(Univariate polynomial ring in z over residue ring, z)

julia> f = (3y^2 + y + x)*z^2 + ((x + 2)*y^2 + x + 1)*z + 4x*y + 3;

julia> g = (7y^2 - y + 2x + 7)*z^2 + (3y^2 + 4x + 1)*z + (2x + 1)*y + 1;

julia> s = f^12;

julia> t = (s + g)^12;

julia> @time resultant(s, t)
  0.059095 seconds (391.89 k allocations: 54.851 MiB, 5.22% gc time)
(x^10 + 4*x^8 + 6*x^7 + 3*x^6 + 4*x^5 + x^4 + 6*x^3 + 5*x^2 + x)*y^2 + (5*x^10 + x^8 + 4*x^7 + 3*x^5 + 5*x^4 + 3*x^3 + x^2 + x + 6)*y + 2*x^10 + 6*x^9 + 5*x^8 + 5*x^7 + x^6 + 6*x^5 + 5*x^4 + 4*x^3 + x + 3
```

Here is an example using matrices.

```jldoctest
julia> using Nemo

julia> R, x = polynomial_ring(ZZ, "x")
(Univariate polynomial ring in x over ZZ, x)

julia> S = matrix_space(R, 40, 40)
Matrix space of 40 rows and 40 columns
  over univariate polynomial ring in x over ZZ

julia> M = rand(S, 2:2, -20:20);

julia> @time det(M);
  0.080976 seconds (132.28 k allocations: 23.341 MiB, 4.11% gc time)
```

And here is an example with power series.

```jldoctest
julia> using Nemo

julia> R, x = QQ["x"]
(Univariate polynomial ring in x over QQ, x)

julia> S, t = power_series_ring(R, 100, "t")
(Univariate power series ring over univariate polynomial ring, t + O(t^101))

julia> u = t + O(t^100)
t + O(t^100)

julia> @time divexact((u*exp(x*u)), (exp(u)-1));
  0.412813 seconds (667.49 k allocations: 33.966 MiB, 90.26% compilation time)
```

## Building dependencies from source

Nemo depends on various C libraries which are installed using binaries by default.
With julia version >= 1.3, the use of these binaries can be overridden by putting the following into the file `~/.julia/artifacts/Overrides.toml`:
```toml
[e134572f-a0d5-539d-bddf-3cad8db41a82]
FLINT = "/prefix/for/libflint"

[d9960996-1013-53c9-9ba4-74a4155039c3]
Arb = "/prefix/for/libarb"

[e21ec000-9f72-519e-ba6d-10061e575a27]
Antic = "/prefix/for/libantic"
```
(If only a specific library should be overridden, only the specific entry should be added.)

## Experimental threading support for flint

Enabling a threaded version of flint can be done by setting the environment
variable `NEMO_THREADED=1`. To set the actual number of threads, use
`Nemo.flint_set_num_threads($numberofthreads)`.
