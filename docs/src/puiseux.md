```@meta
CurrentModule = Nemo
```

# Puiseux series

Nemo allows the creation of Puiseux series over any computable ring $R$. Puiseux series
are series of the form
$a_jx^{j/m} + a_{j+1}x^{(j+1)/m} + \cdots + a_{k-1}x^{(k-1)/m} + O(x^{k/m})$
where $m$ is a positive integer, $a_i \in R$ and the relative precision $k - j$ is at
most equal to some specified precision $n$.

There are two different kinds of implementation: a generic one for
the case where no specific implementation exists (provided by AbstractAlgebra.jl), and
efficient implementations of Puiseux series over numerous specific rings, usually
provided by C/C++ libraries.

The following table shows each of the Puiseux series types available in
Nemo, the base ring $R$, and the Julia/Nemo types for that kind of series (the
type information is mainly of concern to developers).

Base ring                             | Library            | Element type                       | Parent type
--------------------------------------|--------------------|--------------------------------------------------|----------------------------------------------
Generic ring $R$                      | AbstractAlgebra.jl | `Generic.PuiseuxSeriesRingElem{T}                | `Generic.PuiseuxSeriesRing{T}`
Generic field $K$                     | AbstractAlgebra.jl | `Generic.PuiseuxSeriesFieldElem{T}               | `Generic.PuiseuxSeriesField{T}`
$\mathbb{Z}$                          | Flint              | `FlintPuiseuxSeriesRingElem{fmpz_laurent_series}`| `FlintPuiseuxSeriesRing{fmpz_laurent_series}`

For convenience, `FlintPuiseuxSeriesRingElem` and `FlintPuiseuxSeriesFieldElem` both
belong to a union type called `FlintPuiseuxSeriesElem`.

The maximum relative precision, the string representation of the variable and
the base ring $R$ of a generic power series are stored in the parent object. 

Note that unlike most other Nemo types, Puiseux series are parameterised by the type of
the underlying Laurent series type (which must exist before Nemo can make use of it),
instead of the type of the coefficients.

## Puiseux power series

Puiseux series have their maximum relative precision capped at
some value `prec_max`. This refers to the maximum precision of the underlying Laurent
series. See the description of the generic Puiseux series in AbstractAlgebra.jl for
details.

There are numerous important things to be aware of when working with Puiseux series, or
series in general. Please refer to the documentation of generic Puiseux series and 
series in general in AbstractAlgebra.jl for details.

## Puiseux series functionality

Puiseux series rings in Nemo implement the AbstractAlgebra.jl series interface, with the
exception of the `pol_length` and `polcoeff` functions:

<https://nemocas.github.io/AbstractAlgebra.jl/latest/series_rings>

In addition, generic Puiseux series are provided by AbstractAlgebra.jl:

<https://nemocas.github.io/AbstractAlgebra.jl/latest/puiseux.>

Puiseux series rings in Nemo also implement this generic functionality. We list below only
the functionality that differs from this generic functionality, for specific rings
provided by Nemo.

### Special functions

```@docs
Base.sqrt(a::FlintPuiseuxSeriesElem{fmpz_laurent_series})
```

```@docs
Base.exp(a::FlintPuiseuxSeriesElem{fmpz_laurent_series})
```

```@docs
eta_qexp(x::FlintPuiseuxSeriesElem{fmpz_laurent_series})
```

**Examples**

```julia
S, x = PuiseuxSeriesRing(ZZ, 30, "x")

a = 1 + z + 3z^2 + O(z^5)

h = sqrt(a^2)
k = eta_qexp(S)
```
