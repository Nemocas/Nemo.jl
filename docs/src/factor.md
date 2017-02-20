```@meta
CurrentModule = Nemo
DocTestSetup = quote
  using Nemo
end
```

## Introduction

Nemo provides a unified interface to handle factorizations using the
`Fact` objects. These can only be constructed using the factor function for
the respective ring elements. This is best illustrated by an example.

```jldoctest
julia> fac = factor(ZZ(-6000361807272228723606))
-1 * 2 * 229^3 * 43669^3 * 3

julia> unit(fac)
-1

julia> -6000361807272228723606 == unit(fac) * prod([ p^e for (p, e) in fac])
true

julia> for (p, e) in fac; println("$p $e"); end
2 1
229 3
43669 3
3 1

julia> 229 in fac
true

julia> fac[229]
3
```

## Basic functionality

Objects of type `Fac` are iterable, that is, if `a` is an object of type `Fac`,
then `for (p, e) in a` will iterate through all pairs `(p, e)`, where `p` is a
factor and `e` the corresponding exponent.

```@docs
in{T}(a::T, ::Fac{T})
getindex{T}(::Fac{T}, T)
length(::Fac)
unit(::Fac)
```
