```@meta
CurrentModule = Nemo
```

## Introduction

Nemo previously provided maximal orders of absolute number fields via the Pari
C library. However, Pari will be made optional in future.

For maximal orders there is the Hecke.jl project, which is designed to work as
a series of Nemo plugin modules. The Hecke types fit into the Nemo abstract
type hierarchy and so can be used recursively just as for all other Nemo types.

Details of Hecke can be found here:

- [http://hecke.readthedocs.io/en/latest/](http://hecke.readthedocs.io/en/latest/) (Documentation)
- [https://github.com/thofma/Hecke.jl](https://github.com/thofma/Hecke.jl) (Source code)