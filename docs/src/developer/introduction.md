```@meta
CurrentModule = Nemo
```

# Introduction to Nemo development

## Relationship to AbstractAlgebra.jl

Some time in the past, Nemo was split into two packages called Nemo.jl and
AbstractAlgebra.jl. The purpose was to provide a Julia only package which
did some subset of what Nemo could do, albeit slower. This was requested by
people in the Julia community.

Unfortunately this hasn't been terribly successful. Most Julia developers
expect that AbstractAlgebra and Nemo functionality will work for Julia
matrices over AbstractAlgebra/Nemo rings. This would be possible for
functions that do not conflict with `Base` or `LinearAlgebra` at least
when working with non-empty matrices. However, for reasons that we explain
in both the Appendix to the AbstractAlgebra package and in the parent object
section of the developer documentation, this is not possible even in theory
for functions that would conflict with Julia's standard library or for empty
matrices (except in a limited number of special cases).

Unfortunately the Julia standard library functions do not work with matrices
of Nemo objects and there is little we can do about this. Moreover, some Julia
functionality isn't supported by the underlying C libraries in Nemo and would
be difficult or impossible to provide on the C side.

Nowadays we see AbstractAlgebra to provide three things to Nemo:

* An abstract type hierarchy
* Generic ring constructions, e.g. generic polynomials and matrices
* Generic implementations that should work for any ring implementing the
  required interfaces. These interfaces are documented in the AbstractAlgebra
  documentation.

Nemo itself is now more or less just a wrapper of four C libraries:

* Flint : polynomials and matrices over Z, Q, Z/nZ, Qp, Fq
* Arb : polynomials, matrices and special functions over balls over R and C
* Antic : algebraic number field element arithmetic
* Calcium : exact real and complex numbers, including algebraic numbers

Each ring implemented in those C libraries is wrapped in such a way as to
implement the interfaces described by AbstractAlgebra.

Most of the time an AbstractAlgebra implementation will work just as well
using Nemo, but the latter will usually be faster, due to the extremely
performant C code (around half a million lines of it).

## Layout of files

In the `src` directory of Nemo are four directories `flint`, `arb`, `antic` and
`calcium`, each containing the wrappers for the relevant C libraries. The
`test` directory is similarly organised.

Within each of these directories is a set of files, one per module within
the C libraries, e.g. the `fmpz.jl` file wraps the Flint `fmpz` module for
multiple precision integers. The `fmpz_poly.jl` file wraps the Flint
univariate polynomials over `fmpz` integers, and so on.

The `fmpq` prefix is for Flint rationals, `fq` for Flint finite fields with
multiprecision characteristic, `fq_nmod` is the same but for single word
characteristic. The `padic` prefix is for the field of p-adic numbers for a
given `p`. The `nmod` prefix is for `Z/nZ` for a given `n`. The `gfp` prefix is
the same as `Z/nZ` but where `n` is prime, so that we are dealing with a field.

The `FlintTypes.jl` file contains the implementation of all the Flint types.

In the `antic` directory, `nf_elem` is for elements of a number field.

The `AnticTypes.jl` file contains the Antic types.

In the `arb` directory the `arb` prefix is for arbitrary precision ball
arithmetic over the reals. The `acb` prefix is similar but for complex numbers.

The `ArbTypes.jl` file contains the Arb types.

In the `calcium` directory the `ca` prefix is for Calcium's type. There is also
a `qqbar` file for the field of algebraic numbers.

In the AbstractAlgebra.jl package the `src` directory contains a directory
called `generic`. This is where the implementations of generic types, such as
matrices, polynomials, series, etc. reside. Each file such as `Matrix.jl`
corresponds to a generic group/ring/field or other algebraic construction
(typically over a base ring). The files in this directory exist inside a
submodule of AbstractAlgebra called `Generic`.

The file `GenericTypes.jl` is where all the generic types are implemented.

At the top level of the `src` directory is a file `Generic.jl` which is where
the `Generic` submodule of AbstractAlgebra begins and where imports are made
from AbstractAlgebra into `Generic`.

In the `src` directory we have implementations that work for every type
belonging to a given abstract type, e.g. `Matrix.jl` has implementations that
will work for any matrix type, whether from AbstractAlgebra's `Generic` module
or even matrix types from Nemo, and so on. So long as they are implemented to
provide the `Matrix` interface all the functions there will work for them. The
same applies for `Poly.jl` for polynomial types, `AbsSeries.jl` for absolute
series types, `RelSeries.jl` for relative series types, etc.

In the `src` directory is `AbstractTypes.jl` where all the AbstractAlgebra
abstract types are defined.

Also in the `src` directory is a subdirectory called `Julia`. This is where we
give our own implementations of functionality for Julia `Integer`s and
`Rational`s and various other basic rings implemented in terms of Julia types.
These are provided so that the package will work as a pure Julia package,
replacing many of the rings and fields that would be available in Flint and
the other C libraries with Julia equivalents.

Note that some of the implementations we give there would conflict with `Base`
and so are only available inside AbstractAlgebra and are not exported!

We try to keep the `test` directory at the top level of the source tree
organised in the same manner as the other directories just discussed, though
there is currently no split between tests for `Generic` and for the
implementations in `src`. All tests are currently combined in `test/generic`..

## Git, GitHub and project workflows

The official repositories for AbstractAlgebra and Nemo are:

https://github.com/Nemocas/AbstractAlgebra.jl

https://github.com/Nemocas/Nemo.jl

If you wish to contribute to these projects, the first step is to fork them
on GitHub. The button for this is in the upper right of the main project page.
You will need to sign up for a free GitHub account to do this.

Once you have your own GitHub copy of our repository you can push changes to
it from your local machine and this will make them visible to the world.

Before sinking a huge amount of time into a contribution, please open a ticket
on the official project page on GitHub explaining what you intend to do and
discussing it with the other developers.

The easiest way to get going with development on your local machine is to
`dev` AbstractAlgebra and/or Nemo. To do this, press the `]` key in Julia
to enter the special package mode and type:

```julia
dev Nemo
```

Now you will find a local copy on your machine of the Nemo repository in

```
.julia/dev/Nemo
```

However, this will be set up to push to the official repository instead of your
own, so you will need to change this. For example, if your GitHub account name
is `myname`, edit the `.git/config` file in your local `Nemo` directory to say:

```
        url = https://github.com/Nemocas/Nemo.jl.git
        pushurl = https://github.com/myname/Nemo.jl
```

instead of just the first line which will already be there.

It is highly recommended that you do not work in the master branch, but create
a new branch for each thing you want to contribute to Nemo.

```
git checkout -b mynewbranch
```

If your contribution is small and does not take a long time to implement,
everything will likely be fine if you simply commit the changes locally, then
push them to your GitHub account online:

```
git commit -a
git push --all
```

However, if you are working on a much larger project it is *highly* recommended
that you frequently pull from the official master branch and rebase your new
branch on top of any changes that have been made there:

```
git checkout master
git pull
git checkout mynewbranch
git rebase master
```

Note that rebasing will try to rewrite each of your commits over the top of the
branch you are rebasing on (master in this case). This process will have many
steps if there are many commits and lots of conflicts. Simply follow the
instructions until the process is finished.

The longer you leave it before rebasing on master the longer the rebase process
will take. It can eventually become overwhelming as it is not replaying the
latest state of your repository over master, but each commit that you made in
order. You may have completely forgotten what those older commits were about,
so this can become very difficult if not done regularly.

Once you have pushed your changes to your GitHub account, go to the official
project GitHub page and you should see your branch mentioned near the top of
the page. Open a pull request.

Someone will review your code and suggest changes they'd like made. Simply
add more commits to your branch and push again. They will automatically get
added to your pull request.

Note that we don't accept code without tests and documentation. We use
Documenter.jl for our documentation, in Markdown format. See our existing
code for examples of docstrings above functions in the source code and look
in the `docs/src` directory to see how these docstrings are merged into our
online documentation.

## Development list

All developers of AbstractAlgebra and Nemo are welcome to write to our
development list to ask questions and discuss development:

https://groups.google.com/g/nemo-devel

## Reporting bugs

Bugs should be reported by opening an issue (ticket) on the official GitHub
page for the relevant project. Please state the Julia version being used,
the machine you are using and the version of AbstractAlgebra/Nemo you are
using. The version can be found in the Project.toml file at the top level
of the source tree.

## Development roadmap

AbstractAlgebra has a special roadmap ticket which lists the most important
tickets that have been opened. If you want to contribute something high value
this is the place to start:

https://github.com/Nemocas/AbstractAlgebra.jl/issues/492

This ticket is updated every so often.

## Binaries

Binaries of C libraries for Nemo are currently made in a separate repository:

https://github.com/JuliaPackaging/Yggdrasil

If code is added to any of the C libraries used by Nemo, this jll package must
be updated first and the version updated in Nemo.jl before the new
functionality can be used. Ask the core developers for help with this as
various other tasks must be completed at the same time.

## Relationship to Oscar

Nemo and AbstractAlgebra are heavily used by the Oscar computer algebra system
being developed in Germany by a number of universities involved in a large
project known as TRR 195, funded by the DFG.

Oscar is the number one customer for Nemo. Many bugs in Nemo are found and
fixed by Oscar developers and most of the key Nemo developers are part of the
Oscar project.

See the Oscar website for further details:

https://oscar.computeralgebra.de/

