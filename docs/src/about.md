# Nemo

## What is Nemo?

Nemo is a computer algebra package for the Julia programming language. Our aim is to provide a highly
performant computer algebra package covering

  - Commutative Algebra
  - Number Theory
  - Group Theory

Nemo consists of wrappers of specialised C/C++ libraries:

  - Flint    [http://flintlib.org/]
  - Arb      [http://fredrikj.net/arb/]
  - Antic    [https://github.com/wbhart/antic/]
  - Singular [https://www.singular.uni-kl.de/]
  - Pari     [http://pari.math.u-bordeaux.fr/]

It will also eventually provide interfaces to interpreted library code from other computer algebra
systems such as Gap and Singular.

Nemo also provides implementations of generic algorithms and mathematical data structures. So far the
fully recursive constructions include

  - Univariate polynomial rings
  - Power series rings
  - Residue rings (modulo principal ideals)
  - Fraction fields
  - Matrices

## Why Julia?

Julia is a sophisticated, modern programming language which is designed to be both performant and
flexible. It was written by mathematicians, for mathematicians.

The benefits of Julia include

  - Familiar imperative syntax
  - JIT compilation (provides near native performance, even for highly generic code)
  - REPL console (cuts down on development time)
  - Parametric types (allows for fast generic constructions over other data types)
  - Powerful metaprogramming facilities
  - Operator overloading
  - Multiple dispatch (dispatch on every argument of a function)
  - Efficient native C interface (no wrapper overhead)
  - Experimental C++ interface
  - Dynamic type inference
  - Built-in bignums
  - Able to be embedded in C programs
  - High performance collection types (dictionaries, iterators, arrays, etc.)
  - Jupyter support (for web based notebooks)

The main benefits for Nemo are the parametric type system and JIT compilation. The former allows us to
model many mathematical types, e.g. generic polynomial rings over an arbitrary base ring. The latter
speeds up the runtime performance, even of highly generic mathematical procedures.
