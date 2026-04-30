# Changelog

All notable changes to this project will be documented in this file.

The format is based on [Keep a Changelog](https://keepachangelog.com/en/1.1.0/), and this project
tries to adhere to [Semantic Versioning](https://semver.org/spec/v2.0.0.html).

The following gives an overview of the changes compared to the previous releases. This list is not
complete, many more internal or minor changes were made, but we tried to only list those changes
which we think might affect some users directly.

## [0.55.0](https://github.com/Nemocas/Nemo.jl/releases/tag/v0.55.0) - 2026-04-30

### Breaking changes

> !These changes break compatibility from previous versions!

- [#2283](https://github.com/Nemocas/Nemo.jl/pull/2283) Update AbstractAlgebra to v0.49
- [#2228](https://github.com/Nemocas/Nemo.jl/pull/2228) Remove `zeros(::ZZPolyRingElem)` that should have been a `roots` method

### New or extended functionality

- [#2259](https://github.com/Nemocas/Nemo.jl/pull/2259) Add `primes_set`
- [#2275](https://github.com/Nemocas/Nemo.jl/pull/2275) Add inplace operations for integer divisions with divisors of type `ZZRingElem`
- [#2282](https://github.com/Nemocas/Nemo.jl/pull/2282) Update FLINT to v3.5.0

## [0.54.2](https://github.com/Nemocas/Nemo.jl/releases/tag/v0.54.2) - 2026-04-24

### New or extended functionality

- [#2268](https://github.com/Nemocas/Nemo.jl/pull/2268) Add `isqrt!` for inplace floor square roots
- [#2269](https://github.com/Nemocas/Nemo.jl/pull/2269) Add allocation-free signed remainder `mod_sym!`
- [#2272](https://github.com/Nemocas/Nemo.jl/pull/2272) Add inplace operations for / improved performance of `div, cdiv, tdiv, fdiv` with inputs of the form `(ZZRingElem, Int)`

### Performance improvements or improved testing

- [#2270](https://github.com/Nemocas/Nemo.jl/pull/2270) Improve performance of `is_divisible_by`

### Fixed bugs that returned incorrect results

- [#2249](https://github.com/Nemocas/Nemo.jl/pull/2249) Fix `gcd`/`lcm` of a singleton ZZ vector to be non-negative
- [#2252](https://github.com/Nemocas/Nemo.jl/pull/2252) Fix `is_maxreal_type` for number fields
- [#2277](https://github.com/Nemocas/Nemo.jl/pull/2277) Fix MPoly constructors to enforce exponents being nonnegative
- [#2278](https://github.com/Nemocas/Nemo.jl/pull/2278) Unify behavior of `isone` for truncated identity matrices

## [0.54.1](https://github.com/Nemocas/Nemo.jl/releases/tag/v0.54.1) - 2026-01-28

### Fixed bugs

- [#2242](https://github.com/Nemocas/Nemo.jl/pull/2242) Ensure `det` for 0x0 matrices returns 1
- [#2243](https://github.com/Nemocas/Nemo.jl/pull/2243) Add proper `min` for `Arb`s and `RealFieldElem`s

# [0.54.0](https://github.com/Nemocas/Nemo.jl/releases/tag/v0.54.0) - 2026-01-14

### Breaking changes

> !These changes break compatibility from previous versions!

- [#2158](https://github.com/Nemocas/Nemo.jl/pull/2158) Forbid (most) automatic polynomial ring coercions, use `change_base_ring` instead
- [#2229](https://github.com/Nemocas/Nemo.jl/pull/2229) Change the `prec` positional argument to a keyword argument for `derivative` and `integral` methods for `ComplexPolyRingElem` and `RealPolyRingElem`
- [#2233](https://github.com/Nemocas/Nemo.jl/pull/2233) Remove `replace!(::typeof(-), m::ZZMatrix)`

### New or extended functionality

- [#2191](https://github.com/Nemocas/Nemo.jl/pull/2191) Add `OneTo` method for `ZZRingElem`, returns ranges of new type `ZZOneTo`
- [#2222](https://github.com/Nemocas/Nemo.jl/pull/2222) Update to AbstractAlgebra 0.48

