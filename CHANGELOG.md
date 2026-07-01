# Changelog

All notable changes to this project will be documented in this file.

The format is based on [Keep a Changelog](https://keepachangelog.com/en/1.1.0/), and this project
tries to adhere to [Semantic Versioning](https://semver.org/spec/v2.0.0.html).

The following gives an overview of the changes compared to the previous releases. This list is not
complete, many more internal or minor changes were made, but we tried to only list those changes
which we think might affect some users directly.

## [0.56.1](https://github.com/Nemocas/Nemo.jl/releases/tag/v0.56.1) - 2026-06-31

### New or extended functionality

- [#2334](https://github.com/Nemocas/Nemo.jl/pull/2334) Update FLINT to v3.6.0

### Performance improvements or improved testing

- [#2315](https://github.com/Nemocas/Nemo.jl/pull/2315) Add in-place `mul!` specializations for modular matrices
- [#2316](https://github.com/Nemocas/Nemo.jl/pull/2316) Add `sub!` for `ZZModMatrix` / `FpMatrix`

### Fixed bugs that returned incorrect results

- [#2325](https://github.com/Nemocas/Nemo.jl/pull/2325) Fix a subtle bug in `digits!` for negative inputs

## [0.56.0](https://github.com/Nemocas/Nemo.jl/releases/tag/v0.56.0) - 2026-06-09

### New or extended functionality

- [#2152](https://github.com/Nemocas/Nemo.jl/pull/2152) Add new function `is_probably_zero_det` (for `ZZMatrix`, `QQMatrix`)
- [#2212](https://github.com/Nemocas/Nemo.jl/pull/2212) New determinant function `det_hcol_hnf` for ZZMatrix
- [#2287](https://github.com/Nemocas/Nemo.jl/pull/2287) Added QadicField constructors from a defining polynomial (new in flint 3.5.0)
- [#2293](https://github.com/Nemocas/Nemo.jl/pull/2293) Wrap elementary_divisors from flint
- [#2294](https://github.com/Nemocas/Nemo.jl/pull/2294) Wrap det_lu and det_precomp for arb_mat
- [#2296](https://github.com/Nemocas/Nemo.jl/pull/2296) Improve conversion from ZZMatrix to QQMatrix
- [#2297](https://github.com/Nemocas/Nemo.jl/pull/2297) Improve snf_with_transform for ZZMatrix
- [#2299](https://github.com/Nemocas/Nemo.jl/pull/2299) Add discrete `convolution` of two vectors

### Performance improvements or improved testing

- [#2289](https://github.com/Nemocas/Nemo.jl/pull/2289) Added precomputed Artin-Schreier LUP for characteristic 2 qadic sqrt (new in flint 3.5.0)

### Fixed bugs that returned incorrect results

- [#2302](https://github.com/Nemocas/Nemo.jl/pull/2302) Throw error when calling ill-defined FqFieldElem coercion
- [#2304](https://github.com/Nemocas/Nemo.jl/pull/2304) Throw an error when calling `residue_field(ZZ, n)` if  `n` is not a prime number
- [#2309](https://github.com/Nemocas/Nemo.jl/pull/2309) Fix correctness bug in `*(::fpMatrix, ::UInt)` for large UInts

## [0.55.1](https://github.com/Nemocas/Nemo.jl/releases/tag/v0.55.1) - 2026-05-16

### New or extended functionality
- [#2287](https://github.com/Nemocas/Nemo.jl/pull/2287) Added QadicField constructors from a defining polynomial (new in FLINT 3.5.0)
- [#2293](https://github.com/Nemocas/Nemo.jl/pull/2293) Wrap `elementary_divisors` from FLINT
- [#2294](https://github.com/Nemocas/Nemo.jl/pull/2294) Wrap `det_lu` and `det_precomp` for `arb_mat`


### Performance improvements or improved testing
- [#2289](https://github.com/Nemocas/Nemo.jl/pull/2289) Added precomputed Artin-Schreier LUP for characteristic 2 qadic sqrt (new in FLINT 3.5.0)

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

