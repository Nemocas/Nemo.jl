# Changelog

All notable changes to this project will be documented in this file.

The format is based on [Keep a Changelog](https://keepachangelog.com/en/1.1.0/), and this project
tries to adhere to [Semantic Versioning](https://semver.org/spec/v2.0.0.html).

## [0.54.1](https://github.com/Nemocas/Nemo.jl/releases/tag/v0.54.1) - 2026-01-28

The following gives an overview of the changes compared to the previous release. This list is not
complete, many more internal or minor changes were made, but we tried to only list those changes
which we think might affect some users directly.

### Fixed bugs

- [#2242](https://github.com/Nemocas/Nemo.jl/pull/2242) Ensure `det` for 0x0 matrices returns 1
- [#2243](https://github.com/Nemocas/Nemo.jl/pull/2243) Add proper `min` for `Arb`s and `RealFieldElem`s

# [0.54.0](https://github.com/Nemocas/Nemo.jl/releases/tag/v0.54.0) - 2026-01-14

The following gives an overview of the changes compared to the previous release. This list is not
complete, many more internal or minor changes were made, but we tried to only list those changes
which we think might affect some users directly.

### Breaking changes

> !These changes break compatibility from previous versions!

- [#2158](https://github.com/Nemocas/Nemo.jl/pull/2158) Forbid (most) automatic polynomial ring coercions, use `change_base_ring` instead
- [#2229](https://github.com/Nemocas/Nemo.jl/pull/2229) Change the `prec` positional argument to a keyword argument for `derivative` and `integral` methods for `ComplexPolyRingElem` and `RealPolyRingElem`
- [#2233](https://github.com/Nemocas/Nemo.jl/pull/2233) Remove `replace!(::typeof(-), m::ZZMatrix)`

### New or extended functionality

- [#2191](https://github.com/Nemocas/Nemo.jl/pull/2191) Add `OneTo` method for `ZZRingElem`, returns ranges of new type `ZZOneTo`
- [#2222](https://github.com/Nemocas/Nemo.jl/pull/2222) Update to AbstractAlgebra 0.48

