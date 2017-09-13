module Generic

import Base: Array, abs, asin, asinh, atan, atanh, base, bin, checkbounds,
             conj, convert, cmp, contains, cos, cosh, dec, deepcopy,
             deepcopy_internal, den, deserialize, det, div, divrem, exp, eye,
             gcd, gcdx, getindex, hash, hcat, hex, intersect, inv, invmod,
             isequal, isfinite, isless, isqrt, isreal, iszero, lcm,
             ldexp, length, log, lufact, lufact!, mod, ndigits, nextpow2, norm,
             nullspace, num, oct, one, parent, parse, precision, prevpow2,
             rand, rank, Rational, rem, reverse, serialize, setindex!, show,
             similar, sign, sin, sinh, size, sqrt, string, tan, tanh, trace,
             trailing_zeros, transpose, transpose!, truncate, typed_hvcat,
             typed_hcat, var, vcat, zero, zeros, +, -, *, ==, ^, &, |, $, <<,
             >>, ~, <=, >=, <, >, //, /, !=

if VERSION >= v"0.7.0-DEV.1144"
import Base: isone
end

import Base: floor, ceil, hypot, sqrt, log, log1p, exp, expm1, sin, cos, sinpi,
             cospi, tan, cot, sinh, cosh, tanh, coth, atan, asin, acos, atanh,
             asinh, acosh, gamma, lgamma, sinpi, cospi, atan2

import Nemo: Integers, Rationals, Ring, RingElem, RingElement, Field,
             FieldElement, promote_rule

using Nemo

include("generic/GenericTypes.jl")

include("generic/PermGroups.jl")

include("generic/YoungTabs.jl")

include("generic/Residue.jl")

include("generic/Poly.jl")

include("generic/MPoly.jl")

include("generic/SparsePoly.jl")

include("generic/RelSeries.jl")

include("generic/AbsSeries.jl")

include("generic/Matrix.jl")

include("generic/Fraction.jl")

end # generic