# Deprecated in 0.22.*

@deprecate binom(x::ArbFieldElem, n::UInt) binomial(x, n)

@deprecate binom(n::UInt, k::UInt, r::ArbField) binomial(n, k, r)

# Deprecated in 0.23.*

@deprecate modeta(x::AcbFieldElem) dedekind_eta(x)

@deprecate modweber_f(x::AcbFieldElem) modular_weber_f(x)

@deprecate modweber_f1(x::AcbFieldElem) modular_weber_f1(x)

@deprecate modweber_f2(x::AcbFieldElem) modular_weber_f2(x)

@deprecate modj(x::AcbFieldElem) j_invariant(x)

@deprecate modlambda(x::AcbFieldElem) modular_lambda(x)

@deprecate moddelta(x::AcbFieldElem) modular_delta(x)

@deprecate ei(x::AcbFieldElem) exp_integral_ei(x)

@deprecate si(x::AcbFieldElem) sin_integral(x)

@deprecate ci(x::AcbFieldElem) cos_integral(x)

@deprecate shi(x::AcbFieldElem) sinh_integral(x)

@deprecate chi(x::AcbFieldElem) cosh_integral(x)

@deprecate li(x::AcbFieldElem) log_integral(x)

@deprecate expint(s::AcbFieldElem, x::AcbFieldElem) exp_integral_e(s, x)

@deprecate lioffset(x::AcbFieldElem) log_integral_offset(x)

@deprecate hyp1f1(a::AcbFieldElem, b::AcbFieldElem, x::AcbFieldElem) hypergeometric_1f1(a, b, x)

@deprecate hyp1f1r(a::AcbFieldElem, b::AcbFieldElem, x::AcbFieldElem) hypergeometric_1f1_regularized(a, b, x)

@deprecate hyperu(a::AcbFieldElem, b::AcbFieldElem, x::AcbFieldElem) hypergeometric_u(a, b, x)

@deprecate hyp2f1(a::AcbFieldElem, b::AcbFieldElem, c::AcbFieldElem, x::AcbFieldElem) hypergeometric_2f1(a, b, c, x)

@deprecate jtheta(z::AcbFieldElem, tau::AcbFieldElem) jacobi_theta(z, tau)

@deprecate ellipwp(z::AcbFieldElem, tau::AcbFieldElem) weierstrass_p(z, tau)

@deprecate ellipk(x::AcbFieldElem) elliptic_k(x)

@deprecate ellipe(x::AcbFieldElem) elliptic_e(x)

@deprecate barnesg(x::AcbFieldElem) barnes_g(x)

@deprecate logbarnesg(x::AcbFieldElem) log_barnes_g(x)

@deprecate besselj(nu::AcbFieldElem, x::AcbFieldElem) bessel_j(nu, x)

@deprecate bessely(nu::AcbFieldElem, x::AcbFieldElem) bessel_y(nu, x)

@deprecate besseli(nu::AcbFieldElem, x::AcbFieldElem) bessel_i(nu, x)

@deprecate besselk(nu::AcbFieldElem, x::AcbFieldElem) bessel_k(nu, x)

@deprecate logsinpi(x::AcbFieldElem) log_sinpi(x)

@deprecate risingfac(x::AcbFieldElem, n::UInt) rising_factorial(x, n)

@deprecate risingfac(x::AcbFieldElem, n::Int) rising_factorial(x, n)

@deprecate risingfac2(x::AcbFieldElem, n::UInt) rising_factorial2(x, n)

@deprecate risingfac2(x::AcbFieldElem, n::Int) rising_factorial2(x, n)

@deprecate risingfac(x::ArbFieldElem, n::UInt) rising_factorial(x, n)

@deprecate risingfac(x::ArbFieldElem, n::Int) rising_factorial(x, n)

@deprecate risingfac(x::QQFieldElem, n::UInt, r::ArbField) rising_factorial(x, n, r)

@deprecate risingfac(x::QQFieldElem, n::Int, r::ArbField) rising_factorial(x, n, r)

@deprecate risingfac2(x::ArbFieldElem, n::UInt) rising_factorial2(x, n)

@deprecate risingfac2(x::ArbFieldElem, n::Int) rising_factorial2(x, n)

@deprecate fac(x::ArbFieldElem) factorial(x)

@deprecate fac(n::UInt, r::ArbField) factorial(n, r)

@deprecate fac(n::Int, r::ArbField) factorial(n, r)

@deprecate fib(n::ZZRingElem, r::ArbField) fibonacci(n, r)

@deprecate fib(n::UInt, r::ArbField) fibonacci(n, r)

@deprecate fib(n::Int, r::ArbField) fibonacci(n, r)

# Deprecated in 0.34.*

@deprecate gso(x::QQMatrix) gram_schmidt_orthogonalisation(x)

# Deprecated in 0.35.*

@deprecate roots(f::QQPolyRingElem, R::QQBarField) roots(R, f)

@deprecate roots(f::ZZPolyRingElem, R::QQBarField) roots(R, f)

@deprecate roots(f::fpPolyRingElem, K::fqPolyRepField) roots(K, f)

@deprecate roots(f::FpPolyRingElem, K::FqPolyRepField) roots(K, f)

@deprecate roots(f::QQPolyRingElem, R::T) where {T<:Union{fqPolyRepField,fpField}} roots(R, f)

@deprecate factor(f::QQPolyRingElem, R::T) where {T<:Union{fqPolyRepField,fpField}} factor(R, f)

# Deprecated in 0.39.*

@deprecate divisible(x::Int, y::Int) is_divisible_by(x, y)
@deprecate divisible(x::ZZRingElem, y::Int) is_divisible_by(x, y)
@deprecate divisible(x::ZZRingElem, y::ZZRingElem) is_divisible_by(x, y)
