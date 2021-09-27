# Deprecated in 0.16.*

@deprecate ArbField(x::Int, y::Bool) ArbField(x, cached = y)

@deprecate AcbField(x::Int, y::Bool) AcbField(x, cached = y)

@deprecate prec(x::AcbMatSpace) precision(x)

@deprecate prec(x::ArbMatSpace) precision(x)

@deprecate prec(x::AcbPolyRing) precision(x)

@deprecate prec(x::ArbPolyRing) precision(x)

@deprecate prec(x::AcbField) precision(x)

@deprecate prec(x::ArbField) precision(x)

# Deprecated in 0.22.*

@deprecate binom(x::arb, n::UInt) binomial(x, n)

@deprecate binom(n::UInt, k::UInt, r::ArbField) binomial(n, k, r)

# Deprecated in 0.23.*

@deprecate modeta(x::acb) dedekind_eta(x)

@deprecate modweber_f(x::acb) modular_weber_f(x)

@deprecate modweber_f1(x::acb) modular_weber_f1(x)

@deprecate modweber_f2(x::acb) modular_weber_f2(x)

@deprecate modj(x::acb) j_invariant(x)

@deprecate modlambda(x::acb) modular_lambda(x)

@deprecate moddelta(x::acb) modular_delta(x)

@deprecate ei(x::acb) exp_integral_ei(x)

@deprecate si(x::acb) sin_integral(x)

@deprecate ci(x::acb) cos_integral(x)

@deprecate shi(x::acb) sinh_integral(x)

@deprecate chi(x::acb) cosh_integral(x)

@deprecate li(x::acb) log_integral(x)

@deprecate expint(s::acb, x::acb) exp_integral_e(s, x)

@deprecate lioffset(x::acb) log_integral_offset(x)

@deprecate hyp1f1(a::acb, b::acb, x::acb) hypergeometric_1f1(a, b, x)

@deprecate hyp1f1r(a::acb, b::acb, x::acb) hypergeometric_1f1_regularized(a, b, x)

@deprecate hyperu(a::acb, b::acb, x::acb) hypergeometric_u(a, b, x)

@deprecate hyp2f1(a::acb, b::acb, c::acb, x::acb) hypergeometric_2f1(a, b, c, x)

@deprecate jtheta(z::acb, tau::acb) jacobi_theta(z, tau)

@deprecate ellipwp(z::acb, tau::acb) weierstrass_p(z, tau)

@deprecate ellipk(x::acb) elliptic_k(x)

@deprecate ellipe(x::acb) elliptic_e(x)

@deprecate barnesg(x::acb) barnes_g(x)

@deprecate logbarnesg(x::acb) log_barnes_g(x)

@deprecate besselj(nu::acb, x::acb) bessel_j(nu, x)

@deprecate bessely(nu::acb, x::acb) bessel_y(nu, x)

@deprecate besseli(nu::acb, x::acb) bessel_i(nu, x)

@deprecate besselk(nu::acb, x::acb) bessel_k(nu, x)

@deprecate logsinpi(x::acb) log_sinpi(x)

@deprecate risingfac(x::acb, n::UInt) rising_factorial(x, n)

@deprecate risingfac(x::acb, n::Int) rising_factorial(x, n)

@deprecate risingfac2(x::acb, n::UInt) rising_factorial2(x, n)

@deprecate risingfac2(x::acb, n::Int) rising_factorial2(x, n)

@deprecate risingfac(x::arb, n::UInt) rising_factorial(x, n)

@deprecate risingfac(x::arb, n::Int) rising_factorial(x, n)

@deprecate risingfac(x::fmpq, n::UInt, r::ArbField) rising_factorial(x, n, r)

@deprecate risingfac(x::fmpq, n::Int, r::ArbField) rising_factorial(x, n, r)

@deprecate risingfac2(x::arb, n::UInt) rising_factorial2(x, n)

@deprecate risingfac2(x::arb, n::Int) rising_factorial2(x, n)

@deprecate fac(x::arb) factorial(x)

@deprecate fac(n::UInt, r::ArbField) factorial(n, r)

@deprecate fac(n::Int, r::ArbField) factorial(n, r)

@deprecate fib(n::fmpz, r::ArbField) fibonacci(n, r)

@deprecate fib(n::UInt, r::ArbField) fibonacci(n, r)

@deprecate fib(n::Int, r::ArbField) fibonacci(n, r)

# Deprecated in 0.27.*

@deprecate exppii(x::acb) cispi(x)

@deprecate isint(x::arb) isinteger(x)

@deprecate isint(x::acb) isinteger(x)

