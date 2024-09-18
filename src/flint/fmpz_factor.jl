###############################################################################
#
#   Flint factor(s) functiosn
#
###############################################################################

# raw fmpz_factor version
function _factor(a::ZZRingElem)
  F = fmpz_factor()
  ccall((:fmpz_factor, libflint), Nothing, (Ref{fmpz_factor}, Ref{ZZRingElem}), F, a)
  res = Dict{ZZRingElem, Int}()
  for i in 1:F.num
    z = ZZRingElem()
    ccall((:fmpz_factor_get_fmpz, libflint), Nothing,
          (Ref{ZZRingElem}, Ref{fmpz_factor}, Int), z, F, i - 1)
    res[z] = unsafe_load(F.exp, i)
  end
  return res, canonical_unit(a)
end

function factor(a::T) where T <: Union{Int, UInt}
  iszero(a) && throw(ArgumentError("Argument must be non-zero"))
  u = sign(a)
  a = u < 0 ? -a : a
  F = n_factor()
  ccall((:n_factor, libflint), Nothing, (Ref{n_factor}, UInt), F, a)
  res = Dict{T, Int}()
  for i in 1:F.num
    z = F.p[i]
    res[z] = F.exp[i]
  end
  return Fac(u, res)
end

################################################################################
#
#   ECM
#
################################################################################

function _ecm(a::ZZRingElem, B1::UInt, B2::UInt, ncrv::UInt,
    rnd = _flint_rand_states[Threads.threadid()])
  f = ZZRingElem()
  r = ccall((:fmpz_factor_ecm, libflint), Int32,
            (Ref{ZZRingElem}, UInt, UInt, UInt, Ref{rand_ctx}, Ref{ZZRingElem}),
            f, ncrv, B1, B2, rnd, a)
  return r, f
end

function _ecm(a::ZZRingElem, B1::Int, B2::Int, ncrv::Int,
    rnd = _flint_rand_states[Threads.threadid()])
  return _ecm(a, UInt(B1), UInt(B2), UInt(ncrv), rnd)
end

function ecm(a::ZZRingElem, max_digits::Int = div(ndigits(a), 2) + 1,
    rnd = _flint_rand_states[Threads.threadid()],
    B1 = _ecm_B1s[Threads.threadid()],
    nC = _ecm_nCs[Threads.threadid()])
  n = ndigits(a, 10)
  B1s = 15

  i = 1
  s = div(max_digits-15, 5) + 2
  s = max(i, s)
  while i <= s
    e, f = _ecm(a, B1[i]*1000, B1[i]*1000*100, nC[i], rnd)
    if e != 0
      return (e,f)
    end
    i += 1
    if i > length(B1)
      return (e, f)
    end
  end
  return (Int32(0), a)
end

################################################################################
#
#  Main work horse
#
################################################################################

# TODO: problem(s)
# - flint factor = mpqs is hopeless if > n digits, but asymptotically and
#   practically faster than ecm.
# - ecm is much better if there are "small" factors.
# - p-1 and p+1 methods are missing
#
# so probably
# - if n is small enough -> Nemo
# - if n is too large: ecm
# otherwise
# - need ecm to find small factors
# then recurse...

const big_primes = ZZRingElem[]

function factor(N::ZZRingElem)
  if iszero(N)
    throw(ArgumentError("Argument is not non-zero"))
  end
  N_in = N
  global big_primes
  r, c = factor_trial_range(N)
  for (p, v) = r
    N = divexact(N, p^v)
  end
  if is_unit(N)
    @assert N == c
    return Fac(c, r)
  end
  N *= c
  @assert N > 0

  for p = big_primes
    v, N = remove(N, p)
    if v > 0
      @assert !haskey(r, p)
      r[p] = v
    end
  end
  factor_insert!(r, N)
  for p = keys(r)
    if nbits(p) > 60 && !(p in big_primes)
      push!(big_primes, p)
    end
  end
  return Fac(c, r)
end

function factor_insert!(r::Dict{ZZRingElem,Int}, N::ZZRingElem, scale::Int=1)
  #assumes N to be positive
  #        no small divisors
  #        no big_primes
  if isone(N)
    return r
  end
  fac, N = is_perfect_power_with_data(N)
  if fac > 1
    return factor_insert!(r, N, fac * scale)
  end
  if is_prime(N)
    @assert !haskey(r, N)
    r[N] = scale
    return r
  end
  if ndigits(N) < 60
    s, = _factor(N) #MPQS
    for (p, k) in s
      if haskey(r, p)
        r[p] += k * scale
      else
        r[p] = k * scale
      end
    end
    return r
  end

  e, f = ecm(N)
  if e == 0
    s = factor(N)
    for (p, k) in s
      if haskey(r, p)
        r[p] += k * scale
      else
        r[p] = k * scale
      end
    end
    return r
  end
  cp = coprime_base([N, f])
  for i = cp
    factor_insert!(r, i, scale * valuation(N, i))
  end
  return r
end

################################################################################
#
#  Trial factorization
#
################################################################################

function factor_trial_range(N::ZZRingElem, start::Int=0, np::Int=10^5)
  F = fmpz_factor()
  ccall((:fmpz_factor_trial_range, libflint), Nothing, (Ref{fmpz_factor}, Ref{ZZRingElem}, UInt, UInt), F, N, start, np)
  res = Dict{ZZRingElem,Int}()
  for i in 1:F.num
    z = ZZRingElem()
    ccall((:fmpz_factor_get_fmpz, libflint), Nothing,
      (Ref{ZZRingElem}, Ref{fmpz_factor}, Int), z, F, i - 1)
    res[z] = unsafe_load(F.exp, i)
  end
  return res, canonical_unit(N)
end
