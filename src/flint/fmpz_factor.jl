###############################################################################
#
#   FLINT factor(s) functions
#
###############################################################################

# raw fmpz_factor version
function _factor(a::ZZRingElem)
  F = fmpz_factor()
  @ccall libflint.fmpz_factor(F::Ref{fmpz_factor}, a::Ref{ZZRingElemRaw})::Nothing
  res = Dict{ZZRingElem, Int}()
  for i in 1:F.num
    z = ZZRingElem()
    @ccall libflint.fmpz_factor_get_fmpz(z::Ref{ZZRingElemRaw}, F::Ref{fmpz_factor}, (i - 1)::Int)::Nothing
    res[z] = unsafe_load(F.exp, i)
  end
  return res, canonical_unit(a)
end

# Just to give a helpful error message if someone tries to factor a boolean
function factor(::Bool)
  throw(DomainError("Cannot factorize a boolean"))
end


# The main dispatch function: flintify gives either Int or ZZRingElem,
# then use Julia's dispatcher to select the worker function.
function factor(a::T)  where T <: Integer
  @req  !iszero(a)  "Argument must be non-zero"
  return _factor(typeof(a), Nemo.flintify(a))
end


# Three internal worker functions: one for Int, one for UInt, one for ZZRingElem
function _factor(::Type{T}, a::Int) where T <: Integer
  abs_a = reinterpret(UInt, a < 0 ? -a : a) # like abs(a), but correct also when a == typemin(Int)
  fac = _factor(T, abs_a)
  if a < 0
    fac.unit = T(-1)  # OK since fac is mutable; gives error if T is Unsigned
  end
  return fac
end

function _factor(::Type{T}, a::UInt)  where T <: Integer
  @req  (T != Bool)  "Cannot have a factorization into booleans"
  @req  !iszero(a)  "Argument must be non-zero"
  F = n_factor()
  @ccall libflint.n_factor(F::Ref{n_factor}, a::UInt)::Nothing
  res = Dict{T, Int}()  # factor-multiplicity pairs
  for i in 1:F.num
    z = T(F.p[i])
    res[z] = F.exp[i]
  end
  return Fac(T(1), res)
end

function _factor(::Type{T}, a::ZZRingElem)  where T <: Integer
  @req  (T != Bool)  "Cannot have a factorization into booleans"
  @req  !iszero(a)  "Argument must be non-zero"
  u = T(sign(a))
  F = factor(abs(a))
  res = Dict{T, Int}()  # factor-multiplicity pairs
  for (fac,exp) in F.fac
    res[T(fac)] = exp
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
  r = @ccall libflint.fmpz_factor_ecm(f::Ref{ZZRingElemRaw}, ncrv::UInt, B1::UInt, B2::UInt, rnd::Ref{rand_ctx}, a::Ref{ZZRingElemRaw})::Int32
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
  @ccall libflint.fmpz_factor_trial_range(F::Ref{fmpz_factor}, N::Ref{ZZRingElemRaw}, start::UInt, np::UInt)::Nothing
  res = Dict{ZZRingElem,Int}()
  for i in 1:F.num
    z = ZZRingElem()
    @ccall libflint.fmpz_factor_get_fmpz(z::Ref{ZZRingElemRaw}, F::Ref{fmpz_factor}, (i - 1)::Int)::Nothing
    res[z] = unsafe_load(F.exp, i)
  end
  return res, canonical_unit(N)
end
