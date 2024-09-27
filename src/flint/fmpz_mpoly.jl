###############################################################################
#
#   ZZMPolyRingElem.jl : Flint multivariate polynomials over ZZRingElem
#
###############################################################################

###############################################################################
#
#   Data type and parent object methods
#
###############################################################################

parent_type(::Type{ZZMPolyRingElem}) = ZZMPolyRing

elem_type(::Type{ZZMPolyRing}) = ZZMPolyRingElem

mpoly_type(::Type{ZZRingElem}) = ZZMPolyRingElem

symbols(a::ZZMPolyRing) = a.S

parent(a::ZZMPolyRingElem) = a.parent

number_of_variables(a::ZZMPolyRing) = ccall((:fmpz_mpoly_ctx_nvars, libflint), Int,
                                            (Ref{ZZMPolyRing}, ), a)

base_ring(a::ZZMPolyRing) = ZZ

function internal_ordering(a::ZZMPolyRing)
  b = ccall((:fmpz_mpoly_ctx_ord, libflint), Cint, (Ref{ZZMPolyRing}, ), a)
  return flint_orderings[b + 1]
end

function gens(R::ZZMPolyRing)
  A = Vector{ZZMPolyRingElem}(undef, R.nvars)
  for i = 1:R.nvars
    z = R()
    ccall((:fmpz_mpoly_gen, libflint), Nothing,
          (Ref{ZZMPolyRingElem}, Int, Ref{ZZMPolyRing}), z, i - 1, R)
    A[i] = z
  end
  return A
end

function gen(R::ZZMPolyRing, i::Int)
  n = nvars(R)
  (i <= 0 || i > n) && error("Index must be between 1 and $n")
  z = R()
  ccall((:fmpz_mpoly_gen, libflint), Nothing,
        (Ref{ZZMPolyRingElem}, Int, Ref{ZZMPolyRing}), z, i - 1, R)
  return z
end

function is_gen(a::ZZMPolyRingElem, i::Int)
  n = nvars(parent(a))
  (i <= 0 || i > n) && error("Index must be between 1 and $n")
  R = parent(a)
  return Bool(ccall((:fmpz_mpoly_is_gen, libflint), Cint,
                    (Ref{ZZMPolyRingElem}, Int, Ref{ZZMPolyRing}),
                    a, i - 1, a.parent))
end

function is_gen(a::ZZMPolyRingElem)
  n = nvars(parent(a))
  for i in 1:n
    is_gen(a, i) && return true
  end
  return false
end

function deepcopy_internal(a::ZZMPolyRingElem, dict::IdDict)
  z = parent(a)()
  ccall((:fmpz_mpoly_set, libflint), Nothing,
        (Ref{ZZMPolyRingElem}, Ref{ZZMPolyRingElem}, Ref{ZZMPolyRing}),
        z, a, a.parent)
  return z
end

function length(a::ZZMPolyRingElem)
  n = ccall((:fmpz_mpoly_length, libflint), Int, (Ref{ZZMPolyRingElem}, ), a)
  return n
end

one(R::ZZMPolyRing) = one!(R())

zero(R::ZZMPolyRing) = zero!(R())

function isone(a::ZZMPolyRingElem)
  b = ccall((:fmpz_mpoly_is_one, libflint), Cint,
            (Ref{ZZMPolyRingElem}, Ref{ZZMPolyRing}), a, a.parent)
  return Bool(b)
end

function iszero(a::ZZMPolyRingElem)
  b = ccall((:fmpz_mpoly_is_zero, libflint), Cint,
            (Ref{ZZMPolyRingElem}, Ref{ZZMPolyRing}), a, a.parent)
  return Bool(b)
end

function is_monomial(a::ZZMPolyRingElem)
  return length(a) == 1 && coeff(a, 1) == 1
end

function is_term(a::ZZMPolyRingElem)
  return length(a) == 1
end

function is_unit(a::ZZMPolyRingElem)
  return length(a) == 1 && total_degree(a) == 0 && is_unit(coeff(a, 1))
end

function is_constant(a::ZZMPolyRingElem)
  b = ccall((:fmpz_mpoly_is_fmpz, libflint), Cint,
            (Ref{ZZMPolyRingElem}, Ref{ZZMPolyRing}), a, parent(a))
  return Bool(b)
end

function fit!(a::ZZMPolyRingElem, n::Int)
  # needs to exist for the MPoly interface
  return nothing
end

################################################################################
#
#  Getting coefficients
#
################################################################################

function coeff(a::ZZMPolyRingElem, i::Int)
  z = ZZRingElem()
  n = length(a)
  # this check is not needed as fmpz_mpoly_get_term_coeff_fmpz throws
  (i < 1 || i > n) && error("Index must be between 1 and $(length(a))")
  ccall((:fmpz_mpoly_get_term_coeff_fmpz, libflint), Nothing,
        (Ref{ZZRingElem}, Ref{ZZMPolyRingElem}, Int, Ref{ZZMPolyRing}),
        z, a, i - 1, a.parent)
  return z
end

function coeff(a::ZZMPolyRingElem, b::ZZMPolyRingElem)
  check_parent(a, b)
  !isone(length(b)) && error("Second argument must be a monomial")
  z = ZZRingElem()
  ccall((:fmpz_mpoly_get_coeff_fmpz_monomial, libflint), Nothing,
        (Ref{ZZRingElem}, Ref{ZZMPolyRingElem}, Ref{ZZMPolyRingElem}, Ref{ZZMPolyRing}),
        z, a, b, parent(a))
  return z
end

function trailing_coefficient(p::ZZPolyRingElem)
  if iszero(p)
    return zero(base_ring(p))
  else
    return coeff(p, length(p))
  end
end

###############################################################################
#
#   Basic manipulation
#
###############################################################################

# Degree in the i-th variable as an Int
function degree(a::ZZMPolyRingElem, i::Int)
  n = nvars(parent(a))
  (i <= 0 || i > n) && error("Index must be between 1 and $n")
  if degrees_fit_int(a)
    d = ccall((:fmpz_mpoly_degree_si, libflint), Int,
              (Ref{ZZMPolyRingElem}, Int, Ref{ZZMPolyRing}), a, i - 1, a.parent)
    return d
  else
    return Int(degree_fmpz(a, i))
  end
end

# Degree in the i-th variable as an ZZRingElem
function degree_fmpz(a::ZZMPolyRingElem, i::Int)
  n = nvars(parent(a))
  (i <= 0 || i > n) && error("Index must be between 1 and $n")
  d = ZZRingElem()
  ccall((:fmpz_mpoly_degree_fmpz, libflint), Nothing,
        (Ref{ZZRingElem}, Ref{ZZMPolyRingElem}, Int, Ref{ZZMPolyRing}),
        d, a, i - 1, a.parent)
  return d
end

# Return true if degrees fit into an Int
function degrees_fit_int(a::ZZMPolyRingElem)
  b = ccall((:fmpz_mpoly_degrees_fit_si, libflint), Cint,
            (Ref{ZZMPolyRingElem}, Ref{ZZMPolyRing}), a, a.parent)
  return Bool(b)
end

# Return an array of the max degrees in each variable
function degrees(a::ZZMPolyRingElem)
  if !degrees_fit_int(a)
    throw(OverflowError("degrees of polynomial do not fit into Int"))
  end
  degs = Vector{Int}(undef, nvars(parent(a)))
  ccall((:fmpz_mpoly_degrees_si, libflint), Nothing,
        (Ptr{Int}, Ref{ZZMPolyRingElem}, Ref{ZZMPolyRing}),
        degs, a, a.parent)
  return degs
end

# Return an array of the max degrees as fmpzs in each variable
function degrees_fmpz(a::ZZMPolyRingElem)
  n = nvars(parent(a))
  degs = Vector{ZZRingElem}(undef, n)
  for i in 1:n
    degs[i] = ZZRingElem()
  end
  ccall((:fmpz_mpoly_degrees_fmpz, libflint), Nothing,
        (Ptr{Ref{ZZRingElem}}, Ref{ZZMPolyRingElem}, Ref{ZZMPolyRing}),
        degs, a, a.parent)
  return degs
end

# Return true if degree fits into an Int
function total_degree_fits_int(a::ZZMPolyRingElem)
  b = ccall((:fmpz_mpoly_total_degree_fits_si, libflint), Cint,
            (Ref{ZZMPolyRingElem}, Ref{ZZMPolyRing}), a, a.parent)
  return Bool(b)
end

# Total degree as an Int
function total_degree(a::ZZMPolyRingElem)
  if !total_degree_fits_int(a)
    throw(OverflowError("Total degree of polynomial does not fit into Int"))
  end
  d = ccall((:fmpz_mpoly_total_degree_si, libflint), Int,
            (Ref{ZZMPolyRingElem}, Ref{ZZMPolyRing}), a, a.parent)
  return d
end

# Total degree as an ZZRingElem
function total_degree_fmpz(a::ZZMPolyRingElem)
  d = ZZRingElem()
  ccall((:fmpz_mpoly_total_degree_fmpz, libflint), Nothing,
        (Ref{ZZRingElem}, Ref{ZZMPolyRingElem}, Ref{ZZMPolyRing}),
        d, a, a.parent)
  return d
end

###############################################################################
#
#   Multivariable coefficient polynomials
#
###############################################################################

function coeff(a::ZZMPolyRingElem, vars::Vector{Int}, exps::Vector{Int})
  unique(vars) != vars && error("Variables not unique")
  length(vars) != length(exps) &&
  error("Number of variables does not match number of exponents")
  z = parent(a)()
  vars = [UInt(i) - 1 for i in vars]
  for i = 1:length(vars)
    if vars[i] < 0 || vars[i] >= nvars(parent(a))
      error("Variable index not in range")
    end
    if exps[i] < 0
      error("Exponent cannot be negative")
    end
  end
  ccall((:fmpz_mpoly_get_coeff_vars_ui, libflint), Nothing,
        (Ref{ZZMPolyRingElem}, Ref{ZZMPolyRingElem}, Ptr{Int},
         Ptr{Int}, Int, Ref{ZZMPolyRing}),
        z, a, vars, exps, length(vars), a.parent)
  return z
end

###############################################################################
#
#   Basic arithmetic
#
###############################################################################

function -(a::ZZMPolyRingElem)
  z = parent(a)()
  ccall((:fmpz_mpoly_neg, libflint), Nothing,
        (Ref{ZZMPolyRingElem}, Ref{ZZMPolyRingElem}, Ref{ZZMPolyRing}),
        z, a, a.parent)
  return z
end

function +(a::ZZMPolyRingElem, b::ZZMPolyRingElem)
  check_parent(a, b)
  z = parent(a)()
  ccall((:fmpz_mpoly_add, libflint), Nothing,
        (Ref{ZZMPolyRingElem}, Ref{ZZMPolyRingElem}, Ref{ZZMPolyRingElem}, Ref{ZZMPolyRing}),
        z, a, b, a.parent)
  return z
end

function -(a::ZZMPolyRingElem, b::ZZMPolyRingElem)
  check_parent(a, b)
  z = parent(a)()
  ccall((:fmpz_mpoly_sub, libflint), Nothing,
        (Ref{ZZMPolyRingElem}, Ref{ZZMPolyRingElem}, Ref{ZZMPolyRingElem}, Ref{ZZMPolyRing}),
        z, a, b, a.parent)
  return z
end

function *(a::ZZMPolyRingElem, b::ZZMPolyRingElem)
  check_parent(a, b)
  z = parent(a)()
  ccall((:fmpz_mpoly_mul, libflint), Nothing,
        (Ref{ZZMPolyRingElem}, Ref{ZZMPolyRingElem}, Ref{ZZMPolyRingElem}, Ref{ZZMPolyRing}),
        z, a, b, a.parent)
  return z
end

###############################################################################
#
#   Ad hoc arithmetic
#
###############################################################################

for (jT, cN, cT) in ((ZZRingElem, :fmpz, Ref{ZZRingElem}), (Int, :si, Int), (UInt, :ui, UInt))
  @eval begin
    function +(a::ZZMPolyRingElem, b::($jT))
      z = parent(a)()
      ccall(($(string(:fmpz_mpoly_add_, cN)), libflint), Nothing,
            (Ref{ZZMPolyRingElem}, Ref{ZZMPolyRingElem}, ($cT), Ref{ZZMPolyRing}),
            z, a, b, parent(a))
      return z
    end

    +(a::($jT), b::ZZMPolyRingElem) = b + a

    function -(a::ZZMPolyRingElem, b::($jT))
      z = parent(a)()
      ccall(($(string(:fmpz_mpoly_sub_, cN)), libflint), Nothing,
            (Ref{ZZMPolyRingElem}, Ref{ZZMPolyRingElem}, ($cT), Ref{ZZMPolyRing}),
            z, a, b, parent(a))
      return z
    end

    -(a::($jT), b::ZZMPolyRingElem) = - (b - a)

    function *(a::ZZMPolyRingElem, b::($jT))
      z = parent(a)()
      ccall(($(string(:fmpz_mpoly_scalar_mul_, cN)), libflint), Nothing,
            (Ref{ZZMPolyRingElem}, Ref{ZZMPolyRingElem}, ($cT), Ref{ZZMPolyRing}),
            z, a, b, parent(a))
      return z
    end

    *(a::($jT), b::ZZMPolyRingElem) = b * a

    function divexact(a::ZZMPolyRingElem, b::($jT); check::Bool=true)
      z = parent(a)()
      if check
        divides = Bool(ccall(($(string(:fmpz_mpoly_scalar_divides_, cN)), libflint), Cint,
                             (Ref{ZZMPolyRingElem}, Ref{ZZMPolyRingElem}, ($cT), Ref{ZZMPolyRing}),
                             z, a, b, parent(a)))
        divides || error("Division is not exact in divexact")
      else
        ccall(($(string(:fmpz_mpoly_scalar_divexact_, cN)), libflint), Nothing,
              (Ref{ZZMPolyRingElem}, Ref{ZZMPolyRingElem}, ($cT), Ref{ZZMPolyRing}),
              z, a, b, parent(a))
      end
      return z
    end
  end
end

+(a::ZZMPolyRingElem, b::Integer) = a + flintify(b)

+(a::Integer, b::ZZMPolyRingElem) = b + a

-(a::ZZMPolyRingElem, b::Integer) = a - flintify(b)

-(a::Integer, b::ZZMPolyRingElem) = -(b - a)

*(a::ZZMPolyRingElem, b::Integer) = a * flintify(b)

*(a::Integer, b::ZZMPolyRingElem) = b * a

divexact(a::ZZMPolyRingElem, b::Integer; check::Bool=true) = divexact(a, ZZRingElem(b); check=check)

###############################################################################
#
#   Powering
#
###############################################################################

function ^(a::ZZMPolyRingElem, b::Int)
  b < 0 && throw(DomainError(b, "Exponent must be non-negative"))
  z = parent(a)()
  ccall((:fmpz_mpoly_pow_ui, libflint), Nothing,
        (Ref{ZZMPolyRingElem}, Ref{ZZMPolyRingElem}, Int, Ref{ZZMPolyRing}),
        z, a, b, parent(a))
  return z
end

function ^(a::ZZMPolyRingElem, b::ZZRingElem)
  b < 0 && throw(DomainError(b, "Exponent must be non-negative"))
  z = parent(a)()
  ccall((:fmpz_mpoly_pow_fmpz, libflint), Nothing,
        (Ref{ZZMPolyRingElem}, Ref{ZZMPolyRingElem}, Ref{ZZRingElem}, Ref{ZZMPolyRing}),
        z, a, b, parent(a))
  return z
end

################################################################################
#
#   GCD
#
################################################################################

function gcd(a::ZZMPolyRingElem, b::ZZMPolyRingElem)
  check_parent(a, b)
  z = parent(a)()
  r = ccall((:fmpz_mpoly_gcd, libflint), Cint,
            (Ref{ZZMPolyRingElem}, Ref{ZZMPolyRingElem}, Ref{ZZMPolyRingElem}, Ref{ZZMPolyRing}),
            z, a, b, a.parent)
  r == 0 && error("Unable to compute gcd")
  return z
end

function gcd_with_cofactors(a::ZZMPolyRingElem, b::ZZMPolyRingElem)
  z = parent(a)()
  abar = parent(a)()
  bbar = parent(a)()
  r = ccall((:fmpz_mpoly_gcd_cofactors, libflint), Cint,
            (Ref{ZZMPolyRingElem}, Ref{ZZMPolyRingElem}, Ref{ZZMPolyRingElem},
             Ref{ZZMPolyRingElem}, Ref{ZZMPolyRingElem}, Ref{ZZMPolyRing}),
            z, abar, bbar, a, b, a.parent)
  r == 0 && error("Unable to compute gcd")
  return z, abar, bbar
end

################################################################################
#
#   Factorization and Square Root
#
################################################################################

function (::Type{Fac{ZZMPolyRingElem}})(fac::fmpz_mpoly_factor, preserve_input::Bool = true)
  R = fac.parent
  F = Fac{ZZMPolyRingElem}()
  empty!(F.fac)
  for i in 0:fac.num-1
    f = R()
    if preserve_input
      ccall((:fmpz_mpoly_factor_get_base, libflint), Nothing,
            (Ref{ZZMPolyRingElem}, Ref{fmpz_mpoly_factor}, Int, Ref{ZZMPolyRing}),
            f, fac, i, R)
    else
      ccall((:fmpz_mpoly_factor_swap_base, libflint), Nothing,
            (Ref{ZZMPolyRingElem}, Ref{fmpz_mpoly_factor}, Int, Ref{ZZMPolyRing}),
            f, fac, i, R)
    end
    F.fac[f] = ccall((:fmpz_mpoly_factor_get_exp_si, libflint), Int,
                     (Ref{fmpz_mpoly_factor}, Int, Ref{ZZMPolyRing}),
                     fac, i, R)
  end
  c = ZZRingElem()
  ccall((:fmpz_mpoly_factor_get_constant_fmpz, libflint), Nothing,
        (Ref{ZZRingElem}, Ref{fmpz_mpoly_factor}),
        c, fac)
  sgnc = ccall((:fmpz_sgn, libflint), Cint, (Ref{ZZRingElem},), c)
  if sgnc != 0
    G = fmpz_factor()
    ccall((:fmpz_factor, libflint), Nothing, (Ref{fmpz_factor}, Ref{ZZRingElem}), G, c)
    for i in 1:G.num
      ccall((:fmpz_factor_get_fmpz, libflint), Nothing,
            (Ref{ZZRingElem}, Ref{fmpz_factor}, Int), c, G, i - 1)
      F.fac[R(c)] = unsafe_load(G.exp, i)
    end
  end
  F.unit = R(sgnc)
  return F
end

function factor(a::ZZMPolyRingElem)
  iszero(a) && throw(ArgumentError("Argument must be non-zero"))
  R = parent(a)
  fac = fmpz_mpoly_factor(R)
  ok = ccall((:fmpz_mpoly_factor, libflint), Cint,
             (Ref{fmpz_mpoly_factor}, Ref{ZZMPolyRingElem}, Ref{ZZMPolyRing}),
             fac, a, R)
  ok == 0 && error("unable to compute factorization")
  return Fac{ZZMPolyRingElem}(fac, false)
end

function factor_squarefree(a::ZZMPolyRingElem)
  iszero(a) && throw(ArgumentError("Argument must be non-zero"))
  R = parent(a)
  fac = fmpz_mpoly_factor(R)
  ok = ccall((:fmpz_mpoly_factor_squarefree, libflint), Cint,
             (Ref{fmpz_mpoly_factor}, Ref{ZZMPolyRingElem}, Ref{ZZMPolyRing}),
             fac, a, R)
  ok == 0 && error("unable to compute factorization")
  return Fac{ZZMPolyRingElem}(fac, false)
end


function sqrt(a::ZZMPolyRingElem; check::Bool=true)
  q = parent(a)()
  flag = Bool(ccall((:fmpz_mpoly_sqrt_heap, libflint), Cint,
                    (Ref{ZZMPolyRingElem}, Ref{ZZMPolyRingElem}, Ref{ZZMPolyRing}, Cint),
                    q, a, a.parent, Cint(check)))
  check && !flag && error("Not a square")
  return q
end

function is_square(a::ZZMPolyRingElem)
  return Bool(ccall((:fmpz_mpoly_is_square, libflint), Cint,
                    (Ref{ZZMPolyRingElem}, Ref{ZZMPolyRing}),
                    a, a.parent))
end

function is_square_with_sqrt(a::ZZMPolyRingElem)
  q = parent(a)()
  flag = ccall((:fmpz_mpoly_sqrt, libflint), Cint,
               (Ref{ZZMPolyRingElem}, Ref{ZZMPolyRingElem}, Ref{ZZMPolyRing}),
               q, a, a.parent)
  return (Bool(flag), q)
end

###############################################################################
#
#   Comparison
#
###############################################################################

function ==(a::ZZMPolyRingElem, b::ZZMPolyRingElem)
  check_parent(a, b)
  return Bool(ccall((:fmpz_mpoly_equal, libflint), Cint,
                    (Ref{ZZMPolyRingElem}, Ref{ZZMPolyRingElem}, Ref{ZZMPolyRing}),
                    a, b, a.parent))
end

function Base.isless(a::ZZMPolyRingElem, b::ZZMPolyRingElem)
  (!is_monomial(a) || !is_monomial(b)) && error("Not monomials in comparison")
  return ccall((:fmpz_mpoly_cmp, libflint), Cint,
               (Ref{ZZMPolyRingElem}, Ref{ZZMPolyRingElem}, Ref{ZZMPolyRing}),
               a, b, a.parent) < 0
end

###############################################################################
#
#   Ad hoc comparison
#
###############################################################################

function ==(a::ZZMPolyRingElem, b::ZZRingElem)
  return Bool(ccall((:fmpz_mpoly_equal_fmpz, libflint), Cint,
                    (Ref{ZZMPolyRingElem}, Ref{ZZRingElem}, Ref{ZZMPolyRing}),
                    a, b, a.parent))
end

==(a::ZZRingElem, b::ZZMPolyRingElem) = b == a

function ==(a::ZZMPolyRingElem, b::Int)
  return Bool(ccall((:fmpz_mpoly_equal_si, libflint), Cint,
                    (Ref{ZZMPolyRingElem}, Int, Ref{ZZMPolyRing}),
                    a, b, a.parent))
end

==(a::Int, b::ZZMPolyRingElem) = b == a

==(a::ZZMPolyRingElem, b::Integer) = a == flintify(b)

==(a::Integer, b::ZZMPolyRingElem) = b == a

###############################################################################
#
#   Divisibility
#
###############################################################################

function divides(a::ZZMPolyRingElem, b::ZZMPolyRingElem)
  check_parent(a, b)
  if iszero(a)
    return true, zero(parent(a))
  end
  if iszero(b)
    return false, zero(parent(a))
  end
  z = parent(a)()
  d = ccall((:fmpz_mpoly_divides, libflint), Cint,
            (Ref{ZZMPolyRingElem}, Ref{ZZMPolyRingElem}, Ref{ZZMPolyRingElem}, Ref{ZZMPolyRing}),
            z, a, b, a.parent)
  return isone(d), z
end

###############################################################################
#
#   Division with remainder
#
###############################################################################

function Base.div(a::ZZMPolyRingElem, b::ZZMPolyRingElem)
  check_parent(a, b)
  q = parent(a)()
  ccall((:fmpz_mpoly_div, libflint), Nothing,
        (Ref{ZZMPolyRingElem}, Ref{ZZMPolyRingElem},
         Ref{ZZMPolyRingElem}, Ref{ZZMPolyRing}),
        q, a, b, a.parent)
  return q
end

function Base.divrem(a::ZZMPolyRingElem, b::ZZMPolyRingElem)
  check_parent(a, b)
  q = parent(a)()
  r = parent(a)()
  ccall((:fmpz_mpoly_divrem, libflint), Nothing,
        (Ref{ZZMPolyRingElem}, Ref{ZZMPolyRingElem}, Ref{ZZMPolyRingElem},
         Ref{ZZMPolyRingElem}, Ref{ZZMPolyRing}),
        q, r, a, b, a.parent)
  return q, r
end

function Base.divrem(a::ZZMPolyRingElem, b::Vector{ZZMPolyRingElem})
  len = length(b)
  q = [parent(a)() for i in 1:len]
  r = parent(a)()
  ccall((:fmpz_mpoly_divrem_ideal, libflint), Nothing,
        (Ptr{Ref{ZZMPolyRingElem}}, Ref{ZZMPolyRingElem}, Ref{ZZMPolyRingElem},
         Ptr{Ref{ZZMPolyRingElem}}, Int, Ref{ZZMPolyRing}),
        q, r, a, b, len, a.parent)
  return q, r
end

###############################################################################
#
#   Exact division
#
###############################################################################

function divexact(a::ZZMPolyRingElem, b::ZZMPolyRingElem; check::Bool=true)
  check_parent(a, b)
  b, q = divides(a, b)
  check && !b && error("Division is not exact in divexact")
  return q
end

###############################################################################
#
#   Calculus
#
###############################################################################

function derivative(a::ZZMPolyRingElem, i::Int)
  n = nvars(parent(a))
  (i <= 0 || i > n) && error("Index must be between 1 and $n")
  z = parent(a)()
  ccall((:fmpz_mpoly_derivative, libflint), Nothing,
        (Ref{ZZMPolyRingElem}, Ref{ZZMPolyRingElem}, Int, Ref{ZZMPolyRing}),
        z, a, i - 1, parent(a))
  return z
end

###############################################################################
#
#   Evaluation
#
###############################################################################

function evaluate(a::ZZMPolyRingElem, b::Vector{ZZRingElem})
  length(b) != nvars(parent(a)) && error("Vector size incorrect in evaluate")
  z = ZZRingElem()
  GC.@preserve b ccall((:fmpz_mpoly_evaluate_all_fmpz, libflint), Nothing,
                       (Ref{ZZRingElem}, Ref{ZZMPolyRingElem}, Ptr{ZZRingElem}, Ref{ZZMPolyRing}),
                       z, a, b, parent(a))
  return z
end

function evaluate(a::ZZMPolyRingElem, b::Vector{<:Integer})
  fmpz_vec = [ZZRingElem(s) for s in b]
  return evaluate(a, fmpz_vec)
end

function (a::ZZMPolyRingElem)()
  error("need at least one value")
end

function (a::ZZMPolyRingElem)(vals::ZZRingElem...)
  length(vals) != nvars(parent(a)) && error("Number of variables does not match number of values")
  return evaluate(a, [vals...])
end

function (a::ZZMPolyRingElem)(vals::Integer...)
  length(vals) != nvars(parent(a)) && error("Number of variables does not match number of values")
  return evaluate(a, [vals...])
end

function (a::ZZMPolyRingElem)(vals::Union{NCRingElem, RingElement}...)
  length(vals) != nvars(parent(a)) && error("Number of variables does not match number of values")
  R = base_ring(a)
  # The best we can do here is to cache previously used powers of the values
  # being substituted, as we cannot assume anything about the relative
  # performance of powering vs multiplication. The function should not try
  # to optimise computing new powers in any way.
  # Note that this function accepts values in a non-commutative ring, so operations
  # must be done in a certain order.
  powers = [Dict{Int, Any}() for i in 1:length(vals)]
  # First work out types of products
  r = R()
  c = zero(R)
  U = Vector{Any}(undef, length(vals))
  for j = 1:length(vals)
    W = typeof(vals[j])
    if ((W <: Integer && W != BigInt) ||
        (W <: Rational && W != Rational{BigInt}))
      c = c*zero(W)
      U[j] = parent(c)
    else
      U[j] = parent(vals[j])
      c = c*zero(parent(vals[j]))
    end
  end
  for i = 1:length(a)
    v = exponent_vector(a, i)
    t = coeff(a, i)
    for j = 1:length(vals)
      exp = v[j]
      if !haskey(powers[j], exp)
        powers[j][exp] = (U[j](vals[j]))^exp
      end
      t = t*powers[j][exp]
    end
    r += t
  end
  return r
end

function evaluate(a::ZZMPolyRingElem, bs::Vector{ZZMPolyRingElem})
  R = parent(a)
  S = parent(bs[1])

  length(bs) != nvars(R) &&
  error("Number of variables does not match number of values")

  c = S()
  fl = ccall((:fmpz_mpoly_compose_fmpz_mpoly, libflint), Cint,
             (Ref{ZZMPolyRingElem}, Ref{ZZMPolyRingElem}, Ptr{Ref{ZZMPolyRingElem}},
              Ref{ZZMPolyRing}, Ref{ZZMPolyRing}),
             c, a, bs, R, S)
  fl == 0 && error("Something wrong in evaluation.")
  return c
end

function evaluate(a::ZZMPolyRingElem, bs::Vector{ZZPolyRingElem})
  R = parent(a)
  S = parent(bs[1])

  length(bs) != nvars(R) &&
  error("Number of variables does not match number of values")

  c = S()
  fl = ccall((:fmpz_mpoly_compose_fmpz_poly, libflint), Cint,
             (Ref{ZZPolyRingElem}, Ref{ZZMPolyRingElem}, Ptr{Ref{ZZPolyRingElem}},
              Ref{ZZMPolyRing}), c, a, bs, R)
  fl == 0 && error("Something wrong in evaluation.")
  return c
end

###############################################################################
#
#   Unsafe functions
#
###############################################################################

function zero!(a::ZZMPolyRingElem)
  ccall((:fmpz_mpoly_zero, libflint), Nothing,
        (Ref{ZZMPolyRingElem}, Ref{ZZMPolyRing}), a, a.parent)
  return a
end

function one!(a::ZZMPolyRingElem)
  ccall((:fmpz_mpoly_one, libflint), Nothing,
        (Ref{ZZMPolyRingElem}, Ref{ZZMPolyRing}), a, a.parent)
  return a
end

function neg!(z::ZZMPolyRingElem, a::ZZMPolyRingElem)
  ccall((:fmpz_mpoly_neg, libflint), Nothing,
        (Ref{ZZMPolyRingElem}, Ref{ZZMPolyRingElem}, Ref{ZZMPolyRing}), z, a, a.parent)
  return z
end

function add!(a::ZZMPolyRingElem, b::ZZMPolyRingElem, c::ZZMPolyRingElem)
  ccall((:fmpz_mpoly_add, libflint), Nothing,
        (Ref{ZZMPolyRingElem}, Ref{ZZMPolyRingElem},
         Ref{ZZMPolyRingElem}, Ref{ZZMPolyRing}), a, b, c, a.parent)
  return a
end

function mul!(a::ZZMPolyRingElem, b::ZZMPolyRingElem, c::ZZMPolyRingElem)
  ccall((:fmpz_mpoly_mul, libflint), Nothing,
        (Ref{ZZMPolyRingElem}, Ref{ZZMPolyRingElem},
         Ref{ZZMPolyRingElem}, Ref{ZZMPolyRing}), a, b, c, a.parent)
  return a
end

# Set the n-th coefficient of a to c. If zero coefficients are inserted, they
# must be removed with combine_like_terms!
function setcoeff!(a::ZZMPolyRingElem, n::Int, c::ZZRingElem)
  if n > length(a)
    ccall((:fmpz_mpoly_resize, libflint), Nothing,
          (Ref{ZZMPolyRingElem}, Int, Ref{ZZMPolyRing}), a, n, a.parent)
  end
  ccall((:fmpz_mpoly_set_term_coeff_fmpz, libflint), Nothing,
        (Ref{ZZMPolyRingElem}, Int, Ref{ZZRingElem}, Ref{ZZMPolyRing}),
        a, n - 1, c, a.parent)
  return a
end

# Set the i-th coefficient of a to c. If zero coefficients are inserted, they
# must be removed with combine_like_terms!
setcoeff!(a::ZZMPolyRingElem, i::Int, c::Integer) = setcoeff!(a, i, ZZRingElem(c))

# Remove zero terms and combine adjacent terms if they have the same monomial
# no sorting is performed
function combine_like_terms!(a::ZZMPolyRingElem)
  ccall((:fmpz_mpoly_combine_like_terms, libflint), Nothing,
        (Ref{ZZMPolyRingElem}, Ref{ZZMPolyRing}), a, a.parent)
  return a
end

###############################################################################
#
#   Manipulating terms and monomials
#
###############################################################################

function exponent_vector_fits(::Type{Int}, a::ZZMPolyRingElem, i::Int)
  b = ccall((:fmpz_mpoly_term_exp_fits_ui, libflint), Cint,
            (Ref{ZZMPolyRingElem}, Int, Ref{ZZMPolyRing}),
            a, i - 1, a.parent)
  return Bool(b)
end

function exponent_vector_fits(::Type{UInt}, a::ZZMPolyRingElem, i::Int)
  b = ccall((:fmpz_mpoly_term_exp_fits_si, libflint), Cint,
            (Ref{ZZMPolyRingElem}, Int, Ref{ZZMPolyRing}),
            a, i - 1, a.parent)
  return Bool(b)
end

function exponent_vector!(z::Vector{Int}, a::ZZMPolyRingElem, i::Int)
  ccall((:fmpz_mpoly_get_term_exp_si, libflint), Nothing,
        (Ptr{Int}, Ref{ZZMPolyRingElem}, Int, Ref{ZZMPolyRing}),
        z, a, i - 1, parent(a))
  return z
end

function exponent_vector!(z::Vector{UInt}, a::ZZMPolyRingElem, i::Int)
  ccall((:fmpz_mpoly_get_term_exp_ui, libflint), Nothing,
        (Ptr{UInt}, Ref{ZZMPolyRingElem}, Int, Ref{ZZMPolyRing}),
        z, a, i - 1, parent(a))
  return z
end

function exponent_vector!(z::Vector{ZZRingElem}, a::ZZMPolyRingElem, i::Int)
  ccall((:fmpz_mpoly_get_term_exp_fmpz, libflint), Nothing,
        (Ptr{Ref{ZZRingElem}}, Ref{ZZMPolyRingElem}, Int, Ref{ZZMPolyRing}),
        z, a, i - 1, parent(a))
  return z
end

# Return a generator for exponent vectors of $a$
function exponent_vectors_fmpz(a::ZZMPolyRingElem)
  return (exponent_vector_fmpz(a, i) for i in 1:length(a))
end

# Set exponent of n-th term to given vector of UInt's
# No sort is performed, so this is unsafe. These are promoted to ZZRingElem's if
# they don't fit into 31/63 bits
function set_exponent_vector!(a::ZZMPolyRingElem, n::Int, exps::Vector{UInt})
  if n > length(a)
    ccall((:fmpz_mpoly_resize, libflint), Nothing,
          (Ref{ZZMPolyRingElem}, Int, Ref{ZZMPolyRing}), a, n, a.parent)
  end
  ccall((:fmpz_mpoly_set_term_exp_ui, libflint), Nothing,
        (Ref{ZZMPolyRingElem}, Int, Ptr{UInt}, Ref{ZZMPolyRing}),
        a, n - 1, exps, parent(a))
  return a
end

# Set exponent of n-th term to given vector of Int's
# No sort is performed, so this is unsafe. The Int's must be positive, but
# no check is performed
function set_exponent_vector!(a::ZZMPolyRingElem, n::Int, exps::Vector{Int})
  if n > length(a)
    ccall((:fmpz_mpoly_resize, libflint), Nothing,
          (Ref{ZZMPolyRingElem}, Int, Ref{ZZMPolyRing}), a, n, a.parent)
  end
  ccall((:fmpz_mpoly_set_term_exp_ui, libflint), Nothing,
        (Ref{ZZMPolyRingElem}, Int, Ptr{Int}, Ref{ZZMPolyRing}),
        a, n - 1, exps, parent(a))
  return a
end

# Set exponent of n-th term to given vector of ZZRingElem's
# No sort is performed, so this is unsafe
function set_exponent_vector!(a::ZZMPolyRingElem, n::Int, exps::Vector{ZZRingElem})
  if n > length(a)
    ccall((:fmpz_mpoly_resize, libflint), Nothing,
          (Ref{ZZMPolyRingElem}, Int, Ref{ZZMPolyRing}), a, n, a.parent)
    return a
  end
  @GC.preserve exps ccall((:fmpz_mpoly_set_term_exp_fmpz, libflint), Nothing,
                          (Ref{ZZMPolyRingElem}, Int, Ptr{ZZRingElem}, Ref{ZZMPolyRing}),
                          a, n - 1, exps, parent(a))
  return a
end

# Return j-th coordinate of i-th exponent vector
function exponent(a::ZZMPolyRingElem, i::Int, j::Int)
  (j < 1 || j > nvars(parent(a))) && error("Invalid variable index")
  return ccall((:fmpz_mpoly_get_term_var_exp_ui, libflint), Int,
               (Ref{ZZMPolyRingElem}, Int, Int, Ref{ZZMPolyRing}),
               a, i - 1, j - 1, a.parent)
end

# Return the coefficient of the term with the given exponent vector
# Return zero if there is no such term
function coeff(a::ZZMPolyRingElem, exps::Vector{UInt})
  z = ZZRingElem()
  ccall((:fmpz_mpoly_get_coeff_fmpz_ui, libflint), Nothing,
        (Ref{ZZRingElem}, Ref{ZZMPolyRingElem}, Ptr{UInt}, Ref{ZZMPolyRing}),
        z, a, exps, parent(a))
  return z
end

# Return the coefficient of the term with the given exponent vector
# Return zero if there is no such term
function coeff(a::ZZMPolyRingElem, exps::Vector{Int})
  z = ZZRingElem()
  ccall((:fmpz_mpoly_get_coeff_fmpz_ui, libflint), Nothing,
        (Ref{ZZRingElem}, Ref{ZZMPolyRingElem}, Ptr{Int}, Ref{ZZMPolyRing}),
        z, a, exps, parent(a))
  return z
end

# Set the coefficient of the term with the given exponent vector to the
# given ZZRingElem. Removal of a zero term is performed.
function setcoeff!(a::ZZMPolyRingElem, exps::Vector{UInt}, b::ZZRingElem)
  ccall((:fmpz_mpoly_set_coeff_fmpz_ui, libflint), Nothing,
        (Ref{ZZMPolyRingElem}, Ref{ZZRingElem}, Ptr{UInt}, Ref{ZZMPolyRing}),
        a, b, exps, parent(a))
  return a
end

# Set the coefficient of the term with the given exponent vector to the
# given ZZRingElem. Removal of a zero term is performed.
function setcoeff!(a::ZZMPolyRingElem, exps::Vector{Int}, b::ZZRingElem)
  ccall((:fmpz_mpoly_set_coeff_fmpz_ui, libflint), Nothing,
        (Ref{ZZMPolyRingElem}, Ref{ZZRingElem}, Ptr{Int}, Ref{ZZMPolyRing}),
        a, b, exps, parent(a))
  return a
end

# Set the coefficient of the term with the given exponent vector to the
setcoeff!(a::ZZMPolyRingElem, exps::Vector{Int}, b::Integer) =
setcoeff!(a, exps, ZZRingElem(b))

# Sort the terms according to the ordering. This is only needed if unsafe
# functions such as those above have been called and terms have been inserted
# out of order. Note that like terms are not combined and zeros are not
# removed. For that, call combine_like_terms!
function sort_terms!(a::ZZMPolyRingElem)
  ccall((:fmpz_mpoly_sort_terms, libflint), Nothing,
        (Ref{ZZMPolyRingElem}, Ref{ZZMPolyRing}), a, a.parent)
  return a
end

# Return the i-th term of the polynomial, as a polynomial
function term(a::ZZMPolyRingElem, i::Int)
  z = parent(a)()
  ccall((:fmpz_mpoly_get_term, libflint), Nothing,
        (Ref{ZZMPolyRingElem}, Ref{ZZMPolyRingElem}, Int, Ref{ZZMPolyRing}),
        z, a, i - 1, a.parent)
  return z
end

# Return the i-th monomial of the polynomial, as a polynomial
function monomial(a::ZZMPolyRingElem, i::Int)
  z = parent(a)()
  ccall((:fmpz_mpoly_get_term_monomial, libflint), Nothing,
        (Ref{ZZMPolyRingElem}, Ref{ZZMPolyRingElem}, Int, Ref{ZZMPolyRing}),
        z, a, i - 1, a.parent)
  return z
end

# Sets the given polynomial m to the i-th monomial of the polynomial
function monomial!(m::ZZMPolyRingElem, a::ZZMPolyRingElem, i::Int)
  ccall((:fmpz_mpoly_get_term_monomial, libflint), Nothing,
        (Ref{ZZMPolyRingElem}, Ref{ZZMPolyRingElem}, Int, Ref{ZZMPolyRing}),
        m, a, i - 1, a.parent)
  return m
end

###############################################################################
#
#   Promotion rules
#
###############################################################################

promote_rule(::Type{ZZMPolyRingElem}, ::Type{V}) where {V <: Integer} = ZZMPolyRingElem

promote_rule(::Type{ZZMPolyRingElem}, ::Type{ZZRingElem}) = ZZMPolyRingElem

###############################################################################
#
#   Build context
#
###############################################################################

function _push_term!(z::ZZMPolyRingElem, c::ZZRingElem, exp::Vector{Int})
  ccall((:fmpz_mpoly_push_term_fmpz_ui, libflint), Nothing,
        (Ref{ZZMPolyRingElem}, Ref{ZZRingElem}, Ptr{UInt}, Ref{ZZMPolyRing}),
        z, c, exp, parent(z))
  return z
end

function _push_term!(z::ZZMPolyRingElem, c::Int, exp::Vector{Int})
  ccall((:fmpz_mpoly_push_term_si_ui, libflint), Nothing,
        (Ref{ZZMPolyRingElem}, Int, Ptr{UInt}, Ref{ZZMPolyRing}),
        z, c, exp, parent(z))
  return z
end

function _push_term!(z::ZZMPolyRingElem, c::UInt, exp::Vector{Int})
  ccall((:fmpz_mpoly_push_term_ui_ui, libflint), Nothing,
        (Ref{ZZMPolyRingElem}, UInt, Ptr{UInt}, Ref{ZZMPolyRing}),
        z, c, exp, parent(z))
  return z
end

function push_term!(M::MPolyBuildCtx{ZZMPolyRingElem},
    c::Union{ZZRingElem, Int, UInt}, expv::Vector{Int})
  if length(expv) != nvars(parent(M.poly))
    error("length of exponent vector should match the number of variables")
  end
  _push_term!(M.poly, c, expv)
  return M
end

function push_term!(M::MPolyBuildCtx{ZZMPolyRingElem},
    c::RingElement, expv::Vector{Int})
  push_term!(M, ZZ(c), expv)
  return M
end

function finish(M::MPolyBuildCtx{ZZMPolyRingElem})
  res = M.poly
  R = parent(res)
  M.poly = zero(R)
  sort_terms!(res)
  combine_like_terms!(res)
  return res
end

###############################################################################
#
#   Parent object call overload
#
###############################################################################

function (R::ZZMPolyRing)()
  z = ZZMPolyRingElem(R)
  return z
end

function (R::ZZMPolyRing)(b::IntegerUnion)
  z = ZZMPolyRingElem(R, b)
  return z
end

function (R::ZZMPolyRing)(a::ZZMPolyRingElem)
  parent(a) != R && error("Unable to coerce polynomial")
  return a
end

# Create poly with given array of coefficients and array of exponent vectors (sorting is performed)
function (R::ZZMPolyRing)(a::Vector{ZZRingElem}, b::Vector{Vector{T}}) where {T <: Union{ZZRingElem, UInt}}
  length(a) != length(b) && error("Coefficient and exponent vector must have the same length")

  for i in 1:length(b)
    length(b[i]) != nvars(R) && error("Exponent vector $i has length $(length(b[i])) (expected $(nvars(R))")
  end

  z = ZZMPolyRingElem(R, a, b)
  return z
end

# Create poly with given array of coefficients and array of exponent vectors (sorting is performed)
function (R::ZZMPolyRing)(a::Vector{ZZRingElem}, b::Vector{Vector{Int}})
  length(a) != length(b) && error("Coefficient and exponent vector must have the same length")

  for i in 1:length(b)
    length(b[i]) != nvars(R) && error("Exponent vector $i has length $(length(b[i])) (expected $(nvars(R)))")
  end

  z = ZZMPolyRingElem(R, a, b)
  return z
end

# Create poly with given array of coefficients and array of exponent vectors (sorting is performed)
function (R::ZZMPolyRing)(a::Vector{Any}, b::Vector{Vector{T}}) where T
  n = nvars(R)
  length(a) != length(b) && error("Coefficient and exponent vector must have the same length")
  newa = map(ZZ, a)
  newb = map(x -> map(ZZ, x), b)
  newaa = convert(Vector{ZZRingElem}, newa)
  newbb = convert(Vector{Vector{ZZRingElem}}, newb)

  for i in 1:length(newbb)
    length(newbb[i]) != n && error("Exponent vector $i has length $(length(newbb[i])) (expected $(nvars(R)))")
  end

  return R(newaa, newbb)
end
