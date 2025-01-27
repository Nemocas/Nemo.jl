###############################################################################
#
#   fqPolyRepMPolyRingElem.jl : Flint multivariate polynomials over fqPolyRepFieldElem
#
###############################################################################

###############################################################################
#
#   Data type and parent object methods
#
###############################################################################

parent_type(::Type{fqPolyRepMPolyRingElem}) = fqPolyRepMPolyRing

elem_type(::Type{fqPolyRepMPolyRing}) = fqPolyRepMPolyRingElem

mpoly_type(::Type{fqPolyRepFieldElem}) = fqPolyRepMPolyRingElem

symbols(a::fqPolyRepMPolyRing) = a.S

parent(a::fqPolyRepMPolyRingElem) = a.parent

number_of_variables(a::fqPolyRepMPolyRing) = a.nvars

base_ring(a::fqPolyRepMPolyRing) = a.base_ring

function internal_ordering(a::fqPolyRepMPolyRing)
  b = a.ord
  #   b = @ccall libflint.fq_nmod_mpoly_ctx_ord(a::Ref{fqPolyRepMPolyRing})::Cint
  return flint_orderings[b + 1]
end

function gens(R::fqPolyRepMPolyRing)
  A = Vector{fqPolyRepMPolyRingElem}(undef, R.nvars)
  for i = 1:R.nvars
    z = R()
    @ccall libflint.fq_nmod_mpoly_gen(z::Ref{fqPolyRepMPolyRingElem}, (i - 1)::Int, R::Ref{fqPolyRepMPolyRing})::Nothing
    A[i] = z
  end
  return A
end

function gen(R::fqPolyRepMPolyRing, i::Int)
  n = nvars(R)
  !(1 <= i <= n) && error("Index must be between 1 and $n")
  z = R()
  @ccall libflint.fq_nmod_mpoly_gen(z::Ref{fqPolyRepMPolyRingElem}, (i - 1)::Int, R::Ref{fqPolyRepMPolyRing})::Nothing
  return z
end

function is_gen(a::fqPolyRepMPolyRingElem, i::Int)
  n = nvars(parent(a))
  !(1 <= i <= n) && error("Index must be between 1 and $n")
  return Bool(@ccall libflint.fq_nmod_mpoly_is_gen(a::Ref{fqPolyRepMPolyRingElem}, (i - 1)::Int, a.parent::Ref{fqPolyRepMPolyRing})::Cint)
end

function is_gen(a::fqPolyRepMPolyRingElem)
  return Bool(@ccall libflint.fq_nmod_mpoly_is_gen(a::Ref{fqPolyRepMPolyRingElem}, (-1)::Int, a.parent::Ref{fqPolyRepMPolyRing})::Cint)
end

function deepcopy_internal(a::fqPolyRepMPolyRingElem, dict::IdDict)
  z = parent(a)()
  @ccall libflint.fq_nmod_mpoly_set(z::Ref{fqPolyRepMPolyRingElem}, a::Ref{fqPolyRepMPolyRingElem}, a.parent::Ref{fqPolyRepMPolyRing})::Nothing
  return z
end

length(a::fqPolyRepMPolyRingElem) = a.length

one(R::fqPolyRepMPolyRing) = one!(R())

zero(R::fqPolyRepMPolyRing) = zero!(R())

function isone(a::fqPolyRepMPolyRingElem)
  b = @ccall libflint.fq_nmod_mpoly_is_one(a::Ref{fqPolyRepMPolyRingElem}, a.parent::Ref{fqPolyRepMPolyRing})::Cint
  return Bool(b)
end

function iszero(a::fqPolyRepMPolyRingElem)
  b = @ccall libflint.fq_nmod_mpoly_is_zero(a::Ref{fqPolyRepMPolyRingElem}, a.parent::Ref{fqPolyRepMPolyRing})::Cint
  return Bool(b)
end

function is_monomial(a::fqPolyRepMPolyRingElem)
  return length(a) == 1 && isone(coeff(a, 1))
end

function is_term(a::fqPolyRepMPolyRingElem)
  return length(a) == 1
end

function is_constant(a::fqPolyRepMPolyRingElem)
  b = @ccall libflint.fq_nmod_mpoly_is_fq_nmod(a::Ref{fqPolyRepMPolyRingElem}, parent(a)::Ref{fqPolyRepMPolyRing})::Cint
  return Bool(b)
end

function fit!(a::fqPolyRepMPolyRingElem, n::Int)
  # needs to exist for the MPoly interface
  return nothing
end

################################################################################
#
#  Getting coefficients
#
################################################################################

function coeff(a::fqPolyRepMPolyRingElem, i::Int)
  n = length(a)
  !(1 <= i <= n) && error("Index must be between 1 and $(length(a))")
  z = base_ring(parent(a))()
  @ccall libflint.fq_nmod_mpoly_get_term_coeff_fq_nmod(z::Ref{fqPolyRepFieldElem}, a::Ref{fqPolyRepMPolyRingElem}, (i - 1)::Int, a.parent::Ref{fqPolyRepMPolyRing})::Nothing
  return z
end

function coeff(a::fqPolyRepMPolyRingElem, b::fqPolyRepMPolyRingElem)
  check_parent(a, b)
  !isone(length(b)) && error("Second argument must be a monomial")
  z = base_ring(parent(a))()
  @ccall libflint.fq_nmod_mpoly_get_coeff_fq_nmod_monomial(z::Ref{fqPolyRepFieldElem}, a::Ref{fqPolyRepMPolyRingElem}, b::Ref{fqPolyRepMPolyRingElem}, parent(a)::Ref{fqPolyRepMPolyRing})::UInt
  return z
end

function trailing_coefficient(p::fqPolyRepMPolyRingElem)
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
function degree(a::fqPolyRepMPolyRingElem, i::Int)
  n = nvars(parent(a))
  !(1 <= i <= n) && error("Index must be between 1 and $n")
  if degrees_fit_int(a)
    d = @ccall libflint.fq_nmod_mpoly_degree_si(a::Ref{fqPolyRepMPolyRingElem}, (i - 1)::Int, a.parent::Ref{fqPolyRepMPolyRing})::Int
    return d
  else
    return Int(degree_fmpz(a, i))
  end
end

# Degree in the i-th variable as an ZZRingElem
function degree_fmpz(a::fqPolyRepMPolyRingElem, i::Int)
  n = nvars(parent(a))
  !(1 <= i <= n) && error("Index must be between 1 and $n")
  d = ZZRingElem()
  @ccall libflint.fq_nmod_mpoly_degree_fmpz(d::Ref{ZZRingElem}, a::Ref{fqPolyRepMPolyRingElem}, (i - 1)::Int, a.parent::Ref{fqPolyRepMPolyRing})::Nothing
  return d
end

# Return true if degrees fit into an Int
function degrees_fit_int(a::fqPolyRepMPolyRingElem)
  b = @ccall libflint.fq_nmod_mpoly_degrees_fit_si(a::Ref{fqPolyRepMPolyRingElem}, a.parent::Ref{fqPolyRepMPolyRing})::Cint
  return Bool(b)
end

# Return an array of the max degrees in each variable
function degrees(a::fqPolyRepMPolyRingElem)
  if !degrees_fit_int(a)
    throw(OverflowError("degrees of polynomial do not fit into Int"))
  end
  degs = Vector{Int}(undef, nvars(parent(a)))
  @ccall libflint.fq_nmod_mpoly_degrees_si(degs::Ptr{Int}, a::Ref{fqPolyRepMPolyRingElem}, a.parent::Ref{fqPolyRepMPolyRing})::Nothing
  return degs
end

# Return an array of the max degrees as fmpzs in each variable
function degrees_fmpz(a::fqPolyRepMPolyRingElem)
  n = nvars(parent(a))
  degs = Vector{ZZRingElem}(undef, n)
  for i in 1:n
    degs[i] = ZZRingElem()
  end
  @ccall libflint.fq_nmod_mpoly_degrees_fmpz(degs::Ptr{Ref{ZZRingElem}}, a::Ref{fqPolyRepMPolyRingElem}, a.parent::Ref{fqPolyRepMPolyRing})::Nothing
  return degs
end

# Return true if degree fits into an Int
function total_degree_fits_int(a::fqPolyRepMPolyRingElem)
  b = @ccall libflint.fq_nmod_mpoly_total_degree_fits_si(a::Ref{fqPolyRepMPolyRingElem}, a.parent::Ref{fqPolyRepMPolyRing})::Cint
  return Bool(b)
end

# Total degree as an Int
function total_degree(a::fqPolyRepMPolyRingElem)
  if !total_degree_fits_int(a)
    throw(OverflowError("Total degree of polynomial does not fit into Int"))
  end
  d = @ccall libflint.fq_nmod_mpoly_total_degree_si(a::Ref{fqPolyRepMPolyRingElem}, a.parent::Ref{fqPolyRepMPolyRing})::Int
  return d
end

# Total degree as an ZZRingElem
function total_degree_fmpz(a::fqPolyRepMPolyRingElem)
  d = ZZRingElem()
  @ccall libflint.fq_nmod_mpoly_total_degree_fmpz(d::Ref{ZZRingElem}, a::Ref{fqPolyRepMPolyRingElem}, a.parent::Ref{fqPolyRepMPolyRing})::Nothing
  return d
end

###############################################################################
#
#   Multivariable coefficient polynomials
#
###############################################################################

function coeff(a::fqPolyRepMPolyRingElem, vars::Vector{Int}, exps::Vector{Int})
  unique(vars) != vars && error("Variables not unique")
  length(vars) != length(exps) &&
  error("Number of variables does not match number of exponents")
  vars = [UInt(i) - 1 for i in vars]
  for i = 1:length(vars)
    if vars[i] < 0 || vars[i] >= nvars(parent(a))
      error("Variable index not in range")
    end
    if exps[i] < 0
      error("Exponent cannot be negative")
    end
  end
  z = parent(a)()
  @ccall libflint.fq_nmod_mpoly_get_coeff_vars_ui(z::Ref{fqPolyRepMPolyRingElem}, a::Ref{fqPolyRepMPolyRingElem}, vars::Ptr{Int}, exps::Ptr{Int}, length(vars)::Int, a.parent::Ref{fqPolyRepMPolyRing})::Nothing
  return z
end

###############################################################################
#
#   Basic arithmetic
#
###############################################################################

-(a::fqPolyRepMPolyRingElem) = neg!(parent(a)(), a)

function +(a::fqPolyRepMPolyRingElem, b::fqPolyRepMPolyRingElem)
  check_parent(a, b)
  z = parent(a)()
  @ccall libflint.fq_nmod_mpoly_add(z::Ref{fqPolyRepMPolyRingElem}, a::Ref{fqPolyRepMPolyRingElem}, b::Ref{fqPolyRepMPolyRingElem}, a.parent::Ref{fqPolyRepMPolyRing})::Nothing
  return z
end

function -(a::fqPolyRepMPolyRingElem, b::fqPolyRepMPolyRingElem)
  check_parent(a, b)
  z = parent(a)()
  @ccall libflint.fq_nmod_mpoly_sub(z::Ref{fqPolyRepMPolyRingElem}, a::Ref{fqPolyRepMPolyRingElem}, b::Ref{fqPolyRepMPolyRingElem}, a.parent::Ref{fqPolyRepMPolyRing})::Nothing
  return z
end

function *(a::fqPolyRepMPolyRingElem, b::fqPolyRepMPolyRingElem)
  check_parent(a, b)
  z = parent(a)()
  @ccall libflint.fq_nmod_mpoly_mul(z::Ref{fqPolyRepMPolyRingElem}, a::Ref{fqPolyRepMPolyRingElem}, b::Ref{fqPolyRepMPolyRingElem}, a.parent::Ref{fqPolyRepMPolyRing})::Nothing
  return z
end

###############################################################################
#
#   Ad hoc arithmetic
#
###############################################################################

+(a::fqPolyRepMPolyRingElem, b::Integer) = a + base_ring(parent(a))(b)

+(a::Integer, b::fqPolyRepMPolyRingElem) = b + a

-(a::fqPolyRepMPolyRingElem, b::Integer) = a - base_ring(parent(a))(b)

-(a::Integer, b::fqPolyRepMPolyRingElem) = base_ring(parent(b))(a) - b

*(a::fqPolyRepMPolyRingElem, b::Integer) = a*base_ring(parent(a))(b)

*(a::Integer, b::fqPolyRepMPolyRingElem) = b*a

divexact(a::fqPolyRepMPolyRingElem, b::Integer; check::Bool=true) = divexact(a, base_ring(parent(a))(b); check=check)

###############################################################################
#
#   Powering
#
###############################################################################

function ^(a::fqPolyRepMPolyRingElem, b::Int)
  b < 0 && throw(DomainError(b, "Exponent must be non-negative"))
  z = parent(a)()
  r = @ccall libflint.fq_nmod_mpoly_pow_ui(z::Ref{fqPolyRepMPolyRingElem}, a::Ref{fqPolyRepMPolyRingElem}, UInt(b)::UInt, parent(a)::Ref{fqPolyRepMPolyRing})::Cint
  iszero(r) && error("Unable to compute power")
  return z
end

function ^(a::fqPolyRepMPolyRingElem, b::ZZRingElem)
  b < 0 && throw(DomainError(b, "Exponent must be non-negative"))
  z = parent(a)()
  r = @ccall libflint.fq_nmod_mpoly_pow_fmpz(z::Ref{fqPolyRepMPolyRingElem}, a::Ref{fqPolyRepMPolyRingElem}, b::Ref{ZZRingElem}, parent(a)::Ref{fqPolyRepMPolyRing})::Cint
  iszero(r) && error("Unable to compute power")
  return z
end

################################################################################
#
#   GCD
#
################################################################################

function gcd(a::fqPolyRepMPolyRingElem, b::fqPolyRepMPolyRingElem)
  check_parent(a, b)
  z = parent(a)()
  r = @ccall libflint.fq_nmod_mpoly_gcd(z::Ref{fqPolyRepMPolyRingElem}, a::Ref{fqPolyRepMPolyRingElem}, b::Ref{fqPolyRepMPolyRingElem}, a.parent::Ref{fqPolyRepMPolyRing})::Cint
  r == 0 && error("Unable to compute gcd")
  return z
end

function gcd_with_cofactors(a::fqPolyRepMPolyRingElem, b::fqPolyRepMPolyRingElem)
  check_parent(a, b)
  z = parent(a)()
  abar = parent(a)()
  bbar = parent(a)()
  r = @ccall libflint.fq_nmod_mpoly_gcd_cofactors(z::Ref{fqPolyRepMPolyRingElem}, abar::Ref{fqPolyRepMPolyRingElem}, bbar::Ref{fqPolyRepMPolyRingElem}, a::Ref{fqPolyRepMPolyRingElem}, b::Ref{fqPolyRepMPolyRingElem}, a.parent::Ref{fqPolyRepMPolyRing})::Cint
  r == 0 && error("Unable to compute gcd")
  return z, abar, bbar
end

################################################################################
#
#   Factorization and Square Root
#
################################################################################

function (::Type{Fac{fqPolyRepMPolyRingElem}})(fac::fq_nmod_mpoly_factor, preserve_input::Bool = false)
  R = fac.parent
  F = Fac{fqPolyRepMPolyRingElem}()
  for i in 0:fac.num-1
    f = R()
    if preserve_input
      @ccall libflint.fq_nmod_mpoly_factor_get_base(f::Ref{fqPolyRepMPolyRingElem}, fac::Ref{fq_nmod_mpoly_factor}, i::Int, R::Ref{fqPolyRepMPolyRing})::Nothing
    else
      @ccall libflint.fq_nmod_mpoly_factor_swap_base(f::Ref{fqPolyRepMPolyRingElem}, fac::Ref{fq_nmod_mpoly_factor}, i::Int, R::Ref{fqPolyRepMPolyRing})::Nothing
    end
    F.fac[f] = @ccall libflint.fq_nmod_mpoly_factor_get_exp_si(fac::Ref{fq_nmod_mpoly_factor}, i::Int, R::Ref{fqPolyRepMPolyRing})::Int
  end
  c = base_ring(R)()
  @ccall libflint.fq_nmod_mpoly_factor_get_constant_fq_nmod(c::Ref{fqPolyRepFieldElem}, fac::Ref{fq_nmod_mpoly_factor})::Nothing
  F.unit = R(c)
  return F
end

function factor(a::fqPolyRepMPolyRingElem)
  iszero(a) && throw(ArgumentError("Argument must be non-zero"))
  R = parent(a)
  fac = fq_nmod_mpoly_factor(R)
  ok = @ccall libflint.fq_nmod_mpoly_factor(fac::Ref{fq_nmod_mpoly_factor}, a::Ref{fqPolyRepMPolyRingElem}, R::Ref{fqPolyRepMPolyRing})::Cint
  ok == 0 && error("unable to compute factorization")
  return Fac{fqPolyRepMPolyRingElem}(fac, false)
end

function factor_squarefree(a::fqPolyRepMPolyRingElem)
  iszero(a) && throw(ArgumentError("Argument must be non-zero"))
  R = parent(a)
  fac = fq_nmod_mpoly_factor(R)
  ok = @ccall libflint.fq_nmod_mpoly_factor_squarefree(fac::Ref{fq_nmod_mpoly_factor}, a::Ref{fqPolyRepMPolyRingElem}, R::Ref{fqPolyRepMPolyRing})::Cint
  ok == 0 && error("unable to compute factorization")
  return Fac{fqPolyRepMPolyRingElem}(fac, false)
end


function sqrt(a::fqPolyRepMPolyRingElem; check::Bool=true)
  (flag, q) = is_square_with_sqrt(a)
  check && !flag && error("Not a square")
  return q
end

function is_square(a::fqPolyRepMPolyRingElem)
  return Bool(@ccall libflint.fq_nmod_mpoly_is_square(a::Ref{fqPolyRepMPolyRingElem}, a.parent::Ref{fqPolyRepMPolyRing})::Cint)
end

function is_square_with_sqrt(a::fqPolyRepMPolyRingElem)
  q = parent(a)()
  flag = @ccall libflint.fq_nmod_mpoly_sqrt(q::Ref{fqPolyRepMPolyRingElem}, a::Ref{fqPolyRepMPolyRingElem}, a.parent::Ref{fqPolyRepMPolyRing})::Cint
  return (Bool(flag), q)
end

###############################################################################
#
#   Comparison
#
###############################################################################

function ==(a::fqPolyRepMPolyRingElem, b::fqPolyRepMPolyRingElem)
  check_parent(a, b)
  return Bool(@ccall libflint.fq_nmod_mpoly_equal(a::Ref{fqPolyRepMPolyRingElem}, b::Ref{fqPolyRepMPolyRingElem}, a.parent::Ref{fqPolyRepMPolyRing})::Cint)
end

function Base.isless(a::fqPolyRepMPolyRingElem, b::fqPolyRepMPolyRingElem)
  (!is_monomial(a) || !is_monomial(b)) && error("Not monomials in comparison")
  return (@ccall libflint.fq_nmod_mpoly_cmp(a::Ref{fqPolyRepMPolyRingElem}, b::Ref{fqPolyRepMPolyRingElem}, a.parent::Ref{fqPolyRepMPolyRing})::Cint) < 0
end

###############################################################################
#
#   Ad hoc comparison
#
###############################################################################

function ==(a::fqPolyRepMPolyRingElem, b::fqPolyRepFieldElem)
  return Bool(@ccall libflint.fq_nmod_mpoly_equal_fq_nmod(a::Ref{fqPolyRepMPolyRingElem}, b::Ref{fqPolyRepFieldElem}, a.parent::Ref{fqPolyRepMPolyRing})::Cint)
end

==(a::fqPolyRepFieldElem, b::fqPolyRepMPolyRingElem) = b == a

==(a::fqPolyRepMPolyRingElem, b::Integer) = a == base_ring(parent(a))(b)

==(a::fqPolyRepMPolyRingElem, b::ZZRingElem) = a == base_ring(parent(a))(b)

==(a::Integer, b::fqPolyRepMPolyRingElem) = b == a

==(a::ZZRingElem, b::fqPolyRepMPolyRingElem) = b == a

###############################################################################
#
#   Divisibility
#
###############################################################################

function divides(a::fqPolyRepMPolyRingElem, b::fqPolyRepMPolyRingElem)
  check_parent(a, b)
  if iszero(a)
    return true, zero(parent(a))
  end
  if iszero(b)
    return false, zero(parent(a))
  end
  z = parent(a)()
  d = @ccall libflint.fq_nmod_mpoly_divides(z::Ref{fqPolyRepMPolyRingElem}, a::Ref{fqPolyRepMPolyRingElem}, b::Ref{fqPolyRepMPolyRingElem}, a.parent::Ref{fqPolyRepMPolyRing})::Cint
  return isone(d), z
end

###############################################################################
#
#   Division with remainder
#
###############################################################################

function Base.div(a::fqPolyRepMPolyRingElem, b::fqPolyRepMPolyRingElem)
  check_parent(a, b)
  q = parent(a)()
  @ccall libflint.fq_nmod_mpoly_div(q::Ref{fqPolyRepMPolyRingElem}, a::Ref{fqPolyRepMPolyRingElem}, b::Ref{fqPolyRepMPolyRingElem}, a.parent::Ref{fqPolyRepMPolyRing})::Nothing
  return q
end

function Base.divrem(a::fqPolyRepMPolyRingElem, b::fqPolyRepMPolyRingElem)
  check_parent(a, b)
  q = parent(a)()
  r = parent(a)()
  @ccall libflint.fq_nmod_mpoly_divrem(q::Ref{fqPolyRepMPolyRingElem}, r::Ref{fqPolyRepMPolyRingElem}, a::Ref{fqPolyRepMPolyRingElem}, b::Ref{fqPolyRepMPolyRingElem}, a.parent::Ref{fqPolyRepMPolyRing})::Nothing
  return q, r
end

function Base.divrem(a::fqPolyRepMPolyRingElem, b::Vector{fqPolyRepMPolyRingElem})
  len = length(b)
  if len < 1
    error("need at least one divisor in divrem")
  end
  for i in 1:len
    check_parent(a, b[i])
  end
  q = [parent(a)() for i in 1:len]
  r = parent(a)()
  @ccall libflint.fq_nmod_mpoly_divrem_ideal(q::Ptr{Ref{fqPolyRepMPolyRingElem}}, r::Ref{fqPolyRepMPolyRingElem}, a::Ref{fqPolyRepMPolyRingElem}, b::Ptr{Ref{fqPolyRepMPolyRingElem}}, len::Int, a.parent::Ref{fqPolyRepMPolyRing})::Nothing
  return q, r
end

###############################################################################
#
#   Exact division
#
###############################################################################

function divexact(a::fqPolyRepMPolyRingElem, b::fqPolyRepMPolyRingElem; check::Bool=true)
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

function derivative(a::fqPolyRepMPolyRingElem, i::Int)
  n = nvars(parent(a))
  !(1 <= i <= n) && error("Index must be between 1 and $n")
  z = parent(a)()
  @ccall libflint.fq_nmod_mpoly_derivative(z::Ref{fqPolyRepMPolyRingElem}, a::Ref{fqPolyRepMPolyRingElem}, (i - 1)::Int, parent(a)::Ref{fqPolyRepMPolyRing})::Nothing
  return z
end

###############################################################################
#
#   Evaluation
#
###############################################################################

function evaluate(a::fqPolyRepMPolyRingElem, b::Vector{fqPolyRepFieldElem})
  length(b) != nvars(parent(a)) && error("Vector size incorrect in evaluate")
  z = base_ring(parent(a))()
  @ccall libflint.fq_nmod_mpoly_evaluate_all_fq_nmod(z::Ref{fqPolyRepFieldElem}, a::Ref{fqPolyRepMPolyRingElem}, b::Ptr{Ref{fqPolyRepFieldElem}}, parent(a)::Ref{fqPolyRepMPolyRing})::Nothing
  return z
end

function evaluate(a::fqPolyRepMPolyRingElem, b::Vector{Int})
  length(b) != nvars(parent(a)) && error("Vector size incorrect in evaluate")
  R = base_ring(parent(a))
  b2 = [R(d) for d in b]
  return evaluate(a, b2)
end

function evaluate(a::fqPolyRepMPolyRingElem, b::Vector{T}) where T <: Integer
  length(b) != nvars(parent(a)) && error("Vector size incorrect in evaluate")
  R = base_ring(parent(a))
  b2 = [R(d) for d in b]
  return evaluate(a, b2)
end

function evaluate(a::fqPolyRepMPolyRingElem, b::Vector{ZZRingElem})
  length(b) != nvars(parent(a)) && error("Vector size incorrect in evaluate")
  R = base_ring(parent(a))
  b2 = [R(d) for d in b]
  return evaluate(a, b2)
end

function evaluate(a::fqPolyRepMPolyRingElem, b::Vector{UInt})
  length(b) != nvars(parent(a)) && error("Vector size incorrect in evaluate")
  R = base_ring(parent(a))
  b2 = [R(d) for d in b]
  return evaluate(a, b2)
end

function (a::fqPolyRepMPolyRingElem)()
  error("need at least one value")
end

function (a::fqPolyRepMPolyRingElem)(vals::fqPolyRepFieldElem...)
  length(vals) != nvars(parent(a)) && error("Number of variables does not match number of values")
  return evaluate(a, [vals...])
end

function (a::fqPolyRepMPolyRingElem)(vals::Integer...)
  length(vals) != nvars(parent(a)) && error("Number of variables does not match number of values")
  return evaluate(a, [vals...])
end

function (a::fqPolyRepMPolyRingElem)(vals::Union{NCRingElem, RingElement}...)
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

function evaluate(a::fqPolyRepMPolyRingElem, bs::Vector{fqPolyRepMPolyRingElem})
  R = parent(a)
  S = parent(bs[1])
  @assert base_ring(R) === base_ring(S)

  length(bs) != nvars(R) &&
  error("Number of variables does not match number of values")

  c = S()
  fl = @ccall libflint.fq_nmod_mpoly_compose_fq_nmod_mpoly(c::Ref{fqPolyRepMPolyRingElem}, a::Ref{fqPolyRepMPolyRingElem}, bs::Ptr{Ref{fqPolyRepMPolyRingElem}}, R::Ref{fqPolyRepMPolyRing}, S::Ref{fqPolyRepMPolyRing})::Cint
  fl == 0 && error("Something wrong in evaluation.")
  return c
end

function evaluate(a::fqPolyRepMPolyRingElem, bs::Vector{fqPolyRepPolyRingElem})
  R = parent(a)
  S = parent(bs[1])
  @assert base_ring(R) === base_ring(S)

  length(bs) != nvars(R) &&
  error("Number of variables does not match number of values")

  c = S()
  fl = @ccall libflint.fq_nmod_mpoly_compose_fq_nmod_poly(c::Ref{fqPolyRepPolyRingElem}, a::Ref{fqPolyRepMPolyRingElem}, bs::Ptr{Ref{fqPolyRepPolyRingElem}}, R::Ref{fqPolyRepMPolyRing})::Cint
  fl == 0 && error("Something wrong in evaluation.")
  return c
end

###############################################################################
#
#   Unsafe functions
#
###############################################################################

function zero!(a::fqPolyRepMPolyRingElem)
  @ccall libflint.fq_nmod_mpoly_zero(a::Ref{fqPolyRepMPolyRingElem}, a.parent::Ref{fqPolyRepMPolyRing})::Nothing
  return a
end

function one!(a::fqPolyRepMPolyRingElem)
  @ccall libflint.fq_nmod_mpoly_one(a::Ref{fqPolyRepMPolyRingElem}, a.parent::Ref{fqPolyRepMPolyRing})::Nothing
  return a
end

function neg!(z::fqPolyRepMPolyRingElem, a::fqPolyRepMPolyRingElem)
  @ccall libflint.fq_nmod_mpoly_neg(z::Ref{fqPolyRepMPolyRingElem}, a::Ref{fqPolyRepMPolyRingElem}, a.parent::Ref{fqPolyRepMPolyRing})::Nothing
  return z
end

function add!(a::fqPolyRepMPolyRingElem, b::fqPolyRepMPolyRingElem, c::fqPolyRepMPolyRingElem)
  @ccall libflint.fq_nmod_mpoly_add(a::Ref{fqPolyRepMPolyRingElem}, b::Ref{fqPolyRepMPolyRingElem}, c::Ref{fqPolyRepMPolyRingElem}, a.parent::Ref{fqPolyRepMPolyRing})::Nothing
  return a
end

function mul!(a::fqPolyRepMPolyRingElem, b::fqPolyRepMPolyRingElem, c::fqPolyRepMPolyRingElem)
  @ccall libflint.fq_nmod_mpoly_mul(a::Ref{fqPolyRepMPolyRingElem}, b::Ref{fqPolyRepMPolyRingElem}, c::Ref{fqPolyRepMPolyRingElem}, a.parent::Ref{fqPolyRepMPolyRing})::Nothing
  return a
end

# Set the n-th coefficient of a to c. If zero coefficients are inserted, they
# must be removed with combine_like_terms!
function setcoeff!(a::fqPolyRepMPolyRingElem, n::Int, c::fqPolyRepFieldElem)
  if n > length(a)
    @ccall libflint.fq_nmod_mpoly_resize(a::Ref{fqPolyRepMPolyRingElem}, n::Int, a.parent::Ref{fqPolyRepMPolyRing})::Nothing
  end
  @ccall libflint.fq_nmod_mpoly_set_term_coeff_fq_nmod(a::Ref{fqPolyRepMPolyRingElem}, (n - 1)::Int, c::Ref{fqPolyRepFieldElem}, a.parent::Ref{fqPolyRepMPolyRing})::Nothing
  return a
end

# Set the i-th coefficient of a to c. If zero coefficients are inserted, they
# must be removed with combine_like_terms!
setcoeff!(a::fqPolyRepMPolyRingElem, i::Int, c::Integer) = setcoeff!(a, i, base_ring(parent(a))(c))

# Set the i-th coefficient of a to c. If zero coefficients are inserted, they
# must be removed with combine_like_terms!
setcoeff!(a::fqPolyRepMPolyRingElem, i::Int, c::ZZRingElem) = setcoeff!(a, i, base_ring(parent(a))(c))

# Remove zero terms and combine adjacent terms if they have the same monomial
# no sorting is performed
function combine_like_terms!(a::fqPolyRepMPolyRingElem)
  @ccall libflint.fq_nmod_mpoly_combine_like_terms(a::Ref{fqPolyRepMPolyRingElem}, a.parent::Ref{fqPolyRepMPolyRing})::Nothing
  return a
end

###############################################################################
#
#   Manipulating terms and monomials
#
###############################################################################

function exponent_vector_fits(::Type{Int}, a::fqPolyRepMPolyRingElem, i::Int)
  b = @ccall libflint.fq_nmod_mpoly_term_exp_fits_si(a::Ref{fqPolyRepMPolyRingElem}, (i - 1)::Int, a.parent::Ref{fqPolyRepMPolyRing})::Cint
  return Bool(b)
end

function exponent_vector_fits(::Type{UInt}, a::fqPolyRepMPolyRingElem, i::Int)
  b = @ccall libflint.fq_nmod_mpoly_term_exp_fits_ui(a::Ref{fqPolyRepMPolyRingElem}, (i - 1)::Int, a.parent::Ref{fqPolyRepMPolyRing})::Cint
  return Bool(b)
end

function exponent_vector!(z::Vector{Int}, a::fqPolyRepMPolyRingElem, i::Int)
  @ccall libflint.fq_nmod_mpoly_get_term_exp_si(z::Ptr{Int}, a::Ref{fqPolyRepMPolyRingElem}, (i - 1)::Int, parent(a)::Ref{fqPolyRepMPolyRing})::Nothing
  return z
end

function exponent_vector!(z::Vector{UInt}, a::fqPolyRepMPolyRingElem, i::Int)
  @ccall libflint.fq_nmod_mpoly_get_term_exp_ui(z::Ptr{UInt}, a::Ref{fqPolyRepMPolyRingElem}, (i - 1)::Int, parent(a)::Ref{fqPolyRepMPolyRing})::Nothing
  return z
end

function exponent_vector!(z::Vector{ZZRingElem}, a::fqPolyRepMPolyRingElem, i::Int)
  @ccall libflint.fq_nmod_mpoly_get_term_exp_fmpz(z::Ptr{Ref{ZZRingElem}}, a::Ref{fqPolyRepMPolyRingElem}, (i - 1)::Int, parent(a)::Ref{fqPolyRepMPolyRing})::Nothing
  return z
end

# Return a generator for exponent vectors of $a$
function exponent_vectors_fmpz(a::fqPolyRepMPolyRingElem)
  return (exponent_vector_fmpz(a, i) for i in 1:length(a))
end

# Set exponent of n-th term to given vector of UInt's
# No sort is performed, so this is unsafe.
function set_exponent_vector!(a::fqPolyRepMPolyRingElem, n::Int, exps::Vector{UInt})
  if n > length(a)
    @ccall libflint.fq_nmod_mpoly_resize(a::Ref{fqPolyRepMPolyRingElem}, n::Int, a.parent::Ref{fqPolyRepMPolyRing})::Nothing
  end
  @ccall libflint.fq_nmod_mpoly_set_term_exp_ui(a::Ref{fqPolyRepMPolyRingElem}, (n - 1)::Int, exps::Ptr{UInt}, parent(a)::Ref{fqPolyRepMPolyRing})::Nothing
  return a
end

# Set exponent of n-th term to given vector of Int's
# No sort is performed, so this is unsafe. The Int's must be positive, but
# no check is performed
function set_exponent_vector!(a::fqPolyRepMPolyRingElem, n::Int, exps::Vector{Int})
  if n > length(a)
    @ccall libflint.fq_nmod_mpoly_resize(a::Ref{fqPolyRepMPolyRingElem}, n::Int, a.parent::Ref{fqPolyRepMPolyRing})::Nothing
  end
  @ccall libflint.fq_nmod_mpoly_set_term_exp_ui(a::Ref{fqPolyRepMPolyRingElem}, (n - 1)::Int, exps::Ptr{Int}, parent(a)::Ref{fqPolyRepMPolyRing})::Nothing
  return a
end

# Set exponent of n-th term to given vector of ZZRingElem's
# No sort is performed, so this is unsafe
function set_exponent_vector!(a::fqPolyRepMPolyRingElem, n::Int, exps::Vector{ZZRingElem})
  if n > length(a)
    @ccall libflint.fq_nmod_mpoly_resize(a::Ref{fqPolyRepMPolyRingElem}, n::Int, a.parent::Ref{fqPolyRepMPolyRing})::Nothing
  end
  @ccall libflint.fq_nmod_mpoly_set_term_exp_fmpz(a::Ref{fqPolyRepMPolyRingElem}, (n - 1)::Int, exps::Ptr{ZZRingElem}, parent(a)::Ref{fqPolyRepMPolyRing})::Nothing
  return a
end

# Return j-th coordinate of i-th exponent vector
function exponent(a::fqPolyRepMPolyRingElem, i::Int, j::Int)
  (j < 1 || j > nvars(parent(a))) && error("Invalid variable index")
  return @ccall libflint.fq_nmod_mpoly_get_term_var_exp_ui(a::Ref{fqPolyRepMPolyRingElem}, (i - 1)::Int, (j - 1)::Int, a.parent::Ref{fqPolyRepMPolyRing})::Int
end

# Return the coefficient of the term with the given exponent vector
# Return zero if there is no such term
function coeff(a::fqPolyRepMPolyRingElem, exps::Vector{UInt})
  z = base_ring(parent(a))()
  @ccall libflint.fq_nmod_mpoly_get_coeff_fq_nmod_ui(z::Ref{fqPolyRepFieldElem}, a::Ref{fqPolyRepMPolyRingElem}, exps::Ptr{UInt}, parent(a)::Ref{fqPolyRepMPolyRing})::UInt
  return z
end

# Return the coefficient of the term with the given exponent vector
# Return zero if there is no such term
function coeff(a::fqPolyRepMPolyRingElem, exps::Vector{Int})
  z = base_ring(parent(a))()
  @ccall libflint.fq_nmod_mpoly_get_coeff_fq_nmod_ui(z::Ref{fqPolyRepFieldElem}, a::Ref{fqPolyRepMPolyRingElem}, exps::Ptr{Int}, parent(a)::Ref{fqPolyRepMPolyRing})::UInt
  return z
end

# Set the coefficient of the term with the given exponent vector to the
# given ZZRingElem. Removal of a zero term is performed.
function setcoeff!(a::fqPolyRepMPolyRingElem, exps::Vector{Int}, b::fqPolyRepFieldElem)
  @ccall libflint.fq_nmod_mpoly_set_coeff_fq_nmod_ui(a::Ref{fqPolyRepMPolyRingElem}, b::UInt, exps::Ptr{Int}, parent(a)::Ref{fqPolyRepMPolyRing})::Nothing
  return a
end

# Set the coefficient of the term with the given exponent vector to the
# given integer. Removal of a zero term is performed.
setcoeff!(a::fqPolyRepMPolyRingElem, exps::Vector{Int}, b::Union{Integer, zzModRingElem}) =
setcoeff!(a, exps, base_ring(parent(a))(b))

# Sort the terms according to the ordering. This is only needed if unsafe
# functions such as those above have been called and terms have been inserted
# out of order. Note that like terms are not combined and zeros are not
# removed. For that, call combine_like_terms!
function sort_terms!(a::fqPolyRepMPolyRingElem)
  @ccall libflint.fq_nmod_mpoly_sort_terms(a::Ref{fqPolyRepMPolyRingElem}, a.parent::Ref{fqPolyRepMPolyRing})::Nothing
  return a
end

# Return the i-th term of the polynomial, as a polynomial
function term(a::fqPolyRepMPolyRingElem, i::Int)
  z = parent(a)()
  @ccall libflint.fq_nmod_mpoly_get_term(z::Ref{fqPolyRepMPolyRingElem}, a::Ref{fqPolyRepMPolyRingElem}, (i - 1)::Int, a.parent::Ref{fqPolyRepMPolyRing})::Nothing
  return z
end

# Return the i-th monomial of the polynomial, as a polynomial
function monomial(a::fqPolyRepMPolyRingElem, i::Int)
  z = parent(a)()
  @ccall libflint.fq_nmod_mpoly_get_term_monomial(z::Ref{fqPolyRepMPolyRingElem}, a::Ref{fqPolyRepMPolyRingElem}, (i - 1)::Int, a.parent::Ref{fqPolyRepMPolyRing})::Nothing
  return z
end

# Sets the given polynomial m to the i-th monomial of the polynomial
function monomial!(m::fqPolyRepMPolyRingElem, a::fqPolyRepMPolyRingElem, i::Int)
  @ccall libflint.fq_nmod_mpoly_get_term_monomial(m::Ref{fqPolyRepMPolyRingElem}, a::Ref{fqPolyRepMPolyRingElem}, (i - 1)::Int, a.parent::Ref{fqPolyRepMPolyRing})::Nothing
  return m
end

###############################################################################
#
#   Promotion rules
#
###############################################################################

promote_rule(::Type{fqPolyRepMPolyRingElem}, ::Type{V}) where {V <: Integer} = fqPolyRepMPolyRingElem

promote_rule(::Type{fqPolyRepMPolyRingElem}, ::Type{ZZRingElem}) = fqPolyRepMPolyRingElem

promote_rule(::Type{fqPolyRepMPolyRingElem}, ::Type{fqPolyRepFieldElem}) = fqPolyRepMPolyRingElem

###############################################################################
#
#   Build context
#
###############################################################################

function _push_term!(z::fqPolyRepMPolyRingElem, c::fqPolyRepFieldElem, exp::Vector{Int})
  @ccall libflint.fq_nmod_mpoly_push_term_fq_nmod_ui(z::Ref{fqPolyRepMPolyRingElem}, c::Ref{fqPolyRepFieldElem}, exp::Ptr{UInt}, parent(z)::Ref{fqPolyRepMPolyRing})::Nothing
  return z
end

function push_term!(M::MPolyBuildCtx{fqPolyRepMPolyRingElem}, c::fqPolyRepFieldElem, expv::Vector{Int})
  if length(expv) != nvars(parent(M.poly))
    error("length of exponent vector should match the number of variables")
  end
  parent(c) !== base_ring(M.poly) &&error("parent mismatch")
  _push_term!(M.poly, c, expv)
  return M
end

function push_term!(M::MPolyBuildCtx{fqPolyRepMPolyRingElem},
    c::RingElement, expv::Vector{Int})
  push_term!(M, base_ring(M.poly)(c), expv)
  return M
end

function finish(M::MPolyBuildCtx{fqPolyRepMPolyRingElem})
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

function (R::fqPolyRepMPolyRing)()
  z = fqPolyRepMPolyRingElem(R)
  return z
end

function (R::fqPolyRepMPolyRing)(b::zzModRingElem)
  z = fqPolyRepMPolyRingElem(R, b.data)
  return z
end

function (R::fqPolyRepMPolyRing)(b::UInt)
  z = fqPolyRepMPolyRingElem(R, b)
  return z
end

function (R::fqPolyRepMPolyRing)(b::fqPolyRepFieldElem)
  parent(b) != base_ring(R) && error("Unable to coerce element")   
  z = fqPolyRepMPolyRingElem(R, b)
  return z
end

function (R::fqPolyRepMPolyRing)(b::Integer)
  return R(base_ring(R)(b))
end

function (R::fqPolyRepMPolyRing)(b::ZZRingElem)
  return R(base_ring(R)(b))
end

function (R::fqPolyRepMPolyRing)(a::fqPolyRepMPolyRingElem)
  parent(a) != R && error("Unable to coerce polynomial")
  return a
end

# Create poly with given array of coefficients and array of exponent vectors (sorting is performed)
function (R::fqPolyRepMPolyRing)(a::Vector{fqPolyRepFieldElem}, b::Vector{Vector{T}}) where {T <: Union{ZZRingElem, UInt, Int}}
  length(a) != length(b) && error("Coefficient and exponent vector must have the same length")

  for i in 1:length(b)
    length(b[i]) != nvars(R) && error("Exponent vector $i has length $(length(b[i])) (expected $(nvars(R))")
  end

  z = fqPolyRepMPolyRingElem(R, a, b)
  return z
end

# Create poly with given array of coefficients and array of exponent vectors (sorting is performed)
function (R::fqPolyRepMPolyRing)(a::Vector{<:Any}, b::Vector{Vector{T}}) where T
  n = nvars(R)
  length(a) != length(b) && error("Coefficient and exponent vector must have the same length")
  newa = map(base_ring(R), a)
  newb = map(x -> map(ZZ, x), b)
  newaa = convert(Vector{fqPolyRepFieldElem}, newa)
  newbb = convert(Vector{Vector{ZZRingElem}}, newb)

  for i in 1:length(newbb)
    length(newbb[i]) != n && error("Exponent vector $i has length $(length(newbb[i])) (expected $(nvars(R)))")
  end

  return R(newaa, newbb)
end
