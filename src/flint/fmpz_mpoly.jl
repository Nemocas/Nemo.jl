###############################################################################
#
#   ZZMPolyRingElem.jl : FLINT multivariate polynomials over ZZRingElem
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

number_of_variables(a::ZZMPolyRing) = @ccall libflint.fmpz_mpoly_ctx_nvars(a::Ref{ZZMPolyRing})::Int

base_ring(a::ZZMPolyRing) = ZZ

function internal_ordering(a::ZZMPolyRing)
  b = @ccall libflint.fmpz_mpoly_ctx_ord(a::Ref{ZZMPolyRing})::Cint
  return flint_orderings[b + 1]
end

function gens(R::ZZMPolyRing)
  A = Vector{ZZMPolyRingElem}(undef, R.nvars)
  for i = 1:R.nvars
    z = R()
    @ccall libflint.fmpz_mpoly_gen(z::Ref{ZZMPolyRingElem}, (i - 1)::Int, R::Ref{ZZMPolyRing})::Nothing
    A[i] = z
  end
  return A
end

function gen(R::ZZMPolyRing, i::Int)
  n = nvars(R)
  (i <= 0 || i > n) && error("Index must be between 1 and $n")
  z = R()
  @ccall libflint.fmpz_mpoly_gen(z::Ref{ZZMPolyRingElem}, (i - 1)::Int, R::Ref{ZZMPolyRing})::Nothing
  return z
end

function is_gen(a::ZZMPolyRingElem, i::Int)
  n = nvars(parent(a))
  (i <= 0 || i > n) && error("Index must be between 1 and $n")
  R = parent(a)
  return Bool(@ccall libflint.fmpz_mpoly_is_gen(a::Ref{ZZMPolyRingElem}, (i - 1)::Int, a.parent::Ref{ZZMPolyRing})::Cint)
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
  return set!(z, a)
end

length(a::ZZMPolyRingElem) = a.length

one(R::ZZMPolyRing) = one!(R())

zero(R::ZZMPolyRing) = zero!(R())

function isone(a::ZZMPolyRingElem)
  b = @ccall libflint.fmpz_mpoly_is_one(a::Ref{ZZMPolyRingElem}, a.parent::Ref{ZZMPolyRing})::Cint
  return Bool(b)
end

function is_monomial(a::ZZMPolyRingElem)
  return length(a) == 1 && coeff(a, 1) == 1
end

function is_constant(a::ZZMPolyRingElem)
  b = @ccall libflint.fmpz_mpoly_is_fmpz(a::Ref{ZZMPolyRingElem}, parent(a)::Ref{ZZMPolyRing})::Cint
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
  @ccall libflint.fmpz_mpoly_get_term_coeff_fmpz(z::Ref{ZZRingElem}, a::Ref{ZZMPolyRingElem}, (i - 1)::Int, a.parent::Ref{ZZMPolyRing})::Nothing
  return z
end

function coeff(a::ZZMPolyRingElem, b::ZZMPolyRingElem)
  check_parent(a, b)
  !isone(length(b)) && error("Second argument must be a monomial")
  z = ZZRingElem()
  @ccall libflint.fmpz_mpoly_get_coeff_fmpz_monomial(z::Ref{ZZRingElem}, a::Ref{ZZMPolyRingElem}, b::Ref{ZZMPolyRingElem}, parent(a)::Ref{ZZMPolyRing})::Nothing
  return z
end

function trailing_coefficient(p::ZZMPolyRingElem)
  @req !iszero(p) "Zero polynomial does not have a leading monomial"
  return coeff(p, length(p))
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
    d = @ccall libflint.fmpz_mpoly_degree_si(a::Ref{ZZMPolyRingElem}, (i - 1)::Int, a.parent::Ref{ZZMPolyRing})::Int
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
  @ccall libflint.fmpz_mpoly_degree_fmpz(d::Ref{ZZRingElem}, a::Ref{ZZMPolyRingElem}, (i - 1)::Int, a.parent::Ref{ZZMPolyRing})::Nothing
  return d
end

# Return true if degrees fit into an Int
function degrees_fit_int(a::ZZMPolyRingElem)
  b = @ccall libflint.fmpz_mpoly_degrees_fit_si(a::Ref{ZZMPolyRingElem}, a.parent::Ref{ZZMPolyRing})::Cint
  return Bool(b)
end

# Return an array of the max degrees in each variable
function degrees(a::ZZMPolyRingElem)
  if !degrees_fit_int(a)
    throw(OverflowError("degrees of polynomial do not fit into Int"))
  end
  degs = Vector{Int}(undef, nvars(parent(a)))
  @ccall libflint.fmpz_mpoly_degrees_si(degs::Ptr{Int}, a::Ref{ZZMPolyRingElem}, a.parent::Ref{ZZMPolyRing})::Nothing
  return degs
end

# Return an array of the max degrees as fmpzs in each variable
function degrees_fmpz(a::ZZMPolyRingElem)
  n = nvars(parent(a))
  degs = Vector{ZZRingElem}(undef, n)
  for i in 1:n
    degs[i] = ZZRingElem()
  end
  @ccall libflint.fmpz_mpoly_degrees_fmpz(degs::Ptr{Ref{ZZRingElem}}, a::Ref{ZZMPolyRingElem}, a.parent::Ref{ZZMPolyRing})::Nothing
  return degs
end

# Return true if degree fits into an Int
function total_degree_fits_int(a::ZZMPolyRingElem)
  b = @ccall libflint.fmpz_mpoly_total_degree_fits_si(a::Ref{ZZMPolyRingElem}, a.parent::Ref{ZZMPolyRing})::Cint
  return Bool(b)
end

# Total degree as an Int
function total_degree(a::ZZMPolyRingElem)
  if !total_degree_fits_int(a)
    throw(OverflowError("Total degree of polynomial does not fit into Int"))
  end
  d = @ccall libflint.fmpz_mpoly_total_degree_si(a::Ref{ZZMPolyRingElem}, a.parent::Ref{ZZMPolyRing})::Int
  return d
end

# Total degree as an ZZRingElem
function total_degree_fmpz(a::ZZMPolyRingElem)
  d = ZZRingElem()
  @ccall libflint.fmpz_mpoly_total_degree_fmpz(d::Ref{ZZRingElem}, a::Ref{ZZMPolyRingElem}, a.parent::Ref{ZZMPolyRing})::Nothing
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
  @ccall libflint.fmpz_mpoly_get_coeff_vars_ui(z::Ref{ZZMPolyRingElem}, a::Ref{ZZMPolyRingElem}, vars::Ptr{Int}, exps::Ptr{Int}, length(vars)::Int, a.parent::Ref{ZZMPolyRing})::Nothing
  return z
end

###############################################################################
#
#   Basic arithmetic
#
###############################################################################

function -(a::ZZMPolyRingElem)
  z = parent(a)()
  @ccall libflint.fmpz_mpoly_neg(z::Ref{ZZMPolyRingElem}, a::Ref{ZZMPolyRingElem}, a.parent::Ref{ZZMPolyRing})::Nothing
  return z
end

function +(a::ZZMPolyRingElem, b::ZZMPolyRingElem)
  check_parent(a, b)
  z = parent(a)()
  return add!(z, a, b)
end

function -(a::ZZMPolyRingElem, b::ZZMPolyRingElem)
  check_parent(a, b)
  z = parent(a)()
  return sub!(z, a, b)
end

function *(a::ZZMPolyRingElem, b::ZZMPolyRingElem)
  check_parent(a, b)
  z = parent(a)()
  return mul!(z, a, b)
end

###############################################################################
#
#   Ad hoc arithmetic
#
###############################################################################

for jT in (ZZRingElem, Integer)
  @eval begin
    +(a::ZZMPolyRingElem, b::($jT)) = add!(parent(a)(), a, b)
    +(a::($jT), b::ZZMPolyRingElem) = add!(parent(b)(), a, b)

    -(a::ZZMPolyRingElem, b::($jT)) = sub!(parent(a)(), a, b)
    -(a::($jT), b::ZZMPolyRingElem) = sub!(parent(b)(), a, b)

    *(a::ZZMPolyRingElem, b::($jT)) = mul!(parent(a)(), a, b)
    *(a::($jT), b::ZZMPolyRingElem) = mul!(parent(b)(), a, b)

    function divexact(a::ZZMPolyRingElem, b::($jT); check::Bool=true)
      z = parent(a)()
      if check
        d, z = divides!(z, a, b)
        d || error("Division is not exact in divexact")
      else
        z = divexact!(z, a, b)
      end
      return z
    end
  end
end

###############################################################################
#
#   Powering
#
###############################################################################

# Cannot use IntegerUnion here to avoid ambiguity.

function ^(a::ZZMPolyRingElem, b::Int)
  is_negative(b) && throw(DomainError(b, "Exponent must be non-negative"))
  return pow!(parent(a)(), a, b)
end

function ^(a::ZZMPolyRingElem, b::ZZRingElem)
  is_negative(b) && throw(DomainError(b, "Exponent must be non-negative"))
  return pow!(parent(a)(), a, b)
end

################################################################################
#
#   GCD
#
################################################################################

function gcd(a::ZZMPolyRingElem, b::ZZMPolyRingElem)
  check_parent(a, b)
  z = parent(a)()
  r = @ccall libflint.fmpz_mpoly_gcd(z::Ref{ZZMPolyRingElem}, a::Ref{ZZMPolyRingElem}, b::Ref{ZZMPolyRingElem}, a.parent::Ref{ZZMPolyRing})::Cint
  r == 0 && error("Unable to compute gcd")
  return z
end

function gcd_with_cofactors(a::ZZMPolyRingElem, b::ZZMPolyRingElem)
  z = parent(a)()
  abar = parent(a)()
  bbar = parent(a)()
  r = @ccall libflint.fmpz_mpoly_gcd_cofactors(z::Ref{ZZMPolyRingElem}, abar::Ref{ZZMPolyRingElem}, bbar::Ref{ZZMPolyRingElem}, a::Ref{ZZMPolyRingElem}, b::Ref{ZZMPolyRingElem}, a.parent::Ref{ZZMPolyRing})::Cint
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
      @ccall libflint.fmpz_mpoly_factor_get_base(f::Ref{ZZMPolyRingElem}, fac::Ref{fmpz_mpoly_factor}, i::Int, R::Ref{ZZMPolyRing})::Nothing
    else
      @ccall libflint.fmpz_mpoly_factor_swap_base(f::Ref{ZZMPolyRingElem}, fac::Ref{fmpz_mpoly_factor}, i::Int, R::Ref{ZZMPolyRing})::Nothing
    end
    F.fac[f] = @ccall libflint.fmpz_mpoly_factor_get_exp_si(fac::Ref{fmpz_mpoly_factor}, i::Int, R::Ref{ZZMPolyRing})::Int
  end
  c = ZZRingElem()
  @ccall libflint.fmpz_mpoly_factor_get_constant_fmpz(c::Ref{ZZRingElem}, fac::Ref{fmpz_mpoly_factor})::Nothing
  sgnc = sign(Int, c)
  if sgnc != 0
    G = fmpz_factor()
    @ccall libflint.fmpz_factor(G::Ref{fmpz_factor}, c::Ref{ZZRingElem})::Nothing
    for i in 1:G.num
      @ccall libflint.fmpz_factor_get_fmpz(c::Ref{ZZRingElem}, G::Ref{fmpz_factor}, (i - 1)::Int)::Nothing
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
  ok = @ccall libflint.fmpz_mpoly_factor(fac::Ref{fmpz_mpoly_factor}, a::Ref{ZZMPolyRingElem}, R::Ref{ZZMPolyRing})::Cint
  ok == 0 && error("unable to compute factorization")
  return Fac{ZZMPolyRingElem}(fac, false)
end

function factor_squarefree(a::ZZMPolyRingElem)
  iszero(a) && throw(ArgumentError("Argument must be non-zero"))
  R = parent(a)
  fac = fmpz_mpoly_factor(R)
  ok = @ccall libflint.fmpz_mpoly_factor_squarefree(fac::Ref{fmpz_mpoly_factor}, a::Ref{ZZMPolyRingElem}, R::Ref{ZZMPolyRing})::Cint
  ok == 0 && error("unable to compute factorization")
  return Fac{ZZMPolyRingElem}(fac, false)
end


function sqrt(a::ZZMPolyRingElem; check::Bool=true)
  q = parent(a)()
  flag = Bool(@ccall libflint.fmpz_mpoly_sqrt_heap(q::Ref{ZZMPolyRingElem}, a::Ref{ZZMPolyRingElem}, a.parent::Ref{ZZMPolyRing}, Cint(check)::Cint)::Cint)
  check && !flag && error("Not a square")
  return q
end

function is_square(a::ZZMPolyRingElem)
  return Bool(@ccall libflint.fmpz_mpoly_is_square(a::Ref{ZZMPolyRingElem}, a.parent::Ref{ZZMPolyRing})::Cint)
end

function is_square_with_sqrt(a::ZZMPolyRingElem)
  q = parent(a)()
  flag = @ccall libflint.fmpz_mpoly_sqrt(q::Ref{ZZMPolyRingElem}, a::Ref{ZZMPolyRingElem}, a.parent::Ref{ZZMPolyRing})::Cint
  return (Bool(flag), q)
end

###############################################################################
#
#   Comparison
#
###############################################################################

function ==(a::ZZMPolyRingElem, b::ZZMPolyRingElem)
  check_parent(a, b)
  return Bool(@ccall libflint.fmpz_mpoly_equal(a::Ref{ZZMPolyRingElem}, b::Ref{ZZMPolyRingElem}, a.parent::Ref{ZZMPolyRing})::Cint)
end

function Base.isless(a::ZZMPolyRingElem, b::ZZMPolyRingElem)
  (!is_monomial(a) || !is_monomial(b)) && error("Not monomials in comparison")
  return (@ccall libflint.fmpz_mpoly_cmp(a::Ref{ZZMPolyRingElem}, b::Ref{ZZMPolyRingElem}, a.parent::Ref{ZZMPolyRing})::Cint) < 0
end

###############################################################################
#
#   Ad hoc comparison
#
###############################################################################

function ==(a::ZZMPolyRingElem, b::ZZRingElem)
  return Bool(@ccall libflint.fmpz_mpoly_equal_fmpz(a::Ref{ZZMPolyRingElem}, b::Ref{ZZRingElem}, a.parent::Ref{ZZMPolyRing})::Cint)
end

==(a::ZZRingElem, b::ZZMPolyRingElem) = b == a

function ==(a::ZZMPolyRingElem, b::Int)
  return Bool(@ccall libflint.fmpz_mpoly_equal_si(a::Ref{ZZMPolyRingElem}, b::Int, a.parent::Ref{ZZMPolyRing})::Cint)
end

==(a::Int, b::ZZMPolyRingElem) = b == a

==(a::ZZMPolyRingElem, b::Integer) = a == flintify(b)

==(a::Integer, b::ZZMPolyRingElem) = b == a

###############################################################################
#
#   Divisibility
#
###############################################################################

function divides(a::ZZMPolyRingElem, b::Union{IntegerUnion, ZZMPolyRingElem})
  check_parent(a, b)
  z = zero(parent(a))
  iszero(a) && return true, z
  iszero(b) && return false, z
  return divides!(z, a, b)
end

###############################################################################
#
#   Division with remainder
#
###############################################################################

function Base.div(a::ZZMPolyRingElem, b::ZZMPolyRingElem)
  check_parent(a, b)
  q = parent(a)()
  @ccall libflint.fmpz_mpoly_div(q::Ref{ZZMPolyRingElem}, a::Ref{ZZMPolyRingElem}, b::Ref{ZZMPolyRingElem}, a.parent::Ref{ZZMPolyRing})::Nothing
  return q
end

function Base.divrem(a::ZZMPolyRingElem, b::ZZMPolyRingElem)
  check_parent(a, b)
  q = parent(a)()
  r = parent(a)()
  @ccall libflint.fmpz_mpoly_divrem(q::Ref{ZZMPolyRingElem}, r::Ref{ZZMPolyRingElem}, a::Ref{ZZMPolyRingElem}, b::Ref{ZZMPolyRingElem}, a.parent::Ref{ZZMPolyRing})::Nothing
  return q, r
end

function Base.divrem(a::ZZMPolyRingElem, b::Vector{ZZMPolyRingElem})
  len = length(b)
  q = [parent(a)() for i in 1:len]
  r = parent(a)()
  @ccall libflint.fmpz_mpoly_divrem_ideal(q::Ptr{Ref{ZZMPolyRingElem}}, r::Ref{ZZMPolyRingElem}, a::Ref{ZZMPolyRingElem}, b::Ptr{Ref{ZZMPolyRingElem}}, len::Int, a.parent::Ref{ZZMPolyRing})::Nothing
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
  @ccall libflint.fmpz_mpoly_derivative(z::Ref{ZZMPolyRingElem}, a::Ref{ZZMPolyRingElem}, (i - 1)::Int, parent(a)::Ref{ZZMPolyRing})::Nothing
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
  GC.@preserve b @ccall libflint.fmpz_mpoly_evaluate_all_fmpz(z::Ref{ZZRingElem}, a::Ref{ZZMPolyRingElem}, b::Ptr{ZZRingElem}, parent(a)::Ref{ZZMPolyRing})::Nothing
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
  allequal(map(parent, bs)) || error("parents do not match")
  R = parent(a)
  S = parent(bs[1])

  length(bs) != nvars(R) &&
  error("Number of variables does not match number of values")

  c = S()
  fl = @ccall libflint.fmpz_mpoly_compose_fmpz_mpoly(c::Ref{ZZMPolyRingElem}, a::Ref{ZZMPolyRingElem}, bs::Ptr{Ref{ZZMPolyRingElem}}, R::Ref{ZZMPolyRing}, S::Ref{ZZMPolyRing})::Cint
  fl == 0 && error("Something wrong in evaluation.")
  return c
end

function evaluate(a::ZZMPolyRingElem, bs::Vector{ZZPolyRingElem})
  allequal(map(parent, bs)) || error("parents do not match")
  R = parent(a)
  S = parent(bs[1])

  length(bs) != nvars(R) &&
  error("Number of variables does not match number of values")

  c = S()
  fl = @ccall libflint.fmpz_mpoly_compose_fmpz_poly(c::Ref{ZZPolyRingElem}, a::Ref{ZZMPolyRingElem}, bs::Ptr{Ref{ZZPolyRingElem}}, R::Ref{ZZMPolyRing})::Cint
  fl == 0 && error("Something wrong in evaluation.")
  return c
end

###############################################################################
#
#   Unsafe functions
#
###############################################################################

function zero!(a::ZZMPolyRingElem)
  @ccall libflint.fmpz_mpoly_zero(a::Ref{ZZMPolyRingElem}, a.parent::Ref{ZZMPolyRing})::Nothing
  return a
end

function one!(a::ZZMPolyRingElem)
  @ccall libflint.fmpz_mpoly_one(a::Ref{ZZMPolyRingElem}, a.parent::Ref{ZZMPolyRing})::Nothing
  return a
end

function neg!(z::ZZMPolyRingElem, a::ZZMPolyRingElem)
  @ccall libflint.fmpz_mpoly_neg(z::Ref{ZZMPolyRingElem}, a::Ref{ZZMPolyRingElem}, a.parent::Ref{ZZMPolyRing})::Nothing
  return z
end

#

function set!(z::ZZMPolyRingElem, a::ZZMPolyRingElem)
  @ccall libflint.fmpz_mpoly_set(z::Ref{ZZMPolyRingElem}, a::Ref{ZZMPolyRingElem}, parent(z)::Ref{ZZMPolyRing})::Nothing
  return z
end

function set!(z::ZZMPolyRingElem, a::ZZRingElemOrPtr)
  @ccall libflint.fmpz_mpoly_set_fmpz(z::Ref{ZZMPolyRingElem}, a::Ref{ZZRingElem}, parent(z)::Ref{ZZMPolyRing})::Nothing
  return z
end

function set!(z::ZZMPolyRingElem, a::Int)
  @ccall libflint.fmpz_mpoly_set_si(z::Ref{ZZMPolyRingElem}, a::Int, parent(z)::Ref{ZZMPolyRing})::Nothing
  return z
end

function set!(z::ZZMPolyRingElem, a::UInt)
  @ccall libflint.fmpz_mpoly_set_ui(z::Ref{ZZMPolyRingElem}, a::UInt, parent(z)::Ref{ZZMPolyRing})::Nothing
  return z
end

set!(z::ZZMPolyRingElem, a::Integer) = set!(z, flintify(a))

#


function add!(z::ZZMPolyRingElem, a::ZZMPolyRingElem, b::ZZMPolyRingElem)
  @ccall libflint.fmpz_mpoly_add(z::Ref{ZZMPolyRingElem}, a::Ref{ZZMPolyRingElem}, b::Ref{ZZMPolyRingElem}, a.parent::Ref{ZZMPolyRing})::Nothing
  return z
end

function sub!(z::ZZMPolyRingElem, a::ZZMPolyRingElem, b::ZZMPolyRingElem)
  @ccall libflint.fmpz_mpoly_sub(z::Ref{ZZMPolyRingElem}, a::Ref{ZZMPolyRingElem}, b::Ref{ZZMPolyRingElem}, a.parent::Ref{ZZMPolyRing})::Nothing
  return z
end

function mul!(z::ZZMPolyRingElem, a::ZZMPolyRingElem, b::ZZMPolyRingElem)
  @ccall libflint.fmpz_mpoly_mul(z::Ref{ZZMPolyRingElem}, a::Ref{ZZMPolyRingElem}, b::Ref{ZZMPolyRingElem}, a.parent::Ref{ZZMPolyRing})::Nothing
  return z
end

function divides!(z::ZZMPolyRingElem, a::ZZMPolyRingElem, b::ZZMPolyRingElem)
  d = @ccall libflint.fmpz_mpoly_divides(z::Ref{ZZMPolyRingElem}, a::Ref{ZZMPolyRingElem}, b::Ref{ZZMPolyRingElem}, parent(a)::Ref{ZZMPolyRing})::Cint
  return Bool(d), z
end

for (jT, cN, cT) in ((ZZRingElem, :fmpz, Ref{ZZRingElem}), (Int, :si, Int), (UInt, :ui, UInt))
  @eval begin
    function add!(z::ZZMPolyRingElem, a::ZZMPolyRingElem, b::$jT)
      @ccall libflint.$(string(:fmpz_mpoly_add_, cN))(z::Ref{ZZMPolyRingElem},
                a::Ref{ZZMPolyRingElem}, b::$cT, parent(a)::Ref{ZZMPolyRing})::Nothing
      return z
    end

    function sub!(z::ZZMPolyRingElem, a::ZZMPolyRingElem, b::($jT))
      @ccall libflint.$(string(:fmpz_mpoly_sub_, cN))(z::Ref{ZZMPolyRingElem},
                a::Ref{ZZMPolyRingElem}, b::$cT, parent(a)::Ref{ZZMPolyRing})::Nothing
      return z
    end

    function mul!(z::ZZMPolyRingElem, a::ZZMPolyRingElem, b::($jT))
      @ccall libflint.$(string(:fmpz_mpoly_scalar_mul_, cN))(z::Ref{ZZMPolyRingElem},
                a::Ref{ZZMPolyRingElem}, b::$cT, parent(a)::Ref{ZZMPolyRing})::Nothing
      return z
    end

    function divexact!(z::ZZMPolyRingElem, a::ZZMPolyRingElem, b::($jT))
      @ccall libflint.$(string(:fmpz_mpoly_scalar_divexact_, cN))(z::Ref{ZZMPolyRingElem},
                a::Ref{ZZMPolyRingElem}, b::$cT, parent(a)::Ref{ZZMPolyRing})::Nothing
      return z
    end

    function divides!(z::ZZMPolyRingElem, a::ZZMPolyRingElem, b::($jT))
      d = @ccall libflint.$(string(:fmpz_mpoly_scalar_divides_, cN))(z::Ref{ZZMPolyRingElem},
                    a::Ref{ZZMPolyRingElem}, b::$cT, parent(a)::Ref{ZZMPolyRing})::Cint
      return Bool(d), z
    end
  end
end

add!(a::ZZMPolyRingElem, b::ZZMPolyRingElem, c::IntegerUnion) = add!(a, b, flintify(c))
add!(a::ZZMPolyRingElem, b::IntegerUnion, c::ZZMPolyRingElem) = add!(a, c, b)

sub!(a::ZZMPolyRingElem, b::ZZMPolyRingElem, c::IntegerUnion) = sub!(a, b, flintify(c))
sub!(a::ZZMPolyRingElem, b::IntegerUnion, c::ZZMPolyRingElem) = neg!(sub!(a, c, b))

mul!(a::ZZMPolyRingElem, b::ZZMPolyRingElem, c::IntegerUnion) = mul!(a, b, flintify(c))
mul!(a::ZZMPolyRingElem, b::IntegerUnion, c::ZZMPolyRingElem) = mul!(a, c, b)

divexact!(a::ZZMPolyRingElem, b::ZZMPolyRingElem, c::RationalUnion) = divexact!(a, b, flintify(c))

divides!(a::ZZMPolyRingElem, b::ZZMPolyRingElem, c::RationalUnion) = divides!(a, b, flintify(c))

# Set the n-th coefficient of a to c. If zero coefficients are inserted, they
# must be removed with combine_like_terms!
function setcoeff!(a::ZZMPolyRingElem, n::Int, c::ZZRingElem)
  if n > length(a)
    @ccall libflint.fmpz_mpoly_resize(a::Ref{ZZMPolyRingElem}, n::Int, a.parent::Ref{ZZMPolyRing})::Nothing
  end
  @ccall libflint.fmpz_mpoly_set_term_coeff_fmpz(a::Ref{ZZMPolyRingElem}, (n - 1)::Int, c::Ref{ZZRingElem}, a.parent::Ref{ZZMPolyRing})::Nothing
  return a
end

# Set the i-th coefficient of a to c. If zero coefficients are inserted, they
# must be removed with combine_like_terms!
setcoeff!(a::ZZMPolyRingElem, i::Int, c::Integer) = setcoeff!(a, i, ZZRingElem(c))

# Remove zero terms and combine adjacent terms if they have the same monomial
# no sorting is performed
function combine_like_terms!(a::ZZMPolyRingElem)
  @ccall libflint.fmpz_mpoly_combine_like_terms(a::Ref{ZZMPolyRingElem}, a.parent::Ref{ZZMPolyRing})::Nothing
  return a
end

#

function pow!(z::ZZMPolyRingElem, a::ZZMPolyRingElem, n::Integer)
  ok = Bool(@ccall libflint.fmpz_mpoly_pow_ui(z::Ref{ZZMPolyRingElem}, a::Ref{ZZMPolyRingElem}, UInt(n)::UInt, parent(a)::Ref{ZZMPolyRing})::Cint)
  if !ok
    error("unable to compute power")
  end
  return z
end

function pow!(z::ZZMPolyRingElem, a::ZZMPolyRingElem, n::ZZRingElemOrPtr)
  ok = Bool(@ccall libflint.fmpz_mpoly_pow_fmpz(z::Ref{ZZMPolyRingElem}, a::Ref{ZZMPolyRingElem}, n::Ref{ZZRingElem}, parent(a)::Ref{ZZMPolyRing})::Cint)
  if !ok
    error("unable to compute power")
  end
  return z
end

###############################################################################
#
#   Manipulating terms and monomials
#
###############################################################################

function exponent_vector_fits(::Type{Int}, a::ZZMPolyRingElem, i::Int)
  b = @ccall libflint.fmpz_mpoly_term_exp_fits_ui(a::Ref{ZZMPolyRingElem}, (i - 1)::Int, a.parent::Ref{ZZMPolyRing})::Cint
  return Bool(b)
end

function exponent_vector_fits(::Type{UInt}, a::ZZMPolyRingElem, i::Int)
  b = @ccall libflint.fmpz_mpoly_term_exp_fits_si(a::Ref{ZZMPolyRingElem}, (i - 1)::Int, a.parent::Ref{ZZMPolyRing})::Cint
  return Bool(b)
end

function exponent_vector!(z::Vector{Int}, a::ZZMPolyRingElem, i::Int)
  @ccall libflint.fmpz_mpoly_get_term_exp_si(z::Ptr{Int}, a::Ref{ZZMPolyRingElem}, (i - 1)::Int, parent(a)::Ref{ZZMPolyRing})::Nothing
  return z
end

function exponent_vector!(z::Vector{UInt}, a::ZZMPolyRingElem, i::Int)
  @ccall libflint.fmpz_mpoly_get_term_exp_ui(z::Ptr{UInt}, a::Ref{ZZMPolyRingElem}, (i - 1)::Int, parent(a)::Ref{ZZMPolyRing})::Nothing
  return z
end

function exponent_vector!(z::Vector{ZZRingElem}, a::ZZMPolyRingElem, i::Int)
  @ccall libflint.fmpz_mpoly_get_term_exp_fmpz(z::Ptr{Ref{ZZRingElem}}, a::Ref{ZZMPolyRingElem}, (i - 1)::Int, parent(a)::Ref{ZZMPolyRing})::Nothing
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
    @ccall libflint.fmpz_mpoly_resize(a::Ref{ZZMPolyRingElem}, n::Int, a.parent::Ref{ZZMPolyRing})::Nothing
  end
  @ccall libflint.fmpz_mpoly_set_term_exp_ui(a::Ref{ZZMPolyRingElem}, (n - 1)::Int, exps::Ptr{UInt}, parent(a)::Ref{ZZMPolyRing})::Nothing
  return a
end

# Set exponent of n-th term to given vector of Int's
# No sort is performed, so this is unsafe. The Int's must be positive, but
# no check is performed
function set_exponent_vector!(a::ZZMPolyRingElem, n::Int, exps::Vector{Int})
  if n > length(a)
    @ccall libflint.fmpz_mpoly_resize(a::Ref{ZZMPolyRingElem}, n::Int, a.parent::Ref{ZZMPolyRing})::Nothing
  end
  @ccall libflint.fmpz_mpoly_set_term_exp_ui(a::Ref{ZZMPolyRingElem}, (n - 1)::Int, exps::Ptr{Int}, parent(a)::Ref{ZZMPolyRing})::Nothing
  return a
end

# Set exponent of n-th term to given vector of ZZRingElem's
# No sort is performed, so this is unsafe
function set_exponent_vector!(a::ZZMPolyRingElem, n::Int, exps::Vector{ZZRingElem})
  if n > length(a)
    @ccall libflint.fmpz_mpoly_resize(a::Ref{ZZMPolyRingElem}, n::Int, a.parent::Ref{ZZMPolyRing})::Nothing
    return a
  end
  GC.@preserve exps @ccall libflint.fmpz_mpoly_set_term_exp_fmpz(a::Ref{ZZMPolyRingElem}, (n - 1)::Int, exps::Ptr{ZZRingElem}, parent(a)::Ref{ZZMPolyRing})::Nothing
  return a
end

# Return j-th coordinate of i-th exponent vector
function exponent(a::ZZMPolyRingElem, i::Int, j::Int)
  (j < 1 || j > nvars(parent(a))) && error("Invalid variable index")
  return @ccall libflint.fmpz_mpoly_get_term_var_exp_ui(a::Ref{ZZMPolyRingElem}, (i - 1)::Int, (j - 1)::Int, a.parent::Ref{ZZMPolyRing})::Int
end

# Return the coefficient of the term with the given exponent vector
# Return zero if there is no such term
function coeff(a::ZZMPolyRingElem, exps::Vector{UInt})
  z = ZZRingElem()
  @ccall libflint.fmpz_mpoly_get_coeff_fmpz_ui(z::Ref{ZZRingElem}, a::Ref{ZZMPolyRingElem}, exps::Ptr{UInt}, parent(a)::Ref{ZZMPolyRing})::Nothing
  return z
end

# Return the coefficient of the term with the given exponent vector
# Return zero if there is no such term
function coeff(a::ZZMPolyRingElem, exps::Vector{Int})
  z = ZZRingElem()
  @ccall libflint.fmpz_mpoly_get_coeff_fmpz_ui(z::Ref{ZZRingElem}, a::Ref{ZZMPolyRingElem}, exps::Ptr{Int}, parent(a)::Ref{ZZMPolyRing})::Nothing
  return z
end

# Set the coefficient of the term with the given exponent vector to the
# given ZZRingElem. Removal of a zero term is performed.
function setcoeff!(a::ZZMPolyRingElem, exps::Vector{UInt}, b::ZZRingElem)
  @ccall libflint.fmpz_mpoly_set_coeff_fmpz_ui(a::Ref{ZZMPolyRingElem}, b::Ref{ZZRingElem}, exps::Ptr{UInt}, parent(a)::Ref{ZZMPolyRing})::Nothing
  return a
end

# Set the coefficient of the term with the given exponent vector to the
# given ZZRingElem. Removal of a zero term is performed.
function setcoeff!(a::ZZMPolyRingElem, exps::Vector{Int}, b::ZZRingElem)
  @ccall libflint.fmpz_mpoly_set_coeff_fmpz_ui(a::Ref{ZZMPolyRingElem}, b::Ref{ZZRingElem}, exps::Ptr{Int}, parent(a)::Ref{ZZMPolyRing})::Nothing
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
  @ccall libflint.fmpz_mpoly_sort_terms(a::Ref{ZZMPolyRingElem}, a.parent::Ref{ZZMPolyRing})::Nothing
  return a
end

# Return the i-th term of the polynomial, as a polynomial
function term(a::ZZMPolyRingElem, i::Int)
  z = parent(a)()
  @ccall libflint.fmpz_mpoly_get_term(z::Ref{ZZMPolyRingElem}, a::Ref{ZZMPolyRingElem}, (i - 1)::Int, a.parent::Ref{ZZMPolyRing})::Nothing
  return z
end

# Return the i-th monomial of the polynomial, as a polynomial
function monomial(a::ZZMPolyRingElem, i::Int)
  z = parent(a)()
  @ccall libflint.fmpz_mpoly_get_term_monomial(z::Ref{ZZMPolyRingElem}, a::Ref{ZZMPolyRingElem}, (i - 1)::Int, a.parent::Ref{ZZMPolyRing})::Nothing
  return z
end

# Sets the given polynomial m to the i-th monomial of the polynomial
function monomial!(m::ZZMPolyRingElem, a::ZZMPolyRingElem, i::Int)
  @ccall libflint.fmpz_mpoly_get_term_monomial(m::Ref{ZZMPolyRingElem}, a::Ref{ZZMPolyRingElem}, (i - 1)::Int, a.parent::Ref{ZZMPolyRing})::Nothing
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
  @ccall libflint.fmpz_mpoly_push_term_fmpz_ui(z::Ref{ZZMPolyRingElem}, c::Ref{ZZRingElem}, exp::Ptr{UInt}, parent(z)::Ref{ZZMPolyRing})::Nothing
  return z
end

function _push_term!(z::ZZMPolyRingElem, c::Int, exp::Vector{Int})
  @ccall libflint.fmpz_mpoly_push_term_si_ui(z::Ref{ZZMPolyRingElem}, c::Int, exp::Ptr{UInt}, parent(z)::Ref{ZZMPolyRing})::Nothing
  return z
end

function _push_term!(z::ZZMPolyRingElem, c::UInt, exp::Vector{Int})
  @ccall libflint.fmpz_mpoly_push_term_ui_ui(z::Ref{ZZMPolyRingElem}, c::UInt, exp::Ptr{UInt}, parent(z)::Ref{ZZMPolyRing})::Nothing
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
