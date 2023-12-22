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

function check_parent(a::fqPolyRepMPolyRingElem, b::fqPolyRepMPolyRingElem)
   parent(a) != parent(b) &&
      error("Incompatible polynomial rings in polynomial operation")
end

nvars(a::fqPolyRepMPolyRing) = a.nvars

base_ring(a::fqPolyRepMPolyRing) = a.base_ring

function ordering(a::fqPolyRepMPolyRing)
   b = a.ord
#   b = ccall((:fq_nmod_mpoly_ctx_ord, libflint), Cint, (Ref{fqPolyRepMPolyRing}, ), a)
   return flint_orderings[b + 1]
end

function gens(R::fqPolyRepMPolyRing)
   A = Vector{fqPolyRepMPolyRingElem}(undef, R.nvars)
   for i = 1:R.nvars
      z = R()
      ccall((:fq_nmod_mpoly_gen, libflint), Nothing,
            (Ref{fqPolyRepMPolyRingElem}, Int, Ref{fqPolyRepMPolyRing}),
            z, i - 1, R)
      A[i] = z
   end
   return A
end

function gen(R::fqPolyRepMPolyRing, i::Int)
   n = nvars(R)
   !(1 <= i <= n) && error("Index must be between 1 and $n")
   z = R()
   ccall((:fq_nmod_mpoly_gen, libflint), Nothing,
         (Ref{fqPolyRepMPolyRingElem}, Int, Ref{fqPolyRepMPolyRing}),
         z, i - 1, R)
   return z
end

function is_gen(a::fqPolyRepMPolyRingElem, i::Int)
   n = nvars(parent(a))
   !(1 <= i <= n) && error("Index must be between 1 and $n")
   return Bool(ccall((:fq_nmod_mpoly_is_gen, libflint), Cint,
                     (Ref{fqPolyRepMPolyRingElem}, Int, Ref{fqPolyRepMPolyRing}),
                     a, i - 1, a.parent))
end

function is_gen(a::fqPolyRepMPolyRingElem)
   return Bool(ccall((:fq_nmod_mpoly_is_gen, libflint), Cint,
                     (Ref{fqPolyRepMPolyRingElem}, Int, Ref{fqPolyRepMPolyRing}),
                     a, -1, a.parent))
end

function deepcopy_internal(a::fqPolyRepMPolyRingElem, dict::IdDict)
   z = parent(a)()
   ccall((:fq_nmod_mpoly_set, libflint), Nothing,
         (Ref{fqPolyRepMPolyRingElem}, Ref{fqPolyRepMPolyRingElem}, Ref{fqPolyRepMPolyRing}),
         z, a, a.parent)
   return z
end

function length(a::fqPolyRepMPolyRingElem)
   n = ccall((:fq_nmod_mpoly_length, libflint), Int,
             (Ref{fqPolyRepMPolyRingElem}, Ref{fqPolyRepMPolyRing}),
             a, a.parent)
   return n
end

function one(R::fqPolyRepMPolyRing)
   z = R()
   ccall((:fq_nmod_mpoly_one, libflint), Nothing,
         (Ref{fqPolyRepMPolyRingElem}, Ref{fqPolyRepMPolyRing}),
         z, R)
   return z
end

function zero(R::fqPolyRepMPolyRing)
   z = R()
   ccall((:fq_nmod_mpoly_zero, libflint), Nothing,
         (Ref{fqPolyRepMPolyRingElem}, Ref{fqPolyRepMPolyRing}),
         z, R)
   return z
end

function isone(a::fqPolyRepMPolyRingElem)
   b = ccall((:fq_nmod_mpoly_is_one, libflint), Cint,
             (Ref{fqPolyRepMPolyRingElem}, Ref{fqPolyRepMPolyRing}),
             a, a.parent)
   return Bool(b)
end

function iszero(a::fqPolyRepMPolyRingElem)
   b = ccall((:fq_nmod_mpoly_is_zero, libflint), Cint,
             (Ref{fqPolyRepMPolyRingElem}, Ref{fqPolyRepMPolyRing}),
             a, a.parent)
   return Bool(b)
end

function is_monomial(a::fqPolyRepMPolyRingElem)
   return length(a) == 1 && isone(coeff(a, 1))
end

function is_term(a::fqPolyRepMPolyRingElem)
   return length(a) == 1
end

function is_unit(a::fqPolyRepMPolyRingElem)
   return is_constant(a)
end

function is_constant(a::fqPolyRepMPolyRingElem)
   b = ccall((:fq_nmod_mpoly_is_fq_nmod, libflint), Cint,
             (Ref{fqPolyRepMPolyRingElem}, Ref{fqPolyRepMPolyRing}),
             a, parent(a))
   return Bool(b)
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
   ccall((:fq_nmod_mpoly_get_term_coeff_fq_nmod, libflint), Nothing,
         (Ref{fqPolyRepFieldElem}, Ref{fqPolyRepMPolyRingElem}, Int, Ref{fqPolyRepMPolyRing}),
         z, a, i - 1, a.parent)
   return z
end

function coeff(a::fqPolyRepMPolyRingElem, b::fqPolyRepMPolyRingElem)
   check_parent(a, b)
   !isone(length(b)) && error("Second argument must be a monomial")
   z = base_ring(parent(a))()
   ccall((:fq_nmod_mpoly_get_coeff_fq_nmod_monomial, libflint), UInt,
         (Ref{fqPolyRepFieldElem}, Ref{fqPolyRepMPolyRingElem},
          Ref{fqPolyRepMPolyRingElem}, Ref{fqPolyRepMPolyRing}),
         z, a, b, parent(a))
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
      d = ccall((:fq_nmod_mpoly_degree_si, libflint), Int,
             (Ref{fqPolyRepMPolyRingElem}, Int, Ref{fqPolyRepMPolyRing}),
             a, i - 1, a.parent)
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
   ccall((:fq_nmod_mpoly_degree_fmpz, libflint), Nothing,
         (Ref{ZZRingElem}, Ref{fqPolyRepMPolyRingElem}, Int, Ref{fqPolyRepMPolyRing}),
         d, a, i - 1, a.parent)
   return d
end

# Return true if degrees fit into an Int
function degrees_fit_int(a::fqPolyRepMPolyRingElem)
   b = ccall((:fq_nmod_mpoly_degrees_fit_si, libflint), Cint,
             (Ref{fqPolyRepMPolyRingElem}, Ref{fqPolyRepMPolyRing}),
             a, a.parent)
   return Bool(b)
end

# Return an array of the max degrees in each variable
function degrees(a::fqPolyRepMPolyRingElem)
   if !degrees_fit_int(a)
      throw(OverflowError("degrees of polynomial do not fit into Int"))
   end
   degs = Vector{Int}(undef, nvars(parent(a)))
   ccall((:fq_nmod_mpoly_degrees_si, libflint), Nothing,
         (Ptr{Int}, Ref{fqPolyRepMPolyRingElem}, Ref{fqPolyRepMPolyRing}),
         degs, a, a.parent)
   return degs
end

# Return an array of the max degrees as fmpzs in each variable
function degrees_fmpz(a::fqPolyRepMPolyRingElem)
   n = nvars(parent(a))
   degs = Vector{ZZRingElem}(undef, n)
   for i in 1:n
      degs[i] = ZZRingElem()
   end
   ccall((:fq_nmod_mpoly_degrees_fmpz, libflint), Nothing,
         (Ptr{Ref{ZZRingElem}}, Ref{fqPolyRepMPolyRingElem}, Ref{fqPolyRepMPolyRing}),
         degs, a, a.parent)
   return degs
end

# Return true if degree fits into an Int
function total_degree_fits_int(a::fqPolyRepMPolyRingElem)
   b = ccall((:fq_nmod_mpoly_total_degree_fits_si, libflint), Cint,
             (Ref{fqPolyRepMPolyRingElem}, Ref{fqPolyRepMPolyRing}),
             a, a.parent)
   return Bool(b)
end

# Total degree as an Int
function total_degree(a::fqPolyRepMPolyRingElem)
   if !total_degree_fits_int(a)
      throw(OverflowError("Total degree of polynomial does not fit into Int"))
   end
   d = ccall((:fq_nmod_mpoly_total_degree_si, libflint), Int,
             (Ref{fqPolyRepMPolyRingElem}, Ref{fqPolyRepMPolyRing}),
             a, a.parent)
   return d
end

# Total degree as an ZZRingElem
function total_degree_fmpz(a::fqPolyRepMPolyRingElem)
   d = ZZRingElem()
   ccall((:fq_nmod_mpoly_total_degree_fmpz, libflint), Nothing,
         (Ref{ZZRingElem}, Ref{fqPolyRepMPolyRingElem}, Ref{fqPolyRepMPolyRing}),
         d, a, a.parent)
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
   ccall((:fq_nmod_mpoly_get_coeff_vars_ui, libflint), Nothing,
         (Ref{fqPolyRepMPolyRingElem}, Ref{fqPolyRepMPolyRingElem}, Ptr{Int},
          Ptr{Int}, Int, Ref{fqPolyRepMPolyRing}),
         z, a, vars, exps, length(vars), a.parent)
   return z
end

###############################################################################
#
#   Basic arithmetic
#
###############################################################################

function -(a::fqPolyRepMPolyRingElem)
   z = parent(a)()
   ccall((:fq_nmod_mpoly_neg, libflint), Nothing,
         (Ref{fqPolyRepMPolyRingElem}, Ref{fqPolyRepMPolyRingElem}, Ref{fqPolyRepMPolyRing}),
         z, a, a.parent)
   return z
end

function +(a::fqPolyRepMPolyRingElem, b::fqPolyRepMPolyRingElem)
   check_parent(a, b)
   z = parent(a)()
   ccall((:fq_nmod_mpoly_add, libflint), Nothing,
         (Ref{fqPolyRepMPolyRingElem}, Ref{fqPolyRepMPolyRingElem},
          Ref{fqPolyRepMPolyRingElem}, Ref{fqPolyRepMPolyRing}),
         z, a, b, a.parent)
   return z
end

function -(a::fqPolyRepMPolyRingElem, b::fqPolyRepMPolyRingElem)
   check_parent(a, b)
   z = parent(a)()
   ccall((:fq_nmod_mpoly_sub, libflint), Nothing,
         (Ref{fqPolyRepMPolyRingElem}, Ref{fqPolyRepMPolyRingElem},
          Ref{fqPolyRepMPolyRingElem}, Ref{fqPolyRepMPolyRing}),
         z, a, b, a.parent)
   return z
end

function *(a::fqPolyRepMPolyRingElem, b::fqPolyRepMPolyRingElem)
   check_parent(a, b)
   z = parent(a)()
   ccall((:fq_nmod_mpoly_mul, libflint), Nothing,
         (Ref{fqPolyRepMPolyRingElem}, Ref{fqPolyRepMPolyRingElem},
          Ref{fqPolyRepMPolyRingElem}, Ref{fqPolyRepMPolyRing}),
         z, a, b, a.parent)
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
   r = ccall((:fq_nmod_mpoly_pow_ui, libflint), Cint,
             (Ref{fqPolyRepMPolyRingElem}, Ref{fqPolyRepMPolyRingElem}, UInt, Ref{fqPolyRepMPolyRing}),
             z, a, UInt(b), parent(a))
   iszero(r) && error("Unable to compute power")
   return z
end

function ^(a::fqPolyRepMPolyRingElem, b::ZZRingElem)
   b < 0 && throw(DomainError(b, "Exponent must be non-negative"))
   z = parent(a)()
   r = ccall((:fq_nmod_mpoly_pow_fmpz, libflint), Cint,
             (Ref{fqPolyRepMPolyRingElem}, Ref{fqPolyRepMPolyRingElem},
              Ref{ZZRingElem}, Ref{fqPolyRepMPolyRing}),
             z, a, b, parent(a))
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
   r = ccall((:fq_nmod_mpoly_gcd, libflint), Cint,
             (Ref{fqPolyRepMPolyRingElem}, Ref{fqPolyRepMPolyRingElem},
              Ref{fqPolyRepMPolyRingElem}, Ref{fqPolyRepMPolyRing}),
             z, a, b, a.parent)
   r == 0 && error("Unable to compute gcd")
   return z
end

function gcd_with_cofactors(a::fqPolyRepMPolyRingElem, b::fqPolyRepMPolyRingElem)
   check_parent(a, b)
   z = parent(a)()
   abar = parent(a)()
   bbar = parent(a)()
   r = ccall((:fq_nmod_mpoly_gcd_cofactors, Nemo.libflint), Cint,
             (Ref{fqPolyRepMPolyRingElem}, Ref{fqPolyRepMPolyRingElem}, Ref{fqPolyRepMPolyRingElem},
              Ref{fqPolyRepMPolyRingElem}, Ref{fqPolyRepMPolyRingElem}, Ref{fqPolyRepMPolyRing}),
             z, abar, bbar, a, b, a.parent)
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
         ccall((:fq_nmod_mpoly_factor_get_base, libflint), Nothing,
               (Ref{fqPolyRepMPolyRingElem}, Ref{fq_nmod_mpoly_factor}, Int, Ref{fqPolyRepMPolyRing}),
               f, fac, i, R)
      else
         ccall((:fq_nmod_mpoly_factor_swap_base, libflint), Nothing,
               (Ref{fqPolyRepMPolyRingElem}, Ref{fq_nmod_mpoly_factor}, Int, Ref{fqPolyRepMPolyRing}),
               f, fac, i, R)
      end
      F.fac[f] = ccall((:fq_nmod_mpoly_factor_get_exp_si, libflint), Int,
                       (Ref{fq_nmod_mpoly_factor}, Int, Ref{fqPolyRepMPolyRing}),
                       fac, i, R)
   end
   c = base_ring(R)()
   ccall((:fq_nmod_mpoly_factor_get_constant_fq_nmod, libflint), Nothing,
         (Ref{fqPolyRepFieldElem}, Ref{fq_nmod_mpoly_factor}),
         c, fac)
   F.unit = R(c)
   return F
end

function factor(a::fqPolyRepMPolyRingElem)
   iszero(a) && throw(ArgumentError("Argument must be non-zero"))
   R = parent(a)
   fac = fq_nmod_mpoly_factor(R)
   ok = ccall((:fq_nmod_mpoly_factor, libflint), Cint,
              (Ref{fq_nmod_mpoly_factor}, Ref{fqPolyRepMPolyRingElem}, Ref{fqPolyRepMPolyRing}),
              fac, a, R)
   ok == 0 && error("unable to compute factorization")
   return Fac{fqPolyRepMPolyRingElem}(fac, false)
end

function factor_squarefree(a::fqPolyRepMPolyRingElem)
   iszero(a) && throw(ArgumentError("Argument must be non-zero"))
   R = parent(a)
   fac = fq_nmod_mpoly_factor(R)
   ok = ccall((:fq_nmod_mpoly_factor_squarefree, libflint), Cint,
              (Ref{fq_nmod_mpoly_factor}, Ref{fqPolyRepMPolyRingElem}, Ref{fqPolyRepMPolyRing}),
              fac, a, R)
   ok == 0 && error("unable to compute factorization")
   return Fac{fqPolyRepMPolyRingElem}(fac, false)
end


function sqrt(a::fqPolyRepMPolyRingElem; check::Bool=true)
   (flag, q) = is_square_with_sqrt(a)
   check && !flag && error("Not a square")
   return q
end

function is_square(a::fqPolyRepMPolyRingElem)
   return Bool(ccall((:fq_nmod_mpoly_is_square, libflint), Cint,
                     (Ref{fqPolyRepMPolyRingElem}, Ref{fqPolyRepMPolyRing}),
                     a, a.parent))
end

function is_square_with_sqrt(a::fqPolyRepMPolyRingElem)
   q = parent(a)()
   flag = ccall((:fq_nmod_mpoly_sqrt, libflint), Cint,
                (Ref{fqPolyRepMPolyRingElem}, Ref{fqPolyRepMPolyRingElem}, Ref{fqPolyRepMPolyRing}),
                q, a, a.parent)
   return (Bool(flag), q)
end

###############################################################################
#
#   Comparison
#
###############################################################################

function ==(a::fqPolyRepMPolyRingElem, b::fqPolyRepMPolyRingElem)
   check_parent(a, b)
   return ccall((:fq_nmod_mpoly_equal, libflint), Cint,
                (Ref{fqPolyRepMPolyRingElem}, Ref{fqPolyRepMPolyRingElem}, Ref{fqPolyRepMPolyRing}),
                a, b, a.parent) != 0
end

function Base.isless(a::fqPolyRepMPolyRingElem, b::fqPolyRepMPolyRingElem)
   (!is_monomial(a) || !is_monomial(b)) && error("Not monomials in comparison")
   return ccall((:fq_nmod_mpoly_cmp, libflint), Cint,
                (Ref{fqPolyRepMPolyRingElem}, Ref{fqPolyRepMPolyRingElem}, Ref{fqPolyRepMPolyRing}),
                a, b, a.parent) < 0
end

###############################################################################
#
#   Ad hoc comparison
#
###############################################################################

function ==(a::fqPolyRepMPolyRingElem, b::fqPolyRepFieldElem)
   return Bool(ccall((:fq_nmod_mpoly_equal_fq_nmod, libflint), Cint,
                     (Ref{fqPolyRepMPolyRingElem}, Ref{fqPolyRepFieldElem}, Ref{fqPolyRepMPolyRing}),
                     a, b, a.parent))
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
   d = ccall((:fq_nmod_mpoly_divides, libflint), Cint,
             (Ref{fqPolyRepMPolyRingElem}, Ref{fqPolyRepMPolyRingElem},
              Ref{fqPolyRepMPolyRingElem}, Ref{fqPolyRepMPolyRing}),
             z, a, b, a.parent)
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
   ccall((:fq_nmod_mpoly_div, libflint), Nothing,
         (Ref{fqPolyRepMPolyRingElem}, Ref{fqPolyRepMPolyRingElem},
          Ref{fqPolyRepMPolyRingElem}, Ref{fqPolyRepMPolyRing}),
         q, a, b, a.parent)
   return q
end

function Base.divrem(a::fqPolyRepMPolyRingElem, b::fqPolyRepMPolyRingElem)
   check_parent(a, b)
   q = parent(a)()
   r = parent(a)()
   ccall((:fq_nmod_mpoly_divrem, libflint), Nothing,
         (Ref{fqPolyRepMPolyRingElem}, Ref{fqPolyRepMPolyRingElem}, Ref{fqPolyRepMPolyRingElem},
          Ref{fqPolyRepMPolyRingElem}, Ref{fqPolyRepMPolyRing}),
         q, r, a, b, a.parent)
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
   ccall((:fq_nmod_mpoly_divrem_ideal, libflint), Nothing,
         (Ptr{Ref{fqPolyRepMPolyRingElem}}, Ref{fqPolyRepMPolyRingElem}, Ref{fqPolyRepMPolyRingElem},
          Ptr{Ref{fqPolyRepMPolyRingElem}}, Int, Ref{fqPolyRepMPolyRing}),
         q, r, a, b, len, a.parent)
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
   ccall((:fq_nmod_mpoly_derivative, libflint), Nothing,
         (Ref{fqPolyRepMPolyRingElem}, Ref{fqPolyRepMPolyRingElem}, Int, Ref{fqPolyRepMPolyRing}),
         z, a, i - 1, parent(a))
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
   ccall((:fq_nmod_mpoly_evaluate_all_fq_nmod, libflint), Nothing,
         (Ref{fqPolyRepFieldElem}, Ref{fqPolyRepMPolyRingElem}, Ptr{Ref{fqPolyRepFieldElem}}, Ref{fqPolyRepMPolyRing}),
         z, a, b, parent(a))
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
   fl = ccall((:fq_nmod_mpoly_compose_fq_nmod_mpoly, libflint), Cint,
              (Ref{fqPolyRepMPolyRingElem}, Ref{fqPolyRepMPolyRingElem}, Ptr{Ref{fqPolyRepMPolyRingElem}},
               Ref{fqPolyRepMPolyRing}, Ref{fqPolyRepMPolyRing}),
              c, a, bs, R, S)
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
   fl = ccall((:fq_nmod_mpoly_compose_fq_nmod_poly, libflint), Cint,
              (Ref{fqPolyRepPolyRingElem}, Ref{fqPolyRepMPolyRingElem}, Ptr{Ref{fqPolyRepPolyRingElem}},
               Ref{fqPolyRepMPolyRing}), c, a, bs, R)
   fl == 0 && error("Something wrong in evaluation.")
   return c
end

###############################################################################
#
#   Unsafe functions
#
###############################################################################

function zero!(a::fqPolyRepMPolyRingElem)
    ccall((:fq_nmod_mpoly_zero, libflint), Nothing,
          (Ref{fqPolyRepMPolyRingElem}, Ref{fqPolyRepMPolyRing}),
          a, a.parent)
    return a
end

function add!(a::fqPolyRepMPolyRingElem, b::fqPolyRepMPolyRingElem, c::fqPolyRepMPolyRingElem)
   ccall((:fq_nmod_mpoly_add, libflint), Nothing,
         (Ref{fqPolyRepMPolyRingElem}, Ref{fqPolyRepMPolyRingElem},
          Ref{fqPolyRepMPolyRingElem}, Ref{fqPolyRepMPolyRing}),
         a, b, c, a.parent)
   return a
end

function addeq!(a::fqPolyRepMPolyRingElem, b::fqPolyRepMPolyRingElem)
   ccall((:fq_nmod_mpoly_add, libflint), Nothing,
         (Ref{fqPolyRepMPolyRingElem}, Ref{fqPolyRepMPolyRingElem},
          Ref{fqPolyRepMPolyRingElem}, Ref{fqPolyRepMPolyRing}),
         a, a, b, a.parent)
   return a
end

function mul!(a::fqPolyRepMPolyRingElem, b::fqPolyRepMPolyRingElem, c::fqPolyRepMPolyRingElem)
   ccall((:fq_nmod_mpoly_mul, libflint), Nothing,
         (Ref{fqPolyRepMPolyRingElem}, Ref{fqPolyRepMPolyRingElem},
          Ref{fqPolyRepMPolyRingElem}, Ref{fqPolyRepMPolyRing}),
         a, b, c, a.parent)
   return a
end

# Set the n-th coefficient of a to c. If zero coefficients are inserted, they
# must be removed with combine_like_terms!
function setcoeff!(a::fqPolyRepMPolyRingElem, n::Int, c::fqPolyRepFieldElem)
   if n > length(a)
      ccall((:fq_nmod_mpoly_resize, libflint), Nothing,
            (Ref{fqPolyRepMPolyRingElem}, Int, Ref{fqPolyRepMPolyRing}), a, n, a.parent)
   end
   ccall((:fq_nmod_mpoly_set_term_coeff_fq_nmod, libflint), Nothing,
         (Ref{fqPolyRepMPolyRingElem}, Int, Ref{fqPolyRepFieldElem}, Ref{fqPolyRepMPolyRing}),
         a, n - 1, c, a.parent)
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
   ccall((:fq_nmod_mpoly_combine_like_terms, libflint), Nothing,
         (Ref{fqPolyRepMPolyRingElem}, Ref{fqPolyRepMPolyRing}),
         a, a.parent)
   return a
end

###############################################################################
#
#   Manipulating terms and monomials
#
###############################################################################

function exponent_vector_fits(::Type{Int}, a::fqPolyRepMPolyRingElem, i::Int)
   b = ccall((:fq_nmod_mpoly_term_exp_fits_si, libflint), Cint,
             (Ref{fqPolyRepMPolyRingElem}, Int, Ref{fqPolyRepMPolyRing}),
             a, i - 1, a.parent)
   return Bool(b)
end

function exponent_vector_fits(::Type{UInt}, a::fqPolyRepMPolyRingElem, i::Int)
   b = ccall((:fq_nmod_mpoly_term_exp_fits_ui, libflint), Cint,
             (Ref{fqPolyRepMPolyRingElem}, Int, Ref{fqPolyRepMPolyRing}),
             a, i - 1, a.parent)
   return Bool(b)
end

function exponent_vector!(z::Vector{Int}, a::fqPolyRepMPolyRingElem, i::Int)
   ccall((:fq_nmod_mpoly_get_term_exp_si, libflint), Nothing,
         (Ptr{Int}, Ref{fqPolyRepMPolyRingElem}, Int, Ref{fqPolyRepMPolyRing}),
         z, a, i - 1, parent(a))
   return z
end

function exponent_vector!(z::Vector{UInt}, a::fqPolyRepMPolyRingElem, i::Int)
   ccall((:fq_nmod_mpoly_get_term_exp_ui, libflint), Nothing,
         (Ptr{UInt}, Ref{fqPolyRepMPolyRingElem}, Int, Ref{fqPolyRepMPolyRing}),
         z, a, i - 1, parent(a))
   return z
end

function exponent_vector!(z::Vector{ZZRingElem}, a::fqPolyRepMPolyRingElem, i::Int)
   ccall((:fq_nmod_mpoly_get_term_exp_fmpz, libflint), Nothing,
         (Ptr{Ref{ZZRingElem}}, Ref{fqPolyRepMPolyRingElem}, Int, Ref{fqPolyRepMPolyRing}),
         z, a, i - 1, parent(a))
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
      ccall((:fq_nmod_mpoly_resize, libflint), Nothing,
            (Ref{fqPolyRepMPolyRingElem}, Int, Ref{fqPolyRepMPolyRing}), a, n, a.parent)
   end
   ccall((:fq_nmod_mpoly_set_term_exp_ui, libflint), Nothing,
         (Ref{fqPolyRepMPolyRingElem}, Int, Ptr{UInt}, Ref{fqPolyRepMPolyRing}),
         a, n - 1, exps, parent(a))
   return a
end

# Set exponent of n-th term to given vector of Int's
# No sort is performed, so this is unsafe. The Int's must be positive, but
# no check is performed
function set_exponent_vector!(a::fqPolyRepMPolyRingElem, n::Int, exps::Vector{Int})
   if n > length(a)
      ccall((:fq_nmod_mpoly_resize, libflint), Nothing,
            (Ref{fqPolyRepMPolyRingElem}, Int, Ref{fqPolyRepMPolyRing}), a, n, a.parent)
   end
   ccall((:fq_nmod_mpoly_set_term_exp_ui, libflint), Nothing,
         (Ref{fqPolyRepMPolyRingElem}, Int, Ptr{Int}, Ref{fqPolyRepMPolyRing}),
         a, n - 1, exps, parent(a))
   return a
end

# Set exponent of n-th term to given vector of ZZRingElem's
# No sort is performed, so this is unsafe
function set_exponent_vector!(a::fqPolyRepMPolyRingElem, n::Int, exps::Vector{ZZRingElem})
   if n > length(a)
      ccall((:fq_nmod_mpoly_resize, libflint), Nothing,
            (Ref{fqPolyRepMPolyRingElem}, Int, Ref{fqPolyRepMPolyRing}), a, n, a.parent)
   end
   ccall((:fq_nmod_mpoly_set_term_exp_fmpz, libflint), Nothing,
         (Ref{fqPolyRepMPolyRingElem}, Int, Ptr{ZZRingElem}, Ref{fqPolyRepMPolyRing}),
         a, n - 1, exps, parent(a))
   return a
end

# Return j-th coordinate of i-th exponent vector
function exponent(a::fqPolyRepMPolyRingElem, i::Int, j::Int)
   (j < 1 || j > nvars(parent(a))) && error("Invalid variable index")
   return ccall((:fq_nmod_mpoly_get_term_var_exp_ui, libflint), Int,
                (Ref{fqPolyRepMPolyRingElem}, Int, Int, Ref{fqPolyRepMPolyRing}),
                 a, i - 1, j - 1, a.parent)
end

# Return the coefficient of the term with the given exponent vector
# Return zero if there is no such term
function coeff(a::fqPolyRepMPolyRingElem, exps::Vector{UInt})
   z = base_ring(parent(a))()
   ccall((:fq_nmod_mpoly_get_coeff_fq_nmod_ui, libflint), UInt,
         (Ref{fqPolyRepFieldElem}, Ref{fqPolyRepMPolyRingElem}, Ptr{UInt}, Ref{fqPolyRepMPolyRing}),
         z, a, exps, parent(a))
   return z
end

# Return the coefficient of the term with the given exponent vector
# Return zero if there is no such term
function coeff(a::fqPolyRepMPolyRingElem, exps::Vector{Int})
   z = base_ring(parent(a))()
   ccall((:fq_nmod_mpoly_get_coeff_fq_nmod_ui, libflint), UInt,
         (Ref{fqPolyRepFieldElem}, Ref{fqPolyRepMPolyRingElem}, Ptr{Int}, Ref{fqPolyRepMPolyRing}),
         z, a, exps, parent(a))
   return z
end

# Set the coefficient of the term with the given exponent vector to the
# given ZZRingElem. Removal of a zero term is performed.
function setcoeff!(a::fqPolyRepMPolyRingElem, exps::Vector{Int}, b::fqPolyRepFieldElem)
   ccall((:fq_nmod_mpoly_set_coeff_fq_nmod_ui, libflint), Nothing,
         (Ref{fqPolyRepMPolyRingElem}, UInt, Ptr{Int}, Ref{fqPolyRepMPolyRing}),
         a, b, exps, parent(a))
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
   ccall((:fq_nmod_mpoly_sort_terms, libflint), Nothing,
         (Ref{fqPolyRepMPolyRingElem}, Ref{fqPolyRepMPolyRing}), a, a.parent)
   return a
end

# Return the i-th term of the polynomial, as a polynomial
function term(a::fqPolyRepMPolyRingElem, i::Int)
   z = parent(a)()
   ccall((:fq_nmod_mpoly_get_term, libflint), Nothing,
         (Ref{fqPolyRepMPolyRingElem}, Ref{fqPolyRepMPolyRingElem}, Int, Ref{fqPolyRepMPolyRing}),
          z, a, i - 1, a.parent)
   return z
end

# Return the i-th monomial of the polynomial, as a polynomial
function monomial(a::fqPolyRepMPolyRingElem, i::Int)
   z = parent(a)()
   ccall((:fq_nmod_mpoly_get_term_monomial, libflint), Nothing,
         (Ref{fqPolyRepMPolyRingElem}, Ref{fqPolyRepMPolyRingElem}, Int, Ref{fqPolyRepMPolyRing}),
          z, a, i - 1, a.parent)
   return z
end

# Sets the given polynomial m to the i-th monomial of the polynomial
function monomial!(m::fqPolyRepMPolyRingElem, a::fqPolyRepMPolyRingElem, i::Int)
   ccall((:fq_nmod_mpoly_get_term_monomial, libflint), Nothing,
         (Ref{fqPolyRepMPolyRingElem}, Ref{fqPolyRepMPolyRingElem}, Int, Ref{fqPolyRepMPolyRing}),
          m, a, i - 1, a.parent)
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
  ccall((:fq_nmod_mpoly_push_term_fq_nmod_ui, libflint), Nothing,
        (Ref{fqPolyRepMPolyRingElem}, Ref{fqPolyRepFieldElem}, Ptr{UInt}, Ref{fqPolyRepMPolyRing}),
        z, c, exp, parent(z))
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
  ccall((:fq_nmod_mpoly_sort_terms, libflint), Nothing,
        (Ref{fqPolyRepMPolyRingElem}, Ref{fqPolyRepMPolyRing}), res, R)
  ccall((:fq_nmod_mpoly_combine_like_terms, libflint), Nothing,
        (Ref{fqPolyRepMPolyRingElem}, Ref{fqPolyRepMPolyRing}), res, R)
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
   newb = map(x -> map(FlintZZ, x), b)
   newaa = convert(Vector{fqPolyRepFieldElem}, newa)
   newbb = convert(Vector{Vector{ZZRingElem}}, newb)

   for i in 1:length(newbb)
      length(newbb[i]) != n && error("Exponent vector $i has length $(length(newbb[i])) (expected $(nvars(R)))")
   end

   return R(newaa, newbb)
end
