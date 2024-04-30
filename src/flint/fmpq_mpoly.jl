###############################################################################
#
#   QQMPolyRingElem.jl : Flint multivariate polynomials over QQFieldElem
#
###############################################################################

###############################################################################
#
#   Data type and parent object methods
#
###############################################################################

parent_type(::Type{QQMPolyRingElem}) = QQMPolyRing

elem_type(::Type{QQMPolyRing}) = QQMPolyRingElem

mpoly_type(::Type{QQFieldElem}) = QQMPolyRingElem

symbols(a::QQMPolyRing) = a.S

parent(a::QQMPolyRingElem) = a.parent

function check_parent(a::QQMPolyRingElem, b::QQMPolyRingElem)
   parent(a) != parent(b) &&
      error("Incompatible polynomial rings in polynomial operation")
end

number_of_variables(a::QQMPolyRing) = ccall((:fmpq_mpoly_ctx_nvars, libflint), Int,
                                (Ref{QQMPolyRing}, ), a)

base_ring(a::QQMPolyRing) = QQ

function internal_ordering(a::QQMPolyRing)
   b = ccall((:fmpq_mpoly_ctx_ord, libflint), Cint, (Ref{QQMPolyRing}, ), a)
   return flint_orderings[b + 1]
end

function gens(R::QQMPolyRing)
   A = Vector{QQMPolyRingElem}(undef, R.nvars)
   for i = 1:R.nvars
      z = R()
      ccall((:fmpq_mpoly_gen, libflint), Nothing,
            (Ref{QQMPolyRingElem}, Int, Ref{QQMPolyRing}), z, i - 1, R)
      A[i] = z
   end
   return A
end

function gen(R::QQMPolyRing, i::Int)
   n = nvars(R)
   (i <= 0 || i > n) && error("Index must be between 1 and $n")
   z = R()
   ccall((:fmpq_mpoly_gen, libflint), Nothing,
         (Ref{QQMPolyRingElem}, Int, Ref{QQMPolyRing}), z, i - 1, R)
   return z
end

function is_gen(a::QQMPolyRingElem, i::Int)
   n = nvars(parent(a))
   (i <= 0 || i > n) && error("Index must be between 1 and $n")
   R = parent(a)
   return Bool(ccall((:fmpq_mpoly_is_gen, libflint), Cint,
                     (Ref{QQMPolyRingElem}, Int, Ref{QQMPolyRing}),
                     a, i - 1, a.parent))
end

function is_gen(a::QQMPolyRingElem)
   n = nvars(parent(a))
   for i in 1:n
      is_gen(a, i) && return true
   end
   return false
end

function deepcopy_internal(a::QQMPolyRingElem, dict::IdDict)
   z = parent(a)()
   ccall((:fmpq_mpoly_set, libflint), Nothing,
         (Ref{QQMPolyRingElem}, Ref{QQMPolyRingElem}, Ref{QQMPolyRing}),
         z, a, a.parent)
   return z
end

function length(a::QQMPolyRingElem)
   n = ccall((:fmpq_mpoly_length, libflint), Int, (Ref{QQMPolyRingElem}, ), a)
   return n
end

function one(R::QQMPolyRing)
   z = R()
   ccall((:fmpq_mpoly_one, libflint), Nothing,
         (Ref{QQMPolyRingElem}, Ref{QQMPolyRing}), z, R)
   return z
end

function zero(R::QQMPolyRing)
   z = R()
   ccall((:fmpq_mpoly_zero, libflint), Nothing,
         (Ref{QQMPolyRingElem}, Ref{QQMPolyRing}), z, R)
   return z
end

function isone(a::QQMPolyRingElem)
   b = ccall((:fmpq_mpoly_is_one, libflint), Cint,
             (Ref{QQMPolyRingElem}, Ref{QQMPolyRing}), a, a.parent)
   return Bool(b)
end

function iszero(a::QQMPolyRingElem)
   b = ccall((:fmpq_mpoly_is_zero, libflint), Cint,
             (Ref{QQMPolyRingElem}, Ref{QQMPolyRing}), a, a.parent)
   return Bool(b)
end

function is_monomial(a::QQMPolyRingElem)
   return length(a) == 1 && coeff(a, 1) == 1
end

function is_term(a::QQMPolyRingElem)
   return length(a) == 1
end

function is_unit(a::QQMPolyRingElem)
   return length(a) == 1 && total_degree(a) == 0 && is_unit(coeff(a, 1))
end

function is_constant(a::QQMPolyRingElem)
   b = ccall((:fmpq_mpoly_is_fmpq, libflint), Cint,
             (Ref{QQMPolyRingElem}, Ref{QQMPolyRing}), a, parent(a))
   return Bool(b)
end

function content(a::QQMPolyRingElem)
  c = QQFieldElem()
  ccall((:fmpq_mpoly_content, libflint), Nothing,
        (Ref{QQFieldElem}, Ref{QQMPolyRingElem}, Ref{QQMPolyRing}), c, a, parent(a))
  return c
end

function denominator(a::QQMPolyRingElem)
  c = ZZRingElem()
  ccall((:fmpq_mpoly_get_denominator, libflint), Nothing,
        (Ref{ZZRingElem}, Ref{QQMPolyRingElem}, Ref{QQMPolyRing}), c, a, parent(a))
  return c
end

################################################################################
#
#  Getting coefficients
#
################################################################################

function coeff(a::QQMPolyRingElem, i::Int)
   z = QQFieldElem()
   n = length(a)
   (i < 1 || i > n) && error("Index must be between 1 and $(length(a))")
   ccall((:fmpq_mpoly_get_term_coeff_fmpq, libflint), Nothing,
         (Ref{QQFieldElem}, Ref{QQMPolyRingElem}, Int, Ref{QQMPolyRing}),
         z, a, i - 1, a.parent)
   return z
end

function coeff(a::QQMPolyRingElem, b::QQMPolyRingElem)
   check_parent(a, b)
   !isone(length(b)) && error("Second argument must be a monomial")
   z = QQFieldElem()
   ccall((:fmpq_mpoly_get_coeff_fmpq_monomial, libflint), Nothing,
         (Ref{QQFieldElem}, Ref{QQMPolyRingElem}, Ref{QQMPolyRingElem}, Ref{QQMPolyRing}),
         z, a, b, parent(a))
   return z
end

function trailing_coefficient(p::QQMPolyRingElem)
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
function degree(a::QQMPolyRingElem, i::Int)
   n = nvars(parent(a))
   (i <= 0 || i > n) && error("Index must be between 1 and $n")
   if degrees_fit_int(a)
      d = ccall((:fmpq_mpoly_degree_si, libflint), Int,
                (Ref{QQMPolyRingElem}, Int, Ref{QQMPolyRing}), a, i - 1, a.parent)
      return d
   else
      return Int(degree_fmpz(a, i))
   end
end

# Degree in the i-th variable as an ZZRingElem
function degree_fmpz(a::QQMPolyRingElem, i::Int)
   n = nvars(parent(a))
   (i <= 0 || i > n) && error("Index must be between 1 and $n")
   d = ZZRingElem()
   ccall((:fmpq_mpoly_degree_fmpz, libflint), Nothing,
         (Ref{ZZRingElem}, Ref{QQMPolyRingElem}, Int, Ref{QQMPolyRing}),
         d, a, i - 1, a.parent)
   return d
end

# Return true if degrees fit into an Int
function degrees_fit_int(a::QQMPolyRingElem)
   b = ccall((:fmpq_mpoly_degrees_fit_si, libflint), Cint,
             (Ref{QQMPolyRingElem}, Ref{QQMPolyRing}), a, a.parent)
   return Bool(b)
end

# Return an array of the max degrees in each variable
function degrees(a::QQMPolyRingElem)
   if !degrees_fit_int(a)
      throw(OverflowError("degrees of polynomial do not fit into Int"))
   end
   degs = Vector{Int}(undef, nvars(parent(a)))
   ccall((:fmpq_mpoly_degrees_si, libflint), Nothing,
         (Ptr{Int}, Ref{QQMPolyRingElem}, Ref{QQMPolyRing}),
         degs, a, a.parent)
   return degs
end

# Return an array of the max degrees as fmpzs in each variable
function degrees_fmpz(a::QQMPolyRingElem)
   n = nvars(parent(a))
   degs = Vector{ZZRingElem}(undef, n)
   for i in 1:n
      degs[i] = ZZRingElem()
   end
   ccall((:fmpq_mpoly_degrees_fmpz, libflint), Nothing,
         (Ptr{Ref{ZZRingElem}}, Ref{QQMPolyRingElem}, Ref{QQMPolyRing}),
         degs, a, a.parent)
   return degs
end

# Return true if degree fits into an Int
function total_degree_fits_int(a::QQMPolyRingElem)
      b = ccall((:fmpq_mpoly_total_degree_fits_si, libflint), Cint,
                (Ref{QQMPolyRingElem}, Ref{QQMPolyRing}), a, a.parent)
      return Bool(b)
   end

# Total degree as an Int
function total_degree(a::QQMPolyRingElem)
   if !total_degree_fits_int(a)
      throw(OverflowError("Total degree of polynomial does not fit into Int"))
   end
   d = ccall((:fmpq_mpoly_total_degree_si, libflint), Int,
             (Ref{QQMPolyRingElem}, Ref{QQMPolyRing}), a, a.parent)
   return d
end

# Total degree as an ZZRingElem
function total_degree_fmpz(a::QQMPolyRingElem)
   d = ZZRingElem()
   ccall((:fmpq_mpoly_total_degree_fmpz, libflint), Nothing,
         (Ref{ZZRingElem}, Ref{QQMPolyRingElem}, Ref{QQMPolyRing}),
            d, a, a.parent)
   return d
end

###############################################################################
#
#   Multivariable coefficient polynomials
#
###############################################################################

function coeff(a::QQMPolyRingElem, vars::Vector{Int}, exps::Vector{Int})
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
   ccall((:fmpq_mpoly_get_coeff_vars_ui, libflint), Nothing,
         (Ref{QQMPolyRingElem}, Ref{QQMPolyRingElem}, Ptr{Int},
          Ptr{Int}, Int, Ref{QQMPolyRing}),
          z, a, vars, exps, length(vars), a.parent)
   return z
end

###############################################################################
#
#   String I/O
#
###############################################################################

# handled by AbstractAlgebra fallback

###############################################################################
#
#   Basic arithmetic
#
###############################################################################

function -(a::QQMPolyRingElem)
   z = parent(a)()
   ccall((:fmpq_mpoly_neg, libflint), Nothing,
       (Ref{QQMPolyRingElem}, Ref{QQMPolyRingElem}, Ref{QQMPolyRing}),
       z, a, a.parent)
   return z
end

function +(a::QQMPolyRingElem, b::QQMPolyRingElem)
   check_parent(a, b)
   z = parent(a)()
   ccall((:fmpq_mpoly_add, libflint), Nothing,
       (Ref{QQMPolyRingElem}, Ref{QQMPolyRingElem}, Ref{QQMPolyRingElem}, Ref{QQMPolyRing}),
       z, a, b, a.parent)
   return z
end

function -(a::QQMPolyRingElem, b::QQMPolyRingElem)
   check_parent(a, b)
   z = parent(a)()
   ccall((:fmpq_mpoly_sub, libflint), Nothing,
       (Ref{QQMPolyRingElem}, Ref{QQMPolyRingElem}, Ref{QQMPolyRingElem}, Ref{QQMPolyRing}),
       z, a, b, a.parent)
   return z
end

function *(a::QQMPolyRingElem, b::QQMPolyRingElem)
   check_parent(a, b)
   z = parent(a)()
   ccall((:fmpq_mpoly_mul, libflint), Nothing,
       (Ref{QQMPolyRingElem}, Ref{QQMPolyRingElem}, Ref{QQMPolyRingElem}, Ref{QQMPolyRing}),
       z, a, b, a.parent)
   return z
end

###############################################################################
#
#   Ad hoc arithmetic
#
###############################################################################

for (jT, cN, cT) in ((QQFieldElem, :fmpq, Ref{QQFieldElem}), (ZZRingElem, :fmpz, Ref{ZZRingElem}),
                     (Int, :si, Int))
   @eval begin
      function +(a::QQMPolyRingElem, b::($jT))
         z = parent(a)()
         ccall(($(string(:fmpq_mpoly_add_, cN)), libflint), Nothing,
               (Ref{QQMPolyRingElem}, Ref{QQMPolyRingElem}, ($cT), Ref{QQMPolyRing}),
               z, a, b, parent(a))
         return z
      end

      +(a::($jT), b::QQMPolyRingElem) = b + a

      function -(a::QQMPolyRingElem, b::($jT))
         z = parent(a)()
         ccall(($(string(:fmpq_mpoly_sub_, cN)), libflint), Nothing,
               (Ref{QQMPolyRingElem}, Ref{QQMPolyRingElem}, ($cT), Ref{QQMPolyRing}),
               z, a, b, parent(a))
         return z
      end

      -(a::($jT), b::QQMPolyRingElem) = - (b - a)

      function *(a::QQMPolyRingElem, b::($jT))
         z = parent(a)()
         ccall(($(string(:fmpq_mpoly_scalar_mul_, cN)), libflint), Nothing,
               (Ref{QQMPolyRingElem}, Ref{QQMPolyRingElem}, ($cT), Ref{QQMPolyRing}),
               z, a, b, parent(a))
         return z
      end

      *(a::($jT), b::QQMPolyRingElem) = b * a

      function divexact(a::QQMPolyRingElem, b::($jT); check::Bool=true)
         z = parent(a)()
         ccall(($(string(:fmpq_mpoly_scalar_div_, cN)), libflint), Nothing,
               (Ref{QQMPolyRingElem}, Ref{QQMPolyRingElem}, ($cT), Ref{QQMPolyRing}),
               z, a, b, parent(a))
         return z
      end

      //(a::QQMPolyRingElem, b::($jT)) = a//parent(a)(b)
   end
end

+(a::QQMPolyRingElem, b::Integer) = a + ZZRingElem(b)

+(a::Integer, b::QQMPolyRingElem) = b + a

-(a::QQMPolyRingElem, b::Integer) = a - ZZRingElem(b)

-(a::Integer, b::QQMPolyRingElem) = -(b - a)

+(a::QQMPolyRingElem, b::Rational{<:Integer}) = a + QQFieldElem(b)

+(a::Rational{<:Integer}, b::QQMPolyRingElem) = b + a

-(a::QQMPolyRingElem, b::Rational{<:Integer}) = a - QQFieldElem(b)

-(a::Rational{<:Integer}, b::QQMPolyRingElem) = -(b - a)

*(a::QQMPolyRingElem, b::Integer) = a * ZZRingElem(b)

*(a::Integer, b::QQMPolyRingElem) = b * a

*(a::QQMPolyRingElem, b::Rational{<:Integer}) = a * QQFieldElem(b)

*(a::Rational{<:Integer}, b::QQMPolyRingElem) = b * a

divexact(a::QQMPolyRingElem, b::Integer; check::Bool=true) = divexact(a, ZZRingElem(b); check=check)

divexact(a::QQMPolyRingElem, b::Rational{<:Integer}; check::Bool=true) = divexact(a, QQFieldElem(b); check=check)

//(a::QQMPolyRingElem, b::Integer) = //(a, ZZRingElem(b))

//(a::QQMPolyRingElem, b::Rational{<:Integer}) = //(a, QQFieldElem(b))

###############################################################################
#
#   Powering
#
###############################################################################

function ^(a::QQMPolyRingElem, b::Int)
   b < 0 && throw(DomainError(b, "Exponent must be non-negative"))
   z = parent(a)()
   ccall((:fmpq_mpoly_pow_ui, libflint), Nothing,
         (Ref{QQMPolyRingElem}, Ref{QQMPolyRingElem}, Int, Ref{QQMPolyRing}),
         z, a, b, parent(a))
   return z
end

function ^(a::QQMPolyRingElem, b::ZZRingElem)
   b < 0 && throw(DomainError(b, "Exponent must be non-negative"))
   z = parent(a)()
   ccall((:fmpq_mpoly_pow_fmpz, libflint), Nothing,
         (Ref{QQMPolyRingElem}, Ref{QQMPolyRingElem}, Ref{ZZRingElem}, Ref{QQMPolyRing}),
         z, a, b, parent(a))
   return z
end

################################################################################
#
#   GCD
#
################################################################################

function gcd(a::QQMPolyRingElem, b::QQMPolyRingElem)
   check_parent(a, b)
   z = parent(a)()
   r = ccall((:fmpq_mpoly_gcd, libflint), Cint,
         (Ref{QQMPolyRingElem}, Ref{QQMPolyRingElem}, Ref{QQMPolyRingElem}, Ref{QQMPolyRing}),
         z, a, b, a.parent)
   r == 0 && error("Unable to compute gcd")
   return z
end

function gcd_with_cofactors(a::QQMPolyRingElem, b::QQMPolyRingElem)
   z = parent(a)()
   abar = parent(a)()
   bbar = parent(a)()
   r = ccall((:fmpq_mpoly_gcd_cofactors, Nemo.libflint), Cint,
             (Ref{QQMPolyRingElem}, Ref{QQMPolyRingElem}, Ref{QQMPolyRingElem},
              Ref{QQMPolyRingElem}, Ref{QQMPolyRingElem}, Ref{QQMPolyRing}),
             z, abar, bbar, a, b, a.parent)
   r == 0 && error("Unable to compute gcd")
   return z, abar, bbar
end

################################################################################
#
#   Factorization and Square Root
#
################################################################################

function (::Type{Fac{QQMPolyRingElem}})(fac::fmpq_mpoly_factor, preserve_input::Bool = true)
   F = Fac{QQMPolyRingElem}()
   R = fac.parent
   for i in 0:fac.num-1
      f = R()
      if preserve_input
         ccall((:fmpq_mpoly_factor_get_base, libflint), Nothing,
               (Ref{QQMPolyRingElem}, Ref{fmpq_mpoly_factor}, Int, Ref{QQMPolyRing}),
               f, fac, i, R)
      else
         ccall((:fmpq_mpoly_factor_swap_base, libflint), Nothing,
               (Ref{QQMPolyRingElem}, Ref{fmpq_mpoly_factor}, Int, Ref{QQMPolyRing}),
               f, fac, i, R)
      end
      F.fac[f] = ccall((:fmpq_mpoly_factor_get_exp_si, libflint), Int,
                       (Ref{fmpq_mpoly_factor}, Int, Ref{QQMPolyRing}),
                       fac, i, R)
   end
   c = QQFieldElem()
   ccall((:fmpq_mpoly_factor_get_constant_fmpq, libflint), Nothing,
         (Ref{QQFieldElem}, Ref{fmpq_mpoly_factor}),
         c, fac)
   F.unit = R(c)
   return F
end

function factor(a::QQMPolyRingElem)
   iszero(a) && throw(ArgumentError("Argument must be non-zero"))
   R = parent(a)
   fac = fmpq_mpoly_factor(R)
   ok = ccall((:fmpq_mpoly_factor, libflint), Cint,
              (Ref{fmpq_mpoly_factor}, Ref{QQMPolyRingElem}, Ref{QQMPolyRing}),
              fac, a, R)
   ok == 0 && error("unable to compute factorization")
   return Fac{QQMPolyRingElem}(fac, false)
end

function factor_squarefree(a::QQMPolyRingElem)
   iszero(a) && throw(ArgumentError("Argument must be non-zero"))
   R = parent(a)
   fac = fmpq_mpoly_factor(R)
   ok = ccall((:fmpq_mpoly_factor_squarefree, libflint), Cint,
              (Ref{fmpq_mpoly_factor}, Ref{QQMPolyRingElem}, Ref{QQMPolyRing}),
              fac, a, R)
   ok == 0 && error("unable to compute factorization")
   return Fac{QQMPolyRingElem}(fac, false)
end


function sqrt(a::QQMPolyRingElem; check::Bool=true)
   (flag, q) = is_square_with_sqrt(a)
   check && !flag && error("Not a square")
   return q
end

function is_square(a::QQMPolyRingElem)
   return Bool(ccall((:fmpq_mpoly_is_square, libflint), Cint,
                     (Ref{QQMPolyRingElem}, Ref{QQMPolyRing}),
                     a, a.parent))
end

function is_square_with_sqrt(a::QQMPolyRingElem)
   q = parent(a)()
   flag = ccall((:fmpq_mpoly_sqrt, libflint), Cint,
                (Ref{QQMPolyRingElem}, Ref{QQMPolyRingElem}, Ref{QQMPolyRing}),
                q, a, a.parent)
   return (Bool(flag), q)
end

###############################################################################
#
#   Comparison
#
###############################################################################

function ==(a::QQMPolyRingElem, b::QQMPolyRingElem)
   check_parent(a, b)
   return Bool(ccall((:fmpq_mpoly_equal, libflint), Cint,
               (Ref{QQMPolyRingElem}, Ref{QQMPolyRingElem}, Ref{QQMPolyRing}),
               a, b, a.parent))
end

function Base.isless(a::QQMPolyRingElem, b::QQMPolyRingElem)
   (!is_monomial(a) || !is_monomial(b)) && error("Not monomials in comparison")
   return ccall((:fmpq_mpoly_cmp, libflint), Cint,
               (Ref{QQMPolyRingElem}, Ref{QQMPolyRingElem}, Ref{QQMPolyRing}),
               a, b, a.parent) < 0
end

###############################################################################
#
#   Ad hoc comparison
#
###############################################################################

function ==(a::QQMPolyRingElem, b::QQFieldElem)
   return Bool(ccall((:fmpq_mpoly_equal_fmpq, libflint), Cint,
                     (Ref{QQMPolyRingElem}, Ref{QQFieldElem}, Ref{QQMPolyRing}),
                     a, b, a.parent))
end

==(a::QQFieldElem, b::QQMPolyRingElem) = b == a

function ==(a::QQMPolyRingElem, b::ZZRingElem)
   return Bool(ccall((:fmpq_mpoly_equal_fmpz, libflint), Cint,
                     (Ref{QQMPolyRingElem}, Ref{ZZRingElem}, Ref{QQMPolyRing}),
                     a, b, a.parent))
end

==(a::ZZRingElem, b::QQMPolyRingElem) = b == a

function ==(a::QQMPolyRingElem, b::Int)
   return Bool(ccall((:fmpq_mpoly_equal_si, libflint), Cint,
               (Ref{QQMPolyRingElem}, Int, Ref{QQMPolyRing}),
               a, b, a.parent))
end

==(a::Int, b::QQMPolyRingElem) = b == a

==(a::QQMPolyRingElem, b::Integer) = a == ZZRingElem(b)

==(a::Integer, b::QQMPolyRingElem) = b == a

==(a::QQMPolyRingElem, b::Rational{<:Integer}) = a == QQFieldElem(b)

==(a::Rational{<:Integer}, b::QQMPolyRingElem) = b == a

###############################################################################
#
#   Divisibility
#
###############################################################################

function divides(a::QQMPolyRingElem, b::QQMPolyRingElem)
   check_parent(a, b)
   if iszero(a)
      return true, zero(parent(a))
   end
   if iszero(b)
      return false, zero(parent(a))
   end
   z = parent(a)()
   d = ccall((:fmpq_mpoly_divides, libflint), Cint,
       (Ref{QQMPolyRingElem}, Ref{QQMPolyRingElem}, Ref{QQMPolyRingElem}, Ref{QQMPolyRing}),
       z, a, b, a.parent)
   return isone(d), z
end

###############################################################################
#
#   Division with remainder
#
###############################################################################

function Base.div(a::QQMPolyRingElem, b::QQMPolyRingElem)
   check_parent(a, b)
   q = parent(a)()
   ccall((:fmpq_mpoly_div, libflint), Nothing,
       (Ref{QQMPolyRingElem}, Ref{QQMPolyRingElem},
        Ref{QQMPolyRingElem}, Ref{QQMPolyRing}),
       q, a, b, a.parent)
   return q
end

function Base.divrem(a::QQMPolyRingElem, b::QQMPolyRingElem)
   check_parent(a, b)
   q = parent(a)()
   r = parent(a)()
   ccall((:fmpq_mpoly_divrem, libflint), Nothing,
       (Ref{QQMPolyRingElem}, Ref{QQMPolyRingElem}, Ref{QQMPolyRingElem},
        Ref{QQMPolyRingElem}, Ref{QQMPolyRing}),
       q, r, a, b, a.parent)
   return q, r
end

function Base.divrem(a::QQMPolyRingElem, b::Vector{QQMPolyRingElem})
   len = length(b)
   q = [parent(a)() for i in 1:len]
   r = parent(a)()
   ccall((:fmpq_mpoly_divrem_ideal, libflint), Nothing,
         (Ptr{Ref{QQMPolyRingElem}}, Ref{QQMPolyRingElem}, Ref{QQMPolyRingElem},
          Ptr{Ref{QQMPolyRingElem}}, Int, Ref{QQMPolyRing}),
       q, r, a, b, len, a.parent)
   return q, r
end

###############################################################################
#
#   Exact division
#
###############################################################################

function divexact(a::QQMPolyRingElem, b::QQMPolyRingElem; check::Bool=true)
   check_parent(a, b)
   b, q = divides(a, b)
   !b && error("Division is not exact in divexact")
   return q
end

###############################################################################
#
#   Calculus
#
###############################################################################

function derivative(a::QQMPolyRingElem, i::Int)
   n = nvars(parent(a))
   (i <= 0 || i > n) && error("Index must be between 1 and $n")
   z = parent(a)()
   ccall((:fmpq_mpoly_derivative, libflint), Nothing,
         (Ref{QQMPolyRingElem}, Ref{QQMPolyRingElem}, Int, Ref{QQMPolyRing}),
         z, a, i - 1, parent(a))
   return z
end

function integral(a::QQMPolyRingElem, i::Int)
   n = nvars(parent(a))
   (i <= 0 || i > n) && error("Index must be between 1 and $n")
   z = parent(a)()
   ccall((:fmpq_mpoly_integral, libflint), Nothing,
         (Ref{QQMPolyRingElem}, Ref{QQMPolyRingElem}, Int, Ref{QQMPolyRing}),
         z, a, i - 1, parent(a))
   return z
end

###############################################################################
#
#   Evaluation
#
###############################################################################

function evaluate(a::QQMPolyRingElem, b::Vector{QQFieldElem})
   length(b) != nvars(parent(a)) && error("Vector size incorrect in evaluate")
   z = QQFieldElem()
   GC.@preserve b ccall((:fmpq_mpoly_evaluate_all_fmpq, libflint), Nothing,
         (Ref{QQFieldElem}, Ref{QQMPolyRingElem}, Ptr{QQFieldElem}, Ref{QQMPolyRing}),
            z, a, b, parent(a))
   return z
end

function evaluate(a::QQMPolyRingElem, b::Vector{ZZRingElem})
   fmpq_vec = [QQFieldElem(s) for s in b]
   return evaluate(a, fmpq_vec)
end

function evaluate(a::QQMPolyRingElem, b::Vector{<:Integer})
   fmpq_vec = [QQFieldElem(s) for s in b]
   return evaluate(a, fmpq_vec)
end

function (a::QQMPolyRingElem)()
   error("need at least one value")
end

function (a::QQMPolyRingElem)(vals::QQFieldElem...)
   length(vals) != nvars(parent(a)) && error("Number of variables does not match number of values")
   return evaluate(a, [vals...])
end

function (a::QQMPolyRingElem)(vals::Integer...)
   length(vals) != nvars(parent(a)) && error("Number of variables does not match number of values")
   return evaluate(a, [vals...])
end

function (a::QQMPolyRingElem)(vals::Union{NCRingElem, RingElement}...)
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

function evaluate(a::QQMPolyRingElem, bs::Vector{QQMPolyRingElem})
   R = parent(a)
   S = parent(bs[1])

   length(bs) != nvars(R) &&
      error("Number of variables does not match number of values")

   c = S()
   fl = ccall((:fmpq_mpoly_compose_fmpq_mpoly, libflint), Cint,
              (Ref{QQMPolyRingElem}, Ref{QQMPolyRingElem}, Ptr{Ref{QQMPolyRingElem}},
               Ref{QQMPolyRing}, Ref{QQMPolyRing}),
              c, a, bs, R, S)
   fl == 0 && error("Something wrong in evaluation.")
   return c
end

function evaluate(a::QQMPolyRingElem, bs::Vector{QQPolyRingElem})
   R = parent(a)
   S = parent(bs[1])

   length(bs) != nvars(R) &&
      error("Number of variables does not match number of values")

   c = S()
   fl = ccall((:fmpq_mpoly_compose_fmpq_poly, libflint), Cint,
              (Ref{QQPolyRingElem}, Ref{QQMPolyRingElem}, Ptr{Ref{QQPolyRingElem}},
               Ref{QQMPolyRing}), c, a, bs, R)
   fl == 0 && error("Something wrong in evaluation.")
   return c
end

###############################################################################
#
#   Unsafe functions
#
###############################################################################

function zero!(a::QQMPolyRingElem)
    ccall((:fmpq_mpoly_zero, libflint), Nothing,
         (Ref{QQMPolyRingElem}, Ref{QQMPolyRing}), a, a.parent)
    return a
end

function add!(a::QQMPolyRingElem, b::QQMPolyRingElem, c::QQMPolyRingElem)
   ccall((:fmpq_mpoly_add, libflint), Nothing,
         (Ref{QQMPolyRingElem}, Ref{QQMPolyRingElem},
          Ref{QQMPolyRingElem}, Ref{QQMPolyRing}), a, b, c, a.parent)
   return a
end

function addeq!(a::QQMPolyRingElem, b::QQMPolyRingElem)
   ccall((:fmpq_mpoly_add, libflint), Nothing,
         (Ref{QQMPolyRingElem}, Ref{QQMPolyRingElem},
          Ref{QQMPolyRingElem}, Ref{QQMPolyRing}), a, a, b, a.parent)
   return a
end

function mul!(a::QQMPolyRingElem, b::QQMPolyRingElem, c::QQMPolyRingElem)
   ccall((:fmpq_mpoly_mul, libflint), Nothing,
         (Ref{QQMPolyRingElem}, Ref{QQMPolyRingElem},
          Ref{QQMPolyRingElem}, Ref{QQMPolyRing}), a, b, c, a.parent)
   return a
end

# Set the n-th coefficient of a to c. If zero coefficients are inserted, they
# must be removed with combine_like_terms!
function setcoeff!(a::QQMPolyRingElem, n::Int, c::QQFieldElem)
   if n > length(a)
      ccall((:fmpq_mpoly_resize, libflint), Nothing,
            (Ref{QQMPolyRingElem}, Int, Ref{QQMPolyRing}), a, n, a.parent)
   end
   ccall((:fmpq_mpoly_set_term_coeff_fmpq, libflint), Nothing,
         (Ref{QQMPolyRingElem}, Int, Ref{QQFieldElem}, Ref{QQMPolyRing}),
         a, n - 1, c, a.parent)
   return a
end

# Set the i-th coefficient of a to c. If zero coefficients are inserted, they
# must be removed with combine_like_terms!
setcoeff!(a::QQMPolyRingElem, i::Int, c::ZZRingElem) = setcoeff!(a, i, QQFieldElem(c))

# Set the i-th coefficient of a to c. If zero coefficients are inserted, they
# must be removed with combine_like_terms!
setcoeff!(a::QQMPolyRingElem, i::Int, c::Integer) = setcoeff!(a, i, QQFieldElem(c))

# Set the i-th coefficient of a to c. If zero coefficients are inserted, they
# must be removed with combine_like_terms!
setcoeff!(a::QQMPolyRingElem, i::Int, c::Rational{<:Integer}) =
   setcoeff!(a, i, QQFieldElem(c))

# Remove zero terms and combine adjacent terms if they have the same monomial
# no sorting is performed
function combine_like_terms!(a::QQMPolyRingElem)
   ccall((:fmpq_mpoly_combine_like_terms, libflint), Nothing,
         (Ref{QQMPolyRingElem}, Ref{QQMPolyRing}), a, a.parent)
   return a
end

###############################################################################
#
#   Manipulating terms and monomials
#
###############################################################################

function exponent_vector_fits(::Type{Int}, a::QQMPolyRingElem, i::Int)
   b = ccall((:fmpq_mpoly_term_exp_fits_si, libflint), Cint,
             (Ref{QQMPolyRingElem}, Int, Ref{QQMPolyRing}),
             a, i - 1, a.parent)
   return Bool(b)
end

function exponent_vector_fits(::Type{UInt}, a::QQMPolyRingElem, i::Int)
   b = ccall((:fmpq_mpoly_term_exp_fits_ui, libflint), Cint,
             (Ref{QQMPolyRingElem}, Int, Ref{QQMPolyRing}),
             a, i - 1, a.parent)
   return Bool(b)
end

function exponent_vector!(z::Vector{Int}, a::QQMPolyRingElem, i::Int)
   ccall((:fmpq_mpoly_get_term_exp_si, libflint), Nothing,
         (Ptr{Int}, Ref{QQMPolyRingElem}, Int, Ref{QQMPolyRing}),
         z, a, i - 1, parent(a))
   return z
end

function exponent_vector!(z::Vector{UInt}, a::QQMPolyRingElem, i::Int)
   ccall((:fmpq_mpoly_get_term_exp_ui, libflint), Nothing,
         (Ptr{UInt}, Ref{QQMPolyRingElem}, Int, Ref{QQMPolyRing}),
         z, a, i - 1, parent(a))
   return z
end

function exponent_vector!(z::Vector{ZZRingElem}, a::QQMPolyRingElem, i::Int)
   ccall((:fmpq_mpoly_get_term_exp_fmpz, libflint), Nothing,
         (Ptr{Ref{ZZRingElem}}, Ref{QQMPolyRingElem}, Int, Ref{QQMPolyRing}),
         z, a, i - 1, parent(a))
   return z
end

# Return a generator for exponent vectors of $a$
function exponent_vectors_fmpz(a::QQMPolyRingElem)
   return (exponent_vector_fmpz(a, i) for i in 1:length(a))
end

# Set exponent of n-th term to given vector of UInt's
# No sort is performed, so this is unsafe. These are promoted to ZZRingElem's if
# they don't fit into 31/63 bits
function set_exponent_vector!(a::QQMPolyRingElem, n::Int, exps::Vector{UInt})
   if n > length(a)
      ccall((:fmpq_mpoly_resize, libflint), Nothing,
            (Ref{QQMPolyRingElem}, Int, Ref{QQMPolyRing}), a, n, a.parent)
   end
   ccall((:fmpq_mpoly_set_term_exp_ui, libflint), Nothing,
         (Ref{QQMPolyRingElem}, Int, Ptr{UInt}, Ref{QQMPolyRing}),
      a, n - 1, exps, parent(a))
   return a
end

# Set exponent of n-th term to given vector of Int's
# No sort is performed, so this is unsafe. The Int's must be positive, but
# no check is performed
function set_exponent_vector!(a::QQMPolyRingElem, n::Int, exps::Vector{Int})
   if n > length(a)
      ccall((:fmpq_mpoly_resize, libflint), Nothing,
            (Ref{QQMPolyRingElem}, Int, Ref{QQMPolyRing}), a, n, a.parent)
   end
   ccall((:fmpq_mpoly_set_term_exp_ui, libflint), Nothing,
         (Ref{QQMPolyRingElem}, Int, Ptr{Int}, Ref{QQMPolyRing}),
      a, n - 1, exps, parent(a))
   return a
end

# Set exponent of n-th term to given vector of ZZRingElem's
# No sort is performed, so this is unsafe
function set_exponent_vector!(a::QQMPolyRingElem, n::Int, exps::Vector{ZZRingElem})
   if n > length(a)
      ccall((:fmpq_mpoly_resize, libflint), Nothing,
            (Ref{QQMPolyRingElem}, Int, Ref{QQMPolyRing}), a, n, a.parent)
   end
   @GC.preserve exps ccall((:fmpq_mpoly_set_term_exp_fmpz, libflint), Nothing,
         (Ref{QQMPolyRingElem}, Int, Ptr{ZZRingElem}, Ref{QQMPolyRing}),
      a, n - 1, exps, parent(a))
   return a
end

# Return j-th coordinate of i-th exponent vector
function exponent(a::QQMPolyRingElem, i::Int, j::Int)
   (j < 1 || j > nvars(parent(a))) && error("Invalid variable index")
   return ccall((:fmpq_mpoly_get_term_var_exp_ui, libflint), Int,
                (Ref{QQMPolyRingElem}, Int, Int, Ref{QQMPolyRing}),
                 a, i - 1, j - 1, a.parent)
end

# Return the coefficient of the term with the given exponent vector
# Return zero if there is no such term
function coeff(a::QQMPolyRingElem, exps::Vector{UInt})
   z = QQFieldElem()
   ccall((:fmpq_mpoly_get_coeff_fmpq_ui, libflint), Nothing,
         (Ref{QQFieldElem}, Ref{QQMPolyRingElem}, Ptr{UInt}, Ref{QQMPolyRing}),
      z, a, exps, parent(a))
   return z
end

# Return the coefficient of the term with the given exponent vector
# Return zero if there is no such term
function coeff(a::QQMPolyRingElem, exps::Vector{Int})
   z = QQFieldElem()
   ccall((:fmpq_mpoly_get_coeff_fmpq_ui, libflint), Nothing,
         (Ref{QQFieldElem}, Ref{QQMPolyRingElem}, Ptr{Int}, Ref{QQMPolyRing}),
      z, a, exps, parent(a))
   return z
end

# Set the coefficient of the term with the given exponent vector to the
# given QQFieldElem. Removal of a zero term is performed.
function setcoeff!(a::QQMPolyRingElem, exps::Vector{UInt}, b::QQFieldElem)
   ccall((:fmpq_mpoly_set_coeff_fmpq_ui, libflint), Nothing,
         (Ref{QQMPolyRingElem}, Ref{QQFieldElem}, Ptr{UInt}, Ref{QQMPolyRing}),
      a, b, exps, parent(a))
   return a
end

# Set the coefficient of the term with the given exponent vector to the
# given QQFieldElem. Removal of a zero term is performed.
function setcoeff!(a::QQMPolyRingElem, exps::Vector{Int}, b::QQFieldElem)
   ccall((:fmpq_mpoly_set_coeff_fmpq_ui, libflint), Nothing,
         (Ref{QQMPolyRingElem}, Ref{QQFieldElem}, Ptr{Int}, Ref{QQMPolyRing}),
      a, b, exps, parent(a))
   return a
end

# Set the coefficient of the term with the given exponent vector to the
# given integer. Removal of a zero term is performed.
setcoeff!(a::QQMPolyRingElem, exps::Vector{Int}, b::Rational{<:Integer}) =
   setcoeff!(a, exps, QQFieldElem(b))

# Set the coefficient of the term with the given exponent vector to the
# given ZZRingElem. Removal of a zero term is performed.
setcoeff!(a::QQMPolyRingElem, exps::Vector{Int}, b::ZZRingElem) =
   setcoeff!(a, exps, QQFieldElem(b))

# Set the coefficient of the term with the given exponent vector to the
# given integer. Removal of a zero term is performed.
setcoeff!(a::QQMPolyRingElem, exps::Vector{Int}, b::Integer) =
   setcoeff!(a, exps, QQFieldElem(b))

# Sort the terms according to the ordering. This is only needed if unsafe
# functions such as those above have been called and terms have been inserted
# out of order. Note that like terms are not combined and zeros are not
# removed. For that, call combine_like_terms!
function sort_terms!(a::QQMPolyRingElem)
   ccall((:fmpq_mpoly_sort_terms, libflint), Nothing,
         (Ref{QQMPolyRingElem}, Ref{QQMPolyRing}), a, a.parent)
   return a
end

# Return the i-th term of the polynomial, as a polynomial
function term(a::QQMPolyRingElem, i::Int)
   z = parent(a)()
   n = length(a)
   (i < 1 || i > n) && error("Index must be between 1 and $(length(a))")
   ccall((:fmpq_mpoly_get_term, libflint), Nothing,
         (Ref{QQMPolyRingElem}, Ref{QQMPolyRingElem}, Int, Ref{QQMPolyRing}),
          z, a, i - 1, a.parent)
   return z
end

# Return the i-th monomial of the polynomial, as a polynomial
function monomial(a::QQMPolyRingElem, i::Int)
   z = parent(a)()
   n = length(a)
   (i < 1 || i > n) && error("Index must be between 1 and $(length(a))")
   ccall((:fmpq_mpoly_get_term_monomial, libflint), Nothing,
         (Ref{QQMPolyRingElem}, Ref{QQMPolyRingElem}, Int, Ref{QQMPolyRing}),
          z, a, i - 1, a.parent)
   return z
end

# Sets the given polynomial m to the i-th monomial of the polynomial
function monomial!(m::QQMPolyRingElem, a::QQMPolyRingElem, i::Int)
   ccall((:fmpq_mpoly_get_term_monomial, libflint), Nothing,
         (Ref{QQMPolyRingElem}, Ref{QQMPolyRingElem}, Int, Ref{QQMPolyRing}),
          m, a, i - 1, a.parent)
   return m
end

###############################################################################
#
#   Promotion rules
#
###############################################################################

promote_rule(::Type{QQMPolyRingElem}, ::Type{V}) where {V <: Integer} = QQMPolyRingElem

promote_rule(::Type{QQMPolyRingElem}, ::Type{Rational{V}}) where {V <: Integer} = QQMPolyRingElem

promote_rule(::Type{QQMPolyRingElem}, ::Type{ZZRingElem}) = QQMPolyRingElem

promote_rule(::Type{QQMPolyRingElem}, ::Type{QQFieldElem}) = QQMPolyRingElem

###############################################################################
#
#   Build context
#
###############################################################################

function _push_term!(z::QQMPolyRingElem, c::QQFieldElem, exp::Vector{Int})
  ccall((:fmpq_mpoly_push_term_fmpq_ui, libflint), Nothing,
        (Ref{QQMPolyRingElem}, Ref{QQFieldElem}, Ptr{UInt}, Ref{QQMPolyRing}),
        z, c, exp, parent(z))
  return z
end

function _push_term!(z::QQMPolyRingElem, c::ZZRingElem, exp::Vector{Int})
  ccall((:fmpq_mpoly_push_term_fmpz_ui, libflint), Nothing,
        (Ref{QQMPolyRingElem}, Ref{ZZRingElem}, Ptr{UInt}, Ref{QQMPolyRing}),
        z, c, exp, parent(z))
  return z
end

function _push_term!(z::QQMPolyRingElem, c::Int, exp::Vector{Int})
  ccall((:fmpq_mpoly_push_term_si_ui, libflint), Nothing,
        (Ref{QQMPolyRingElem}, Int, Ptr{UInt}, Ref{QQMPolyRing}),
        z, c, exp, parent(z))
  return z
end

function _push_term!(z::QQMPolyRingElem, c::UInt, exp::Vector{Int})
  ccall((:fmpq_mpoly_push_term_ui_ui, libflint), Nothing,
        (Ref{QQMPolyRingElem}, UInt, Ptr{UInt}, Ref{QQMPolyRing}),
        z, c, exp, parent(z))
  return z
end

function push_term!(M::MPolyBuildCtx{QQMPolyRingElem},
                    c::Union{ZZRingElem, QQFieldElem, Int, UInt}, expv::Vector{Int})
   if length(expv) != nvars(parent(M.poly))
      error("length of exponent vector should match the number of variables")
   end
  _push_term!(M.poly, c, expv)
  return M
end

function push_term!(M::MPolyBuildCtx{QQMPolyRingElem},
                    c::RingElement, expv::Vector{Int})
  push_term!(M, QQ(c), expv)
  return M
end

function finish(M::MPolyBuildCtx{QQMPolyRingElem})
  res = M.poly
  R = parent(res)
  M.poly = zero(R)
  ccall((:fmpq_mpoly_sort_terms, libflint), Nothing,
        (Ref{QQMPolyRingElem}, Ref{QQMPolyRing}), res, R)
  ccall((:fmpq_mpoly_combine_like_terms, libflint), Nothing,
        (Ref{QQMPolyRingElem}, Ref{QQMPolyRing}), res, R)
  return res
end

###############################################################################
#
#   Parent object call overload
#
###############################################################################

function (R::QQMPolyRing)()
   z = QQMPolyRingElem(R)
   return z
end

function (R::QQMPolyRing)(b::QQFieldElem)
   z = QQMPolyRingElem(R, b)
   return z
end

function (R::QQMPolyRing)(b::ZZRingElem)
   z = QQMPolyRingElem(R, b)
   return z
end

function (R::QQMPolyRing)(b::Int)
   z = QQMPolyRingElem(R, b)
   return z
end

function (R::QQMPolyRing)(b::UInt)
   z = QQMPolyRingElem(R, b)
   return z
end

function (R::QQMPolyRing)(b::Integer)
   return R(ZZRingElem(b))
end

function (R::QQMPolyRing)(b::Rational{<:Integer})
   return R(QQFieldElem(b))
end

function (R::QQMPolyRing)(a::QQMPolyRingElem)
   parent(a) != R && error("Unable to coerce polynomial")
   return a
end

# Create poly with given array of coefficients and array of exponent vectors (sorting is performed)
function (R::QQMPolyRing)(a::Vector{QQFieldElem}, b::Vector{Vector{T}}) where {T <: Union{ZZRingElem, UInt}}
   length(a) != length(b) && error("Coefficient and exponent vector must have the same length")

   for i in 1:length(b)
     length(b[i]) != nvars(R) && error("Exponent vector $i has length $(length(b[i])) (expected $(nvars(R))")
   end

   z = QQMPolyRingElem(R, a, b)
   return z
end

# Create poly with given array of coefficients and array of exponent vectors (sorting is performed)
function (R::QQMPolyRing)(a::Vector{QQFieldElem}, b::Vector{Vector{Int}})
   length(a) != length(b) && error("Coefficient and exponent vector must have the same length")

   for i in 1:length(b)
      length(b[i]) != nvars(R) && error("Exponent vector $i has length $(length(b[i])) (expected $(nvars(R)))")
   end

   z = QQMPolyRingElem(R, a, b)
   return z
end

# Create poly with given array of coefficients and array of exponent vectors (sorting is performed)
function (R::QQMPolyRing)(a::Vector{Any}, b::Vector{Vector{T}}) where T
   n = nvars(R)
   length(a) != length(b) && error("Coefficient and exponent vector must have the same length")
   newa = map(QQ, a)
   newb = map(x -> map(ZZ, x), b)
   newaa = convert(Vector{QQFieldElem}, newa)
   newbb = convert(Vector{Vector{ZZRingElem}}, newb)

   for i in 1:length(newbb)
      length(newbb[i]) != n && error("Exponent vector $i has length $(length(newbb[i])) (expected $(nvars(R)))")
   end

   return R(newaa, newbb)
end
