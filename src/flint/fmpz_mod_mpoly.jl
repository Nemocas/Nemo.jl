###############################################################################
#
#   fmpz_mod_mpoly.jl : Flint multivariate polynomials over ZZModRingElem and FpFieldElem
#                       (ZZModRingElem is left out for the time being)
#
###############################################################################

export FpMPolyRing, FpMPolyRingElem

for (etype, rtype, ftype, ctype) in (
      (FpMPolyRingElem, FpMPolyRing, gfp_fmpz_mpoly_factor, FpFieldElem),)
@eval begin

###############################################################################
#
#   Data type and parent object methods
#
###############################################################################

parent_type(::Type{($etype)}) = ($rtype)

elem_type(::Type{($rtype)}) = ($etype)

elem_type(::($rtype)) = ($etype)

mpoly_type(::Type{FpFieldElem}) = FpMPolyRingElem

symbols(a::($rtype)) = a.S

parent(a::($etype)) = a.parent

function check_parent(a::($etype), b::($etype))
   parent(a) != parent(b) &&
      error("Incompatible polynomial rings in polynomial operation")
end

nvars(a::($rtype)) = a.nvars

base_ring(a::($rtype)) = a.base_ring

base_ring(f::($etype)) = base_ring(parent(f))

coefficient_ring(a::($rtype)) = a.base_ring

coefficient_ring(f::($etype)) = coefficient_ring(parent(f))

characteristic(R::($rtype)) = characteristic(base_ring(R))

modulus(R::($rtype)) = modulus(base_ring(R))

modulus(f::($etype)) = modulus(base_ring(parent(f)))


function ordering(a::($rtype))
   b = a.ord
#   b = ccall((:fmpz_mod_mpoly_ctx_ord, libflint), Cint, (Ref{zzModMPolyRing}, ), a)
   return flint_orderings[b + 1]
end

function gens(R::($rtype))
   A = Vector{($etype)}(undef, R.nvars)
   for i = 1:R.nvars
      z = R()
      ccall((:fmpz_mod_mpoly_gen, libflint), Nothing,
            (Ref{($etype)}, Int, Ref{($rtype)}),
            z, i - 1, R)
      A[i] = z
   end
   return A
end

function gen(R::($rtype), i::Int)
   n = nvars(R)
   (i <= 0 || i > n) && error("Index must be between 1 and $n")
   z = R()
   ccall((:fmpz_mod_mpoly_gen, libflint), Nothing,
         (Ref{($etype)}, Int, Ref{($rtype)}),
         z, i - 1, R)
   return z
end

function is_gen(a::($etype), i::Int)
   n = nvars(parent(a))
   (i <= 0 || i > n) && error("Index must be between 1 and $n")
   return Bool(ccall((:fmpz_mod_mpoly_is_gen, libflint), Cint,
                     (Ref{($etype)}, Int, Ref{($rtype)}),
                     a, i - 1, parent(a)))
end

function is_gen(a::($etype))
   return Bool(ccall((:fmpz_mod_mpoly_is_gen, libflint), Cint,
                     (Ref{($etype)}, Int, Ref{($rtype)}),
                     a, -1, parent(a)))
end

function deepcopy_internal(a::($etype), dict::IdDict)
   z = parent(a)()
   ccall((:fmpz_mod_mpoly_set, libflint), Nothing,
         (Ref{($etype)}, Ref{($etype)}, Ref{($rtype)}),
         z, a, parent(a))
   return z
end

function length(a::($etype))
   return a.length
#   return ccall((:fmpz_mod_mpoly_length, libflint), Int,
#                (Ref{T}, Ref{parent_type(T)}),
#                a, a.parent)
end

function one(R::($rtype))
   z = R()
   ccall((:fmpz_mod_mpoly_one, libflint), Nothing,
         (Ref{($etype)}, Ref{($rtype)}),
         z, R)
   return z
end

function zero(R::($rtype))
   z = R()
   ccall((:fmpz_mod_mpoly_zero, libflint), Nothing,
         (Ref{($etype)}, Ref{($rtype)}),
         z, R)
   return z
end

function isone(a::($etype))
   return Bool(ccall((:fmpz_mod_mpoly_is_one, libflint), Cint,
                     (Ref{($etype)}, Ref{($rtype)}),
                     a, parent(a)))
end

function iszero(a::($etype))
   return Bool(ccall((:fmpz_mod_mpoly_is_zero, libflint), Cint,
                     (Ref{($etype)}, Ref{($rtype)}),
                     a, parent(a)))
end

function is_monomial(a::($etype))
   return length(a) == 1 && isone(coeff(a, 1))
end

function is_term(a::($etype))
   return length(a) == 1
end

function is_unit(a::($etype))
   return length(a) == 1 && total_degree(a) == 0 && is_unit(coeff(a, 1))
end

function is_constant(a::($etype))
   return Bool(ccall((:fmpz_mod_mpoly_is_fmpz, libflint), Cint,
                     (Ref{($etype)}, Ref{($rtype)}),
                     a, parent(a)))
end

# TODO move this
function expressify(a::($rtype); context = nothing)
    return Expr(:sequence, Expr(:text, "Multivariate Polynomial Ring in "),
                           Expr(:series, symbols(a)...),
                           Expr(:text, " over "),
                           expressify(coefficient_ring(a); context = context))
end

@enable_all_show_via_expressify ($rtype)

################################################################################
#
#  Getting coefficients
#
################################################################################

function coeff(a::($etype), i::Int)
   n = length(a)
   (i < 1 || i > n) && error("Index must be between 1 and $(length(a))")
   z = ZZRingElem()
   ccall((:fmpz_mod_mpoly_get_term_coeff_fmpz, libflint), UInt,
         (Ref{ZZRingElem}, Ref{($etype)}, Int, Ref{($rtype)}),
         z, a, i - 1, parent(a))
   return base_ring(parent(a))(z)
end

function coeff(a::($etype), b::($etype))
   check_parent(a, b)
   !isone(length(b)) && error("Second argument must be a monomial")
   z = ZZRingElem()
   ccall((:fmpz_mod_mpoly_get_coeff_fmpz_monomial, libflint), UInt,
         (Ref{ZZRingElem}, Ref{($etype)}, Ref{($etype)}, Ref{($rtype)}),
         z, a, b, parent(a))
   return base_ring(parent(a))(z)
end

function trailing_coefficient(p::($etype))
   if length(p) > 0
      return coeff(p, length(p))
   else
      return zero(base_ring(p))
   end
end

###############################################################################
#
#   Basic manipulation
#
###############################################################################

# Degree in the i-th variable as an Int
function degree(a::($etype), i::Int)
   n = nvars(parent(a))
   (i <= 0 || i > n) && error("Index must be between 1 and $n")
   d = ccall((:fmpz_mod_mpoly_degree_si, libflint), Int,
             (Ref{($etype)}, Int, Ref{($rtype)}),
             a, i - 1, parent(a))
   return d
end

# Degree in the i-th variable as an ZZRingElem
function degree_fmpz(a::($etype), i::Int)
   n = nvars(parent(a))
   (i <= 0 || i > n) && error("Index must be between 1 and $n")
   d = ZZRingElem()
   ccall((:fmpz_mod_mpoly_degree_fmpz, libflint), Nothing,
         (Ref{ZZRingElem}, Ref{($etype)}, Int, Ref{($rtype)}),
         d, a, i - 1, parent(a))
   return d
end

# Return true if degrees fit into an Int
function degrees_fit_int(a::($etype))
   return Bool(ccall((:fmpz_mod_mpoly_degrees_fit_si, libflint), Cint,
                     (Ref{($etype)}, Ref{($rtype)}),
                     a, parent(a)))
end

# Return an array of the max degrees in each variable
function degrees(a::($etype))
   degs = Vector{Int}(undef, nvars(parent(a)))
   ccall((:fmpz_mod_mpoly_degrees_si, libflint), Nothing,
         (Ptr{Int}, Ref{($etype)}, Ref{($rtype)}),
         degs, a, parent(a))
   return degs
end

# Return an array of the max degrees as fmpzs in each variable
function degrees_fmpz(a::($etype))
   n = nvars(parent(a))
   degs = Vector{ZZRingElem}(undef, n)
   for i in 1:n
      degs[i] = ZZRingElem()
   end
   ccall((:fmpz_mod_mpoly_degrees_fmpz, libflint), Nothing,
         (Ptr{Ref{ZZRingElem}}, Ref{($etype)}, Ref{($rtype)}),
         degs, a, parent(a))
   return degs
end

# Return true if degree fits into an Int
function total_degree_fits_int(a::($etype))
   return Bool(ccall((:fmpz_mod_mpoly_total_degree_fits_si, libflint), Cint,
                     (Ref{($etype)}, Ref{($rtype)}),
                     a, parent(a)))
end

# Total degree as an Int
function total_degree(a::($etype))
   d = ccall((:fmpz_mod_mpoly_total_degree_si, libflint), Int,
             (Ref{($etype)}, Ref{($rtype)}),
             a, a.parent)
   return d
end

# Total degree as an ZZRingElem
function total_degree_fmpz(a::($etype))
   d = ZZRingElem()
   ccall((:fmpz_mod_mpoly_total_degree_fmpz, libflint), Nothing,
         (Ref{ZZRingElem}, Ref{($etype)}, Ref{($rtype)}),
         d, a, parent(a))
   return d
end

###############################################################################
#
#   Multivariable coefficient polynomials
#
###############################################################################

function coeff(a::($etype), vars::Vector{Int}, exps::Vector{Int})
   unique(vars) == vars || error("Variables not unique")
   length(vars) == length(exps) || error("Number of variables does not match number of exponents")
   z = parent(a)()
   vars .-= 1
   for i in 1:length(vars)
      0 <= vars[i] < nvars(parent(a)) || error("Variable index not in range")
      exps[i] >= 0 || error("Exponent cannot be negative")
   end
   ccall((:fmpz_mod_mpoly_get_coeff_vars_ui, libflint), Nothing,
         (Ref{($etype)}, Ref{($etype)}, Ptr{Int}, Ptr{Int}, Int, Ref{($rtype)}),
         z, a, vars, exps, length(vars), parent(a))
   return z
end

###############################################################################
#
#   Basic arithmetic
#
###############################################################################

function -(a::($etype))
   z = parent(a)()
   ccall((:fmpz_mod_mpoly_neg, libflint), Nothing,
         (Ref{($etype)}, Ref{($etype)}, Ref{($rtype)}),
         z, a, parent(a))
   return z
end

function +(a::($etype), b::($etype))
   check_parent(a, b)
   z = parent(a)()
   ccall((:fmpz_mod_mpoly_add, libflint), Nothing,
         (Ref{($etype)}, Ref{($etype)}, Ref{($etype)}, Ref{($rtype)}),
         z, a, b, parent(a))
   return z
end

function -(a::($etype), b::($etype))
   check_parent(a, b)
   z = parent(a)()
   ccall((:fmpz_mod_mpoly_sub, libflint), Nothing,
         (Ref{($etype)}, Ref{($etype)}, Ref{($etype)}, Ref{($rtype)}),
         z, a, b, parent(a))
   return z
end

function *(a::($etype), b::($etype))
   check_parent(a, b)
   z = parent(a)()
   ccall((:fmpz_mod_mpoly_mul, libflint), Nothing,
         (Ref{($etype)}, Ref{($etype)}, Ref{($etype)}, Ref{($rtype)}),
         z, a, b, parent(a))
   return z
end

###############################################################################
#
#   Ad hoc arithmetic
#
###############################################################################

function +(a::($etype), b::UInt)
   z = parent(a)()
   ccall((:fmpz_mod_mpoly_add_ui, libflint), Nothing,
         (Ref{($etype)}, Ref{($etype)}, UInt, Ref{($rtype)}),
         z, a, b, parent(a))
   return z
end

function +(a::($etype), b::Int)
   z = parent(a)()
   ccall((:fmpz_mod_mpoly_add_si, libflint), Nothing,
         (Ref{($etype)}, Ref{($etype)}, Int, Ref{($rtype)}),
         z, a, b, parent(a))
   return z
end

function +(a::($etype), b::ZZRingElem)
   z = parent(a)()
   ccall((:fmpz_mod_mpoly_add_fmpz, libflint), Nothing,
         (Ref{($etype)}, Ref{($etype)}, Ref{ZZRingElem}, Ref{($rtype)}),
         z, a, b, parent(a))
   return z
end

+(a::($etype), b::($ctype)) = a + data(b)

+(b::($ctype), a::($etype)) = a + data(b)

+(a::($etype), b::Integer) = a + base_ring(parent(a))(b)

+(a::Integer, b::($etype)) = b + a

function -(a::($etype), b::UInt)
   z = parent(a)()
   ccall((:fmpz_mod_mpoly_sub_ui, libflint), Nothing,
         (Ref{($etype)}, Ref{($etype)}, UInt, Ref{($rtype)}),
         z, a, b, parent(a))
   return z
end

function -(a::($etype), b::Int)
   z = parent(a)()
   ccall((:fmpz_mod_mpoly_sub_si, libflint), Nothing,
         (Ref{($etype)}, Ref{($etype)}, Int, Ref{($rtype)}),
         z, a, b, parent(a))
   return z
end

function -(a::($etype), b::ZZRingElem)
   z = parent(a)()
   ccall((:fmpz_mod_mpoly_sub_fmpz, libflint), Nothing,
         (Ref{($etype)}, Ref{($etype)}, Ref{ZZRingElem}, Ref{($rtype)}),
         z, a, b, parent(a))
   return z
end

-(a::($etype), b::($ctype)) = a - data(b)

-(b::($ctype), a::($etype)) = data(b) - a

-(a::($etype), b::Integer) = a - base_ring(parent(a))(b)

function -(b::Integer, a::($etype))
   z = a - b
   ccall((:fmpz_mod_mpoly_neg, libflint), Nothing,
         (Ref{($etype)}, Ref{($etype)}, Ref{($rtype)}),
         z, z, parent(a))
   return z
end

function *(a::($etype), b::UInt)
   z = parent(a)()
   ccall((:fmpz_mod_mpoly_scalar_mul_ui, libflint), Nothing,
         (Ref{($etype)}, Ref{($etype)}, UInt, Ref{($rtype)}),
         z, a, b, parent(a))
   return z
end

function *(a::($etype), b::Int)
   z = parent(a)()
   ccall((:fmpz_mod_mpoly_scalar_mul_si, libflint), Nothing,
         (Ref{($etype)}, Ref{($etype)}, Int, Ref{($rtype)}),
         z, a, b, parent(a))
   return z
end

function *(a::($etype), b::ZZRingElem)
   z = parent(a)()
   ccall((:fmpz_mod_mpoly_scalar_mul_fmpz, libflint), Nothing,
         (Ref{($etype)}, Ref{($etype)}, Ref{ZZRingElem}, Ref{($rtype)}),
         z, a, b, parent(a))
   return z
end

*(a::($etype), b::($ctype)) = a * data(b)

*(b::($ctype), a::($etype)) = a * data(b)

*(a::($etype), b::Integer) = a * base_ring(parent(a))(b)

*(b::Integer, a::($etype)) = a * base_ring(parent(a))(b)



###############################################################################
#
#   Powering
#
###############################################################################

function ^(a::($etype), b::Int)
   b >= 0 || throw(DomainError(b, "Exponent must be non-negative"))
   z = parent(a)()
   ccall((:fmpz_mod_mpoly_pow_ui, libflint), Nothing,
         (Ref{($etype)}, Ref{($etype)}, UInt, Ref{($rtype)}),
         z, a, UInt(b), parent(a))
   return z
end

function ^(a::($etype), b::ZZRingElem)
   b >= 0 || throw(DomainError(b, "Exponent must be non-negative"))
   z = parent(a)()
   ok = ccall((:fmpz_mod_mpoly_pow_fmpz, libflint), Cint,
              (Ref{($etype)}, Ref{($etype)}, Ref{ZZRingElem}, Ref{($rtype)}),
              z, a, b, parent(a))
   iszero(ok) && error("Unable to compute power")
   return z
end

################################################################################
#
#   GCD
#
################################################################################

function gcd(a::($etype), b::($etype))
   check_parent(a, b)
   z = parent(a)()
   ok = ccall((:fmpz_mod_mpoly_gcd, libflint), Cint,
              (Ref{($etype)}, Ref{($etype)}, Ref{($etype)}, Ref{($rtype)}),
              z, a, b, a.parent)
   iszero(ok) && error("Unable to compute gcd")
   return z
end

################################################################################
#
#   Factorization and Square Root
#
################################################################################

function (::Type{Fac{($etype)}})(fac::($ftype), preserve_input::Bool = true)
   R = fac.parent
   F = Fac{($etype)}()
   for i in 0:fac.num-1
      f = R()
      if preserve_input
         ccall((:fmpz_mod_mpoly_factor_get_base, libflint), Nothing,
               (Ref{($etype)}, Ref{($ftype)}, Int, Ref{($rtype)}),
               f, fac, i, R)
      else
         ccall((:fmpz_mod_mpoly_factor_swap_base, libflint), Nothing,
               (Ref{($etype)}, Ref{($ftype)}, Int, Ref{($rtype)}),
               f, fac, i, R)
      end
      F.fac[f] = ccall((:fmpz_mod_mpoly_factor_get_exp_si, libflint), Int,
                       (Ref{($ftype)}, Int, Ref{($rtype)}),
                       fac, i, R)
   end
   c = ZZRingElem()
   ccall((:fmpz_mod_mpoly_factor_get_constant_fmpz, libflint), UInt,
         (Ref{ZZRingElem}, Ref{($ftype)}),
         c, fac)
   F.unit = R(c)
   return F
end

function factor(a::($etype))
   R = parent(a)
   fac = ($ftype)(R)
   ok = ccall((:fmpz_mod_mpoly_factor, libflint), Cint,
              (Ref{($ftype)}, Ref{($etype)}, Ref{($rtype)}),
              fac, a, R)
   iszero(ok) && error("unable to compute factorization")
   return Fac{($etype)}(fac, false)
end

function factor_squarefree(a::($etype))
   R = parent(a)
   fac = ($ftype)(R)
   ok = ccall((:fmpz_mod_mpoly_factor_squarefree, libflint), Cint,
              (Ref{($ftype)}, Ref{($etype)}, Ref{($rtype)}),
              fac, a, R)
   iszero(ok) && error("unable to compute factorization")
   return Fac{($etype)}(fac, false)
end


function sqrt(a::($etype); check::Bool=true)
   (flag, q) = is_square_with_sqrt(a)
   check && !flag && error("Not a square")
   return q
end

function is_square(a::($etype))
   return Bool(ccall((:fmpz_mod_mpoly_is_square, libflint), Cint,
                     (Ref{($etype)}, Ref{($rtype)}),
                     a, a.parent))
end

function is_square_with_sqrt(a::($etype))
   q = parent(a)()
   flag = ccall((:fmpz_mod_mpoly_sqrt, libflint), Cint,
                (Ref{($etype)}, Ref{($etype)}, Ref{($rtype)}),
                q, a, a.parent)
   return (Bool(flag), q)
end

###############################################################################
#
#   Comparison
#
###############################################################################

function ==(a::($etype), b::($etype))
   check_parent(a, b)
   return Bool(ccall((:fmpz_mod_mpoly_equal, libflint), Cint,
                     (Ref{($etype)}, Ref{($etype)}, Ref{($rtype)}),
                     a, b, a.parent))
end

function Base.isless(a::($etype), b::($etype))
   (!is_monomial(a) || !is_monomial(b)) && error("Not monomials in comparison")
   return ccall((:fmpz_mod_mpoly_cmp, libflint), Cint,
                (Ref{($etype)}, Ref{($etype)}, Ref{($rtype)}),
                a, b, a.parent) < 0
end

###############################################################################
#
#   Ad hoc comparison
#
###############################################################################

function ==(a::($etype), b::($ctype))
   return Bool(ccall((:fmpz_mod_mpoly_equal_fmpz, libflint), Cint,
                     (Ref{($etype)}, Ref{ZZRingElem}, Ref{($rtype)}),
                     a, data(b), a.parent))
end

==(a::($ctype), b::($etype)) = b == a

function ==(a::($etype), b::UInt)
   return Bool(ccall((:fmpz_mod_mpoly_equal_ui, libflint), Cint,
                     (Ref{($etype)}, UInt, Ref{($rtype)}),
                     a, b, parent(a)))
end

==(a::UInt, b::($etype)) = b == a

function ==(a::($etype), b::Int)
   return Bool(ccall((:fmpz_mod_mpoly_equal_si, libflint), Cint,
                     (Ref{($etype)}, Int, Ref{($rtype)}),
                     a, b, parent(a)))
end

==(a::Int, b::($etype)) = b == a

==(a::($etype), b::Integer) = a == base_ring(parent(a))(b)

==(a::($etype), b::ZZRingElem) = a == base_ring(parent(a))(b)

==(a::Integer, b::($etype)) = b == a

==(a::ZZRingElem, b::($etype)) = b == a

###############################################################################
#
#   Divisibility
#
###############################################################################

function divides(a::($etype), b::($etype))
   check_parent(a, b)
   if iszero(a)
      return true, zero(parent(a))
   end
   if iszero(b)
      return false, zero(parent(a))
   end
   z = parent(a)()
   d = ccall((:fmpz_mod_mpoly_divides, libflint), Cint,
             (Ref{($etype)}, Ref{($etype)}, Ref{($etype)}, Ref{($rtype)}),
             z, a, b, parent(a))
   return isone(d), z
end

###############################################################################
#
#   Division with remainder
#
###############################################################################

function Base.div(a::($etype), b::($etype))
   check_parent(a, b)
   q = parent(a)()
   ccall((:fmpz_mod_mpoly_div, libflint), Nothing,
         (Ref{($etype)}, Ref{($etype)}, Ref{($etype)}, Ref{($rtype)}),
         q, a, b, parent(a))
   return q
end

function Base.divrem(a::($etype), b::($etype))
   check_parent(a, b)
   q = parent(a)()
   r = parent(a)()
   ccall((:fmpz_mod_mpoly_divrem, libflint), Nothing,
         (Ref{($etype)}, Ref{($etype)}, Ref{($etype)},
          Ref{($etype)}, Ref{($rtype)}),
         q, r, a, b, parent(a))
   return q, r
end

function Base.divrem(a::($etype), b::Vector{($etype)})
   len = length(b)
   q = [parent(a)() for i in 1:len]
   r = parent(a)()
   ccall((:fmpz_mod_mpoly_divrem_ideal, libflint), Nothing,
         (Ptr{Ref{($etype)}}, Ref{($etype)}, Ref{($etype)},
          Ptr{Ref{($etype)}}, Int, Ref{($rtype)}),
         q, r, a, b, len, parent(a))
   return q, r
end

###############################################################################
#
#   Exact division
#
###############################################################################

function divexact(a::($etype), b::($etype); check::Bool=true)
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

function derivative(a::($etype), i::Int)
   n = nvars(parent(a))
   (i <= 0 || i > n) && error("Index must be between 1 and $n")
   z = parent(a)()
   ccall((:fmpz_mod_mpoly_derivative, libflint), Nothing,
         (Ref{($etype)}, Ref{($etype)}, Int, Ref{($rtype)}),
         z, a, i - 1, parent(a))
   return z
end

###############################################################################
#
#   Evaluation
#
###############################################################################

# TODO have AA define evaluate(a, vals) for general vals
# so we can get rid of this copy pasta
function (a::($etype))(vals::Union{NCRingElem, RingElement}...)
   length(vals) != nvars(parent(a)) && error("Number of variables does not match number of values")
   R = base_ring(a)
   powers = [Dict{Int, Any}() for i in 1:length(vals)]
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
   cvzip = zip(coefficients(a), exponent_vectors(a))
   for (c, v) in cvzip
      t = c
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


###############################################################################
#
#   Unsafe functions
#
###############################################################################

function zero!(a::($etype))
    ccall((:fmpz_mod_mpoly_zero, libflint), Nothing,
         (Ref{($etype)}, Ref{($rtype)}),
         a, parent(a))
    return a
end

function add!(a::($etype), b::($etype), c::($etype))
   ccall((:fmpz_mod_mpoly_add, libflint), Nothing,
         (Ref{($etype)}, Ref{($etype)}, Ref{($etype)}, Ref{($rtype)}),
         a, b, c, parent(a))
   return a
end

function addeq!(a::($etype), b::($etype))
   ccall((:fmpz_mod_mpoly_add, libflint), Nothing,
         (Ref{($etype)}, Ref{($etype)}, Ref{($etype)}, Ref{($rtype)}),
         a, a, b, parent(a))
   return a
end

function mul!(a::($etype), b::($etype), c::($etype))
   ccall((:fmpz_mod_mpoly_mul, libflint), Nothing,
         (Ref{($etype)}, Ref{($etype)}, Ref{($etype)}, Ref{($rtype)}),
         a, b, c, parent(a))
   return a
end

# Set the n-th coefficient of a to c. If zero coefficients are inserted, they
# must be removed with combine_like_terms!
function setcoeff!(a::($etype), n::Int, c::($ctype))
   if n > length(a)
      ccall((:fmpz_mod_mpoly_resize, libflint), Nothing,
            (Ref{($etype)}, Int, Ref{($rtype)}),
            a, n, a.parent)
   end
   ccall((:fmpz_mod_mpoly_set_term_coeff_fmpz, libflint), Nothing,
         (Ref{($etype)}, Int, Ref{ZZRingElem}, Ref{($rtype)}),
         a, n - 1, data(c), a.parent)
   return a
end

# Set the i-th coefficient of a to c. If zero coefficients are inserted, they
# must be removed with combine_like_terms!
setcoeff!(a::($etype), i::Int, c::Integer) = setcoeff!(a, i, base_ring(parent(a))(c))

# Set the i-th coefficient of a to c. If zero coefficients are inserted, they
# must be removed with combine_like_terms!
setcoeff!(a::($etype), i::Int, c::ZZRingElem) = setcoeff!(a, i, base_ring(parent(a))(c))

# Remove zero terms and combine adjacent terms if they have the same monomial
# no sorting is performed
function combine_like_terms!(a::($etype))
   ccall((:fmpz_mod_mpoly_combine_like_terms, libflint), Nothing,
         (Ref{($etype)}, Ref{($rtype)}),
         a, a.parent)
   return a
end

###############################################################################
#
#   Manipulating terms and monomials
#
###############################################################################

function exponent_vector_fits(::Type{Int}, a::($etype), i::Int)
   b = ccall((:fmpz_mod_mpoly_term_exp_fits_si, libflint), Cint,
             (Ref{($etype)}, Int, Ref{($rtype)}),
             a, i - 1, parent(a))
   return Bool(b)
end

function exponent_vector_fits(::Type{UInt}, a::($etype), i::Int)
   b = ccall((:fmpz_mod_mpoly_term_exp_fits_ui, libflint), Cint,
             (Ref{($etype)}, Int, Ref{($rtype)}),
             a, i - 1, parent(a))
   return Bool(b)
end

function exponent_vector!(z::Vector{Int}, a::($etype), i::Int)
   ccall((:fmpz_mod_mpoly_get_term_exp_si, libflint), Nothing,
         (Ptr{Int}, Ref{($etype)}, Int, Ref{($rtype)}),
         z, a, i - 1, parent(a))
   return z
end

function exponent_vector!(z::Vector{UInt}, a::($etype), i::Int)
   ccall((:fmpz_mod_mpoly_get_term_exp_ui, libflint), Nothing,
         (Ptr{UInt}, Ref{($etype)}, Int, Ref{($rtype)}),
         z, a, i - 1, parent(a))
   return z
end

function exponent_vector!(z::Vector{ZZRingElem}, a::($etype), i::Int)
   ccall((:fmpz_mod_mpoly_get_term_exp_fmpz, libflint), Nothing,
         (Ptr{Ref{ZZRingElem}}, Ref{($etype)}, Int, Ref{($rtype)}),
         z, a, i - 1, parent(a))
   return z
end

# Return a generator for exponent vectors of $a$
function exponent_vectors_fmpz(a::($etype))
   return (exponent_vector_fmpz(a, i) for i in 1:length(a))
end

# Set exponent of n-th term to given vector of UInt's
# No sort is performed, so this is unsafe.
function set_exponent_vector!(a::($etype), n::Int, exps::Vector{UInt})
   if n > length(a)
      ccall((:fmpz_mod_mpoly_resize, libflint), Nothing,
            (Ref{($etype)}, Int, Ref{($rtype)}), a, n, a.parent)
   end
   ccall((:fmpz_mod_mpoly_set_term_exp_ui, libflint), Nothing,
         (Ref{($etype)}, Int, Ptr{UInt}, Ref{($rtype)}),
         a, n - 1, exps, parent(a))
   return a
end

# Set exponent of n-th term to given vector of Int's
# No sort is performed, so this is unsafe. The Int's must be positive, but
# no check is performed
function set_exponent_vector!(a::($etype), n::Int, exps::Vector{Int})
   if n > length(a)
      ccall((:fmpz_mod_mpoly_resize, libflint), Nothing,
            (Ref{($etype)}, Int, Ref{($rtype)}),
            a, n, parent(a))
   end
   ccall((:fmpz_mod_mpoly_set_term_exp_ui, libflint), Nothing,
         (Ref{($etype)}, Int, Ptr{Int}, Ref{($rtype)}),
         a, n - 1, exps, parent(a))
   return a
end

# Set exponent of n-th term to given vector of ZZRingElem's
# No sort is performed, so this is unsafe
function set_exponent_vector!(a::($etype), n::Int, exps::Vector{ZZRingElem})
   if n > length(a)
      ccall((:fmpz_mod_mpoly_resize, libflint), Nothing,
            (Ref{($etype)}, Int, Ref{($rtype)}),
            a, n, parent(a))
   end
   ccall((:fmpz_mod_mpoly_set_term_exp_fmpz, libflint), Nothing,
         (Ref{($etype)}, Int, Ptr{ZZRingElem}, Ref{($rtype)}),
         a, n - 1, exps, parent(a))
   return a
end

# Return j-th coordinate of i-th exponent vector
function exponent(a::($etype), i::Int, j::Int)
   (j < 1 || j > nvars(parent(a))) && error("Invalid variable index")
   return ccall((:fmpz_mod_mpoly_get_term_var_exp_ui, libflint), Int,
                (Ref{($etype)}, Int, Int, Ref{($rtype)}),
                 a, i - 1, j - 1, parent(a))
end

# Return the coefficient of the term with the given exponent vector
# Return zero if there is no such term
function coeff(a::($etype), exps::Vector{UInt})
   z = ZZRingElem()
   ccall((:fmpz_mod_mpoly_get_coeff_fmpz_ui, libflint), UInt,
         (Ref{ZZRingElem}, Ref{($etype)}, Ptr{UInt}, Ref{($rtype)}),
         z, a, exps, parent(a))
   return base_ring(parent(a))(z)
end

# Return the coefficient of the term with the given exponent vector
# Return zero if there is no such term
function coeff(a::($etype), exps::Vector{Int})
   z = ZZRingElem()
   ccall((:fmpz_mod_mpoly_get_coeff_fmpz_ui, libflint), UInt,
         (Ref{ZZRingElem}, Ref{($etype)}, Ptr{Int}, Ref{($rtype)}),
         z, a, exps, parent(a))
   return base_ring(parent(a))(z)
end

# Set the coefficient of the term with the given exponent vector to the
# given ZZRingElem. Removal of a zero term is performed.
function setcoeff!(a::($etype), exps::Vector{UInt}, b::($ctype))
   ccall((:fmpz_mod_mpoly_set_coeff_fmpz_ui, libflint), Nothing,
         (Ref{($etype)}, Ref{ZZRingElem}, Ptr{UInt}, Ref{($rtype)}),
         a, data(b), exps, parent(a))
   return a
end

# Set the coefficient of the term with the given exponent vector to the
# given ZZRingElem. Removal of a zero term is performed.
function setcoeff!(a::($etype), exps::Vector{Int}, b::($ctype))
   ccall((:fmpz_mod_mpoly_set_coeff_fmpz_ui, libflint), Nothing,
         (Ref{($etype)}, Ref{ZZRingElem}, Ptr{Int}, Ref{($rtype)}),
         a, data(b), exps, parent(a))
   return a
end

# Set the coefficient of the term with the given exponent vector to the
# given integer. Removal of a zero term is performed.
setcoeff!(a::($etype), exps::Vector{Int}, b::Integer) =
   setcoeff!(a, exps, base_ring(parent(a))(b))

# Sort the terms according to the ordering. This is only needed if unsafe
# functions such as those above have been called and terms have been inserted
# out of order. Note that like terms are not combined and zeros are not
# removed. For that, call combine_like_terms!
function sort_terms!(a::($etype))
   ccall((:fmpz_mod_mpoly_sort_terms, libflint), Nothing,
         (Ref{($etype)}, Ref{($rtype)}),
         a, parent(a))
   return a
end

# Return the i-th term of the polynomial, as a polynomial
function term(a::($etype), i::Int)
   z = parent(a)()
   ccall((:fmpz_mod_mpoly_get_term, libflint), Nothing,
         (Ref{($etype)}, Ref{($etype)}, Int, Ref{($rtype)}),
         z, a, i - 1, parent(a))
   return z
end

# Sets the given polynomial m to the i-th monomial of the polynomial
function monomial!(m::($etype), a::($etype), i::Int)
   ccall((:fmpz_mod_mpoly_get_term_monomial, libflint), Nothing,
         (Ref{($etype)}, Ref{($etype)}, Int, Ref{($rtype)}),
         m, a, i - 1, a.parent)
   return m
end

# Return the i-th monomial of the polynomial, as a polynomial
function monomial(a::($etype), i::Int)
   return monomial!(parent(a)(), a, i)
end

###############################################################################
#
#   Promotion rules
#
###############################################################################

promote_rule(::Type{($etype)}, ::Type{V}) where {V <: Integer} = ($etype)

promote_rule(::Type{($etype)}, ::Type{ZZRingElem}) = ($etype)

promote_rule(::Type{($etype)}, ::Type{$ctype}) = ($etype)

###############################################################################
#
#   Parent object call overload
#
###############################################################################

function (R::($rtype))()
   return ($etype)(R)
end

function (R::($rtype))(b::($ctype))
   return ($etype)(R, b)
end

function (R::($rtype))(b::IntegerUnion)
   return ($etype)(R, ZZRingElem(b))
end

function (R::($rtype))(a::($etype))
   parent(a) != R && error("Unable to coerce polynomial")
   return a
end

# Create poly with given array of coefficients and array of exponent vectors (sorting is performed)
function (R::($rtype))(a::Vector{($ctype)}, b::Vector{Vector{T}}) where {T <: Union{ZZRingElem, UInt, Int}}
   length(a) != length(b) && error("Coefficient and exponent vector must have the same length")
   for i in 1:length(b)
      length(b[i]) != nvars(R) && error("Exponent vector $i has length $(length(b[i])) (expected $(nvars(R))")
      T !== UInt && any(x->x<0, b[i]) && error("negative exponent")
   end
   z = ($etype)(R, a, b)
   return z
end

# Create poly with given array of coefficients and array of exponent vectors (sorting is performed)
function (R::($rtype))(a::Vector, b::Vector{Vector{T}}) where T
   n = nvars(R)
   length(a) != length(b) && error("Coefficient and exponent vector must have the same length")
   newa = map(base_ring(R), a)
   newb = map(x -> map(FlintZZ, x), b)
   newaa = convert(Vector{($ctype)}, newa)
   newbb = convert(Vector{Vector{ZZRingElem}}, newb)
   for i in 1:length(newbb)
      length(newbb[i]) != n && error("Exponent vector $i has length $(length(newbb[i])) (expected $(nvars(R)))")
   end
   return R(newaa, newbb)
end

end #eval
end #for

################################################################################
#
#  Ad hoc exact division
#
################################################################################

function divexact(f::FpMPolyRingElem, a::fpFieldElem; check::Bool=true)
  return f*inv(a)
end

function divexact(f::FpMPolyRingElem, a::IntegerUnion; check::Bool=true)
  return divexact(f, base_ring(f)(a))
end

###############################################################################
#
#   polynomial_ring constructor
#
###############################################################################

function polynomial_ring(R::FpField, s::Vector{Symbol}; cached::Bool = true, ordering::Symbol = :lex)
   parent_obj = FpMPolyRing(R, s, ordering, cached)
   return tuple(parent_obj, gens(parent_obj))
end

function polynomial_ring(R::FpField, s::Vector{String}; cached::Bool = true, ordering::Symbol = :lex)
   return polynomial_ring(R, [Symbol(x) for x in s]; cached=cached, ordering=ordering)
end
