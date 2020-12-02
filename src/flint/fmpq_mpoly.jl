###############################################################################
#
#   fmpq_mpoly.jl : Flint multivariate polynomials over fmpq
#
###############################################################################

export FmpqMPolyRing, fmpq_mpoly, degrees, symbols, degree_fmpz,
       degrees_fit_int, degrees_fmpz, total_degree_fits_int, total_degree_fmpz,
       combine_like_terms!, sort_terms!, exponent_vector_fits_ui,
       exponent_vector_fits_int, exponent_vector, exponent_vector_fmpz,
       exponent_vectors, exponent_vectors_fmpz, set_exponent_vector!,
       sort_terms!

###############################################################################
#
#   Data type and parent object methods
#
###############################################################################

parent_type(::Type{fmpq_mpoly}) = FmpqMPolyRing

elem_type(::Type{FmpqMPolyRing}) = fmpq_mpoly

elem_type(::FmpqMPolyRing) = fmpq_mpoly

symbols(a::FmpqMPolyRing) = a.S

parent(a::fmpq_mpoly) = a.parent

function check_parent(a::fmpq_mpoly, b::fmpq_mpoly)
   parent(a) != parent(b) &&
      error("Incompatible polynomial rings in polynomial operation")
end

nvars(a::FmpqMPolyRing) = ccall((:fmpq_mpoly_ctx_nvars, libflint), Int,
                                (Ref{FmpqMPolyRing}, ), a)

base_ring(a::FmpqMPolyRing) = a.base_ring

base_ring(f::fmpq_mpoly) = f.parent.base_ring

function ordering(a::FmpqMPolyRing)
   b = ccall((:fmpq_mpoly_ctx_ord, libflint), Cint, (Ref{FmpqMPolyRing}, ), a)
   return flint_orderings[b + 1]
end

function gens(R::FmpqMPolyRing)
   A = Vector{fmpq_mpoly}(undef, R.nvars)
   for i = 1:R.nvars
      z = R()
      ccall((:fmpq_mpoly_gen, libflint), Nothing,
            (Ref{fmpq_mpoly}, Int, Ref{FmpqMPolyRing}), z, i - 1, R)
      A[i] = z
   end
   return A
end

function gen(R::FmpqMPolyRing, i::Int)
   n = nvars(R)
   (i <= 0 || i > n) && error("Index must be between 1 and $n")
   z = R()
   ccall((:fmpq_mpoly_gen, libflint), Nothing,
         (Ref{fmpq_mpoly}, Int, Ref{FmpqMPolyRing}), z, i - 1, R)
   return z
end

function isgen(a::fmpq_mpoly, i::Int)
   n = nvars(parent(a))
   (i <= 0 || i > n) && error("Index must be between 1 and $n")
   R = parent(a)
   return Bool(ccall((:fmpq_mpoly_is_gen, libflint), Cint,
                     (Ref{fmpq_mpoly}, Int, Ref{FmpqMPolyRing}),
                     a, i - 1, a.parent))
end

function isgen(a::fmpq_mpoly)
   n = nvars(parent(a))
   for i in 1:n
      isgen(a, i) && return true
   end
   return false
end

function deepcopy_internal(a::fmpq_mpoly, dict::IdDict)
   z = parent(a)()
   ccall((:fmpq_mpoly_set, libflint), Nothing,
         (Ref{fmpq_mpoly}, Ref{fmpq_mpoly}, Ref{FmpqMPolyRing}),
         z, a, a.parent)
   return z
end

function length(a::fmpq_mpoly)
   n = ccall((:fmpq_mpoly_length, libflint), Int, (Ref{fmpq_mpoly}, ), a)
   return n
end

function one(R::FmpqMPolyRing)
   z = R()
   ccall((:fmpq_mpoly_one, libflint), Nothing,
         (Ref{fmpq_mpoly}, Ref{FmpqMPolyRing}), z, R)
   return z
end

function zero(R::FmpqMPolyRing)
   z = R()
   ccall((:fmpq_mpoly_zero, libflint), Nothing,
         (Ref{fmpq_mpoly}, Ref{FmpqMPolyRing}), z, R)
   return z
end

function isone(a::fmpq_mpoly)
   b = ccall((:fmpq_mpoly_is_one, libflint), Cint,
             (Ref{fmpq_mpoly}, Ref{FmpqMPolyRing}), a, a.parent)
   return Bool(b)
end

function iszero(a::fmpq_mpoly)
   b = ccall((:fmpq_mpoly_is_zero, libflint), Cint,
             (Ref{fmpq_mpoly}, Ref{FmpqMPolyRing}), a, a.parent)
   return Bool(b)
end

function ismonomial(a::fmpq_mpoly)
   return length(a) == 1 && coeff(a, 1) == 1
end

function isterm(a::fmpq_mpoly)
   return length(a) == 1
end

function isunit(a::fmpq_mpoly)
   return length(a) == 1 && total_degree(a) == 0 && isunit(coeff(a, 1))
end

function isconstant(a::fmpq_mpoly)
   b = ccall((:fmpq_mpoly_is_fmpq, libflint), Cint,
             (Ref{fmpq_mpoly}, Ref{FmpqMPolyRing}), a, parent(a))
   return Bool(b)
end

function content(a::fmpq_mpoly)
  c = fmpq()
  ccall((:fmpq_mpoly_content, libflint), Nothing,
        (Ref{fmpq}, Ref{fmpq_mpoly}, Ref{FmpqMPolyRing}), c, a, parent(a))
  return c
end

function denominator(a::fmpq_mpoly)
  c = fmpz()
  ccall((:fmpq_mpoly_get_denominator, libflint), Nothing,
        (Ref{fmpz}, Ref{fmpq_mpoly}, Ref{FmpqMPolyRing}), c, a, parent(a))
  return c
end

characteristic(::FmpqMPolyRing) = 0

################################################################################
#
#  Getting coefficients
#
################################################################################

function coeff(a::fmpq_mpoly, i::Int)
   z = fmpq()
   n = length(a)
   (i < 1 || i > n) && error("Index must be between 1 and $(length(a))")
   ccall((:fmpq_mpoly_get_term_coeff_fmpq, libflint), Nothing,
         (Ref{fmpq}, Ref{fmpq_mpoly}, Int, Ref{FmpqMPolyRing}),
         z, a, i - 1, a.parent)
   return z
end

function coeff(a::fmpq_mpoly, b::fmpq_mpoly)
   check_parent(a, b)
   !isone(length(b)) && error("Second argument must be a monomial")
   z = fmpq()
   ccall((:fmpq_mpoly_get_coeff_fmpq_monomial, libflint), Nothing,
         (Ref{fmpq}, Ref{fmpq_mpoly}, Ref{fmpq_mpoly}, Ref{FmpqMPolyRing}),
         z, a, b, parent(a))
   return z
end

###############################################################################
#
#   Basic manipulation
#
###############################################################################

# Degree in the i-th variable as an Int
function degree(a::fmpq_mpoly, i::Int)
   n = nvars(parent(a))
   (i <= 0 || i > n) && error("Index must be between 1 and $n")
   d = ccall((:fmpq_mpoly_degree_si, libflint), Int,
             (Ref{fmpq_mpoly}, Int, Ref{FmpqMPolyRing}), a, i - 1, a.parent)
   return d
end

# Degree in the i-th variable as an fmpz
function degree_fmpz(a::fmpq_mpoly, i::Int)
   n = nvars(parent(a))
   (i <= 0 || i > n) && error("Index must be between 1 and $n")
   d = fmpz()
   ccall((:fmpq_mpoly_degree_fmpz, libflint), Nothing,
         (Ref{fmpz}, Ref{fmpq_mpoly}, Int, Ref{FmpqMPolyRing}),
         d, a, i - 1, a.parent)
   return d
end

# Return true if degrees fit into an Int
function degrees_fit_int(a::fmpq_mpoly)
   b = ccall((:fmpq_mpoly_degrees_fit_si, libflint), Cint,
             (Ref{fmpq_mpoly}, Ref{FmpqMPolyRing}), a, a.parent)
   return Bool(b)
end

# Return an array of the max degrees in each variable
function degrees(a::fmpq_mpoly)
   degs = Vector{Int}(undef, nvars(parent(a)))
   ccall((:fmpq_mpoly_degrees_si, libflint), Nothing,
         (Ptr{Int}, Ref{fmpq_mpoly}, Ref{FmpqMPolyRing}),
         degs, a, a.parent)
   return degs
end

# Return an array of the max degrees as fmpzs in each variable
function degrees_fmpz(a::fmpq_mpoly)
   n = nvars(parent(a))
   degs = Vector{fmpz}(undef, n)
   for i in 1:n
      degs[i] = fmpz()
   end
   ccall((:fmpq_mpoly_degrees_fmpz, libflint), Nothing,
         (Ptr{Ref{fmpz}}, Ref{fmpq_mpoly}, Ref{FmpqMPolyRing}),
         degs, a, a.parent)
   return degs
end

# Return true if degree fits into an Int
function total_degree_fits_int(a::fmpq_mpoly)
      b = ccall((:fmpq_mpoly_total_degree_fits_si, libflint), Cint,
                (Ref{fmpq_mpoly}, Ref{FmpqMPolyRing}), a, a.parent)
      return Bool(b)
   end

# Total degree as an Int
function total_degree(a::fmpq_mpoly)
   d = ccall((:fmpq_mpoly_total_degree_si, libflint), Int,
             (Ref{fmpq_mpoly}, Ref{FmpqMPolyRing}), a, a.parent)
   return d
end

# Total degree as an fmpz
function total_degree_fmpz(a::fmpq_mpoly)
   d = fmpz()
   ccall((:fmpq_mpoly_total_degree_fmpz, libflint), Nothing,
         (Ref{fmpz}, Ref{fmpq_mpoly}, Ref{FmpqMPolyRing}),
            d, a, a.parent)
   return d
end

###############################################################################
#
#   Multivariable coefficient polynomials
#
###############################################################################

function coeff(a::fmpq_mpoly, vars::Vector{Int}, exps::Vector{Int})
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
         (Ref{fmpq_mpoly}, Ref{fmpq_mpoly}, Ptr{Int},
          Ptr{Int}, Int, Ref{FmpqMPolyRing}),
          z, a, vars, exps, length(vars), a.parent)
   return z
end

###############################################################################
#
#   String I/O
#
###############################################################################

function show(io::IO, p::FmpqMPolyRing)
   local max_vars = 5 # largest number of variables to print
   n = nvars(p)
   print(io, "Multivariate Polynomial Ring in ")
   if n > max_vars
      print(io, nvars(p))
      print(io, " variables ")
   end
   for i = 1:min(n - 1, max_vars - 1)
      print(io, string(p.S[i]), ", ")
   end
   if n > max_vars
      print(io, "..., ")
   end
   print(io, string(p.S[n]))
   print(io, " over ")
   show(io, base_ring(p))
end

###############################################################################
#
#   Basic arithmetic
#
###############################################################################

function -(a::fmpq_mpoly)
   z = parent(a)()
   ccall((:fmpq_mpoly_neg, libflint), Nothing,
       (Ref{fmpq_mpoly}, Ref{fmpq_mpoly}, Ref{FmpqMPolyRing}),
       z, a, a.parent)
   return z
end

function +(a::fmpq_mpoly, b::fmpq_mpoly)
   check_parent(a, b)
   z = parent(a)()
   ccall((:fmpq_mpoly_add, libflint), Nothing,
       (Ref{fmpq_mpoly}, Ref{fmpq_mpoly}, Ref{fmpq_mpoly}, Ref{FmpqMPolyRing}),
       z, a, b, a.parent)
   return z
end

function -(a::fmpq_mpoly, b::fmpq_mpoly)
   check_parent(a, b)
   z = parent(a)()
   ccall((:fmpq_mpoly_sub, libflint), Nothing,
       (Ref{fmpq_mpoly}, Ref{fmpq_mpoly}, Ref{fmpq_mpoly}, Ref{FmpqMPolyRing}),
       z, a, b, a.parent)
   return z
end

function *(a::fmpq_mpoly, b::fmpq_mpoly)
   check_parent(a, b)
   z = parent(a)()
   ccall((:fmpq_mpoly_mul, libflint), Nothing,
       (Ref{fmpq_mpoly}, Ref{fmpq_mpoly}, Ref{fmpq_mpoly}, Ref{FmpqMPolyRing}),
       z, a, b, a.parent)
   return z
end

###############################################################################
#
#   Ad hoc arithmetic
#
###############################################################################

for (jT, cN, cT) in ((fmpq, :fmpq, Ref{fmpq}), (fmpz, :fmpz, Ref{fmpz}),
                     (Int, :si, Int))
   @eval begin
      function +(a::fmpq_mpoly, b::($jT))
         z = parent(a)()
         ccall(($(string(:fmpq_mpoly_add_, cN)), libflint), Nothing,
               (Ref{fmpq_mpoly}, Ref{fmpq_mpoly}, ($cT), Ref{FmpqMPolyRing}),
               z, a, b, parent(a))
         return z
      end

      +(a::($jT), b::fmpq_mpoly) = b + a

      function -(a::fmpq_mpoly, b::($jT))
         z = parent(a)()
         ccall(($(string(:fmpq_mpoly_sub_, cN)), libflint), Nothing,
               (Ref{fmpq_mpoly}, Ref{fmpq_mpoly}, ($cT), Ref{FmpqMPolyRing}),
               z, a, b, parent(a))
         return z
      end

      -(a::($jT), b::fmpq_mpoly) = - (b - a)

      function *(a::fmpq_mpoly, b::($jT))
         z = parent(a)()
         ccall(($(string(:fmpq_mpoly_scalar_mul_, cN)), libflint), Nothing,
               (Ref{fmpq_mpoly}, Ref{fmpq_mpoly}, ($cT), Ref{FmpqMPolyRing}),
               z, a, b, parent(a))
         return z
      end

      *(a::($jT), b::fmpq_mpoly) = b * a

      function divexact(a::fmpq_mpoly, b::($jT))
         z = parent(a)()
         ccall(($(string(:fmpq_mpoly_scalar_div_, cN)), libflint), Nothing,
               (Ref{fmpq_mpoly}, Ref{fmpq_mpoly}, ($cT), Ref{FmpqMPolyRing}),
               z, a, b, parent(a))
         return z
      end

      //(a::fmpq_mpoly, b::($jT)) = divexact(a, b)
   end
end

+(a::fmpq_mpoly, b::Integer) = a + fmpz(b)

+(a::Integer, b::fmpq_mpoly) = b + a

-(a::fmpq_mpoly, b::Integer) = a - fmpz(b)

-(a::Integer, b::fmpq_mpoly) = -(b - a)

+(a::fmpq_mpoly, b::Rational{<:Integer}) = a + fmpq(b)

+(a::Rational{<:Integer}, b::fmpq_mpoly) = b + a

-(a::fmpq_mpoly, b::Rational{<:Integer}) = a - fmpq(b)

-(a::Rational{<:Integer}, b::fmpq_mpoly) = -(b - a)

*(a::fmpq_mpoly, b::Integer) = a * fmpz(b)

*(a::Integer, b::fmpq_mpoly) = b * a

*(a::fmpq_mpoly, b::Rational{<:Integer}) = a * fmpq(b)

*(a::Rational{<:Integer}, b::fmpq_mpoly) = b * a

divexact(a::fmpq_mpoly, b::Integer) = divexact(a, fmpz(b))

divexact(a::fmpq_mpoly, b::Rational{<:Integer}) = divexact(a, fmpq(b))

//(a::fmpq_mpoly, b::Integer) = //(a, fmpz(b))

//(a::fmpq_mpoly, b::Rational{<:Integer}) = //(a, fmpq(b))

###############################################################################
#
#   Powering
#
###############################################################################

function ^(a::fmpq_mpoly, b::Int)
   b < 0 && throw(DomainError(b, "Exponent must be non-negative"))
   z = parent(a)()
   ccall((:fmpq_mpoly_pow_ui, libflint), Nothing,
         (Ref{fmpq_mpoly}, Ref{fmpq_mpoly}, Int, Ref{FmpqMPolyRing}),
         z, a, b, parent(a))
   return z
end

function ^(a::fmpq_mpoly, b::fmpz)
   b < 0 && throw(DomainError(b, "Exponent must be non-negative"))
   z = parent(a)()
   ccall((:fmpq_mpoly_pow_fmpz, libflint), Nothing,
         (Ref{fmpq_mpoly}, Ref{fmpq_mpoly}, Ref{fmpz}, Ref{FmpqMPolyRing}),
         z, a, b, parent(a))
   return z
end

################################################################################
#
#   GCD
#
################################################################################

function gcd(a::fmpq_mpoly, b::fmpq_mpoly)
   check_parent(a, b)
   z = parent(a)()
   r = Bool(ccall((:fmpq_mpoly_gcd, libflint), Cint,
         (Ref{fmpq_mpoly}, Ref{fmpq_mpoly}, Ref{fmpq_mpoly}, Ref{FmpqMPolyRing}),
         z, a, b, a.parent))
   r == false && error("Unable to compute gcd")
   return z
end

################################################################################
#
#   Factorization and Square Root
#
################################################################################

function (F::Fac{fmpq_mpoly})(fac::fmpq_mpoly_factor, preserve_input::Bool = true)
   R = fac.parent
   empty!(F.fac)
   for i in 0:fac.num-1
      f = R()
      if preserve_input
         ccall((:fmpq_mpoly_factor_get_base, libflint), Nothing,
               (Ref{fmpq_mpoly}, Ref{fmpq_mpoly_factor}, Int, Ref{FmpqMPolyRing}),
               f, fac, i, R)
      else
         ccall((:fmpq_mpoly_factor_swap_base, libflint), Nothing,
               (Ref{fmpq_mpoly}, Ref{fmpq_mpoly_factor}, Int, Ref{FmpqMPolyRing}),
               f, fac, i, R)
      end
      F.fac[f] = ccall((:fmpq_mpoly_factor_get_exp_si, libflint), Int,
                       (Ref{fmpq_mpoly_factor}, Int, Ref{FmpqMPolyRing}),
                       fac, i, R)
   end
   c = fmpq()
   ccall((:fmpq_mpoly_factor_get_constant_fmpq, libflint), Nothing,
         (Ref{fmpq}, Ref{fmpq_mpoly_factor}),
         c, fac)
   F.unit = R(c)
   return F
end

function factor(a::fmpq_mpoly)
   R = parent(a)
   fac = fmpq_mpoly_factor(R)
   ok = ccall((:fmpq_mpoly_factor, libflint), Cint,
              (Ref{fmpq_mpoly_factor}, Ref{fmpq_mpoly}, Ref{FmpqMPolyRing}),
              fac, a, R)
   ok == 0 && error("unable to compute factorization")
   return (Fac{fmpq_mpoly}())(fac, false)
end

function factor_squarefree(a::fmpq_mpoly)
   R = parent(a)
   fac = fmpq_mpoly_factor(R)
   ok = ccall((:fmpq_mpoly_factor_squarefree, libflint), Cint,
              (Ref{fmpq_mpoly_factor}, Ref{fmpq_mpoly}, Ref{FmpqMPolyRing}),
              fac, a, R)
   ok == 0 && error("unable to compute factorization")
   return (Fac{fmpq_mpoly}())(fac, false)
end


function square_root(a::fmpq_mpoly)
   (flag, q) = issquare_with_square_root(a)
   !flag && error("Not a square in square_root")
   return q
end

function issquare(a::fmpq_mpoly)
   return Bool(ccall((:fmpq_mpoly_is_square, libflint), Cint,
                     (Ref{fmpq_mpoly}, Ref{FmpqMPolyRing}),
                     a, a.parent))
end

function issquare_with_square_root(a::fmpq_mpoly)
   q = parent(a)()
   flag = ccall((:fmpq_mpoly_sqrt, libflint), Cint,
                (Ref{fmpq_mpoly}, Ref{fmpq_mpoly}, Ref{FmpqMPolyRing}),
                q, a, a.parent)
   return (Bool(flag), q)
end

###############################################################################
#
#   Comparison
#
###############################################################################

function ==(a::fmpq_mpoly, b::fmpq_mpoly)
   check_parent(a, b)
   return Bool(ccall((:fmpq_mpoly_equal, libflint), Cint,
               (Ref{fmpq_mpoly}, Ref{fmpq_mpoly}, Ref{FmpqMPolyRing}),
               a, b, a.parent))
end

function Base.isless(a::fmpq_mpoly, b::fmpq_mpoly)
   (!ismonomial(a) || !ismonomial(b)) && error("Not monomials in comparison")
   return ccall((:fmpq_mpoly_cmp, libflint), Cint,
               (Ref{fmpq_mpoly}, Ref{fmpq_mpoly}, Ref{FmpqMPolyRing}),
               a, b, a.parent) < 0
end

###############################################################################
#
#   Ad hoc comparison
#
###############################################################################

function ==(a::fmpq_mpoly, b::fmpq)
   return Bool(ccall((:fmpq_mpoly_equal_fmpq, libflint), Cint,
                     (Ref{fmpq_mpoly}, Ref{fmpq}, Ref{FmpqMPolyRing}),
                     a, b, a.parent))
end

==(a::fmpq, b::fmpq_mpoly) = b == a

function ==(a::fmpq_mpoly, b::fmpz)
   return Bool(ccall((:fmpq_mpoly_equal_fmpz, libflint), Cint,
                     (Ref{fmpq_mpoly}, Ref{fmpz}, Ref{FmpqMPolyRing}),
                     a, b, a.parent))
end

==(a::fmpz, b::fmpq_mpoly) = b == a

function ==(a::fmpq_mpoly, b::Int)
   return Bool(ccall((:fmpq_mpoly_equal_si, libflint), Cint,
               (Ref{fmpq_mpoly}, Int, Ref{FmpqMPolyRing}),
               a, b, a.parent))
end

==(a::Int, b::fmpq_mpoly) = b == a

==(a::fmpq_mpoly, b::Integer) = a == fmpz(b)

==(a::Integer, b::fmpq_mpoly) = b == a

==(a::fmpq_mpoly, b::Rational{<:Integer}) = a == fmpq(b)

==(a::Rational{<:Integer}, b::fmpq_mpoly) = b == a

###############################################################################
#
#   Divisibility
#
###############################################################################

function divides(a::fmpq_mpoly, b::fmpq_mpoly)
   check_parent(a, b)
   if iszero(a)
      return true, zero(parent(a))
   end
   if iszero(b)
      return false, zero(parent(a))
   end
   z = parent(a)()
   d = ccall((:fmpq_mpoly_divides, libflint), Cint,
       (Ref{fmpq_mpoly}, Ref{fmpq_mpoly}, Ref{fmpq_mpoly}, Ref{FmpqMPolyRing}),
       z, a, b, a.parent)
   return isone(d), z
end

###############################################################################
#
#   Division with remainder
#
###############################################################################

function Base.div(a::fmpq_mpoly, b::fmpq_mpoly)
   check_parent(a, b)
   q = parent(a)()
   ccall((:fmpq_mpoly_div, libflint), Nothing,
       (Ref{fmpq_mpoly}, Ref{fmpq_mpoly},
        Ref{fmpq_mpoly}, Ref{FmpqMPolyRing}),
       q, a, b, a.parent)
   return q
end

function Base.divrem(a::fmpq_mpoly, b::fmpq_mpoly)
   check_parent(a, b)
   q = parent(a)()
   r = parent(a)()
   ccall((:fmpq_mpoly_divrem, libflint), Nothing,
       (Ref{fmpq_mpoly}, Ref{fmpq_mpoly}, Ref{fmpq_mpoly},
        Ref{fmpq_mpoly}, Ref{FmpqMPolyRing}),
       q, r, a, b, a.parent)
   return q, r
end

function Base.divrem(a::fmpq_mpoly, b::Array{fmpq_mpoly, 1})
   len = length(b)
   q = [parent(a)() for i in 1:len]
   r = parent(a)()
   ccall((:fmpq_mpoly_divrem_ideal, libflint), Nothing,
         (Ptr{Ref{fmpq_mpoly}}, Ref{fmpq_mpoly}, Ref{fmpq_mpoly},
          Ptr{Ref{fmpq_mpoly}}, Int, Ref{FmpqMPolyRing}),
       q, r, a, b, len, a.parent)
   return q, r
end

###############################################################################
#
#   Exact division
#
###############################################################################

function divexact(a::fmpq_mpoly, b::fmpq_mpoly)
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

function derivative(a::fmpq_mpoly, i::Int)
   n = nvars(parent(a))
   (i <= 0 || i > n) && error("Index must be between 1 and $n")
   z = parent(a)()
   ccall((:fmpq_mpoly_derivative, libflint), Nothing,
         (Ref{fmpq_mpoly}, Ref{fmpq_mpoly}, Int, Ref{FmpqMPolyRing}),
         z, a, i - 1, parent(a))
   return z
end

function integral(a::fmpq_mpoly, i::Int)
   n = nvars(parent(a))
   (i <= 0 || i > n) && error("Index must be between 1 and $n")
   z = parent(a)()
   ccall((:fmpq_mpoly_integral, libflint), Nothing,
         (Ref{fmpq_mpoly}, Ref{fmpq_mpoly}, Int, Ref{FmpqMPolyRing}),
         z, a, i - 1, parent(a))
   return z
end

###############################################################################
#
#   Evaluation
#
###############################################################################

function evaluate(a::fmpq_mpoly, b::Vector{fmpq})
   length(b) != nvars(parent(a)) && error("Vector size incorrect in evaluate")
   z = fmpq()
   GC.@preserve b ccall((:fmpq_mpoly_evaluate_all_fmpq, libflint), Nothing,
         (Ref{fmpq}, Ref{fmpq_mpoly}, Ptr{fmpq}, Ref{FmpqMPolyRing}),
            z, a, b, parent(a))
   return z
end

function evaluate(a::fmpq_mpoly, b::Vector{fmpz})
   fmpq_vec = [fmpq(s) for s in b]
   return evaluate(a, fmpq_vec)
end

function evaluate(a::fmpq_mpoly, b::Vector{<:Integer})
   fmpq_vec = [fmpq(s) for s in b]
   return evaluate(a, fmpq_vec)
end

function (a::fmpq_mpoly)(vals::fmpq...)
   length(vals) != nvars(parent(a)) && error("Number of variables does not match number o
f values")
   return evaluate(a, [vals...])
end

function (a::fmpq_mpoly)(vals::Integer...)
   length(vals) != nvars(parent(a)) && error("Number of variables does not match number o
f values")
   return evaluate(a, [vals...])
end

function (a::fmpq_mpoly)(vals::Union{NCRingElem, RingElement}...)
   length(vals) != nvars(parent(a)) && error("Number of variables does not match number o
f values")
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
   U = Array{Any, 1}(undef, length(vals))
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

###############################################################################
#
#   Unsafe functions
#
###############################################################################

function zero!(a::fmpq_mpoly)
    ccall((:fmpq_mpoly_zero, libflint), Nothing,
         (Ref{fmpq_mpoly}, Ref{FmpqMPolyRing}), a, a.parent)
    return a
end

function add!(a::fmpq_mpoly, b::fmpq_mpoly, c::fmpq_mpoly)
   ccall((:fmpq_mpoly_add, libflint), Nothing,
         (Ref{fmpq_mpoly}, Ref{fmpq_mpoly},
          Ref{fmpq_mpoly}, Ref{FmpqMPolyRing}), a, b, c, a.parent)
   return a
end

function addeq!(a::fmpq_mpoly, b::fmpq_mpoly)
   ccall((:fmpq_mpoly_add, libflint), Nothing,
         (Ref{fmpq_mpoly}, Ref{fmpq_mpoly},
          Ref{fmpq_mpoly}, Ref{FmpqMPolyRing}), a, a, b, a.parent)
   return a
end

function mul!(a::fmpq_mpoly, b::fmpq_mpoly, c::fmpq_mpoly)
   ccall((:fmpq_mpoly_mul, libflint), Nothing,
         (Ref{fmpq_mpoly}, Ref{fmpq_mpoly},
          Ref{fmpq_mpoly}, Ref{FmpqMPolyRing}), a, b, c, a.parent)
   return a
end

# Set the n-th coefficient of a to c. If zero coefficients are inserted, they
# must be removed with combine_like_terms!
function setcoeff!(a::fmpq_mpoly, n::Int, c::fmpq)
   if n > length(a)
      ccall((:fmpq_mpoly_resize, libflint), Nothing,
            (Ref{fmpq_mpoly}, Int, Ref{FmpqMPolyRing}), a, n, a.parent)
   end
   ccall((:fmpq_mpoly_set_term_coeff_fmpq, libflint), Nothing,
         (Ref{fmpq_mpoly}, Int, Ref{fmpq}, Ref{FmpqMPolyRing}),
         a, n - 1, c, a.parent)
   return a
end

# Set the i-th coefficient of a to c. If zero coefficients are inserted, they
# must be removed with combine_like_terms!
setcoeff!(a::fmpq_mpoly, i::Int, c::fmpz) = setcoeff!(a, i, fmpq(c))

# Set the i-th coefficient of a to c. If zero coefficients are inserted, they
# must be removed with combine_like_terms!
setcoeff!(a::fmpq_mpoly, i::Int, c::Integer) = setcoeff!(a, i, fmpq(c))

# Set the i-th coefficient of a to c. If zero coefficients are inserted, they
# must be removed with combine_like_terms!
setcoeff!(a::fmpq_mpoly, i::Int, c::Rational{<:Integer}) =
   setcoeff!(a, i, fmpq(c))

# Remove zero terms and combine adjacent terms if they have the same monomial
# no sorting is performed
function combine_like_terms!(a::fmpq_mpoly)
   ccall((:fmpq_mpoly_combine_like_terms, libflint), Nothing,
         (Ref{fmpq_mpoly}, Ref{FmpqMPolyRing}), a, a.parent)
   return a
end

###############################################################################
#
#   Manipulating terms and monomials
#
###############################################################################

# Return true if the exponents of the i-th exp. vector fit into UInts
function exponent_vector_fits_ui(a::fmpq_mpoly, i::Int)
   b = ccall((:fmpq_mpoly_term_exp_fits_ui, libflint), Cint,
             (Ref{fmpq_mpoly}, Int, Ref{FmpqMPolyRing}), a, i - 1, a.parent)
      return Bool(b)
end

# Return true if the exponents of the i-th exp. vector fit into UInts
function exponent_vector_fits_int(a::fmpq_mpoly, i::Int)
   b = ccall((:fmpq_mpoly_term_exp_fits_si, libflint), Cint,
             (Ref{fmpq_mpoly}, Int, Ref{FmpqMPolyRing}), a, i - 1, a.parent)
   return Bool(b)
end

# Return Julia array of UInt's corresponding to exponent vector of i-th term
function exponent_vector_ui(a::fmpq_mpoly, i::Int)
   z = Vector{UInt}(undef, nvars(parent(a)))
   ccall((:fmpq_mpoly_get_term_exp_ui, libflint), Nothing,
         (Ptr{UInt}, Ref{fmpq_mpoly}, Int, Ref{FmpqMPolyRing}),
      z, a, i - 1, parent(a))
   return z
end

# Return Julia array of Int's corresponding to exponent vector of i-th term
function exponent_vector(a::fmpq_mpoly, i::Int)
   exponent_vector_fits_int(a, i) ||
      throw(DomainError(term(a, i), "exponents don't fit in `Int` (try exponent_vector_fmpz)"))
   z = Vector{Int}(undef, nvars(parent(a)))
   ccall((:fmpq_mpoly_get_term_exp_si, libflint), Nothing,
         (Ptr{Int}, Ref{fmpq_mpoly}, Int, Ref{FmpqMPolyRing}),
      z, a, i - 1, parent(a))
   return z
end

# Return Julia array of fmpz's corresponding to exponent vector of i-th term
function exponent_vector_fmpz(a::fmpq_mpoly, i::Int)
   n = nvars(parent(a))
   z = Vector{fmpz}(undef, n)
   for j in 1:n
      z[j] = fmpz()
   end
   ccall((:fmpq_mpoly_get_term_exp_fmpz, libflint), Nothing,
         (Ptr{Ref{fmpz}}, Ref{fmpq_mpoly}, Int, Ref{FmpqMPolyRing}),
         z, a, i - 1, parent(a))
   return z
end

# Return a generator for exponent vectors of $a$
function exponent_vectors_fmpz(a::fmpq_mpoly)
   return (exponent_vector_fmpz(a, i) for i in 1:length(a))
end

# Set exponent of n-th term to given vector of UInt's
# No sort is performed, so this is unsafe. These are promoted to fmpz's if
# they don't fit into 31/63 bits
function set_exponent_vector!(a::fmpq_mpoly, n::Int, exps::Vector{UInt})
   if n > length(a)
      ccall((:fmpq_mpoly_resize, libflint), Nothing,
            (Ref{fmpq_mpoly}, Int, Ref{FmpqMPolyRing}), a, n, a.parent)
   end
   ccall((:fmpq_mpoly_set_term_exp_ui, libflint), Nothing,
         (Ref{fmpq_mpoly}, Int, Ptr{UInt}, Ref{FmpqMPolyRing}),
      a, n - 1, exps, parent(a))
   return a
end

# Set exponent of n-th term to given vector of Int's
# No sort is performed, so this is unsafe. The Int's must be positive, but
# no check is performed
function set_exponent_vector!(a::fmpq_mpoly, n::Int, exps::Vector{Int})
   if n > length(a)
      ccall((:fmpq_mpoly_resize, libflint), Nothing,
            (Ref{fmpq_mpoly}, Int, Ref{FmpqMPolyRing}), a, n, a.parent)
   end
   ccall((:fmpq_mpoly_set_term_exp_ui, libflint), Nothing,
         (Ref{fmpq_mpoly}, Int, Ptr{Int}, Ref{FmpqMPolyRing}),
      a, n - 1, exps, parent(a))
   return a
end

# Set exponent of n-th term to given vector of fmpz's
# No sort is performed, so this is unsafe
function set_exponent_vector!(a::fmpq_mpoly, n::Int, exps::Vector{fmpz})
   if n > length(a)
      ccall((:fmpq_mpoly_resize, libflint), Nothing,
            (Ref{fmpq_mpoly}, Int, Ref{FmpqMPolyRing}), a, n, a.parent)
   end
   @GC.preserve exps ccall((:fmpq_mpoly_set_term_exp_fmpz, libflint), Nothing,
         (Ref{fmpq_mpoly}, Int, Ptr{fmpz}, Ref{FmpqMPolyRing}),
      a, n - 1, exps, parent(a))
   return a
end

# Return j-th coordinate of i-th exponent vector
function exponent(a::fmpq_mpoly, i::Int, j::Int)
   (j < 1 || j > nvars(parent(a))) && error("Invalid variable index")
   return ccall((:fmpq_mpoly_get_term_var_exp_ui, libflint), Int,
                (Ref{fmpq_mpoly}, Int, Int, Ref{FmpqMPolyRing}),
                 a, i - 1, j - 1, a.parent)
end

# Return the coefficient of the term with the given exponent vector
# Return zero if there is no such term
function coeff(a::fmpq_mpoly, exps::Vector{UInt})
   z = fmpq()
   ccall((:fmpq_mpoly_get_coeff_fmpq_ui, libflint), Nothing,
         (Ref{fmpq}, Ref{fmpq_mpoly}, Ptr{UInt}, Ref{FmpqMPolyRing}),
      z, a, exps, parent(a))
   return z
end

# Return the coefficient of the term with the given exponent vector
# Return zero if there is no such term
function coeff(a::fmpq_mpoly, exps::Vector{Int})
   z = fmpq()
   ccall((:fmpq_mpoly_get_coeff_fmpq_ui, libflint), Nothing,
         (Ref{fmpq}, Ref{fmpq_mpoly}, Ptr{Int}, Ref{FmpqMPolyRing}),
      z, a, exps, parent(a))
   return z
end

# Set the coefficient of the term with the given exponent vector to the
# given fmpq. Removal of a zero term is performed.
function setcoeff!(a::fmpq_mpoly, exps::Vector{UInt}, b::fmpq)
   ccall((:fmpq_mpoly_set_coeff_fmpq_ui, libflint), Nothing,
         (Ref{fmpq_mpoly}, Ref{fmpq}, Ptr{UInt}, Ref{FmpqMPolyRing}),
      a, b, exps, parent(a))
   return a
end

# Set the coefficient of the term with the given exponent vector to the
# given fmpq. Removal of a zero term is performed.
function setcoeff!(a::fmpq_mpoly, exps::Vector{Int}, b::fmpq)
   ccall((:fmpq_mpoly_set_coeff_fmpq_ui, libflint), Nothing,
         (Ref{fmpq_mpoly}, Ref{fmpq}, Ptr{Int}, Ref{FmpqMPolyRing}),
      a, b, exps, parent(a))
   return a
end

# Set the coefficient of the term with the given exponent vector to the
# given integer. Removal of a zero term is performed.
setcoeff!(a::fmpq_mpoly, exps::Vector{Int}, b::Rational{<:Integer}) =
   setcoeff!(a, exps, fmpq(b))

# Set the coefficient of the term with the given exponent vector to the
# given fmpz. Removal of a zero term is performed.
setcoeff!(a::fmpq_mpoly, exps::Vector{Int}, b::fmpz) =
   setcoeff!(a, exps, fmpq(b))

# Set the coefficient of the term with the given exponent vector to the
# given integer. Removal of a zero term is performed.
setcoeff!(a::fmpq_mpoly, exps::Vector{Int}, b::Integer) =
   setcoeff!(a, exps, fmpq(b))

# Sort the terms according to the ordering. This is only needed if unsafe
# functions such as those above have been called and terms have been inserted
# out of order. Note that like terms are not combined and zeros are not
# removed. For that, call combine_like_terms!
function sort_terms!(a::fmpq_mpoly)
   ccall((:fmpq_mpoly_sort_terms, libflint), Nothing,
         (Ref{fmpq_mpoly}, Ref{FmpqMPolyRing}), a, a.parent)
   return a
end

# Return the i-th term of the polynomial, as a polynomial
function term(a::fmpq_mpoly, i::Int)
   z = parent(a)()
   ccall((:fmpq_mpoly_get_term, libflint), Nothing,
         (Ref{fmpq_mpoly}, Ref{fmpq_mpoly}, Int, Ref{FmpqMPolyRing}),
          z, a, i - 1, a.parent)
   return z
end

# Return the i-th monomial of the polynomial, as a polynomial
function monomial(a::fmpq_mpoly, i::Int)
   z = parent(a)()
   ccall((:fmpq_mpoly_get_term_monomial, libflint), Nothing,
         (Ref{fmpq_mpoly}, Ref{fmpq_mpoly}, Int, Ref{FmpqMPolyRing}),
          z, a, i - 1, a.parent)
   return z
end

# Sets the given polynomial m to the i-th monomial of the polynomial
function monomial!(m::fmpq_mpoly, a::fmpq_mpoly, i::Int)
   ccall((:fmpq_mpoly_get_term_monomial, libflint), Nothing,
         (Ref{fmpq_mpoly}, Ref{fmpq_mpoly}, Int, Ref{FmpqMPolyRing}),
          m, a, i - 1, a.parent)
   return m
end

###############################################################################
#
#   Promotion rules
#
###############################################################################

promote_rule(::Type{fmpq_mpoly}, ::Type{V}) where {V <: Integer} = fmpq_mpoly

promote_rule(::Type{fmpq_mpoly}, ::Type{Rational{V}}) where {V <: Integer} = fmpq_mpoly

promote_rule(::Type{fmpq_mpoly}, ::Type{fmpz}) = fmpq_mpoly

promote_rule(::Type{fmpq_mpoly}, ::Type{fmpq}) = fmpq_mpoly

###############################################################################
#
#   Parent object call overload
#
###############################################################################

function (R::FmpqMPolyRing)()
   z = fmpq_mpoly(R)
   return z
end

function (R::FmpqMPolyRing)(b::fmpq)
   z = fmpq_mpoly(R, b)
   return z
end

function (R::FmpqMPolyRing)(b::fmpz)
   z = fmpq_mpoly(R, b)
   return z
end

function (R::FmpqMPolyRing)(b::Int)
   z = fmpq_mpoly(R, b)
   return z
end

function (R::FmpqMPolyRing)(b::UInt)
   z = fmpq_mpoly(R, b)
   return z
end

function (R::FmpqMPolyRing)(b::Integer)
   return R(fmpz(b))
end

function (R::FmpqMPolyRing)(b::Rational{<:Integer})
   return R(fmpq(b))
end

function (R::FmpqMPolyRing)(a::fmpq_mpoly)
   parent(a) != R && error("Unable to coerce polynomial")
   return a
end

# Create poly with given array of coefficients and array of exponent vectors (sorting is performed)
function (R::FmpqMPolyRing)(a::Vector{fmpq}, b::Vector{Vector{T}}) where {T <: Union{fmpz, UInt}}
   length(a) != length(b) && error("Coefficient and exponent vector must have the same length")

   for i in 1:length(b)
     length(b[i]) != nvars(R) && error("Exponent vector $i has length $(length(b[i])) (expected $(nvars(R))")
   end

   z = fmpq_mpoly(R, a, b)
   return z
end

# Create poly with given array of coefficients and array of exponent vectors (sorting is performed)
function (R::FmpqMPolyRing)(a::Vector{fmpq}, b::Vector{Vector{Int}})
   length(a) != length(b) && error("Coefficient and exponent vector must have the same length")

   for i in 1:length(b)
      length(b[i]) != nvars(R) && error("Exponent vector $i has length $(length(b[i])) (expected $(nvars(R)))")
   end

   z = fmpq_mpoly(R, a, b)
   return z
end

# Create poly with given array of coefficients and array of exponent vectors (sorting is performed)
function (R::FmpqMPolyRing)(a::Vector{Any}, b::Vector{Vector{T}}) where T
   n = nvars(R)
   length(a) != length(b) && error("Coefficient and exponent vector must have the same length")
   newa = map(FlintQQ, a)
   newb = map(x -> map(FlintZZ, x), b)
   newaa = convert(Vector{fmpq}, newa)
   newbb = convert(Vector{Vector{fmpz}}, newb)

   for i in 1:length(newbb)
      length(newbb[i]) != n && error("Exponent vector $i has length $(length(newbb[i])) (expected $(nvars(R)))")
   end

   return R(newaa, newbb)
end

###############################################################################
#
#   PolynomialRing constructor
#
###############################################################################

function PolynomialRing(R::FlintRationalField, s::Array{String, 1}; cached::Bool = true, ordering::Symbol = :lex)
   U = [Symbol(x) for x in s]
   parent_obj = FmpqMPolyRing(U, ordering, cached)
   return tuple(parent_obj, gens(parent_obj))
end
