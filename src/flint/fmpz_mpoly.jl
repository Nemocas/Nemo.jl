###############################################################################
#
#   fmpz_mpoly.jl : Flint multivariate polynomials over fmpz
#
###############################################################################

export FmpzMPolyRing, fmpz_mpoly, trailing_coefficient

###############################################################################
#
#   Data type and parent object methods
#
###############################################################################

parent_type(::Type{fmpz_mpoly}) = FmpzMPolyRing

elem_type(::Type{FmpzMPolyRing}) = fmpz_mpoly

elem_type(::FmpzMPolyRing) = fmpz_mpoly

symbols(a::FmpzMPolyRing) = a.S

parent(a::fmpz_mpoly) = a.parent

function check_parent(a::fmpz_mpoly, b::fmpz_mpoly)
   parent(a) != parent(b) &&
      error("Incompatible polynomial rings in polynomial operation")
end

nvars(a::FmpzMPolyRing) = ccall((:fmpz_mpoly_ctx_nvars, libflint), Int,
                                (Ref{FmpzMPolyRing}, ), a)

base_ring(a::FmpzMPolyRing) = a.base_ring

base_ring(f::fmpz_mpoly) = f.parent.base_ring

function ordering(a::FmpzMPolyRing)
   b = ccall((:fmpz_mpoly_ctx_ord, libflint), Cint, (Ref{FmpzMPolyRing}, ), a)
   return flint_orderings[b + 1]
end

function gens(R::FmpzMPolyRing)
   A = Vector{fmpz_mpoly}(undef, R.nvars)
   for i = 1:R.nvars
      z = R()
      ccall((:fmpz_mpoly_gen, libflint), Nothing,
            (Ref{fmpz_mpoly}, Int, Ref{FmpzMPolyRing}), z, i - 1, R)
      A[i] = z
   end
   return A
end

function gen(R::FmpzMPolyRing, i::Int)
   n = nvars(R)
   (i <= 0 || i > n) && error("Index must be between 1 and $n")
   z = R()
   ccall((:fmpz_mpoly_gen, libflint), Nothing,
         (Ref{fmpz_mpoly}, Int, Ref{FmpzMPolyRing}), z, i - 1, R)
   return z
end

function isgen(a::fmpz_mpoly, i::Int)
   n = nvars(parent(a))
   (i <= 0 || i > n) && error("Index must be between 1 and $n")
   R = parent(a)
   return Bool(ccall((:fmpz_mpoly_is_gen, libflint), Cint,
                     (Ref{fmpz_mpoly}, Int, Ref{FmpzMPolyRing}),
                     a, i - 1, a.parent))
end

function isgen(a::fmpz_mpoly)
   n = nvars(parent(a))
   for i in 1:n
      isgen(a, i) && return true
   end
   return false
end

function deepcopy_internal(a::fmpz_mpoly, dict::IdDict)
   z = parent(a)()
   ccall((:fmpz_mpoly_set, libflint), Nothing,
         (Ref{fmpz_mpoly}, Ref{fmpz_mpoly}, Ref{FmpzMPolyRing}),
         z, a, a.parent)
   return z
end

function length(a::fmpz_mpoly)
   n = ccall((:fmpz_mpoly_length, libflint), Int, (Ref{fmpz_mpoly}, ), a)
   return n
end

function one(R::FmpzMPolyRing)
   z = R()
   ccall((:fmpz_mpoly_one, libflint), Nothing,
         (Ref{fmpz_mpoly}, Ref{FmpzMPolyRing}), z, R)
   return z
end

function zero(R::FmpzMPolyRing)
   z = R()
   ccall((:fmpz_mpoly_zero, libflint), Nothing,
         (Ref{fmpz_mpoly}, Ref{FmpzMPolyRing}), z, R)
   return z
end

function isone(a::fmpz_mpoly)
   b = ccall((:fmpz_mpoly_is_one, libflint), Cint,
             (Ref{fmpz_mpoly}, Ref{FmpzMPolyRing}), a, a.parent)
   return Bool(b)
end

function iszero(a::fmpz_mpoly)
   b = ccall((:fmpz_mpoly_is_zero, libflint), Cint,
             (Ref{fmpz_mpoly}, Ref{FmpzMPolyRing}), a, a.parent)
   return Bool(b)
end

function ismonomial(a::fmpz_mpoly)
   return length(a) == 1 && coeff(a, 1) == 1
end

function isterm(a::fmpz_mpoly)
   return length(a) == 1
end

function isunit(a::fmpz_mpoly)
   return length(a) == 1 && total_degree(a) == 0 && isunit(coeff(a, 1))
end

function isconstant(a::fmpz_mpoly)
   b = ccall((:fmpz_mpoly_is_fmpz, libflint), Cint,
             (Ref{fmpz_mpoly}, Ref{FmpzMPolyRing}), a, parent(a))
   return Bool(b)
end

################################################################################
#
#  Getting coefficients
#
################################################################################

function coeff(a::fmpz_mpoly, i::Int)
   z = fmpz()
   n = length(a)
   (i < 1 || i > n) && error("Index must be between 1 and $(length(a))")
   ccall((:fmpz_mpoly_get_term_coeff_fmpz, libflint), Nothing,
         (Ref{fmpz}, Ref{fmpz_mpoly}, Int, Ref{FmpzMPolyRing}),
         z, a, i - 1, a.parent)
   return z
end

function coeff(a::fmpz_mpoly, b::fmpz_mpoly)
   check_parent(a, b)
   !isone(length(b)) && error("Second argument must be a monomial")
   z = fmpz()
   ccall((:fmpz_mpoly_get_coeff_fmpz_monomial, libflint), Nothing,
         (Ref{fmpz}, Ref{fmpz_mpoly}, Ref{fmpz_mpoly}, Ref{FmpzMPolyRing}),
         z, a, b, parent(a))
   return z
end

function trailing_coefficient(p::fmpz_poly)
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
function degree(a::fmpz_mpoly, i::Int)
   n = nvars(parent(a))
   (i <= 0 || i > n) && error("Index must be between 1 and $n")
   d = ccall((:fmpz_mpoly_degree_si, libflint), Int,
             (Ref{fmpz_mpoly}, Int, Ref{FmpzMPolyRing}), a, i - 1, a.parent)
   return d
end

# Degree in the i-th variable as an fmpz
function degree_fmpz(a::fmpz_mpoly, i::Int)
   n = nvars(parent(a))
   (i <= 0 || i > n) && error("Index must be between 1 and $n")
   d = fmpz()
   ccall((:fmpz_mpoly_degree_fmpz, libflint), Nothing,
         (Ref{fmpz}, Ref{fmpz_mpoly}, Int, Ref{FmpzMPolyRing}),
         d, a, i - 1, a.parent)
   return d
end

# Return true if degrees fit into an Int
function degrees_fit_int(a::fmpz_mpoly)
   b = ccall((:fmpz_mpoly_degrees_fit_si, libflint), Cint,
             (Ref{fmpz_mpoly}, Ref{FmpzMPolyRing}), a, a.parent)
   return Bool(b)
end

# Return an array of the max degrees in each variable
function degrees(a::fmpz_mpoly)
   degs = Vector{Int}(undef, nvars(parent(a)))
   ccall((:fmpz_mpoly_degrees_si, libflint), Nothing,
         (Ptr{Int}, Ref{fmpz_mpoly}, Ref{FmpzMPolyRing}),
         degs, a, a.parent)
   return degs
end

# Return an array of the max degrees as fmpzs in each variable
function degrees_fmpz(a::fmpz_mpoly)
   n = nvars(parent(a))
degs = Vector{fmpz}(undef, n)
   for i in 1:n
      degs[i] = fmpz()
   end
   ccall((:fmpz_mpoly_degrees_fmpz, libflint), Nothing,
         (Ptr{Ref{fmpz}}, Ref{fmpz_mpoly}, Ref{FmpzMPolyRing}),
         degs, a, a.parent)
   return degs
end

# Return true if degree fits into an Int
function total_degree_fits_int(a::fmpz_mpoly)
      b = ccall((:fmpz_mpoly_total_degree_fits_si, libflint), Cint,
                (Ref{fmpz_mpoly}, Ref{FmpzMPolyRing}), a, a.parent)
      return Bool(b)
   end

# Total degree as an Int
function total_degree(a::fmpz_mpoly)
   d = ccall((:fmpz_mpoly_total_degree_si, libflint), Int,
             (Ref{fmpz_mpoly}, Ref{FmpzMPolyRing}), a, a.parent)
   return d
end

# Total degree as an fmpz
function total_degree_fmpz(a::fmpz_mpoly)
   d = fmpz()
   ccall((:fmpz_mpoly_total_degree_fmpz, libflint), Nothing,
         (Ref{fmpz}, Ref{fmpz_mpoly}, Ref{FmpzMPolyRing}),
            d, a, a.parent)
   return d
end

characteristic(::FmpzMPolyRing) = 0

###############################################################################
#
#   Multivariable coefficient polynomials
#
###############################################################################

function coeff(a::fmpz_mpoly, vars::Vector{Int}, exps::Vector{Int})
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
         (Ref{fmpz_mpoly}, Ref{fmpz_mpoly}, Ptr{Int},
          Ptr{Int}, Int, Ref{FmpzMPolyRing}),
          z, a, vars, exps, length(vars), a.parent)
   return z
end

###############################################################################
#
#   String I/O
#
###############################################################################

function show(io::IO, p::FmpzMPolyRing)
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

function -(a::fmpz_mpoly)
   z = parent(a)()
   ccall((:fmpz_mpoly_neg, libflint), Nothing,
       (Ref{fmpz_mpoly}, Ref{fmpz_mpoly}, Ref{FmpzMPolyRing}),
       z, a, a.parent)
   return z
end

function +(a::fmpz_mpoly, b::fmpz_mpoly)
   check_parent(a, b)
   z = parent(a)()
   ccall((:fmpz_mpoly_add, libflint), Nothing,
       (Ref{fmpz_mpoly}, Ref{fmpz_mpoly}, Ref{fmpz_mpoly}, Ref{FmpzMPolyRing}),
       z, a, b, a.parent)
   return z
end

function -(a::fmpz_mpoly, b::fmpz_mpoly)
   check_parent(a, b)
   z = parent(a)()
   ccall((:fmpz_mpoly_sub, libflint), Nothing,
       (Ref{fmpz_mpoly}, Ref{fmpz_mpoly}, Ref{fmpz_mpoly}, Ref{FmpzMPolyRing}),
       z, a, b, a.parent)
   return z
end

function *(a::fmpz_mpoly, b::fmpz_mpoly)
   check_parent(a, b)
   z = parent(a)()
   ccall((:fmpz_mpoly_mul, libflint), Nothing,
       (Ref{fmpz_mpoly}, Ref{fmpz_mpoly}, Ref{fmpz_mpoly}, Ref{FmpzMPolyRing}),
       z, a, b, a.parent)
   return z
end

###############################################################################
#
#   Ad hoc arithmetic
#
###############################################################################

for (jT, cN, cT) in ((fmpz, :fmpz, Ref{fmpz}), (Int, :si, Int))
   @eval begin
      function +(a::fmpz_mpoly, b::($jT))
         z = parent(a)()
         ccall(($(string(:fmpz_mpoly_add_, cN)), libflint), Nothing,
               (Ref{fmpz_mpoly}, Ref{fmpz_mpoly}, ($cT), Ref{FmpzMPolyRing}),
               z, a, b, parent(a))
         return z
      end

      +(a::($jT), b::fmpz_mpoly) = b + a

      function -(a::fmpz_mpoly, b::($jT))
         z = parent(a)()
         ccall(($(string(:fmpz_mpoly_sub_, cN)), libflint), Nothing,
               (Ref{fmpz_mpoly}, Ref{fmpz_mpoly}, ($cT), Ref{FmpzMPolyRing}),
               z, a, b, parent(a))
         return z
      end

      -(a::($jT), b::fmpz_mpoly) = - (b - a)

      function *(a::fmpz_mpoly, b::($jT))
         z = parent(a)()
         ccall(($(string(:fmpz_mpoly_scalar_mul_, cN)), libflint), Nothing,
               (Ref{fmpz_mpoly}, Ref{fmpz_mpoly}, ($cT), Ref{FmpzMPolyRing}),
               z, a, b, parent(a))
         return z
      end

      *(a::($jT), b::fmpz_mpoly) = b * a

      function divexact(a::fmpz_mpoly, b::($jT))
         z = parent(a)()
         checked = true
         if checked
            divides = Bool(ccall(($(string(:fmpz_mpoly_scalar_divides_, cN)), libflint), Cint,
                                 (Ref{fmpz_mpoly}, Ref{fmpz_mpoly}, ($cT), Ref{FmpzMPolyRing}),
                                 z, a, b, parent(a)))
            divides || error("Division is not exact in divexact")
         else
            ccall(($(string(:fmpz_mpoly_scalar_divexact_, cN)), libflint), Nothing,
                  (Ref{fmpz_mpoly}, Ref{fmpz_mpoly}, ($cT), Ref{FmpzMPolyRing}),
                  z, a, b, parent(a))
         end
         return z
      end
   end
end

+(a::fmpz_mpoly, b::Integer) = a + fmpz(b)

+(a::Integer, b::fmpz_mpoly) = b + a

-(a::fmpz_mpoly, b::Integer) = a - fmpz(b)

-(a::Integer, b::fmpz_mpoly) = -(b - a)

*(a::fmpz_mpoly, b::Integer) = a * fmpz(b)

*(a::Integer, b::fmpz_mpoly) = b * a

divexact(a::fmpz_mpoly, b::Integer) = divexact(a, fmpz(b))

###############################################################################
#
#   Powering
#
###############################################################################

function ^(a::fmpz_mpoly, b::Int)
   b < 0 && throw(DomainError(b, "Exponent must be non-negative"))
   z = parent(a)()
   ccall((:fmpz_mpoly_pow_ui, libflint), Nothing,
         (Ref{fmpz_mpoly}, Ref{fmpz_mpoly}, Int, Ref{FmpzMPolyRing}),
         z, a, b, parent(a))
   return z
end

function ^(a::fmpz_mpoly, b::fmpz)
   b < 0 && throw(DomainError(b, "Exponent must be non-negative"))
   z = parent(a)()
   ccall((:fmpz_mpoly_pow_fmpz, libflint), Nothing,
         (Ref{fmpz_mpoly}, Ref{fmpz_mpoly}, Ref{fmpz}, Ref{FmpzMPolyRing}),
         z, a, b, parent(a))
   return z
end

################################################################################
#
#   GCD
#
################################################################################

function gcd(a::fmpz_mpoly, b::fmpz_mpoly)
   check_parent(a, b)
   z = parent(a)()
   r = Bool(ccall((:fmpz_mpoly_gcd, libflint), Cint,
         (Ref{fmpz_mpoly}, Ref{fmpz_mpoly}, Ref{fmpz_mpoly}, Ref{FmpzMPolyRing}),
         z, a, b, a.parent))
   r == false && error("Unable to compute gcd")
   return z
end

################################################################################
#
#   Factorization and Square Root
#
################################################################################

function (::Type{Fac{fmpz_mpoly}})(fac::fmpz_mpoly_factor, preserve_input::Bool = true)
   R = fac.parent
   F = Fac{fmpz_mpoly}()
   empty!(F.fac)
   for i in 0:fac.num-1
      f = R()
      if preserve_input
         ccall((:fmpz_mpoly_factor_get_base, libflint), Nothing,
               (Ref{fmpz_mpoly}, Ref{fmpz_mpoly_factor}, Int, Ref{FmpzMPolyRing}),
               f, fac, i, R)
      else
         ccall((:fmpz_mpoly_factor_swap_base, libflint), Nothing,
               (Ref{fmpz_mpoly}, Ref{fmpz_mpoly_factor}, Int, Ref{FmpzMPolyRing}),
               f, fac, i, R)
      end
      F.fac[f] = ccall((:fmpz_mpoly_factor_get_exp_si, libflint), Int,
                       (Ref{fmpz_mpoly_factor}, Int, Ref{FmpzMPolyRing}),
                       fac, i, R)
   end
   c = fmpz()
   ccall((:fmpz_mpoly_factor_get_constant_fmpz, libflint), Nothing,
         (Ref{fmpz}, Ref{fmpz_mpoly_factor}),
         c, fac)
   F.unit = R(c)
   return F
end

function factor(a::fmpz_mpoly)
   R = parent(a)
   fac = fmpz_mpoly_factor(R)
   ok = ccall((:fmpz_mpoly_factor, libflint), Cint,
              (Ref{fmpz_mpoly_factor}, Ref{fmpz_mpoly}, Ref{FmpzMPolyRing}),
              fac, a, R)
   ok == 0 && error("unable to compute factorization")
   return Fac{fmpz_mpoly}(fac, false)
end

function factor_squarefree(a::fmpz_mpoly)
   R = parent(a)
   fac = fmpz_mpoly_factor(R)
   ok = ccall((:fmpz_mpoly_factor_squarefree, libflint), Cint,
              (Ref{fmpz_mpoly_factor}, Ref{fmpz_mpoly}, Ref{FmpzMPolyRing}),
              fac, a, R)
   ok == 0 && error("unable to compute factorization")
   return Fac{fmpz_mpoly}(fac, false)
end


function square_root(a::fmpz_mpoly)
   (flag, q) = issquare_with_square_root(a)
   !flag && error("Not a square in square_root")
   return q
end

function issquare(a::fmpz_mpoly)
   return Bool(ccall((:fmpz_mpoly_is_square, libflint), Cint,
                     (Ref{fmpz_mpoly}, Ref{FmpzMPolyRing}),
                     a, a.parent))
end

function issquare_with_square_root(a::fmpz_mpoly)
   q = parent(a)()
   flag = ccall((:fmpz_mpoly_sqrt, libflint), Cint,
                (Ref{fmpz_mpoly}, Ref{fmpz_mpoly}, Ref{FmpzMPolyRing}),
                q, a, a.parent)
   return (Bool(flag), q)
end

###############################################################################
#
#   Comparison
#
###############################################################################

function ==(a::fmpz_mpoly, b::fmpz_mpoly)
   check_parent(a, b)
   return Bool(ccall((:fmpz_mpoly_equal, libflint), Cint,
               (Ref{fmpz_mpoly}, Ref{fmpz_mpoly}, Ref{FmpzMPolyRing}),
               a, b, a.parent))
end

function Base.isless(a::fmpz_mpoly, b::fmpz_mpoly)
   (!ismonomial(a) || !ismonomial(b)) && error("Not monomials in comparison")
   return ccall((:fmpz_mpoly_cmp, libflint), Cint,
               (Ref{fmpz_mpoly}, Ref{fmpz_mpoly}, Ref{FmpzMPolyRing}),
               a, b, a.parent) < 0
end

###############################################################################
#
#   Ad hoc comparison
#
###############################################################################

function ==(a::fmpz_mpoly, b::fmpz)
   return Bool(ccall((:fmpz_mpoly_equal_fmpz, libflint), Cint,
                     (Ref{fmpz_mpoly}, Ref{fmpz}, Ref{FmpzMPolyRing}),
                     a, b, a.parent))
end

==(a::fmpz, b::fmpz_mpoly) = b == a

function ==(a::fmpz_mpoly, b::Int)
   return Bool(ccall((:fmpz_mpoly_equal_si, libflint), Cint,
               (Ref{fmpz_mpoly}, Int, Ref{FmpzMPolyRing}),
               a, b, a.parent))
end

==(a::Int, b::fmpz_mpoly) = b == a

==(a::fmpz_mpoly, b::Integer) = a == fmpz(b)

==(a::Integer, b::fmpz_mpoly) = b == a

###############################################################################
#
#   Divisibility
#
###############################################################################

function divides(a::fmpz_mpoly, b::fmpz_mpoly)
   check_parent(a, b)
   if iszero(a)
      return true, zero(parent(a))
   end
   if iszero(b)
      return false, zero(parent(a))
   end
   z = parent(a)()
   d = ccall((:fmpz_mpoly_divides, libflint), Cint,
       (Ref{fmpz_mpoly}, Ref{fmpz_mpoly}, Ref{fmpz_mpoly}, Ref{FmpzMPolyRing}),
       z, a, b, a.parent)
   return isone(d), z
end

###############################################################################
#
#   Division with remainder
#
###############################################################################

function Base.div(a::fmpz_mpoly, b::fmpz_mpoly)
   check_parent(a, b)
   q = parent(a)()
   ccall((:fmpz_mpoly_div, libflint), Nothing,
       (Ref{fmpz_mpoly}, Ref{fmpz_mpoly},
        Ref{fmpz_mpoly}, Ref{FmpzMPolyRing}),
       q, a, b, a.parent)
   return q
end

function Base.divrem(a::fmpz_mpoly, b::fmpz_mpoly)
   check_parent(a, b)
   q = parent(a)()
   r = parent(a)()
   ccall((:fmpz_mpoly_divrem, libflint), Nothing,
       (Ref{fmpz_mpoly}, Ref{fmpz_mpoly}, Ref{fmpz_mpoly},
        Ref{fmpz_mpoly}, Ref{FmpzMPolyRing}),
       q, r, a, b, a.parent)
   return q, r
end

function Base.divrem(a::fmpz_mpoly, b::Vector{fmpz_mpoly})
   len = length(b)
   q = [parent(a)() for i in 1:len]
   r = parent(a)()
   ccall((:fmpz_mpoly_divrem_ideal, libflint), Nothing,
         (Ptr{Ref{fmpz_mpoly}}, Ref{fmpz_mpoly}, Ref{fmpz_mpoly},
          Ptr{Ref{fmpz_mpoly}}, Int, Ref{FmpzMPolyRing}),
       q, r, a, b, len, a.parent)
   return q, r
end

###############################################################################
#
#   Exact division
#
###############################################################################

function divexact(a::fmpz_mpoly, b::fmpz_mpoly)
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

function derivative(a::fmpz_mpoly, i::Int)
   n = nvars(parent(a))
   (i <= 0 || i > n) && error("Index must be between 1 and $n")
   z = parent(a)()
   ccall((:fmpz_mpoly_derivative, libflint), Nothing,
         (Ref{fmpz_mpoly}, Ref{fmpz_mpoly}, Int, Ref{FmpzMPolyRing}),
         z, a, i - 1, parent(a))
   return z
end

###############################################################################
#
#   Evaluation
#
###############################################################################

function evaluate(a::fmpz_mpoly, b::Vector{fmpz})
   length(b) != nvars(parent(a)) && error("Vector size incorrect in evaluate")
   z = fmpz()
   GC.@preserve b ccall((:fmpz_mpoly_evaluate_all_fmpz, libflint), Nothing,
         (Ref{fmpz}, Ref{fmpz_mpoly}, Ptr{fmpz}, Ref{FmpzMPolyRing}),
            z, a, b, parent(a))
   return z
end

function evaluate(a::fmpz_mpoly, b::Vector{<:Integer})
   fmpz_vec = [fmpz(s) for s in b]
   return evaluate(a, fmpz_vec)
end

function (a::fmpz_mpoly)(vals::fmpz...)
   length(vals) != nvars(parent(a)) && error("Number of variables does not match number of values")
   return evaluate(a, [vals...])
end

function (a::fmpz_mpoly)(vals::Integer...)
   length(vals) != nvars(parent(a)) && error("Number of variables does not match number of values")
   return evaluate(a, [vals...])
end

function (a::fmpz_mpoly)(vals::Union{NCRingElem, RingElement}...)
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

###############################################################################
#
#   Unsafe functions
#
###############################################################################

function zero!(a::fmpz_mpoly)
    ccall((:fmpz_mpoly_zero, libflint), Nothing,
         (Ref{fmpz_mpoly}, Ref{FmpzMPolyRing}), a, a.parent)
    return a
end

function add!(a::fmpz_mpoly, b::fmpz_mpoly, c::fmpz_mpoly)
   ccall((:fmpz_mpoly_add, libflint), Nothing,
         (Ref{fmpz_mpoly}, Ref{fmpz_mpoly},
          Ref{fmpz_mpoly}, Ref{FmpzMPolyRing}), a, b, c, a.parent)
   return a
end

function addeq!(a::fmpz_mpoly, b::fmpz_mpoly)
   ccall((:fmpz_mpoly_add, libflint), Nothing,
         (Ref{fmpz_mpoly}, Ref{fmpz_mpoly},
          Ref{fmpz_mpoly}, Ref{FmpzMPolyRing}), a, a, b, a.parent)
   return a
end

function mul!(a::fmpz_mpoly, b::fmpz_mpoly, c::fmpz_mpoly)
   ccall((:fmpz_mpoly_mul, libflint), Nothing,
         (Ref{fmpz_mpoly}, Ref{fmpz_mpoly},
          Ref{fmpz_mpoly}, Ref{FmpzMPolyRing}), a, b, c, a.parent)
   return a
end

# Set the n-th coefficient of a to c. If zero coefficients are inserted, they
# must be removed with combine_like_terms!
function setcoeff!(a::fmpz_mpoly, n::Int, c::fmpz)
   if n > length(a)
      ccall((:fmpz_mpoly_resize, libflint), Nothing,
            (Ref{fmpz_mpoly}, Int, Ref{FmpzMPolyRing}), a, n, a.parent)
   end
   ccall((:fmpz_mpoly_set_term_coeff_fmpz, libflint), Nothing,
         (Ref{fmpz_mpoly}, Int, Ref{fmpz}, Ref{FmpzMPolyRing}),
         a, n - 1, c, a.parent)
   return a
end

# Set the i-th coefficient of a to c. If zero coefficients are inserted, they
# must be removed with combine_like_terms!
setcoeff!(a::fmpz_mpoly, i::Int, c::Integer) = setcoeff!(a, i, fmpz(c))

# Remove zero terms and combine adjacent terms if they have the same monomial
# no sorting is performed
function combine_like_terms!(a::fmpz_mpoly)
   ccall((:fmpz_mpoly_combine_like_terms, libflint), Nothing,
         (Ref{fmpz_mpoly}, Ref{FmpzMPolyRing}), a, a.parent)
   return a
end

###############################################################################
#
#   Manipulating terms and monomials
#
###############################################################################

# Return true if the exponents of the i-th exp. vector fit into UInts
function exponent_vector_fits_ui(a::fmpz_mpoly, i::Int)
   b = ccall((:fmpz_mpoly_term_exp_fits_ui, libflint), Cint,
             (Ref{fmpz_mpoly}, Int, Ref{FmpzMPolyRing}), a, i - 1, a.parent)
      return Bool(b)
end

# Return true if the exponents of the i-th exp. vector fit into UInts
function exponent_vector_fits_int(a::fmpz_mpoly, i::Int)
   b = ccall((:fmpz_mpoly_term_exp_fits_si, libflint), Cint,
             (Ref{fmpz_mpoly}, Int, Ref{FmpzMPolyRing}), a, i - 1, a.parent)
   return Bool(b)
end

# Return Julia array of UInt's corresponding to exponent vector of i-th term
function exponent_vector_ui(a::fmpz_mpoly, i::Int)
   z = Vector{UInt}(undef, nvars(parent(a)))
   ccall((:fmpz_mpoly_get_term_exp_ui, libflint), Nothing,
         (Ptr{UInt}, Ref{fmpz_mpoly}, Int, Ref{FmpzMPolyRing}),
      z, a, i - 1, parent(a))
   return z
end

# Return Julia array of Int's corresponding to exponent vector of i-th term
function exponent_vector(a::fmpz_mpoly, i::Int)
   exponent_vector_fits_int(a, i) ||
      throw(DomainError(term(a, i), "exponents don't fit in `Int` (try exponent_vector_fmpz)"))
   z = Vector{Int}(undef, nvars(parent(a)))
   ccall((:fmpz_mpoly_get_term_exp_si, libflint), Nothing,
         (Ptr{Int}, Ref{fmpz_mpoly}, Int, Ref{FmpzMPolyRing}),
      z, a, i - 1, parent(a))
   return z
end

# Return Julia array of fmpz's corresponding to exponent vector of i-th term
function exponent_vector_fmpz(a::fmpz_mpoly, i::Int)
   n = nvars(parent(a))
   z = Vector{fmpz}(undef, n)
   for j in 1:n
      z[j] = fmpz()
   end
   ccall((:fmpz_mpoly_get_term_exp_fmpz, libflint), Nothing,
         (Ptr{Ref{fmpz}}, Ref{fmpz_mpoly}, Int, Ref{FmpzMPolyRing}),
         z, a, i - 1, parent(a))
   return z
end

# Return a generator for exponent vectors of $a$
function exponent_vectors_fmpz(a::fmpz_mpoly)
   return (exponent_vector_fmpz(a, i) for i in 1:length(a))
end

# Set exponent of n-th term to given vector of UInt's
# No sort is performed, so this is unsafe. These are promoted to fmpz's if
# they don't fit into 31/63 bits
function set_exponent_vector!(a::fmpz_mpoly, n::Int, exps::Vector{UInt})
   if n > length(a)
      ccall((:fmpz_mpoly_resize, libflint), Nothing,
            (Ref{fmpz_mpoly}, Int, Ref{FmpzMPolyRing}), a, n, a.parent)
   end
   ccall((:fmpz_mpoly_set_term_exp_ui, libflint), Nothing,
         (Ref{fmpz_mpoly}, Int, Ptr{UInt}, Ref{FmpzMPolyRing}),
      a, n - 1, exps, parent(a))
   return a
end

# Set exponent of n-th term to given vector of Int's
# No sort is performed, so this is unsafe. The Int's must be positive, but
# no check is performed
function set_exponent_vector!(a::fmpz_mpoly, n::Int, exps::Vector{Int})
   if n > length(a)
      ccall((:fmpz_mpoly_resize, libflint), Nothing,
            (Ref{fmpz_mpoly}, Int, Ref{FmpzMPolyRing}), a, n, a.parent)
   end
   ccall((:fmpz_mpoly_set_term_exp_ui, libflint), Nothing,
         (Ref{fmpz_mpoly}, Int, Ptr{Int}, Ref{FmpzMPolyRing}),
      a, n - 1, exps, parent(a))
   return a
end

# Set exponent of n-th term to given vector of fmpz's
# No sort is performed, so this is unsafe
function set_exponent_vector!(a::fmpz_mpoly, n::Int, exps::Vector{fmpz})
   if n > length(a)
      ccall((:fmpz_mpoly_resize, libflint), Nothing,
            (Ref{fmpz_mpoly}, Int, Ref{FmpzMPolyRing}), a, n, a.parent)
      return a
   end
   @GC.preserve exps ccall((:fmpz_mpoly_set_term_exp_fmpz, libflint), Nothing,
         (Ref{fmpz_mpoly}, Int, Ptr{fmpz}, Ref{FmpzMPolyRing}),
      a, n - 1, exps, parent(a))
   return a
end

# Return j-th coordinate of i-th exponent vector
function exponent(a::fmpz_mpoly, i::Int, j::Int)
   (j < 1 || j > nvars(parent(a))) && error("Invalid variable index")
   return ccall((:fmpz_mpoly_get_term_var_exp_ui, libflint), Int,
                (Ref{fmpz_mpoly}, Int, Int, Ref{FmpzMPolyRing}),
                 a, i - 1, j - 1, a.parent)
end

# Return the coefficient of the term with the given exponent vector
# Return zero if there is no such term
function coeff(a::fmpz_mpoly, exps::Vector{UInt})
   z = fmpz()
   ccall((:fmpz_mpoly_get_coeff_fmpz_ui, libflint), Nothing,
         (Ref{fmpz}, Ref{fmpz_mpoly}, Ptr{UInt}, Ref{FmpzMPolyRing}),
      z, a, exps, parent(a))
   return z
end

# Return the coefficient of the term with the given exponent vector
# Return zero if there is no such term
function coeff(a::fmpz_mpoly, exps::Vector{Int})
   z = fmpz()
   ccall((:fmpz_mpoly_get_coeff_fmpz_ui, libflint), Nothing,
         (Ref{fmpz}, Ref{fmpz_mpoly}, Ptr{Int}, Ref{FmpzMPolyRing}),
      z, a, exps, parent(a))
   return z
end

# Set the coefficient of the term with the given exponent vector to the
# given fmpz. Removal of a zero term is performed.
function setcoeff!(a::fmpz_mpoly, exps::Vector{UInt}, b::fmpz)
   ccall((:fmpz_mpoly_set_coeff_fmpz_ui, libflint), Nothing,
         (Ref{fmpz_mpoly}, Ref{fmpz}, Ptr{UInt}, Ref{FmpzMPolyRing}),
      a, b, exps, parent(a))
   return a
end

# Set the coefficient of the term with the given exponent vector to the
# given fmpz. Removal of a zero term is performed.
function setcoeff!(a::fmpz_mpoly, exps::Vector{Int}, b::fmpz)
   ccall((:fmpz_mpoly_set_coeff_fmpz_ui, libflint), Nothing,
         (Ref{fmpz_mpoly}, Ref{fmpz}, Ptr{Int}, Ref{FmpzMPolyRing}),
      a, b, exps, parent(a))
   return a
end

# Set the coefficient of the term with the given exponent vector to the
setcoeff!(a::fmpz_mpoly, exps::Vector{Int}, b::Integer) =
   setcoeff!(a, exps, fmpz(b))

# Sort the terms according to the ordering. This is only needed if unsafe
# functions such as those above have been called and terms have been inserted
# out of order. Note that like terms are not combined and zeros are not
# removed. For that, call combine_like_terms!
function sort_terms!(a::fmpz_mpoly)
   ccall((:fmpz_mpoly_sort_terms, libflint), Nothing,
         (Ref{fmpz_mpoly}, Ref{FmpzMPolyRing}), a, a.parent)
   return a
end

# Return the i-th term of the polynomial, as a polynomial
function term(a::fmpz_mpoly, i::Int)
   z = parent(a)()
   ccall((:fmpz_mpoly_get_term, libflint), Nothing,
         (Ref{fmpz_mpoly}, Ref{fmpz_mpoly}, Int, Ref{FmpzMPolyRing}),
          z, a, i - 1, a.parent)
   return z
end

# Return the i-th monomial of the polynomial, as a polynomial
function monomial(a::fmpz_mpoly, i::Int)
   z = parent(a)()
   ccall((:fmpz_mpoly_get_term_monomial, libflint), Nothing,
         (Ref{fmpz_mpoly}, Ref{fmpz_mpoly}, Int, Ref{FmpzMPolyRing}),
          z, a, i - 1, a.parent)
   return z
end

# Sets the given polynomial m to the i-th monomial of the polynomial
function monomial!(m::fmpz_mpoly, a::fmpz_mpoly, i::Int)
   ccall((:fmpz_mpoly_get_term_monomial, libflint), Nothing,
         (Ref{fmpz_mpoly}, Ref{fmpz_mpoly}, Int, Ref{FmpzMPolyRing}),
          m, a, i - 1, a.parent)
   return m
end

###############################################################################
#
#   Promotion rules
#
###############################################################################

promote_rule(::Type{fmpz_mpoly}, ::Type{V}) where {V <: Integer} = fmpz_mpoly

promote_rule(::Type{fmpz_mpoly}, ::Type{fmpz}) = fmpz_mpoly

###############################################################################
#
#   Parent object call overload
#
###############################################################################

function (R::FmpzMPolyRing)()
   z = fmpz_mpoly(R)
   return z
end

function (R::FmpzMPolyRing)(b::fmpz)
   z = fmpz_mpoly(R, b)
   return z
end

function (R::FmpzMPolyRing)(b::Int)
   z = fmpz_mpoly(R, b)
   return z
end

function (R::FmpzMPolyRing)(b::UInt)
   z = fmpz_mpoly(R, b)
   return z
end

function (R::FmpzMPolyRing)(b::Integer)
   return R(fmpz(b))
end


function (R::FmpzMPolyRing)(a::fmpz_mpoly)
   parent(a) != R && error("Unable to coerce polynomial")
   return a
end

# Create poly with given array of coefficients and array of exponent vectors (sorting is performed)
function (R::FmpzMPolyRing)(a::Vector{fmpz}, b::Vector{Vector{T}}) where {T <: Union{fmpz, UInt}}
   length(a) != length(b) && error("Coefficient and exponent vector must have the same length")

   for i in 1:length(b)
     length(b[i]) != nvars(R) && error("Exponent vector $i has length $(length(b[i])) (expected $(nvars(R))")
   end

   z = fmpz_mpoly(R, a, b)
   return z
end

# Create poly with given array of coefficients and array of exponent vectors (sorting is performed)
function (R::FmpzMPolyRing)(a::Vector{fmpz}, b::Vector{Vector{Int}})
   length(a) != length(b) && error("Coefficient and exponent vector must have the same length")

   for i in 1:length(b)
      length(b[i]) != nvars(R) && error("Exponent vector $i has length $(length(b[i])) (expected $(nvars(R)))")
   end

   z = fmpz_mpoly(R, a, b)
   return z
end

# Create poly with given array of coefficients and array of exponent vectors (sorting is performed)
function (R::FmpzMPolyRing)(a::Vector{Any}, b::Vector{Vector{T}}) where T
   n = nvars(R)
   length(a) != length(b) && error("Coefficient and exponent vector must have the same length")
   newa = map(FlintZZ, a)
   newb = map(x -> map(FlintZZ, x), b)
   newaa = convert(Vector{fmpz}, newa)
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

function PolynomialRing(R::FlintIntegerRing, s::Vector{Symbol}; cached::Bool = true, ordering::Symbol = :lex)
   parent_obj = FmpzMPolyRing(s, ordering, cached)
   return tuple(parent_obj, gens(parent_obj))
end

function PolynomialRing(R::FlintIntegerRing, s::Vector{String}; cached::Bool = true, ordering::Symbol = :lex)
   return PolynomialRing(R, [Symbol(x) for x in s]; cached=cached, ordering=ordering)
end

