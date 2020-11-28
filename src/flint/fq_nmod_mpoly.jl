###############################################################################
#
#   fq_nmod_mpoly.jl : Flint multivariate polynomials over fq_nmod
#
###############################################################################

export FqNmodMPolyRing, fq_nmod_mpoly

###############################################################################
#
#   Data type and parent object methods
#
###############################################################################

parent_type(::Type{fq_nmod_mpoly}) = FqNmodMPolyRing

elem_type(::Type{FqNmodMPolyRing}) = fq_nmod_mpoly

elem_type(::FqNmodMPolyRing) = fq_nmod_mpoly

symbols(a::FqNmodMPolyRing) = a.S

parent(a::fq_nmod_mpoly) = a.parent

function check_parent(a::fq_nmod_mpoly, b::fq_nmod_mpoly)
   parent(a) != parent(b) &&
      error("Incompatible polynomial rings in polynomial operation")
end

nvars(a::FqNmodMPolyRing) = a.nvars

base_ring(a::FqNmodMPolyRing) = a.base_ring

base_ring(f::fq_nmod_mpoly) = f.parent.base_ring

function ordering(a::FqNmodMPolyRing)
   b = a.ord
#   b = ccall((:fq_nmod_mpoly_ctx_ord, libflint), Cint, (Ref{FqNmodMPolyRing}, ), a)
   return flint_orderings[b + 1]
end

function gens(R::FqNmodMPolyRing)
   A = Vector{fq_nmod_mpoly}(undef, R.nvars)
   for i = 1:R.nvars
      z = R()
      ccall((:fq_nmod_mpoly_gen, libflint), Nothing,
            (Ref{fq_nmod_mpoly}, Int, Ref{FqNmodMPolyRing}),
            z, i - 1, R)
      A[i] = z
   end
   return A
end

function gen(R::FqNmodMPolyRing, i::Int)
   n = nvars(R)
   !(1 <= i <= n) && error("Index must be between 1 and $n")
   z = R()
   ccall((:fq_nmod_mpoly_gen, libflint), Nothing,
         (Ref{fq_nmod_mpoly}, Int, Ref{FqNmodMPolyRing}),
         z, i - 1, R)
   return z
end

function isgen(a::fq_nmod_mpoly, i::Int)
   n = nvars(parent(a))
   !(1 <= i <= n) && error("Index must be between 1 and $n")
   return Bool(ccall((:fq_nmod_mpoly_is_gen, libflint), Cint,
                     (Ref{fq_nmod_mpoly}, Int, Ref{FqNmodMPolyRing}),
                     a, i - 1, a.parent))
end

function isgen(a::fq_nmod_mpoly)
   return Bool(ccall((:fq_nmod_mpoly_is_gen, libflint), Cint,
                     (Ref{fq_nmod_mpoly}, Int, Ref{FqNmodMPolyRing}),
                     a, -1, a.parent))
end

function deepcopy_internal(a::fq_nmod_mpoly, dict::IdDict)
   z = parent(a)()
   ccall((:fq_nmod_mpoly_set, libflint), Nothing,
         (Ref{fq_nmod_mpoly}, Ref{fq_nmod_mpoly}, Ref{FqNmodMPolyRing}),
         z, a, a.parent)
   return z
end

function length(a::fq_nmod_mpoly)
   n = ccall((:fq_nmod_mpoly_length, libflint), Int,
             (Ref{fq_nmod_mpoly}, Ref{FqNmodMPolyRing}),
             a, a.parent)
   return n
end

function one(R::FqNmodMPolyRing)
   z = R()
   ccall((:fq_nmod_mpoly_one, libflint), Nothing,
         (Ref{fq_nmod_mpoly}, Ref{FqNmodMPolyRing}),
         z, R)
   return z
end

function zero(R::FqNmodMPolyRing)
   z = R()
   ccall((:fq_nmod_mpoly_zero, libflint), Nothing,
         (Ref{fq_nmod_mpoly}, Ref{FqNmodMPolyRing}),
         z, R)
   return z
end

function isone(a::fq_nmod_mpoly)
   b = ccall((:fq_nmod_mpoly_is_one, libflint), Cint,
             (Ref{fq_nmod_mpoly}, Ref{FqNmodMPolyRing}),
             a, a.parent)
   return Bool(b)
end

function iszero(a::fq_nmod_mpoly)
   b = ccall((:fq_nmod_mpoly_is_zero, libflint), Cint,
             (Ref{fq_nmod_mpoly}, Ref{FqNmodMPolyRing}),
             a, a.parent)
   return Bool(b)
end

function ismonomial(a::fq_nmod_mpoly)
   return length(a) == 1 && isone(coeff(a, 1))
end

function isterm(a::fq_nmod_mpoly)
   return length(a) == 1
end

function isunit(a::fq_nmod_mpoly)
   return isconstant(a)
end

function isconstant(a::fq_nmod_mpoly)
   b = ccall((:fq_nmod_mpoly_is_fq_nmod, libflint), Cint,
             (Ref{fq_nmod_mpoly}, Ref{FqNmodMPolyRing}),
             a, parent(a))
   return Bool(b)
end

characteristic(R::FqNmodMPolyRing) = characteristic(base_ring(R))

################################################################################
#
#  Getting coefficients
#
################################################################################

function coeff(a::fq_nmod_mpoly, i::Int)
   n = length(a)
   !(1 <= i <= n) && error("Index must be between 1 and $(length(a))")
   z = base_ring(parent(a))()
   ccall((:fq_nmod_mpoly_get_term_coeff_fq_nmod, libflint), Nothing,
         (Ref{fq_nmod}, Ref{fq_nmod_mpoly}, Int, Ref{FqNmodMPolyRing}),
         z, a, i - 1, a.parent)
   return z
end

function coeff(a::fq_nmod_mpoly, b::fq_nmod_mpoly)
   check_parent(a, b)
   !isone(length(b)) && error("Second argument must be a monomial")
   z = base_ring(parent(a))()
   ccall((:fq_nmod_mpoly_get_coeff_fq_nmod_monomial, libflint), UInt,
         (Ref{fq_nmod}, Ref{fq_nmod_mpoly},
          Ref{fq_nmod_mpoly}, Ref{FqNmodMPolyRing}),
         z, a, b, parent(a))
   return z
end

###############################################################################
#
#   Basic manipulation
#
###############################################################################

# Degree in the i-th variable as an Int
function degree(a::fq_nmod_mpoly, i::Int)
   n = nvars(parent(a))
   !(1 <= i <= n) && error("Index must be between 1 and $n")
   d = ccall((:fq_nmod_mpoly_degree_si, libflint), Int,
             (Ref{fq_nmod_mpoly}, Int, Ref{FqNmodMPolyRing}),
             a, i - 1, a.parent)
   return d
end

# Degree in the i-th variable as an fmpz
function degree_fmpz(a::fq_nmod_mpoly, i::Int)
   n = nvars(parent(a))
   !(1 <= i <= n) && error("Index must be between 1 and $n")
   d = fmpz()
   ccall((:fq_nmod_mpoly_degree_fmpz, libflint), Nothing,
         (Ref{fmpz}, Ref{fq_nmod_mpoly}, Int, Ref{FqNmodMPolyRing}),
         d, a, i - 1, a.parent)
   return d
end

# Return true if degrees fit into an Int
function degrees_fit_int(a::fq_nmod_mpoly)
   b = ccall((:fq_nmod_mpoly_degrees_fit_si, libflint), Cint,
             (Ref{fq_nmod_mpoly}, Ref{FqNmodMPolyRing}),
             a, a.parent)
   return Bool(b)
end

# Return an array of the max degrees in each variable
function degrees(a::fq_nmod_mpoly)
   degs = Vector{Int}(undef, nvars(parent(a)))
   ccall((:fq_nmod_mpoly_degrees_si, libflint), Nothing,
         (Ptr{Int}, Ref{fq_nmod_mpoly}, Ref{FqNmodMPolyRing}),
         degs, a, a.parent)
   return degs
end

# Return an array of the max degrees as fmpzs in each variable
function degrees_fmpz(a::fq_nmod_mpoly)
   n = nvars(parent(a))
   degs = Vector{fmpz}(undef, n)
   for i in 1:n
      degs[i] = fmpz()
   end
   ccall((:fq_nmod_mpoly_degrees_fmpz, libflint), Nothing,
         (Ptr{Ref{fmpz}}, Ref{fq_nmod_mpoly}, Ref{FqNmodMPolyRing}),
         degs, a, a.parent)
   return degs
end

# Return true if degree fits into an Int
function total_degree_fits_int(a::fq_nmod_mpoly)
   b = ccall((:fq_nmod_mpoly_total_degree_fits_si, libflint), Cint,
             (Ref{fq_nmod_mpoly}, Ref{FqNmodMPolyRing}),
             a, a.parent)
   return Bool(b)
end

# Total degree as an Int
function total_degree(a::fq_nmod_mpoly)
   d = ccall((:fq_nmod_mpoly_total_degree_si, libflint), Int,
             (Ref{fq_nmod_mpoly}, Ref{FqNmodMPolyRing}),
             a, a.parent)
   return d
end

# Total degree as an fmpz
function total_degree_fmpz(a::fq_nmod_mpoly)
   d = fmpz()
   ccall((:fq_nmod_mpoly_total_degree_fmpz, libflint), Nothing,
         (Ref{fmpz}, Ref{fq_nmod_mpoly}, Ref{FqNmodMPolyRing}),
         d, a, a.parent)
   return d
end

###############################################################################
#
#   Multivariable coefficient polynomials
#
###############################################################################

function coeff(a::fq_nmod_mpoly, vars::Vector{Int}, exps::Vector{Int})
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
         (Ref{fq_nmod_mpoly}, Ref{fq_nmod_mpoly}, Ptr{Int},
          Ptr{Int}, Int, Ref{FqNmodMPolyRing}),
         z, a, vars, exps, length(vars), a.parent)
   return z
end

###############################################################################
#
#   String I/O
#
###############################################################################

function show(io::IO, p::FqNmodMPolyRing)
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

function -(a::fq_nmod_mpoly)
   z = parent(a)()
   ccall((:fq_nmod_mpoly_neg, libflint), Nothing,
         (Ref{fq_nmod_mpoly}, Ref{fq_nmod_mpoly}, Ref{FqNmodMPolyRing}),
         z, a, a.parent)
   return z
end

function +(a::fq_nmod_mpoly, b::fq_nmod_mpoly)
   check_parent(a, b)
   z = parent(a)()
   ccall((:fq_nmod_mpoly_add, libflint), Nothing,
         (Ref{fq_nmod_mpoly}, Ref{fq_nmod_mpoly},
          Ref{fq_nmod_mpoly}, Ref{FqNmodMPolyRing}),
         z, a, b, a.parent)
   return z
end

function -(a::fq_nmod_mpoly, b::fq_nmod_mpoly)
   check_parent(a, b)
   z = parent(a)()
   ccall((:fq_nmod_mpoly_sub, libflint), Nothing,
         (Ref{fq_nmod_mpoly}, Ref{fq_nmod_mpoly},
          Ref{fq_nmod_mpoly}, Ref{FqNmodMPolyRing}),
         z, a, b, a.parent)
   return z
end

function *(a::fq_nmod_mpoly, b::fq_nmod_mpoly)
   check_parent(a, b)
   z = parent(a)()
   ccall((:fq_nmod_mpoly_mul, libflint), Nothing,
         (Ref{fq_nmod_mpoly}, Ref{fq_nmod_mpoly},
          Ref{fq_nmod_mpoly}, Ref{FqNmodMPolyRing}),
         z, a, b, a.parent)
   return z
end

###############################################################################
#
#   Ad hoc arithmetic
#
###############################################################################

+(a::fq_nmod_mpoly, b::Integer) = a + base_ring(parent(a))(b)

+(a::Integer, b::fq_nmod_mpoly) = b + a

-(a::fq_nmod_mpoly, b::Integer) = a - base_ring(parent(a))(b)

-(a::Integer, b::fq_nmod_mpoly) = base_ring(parent(b))(a) - b

*(a::fq_nmod_mpoly, b::Integer) = a*base_ring(parent(a))(b)

*(a::Integer, b::fq_nmod_mpoly) = b*a

divexact(a::fq_nmod_mpoly, b::Integer) = divexact(a, base_ring(parent(a))(b))

###############################################################################
#
#   Powering
#
###############################################################################

function ^(a::fq_nmod_mpoly, b::Int)
   b < 0 && throw(DomainError(b, "Exponent must be non-negative"))
   z = parent(a)()
   r = ccall((:fq_nmod_mpoly_pow_ui, libflint), Cint,
             (Ref{fq_nmod_mpoly}, Ref{fq_nmod_mpoly}, UInt, Ref{FqNmodMPolyRing}),
             z, a, UInt(b), parent(a))
   iszero(r) && error("Unable to compute power")
   return z
end

function ^(a::fq_nmod_mpoly, b::fmpz)
   b < 0 && throw(DomainError(b, "Exponent must be non-negative"))
   z = parent(a)()
   r = ccall((:fq_nmod_mpoly_pow_fmpz, libflint), Cint,
             (Ref{fq_nmod_mpoly}, Ref{fq_nmod_mpoly},
              Ref{fmpz}, Ref{FqNmodMPolyRing}),
             z, a, b, parent(a))
   iszero(r) && error("Unable to compute power")
   return z
end

################################################################################
#
#   GCD
#
################################################################################

function gcd(a::fq_nmod_mpoly, b::fq_nmod_mpoly)
   check_parent(a, b)
   z = parent(a)()
   r = ccall((:fq_nmod_mpoly_gcd, libflint), Cint,
             (Ref{fq_nmod_mpoly}, Ref{fq_nmod_mpoly},
              Ref{fq_nmod_mpoly}, Ref{FqNmodMPolyRing}),
             z, a, b, a.parent)
   iszero(r) && error("Unable to compute gcd")
   return z
end

################################################################################
#
#   Factorization and Square Root
#
################################################################################

function Base.convert(::Type{Fac{fq_nmod_mpoly}}, fac::fq_nmod_mpoly_factor)
   R = fac.parent
   dic = Dict{fq_nmod_mpoly, Int}()
   for i in 0:fac.num-1
      f = R()
      # use get_base instead of swap_base if convert is to preserve input
      ccall((:fq_nmod_mpoly_factor_swap_base, libflint), Nothing,
            (Ref{fq_nmod_mpoly}, Ref{fq_nmod_mpoly_factor}, Int, Ref{FqNmodMPolyRing}),
            f, fac, i, R)
      dic[f] = ccall((:fq_nmod_mpoly_factor_get_exp_si, libflint), Int,
                     (Ref{fq_nmod_mpoly_factor}, Int, Ref{FqNmodMPolyRing}),
                     fac, i, R)
   end
   c = base_ring(R)()
   ccall((:fq_nmod_mpoly_factor_get_constant_fq_nmod, libflint), Nothing,
         (Ref{fq_nmod}, Ref{fq_nmod_mpoly_factor}),
         c, fac)
   return Fac(R(c), dic)
end

function factor(a::fq_nmod_mpoly)
   R = parent(a)
   fac = fq_nmod_mpoly_factor(R)
   ok = ccall((:fq_nmod_mpoly_factor, libflint), Cint,
              (Ref{fq_nmod_mpoly_factor}, Ref{fq_nmod_mpoly}, Ref{FqNmodMPolyRing}),
              fac, a, R)
   ok == 0 && error("unable to compute factorization")
   return convert(Fac{fq_nmod_mpoly}, fac)
end

function factor_squarefree(a::fq_nmod_mpoly)
   R = parent(a)
   fac = fq_nmod_mpoly_factor(R)
   ok = ccall((:fq_nmod_mpoly_factor_squarefree, libflint), Cint,
              (Ref{fq_nmod_mpoly_factor}, Ref{fq_nmod_mpoly}, Ref{FqNmodMPolyRing}),
              fac, a, R)
   ok == 0 && error("unable to compute factorization")
   return convert(Fac{fq_nmod_mpoly}, fac)
end


function square_root(a::fq_nmod_mpoly)
   (flag, q) = issquare_with_square_root(a)
   !flag && error("Not a square in square_root")
   return q
end

function issquare(a::fq_nmod_mpoly)
   return Bool(ccall((:fq_nmod_mpoly_is_square, libflint), Cint,
                     (Ref{fq_nmod_mpoly}, Ref{FqNmodMPolyRing}),
                     a, a.parent))
end

function issquare_with_square_root(a::fq_nmod_mpoly)
   q = parent(a)()
   flag = ccall((:fq_nmod_mpoly_sqrt, libflint), Cint,
                (Ref{fq_nmod_mpoly}, Ref{fq_nmod_mpoly}, Ref{FqNmodMPolyRing}),
                q, a, a.parent)
   return (Bool(flag), q)
end

###############################################################################
#
#   Comparison
#
###############################################################################

function ==(a::fq_nmod_mpoly, b::fq_nmod_mpoly)
   check_parent(a, b)
   return ccall((:fq_nmod_mpoly_equal, libflint), Cint,
                (Ref{fq_nmod_mpoly}, Ref{fq_nmod_mpoly}, Ref{FqNmodMPolyRing}),
                a, b, a.parent) != 0
end

function Base.isless(a::fq_nmod_mpoly, b::fq_nmod_mpoly)
   (!ismonomial(a) || !ismonomial(b)) && error("Not monomials in comparison")
   return ccall((:fq_nmod_mpoly_cmp, libflint), Cint,
                (Ref{fq_nmod_mpoly}, Ref{fq_nmod_mpoly}, Ref{FqNmodMPolyRing}),
                a, b, a.parent) < 0
end

###############################################################################
#
#   Ad hoc comparison
#
###############################################################################

function ==(a::fq_nmod_mpoly, b::fq_nmod)
   return Bool(ccall((:fq_nmod_mpoly_equal_fq_nmod, libflint), Cint,
                     (Ref{fq_nmod_mpoly}, Ref{fq_nmod}, Ref{FqNmodMPolyRing}),
                     a, b, a.parent))
end

==(a::fq_nmod, b::fq_nmod_mpoly) = b == a

==(a::fq_nmod_mpoly, b::Integer) = a == base_ring(parent(a))(b)

==(a::fq_nmod_mpoly, b::fmpz) = a == base_ring(parent(a))(b)

==(a::Integer, b::fq_nmod_mpoly) = b == a

==(a::fmpz, b::fq_nmod_mpoly) = b == a

###############################################################################
#
#   Divisibility
#
###############################################################################

function divides(a::fq_nmod_mpoly, b::fq_nmod_mpoly)
   check_parent(a, b)
   if iszero(a)
      return true, zero(parent(a))
   end
   if iszero(b)
      return false, zero(parent(a))
   end
   z = parent(a)()
   d = ccall((:fq_nmod_mpoly_divides, libflint), Cint,
             (Ref{fq_nmod_mpoly}, Ref{fq_nmod_mpoly},
              Ref{fq_nmod_mpoly}, Ref{FqNmodMPolyRing}),
             z, a, b, a.parent)
   return isone(d), z
end

###############################################################################
#
#   Division with remainder
#
###############################################################################

function Base.div(a::fq_nmod_mpoly, b::fq_nmod_mpoly)
   check_parent(a, b)
   q = parent(a)()
   ccall((:fq_nmod_mpoly_div, libflint), Nothing,
         (Ref{fq_nmod_mpoly}, Ref{fq_nmod_mpoly},
          Ref{fq_nmod_mpoly}, Ref{FqNmodMPolyRing}),
         q, a, b, a.parent)
   return q
end

function Base.divrem(a::fq_nmod_mpoly, b::fq_nmod_mpoly)
   check_parent(a, b)
   q = parent(a)()
   r = parent(a)()
   ccall((:fq_nmod_mpoly_divrem, libflint), Nothing,
         (Ref{fq_nmod_mpoly}, Ref{fq_nmod_mpoly}, Ref{fq_nmod_mpoly},
          Ref{fq_nmod_mpoly}, Ref{FqNmodMPolyRing}),
         q, r, a, b, a.parent)
   return q, r
end

function Base.divrem(a::fq_nmod_mpoly, b::Array{fq_nmod_mpoly, 1})
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
         (Ptr{Ref{fq_nmod_mpoly}}, Ref{fq_nmod_mpoly}, Ref{fq_nmod_mpoly},
          Ptr{Ref{fq_nmod_mpoly}}, Int, Ref{FqNmodMPolyRing}),
         q, r, a, b, len, a.parent)
   return q, r
end

###############################################################################
#
#   Exact division
#
###############################################################################

function divexact(a::fq_nmod_mpoly, b::fq_nmod_mpoly)
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

function derivative(a::fq_nmod_mpoly, i::Int)
   n = nvars(parent(a))
   !(1 <= i <= n) && error("Index must be between 1 and $n")
   z = parent(a)()
   ccall((:fq_nmod_mpoly_derivative, libflint), Nothing,
         (Ref{fq_nmod_mpoly}, Ref{fq_nmod_mpoly}, Int, Ref{FqNmodMPolyRing}),
         z, a, i - 1, parent(a))
   return z
end

###############################################################################
#
#   Evaluation
#
###############################################################################

function evaluate(a::fq_nmod_mpoly, b::Vector{fq_nmod})
   length(b) != nvars(parent(a)) && error("Vector size incorrect in evaluate")
   z = base_ring(parent(a))()
   ccall((:fq_nmod_mpoly_evaluate_all_fq_nmod, libflint), Nothing,
         (Ref{fq_nmod}, Ref{fq_nmod_mpoly}, Ptr{Ref{fq_nmod}}, Ref{FqNmodMPolyRing}),
         z, a, b, parent(a))
   return z
end

function evaluate(a::fq_nmod_mpoly, b::Vector{Int})
   length(b) != nvars(parent(a)) && error("Vector size incorrect in evaluate")
   R = base_ring(parent(a))
   b2 = [R(d) for d in b]
   return evaluate(a, b2)
end

function evaluate(a::fq_nmod_mpoly, b::Vector{T}) where T <: Integer
   length(b) != nvars(parent(a)) && error("Vector size incorrect in evaluate")
   R = base_ring(parent(a))
   b2 = [R(d) for d in b]
   return evaluate(a, b2)
end

function evaluate(a::fq_nmod_mpoly, b::Vector{fmpz})
   length(b) != nvars(parent(a)) && error("Vector size incorrect in evaluate")
   R = base_ring(parent(a))
   b2 = [R(d) for d in b]
   return evaluate(a, b2)
end

function evaluate(a::fq_nmod_mpoly, b::Vector{UInt})
   length(b) != nvars(parent(a)) && error("Vector size incorrect in evaluate")
   R = base_ring(parent(a))
   b2 = [R(d) for d in b]
   return evaluate(a, b2)
end

function (a::fq_nmod_mpoly)(vals::fq_nmod...)
   length(vals) != nvars(parent(a)) && error("Number of variables does not match number of values")
   return evaluate(a, [vals...])
end

function (a::fq_nmod_mpoly)(vals::Integer...)
   length(vals) != nvars(parent(a)) && error("Number of variables does not match number of values")
   return evaluate(a, [vals...])
end

function (a::fq_nmod_mpoly)(vals::Union{NCRingElem, RingElement}...)
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

function zero!(a::fq_nmod_mpoly)
    ccall((:fq_nmod_mpoly_zero, libflint), Nothing,
          (Ref{fq_nmod_mpoly}, Ref{FqNmodMPolyRing}),
          a, a.parent)
    return a
end

function add!(a::fq_nmod_mpoly, b::fq_nmod_mpoly, c::fq_nmod_mpoly)
   ccall((:fq_nmod_mpoly_add, libflint), Nothing,
         (Ref{fq_nmod_mpoly}, Ref{fq_nmod_mpoly},
          Ref{fq_nmod_mpoly}, Ref{FqNmodMPolyRing}),
         a, b, c, a.parent)
   return a
end

function addeq!(a::fq_nmod_mpoly, b::fq_nmod_mpoly)
   ccall((:fq_nmod_mpoly_add, libflint), Nothing,
         (Ref{fq_nmod_mpoly}, Ref{fq_nmod_mpoly},
          Ref{fq_nmod_mpoly}, Ref{FqNmodMPolyRing}),
         a, a, b, a.parent)
   return a
end

function mul!(a::fq_nmod_mpoly, b::fq_nmod_mpoly, c::fq_nmod_mpoly)
   ccall((:fq_nmod_mpoly_mul, libflint), Nothing,
         (Ref{fq_nmod_mpoly}, Ref{fq_nmod_mpoly},
          Ref{fq_nmod_mpoly}, Ref{FqNmodMPolyRing}),
         a, b, c, a.parent)
   return a
end

# Set the n-th coefficient of a to c. If zero coefficients are inserted, they
# must be removed with combine_like_terms!
function setcoeff!(a::fq_nmod_mpoly, n::Int, c::fq_nmod)
   if n > length(a)
      ccall((:fq_nmod_mpoly_resize, libflint), Nothing,
            (Ref{fq_nmod_mpoly}, Int, Ref{FqNmodMPolyRing}), a, n, a.parent)
   end
   ccall((:fq_nmod_mpoly_set_term_coeff_fq_nmod, libflint), Nothing,
         (Ref{fq_nmod_mpoly}, Int, Ref{fq_nmod}, Ref{FqNmodMPolyRing}),
         a, n - 1, c, a.parent)
   return a
end

# Set the i-th coefficient of a to c. If zero coefficients are inserted, they
# must be removed with combine_like_terms!
setcoeff!(a::fq_nmod_mpoly, i::Int, c::Integer) = setcoeff!(a, i, base_ring(parent(a))(c))

# Set the i-th coefficient of a to c. If zero coefficients are inserted, they
# must be removed with combine_like_terms!
setcoeff!(a::fq_nmod_mpoly, i::Int, c::fmpz) = setcoeff!(a, i, base_ring(parent(a))(c))

# Remove zero terms and combine adjacent terms if they have the same monomial
# no sorting is performed
function combine_like_terms!(a::fq_nmod_mpoly)
   ccall((:fq_nmod_mpoly_combine_like_terms, libflint), Nothing,
         (Ref{fq_nmod_mpoly}, Ref{FqNmodMPolyRing}),
         a, a.parent)
   return a
end

###############################################################################
#
#   Manipulating terms and monomials
#
###############################################################################

# Return true if the exponents of the i-th exp. vector fit into UInts
function exponent_vector_fits_ui(a::fq_nmod_mpoly, i::Int)
   b = ccall((:fq_nmod_mpoly_term_exp_fits_ui, libflint), Cint,
             (Ref{fq_nmod_mpoly}, Int, Ref{FqNmodMPolyRing}), a, i - 1, a.parent)
   return Bool(b)
end

# Return true if the exponents of the i-th exp. vector fit into UInts
function exponent_vector_fits_int(a::fq_nmod_mpoly, i::Int)
   b = ccall((:fq_nmod_mpoly_term_exp_fits_si, libflint), Cint,
             (Ref{fq_nmod_mpoly}, Int, Ref{FqNmodMPolyRing}), a, i - 1, a.parent)
   return Bool(b)
end

# Return Julia array of UInt's corresponding to exponent vector of i-th term
function exponent_vector_ui(a::fq_nmod_mpoly, i::Int)
   z = Vector{UInt}(undef, nvars(parent(a)))
   ccall((:fq_nmod_mpoly_get_term_exp_ui, libflint), Nothing,
         (Ptr{UInt}, Ref{fq_nmod_mpoly}, Int, Ref{FqNmodMPolyRing}),
         z, a, i - 1, parent(a))
   return z
end

# Return Julia array of Int's corresponding to exponent vector of i-th term
function exponent_vector(a::fq_nmod_mpoly, i::Int)
   exponent_vector_fits_int(a, i) ||
      throw(DomainError(term(a, i), "exponents don't fit in `Int` (try exponent_vector_fmpz)"))
   z = Vector{Int}(undef, nvars(parent(a)))
   ccall((:fq_nmod_mpoly_get_term_exp_si, libflint), Nothing,
         (Ptr{Int}, Ref{fq_nmod_mpoly}, Int, Ref{FqNmodMPolyRing}),
         z, a, i - 1, parent(a))
   return z
end

# Return Julia array of fmpz's corresponding to exponent vector of i-th term
function exponent_vector_fmpz(a::fq_nmod_mpoly, i::Int)
   n = nvars(parent(a))
   z = Vector{fmpz}(undef, n)
   for j in 1:n
      z[j] = fmpz()
   end
   ccall((:fq_nmod_mpoly_get_term_exp_fmpz, libflint), Nothing,
         (Ptr{Ref{fmpz}}, Ref{fq_nmod_mpoly}, Int, Ref{FqNmodMPolyRing}),
         z, a, i - 1, parent(a))
   return z
end

# Return a generator for exponent vectors of $a$
function exponent_vectors_fmpz(a::fq_nmod_mpoly)
   return (exponent_vector_fmpz(a, i) for i in 1:length(a))
end

# Set exponent of n-th term to given vector of UInt's
# No sort is performed, so this is unsafe.
function set_exponent_vector!(a::fq_nmod_mpoly, n::Int, exps::Vector{UInt})
   if n > length(a)
      ccall((:fq_nmod_mpoly_resize, libflint), Nothing,
            (Ref{fq_nmod_mpoly}, Int, Ref{FqNmodMPolyRing}), a, n, a.parent)
   end
   ccall((:fq_nmod_mpoly_set_term_exp_ui, libflint), Nothing,
         (Ref{fq_nmod_mpoly}, Int, Ptr{UInt}, Ref{FqNmodMPolyRing}),
         a, n - 1, exps, parent(a))
   return a
end

# Set exponent of n-th term to given vector of Int's
# No sort is performed, so this is unsafe. The Int's must be positive, but
# no check is performed
function set_exponent_vector!(a::fq_nmod_mpoly, n::Int, exps::Vector{Int})
   if n > length(a)
      ccall((:fq_nmod_mpoly_resize, libflint), Nothing,
            (Ref{fq_nmod_mpoly}, Int, Ref{FqNmodMPolyRing}), a, n, a.parent)
   end
   ccall((:fq_nmod_mpoly_set_term_exp_ui, libflint), Nothing,
         (Ref{fq_nmod_mpoly}, Int, Ptr{Int}, Ref{FqNmodMPolyRing}),
         a, n - 1, exps, parent(a))
   return a
end

# Set exponent of n-th term to given vector of fmpz's
# No sort is performed, so this is unsafe
function set_exponent_vector!(a::fq_nmod_mpoly, n::Int, exps::Vector{fmpz})
   if n > length(a)
      ccall((:fq_nmod_mpoly_resize, libflint), Nothing,
            (Ref{fq_nmod_mpoly}, Int, Ref{FqNmodMPolyRing}), a, n, a.parent)
   end
   ccall((:fq_nmod_mpoly_set_term_exp_fmpz, libflint), Nothing,
         (Ref{fq_nmod_mpoly}, Int, Ptr{fmpz}, Ref{FqNmodMPolyRing}),
         a, n - 1, exps, parent(a))
   return a
end

# Return j-th coordinate of i-th exponent vector
function exponent(a::fq_nmod_mpoly, i::Int, j::Int)
   (j < 1 || j > nvars(parent(a))) && error("Invalid variable index")
   return ccall((:fq_nmod_mpoly_get_term_var_exp_ui, libflint), Int,
                (Ref{fq_nmod_mpoly}, Int, Int, Ref{FqNmodMPolyRing}),
                 a, i - 1, j - 1, a.parent)
end

# Return the coefficient of the term with the given exponent vector
# Return zero if there is no such term
function coeff(a::fq_nmod_mpoly, exps::Vector{UInt})
   z = base_ring(parent(a))()
   ccall((:fq_nmod_mpoly_get_coeff_fq_nmod_ui, libflint), UInt,
         (Ref{fq_nmod}, Ref{fq_nmod_mpoly}, Ptr{UInt}, Ref{FqNmodMPolyRing}),
         z, a, exps, parent(a))
   return z
end

# Return the coefficient of the term with the given exponent vector
# Return zero if there is no such term
function coeff(a::fq_nmod_mpoly, exps::Vector{Int})
   z = base_ring(parent(a))()
   ccall((:fq_nmod_mpoly_get_coeff_fq_nmod_ui, libflint), UInt,
         (Ref{fq_nmod}, Ref{fq_nmod_mpoly}, Ptr{Int}, Ref{FqNmodMPolyRing}),
         z, a, exps, parent(a))
   return z
end

# Set the coefficient of the term with the given exponent vector to the
# given fmpz. Removal of a zero term is performed.
function setcoeff!(a::fq_nmod_mpoly, exps::Vector{Int}, b::fq_nmod)
   ccall((:fq_nmod_mpoly_set_coeff_fq_nmod_ui, libflint), Nothing,
         (Ref{fq_nmod_mpoly}, UInt, Ptr{Int}, Ref{FqNmodMPolyRing}),
         a, b, exps, parent(a))
   return a
end

# Set the coefficient of the term with the given exponent vector to the
# given integer. Removal of a zero term is performed.
setcoeff!(a::fq_nmod_mpoly, exps::Vector{Int}, b::Union{Integer, nmod}) =
   setcoeff!(a, exps, base_ring(parent(a))(b))

# Sort the terms according to the ordering. This is only needed if unsafe
# functions such as those above have been called and terms have been inserted
# out of order. Note that like terms are not combined and zeros are not
# removed. For that, call combine_like_terms!
function sort_terms!(a::fq_nmod_mpoly)
   ccall((:fq_nmod_mpoly_sort_terms, libflint), Nothing,
         (Ref{fq_nmod_mpoly}, Ref{FqNmodMPolyRing}), a, a.parent)
   return a
end

# Return the i-th term of the polynomial, as a polynomial
function term(a::fq_nmod_mpoly, i::Int)
   z = parent(a)()
   ccall((:fq_nmod_mpoly_get_term, libflint), Nothing,
         (Ref{fq_nmod_mpoly}, Ref{fq_nmod_mpoly}, Int, Ref{FqNmodMPolyRing}),
          z, a, i - 1, a.parent)
   return z
end

# Return the i-th monomial of the polynomial, as a polynomial
function monomial(a::fq_nmod_mpoly, i::Int)
   z = parent(a)()
   ccall((:fq_nmod_mpoly_get_term_monomial, libflint), Nothing,
         (Ref{fq_nmod_mpoly}, Ref{fq_nmod_mpoly}, Int, Ref{FqNmodMPolyRing}),
          z, a, i - 1, a.parent)
   return z
end

# Sets the given polynomial m to the i-th monomial of the polynomial
function monomial!(m::fq_nmod_mpoly, a::fq_nmod_mpoly, i::Int)
   ccall((:fq_nmod_mpoly_get_term_monomial, libflint), Nothing,
         (Ref{fq_nmod_mpoly}, Ref{fq_nmod_mpoly}, Int, Ref{FqNmodMPolyRing}),
          m, a, i - 1, a.parent)
   return m
end

###############################################################################
#
#   Promotion rules
#
###############################################################################

promote_rule(::Type{fq_nmod_mpoly}, ::Type{V}) where {V <: Integer} = fq_nmod_mpoly

promote_rule(::Type{fq_nmod_mpoly}, ::Type{fq_nmod}) = fq_nmod_mpoly

###############################################################################
#
#   Parent object call overload
#
###############################################################################

function (R::FqNmodMPolyRing)()
   z = fq_nmod_mpoly(R)
   return z
end

function (R::FqNmodMPolyRing)(b::nmod)
   z = fq_nmod_mpoly(R, b.data)
   return z
end

function (R::FqNmodMPolyRing)(b::UInt)
   z = fq_nmod_mpoly(R, b)
   return z
end

function (R::FqNmodMPolyRing)(b::fq_nmod)
   parent(b) != base_ring(R) && error("Unable to coerce element")   
   z = fq_nmod_mpoly(R, b)
   return z
end

function (R::FqNmodMPolyRing)(b::Integer)
   return R(base_ring(R)(b))
end

function (R::FqNmodMPolyRing)(b::fmpz)
   return R(base_ring(R)(b))
end

function (R::FqNmodMPolyRing)(a::fq_nmod_mpoly)
   parent(a) != R && error("Unable to coerce polynomial")
   return a
end

# Create poly with given array of coefficients and array of exponent vectors (sorting is performed)
function (R::FqNmodMPolyRing)(a::Vector{fq_nmod}, b::Vector{Vector{T}}) where {T <: Union{fmpz, UInt, Int}}
   length(a) != length(b) && error("Coefficient and exponent vector must have the same length")

   for i in 1:length(b)
     length(b[i]) != nvars(R) && error("Exponent vector $i has length $(length(b[i])) (expected $(nvars(R))")
   end

   z = fq_nmod_mpoly(R, a, b)
   return z
end

# Create poly with given array of coefficients and array of exponent vectors (sorting is performed)
function (R::FqNmodMPolyRing)(a::Vector{Any}, b::Vector{Vector{T}}) where T
   n = nvars(R)
   length(a) != length(b) && error("Coefficient and exponent vector must have the same length")
   newa = map(R, a)
   newb = map(x -> map(FlintZZ, x), b)
   newaa = convert(Vector{fq_nmod}, newa)
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

function PolynomialRing(R::FqNmodFiniteField, s::Array{String, 1}; cached::Bool = true, ordering::Symbol = :lex)
   U = [Symbol(x) for x in s]
   parent_obj = FqNmodMPolyRing(R, U, ordering, cached)
   return tuple(parent_obj, gens(parent_obj))
end
