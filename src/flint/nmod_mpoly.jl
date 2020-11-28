###############################################################################
#
#   nmod_mpoly.jl : Flint multivariate polynomials over nmod
#
###############################################################################

export NmodMPolyRing, nmod_mpoly

###############################################################################
#
#   Data type and parent object methods
#
###############################################################################

parent_type(::Type{nmod_mpoly}) = NmodMPolyRing

elem_type(::Type{NmodMPolyRing}) = nmod_mpoly

elem_type(::NmodMPolyRing) = nmod_mpoly

symbols(a::NmodMPolyRing) = a.S

parent(a::nmod_mpoly) = a.parent

function check_parent(a::nmod_mpoly, b::nmod_mpoly)
   parent(a) != parent(b) &&
      error("Incompatible polynomial rings in polynomial operation")
end

nvars(a::NmodMPolyRing) = a.nvars

base_ring(a::NmodMPolyRing) = a.base_ring

base_ring(f::nmod_mpoly) = f.parent.base_ring

function ordering(a::NmodMPolyRing)
b = a.ord
#   b = ccall((:nmod_mpoly_ctx_ord, libflint), Cint, (Ref{NmodMPolyRing}, ), a)
   return flint_orderings[b + 1]
end

function gens(R::NmodMPolyRing)
   A = Vector{nmod_mpoly}(undef, R.nvars)
   for i = 1:R.nvars
      z = R()
      ccall((:nmod_mpoly_gen, libflint), Nothing,
            (Ref{nmod_mpoly}, Int, Ref{NmodMPolyRing}), z, i - 1, R)
      A[i] = z
   end
   return A
end

function gen(R::NmodMPolyRing, i::Int)
   n = nvars(R)
   (i <= 0 || i > n) && error("Index must be between 1 and $n")
   z = R()
   ccall((:nmod_mpoly_gen, libflint), Nothing,
         (Ref{nmod_mpoly}, Int, Ref{NmodMPolyRing}), z, i - 1, R)
   return z
end

function isgen(a::nmod_mpoly, i::Int)
   n = nvars(parent(a))
   (i <= 0 || i > n) && error("Index must be between 1 and $n")
   R = parent(a)
#   return Bool(ccall((:nmod_mpoly_is_gen, libflint), Cint,
#                     (Ref{nmod_mpoly}, Int, Ref{NmodMPolyRing}),
#                     a, i - 1, R))
   g = gen(parent(a), i)
   return a == g
end

function isgen(a::nmod_mpoly)
   n = nvars(parent(a))
   for i in 1:n
      isgen(a, i) && return true
   end
   return false
end

function deepcopy_internal(a::nmod_mpoly, dict::IdDict)
   z = parent(a)()
   ccall((:nmod_mpoly_set, libflint), Nothing,
         (Ref{nmod_mpoly}, Ref{nmod_mpoly}, Ref{NmodMPolyRing}),
         z, a, a.parent)
   return z
end

function length(a::nmod_mpoly)
   n = ccall((:nmod_mpoly_length, libflint), Int, (Ref{nmod_mpoly}, Ref{NmodMPolyRing}),
         a, a.parent)
   return n
end

function one(R::NmodMPolyRing)
   z = R()
   ccall((:nmod_mpoly_one, libflint), Nothing,
         (Ref{nmod_mpoly}, Ref{NmodMPolyRing}), z, R)
   return z
end

function zero(R::NmodMPolyRing)
   z = R()
   ccall((:nmod_mpoly_zero, libflint), Nothing,
         (Ref{nmod_mpoly}, Ref{NmodMPolyRing}), z, R)
   return z
end

function isone(a::nmod_mpoly)
   b = ccall((:nmod_mpoly_is_one, libflint), Cint,
             (Ref{nmod_mpoly}, Ref{NmodMPolyRing}), a, a.parent)
   return Bool(b)
end

function iszero(a::nmod_mpoly)
   b = ccall((:nmod_mpoly_is_zero, libflint), Cint,
             (Ref{nmod_mpoly}, Ref{NmodMPolyRing}), a, a.parent)
   return Bool(b)
end

function ismonomial(a::nmod_mpoly)
   return length(a) == 1 && coeff(a, 1) == 1
end

function isterm(a::nmod_mpoly)
   return length(a) == 1
end

function isunit(a::nmod_mpoly)
   return length(a) == 1 && total_degree(a) == 0 && isunit(coeff(a, 1))
end

function isconstant(a::nmod_mpoly)
   b = ccall((:nmod_mpoly_is_ui, libflint), Cint,
             (Ref{nmod_mpoly}, Ref{NmodMPolyRing}), a, parent(a))
   return Bool(b)
end

characteristic(R::NmodMPolyRing) = characteristic(base_ring(R))

################################################################################
#
#  Getting coefficients
#
################################################################################

function coeff(a::nmod_mpoly, i::Int)
   n = length(a)
   (i < 1 || i > n) && error("Index must be between 1 and $(length(a))")
   z = ccall((:nmod_mpoly_get_term_coeff_ui, libflint), UInt,
         (Ref{nmod_mpoly}, Int, Ref{NmodMPolyRing}),
         a, i - 1, a.parent)
   return base_ring(parent(a))(z)
end

function coeff(a::nmod_mpoly, b::nmod_mpoly)
   check_parent(a, b)
   !isone(length(b)) && error("Second argument must be a monomial")
   z = ccall((:nmod_mpoly_get_coeff_ui_monomial, libflint), UInt,
         (Ref{nmod_mpoly}, Ref{nmod_mpoly}, Ref{NmodMPolyRing}),
         a, b, parent(a))
   return base_ring(parent(a))(z)
end

###############################################################################
#
#   Basic manipulation
#
###############################################################################

# Degree in the i-th variable as an Int
function degree(a::nmod_mpoly, i::Int)
   n = nvars(parent(a))
   (i <= 0 || i > n) && error("Index must be between 1 and $n")
   d = ccall((:nmod_mpoly_degree_si, libflint), Int,
             (Ref{nmod_mpoly}, Int, Ref{NmodMPolyRing}), a, i - 1, a.parent)
   return d
end

# Degree in the i-th variable as an fmpz
function degree_fmpz(a::nmod_mpoly, i::Int)
   n = nvars(parent(a))
   (i <= 0 || i > n) && error("Index must be between 1 and $n")
   d = fmpz()
   ccall((:nmod_mpoly_degree_fmpz, libflint), Nothing,
         (Ref{fmpz}, Ref{nmod_mpoly}, Int, Ref{NmodMPolyRing}),
         d, a, i - 1, a.parent)
   return d
end

# Return true if degrees fit into an Int
function degrees_fit_int(a::nmod_mpoly)
   b = ccall((:nmod_mpoly_degrees_fit_si, libflint), Cint,
             (Ref{nmod_mpoly}, Ref{NmodMPolyRing}), a, a.parent)
   return Bool(b)
end

# Return an array of the max degrees in each variable
function degrees(a::nmod_mpoly)
   degs = Vector{Int}(undef, nvars(parent(a)))
   ccall((:nmod_mpoly_degrees_si, libflint), Nothing,
         (Ptr{Int}, Ref{nmod_mpoly}, Ref{NmodMPolyRing}),
         degs, a, a.parent)
   return degs
end

# Return an array of the max degrees as fmpzs in each variable
function degrees_fmpz(a::nmod_mpoly)
   n = nvars(parent(a))
   degs = Vector{fmpz}(undef, n)
   for i in 1:n
      degs[i] = fmpz()
   end
   ccall((:nmod_mpoly_degrees_fmpz, libflint), Nothing,
         (Ptr{Ref{fmpz}}, Ref{nmod_mpoly}, Ref{NmodMPolyRing}),
         degs, a, a.parent)
   return degs
end

# Return true if degree fits into an Int
function total_degree_fits_int(a::nmod_mpoly)
      b = ccall((:nmod_mpoly_total_degree_fits_si, libflint), Cint,
                (Ref{nmod_mpoly}, Ref{NmodMPolyRing}), a, a.parent)
      return Bool(b)
   end

# Total degree as an Int
function total_degree(a::nmod_mpoly)
   d = ccall((:nmod_mpoly_total_degree_si, libflint), Int,
             (Ref{nmod_mpoly}, Ref{NmodMPolyRing}), a, a.parent)
   return d
end

# Total degree as an fmpz
function total_degree_fmpz(a::nmod_mpoly)
   d = fmpz()
   ccall((:nmod_mpoly_total_degree_fmpz, libflint), Nothing,
         (Ref{fmpz}, Ref{nmod_mpoly}, Ref{NmodMPolyRing}),
            d, a, a.parent)
   return d
end

###############################################################################
#
#   Multivariable coefficient polynomials
#
###############################################################################

function coeff(a::nmod_mpoly, vars::Vector{Int}, exps::Vector{Int})
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
   ccall((:nmod_mpoly_get_coeff_vars_ui, libflint), Nothing,
         (Ref{nmod_mpoly}, Ref{nmod_mpoly}, Ptr{Int},
          Ptr{Int}, Int, Ref{NmodMPolyRing}),
          z, a, vars, exps, length(vars), a.parent)
   return z
end

###############################################################################
#
#   String I/O
#
###############################################################################

function show(io::IO, p::NmodMPolyRing)
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

function -(a::nmod_mpoly)
   z = parent(a)()
   ccall((:nmod_mpoly_neg, libflint), Nothing,
       (Ref{nmod_mpoly}, Ref{nmod_mpoly}, Ref{NmodMPolyRing}),
       z, a, a.parent)
   return z
end

function +(a::nmod_mpoly, b::nmod_mpoly)
   check_parent(a, b)
   z = parent(a)()
   ccall((:nmod_mpoly_add, libflint), Nothing,
       (Ref{nmod_mpoly}, Ref{nmod_mpoly}, Ref{nmod_mpoly}, Ref{NmodMPolyRing}),
       z, a, b, a.parent)
   return z
end

function -(a::nmod_mpoly, b::nmod_mpoly)
   check_parent(a, b)
   z = parent(a)()
   ccall((:nmod_mpoly_sub, libflint), Nothing,
       (Ref{nmod_mpoly}, Ref{nmod_mpoly}, Ref{nmod_mpoly}, Ref{NmodMPolyRing}),
       z, a, b, a.parent)
   return z
end

function *(a::nmod_mpoly, b::nmod_mpoly)
   check_parent(a, b)
   z = parent(a)()
   ccall((:nmod_mpoly_mul, libflint), Nothing,
       (Ref{nmod_mpoly}, Ref{nmod_mpoly}, Ref{nmod_mpoly}, Ref{NmodMPolyRing}),
       z, a, b, a.parent)
   return z
end

###############################################################################
#
#   Ad hoc arithmetic
#
###############################################################################

for (jT, cN, cT) in ((UInt, :ui, UInt),)
   @eval begin
      function +(a::nmod_mpoly, b::($jT))
         z = parent(a)()
         ccall(($(string(:nmod_mpoly_add_, cN)), libflint), Nothing,
               (Ref{nmod_mpoly}, Ref{nmod_mpoly}, ($cT), Ref{NmodMPolyRing}),
               z, a, b, parent(a))
         return z
      end

      +(a::($jT), b::nmod_mpoly) = b + a

      function -(a::nmod_mpoly, b::($jT))
         z = parent(a)()
         ccall(($(string(:nmod_mpoly_sub_, cN)), libflint), Nothing,
               (Ref{nmod_mpoly}, Ref{nmod_mpoly}, ($cT), Ref{NmodMPolyRing}),
               z, a, b, parent(a))
         return z
      end

      -(a::($jT), b::nmod_mpoly) = - (b - a)

      function *(a::nmod_mpoly, b::($jT))
         z = parent(a)()
         ccall(($(string(:nmod_mpoly_scalar_mul_, cN)), libflint), Nothing,
               (Ref{nmod_mpoly}, Ref{nmod_mpoly}, ($cT), Ref{NmodMPolyRing}),
               z, a, b, parent(a))
         return z
      end

      *(a::($jT), b::nmod_mpoly) = b * a

      function divexact(a::nmod_mpoly, b::($jT))
         z = parent(a)()
         ccall(($(string(:nmod_mpoly_scalar_div_, cN)), libflint), Nothing,
               (Ref{nmod_mpoly}, Ref{nmod_mpoly}, ($cT), Ref{NmodMPolyRing}),
               z, a, b, parent(a))
         return z
      end
   end
end

+(a::nmod_mpoly, b::Integer) = a + base_ring(parent(a))(b)

+(a::Integer, b::nmod_mpoly) = b + a

-(a::nmod_mpoly, b::Integer) = a - base_ring(parent(a))(b)

-(a::Integer, b::nmod_mpoly) = base_ring(parent(b))(a) - b

*(a::nmod_mpoly, b::Integer) = a*base_ring(parent(a))(b)

*(a::Integer, b::nmod_mpoly) = b*a

divexact(a::nmod_mpoly, b::Integer) = divexact(a, base_ring(parent(a))(b))

###############################################################################
#
#   Powering
#
###############################################################################

function ^(a::nmod_mpoly, b::Int)
   b < 0 && throw(DomainError(b, "Exponent must be non-negative"))
   z = parent(a)()
   ccall((:nmod_mpoly_pow_ui, libflint), Nothing,
         (Ref{nmod_mpoly}, Ref{nmod_mpoly}, Int, Ref{NmodMPolyRing}),
         z, a, b, parent(a))
   return z
end

function ^(a::nmod_mpoly, b::fmpz)
   b < 0 && throw(DomainError(b, "Exponent must be non-negative"))
   z = parent(a)()
   ccall((:nmod_mpoly_pow_fmpz, libflint), Nothing,
         (Ref{nmod_mpoly}, Ref{nmod_mpoly}, Ref{fmpz}, Ref{NmodMPolyRing}),
         z, a, b, parent(a))
   return z
end

################################################################################
#
#   GCD
#
################################################################################

function gcd(a::nmod_mpoly, b::nmod_mpoly)
   check_parent(a, b)
   z = parent(a)()
   r = Bool(ccall((:nmod_mpoly_gcd, libflint), Cint,
         (Ref{nmod_mpoly}, Ref{nmod_mpoly}, Ref{nmod_mpoly}, Ref{NmodMPolyRing}),
         z, a, b, a.parent))
   r == false && error("Unable to compute gcd")
   return z
end

################################################################################
#
#   Factorization and Square Root
#
################################################################################

function Base.convert(::Type{Fac{nmod_mpoly}}, fac::nmod_mpoly_factor)
   R = fac.parent
   dic = Dict{nmod_mpoly, Int}()
   for i in 0:fac.num-1
      f = R()
      # use get_base instead of swap_base if convert is to preserve input
      ccall((:nmod_mpoly_factor_swap_base, libflint), Nothing,
            (Ref{nmod_mpoly}, Ref{nmod_mpoly_factor}, Int, Ref{NmodMPolyRing}),
            f, fac, i, R)
      dic[f] = ccall((:nmod_mpoly_factor_get_exp_si, libflint), Int,
                     (Ref{nmod_mpoly_factor}, Int, Ref{NmodMPolyRing}),
                     fac, i, R)
   end
   c = ccall((:nmod_mpoly_factor_get_constant_ui, libflint), UInt,
             (Ref{nmod_mpoly_factor}, ),
             fac)
   return Fac(R(c), dic)
end

function factor(a::nmod_mpoly)
   R = parent(a)
   fac = nmod_mpoly_factor(R)
   ok = ccall((:nmod_mpoly_factor, libflint), Cint,
              (Ref{nmod_mpoly_factor}, Ref{nmod_mpoly}, Ref{NmodMPolyRing}),
              fac, a, R)
   ok == 0 && error("unable to compute factorization")
   return convert(Fac{nmod_mpoly}, fac)
end

function factor_squarefree(a::nmod_mpoly)
   R = parent(a)
   fac = nmod_mpoly_factor(R)
   ok = ccall((:nmod_mpoly_factor_squarefree, libflint), Cint,
              (Ref{nmod_mpoly_factor}, Ref{nmod_mpoly}, Ref{NmodMPolyRing}),
              fac, a, R)
   ok == 0 && error("unable to compute factorization")
   return convert(Fac{nmod_mpoly}, fac)
end


function square_root(a::nmod_mpoly)
   (flag, q) = issquare_with_square_root(a)
   !flag && error("Not a square in square_root")
   return q
end

function issquare(a::nmod_mpoly)
   return Bool(ccall((:nmod_mpoly_is_square, libflint), Cint,
                     (Ref{nmod_mpoly}, Ref{NmodMPolyRing}),
                     a, a.parent))
end

function issquare_with_square_root(a::nmod_mpoly)
   q = parent(a)()
   flag = ccall((:nmod_mpoly_sqrt, libflint), Cint,
                (Ref{nmod_mpoly}, Ref{nmod_mpoly}, Ref{NmodMPolyRing}),
                q, a, a.parent)
   return (Bool(flag), q)
end

###############################################################################
#
#   Comparison
#
###############################################################################

function ==(a::nmod_mpoly, b::nmod_mpoly)
   check_parent(a, b)
   return Bool(ccall((:nmod_mpoly_equal, libflint), Cint,
               (Ref{nmod_mpoly}, Ref{nmod_mpoly}, Ref{NmodMPolyRing}),
               a, b, a.parent))
end

function Base.isless(a::nmod_mpoly, b::nmod_mpoly)
   (!ismonomial(a) || !ismonomial(b)) && error("Not monomials in comparison")
   return ccall((:nmod_mpoly_cmp, libflint), Cint,
               (Ref{nmod_mpoly}, Ref{nmod_mpoly}, Ref{NmodMPolyRing}),
               a, b, a.parent) < 0
end

###############################################################################
#
#   Ad hoc comparison
#
###############################################################################

function ==(a::nmod_mpoly, b::nmod)
   return Bool(ccall((:nmod_mpoly_equal_ui, libflint), Cint,
                     (Ref{nmod_mpoly}, UInt, Ref{NmodMPolyRing}),
                     a, b.data, a.parent))
end

==(a::nmod, b::nmod_mpoly) = b == a

function ==(a::nmod_mpoly, b::UInt)
   return Bool(ccall((:nmod_mpoly_equal_ui, libflint), Cint,
               (Ref{nmod_mpoly}, UInt, Ref{NmodMPolyRing}),
               a, b, a.parent))
end

==(a::UInt, b::nmod_mpoly) = b == a

==(a::nmod_mpoly, b::Integer) = a == base_ring(parent(a))(b)

==(a::nmod_mpoly, b::fmpz) = a == base_ring(parent(a))(b)

==(a::Integer, b::nmod_mpoly) = b == a

==(a::fmpz, b::nmod_mpoly) = b == a

###############################################################################
#
#   Divisibility
#
###############################################################################

function divides(a::nmod_mpoly, b::nmod_mpoly)
   check_parent(a, b)
   if iszero(a)
      return true, zero(parent(a))
   end
   if iszero(b)
      return false, zero(parent(a))
   end
   z = parent(a)()
   d = ccall((:nmod_mpoly_divides, libflint), Cint,
       (Ref{nmod_mpoly}, Ref{nmod_mpoly}, Ref{nmod_mpoly}, Ref{NmodMPolyRing}),
       z, a, b, a.parent)
   return isone(d), z
end

###############################################################################
#
#   Division with remainder
#
###############################################################################

function Base.div(a::nmod_mpoly, b::nmod_mpoly)
   check_parent(a, b)
   q = parent(a)()
   ccall((:nmod_mpoly_div, libflint), Nothing,
       (Ref{nmod_mpoly}, Ref{nmod_mpoly},
        Ref{nmod_mpoly}, Ref{NmodMPolyRing}),
       q, a, b, a.parent)
   return q
end

function Base.divrem(a::nmod_mpoly, b::nmod_mpoly)
   check_parent(a, b)
   q = parent(a)()
   r = parent(a)()
   ccall((:nmod_mpoly_divrem, libflint), Nothing,
       (Ref{nmod_mpoly}, Ref{nmod_mpoly}, Ref{nmod_mpoly},
        Ref{nmod_mpoly}, Ref{NmodMPolyRing}),
       q, r, a, b, a.parent)
   return q, r
end

function Base.divrem(a::nmod_mpoly, b::Array{nmod_mpoly, 1})
   len = length(b)
   q = [parent(a)() for i in 1:len]
   r = parent(a)()
   ccall((:nmod_mpoly_divrem_ideal, libflint), Nothing,
         (Ptr{Ref{nmod_mpoly}}, Ref{nmod_mpoly}, Ref{nmod_mpoly},
          Ptr{Ref{nmod_mpoly}}, Int, Ref{NmodMPolyRing}),
       q, r, a, b, len, a.parent)
   return q, r
end

###############################################################################
#
#   Exact division
#
###############################################################################

function divexact(a::nmod_mpoly, b::nmod_mpoly)
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

function derivative(a::nmod_mpoly, i::Int)
   n = nvars(parent(a))
   (i <= 0 || i > n) && error("Index must be between 1 and $n")
   z = parent(a)()
   ccall((:nmod_mpoly_derivative, libflint), Nothing,
         (Ref{nmod_mpoly}, Ref{nmod_mpoly}, Int, Ref{NmodMPolyRing}),
         z, a, i - 1, parent(a))
   return z
end

function integral(a::nmod_mpoly, i::Int)
   n = nvars(parent(a))
   (i <= 0 || i > n) && error("Index must be between 1 and $n")
   z = parent(a)()
   ccall((:nmod_mpoly_integral, libflint), Nothing,
         (Ref{nmod_mpoly}, Ref{nmod_mpoly}, Int, Ref{NmodMPolyRing}),
         z, a, i - 1, parent(a))
   return z
end

###############################################################################
#
#   Evaluation
#
###############################################################################

function evaluate(a::nmod_mpoly, b::Vector{nmod})
   length(b) != nvars(parent(a)) && error("Vector size incorrect in evaluate")
   b2 = [d.data for d in b]
   z = ccall((:nmod_mpoly_evaluate_all_ui, libflint), UInt,
         (Ref{nmod_mpoly}, Ptr{UInt}, Ref{NmodMPolyRing}),
            a, b2, parent(a))
   return base_ring(parent(a))(z)
end

function evaluate(a::nmod_mpoly, b::Vector{Int})
   length(b) != nvars(parent(a)) && error("Vector size incorrect in evaluate")
   R = base_ring(parent(a))
   b2 = [R(d) for d in b]
   return evaluate(a, b2)
end

function evaluate(a::nmod_mpoly, b::Vector{T}) where T <: Integer
   length(b) != nvars(parent(a)) && error("Vector size incorrect in evaluate")
   R = base_ring(parent(a))
   b2 = [R(d) for d in b]
   return evaluate(a, b2)
end

function evaluate(a::nmod_mpoly, b::Vector{fmpz})
   length(b) != nvars(parent(a)) && error("Vector size incorrect in evaluate")
   R = base_ring(parent(a))
   b2 = [R(d) for d in b]
   return evaluate(a, b2)
end

function evaluate(a::nmod_mpoly, b::Vector{UInt})
   length(b) != nvars(parent(a)) && error("Vector size incorrect in evaluate")
   R = base_ring(parent(a))
   b2 = [R(d) for d in b]
   return evaluate(a, b2)
end

function (a::nmod_mpoly)(vals::nmod...)
   length(vals) != nvars(parent(a)) && error("Number of variables does not match number o
f values")
   return evaluate(a, [vals...])
end

function (a::nmod_mpoly)(vals::Integer...)
   length(vals) != nvars(parent(a)) && error("Number of variables does not match number o
f values")
   return evaluate(a, [vals...])
end

function (a::nmod_mpoly)(vals::Union{NCRingElem, RingElement}...)
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

function zero!(a::nmod_mpoly)
    ccall((:nmod_mpoly_zero, libflint), Nothing,
         (Ref{nmod_mpoly}, Ref{NmodMPolyRing}), a, a.parent)
    return a
end

function add!(a::nmod_mpoly, b::nmod_mpoly, c::nmod_mpoly)
   ccall((:nmod_mpoly_add, libflint), Nothing,
         (Ref{nmod_mpoly}, Ref{nmod_mpoly},
          Ref{nmod_mpoly}, Ref{NmodMPolyRing}), a, b, c, a.parent)
   return a
end

function addeq!(a::nmod_mpoly, b::nmod_mpoly)
   ccall((:nmod_mpoly_add, libflint), Nothing,
         (Ref{nmod_mpoly}, Ref{nmod_mpoly},
          Ref{nmod_mpoly}, Ref{NmodMPolyRing}), a, a, b, a.parent)
   return a
end

function mul!(a::nmod_mpoly, b::nmod_mpoly, c::nmod_mpoly)
   ccall((:nmod_mpoly_mul, libflint), Nothing,
         (Ref{nmod_mpoly}, Ref{nmod_mpoly},
          Ref{nmod_mpoly}, Ref{NmodMPolyRing}), a, b, c, a.parent)
   return a
end

# Set the n-th coefficient of a to c. If zero coefficients are inserted, they
# must be removed with combine_like_terms!
function setcoeff!(a::nmod_mpoly, n::Int, c::nmod)
   if n > length(a)
      ccall((:nmod_mpoly_resize, libflint), Nothing,
            (Ref{nmod_mpoly}, Int, Ref{NmodMPolyRing}), a, n, a.parent)
   end
   ccall((:nmod_mpoly_set_term_coeff_ui, libflint), Nothing,
         (Ref{nmod_mpoly}, Int, UInt, Ref{NmodMPolyRing}),
         a, n - 1, c.data, a.parent)
   return a
end

# Set the i-th coefficient of a to c. If zero coefficients are inserted, they
# must be removed with combine_like_terms!
setcoeff!(a::nmod_mpoly, i::Int, c::Integer) = setcoeff!(a, i, base_ring(parent(a))(c))

# Set the i-th coefficient of a to c. If zero coefficients are inserted, they
# must be removed with combine_like_terms!
setcoeff!(a::nmod_mpoly, i::Int, c::fmpz) = setcoeff!(a, i, base_ring(parent(a))(c))

# Remove zero terms and combine adjacent terms if they have the same monomial
# no sorting is performed
function combine_like_terms!(a::nmod_mpoly)
   ccall((:nmod_mpoly_combine_like_terms, libflint), Nothing,
         (Ref{nmod_mpoly}, Ref{NmodMPolyRing}), a, a.parent)
   return a
end

###############################################################################
#
#   Manipulating terms and monomials
#
###############################################################################

# Return true if the exponents of the i-th exp. vector fit into UInts
function exponent_vector_fits_ui(a::nmod_mpoly, i::Int)
   b = ccall((:nmod_mpoly_term_exp_fits_ui, libflint), Cint,
             (Ref{nmod_mpoly}, Int, Ref{NmodMPolyRing}), a, i - 1, a.parent)
      return Bool(b)
end

# Return true if the exponents of the i-th exp. vector fit into UInts
function exponent_vector_fits_int(a::nmod_mpoly, i::Int)
   b = ccall((:nmod_mpoly_term_exp_fits_si, libflint), Cint,
             (Ref{nmod_mpoly}, Int, Ref{NmodMPolyRing}), a, i - 1, a.parent)
   return Bool(b)
end

# Return Julia array of UInt's corresponding to exponent vector of i-th term
function exponent_vector_ui(a::nmod_mpoly, i::Int)
   z = Vector{UInt}(undef, nvars(parent(a)))
   ccall((:nmod_mpoly_get_term_exp_ui, libflint), Nothing,
         (Ptr{UInt}, Ref{nmod_mpoly}, Int, Ref{NmodMPolyRing}),
      z, a, i - 1, parent(a))
   return z
end

# Return Julia array of Int's corresponding to exponent vector of i-th term
function exponent_vector(a::nmod_mpoly, i::Int)
   exponent_vector_fits_int(a, i) ||
      throw(DomainError(term(a, i), "exponents don't fit in `Int` (try exponent_vector_fmpz)"))
   z = Vector{Int}(undef, nvars(parent(a)))
   ccall((:nmod_mpoly_get_term_exp_si, libflint), Nothing,
         (Ptr{Int}, Ref{nmod_mpoly}, Int, Ref{NmodMPolyRing}),
      z, a, i - 1, parent(a))
   return z
end

# Return Julia array of fmpz's corresponding to exponent vector of i-th term
function exponent_vector_fmpz(a::nmod_mpoly, i::Int)
   n = nvars(parent(a))
   z = Vector{fmpz}(undef, n)
   for j in 1:n
      z[j] = fmpz()
   end
   ccall((:nmod_mpoly_get_term_exp_fmpz, libflint), Nothing,
         (Ptr{Ref{fmpz}}, Ref{nmod_mpoly}, Int, Ref{NmodMPolyRing}),
         z, a, i - 1, parent(a))
   return z
end

# Return a generator for exponent vectors of $a$
function exponent_vectors_fmpz(a::nmod_mpoly)
   return (exponent_vector_fmpz(a, i) for i in 1:length(a))
end

# Set exponent of n-th term to given vector of UInt's
# No sort is performed, so this is unsafe. These are promoted to fmpz's if
# they don't fit into 31/63 bits
function set_exponent_vector!(a::nmod_mpoly, n::Int, exps::Vector{UInt})
   if n > length(a)
      ccall((:nmod_mpoly_resize, libflint), Nothing,
            (Ref{nmod_mpoly}, Int, Ref{NmodMPolyRing}), a, n, a.parent)
   end
   ccall((:nmod_mpoly_set_term_exp_ui, libflint), Nothing,
         (Ref{nmod_mpoly}, Int, Ptr{UInt}, Ref{NmodMPolyRing}),
      a, n - 1, exps, parent(a))
   return a
end

# Set exponent of n-th term to given vector of Int's
# No sort is performed, so this is unsafe. The Int's must be positive, but
# no check is performed
function set_exponent_vector!(a::nmod_mpoly, n::Int, exps::Vector{Int})
   if n > length(a)
      ccall((:nmod_mpoly_resize, libflint), Nothing,
            (Ref{nmod_mpoly}, Int, Ref{NmodMPolyRing}), a, n, a.parent)
   end
   ccall((:nmod_mpoly_set_term_exp_ui, libflint), Nothing,
         (Ref{nmod_mpoly}, Int, Ptr{Int}, Ref{NmodMPolyRing}),
      a, n - 1, exps, parent(a))
   return a
end

# Set exponent of n-th term to given vector of fmpz's
# No sort is performed, so this is unsafe
function set_exponent_vector!(a::nmod_mpoly, n::Int, exps::Vector{fmpz})
   if n > length(a)
      ccall((:nmod_mpoly_resize, libflint), Nothing,
            (Ref{nmod_mpoly}, Int, Ref{NmodMPolyRing}), a, n, a.parent)
   end
   @GC.preserve exps ccall((:nmod_mpoly_set_term_exp_fmpz, libflint), Nothing,
         (Ref{nmod_mpoly}, Int, Ptr{fmpz}, Ref{NmodMPolyRing}),
      a, n - 1, exps, parent(a))
   return a
end

# Return j-th coordinate of i-th exponent vector
function exponent(a::nmod_mpoly, i::Int, j::Int)
   (j < 1 || j > nvars(parent(a))) && error("Invalid variable index")
   return ccall((:nmod_mpoly_get_term_var_exp_ui, libflint), Int,
                (Ref{nmod_mpoly}, Int, Int, Ref{NmodMPolyRing}),
                 a, i - 1, j - 1, a.parent)
end

# Return the coefficient of the term with the given exponent vector
# Return zero if there is no such term
function coeff(a::nmod_mpoly, exps::Vector{UInt})
   z = ccall((:nmod_mpoly_get_coeff_ui_ui, libflint), UInt,
         (Ref{nmod_mpoly}, Ptr{UInt}, Ref{NmodMPolyRing}),
      a, exps, parent(a))
   return base_ring(parent(a))(z)
end

# Return the coefficient of the term with the given exponent vector
# Return zero if there is no such term
function coeff(a::nmod_mpoly, exps::Vector{Int})
   z = ccall((:nmod_mpoly_get_coeff_ui_ui, libflint), UInt,
         (Ref{nmod_mpoly}, Ptr{Int}, Ref{NmodMPolyRing}),
      a, exps, parent(a))
   return base_ring(parent(a))(z)
end

# Set the coefficient of the term with the given exponent vector to the
# given fmpz. Removal of a zero term is performed.
function setcoeff!(a::nmod_mpoly, exps::Vector{UInt}, b::nmod)
   ccall((:nmod_mpoly_set_coeff_ui_ui, libflint), Nothing,
         (Ref{nmod_mpoly}, UInt, Ptr{UInt}, Ref{NmodMPolyRing}),
      a, b.data, exps, parent(a))
   return a
end

# Set the coefficient of the term with the given exponent vector to the
# given fmpz. Removal of a zero term is performed.
function setcoeff!(a::nmod_mpoly, exps::Vector{Int}, b::nmod)
   ccall((:nmod_mpoly_set_coeff_fmpz_ui, libflint), Nothing,
         (Ref{nmod_mpoly}, UInt, Ptr{Int}, Ref{NmodMPolyRing}),
      a, b.data, exps, parent(a))
   return a
end

# Set the coefficient of the term with the given exponent vector to the
# given integer. Removal of a zero term is performed.
setcoeff!(a::nmod_mpoly, exps::Vector{Int}, b::Integer) =
   setcoeff!(a, exps, base_ring(parent(a))(b))

# Sort the terms according to the ordering. This is only needed if unsafe
# functions such as those above have been called and terms have been inserted
# out of order. Note that like terms are not combined and zeros are not
# removed. For that, call combine_like_terms!
function sort_terms!(a::nmod_mpoly)
   ccall((:nmod_mpoly_sort_terms, libflint), Nothing,
         (Ref{nmod_mpoly}, Ref{NmodMPolyRing}), a, a.parent)
   return a
end

# Return the i-th term of the polynomial, as a polynomial
function term(a::nmod_mpoly, i::Int)
   z = parent(a)()
   ccall((:nmod_mpoly_get_term, libflint), Nothing,
         (Ref{nmod_mpoly}, Ref{nmod_mpoly}, Int, Ref{NmodMPolyRing}),
          z, a, i - 1, a.parent)
   return z
end

# Return the i-th monomial of the polynomial, as a polynomial
function monomial(a::nmod_mpoly, i::Int)
   z = parent(a)()
   ccall((:nmod_mpoly_get_term_monomial, libflint), Nothing,
         (Ref{nmod_mpoly}, Ref{nmod_mpoly}, Int, Ref{NmodMPolyRing}),
          z, a, i - 1, a.parent)
   return z
end

# Sets the given polynomial m to the i-th monomial of the polynomial
function monomial!(m::nmod_mpoly, a::nmod_mpoly, i::Int)
   ccall((:nmod_mpoly_get_term_monomial, libflint), Nothing,
         (Ref{nmod_mpoly}, Ref{nmod_mpoly}, Int, Ref{NmodMPolyRing}),
          m, a, i - 1, a.parent)
   return m
end

###############################################################################
#
#   Promotion rules
#
###############################################################################

promote_rule(::Type{nmod_mpoly}, ::Type{V}) where {V <: Integer} = nmod_mpoly

promote_rule(::Type{nmod_mpoly}, ::Type{nmod}) = nmod_mpoly

###############################################################################
#
#   Parent object call overload
#
###############################################################################

function (R::NmodMPolyRing)()
   z = nmod_mpoly(R)
   return z
end

function (R::NmodMPolyRing)(b::nmod)
   z = nmod_mpoly(R, b)
   return z
end

function (R::NmodMPolyRing)(b::Int)
   z = nmod_mpoly(R, base_ring(R)(b))
   return z
end

function (R::NmodMPolyRing)(b::UInt)
   z = nmod_mpoly(R, b)
   return z
end

function (R::NmodMPolyRing)(b::Integer)
   return R(base_ring(R)(b))
end

function (R::NmodMPolyRing)(b::fmpz)
   return R(base_ring(R)(b))
end

function (R::NmodMPolyRing)(a::nmod_mpoly)
   parent(a) != R && error("Unable to coerce polynomial")
   return a
end

# Create poly with given array of coefficients and array of exponent vectors (sorting is performed)
function (R::NmodMPolyRing)(a::Vector{nmod}, b::Vector{Vector{T}}) where {T <: Union{fmpz, UInt}}
   length(a) != length(b) && error("Coefficient and exponent vector must have the same length")

   for i in 1:length(b)
     length(b[i]) != nvars(R) && error("Exponent vector $i has length $(length(b[i])) (expected $(nvars(R))")
   end

   z = nmod_mpoly(R, a, b)
   return z
end

# Create poly with given array of coefficients and array of exponent vectors (sorting is performed)
function (R::NmodMPolyRing)(a::Vector{nmod}, b::Vector{Vector{Int}})
   length(a) != length(b) && error("Coefficient and exponent vector must have the same length")

   for i in 1:length(b)
      length(b[i]) != nvars(R) && error("Exponent vector $i has length $(length(b[i])) (expected $(nvars(R)))")
   end

   z = nmod_mpoly(R, a, b)
   return z
end

# Create poly with given array of coefficients and array of exponent vectors (sorting is performed)
function (R::NmodMPolyRing)(a::Vector{Any}, b::Vector{Vector{T}}) where T
   n = nvars(R)
   length(a) != length(b) && error("Coefficient and exponent vector must have the same length")
   newa = map(R, a)
   newb = map(x -> map(FlintZZ, x), b)
   newaa = convert(Vector{nmod}, newa)
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

function PolynomialRing(R::NmodRing, s::Array{String, 1}; cached::Bool = true, ordering::Symbol = :lex)
   U = [Symbol(x) for x in s]
   parent_obj = NmodMPolyRing(R, U, ordering, cached)
   return tuple(parent_obj, gens(parent_obj))
end
