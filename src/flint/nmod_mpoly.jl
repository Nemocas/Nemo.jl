###############################################################################
#
#   nmod_mpoly.jl : FLINT multivariate polynomials over nmod and GaloisField
#
###############################################################################

for (etype, rtype, ftype, ctype, utype) in (
                                            (zzModMPolyRingElem, zzModMPolyRing, nmod_mpoly_factor, zzModRingElem, zzModPolyRingElem),
                                            (fpMPolyRingElem, fpMPolyRing, gfp_mpoly_factor, fpFieldElem, fpPolyRingElem))
  @eval begin

    ###############################################################################
    #
    #   Data type and parent object methods
    #
    ###############################################################################

    parent_type(::Type{($etype)}) = ($rtype)

    elem_type(::Type{($rtype)}) = ($etype)

    mpoly_type(::Type{$ctype}) = $etype

    symbols(a::($rtype)) = a.S

    parent(a::($etype)) = a.parent

    number_of_variables(a::($rtype)) = a.nvars

    base_ring(a::($rtype)) = a.base_ring

    characteristic(R::($rtype)) = characteristic(base_ring(R)) # characteristic of Z/4Z?

    modulus(R::($rtype)) = modulus(base_ring(R))

    modulus(f::($etype)) = modulus(base_ring(parent(f)))


    function internal_ordering(a::($rtype))
      b = a.ord
      #   b = @ccall libflint.nmod_mpoly_ctx_ord(a::Ref{zzModMPolyRing})::Cint
      return flint_orderings[b + 1]
    end

    function gens(R::($rtype))
      A = Vector{($etype)}(undef, R.nvars)
      for i = 1:R.nvars
        z = R()
        @ccall libflint.nmod_mpoly_gen(z::Ref{($etype)}, (i - 1)::Int, R::Ref{($rtype)})::Nothing
        A[i] = z
      end
      return A
    end

    function gen(R::($rtype), i::Int)
      n = nvars(R)
      (i <= 0 || i > n) && error("Index must be between 1 and $n")
      z = R()
      @ccall libflint.nmod_mpoly_gen(z::Ref{($etype)}, (i - 1)::Int, R::Ref{($rtype)})::Nothing
      return z
    end

    function is_gen(a::($etype), i::Int)
      n = nvars(parent(a))
      (i <= 0 || i > n) && error("Index must be between 1 and $n")
      return Bool(@ccall libflint.nmod_mpoly_is_gen(a::Ref{($etype)}, (i - 1)::Int, parent(a)::Ref{($rtype)})::Cint)
    end

    function is_gen(a::($etype))
      return Bool(@ccall libflint.nmod_mpoly_is_gen(a::Ref{($etype)}, (-1)::Int, parent(a)::Ref{($rtype)})::Cint)
    end

    function deepcopy_internal(a::($etype), dict::IdDict)
      z = parent(a)()
      @ccall libflint.nmod_mpoly_set(z::Ref{($etype)}, a::Ref{($etype)}, parent(a)::Ref{($rtype)})::Nothing
      return z
    end

    function length(a::($etype))
      return a.length
    end

    one(R::($rtype)) = one!(R())

    zero(R::($rtype)) = zero!(R())

    function isone(a::($etype))
      return Bool(@ccall libflint.nmod_mpoly_is_one(a::Ref{($etype)}, parent(a)::Ref{($rtype)})::Cint)
    end

    function iszero(a::($etype))
      return Bool(@ccall libflint.nmod_mpoly_is_zero(a::Ref{($etype)}, parent(a)::Ref{($rtype)})::Cint)
    end

    function is_monomial(a::($etype))
      return length(a) == 1 && coeff(a, 1) == 1
    end

    function is_constant(a::($etype))
      return Bool(@ccall libflint.nmod_mpoly_is_ui(a::Ref{($etype)}, parent(a)::Ref{($rtype)})::Cint)
    end

    function fit!(a::($etype), n::Int)
      # needs to exist for the MPoly interface
      return nothing
    end

    ################################################################################
    #
    #  Getting coefficients
    #
    ################################################################################

    function coeff(a::($etype), i::Int)
      n = length(a)
      (i < 1 || i > n) && error("Index must be between 1 and $(length(a))")
      z = @ccall libflint.nmod_mpoly_get_term_coeff_ui(a::Ref{($etype)}, (i - 1)::Int, parent(a)::Ref{($rtype)})::UInt
      return base_ring(parent(a))(z)
    end

    function coeff(a::($etype), b::($etype))
      check_parent(a, b)
      !isone(length(b)) && error("Second argument must be a monomial")
      z = @ccall libflint.nmod_mpoly_get_coeff_ui_monomial(a::Ref{($etype)}, b::Ref{($etype)}, parent(a)::Ref{($rtype)})::UInt
      return base_ring(parent(a))(z)
    end

    function trailing_coefficient(p::($etype))
      @req !iszero(p) "Zero polynomial does not have a leading monomial"
      return coeff(p, length(p))
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
      if degrees_fit_int(a)
        d = @ccall libflint.nmod_mpoly_degree_si(a::Ref{($etype)}, (i - 1)::Int, parent(a)::Ref{($rtype)})::Int
      else
        return Int(degree_fmpz(a, i))
      end
    end

    # Degree in the i-th variable as an ZZRingElem
    function degree_fmpz(a::($etype), i::Int)
      n = nvars(parent(a))
      (i <= 0 || i > n) && error("Index must be between 1 and $n")
      d = ZZRingElem()
      @ccall libflint.nmod_mpoly_degree_fmpz(d::Ref{ZZRingElem}, a::Ref{($etype)}, (i - 1)::Int, parent(a)::Ref{($rtype)})::Nothing
      return d
    end

    # Return true if degrees fit into an Int
    function degrees_fit_int(a::($etype))
      return Bool(@ccall libflint.nmod_mpoly_degrees_fit_si(a::Ref{($etype)}, parent(a)::Ref{($rtype)})::Cint)
    end

    # Return an array of the max degrees in each variable
    function degrees(a::($etype))
      if !degrees_fit_int(a)
        throw(OverflowError("degrees of polynomial do not fit into Int"))
      end
      degs = Vector{Int}(undef, nvars(parent(a)))
      @ccall libflint.nmod_mpoly_degrees_si(degs::Ptr{Int}, a::Ref{($etype)}, parent(a)::Ref{($rtype)})::Nothing
      return degs
    end

    # Return an array of the max degrees as fmpzs in each variable
    function degrees_fmpz(a::($etype))
      n = nvars(parent(a))
      degs = Vector{ZZRingElem}(undef, n)
      for i in 1:n
        degs[i] = ZZRingElem()
      end
      @ccall libflint.nmod_mpoly_degrees_fmpz(degs::Ptr{Ref{ZZRingElem}}, a::Ref{($etype)}, parent(a)::Ref{($rtype)})::Nothing
      return degs
    end

    # Return true if degree fits into an Int
    function total_degree_fits_int(a::($etype))
      return Bool(@ccall libflint.nmod_mpoly_total_degree_fits_si(a::Ref{($etype)}, parent(a)::Ref{($rtype)})::Cint)
    end

    # Total degree as an Int
    function total_degree(a::($etype))
      if !total_degree_fits_int(a)
        throw(OverflowError("Total degree of polynomial does not fit into Int"))
      end
      d = @ccall libflint.nmod_mpoly_total_degree_si(a::Ref{($etype)}, a.parent::Ref{($rtype)})::Int
      return d
    end

    # Total degree as an ZZRingElem
    function total_degree_fmpz(a::($etype))
      d = ZZRingElem()
      @ccall libflint.nmod_mpoly_total_degree_fmpz(d::Ref{ZZRingElem}, a::Ref{($etype)}, parent(a)::Ref{($rtype)})::Nothing
      return d
    end

    ###############################################################################
    #
    #   Multivariable coefficient polynomials
    #
    ###############################################################################

    function coeff(a::($etype), vars::Vector{Int}, exps::Vector{Int})
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
      @ccall libflint.nmod_mpoly_get_coeff_vars_ui(z::Ref{($etype)}, a::Ref{($etype)}, vars::Ptr{Int}, exps::Ptr{Int}, length(vars)::Int, parent(a)::Ref{($rtype)})::Nothing
      return z
    end

    ###############################################################################
    #
    #   Basic arithmetic
    #
    ###############################################################################

    -(a::($etype)) = neg!(parent(a)(), a)

    function +(a::($etype), b::($etype))
      check_parent(a, b)
      z = parent(a)()
      @ccall libflint.nmod_mpoly_add(z::Ref{($etype)}, a::Ref{($etype)}, b::Ref{($etype)}, parent(a)::Ref{($rtype)})::Nothing
      return z
    end

    function -(a::($etype), b::($etype))
      check_parent(a, b)
      z = parent(a)()
      @ccall libflint.nmod_mpoly_sub(z::Ref{($etype)}, a::Ref{($etype)}, b::Ref{($etype)}, parent(a)::Ref{($rtype)})::Nothing
      return z
    end

    function *(a::($etype), b::($etype))
      check_parent(a, b)
      z = parent(a)()
      @ccall libflint.nmod_mpoly_mul(z::Ref{($etype)}, a::Ref{($etype)}, b::Ref{($etype)}, parent(a)::Ref{($rtype)})::Nothing
      return z
    end

    ###############################################################################
    #
    #   Ad hoc arithmetic
    #
    ###############################################################################

    function +(a::($etype), b::UInt)
      z = parent(a)()
      @ccall libflint.nmod_mpoly_add_ui(z::Ref{($etype)}, a::Ref{($etype)}, b::UInt, parent(a)::Ref{($rtype)})::Nothing
      return z
    end

    +(b::UInt, a::($etype)) = a + b

    +(a::($etype), b::($ctype)) = a + b.data

    +(b::($ctype), a::($etype)) = a + b.data

    +(a::($etype), b::Integer) = a + base_ring(parent(a))(b)

    +(a::Integer, b::($etype)) = b + a

    function -(a::($etype), b::UInt)
      z = parent(a)()
      @ccall libflint.nmod_mpoly_sub_ui(z::Ref{($etype)}, a::Ref{($etype)}, b::UInt, parent(a)::Ref{($rtype)})::Nothing
      return z
    end

    function -(b::UInt, a::($etype))
      z = parent(a)()
      @ccall libflint.nmod_mpoly_sub_ui(z::Ref{($etype)}, a::Ref{($etype)}, b::UInt, parent(a)::Ref{($rtype)})::Nothing
      @ccall libflint.nmod_mpoly_neg(z::Ref{($etype)}, z::Ref{($etype)}, parent(a)::Ref{($rtype)})::Nothing
      return z
    end

    -(a::($etype), b::($ctype)) = a - b.data

    -(b::($ctype), a::($etype)) = b.data - a

    -(a::($etype), b::Integer) = a - base_ring(parent(a))(b)

    -(a::Integer, b::($etype)) = base_ring(parent(b))(a) - b

    function *(a::($etype), b::UInt)
      z = parent(a)()
      @ccall libflint.nmod_mpoly_scalar_mul_ui(z::Ref{($etype)}, a::Ref{($etype)}, b::UInt, parent(a)::Ref{($rtype)})::Nothing
      return z
    end

    *(b::UInt, a::($etype)) = a * b

    *(a::($etype), b::($ctype)) = a * b.data

    *(b::($ctype), a::($etype)) = a * b.data

    *(a::($etype), b::Integer) = a * base_ring(parent(a))(b)

    ###############################################################################
    #
    #   Powering
    #
    ###############################################################################

    function ^(a::($etype), b::Int)
      b < 0 && throw(DomainError(b, "Exponent must be non-negative"))
      z = parent(a)()
      @ccall libflint.nmod_mpoly_pow_ui(z::Ref{($etype)}, a::Ref{($etype)}, UInt(b)::UInt, parent(a)::Ref{($rtype)})::Nothing
      return z
    end

    function ^(a::($etype), b::ZZRingElem)
      b < 0 && throw(DomainError(b, "Exponent must be non-negative"))
      z = parent(a)()
      ok = @ccall libflint.nmod_mpoly_pow_fmpz(z::Ref{($etype)}, a::Ref{($etype)}, b::Ref{ZZRingElem}, parent(a)::Ref{($rtype)})::Cint
      !isone(ok) && error("Unable to compute power")
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
      r = @ccall libflint.nmod_mpoly_gcd(z::Ref{($etype)}, a::Ref{($etype)}, b::Ref{($etype)}, a.parent::Ref{($rtype)})::Cint
      r == 0 && error("Unable to compute gcd")
      return z
    end

    function gcd_with_cofactors(a::($etype), b::($etype))
      check_parent(a, b)
      z = parent(a)()
      abar = parent(a)()
      bbar = parent(a)()
      r = @ccall libflint.nmod_mpoly_gcd_cofactors(z::Ref{($etype)}, abar::Ref{($etype)}, bbar::Ref{($etype)}, a::Ref{($etype)}, b::Ref{($etype)}, a.parent::Ref{($rtype)})::Cint
      r == 0 && error("Unable to compute gcd")
      return z, abar, bbar
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
          @ccall libflint.nmod_mpoly_factor_get_base(f::Ref{($etype)}, fac::Ref{($ftype)}, i::Int, R::Ref{($rtype)})::Nothing
        else
          @ccall libflint.nmod_mpoly_factor_swap_base(f::Ref{($etype)}, fac::Ref{($ftype)}, i::Int, R::Ref{($rtype)})::Nothing
        end
        F.fac[f] = @ccall libflint.nmod_mpoly_factor_get_exp_si(fac::Ref{($ftype)}, i::Int, R::Ref{($rtype)})::Int
      end
      c = @ccall libflint.nmod_mpoly_factor_get_constant_ui(fac::Ref{($ftype)})::UInt
      F.unit = R(c)
      return F
    end

    function factor(a::($etype))
      iszero(a) && throw(ArgumentError("Argument must be non-zero"))
      R = parent(a)
      fac = ($ftype)(R)
      ok = @ccall libflint.nmod_mpoly_factor(fac::Ref{($ftype)}, a::Ref{($etype)}, R::Ref{($rtype)})::Cint
      !isone(ok) && error("unable to compute factorization")
      return Fac{($etype)}(fac, false)
    end

    function factor_squarefree(a::($etype))
      iszero(a) && throw(ArgumentError("Argument must be non-zero"))
      R = parent(a)
      fac = ($ftype)(R)
      ok = @ccall libflint.nmod_mpoly_factor_squarefree(fac::Ref{($ftype)}, a::Ref{($etype)}, R::Ref{($rtype)})::Cint
      !isone(ok) && error("unable to compute factorization")
      return Fac{($etype)}(fac, false)
    end


    function sqrt(a::($etype); check::Bool=true)
      (flag, q) = is_square_with_sqrt(a)
      check && !flag && error("Not a square")
      return q
    end

    function is_square(a::($etype))
      return Bool(@ccall libflint.nmod_mpoly_is_square(a::Ref{($etype)}, a.parent::Ref{($rtype)})::Cint)
    end

    function is_square_with_sqrt(a::($etype))
      q = parent(a)()
      flag = @ccall libflint.nmod_mpoly_sqrt(q::Ref{($etype)}, a::Ref{($etype)}, a.parent::Ref{($rtype)})::Cint
      return (Bool(flag), q)
    end

    ###############################################################################
    #
    #   Comparison
    #
    ###############################################################################

    function ==(a::($etype), b::($etype))
      check_parent(a, b)
      return Bool(@ccall libflint.nmod_mpoly_equal(a::Ref{($etype)}, b::Ref{($etype)}, a.parent::Ref{($rtype)})::Cint)
    end

    function Base.isless(a::($etype), b::($etype))
      (!is_monomial(a) || !is_monomial(b)) && error("Not monomials in comparison")
      return (@ccall libflint.nmod_mpoly_cmp(a::Ref{($etype)}, b::Ref{($etype)}, a.parent::Ref{($rtype)})::Cint) < 0
    end

    ###############################################################################
    #
    #   Ad hoc comparison
    #
    ###############################################################################

    function ==(a::($etype), b::($ctype))
      return Bool(@ccall libflint.nmod_mpoly_equal_ui(a::Ref{($etype)}, b.data::UInt, a.parent::Ref{($rtype)})::Cint)
    end

    ==(a::($ctype), b::($etype)) = b == a

    function ==(a::($etype), b::UInt)
      return Bool(@ccall libflint.nmod_mpoly_equal_ui(a::Ref{($etype)}, b::UInt, parent(a)::Ref{($rtype)})::Cint)
    end

    ==(a::UInt, b::($etype)) = b == a

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
      d = @ccall libflint.nmod_mpoly_divides(z::Ref{($etype)}, a::Ref{($etype)}, b::Ref{($etype)}, parent(a)::Ref{($rtype)})::Cint
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
      @ccall libflint.nmod_mpoly_div(q::Ref{($etype)}, a::Ref{($etype)}, b::Ref{($etype)}, parent(a)::Ref{($rtype)})::Nothing
      return q
    end

    function Base.divrem(a::($etype), b::($etype))
      check_parent(a, b)
      q = parent(a)()
      r = parent(a)()
      @ccall libflint.nmod_mpoly_divrem(q::Ref{($etype)}, r::Ref{($etype)}, a::Ref{($etype)}, b::Ref{($etype)}, parent(a)::Ref{($rtype)})::Nothing
      return q, r
    end

    function Base.divrem(a::($etype), b::Vector{($etype)})
      len = length(b)
      q = [parent(a)() for i in 1:len]
      r = parent(a)()
      @ccall libflint.nmod_mpoly_divrem_ideal(q::Ptr{Ref{($etype)}}, r::Ref{($etype)}, a::Ref{($etype)}, b::Ptr{Ref{($etype)}}, len::Int, parent(a)::Ref{($rtype)})::Nothing
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
      @ccall libflint.nmod_mpoly_derivative(z::Ref{($etype)}, a::Ref{($etype)}, (i - 1)::Int, parent(a)::Ref{($rtype)})::Nothing
      return z
    end

    ###############################################################################
    #
    #   Evaluation
    #
    ###############################################################################

    function evaluate(a::($etype), b::Vector{$ctype})
      length(b) != nvars(parent(a)) && error("Vector size incorrect in evaluate")
      b2 = [d.data for d in b]
      z = @ccall libflint.nmod_mpoly_evaluate_all_ui(a::Ref{($etype)}, b2::Ptr{UInt}, parent(a)::Ref{($rtype)})::UInt
      return base_ring(parent(a))(z)
    end

    function evaluate(a::($etype), b::Vector{Int})
      length(b) != nvars(parent(a)) && error("Vector size incorrect in evaluate")
      R = base_ring(parent(a))
      b2 = [R(d) for d in b]
      return evaluate(a, b2)
    end

    function evaluate(a::($etype), b::Vector{T}) where T <: Integer
      length(b) != nvars(parent(a)) && error("Vector size incorrect in evaluate")
      R = base_ring(parent(a))
      b2 = [R(d) for d in b]
      return evaluate(a, b2)
    end

    function evaluate(a::($etype), b::Vector{ZZRingElem})
      length(b) != nvars(parent(a)) && error("Vector size incorrect in evaluate")
      R = base_ring(parent(a))
      b2 = [R(d) for d in b]
      return evaluate(a, b2)
    end

    function evaluate(a::($etype), b::Vector{UInt})
      length(b) != nvars(parent(a)) && error("Vector size incorrect in evaluate")
      R = base_ring(parent(a))
      b2 = [R(d) for d in b]
      return evaluate(a, b2)
    end

    function (a::($etype))()
      error("need at least one value")
    end

    function (a::($etype))(vals::zzModRingElem...)
      length(vals) != nvars(parent(a)) && error("Number of variables does not match number of values")
      return evaluate(a, [vals...])
    end

    function (a::($etype))(vals::Integer...)
      length(vals) != nvars(parent(a)) && error("Number of variables does not match number of values")
      return evaluate(a, [vals...])
    end

    function (a::($etype))(vals::Union{NCRingElem, RingElement}...)
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

    function evaluate(a::$(etype), bs::Vector{$etype})
      R = parent(a)
      S = parent(bs[1])
      @assert base_ring(R) === base_ring(S)

      length(bs) != nvars(R) &&
      error("Number of variables does not match number of values")

      c = S()
      fl = @ccall libflint.nmod_mpoly_compose_nmod_mpoly(c::Ref{$etype}, a::Ref{$etype}, bs::Ptr{Ref{$etype}}, R::Ref{$rtype}, S::Ref{$rtype})::Cint
      fl == 0 && error("Something wrong in evaluation.")
      return c
    end


    function evaluate(a::($etype), bs::Vector{$utype})
      R = parent(a)
      S = parent(bs[1])
      @assert base_ring(R) === base_ring(S)

      length(bs) != nvars(R) &&
      error("Number of variables does not match number of values")

      c = S()
      fl = @ccall libflint.nmod_mpoly_compose_nmod_poly(c::Ref{$utype}, a::Ref{$etype}, bs::Ptr{Ref{$utype}}, R::Ref{$rtype})::Cint
      fl == 0 && error("Something wrong in evaluation.")
      return c
    end

    ###############################################################################
    #
    #   Unsafe functions
    #
    ###############################################################################

    function zero!(a::($etype))
      @ccall libflint.nmod_mpoly_zero(a::Ref{($etype)}, parent(a)::Ref{($rtype)})::Nothing
      return a
    end

    function one!(a::($etype))
      @ccall libflint.nmod_mpoly_one(a::Ref{($etype)}, parent(a)::Ref{($rtype)})::Nothing
      return a
    end

    function neg!(z::($etype), a::($etype))
      @ccall libflint.nmod_mpoly_neg(z::Ref{($etype)}, a::Ref{($etype)}, parent(a)::Ref{($rtype)})::Nothing
      return z
    end

    function add!(a::($etype), b::($etype), c::($etype))
      @ccall libflint.nmod_mpoly_add(a::Ref{($etype)}, b::Ref{($etype)}, c::Ref{($etype)}, parent(a)::Ref{($rtype)})::Nothing
      return a
    end

    function mul!(a::($etype), b::($etype), c::($etype))
      @ccall libflint.nmod_mpoly_mul(a::Ref{($etype)}, b::Ref{($etype)}, c::Ref{($etype)}, parent(a)::Ref{($rtype)})::Nothing
      return a
    end

    # Set the n-th coefficient of a to c. If zero coefficients are inserted, they
    # must be removed with combine_like_terms!
    function setcoeff!(a::($etype), n::Int, c::($ctype))
      if n > length(a)
        @ccall libflint.nmod_mpoly_resize(a::Ref{($etype)}, n::Int, a.parent::Ref{($rtype)})::Nothing
      end
      @ccall libflint.nmod_mpoly_set_term_coeff_ui(a::Ref{($etype)}, (n - 1)::Int, c.data::UInt, a.parent::Ref{($rtype)})::Nothing
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
      @ccall libflint.nmod_mpoly_combine_like_terms(a::Ref{($etype)}, a.parent::Ref{($rtype)})::Nothing
      return a
    end

    ###############################################################################
    #
    #   Manipulating terms and monomials
    #
    ###############################################################################

    function exponent_vector_fits(::Type{Int}, a::($etype), i::Int)
      b = @ccall libflint.nmod_mpoly_term_exp_fits_si(a::Ref{($etype)}, (i - 1)::Int, parent(a)::Ref{($rtype)})::Cint
      return Bool(b)
    end

    function exponent_vector_fits(::Type{UInt}, a::($etype), i::Int)
      b = @ccall libflint.nmod_mpoly_term_exp_fits_ui(a::Ref{($etype)}, (i - 1)::Int, parent(a)::Ref{($rtype)})::Cint
      return Bool(b)
    end

    function exponent_vector!(z::Vector{Int}, a::($etype), i::Int)
      @ccall libflint.nmod_mpoly_get_term_exp_si(z::Ptr{Int}, a::Ref{($etype)}, (i - 1)::Int, parent(a)::Ref{($rtype)})::Nothing
      return z
    end

    function exponent_vector!(z::Vector{UInt}, a::($etype), i::Int)
      @ccall libflint.nmod_mpoly_get_term_exp_ui(z::Ptr{UInt}, a::Ref{($etype)}, (i - 1)::Int, parent(a)::Ref{($rtype)})::Nothing
      return z
    end

    function exponent_vector!(z::Vector{ZZRingElem}, a::($etype), i::Int)
      @ccall libflint.nmod_mpoly_get_term_exp_fmpz(z::Ptr{Ref{ZZRingElem}}, a::Ref{($etype)}, (i - 1)::Int, parent(a)::Ref{($rtype)})::Nothing
      return z
    end

    # Return a generator for exponent vectors of $a$
    function exponent_vectors_fmpz(a::($etype))
      return (exponent_vector_fmpz(a, i) for i in 1:length(a))
    end

    # Set exponent of n-th term to given vector of UInt's
    # No sort is performed, so this is unsafe. These are promoted to ZZRingElem's if
    # they don't fit into 31/63 bits
    function set_exponent_vector!(a::($etype), n::Int, exps::Vector{UInt})
      if n > length(a)
        @ccall libflint.nmod_mpoly_resize(a::Ref{($etype)}, n::Int, a.parent::Ref{($rtype)})::Nothing
      end
      @ccall libflint.nmod_mpoly_set_term_exp_ui(a::Ref{($etype)}, (n - 1)::Int, exps::Ptr{UInt}, parent(a)::Ref{($rtype)})::Nothing
      return a
    end

    # Set exponent of n-th term to given vector of Int's
    # No sort is performed, so this is unsafe. The Int's must be positive, but
    # no check is performed
    function set_exponent_vector!(a::($etype), n::Int, exps::Vector{Int})
      if n > length(a)
        @ccall libflint.nmod_mpoly_resize(a::Ref{($etype)}, n::Int, parent(a)::Ref{($rtype)})::Nothing
      end
      @ccall libflint.nmod_mpoly_set_term_exp_ui(a::Ref{($etype)}, (n - 1)::Int, exps::Ptr{Int}, parent(a)::Ref{($rtype)})::Nothing
      return a
    end

    # Set exponent of n-th term to given vector of ZZRingElem's
    # No sort is performed, so this is unsafe
    function set_exponent_vector!(a::($etype), n::Int, exps::Vector{ZZRingElem})
      if n > length(a)
        @ccall libflint.nmod_mpoly_resize(a::Ref{($etype)}, n::Int, parent(a)::Ref{($rtype)})::Nothing
      end
      @ccall libflint.nmod_mpoly_set_term_exp_fmpz(a::Ref{($etype)}, (n - 1)::Int, exps::Ptr{ZZRingElem}, parent(a)::Ref{($rtype)})::Nothing
      return a
    end

    # Return j-th coordinate of i-th exponent vector
    function exponent(a::($etype), i::Int, j::Int)
      (j < 1 || j > nvars(parent(a))) && error("Invalid variable index")
      return @ccall libflint.nmod_mpoly_get_term_var_exp_ui(a::Ref{($etype)}, (i - 1)::Int, (j - 1)::Int, parent(a)::Ref{($rtype)})::Int
    end

    # Return the coefficient of the term with the given exponent vector
    # Return zero if there is no such term
    function coeff(a::($etype), exps::Vector{UInt})
      z = @ccall libflint.nmod_mpoly_get_coeff_ui_ui(a::Ref{($etype)}, exps::Ptr{UInt}, parent(a)::Ref{($rtype)})::UInt
      return base_ring(parent(a))(z)
    end

    # Return the coefficient of the term with the given exponent vector
    # Return zero if there is no such term
    function coeff(a::($etype), exps::Vector{Int})
      z = @ccall libflint.nmod_mpoly_get_coeff_ui_ui(a::Ref{($etype)}, exps::Ptr{Int}, parent(a)::Ref{($rtype)})::UInt
      return base_ring(parent(a))(z)
    end

    # Set the coefficient of the term with the given exponent vector to the
    # given ZZRingElem. Removal of a zero term is performed.
    function setcoeff!(a::($etype), exps::Vector{UInt}, b::($ctype))
      @ccall libflint.nmod_mpoly_set_coeff_ui_ui(a::Ref{($etype)}, b.data::UInt, exps::Ptr{UInt}, parent(a)::Ref{($rtype)})::Nothing
      return a
    end

    # Set the coefficient of the term with the given exponent vector to the
    # given ZZRingElem. Removal of a zero term is performed.
    function setcoeff!(a::($etype), exps::Vector{Int}, b::($ctype))
      @ccall libflint.nmod_mpoly_set_coeff_ui_ui(a::Ref{($etype)}, b.data::UInt, exps::Ptr{Int}, parent(a)::Ref{($rtype)})::Nothing
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
      @ccall libflint.nmod_mpoly_sort_terms(a::Ref{($etype)}, parent(a)::Ref{($rtype)})::Nothing
      return a
    end

    # Return the i-th term of the polynomial, as a polynomial
    function term(a::($etype), i::Int)
      z = parent(a)()
      @ccall libflint.nmod_mpoly_get_term(z::Ref{($etype)}, a::Ref{($etype)}, (i - 1)::Int, parent(a)::Ref{($rtype)})::Nothing
      return z
    end

    # Return the i-th monomial of the polynomial, as a polynomial
    function monomial(a::($etype), i::Int)
      z = parent(a)()
      @ccall libflint.nmod_mpoly_get_term_monomial(z::Ref{($etype)}, a::Ref{($etype)}, (i - 1)::Int, a.parent::Ref{($rtype)})::Nothing
      return z
    end

    # Sets the given polynomial m to the i-th monomial of the polynomial
    function monomial!(m::($etype), a::($etype), i::Int)
      @ccall libflint.nmod_mpoly_get_term_monomial(m::Ref{($etype)}, a::Ref{($etype)}, (i - 1)::Int, a.parent::Ref{($rtype)})::Nothing
      return m
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
    #   Build context
    #
    ###############################################################################

    function _push_term!(z::($etype), c::($ctype), exp::Vector{Int})
      @ccall libflint.nmod_mpoly_push_term_ui_ui(z::Ref{$etype}, c.data::UInt, exp::Ptr{UInt}, parent(z)::Ref{$rtype})::Nothing
      return z
    end

    function push_term!(M::MPolyBuildCtx{$etype}, c::($ctype), expv::Vector{Int})
      if length(expv) != nvars(parent(M.poly))
        error("length of exponent vector should match the number of variables")
      end
      parent(c) !== base_ring(M.poly) &&error("parent mismatch")
      _push_term!(M.poly, c, expv)
      return M
    end

    function push_term!(M::MPolyBuildCtx{$etype},
        c::RingElement, expv::Vector{Int})
      push_term!(M, base_ring(M.poly)(c), expv)
      return M
    end

    function finish(M::MPolyBuildCtx{$etype})
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

    function (R::($rtype))()
      z = ($etype)(R)
      return z
    end

    function (R::($rtype))(b::($ctype))
      z = ($etype)(R, b)
      return z
    end

    function (R::($rtype))(b::Int)
      z = ($etype)(R, base_ring(R)(b))
      return z
    end

    function (R::($rtype))(b::UInt)
      z = ($etype)(R, b)
      return z
    end

    function (R::($rtype))(b::Integer)
      return R(base_ring(R)(b))
    end

    function (R::($rtype))(b::ZZRingElem)
      return R(base_ring(R)(b))
    end

    function (R::($rtype))(a::($etype))
      parent(a) != R && error("Unable to coerce polynomial")
      return a
    end

    # Create poly with given array of coefficients and array of exponent vectors (sorting is performed)
    function (R::($rtype))(a::Vector{($ctype)}, b::Vector{Vector{T}}) where {T <: Union{ZZRingElem, UInt}}
      length(a) != length(b) && error("Coefficient and exponent vector must have the same length")
      for i in 1:length(b)
        length(b[i]) != nvars(R) && error("Exponent vector $i has length $(length(b[i])) (expected $(nvars(R))")
      end
      z = ($etype)(R, a, b)
      return z
    end

    # Create poly with given array of coefficients and array of exponent vectors (sorting is performed)
    function (R::($rtype))(a::Vector{($ctype)}, b::Vector{Vector{Int}})
      length(a) != length(b) && error("Coefficient and exponent vector must have the same length")
      for i in 1:length(b)
        length(b[i]) != nvars(R) && error("Exponent vector $i has length $(length(b[i])) (expected $(nvars(R)))")
      end
      z = ($etype)(R, a, b)
      return z
    end

    # Create poly with given array of coefficients and array of exponent vectors (sorting is performed)
    function (R::($rtype))(a::Vector, b::Vector{Vector{T}}) where T
      n = nvars(R)
      length(a) != length(b) && error("Coefficient and exponent vector must have the same length")
      newa = map(base_ring(R), a)
      newb = map(x -> map(ZZ, x), b)
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

function divexact(f::fpMPolyRingElem, a::fpFieldElem; check::Bool=true)
  ainv = inv(a)
  return ainv * f
end

function divexact(f::fpMPolyRingElem, a::IntegerUnion; check::Bool=true)
  return divexact(f, base_ring(f)(a))
end

function divexact(f::zzModMPolyRingElem, a::zzModRingElem; check::Bool=true)
  return divexact(f, parent(f)(a))
end

function divexact(f::zzModMPolyRingElem, a::IntegerUnion; check::Bool=true)
  return divexact(f, base_ring(f)(a))
end
