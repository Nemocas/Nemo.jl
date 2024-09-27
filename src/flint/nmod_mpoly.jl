###############################################################################
#
#   nmod_mpoly.jl : Flint multivariate polynomials over nmod and GaloisField
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
      #   b = ccall((:nmod_mpoly_ctx_ord, libflint), Cint, (Ref{zzModMPolyRing}, ), a)
      return flint_orderings[b + 1]
    end

    function gens(R::($rtype))
      A = Vector{($etype)}(undef, R.nvars)
      for i = 1:R.nvars
        z = R()
        ccall((:nmod_mpoly_gen, libflint), Nothing,
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
      ccall((:nmod_mpoly_gen, libflint), Nothing,
            (Ref{($etype)}, Int, Ref{($rtype)}),
            z, i - 1, R)
      return z
    end

    function is_gen(a::($etype), i::Int)
      n = nvars(parent(a))
      (i <= 0 || i > n) && error("Index must be between 1 and $n")
      return Bool(ccall((:nmod_mpoly_is_gen, libflint), Cint,
                        (Ref{($etype)}, Int, Ref{($rtype)}),
                        a, i - 1, parent(a)))
    end

    function is_gen(a::($etype))
      return Bool(ccall((:nmod_mpoly_is_gen, libflint), Cint,
                        (Ref{($etype)}, Int, Ref{($rtype)}),
                        a, -1, parent(a)))
    end

    function deepcopy_internal(a::($etype), dict::IdDict)
      z = parent(a)()
      ccall((:nmod_mpoly_set, libflint), Nothing,
            (Ref{($etype)}, Ref{($etype)}, Ref{($rtype)}),
            z, a, parent(a))
      return z
    end

    function length(a::($etype))
      return a.length
      #   return ccall((:nmod_mpoly_length, libflint), Int,
      #                (Ref{T}, Ref{parent_type(T)}),
      #                a, a.parent)
    end

    one(R::($rtype)) = one!(R())

    zero(R::($rtype)) = zero!(R())

    function isone(a::($etype))
      return Bool(ccall((:nmod_mpoly_is_one, libflint), Cint,
                        (Ref{($etype)}, Ref{($rtype)}),
                        a, parent(a)))
    end

    function iszero(a::($etype))
      return Bool(ccall((:nmod_mpoly_is_zero, libflint), Cint,
                        (Ref{($etype)}, Ref{($rtype)}),
                        a, parent(a)))
    end

    function is_monomial(a::($etype))
      return length(a) == 1 && coeff(a, 1) == 1
    end

    function is_term(a::($etype))
      return length(a) == 1
    end

    function is_unit(a::($etype))
      return length(a) == 1 && total_degree(a) == 0 && is_unit(coeff(a, 1))
    end

    function is_constant(a::($etype))
      return Bool(ccall((:nmod_mpoly_is_ui, libflint), Cint,
                        (Ref{($etype)}, Ref{($rtype)}),
                        a, parent(a)))
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
      z = ccall((:nmod_mpoly_get_term_coeff_ui, libflint), UInt,
                (Ref{($etype)}, Int, Ref{($rtype)}),
                a, i - 1, parent(a))
      return base_ring(parent(a))(z)
    end

    function coeff(a::($etype), b::($etype))
      check_parent(a, b)
      !isone(length(b)) && error("Second argument must be a monomial")
      z = ccall((:nmod_mpoly_get_coeff_ui_monomial, libflint), UInt,
                (Ref{($etype)}, Ref{($etype)}, Ref{($rtype)}),
                a, b, parent(a))
      return base_ring(parent(a))(z)
    end

    function trailing_coefficient(p::($etype))
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
    function degree(a::($etype), i::Int)
      n = nvars(parent(a))
      (i <= 0 || i > n) && error("Index must be between 1 and $n")
      if degrees_fit_int(a)
        d = ccall((:nmod_mpoly_degree_si, libflint), Int,
                  (Ref{($etype)}, Int, Ref{($rtype)}),
                  a, i - 1, parent(a))
      else
        return Int(degree_fmpz(a, i))
      end
    end

    # Degree in the i-th variable as an ZZRingElem
    function degree_fmpz(a::($etype), i::Int)
      n = nvars(parent(a))
      (i <= 0 || i > n) && error("Index must be between 1 and $n")
      d = ZZRingElem()
      ccall((:nmod_mpoly_degree_fmpz, libflint), Nothing,
            (Ref{ZZRingElem}, Ref{($etype)}, Int, Ref{($rtype)}),
            d, a, i - 1, parent(a))
      return d
    end

    # Return true if degrees fit into an Int
    function degrees_fit_int(a::($etype))
      return Bool(ccall((:nmod_mpoly_degrees_fit_si, libflint), Cint,
                        (Ref{($etype)}, Ref{($rtype)}),
                        a, parent(a)))
    end

    # Return an array of the max degrees in each variable
    function degrees(a::($etype))
      if !degrees_fit_int(a)
        throw(OverflowError("degrees of polynomial do not fit into Int"))
      end
      degs = Vector{Int}(undef, nvars(parent(a)))
      ccall((:nmod_mpoly_degrees_si, libflint), Nothing,
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
      ccall((:nmod_mpoly_degrees_fmpz, libflint), Nothing,
            (Ptr{Ref{ZZRingElem}}, Ref{($etype)}, Ref{($rtype)}),
            degs, a, parent(a))
      return degs
    end

    # Return true if degree fits into an Int
    function total_degree_fits_int(a::($etype))
      return Bool(ccall((:nmod_mpoly_total_degree_fits_si, libflint), Cint,
                        (Ref{($etype)}, Ref{($rtype)}),
                        a, parent(a)))
    end

    # Total degree as an Int
    function total_degree(a::($etype))
      if !total_degree_fits_int(a)
        throw(OverflowError("Total degree of polynomial does not fit into Int"))
      end
      d = ccall((:nmod_mpoly_total_degree_si, libflint), Int,
                (Ref{($etype)}, Ref{($rtype)}),
                a, a.parent)
      return d
    end

    # Total degree as an ZZRingElem
    function total_degree_fmpz(a::($etype))
      d = ZZRingElem()
      ccall((:nmod_mpoly_total_degree_fmpz, libflint), Nothing,
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
            (Ref{($etype)}, Ref{($etype)}, Ptr{Int}, Ptr{Int}, Int, Ref{($rtype)}),
            z, a, vars, exps, length(vars), parent(a))
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
      ccall((:nmod_mpoly_add, libflint), Nothing,
            (Ref{($etype)}, Ref{($etype)}, Ref{($etype)}, Ref{($rtype)}),
            z, a, b, parent(a))
      return z
    end

    function -(a::($etype), b::($etype))
      check_parent(a, b)
      z = parent(a)()
      ccall((:nmod_mpoly_sub, libflint), Nothing,
            (Ref{($etype)}, Ref{($etype)}, Ref{($etype)}, Ref{($rtype)}),
            z, a, b, parent(a))
      return z
    end

    function *(a::($etype), b::($etype))
      check_parent(a, b)
      z = parent(a)()
      ccall((:nmod_mpoly_mul, libflint), Nothing,
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
      ccall((:nmod_mpoly_add_ui, libflint), Nothing,
            (Ref{($etype)}, Ref{($etype)}, UInt, Ref{($rtype)}),
            z, a, b, parent(a))
      return z
    end

    +(b::UInt, a::($etype)) = a + b

    +(a::($etype), b::($ctype)) = a + b.data

    +(b::($ctype), a::($etype)) = a + b.data

    +(a::($etype), b::Integer) = a + base_ring(parent(a))(b)

    +(a::Integer, b::($etype)) = b + a

    function -(a::($etype), b::UInt)
      z = parent(a)()
      ccall((:nmod_mpoly_sub_ui, libflint), Nothing,
            (Ref{($etype)}, Ref{($etype)}, UInt, Ref{($rtype)}),
            z, a, b, parent(a))
      return z
    end

    function -(b::UInt, a::($etype))
      z = parent(a)()
      ccall((:nmod_mpoly_sub_ui, libflint), Nothing,
            (Ref{($etype)}, Ref{($etype)}, UInt, Ref{($rtype)}),
            z, a, b, parent(a))
      ccall((:nmod_mpoly_neg, libflint), Nothing,
            (Ref{($etype)}, Ref{($etype)}, Ref{($rtype)}),
            z, z, parent(a))
      return z
    end

    -(a::($etype), b::($ctype)) = a - b.data

    -(b::($ctype), a::($etype)) = b.data - a

    -(a::($etype), b::Integer) = a - base_ring(parent(a))(b)

    -(a::Integer, b::($etype)) = base_ring(parent(b))(a) - b

    function *(a::($etype), b::UInt)
      z = parent(a)()
      ccall((:nmod_mpoly_scalar_mul_ui, libflint), Nothing,
            (Ref{($etype)}, Ref{($etype)}, UInt, Ref{($rtype)}),
            z, a, b, parent(a))
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
      ccall((:nmod_mpoly_pow_ui, libflint), Nothing,
            (Ref{($etype)}, Ref{($etype)}, UInt, Ref{($rtype)}),
            z, a, UInt(b), parent(a))
      return z
    end

    function ^(a::($etype), b::ZZRingElem)
      b < 0 && throw(DomainError(b, "Exponent must be non-negative"))
      z = parent(a)()
      ok = ccall((:nmod_mpoly_pow_fmpz, libflint), Cint,
                 (Ref{($etype)}, Ref{($etype)}, Ref{ZZRingElem}, Ref{($rtype)}),
                 z, a, b, parent(a))
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
      r = ccall((:nmod_mpoly_gcd, libflint), Cint,
                (Ref{($etype)}, Ref{($etype)}, Ref{($etype)}, Ref{($rtype)}),
                z, a, b, a.parent)
      r == 0 && error("Unable to compute gcd")
      return z
    end

    function gcd_with_cofactors(a::($etype), b::($etype))
      check_parent(a, b)
      z = parent(a)()
      abar = parent(a)()
      bbar = parent(a)()
      r = ccall((:nmod_mpoly_gcd_cofactors, libflint), Cint,
                (Ref{($etype)}, Ref{($etype)}, Ref{($etype)},
                 Ref{($etype)}, Ref{($etype)}, Ref{($rtype)}),
                z, abar, bbar, a, b, a.parent)
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
          ccall((:nmod_mpoly_factor_get_base, libflint), Nothing,
                (Ref{($etype)}, Ref{($ftype)}, Int, Ref{($rtype)}),
                f, fac, i, R)
        else
          ccall((:nmod_mpoly_factor_swap_base, libflint), Nothing,
                (Ref{($etype)}, Ref{($ftype)}, Int, Ref{($rtype)}),
                f, fac, i, R)
        end
        F.fac[f] = ccall((:nmod_mpoly_factor_get_exp_si, libflint), Int,
                         (Ref{($ftype)}, Int, Ref{($rtype)}),
                         fac, i, R)
      end
      c = ccall((:nmod_mpoly_factor_get_constant_ui, libflint), UInt,
                (Ref{($ftype)}, ),
                fac)
      F.unit = R(c)
      return F
    end

    function factor(a::($etype))
      iszero(a) && throw(ArgumentError("Argument must be non-zero"))
      R = parent(a)
      fac = ($ftype)(R)
      ok = ccall((:nmod_mpoly_factor, libflint), Cint,
                 (Ref{($ftype)}, Ref{($etype)}, Ref{($rtype)}),
                 fac, a, R)
      !isone(ok) && error("unable to compute factorization")
      return Fac{($etype)}(fac, false)
    end

    function factor_squarefree(a::($etype))
      iszero(a) && throw(ArgumentError("Argument must be non-zero"))
      R = parent(a)
      fac = ($ftype)(R)
      ok = ccall((:nmod_mpoly_factor_squarefree, libflint), Cint,
                 (Ref{($ftype)}, Ref{($etype)}, Ref{($rtype)}),
                 fac, a, R)
      !isone(ok) && error("unable to compute factorization")
      return Fac{($etype)}(fac, false)
    end


    function sqrt(a::($etype); check::Bool=true)
      (flag, q) = is_square_with_sqrt(a)
      check && !flag && error("Not a square")
      return q
    end

    function is_square(a::($etype))
      return Bool(ccall((:nmod_mpoly_is_square, libflint), Cint,
                        (Ref{($etype)}, Ref{($rtype)}),
                        a, a.parent))
    end

    function is_square_with_sqrt(a::($etype))
      q = parent(a)()
      flag = ccall((:nmod_mpoly_sqrt, libflint), Cint,
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
      return Bool(ccall((:nmod_mpoly_equal, libflint), Cint,
                        (Ref{($etype)}, Ref{($etype)}, Ref{($rtype)}),
                        a, b, a.parent))
    end

    function Base.isless(a::($etype), b::($etype))
      (!is_monomial(a) || !is_monomial(b)) && error("Not monomials in comparison")
      return ccall((:nmod_mpoly_cmp, libflint), Cint,
                   (Ref{($etype)}, Ref{($etype)}, Ref{($rtype)}),
                   a, b, a.parent) < 0
    end

    ###############################################################################
    #
    #   Ad hoc comparison
    #
    ###############################################################################

    function ==(a::($etype), b::($ctype))
      return Bool(ccall((:nmod_mpoly_equal_ui, libflint), Cint,
                        (Ref{($etype)}, UInt, Ref{($rtype)}),
                        a, b.data, a.parent))
    end

    ==(a::($ctype), b::($etype)) = b == a

    function ==(a::($etype), b::UInt)
      return Bool(ccall((:nmod_mpoly_equal_ui, libflint), Cint,
                        (Ref{($etype)}, UInt, Ref{($rtype)}),
                        a, b, parent(a)))
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
      d = ccall((:nmod_mpoly_divides, libflint), Cint,
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
      ccall((:nmod_mpoly_div, libflint), Nothing,
            (Ref{($etype)}, Ref{($etype)}, Ref{($etype)}, Ref{($rtype)}),
            q, a, b, parent(a))
      return q
    end

    function Base.divrem(a::($etype), b::($etype))
      check_parent(a, b)
      q = parent(a)()
      r = parent(a)()
      ccall((:nmod_mpoly_divrem, libflint), Nothing,
            (Ref{($etype)}, Ref{($etype)}, Ref{($etype)},
             Ref{($etype)}, Ref{($rtype)}),
            q, r, a, b, parent(a))
      return q, r
    end

    function Base.divrem(a::($etype), b::Vector{($etype)})
      len = length(b)
      q = [parent(a)() for i in 1:len]
      r = parent(a)()
      ccall((:nmod_mpoly_divrem_ideal, libflint), Nothing,
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
      ccall((:nmod_mpoly_derivative, libflint), Nothing,
            (Ref{($etype)}, Ref{($etype)}, Int, Ref{($rtype)}),
            z, a, i - 1, parent(a))
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
      z = ccall((:nmod_mpoly_evaluate_all_ui, libflint), UInt,
                (Ref{($etype)}, Ptr{UInt}, Ref{($rtype)}),
                a, b2, parent(a))
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
      fl = ccall((:nmod_mpoly_compose_nmod_mpoly, libflint), Cint,
                 (Ref{$etype}, Ref{$etype}, Ptr{Ref{$etype}},
                  Ref{$rtype}, Ref{$rtype}),
                 c, a, bs, R, S)
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
      fl = ccall((:nmod_mpoly_compose_nmod_poly, libflint), Cint,
                 (Ref{$utype}, Ref{$etype}, Ptr{Ref{$utype}},
                  Ref{$rtype}), c, a, bs, R)
      fl == 0 && error("Something wrong in evaluation.")
      return c
    end

    ###############################################################################
    #
    #   Unsafe functions
    #
    ###############################################################################

    function zero!(a::($etype))
      ccall((:nmod_mpoly_zero, libflint), Nothing,
            (Ref{($etype)}, Ref{($rtype)}),
            a, parent(a))
      return a
    end

    function one!(a::($etype))
      ccall((:nmod_mpoly_one, libflint), Nothing,
            (Ref{($etype)}, Ref{($rtype)}),
            a, parent(a))
      return a
    end

    function neg!(z::($etype), a::($etype))
      ccall((:nmod_mpoly_neg, libflint), Nothing,
            (Ref{($etype)}, Ref{($etype)}, Ref{($rtype)}),
            z, a, parent(a))
      return z
    end

    function add!(a::($etype), b::($etype), c::($etype))
      ccall((:nmod_mpoly_add, libflint), Nothing,
            (Ref{($etype)}, Ref{($etype)}, Ref{($etype)}, Ref{($rtype)}),
            a, b, c, parent(a))
      return a
    end

    function mul!(a::($etype), b::($etype), c::($etype))
      ccall((:nmod_mpoly_mul, libflint), Nothing,
            (Ref{($etype)}, Ref{($etype)}, Ref{($etype)}, Ref{($rtype)}),
            a, b, c, parent(a))
      return a
    end

    # Set the n-th coefficient of a to c. If zero coefficients are inserted, they
    # must be removed with combine_like_terms!
    function setcoeff!(a::($etype), n::Int, c::($ctype))
      if n > length(a)
        ccall((:nmod_mpoly_resize, libflint), Nothing,
              (Ref{($etype)}, Int, Ref{($rtype)}),
              a, n, a.parent)
      end
      ccall((:nmod_mpoly_set_term_coeff_ui, libflint), Nothing,
            (Ref{($etype)}, Int, UInt, Ref{($rtype)}),
            a, n - 1, c.data, a.parent)
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
      ccall((:nmod_mpoly_combine_like_terms, libflint), Nothing,
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
      b = ccall((:nmod_mpoly_term_exp_fits_si, libflint), Cint,
                (Ref{($etype)}, Int, Ref{($rtype)}),
                a, i - 1, parent(a))
      return Bool(b)
    end

    function exponent_vector_fits(::Type{UInt}, a::($etype), i::Int)
      b = ccall((:nmod_mpoly_term_exp_fits_ui, libflint), Cint,
                (Ref{($etype)}, Int, Ref{($rtype)}),
                a, i - 1, parent(a))
      return Bool(b)
    end

    function exponent_vector!(z::Vector{Int}, a::($etype), i::Int)
      ccall((:nmod_mpoly_get_term_exp_si, libflint), Nothing,
            (Ptr{Int}, Ref{($etype)}, Int, Ref{($rtype)}),
            z, a, i - 1, parent(a))
      return z
    end

    function exponent_vector!(z::Vector{UInt}, a::($etype), i::Int)
      ccall((:nmod_mpoly_get_term_exp_ui, libflint), Nothing,
            (Ptr{UInt}, Ref{($etype)}, Int, Ref{($rtype)}),
            z, a, i - 1, parent(a))
      return z
    end

    function exponent_vector!(z::Vector{ZZRingElem}, a::($etype), i::Int)
      ccall((:nmod_mpoly_get_term_exp_fmpz, libflint), Nothing,
            (Ptr{Ref{ZZRingElem}}, Ref{($etype)}, Int, Ref{($rtype)}),
            z, a, i - 1, parent(a))
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
        ccall((:nmod_mpoly_resize, libflint), Nothing,
              (Ref{($etype)}, Int, Ref{($rtype)}), a, n, a.parent)
      end
      ccall((:nmod_mpoly_set_term_exp_ui, libflint), Nothing,
            (Ref{($etype)}, Int, Ptr{UInt}, Ref{($rtype)}),
            a, n - 1, exps, parent(a))
      return a
    end

    # Set exponent of n-th term to given vector of Int's
    # No sort is performed, so this is unsafe. The Int's must be positive, but
    # no check is performed
    function set_exponent_vector!(a::($etype), n::Int, exps::Vector{Int})
      if n > length(a)
        ccall((:nmod_mpoly_resize, libflint), Nothing,
              (Ref{($etype)}, Int, Ref{($rtype)}),
              a, n, parent(a))
      end
      ccall((:nmod_mpoly_set_term_exp_ui, libflint), Nothing,
            (Ref{($etype)}, Int, Ptr{Int}, Ref{($rtype)}),
            a, n - 1, exps, parent(a))
      return a
    end

    # Set exponent of n-th term to given vector of ZZRingElem's
    # No sort is performed, so this is unsafe
    function set_exponent_vector!(a::($etype), n::Int, exps::Vector{ZZRingElem})
      if n > length(a)
        ccall((:nmod_mpoly_resize, libflint), Nothing,
              (Ref{($etype)}, Int, Ref{($rtype)}),
              a, n, parent(a))
      end
      ccall((:nmod_mpoly_set_term_exp_fmpz, libflint), Nothing,
            (Ref{($etype)}, Int, Ptr{ZZRingElem}, Ref{($rtype)}),
            a, n - 1, exps, parent(a))
      return a
    end

    # Return j-th coordinate of i-th exponent vector
    function exponent(a::($etype), i::Int, j::Int)
      (j < 1 || j > nvars(parent(a))) && error("Invalid variable index")
      return ccall((:nmod_mpoly_get_term_var_exp_ui, libflint), Int,
                   (Ref{($etype)}, Int, Int, Ref{($rtype)}),
                   a, i - 1, j - 1, parent(a))
    end

    # Return the coefficient of the term with the given exponent vector
    # Return zero if there is no such term
    function coeff(a::($etype), exps::Vector{UInt})
      z = ccall((:nmod_mpoly_get_coeff_ui_ui, libflint), UInt,
                (Ref{($etype)}, Ptr{UInt}, Ref{($rtype)}),
                a, exps, parent(a))
      return base_ring(parent(a))(z)
    end

    # Return the coefficient of the term with the given exponent vector
    # Return zero if there is no such term
    function coeff(a::($etype), exps::Vector{Int})
      z = ccall((:nmod_mpoly_get_coeff_ui_ui, libflint), UInt,
                (Ref{($etype)}, Ptr{Int}, Ref{($rtype)}),
                a, exps, parent(a))
      return base_ring(parent(a))(z)
    end

    # Set the coefficient of the term with the given exponent vector to the
    # given ZZRingElem. Removal of a zero term is performed.
    function setcoeff!(a::($etype), exps::Vector{UInt}, b::($ctype))
      ccall((:nmod_mpoly_set_coeff_ui_ui, libflint), Nothing,
            (Ref{($etype)}, UInt, Ptr{UInt}, Ref{($rtype)}),
            a, b.data, exps, parent(a))
      return a
    end

    # Set the coefficient of the term with the given exponent vector to the
    # given ZZRingElem. Removal of a zero term is performed.
    function setcoeff!(a::($etype), exps::Vector{Int}, b::($ctype))
      ccall((:nmod_mpoly_set_coeff_ui_ui, libflint), Nothing,
            (Ref{($etype)}, UInt, Ptr{Int}, Ref{($rtype)}),
            a, b.data, exps, parent(a))
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
      ccall((:nmod_mpoly_sort_terms, libflint), Nothing,
            (Ref{($etype)}, Ref{($rtype)}),
            a, parent(a))
      return a
    end

    # Return the i-th term of the polynomial, as a polynomial
    function term(a::($etype), i::Int)
      z = parent(a)()
      ccall((:nmod_mpoly_get_term, libflint), Nothing,
            (Ref{($etype)}, Ref{($etype)}, Int, Ref{($rtype)}),
            z, a, i - 1, parent(a))
      return z
    end

    # Return the i-th monomial of the polynomial, as a polynomial
    function monomial(a::($etype), i::Int)
      z = parent(a)()
      ccall((:nmod_mpoly_get_term_monomial, libflint), Nothing,
            (Ref{($etype)}, Ref{($etype)}, Int, Ref{($rtype)}),
            z, a, i - 1, a.parent)
      return z
    end

    # Sets the given polynomial m to the i-th monomial of the polynomial
    function monomial!(m::($etype), a::($etype), i::Int)
      ccall((:nmod_mpoly_get_term_monomial, libflint), Nothing,
            (Ref{($etype)}, Ref{($etype)}, Int, Ref{($rtype)}),
            m, a, i - 1, a.parent)
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
      ccall((:nmod_mpoly_push_term_ui_ui, libflint), Nothing,
            (Ref{$etype}, UInt, Ptr{UInt}, Ref{$rtype}),
            z, c.data, exp, parent(z))
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
