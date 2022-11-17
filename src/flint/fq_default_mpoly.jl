

export FqDefaultMPolyRing, fq_default_mpoly

###############################################################################
#
#   Data type and parent object methods
#
###############################################################################

parent_type(::Type{fq_default_mpoly}) = FqDefaultMPolyRing

elem_type(::Type{FqDefaultMPolyRing}) = fq_default_mpoly

elem_type(::FqDefaultMPolyRing) = fq_default_mpoly

symbols(a::FqDefaultMPolyRing) = symbols(a.data)

parent(a::fq_default_mpoly) = a.parent

nvars(a::FqDefaultMPolyRing) = nvars(a.data)

base_ring(a::FqDefaultMPolyRing) = a.base_ring

base_ring(f::fq_default_mpoly) = base_ring(parent(f))

characteristic(R::FqDefaultMPolyRing) = characteristic(base_ring(R))

modulus(R::FqDefaultMPolyRing) = modulus(base_ring(R))

modulus(f::fq_default_mpoly) = modulus(base_ring(parent(f)))

function ordering(a::FqDefaultMPolyRing)
    return ordering(a.data)
end

function gens(R::FqDefaultMPolyRing)
    return [fq_default_mpoly(R, a) for a in gens(R.data)]
end

function gen(R::FqDefaultMPolyRing, i::Int)
    return fq_default_mpoly(R, gen(R.data, i))
end

function is_gen(a::fq_default_mpoly)
    return is_gen(a.data)
end

function deepcopy_internal(a::fq_default_mpoly, dict::IdDict)
    return fq_default_mpoly(parent(a), deepcopy_internal(a.data, dict))
end

function length(a::fq_default_mpoly)
   return length(a.data)
end

function one(R::FqDefaultMPolyRing)
    return fq_default_mpoly(R, one(R.data))
end

function zero(R::FqDefaultMPolyRing)
    return fq_default_mpoly(R, zero(R.data))
end

function isone(a::fq_default_mpoly)
    return isone(a.data)
end

function iszero(a::fq_default_mpoly)
    return iszero(a.data)
end

function is_monomial(a::fq_default_mpoly)
    return is_monomial(a.data)
end

function is_term(a::fq_default_mpoly)
    return is_term(a.data)
end

function is_unit(a::fq_default_mpoly)
    return is_unit(a.data)
end

function is_constant(a::fq_default_mpoly)
    return is_constant(a.data)
end

###############################################################################
#
#   AbstractString I/O
#
###############################################################################

function expressify(a::fq_default_mpoly, x = symbols(parent(a)); context = nothing)
    return expressify(a.data, x, context = context)
end

# AA has enable all show via expressify for all MPolys

function show(io::IO, p::FqDefaultMPolyRing)
    local max_vars = 5 # largest number of variables to print
    S = symbols(p)
    n = length(S)
    print(io, "Multivariate Polynomial Ring in ")
    if n == 0 || n > max_vars
        print(io, n)
        print(io, " variables ")
    end
    for i in 1:min(n - 1, max_vars - 1)
        print(io, string(S[i]), ", ")
    end
    if n > max_vars
        print(io, "..., ")
    end
    if n > 0
        print(io, string(S[n]))
    end
    print(io, " over ")
    print(IOContext(io, :compact => true), base_ring(p))
end

################################################################################
#
#  Getting coefficients
#
################################################################################

function coeff(a::fq_default_mpoly, i::Int)
    return _unchecked_coerce(base_ring(a), coeff(a.data, i))
end

function coeff(a::fq_default_mpoly, b::fq_default_mpoly)
    return _unchecked_coerce(base_ring(a), coeff(a.data, b.data))
end

function trailing_coefficient(a::fq_default_mpoly)
    return _unchecked_coerce(base_ring(a), trailing_coefficient(a.data))
end

###############################################################################
#
#   Basic manipulation
#
###############################################################################

function degree(a::fq_default_mpoly, i::Int)
    return degree(a.data, i)
end

function degrees(a::fq_default_mpoly)
    return degrees(a.data)
end

function total_degree(a::fq_default_mpoly)
    return total_degree(a.data)
end

###############################################################################
#
#   Multivariable coefficient polynomials
#
###############################################################################

function coeff(a::fq_default_mpoly, vars::Vector{Int}, exps::Vector{Int})
    return fq_default_mpoly(parent(a), coeff(a.data, vars, exps))
end

###############################################################################
#
#   Basic arithmetic
#
###############################################################################

function -(a::fq_default_mpoly)
    R = parent(a)
    @fq_default_mpoly_do_op(-, R, a)
end

function +(a::fq_default_mpoly, b::fq_default_mpoly)
    check_parent(a, b)
    R = parent(a)
    @fq_default_mpoly_do_op(+, R, a, b)
end

function -(a::fq_default_mpoly, b::fq_default_mpoly)
    check_parent(a, b)
    R = parent(a)
    @fq_default_mpoly_do_op(-, R, a, b)
end

function *(a::fq_default_mpoly, b::fq_default_mpoly)
    check_parent(a, b)
    R = parent(a)
    @fq_default_mpoly_do_op(*, R, a, b)
end

###############################################################################
#
#   Ad hoc arithmetic
#
###############################################################################

function +(a::fq_default_mpoly, b::IntegerUnion)
    return fq_default_mpoly(parent(a), a.data + base_ring(a.data)(b))
end

function +(a::fq_default_mpoly, b::fq_default)
    parent(b) == base_ring(a) || error("Unable to coerce element")
    b1 = _unchecked_coerce(base_ring(a.data), b)
    return fq_default_mpoly(parent(a), a.data + b1)
end

function +(b::Union{fq_default, Integer}, a::fq_default_mpoly)
    return a + b
end

function -(a::fq_default_mpoly, b::IntegerUnion)
    return fq_default_mpoly(parent(a), a.data - base_ring(a.data)(b))
end

function -(a::fq_default_mpoly, b::fq_default)
    parent(b) == base_ring(a) || error("Unable to coerce element")
    b1 = _unchecked_coerce(base_ring(a.data), b)
    return fq_default_mpoly(parent(a), a.data - b1)
end

function -(b::IntegerUnion, a::fq_default_mpoly)
    return fq_default_mpoly(parent(a), base_ring(a.data)(b) - a.data)
end

function -(b::fq_default, a::fq_default_mpoly)
    parent(b) == base_ring(a) || error("Unable to coerce element")
    b1 = _unchecked_coerce(base_ring(a.data), b)
    return fq_default_mpoly(parent(a), b1 - a.data)
end

function *(a::fq_default_mpoly, b::IntegerUnion)
    return fq_default_mpoly(parent(a), a.data * base_ring(a.data)(b))
end

function *(a::fq_default_mpoly, b::fq_default)
    parent(b) == base_ring(a) || error("Unable to coerce element")
    b1 = _unchecked_coerce(base_ring(a.data), b)
    return fq_default_mpoly(parent(a), a.data * b1)
end

function *(b::Union{fq_default, Integer}, a::fq_default_mpoly)
    return a*b
end

###############################################################################
#
#   Powering
#
###############################################################################

function ^(a::fq_default_mpoly, b::Integer)
    return fq_default_mpoly(parent(a), a.data^b)
end

################################################################################
#
#   GCD
#
################################################################################

function gcd(a::fq_default_mpoly, b::fq_default_mpoly)
    check_parent(a, b)
    return fq_default_mpoly(parent(a), gcd(a.data, b.data))
end

function gcd_with_cofactors(a::fq_default_mpoly, b::fq_default_mpoly)
    check_parent(a, b)
    (g, abar, bbar) = gcd_with_cofactors(a.data, b.data)
    R = parent(a)
    return (fq_default_mpoly(R, g), fq_default_mpoly(R, abar), fq_default_mpoly(R, bbar))
end

################################################################################
#
#   Factorization and Square Root
#
################################################################################

function _convert_fac(a::FqDefaultMPolyRing, b::Fac)
    f = Fac{fq_default_mpoly}()
    f.unit = fq_default_mpoly(a, b.unit)
    for (p, e) in b
        f[fq_default_mpoly(a, p)] = e
    end
    return f
end

function factor(a::fq_default_mpoly)
    return _convert_fac(parent(a), factor(a.data))
end

function factor_squarefree(a::fq_default_mpoly)
    return _convert_fac(parent(a), factor_squarefree(a.data))
end

function sqrt(a::fq_default_mpoly; check::Bool=true)
    return fq_default_mpoly(parent(a), sqrt(a.data, check = check))
end

function is_square(a::fq_default_mpoly)
    return is_square(a.data)
end

function is_square_with_sqrt(a::fq_default_mpoly)
    x, y = is_square_with_sqrt(a.data)
    return x, fq_default_mpoly(parent(a), y)
end

###############################################################################
#
#   Comparison
#
###############################################################################

function ==(a::fq_default_mpoly, b::fq_default_mpoly)
    check_parent(a, b)
    return a.data == b.data
end

function Base.isless(a::fq_default_mpoly, b::fq_default_mpoly)
    check_parent(a, b)
    return isless(a.data, b.data)
end

###############################################################################
#
#   Ad hoc comparison
#
###############################################################################

function ==(a::fq_default_mpoly, b::fq_default)
    return a.data == _unchecked_coerce(base_ring(a.data), b)
end

function ==(b::fq_default, a::fq_default_mpoly)
    return a.data == _unchecked_coerce(base_ring(a.data), b)
end

function ==(a::fq_default_mpoly, b::IntegerUnion)
    return a.data == base_ring(a.data)(b)
end

function ==(b::IntegerUnion, a::fq_default_mpoly)
    return a.data == base_ring(a.data)(b)
end

###############################################################################
#
#   Divisibility
#
###############################################################################

function divides(a::fq_default_mpoly, b::fq_default_mpoly)
    check_parent(a, b)
    x, y = divides(a.data, b.data)
    return x, fq_default_mpoly(parent(a), y)
end

###############################################################################
#
#   Division with remainder
#
###############################################################################

function Base.div(a::(fq_default_mpoly), b::(fq_default_mpoly))
    check_parent(a, b)
    return fq_default_mpoly(parent(a), div(a.data, b.data))
end

function Base.divrem(a::fq_default_mpoly, b::fq_default_mpoly)
    check_parent(a, b)
    x, y = divrem(a.data, b.data)
    return fq_default_mpoly(parent(a), x), fq_default_mpoly(parent(a), y)
end

function Base.divrem(a::fq_default_mpoly, b::Vector{fq_default_mpoly})
    for bi in b
        check_parent(a, bi)
    end
    ad = a.data
    bd = typeof(ad)[bi.data for bi in b]
    q, r = Base.divrem(ad, bd)
    return [fq_default_mpoly(parent(a), qi) for qi in q],
           fq_default_mpoly(parent(a), r)
end

###############################################################################
#
#   Exact division
#
###############################################################################

function divexact(a::(fq_default_mpoly), b::(fq_default_mpoly); check::Bool=true)
    check_parent(a, b)
    return fq_default_mpoly(parent(a), divexact(a.data, b.data))
end

###############################################################################
#
#   Calculus
#
###############################################################################

function derivative(a::fq_default_mpoly, i::Int)
    return fq_default_mpoly(parent(a), derivative(a.data, i))
end

###############################################################################
#
#   Evaluation
#
###############################################################################

# TODO have AA define evaluate(a, vals) for general vals
# so we can get rid of this copy pasta
function (a::fq_default_mpoly)(vals::Union{NCRingElem, RingElement}...)
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

function zero!(a::fq_default_mpoly)
    a.data = zero!(a.data)
    return a
end

function add!(a::fq_default_mpoly, b::fq_default_mpoly, c::fq_default_mpoly)
    a.data = add!(a.data, b.data, c.data)
    return a
end

function addeq!(a::fq_default_mpoly, b::fq_default_mpoly)
    a.data = addeq!(a.data, b.data)
    return a
end

function mul!(a::fq_default_mpoly, b::fq_default_mpoly, c::fq_default_mpoly)
    a.data = mul!(a.data, b.data, c.data)
    return a
end

function setcoeff!(a::fq_default_mpoly, n::Int, c::fq_default)
    Rd = parent(a).data
    a.data = setcoeff!(a.data, n, _unchecked_coerce(base_ring(Rd), c))
    return a
end

function combine_like_terms!(a::fq_default_mpoly)
    a.data = combine_like_terms!(a.data)
    return a
end

###############################################################################
#
#   Manipulating terms and monomials
#
###############################################################################

function set_exponent_vector!(a::fq_default_mpoly, n::Int, exps::Vector{T}) where T
    a.data = set_exponent_vector!(a.data, n, exps)
    return a
end

function exponent_vector(a::fq_default_mpoly, i::Int)
    return exponent_vector(a.data, i)
end

function exponent(a::fq_default_mpoly, i::Int, j::Int)
    return exponent(a.data, i, j)
end

function coeff(a::fq_default_mpoly, exps::Vector{T}) where T
    return _unchecked_coerce(base_ring(a), coeff(a.data, exps))
end

function sort_terms!(a::fq_default_mpoly)
    sort_terms!(a.data)
    return a
end

function term(a::fq_default_mpoly, i::Int)
    return fq_default_mpoly(parent(a), term(a.data, i))
end

function monomial(a::(fq_default_mpoly), i::Int)
    return fq_default_mpoly(parent(a), monomial(a.data, i))
end

function monomial!(m::(fq_default_mpoly), a::(fq_default_mpoly), i::Int)
    m.data = monomial!(m.data, a.data, i)
    return m
end

###############################################################################
#
#   Promotion rules
#
###############################################################################

promote_rule(::Type{(fq_default_mpoly)}, ::Type{V}) where {V <: Integer} = (fq_default_mpoly)

promote_rule(::Type{(fq_default_mpoly)}, ::Type{fmpz}) = (fq_default_mpoly)

promote_rule(::Type{(fq_default_mpoly)}, ::Type{fq_default}) = (fq_default_mpoly)

###############################################################################
#
#   Parent object call overload
#
###############################################################################

function (R::FqDefaultMPolyRing)()
    return fq_default_mpoly(R, R.data())
end

function (R::FqDefaultMPolyRing)(b::IntegerUnion)
    return fq_default_mpoly(R, R.data(b))
end

function (R::FqDefaultMPolyRing)(b::fq_default)
    parent(b) == base_ring(R) || error("Unable to coerce element")
    return fq_default_mpoly(R, R.data(_unchecked_coerce(base_ring(R.data), b)))
end


function (R::FqDefaultMPolyRing)(a::fq_default_mpoly)
   parent(a) == R || error("Unable to coerce polynomial")
   return a
end

function (R::FqDefaultMPolyRing)(a::Vector{fq_default}, b::Vector{Vector{Int}})
    F = base_ring(R.data)
    ad = elem_type(F)[if parent(ai) != base_ring(R)
                        error("coefficient is in the wrong field")
                      else
                        _unchecked_coerce(F, ai)
                      end for ai in a]
    return fq_default_mpoly(R, R.data(ad, b))
end

###############################################################################
#
#  Ad hoc exact division
#
###############################################################################

function divexact(a::fq_default_mpoly, b::fq_default; check::Bool=true)
    return a*inv(b)
end

function divexact(a::fq_default_mpoly, b::IntegerUnion; check::Bool=true)
  return a*inv(base_ring(a)(b))
end

###############################################################################
#
#   PolynomialRing constructor
#
###############################################################################

function PolynomialRing(R::FqDefaultFiniteField, s::Vector{Symbol}; cached::Bool = true, ordering::Symbol = :lex)
    # try just fq for now
    m = modulus(R)
    p = characteristic(R)
    if fits(UInt, p)
        Fq = GF(UInt(p))
        if isone(degree(m))
            Fqx = PolynomialRing(Fq, s, cached = cached, ordering = ordering)[1]
            parent_obj = FqDefaultMPolyRing(Fqx, R, 3, cached)
        else
            mm = PolynomialRing(Fq, "x")[1](lift(PolynomialRing(ZZ, "x")[1], m))
            Fq = FlintFiniteField(mm, R.var, cached = cached, check = false)[1]
            Fqx = PolynomialRing(Fq, s, cached = cached, ordering = ordering)[1]
            parent_obj = FqDefaultMPolyRing(Fqx, R, 2, cached)
        end
    else
        Fq = FqFiniteField(m, Symbol(R.var), cached, check = false)
        Fqx = AbstractAlgebra.Generic.PolynomialRing(Fq, s, cached = cached, ordering = ordering)[1]
        parent_obj = FqDefaultMPolyRing(Fqx, R, 1, cached)
    end
    return parent_obj, gens(parent_obj)
end

function PolynomialRing(R::FqDefaultFiniteField, s::Vector{String}; cached::Bool = true, ordering::Symbol = :lex)
   return PolynomialRing(R, [Symbol(x) for x in s]; cached=cached, ordering=ordering)
end

