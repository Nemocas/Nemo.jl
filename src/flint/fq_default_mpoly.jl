###############################################################################
#
#   Data type and parent object methods
#
###############################################################################

parent_type(::Type{FqMPolyRingElem}) = FqMPolyRing

elem_type(::Type{FqMPolyRing}) = FqMPolyRingElem

mpoly_type(::Type{FqFieldElem}) = FqMPolyRingElem

symbols(a::FqMPolyRing) = symbols(a.data)

parent(a::FqMPolyRingElem) = a.parent

nvars(a::FqMPolyRing) = nvars(a.data)

base_ring(a::FqMPolyRing) = a.base_ring

modulus(R::FqMPolyRing) = modulus(base_ring(R))

modulus(f::FqMPolyRingElem) = modulus(base_ring(parent(f)))

function ordering(a::FqMPolyRing)
    return ordering(a.data)
end

function gens(R::FqMPolyRing)
    return [FqMPolyRingElem(R, a) for a in gens(R.data)]
end

function gen(R::FqMPolyRing, i::Int)
    return FqMPolyRingElem(R, gen(R.data, i))
end

function is_gen(a::FqMPolyRingElem)
    return is_gen(a.data)
end

function deepcopy_internal(a::FqMPolyRingElem, dict::IdDict)
    return FqMPolyRingElem(parent(a), deepcopy_internal(a.data, dict))
end

function length(a::FqMPolyRingElem)
   return length(a.data)
end

function one(R::FqMPolyRing)
    return FqMPolyRingElem(R, one(R.data))
end

function zero(R::FqMPolyRing)
    return FqMPolyRingElem(R, zero(R.data))
end

function isone(a::FqMPolyRingElem)
    return isone(a.data)
end

function iszero(a::FqMPolyRingElem)
    return iszero(a.data)
end

function is_monomial(a::FqMPolyRingElem)
    return is_monomial(a.data)
end

function is_term(a::FqMPolyRingElem)
    return is_term(a.data)
end

function is_unit(a::FqMPolyRingElem)
    return is_unit(a.data)
end

function is_constant(a::FqMPolyRingElem)
    return is_constant(a.data)
end

###############################################################################
#
#   AbstractString I/O
#
###############################################################################

function expressify(a::FqMPolyRingElem, x = symbols(parent(a)); context = nothing)
    return expressify(a.data, x, context = context)
end

# AA has enable all show via expressify for all MPolys

################################################################################
#
#  Getting coefficients
#
################################################################################

function coeff(a::FqMPolyRingElem, i::Int)
    return _unchecked_coerce(base_ring(a), coeff(a.data, i))
end

function coeff(a::FqMPolyRingElem, b::FqMPolyRingElem)
    return _unchecked_coerce(base_ring(a), coeff(a.data, b.data))
end

function trailing_coefficient(a::FqMPolyRingElem)
    return _unchecked_coerce(base_ring(a), trailing_coefficient(a.data))
end

###############################################################################
#
#   Basic manipulation
#
###############################################################################

function degree(a::FqMPolyRingElem, i::Int)
    return degree(a.data, i)
end

function degrees(a::FqMPolyRingElem)
    return degrees(a.data)
end

function total_degree(a::FqMPolyRingElem)
    return total_degree(a.data)
end

###############################################################################
#
#   Multivariable coefficient polynomials
#
###############################################################################

function coeff(a::FqMPolyRingElem, vars::Vector{Int}, exps::Vector{Int})
    return FqMPolyRingElem(parent(a), coeff(a.data, vars, exps))
end

###############################################################################
#
#   Basic arithmetic
#
###############################################################################

function -(a::FqMPolyRingElem)
    R = parent(a)
    @fq_default_mpoly_do_op(-, R, a)
end

function +(a::FqMPolyRingElem, b::FqMPolyRingElem)
    check_parent(a, b)
    R = parent(a)
    @fq_default_mpoly_do_op(+, R, a, b)
end

function -(a::FqMPolyRingElem, b::FqMPolyRingElem)
    check_parent(a, b)
    R = parent(a)
    @fq_default_mpoly_do_op(-, R, a, b)
end

function *(a::FqMPolyRingElem, b::FqMPolyRingElem)
    check_parent(a, b)
    R = parent(a)
    @fq_default_mpoly_do_op(*, R, a, b)
end

###############################################################################
#
#   Ad hoc arithmetic
#
###############################################################################

function +(a::FqMPolyRingElem, b::IntegerUnion)
    return FqMPolyRingElem(parent(a), a.data + base_ring(a.data)(b))
end

function +(a::FqMPolyRingElem, b::FqFieldElem)
    parent(b) == base_ring(a) || error("Unable to coerce element")
    b1 = _unchecked_coerce(base_ring(a.data), b)
    return FqMPolyRingElem(parent(a), a.data + b1)
end

function +(b::Union{FqFieldElem, Integer}, a::FqMPolyRingElem)
    return a + b
end

function -(a::FqMPolyRingElem, b::IntegerUnion)
    return FqMPolyRingElem(parent(a), a.data - base_ring(a.data)(b))
end

function -(a::FqMPolyRingElem, b::FqFieldElem)
    parent(b) == base_ring(a) || error("Unable to coerce element")
    b1 = _unchecked_coerce(base_ring(a.data), b)
    return FqMPolyRingElem(parent(a), a.data - b1)
end

function -(b::IntegerUnion, a::FqMPolyRingElem)
    return FqMPolyRingElem(parent(a), base_ring(a.data)(b) - a.data)
end

function -(b::FqFieldElem, a::FqMPolyRingElem)
    parent(b) == base_ring(a) || error("Unable to coerce element")
    b1 = _unchecked_coerce(base_ring(a.data), b)
    return FqMPolyRingElem(parent(a), b1 - a.data)
end

function *(a::FqMPolyRingElem, b::IntegerUnion)
    return FqMPolyRingElem(parent(a), a.data * base_ring(a.data)(b))
end

function *(a::FqMPolyRingElem, b::FqFieldElem)
    parent(b) == base_ring(a) || error("Unable to coerce element")
    b1 = _unchecked_coerce(base_ring(a.data), b)
    return FqMPolyRingElem(parent(a), a.data * b1)
end

function *(b::Union{FqFieldElem, Integer}, a::FqMPolyRingElem)
    return a*b
end

###############################################################################
#
#   Powering
#
###############################################################################

function ^(a::FqMPolyRingElem, b::Integer)
    return FqMPolyRingElem(parent(a), a.data^b)
end

################################################################################
#
#   GCD
#
################################################################################

function gcd(a::FqMPolyRingElem, b::FqMPolyRingElem)
    check_parent(a, b)
    return FqMPolyRingElem(parent(a), gcd(a.data, b.data))
end

function gcd_with_cofactors(a::FqMPolyRingElem, b::FqMPolyRingElem)
    check_parent(a, b)
    (g, abar, bbar) = gcd_with_cofactors(a.data, b.data)
    R = parent(a)
    return (FqMPolyRingElem(R, g), FqMPolyRingElem(R, abar), FqMPolyRingElem(R, bbar))
end

################################################################################
#
#   Factorization and Square Root
#
################################################################################

function _convert_fac(a::FqMPolyRing, b::Fac)
    f = Fac{FqMPolyRingElem}()
    f.unit = FqMPolyRingElem(a, b.unit)
    for (p, e) in b
        f[FqMPolyRingElem(a, p)] = e
    end
    return f
end

function factor(a::FqMPolyRingElem)
    iszero(a) && throw(ArgumentError("Argument must be non-zero"))
    return _convert_fac(parent(a), factor(a.data))
end

function factor_squarefree(a::FqMPolyRingElem)
    iszero(a) && throw(ArgumentError("Argument must be non-zero"))
    return _convert_fac(parent(a), factor_squarefree(a.data))
end

function sqrt(a::FqMPolyRingElem; check::Bool=true)
    return FqMPolyRingElem(parent(a), sqrt(a.data, check = check))
end

function is_square(a::FqMPolyRingElem)
    return is_square(a.data)
end

function is_square_with_sqrt(a::FqMPolyRingElem)
    x, y = is_square_with_sqrt(a.data)
    return x, FqMPolyRingElem(parent(a), y)
end

###############################################################################
#
#   Comparison
#
###############################################################################

function ==(a::FqMPolyRingElem, b::FqMPolyRingElem)
    check_parent(a, b)
    return a.data == b.data
end

function Base.isless(a::FqMPolyRingElem, b::FqMPolyRingElem)
    check_parent(a, b)
    return isless(a.data, b.data)
end

###############################################################################
#
#   Ad hoc comparison
#
###############################################################################

function ==(a::FqMPolyRingElem, b::FqFieldElem)
    return a.data == _unchecked_coerce(base_ring(a.data), b)
end

function ==(b::FqFieldElem, a::FqMPolyRingElem)
    return a.data == _unchecked_coerce(base_ring(a.data), b)
end

function ==(a::FqMPolyRingElem, b::IntegerUnion)
    return a.data == base_ring(a.data)(b)
end

function ==(b::IntegerUnion, a::FqMPolyRingElem)
    return a.data == base_ring(a.data)(b)
end

###############################################################################
#
#   Divisibility
#
###############################################################################

function divides(a::FqMPolyRingElem, b::FqMPolyRingElem)
    check_parent(a, b)
    x, y = divides(a.data, b.data)
    return x, FqMPolyRingElem(parent(a), y)
end

###############################################################################
#
#   Division with remainder
#
###############################################################################

function Base.div(a::(FqMPolyRingElem), b::(FqMPolyRingElem))
    check_parent(a, b)
    return FqMPolyRingElem(parent(a), div(a.data, b.data))
end

function Base.divrem(a::FqMPolyRingElem, b::FqMPolyRingElem)
    check_parent(a, b)
    x, y = divrem(a.data, b.data)
    return FqMPolyRingElem(parent(a), x), FqMPolyRingElem(parent(a), y)
end

function Base.divrem(a::FqMPolyRingElem, b::Vector{FqMPolyRingElem})
    for bi in b
        check_parent(a, bi)
    end
    ad = a.data
    bd = typeof(ad)[bi.data for bi in b]
    q, r = Base.divrem(ad, bd)
    return [FqMPolyRingElem(parent(a), qi) for qi in q],
           FqMPolyRingElem(parent(a), r)
end

###############################################################################
#
#   Exact division
#
###############################################################################

function divexact(a::(FqMPolyRingElem), b::(FqMPolyRingElem); check::Bool=true)
    check_parent(a, b)
    return FqMPolyRingElem(parent(a), divexact(a.data, b.data))
end

###############################################################################
#
#   Calculus
#
###############################################################################

function derivative(a::FqMPolyRingElem, i::Int)
    return FqMPolyRingElem(parent(a), derivative(a.data, i))
end

###############################################################################
#
#   Evaluation
#
###############################################################################

# TODO have AA define evaluate(a, vals) for general vals
# so we can get rid of this copy pasta
function (a::FqMPolyRingElem)(vals::Union{NCRingElem, RingElement}...)
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

function zero!(a::FqMPolyRingElem)
    a.data = zero!(a.data)
    return a
end

function add!(a::FqMPolyRingElem, b::FqMPolyRingElem, c::FqMPolyRingElem)
    a.data = add!(a.data, b.data, c.data)
    return a
end

function addeq!(a::FqMPolyRingElem, b::FqMPolyRingElem)
    a.data = addeq!(a.data, b.data)
    return a
end

function mul!(a::FqMPolyRingElem, b::FqMPolyRingElem, c::FqMPolyRingElem)
    a.data = mul!(a.data, b.data, c.data)
    return a
end

function setcoeff!(a::FqMPolyRingElem, n::Int, c::FqFieldElem)
    Rd = parent(a).data
    a.data = setcoeff!(a.data, n, _unchecked_coerce(base_ring(Rd), c))
    return a
end

function combine_like_terms!(a::FqMPolyRingElem)
    a.data = combine_like_terms!(a.data)
    return a
end

###############################################################################
#
#   Manipulating terms and monomials
#
###############################################################################

function set_exponent_vector!(a::FqMPolyRingElem, n::Int, exps::Vector{T}) where T
    a.data = set_exponent_vector!(a.data, n, exps)
    return a
end

function exponent_vector(a::FqMPolyRingElem, i::Int)
    return exponent_vector(a.data, i)
end

function exponent(a::FqMPolyRingElem, i::Int, j::Int)
    return exponent(a.data, i, j)
end

function coeff(a::FqMPolyRingElem, exps::Vector{T}) where T
    return _unchecked_coerce(base_ring(a), coeff(a.data, exps))
end

function sort_terms!(a::FqMPolyRingElem)
    sort_terms!(a.data)
    return a
end

function term(a::FqMPolyRingElem, i::Int)
    return FqMPolyRingElem(parent(a), term(a.data, i))
end

function monomial(a::(FqMPolyRingElem), i::Int)
    return FqMPolyRingElem(parent(a), monomial(a.data, i))
end

function monomial!(m::(FqMPolyRingElem), a::(FqMPolyRingElem), i::Int)
    m.data = monomial!(m.data, a.data, i)
    return m
end

###############################################################################
#
#   Promotion rules
#
###############################################################################

promote_rule(::Type{(FqMPolyRingElem)}, ::Type{V}) where {V <: Integer} = (FqMPolyRingElem)

promote_rule(::Type{(FqMPolyRingElem)}, ::Type{ZZRingElem}) = (FqMPolyRingElem)

promote_rule(::Type{(FqMPolyRingElem)}, ::Type{FqFieldElem}) = (FqMPolyRingElem)

###############################################################################
#
#   Parent object call overload
#
###############################################################################

function (R::FqMPolyRing)()
    return FqMPolyRingElem(R, R.data())
end

function (R::FqMPolyRing)(b::IntegerUnion)
    return FqMPolyRingElem(R, R.data(b))
end

function (R::FqMPolyRing)(b::FqFieldElem)
    parent(b) == base_ring(R) || error("Unable to coerce element")
    return FqMPolyRingElem(R, R.data(_unchecked_coerce(base_ring(R.data), b)))
end


function (R::FqMPolyRing)(a::FqMPolyRingElem)
   parent(a) == R || error("Unable to coerce polynomial")
   return a
end

function (R::FqMPolyRing)(a::Vector{FqFieldElem}, b::Vector{Vector{Int}})
    F = base_ring(R.data)
    ad = elem_type(F)[if parent(ai) != base_ring(R)
                        error("coefficient is in the wrong field")
                      else
                        _unchecked_coerce(F, ai)
                      end for ai in a]
    return FqMPolyRingElem(R, R.data(ad, b))
end

###############################################################################
#
#  Ad hoc exact division
#
###############################################################################

function divexact(a::FqMPolyRingElem, b::FqFieldElem; check::Bool=true)
    return a*inv(b)
end

function divexact(a::FqMPolyRingElem, b::IntegerUnion; check::Bool=true)
  return a*inv(base_ring(a)(b))
end
