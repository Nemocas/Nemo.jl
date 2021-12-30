
const _fq_default_mpoly_union = Union{AbstractAlgebra.Generic.MPoly{fq},
                                      fq_nmod_mpoly,
                                      gfp_mpoly,
                                      #gfp_fmpz_mpoly
                                      }

mutable struct FqDefaultMPolyRing <: MPolyRing{fq_default}
    data::Union{GFPMPolyRing,
                #GFPFmpzMPolyRing,
                FqNmodMPolyRing,
                AbstractAlgebra.Generic.MPolyRing{fq}}
    base_ring::FqDefaultFiniteField
    typ::Int    # keep these in sync with fq_default_mpoly_do_op
end

mutable struct fq_default_mpoly <: MPolyElem{fq_default}
    parent::FqDefaultMPolyRing
    data::_fq_default_mpoly_union

    function fq_default_mpoly(a::FqDefaultMPolyRing, b::_fq_default_mpoly_union)
        a.data == parent(b) || error("bad parents")
        return new(a, b)
    end
end

# julia fails to generate decent code unless it is all pasted in
macro fq_default_mpoly_do_op(f, R, a...)
    f = Expr(:escape, f)
    R = Expr(:escape, R)
    a = (Expr(:escape, ai) for ai in a)
    res = nothing
    for (tnum, T) in ((1, :(AbstractAlgebra.Generic.MPoly{fq})),
                      (2, :(fq_nmod_mpoly)),
                      (3, :(gfp_mpoly)),
                     )
        ret = (Expr(:(::), Expr(:(.), ai, QuoteNode(:data)), T) for ai in a)
        ret = Expr(:return, Expr(:call, f, ret...))
        if res == nothing
            res = ret
        else
            cond = Expr(:call, :(==), Expr(:(.), R, QuoteNode(:typ)), tnum)
            res = Expr(:if, cond, ret, res)
        end
    end
    return res
end

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

function isgen(a::fq_default_mpoly, i::Int)
    return isgen(a.data, i)
end

function isgen(a::fq_default_mpoly)
    return isgen(a.data)
end

function deepcopy_internal(a::fq_default_mpoly, dict::IdDict)
    return fq_default_mpoly(parent(a), deepcopy_internal(a.data, dict))
end

function length(a::fq_default_mpoly)
   return length(a.data)
end

function one(R::FqDefaultMPolyRing)
    return one(R.data)
end

function zero(R::FqDefaultMPolyRing)
    return zero(R.data)
end

function isone(a::fq_default_mpoly)
    return isone(a.data)
end

function iszero(a::fq_default_mpoly)
    return iszero(a.data)
end

function ismonomial(a::fq_default_mpoly)
    return ismonomial(a.data)
end

function isterm(a::fq_default_mpoly)
    return isterm(a.data)
end

function isunit(a::fq_default_mpoly)
    return isunit(a.data)
end

function isconstant(a::fq_default_mpoly)
    return isconstant(a.data)
end

###############################################################################
#
#   AbstractString I/O
#
###############################################################################

function expressify(a::fq_default_mpoly, x = symbols(parent(a)); context = nothing)
    return expressify(a.data, x, context = context)
end

@enable_all_show_via_expressify fq_default_mpoly


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
    return fq_default(base_ring(a), coeff(a.data, i))
end

function coeff(a::fq_default_mpoly, b::fq_default_mpoly)
    return fq_default(base_ring(a), coeff(a.data, b.data))
end

function trailing_coefficient(p::fq_default_mpoly)
    return fq_default(base_ring(a), trailing_coefficient(p.data))
end

###############################################################################
#
#   Basic manipulation
#
###############################################################################

function degree(a::fq_default_mpoly, i::Int)
    return degree(a.data, i)
end

function degree_fmpz(a::fq_default_mpoly, i::Int)
    return degree_fmpz(a.data, i)
end

function degrees_fit_int(a::fq_default_mpoly)
    return degrees_fit_int(a.data)
end

function degrees(a::fq_default_mpoly)
    return degrees(a.data)
end

function degrees_fmpz(a::fq_default_mpoly)
    return degrees_fmpz(a.data)
end

function total_degree_fits_int(a::fq_default_mpoly)
    return total_degree_fits_int(a.data)
end

function total_degree(a::fq_default_mpoly)
    return total_degree(a.data)
end

function total_degree_fmpz(a::fq_default_mpoly)
    return total_degree_fmpz(a.data)
end

###############################################################################
#
#   Multivariable coefficient polynomials
#
###############################################################################

function coeff(a::fq_default_mpoly, vars::Vector{Int}, exps::Vector{Int})
    return fq_default_mpoly(parent(a), coeff(a, vars, exps))
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

function +(a::fq_default_mpoly, b::Union{fq_default, Integer})
    return fq_default_mpoly(parent(a), a.data + base_ring(a.data)(b))
end

function +(b::Union{fq_default, Integer}, a::fq_default_mpoly)
    return fq_default_mpoly(parent(a), base_ring(a.data)(b) + a.data)
end

function -(a::fq_default_mpoly, b::Union{fq_default, Integer})
    return fq_default_mpoly(parent(a), a.data - base_ring(a.data)(b))
end

function -(b::Union{fq_default, Integer}, a::fq_default_mpoly)
    return fq_default_mpoly(parent(a), base_ring(a.data)(b) - a.data)
end

function *(a::fq_default_mpoly, b::Union{fq_default, Integer})
    return fq_default_mpoly(parent(a), a.data * base_ring(a.data)(b))
end

function *(b::Union{fq_default, Integer}, a::fq_default_mpoly)
    return fq_default_mpoly(parent(a), base_ring(a.data)(b) * a.data)
end

###############################################################################
#
#   Powering
#
###############################################################################

function ^(a::fq_default_mpoly, b::Union{Int, fmpz})
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

################################################################################
#
#   Factorization and Square Root
#
################################################################################

function factor(a::fq_default_mpoly)
    return _convert_fac(parent(a), factor(a.data))
end

function factor_squarefree(a::fq_default_mpoly)
    return _convert_fac(parent(a), factor_squarefree(a.data))
end


function sqrt(a::fq_default_mpoly; check::Bool=true)
    return fq_default_mpoly(parent(a), sqrt(a.data, check = check))
end

function issquare(a::fq_default_mpoly)
    return issquare(a.data)
end

function issquare_with_sqrt(a::fq_default_mpoly)
    x, y = issquare_with_sqrt(a.data)
    return x, fq_default_mpoly(parent(a), x)
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
    return a.data == base_ring(a.data)(b)
end

function ==(b::fq_default, a::fq_default_mpoly)
    return a.data == base_ring(a.data)(b)
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
    x, y = divrem(ad, bd)
    return fq_default_mpoly(parent(a), x),
           [fq_default_mpoly(parent(a), yi) for yi in y]
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

# this is probably not going to work
function evaluate(a::fq_default_mpoly, b::Vector)
    return evaluate(a.data, b)
end

function (a::(fq_default_mpoly))(vals::Integer...)
   return evaluate(a.data, [vals...])
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

function setcoeff!(a::fq_default_mpoly, n::Int, c)
    a.data = setcoeff!(a.data, n, base_ring(a)(c))
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

function exponent_vector_fits(::Type{T}, a::fq_default_mpoly, i::Int) where T
    return exponent_vector_fits(T, a.data, i)
end

function exponent_vector!(z::Vector{T}, a::(fq_default_mpoly), i::Int) where T
    return exponent_vector!(z, a.data, i)
end

function exponent_vectors_fmpz(a::fq_default_mpoly)
    return exponent_vectors_fmpz(a.data)
end

function set_exponent_vector!(a::fq_default_mpoly, n::Int, exps::Vector{T}) where T
    a.data = set_exponent_vector(a.data, n, exps)
    return a
end

function exponent(a::fq_default_mpoly, i::Int, j::Int)
    return exponent(a.data, i, j)
end

function coeff(a::fq_default_mpoly, exps::Vector{T}) where T
    return fq_default(base_ring(a), coeff(a.data, exps))
end

function setcoeff!(a::fq_default_mpoly, exps::Vector{T}, b) where T
    setcoeff!(a.data, exps, base_ring(a.data)(b))
    return a
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

promote_rule(::Type{(fq_default_mpoly)}, ::Type{fq_default}) = (fq_default_mpoly)

###############################################################################
#
#   Parent object call overload
#
###############################################################################

function (R::FqDefaultMPolyRing)()
    return fq_default_mpoly(R, R.data())
end

function (R::FqDefaultMPolyRing)(b::Union{fq_default, Integer, fmpz})
    return fq_default_mpoly(R, R.data(base_ring(R)(b)))
end

function (R::FqDefaultMPolyRing)(a::fq_default_mpoly)
   parent(a) == R || error("Unable to coerce polynomial")
   return a
end

function (R::FqDefaultMPolyRing)(a::Vector{fq_default}, b::Vector{Vector{T}}) where {T <: Union{fmpz, UInt}}
    F = base_ring(R)
    A = elem_type(F)[F(ai) for ai in a]
    return fq_default_mpoly(R, R.data(A, b))
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
    Fq = FqFiniteField(m, Symbol(R.var), cached, check = false)
    Fqx = AbstractAlgebra.Generic.PolynomialRing(Fq, s, cached = cached, ordering = ordering)[1]
    parent_obj = FqDefaultMPolyRing(Fqx, R, 1)
    return parent_obj, gens(parent_obj)
end

function PolynomialRing(R::FqDefaultFiniteField, s::Vector{String}; cached::Bool = true, ordering::Symbol = :lex)
   return PolynomialRing(R, [Symbol(x) for x in s]; cached=cached, ordering=ordering)
end

