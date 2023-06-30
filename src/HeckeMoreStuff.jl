function Base.copy(a::ZZRingElem)
    return deepcopy(a)
end

function QQMatrix(x::ZZMatrix)
    z = zero_matrix(FlintQQ, nrows(x), ncols(x))
    ccall((:fmpq_mat_set_fmpz_mat, libflint), Nothing, (Ref{QQMatrix}, Ref{ZZMatrix}), z, x)
    return z
end

function round(::Type{Int}, a::QQFieldElem)
    return round(Int, Rational{BigInt}(a))
end

function matrix(a::Vector{Vector{T}}) where {T}
    return matrix(permutedims(reduce(hcat, a), (2, 1)))
end

function prime_field(_::NumField)
    return QQField()
end

function prime_field(F::fqPolyRepField; cached::Bool=true)
    return Native.GF(Int(characteristic(F)), cached=cached)
end

function prime_field(F::FqPolyRepField; cached::Bool=true)
    return Native.GF(characteristic(F), cached=cached)
end

function prime_field(F::T; cached::Bool=true) where {T<:Union{fpField,FpField}}
    return F
end

function evaluate(f::ZZPolyRingElem, r::fqPolyRepFieldElem)
    #Horner - stolen from Claus

    if length(f) == 0
        return parent(r)()
    end

    l = f.length - 1
    s = parent(r)(coeff(f, l))
    for i = l-1:-1:0
        s = s * r + parent(r)(coeff(f, i))
    end
    return s
end

export evaluate!

function evaluate!(z::fqPolyRepFieldElem, f::ZZPolyRingElem, r::fqPolyRepFieldElem)
    #Horner - stolen from Claus

    zero!(z)

    if length(f) == 0
        return z
    end

    l = f.length - 1
    set!(z, parent(r)(coeff(f, l)))
    #s = parent(r)(coeff(f, l))
    for i = l-1:-1:0
        mul!(z, z, r)
        add!(z, z, parent(r)(coeff(f, i)))
        #s = s*r + parent(r)(coeff(f, i))
    end
    return z
end

export trunc, round, ceil, floor

for (s, f) in ((:trunc, Base.trunc), (:round, Base.round), (:ceil, Base.ceil), (:floor, Base.floor))
    @eval begin
        function ($s)(a::Matrix{BigFloat})
            s = Base.size(a)
            m = zero_matrix(FlintZZ, s[1], s[2])
            for i = 1:s[1]
                for j = 1:s[2]
                    m[i, j] = FlintZZ(BigInt(($f)(a[i, j])))
                end
            end
            return m
        end
    end
end

export is_constant

function is_constant(f::PolyElem)
    return f.length < 2
end

function identity_matrix(::Type{MatElem}, R::Ring, n::Int)
    return identity_matrix(R, n)
end

function norm(v::arb_mat)
    return sqrt(sum([a^2 for a in v]))
end

function real(tau::acb_mat)
    return map(real, tau)
end

function imag(tau::acb_mat)
    return map(imag, tau)
end

*(x::acb, y::arb_mat) = x * _acb_mat(y)
*(x::arb_mat, y::acb) = y * x
*(x::arb_mat, y::acb_mat) = _acb_mat(x) * y
*(x::acb_mat, y::arb_mat) = x * _acb_mat(y)
+(x::arb_mat, y::acb_mat) = _acb_mat(x) + y
+(x::acb_mat, y::arb_mat) = y + x
-(x::arb_mat, y::acb_mat) = x + (-y)
-(x::acb_mat, y::arb_mat) = x + (-y)
//(x::arb_mat, y::arb) = map(t -> t // y, x)


function _acb_mat(A::arb_mat)
    p = precision(base_ring(A))
    Cc = AcbField(p)
    return change_base_ring(Cc, A)
end

function mul!(z::acb, x::acb, y::arb)
    ccall((:acb_mul_arb, libarb), Nothing, (Ref{acb}, Ref{acb}, Ref{arb}, Int),
        z, x, y, parent(z).prec)
    return z
end

#TODO: should be done in Nemo/AbstractAlgebra s.w.
#      needed by ^ (the generic power in Base using square and multiply)
Base.copy(f::Generic.MPoly) = deepcopy(f)
Base.copy(f::Generic.Poly) = deepcopy(f)

@doc raw"""
    valuation(G::QQMatrix, p)

Return the minimum valuation of the entries of `G`.
"""
function valuation(G::QQMatrix, p)
    return minimum([x == 0 ? inf : valuation(x, p) for x in G])
end

function roots(f::ZZModPolyRingElem, p::ZZRingElem, e::Int)
    F = Fac{ZZRingElem}()
    F.unit = one(ZZRingElem)
    F[p] = e
    return roots(f, F)
end
function roots(f::ZZModPolyRingElem, fac::Fac{ZZRingElem})
    res = fmpz_mod_poly_factor(base_ring(f))
    _fac = fmpz_factor()
    for (p, e) in fac
        ccall((:_fmpz_factor_append, libflint), Nothing, (Ref{fmpz_factor}, Ref{ZZRingElem}, UInt), _fac, p, UInt(e))
    end
    ccall((:fmpz_mod_poly_roots_factored, libflint), Nothing, (Ref{fmpz_mod_poly_factor}, Ref{ZZModPolyRingElem}, Cint, Ref{fmpz_factor}, Ref{fmpz_mod_ctx_struct}), res, f, 1, _fac, base_ring(f).ninv)
    _res = Tuple{ZZModRingElem,Int}[]
    for i in 1:res.num
        g = parent(f)()
        ccall((:fmpz_mod_poly_factor_get_fmpz_mod_poly, libflint), Nothing,
            (Ref{ZZModPolyRingElem}, Ref{fmpz_mod_poly_factor}, Int,
                Ref{fmpz_mod_ctx_struct}),
            g, res, i - 1, base_ring(f).ninv)
        e = unsafe_load(res.exp, i)
        push!(_res, (-coeff(g, 0), e))
    end
    return _res
end

ZZMatrix(M::Matrix{Int}) = matrix(FlintZZ, M)

zero_matrix(::Type{Int}, r, c) = zeros(Int, r, c)

base_ring(::Vector{Int}) = Int

function AbstractAlgebra.is_symmetric(M::MatElem)
    for i in 1:nrows(M)
        for j in i:ncols(M)
            if M[i, j] != M[j, i]
                return false
            end
        end
    end
    return true
end

################################################################################
#
#  Create a matrix from rows
#
################################################################################

function matrix(K::Ring, R::Vector{<:Vector})
    if length(R) == 0
        return zero_matrix(K, 0, 0)
    else
        n = length(R)
        m = length(R[1])
        z = zero_matrix(K, n, m)
        for i in 1:n
            @assert length(R[i]) == m
            for j in 1:m
                z[i, j] = R[i][j]
            end
        end
        return z
    end
end

order(::ZZRingElem) = FlintZZ

export neg!

sub!(z::Rational{Int}, x::Rational{Int}, y::Int) = x - y

neg!(z::Rational{Int}, x::Rational{Int}) = -x

add!(z::Rational{Int}, x::Rational{Int}, y::Int) = x + y

mul!(z::Rational{Int}, x::Rational{Int}, y::Int) = x * y

is_negative(x::Rational) = x.num < 0

function is_negative(x::QQFieldElem)
    c = ccall((:fmpq_sgn, libflint), Cint, (Ref{QQFieldElem},), x)
    return c < 0
end

function sub!(z::Vector{QQFieldElem}, x::Vector{QQFieldElem}, y::Vector{ZZRingElem})
    for i in 1:length(z)
        sub!(z[i], x[i], y[i])
    end
    return z
end

function is_upper_triangular(A::Generic.Mat)
    m = nrows(A)
    n = ncols(A)
    d = 0
    for r = 1:m
        for c = 1:n
            if !iszero(A[r, c])
                if c <= d
                    return false
                end
                d = c
                break
            end
        end
    end
    return true
end

function sub(M::Generic.Mat, rows::UnitRange{Int}, cols::UnitRange{Int})
    @assert step(rows) == 1 && step(cols) == 1
    z = zero_matrix(base_ring(M), length(rows), length(cols))
    for i in rows
        for j in cols
            z[i-first(rows)+1, j-first(cols)+1] = M[i, j]
        end
    end
    return z
end

function valuation(a::UInt, b::UInt)
    return ccall((:n_remove, libflint), Int, (Ref{UInt}, UInt), a, b)
end

fits(::Type{Int}, a::Int) = true

function fits(::Type{Int}, a::Integer)
    #TODO: possibly not optimal?
    return a % Int == a
end

function (Zx::ZZPolyRing)(a::nf_elem)
    b = Zx()
    @assert denominator(a) == 1
    if degree(parent(a)) == 1
        # If the number field is linear, then a.elem_length is not properly
        # initialized, that is, it could be anything.
        setcoeff!(b, 0, numerator(coeff(a, 0)))
    elseif degree(parent(a)) == 2
        # ... or quadratic, then a.elem_length is not properly
        # initialized, that is, it could be anything.
        setcoeff!(b, 0, numerator(coeff(a, 0)))
        setcoeff!(b, 1, numerator(coeff(a, 1)))
    else
        for i = 0:a.elem_length
            setcoeff!(b, i, numerator(coeff(a, i)))
        end
    end
    return b
end

function Base.round(::Type{ZZRingElem}, x::arb)
    if radius(x) > 1e-1
        throw(InexactError(:round, ZZRingElem, x))
    end
    return setprecision(BigFloat, precision(parent(x))) do
        round(ZZRingElem, BigFloat(x))
    end
end

function Base.round(::Type{ZZMatrix}, C::arb_mat)
    v = zero_matrix(FlintZZ, nrows(C), ncols(C))

    for i = 1:nrows(C)
        for j = 1:ncols(C)
            v[i, j] = round(ZZRingElem, C[i, j])
        end
    end
    return v
end

function discriminant(K::QQField)
    return one(K)
end

gen(Q::QQField) = one(Q)

real(x::QQFieldElem) = x

norm(x::ZZRingElem) = abs(x)

number_field(::ZZRing) = FlintQQ

function change_base_ring(p::MPolyRingElem{T}, g, new_polynomial_ring) where {T<:RingElement}
    cvzip = zip(coefficients(p), exponent_vectors(p))
    M = MPolyBuildCtx(new_polynomial_ring)
    for (c, v) in cvzip
        res = g(c)
        if !iszero(res)
            push_term!(M, g(c), v)
        end
    end
    return finish(M)::elem_type(new_polynomial_ring)
end

function mulmod(a::S, b::S, mod::Vector{S}) where {S<:MPolyRingElem{T}} where {T<:RingElem}
    return Base.divrem(a * b, mod)[2]
end

function Base.hash(f::zzModMPolyRingElem, h::UInt)
    return UInt(1) # TODO: enhance or throw error
end

@inline ngens(R::AbstractAlgebra.Generic.MPolyRing) = R.num_vars

#to make the MPoly module happy, divrem needs it...
function Base.div(a::nf_elem, b::nf_elem)
    return a // b
end

function rem(a::nf_elem, b::nf_elem)
    return parent(a)(0)
end

function AbstractAlgebra.map_coefficients(F::fpField, f::QQMPolyRingElem; parent=polynomial_ring(F, nvars(parent(f)), cached=false)[1])
    dF = denominator(f)
    d = F(dF)
    if iszero(d)
        error("Denominator divisible by p!")
    end
    m = inv(d)
    ctx = MPolyBuildCtx(parent)
    for x in zip(coefficients(f), exponent_vectors(f))
        el = numerator(x[1] * dF)
        push_term!(ctx, F(el) * m, x[2])
    end
    return finish(ctx)
end

export tdivpow2, tdivpow2!

function tdivpow2!(B::ZZMatrix, t::Int)
    ccall((:fmpz_mat_scalar_tdiv_q_2exp, libflint), Nothing, (Ref{ZZMatrix}, Ref{ZZMatrix}, Cint), B, B, t)
end

function tdivpow2(B::ZZMatrix, t::Int)
    C = similar(B)
    ccall((:fmpz_mat_scalar_tdiv_q_2exp, libflint), Nothing, (Ref{ZZMatrix}, Ref{ZZMatrix}, Cint), C, B, t)
    return C
end

@doc raw"""
    round(::Type{ZZRingElem}, a::ZZRingElem, b::ZZRingElem) -> ZZRingElem

Computes `round(a//b)`.
"""
function Base.round(::Type{ZZRingElem}, a::ZZRingElem, b::ZZRingElem)
    s = sign(a) * sign(b)
    bs = abs(b)
    as = abs(a)
    r = s * div(2 * as + bs, 2 * bs)
    #  @assert r == round(ZZRingElem, a//b)
    return r
end

function is_squarefree(x::Generic.Poly{nf_elem})
    return isone(gcd(x, derivative(x), true))
end

function degree(a::nf_elem)
    return degree(minpoly(a))
end

################################################################################
#
#  Characteristic polynomial
#
################################################################################

function charpoly(Qx::QQPolyRing, a::nf_elem)
    f = charpoly(Qx, representation_matrix(a))
    return f
end

function charpoly(a::nf_elem)
    f = charpoly(parent(parent(a).pol), a)
    return f
end

function charpoly(a::nf_elem, ::QQField)
    return charpoly(a)
end

function charpoly(Zx::ZZPolyRing, a::nf_elem)
    f = charpoly(a)
    if !isone(denominator(f))
        error("Element is not integral")
    end
    return Zx(f)
end

function charpoly(a::nf_elem, Z::ZZRing)
    return charpoly(polynomial_ring(Z, cached=false)[1], a)
end

################################################################################
#
#  Minimal polynomial
#
################################################################################

@doc raw"""
    minpoly(a::nf_elem) -> QQPolyRingElem

The minimal polynomial of $a$.
"""
function minpoly(Qx::QQPolyRing, a::nf_elem)
    f = minpoly(Qx, representation_matrix(a))
    return f
end

function minpoly(a::nf_elem)
    f = minpoly(parent(parent(a).pol), a)
    return f
end

function minpoly(a::nf_elem, ::QQField)
    return minpoly(a)
end

function minpoly(a::nf_elem, ZZ::ZZRing)
    return minpoly(polynomial_ring(ZZ, cached=false)[1], a)
end

function minpoly(Zx::ZZPolyRing, a::nf_elem)
    f = minpoly(a)
    if !isone(denominator(f))
        error("Element is not integral")
    end
    return Zx(f)
end

###

function one!(a::QQMPolyRingElem)
    ccall((:fmpq_mpoly_one, libflint), Nothing,
        (Ref{QQMPolyRingElem}, Ref{QQMPolyRing}), a, parent(a))
    return a
end

(::QQField)(a::nf_elem) = (is_rational(a) && return coeff(a, 0)) || error("not a rational")
(::ZZRing)(a::nf_elem) = (isinteger(a) && return numerator(coeff(a, 0))) || error("not an integer")

function set_name!(K::AnticNumberField, s::String)
    set_attribute!(K, :name => s)
end

function set_name!(K::AnticNumberField)
    s = find_name(K)
    s === nothing || set_name!(K, string(s))
end

function Base.:(^)(a::nf_elem, e::UInt)
    b = parent(a)()
    ccall((:nf_elem_pow, libantic), Nothing,
        (Ref{nf_elem}, Ref{nf_elem}, UInt, Ref{AnticNumberField}),
        b, a, e, parent(a))
    return b
end

Base.copy(f::QQPolyRingElem) = parent(f)(f)

function basis(K::AnticNumberField)
    n = degree(K)
    g = gen(K)
    d = Array{typeof(g)}(undef, n)
    b = K(1)
    for i = 1:n-1
        d[i] = b
        b *= g
    end
    d[n] = b
    return d
end

base_field(_::AnticNumberField) = FlintQQ

#trivia to make life easier

gens(L::SimpleNumField{T}) where {T} = [gen(L)]

function gen(L::SimpleNumField{T}, i::Int) where {T}
    i == 1 || error("index must be 1")
    return gen(L)
end

function Base.getindex(L::SimpleNumField{T}, i::Int) where {T}
    if i == 0
        return one(L)
    elseif i == 1
        return gen(L)
    else
        error("index has to be 0 or 1")
    end
end

ngens(L::SimpleNumField{T}) where {T} = 1

is_unit(a::NumFieldElem) = !iszero(a)

canonical_unit(a::NumFieldElem) = a

################################################################################
#
#  Base case for dot products
#
################################################################################

dot(x::ZZRingElem, y::NumFieldElem) = x * y

dot(x::Integer, y::NumFieldElem) = x * y

dot(x::NumFieldElem, y::Integer) = x * y

function dot(a::Vector{<:NumFieldElem}, b::Vector{ZZRingElem})
    d = zero(parent(a[1]))
    t = zero(d)
    for i = 1:length(a)
        mul!(t, a[i], b[i])
        add!(d, d, t)
    end
    return d
end

function (R::zzModPolyRing)(g::QQPolyRingElem)
    return fmpq_poly_to_nmod_poly(R, g)
end

function (R::fpPolyRing)(g::QQPolyRingElem)
    return fmpq_poly_to_gfp_poly(R, g)
end

function (R::ZZModPolyRing)(g::QQPolyRingElem)
    return fmpq_poly_to_fmpz_mod_poly(R, g)
end

function (R::FpPolyRing)(g::QQPolyRingElem)
    return fmpq_poly_to_gfp_fmpz_poly(R, g)
end

function (R::FqPolyRing)(g::QQPolyRingElem)
    return fmpq_poly_to_fq_default_poly(R, g)
end
