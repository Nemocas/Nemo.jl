##
## rand for Flint-Finite fields
##
## fqPolyRepFieldElem has no base ring
function rand(R::fqPolyRepField)
    #gen is not a generator for the group!
    r = R(0)
    for i = 0:degree(R)
        r *= gen(R)
        r += rand(1:characteristic(R))
    end

    return r
end

function rand(R::FqPolyRepField)
    #gen is not a generator for the group!
    r = R(0)
    for i = 0:degree(R)
        r *= gen(R)
        r += rand(1:characteristic(R))
    end

    return r
end

################################################################################
#
#  Iterators for finite fields
#
################################################################################

# fqPolyRepField

function Base.iterate(F::fqPolyRepField)
    return zero(F), zeros(UInt, degree(F))
end

function Base.iterate(F::fqPolyRepField, st::Vector{UInt})
    done = true
    for j in 1:length(st)
        if st[j] != F.n - 1
            done = false
        end
    end

    if done
        return nothing
    end

    st[1] = st[1] + 1
    for j in 1:(length(st)-1)
        if st[j] == F.n
            st[j] = 0
            st[j+1] = st[j+1] + 1
        end
    end

    d = F()
    ccall((:fq_nmod_init2, libflint), Nothing,
        (Ref{fqPolyRepFieldElem}, Ref{fqPolyRepField}), d, F)
    for j in 1:length(st)
        ccall((:nmod_poly_set_coeff_ui, libflint), Nothing,
            (Ref{fqPolyRepFieldElem}, Int, UInt), d, j - 1, st[j])
    end

    return d, st
end

Base.length(F::fqPolyRepField) = BigInt(F.n)^degree(F)

Base.IteratorSize(::Type{fqPolyRepField}) = Base.HasLength()

Base.eltype(::fqPolyRepField) = fqPolyRepFieldElem

# FqPolyRepField

function Base.iterate(F::FqPolyRepField)
    return zero(F), zeros(ZZ, degree(F))
end

function Base.iterate(F::FqPolyRepField, st::Vector{ZZRingElem})
    done = true
    for j in 1:length(st)
        if st[j] != characteristic(F) - 1
            done = false
        end
    end

    if done
        return nothing
    end

    st[1] = st[1] + 1
    for j in 1:(length(st)-1)
        if st[j] == characteristic(F)
            st[j] = 0
            st[j+1] = st[j+1] + 1
        end
    end

    d = F()
    ccall((:fq_init2, libflint), Nothing,
        (Ref{FqPolyRepFieldElem}, Ref{FqPolyRepField}), d, F)
    for j in 1:length(st)
        #ccall((:fmpz_poly_set_coeff_fmpz, libflint), (Ref{ZZPolyRingElem}, Int, Ref{ZZRingElem}), g, j - 1, st[j])
        ccall((:fmpz_poly_set_coeff_fmpz, libflint), Nothing,
            (Ref{FqPolyRepFieldElem}, Int, Ref{ZZRingElem}), d, j - 1, st[j])
    end

    return d, st
end

Base.eltype(::FqPolyRepField) = FqPolyRepFieldElem

Base.length(F::FqPolyRepField) = BigInt(characteristic(F)^degree(F))

Base.IteratorSize(::Type{FqPolyRepField}) = Base.HasLength()

function (A::fqPolyRepField)(x::fpFieldElem)
    @assert characteristic(A) == characteristic(parent(x))
    return A(lift(x))
end

function (A::FqPolyRepField)(x::EuclideanRingResidueFieldElem{ZZRingElem})
    @assert characteristic(A) == characteristic(parent(x))
    return A(lift(x))
end

AbstractAlgebra.promote_rule(::Type{fqPolyRepFieldElem}, ::Type{fpFieldElem}) = fqPolyRepFieldElem

AbstractAlgebra.promote_rule(::Type{FqPolyRepFieldElem}, ::Type{FpFieldElem}) = FqPolyRepFieldElem

## Minpoly/ Charpoly

function minpoly(a::fqPolyRepFieldElem)
    return minpoly(polynomial_ring(Native.GF(Int(characteristic(parent(a))), cached=false), cached=false)[1], a)
end

function minpoly(Rx::fpPolyRing, a::fqPolyRepFieldElem)
    c = [a]
    fa = frobenius(a)
    while !(fa in c)
        push!(c, fa)
        fa = frobenius(fa)
    end
    St = polynomial_ring(parent(a), cached=false)[1]
    f = prod([gen(St) - x for x = c], init=one(St))
    g = Rx()
    for i = 0:degree(f)
        setcoeff!(g, i, coeff(coeff(f, i), 0))
    end
    return g
end

function charpoly(a::fqPolyRepFieldElem)
    return charpoly(polynomial_ring(Native.GF(Int(characteristic(parent(a))), cached=false), cached=false)[1], a)
end

function charpoly(Rx::fpPolyRing, a::fqPolyRepFieldElem)
    g = minpoly(Rx, a)
    return g^div(degree(parent(a)), degree(g))
end


function minpoly(a::FqPolyRepFieldElem)
    return minpoly(polynomial_ring(Native.GF(characteristic(parent(a)), cached=false), cached=false)[1], a)
end

function minpoly(Rx::FpPolyRing, a::FqPolyRepFieldElem)
    c = [a]
    fa = frobenius(a)
    while !(fa in c)
        push!(c, fa)
        fa = frobenius(fa)
    end
    St = polynomial_ring(parent(a), cached=false)[1]
    f = prod([gen(St) - x for x = c])
    g = Rx()
    for i = 0:degree(f)
        setcoeff!(g, i, coeff(coeff(f, i), 0))
    end
    return g
end

################################################################################
#
#  Missing ad hoc operations
#
################################################################################

function (R::FqPolyRepField)(x::FpFieldElem)
    return R(lift(x))
end

function *(a::FqPolyRepFieldElem, b::FpFieldElem)
    return a * parent(a)(b)
end

function *(a::FpFieldElem, b::FqPolyRepFieldElem)
    return parent(b)(a) * b
end

function preimage(phi::FinFieldMorphism, x::FinFieldElem)
    return preimage_map(phi)(x)
end

function (R::zzModRing)(a::fpFieldElem)
    @assert modulus(R) == characteristic(parent(a))
    return R(data(a))
end

function (k::fqPolyRepField)(a::QQFieldElem)
    return k(numerator(a)) // k(denominator(a))
end

function (k::FpField)(a::QQFieldElem)
    return k(numerator(a)) // k(denominator(a))
end

function (k::FqPolyRepField)(a::QQFieldElem)
    return k(numerator(a)) // k(denominator(a))
end


(F::fqPolyRepField)(a::zzModRingElem) = F(a.data)

#TODO/ think: 
# - should those be zzModMatrix of fpMatrix
# - base_ring/ coeff_field needs to be unique?
function representation_matrix(a::fpFieldElem)
    return matrix(parent(a), 1, 1, [a])
end

function representation_matrix(a::fqPolyRepFieldElem)
    F = parent(a)
    k = quo(ZZ, Int(characteristic(F)))[1]
    k = Native.GF(Int(characteristic(F)))
    m = zero_matrix(k, degree(F), degree(F))
    ccall((:fq_nmod_embed_mul_matrix, libflint), Nothing, (Ref{fpMatrix}, Ref{fqPolyRepFieldElem}, Ref{fqPolyRepField}), m, a, F)
    ccall((:nmod_mat_transpose, libflint), Nothing, (Ref{fpMatrix}, Ref{fpMatrix}), m, m)
    return m
end

function frobenius_matrix(F::fqPolyRepField, n::Int=1)
    a = frobenius(gen(F), n)
    k = quo(ZZ, Int(characteristic(F)))[1]
    k = Native.GF(Int(characteristic(F)))
    m = zero_matrix(k, degree(F), degree(F))
    ccall((:fq_nmod_embed_composition_matrix_sub, libflint), Nothing, (Ref{fpMatrix}, Ref{fqPolyRepFieldElem}, Ref{fqPolyRepField}, Int), m, a, F, degree(F))
    ccall((:nmod_mat_transpose, libflint), Nothing, (Ref{fpMatrix}, Ref{fpMatrix}), m, m)
    return m
end

function frobenius!(a::fqPolyRepFieldElem, b::fqPolyRepFieldElem, i::Int=1)
    ccall((:fq_nmod_frobenius, libflint), Nothing,
        (Ref{fqPolyRepFieldElem}, Ref{fqPolyRepFieldElem}, Int, Ref{fqPolyRepField}),
        a, b, i, a.parent)
end

################################################################################
#
#  Defining polynomial for finite fields
#
################################################################################

defining_polynomial(F::FqPolyRepField) = minpoly(gen(F))

function defining_polynomial(Q::fqPolyRepField, P::Ring=Native.GF(Int(characteristic(Q)), cached=false))
    Pt, t = polynomial_ring(P, cached=false)
    f = Pt()
    for i = 0:Q.len-1
        j = unsafe_load(reinterpret(Ptr{Int}, Q.j), i + 1)
        a = ZZRingElem()
        ccall((:fmpz_set, libflint), Nothing, (Ref{ZZRingElem}, Int64), a, Q.a + i * sizeof(Ptr))
        setcoeff!(f, j, P(a))
    end
    return f
end

lift(::ZZRing, x::fpFieldElem) = lift(x)

lift(::ZZRing, x::FpFieldElem) = lift(x)
