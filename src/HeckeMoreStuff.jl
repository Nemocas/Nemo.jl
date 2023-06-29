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
