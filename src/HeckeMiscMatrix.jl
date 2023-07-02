export is_zero_row, is_diagonal, is_lower_triangular, is_positive_entry, is_upper_triangular, diagonal

import LinearAlgebra
LinearAlgebra.dot(a::RingElem, b::RingElem) = a * b

################################################################################
#
#  Dense matrix types
#
################################################################################

dense_matrix_type(::Type{T}) where {T} = Generic.MatSpaceElem{T}

################################################################################
#
#  Unsafe functions for generic matrices
#
################################################################################

#function zero!(a::MatElem)
#  for i in 1:nrows(a)
#    for j in 1:ncols(a)
#      a[i, j] = zero!(a[i, j])
#    end
#  end
#  return a
#end

function mul!(c::MatElem, a::MatElem, b::MatElem)
    ncols(a) != nrows(b) && error("Incompatible matrix dimensions")
    nrows(c) != nrows(a) && error("Incompatible matrix dimensions")
    ncols(c) != ncols(b) && error("Incompatible matrix dimensions")

    if c === a || c === b
        d = parent(a)()
        return mul!(d, a, b)
    end

    t = base_ring(a)()
    for i = 1:nrows(a)
        for j = 1:ncols(b)
            c[i, j] = zero!(c[i, j])
            for k = 1:ncols(a)
                c[i, j] = addmul_delayed_reduction!(c[i, j], a[i, k], b[k, j], t)
            end
            c[i, j] = reduce!(c[i, j])
        end
    end
    return c
end

function add!(c::MatElem, a::MatElem, b::MatElem)
    parent(a) != parent(b) && error("Parents don't match.")
    parent(c) != parent(b) && error("Parents don't match.")
    for i = 1:nrows(c)
        for j = 1:ncols(c)
            c[i, j] = add!(c[i, j], a[i, j], b[i, j])
        end
    end
    return c
end

function mul!(a::zzModMatrix, b::zzModMatrix, c::zzModRingElem)
    ccall((:nmod_mat_scalar_mul, libflint), Nothing,
        (Ref{zzModMatrix}, Ref{zzModMatrix}, UInt), a, b, c.data)
    return a
end

function mul!(c::MatElem, a::MatElem, b::RingElement)
    nrows(c) != nrows(a) && error("Incompatible matrix dimensions")

    if c === a || c === b
        d = parent(a)()
        return mul!(d, a, b)
    end

    t = base_ring(a)()
    for i = 1:nrows(a)
        for j = 1:ncols(a)
            c[i, j] = mul!(c[i, j], a[i, j], b)
        end
    end
    return c
end

################################################################################
#
#  Denominator
#
################################################################################

# This function is really slow...
function denominator(M::QQMatrix)
    d = one(FlintZZ)
    for i in 1:nrows(M)
        for j in 1:ncols(M)
            d = lcm!(d, d, denominator(M[i, j]))
        end
    end
    return d
end

transpose!(A::Union{ZZMatrix, QQMatrix}) = is_square(A) ? transpose!(A, A) : transpose(A)
transpose!(A::MatrixElem) = transpose(A)

function transpose!(A::ZZMatrix, B::ZZMatrix)
    ccall((:fmpz_mat_transpose, libflint), Nothing,
        (Ref{ZZMatrix}, Ref{ZZMatrix}), A, B)
    return A
end

function transpose!(A::QQMatrix, B::QQMatrix)
    ccall((:fmpq_mat_transpose, libflint), Nothing,
        (Ref{QQMatrix}, Ref{QQMatrix}), A, B)
    return A
end

################################################################################
#
#  Zero matrix constructors
#
################################################################################

function zero_matrix(::Type{MatElem}, R::Ring, n::Int)
    return zero_matrix(R, n)
end

function zero_matrix(::Type{MatElem}, R::Ring, n::Int, m::Int)
    return zero_matrix(R, n, m)
end


function matrix(A::Matrix{ZZRingElem})
    m = matrix(FlintZZ, A)
    return m
end

function matrix(A::Matrix{T}) where {T<:RingElem}
    r, c = size(A)
    (r < 0 || c < 0) && error("Array must be non-empty")
    m = matrix(parent(A[1, 1]), A)
    return m
end

function matrix(A::Vector{T}) where {T<:RingElem}
    return matrix(reshape(A, length(A), 1))
end

export scalar_matrix

function scalar_matrix(R::Ring, n::Int, a::RingElement)
    b = R(a)
    z = zero_matrix(R, n, n)
    for i in 1:n
        z[i, i] = b
    end
    return z
end

function Array(a::ZZMatrix; S::Type{T}=ZZRingElem) where {T}
    A = Array{T}(undef, nrows(a), ncols(a))
    for i = 1:nrows(a)
        for j = 1:ncols(a)
            A[i, j] = T(a[i, j])
        end
    end
    return A
end

function is_zero_row(M::ZZMatrix, i::Int)
    GC.@preserve M begin
        for j = 1:ncols(M)
            m = ccall((:fmpz_mat_entry, libflint), Ptr{ZZRingElem}, (Ref{ZZMatrix}, Int, Int), M, i - 1, j - 1)
            fl = ccall((:fmpz_is_zero, libflint), Bool, (Ptr{ZZRingElem},), m)
            if !fl
                return false
            end
        end
    end
    return true
end

function is_positive_entry(M::ZZMatrix, i::Int, j::Int)
    GC.@preserve M begin
        m = ccall((:fmpz_mat_entry, libflint), Ptr{ZZRingElem}, (Ref{ZZMatrix}, Int, Int), M, i - 1, j - 1)
        fl = ccall((:fmpz_sgn, libflint), Int, (Ptr{ZZRingElem},), m)
        return isone(fl)
    end
end



function is_zero_row(M::zzModMatrix, i::Int)
    zero = UInt(0)
    for j in 1:ncols(M)
        t = ccall((:nmod_mat_get_entry, libflint), Base.GMP.Limb, (Ref{zzModMatrix}, Int, Int), M, i - 1, j - 1)
        if t != zero
            return false
        end
    end
    return true
end

function is_zero_row(M::MatElem{T}, i::Int) where {T}
    for j in 1:ncols(M)
        if !iszero(M[i, j])
            return false
        end
    end
    return true
end

function is_zero_row(M::Matrix{T}, i::Int) where {T<:Integer}
    for j = 1:Base.size(M, 2)
        if M[i, j] != 0
            return false
        end
    end
    return true
end

function is_zero_row(M::Matrix{ZZRingElem}, i::Int)
    for j = 1:Base.size(M, 2)
        if M[i, j] != 0
            return false
        end
    end
    return true
end

function is_zero_row(M::Matrix{T}, i::Int) where {T<:RingElem}
    for j in 1:Base.size(M, 2)
        if !iszero(M[i, j])
            return false
        end
    end
    return true
end

export divexact!
export mul!

function divexact!(a::ZZMatrix, b::ZZMatrix, d::ZZRingElem)
    ccall((:fmpz_mat_scalar_divexact_fmpz, libflint), Nothing,
        (Ref{ZZMatrix}, Ref{ZZMatrix}, Ref{ZZRingElem}), a, b, d)
end

function mul!(a::ZZMatrix, b::ZZMatrix, c::ZZRingElem)
    ccall((:fmpz_mat_scalar_mul_fmpz, libflint), Nothing,
        (Ref{ZZMatrix}, Ref{ZZMatrix}, Ref{ZZRingElem}), a, b, c)
end

################################################################################
#
################################################################################

function maximum(f::typeof(abs), a::ZZMatrix)
    m = ccall((:fmpz_mat_entry, libflint), Ptr{ZZRingElem}, (Ref{ZZMatrix}, Int, Int), a, 0, 0)
    for i = 1:nrows(a)
        for j = 1:ncols(a)
            z = ccall((:fmpz_mat_entry, libflint), Ptr{ZZRingElem}, (Ref{ZZMatrix}, Int, Int), a, i - 1, j - 1)
            if ccall((:fmpz_cmpabs, libflint), Cint, (Ptr{ZZRingElem}, Ptr{ZZRingElem}), m, z) < 0
                m = z
            end
        end
    end
    r = ZZRingElem()
    ccall((:fmpz_abs, libflint), Nothing, (Ref{ZZRingElem}, Ptr{ZZRingElem}), r, m)
    return r
end

function maximum(a::ZZMatrix)
    m = ccall((:fmpz_mat_entry, libflint), Ptr{ZZRingElem}, (Ref{ZZMatrix}, Int, Int), a, 0, 0)
    for i = 1:nrows(a)
        for j = 1:ncols(a)
            z = ccall((:fmpz_mat_entry, libflint), Ptr{ZZRingElem}, (Ref{ZZMatrix}, Int, Int), a, i - 1, j - 1)
            if ccall((:fmpz_cmp, libflint), Cint, (Ptr{ZZRingElem}, Ptr{ZZRingElem}), m, z) < 0
                m = z
            end
        end
    end
    r = ZZRingElem()
    ccall((:fmpz_set, libflint), Nothing, (Ref{ZZRingElem}, Ptr{ZZRingElem}), r, m)
    return r
end

function minimum(a::ZZMatrix)
    m = ccall((:fmpz_mat_entry, libflint), Ptr{ZZRingElem}, (Ref{ZZMatrix}, Int, Int), a, 0, 0)
    for i = 1:nrows(a)
        for j = 1:ncols(a)
            z = ccall((:fmpz_mat_entry, libflint), Ptr{ZZRingElem}, (Ref{ZZMatrix}, Int, Int), a, i - 1, j - 1)
            if ccall((:fmpz_cmp, libflint), Cint, (Ptr{ZZRingElem}, Ptr{ZZRingElem}), m, z) > 0
                m = z
            end
        end
    end
    r = ZZRingElem()
    ccall((:fmpz_set, libflint), Nothing, (Ref{ZZRingElem}, Ptr{ZZRingElem}), r, m)
    return r
end

################################################################################
#
#  Lift of matrices to overrings
#
################################################################################

@doc raw"""
    lift(a::Generic.Mat{Generic.ResidueRingElem{ZZRingElem}}) -> ZZMatrix

It returns a lift of the matrix to the integers.
"""
function lift(a::Generic.Mat{Generic.ResidueRingElem{ZZRingElem}})
    z = zero_matrix(FlintZZ, nrows(a), ncols(a))
    for i in 1:nrows(a)
        for j in 1:ncols(a)
            z[i, j] = lift(a[i, j])
        end
    end
    return z
end

function lift(a::ZZModMatrix)
    z = zero_matrix(FlintZZ, nrows(a), ncols(a))
    GC.@preserve a z begin
        for i in 1:nrows(a)
            for j in 1:ncols(a)
                m = ccall((:fmpz_mat_entry, libflint), Ptr{ZZRingElem}, (Ref{ZZMatrix}, Int, Int), z, i - 1, j - 1)
                n = ccall((:fmpz_mod_mat_entry, libflint), Ptr{ZZRingElem}, (Ref{ZZModMatrix}, Int, Int), a, i - 1, j - 1)
                ccall((:fmpz_set, libflint), Nothing, (Ptr{ZZRingElem}, Ptr{ZZRingElem}), m, n)
                #z[i, j] = lift(a[i, j])
            end
        end
    end
    return z
end

function lift(x::FpMatrix)
    return map_entries(lift, x)
end

function lift(a::Generic.Mat{ZZModRingElem})
    z = zero_matrix(FlintZZ, nrows(a), ncols(a))
    for i in 1:nrows(a)
        for j in 1:ncols(a)
            z[i, j] = lift(a[i, j])
        end
    end
    return z
end

################################################################################
#
#  Kernel function
#
################################################################################

@doc raw"""
    kernel(a::MatElem{T}; side::Symbol = :right) -> Int, MatElem{T}

It returns a tuple $(n, M)$, where $n$ is the rank of the kernel and $M$ is a basis for it. If side is $:right$ or not
specified, the right kernel is computed. If side is $:left$, the left kernel is computed.
"""
function kernel(A::MatElem; side::Symbol=:right)
    if side == :right
        return right_kernel(A)
    elseif side == :left
        return left_kernel(A)
    else
        error("Unsupported argument: :$side for side: Must be :left or :right")
    end
end

function right_kernel(x::fpMatrix)
    z = zero_matrix(base_ring(x), ncols(x), max(nrows(x), ncols(x)))
    n = ccall((:nmod_mat_nullspace, libflint), Int, (Ref{fpMatrix}, Ref{fpMatrix}), z, x)
    return n, z
end

function left_kernel(x::fpMatrix)
    n, M = right_kernel(transpose(x))
    return n, transpose(M)
end

@doc raw"""
    left_kernel(a::ZZMatrix) -> Int, ZZMatrix

It returns a tuple $(n, M)$ where $M$ is a matrix whose rows generate
the kernel of $a$ and $n$ is the rank of the kernel.
"""
function left_kernel(x::ZZMatrix)
    if nrows(x) == 0
        return 0, zero(x, 0, 0)
    end
    x1 = transpose(hnf(transpose(x)))
    H, U = hnf_with_transform(x1)
    i = 1
    for outer i in 1:nrows(H)
        if is_zero_row(H, i)
            break
        end
    end
    if is_zero_row(H, i)
        return nrows(U) - i + 1, view(U, i:nrows(U), 1:ncols(U))
    else
        return 0, zero_matrix(FlintZZ, 0, ncols(U))
    end
end

right_kernel(M::MatElem) = nullspace(M)

function left_kernel(M::MatElem)
    rk, M1 = nullspace(transpose(M))
    return rk, transpose(M1)
end

function right_kernel(x::ZZMatrix)
    n, M = left_kernel(transpose(x))
    return n, transpose(M)
end

function right_kernel(M::zzModMatrix)
    R = base_ring(M)
    if is_prime(modulus(R))
        k = zero_matrix(R, ncols(M), ncols(M))
        n = ccall((:nmod_mat_nullspace, libflint), Int, (Ref{zzModMatrix}, Ref{zzModMatrix}), k, M)
        return n, k
    end

    H = hcat(transpose(M), identity_matrix(R, ncols(M)))
    if nrows(H) < ncols(H)
        H = vcat(H, zero_matrix(R, ncols(H) - nrows(H), ncols(H)))
    end
    howell_form!(H)
    nr = 1
    while nr <= nrows(H) && !is_zero_row(H, nr)
        nr += 1
    end
    nr -= 1
    h = sub(H, 1:nr, 1:nrows(M))
    for i = 1:nrows(h)
        if is_zero_row(h, i)
            k = sub(H, i:nrows(h), nrows(M)+1:ncols(H))
            return nrows(k), transpose(k)
        end
    end
    return 0, zero_matrix(R, nrows(M), 0)
end

function left_kernel(a::zzModMatrix)
    n, M = right_kernel(transpose(a))
    return n, transpose(M)
end

function right_kernel(M::ZZModMatrix)
    R = base_ring(M)
    N = hcat(transpose(M), identity_matrix(R, ncols(M)))
    if nrows(N) < ncols(N)
        N = vcat(N, zero_matrix(R, ncols(N) - nrows(N), ncols(N)))
    end
    howell_form!(N)
    H = N
    nr = 1
    while nr <= nrows(H) && !is_zero_row(H, nr)
        nr += 1
    end
    nr -= 1
    h = sub(H, 1:nr, 1:nrows(M))
    for i = 1:nrows(h)
        if is_zero_row(h, i)
            k = sub(H, i:nrows(h), nrows(M)+1:ncols(H))
            return nrows(k), transpose(k)
        end
    end
    return 0, zero_matrix(R, nrows(M), 0)
end

function left_kernel(a::ZZModMatrix)
    n, M = right_kernel(transpose(a))
    return n, transpose(M)
end

################################################################################
#
#  Kernel over different rings
#
################################################################################

@doc raw"""
kernel(a::MatrixElem{T}, R::Ring; side::Symbol = :right) -> n, MatElem{elem_type(R)}

It returns a tuple $(n, M)$, where $n$ is the rank of the kernel over $R$ and $M$ is a basis for it. If side is $:right$ or not
specified, the right kernel is computed. If side is $:left$, the left kernel is computed.
"""
function kernel(M::MatrixElem, R::Ring; side::Symbol=:right)
    MP = change_base_ring(R, M)
    return kernel(MP, side=side)
end

################################################################################
#
#  Diagonal (block) matrix creation
#
################################################################################

@doc raw"""
    diagonal_matrix(x::T...) where T <: RingElem -> MatElem{T}
    diagonal_matrix(x::Vector{T}) where T <: RingElem -> MatElem{T}
    diagonal_matrix(Q, x::Vector{T}) where T <: RingElem -> MatElem{T}

Returns a diagonal matrix whose diagonal entries are the elements of $x$.

# Examples

```jldoctest
julia> diagonal_matrix(QQ(1), QQ(2))
[1   0]
[0   2]

julia> diagonal_matrix([QQ(3), QQ(4)])
[3   0]
[0   4]

julia> diagonal_matrix(QQ, [5, 6])
[5   0]
[0   6]
```
"""
function diagonal_matrix(R::Ring, x::Vector{<:RingElement})
    x = R.(x)
    M = zero_matrix(R, length(x), length(x))
    for i = 1:length(x)
        M[i, i] = x[i]
    end
    return M
end

function diagonal_matrix(x::T, xs::T...) where {T<:RingElem}
    return diagonal_matrix(collect((x, xs...)))
end

diagonal_matrix(x::Vector{<:RingElement}) = diagonal_matrix(parent(x[1]), x)

@doc raw"""
    diagonal_matrix(x::Vector{T}) where T <: MatElem -> MatElem

Returns a block diagonal matrix whose diagonal blocks are the matrices in $x$.
"""
function diagonal_matrix(x::Vector{T}) where {T<:MatElem}
    return cat(x..., dims=(1, 2))::T
end

function diagonal_matrix(x::T, xs::T...) where {T<:MatElem}
    return cat(x, xs..., dims=(1, 2))::T
end

function diagonal_matrix(R::Ring, x::Vector{<:MatElem})
    if length(x) == 0
        return zero_matrix(R, 0, 0)
    end
    x = [change_base_ring(R, i) for i in x]
    return diagonal_matrix(x)
end

################################################################################
#
#  Reduce the entries of a matrix modulo p
#
################################################################################

@doc raw"""
    mod!(M::ZZMatrix, p::ZZRingElem)

Reduces every entry modulo $p$ in-place, i.e. applies the mod function to every entry.
Positive residue system.
"""
function mod!(M::ZZMatrix, p::ZZRingElem)
    GC.@preserve M begin
        for i = 1:nrows(M)
            for j = 1:ncols(M)
                z = ccall((:fmpz_mat_entry, libflint), Ptr{ZZRingElem}, (Ref{ZZMatrix}, Int, Int), M, i - 1, j - 1)
                ccall((:fmpz_mod, libflint), Nothing, (Ptr{ZZRingElem}, Ptr{ZZRingElem}, Ref{ZZRingElem}), z, z, p)
            end
        end
    end
    return nothing
end

@doc raw"""
    mod(M::ZZMatrix, p::ZZRingElem) -> ZZMatrix

Reduces every entry modulo $p$, i.e. applies the mod function to every entry.
"""
function mod(M::ZZMatrix, p::ZZRingElem)
    N = deepcopy(M)
    mod!(N, p)
    return N
end

export mod_sym, mod_sym!

@doc raw"""
    mod_sym!(M::ZZMatrix, p::ZZRingElem)

Reduces every entry modulo $p$ in-place, into the symmetric residue system.
"""
function mod_sym!(M::ZZMatrix, B::ZZRingElem)
    @assert !iszero(B)
    ccall((:fmpz_mat_scalar_smod, libflint), Nothing, (Ref{ZZMatrix}, Ref{ZZMatrix}, Ref{ZZRingElem}), M, M, B)
end
mod_sym!(M::ZZMatrix, B::Integer) = mod_sym!(M, ZZRingElem(B))

@doc raw"""
    mod_sym(M::ZZMatrix, p::ZZRingElem) -> ZZMatrix

Reduces every entry modulo $p$ into the symmetric residue system.
"""
function mod_sym(M::ZZMatrix, B::ZZRingElem)
    N = zero_matrix(FlintZZ, nrows(M), ncols(M))
    ccall((:fmpz_mat_scalar_smod, libflint), Nothing, (Ref{ZZMatrix}, Ref{ZZMatrix}, Ref{ZZRingElem}), N, M, B)
    return N
end
mod_sym(M::ZZMatrix, B::Integer) = mod_sym(M, ZZRingElem(B))


@doc raw"""
    mod_sym!(A::Generic.Mat{nf_elem}, m::ZZRingElem)

Inplace: reduce all entries of $A$ modulo $m$, into the symmetric residue system.
"""
function mod_sym!(A::Generic.Mat{nf_elem}, m::ZZRingElem)
    for i = 1:nrows(A)
        for j = 1:ncols(A)
            mod_sym!(A[i, j], m)
        end
    end
end

################################################################################
#
#  Special map entries
#
################################################################################

function map_entries(R::zzModRing, M::ZZMatrix)
    MR = zero_matrix(R, nrows(M), ncols(M))
    ccall((:fmpz_mat_get_nmod_mat, libflint), Cvoid, (Ref{zzModMatrix}, Ref{ZZMatrix}), MR, M)
    return MR
end

################################################################################
#
#  Concatenation of matrices
#
################################################################################

@doc raw"""
vcat(A::Vector{Generic.Mat}) -> Generic.Mat
vcat(A::Vector{ZZMatrix}) -> ZZMatrix

Forms a big matrix by vertically concatenating the matrices in $A$.
All component matrices need to have the same number of columns.
"""
function Base.vcat(A::Vector{T}) where {S<:RingElem,T<:MatElem{S}}
    if any(x -> ncols(x) != ncols(A[1]), A)
        error("Matrices must have same number of columns")
    end
    M = zero_matrix(base_ring(A[1]), sum(nrows, A), ncols(A[1]))
    s = 0
    for i = A
        for j = 1:nrows(i)
            for k = 1:ncols(i)
                M[s+j, k] = i[j, k]
            end
        end
        s += nrows(i)
    end
    return M
end

function Base.vcat(A::Vector{ZZMatrix})
    if any(x -> ncols(x) != ncols(A[1]), A)
        error("Matrices must have same number of columns")
    end
    M = zero_matrix(base_ring(A[1]), sum(nrows, A), ncols(A[1]))
    s = 0
    for i = A
        for j = 1:nrows(i)
            for k = 1:ncols(i)
                M[s+j, k] = i[j, k]
            end
        end
        s += nrows(i)
    end
    return M
end

function Base.vcat(A::Vector{zzModMatrix})
    if any(x -> ncols(x) != ncols(A[1]), A)
        error("Matrices must have same number of columns")
    end
    M = zero_matrix(base_ring(A[1]), sum(nrows, A), ncols(A[1]))
    s = 0
    for i = A
        for j = 1:nrows(i)
            for k = 1:ncols(i)
                M[s+j, k] = i[j, k]
            end
        end
        s += nrows(i)
    end
    return M
end

function Base.vcat(A::MatElem...)
    r = nrows(A[1])
    c = ncols(A[1])
    R = base_ring(A[1])
    for i = 2:length(A)
        @assert ncols(A[i]) == c
        @assert base_ring(A[i]) == R
        r += nrows(A[i])
    end
    X = zero_matrix(R, r, c)
    o = 1
    for i = 1:length(A)
        for j = 1:nrows(A[i])
            X[o, :] = A[i][j, :]
            o += 1
        end
    end
    return X
end

function Base.hcat(A::Vector{T}) where {S<:RingElem,T<:MatElem{S}}
    if any(x -> nrows(x) != nrows(A[1]), A)
        error("Matrices must have same number of rows")
    end
    M = zero_matrix(base_ring(A[1]), nrows(A[1]), sum(ncols, A))
    s = 0
    for i = A
        for j = 1:ncols(i)
            for k = 1:nrows(i)
                M[k, s+j] = i[k, j]
            end
        end
        s += ncols(i)
    end
    return M
end

function Base.hcat(A::MatElem...)
    r = nrows(A[1])
    c = ncols(A[1])
    R = base_ring(A[1])
    for i = 2:length(A)
        @assert nrows(A[i]) == r
        @assert base_ring(A[i]) == R
        c += ncols(A[i])
    end
    X = zero_matrix(R, r, c)
    o = 1
    for i = 1:length(A)
        for j = 1:ncols(A[i])
            X[:, o] = A[i][:, j]
            o += 1
        end
    end
    return X
end

function Base.cat(A::MatElem...; dims)
    @assert dims == (1, 2) || isa(dims, Int)

    if isa(dims, Int)
        if dims == 1
            return hcat(A...)
        elseif dims == 2
            return vcat(A...)
        else
            error("dims must be 1, 2, or (1,2)")
        end
    end

    local X
    for i = 1:length(A)
        if i == 1
            X = hcat(A[1], zero_matrix(base_ring(A[1]), nrows(A[1]), sum(Int[ncols(A[j]) for j = 2:length(A)])))
        else
            X = vcat(X, hcat(zero_matrix(base_ring(A[1]), nrows(A[i]), sum(ncols(A[j]) for j = 1:i-1)), A[i], zero_matrix(base_ring(A[1]), nrows(A[i]), sum(Int[ncols(A[j]) for j = i+1:length(A)]))))
        end
    end
    return X
end

#= seems to be in AA now
function Base.hvcat(rows::Tuple{Vararg{Int}}, A::MatElem...)
  B = hcat([A[i] for i=1:rows[1]]...)
  o = rows[1]
  for j=2:length(rows)
    C = hcat([A[i+o] for i=1:rows[j]]...)
    o += rows[j]
    B = vcat(B, C)
  end
  return B
end
=#

export hnf!

function hnf!(x::ZZMatrix)
    ccall((:fmpz_mat_hnf, libflint), Nothing, (Ref{ZZMatrix}, Ref{ZZMatrix}), x, x)
    return x
end

################################################################################
#
#  Smith normal form with trafo
#
################################################################################

#=
g, e,f = gcdx(a, b)
U = [1 0 ; -divexact(b, g)*f 1]*[1 1; 0 1];
V = [e -divexact(b, g) ; f divexact(a, g)];

then U*[ a 0; 0 b] * V = [g 0 ; 0 l]
=#
@doc raw"""
    snf_with_transform(A::ZZMatrix, l::Bool = true, r::Bool = true) -> ZZMatrix, ZZMatrix, ZZMatrix

Given some integer matrix $A$, compute the Smith normal form (elementary
divisor normal form) of $A$. If `l` and/ or `r` are true, then the corresponding
left and/ or right transformation matrices are computed as well.
"""
function snf_with_transform(A::ZZMatrix, l::Bool=true, r::Bool=true)
    if r
        R = identity_matrix(FlintZZ, ncols(A))
    end

    if l
        L = identity_matrix(FlintZZ, nrows(A))
    end
    # TODO: if only one trafo is required, start with the HNF that does not
    #       compute the trafo
    #       Rationale: most of the work is on the 1st HNF..
    S = deepcopy(A)
    while !is_diagonal(S)
        if l
            S, T = hnf_with_transform(S)
            L = T * L
        else
            S = hnf!(S)
        end

        if is_diagonal(S)
            break
        end
        if r
            S, T = hnf_with_transform(transpose(S))
            R = T * R
        else
            S = hnf!(transpose(S))
        end
        S = transpose(S)
    end
    #this is probably not really optimal...
    for i = 1:min(nrows(S), ncols(S))
        if S[i, i] == 1
            continue
        end
        for j = i+1:min(nrows(S), ncols(S))
            if S[j, j] == 0
                continue
            end
            if S[i, i] != 0 && S[j, j] % S[i, i] == 0
                continue
            end
            g, e, f = gcdx(S[i, i], S[j, j])
            a = divexact(S[i, i], g)
            S[i, i] = g
            b = divexact(S[j, j], g)
            S[j, j] *= a
            if l
                # U = [1 0; -b*f 1] * [ 1 1; 0 1] = [1 1; -b*f -b*f+1]
                # so row i and j of L will be transformed. We do it naively
                # those 2x2 transformations of 2 rows should be a c-primitive
                # or at least a Nemo/Hecke primitive
                for k = 1:ncols(L)
                    x = -b * f
                    #          L[i,k], L[j,k] = L[i,k]+L[j,k], x*L[i,k]+(x+1)*L[j,k]
                    L[i, k], L[j, k] = L[i, k] + L[j, k], x * (L[i, k] + L[j, k]) + L[j, k]
                end
            end
            if r
                # V = [e -b ; f a];
                # so col i and j of R will be transformed. We do it naively
                # careful: at this point, R is still transposed
                for k = 1:nrows(R)
                    R[i, k], R[j, k] = e * R[i, k] + f * R[j, k], -b * R[i, k] + a * R[j, k]
                end
            end
        end
    end

    # It might be the case that S was diagonal with negative diagonal entries.
    for i in 1:min(nrows(S), ncols(S))
        if S[i, i] < 0
            if l
                multiply_row!(L, ZZRingElem(-1), i)
            end
            S[i, i] = -S[i, i]
        end
    end

    if l
        if r
            return S, L, transpose(R)
        else
            # last is dummy
            return S, L, L
        end
    elseif r
        # second is dummy
        return S, R, transpose(R)
    else
        # last two are dummy
        return S, S, S
    end
end

################################################################################
#
#  IsUpper\Lower triangular
#
################################################################################

function is_upper_triangular(M::MatElem)
    n = nrows(M)
    for i = 2:n
        for j = 1:min(i - 1, ncols(M))
            if !iszero(M[i, j])
                return false
            end
        end
    end
    return true
end

function is_upper_triangular(M::ZZMatrix)
    GC.@preserve M begin
        for i = 2:nrows(M)
            for j = 1:min(i - 1, ncols(M))
                t = ccall((:fmpz_mat_entry, libflint), Ptr{ZZRingElem}, (Ref{ZZMatrix}, Int, Int), M, i - 1, j - 1)
                fl = ccall((:fmpz_is_zero, libflint), Bool, (Ref{ZZRingElem},), t)
                if !fl
                    return false
                end
            end
        end
    end
    return true
end

function is_lower_triangular(M::ZZMatrix)
    GC.@preserve M begin
        for i = 1:nrows(M)
            for j = i+1:ncols(M)
                t = ccall((:fmpz_mat_entry, libflint), Ptr{ZZRingElem}, (Ref{ZZMatrix}, Int, Int), M, i - 1, j - 1)
                fl = ccall((:fmpz_is_zero, libflint), Bool, (Ref{ZZRingElem},), t)
                if !fl
                    return false
                end
            end
        end
    end
    return true
end

function is_lower_triangular(M::MatElem)
    for i = 1:nrows(M)
        for j = i+1:ncols(M)
            if !iszero(M[i, j])
                return false
            end
        end
    end
    return true
end

export compare_index

#Returns a positive integer if A[i, j] > b, negative if A[i, j] < b, 0 otherwise
function compare_index(A::ZZMatrix, i::Int, j::Int, b::ZZRingElem)
    a = ccall((:fmpz_mat_entry, libflint), Ptr{ZZRingElem}, (Ref{ZZMatrix}, Int, Int), A, i - 1, j - 1)
    return ccall((:fmpz_cmp, libflint), Int32, (Ptr{ZZRingElem}, Ref{ZZRingElem}), a, b)
end

function round!(b::ZZMatrix, a::arb_mat)
    s = size(a)
    for i = 1:s[1]
        for j = 1:s[2]
            b[i, j] = round(ZZRingElem, a[i, j])
        end
    end
    return b
end

export shift!

function shift!(g::ZZMatrix, l::Int)
    for i = 1:nrows(g)
        for j = 1:ncols(g)
            z = ccall((:fmpz_mat_entry, libflint), Ptr{ZZRingElem}, (Ref{ZZMatrix}, Int, Int), g, i - 1, j - 1)
            if l > 0
                ccall((:fmpz_mul_2exp, libflint), Nothing, (Ptr{ZZRingElem}, Ptr{ZZRingElem}, Int), z, z, l)
            else
                ccall((:fmpz_tdiv_q_2exp, libflint), Nothing, (Ptr{ZZRingElem}, Ptr{ZZRingElem}, Int), z, z, -l)
            end
        end
    end
    return g
end

################################################################################
#
#  Is diagonal
#
################################################################################

@doc raw"""
is_diagonal(A::Mat)

Tests if $A$ is diagonal.
"""
function is_diagonal(A::MatElem)
    for i = 1:ncols(A)
        for j = 1:nrows(A)
            if i != j && !iszero(A[j, i])
                return false
            end
        end
    end
    return true
end

function is_diagonal(A::ZZMatrix)
    for i = 1:ncols(A)
        for j = 1:nrows(A)
            if i != j
                t = ccall((:fmpz_mat_entry, libflint), Ptr{ZZRingElem}, (Ref{ZZMatrix}, Int, Int), A, j - 1, i - 1)
                fl = ccall((:fmpz_is_zero, libflint), Bool, (Ref{ZZRingElem},), t)
                if !fl
                    return false
                end
            end
        end
    end
    return true
end

################################################################################
#
#  Diagonal
#
################################################################################

@doc raw"""
    diagonal(A::Mat{T}) -> Vector{T}

Returns the diagonal of `A` as an array.
"""
diagonal(A::MatrixElem{T}) where {T} = T[A[i, i] for i in 1:nrows(A)]

################################################################################
#
#  Product of the diagonal entries
#
################################################################################

export prod_diagonal

function prod_diagonal(A::ZZMatrix)
    a = one(ZZRingElem)
    GC.@preserve a A begin
        for i = 1:nrows(A)
            b = ccall((:fmpz_mat_entry, libflint), Ptr{ZZRingElem}, (Ref{ZZMatrix}, Int, Int), A, i - 1, i - 1)
            ccall((:fmpz_mul, libflint), Nothing, (Ref{ZZRingElem}, Ref{ZZRingElem}, Ptr{ZZRingElem}), a, a, b)
        end
    end
    return a
end

function prod_diagonal(A::MatrixElem{T}) where {T}
    @assert nrows(A) == ncols(A)
    return prod(T[A[i, i] for i = 1:nrows(A)])
end

export reduce_mod!

@doc raw"""
    reduce_mod!(A::MatElem{T}, B::MatElem{T}) where T <: FieldElem

For a reduced row echelon matrix $B$, reduce $A$ modulo $B$, i.e. all the pivot
columns will be zero afterwards.
"""
function reduce_mod!(A::MatElem{T}, B::MatElem{T}) where {T<:FieldElem}
    if is_rref(B)
        scale = false
    else
        scale = true
    end

    for h = 1:nrows(A)
        j = 1
        for i = 1:nrows(B)
            while iszero(B[i, j])
                j += 1
            end
            if scale
                A[h, :] -= A[h, j] * (inv(B[i, j]) * B[i, :])
            else
                A[h, :] -= A[h, j] * B[i, :]
            end
        end
    end
    return A
end

@doc raw"""
    reduce_mod(A::MatElem{T}, B::MatElem{T}) where T <: FieldElem -> MatElem

For a reduced row echelon matrix $B$, reduce $A$ modulo $B$, i.e. all the pivot
columns will be zero afterwards.
"""
function reduce_mod(A::MatElem{T}, B::MatElem{T}) where {T<:FieldElem}
    C = deepcopy(A)
    reduce_mod!(C, B)
    return C
end

################################################################################
#
#  Function to convert a matrix to array
#
################################################################################

function to_array(M::QQMatrix)
    A = Vector{QQFieldElem}(undef, ncols(M) * nrows(M))
    for i = 1:nrows(M)
        for j = 1:ncols(M)
            A[(i-1)*ncols(M)+j] = M[i, j]
        end
    end
    return A
end

################################################################################
#
#  Minpoly and Charpoly
#
################################################################################

function minpoly(M::MatElem)
    k = base_ring(M)
    kx, x = polynomial_ring(k, cached=false)
    return minpoly(kx, M)
end

function charpoly(M::MatElem)
    k = base_ring(M)
    kx, x = polynomial_ring(k, cached=false)
    return charpoly(kx, M)
end

###############################################################################
#
#  Sub
#
###############################################################################

function sub(M::MatElem, rows::Vector{Int}, cols::Vector{Int})
    N = zero_matrix(base_ring(M), length(rows), length(cols))
    for i = 1:length(rows)
        for j = 1:length(cols)
            N[i, j] = M[rows[i], cols[j]]
        end
    end
    return N
end

function sub(M::MatElem{T}, r::UnitRange{<:Integer}, c::UnitRange{<:Integer}) where {T}
    z = similar(M, length(r), length(c))
    for i in 1:length(r)
        for j in 1:length(c)
            z[i, j] = M[r[i], c[j]]
        end
    end
    return z
end

################################################################################
#
#  Map Entries
#
################################################################################

function map_entries(F::fpField, M::ZZMatrix)
    MR = zero_matrix(F, nrows(M), ncols(M))
    ccall((:fmpz_mat_get_nmod_mat, libflint), Cvoid, (Ref{fpMatrix}, Ref{ZZMatrix}), MR, M)
    return MR
end

function map_entries(R::ZZModRing, M::ZZMatrix)
    N = zero_matrix(R, nrows(M), ncols(M))
    GC.@preserve M N begin
        for i = 1:nrows(M)
            for j = 1:ncols(M)
                m = ccall((:fmpz_mat_entry, libflint), Ptr{ZZRingElem}, (Ref{ZZMatrix}, Int, Int), M, i - 1, j - 1)
                n = ccall((:fmpz_mod_mat_entry, libflint), Ptr{ZZRingElem}, (Ref{ZZModMatrix}, Int, Int), N, i - 1, j - 1)
                ccall((:fmpz_mod, libflint), Nothing, (Ptr{ZZRingElem}, Ptr{ZZRingElem}, Ref{ZZRingElem}), n, m, R.n)
            end
        end
    end
    return N
end
