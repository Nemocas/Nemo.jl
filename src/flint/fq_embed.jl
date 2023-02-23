###############################################################################
#
#   fq_embed.jl : Flint finite fields embeddings
#
###############################################################################

###############################################################################
#
#   Linear factor
#
###############################################################################

function linear_factor(x::FqPolyRepPolyRingElem)
    y = parent(x)()
    ccall((:fq_poly_factor_split_single, libflint), Nothing,
          (Ref{FqPolyRepPolyRingElem}, Ref{FqPolyRepPolyRingElem}, Ref{FqPolyRepField}),
           y, x, base_ring(x))
    return y
end

###############################################################################
#
#   Naive functions
#
###############################################################################

function embed_gens(k::FqPolyRepField, K::FqPolyRepField)
    a = k()
    b = K()
    p = ZZRingElem(characteristic(k))::ZZRingElem
    R = GF(p)
    PR = polynomial_ring(R, "T")[1]
    P = PR()

    ccall((:fq_embed_gens, libflint), Nothing,
          (Ref{FqPolyRepFieldElem}, Ref{FqPolyRepFieldElem}, Ref{FpPolyRingElem}, Ref{FqPolyRepField},
                                                         Ref{FqPolyRepField}),
          a, b, P, k, K)

    return a, b, P
end

function embed_matrices(k::FqPolyRepField, K::FqPolyRepField)

    m, n = degree(k), degree(K)
    if m == n
        T1, T2 = modulus(k), modulus(K)
        if T1 == T2
            s1 = identity_matrix(base_ring(T1), n)
            s2 = s1
            return s1, s2
        end
    end

    a, b, P = embed_gens(k, K)
    R = base_ring(P)
    s1 = zero_matrix(R, n, m)
    s2 = zero_matrix(R, m, n)

    ccall((:fq_embed_matrices, libflint), Nothing,
          (Ref{FpMatrix}, Ref{FpMatrix}, Ref{FqPolyRepFieldElem}, Ref{FqPolyRepField},
                             Ref{FqPolyRepFieldElem}, Ref{FqPolyRepField}, Ref{FpPolyRingElem}),
          s1, s2, a, k, b, K, P)
    return s1, s2
end

function embed_matrices_pre(a::FqPolyRepFieldElem, b::FqPolyRepFieldElem, P::FpPolyRingElem)
    k = parent(a)
    K = parent(b)
    m, n = degree(k), degree(K)
    R = base_ring(P)
    s1 = zero_matrix(R, n, m)
    s2 = zero_matrix(R, m, n)

    ccall((:fq_embed_matrices, libflint), Nothing,
          (Ref{FpMatrix}, Ref{FpMatrix}, Ref{FqPolyRepFieldElem}, Ref{FqPolyRepField},
                              Ref{FqPolyRepFieldElem}, Ref{FqPolyRepField}, Ref{FpPolyRingElem}),
           s1, s2, a, k, b, K, P)
    return s1, s2
end

# dirty: internally in flint an fq_struct is just an fmpz_poly_struct
function setcoeff!(x::FqPolyRepFieldElem, j::Int, c::ZZRingElem)
    ccall((:fmpz_poly_set_coeff_fmpz, libflint), Nothing,
          (Ref{FqPolyRepFieldElem}, Int, Ref{ZZRingElem}), x, j, c)
end

function embed_pre_mat(x::FqPolyRepFieldElem, K::FqPolyRepField, M::FpMatrix)

    d = degree(parent(x))
    col = zero_matrix(base_ring(M), d, 1)

    for j in 0:(d - 1)
        col[j + 1, 1] = coeff(x, j)
    end

    product = M*col
    res = K()

    for j in degree(K):-1:1
        setcoeff!(res, j - 1, data(product[j, 1]))
    end

    return res
end

################################################################################
#
#   Embedding a polynomial
#
################################################################################

function embed_polynomial(P::FqPolyRepPolyRingElem, f::FinFieldMorphism)
    S = polynomial_ring(codomain(f), "T")[1]
    return S([f(coeff(P, j)) for j in 0:degree(P)])
end
