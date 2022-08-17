###############################################################################
#
#   fq_default_embed.jl : Flint finite fields embeddings
#
###############################################################################

function _as_fq_finite_field(F::FqDefaultFiniteField)
    return FlintFiniteField(modulus(F), :a; cached = false)[1]
end


function _unchecked_coerce!(z::fq_default, a::FqDefaultFiniteField, b::fq)
    x = fmpz_poly()
    ccall((:fq_get_fmpz_poly, libflint), Nothing,
         (Ref{fmpz_poly}, Ref{fq}, Ref{FqFiniteField}),
          x, b, parent(b))
    ccall((:fq_default_set_fmpz_poly, libflint), Nothing,
          (Ref{fq_default}, Ref{fmpz_poly}, Ref{FqDefaultFiniteField}),
          z, x, a)
end

###############################################################################
#
#   Linear factor
#
###############################################################################

function linear_factor(x::fq_default_poly)
    for (f, e) in factor(x)
        if degree(f) == 1
            return f
        end
    end
    error("unreachable")
end

###############################################################################
#
#   Naive functions
#
###############################################################################

function _fq_default_embed_gens(
    gen_sub::fq_default,
    gen_sup::fq_default,
    minpoly::gfp_fmpz_poly,
    sub_ctx::FqDefaultFiniteField,
    sup_ctx::FqDefaultFiniteField)

    sub_ctx1 = _as_fq_finite_field(sub_ctx)
    sup_ctx1 = _as_fq_finite_field(sup_ctx)

    gen_sub1 = zero(sub_ctx1)
    gen_sup1 = zero(sup_ctx1)

    ccall((:fq_embed_gens, libflint), Nothing,
          (Ref{fq}, Ref{fq}, Ref{gfp_fmpz_poly}, Ref{FqFiniteField}, Ref{FqFiniteField}),
          gen_sub1, gen_sup1, minpoly, sub_ctx1, sup_ctx1)

    _unchecked_coerce!(gen_sub, sub_ctx, gen_sub1)
    _unchecked_coerce!(gen_sup, sup_ctx, gen_sup1)
end



function embed_gens(k::FqDefaultFiniteField, K::FqDefaultFiniteField)
    a = k()
    b = K()
    p = fmpz(characteristic(k))::fmpz
    R = GF(p)
    PR = PolynomialRing(R, "T")[1]
    P = PR()

    _fq_default_embed_gens(a, b, P, k, K)
    return a, b, P
end

function _fq_default_embed_matrices(
    emb::gfp_fmpz_mat,
    pro::gfp_fmpz_mat,
    gen_sub::fq_default,
    sub_ctx::FqDefaultFiniteField,
    gen_sup::fq_default,
    sup_ctx::FqDefaultFiniteField,
    gen_minpoly::gfp_fmpz_poly
)

    sub_ctx1 = _as_fq_finite_field(sub_ctx)
    sup_ctx1 = _as_fq_finite_field(sup_ctx)

    ccall((:fq_embed_matrices, libflint), Nothing,
          (Ref{gfp_fmpz_mat}, Ref{gfp_fmpz_mat}, Ref{fq}, Ref{FqFiniteField},
                             Ref{fq}, Ref{FqFiniteField}, Ref{gfp_fmpz_poly}),
          emb, pro, _unchecked_coerce(sub_ctx1, gen_sub), sub_ctx1,
                    _unchecked_coerce(sup_ctx1, gen_sup), sup_ctx1, gen_minpoly)
end

function embed_matrices(k::FqDefaultFiniteField, K::FqDefaultFiniteField)
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
    _fq_default_embed_matrices(s1, s2, a, k, b, K, P)
    return s1, s2
end

function embed_matrices_pre(a::fq_default, b::fq_default, P::gfp_fmpz_poly)
    k = parent(a)
    K = parent(b)
    m, n = degree(k), degree(K)
    R = base_ring(P)
    s1 = zero_matrix(R, n, m)
    s2 = zero_matrix(R, m, n)
    _fq_default_embed_matrices(s1, s2, a, k, b, K, P)
    return s1, s2
end

function embed_pre_mat(x::fq_default, K::FqDefaultFiniteField, M::gfp_fmpz_mat)

    d = degree(parent(x))
    col = zero_matrix(base_ring(M), d, 1)

    for j in 0:(d - 1)
        col[j + 1, 1] = coeff(x, j)
    end

    product = M*col
    res = fq_default(K, fmpz_poly([data(product[j, 1]) for j in 1:degree(K)]))
    return res
end

################################################################################
#
#   Embedding a polynomial
#
################################################################################

function embed_polynomial(P::fq_default_poly, f::FinFieldMorphism)
    S = PolynomialRing(codomain(f), "T")[1]
    return S([f(coeff(P, j)) for j in 0:degree(P)])
end
