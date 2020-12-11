################################################################################
#
#   FinFieldsLattices.jl : Finite Fields Lattices
#
################################################################################

export embed, preimage, preimage_map, hom

################################################################################
#
#   Over/Sub fields
#
################################################################################

_overfields(k::FinField) = k.overfields
_subfields(k::FinField) = k.subfields

@doc Markdown.doc"""
    AddOverfield!(F::T, f::FinFieldMorphism{T}) where T <: FinField

Add an overfield to $F$, represented by a morphism $f: F\to G$ where
$G$ is the codomain of $f$.
"""
function AddOverfield!(F::T, f::FinFieldMorphism{T, T}) where T <: FinField

    d = degree(codomain(f))
    over = _overfields(F)

    if haskey(over, d)
        lm = over[d]
        found = false
        for j = 1:length(lm)
            if lm[j] == f
                found = true
                break
            end
        end
        if !found
            push!(over[d], f)
        end
    else
        a = FinFieldMorphism{T, T}[f]
        over[d] = a
    end
    return nothing
end

@doc Markdown.doc"""
    AddSubfield!(F::T, f::FinFieldMorphism{T}) where T <: FinField

Add a subfield to $F$, represented by a morphism $f: G\to F$ where
$G$ is the domain of $f$.
"""
function AddSubfield!(F::T, f::FinFieldMorphism{T, T}) where T <: FinField

    d = degree(domain(f))
    sub = _subfields(F)

    if haskey(sub, d)
        lm = sub[d]
        found = false
        for j = 1:length(lm)
            if lm[j] == f
                found = true
                break
            end
        end
        if !found
            push!(sub[d], f)
        end    
    else
        a = FinFieldMorphism{T, T}[f]
        sub[d] = a
    end
end


################################################################################
#
#   Root Finding (of a splitting polynomial, internal use only)
#
################################################################################

any_root(x::PolyElem) = -coeff(linear_factor(x), 0)

################################################################################
#
#   Minimal polynomial of the generator
#
################################################################################

function generator_minimal_polynomial(f::FinFieldMorphism)
    E = domain(f)
    F = codomain(f)
    d = div(degree(F), degree(E))
    q = order(E)
    conjs = Vector{elem_type(F)}(undef, d)
    conjs[1] = gen(F)
    for i = 2:d
        conjs[i] = conjs[i-1]^q
    end
    x = PolynomialRing(F, "x", cached = false)[2]
    g = prod(x-a for a in conjs)
    Ex = PolynomialRing(E, "x", cached = false)[1]
    coeffs = Vector{elem_type(E)}(undef, length(g))
    for i = 1:length(coeffs)
        coeffs[i] = preimage(f, coeff(g, i-1))
    end
    return Ex(coeffs)
end

################################################################################
#
#   Embedding
#
################################################################################

@doc Markdown.doc"""
    isembedded(k::T, K::T) where T <: FinField

If $k$ is embbeded in $K$, return the corresponding embedding.
"""
function isembedded(k::T, K::T) where T <: FinField

    dK = degree(K)
    dk = degree(k)
    subf = _subfields(K)
    if haskey(subf, dk)
        for f in subf[dk]
            if domain(f) == k && codomain(f) == K
                return f
            end
        end
    end
    
    # We look for an embedding that has k as domain and K as codomain
    ov = _overfields(k)
    if haskey(ov, dK)
        for f in ov[dK]
            if domain(f) == k && codomain(f) == K
                return f
            end
        end
    end
    return nothing
end

@doc Markdown.doc"""
    embed_any(k::T, K::T) where T <: FinField

Embed $k$ in $K$ without worrying about compatibility conditions.
"""
function embed_any(k::FqNmodFiniteField, K::FqNmodFiniteField)

    # We call the Flint algorithms directly, currently this is based on
    # factorization

    M, N = embed_matrices(k, K)
    f(x) = embed_pre_mat(x, K, M)
    inv(y) = embed_pre_mat(y, k, N)

    return FinFieldMorphism(k, K, f, inv)
end

function hom(k::FqNmodFiniteField, K::FqNmodFiniteField, a::fq_nmod)
    defPol = modulus(k)
    M, N = embed_matrices_pre(gen(k), a, defPol)
    f(x) = embed_pre_mat(x, K, M)
    g(y) = embed_pre_mat(y, k, N)
    morph = FinFieldMorphism(k, K, f, g)
    return morph
end

@doc Markdown.doc"""
    find_morphism(k::T, K::T) where T <: FinField

Returns a compatible embedding from $k$ to $K$.
"""
function find_morphism(k::T, K::T) where T <: FinField

    S = PolynomialRing(K, "T")[1]
    Q = S()
    needy = false
    m, n = degree(k), degree(K)

    # For each common subfield S of k and K, we compute the minimal polynomial
    # of the canonical generator of k over S, with coefficient seen in K and we
    # compute the gcd of all these polynomials 

    for l in keys(_subfields(k))
        if haskey(_subfields(K), l)
            f = _subfields(k)[l][1]
            g = _subfields(K)[l][1]
            P = map_coeffs(g, generator_minimal_polynomial(f))
            if needy
                Q = gcd(Q, P)
            else
                Q = P
            end
            needy = true
        end
    end

    # If there is at least one common subfield, we define the embedding from k
    # to K by sending the canonical generator of k to a root of the gcd
    # computed above

    if needy
        
        t = any_root(Q)
        morph = hom(k, K, t)

    # If there is no common subfield, there is no compatibility condition to
    # fulfill
    else
        morph = embed_any(k, K)
    end

    return morph
end

@doc Markdown.doc"""
    transitive_closure(f::FinFieldMorphism)

Compute the transitive closure.
"""
function transitive_closure(f::FinFieldMorphism{T, T}) where T

    k = domain(f)
    K = codomain(f)



    # _subfields

    subk = _subfields(k)
    subK = _subfields(K)

    # We go through all subfields of k and check if they are also subfields of
    # K, we add them if they are not
    ksubk = sort(collect(keys(subk)), rev = true)
    for d in ksubk
        if d == degree(k)
            continue
        end
        if !haskey(subK, d)
            for g in subk[d]
                t(y) = f(g(y))
                tinv(x) = inverse_fn(g)(inverse_fn(f)(x))
                phi = FinFieldMorphism(domain(g), K, t, tinv)

                AddSubfield!(K, phi)
                AddOverfield!(domain(g), phi)
            end
        else
            val = T[codomain(v) for v in subK[d]]
            
            for g in subk[d]
                if !(domain(g) in val)
                    t(y) = f(g(y))
                    tinv(x) = inverse_fn(g)(inverse_fn(f)(x))
                    phi = FinFieldMorphism(domain(g), K, t, tinv)

                    AddSubfield!(K, phi)
                    AddOverfield!(domain(g), phi)
               end
            end
        end
    end

    # Overfields

    ov = _overfields(K)

    # We call the same procedure on the overfields
    kov = sort(collect(keys(ov)), rev = true)
    for d in kov
        if d == degree(K)
            continue
        end
        for g in ov[d]
            transitive_closure(g)
        end
    end

    return nothing

end

@doc Markdown.doc"""
    intersections(k::T, K::T) where T <: FinField

For each subfield $S$ of $K$, embed $I$ in $S$ and $k$, where $I$ is the intersection
between $S$ and $k$.
"""
function intersections(k::T, K::T) where T <: FinField

    d = degree(k)
    subk = _subfields(k)
    subK = _subfields(K)
    needmore = true

    # We loop through the subfields of K and we have different cases
    ksubK = sort(collect(keys(subK)), rev = true)
    for l in ksubK
        c = gcd(d, l)

        # The intersection may be trivial, I = F_p
        if c == 1

        # I = k, so k is a subfield of S and we embed k in S
        # In that case, we finally have the embeddings k in S and S in K, so by
        # transitive closure we have k in K and we do not need more work
        elseif c == d
            for g in subK[l]
                embed(k, domain(g))
            end
            needmore = false

        # I = S, so S is a subfield of k, and we embed S in k
        elseif c == l
            for g in subK[l]
                embed(domain(g), k)
            end

        # If I is a subfield of k, we embed it in S
        elseif haskey(subk, c)
            L = domain(subk[c][1])
            for h in subK[l]
                embed(L, domain(h))
            end

        # If I is a subfield of K, we embed it in k and S
        elseif haskey(subK, c)
            L = domain(subK[c][1])
            embed(L, k)
            for h in subK[l]
                embed(L, domain(h))
            end

        # Otherwise it means that there is no field I around so we create one
        # and we embed it in k and S
        else
            p::Int = characteristic(k)
            kc, xc = FiniteField(p, c, string("x", c))
            embed(kc, k)
            for g in subK[l]
                embed(kc, domain(g))
            end
        end
    end

    # We return a boolean to tell if some more work needs to be done
    return needmore
end

@doc Markdown.doc"""
    embed(k::T, K::T) where T <: FinField

Embed $k$ in $K$, with some additionnal computations in order to satisfy
compatibility conditions with previous and future embeddings.
"""
function embed(k::T, K::T) where T <: FinField

    degree(K) % degree(k) != 0 && error("Embedding impossible")

    # Special cases of k == K or degree(k) == 1

    if k == K
        identity(x) = x
        morph = FinFieldMorphism(k, k, identity, identity)
        return morph

    elseif degree(k) == 1
        f(x) = K(coeff(x, 0))
        finv(y) = k(coeff(y, 0))
        morph = FinFieldMorphism(k, K, f, finv)
        return morph
    end

    # If k is already embedded in K, we return the corresponding embedding

    tr = isembedded(k, K)
    if tr != nothing
        return tr
    end

    # Prior to embed k in K, we compute the needed embeddings
    # The embedding might be computed during the process !

    needmore = intersections(k, K) # reccursive calls to embed

    # And, if the wanted embeddings has not been computed during the process, we
    # finally compute a compatible embedding

    if needmore 

        # We compute a compatible embedding
        morph = find_morphism(k, K)

        # We had it to the over and sub fields of k and K
        AddOverfield!(k, morph)
        AddSubfield!(K, morph)

        # We compute the transitive closure induced by the new embedding
        transitive_closure(morph)

        # And return the embedding
        return morph
    else

        # If the embedding has already been computing, we return it
        return isembedded(k, K)
    end
end

################################################################################
#
#   Preimage map
#
################################################################################

@doc Markdown.doc"""
    preimage_map(k::T, k::T) where T <: FinField

Computes the preimage map corresponding to the embedding of $k$ into $K$.
"""
function preimage_map(k::T, K::T) where T <: FinField
    f = embed(k, K)
    return preimage_map(f)
end

preimage(f::FinFieldMorphism{U, V}, x::T) where {U <: FinField, V<: FinField, T <: FinFieldElem} = preimage_map(f)(x)::elem_type(U)
