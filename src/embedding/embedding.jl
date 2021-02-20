################################################################################
#
#   FinFieldsLattices.jl : Finite Fields Lattices
#
################################################################################

export embed, preimage, preimage_map

################################################################################
#
#   Over/Sub fields
#
################################################################################

overfields(k::FinField) = k.overfields
subfields(k::FinField) = k.subfields

@doc Markdown.doc"""
    AddOverfield!(F::T, f::FinFieldMorphism{T}) where T <: FinField

Add an overfield to $F$, represented by a morphism $f: F\to G$ where
$G$ is the codomain of $f$.
"""
function AddOverfield!(F::T, f::FinFieldMorphism{T}) where T <: FinField

    d = degree(codomain(f))
    over = overfields(F)

    if haskey(over, d)
        push!(over[d], f)
    else
        a = FinFieldMorphism{T}[f]
        over[d] = a
    end
end

@doc Markdown.doc"""
    AddSubfield!(F::T, f::FinFieldMorphism{T}) where T <: FinField

Add a subfield to $F$, represented by a morphism $f: G\to F$ where
$G$ is the domain of $f$.
"""
function AddSubfield!(F::T, f::FinFieldMorphism{T}) where T <: FinField

    d = degree(domain(f))
    sub = subfields(F)

    if haskey(sub, d)
        push!(sub[d], f)
    else
        a = FinFieldMorphism{T}[f]
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

@doc Markdown.doc"""
    berlekamp_massey(a::Array{Y, 1}, n::Int) where Y <: FieldElem

Compute the minimal polynomial of a linear recurring sequence $a$.
"""
function berlekamp_massey(a::Array{Y, 1}, n::Int) where Y <: FieldElem

  S, T = PolynomialRing(parent(a[1]), "T")
  m = 2*n - 1
  R0 = T^(2*n)
  R1 = S(reverse(a))
  V0 = S(0)
  V1 = S(1)

  while n <= degree(R1)
    Q, R = divrem(R0,R1)
    V = V0-Q*V1
    V0 = V1
    V1 = V
    R0 = R1
    R1 = R
  end

  return V1*lead(V1)^(-1)
end

@doc Markdown.doc"""
    generator_minimum_polynomial(f::FinFieldMorphism)

Compute the minimal polynomial of the generator of the codomain
of $f$ over the domain of $f$.
"""
function generator_minimum_polynomial(f::FinFieldMorphism)

    E = domain(f)
    F = codomain(f)
    d::Int = div(degree(F), degree(E))
    b = 2*d

    # We define `sec`, the preimage of the morphism `f`
    
    sec = inverse_fn(f)
    x = gen(F)
    y = one(F)

    # We compute the recurring sequence sec(x^j) for j from 0 to 2d - 1

    A = Array{typeof(x)}(undef, b)

    for j in 1:(b - 1)
        A[j] = sec(y)
        y *= x
    end
    A[b] = sec(y)

    # We apply Berlekamp Massey algorithm to the sequence

    return berlekamp_massey(A, d)
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

    d = degree(K)
    ov = overfields(k)

    # We look for an embedding that has k as domain and K as codomain

    if haskey(ov, d)
        for f in ov[d]
            if domain(f) == k && codomain(f) == K
                return f
            end
        end
    end
end

@doc Markdown.doc"""
    embed_any(k::T, K::T) where T <: FinField

Embed $k$ in $K$ without worrying about compatibility conditions.
"""
function embed_any(k::T, K::T) where T <: FinField

    # We call the Flint algorithms directly, currently this is based on
    # factorization

    M, N = embed_matrices(k, K)
    f(x) = embed_pre_mat(x, K, M)
    inv(y) = embed_pre_mat(y, k, N)

    return FinFieldMorphism(k, K, f, inv)
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

    for l in keys(subfields(k))
        if haskey(subfields(K), l)
            f = subfields(k)[l][1]
            g = subfields(K)[l][1]
            P = embed_polynomial(generator_minimum_polynomial(f), g)
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
        defPol = modulus(k)
        t = any_root(Q)
        M, N = embed_matrices_pre(gen(k), t, defPol)
        f(x) = embed_pre_mat(x, K, M)
        g(y) = embed_pre_mat(y, k, N)
        morph = FinFieldMorphism(k, K, f, g)

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
function transitive_closure(f::FinFieldMorphism)

    k = domain(f)
    K = codomain(f)

    # Subfields

    subk = subfields(k)
    subK = subfields(K)

    # We go through all subfields of k and check if they are also subfields of
    # K, we add them if they are not

    for d in keys(subk)
        if !haskey(subK, d)
            for g in subk[d]
                t(y) = f(g(y))
                tinv(x) = inverse_fn(g)(inverse_fn(f)(x))
                phi = FinFieldMorphism(domain(g), K, t, tinv)

                AddSubfield!(K, phi)
                AddOverfield!(domain(g), phi)
            end
        else
            val = FqNmodFiniteField[domain(v) for v in subK[d]]
            
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

    ov = overfields(K)

    # We call the same procedure on the overfields

    for d in keys(ov)
        for g in ov[d]
            transitive_closure(g)
        end
    end
end

@doc Markdown.doc"""
    intersections(k::T, K::T) where T <: FinField

For each subfield $S$ of $K$, embed $I$ in $S$ and $k$, where $I$ is the intersection
between $S$ and $k$.
"""
function intersections(k::T, K::T) where T <: FinField

    d = degree(k)
    subk = subfields(k)
    subK = subfields(K)
    needmore = true

    # We loop through the subfields of K and we have different cases
    for l in keys(subK)
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

    # If the finite fields are different but have the same degree, we check that
    # the embedding in the other direction does not exist. This is done in order
    # to prevent the creation of loops in the lattice

    if degree(k) == degree(K)
        tr = isembedded(K, k)
        if tr != nothing
            return preimage_map(tr)
        end
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

preimage(f::FinFieldMorphism, x::T) where T <: FinField = preimage_map(f)(x)
