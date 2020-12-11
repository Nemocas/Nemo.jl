export FinFieldMorphism

################################################################################
#
#  FinFieldsMorphism : some types needed to work with embeddings
#
################################################################################

struct FinFieldMorphism{S, T} <: AbstractAlgebra.Map{S, T, AbstractAlgebra.SetMap,
                                                  FinFieldMorphism} 
    map::AbstractAlgebra.Map
    preimage::AbstractAlgebra.Map

    function FinFieldMorphism(domain::S, codomain::T, image_fn::Function,
                              inverse_fn::Function) where {S, T}
        map = AbstractAlgebra.map_from_func(image_fn, domain, codomain)
        preimage = AbstractAlgebra.map_from_func(inverse_fn, codomain, domain)
        return new{S, T}(map, preimage)
    end
end


domain(f::FinFieldMorphism{S, T}) where {S, T} = domain(f.map)::S
codomain(f::FinFieldMorphism{S, T}) where {S, T} = codomain(f.map)::T
image_fn(f::FinFieldMorphism) = image_fn(f.map)
inverse_fn(f::FinFieldMorphism) = image_fn(f.preimage)

function (f::FinFieldMorphism{S, T})(x::U) where {S <: FinField, T <: FinField, U<:FinFieldElem}
    return image_fn(f)(x)::elem_type(T)
end

function ==(f::FinFieldMorphism{S, T}, g::FinFieldMorphism{S, T}) where {S, T}
    if domain(f) != domain(g)
        return false
    end
    if codomain(f) != codomain(g)
        return false
    end
    return f(gen(domain(f))) == g(gen(domain(f)))
end

function Base.show(io::IO, f::FinFieldMorphism)
    print(io, "Morphism from $(domain(f))\nto $(codomain(f))")
end

struct FinFieldPreimage{S, T} <: AbstractAlgebra.Map{S, T, AbstractAlgebra.SetMap,
                                                  FinFieldPreimage}
    map::AbstractAlgebra.Map
    preimage::AbstractAlgebra.Map

    function FinFieldPreimage(domain::S, codomain::T, image_fn::Function,
                              inverse_fn::Function) where {S, T}
        map = AbstractAlgebra.map_from_func(image_fn, domain, codomain)
        preimage = AbstractAlgebra.map_from_func(inverse_fn, codomain, domain)
        return new{S, T}(map, preimage)
    end
end

domain(f::FinFieldPreimage{S, T}) where {S, T} = domain(f.map)::S
codomain(f::FinFieldPreimage{S, T}) where {S, T} = codomain(f.map)::T
image_fn(f::FinFieldPreimage) = image_fn(f.map)
inverse_fn(f::FinFieldPreimage) = image_fn(f.preimage)

function (f::FinFieldPreimage{S, T})(x) where {S, T}
    a = inverse_fn(f)(x)::elem_type(S)
    b = image_fn(f)(a)::elem_type(T)
    if x == b
        return a
    else
        throw(ArgumentError(string("not an element in the subfield of degree ",
                                   degree(domain(f)), " over F_",
                                   characteristic(domain(f)))))
    end
end

function Base.show(io::IO, f::FinFieldPreimage)
    print(io, "Preimage of the morphism from $(domain(f))\nto $(codomain(f))")
end

@doc Markdown.doc"""
    preimage_map(f::FinFieldMorphism)

Compute the preimage map corresponding to the embedding $f$.
"""
preimage_map(f::FinFieldMorphism) = FinFieldPreimage(domain(f), codomain(f),
                                                     image_fn(f), inverse_fn(f))
