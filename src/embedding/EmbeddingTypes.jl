################################################################################
#
#  FinFieldsMorphism : some types needed to work with embeddings
#
################################################################################

struct FinFieldMorphism{T} <: AbstractAlgebra.Map{T, T, AbstractAlgebra.SetMap,
                                                  FinFieldMorphism} 
    map::AbstractAlgebra.Map
    preimage::AbstractAlgebra.Map

    function FinFieldMorphism(domain::T, codomain::T, image_fn::Function,
                              inverse_fn::Function) where T
        map = AbstractAlgebra.map_from_func(image_fn, domain, codomain)
        preimage = AbstractAlgebra.map_from_func(inverse_fn, codomain, domain)
        return new{T}(map, preimage)
    end
end


domain(f::FinFieldMorphism) = domain(f.map)
codomain(f::FinFieldMorphism) = codomain(f.map)
image_fn(f::FinFieldMorphism) = image_fn(f.map)
inverse_fn(f::FinFieldMorphism) = image_fn(f.preimage)

function (f::FinFieldMorphism)(x)
    return image_fn(f)(x)::elem_type(codomain(f))
end

function Base.show(io::IO, f::FinFieldMorphism)
    print("Morphism from $(domain(f))\nto $(codomain(f))")
end

struct FinFieldPreimage{T} <: AbstractAlgebra.Map{T, T, AbstractAlgebra.SetMap,
                                                  FinFieldPreimage}
    map::AbstractAlgebra.Map
    preimage::AbstractAlgebra.Map

    function FinFieldPreimage(domain::T, codomain::T, image_fn::Function,
                              inverse_fn::Function) where T
        map = AbstractAlgebra.map_from_func(image_fn, domain, codomain)
        preimage = AbstractAlgebra.map_from_func(inverse_fn, codomain, domain)
        return new{T}(map, preimage)
    end
end

domain(f::FinFieldPreimage) = domain(f.map)
codomain(f::FinFieldPreimage) = codomain(f.map)
image_fn(f::FinFieldPreimage) = image_fn(f.map)
inverse_fn(f::FinFieldPreimage) = image_fn(f.preimage)

function (f::FinFieldPreimage)(x)
    a = inverse_fn(f)(x)::elem_type(domain(f))
    b = image_fn(f)(a)
    if x == b
        return a
    else
        throw(ArgumentError(string("not an element in the subfield of degree ",
                                   degree(domain(f)), " over F_",
                                   characteristic(domain(f)))))
    end
end

function Base.show(io::IO, f::FinFieldPreimage)
    print("Preimage of the morphism from $(domain(f))\nto $(codomain(f))")
end

@doc Markdown.doc"""
    preimage_map(f::FinFieldMorphism)

Compute the preimage map corresponding to the embedding $f$.
"""
preimage_map(f::FinFieldMorphism) = FinFieldPreimage(domain(f), codomain(f),
                                                     image_fn(f), inverse_fn(f))
