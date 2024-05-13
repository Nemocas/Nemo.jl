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

function (f::FinFieldMorphism{S, T})(x) where {S, T}
  return image_fn(f)(x)::elem_type(T)
end

function show(io::IO, f::FinFieldMorphism)
  if is_terse(io)
    print(io, "Morphism of finite fields")
  else
    print(io, "Hom: ")
    print(terse(io), domain(f), " -> ", codomain(f))
  end
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
  if is_terse(io)
    print(io, "Preimage of a morphism")
  else
    print(io, "Hom: ")
    print(terse(io), domain(f), " -> ", codomain(f))
  end
end

@doc raw"""
    preimage_map(f::FinFieldMorphism)

Compute the preimage map corresponding to the embedding $f$.
"""
function preimage_map(f::FinFieldMorphism{S, T}) where {S, T}
  return FinFieldPreimage(domain(f), codomain(f), image_fn(f), inverse_fn(f))::FinFieldPreimage{S, T}
end

@doc raw"""
    preimage_map(f::FinFieldPreimage)

Compute the preimage map corresponding to the preimage of the embedding $f$,
i.e. return the embedding $f$.
"""
preimage_map(f::FinFieldPreimage{S, T}) where {S, T} = embed(domain(f), codomain(f))::FinFieldMorphism{S, T}
