"""
    FlintStruct

Supertype of the generated mirrors of FLINT's C structs.

FLINT's `typedef foo_struct foo_t[1];` is generated as
`const foo_t = Tuple{foo_struct}`. Reading a field of type `foo_t` returns
its `foo_struct` instead of the tuple.
"""
abstract type FlintStruct end

@inline Base.getproperty(val::FlintStruct, name::Symbol) = _unwrap(getfield(val, name))
@inline _unwrap(v::Tuple{T}) where {T} = only(v)
@inline _unwrap(v) = v
