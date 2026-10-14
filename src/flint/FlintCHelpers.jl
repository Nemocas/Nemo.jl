abstract type FlintStruct end

# unwrap singleton tuples, i.e. C's `foo_t x[1]` fields
@inline Base.getproperty(val::FlintStruct, name::Symbol) = _unwrap(getfield(val, name))
@inline _unwrap(v::Tuple{T}) where {T} = only(v)
@inline _unwrap(v) = v

# copy of `d` with field `name` replaced by `v`
function _setfield(d::T, name::Symbol, v) where {T<:FlintStruct}
  return T(ntuple(i -> fieldname(T, i) === name ? v : getfield(d, i), Val(fieldcount(T)))...)
end
