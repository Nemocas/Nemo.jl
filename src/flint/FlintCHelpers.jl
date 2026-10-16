abstract type FlintStruct end

function Base.getproperty(val::FlintStruct, name::Symbol)
  if fieldtype(typeof(val), name) <: Tuple{T} where T # unwrap singleton tuples
    return only(getfield(val, name))
  else
    getfield(val, name)
  end
end
