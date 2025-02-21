import MacroTools

abstract type FlintStruct end

function Base.getproperty(val::FlintStruct, name::Symbol)
  if fieldtype(typeof(val), name) <: Tuple{T} where T # unwrap singleton tuples
    return only(getfield(val, name))
  else
    getfield(val, name)
  end
end

macro flintstruct(structdef::Expr)
  structprops = MacroTools.splitstructdef(structdef)
  @assert !structprops[:mutable]
  @assert isempty(structprops[:params])
  @assert structprops[:supertype] == :Any
  
  @show structprops

  structprops[:supertype] = :(FlintStruct)

  return esc(quote
    $(MacroTools.combinestructdef(structprops))
  end)
end
