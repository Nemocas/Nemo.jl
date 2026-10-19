abstract type FlintStruct end

# unwrap singleton tuples, i.e. C's `foo_t x[1]` fields
@inline Base.getproperty(val::FlintStruct, name::Symbol) = _unwrap(getfield(val, name))
@inline _unwrap(v::Tuple{T}) where {T} = only(v)
@inline _unwrap(v) = v

# field `name` of the C struct at `p`, which is a pointer to a `T` or to
# its C part alone
@inline function _unsafe_load_field(p::Ptr{T}, ::Val{name}) where {T, name}
  S = fieldtype(T, :data)
  i = Base.fieldindex(S, name)
  return _unwrap(unsafe_load(Ptr{fieldtype(S, i)}(p + fieldoffset(S, i))))
end

# copy of `d` with field `name` replaced by `v`
function _setfield(d::T, name::Symbol, v) where {T<:FlintStruct}
  return T(ntuple(i -> fieldname(T, i) === name ? _wrap(fieldtype(T, i), v) : getfield(d, i), Val(fieldcount(T)))...)
end
_wrap(::Type{Tuple{T}}, v::Tuple{T}) where {T} = v
_wrap(::Type{Tuple{T}}, v) where {T} = (convert(T, v),)
_wrap(::Type, v) = v

# `@flint_fields T [f = JuliaType ...]` forwards `x.f` and `x.f = v` to the
# fields of `x.data::FlintStruct`. A `f = JuliaType` pair converts that field
# on read, e.g. `entries = Ptr{ZZRingElem}` for Nemo's spelling of `fmpz*`.
macro flint_fields(T, conversions...)
  branches = map(conversions) do c
    Meta.isexpr(c, :(=), 2) || error("expected `field = Type`, got `$c`")
    f, ty = c.args
    :(s === $(QuoteNode(f)) && return $(esc(ty))(getfield(getfield(x, :data), $(QuoteNode(f)))))
  end
  cfields = esc(Symbol("_", T, "_cfields"))
  jfields = esc(Symbol("_", T, "_jfields"))
  T = esc(T)
  # Julia fields win over C fields of the same name, so `x.data` is always the
  # C struct. Extra and legacy properties win over C fields too, e.g. to keep
  # an enum field returning a `Cint` as before.
  return quote
    const $jfields = fieldnames($T)
    const $cfields = filter(!in($jfields), fieldnames(fieldtype($T, :data)))
    @inline function Base.getproperty(x::$T, s::Symbol)
      s in $jfields && return getfield(x, s)
      $(branches...)
      (s in _extra_fieldnames($T) || s in _legacy_fieldnames($T)) && return _extra_property(x, Val(s))
      s in $cfields && return getproperty(getfield(x, :data), s)
      return getfield(x, s)
    end
    @inline function Base.setproperty!(x::$T, s::Symbol, v)
      s in $jfields && return setfield!(x, s, v)
      (s in _extra_fieldnames($T) || s in _legacy_fieldnames($T)) && return _set_extra_property!(x, Val(s), v)
      s in $cfields && return setfield!(x, :data, _setfield(getfield(x, :data), s, v))
      return setfield!(x, s, v)
    end
    function Base.propertynames(x::$T, private::Bool=false)
      names = ($cfields..., _extra_fieldnames($T)..., filter(!=(:data), $jfields)...)
      return private ? (names..., :data, _legacy_fieldnames($T)...) : names
    end
  end
end

# Properties beyond the fields of `data`, e.g. views into an opaque union.
# `@flint_extra_fields T name = expr ...` makes `x.name` evaluate `expr`, where
# `data` is the C struct. A `begin` block may hold the pairs. The body must not
# go through `x.` again: inference gives up on that recursion.
#
# Writing `x.name = v` works when `expr` is a plain field path `data.a.b`.
# Otherwise `name! = expr` gives the new C struct in terms of `data` and `v`.
_extra_fieldnames(::Type) = ()
_set_extra_property!(x, ::Val{s}, v) where {s} = error("property $s of $(typeof(x)) is read-only")

# Field names from before the generated structs, kept so that downstream code
# keeps working. To drop them, delete `@flint_legacy_fields` and its uses and
# `_legacy_fieldnames`.
_legacy_fieldnames(::Type) = ()

# `d` with the field at `path` replaced by `v`
@inline _setpath(d, path::Tuple{Symbol}, v) = _setfield(d, path[1], v)
@inline _setpath(d, path::Tuple, v) = _setfield(d, path[1], _setpath(getproperty(d, path[1]), Base.tail(path), v))

# word `i` of a union, and the union with word `i` replaced
@inline _union_word(d, i) = getfield(d, :uniondata)[i]
@inline _set_union_word(d, i, v) = _setfield(d, :uniondata, Base.setindex(getfield(d, :uniondata), _asword(v), i))
_asword(v::Ptr) = UInt(v)
_asword(v::Integer) = v % UInt

function _property_defs(T, pairs, namesfun)
  if length(pairs) == 1 && Meta.isexpr(pairs[1], :block)
    pairs = filter(a -> !(a isa LineNumberNode), pairs[1].args)
  end
  subst(ex) = ex === :data ? :(getfield(x, :data)) : ex isa Expr ? Expr(ex.head, map(subst, ex.args)...) : ex
  # `data.a.b` as `(:a, :b)`, or nothing
  function path(ex)
    ex === :data && return ()
    Meta.isexpr(ex, :., 2) && ex.args[2] isa QuoteNode || return nothing
    inner = path(ex.args[1])
    return inner === nothing ? nothing : (inner..., ex.args[2].value)
  end
  defs = Expr[]
  names = Symbol[]
  setters = Base.Set{Symbol}()
  for c in pairs
    Meta.isexpr(c, :(=), 2) || error("expected `name = expr`, got `$c`")
    name, ex = c.args
    if endswith(string(name), "!")
      name = Symbol(chop(string(name)))
      push!(setters, name)
      push!(defs, :(@inline $(esc(:_set_extra_property!))($(esc(:x))::$(esc(T)), ::Val{$(QuoteNode(name))}, $(esc(:v))) = setfield!($(esc(:x)), :data, $(esc(subst(ex))))))
      continue
    end
    push!(names, name)
    push!(defs, :(@inline $(esc(:_extra_property))($(esc(:x))::$(esc(T)), ::Val{$(QuoteNode(name))}) = $(esc(subst(ex)))))
    pth = path(ex)
    if pth !== nothing && !isempty(pth) && !(name in setters)
      push!(setters, name)
      push!(defs, :(@inline $(esc(:_set_extra_property!))($(esc(:x))::$(esc(T)), ::Val{$(QuoteNode(name))}, $(esc(:v))) = setfield!($(esc(:x)), :data, _setpath(getfield($(esc(:x)), :data), $(Tuple(pth)), $(esc(:v))))))
    end
  end
  push!(defs, :($(esc(namesfun))(::Type{$(esc(T))}) = $(Tuple(names))))
  return Expr(:block, defs...)
end

macro flint_extra_fields(T, pairs...)
  return _property_defs(T, pairs, :_extra_fieldnames)
end

macro flint_legacy_fields(T, pairs...)
  return _property_defs(T, pairs, :_legacy_fieldnames)
end
