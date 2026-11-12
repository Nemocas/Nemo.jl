# Property access for Nemo's FLINT-backed types.
#
# Such a type stores the C struct as its first field, `cstruct`, of a type from
# the generated module `FlintC` (`src/FlintC.jl`). `ccall`s then see
# FLINT's layout, and the macros in this file make `x.f` and `x.f = v` reach
# the C fields, so the rest of Nemo keeps field syntax.
#
# A type `T` declared with these macros has three kinds of properties besides
# its own Julia fields:
#
# - C fields: the fields of `T.cstruct`, forwarded by `@flint_fields`.
# - extra properties: values computed from `cstruct`, declared with
#   `@flint_extra_fields`; e.g. the `entries` pointer of a matrix that FLINT
#   stores as a C union.
# - legacy properties: the field names Nemo had before the generated structs,
#   declared with `@flint_legacy_fields`. They keep downstream code working
#   and are to be removed: delete the macro, its uses and
#   `_legacy_fieldnames`.
#
# Lookup order: Julia fields, then conversions, extra and legacy properties,
# then C fields. So `x.cstruct` is always the C struct, and a legacy name can
# shadow a C field, e.g. to keep returning a `Cint` where FLINT has an enum.

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

"""
    _setfield(d::FlintStruct, name::Symbol, v)

Return a copy of `d` with field `name` set to `v`.

For a field of a singleton array type `foo_t = Tuple{foo_struct}`, `v` may
be a `foo_struct`; it is wrapped into the tuple.
"""
function _setfield(d::T, name::Symbol, v) where {T<:FlintStruct}
  return T(ntuple(i -> fieldname(T, i) === name ? _wrap(fieldtype(T, i), v) : getfield(d, i), Val(fieldcount(T)))...)
end
_wrap(::Type{Tuple{T}}, v::Tuple{T}) where {T} = v
_wrap(::Type{Tuple{T}}, v) where {T} = (convert(T, v),)
_wrap(::Type, v) = v

"""
    _cstruct_type(::Type{T})

Return the type of the C struct behind `T`.

That is the type of the `cstruct` field, or `T` itself for a type without
one, such as a generated struct or `BigInt`.
"""
_cstruct_type(::Type{T}) where {T} = hasfield(T, :cstruct) ? fieldtype(T, :cstruct) : T

"""
    _cfield_path(::Type{T}, ::Val{name})

Return the C fields leading to property `name` of `T`, as a tuple of symbols.

That is `(name,)` for a C field, and the declared path for an extra or
legacy property defined by a plain field path; the macros define those
methods. The path is relative to [`_cstruct_type`](@ref)`(T)`.
"""
_cfield_path(::Type{T}, ::Val{name}) where {T, name} = (name,)

"""
    _cfieldtype(::Type{T}, name::Symbol)

Return the type of the C field `name` of `T`, i.e. of the field `name` of
[`_cstruct_type`](@ref)`(T)`.
"""
_cfieldtype(::Type{T}, name::Symbol) where {T} = fieldtype(_cstruct_type(T), name)

"""
    _path_offset(::Type{T}, path)
    _path_type(::Type{T}, path)

Return the byte offset and the type of the nested field `path` of the C
struct `T`, a tuple of symbols as returned by [`_cfield_path`](@ref).

A singleton array field is entered at its element.
"""
@inline function _path_offset(::Type{T}, path::Tuple{Symbol, Vararg{Symbol}}) where {T}
  i = Base.fieldindex(T, first(path))
  return fieldoffset(T, i) + _path_offset(_unwrap_type(fieldtype(T, i)), Base.tail(path))
end
@inline _path_offset(::Type, ::Tuple{}) = 0
@inline _path_type(::Type{T}, path::Tuple{Symbol, Vararg{Symbol}}) where {T} =
  _path_type(_unwrap_type(fieldtype(T, first(path))), Base.tail(path))
@inline _path_type(::Type{T}, ::Tuple{}) where {T} = T
_unwrap_type(::Type{Tuple{T}}) where {T} = T
_unwrap_type(::Type{T}) where {T} = T

"""
    _setpath(d::FlintStruct, path::Tuple{Vararg{Symbol}}, v)

Return a copy of `d` in which the nested field given by `path` is set to
`v`. For example, the path `(:mod, :n)` denotes the field `d.mod.n`.

Each level is set with [`_setfield`](@ref), so singleton array fields along
the path are unwrapped and wrapped again.
"""
@inline _setpath(d, path::Tuple{Symbol, Vararg{Symbol}}, v) = _setfield(d, first(path), _setpath(getproperty(d, first(path)), Base.tail(path), v))
@inline _setpath(d, ::Tuple{}, v) = v

"""
    @flint_fields T
    @flint_fields T begin
      f = JuliaType
      [...]
    end

Forward `x.f` and `x.f = v` for `x::T` to the fields of `x.cstruct`.

A pair `f = JuliaType` wraps every read of `x.f` in a call to `JuliaType`.
The generated structs declare pointer fields with the C element type, and
the conversion gives them the type Nemo uses instead.

Also defines `propertynames`: the C fields, the extra properties (see
[`@flint_extra_fields`](@ref)) and the Julia fields other than `cstruct`; with
`private = true` also `cstruct` and the legacy properties (see
[`@flint_legacy_fields`](@ref)).

# Examples
```julia
mutable struct ZZMatrix <: FlintMatElem{ZZRingElem}
  cstruct::FlintC.fmpz_mat_struct  # entries::Ptr{fmpz}, r, c, stride
  view_parent
end
@flint_fields ZZMatrix begin
  entries = Ptr{ZZRingElem}
end

A = ZZ[1 2; 3 4]
A.r          # 2, read from A.cstruct.r
A.entries    # Ptr{ZZRingElem}(A.cstruct.entries)
A.r = 2      # replaces A.cstruct by a copy with r = 2
```
"""
macro flint_fields(T, block=:(begin end))
  @capture(block, begin conversions__ end) || error("expected a `begin` block, got `$block`")
  branches = map(conversions) do c
    @capture(c, f_ = ty_) || error("expected `field = Type`, got `$c`")
    :(s === $(QuoteNode(f)) && return $(esc(ty))(getfield(getfield(x, :cstruct), $(QuoteNode(f)))))
  end
  cfields = esc(Symbol("_", T, "_cfields"))
  jfields = esc(Symbol("_", T, "_jfields"))
  T = esc(T)
  # lookup order as described at the top of this file
  return quote
    const $jfields = fieldnames($T)
    const $cfields = filter(!in($jfields), fieldnames(fieldtype($T, :cstruct)))
    @inline function Base.getproperty(x::$T, s::Symbol)
      s in $jfields && return getfield(x, s)
      $(branches...)
      (s in _extra_fieldnames($T) || s in _legacy_fieldnames($T)) && return _property(x, Val(s))
      s in $cfields && return getproperty(getfield(x, :cstruct), s)
      return getfield(x, s)
    end
    @inline function Base.setproperty!(x::$T, s::Symbol, v)
      s in $jfields && return setfield!(x, s, v)
      (s in _extra_fieldnames($T) || s in _legacy_fieldnames($T)) && return _set_property!(x, Val(s), v)
      s in $cfields && return setfield!(x, :cstruct, _setfield(getfield(x, :cstruct), s, v))
      return setfield!(x, s, v)
    end
    function Base.propertynames(x::$T, private::Bool=false)
      names = ($cfields..., _extra_fieldnames($T)..., filter(!=(:cstruct), $jfields)...)
      return private ? (names..., :cstruct, _legacy_fieldnames($T)...) : names
    end
  end
end

"""
    @flint_extra_fields T begin
      name = expr
      [...]
    end

Define extra properties of `T`: `x.name` evaluates `expr`, in which `cstruct`
stands for the C struct of `x`.

`expr` must not access `x` through `getproperty`, i.e. `x.a` is not
allowed, as inference does not handle the recursion.

`x.name = v` is defined when `expr` is a plain field path `cstruct.a.b`. For
anything else, add `name! = expr` with `expr` giving the new C struct in
terms of `cstruct` and `v`.

# Examples
`FqMatrix` wraps a C union, so its matrix fields are words of that union:
```julia
@flint_extra_fields FqMatrix begin
  entries = Ptr{FqFieldElem}(_union_word(cstruct, 1))
  entries! = _set_union_word(cstruct, 1, v)
  r = _union_word(cstruct, 2) % Int
  r! = _set_union_word(cstruct, 2, v)
end
```
"""
macro flint_extra_fields(T, block)
  return _property_defs(T, block, :_extra_fieldnames)
end

"""
    @flint_legacy_fields T begin
      name = expr
      [...]
    end

Same as [`@flint_extra_fields`](@ref), but the names are listed by
`_legacy_fieldnames` instead of `_extra_fieldnames`, and so appear only in
the private `propertynames`.

# Examples
Before the generated structs, `zzModPolyRingElem` had the fields of its
`nmod_t` flattened into `mod_n`, `mod_ninv` and `mod_norm`:
```julia
@flint_legacy_fields zzModPolyRingElem begin
  mod_n = cstruct.mod.n
  mod_ninv = cstruct.mod.ninv
  mod_norm = cstruct.mod.norm
end
```

Each is a field path, so `x.mod_n = v` works without a `mod_n!` pair.
"""
macro flint_legacy_fields(T, block)
  return _property_defs(T, block, :_legacy_fieldnames)
end

"""
    _extra_fieldnames(::Type{T})

Names of the extra properties of `T`, as declared by
[`@flint_extra_fields`](@ref); empty for other types.
"""
_extra_fieldnames(::Type) = ()

"""
    _legacy_fieldnames(::Type{T})

Names of the legacy properties of `T`, as declared by
[`@flint_legacy_fields`](@ref); empty for other types.
"""
_legacy_fieldnames(::Type) = ()

"""
    _property(x::T, ::Val{name})

Getter behind the extra or legacy property `name` of `x`; the macros define
a method for each declared name.
"""
function _property end

"""
    _set_property!(x::T, ::Val{name}, v)

Setter behind the extra or legacy property `name` of `x`; the macros define
a method for each declared name that has one. The fallback errors.
"""
_set_property!(x, ::Val{s}, v) where {s} = error("property $s of $(typeof(x)) is read-only")

"""
    _union_word(d::FlintStruct, i)

Return the `i`-th machine word of the C union `d` as a `UInt`.

A generated C union has the single field `uniondata::NTuple{n, UInt}`
holding its bytes, as its members cannot be told apart by the type. The
caller knows which member is live and reinterprets the word, e.g. as a
`Ptr` or an `Int`. See [`@flint_extra_fields`](@ref) for the use with
`FqMatrix`.
"""
@inline _union_word(d, i) = getfield(d, :uniondata)[i]

"""
    _set_union_word(d::FlintStruct, i, v)

Return a copy of the C union `d` with its `i`-th machine word replaced by
`v`, a `Ptr` or an `Integer`, converted to a `UInt`.
"""
@inline _set_union_word(d, i, v) = _setfield(d, :uniondata, Base.setindex(getfield(d, :uniondata), _asword(v), i))
_asword(v::Ptr) = UInt(v)
_asword(v::Integer) = v % UInt

"""
    _copy_union_words!(z, x; from::Int = 1, to::Int = <last word>)

Copy the words `from` to `to` of the C union of `x` into that of `z`.

Both wrap the same generated union type, and `z` is modified in place.
"""
function _copy_union_words!(z, x; from::Int = 1, to::Int = typemax(Int))
  zw = getfield(getfield(z, :cstruct), :uniondata)
  xw = getfield(getfield(x, :cstruct), :uniondata)
  words = ntuple(i -> from <= i <= to ? xw[i] : zw[i], Val(length(zw)))
  setfield!(z, :cstruct, _setfield(getfield(z, :cstruct), :uniondata, words))
  return z
end

"""
    _subst_cstruct(ex)

Replace the symbol `cstruct` in the expression `ex` by `getfield(x, :cstruct)`.

Used as a helper in [`_property_defs`](@ref).
"""
_subst_cstruct(ex) = postwalk(e -> e === :cstruct ? :(getfield(x, :cstruct)) : e, ex)

"""
    _field_path(ex)

Return the field names of a plain field path starting at `cstruct`.

The result is `(:a, :b)` for `cstruct.a.b` and `()` for `cstruct`. Any other
expression, including a path that does not start at `cstruct`, gives
`nothing`. Used as a helper in [`_property_defs`](@ref).
"""
function _field_path(ex)
  ex === :cstruct && return ()
  @capture(ex, inner_.field_) || return nothing
  path = _field_path(inner)
  return path === nothing ? nothing : (path..., field)
end

"""
    _property_defs(T, block, namesfun)

Expand the `name = expr` pairs in the `block` of
[`@flint_extra_fields`](@ref) or [`@flint_legacy_fields`](@ref) for `T` into
[`_property`](@ref) and [`_set_property!`](@ref) methods.

`namesfun` is [`_extra_fieldnames`](@ref) or [`_legacy_fieldnames`](@ref),
depending on the macro; a method of it for `T` returning the declared names
is generated as well.
"""
function _property_defs(T, block, namesfun)
  @capture(block, begin pairs__ end) || error("expected a `begin` block, got `$block`")
  defs = Expr[]
  names = Symbol[]
  setters = Base.Set{Symbol}()
  for c in pairs
    @capture(c, name_ = ex_) || error("expected `name = expr`, got `$c`")
    if endswith(string(name), "!")
      name = Symbol(chop(string(name)))
      push!(setters, name)
      push!(defs, :(@inline $(esc(:_set_property!))($(esc(:x))::$(esc(T)), ::Val{$(QuoteNode(name))}, $(esc(:v))) = setfield!($(esc(:x)), :cstruct, $(esc(_subst_cstruct(ex))))))
      continue
    end
    push!(names, name)
    push!(defs, :(@inline $(esc(:_property))($(esc(:x))::$(esc(T)), ::Val{$(QuoteNode(name))}) = $(esc(_subst_cstruct(ex)))))
    pth = _field_path(ex)
    # a conversion around a path, `Ptr{T}(cstruct.a.b)`, keeps the location
    cpth = pth !== nothing ? pth : @capture(ex, f_(inner_)) ? _field_path(inner) : nothing
    cpth === nothing || push!(defs, :($(esc(:_cfield_path))(::Type{$(esc(T))}, ::Val{$(QuoteNode(name))}) = $(Tuple(cpth))))
    if pth !== nothing && !(name in setters)
      push!(setters, name)
      push!(defs, :(@inline $(esc(:_set_property!))($(esc(:x))::$(esc(T)), ::Val{$(QuoteNode(name))}, $(esc(:v))) = setfield!($(esc(:x)), :cstruct, _setpath(getfield($(esc(:x)), :cstruct), $(Tuple(pth)), $(esc(:v))))))
    end
  end
  push!(defs, :($(esc(namesfun))(::Type{$(esc(T))}) = $(Tuple(names))))
  return Expr(:block, defs...)
end
