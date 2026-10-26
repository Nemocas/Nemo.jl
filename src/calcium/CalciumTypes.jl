###############################################################################
#
#   CalciumTypes.jl : Parent and object types for Calcium
#
###############################################################################

################################################################################
#
#  truth_t triple-valued logic
#
################################################################################

const T_TRUE = 0
const T_FALSE = 1
const T_UNKNOWN = 2

function truth_as_bool(t::Cint, operation::Symbol)
  if t == T_TRUE
    return true
  elseif t == T_FALSE
    return false
  else
    error("Unable to perform operation (failed deciding truth of a predicate): ", operation)
  end
end

################################################################################
#
#  Structs for shallow operations
#
################################################################################

mutable struct qqbar_struct
  cstruct::FlintC.qqbar_struct
end
@flint_fields qqbar_struct
@flint_legacy_fields qqbar_struct begin
  coeffs = Ptr{ZZRingElem}(cstruct.poly.coeffs)
  alloc = cstruct.poly.alloc
  length = cstruct.poly.length
  real_mid_exp = cstruct.enclosure.real.mid.exp
  real_mid_size = cstruct.enclosure.real.mid.size
  real_mid_d1 = _union_word(cstruct.enclosure.real.mid.d, 1)
  real_mid_d1! = _setpath(cstruct, (:enclosure, :real, :mid, :d), _set_union_word(cstruct.enclosure.real.mid.d, 1, v))
  real_mid_d2 = _union_word(cstruct.enclosure.real.mid.d, 2)
  real_mid_d2! = _setpath(cstruct, (:enclosure, :real, :mid, :d), _set_union_word(cstruct.enclosure.real.mid.d, 2, v))
  real_rad_exp = cstruct.enclosure.real.rad.exp
  real_rad_man = cstruct.enclosure.real.rad.man
  imag_mid_exp = cstruct.enclosure.imag.mid.exp
  imag_mid_size = cstruct.enclosure.imag.mid.size
  imag_mid_d1 = _union_word(cstruct.enclosure.imag.mid.d, 1)
  imag_mid_d1! = _setpath(cstruct, (:enclosure, :imag, :mid, :d), _set_union_word(cstruct.enclosure.imag.mid.d, 1, v))
  imag_mid_d2 = _union_word(cstruct.enclosure.imag.mid.d, 2)
  imag_mid_d2! = _setpath(cstruct, (:enclosure, :imag, :mid, :d), _set_union_word(cstruct.enclosure.imag.mid.d, 2, v))
  imag_rad_exp = cstruct.enclosure.imag.rad.exp
  imag_rad_man = cstruct.enclosure.imag.rad.man
end

mutable struct fexpr_struct
  cstruct::FlintC.fexpr_struct
end
@flint_fields fexpr_struct

################################################################################
#
#  Types and memory management for QQBarField
#
################################################################################

const qqbar_field_doc = raw"""
    QQBarField <: Field
    QQBarFieldElem <: FieldElem

The algebraic closure $\overline{\mathbb Q}$ of the rationals $\mathbb Q$ and
its elements. This is a singleton type, meaning that there is a unique global
instance of this field, which can be obtained via `algebraic_closure(QQ)`.

```jldoctest
julia> K = algebraic_closure(QQ)
Algebraic closure of rational field

julia> sqrt(K(2))
{a2: 1.41421}

julia> QQBar(2//3)
{a1: 0.666667}

julia> sinpi(QQBar(2//3))
{a2: 0.866025}
```
"""

@doc qqbar_field_doc
struct QQBarField <: Field
end

@doc qqbar_field_doc
mutable struct QQBarFieldElem <: FieldElem
  cstruct::FlintC.qqbar_struct

  function QQBarFieldElem()
    z = new()
    @ccall libflint.qqbar_init(z::Ref{QQBarFieldElem})::Nothing
    finalizer(_qqbar_clear_fn, z)
    return z
  end

end
@flint_fields QQBarFieldElem
@flint_legacy_fields QQBarFieldElem begin
  coeffs = Ptr{ZZRingElem}(cstruct.poly.coeffs)
  alloc = cstruct.poly.alloc
  length = cstruct.poly.length
  real_mid_exp = cstruct.enclosure.real.mid.exp
  real_mid_size = cstruct.enclosure.real.mid.size
  real_mid_d1 = _union_word(cstruct.enclosure.real.mid.d, 1)
  real_mid_d1! = _setpath(cstruct, (:enclosure, :real, :mid, :d), _set_union_word(cstruct.enclosure.real.mid.d, 1, v))
  real_mid_d2 = _union_word(cstruct.enclosure.real.mid.d, 2)
  real_mid_d2! = _setpath(cstruct, (:enclosure, :real, :mid, :d), _set_union_word(cstruct.enclosure.real.mid.d, 2, v))
  real_rad_exp = cstruct.enclosure.real.rad.exp
  real_rad_man = cstruct.enclosure.real.rad.man
  imag_mid_exp = cstruct.enclosure.imag.mid.exp
  imag_mid_size = cstruct.enclosure.imag.mid.size
  imag_mid_d1 = _union_word(cstruct.enclosure.imag.mid.d, 1)
  imag_mid_d1! = _setpath(cstruct, (:enclosure, :imag, :mid, :d), _set_union_word(cstruct.enclosure.imag.mid.d, 1, v))
  imag_mid_d2 = _union_word(cstruct.enclosure.imag.mid.d, 2)
  imag_mid_d2! = _setpath(cstruct, (:enclosure, :imag, :mid, :d), _set_union_word(cstruct.enclosure.imag.mid.d, 2, v))
  imag_rad_exp = cstruct.enclosure.imag.rad.exp
  imag_rad_man = cstruct.enclosure.imag.rad.man
end

function _qqbar_clear_fn(a::QQBarFieldElem)
  @ccall libflint.qqbar_clear(a::Ref{QQBarFieldElem})::Nothing
end

################################################################################
#
#  Types and memory management for CalciumField
#
################################################################################

const ca_ctx_options = [
                  :verbose,
                  :print_flags,
                  :mpoly_ord,
                  :prec_limit,
                  :qqbar_deg_limit,
                  :low_prec,
                  :smooth_limit,
                  :lll_prec,
                  :pow_limit,
                  :use_gb,
                  :gb_length_limit,
                  :gb_poly_length_limit,
                  :gb_poly_bits_limit,
                  :vieta_limit,
                  :trig_form]

@attributes mutable struct CalciumField <: Field
  cstruct::FlintC.ca_ctx_struct

  extended::Bool
  refcount::Int

  function CalciumField(; extended::Bool=false, options::Dict{Symbol,Int}=Dict{Symbol,Int}())
    C = new()

    @ccall libflint.ca_ctx_init(C::Ref{CalciumField})::Nothing
    finalizer(_CalciumField_clear_fn, C)
    C.extended = extended

    for (opt, value) in options
      i = findfirst(isequal(opt), ca_ctx_options)
      (i === nothing) && error("unknown option ", opt)
      @ccall libflint.ca_ctx_set_option(C::Ref{CalciumField}, (i - 1)::Int, value::Int)::Nothing
    end

    C.refcount = 1
    return C
  end
end
@flint_fields CalciumField
@flint_legacy_fields CalciumField begin
  ext_cache_items = cstruct.ext_cache.items
  ext_cache_length = cstruct.ext_cache.length
  ext_cache_alloc = cstruct.ext_cache.alloc
  ext_cache_hash_size = cstruct.ext_cache.hash_size
  ext_cache_hash_table = cstruct.ext_cache.hash_table
  field_cache_items = cstruct.field_cache.items
  field_cache_length = cstruct.field_cache.length
  field_cache_alloc = cstruct.field_cache.alloc
  field_cache_hash_size = cstruct.field_cache.hash_size
  field_cache_hash_table = cstruct.field_cache.hash_table
end

function options(C::CalciumField)
  d = Dict{Symbol,Int}()
  for i=1:length(ca_ctx_options)
    d[ca_ctx_options[i]] = @ccall libflint.ca_ctx_get_option(C::Ref{CalciumField}, (i - 1)::Int)::Int
  end
  return d
end

function decrement_refcount(C::CalciumField)
  C.refcount -= 1
  if C.refcount == 0
    @ccall libflint.ca_ctx_clear(C::Ref{CalciumField})::Nothing
  end
end

function _CalciumField_clear_fn(C::CalciumField)
  decrement_refcount(C)
end

mutable struct CalciumFieldElem <: FieldElem
  cstruct::FlintC.ca_struct

  parent::CalciumField

  function CalciumFieldElem(ctx::CalciumField)
    z = new()
    @ccall libflint.ca_init(z::Ref{CalciumFieldElem}, ctx::Ref{CalciumField})::Nothing
    z.parent = ctx
    z.parent.refcount += 1
    finalizer(_ca_clear_fn, z)
    return z
  end

end
@flint_fields CalciumFieldElem
@flint_legacy_fields CalciumFieldElem begin
  data0 = _union_word(cstruct.elem, 1)
  data0! = _setfield(cstruct, :elem, _set_union_word(cstruct.elem, 1, v))
  data1 = _union_word(cstruct.elem, 2)
  data1! = _setfield(cstruct, :elem, _set_union_word(cstruct.elem, 2, v))
  data2 = _union_word(cstruct.elem, 3)
  data2! = _setfield(cstruct, :elem, _set_union_word(cstruct.elem, 3, v))
  data3 = _union_word(cstruct.elem, 4)
  data3! = _setfield(cstruct, :elem, _set_union_word(cstruct.elem, 4, v))
end

function _ca_clear_fn(a::CalciumFieldElem)
  @ccall libflint.ca_clear(a::Ref{CalciumFieldElem}, parent(a)::Ref{CalciumField})::Nothing
  decrement_refcount(parent(a))
end


################################################################################
#
#   Type unions
#
################################################################################

const QQBarFieldElemOrPtr = TypeOrPtr{QQBarFieldElem}
const CalciumFieldElemOrPtr = TypeOrPtr{CalciumFieldElem}
