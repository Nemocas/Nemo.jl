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
  coeffs::Ptr{Nothing}
  alloc::Int
  length::Int
  real_mid_exp::Int     # ZZRingElem
  real_mid_size::UInt # mp_size_t
  real_mid_d1::UInt    # mantissa_struct
  real_mid_d2::UInt
  real_rad_exp::Int     # ZZRingElem
  real_rad_man::UInt
  imag_mid_exp::Int     # ZZRingElem
  imag_mid_size::UInt # mp_size_t
  imag_mid_d1::UInt    # mantissa_struct
  imag_mid_d2::UInt
  imag_rad_exp::Int     # ZZRingElem
  imag_rad_man::UInt
end

mutable struct fexpr_struct
  data::Ptr{Nothing}
  alloc::Int
end

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
Field of algebraic numbers

julia> sqrt(K(2))
Root 1.41421 of x^2 - 2

julia> QQBar(2//3)
Root 0.666667 of 3x - 2

julia> sinpi(QQBar(2//3))
Root 0.866025 of 4x^2 - 3
```
"""

@doc qqbar_field_doc
struct QQBarField <: Field
end

@doc qqbar_field_doc
mutable struct QQBarFieldElem <: FieldElem
  coeffs::Ptr{Nothing}
  alloc::Int
  length::Int
  real_mid_exp::Int     # ZZRingElem
  real_mid_size::UInt # mp_size_t
  real_mid_d1::UInt    # mantissa_struct
  real_mid_d2::UInt
  real_rad_exp::Int     # ZZRingElem
  real_rad_man::UInt
  imag_mid_exp::Int     # ZZRingElem
  imag_mid_size::UInt # mp_size_t
  imag_mid_d1::UInt    # mantissa_struct
  imag_mid_d2::UInt
  imag_rad_exp::Int     # ZZRingElem
  imag_rad_man::UInt

  function QQBarFieldElem()
    z = new()
    ccall((:qqbar_init, libflint), Nothing, (Ref{QQBarFieldElem}, ), z)
    finalizer(_qqbar_clear_fn, z)
    return z
  end

end

function _qqbar_clear_fn(a::QQBarFieldElem)
  ccall((:qqbar_clear, libflint), Nothing, (Ref{QQBarFieldElem},), a)
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
  ext_cache_items::Ptr{Nothing}
  ext_cache_length::Int
  ext_cache_alloc::Int
  ext_cache_hash_size::Int
  ext_cache_hash_table::Ptr{Nothing}
  field_cache_items::Ptr{Nothing}
  field_cache_length::Int
  field_cache_alloc::Int
  field_cache_hash_size::Int
  field_cache_hash_table::Ptr{Nothing}
  field_qq::Ptr{Nothing}
  field_qq_i::Ptr{Nothing}
  mctx::Ptr{Nothing}
  mctx_len::Int
  options::Ptr{Int}
  # end C struct

  extended::Bool
  refcount::Int

  function CalciumField(; extended::Bool=false, options::Dict{Symbol,Int}=Dict{Symbol,Int}())
    C = new()

    ccall((:ca_ctx_init, libflint), Nothing, (Ref{CalciumField}, ), C)
    finalizer(_CalciumField_clear_fn, C)
    C.extended = extended

    for (opt, value) in options
      i = findfirst(isequal(opt), ca_ctx_options)
      (i === nothing) && error("unknown option ", opt)
      ccall((:ca_ctx_set_option, libflint), Nothing, (Ref{CalciumField}, Int, Int), C, i - 1, value)
    end

    C.refcount = 1
    return C
  end
end

function options(C::CalciumField)
  d = Dict{Symbol,Int}()
  for i=1:length(ca_ctx_options)
    d[ca_ctx_options[i]] = ccall((:ca_ctx_get_option, libflint), Int, (Ref{CalciumField}, Int), C, i - 1)
  end
  return d
end

function decrement_refcount(C::CalciumField)
  C.refcount -= 1
  if C.refcount == 0
    ccall((:ca_ctx_clear, libflint), Nothing, (Ref{CalciumField},), C)
  end
end

function _CalciumField_clear_fn(C::CalciumField)
  decrement_refcount(C)
end

mutable struct CalciumFieldElem <: FieldElem
  field::Int
  data0::UInt
  data1::UInt
  data2::UInt
  data3::UInt
  # end C struct

  parent::CalciumField

  function CalciumFieldElem(ctx::CalciumField)
    z = new()
    ccall((:ca_init, libflint), Nothing,
          (Ref{CalciumFieldElem}, Ref{CalciumField}), z, ctx)
    z.parent = ctx
    z.parent.refcount += 1
    finalizer(_ca_clear_fn, z)
    return z
  end

end

function _ca_clear_fn(a::CalciumFieldElem)
  ccall((:ca_clear, libflint),
        Nothing, (Ref{CalciumFieldElem}, Ref{CalciumField}), a, a.parent)
  decrement_refcount(a.parent)
end


################################################################################
#
#   Type unions
#
################################################################################

const QQBarFieldElemOrPtr = Union{QQBarFieldElem, Ref{QQBarFieldElem}, Ptr{QQBarFieldElem}}
const CalciumFieldElemOrPtr = Union{CalciumFieldElem, Ref{CalciumFieldElem}, Ptr{CalciumFieldElem}}
