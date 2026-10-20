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
  coeffs::Ptr{ZZRingElem} # array of fmpz
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
  data::Ptr{UInt} # array of ulong
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
  coeffs::Ptr{ZZRingElem} # array of fmpz
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
    @ccall libflint.qqbar_init(z::Ref{QQBarFieldElem})::Nothing
    finalizer(_qqbar_clear_fn, z)
    return z
  end

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
  ext_cache_items::Ptr{Ptr{Cvoid}} # array of ca_ext_struct *
  ext_cache_length::Int
  ext_cache_alloc::Int
  ext_cache_hash_size::Int
  ext_cache_hash_table::Ptr{Int} # array of slong
  field_cache_items::Ptr{Ptr{Cvoid}} # array of ca_field_struct *
  field_cache_length::Int
  field_cache_alloc::Int
  field_cache_hash_size::Int
  field_cache_hash_table::Ptr{Int} # array of slong
  field_qq::Ptr{Cvoid} # ca_field_struct *
  field_qq_i::Ptr{Cvoid} # ca_field_struct *
  mctx::Ptr{Ptr{Cvoid}} # array of fmpz_mpoly_ctx_struct *
  mctx_len::Int
  options::Ptr{Int}
  # end C struct

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
  field::Int
  data0::UInt
  data1::UInt
  data2::UInt
  data3::UInt
  # end C struct

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
