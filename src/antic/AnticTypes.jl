###############################################################################
#
#   AnticTypes.jl : Antic types
#
###############################################################################

###############################################################################
#
#   AbsSimpleNumField / AbsSimpleNumFieldElem
#
###############################################################################

"""
    AbsSimpleNumField

This is the basic type for absolute extensions, i.e., an extension of QQ by some
irreducible monic polyomial with coefficients in either ZZ or QQ.

Creation is usually by calling [`number_field`](@ref), but see also
[`cyclotomic_field`](@ref) and [`cyclotomic_real_subfield`](@ref)
for some more specialized fields.
"""
@attributes mutable struct AbsSimpleNumField <: SimpleNumField{QQFieldElem}
  pol_coeffs::Ptr{Nothing}
  pol_alloc::Int
  pol_length::Int
  pol_den::Int
  pinv_dinv::Ptr{Nothing}
  pinv_n::Int
  pinv_norm::Int
  powers::Ptr{Nothing}
  powers_len::Int
  traces_coeffs::Ptr{Nothing}
  traces_den::Int
  traces_alloc::Int
  traces_length::Int
  flag::UInt
  pol::QQPolyRingElem
  S::Symbol

  function AbsSimpleNumField(pol::QQPolyRingElem, s::Symbol, cached::Bool = false, check::Bool = true)
    check && !is_irreducible(pol) && error("Polynomial must be irreducible")
    return get_cached!(AnticNumberFieldID, (parent(pol), pol, s), cached) do
      nf = new()
      nf.pol = pol
      @ccall libflint.nf_init(nf::Ref{AbsSimpleNumField}, pol::Ref{QQPolyRingElem})::Nothing
      finalizer(_AnticNumberField_clear_fn, nf)
      nf.S = s
      return nf
    end
  end
end

const AnticNumberFieldID = CacheDictType{Tuple{QQPolyRing, QQPolyRingElem, Symbol}, AbsSimpleNumField}()


function _AnticNumberField_clear_fn(a::AbsSimpleNumField)
  @ccall libflint.nf_clear(a::Ref{AbsSimpleNumField})::Nothing
end

"""
    AbsSimpleNumFieldElem

The element type of an (absolute simple) number field, i.e., an extension
of QQ by an irreducible polynomial.

To construct such an element, one starts by constructing the field first.
Essentially never called directly.

See also [`number_field`](@ref).
"""
mutable struct AbsSimpleNumFieldElem <: SimpleNumFieldElem{QQFieldElem}
  elem_coeffs::Ptr{Nothing}
  elem_alloc::Int
  elem_length::Int
  elem_den::Int
  # end antic struct

  parent::AbsSimpleNumField

  function AbsSimpleNumFieldElem(p::AbsSimpleNumField)
    r = new()
    @ccall libflint.nf_elem_init(r::Ref{AbsSimpleNumFieldElem}, p::Ref{AbsSimpleNumField})::Nothing
    r.parent = p
    finalizer(_nf_elem_clear_fn, r)
    return r
  end

  function AbsSimpleNumFieldElem(p::AbsSimpleNumField, a::AbsSimpleNumFieldElem)
    r = new()
    @ccall libflint.nf_elem_init(r::Ref{AbsSimpleNumFieldElem}, p::Ref{AbsSimpleNumField})::Nothing
    @ccall libflint.nf_elem_set(r::Ref{AbsSimpleNumFieldElem}, a::Ref{AbsSimpleNumFieldElem}, p::Ref{AbsSimpleNumField})::Nothing
    r.parent = p
    finalizer(_nf_elem_clear_fn, r)
    return r
  end
end

function _nf_elem_clear_fn(a::AbsSimpleNumFieldElem)
  @ccall libflint.nf_elem_clear(a::Ref{AbsSimpleNumFieldElem}, a.parent::Ref{AbsSimpleNumField})::Nothing
end


################################################################################
#
#   Type unions
#
################################################################################

const AbsSimpleNumFieldElemOrPtr = Union{AbsSimpleNumFieldElem, Ref{AbsSimpleNumFieldElem}, Ptr{AbsSimpleNumFieldElem}}
