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
  cstruct::FlintC.nf_struct
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
@flint_fields AbsSimpleNumField
@flint_legacy_fields AbsSimpleNumField begin
  pol_coeffs = cstruct.pol.coeffs
  pol_alloc = cstruct.pol.alloc
  pol_length = cstruct.pol.length
  pol_den = cstruct.pol.den
  pinv_dinv = Ptr{Cvoid}(_union_word(cstruct.pinv, 1))
  pinv_n = _union_word(cstruct.pinv, 2) % Int
  pinv_norm = _union_word(cstruct.pinv, 3) % Int
  powers = Ptr{Cvoid}(_union_word(cstruct.powers, 1))
  powers_len = _union_word(cstruct.powers, 2) % Int
  traces_coeffs = cstruct.traces.coeffs
  traces_alloc = cstruct.traces.alloc
  traces_length = cstruct.traces.length
  traces_den = cstruct.traces.den
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
  cstruct::FlintC.nf_elem_struct

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
@flint_fields AbsSimpleNumFieldElem
@flint_extra_fields AbsSimpleNumFieldElem begin
  elem_coeffs = Ptr{Cvoid}(_union_word(cstruct, 1))
  elem_coeffs! = _set_union_word(cstruct, 1, v)
  elem_alloc = _union_word(cstruct, 2) % Int
  elem_alloc! = _set_union_word(cstruct, 2, v)
  elem_length = _union_word(cstruct, 3) % Int
  elem_length! = _set_union_word(cstruct, 3, v)
  elem_den = _union_word(cstruct, 4) % Int
  elem_den! = _set_union_word(cstruct, 4, v)
end

function _nf_elem_clear_fn(a::AbsSimpleNumFieldElem)
  @ccall libflint.nf_elem_clear(a::Ref{AbsSimpleNumFieldElem}, parent(a)::Ref{AbsSimpleNumField})::Nothing
end


################################################################################
#
#   Type unions
#
################################################################################

const AbsSimpleNumFieldElemOrPtr = TypeOrPtr{AbsSimpleNumFieldElem}
