###############################################################################
#
#   AnticTypes.jl : Antic types
#
###############################################################################

###############################################################################
#
#   AnticNumberField / nf_elem
#
###############################################################################

"""
    AnticNumberField

This is the basic type for absolute extensions, i.e., an extension of QQ by some
irreducible monic polyomial with coefficients in either ZZ or QQ.

Creation is usually by calling [`number_field`](@ref), but see also
[`cyclotomic_field`](@ref) and [`cyclotomic_real_subfield`](@ref)
for some more specialized fields.
"""
@attributes mutable struct AnticNumberField <: SimpleNumField{QQFieldElem}
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

   function AnticNumberField(pol::QQPolyRingElem, s::Symbol, cached::Bool = false, check::Bool = true)
     check && !is_irreducible(pol) && error("Polynomial must be irreducible")
     return get_cached!(AnticNumberFieldID, (parent(pol), pol, s), cached) do
        nf = new()
        nf.pol = pol
        ccall((:nf_init, libantic), Nothing, 
           (Ref{AnticNumberField}, Ref{QQPolyRingElem}), nf, pol)
        finalizer(_AnticNumberField_clear_fn, nf)
        nf.S = s
        return nf
      end
   end
end

const AnticNumberFieldID = Dict{Tuple{QQPolyRing, QQPolyRingElem, Symbol}, AnticNumberField}()


function _AnticNumberField_clear_fn(a::AnticNumberField)
   ccall((:nf_clear, libantic), Nothing, (Ref{AnticNumberField},), a)
end

"""
    nf_elem
 
The element type of an (absolute simple) number field, i.e., an extension
of QQ by an irreducible polynomial.

To construct such an element, one starts by constructing the field first.
Essentially never called directly.

See also [`number_field`](@ref).
"""
mutable struct nf_elem <: SimpleNumFieldElem{QQFieldElem}
   elem_coeffs::Ptr{Nothing}
   elem_alloc::Int
   elem_length::Int
   elem_den::Int
   # end antic struct

   parent::AnticNumberField

   function nf_elem(p::AnticNumberField)
      r = new()
      ccall((:nf_elem_init, libantic), Nothing, 
            (Ref{nf_elem}, Ref{AnticNumberField}), r, p)
      r.parent = p
      finalizer(_nf_elem_clear_fn, r)
      return r
   end

   function nf_elem(p::AnticNumberField, a::nf_elem)
      r = new()
      ccall((:nf_elem_init, libantic), Nothing, 
            (Ref{nf_elem}, Ref{AnticNumberField}), r, p)
      ccall((:nf_elem_set, libantic), Nothing,
            (Ref{nf_elem}, Ref{nf_elem}, Ref{AnticNumberField}), r, a, p)
      r.parent = p
      finalizer(_nf_elem_clear_fn, r)
      return r
   end
end

function _nf_elem_clear_fn(a::nf_elem)
   ccall((:nf_elem_clear, libantic), Nothing, 
         (Ref{nf_elem}, Ref{AnticNumberField}), a, a.parent)
end
