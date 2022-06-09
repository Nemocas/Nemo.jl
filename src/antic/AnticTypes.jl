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

@attributes mutable struct AnticNumberField <: SimpleNumField{fmpq}
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
   pol::fmpq_poly
   S::Symbol

   function AnticNumberField(pol::fmpq_poly, s::Symbol, cached::Bool = false, check::Bool = true)
     check && !is_irreducible(pol) && error("Polynomial must be irreducible")
     return get_cached!(AnticNumberFieldID, (parent(pol), pol, s), cached) do
        nf = new()
        nf.pol = pol
        ccall((:nf_init, libantic), Nothing, 
           (Ref{AnticNumberField}, Ref{fmpq_poly}), nf, pol)
        finalizer(_AnticNumberField_clear_fn, nf)
        nf.S = s
        return nf
      end
   end
end

const AnticNumberFieldID = Dict{Tuple{FmpqPolyRing, fmpq_poly, Symbol}, AnticNumberField}()


function _AnticNumberField_clear_fn(a::AnticNumberField)
   ccall((:nf_clear, libantic), Nothing, (Ref{AnticNumberField},), a)
end

mutable struct nf_elem <: SimpleNumFieldElem{fmpq}
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
