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

const AnticNumberFieldID = Dict{Tuple{FmpqPolyRing, fmpq_poly, Symbol}, Field}()

mutable struct AnticNumberField <: SimpleNumField{fmpq}
   pol_coeffs::Ptr{Nothing}
   pol_den::Int
   pol_alloc::Int
   pol_length::Int
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
   auxilliary_data::Vector{Any}

   function AnticNumberField(pol::fmpq_poly, s::Symbol, cached::Bool = false, check::Bool = true)
     check && !isirreducible(pol) && error("Polynomial must be irreducible")
      if !cached
         nf = new()
         nf.pol = pol
         ccall((:nf_init, libantic), Nothing, 
            (Ref{AnticNumberField}, Ref{fmpq_poly}), nf, pol)
         finalizer(_AnticNumberField_clear_fn, nf)
         nf.S = s
         nf.auxilliary_data = Array{Any}(undef, 5)
         return nf
      else
         if haskey(AnticNumberFieldID, (parent(pol), pol, s))
            return AnticNumberFieldID[parent(pol), pol, s]::AnticNumberField
         else
            nf = new()
            nf.pol = pol
            ccall((:nf_init, libantic), Nothing, 
               (Ref{AnticNumberField}, Ref{fmpq_poly}), nf, pol)
            finalizer(_AnticNumberField_clear_fn, nf)
            nf.S = s
            nf.auxilliary_data = Vector{Any}(undef, 5)
            if cached
               AnticNumberFieldID[parent(pol), pol, s] = nf
            end
            return nf
         end
      end
   end
end

function _AnticNumberField_clear_fn(a::AnticNumberField)
   ccall((:nf_clear, libantic), Nothing, (Ref{AnticNumberField},), a)
end

mutable struct nf_elem <: SimpleNumFieldElem{fmpq}
   elem_coeffs::Ptr{Nothing}
   elem_den::Int
   elem_alloc::Int
   elem_length::Int
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
