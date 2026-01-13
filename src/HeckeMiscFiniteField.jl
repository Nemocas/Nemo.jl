function (k::fqPolyRepField)(a::Vector{fpFieldElem})
  return k(polynomial(Native.GF(Int(characteristic(k))), a))
end

###############################################################################

function _reduce(a::fqPolyRepFieldElem)
  A = parent(a)
  if a.length < 2*degree(A)
    ccall((:fq_nmod_reduce, libflint), Nothing, (Ref{fqPolyRepFieldElem}, Ref{fqPolyRepField}), a, A)
  else
    ccall((:nmod_poly_rem, libflint), Nothing, (Ref{fqPolyRepFieldElem}, Ref{fqPolyRepFieldElem}, Ref{Nothing}, Ref{Nothing}), a, a, pointer_from_objref(A)+6*sizeof(Int) + sizeof(Ptr{Nothing}), pointer_from_objref(A)+sizeof(ZZRingElem))
  end
end

function _reduce(a::FqPolyRepFieldElem)
  A = parent(a)
  #if a.length < 2*degree(A)
    ccall((:fq_reduce, libflint), Nothing, (Ref{FqPolyRepFieldElem}, Ref{FqPolyRepField}), a, A)
  #else
  #  ccall((:fmpz_mod_poly_rem, libflint), Nothing, (Ref{FqPolyRepFieldElem}, Ref{FqPolyRepFieldElem}, Ref{Nothing}, Ref{Nothing}), a, a, pointer_from_objref(A)+6*sizeof(Int) + 2*sizeof(Ptr{Nothing}), pointer_from_objref(A)+sizeof(ZZRingElem))
  #end
end


#TODO: move elsewhere - and use. There are more calls to nmod_set/reduce
function (A::fqPolyRepField)(x::zzModPolyRingElem)
  u = A()
  ccall((:fq_nmod_set, libflint), Nothing,
                     (Ref{fqPolyRepFieldElem}, Ref{zzModPolyRingElem}, Ref{fqPolyRepField}),
                                     u, x, A)
  _reduce(u)
  return u
end

function (A::fqPolyRepField)(x::fpPolyRingElem)
  u = A()
  ccall((:fq_nmod_set, libflint), Nothing,
                     (Ref{fqPolyRepFieldElem}, Ref{fpPolyRingElem}, Ref{fqPolyRepField}),
                                     u, x, A)
  _reduce(u)
  return u
end

function _nf_to_fq!(a::fqPolyRepFieldElem, b::AbsSimpleNumFieldElem, K::fqPolyRepField, a_tmp::zzModPolyRingElem)
  Nemo.nf_elem_to_nmod_poly!(a_tmp, b)
  ccall((:fq_nmod_set, libflint), Nothing,
                     (Ref{fqPolyRepFieldElem}, Ref{zzModPolyRingElem}, Ref{fqPolyRepField}),
                                     a, a_tmp, K)
  _reduce(a)
end

function _nf_to_fq!(a::fqPolyRepFieldElem, b::AbsSimpleNumFieldElem, K::fqPolyRepField, a_tmp::fpPolyRingElem)
  Nemo.nf_elem_to_gfp_poly!(a_tmp, b)
  ccall((:fq_nmod_set, libflint), Nothing,
                     (Ref{fqPolyRepFieldElem}, Ref{fpPolyRingElem}, Ref{fqPolyRepField}),
                                     a, a_tmp, K)
  _reduce(a)
end

function _nf_to_fq!(a::FqPolyRepFieldElem, b::AbsSimpleNumFieldElem, K::FqPolyRepField, a_tmp::FpPolyRingElem)
  Nemo.nf_elem_to_gfp_fmpz_poly!(a_tmp, b)
  ccall((:fq_set, libflint), Nothing,
                     (Ref{FqPolyRepFieldElem}, Ref{FpPolyRingElem}, Ref{FqPolyRepField}),
                                     a, a_tmp, K)
  _reduce(a)
end

function _nf_to_fq!(a::FqFieldElem, b::AbsSimpleNumFieldElem, K::FqField)#, a_tmp::FpPolyRingElem)
  # AbsSimpleNumFieldElem -> QQPolyRingElem
  z = QQPolyRingElem()
  ccall((:nf_elem_get_fmpq_poly, libflint), Nothing,
        (Ref{QQPolyRingElem}, Ref{AbsSimpleNumFieldElem}, Ref{AbsSimpleNumField}), z, b, parent(b))
  z.parent = Globals.Qx
  # QQPolyRingElem -> ZZPolyRingElem, ZZRingElem
  zz = ZZPolyRingElem()
  ccall((:fmpq_poly_get_numerator, libflint), Nothing, (Ref{ZZPolyRingElem}, Ref{QQPolyRingElem}), zz, z)
  zz.parent = Globals.Zx
  zzz = ZZRingElem()
  ccall((:fmpq_poly_get_denominator, libflint), Nothing, (Ref{ZZRingElem}, Ref{QQPolyRingElem}), zzz, z)
  ccall((:fq_default_set_fmpz_poly, libflint), Nothing, (Ref{FqFieldElem}, Ref{ZZPolyRingElem}, Ref{FqField}), a, zz, K)
  # invert the denominator
  c = characteristic(K)
  ccall((:fmpz_invmod, libflint), Cint,
        (Ref{ZZRingElem}, Ref{ZZRingElem}, Ref{ZZRingElem}), zzz, zzz, c)
  ccall((:fq_default_mul_fmpz, libflint), Nothing, (Ref{FqFieldElem}, Ref{FqFieldElem}, Ref{ZZRingElem}, Ref{FqField}), a, a, zzz, K)
    #ccall((:fq_set, libflint), Nothing,
  #                   (Ref{FqPolyRepFieldElem}, Ref{FpPolyRingElem}, Ref{FqPolyRepField}),
  #                                   a, a_tmp, K)
  #_reduce(a)
  return a
end
