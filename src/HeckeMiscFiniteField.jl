function (k::fqPolyRepField)(a::Vector{fpFieldElem})
  return k(polynomial(Native.GF(Int(characteristic(k))), a))
end

###############################################################################

function _reduce(a::fqPolyRepFieldElem)
  A = parent(a)
  if a.length < 2*degree(A)
    @ccall libflint.fq_nmod_reduce(a::Ref{fqPolyRepFieldElem}, A::Ref{fqPolyRepField})::Nothing
  else
    @ccall libflint.nmod_poly_rem(a::Ref{fqPolyRepFieldElem}, a::Ref{fqPolyRepFieldElem}, (pointer_from_objref(A)+6*sizeof(Int) + sizeof(Ptr{Nothing}))::Ref{Nothing}, (pointer_from_objref(A)+sizeof(ZZRingElem))::Ref{Nothing})::Nothing
  end
end

function _reduce(a::FqPolyRepFieldElem)
  A = parent(a)
  #if a.length < 2*degree(A)
    @ccall libflint.fq_reduce(a::Ref{FqPolyRepFieldElem}, A::Ref{FqPolyRepField})::Nothing
  #else
  #  @ccall libflint.fmpz_mod_poly_rem(a::Ref{FqPolyRepFieldElem}, a::Ref{FqPolyRepFieldElem}, (pointer_from_objref(A)+6*sizeof(Int) + 2*sizeof(Ptr{Nothing}))::Ref{Nothing}, (pointer_from_objref(A)+sizeof(ZZRingElem))::Ref{Nothing})::Nothing
  #end
end


#TODO: move elsewhere - and use. There are more calls to nmod_set/reduce
function (A::fqPolyRepField)(x::zzModPolyRingElem)
  u = A()
  @ccall libflint.fq_nmod_set(u::Ref{fqPolyRepFieldElem}, x::Ref{zzModPolyRingElem}, A::Ref{fqPolyRepField})::Nothing
  _reduce(u)
  return u
end

function (A::fqPolyRepField)(x::fpPolyRingElem)
  u = A()
  @ccall libflint.fq_nmod_set(u::Ref{fqPolyRepFieldElem}, x::Ref{fpPolyRingElem}, A::Ref{fqPolyRepField})::Nothing
  _reduce(u)
  return u
end

function _nf_to_fq!(a::fqPolyRepFieldElem, b::AbsSimpleNumFieldElem, K::fqPolyRepField, a_tmp::zzModPolyRingElem)
  Nemo.nf_elem_to_nmod_poly!(a_tmp, b)
  @ccall libflint.fq_nmod_set(a::Ref{fqPolyRepFieldElem}, a_tmp::Ref{zzModPolyRingElem}, K::Ref{fqPolyRepField})::Nothing
  _reduce(a)
end

function _nf_to_fq!(a::fqPolyRepFieldElem, b::AbsSimpleNumFieldElem, K::fqPolyRepField, a_tmp::fpPolyRingElem)
  Nemo.nf_elem_to_gfp_poly!(a_tmp, b)
  @ccall libflint.fq_nmod_set(a::Ref{fqPolyRepFieldElem}, a_tmp::Ref{fpPolyRingElem}, K::Ref{fqPolyRepField})::Nothing
  _reduce(a)
end

function _nf_to_fq!(a::FqPolyRepFieldElem, b::AbsSimpleNumFieldElem, K::FqPolyRepField, a_tmp::FpPolyRingElem)
  Nemo.nf_elem_to_gfp_fmpz_poly!(a_tmp, b)
  @ccall libflint.fq_set(a::Ref{FqPolyRepFieldElem}, a_tmp::Ref{FpPolyRingElem}, K::Ref{FqPolyRepField})::Nothing
  _reduce(a)
end

function _nf_to_fq!(a::FqFieldElem, b::AbsSimpleNumFieldElem, K::FqField)#, a_tmp::FpPolyRingElem)
  # AbsSimpleNumFieldElem -> QQPolyRingElem
  z = QQPolyRingElem()
  @ccall libflint.nf_elem_get_fmpq_poly(z::Ref{QQPolyRingElem}, b::Ref{AbsSimpleNumFieldElem}, parent(b)::Ref{AbsSimpleNumField})::Nothing
  z.parent = Globals.Qx
  # QQPolyRingElem -> ZZPolyRingElem, ZZRingElem
  zz = ZZPolyRingElem()
  @ccall libflint.fmpq_poly_get_numerator(zz::Ref{ZZPolyRingElem}, z::Ref{QQPolyRingElem})::Nothing
  zz.parent = Globals.Zx
  zzz = ZZRingElem()
  @ccall libflint.fmpq_poly_get_denominator(zzz::Ref{ZZRingElem}, z::Ref{QQPolyRingElem})::Nothing
  @ccall libflint.fq_default_set_fmpz_poly(a::Ref{FqFieldElem}, zz::Ref{ZZPolyRingElem}, K::Ref{FqField})::Nothing
  # invert the denominator
  c = characteristic(K)
  @ccall libflint.fmpz_invmod(zzz::Ref{ZZRingElem}, zzz::Ref{ZZRingElem}, c::Ref{ZZRingElem})::Cint
  @ccall libflint.fq_default_mul_fmpz(a::Ref{FqFieldElem}, a::Ref{FqFieldElem}, zzz::Ref{ZZRingElem}, K::Ref{FqField})::Nothing
    #ccall((:fq_set, libflint), Nothing,
  #                   (Ref{FqPolyRepFieldElem}, Ref{FpPolyRingElem}, Ref{FqPolyRepField}),
  #                                   a, a_tmp, K)
  #_reduce(a)
  return a
end
