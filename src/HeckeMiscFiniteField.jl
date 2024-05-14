################################################################################
#
#  Defining polynomial for finite fields
#
################################################################################

defining_polynomial(F::FqPolyRepField) = minpoly(gen(F))

function defining_polynomial(Q::fqPolyRepField, P::Ring=Native.GF(Int(characteristic(Q)), cached=false))
  Pt, t = polynomial_ring(P, cached=false)
  f = Pt()
  for i = 0:Q.len-1
    j = unsafe_load(reinterpret(Ptr{Int}, Q.j), i + 1)
    a = ZZRingElem()
    ccall((:fmpz_set, libflint), Nothing, (Ref{ZZRingElem}, Int64), a, Q.a + i * sizeof(Ptr))
    setcoeff!(f, j, P(a))
  end
  return f
end
