################################################################################
#
#  Missing ad hoc operations
#
################################################################################

#TODO/ think: 
# - should those be zzModMatrix of fpMatrix
# - base_ring/ coeff_field needs to be unique?
function representation_matrix(a::fpFieldElem)
  return matrix(parent(a), 1, 1, [a])
end

function representation_matrix(a::fqPolyRepFieldElem)
  F = parent(a)
  k = quo(ZZ, Int(characteristic(F)))[1]
  k = Native.GF(Int(characteristic(F)))
  m = zero_matrix(k, degree(F), degree(F))
  ccall((:fq_nmod_embed_mul_matrix, libflint), Nothing, (Ref{fpMatrix}, Ref{fqPolyRepFieldElem}, Ref{fqPolyRepField}), m, a, F)
  ccall((:nmod_mat_transpose, libflint), Nothing, (Ref{fpMatrix}, Ref{fpMatrix}), m, m)
  return m
end

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

lift(::ZZRing, x::fpFieldElem) = lift(x)

lift(::ZZRing, x::FpFieldElem) = lift(x)
