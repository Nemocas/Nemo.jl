###############################################################################
#
#   fq_embed.jl : Flint finite fields embeddings
#
###############################################################################

###############################################################################
#
#   Linear factor
#
###############################################################################

function linear_factor(x::FqPolyRepPolyRingElem)
  y = parent(x)()
  @ccall libflint.fq_poly_factor_split_single(y::Ref{FqPolyRepPolyRingElem}, x::Ref{FqPolyRepPolyRingElem}, base_ring(x)::Ref{FqPolyRepField})::Nothing
  return y
end

###############################################################################
#
#   Naive functions
#
###############################################################################

function embed_gens(k::FqPolyRepField, K::FqPolyRepField)
  a = k()
  b = K()
  p = ZZRingElem(characteristic(k))::ZZRingElem
  R = Native.GF(p)
  PR = polynomial_ring(R, "T")[1]
  P = PR()

  @ccall libflint.fq_embed_gens(a::Ref{FqPolyRepFieldElem}, b::Ref{FqPolyRepFieldElem}, P::Ref{FpPolyRingElem}, k::Ref{FqPolyRepField}, K::Ref{FqPolyRepField})::Nothing

  return a, b, P
end

function embed_matrices(k::FqPolyRepField, K::FqPolyRepField)

  m, n = degree(k), degree(K)
  if m == n
    T1, T2 = modulus(k), modulus(K)
    if T1 == T2
      s1 = identity_matrix(base_ring(T1), n)
      s2 = s1
      return s1, s2
    end
  end

  a, b, P = embed_gens(k, K)
  R = base_ring(P)
  s1 = zero_matrix(R, n, m)
  s2 = zero_matrix(R, m, n)

  @ccall libflint.fq_embed_matrices(s1::Ref{FpMatrix}, s2::Ref{FpMatrix}, a::Ref{FqPolyRepFieldElem}, k::Ref{FqPolyRepField}, b::Ref{FqPolyRepFieldElem}, K::Ref{FqPolyRepField}, P::Ref{FpPolyRingElem})::Nothing
  return s1, s2
end

function embed_matrices_pre(a::FqPolyRepFieldElem, b::FqPolyRepFieldElem, P::FpPolyRingElem)
  k = parent(a)
  K = parent(b)
  m, n = degree(k), degree(K)
  R = base_ring(P)
  s1 = zero_matrix(R, n, m)
  s2 = zero_matrix(R, m, n)

  @ccall libflint.fq_embed_matrices(s1::Ref{FpMatrix}, s2::Ref{FpMatrix}, a::Ref{FqPolyRepFieldElem}, k::Ref{FqPolyRepField}, b::Ref{FqPolyRepFieldElem}, K::Ref{FqPolyRepField}, P::Ref{FpPolyRingElem})::Nothing
  return s1, s2
end

# dirty: internally in flint an fq_struct is just an fmpz_poly_struct
function setcoeff!(x::FqPolyRepFieldElem, j::Int, c::ZZRingElem)
  @ccall libflint.fmpz_poly_set_coeff_fmpz(x::Ref{FqPolyRepFieldElem}, j::Int, c::Ref{ZZRingElem})::Nothing
end

function embed_pre_mat(x::FqPolyRepFieldElem, K::FqPolyRepField, M::FpMatrix)

  d = degree(parent(x))
  col = zero_matrix(base_ring(M), d, 1)

  for j in 0:(d - 1)
    col[j + 1, 1] = coeff(x, j)
  end

  product = M*col
  res = K()

  for j in degree(K):-1:1
    setcoeff!(res, j - 1, data(product[j, 1]))
  end

  return res
end

################################################################################
#
#   Embedding a polynomial
#
################################################################################

function embed_polynomial(P::FqPolyRepPolyRingElem, f::FinFieldMorphism)
  S = polynomial_ring(codomain(f), "T")[1]
  return S([f(coeff(P, j)) for j in 0:degree(P)])
end
