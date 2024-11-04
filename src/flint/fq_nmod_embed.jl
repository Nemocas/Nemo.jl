###############################################################################
#
#   fq_nmod_embed.jl : Flint finite fields embeddings
#
###############################################################################

###############################################################################
#
#   Linear factor
#
###############################################################################

function linear_factor(x::fqPolyRepPolyRingElem)
  y = parent(x)()
  @ccall libflint.fq_nmod_poly_factor_split_single(y::Ref{fqPolyRepPolyRingElem}, x::Ref{fqPolyRepPolyRingElem}, base_ring(x)::Ref{fqPolyRepField})::Nothing
  return y
end

###############################################################################
#
#   Naive functions
#
###############################################################################

function embed_gens(k::fqPolyRepField, K::fqPolyRepField)
  a = k()
  b = K()
  p::Int = characteristic(k)
  R = Native.GF(p)
  PR = polynomial_ring(R, "T")[1]
  P = PR()

  @ccall libflint.fq_nmod_embed_gens(
    a::Ref{fqPolyRepFieldElem},
    b::Ref{fqPolyRepFieldElem},
    P::Ref{fpPolyRingElem},
    k::Ref{fqPolyRepField},
    K::Ref{fqPolyRepField}
  )::Nothing

  return a, b, P
end

function embed_matrices(k::fqPolyRepField, K::fqPolyRepField)

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

  @ccall libflint.fq_nmod_embed_matrices(s1::Ref{fpMatrix}, s2::Ref{fpMatrix}, a::Ref{fqPolyRepFieldElem}, k::Ref{fqPolyRepField}, b::Ref{fqPolyRepFieldElem}, K::Ref{fqPolyRepField}, P::Ref{fpPolyRingElem})::Nothing
  return s1, s2
end

function embed_matrices_pre(a::fqPolyRepFieldElem, b::fqPolyRepFieldElem, P::fpPolyRingElem)
  k = parent(a)
  K = parent(b)
  m, n = degree(k), degree(K)
  R = base_ring(P)
  s1 = zero_matrix(R, n, m)
  s2 = zero_matrix(R, m, n)

  @ccall libflint.fq_nmod_embed_matrices(s1::Ref{fpMatrix}, s2::Ref{fpMatrix}, a::Ref{fqPolyRepFieldElem}, k::Ref{fqPolyRepField}, b::Ref{fqPolyRepFieldElem}, K::Ref{fqPolyRepField}, P::Ref{fpPolyRingElem})::Nothing
  return s1, s2
end

function setcoeff!(x::fqPolyRepFieldElem, j::Int, c::Int)
  @ccall libflint.nmod_poly_set_coeff_ui(x::Ref{fqPolyRepFieldElem}, j::Int, c::UInt)::Nothing
end

function embed_pre_mat(x::fqPolyRepFieldElem, K::fqPolyRepField, M::fpMatrix)

  d = degree(parent(x))
  col = zero_matrix(base_ring(M), d, 1)

  for j in 0:(d - 1)
    col[j + 1, 1] = coeff(x, j)
  end

  product = M*col
  res = K()

  for j in degree(K):-1:1
    setcoeff!(res, j - 1, Int(data(product[j, 1])))
  end

  return res
end

################################################################################
#
#   Embedding a polynomial
#
################################################################################

function embed_polynomial(P::fqPolyRepPolyRingElem, f::FinFieldMorphism)
  S = polynomial_ring(codomain(f), "T")[1]
  return S([f(coeff(P, j)) for j in 0:degree(P)])
end
