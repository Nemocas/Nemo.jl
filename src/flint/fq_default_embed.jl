###############################################################################
#
#   fq_default_embed.jl : Flint finite fields embeddings
#
###############################################################################

function _as_fq_finite_field(F::FqField)
  return Native.FiniteField(modulus(F), :a; cached = false)[1]
end


function _unchecked_coerce!(z::FqFieldElem, a::FqField, b::FqPolyRepFieldElem)
  x = ZZPolyRingElem()
  @ccall libflint.fq_get_fmpz_poly(x::Ref{ZZPolyRingElem}, b::Ref{FqPolyRepFieldElem}, parent(b)::Ref{FqPolyRepField})::Nothing
  @ccall libflint.fq_default_set_fmpz_poly(z::Ref{FqFieldElem}, x::Ref{ZZPolyRingElem}, a::Ref{FqField})::Nothing
end

###############################################################################
#
#   Linear factor
#
###############################################################################

function linear_factor(x::FqPolyRingElem)
  for (f, e) in factor(x)
    if degree(f) == 1
      return f
    end
  end
  error("unreachable")
end

###############################################################################
#
#   Naive functions
#
###############################################################################

function _fq_default_embed_gens(
    gen_sub::FqFieldElem,
    gen_sup::FqFieldElem,
    minpoly::FpPolyRingElem,
    sub_ctx::FqField,
    sup_ctx::FqField)

  sub_ctx1 = _as_fq_finite_field(sub_ctx)
  sup_ctx1 = _as_fq_finite_field(sup_ctx)

  gen_sub1 = zero(sub_ctx1)
  gen_sup1 = zero(sup_ctx1)

  @ccall libflint.fq_embed_gens(gen_sub1::Ref{FqPolyRepFieldElem}, gen_sup1::Ref{FqPolyRepFieldElem}, minpoly::Ref{FpPolyRingElem}, sub_ctx1::Ref{FqPolyRepField}, sup_ctx1::Ref{FqPolyRepField})::Nothing

  _unchecked_coerce!(gen_sub, sub_ctx, gen_sub1)
  _unchecked_coerce!(gen_sup, sup_ctx, gen_sup1)
end



function embed_gens(k::FqField, K::FqField)
  a = k()
  b = K()
  p = ZZRingElem(characteristic(k))::ZZRingElem
  R = Native.GF(p)
  PR = polynomial_ring(R, "T")[1]
  P = PR()

  _fq_default_embed_gens(a, b, P, k, K)
  return a, b, P
end

function _fq_default_embed_matrices(
    emb::FpMatrix,
    pro::FpMatrix,
    gen_sub::FqFieldElem,
    sub_ctx::FqField,
    gen_sup::FqFieldElem,
    sup_ctx::FqField,
    gen_minpoly::FpPolyRingElem
  )

  sub_ctx1 = _as_fq_finite_field(sub_ctx)
  sup_ctx1 = _as_fq_finite_field(sup_ctx)

  @ccall libflint.fq_embed_matrices(
    emb::Ref{FpMatrix},
    pro::Ref{FpMatrix},
    _unchecked_coerce(sub_ctx1, gen_sub)::Ref{FqPolyRepFieldElem},
    sub_ctx1::Ref{FqPolyRepField},
    _unchecked_coerce(sup_ctx1, gen_sup)::Ref{FqPolyRepFieldElem},
    sup_ctx1::Ref{FqPolyRepField},
    gen_minpoly::Ref{FpPolyRingElem}
  )::Nothing
end

function embed_matrices(k::FqField, K::FqField)
  m, n = degree(k), degree(K)
  if m == n
    T1 = modulus(k)
    T2 = modulus(parent(T1), K)
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
  _fq_default_embed_matrices(s1, s2, a, k, b, K, P)
  return s1, s2
end

function embed_matrices_pre(a::FqFieldElem, b::FqFieldElem, P::FpPolyRingElem)
  k = parent(a)
  K = parent(b)
  m, n = degree(k), degree(K)
  R = base_ring(P)
  s1 = zero_matrix(R, n, m)
  s2 = zero_matrix(R, m, n)
  _fq_default_embed_matrices(s1, s2, a, k, b, K, P)
  return s1, s2
end

function embed_pre_mat(x::FqFieldElem, K::FqField, M::FpMatrix)

  d = degree(parent(x))
  col = zero_matrix(base_ring(M), d, 1)

  for j in 0:(d - 1)
    col[j + 1, 1] = _coeff(x, j)
  end

  product = M*col
  res = FqFieldElem(K, ZZPolyRingElem([data(product[j, 1]) for j in 1:degree(K)]))
  return res
end

################################################################################
#
#   Embedding a polynomial
#
################################################################################

function embed_polynomial(P::FqPolyRingElem, f::FinFieldMorphism)
  S = polynomial_ring(codomain(f), "T")[1]
  return S([f(coeff(P, j)) for j in 0:degree(P)])
end
