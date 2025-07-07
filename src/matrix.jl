# Types of all FLINT-backed matrices
const _FieldMatTypes = Union{QQMatrix, fpMatrix, FpMatrix, FqMatrix, fqPolyRepMatrix, FqPolyRepMatrix}
const _MatTypes = Union{_FieldMatTypes, ZZMatrix, zzModMatrix, ZZModMatrix}

################################################################################
#
#  common functionality for views
#
################################################################################

function Base.view(x::_MatTypes, r::AbstractUnitRange{Int}, c::AbstractUnitRange{Int})
  return _view_window(x, first(r), first(c), last(r), last(c))
end

function sub(x::_MatTypes, r1::Int, c1::Int, r2::Int, c2::Int)
  return deepcopy(view(x, r1:r2, c1:c2))
end

function sub(x::_MatTypes, r::AbstractUnitRange{Int}, c::AbstractUnitRange{Int})
  return deepcopy(view(x, r, c))
end

getindex(x::_MatTypes, r::AbstractUnitRange{Int}, c::AbstractUnitRange{Int}) = sub(x, r, c)

################################################################################
#
#  Support for view(A, :, i) and view(A, i, :)
#
################################################################################

# MatrixView{MatrixType, ElemType}
struct MatrixView{S, T} <: AbstractVector{T}
  A::S
end

Base.length(V::MatrixView) = length(V.A)

Base.getindex(V::MatrixView, i::Int) = V.A[i]

Base.setindex!(V::MatrixView, z::ZZRingElem, i::Int) = (V.A[i] = z)

Base.setindex!(V::MatrixView, z, i::Int) = setindex!(V, ZZ(z), i)

Base.size(V::MatrixView) = (length(V.A), )

function Base.view(x::_MatTypes, r::Int, c::Union{Colon, AbstractUnitRange{Int}})
  A = view(x, r:r, c)
  return MatrixView{typeof(x), elem_type(base_ring(x))}(A)
end

function Base.view(x::_MatTypes, r::Union{Colon, AbstractUnitRange{Int}}, c::Int)
  A = view(x, r, c:c)
  return MatrixView{typeof(x), elem_type(base_ring(x))}(A)
end

################################################################################
#
#  Solve context functionality
#
################################################################################

# Make sure we don't use lazy_transpose for any flint backed type
function Solve.solve_context_type(NF::Solve.MatrixNormalFormTrait,
                                            ::Type{T}) where {T <: Union{
  ZZRingElem, QQFieldElem,
  fpFieldElem, FpFieldElem, FqFieldElem, fqPolyRepFieldElem, FqPolyRepFieldElem,
  zzModRingElem, ZZModRingElem,
  RealFieldElem, ArbFieldElem, ComplexFieldElem, AcbFieldElem}}
  MatType = dense_matrix_type(T)
  return Solve.SolveCtx{T, typeof(NF), MatType, MatType, MatType}
end

################################################################################
#
#  (No) lazy transpose
#
################################################################################

# Producing a LazyTransposedMatElem from a flint matrix should always be
# unintended because the resulting matrix will use generic code and not flint
Solve.lazy_transpose(A::_MatTypes) = transpose(A)

################################################################################
#
#  Solve context for matrices over finite fields
#
################################################################################

function Solve._can_solve_internal_no_check(::Solve.LUTrait, C::Solve.SolveCtx{T, Solve.LUTrait}, b::MatElem{T}, task::Symbol; side::Symbol = :left) where {T <: Union{fpFieldElem, FpFieldElem, FqFieldElem, fqPolyRepFieldElem, FqPolyRepFieldElem}}
  # Split up in separate functions to make the compiler happy
  if side === :right
    return Solve._can_solve_internal_no_check_right(Solve.LUTrait(), C, b, task)
  else
    return Solve._can_solve_internal_no_check_left(Solve.LUTrait(), C, b, task)
  end
end

function Solve._can_solve_internal_no_check_right(::Solve.LUTrait, C::Solve.SolveCtx{T, Solve.LUTrait}, b::MatElem{T}, task::Symbol) where {T <: Union{fpFieldElem, FpFieldElem, FqFieldElem, fqPolyRepFieldElem, FqPolyRepFieldElem}}
  LU = Solve.reduced_matrix(C)
  p = Solve.lu_permutation(C)
  pb = p*b
  r = rank(C)

  # Let A = matrix(C) be m x n of rank r. Then LU is build as follows (modulo
  # the permutation p).
  # For example, m = 5, n = 6, r = 3:
  #   (d b b b b b)
  #   (a d b b b b)
  #   (a a d b b b)
  #   (a a a 0 0 0)
  #   (a a a 0 0 0)
  #
  # L is the m x r matrix
  #   (1 0 0)
  #   (a 1 0)
  #   (a a 1)
  #   (a a a)
  #   (a a a)
  #
  # and U is the r x n matrix
  #   (d b b b b b)
  #   (0 d b b b b)
  #   (0 0 d b b b)
  # Notice that the diagonal entries d need not be non-zero!

  # Would be great if there were a `*_solve_lu_precomp` for the finite field
  # types in flint.

  x = similar(b, r, ncols(b))
  # Solve L x = b for the first r rows. We tell flint to pretend that there
  # are ones on the diagonal of LU (fourth argument)
  _solve_tril_right_flint!(x, view(LU, 1:r, 1:r), view(pb, 1:r, :), true)

  # Check whether x solves Lx = b also for the lower part of L
  if r < nrows(C) && view(LU, r + 1:nrows(LU), 1:r)*x != view(pb, r + 1:nrows(b), :)
    return false, zero(b, 0, 0), zero(b, 0, 0)
  end

  # Solve U y = x. We need to take extra care as U might have non-pivot columns.
  y = _solve_triu_right(view(LU, 1:r, :), x)

  fl = true
  if r < nrows(C)
    fl = Solve.permuted_matrix(C)*y == view(pb, r + 1:nrows(C), :)
  end

  if task !== :with_kernel
    return fl, y, zero(b, 0, 0)
  else
    return fl, y, kernel(C, side = :right)
  end
end

function Solve._can_solve_internal_no_check_left(::Solve.LUTrait, C::Solve.SolveCtx{T, Solve.LUTrait}, b::MatElem{T}, task::Symbol) where {T <: Union{fpFieldElem, FpFieldElem, FqFieldElem, fqPolyRepFieldElem, FqPolyRepFieldElem}}
  LU = Solve.reduced_matrix_of_transpose(C)
  p = Solve.lu_permutation_of_transpose(C)
  pbt = p*transpose(b)
  r = rank(C)

  x = similar(b, r, nrows(b))
  _solve_tril_right_flint!(x, view(LU, 1:r, 1:r), view(pbt, 1:r, :), true)

  # Check whether x solves Lx = b also for the lower part of L
  if r < ncols(C) && view(LU, r + 1:nrows(LU), 1:r)*x != view(pbt, r + 1:nrows(pbt), :)
    return false, zero(b, 0, 0), zero(b, 0, 0)
  end

  # Solve U y = x. We need to take extra care as U might have non-pivot columns.
  yy = _solve_triu_right(view(LU, 1:r, :), x)
  y = transpose(yy)

  fl = true
  if r < ncols(C)
    bp = b*p
    fl = y*Solve.permuted_matrix_of_transpose(C) == view(bp, :, r + 1:ncols(C))
  end

  if task !== :with_kernel
    return fl, y, zero(b, 0, 0)
  else
    return fl, y, kernel(C, side = :left)
  end
end

# Solves A x = B with A in upper triangular shape of full rank, so something
# like
#   ( + * * * * )
#   ( 0 0 + * * )
#   ( 0 0 0 + * )
# where + is non-zero and * is anything.
# This is a helper functions because flint can only do the case where the
# diagonal entries are non-zero.
function _solve_triu_right(A::MatT, B::MatT) where {MatT <: Union{fpMatrix, FpMatrix, FqMatrix, fqPolyRepMatrix, FqPolyRepMatrix}}
  @assert nrows(A) == nrows(B)
  pivot_cols = Int[]
  next_pivot_col = ncols(A) + 1
  @inbounds for r in nrows(A):-1:1
    for c in r:next_pivot_col - 1
      if !is_zero_entry(A, r, c)
        push!(pivot_cols, c)
        next_pivot_col = c
        break
      end
      if c == next_pivot_col - 1
        error("Matrix is not in upper triangular shape")
      end
    end
  end
  reverse!(pivot_cols)
  AA = reduce(hcat, view(A, 1:nrows(A), c:c) for c in pivot_cols; init = zero(A, nrows(A), 0))
  xx = similar(B, nrows(A), ncols(B))
  _solve_triu_right_flint!(xx, AA, B, false)
  x = zero(B, ncols(A), ncols(B))
  for i in 1:nrows(xx)
    view(x, pivot_cols[i]:pivot_cols[i], :) .= view(xx, i:i, :)
  end
  return x
end

################################################################################
#
#  Eigenvalues and eigenspaces
#
################################################################################

@doc raw"""
    eigenvalues(M::MatElem{T}) where T <: RingElem

Return the eigenvalues of `M` which lie in `base_ring(M)`.
"""
function eigenvalues(M::MatElem{T}) where T <: RingElem
  @assert is_square(M)
  return roots(charpoly(M))
end

@doc raw"""
    eigenvalues_with_multiplicities(M::MatElem{T}) where T <: FieldElem

Return the eigenvalues of `M` (which lie in `base_ring(M)`) together with their
algebraic multiplicities as a vector of tuples of (root, multiplicity).
"""
function eigenvalues_with_multiplicities(M::MatElem{T}) where T <: FieldElem
  @assert is_square(M)
  K = base_ring(M)
  f = charpoly(M)
  x = gen(parent(g))
  r = roots(f)
  return [ (a, valuation(f, x - a)) for a in r ]
end

# This function just aims to give a helpful error message if the
# argument matrix is not over a field.  It deliberately has no doc!
function eigenvalues_with_multiplicities(::MatElem{T}) where T <: RingElem
  throw(ArgumentError("Please specify the field over which to compute (since matrix is not over a field)"))
end

@doc raw"""
    eigenvalues(L::Field, M::MatElem{T}) where T <: RingElem

Return the eigenvalues of `M` over the field `L`.
"""
function eigenvalues(L::Field, M::MatElem{T}) where T <: RingElem
  @assert is_square(M)
  return roots(L, charpoly(M))
end

@doc raw"""
    eigenvalues_with_multiplicities(L::Field, M::MatElem{T}) where T <: RingElem

Return the eigenvalues of `M` over the field `L` together with their algebraic
multiplicities as a vector of tuples.
"""
function eigenvalues_with_multiplicities(L::Field, M::MatElem{T}) where T <: RingElem
  @assert is_square(M)
  f = change_base_ring(L, charpoly(M))
  r = roots(L, charpoly(M))
  x = gen(parent(f))
  return [ (a, valuation(f, x - a)) for a in r ]
end

@doc raw"""
    eigenspace(M::MatElem{T1}, lambda::T2; side::Symbol = :left)
      where {T1 <: RingElem, T2 <: RingElement} -> MatElem{T}

Return a matrix whose rows (if `side == :left`) or columns (if `side == :right`)
give a basis of the eigenspace of $M$ with respect to the eigenvalue $\lambda$.
If `side` is `:right`, the right eigenspace is computed, i.e. vectors $v$ such that
$Mv = \lambda v$. If `side` is `:left`, the left eigenspace is computed, i.e. vectors
$v$ such that $vM = \lambda v$.
"""
function eigenspace(M::MatElem{T1}, lambda::T2; side::Symbol = :left) where {T1 <: RingElem, T2 <: RingElement}
  @assert is_square(M)
  if lambda isa Rational
    lambda = QQ(lambda)
  end
  common_parent = parent(zero(base_ring(M))+lambda)
  isa(common_parent, Field) || throw(ArgumentError("Please give the eigenvalue as a field element (since matrix is not over a field)"))
  N = change_base_ring(common_parent,M)
  for i = 1:ncols(N)
    N[i, i] -= lambda
  end
  return kernel(N, side = side)
end



@doc raw"""
    eigenspaces(M::MatElem{T}; side::Symbol = :left)
      where T <: FieldElem -> Dict{T, MatElem{T}}

Return a dictionary containing the eigenvalues of $M$ as keys and bases for the
corresponding eigenspaces as values.
If side is `:right`, the right eigenspaces are computed, if it is `:left` then the
left eigenspaces are computed.

See also `eigenspace`.
"""
function eigenspaces(M::MatElem{T}; side::Symbol = :left) where T<:FieldElem
  S = eigenvalues(M)
  E = Dict{elem_type(base_ring(M)), typeof(M)}()
  for lambda in S
    push!(E, lambda => vcat(eigenspace(M, lambda; side = side)))
  end
  return E
end

# This function just aims to give a helpful error message if the
# argument matrix is not over a field.  It deliberately has no doc!
function eigenspaces(::MatElem{T}; side::Symbol = :left) where T<:RingElem
  throw(ArgumentError("Please specify the field over which to compute (since matrix is not over a field)"))
end

@doc raw"""
    eigenspaces(L::Field, M::MatElem{T}; side::Symbol = :left)
      where T <: RingElem -> Dict{T, MatElem{T}}

Return a dictionary containing the eigenvalues of $M$ over the field $L$ as keys
and bases for the corresponding eigenspaces as values.
If side is `:right`, the right eigenspaces are computed, if it is `:left` then the
left eigenspaces are computed.

See also `eigenspace`.
"""
function eigenspaces(L::Field, M::MatElem{T}; side::Symbol = :left) where T<:RingElem
  S = eigenvalues(L,M)
  M_over_L = change_base_ring(L,M)
  E = Dict{elem_type(L), typeof(M_over_L)}()
  for lambda in S
    push!(E, lambda => vcat(eigenspace(M_over_L, lambda; side = side)))
  end
  return E
end

###############################################################################
#
#   Permutation
#
###############################################################################

# Unfortunately, there is no fmpq_mat_set_perm etc. in flint
function *(P::Perm, x::_FieldMatTypes)
  z = similar(x)
  t = base_ring(x)()
  @inbounds for i = 1:nrows(x)
    for j = 1:ncols(x)
      z[P[i], j] = getindex!(t, x, i, j)
    end
  end
  return z
end

function *(x::_FieldMatTypes, P::Perm)
  z = similar(x)
  t = base_ring(x)()
  @inbounds for i = 1:nrows(x)
    for j = 1:ncols(x)
      z[i, P[j]] = getindex!(t, x, i, j)
    end
  end
  return z
end

###############################################################################
#
#  Norm
#
###############################################################################

function norm(v::Union{AcbMatrix, ArbMatrix, ComplexMatrix, RealMatrix})
  return sqrt(sum(a^2 for a in v; init=zero(base_ring(v))))
end

################################################################################
#
#  Diagonal
#
################################################################################

@doc raw"""
    diagonal(A::MatrixElem{T}) -> Vector{T}

Return the diagonal of `A` as an array.
"""
diagonal(A::MatrixElem{T}) where {T} = T[A[i, i] for i in 1:min(nrows(A), ncols(A))]

function prod_diagonal(A::MatrixElem{T}) where {T}
  return prod(diagonal(A))
end

################################################################################
#
#  Reduce "modulo" RREF
#
################################################################################

@doc raw"""
    reduce_mod!(A::MatElem{T}, B::MatElem{T}) where T <: FieldElem

For a reduced row echelon matrix $B$, reduce the rows of $A$ modulo $B$, i.e. all the pivot
columns will be zero afterwards.
"""
function reduce_mod!(A::MatElem{T}, B::MatElem{T}) where {T<:FieldElem}
  if is_rref(B)
    scale = false
  else
    scale = true
  end

  for h = 1:nrows(A)
    j = 1
    for i = 1:nrows(B)
      while iszero(B[i, j])
        j += 1
      end
      if scale
        A[h, :] -= A[h, j] * (inv(B[i, j]) * B[i, :])
      else
        A[h, :] -= A[h, j] * B[i, :]
      end
    end
  end
  return A
end

@doc raw"""
    reduce_mod(A::MatElem{T}, B::MatElem{T}) where T <: FieldElem -> MatElem

For a reduced row echelon matrix $B$, reduce $A$ modulo $B$, i.e. all the pivot
columns will be zero afterwards.
"""
function reduce_mod(A::MatElem{T}, B::MatElem{T}) where {T<:FieldElem}
  C = deepcopy(A)
  reduce_mod!(C, B)
  return C
end

