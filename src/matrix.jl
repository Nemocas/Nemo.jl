# Types of all Flint-backed matrices
const _FieldMatTypes = Union{QQMatrix, fpMatrix, FpMatrix, FqMatrix, fqPolyRepMatrix, FqPolyRepMatrix}
const _MatTypes = Union{_FieldMatTypes, ZZMatrix, zzModMatrix, ZZModMatrix}


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

function Base.view(x::_MatTypes, r::Int, c::UnitRange{Int})
  A = view(x, r:r, c)
	return MatrixView{typeof(x), elem_type(base_ring(x))}(A)
end

function Base.view(x::_MatTypes, r::UnitRange{Int}, c::Int)
  A = view(x, r, c:c)
	return MatrixView{typeof(x), elem_type(base_ring(x))}(A)
end

################################################################################
#
#  Generic kernel (calling nullspace in flint)
#
################################################################################

function kernel(A::_FieldMatTypes; side::Symbol = :left)
  Solve.check_option(side, [:right, :left], "side")

  if side === :left
    K = kernel(transpose(A), side = :right)
    return transpose(K)
  end

  return nullspace(A)[2]
end

################################################################################
#
#  Solve context functionality
#
################################################################################

# Overwrite some solve context functionality so that it uses `transpose` and not
# `lazy_transpose`

function solve_init(A::_FieldMatTypes)
  return Solve.SolveCtx{elem_type(base_ring(A)), typeof(A), typeof(A)}(A)
end

function Solve._init_reduce_transpose(C::Solve.SolveCtx{S, T}) where {S <: FieldElem, T <: _FieldMatTypes}
  if isdefined(C, :red_transp) && isdefined(C, :trafo_transp)
    return nothing
  end

  r, R, U = Solve._rref_with_transformation(transpose(matrix(C)))
  Solve.set_rank!(C, r)
  C.red_transp = R
  C.trafo_transp = U
  return nothing
end

function Solve._can_solve_internal_no_check(C::Solve.SolveCtx{S, T}, b::T, task::Symbol; side::Symbol = :left) where {S <: FieldElem, T <: _FieldMatTypes}
  if side === :right
    fl, sol = Solve._can_solve_with_rref(b, Solve.transformation_matrix(C), rank(C), Solve.pivot_and_non_pivot_cols(C), task)
  else
    fl, sol = Solve._can_solve_with_rref(transpose(b), Solve.transformation_matrix_of_transpose(C), rank(C), Solve.pivot_and_non_pivot_cols_of_transpose(C), task)
    sol = transpose(sol)
  end
  if !fl || task !== :with_kernel
    return fl, sol, zero(b, 0, 0)
  end

  return true, sol, kernel(C, side = side)
end

function Solve.kernel(C::Solve.SolveCtx{S, T}; side::Symbol = :left) where {S <: FieldElem, T <: _FieldMatTypes}
  Solve.check_option(side, [:right, :left], "side")

  if side === :right
    return Solve._kernel_of_rref(Solve.reduced_matrix(C), rank(C), Solve.pivot_and_non_pivot_cols(C))[2]
  else
    nullity, X = Solve._kernel_of_rref(Solve.reduced_matrix_of_transpose(C), rank(C), Solve.pivot_and_non_pivot_cols_of_transpose(C))
    return transpose(X)
  end
end

################################################################################
#
#  Eigenvalues and eigenspaces
#
################################################################################

@doc raw"""
    eigenvalues(M::MatElem{T}) where T <: FieldElem

Return the eigenvalues of `M`.
"""
function eigenvalues(M::MatElem{T}) where T <: FieldElem
  @assert is_square(M)
  K = base_ring(M)
  f = charpoly(M)
  return roots(f)
end

@doc raw"""
    eigenvalues_with_multiplicities(M::MatElem{T}) where T <: FieldElem

Return the eigenvalues of `M` together with their algebraic multiplicities as a
vector of tuples.
"""
function eigenvalues_with_multiplicities(M::MatElem{T}) where T <: FieldElem
  @assert is_square(M)
  K = base_ring(M)
  Kx, x = polynomial_ring(K, "x", cached = false)
  f = charpoly(Kx, M)
  r = roots(f)
  return [ (a, valuation(f, x - a)) for a in r ]
end

@doc raw"""
    eigenvalues(L::Field, M::MatElem{T}) where T <: RingElem

Return the eigenvalues of `M` over the field `L`.
"""
function eigenvalues(L::Field, M::MatElem{T}) where T <: RingElem
  @assert is_square(M)
  M1 = change_base_ring(L, M)
  return eigenvalues(M1)
end

@doc raw"""
    eigenvalues_with_multiplicities(L::Field, M::MatElem{T}) where T <: RingElem

Return the eigenvalues of `M` over the field `L` together with their algebraic
multiplicities as a vector of tuples.
"""
function eigenvalues_with_multiplicities(L::Field, M::MatElem{T}) where T <: RingElem
  @assert is_square(M)
  M1 = change_base_ring(L, M)
  return eigenvalues_with_multiplicities(M1)
end

@doc raw"""
    eigenspace(M::MatElem{T}, lambda::T; side::Symbol = :left)
      where T <: FieldElem -> Vector{MatElem{T}}

Return a basis of the eigenspace of $M$ with respect to the eigenvalue $\lambda$.
If side is `:right`, the right eigenspace is computed, i.e. vectors $v$ such that
$Mv = \lambda v$. If side is `:left`, the left eigenspace is computed, i.e. vectors
$v$ such that $vM = \lambda v$.
"""
function eigenspace(M::MatElem{T}, lambda::T; side::Symbol = :left) where T <: FieldElem
  @assert is_square(M)
  N = deepcopy(M)
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
  L = Dict{elem_type(base_ring(M)), typeof(M)}()
  for k in S
    push!(L, k => vcat(eigenspace(M, k, side = side)))
  end
  return L
end
