# Support for view(A, :, i) and view(A, i, :)

struct MatrixView{S, T} <: AbstractVector{T}
  A::S
end

Base.length(V::MatrixView) = length(V.A)

Base.getindex(V::MatrixView, i::Int) = V.A[i]

Base.setindex!(V::MatrixView, z::ZZRingElem, i::Int) = (V.A[i] = z)

Base.setindex!(V::MatrixView, z, i::Int) = setindex!(V, ZZ(z), i)

Base.size(V::MatrixView) = (length(V.A), )

const _MatTypes = Union{ZZMatrix, QQMatrix, zzModMatrix, ZZModMatrix, fpMatrix, FpMatrix, FqMatrix, fqPolyRepMatrix, FqPolyRepMatrix}

function Base.view(x::_MatTypes, r::Int, c::UnitRange{Int})
  A = view(x, r:r, c)
	return MatrixView{typeof(x), typeof(base_ring(x))}(A)
end

function Base.view(x::_MatTypes, r::UnitRange{Int}, c::Int)
  A = view(x, r, c:c)
	return MatrixView{typeof(x), typeof(base_ring(x))}(A)
end

# Generic kernel (calling nullspace in flint)
const _FieldMatTypes = Union{QQMatrix, fpMatrix, FpMatrix, FqMatrix, fqPolyRepMatrix, FqPolyRepMatrix}

function kernel(A::_FieldMatTypes; side::Symbol = :left)
  Solve.check_option(side, [:right, :left], "side")

  if side === :left
    K = kernel(transpose(A), side = :right)
    return transpose(K)
  end

  return nullspace(A)[2]
end

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
