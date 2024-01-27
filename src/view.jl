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
