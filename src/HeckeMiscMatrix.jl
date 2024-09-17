function round!(b::ZZMatrix, a::ArbMatrix)
  s = size(a)
  for i = 1:s[1]
    for j = 1:s[2]
      b[i, j] = round(ZZRingElem, a[i, j])
    end
  end
  return b
end


function shift!(g::ZZMatrix, l::Int)
  GC.@preserve g begin
    for i = 1:nrows(g)
      for j = 1:ncols(g)
        z = mat_entry_ptr(g, i, j)
        if l > 0
          ccall((:fmpz_mul_2exp, libflint), Nothing, (Ptr{ZZRingElem}, Ptr{ZZRingElem}, Int), z, z, l)
        else
          ccall((:fmpz_tdiv_q_2exp, libflint), Nothing, (Ptr{ZZRingElem}, Ptr{ZZRingElem}, Int), z, z, -l)
        end
      end
    end
  end
  return g
end

################################################################################
#
#  Diagonal
#
################################################################################

@doc raw"""
    diagonal(A::Mat{T}) -> Vector{T}

Returns the diagonal of `A` as an array.
"""
diagonal(A::MatrixElem{T}) where {T} = T[A[i, i] for i in 1:min(nrows(A),ncols(A))]

################################################################################
#
#  Product of the diagonal entries
#
################################################################################

function prod_diagonal(A::ZZMatrix)
  a = one(ZZRingElem)
  GC.@preserve a A begin
    for i = 1:min(nrows(A),ncols(A))
      b = mat_entry_ptr(A, i, i)
      mul!(a, a, b)
    end
  end
  return a
end

function prod_diagonal(A::MatrixElem{T}) where {T}
  @assert nrows(A) == ncols(A)
  return prod(T[A[i, i] for i = 1:nrows(A)])
end


@doc raw"""
    reduce_mod!(A::MatElem{T}, B::MatElem{T}) where T <: FieldElem

For a reduced row echelon matrix $B$, reduce $A$ modulo $B$, i.e. all the pivot
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

################################################################################
#
#  Function to convert a matrix to array
#
################################################################################

function to_array(M::QQMatrix)
  A = Vector{QQFieldElem}(undef, ncols(M) * nrows(M))
  for i = 1:nrows(M)
    for j = 1:ncols(M)
      A[(i-1)*ncols(M)+j] = M[i, j]
    end
  end
  return A
end



################################################################################
#
#  Map Entries
#
################################################################################

function map_entries(R::zzModRing, M::ZZMatrix)
  MR = zero_matrix(R, nrows(M), ncols(M))
  ccall((:fmpz_mat_get_nmod_mat, libflint), Cvoid, (Ref{zzModMatrix}, Ref{ZZMatrix}), MR, M)
  return MR
end

function map_entries(F::fpField, M::ZZMatrix)
  MR = zero_matrix(F, nrows(M), ncols(M))
  ccall((:fmpz_mat_get_nmod_mat, libflint), Cvoid, (Ref{fpMatrix}, Ref{ZZMatrix}), MR, M)
  return MR
end

function map_entries(R::ZZModRing, M::ZZMatrix)
  N = zero_matrix(R, nrows(M), ncols(M))
  GC.@preserve M N begin
    for i = 1:nrows(M)
      for j = 1:ncols(M)
        m = mat_entry_ptr(M, i, j)
        n = mat_entry_ptr(N, i, j)
        ccall((:fmpz_mod, libflint), Nothing, (Ptr{ZZRingElem}, Ptr{ZZRingElem}, Ref{ZZRingElem}), n, m, R.n)
      end
    end
  end
  return N
end
