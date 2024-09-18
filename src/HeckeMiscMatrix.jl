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
