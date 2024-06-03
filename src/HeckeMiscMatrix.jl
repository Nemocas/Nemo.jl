################################################################################
#
#  Denominator
#
################################################################################

# This function is really slow...
function denominator(M::QQMatrix)
  d = one(ZZ)
  for i in 1:nrows(M)
    for j in 1:ncols(M)
      d = lcm!(d, d, denominator(M[i, j]))
    end
  end
  return d
end

transpose!(A::Union{ZZMatrix,QQMatrix}) = is_square(A) ? transpose!(A, A) : transpose(A)

matrix(A::Matrix{ZZRingElem}) = matrix(ZZ, A)

function Array(a::ZZMatrix; S::Type{T}=ZZRingElem) where {T}
  A = Array{T}(undef, nrows(a), ncols(a))
  for i = 1:nrows(a)
    for j = 1:ncols(a)
      A[i, j] = T(a[i, j])
    end
  end
  return A
end

function is_zero_row(M::zzModMatrix, i::Int)
  zero = UInt(0)
  for j in 1:ncols(M)
    t = ccall((:nmod_mat_get_entry, libflint), Base.GMP.Limb, (Ref{zzModMatrix}, Int, Int), M, i - 1, j - 1)
    if t != zero
      return false
    end
  end
  return true
end

################################################################################
#
################################################################################

function maximum(f::typeof(abs), a::ZZMatrix)
  m = ccall((:fmpz_mat_entry, libflint), Ptr{ZZRingElem}, (Ref{ZZMatrix}, Int, Int), a, 0, 0)
  for i = 1:nrows(a)
    for j = 1:ncols(a)
      z = ccall((:fmpz_mat_entry, libflint), Ptr{ZZRingElem}, (Ref{ZZMatrix}, Int, Int), a, i - 1, j - 1)
      if ccall((:fmpz_cmpabs, libflint), Cint, (Ptr{ZZRingElem}, Ptr{ZZRingElem}), m, z) < 0
        m = z
      end
    end
  end
  r = ZZRingElem()
  ccall((:fmpz_abs, libflint), Nothing, (Ref{ZZRingElem}, Ptr{ZZRingElem}), r, m)
  return r
end

function maximum(a::ZZMatrix)
  m = ccall((:fmpz_mat_entry, libflint), Ptr{ZZRingElem}, (Ref{ZZMatrix}, Int, Int), a, 0, 0)
  for i = 1:nrows(a)
    for j = 1:ncols(a)
      z = ccall((:fmpz_mat_entry, libflint), Ptr{ZZRingElem}, (Ref{ZZMatrix}, Int, Int), a, i - 1, j - 1)
      if ccall((:fmpz_cmp, libflint), Cint, (Ptr{ZZRingElem}, Ptr{ZZRingElem}), m, z) < 0
        m = z
      end
    end
  end
  r = ZZRingElem()
  ccall((:fmpz_set, libflint), Nothing, (Ref{ZZRingElem}, Ptr{ZZRingElem}), r, m)
  return r
end

function minimum(a::ZZMatrix)
  m = ccall((:fmpz_mat_entry, libflint), Ptr{ZZRingElem}, (Ref{ZZMatrix}, Int, Int), a, 0, 0)
  for i = 1:nrows(a)
    for j = 1:ncols(a)
      z = ccall((:fmpz_mat_entry, libflint), Ptr{ZZRingElem}, (Ref{ZZMatrix}, Int, Int), a, i - 1, j - 1)
      if ccall((:fmpz_cmp, libflint), Cint, (Ptr{ZZRingElem}, Ptr{ZZRingElem}), m, z) > 0
        m = z
      end
    end
  end
  r = ZZRingElem()
  ccall((:fmpz_set, libflint), Nothing, (Ref{ZZRingElem}, Ptr{ZZRingElem}), r, m)
  return r
end

################################################################################
#
#  Lift of matrices to overrings
#
################################################################################

@doc raw"""
    lift(a::Generic.Mat{EuclideanRingResidueRingElem{ZZRingElem}}) -> ZZMatrix

It returns a lift of the matrix to the integers.
"""
function lift(a::Generic.Mat{EuclideanRingResidueRingElem{ZZRingElem}})
  z = zero_matrix(ZZ, nrows(a), ncols(a))
  for i in 1:nrows(a)
    for j in 1:ncols(a)
      z[i, j] = lift(a[i, j])
    end
  end
  return z
end

function lift(a::ZZModMatrix)
  z = zero_matrix(ZZ, nrows(a), ncols(a))
  GC.@preserve a z begin
    for i in 1:nrows(a)
      for j in 1:ncols(a)
        m = ccall((:fmpz_mat_entry, libflint), Ptr{ZZRingElem}, (Ref{ZZMatrix}, Int, Int), z, i - 1, j - 1)
        n = ccall((:fmpz_mod_mat_entry, libflint), Ptr{ZZRingElem}, (Ref{ZZModMatrix}, Int, Int), a, i - 1, j - 1)
        ccall((:fmpz_set, libflint), Nothing, (Ptr{ZZRingElem}, Ptr{ZZRingElem}), m, n)
        #z[i, j] = lift(a[i, j])
      end
    end
  end
  return z
end

function lift(x::FpMatrix)
  return map_entries(lift, x)
end

function lift(a::Generic.Mat{ZZModRingElem})
  z = zero_matrix(ZZ, nrows(a), ncols(a))
  for i in 1:nrows(a)
    for j in 1:ncols(a)
      z[i, j] = lift(a[i, j])
    end
  end
  return z
end

################################################################################
#
#  Reduce the entries of a matrix modulo p
#
################################################################################

@doc raw"""
    mod_sym!(A::Generic.Mat{AbsSimpleNumFieldElem}, m::ZZRingElem)

Inplace: reduce all entries of $A$ modulo $m$, into the symmetric residue system.
"""
function mod_sym!(A::Generic.Mat{AbsSimpleNumFieldElem}, m::ZZRingElem)
  for i = 1:nrows(A)
    for j = 1:ncols(A)
      mod_sym!(A[i, j], m)
    end
  end
end

#Returns a positive integer if A[i, j] > b, negative if A[i, j] < b, 0 otherwise
function compare_index(A::ZZMatrix, i::Int, j::Int, b::ZZRingElem)
  a = ccall((:fmpz_mat_entry, libflint), Ptr{ZZRingElem}, (Ref{ZZMatrix}, Int, Int), A, i - 1, j - 1)
  return ccall((:fmpz_cmp, libflint), Int32, (Ptr{ZZRingElem}, Ref{ZZRingElem}), a, b)
end

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
  for i = 1:nrows(g)
    for j = 1:ncols(g)
      z = ccall((:fmpz_mat_entry, libflint), Ptr{ZZRingElem}, (Ref{ZZMatrix}, Int, Int), g, i - 1, j - 1)
      if l > 0
        ccall((:fmpz_mul_2exp, libflint), Nothing, (Ptr{ZZRingElem}, Ptr{ZZRingElem}, Int), z, z, l)
      else
        ccall((:fmpz_tdiv_q_2exp, libflint), Nothing, (Ptr{ZZRingElem}, Ptr{ZZRingElem}, Int), z, z, -l)
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
      b = ccall((:fmpz_mat_entry, libflint), Ptr{ZZRingElem}, (Ref{ZZMatrix}, Int, Int), A, i - 1, i - 1)
      ccall((:fmpz_mul, libflint), Nothing, (Ref{ZZRingElem}, Ref{ZZRingElem}, Ptr{ZZRingElem}), a, a, b)
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
        m = ccall((:fmpz_mat_entry, libflint), Ptr{ZZRingElem}, (Ref{ZZMatrix}, Int, Int), M, i - 1, j - 1)
        n = ccall((:fmpz_mod_mat_entry, libflint), Ptr{ZZRingElem}, (Ref{ZZModMatrix}, Int, Int), N, i - 1, j - 1)
        ccall((:fmpz_mod, libflint), Nothing, (Ptr{ZZRingElem}, Ptr{ZZRingElem}, Ref{ZZRingElem}), n, m, R.n)
      end
    end
  end
  return N
end
