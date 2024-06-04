################################################################################
#
#  fq_mat.jl: flint fq_mat types in julia
#
################################################################################

################################################################################
#
#  Data type and parent object methods
#
################################################################################

dense_matrix_type(::Type{FqPolyRepFieldElem}) = FqPolyRepMatrix

function check_parent(x::FqPolyRepMatrix, y::FqPolyRepMatrix, throw::Bool = true)
  fl = base_ring(x) != base_ring(y)
  fl && throw && error("Residue rings must be equal")
  fl && return false
  fl = (ncols(x) != ncols(y)) && (nrows(x) != nrows(y))
  fl && throw && error("Matrices have wrong dimensions")
  return !fl
end

###############################################################################
#
#   Similar & zero
#
###############################################################################

similar(::FqPolyRepMatrix, R::FqPolyRepField, r::Int, c::Int) = FqPolyRepMatrix(r, c, R)
zero(m::FqPolyRepMatrix, R::FqPolyRepField, r::Int, c::Int) = FqPolyRepMatrix(r, c, R)

################################################################################
#
#  Manipulation
#
################################################################################

function getindex!(v::FqPolyRepFieldElem, a::FqPolyRepMatrix, i::Int, j::Int)
  @boundscheck Generic._checkbounds(a, i, j)
  GC.@preserve a begin
    z = mat_entry_ptr(a, i, j)
    ccall((:fq_set, libflint), Nothing,
          (Ref{FqPolyRepFieldElem}, Ptr{FqPolyRepFieldElem}), v, z)
  end
  return v
end

@inline function getindex(a::FqPolyRepMatrix, i::Int, j::Int)
  @boundscheck Generic._checkbounds(a, i, j)
  GC.@preserve a begin
    el = mat_entry_ptr(a, i, j)
    z = base_ring(a)()
    ccall((:fq_set, libflint), Nothing, (Ref{FqPolyRepFieldElem}, Ptr{FqPolyRepFieldElem}), z, el)
  end
  return z
end

@inline function setindex!(a::FqPolyRepMatrix, u::FqPolyRepFieldElem, i::Int, j::Int)
  @boundscheck Generic._checkbounds(a, i, j)
  ccall((:fq_mat_entry_set, libflint), Nothing,
        (Ref{FqPolyRepMatrix}, Int, Int, Ref{FqPolyRepFieldElem}, Ref{FqPolyRepField}),
        a, i - 1, j - 1, u, base_ring(a))
end

@inline function setindex!(a::FqPolyRepMatrix, u::ZZRingElem, i::Int, j::Int)
  @boundscheck Generic._checkbounds(a, i, j)
  GC.@preserve a begin
    el = mat_entry_ptr(a, i, j)
    ccall((:fq_set_fmpz, libflint), Nothing,
          (Ptr{FqPolyRepFieldElem}, Ref{ZZRingElem}, Ref{FqPolyRepField}), el, u, base_ring(a))
  end
end

setindex!(a::FqPolyRepMatrix, u::Integer, i::Int, j::Int) =
setindex!(a, base_ring(a)(u), i, j)

function setindex!(a::FqPolyRepMatrix, b::FqPolyRepMatrix, r::UnitRange{Int64}, c::UnitRange{Int64})
  _checkbounds(a, r, c)
  size(b) == (length(r), length(c)) || throw(DimensionMismatch("tried to assign a $(size(b, 1))x$(size(b, 2)) matrix to a $(length(r))x$(length(c)) destination"))
  A = view(a, r, c)
  ccall((:fq_mat_set, libflint), Nothing,
        (Ref{FqPolyRepMatrix}, Ref{FqPolyRepMatrix}, Ref{FqPolyRepField}), A, b, base_ring(A))
end

function deepcopy_internal(a::FqPolyRepMatrix, dict::IdDict)
  z = FqPolyRepMatrix(nrows(a), ncols(a), base_ring(a))
  ccall((:fq_mat_set, libflint), Nothing,
        (Ref{FqPolyRepMatrix}, Ref{FqPolyRepMatrix}, Ref{FqPolyRepField}), z, a, base_ring(a))
  return z
end

number_of_rows(a::FqPolyRepMatrix) = a.r

number_of_columns(a::FqPolyRepMatrix) = a.c

base_ring(a::FqPolyRepMatrix) = a.base_ring

function one(a::FqPolyRepMatrixSpace)
  (nrows(a) != ncols(a)) && error("Matrices must be square")
  return a(one(base_ring(a)))
end

function iszero(a::FqPolyRepMatrix)
  r = ccall((:fq_mat_is_zero, libflint), Cint,
            (Ref{FqPolyRepMatrix}, Ref{FqPolyRepField}), a, base_ring(a))
  return Bool(r)
end

@inline function is_zero_entry(A::FqPolyRepMatrix, i::Int, j::Int)
  @boundscheck Generic._checkbounds(A, i, j)
  GC.@preserve A begin
    x = mat_entry_ptr(A, i, j)
    return ccall((:fq_is_zero, libflint), Bool,
                 (Ptr{FqPolyRepFieldElem}, Ref{FqPolyRepField}), x, base_ring(A))
  end
end

################################################################################
#
#  Comparison
#
################################################################################

function ==(a::FqPolyRepMatrix, b::FqPolyRepMatrix)
  if !(a.base_ring == b.base_ring)
    return false
  end
  r = ccall((:fq_mat_equal, libflint), Cint,
            (Ref{FqPolyRepMatrix}, Ref{FqPolyRepMatrix}, Ref{FqPolyRepField}), a, b, base_ring(a))
  return Bool(r)
end

isequal(a::FqPolyRepMatrix, b::FqPolyRepMatrix) = ==(a, b)

################################################################################
#
#  Transpose
#
################################################################################

function transpose(a::FqPolyRepMatrix)
  z = FqPolyRepMatrix(ncols(a), nrows(a), base_ring(a))
  for i in 1:nrows(a)
    for j in 1:ncols(a)
      z[j, i] = a[i, j]
    end
  end
  return z
end

# There is no transpose for FqPolyRepMatrix
#function transpose(a::FqPolyRepMatrix)
#  z = FqPolyRepMatrixSpace(base_ring(a), ncols(a), nrows(a))()
#  ccall((:fq_mat_transpose, libflint), Nothing,
#        (Ref{FqPolyRepMatrix}, Ref{FqPolyRepMatrix}, Ref{FqPolyRepField}), z, a, base_ring(a))
#  return z
#end
#
#function transpose!(a::FqPolyRepMatrix)
#  !is_square(a) && error("Matrix must be a square matrix")
#  ccall((:fq_mat_transpose, libflint), Nothing,
#        (Ref{FqPolyRepMatrix}, Ref{FqPolyRepMatrix}, Ref{FqPolyRepField}), a, a, base_ring(a))
#end

###############################################################################
#
#   Row and column swapping
#
###############################################################################

function swap_rows!(x::FqPolyRepMatrix, i::Int, j::Int)
  ccall((:fq_mat_swap_rows, libflint), Nothing,
        (Ref{FqPolyRepMatrix}, Ptr{Nothing}, Int, Int, Ref{FqPolyRepField}),
        x, C_NULL, i - 1, j - 1, base_ring(x))
  return x
end

function swap_rows(x::FqPolyRepMatrix, i::Int, j::Int)
  (1 <= i <= nrows(x) && 1 <= j <= nrows(x)) || throw(BoundsError())
  y = deepcopy(x)
  return swap_rows!(y, i, j)
end

function swap_cols!(x::FqPolyRepMatrix, i::Int, j::Int)
  ccall((:fq_mat_swap_cols, libflint), Nothing,
        (Ref{FqPolyRepMatrix}, Ptr{Nothing}, Int, Int, Ref{FqPolyRepField}),
        x, C_NULL, i - 1, j - 1, base_ring(x))
  return x
end

function swap_cols(x::FqPolyRepMatrix, i::Int, j::Int)
  (1 <= i <= ncols(x) && 1 <= j <= ncols(x)) || throw(BoundsError())
  y = deepcopy(x)
  return swap_cols!(y, i, j)
end

function reverse_rows!(x::FqPolyRepMatrix)
  ccall((:fq_mat_invert_rows, libflint), Nothing,
        (Ref{FqPolyRepMatrix}, Ptr{Nothing}, Ref{FqPolyRepField}), x, C_NULL, base_ring(x))
  return x
end

reverse_rows(x::FqPolyRepMatrix) = reverse_rows!(deepcopy(x))

function reverse_cols!(x::FqPolyRepMatrix)
  ccall((:fq_mat_invert_cols, libflint), Nothing,
        (Ref{FqPolyRepMatrix}, Ptr{Nothing}, Ref{FqPolyRepField}), x, C_NULL, base_ring(x))
  return x
end

reverse_cols(x::FqPolyRepMatrix) = reverse_cols!(deepcopy(x))

################################################################################
#
#  Unary operators
#
################################################################################

function -(x::FqPolyRepMatrix)
  z = similar(x)
  ccall((:fq_mat_neg, libflint), Nothing,
        (Ref{FqPolyRepMatrix}, Ref{FqPolyRepMatrix}, Ref{FqPolyRepField}), z, x, base_ring(x))
  return z
end

################################################################################
#
#  Binary operators
#
################################################################################

function +(x::FqPolyRepMatrix, y::FqPolyRepMatrix)
  check_parent(x,y)
  z = similar(x)
  ccall((:fq_mat_add, libflint), Nothing,
        (Ref{FqPolyRepMatrix}, Ref{FqPolyRepMatrix}, Ref{FqPolyRepMatrix}, Ref{FqPolyRepField}),
        z, x, y, base_ring(x))
  return z
end

function -(x::FqPolyRepMatrix, y::FqPolyRepMatrix)
  check_parent(x,y)
  z = similar(x)
  ccall((:fq_mat_sub, libflint), Nothing,
        (Ref{FqPolyRepMatrix}, Ref{FqPolyRepMatrix}, Ref{FqPolyRepMatrix}, Ref{FqPolyRepField}),
        z, x, y, base_ring(x))

  return z
end

function *(x::FqPolyRepMatrix, y::FqPolyRepMatrix)
  (base_ring(x) != base_ring(y)) && error("Base ring must be equal")
  (ncols(x) != nrows(y)) && error("Dimensions are wrong")
  z = similar(x, nrows(x), ncols(y))
  ccall((:fq_mat_mul, libflint), Nothing,
        (Ref{FqPolyRepMatrix}, Ref{FqPolyRepMatrix}, Ref{FqPolyRepMatrix}, Ref{FqPolyRepField}), z, x, y, base_ring(x))
  return z
end


################################################################################
#
#  Unsafe operations
#
################################################################################

function mul!(a::FqPolyRepMatrix, b::FqPolyRepMatrix, c::FqPolyRepMatrix)
  ccall((:fq_mat_mul, libflint), Nothing,
        (Ref{FqPolyRepMatrix}, Ref{FqPolyRepMatrix}, Ref{FqPolyRepMatrix}, Ref{FqPolyRepField}),
        a, b, c, base_ring(a))
  return a
end

function add!(a::FqPolyRepMatrix, b::FqPolyRepMatrix, c::FqPolyRepMatrix)
  ccall((:fq_mat_add, libflint), Nothing,
        (Ref{FqPolyRepMatrix}, Ref{FqPolyRepMatrix}, Ref{FqPolyRepMatrix}, Ref{FqPolyRepField}),
        a, b, c, base_ring(a))
  return a
end

function zero!(a::FqPolyRepMatrix)
  ccall((:fq_mat_zero, libflint), Nothing,
        (Ref{FqPolyRepMatrix}, Ref{FqPolyRepField}), a, base_ring(a))
  return a
end

function mul!(z::Vector{FqPolyRepFieldElem}, a::FqPolyRepMatrix, b::Vector{FqPolyRepFieldElem})
  ccall((:fq_mat_mul_vec_ptr, libflint), Nothing,
        (Ptr{Ref{FqPolyRepFieldElem}}, Ref{FqPolyRepMatrix}, Ptr{Ref{FqPolyRepFieldElem}}, Int, Ref{FqPolyRepField}),
        z, a, b, length(b), base_ring(a))
  return z
end

function mul!(z::Vector{FqPolyRepFieldElem}, a::Vector{FqPolyRepFieldElem}, b::FqPolyRepMatrix)
  ccall((:fq_mat_vec_mul_ptr, libflint), Nothing,
        (Ptr{Ref{FqPolyRepFieldElem}}, Ptr{Ref{FqPolyRepFieldElem}}, Int, Ref{FqPolyRepMatrix}, Ref{FqPolyRepField}),
        z, a, length(a), b, base_ring(b))
  return z
end

function Generic.add_one!(a::FqPolyRepMatrix, i::Int, j::Int)
  @boundscheck Generic._checkbounds(a, i, j)
  F = base_ring(a)
  GC.@preserve a begin
    x = mat_entry_ptr(a, i, j)
    # There is no fq_add_one, but only ...sub_one
    ccall((:fq_neg, libflint), Nothing,
          (Ptr{FqPolyRepFieldElem}, Ptr{FqPolyRepFieldElem}, Ref{FqPolyRepField}),
          x, x, F)
    ccall((:fq_sub_one, libflint), Nothing,
          (Ptr{FqPolyRepFieldElem}, Ptr{FqPolyRepFieldElem}, Ref{FqPolyRepField}),
          x, x, F)
    ccall((:fq_neg, libflint), Nothing,
          (Ptr{FqPolyRepFieldElem}, Ptr{FqPolyRepFieldElem}, Ref{FqPolyRepField}),
          x, x, F)
  end
  return a
end

################################################################################
#
#  Ad hoc binary operators
#
################################################################################

function *(x::FqPolyRepMatrix, y::FqPolyRepFieldElem)
  z = similar(x)
  for i in 1:nrows(x)
    for j in 1:ncols(x)
      z[i, j] = y * x[i, j]
    end
  end
  return z
end

*(x::FqPolyRepFieldElem, y::FqPolyRepMatrix) = y * x

function *(x::FqPolyRepMatrix, y::ZZRingElem)
  return base_ring(x)(y) * x
end

*(x::ZZRingElem, y::FqPolyRepMatrix) = y * x

function *(x::FqPolyRepMatrix, y::Integer)
  return x * base_ring(x)(y)
end

*(x::Integer, y::FqPolyRepMatrix) = y * x

################################################################################
#
#  Powering
#
################################################################################

# Fall back to generic one

################################################################################
#
#  Row echelon form
#
################################################################################

function rref(a::FqPolyRepMatrix)
  z = similar(a)
  r = ccall((:fq_mat_rref, libflint), Int,
            (Ref{FqPolyRepMatrix}, Ref{FqPolyRepMatrix}, Ref{FqPolyRepField}),
            z, a, base_ring(a))
  return r, z
end

function rref!(a::FqPolyRepMatrix)
  r = ccall((:fq_mat_rref, libflint), Int,
            (Ref{FqPolyRepMatrix}, Ref{FqPolyRepMatrix}, Ref{FqPolyRepField}),
            a, a, base_ring(a))
  return r
end

#################################################################################
#
#  Trace
#
#################################################################################

function tr(a::FqPolyRepMatrix)
  !is_square(a) && error("Non-square matrix")
  n = nrows(a)
  t = zero(base_ring(a))
  for i in 1:nrows(a)
    add!(t, t, a[i, i])
  end
  return t
end

################################################################################
#
#  Determinant
#
################################################################################

function det(a::FqPolyRepMatrix)
  !is_square(a) && error("Non-square matrix")
  n = nrows(a)
  R = base_ring(a)
  if n == 0
    return zero(R)
  end
  r, p, l, u = lu(a)
  if r < n
    return zero(R)
  else
    d = one(R)
    for i in 1:nrows(u)
      mul!(d, d, u[i, i])
    end
    return (parity(p) == 0 ? d : -d)
  end
end

################################################################################
#
#  Rank
#
################################################################################

function rank(a::FqPolyRepMatrix)
  n = nrows(a)
  if n == 0
    return 0
  end
  r, _, _, _ = lu(a)
  return r
end

################################################################################
#
#  Inverse
#
################################################################################

function inv(a::FqPolyRepMatrix)
  !is_square(a) && error("Matrix must be a square matrix")
  z = similar(a)
  r = ccall((:fq_mat_inv, libflint), Int,
            (Ref{FqPolyRepMatrix}, Ref{FqPolyRepMatrix}, Ref{FqPolyRepField}), z, a, base_ring(a))
  !Bool(r) && error("Matrix not invertible")
  return z
end

################################################################################
#
#  Linear solving
#
################################################################################

function Solve._can_solve_internal_no_check(A::FqPolyRepMatrix, b::FqPolyRepMatrix, task::Symbol; side::Symbol = :left)
  check_parent(A, b)
  if side === :left
    fl, sol, K = Solve._can_solve_internal_no_check(transpose(A), transpose(b), task, side = :right)
    return fl, transpose(sol), transpose(K)
  end

  x = similar(A, ncols(A), ncols(b))
  fl = ccall((:fq_mat_can_solve, libflint), Cint,
             (Ref{FqPolyRepMatrix}, Ref{FqPolyRepMatrix}, Ref{FqPolyRepMatrix},
              Ref{FqPolyRepField}), x, A, b, base_ring(A))
  if task === :only_check || task === :with_solution
    return Bool(fl), x, zero(A, 0, 0)
  end
  return Bool(fl), x, kernel(A, side = :right)
end

# Direct interface to the C functions to be able to write 'generic' code for
# different matrix types
function _solve_tril_right_flint!(x::FqPolyRepMatrix, L::FqPolyRepMatrix, B::FqPolyRepMatrix, unit::Bool)
  ccall((:fq_mat_solve_tril, libflint), Nothing,
        (Ref{FqPolyRepMatrix}, Ref{FqPolyRepMatrix}, Ref{FqPolyRepMatrix},
         Cint, Ref{FqPolyRepField}),
        x, L, B, Cint(unit), base_ring(L))
  return nothing
end

function _solve_triu_right_flint!(x::FqPolyRepMatrix, U::FqPolyRepMatrix, B::FqPolyRepMatrix, unit::Bool)
  ccall((:fq_mat_solve_triu, libflint), Nothing,
        (Ref{FqPolyRepMatrix}, Ref{FqPolyRepMatrix}, Ref{FqPolyRepMatrix},
         Cint, Ref{FqPolyRepField}),
        x, U, B, Cint(unit), base_ring(U))
  return nothing
end

################################################################################
#
#  LU decomposition
#
################################################################################

function lu!(P::Generic.Perm, x::FqPolyRepMatrix)
  P.d .-= 1

  rank = Int(ccall((:fq_mat_lu, libflint), Cint,
                   (Ptr{Int}, Ref{FqPolyRepMatrix}, Cint, Ref{FqPolyRepField}),
                   P.d, x, 0, base_ring(x)))

  P.d .+= 1

  # flint does x == PLU instead of Px == LU (docs are wrong)
  inv!(P)

  return rank
end

function lu(x::FqPolyRepMatrix, P = SymmetricGroup(nrows(x)))
  m = nrows(x)
  n = ncols(x)
  P.n != m && error("Permutation does not match matrix")
  p = one(P)
  R = base_ring(x)
  U = deepcopy(x)

  L = similar(x, m, m)

  rank = lu!(p, U)

  for i = 1:m
    for j = 1:n
      if i > j
        L[i, j] = U[i, j]
        U[i, j] = R()
      elseif i == j
        L[i, j] = one(R)
      elseif j <= m
        L[i, j] = R()
      end
    end
  end
  return rank, p, L, U
end

################################################################################
#
#  Windowing
#
################################################################################

function Base.view(x::FqPolyRepMatrix, r1::Int, c1::Int, r2::Int, c2::Int)

  _checkrange_or_empty(nrows(x), r1, r2) ||
  Base.throw_boundserror(x, (r1:r2, c1:c2))

  _checkrange_or_empty(ncols(x), c1, c2) ||
  Base.throw_boundserror(x, (r1:r2, c1:c2))

  if (r1 > r2)
    r1 = 1
    r2 = 0
  end
  if (c1 > c2)
    c1 = 1
    c2 = 0
  end

  z = FqPolyRepMatrix()
  z.base_ring = x.base_ring
  z.view_parent = x
  ccall((:fq_mat_window_init, libflint), Nothing,
        (Ref{FqPolyRepMatrix}, Ref{FqPolyRepMatrix}, Int, Int, Int, Int, Ref{FqPolyRepField}),
        z, x, r1 - 1, c1 - 1, r2, c2, base_ring(x))
  finalizer(_fq_mat_window_clear_fn, z)
  return z
end

function Base.view(x::FqPolyRepMatrix, r::AbstractUnitRange{Int}, c::AbstractUnitRange{Int})
  return Base.view(x, first(r), first(c), last(r), last(c))
end

function _fq_mat_window_clear_fn(a::FqPolyRepMatrix)
  ccall((:fq_mat_window_clear, libflint), Nothing,
        (Ref{FqPolyRepMatrix}, Ref{FqPolyRepField}), a, base_ring(a))
end

function sub(x::FqPolyRepMatrix, r1::Int, c1::Int, r2::Int, c2::Int)
  return deepcopy(Base.view(x, r1, c1, r2, c2))
end

function sub(x::FqPolyRepMatrix, r::AbstractUnitRange{Int}, c::AbstractUnitRange{Int})
  return deepcopy(Base.view(x, r, c))
end

getindex(x::FqPolyRepMatrix, r::AbstractUnitRange{Int}, c::AbstractUnitRange{Int}) = sub(x, r, c)

################################################################################
#
#  Concatenation
#
################################################################################

function hcat(x::FqPolyRepMatrix, y::FqPolyRepMatrix)
  (base_ring(x) != base_ring(y)) && error("Matrices must have same base ring")
  (x.r != y.r) && error("Matrices must have same number of rows")
  z = similar(x, nrows(x), ncols(x) + ncols(y))
  ccall((:fq_mat_concat_horizontal, libflint), Nothing,
        (Ref{FqPolyRepMatrix}, Ref{FqPolyRepMatrix}, Ref{FqPolyRepMatrix}, Ref{FqPolyRepField}),
        z, x, y, base_ring(x))
  return z
end

function vcat(x::FqPolyRepMatrix, y::FqPolyRepMatrix)
  (base_ring(x) != base_ring(y)) && error("Matrices must have same base ring")
  (x.c != y.c) && error("Matrices must have same number of columns")
  z = similar(x, nrows(x) + nrows(y), ncols(x))
  ccall((:fq_mat_concat_vertical, libflint), Nothing,
        (Ref{FqPolyRepMatrix}, Ref{FqPolyRepMatrix}, Ref{FqPolyRepMatrix}, Ref{FqPolyRepField}),
        z, x, y, base_ring(x))
  return z
end

################################################################################
#
#  Conversion
#
################################################################################

function Array(b::FqPolyRepMatrix)
  a = Array{FqPolyRepFieldElem}(undef, b.r, b.c)
  for i = 1:nrows(b)
    for j = 1:ncols(b)
      a[i, j] = b[i, j]
    end
  end
  return a
end

################################################################################
#
#  Characteristic polynomial
#
################################################################################

function charpoly(R::FqPolyRepPolyRing, a::FqPolyRepMatrix)
  !is_square(a) && error("Matrix must be square")
  base_ring(R) != base_ring(a) && error("Must have common base ring")
  p = R()
  ccall((:fq_mat_charpoly, libflint), Nothing,
        (Ref{FqPolyRepPolyRingElem}, Ref{FqPolyRepMatrix}, Ref{FqPolyRepField}), p, a, base_ring(a))
  return p
end

function charpoly_danivlesky!(R::FqPolyRepPolyRing, a::FqPolyRepMatrix)
  !is_square(a) && error("Matrix must be square")
  base_ring(R) != base_ring(a) && error("Must have common base ring")
  p = R()
  ccall((:fq_mat_charpoly_danilevsky, libflint), Nothing,
        (Ref{FqPolyRepPolyRingElem}, Ref{FqPolyRepMatrix}, Ref{FqPolyRepField}), p, a, base_ring(a))
  return p
end


################################################################################
#
#  Minimal polynomial
#
################################################################################

function minpoly(R::FqPolyRepPolyRing, a::FqPolyRepMatrix)
  !is_square(a) && error("Matrix must be square")
  base_ring(R) != base_ring(a) && error("Must have common base ring")
  m = deepcopy(a)
  p = R()
  ccall((:fq_mat_minpoly, libflint), Nothing,
        (Ref{FqPolyRepPolyRingElem}, Ref{FqPolyRepMatrix}, Ref{FqPolyRepField}), p, m, base_ring(a))
  return p
end

###############################################################################
#
#   Promotion rules
#
###############################################################################

promote_rule(::Type{FqPolyRepMatrix}, ::Type{V}) where {V <: Integer} = FqPolyRepMatrix

promote_rule(::Type{FqPolyRepMatrix}, ::Type{FqPolyRepFieldElem}) = FqPolyRepMatrix

promote_rule(::Type{FqPolyRepMatrix}, ::Type{ZZRingElem}) = FqPolyRepMatrix

################################################################################
#
#  Parent object overloading
#
################################################################################

function (a::FqPolyRepMatrixSpace)()
  z = FqPolyRepMatrix(nrows(a), ncols(a), base_ring(a))
  return z
end

function (a::FqPolyRepMatrixSpace)(b::FqPolyRepFieldElem)
  parent(b) != base_ring(a) && error("Unable to coerce to matrix")
  return FqPolyRepMatrix(nrows(a), ncols(a), b)
end

function (a::FqPolyRepMatrixSpace)(arr::AbstractMatrix{T}) where {T <: Integer}
  _check_dim(nrows(a), ncols(a), arr)
  return FqPolyRepMatrix(nrows(a), ncols(a), arr, base_ring(a))
end

function (a::FqPolyRepMatrixSpace)(arr::AbstractVector{T}) where {T <: Integer}
  _check_dim(nrows(a), ncols(a), arr)
  return FqPolyRepMatrix(nrows(a), ncols(a), arr, base_ring(a))
end

function (a::FqPolyRepMatrixSpace)(arr::AbstractMatrix{ZZRingElem})
  _check_dim(nrows(a), ncols(a), arr)
  return FqPolyRepMatrix(nrows(a), ncols(a), arr, base_ring(a))
end

function (a::FqPolyRepMatrixSpace)(arr::AbstractVector{ZZRingElem})
  _check_dim(nrows(a), ncols(a), arr)
  return FqPolyRepMatrix(nrows(a), ncols(a), arr, base_ring(a))
end

function (a::FqPolyRepMatrixSpace)(arr::AbstractMatrix{FqPolyRepFieldElem})
  _check_dim(nrows(a), ncols(a), arr)
  (length(arr) > 0 && (base_ring(a) != parent(arr[1]))) && error("Elements must have same base ring")
  return FqPolyRepMatrix(nrows(a), ncols(a), arr, base_ring(a))
end

function (a::FqPolyRepMatrixSpace)(arr::AbstractVector{FqPolyRepFieldElem})
  _check_dim(nrows(a), ncols(a), arr)
  (length(arr) > 0 && (base_ring(a) != parent(arr[1]))) && error("Elements must have same base ring")
  return FqPolyRepMatrix(nrows(a), ncols(a), arr, base_ring(a))
end

function (a::FqPolyRepMatrixSpace)(b::ZZMatrix)
  (ncols(a) != b.c || nrows(a) != b.r) && error("Dimensions do not fit")
  return FqPolyRepMatrix(b, base_ring(a))
end

###############################################################################
#
#   Matrix constructor
#
###############################################################################

function matrix(R::FqPolyRepField, arr::AbstractMatrix{<: Union{FqPolyRepFieldElem, ZZRingElem, Integer}})
  z = FqPolyRepMatrix(size(arr, 1), size(arr, 2), arr, R)
  return z
end

function matrix(R::FqPolyRepField, r::Int, c::Int, arr::AbstractVector{<: Union{FqPolyRepFieldElem, ZZRingElem, Integer}})
  _check_dim(r, c, arr)
  z = FqPolyRepMatrix(r, c, arr, R)
  return z
end

###############################################################################
#
#  Zero matrix
#
###############################################################################

function zero_matrix(R::FqPolyRepField, r::Int, c::Int)
  if r < 0 || c < 0
    error("dimensions must not be negative")
  end
  z = FqPolyRepMatrix(r, c, R)
  return z
end

###############################################################################
#
#  Identity matrix
#
###############################################################################

function identity_matrix(R::FqPolyRepField, n::Int)
  z = zero_matrix(R, n, n)
  for i in 1:n
    z[i, i] = one(R)
  end
  return z
end

################################################################################
#
#  Kernel
#
################################################################################

function nullspace(M::FqPolyRepMatrix)
  N = similar(M, ncols(M), ncols(M))
  nullity = ccall((:fq_mat_nullspace, libflint), Int,
                  (Ref{FqPolyRepMatrix}, Ref{FqPolyRepMatrix}, Ref{FqPolyRepField}), N, M, base_ring(M))
  return nullity, view(N, 1:nrows(N), 1:nullity)
end

################################################################################
#
#  Entry pointers
#
################################################################################

@inline mat_entry_ptr(A::FqPolyRepMatrix, i::Int, j::Int) =
ccall((:fq_mat_entry, libflint), Ptr{FqPolyRepFieldElem},
      (Ref{FqPolyRepMatrix}, Int, Int), A, i - 1, j - 1)
