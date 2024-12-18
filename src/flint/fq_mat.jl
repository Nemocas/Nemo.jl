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

is_zero_initialized(::Type{FqPolyRepMatrix}) = true

################################################################################
#
#  Manipulation
#
################################################################################

function getindex!(v::FqPolyRepFieldElem, a::FqPolyRepMatrix, i::Int, j::Int)
  @boundscheck _checkbounds(a, i, j)
  GC.@preserve a begin
    z = mat_entry_ptr(a, i, j)
    @ccall libflint.fq_set(v::Ref{FqPolyRepFieldElem}, z::Ptr{FqPolyRepFieldElem})::Nothing
  end
  return v
end

@inline function getindex(a::FqPolyRepMatrix, i::Int, j::Int)
  @boundscheck _checkbounds(a, i, j)
  GC.@preserve a begin
    el = mat_entry_ptr(a, i, j)
    z = base_ring(a)()
    @ccall libflint.fq_set(z::Ref{FqPolyRepFieldElem}, el::Ptr{FqPolyRepFieldElem})::Nothing
  end
  return z
end

@inline function setindex!(a::FqPolyRepMatrix, u::FqPolyRepFieldElemOrPtr, i::Int, j::Int)
  @boundscheck _checkbounds(a, i, j)
  @ccall libflint.fq_mat_entry_set(
    a::Ref{FqPolyRepMatrix}, (i-1)::Int, (j-1)::Int, u::Ref{FqPolyRepFieldElem}, base_ring(a)::Ref{FqPolyRepField}
  )::Nothing
end

@inline function setindex!(a::FqPolyRepMatrix, u::ZZRingElem, i::Int, j::Int)
  @boundscheck _checkbounds(a, i, j)
  GC.@preserve a begin
    el = mat_entry_ptr(a, i, j)
    @ccall libflint.fq_set_fmpz(el::Ptr{FqPolyRepFieldElem}, u::Ref{ZZRingElem}, base_ring(a)::Ref{FqPolyRepField})::Nothing
  end
end

setindex!(a::FqPolyRepMatrix, u::Integer, i::Int, j::Int) = setindex!(a, base_ring(a)(u), i, j)

function setindex!(a::FqPolyRepMatrix, b::FqPolyRepMatrix, r::UnitRange{Int64}, c::UnitRange{Int64})
  _checkbounds(a, r, c)
  size(b) == (length(r), length(c)) || throw(DimensionMismatch("tried to assign a $(size(b, 1))x$(size(b, 2)) matrix to a $(length(r))x$(length(c)) destination"))
  A = view(a, r, c)
  @ccall libflint.fq_mat_set(A::Ref{FqPolyRepMatrix}, b::Ref{FqPolyRepMatrix}, base_ring(A)::Ref{FqPolyRepField})::Nothing
end

function deepcopy_internal(a::FqPolyRepMatrix, dict::IdDict)
  z = FqPolyRepMatrix(nrows(a), ncols(a), base_ring(a))
  @ccall libflint.fq_mat_set(z::Ref{FqPolyRepMatrix}, a::Ref{FqPolyRepMatrix}, base_ring(a)::Ref{FqPolyRepField})::Nothing
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
  r = @ccall libflint.fq_mat_is_zero(a::Ref{FqPolyRepMatrix}, base_ring(a)::Ref{FqPolyRepField})::Cint
  return Bool(r)
end

@inline function is_zero_entry(A::FqPolyRepMatrix, i::Int, j::Int)
  @boundscheck _checkbounds(A, i, j)
  GC.@preserve A begin
    x = mat_entry_ptr(A, i, j)
    return @ccall libflint.fq_is_zero(x::Ptr{FqPolyRepFieldElem}, base_ring(A)::Ref{FqPolyRepField})::Bool
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
  r = @ccall libflint.fq_mat_equal(a::Ref{FqPolyRepMatrix}, b::Ref{FqPolyRepMatrix}, base_ring(a)::Ref{FqPolyRepField})::Cint
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
#  @ccall libflint.fq_mat_transpose(z::Ref{FqPolyRepMatrix}, a::Ref{FqPolyRepMatrix}, base_ring(a)::Ref{FqPolyRepField})::Nothing
#  return z
#end
#
#function transpose!(a::FqPolyRepMatrix)
#  !is_square(a) && error("Matrix must be a square matrix")
#  @ccall libflint.fq_mat_transpose(a::Ref{FqPolyRepMatrix}, a::Ref{FqPolyRepMatrix}, base_ring(a)::Ref{FqPolyRepField})::Nothing
#end

###############################################################################
#
#   Row and column swapping
#
###############################################################################

function swap_rows!(x::FqPolyRepMatrix, i::Int, j::Int)
  @ccall libflint.fq_mat_swap_rows(x::Ref{FqPolyRepMatrix}, C_NULL::Ptr{Nothing}, (i - 1)::Int, (j - 1)::Int, base_ring(x)::Ref{FqPolyRepField})::Nothing
  return x
end

function swap_cols!(x::FqPolyRepMatrix, i::Int, j::Int)
  @ccall libflint.fq_mat_swap_cols(x::Ref{FqPolyRepMatrix}, C_NULL::Ptr{Nothing}, (i - 1)::Int, (j - 1)::Int, base_ring(x)::Ref{FqPolyRepField})::Nothing
  return x
end

function reverse_rows!(x::FqPolyRepMatrix)
  @ccall libflint.fq_mat_invert_rows(x::Ref{FqPolyRepMatrix}, C_NULL::Ptr{Nothing}, base_ring(x)::Ref{FqPolyRepField})::Nothing
  return x
end

function reverse_cols!(x::FqPolyRepMatrix)
  @ccall libflint.fq_mat_invert_cols(x::Ref{FqPolyRepMatrix}, C_NULL::Ptr{Nothing}, base_ring(x)::Ref{FqPolyRepField})::Nothing
  return x
end

################################################################################
#
#  Unary operators
#
################################################################################

-(x::FqPolyRepMatrix) = neg!(similar(x), x)

################################################################################
#
#  Binary operators
#
################################################################################

function +(x::FqPolyRepMatrix, y::FqPolyRepMatrix)
  check_parent(x,y)
  z = similar(x)
  add!(z, x, y)
end

function -(x::FqPolyRepMatrix, y::FqPolyRepMatrix)
  check_parent(x,y)
  z = similar(x)
  sub!(z, x, y)
  return z
end

function *(x::FqPolyRepMatrix, y::FqPolyRepMatrix)
  (base_ring(x) != base_ring(y)) && error("Base ring must be equal")
  (ncols(x) != nrows(y)) && error("Dimensions are wrong")
  z = similar(x, nrows(x), ncols(y))
  mul!(z, x, y)
  return z
end


################################################################################
#
#  Unsafe operations
#
################################################################################

function zero!(a::FqPolyRepMatrix)
  @ccall libflint.fq_mat_zero(a::Ref{FqPolyRepMatrix}, base_ring(a)::Ref{FqPolyRepField})::Nothing
  return a
end

function one!(a::FqPolyRepMatrix)
  @ccall libflint.fq_mat_one(a::Ref{FqPolyRepMatrix}, base_ring(a)::Ref{FqPolyRepField})::Nothing
  return a
end

function neg!(z::FqPolyRepMatrix, a::FqPolyRepMatrix)
  @ccall libflint.fq_mat_neg(z::Ref{FqPolyRepMatrix}, a::Ref{FqPolyRepMatrix}, base_ring(a)::Ref{FqPolyRepField})::Nothing
  return z
end

function mul!(a::FqPolyRepMatrix, b::FqPolyRepMatrix, c::FqPolyRepMatrix)
  @ccall libflint.fq_mat_mul(a::Ref{FqPolyRepMatrix}, b::Ref{FqPolyRepMatrix}, c::Ref{FqPolyRepMatrix}, base_ring(a)::Ref{FqPolyRepField})::Nothing
  return a
end

function add!(a::FqPolyRepMatrix, b::FqPolyRepMatrix, c::FqPolyRepMatrix)
  @ccall libflint.fq_mat_add(a::Ref{FqPolyRepMatrix}, b::Ref{FqPolyRepMatrix}, c::Ref{FqPolyRepMatrix}, base_ring(a)::Ref{FqPolyRepField})::Nothing
  return a
end

function sub!(a::FqPolyRepMatrix, b::FqPolyRepMatrix, c::FqPolyRepMatrix)
  @ccall libflint.fq_mat_sub(a::Ref{FqPolyRepMatrix}, b::Ref{FqPolyRepMatrix}, c::Ref{FqPolyRepMatrix}, base_ring(a)::Ref{FqPolyRepField})::Nothing
  return a
end

function mul!(z::Vector{FqPolyRepFieldElem}, a::FqPolyRepMatrix, b::Vector{FqPolyRepFieldElem})
  @ccall libflint.fq_mat_mul_vec_ptr(z::Ptr{Ref{FqPolyRepFieldElem}}, a::Ref{FqPolyRepMatrix}, b::Ptr{Ref{FqPolyRepFieldElem}}, length(b)::Int, base_ring(a)::Ref{FqPolyRepField})::Nothing
  return z
end

function mul!(z::Vector{FqPolyRepFieldElem}, a::Vector{FqPolyRepFieldElem}, b::FqPolyRepMatrix)
  @ccall libflint.fq_mat_vec_mul_ptr(z::Ptr{Ref{FqPolyRepFieldElem}}, a::Ptr{Ref{FqPolyRepFieldElem}}, length(a)::Int, b::Ref{FqPolyRepMatrix}, base_ring(b)::Ref{FqPolyRepField})::Nothing
  return z
end

function Generic.add_one!(a::FqPolyRepMatrix, i::Int, j::Int)
  @boundscheck _checkbounds(a, i, j)
  F = base_ring(a)
  GC.@preserve a begin
    x = mat_entry_ptr(a, i, j)
    # There is no fq_add_one, but only ...sub_one
    @ccall libflint.fq_neg(x::Ptr{FqPolyRepFieldElem}, x::Ptr{FqPolyRepFieldElem}, F::Ref{FqPolyRepField})::Nothing
    @ccall libflint.fq_sub_one(x::Ptr{FqPolyRepFieldElem}, x::Ptr{FqPolyRepFieldElem}, F::Ref{FqPolyRepField})::Nothing
    @ccall libflint.fq_neg(x::Ptr{FqPolyRepFieldElem}, x::Ptr{FqPolyRepFieldElem}, F::Ref{FqPolyRepField})::Nothing
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
  r = @ccall libflint.fq_mat_rref(z::Ref{FqPolyRepMatrix}, a::Ref{FqPolyRepMatrix}, base_ring(a)::Ref{FqPolyRepField})::Int
  return r, z
end

function rref!(a::FqPolyRepMatrix)
  r = @ccall libflint.fq_mat_rref(a::Ref{FqPolyRepMatrix}, a::Ref{FqPolyRepMatrix}, base_ring(a)::Ref{FqPolyRepField})::Int
  return r
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
  r = @ccall libflint.fq_mat_inv(z::Ref{FqPolyRepMatrix}, a::Ref{FqPolyRepMatrix}, base_ring(a)::Ref{FqPolyRepField})::Int
  !Bool(r) && error("Matrix not invertible")
  return z
end

################################################################################
#
#  Linear solving
#
################################################################################

Solve.matrix_normal_form_type(::FqPolyRepField) = Solve.LUTrait()
Solve.matrix_normal_form_type(::FqPolyRepMatrix) = Solve.LUTrait()

function Solve._can_solve_internal_no_check(::Solve.LUTrait, A::FqPolyRepMatrix, b::FqPolyRepMatrix, task::Symbol; side::Symbol = :left)
  if side === :left
    fl, sol, K = Solve._can_solve_internal_no_check(Solve.LUTrait(), transpose(A), transpose(b), task, side = :right)
    return fl, transpose(sol), transpose(K)
  end

  x = similar(A, ncols(A), ncols(b))
  fl = @ccall libflint.fq_mat_can_solve(x::Ref{FqPolyRepMatrix}, A::Ref{FqPolyRepMatrix}, b::Ref{FqPolyRepMatrix}, base_ring(A)::Ref{FqPolyRepField})::Cint
  if task === :only_check || task === :with_solution
    return Bool(fl), x, zero(A, 0, 0)
  end
  return Bool(fl), x, kernel(A, side = :right)
end

# Direct interface to the C functions to be able to write 'generic' code for
# different matrix types
function _solve_tril_right_flint!(x::FqPolyRepMatrix, L::FqPolyRepMatrix, B::FqPolyRepMatrix, unit::Bool)
  @ccall libflint.fq_mat_solve_tril(x::Ref{FqPolyRepMatrix}, L::Ref{FqPolyRepMatrix}, B::Ref{FqPolyRepMatrix}, Cint(unit)::Cint, base_ring(L)::Ref{FqPolyRepField})::Nothing
  return nothing
end

function _solve_triu_right_flint!(x::FqPolyRepMatrix, U::FqPolyRepMatrix, B::FqPolyRepMatrix, unit::Bool)
  @ccall libflint.fq_mat_solve_triu(x::Ref{FqPolyRepMatrix}, U::Ref{FqPolyRepMatrix}, B::Ref{FqPolyRepMatrix}, Cint(unit)::Cint, base_ring(U)::Ref{FqPolyRepField})::Nothing
  return nothing
end

################################################################################
#
#  LU decomposition
#
################################################################################

function lu!(P::Perm, x::FqPolyRepMatrix)
  P.d .-= 1

  rank = Int(@ccall libflint.fq_mat_lu(P.d::Ptr{Int}, x::Ref{FqPolyRepMatrix}, 0::Cint, base_ring(x)::Ref{FqPolyRepField})::Cint)

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
  @ccall libflint.fq_mat_window_init(z::Ref{FqPolyRepMatrix}, x::Ref{FqPolyRepMatrix}, (r1 - 1)::Int, (c1 - 1)::Int, r2::Int, c2::Int, base_ring(x)::Ref{FqPolyRepField})::Nothing
  finalizer(_fq_mat_window_clear_fn, z)
  return z
end

function Base.view(x::FqPolyRepMatrix, r::AbstractUnitRange{Int}, c::AbstractUnitRange{Int})
  return Base.view(x, first(r), first(c), last(r), last(c))
end

function _fq_mat_window_clear_fn(a::FqPolyRepMatrix)
  @ccall libflint.fq_mat_window_clear(a::Ref{FqPolyRepMatrix}, base_ring(a)::Ref{FqPolyRepField})::Nothing
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
  @ccall libflint.fq_mat_concat_horizontal(z::Ref{FqPolyRepMatrix}, x::Ref{FqPolyRepMatrix}, y::Ref{FqPolyRepMatrix}, base_ring(x)::Ref{FqPolyRepField})::Nothing
  return z
end

function vcat(x::FqPolyRepMatrix, y::FqPolyRepMatrix)
  (base_ring(x) != base_ring(y)) && error("Matrices must have same base ring")
  (x.c != y.c) && error("Matrices must have same number of columns")
  z = similar(x, nrows(x) + nrows(y), ncols(x))
  @ccall libflint.fq_mat_concat_vertical(z::Ref{FqPolyRepMatrix}, x::Ref{FqPolyRepMatrix}, y::Ref{FqPolyRepMatrix}, base_ring(x)::Ref{FqPolyRepField})::Nothing
  return z
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
  @ccall libflint.fq_mat_charpoly(p::Ref{FqPolyRepPolyRingElem}, a::Ref{FqPolyRepMatrix}, base_ring(a)::Ref{FqPolyRepField})::Nothing
  return p
end

function charpoly_danivlesky!(R::FqPolyRepPolyRing, a::FqPolyRepMatrix)
  !is_square(a) && error("Matrix must be square")
  base_ring(R) != base_ring(a) && error("Must have common base ring")
  p = R()
  @ccall libflint.fq_mat_charpoly_danilevsky(p::Ref{FqPolyRepPolyRingElem}, a::Ref{FqPolyRepMatrix}, base_ring(a)::Ref{FqPolyRepField})::Nothing
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
  @ccall libflint.fq_mat_minpoly(p::Ref{FqPolyRepPolyRingElem}, m::Ref{FqPolyRepMatrix}, base_ring(a)::Ref{FqPolyRepField})::Nothing
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

function (a::FqPolyRepMatrixSpace)(arr::AbstractMatrix{<:IntegerUnion})
  _check_dim(nrows(a), ncols(a), arr)
  return FqPolyRepMatrix(arr, base_ring(a))
end

function (a::FqPolyRepMatrixSpace)(arr::AbstractVector{<:IntegerUnion})
  _check_dim(nrows(a), ncols(a), arr)
  return FqPolyRepMatrix(nrows(a), ncols(a), arr, base_ring(a))
end

function (a::FqPolyRepMatrixSpace)(arr::AbstractMatrix{FqPolyRepFieldElem})
  _check_dim(nrows(a), ncols(a), arr)
  (length(arr) > 0 && (base_ring(a) != parent(arr[1]))) && error("Elements must have same base ring")
  return FqPolyRepMatrix(arr, base_ring(a))
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
  Base.require_one_based_indexing(arr)
  z = FqPolyRepMatrix(arr, R)
  return z
end

function matrix(R::FqPolyRepField, r::Int, c::Int, arr::AbstractVector{<: Union{FqPolyRepFieldElem, ZZRingElem, Integer}})
  _check_dim(r, c, arr)
  z = FqPolyRepMatrix(r, c, arr, R)
  return z
end

################################################################################
#
#  Kernel
#
################################################################################

function nullspace(M::FqPolyRepMatrix)
  N = similar(M, ncols(M), ncols(M))
  nullity = @ccall libflint.fq_mat_nullspace(N::Ref{FqPolyRepMatrix}, M::Ref{FqPolyRepMatrix}, base_ring(M)::Ref{FqPolyRepField})::Int
  return nullity, view(N, 1:nrows(N), 1:nullity)
end

################################################################################
#
#  Entry pointers
#
################################################################################

# each matrix entry consists of 
#   coeffs :: Ptr{Nothing}
#   alloc :: Int
#   length :: Int
# The `parent` member of struct FqPolyRepFieldElem is not replicated in each
# struct member, so we cannot use `sizeof(FqPolyRepFieldElem)`.
mat_entry_ptr(A::FqPolyRepMatrix, i::Int, j::Int) = unsafe_load(A.rows, i) + (j-1)*(sizeof(Ptr)+2*sizeof(Int))
