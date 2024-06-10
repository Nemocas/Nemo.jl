################################################################################
#
#  gfp_fmpz_mat.jl: flint fmpz_mod_mat (matrices over Z/nZ, large prime n)
#
################################################################################

################################################################################
#
#  Data type and parent object methods
#
################################################################################

dense_matrix_type(::Type{FpFieldElem}) = FpMatrix

###############################################################################
#
#   Similar
#
###############################################################################

function similar(::MatElem, R::FpField, r::Int, c::Int)
  z = FpMatrix(R, r, c)
  return z
end

################################################################################
#
#  Manipulation
#
################################################################################

function getindex!(v::FpFieldElem, a::FpMatrix, i::Int, j::Int)
  @boundscheck _checkbounds(a, i, j)
  GC.@preserve a begin
    z = mat_entry_ptr(a, i, j)
    @ccall libflint.fmpz_mod_set_fmpz(v.data::Ref{ZZRingElem}, z::Ptr{ZZRingElem}, base_ring(a)::Ref{FpField})::Nothing
  end
  return v
end

# return plain ZZRingElem, no bounds checking
@inline function getindex_raw(a::FpMatrix, i::Int, j::Int)
  u = ZZRingElem()
  @ccall libflint.fmpz_mod_mat_get_entry(u::Ref{ZZRingElem}, a::Ref{FpMatrix}, (i - 1)::Int, (j - 1)::Int, base_ring(a).ninv::Ref{fmpz_mod_ctx_struct})::Nothing
  return u
end

@inline function getindex(a::FpMatrix, i::Int, j::Int)
  @boundscheck _checkbounds(a, i, j)
  return FpFieldElem(getindex_raw(a, i, j), base_ring(a)) # no reduction needed
end

@inline function setindex!(a::FpMatrix, u::ZZRingElem, i::Int, j::Int)
  @boundscheck _checkbounds(a, i, j)
  R = base_ring(a)
  setindex_raw!(a, mod(u, R.n), i, j)
end

@inline function setindex!(a::FpMatrix, u::FpFieldElem, i::Int, j::Int)
  @boundscheck _checkbounds(a, i, j)
  (base_ring(a) != parent(u)) && error("Parent objects must coincide")
  setindex_raw!(a, u.data, i, j) # no reduction needed
end

function setindex!(a::FpMatrix, u::Integer, i::Int, j::Int)
  setindex!(a, ZZRingElem(u), i, j)
end

# as per setindex! but no reduction mod n and no bounds checking
@inline function setindex_raw!(a::FpMatrix, u::ZZRingElem, i::Int, j::Int)
  @ccall libflint.fmpz_mod_mat_set_entry(a::Ref{FpMatrix}, (i - 1)::Int, (j - 1)::Int, u::Ref{ZZRingElem}, C_NULL::Ref{Nothing})::Nothing # ctx is not needed here
end

function setindex!(a::FpMatrix, b::FpMatrix, r::UnitRange{Int64}, c::UnitRange{Int64})
  _checkbounds(a, r, c)
  size(b) == (length(r), length(c)) || throw(DimensionMismatch("tried to assign a $(size(b, 1))x$(size(b, 2)) matrix to a $(length(r))x$(length(c)) destination"))
  A = view(a, r, c)
  @ccall libflint.fmpz_mod_mat_set(A::Ref{FpMatrix}, b::Ref{FpMatrix}, C_NULL::Ref{Nothing})::Nothing
end

function deepcopy_internal(a::FpMatrix, dict::IdDict)
  z = FpMatrix(nrows(a), ncols(a), base_ring(a).ninv)
  if isdefined(a, :base_ring)
    z.base_ring = a.base_ring
  end
  @ccall libflint.fmpz_mod_mat_set(z::Ref{FpMatrix}, a::Ref{FpMatrix}, base_ring(a).ninv::Ref{fmpz_mod_ctx_struct})::Nothing
  return z
end

number_of_rows(a::FpMatrix) = a.r

number_of_columns(a::FpMatrix) = a.c

base_ring(a::FpMatrix) = a.base_ring

function one(a::FpMatrixSpace)
  (nrows(a) != ncols(a)) && error("Matrices must be square")
  z = a()
  @ccall libflint.fmpz_mod_mat_one(z::Ref{FpMatrix}, base_ring(a).ninv::Ref{fmpz_mod_ctx_struct})::Nothing
  return z
end

function iszero(a::FpMatrix)
  r = @ccall libflint.fmpz_mod_mat_is_zero(a::Ref{FpMatrix}, base_ring(a).ninv::Ref{fmpz_mod_ctx_struct})::Cint
  return Bool(r)
end

@inline function is_zero_entry(A::FpMatrix, i::Int, j::Int)
  @boundscheck _checkbounds(A, i, j)
  GC.@preserve A begin
    x = mat_entry_ptr(A, i, j)
    return is_zero(x)
  end
end

################################################################################
#
#  Ad hoc binary operators
#
################################################################################

function *(x::FpMatrix, y::FpFieldElem)
  (base_ring(x) != parent(y)) && error("Parent objects must coincide")
  return x*y.data
end

*(x::FpFieldElem, y::FpMatrix) = y*x

################################################################################
#
#  Unsafe operations
#
################################################################################

function Generic.add_one!(a::FpMatrix, i::Int, j::Int)
  @boundscheck _checkbounds(a, i, j)
  GC.@preserve a begin
    x = mat_entry_ptr(a, i, j)
    @ccall libflint.fmpz_mod_add_si(x::Ptr{ZZRingElem}, x::Ptr{ZZRingElem}, 1::Int, base_ring(a).ninv::Ref{fmpz_mod_ctx_struct})::Nothing
  end
  return a
end

################################################################################
#
#  Windowing
#
################################################################################

function Base.view(x::FpMatrix, r1::Int, c1::Int, r2::Int, c2::Int)

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

  z = FpMatrix()
  z.base_ring = x.base_ring
  z.view_parent = x
  @ccall libflint.fmpz_mod_mat_window_init(z::Ref{FpMatrix}, x::Ref{FpMatrix}, (r1 - 1)::Int, (c1 - 1)::Int, r2::Int, c2::Int, base_ring(x).ninv::Ref{fmpz_mod_ctx_struct})::Nothing
  finalizer(_gfp_fmpz_mat_window_clear_fn, z)
  return z
end

function _gfp_fmpz_mat_window_clear_fn(a::FpMatrix)
  @ccall libflint.fmpz_mod_mat_window_clear(a::Ref{FpMatrix}, base_ring(a).ninv::Ref{fmpz_mod_ctx_struct})::Nothing
end

###############################################################################
#
#   Promotion rules
#
###############################################################################

promote_rule(::Type{FpMatrix}, ::Type{V}) where {V <: Integer} = FpMatrix

promote_rule(::Type{FpMatrix}, ::Type{FpFieldElem}) = FpMatrix

promote_rule(::Type{FpMatrix}, ::Type{ZZRingElem}) = FpMatrix

################################################################################
#
#  Inverse
#
################################################################################

function inv(a::FpMatrix)
  !is_square(a) && error("Matrix must be a square matrix")
  z = similar(a)
  r = @ccall libflint.fmpz_mod_mat_inv(z::Ref{FpMatrix}, a::Ref{FpMatrix}, base_ring(a).ninv::Ref{fmpz_mod_ctx_struct})::Int
  !Bool(r) && error("Matrix not invertible")
  return z
end

################################################################################
#
#  Parent object overloading
#
################################################################################

function (a::FpMatrixSpace)()
  z = FpMatrix(base_ring(a), nrows(a), ncols(a))
  return z
end

function (a::FpMatrixSpace)(b::FpFieldElem)
  parent(b) != base_ring(a) && error("Unable to coerce to matrix")
  M = a()  # zero
  for i in 1:min(nrows(a), ncols(a))
    M[i, i] = b
  end
  return M
end

function (a::FpMatrixSpace)(arr::AbstractMatrix{BigInt})
  _check_dim(nrows(a), ncols(a), arr)
  z = FpMatrix(nrows(a), ncols(a), base_ring(a).ninv, arr)
  z.base_ring = a.base_ring
  return z
end

function (a::FpMatrixSpace)(arr::AbstractVector{BigInt})
  _check_dim(nrows(a), ncols(a), arr)
  z = FpMatrix(nrows(a), ncols(a), base_ring(a).ninv, arr)
  z.base_ring = a.base_ring
  return z
end

function (a::FpMatrixSpace)(arr::AbstractMatrix{ZZRingElem})
  _check_dim(nrows(a), ncols(a), arr)
  z = FpMatrix(nrows(a), ncols(a), base_ring(a).ninv, arr)
  z.base_ring = a.base_ring
  return z
end

function (a::FpMatrixSpace)(arr::AbstractVector{ZZRingElem})
  _check_dim(nrows(a), ncols(a), arr)
  z = FpMatrix(nrows(a), ncols(a), base_ring(a).ninv, arr)
  z.base_ring = a.base_ring
  return z
end

function (a::FpMatrixSpace)(arr::AbstractMatrix{Int})
  _check_dim(nrows(a), ncols(a), arr)
  z = FpMatrix(nrows(a), ncols(a), base_ring(a).ninv, arr)
  z.base_ring = a.base_ring
  return z
end

function (a::FpMatrixSpace)(arr::AbstractVector{Int})
  _check_dim(nrows(a), ncols(a), arr)
  z = FpMatrix(nrows(a), ncols(a), base_ring(a).ninv, arr)
  z.base_ring = a.base_ring
  return z
end

function (a::FpMatrixSpace)(arr::AbstractMatrix{FpFieldElem})
  _check_dim(nrows(a), ncols(a), arr)
  (length(arr) > 0 && (base_ring(a) != parent(arr[1]))) && error("Elements must have same base ring")
  z = FpMatrix(nrows(a), ncols(a), base_ring(a).ninv, arr)
  z.base_ring = a.base_ring
  return z
end

function (a::FpMatrixSpace)(arr::AbstractVector{FpFieldElem})
  _check_dim(nrows(a), ncols(a), arr)
  (length(arr) > 0 && (base_ring(a) != parent(arr[1]))) && error("Elements must have same base ring")
  z = FpMatrix(nrows(a), ncols(a), base_ring(a).ninv, arr)
  z.base_ring = a.base_ring
  return z
end

###############################################################################
#
#   Matrix constructor
#
###############################################################################

function matrix(R::FpField, arr::AbstractMatrix{<: Union{FpFieldElem, ZZRingElem, Integer}})
  z = FpMatrix(size(arr, 1), size(arr, 2), R.ninv, arr)
  z.base_ring = R
  return z
end

function matrix(R::FpField, r::Int, c::Int, arr::AbstractVector{<: Union{FpFieldElem, ZZRingElem, Integer}})
  _check_dim(r, c, arr)
  z = FpMatrix(r, c, R.ninv, arr)
  z.base_ring = R
  return z
end

###############################################################################
#
#  Zero matrix
#
###############################################################################

function zero_matrix(R::FpField, r::Int, c::Int)
  if r < 0 || c < 0
    error("dimensions must not be negative")
  end
  z = FpMatrix(R, r, c)
  return z
end

################################################################################
#
#  Kernel
#
################################################################################

function nullspace(M::FpMatrix)
  N = similar(M, ncols(M), ncols(M))
  nullity = @ccall libflint.fmpz_mod_mat_nullspace(N::Ref{FpMatrix}, M::Ref{FpMatrix}, base_ring(M).ninv::Ref{fmpz_mod_ctx_struct})::Int
  return nullity, view(N, 1:nrows(N), 1:nullity)
end

################################################################################
#
#  Linear solving
#
################################################################################

Solve.matrix_normal_form_type(::FpField) = Solve.LUTrait()
Solve.matrix_normal_form_type(::FpMatrix) = Solve.LUTrait()

function Solve._can_solve_internal_no_check(::Solve.LUTrait, A::FpMatrix, b::FpMatrix, task::Symbol; side::Symbol = :left)
  if side === :left
    fl, sol, K = Solve._can_solve_internal_no_check(Solve.LUTrait(), transpose(A), transpose(b), task, side = :right)
    return fl, transpose(sol), transpose(K)
  end

  x = similar(A, ncols(A), ncols(b))
  fl = @ccall libflint.fmpz_mod_mat_can_solve(x::Ref{FpMatrix}, A::Ref{FpMatrix}, b::Ref{FpMatrix}, base_ring(x).ninv::Ref{fmpz_mod_ctx_struct})::Cint
  if task === :only_check || task === :with_solution
    return Bool(fl), x, zero(A, 0, 0)
  end
  return Bool(fl), x, kernel(A, side = :right)
end

# Direct interface to the C functions to be able to write 'generic' code for
# different matrix types
function _solve_tril_right_flint!(x::FpMatrix, L::FpMatrix, B::FpMatrix, unit::Bool)
  @ccall libflint.fmpz_mod_mat_solve_tril(x::Ref{FpMatrix}, L::Ref{FpMatrix}, B::Ref{FpMatrix}, Cint(unit)::Cint, base_ring(x).ninv::Ref{fmpz_mod_ctx_struct})::Nothing
  return nothing
end

function _solve_triu_right_flint!(x::FpMatrix, U::FpMatrix, B::FpMatrix, unit::Bool)
  @ccall libflint.fmpz_mod_mat_solve_triu(x::Ref{FpMatrix}, U::Ref{FpMatrix}, B::Ref{FpMatrix}, Cint(unit)::Cint, base_ring(x).ninv::Ref{fmpz_mod_ctx_struct})::Nothing
  return nothing
end

################################################################################
#
#  LU decomposition
#
################################################################################

function lu!(P::Perm, x::FpMatrix)
  P.d .-= 1

  rank = @ccall libflint.fmpz_mod_mat_lu(P.d::Ptr{Int}, x::Ref{FpMatrix}, Cint(false)::Cint, base_ring(x).ninv::Ref{fmpz_mod_ctx_struct})::Int

  P.d .+= 1

  # flint does x == PLU instead of Px == LU (docs are wrong)
  inv!(P)

  return rank
end

function lu(x::FpMatrix, P = SymmetricGroup(nrows(x)))
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
#  Entry pointers
#
################################################################################

mat_entry_ptr(A::FpMatrix, i::Int, j::Int) = unsafe_load(A.rows, i) + (j-1)*sizeof(ZZRingElem)
