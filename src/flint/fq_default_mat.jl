################################################################################
#
#  fq_default_mat.jl: flint fq_default_mat types in julia
#
################################################################################

################################################################################
#
#  Data type and parent object methods
#
################################################################################

dense_matrix_type(::Type{FqFieldElem}) = FqMatrix

is_zero_initialized(::Type{FqMatrix}) = true

################################################################################
#
#  Manipulation
#
################################################################################

function getindex!(v::FqFieldElem, a::FqMatrix, i::Int, j::Int)
  @boundscheck _checkbounds(a, i, j)
  @ccall libflint.fq_default_mat_entry(v::Ref{FqFieldElem}, a::Ref{FqMatrix}, (i - 1)::Int, (j - 1)::Int, base_ring(a)::Ref{FqField})::Ptr{FqFieldElem}
  return v
end

@inline function getindex(a::FqMatrix, i::Int, j::Int)
  @boundscheck _checkbounds(a, i, j)
  z = base_ring(a)()
  @ccall libflint.fq_default_mat_entry(z::Ref{FqFieldElem}, a::Ref{FqMatrix}, (i - 1)::Int, (j - 1)::Int, base_ring(a)::Ref{FqField})::Ptr{FqFieldElem}
  return z
end

@inline function setindex!(a::FqMatrix, u::FqFieldElemOrPtr, i::Int, j::Int)
  @boundscheck _checkbounds(a, i, j)
  uu = base_ring(a)(u)
  @ccall libflint.fq_default_mat_entry_set(
    a::Ref{FqMatrix}, (i-1)::Int, (j-1)::Int, uu::Ref{FqFieldElem}, base_ring(a)::Ref{FqField}
  )::Nothing
end

@inline function setindex!(a::FqMatrix, u::ZZRingElem, i::Int, j::Int)
  @boundscheck _checkbounds(a, i, j)
  @ccall libflint.fq_default_mat_entry_set_fmpz(a::Ref{FqMatrix}, (i - 1)::Int, (j - 1)::Int, u::Ref{ZZRingElem}, base_ring(a)::Ref{FqField})::Nothing
  nothing
end

setindex!(a::FqMatrix, u::Integer, i::Int, j::Int) = setindex!(a, base_ring(a)(u), i, j)

function setindex!(a::FqMatrix, b::FqMatrix, r::UnitRange{Int64}, c::UnitRange{Int64})
  _checkbounds(a, r, c)
  size(b) == (length(r), length(c)) || throw(DimensionMismatch("tried to assign a $(size(b, 1))x$(size(b, 2)) matrix to a $(length(r))x$(length(c)) destination"))
  A = view(a, r, c)
  @ccall libflint.fq_default_mat_set(A::Ref{FqMatrix}, b::Ref{FqMatrix}, base_ring(a)::Ref{FqField})::Nothing
end

function deepcopy_internal(a::FqMatrix, dict::IdDict)
  z = FqMatrix(nrows(a), ncols(a), base_ring(a))
  @ccall libflint.fq_default_mat_set(z::Ref{FqMatrix}, a::Ref{FqMatrix}, base_ring(a)::Ref{FqField})::Nothing
  return z
end

function number_of_rows(a::FqMatrix)
  return @ccall libflint.fq_default_mat_nrows(a::Ref{FqMatrix}, base_ring(a)::Ref{FqField})::Int
end

function number_of_columns(a::FqMatrix)
  return @ccall libflint.fq_default_mat_ncols(a::Ref{FqMatrix}, base_ring(a)::Ref{FqField})::Int
end

base_ring(a::FqMatrix) = a.base_ring

function one(a::FqMatrixSpace)
  (nrows(a) != ncols(a)) && error("Matrices must be square")
  return a(one(base_ring(a)))
end

function iszero(a::FqMatrix)
  r = @ccall libflint.fq_default_mat_is_zero(a::Ref{FqMatrix}, base_ring(a)::Ref{FqField})::Cint
  return Bool(r)
end

@inline function is_zero_entry(A::FqMatrix, i::Int, j::Int)
  @boundscheck _checkbounds(A, i, j)
  GC.@preserve A begin
    x = fq_default_mat_entry_ptr(A, i, j)
    return @ccall libflint.fq_default_is_zero(x::Ptr{FqFieldElem}, base_ring(A)::Ref{FqField})::Bool
  end
end

################################################################################
#
#  Comparison
#
################################################################################

function ==(a::FqMatrix, b::FqMatrix)
  if !(a.base_ring == b.base_ring)
    return false
  end
  r = @ccall libflint.fq_default_mat_equal(a::Ref{FqMatrix}, b::Ref{FqMatrix}, base_ring(a)::Ref{FqField})::Cint
  return Bool(r)
end

isequal(a::FqMatrix, b::FqMatrix) = ==(a, b)

################################################################################
#
#  Transpose
#
################################################################################

function transpose(a::FqMatrix)
  z = similar(a, ncols(a), nrows(a))
  _fq_ctx_type = _fq_default_ctx_type(base_ring(a))
  if _fq_ctx_type == _FQ_DEFAULT_NMOD
    @ccall libflint.nmod_mat_transpose(z::Ref{FqMatrix}, a::Ref{FqMatrix}, base_ring(a)::Ref{FqField})::Nothing
    return z
  elseif _fq_ctx_type == _FQ_DEFAULT_FMPZ_NMOD
    @ccall libflint.fmpz_mod_mat_transpose(z::Ref{FqMatrix}, a::Ref{FqMatrix}, base_ring(a)::Ref{FqField})::Nothing
    return z
  end
  # There is no flint functionality for the other cases
  t = base_ring(a)()
  for i in 1:nrows(a)
    for j in 1:ncols(a)
      getindex!(t, a, i, j)
      z[j, i] = t
    end
  end
  return z
end

###############################################################################
#
#   Row and column swapping
#
###############################################################################

function swap_rows!(x::FqMatrix, i::Int, j::Int)
  @ccall libflint.fq_default_mat_swap_rows(x::Ref{FqMatrix}, C_NULL::Ptr{Nothing}, (i - 1)::Int, (j - 1)::Int, base_ring(x)::Ref{FqField})::Nothing
  return x
end

function swap_cols!(x::FqMatrix, i::Int, j::Int)
  @ccall libflint.fq_default_mat_swap_cols(x::Ref{FqMatrix}, C_NULL::Ptr{Nothing}, (i - 1)::Int, (j - 1)::Int, base_ring(x)::Ref{FqField})::Nothing
  return x
end

function reverse_rows!(x::FqMatrix)
  @ccall libflint.fq_default_mat_invert_rows(x::Ref{FqMatrix}, C_NULL::Ptr{Nothing}, base_ring(x)::Ref{FqField})::Nothing
  return x
end

function reverse_cols!(x::FqMatrix)
  @ccall libflint.fq_default_mat_invert_cols(x::Ref{FqMatrix}, C_NULL::Ptr{Nothing}, base_ring(x)::Ref{FqField})::Nothing
  return x
end

################################################################################
#
#  Unary operators
#
################################################################################

-(x::FqMatrix) = neg!(similar(x), x)

################################################################################
#
#  Binary operators
#
################################################################################

function +(x::FqMatrix, y::FqMatrix)
  check_parent(x,y)
  z = similar(x)
  @ccall libflint.fq_default_mat_add(z::Ref{FqMatrix}, x::Ref{FqMatrix}, y::Ref{FqMatrix}, base_ring(x)::Ref{FqField})::Nothing
  return z
end

function -(x::FqMatrix, y::FqMatrix)
  check_parent(x,y)
  z = similar(x)
  @ccall libflint.fq_default_mat_sub(z::Ref{FqMatrix}, x::Ref{FqMatrix}, y::Ref{FqMatrix}, base_ring(x)::Ref{FqField})::Nothing

  return z
end

function *(x::FqMatrix, y::FqMatrix)
  (base_ring(x) != base_ring(y)) && error("Base ring must be equal")
  (ncols(x) != nrows(y)) && error("Dimensions are wrong")
  z = similar(x, nrows(x), ncols(y))
  @ccall libflint.fq_default_mat_mul(z::Ref{FqMatrix}, x::Ref{FqMatrix}, y::Ref{FqMatrix}, base_ring(x)::Ref{FqField})::Nothing
  return z
end


################################################################################
#
#  Unsafe operations
#
################################################################################

function zero!(a::FqMatrix)
  @ccall libflint.fq_default_mat_zero(a::Ref{FqMatrix}, base_ring(a)::Ref{FqField})::Nothing
  return a
end

function one!(a::FqMatrix)
  @ccall libflint.fq_default_mat_one(a::Ref{FqMatrix}, base_ring(a)::Ref{FqField})::Nothing
  return a
end

function neg!(z::FqMatrix, a::FqMatrix)
  @ccall libflint.fq_default_mat_neg(z::Ref{FqMatrix}, a::Ref{FqMatrix}, base_ring(a)::Ref{FqField})::Nothing
  return z
end

function mul!(a::FqMatrix, b::FqMatrix, c::FqMatrix)
  @ccall libflint.fq_default_mat_mul(a::Ref{FqMatrix}, b::Ref{FqMatrix}, c::Ref{FqMatrix}, base_ring(a)::Ref{FqField})::Nothing
  return a
end

function mul!(a::FqMatrix, b::FqMatrix, c::FqFieldElem)
  F = base_ring(a)
  if _fq_default_ctx_type(F) == _FQ_DEFAULT_NMOD
    @ccall libflint.nmod_mat_scalar_mul(a::Ref{FqMatrix}, b::Ref{FqMatrix}, UInt(lift(ZZ, c))::UInt)::Nothing
    return a
  end
  GC.@preserve a begin
    for i in 1:nrows(a)
      for j in 1:ncols(a)
        x = fq_default_mat_entry_ptr(a, i, j)
        y = fq_default_mat_entry_ptr(b, i, j)
        @ccall libflint.fq_default_mul(x::Ptr{FqFieldElem}, y::Ptr{FqFieldElem}, c::Ref{FqFieldElem}, F::Ref{FqField})::Nothing
      end
    end
  end
  return a
end

mul!(a::FqMatrix, b::FqFieldElem, c::FqMatrix) = mul!(a, c, b)

function add!(a::FqMatrix, b::FqMatrix, c::FqMatrix)
  @ccall libflint.fq_default_mat_add(a::Ref{FqMatrix}, b::Ref{FqMatrix}, c::Ref{FqMatrix}, base_ring(a)::Ref{FqField})::Nothing
  return a
end

function Generic.add_one!(a::FqMatrix, i::Int, j::Int)
  @boundscheck _checkbounds(a, i, j)
  F = base_ring(a)
  GC.@preserve a begin
    x = fq_default_mat_entry_ptr(a, i, j)
    # There is no fq_default_add_one, but only ...sub_one
    @ccall libflint.fq_default_neg(x::Ptr{FqFieldElem}, x::Ptr{FqFieldElem}, F::Ref{FqField})::Nothing
    @ccall libflint.fq_default_sub_one(x::Ptr{FqFieldElem}, x::Ptr{FqFieldElem}, F::Ref{FqField})::Nothing
    @ccall libflint.fq_default_neg(x::Ptr{FqFieldElem}, x::Ptr{FqFieldElem}, F::Ref{FqField})::Nothing
  end
  return a
end

################################################################################
#
#  Ad hoc binary operators
#
################################################################################

function *(x::FqMatrix, y::FqFieldElem)
  z = similar(x)
  for i in 1:nrows(x)
    for j in 1:ncols(x)
      z[i, j] = y * x[i, j]
    end
  end
  return z
end

*(x::FqFieldElem, y::FqMatrix) = y * x

function *(x::FqMatrix, y::ZZRingElem)
  return base_ring(x)(y) * x
end

*(x::ZZRingElem, y::FqMatrix) = y * x

function *(x::FqMatrix, y::Integer)
  return x * base_ring(x)(y)
end

*(x::Integer, y::FqMatrix) = y * x

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

function rref(a::FqMatrix)
  z = similar(a)
  r = @ccall libflint.fq_default_mat_rref(z::Ref{FqMatrix}, a::Ref{FqMatrix}, base_ring(a)::Ref{FqField})::Int
  return r, z
end

function rref!(a::FqMatrix)
  r = @ccall libflint.fq_default_mat_rref(a::Ref{FqMatrix}, a::Ref{FqMatrix}, base_ring(a)::Ref{FqField})::Int
  return r
end

################################################################################
#
#  Determinant
#
################################################################################

function det(a::FqMatrix)
  !is_square(a) && error("Non-square matrix")
  n = nrows(a)
  R = base_ring(a)
  if n == 0
    return one(R)
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

function rank(a::FqMatrix)
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

function inv(a::FqMatrix)
  !is_square(a) && error("Matrix must be a square matrix")
  z = similar(a)
  r = @ccall libflint.fq_default_mat_inv(z::Ref{FqMatrix}, a::Ref{FqMatrix}, base_ring(a)::Ref{FqField})::Int
  !Bool(r) && error("Matrix not invertible")
  return z
end

################################################################################
#
#  Linear solving
#
################################################################################

Solve.matrix_normal_form_type(::FqField) = Solve.LUTrait()
Solve.matrix_normal_form_type(::FqMatrix) = Solve.LUTrait()

function Solve._can_solve_internal_no_check(::Solve.LUTrait, A::FqMatrix, b::FqMatrix, task::Symbol; side::Symbol = :left)
  if side === :left
    fl, sol, K = Solve._can_solve_internal_no_check(Solve.LUTrait(), transpose(A), transpose(b), task, side = :right)
    return fl, transpose(sol), transpose(K)
  end

  x = similar(A, ncols(A), ncols(b))
  fl = @ccall libflint.fq_default_mat_can_solve(x::Ref{FqMatrix}, A::Ref{FqMatrix}, b::Ref{FqMatrix}, base_ring(A)::Ref{FqField})::Cint
  if task === :only_check || task === :with_solution
    return Bool(fl), x, zero(A, 0, 0)
  end
  return Bool(fl), x, kernel(A, side = :right)
end

# Direct interface to the C functions to be able to write 'generic' code for
# different matrix types
function _solve_tril_right_flint!(x::FqMatrix, L::FqMatrix, B::FqMatrix, unit::Bool)
  @ccall libflint.fq_default_mat_solve_tril(x::Ref{FqMatrix}, L::Ref{FqMatrix}, B::Ref{FqMatrix}, Cint(unit)::Cint, base_ring(L)::Ref{FqField})::Nothing
  return nothing
end

function _solve_triu_right_flint!(x::FqMatrix, U::FqMatrix, B::FqMatrix, unit::Bool)
  @ccall libflint.fq_default_mat_solve_triu(x::Ref{FqMatrix}, U::Ref{FqMatrix}, B::Ref{FqMatrix}, Cint(unit)::Cint, base_ring(U)::Ref{FqField})::Nothing
  return nothing
end

################################################################################
#
#  LU decomposition
#
################################################################################

function lu!(P::Perm, x::FqMatrix)
  P.d .-= 1
  rank = Int(@ccall libflint.fq_default_mat_lu(P.d::Ptr{Int}, x::Ref{FqMatrix}, 0::Cint, base_ring(x)::Ref{FqField})::Cint)
  P.d .+= 1

  inv!(P) # FLINT does PLU = x instead of Px = LU

  return rank
end

function lu(x::FqMatrix, P = SymmetricGroup(nrows(x)))
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

function Base.view(x::FqMatrix, r1::Int, c1::Int, r2::Int, c2::Int)

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

  z = FqMatrix()
  z.base_ring = x.base_ring
  z.view_parent = x
  @ccall libflint.fq_default_mat_window_init(z::Ref{FqMatrix}, x::Ref{FqMatrix}, (r1 - 1)::Int, (c1 - 1)::Int, r2::Int, c2::Int, base_ring(x)::Ref{FqField})::Nothing
  finalizer(_fq_default_mat_window_clear_fn, z)
  return z
end

function Base.view(x::FqMatrix, r::AbstractUnitRange{Int}, c::AbstractUnitRange{Int})
  return Base.view(x, first(r), first(c), last(r), last(c))
end

function _fq_default_mat_window_clear_fn(a::FqMatrix)
  @ccall libflint.fq_default_mat_window_clear(a::Ref{FqMatrix}, base_ring(a)::Ref{FqField})::Nothing
end

function sub(x::FqMatrix, r1::Int, c1::Int, r2::Int, c2::Int)
  return deepcopy(Base.view(x, r1, c1, r2, c2))
end

function sub(x::FqMatrix, r::AbstractUnitRange{Int}, c::AbstractUnitRange{Int})
  return deepcopy(Base.view(x, r, c))
end

getindex(x::FqMatrix, r::AbstractUnitRange{Int}, c::AbstractUnitRange{Int}) = sub(x, r, c)

################################################################################
#
#  Concatenation
#
################################################################################

function hcat(x::FqMatrix, y::FqMatrix)
  (base_ring(x) != base_ring(y)) && error("Matrices must have same base ring")
  (nrows(x) != nrows(y)) && error("Matrices must have same number of rows")
  z = similar(x, nrows(x), ncols(x) + ncols(y))
  @ccall libflint.fq_default_mat_concat_horizontal(z::Ref{FqMatrix}, x::Ref{FqMatrix}, y::Ref{FqMatrix}, base_ring(x)::Ref{FqField})::Nothing
  return z
end

function vcat(x::FqMatrix, y::FqMatrix)
  (base_ring(x) != base_ring(y)) && error("Matrices must have same base ring")
  (ncols(x) != ncols(y)) && error("Matrices must have same number of columns")
  z = similar(x, nrows(x) + nrows(y), ncols(x))
  @ccall libflint.fq_default_mat_concat_vertical(z::Ref{FqMatrix}, x::Ref{FqMatrix}, y::Ref{FqMatrix}, base_ring(x)::Ref{FqField})::Nothing
  return z
end

################################################################################
#
#  Characteristic polynomial
#
################################################################################

function charpoly(R::FqPolyRing, a::FqMatrix)
  !is_square(a) && error("Matrix must be square")
  base_ring(R) != base_ring(a) && error("Must have common base ring")
  p = R()
  @ccall libflint.fq_default_mat_charpoly(p::Ref{FqPolyRingElem}, a::Ref{FqMatrix}, base_ring(a)::Ref{FqField})::Nothing
  return p
end

function charpoly_danivlesky!(R::FqPolyRing, a::FqMatrix)
  !is_square(a) && error("Matrix must be square")
  base_ring(R) != base_ring(a) && error("Must have common base ring")
  p = R()
  @ccall libflint.fq_default_mat_charpoly_danilevsky(p::Ref{FqPolyRingElem}, a::Ref{FqMatrix}, base_ring(a)::Ref{FqField})::Nothing
  return p
end


################################################################################
#
#  Minimal polynomial
#
################################################################################

function minpoly(R::FqPolyRing, a::FqMatrix)
  !is_square(a) && error("Matrix must be square")
  base_ring(R) != base_ring(a) && error("Must have common base ring")
  m = deepcopy(a)
  p = R()
  @ccall libflint.fq_default_mat_minpoly(p::Ref{FqPolyRingElem}, m::Ref{FqMatrix}, base_ring(a)::Ref{FqField})::Nothing
  return p
end

###############################################################################
#
#   Promotion rules
#
###############################################################################

promote_rule(::Type{FqMatrix}, ::Type{V}) where {V <: Integer} = FqMatrix

promote_rule(::Type{FqMatrix}, ::Type{FqFieldElem}) = FqMatrix

promote_rule(::Type{FqMatrix}, ::Type{ZZRingElem}) = FqMatrix

################################################################################
#
#  Parent object overloading
#
################################################################################

function (a::FqMatrixSpace)()
  z = FqMatrix(nrows(a), ncols(a), base_ring(a))
  return z
end

function (a::FqMatrixSpace)(b::FqFieldElem)
  parent(b) != base_ring(a) && error("Unable to coerce to matrix")
  return FqMatrix(nrows(a), ncols(a), b)
end

function (a::FqMatrixSpace)(arr::AbstractMatrix{<:IntegerUnion})
  _check_dim(nrows(a), ncols(a), arr)
  return FqMatrix(arr, base_ring(a))
end

function (a::FqMatrixSpace)(arr::AbstractVector{<:IntegerUnion})
  _check_dim(nrows(a), ncols(a), arr)
  return FqMatrix(nrows(a), ncols(a), arr, base_ring(a))
end

function (a::FqMatrixSpace)(arr::AbstractMatrix{FqFieldElem})
  _check_dim(nrows(a), ncols(a), arr)
  (length(arr) > 0 && (base_ring(a) != parent(arr[1]))) && error("Elements must have same base ring")
  return FqMatrix(arr, base_ring(a))
end

function (a::FqMatrixSpace)(arr::AbstractVector{FqFieldElem})
  _check_dim(nrows(a), ncols(a), arr)
  (length(arr) > 0 && (base_ring(a) != parent(arr[1]))) && error("Elements must have same base ring")
  return FqMatrix(nrows(a), ncols(a), arr, base_ring(a))
end

function (a::FqMatrixSpace)(b::ZZMatrix)
  (ncols(a) != ncols(b) || nrows(a) != nrows(b)) && error("Dimensions do not fit")
  return FqMatrix(b, base_ring(a))
end

function (a::FqMatrixSpace)(b::Union{zzModMatrix, fpMatrix})
  characteristic(base_ring(b)) != characteristic(base_ring(a)) &&
  error("Incompatible characteristic")
  (ncols(a) != ncols(b) || nrows(a) != nrows(b)) && error("Dimensions do not fit")
  return FqMatrix(b, base_ring(a))
end

function (a::FqMatrixSpace)(b::Zmod_fmpz_mat)
  characteristic(base_ring(b)) != characteristic(base_ring(a)) &&
  error("Incompatible characteristic")
  (ncols(a) != ncols(b) || nrows(a) != nrows(b)) && error("Dimensions do not fit")
  return FqMatrix(b, base_ring(a))
end

###############################################################################
#
#   Matrix constructor
#
###############################################################################

function matrix(R::FqField, arr::AbstractMatrix{<: Union{FqFieldElem, ZZRingElem, Integer}})
  Base.require_one_based_indexing(arr)
  z = FqMatrix(arr, R)
  return z
end

function matrix(R::FqField, r::Int, c::Int, arr::AbstractVector{<: Union{FqFieldElem, ZZRingElem, Integer}})
  _check_dim(r, c, arr)
  z = FqMatrix(r, c, arr, R)
  return z
end

################################################################################
#
#  Entry pointers
#
################################################################################

function fq_default_mat_entry_ptr(a::FqMatrix, i, j)
  t = _fq_default_ctx_type(base_ring(a))
  ptr = pointer_from_objref(a)
  if t == _FQ_DEFAULT_FQ_ZECH
    pptr = @ccall libflint.fq_zech_mat_entry(ptr::Ptr{Cvoid}, (i - 1)::Int, (j - 1)::Int)::Ptr{FqFieldElem}
  elseif t == _FQ_DEFAULT_FQ_NMOD
    pptr = @ccall libflint.fq_nmod_mat_entry(ptr::Ptr{Cvoid}, (i - 1)::Int, (j - 1)::Int)::Ptr{FqFieldElem}
  elseif t == _FQ_DEFAULT_FQ
    pptr = @ccall libflint.fq_mat_entry(ptr::Ptr{Cvoid}, (i - 1)::Int, (j - 1)::Int)::Ptr{FqFieldElem}
  elseif t == _FQ_DEFAULT_NMOD
    pptr = @ccall libflint.nmod_mat_entry_ptr(ptr::Ptr{Cvoid}, (i - 1)::Int, (j - 1)::Int)::Ptr{FqFieldElem}
  else#if t == _FQ_DEFAULT_FMPZ_NMOD
    pptr = @ccall libflint.fmpz_mod_mat_entry(ptr::Ptr{Cvoid}, (i - 1)::Int, (j - 1)::Int)::Ptr{FqFieldElem}
  end
  return pptr
end

################################################################################
#
#  Kernel
#
################################################################################

function nullspace(M::FqMatrix)
  N = similar(M, ncols(M), ncols(M))
  nullity = @ccall libflint.fq_default_mat_nullspace(N::Ref{FqMatrix}, M::Ref{FqMatrix}, base_ring(M)::Ref{FqField})::Int
  return nullity, view(N, 1:nrows(N), 1:nullity)
end
