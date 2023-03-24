################################################################################
#
#  gfp_fmpz_mat.jl: flint fmpz_mod_mat (matrices over Z/nZ, large prime n)
#
################################################################################

export FpMatrix, FpMatrixSpace

################################################################################
#
#  Data type and parent object methods
#
################################################################################

parent_type(::Type{FpMatrix}) = FpMatrixSpace

elem_type(::Type{FpMatrixSpace}) = FpMatrix

dense_matrix_type(::Type{FpFieldElem}) = FpMatrix

###############################################################################
#
#   Similar
#
###############################################################################

function similar(::MatElem, R::FpField, r::Int, c::Int)
   z = FpMatrix(r, c, R.n)
   z.base_ring = R
   return z
end

################################################################################
#
#  Manipulation
#
################################################################################

# return plain ZZRingElem, no bounds checking
@inline function getindex_raw(a::FpMatrix, i::Int, j::Int)
  u = ZZRingElem()
  ccall((:fmpz_mod_mat_get_entry, libflint), Nothing,
                 (Ref{ZZRingElem}, Ref{FpMatrix}, Int, Int), u, a, i - 1, j - 1)
  return u
end

@inline function getindex(a::FpMatrix, i::Int, j::Int)
  @boundscheck Generic._checkbounds(a, i, j)
  return FpFieldElem(getindex_raw(a, i, j), base_ring(a)) # no reduction needed
end

@inline function setindex!(a::FpMatrix, u::ZZRingElem, i::Int, j::Int)
  @boundscheck Generic._checkbounds(a, i, j)
  R = base_ring(a)
  setindex_raw!(a, mod(u, R.n), i, j)
end

@inline function setindex!(a::FpMatrix, u::FpFieldElem, i::Int, j::Int)
  @boundscheck Generic._checkbounds(a, i, j)
  (base_ring(a) != parent(u)) && error("Parent objects must coincide")
  setindex_raw!(a, u.data, i, j) # no reduction needed
end

function setindex!(a::FpMatrix, u::Integer, i::Int, j::Int)
   setindex!(a, ZZRingElem(u), i, j)
end

# as per setindex! but no reduction mod n and no bounds checking
@inline function setindex_raw!(a::FpMatrix, u::ZZRingElem, i::Int, j::Int)
  ccall((:fmpz_mod_mat_set_entry, libflint), Nothing,
        (Ref{FpMatrix}, Int, Int, Ref{ZZRingElem}), a, i - 1, j - 1, u)
end

function deepcopy_internal(a::FpMatrix, dict::IdDict)
  z = FpMatrix(nrows(a), ncols(a), a.base_ring.n)
  if isdefined(a, :base_ring)
    z.base_ring = a.base_ring
  end
  ccall((:fmpz_mod_mat_set, libflint), Nothing,
        (Ref{FpMatrix}, Ref{FpMatrix}), z, a)
  return z
end

nrows(a::FpMatrix) = a.r

ncols(a::FpMatrix) = a.c

nrows(a::FpMatrixSpace) = a.nrows

ncols(a::FpMatrixSpace) = a.ncols

function parent(a::FpMatrix, cached::Bool = true)
   matrix_space(base_ring(a), nrows(a), ncols(a); cached)
end

base_ring(a::FpMatrixSpace) = a.base_ring

base_ring(a::FpMatrix) = a.base_ring

zero(a::FpMatrixSpace) = a()

function one(a::FpMatrixSpace)
  (nrows(a) != ncols(a)) && error("Matrices must be square")
  z = a()
  ccall((:fmpz_mod_mat_one, libflint), Nothing, (Ref{FpMatrix}, ), z)
  return z
end

function iszero(a::FpMatrix)
  r = ccall((:fmpz_mod_mat_is_zero, libflint), Cint, (Ref{FpMatrix}, ), a)
  return Bool(r)
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
#  Trace
#
################################################################################

function tr(a::FpMatrix)
  !is_square(a) && error("Matrix must be a square matrix")
  R = base_ring(a)
  r = ZZRingElem()
  ccall((:fmpz_mod_mat_trace, libflint), Nothing,
        (Ref{ZZRingElem}, Ref{FpMatrix}), r, a)
  return FpFieldElem(r, R)
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
  ccall((:fmpz_mod_mat_window_init, libflint), Nothing,
        (Ref{FpMatrix}, Ref{FpMatrix}, Int, Int, Int, Int),
        z, x, r1 - 1, c1 - 1, r2, c2)
  finalizer(_gfp_fmpz_mat_window_clear_fn, z)
  return z
end

function _gfp_fmpz_mat_window_clear_fn(a::FpMatrix)
  ccall((:fmpz_mod_mat_window_clear, libflint), Nothing, (Ref{FpMatrix}, ), a)
end


################################################################################
#
#  Conversion
#
################################################################################

function Array(b::FpMatrix)
  a = Array{FpFieldElem}(undef, b.r, b.c)
  for i = 1:b.r
    for j = 1:b.c
      a[i, j] = b[i, j]
    end
  end
  return a
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
#  Parent object overloading
#
################################################################################

function (a::FpMatrixSpace)()
  z = FpMatrix(nrows(a), ncols(a), a.n)
  z.base_ring = a.base_ring
  return z
end

function (a::FpMatrixSpace)(b::IntegerUnion)
   M = a()  # zero
   for i in 1:min(nrows(a), ncols(a))
      M[i, i] = base_ring(a)(b)
   end
   return M
end

function (a::FpMatrixSpace)(b::FpFieldElem)
   parent(b) != base_ring(a) && error("Unable to coerce to matrix")
   M = a()  # zero
   for i in 1:min(nrows(a), ncols(a))
      M[i, i] = b
   end
   return M
end

function (a::FpMatrixSpace)(arr::AbstractMatrix{BigInt}, transpose::Bool = false)
  _check_dim(nrows(a), ncols(a), arr, transpose)
  z = FpMatrix(nrows(a), ncols(a), a.n, arr, transpose)
  z.base_ring = a.base_ring
  return z
end

function (a::FpMatrixSpace)(arr::AbstractVector{BigInt})
  _check_dim(nrows(a), ncols(a), arr)
  z = FpMatrix(nrows(a), ncols(a), a.n, arr)
  z.base_ring = a.base_ring
  return z
end

function (a::FpMatrixSpace)(arr::AbstractMatrix{ZZRingElem}, transpose::Bool = false)
  _check_dim(nrows(a), ncols(a), arr, transpose)
  z = FpMatrix(nrows(a), ncols(a), a.n, arr, transpose)
  z.base_ring = a.base_ring
  return z
end

function (a::FpMatrixSpace)(arr::AbstractVector{ZZRingElem})
  _check_dim(nrows(a), ncols(a), arr)
  z = FpMatrix(nrows(a), ncols(a), a.n, arr)
  z.base_ring = a.base_ring
  return z
end

function (a::FpMatrixSpace)(arr::AbstractMatrix{Int}, transpose::Bool = false)
  _check_dim(nrows(a), ncols(a), arr, transpose)
  z = FpMatrix(nrows(a), ncols(a), a.n, arr, transpose)
  z.base_ring = a.base_ring
  return z
end

function (a::FpMatrixSpace)(arr::AbstractVector{Int})
  _check_dim(nrows(a), ncols(a), arr)
  z = FpMatrix(nrows(a), ncols(a), a.n, arr)
  z.base_ring = a.base_ring
  return z
end

function (a::FpMatrixSpace)(arr::AbstractMatrix{FpFieldElem}, transpose::Bool = false)
  _check_dim(nrows(a), ncols(a), arr, transpose)
  (length(arr) > 0 && (base_ring(a) != parent(arr[1]))) && error("Elements must have same base ring")
  z = FpMatrix(nrows(a), ncols(a), a.n, arr, transpose)
  z.base_ring = a.base_ring
  return z
end

function (a::FpMatrixSpace)(arr::AbstractVector{FpFieldElem})
  _check_dim(nrows(a), ncols(a), arr)
  (length(arr) > 0 && (base_ring(a) != parent(arr[1]))) && error("Elements must have same base ring")
  z = FpMatrix(nrows(a), ncols(a), a.n, arr)
  z.base_ring = a.base_ring
  return z
end

###############################################################################
#
#   Matrix constructor
#
###############################################################################

function matrix(R::FpField, arr::AbstractMatrix{<: Union{FpFieldElem, ZZRingElem, Integer}})
   z = FpMatrix(size(arr, 1), size(arr, 2), R.n, arr)
   z.base_ring = R
   return z
end

function matrix(R::FpField, r::Int, c::Int, arr::AbstractVector{<: Union{FpFieldElem, ZZRingElem, Integer}})
   _check_dim(r, c, arr)
   z = FpMatrix(r, c, R.n, arr)
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
   z = FpMatrix(r, c, R.n)
   z.base_ring = R
   return z
end

###############################################################################
#
#  Identity matrix
#
###############################################################################

function identity_matrix(R::FpField, n::Int)
   z = zero_matrix(R, n, n)
   for i in 1:n
      z[i, i] = one(R)
   end
   z.base_ring = R
   return z
end

################################################################################
#
#  Matrix space constructor
#
################################################################################

function matrix_space(R::FpField, r::Int, c::Int; cached::Bool = true)
  FpMatrixSpace(R, r, c, cached)
end

