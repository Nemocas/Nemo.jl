###############################################################################
#
#   fmpz_poly_mat.jl : FLINT matrices of polynomials over ZZ
#
###############################################################################

###############################################################################
#
#   Data type and parent object methods
#
###############################################################################

base_ring(a::ZZPolyRingMatrix) = a.base_ring

dense_matrix_type(::Type{ZZPolyRingElem}) = ZZPolyRingMatrix

is_zero_initialized(::Type{ZZPolyRingMatrix}) = true

###############################################################################
#
#   Basic manipulation
#
###############################################################################

@inline number_of_rows(a::ZZPolyRingMatrix) = a.r

@inline number_of_columns(a::ZZPolyRingMatrix) = a.c

function getindex!(v::ZZPolyRingElem, a::ZZPolyRingMatrix, r::Int, c::Int)
  @boundscheck _checkbounds(a, r, c)
  GC.@preserve a begin
    z = mat_entry_ptr(a, r, c)
    set!(v, z)
  end
  return v
end

@inline function getindex(a::ZZPolyRingMatrix, r::Int, c::Int)
  @boundscheck _checkbounds(a, r, c)
  v = base_ring(a)()
  GC.@preserve a begin
    z = mat_entry_ptr(a, r, c)
    set!(v, z)
  end
  return v
end

@inline function setindex!(a::ZZPolyRingMatrix, d::ZZPolyRingElem, r::Int, c::Int)
  @boundscheck _checkbounds(a, r, c)
  GC.@preserve a begin
    z = mat_entry_ptr(a, r, c)
    set!(z, d)
  end
end

function setindex!(a::ZZPolyRingMatrix, b::ZZPolyRingMatrix, r::UnitRange{Int64}, c::UnitRange{Int64})
  _checkbounds(a, r, c)
  size(b) == (length(r), length(c)) || throw(DimensionMismatch("tried to assign a $(size(b, 1))x$(size(b, 2)) matrix to a $(length(r))x$(length(c)) destination"))
  A = view(a, r, c)
  @ccall libflint.fmpz_poly_mat_set(A::Ref{ZZPolyRingMatrix}, b::Ref{ZZPolyRingMatrix})::Nothing
end

function deepcopy_internal(d::ZZPolyRingMatrix, dict::IdDict)
  z = ZZPolyRingMatrix(d)
  return z
end

function similar(x::ZZPolyRingMatrix, R::ZZPolyRing, r::Int, c::Int)
  z = ZZPolyRingMatrix(r, c, R)
  return z
end

###############################################################################
#
#   Unary operations
#
###############################################################################

function -(x::ZZPolyRingMatrix)
  z = similar(x)
  neg!(z, x)
  return z
end

###############################################################################
#
#   Binary operations
#
###############################################################################

function +(x::ZZPolyRingMatrix, y::ZZPolyRingMatrix)
  check_parent(x, y)
  z = similar(x)
  add!(z, x, y)
  return z
end

function -(x::ZZPolyRingMatrix, y::ZZPolyRingMatrix)
  check_parent(x, y)
  z = similar(x)
  sub!(z, x, y)
  return z
end

function *(x::ZZPolyRingMatrix, y::ZZPolyRingMatrix)
  @req ncols(x) == nrows(y) "Incompatible matrix dimensions"
  if nrows(x) == ncols(y) && nrows(x) == ncols(x)
    z = similar(x)
  else
    z = similar(x, nrows(x), ncols(y))
  end
  mul!(z, x, y)
  return z
end

###############################################################################
#
#   In-place operations
#
###############################################################################

function zero!(z::ZZPolyRingMatrixOrPtr)
  @ccall libflint.fmpz_poly_mat_zero(z::Ref{ZZPolyRingMatrix})::Nothing
  return z
end

function one!(z::ZZPolyRingMatrixOrPtr)
  @ccall libflint.fmpz_poly_mat_one(z::Ref{ZZPolyRingMatrix})::Nothing
  return z
end

function neg!(z::ZZPolyRingMatrixOrPtr, w::ZZPolyRingMatrixOrPtr)
  @ccall libflint.fmpz_poly_mat_neg(z::Ref{ZZPolyRingMatrix}, w::Ref{ZZPolyRingMatrix})::Nothing
  return z
end

function add!(z::ZZPolyRingMatrixOrPtr, x::ZZPolyRingMatrixOrPtr, y::ZZPolyRingMatrixOrPtr)
  @ccall libflint.fmpz_poly_mat_add(z::Ref{ZZPolyRingMatrix}, x::Ref{ZZPolyRingMatrix}, y::Ref{ZZPolyRingMatrix})::Nothing
  return z
end

function sub!(z::ZZPolyRingMatrixOrPtr, x::ZZPolyRingMatrixOrPtr, y::ZZPolyRingMatrixOrPtr)
  @ccall libflint.fmpz_poly_mat_sub(z::Ref{ZZPolyRingMatrix}, x::Ref{ZZPolyRingMatrix}, y::Ref{ZZPolyRingMatrix})::Nothing
  return z
end

function mul!(z::ZZPolyRingMatrixOrPtr, x::ZZPolyRingMatrixOrPtr, y::ZZPolyRingMatrixOrPtr)
  @ccall libflint.fmpz_poly_mat_mul(z::Ref{ZZPolyRingMatrix}, x::Ref{ZZPolyRingMatrix}, y::Ref{ZZPolyRingMatrix})::Nothing
  return z
end

###############################################################################
#
#   Comparison
#
###############################################################################

function ==(x::ZZPolyRingMatrix, y::ZZPolyRingMatrix)
  check_parent(x, y)
  return Bool(@ccall libflint.fmpz_poly_mat_equal(x::Ref{ZZPolyRingMatrix}, y::Ref{ZZPolyRingMatrix})::Cint)
end

function iszero(a::ZZPolyRingMatrix)
  return Bool(@ccall libflint.fmpz_poly_mat_is_zero(a::Ref{ZZPolyRingMatrix})::Cint)
end

function isone(a::ZZPolyRingMatrix)
  return Bool(@ccall libflint.fmpz_poly_mat_is_one(a::Ref{ZZPolyRingMatrix})::Cint)
end

###############################################################################
#
#   Transpose
#
###############################################################################

function transpose(a::ZZPolyRingMatrix)
  z = similar(a, ncols(a), nrows(a))
  @ccall libflint.fmpz_poly_mat_transpose(z::Ref{ZZPolyRingMatrix}, a::Ref{ZZPolyRingMatrix})::Nothing
  return z
end

###############################################################################
#
#   Determinant
#
###############################################################################

function det(a::ZZPolyRingMatrix)
  @req is_square(a) "Matrix must be a square matrix"
  z = base_ring(a)()
  @ccall libflint.fmpz_poly_mat_det(z::Ref{ZZPolyRingElem}, a::Ref{ZZPolyRingMatrix})::Nothing
  return z
end

###############################################################################
#
#   Rank
#
###############################################################################

function rank(a::ZZPolyRingMatrix)
  return Int(@ccall libflint.fmpz_poly_mat_rank(a::Ref{ZZPolyRingMatrix})::Int)
end

###############################################################################
#
#   Trace
#
###############################################################################

function tr(a::ZZPolyRingMatrix)
  @req is_square(a) "Matrix must be a square matrix"
  z = base_ring(a)()
  @ccall libflint.fmpz_poly_mat_trace(z::Ref{ZZPolyRingElem}, a::Ref{ZZPolyRingMatrix})::Nothing
  return z
end

###############################################################################
#
#   Entry pointers
#
###############################################################################

# Size of the C fmpz_poly_struct (coeffs pointer + alloc + length), without
# the Julia-only parent field.
const SIZEOF_FMPZ_POLY_STRUCT = 3 * sizeof(Int)

function mat_entry_ptr(A::ZZPolyRingMatrix, i::Int, j::Int)
  return Ptr{ZZPolyRingElem}(UInt(A.entries) + UInt(((i - 1) * A.stride + (j - 1)) * SIZEOF_FMPZ_POLY_STRUCT))
end

###############################################################################
#
#   Constructors
#
###############################################################################

function ZZPolyRingMatrix(m::Matrix{ZZPolyRingElem})
  r, c = size(m)
  @req !isempty(m) "Matrix must be non-empty"
  base_ring = parent(m[1, 1])
  z = ZZPolyRingMatrix(r, c, base_ring)
  GC.@preserve z for i in 1:r
    for j in 1:c
      el = mat_entry_ptr(z, i, j)
      set!(el, m[i, j])
    end
  end
  return z
end

function ZZPolyRingMatrix(m::MatElem{ZZPolyRingElem})
  r = nrows(m)
  c = ncols(m)
  base_ring = Nemo.base_ring(m)
  z = ZZPolyRingMatrix(r, c, base_ring)
  GC.@preserve z for i in 1:r
    for j in 1:c
      el = mat_entry_ptr(z, i, j)
      set!(el, m[i, j])
    end
  end
  return z
end
