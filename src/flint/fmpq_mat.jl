###############################################################################
#
#   QQMatrix.jl : Flint matrices over the rationals
#
###############################################################################

###############################################################################
#
#   Data type and parent object methods
#
###############################################################################

base_ring(a::QQMatrix) = QQ

dense_matrix_type(::Type{QQFieldElem}) = QQMatrix

###############################################################################
#
#   Similar & zero
#
###############################################################################

function similar(::QQMatrix, R::QQField, r::Int, c::Int)
  z = QQMatrix(r, c)
  return z
end

zero(m::QQMatrix, R::QQField, r::Int, c::Int) = similar(m, R, r, c)

###############################################################################
#
#   Windows - handle with care!!!
#
###############################################################################

function Base.view(x::QQMatrix, r1::Int, c1::Int, r2::Int, c2::Int)
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

  b = QQMatrix()
  b.view_parent = x
  @ccall libflint.fmpq_mat_window_init(b::Ref{QQMatrix}, x::Ref{QQMatrix}, (r1 - 1)::Int, (c1 - 1)::Int, r2::Int, c2::Int)::Nothing
  finalizer(_fmpq_mat_window_clear_fn, b)
  return b
end

function Base.view(x::QQMatrix, r::AbstractUnitRange{Int}, c::AbstractUnitRange{Int})
  return Base.view(x, first(r), first(c), last(r), last(c))
end

function _fmpq_mat_window_clear_fn(a::QQMatrix)
  @ccall libflint.fmpq_mat_window_clear(a::Ref{QQMatrix})::Nothing
end

function sub(x::QQMatrix, r1::Int, c1::Int, r2::Int, c2::Int)
  return deepcopy(view(x, r1, c1, r2, c2))
end

function sub(x::QQMatrix, r::AbstractUnitRange{Int}, c::AbstractUnitRange{Int})
  return deepcopy(view(x, r, c))
end

getindex(x::QQMatrix, r::AbstractUnitRange{Int}, c::AbstractUnitRange{Int}) = sub(x, r, c)

###############################################################################
#
#   Basic manipulation
#
###############################################################################

function getindex!(v::QQFieldElem, a::QQMatrix, r::Int, c::Int)
  GC.@preserve a begin
    z = mat_entry_ptr(a, r, c)
    set!(v, z)
  end
  return v
end

@inline function getindex(a::QQMatrix, r::Int, c::Int)
  @boundscheck _checkbounds(a, r, c)
  v = QQFieldElem()
  GC.@preserve a begin
    z = mat_entry_ptr(a, r, c)
    set!(v, z)
  end
  return v
end

@inline function setindex!(a::QQMatrix, d::RationalUnion, r::Int, c::Int)
  @boundscheck _checkbounds(a, r, c)
  GC.@preserve a begin
    z = mat_entry_ptr(a, r, c)
    set!(z, flintify(d))
  end
end

function setindex!(a::QQMatrix, b::QQMatrix, r::UnitRange{Int64}, c::UnitRange{Int64})
  _checkbounds(a, r, c)
  size(b) == (length(r), length(c)) || throw(DimensionMismatch("tried to assign a $(size(b, 1))x$(size(b, 2)) matrix to a $(length(r))x$(length(c)) destination"))
  A = view(a, r, c)
  @ccall libflint.fmpq_mat_set(A::Ref{QQMatrix}, b::Ref{QQMatrix})::Nothing
end

number_of_rows(a::QQMatrix) = a.r

number_of_columns(a::QQMatrix) = a.c

iszero(a::QQMatrix) = @ccall libflint.fmpq_mat_is_zero(a::Ref{QQMatrix})::Bool

@inline function is_zero_entry(A::QQMatrix, i::Int, j::Int)
  @boundscheck _checkbounds(A, i, j)
  GC.@preserve A begin
    x = mat_entry_ptr(A, i, j)
    return is_zero(x)
  end
end

isone(a::QQMatrix) = @ccall libflint.fmpq_mat_is_one(a::Ref{QQMatrix})::Bool

function deepcopy_internal(d::QQMatrix, dict::IdDict)
  z = QQMatrix(d)
  return z
end

# This function is filthy because it
#     relies on the internals of QQMatrix, and
#     converts a C array of fmpqs to an array of Ints of twice the length, and
#     assumes the fmpqs in the mat are each canonical.
# This function needs to be changed if the internals ever change or if any
# architectures with strange struct packings are supported.
function Base.hash(a::QQMatrix, h::UInt)
  GC.@preserve a begin
    r = nrows(a)
    c = ncols(a)
    h = hash(r, h)
    h = hash(c, h)
    rowptr = convert(Ptr{Ptr{Int}}, a.rows)
    for i in 1:r
      h = _hash_integer_array(unsafe_load(rowptr, i), 2*c, h)
    end
    return xor(h, 0xb591d5c795885682%UInt)
  end
end

###############################################################################
#
#   Canonicalisation
#
###############################################################################

canonical_unit(a::QQMatrix) = canonical_unit(a[1, 1])

###############################################################################
#
#   AbstractString I/O
#
###############################################################################

###############################################################################
#
#   Unary operations
#
###############################################################################

-(x::QQMatrix) = neg!(similar(x), x)

###############################################################################
#
#   Common denominator
#
###############################################################################

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

###############################################################################
#
#   transpose
#
###############################################################################

function transpose(x::QQMatrix)
  z = similar(x, ncols(x), nrows(x))
  transpose!(z, x)
  return z
end

transpose!(A::Union{ZZMatrix,QQMatrix}) = is_square(A) ? transpose!(A, A) : transpose(A)

function transpose!(A::QQMatrixOrPtr, B::QQMatrixOrPtr)
  @ccall libflint.fmpq_mat_transpose(A::Ref{QQMatrix}, B::Ref{QQMatrix})::Nothing
  return A
end

###############################################################################
#
#   Row and column swapping
#
###############################################################################

function swap_rows!(x::QQMatrixOrPtr, i::Int, j::Int)
  @ccall libflint.fmpq_mat_swap_rows(x::Ref{QQMatrix}, C_NULL::Ptr{Nothing}, (i - 1)::Int, (j - 1)::Int)::Nothing
  return x
end

function swap_cols!(x::QQMatrixOrPtr, i::Int, j::Int)
  @ccall libflint.fmpq_mat_swap_cols(x::Ref{QQMatrix}, C_NULL::Ptr{Nothing}, (i - 1)::Int, (j - 1)::Int)::Nothing
  return x
end

function reverse_rows!(x::QQMatrixOrPtr)
  @ccall libflint.fmpq_mat_invert_rows(x::Ref{QQMatrix}, C_NULL::Ptr{Nothing})::Nothing
  return x
end

function reverse_cols!(x::QQMatrixOrPtr)
  @ccall libflint.fmpq_mat_invert_cols(x::Ref{QQMatrix}, C_NULL::Ptr{Nothing})::Nothing
  return x
end

###############################################################################
#
#   Binary operations
#
###############################################################################

function +(x::QQMatrix, y::QQMatrix)
  check_parent(x, y)
  z = similar(x)
  return add!(z, x, y)
end

function -(x::QQMatrix, y::QQMatrix)
  check_parent(x, y)
  z = similar(x)
  return sub!(z, x, y)
end

function *(x::QQMatrix, y::QQMatrix)
  ncols(x) != nrows(y) && error("Incompatible matrix dimensions")
  z = similar(x, nrows(x), ncols(y))
  return mul!(z, x, y)
end

###############################################################################
#
#   Ad hoc binary operators
#
###############################################################################

for T in [Integer, Rational, ZZRingElem, QQFieldElem]
  @eval begin
    *(mat::QQMatrix, scalar::$T) = mul!(similar(mat), mat, scalar)
    *(scalar::$T, mat::QQMatrix) = mul!(similar(mat), mat, scalar)

    function +(mat::QQMatrix, scalar::$T)
      z = deepcopy(mat)
      for i = 1:min(nrows(mat), ncols(mat))
        add!(mat_entry_ptr(z, i, i), scalar)
      end
      return z
    end

    +(scalar::$T, mat::QQMatrix) = mat + scalar

    function -(mat::QQMatrix, scalar::$T)
      z = deepcopy(mat)
      for i = 1:min(nrows(mat), ncols(mat))
        sub!(mat_entry_ptr(z, i, i), scalar)
      end
      return z
    end

    function -(scalar::$T, mat::QQMatrix)
      z = -mat
      for i = 1:min(nrows(mat), ncols(mat))
        add!(mat_entry_ptr(z, i, i), scalar)
      end
      return z
    end
  end
end

###############################################################################
#
#   Comparisons
#
###############################################################################

function ==(x::QQMatrix, y::QQMatrix)
  fl = check_parent(x, y, false)
  fl && @ccall libflint.fmpq_mat_equal(x::Ref{QQMatrix}, y::Ref{QQMatrix})::Bool
end

isequal(x::QQMatrix, y::QQMatrix) = ==(x, y)

###############################################################################
#
#   Ad hoc comparisons
#
###############################################################################

function ==(x::QQMatrix, y::Integer)
  for i = 1:min(nrows(x), ncols(x))
    if x[i, i] != y
      return false
    end
  end
  for i = 1:nrows(x)
    for j = 1:ncols(x)
      if i != j && x[i, j] != 0
        return false
      end
    end
  end
  return true
end

==(x::Integer, y::QQMatrix) = y == x

==(x::QQMatrix, y::Rational) = x == QQFieldElem(y)

==(x::Rational, y::QQMatrix) = y == x

###############################################################################
#
#   Inversion
#
###############################################################################

function inv(x::QQMatrix)
  !is_square(x) && error("Matrix not invertible")
  z = similar(x)
  success = @ccall libflint.fmpq_mat_inv(z::Ref{QQMatrix}, x::Ref{QQMatrix})::Cint
  success == 0 && error("Matrix not invertible")
  return z
end

###############################################################################
#
#   Exact division
#
###############################################################################

function divexact(x::QQMatrix, y::QQMatrix; check::Bool=true)
  ncols(x) != ncols(y) && error("Incompatible matrix dimensions")
  x*inv(y)
end

###############################################################################
#
#   Ad hoc exact division
#
###############################################################################

function divexact(x::QQMatrix, y::QQFieldElem; check::Bool=true)
  z = similar(x)
  divexact!(z, x, y)
  return z
end

function divexact(x::QQMatrix, y::ZZRingElem; check::Bool=true)
  z = similar(x)
  divexact!(z, x, y)
  return z
end

divexact(x::QQMatrix, y::Integer; check::Bool=true) = divexact(x, ZZRingElem(y); check=check)

divexact(x::QQMatrix, y::Rational; check::Bool=true) = divexact(x, QQFieldElem(y); check=check)

###############################################################################
#
#   Kronecker product
#
###############################################################################

function kronecker_product(x::QQMatrix, y::QQMatrix)
  z = similar(x, nrows(x)*nrows(y), ncols(x)*ncols(y))
  @ccall libflint.fmpq_mat_kronecker_product(z::Ref{QQMatrix}, x::Ref{QQMatrix}, y::Ref{QQMatrix})::Nothing
  return z
end

###############################################################################
#
#   Characteristic polynomial
#
###############################################################################

function charpoly(R::QQPolyRing, x::QQMatrix)
  nrows(x) != ncols(x) && error("Non-square")
  z = R()
  @ccall libflint.fmpq_mat_charpoly(z::Ref{QQPolyRingElem}, x::Ref{QQMatrix})::Nothing
  return z
end

###############################################################################
#
#   Minimal polynomial
#
###############################################################################

function minpoly(R::QQPolyRing, x::QQMatrix)
  nrows(x) != ncols(x) && error("Non-square")
  z = R()
  @ccall libflint.fmpq_mat_minpoly(z::Ref{QQPolyRingElem}, x::Ref{QQMatrix})::Nothing
  return z
end

###############################################################################
#
#   Determinant
#
###############################################################################

function det(x::QQMatrix)
  nrows(x) != ncols(x) && error("Non-square matrix")
  z = QQFieldElem()
  @ccall libflint.fmpq_mat_det(z::Ref{QQFieldElem}, x::Ref{QQMatrix})::Nothing
  return z
end

###############################################################################
#
#   Gram-Schmidt orthogonalisation
#
###############################################################################

@doc raw"""
    gram_schmidt_orthogonalisation(x::QQMatrix)

Takes the columns of $x$ as the generators of a subset of $\mathbb{Q}^m$ and
returns a matrix whose columns are an orthogonal generating set for the same
subspace.

# Examples
```jldctest
julia> S = matrix_space(QQ, 3, 3);

julia> A = S([4 7 3; 2 9 1; 0 5 3])
[4   7   3]
[2   9   1]
[0   5   3]

julia> B = gram_schmidt_orthogonalisation(A)
[4   -11//5     95//123]
[2    22//5   -190//123]
[0        5    209//123]
```
"""
function gram_schmidt_orthogonalisation(x::QQMatrix)
  z = similar(x)
  @ccall libflint.fmpq_mat_gso(z::Ref{QQMatrix}, x::Ref{QQMatrix})::Nothing
  return z
end

###############################################################################
#
#   Hilbert matrix
#
###############################################################################

@doc raw"""
    hilbert(R::QQMatrixSpace)

Return the Hilbert matrix in the given matrix space. This is the matrix with
entries $H_{i,j} = 1/(i + j - 1)$.
"""
function hilbert(R::QQMatrixSpace)
  z = R()
  @ccall libflint.fmpq_mat_hilbert_matrix(z::Ref{QQMatrix})::Bool
  return z
end

###############################################################################
#
#   Rank
#
###############################################################################

function rank(x::QQMatrix)
  z = similar(x)
  r = @ccall libflint.fmpq_mat_rref(z::Ref{QQMatrix}, x::Ref{QQMatrix})::Int
  return r
end

###############################################################################
#
#   Reduced row echelon form
#
###############################################################################

function rref(x::QQMatrix)
  z = similar(x)
  r = @ccall libflint.fmpq_mat_rref(z::Ref{QQMatrix}, x::Ref{QQMatrix})::Int
  return r, z
end

function rref!(x::QQMatrix)
  r = @ccall libflint.fmpq_mat_rref(x::Ref{QQMatrix}, x::Ref{QQMatrix})::Int
  return r
end

###############################################################################
#
#   Linear solving
#
###############################################################################

@doc raw"""
    _solve_dixon(a::QQMatrix, b::QQMatrix)

Solve $ax = b$ by clearing denominators and using Dixon's algorithm. This is
usually faster for large systems.
"""
function _solve_dixon(a::QQMatrix, b::QQMatrix)
  nrows(a) != ncols(a) && error("Not a square matrix in solve")
  nrows(b) != nrows(a) && error("Incompatible dimensions in solve")
  z = similar(b)
  nonsing = @ccall libflint.fmpq_mat_solve_dixon(z::Ref{QQMatrix}, a::Ref{QQMatrix}, b::Ref{QQMatrix})::Bool
  !nonsing && error("Singular matrix in solve")
  return z
end

# Actually we use a modular algorithm in flint...
Solve.matrix_normal_form_type(::QQField) = Solve.RREFTrait()
Solve.matrix_normal_form_type(::QQMatrix) = Solve.RREFTrait()

# fflu is much slower in some cases, so we do an rref (with transformation)
# here and let flint choose an algorithm, see
# https://github.com/Nemocas/Nemo.jl/issues/1710.
function Solve.solve_context_type(::QQField)
  return Solve.solve_context_type(Solve.RREFTrait(), QQFieldElem)
end

function Solve._can_solve_internal_no_check(::Solve.RREFTrait, A::QQMatrix, b::QQMatrix, task::Symbol; side::Symbol = :left)
  if side === :left
    fl, sol, K = Solve._can_solve_internal_no_check(Solve.RREFTrait(), transpose(A), transpose(b), task, side = :right)
    return fl, transpose(sol), transpose(K)
  end

  x = similar(A, ncols(A), ncols(b))
  fl = @ccall libflint.fmpq_mat_can_solve_multi_mod(x::Ref{QQMatrix}, A::Ref{QQMatrix}, b::Ref{QQMatrix})::Cint

  if task === :only_check || task === :with_solution
    return Bool(fl), x, zero(A, 0, 0)
  end
  return Bool(fl), x, kernel(A, side = :right)
end

###############################################################################
#
#   Trace
#
###############################################################################

function tr(x::QQMatrix)
  nrows(x) != ncols(x) && error("Not a square matrix in trace")
  d = QQFieldElem()
  @ccall libflint.fmpq_mat_trace(d::Ref{QQFieldElem}, x::Ref{QQMatrix})::Nothing
  return d
end

###############################################################################
#
#   Concatenation
#
###############################################################################

function hcat(a::QQMatrix, b::QQMatrix)
  nrows(a) != nrows(b) && error("Incompatible number of rows in hcat")
  c = similar(a, nrows(a), ncols(a) + ncols(b))
  @ccall libflint.fmpq_mat_concat_horizontal(c::Ref{QQMatrix}, a::Ref{QQMatrix}, b::Ref{QQMatrix})::Nothing
  return c
end

function vcat(a::QQMatrix, b::QQMatrix)
  ncols(a) != ncols(b) && error("Incompatible number of columns in vcat")
  c = similar(a, nrows(a) + nrows(b), ncols(a))
  @ccall libflint.fmpq_mat_concat_vertical(c::Ref{QQMatrix}, a::Ref{QQMatrix}, b::Ref{QQMatrix})::Nothing
  return c
end

###############################################################################
#
#   Similarity
#
###############################################################################

function similarity!(z::QQMatrix, r::Int, d::QQFieldElem)
  @ccall libflint.fmpq_mat_similarity(z::Ref{QQMatrix}, (r - 1)::Int, d::Ref{QQFieldElem})::Nothing
end

###############################################################################
#
#   Unsafe functions
#
###############################################################################

function zero!(z::QQMatrixOrPtr)
  @ccall libflint.fmpq_mat_zero(z::Ref{QQMatrix})::Nothing
  return z
end

function one!(z::QQMatrixOrPtr)
  @ccall libflint.fmpq_mat_one(z::Ref{QQMatrix})::Nothing
  return z
end

function neg!(z::QQMatrixOrPtr, a::QQMatrixOrPtr)
  @ccall libflint.fmpq_mat_neg(z::Ref{QQMatrix}, a::Ref{QQMatrix})::Nothing
  return z
end

function add!(z::QQMatrixOrPtr, x::QQMatrixOrPtr, y::QQMatrixOrPtr)
  @ccall libflint.fmpq_mat_add(z::Ref{QQMatrix}, x::Ref{QQMatrix}, y::Ref{QQMatrix})::Nothing
  return z
end

function sub!(z::QQMatrixOrPtr, x::QQMatrixOrPtr, y::QQMatrixOrPtr)
  @ccall libflint.fmpq_mat_sub(z::Ref{QQMatrix}, x::Ref{QQMatrix}, y::Ref{QQMatrix})::Nothing
  return z
end

#
# matrix x matrix
#
function mul!(z::QQMatrixOrPtr, x::QQMatrixOrPtr, y::QQMatrixOrPtr)
  @ccall libflint.fmpq_mat_mul(z::Ref{QQMatrix}, x::Ref{QQMatrix}, y::Ref{QQMatrix})::Nothing
  return z
end

#
# matrix x vector, vector x matrix
#
function mul!(z::Vector{QQFieldElem}, a::Vector{QQFieldElem}, b::QQMatrixOrPtr)
  @ccall libflint.fmpq_mat_fmpq_vec_mul_ptr(z::Ptr{Ref{QQFieldElem}}, a::Ptr{Ref{QQFieldElem}}, length(a)::Int, b::Ref{QQMatrix})::Nothing
  return z
end

function mul!(z::Vector{QQFieldElem}, a::QQMatrixOrPtr, b::Vector{QQFieldElem})
  @ccall libflint.fmpq_mat_mul_fmpq_vec_ptr(z::Ptr{Ref{QQFieldElem}}, a::Ref{QQMatrix}, b::Ptr{Ref{QQFieldElem}}, length(b)::Int)::Nothing
  return z
end

function mul!(z::Vector{QQFieldElem}, a::Vector{ZZRingElem}, b::QQMatrixOrPtr)
  @ccall libflint.fmpq_mat_fmpz_vec_mul_ptr(z::Ptr{Ref{QQFieldElem}}, a::Ptr{Ref{ZZRingElem}}, length(a)::Int, b::Ref{QQMatrix})::Nothing
  return z
end

function mul!(z::Vector{QQFieldElem}, a::QQMatrixOrPtr, b::Vector{ZZRingElem})
  @ccall libflint.fmpq_mat_mul_fmpz_vec_ptr(z::Ptr{Ref{QQFieldElem}}, a::Ref{QQMatrix}, b::Ptr{Ref{ZZRingElem}}, length(b)::Int)::Nothing
  return z
end

#
# matrix x scalar, scalar x matrix
#
function mul!(z::QQMatrixOrPtr, a::QQMatrixOrPtr, b::QQFieldElemOrPtr)
   @ccall libflint.fmpq_mat_scalar_mul_fmpq(z::Ref{QQMatrix}, a::Ref{QQMatrix}, b::Ref{QQFieldElem})::Nothing
   return z
end

function mul!(z::QQMatrixOrPtr, a::QQMatrixOrPtr, b::ZZRingElemOrPtr)
  @ccall libflint.fmpq_mat_scalar_mul_fmpz(z::Ref{QQMatrix}, a::Ref{QQMatrix}, b::Ref{ZZRingElem})::Nothing
  return z
end

mul!(z::QQMatrixOrPtr, a::QQMatrixOrPtr, b::Integer) = mul!(z, a, ZZ(b))
mul!(z::QQMatrixOrPtr, a::QQMatrixOrPtr, b::Rational) = mul!(z, a, QQ(b))

mul!(z::QQMatrixOrPtr, a::RationalUnionOrPtr, b::QQMatrixOrPtr) = mul!(z, b, a)


function divexact!(z::QQMatrixOrPtr, x::QQMatrixOrPtr, y::QQFieldElemOrPtr)
  GC.@preserve y begin
    divexact!(z, x, _num_ptr(y))
    mul!(z, z, _den_ptr(y))
  end
  return z
end

function divexact!(z::QQMatrixOrPtr, x::QQMatrixOrPtr, y::ZZRingElemOrPtr)
  @ccall libflint.fmpq_mat_scalar_div_fmpz(z::Ref{QQMatrix}, x::Ref{QQMatrix}, y::Ref{ZZRingElem})::Nothing
  return z
end

function Generic.add_one!(a::QQMatrixOrPtr, i::Int, j::Int)
  @boundscheck _checkbounds(a, i, j)
  GC.@preserve a begin
    x = mat_entry_ptr(a, i, j)
    add!(x, 1)
  end
  return a
end

###############################################################################
#
#   Parent object call overloads
#
###############################################################################

function (a::QQMatrixSpace)()
  z = QQMatrix(nrows(a), ncols(a))
  return z
end

function (a::QQMatrixSpace)(arr::AbstractVecOrMat{QQFieldElem})
  _check_dim(nrows(a), ncols(a), arr)
  z = QQMatrix(nrows(a), ncols(a), arr)
  return z
end

function (a::QQMatrixSpace)(arr::AbstractVecOrMat{T}) where {T <: IntegerUnion}
  _check_dim(nrows(a), ncols(a), arr)
  z = QQMatrix(nrows(a), ncols(a), arr)
  return z
end

function (a::QQMatrixSpace)(arr::AbstractVecOrMat{<:Rational})
  _check_dim(nrows(a), ncols(a), arr)
  z = QQMatrix(nrows(a), ncols(a), map(QQFieldElem, arr))
  return z
end

function (a::QQMatrixSpace)(d::QQFieldElem)
  z = QQMatrix(nrows(a), ncols(a), d)
  return z
end

(a::QQMatrixSpace)(d::RationalUnion) = a(QQFieldElem(d))

function (a::QQMatrixSpace)(M::ZZMatrix)
  (ncols(a) == ncols(M) && nrows(a) == nrows(M)) || error("wrong matrix dimension")
  z = a()
  @ccall libflint.fmpq_mat_set_fmpz_mat(z::Ref{QQMatrix}, M::Ref{ZZMatrix})::Nothing
  return z
end

###############################################################################
#
#   Promotions
#
###############################################################################

promote_rule(::Type{QQMatrix}, ::Type{T}) where {T <: Integer} = QQMatrix

promote_rule(::Type{QQMatrix}, ::Type{QQFieldElem}) = QQMatrix

promote_rule(::Type{QQMatrix}, ::Type{ZZRingElem}) = QQMatrix

promote_rule(::Type{QQMatrix}, ::Type{<:Rational}) = QQMatrix

###############################################################################
#
#   Matrix constructor
#
###############################################################################

function matrix(R::QQField, arr::AbstractMatrix{T}) where {T <: RationalUnion}
  z = QQMatrix(size(arr, 1), size(arr, 2), arr)
  return z
end

function matrix(R::QQField, r::Int, c::Int, arr::AbstractVector{T}) where {T <: RationalUnion}
  _check_dim(r, c, arr)
  z = QQMatrix(r, c, arr)
  return z
end

function QQMatrix(r::Int, c::Int, arr::AbstractMatrix{T}) where {T <: RationalUnion}
  z = QQMatrix(r, c)
  GC.@preserve z for i = 1:r
    for j = 1:c
      el = mat_entry_ptr(z, i, j)
      set!(el, flintify(arr[i, j]))
    end
  end
  return z
end

function QQMatrix(r::Int, c::Int, arr::AbstractVector{T}) where {T <: RationalUnion}
  z = QQMatrix(r, c)
  GC.@preserve z for i = 1:r
    for j = 1:c
      el = mat_entry_ptr(z, i, j)
      set!(el, flintify(arr[(i-1)*c+j]))
    end
  end
  return z
end

function QQMatrix(r::Int, c::Int, d::RationalUnion)
  z = QQMatrix(r, c)
  GC.@preserve z for i = 1:min(r, c)
    el = mat_entry_ptr(z, i, i)
    set!(el, d)
  end
  return z
end

function QQMatrix(x::ZZMatrix)
  z = QQMatrix(nrows(x), ncols(x))
  @ccall libflint.fmpq_mat_set_fmpz_mat(z::Ref{QQMatrix}, x::Ref{ZZMatrix})::Nothing
  return z
end

###############################################################################
#
#  Zero matrix
#
###############################################################################

function zero_matrix(R::QQField, r::Int, c::Int)
  if r < 0 || c < 0
    error("dimensions must not be negative")
  end
  z = QQMatrix(r, c)
  return z
end

###############################################################################
#
#  Identity matrix
#
###############################################################################

function identity_matrix(R::QQField, n::Int)
  if n < 0
    error("dimension must not be negative")
  end
  return one!(QQMatrix(n, n))
end

################################################################################
#
#  Entry pointers
#
################################################################################

mat_entry_ptr(A::QQMatrix, i::Int, j::Int) = unsafe_load(A.rows, i) + (j-1)*sizeof(QQFieldElem)

################################################################################
#
#  Nullspace
#
################################################################################

function nullspace(A::QQMatrix)
  AZZ = zero_matrix(ZZ, nrows(A), ncols(A))
  @ccall libflint.fmpq_mat_get_fmpz_mat_rowwise(AZZ::Ref{ZZMatrix}, C_NULL::Ptr{Nothing}, A::Ref{QQMatrix})::Nothing
  N = similar(AZZ, ncols(A), ncols(A))
  nullity = @ccall libflint.fmpz_mat_nullspace(N::Ref{ZZMatrix}, AZZ::Ref{ZZMatrix})::Int
  NQQ = similar(A, ncols(A), ncols(A))
  @ccall libflint.fmpq_mat_set_fmpz_mat(NQQ::Ref{QQMatrix}, N::Ref{ZZMatrix})::Nothing

  # Now massage the result until it looks like what the generic AbstractAlgebra
  # nullspace would return: remove zero columns and rescale the columns so that
  # the lowest non-zero entry is 1.
  # This is necessary because code in Hecke expects this structure, see e.g.
  # https://github.com/thofma/Hecke.jl/issues/1385 .

  NQQ = view(NQQ, 1:nrows(NQQ), 1:nullity)

  # Produce a 1 in the lowest non-zero entry of any column
  s = QQ()
  t = QQ()
  for j in 1:nullity
    # Find lowest non-zero entry
    for i in nrows(NQQ):-1:1
      getindex!(s, NQQ, i, j)
      !is_zero(s) && break
    end
    # Scale the column by the inverse
    s = inv(s)
    for i in nrows(NQQ):-1:1
      is_zero_entry(NQQ, i, j) && continue
      getindex!(t, NQQ, i, j)
      mul!(t, t, s)
      NQQ[i, j] = t # creates a copy of t
    end
  end

  return nullity, NQQ
end
