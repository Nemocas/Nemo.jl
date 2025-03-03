################################################################################
#
#  nmod_mat.jl: flint nmod_mat types in julia
#
################################################################################

################################################################################
#
#  Data type and parent object methods
#
################################################################################

dense_matrix_type(::Type{zzModRingElem}) = zzModMatrix

is_zero_initialized(::Type{zzModMatrix}) = true

################################################################################
#
#  Manipulation
#
################################################################################

@inline function getindex(a::zzModMatrix, i::Int, j::Int)
  @boundscheck _checkbounds(a, i, j)
  u = getindex_raw(a, i, j)
  return zzModRingElem(u, base_ring(a)) # no reduction needed
end

#as above, but as a plain UInt, no bounds checking
function getindex_raw(a::T, i::Int, j::Int) where T <: Zmodn_mat
  return @ccall libflint.nmod_mat_get_entry(a::Ref{T}, (i - 1)::Int, (j - 1)::Int)::UInt
end

@inline function setindex!(a::T, u::UInt, i::Int, j::Int) where T <: Zmodn_mat
  @boundscheck _checkbounds(a, i, j)
  R = base_ring(a)
  setindex_raw!(a, mod(u, R.n), i, j)
end

@inline function setindex!(a::T, u::ZZRingElem, i::Int, j::Int) where T <: Zmodn_mat
  @boundscheck _checkbounds(a, i, j)
  setindex_raw!(a, u, i, j)
end

@inline function setindex!(a::zzModMatrix, u::zzModRingElem, i::Int, j::Int)
  @boundscheck _checkbounds(a, i, j)
  (base_ring(a) != parent(u)) && error("Parent objects must coincide")
  setindex_raw!(a, u.data, i, j) # no reduction necessary
end

setindex!(a::Zmodn_mat, u::Integer, i::Int, j::Int) = setindex!(a, ZZRingElem(u), i, j)

# as per setindex! but no reduction mod n and no bounds checking
function setindex_raw!(a::T, u::UInt, i::Int, j::Int) where T <: Zmodn_mat
  @ccall libflint.nmod_mat_set_entry(a::Ref{T}, (i - 1)::Int, (j - 1)::Int, u::UInt)::Nothing
end

# as per setindex! but no reduction mod n and no bounds checking
function setindex_raw!(a::T, u::ZZRingElem, i::Int, j::Int) where T <: Zmodn_mat
  t = ZZRingElem()
  @ccall libflint.fmpz_mod_ui(t::Ref{ZZRingElem}, u::Ref{ZZRingElem}, a.n::UInt)::UInt
  tt = @ccall libflint.fmpz_get_ui(t::Ref{ZZRingElem})::UInt
  setindex_raw!(a, tt, i, j)
end

function setindex!(a::zzModMatrix, b::zzModMatrix, r::UnitRange{Int64}, c::UnitRange{Int64})
  _checkbounds(a, r, c)
  size(b) == (length(r), length(c)) || throw(DimensionMismatch("tried to assign a $(size(b, 1))x$(size(b, 2)) matrix to a $(length(r))x$(length(c)) destination"))
  A = view(a, r, c)
  @ccall libflint.nmod_mat_set(A::Ref{zzModMatrix}, b::Ref{zzModMatrix})::Nothing
end

function deepcopy_internal(a::zzModMatrix, dict::IdDict)
  z = zzModMatrix(nrows(a), ncols(a), a.n)
  if isdefined(a, :base_ring)
    z.base_ring = a.base_ring
  end
  @ccall libflint.nmod_mat_set(z::Ref{zzModMatrix}, a::Ref{zzModMatrix})::Nothing
  return z
end

number_of_rows(a::T) where T <: Zmodn_mat = a.r

number_of_columns(a::T) where T <: Zmodn_mat = a.c

base_ring(a::T) where T <: Zmodn_mat = a.base_ring

function one(a::zzModMatrixSpace)
  (nrows(a) != ncols(a)) && error("Matrices must be square")
  return one!(a())
end

function iszero(a::T) where T <: Zmodn_mat
  r = @ccall libflint.nmod_mat_is_zero(a::Ref{T})::Cint
  return Bool(r)
end

@inline function is_zero_entry(A::T, i::Int, j::Int) where T <: Zmodn_mat
  @boundscheck _checkbounds(A, i, j)
  return is_zero(getindex_raw(A, i, j))
end

################################################################################
#
#  Comparison
#
################################################################################

==(a::T, b::T) where T <: Zmodn_mat = (a.base_ring == b.base_ring) &&
Bool(@ccall libflint.nmod_mat_equal(a::Ref{T}, b::Ref{T})::Cint)

isequal(a::T, b::T) where T <: Zmodn_mat = ==(a, b)

################################################################################
#
#  Transpose
#
################################################################################

function transpose(a::T) where T <: Zmodn_mat
  z = similar(a, ncols(a), nrows(a))
  @ccall libflint.nmod_mat_transpose(z::Ref{T}, a::Ref{T})::Nothing
  return z
end

function transpose!(a::T) where T <: Zmodn_mat
  !is_square(a) && error("Matrix must be a square matrix")
  @ccall libflint.nmod_mat_transpose(a::Ref{T}, a::Ref{T})::Nothing
end

###############################################################################
#
#   Row and column swapping
#
###############################################################################

function swap_rows!(x::T, i::Int, j::Int) where T <: Zmodn_mat
  @ccall libflint.nmod_mat_swap_rows(x::Ref{T}, C_NULL::Ptr{Nothing}, (i - 1)::Int, (j - 1)::Int)::Nothing
  return x
end

function swap_cols!(x::T, i::Int, j::Int) where T <: Zmodn_mat
  @ccall libflint.nmod_mat_swap_cols(x::Ref{T}, C_NULL::Ptr{Nothing}, (i - 1)::Int, (j - 1)::Int)::Nothing
  return x
end

function reverse_rows!(x::T) where T <: Zmodn_mat
  @ccall libflint.nmod_mat_invert_rows(x::Ref{T}, C_NULL::Ptr{Nothing})::Nothing
  return x
end

function reverse_cols!(x::T) where T <: Zmodn_mat
  @ccall libflint.nmod_mat_invert_cols(x::Ref{T}, C_NULL::Ptr{Nothing})::Nothing
  return x
end

################################################################################
#
#  Unary operators
#
################################################################################

-(x::T) where T <: Zmodn_mat = neg!(similar(x), x)

################################################################################
#
#  Binary operators
#
################################################################################

function +(x::T, y::T) where T <: Zmodn_mat
  check_parent(x,y)
  return add!(similar(x), x, y)
end

function -(x::T, y::T) where T <: Zmodn_mat
  check_parent(x,y)
  return sub!(similar(x), x, y)
end

function *(x::T, y::T) where T <: Zmodn_mat
  (base_ring(x) != base_ring(y)) && error("Base ring must be equal")
  (ncols(x) != nrows(y)) && error("Dimensions are wrong")
  z = similar(x, nrows(x), ncols(y))
  return mul!(z, x, y)
end

################################################################################
#
#  Unsafe operations
#
################################################################################

function zero!(a::T) where T <: Zmodn_mat
  @ccall libflint.nmod_mat_zero(a::Ref{T})::Nothing
  return a
end

function one!(a::T) where T <: Zmodn_mat
  @ccall libflint.nmod_mat_one(a::Ref{T})::Nothing
  return a
end

function neg!(z::T, a::T) where T <: Zmodn_mat
  @ccall libflint.nmod_mat_neg(z::Ref{T}, a::Ref{T})::Nothing
  return z
end

function mul!(a::T, b::T, c::T) where T <: Zmodn_mat
  @ccall libflint.nmod_mat_mul(a::Ref{T}, b::Ref{T}, c::Ref{T})::Nothing
  return a
end

function add!(a::T, b::T, c::T) where T <: Zmodn_mat
  @ccall libflint.nmod_mat_add(a::Ref{T}, b::Ref{T}, c::Ref{T})::Nothing
  return a
end

function sub!(a::T, b::T, c::T) where T <: Zmodn_mat
  @ccall libflint.nmod_mat_sub(a::Ref{T}, b::Ref{T}, c::Ref{T})::Nothing
  return a
end

# entries of b required to be in [0,n)
function mul!(z::Vector{UInt}, a::T, b::Vector{UInt}) where T <: Zmodn_mat
  @ccall libflint.nmod_mat_mul_nmod_vec(z::Ptr{UInt}, a::Ref{T}, b::Ptr{UInt}, length(b)::Int)::Nothing
  return z
end

# entries of a required to be in [0,n)
function mul!(z::Vector{UInt}, a::Vector{UInt}, b::T) where T <: Zmodn_mat
  @ccall libflint.nmod_mat_nmod_vec_mul(z::Ptr{UInt}, a::Ptr{UInt}, length(a)::Int, b::Ref{T})::Nothing
  return z
end

function mul!(a::T, b::T, c::UInt) where T <: Zmodn_mat
  @ccall libflint.nmod_mat_scalar_mul(a::Ref{T}, b::Ref{T}, c::UInt)::Nothing
  return a
end

function mul!(a::T, b::UInt, c::T) where T <: Zmodn_mat
  return mul!(a, c, b)
end

function mul!(a::zzModMatrix, b::zzModMatrix, c::zzModRingElem)
  return mul!(a, b, c.data)
end

function mul!(a::zzModMatrix, b::zzModRingElem, c::zzModMatrix)
  return mul!(a, c, b)
end

function mul!(a::fpMatrix, b::fpMatrix, c::fpFieldElem)
  return mul!(a, b, c.data)
end

function mul!(a::fpMatrix, b::fpFieldElem, c::fpMatrix)
  return mul!(a, c, b)
end

function addmul!(A::fpMatrix, B::fpMatrix, C::fpFieldElem, D::fpMatrix)
  @ccall libflint.nmod_mat_scalar_addmul_ui(A::Ref{fpMatrix}, B::Ref{fpMatrix}, D::Ref{fpMatrix}, C.data::UInt)::Nothing
  return A
end

function Generic.add_one!(a::T, i::Int, j::Int) where T <: Zmodn_mat
  @boundscheck _checkbounds(a, i, j)
  x = getindex_raw(a, i, j)
  x += 1
  if x == base_ring(a).n
    x = UInt(0)
  end
  setindex_raw!(a, x, i, j)
  return a
end

################################################################################
#
#  Ad hoc binary operators
#
################################################################################

function *(x::T, y::UInt) where T <: Zmodn_mat
  return mul!(similar(x), x, y)
end

*(x::UInt, y::T) where T <: Zmodn_mat = y*x

function *(x::T, y::ZZRingElem) where T <: Zmodn_mat
  t = ZZRingElem()
  @ccall libflint.fmpz_mod_ui(t::Ref{ZZRingElem}, y::Ref{ZZRingElem}, x.n::UInt)::UInt
  tt = @ccall libflint.fmpz_get_ui(t::Ref{ZZRingElem})::UInt
  return x*tt
end

*(x::ZZRingElem, y::T) where T <: Zmodn_mat = y*x

function *(x::T, y::Integer) where T <: Zmodn_mat
  return x*ZZRingElem(y)
end

*(x::Integer, y::T) where T <: Zmodn_mat = y*x

function *(x::zzModMatrix, y::zzModRingElem)
  (base_ring(x) != parent(y)) && error("Parent objects must coincide")
  return x*y.data
end

*(x::zzModRingElem, y::zzModMatrix) = y*x

################################################################################
#
#  Powering
#
################################################################################

function ^(x::T, y::UInt) where T <: Zmodn_mat
  nrows(x) != ncols(x) && error("Incompatible matrix dimensions")
  z = similar(x)
  @ccall libflint.nmod_mat_pow(z::Ref{T}, x::Ref{T}, y::UInt)::Nothing
  return z
end

function ^(x::T, y::Int) where T <: Zmodn_mat
  ( y < 0 ) && error("Exponent must be positive")
  return x^UInt(y)
end

function ^(x::T, y::ZZRingElem) where T <: Zmodn_mat
  ( y < 0 ) && error("Exponent must be positive")
  ( y > ZZRingElem(typemax(UInt))) &&
  error("Exponent must be smaller than ", ZZRingElem(typemax(UInt)))
  return x^(UInt(y))
end

################################################################################
#
#  Strong echelon form and Howell form
#
################################################################################

function strong_echelon_form!(a::T) where T <: Zmodn_mat
  @ccall libflint.nmod_mat_strong_echelon_form(a::Ref{T})::Nothing
end

@doc raw"""
    strong_echelon_form(a::zzModMatrix)

Return the strong echeleon form of $a$. The matrix $a$ must have at least as
many rows as columns.
"""
function strong_echelon_form(a::zzModMatrix)
  (nrows(a) < ncols(a)) &&
  error("Matrix must have at least as many rows as columns")
  z = deepcopy(a)
  strong_echelon_form!(z)
  return z
end

function howell_form!(a::T) where T <: Zmodn_mat
  @ccall libflint.nmod_mat_howell_form(a::Ref{T})::Nothing
end

@doc raw"""
    howell_form(a::zzModMatrix)

Return the Howell normal form of $a$. The matrix $a$ must have at least as
many rows as columns.
"""
function howell_form(a::zzModMatrix)
  (nrows(a) < ncols(a)) &&
  error("Matrix must have at least as many rows as columns")

  z = deepcopy(a)
  howell_form!(z)
  return z
end

################################################################################
#
#  Trace
#
################################################################################

function tr(a::T) where T <: Zmodn_mat
  !is_square(a) && error("Matrix must be a square matrix")
  r = @ccall libflint.nmod_mat_trace(a::Ref{T})::UInt
  return base_ring(a)(r)
end

################################################################################
#
#  Determinant
#
################################################################################

function det(a::zzModMatrix)
  !is_square(a) && error("Matrix must be a square matrix")
  if is_prime(a.n)
    r = @ccall libflint.nmod_mat_det(a::Ref{zzModMatrix})::UInt
    return base_ring(a)(r)
  else
    try
      return AbstractAlgebra.det_fflu(a)
    catch
      return AbstractAlgebra.det_df(a)
    end
  end
end

################################################################################
#
#  Rank
#
################################################################################

function rank(a::T) where T <: Zmodn_mat
  r = @ccall libflint.nmod_mat_rank(a::Ref{T})::Int
  return r
end

################################################################################
#
#  Inverse
#
################################################################################

function inv(a::zzModMatrix)
  !is_square(a) && error("Matrix must be a square matrix")
  if is_prime(a.n)
    z = similar(a)
    r = @ccall libflint.nmod_mat_inv(z::Ref{zzModMatrix}, a::Ref{zzModMatrix})::Int
    !Bool(r) && error("Matrix not invertible")
    return z
  else
    b = lift(a)
    c, d = pseudo_inv(b)
    R = base_ring(a)

    if !isone(gcd(d, modulus(R)))
      error("Matrix not invertible")
    end
    return change_base_ring(R, c) * inv(R(d))
  end
end

################################################################################
#
#  Linear solving
#
################################################################################

function AbstractAlgebra._solve_triu(x::T, y::T) where T <: Zmodn_mat
  (base_ring(x) != base_ring(y)) && error("Matrices must have same base ring")
  is_upper_triangular(x) || error("Matrix must be upper triangular")
  z = similar(x, nrows(x), ncols(y))
  _solve_triu!(z, x, y, 0)
  return z
end

#solves upper_triangular_part(B)A = C, 
#if unit == 1, then only the strictly upper triangular part is used
#and the diagonal is assumed to be 1
#
# useful in the context of solving/ lu decomposition: the lu
# is done inplace, so the lower part wil be "l", the upper "u",
# both are implicit only.
function _solve_triu!(A::T, B::T, C::T, unit::Int = 0) where T <: Zmodn_mat
  @ccall libflint.nmod_mat_solve_triu(A::Ref{T}, B::Ref{T}, C::Ref{T}, unit::Cint)::Cvoid
end

#solves lower_triangular_part(B)A = C, 
#if unit == 1, then only the strictly lower triangular part is used
#and the diagonal is assumed to be 1
function AbstractAlgebra._solve_tril!(A::T, B::T, C::T, unit::Int = 0) where T <: Zmodn_mat
  @ccall libflint.nmod_mat_solve_tril(A::Ref{T}, B::Ref{T}, C::Ref{T}, unit::Cint)::Cvoid
end

function Solve._can_solve_internal_no_check(::Solve.LUTrait, A::zzModMatrix, b::zzModMatrix, task::Symbol; side::Symbol = :left)
  if side === :left
    fl, sol, K = Solve._can_solve_internal_no_check(Solve.LUTrait(), transpose(A), transpose(b), task, side = :right)
    return fl, transpose(sol), transpose(K)
  end

  x = similar(A, ncols(A), ncols(b))
  # This is probably only correct if the characteristic is prime
  fl = @ccall libflint.nmod_mat_can_solve(x::Ref{zzModMatrix}, A::Ref{zzModMatrix}, b::Ref{zzModMatrix})::Cint
  if task === :only_check || task === :with_solution
    return Bool(fl), x, zero(A, 0, 0)
  end
  return Bool(fl), x, kernel(A, side = :right)
end

# For _can_solve_internal_no_check(::HowellFormTrait, ...) we use generic
# AbstractAlgebra code

################################################################################
#
#  LU decomposition
#
################################################################################

function lu!(P::Perm, x::T) where T <: Zmodn_mat
  P.d .-= 1
  rank = Int(@ccall libflint.nmod_mat_lu(P.d::Ptr{Int}, x::Ref{T}, 0::Cint)::Cint)
  P.d .+= 1

  inv!(P) # FLINT does PLU = x instead of Px = LU

  return rank
end

function lu(x::T, P = SymmetricGroup(nrows(x))) where T <: Zmodn_mat
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
        L[i, j] = R(1)
      elseif j <= m
        L[i, j] = R()
      end
    end
  end
  return rank, p, L, U
end

#to support FAST lu!
function AbstractAlgebra.Strassen.apply!(A::fpMatrix, P::Perm{Int}; offset::Int = 0)
  n = length(P.d)
  t = zeros(Int, n-offset)
  for i=1:n-offset
    t[i] = unsafe_load(reinterpret(Ptr{Int}, A.rows), P.d[i] + offset)
  end
  for i=1:n-offset
    unsafe_store!(reinterpret(Ptr{Int}, A.rows), t[i], i + offset)
  end
end

################################################################################
#
#  Windowing
#
################################################################################

function Base.view(x::zzModMatrix, r1::Int, c1::Int, r2::Int, c2::Int)

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

  z = zzModMatrix()
  z.base_ring = x.base_ring
  z.view_parent = x
  @ccall libflint.nmod_mat_window_init(z::Ref{zzModMatrix}, x::Ref{zzModMatrix}, (r1 - 1)::Int, (c1 - 1)::Int, r2::Int, c2::Int)::Nothing
  finalizer(_nmod_mat_window_clear_fn, z)
  return z
end

function Base.view(x::T, r::AbstractUnitRange{Int}, c::AbstractUnitRange{Int}) where T <: Zmodn_mat
  return Base.view(x, first(r), first(c), last(r), last(c))
end

function _nmod_mat_window_clear_fn(a::zzModMatrix)
  @ccall libflint.nmod_mat_window_clear(a::Ref{zzModMatrix})::Nothing
end

function sub(x::T, r1::Int, c1::Int, r2::Int, c2::Int) where T <: Zmodn_mat
  return deepcopy(Base.view(x, r1, c1, r2, c2))
end

function sub(x::T, r::AbstractUnitRange{Int}, c::AbstractUnitRange{Int}) where T <: Zmodn_mat
  return deepcopy(Base.view(x, r, c))
end

function getindex(x::T, r::AbstractUnitRange{Int}, c::AbstractUnitRange{Int}) where T <: Zmodn_mat
  sub(x, r, c)
end

################################################################################
#
#  Concatenation
#
################################################################################

function hcat(x::T, y::T) where T <: Zmodn_mat
  (base_ring(x) != base_ring(y)) && error("Matrices must have same base ring")
  (x.r != y.r) && error("Matrices must have same number of rows")
  z = similar(x, nrows(x), ncols(x) + ncols(y))
  @ccall libflint.nmod_mat_concat_horizontal(z::Ref{T}, x::Ref{T}, y::Ref{T})::Nothing
  return z
end

function vcat(x::T, y::T) where T <: Zmodn_mat
  (base_ring(x) != base_ring(y)) && error("Matrices must have same base ring")
  (x.c != y.c) && error("Matrices must have same number of columns")
  z = similar(x, nrows(x) + nrows(y), ncols(x))
  @ccall libflint.nmod_mat_concat_vertical(z::Ref{T}, x::Ref{T}, y::Ref{T})::Nothing
  return z
end

################################################################################
#
#  Lifting
#
################################################################################

@doc raw"""
    lift(a::T) where {T <: Zmodn_mat}

Return a lift of the matrix $a$ to a matrix over $\mathbb{Z}$, i.e. where the
entries of the returned matrix are those of $a$ lifted to $\mathbb{Z}$.
"""
function lift(a::T) where {T <: Zmodn_mat}
  z = ZZMatrix(nrows(a), ncols(a))
  @ccall libflint.fmpz_mat_set_nmod_mat(z::Ref{ZZMatrix}, a::Ref{T})::Nothing
  return z
end

function lift!(z::ZZMatrix, a::T) where T <: Zmodn_mat
  @ccall libflint.fmpz_mat_set_nmod_mat(z::Ref{ZZMatrix}, a::Ref{T})::Nothing
  return z
end

################################################################################
#
#  Characteristic polynomial
#
################################################################################

function charpoly(R::zzModPolyRing, a::zzModMatrix)
  m = deepcopy(a)
  p = R()
  @ccall libflint.nmod_mat_charpoly(p::Ref{zzModPolyRingElem}, m::Ref{zzModMatrix})::Nothing
  return p
end

################################################################################
#
#  Minimal polynomial
#
################################################################################

function minpoly(R::zzModPolyRing, a::zzModMatrix)
  p = R()
  @ccall libflint.nmod_mat_minpoly(p::Ref{zzModPolyRingElem}, a::Ref{zzModMatrix})::Nothing
  return p
end

###############################################################################
#
#   Promotion rules
#
###############################################################################

promote_rule(::Type{zzModMatrix}, ::Type{V}) where {V <: Integer} = zzModMatrix

promote_rule(::Type{zzModMatrix}, ::Type{zzModRingElem}) = zzModMatrix

promote_rule(::Type{zzModMatrix}, ::Type{ZZRingElem}) = zzModMatrix

################################################################################
#
#  Parent object overloading
#
################################################################################

function (a::zzModMatrixSpace)()
  z = zzModMatrix(base_ring(a), undef, nrows(a), ncols(a))
  return z
end

function (a::zzModMatrixSpace)(arr::AbstractVecOrMat{T}) where {T <: IntegerUnion}
  _check_dim(nrows(a), ncols(a), arr)
  z = zzModMatrix(nrows(a), ncols(a), modulus(base_ring(a)), arr)
  z.base_ring = a.base_ring
  return z
end

function (a::zzModMatrixSpace)(arr::AbstractVecOrMat{zzModRingElem})
  _check_dim(nrows(a), ncols(a), arr)
  (length(arr) > 0 && (base_ring(a) != parent(arr[1]))) && error("Elements must have same base ring")
  z = zzModMatrix(nrows(a), ncols(a), modulus(base_ring(a)), arr)
  z.base_ring = a.base_ring
  return z
end

function (a::zzModMatrixSpace)(b::ZZMatrix)
  (ncols(a) != b.c || nrows(a) != b.r) && error("Dimensions do not fit")
  z = zzModMatrix(modulus(base_ring(a)), b)
  z.base_ring = a.base_ring
  return z
end

###############################################################################
#
#   Matrix constructor
#
###############################################################################

function matrix(R::zzModRing, arr::AbstractMatrix{<: Union{zzModRingElem, ZZRingElem, Integer}})
  z = zzModMatrix(size(arr, 1), size(arr, 2), R.n, arr)
  z.base_ring = R
  return z
end

function matrix(R::zzModRing, r::Int, c::Int, arr::AbstractVector{<: Union{zzModRingElem, ZZRingElem, Integer}})
  _check_dim(r, c, arr)
  z = zzModMatrix(r, c, R.n, arr)
  z.base_ring = R
  return z
end

################################################################################
#
#  Kernel
#
################################################################################

function nullspace(M::zzModMatrix)
  # Apparently this only works correctly if base_ring(M) is a field
  N = similar(M, ncols(M), ncols(M))
  nullity = @ccall libflint.nmod_mat_nullspace(N::Ref{zzModMatrix}, M::Ref{zzModMatrix})::Int
  return nullity, view(N, 1:nrows(N), 1:nullity)
end

function kernel(::Solve.RREFTrait, M::Union{zzModMatrix, ZZModMatrix}; side::Symbol = :left)
  Solve.check_option(side, [:right, :left], "side")

  if side === :left
    K = kernel(Solve.RREFTrait(), transpose(M), side = :right)
    return transpose(K)
  end

  return nullspace(M)[2]
end

# For kernel(::HowellFormTrait, ...) we use generic AbstractAlgebra code

################################################################################
#
#  Entry pointers
#
################################################################################

mat_entry_ptr(A::Zmodn_mat, i::Int, j::Int) = unsafe_load(A.rows, i) + (j-1)*sizeof(UInt)
