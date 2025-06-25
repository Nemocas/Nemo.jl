###############################################################################
#
#   arb_mat.jl : Arb matrices over ArbFieldElem
#
###############################################################################

###############################################################################
#
#   Basic manipulation
#
###############################################################################

base_ring(a::ArbMatrix) = a.base_ring

dense_matrix_type(::Type{ArbFieldElem}) = ArbMatrix

is_zero_initialized(::Type{ArbMatrix}) = true

precision(x::ArbMatrixSpace) = precision(x.base_ring)

function getindex!(z::ArbFieldElem, x::ArbMatrix, r::Int, c::Int)
  GC.@preserve x begin
    v = mat_entry_ptr(x, r, c)
    _arb_set(z, v)
  end
  return z
end

@inline function getindex(x::ArbMatrix, r::Int, c::Int)
  @boundscheck _checkbounds(x, r, c)

  z = base_ring(x)()
  GC.@preserve x begin
    v = mat_entry_ptr(x, r, c)
    _arb_set(z, v)
  end
  return z
end

for T in [Int, UInt, ZZRingElem, QQFieldElem, Float64, BigFloat, ArbFieldElem, AbstractString]
  @eval begin
    @inline function setindex!(x::ArbMatrix, y::$T, r::Int, c::Int)
      @boundscheck _checkbounds(x, r, c)

      GC.@preserve x begin
        z = mat_entry_ptr(x, r, c)
        _arb_set(z, y, precision(base_ring(x)))
      end
    end
  end
end

Base.@propagate_inbounds setindex!(x::ArbMatrix, y::Integer,
                                   r::Int, c::Int) =
setindex!(x, ZZRingElem(y), r, c)

Base.@propagate_inbounds setindex!(x::ArbMatrix, y::Rational{T},
                                   r::Int, c::Int) where {T <: Integer} =
setindex!(x, ZZRingElem(y), r, c)

function one(x::ArbMatrixSpace)
  check_square(x)
  return one!(x())
end

number_of_rows(a::ArbMatrix) = a.r

number_of_columns(a::ArbMatrix) = a.c

function deepcopy_internal(x::ArbMatrix, dict::IdDict)
  z = ArbMatrix(base_ring(x), undef, nrows(x), ncols(x))
  @ccall libflint.arb_mat_set(z::Ref{ArbMatrix}, x::Ref{ArbMatrix})::Nothing
  return z
end

################################################################################
#
#  Unary operations
#
################################################################################

-(x::ArbMatrix) = neg!(similar(x), x)

################################################################################
#
#  Transpose
#
################################################################################

function transpose(x::ArbMatrix)
  z = similar(x, ncols(x), nrows(x))
  @ccall libflint.arb_mat_transpose(z::Ref{ArbMatrix}, x::Ref{ArbMatrix})::Nothing
  return z
end

################################################################################
#
#  Binary operations
#
################################################################################

function +(x::ArbMatrix, y::ArbMatrix)
  check_parent(x, y)
  z = similar(x)
  @ccall libflint.arb_mat_add(z::Ref{ArbMatrix}, x::Ref{ArbMatrix}, y::Ref{ArbMatrix}, precision(parent(x))::Int)::Nothing
  return z
end

function -(x::ArbMatrix, y::ArbMatrix)
  check_parent(x, y)
  z = similar(x)
  @ccall libflint.arb_mat_sub(z::Ref{ArbMatrix}, x::Ref{ArbMatrix}, y::Ref{ArbMatrix}, precision(parent(x))::Int)::Nothing
  return z
end

function *(x::ArbMatrix, y::ArbMatrix)
  ncols(x) != nrows(y) && error("Matrices have wrong dimensions")
  z = similar(x, nrows(x), ncols(y))
  @ccall libflint.arb_mat_mul(z::Ref{ArbMatrix}, x::Ref{ArbMatrix}, y::Ref{ArbMatrix}, precision(base_ring(x))::Int)::Nothing
  return z
end

################################################################################
#
#   Ad hoc binary operators
#
################################################################################

function ^(x::ArbMatrix, y::UInt)
  nrows(x) != ncols(x) && error("Matrix must be square")
  z = similar(x)
  @ccall libflint.arb_mat_pow_ui(z::Ref{ArbMatrix}, x::Ref{ArbMatrix}, y::UInt, precision(base_ring(x))::Int)::Nothing
  return z
end

function *(x::ArbMatrix, y::Int)
  z = similar(x)
  @ccall libflint.arb_mat_scalar_mul_si(z::Ref{ArbMatrix}, x::Ref{ArbMatrix}, y::Int, precision(base_ring(x))::Int)::Nothing
  return z
end

*(x::Int, y::ArbMatrix) = y*x

*(x::ArbMatrix, y::QQFieldElem) = x*base_ring(x)(y)

*(x::QQFieldElem, y::ArbMatrix) = y*x

function *(x::ArbMatrix, y::ZZRingElem)
  z = similar(x)
  @ccall libflint.arb_mat_scalar_mul_fmpz(z::Ref{ArbMatrix}, x::Ref{ArbMatrix}, y::Ref{ZZRingElem}, precision(base_ring(x))::Int)::Nothing
  return z
end

*(x::ZZRingElem, y::ArbMatrix) = y*x

function *(x::ArbMatrix, y::ArbFieldElem)
  z = similar(x)
  @ccall libflint.arb_mat_scalar_mul_arb(z::Ref{ArbMatrix}, x::Ref{ArbMatrix}, y::Ref{ArbFieldElem}, precision(base_ring(x))::Int)::Nothing
  return z
end

*(x::ArbFieldElem, y::ArbMatrix) = y*x

for T in [Integer, ZZRingElem, QQFieldElem, ArbFieldElem]
  @eval begin
    function +(x::ArbMatrix, y::$T)
      z = deepcopy(x)
      for i = 1:min(nrows(x), ncols(x))
        z[i, i] += y
      end
      return z
    end

    +(x::$T, y::ArbMatrix) = y + x

    function -(x::ArbMatrix, y::$T)
      z = deepcopy(x)
      for i = 1:min(nrows(x), ncols(x))
        z[i, i] -= y
      end
      return z
    end

    function -(x::$T, y::ArbMatrix)
      z = -y
      for i = 1:min(nrows(y), ncols(y))
        z[i, i] += x
      end
      return z
    end
  end
end

function +(x::ArbMatrix, y::Rational{T}) where T <: Union{Int, BigInt}
  z = deepcopy(x)
  for i = 1:min(nrows(x), ncols(x))
    z[i, i] += y
  end
  return z
end

+(x::Rational{T}, y::ArbMatrix) where T <: Union{Int, BigInt} = y + x

function -(x::ArbMatrix, y::Rational{T}) where T <: Union{Int, BigInt}
  z = deepcopy(x)
  for i = 1:min(nrows(x), ncols(x))
    z[i, i] -= y
  end
  return z
end

function -(x::Rational{T}, y::ArbMatrix) where T <: Union{Int, BigInt}
  z = -y
  for i = 1:min(nrows(y), ncols(y))
    z[i, i] += x
  end
  return z
end

###############################################################################
#
#   Shifting
#
###############################################################################

function ldexp(x::ArbMatrix, y::Int)
  z = similar(x)
  @ccall libflint.arb_mat_scalar_mul_2exp_si(z::Ref{ArbMatrix}, x::Ref{ArbMatrix}, y::Int)::Nothing
  return z
end

###############################################################################
#
#   Comparisons
#
###############################################################################

@doc raw"""
    isequal(x::ArbMatrix, y::ArbMatrix)

Return `true` if the matrices of balls $x$ and $y$ are precisely equal,
i.e. if all matrix entries have the same midpoints and radii.
"""
function isequal(x::ArbMatrix, y::ArbMatrix)
  r = @ccall libflint.arb_mat_equal(x::Ref{ArbMatrix}, y::Ref{ArbMatrix})::Cint
  return Bool(r)
end

function ==(x::ArbMatrix, y::ArbMatrix)
  fl = check_parent(x, y, false)
  !fl && return false
  r = @ccall libflint.arb_mat_eq(x::Ref{ArbMatrix}, y::Ref{ArbMatrix})::Cint
  return Bool(r)
end

function !=(x::ArbMatrix, y::ArbMatrix)
  r = @ccall libflint.arb_mat_ne(x::Ref{ArbMatrix}, y::Ref{ArbMatrix})::Cint
  return Bool(r)
end

@doc raw"""
    overlaps(x::ArbMatrix, y::ArbMatrix)

Returns `true` if all entries of $x$ overlap with the corresponding entry of
$y$, otherwise return `false`.
"""
function overlaps(x::ArbMatrix, y::ArbMatrix)
  r = @ccall libflint.arb_mat_overlaps(x::Ref{ArbMatrix}, y::Ref{ArbMatrix})::Cint
  return Bool(r)
end

@doc raw"""
    contains(x::ArbMatrix, y::ArbMatrix)

Returns `true` if all entries of $x$ contain the corresponding entry of
$y$, otherwise return `false`.
"""
function contains(x::ArbMatrix, y::ArbMatrix)
  r = @ccall libflint.arb_mat_contains(x::Ref{ArbMatrix}, y::Ref{ArbMatrix})::Cint
  return Bool(r)
end

###############################################################################
#
#   Ad hoc comparisons
#
###############################################################################

@doc raw"""
    contains(x::ArbMatrix, y::ZZMatrix)

Returns `true` if all entries of $x$ contain the corresponding entry of
$y$, otherwise return `false`.
"""
function contains(x::ArbMatrix, y::ZZMatrix)
  r = @ccall libflint.arb_mat_contains_fmpz_mat(x::Ref{ArbMatrix}, y::Ref{ZZMatrix})::Cint
  return Bool(r)
end


@doc raw"""
    contains(x::ArbMatrix, y::QQMatrix)

Returns `true` if all entries of $x$ contain the corresponding entry of
$y$, otherwise return `false`.
"""
function contains(x::ArbMatrix, y::QQMatrix)
  r = @ccall libflint.arb_mat_contains_fmpq_mat(x::Ref{ArbMatrix}, y::Ref{QQMatrix})::Cint
  return Bool(r)
end

==(x::ArbMatrix, y::Integer) = x == parent(x)(y)

==(x::Integer, y::ArbMatrix) = y == x

==(x::ArbMatrix, y::ZZRingElem) = x == parent(x)(y)

==(x::ZZRingElem, y::ArbMatrix) = y == x

==(x::ArbMatrix, y::ZZMatrix) = x == parent(x)(y)

==(x::ZZMatrix, y::ArbMatrix) = y == x

###############################################################################
#
#   Inversion
#
###############################################################################

@doc raw"""
    inv(x::ArbMatrix)

Given a  $n\times n$ matrix of type `ArbMatrix`, return an
$n\times n$ matrix $X$ such that $AX$ contains the
identity matrix. If $A$ cannot be inverted numerically an exception is raised.
"""
function inv(x::ArbMatrix)
  fl, z = is_invertible_with_inverse(x)
  fl && return z
  error("Matrix singular or cannot be inverted numerically")
end

function is_invertible_with_inverse(x::ArbMatrix)
  ncols(x) != nrows(x) && return false, x
  z = similar(x)
  r = @ccall libflint.arb_mat_inv(z::Ref{ArbMatrix}, x::Ref{ArbMatrix}, precision(base_ring(x))::Int)::Cint
  return Bool(r), z
end

###############################################################################
#
#   Exact division
#
###############################################################################

function divexact(x::ArbMatrix, y::ArbMatrix; check::Bool=true)
  ncols(x) != ncols(y) && error("Incompatible matrix dimensions")
  x*inv(y)
end

###############################################################################
#
#   Ad hoc exact division
#
###############################################################################

function divexact(x::ArbMatrix, y::Int; check::Bool=true)
  y == 0 && throw(DivideError())
  z = similar(x)
  @ccall libflint.arb_mat_scalar_div_si(z::Ref{ArbMatrix}, x::Ref{ArbMatrix}, y::Int, precision(base_ring(x))::Int)::Nothing
  return z
end

function divexact(x::ArbMatrix, y::ZZRingElem; check::Bool=true)
  z = similar(x)
  @ccall libflint.arb_mat_scalar_div_fmpz(z::Ref{ArbMatrix}, x::Ref{ArbMatrix}, y::Ref{ZZRingElem}, precision(base_ring(x))::Int)::Nothing
  return z
end

function divexact(x::ArbMatrix, y::ArbFieldElem; check::Bool=true)
  z = similar(x)
  @ccall libflint.arb_mat_scalar_div_arb(z::Ref{ArbMatrix}, x::Ref{ArbMatrix}, y::Ref{ArbFieldElem}, precision(base_ring(x))::Int)::Nothing
  return z
end

################################################################################
#
#  Characteristic polynomial
#
################################################################################

function charpoly(x::ArbPolyRing, y::ArbMatrix)
  base_ring(y) != base_ring(x) && error("Base rings must coincide")
  z = x()
  @ccall libflint.arb_mat_charpoly(z::Ref{ArbPolyRingElem}, y::Ref{ArbMatrix}, precision(base_ring(y))::Int)::Nothing
  return z
end

###############################################################################
#
#   Determinant
#
###############################################################################

function det(x::ArbMatrix)
  ncols(x) != nrows(x) && error("Matrix must be square")
  z = base_ring(x)()
  @ccall libflint.arb_mat_det(z::Ref{ArbFieldElem}, x::Ref{ArbMatrix}, precision(base_ring(x))::Int)::Nothing
  return z
end

################################################################################
#
#   Exponential function
#
################################################################################

function Base.exp(x::ArbMatrix)
  ncols(x) != nrows(x) && error("Matrix must be square")
  z = similar(x)
  @ccall libflint.arb_mat_exp(z::Ref{ArbMatrix}, x::Ref{ArbMatrix}, precision(base_ring(x))::Int)::Nothing
  return z
end

###############################################################################
#
#   Linear solving
#
###############################################################################

function cholesky(x::ArbMatrix)
  ncols(x) != nrows(x) && error("Matrix must be square")
  z = similar(x, nrows(x), ncols(x))
  p = precision(base_ring(x))
  fl = @ccall libflint.arb_mat_cho(z::Ref{ArbMatrix}, x::Ref{ArbMatrix}, p::Int)::Cint
  fl == 0 && error("Matrix is not positive definite")
  return z
end

function lu!(P::Perm, z::ArbMatrix, x::ArbMatrix)
  parent(P).n != nrows(x) && error("Permutation does not match matrix")
  P.d .-= 1
  r = @ccall libflint.arb_mat_lu(P.d::Ptr{Int}, z::Ref{ArbMatrix}, x::Ref{ArbMatrix}, precision(base_ring(x))::Int)::Cint
  r == 0 && error("Could not find $(nrows(x)) invertible pivot elements")
  P.d .+= 1
  inv!(P) # FLINT does PLU = x instead of Px = LU
  return nrows(x)
end

function lu!(P::Perm, x::ArbMatrix)
  return lu!(P, x, x)
end

function _solve!(z::ArbMatrix, x::ArbMatrix, y::ArbMatrix)
  r = @ccall libflint.arb_mat_solve(z::Ref{ArbMatrix}, x::Ref{ArbMatrix}, y::Ref{ArbMatrix}, precision(base_ring(x))::Int)::Cint
  r == 0 && error("Matrix cannot be inverted numerically")
  nothing
end

function _solve_lu_precomp!(z::ArbMatrix, P::Perm, LU::ArbMatrix, y::ArbMatrix)
  Q = inv(P)
  @ccall libflint.arb_mat_solve_lu_precomp(z::Ref{ArbMatrix}, (Q.d .- 1)::Ptr{Int}, LU::Ref{ArbMatrix}, y::Ref{ArbMatrix}, precision(base_ring(LU))::Int)::Nothing
  nothing
end

function _solve_lu_precomp(P::Perm, LU::ArbMatrix, y::ArbMatrix)
  ncols(LU) != nrows(y) && error("Matrix dimensions are wrong")
  z = similar(y)
  _solve_lu_precomp!(z, P, LU, y)
  return z
end

function _solve_cholesky_precomp!(z::ArbMatrix, cho::ArbMatrix, y::ArbMatrix)
  @ccall libflint.arb_mat_solve_cho_precomp(z::Ref{ArbMatrix}, cho::Ref{ArbMatrix}, y::Ref{ArbMatrix}, precision(base_ring(cho))::Int)::Nothing
  nothing
end

function _solve_cholesky_precomp(cho::ArbMatrix, y::ArbMatrix)
  ncols(cho) != nrows(y) && error("Matrix dimensions are wrong")
  z = similar(y)
  _solve_cholesky_precomp!(z, cho, y)
  return z
end

Solve.matrix_normal_form_type(::ArbField) = Solve.LUTrait()
Solve.matrix_normal_form_type(::ArbMatrix) = Solve.LUTrait()

function Solve._can_solve_internal_no_check(::Solve.LUTrait, A::ArbMatrix, b::ArbMatrix, task::Symbol; side::Symbol = :left)
  nrows(A) != ncols(A) && error("Only implemented for square matrices")
  if side === :left
    fl, sol, K = Solve._can_solve_internal_no_check(Solve.LUTrait(), transpose(A), transpose(b), task, side = :right)
    return fl, transpose(sol), transpose(K)
  end

  x = similar(A, ncols(A), ncols(b))
  _solve!(x, A, b)
  if task === :only_check || task === :with_solution
    return true, x, zero(A, 0, 0)
  end
  # If we ended up here, then A is invertible, so the kernel is trivial
  return true, x, zero(A, ncols(A), 0)
end

################################################################################
#
#   Linear solving via context object
#
################################################################################

function Solve._init_reduce(C::Solve.SolveCtx{ArbFieldElem, Solve.LUTrait})
  if isdefined(C, :red) && isdefined(C, :lu_perm)
    return nothing
  end

  nrows(C) != ncols(C) && error("Only implemented for square matrices")

  A = matrix(C)
  P = Perm(nrows(C))
  x = similar(A, nrows(A), ncols(A))
  lu!(P, x, A)

  C.red = x
  C.lu_perm = P
  return nothing
end

function Solve._init_reduce_transpose(C::Solve.SolveCtx{ArbFieldElem, Solve.LUTrait})
  if isdefined(C, :red_transp) && isdefined(C, :lu_perm_transp)
    return nothing
  end

  nrows(C) != ncols(C) && error("Only implemented for square matrices")

  A = transpose(matrix(C))
  P = Perm(nrows(C))
  x = similar(A, nrows(A), ncols(A))
  lu!(P, x, A)

  C.red_transp = x
  C.lu_perm_transp = P
  return nothing
end

function Solve._can_solve_internal_no_check(::Solve.LUTrait, C::Solve.SolveCtx{ArbFieldElem, Solve.LUTrait}, b::ArbMatrix, task::Symbol; side::Symbol = :left)
  if side === :right
    LU = Solve.reduced_matrix(C)
    p = Solve.lu_permutation(C)
  else
    LU = Solve.reduced_matrix_of_transpose(C)
    p = Solve.lu_permutation_of_transpose(C)
    b = transpose(b)
  end

  x = similar(b, ncols(C), ncols(b))
  _solve_lu_precomp!(x, p, LU, b)

  if side === :left
    x = transpose(x)
  end

  if task === :only_check || task === :with_solution
    return true, x, zero(b, 0, 0)
  end
  # If we ended up here, then the matrix is invertible, so the kernel is trivial
  if side === :right
    return true, x, zero(b, ncols(C), 0)
  else
    return true, x, zero(b, 0, nrows(C))
  end
end

################################################################################
#
#   Row swapping
#
################################################################################

function swap_rows!(x::ArbMatrix, i::Int, j::Int)
  @ccall libflint.arb_mat_swap_rows(x::Ref{ArbMatrix}, C_NULL::Ptr{Nothing}, (i - 1)::Int, (j - 1)::Int)::Nothing
end

################################################################################
#
#   Norm
#
################################################################################

@doc raw"""
    bound_inf_norm(x::ArbMatrix)

Returns a non-negative element $z$ of type `ArbFieldElem`, such that $z$ is an upper
bound for the infinity norm for every matrix in $x$
"""
function bound_inf_norm(x::ArbMatrix)
  z = ArbFieldElem()
  GC.@preserve x z begin
    t = _rad_ptr(z)
    @ccall libflint.arb_mat_bound_inf_norm(t::Ptr{mag_struct}, x::Ref{ArbMatrix})::Nothing
    s = _mid_ptr(z)
    @ccall libflint.arf_set_mag(s::Ptr{arf_struct}, t::Ptr{mag_struct})::Nothing
    @ccall libflint.mag_zero(t::Ptr{mag_struct})::Nothing
  end
  return base_ring(x)(z)
end

################################################################################
#
#   Unsafe functions
#
################################################################################

function zero!(z::ArbMatrixOrPtr)
  @ccall libflint.arb_mat_zero(z::Ref{ArbMatrix})::Nothing
  return z
end

function one!(z::ArbMatrixOrPtr)
  @ccall libflint.arb_mat_one(z::Ref{ArbMatrix})::Nothing
  return z
end

function neg!(z::ArbMatrixOrPtr, a::ArbMatrixOrPtr)
  @ccall libflint.arb_mat_neg(z::Ref{ArbMatrix}, a::Ref{ArbMatrix})::Nothing
  return z
end

for (s,f) in (("add!","arb_mat_add"), ("mul!","arb_mat_mul"),
              ("sub!","arb_mat_sub"))
  @eval begin
    function ($(Symbol(s)))(z::ArbMatrix, x::ArbMatrix, y::ArbMatrix)
      prec = precision(base_ring(x))
      @ccall libflint.f(z::Ref{ArbMatrix}, x::Ref{ArbMatrix}, y::Ref{ArbMatrix}, prec::Int)::Nothing
      return z
    end
  end
end

###############################################################################
#
#   Parent object call overloads
#
###############################################################################

function (x::ArbMatrixSpace)()
  z = ArbMatrix(base_ring(x), undef, nrows(x), ncols(x))
  return z
end

function (x::ArbMatrixSpace)(y::ZZMatrix)
  (ncols(x) != ncols(y) || nrows(x) != nrows(y)) &&
  error("Dimensions are wrong")
  z = ArbMatrix(y, precision(x))
  z.base_ring = x.base_ring
  return z
end

function (x::ArbMatrixSpace)(y::AbstractVecOrMat{T}) where {T <: Union{Int, UInt, ZZRingElem, QQFieldElem, Float64, BigFloat, ArbFieldElem, AbstractString}}
  _check_dim(nrows(x), ncols(x), y)
  z = ArbMatrix(nrows(x), ncols(x), y, precision(x))
  z.base_ring = x.base_ring
  return z
end

###############################################################################
#
#   Matrix constructor
#
###############################################################################

function matrix(R::ArbField, arr::AbstractMatrix{T}) where {T <: Union{Int, UInt, ZZRingElem, QQFieldElem, Float64, BigFloat, ArbFieldElem, AbstractString}}
  z = ArbMatrix(size(arr, 1), size(arr, 2), arr, precision(R))
  z.base_ring = R
  return z
end

function matrix(R::ArbField, r::Int, c::Int, arr::AbstractVector{T}) where {T <: Union{Int, UInt, ZZRingElem, QQFieldElem, Float64, BigFloat, ArbFieldElem, AbstractString}}
  _check_dim(r, c, arr)
  z = ArbMatrix(r, c, arr, precision(R))
  z.base_ring = R
  return z
end

function matrix(R::ArbField, arr::AbstractMatrix{<: Integer})
  arr_fmpz = map(ZZRingElem, arr)
  return matrix(R, arr_fmpz)
end

function matrix(R::ArbField, r::Int, c::Int, arr::AbstractVector{<: Integer})
  arr_fmpz = map(ZZRingElem, arr)
  return matrix(R, r, c, arr_fmpz)
end

function matrix(R::ArbField, arr::AbstractMatrix{Rational{T}}) where {T <: Integer}
  arr_fmpz = map(QQFieldElem, arr)
  return matrix(R, arr_fmpz)
end

function matrix(R::ArbField, r::Int, c::Int, arr::AbstractVector{Rational{T}}) where {T <: Integer}
  arr_fmpz = map(QQFieldElem, arr)
  return matrix(R, r, c, arr_fmpz)
end

###############################################################################
#
#  Identity matrix
#
###############################################################################

function identity_matrix(R::ArbField, n::Int)
  if n < 0
    error("dimension must not be negative")
  end
  return one!(ArbMatrix(R, undef, n, n))
end

################################################################################
#
#  Entry pointers
#
################################################################################

@inline mat_entry_ptr(A::ArbMatrix, i::Int, j::Int) = 
@ccall libflint.arb_mat_entry_ptr(A::Ref{ArbMatrix}, (i-1)::Int, (j-1)::Int)::Ptr{ArbFieldElem}

###############################################################################
#
#   Promotions
#
###############################################################################

promote_rule(::Type{ArbMatrix}, ::Type{T}) where {T <: Integer} = ArbMatrix

promote_rule(::Type{ArbMatrix}, ::Type{Rational{T}}) where T <: Union{Int, BigInt} = ArbMatrix

promote_rule(::Type{ArbMatrix}, ::Type{ZZRingElem}) = ArbMatrix

promote_rule(::Type{ArbMatrix}, ::Type{QQFieldElem}) = ArbMatrix

promote_rule(::Type{ArbMatrix}, ::Type{ArbFieldElem}) = ArbMatrix

promote_rule(::Type{ArbMatrix}, ::Type{Float64}) = ArbMatrix

promote_rule(::Type{ArbMatrix}, ::Type{BigFloat}) = ArbMatrix

promote_rule(::Type{ArbMatrix}, ::Type{ZZMatrix}) = ArbMatrix

promote_rule(::Type{ArbMatrix}, ::Type{QQMatrix}) = ArbMatrix
