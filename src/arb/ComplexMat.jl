###############################################################################
#
#   ComplexMat.jl : Arb matrices over AcbFieldElem
#
###############################################################################

###############################################################################
#
#   Basic manipulation
#
###############################################################################

base_ring(a::ComplexMatrix) = ComplexField()

dense_matrix_type(::Type{ComplexFieldElem}) = ComplexMatrix

is_zero_initialized(::Type{ComplexMatrix}) = true

function getindex!(z::ComplexFieldElem, x::ComplexMatrix, r::Int, c::Int)
  GC.@preserve x begin
    v = mat_entry_ptr(x, r, c)
    _acb_set(z, v)
  end
  return z
end

@inline function getindex(x::ComplexMatrix, r::Int, c::Int)
  @boundscheck _checkbounds(x, r, c)

  z = base_ring(x)()
  GC.@preserve x begin
    v = mat_entry_ptr(x, r, c)
    _acb_set(z, v)
  end
  return z
end

for T in [Integer, Float64, ZZRingElem, QQFieldElem, RealFieldElem, BigFloat, ComplexFieldElem, AbstractString]
  @eval begin
    @inline function setindex!(x::ComplexMatrix, y::$T, r::Int, c::Int)
      @boundscheck _checkbounds(x, r, c)

      GC.@preserve x begin
        z = mat_entry_ptr(x, r, c)
        _acb_set(z, y, precision(Balls))
      end
    end
  end
end

Base.@propagate_inbounds setindex!(x::ComplexMatrix, y::Rational{T},
                                   r::Int, c::Int) where {T <: Integer} =
setindex!(x, QQFieldElem(y), r, c)

for T in [Integer, Float64, ZZRingElem, QQFieldElem, RealFieldElem, BigFloat, AbstractString]
  @eval begin
    @inline function setindex!(x::ComplexMatrix, y::Tuple{$T, $T}, r::Int, c::Int)
      @boundscheck _checkbounds(x, r, c)

      GC.@preserve x begin
        z = mat_entry_ptr(x, r, c)
        _acb_set(z, y, precision(Balls))
      end
    end
  end
end

setindex!(x::ComplexMatrix, y::Tuple{Rational{T}, Rational{T}}, r::Int, c::Int) where {T <: Integer} =
setindex!(x, map(QQFieldElem, y), r, c)

function one(x::ComplexMatrixSpace)
  check_square(x)
  return one!(x())
end

number_of_rows(a::ComplexMatrix) = a.r

number_of_columns(a::ComplexMatrix) = a.c

function deepcopy_internal(x::ComplexMatrix, dict::IdDict)
  z = similar(x)
  @ccall libflint.acb_mat_set(z::Ref{ComplexMatrix}, x::Ref{ComplexMatrix})::Nothing
  return z
end

################################################################################
#
#  Unary operations
#
################################################################################

-(x::ComplexMatrix) = neg!(similar(x), x)

################################################################################
#
#  Transpose
#
################################################################################

function transpose(x::ComplexMatrix)
  z = similar(x, ncols(x), nrows(x))
  @ccall libflint.acb_mat_transpose(z::Ref{ComplexMatrix}, x::Ref{ComplexMatrix})::Nothing
  return z
end

################################################################################
#
#  Binary operations
#
################################################################################

function +(x::ComplexMatrix, y::ComplexMatrix)
  check_parent(x, y)
  z = similar(x)
  @ccall libflint.acb_mat_add(z::Ref{ComplexMatrix}, x::Ref{ComplexMatrix}, y::Ref{ComplexMatrix}, precision(Balls)::Int)::Nothing
  return z
end

function -(x::ComplexMatrix, y::ComplexMatrix)
  check_parent(x, y)
  z = similar(x)
  @ccall libflint.acb_mat_sub(z::Ref{ComplexMatrix}, x::Ref{ComplexMatrix}, y::Ref{ComplexMatrix}, precision(Balls)::Int)::Nothing
  return z
end

function *(x::ComplexMatrix, y::ComplexMatrix)
  ncols(x) != nrows(y) && error("Matrices have wrong dimensions")
  z = similar(x, nrows(x), ncols(y))
  @ccall libflint.acb_mat_mul(z::Ref{ComplexMatrix}, x::Ref{ComplexMatrix}, y::Ref{ComplexMatrix}, precision(Balls)::Int)::Nothing
  return z
end

################################################################################
#
#   Ad hoc binary operators
#
################################################################################

function ^(x::ComplexMatrix, y::UInt)
  nrows(x) != ncols(x) && error("Matrix must be square")
  z = similar(x)
  @ccall libflint.acb_mat_pow_ui(z::Ref{ComplexMatrix}, x::Ref{ComplexMatrix}, y::UInt, precision(Balls)::Int)::Nothing
  return z
end

function *(x::ComplexMatrix, y::Int)
  z = similar(x)
  @ccall libflint.acb_mat_scalar_mul_si(z::Ref{ComplexMatrix}, x::Ref{ComplexMatrix}, y::Int, precision(Balls)::Int)::Nothing
  return z
end

*(x::Int, y::ComplexMatrix) = y*x

function *(x::ComplexMatrix, y::ZZRingElem)
  z = similar(x)
  @ccall libflint.acb_mat_scalar_mul_fmpz(z::Ref{ComplexMatrix}, x::Ref{ComplexMatrix}, y::Ref{ZZRingElem}, precision(Balls)::Int)::Nothing
  return z
end

*(x::ZZRingElem, y::ComplexMatrix) = y*x

function *(x::ComplexMatrix, y::RealFieldElem)
  z = similar(x)
  @ccall libflint.acb_mat_scalar_mul_arb(z::Ref{ComplexMatrix}, x::Ref{ComplexMatrix}, y::Ref{RealFieldElem}, precision(Balls)::Int)::Nothing
  return z
end

*(x::RealFieldElem, y::ComplexMatrix) = y*x

function *(x::ComplexMatrix, y::ComplexFieldElem)
  z = similar(x)
  @ccall libflint.acb_mat_scalar_mul_acb(z::Ref{ComplexMatrix}, x::Ref{ComplexMatrix}, y::Ref{ComplexFieldElem}, precision(Balls)::Int)::Nothing
  return z
end

*(x::ComplexFieldElem, y::ComplexMatrix) = y*x

*(x::Integer, y::ComplexMatrix) = ZZRingElem(x) * y

*(x::ComplexMatrix, y::Integer) = y * x

*(x::QQFieldElem, y::ComplexMatrix) = base_ring(y)(x) * y

*(x::ComplexMatrix, y::QQFieldElem) = y * x

*(x::Float64, y::ComplexMatrix) = base_ring(y)(x) * y

*(x::ComplexMatrix, y::Float64) = y * x

*(x::BigFloat, y::ComplexMatrix) = base_ring(y)(x) * y

*(x::ComplexMatrix, y::BigFloat) = y * x

*(x::Rational{T}, y::ComplexMatrix) where T <: Union{Int, BigInt} = QQFieldElem(x) * y

*(x::ComplexMatrix, y::Rational{T}) where T <: Union{Int, BigInt} = y * x

for T in [Integer, ZZRingElem, QQFieldElem, RealFieldElem, ComplexFieldElem]
  @eval begin
    function +(x::ComplexMatrix, y::$T)
      z = deepcopy(x)
      for i = 1:min(nrows(x), ncols(x))
        z[i, i] += y
      end
      return z
    end

    +(x::$T, y::ComplexMatrix) = y + x

    function -(x::ComplexMatrix, y::$T)
      z = deepcopy(x)
      for i = 1:min(nrows(x), ncols(x))
        z[i, i] -= y
      end
      return z
    end

    function -(x::$T, y::ComplexMatrix)
      z = -y
      for i = 1:min(nrows(y), ncols(y))
        z[i, i] += x
      end
      return z
    end
  end
end

function +(x::ComplexMatrix, y::Rational{T}) where T <: Union{Int, BigInt}
  z = deepcopy(x)
  for i = 1:min(nrows(x), ncols(x))
    z[i, i] += y
  end
  return z
end

+(x::Rational{T}, y::ComplexMatrix) where T <: Union{Int, BigInt} = y + x

function -(x::ComplexMatrix, y::Rational{T}) where T <: Union{Int, BigInt}
  z = deepcopy(x)
  for i = 1:min(nrows(x), ncols(x))
    z[i, i] -= y
  end
  return z
end

function -(x::Rational{T}, y::ComplexMatrix) where T <: Union{Int, BigInt}
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

function ldexp(x::ComplexMatrix, y::Int)
  z = similar(x)
  @ccall libflint.acb_mat_scalar_mul_2exp_si(z::Ref{ComplexMatrix}, x::Ref{ComplexMatrix}, y::Int)::Nothing
  return z
end

###############################################################################
#
#   Comparisons
#
###############################################################################

@doc raw"""
    isequal(x::ComplexMatrix, y::ComplexMatrix)

Return `true` if the matrices of balls $x$ and $y$ are precisely equal,
i.e. if all matrix entries have the same midpoints and radii.
"""
function isequal(x::ComplexMatrix, y::ComplexMatrix)
  r = @ccall libflint.acb_mat_equal(x::Ref{ComplexMatrix}, y::Ref{ComplexMatrix})::Cint
  return Bool(r)
end

function ==(x::ComplexMatrix, y::ComplexMatrix)
  fl = check_parent(x, y, false)
  !fl && return false
  r = @ccall libflint.acb_mat_eq(x::Ref{ComplexMatrix}, y::Ref{ComplexMatrix})::Cint
  return Bool(r)
end

function !=(x::ComplexMatrix, y::ComplexMatrix)
  r = @ccall libflint.acb_mat_ne(x::Ref{ComplexMatrix}, y::Ref{ComplexMatrix})::Cint
  return Bool(r)
end

@doc raw"""
    overlaps(x::ComplexMatrix, y::ComplexMatrix)

Returns `true` if all entries of $x$ overlap with the corresponding entry of
$y$, otherwise return `false`.
"""
function overlaps(x::ComplexMatrix, y::ComplexMatrix)
  r = @ccall libflint.acb_mat_overlaps(x::Ref{ComplexMatrix}, y::Ref{ComplexMatrix})::Cint
  return Bool(r)
end

@doc raw"""
    contains(x::ComplexMatrix, y::ComplexMatrix)

Returns `true` if all entries of $x$ contain the corresponding entry of
$y$, otherwise return `false`.
"""
function contains(x::ComplexMatrix, y::ComplexMatrix)
  r = @ccall libflint.acb_mat_contains(x::Ref{ComplexMatrix}, y::Ref{ComplexMatrix})::Cint
  return Bool(r)
end

################################################################################
#
#  Ad hoc comparisons
#
################################################################################

@doc raw"""
    contains(x::ComplexMatrix, y::ZZMatrix)

Returns `true` if all entries of $x$ contain the corresponding entry of
$y$, otherwise return `false`.
"""
function contains(x::ComplexMatrix, y::ZZMatrix)
  r = @ccall libflint.acb_mat_contains_fmpz_mat(x::Ref{ComplexMatrix}, y::Ref{ZZMatrix})::Cint
  return Bool(r)
end

@doc raw"""
    contains(x::ComplexMatrix, y::QQMatrix)

Returns `true` if all entries of $x$ contain the corresponding entry of
$y$, otherwise return `false`.
"""
function contains(x::ComplexMatrix, y::QQMatrix)
  r = @ccall libflint.acb_mat_contains_fmpq_mat(x::Ref{ComplexMatrix}, y::Ref{QQMatrix})::Cint
  return Bool(r)
end

==(x::ComplexMatrix, y::ZZMatrix) = x == parent(x)(y)

==(x::ZZMatrix, y::ComplexMatrix) = y == x

==(x::ComplexMatrix, y::RealMatrix) = x == parent(x)(y)

==(x::RealMatrix, y::ComplexMatrix) = y == x

################################################################################
#
#  Predicates
#
################################################################################

isreal(x::ComplexMatrix) =
Bool(@ccall libflint.acb_mat_is_real(x::Ref{ComplexMatrix})::Cint)

###############################################################################
#
#   Inversion
#
###############################################################################

@doc raw"""
    inv(x::ComplexMatrix)

Given a $n\times n$ matrix of type `AcbMatrix`, return an
$n\times n$ matrix $X$ such that $AX$ contains the
identity matrix. If $A$ cannot be inverted numerically an exception is raised.
"""
function inv(x::ComplexMatrix)
  fl, z = is_invertible_with_inverse(x)
  fl && return z
  error("Matrix singular or cannot be inverted numerically")
end

function is_invertible_with_inverse(x::ComplexMatrix)
  ncols(x) != nrows(x) && return false, x
  z = similar(x)
  r = @ccall libflint.acb_mat_inv(z::Ref{ComplexMatrix}, x::Ref{ComplexMatrix}, precision(Balls)::Int)::Cint
  return Bool(r), z
end

###############################################################################
#
#   Exact division
#
###############################################################################

function divexact(x::ComplexMatrix, y::ComplexMatrix; check::Bool=true)
  ncols(x) != ncols(y) && error("Incompatible matrix dimensions")
  x*inv(y)
end

###############################################################################
#
#   Ad hoc exact division
#
###############################################################################

function divexact(x::ComplexMatrix, y::Int; check::Bool=true)
  y == 0 && throw(DivideError())
  z = similar(x)
  @ccall libflint.acb_mat_scalar_div_si(z::Ref{ComplexMatrix}, x::Ref{ComplexMatrix}, y::Int, precision(Balls)::Int)::Nothing
  return z
end

function divexact(x::ComplexMatrix, y::ZZRingElem; check::Bool=true)
  z = similar(x)
  @ccall libflint.acb_mat_scalar_div_fmpz(z::Ref{ComplexMatrix}, x::Ref{ComplexMatrix}, y::Ref{ZZRingElem}, precision(Balls)::Int)::Nothing
  return z
end

function divexact(x::ComplexMatrix, y::RealFieldElem; check::Bool=true)
  z = similar(x)
  @ccall libflint.acb_mat_scalar_div_arb(z::Ref{ComplexMatrix}, x::Ref{ComplexMatrix}, y::Ref{RealFieldElem}, precision(Balls)::Int)::Nothing
  return z
end

function divexact(x::ComplexMatrix, y::ComplexFieldElem; check::Bool=true)
  z = similar(x)
  @ccall libflint.acb_mat_scalar_div_acb(z::Ref{ComplexMatrix}, x::Ref{ComplexMatrix}, y::Ref{ComplexFieldElem}, precision(Balls)::Int)::Nothing
  return z
end

divexact(x::ComplexMatrix, y::Float64; check::Bool=true) = divexact(x, base_ring(x)(y); check=check)

divexact(x::ComplexMatrix, y::BigFloat; check::Bool=true) = divexact(x, base_ring(x)(y); check=check)

divexact(x::ComplexMatrix, y::Integer; check::Bool=true) = divexact(x, ZZRingElem(y); check=check)

divexact(x::ComplexMatrix, y::Rational{T}; check::Bool=true) where T <: Union{Int, BigInt} = divexact(x, QQFieldElem(y); check=check)

################################################################################
#
#  Characteristic polynomial
#
################################################################################

function charpoly(x::AcbPolyRing, y::ComplexMatrix, prec::Int = precision(Balls))
  base_ring(x) != base_ring(y) && error("Base rings must coincide")
  z = x()
  @ccall libflint.acb_mat_charpoly(z::Ref{AcbPolyRingElem}, y::Ref{ComplexMatrix}, prec::Int)::Nothing
  return z
end

################################################################################
#
#  Determinant
#
################################################################################

function det(x::ComplexMatrix, prec::Int = precision(Balls))
  ncols(x) != nrows(x) && error("Matrix must be square")
  z = base_ring(x)()
  @ccall libflint.acb_mat_det(z::Ref{ComplexFieldElem}, x::Ref{ComplexMatrix}, prec::Int)::Nothing
  return z
end

################################################################################
#
#  Exponential function
#
################################################################################

function Base.exp(x::ComplexMatrix)
  ncols(x) != nrows(x) && error("Matrix must be square")
  z = similar(x)
  @ccall libflint.acb_mat_exp(z::Ref{ComplexMatrix}, x::Ref{ComplexMatrix}, precision(Balls)::Int)::Nothing
  return z
end

###############################################################################
#
#   Linear solving
#
###############################################################################

function lu!(P::Perm, z::ComplexMatrix, x::ComplexMatrix)
  P.d .-= 1
  r = @ccall libflint.acb_mat_lu(P.d::Ptr{Int}, z::Ref{ComplexMatrix}, x::Ref{ComplexMatrix}, precision(Balls)::Int)::Cint
  r == 0 && error("Could not find $(nrows(x)) invertible pivot elements")
  P.d .+= 1
  inv!(P) # FLINT does PLU = x instead of Px = LU
  return min(nrows(x), ncols(x))
end

function lu!(P::Perm, x::ComplexMatrix)
  return lu!(P, x, x)
end

function _solve!(z::ComplexMatrix, x::ComplexMatrix, y::ComplexMatrix)
  r = @ccall libflint.acb_mat_solve(z::Ref{ComplexMatrix}, x::Ref{ComplexMatrix}, y::Ref{ComplexMatrix}, precision(Balls)::Int)::Cint
  r == 0 && error("Matrix cannot be inverted numerically")
  nothing
end

function _solve_lu_precomp!(z::ComplexMatrix, P::Perm, LU::ComplexMatrix, y::ComplexMatrix)
  Q = inv(P)
  @ccall libflint.acb_mat_solve_lu_precomp(z::Ref{ComplexMatrix}, (Q.d .- 1)::Ptr{Int}, LU::Ref{ComplexMatrix}, y::Ref{ComplexMatrix}, precision(Balls)::Int)::Nothing
  nothing
end

function _solve_lu_precomp(P::Perm, LU::ComplexMatrix, y::ComplexMatrix)
  ncols(LU) != nrows(y) && error("Matrix dimensions are wrong")
  z = similar(y)
  _solve_lu_precomp!(z, P, LU, y)
  return z
end

Solve.matrix_normal_form_type(::ComplexField) = Solve.LUTrait()
Solve.matrix_normal_form_type(::ComplexMatrix) = Solve.LUTrait()

function Solve._can_solve_internal_no_check(::Solve.LUTrait, A::ComplexMatrix, b::ComplexMatrix, task::Symbol; side::Symbol = :left)
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


function Solve._init_reduce(C::Solve.SolveCtx{ComplexFieldElem, Solve.LUTrait})
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

function Solve._init_reduce_transpose(C::Solve.SolveCtx{ComplexFieldElem, Solve.LUTrait})
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

function Solve._can_solve_internal_no_check(::Solve.LUTrait, C::Solve.SolveCtx{ComplexFieldElem, Solve.LUTrait}, b::ComplexMatrix, task::Symbol; side::Symbol = :left)
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

function swap_rows!(x::ComplexMatrix, i::Int, j::Int)
  @ccall libflint.acb_mat_swap_rows(x::Ref{ComplexMatrix}, C_NULL::Ptr{Nothing}, (i - 1)::Int, (j - 1)::Int)::Nothing
end

################################################################################
#
#   Norm
#
################################################################################

@doc raw"""
    bound_inf_norm(x::ComplexMatrix)

Returns a non-negative element $z$ of type `AcbFieldElem`, such that $z$ is an upper
bound for the infinity norm for every matrix in $x$
"""
function bound_inf_norm(x::ComplexMatrix)
  z = RealFieldElem()
  GC.@preserve x z begin
    t = _rad_ptr(z)
    @ccall libflint.acb_mat_bound_inf_norm(t::Ptr{mag_struct}, x::Ref{ComplexMatrix})::Nothing
    s = _mid_ptr(z)
    @ccall libflint.arf_set_mag(s::Ptr{arf_struct}, t::Ptr{mag_struct})::Nothing
    @ccall libflint.mag_zero(t::Ptr{mag_struct})::Nothing
  end
  return z
end

################################################################################
#
#   Unsafe functions
#
################################################################################

function zero!(z::ComplexMatrixOrPtr)
  @ccall libflint.acb_mat_zero(z::Ref{ComplexMatrix})::Nothing
  return z
end

function one!(z::ComplexMatrixOrPtr)
  @ccall libflint.acb_mat_one(z::Ref{ComplexMatrix})::Nothing
  return z
end

function neg!(z::ComplexMatrixOrPtr, a::ComplexMatrixOrPtr)
  @ccall libflint.acb_mat_neg(z::Ref{ComplexMatrix}, a::Ref{ComplexMatrix})::Nothing
  return z
end

for (s,f) in (("add!","acb_mat_add"), ("mul!","acb_mat_mul"),
              ("sub!","acb_mat_sub"))
  @eval begin
    function ($(Symbol(s)))(z::ComplexMatrix, x::ComplexMatrix, y::ComplexMatrix, prec::Int = precision(Balls))
      @ccall libflint.$f(z::Ref{ComplexMatrix}, x::Ref{ComplexMatrix}, y::Ref{ComplexMatrix}, prec::Int)::Nothing
      return z
    end
  end
end

###############################################################################
#
#   Parent object call overloads
#
###############################################################################

function (x::ComplexMatrixSpace)()
  z = ComplexMatrix(nrows(x), ncols(x))
  return z
end

function (x::ComplexMatrixSpace)(y::ZZMatrix)
  (ncols(x) != ncols(y) || nrows(x) != nrows(y)) &&
  error("Dimensions are wrong")
  z = ComplexMatrix(y, precision(Balls))
  return z
end

function (x::ComplexMatrixSpace)(y::RealMatrix)
  (ncols(x) != ncols(y) || nrows(x) != nrows(y)) &&
  error("Dimensions are wrong")
  z = ComplexMatrix(y, precision(Balls))
  return z
end

for T in [Float64, ZZRingElem, QQFieldElem, BigFloat, RealFieldElem, ComplexFieldElem, String]
  @eval begin
    function (x::ComplexMatrixSpace)(y::AbstractVecOrMat{$T})
      _check_dim(nrows(x), ncols(x), y)
      z = ComplexMatrix(nrows(x), ncols(x), y, precision(Balls))
      return z
    end
  end
end

(x::ComplexMatrixSpace)(y::AbstractVecOrMat{T}) where {T <: Integer} = x(map(ZZRingElem, y))

(x::ComplexMatrixSpace)(y::AbstractVecOrMat{Rational{T}}) where {T <: Integer} = x(map(QQFieldElem, y))

for T in [Float64, ZZRingElem, QQFieldElem, BigFloat, RealFieldElem, String]
  @eval begin
    function (x::ComplexMatrixSpace)(y::AbstractVecOrMat{Tuple{$T, $T}})
      _check_dim(nrows(x), ncols(x), y)
      z = ComplexMatrix(nrows(x), ncols(x), y, precision(Balls))
      return z
    end
  end
end

(x::ComplexMatrixSpace)(y::AbstractVecOrMat{Tuple{T, T}}) where {T <: Integer} =
x(map(z -> (ZZRingElem(z[1]), ZZRingElem(z[2])), y))

(x::ComplexMatrixSpace)(y::AbstractVecOrMat{Tuple{Rational{T}, Rational{T}}}) where {T <: Integer} =
x(map(z -> (QQFieldElem(z[1]), QQFieldElem(z[2])), y))

for T in [Integer, ZZRingElem, QQFieldElem, Float64, BigFloat, RealFieldElem, ComplexFieldElem, String]
  @eval begin
    function (x::ComplexMatrixSpace)(y::$T)
      z = x()
      for i in 1:nrows(z)
        for j = 1:ncols(z)
          if i != j
            z[i, j] = zero(base_ring(x))
          else
            z[i, j] = y
          end
        end
      end
      return z
    end
  end
end

(x::ComplexMatrixSpace)(y::Rational{T}) where {T <: Integer} = x(QQFieldElem(y))

###############################################################################
#
#   Matrix constructor
#
###############################################################################

function matrix(R::ComplexField, arr::AbstractMatrix{T}) where {T <: Union{Int, UInt, ZZRingElem, QQFieldElem, Float64, BigFloat, RealFieldElem, ComplexFieldElem, AbstractString}}
  z = ComplexMatrix(size(arr, 1), size(arr, 2), arr, precision(Balls))
  return z
end

function matrix(R::ComplexField, r::Int, c::Int, arr::AbstractVector{T}) where {T <: Union{Int, UInt, ZZRingElem, QQFieldElem, Float64, BigFloat, RealFieldElem, ComplexFieldElem, AbstractString}}
  _check_dim(r, c, arr)
  z = ComplexMatrix(r, c, arr, precision(Balls))
  return z
end

function matrix(R::ComplexField, arr::AbstractMatrix{<: Integer})
  arr_fmpz = map(ZZRingElem, arr)
  return matrix(R, arr_fmpz)
end

function matrix(R::ComplexField, r::Int, c::Int, arr::AbstractVector{<: Integer})
  arr_fmpz = map(ZZRingElem, arr)
  return matrix(R, r, c, arr_fmpz)
end

function matrix(R::ComplexField, arr::AbstractMatrix{Rational{T}}) where {T <: Integer}
  arr_fmpz = map(QQFieldElem, arr)
  return matrix(R, arr_fmpz)
end

function matrix(R::ComplexField, r::Int, c::Int, arr::AbstractVector{Rational{T}}) where {T <: Integer}
  arr_fmpz = map(QQFieldElem, arr)
  return matrix(R, r, c, arr_fmpz)
end

###############################################################################
#
#  Identity matrix
#
###############################################################################

function identity_matrix(R::ComplexField, n::Int)
  if n < 0
    error("dimension must not be negative")
  end
  return one!(ComplexMatrix(n, n))
end

################################################################################
#
#  Entry pointers
#
################################################################################

@inline mat_entry_ptr(A::ComplexMatrix, i::Int, j::Int) = 
@ccall libflint.acb_mat_entry_ptr(A::Ref{ComplexMatrix}, (i-1)::Int, (j-1)::Int)::Ptr{ComplexFieldElem}



###############################################################################
#
#   Promotions
#
###############################################################################

promote_rule(::Type{ComplexMatrix}, ::Type{T}) where {T <: Integer} = ComplexMatrix

promote_rule(::Type{ComplexMatrix}, ::Type{Rational{T}}) where T <: Union{Int, BigInt} = ComplexMatrix

promote_rule(::Type{ComplexMatrix}, ::Type{ZZRingElem}) = ComplexMatrix

promote_rule(::Type{ComplexMatrix}, ::Type{QQFieldElem}) = ComplexMatrix

promote_rule(::Type{ComplexMatrix}, ::Type{ArbFieldElem}) = ComplexMatrix

promote_rule(::Type{ComplexMatrix}, ::Type{ComplexFieldElem}) = ComplexMatrix

promote_rule(::Type{ComplexMatrix}, ::Type{ZZMatrix}) = ComplexMatrix

promote_rule(::Type{ComplexMatrix}, ::Type{QQMatrix}) = ComplexMatrix

promote_rule(::Type{ComplexMatrix}, ::Type{ArbMatrix}) = ComplexMatrix

###############################################################################
#
#   Eigenvalues and eigenvectors
#
###############################################################################

function __approx_eig_qr!(v::Ptr{acb_struct}, R::ComplexMatrix, A::ComplexMatrix)
  n = nrows(A)
  @ccall libflint.acb_mat_approx_eig_qr(v::Ptr{acb_struct}, C_NULL::Ptr{Nothing}, R::Ref{ComplexMatrix}, A::Ref{ComplexMatrix}, C_NULL::Ptr{Nothing}, 0::Int, precision(Balls)::Int)::Cint
  return nothing
end

function _approx_eig_qr(A::ComplexMatrix)
  n = nrows(A)
  v = acb_vec(n)
  R = zero_matrix(base_ring(A), ncols(A), nrows(A))
  __approx_eig_qr!(v, R, A)
  z = array(base_ring(A), v, n)
  acb_vec_clear(v, n)
  return z, R
end

function _eig_multiple(A::ComplexMatrix, check::Bool = true)
  n = nrows(A)
  v = acb_vec(n)
  v_approx = acb_vec(n)
  R = zero_matrix(base_ring(A), n, n)
  __approx_eig_qr!(v, R, A)
  b = @ccall libflint.acb_mat_eig_multiple(v_approx::Ptr{acb_struct}, A::Ref{ComplexMatrix}, v::Ptr{acb_struct}, R::Ref{ComplexMatrix}, precision(Balls)::Int)::Cint
  check && b == 0 && error("Could not isolate eigenvalues of matrix $A")
  z = array(base_ring(A), v, n)
  acb_vec_clear(v, n)
  acb_vec_clear(v_approx, n)
  res = Vector{Tuple{ComplexFieldElem, Int}}()
  k = 1
  for i in 1:n
    if i < n && isequal(z[i], z[i + 1])
      k = k + 1
      if i == n - 1
        push!(res, (z[i], k))
        break
      end
    else
      push!(res, (z[i], k))
      k = 1
    end
  end

  return res, R
end

function _eig_simple(A::ComplexMatrix; check::Bool = true, algorithm::Symbol = :default)
  n = nrows(A)
  v = acb_vec(n)
  v_approx = acb_vec(n)
  Rapprox = zero_matrix(base_ring(A), n, n)
  L = zero_matrix(base_ring(A), n, n)
  R = zero_matrix(base_ring(A), n, n)
  __approx_eig_qr!(v, Rapprox, A)
  if algorithm == :vdhoeven_mourrain
    b = @ccall libflint.acb_mat_eig_simple_vdhoeven_mourrain(v_approx::Ptr{acb_struct}, L::Ref{ComplexMatrix}, R::Ref{ComplexMatrix}, A::Ref{ComplexMatrix}, v::Ptr{acb_struct}, Rapprox::Ref{ComplexMatrix}, precision(Balls)::Int)::Cint
  elseif algorithm == :rump
    b = @ccall libflint.acb_mat_eig_simple_rump(v_approx::Ptr{acb_struct}, L::Ref{ComplexMatrix}, R::Ref{ComplexMatrix}, A::Ref{ComplexMatrix}, v::Ptr{acb_struct}, Rapprox::Ref{ComplexMatrix}, precision(Balls)::Int)::Cint
  elseif algorithm == :default
    b = @ccall libflint.acb_mat_eig_simple(v_approx::Ptr{acb_struct}, L::Ref{ComplexMatrix}, R::Ref{ComplexMatrix}, A::Ref{ComplexMatrix}, v::Ptr{acb_struct}, Rapprox::Ref{ComplexMatrix}, precision(Balls)::Int)::Cint
  else
    error("Algorithm $algorithm not supported")
  end

  if check && b == 0
    if nrows(A) <= 10
      error("Could not isolate eigenvalues of matrix $A")
    else
      error("Could not isolate eigenvalues")
    end
  end
  z = array(base_ring(A), v, n)
  acb_vec_clear(v, n)
  acb_vec_clear(v_approx, n)

  return z, L, R
end

@doc raw"""
    eigenvalues_simple(A::ComplexMatrix, algorithm::Symbol = :default)

Returns the eigenvalues of `A` as a vector of `AcbFieldElem`. It is assumed that `A`
has only simple eigenvalues.

The algorithm used can be changed by setting the `algorithm` keyword to
`:vdhoeven_mourrain` or `:rump`.

This function is experimental.
"""
function eigenvalues_simple(A::ComplexMatrix, algorithm::Symbol = :default)
  E, _, _ = _eig_simple(A, algorithm = algorithm)
  return E
end

@doc raw"""
    eigenvalues_with_multiplicities(A::ComplexMatrix)

Return the eigenvalues of `A` with their algebraic multiplicities as a vector of
tuples `(ComplexFieldElem, Int)`. Each tuple `(z, k)` corresponds to a cluster
of `k` eigenvalues of $A$.

This function is experimental.
"""
function eigenvalues_with_multiplicities(A::ComplexMatrix)
  e, _ = _eig_multiple(A)
  return e
end

@doc raw"""
    eigenvalues(A::ComplexMatrix)

Return the eigenvalues of `A`.

This function is experimental.
"""
function eigenvalues(A::ComplexMatrix)
  e, _ = _eig_multiple(A)
  return [ x[1] for x in e ]
end
