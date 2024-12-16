###############################################################################
#
#   acb_mat.jl : Arb matrices over AcbFieldElem
#
###############################################################################

###############################################################################
#
#   Similar & zero
#
###############################################################################

function similar(::AcbMatrix, R::AcbField, r::Int, c::Int)
  z = AcbMatrix(R, undef, r, c)
  return z
end

zero(m::AcbMatrix, R::AcbField, r::Int, c::Int) = similar(m, R, r, c)

###############################################################################
#
#   Basic manipulation
#
###############################################################################

base_ring(a::AcbMatrix) = a.base_ring

dense_matrix_type(::Type{AcbFieldElem}) = AcbMatrix

precision(x::AcbMatrixSpace) = precision(base_ring(x))

function getindex!(z::AcbFieldElem, x::AcbMatrix, r::Int, c::Int)
  GC.@preserve x begin
    v = mat_entry_ptr(x, r, c)
    _acb_set(z, v)
  end
  return z
end

@inline function getindex(x::AcbMatrix, r::Int, c::Int)
  @boundscheck _checkbounds(x, r, c)

  z = base_ring(x)()
  GC.@preserve x begin
    v = mat_entry_ptr(x, r, c)
    _acb_set(z, v)
  end
  return z
end

for T in [Integer, Float64, ZZRingElem, QQFieldElem, ArbFieldElem, BigFloat, AcbFieldElem, AbstractString]
  @eval begin
    @inline function setindex!(x::AcbMatrix, y::$T, r::Int, c::Int)
      @boundscheck _checkbounds(x, r, c)

      GC.@preserve x begin
        z = mat_entry_ptr(x, r, c)
        _acb_set(z, y, precision(base_ring(x)))
      end
    end
  end
end

Base.@propagate_inbounds setindex!(x::AcbMatrix, y::Rational{T},
                                   r::Int, c::Int) where {T <: Integer} =
setindex!(x, QQFieldElem(y), r, c)

for T in [Integer, Float64, ZZRingElem, QQFieldElem, ArbFieldElem, BigFloat, AbstractString]
  @eval begin
    @inline function setindex!(x::AcbMatrix, y::Tuple{$T, $T}, r::Int, c::Int)
      @boundscheck _checkbounds(x, r, c)

      GC.@preserve x begin
        z = mat_entry_ptr(x, r, c)
        _acb_set(z, y, precision(base_ring(x)))
      end
    end
  end
end

setindex!(x::AcbMatrix, y::Tuple{Rational{T}, Rational{T}}, r::Int, c::Int) where {T <: Integer} =
setindex!(x, map(QQFieldElem, y), r, c)

function one(x::AcbMatrixSpace)
  return one!(x())
end

number_of_rows(a::AcbMatrix) = a.r

number_of_columns(a::AcbMatrix) = a.c

function deepcopy_internal(x::AcbMatrix, dict::IdDict)
  z = similar(x)
  @ccall libflint.acb_mat_set(z::Ref{AcbMatrix}, x::Ref{AcbMatrix})::Nothing
  return z
end

################################################################################
#
#  Unary operations
#
################################################################################

-(x::AcbMatrix)= neg!(similar(x), x)

################################################################################
#
#  Transpose
#
################################################################################

function transpose(x::AcbMatrix)
  z = similar(x, ncols(x), nrows(x))
  @ccall libflint.acb_mat_transpose(z::Ref{AcbMatrix}, x::Ref{AcbMatrix})::Nothing
  return z
end

################################################################################
#
#  Binary operations
#
################################################################################

function +(x::AcbMatrix, y::AcbMatrix)
  check_parent(x, y)
  z = similar(x)
  @ccall libflint.acb_mat_add(z::Ref{AcbMatrix}, x::Ref{AcbMatrix}, y::Ref{AcbMatrix}, precision(base_ring(x))::Int)::Nothing
  return z
end

function -(x::AcbMatrix, y::AcbMatrix)
  check_parent(x, y)
  z = similar(x)
  @ccall libflint.acb_mat_sub(z::Ref{AcbMatrix}, x::Ref{AcbMatrix}, y::Ref{AcbMatrix}, precision(base_ring(x))::Int)::Nothing
  return z
end

function *(x::AcbMatrix, y::AcbMatrix)
  ncols(x) != nrows(y) && error("Matrices have wrong dimensions")
  z = similar(x, nrows(x), ncols(y))
  @ccall libflint.acb_mat_mul(z::Ref{AcbMatrix}, x::Ref{AcbMatrix}, y::Ref{AcbMatrix}, precision(base_ring(x))::Int)::Nothing
  return z
end

################################################################################
#
#   Ad hoc binary operators
#
################################################################################

function ^(x::AcbMatrix, y::UInt)
  nrows(x) != ncols(x) && error("Matrix must be square")
  z = similar(x)
  @ccall libflint.acb_mat_pow_ui(z::Ref{AcbMatrix}, x::Ref{AcbMatrix}, y::UInt, precision(base_ring(x))::Int)::Nothing
  return z
end

function *(x::AcbMatrix, y::Int)
  z = similar(x)
  @ccall libflint.acb_mat_scalar_mul_si(z::Ref{AcbMatrix}, x::Ref{AcbMatrix}, y::Int, precision(base_ring(x))::Int)::Nothing
  return z
end

*(x::Int, y::AcbMatrix) = y*x

function *(x::AcbMatrix, y::ZZRingElem)
  z = similar(x)
  @ccall libflint.acb_mat_scalar_mul_fmpz(z::Ref{AcbMatrix}, x::Ref{AcbMatrix}, y::Ref{ZZRingElem}, precision(base_ring(x))::Int)::Nothing
  return z
end

*(x::ZZRingElem, y::AcbMatrix) = y*x

function *(x::AcbMatrix, y::ArbFieldElem)
  z = similar(x)
  @ccall libflint.acb_mat_scalar_mul_arb(z::Ref{AcbMatrix}, x::Ref{AcbMatrix}, y::Ref{ArbFieldElem}, precision(base_ring(x))::Int)::Nothing
  return z
end

*(x::ArbFieldElem, y::AcbMatrix) = y*x

function *(x::AcbMatrix, y::AcbFieldElem)
  z = similar(x)
  @ccall libflint.acb_mat_scalar_mul_acb(z::Ref{AcbMatrix}, x::Ref{AcbMatrix}, y::Ref{AcbFieldElem}, precision(base_ring(x))::Int)::Nothing
  return z
end

*(x::AcbFieldElem, y::AcbMatrix) = y*x

*(x::Integer, y::AcbMatrix) = ZZRingElem(x) * y

*(x::AcbMatrix, y::Integer) = y * x

*(x::QQFieldElem, y::AcbMatrix) = base_ring(y)(x) * y

*(x::AcbMatrix, y::QQFieldElem) = y * x

*(x::Float64, y::AcbMatrix) = base_ring(y)(x) * y

*(x::AcbMatrix, y::Float64) = y * x

*(x::BigFloat, y::AcbMatrix) = base_ring(y)(x) * y

*(x::AcbMatrix, y::BigFloat) = y * x

*(x::Rational{T}, y::AcbMatrix) where T <: Union{Int, BigInt} = QQFieldElem(x) * y

*(x::AcbMatrix, y::Rational{T}) where T <: Union{Int, BigInt} = y * x

for T in [Integer, ZZRingElem, QQFieldElem, ArbFieldElem, AcbFieldElem]
  @eval begin
    function +(x::AcbMatrix, y::$T)
      z = deepcopy(x)
      for i = 1:min(nrows(x), ncols(x))
        z[i, i] += y
      end
      return z
    end

    +(x::$T, y::AcbMatrix) = y + x

    function -(x::AcbMatrix, y::$T)
      z = deepcopy(x)
      for i = 1:min(nrows(x), ncols(x))
        z[i, i] -= y
      end
      return z
    end

    function -(x::$T, y::AcbMatrix)
      z = -y
      for i = 1:min(nrows(y), ncols(y))
        z[i, i] += x
      end
      return z
    end
  end
end

function +(x::AcbMatrix, y::Rational{T}) where T <: Union{Int, BigInt}
  z = deepcopy(x)
  for i = 1:min(nrows(x), ncols(x))
    z[i, i] += y
  end
  return z
end

+(x::Rational{T}, y::AcbMatrix) where T <: Union{Int, BigInt} = y + x

function -(x::AcbMatrix, y::Rational{T}) where T <: Union{Int, BigInt}
  z = deepcopy(x)
  for i = 1:min(nrows(x), ncols(x))
    z[i, i] -= y
  end
  return z
end

function -(x::Rational{T}, y::AcbMatrix) where T <: Union{Int, BigInt}
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

function ldexp(x::AcbMatrix, y::Int)
  z = similar(x)
  @ccall libflint.acb_mat_scalar_mul_2exp_si(z::Ref{AcbMatrix}, x::Ref{AcbMatrix}, y::Int)::Nothing
  return z
end

###############################################################################
#
#   Comparisons
#
###############################################################################

@doc raw"""
    isequal(x::AcbMatrix, y::AcbMatrix)

Return `true` if the matrices of balls $x$ and $y$ are precisely equal,
i.e. if all matrix entries have the same midpoints and radii.
"""
function isequal(x::AcbMatrix, y::AcbMatrix)
  r = @ccall libflint.acb_mat_equal(x::Ref{AcbMatrix}, y::Ref{AcbMatrix})::Cint
  return Bool(r)
end

function ==(x::AcbMatrix, y::AcbMatrix)
  fl = check_parent(x, y, false)
  !fl && return false
  r = @ccall libflint.acb_mat_eq(x::Ref{AcbMatrix}, y::Ref{AcbMatrix})::Cint
  return Bool(r)
end

function !=(x::AcbMatrix, y::AcbMatrix)
  r = @ccall libflint.acb_mat_ne(x::Ref{AcbMatrix}, y::Ref{AcbMatrix})::Cint
  return Bool(r)
end

@doc raw"""
    overlaps(x::AcbMatrix, y::AcbMatrix)

Returns `true` if all entries of $x$ overlap with the corresponding entry of
$y$, otherwise return `false`.
"""
function overlaps(x::AcbMatrix, y::AcbMatrix)
  r = @ccall libflint.acb_mat_overlaps(x::Ref{AcbMatrix}, y::Ref{AcbMatrix})::Cint
  return Bool(r)
end

@doc raw"""
    contains(x::AcbMatrix, y::AcbMatrix)

Returns `true` if all entries of $x$ contain the corresponding entry of
$y$, otherwise return `false`.
"""
function contains(x::AcbMatrix, y::AcbMatrix)
  r = @ccall libflint.acb_mat_contains(x::Ref{AcbMatrix}, y::Ref{AcbMatrix})::Cint
  return Bool(r)
end

################################################################################
#
#  Ad hoc comparisons
#
################################################################################

@doc raw"""
    contains(x::AcbMatrix, y::ZZMatrix)

Returns `true` if all entries of $x$ contain the corresponding entry of
$y$, otherwise return `false`.
"""
function contains(x::AcbMatrix, y::ZZMatrix)
  r = @ccall libflint.acb_mat_contains_fmpz_mat(x::Ref{AcbMatrix}, y::Ref{ZZMatrix})::Cint
  return Bool(r)
end

@doc raw"""
    contains(x::AcbMatrix, y::QQMatrix)

Returns `true` if all entries of $x$ contain the corresponding entry of
$y$, otherwise return `false`.
"""
function contains(x::AcbMatrix, y::QQMatrix)
  r = @ccall libflint.acb_mat_contains_fmpq_mat(x::Ref{AcbMatrix}, y::Ref{QQMatrix})::Cint
  return Bool(r)
end

==(x::AcbMatrix, y::ZZMatrix) = x == parent(x)(y)

==(x::ZZMatrix, y::AcbMatrix) = y == x

==(x::AcbMatrix, y::ArbMatrix) = x == parent(x)(y)

==(x::ArbMatrix, y::AcbMatrix) = y == x

################################################################################
#
#  Predicates
#
################################################################################

isreal(x::AcbMatrix) =
Bool(@ccall libflint.acb_mat_is_real(x::Ref{AcbMatrix})::Cint)

###############################################################################
#
#   Inversion
#
###############################################################################

@doc raw"""
    inv(x::AcbMatrix)

Given a $n\times n$ matrix of type `AcbMatrix`, return an
$n\times n$ matrix $X$ such that $AX$ contains the
identity matrix. If $A$ cannot be inverted numerically an exception is raised.
"""
function inv(x::AcbMatrix)
  fl, z = is_invertible_with_inverse(x)
  fl && return z
  error("Matrix singular or cannot be inverted numerically")
end

function is_invertible_with_inverse(x::AcbMatrix)
  ncols(x) != nrows(x) && return false, x
  z = similar(x)
  r = @ccall libflint.acb_mat_inv(z::Ref{AcbMatrix}, x::Ref{AcbMatrix}, precision(base_ring(x))::Int)::Cint
  return Bool(r), z
end

###############################################################################
#
#   Exact division
#
###############################################################################

function divexact(x::AcbMatrix, y::AcbMatrix; check::Bool=true)
  ncols(x) != ncols(y) && error("Incompatible matrix dimensions")
  x*inv(y)
end

###############################################################################
#
#   Ad hoc exact division
#
###############################################################################

function divexact(x::AcbMatrix, y::Int; check::Bool=true)
  y == 0 && throw(DivideError())
  z = similar(x)
  @ccall libflint.acb_mat_scalar_div_si(z::Ref{AcbMatrix}, x::Ref{AcbMatrix}, y::Int, precision(base_ring(x))::Int)::Nothing
  return z
end

function divexact(x::AcbMatrix, y::ZZRingElem; check::Bool=true)
  z = similar(x)
  @ccall libflint.acb_mat_scalar_div_fmpz(z::Ref{AcbMatrix}, x::Ref{AcbMatrix}, y::Ref{ZZRingElem}, precision(base_ring(x))::Int)::Nothing
  return z
end

function divexact(x::AcbMatrix, y::ArbFieldElem; check::Bool=true)
  z = similar(x)
  @ccall libflint.acb_mat_scalar_div_arb(z::Ref{AcbMatrix}, x::Ref{AcbMatrix}, y::Ref{ArbFieldElem}, precision(base_ring(x))::Int)::Nothing
  return z
end

function divexact(x::AcbMatrix, y::AcbFieldElem; check::Bool=true)
  z = similar(x)
  @ccall libflint.acb_mat_scalar_div_acb(z::Ref{AcbMatrix}, x::Ref{AcbMatrix}, y::Ref{AcbFieldElem}, precision(base_ring(x))::Int)::Nothing
  return z
end

divexact(x::AcbMatrix, y::Float64; check::Bool=true) = divexact(x, base_ring(x)(y); check=check)

divexact(x::AcbMatrix, y::BigFloat; check::Bool=true) = divexact(x, base_ring(x)(y); check=check)

divexact(x::AcbMatrix, y::Integer; check::Bool=true) = divexact(x, ZZRingElem(y); check=check)

divexact(x::AcbMatrix, y::Rational{T}; check::Bool=true) where T <: Union{Int, BigInt} = divexact(x, QQFieldElem(y); check=check)

################################################################################
#
#  Characteristic polynomial
#
################################################################################

function charpoly(x::AcbPolyRing, y::AcbMatrix)
  base_ring(x) != base_ring(y) && error("Base rings must coincide")
  z = x()
  @ccall libflint.acb_mat_charpoly(z::Ref{AcbPolyRingElem}, y::Ref{AcbMatrix}, precision(base_ring(y))::Int)::Nothing
  return z
end

################################################################################
#
#  Determinant
#
################################################################################

function det(x::AcbMatrix)
  ncols(x) != nrows(x) && error("Matrix must be square")
  z = base_ring(x)()
  @ccall libflint.acb_mat_det(z::Ref{AcbFieldElem}, x::Ref{AcbMatrix}, precision(base_ring(x))::Int)::Nothing
  return z
end

################################################################################
#
#  Exponential function
#
################################################################################

function Base.exp(x::AcbMatrix)
  ncols(x) != nrows(x) && error("Matrix must be square")
  z = similar(x)
  @ccall libflint.acb_mat_exp(z::Ref{AcbMatrix}, x::Ref{AcbMatrix}, precision(base_ring(x))::Int)::Nothing
  return z
end

###############################################################################
#
#   Linear solving
#
###############################################################################

function lu!(P::Perm, z::AcbMatrix, x::AcbMatrix)
  P.d .-= 1
  r = @ccall libflint.acb_mat_lu(P.d::Ptr{Int}, z::Ref{AcbMatrix}, x::Ref{AcbMatrix}, precision(base_ring(x))::Int)::Cint
  r == 0 && error("Could not find $(nrows(x)) invertible pivot elements")
  P.d .+= 1
  inv!(P)
  return nrows(x)
end

function lu!(P::Perm, x::AcbMatrix)
  return lu!(P, x, x)
end

function _solve!(z::AcbMatrix, x::AcbMatrix, y::AcbMatrix)
  r = @ccall libflint.acb_mat_solve(z::Ref{AcbMatrix}, x::Ref{AcbMatrix}, y::Ref{AcbMatrix}, precision(base_ring(x))::Int)::Cint
  r == 0 && error("Matrix cannot be inverted numerically")
  nothing
end

function _solve_lu_precomp!(z::AcbMatrix, P::Perm, LU::AcbMatrix, y::AcbMatrix)
  Q = inv(P)
  @ccall libflint.acb_mat_solve_lu_precomp(z::Ref{AcbMatrix}, (Q.d .- 1)::Ptr{Int}, LU::Ref{AcbMatrix}, y::Ref{AcbMatrix}, precision(base_ring(LU))::Int)::Nothing
  nothing
end

function _solve_lu_precomp(P::Perm, LU::AcbMatrix, y::AcbMatrix)
  ncols(LU) != nrows(y) && error("Matrix dimensions are wrong")
  z = similar(y)
  _solve_lu_precomp!(z, P, LU, y)
  return z
end

Solve.matrix_normal_form_type(::AcbField) = Solve.LUTrait()
Solve.matrix_normal_form_type(::AcbMatrix) = Solve.LUTrait()

function Solve._can_solve_internal_no_check(::Solve.LUTrait, A::AcbMatrix, b::AcbMatrix, task::Symbol; side::Symbol = :left)
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

function Solve._init_reduce(C::Solve.SolveCtx{AcbFieldElem, Solve.LUTrait})
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

function Solve._init_reduce_transpose(C::Solve.SolveCtx{AcbFieldElem, Solve.LUTrait})
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

function Solve._can_solve_internal_no_check(::Solve.LUTrait, C::Solve.SolveCtx{AcbFieldElem, Solve.LUTrait}, b::AcbMatrix, task::Symbol; side::Symbol = :left)
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

function swap_rows!(x::AcbMatrix, i::Int, j::Int)
  @ccall libflint.acb_mat_swap_rows(x::Ref{AcbMatrix}, C_NULL::Ptr{Nothing}, (i - 1)::Int, (j - 1)::Int)::Nothing
end

################################################################################
#
#   Norm
#
################################################################################

@doc raw"""
    bound_inf_norm(x::AcbMatrix)

Returns a non-negative element $z$ of type `AcbFieldElem`, such that $z$ is an upper
bound for the infinity norm for every matrix in $x$
"""
function bound_inf_norm(x::AcbMatrix)
  z = ArbFieldElem()
  GC.@preserve x z begin
    t = _rad_ptr(z)
    @ccall libflint.acb_mat_bound_inf_norm(t::Ptr{mag_struct}, x::Ref{AcbMatrix})::Nothing
    s = _mid_ptr(z)
    @ccall libflint.arf_set_mag(s::Ptr{arf_struct}, t::Ptr{mag_struct})::Nothing
    @ccall libflint.mag_zero(t::Ptr{mag_struct})::Nothing
  end
  return ArbField(precision(base_ring(x)))(z)
end

################################################################################
#
#   Unsafe functions
#
################################################################################

function zero!(z::AcbMatrixOrPtr)
  @ccall libflint.acb_mat_zero(z::Ref{AcbMatrix})::Nothing
  return z
end

function one!(z::AcbMatrixOrPtr)
  @ccall libflint.acb_mat_one(z::Ref{AcbMatrix})::Nothing
  return z
end

function neg!(z::AcbMatrix, a::AcbMatrixOrPtr)
  @ccall libflint.acb_mat_neg(z::Ref{AcbMatrix}, a::Ref{AcbMatrix})::Nothing
  return z
end

for (s,f) in (("add!","acb_mat_add"), ("mul!","acb_mat_mul"),
              ("sub!","acb_mat_sub"))
  @eval begin
    function ($(Symbol(s)))(z::AcbMatrix, x::AcbMatrix, y::AcbMatrix)
      ccall(($f, libflint), Nothing,
            (Ref{AcbMatrix}, Ref{AcbMatrix}, Ref{AcbMatrix}, Int),
            z, x, y, precision(base_ring(x)))
      return z
    end
  end
end

###############################################################################
#
#   Parent object call overloads
#
###############################################################################

function (x::AcbMatrixSpace)()
  z = AcbMatrix(base_ring(x), undef, nrows(x), ncols(x))
  return z
end

function (x::AcbMatrixSpace)(y::ZZMatrix)
  (ncols(x) != ncols(y) || nrows(x) != nrows(y)) &&
  error("Dimensions are wrong")
  z = AcbMatrix(y, precision(x))
  z.base_ring = x.base_ring
  return z
end

function (x::AcbMatrixSpace)(y::ArbMatrix)
  (ncols(x) != ncols(y) || nrows(x) != nrows(y)) &&
  error("Dimensions are wrong")
  z = AcbMatrix(y, precision(x))
  z.base_ring = x.base_ring
  return z
end

for T in [Float64, ZZRingElem, QQFieldElem, BigFloat, ArbFieldElem, AcbFieldElem, String]
  @eval begin
    function (x::AcbMatrixSpace)(y::AbstractMatrix{$T})
      _check_dim(nrows(x), ncols(x), y)
      z = AcbMatrix(nrows(x), ncols(x), y, precision(x))
      z.base_ring = x.base_ring
      return z
    end

    function (x::AcbMatrixSpace)(y::AbstractVector{$T})
      _check_dim(nrows(x), ncols(x), y)
      z = AcbMatrix(nrows(x), ncols(x), y, precision(x))
      z.base_ring = x.base_ring
      return z
    end
  end
end

(x::AcbMatrixSpace)(y::AbstractMatrix{T}) where {T <: Integer} = x(map(ZZRingElem, y))

(x::AcbMatrixSpace)(y::AbstractVector{T}) where {T <: Integer} = x(map(ZZRingElem, y))

(x::AcbMatrixSpace)(y::AbstractMatrix{Rational{T}}) where {T <: Integer} = x(map(QQFieldElem, y))

(x::AcbMatrixSpace)(y::AbstractVector{Rational{T}}) where {T <: Integer} = x(map(QQFieldElem, y))

for T in [Float64, ZZRingElem, QQFieldElem, BigFloat, ArbFieldElem, String]
  @eval begin
    function (x::AcbMatrixSpace)(y::AbstractMatrix{Tuple{$T, $T}})
      _check_dim(nrows(x), ncols(x), y)
      z = AcbMatrix(nrows(x), ncols(x), y, precision(x))
      z.base_ring = x.base_ring
      return z
    end

    function (x::AcbMatrixSpace)(y::AbstractVector{Tuple{$T, $T}})
      _check_dim(nrows(x), ncols(x), y)
      z = AcbMatrix(nrows(x), ncols(x), y, precision(x))
      z.base_ring = x.base_ring
      return z
    end
  end
end

(x::AcbMatrixSpace)(y::AbstractMatrix{Tuple{T, T}}) where {T <: Integer} =
x(map(z -> (ZZRingElem(z[1]), ZZRingElem(z[2])), y))

(x::AcbMatrixSpace)(y::AbstractVector{Tuple{T, T}}) where {T <: Integer} =
x(map(z -> (ZZRingElem(z[1]), ZZRingElem(z[2])), y))

(x::AcbMatrixSpace)(y::AbstractMatrix{Tuple{Rational{T}, Rational{T}}}) where {T <: Integer} =
x(map(z -> (QQFieldElem(z[1]), QQFieldElem(z[2])), y))

(x::AcbMatrixSpace)(y::AbstractVector{Tuple{Rational{T}, Rational{T}}}) where {T <: Integer} =
x(map(z -> (QQFieldElem(z[1]), QQFieldElem(z[2])), y))

for T in [Integer, ZZRingElem, QQFieldElem, Float64, BigFloat, ArbFieldElem, AcbFieldElem, String]
  @eval begin
    function (x::AcbMatrixSpace)(y::$T)
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

(x::AcbMatrixSpace)(y::Rational{T}) where {T <: Integer} = x(QQFieldElem(y))

###############################################################################
#
#   Matrix constructor
#
###############################################################################

function matrix(R::AcbField, arr::AbstractMatrix{T}) where {T <: Union{Int, UInt, ZZRingElem, QQFieldElem, Float64, BigFloat, ArbFieldElem, AcbFieldElem, AbstractString}}
  z = AcbMatrix(size(arr, 1), size(arr, 2), arr, precision(R))
  z.base_ring = R
  return z
end

function matrix(R::AcbField, r::Int, c::Int, arr::AbstractVector{T}) where {T <: Union{Int, UInt, ZZRingElem, QQFieldElem, Float64, BigFloat, ArbFieldElem, AcbFieldElem, AbstractString}}
  _check_dim(r, c, arr)
  z = AcbMatrix(r, c, arr, precision(R))
  z.base_ring = R
  return z
end

function matrix(R::AcbField, arr::AbstractMatrix{<: Integer})
  arr_fmpz = map(ZZRingElem, arr)
  return matrix(R, arr_fmpz)
end

function matrix(R::AcbField, r::Int, c::Int, arr::AbstractVector{<: Integer})
  arr_fmpz = map(ZZRingElem, arr)
  return matrix(R, r, c, arr_fmpz)
end

function matrix(R::AcbField, arr::AbstractMatrix{Rational{T}}) where {T <: Integer}
  arr_fmpz = map(QQFieldElem, arr)
  return matrix(R, arr_fmpz)
end

function matrix(R::AcbField, r::Int, c::Int, arr::AbstractVector{Rational{T}}) where {T <: Integer}
  arr_fmpz = map(QQFieldElem, arr)
  return matrix(R, r, c, arr_fmpz)
end

###############################################################################
#
#  Zero matrix
#
###############################################################################

function zero_matrix(R::AcbField, r::Int, c::Int)
  if r < 0 || c < 0
    error("dimensions must not be negative")
  end
  z = AcbMatrix(R, undef, r, c)
  return z
end

###############################################################################
#
#  Identity matrix
#
###############################################################################

function identity_matrix(R::AcbField, n::Int)
  if n < 0
    error("dimension must not be negative")
  end
  return one!(AcbMatrix(R, undef, n, n))
end

################################################################################
#
#  Entry pointers
#
################################################################################

@inline mat_entry_ptr(A::AcbMatrix, i::Int, j::Int) = 
@ccall libflint.acb_mat_entry_ptr(A::Ref{AcbMatrix}, (i-1)::Int, (j-1)::Int)::Ptr{AcbFieldElem}

###############################################################################
#
#   Promotions
#
###############################################################################

promote_rule(::Type{AcbMatrix}, ::Type{T}) where {T <: Integer} = AcbMatrix

promote_rule(::Type{AcbMatrix}, ::Type{Rational{T}}) where T <: Union{Int, BigInt} = AcbMatrix

promote_rule(::Type{AcbMatrix}, ::Type{ZZRingElem}) = AcbMatrix

promote_rule(::Type{AcbMatrix}, ::Type{QQFieldElem}) = AcbMatrix

promote_rule(::Type{AcbMatrix}, ::Type{ArbFieldElem}) = AcbMatrix

promote_rule(::Type{AcbMatrix}, ::Type{AcbFieldElem}) = AcbMatrix

promote_rule(::Type{AcbMatrix}, ::Type{ZZMatrix}) = AcbMatrix

promote_rule(::Type{AcbMatrix}, ::Type{QQMatrix}) = AcbMatrix

promote_rule(::Type{AcbMatrix}, ::Type{ArbMatrix}) = AcbMatrix

###############################################################################
#
#   Eigenvalues and eigenvectors
#
###############################################################################

function __approx_eig_qr!(v::Ptr{acb_struct}, R::AcbMatrix, A::AcbMatrix)
  n = nrows(A)
  @ccall libflint.acb_mat_approx_eig_qr(v::Ptr{acb_struct}, C_NULL::Ptr{Nothing}, R::Ref{AcbMatrix}, A::Ref{AcbMatrix}, C_NULL::Ptr{Nothing}, 0::Int, precision(parent(A))::Int)::Cint
  return nothing
end

function _approx_eig_qr(A::AcbMatrix)
  n = nrows(A)
  v = acb_vec(n)
  R = zero_matrix(base_ring(A), ncols(A), nrows(A))
  __approx_eig_qr!(v, R, A)
  z = array(base_ring(A), v, n)
  acb_vec_clear(v, n)
  return z, R
end

function _eig_multiple(A::AcbMatrix, check::Bool = true)
  n = nrows(A)
  v = acb_vec(n)
  v_approx = acb_vec(n)
  R = zero_matrix(base_ring(A), n, n)
  __approx_eig_qr!(v, R, A)
  b = @ccall libflint.acb_mat_eig_multiple(v_approx::Ptr{acb_struct}, A::Ref{AcbMatrix}, v::Ptr{acb_struct}, R::Ref{AcbMatrix}, precision(base_ring(A))::Int)::Cint
  check && b == 0 && error("Could not isolate eigenvalues of matrix $A")
  z = array(base_ring(A), v, n)
  acb_vec_clear(v, n)
  acb_vec_clear(v_approx, n)
  res = Vector{Tuple{AcbFieldElem, Int}}()
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

function _eig_simple(A::AcbMatrix; check::Bool = true, algorithm::Symbol = :default)
  n = nrows(A)
  v = acb_vec(n)
  v_approx = acb_vec(n)
  Rapprox = zero_matrix(base_ring(A), n, n)
  L = zero_matrix(base_ring(A), n, n)
  R = zero_matrix(base_ring(A), n, n)
  __approx_eig_qr!(v, Rapprox, A)
  if algorithm == :vdhoeven_mourrain
    b = @ccall libflint.acb_mat_eig_simple_vdhoeven_mourrain(v_approx::Ptr{acb_struct}, L::Ref{AcbMatrix}, R::Ref{AcbMatrix}, A::Ref{AcbMatrix}, v::Ptr{acb_struct}, Rapprox::Ref{AcbMatrix}, precision(base_ring(A))::Int)::Cint
  elseif algorithm == :rump
    b = @ccall libflint.acb_mat_eig_simple_rump(v_approx::Ptr{acb_struct}, L::Ref{AcbMatrix}, R::Ref{AcbMatrix}, A::Ref{AcbMatrix}, v::Ptr{acb_struct}, Rapprox::Ref{AcbMatrix}, precision(base_ring(A))::Int)::Cint
  elseif algorithm == :default
    b = @ccall libflint.acb_mat_eig_simple(v_approx::Ptr{acb_struct}, L::Ref{AcbMatrix}, R::Ref{AcbMatrix}, A::Ref{AcbMatrix}, v::Ptr{acb_struct}, Rapprox::Ref{AcbMatrix}, precision(base_ring(A))::Int)::Cint
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
    eigenvalues_simple(A::AcbMatrix, algorithm::Symbol = :default)

Returns the eigenvalues of `A` as a vector of `AcbFieldElem`. It is assumed that `A`
has only simple eigenvalues.

The algorithm used can be changed by setting the `algorithm` keyword to
`:vdhoeven_mourrain` or `:rump`.

This function is experimental.
"""
function eigenvalues_simple(A::AcbMatrix, algorithm::Symbol = :default)
  E, _, _ = _eig_simple(A, algorithm = algorithm)
  return E
end

@doc raw"""
    eigenvalues_with_multiplicities(A::AcbMatrix)

Return the eigenvalues of `A` with their algebraic multiplicities as a vector of
tuples `(AcbFieldElem, Int)`. Each tuple `(z, k)` corresponds to a cluster of `k`
eigenvalues of $A$.

This function is experimental.
"""
function eigenvalues_with_multiplicities(A::AcbMatrix)
  e, _ = _eig_multiple(A)
  return e
end

@doc raw"""
    eigenvalues(A::AcbMatrix)

Return the eigenvalues of `A`.

This function is experimental.
"""
function eigenvalues(A::AcbMatrix)
  e, _ = _eig_multiple(A)
  return [ x[1] for x in e ]
end
