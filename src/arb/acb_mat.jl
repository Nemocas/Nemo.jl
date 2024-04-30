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
   z = AcbMatrix(r, c)
   z.base_ring = R
   return z
end

zero(m::AcbMatrix, R::AcbField, r::Int, c::Int) = similar(m, R, r, c)

###############################################################################
#
#   Basic manipulation
#
###############################################################################

parent_type(::Type{AcbMatrix}) = AcbMatSpace

elem_type(::Type{AcbMatSpace}) = AcbMatrix

parent(x::AcbMatrix) = matrix_space(base_ring(x), nrows(x), ncols(x))

dense_matrix_type(::Type{AcbFieldElem}) = AcbMatrix

precision(x::AcbMatSpace) = precision(x.base_ring)

base_ring(a::AcbMatSpace) = a.base_ring

base_ring(a::AcbMatrix) = a.base_ring

function check_parent(x::AcbMatrix, y::AcbMatrix, throw::Bool = true)
   fl = (nrows(x) != nrows(y) || ncols(x) != ncols(y) || base_ring(x) != base_ring(y))
   fl && throw && error("Incompatible matrices")
   return !fl
end

function getindex!(z::AcbFieldElem, x::AcbMatrix, r::Int, c::Int)
  GC.@preserve x begin
    v = ccall((:acb_mat_entry_ptr, libarb), Ptr{AcbFieldElem},
                (Ref{AcbMatrix}, Int, Int), x, r - 1, c - 1)
    ccall((:acb_set, libarb), Nothing, (Ref{AcbFieldElem}, Ptr{AcbFieldElem}), z, v)
  end
  return z
end

@inline function getindex(x::AcbMatrix, r::Int, c::Int)
  @boundscheck Generic._checkbounds(x, r, c)

  z = base_ring(x)()
  GC.@preserve x begin
     v = ccall((:acb_mat_entry_ptr, libarb), Ptr{AcbFieldElem},
               (Ref{AcbMatrix}, Int, Int), x, r - 1, c - 1)
     ccall((:acb_set, libarb), Nothing, (Ref{AcbFieldElem}, Ptr{AcbFieldElem}), z, v)
  end
  return z
end

for T in [Integer, Float64, ZZRingElem, QQFieldElem, ArbFieldElem, BigFloat, AcbFieldElem, AbstractString]
   @eval begin
      @inline function setindex!(x::AcbMatrix, y::$T, r::Int, c::Int)
         @boundscheck Generic._checkbounds(x, r, c)

         GC.@preserve x begin
            z = ccall((:acb_mat_entry_ptr, libarb), Ptr{AcbFieldElem},
                      (Ref{AcbMatrix}, Int, Int), x, r - 1, c - 1)
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
         @boundscheck Generic._checkbounds(x, r, c)

         GC.@preserve x begin
            z = ccall((:acb_mat_entry_ptr, libarb), Ptr{AcbFieldElem},
                      (Ref{AcbMatrix}, Int, Int), x, r - 1, c - 1)
            _acb_set(z, y[1], y[2], precision(base_ring(x)))
         end
      end
   end
end

setindex!(x::AcbMatrix, y::Tuple{Rational{T}, Rational{T}}, r::Int, c::Int) where {T <: Integer} =
         setindex!(x, map(QQFieldElem, y), r, c)

zero(x::AcbMatSpace) = x()

function one(x::AcbMatSpace)
  z = x()
  ccall((:acb_mat_one, libarb), Nothing, (Ref{AcbMatrix}, ), z)
  return z
end

number_of_rows(a::AcbMatrix) = a.r

number_of_columns(a::AcbMatrix) = a.c

number_of_rows(a::AcbMatSpace) = a.nrows

number_of_columns(a::AcbMatSpace) = a.ncols

function deepcopy_internal(x::AcbMatrix, dict::IdDict)
  z = similar(x)
  ccall((:acb_mat_set, libarb), Nothing, (Ref{AcbMatrix}, Ref{AcbMatrix}), z, x)
  return z
end

################################################################################
#
#  Unary operations
#
################################################################################

function -(x::AcbMatrix)
  z = similar(x)
  ccall((:acb_mat_neg, libarb), Nothing, (Ref{AcbMatrix}, Ref{AcbMatrix}), z, x)
  return z
end

################################################################################
#
#  Transpose
#
################################################################################

function transpose(x::AcbMatrix)
  z = similar(x, ncols(x), nrows(x))
  ccall((:acb_mat_transpose, libarb), Nothing,
              (Ref{AcbMatrix}, Ref{AcbMatrix}), z, x)
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
  ccall((:acb_mat_add, libarb), Nothing,
              (Ref{AcbMatrix}, Ref{AcbMatrix}, Ref{AcbMatrix}, Int),
              z, x, y, precision(base_ring(x)))
  return z
end

function -(x::AcbMatrix, y::AcbMatrix)
  check_parent(x, y)
  z = similar(x)
  ccall((:acb_mat_sub, libarb), Nothing,
              (Ref{AcbMatrix}, Ref{AcbMatrix}, Ref{AcbMatrix}, Int),
              z, x, y, precision(base_ring(x)))
  return z
end

function *(x::AcbMatrix, y::AcbMatrix)
  ncols(x) != nrows(y) && error("Matrices have wrong dimensions")
  z = similar(x, nrows(x), ncols(y))
  ccall((:acb_mat_mul, libarb), Nothing,
              (Ref{AcbMatrix}, Ref{AcbMatrix}, Ref{AcbMatrix}, Int),
              z, x, y, precision(base_ring(x)))
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
  ccall((:acb_mat_pow_ui, libarb), Nothing,
              (Ref{AcbMatrix}, Ref{AcbMatrix}, UInt, Int),
              z, x, y, precision(base_ring(x)))
  return z
end

function *(x::AcbMatrix, y::Int)
  z = similar(x)
  ccall((:acb_mat_scalar_mul_si, libarb), Nothing,
              (Ref{AcbMatrix}, Ref{AcbMatrix}, Int, Int),
              z, x, y, precision(base_ring(x)))
  return z
end

*(x::Int, y::AcbMatrix) = y*x

function *(x::AcbMatrix, y::ZZRingElem)
  z = similar(x)
  ccall((:acb_mat_scalar_mul_fmpz, libarb), Nothing,
              (Ref{AcbMatrix}, Ref{AcbMatrix}, Ref{ZZRingElem}, Int),
              z, x, y, precision(base_ring(x)))
  return z
end

*(x::ZZRingElem, y::AcbMatrix) = y*x

function *(x::AcbMatrix, y::ArbFieldElem)
  z = similar(x)
  ccall((:acb_mat_scalar_mul_arb, libarb), Nothing,
              (Ref{AcbMatrix}, Ref{AcbMatrix}, Ref{ArbFieldElem}, Int),
              z, x, y, precision(base_ring(x)))
  return z
end

*(x::ArbFieldElem, y::AcbMatrix) = y*x

function *(x::AcbMatrix, y::AcbFieldElem)
  z = similar(x)
  ccall((:acb_mat_scalar_mul_acb, libarb), Nothing,
              (Ref{AcbMatrix}, Ref{AcbMatrix}, Ref{AcbFieldElem}, Int),
              z, x, y, precision(base_ring(x)))
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
  ccall((:acb_mat_scalar_mul_2exp_si, libarb), Nothing,
              (Ref{AcbMatrix}, Ref{AcbMatrix}, Int), z, x, y)
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
  r = ccall((:acb_mat_equal, libarb), Cint,
              (Ref{AcbMatrix}, Ref{AcbMatrix}), x, y)
  return Bool(r)
end

function ==(x::AcbMatrix, y::AcbMatrix)
  fl = check_parent(x, y, false)
  !fl && return false
  r = ccall((:acb_mat_eq, libarb), Cint, (Ref{AcbMatrix}, Ref{AcbMatrix}), x, y)
  return Bool(r)
end

function !=(x::AcbMatrix, y::AcbMatrix)
  r = ccall((:acb_mat_ne, libarb), Cint, (Ref{AcbMatrix}, Ref{AcbMatrix}), x, y)
  return Bool(r)
end

@doc raw"""
    overlaps(x::AcbMatrix, y::AcbMatrix)

Returns `true` if all entries of $x$ overlap with the corresponding entry of
$y$, otherwise return `false`.
"""
function overlaps(x::AcbMatrix, y::AcbMatrix)
  r = ccall((:acb_mat_overlaps, libarb), Cint,
              (Ref{AcbMatrix}, Ref{AcbMatrix}), x, y)
  return Bool(r)
end

@doc raw"""
    contains(x::AcbMatrix, y::AcbMatrix)

Returns `true` if all entries of $x$ contain the corresponding entry of
$y$, otherwise return `false`.
"""
function contains(x::AcbMatrix, y::AcbMatrix)
  r = ccall((:acb_mat_contains, libarb), Cint,
              (Ref{AcbMatrix}, Ref{AcbMatrix}), x, y)
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
  r = ccall((:acb_mat_contains_fmpz_mat, libarb), Cint,
              (Ref{AcbMatrix}, Ref{ZZMatrix}), x, y)
  return Bool(r)
end

@doc raw"""
    contains(x::AcbMatrix, y::QQMatrix)

Returns `true` if all entries of $x$ contain the corresponding entry of
$y$, otherwise return `false`.
"""
function contains(x::AcbMatrix, y::QQMatrix)
  r = ccall((:acb_mat_contains_fmpq_mat, libarb), Cint,
              (Ref{AcbMatrix}, Ref{QQMatrix}), x, y)
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
            Bool(ccall((:acb_mat_is_real, libarb), Cint, (Ref{AcbMatrix}, ), x))

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
  r = ccall((:acb_mat_inv, libarb), Cint,
              (Ref{AcbMatrix}, Ref{AcbMatrix}, Int), z, x, precision(base_ring(x)))
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
  ccall((:acb_mat_scalar_div_si, libarb), Nothing,
              (Ref{AcbMatrix}, Ref{AcbMatrix}, Int, Int),
              z, x, y, precision(base_ring(x)))
  return z
end

function divexact(x::AcbMatrix, y::ZZRingElem; check::Bool=true)
  z = similar(x)
  ccall((:acb_mat_scalar_div_fmpz, libarb), Nothing,
              (Ref{AcbMatrix}, Ref{AcbMatrix}, Ref{ZZRingElem}, Int),
              z, x, y, precision(base_ring(x)))
  return z
end

function divexact(x::AcbMatrix, y::ArbFieldElem; check::Bool=true)
  z = similar(x)
  ccall((:acb_mat_scalar_div_arb, libarb), Nothing,
              (Ref{AcbMatrix}, Ref{AcbMatrix}, Ref{ArbFieldElem}, Int),
              z, x, y, precision(base_ring(x)))
  return z
end

function divexact(x::AcbMatrix, y::AcbFieldElem; check::Bool=true)
  z = similar(x)
  ccall((:acb_mat_scalar_div_acb, libarb), Nothing,
              (Ref{AcbMatrix}, Ref{AcbMatrix}, Ref{AcbFieldElem}, Int),
              z, x, y, precision(base_ring(x)))
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
  ccall((:acb_mat_charpoly, libarb), Nothing,
              (Ref{AcbPolyRingElem}, Ref{AcbMatrix}, Int), z, y, precision(base_ring(y)))
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
  ccall((:acb_mat_det, libarb), Nothing,
              (Ref{AcbFieldElem}, Ref{AcbMatrix}, Int), z, x, precision(base_ring(x)))
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
  ccall((:acb_mat_exp, libarb), Nothing,
              (Ref{AcbMatrix}, Ref{AcbMatrix}, Int), z, x, precision(base_ring(x)))
  return z
end

###############################################################################
#
#   Linear solving
#
###############################################################################

function lu!(P::Generic.Perm, x::AcbMatrix)
  P.d .-= 1
  r = ccall((:acb_mat_lu, libarb), Cint,
              (Ptr{Int}, Ref{AcbMatrix}, Ref{AcbMatrix}, Int),
              P.d, x, x, precision(base_ring(x)))
  r == 0 && error("Could not find $(nrows(x)) invertible pivot elements")
  P.d .+= 1
  inv!(P)
  return nrows(x)
end

function _solve!(z::AcbMatrix, x::AcbMatrix, y::AcbMatrix)
  r = ccall((:acb_mat_solve, libarb), Cint,
              (Ref{AcbMatrix}, Ref{AcbMatrix}, Ref{AcbMatrix}, Int),
              z, x, y, precision(base_ring(x)))
  r == 0 && error("Matrix cannot be inverted numerically")
  nothing
end

function _solve_lu_precomp!(z::AcbMatrix, P::Generic.Perm, LU::AcbMatrix, y::AcbMatrix)
  Q = inv(P)
  ccall((:acb_mat_solve_lu_precomp, libarb), Nothing,
              (Ref{AcbMatrix}, Ptr{Int}, Ref{AcbMatrix}, Ref{AcbMatrix}, Int),
              z, Q.d .- 1, LU, y, precision(base_ring(LU)))
  nothing
end

function _solve_lu_precomp(P::Generic.Perm, LU::AcbMatrix, y::AcbMatrix)
  ncols(LU) != nrows(y) && error("Matrix dimensions are wrong")
  z = similar(y)
  _solve_lu_precomp!(z, P, LU, y)
  return z
end

function Solve._can_solve_internal_no_check(A::AcbMatrix, b::AcbMatrix, task::Symbol; side::Symbol = :left)
   nrows(A) != ncols(A) && error("Only implemented for square matrices")
   if side === :left
      fl, sol, K = Solve._can_solve_internal_no_check(transpose(A), transpose(b), task, side = :right)
      return fl, transpose(sol), transpose(K)
   end

   x = similar(A, ncols(A), ncols(b))
   fl = ccall((:acb_mat_solve, libarb), Cint,
              (Ref{AcbMatrix}, Ref{AcbMatrix}, Ref{AcbMatrix}, Int),
              x, A, b, precision(base_ring(A)))
   fl == 0 && error("Matrix cannot be inverted numerically")
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

function solve_init(A::AcbMatrix)
   return Solve.SolveCtx{AcbFieldElem, AcbMatrix, AcbMatrix, AcbMatrix}(A)
end

function Solve._init_reduce(C::Solve.SolveCtx{AcbFieldElem})
   if isdefined(C, :red) && isdefined(C, :lu_perm)
      return nothing
   end

   nrows(C) != ncols(C) && error("Only implemented for square matrices")

   A = matrix(C)
   P = Generic.Perm(nrows(C))
   x = similar(A, nrows(A), ncols(A))
   P.d .-= 1
   fl = ccall((:acb_mat_lu, libarb), Cint,
               (Ptr{Int}, Ref{AcbMatrix}, Ref{AcbMatrix}, Int),
               P.d, x, A, precision(base_ring(A)))
   fl == 0 && error("Could not find $(nrows(x)) invertible pivot elements")
   P.d .+= 1
   inv!(P)

   C.red = x
   C.lu_perm = P
   return nothing
end

function Solve._init_reduce_transpose(C::Solve.SolveCtx{AcbFieldElem})
   if isdefined(C, :red_transp) && isdefined(C, :lu_perm_transp)
      return nothing
   end

   nrows(C) != ncols(C) && error("Only implemented for square matrices")

   A = transpose(matrix(C))
   P = Generic.Perm(nrows(C))
   x = similar(A, nrows(A), ncols(A))
   P.d .-= 1
   fl = ccall((:acb_mat_lu, libarb), Cint,
               (Ptr{Int}, Ref{AcbMatrix}, Ref{AcbMatrix}, Int),
               P.d, x, A, precision(base_ring(A)))
   fl == 0 && error("Could not find $(nrows(x)) invertible pivot elements")
   P.d .+= 1
   inv!(P)

   C.red_transp = x
   C.lu_perm_transp = P
   return nothing
end

function Solve._can_solve_internal_no_check(C::Solve.SolveCtx{AcbFieldElem}, b::AcbMatrix, task::Symbol; side::Symbol = :left)
   if side === :right
      LU = Solve.reduced_matrix(C)
      p = Solve.lu_permutation(C)
   else
      LU = Solve.reduced_matrix_of_transpose(C)
      p = Solve.lu_permutation_of_transpose(C)
      b = transpose(b)
   end

   x = similar(b, ncols(C), ncols(b))
   ccall((:acb_mat_solve_lu_precomp, libarb), Nothing,
         (Ref{AcbMatrix}, Ptr{Int}, Ref{AcbMatrix}, Ref{AcbMatrix}, Int),
         x, inv(p).d .- 1, LU, b, precision(base_ring(LU)))

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

function swap_rows(x::AcbMatrix, i::Int, j::Int)
  Generic._checkbounds(nrows(x), i) || throw(BoundsError())
  Generic._checkbounds(nrows(x), j) || throw(BoundsError())
  z = deepcopy(x)
  swap_rows!(z, i, j)
  return z
end

function swap_rows!(x::AcbMatrix, i::Int, j::Int)
  ccall((:acb_mat_swap_rows, libarb), Nothing,
              (Ref{AcbMatrix}, Ptr{Nothing}, Int, Int),
              x, C_NULL, i - 1, j - 1)
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
     t = ccall((:arb_rad_ptr, libarb), Ptr{mag_struct}, (Ref{ArbFieldElem}, ), z)
     ccall((:acb_mat_bound_inf_norm, libarb), Nothing,
                 (Ptr{mag_struct}, Ref{AcbMatrix}), t, x)
     s = ccall((:arb_mid_ptr, libarb), Ptr{arf_struct}, (Ref{ArbFieldElem}, ), z)
     ccall((:arf_set_mag, libarb), Nothing,
                 (Ptr{arf_struct}, Ptr{mag_struct}), s, t)
     ccall((:mag_zero, libarb), Nothing,
                 (Ptr{mag_struct},), t)
  end
  return ArbField(precision(base_ring(x)))(z)
end

################################################################################
#
#   Unsafe functions
#
################################################################################

for (s,f) in (("add!","acb_mat_add"), ("mul!","acb_mat_mul"),
              ("sub!","acb_mat_sub"))
  @eval begin
    function ($(Symbol(s)))(z::AcbMatrix, x::AcbMatrix, y::AcbMatrix)
      ccall(($f, libarb), Nothing,
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

function (x::AcbMatSpace)()
  z = AcbMatrix(nrows(x), ncols(x))
  z.base_ring = x.base_ring
  return z
end

function (x::AcbMatSpace)(y::ZZMatrix)
  (ncols(x) != ncols(y) || nrows(x) != nrows(y)) &&
      error("Dimensions are wrong")
  z = AcbMatrix(y, precision(x))
  z.base_ring = x.base_ring
  return z
end

function (x::AcbMatSpace)(y::ArbMatrix)
  (ncols(x) != ncols(y) || nrows(x) != nrows(y)) &&
      error("Dimensions are wrong")
  z = AcbMatrix(y, precision(x))
  z.base_ring = x.base_ring
  return z
end

for T in [Float64, ZZRingElem, QQFieldElem, BigFloat, ArbFieldElem, AcbFieldElem, String]
   @eval begin
      function (x::AcbMatSpace)(y::AbstractMatrix{$T})
         _check_dim(nrows(x), ncols(x), y)
         z = AcbMatrix(nrows(x), ncols(x), y, precision(x))
         z.base_ring = x.base_ring
         return z
      end

      function (x::AcbMatSpace)(y::AbstractVector{$T})
         _check_dim(nrows(x), ncols(x), y)
         z = AcbMatrix(nrows(x), ncols(x), y, precision(x))
         z.base_ring = x.base_ring
         return z
      end
   end
end

(x::AcbMatSpace)(y::AbstractMatrix{T}) where {T <: Integer} = x(map(ZZRingElem, y))

(x::AcbMatSpace)(y::AbstractVector{T}) where {T <: Integer} = x(map(ZZRingElem, y))

(x::AcbMatSpace)(y::AbstractMatrix{Rational{T}}) where {T <: Integer} = x(map(QQFieldElem, y))

(x::AcbMatSpace)(y::AbstractVector{Rational{T}}) where {T <: Integer} = x(map(QQFieldElem, y))

for T in [Float64, ZZRingElem, QQFieldElem, BigFloat, ArbFieldElem, String]
   @eval begin
      function (x::AcbMatSpace)(y::AbstractMatrix{Tuple{$T, $T}})
         _check_dim(nrows(x), ncols(x), y)
         z = AcbMatrix(nrows(x), ncols(x), y, precision(x))
         z.base_ring = x.base_ring
         return z
      end

      function (x::AcbMatSpace)(y::AbstractVector{Tuple{$T, $T}})
         _check_dim(nrows(x), ncols(x), y)
         z = AcbMatrix(nrows(x), ncols(x), y, precision(x))
         z.base_ring = x.base_ring
         return z
      end
   end
end

(x::AcbMatSpace)(y::AbstractMatrix{Tuple{T, T}}) where {T <: Integer} =
         x(map(z -> (ZZRingElem(z[1]), ZZRingElem(z[2])), y))

(x::AcbMatSpace)(y::AbstractVector{Tuple{T, T}}) where {T <: Integer} =
         x(map(z -> (ZZRingElem(z[1]), ZZRingElem(z[2])), y))

(x::AcbMatSpace)(y::AbstractMatrix{Tuple{Rational{T}, Rational{T}}}) where {T <: Integer} =
         x(map(z -> (QQFieldElem(z[1]), QQFieldElem(z[2])), y))

(x::AcbMatSpace)(y::AbstractVector{Tuple{Rational{T}, Rational{T}}}) where {T <: Integer} =
         x(map(z -> (QQFieldElem(z[1]), QQFieldElem(z[2])), y))

for T in [Integer, ZZRingElem, QQFieldElem, Float64, BigFloat, ArbFieldElem, AcbFieldElem, String]
   @eval begin
      function (x::AcbMatSpace)(y::$T)
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

(x::AcbMatSpace)(y::Rational{T}) where {T <: Integer} = x(QQFieldElem(y))

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
   z = AcbMatrix(r, c)
   z.base_ring = R
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
   z = AcbMatrix(n, n)
   ccall((:acb_mat_one, libarb), Nothing, (Ref{AcbMatrix}, ), z)
   z.base_ring = R
   return z
end

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
  ccall((:acb_mat_approx_eig_qr, libarb), Cint,
        (Ptr{acb_struct}, Ptr{Nothing}, Ref{AcbMatrix},
        Ref{AcbMatrix}, Ptr{Nothing}, Int, Int),
        v, C_NULL, R, A, C_NULL, 0, precision(parent(A)))
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
  b = ccall((:acb_mat_eig_multiple, libarb), Cint,
            (Ptr{acb_struct}, Ref{AcbMatrix}, Ptr{acb_struct}, Ref{AcbMatrix}, Int),
             v_approx, A, v, R, precision(base_ring(A)))
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
      b = ccall((:acb_mat_eig_simple_vdhoeven_mourrain, libarb), Cint,
                (Ptr{acb_struct}, Ref{AcbMatrix}, Ref{AcbMatrix},
                 Ref{AcbMatrix}, Ptr{acb_struct}, Ref{AcbMatrix}, Int),
                 v_approx, L, R, A, v, Rapprox, precision(base_ring(A)))
  elseif algorithm == :rump
      b = ccall((:acb_mat_eig_simple_rump, libarb), Cint,
                (Ptr{acb_struct}, Ref{AcbMatrix}, Ref{AcbMatrix},
                 Ref{AcbMatrix}, Ptr{acb_struct}, Ref{AcbMatrix}, Int),
                 v_approx, L, R, A, v, Rapprox, precision(base_ring(A)))
  elseif algorithm == :default
      b = ccall((:acb_mat_eig_simple, libarb), Cint,
                (Ptr{acb_struct}, Ref{AcbMatrix}, Ref{AcbMatrix},
                 Ref{AcbMatrix}, Ptr{acb_struct}, Ref{AcbMatrix}, Int),
                 v_approx, L, R, A, v, Rapprox, precision(base_ring(A)))
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

###############################################################################
#
#   matrix_space constructor
#
###############################################################################

function matrix_space(R::AcbField, r::Int, c::Int; cached = true)
  # TODO/FIXME: `cached` is ignored and only exists for backwards compatibility
  return AcbMatSpace(R, r, c)
end
