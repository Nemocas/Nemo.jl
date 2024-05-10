###############################################################################
#
#   RealMat.jl : Arb matrices over ArbFieldElem
#
###############################################################################

###############################################################################
#
#   Similar & zero
#
###############################################################################

function similar(::RealMat, R::RealField, r::Int, c::Int)
   z = RealMat(r, c)
   return z
end

zero(m::RealMat, R::RealField, r::Int, c::Int) = similar(m, R, r, c)

###############################################################################
#
#   Basic manipulation
#
###############################################################################

base_ring(a::RealMat) = RealField()

dense_matrix_type(::Type{RealFieldElem}) = RealMat

function check_parent(x::RealMat, y::RealMat, throw::Bool = true)
   fl = (nrows(x) != nrows(y) || ncols(x) != ncols(y) || base_ring(x) != base_ring(y))
   fl && throw && error("Incompatible matrices")
   return !fl
end

function getindex!(z::ArbFieldElem, x::RealMat, r::Int, c::Int)
  GC.@preserve x begin
     v = ccall((:arb_mat_entry_ptr, libflint), Ptr{RealFieldElem},
                 (Ref{RealMat}, Int, Int), x, r - 1, c - 1)
     ccall((:arb_set, libflint), Nothing, (Ref{RealFieldElem}, Ptr{RealFieldElem}), z, v)
  end
  return z
end

@inline function getindex(x::RealMat, r::Int, c::Int)
  @boundscheck Generic._checkbounds(x, r, c)

  z = base_ring(x)()
  GC.@preserve x begin
     v = ccall((:arb_mat_entry_ptr, libflint), Ptr{RealFieldElem},
                 (Ref{RealMat}, Int, Int), x, r - 1, c - 1)
     ccall((:arb_set, libflint), Nothing, (Ref{RealFieldElem}, Ptr{RealFieldElem}), z, v)
  end
  return z
end

for T in [Int, UInt, ZZRingElem, QQFieldElem, Float64, BigFloat, RealFieldElem, AbstractString]
   @eval begin
      @inline function setindex!(x::RealMat, y::$T, r::Int, c::Int)
         @boundscheck Generic._checkbounds(x, r, c)

         GC.@preserve x begin
            z = ccall((:arb_mat_entry_ptr, libflint), Ptr{RealFieldElem},
                      (Ref{RealMat}, Int, Int), x, r - 1, c - 1)
            Nemo._arb_set(z, y, precision(Balls))
         end
      end
   end
end

Base.@propagate_inbounds setindex!(x::RealMat, y::Integer,
                                 r::Int, c::Int) =
         setindex!(x, ZZRingElem(y), r, c)

Base.@propagate_inbounds setindex!(x::RealMat, y::Rational{T},
                                 r::Int, c::Int) where {T <: Integer} =
         setindex!(x, ZZRingElem(y), r, c)

function one(x::RealMatSpace)
  z = x()
  ccall((:arb_mat_one, libflint), Nothing, (Ref{RealMat}, ), z)
  return z
end

number_of_rows(a::RealMat) = a.r

number_of_columns(a::RealMat) = a.c

function deepcopy_internal(x::RealMat, dict::IdDict)
  z = RealMat(nrows(x), ncols(x))
  ccall((:arb_mat_set, libflint), Nothing, (Ref{RealMat}, Ref{RealMat}), z, x)
  return z
end

################################################################################
#
#  Unary operations
#
################################################################################

function -(x::RealMat)
  z = similar(x)
  ccall((:arb_mat_neg, libflint), Nothing, (Ref{RealMat}, Ref{RealMat}), z, x)
  return z
end

################################################################################
#
#  Transpose
#
################################################################################

function transpose(x::RealMat)
  z = similar(x, ncols(x), nrows(x))
  ccall((:arb_mat_transpose, libflint), Nothing,
              (Ref{RealMat}, Ref{RealMat}), z, x)
  return z
end

################################################################################
#
#  Binary operations
#
################################################################################

function +(x::RealMat, y::RealMat)
  check_parent(x, y)
  z = similar(x)
  ccall((:arb_mat_add, libflint), Nothing,
              (Ref{RealMat}, Ref{RealMat}, Ref{RealMat}, Int),
              z, x, y, precision(Balls))
  return z
end

function -(x::RealMat, y::RealMat)
  check_parent(x, y)
  z = similar(x)
  ccall((:arb_mat_sub, libflint), Nothing,
              (Ref{RealMat}, Ref{RealMat}, Ref{RealMat}, Int),
              z, x, y, precision(Balls))
  return z
end

function *(x::RealMat, y::RealMat)
  ncols(x) != nrows(y) && error("Matrices have wrong dimensions")
  z = similar(x, nrows(x), ncols(y))
  ccall((:arb_mat_mul, libflint), Nothing,
              (Ref{RealMat}, Ref{RealMat}, Ref{RealMat}, Int),
              z, x, y, precision(Balls))
  return z
end

################################################################################
#
#   Ad hoc binary operators
#
################################################################################

function ^(x::RealMat, y::UInt)
  nrows(x) != ncols(x) && error("Matrix must be square")
  z = similar(x)
  ccall((:arb_mat_pow_ui, libflint), Nothing,
              (Ref{RealMat}, Ref{RealMat}, UInt, Int),
              z, x, y, precision(Balls))
  return z
end

function *(x::RealMat, y::Int)
  z = similar(x)
  ccall((:arb_mat_scalar_mul_si, libflint), Nothing,
              (Ref{RealMat}, Ref{RealMat}, Int, Int),
              z, x, y, precision(Balls))
  return z
end

*(x::Int, y::RealMat) = y*x

*(x::RealMat, y::QQFieldElem) = x*base_ring(x)(y)

*(x::QQFieldElem, y::RealMat) = y*x

function *(x::RealMat, y::ZZRingElem)
  z = similar(x)
  ccall((:arb_mat_scalar_mul_fmpz, libflint), Nothing,
              (Ref{RealMat}, Ref{RealMat}, Ref{ZZRingElem}, Int),
              z, x, y, precision(Balls))
  return z
end

*(x::ZZRingElem, y::RealMat) = y*x

function *(x::RealMat, y::ArbFieldElem)
  z = similar(x)
  ccall((:arb_mat_scalar_mul_arb, libflint), Nothing,
              (Ref{RealMat}, Ref{RealMat}, Ref{RealFieldElem}, Int),
              z, x, y, precision(Balls))
  return z
end

*(x::ArbFieldElem, y::RealMat) = y*x

for T in [Integer, ZZRingElem, QQFieldElem, RealFieldElem]
   @eval begin
      function +(x::RealMat, y::$T)
         z = deepcopy(x)
         for i = 1:min(nrows(x), ncols(x))
            z[i, i] += y
         end
         return z
      end

      +(x::$T, y::RealMat) = y + x

      function -(x::RealMat, y::$T)
         z = deepcopy(x)
         for i = 1:min(nrows(x), ncols(x))
            z[i, i] -= y
         end
         return z
      end

      function -(x::$T, y::RealMat)
         z = -y
         for i = 1:min(nrows(y), ncols(y))
            z[i, i] += x
         end
         return z
      end
   end
end

function +(x::RealMat, y::Rational{T}) where T <: Union{Int, BigInt}
   z = deepcopy(x)
   for i = 1:min(nrows(x), ncols(x))
      z[i, i] += y
   end
   return z
end

+(x::Rational{T}, y::RealMat) where T <: Union{Int, BigInt} = y + x

function -(x::RealMat, y::Rational{T}) where T <: Union{Int, BigInt}
   z = deepcopy(x)
   for i = 1:min(nrows(x), ncols(x))
      z[i, i] -= y
   end
   return z
end

function -(x::Rational{T}, y::RealMat) where T <: Union{Int, BigInt}
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

function ldexp(x::RealMat, y::Int)
  z = similar(x)
  ccall((:arb_mat_scalar_mul_2exp_si, libflint), Nothing,
              (Ref{RealMat}, Ref{RealMat}, Int), z, x, y)
  return z
end

###############################################################################
#
#   Comparisons
#
###############################################################################

@doc raw"""
    isequal(x::RealMat, y::RealMat)

Return `true` if the matrices of balls $x$ and $y$ are precisely equal,
i.e. if all matrix entries have the same midpoints and radii.
"""
function isequal(x::RealMat, y::RealMat)
  r = ccall((:arb_mat_equal, libflint), Cint,
              (Ref{RealMat}, Ref{RealMat}), x, y)
  return Bool(r)
end

function ==(x::RealMat, y::RealMat)
  fl = check_parent(x, y, false)
  !fl && return false
  r = ccall((:arb_mat_eq, libflint), Cint, (Ref{RealMat}, Ref{RealMat}), x, y)
  return Bool(r)
end

function !=(x::RealMat, y::RealMat)
  r = ccall((:arb_mat_ne, libflint), Cint, (Ref{RealMat}, Ref{RealMat}), x, y)
  return Bool(r)
end

@doc raw"""
    overlaps(x::RealMat, y::RealMat)

Returns `true` if all entries of $x$ overlap with the corresponding entry of
$y$, otherwise return `false`.
"""
function overlaps(x::RealMat, y::RealMat)
  r = ccall((:arb_mat_overlaps, libflint), Cint,
              (Ref{RealMat}, Ref{RealMat}), x, y)
  return Bool(r)
end

@doc raw"""
    contains(x::RealMat, y::RealMat)

Returns `true` if all entries of $x$ contain the corresponding entry of
$y$, otherwise return `false`.
"""
function contains(x::RealMat, y::RealMat)
  r = ccall((:arb_mat_contains, libflint), Cint,
              (Ref{RealMat}, Ref{RealMat}), x, y)
  return Bool(r)
end

###############################################################################
#
#   Ad hoc comparisons
#
###############################################################################

@doc raw"""
    contains(x::RealMat, y::ZZMatrix)

Returns `true` if all entries of $x$ contain the corresponding entry of
$y$, otherwise return `false`.
"""
function contains(x::RealMat, y::ZZMatrix)
  r = ccall((:arb_mat_contains_fmpz_mat, libflint), Cint,
              (Ref{RealMat}, Ref{ZZMatrix}), x, y)
  return Bool(r)
end


@doc raw"""
    contains(x::RealMat, y::QQMatrix)

Returns `true` if all entries of $x$ contain the corresponding entry of
$y$, otherwise return `false`.
"""
function contains(x::RealMat, y::QQMatrix)
  r = ccall((:arb_mat_contains_fmpq_mat, libflint), Cint,
              (Ref{RealMat}, Ref{QQMatrix}), x, y)
  return Bool(r)
end

==(x::RealMat, y::Integer) = x == parent(x)(y)

==(x::Integer, y::RealMat) = y == x

==(x::RealMat, y::ZZRingElem) = x == parent(x)(y)

==(x::ZZRingElem, y::RealMat) = y == x

==(x::RealMat, y::ZZMatrix) = x == parent(x)(y)

==(x::ZZMatrix, y::RealMat) = y == x

###############################################################################
#
#   Inversion
#
###############################################################################

@doc raw"""
    inv(x::RealMat)

Given a  $n\times n$ matrix of type `ArbMatrix`, return an
$n\times n$ matrix $X$ such that $AX$ contains the
identity matrix. If $A$ cannot be inverted numerically an exception is raised.
"""
function inv(x::RealMat)
  fl, z = is_invertible_with_inverse(x)
  fl && return z
  error("Matrix singular or cannot be inverted numerically")
end

function is_invertible_with_inverse(x::RealMat)
  ncols(x) != nrows(x) && return false, x
  z = similar(x)
  r = ccall((:arb_mat_inv, libflint), Cint,
              (Ref{RealMat}, Ref{RealMat}, Int), z, x, precision(Balls))
  return Bool(r), z
end

###############################################################################
#
#   Exact division
#
###############################################################################

function divexact(x::RealMat, y::RealMat; check::Bool=true)
   ncols(x) != ncols(y) && error("Incompatible matrix dimensions")
   x*inv(y)
end

###############################################################################
#
#   Ad hoc exact division
#
###############################################################################

function divexact(x::RealMat, y::Int; check::Bool=true)
  y == 0 && throw(DivideError())
  z = similar(x)
  ccall((:arb_mat_scalar_div_si, libflint), Nothing,
              (Ref{RealMat}, Ref{RealMat}, Int, Int),
              z, x, y, precision(Balls))
  return z
end

function divexact(x::RealMat, y::ZZRingElem; check::Bool=true)
  z = similar(x)
  ccall((:arb_mat_scalar_div_fmpz, libflint), Nothing,
              (Ref{RealMat}, Ref{RealMat}, Ref{ZZRingElem}, Int),
              z, x, y, precision(Balls))
  return z
end

function divexact(x::RealMat, y::ArbFieldElem; check::Bool=true)
  z = similar(x)
  ccall((:arb_mat_scalar_div_arb, libflint), Nothing,
              (Ref{RealMat}, Ref{RealMat}, Ref{RealFieldElem}, Int),
              z, x, y, precision(Balls))
  return z
end

################################################################################
#
#  Characteristic polynomial
#
################################################################################

function charpoly(x::RealPolyRing, y::RealMat, prec::Int = precision(Balls))
  base_ring(y) != base_ring(x) && error("Base rings must coincide")
  z = x()
  ccall((:arb_mat_charpoly, libflint), Nothing,
              (Ref{RealPoly}, Ref{RealMat}, Int), z, y, prec)
  return z
end

###############################################################################
#
#   Determinant
#
###############################################################################

function det(x::RealMat, prec::Int = precision(Balls))
  ncols(x) != nrows(x) && error("Matrix must be square")
  z = base_ring(x)()
  ccall((:arb_mat_det, libflint), Nothing,
              (Ref{RealFieldElem}, Ref{RealMat}, Int), z, x, prec)
  return z
end

################################################################################
#
#   Exponential function
#
################################################################################

function Base.exp(x::RealMat)
  ncols(x) != nrows(x) && error("Matrix must be square")
  z = similar(x)
  ccall((:arb_mat_exp, libflint), Nothing,
              (Ref{RealMat}, Ref{RealMat}, Int), z, x, precision(Balls))
  return z
end

###############################################################################
#
#   Linear solving
#
###############################################################################

function lu!(P::Generic.Perm, x::RealMat)
  parent(P).n != nrows(x) && error("Permutation does not match matrix")
  P.d .-= 1
  r = ccall((:arb_mat_lu, libflint), Cint,
              (Ptr{Int}, Ref{RealMat}, Ref{RealMat}, Int),
              P.d, x, x, precision(Balls))
  r == 0 && error("Could not find $(nrows(x)) invertible pivot elements")
  P.d .+= 1
  inv!(P)
  return min(nrows(x), ncols(x))
end

function _solve!(z::RealMat, x::RealMat, y::RealMat)
  r = ccall((:arb_mat_solve, libflint), Cint,
              (Ref{RealMat}, Ref{RealMat}, Ref{RealMat}, Int),
              z, x, y, precision(Balls))
  r == 0 && error("Matrix cannot be inverted numerically")
  nothing
end

function _solve_lu_precomp!(z::RealMat, P::Generic.Perm, LU::RealMat, y::RealMat)
  Q = inv(P)
  ccall((:arb_mat_solve_lu_precomp, libflint), Nothing,
              (Ref{RealMat}, Ptr{Int}, Ref{RealMat}, Ref{RealMat}, Int),
              z, Q.d .- 1, LU, y, precision(Balls))
  nothing
end

function _solve_lu_precomp(P::Generic.Perm, LU::RealMat, y::RealMat)
  ncols(LU) != nrows(y) && error("Matrix dimensions are wrong")
  z = similar(y)
  _solve_lu_precomp!(z, P, LU, y)
  return z
end

function Solve._can_solve_internal_no_check(A::RealMat, b::RealMat, task::Symbol; side::Symbol = :left)
   nrows(A) != ncols(A) && error("Only implemented for square matrices")
   if side === :left
      fl, sol, K = Solve._can_solve_internal_no_check(transpose(A), transpose(b), task, side = :right)
      return fl, transpose(sol), transpose(K)
   end

   x = similar(A, ncols(A), ncols(b))
   fl = ccall((:arb_mat_solve, libflint), Cint,
              (Ref{RealMat}, Ref{RealMat}, Ref{RealMat}, Int),
              x, A, b, precision(Balls))
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

function solve_init(A::RealMat)
   return Solve.SolveCtx{RealFieldElem, RealMat, RealMat, RealMat}(A)
end

function Solve._init_reduce(C::Solve.SolveCtx{RealFieldElem})
   if isdefined(C, :red) && isdefined(C, :lu_perm)
      return nothing
   end

   nrows(C) != ncols(C) && error("Only implemented for square matrices")

   A = matrix(C)
   P = Generic.Perm(nrows(C))
   x = similar(A, nrows(A), ncols(A))
   P.d .-= 1
   fl = ccall((:arb_mat_lu, libflint), Cint,
               (Ptr{Int}, Ref{RealMat}, Ref{RealMat}, Int),
               P.d, x, A, precision(Balls))
   fl == 0 && error("Could not find $(nrows(x)) invertible pivot elements")
   P.d .+= 1
   inv!(P)

   C.red = x
   C.lu_perm = P
   return nothing
end

function Solve._init_reduce_transpose(C::Solve.SolveCtx{RealFieldElem})
   if isdefined(C, :red_transp) && isdefined(C, :lu_perm_transp)
      return nothing
   end

   nrows(C) != ncols(C) && error("Only implemented for square matrices")

   A = transpose(matrix(C))
   P = Generic.Perm(nrows(C))
   x = similar(A, nrows(A), ncols(A))
   P.d .-= 1
   fl = ccall((:arb_mat_lu, libflint), Cint,
               (Ptr{Int}, Ref{RealMat}, Ref{RealMat}, Int),
               P.d, x, A, precision(Balls))
   fl == 0 && error("Could not find $(nrows(x)) invertible pivot elements")
   P.d .+= 1
   inv!(P)

   C.red_transp = x
   C.lu_perm_transp = P
   return nothing
end

function Solve._can_solve_internal_no_check(C::Solve.SolveCtx{RealFieldElem}, b::RealMat, task::Symbol; side::Symbol = :left)
   if side === :right
      LU = Solve.reduced_matrix(C)
      p = Solve.lu_permutation(C)
   else
      LU = Solve.reduced_matrix_of_transpose(C)
      p = Solve.lu_permutation_of_transpose(C)
      b = transpose(b)
   end

   x = similar(b, ncols(C), ncols(b))
   ccall((:arb_mat_solve_lu_precomp, libflint), Nothing,
         (Ref{RealMat}, Ptr{Int}, Ref{RealMat}, Ref{RealMat}, Int),
         x, inv(p).d .- 1, LU, b, precision(Balls))

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

function swap_rows(x::RealMat, i::Int, j::Int)
  Generic._checkbounds(nrows(x), i) || throw(BoundsError())
  Generic._checkbounds(nrows(x), j) || throw(BoundsError())
  z = deepcopy(x)
  swap_rows!(z, i, j)
  return z
end

function swap_rows!(x::RealMat, i::Int, j::Int)
  ccall((:arb_mat_swap_rows, libflint), Nothing,
              (Ref{RealMat}, Ptr{Nothing}, Int, Int),
              x, C_NULL, i - 1, j - 1)
end

################################################################################
#
#   Norm
#
################################################################################

@doc raw"""
    bound_inf_norm(x::RealMat)

Returns a non-negative element $z$ of type `ArbFieldElem`, such that $z$ is an upper
bound for the infinity norm for every matrix in $x$
"""
function bound_inf_norm(x::RealMat)
  z = RealFieldElem()
  GC.@preserve x z begin
     t = ccall((:arb_rad_ptr, libflint), Ptr{mag_struct}, (Ref{RealFieldElem}, ), z)
     ccall((:arb_mat_bound_inf_norm, libflint), Nothing,
                 (Ptr{mag_struct}, Ref{RealMat}), t, x)
     s = ccall((:arb_mid_ptr, libflint), Ptr{arf_struct}, (Ref{RealFieldElem}, ), z)
     ccall((:arf_set_mag, libflint), Nothing,
                 (Ptr{arf_struct}, Ptr{mag_struct}), s, t)
     ccall((:mag_zero, libflint), Nothing,
                 (Ptr{mag_struct},), t)
  end
  return base_ring(x)(z)
end

################################################################################
#
#   Unsafe functions
#
################################################################################

for (s,f) in (("add!","arb_mat_add"), ("mul!","arb_mat_mul"),
              ("sub!","arb_mat_sub"))
  @eval begin
    function ($(Symbol(s)))(z::RealMat, x::RealMat, y::RealMat, prec::Int = precision(Balls))
      ccall(($f, libflint), Nothing,
                  (Ref{RealMat}, Ref{RealMat}, Ref{RealMat}, Int),
                  z, x, y, prec)
      return z
    end
  end
end

###############################################################################
#
#   Parent object call overloads
#
###############################################################################

function (x::RealMatSpace)()
  z = RealMat(nrows(x), ncols(x))
  return z
end

function (x::RealMatSpace)(y::ZZMatrix)
  (ncols(x) != ncols(y) || nrows(x) != nrows(y)) &&
      error("Dimensions are wrong")
  z = RealMat(y, precision(Balls))
  return z
end

function (x::RealMatSpace)(y::AbstractMatrix{T}) where {T <: Union{Int, UInt, ZZRingElem, QQFieldElem, Float64, BigFloat, RealFieldElem, AbstractString}}
  _check_dim(nrows(x), ncols(x), y)
  z = RealMat(nrows(x), ncols(x), y, precision(Balls))
  return z
end

function (x::RealMatSpace)(y::AbstractVector{T}) where {T <: Union{Int, UInt, ZZRingElem, QQFieldElem, Float64, BigFloat, RealFieldElem, AbstractString}}
  _check_dim(nrows(x), ncols(x), y)
  z = RealMat(nrows(x), ncols(x), y, precision(Balls))
  return z
end

function (x::RealMatSpace)(y::Union{Int, UInt, ZZRingElem, QQFieldElem, Float64,
                          BigFloat, RealFieldElem, AbstractString})
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

###############################################################################
#
#   Matrix constructor
#
###############################################################################

function matrix(R::RealField, arr::AbstractMatrix{T}) where {T <: Union{Int, UInt, ZZRingElem, QQFieldElem, Float64, BigFloat, RealFieldElem, AbstractString}}
   z = RealMat(size(arr, 1), size(arr, 2), arr, precision(Balls))
   return z
end

function matrix(R::RealField, r::Int, c::Int, arr::AbstractVector{T}) where {T <: Union{Int, UInt, ZZRingElem, QQFieldElem, Float64, BigFloat, RealFieldElem, AbstractString}}
   _check_dim(r, c, arr)
   z = RealMat(r, c, arr, precision(Balls))
   return z
end

function matrix(R::RealField, arr::AbstractMatrix{<: Integer})
   arr_fmpz = map(ZZRingElem, arr)
   return matrix(R, arr_fmpz)
end

function matrix(R::RealField, r::Int, c::Int, arr::AbstractVector{<: Integer})
   arr_fmpz = map(ZZRingElem, arr)
   return matrix(R, r, c, arr_fmpz)
end

function matrix(R::RealField, arr::AbstractMatrix{Rational{T}}) where {T <: Integer}
   arr_fmpz = map(QQFieldElem, arr)
   return matrix(R, arr_fmpz)
end

function matrix(R::RealField, r::Int, c::Int, arr::AbstractVector{Rational{T}}) where {T <: Integer}
   arr_fmpz = map(QQFieldElem, arr)
   return matrix(R, r, c, arr_fmpz)
end

###############################################################################
#
#  Zero matrix
#
###############################################################################

function zero_matrix(R::RealField, r::Int, c::Int)
   if r < 0 || c < 0
     error("dimensions must not be negative")
   end
   z = RealMat(r, c)
   return z
end

###############################################################################
#
#  Identity matrix
#
###############################################################################

function identity_matrix(R::RealField, n::Int)
   if n < 0
     error("dimension must not be negative")
   end
   z = RealMat(n, n)
   ccall((:arb_mat_one, libflint), Nothing, (Ref{RealMat}, ), z)
   return z
end

###############################################################################
#
#   Promotions
#
###############################################################################

promote_rule(::Type{RealMat}, ::Type{T}) where {T <: Integer} = RealMat

promote_rule(::Type{RealMat}, ::Type{Rational{T}}) where T <: Union{Int, BigInt} = RealMat

promote_rule(::Type{RealMat}, ::Type{ZZRingElem}) = RealMat

promote_rule(::Type{RealMat}, ::Type{QQFieldElem}) = RealMat

promote_rule(::Type{RealMat}, ::Type{RealFieldElem}) = RealMat

promote_rule(::Type{RealMat}, ::Type{Float64}) = RealMat

promote_rule(::Type{RealMat}, ::Type{BigFloat}) = RealMat

promote_rule(::Type{RealMat}, ::Type{ZZMatrix}) = RealMat

promote_rule(::Type{RealMat}, ::Type{QQMatrix}) = RealMat
