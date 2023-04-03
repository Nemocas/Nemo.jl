###############################################################################
#
#   arb_mat.jl : Arb matrices over arb
#
###############################################################################

export zero, one, deepcopy, -, transpose, +, *, &, ==, !=,
       overlaps, contains, inv, divexact, charpoly, det, lu, lu!, solve,
       solve!, solve_lu_precomp, solve_lu_precomp!, swap_rows, swap_rows!,
       bound_inf_norm

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

parent_type(::Type{RealMat}) = RealMatSpace

elem_type(::Type{RealMatSpace}) = RealMat

base_ring(a::RealMatSpace) = RealField()

base_ring(a::RealMat) = RealField()

parent(x::RealMat) = matrix_space(base_ring(x), nrows(x), ncols(x))

dense_matrix_type(::Type{RealFieldElem}) = RealMat

function check_parent(x::RealMat, y::RealMat, throw::Bool = true)
   fl = (nrows(x) != nrows(y) || ncols(x) != ncols(y) || base_ring(x) != base_ring(y))
   fl && throw && error("Incompatible matrices")
   return !fl
end

function getindex!(z::arb, x::RealMat, r::Int, c::Int)
  GC.@preserve x begin
     v = ccall((:arb_mat_entry_ptr, libarb), Ptr{RealFieldElem},
                 (Ref{RealMat}, Int, Int), x, r - 1, c - 1)
     ccall((:arb_set, libarb), Nothing, (Ref{RealFieldElem}, Ptr{RealFieldElem}), z, v)
  end
  return z
end

@inline function getindex(x::RealMat, r::Int, c::Int)
  @boundscheck Generic._checkbounds(x, r, c)

  z = base_ring(x)()
  GC.@preserve x begin
     v = ccall((:arb_mat_entry_ptr, libarb), Ptr{RealFieldElem},
                 (Ref{RealMat}, Int, Int), x, r - 1, c - 1)
     ccall((:arb_set, libarb), Nothing, (Ref{RealFieldElem}, Ptr{RealFieldElem}), z, v)
  end
  return z
end

for T in [Int, UInt, ZZRingElem, QQFieldElem, Float64, BigFloat, RealFieldElem, AbstractString]
   @eval begin
      @inline function setindex!(x::RealMat, y::$T, r::Int, c::Int)
         @boundscheck Generic._checkbounds(x, r, c)

         GC.@preserve x begin
            z = ccall((:arb_mat_entry_ptr, libarb), Ptr{RealFieldElem},
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

zero(a::RealMatSpace) = a()

function one(x::RealMatSpace)
  z = x()
  ccall((:arb_mat_one, libarb), Nothing, (Ref{RealMat}, ), z)
  return z
end

nrows(a::RealMat) = a.r

ncols(a::RealMat) = a.c

nrows(a::RealMatSpace) = a.nrows

ncols(a::RealMatSpace) = a.ncols

function deepcopy_internal(x::RealMat, dict::IdDict)
  z = RealMat(nrows(x), ncols(x))
  ccall((:arb_mat_set, libarb), Nothing, (Ref{RealMat}, Ref{RealMat}), z, x)
  return z
end

################################################################################
#
#  Unary operations
#
################################################################################

function -(x::RealMat)
  z = similar(x)
  ccall((:arb_mat_neg, libarb), Nothing, (Ref{RealMat}, Ref{RealMat}), z, x)
  return z
end

################################################################################
#
#  Transpose
#
################################################################################

function transpose(x::RealMat)
  z = similar(x, ncols(x), nrows(x))
  ccall((:arb_mat_transpose, libarb), Nothing,
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
  ccall((:arb_mat_add, libarb), Nothing,
              (Ref{RealMat}, Ref{RealMat}, Ref{RealMat}, Int),
              z, x, y, precision(Balls))
  return z
end

function -(x::RealMat, y::RealMat)
  check_parent(x, y)
  z = similar(x)
  ccall((:arb_mat_sub, libarb), Nothing,
              (Ref{RealMat}, Ref{RealMat}, Ref{RealMat}, Int),
              z, x, y, precision(Balls))
  return z
end

function *(x::RealMat, y::RealMat)
  ncols(x) != nrows(y) && error("Matrices have wrong dimensions")
  z = similar(x, nrows(x), ncols(y))
  ccall((:arb_mat_mul, libarb), Nothing,
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
  ccall((:arb_mat_pow_ui, libarb), Nothing,
              (Ref{RealMat}, Ref{RealMat}, UInt, Int),
              z, x, y, precision(Balls))
  return z
end

function *(x::RealMat, y::Int)
  z = similar(x)
  ccall((:arb_mat_scalar_mul_si, libarb), Nothing,
              (Ref{RealMat}, Ref{RealMat}, Int, Int),
              z, x, y, precision(Balls))
  return z
end

*(x::Int, y::RealMat) = y*x

*(x::RealMat, y::QQFieldElem) = x*base_ring(x)(y)

*(x::QQFieldElem, y::RealMat) = y*x

function *(x::RealMat, y::ZZRingElem)
  z = similar(x)
  ccall((:arb_mat_scalar_mul_fmpz, libarb), Nothing,
              (Ref{RealMat}, Ref{RealMat}, Ref{ZZRingElem}, Int),
              z, x, y, precision(Balls))
  return z
end

*(x::ZZRingElem, y::RealMat) = y*x

function *(x::RealMat, y::arb)
  z = similar(x)
  ccall((:arb_mat_scalar_mul_arb, libarb), Nothing,
              (Ref{RealMat}, Ref{RealMat}, Ref{RealFieldElem}, Int),
              z, x, y, precision(Balls))
  return z
end

*(x::arb, y::RealMat) = y*x

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
  ccall((:arb_mat_scalar_mul_2exp_si, libarb), Nothing,
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
  r = ccall((:arb_mat_equal, libarb), Cint,
              (Ref{RealMat}, Ref{RealMat}), x, y)
  return Bool(r)
end

function ==(x::RealMat, y::RealMat)
  fl = check_parent(x, y, false)
  !fl && return false
  r = ccall((:arb_mat_eq, libarb), Cint, (Ref{RealMat}, Ref{RealMat}), x, y)
  return Bool(r)
end

function !=(x::RealMat, y::RealMat)
  r = ccall((:arb_mat_ne, libarb), Cint, (Ref{RealMat}, Ref{RealMat}), x, y)
  return Bool(r)
end

@doc raw"""
    overlaps(x::RealMat, y::RealMat)

Returns `true` if all entries of $x$ overlap with the corresponding entry of
$y$, otherwise return `false`.
"""
function overlaps(x::RealMat, y::RealMat)
  r = ccall((:arb_mat_overlaps, libarb), Cint,
              (Ref{RealMat}, Ref{RealMat}), x, y)
  return Bool(r)
end

@doc raw"""
    contains(x::RealMat, y::RealMat)

Returns `true` if all entries of $x$ contain the corresponding entry of
$y$, otherwise return `false`.
"""
function contains(x::RealMat, y::RealMat)
  r = ccall((:arb_mat_contains, libarb), Cint,
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
  r = ccall((:arb_mat_contains_fmpz_mat, libarb), Cint,
              (Ref{RealMat}, Ref{ZZMatrix}), x, y)
  return Bool(r)
end


@doc raw"""
    contains(x::RealMat, y::QQMatrix)

Returns `true` if all entries of $x$ contain the corresponding entry of
$y$, otherwise return `false`.
"""
function contains(x::RealMat, y::QQMatrix)
  r = ccall((:arb_mat_contains_fmpq_mat, libarb), Cint,
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

Given a  $n\times n$ matrix of type `arb_mat`, return an
$n\times n$ matrix $X$ such that $AX$ contains the
identity matrix. If $A$ cannot be inverted numerically an exception is raised.
"""
function inv(x::RealMat)
  ncols(x) != nrows(x) && error("Matrix must be square")
  z = similar(x)
  r = ccall((:arb_mat_inv, libarb), Cint,
              (Ref{RealMat}, Ref{RealMat}, Int), z, x, precision(Balls))
  Bool(r) ? (return z) : error("Matrix cannot be inverted numerically")
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
  ccall((:arb_mat_scalar_div_si, libarb), Nothing,
              (Ref{RealMat}, Ref{RealMat}, Int, Int),
              z, x, y, precision(Balls))
  return z
end

function divexact(x::RealMat, y::ZZRingElem; check::Bool=true)
  z = similar(x)
  ccall((:arb_mat_scalar_div_fmpz, libarb), Nothing,
              (Ref{RealMat}, Ref{RealMat}, Ref{ZZRingElem}, Int),
              z, x, y, precision(Balls))
  return z
end

function divexact(x::RealMat, y::arb; check::Bool=true)
  z = similar(x)
  ccall((:arb_mat_scalar_div_arb, libarb), Nothing,
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
  ccall((:arb_mat_charpoly, libarb), Nothing,
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
  ccall((:arb_mat_det, libarb), Nothing,
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
  ccall((:arb_mat_exp, libarb), Nothing,
              (Ref{RealMat}, Ref{RealMat}, Int), z, x, precision(Balls))
  return z
end

###############################################################################
#
#   Linear solving
#
###############################################################################

function lu!(P::Generic.Perm, x::RealMat)
  ncols(x) != nrows(x) && error("Matrix must be square")
  parent(P).n != nrows(x) && error("Permutation does not match matrix")
  P.d .-= 1
  r = ccall((:arb_mat_lu, libarb), Cint,
              (Ptr{Int}, Ref{RealMat}, Ref{RealMat}, Int),
              P.d, x, x, precision(Balls))
  r == 0 && error("Could not find $(nrows(x)) invertible pivot elements")
  P.d .+= 1
  inv!(P)
  return nrows(x)
end

function lu(x::RealMat, P = SymmetricGroup(nrows(x)))
  p = one(P)
  R = base_ring(x)
  L = similar(x)
  U = deepcopy(x)
  n = ncols(x)
  r = lu!(p, U)
  for i = 1:n
    for j = 1:n
      if i > j
        L[i, j] = U[i, j]
        U[i, j] = R()
      elseif i == j
        L[i, j] = one(R)
      else
        L[i, j] = R()
      end
    end
  end
  return r, p, L, U
end

function solve!(z::RealMat, x::RealMat, y::RealMat)
  r = ccall((:arb_mat_solve, libarb), Cint,
              (Ref{RealMat}, Ref{RealMat}, Ref{RealMat}, Int),
              z, x, y, precision(Balls))
  r == 0 && error("Matrix cannot be inverted numerically")
  nothing
end

function solve(x::RealMat, y::RealMat)
  ncols(x) != nrows(x) && error("First argument must be square")
  ncols(x) != nrows(y) && error("Matrix dimensions are wrong")
  z = similar(y)
  solve!(z, x, y)
  return z
end

function solve_lu_precomp!(z::RealMat, P::Generic.Perm, LU::RealMat, y::RealMat)
  Q = inv(P)
  ccall((:arb_mat_solve_lu_precomp, libarb), Nothing,
              (Ref{RealMat}, Ptr{Int}, Ref{RealMat}, Ref{RealMat}, Int),
              z, Q.d .- 1, LU, y, precision(Balls))
  nothing
end

function solve_lu_precomp(P::Generic.Perm, LU::RealMat, y::RealMat)
  ncols(LU) != nrows(y) && error("Matrix dimensions are wrong")
  z = similar(y)
  solve_lu_precomp!(z, P, LU, y)
  return z
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
  ccall((:arb_mat_swap_rows, libarb), Nothing,
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

Returns a nonnegative element $z$ of type `arb`, such that $z$ is an upper
bound for the infinity norm for every matrix in $x$
"""
function bound_inf_norm(x::RealMat)
  z = RealFieldElem()
  GC.@preserve x z begin
     t = ccall((:arb_rad_ptr, libarb), Ptr{mag_struct}, (Ref{RealFieldElem}, ), z)
     ccall((:arb_mat_bound_inf_norm, libarb), Nothing,
                 (Ptr{mag_struct}, Ref{RealMat}), t, x)
     s = ccall((:arb_mid_ptr, libarb), Ptr{arf_struct}, (Ref{RealFieldElem}, ), z)
     ccall((:arf_set_mag, libarb), Nothing,
                 (Ptr{arf_struct}, Ptr{mag_struct}), s, t)
     ccall((:mag_zero, libarb), Nothing,
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
      ccall(($f, libarb), Nothing,
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
   ccall((:arb_mat_one, libarb), Nothing, (Ref{RealMat}, ), z)
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

###############################################################################
#
#   matrix_space constructor
#
###############################################################################

function matrix_space(R::RealField, r::Int, c::Int; cached = true)
  # TODO/FIXME: `cached` is ignored and only exists for backwards compatibility
  (r <= 0 || c <= 0) && error("Dimensions must be positive")
  return RealMatSpace(R, r, c)
end
