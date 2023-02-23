###############################################################################
#
#   acb_mat.jl : Arb matrices over acb
#
###############################################################################

export zero, one, deepcopy, -, transpose, +, *, &, ==, !=,
       overlaps, contains, inv, divexact, charpoly, det, lu, lu!, solve,
       solve!, solve_lu_precomp, solve_lu_precomp!, swap_rows, swap_rows!,
       bound_inf_norm, isreal, eigvals, eigvals_simple

###############################################################################
#
#   Similar & zero
#
###############################################################################

function similar(::ComplexMat, R::ComplexField, r::Int, c::Int)
   z = ComplexMat(r, c)
   return z
end

zero(m::ComplexMat, R::ComplexField, r::Int, c::Int) = similar(m, R, r, c)

###############################################################################
#
#   Basic manipulation
#
###############################################################################

parent_type(::Type{ComplexMat}) = ComplexMatSpace

elem_type(::Type{ComplexMatSpace}) = ComplexMat

parent(x::ComplexMat, cached::Bool = true) =
      matrix_space(base_ring(x), nrows(x), ncols(x))

dense_matrix_type(::Type{ComplexFieldElem}) = ComplexMat

base_ring(a::ComplexMatSpace) = ComplexField()

base_ring(a::ComplexMat) = ComplexField()

function check_parent(x::ComplexMat, y::ComplexMat, throw::Bool = true)
   fl = (nrows(x) != nrows(y) || ncols(x) != ncols(y) || base_ring(x) != base_ring(y))
   fl && throw && error("Incompatible matrices")
   return !fl
end

function getindex!(z::ComplexFieldElem, x::ComplexMat, r::Int, c::Int)
  GC.@preserve x begin
    v = ccall((:acb_mat_entry_ptr, libarb), Ptr{ComplexFieldElem},
                (Ref{ComplexMat}, Int, Int), x, r - 1, c - 1)
    ccall((:acb_set, libarb), Nothing, (Ref{ComplexFieldElem}, Ptr{ComplexFieldElem}), z, v)
  end
  return z
end

@inline function getindex(x::ComplexMat, r::Int, c::Int)
  @boundscheck Generic._checkbounds(x, r, c)

  z = base_ring(x)()
  GC.@preserve x begin
     v = ccall((:acb_mat_entry_ptr, libarb), Ptr{ComplexFieldElem},
               (Ref{ComplexMat}, Int, Int), x, r - 1, c - 1)
     ccall((:acb_set, libarb), Nothing, (Ref{ComplexFieldElem}, Ptr{ComplexFieldElem}), z, v)
  end
  return z
end

for T in [Integer, Float64, ZZRingElem, QQFieldElem, RealFieldElem, BigFloat, ComplexFieldElem, AbstractString]
   @eval begin
      @inline function setindex!(x::ComplexMat, y::$T, r::Int, c::Int)
         @boundscheck Generic._checkbounds(x, r, c)

         GC.@preserve x begin
            z = ccall((:acb_mat_entry_ptr, libarb), Ptr{ComplexFieldElem},
                      (Ref{ComplexMat}, Int, Int), x, r - 1, c - 1)
            _acb_set(z, y, precision(Balls))
         end
      end
   end
end

Base.@propagate_inbounds setindex!(x::ComplexMat, y::Rational{T},
                                   r::Int, c::Int) where {T <: Integer} =
         setindex!(x, QQFieldElem(y), r, c)

for T in [Integer, Float64, ZZRingElem, QQFieldElem, RealFieldElem, BigFloat, AbstractString]
   @eval begin
      @inline function setindex!(x::ComplexMat, y::Tuple{$T, $T}, r::Int, c::Int)
         @boundscheck Generic._checkbounds(x, r, c)

         GC.@preserve x begin
            z = ccall((:acb_mat_entry_ptr, libarb), Ptr{ComplexFieldElem},
                      (Ref{ComplexMat}, Int, Int), x, r - 1, c - 1)
            _acb_set(z, y[1], y[2], precision(Balls))
         end
      end
   end
end

setindex!(x::ComplexMat, y::Tuple{Rational{T}, Rational{T}}, r::Int, c::Int) where {T <: Integer} =
         setindex!(x, map(QQFieldElem, y), r, c)

zero(x::ComplexMatSpace) = x()

function one(x::ComplexMatSpace)
  z = x()
  ccall((:acb_mat_one, libarb), Nothing, (Ref{ComplexMat}, ), z)
  return z
end

nrows(a::ComplexMat) = a.r

ncols(a::ComplexMat) = a.c

nrows(a::ComplexMatSpace) = a.nrows

ncols(a::ComplexMatSpace) = a.ncols

function deepcopy_internal(x::ComplexMat, dict::IdDict)
  z = similar(x)
  ccall((:acb_mat_set, libarb), Nothing, (Ref{ComplexMat}, Ref{ComplexMat}), z, x)
  return z
end

################################################################################
#
#  Unary operations
#
################################################################################

function -(x::ComplexMat)
  z = similar(x)
  ccall((:acb_mat_neg, libarb), Nothing, (Ref{ComplexMat}, Ref{ComplexMat}), z, x)
  return z
end

################################################################################
#
#  Transpose
#
################################################################################

function transpose(x::ComplexMat)
  z = similar(x, ncols(x), nrows(x))
  ccall((:acb_mat_transpose, libarb), Nothing,
              (Ref{ComplexMat}, Ref{ComplexMat}), z, x)
  return z
end

################################################################################
#
#  Binary operations
#
################################################################################

function +(x::ComplexMat, y::ComplexMat)
  check_parent(x, y)
  z = similar(x)
  ccall((:acb_mat_add, libarb), Nothing,
              (Ref{ComplexMat}, Ref{ComplexMat}, Ref{ComplexMat}, Int),
              z, x, y, precision(Balls))
  return z
end

function -(x::ComplexMat, y::ComplexMat)
  check_parent(x, y)
  z = similar(x)
  ccall((:acb_mat_sub, libarb), Nothing,
              (Ref{ComplexMat}, Ref{ComplexMat}, Ref{ComplexMat}, Int),
              z, x, y, precision(Balls))
  return z
end

function *(x::ComplexMat, y::ComplexMat)
  ncols(x) != nrows(y) && error("Matrices have wrong dimensions")
  z = similar(x, nrows(x), ncols(y))
  ccall((:acb_mat_mul, libarb), Nothing,
              (Ref{ComplexMat}, Ref{ComplexMat}, Ref{ComplexMat}, Int),
              z, x, y, precision(Balls))
  return z
end

################################################################################
#
#   Ad hoc binary operators
#
################################################################################

function ^(x::ComplexMat, y::UInt)
  nrows(x) != ncols(x) && error("Matrix must be square")
  z = similar(x)
  ccall((:acb_mat_pow_ui, libarb), Nothing,
              (Ref{ComplexMat}, Ref{ComplexMat}, UInt, Int),
              z, x, y, precision(Balls))
  return z
end

function *(x::ComplexMat, y::Int)
  z = similar(x)
  ccall((:acb_mat_scalar_mul_si, libarb), Nothing,
              (Ref{ComplexMat}, Ref{ComplexMat}, Int, Int),
              z, x, y, precision(Balls))
  return z
end

*(x::Int, y::ComplexMat) = y*x

function *(x::ComplexMat, y::ZZRingElem)
  z = similar(x)
  ccall((:acb_mat_scalar_mul_fmpz, libarb), Nothing,
              (Ref{ComplexMat}, Ref{ComplexMat}, Ref{ZZRingElem}, Int),
              z, x, y, precision(Balls))
  return z
end

*(x::ZZRingElem, y::ComplexMat) = y*x

function *(x::ComplexMat, y::RealFieldElem)
  z = similar(x)
  ccall((:acb_mat_scalar_mul_arb, libarb), Nothing,
              (Ref{ComplexMat}, Ref{ComplexMat}, Ref{RealFieldElem}, Int),
              z, x, y, precision(Balls))
  return z
end

*(x::RealFieldElem, y::ComplexMat) = y*x

function *(x::ComplexMat, y::ComplexFieldElem)
  z = similar(x)
  ccall((:acb_mat_scalar_mul_acb, libarb), Nothing,
              (Ref{ComplexMat}, Ref{ComplexMat}, Ref{ComplexFieldElem}, Int),
              z, x, y, precision(Balls))
  return z
end

*(x::ComplexFieldElem, y::ComplexMat) = y*x

*(x::Integer, y::ComplexMat) = ZZRingElem(x) * y

*(x::ComplexMat, y::Integer) = y * x

*(x::QQFieldElem, y::ComplexMat) = base_ring(y)(x) * y

*(x::ComplexMat, y::QQFieldElem) = y * x

*(x::Float64, y::ComplexMat) = base_ring(y)(x) * y

*(x::ComplexMat, y::Float64) = y * x

*(x::BigFloat, y::ComplexMat) = base_ring(y)(x) * y

*(x::ComplexMat, y::BigFloat) = y * x

*(x::Rational{T}, y::ComplexMat) where T <: Union{Int, BigInt} = QQFieldElem(x) * y

*(x::ComplexMat, y::Rational{T}) where T <: Union{Int, BigInt} = y * x

for T in [Integer, ZZRingElem, QQFieldElem, RealFieldElem, ComplexFieldElem]
   @eval begin
      function +(x::ComplexMat, y::$T)
         z = deepcopy(x)
         for i = 1:min(nrows(x), ncols(x))
            z[i, i] += y
         end
         return z
      end

      +(x::$T, y::ComplexMat) = y + x

      function -(x::ComplexMat, y::$T)
         z = deepcopy(x)
         for i = 1:min(nrows(x), ncols(x))
            z[i, i] -= y
         end
         return z
      end

      function -(x::$T, y::ComplexMat)
         z = -y
         for i = 1:min(nrows(y), ncols(y))
            z[i, i] += x
         end
         return z
      end
   end
end

function +(x::ComplexMat, y::Rational{T}) where T <: Union{Int, BigInt}
   z = deepcopy(x)
   for i = 1:min(nrows(x), ncols(x))
      z[i, i] += y
   end
   return z
end

+(x::Rational{T}, y::ComplexMat) where T <: Union{Int, BigInt} = y + x

function -(x::ComplexMat, y::Rational{T}) where T <: Union{Int, BigInt}
   z = deepcopy(x)
   for i = 1:min(nrows(x), ncols(x))
      z[i, i] -= y
   end
   return z
end

function -(x::Rational{T}, y::ComplexMat) where T <: Union{Int, BigInt}
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

function ldexp(x::ComplexMat, y::Int)
  z = similar(x)
  ccall((:acb_mat_scalar_mul_2exp_si, libarb), Nothing,
              (Ref{ComplexMat}, Ref{ComplexMat}, Int), z, x, y)
  return z
end

###############################################################################
#
#   Comparisons
#
###############################################################################

@doc Markdown.doc"""
    isequal(x::ComplexMat, y::ComplexMat)

Return `true` if the matrices of balls $x$ and $y$ are precisely equal,
i.e. if all matrix entries have the same midpoints and radii.
"""
function isequal(x::ComplexMat, y::ComplexMat)
  r = ccall((:acb_mat_equal, libarb), Cint,
              (Ref{ComplexMat}, Ref{ComplexMat}), x, y)
  return Bool(r)
end

function ==(x::ComplexMat, y::ComplexMat)
  fl = check_parent(x, y, false)
  !fl && return false
  r = ccall((:acb_mat_eq, libarb), Cint, (Ref{ComplexMat}, Ref{ComplexMat}), x, y)
  return Bool(r)
end

function !=(x::ComplexMat, y::ComplexMat)
  r = ccall((:acb_mat_ne, libarb), Cint, (Ref{ComplexMat}, Ref{ComplexMat}), x, y)
  return Bool(r)
end

@doc Markdown.doc"""
    overlaps(x::ComplexMat, y::ComplexMat)

Returns `true` if all entries of $x$ overlap with the corresponding entry of
$y$, otherwise return `false`.
"""
function overlaps(x::ComplexMat, y::ComplexMat)
  r = ccall((:acb_mat_overlaps, libarb), Cint,
              (Ref{ComplexMat}, Ref{ComplexMat}), x, y)
  return Bool(r)
end

@doc Markdown.doc"""
    contains(x::ComplexMat, y::ComplexMat)

Returns `true` if all entries of $x$ contain the corresponding entry of
$y$, otherwise return `false`.
"""
function contains(x::ComplexMat, y::ComplexMat)
  r = ccall((:acb_mat_contains, libarb), Cint,
              (Ref{ComplexMat}, Ref{ComplexMat}), x, y)
  return Bool(r)
end

################################################################################
#
#  Ad hoc comparisons
#
################################################################################

@doc Markdown.doc"""
    contains(x::ComplexMat, y::ZZMatrix)

Returns `true` if all entries of $x$ contain the corresponding entry of
$y$, otherwise return `false`.
"""
function contains(x::ComplexMat, y::ZZMatrix)
  r = ccall((:acb_mat_contains_fmpz_mat, libarb), Cint,
              (Ref{ComplexMat}, Ref{ZZMatrix}), x, y)
  return Bool(r)
end

@doc Markdown.doc"""
    contains(x::ComplexMat, y::QQMatrix)

Returns `true` if all entries of $x$ contain the corresponding entry of
$y$, otherwise return `false`.
"""
function contains(x::ComplexMat, y::QQMatrix)
  r = ccall((:acb_mat_contains_fmpq_mat, libarb), Cint,
              (Ref{ComplexMat}, Ref{QQMatrix}), x, y)
  return Bool(r)
end

==(x::ComplexMat, y::ZZMatrix) = x == parent(x)(y)

==(x::ZZMatrix, y::ComplexMat) = y == x

==(x::ComplexMat, y::RealMat) = x == parent(x)(y)

==(x::RealMat, y::ComplexMat) = y == x

################################################################################
#
#  Predicates
#
################################################################################

isreal(x::ComplexMat) =
            Bool(ccall((:acb_mat_is_real, libarb), Cint, (Ref{ComplexMat}, ), x))

###############################################################################
#
#   Inversion
#
###############################################################################

@doc Markdown.doc"""
    inv(x::ComplexMat)

Given a $n\times n$ matrix of type `acb_mat`, return an
$n\times n$ matrix $X$ such that $AX$ contains the
identity matrix. If $A$ cannot be inverted numerically an exception is raised.
"""
function inv(x::ComplexMat, prec = precision(Balls))
  ncols(x) != nrows(x) && error("Matrix must be square")
  z = similar(x)
  r = ccall((:acb_mat_inv, libarb), Cint,
              (Ref{ComplexMat}, Ref{ComplexMat}, Int), z, x, prec)
  Bool(r) ? (return z) : error("Matrix cannot be inverted numerically")
end

###############################################################################
#
#   Exact division
#
###############################################################################

function divexact(x::ComplexMat, y::ComplexMat; check::Bool=true)
   ncols(x) != ncols(y) && error("Incompatible matrix dimensions")
   x*inv(y)
end

###############################################################################
#
#   Ad hoc exact division
#
###############################################################################

function divexact(x::ComplexMat, y::Int; check::Bool=true)
  y == 0 && throw(DivideError())
  z = similar(x)
  ccall((:acb_mat_scalar_div_si, libarb), Nothing,
              (Ref{ComplexMat}, Ref{ComplexMat}, Int, Int),
              z, x, y, precision(Balls))
  return z
end

function divexact(x::ComplexMat, y::ZZRingElem; check::Bool=true)
  z = similar(x)
  ccall((:acb_mat_scalar_div_fmpz, libarb), Nothing,
              (Ref{ComplexMat}, Ref{ComplexMat}, Ref{ZZRingElem}, Int),
              z, x, y, precision(Balls))
  return z
end

function divexact(x::ComplexMat, y::RealFieldElem; check::Bool=true)
  z = similar(x)
  ccall((:acb_mat_scalar_div_arb, libarb), Nothing,
              (Ref{ComplexMat}, Ref{ComplexMat}, Ref{RealFieldElem}, Int),
              z, x, y, precision(Balls))
  return z
end

function divexact(x::ComplexMat, y::ComplexFieldElem; check::Bool=true)
  z = similar(x)
  ccall((:acb_mat_scalar_div_acb, libarb), Nothing,
              (Ref{ComplexMat}, Ref{ComplexMat}, Ref{ComplexFieldElem}, Int),
              z, x, y, precision(Balls))
  return z
end

divexact(x::ComplexMat, y::Float64; check::Bool=true) = divexact(x, base_ring(x)(y); check=check)

divexact(x::ComplexMat, y::BigFloat; check::Bool=true) = divexact(x, base_ring(x)(y); check=check)

divexact(x::ComplexMat, y::Integer; check::Bool=true) = divexact(x, ZZRingElem(y); check=check)

divexact(x::ComplexMat, y::Rational{T}; check::Bool=true) where T <: Union{Int, BigInt} = divexact(x, QQFieldElem(y); check=check)

################################################################################
#
#  Characteristic polynomial
#
################################################################################

function charpoly(x::AcbPolyRing, y::ComplexMat, prec = precision(Balls))
  base_ring(x) != base_ring(y) && error("Base rings must coincide")
  z = x()
  ccall((:acb_mat_charpoly, libarb), Nothing,
              (Ref{acb_poly}, Ref{ComplexMat}, Int), z, y, prec)
  return z
end

################################################################################
#
#  Determinant
#
################################################################################

function det(x::ComplexMat, prec = precision(Balls))
  ncols(x) != nrows(x) && error("Matrix must be square")
  z = base_ring(x)()
  ccall((:acb_mat_det, libarb), Nothing,
              (Ref{ComplexFieldElem}, Ref{ComplexMat}, Int), z, x, prec)
  return z
end

################################################################################
#
#  Exponential function
#
################################################################################

function Base.exp(x::ComplexMat)
  ncols(x) != nrows(x) && error("Matrix must be square")
  z = similar(x)
  ccall((:acb_mat_exp, libarb), Nothing,
              (Ref{ComplexMat}, Ref{ComplexMat}, Int), z, x, precision(Balls))
  return z
end

###############################################################################
#
#   Linear solving
#
###############################################################################

function lu!(P::Generic.Perm, x::ComplexMat)
  P.d .-= 1
  r = ccall((:acb_mat_lu, libarb), Cint,
              (Ptr{Int}, Ref{ComplexMat}, Ref{ComplexMat}, Int),
              P.d, x, x, precision(Balls))
  r == 0 && error("Could not find $(nrows(x)) invertible pivot elements")
  P.d .+= 1
  inv!(P)
  return nrows(x)
end

function lu(P::Generic.Perm, x::ComplexMat)
  ncols(x) != nrows(x) && error("Matrix must be square")
  parent(P).n != nrows(x) && error("Permutation does not match matrix")
  R = base_ring(x)
  L = similar(x)
  U = deepcopy(x)
  n = ncols(x)
  lu!(P, U)
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
  return L, U
end

function solve!(z::ComplexMat, x::ComplexMat, y::ComplexMat)
  r = ccall((:acb_mat_solve, libarb), Cint,
              (Ref{ComplexMat}, Ref{ComplexMat}, Ref{ComplexMat}, Int),
              z, x, y, precision(Balls))
  r == 0 && error("Matrix cannot be inverted numerically")
  nothing
end

function solve(x::ComplexMat, y::ComplexMat)
  ncols(x) != nrows(x) && error("First argument must be square")
  ncols(x) != nrows(y) && error("Matrix dimensions are wrong")
  z = similar(y)
  solve!(z, x, y)
  return z
end

function solve_lu_precomp!(z::ComplexMat, P::Generic.Perm, LU::ComplexMat, y::ComplexMat)
  Q = inv(P)
  ccall((:acb_mat_solve_lu_precomp, libarb), Nothing,
              (Ref{ComplexMat}, Ptr{Int}, Ref{ComplexMat}, Ref{ComplexMat}, Int),
              z, Q.d .- 1, LU, y, precision(Balls))
  nothing
end

function solve_lu_precomp(P::Generic.Perm, LU::ComplexMat, y::ComplexMat)
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

function swap_rows(x::ComplexMat, i::Int, j::Int)
  Generic._checkbounds(nrows(x), i) || throw(BoundsError())
  Generic._checkbounds(nrows(x), j) || throw(BoundsError())
  z = deepcopy(x)
  swap_rows!(z, i, j)
  return z
end

function swap_rows!(x::ComplexMat, i::Int, j::Int)
  ccall((:acb_mat_swap_rows, libarb), Nothing,
              (Ref{ComplexMat}, Ptr{Nothing}, Int, Int),
              x, C_NULL, i - 1, j - 1)
end

################################################################################
#
#   Norm
#
################################################################################

@doc Markdown.doc"""
    bound_inf_norm(x::ComplexMat)

Returns a nonnegative element $z$ of type `acb`, such that $z$ is an upper
bound for the infinity norm for every matrix in $x$
"""
function bound_inf_norm(x::ComplexMat)
  z = RealFieldElem()
  GC.@preserve x z begin
     t = ccall((:arb_rad_ptr, libarb), Ptr{mag_struct}, (Ref{RealFieldElem}, ), z)
     ccall((:acb_mat_bound_inf_norm, libarb), Nothing,
                 (Ptr{mag_struct}, Ref{ComplexMat}), t, x)
     s = ccall((:arb_mid_ptr, libarb), Ptr{arf_struct}, (Ref{RealFieldElem}, ), z)
     ccall((:arf_set_mag, libarb), Nothing,
                 (Ptr{arf_struct}, Ptr{mag_struct}), s, t)
     ccall((:mag_zero, libarb), Nothing,
                 (Ptr{mag_struct},), t)
  end
  return z
end

################################################################################
#
#   Unsafe functions
#
################################################################################

for (s,f) in (("add!","acb_mat_add"), ("mul!","acb_mat_mul"),
              ("sub!","acb_mat_sub"))
  @eval begin
    function ($(Symbol(s)))(z::ComplexMat, x::ComplexMat, y::ComplexMat, prec = precision(Balls))
      ccall(($f, libarb), Nothing,
                  (Ref{ComplexMat}, Ref{ComplexMat}, Ref{ComplexMat}, Int),
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

function (x::ComplexMatSpace)()
  z = ComplexMat(nrows(x), ncols(x))
  return z
end

function (x::ComplexMatSpace)(y::ZZMatrix)
  (ncols(x) != ncols(y) || nrows(x) != nrows(y)) &&
      error("Dimensions are wrong")
  z = ComplexMat(y, precision(Balls))
  return z
end

function (x::ComplexMatSpace)(y::RealMat)
  (ncols(x) != ncols(y) || nrows(x) != nrows(y)) &&
      error("Dimensions are wrong")
  z = ComplexMat(y, precision(Balls))
  return z
end

for T in [Float64, ZZRingElem, QQFieldElem, BigFloat, RealFieldElem, ComplexFieldElem, String]
   @eval begin
      function (x::ComplexMatSpace)(y::AbstractMatrix{$T})
         _check_dim(nrows(x), ncols(x), y)
         z = ComplexMat(nrows(x), ncols(x), y, precision(Balls))
         return z
      end

      function (x::ComplexMatSpace)(y::AbstractVector{$T})
         _check_dim(nrows(x), ncols(x), y)
         z = ComplexMat(nrows(x), ncols(x), y, precision(Balls))
         return z
      end
   end
end

(x::ComplexMatSpace)(y::AbstractMatrix{T}) where {T <: Integer} = x(map(ZZRingElem, y))

(x::ComplexMatSpace)(y::AbstractVector{T}) where {T <: Integer} = x(map(ZZRingElem, y))

(x::ComplexMatSpace)(y::AbstractMatrix{Rational{T}}) where {T <: Integer} = x(map(QQFieldElem, y))

(x::ComplexMatSpace)(y::AbstractVector{Rational{T}}) where {T <: Integer} = x(map(QQFieldElem, y))

for T in [Float64, ZZRingElem, QQFieldElem, BigFloat, RealFieldElem, String]
   @eval begin
      function (x::ComplexMatSpace)(y::AbstractMatrix{Tuple{$T, $T}})
         _check_dim(nrows(x), ncols(x), y)
         z = ComplexMat(nrows(x), ncols(x), y, precision(Balls))
         return z
      end

      function (x::ComplexMatSpace)(y::AbstractVector{Tuple{$T, $T}})
         _check_dim(nrows(x), ncols(x), y)
         z = ComplexMat(nrows(x), ncols(x), y, precision(Balls))
         return z
      end
   end
end

(x::ComplexMatSpace)(y::AbstractMatrix{Tuple{T, T}}) where {T <: Integer} =
         x(map(z -> (ZZRingElem(z[1]), ZZRingElem(z[2])), y))

(x::ComplexMatSpace)(y::AbstractVector{Tuple{T, T}}) where {T <: Integer} =
         x(map(z -> (ZZRingElem(z[1]), ZZRingElem(z[2])), y))

(x::ComplexMatSpace)(y::AbstractMatrix{Tuple{Rational{T}, Rational{T}}}) where {T <: Integer} =
         x(map(z -> (QQFieldElem(z[1]), QQFieldElem(z[2])), y))

(x::ComplexMatSpace)(y::AbstractVector{Tuple{Rational{T}, Rational{T}}}) where {T <: Integer} =
         x(map(z -> (QQFieldElem(z[1]), QQFieldElem(z[2])), y))

for T in [Integer, ZZRingElem, QQFieldElem, Float64, BigFloat, RealFieldElem, ComplexFieldElem, String]
   @eval begin
      function (x::ComplexMatSpace)(y::$T)
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

(x::ComplexMatSpace)(y::Rational{T}) where {T <: Integer} = x(QQFieldElem(y))

(x::ComplexMatSpace)(y::ComplexMat) = y

###############################################################################
#
#   Matrix constructor
#
###############################################################################

function matrix(R::ComplexField, arr::AbstractMatrix{T}) where {T <: Union{Int, UInt, ZZRingElem, QQFieldElem, Float64, BigFloat, RealFieldElem, ComplexFieldElem, AbstractString}}
   z = ComplexMat(size(arr, 1), size(arr, 2), arr, precision(Balls))
   return z
end

function matrix(R::ComplexField, r::Int, c::Int, arr::AbstractVector{T}) where {T <: Union{Int, UInt, ZZRingElem, QQFieldElem, Float64, BigFloat, RealFieldElem, ComplexFieldElem, AbstractString}}
   _check_dim(r, c, arr)
   z = ComplexMat(r, c, arr, precision(Balls))
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
#  Zero matrix
#
###############################################################################

function zero_matrix(R::ComplexField, r::Int, c::Int)
   if r < 0 || c < 0
     error("dimensions must not be negative")
   end
   z = ComplexMat(r, c)
   return z
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
   z = ComplexMat(n, n)
   ccall((:acb_mat_one, libarb), Nothing, (Ref{ComplexMat}, ), z)
   return z
end

###############################################################################
#
#   Promotions
#
###############################################################################

promote_rule(::Type{ComplexMat}, ::Type{T}) where {T <: Integer} = ComplexMat

promote_rule(::Type{ComplexMat}, ::Type{Rational{T}}) where T <: Union{Int, BigInt} = ComplexMat

promote_rule(::Type{ComplexMat}, ::Type{ZZRingElem}) = ComplexMat

promote_rule(::Type{ComplexMat}, ::Type{QQFieldElem}) = ComplexMat

promote_rule(::Type{ComplexMat}, ::Type{arb}) = ComplexMat

promote_rule(::Type{ComplexMat}, ::Type{ComplexFieldElem}) = ComplexMat

promote_rule(::Type{ComplexMat}, ::Type{ZZMatrix}) = ComplexMat

promote_rule(::Type{ComplexMat}, ::Type{QQMatrix}) = ComplexMat

promote_rule(::Type{ComplexMat}, ::Type{arb_mat}) = ComplexMat

###############################################################################
#
#   Eigenvalues and eigenvectors
#
###############################################################################

function __approx_eig_qr!(v::Ptr{acb_struct}, R::ComplexMat, A::ComplexMat)
  n = nrows(A)
  ccall((:acb_mat_approx_eig_qr, libarb), Cint,
        (Ptr{acb_struct}, Ptr{Nothing}, Ref{ComplexMat},
        Ref{ComplexMat}, Ptr{Nothing}, Int, Int),
        v, C_NULL, R, A, C_NULL, 0, precision(Balls))
  return nothing
end

function _approx_eig_qr(A::ComplexMat)
  n = nrows(A)
  v = acb_vec(n)
  R = zero_matrix(base_ring(A), ncols(A), nrows(A))
  __approx_eig_qr!(v, R, A)
  z = array(base_ring(A), v, n)
  acb_vec_clear(v, n)
  return z, R
end

function _eig_multiple(A::ComplexMat, check::Bool = true)
  n = nrows(A)
  v = acb_vec(n)
  v_approx = acb_vec(n)
  R = zero_matrix(base_ring(A), n, n)
  __approx_eig_qr!(v, R, A)
  b = ccall((:acb_mat_eig_multiple, libarb), Cint,
            (Ptr{acb_struct}, Ref{ComplexMat}, Ptr{acb_struct}, Ref{ComplexMat}, Int),
             v_approx, A, v, R, precision(Balls))
  check && b == 0 && throw(error("Could not isolate eigenvalues of matrix $A"))
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

function _eig_simple(A::ComplexMat; check::Bool = true, alg = :default)
  n = nrows(A)
  v = acb_vec(n)
  v_approx = acb_vec(n)
  Rapprox = zero_matrix(base_ring(A), n, n)
  L = zero_matrix(base_ring(A), n, n)
  R = zero_matrix(base_ring(A), n, n)
  __approx_eig_qr!(v, Rapprox, A)
  if alg == :vdhoeven_mourrain
      b = ccall((:acb_mat_eig_simple_vdhoeven_mourrain, libarb), Cint,
                (Ptr{acb_struct}, Ref{ComplexMat}, Ref{ComplexMat},
                 Ref{ComplexMat}, Ptr{acb_struct}, Ref{ComplexMat}, Int),
                 v_approx, L, R, A, v, Rapprox, precision(Balls))
  elseif alg == :rump
      b = ccall((:acb_mat_eig_simple_rump, libarb), Cint,
                (Ptr{acb_struct}, Ref{ComplexMat}, Ref{ComplexMat},
                 Ref{ComplexMat}, Ptr{acb_struct}, Ref{ComplexMat}, Int),
                 v_approx, L, R, A, v, Rapprox, precision(Balls))
  elseif alg == :default
      b = ccall((:acb_mat_eig_simple, libarb), Cint,
                (Ptr{acb_struct}, Ref{ComplexMat}, Ref{ComplexMat},
                 Ref{ComplexMat}, Ptr{acb_struct}, Ref{ComplexMat}, Int),
                 v_approx, L, R, A, v, Rapprox, precision(Balls))
  else
      throw(error("Algorithm $alg not supported"))
  end

  if check && b == 0
    if nrows(A) <= 10
      throw(error("Could not isolate eigenvalues of matrix $A"))
    else
      throw(error("Could not isolate eigenvalues"))
    end
  end
  z = array(base_ring(A), v, n)
  acb_vec_clear(v, n)
  acb_vec_clear(v_approx, n)

  return z, L, R
end

@doc Markdown.doc"""
    eigvals_simple(A::ComplexMat, alg = :default)

Returns the eigenvalues of `A` as a vector of `acb`. It is assumed that `A`
has only simple eigenvalues.

The algorithm used can be changed by setting the `alg` keyword to
`:vdhoeven_mourrain` or `:rump`.

This function is experimental.
"""
function eigvals_simple(A::ComplexMat, alg = :default)
  E, _, _ = _eig_simple(A, alg = alg)
  return E
end

@doc Markdown.doc"""
    eigvals(A::ComplexMat)

Returns the eigenvalues of `A` as a vector of tuples `(ComplexFieldElem, Int)`.
Each tuple `(z, k)` corresponds to a cluster of `k` eigenvalues
of $A$.

This function is experimental.
"""
function eigvals(A::ComplexMat)
  e, _ = _eig_multiple(A)
  return e
end

###############################################################################
#
#   matrix_space constructor
#
###############################################################################

function matrix_space(R::ComplexField, r::Int, c::Int; cached = true)
  (r <= 0 || c <= 0) && error("Dimensions must be positive")
  return ComplexMatSpace(R, r, c, cached)
end
