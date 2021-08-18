################################################################################
#
#  fq_default_mat.jl: flint fq_default_mat types in julia
#
################################################################################

export fq_default_mat, FqDefaultMatSpace

################################################################################
#
#  Data type and parent object methods
#
################################################################################

parent_type(::Type{fq_default_mat}) = FqDefaultMatSpace

elem_type(::Type{FqDefaultMatSpace}) = fq_default_mat

dense_matrix_type(::Type{fq_default}) = fq_default_mat

function check_parent(x::fq_default_mat, y::fq_default_mat, throw::Bool = true)
   fl = base_ring(x) != base_ring(y)
   fl && throw && error("Residue rings must be equal")
   fl && return false
   fl = (ncols(x) != ncols(y)) && (nrows(x) != nrows(y))
   fl && throw && error("Matrices have wrong dimensions")
   return !fl
end

###############################################################################
#
#   Similar & zero
#
###############################################################################

similar(::fq_default_mat, R::FqDefaultFiniteField, r::Int, c::Int) = fq_default_mat(r, c, R)
zero(m::fq_default_mat, R::FqDefaultFiniteField, r::Int, c::Int) = fq_default_mat(r, c, R)

################################################################################
#
#  Manipulation
#
################################################################################

@inline function getindex(a::fq_default_mat, i::Int, j::Int)
   @boundscheck Generic._checkbounds(a, i, j)
   z = base_ring(a)()
   ccall((:fq_default_mat_entry, libflint), Ptr{fq_default},
         (Ref{fq_default}, Ref{fq_default_mat}, Int, Int,
          Ref{FqDefaultFiniteField}),
          z, a, i - 1 , j - 1, base_ring(a))
   return z
end

@inline function setindex!(a::fq_default_mat, u::fq_default, i::Int, j::Int)
   @boundscheck Generic._checkbounds(a, i, j)
   ccall((:fq_default_mat_entry_set, libflint), Nothing,
         (Ref{fq_default_mat}, Int, Int, Ref{fq_default}, Ref{FqDefaultFiniteField}),
         a, i - 1, j - 1, u, base_ring(a))
   nothing
end

@inline function setindex!(a::fq_default_mat, u::fmpz, i::Int, j::Int)
   @boundscheck Generic._checkbounds(a, i, j)
   ccall((:fq_default_mat_entry_set_fmpz, libflint), Nothing,
         (Ref{fq_default_mat}, Int, Int, Ref{fmpz},
          Ref{FqDefaultFiniteField}),
          a, i - 1, j - 1, u, base_ring(a))
   nothing
end

setindex!(a::fq_default_mat, u::Integer, i::Int, j::Int) =
        setindex!(a, base_ring(a)(u), i, j)

function deepcopy_internal(a::fq_default_mat, dict::IdDict)
  z = fq_default_mat(nrows(a), ncols(a), base_ring(a))
  ccall((:fq_default_mat_set, libflint), Nothing,
        (Ref{fq_default_mat}, Ref{fq_default_mat}, Ref{FqDefaultFiniteField}), z, a, base_ring(a))
  return z
end

function nrows(a::fq_default_mat)
   return ccall((:fq_default_mat_nrows, libflint), Int,
   (Ref{fq_default_mat}, Ref{FqDefaultFiniteField}),
    a, base_ring(a))
end

function ncols(a::fq_default_mat)
   return ccall((:fq_default_mat_ncols, libflint), Int,
   (Ref{fq_default_mat}, Ref{FqDefaultFiniteField}),
    a, base_ring(a))
end

nrows(a::FqDefaultMatSpace) = a.nrows

ncols(a::FqDefaultMatSpace) = a.ncols

parent(a::fq_default_mat, cached::Bool = true) = FqDefaultMatSpace(base_ring(a), nrows(a), ncols(a), cached)

base_ring(a::FqDefaultMatSpace) = a.base_ring

base_ring(a::fq_default_mat) = a.base_ring

zero(a::FqDefaultMatSpace) = a()

function one(a::FqDefaultMatSpace)
  (nrows(a) != ncols(a)) && error("Matrices must be square")
  return a(one(base_ring(a)))
end

function iszero(a::fq_default_mat)
   r = ccall((:fq_default_mat_is_zero, libflint), Cint,
             (Ref{fq_default_mat}, Ref{FqDefaultFiniteField}), a, base_ring(a))
  return Bool(r)
end

################################################################################
#
#  Comparison
#
################################################################################

function ==(a::fq_default_mat, b::fq_default_mat)
   if !(a.base_ring == b.base_ring)
      return false
   end
   r = ccall((:fq_default_mat_equal, libflint), Cint,
             (Ref{fq_default_mat}, Ref{fq_default_mat}, Ref{FqDefaultFiniteField}), a, b, base_ring(a))
   return Bool(r)
end

isequal(a::fq_default_mat, b::fq_default_mat) = ==(a, b)

################################################################################
#
#  Transpose
#
################################################################################

function transpose(a::fq_default_mat)
   z = fq_default_mat(ncols(a), nrows(a), base_ring(a))
   for i in 1:nrows(a)
      for j in 1:ncols(a)
         z[j, i] = a[i, j]
      end
   end
   return z
end

# There is no transpose for fq_default_mat
#function transpose(a::fq_default_mat)
#  z = FqDefaultMatSpace(base_ring(a), ncols(a), nrows(a))()
#  ccall((:fq_default_mat_transpose, libflint), Nothing,
#        (Ref{fq_default_mat}, Ref{fq_default_mat}, Ref{FqDefaultFiniteField}), z, a, base_ring(a))
#  return z
#end
#
#function transpose!(a::fq_default_mat)
#  !issquare(a) && error("Matrix must be a square matrix")
#  ccall((:fq_default_mat_transpose, libflint), Nothing,
#        (Ref{fq_default_mat}, Ref{fq_default_mat}, Ref{FqDefaultFiniteField}), a, a, base_ring(a))
#end

###############################################################################
#
#   Row and column swapping
#
###############################################################################

function swap_rows!(x::fq_default_mat, i::Int, j::Int)
  ccall((:fq_default_mat_swap_rows, libflint), Nothing,
        (Ref{fq_default_mat}, Ptr{Nothing}, Int, Int, Ref{FqDefaultFiniteField}),
        x, C_NULL, i - 1, j - 1, base_ring(x))
  return x
end

function swap_rows(x::fq_default_mat, i::Int, j::Int)
   (1 <= i <= nrows(x) && 1 <= j <= nrows(x)) || throw(BoundsError())
   y = deepcopy(x)
   return swap_rows!(y, i, j)
end

function swap_cols!(x::fq_default_mat, i::Int, j::Int)
  ccall((:fq_default_mat_swap_cols, libflint), Nothing,
        (Ref{fq_default_mat}, Ptr{Nothing}, Int, Int, Ref{FqDefaultFiniteField}),
        x, C_NULL, i - 1, j - 1, base_ring(x))
  return x
end

function swap_cols(x::fq_default_mat, i::Int, j::Int)
   (1 <= i <= ncols(x) && 1 <= j <= ncols(x)) || throw(BoundsError())
   y = deepcopy(x)
   return swap_cols!(y, i, j)
end

function reverse_rows!(x::fq_default_mat)
   ccall((:fq_default_mat_invert_rows, libflint), Nothing,
         (Ref{fq_default_mat}, Ptr{Nothing}, Ref{FqDefaultFiniteField}), x, C_NULL, base_ring(x))
   return x
end

reverse_rows(x::fq_default_mat) = reverse_rows!(deepcopy(x))

function reverse_cols!(x::fq_default_mat)
   ccall((:fq_default_mat_invert_cols, libflint), Nothing,
         (Ref{fq_default_mat}, Ptr{Nothing}, Ref{FqDefaultFiniteField}), x, C_NULL, base_ring(x))
   return x
end

reverse_cols(x::fq_default_mat) = reverse_cols!(deepcopy(x))

################################################################################
#
#  Unary operators
#
################################################################################

function -(x::fq_default_mat)
   z = similar(x)
   ccall((:fq_default_mat_neg, libflint), Nothing,
         (Ref{fq_default_mat}, Ref{fq_default_mat}, Ref{FqDefaultFiniteField}), z, x, base_ring(x))
   return z
end

################################################################################
#
#  Binary operators
#
################################################################################

function +(x::fq_default_mat, y::fq_default_mat)
   check_parent(x,y)
   z = similar(x)
   ccall((:fq_default_mat_add, libflint), Nothing,
         (Ref{fq_default_mat}, Ref{fq_default_mat}, Ref{fq_default_mat}, Ref{FqDefaultFiniteField}),
         z, x, y, base_ring(x))
   return z
end

function -(x::fq_default_mat, y::fq_default_mat)
   check_parent(x,y)
   z = similar(x)
   ccall((:fq_default_mat_sub, libflint), Nothing,
         (Ref{fq_default_mat}, Ref{fq_default_mat}, Ref{fq_default_mat}, Ref{FqDefaultFiniteField}),
         z, x, y, base_ring(x))

   return z
end

function *(x::fq_default_mat, y::fq_default_mat)
   (base_ring(x) != base_ring(y)) && error("Base ring must be equal")
   (ncols(x) != nrows(y)) && error("Dimensions are wrong")
   z = similar(x, nrows(x), ncols(y))
   ccall((:fq_default_mat_mul, libflint), Nothing,
         (Ref{fq_default_mat}, Ref{fq_default_mat}, Ref{fq_default_mat}, Ref{FqDefaultFiniteField}), z, x, y, base_ring(x))
   return z
end


################################################################################
#
#  Unsafe operations
#
################################################################################

function mul!(a::fq_default_mat, b::fq_default_mat, c::fq_default_mat)
   ccall((:fq_default_mat_mul, libflint), Nothing,
         (Ref{fq_default_mat}, Ref{fq_default_mat}, Ref{fq_default_mat}, Ref{FqDefaultFiniteField}),
         a, b, c, base_ring(a))
  return a
end

function add!(a::fq_default_mat, b::fq_default_mat, c::fq_default_mat)
   ccall((:fq_default_mat_add, libflint), Nothing,
         (Ref{fq_default_mat}, Ref{fq_default_mat}, Ref{fq_default_mat}, Ref{FqDefaultFiniteField}),
         a, b, c, base_ring(a))
  return a
end

function zero!(a::fq_default_mat)
   ccall((:fq_default_mat_zero, libflint), Nothing,
         (Ref{fq_default_mat}, Ref{FqDefaultFiniteField}), a, base_ring(a))
   return a
end

################################################################################
#
#  Ad hoc binary operators
#
################################################################################

function *(x::fq_default_mat, y::fq_default)
   z = similar(x)
   for i in 1:nrows(x)
      for j in 1:ncols(x)
         z[i, j] = y * x[i, j]
      end
   end
   return z
end

*(x::fq_default, y::fq_default_mat) = y * x

function *(x::fq_default_mat, y::fmpz)
   return base_ring(x)(y) * x
end

*(x::fmpz, y::fq_default_mat) = y * x

function *(x::fq_default_mat, y::Integer)
   return x * base_ring(x)(y)
end

*(x::Integer, y::fq_default_mat) = y * x

################################################################################
#
#  Powering
#
################################################################################

# Fall back to generic one

################################################################################
#
#  Row echelon form
#
################################################################################

function rref(a::fq_default_mat)
   z = deepcopy(a)
   r = ccall((:fq_default_mat_rref, libflint), Int,
             (Ref{fq_default_mat}, Ref{FqDefaultFiniteField}), z, base_ring(a))
   return r, z
end

function rref!(a::fq_default_mat)
   r = ccall((:fq_default_mat_rref, libflint), Int,
         (Ref{fq_default_mat}, Ref{FqDefaultFiniteField}), a, base_ring(a))
   return r
end

#################################################################################
#
#  Trace
#
#################################################################################

function tr(a::fq_default_mat)
   !issquare(a) && error("Non-square matrix")
   n = nrows(a)
   t = zero(base_ring(a))
   for i in 1:nrows(a)
      add!(t, t, a[i, i])
   end
   return t
end

################################################################################
#
#  Determinant
#
################################################################################

function det(a::fq_default_mat)
   !issquare(a) && error("Non-square matrix")
   n = nrows(a)
   R = base_ring(a)
   if n == 0
      return zero(R)
   end
   r, p, l, u = lu(a)
   if r < n
      return zero(R)
   else
      d = one(R)
      for i in 1:nrows(u)
         mul!(d, d, u[i, i])
      end
      return (parity(p) == 0 ? d : -d)
   end
end

################################################################################
#
#  Rank
#
################################################################################

function rank(a::fq_default_mat)
   n = nrows(a)
   if n == 0
      return 0
   end
   r, _, _, _ = lu(a)
   return r
end

################################################################################
#
#  Inverse
#
################################################################################

function inv(a::fq_default_mat)
   !issquare(a) && error("Matrix must be a square matrix")
   z = similar(a)
   r = ccall((:fq_default_mat_inv, libflint), Int,
             (Ref{fq_default_mat}, Ref{fq_default_mat}, Ref{FqDefaultFiniteField}), z, a, base_ring(a))
   !Bool(r) && error("Matrix not invertible")
   return z
end

################################################################################
#
#  Linear solving
#
################################################################################

function solve(x::fq_default_mat, y::fq_default_mat)
   (base_ring(x) != base_ring(y)) && error("Matrices must have same base ring")
   !issquare(x)&& error("First argument not a square matrix in solve")
   (nrows(y) != nrows(x)) || ncols(y) != 1 && ("Not a column vector in solve")
   z = similar(y)
   r = ccall((:fq_default_mat_solve, libflint), Int,
             (Ref{fq_default_mat}, Ref{fq_default_mat}, Ref{fq_default_mat}, Ref{FqDefaultFiniteField}),
             z, x, y, base_ring(x))
   !Bool(r) && error("Singular matrix in solve")
   return z
end

function can_solve_with_solution(a::fq_default_mat, b::fq_default_mat; side::Symbol = :right)
   (base_ring(a) != base_ring(b)) && error("Matrices must have same base ring")
   if side == :left
      (ncols(a) != ncols(b)) && error("Matrices must have same number of columns")
      (f, x) = can_solve_with_solution(transpose(a), transpose(b); side=:right)
      return (f, transpose(x))
   elseif side == :right
      (nrows(a) != nrows(b)) && error("Matrices must have same number of rows")
      x = similar(a, ncols(a), ncols(b))
      r = ccall((:fq_default_mat_can_solve, libflint), Cint,
                (Ref{fq_default_mat}, Ref{fq_default_mat}, Ref{fq_default_mat},
                 Ref{FqDefaultFiniteField}), x, a, b, base_ring(a))
      return Bool(r), x
   else
      error("Unsupported argument :$side for side: Must be :left or :right.")
   end
end

function can_solve(a::fq_default_mat, b::fq_default_mat; side::Symbol = :right)
   fl, _ = can_solve_with_solution(a, b, side = side)
   return fl
end

################################################################################
#
#  LU decomposition
#
################################################################################

function lu!(P::Generic.Perm, x::fq_default_mat)
   P.d .-= 1

   rank = Int(ccall((:fq_default_mat_lu, libflint), Cint,
                (Ptr{Int}, Ref{fq_default_mat}, Cint, Ref{FqDefaultFiniteField}),
                P.d, x, 0, base_ring(x)))

  P.d .+= 1

  # flint does x == PLU instead of Px == LU (docs are wrong)
  inv!(P)

  return rank
end

function lu(x::fq_default_mat, P = SymmetricGroup(nrows(x)))
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
            L[i, j] = one(R)
         elseif j <= m
            L[i, j] = R()
         end
      end
   end
   return rank, p, L, U
end

################################################################################
#
#  Windowing
#
################################################################################

function Base.view(x::fq_default_mat, r1::Int, c1::Int, r2::Int, c2::Int)

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

   z = fq_default_mat()
   z.base_ring = x.base_ring
   z.view_parent = x
   ccall((:fq_default_mat_window_init, libflint), Nothing,
         (Ref{fq_default_mat}, Ref{fq_default_mat}, Int, Int, Int, Int, Ref{FqDefaultFiniteField}),
         z, x, r1 - 1, c1 - 1, r2, c2, base_ring(x))
   finalizer(_fq_default_mat_window_clear_fn, z)
   return z
end

function Base.view(x::fq_default_mat, r::UnitRange{Int}, c::UnitRange{Int})
   return Base.view(x, r.start, c.start, r.stop, c.stop)
end

function _fq_default_mat_window_clear_fn(a::fq_default_mat)
   ccall((:fq_default_mat_window_clear, libflint), Nothing,
         (Ref{fq_default_mat}, Ref{FqDefaultFiniteField}), a, base_ring(a))
end

function sub(x::fq_default_mat, r1::Int, c1::Int, r2::Int, c2::Int)
  return deepcopy(Base.view(x, r1, c1, r2, c2))
end

function sub(x::fq_default_mat, r::UnitRange{Int}, c::UnitRange{Int})
  return deepcopy(Base.view(x, r, c))
end

getindex(x::fq_default_mat, r::UnitRange{Int}, c::UnitRange{Int}) = sub(x, r, c)

################################################################################
#
#  Concatenation
#
################################################################################

function hcat(x::fq_default_mat, y::fq_default_mat)
   (base_ring(x) != base_ring(y)) && error("Matrices must have same base ring")
   (nrows(x) != nrows(y)) && error("Matrices must have same number of rows")
   z = similar(x, nrows(x), ncols(x) + ncols(y))
   ccall((:fq_default_mat_concat_horizontal, libflint), Nothing,
         (Ref{fq_default_mat}, Ref{fq_default_mat}, Ref{fq_default_mat},
          Ref{FqDefaultFiniteField}),
         z, x, y, base_ring(x))
   return z
end

function vcat(x::fq_default_mat, y::fq_default_mat)
   (base_ring(x) != base_ring(y)) && error("Matrices must have same base ring")
   (ncols(x) != ncols(y)) && error("Matrices must have same number of columns")
   z = similar(x, nrows(x) + nrows(y), ncols(x))
   ccall((:fq_default_mat_concat_vertical, libflint), Nothing,
         (Ref{fq_default_mat}, Ref{fq_default_mat}, Ref{fq_default_mat},
          Ref{FqDefaultFiniteField}),
         z, x, y, base_ring(x))
   return z
end

################################################################################
#
#  Conversion
#
################################################################################

function Array(b::fq_default_mat)
  a = Array{fq_default}(undef, nrows(b), ncols(b))
  for i = 1:nrows(b)
    for j = 1:ncols(b)
      a[i, j] = b[i, j]
    end
  end
  return a
end

################################################################################
#
#  Characteristic polynomial
#
################################################################################

function charpoly(R::FqDefaultPolyRing, a::fq_default_mat)
  !issquare(a) && error("Matrix must be square")
  base_ring(R) != base_ring(a) && error("Must have common base ring")
  p = R()
  ccall((:fq_default_mat_charpoly, libflint), Nothing,
          (Ref{fq_default_poly}, Ref{fq_default_mat}, Ref{FqDefaultFiniteField}), p, a, base_ring(a))
  return p
end

function charpoly_danivlesky!(R::FqDefaultPolyRing, a::fq_default_mat)
  !issquare(a) && error("Matrix must be square")
  base_ring(R) != base_ring(a) && error("Must have common base ring")
  p = R()
  ccall((:fq_default_mat_charpoly_danilevsky, libflint), Nothing,
          (Ref{fq_default_poly}, Ref{fq_default_mat}, Ref{FqDefaultFiniteField}), p, a, base_ring(a))
  return p
end


################################################################################
#
#  Minimal polynomial
#
################################################################################

function minpoly(R::FqDefaultPolyRing, a::fq_default_mat)
  !issquare(a) && error("Matrix must be square")
  base_ring(R) != base_ring(a) && error("Must have common base ring")
  m = deepcopy(a)
  p = R()
  ccall((:fq_default_mat_minpoly, libflint), Nothing,
          (Ref{fq_default_poly}, Ref{fq_default_mat}, Ref{FqDefaultFiniteField}), p, m, base_ring(a))
  return p
end

###############################################################################
#
#   Promotion rules
#
###############################################################################

promote_rule(::Type{fq_default_mat}, ::Type{V}) where {V <: Integer} = fq_default_mat

promote_rule(::Type{fq_default_mat}, ::Type{fq_default}) = fq_default_mat

promote_rule(::Type{fq_default_mat}, ::Type{fmpz}) = fq_default_mat

################################################################################
#
#  Parent object overloading
#
################################################################################

function (a::FqDefaultMatSpace)()
  z = fq_default_mat(nrows(a), ncols(a), base_ring(a))
  return z
end

function (a::FqDefaultMatSpace)(b::Integer)
   M = a()
   for i = 1:nrows(a)
      for j = 1:ncols(a)
         if i != j
            M[i, j] = zero(base_ring(a))
         else
            M[i, j] = base_ring(a)(b)
         end
      end
   end
   return M
end

function (a::FqDefaultMatSpace)(b::fmpz)
   M = a()
   for i = 1:nrows(a)
      for j = 1:ncols(a)
         if i != j
            M[i, j] = zero(base_ring(a))
         else
            M[i, j] = base_ring(a)(b)
         end
      end
   end
   return M
end

function (a::FqDefaultMatSpace)(b::fq_default)
   parent(b) != base_ring(a) && error("Unable to coerce to matrix")
   return fq_default_mat(nrows(a), ncols(a), b)
end

function (a::FqDefaultMatSpace)(arr::AbstractMatrix{T}) where {T <: Integer}
  _check_dim(nrows(a), ncols(a), arr)
  return fq_default_mat(nrows(a), ncols(a), arr, base_ring(a))
end

function (a::FqDefaultMatSpace)(arr::AbstractVector{T}) where {T <: Integer}
  _check_dim(nrows(a), ncols(a), arr)
  return fq_default_mat(nrows(a), ncols(a), arr, base_ring(a))
  return z
end

function (a::FqDefaultMatSpace)(arr::AbstractMatrix{fmpz})
  _check_dim(nrows(a), ncols(a), arr)
  return fq_default_mat(nrows(a), ncols(a), arr, base_ring(a))
  return z
end

function (a::FqDefaultMatSpace)(arr::AbstractVector{fmpz})
  _check_dim(nrows(a), ncols(a), arr)
  return fq_default_mat(nrows(a), ncols(a), arr, base_ring(a))
  return z
end

function (a::FqDefaultMatSpace)(arr::AbstractMatrix{fq_default})
  _check_dim(nrows(a), ncols(a), arr)
  (length(arr) > 0 && (base_ring(a) != parent(arr[1]))) && error("Elements must have same base ring")
  return fq_default_mat(nrows(a), ncols(a), arr, base_ring(a))
end

function (a::FqDefaultMatSpace)(arr::AbstractVector{fq_default})
  _check_dim(nrows(a), ncols(a), arr)
  (length(arr) > 0 && (base_ring(a) != parent(arr[1]))) && error("Elements must have same base ring")
  return fq_default_mat(nrows(a), ncols(a), arr, base_ring(a))
end

function (a::FqDefaultMatSpace)(b::fmpz_mat)
   (ncols(a) != ncols(b) || nrows(a) != nrows(b)) && error("Dimensions do not fit")
   return fq_default_mat(b, base_ring(a))
end
 
function (a::FqDefaultMatSpace)(b::Union{nmod_mat, gfp_mat})
   characteristic(base_ring(b)) != characteristic(base_ring(a)) &&
                                   error("Incompatible characteristic")
   (ncols(a) != ncols(b) || nrows(a) != nrows(b)) && error("Dimensions do not fit")
   return fq_default_mat(b, base_ring(a))
end
 
function (a::FqDefaultMatSpace)(b::Zmod_fmpz_mat)
   characteristic(base_ring(b)) != characteristic(base_ring(a)) &&
                                   error("Incompatible characteristic")
   (ncols(a) != ncols(b) || nrows(a) != nrows(b)) && error("Dimensions do not fit")
   return fq_default_mat(b, base_ring(a))
end
 
 ###############################################################################
#
#   Matrix constructor
#
###############################################################################

function matrix(R::FqDefaultFiniteField, arr::AbstractMatrix{<: Union{fq_default, fmpz, Integer}})
   z = fq_default_mat(size(arr, 1), size(arr, 2), arr, R)
   return z
end

function matrix(R::FqDefaultFiniteField, r::Int, c::Int, arr::AbstractVector{<: Union{fq_default, fmpz, Integer}})
   _check_dim(r, c, arr)
   z = fq_default_mat(r, c, arr, R)
   return z
end

###############################################################################
#
#  Zero matrix
#
###############################################################################

function zero_matrix(R::FqDefaultFiniteField, r::Int, c::Int)
   if r < 0 || c < 0
     error("dimensions must not be negative")
   end
   z = fq_default_mat(r, c, R)
   return z
end

###############################################################################
#
#  Identity matrix
#
###############################################################################

function identity_matrix(R::FqDefaultFiniteField, n::Int)
   z = zero_matrix(R, n, n)
   for i in 1:n
      z[i, i] = one(R)
   end
   return z
end

################################################################################
#
#  Matrix space constructor
#
################################################################################

function MatrixSpace(R::FqDefaultFiniteField, r::Int, c::Int, cached::Bool = true)
  FqDefaultMatSpace(R, r, c, cached)
end
