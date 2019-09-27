################################################################################
#
#  fq_mat.jl: flint fq_mat types in julia
#
################################################################################

export fq_mat, FqMatSpace, getindex, setindex!, set_entry!, deepcopy,
       parent, base_ring, zero, one, transpose,
       transpose!, rref, rref!, tr, det, rank, inv, solve, lu,
       sub, hcat, vcat, Array, lift, lift!, MatrixSpace, check_parent,
       howell_form, howell_form!, strong_echelon_form, strong_echelon_form!

################################################################################
#
#  Data type and parent object methods
#
################################################################################

parent_type(::Type{fq_mat}) = FqMatSpace

elem_type(::Type{FqMatSpace}) = fq_mat

dense_matrix_type(::Type{fq}) = fq_mat

function check_parent(x::fq_mat, y::fq_mat, throw::Bool = true)
   fl = base_ring(x) != base_ring(y)
   fl && throw && error("Residue rings must be equal")
   fl && return false
   fl = (ncols(x) != ncols(y)) && (nrows(x) != nrows(y))
   fl && throw && error("Matrices have wrong dimensions")
   return !fl
end

###############################################################################
#
#   Similar
#
###############################################################################

similar(::MatElem, R::FqFiniteField, r::Int, c::Int) = fq_mat(r, c, R)

################################################################################
#
#  Manipulation
#
################################################################################

@inline function getindex(a::fq_mat, i::Int, j::Int)
   @boundscheck Generic._checkbounds(a, i, j)
   GC.@preserve a begin
      el = ccall((:fq_mat_entry, :libflint), Ptr{fq},
                 (Ref{fq_mat}, Int, Int), a, i - 1 , j - 1)
      z = base_ring(a)()
      ccall((:fq_set, :libflint), Nothing, (Ref{fq}, Ptr{fq}), z, el)
   end
   return z
end

@inline function setindex!(a::fq_mat, u::fq, i::Int, j::Int)
   @boundscheck Generic._checkbounds(a, i, j)
   ccall((:fq_mat_entry_set, :libflint), Nothing,
         (Ref{fq_mat}, Int, Int, Ref{fq}, Ref{FqFiniteField}),
         a, i - 1, j - 1, u, base_ring(a))
end

@inline function setindex!(a::fq_mat, u::fmpz, i::Int, j::Int)
   @boundscheck Generic._checkbounds(a, i, j)
   GC.@preserve a begin
      el = ccall((:fq_mat_entry, :libflint), Ptr{fq},
                 (Ref{fq_mat}, Int, Int), a, i - 1, j - 1)
      ccall((:fq_set_fmpz, :libflint), Nothing,
            (Ptr{fq}, Ref{fmpz}, Ref{FqFiniteField}), el, u, base_ring(a))
   end
end

setindex!(a::fq_mat, u::Integer, i::Int, j::Int) =
        setindex!(a, base_ring(a)(u), i, j)

function deepcopy_internal(a::fq_mat, dict::IdDict)
  z = fq_mat(nrows(a), ncols(a), base_ring(a))
  ccall((:fq_mat_set, :libflint), Nothing,
        (Ref{fq_mat}, Ref{fq_mat}, Ref{FqFiniteField}), z, a, base_ring(a))
  return z
end

nrows(a::fq_mat) = a.r

ncols(a::fq_mat) = a.c

nrows(a::FqMatSpace) = a.nrows

ncols(a::FqMatSpace) = a.ncols

parent(a::fq_mat, cached::Bool = true) = FqMatSpace(base_ring(a), nrows(a), ncols(a), cached)

base_ring(a::FqMatSpace) = a.base_ring

base_ring(a::fq_mat) = a.base_ring

zero(a::FqMatSpace) = a()

function one(a::FqMatSpace)
  (nrows(a) != ncols(a)) && error("Matrices must be quadratic")
  return a(one(base_ring(a)))
end

function iszero(a::fq_mat)
   r = ccall((:fq_mat_is_zero, :libflint), Cint,
             (Ref{fq_mat}, Ref{FqFiniteField}), a, base_ring(a))
  return Bool(r)
end

################################################################################
#
#  Comparison
#
################################################################################

function ==(a::fq_mat, b::fq_mat)
   if !(a.base_ring == b.base_ring)
      return false
   end
   r = ccall((:fq_mat_equal, :libflint), Cint,
             (Ref{fq_mat}, Ref{fq_mat}, Ref{FqFiniteField}), a, b, base_ring(a))
   return Bool(r)
end

isequal(a::fq_mat, b::fq_mat) = ==(a, b)

################################################################################
#
#  Transpose
#
################################################################################

function transpose(a::fq_mat)
   z = fq_mat(ncols(a), nrows(a), base_ring(a))
   for i in 1:nrows(a)
      for j in 1:ncols(a)
         z[j, i] = a[i, j]
      end
   end
   return z
end

# There is no transpose for fq_mat
#function transpose(a::fq_mat)
#  z = FqMatSpace(base_ring(a), ncols(a), nrows(a))()
#  ccall((:fq_mat_transpose, :libflint), Nothing,
#        (Ref{fq_mat}, Ref{fq_mat}, Ref{FqFiniteField}), z, a, base_ring(a))
#  return z
#end
#
#function transpose!(a::fq_mat)
#  !issquare(a) && error("Matrix must be a square matrix")
#  ccall((:fq_mat_transpose, :libflint), Nothing,
#        (Ref{fq_mat}, Ref{fq_mat}, Ref{FqFiniteField}), a, a, base_ring(a))
#end

###############################################################################
#
#   Row and column swapping
#
###############################################################################

function swap_rows!(x::fq_mat, i::Int, j::Int)
  ccall((:fq_mat_swap_rows, :libflint), Nothing,
        (Ref{fq_mat}, Ptr{Nothing}, Int, Int, Ref{FqFiniteField}),
        x, C_NULL, i - 1, j - 1, base_ring(x))
  return x
end

function swap_rows(x::fq_mat, i::Int, j::Int)
   (1 <= i <= nrows(x) && 1 <= j <= nrows(x)) || throw(BoundsError())
   y = deepcopy(x)
   return swap_rows!(y, i, j)
end

function swap_cols!(x::fq_mat, i::Int, j::Int)
  ccall((:fq_mat_swap_cols, :libflint), Nothing,
        (Ref{fq_mat}, Ptr{Nothing}, Int, Int, Ref{FqFiniteField}),
        x, C_NULL, i - 1, j - 1, base_ring(x))
  return x
end

function swap_cols(x::fq_mat, i::Int, j::Int)
   (1 <= i <= ncols(x) && 1 <= j <= ncols(x)) || throw(BoundsError())
   y = deepcopy(x)
   return swap_cols!(y, i, j)
end

function reverse_rows!(x::fq_mat)
   ccall((:fq_mat_invert_rows, :libflint), Nothing,
         (Ref{fq_mat}, Ptr{Nothing}, Ref{FqFiniteField}), x, C_NULL, base_ring(x))
   return x
end

reverse_rows(x::fq_mat) = reverse_rows!(deepcopy(x))

function reverse_cols!(x::fq_mat)
   ccall((:fq_mat_invert_cols, :libflint), Nothing,
         (Ref{fq_mat}, Ptr{Nothing}, Ref{FqFiniteField}), x, C_NULL, base_ring(x))
   return x
end

reverse_cols(x::fq_mat) = reverse_cols!(deepcopy(x))

################################################################################
#
#  Unary operators
#
################################################################################

function -(x::fq_mat)
   z = similar(x)
   ccall((:fq_mat_neg, :libflint), Nothing,
         (Ref{fq_mat}, Ref{fq_mat}, Ref{FqFiniteField}), z, x, base_ring(x))
   return z
end

################################################################################
#
#  Binary operators
#
################################################################################

function +(x::fq_mat, y::fq_mat)
   check_parent(x,y)
   z = similar(x)
   ccall((:fq_mat_add, :libflint), Nothing,
         (Ref{fq_mat}, Ref{fq_mat}, Ref{fq_mat}, Ref{FqFiniteField}),
         z, x, y, base_ring(x))
   return z
end

function -(x::fq_mat, y::fq_mat)
   check_parent(x,y)
   z = similar(x)
   ccall((:fq_mat_sub, :libflint), Nothing,
         (Ref{fq_mat}, Ref{fq_mat}, Ref{fq_mat}, Ref{FqFiniteField}),
         z, x, y, base_ring(x))

   return z
end

function *(x::fq_mat, y::fq_mat)
   (base_ring(x) != base_ring(y)) && error("Base ring must be equal")
   (ncols(x) != nrows(y)) && error("Dimensions are wrong")
   z = similar(x, nrows(x), ncols(y))
   ccall((:fq_mat_mul, :libflint), Nothing,
         (Ref{fq_mat}, Ref{fq_mat}, Ref{fq_mat}, Ref{FqFiniteField}), z, x, y, base_ring(x))
   return z
end


################################################################################
#
#  Unsafe operations
#
################################################################################

function mul!(a::fq_mat, b::fq_mat, c::fq_mat)
   ccall((:fq_mat_mul, :libflint), Nothing,
         (Ref{fq_mat}, Ref{fq_mat}, Ref{fq_mat}, Ref{FqFiniteField}),
         a, b, c, base_ring(a))
  return a
end

function add!(a::fq_mat, b::fq_mat, c::fq_mat)
   ccall((:fq_mat_add, :libflint), Nothing,
         (Ref{fq_mat}, Ref{fq_mat}, Ref{fq_mat}, Ref{FqFiniteField}),
         a, b, c, base_ring(a))
  return a
end

function zero!(a::fq_mat)
   ccall((:fq_mat_zero, :libflint), Nothing,
         (Ref{fq_mat}, Ref{FqFiniteField}), a, base_ring(a))
   return a
end

################################################################################
#
#  Ad hoc binary operators
#
################################################################################

function *(x::fq_mat, y::fq)
   z = similar(x)
   for i in 1:nrows(x)
      for j in 1:ncols(x)
         z[i, j] = y * x[i, j]
      end
   end
   return z
end

*(x::fq, y::fq_mat) = y * x

function *(x::fq_mat, y::fmpz)
   return base_ring(x)(y) * x
end

*(x::fmpz, y::fq_mat) = y * x

function *(x::fq_mat, y::Integer)
   return x * base_ring(x)(y)
end

*(x::Integer, y::fq_mat) = y * x

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

function rref(a::fq_mat)
   z = deepcopy(a)
   r = ccall((:fq_mat_rref, :libflint), Int,
             (Ref{fq_mat}, Ref{FqFiniteField}), z, base_ring(a))
   return r, z
end

function rref!(a::fq_mat)
   r = ccall((:fq_mat_rref, :libflint), Int,
         (Ref{fq_mat}, Ref{FqFiniteField}), a, base_ring(a))
   return r
end

#################################################################################
#
#  Trace
#
#################################################################################

function tr(a::fq_mat)
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

function det(a::fq_mat)
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

function rank(a::fq_mat)
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

function inv(a::fq_mat)
   !issquare(a) && error("Matrix must be a square matrix")
   z = similar(a)
   r = ccall((:fq_mat_inv, :libflint), Int,
             (Ref{fq_mat}, Ref{fq_mat}, Ref{FqFiniteField}), z, a, base_ring(a))
   !Bool(r) && error("Matrix not invertible")
   return z
end

################################################################################
#
#  Linear solving
#
################################################################################

function solve(x::fq_mat, y::fq_mat)
   (base_ring(x) != base_ring(y)) && error("Matrices must have same base ring")
   !issquare(x)&& error("First argument not a square matrix in solve")
   (nrows(y) != nrows(x)) || ncols(y) != 1 && ("Not a column vector in solve")
   z = similar(y)
   r = ccall((:fq_mat_solve, :libflint), Int,
             (Ref{fq_mat}, Ref{fq_mat}, Ref{fq_mat}, Ref{FqFiniteField}),
             z, x, y, base_ring(x))
   !Bool(r) && error("Singular matrix in solve")
   return z
end

################################################################################
#
#  LU decomposition
#
################################################################################

function lu!(P::Generic.Perm, x::fq_mat)
   rank = Int(ccall((:fq_mat_lu, :libflint), Cint,
                (Ptr{Int}, Ref{fq_mat}, Cint, Ref{FqFiniteField}),
                P.d, x, 0, base_ring(x)))

  for i in 1:length(P.d)
    P.d[i] += 1
  end

  # flint does x == PLU instead of Px == LU (docs are wrong)
  inv!(P)

  return rank
end

function lu(x::fq_mat, P = PermGroup(nrows(x)))
   m = nrows(x)
   n = ncols(x)
   P.n != m && error("Permutation does not match matrix")
   p = P()
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

function Base.view(x::fq_mat, r1::Int, c1::Int, r2::Int, c2::Int)

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

   z = fq_mat()
   z.base_ring = x.base_ring
   z.view_parent = x
   ccall((:fq_mat_window_init, :libflint), Nothing,
         (Ref{fq_mat}, Ref{fq_mat}, Int, Int, Int, Int, Ref{FqFiniteField}),
         z, x, r1 - 1, c1 - 1, r2, c2, base_ring(x))
   finalizer(_fq_mat_window_clear_fn, z)
   return z
end

function Base.view(x::fq_mat, r::UnitRange{Int}, c::UnitRange{Int})
   return Base.view(x, r.start, c.start, r.stop, c.stop)
end

function _fq_mat_window_clear_fn(a::fq_mat)
   ccall((:fq_mat_window_clear, :libflint), Nothing,
         (Ref{fq_mat}, Ref{FqFiniteField}), a, base_ring(a))
end

function sub(x::fq_mat, r1::Int, c1::Int, r2::Int, c2::Int)
  return deepcopy(Base.view(x, r1, c1, r2, c2))
end

function sub(x::fq_mat, r::UnitRange{Int}, c::UnitRange{Int})
  return deepcopy(Base.view(x, r, c))
end

getindex(x::fq_mat, r::UnitRange{Int}, c::UnitRange{Int}) = sub(x, r, c)

################################################################################
#
#  Concatenation
#
################################################################################

function hcat(x::fq_mat, y::fq_mat)
   (base_ring(x) != base_ring(y)) && error("Matrices must have same base ring")
   (x.r != y.r) && error("Matrices must have same number of rows")
   z = similar(x, nrows(x), ncols(x) + ncols(y))
   ccall((:fq_mat_concat_horizontal, :libflint), Nothing,
         (Ref{fq_mat}, Ref{fq_mat}, Ref{fq_mat}, Ref{FqFiniteField}),
         z, x, y, base_ring(x))
   return z
end

function vcat(x::fq_mat, y::fq_mat)
   (base_ring(x) != base_ring(y)) && error("Matrices must have same base ring")
   (x.c != y.c) && error("Matrices must have same number of columns")
   z = similar(x, nrows(x) + nrows(y), ncols(x))
   ccall((:fq_mat_concat_vertical, :libflint), Nothing,
         (Ref{fq_mat}, Ref{fq_mat}, Ref{fq_mat}, Ref{FqFiniteField}),
         z, x, y, base_ring(x))
   return z
end

################################################################################
#
#  Conversion
#
################################################################################

function Array(b::fq_mat)
  a = Array{fq}(undef, b.r, b.c)
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

function charpoly(R::FqPolyRing, a::fq_mat)
  !issquare(a) && error("Matrix must be square")
  base_ring(R) != base_ring(a) && error("Must have common base ring")
  p = R()
  ccall((:fq_mat_charpoly, :libflint), Nothing,
          (Ref{fq_poly}, Ref{fq_mat}, Ref{FqFiniteField}), p, a, base_ring(a))
  return p
end

function charpoly_danivlesky!(R::FqPolyRing, a::fq_mat)
  !issquare(a) && error("Matrix must be square")
  base_ring(R) != base_ring(a) && error("Must have common base ring")
  p = R()
  ccall((:fq_mat_charpoly_danilevsky, :libflint), Nothing,
          (Ref{fq_poly}, Ref{fq_mat}, Ref{FqFiniteField}), p, a, base_ring(a))
  return p
end


################################################################################
#
#  Minimal polynomial
#
################################################################################

function minpoly(R::FqPolyRing, a::fq_mat)
  !issquare(a) && error("Matrix must be square")
  base_ring(R) != base_ring(a) && error("Must have common base ring")
  m = deepcopy(a)
  p = R()
  ccall((:fq_mat_minpoly, :libflint), Nothing,
          (Ref{fq_poly}, Ref{fq_mat}, Ref{FqFiniteField}), p, m, base_ring(a))
  return p
end

###############################################################################
#
#   Promotion rules
#
###############################################################################

promote_rule(::Type{fq_mat}, ::Type{V}) where {V <: Integer} = fq_mat

promote_rule(::Type{fq_mat}, ::Type{fq}) = fq_mat

promote_rule(::Type{fq_mat}, ::Type{fmpz}) = fq_mat

################################################################################
#
#  Parent object overloading
#
################################################################################

function (a::FqMatSpace)()
  z = fq_mat(nrows(a), ncols(a), base_ring(a))
  return z
end

function (a::FqMatSpace)(b::Integer)
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

function (a::FqMatSpace)(b::fmpz)
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

function (a::FqMatSpace)(b::fq)
   parent(b) != base_ring(a) && error("Unable to coerce to matrix")
   return fq_mat(nrows(a), ncols(a), b)
end

function (a::FqMatSpace)(arr::AbstractArray{T, 2}) where {T <: Integer}
  _check_dim(nrows(a), ncols(a), arr)
  return fq_mat(nrows(a), ncols(a), arr, base_ring(a))
end

function (a::FqMatSpace)(arr::AbstractArray{T, 1}) where {T <: Integer}
  _check_dim(nrows(a), ncols(a), arr)
  return fq_mat(nrows(a), ncols(a), arr, base_ring(a))
  return z
end

function (a::FqMatSpace)(arr::AbstractArray{fmpz, 2})
  _check_dim(nrows(a), ncols(a), arr)
  return fq_mat(nrows(a), ncols(a), arr, base_ring(a))
  return z
end

function (a::FqMatSpace)(arr::AbstractArray{fmpz, 1})
  _check_dim(nrows(a), ncols(a), arr)
  return fq_mat(nrows(a), ncols(a), arr, base_ring(a))
  return z
end

function (a::FqMatSpace)(arr::AbstractArray{fq, 2})
  _check_dim(nrows(a), ncols(a), arr)
  (length(arr) > 0 && (base_ring(a) != parent(arr[1]))) && error("Elements must have same base ring")
  return fq_mat(nrows(a), ncols(a), arr, base_ring(a))
end

function (a::FqMatSpace)(arr::AbstractArray{fq, 1})
  _check_dim(nrows(a), ncols(a), arr)
  (length(arr) > 0 && (base_ring(a) != parent(arr[1]))) && error("Elements must have same base ring")
  return fq_mat(nrows(a), ncols(a), arr, base_ring(a))
end

function (a::FqMatSpace)(b::fmpz_mat)
  (ncols(a) != b.c || nrows(a) != b.r) && error("Dimensions do not fit")
  return fq_mat(b, base_ring(a))
end

###############################################################################
#
#   Matrix constructor
#
###############################################################################

function matrix(R::FqFiniteField, arr::AbstractArray{<: Union{fq, fmpz, Integer}, 2})
   z = fq_mat(size(arr, 1), size(arr, 2), arr, R)
   return z
end

function matrix(R::FqFiniteField, r::Int, c::Int, arr::AbstractArray{<: Union{fq, fmpz, Integer}, 1})
   _check_dim(r, c, arr)
   z = fq_mat(r, c, arr, R)
   return z
end

###############################################################################
#
#  Zero matrix
#
###############################################################################

function zero_matrix(R::FqFiniteField, r::Int, c::Int)
   if r < 0 || c < 0
     error("dimensions must not be negative")
   end
   z = fq_mat(r, c, R)
   return z
end

###############################################################################
#
#  Identity matrix
#
###############################################################################

function identity_matrix(R::FqFiniteField, n::Int)
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

function MatrixSpace(R::FqFiniteField, r::Int, c::Int, cached::Bool = true)
  FqMatSpace(R, r, c, cached)
end
