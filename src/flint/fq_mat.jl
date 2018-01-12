################################################################################
#
#  fq_mat.jl: flint fq_mat types in julia
#
################################################################################

export fq_mat, FqMatSpace, getindex, setindex!, set_entry!, deepcopy, rows, 
       cols, parent, base_ring, zero, one, issquare, show, transpose,
       transpose!, rref, rref!, trace, det, rank, inv, solve, lufact,
       sub, hcat, vcat, Array, lift, lift!, MatrixSpace, check_parent,
       howell_form, howell_form!, strong_echelon_form, strong_echelon_form!

################################################################################
#
#  Data type and parent object methods
#
################################################################################

parent_type(::Type{fq_mat}) = FqMatSpace

elem_type(::Type{FqMatSpace}) = fq_mat

function check_parent(x::fq_mat, y::fq_mat)
   base_ring(x) != base_ring(y) && error("Residue rings must be equal")
   (cols(x) != cols(y)) && (rows(x) != rows(y)) &&
   error("Matrices have wrong dimensions")
   return nothing
end

size(x::fq_mat) = tuple(x.r, x.c)

size(t::fq_mat, d) = d <= 2 ? size(t)[d] : 1

issquare(a::fq_mat) = (rows(a) == cols(a))

###############################################################################
#
#   Similar
#
###############################################################################

function similar(x::fq_mat)
   z = fq_mat(rows(x), cols(x), base_ring(x))
   return z
end

function similar(x::fq_mat, r::Int, c::Int)
   z = fq_mat(r, c, base_ring(x))
   return z
end

################################################################################
#
#  Manipulation
#
################################################################################

@inline function getindex(a::fq_mat, i::Int, j::Int)
   @boundscheck Generic._checkbounds(a, i, j)
   el = ccall((:fq_mat_entry, :libflint), Ptr{fq},
              (Ref{fq_mat}, Int, Int), a, i - 1 , j - 1)
   z = base_ring(a)()
   ccall((:fq_set, :libflint), Void, (Ref{fq}, Ptr{fq}), z, el)
   return z
end

@inline function setindex!(a::fq_mat, u::fq, i::Int, j::Int)
   @boundscheck Generic._checkbounds(a, i, j)
   ccall((:fq_mat_entry_set, :libflint), Void,
         (Ref{fq_mat}, Int, Int, Ref{fq}, Ref{FqFiniteField}),
         a, i - 1, j - 1, u, base_ring(a)) end

@inline function setindex!(a::fq_mat, u::fmpz, i::Int, j::Int)
   @boundscheck Generic._checkbounds(a, i, j)
   el = ccall((:fq_mat_entry, :libflint), Ptr{fq},
              (Ref{fq_mat}, Int, Int), a, i - 1, j - 1)
   ccall((:fq_set_fmpz, :libflint), Void,
         (Ptr{fq}, Ref{fmpz}, Ref{FqFiniteField}), el, u, base_ring(a))
end

setindex!(a::fq_mat, u::Integer, i::Int, j::Int) =
        setindex!(a, base_ring(a)(u), i, j)

function deepcopy_internal(a::fq_mat, dict::ObjectIdDict)
  z = fq_mat(rows(a), cols(a), base_ring(a))
  ccall((:fq_mat_set, :libflint), Void,
        (Ref{fq_mat}, Ref{fq_mat}, Ref{FqFiniteField}), z, a, base_ring(a))
  return z
end

rows(a::fq_mat) = a.r

cols(a::fq_mat) = a.c

parent(a::fq_mat, cached::Bool = true) = FqMatSpace(base_ring(a), rows(a), cols(a), cached)

base_ring(a::FqMatSpace) = a.base_ring

base_ring(a::fq_mat) = a.base_ring

zero(a::FqMatSpace) = a()

function one(a::FqMatSpace)
  (a.rows != a.cols) && error("Matrices must be quadratic")
  return a(one(base_ring(a)))
end

function iszero(a::fq_mat)
   r = ccall((:fq_mat_is_zero, :libflint), Cint,
             (Ref{fq_mat}, Ref{FqFiniteField}), a, base_ring(a))
  return Bool(r)
end

################################################################################
#
#  AbstractString I/O
#
################################################################################

function show(io::IO, a::FqMatSpace)
   print(io, "Matrix Space of ")
   print(io, a.rows, " rows and ", a.cols, " columns over ")
   print(io, a.base_ring)
end

function show(io::IO, a::fq_mat)
   rows = a.r
   cols = a.c
   for i = 1:rows
      print(io, "[")
      for j = 1:cols
         print(io, a[i, j])
         if j != cols
            print(io, " ")
         end
      end
      print(io, "]")
      if i != rows
         println(io, "")
      end
   end
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

################################################################################
#
#  Transpose
#
################################################################################

function transpose(a::fq_mat)
   z = fq_mat(cols(a), rows(a), base_ring(a))
   for i in 1:rows(a)
      for j in 1:cols(a)
         z[j, i] = a[i, j]
      end
   end
   return z
end

# There is no transpose for fq_mat 
#function transpose(a::fq_mat)
#  z = FqMatSpace(base_ring(a), cols(a), rows(a))()
#  ccall((:fq_mat_transpose, :libflint), Void,
#        (Ref{fq_mat}, Ref{fq_mat}, Ref{FqFiniteField}), z, a, base_ring(a))
#  return z
#end
#
#function transpose!(a::fq_mat)
#  !issquare(a) && error("Matrix must be a square matrix")
#  ccall((:fq_mat_transpose, :libflint), Void,
#        (Ref{fq_mat}, Ref{fq_mat}, Ref{FqFiniteField}), a, a, base_ring(a))
#end

################################################################################
#
#  Unary operators
#
################################################################################

function -(x::fq_mat)
   z = similar(x)
   ccall((:fq_mat_neg, :libflint), Void,
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
   ccall((:fq_mat_add, :libflint), Void,
         (Ref{fq_mat}, Ref{fq_mat}, Ref{fq_mat}, Ref{FqFiniteField}),
         z, x, y, base_ring(x))
   return z
end

function -(x::fq_mat, y::fq_mat)
   check_parent(x,y)
   z = similar(x)
   ccall((:fq_mat_sub, :libflint), Void,
         (Ref{fq_mat}, Ref{fq_mat}, Ref{fq_mat}, Ref{FqFiniteField}),
         z, x, y, base_ring(x))

   return z
end

function *(x::fq_mat, y::fq_mat)
   (base_ring(x) != base_ring(y)) && error("Base ring must be equal")
   (cols(x) != rows(y)) && error("Dimensions are wrong")
   z = similar(x, rows(x), cols(y))
   ccall((:fq_mat_mul, :libflint), Void,
         (Ref{fq_mat}, Ref{fq_mat}, Ref{fq_mat}, Ref{FqFiniteField}), z, x, y, base_ring(x))
   return z
end


################################################################################
#
#  Unsafe operations
#
################################################################################

function mul!(a::fq_mat, b::fq_mat, c::fq_mat)
   ccall((:fq_mat_mul, :libflint), Void,
         (Ref{fq_mat}, Ref{fq_mat}, Ref{fq_mat}, Ref{FqFiniteField}),
         a, b, c, base_ring(a))
  return a
end

function add!(a::fq_mat, b::fq_mat, c::fq_mat)
   ccall((:fq_mat_add, :libflint), Void,
         (Ref{fq_mat}, Ref{fq_mat}, Ref{fq_mat}, Ref{FqFiniteField}),
         a, b, c, base_ring(a))
  return a
end

function zero!(a::fq_mat)
   ccall((:fq_mat_zero, :libflint), Void,
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
   for i in 1:rows(x)
      for j in 1:cols(x)
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

function trace(a::fq_mat)
   !issquare(a) && error("Non-square matrix")
   n = rows(a)
   t = zero(base_ring(a))
   for i in 1:rows(a)
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
   n = rows(a)
   R = base_ring(a)
   if n == 0
      return zero(R)
   end
   r, p, l, u = lufact(a)
   if r < n
      return zero(R)
   else
      d = one(R)
      for i in 1:rows(u)
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
   n = rows(a)
   if n == 0
      return 0
   end
   r, _, _, _ = lufact(a)
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
   (rows(y) != rows(x)) || cols(y) != 1 && ("Not a column vector in solve")
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

function lufact!(P::Generic.perm, x::fq_mat)
   rank = ccall((:fq_mat_lu, :libflint), Cint,
                (Ptr{Int}, Ref{fq_mat}, Cint, Ref{FqFiniteField}),
                P.d, x, 0, base_ring(x))

  for i in 1:length(P.d)
    P.d[i] += 1
  end

  return rank
end

function lufact(x::fq_mat, P = PermGroup(rows(x)))
   m = rows(x)
   n = cols(x)
   P.n != m && error("Permutation does not match matrix")
   p = P()
   R = base_ring(x)
   U = deepcopy(x)

   L = similar(x, m, m)

   rank = lufact!(p, U)

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
   Generic._checkbounds(x, r1, c1)
   Generic._checkbounds(x, r2, c2)
   (r1 > r2 || c1 > c2) && error("Invalid parameters")
   z = fq_mat()
   z.base_ring = x.base_ring
   ccall((:fq_mat_window_init, :libflint), Void,
         (Ref{fq_mat}, Ref{fq_mat}, Int, Int, Int, Int, Ref{FqFiniteField}),
         z, x, r1 - 1, c1 - 1, r2, c2, base_ring(x))
   finalizer(z, _fq_mat_window_clear_fn)
   return z
end

function Base.view(x::fq_mat, r::UnitRange{Int}, c::UnitRange{Int})
   return Base.view(x, r.start, c.start, r.stop, c.stop)
end

function _fq_mat_window_clear_fn(a::fq_mat)
   ccall((:fq_mat_window_clear, :libflint), Void,
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
   z = similar(x, rows(x), cols(x) + cols(y))
   ccall((:fq_mat_concat_horizontal, :libflint), Void,
         (Ref{fq_mat}, Ref{fq_mat}, Ref{fq_mat}, Ref{FqFiniteField}),
         z, x, y, base_ring(x))
   return z
end

function vcat(x::fq_mat, y::fq_mat)
   (base_ring(x) != base_ring(y)) && error("Matrices must have same base ring")
   (x.c != y.c) && error("Matrices must have same number of columns")
   z = similar(x, rows(x) + rows(y), cols(x))
   ccall((:fq_mat_concat_vertical, :libflint), Void,
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
  a = Array{fq}(b.r, b.c)
  for i = 1:rows(b)
    for j = 1:cols(b)
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
  ccall((:fq_mat_charpoly, :libflint), Void,
          (Ref{fq_poly}, Ref{fq_mat}, Ref{FqFiniteField}), p, a, base_ring(a))
  return p
end

function charpoly_danivlesky!(R::FqPolyRing, a::fq_mat)
  !issquare(a) && error("Matrix must be square")
  base_ring(R) != base_ring(a) && error("Must have common base ring")
  p = R()
  ccall((:fq_mat_charpoly_danilevsky, :libflint), Void,
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
  ccall((:fq_mat_minpoly, :libflint), Void,
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
  z = fq_mat(a.rows, a.cols, base_ring(a))
  return z
end

function (a::FqMatSpace)(b::Integer)
   M = a()
   for i = 1:a.rows
      for j = 1:a.cols
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
   for i = 1:a.rows
      for j = 1:a.cols
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
   return fq_mat(a.rows, a.cols, b)
end

function (a::FqMatSpace)(arr::Array{T, 2}) where {T <: Integer}
  _check_dim(a.rows, a.cols, arr)
  return fq_mat(a.rows, a.cols, arr, base_ring(a))
end

function (a::FqMatSpace)(arr::Array{T, 1}) where {T <: Integer}
  _check_dim(a.rows, a.cols, arr)
  return fq_mat(a.rows, a.cols, arr, base_ring(a))
  return z
end

function (a::FqMatSpace)(arr::Array{fmpz, 2})
  _check_dim(a.rows, a.cols, arr)
  return fq_mat(a.rows, a.cols, arr, base_ring(a))
  return z
end

function (a::FqMatSpace)(arr::Array{fmpz, 1})
  _check_dim(a.rows, a.cols, arr)
  return fq_mat(a.rows, a.cols, arr, base_ring(a))
  return z
end

function (a::FqMatSpace)(arr::Array{fq, 2})
  _check_dim(a.rows, a.cols, arr)
  (length(arr) > 0 && (base_ring(a) != parent(arr[1]))) && error("Elements must have same base ring")
  return fq_mat(a.rows, a.cols, arr, base_ring(a))
end

function (a::FqMatSpace)(arr::Array{fq, 1})
  _check_dim(a.rows, a.cols, arr)
  (length(arr) > 0 && (base_ring(a) != parent(arr[1]))) && error("Elements must have same base ring")
  return fq_mat(a.rows, a.cols, arr, base_ring(a))
end

function (a::FqMatSpace)(b::fmpz_mat)
  (a.cols != b.c || a.rows != b.r) && error("Dimensions do not fit")
  return fq_mat(b, base_ring(a))
end

###############################################################################
#
#   Matrix constructor
#
###############################################################################

function matrix(R::FqFiniteField, arr::Array{<: Union{fq, fmpz, Integer}, 2})
   z = fq_mat(size(arr, 1), size(arr, 2), arr, R)
   return z
end

function matrix(R::FqFiniteField, r::Int, c::Int, arr::Array{<: Union{fq, fmpz, Integer}, 1})
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
