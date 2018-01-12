################################################################################
#
#  fq_nmod_mat.jl: flint fq_nmod_mat types in julia
#
################################################################################

export fq_nmod_mat, FqNmodMatSpace, getindex, setindex!, set_entry!, deepcopy, rows, 
       cols, parent, base_ring, zero, one, issquare, show, transpose,
       transpose!, rref, rref!, trace, det, rank, inv, solve, lufact,
       sub, hcat, vcat, Array, lift, lift!, MatrixSpace, check_parent,
       howell_form, howell_form!, strong_echelon_form, strong_echelon_form!

################################################################################
#
#  Data type and parent object methods
#
################################################################################

parent_type(::Type{fq_nmod_mat}) = FqNmodMatSpace

elem_type(::Type{FqNmodMatSpace}) = fq_nmod_mat

function check_parent(x::fq_nmod_mat, y::fq_nmod_mat)
   base_ring(x) != base_ring(y) && error("Residue rings must be equal")
   (cols(x) != cols(y)) && (rows(x) != rows(y)) &&
   error("Matrices have wrong dimensions")
   return nothing
end

size(x::fq_nmod_mat) = tuple(x.r, x.c)

size(t::fq_nmod_mat, d) = d <= 2 ? size(t)[d] : 1

issquare(a::fq_nmod_mat) = (rows(a) == cols(a))

###############################################################################
#
#   Similar
#
###############################################################################

function similar(x::fq_nmod_mat)
   z = fq_nmod_mat(rows(x), cols(x), base_ring(x))
   return z
end

function similar(x::fq_nmod_mat, r::Int, c::Int)
   z = fq_nmod_mat(r, c, base_ring(x))
   return z
end

################################################################################
#
#  Manipulation
#
################################################################################

@inline function getindex(a::fq_nmod_mat, i::Int, j::Int)
   @boundscheck Generic._checkbounds(a, i, j)
   el = ccall((:fq_nmod_mat_entry, :libflint), Ptr{fq_nmod},
              (Ref{fq_nmod_mat}, Int, Int), a, i - 1 , j - 1)
   z = base_ring(a)()
   ccall((:fq_nmod_set, :libflint), Void, (Ref{fq_nmod}, Ptr{fq_nmod}), z, el)
   return z
end

@inline function setindex!(a::fq_nmod_mat, u::fq_nmod, i::Int, j::Int)
   @boundscheck Generic._checkbounds(a, i, j)
   ccall((:fq_nmod_mat_entry_set, :libflint), Void,
         (Ref{fq_nmod_mat}, Int, Int, Ref{fq_nmod}, Ref{FqNmodFiniteField}),
         a, i - 1, j - 1, u, base_ring(a)) end

@inline function setindex!(a::fq_nmod_mat, u::fmpz, i::Int, j::Int)
   @boundscheck Generic._checkbounds(a, i, j)
   el = ccall((:fq_nmod_mat_entry, :libflint), Ptr{fq_nmod},
              (Ref{fq_nmod_mat}, Int, Int), a, i - 1, j - 1)
   ccall((:fq_nmod_set_fmpz, :libflint), Void,
         (Ptr{fq_nmod}, Ref{fmpz}, Ref{FqNmodFiniteField}), el, u, base_ring(a))
end

setindex!(a::fq_nmod_mat, u::Integer, i::Int, j::Int) =
        setindex!(a, base_ring(a)(u), i, j)

function deepcopy_internal(a::fq_nmod_mat, dict::ObjectIdDict)
  z = fq_nmod_mat(rows(a), cols(a), base_ring(a))
  ccall((:fq_nmod_mat_set, :libflint), Void,
        (Ref{fq_nmod_mat}, Ref{fq_nmod_mat}, Ref{FqNmodFiniteField}), z, a, base_ring(a))
  return z
end

rows(a::fq_nmod_mat) = a.r

cols(a::fq_nmod_mat) = a.c

parent(a::fq_nmod_mat, cached::Bool = true) = FqNmodMatSpace(base_ring(a), rows(a), cols(a), cached)

base_ring(a::FqNmodMatSpace) = a.base_ring

base_ring(a::fq_nmod_mat) = a.base_ring

zero(a::FqNmodMatSpace) = a()

function one(a::FqNmodMatSpace)
  (a.rows != a.cols) && error("Matrices must be quadratic")
  return a(one(base_ring(a)))
end

function iszero(a::fq_nmod_mat)
   r = ccall((:fq_nmod_mat_is_zero, :libflint), Cint,
             (Ref{fq_nmod_mat}, Ref{FqNmodFiniteField}), a, base_ring(a))
  return Bool(r)
end

################################################################################
#
#  AbstractString I/O
#
################################################################################

function show(io::IO, a::FqNmodMatSpace)
   print(io, "Matrix Space of ")
   print(io, a.rows, " rows and ", a.cols, " columns over ")
   print(io, a.base_ring)
end

function show(io::IO, a::fq_nmod_mat)
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

function ==(a::fq_nmod_mat, b::fq_nmod_mat)
   if !(a.base_ring == b.base_ring)
      return false
   end
   r = ccall((:fq_nmod_mat_equal, :libflint), Cint,
             (Ref{fq_nmod_mat}, Ref{fq_nmod_mat}, Ref{FqNmodFiniteField}), a, b, base_ring(a))
   return Bool(r)
end

################################################################################
#
#  Transpose
#
################################################################################

function transpose(a::fq_nmod_mat)
   z = fq_nmod_mat(cols(a), rows(a), base_ring(a))
   for i in 1:rows(a)
      for j in 1:cols(a)
         z[j, i] = a[i, j]
      end
   end
   return z
end

# There is no transpose for fq_nmod_mat 
#function transpose(a::fq_nmod_mat)
#  z = FqNmodMatSpace(base_ring(a), cols(a), rows(a))()
#  ccall((:fq_nmod_mat_transpose, :libflint), Void,
#        (Ref{fq_nmod_mat}, Ref{fq_nmod_mat}, Ref{FqNmodFiniteField}), z, a, base_ring(a))
#  return z
#end
#
#function transpose!(a::fq_nmod_mat)
#  !issquare(a) && error("Matrix must be a square matrix")
#  ccall((:fq_nmod_mat_transpose, :libflint), Void,
#        (Ref{fq_nmod_mat}, Ref{fq_nmod_mat}, Ref{FqNmodFiniteField}), a, a, base_ring(a))
#end

################################################################################
#
#  Unary operators
#
################################################################################

function -(x::fq_nmod_mat)
   z = similar(x)
   ccall((:fq_nmod_mat_neg, :libflint), Void,
         (Ref{fq_nmod_mat}, Ref{fq_nmod_mat}, Ref{FqNmodFiniteField}), z, x, base_ring(x))
   return z
end

################################################################################
#
#  Binary operators
#
################################################################################

function +(x::fq_nmod_mat, y::fq_nmod_mat)
   check_parent(x,y)
   z = similar(x)
   ccall((:fq_nmod_mat_add, :libflint), Void,
         (Ref{fq_nmod_mat}, Ref{fq_nmod_mat}, Ref{fq_nmod_mat}, Ref{FqNmodFiniteField}),
         z, x, y, base_ring(x))
   return z
end

function -(x::fq_nmod_mat, y::fq_nmod_mat)
   check_parent(x,y)
   z = similar(x)
   ccall((:fq_nmod_mat_sub, :libflint), Void,
         (Ref{fq_nmod_mat}, Ref{fq_nmod_mat}, Ref{fq_nmod_mat}, Ref{FqNmodFiniteField}),
         z, x, y, base_ring(x))

   return z
end

function *(x::fq_nmod_mat, y::fq_nmod_mat)
   (base_ring(x) != base_ring(y)) && error("Base ring must be equal")
   (cols(x) != rows(y)) && error("Dimensions are wrong")
   z = similar(x, rows(x), cols(y))
   ccall((:fq_nmod_mat_mul, :libflint), Void,
         (Ref{fq_nmod_mat}, Ref{fq_nmod_mat}, Ref{fq_nmod_mat}, Ref{FqNmodFiniteField}), z, x, y, base_ring(x))
   return z
end


################################################################################
#
#  Unsafe operations
#
################################################################################

function mul!(a::fq_nmod_mat, b::fq_nmod_mat, c::fq_nmod_mat)
   ccall((:fq_nmod_mat_mul, :libflint), Void,
         (Ref{fq_nmod_mat}, Ref{fq_nmod_mat}, Ref{fq_nmod_mat}, Ref{FqNmodFiniteField}),
         a, b, c, base_ring(a))
  return a
end

function add!(a::fq_nmod_mat, b::fq_nmod_mat, c::fq_nmod_mat)
   ccall((:fq_nmod_mat_add, :libflint), Void,
         (Ref{fq_nmod_mat}, Ref{fq_nmod_mat}, Ref{fq_nmod_mat}, Ref{FqNmodFiniteField}),
         a, b, c, base_ring(a))
  return a
end

function zero!(a::fq_nmod_mat)
   ccall((:fq_nmod_mat_zero, :libflint), Void,
         (Ref{fq_nmod_mat}, Ref{FqNmodFiniteField}), a, base_ring(a))
   return a
end

################################################################################
#
#  Ad hoc binary operators
#
################################################################################

function *(x::fq_nmod_mat, y::fq_nmod)
   z = similar(x)
   for i in 1:rows(x)
      for j in 1:cols(x)
         z[i, j] = y * x[i, j]
      end
   end
   return z
end

*(x::fq_nmod, y::fq_nmod_mat) = y * x

function *(x::fq_nmod_mat, y::fmpz)
   return base_ring(x)(y) * x 
end

*(x::fmpz, y::fq_nmod_mat) = y * x

function *(x::fq_nmod_mat, y::Integer)
   return x * base_ring(x)(y)
end

*(x::Integer, y::fq_nmod_mat) = y * x

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

function rref(a::fq_nmod_mat)
   z = deepcopy(a)
   r = ccall((:fq_nmod_mat_rref, :libflint), Int,
             (Ref{fq_nmod_mat}, Ref{FqNmodFiniteField}), z, base_ring(a))
   return r, z
end

function rref!(a::fq_nmod_mat)
   r = ccall((:fq_nmod_mat_rref, :libflint), Int,
         (Ref{fq_nmod_mat}, Ref{FqNmodFiniteField}), a, base_ring(a))
   return r
end

#################################################################################
#
#  Trace
#
#################################################################################

function trace(a::fq_nmod_mat)
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

function det(a::fq_nmod_mat)
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

function rank(a::fq_nmod_mat)
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

function inv(a::fq_nmod_mat)
   !issquare(a) && error("Matrix must be a square matrix")
   z = similar(a)
   r = ccall((:fq_nmod_mat_inv, :libflint), Int,
             (Ref{fq_nmod_mat}, Ref{fq_nmod_mat}, Ref{FqNmodFiniteField}), z, a, base_ring(a))
   !Bool(r) && error("Matrix not invertible")
   return z
end

################################################################################
#
#  Linear solving
#
################################################################################

function solve(x::fq_nmod_mat, y::fq_nmod_mat)
   (base_ring(x) != base_ring(y)) && error("Matrices must have same base ring")
   !issquare(x)&& error("First argument not a square matrix in solve")
   (rows(y) != rows(x)) || cols(y) != 1 && ("Not a column vector in solve")
   z = similar(y)
   r = ccall((:fq_nmod_mat_solve, :libflint), Int,
             (Ref{fq_nmod_mat}, Ref{fq_nmod_mat}, Ref{fq_nmod_mat}, Ref{FqNmodFiniteField}),
             z, x, y, base_ring(x))
   !Bool(r) && error("Singular matrix in solve")
   return z
end

################################################################################
#
#  LU decomposition
#
################################################################################

function lufact!(P::Generic.perm, x::fq_nmod_mat)
   rank = ccall((:fq_nmod_mat_lu, :libflint), Cint,
                (Ptr{Int}, Ref{fq_nmod_mat}, Cint, Ref{FqNmodFiniteField}),
                P.d, x, 0, base_ring(x))

  for i in 1:length(P.d)
    P.d[i] += 1
  end

  return rank
end

function lufact(x::fq_nmod_mat, P = PermGroup(rows(x)))
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

function Base.view(x::fq_nmod_mat, r1::Int, c1::Int, r2::Int, c2::Int)
   Generic._checkbounds(x, r1, c1)
   Generic._checkbounds(x, r2, c2)
   (r1 > r2 || c1 > c2) && error("Invalid parameters")
   z = fq_nmod_mat()
   z.base_ring = x.base_ring
   ccall((:fq_nmod_mat_window_init, :libflint), Void,
         (Ref{fq_nmod_mat}, Ref{fq_nmod_mat}, Int, Int, Int, Int, Ref{FqNmodFiniteField}),
         z, x, r1 - 1, c1 - 1, r2, c2, base_ring(x))
   finalizer(z, _fq_nmod_mat_window_clear_fn)
   return z
end

function Base.view(x::fq_nmod_mat, r::UnitRange{Int}, c::UnitRange{Int})
   return Base.view(x, r.start, c.start, r.stop, c.stop)
end

function _fq_nmod_mat_window_clear_fn(a::fq_nmod_mat)
   ccall((:fq_nmod_mat_window_clear, :libflint), Void,
         (Ref{fq_nmod_mat}, Ref{FqNmodFiniteField}), a, base_ring(a))
end

function sub(x::fq_nmod_mat, r1::Int, c1::Int, r2::Int, c2::Int)
  return deepcopy(Base.view(x, r1, c1, r2, c2))
end

function sub(x::fq_nmod_mat, r::UnitRange{Int}, c::UnitRange{Int})
  return deepcopy(Base.view(x, r, c))
end

getindex(x::fq_nmod_mat, r::UnitRange{Int}, c::UnitRange{Int}) = sub(x, r, c)
 
################################################################################
#
#  Concatenation
#
################################################################################

function hcat(x::fq_nmod_mat, y::fq_nmod_mat)
   (base_ring(x) != base_ring(y)) && error("Matrices must have same base ring")
   (x.r != y.r) && error("Matrices must have same number of rows")
   z = similar(x, rows(x), cols(x) + cols(y))
   ccall((:fq_nmod_mat_concat_horizontal, :libflint), Void,
         (Ref{fq_nmod_mat}, Ref{fq_nmod_mat}, Ref{fq_nmod_mat}, Ref{FqNmodFiniteField}),
         z, x, y, base_ring(x))
   return z
end

function vcat(x::fq_nmod_mat, y::fq_nmod_mat)
   (base_ring(x) != base_ring(y)) && error("Matrices must have same base ring")
   (x.c != y.c) && error("Matrices must have same number of columns")
   z = similar(x, rows(x) + rows(y), cols(x))
   ccall((:fq_nmod_mat_concat_vertical, :libflint), Void,
         (Ref{fq_nmod_mat}, Ref{fq_nmod_mat}, Ref{fq_nmod_mat}, Ref{FqNmodFiniteField}),
         z, x, y, base_ring(x))
   return z
end

################################################################################
#
#  Conversion
#
################################################################################

function Array(b::fq_nmod_mat)
  a = Array{fq_nmod}(b.r, b.c)
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

function charpoly(R::FqNmodPolyRing, a::fq_nmod_mat)
  !issquare(a) && error("Matrix must be square")
  base_ring(R) != base_ring(a) && error("Must have common base ring")
  p = R()
  ccall((:fq_nmod_mat_charpoly, :libflint), Void,
          (Ref{fq_nmod_poly}, Ref{fq_nmod_mat}, Ref{FqNmodFiniteField}), p, a, base_ring(a))
  return p
end

function charpoly_danivlesky!(R::FqNmodPolyRing, a::fq_nmod_mat)
  !issquare(a) && error("Matrix must be square")
  base_ring(R) != base_ring(a) && error("Must have common base ring")
  p = R()
  ccall((:fq_nmod_mat_charpoly_danilevsky, :libflint), Void,
          (Ref{fq_nmod_poly}, Ref{fq_nmod_mat}, Ref{FqNmodFiniteField}), p, a, base_ring(a))
  return p
end


################################################################################
#
#  Minimal polynomial
#
################################################################################

function minpoly(R::FqNmodPolyRing, a::fq_nmod_mat)
  !issquare(a) && error("Matrix must be square")
  base_ring(R) != base_ring(a) && error("Must have common base ring")
  m = deepcopy(a)
  p = R()
  ccall((:fq_nmod_mat_minpoly, :libflint), Void,
          (Ref{fq_nmod_poly}, Ref{fq_nmod_mat}, Ref{FqNmodFiniteField}), p, m, base_ring(a))
  return p
end

###############################################################################
#
#   Promotion rules
#
###############################################################################

promote_rule(::Type{fq_nmod_mat}, ::Type{V}) where {V <: Integer} = fq_nmod_mat

promote_rule(::Type{fq_nmod_mat}, ::Type{fq_nmod}) = fq_nmod_mat

promote_rule(::Type{fq_nmod_mat}, ::Type{fmpz}) = fq_nmod_mat

################################################################################
#
#  Parent object overloading
#
################################################################################

function (a::FqNmodMatSpace)()
  z = fq_nmod_mat(a.rows, a.cols, base_ring(a))
  return z
end

function (a::FqNmodMatSpace)(b::Integer)
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

function (a::FqNmodMatSpace)(b::fmpz)
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

function (a::FqNmodMatSpace)(b::fq_nmod)
   parent(b) != base_ring(a) && error("Unable to coerce to matrix")
   return fq_nmod_mat(a.rows, a.cols, b)
end

function (a::FqNmodMatSpace)(arr::Array{T, 2}) where {T <: Integer}
  _check_dim(a.rows, a.cols, arr)
  return fq_nmod_mat(a.rows, a.cols, arr, base_ring(a))
end

function (a::FqNmodMatSpace)(arr::Array{T, 1}) where {T <: Integer}
  _check_dim(a.rows, a.cols, arr)
  return fq_nmod_mat(a.rows, a.cols, arr, base_ring(a))
  return z
end

function (a::FqNmodMatSpace)(arr::Array{fmpz, 2})
  _check_dim(a.rows, a.cols, arr)
  return fq_nmod_mat(a.rows, a.cols, arr, base_ring(a))
  return z
end

function (a::FqNmodMatSpace)(arr::Array{fmpz, 1})
  _check_dim(a.rows, a.cols, arr)
  return fq_nmod_mat(a.rows, a.cols, arr, base_ring(a))
  return z
end

function (a::FqNmodMatSpace)(arr::Array{fq_nmod, 2})
  _check_dim(a.rows, a.cols, arr)
  (length(arr) > 0 && (base_ring(a) != parent(arr[1]))) && error("Elements must have same base ring")
  return fq_nmod_mat(a.rows, a.cols, arr, base_ring(a))
end

function (a::FqNmodMatSpace)(arr::Array{fq_nmod, 1})
  _check_dim(a.rows, a.cols, arr)
  (length(arr) > 0 && (base_ring(a) != parent(arr[1]))) && error("Elements must have same base ring")
  return fq_nmod_mat(a.rows, a.cols, arr, base_ring(a))
end

function (a::FqNmodMatSpace)(b::fmpz_mat)
  (a.cols != b.c || a.rows != b.r) && error("Dimensions do not fit")
  return fq_nmod_mat(b, base_ring(a))
end

###############################################################################
#
#   Matrix constructor
#
###############################################################################

function matrix(R::FqNmodFiniteField, arr::Array{<: Union{fq_nmod, fmpz, Integer}, 2})
   z = fq_nmod_mat(size(arr, 1), size(arr, 2), arr, R)
   return z
end

function matrix(R::FqNmodFiniteField, r::Int, c::Int, arr::Array{<: Union{fq_nmod, fmpz, Integer}, 1})
   _check_dim(r, c, arr)
   z = fq_nmod_mat(r, c, arr, R)
   return z
end

###############################################################################
#
#  Zero matrix
#
###############################################################################

function zero_matrix(R::FqNmodFiniteField, r::Int, c::Int)
   z = fq_nmod_mat(r, c, R)
   return z
end

###############################################################################
#
#  Identity matrix
#
###############################################################################

function identity_matrix(R::FqNmodFiniteField, n::Int)
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

function MatrixSpace(R::FqNmodFiniteField, r::Int, c::Int, cached::Bool = true)
  FqNmodMatSpace(R, r, c, cached)
end
