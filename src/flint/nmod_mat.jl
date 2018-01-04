################################################################################
#
#  nmod_mat.jl: flint nmod_mat types in julia
#
################################################################################

export nmod_mat, NmodMatSpace, getindex, setindex!, set_entry!, deepcopy, rows, 
       cols, parent, base_ring, zero, one, issquare, show, transpose,
       transpose!, rref, rref!, trace, det, rank, inv, solve, lufact,
       sub, hcat, vcat, Array, lift, lift!, MatrixSpace, check_parent,
       howell_form, howell_form!, strong_echelon_form, strong_echelon_form!

################################################################################
#
#  Data type and parent object methods
#
################################################################################

parent_type(::Type{nmod_mat}) = NmodMatSpace

elem_type(::Type{NmodMatSpace}) = nmod_mat

function check_parent(x::nmod_mat, y::nmod_mat)
  base_ring(x) != base_ring(y) && error("Residue rings must be equal")
  (cols(x) != cols(y)) && (rows(x) != rows(y)) &&
          error("Matrices have wrong dimensions")
  return nothing
end

size(x::nmod_mat) = tuple(x.r, x.c)

size(t::nmod_mat, d) = d <= 2 ? size(t)[d] : 1

issquare(a::nmod_mat) = (rows(a) == cols(a))

###############################################################################
#
#   Similar
#
###############################################################################

function similar(x::nmod_mat)
   z = nmod_mat(rows(x), cols(x), x.n)
   z.base_ring = x.base_ring
   return z
end

function similar(x::nmod_mat, r::Int, c::Int)
   z = nmod_mat(r, c, x.n)
   z.base_ring = x.base_ring
   return z
end

################################################################################
#
#  Manipulation
#
################################################################################

@inline function getindex(a::nmod_mat, i::Int, j::Int)
  @boundscheck Generic._checkbounds(a, i, j)
  u = ccall((:nmod_mat_get_entry, :libflint), UInt,
              (Ref{nmod_mat}, Int, Int), a, i - 1 , j - 1)
  return base_ring(a)(u)
end

#as above, but as a plain UInt
function getindex_raw(a::nmod_mat, i::Int, j::Int)
  return ccall((:nmod_mat_get_entry, :libflint), UInt,
                 (Ref{nmod_mat}, Int, Int), a, i - 1, j - 1)
end

@inline function setindex!(a::nmod_mat, u::UInt, i::Int, j::Int)
  @boundscheck Generic._checkbounds(a, i, j)
  set_entry!(a, i, j, u)
end

@inline function setindex!(a::nmod_mat, u::fmpz, i::Int, j::Int)
  @boundscheck Generic._checkbounds(a, i, j)
  set_entry!(a, i, j, u)
end

@inline function setindex!(a::nmod_mat, u::nmod, i::Int, j::Int)
  @boundscheck Generic._checkbounds(a, i, j)
  (base_ring(a) != parent(u)) && error("Parent objects must coincide") 
  set_entry!(a, i, j, u.data)
end

setindex!(a::nmod_mat, u::Integer, i::Int, j::Int) =
        setindex!(a, fmpz(u), i, j)

setindex_t!(a::nmod_mat, u::T, i::Int, j::Int) where {T<:Union{RingElem, Integer}} =
  setindex!(a, u, j, i)

function set_entry!(a::nmod_mat, i::Int, j::Int, u::UInt)
  ccall((:nmod_mat_set_entry, :libflint), Void,
          (Ref{nmod_mat}, Int, Int, UInt), a, i - 1, j - 1, u)
end

function set_entry!(a::nmod_mat, i::Int, j::Int, u::fmpz)
  t = fmpz()
  ccall((:fmpz_mod_ui, :libflint), UInt,
          (Ref{fmpz}, Ref{fmpz}, UInt), t, u, a.n)
  tt = ccall((:fmpz_get_ui, :libflint), UInt, (Ref{fmpz}, ), t)
  set_entry!(a, i, j, tt)
end

set_entry!(a::nmod_mat, i::Int, j::Int, u::nmod) =
        set_entry!(a, i, j, u.data)

set_entry_t!(a::nmod_mat, i::Int, j::Int, u::T) where {T<:Union{RingElem, Integer}} =
  set_entry!(a, j, i, u)
 
function deepcopy_internal(a::nmod_mat, dict::ObjectIdDict)
  z = nmod_mat(rows(a), cols(a), a.n)
  if isdefined(a, :base_ring)
    z.base_ring = a.base_ring
  end
  ccall((:nmod_mat_set, :libflint), Void,
          (Ref{nmod_mat}, Ref{nmod_mat}), z, a)
  return z
end

rows(a::nmod_mat) = a.r

cols(a::nmod_mat) = a.c

parent(a::nmod_mat, cached::Bool = true) = MatrixSpace(base_ring(a), rows(a), cols(a), cached)

base_ring(a::NmodMatSpace) = a.base_ring

base_ring(a::nmod_mat) = a.base_ring

zero(a::NmodMatSpace) = a()

function one(a::NmodMatSpace)
  (a.rows != a.cols) && error("Matrices must be quadratic")
  z = a()
  ccall((:nmod_mat_one, :libflint), Void, (Ref{nmod_mat}, ), z)
  return z
end

function iszero(a::nmod_mat)
  r = ccall((:nmod_mat_is_zero, :libflint), Cint, (Ref{nmod_mat}, ), a)
  return Bool(r)
end

################################################################################
#
#  AbstractString I/O
#
################################################################################

function show(io::IO, a::NmodMatSpace)
   print(io, "Matrix Space of ")
   print(io, a.rows, " rows and ", a.cols, " columns over ")
   print(io, a.base_ring)
end

function show(io::IO, a::nmod_mat)
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

==(a::nmod_mat, b::nmod_mat) = (a.base_ring == b.base_ring) &&
        Bool(ccall((:nmod_mat_equal, :libflint), Cint,
                (Ref{nmod_mat}, Ref{nmod_mat}), a, b))

################################################################################
#
#  Transpose
#
################################################################################

function transpose(a::nmod_mat)
  z = NmodMatSpace(base_ring(a), cols(a), rows(a))()
  ccall((:nmod_mat_transpose, :libflint), Void,
          (Ref{nmod_mat}, Ref{nmod_mat}), z, a)
  return z
end

function transpose!(a::nmod_mat)
  !issquare(a) && error("Matrix must be a square matrix")
  ccall((:nmod_mat_transpose, :libflint), Void,
          (Ref{nmod_mat}, Ref{nmod_mat}), a, a)
end

################################################################################
#
#  Unary operators
#
################################################################################

function -(x::nmod_mat)
  z = similar(x)
  ccall((:nmod_mat_neg, :libflint), Void,
          (Ref{nmod_mat}, Ref{nmod_mat}), z, x)
  return z
end

################################################################################
#
#  Binary operators
#
################################################################################

function +(x::nmod_mat, y::nmod_mat)
  check_parent(x,y)
  z = similar(x)
  ccall((:nmod_mat_add, :libflint), Void,
          (Ref{nmod_mat}, Ref{nmod_mat}, Ref{nmod_mat}), z, x, y)
  return z
end

function -(x::nmod_mat, y::nmod_mat)
  check_parent(x,y)
  z = similar(x)
  ccall((:nmod_mat_sub, :libflint), Void,
          (Ref{nmod_mat}, Ref{nmod_mat}, Ref{nmod_mat}), z, x, y)
  return z
end

function *(x::nmod_mat, y::nmod_mat)
  (base_ring(x) != base_ring(y)) && error("Base ring must be equal")
  (cols(x) != rows(y)) && error("Dimensions are wrong")
  z = similar(x, rows(x), cols(y))
  ccall((:nmod_mat_mul, :libflint), Void,
          (Ref{nmod_mat}, Ref{nmod_mat}, Ref{nmod_mat}), z, x, y)
  return z
end


################################################################################
#
#  Unsafe operations
#
################################################################################

function mul!(a::nmod_mat, b::nmod_mat, c::nmod_mat)
  ccall((:nmod_mat_mul, :libflint), Void, (Ref{nmod_mat}, Ref{nmod_mat}, Ref{nmod_mat}), a, b, c)
  return a
end

function add!(a::nmod_mat, b::nmod_mat, c::nmod_mat)
  ccall((:nmod_mat_add, :libflint), Void, (Ref{nmod_mat}, Ref{nmod_mat}, Ref{nmod_mat}), a, b, c)
  return a
end

function zero!(a::nmod_mat)
  ccall((:nmod_mat_zero, :libflint), Void, (Ref{nmod_mat}, ), a)
  return a
end

################################################################################
#
#  Ad hoc binary operators
#
################################################################################

function *(x::nmod_mat, y::UInt)
  z = similar(x)
  ccall((:nmod_mat_scalar_mul, :libflint), Void,
          (Ref{nmod_mat}, Ref{nmod_mat}, UInt), z, x, y)
  return z
end

*(x::UInt, y::nmod_mat) = y*x

function *(x::nmod_mat, y::fmpz)
  t = fmpz()
  ccall((:fmpz_mod_ui, :libflint), UInt,
          (Ref{fmpz}, Ref{fmpz}, UInt), t, y, x.n)
  tt = ccall((:fmpz_get_ui, :libflint), UInt, (Ref{fmpz}, ), t)
  return x*tt
end

*(x::fmpz, y::nmod_mat) = y*x

function *(x::nmod_mat, y::Integer)
  return x*fmpz(y)
end

*(x::Integer, y::nmod_mat) = y*x

function *(x::nmod_mat, y::nmod)
  (base_ring(x) != parent(y)) && error("Parent objects must coincide")
  return x*y.data
end

*(x::Generic.Res{fmpz}, y::nmod_mat) = y*x

################################################################################
#
#  Powering
#
################################################################################

function ^(x::nmod_mat, y::UInt)
  z = similar(x)
  ccall((:nmod_mat_pow, :libflint), Void,
          (Ref{nmod_mat}, Ref{nmod_mat}, UInt), z, x, y)
  return z
end

function ^(x::nmod_mat, y::Int)
  ( y < 0 ) && error("Exponent must be positive")
  return x^UInt(y)
end

function ^(x::nmod_mat, y::fmpz)
  ( y < 0 ) && error("Exponent must be positive")
  ( y > fmpz(typemax(UInt))) &&
          error("Exponent must be smaller then ", fmpz(typemax(UInt)))
  return x^(UInt(y))
end

################################################################################
#
#  Row echelon form
#
################################################################################

function rref(a::nmod_mat)
  z = deepcopy(a)
  ccall((:nmod_mat_rref, :libflint), Void, (Ref{nmod_mat}, ), z)
  return z
end

function rref!(a::nmod_mat)
  ccall((:nmod_mat_rref, :libflint), Void, (Ref{nmod_mat}, ), a)
  return a
end

################################################################################
#
#  Strong echelon form and Howell form
#
################################################################################

function strong_echelon_form!(a::nmod_mat)
  ccall((:nmod_mat_strong_echelon_form, :libflint), Void, (Ref{nmod_mat}, ), a)
end

doc"""
    strong_echelon_form(a::nmod_mat)
> Return the strong echeleon form of $a$. The matrix $a$ must have at least as
> many rows as columns.
"""
function strong_echelon_form(a::nmod_mat)
  (rows(a) < cols(a)) &&
              error("Matrix must have at least as many rows as columns")
  z = deepcopy(a)
  strong_echelon_form!(z)
  return z
end

function howell_form!(a::nmod_mat)
  ccall((:nmod_mat_howell_form, :libflint), Void, (Ref{nmod_mat}, ), a)
end

doc"""
    howell_form(a::nmod_mat)
> Return the Howell normal form of $a$. The matrix $a$ must have at least as
> many rows as columns.
"""
function howell_form(a::nmod_mat)
  (rows(a) < cols(a)) &&
              error("Matrix must have at least as many rows as columns")

  z = deepcopy(a)
  howell_form!(z)
  return z
end

################################################################################
#
#  Trace
#
################################################################################

function trace(a::nmod_mat)
  !issquare(a) && error("Matrix must be a square matrix")
  r = ccall((:nmod_mat_trace, :libflint), UInt, (Ref{nmod_mat}, ), a)
  return base_ring(a)(r)
end

################################################################################
#
#  Determinant
#
################################################################################

function det(a::nmod_mat)
  !issquare(a) && error("Matrix must be a square matrix")
  if is_prime(a.n)
     r = ccall((:nmod_mat_det, :libflint), UInt, (Ref{nmod_mat}, ), a)
     return base_ring(a)(r)
  else
     try
        return det_fflu(a)
     catch
        return det_df(a)
     end
  end
end

################################################################################
#
#  Rank
#
################################################################################

function rank(a::nmod_mat)
  r = ccall((:nmod_mat_rank, :libflint), Int, (Ref{nmod_mat}, ), a)
  return r
end

################################################################################
#
#  Inverse
#
################################################################################

function inv(a::nmod_mat)
  !issquare(a) && error("Matrix must be a square matrix")
  z = similar(a)
  r = ccall((:nmod_mat_inv, :libflint), Int,
          (Ref{nmod_mat}, Ref{nmod_mat}), z, a)
  !Bool(r) && error("Matrix not invertible")
  return z
end

################################################################################
#
#  Linear solving
#
################################################################################

function solve(x::nmod_mat, y::nmod_mat)
  (base_ring(x) != base_ring(y)) && error("Matrices must have same base ring")
  !issquare(x)&& error("First argument not a square matrix in solve")
  (y.r != x.r) || y.c != 1 && ("Not a column vector in solve")
  z = similar(y)
  r = ccall((:nmod_mat_solve, :libflint), Int,
          (Ref{nmod_mat}, Ref{nmod_mat}, Ref{nmod_mat}), z, x, y)
  !Bool(r) && error("Singular matrix in solve")
  return z
end

################################################################################
#
#  LU decomposition
#
################################################################################

function lufact!(P::Generic.perm, x::nmod_mat)
  rank = ccall((:nmod_mat_lu, :libflint), Cint, (Ptr{Int}, Ref{nmod_mat}, Cint),
           P.d, x, 0)

  for i in 1:length(P.d)
    P.d[i] += 1
  end

  return rank
end

function lufact(x::nmod_mat, P = PermGroup(rows(x)))
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
        L[i, j] = R(1)
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

function Base.view(x::nmod_mat, r1::Int, c1::Int, r2::Int, c2::Int)
  Generic._checkbounds(x, r1, c1)
  Generic._checkbounds(x, r2, c2)
  (r1 > r2 || c1 > c2) && error("Invalid parameters")
  temp = similar(x, r2 - r1 + 1, c2 - c1 + 1)
  ccall((:nmod_mat_window_init, :libflint), Void,
          (Ref{nmod_mat}, Ref{nmod_mat}, Int, Int, Int, Int),
          temp, x, r1-1, c1-1, r2, c2)
  z = deepcopy(temp)
  ccall((:nmod_mat_window_clear, :libflint), Void, (Ref{nmod_mat}, ), temp)
  return z
end

function Base.view(x::nmod_mat, r::UnitRange{Int}, c::UnitRange{Int})
  return Base.view(x, r.start, c.start, r.stop, c.stop)
end

sub(x::nmod_mat, r1::Int, c1::Int, r2::Int, c2::Int) =
        Base.view(x, r1, c1, r2, c2)

sub(x::nmod_mat, r::UnitRange{Int}, c::UnitRange{Int}) = Base.view(x, r, c)
  
################################################################################
#
#  Concatenation
#
################################################################################

function hcat(x::nmod_mat, y::nmod_mat)
  (base_ring(x) != base_ring(y)) && error("Matrices must have same base ring")
  (x.r != y.r) && error("Matrices must have same number of rows")
  z = similar(x, rows(x), cols(x) + cols(y))
  ccall((:nmod_mat_concat_horizontal, :libflint), Void,
          (Ref{nmod_mat}, Ref{nmod_mat}, Ref{nmod_mat}), z, x, y)
  return z
end

function vcat(x::nmod_mat, y::nmod_mat)
  (base_ring(x) != base_ring(y)) && error("Matrices must have same base ring")
  (x.c != y.c) && error("Matrices must have same number of columns")
  z = similar(x, rows(x) + rows(y), cols(x))
  ccall((:nmod_mat_concat_vertical, :libflint), Void,
          (Ref{nmod_mat}, Ref{nmod_mat}, Ref{nmod_mat}), z, x, y)
  return z
end

################################################################################
#
#  Conversion
#
################################################################################

function Array(b::nmod_mat)
  a = Array{nmod}(b.r, b.c)
  for i = 1:b.r
    for j = 1:b.c
      a[i,j] = b[i,j]
    end
  end
  return a
end

################################################################################
#
#  Lifting
#
################################################################################

doc"""
    lift(a::nmod_mat)
> Return a lift of the matrix $a$ to a matrix over $\mathbb{Z}$, i.e. where the
> entries of the returned matrix are those of $a$ lifted to $\mathbb{Z}$.
"""
function lift(a::nmod_mat)
  z = fmpz_mat(rows(a), cols(a))
  z.base_ring = FlintIntegerRing()
  ccall((:fmpz_mat_set_nmod_mat, :libflint), Void,
          (Ref{fmpz_mat}, Ref{nmod_mat}), z, a)
  return z 
end

function lift!(z::fmpz_mat, a::nmod_mat)
  ccall((:fmpz_mat_set_nmod_mat, :libflint), Void,
          (Ref{fmpz_mat}, Ref{nmod_mat}), z, a)
  return z 
end

################################################################################
#
#  Characteristic polynomial
#
################################################################################

function charpoly(R::NmodPolyRing, a::nmod_mat)
  m = deepcopy(a)
  p = R()
  ccall((:nmod_mat_charpoly, :libflint), Void,
          (Ref{nmod_poly}, Ref{nmod_mat}), p, m)
  return p
end

################################################################################
#
#  Minimal polynomial
#
################################################################################

function minpoly(R::NmodPolyRing, a::nmod_mat)
  p = R()
  ccall((:nmod_mat_minpoly, :libflint), Void,
          (Ref{nmod_poly}, Ref{nmod_mat}), p, a)
  return p
end

###############################################################################
#
#   Promotion rules
#
###############################################################################

promote_rule(::Type{nmod_mat}, ::Type{V}) where {V <: Integer} = nmod_mat

promote_rule(::Type{nmod_mat}, ::Type{nmod}) = nmod_mat

promote_rule(::Type{nmod_mat}, ::Type{fmpz}) = nmod_mat

################################################################################
#
#  Parent object overloading
#
################################################################################

function (a::NmodMatSpace)()
  z = nmod_mat(a.rows, a.cols, a.n)
  z.base_ring = a.base_ring
  return z
end

function (a::NmodMatSpace)(b::Integer)
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

function (a::NmodMatSpace)(b::fmpz)
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

function (a::NmodMatSpace)(b::nmod)
   parent(b) != base_ring(a) && error("Unable to coerce to matrix")
   M = a()
   for i = 1:a.rows
      for j = 1:a.cols
         if i != j
            M[i, j] = zero(base_ring(a))
         else
            M[i, j] = deepcopy(b)
         end
      end
   end
   return M
end

function (a::NmodMatSpace)(arr::Array{BigInt, 2}, transpose::Bool = false)
  _check_dim(a.rows, a.cols, arr, transpose)
  z = nmod_mat(a.rows, a.cols, a.n, arr, transpose)
  z.base_ring = a.base_ring
  return z
end

function (a::NmodMatSpace)(arr::Array{BigInt, 1}, transpose::Bool = false)
  _check_dim(a.rows, a.cols, arr)
  z = nmod_mat(a.rows, a.cols, a.n, arr, transpose)
  z.base_ring = a.base_ring
  return z
end

function (a::NmodMatSpace)(arr::Array{fmpz, 2}, transpose::Bool = false)
  _check_dim(a.rows, a.cols, arr, transpose)
  z = nmod_mat(a.rows, a.cols, a.n, arr, transpose)
  z.base_ring = a.base_ring
  return z
end

function (a::NmodMatSpace)(arr::Array{fmpz, 1}, transpose::Bool = false)
  _check_dim(a.rows, a.cols, arr)
  z = nmod_mat(a.rows, a.cols, a.n, arr, transpose)
  z.base_ring = a.base_ring
  return z
end

function (a::NmodMatSpace)(arr::Array{Int, 2}, transpose::Bool = false)
  _check_dim(a.rows, a.cols, arr, transpose)
  z = nmod_mat(a.rows, a.cols, a.n, arr, transpose)
  z.base_ring = a.base_ring
  return z
end

function (a::NmodMatSpace)(arr::Array{Int, 1}, transpose::Bool = false)
  _check_dim(a.rows, a.cols, arr)
  z = nmod_mat(a.rows, a.cols, a.n, arr, transpose)
  z.base_ring = a.base_ring
  return z
end

function (a::NmodMatSpace)(arr::Array{nmod, 2}, transpose::Bool = false)
  _check_dim(a.rows, a.cols, arr, transpose)
  (length(arr) > 0 && (base_ring(a) != parent(arr[1]))) && error("Elements must have same base ring")
  z = nmod_mat(a.rows, a.cols, a.n, arr, transpose)
  z.base_ring = a.base_ring
  return z
end

function (a::NmodMatSpace)(arr::Array{nmod, 1}, transpose::Bool = false)
  _check_dim(a.rows, a.cols, arr)
  (length(arr) > 0 && (base_ring(a) != parent(arr[1]))) && error("Elements must have same base ring")
  z = nmod_mat(a.rows, a.cols, a.n, arr, transpose)
  z.base_ring = a.base_ring
  return z
end

function (a::NmodMatSpace)(b::fmpz_mat)
  (a.cols != b.c || a.rows != b.r) && error("Dimensions do not fit")
  z = nmod_mat(a.n, b)
  z.base_ring = a.base_ring
  return z
end

###############################################################################
#
#   Matrix constructor
#
###############################################################################

function matrix(R::NmodRing, arr::Array{<: Union{nmod, fmpz, Integer}, 2})
   z = nmod_mat(size(arr, 1), size(arr, 2), R.n, arr)
   z.base_ring = R
   return z
end

function matrix(R::NmodRing, r::Int, c::Int, arr::Array{<: Union{nmod, fmpz, Integer}, 1})
   _check_dim(r, c, arr)
   z = nmod_mat(r, c, R.n, arr)
   z.base_ring = R
   return z
end

###############################################################################
#
#  Zero matrix
#
###############################################################################

function zero_matrix(R::NmodRing, r::Int, c::Int)
   z = nmod_mat(r, c, R.n)
   z.base_ring = R
   return z
end

###############################################################################
#
#  Identity matrix
#
###############################################################################

function identity_matrix(R::NmodRing, n::Int)
   z = zero_matrix(R, n, n)
   for i in 1:n
      z[i, i] = one(R)
   end
   z.base_ring = R
   return z
end

################################################################################
#
#  Matrix space constructor
#
################################################################################

function MatrixSpace(R::NmodRing, r::Int, c::Int, cached::Bool = true)
  NmodMatSpace(R, r, c, cached)
end

function MatrixSpace(R::Generic.ResRing{fmpz}, r::Int, c::Int, cached::Bool = true)
  T = elem_type(R)
  return Generic.MatSpace{T}(R, r, c, cached)
end

