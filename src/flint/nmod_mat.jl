################################################################################
#
#  nmod_mat.jl: flint nmod_mat types in julia
#
################################################################################

export nmod_mat, NmodMatSpace, getindex, setindex!, set_entry!, deepcopy, rows, 
       cols, parent, base_ring, zero, one, show, transpose,
       transpose!, rref, rref!, tr, det, rank, inv, solve, lu,
       sub, hcat, vcat, Array, lift, lift!, MatrixSpace, check_parent,
       howell_form, howell_form!, strong_echelon_form, strong_echelon_form!

################################################################################
#
#  Data type and parent object methods
#
################################################################################

parent_type(::Type{nmod_mat}) = NmodMatSpace

elem_type(::Type{NmodMatSpace}) = nmod_mat

function check_parent(x::T, y::T, throw::Bool = true) where T <: Zmodn_mat
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

function similar(x::nmod_mat)
   z = nmod_mat(nrows(x), ncols(x), x.n)
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

@inline function getindex(a::T, i::Int, j::Int) where T <: Zmodn_mat
  @boundscheck Generic._checkbounds(a, i, j)
  u = ccall((:nmod_mat_get_entry, :libflint), UInt,
              (Ref{T}, Int, Int), a, i - 1 , j - 1)
  return base_ring(a)(u)
end

#as above, but as a plain UInt
function getindex_raw(a::T, i::Int, j::Int) where T <: Zmodn_mat
  return ccall((:nmod_mat_get_entry, :libflint), UInt,
                 (Ref{T}, Int, Int), a, i - 1, j - 1)
end

@inline function setindex!(a::T, u::UInt, i::Int, j::Int) where T <: Zmodn_mat
  @boundscheck Generic._checkbounds(a, i, j)
  set_entry!(a, i, j, u)
end

@inline function setindex!(a::T, u::fmpz, i::Int, j::Int) where T <: Zmodn_mat
  @boundscheck Generic._checkbounds(a, i, j)
  set_entry!(a, i, j, u)
end

@inline function setindex!(a::nmod_mat, u::nmod, i::Int, j::Int)
  @boundscheck Generic._checkbounds(a, i, j)
  (base_ring(a) != parent(u)) && error("Parent objects must coincide") 
  set_entry!(a, i, j, u.data)
end

setindex!(a::T, u::Integer, i::Int, j::Int) where T <: Zmodn_mat =
        setindex!(a, fmpz(u), i, j)

setindex_t!(a::T, u::V, i::Int, j::Int) where {V <: Union{RingElem, Integer}, T <: Zmodn_mat} =
  setindex!(a, u, j, i)

function set_entry!(a::T, i::Int, j::Int, u::UInt) where T <: Zmodn_mat
  ccall((:nmod_mat_set_entry, :libflint), Nothing,
          (Ref{T}, Int, Int, UInt), a, i - 1, j - 1, u)
end

function set_entry!(a::T, i::Int, j::Int, u::fmpz) where T <: Zmodn_mat
  t = fmpz()
  ccall((:fmpz_mod_ui, :libflint), UInt,
          (Ref{fmpz}, Ref{fmpz}, UInt), t, u, a.n)
  tt = ccall((:fmpz_get_ui, :libflint), UInt, (Ref{fmpz}, ), t)
  set_entry!(a, i, j, tt)
end

set_entry!(a::nmod_mat, i::Int, j::Int, u::nmod) =
        set_entry!(a, i, j, u.data)

set_entry_t!(a::T, i::Int, j::Int, u::V) where {V <: Union{RingElem, Integer}, T <: Zmodn_mat} =
  set_entry!(a, j, i, u)
 
function deepcopy_internal(a::nmod_mat, dict::IdDict)
  z = nmod_mat(nrows(a), ncols(a), a.n)
  if isdefined(a, :base_ring)
    z.base_ring = a.base_ring
  end
  ccall((:nmod_mat_set, :libflint), Nothing,
          (Ref{nmod_mat}, Ref{nmod_mat}), z, a)
  return z
end

nrows(a::T) where T <: Zmodn_mat = a.r

ncols(a::T) where T <: Zmodn_mat = a.c

nrows(a::NmodMatSpace) = a.nrows

ncols(a::NmodMatSpace) = a.ncols

function parent(a::T, cached::Bool = true) where T <: Zmodn_mat
   MatrixSpace(base_ring(a), nrows(a), ncols(a), cached)
end

base_ring(a::NmodMatSpace) = a.base_ring

base_ring(a::T) where T <: Zmodn_mat = a.base_ring

zero(a::NmodMatSpace) = a()

function one(a::NmodMatSpace)
  (nrows(a) != ncols(a)) && error("Matrices must be quadratic")
  z = a()
  ccall((:nmod_mat_one, :libflint), Nothing, (Ref{nmod_mat}, ), z)
  return z
end

function iszero(a::T) where T <: Zmodn_mat
  r = ccall((:nmod_mat_is_zero, :libflint), Cint, (Ref{T}, ), a)
  return Bool(r)
end

################################################################################
#
#  AbstractString I/O
#
################################################################################

function show(io::IO, a::NmodMatSpace)
   print(io, "Matrix Space of ")
   print(io, nrows(a), " rows and ", ncols(a), " columns over ")
   print(io, a.base_ring)
end

function show(io::IO, a::T) where T <: Zmodn_mat
   rows = a.r
   cols = a.c
   if rows * cols == 0
      print(io, "$rows by $cols matrix")
   end
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

==(a::T, b::T) where T <: Zmodn_mat = (a.base_ring == b.base_ring) &&
        Bool(ccall((:nmod_mat_equal, :libflint), Cint,
                (Ref{T}, Ref{T}), a, b))

isequal(a::T, b::T) where T <: Zmodn_mat = ==(a, b)

################################################################################
#
#  Transpose
#
################################################################################

function transpose(a::T) where T <: Zmodn_mat
  z = similar(a, ncols(a), nrows(a))
  ccall((:nmod_mat_transpose, :libflint), Nothing,
          (Ref{T}, Ref{T}), z, a)
  return z
end

function transpose!(a::T) where T <: Zmodn_mat
  !issquare(a) && error("Matrix must be a square matrix")
  ccall((:nmod_mat_transpose, :libflint), Nothing,
          (Ref{T}, Ref{T}), a, a)
end

################################################################################
#
#  Unary operators
#
################################################################################

function -(x::T) where T <: Zmodn_mat
  z = similar(x)
  ccall((:nmod_mat_neg, :libflint), Nothing,
          (Ref{T}, Ref{T}), z, x)
  return z
end

################################################################################
#
#  Binary operators
#
################################################################################

function +(x::T, y::T) where T <: Zmodn_mat
  check_parent(x,y)
  z = similar(x)
  ccall((:nmod_mat_add, :libflint), Nothing,
          (Ref{T}, Ref{T}, Ref{T}), z, x, y)
  return z
end

function -(x::T, y::T) where T <: Zmodn_mat
  check_parent(x,y)
  z = similar(x)
  ccall((:nmod_mat_sub, :libflint), Nothing,
          (Ref{T}, Ref{T}, Ref{T}), z, x, y)
  return z
end

function *(x::T, y::T) where T <: Zmodn_mat
  (base_ring(x) != base_ring(y)) && error("Base ring must be equal")
  (ncols(x) != nrows(y)) && error("Dimensions are wrong")
  z = similar(x, nrows(x), ncols(y))
  ccall((:nmod_mat_mul, :libflint), Nothing,
          (Ref{T}, Ref{T}, Ref{T}), z, x, y)
  return z
end


################################################################################
#
#  Unsafe operations
#
################################################################################

function mul!(a::T, b::T, c::T) where T <: Zmodn_mat
  ccall((:nmod_mat_mul, :libflint), Nothing, (Ref{T}, Ref{T}, Ref{T}), a, b, c)
  return a
end

function add!(a::T, b::T, c::T) where T <: Zmodn_mat
  ccall((:nmod_mat_add, :libflint), Nothing, (Ref{T}, Ref{T}, Ref{T}), a, b, c)
  return a
end

function zero!(a::T) where T <: Zmodn_mat
  ccall((:nmod_mat_zero, :libflint), Nothing, (Ref{T}, ), a)
  return a
end

################################################################################
#
#  Ad hoc binary operators
#
################################################################################

function *(x::T, y::UInt) where T <: Zmodn_mat
  z = similar(x)
  ccall((:nmod_mat_scalar_mul, :libflint), Nothing,
          (Ref{T}, Ref{T}, UInt), z, x, y)
  return z
end

*(x::UInt, y::T) where T <: Zmodn_mat = y*x

function *(x::T, y::fmpz) where T <: Zmodn_mat
  t = fmpz()
  ccall((:fmpz_mod_ui, :libflint), UInt,
          (Ref{fmpz}, Ref{fmpz}, UInt), t, y, x.n)
  tt = ccall((:fmpz_get_ui, :libflint), UInt, (Ref{fmpz}, ), t)
  return x*tt
end

*(x::fmpz, y::T) where T <: Zmodn_mat = y*x

function *(x::T, y::Integer) where T <: Zmodn_mat
  return x*fmpz(y)
end

*(x::Integer, y::T) where T <: Zmodn_mat = y*x

function *(x::nmod_mat, y::nmod)
  (base_ring(x) != parent(y)) && error("Parent objects must coincide")
  return x*y.data
end

*(x::nmod, y::nmod_mat) = y*x

################################################################################
#
#  Powering
#
################################################################################

function ^(x::T, y::UInt) where T <: Zmodn_mat
  z = similar(x)
  ccall((:nmod_mat_pow, :libflint), Nothing,
          (Ref{T}, Ref{T}, UInt), z, x, y)
  return z
end

function ^(x::T, y::Int) where T <: Zmodn_mat
  ( y < 0 ) && error("Exponent must be positive")
  return x^UInt(y)
end

function ^(x::T, y::fmpz) where T <: Zmodn_mat
  ( y < 0 ) && error("Exponent must be positive")
  ( y > fmpz(typemax(UInt))) &&
          error("Exponent must be smaller then ", fmpz(typemax(UInt)))
  return x^(UInt(y))
end

################################################################################
#
#  Strong echelon form and Howell form
#
################################################################################

function strong_echelon_form!(a::T) where T <: Zmodn_mat
  ccall((:nmod_mat_strong_echelon_form, :libflint), Nothing, (Ref{T}, ), a)
end

@doc Markdown.doc"""
    strong_echelon_form(a::nmod_mat)
> Return the strong echeleon form of $a$. The matrix $a$ must have at least as
> many rows as columns.
"""
function strong_echelon_form(a::nmod_mat)
  (nrows(a) < ncols(a)) &&
              error("Matrix must have at least as many rows as columns")
  z = deepcopy(a)
  strong_echelon_form!(z)
  return z
end

function howell_form!(a::T) where T <: Zmodn_mat
  ccall((:nmod_mat_howell_form, :libflint), Nothing, (Ref{T}, ), a)
end

@doc Markdown.doc"""
    howell_form(a::nmod_mat)
> Return the Howell normal form of $a$. The matrix $a$ must have at least as
> many rows as columns.
"""
function howell_form(a::nmod_mat)
  (nrows(a) < ncols(a)) &&
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

function tr(a::T) where T <: Zmodn_mat
  !issquare(a) && error("Matrix must be a square matrix")
  r = ccall((:nmod_mat_trace, :libflint), UInt, (Ref{T}, ), a)
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

function rank(a::T) where T <: Zmodn_mat
  r = ccall((:nmod_mat_rank, :libflint), Int, (Ref{T}, ), a)
  return r
end

################################################################################
#
#  Inverse
#
################################################################################

function inv(a::T) where T <: Zmodn_mat
  !issquare(a) && error("Matrix must be a square matrix")
  z = similar(a)
  r = ccall((:nmod_mat_inv, :libflint), Int,
          (Ref{T}, Ref{T}), z, a)
  !Bool(r) && error("Matrix not invertible")
  return z
end

################################################################################
#
#  Linear solving
#
################################################################################

function solve(x::T, y::T) where T <: Zmodn_mat
  (base_ring(x) != base_ring(y)) && error("Matrices must have same base ring")
  !issquare(x)&& error("First argument not a square matrix in solve")
  (y.r != x.r) || y.c != 1 && ("Not a column vector in solve")
  z = similar(y)
  r = ccall((:nmod_mat_solve, :libflint), Int,
          (Ref{T}, Ref{T}, Ref{T}), z, x, y)
  !Bool(r) && error("Singular matrix in solve")
  return z
end

################################################################################
#
#  LU decomposition
#
################################################################################

function lu!(P::Generic.perm, x::T) where T <: Zmodn_mat
  rank = Int(ccall((:nmod_mat_lu, :libflint), Cint, (Ptr{Int}, Ref{T}, Cint),
           P.d, x, 0))

  for i in 1:length(P.d)
    P.d[i] += 1
  end

  # flint does x == PLU instead of Px == LU (docs are wrong)
  inv!(P)

  return rank
end

function lu(x::T, P = PermGroup(nrows(x))) where T <: Zmodn_mat
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
#  Row swapping
#
################################################################################

function swap_rows(x::T, i::Int, j::Int) where T <: Zmodn_mat
   n = nrows(x)
   (i < 1 || i > n) && error("Index $i must be between 1 and $n")
   (j < 1 || j > n) && error("Index $j must be between 1 and $n")
   z = deepcopy(x)
   swap_rows!(z, i, j)
   return z
end

function swap_rows!(x::T, i::Int, j::Int) where T <: Zmodn_mat
   ccall((:nmod_mat_swap_rows, :libflint), Nothing,
         (Ref{T}, Ptr{Nothing}, Int, Int), x, C_NULL, i - 1, j - 1)
   return x
end

################################################################################
#
#  Windowing
#
################################################################################

function Base.view(x::nmod_mat, r1::Int, c1::Int, r2::Int, c2::Int)

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

  z = nmod_mat()
  z.base_ring = x.base_ring
  z.view_parent = x
  ccall((:nmod_mat_window_init, :libflint), Nothing,
          (Ref{nmod_mat}, Ref{nmod_mat}, Int, Int, Int, Int),
          z, x, r1 - 1, c1 - 1, r2, c2)
  finalizer(_nmod_mat_window_clear_fn, z)
  return z
end

function Base.view(x::T, r::UnitRange{Int}, c::UnitRange{Int}) where T <: Zmodn_mat
  return Base.view(x, r.start, c.start, r.stop, c.stop)
end

function _nmod_mat_window_clear_fn(a::nmod_mat)
  ccall((:nmod_mat_window_clear, :libflint), Nothing, (Ref{nmod_mat}, ), a)
end

function sub(x::T, r1::Int, c1::Int, r2::Int, c2::Int) where T <: Zmodn_mat
  return deepcopy(Base.view(x, r1, c1, r2, c2))
end

function sub(x::T, r::UnitRange{Int}, c::UnitRange{Int}) where T <: Zmodn_mat
  return deepcopy(Base.view(x, r, c))
end

function getindex(x::T, r::UnitRange{Int}, c::UnitRange{Int}) where T <: Zmodn_mat
   sub(x, r, c)
end

################################################################################
#
#  Concatenation
#
################################################################################

function hcat(x::T, y::T) where T <: Zmodn_mat
  (base_ring(x) != base_ring(y)) && error("Matrices must have same base ring")
  (x.r != y.r) && error("Matrices must have same number of rows")
  z = similar(x, nrows(x), ncols(x) + ncols(y))
  ccall((:nmod_mat_concat_horizontal, :libflint), Nothing,
          (Ref{T}, Ref{T}, Ref{T}), z, x, y)
  return z
end

function vcat(x::T, y::T) where T <: Zmodn_mat
  (base_ring(x) != base_ring(y)) && error("Matrices must have same base ring")
  (x.c != y.c) && error("Matrices must have same number of columns")
  z = similar(x, nrows(x) + nrows(y), ncols(x))
  ccall((:nmod_mat_concat_vertical, :libflint), Nothing,
          (Ref{T}, Ref{T}, Ref{T}), z, x, y)
  return z
end

################################################################################
#
#  Conversion
#
################################################################################

function Array(b::nmod_mat)
  a = Array{nmod}(undef, b.r, b.c)
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

@doc Markdown.doc"""
    lift(a::T) where {T <: Zmodn_mat}
> Return a lift of the matrix $a$ to a matrix over $\mathbb{Z}$, i.e. where the
> entries of the returned matrix are those of $a$ lifted to $\mathbb{Z}$.
"""
function lift(a::T) where {T <: Zmodn_mat}
  z = fmpz_mat(nrows(a), ncols(a))
  z.base_ring = FlintZZ
  ccall((:fmpz_mat_set_nmod_mat, :libflint), Nothing,
          (Ref{fmpz_mat}, Ref{T}), z, a)
  return z 
end

function lift!(z::fmpz_mat, a::T) where T <: Zmodn_mat
  ccall((:fmpz_mat_set_nmod_mat, :libflint), Nothing,
          (Ref{fmpz_mat}, Ref{T}), z, a)
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
  ccall((:nmod_mat_charpoly, :libflint), Nothing,
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
  ccall((:nmod_mat_minpoly, :libflint), Nothing,
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
  z = nmod_mat(nrows(a), ncols(a), a.n)
  z.base_ring = a.base_ring
  return z
end

function (a::NmodMatSpace)(b::Integer)
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

function (a::NmodMatSpace)(b::fmpz)
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

function (a::NmodMatSpace)(b::nmod)
   parent(b) != base_ring(a) && error("Unable to coerce to matrix")
   M = a()
   for i = 1:nrows(a)
      for j = 1:ncols(a)
         if i != j
            M[i, j] = zero(base_ring(a))
         else
            M[i, j] = deepcopy(b)
         end
      end
   end
   return M
end

function (a::NmodMatSpace)(arr::AbstractArray{BigInt, 2}, transpose::Bool = false)
  _check_dim(nrows(a), ncols(a), arr, transpose)
  z = nmod_mat(nrows(a), ncols(a), a.n, arr, transpose)
  z.base_ring = a.base_ring
  return z
end

function (a::NmodMatSpace)(arr::AbstractArray{BigInt, 1}, transpose::Bool = false)
  _check_dim(nrows(a), ncols(a), arr)
  z = nmod_mat(nrows(a), ncols(a), a.n, arr, transpose)
  z.base_ring = a.base_ring
  return z
end

function (a::NmodMatSpace)(arr::AbstractArray{fmpz, 2}, transpose::Bool = false)
  _check_dim(nrows(a), ncols(a), arr, transpose)
  z = nmod_mat(nrows(a), ncols(a), a.n, arr, transpose)
  z.base_ring = a.base_ring
  return z
end

function (a::NmodMatSpace)(arr::AbstractArray{fmpz, 1}, transpose::Bool = false)
  _check_dim(nrows(a), ncols(a), arr)
  z = nmod_mat(nrows(a), ncols(a), a.n, arr, transpose)
  z.base_ring = a.base_ring
  return z
end

function (a::NmodMatSpace)(arr::AbstractArray{Int, 2}, transpose::Bool = false)
  _check_dim(nrows(a), ncols(a), arr, transpose)
  z = nmod_mat(nrows(a), ncols(a), a.n, arr, transpose)
  z.base_ring = a.base_ring
  return z
end

function (a::NmodMatSpace)(arr::AbstractArray{Int, 1}, transpose::Bool = false)
  _check_dim(nrows(a), ncols(a), arr)
  z = nmod_mat(nrows(a), ncols(a), a.n, arr, transpose)
  z.base_ring = a.base_ring
  return z
end

function (a::NmodMatSpace)(arr::AbstractArray{nmod, 2}, transpose::Bool = false)
  _check_dim(nrows(a), ncols(a), arr, transpose)
  (length(arr) > 0 && (base_ring(a) != parent(arr[1]))) && error("Elements must have same base ring")
  z = nmod_mat(nrows(a), ncols(a), a.n, arr, transpose)
  z.base_ring = a.base_ring
  return z
end

function (a::NmodMatSpace)(arr::AbstractArray{nmod, 1}, transpose::Bool = false)
  _check_dim(nrows(a), ncols(a), arr)
  (length(arr) > 0 && (base_ring(a) != parent(arr[1]))) && error("Elements must have same base ring")
  z = nmod_mat(nrows(a), ncols(a), a.n, arr, transpose)
  z.base_ring = a.base_ring
  return z
end

function (a::NmodMatSpace)(b::fmpz_mat)
  (ncols(a) != b.c || nrows(a) != b.r) && error("Dimensions do not fit")
  z = nmod_mat(a.n, b)
  z.base_ring = a.base_ring
  return z
end

###############################################################################
#
#   Matrix constructor
#
###############################################################################

function matrix(R::NmodRing, arr::AbstractArray{<: Union{nmod, fmpz, Integer}, 2})
   z = nmod_mat(size(arr, 1), size(arr, 2), R.n, arr)
   z.base_ring = R
   return z
end

function matrix(R::NmodRing, r::Int, c::Int, arr::AbstractArray{<: Union{nmod, fmpz, Integer}, 1})
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
   if r < 0 || c < 0
     error("dimensions must not be negative")
   end
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

