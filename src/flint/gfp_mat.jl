################################################################################
#
#  gfp_mat.jl: flint gfp_mat types in julia for small prime modulus
#
################################################################################

export gfp_mat, GFPMatSpace

################################################################################
#
#  Data type and parent object methods
#
################################################################################

parent_type(::Type{gfp_mat}) = GFPMatSpace

elem_type(::Type{GFPMatSpace}) = gfp_mat

dense_matrix_type(::Type{gfp_elem}) = gfp_mat

###############################################################################
#
#   Similar
#
###############################################################################

function similar(x::gfp_mat, R::GaloisField, r::Int, c::Int)
   z = gfp_mat(r, c, R.n)
   z.base_ring = R
   return z
end

################################################################################
#
#  Manipulation
#
################################################################################

set_entry!(a::gfp_mat, i::Int, j::Int, u::gfp_elem) =
        set_entry!(a, i, j, u.data)

@inline function setindex!(a::gfp_mat, u::gfp_elem, i::Int, j::Int)
  @boundscheck Generic._checkbounds(a, i, j)
  (base_ring(a) != parent(u)) && error("Parent objects must coincide")
  set_entry!(a, i, j, u.data)
end

function deepcopy_internal(a::gfp_mat, dict::IdDict)
  z = gfp_mat(nrows(a), ncols(a), a.n)
  if isdefined(a, :base_ring)
    z.base_ring = a.base_ring
  end
  ccall((:nmod_mat_set, :libflint), Nothing,
          (Ref{gfp_mat}, Ref{gfp_mat}), z, a)
  return z
end

nrows(a::GFPMatSpace) = a.nrows

ncols(a::GFPMatSpace) = a.ncols

base_ring(a::GFPMatSpace) = a.base_ring

zero(a::GFPMatSpace) = a()

function one(a::GFPMatSpace)
  (nrows(a) != ncols(a)) && error("Matrices must be quadratic")
  z = a()
  ccall((:nmod_mat_one, :libflint), Nothing, (Ref{gfp_mat}, ), z)
  return z
end

################################################################################
#
#  Ad hoc binary operators
#
################################################################################

function *(x::gfp_mat, y::gfp_elem)
  (base_ring(x) != parent(y)) && error("Parent objects must coincide")
  return x*y.data
end

*(x::gfp_elem, y::gfp_mat) = y*x

################################################################################
#
#  Row reduced echelon form
#
################################################################################

function rref(a::gfp_mat)
  z = deepcopy(a)
  r = ccall((:nmod_mat_rref, :libflint), Int, (Ref{gfp_mat}, ), z)
  return r, z
end

function rref!(a::gfp_mat)
  r = ccall((:nmod_mat_rref, :libflint), Int, (Ref{gfp_mat}, ), a)
  return r
end

################################################################################
#
#  Strong echelon form and Howell form
#
################################################################################

@doc Markdown.doc"""
    strong_echelon_form(a::gfp_mat)
> Return the strong echeleon form of $a$. The matrix $a$ must have at least as
> many rows as columns.
"""
function strong_echelon_form(a::gfp_mat)
  (nrows(a) < ncols(a)) &&
              error("Matrix must have at least as many rows as columns")
  r, z = rref(a)
  j_new =  1
  for i in 1:r
    for j in j_new:ncols(a)
      if isone(a[i, j]) && i != j
        z[i, j] = 0
        z[j, j] = 1
        j_new = j
        continue
      end
    end
  end
  return z
end

@doc Markdown.doc"""
    howell_form(a::gfp_mat)
> Return the Howell normal form of $a$. The matrix $a$ must have at least as
> many rows as columns.
"""
function howell_form(a::gfp_mat)
  (nrows(a) < ncols(a)) &&
              error("Matrix must have at least as many rows as columns")
  return rref(a)[2]
end

################################################################################
#
#  Determinant
#
################################################################################

function det(a::gfp_mat)
  !issquare(a) && error("Matrix must be a square matrix")
  r = ccall((:nmod_mat_det, :libflint), UInt, (Ref{gfp_mat}, ), a)
  return base_ring(a)(r)
end

################################################################################
#
#  Windowing
#
################################################################################

function Base.view(x::gfp_mat, r1::Int, c1::Int, r2::Int, c2::Int)

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

  z = gfp_mat()
  z.base_ring = x.base_ring
  z.view_parent = x
  ccall((:nmod_mat_window_init, :libflint), Nothing,
          (Ref{gfp_mat}, Ref{gfp_mat}, Int, Int, Int, Int),
          z, x, r1 - 1, c1 - 1, r2, c2)
  finalizer(_gfp_mat_window_clear_fn, z)
  return z
end

function _gfp_mat_window_clear_fn(a::gfp_mat)
  ccall((:nmod_mat_window_clear, :libflint), Nothing, (Ref{gfp_mat}, ), a)
end

################################################################################
#
#  Conversion
#
################################################################################

function Array(b::gfp_mat)
  a = Array{gfp_elem}(undef, b.r, b.c)
  for i = 1:b.r
    for j = 1:b.c
      a[i, j] = b[i, j]
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
    lift(a::gfp_mat)
> Return a lift of the matrix $a$ to a matrix over $\mathbb{Z}$, i.e. where the
> entries of the returned matrix are those of $a$ lifted to $\mathbb{Z}$.
"""
function lift(a::gfp_mat)
  z = fmpz_mat(nrows(a), ncols(a))
  z.base_ring = FlintZZ
  ccall((:fmpz_mat_set_nmod_mat, :libflint), Nothing,
          (Ref{fmpz_mat}, Ref{gfp_mat}), z, a)
  return z
end

################################################################################
#
#  Characteristic polynomial
#
################################################################################

function charpoly(R::GFPPolyRing, a::gfp_mat)
  m = deepcopy(a)
  p = R()
  ccall((:nmod_mat_charpoly, :libflint), Nothing,
          (Ref{gfp_poly}, Ref{gfp_mat}), p, m)
  return p
end

################################################################################
#
#  Minimal polynomial
#
################################################################################

function minpoly(R::GFPPolyRing, a::gfp_mat)
  p = R()
  ccall((:nmod_mat_minpoly, :libflint), Nothing,
          (Ref{gfp_poly}, Ref{gfp_mat}), p, a)
  return p
end

###############################################################################
#
#   Promotion rules
#
###############################################################################

promote_rule(::Type{gfp_mat}, ::Type{V}) where {V <: Integer} = gfp_mat

promote_rule(::Type{gfp_mat}, ::Type{gfp_elem}) = gfp_mat

promote_rule(::Type{gfp_mat}, ::Type{fmpz}) = gfp_mat

################################################################################
#
#  Parent object overloading
#
################################################################################

function (a::GFPMatSpace)()
  z = gfp_mat(nrows(a), ncols(a), a.n)
  z.base_ring = a.base_ring
  return z
end

function (a::GFPMatSpace)(b::Integer)
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

function (a::GFPMatSpace)(b::fmpz)
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

function (a::GFPMatSpace)(b::gfp_elem)
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

function (a::GFPMatSpace)(arr::AbstractArray{BigInt, 2}, transpose::Bool = false)
  _check_dim(nrows(a), ncols(a), arr, transpose)
  z = gfp_mat(nrows(a), ncols(a), a.n, arr, transpose)
  z.base_ring = a.base_ring
  return z
end

function (a::GFPMatSpace)(arr::AbstractArray{BigInt, 1}, transpose::Bool = false)
  _check_dim(nrows(a), ncols(a), arr)
  z = gfp_mat(nrows(a), ncols(a), a.n, arr, transpose)
  z.base_ring = a.base_ring
  return z
end

function (a::GFPMatSpace)(arr::AbstractArray{fmpz, 2}, transpose::Bool = false)
  _check_dim(nrows(a), ncols(a), arr, transpose)
  z = gfp_mat(nrows(a), ncols(a), a.n, arr, transpose)
  z.base_ring = a.base_ring
  return z
end

function (a::GFPMatSpace)(arr::AbstractArray{fmpz, 1}, transpose::Bool = false)
  _check_dim(nrows(a), ncols(a), arr)
  z = gfp_mat(nrows(a), ncols(a), a.n, arr, transpose)
  z.base_ring = a.base_ring
  return z
end

function (a::GFPMatSpace)(arr::AbstractArray{Int, 2}, transpose::Bool = false)
  _check_dim(nrows(a), ncols(a), arr, transpose)
  z = gfp_mat(nrows(a), ncols(a), a.n, arr, transpose)
  z.base_ring = a.base_ring
  return z
end

function (a::GFPMatSpace)(arr::AbstractArray{Int, 1}, transpose::Bool = false)
  _check_dim(nrows(a), ncols(a), arr)
  z = gfp_mat(nrows(a), ncols(a), a.n, arr, transpose)
  z.base_ring = a.base_ring
  return z
end

function (a::GFPMatSpace)(arr::AbstractArray{gfp_elem, 2}, transpose::Bool = false)
  _check_dim(nrows(a), ncols(a), arr, transpose)
  (length(arr) > 0 && (base_ring(a) != parent(arr[1]))) && error("Elements must have same base ring")
  z = gfp_mat(nrows(a), ncols(a), a.n, arr, transpose)
  z.base_ring = a.base_ring
  return z
end

function (a::GFPMatSpace)(arr::AbstractArray{gfp_elem, 1}, transpose::Bool = false)
  _check_dim(nrows(a), ncols(a), arr)
  (length(arr) > 0 && (base_ring(a) != parent(arr[1]))) && error("Elements must have same base ring")
  z = gfp_mat(nrows(a), ncols(a), a.n, arr, transpose)
  z.base_ring = a.base_ring
  return z
end

function (a::GFPMatSpace)(b::fmpz_mat)
  (ncols(a) != b.c || nrows(a) != b.r) && error("Dimensions do not fit")
  z = gfp_mat(a.n, b)
  z.base_ring = a.base_ring
  return z
end

###############################################################################
#
#   Matrix constructor
#
###############################################################################

function matrix(R::GaloisField, arr::AbstractArray{<: Union{gfp_elem, fmpz, Integer}, 2})
   z = gfp_mat(size(arr, 1), size(arr, 2), R.n, arr)
   z.base_ring = R
   return z
end

function matrix(R::GaloisField, r::Int, c::Int, arr::AbstractArray{<: Union{gfp_elem, fmpz, Integer}, 1})
   _check_dim(r, c, arr)
   z = gfp_mat(r, c, R.n, arr)
   z.base_ring = R
   return z
end

###############################################################################
#
#  Zero matrix
#
###############################################################################

function zero_matrix(R::GaloisField, r::Int, c::Int)
   if r < 0 || c < 0
     error("dimensions must not be negative")
   end
   z = gfp_mat(r, c, R.n)
   z.base_ring = R
   return z
end

###############################################################################
#
#  Identity matrix
#
###############################################################################

function identity_matrix(R::GaloisField, n::Int)
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

function MatrixSpace(R::GaloisField, r::Int, c::Int, cached::Bool = true)
  GFPMatSpace(R, r, c, cached)
end
