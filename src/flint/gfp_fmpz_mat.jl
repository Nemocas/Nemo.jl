################################################################################
#
#  gfp_fmpz_mat.jl: flint fmpz_mod_mat (matrices over Z/nZ, large prime n)
#
################################################################################

export gfp_fmpz_mat, GaloisFmpzMatSpace

################################################################################
#
#  Data type and parent object methods
#
################################################################################

parent_type(::Type{gfp_fmpz_mat}) = GaloisFmpzMatSpace

elem_type(::Type{GaloisFmpzMatSpace}) = gfp_fmpz_mat

dense_matrix_type(::Type{gfp_fmpz_elem}) = gfp_fmpz_mat

###############################################################################
#
#   Similar
#
###############################################################################

function similar(::MatElem, R::GaloisFmpzField, r::Int, c::Int)
   z = gfp_fmpz_mat(r, c, R.n)
   z.base_ring = R
   return z
end

################################################################################
#
#  Manipulation
#
################################################################################

# return plain fmpz, no bounds checking
@inline function getindex_raw(a::gfp_fmpz_mat, i::Int, j::Int)
  u = fmpz()
  ccall((:fmpz_mod_mat_get_entry, libflint), Nothing,
                 (Ref{fmpz}, Ref{gfp_fmpz_mat}, Int, Int), u, a, i - 1, j - 1)
  return u
end

@inline function getindex(a::gfp_fmpz_mat, i::Int, j::Int)
  @boundscheck Generic._checkbounds(a, i, j)
  return gfp_fmpz_elem(getindex_raw(a, i, j), base_ring(a)) # no reduction needed
end

@inline function setindex!(a::gfp_fmpz_mat, u::fmpz, i::Int, j::Int)
  @boundscheck Generic._checkbounds(a, i, j)
  R = base_ring(a)
  setindex_raw!(a, mod(u, R.n), i, j)
end

@inline function setindex!(a::gfp_fmpz_mat, u::gfp_fmpz_elem, i::Int, j::Int)
  @boundscheck Generic._checkbounds(a, i, j)
  (base_ring(a) != parent(u)) && error("Parent objects must coincide")
  setindex_raw!(a, u.data, i, j) # no reduction needed
end

function setindex!(a::gfp_fmpz_mat, u::Integer, i::Int, j::Int)
   setindex!(a, fmpz(u), i, j)
end

# as per setindex! but no reduction mod n and no bounds checking
@inline function setindex_raw!(a::gfp_fmpz_mat, u::fmpz, i::Int, j::Int)
  ccall((:fmpz_mod_mat_set_entry, libflint), Nothing,
        (Ref{gfp_fmpz_mat}, Int, Int, Ref{fmpz}), a, i - 1, j - 1, u)
end

function deepcopy_internal(a::gfp_fmpz_mat, dict::IdDict)
  z = gfp_fmpz_mat(nrows(a), ncols(a), parent(a).n)
  if isdefined(a, :base_ring)
    z.base_ring = a.base_ring
  end
  ccall((:fmpz_mod_mat_set, libflint), Nothing,
        (Ref{gfp_fmpz_mat}, Ref{gfp_fmpz_mat}), z, a)
  return z
end

nrows(a::gfp_fmpz_mat) = a.r

ncols(a::gfp_fmpz_mat) = a.c

nrows(a::GaloisFmpzMatSpace) = a.nrows

ncols(a::GaloisFmpzMatSpace) = a.ncols

function parent(a::gfp_fmpz_mat, cached::Bool = true)
   MatrixSpace(base_ring(a), nrows(a), ncols(a); cached)
end

base_ring(a::GaloisFmpzMatSpace) = a.base_ring

base_ring(a::gfp_fmpz_mat) = a.base_ring

zero(a::GaloisFmpzMatSpace) = a()

function one(a::GaloisFmpzMatSpace)
  (nrows(a) != ncols(a)) && error("Matrices must be square")
  z = a()
  ccall((:fmpz_mod_mat_one, libflint), Nothing, (Ref{gfp_fmpz_mat}, ), z)
  return z
end

function iszero(a::gfp_fmpz_mat)
  r = ccall((:fmpz_mod_mat_is_zero, libflint), Cint, (Ref{gfp_fmpz_mat}, ), a)
  return Bool(r)
end

################################################################################
#
#  Ad hoc binary operators
#
################################################################################

function *(x::gfp_fmpz_mat, y::gfp_fmpz_elem)
  (base_ring(x) != parent(y)) && error("Parent objects must coincide")
  return x*y.data
end

*(x::gfp_fmpz_elem, y::gfp_fmpz_mat) = y*x

################################################################################
#
#  Trace
#
################################################################################

function tr(a::gfp_fmpz_mat)
  !is_square(a) && error("Matrix must be a square matrix")
  R = base_ring(a)
  r = fmpz()
  ccall((:fmpz_mod_mat_trace, libflint), Nothing,
        (Ref{fmpz}, Ref{gfp_fmpz_mat}), r, a)
  return gfp_fmpz_elem(r, R)
end


################################################################################
#
#  Windowing
#
################################################################################

function Base.view(x::gfp_fmpz_mat, r1::Int, c1::Int, r2::Int, c2::Int)

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

  z = gfp_fmpz_mat()
  z.base_ring = x.base_ring
  z.view_parent = x
  ccall((:fmpz_mod_mat_window_init, libflint), Nothing,
        (Ref{gfp_fmpz_mat}, Ref{gfp_fmpz_mat}, Int, Int, Int, Int),
        z, x, r1 - 1, c1 - 1, r2, c2)
  finalizer(_gfp_fmpz_mat_window_clear_fn, z)
  return z
end

function _gfp_fmpz_mat_window_clear_fn(a::gfp_fmpz_mat)
  ccall((:fmpz_mod_mat_window_clear, libflint), Nothing, (Ref{gfp_fmpz_mat}, ), a)
end


################################################################################
#
#  Conversion
#
################################################################################

function Array(b::gfp_fmpz_mat)
  a = Array{gfp_fmpz_elem}(undef, b.r, b.c)
  for i = 1:b.r
    for j = 1:b.c
      a[i, j] = b[i, j]
    end
  end
  return a
end

###############################################################################
#
#   Promotion rules
#
###############################################################################

promote_rule(::Type{gfp_fmpz_mat}, ::Type{V}) where {V <: Integer} = gfp_fmpz_mat

promote_rule(::Type{gfp_fmpz_mat}, ::Type{gfp_fmpz_elem}) = gfp_fmpz_mat

promote_rule(::Type{gfp_fmpz_mat}, ::Type{fmpz}) = gfp_fmpz_mat

################################################################################
#
#  Parent object overloading
#
################################################################################

function (a::GaloisFmpzMatSpace)()
  z = gfp_fmpz_mat(nrows(a), ncols(a), a.n)
  z.base_ring = a.base_ring
  return z
end

function (a::GaloisFmpzMatSpace)(b::IntegerUnion)
   M = a()  # zero
   for i in 1:min(nrows(a), ncols(a))
      M[i, i] = base_ring(a)(b)
   end
   return M
end

function (a::GaloisFmpzMatSpace)(b::gfp_fmpz_elem)
   parent(b) != base_ring(a) && error("Unable to coerce to matrix")
   M = a()  # zero
   for i in 1:min(nrows(a), ncols(a))
      M[i, i] = b
   end
   return M
end

function (a::GaloisFmpzMatSpace)(arr::AbstractMatrix{BigInt}, transpose::Bool = false)
  _check_dim(nrows(a), ncols(a), arr, transpose)
  z = gfp_fmpz_mat(nrows(a), ncols(a), a.n, arr, transpose)
  z.base_ring = a.base_ring
  return z
end

function (a::GaloisFmpzMatSpace)(arr::AbstractVector{BigInt})
  _check_dim(nrows(a), ncols(a), arr)
  z = gfp_fmpz_mat(nrows(a), ncols(a), a.n, arr)
  z.base_ring = a.base_ring
  return z
end

function (a::GaloisFmpzMatSpace)(arr::AbstractMatrix{fmpz}, transpose::Bool = false)
  _check_dim(nrows(a), ncols(a), arr, transpose)
  z = gfp_fmpz_mat(nrows(a), ncols(a), a.n, arr, transpose)
  z.base_ring = a.base_ring
  return z
end

function (a::GaloisFmpzMatSpace)(arr::AbstractVector{fmpz})
  _check_dim(nrows(a), ncols(a), arr)
  z = gfp_fmpz_mat(nrows(a), ncols(a), a.n, arr)
  z.base_ring = a.base_ring
  return z
end

function (a::GaloisFmpzMatSpace)(arr::AbstractMatrix{Int}, transpose::Bool = false)
  _check_dim(nrows(a), ncols(a), arr, transpose)
  z = gfp_fmpz_mat(nrows(a), ncols(a), a.n, arr, transpose)
  z.base_ring = a.base_ring
  return z
end

function (a::GaloisFmpzMatSpace)(arr::AbstractVector{Int})
  _check_dim(nrows(a), ncols(a), arr)
  z = gfp_fmpz_mat(nrows(a), ncols(a), a.n, arr)
  z.base_ring = a.base_ring
  return z
end

function (a::GaloisFmpzMatSpace)(arr::AbstractMatrix{gfp_fmpz_elem}, transpose::Bool = false)
  _check_dim(nrows(a), ncols(a), arr, transpose)
  (length(arr) > 0 && (base_ring(a) != parent(arr[1]))) && error("Elements must have same base ring")
  z = gfp_fmpz_mat(nrows(a), ncols(a), a.n, arr, transpose)
  z.base_ring = a.base_ring
  return z
end

function (a::GaloisFmpzMatSpace)(arr::AbstractVector{gfp_fmpz_elem})
  _check_dim(nrows(a), ncols(a), arr)
  (length(arr) > 0 && (base_ring(a) != parent(arr[1]))) && error("Elements must have same base ring")
  z = gfp_fmpz_mat(nrows(a), ncols(a), a.n, arr)
  z.base_ring = a.base_ring
  return z
end

###############################################################################
#
#   Matrix constructor
#
###############################################################################

function matrix(R::GaloisFmpzField, arr::AbstractMatrix{<: Union{gfp_fmpz_elem, fmpz, Integer}})
   z = gfp_fmpz_mat(size(arr, 1), size(arr, 2), R.n, arr)
   z.base_ring = R
   return z
end

function matrix(R::GaloisFmpzField, r::Int, c::Int, arr::AbstractVector{<: Union{gfp_fmpz_elem, fmpz, Integer}})
   _check_dim(r, c, arr)
   z = gfp_fmpz_mat(r, c, R.n, arr)
   z.base_ring = R
   return z
end

###############################################################################
#
#  Zero matrix
#
###############################################################################

function zero_matrix(R::GaloisFmpzField, r::Int, c::Int)
   if r < 0 || c < 0
     error("dimensions must not be negative")
   end
   z = gfp_fmpz_mat(r, c, R.n)
   z.base_ring = R
   return z
end

###############################################################################
#
#  Identity matrix
#
###############################################################################

function identity_matrix(R::GaloisFmpzField, n::Int)
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

function MatrixSpace(R::GaloisFmpzField, r::Int, c::Int; cached::Bool = true)
  GaloisFmpzMatSpace(R, r, c, cached)
end

