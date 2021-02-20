################################################################################
#
#  fmpz_mod_mat.jl: flint fmpz_mod_mat (matrices over Z/nZ, large n)
#
################################################################################

export fmpz_mod_mat, FmpzModMatSpace

################################################################################
#
#  Data type and parent object methods
#
################################################################################

parent_type(::Type{fmpz_mod_mat}) = FmpzModMatSpace

elem_type(::Type{FmpzModMatSpace}) = fmpz_mod_mat

dense_matrix_type(::Type{fmpz_mod}) = fmpz_mod_mat

function check_parent(x::T, y::T, throw::Bool = true) where T <: Zmod_fmpz_mat
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

function similar(::MatElem, R::FmpzModRing, r::Int, c::Int)
   z = fmpz_mod_mat(r, c, R.n)
   z.base_ring = R
   return z
end

################################################################################
#
#  Manipulation
#
################################################################################

@inline function getindex(a::T, i::Int, j::Int) where T <: Zmod_fmpz_mat
  @boundscheck Generic._checkbounds(a, i, j)
  u = fmpz()
  ccall((:fmpz_mod_mat_get_entry, libflint), Nothing,
              (Ref{fmpz}, Ref{T}, Int, Int), u, a, i - 1 , j - 1)
  return fmpz_mod(u, base_ring(a)) # no reduction needed
end

# as above, but as a plain fmpz, no bounds checking
function getindex_raw(a::T, i::Int, j::Int) where T <: Zmod_fmpz_mat
  u = fmpz()
  ccall((:fmpz_mod_mat_get_entry, libflint), Nothing,
                 (Ref{fmpz}, Ref{T}, Int, Int), u, a, i - 1, j - 1)
  return u
end

@inline function setindex!(a::T, u::fmpz, i::Int, j::Int) where T <: Zmod_fmpz_mat
  @boundscheck Generic._checkbounds(a, i, j)
  R = base_ring(a)
  setindex_raw!(a, mod(u, R.n), i, j)
end

@inline function setindex!(a::T, u::fmpz_mod, i::Int, j::Int) where T <: Zmod_fmpz_mat
  @boundscheck Generic._checkbounds(a, i, j)
  (base_ring(a) != parent(u)) && error("Parent objects must coincide")
  setindex_raw!(a, u.data, i, j) # no reduction needed
end

function setindex!(a::T, u::Integer, i::Int, j::Int) where T <: Zmod_fmpz_mat
   setindex!(a, fmpz(u), i, j)
end

# as per setindex! but no reduction mod n and no bounds checking
@inline function setindex_raw!(a::T, u::fmpz, i::Int, j::Int) where T <: Zmod_fmpz_mat
  ccall((:fmpz_mod_mat_set_entry, libflint), Nothing,
        (Ref{T}, Int, Int, Ref{fmpz}), a, i - 1, j - 1, u)
end

function deepcopy_internal(a::fmpz_mod_mat, dict::IdDict)
  z = fmpz_mod_mat(nrows(a), ncols(a), parent(a).n)
  if isdefined(a, :base_ring)
    z.base_ring = a.base_ring
  end
  ccall((:fmpz_mod_mat_set, libflint), Nothing,
          (Ref{fmpz_mod_mat}, Ref{fmpz_mod_mat}), z, a)
  return z
end

nrows(a::T) where T <: Zmod_fmpz_mat = a.r

ncols(a::T) where T <: Zmod_fmpz_mat = a.c

nrows(a::FmpzModMatSpace) = a.nrows

ncols(a::FmpzModMatSpace) = a.ncols

function parent(a::T, cached::Bool = true) where T <: Zmod_fmpz_mat
   MatrixSpace(base_ring(a), nrows(a), ncols(a), cached)
end

base_ring(a::FmpzModMatSpace) = a.base_ring

base_ring(a::T) where T <: Zmod_fmpz_mat = a.base_ring

zero(a::FmpzModMatSpace) = a()

function one(a::FmpzModMatSpace)
  (nrows(a) != ncols(a)) && error("Matrices must be square")
  z = a()
  ccall((:fmpz_mod_mat_one, libflint), Nothing, (Ref{fmpz_mod_mat}, ), z)
  return z
end

function iszero(a::T) where T <: Zmod_fmpz_mat
  r = ccall((:fmpz_mod_mat_is_zero, libflint), Cint, (Ref{T}, ), a)
  return Bool(r)
end

################################################################################
#
#  Comparison
#
################################################################################

==(a::T, b::T) where T <: Zmod_fmpz_mat = (a.base_ring == b.base_ring) &&
        Bool(ccall((:fmpz_mod_mat_equal, libflint), Cint,
                (Ref{T}, Ref{T}), a, b))

isequal(a::T, b::T) where T <: Zmod_fmpz_mat = ==(a, b)

################################################################################
#
#  Transpose
#
################################################################################

function transpose(a::T) where T <: Zmod_fmpz_mat
  z = similar(a, ncols(a), nrows(a))
  ccall((:fmpz_mod_mat_transpose, libflint), Nothing,
          (Ref{T}, Ref{T}), z, a)
  return z
end

function transpose!(a::T) where T <: Zmod_fmpz_mat
  !issquare(a) && error("Matrix must be a square matrix")
  ccall((:fmpz_mod_mat_transpose, libflint), Nothing,
          (Ref{T}, Ref{T}), a, a)
end

###############################################################################
#
#   Row and column swapping
#
###############################################################################

#= Not currently implemented in Flint

function swap_rows!(x::T, i::Int, j::Int) where T <: Zmod_fmpz_mat
  ccall((:fmpz_mod_mat_swap_rows, libflint), Nothing,
        (Ref{T}, Ptr{Nothing}, Int, Int), x, C_NULL, i - 1, j - 1)
  return x
end

function swap_rows(x::T, i::Int, j::Int) where T <: Zmod_fmpz_mat
   (1 <= i <= nrows(x) && 1 <= j <= nrows(x)) || throw(BoundsError())
   y = deepcopy(x)
   return swap_rows!(y, i, j)
end

function swap_cols!(x::T, i::Int, j::Int) where T <: Zmod_fmpz_mat
  ccall((:fmpz_mod_mat_swap_cols, libflint), Nothing,
        (Ref{T}, Ptr{Nothing}, Int, Int), x, C_NULL, i - 1, j - 1)
  return x
end

function swap_cols(x::T, i::Int, j::Int) where T <: Zmod_fmpz_mat
   (1 <= i <= ncols(x) && 1 <= j <= ncols(x)) || throw(BoundsError())
   y = deepcopy(x)
   return swap_cols!(y, i, j)
end

function reverse_rows!(x::T) where T <: Zmod_fmpz_mat
   ccall((:fmpz_mod_mat_invert_rows, libflint), Nothing,
         (Ref{T}, Ptr{Nothing}), x, C_NULL)
   return x
end

reverse_rows(x::T) where T <: Zmod_fmpz_mat = reverse_rows!(deepcopy(x))

function reverse_cols!(x::T) where T <: Zmod_fmpz_mat
   ccall((:fmpz_mod_mat_invert_cols, libflint), Nothing,
         (Ref{T}, Ptr{Nothing}), x, C_NULL)
   return x
end

reverse_cols(x::T) where T <: Zmod_fmpz_mat = reverse_cols!(deepcopy(x))

=#

################################################################################
#
#  Unary operators
#
################################################################################

function -(x::T) where T <: Zmod_fmpz_mat
  z = similar(x)
  ccall((:fmpz_mod_mat_neg, libflint), Nothing,
          (Ref{T}, Ref{T}), z, x)
  return z
end

################################################################################
#
#  Binary operators
#
################################################################################

function +(x::T, y::T) where T <: Zmod_fmpz_mat
  check_parent(x,y)
  z = similar(x)
  ccall((:fmpz_mod_mat_add, libflint), Nothing,
          (Ref{T}, Ref{T}, Ref{T}), z, x, y)
  return z
end

function -(x::T, y::T) where T <: Zmod_fmpz_mat
  check_parent(x,y)
  z = similar(x)
  ccall((:fmpz_mod_mat_sub, libflint), Nothing,
          (Ref{T}, Ref{T}, Ref{T}), z, x, y)
  return z
end

function *(x::T, y::T) where T <: Zmod_fmpz_mat
  (base_ring(x) != base_ring(y)) && error("Base ring must be equal")
  (ncols(x) != nrows(y)) && error("Dimensions are wrong")
  z = similar(x, nrows(x), ncols(y))
  ccall((:fmpz_mod_mat_mul, libflint), Nothing,
          (Ref{T}, Ref{T}, Ref{T}), z, x, y)
  return z
end


################################################################################
#
#  Unsafe operations
#
################################################################################

function mul!(a::T, b::T, c::T) where T <: Zmod_fmpz_mat
  ccall((:fmpz_mod_mat_mul, libflint), Nothing, (Ref{T}, Ref{T}, Ref{T}), a, b, c)
  return a
end

function add!(a::T, b::T, c::T) where T <: Zmod_fmpz_mat
  ccall((:fmpz_mod_mat_add, libflint), Nothing, (Ref{T}, Ref{T}, Ref{T}), a, b, c)
  return a
end

function zero!(a::T) where T <: Zmod_fmpz_mat
  ccall((:fmpz_mod_mat_zero, libflint), Nothing, (Ref{T}, ), a)
  return a
end

################################################################################
#
#  Ad hoc binary operators
#
################################################################################

function *(x::T, y::Int) where T <: Zmod_fmpz_mat
  z = similar(x)
  ccall((:fmpz_mod_mat_scalar_mul_si, libflint), Nothing,
          (Ref{T}, Ref{T}, Int), z, x, y)
  return z
end

*(x::Int, y::T) where T <: Zmod_fmpz_mat = y*x

function *(x::T, y::fmpz) where T <: Zmod_fmpz_mat
  z = similar(x)
  ccall((:fmpz_mod_mat_scalar_mul_fmpz, libflint), Nothing,
	(Ref{T}, Ref{T}, Ref{fmpz}), z, x, y)
  return z
end

*(x::fmpz, y::T) where T <: Zmod_fmpz_mat = y*x

function *(x::T, y::Integer) where T <: Zmod_fmpz_mat
  return x*fmpz(y)
end

*(x::Integer, y::T) where T <: Zmod_fmpz_mat = y*x

function *(x::fmpz_mod_mat, y::fmpz_mod)
  (base_ring(x) != parent(y)) && error("Parent objects must coincide")
  return x*y.data
end

*(x::fmpz_mod, y::fmpz_mod_mat) = y*x

################################################################################
#
#  Powering
#
################################################################################

#= Not implemented in Flint yet

function ^(x::T, y::Int) where T <: Zmod_fmpz_mat
  if y < 0
     x = inv(x)
     y = -y
  end
  z = similar(x)
  ccall((:fmpz_mod_mat_pow, libflint), Nothing,
          (Ref{T}, Ref{T}, Int), z, x, y)
  return z
end
=#

function ^(x::T, y::fmpz) where T <: Zmod_fmpz_mat
  (y > fmpz(typemax(Int))) &&
          error("Exponent must be smaller than ", fmpz(typemax(Int)))
  (y < fmpz(typemin(Int))) &&
          error("Exponent must be bigger than ", fmpz(typemin(Int)))
  return x^(Int(y))
end

################################################################################
#
#  Strong echelon form and Howell form
#
################################################################################

function strong_echelon_form!(a::T) where T <: Zmod_fmpz_mat
  ccall((:fmpz_mod_mat_strong_echelon_form, libflint), Nothing, (Ref{T}, ), a)
end

@doc Markdown.doc"""
    strong_echelon_form(a::fmpz_mod_mat)

Return the strong echeleon form of $a$. The matrix $a$ must have at least as
many rows as columns.
"""
function strong_echelon_form(a::fmpz_mod_mat)
  (nrows(a) < ncols(a)) &&
              error("Matrix must have at least as many rows as columns")
  z = deepcopy(a)
  strong_echelon_form!(z)
  return z
end

function howell_form!(a::T) where T <: Zmod_fmpz_mat
  ccall((:fmpz_mod_mat_howell_form, libflint), Nothing, (Ref{T}, ), a)
end

@doc Markdown.doc"""
    howell_form(a::fmpz_mod_mat)

Return the Howell normal form of $a$. The matrix $a$ must have at least as
many rows as columns.
"""
function howell_form(a::fmpz_mod_mat)
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

function tr(a::T) where T <: Zmod_fmpz_mat
  !issquare(a) && error("Matrix must be a square matrix")
  R = base_ring(a)
  r = fmpz()
  ccall((:fmpz_mod_mat_trace, libflint), Nothing, (Ref{fmpz}, Ref{T}), r, a)
  return fmpz_mod(r, R)
end

################################################################################
#
#  Determinant
#
################################################################################

#= Not implemented in Flint yet

function det(a::fmpz_mod_mat)
  !issquare(a) && error("Matrix must be a square matrix")
  if isprime(a.n)
     r = ccall((:fmpz_mod_mat_det, libflint), UInt, (Ref{fmpz_mod_mat}, ), a)
     return base_ring(a)(r)
  else
     try
        return det_fflu(a)
     catch
        return det_df(a)
     end
  end
end

=#

################################################################################
#
#  Rank
#
################################################################################

#= Not implemented in Flint yet

function rank(a::T) where T <: Zmod_fmpz_mat
  r = ccall((:fmpz_mod_mat_rank, libflint), Int, (Ref{T}, ), a)
  return r
end

=#

################################################################################
#
#  Inverse
#
################################################################################

function inv(a::fmpz_mod_mat)
  !issquare(a) && error("Matrix must be a square matrix")
  if isprobable_prime(modulus(base_ring(a)))
     X, d = pseudo_inv(a)
     if !isunit(d)
        error("Matrix is not invertible")
     end
     return divexact(X, d)
  else
     b = map_entries(x -> x.data, a)
     c, d = pseudo_inv(b)
     R = base_ring(a)
     if !isone(gcd(d, modulus(R)))
        error("Matrix not invertible")
     end
     return change_base_ring(R, c) * inv(R(d))
  end
end

#= Not implemented in Flint yet

function inv(a::T) where T <: Zmod_fmpz_mat
  !issquare(a) && error("Matrix must be a square matrix")
  z = similar(a)
  r = ccall((:fmpz_mod_mat_inv, libflint), Int,
          (Ref{T}, Ref{T}), z, a)
  !Bool(r) && error("Matrix not invertible")
  return z
end

=#

################################################################################
#
#  Linear solving
#
################################################################################

#= Not implemented in Flint yet

function solve(x::T, y::T) where T <: Zmod_fmpz_mat
  (base_ring(x) != base_ring(y)) && error("Matrices must have same base ring")
  !issquare(x)&& error("First argument not a square matrix in solve")
  (y.r != x.r) || y.c != 1 && ("Not a column vector in solve")
  z = similar(y)
  r = ccall((:fmpz_mod_mat_solve, libflint), Int,
          (Ref{T}, Ref{T}, Ref{T}), z, x, y)
  !Bool(r) && error("Singular matrix in solve")
  return z
end

=#

################################################################################
#
#  LU decomposition
#
################################################################################

#= Not implemented in Flint yet

function lu!(P::Generic.Perm, x::T) where T <: Zmod_fmpz_mat
  P.d .-= 1

  rank = Int(ccall((:fmpz_mod_mat_lu, libflint), Cint, (Ptr{Int}, Ref{T}, Cint),
           P.d, x, 0))

  P.d .+= 1

  # flint does x == PLU instead of Px == LU (docs are wrong)
  inv!(P)

  return rank
end

function lu(x::T, P = SymmetricGroup(nrows(x))) where T <: Zmod_fmpz_mat
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
        L[i, j] = R(1)
      elseif j <= m
        L[i, j] = R()
      end
    end
  end
  return rank, p, L, U
end

=#

################################################################################
#
#  Windowing
#
################################################################################

function Base.view(x::fmpz_mod_mat, r1::Int, c1::Int, r2::Int, c2::Int)

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

  z = fmpz_mod_mat()
  z.base_ring = x.base_ring
  z.view_parent = x
  ccall((:fmpz_mod_mat_window_init, libflint), Nothing,
          (Ref{fmpz_mod_mat}, Ref{fmpz_mod_mat}, Int, Int, Int, Int),
          z, x, r1 - 1, c1 - 1, r2, c2)
  finalizer(_fmpz_mod_mat_window_clear_fn, z)
  return z
end

function Base.view(x::T, r::UnitRange{Int}, c::UnitRange{Int}) where T <: Zmod_fmpz_mat
  return Base.view(x, r.start, c.start, r.stop, c.stop)
end

function _fmpz_mod_mat_window_clear_fn(a::fmpz_mod_mat)
  ccall((:fmpz_mod_mat_window_clear, libflint), Nothing, (Ref{fmpz_mod_mat}, ), a)
end

function sub(x::T, r1::Int, c1::Int, r2::Int, c2::Int) where T <: Zmod_fmpz_mat
  return deepcopy(Base.view(x, r1, c1, r2, c2))
end

function sub(x::T, r::UnitRange{Int}, c::UnitRange{Int}) where T <: Zmod_fmpz_mat
  return deepcopy(Base.view(x, r, c))
end

function getindex(x::T, r::UnitRange{Int}, c::UnitRange{Int}) where T <: Zmod_fmpz_mat
   sub(x, r, c)
end

################################################################################
#
#  Concatenation
#
################################################################################

function hcat(x::T, y::T) where T <: Zmod_fmpz_mat
  (base_ring(x) != base_ring(y)) && error("Matrices must have same base ring")
  (x.r != y.r) && error("Matrices must have same number of rows")
  z = similar(x, nrows(x), ncols(x) + ncols(y))
  ccall((:fmpz_mod_mat_concat_horizontal, libflint), Nothing,
          (Ref{T}, Ref{T}, Ref{T}), z, x, y)
  return z
end

function vcat(x::T, y::T) where T <: Zmod_fmpz_mat
  (base_ring(x) != base_ring(y)) && error("Matrices must have same base ring")
  (x.c != y.c) && error("Matrices must have same number of columns")
  z = similar(x, nrows(x) + nrows(y), ncols(x))
  ccall((:fmpz_mod_mat_concat_vertical, libflint), Nothing,
          (Ref{T}, Ref{T}, Ref{T}), z, x, y)
  return z
end

################################################################################
#
#  Conversion
#
################################################################################

function Array(b::fmpz_mod_mat)
  a = Array{fmpz_mod}(undef, b.r, b.c)
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

#= Not implemented in Flint yet

@doc Markdown.doc"""
    lift(a::T) where {T <: Zmod_fmpz_mat}

Return a lift of the matrix $a$ to a matrix over $\mathbb{Z}$, i.e. where the
entries of the returned matrix are those of $a$ lifted to $\mathbb{Z}$.
"""
function lift(a::T) where {T <: Zmod_fmpz_mat}
  z = fmpz_mat(nrows(a), ncols(a))
  z.base_ring = FlintZZ
  ccall((:fmpz_mat_set_fmpz_mod_mat, libflint), Nothing,
          (Ref{fmpz_mat}, Ref{T}), z, a)
  return z
end

function lift!(z::fmpz_mat, a::T) where T <: Zmod_fmpz_mat
  ccall((:fmpz_mat_set_fmpz_mod_mat, libflint), Nothing,
          (Ref{fmpz_mat}, Ref{T}), z, a)
  return z
end

=#

################################################################################
#
#  Characteristic polynomial
#
################################################################################

#= Not implemented in Flint yet

function charpoly(R::FmpzModPolyRing, a::fmpz_mod_mat)
  m = deepcopy(a)
  p = R()
  ccall((:fmpz_mod_mat_charpoly, libflint), Nothing,
          (Ref{fmpz_mod_poly}, Ref{fmpz_mod_mat}), p, m)
  return p
end

=#

################################################################################
#
#  Minimal polynomial
#
################################################################################

#= Not implemented in Flint yet

function minpoly(R::FmpzModPolyRing, a::fmpz_mod_mat)
  p = R()
  ccall((:fmpz_mod_mat_minpoly, libflint), Nothing,
          (Ref{fmpz_mod_poly}, Ref{fmpz_mod_mat}), p, a)
  return p
end

=#

###############################################################################
#
#   Promotion rules
#
###############################################################################

promote_rule(::Type{fmpz_mod_mat}, ::Type{V}) where {V <: Integer} = fmpz_mod_mat

promote_rule(::Type{fmpz_mod_mat}, ::Type{fmpz_mod}) = fmpz_mod_mat

promote_rule(::Type{fmpz_mod_mat}, ::Type{fmpz}) = fmpz_mod_mat

################################################################################
#
#  Parent object overloading
#
################################################################################

function (a::FmpzModMatSpace)()
  z = fmpz_mod_mat(nrows(a), ncols(a), a.n)
  z.base_ring = a.base_ring
  return z
end

function (a::FmpzModMatSpace)(b::Integer)
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

function (a::FmpzModMatSpace)(b::fmpz)
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

function (a::FmpzModMatSpace)(b::fmpz_mod)
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

function (a::FmpzModMatSpace)(arr::AbstractArray{BigInt, 2}, transpose::Bool = false)
  _check_dim(nrows(a), ncols(a), arr, transpose)
  z = fmpz_mod_mat(nrows(a), ncols(a), a.n, arr, transpose)
  z.base_ring = a.base_ring
  return z
end

function (a::FmpzModMatSpace)(arr::AbstractArray{BigInt, 1})
  _check_dim(nrows(a), ncols(a), arr)
  z = fmpz_mod_mat(nrows(a), ncols(a), a.n, arr)
  z.base_ring = a.base_ring
  return z
end

function (a::FmpzModMatSpace)(arr::AbstractArray{fmpz, 2}, transpose::Bool = false)
  _check_dim(nrows(a), ncols(a), arr, transpose)
  z = fmpz_mod_mat(nrows(a), ncols(a), a.n, arr, transpose)
  z.base_ring = a.base_ring
  return z
end

function (a::FmpzModMatSpace)(arr::AbstractArray{fmpz, 1})
  _check_dim(nrows(a), ncols(a), arr)
  z = fmpz_mod_mat(nrows(a), ncols(a), a.n, arr)
  z.base_ring = a.base_ring
  return z
end

function (a::FmpzModMatSpace)(arr::AbstractArray{Int, 2}, transpose::Bool = false)
  _check_dim(nrows(a), ncols(a), arr, transpose)
  z = fmpz_mod_mat(nrows(a), ncols(a), a.n, arr, transpose)
  z.base_ring = a.base_ring
  return z
end

function (a::FmpzModMatSpace)(arr::AbstractArray{Int, 1})
  _check_dim(nrows(a), ncols(a), arr)
  z = fmpz_mod_mat(nrows(a), ncols(a), a.n, arr)
  z.base_ring = a.base_ring
  return z
end

function (a::FmpzModMatSpace)(arr::AbstractArray{fmpz_mod, 2}, transpose::Bool = false)
  _check_dim(nrows(a), ncols(a), arr, transpose)
  (length(arr) > 0 && (base_ring(a) != parent(arr[1]))) && error("Elements must have same base ring")
  z = fmpz_mod_mat(nrows(a), ncols(a), a.n, arr, transpose)
  z.base_ring = a.base_ring
  return z
end

function (a::FmpzModMatSpace)(arr::AbstractArray{fmpz_mod, 1})
  _check_dim(nrows(a), ncols(a), arr)
  (length(arr) > 0 && (base_ring(a) != parent(arr[1]))) && error("Elements must have same base ring")
  z = fmpz_mod_mat(nrows(a), ncols(a), a.n, arr)
  z.base_ring = a.base_ring
  return z
end

###############################################################################
#
#   Matrix constructor
#
###############################################################################

function matrix(R::FmpzModRing, arr::AbstractArray{<: Union{fmpz_mod, fmpz, Integer}, 2})
   z = fmpz_mod_mat(size(arr, 1), size(arr, 2), R.n, arr)
   z.base_ring = R
   return z
end

function matrix(R::FmpzModRing, r::Int, c::Int, arr::AbstractArray{<: Union{fmpz_mod, fmpz, Integer}, 1})
   _check_dim(r, c, arr)
   z = fmpz_mod_mat(r, c, R.n, arr)
   z.base_ring = R
   return z
end

###############################################################################
#
#  Zero matrix
#
###############################################################################

function zero_matrix(R::FmpzModRing, r::Int, c::Int)
   if r < 0 || c < 0
     error("dimensions must not be negative")
   end
   z = fmpz_mod_mat(r, c, R.n)
   z.base_ring = R
   return z
end

###############################################################################
#
#  Identity matrix
#
###############################################################################

function identity_matrix(R::FmpzModRing, n::Int)
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

function MatrixSpace(R::FmpzModRing, r::Int, c::Int, cached::Bool = true)
  FmpzModMatSpace(R, r, c, cached)
end

