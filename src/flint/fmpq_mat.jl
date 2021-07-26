###############################################################################
#
#   fmpq_mat.jl : Flint matrices over the rationals
#
###############################################################################

export fmpq_mat, FmpqMatSpace, gso, hilbert

###############################################################################
#
#   Data type and parent object methods
#
###############################################################################

elem_type(::Type{FmpqMatSpace}) = fmpq_mat

parent_type(::Type{fmpq_mat}) = FmpqMatSpace

base_ring(a::FmpqMatSpace) = a.base_ring

base_ring(a::fmpq_mat) = a.base_ring

dense_matrix_type(::Type{fmpq}) = fmpq_mat

parent(a::fmpq_mat, cached::Bool = true) =
      FmpqMatSpace(nrows(a), ncols(a), cached)

function check_parent(a::fmpq_mat, b::fmpq_mat, throw::Bool = true)
   fl = (nrows(a) != nrows(b) || ncols(a) != ncols(b) || base_ring(a) != base_ring(b))
   fl && throw && error("Incompatible matrices")
   return !fl
end

###############################################################################
#
#   Similar & zero
#
###############################################################################

function similar(::fmpq_mat, R::FlintRationalField, r::Int, c::Int)
   z = fmpq_mat(r, c)
   z.base_ring = R
   return z
end

zero(m::fmpq_mat, R::FlintRationalField, r::Int, c::Int) = similar(m, R, r, c)

###############################################################################
#
#   Windows - handle with care!!!
#
###############################################################################

function Base.view(x::fmpq_mat, r1::Int, c1::Int, r2::Int, c2::Int)
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

  b = fmpq_mat()
  b.view_parent = x
  b.base_ring = x.base_ring
  ccall((:fmpq_mat_window_init, libflint), Nothing,
        (Ref{fmpq_mat}, Ref{fmpq_mat}, Int, Int, Int, Int),
            b, x, r1 - 1, c1 - 1, r2, c2)
  finalizer(_fmpq_mat_window_clear_fn, b)
  return b
end

function Base.view(x::fmpq_mat, r::UnitRange{Int}, c::UnitRange{Int})
  return Base.view(x, r.start, c.start, r.stop, c.stop)
end

function _fmpq_mat_window_clear_fn(a::fmpq_mat)
   ccall((:fmpq_mat_window_clear, libflint), Nothing, (Ref{fmpq_mat},), a)
end

function sub(x::fmpq_mat, r1::Int, c1::Int, r2::Int, c2::Int)
   return deepcopy(view(x, r1, c1, r2, c2))
end

function sub(x::fmpq_mat, r::UnitRange{Int}, c::UnitRange{Int})
   return deepcopy(view(x, r, c))
end

getindex(x::fmpq_mat, r::UnitRange{Int}, c::UnitRange{Int}) = sub(x, r, c)

###############################################################################
#
#   Basic manipulation
#
###############################################################################

function getindex!(v::fmpq, a::fmpq_mat, r::Int, c::Int)
   GC.@preserve a begin
      z = ccall((:fmpq_mat_entry, libflint), Ptr{fmpq},
                (Ref{fmpq_mat}, Int, Int), a, r - 1, c - 1)
      ccall((:fmpq_set, libflint), Nothing, (Ref{fmpq}, Ptr{fmpq}), v, z)
   end
   return v
end

@inline function getindex(a::fmpq_mat, r::Int, c::Int)
   @boundscheck Generic._checkbounds(a, r, c)
   v = fmpq()
   GC.@preserve a begin
      z = ccall((:fmpq_mat_entry, libflint), Ptr{fmpq},
                (Ref{fmpq_mat}, Int, Int), a, r - 1, c - 1)
      ccall((:fmpq_set, libflint), Nothing, (Ref{fmpq}, Ptr{fmpq}), v, z)
   end
   return v
end

@inline function setindex!(a::fmpq_mat, d::fmpz, r::Int, c::Int)
   @boundscheck Generic._checkbounds(a, r, c)
   GC.@preserve a begin
      z = ccall((:fmpq_mat_entry_num, libflint), Ptr{fmpz},
                (Ref{fmpq_mat}, Int, Int), a, r - 1, c - 1)
      ccall((:fmpz_set, libflint), Nothing, (Ptr{fmpz}, Ref{fmpz}), z, d)
      z = ccall((:fmpq_mat_entry_den, libflint), Ptr{fmpz},
                (Ref{fmpq_mat}, Int, Int), a, r - 1, c - 1)
      ccall((:fmpz_set_si, libflint), Nothing, (Ptr{fmpz}, Int), z, 1)
   end
end

@inline function setindex!(a::fmpq_mat, d::fmpq, r::Int, c::Int)
   @boundscheck Generic._checkbounds(a, r, c)
   GC.@preserve a begin
      z = ccall((:fmpq_mat_entry, libflint), Ptr{fmpq},
                (Ref{fmpq_mat}, Int, Int), a, r - 1, c - 1)
      ccall((:fmpq_set, libflint), Nothing, (Ptr{fmpq}, Ref{fmpq}), z, d)
   end
end

Base.@propagate_inbounds setindex!(a::fmpq_mat, d::Integer,
                                 r::Int, c::Int) =
         setindex!(a, fmpq(d), r, c)

@inline function setindex!(a::fmpq_mat, d::Int, r::Int, c::Int)
   @boundscheck Generic._checkbounds(a, r, c)
   GC.@preserve a begin
      z = ccall((:fmpq_mat_entry, libflint), Ptr{fmpq},
                (Ref{fmpq_mat}, Int, Int), a, r - 1, c - 1)
      ccall((:fmpq_set_si, libflint), Nothing, (Ptr{fmpq}, Int, Int), z, d, 1)
   end
end

Base.@propagate_inbounds setindex!(a::fmpq_mat, d::Rational,
                                 r::Int, c::Int) =
         setindex!(a, fmpq(d), r, c)

nrows(a::fmpq_mat) = a.r

ncols(a::fmpq_mat) = a.c

nrows(a::FmpqMatSpace) = a.nrows

ncols(a::FmpqMatSpace) = a.ncols

zero(a::FmpqMatSpace) = a()

one(a::FmpqMatSpace) = a(1)

iszero(a::fmpq_mat) = ccall((:fmpq_mat_is_zero, libflint), Bool,
                            (Ref{fmpq_mat},), a)

isone(a::fmpq_mat) = ccall((:fmpq_mat_is_one, libflint), Bool,
                           (Ref{fmpq_mat},), a)

function deepcopy_internal(d::fmpq_mat, dict::IdDict)
   z = fmpq_mat(d)
   z.base_ring = d.base_ring
   return z
end

###############################################################################
#
#   Canonicalisation
#
###############################################################################

canonical_unit(a::fmpq_mat) = canonical_unit(a[1, 1])

###############################################################################
#
#   AbstractString I/O
#
###############################################################################

###############################################################################
#
#   Unary operations
#
###############################################################################

function -(x::fmpq_mat)
   z = similar(x)
   ccall((:fmpq_mat_neg, libflint), Nothing,
         (Ref{fmpq_mat}, Ref{fmpq_mat}), z, x)
   return z
end

###############################################################################
#
#   transpose
#
###############################################################################

function transpose(x::fmpq_mat)
   z = similar(x, ncols(x), nrows(x))
   ccall((:fmpq_mat_transpose, libflint), Nothing,
         (Ref{fmpq_mat}, Ref{fmpq_mat}), z, x)
   return z
end

###############################################################################
#
#   Row and column swapping
#
###############################################################################

function swap_rows!(x::fmpq_mat, i::Int, j::Int)
  ccall((:fmpq_mat_swap_rows, libflint), Nothing,
        (Ref{fmpq_mat}, Ptr{Nothing}, Int, Int), x, C_NULL, i - 1, j - 1)
  return x
end

function swap_rows(x::fmpq_mat, i::Int, j::Int)
   (1 <= i <= nrows(x) && 1 <= j <= nrows(x)) || throw(BoundsError())
   y = deepcopy(x)
   return swap_rows!(y, i, j)
end

function swap_cols!(x::fmpq_mat, i::Int, j::Int)
  ccall((:fmpq_mat_swap_cols, libflint), Nothing,
        (Ref{fmpq_mat}, Ptr{Nothing}, Int, Int), x, C_NULL, i - 1, j - 1)
  return x
end

function swap_cols(x::fmpq_mat, i::Int, j::Int)
   (1 <= i <= ncols(x) && 1 <= j <= ncols(x)) || throw(BoundsError())
   y = deepcopy(x)
   return swap_cols!(y, i, j)
end

function reverse_rows!(x::fmpq_mat)
   ccall((:fmpq_mat_invert_rows, libflint), Nothing,
         (Ref{fmpq_mat}, Ptr{Nothing}), x, C_NULL)
   return x
end

reverse_rows(x::fmpq_mat) = reverse_rows!(deepcopy(x))

function reverse_cols!(x::fmpq_mat)
   ccall((:fmpq_mat_invert_cols, libflint), Nothing,
         (Ref{fmpq_mat}, Ptr{Nothing}), x, C_NULL)
   return x
end

reverse_cols(x::fmpq_mat) = reverse_cols!(deepcopy(x))

###############################################################################
#
#   Binary operations
#
###############################################################################

function +(x::fmpq_mat, y::fmpq_mat)
   check_parent(x, y)
   z = similar(x)
   ccall((:fmpq_mat_add, libflint), Nothing,
                (Ref{fmpq_mat}, Ref{fmpq_mat},  Ref{fmpq_mat}),
               z, x, y)
   return z
end

function -(x::fmpq_mat, y::fmpq_mat)
   check_parent(x, y)
   z = similar(x)
   ccall((:fmpq_mat_sub, libflint), Nothing,
                (Ref{fmpq_mat}, Ref{fmpq_mat},  Ref{fmpq_mat}),
               z, x, y)
   return z
end

function *(x::fmpq_mat, y::fmpq_mat)
   ncols(x) != nrows(y) && error("Incompatible matrix dimensions")
   z = similar(x, nrows(x), ncols(y))
   ccall((:fmpq_mat_mul, libflint), Nothing,
                (Ref{fmpq_mat}, Ref{fmpq_mat},  Ref{fmpq_mat}),
               z, x, y)
   return z
end

###############################################################################
#
#   Ad hoc binary operators
#
###############################################################################

function *(x::fmpz, y::fmpq_mat)
   z = similar(y)
   ccall((:fmpq_mat_scalar_mul_fmpz, libflint), Nothing,
                (Ref{fmpq_mat}, Ref{fmpq_mat}, Ref{fmpz}), z, y, x)
   return z
end

function *(x::fmpq, y::fmpq_mat)
   z = similar(y)
   ccall((:fmpq_mat_scalar_mul_fmpz, libflint), Nothing,
                (Ref{fmpq_mat}, Ref{fmpq_mat}, Ref{fmpq}), z, y, numerator(x))
   ccall((:fmpq_mat_scalar_div_fmpz, libflint), Nothing,
                (Ref{fmpq_mat}, Ref{fmpq_mat}, Ref{fmpq}), z, z, denominator(x))
   return z
end

*(x::fmpq_mat, y::fmpq) = y*x

*(x::fmpq_mat, y::fmpz) = y*x

*(x::Integer, y::fmpq_mat) = fmpz(x)*y

*(x::fmpq_mat, y::Integer) = fmpz(y)*x

*(x::Rational, y::fmpq_mat) = fmpq(x)*y

*(x::fmpq_mat, y::Rational) = fmpq(y)*x

for T in [Integer, fmpz, fmpq]
   @eval begin
      function +(x::fmpq_mat, y::$T)
         z = deepcopy(x)
         for i = 1:min(nrows(x), ncols(x))
            z[i, i] += y
         end
         return z
      end

      +(x::$T, y::fmpq_mat) = y + x

      function -(x::fmpq_mat, y::$T)
         z = deepcopy(x)
         for i = 1:min(nrows(x), ncols(x))
            z[i, i] -= y
         end
         return z
      end

      function -(x::$T, y::fmpq_mat)
         z = -y
         for i = 1:min(nrows(y), ncols(y))
            z[i, i] += x
         end
         return z
      end
   end
end

function +(x::fmpq_mat, y::Rational)
   z = deepcopy(x)
   for i = 1:min(nrows(x), ncols(x))
      z[i, i] += y
   end
   return z
end

+(x::Rational, y::fmpq_mat) = y + x

function -(x::fmpq_mat, y::Rational)
   z = deepcopy(x)
   for i = 1:min(nrows(x), ncols(x))
      z[i, i] -= y
   end
   return z
end

function -(x::Rational, y::fmpq_mat)
   z = -y
   for i = 1:min(nrows(y), ncols(y))
      z[i, i] += x
   end
   return z
end

###############################################################################
#
#   Comparisons
#
###############################################################################

function ==(x::fmpq_mat, y::fmpq_mat)
   fl = check_parent(x, y, false)
   fl && ccall((:fmpq_mat_equal, libflint), Bool,
                                       (Ref{fmpq_mat}, Ref{fmpq_mat}), x, y)
end

isequal(x::fmpq_mat, y::fmpq_mat) = ==(x, y)

###############################################################################
#
#   Ad hoc comparisons
#
###############################################################################

function ==(x::fmpq_mat, y::Integer)
   for i = 1:min(nrows(x), ncols(x))
      if x[i, i] != y
         return false
      end
   end
   for i = 1:nrows(x)
      for j = 1:ncols(x)
         if i != j && x[i, j] != 0
            return false
         end
      end
   end
   return true
end

==(x::Integer, y::fmpq_mat) = y == x

==(x::fmpq_mat, y::Rational{T}) where T <: Union{Int, BigInt} = x == fmpq(y)

==(x::Rational{T}, y::fmpq_mat) where T <: Union{Int, BigInt} = y == x

###############################################################################
#
#   Inversion
#
###############################################################################

function inv(x::fmpq_mat)
   z = similar(x)
   success = ccall((:fmpq_mat_inv, libflint), Cint,
         (Ref{fmpq_mat}, Ref{fmpq_mat}), z, x)
   success == 0 && error("Matrix not invertible")
   return z
end

###############################################################################
#
#   Exact division
#
###############################################################################

function divexact(x::fmpq_mat, y::fmpq_mat)
   ncols(x) != ncols(y) && error("Incompatible matrix dimensions")
   x*inv(y)
end

###############################################################################
#
#   Ad hoc exact division
#
###############################################################################

function divexact(x::fmpq_mat, y::fmpq)
   z = similar(x)
   ccall((:fmpq_mat_scalar_div_fmpz, libflint), Nothing,
                (Ref{fmpq_mat}, Ref{fmpq_mat}, Ref{fmpz}), z, x, numerator(y))
   ccall((:fmpq_mat_scalar_mul_fmpz, libflint), Nothing,
                (Ref{fmpq_mat}, Ref{fmpq_mat}, Ref{fmpz}), z, z, denominator(y))
   return z
end

function divexact(x::fmpq_mat, y::fmpz)
   z = similar(x)
   ccall((:fmpq_mat_scalar_div_fmpz, libflint), Nothing,
                (Ref{fmpq_mat}, Ref{fmpq_mat}, Ref{fmpz}), z, x, y)
   return z
end

divexact(x::fmpq_mat, y::Integer) = divexact(x, fmpz(y))

divexact(x::fmpq_mat, y::Rational{T}) where T <: Union{Int, BigInt} = divexact(x, fmpq(y))

###############################################################################
#
#   Kronecker product
#
###############################################################################

function kronecker_product(x::fmpq_mat, y::fmpq_mat)
   base_ring(x) == base_ring(y) || error("Incompatible matrices")
   z = similar(x, nrows(x)*nrows(y), ncols(x)*ncols(y))
   ccall((:fmpq_mat_kronecker_product, libflint), Nothing,
                (Ref{fmpq_mat}, Ref{fmpq_mat}, Ref{fmpq_mat}), z, x, y)
   return z
end

###############################################################################
#
#   Characteristic polynomial
#
###############################################################################

function charpoly(R::FmpqPolyRing, x::fmpq_mat)
   nrows(x) != ncols(x) && error("Non-square")
   z = R()
   ccall((:fmpq_mat_charpoly, libflint), Nothing,
                (Ref{fmpq_poly}, Ref{fmpq_mat}), z, x)
   return z
end

###############################################################################
#
#   Minimal polynomial
#
###############################################################################

function minpoly(R::FmpqPolyRing, x::fmpq_mat)
   nrows(x) != ncols(x) && error("Non-square")
   z = R()
   ccall((:fmpq_mat_minpoly, libflint), Nothing,
                (Ref{fmpq_poly}, Ref{fmpq_mat}), z, x)
   return z
end

###############################################################################
#
#   Determinant
#
###############################################################################

function det(x::fmpq_mat)
   nrows(x) != ncols(x) && error("Non-square matrix")
   z = fmpq()
   ccall((:fmpq_mat_det, libflint), Nothing,
                (Ref{fmpq}, Ref{fmpq_mat}), z, x)
   return z
end

###############################################################################
#
#   Gram-Schmidt orthogonalisation
#
###############################################################################

@doc Markdown.doc"""
    gso(x::fmpq_mat)

Return the Gram-Schmidt Orthogonalisation of the matrix $x$.
"""
function gso(x::fmpq_mat)
   z = similar(x)
   ccall((:fmpq_mat_gso, libflint), Nothing,
                (Ref{fmpq_mat}, Ref{fmpq_mat}), z, x)
   return z
end

###############################################################################
#
#   Hilbert matrix
#
###############################################################################

@doc Markdown.doc"""
    hilbert(R::FmpqMatSpace)

Return the Hilbert matrix in the given matrix space. This is the matrix with
entries $H_{i,j} = 1/(i + j - 1)$.
"""
function hilbert(R::FmpqMatSpace)
   z = R()
   ccall((:fmpq_mat_hilbert_matrix, libflint), Bool,
                   (Ref{fmpq_mat},), z)
   return z
end

###############################################################################
#
#   Rank
#
###############################################################################

function rank(x::fmpq_mat)
   z = similar(x)
   r = ccall((:fmpq_mat_rref, libflint), Int,
         (Ref{fmpq_mat}, Ref{fmpq_mat}), z, x)
   return r
end

###############################################################################
#
#   Reduced row echelon form
#
###############################################################################

function rref(x::fmpq_mat)
   z = similar(x)
   r = ccall((:fmpq_mat_rref, libflint), Int,
         (Ref{fmpq_mat}, Ref{fmpq_mat}), z, x)
   return r, z
end

###############################################################################
#
#   Linear solving
#
###############################################################################

function solve(a::fmpq_mat, b::fmpq_mat)
   nrows(a) != ncols(a) && error("Not a square matrix in solve")
   nrows(b) != nrows(a) && error("Incompatible dimensions in solve")
   z = similar(b)
   nonsing = ccall((:fmpq_mat_solve, libflint), Bool,
      (Ref{fmpq_mat}, Ref{fmpq_mat}, Ref{fmpq_mat}), z, a, b)
   !nonsing && error("Singular matrix in solve")
   return z
end

@doc Markdown.doc"""
    solve_dixon(a::fmpq_mat, b::fmpq_mat)

Solve $ax = b$ by clearing denominators and using Dixon's algorithm. This is
usually faster for large systems.
"""
function solve_dixon(a::fmpq_mat, b::fmpq_mat)
   nrows(a) != ncols(a) && error("Not a square matrix in solve")
   nrows(b) != nrows(a) && error("Incompatible dimensions in solve")
   z = similar(b)
   nonsing = ccall((:fmpq_mat_solve_dixon, libflint), Bool,
      (Ref{fmpq_mat}, Ref{fmpq_mat}, Ref{fmpq_mat}), z, a, b)
   !nonsing && error("Singular matrix in solve")
   return z
end

function can_solve_with_solution(a::fmpq_mat, b::fmpq_mat; side::Symbol = :right)
   if side == :left
      (ncols(a) != ncols(b)) && error("Matrices must have same number of columns")
      (f, x) = can_solve_with_solution(a', b'; side=:right)
      return (f, x')
   elseif side == :right
      (nrows(a) != nrows(b)) && error("Matrices must have same number of rows")
      x = similar(a, ncols(a), ncols(b))
      r = ccall((:fmpq_mat_can_solve_multi_mod, libflint), Cint,
                (Ref{fmpq_mat}, Ref{fmpq_mat}, Ref{fmpq_mat}), x, a, b)
      return Bool(r), x
   else
      error("Unsupported argument :$side for side: Must be :left or :right.")
   end
end

function can_solve(a::fmpq_mat, b::fmpq_mat; side::Symbol = :right)
   fl, _ = can_solve_with_solution(a, b, side = side)
   return fl
end

###############################################################################
#
#   Trace
#
###############################################################################

function tr(x::fmpq_mat)
   nrows(x) != ncols(x) && error("Not a square matrix in trace")
   d = fmpq()
   ccall((:fmpq_mat_trace, libflint), Nothing,
                (Ref{fmpq}, Ref{fmpq_mat}), d, x)
   return d
end

###############################################################################
#
#   Concatenation
#
###############################################################################

function hcat(a::fmpq_mat, b::fmpq_mat)
  nrows(a) != nrows(b) && error("Incompatible number of rows in hcat")
  c = similar(a, nrows(a), ncols(a) + ncols(b))
  ccall((:fmpq_mat_concat_horizontal, libflint), Nothing,
        (Ref{fmpq_mat}, Ref{fmpq_mat}, Ref{fmpq_mat}), c, a, b)
  return c
end

function vcat(a::fmpq_mat, b::fmpq_mat)
  ncols(a) != ncols(b) && error("Incompatible number of columns in vcat")
  c = similar(a, nrows(a) + nrows(b), ncols(a))
  ccall((:fmpq_mat_concat_vertical, libflint), Nothing,
        (Ref{fmpq_mat}, Ref{fmpq_mat}, Ref{fmpq_mat}), c, a, b)
  return c
end

###############################################################################
#
#   Similarity
#
###############################################################################

function similarity!(z::fmpq_mat, r::Int, d::fmpq)
   ccall((:fmpq_mat_similarity, libflint), Nothing,
         (Ref{fmpq_mat}, Int, Ref{fmpq}), z, r - 1, d)
end

###############################################################################
#
#   Unsafe functions
#
###############################################################################

function mul!(z::fmpq_mat, x::fmpq_mat, y::fmpq_mat)
   ccall((:fmpq_mat_mul, libflint), Nothing,
                (Ref{fmpq_mat}, Ref{fmpq_mat}, Ref{fmpq_mat}), z, x, y)
   return z
end

function add!(z::fmpq_mat, x::fmpq_mat, y::fmpq_mat)
   ccall((:fmpq_mat_add, libflint), Nothing,
                (Ref{fmpq_mat}, Ref{fmpq_mat}, Ref{fmpq_mat}), z, x, y)
   return z
end

function mul!(y::fmpq_mat, x::Int)
   ccall((:fmpq_mat_scalar_mul_fmpz, libflint), Nothing,
                (Ref{fmpq_mat}, Ref{fmpq_mat}, Ref{fmpq}), y, y, fmpz(x))
   return y
end

function mul!(y::fmpq_mat, x::fmpz)
   ccall((:fmpq_mat_scalar_mul_fmpz, libflint), Nothing,
                (Ref{fmpq_mat}, Ref{fmpq_mat}, Ref{fmpz}), y, y, x)
   return y
end

function addeq!(z::fmpq_mat, x::fmpq_mat)
   ccall((:fmpq_mat_add, libflint), Nothing,
                (Ref{fmpq_mat}, Ref{fmpq_mat}, Ref{fmpq_mat}), z, z, x)
   return z
end

function zero!(z::fmpq_mat)
   ccall((:fmpq_mat_zero, libflint), Nothing,
                (Ref{fmpq_mat},), z)
   return z
end

###############################################################################
#
#   Parent object call overloads
#
###############################################################################

function (a::FmpqMatSpace)()
   z = fmpq_mat(nrows(a), ncols(a))
   z.base_ring = a.base_ring
   return z
end

function (a::FmpqMatSpace)(arr::AbstractMatrix{fmpq})
   _check_dim(nrows(a), ncols(a), arr)
   z = fmpq_mat(nrows(a), ncols(a), arr)
   z.base_ring = a.base_ring
   return z
end

function (a::FmpqMatSpace)(arr::AbstractMatrix{fmpz})
   _check_dim(nrows(a), ncols(a), arr)
   z = fmpq_mat(nrows(a), ncols(a), arr)
   z.base_ring = a.base_ring
   return z
end


function (a::FmpqMatSpace)(arr::AbstractMatrix{T}) where {T <: Integer}
   _check_dim(nrows(a), ncols(a), arr)
   z = fmpq_mat(nrows(a), ncols(a), arr)
   z.base_ring = a.base_ring
   return z
end

function (a::FmpqMatSpace)(arr::AbstractMatrix{Rational{T}}) where {T <: Integer}
   _check_dim(nrows(a), ncols(a), arr)
   z = fmpq_mat(nrows(a), ncols(a), map(fmpq, arr))
   z.base_ring = a.base_ring
   return z
end

function (a::FmpqMatSpace)(arr::AbstractVector{fmpq})
   _check_dim(nrows(a), ncols(a), arr)
   z = fmpq_mat(nrows(a), ncols(a), arr)
   z.base_ring = a.base_ring
   return z
end

function (a::FmpqMatSpace)(arr::AbstractVector{fmpz})
   _check_dim(nrows(a), ncols(a), arr)
   z = fmpq_mat(nrows(a), ncols(a), arr)
   z.base_ring = a.base_ring
   return z
end

function (a::FmpqMatSpace)(arr::AbstractVector{T}) where {T <: Integer}
   _check_dim(nrows(a), ncols(a), arr)
   z = fmpq_mat(nrows(a), ncols(a), arr)
   z.base_ring = a.base_ring
   return z
end

function (a::FmpqMatSpace)(arr::AbstractVector{Rational{T}}) where {T <: Integer}
   _check_dim(nrows(a), ncols(a), arr)
   z = fmpq_mat(nrows(a), ncols(a), map(fmpq, arr))
   z.base_ring = a.base_ring
   return z
end

function (a::FmpqMatSpace)(d::fmpq)
   z = fmpq_mat(nrows(a), ncols(a), d)
   z.base_ring = a.base_ring
   return z
end

function (a::FmpqMatSpace)(d::fmpz)
   z = fmpq_mat(nrows(a), ncols(a), fmpq(d))
   z.base_ring = a.base_ring
   return z
end

function (a::FmpqMatSpace)(d::Integer)
   z = fmpq_mat(nrows(a), ncols(a), fmpq(d))
   z.base_ring = a.base_ring
   return z
end

(a::FmpqMatSpace)(d::Rational) = a(fmpq(d))

function (a::FmpqMatSpace)(M::fmpz_mat)
   (ncols(a) == ncols(M) && nrows(a) == nrows(M)) || error("wrong matrix dimension")
   z = a()
   ccall((:fmpq_mat_set_fmpz_mat, libflint), Nothing, (Ref{fmpq_mat}, Ref{fmpz_mat}), z, M)
   return z
end

(a::FmpqMatSpace)(d::fmpq_mat) = d

###############################################################################
#
#   Promotions
#
###############################################################################

promote_rule(::Type{fmpq_mat}, ::Type{T}) where {T <: Integer} = fmpq_mat

promote_rule(::Type{fmpq_mat}, ::Type{fmpq}) = fmpq_mat

promote_rule(::Type{fmpq_mat}, ::Type{fmpz}) = fmpq_mat

promote_rule(::Type{fmpq_mat}, ::Type{Rational{T}}) where T <: Union{Int, BigInt} = fmpq_mat

###############################################################################
#
#   Matrix constructor
#
###############################################################################

function matrix(R::FlintRationalField, arr::AbstractMatrix{fmpq})
   z = fmpq_mat(size(arr, 1), size(arr, 2), arr)
   z.base_ring = FlintQQ
   return z
end

function matrix(R::FlintRationalField, arr::AbstractMatrix{<: Union{fmpz, Int, BigInt}})
   z = fmpq_mat(size(arr, 1), size(arr, 2), arr)
   z.base_ring = FlintQQ
   return z
end

function matrix(R::FlintRationalField, arr::AbstractMatrix{Rational{T}}) where {T <: Integer}
   z = fmpq_mat(size(arr, 1), size(arr, 2), map(fmpq, arr))
   z.base_ring = FlintQQ
   return z
end

function matrix(R::FlintRationalField, r::Int, c::Int, arr::AbstractVector{fmpq})
   _check_dim(r, c, arr)
   z = fmpq_mat(r, c, arr)
   z.base_ring = FlintQQ
   return z
end

function matrix(R::FlintRationalField, r::Int, c::Int, arr::AbstractVector{<: Union{fmpz, Int, BigInt}})
   _check_dim(r, c, arr)
   z = fmpq_mat(r, c, arr)
   z.base_ring = FlintQQ
   return z
end

function matrix(R::FlintRationalField, r::Int, c::Int, arr::AbstractVector{Rational{T}}) where {T <: Union{fmpz, Int, BigInt}}
   _check_dim(r, c, arr)
   z = fmpq_mat(r, c, map(fmpq, arr))
   z.base_ring = FlintQQ
   return z
end

###############################################################################
#
#  Zero matrix
#
###############################################################################

function zero_matrix(R::FlintRationalField, r::Int, c::Int)
   if r < 0 || c < 0
     error("dimensions must not be negative")
   end
   z = fmpq_mat(r, c)
   z.base_ring = FlintQQ
   return z
end

###############################################################################
#
#  Identity matrix
#
###############################################################################

function identity_matrix(R::FlintRationalField, n::Int)
   if n < 0
     error("dimension must not be negative")
   end
   z = fmpq_mat(n, n)
   ccall((:fmpq_mat_one, libflint), Nothing, (Ref{fmpq_mat}, ), z)
   z.base_ring = FlintQQ
   return z
end

###############################################################################
#
#   MatrixSpace constructor
#
###############################################################################

function MatrixSpace(R::FlintRationalField, r::Int, c::Int; cached = true)
   return FmpqMatSpace(r, c, cached)
end
