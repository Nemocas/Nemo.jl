################################################################################
#
#  fq_default_mat.jl: flint fq_default_mat types in julia
#
################################################################################

################################################################################
#
#  Data type and parent object methods
#
################################################################################

parent_type(::Type{FqMatrix}) = FqMatrixSpace

elem_type(::Type{FqMatrixSpace}) = FqMatrix

dense_matrix_type(::Type{FqFieldElem}) = FqMatrix

function check_parent(x::FqMatrix, y::FqMatrix, throw::Bool = true)
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

similar(::FqMatrix, R::FqField, r::Int, c::Int) = FqMatrix(r, c, R)
zero(m::FqMatrix, R::FqField, r::Int, c::Int) = FqMatrix(r, c, R)

################################################################################
#
#  Manipulation
#
################################################################################

function getindex!(v::FqFieldElem, a::FqMatrix, i::Int, j::Int)
   @boundscheck Generic._checkbounds(a, i, j)
   ccall((:fq_default_mat_entry, libflint), Ptr{FqFieldElem},
         (Ref{FqFieldElem}, Ref{FqMatrix}, Int, Int,
          Ref{FqField}),
          v, a, i - 1 , j - 1, base_ring(a))
   return v
end

@inline function getindex(a::FqMatrix, i::Int, j::Int)
   @boundscheck Generic._checkbounds(a, i, j)
   z = base_ring(a)()
   ccall((:fq_default_mat_entry, libflint), Ptr{FqFieldElem},
         (Ref{FqFieldElem}, Ref{FqMatrix}, Int, Int,
          Ref{FqField}),
          z, a, i - 1 , j - 1, base_ring(a))
   return z
end

@inline function setindex!(a::FqMatrix, u::FqFieldElem, i::Int, j::Int)
   @boundscheck Generic._checkbounds(a, i, j)
   ccall((:fq_default_mat_entry_set, libflint), Nothing,
         (Ref{FqMatrix}, Int, Int, Ref{FqFieldElem}, Ref{FqField}),
         a, i - 1, j - 1, u, base_ring(a))
   nothing
end

@inline function setindex!(a::FqMatrix, u::ZZRingElem, i::Int, j::Int)
   @boundscheck Generic._checkbounds(a, i, j)
   ccall((:fq_default_mat_entry_set_fmpz, libflint), Nothing,
         (Ref{FqMatrix}, Int, Int, Ref{ZZRingElem},
          Ref{FqField}),
          a, i - 1, j - 1, u, base_ring(a))
   nothing
end

setindex!(a::FqMatrix, u::Integer, i::Int, j::Int) =
        setindex!(a, base_ring(a)(u), i, j)

function deepcopy_internal(a::FqMatrix, dict::IdDict)
  z = FqMatrix(nrows(a), ncols(a), base_ring(a))
  ccall((:fq_default_mat_set, libflint), Nothing,
        (Ref{FqMatrix}, Ref{FqMatrix}, Ref{FqField}), z, a, base_ring(a))
  return z
end

function number_of_rows(a::FqMatrix)
   return ccall((:fq_default_mat_nrows, libflint), Int,
   (Ref{FqMatrix}, Ref{FqField}),
    a, base_ring(a))
end

function number_of_columns(a::FqMatrix)
   return ccall((:fq_default_mat_ncols, libflint), Int,
   (Ref{FqMatrix}, Ref{FqField}),
    a, base_ring(a))
end

number_of_rows(a::FqMatrixSpace) = a.nrows

number_of_columns(a::FqMatrixSpace) = a.ncols

parent(a::FqMatrix) = matrix_space(base_ring(a), nrows(a), ncols(a))

base_ring(a::FqMatrixSpace) = a.base_ring

base_ring(a::FqMatrix) = a.base_ring

zero(a::FqMatrixSpace) = a()

function one(a::FqMatrixSpace)
  (nrows(a) != ncols(a)) && error("Matrices must be square")
  return a(one(base_ring(a)))
end

function iszero(a::FqMatrix)
   r = ccall((:fq_default_mat_is_zero, libflint), Cint,
             (Ref{FqMatrix}, Ref{FqField}), a, base_ring(a))
  return Bool(r)
end

@inline function is_zero_entry(A::FqMatrix, i::Int, j::Int)
   GC.@preserve A begin
      x = fq_default_mat_entry_ptr(A, i, j)
      return ccall((:fq_default_is_zero, libflint), Bool, (Ptr{FqFieldElem}, Ref{FqField}), x, base_ring(A))
   end
end

################################################################################
#
#  Comparison
#
################################################################################

function ==(a::FqMatrix, b::FqMatrix)
   if !(a.base_ring == b.base_ring)
      return false
   end
   r = ccall((:fq_default_mat_equal, libflint), Cint,
             (Ref{FqMatrix}, Ref{FqMatrix}, Ref{FqField}), a, b, base_ring(a))
   return Bool(r)
end

isequal(a::FqMatrix, b::FqMatrix) = ==(a, b)

################################################################################
#
#  Transpose
#
################################################################################

function transpose(a::FqMatrix)
   z = FqMatrix(ncols(a), nrows(a), base_ring(a))
   for i in 1:nrows(a)
      for j in 1:ncols(a)
         z[j, i] = a[i, j]
      end
   end
   return z
end

# There is no transpose for FqMatrix
#function transpose(a::FqMatrix)
#  z = FqMatrixSpace(base_ring(a), ncols(a), nrows(a))()
#  ccall((:fq_default_mat_transpose, libflint), Nothing,
#        (Ref{FqMatrix}, Ref{FqMatrix}, Ref{FqField}), z, a, base_ring(a))
#  return z
#end
#
#function transpose!(a::FqMatrix)
#  !is_square(a) && error("Matrix must be a square matrix")
#  ccall((:fq_default_mat_transpose, libflint), Nothing,
#        (Ref{FqMatrix}, Ref{FqMatrix}, Ref{FqField}), a, a, base_ring(a))
#end

###############################################################################
#
#   Row and column swapping
#
###############################################################################

function swap_rows!(x::FqMatrix, i::Int, j::Int)
  ccall((:fq_default_mat_swap_rows, libflint), Nothing,
        (Ref{FqMatrix}, Ptr{Nothing}, Int, Int, Ref{FqField}),
        x, C_NULL, i - 1, j - 1, base_ring(x))
  return x
end

function swap_rows(x::FqMatrix, i::Int, j::Int)
   (1 <= i <= nrows(x) && 1 <= j <= nrows(x)) || throw(BoundsError())
   y = deepcopy(x)
   return swap_rows!(y, i, j)
end

function swap_cols!(x::FqMatrix, i::Int, j::Int)
  ccall((:fq_default_mat_swap_cols, libflint), Nothing,
        (Ref{FqMatrix}, Ptr{Nothing}, Int, Int, Ref{FqField}),
        x, C_NULL, i - 1, j - 1, base_ring(x))
  return x
end

function swap_cols(x::FqMatrix, i::Int, j::Int)
   (1 <= i <= ncols(x) && 1 <= j <= ncols(x)) || throw(BoundsError())
   y = deepcopy(x)
   return swap_cols!(y, i, j)
end

function reverse_rows!(x::FqMatrix)
   ccall((:fq_default_mat_invert_rows, libflint), Nothing,
         (Ref{FqMatrix}, Ptr{Nothing}, Ref{FqField}), x, C_NULL, base_ring(x))
   return x
end

reverse_rows(x::FqMatrix) = reverse_rows!(deepcopy(x))

function reverse_cols!(x::FqMatrix)
   ccall((:fq_default_mat_invert_cols, libflint), Nothing,
         (Ref{FqMatrix}, Ptr{Nothing}, Ref{FqField}), x, C_NULL, base_ring(x))
   return x
end

reverse_cols(x::FqMatrix) = reverse_cols!(deepcopy(x))

################################################################################
#
#  Unary operators
#
################################################################################

function -(x::FqMatrix)
   z = similar(x)
   ccall((:fq_default_mat_neg, libflint), Nothing,
         (Ref{FqMatrix}, Ref{FqMatrix}, Ref{FqField}), z, x, base_ring(x))
   return z
end

################################################################################
#
#  Binary operators
#
################################################################################

function +(x::FqMatrix, y::FqMatrix)
   check_parent(x,y)
   z = similar(x)
   ccall((:fq_default_mat_add, libflint), Nothing,
         (Ref{FqMatrix}, Ref{FqMatrix}, Ref{FqMatrix}, Ref{FqField}),
         z, x, y, base_ring(x))
   return z
end

function -(x::FqMatrix, y::FqMatrix)
   check_parent(x,y)
   z = similar(x)
   ccall((:fq_default_mat_sub, libflint), Nothing,
         (Ref{FqMatrix}, Ref{FqMatrix}, Ref{FqMatrix}, Ref{FqField}),
         z, x, y, base_ring(x))

   return z
end

function *(x::FqMatrix, y::FqMatrix)
   (base_ring(x) != base_ring(y)) && error("Base ring must be equal")
   (ncols(x) != nrows(y)) && error("Dimensions are wrong")
   z = similar(x, nrows(x), ncols(y))
   ccall((:fq_default_mat_mul, libflint), Nothing,
         (Ref{FqMatrix}, Ref{FqMatrix}, Ref{FqMatrix}, Ref{FqField}), z, x, y, base_ring(x))
   return z
end


################################################################################
#
#  Unsafe operations
#
################################################################################

function mul!(a::FqMatrix, b::FqMatrix, c::FqMatrix)
   ccall((:fq_default_mat_mul, libflint), Nothing,
         (Ref{FqMatrix}, Ref{FqMatrix}, Ref{FqMatrix}, Ref{FqField}),
         a, b, c, base_ring(a))
  return a
end

function add!(a::FqMatrix, b::FqMatrix, c::FqMatrix)
   ccall((:fq_default_mat_add, libflint), Nothing,
         (Ref{FqMatrix}, Ref{FqMatrix}, Ref{FqMatrix}, Ref{FqField}),
         a, b, c, base_ring(a))
  return a
end

function zero!(a::FqMatrix)
   ccall((:fq_default_mat_zero, libflint), Nothing,
         (Ref{FqMatrix}, Ref{FqField}), a, base_ring(a))
   return a
end

################################################################################
#
#  Ad hoc binary operators
#
################################################################################

function *(x::FqMatrix, y::FqFieldElem)
   z = similar(x)
   for i in 1:nrows(x)
      for j in 1:ncols(x)
         z[i, j] = y * x[i, j]
      end
   end
   return z
end

*(x::FqFieldElem, y::FqMatrix) = y * x

function *(x::FqMatrix, y::ZZRingElem)
   return base_ring(x)(y) * x
end

*(x::ZZRingElem, y::FqMatrix) = y * x

function *(x::FqMatrix, y::Integer)
   return x * base_ring(x)(y)
end

*(x::Integer, y::FqMatrix) = y * x

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

function rref(a::FqMatrix)
   z = deepcopy(a)
   r = ccall((:fq_default_mat_rref, libflint), Int,
             (Ref{FqMatrix}, Ref{FqField}), z, base_ring(a))
   return r, z
end

function rref!(a::FqMatrix)
   r = ccall((:fq_default_mat_rref, libflint), Int,
         (Ref{FqMatrix}, Ref{FqField}), a, base_ring(a))
   return r
end

#################################################################################
#
#  Trace
#
#################################################################################

function tr(a::FqMatrix)
   !is_square(a) && error("Non-square matrix")
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

function det(a::FqMatrix)
   !is_square(a) && error("Non-square matrix")
   n = nrows(a)
   R = base_ring(a)
   if n == 0
      return one(R)
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

function rank(a::FqMatrix)
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

function inv(a::FqMatrix)
   !is_square(a) && error("Matrix must be a square matrix")
   z = similar(a)
   r = ccall((:fq_default_mat_inv, libflint), Int,
             (Ref{FqMatrix}, Ref{FqMatrix}, Ref{FqField}), z, a, base_ring(a))
   !Bool(r) && error("Matrix not invertible")
   return z
end

################################################################################
#
#  Linear solving
#
################################################################################

function Solve._can_solve_internal_no_check(A::FqMatrix, b::FqMatrix, task::Symbol; side::Symbol = :left)
   check_parent(A, b)
   if side === :left
      fl, sol, K = Solve._can_solve_internal_no_check(transpose(A), transpose(b), task, side = :right)
      return fl, transpose(sol), transpose(K)
   end

   x = similar(A, ncols(A), ncols(b))
   fl = ccall((:fq_default_mat_can_solve, libflint), Cint,
             (Ref{FqMatrix}, Ref{FqMatrix}, Ref{FqMatrix},
              Ref{FqField}), x, A, b, base_ring(A))
   if task === :only_check || task === :with_solution
      return Bool(fl), x, zero(A, 0, 0)
   end
   return Bool(fl), x, kernel(A, side = :right)
end

function Solve._init_reduce(C::Solve.SolveCtx{FqFieldElem})
   if isdefined(C, :red) && isdefined(C, :lu_perm)
      return nothing
   end

   LU = deepcopy(matrix(C))
   p = Generic.Perm(1:nrows(LU))
   p.d .-= 1
   r = ccall((:fq_default_mat_lu, libflint), Int,
             (Ptr{Int}, Ref{FqMatrix}, Int, Ref{FqField}),
             p.d, LU, 0, base_ring(C))

   p.d .+= 1
   inv!(p)
   Solve.set_rank!(C, r)
   C.red = LU
   C.lu_perm = p
   if r < nrows(C)
      pA = p*matrix(C)
      set_attribute!(C, :permuted_matrix_lu => view(pA, r + 1:nrows(C), :))
   else
      set_attribute!(C, :permuted_matrix_lu => zero(matrix(C), 0, ncols(C)))
   end
   return nothing
end

function permuted_matrix_lu(C::Solve.SolveCtx{FqFieldElem})
   Solve._init_reduce(C)
   return get_attribute(C, :permuted_matrix_lu)::FqMatrix
end

function Solve._init_reduce_transpose(C::Solve.SolveCtx{FqFieldElem})
   if isdefined(C, :red_transp) && isdefined(C, :lu_perm_transp)
      return nothing
   end

   LU = transpose(matrix(C))
   p = Generic.Perm(1:nrows(LU))
   p.d .-= 1
   r = ccall((:fq_default_mat_lu, libflint), Int,
             (Ptr{Int}, Ref{FqMatrix}, Int, Ref{FqField}),
             p.d, LU, 0, base_ring(C))

   p.d .+= 1
   inv!(p)
   Solve.set_rank!(C, r)
   C.red_transp = LU
   C.lu_perm_transp = p
   if r < ncols(C)
      Ap = matrix(C)*p
      set_attribute!(C, :permuted_matrix_of_transpose_lu => view(Ap, :, r + 1:ncols(C)))
   else
      set_attribute!(C, :permuted_matrix_of_transpose_lu => zero(matrix(C), nrows(C), 0))
   end
   return nothing
end

function permuted_matrix_of_transpose_lu(C::Solve.SolveCtx{FqFieldElem})
   Solve._init_reduce_transpose(C)
   return get_attribute(C, :permuted_matrix_of_transpose_lu)::FqMatrix
end

function Solve._can_solve_internal_no_check(C::Solve.SolveCtx{FqFieldElem}, b::FqMatrix, task::Symbol; side::Symbol = :left)
  # Split up in separate functions to make the compiler happy
  if side === :right
    return Solve._can_solve_internal_no_check_right(C, b, task)
  else
    return Solve._can_solve_internal_no_check_left(C, b, task)
  end
end

function Solve._can_solve_internal_no_check_right(C::Solve.SolveCtx{FqFieldElem}, b::FqMatrix, task::Symbol)
   LU = Solve.reduced_matrix(C)
   p = Solve.lu_permutation(C)
   pb = p*b
   r = rank(C)

   x = similar(b, r, ncols(b))
   # Solve L x = b for the first r rows. We tell flint to pretend that there
   # are ones on the diagonal of LU (fourth argument)
   ccall((:fq_default_mat_solve_tril, libflint), Nothing,
         (Ref{FqMatrix}, Ref{FqMatrix}, Ref{FqMatrix}, Int, Ref{FqField}),
         x, view(LU, 1:r, 1:r), view(pb, 1:r, :), 1, base_ring(C))

   # Check whether x solves Lx = b also for the lower part of L
   if r < nrows(C) && view(LU, r + 1:nrows(LU), 1:r)*x != view(pb, r + 1:nrows(b), :)
     return false, zero(b, 0, 0), zero(b, 0, 0)
   end

   # Solve U y = x. We need to take extra care as U might have non-pivot columns.
   y = _solve_triu_right(view(LU, 1:r, :), x)

   fl = true
   if r < nrows(C)
     fl = permuted_matrix_lu(C)*y == view(pb, r + 1:nrows(C), :)
   end

   if task !== :with_kernel
     return fl, y, zero(b, 0, 0)
   else
     return fl, y, kernel(C, side = :right)
   end
end

function Solve._can_solve_internal_no_check_left(C::Solve.SolveCtx{FqFieldElem}, b::FqMatrix, task::Symbol)
   LU = Solve.reduced_matrix_of_transpose(C)
   p = Solve.lu_permutation_of_transpose(C)
   pbt = p*transpose(b)
   r = rank(C)

   x = similar(b, r, nrows(b))
   ccall((:fq_default_mat_solve_tril, libflint), Nothing,
         (Ref{FqMatrix}, Ref{FqMatrix}, Ref{FqMatrix}, Int, Ref{FqField}),
         x, view(LU, 1:r, 1:r), view(pbt, 1:r, :), 1, base_ring(C))

   # Check whether x solves Lx = b also for the lower part of L
   if r < ncols(C) && view(LU, r + 1:nrows(LU), 1:r)*x != view(pbt, r + 1:nrows(pbt), :)
     return false, zero(b, 0, 0), zero(b, 0, 0)
   end

   # Solve U y = x. We need to take extra care as U might have non-pivot columns.
   yy = _solve_triu_right(view(LU, 1:r, :), x)
   y = transpose(yy)

   fl = true
   if r < ncols(C)
     bp = b*p
     fl = y*permuted_matrix_of_transpose_lu(C) == view(bp, :, r + 1:ncols(C))
   end

   if task !== :with_kernel
     return fl, y, zero(b, 0, 0)
   else
     return fl, y, kernel(C, side = :left)
   end
end

# Solves A x = B with A in upper triangular shape of full rank, so something
# like
#   ( + * * * * )
#   ( 0 0 + * * )
#   ( 0 0 0 + * )
# where + is non-zero and * is anything.
# This is a helper functions because flint can only do the case where the
# diagonal entries are non-zero.
function _solve_triu_right(A::FqMatrix, B::FqMatrix)
   @assert nrows(A) == nrows(B)
   pivot_cols = Int[]
   next_pivot_col = ncols(A) + 1
   for r in nrows(A):-1:1
      for c in r:next_pivot_col - 1
         if is_zero_entry(A, r, c)
            if c == next_pivot_col - 1
               error("Matrix is not in upper triangular shape")
            end
            continue
         end
         push!(pivot_cols, c)
         next_pivot_col = c
         break
      end
   end
   reverse!(pivot_cols)
   AA = reduce(hcat, view(A, 1:nrows(A), c:c) for c in pivot_cols; init = zero(A, nrows(A), 0))
   xx = similar(B, nrows(A), ncols(B))
   ccall((:fq_default_mat_solve_triu, libflint), Nothing,
         (Ref{FqMatrix}, Ref{FqMatrix}, Ref{FqMatrix}, Int, Ref{FqField}),
         xx, AA, B, 0, base_ring(A))
   x = zero(B, ncols(A), ncols(B))
   for i in 1:nrows(xx)
     view(x, pivot_cols[i]:pivot_cols[i], :) .= view(xx, i:i, :)
   end
   return x
end

################################################################################
#
#  LU decomposition
#
################################################################################

function lu!(P::Generic.Perm, x::FqMatrix)
   P.d .-= 1

   rank = Int(ccall((:fq_default_mat_lu, libflint), Cint,
                (Ptr{Int}, Ref{FqMatrix}, Cint, Ref{FqField}),
                P.d, x, 0, base_ring(x)))

  P.d .+= 1

  # flint does x == PLU instead of Px == LU (docs are wrong)
  inv!(P)

  return rank
end

function lu(x::FqMatrix, P = SymmetricGroup(nrows(x)))
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

function Base.view(x::FqMatrix, r1::Int, c1::Int, r2::Int, c2::Int)

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

   z = FqMatrix()
   z.base_ring = x.base_ring
   z.view_parent = x
   ccall((:fq_default_mat_window_init, libflint), Nothing,
         (Ref{FqMatrix}, Ref{FqMatrix}, Int, Int, Int, Int, Ref{FqField}),
         z, x, r1 - 1, c1 - 1, r2, c2, base_ring(x))
   finalizer(_fq_default_mat_window_clear_fn, z)
   return z
end

function Base.view(x::FqMatrix, r::AbstractUnitRange{Int}, c::AbstractUnitRange{Int})
   return Base.view(x, first(r), first(c), last(r), last(c))
end

function _fq_default_mat_window_clear_fn(a::FqMatrix)
   ccall((:fq_default_mat_window_clear, libflint), Nothing,
         (Ref{FqMatrix}, Ref{FqField}), a, base_ring(a))
end

function sub(x::FqMatrix, r1::Int, c1::Int, r2::Int, c2::Int)
  return deepcopy(Base.view(x, r1, c1, r2, c2))
end

function sub(x::FqMatrix, r::AbstractUnitRange{Int}, c::AbstractUnitRange{Int})
  return deepcopy(Base.view(x, r, c))
end

getindex(x::FqMatrix, r::AbstractUnitRange{Int}, c::AbstractUnitRange{Int}) = sub(x, r, c)

################################################################################
#
#  Concatenation
#
################################################################################

function hcat(x::FqMatrix, y::FqMatrix)
   (base_ring(x) != base_ring(y)) && error("Matrices must have same base ring")
   (nrows(x) != nrows(y)) && error("Matrices must have same number of rows")
   z = similar(x, nrows(x), ncols(x) + ncols(y))
   ccall((:fq_default_mat_concat_horizontal, libflint), Nothing,
         (Ref{FqMatrix}, Ref{FqMatrix}, Ref{FqMatrix},
          Ref{FqField}),
         z, x, y, base_ring(x))
   return z
end

function vcat(x::FqMatrix, y::FqMatrix)
   (base_ring(x) != base_ring(y)) && error("Matrices must have same base ring")
   (ncols(x) != ncols(y)) && error("Matrices must have same number of columns")
   z = similar(x, nrows(x) + nrows(y), ncols(x))
   ccall((:fq_default_mat_concat_vertical, libflint), Nothing,
         (Ref{FqMatrix}, Ref{FqMatrix}, Ref{FqMatrix},
          Ref{FqField}),
         z, x, y, base_ring(x))
   return z
end

################################################################################
#
#  Conversion
#
################################################################################

function Array(b::FqMatrix)
  a = Array{FqFieldElem}(undef, nrows(b), ncols(b))
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

function charpoly(R::FqPolyRing, a::FqMatrix)
  !is_square(a) && error("Matrix must be square")
  base_ring(R) != base_ring(a) && error("Must have common base ring")
  p = R()
  ccall((:fq_default_mat_charpoly, libflint), Nothing,
          (Ref{FqPolyRingElem}, Ref{FqMatrix}, Ref{FqField}), p, a, base_ring(a))
  return p
end

function charpoly_danivlesky!(R::FqPolyRing, a::FqMatrix)
  !is_square(a) && error("Matrix must be square")
  base_ring(R) != base_ring(a) && error("Must have common base ring")
  p = R()
  ccall((:fq_default_mat_charpoly_danilevsky, libflint), Nothing,
          (Ref{FqPolyRingElem}, Ref{FqMatrix}, Ref{FqField}), p, a, base_ring(a))
  return p
end


################################################################################
#
#  Minimal polynomial
#
################################################################################

function minpoly(R::FqPolyRing, a::FqMatrix)
  !is_square(a) && error("Matrix must be square")
  base_ring(R) != base_ring(a) && error("Must have common base ring")
  m = deepcopy(a)
  p = R()
  ccall((:fq_default_mat_minpoly, libflint), Nothing,
          (Ref{FqPolyRingElem}, Ref{FqMatrix}, Ref{FqField}), p, m, base_ring(a))
  return p
end

###############################################################################
#
#   Promotion rules
#
###############################################################################

promote_rule(::Type{FqMatrix}, ::Type{V}) where {V <: Integer} = FqMatrix

promote_rule(::Type{FqMatrix}, ::Type{FqFieldElem}) = FqMatrix

promote_rule(::Type{FqMatrix}, ::Type{ZZRingElem}) = FqMatrix

################################################################################
#
#  Parent object overloading
#
################################################################################

function (a::FqMatrixSpace)()
  z = FqMatrix(nrows(a), ncols(a), base_ring(a))
  return z
end

function (a::FqMatrixSpace)(b::Integer)
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

function (a::FqMatrixSpace)(b::ZZRingElem)
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

function (a::FqMatrixSpace)(b::FqFieldElem)
   parent(b) != base_ring(a) && error("Unable to coerce to matrix")
   return FqMatrix(nrows(a), ncols(a), b)
end

function (a::FqMatrixSpace)(arr::AbstractMatrix{T}) where {T <: Integer}
  _check_dim(nrows(a), ncols(a), arr)
  return FqMatrix(nrows(a), ncols(a), arr, base_ring(a))
end

function (a::FqMatrixSpace)(arr::AbstractVector{T}) where {T <: Integer}
  _check_dim(nrows(a), ncols(a), arr)
  return FqMatrix(nrows(a), ncols(a), arr, base_ring(a))
end

function (a::FqMatrixSpace)(arr::AbstractMatrix{ZZRingElem})
  _check_dim(nrows(a), ncols(a), arr)
  return FqMatrix(nrows(a), ncols(a), arr, base_ring(a))
end

function (a::FqMatrixSpace)(arr::AbstractVector{ZZRingElem})
  _check_dim(nrows(a), ncols(a), arr)
  return FqMatrix(nrows(a), ncols(a), arr, base_ring(a))
end

function (a::FqMatrixSpace)(arr::AbstractMatrix{FqFieldElem})
  _check_dim(nrows(a), ncols(a), arr)
  (length(arr) > 0 && (base_ring(a) != parent(arr[1]))) && error("Elements must have same base ring")
  return FqMatrix(nrows(a), ncols(a), arr, base_ring(a))
end

function (a::FqMatrixSpace)(arr::AbstractVector{FqFieldElem})
  _check_dim(nrows(a), ncols(a), arr)
  (length(arr) > 0 && (base_ring(a) != parent(arr[1]))) && error("Elements must have same base ring")
  return FqMatrix(nrows(a), ncols(a), arr, base_ring(a))
end

function (a::FqMatrixSpace)(b::ZZMatrix)
   (ncols(a) != ncols(b) || nrows(a) != nrows(b)) && error("Dimensions do not fit")
   return FqMatrix(b, base_ring(a))
end
 
function (a::FqMatrixSpace)(b::Union{zzModMatrix, fpMatrix})
   characteristic(base_ring(b)) != characteristic(base_ring(a)) &&
                                   error("Incompatible characteristic")
   (ncols(a) != ncols(b) || nrows(a) != nrows(b)) && error("Dimensions do not fit")
   return FqMatrix(b, base_ring(a))
end
 
function (a::FqMatrixSpace)(b::Zmod_fmpz_mat)
   characteristic(base_ring(b)) != characteristic(base_ring(a)) &&
                                   error("Incompatible characteristic")
   (ncols(a) != ncols(b) || nrows(a) != nrows(b)) && error("Dimensions do not fit")
   return FqMatrix(b, base_ring(a))
end
 
 ###############################################################################
#
#   Matrix constructor
#
###############################################################################

function matrix(R::FqField, arr::AbstractMatrix{<: Union{FqFieldElem, ZZRingElem, Integer}})
   z = FqMatrix(size(arr, 1), size(arr, 2), arr, R)
   return z
end

function matrix(R::FqField, r::Int, c::Int, arr::AbstractVector{<: Union{FqFieldElem, ZZRingElem, Integer}})
   _check_dim(r, c, arr)
   z = FqMatrix(r, c, arr, R)
   return z
end

###############################################################################
#
#  Zero matrix
#
###############################################################################

function zero_matrix(R::FqField, r::Int, c::Int)
   if r < 0 || c < 0
     error("dimensions must not be negative")
   end
   z = FqMatrix(r, c, R)
   return z
end

###############################################################################
#
#  Identity matrix
#
###############################################################################

function identity_matrix(R::FqField, n::Int)
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

function matrix_space(R::FqField, r::Int, c::Int; cached::Bool = true)
  # TODO/FIXME: `cached` is ignored and only exists for backwards compatibility
  FqMatrixSpace(R, r, c)
end

################################################################################
#
#  Entry pointers
#
################################################################################

function fq_default_mat_entry_ptr(a::FqMatrix, i, j)
  t = _fq_default_ctx_type(base_ring(a))
  ptr = pointer_from_objref(a)
  if t == _FQ_DEFAULT_FQ_ZECH
    pptr = ccall((:fq_zech_mat_entry, libflint), Ptr{FqFieldElem},
                 (Ptr{Cvoid}, Int, Int), ptr, i - 1, j - 1)
  elseif t == _FQ_DEFAULT_FQ_NMOD
    pptr = ccall((:fq_nmod_mat_entry, libflint), Ptr{FqFieldElem},
                 (Ptr{Cvoid}, Int, Int), ptr, i - 1, j - 1)
  elseif t == _FQ_DEFAULT_FQ
    pptr = ccall((:fq_mat_entry, libflint), Ptr{FqFieldElem},
                 (Ptr{Cvoid}, Int, Int), ptr, i - 1, j - 1)
  elseif t == _FQ_DEFAULT_NMOD
    pptr = ccall((:nmod_mat_entry_ptr, libflint), Ptr{FqFieldElem},
                 (Ptr{Cvoid}, Int, Int), ptr, i - 1, j - 1)
  else#if t == _FQ_DEFAULT_FMPZ_NMOD
    pptr = ccall((:fmpz_mod_mat_entry, libflint), Ptr{FqFieldElem},
                 (Ptr{Cvoid}, Int, Int), ptr, i - 1, j - 1)
  end
  return pptr
end

################################################################################
#
#  Kernel
#
################################################################################

function nullspace(M::FqMatrix)
  N = similar(M, ncols(M), ncols(M))
  nullity = ccall((:fq_default_mat_nullspace, libflint), Int,
                  (Ref{FqMatrix}, Ref{FqMatrix}, Ref{FqField}), N, M, base_ring(M))
  return nullity, view(N, 1:nrows(N), 1:nullity)
end
