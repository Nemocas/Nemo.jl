###############################################################################
#
#   ZZMatrix.jl : Flint matrices over ZZRingElem
#
###############################################################################

###############################################################################
#
#   Data type and parent object methods
#
###############################################################################

elem_type(::Type{ZZMatrixSpace}) = ZZMatrix

parent_type(::Type{ZZMatrix}) = ZZMatrixSpace

base_ring(a::ZZMatrixSpace) = FlintZZ

base_ring(a::ZZMatrix) = FlintZZ

dense_matrix_type(::Type{ZZRingElem}) = ZZMatrix

parent(a::ZZMatrix) = matrix_space(base_ring(a), nrows(a), ncols(a))

function check_parent(a::ZZMatrix, b::ZZMatrix, throw::Bool = true)
   b = (nrows(a) != nrows(b) || ncols(a) != ncols(b))
   b && throw && error("Incompatible matrices")
   return !b
end

###############################################################################
#
#   similar & zero
#
###############################################################################

function similar(::ZZMatrix, R::ZZRing, r::Int, c::Int)
   z = ZZMatrix(r, c)
   return z
end

zero(m::ZZMatrix, R::ZZRing, r::Int, c::Int) = similar(m, R, r, c)

###############################################################################
#
#   View and sub
#
###############################################################################

function _checkrange_or_empty(l::Int, start::Int, stop::Int)
   (stop < start) ||
   (Generic._checkbounds(l, start) &&
    Generic._checkbounds(l, stop))
end

function Base.view(x::ZZMatrix, r1::Int, c1::Int, r2::Int, c2::Int)

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

   b = ZZMatrix()
   b.view_parent = x
   ccall((:fmpz_mat_window_init, libflint), Nothing,
         (Ref{ZZMatrix}, Ref{ZZMatrix}, Int, Int, Int, Int),
             b, x, r1 - 1, c1 - 1, r2, c2)
   finalizer(_fmpz_mat_window_clear_fn, b)
   return b
end

function Base.reshape(x::ZZMatrix, r::Int, c::Int)
  @assert nrows(x) * ncols(x) == r*c
  @assert r == 1

   b = ZZMatrix()
   b.view_parent = x
   ccall((:fmpz_mat_window_init, libflint), Nothing,
         (Ref{ZZMatrix}, Ref{ZZMatrix}, Int, Int, Int, Int),
             b, x, 0, 0, r, c)
   finalizer(_fmpz_mat_window_clear_fn, b)
   return b
end


function Base.view(x::ZZMatrix, r::UnitRange{Int}, c::UnitRange{Int})
   return Base.view(x, r.start, c.start, r.stop, c.stop)
end

function _fmpz_mat_window_clear_fn(a::ZZMatrix)
   ccall((:fmpz_mat_window_clear, libflint), Nothing, (Ref{ZZMatrix},), a)
end

function sub(x::ZZMatrix, r1::Int, c1::Int, r2::Int, c2::Int)
   return deepcopy(view(x, r1, c1, r2, c2))
end

function sub(x::ZZMatrix, r::AbstractUnitRange{Int}, c::AbstractUnitRange{Int})
   return deepcopy(view(x, r, c))
end

getindex(x::ZZMatrix, r::AbstractUnitRange{Int}, c::AbstractUnitRange{Int}) = sub(x, r, c)

###############################################################################
#
#   Basic manipulation
#
###############################################################################

function getindex!(v::ZZRingElem, a::ZZMatrix, r::Int, c::Int)
   GC.@preserve a begin
      z = ccall((:fmpz_mat_entry, libflint), Ptr{ZZRingElem},
                (Ref{ZZMatrix}, Int, Int), a, r - 1, c - 1)
      ccall((:fmpz_set, libflint), Nothing, (Ref{ZZRingElem}, Ptr{ZZRingElem}), v, z)
   end
end

@inline function getindex(a::ZZMatrix, r::Int, c::Int)
   @boundscheck Generic._checkbounds(a, r, c)
   v = ZZRingElem()
   GC.@preserve a begin
      z = ccall((:fmpz_mat_entry, libflint), Ptr{ZZRingElem},
                (Ref{ZZMatrix}, Int, Int), a, r - 1, c - 1)
      ccall((:fmpz_set, libflint), Nothing, (Ref{ZZRingElem}, Ptr{ZZRingElem}), v, z)
   end
   return v
end

@inline function setindex!(a::ZZMatrix, d::ZZRingElem, r::Int, c::Int)
   @boundscheck Generic._checkbounds(a, r, c)
   GC.@preserve a begin
      z = ccall((:fmpz_mat_entry, libflint), Ptr{ZZRingElem},
                (Ref{ZZMatrix}, Int, Int), a, r - 1, c - 1)
      ccall((:fmpz_set, libflint), Nothing, (Ptr{ZZRingElem}, Ref{ZZRingElem}), z, d)
   end
end

@inline setindex!(a::ZZMatrix, d::Integer, r::Int, c::Int) = setindex!(a, ZZRingElem(d), r, c)

@inline function setindex!(a::ZZMatrix, d::Int, r::Int, c::Int)
   @boundscheck Generic._checkbounds(a, r, c)
   GC.@preserve a begin
      z = ccall((:fmpz_mat_entry, libflint), Ptr{ZZRingElem},
                (Ref{ZZMatrix}, Int, Int), a, r - 1, c - 1)
      ccall((:fmpz_set_si, libflint), Nothing, (Ptr{ZZRingElem}, Int), z, d)
   end
end

function setindex!(a::ZZMatrix, b::ZZMatrix, r::UnitRange{Int64}, c::UnitRange{Int64})
  _checkbounds(a, r, c)
  size(b) == (length(r), length(c)) || throw(DimensionMismatch("tried to assign a $(size(b, 1))x$(size(b, 2)) matrix to a $(length(r))x$(length(c)) destination"))
  A = view(a, r, c)
  ccall((:fmpz_mat_set, libflint), Nothing,
        (Ref{ZZMatrix}, Ref{ZZMatrix}), A, b)
end

@inline number_of_rows(a::ZZMatrix) = a.r

@inline number_of_columns(a::ZZMatrix) = a.c

number_of_rows(a::ZZMatrixSpace) = a.nrows

number_of_columns(a::ZZMatrixSpace) = a.ncols

zero(a::ZZMatrixSpace) = a()

one(a::ZZMatrixSpace) = a(1)

iszero(a::ZZMatrix) = ccall((:fmpz_mat_is_zero, libflint), Bool,
                            (Ref{ZZMatrix},), a)

isone(a::ZZMatrix) = ccall((:fmpz_mat_is_one, libflint), Bool,
                           (Ref{ZZMatrix},), a)

@inline function is_zero_entry(A::ZZMatrix, i::Int, j::Int)
   @boundscheck Generic._checkbounds(A, i, j)
   GC.@preserve A begin
      x = mat_entry_ptr(A, i, j)
      return ccall((:fmpz_is_zero, libflint), Bool, (Ptr{ZZRingElem},), x)
   end
end

function is_positive_entry(M::ZZMatrix, i::Int, j::Int)
    GC.@preserve M begin
        m = mat_entry_ptr(M, i, j)
        fl = ccall((:fmpz_sgn, libflint), Int, (Ptr{ZZRingElem},), m)
        return isone(fl)
    end
end

function deepcopy_internal(d::ZZMatrix, dict::IdDict)
   z = ZZMatrix(d)
   return z
end

# This function is dirty because it relies on the internals of ZZMatrix.
# This function needs to be changed if the internals ever change.
function Base.hash(a::ZZMatrix, h::UInt)
   GC.@preserve a begin
      r = nrows(a)
      c = ncols(a)
      h = hash(r, h)
      h = hash(c, h)
      rowptr = convert(Ptr{Ptr{Int}}, a.rows)
      for i in 1:r
         h = _hash_integer_array(unsafe_load(rowptr, i), c, h)
      end
      return xor(h, 0x5c22af6d5986f453%UInt)
   end
end

###############################################################################
#
#   Canonicalisation
#
###############################################################################

canonical_unit(a::ZZMatrix) = canonical_unit(a[1, 1])

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

function -(x::ZZMatrix)
   z = similar(x)
   ccall((:fmpz_mat_neg, libflint), Nothing,
         (Ref{ZZMatrix}, Ref{ZZMatrix}), z, x)
   return z
end

###############################################################################
#
#   transpose
#
###############################################################################

function transpose(x::ZZMatrix)
   z = similar(x, ncols(x), nrows(x))
   ccall((:fmpz_mat_transpose, libflint), Nothing,
         (Ref{ZZMatrix}, Ref{ZZMatrix}), z, x)
   return z
end

function transpose!(A::ZZMatrix, B::ZZMatrix)
    ccall((:fmpz_mat_transpose, libflint), Nothing,
        (Ref{ZZMatrix}, Ref{ZZMatrix}), A, B)
    return A
end

###############################################################################
#
#   Row and column swapping
#
###############################################################################

function swap_rows!(x::ZZMatrix, i::Int, j::Int)
  ccall((:fmpz_mat_swap_rows, libflint), Nothing,
        (Ref{ZZMatrix}, Ptr{Nothing}, Int, Int), x, C_NULL, i - 1, j - 1)
  return x
end

function swap_rows(x::ZZMatrix, i::Int, j::Int)
   (1 <= i <= nrows(x) && 1 <= j <= nrows(x)) || throw(BoundsError())
   y = deepcopy(x)
   return swap_rows!(y, i, j)
end

function swap_cols!(x::ZZMatrix, i::Int, j::Int)
  ccall((:fmpz_mat_swap_cols, libflint), Nothing,
        (Ref{ZZMatrix}, Ptr{Nothing}, Int, Int), x, C_NULL, i - 1, j - 1)
  return x
end

function swap_cols(x::ZZMatrix, i::Int, j::Int)
   (1 <= i <= ncols(x) && 1 <= j <= ncols(x)) || throw(BoundsError())
   y = deepcopy(x)
   return swap_cols!(y, i, j)
end

function reverse_rows!(x::ZZMatrix)
   ccall((:fmpz_mat_invert_rows, libflint), Nothing,
         (Ref{ZZMatrix}, Ptr{Nothing}), x, C_NULL)
   return x
end

reverse_rows(x::ZZMatrix) = reverse_rows!(deepcopy(x))

function reverse_cols!(x::ZZMatrix)
   ccall((:fmpz_mat_invert_cols, libflint), Nothing,
         (Ref{ZZMatrix}, Ptr{Nothing}), x, C_NULL)
   return x
end

reverse_cols(x::ZZMatrix) = reverse_cols!(deepcopy(x))

###############################################################################
#
#   Binary operations
#
###############################################################################

function +(x::ZZMatrix, y::ZZMatrix)
   check_parent(x, y)
   z = similar(x)
   ccall((:fmpz_mat_add, libflint), Nothing,
                (Ref{ZZMatrix}, Ref{ZZMatrix},  Ref{ZZMatrix}),
               z, x, y)
   return z
end

function -(x::ZZMatrix, y::ZZMatrix)
   check_parent(x, y)
   z = similar(x)
   ccall((:fmpz_mat_sub, libflint), Nothing,
                (Ref{ZZMatrix}, Ref{ZZMatrix},  Ref{ZZMatrix}),
               z, x, y)
   return z
end

function *(x::ZZMatrix, y::ZZMatrix)
   ncols(x) != nrows(y) && error("Incompatible matrix dimensions")
   if nrows(x) == ncols(y) && nrows(x) == ncols(x)
      z = similar(x)
   else
      z = similar(x, nrows(x), ncols(y))
   end
   ccall((:fmpz_mat_mul, libflint), Nothing,
                (Ref{ZZMatrix}, Ref{ZZMatrix},  Ref{ZZMatrix}),
               z, x, y)
   return z
end

###############################################################################
#
#   Ad hoc binary operators
#
###############################################################################

function *(x::Int, y::ZZMatrix)
   z = similar(y)
   ccall((:fmpz_mat_scalar_mul_si, libflint), Nothing,
                (Ref{ZZMatrix}, Ref{ZZMatrix}, Int), z, y, x)
   return z
end

function *(x::ZZRingElem, y::ZZMatrix)
   z = similar(y)
   ccall((:fmpz_mat_scalar_mul_fmpz, libflint), Nothing,
                (Ref{ZZMatrix}, Ref{ZZMatrix}, Ref{ZZRingElem}), z, y, x)
   return z
end

*(x::ZZMatrix, y::Int) = y*x

*(x::ZZMatrix, y::ZZRingElem) = y*x

*(x::Integer, y::ZZMatrix) = ZZRingElem(x)*y

*(x::ZZMatrix, y::Integer) = ZZRingElem(y)*x

function +(x::ZZMatrix, y::Integer)
   z = deepcopy(x)
   for i = 1:min(nrows(x), ncols(x))
      z[i, i] += y
   end
   return z
end

function +(x::ZZMatrix, y::ZZRingElem)
   z = deepcopy(x)
   for i = 1:min(nrows(x), ncols(x))
      z[i, i] = addeq!(z[i, i], y)
   end
   return z
end

+(x::Integer, y::ZZMatrix) = y + x

+(x::ZZRingElem, y::ZZMatrix) = y + x

-(x::ZZMatrix, y::Integer) = x + (-y)

-(x::ZZMatrix, y::ZZRingElem) = x + (-y)

function -(x::Integer, y::ZZMatrix)
   z = -y
   for i = 1:min(nrows(y), ncols(y))
      z[i, i] += x
   end
   return z
end

function -(x::ZZRingElem, y::ZZMatrix)
   z = -y
   for i = 1:min(nrows(y), ncols(y))
      z[i, i] = addeq!(z[i, i], x)
   end
   return z
end

###############################################################################
#
#   Scaling
#
###############################################################################

@doc raw"""
    <<(x::ZZMatrix, y::Int)

Return $2^yx$.
"""
function <<(x::ZZMatrix, y::Int)
   y < 0 && throw(DomainError(y, "Exponent must be non-negative"))
   z = similar(x)
   ccall((:fmpz_mat_scalar_mul_2exp, libflint), Nothing,
                (Ref{ZZMatrix}, Ref{ZZMatrix}, Int),
               z, x, y)
   return z
end

@doc raw"""
    >>(x::ZZMatrix, y::Int)

Return $x/2^y$ where rounding is towards zero.
"""
function >>(x::ZZMatrix, y::Int)
   y < 0 && throw(DomainError(y, "Exponent must be non-negative"))
   z = similar(x)
   ccall((:fmpz_mat_scalar_tdiv_q_2exp, libflint), Nothing,
                (Ref{ZZMatrix}, Ref{ZZMatrix}, Int),
               z, x, y)
   return z
end

###############################################################################
#
#   Powering
#
###############################################################################

function ^(x::ZZMatrix, y::Int)
   y < 0 && throw(DomainError(y, "Exponent must be non-negative"))
   nrows(x) != ncols(x) && error("Incompatible matrix dimensions")
   z = similar(x)
   ccall((:fmpz_mat_pow, libflint), Nothing,
                (Ref{ZZMatrix}, Ref{ZZMatrix}, Int),
               z, x, y)
   return z
end

###############################################################################
#
#   Comparisons
#
###############################################################################

function ==(x::ZZMatrix, y::ZZMatrix)
   b = check_parent(x, y, false)
   b && ccall((:fmpz_mat_equal, libflint), Bool,
                                       (Ref{ZZMatrix}, Ref{ZZMatrix}), x, y)
end

isequal(x::ZZMatrix, y::ZZMatrix) = ==(x, y)

###############################################################################
#
#   Ad hoc comparisons
#
###############################################################################

function ==(x::ZZMatrix, y::Integer)
   for i = 1:min(nrows(x), ncols(x))
      if x[i, i] != y
         return false
      end
   end
   for i = 1:nrows(x)
      for j = 1:ncols(x)
         if i != j && !iszero(x[i, j])
            return false
         end
      end
   end
   return true
end

==(x::Integer, y::ZZMatrix) = y == x

==(x::ZZMatrix, y::ZZRingElem) = x == parent(x)(y)

==(x::ZZRingElem, y::ZZMatrix) = parent(y)(x) == y

###############################################################################
#
#   Inversion
#
###############################################################################

function inv(x::ZZMatrix)
   !is_square(x) && error("Matrix not invertible")
   z = similar(x)
   d = ZZRingElem()
   ccall((:fmpz_mat_inv, libflint), Nothing,
         (Ref{ZZMatrix}, Ref{ZZRingElem}, Ref{ZZMatrix}), z, d, x)
   if isone(d)
      return z
   end
   if d == -1
      return -z
   end
   error("Matrix not invertible")
end

###############################################################################
#
#   Pseudo inversion
#
###############################################################################

@doc raw"""
    pseudo_inv(x::ZZMatrix)

Return a tuple $(z, d)$ consisting of a matrix $z$ and denominator $d$ such
that $z/d$ is the inverse of $x$.
"""
function pseudo_inv(x::ZZMatrix)
   z = similar(x)
   d = ZZRingElem()
   ccall((:fmpz_mat_inv, libflint), Nothing,
         (Ref{ZZMatrix}, Ref{ZZRingElem}, Ref{ZZMatrix}), z, d, x)
   if !iszero(d)
      return (z, d)
   end
   error("Matrix is singular")
end

###############################################################################
#
#   Exact division
#
###############################################################################

function divexact(x::ZZMatrix, y::ZZMatrix; check::Bool=true)
   ncols(x) != ncols(y) && error("Incompatible matrix dimensions")
   x*inv(y)
end

###############################################################################
#
#   Ad hoc exact division
#
###############################################################################

function divexact(x::ZZMatrix, y::Int; check::Bool=true)
   z = similar(x)
   ccall((:fmpz_mat_scalar_divexact_si, libflint), Nothing,
                (Ref{ZZMatrix}, Ref{ZZMatrix}, Int), z, x, y)
   return z
end

function divexact(x::ZZMatrix, y::ZZRingElem; check::Bool=true)
   z = similar(x)
   ccall((:fmpz_mat_scalar_divexact_fmpz, libflint), Nothing,
                (Ref{ZZMatrix}, Ref{ZZMatrix}, Ref{ZZRingElem}), z, x, y)
   return z
end

divexact(x::ZZMatrix, y::Integer; check::Bool=true) = divexact(x, ZZRingElem(y); check=check)

###############################################################################
#
#   Kronecker product
#
###############################################################################

function kronecker_product(x::ZZMatrix, y::ZZMatrix)
   z = similar(x, nrows(x)*nrows(y), ncols(x)*ncols(y))
   ccall((:fmpz_mat_kronecker_product, libflint), Nothing,
                (Ref{ZZMatrix}, Ref{ZZMatrix}, Ref{ZZMatrix}), z, x, y)
   return z
end

###############################################################################
#
#   Modular reduction
#
###############################################################################

@doc raw"""
    reduce_mod(x::ZZMatrix, y::ZZRingElem)

Reduce the entries of $x$ modulo $y$ and return the result.
"""
function reduce_mod(x::ZZMatrix, y::ZZRingElem)
   z = similar(x)
   ccall((:fmpz_mat_scalar_mod_fmpz, libflint), Nothing,
                (Ref{ZZMatrix}, Ref{ZZMatrix}, Ref{ZZRingElem}), z, x, y)
   return z
end

@doc raw"""
    reduce_mod(x::ZZMatrix, y::Integer)

Reduce the entries of $x$ modulo $y$ and return the result.
"""
reduce_mod(x::ZZMatrix, y::Integer) = reduce_mod(x, ZZRingElem(y))

###############################################################################
#
#   Fraction free LU decomposition
#
###############################################################################

function fflu(x::ZZMatrix, P = SymmetricGroup(nrows(x)))
   m = nrows(x)
   n = ncols(x)
   L = similar(x, m, m)
   U = similar(x)
   d = ZZRingElem()
   p = one(P)

   p.d .-= 1

   r = ccall((:fmpz_mat_fflu, libflint), Int,
                (Ref{ZZMatrix}, Ref{ZZRingElem}, Ptr{Int}, Ref{ZZMatrix}, Int),
                U, d, p.d, x, 0)

   p.d .+= 1

   i = 1
   j = 1
   k = 1
   while i <= m && j <= n
      if !iszero(U[i, j])
         L[i, k] = U[i, j]
         for l = i + 1:m
            L[l, k] = U[l, j]
            U[l, j] = 0
         end
         i += 1
         k += 1
      end
      j += 1
   end

   while k <= m
      L[k, k] = 1
      k += 1
   end

   return r, d, p^(-1), L, U
end

###############################################################################
#
#   Characteristic polynomial
#
###############################################################################

function charpoly(R::ZZPolyRing, x::ZZMatrix)
   nrows(x) != ncols(x) && error("Non-square")
   z = R()
   ccall((:fmpz_mat_charpoly, libflint), Nothing,
                (Ref{ZZPolyRingElem}, Ref{ZZMatrix}), z, x)
   return z
end

###############################################################################
#
#   Minimal polynomial
#
###############################################################################

function minpoly(R::ZZPolyRing, x::ZZMatrix)
   nrows(x) != ncols(x) && error("Non-square")
   z = R()
   ccall((:fmpz_mat_minpoly, libflint), Nothing,
                (Ref{ZZPolyRingElem}, Ref{ZZMatrix}), z, x)
   return z
end

###############################################################################
#
#   Determinant
#
###############################################################################

function det(x::ZZMatrix)
   nrows(x) != ncols(x) && error("Non-square matrix")
   z = ZZRingElem()
   ccall((:fmpz_mat_det, libflint), Nothing,
                (Ref{ZZRingElem}, Ref{ZZMatrix}), z, x)
   return z
end

@doc raw"""
    det_divisor(x::ZZMatrix)

Return some positive divisor of the determinant of $x$, if the determinant
is nonzero, otherwise return zero.
"""
function det_divisor(x::ZZMatrix)
   nrows(x) != ncols(x) && error("Non-square matrix")
   z = ZZRingElem()
   ccall((:fmpz_mat_det_divisor, libflint), Nothing,
                (Ref{ZZRingElem}, Ref{ZZMatrix}), z, x)
   return z
end

@doc raw"""
    det_given_divisor(x::ZZMatrix, d::ZZRingElem, proved=true)

Return the determinant of $x$ given a positive divisor of its determinant. If
`proved == true` (the default), the output is guaranteed to be correct,
otherwise a heuristic algorithm is used.
"""
function det_given_divisor(x::ZZMatrix, d::ZZRingElem, proved=true)
   nrows(x) != ncols(x) && error("Non-square")
   z = ZZRingElem()
   ccall((:fmpz_mat_det_modular_given_divisor, libflint), Nothing,
               (Ref{ZZRingElem}, Ref{ZZMatrix}, Ref{ZZRingElem}, Cint), z, x, d, proved)
   return z
end

@doc raw"""
    det_given_divisor(x::ZZMatrix, d::Integer, proved=true)

Return the determinant of $x$ given a positive divisor of its determinant. If
`proved == true` (the default), the output is guaranteed to be correct,
otherwise a heuristic algorithm is used.
"""
function det_given_divisor(x::ZZMatrix, d::Integer, proved=true)
   return det_given_divisor(x, ZZRingElem(d), proved)
end


@doc raw"""
    hadamard_bound2(M::ZZMatrix)

Return the Hadamard bound squared for the determinant, i.e. the product
of the euclidean row-norms squared.
"""
function hadamard_bound2(M::ZZMatrix)
  is_square(M) || error("Matrix must be square")
  H = ZZ(1);
  r = ZZ(0)
  n = nrows(M)
  for i in 1:n
    ccall((:fmpz_set_si, Nemo.libflint), Cvoid, (Ref{ZZRingElem}, Int), r, 0)
    M_ptr = Nemo.mat_entry_ptr(M, i, 1)
    for j in 1:n
      ccall((:fmpz_addmul, Nemo.libflint), Cvoid, (Ref{ZZRingElem}, Ptr{ZZRingElem}, Ptr{ZZRingElem}), r, M_ptr, M_ptr)
      M_ptr += sizeof(ZZRingElem)
    end
    if iszero(r)
      return r
    end
    Nemo.mul!(H, H, r)
  end
  return H
end

function Base.maximum(::typeof(nbits), M::ZZMatrix)
  mx = 0
  n = nrows(M)
  m = ncols(M)
  Base.GC.@preserve M begin
    for i in 1:n
      M_ptr = Nemo.mat_entry_ptr(M, i, 1)
      for j in 1:m
        #a zero fmpz is a binary zero, hence this works
        #fmpz_bits does not work on 0 I think (at least is it unneccessary)
        #this is not going through the "correct" order of the rows, but 
        #for this is does not matter
        if !iszero(unsafe_load(reinterpret(Ptr{Int}, M_ptr)))
          mx = max(mx, ccall((:fmpz_bits, Nemo.libflint), Culong, (Ptr{ZZRingElem},), M_ptr))
        end
        M_ptr += sizeof(ZZRingElem)
      end
    end
  end
  return Int(mx)
end
###############################################################################
#
#   Gram matrix
#
###############################################################################

function gram(x::ZZMatrix)
   z = similar(x, nrows(x), nrows(x))
   ccall((:fmpz_mat_gram, libflint), Nothing,
                (Ref{ZZMatrix}, Ref{ZZMatrix}), z, x)
   return z
end

###############################################################################
#
#   Hadamard matrix
#
###############################################################################

@doc raw"""
    hadamard(R::ZZMatrixSpace)

Return the Hadamard matrix for the given matrix space. The number of rows and
columns must be equal.
"""
function hadamard(R::ZZMatrixSpace)
   nrows(R) != ncols(R) && error("Unable to create Hadamard matrix")
   z = R()
   success = ccall((:fmpz_mat_hadamard, libflint), Bool,
                   (Ref{ZZMatrix},), z)
   !success && error("Unable to create Hadamard matrix")
   return z
end

@doc raw"""
    is_hadamard(x::ZZMatrix)

Return `true` if the given matrix is Hadamard, otherwise return `false`.
"""
function is_hadamard(x::ZZMatrix)
   return ccall((:fmpz_mat_is_hadamard, libflint), Bool,
                   (Ref{ZZMatrix},), x)
end

###############################################################################
#
#   Hermite normal form
#
###############################################################################

# We introduce _hnf, __hnf to make it possible for Oscar to overload the
# hnf(x::ZZMatrix) call to something more performant, while at the same time
# being able to call the Nemo/flint implementation.

@doc raw"""
    hnf(x::ZZMatrix)

Return the Hermite Normal Form of $x$.
"""
@inline hnf(x::ZZMatrix) = _hnf(x)

@inline _hnf(x) = __hnf(x)

function __hnf(x)
   z = similar(x)
   ccall((:fmpz_mat_hnf, libflint), Nothing,
                (Ref{ZZMatrix}, Ref{ZZMatrix}), z, x)
   return z
end

@doc raw"""
    hnf_with_transform(x::ZZMatrix)

Compute a tuple $(H, T)$ where $H$ is the Hermite normal form of $x$ and $T$
is a transformation matrix so that $H = Tx$.
"""
@inline hnf_with_transform(x::ZZMatrix) = _hnf_with_transform(x)

@inline _hnf_with_transform(x) = __hnf_with_transform(x)

function __hnf_with_transform(x::ZZMatrix)
   z = similar(x)
   u = similar(x, nrows(x), nrows(x))
   ccall((:fmpz_mat_hnf_transform, libflint), Nothing,
                (Ref{ZZMatrix}, Ref{ZZMatrix}, Ref{ZZMatrix}), z, u, x)
   return z, u
end

@doc raw"""
    hnf_modular(x::ZZMatrix, d::ZZRingElem)

Compute the Hermite normal form of $x$ given that $d$ is a multiple of the
determinant of the nonzero rows of $x$.
"""
function hnf_modular(x::ZZMatrix, d::ZZRingElem)
   z = similar(x)
   ccall((:fmpz_mat_hnf_modular, libflint), Nothing,
                (Ref{ZZMatrix}, Ref{ZZMatrix}, Ref{ZZRingElem}), z, x, d)
   return z
end

@doc raw"""
    hnf_modular_eldiv(x::ZZMatrix, d::ZZRingElem)

Compute the Hermite normal form of $x$ given that $d$ is a multiple of the
largest elementary divisor of $x$. The matrix $x$ must have full rank.
"""
function hnf_modular_eldiv(x::ZZMatrix, d::ZZRingElem)
   (nrows(x) < ncols(x)) &&
                error("Matrix must have at least as many rows as columns")
   z = deepcopy(x)
   ccall((:fmpz_mat_hnf_modular_eldiv, libflint), Nothing,
                (Ref{ZZMatrix}, Ref{ZZRingElem}), z, d)
   return z
end

@doc raw"""
    is_hnf(x::ZZMatrix)

Return `true` if the given matrix is in Hermite Normal Form, otherwise return
`false`.
"""
function is_hnf(x::ZZMatrix)
   return ccall((:fmpz_mat_is_in_hnf, libflint), Bool,
                   (Ref{ZZMatrix},), x)
end

###############################################################################
#
#   LLL
#
###############################################################################

mutable struct LLLContext
   delta::Float64
   eta::Float64
   rep_type::Cint
   gram_type::Cint

   function LLLContext(delta::Float64, eta::Float64,
                    rep::Symbol = :zbasis, gram::Symbol = :approx)
      rt = rep == :zbasis ? 1 : 0
      gt = gram == :approx ? 0 : 1
      return new(delta, eta, rt, gt)
   end
end

@doc raw"""
    lll_with_transform(x::ZZMatrix, ctx::LLLContext = LLLContext(0.99, 0.51))

Return a tuple $(L, T)$ where the rows of $L$ form an LLL-reduced basis of the
$\mathbb{Z}$-lattice generated by the rows of $x$ and $T$ is a transformation
matrix so that $L = Tx$. $L$ may contain additional zero rows.
See [`lll`](@ref) for the used default parameters which can be overridden by
supplying an optional context object.

See [`lll_gram_with_transform`](@ref) for a function taking the Gram matrix as
input.
"""
function lll_with_transform(x::ZZMatrix, ctx::LLLContext = LLLContext(0.99, 0.51))
   z = deepcopy(x)
   u = similar(x, nrows(x), nrows(x))
   for i in 1:nrows(u)
      u[i, i] = 1
   end
   ccall((:fmpz_lll, libflint), Nothing,
         (Ref{ZZMatrix}, Ref{ZZMatrix}, Ref{LLLContext}), z, u, ctx)
   return z, u
end

@doc raw"""
    lll(x::ZZMatrix, ctx::LLLContext = LLLContext(0.99, 0.51))

Return a matrix $L$ whose rows form an LLL-reduced basis of the
$\mathbb{Z}$-lattice generated by the rows of $x$. $L$ may contain additional
zero rows.

By default, the LLL is performed with reduction parameters $\delta = 0.99$ and
$\eta = 0.51$. These defaults can be overridden by specifying an optional context
object.

See [`lll_gram`](@ref) for a function taking the Gram matrix as input.
"""
function lll(x::ZZMatrix, ctx::LLLContext = LLLContext(0.99, 0.51))
   z = deepcopy(x)
   return lll!(z)
end

@doc raw"""
    lll!(x::ZZMatrix, ctx::LLLContext = LLLContext(0.99, 0.51))

Compute an LLL-reduced basis of the $\mathbb{Z}$-lattice generated by the rows
of $x$ inplace.

By default, the LLL is performed with reduction parameters $\delta = 0.99$ and
$\eta = 0.51$. These defaults can be overridden by specifying an optional context
object.

See [`lll_gram!`](@ref) for a function taking the Gram matrix as input.
"""
function lll!(x::ZZMatrix, ctx::LLLContext = LLLContext(0.99, 0.51))
   if nrows(x) == 0
     return x
   end
   ccall((:fmpz_lll, libflint), Nothing,
         (Ref{ZZMatrix}, Ptr{nothing}, Ref{LLLContext}), x, C_NULL, ctx)
   return x
end

@doc raw"""
    lll_gram_with_transform(x::ZZMatrix, ctx::LLLContext = LLLContext(0.99, 0.51, :gram))

Return a tuple $(L, T)$ where $L$ is the Gram matrix of an LLL-reduced basis of
the lattice given by the Gram matrix $x$ and $T$ is a transformation matrix with
$L = T^\top x T$.
The matrix $x$ must be symmetric and non-singular.

See [`lll_gram`](@ref) for the used default parameters which can be overridden by
supplying an optional context object.
"""
function lll_gram_with_transform(x::ZZMatrix, ctx::LLLContext = LLLContext(0.99, 0.51, :gram))
   z = deepcopy(x)
   u = similar(x, nrows(x), nrows(x))
   for i in 1:nrows(u)
      u[i, i] = 1
   end
   ccall((:fmpz_lll, libflint), Nothing,
         (Ref{ZZMatrix}, Ref{ZZMatrix}, Ref{LLLContext}), z, u, ctx)
   return z, u
end

@doc raw"""
    lll_gram(x::ZZMatrix, ctx::LLLContext = LLLContext(0.99, 0.51, :gram))

Return the Gram matrix $L$ of an LLL-reduced basis of the lattice given by the
Gram matrix $x$.
The matrix $x$ must be symmetric and non-singular.

By default, the LLL is performed with reduction parameters $\delta = 0.99$ and
$\eta = 0.51$. These defaults can be overridden by specifying an optional context
object.
"""
function lll_gram(x::ZZMatrix, ctx::LLLContext = LLLContext(0.99, 0.51, :gram))
   z = deepcopy(x)
   return lll_gram!(z)
end

@doc raw"""
    lll_gram!(x::ZZMatrix, ctx::LLLContext = LLLContext(0.99, 0.51, :gram))

Compute the Gram matrix of an LLL-reduced basis of the lattice given by the
Gram matrix $x$ inplace.
The matrix $x$ must be symmetric and non-singular.

By default, the LLL is performed with reduction parameters $\delta = 0.99$ and
$\eta = 0.51$. These defaults can be overridden by specifying an optional context
object.
"""
function lll_gram!(x::ZZMatrix, ctx::LLLContext = LLLContext(0.99, 0.51, :gram))
   ccall((:fmpz_lll, libflint), Nothing,
         (Ref{ZZMatrix}, Ptr{Nothing}, Ref{LLLContext}), x, C_NULL, ctx)
   return x
end

@doc raw"""
    lll_with_removal_transform(x::ZZMatrix, b::ZZRingElem, ctx::LLLContext = LLLContext(0.99, 0.51))

Compute a tuple $(r, L, T)$ where the first $r$ rows of $L$ are those
remaining from the LLL reduction after removal of vectors with norm exceeding
the bound $b$ and $T$ is a transformation matrix so that $L = Tx$.
"""
function lll_with_removal_transform(x::ZZMatrix, b::ZZRingElem, ctx::LLLContext = LLLContext(0.99, 0.51))
   z = deepcopy(x)
   u = similar(x, nrows(x), nrows(x))
   for i in 1:nrows(u)
      u[i, i] = 1
   end
   d = Int(ccall((:fmpz_lll_with_removal, libflint), Cint,
    (Ref{ZZMatrix}, Ref{ZZMatrix}, Ref{ZZRingElem}, Ref{LLLContext}), z, u, b, ctx))
   return d, z, u
end

@doc raw"""
    lll_with_removal(x::ZZMatrix, b::ZZRingElem, ctx::LLLContext = LLLContext(0.99, 0.51))

Compute the LLL reduction of $x$ and throw away rows whose norm exceeds
the given bound $b$. Return a tuple $(r, L)$ where the first $r$ rows of $L$
are the rows remaining after removal.
"""
function lll_with_removal(x::ZZMatrix, b::ZZRingElem, ctx::LLLContext = LLLContext(0.99, 0.51))
   z = deepcopy(x)
   d = Int(ccall((:fmpz_lll_with_removal, libflint), Cint,
    (Ref{ZZMatrix}, Ptr{Nothing}, Ref{ZZRingElem}, Ref{LLLContext}), z, C_NULL, b, ctx))
   return d, z
end

###############################################################################
#
#   Nullspace
#
###############################################################################

function nullspace(x::ZZMatrix)
  H, T = hnf_with_transform(transpose(x))
  for i = nrows(H):-1:1
    for j = 1:ncols(H)
      if !iszero(H[i, j])
        N = similar(x, ncols(x), nrows(H) - i)
        for k = 1:nrows(N)
          for l = 1:ncols(N)
            N[k, l] = T[nrows(T) - l + 1, k]
          end
        end
        return ncols(N), N
      end
    end
  end
  return ncols(x), identity_matrix(x, ncols(x))
end

function kernel(A::ZZMatrix; side::Symbol = :left)
   Solve.check_option(side, [:right, :left], "side")

   if side === :left
      K = kernel(transpose(A), side = :right)
      return transpose(K)
   end

   A = transpose(hnf(A))
   H, U = hnf_with_transform(A)
   r = nrows(H)
   while r > 0 && is_zero_row(H, r)
      r -= 1
   end
   if is_zero(r)
      return transpose(U)
   else
      return transpose(view(U, r + 1:nrows(U), 1:ncols(U)))
   end
end

@doc raw"""
    nullspace_right_rational(x::ZZMatrix)

Return a tuple $(r, U)$ consisting of a matrix $U$ such that the first $r$ columns
form the right rational nullspace of $x$, i.e. a set of vectors over $\mathbb{Z}$
giving a $\mathbb{Q}$-basis  for the nullspace of $x$ considered as a matrix over
$\mathbb{Q}$.
"""
function nullspace_right_rational(x::ZZMatrix)
   z = similar(x)
   u = similar(x, ncols(x), ncols(x))
   rank = ccall((:fmpz_mat_nullspace, libflint), Int,
                (Ref{ZZMatrix}, Ref{ZZMatrix}), u, x)
   return rank, u
end

###############################################################################
#
#   Rank
#
###############################################################################

function rank(x::ZZMatrix)
   return ccall((:fmpz_mat_rank, libflint), Int,
                (Ref{ZZMatrix},), x)
end

###############################################################################
#
#   Reduced row echelon form
#
###############################################################################

function rref(x::ZZMatrix)
   z = similar(x)
   d = ZZRingElem()
   r = ccall((:fmpz_mat_rref, libflint), Int,
            (Ref{ZZMatrix}, Ref{ZZRingElem}, Ref{ZZMatrix}), z, d, x)
   return r, z, d
end

###############################################################################
#
#   Smith normal form
#
###############################################################################

@doc raw"""
    snf(x::ZZMatrix)

Compute the Smith normal form of $x$.
"""
function snf(x::ZZMatrix)
   z = similar(x)
   ccall((:fmpz_mat_snf, libflint), Nothing,
                (Ref{ZZMatrix}, Ref{ZZMatrix}), z, x)
   return z
end

@doc raw"""
    snf_diagonal(x::ZZMatrix)

Given a diagonal matrix $x$ compute the Smith normal form of $x$.
"""
function snf_diagonal(x::ZZMatrix)
   z = similar(x)
   ccall((:fmpz_mat_snf_diagonal, libflint), Nothing,
                (Ref{ZZMatrix}, Ref{ZZMatrix}), z, x)
   return z
end

@doc raw"""
    is_snf(x::ZZMatrix)

Return `true` if $x$ is in Smith normal form, otherwise return `false`.
"""
function is_snf(x::ZZMatrix)
   return ccall((:fmpz_mat_is_in_snf, libflint), Bool,
                   (Ref{ZZMatrix},), x)
end

###############################################################################
#
#   manual linear algebra: row and col operations
#
###############################################################################

function AbstractAlgebra.add_row!(A::ZZMatrix, s::ZZRingElem, i::Int, j::Int)
   @assert 1 <= i <= nrows(A)
   @assert 1 <= j <= nrows(A)
   GC.@preserve A begin
     i_ptr = mat_entry_ptr(A, i, 1)
     j_ptr = mat_entry_ptr(A, j, 1)
     for k = 1:ncols(A)
        ccall((:fmpz_addmul, libflint), Cvoid, (Ptr{ZZRingElem}, Ref{ZZRingElem}, Ptr{ZZRingElem}), i_ptr, s, j_ptr)
        i_ptr += sizeof(ZZRingElem)
        j_ptr += sizeof(ZZRingElem)
     end
   end
end

function AbstractAlgebra.add_column!(A::ZZMatrix, s::ZZRingElem, i::Int, j::Int)
   @assert 1 <= i <= ncols(A)
   @assert 1 <= j <= ncols(A)
   GC.@preserve A begin
     for k = 1:nrows(A)
        i_ptr = mat_entry_ptr(A, k, i)
        j_ptr = mat_entry_ptr(A, k, j)
        ccall((:fmpz_addmul, libflint), Cvoid, (Ptr{ZZRingElem}, Ref{ZZRingElem}, Ptr{ZZRingElem}), i_ptr, s, j_ptr)
     end
   end
end

###############################################################################
#
#   Linear solving
#
###############################################################################

function Solve._can_solve_internal_no_check(A::ZZMatrix, b::ZZMatrix, task::Symbol; side::Symbol = :left)
   if side === :left
      fl, sol, K = Solve._can_solve_internal_no_check(transpose(A), transpose(b), task, side = :right)
      return fl, transpose(sol), transpose(K)
   end

   H, T = hnf_with_transform(transpose(A))
   b = deepcopy(b)
   z = similar(A, ncols(b), ncols(A))
   l = min(nrows(A), ncols(A))
   t = ZZRingElem() # temp. variable

   for i = 1:ncols(b)
     for j = 1:l
       k = 1
       while k <= ncols(H) && is_zero_entry(H, j, k)
         k += 1
       end
       if k > ncols(H)
         continue
       end
       q, r = divrem(b[k, i], H[j, k])
       if !iszero(r)
         return false, b, zero(A, 0, 0)
       end
       if !iszero(q)
          # b[h, i] -= q*H[j, h]
          GC.@preserve b H q t begin
             H_ptr = mat_entry_ptr(H, j, k)
             for h = k:ncols(H)
               b_ptr = mat_entry_ptr(b, h, i)
               ccall((:fmpz_mul, libflint), Cvoid, (Ref{ZZRingElem}, Ref{ZZRingElem}, Ptr{ZZRingElem}), t, q, H_ptr)
               ccall((:fmpz_sub, libflint), Cvoid, (Ptr{ZZRingElem}, Ptr{ZZRingElem}, Ref{ZZRingElem}), b_ptr, b_ptr, t)
               H_ptr += sizeof(ZZRingElem)
             end
          end
       end
       z[i, j] = q
     end
   end

   fl = is_zero(b)
   if !fl
     return false, zero(A, 0, 0), zero(A, 0, 0)
   end
   if task === :only_check
     return true, zero(A, 0, 0), zero(A, 0, 0)
   end

   sol = transpose(z*T)
   if task === :with_solution
     return true, sol, zero(A, 0, 0)
   end
   _, K = Solve._kernel_of_hnf(A, H, T)
   return fl, sol, K
 end

# Overwrite some solve context functionality so that it uses `transpose` and not
# `lazy_transpose`

function solve_init(A::ZZMatrix)
  return Solve.SolveCtx{ZZRingElem, ZZMatrix, ZZMatrix}(A)
end

function Solve._init_reduce_transpose(C::Solve.SolveCtx{ZZRingElem})
  if isdefined(C, :red_transp) && isdefined(C, :trafo_transp)
    return nothing
  end

  R, U = hnf_with_transform(transpose(matrix(C)))
  C.red_transp = R
  C.trafo_transp = U
  return nothing
end

function Solve.kernel(C::Solve.SolveCtx{ZZRingElem}; side::Symbol = :left)
  Solve.check_option(side, [:right, :left], "side")

  if side === :right
    return Solve._kernel_of_hnf(matrix(C), Solve.reduced_matrix_of_transpose(C), Solve.transformation_matrix_of_transpose(C))[2]
  else
    nullity, X = Solve._kernel_of_hnf(matrix(C), Solve.reduced_matrix(C), Solve.transformation_matrix(C))
    return transpose(X)
  end
end

Base.reduce(::typeof(hcat), A::AbstractVector{ZZMatrix}) = AbstractAlgebra._hcat(A)

Base.reduce(::typeof(vcat), A::AbstractVector{ZZMatrix}) = AbstractAlgebra._vcat(A)

function Base.cat(A::ZZMatrix...;dims)
  @assert dims == (1,2) || isa(dims, Int)

  if isa(dims, Int)
    if dims == 1
      return hcat(A...)
    elseif dims == 2
      return vcat(A...)
    else
      error("dims must be 1, 2, or (1,2)")
    end
  end

  X = zero_matrix(ZZ, sum(nrows(x) for x = A), sum(ncols(x) for x = A))
  start_row = start_col = 0
  for i in 1:length(A)
    Ai = A[i]
    for k = 1:nrows(Ai)
      GC.@preserve Ai X begin
        A_ptr = mat_entry_ptr(Ai, k, 1)
        X_ptr = mat_entry_ptr(X, start_row + k, start_col+1)
        for l = 1:ncols(Ai)
          ccall((:fmpz_set, libflint), Cvoid, (Ptr{ZZRingElem}, Ptr{ZZRingElem}), X_ptr, A_ptr)
          X_ptr += sizeof(ZZRingElem)
          A_ptr += sizeof(ZZRingElem)
        end
      end
    end
    start_row += nrows(Ai)
    start_col += ncols(Ai)
  end
  return X
end

function AbstractAlgebra._vcat(A::AbstractVector{ZZMatrix})
  if any(x -> ncols(x) != ncols(A[1]), A)
    error("Matrices must have the same number of columns")
  end

  M = zero_matrix(ZZ, sum(nrows, A, init = 0), ncols(A[1]))
  s = 0
  for N in A
    GC.@preserve M N begin
      for j in 1:nrows(N)
        M_ptr = mat_entry_ptr(M, s+j, 1)
        N_ptr = mat_entry_ptr(N, j, 1)
        for k in 1:ncols(N)
          ccall((:fmpz_set, libflint), Cvoid, (Ptr{ZZRingElem}, Ptr{ZZRingElem}), M_ptr, N_ptr)
          M_ptr += sizeof(ZZRingElem)
          N_ptr += sizeof(ZZRingElem)
        end
      end
    end
    s += nrows(N)
  end
  return M
end


function AbstractAlgebra._hcat(A::AbstractVector{ZZMatrix})
  if any(x -> nrows(x) != nrows(A[1]), A)
    error("Matrices must have the same number of columns")
  end

  M = zero_matrix(ZZ, nrows(A[1]), sum(ncols, A, init = 0))
  s = 0
  for N in A
    GC.@preserve M N begin
      for j in 1:nrows(N)
        M_ptr = mat_entry_ptr(M, j, s+1)
        N_ptr = mat_entry_ptr(N, j, 1)
        for k in 1:ncols(N)
          ccall((:fmpz_set, libflint), Cvoid, (Ptr{ZZRingElem}, Ptr{ZZRingElem}), M_ptr, N_ptr)
          M_ptr += sizeof(ZZRingElem)
          N_ptr += sizeof(ZZRingElem)
        end
      end
    end
    s += ncols(N)
  end
  return M
end

@doc raw"""
    _solve_rational(a::ZZMatrix, b::ZZMatrix)

If it exists, return a tuple $(x, d)$ consisting of a column vector $x$ such
that $ax = db$. The element $b$ must be a column vector with the same number
of rows as $a$ and $a$ must be a square matrix. If these conditions are not
met or $(x, d)$ does not exist, an exception is raised.
"""
function _solve_rational(a::ZZMatrix, b::ZZMatrix)
   nrows(a) != ncols(a) && error("Not a square matrix in _solve_rational")
   nrows(b) != nrows(a) && error("Incompatible dimensions in _solve_rational")
   z = similar(b)
   d = ZZRingElem()
   nonsing = ccall((:fmpz_mat_solve, libflint), Bool,
      (Ref{ZZMatrix}, Ref{ZZRingElem}, Ref{ZZMatrix}, Ref{ZZMatrix}), z, d, a, b)
   !nonsing && error("Singular matrix in _solve_rational")
   return z, d
end

function _solve_with_det(a::ZZMatrix, b::ZZMatrix)
   return _solve_rational(a, b)
end

@doc raw"""
    _solve_dixon(a::ZZMatrix, b::ZZMatrix)

Return a tuple $(x, m)$ consisting of a column vector $x$ such that $ax = b
\pmod{m}$. The element  $b$ must be a column vector with the same number > of
rows as $a$ and $a$ must be a square matrix. If these conditions are not met
or $(x, d)$ does not exist, an exception is raised.
"""
function _solve_dixon(a::ZZMatrix, b::ZZMatrix)
   nrows(a) != ncols(a) && error("Not a square matrix in solve")
   nrows(b) != nrows(a) && error("Incompatible dimensions in solve")
   z = similar(b)
   d = ZZRingElem()
   nonsing = ccall((:fmpz_mat_solve_dixon, libflint), Bool,
      (Ref{ZZMatrix}, Ref{ZZRingElem}, Ref{ZZMatrix}, Ref{ZZMatrix}), z, d, a, b)
   !nonsing && error("Singular matrix in solve")
   return z, d
end

#XU = B. only the upper triangular part of U is used
function _solve_triu_left(U::ZZMatrix, b::ZZMatrix)
   n = ncols(U)
   m = nrows(b)
   R = base_ring(U)
   X = zero(b)
   tmp = zero_matrix(ZZ, 1, n)
   t = R()
   s = R()
   GC.@preserve X b tmp begin
     for i = 1:m
        tmp_p = mat_entry_ptr(tmp, 1, 1)
        X_p = mat_entry_ptr(X, i, 1)
        for j = 1:n
           ccall((:fmpz_set, Nemo.libflint), Cvoid, (Ptr{ZZRingElem}, Ptr{ZZRingElem}), tmp_p, X_p)
           X_p += sizeof(ZZRingElem)
           tmp_p += sizeof(ZZRingElem)
        end
        for j = 1:n
           ccall((:fmpz_zero, Nemo.libflint), Cvoid, (Ref{ZZRingElem}, ), s) 

           tmp_p = mat_entry_ptr(tmp, 1, 1)
           for k = 1:j-1
              U_p = mat_entry_ptr(U, k, j)
              ccall((:fmpz_addmul, Nemo.libflint), Cvoid, (Ref{ZZRingElem}, Ptr{ZZRingElem}, Ptr{ZZRingElem}), s, U_p, tmp_p)
              tmp_p += sizeof(ZZRingElem)
           end
           ccall((:fmpz_sub, Nemo.libflint), Cvoid, 
            (Ref{ZZRingElem}, Ptr{ZZRingElem}, Ref{ZZRingElem}), s, mat_entry_ptr(b, i, j), s)
           ccall((:fmpz_divexact, Nemo.libflint), Cvoid, 
            (Ptr{ZZRingElem}, Ref{ZZRingElem}, Ptr{ZZRingElem}), mat_entry_ptr(tmp, 1, j), s, mat_entry_ptr(U, j, j))
        end
        tmp_p = mat_entry_ptr(tmp, 1, 1)
        X_p = mat_entry_ptr(X, i, 1)
        for j = 1:n
           ccall((:fmpz_set, Nemo.libflint), Cvoid, (Ptr{ZZRingElem}, Ptr{ZZRingElem}), X_p, tmp_p)
           X_p += sizeof(ZZRingElem)
           tmp_p += sizeof(ZZRingElem)
        end
     end
   end
   return X
end

#UX = B
function _solve_triu(U::ZZMatrix, b::ZZMatrix) 
   n = nrows(U)
   m = ncols(b)
   X = zero(b)
   tmp = zero_matrix(ZZ, 1, n)
   s = ZZ()
   GC.@preserve U b tmp begin
     for i = 1:m
        tmp_ptr = mat_entry_ptr(tmp, 1, 1)
        for j = 1:n
           X_ptr = mat_entry_ptr(X, j, i)
           ccall((:fmpz_set, Nemo.libflint), Cvoid, (Ptr{ZZRingElem}, Ptr{ZZRingElem}), tmp_ptr, X_ptr)
           tmp_ptr += sizeof(ZZRingElem)
        end
        for j = n:-1:1
           ccall((:fmpz_zero, Nemo.libflint), Cvoid, (Ref{ZZRingElem}, ), s)
           tmp_ptr = mat_entry_ptr(tmp, 1, j+1)
           for k = j + 1:n
              U_ptr = mat_entry_ptr(U, j, k)
              ccall((:fmpz_addmul, Nemo.libflint), Cvoid, (Ref{ZZRingElem}, Ptr{ZZRingElem}, Ptr{ZZRingElem}), s, U_ptr, tmp_ptr)
              tmp_ptr += sizeof(ZZRingElem)
  #           s = addmul!(s, U[j, k], tmp[k])
           end
           b_ptr = mat_entry_ptr(b, j, i)
           ccall((:fmpz_sub, Nemo.libflint), Cvoid, (Ref{ZZRingElem}, Ptr{ZZRingElem}, Ref{ZZRingElem}), s, b_ptr, s)
  #         s = b[j, i] - s
           tmp_ptr = mat_entry_ptr(tmp, 1, j)
           U_ptr = mat_entry_ptr(U, j, j)
           ccall((:fmpz_divexact, Nemo.libflint), Cvoid, (Ptr{ZZRingElem}, Ref{ZZRingElem}, Ptr{ZZRingElem}), tmp_ptr, s, U_ptr)
           
#           tmp[j] = divexact(s, U[j,j])
        end
        tmp_ptr = mat_entry_ptr(tmp, 1, 1)
        for j = 1:n
           X_ptr = mat_entry_ptr(X, j, i)
           ccall((:fmpz_set, Nemo.libflint), Cvoid, (Ptr{ZZRingElem}, Ptr{ZZRingElem}), X_ptr, tmp_ptr)
           tmp_ptr += sizeof(ZZRingElem)
        end
     end
   end
   return X
end

function AbstractAlgebra._solve_tril!(A::ZZMatrix, B::ZZMatrix, C::ZZMatrix, f::Int = 0) 

  # a       x   u      ax = u
  # b c   * y = v      bx + cy = v
  # d e f   z   w      ....

  @assert ncols(A) == ncols(C)
  s = ZZ(0)
  GC.@preserve A B C begin
    for i=1:ncols(A)
      for j = 1:nrows(A)
        t = C[j, i]
        B_ptr = mat_entry_ptr(B, j, 1)
        for k = 1:j-1
          A_ptr = mat_entry_ptr(B, k, i)
          ccall((:fmpz_mul, libflint), Cvoid, (Ref{ZZRingElem}, Ptr{ZZRingElem}, Ptr{ZZRingElem}), s, A_ptr, B_ptr)
          B_ptr += sizeof(ZZRingElem)
          sub!(t, t, s)
        end
        if f == 1
          A[j,i] = t
        else
          A[j,i] = divexact(t, B[j, j])
        end
      end
    end
  end
end

###############################################################################
#
#   Trace
#
###############################################################################

function tr(x::ZZMatrix)
   nrows(x) != ncols(x) && error("Not a square matrix in trace")
   d = ZZRingElem()
   ccall((:fmpz_mat_trace, libflint), Int,
                (Ref{ZZRingElem}, Ref{ZZMatrix}), d, x)
   return d
end

###############################################################################
#
#   Content
#
###############################################################################

function content(x::ZZMatrix)
  d = ZZRingElem()
  ccall((:fmpz_mat_content, libflint), Nothing,
        (Ref{ZZRingElem}, Ref{ZZMatrix}), d, x)
  return d
end

###############################################################################
#
#   Concatenation
#
###############################################################################

function hcat(a::ZZMatrix, b::ZZMatrix)
  nrows(a) != nrows(b) && error("Incompatible number of rows in hcat")
  c = similar(a, nrows(a), ncols(a) + ncols(b))
  ccall((:fmpz_mat_concat_horizontal, libflint), Nothing,
        (Ref{ZZMatrix}, Ref{ZZMatrix}, Ref{ZZMatrix}), c, a, b)
  return c
end

function vcat(a::ZZMatrix, b::ZZMatrix)
  ncols(a) != ncols(b) && error("Incompatible number of columns in vcat")
  c = similar(a, nrows(a) + nrows(b), ncols(a))
  ccall((:fmpz_mat_concat_vertical, libflint), Nothing,
        (Ref{ZZMatrix}, Ref{ZZMatrix}, Ref{ZZMatrix}), c, a, b)
  return c
end

###############################################################################
#
#   Unsafe functions
#
###############################################################################

function add!(z::ZZMatrix, x::ZZMatrix, y::ZZMatrix)
   ccall((:fmpz_mat_add, libflint), Nothing,
                (Ref{ZZMatrix}, Ref{ZZMatrix}, Ref{ZZMatrix}), z, x, y)
   return z
end

function sub!(z::ZZMatrix, x::ZZMatrix, y::ZZMatrix)
   ccall((:fmpz_mat_sub, libflint), Nothing,
                (Ref{ZZMatrix}, Ref{ZZMatrix}, Ref{ZZMatrix}), z, x, y)
   return z
end

function mul!(z::ZZMatrix, x::ZZMatrix, y::ZZMatrix, fl::Bool = false)
   if fl
     n = similar(z)
     ccall((:fmpz_mat_mul, libflint), Nothing,
                  (Ref{ZZMatrix}, Ref{ZZMatrix}, Ref{ZZMatrix}), n, x, y)
     add!(z, z, n)             
   else
     ccall((:fmpz_mat_mul, libflint), Nothing,
                  (Ref{ZZMatrix}, Ref{ZZMatrix}, Ref{ZZMatrix}), z, x, y)
   end               
   return z
end

function mul!(y::ZZMatrix, x::Int)
   ccall((:fmpz_mat_scalar_mul_si, libflint), Nothing,
                (Ref{ZZMatrix}, Ref{ZZMatrix}, Int), y, y, x)
   return y
end

function mul!(y::ZZMatrix, x::ZZRingElem)
   ccall((:fmpz_mat_scalar_mul_fmpz, libflint), Nothing,
                (Ref{ZZMatrix}, Ref{ZZMatrix}, Ref{ZZRingElem}), y, y, x)
   return y
end

function addmul!(z::ZZMatrix, y::ZZMatrix, x::ZZRingElem)
   ccall((:fmpz_mat_scalar_addmul_fmpz, libflint), Nothing,
                (Ref{ZZMatrix}, Ref{ZZMatrix}, Ref{ZZRingElem}), z, y, x)
   return z
end

function addmul!(z::ZZMatrix, y::ZZMatrix, x::Int)
   ccall((:fmpz_mat_scalar_addmul_si, libflint), Nothing,
                (Ref{ZZMatrix}, Ref{ZZMatrix}, Int), z, y, x)
   return z
end

function addeq!(z::ZZMatrix, x::ZZMatrix)
   ccall((:fmpz_mat_add, libflint), Nothing,
                (Ref{ZZMatrix}, Ref{ZZMatrix}, Ref{ZZMatrix}), z, z, x)
   return z
end

function zero!(z::ZZMatrix)
   ccall((:fmpz_mat_zero, libflint), Nothing,
                (Ref{ZZMatrix},), z)
   return z
end

function mul!(z::Vector{ZZRingElem}, a::ZZMatrix, b::Vector{ZZRingElem})
   ccall((:fmpz_mat_mul_fmpz_vec_ptr, libflint), Nothing,
         (Ptr{Ref{ZZRingElem}}, Ref{ZZMatrix}, Ptr{Ref{ZZRingElem}}, Int),
         z, a, b, length(b))
   return z
end

function mul!(z::Vector{ZZRingElem}, a::Vector{ZZRingElem}, b::ZZMatrix)
   ccall((:fmpz_mat_fmpz_vec_mul_ptr, libflint), Nothing,
         (Ptr{Ref{ZZRingElem}}, Ptr{Ref{ZZRingElem}}, Int, Ref{ZZMatrix}),
         z, a, length(a), b)
   return z
end

function Generic.add_one!(a::ZZMatrix, i::Int, j::Int)
  @boundscheck Generic._checkbounds(a, i, j)
  GC.@preserve a begin
    x = mat_entry_ptr(a, i, j)
    ccall((:fmpz_add_si, libflint), Nothing,
          (Ptr{ZZRingElem}, Ptr{ZZRingElem}, Int),
          x, x, 1)
  end
  return a
end

###############################################################################
#
#   Parent object call overloads
#
###############################################################################

function (a::ZZMatrixSpace)()
   z = ZZMatrix(nrows(a), ncols(a))
   return z
end

function (a::ZZMatrixSpace)(arr::AbstractMatrix{ZZRingElem})
   _check_dim(nrows(a), ncols(a), arr)
   z = ZZMatrix(nrows(a), ncols(a), arr)
   return z
end

function (a::ZZMatrixSpace)(arr::AbstractMatrix{T}) where {T <: Integer}
   _check_dim(nrows(a), ncols(a), arr)
   z = ZZMatrix(nrows(a), ncols(a), arr)
   return z
end

function (a::ZZMatrixSpace)(arr::AbstractVector{ZZRingElem})
   _check_dim(nrows(a), ncols(a), arr)
   z = ZZMatrix(nrows(a), ncols(a), arr)
   return z
end

function (a::ZZMatrixSpace)(arr::AbstractVector{T}) where {T <: Integer}
   _check_dim(nrows(a), ncols(a), arr)
   z = ZZMatrix(nrows(a), ncols(a), arr)
   return z
end

function (a::ZZMatrixSpace)(d::ZZRingElem)
   z = ZZMatrix(nrows(a), ncols(a), d)
   return z
end

function (a::ZZMatrixSpace)(d::Integer)
   z = ZZMatrix(nrows(a), ncols(a), ZZRingElem(d))
   return z
end

###############################################################################
#
#   Conversions and promotions
#
###############################################################################

promote_rule(::Type{ZZMatrix}, ::Type{T}) where {T <: Integer} = ZZMatrix

promote_rule(::Type{ZZMatrix}, ::Type{ZZRingElem}) = ZZMatrix

function (::Type{Base.Matrix{Int}})(A::ZZMatrix)
    m, n = size(A)

    fittable = [fits(Int, A[i, j]) for i in 1:m, j in 1:n]
    if !all(fittable)
        error("When trying to convert a ZZMatrix to a Matrix{Int}, some elements were too large to fit into Int: try to convert to a matrix of BigInt.")
    end

    mat::Matrix{Int} = Int[A[i, j] for i in 1:m, j in 1:n]
    return mat
end

function (::Type{Base.Matrix{BigInt}})(A::ZZMatrix)
    m, n = size(A)
    # No check: always ensured to fit a BigInt.
    mat::Matrix{BigInt} = BigInt[A[i, j] for i in 1:m, j in 1:n]
    return mat
end

###############################################################################
#
#   Matrix constructor
#
###############################################################################

function matrix(R::ZZRing, arr::AbstractMatrix{ZZRingElem})
   z = ZZMatrix(size(arr, 1), size(arr, 2), arr)
   return z
end

function matrix(R::ZZRing, arr::AbstractMatrix{<: Integer})
   z = ZZMatrix(size(arr, 1), size(arr, 2), arr)
   return z
end

function matrix(R::ZZRing, r::Int, c::Int, arr::AbstractVector{ZZRingElem})
   _check_dim(r, c, arr)
   z = ZZMatrix(r, c, arr)
   return z
end

function matrix(R::ZZRing, r::Int, c::Int, arr::AbstractVector{<: Integer})
   _check_dim(r, c, arr)
   z = ZZMatrix(r, c, arr)
   return z
end

###############################################################################
#
#  Zero matrix
#
###############################################################################

function zero_matrix(R::ZZRing, r::Int, c::Int)
   if r < 0 || c < 0
     error("dimensions must not be negative")
   end
   z = ZZMatrix(r, c)
   return z
end

###############################################################################
#
#  Identity matrix
#
###############################################################################

function identity_matrix(R::ZZRing, n::Int)
   if n < 0
     error("dimension must not be negative")
   end
   z = ZZMatrix(n, n)
   ccall((:fmpz_mat_one, libflint), Nothing, (Ref{ZZMatrix}, ), z)
   return z
end

###############################################################################
#
#   matrix_space constructor
#
###############################################################################

function matrix_space(R::ZZRing, r::Int, c::Int; cached::Bool = true)
   # TODO/FIXME: `cached` is ignored and only exists for backwards compatibility
   return ZZMatrixSpace(r, c)
end

################################################################################
#
#  Entry pointers
#
################################################################################
 
@inline mat_entry_ptr(A::ZZMatrix, i::Int, j::Int) = 
   ccall((:fmpz_mat_entry, libflint), 
      Ptr{ZZRingElem}, (Ref{ZZMatrix}, Int, Int), A, i-1, j-1)
