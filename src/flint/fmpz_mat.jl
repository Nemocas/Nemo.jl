###############################################################################
#
#   ZZMatrix.jl : FLINT matrices over ZZRingElem
#
###############################################################################

###############################################################################
#
#   Data type and parent object methods
#
###############################################################################

base_ring(a::ZZMatrix) = ZZ

dense_matrix_type(::Type{ZZRingElem}) = ZZMatrix

is_zero_initialized(::Type{ZZMatrix}) = true

###############################################################################
#
#   View and sub
#
###############################################################################

function _checkrange_or_empty(l::Int, start::Int, stop::Int)
  (stop < start) ||
  (_checkbounds(l, start) &&
   _checkbounds(l, stop))
end

function _view_window(x::ZZMatrix, r1::Int, c1::Int, r2::Int, c2::Int)

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
  @ccall libflint.fmpz_mat_window_init(b::Ref{ZZMatrix}, x::Ref{ZZMatrix}, (r1 - 1)::Int, (c1 - 1)::Int, r2::Int, c2::Int)::Nothing
  finalizer(_fmpz_mat_window_clear_fn, b)
  return b
end

function Base.reshape(x::ZZMatrix, r::Int, c::Int)
  @assert nrows(x) * ncols(x) == r*c
  @assert r == 1

  b = ZZMatrix()
  b.view_parent = x
  @ccall libflint.fmpz_mat_window_init(b::Ref{ZZMatrix}, x::Ref{ZZMatrix}, 0::Int, 0::Int, r::Int, c::Int)::Nothing
  finalizer(_fmpz_mat_window_clear_fn, b)
  return b
end

function _fmpz_mat_window_clear_fn(a::ZZMatrix)
  @ccall libflint.fmpz_mat_window_clear(a::Ref{ZZMatrix})::Nothing
end

###############################################################################
#
#   Basic manipulation
#
###############################################################################

function getindex!(v::ZZRingElem, a::ZZMatrix, r::Int, c::Int)
  GC.@preserve a begin
    z = mat_entry_ptr(a, r, c)
    set!(v, z)
  end
end

@inline function getindex(a::ZZMatrix, r::Int, c::Int)
  @boundscheck _checkbounds(a, r, c)
  v = ZZRingElem()
  GC.@preserve a begin
    z = mat_entry_ptr(a, r, c)
    set!(v, z)
  end
  return v
end

@inline function setindex!(a::ZZMatrix, d::IntegerUnion, r::Int, c::Int)
  @boundscheck _checkbounds(a, r, c)
  GC.@preserve a begin
    z = mat_entry_ptr(a, r, c)
    set!(z, flintify(d))
  end
end

function setindex!(a::ZZMatrix, b::ZZMatrix, r::UnitRange{Int64}, c::UnitRange{Int64})
  _checkbounds(a, r, c)
  size(b) == (length(r), length(c)) || throw(DimensionMismatch("tried to assign a $(size(b, 1))x$(size(b, 2)) matrix to a $(length(r))x$(length(c)) destination"))
  A = view(a, r, c)
  @ccall libflint.fmpz_mat_set(A::Ref{ZZMatrix}, b::Ref{ZZMatrix})::Nothing
end

@inline number_of_rows(a::ZZMatrix) = a.r

@inline number_of_columns(a::ZZMatrix) = a.c

iszero(a::ZZMatrix) = @ccall libflint.fmpz_mat_is_zero(a::Ref{ZZMatrix})::Bool

isone(a::ZZMatrix) = @ccall libflint.fmpz_mat_is_one(a::Ref{ZZMatrix})::Bool

@inline function is_zero_entry(A::ZZMatrix, i::Int, j::Int)
  @boundscheck _checkbounds(A, i, j)
  GC.@preserve A begin
    x = mat_entry_ptr(A, i, j)
    return is_zero(x)
  end
end

@inline function is_positive_entry(A::ZZMatrix, i::Int, j::Int)
  @boundscheck _checkbounds(A, i, j)
  GC.@preserve A begin
    m = mat_entry_ptr(A, i, j)
    return is_positive(m)
  end
end

@inline function is_negative_entry(A::ZZMatrix, i::Int, j::Int)
  @boundscheck _checkbounds(A, i, j)
  GC.@preserve A begin
    m = mat_entry_ptr(A, i, j)
    return is_negative(m)
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
    entries = convert(Ptr{Int}, a.entries)
    for i in 1:r
      h = _hash_integer_array(entries + (i - 1) * a.stride * sizeof(Int), c, h)
    end
    return xor(h, 0x5c22af6d5986f453%UInt)
  end
end

###############################################################################
#
#   Unary operations
#
###############################################################################

function -(x::ZZMatrix)
  z = similar(x)
  neg!(z, x)
  return z
end

###############################################################################
#
#   transpose
#
###############################################################################

function transpose(x::ZZMatrix)
  z = similar(x, ncols(x), nrows(x))
  @ccall libflint.fmpz_mat_transpose(z::Ref{ZZMatrix}, x::Ref{ZZMatrix})::Nothing
  return z
end

function transpose!(A::ZZMatrix, B::ZZMatrix)
  @ccall libflint.fmpz_mat_transpose(A::Ref{ZZMatrix}, B::Ref{ZZMatrix})::Nothing
  return A
end

###############################################################################
#
#   Row and column swapping
#
###############################################################################

function swap_rows!(x::ZZMatrix, i::Int, j::Int)
  @ccall libflint.fmpz_mat_swap_rows(x::Ref{ZZMatrix}, C_NULL::Ptr{Nothing}, (i - 1)::Int, (j - 1)::Int)::Nothing
  return x
end

function swap_cols!(x::ZZMatrix, i::Int, j::Int)
  @ccall libflint.fmpz_mat_swap_cols(x::Ref{ZZMatrix}, C_NULL::Ptr{Nothing}, (i - 1)::Int, (j - 1)::Int)::Nothing
  return x
end

function reverse_rows!(x::ZZMatrix)
  @ccall libflint.fmpz_mat_invert_rows(x::Ref{ZZMatrix}, C_NULL::Ptr{Nothing})::Nothing
  return x
end

function reverse_cols!(x::ZZMatrix)
  @ccall libflint.fmpz_mat_invert_cols(x::Ref{ZZMatrix}, C_NULL::Ptr{Nothing})::Nothing
  return x
end

###############################################################################
#
#   Binary operations
#
###############################################################################

function +(x::ZZMatrix, y::ZZMatrix)
  check_parent(x, y)
  z = similar(x)
  add!(z, x, y)
  return z
end

function -(x::ZZMatrix, y::ZZMatrix)
  check_parent(x, y)
  z = similar(x)
  sub!(z, x, y)
  return z
end

function *(x::ZZMatrix, y::ZZMatrix)
  ncols(x) != nrows(y) && error("Incompatible matrix dimensions")
  if nrows(x) == ncols(y) && nrows(x) == ncols(x)
    z = similar(x)
  else
    z = similar(x, nrows(x), ncols(y))
  end
  mul!(z, x, y)
  return z
end

###############################################################################
#
#   Ad hoc binary operators
#
###############################################################################

for T in [Integer, ZZRingElem]
  @eval begin
    *(mat::ZZMatrix, scalar::$T) = mul!(similar(mat), mat, scalar)
    *(scalar::$T, mat::ZZMatrix) = mul!(similar(mat), mat, scalar)

    function +(mat::ZZMatrix, scalar::$T)
      z = deepcopy(mat)
      for i = 1:min(nrows(mat), ncols(mat))
        add!(mat_entry_ptr(z, i, i), scalar)
      end
      return z
    end

    +(scalar::$T, mat::ZZMatrix) = mat + scalar

    function -(mat::ZZMatrix, scalar::$T)
      z = deepcopy(mat)
      for i = 1:min(nrows(mat), ncols(mat))
        sub!(mat_entry_ptr(z, i, i), scalar)
      end
      return z
    end

    function -(scalar::$T, mat::ZZMatrix)
      z = -mat
      for i = 1:min(nrows(mat), ncols(mat))
        add!(mat_entry_ptr(z, i, i), scalar)
      end
      return z
    end
  end
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
  @ccall libflint.fmpz_mat_scalar_mul_2exp(z::Ref{ZZMatrix}, x::Ref{ZZMatrix}, y::Int)::Nothing
  return z
end

@doc raw"""
    >>(x::ZZMatrix, y::Int)

Return $x/2^y$ where rounding is towards zero.
"""
function >>(x::ZZMatrix, y::Int)
  y < 0 && throw(DomainError(y, "Exponent must be non-negative"))
  z = similar(x)
  @ccall libflint.fmpz_mat_scalar_tdiv_q_2exp(z::Ref{ZZMatrix}, x::Ref{ZZMatrix}, y::Int)::Nothing
  return z
end

###############################################################################
#
#   Powering
#
###############################################################################

# Cannot use IntegerUnion here to avoid ambiguity.

^(x::ZZMatrix, y::Int) = _powpow(x, y)

^(x::ZZMatrix, y::ZZRingElem) = _powpow(x, y)

function _powpow(x, y)
  @req nrows(x) == ncols(x) "Incompatible matrix dimensions"
  if y >= 0
    return pow!(similar(x), x, y)
  else
    fl, xi = is_invertible_with_inverse(x)
    @req fl "Matrix must be invertible"
    return pow!(xi, xi, -y)
  end
end

###############################################################################
#
#   Comparisons
#
###############################################################################

function ==(x::ZZMatrix, y::ZZMatrix)
  b = check_parent(x, y, false)
  b && @ccall libflint.fmpz_mat_equal(x::Ref{ZZMatrix}, y::Ref{ZZMatrix})::Bool
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

# Return a positive integer if A[i, j] > b, negative if A[i, j] < b, 0 otherwise
function compare_index(A::ZZMatrix, i::Int, j::Int, b::ZZRingElem)
  GC.@preserve A begin
    a = mat_entry_ptr(A, i, j)
    return @ccall libflint.fmpz_cmp(a::Ptr{ZZRingElem}, b::Ref{ZZRingElem})::Cint
  end
end

###############################################################################
#
#   Inversion
#
###############################################################################

function is_invertible_with_inverse(x::ZZMatrix)
  z = similar(x)
  d = ZZRingElem()
  @ccall libflint.fmpz_mat_inv(z::Ref{ZZMatrix}, d::Ref{ZZRingElem}, x::Ref{ZZMatrix})::Nothing
  if isone(d)
    return true, z
  end
  if d == -1
    return true, -z
  end
  return false, x
end

function inv(x::ZZMatrix)
  @req is_square(x) "Matrix not invertible"
  fl, z = is_invertible_with_inverse(x)
  @req fl "Matrix not invertible"
  return z
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

# Examples
```jldoctest
julia> A = ZZ[1 0 1; 2 3 1; 5 6 7]
[1   0   1]
[2   3   1]
[5   6   7]

julia> B, d = pseudo_inv(A)
([15 6 -3; -9 2 1; -3 -6 3], 12)
```
"""
function pseudo_inv(x::ZZMatrix)
  z = similar(x)
  d = ZZRingElem()
  @ccall libflint.fmpz_mat_inv(z::Ref{ZZMatrix}, d::Ref{ZZRingElem}, x::Ref{ZZMatrix})::Nothing
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
  @ccall libflint.fmpz_mat_scalar_divexact_si(z::Ref{ZZMatrix}, x::Ref{ZZMatrix}, y::Int)::Nothing
  return z
end

divexact(x::ZZMatrix, y::ZZRingElem; check::Bool=true) = divexact!(similar(x), x, y)

divexact(x::ZZMatrix, y::Integer; check::Bool=true) = divexact(x, ZZRingElem(y); check=check)

function divexact!(a::ZZMatrix, b::ZZMatrix, d::ZZRingElem)
  @ccall libflint.fmpz_mat_scalar_divexact_fmpz(a::Ref{ZZMatrix}, b::Ref{ZZMatrix}, d::Ref{ZZRingElem})::Nothing
  return a
end

###############################################################################
#
#   Kronecker product
#
###############################################################################

function kronecker_product(x::ZZMatrix, y::ZZMatrix)
  z = similar(x, nrows(x)*nrows(y), ncols(x)*ncols(y))
  @ccall libflint.fmpz_mat_kronecker_product(z::Ref{ZZMatrix}, x::Ref{ZZMatrix}, y::Ref{ZZMatrix})::Nothing
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
  @ccall libflint.fmpz_mat_scalar_mod_fmpz(z::Ref{ZZMatrix}, x::Ref{ZZMatrix}, y::Ref{ZZRingElem})::Nothing
  return z
end

@doc raw"""
    reduce_mod(x::ZZMatrix, y::Integer)

Reduce the entries of $x$ modulo $y$ and return the result.
"""
reduce_mod(x::ZZMatrix, y::Integer) = reduce_mod(x, ZZRingElem(y))

@doc raw"""
    mod!(M::ZZMatrix, p::ZZRingElem)

Reduces every entry modulo $p$ in-place, i.e. applies the mod function to every entry.
Positive residue system.
"""
function mod!(M::ZZMatrix, p::ZZRingElem)
  GC.@preserve M begin
    for i = 1:nrows(M)
      for j = 1:ncols(M)
        z = mat_entry_ptr(M, i, j)
        @ccall libflint.fmpz_mod(z::Ptr{ZZRingElem}, z::Ptr{ZZRingElem}, p::Ref{ZZRingElem})::Nothing
      end
    end
  end
  return nothing
end

@doc raw"""
    mod(M::ZZMatrix, p::ZZRingElem) -> ZZMatrix

Reduces every entry modulo $p$, i.e. applies the mod function to every entry.
"""
function mod(M::ZZMatrix, p::ZZRingElem)
  N = deepcopy(M)
  mod!(N, p)
  return N
end

@doc raw"""
    mod_sym!(M::ZZMatrix, p::ZZRingElem)

Reduces every entry modulo $p$ in-place, into the symmetric residue system.
"""
function mod_sym!(M::ZZMatrix, B::ZZRingElem)
  @assert !iszero(B)
  @ccall libflint.fmpz_mat_scalar_smod(M::Ref{ZZMatrix}, M::Ref{ZZMatrix}, B::Ref{ZZRingElem})::Nothing
end
mod_sym!(M::ZZMatrix, B::Integer) = mod_sym!(M, ZZRingElem(B))

function mod_sym!(M::ZZMatrix, N::ZZMatrix, B::ZZRingElem)
  @assert !iszero(B)
  @ccall libflint.fmpz_mat_scalar_smod(M::Ref{ZZMatrix}, N::Ref{ZZMatrix}, B::Ref{ZZRingElem})::Nothing
end

@doc raw"""
    mod_sym(M::ZZMatrix, p::ZZRingElem) -> ZZMatrix

Reduces every entry modulo $p$ into the symmetric residue system.
"""
function mod_sym(M::ZZMatrix, B::ZZRingElem)
  N = zero_matrix(ZZ, nrows(M), ncols(M))
  @ccall libflint.fmpz_mat_scalar_smod(N::Ref{ZZMatrix}, M::Ref{ZZMatrix}, B::Ref{ZZRingElem})::Nothing
  return N
end
mod_sym(M::ZZMatrix, B::Integer) = mod_sym(M, ZZRingElem(B))

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

  r = @ccall libflint.fmpz_mat_fflu(U::Ref{ZZMatrix}, d::Ref{ZZRingElem}, p.d::Ptr{Int}, x::Ref{ZZMatrix}, 0::Int)::Int

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
  @ccall libflint.fmpz_mat_charpoly(z::Ref{ZZPolyRingElem}, x::Ref{ZZMatrix})::Nothing
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
  @ccall libflint.fmpz_mat_minpoly(z::Ref{ZZPolyRingElem}, x::Ref{ZZMatrix})::Nothing
  return z
end

minpoly(x::ZZMatrix) = minpoly(polynomial_ring(ZZ; cached = false)[1], x)

###############################################################################
#
#   Determinant
#
###############################################################################

function det(x::ZZMatrix)
  nrows(x) != ncols(x) && error("Non-square matrix")
  z = ZZRingElem()
  @ccall libflint.fmpz_mat_det(z::Ref{ZZRingElem}, x::Ref{ZZMatrix})::Nothing
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
  @ccall libflint.fmpz_mat_det_divisor(z::Ref{ZZRingElem}, x::Ref{ZZMatrix})::Nothing
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
  @ccall libflint.fmpz_mat_det_modular_given_divisor(z::Ref{ZZRingElem}, x::Ref{ZZMatrix}, d::Ref{ZZRingElem}, proved::Cint)::Nothing
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
  GC.@preserve M begin
    for i in 1:n
      zero!(r)
      M_ptr = mat_entry_ptr(M, i, 1)
      for j in 1:n
        addmul!(r, M_ptr, M_ptr)
        M_ptr += sizeof(ZZRingElem)
      end
      if iszero(r)
        return r
      end
      mul!(H, H, r)
    end
  end
  return H
end

function maximum(::typeof(nbits), M::ZZMatrix)
  mx = 0
  n = nrows(M)
  m = ncols(M)
  Base.GC.@preserve M begin
    for i in 1:n
      M_ptr = mat_entry_ptr(M, i, 1)
      for j in 1:m
        #a zero fmpz is a binary zero, hence this works
        #fmpz_bits does not work on 0 I think (at least is it unneccessary)
        #this is not going through the "correct" order of the rows, but 
        #for this is does not matter
        if !iszero(unsafe_load(reinterpret(Ptr{Int}, M_ptr)))
          mx = max(mx, nbits(M_ptr))
        end
        M_ptr += sizeof(ZZRingElem)
      end
    end
  end
  return Int(mx)
end

function maximum(f::typeof(abs), a::ZZMatrix)
  r = ZZRingElem()
  GC.@preserve a r begin
    m = mat_entry_ptr(a, 1, 1)
    for i = 1:nrows(a)
      for j = 1:ncols(a)
        z = mat_entry_ptr(a, i, j)
        if (@ccall libflint.fmpz_cmpabs(m::Ptr{ZZRingElem}, z::Ptr{ZZRingElem})::Cint) < 0
          m = z
        end
      end
    end
    @ccall libflint.fmpz_abs(r::Ref{ZZRingElem}, m::Ptr{ZZRingElem})::Nothing
  end
  return r
end

function maximum(a::ZZMatrix)
  r = ZZRingElem()
  GC.@preserve a r begin
    m = mat_entry_ptr(a, 1, 1)
    for i = 1:nrows(a)
      for j = 1:ncols(a)
        z = mat_entry_ptr(a, i, j)
        if (@ccall libflint.fmpz_cmp(m::Ptr{ZZRingElem}, z::Ptr{ZZRingElem})::Cint) < 0
          m = z
        end
      end
    end
    set!(r, m)
  end
  return r
end

function minimum(a::ZZMatrix)
  r = ZZRingElem()
  GC.@preserve a r begin
    m = mat_entry_ptr(a, 1, 1)
    for i = 1:nrows(a)
      for j = 1:ncols(a)
        z = mat_entry_ptr(a, i, j)
        if (@ccall libflint.fmpz_cmp(m::Ptr{ZZRingElem}, z::Ptr{ZZRingElem})::Cint) > 0
          m = z
        end
      end
    end
    set!(r, m)
  end
  return r
end

###############################################################################
#
#   Gram matrix
#
###############################################################################

function gram(x::ZZMatrix)
  z = similar(x, nrows(x), nrows(x))
  @ccall libflint.fmpz_mat_gram(z::Ref{ZZMatrix}, x::Ref{ZZMatrix})::Nothing
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
  success = @ccall libflint.fmpz_mat_hadamard(z::Ref{ZZMatrix})::Bool
  !success && error("Unable to create Hadamard matrix")
  return z
end

@doc raw"""
    is_hadamard(x::ZZMatrix)

Return `true` if the given matrix is Hadamard, otherwise return `false`.
"""
function is_hadamard(x::ZZMatrix)
  return @ccall libflint.fmpz_mat_is_hadamard(x::Ref{ZZMatrix})::Bool
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
  @ccall libflint.fmpz_mat_hnf(z::Ref{ZZMatrix}, x::Ref{ZZMatrix})::Nothing
  return z
end

function hnf!(x::ZZMatrix)
  if nrows(x) * ncols(x) > 100
    z = hnf(x)
    @ccall libflint.fmpz_mat_set(x::Ref{ZZMatrix}, z::Ref{ZZMatrix})::Nothing

    return x
  end
  @ccall libflint.fmpz_mat_hnf(x::Ref{ZZMatrix}, x::Ref{ZZMatrix})::Nothing
  return x
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
  @ccall libflint.fmpz_mat_hnf_transform(z::Ref{ZZMatrix}, u::Ref{ZZMatrix}, x::Ref{ZZMatrix})::Nothing
  return z, u
end

@doc raw"""
    hnf_modular(x::ZZMatrix, d::ZZRingElem)

Compute the Hermite normal form of $x$ given that $d$ is a multiple of the
determinant of the nonzero rows of $x$.
"""
function hnf_modular(x::ZZMatrix, d::ZZRingElem)
  z = similar(x)
  @ccall libflint.fmpz_mat_hnf_modular(z::Ref{ZZMatrix}, x::Ref{ZZMatrix}, d::Ref{ZZRingElem})::Nothing
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
  @ccall libflint.fmpz_mat_hnf_modular_eldiv(z::Ref{ZZMatrix}, d::Ref{ZZRingElem})::Nothing
  return z
end

@doc raw"""
    is_hnf(x::ZZMatrix)

Return `true` if the given matrix is in Hermite Normal Form, otherwise return
`false`.
"""
function is_hnf(x::ZZMatrix)
  return @ccall libflint.fmpz_mat_is_in_hnf(x::Ref{ZZMatrix})::Bool
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
  u = identity_matrix(ZZ, nrows(x))
  @ccall libflint.fmpz_lll(z::Ref{ZZMatrix}, u::Ref{ZZMatrix}, ctx::Ref{LLLContext})::Nothing
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
  @ccall libflint.fmpz_lll(x::Ref{ZZMatrix}, C_NULL::Ptr{nothing}, ctx::Ref{LLLContext})::Nothing
  return x
end

# for lll_gram, we use the following helpers to deal with the degenerate case.

function _is_definitely_full_rank(x::ZZMatrix)
  if Int == Int64
    p = 1099511627791 % Int64
  else
    p = 1048583 % Int32
  end
  F = Nemo.Native.GF(p; cached = false)
  xmodp = F.(x)
  return rank(xmodp) == nrows(x)
end

function _complete_to_basis(x::ZZMatrix)
  @assert nrows(x) <= ncols(x)
  # compute column HNF and take the inverse
  h, t = hnf_with_transform(transpose(x))
  for i in 1:nrows(x)
    @assert is_one(h[i, i])
  end
  transpose!(t)
  return inv!(t)
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
  @req is_square(x) && is_symmetric(x) "The matrix must be a symmetric square matrix"
  # try to recognize the definite case
  if nrows(x) == 0
    return x, x
  end
  if _is_definitely_full_rank(x)
    u = identity_matrix(ZZ, nrows(x))
    if x[1, 1] < 0
      y = neg(x)
      _lll_gram_with_transform!(y, u, ctx)
      return neg!(y), u
    else
      y = deepcopy(x)
      _lll_gram_with_transform!(y, u, ctx)
      return y, u
    end
  end
  # remove the radical using a unimodular transformation
  L = kernel(x; side = :left)
  n = nrows(L)
  r = nrows(x) - n
  C = _complete_to_basis(L)
  # move kernel to bottom, so that
  # C * x * transpose(C) = [ * | 0 ]
  #                        [-------]
  #                        [ 0 | 0 ]
  uu = deepcopy(reverse_rows!(C))
  mul!(C, C, x * transpose(C))
  u = identity_matrix(ZZ, nrows(x))
  if C[1, 1] < 0
    _lll_gram_with_transform!(neg!(@view(C[1:r, 1:r])), @view(u[1:r, 1:r]), ctx)
    neg!(C)
  else
    _lll_gram_with_transform!(@view(C[1:r, 1:r]), @view(u[1:r, 1:r]), ctx)
  end
  return C, u * uu
end

function _lll_gram_with_transform!(x::ZZMatrix, u::ZZMatrix, ctx::LLLContext = LLLContext(0.99, 0.51, :gram))
  # modifies x and u
  @ccall libflint.fmpz_lll(x::Ref{ZZMatrix}, u::Ref{ZZMatrix}, ctx::Ref{LLLContext})::Nothing
  return x, u
end

function _lll_gram_with_transform!(x::ZZMatrix, ctx::LLLContext = LLLContext(0.99, 0.51, :gram))
  u = identity_matrix(ZZ, nrows(x))
  @ccall libflint.fmpz_lll(x::Ref{ZZMatrix}, u::Ref{ZZMatrix}, ctx::Ref{LLLContext})::Nothing
  return x, u
end

@doc raw"""
    lll_gram(x::ZZMatrix, ctx::LLLContext = LLLContext(0.99, 0.51, :gram))

Return the Gram matrix $L$ of an LLL-reduced basis of the lattice given by the
Gram matrix $x$.
The matrix $x$ must be symmetric and semidefinite. If an indefinite matrix is
provided, the output is undefined.

By default, the LLL is performed with reduction parameters $\delta = 0.99$ and
$\eta = 0.51$. These defaults can be overridden by specifying an optional context
object.
"""
function lll_gram(x::ZZMatrix, ctx::LLLContext = LLLContext(0.99, 0.51, :gram))
  @req is_square(x) && is_symmetric(x) "The matrix must be a symmetric square matrix"
  z = deepcopy(x)
  return lll_gram!(z)
end
@doc raw"""
    lll_gram!(x::ZZMatrix, ctx::LLLContext = LLLContext(0.99, 0.51, :gram))

Compute the Gram matrix of an LLL-reduced basis of the lattice given by the
Gram matrix $x$ inplace.
The matrix $x$ must be symmetric and semidefinite.

By default, the LLL is performed with reduction parameters $\delta = 0.99$ and
$\eta = 0.51$. These defaults can be overridden by specifying an optional context
object.
"""
function lll_gram!(x::ZZMatrix, ctx::LLLContext = LLLContext(0.99, 0.51, :gram))
  if nrows(x) == 0
    return x
  end
  # try to recognize the definite case
  if _is_definitely_full_rank(x)
    if x[1, 1] < 0
      return neg!(_lll_gram!(!neg(x), ctx))
    else
      return _lll_gram!(x, ctx)
    end
  end
  # remove the radical using a unimodular transformation
  L = kernel(x; side = :left)
  n = nrows(L)
  r = nrows(x) - n
  C = _complete_to_basis(L)
  # move kernel to bottom, so that
  # C * x * transpose(C) = [ * | 0 ]
  #                        [-------]
  #                        [ 0 | 0 ]
  reverse_rows!(C)
  mul!(x, x, transpose(C))
  C = mul!(x, C, x)
  if C[1, 1] < 0
    neg!(_lll_gram!(neg!(@view(x[1:r, 1:r])), ctx))
  else
    _lll_gram!(@view(x[1:r, 1:r]), ctx)
  end
  return x
end

function _lll_gram!(x::ZZMatrix, ctx::LLLContext = LLLContext(0.99, 0.51, :gram))
  @ccall libflint.fmpz_lll(x::Ref{ZZMatrix}, C_NULL::Ptr{Nothing}, ctx::Ref{LLLContext})::Nothing
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
  d = Int(@ccall libflint.fmpz_lll_with_removal(z::Ref{ZZMatrix}, u::Ref{ZZMatrix}, b::Ref{ZZRingElem}, ctx::Ref{LLLContext})::Cint)
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
  d = Int(@ccall libflint.fmpz_lll_with_removal(z::Ref{ZZMatrix}, C_NULL::Ptr{Nothing}, b::Ref{ZZRingElem}, ctx::Ref{LLLContext})::Cint)
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
  rank = @ccall libflint.fmpz_mat_nullspace(u::Ref{ZZMatrix}, x::Ref{ZZMatrix})::Int
  return rank, u
end

###############################################################################
#
#   Rank
#
###############################################################################

function rank(x::ZZMatrix)
  return @ccall libflint.fmpz_mat_rank(x::Ref{ZZMatrix})::Int
end

###############################################################################
#
#   Reduced row echelon form
#
###############################################################################

function rref(x::ZZMatrix)
  z = similar(x)
  d = ZZRingElem()
  r = @ccall libflint.fmpz_mat_rref(z::Ref{ZZMatrix}, d::Ref{ZZRingElem}, x::Ref{ZZMatrix})::Int
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
@inline snf(x::ZZMatrix) = _snf(x)

@inline _snf(x) = __snf(x)

function __snf(x::ZZMatrix)
  z = similar(x)
  @ccall libflint.fmpz_mat_snf(z::Ref{ZZMatrix}, x::Ref{ZZMatrix})::Nothing
  return z
end

@doc raw"""
    snf_diagonal(x::ZZMatrix)

Given a diagonal matrix $x$ compute the Smith normal form of $x$.
"""
function snf_diagonal(x::ZZMatrix)
  z = similar(x)
  @ccall libflint.fmpz_mat_snf_diagonal(z::Ref{ZZMatrix}, x::Ref{ZZMatrix})::Nothing
  return z
end

@doc raw"""
    is_snf(x::ZZMatrix)

Return `true` if $x$ is in Smith normal form, otherwise return `false`.
"""
function is_snf(x::ZZMatrix)
  return @ccall libflint.fmpz_mat_is_in_snf(x::Ref{ZZMatrix})::Bool
end

################################################################################
#
#  Smith normal form with trafo
#
################################################################################

#=
g, e,f = gcdx(a, b)
U = [1 0 ; -divexact(b, g)*f 1]*[1 1; 0 1];
V = [e -divexact(b, g) ; f divexact(a, g)];

then U*[ a 0; 0 b] * V = [g 0 ; 0 l]
=#
@doc raw"""
    snf_with_transform(A::ZZMatrix, l::Bool = true, r::Bool = true) -> ZZMatrix, ZZMatrix, ZZMatrix

Given some integer matrix $A$, compute the Smith normal form (elementary
divisor normal form) of $A$. If `l` and/ or `r` are true, then the corresponding
left and/ or right transformation matrices are computed as well.
"""
function snf_with_transform(A::ZZMatrix, l::Bool=true, r::Bool=true)
  if r
    R = identity_matrix(ZZ, ncols(A))
  end

  if l
    L = identity_matrix(ZZ, nrows(A))
  end
  # TODO: if only one trafo is required, start with the HNF that does not
  #       compute the trafo
  #       Rationale: most of the work is on the 1st HNF..
  S = deepcopy(A)
  while !is_diagonal(S)
    if l
      S, T = hnf_with_transform(S)
      L = T * L
    else
      S = hnf!(S)
    end

    if is_diagonal(S)
      break
    end
    if r
      S, T = hnf_with_transform(transpose(S))
      R = T * R
    else
      S = hnf!(transpose(S))
    end
    S = transpose(S)
  end
  #this is probably not really optimal...
  for i = 1:min(nrows(S), ncols(S))
    if S[i, i] == 1
      continue
    end
    for j = i+1:min(nrows(S), ncols(S))
      if S[j, j] == 0
        continue
      end
      if S[i, i] != 0 && S[j, j] % S[i, i] == 0
        continue
      end
      g, e, f = gcdx(S[i, i], S[j, j])
      a = divexact(S[i, i], g)
      S[i, i] = g
      b = divexact(S[j, j], g)
      S[j, j] *= a
      if l
        # U = [1 0; -b*f 1] * [ 1 1; 0 1] = [1 1; -b*f -b*f+1]
        # so row i and j of L will be transformed. We do it naively
        # those 2x2 transformations of 2 rows should be a c-primitive
        # or at least a Nemo/Hecke primitive
        for k = 1:ncols(L)
          x = -b * f
          #          L[i,k], L[j,k] = L[i,k]+L[j,k], x*L[i,k]+(x+1)*L[j,k]
          L[i, k], L[j, k] = L[i, k] + L[j, k], x * (L[i, k] + L[j, k]) + L[j, k]
        end
      end
      if r
        # V = [e -b ; f a];
        # so col i and j of R will be transformed. We do it naively
        # careful: at this point, R is still transposed
        for k = 1:nrows(R)
          R[i, k], R[j, k] = e * R[i, k] + f * R[j, k], -b * R[i, k] + a * R[j, k]
        end
      end
    end
  end

  # It might be the case that S was diagonal with negative diagonal entries.
  for i in 1:min(nrows(S), ncols(S))
    if S[i, i] < 0
      if l
        multiply_row!(L, ZZRingElem(-1), i)
      end
      S[i, i] = -S[i, i]
    end
  end

  if l
    if r
      return S, L, transpose(R)
    else
      # last is dummy
      return S, L, L
    end
  elseif r
    # second is dummy
    return S, R, transpose(R)
  else
    # last two are dummy
    return S, S, S
  end
end

###############################################################################
#
#   manual linear algebra: row and col operations
#
###############################################################################

function AbstractAlgebra.multiply_row!(A::ZZMatrix, s::Union{Int, ZZRingElemOrPtr}, i::Int, cols::UnitRange{Int}=1:ncols(A))
  @assert 1 <= i <= nrows(A)
  @assert 1 <= first(cols) && last(cols) <= ncols(A)
  c = first(cols)
  GC.@preserve A begin
    # these are Ptr{ZZRingElem}
    i_ptr = mat_entry_ptr(A, i, c)
    for k = cols
      mul!(i_ptr, s, i_ptr)
      i_ptr += sizeof(ZZRingElem)
    end
  end
end

function AbstractAlgebra.multiply_column!(A::ZZMatrix, s::Union{Int, ZZRingElemOrPtr}, i::Int, j::Int, rows::UnitRange{Int}=1:nrows(A))
  @assert 1 <= j <= ncols(A)
  @assert 1 <= first(rows)
  @assert last(rows) <= nrows(A)
  GC.@preserve A begin
    for k = rows
      i_ptr = mat_entry_ptr(A, k, i)
      mul!(i_ptr, s, i_ptr)
    end
  end
end


function AbstractAlgebra.add_row!(A::ZZMatrix, s::Union{ZZRingElemOrPtr, Int}, i::Int, j::Int, cols::UnitRange{Int}=1:ncols(A))
  @assert 1 <= i <= nrows(A)
  @assert 1 <= j <= nrows(A)
  @assert 1 <= first(cols) && last(cols) <= ncols(A)
  c = first(cols)
  GC.@preserve A begin
    i_ptr = mat_entry_ptr(A, i, c)
    j_ptr = mat_entry_ptr(A, j, c)
    for k = cols
      addmul!(j_ptr, s, i_ptr)
      i_ptr += sizeof(ZZRingElem)
      j_ptr += sizeof(ZZRingElem)
    end
  end
end

function AbstractAlgebra.add_column!(A::ZZMatrix, s::Union{ZZRingElemOrPtr, Int}, i::Int, j::Int, rows::UnitRange{Int}=1:nrows(A))
  @assert 1 <= i <= ncols(A)
  @assert 1 <= j <= ncols(A)
  @assert 1 <= first(rows)
  @assert last(rows) <= nrows(A)
  GC.@preserve A begin
    for k = rows
      i_ptr = mat_entry_ptr(A, k, i)
      j_ptr = mat_entry_ptr(A, k, j)
      addmul!(j_ptr, s, i_ptr)
    end
  end
end

###############################################################################
#
#   Linear solving
#
###############################################################################

Solve.matrix_normal_form_type(::ZZRing) = Solve.HermiteFormTrait()
Solve.matrix_normal_form_type(::ZZMatrix) = Solve.HermiteFormTrait()

function Solve._can_solve_internal_no_check(::Solve.HermiteFormTrait, A::ZZMatrix, b::ZZMatrix, task::Symbol; side::Symbol = :left)
  if side === :left
    fl, sol, K = Solve._can_solve_internal_no_check(Solve.HermiteFormTrait(), transpose(A), transpose(b), task, side = :right)
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
            mul!(t, q, H_ptr)
            sub!(b_ptr, b_ptr, t)
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
  K = transpose(Solve._kernel_of_hnf(H, T))
  return fl, sol, K
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
          set!(X_ptr, A_ptr)
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
      if ncols(N) == 0
        continue
      end
      for j in 1:nrows(N)
        M_ptr = mat_entry_ptr(M, s+j, 1)
        N_ptr = mat_entry_ptr(N, j, 1)
        for k in 1:ncols(N)
          set!(M_ptr, N_ptr)
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
    error("Matrices must have the same number of rows")
  end

  M = zero_matrix(ZZ, nrows(A[1]), sum(ncols, A, init = 0))

  s = 0
  for N in A
    GC.@preserve M N begin
      if ncols(N) == 0
        continue
      end
      for j in 1:nrows(N)
        M_ptr = mat_entry_ptr(M, j, s+1)
        N_ptr = mat_entry_ptr(N, j, 1)
        for k in 1:ncols(N)
          set!(M_ptr, N_ptr)
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
  nonsing = @ccall libflint.fmpz_mat_solve(z::Ref{ZZMatrix}, d::Ref{ZZRingElem}, a::Ref{ZZMatrix}, b::Ref{ZZMatrix})::Bool
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
  nonsing = @ccall libflint.fmpz_mat_solve_dixon(z::Ref{ZZMatrix}, d::Ref{ZZRingElem}, a::Ref{ZZMatrix}, b::Ref{ZZMatrix})::Bool
  !nonsing && error("Singular matrix in solve")
  return z, d
end

#XU = B. only the upper triangular part of U is used
function AbstractAlgebra._solve_triu_left(U::ZZMatrix, b::ZZMatrix; unipotent::Bool = false)
  n = ncols(U)
  m = nrows(b)
  R = base_ring(U)
  X = zero(b)
  tmp = zero_matrix(ZZ, 1, n)
  t = R()
  s = R()
  GC.@preserve U b X tmp begin
    for i = 1:m
      tmp_p = mat_entry_ptr(tmp, 1, 1)
      X_p = mat_entry_ptr(X, i, 1)
      for j = 1:n
        set!(tmp_p, X_p)
        X_p += sizeof(ZZRingElem)
        tmp_p += sizeof(ZZRingElem)
      end
      for j = 1:n
        zero!(s)

        tmp_p = mat_entry_ptr(tmp, 1, 1)
        for k = 1:j-1
          U_p = mat_entry_ptr(U, k, j)
          addmul!(s, U_p, tmp_p)
          tmp_p += sizeof(ZZRingElem)
        end
        sub!(s, mat_entry_ptr(b, i, j), s)
        if unipotent
          set!(mat_entry_ptr(tmp, 1, j), s)
        else
          divexact!(mat_entry_ptr(tmp, 1, j), s, mat_entry_ptr(U, j, j))
        end
      end
      tmp_p = mat_entry_ptr(tmp, 1, 1)
      X_p = mat_entry_ptr(X, i, 1)
      for j = 1:n
        set!(X_p, tmp_p)
        X_p += sizeof(ZZRingElem)
        tmp_p += sizeof(ZZRingElem)
      end
    end
  end
  return X
end

#UX = B, U has to be upper triangular
#I think due to the Strassen calling path, where Strasse.solve(side = :left) 
#call directly AA.solve_left, this has to be in AA and cannot be independent.
function AbstractAlgebra._solve_triu(U::ZZMatrix, b::ZZMatrix; side::Symbol=:left, unipotent::Bool = false) 
  if side == :left
    return AbstractAlgebra._solve_triu_left(U, b; unipotent)
  end
  @assert side == :right
  n = nrows(U)
  m = ncols(b)
  X = zero(b)
  tmp = zero_matrix(ZZ, 1, n)
  s = ZZ()
  GC.@preserve U b X tmp begin
    for i = 1:m
      tmp_ptr = mat_entry_ptr(tmp, 1, 1)
      for j = 1:n
        X_ptr = mat_entry_ptr(X, j, i)
        set!(tmp_ptr, X_ptr)
        tmp_ptr += sizeof(ZZRingElem)
      end
      for j = n:-1:1
        zero!(s)
        tmp_ptr = mat_entry_ptr(tmp, 1, j+1)
        for k = j + 1:n
          U_ptr = mat_entry_ptr(U, j, k)
          mul!(s, U_ptr, tmp_ptr)
          tmp_ptr += sizeof(ZZRingElem)
          #           s = addmul!(s, U[j, k], tmp[k])
        end
        b_ptr = mat_entry_ptr(b, j, i)
        sub!(s, b_ptr, s)
        #         s = b[j, i] - s
        tmp_ptr = mat_entry_ptr(tmp, 1, j)
        U_ptr = mat_entry_ptr(U, j, j)
        if unipotent
          set!(tmp_ptr, s)
        else
          divexact!(tmp_ptr, s, U_ptr)
        end
        #           tmp[j] = divexact(s, U[j,j])
      end
      tmp_ptr = mat_entry_ptr(tmp, 1, 1)
      for j = 1:n
        X_ptr = mat_entry_ptr(X, j, i)
        set!(X_ptr, tmp_ptr)
        tmp_ptr += sizeof(ZZRingElem)
      end
    end
  end
  return X
end

#solves Ax = B for A lower triagular. if f != 0 (f is true), the diagonal
#is assumed to be 1 and not actually used.
#the upper part of A is not used/ touched.
#one cannot assert is_lower_triangular as this is used for the inplace
#lu decomposition where the matrix is full, encoding an upper triangular
#using the diagonal and a lower triangular with trivial diagonal
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
          mul!(s, A_ptr, B_ptr)
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
  @ccall libflint.fmpz_mat_trace(d::Ref{ZZRingElem}, x::Ref{ZZMatrix})::Int
  return d
end

###############################################################################
#
#   Content
#
###############################################################################

function content(x::ZZMatrix)
  d = ZZRingElem()
  @ccall libflint.fmpz_mat_content(d::Ref{ZZRingElem}, x::Ref{ZZMatrix})::Nothing
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
  @ccall libflint.fmpz_mat_concat_horizontal(c::Ref{ZZMatrix}, a::Ref{ZZMatrix}, b::Ref{ZZMatrix})::Nothing
  return c
end

function vcat(a::ZZMatrix, b::ZZMatrix)
  ncols(a) != ncols(b) && error("Incompatible number of columns in vcat")
  c = similar(a, nrows(a) + nrows(b), ncols(a))
  @ccall libflint.fmpz_mat_concat_vertical(c::Ref{ZZMatrix}, a::Ref{ZZMatrix}, b::Ref{ZZMatrix})::Nothing
  return c
end

###############################################################################
#
#   Unsafe functions
#
###############################################################################

function Base.copy!(A::ZZMatrix, B::ZZMatrix)
  @ccall libflint.fmpz_mat_set(A::Ref{ZZMatrix}, B::Ref{ZZMatrix})::Cvoid
end

function zero!(z::ZZMatrixOrPtr)
  @ccall libflint.fmpz_mat_zero(z::Ref{ZZMatrix})::Nothing
  return z
end

function zero_row!(z::ZZMatrix, i::Int)
  GC.@preserve z begin
    z_ptr = mat_entry_ptr(z, i, 1)
    for i=1:ncols(z)
      zero!(z_ptr)
      z_ptr += sizeof(Int)
    end
  end
  return z
end

function one!(z::ZZMatrixOrPtr)
  @ccall libflint.fmpz_mat_one(z::Ref{ZZMatrix})::Nothing
  return z
end

function neg!(z::ZZMatrixOrPtr, w::ZZMatrixOrPtr)
  @ccall libflint.fmpz_mat_neg(z::Ref{ZZMatrix}, w::Ref{ZZMatrix})::Nothing
  return z
end

function add!(z::ZZMatrixOrPtr, x::ZZMatrixOrPtr, y::ZZMatrixOrPtr)
  @ccall libflint.fmpz_mat_add(z::Ref{ZZMatrix}, x::Ref{ZZMatrix}, y::Ref{ZZMatrix})::Nothing
  return z
end

function sub!(z::ZZMatrixOrPtr, x::ZZMatrixOrPtr, y::ZZMatrixOrPtr)
  @ccall libflint.fmpz_mat_sub(z::Ref{ZZMatrix}, x::Ref{ZZMatrix}, y::Ref{ZZMatrix})::Nothing
  return z
end

function sub!(A::ZZMatrix, B::ZZMatrix, m::Int)
  GC.@preserve A B begin
    for i=1:nrows(A)
      A_p = Nemo.mat_entry_ptr(A, i, i)
      B_p = Nemo.mat_entry_ptr(B, i, i)
      sub!(A_p, B_p, m)
    end
  end
  return A
end

function mul!(z::ZZMatrixOrPtr, x::ZZMatrixOrPtr, y::ZZMatrixOrPtr)
  @ccall libflint.fmpz_mat_mul(z::Ref{ZZMatrix}, x::Ref{ZZMatrix}, y::Ref{ZZMatrix})::Nothing
  return z
end

function mul_classical!(z::ZZMatrixOrPtr, x::ZZMatrixOrPtr, y::ZZMatrixOrPtr)
  @ccall libflint.fmpz_mat_mul_classical(z::Ref{ZZMatrix}, x::Ref{ZZMatrix}, y::Ref{ZZMatrix})::Nothing
  return z
end

function mul!(z::ZZMatrixOrPtr, a::ZZMatrixOrPtr, b::Int)
  @ccall libflint.fmpz_mat_scalar_mul_si(z::Ref{ZZMatrix}, a::Ref{ZZMatrix}, b::Int)::Nothing
  return z
end

function mul!(z::ZZMatrixOrPtr, a::ZZMatrixOrPtr, b::UInt)
  @ccall libflint.fmpz_mat_scalar_mul_ui(z::Ref{ZZMatrix}, a::Ref{ZZMatrix}, b::UInt)::Nothing
  return z
end

function mul!(z::ZZMatrixOrPtr, a::ZZMatrixOrPtr, b::ZZRingElemOrPtr)
  @ccall libflint.fmpz_mat_scalar_mul_fmpz(z::Ref{ZZMatrix}, a::Ref{ZZMatrix}, b::Ref{ZZRingElem})::Nothing
  return z
end

mul!(z::ZZMatrixOrPtr, a::ZZMatrixOrPtr, b::Integer) = mul!(z, a, flintify(b))
mul!(z::ZZMatrixOrPtr, a::IntegerUnionOrPtr, b::ZZMatrixOrPtr) = mul!(z, b, a)

function addmul!(z::ZZMatrixOrPtr, a::ZZMatrixOrPtr, b::ZZRingElemOrPtr)
  @ccall libflint.fmpz_mat_scalar_addmul_fmpz(z::Ref{ZZMatrix}, a::Ref{ZZMatrix}, b::Ref{ZZRingElem})::Nothing
  return z
end

function addmul!(z::ZZMatrixOrPtr, a::ZZMatrixOrPtr, b::Int)
  @ccall libflint.fmpz_mat_scalar_addmul_si(z::Ref{ZZMatrix}, a::Ref{ZZMatrix}, b::Int)::Nothing
  return z
end

function addmul!(z::ZZMatrixOrPtr, a::ZZMatrixOrPtr, b::UInt)
  @ccall libflint.fmpz_mat_scalar_addmul_ui(z::Ref{ZZMatrix}, a::Ref{ZZMatrix}, b::UInt)::Nothing
  return z
end

addmul!(z::ZZMatrixOrPtr, a::ZZMatrixOrPtr, b::Integer) = addmul!(z, a, flintify(b))
addmul!(z::ZZMatrixOrPtr, a::IntegerUnionOrPtr, b::ZZMatrixOrPtr) = addmul!(z, b, a)

# ignore fourth argument
addmul!(z::ZZMatrixOrPtr, x::ZZMatrixOrPtr, y::IntegerUnionOrPtr, ::ZZMatrixOrPtr) = addmul!(z, x, y)
addmul!(z::ZZMatrixOrPtr, x::IntegerUnionOrPtr, y::ZZMatrixOrPtr, ::ZZMatrixOrPtr) = addmul!(z, x, y)

function submul!(z::ZZMatrixOrPtr, a::ZZMatrixOrPtr, b::ZZRingElemOrPtr)
  @ccall libflint.fmpz_mat_scalar_submul_fmpz(z::Ref{ZZMatrix}, a::Ref{ZZMatrix}, b::Ref{ZZRingElem})::Nothing
  return z
end

function submul!(z::ZZMatrixOrPtr, a::ZZMatrixOrPtr, b::Int)
  @ccall libflint.fmpz_mat_scalar_submul_si(z::Ref{ZZMatrix}, a::Ref{ZZMatrix}, b::Int)::Nothing
  return z
end

function submul!(z::ZZMatrixOrPtr, a::ZZMatrixOrPtr, b::UInt)
  @ccall libflint.fmpz_mat_scalar_submul_ui(z::Ref{ZZMatrix}, a::Ref{ZZMatrix}, b::UInt)::Nothing
  return z
end

submul!(z::ZZMatrixOrPtr, a::ZZMatrixOrPtr, b::Integer) = submul!(z, a, flintify(b))
submul!(z::ZZMatrixOrPtr, a::IntegerUnionOrPtr, b::ZZMatrixOrPtr) = submul!(z, b, a)

# ignore fourth argument
submul!(z::ZZMatrixOrPtr, x::ZZMatrixOrPtr, y::IntegerUnionOrPtr, ::ZZMatrixOrPtr) = submul!(z, x, y)
submul!(z::ZZMatrixOrPtr, x::IntegerUnionOrPtr, y::ZZMatrixOrPtr, ::ZZMatrixOrPtr) = submul!(z, x, y)

function Generic.add_one!(a::ZZMatrix, i::Int, j::Int)
  @boundscheck _checkbounds(a, i, j)
  GC.@preserve a begin
    x = mat_entry_ptr(a, i, j)
    add!(x, 1)
  end
  return a
end

function shift!(g::ZZMatrix, l::Int)
  GC.@preserve g begin
    for i = 1:nrows(g)
      for j = 1:ncols(g)
        z = mat_entry_ptr(g, i, j)
        if l > 0
          @ccall libflint.fmpz_mul_2exp(z::Ptr{ZZRingElem}, z::Ptr{ZZRingElem}, l::Int)::Nothing
        else
          @ccall libflint.fmpz_tdiv_q_2exp(z::Ptr{ZZRingElem}, z::Ptr{ZZRingElem}, (-l)::Int)::Nothing
        end
      end
    end
  end
  return g
end

function pow!(z::ZZMatrixOrPtr, x::ZZMatrixOrPtr, n::IntegerUnion)
  @ccall libflint.fmpz_mat_pow(z::Ref{ZZMatrix}, x::Ref{ZZMatrix}, UInt(n)::UInt)::Nothing
  return z
end

################################################################################
#
#  Vector * Matrix and Matrix * Vector
#
################################################################################

# Vector{fmpz} * fmpz_mat can be performed using
# - fmpz_mat_fmpz_vec_mul_ptr
# - or conversion + fmpz_mat_mul
#
# The fmpz_mat_fmpz_vec_mul_ptr variants are not optimized.
# Thus, if the conversion is negliable, we convert and call fmpz_mat.
# The conversion is done on the julia side, trying to reduce the number of
# allocations and objects tracked by the GC.

function _very_unsafe_convert(::Type{ZZMatrix}, a::Vector{ZZRingElem}, row = true)
  # a must be GC.@preserved
  # row = true -> make it a row
  # row = false -> make it a column
  M = Nemo.@new_struct(ZZMatrix)
  Me = zeros(Int, length(a))
  M.entries = reinterpret(Ptr{ZZRingElem}, pointer(Me))
  if row
    M.r = 1
    M.c = length(a)
    M.stride = M.c
  else
    M.r = length(a)
    M.c = 1
    M.stride = M.c
  end
  for i in 1:length(a)
    Me[i] = a[i].d
  end
  return M, Me
end

function mul!_flint(z::Vector{ZZRingElem}, a::ZZMatrixOrPtr, b::Vector{ZZRingElem})
  @ccall libflint.fmpz_mat_mul_fmpz_vec_ptr(z::Ptr{Ref{ZZRingElem}}, a::Ref{ZZMatrix}, b::Ptr{Ref{ZZRingElem}}, length(b)::Int)::Nothing
  return z
end

function mul!_flint(z::Vector{ZZRingElem}, a::Vector{ZZRingElem}, b::ZZMatrixOrPtr)
  @ccall libflint.fmpz_mat_fmpz_vec_mul_ptr(z::Ptr{Ref{ZZRingElem}}, a::Ptr{Ref{ZZRingElem}}, length(a)::Int, b::Ref{ZZMatrix})::Nothing
  return z
end

function mul!(z::Vector{ZZRingElem}, a::ZZMatrixOrPtr, b::Vector{ZZRingElem})
  # cutoff for the flint method
  if nrows(a) < 50 && maximum(nbits, a) < 10
    return mul!_flint(z, a, b)
  end

  GC.@preserve z b begin
    bb, dk1 = _very_unsafe_convert(ZZMatrix, b, false)
    zz, dk3 = _very_unsafe_convert(ZZMatrix, z, false)
    GC.@preserve dk1 dk3 begin
      mul!(zz, a, bb)
      for i in 1:length(z)
        z[i].d = unsafe_load(zz.entries, i).d
      end
    end
  end
  return z
end

function mul!(z::Vector{ZZRingElem}, a::Vector{ZZRingElem}, b::ZZMatrixOrPtr)
  # cutoff for the flint method
  if nrows(b) < 50 && maximum(nbits, b) < 10
    return mul!_flint(z, a, b)
  end
  GC.@preserve z a begin
    aa, dk1 = _very_unsafe_convert(ZZMatrix, a)
    zz, dk3 = _very_unsafe_convert(ZZMatrix, z)
    GC.@preserve dk1 dk3 begin
      mul!(zz, aa, b)
      for i in 1:length(z)
        z[i].d = unsafe_load(zz.entries, i).d
      end
    end
  end
  return z
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

function (a::ZZMatrixSpace)(arr::AbstractVecOrMat{T}) where {T <: IntegerUnion}
  _check_dim(nrows(a), ncols(a), arr)
  z = ZZMatrix(nrows(a), ncols(a), arr)
  return z
end

function (a::ZZMatrixSpace)(d::ZZRingElem)
  z = ZZMatrix(nrows(a), ncols(a), d)
  return z
end

function (a::ZZMatrixSpace)(d::Integer)
  z = ZZMatrix(nrows(a), ncols(a), flintify(d))
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

function map_entries(R::zzModRing, M::ZZMatrix)
  MR = zero_matrix(R, nrows(M), ncols(M))
  @ccall libflint.fmpz_mat_get_nmod_mat(MR::Ref{zzModMatrix}, M::Ref{ZZMatrix})::Nothing
  return MR
end

function map_entries(F::fpField, M::ZZMatrix)
  MR = zero_matrix(F, nrows(M), ncols(M))
  @ccall libflint.fmpz_mat_get_nmod_mat(MR::Ref{fpMatrix}, M::Ref{ZZMatrix})::Nothing
  return MR
end

function map_entries(R::ZZModRing, M::ZZMatrix)
  N = zero_matrix(R, nrows(M), ncols(M))
  GC.@preserve M N begin
    for i = 1:nrows(M)
      for j = 1:ncols(M)
        m = mat_entry_ptr(M, i, j)
        n = mat_entry_ptr(N, i, j)
        @ccall libflint.fmpz_mod(n::Ptr{ZZRingElem}, m::Ptr{ZZRingElem}, R.n::Ref{ZZRingElem})::Nothing
      end
    end
  end
  return N
end

###############################################################################
#
#   Matrix constructor
#
###############################################################################

function matrix(R::ZZRing, arr::AbstractMatrix{<: IntegerUnion})
  z = ZZMatrix(size(arr, 1), size(arr, 2), arr)
  return z
end

function matrix(R::ZZRing, r::Int, c::Int, arr::AbstractVector{<: IntegerUnion})
  _check_dim(r, c, arr)
  z = ZZMatrix(r, c, arr)
  return z
end

function ZZMatrix(r::Int, c::Int, arr::AbstractMatrix{T}) where {T <: IntegerUnion}
  z = ZZMatrix(r, c)
  GC.@preserve z for i = 1:r
    for j = 1:c
      el = mat_entry_ptr(z, i, j)
      set!(el, flintify(arr[i, j]))
    end
  end
  return z
end

function ZZMatrix(r::Int, c::Int, arr::AbstractVector{T}) where {T <: IntegerUnion}
  z = ZZMatrix(r, c)
  GC.@preserve z for i = 1:r
    for j = 1:c
      el = mat_entry_ptr(z, i, j)
      set!(el, flintify(arr[(i-1)*c+j]))
    end
  end
  return z
end

function ZZMatrix(r::Int, c::Int, d::IntegerUnion)
  z = ZZMatrix(r, c)
  GC.@preserve z for i = 1:min(r, c)
    el = mat_entry_ptr(z, i, i)
    set!(el, d)
  end
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
  return one!(ZZMatrix(n, n))
end

################################################################################
#
#  Product of diagonal
#
################################################################################

function prod_diagonal(A::ZZMatrix)
  a = one(ZZ)
  GC.@preserve a A begin
    for i = 1:min(nrows(A),ncols(A))
      b = mat_entry_ptr(A, i, i)
      mul!(a, a, b)
    end
  end
  return a
end

################################################################################
#
#  Entry pointers
#
################################################################################

mat_entry_ptr(A::ZZMatrix, i::Int, j::Int) = A.entries + ((i - 1) * A.stride + (j - 1)) * sizeof(ZZRingElem)
