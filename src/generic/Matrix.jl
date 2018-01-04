###############################################################################
#
#   Matrix.jl : Generic mxn matrices over rings
#
###############################################################################

export MatrixSpace, fflu!, fflu, solve_triu, isrref,
       charpoly_danilevsky!, charpoly_danilevsky_ff!, hessenberg!, hessenberg,
       ishessenberg, identity_matrix, charpoly_hessenberg!, matrix, minpoly,
       typed_hvcat, typed_hcat, powers, randmat_triu, randmat_with_rank,
       similarity!, solve, solve_rational, hnf, hnf_minors,
       hnf_minors_with_trafo, hnf_with_trafo, snf, snf_with_trafo, weak_popov,
       weak_popov_with_trafo, extended_weak_popov,
       extended_weak_popov_with_trafo, rank_profile_popov, hnf_via_popov,
       hnf_via_popov_with_trafo, popov, det_popov, _check_dim, rows, cols,
       gram, rref, rref!, swap_rows, swap_rows!, hnf_kb, hnf_kb_with_trafo,
       hnf_cohen, hnf_cohen_with_trafo, snf_kb, snf_kb_with_trafo,
       find_pivot_popov, inv!, zero_matrix

###############################################################################
#
#   Similar and eye
#
###############################################################################

function similar(x::Mat{T}) where T <: RingElement
   R = base_ring(x)
   M = similar(x.entries)
   for i in 1:size(M, 1)
      for j in 1:size(M, 2)
         M[i, j] = zero(R)
      end
   end
   z = Mat{T}(M)
   z.base_ring = R
   return z
end

function similar(x::Mat{T}, r::Int, c::Int) where T <: RingElement
   R = base_ring(x)
   M = similar(x.entries, r, c)
   for i in 1:size(M, 1)
      for j in 1:size(M, 2)
         M[i, j] = zero(R)
      end
   end
   z = Mat{T}(M)
   z.base_ring = R
   return z
end

doc"""
    eye(x::Nemo.MatElem)
> Return the identity matrix with the same shape as $x$.
"""
function eye(x::Nemo.MatElem)
  z = similar(x)
  for i in 1:rows(x)
    z[i, i] = one(base_ring(x))
  end
  return z
end

doc"""
    eye(x::Nemo.MatElem, d::Int)
> Return the $d$-by-$d$ identity matrix with the same base ring as $x$.
"""
function eye(x::Nemo.MatElem, d::Int)
  z = similar(x, d, d)
  for i in 1:rows(z)
    z[i, i] = one(base_ring(x))
  end
  return z
end

###############################################################################
#
#   Data type and parent object methods
#
###############################################################################

parent_type(::Type{Mat{T}}) where T <: RingElement = MatSpace{T}

elem_type(::Type{MatSpace{T}}) where {T <: RingElement} = Mat{T}

doc"""
    base_ring{T <: RingElement}(S::Nemo.MatSpace{T})
> Return the base ring $R$ of the given matrix space.
"""
base_ring(a::Nemo.MatSpace{T}) where {T <: RingElement} = a.base_ring::parent_type(T)

doc"""
    base_ring(r::Nemo.MatElem)
> Return the base ring $R$ of the matrix space that the supplied matrix $r$
> belongs to.
"""
base_ring(a::Nemo.MatElem{T}) where {T <: RingElement} = a.base_ring::parent_type(T)

doc"""
    parent(a::Nemo.MatElem)
> Return the parent object of the given matrix.
"""
parent(a::Nemo.MatElem{T}, cached::Bool = true) where T <: RingElement =
    MatSpace{T}(a.base_ring, size(a.entries)..., cached)

function check_parent(a::Nemo.MatElem, b::Nemo.MatElem)
  (base_ring(a) != base_ring(b) || rows(a) != rows(b) || cols(a) != cols(b)) &&
                error("Incompatible matrix spaces in matrix operation")
end

function _check_dim(r::Int, c::Int, arr::Array{T, 2}, transpose::Bool = false) where {T}
  if !transpose
    size(arr) != (r, c) && throw(ErrorConstrDimMismatch(r, c, size(arr)...))
  else
    size(arr) != (c, r) && throw(ErrorConstrDimMismatch(r, c, (reverse(size(arr)))...))
  end
  return nothing
end

function _check_dim(r::Int, c::Int, arr::Array{T, 1}) where {T}
  length(arr) != r*c && throw(ErrorConstrDimMismatch(r, c, length(arr)))
  return nothing
end

function _checkbounds(i::Int, j::Int)
   j >= 1 && j <= i
end

function _checkbounds(A, i::Int, j::Int)
  (_checkbounds(rows(A), i) && _checkbounds(cols(A), j)) ||
            Base.throw_boundserror(A, (i, j))
end

###############################################################################
#
#   Basic manipulation
#
###############################################################################

function Base.hash(a::Nemo.MatElem, h::UInt)
   b = 0x3e4ea81eb31d94f4%UInt
   for i in 1:rows(a)
      for j in 1:cols(a)
         b = xor(b, xor(hash(a[i, j], h), h))
         b = (b << 1) | (b >> (sizeof(Int)*8 - 1))
      end
   end
   return b
end

doc"""
    rows(a::Nemo.MatElem)
> Return the number of rows of the given matrix.
"""
rows(a::Nemo.MatElem) = size(a.entries, 1)

doc"""
    cols(a::Nemo.MatElem)
> Return the number of columns of the given matrix.
"""
cols(a::Nemo.MatElem) = size(a.entries, 2)

Base.@propagate_inbounds function getindex(a::Nemo.MatElem, r::Int, c::Int)
   return a.entries[r, c]
end

Base.@propagate_inbounds function setindex!(a::Nemo.MatElem, d::T, r::Int,
                                            c::Int) where T <: RingElement
    a.entries[r, c] = base_ring(a)(d)
end

doc"""
    zero(a::Nemo.MatSpace)
> Construct the zero matrix in the given matrix space.
"""
zero(a::Nemo.MatSpace) = a()

doc"""
    one(a::Nemo.MatSpace)
> Construct the matrix in the given matrix space with ones down the diagonal
> and zeroes elsewhere.
"""
one(a::Nemo.MatSpace) = a(1)

doc"""
    iszero(a::Nemo.MatElem)
> Return `true` if the supplied matrix $a$ is the zero matrix, otherwise
> return `false`.
"""
function iszero(a::Nemo.MatElem)
   for i = 1:rows(a)
      for j = 1:cols(a)
         if !iszero(a[i, j])
            return false
         end
      end
  end
  return true
end

doc"""
    isone(a::Nemo.MatElem)
> Return `true` if the supplied matrix $a$ is diagonal with ones along the
> diagonal, otherwise return `false`.
"""
function isone(a::Nemo.MatElem)
   for i = 1:rows(a)
      for j = 1:cols(a)
         if i == j
            if !isone(a[i, j])
               return false
            end
         else
            if !iszero(a[i, j])
               return false
            end
         end
      end
  end
  return true
end

function deepcopy_internal(d::Nemo.MatElem, dict::ObjectIdDict)
   c = similar(d)
   for i = 1:rows(d)
      for j = 1:cols(d)
         c[i, j] = deepcopy_internal(d[i, j], dict)
      end
   end
   return c
end

###############################################################################
#
#   Canonicalisation
#
###############################################################################

canonical_unit(a::Nemo.MatElem) = canonical_unit(a[1, 1])

###############################################################################
#
#   String I/O
#
###############################################################################

function show(io::IO, a::Nemo.MatSpace)
   print(io, "Matrix Space of ")
   print(io, a.rows, " rows and ", a.cols, " columns over ")
   print(io, base_ring(a))
end

function show(io::IO, a::Nemo.MatElem)
   r = rows(a)
   c = cols(a)
   for i = 1:r
      print(io, "[")
      for j = 1:c
         print(io, a[i, j])
         if j != c
            print(io, " ")
         end
      end
      print(io, "]")
      if i != r
         println(io, "")
      end
   end
end

show_minus_one(::Type{Nemo.MatElem{T}}) where {T <: RingElement} = false

###############################################################################
#
#   Unary operations
#
###############################################################################

doc"""
    -(a::Nemo.MatElem)
> Return $-a$.
"""
function -(x::Nemo.MatElem)
   z = similar(x)
   for i in 1:rows(x)
      for j in 1:cols(x)
         z[i, j] = -x[i, j]
      end
   end
   return z
end

###############################################################################
#
#   Binary operations
#
###############################################################################

doc"""
    +{T <: RingElement}(a::Nemo.MatElem{T}, b::Nemo.MatElem{T})
> Return $a + b$.
"""
function +(x::Nemo.MatElem{T}, y::Nemo.MatElem{T}) where {T <: RingElement}
   check_parent(x, y)
   r = similar(x)
   for i = 1:rows(x)
      for j = 1:cols(x)
         r[i, j] = x[i, j] + y[i, j]
      end
   end
   return r
end

doc"""
    -{T <: RingElement}(a::Nemo.MatElem{T}, b::Nemo.MatElem{T})
> Return $a - b$.
"""
function -(x::Nemo.MatElem{T}, y::Nemo.MatElem{T}) where {T <: RingElement}
   check_parent(x, y)
   r = similar(x)
   for i = 1:rows(x)
      for j = 1:cols(x)
         r[i, j] = x[i, j] - y[i, j]
      end
   end
   return r
end

doc"""
    *{T <: RingElement}(a::Nemo.MatElem{T}, b::Nemo.MatElem{T})
> Return $a\times b$.
"""
function *(x::Nemo.MatElem{T}, y::Nemo.MatElem{T}) where {T <: RingElement}
   cols(x) != rows(y) && error("Incompatible matrix dimensions")
   A = similar(x, rows(x), cols(y))
   C = base_ring(x)()
   for i = 1:rows(x)
      for j = 1:cols(y)
         A[i, j] = base_ring(x)()
         for k = 1:cols(x)
            C = mul!(C, x[i, k], y[k, j])
            A[i, j] = addeq!(A[i, j], C)
         end
      end
   end
   return A
end

###############################################################################
#
#   Ad hoc binary operators
#
###############################################################################

doc"""
    *(x::Union{Integer, Rational, AbstractFloat}, y::Nemo.MatElem)
> Return $x\times y$.
"""
function *(x::Union{Integer, Rational, AbstractFloat}, y::Nemo.MatElem)
   z = similar(y)
   for i = 1:rows(y)
      for j = 1:cols(y)
         z[i, j] = x*y[i, j]
      end
   end
   return z
end

doc"""
    *{T <: RingElem}(x::T, y::Nemo.MatElem{T})
> Return $x\times y$.
"""
function *(x::T, y::Nemo.MatElem{T}) where {T <: RingElem}
   z = similar(y)
   for i = 1:rows(y)
      for j = 1:cols(y)
         z[i, j] = x*y[i, j]
      end
   end
   return z
end

doc"""
    *(x::Nemo.MatElem, y::Union{Integer, Rational, AbstractFloat})
> Return $x\times y$.
"""
*(x::Nemo.MatElem, y::Union{Integer, Rational, AbstractFloat}) = y*x

doc"""
    *{T <: RingElem}(x::Nemo.MatElem{T}, y::T)
> Return $x\times y$.
"""
*(x::Nemo.MatElem{T}, y::T) where {T <: RingElem} = y*x

doc"""
    +(x::Union{Integer, Rational, AbstractFloat}, y::Nemo.MatElem)
> Return $S(x) + y$ where $S$ is the parent of $y$.
"""
function +(x::Union{Integer, Rational, AbstractFloat}, y::Nemo.MatElem)
   z = similar(y)
   R = base_ring(y)
   for i = 1:rows(y)
      for j = 1:cols(y)
         if i != j
            z[i, j] = deepcopy(y[i, j])
         else
            z[i, j] = y[i, j] + R(x)
         end
      end
   end
   return z
end

doc"""
    +(x::Nemo.MatElem, y::Union{Integer, Rational, AbstractFloat})
> Return $x + S(y)$ where $S$ is the parent of $x$.
"""
+(x::Nemo.MatElem, y::Union{Integer, Rational, AbstractFloat}) = y + x

doc"""
    +{T <: RingElem}(x::T, y::Nemo.MatElem{T})
> Return $S(x) + y$ where $S$ is the parent of $y$.
"""
function +(x::T, y::Nemo.MatElem{T}) where {T <: RingElem}
   z = similar(y)
   for i = 1:rows(y)
      for j = 1:cols(y)
         if i != j
            z[i, j] = deepcopy(y[i, j])
         else
            z[i, j] = y[i, j] + x
         end
      end
   end
   return z
end

doc"""
    +{T <: RingElem}(x::Nemo.MatElem{T}, y::T)
> Return $x + S(y)$ where $S$ is the parent of $x$.
"""
+(x::Nemo.MatElem{T}, y::T) where {T <: RingElem} = y + x

doc"""
    -(x::Union{Integer, Rational, AbstractFloat}, y::Nemo.MatElem)
> Return $S(x) - y$ where $S$ is the parent of $y$.
"""
function -(x::Union{Integer, Rational, AbstractFloat}, y::Nemo.MatElem)
   z = similar(y)
   R = base_ring(y)
   for i = 1:rows(y)
      for j = 1:cols(y)
         if i != j
            z[i, j] = -y[i, j]
         else
            z[i, j] = R(x) - y[i, j]
         end
      end
   end
   return z
end

doc"""
    -(x::Nemo.MatElem, y::Union{Integer, Rational, AbstractFloat})
> Return $x - S(y)$, where $S$ is the parent of $x$.
"""
function -(x::Nemo.MatElem, y::Union{Integer, Rational, AbstractFloat})
   z = similar(x)
   R = base_ring(x)
   for i = 1:rows(x)
      for j = 1:cols(x)
         if i != j
            z[i, j] = deepcopy(x[i, j])
         else
            z[i, j] = x[i, j] - R(y)
         end
      end
   end
   return z
end

doc"""
    -{T <: RingElem}(x::T, y::Nemo.MatElem{T})
> Return $S(x) - y$ where $S$ is the parent of $y$.
"""
function -(x::T, y::Nemo.MatElem{T}) where {T <: RingElem}
   z = similar(y)
   R = base_ring(y)
   for i = 1:rows(y)
      for j = 1:cols(y)
         if i != j
            z[i, j] = -y[i, j]
         else
            z[i, j] = x - y[i, j]
         end
      end
   end
   return z
end

doc"""
    -{T <: RingElem}(x::Nemo.MatElem{T}, y::T)
> Return $x - S(y)$, where $S$ is the parent of $a$.
"""
function -(x::Nemo.MatElem{T}, y::T) where {T <: RingElem}
   z = similar(x)
   R = base_ring(x)
   for i = 1:rows(x)
      for j = 1:cols(x)
         if i != j
            z[i, j] = deepcopy(x[i, j])
         else
            z[i, j] = x[i, j] - y
         end
      end
   end
   return z
end

###############################################################################
#
#   Powering
#
###############################################################################

doc"""
    ^(a::Nemo.MatElem, b::Int)
> Return $a^b$. We require $b \geq 0$ and that the matrix $a$ is square.
"""
function ^(a::Nemo.MatElem, b::Int)
   b < 0 && throw(DomainError())
   rows(a) != cols(a) && error("Incompatible matrix dimensions in power")
   # special case powers of x for constructing polynomials efficiently
   if b == 0
      return eye(a)
   elseif b == 1
      return deepcopy(a)
   else
      bit = ~((~UInt(0)) >> 1)
      while (UInt(bit) & b) == 0
         bit >>= 1
      end
      z = a
      bit >>= 1
      while bit != 0
         z = z*z
         if (UInt(bit) & b) != 0
            z *= a
         end
         bit >>= 1
      end
      return z
   end
end

doc"""
    powers{T <: RingElement}(a::Nemo.MatElem{T}, d::Int)
> Return an array of matrices $M$ wher $M[i + 1] = a^i$ for $i = 0..d$
"""
function powers(a::Nemo.MatElem, d::Int)
   rows(a) != cols(a) && error("Dimensions do not match in powers")
   d <= 0 && throw(DomainError())
   A = Array{typeof(a)}(d + 1)
   A[1] = eye(a)
   if d > 1
      c = a
      A[2] = a
      for i = 2:d
         c *= a
         A[i + 1] = c
      end
   end
   return A
end

###############################################################################
#
#   Comparisons
#
###############################################################################

doc"""
    =={T <: RingElement}(x::Nemo.MatElem{T}, y::Nemo.MatElem{T})
> Return `true` if $x == y$ arithmetically, otherwise return `false`. Recall
> that power series to different precisions may still be arithmetically
> equal to the minimum of the two precisions.
"""
function ==(x::Nemo.MatElem{T}, y::Nemo.MatElem{T}) where {T <: RingElement}
   check_parent(x, y)
   for i = 1:rows(x)
      for j = 1:cols(x)
         if x[i, j] != y[i, j]
            return false
         end
      end
   end
   return true
end

doc"""
    isequal{T <: RingElement}(x::Nemo.MatElem{T}, y::Nemo.MatElem{T})
> Return `true` if $x == y$ exactly, otherwise return `false`. This function is
> useful in cases where the entries of the matrices are inexact, e.g. power
> series. Only if the power series are precisely the same, to the same precision,
> are they declared equal by this function.
"""
function isequal(x::Nemo.MatElem{T}, y::Nemo.MatElem{T}) where {T <: RingElement}
   check_parent(x, y)
   for i = 1:rows(x)
      for j = 1:cols(x)
         if !isequal(x[i, j], y[i, j])
            return false
         end
      end
   end
   return true
end

###############################################################################
#
#   Ad hoc comparisons
#
###############################################################################

doc"""
    ==(x::Nemo.MatElem, y::Union{Integer, Rational, AbstractFloat})
> Return `true` if $x == S(y)$ arithmetically, where $S$ is the parent of $x$,
> otherwise return `false`.
"""
function ==(x::Nemo.MatElem, y::Union{Integer, Rational, AbstractFloat})
   for i = 1:min(rows(x), cols(x))
      if x[i, i] != y
         return false
      end
   end
   for i = 1:rows(x)
      for j = 1:cols(x)
         if i != j && !iszero(x[i, j])
            return false
         end
      end
   end
   return true
end

doc"""
    ==(x::Union{Integer, Rational, AbstractFloat}, y::Nemo.MatElem)
> Return `true` if $S(x) == y$ arithmetically, where $S$ is the parent of $y$,
> otherwise return `false`.
"""
==(x::Union{Integer, Rational, AbstractFloat}, y::Nemo.MatElem) = y == x

doc"""
    =={T <: RingElem}(x::Nemo.MatElem{T}, y::T)
> Return `true` if $x == S(y)$ arithmetically, where $S$ is the parent of $x$,
> otherwise return `false`.
"""
function ==(x::Nemo.MatElem{T}, y::T) where {T <: RingElem}
   for i = 1:min(rows(x), cols(x))
      if x[i, i] != y
         return false
      end
   end
   for i = 1:rows(x)
      for j = 1:cols(x)
         if i != j && !iszero(x[i, j])
            return false
         end
      end
   end
   return true
end

doc"""
    =={T <: RingElem}(x::T, y::Nemo.MatElem{T})
> Return `true` if $S(x) == y$ arithmetically, where $S$ is the parent of $y$,
> otherwise return `false`.
"""
==(x::T, y::Nemo.MatElem{T}) where {T <: RingElem} = y == x

###############################################################################
#
#   Ad hoc exact division
#
###############################################################################

doc"""
    divexact(x::Nemo.MatElem, y::Union{Integer, Rational, AbstractFloat})
> Return $x/y$, i.e. the matrix where each of the entries has been divided by
> $y$. Each division is expected to be exact.
"""
function divexact(x::Nemo.MatElem, y::Union{Integer, Rational, AbstractFloat})
   z = similar(x)
   for i = 1:rows(x)
      for j = 1:cols(x)
         z[i, j] = divexact(x[i, j], y)
      end
   end
   return z
end

doc"""
    divexact{T <: RingElem}(x::Nemo.MatElem{T}, y::T)
> Return $x/y$, i.e. the matrix where each of the entries has been divided by
> $y$. Each division is expected to be exact.
"""
function divexact(x::Nemo.MatElem{T}, y::T) where {T <: RingElem}
   z = similar(x)
   for i = 1:rows(x)
      for j = 1:cols(x)
         z[i, j] = divexact(x[i, j], y)
      end
   end
   return z
end

###############################################################################
#
#   Transpose
#
###############################################################################

doc"""
    transpose(x::Nemo.MatElem)
> Return the transpose of the given matrix.
"""
function transpose(x::Mat)
   return matrix(base_ring(x), permutedims(x.entries, [2, 1]))
end

###############################################################################
#
#   Gram matrix
#
###############################################################################

doc"""
    gram(x::Nemo.MatElem)
> Return the Gram matrix of $x$, i.e. if $x$ is an $r\times c$ matrix return
> the $r\times r$ matrix whose entries $i, j$ are the dot products of the
> $i$-th and $j$-th rows, respectively.
"""
function gram(x::Nemo.MatElem)
   z = similar(x, rows(x), rows(x))
   for i = 1:rows(x)
      for j = 1:rows(x)
         z[i, j] = zero(base_ring(x))
         for k = 1:cols(x)
            z[i, j] += x[i, k] * x[j, k]
         end
      end
   end
   return z
end

###############################################################################
#
#   Trace
#
###############################################################################

doc"""
    trace(x::Nemo.MatElem)
> Return the trace of the matrix $a$, i.e. the sum of the diagonal elements. We
> require the matrix to be square.
"""
function trace(x::Nemo.MatElem)
   rows(x) != cols(x) && error("Not a square matrix in trace")
   d = zero(base_ring(x))
   for i = 1:rows(x)
      d = addeq!(d, x[i, i])
   end
   return d
end

###############################################################################
#
#   Content
#
###############################################################################

doc"""
    content(x::Nemo.MatElem)
> Return the content of the matrix $a$, i.e. the greatest common divisor of all
> its entries, assuming it exists.
"""
function content(x::Nemo.MatElem)
  d = zero(base_ring(x))
  for i = 1:rows(x)
     for j = 1:cols(x)
        d = gcd(d, x[i, j])
        if isone(d)
           return d
        end
     end
  end
  return d
end

###############################################################################
#
#   Permutation
#
###############################################################################

doc"""
    *(P::Generic.perm, x::Nemo.MatElem)
> Apply the pemutation $P$ to the rows of the matrix $x$ and return the result.
"""
function *(P::Generic.perm, x::Nemo.MatElem)
   z = similar(x)
   m = rows(x)
   n = cols(x)
   for i = 1:m
      for j = 1:n
         z[P[i], j] = x[i, j]
      end
   end
   return z
end

###############################################################################
#
#   LU factorisation
#
###############################################################################

function lufact!(P::Generic.perm, A::Nemo.MatElem{T}) where {T <: FieldElement}
   m = rows(A)
   n = cols(A)
   rank = 0
   r = 1
   c = 1
   R = base_ring(A)
   t = R()
   while r <= m && c <= n
      if A[r, c] == 0
         i = r + 1
         while i <= m
            if !iszero(A[i, c])
               for j = 1:n
                  A[i, j], A[r, j] = A[r, j], A[i, j]
               end
               P[r], P[i] = P[i], P[r]
               break
            end
            i += 1
         end
         if i > m
            c += 1
            continue
         end
      end
      rank += 1
      d = -inv(A[r, c])
      for i = r + 1:m
         q = A[i, c]*d
         for j = c + 1:n
            t = mul!(t, A[r, j], q)
            A[i, j] = addeq!(A[i, j], t)
         end
         A[i, c] = R()
         A[i, rank] = -q
      end
      r += 1
      c += 1
   end
   inv!(P)
   return rank
end

doc"""
    lufact{T <: FieldElement}(A::Nemo.MatElem{T}, P = PermGroup(rows(A)))
> Return a tuple $r, p, L, U$ consisting of the rank of $A$, a permutation
> $p$ of $A$ belonging to $P$, a lower triangular matrix $L$ and an upper
> triangular matrix $U$ such that $p(A) = LU$, where $p(A)$ stands for the
> matrix whose rows are the given permutation $p$ of the rows of $A$.
"""
function lufact(A::Nemo.MatElem{T}, P = PermGroup(rows(A))) where {T <: FieldElement}
   m = rows(A)
   n = cols(A)
   P.n != m && error("Permutation does not match matrix")
   p = P()
   R = base_ring(A)
   U = deepcopy(A)
   L = similar(A, m, m)
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

function fflu!(P::Generic.perm, A::Nemo.MatElem{T}) where {T <: RingElement}
   m = rows(A)
   n = cols(A)
   rank = 0
   r = 1
   c = 1
   R = base_ring(A)
   d = R(1)
   d2 = R(1)
   if m == 0 || n == 0
      return 0, d
   end
   t = R()
   while r <= m && c <= n
      if A[r, c] == 0
         i = r + 1
         while i <= m
            if !iszero(A[i, c])
               for j = 1:n
                  A[i, j], A[r, j] = A[r, j], A[i, j]
               end
               P[r], P[i] = P[i], P[r]
               break
            end
            i += 1
         end
         if i > m
            c += 1
            continue
         end
      end
      rank += 1
      q = -A[r, c]
      for i = r + 1:m
         for j = c + 1:n
            A[i, j] = mul!(A[i, j], A[i, j], q)
            t = mul!(t, A[i, c], A[r, j])
            A[i, j] = addeq!(A[i, j], t)
            if r > 1
               A[i, j] = divexact(A[i, j], d)
            else
               A[i, j] = -A[i, j]
            end
         end
      end
      d = -A[r, c]
      d2 = A[r, c]
      r += 1
      c += 1
   end
   inv!(P)
   return rank, d2
end

function fflu!(P::Generic.perm, A::Nemo.MatElem{T}) where {T <: FieldElement}
   m = rows(A)
   n = cols(A)
   rank = 0
   r = 1
   c = 1
   R = base_ring(A)
   d = R(1)
   d2 = R(1)
   if m == 0 || n == 0
      return 0, d
   end
   t = R()
   while r <= m && c <= n
      if A[r, c] == 0
         i = r + 1
         while i <= m
            if !iszero(A[i, c])
               for j = 1:n
                  A[i, j], A[r, j] = A[r, j], A[i, j]
               end
               P[r], P[i] = P[i], P[r]
               break
            end
            i += 1
         end
         if i > m
            c += 1
            continue
         end
      end
      rank += 1
      q = -A[r, c]
      for i = r + 1:m
         for j = c + 1:n
            A[i, j] = mul!(A[i, j], A[i, j], q)
            t = mul!(t, A[i, c], A[r, j])
            A[i, j] = addeq!(A[i, j], t)
            if r > 1
               A[i, j] = mul!(A[i, j], A[i, j], d)
            else
               A[i, j] = -A[i, j]
            end
         end
      end
      d = -inv(A[r, c])
      d2 = A[r, c]
      r += 1
      c += 1
   end
   inv!(P)
   return rank, d2
end

doc"""
    fflu{T <: RingElement}(A::Nemo.MatElem{T}, P = PermGroup(rows(A)))
> Return a tuple $r, d, p, L, U$ consisting of the rank of $A$, a
> denominator $d$, a permutation $p$ of $A$ belonging to $P$, a lower
> triangular matrix $L$ and an upper triangular matrix $U$ such that
> $p(A) = LD^1U$, where $p(A)$ stands for the matrix whose rows are the given
> permutation $p$ of the rows of $A$ and such that $D$ is the diagonal matrix
> diag$(p_1, p_1p_2, \ldots, p_{n-2}p_{n-1}, p_{n-1}$ where the $p_i$ are the
> inverses of the diagonal entries of $U$. The denominator $d$ is set to
> $\pm \mbox{det}(S)$ where $S$ is an appropriate submatrix of $A$ ($S = A$ if
> $A$ is square) and the sign is decided by the parity of the permutation.
"""
function fflu(A::Nemo.MatElem{T}, P = PermGroup(rows(A))) where {T <: RingElement}
   m = rows(A)
   n = cols(A)
   P.n != m && error("Permutation does not match matrix")
   p = P()
   R = base_ring(A)
   U = deepcopy(A)
   L = similar(A, m, m)
   rank, d = fflu!(p, U)
   for i = 1:m
      for j = 1:n
         if i > j
            L[i, j] = U[i, j]
            U[i, j] = R()
         elseif i == j
            L[i, j] = U[i, j]
         elseif j <= m
            L[i, j] = R()
         end
      end
   end
   if m > 0
      L[m, m] = R(1)
   end
   return rank, d, p, L, U
end

###############################################################################
#
#   Reduced row-echelon form
#
###############################################################################

function rref!(A::Nemo.MatElem{T}) where {T <: RingElement}
   m = rows(A)
   n = cols(A)
   R = base_ring(A)
   P = PermGroup(m)()
   rank, d = fflu!(P, A)
   for i = rank + 1:m
      for j = 1:n
         A[i, j] = R()
      end
   end
   if rank > 1
      t = R()
      q = R()
      d = -d
      pivots = Array{Int}(n)
      np = rank
      j = k = 1
      for i = 1:rank
         while A[i, j] == 0
            pivots[np + k] = j
            j += 1
            k += 1
         end
         pivots[i] = j
         j += 1
      end
      while k <= n - rank
         pivots[np + k] = j
         j += 1
         k += 1
      end
      for k = 1:n - rank
         for i = rank - 1:-1:1
            t = mul!(t, A[i, pivots[np + k]], d)
            for j = i + 1:rank
               q = mul!(q, A[i, pivots[j]], A[j, pivots[np + k]])
               t = addeq!(t, q)
            end
            A[i, pivots[np + k]] = divexact(-t, A[i, pivots[i]])
         end
      end
      d = -d
      for i = 1:rank
         for j = 1:rank
            if i == j
               A[j, pivots[i]] = d
            else
               A[j, pivots[i]] = R()
            end
         end
      end
   end
   return rank, d
end

doc"""
    rref{T <: RingElement}(M::Nemo.MatElem{T})
> Returns a tuple $(r, d, A)$ consisting of the rank $r$ of $M$ and a
> denominator $d$ in the base ring of $M$ and a matrix $A$ such that $A/d$ is
> the reduced row echelon form of $M$. Note that the denominator is not usually
> minimal.
"""
function rref(M::Nemo.MatElem{T}) where {T <: RingElement}
   A = deepcopy(M)
   r, d = rref!(A)
   return r, d, A
end

function rref!(A::Nemo.MatElem{T}) where {T <: FieldElement}
   m = rows(A)
   n = cols(A)
   R = base_ring(A)
   P = PermGroup(m)()
   rnk = lufact!(P, A)
   if rnk == 0
      return 0
   end
   for i = 1:m
      for j = 1:min(rnk, i - 1)
         A[i, j] = R()
      end
   end
   U = similar(A, rnk, rnk)
   V = similar(A, rnk, n - rnk)
   pivots = Array{Int}(n)
   np = rnk
   j = k = 1
   for i = 1:rnk
      while A[i, j] == 0
         pivots[np + k] = j
         j += 1
         k += 1
      end
      pivots[i] = j
      j += 1
   end
   while k <= n - rnk
      pivots[np + k] = j
      j += 1
      k += 1
   end
   for i = 1:rnk
      for j = 1:i
         U[j, i] = A[j, pivots[i]]
      end
      for j = i + 1:rnk
         U[j, i] = R()
      end
   end
   for i = 1:n - rnk
      for j = 1:rnk
         V[j, i] = A[j, pivots[np + i]]
      end
   end
   V = solve_triu(U, V, false)
   for i = 1:rnk
      for j = 1:i
         A[j, pivots[i]] = i == j ? R(1) : R()
      end
   end
   for i = 1:n - rnk
      for j = 1:rnk
         A[j, pivots[np + i]] = V[j, i]
      end
   end
   return rnk
end

doc"""
    rref{T <: FieldElement}(M::Nemo.MatElem{T})
> Returns a tuple $(r, A)$ consisting of the rank $r$ of $M$ and a reduced row
> echelon form $A$ of $M$.
"""
function rref(M::Nemo.MatElem{T}) where {T <: FieldElement}
   A = deepcopy(M)
   r = rref!(A)
   return r, A
end

doc"""
    isrref{T <: RingElement}(M::Nemo.MatElem{T})
> Return `true` if $M$ is in reduced row echelon form, otherwise return
> `false`.
"""
function isrref(M::Nemo.MatElem{T}) where {T <: RingElement}
   m = rows(M)
   n = cols(M)
   c = 1
   for r = 1:m
      for i = 1:c - 1
         if !iszero(M[r, i])
            return false
         end
      end
      while c <= n && M[r, c] == 0
         c += 1
      end
      if c <= n
         for i = 1:r - 1
            if !iszero(M[i, c])
               return false
            end
         end
      end
   end
   return true
end

doc"""
    isrref(M::Nemo.MatElem{T}) where {T <: FieldElement}
> Return `true` if $M$ is in reduced row echelon form, otherwise return
> `false`.
"""
function isrref(M::Nemo.MatElem{T}) where {T <: FieldElement}
   m = rows(M)
   n = cols(M)
   c = 1
   for r = 1:m
      for i = 1:c - 1
         if !iszero(M[r, i])
            return false
         end
      end
      while c <= n && M[r, c] == 0
         c += 1
      end
      if c <= n
         if !isone(M[r, c])
            return false
         end
         for i = 1:r - 1
            if !iszero(M[i, c])
               return false
            end
         end
      end
   end
   return true
end

# Reduce row n of the matrix A, assuming the first n - 1 rows are in Gauss
# form. However those rows may not be in order. The i-th entry of the array
# P is the row of A which has a pivot in the i-th column. If no such row
# exists, the entry of P will be 0. The function returns the column in which
# the n-th row has a pivot after reduction. This will always be chosen to be
# the first available column for a pivot from the left. This information is
# also updated in P. The i-th entry of the array L contains the last column
# of A for which the row i is nonzero. This speeds up reduction in the case
# that A is chambered on the right. Otherwise the entries can all be set to
# the number of columns of A. The entries of L must be monotonic increasing.

function reduce_row!(A::Nemo.MatElem{T}, P::Array{Int}, L::Array{Int}, m::Int) where {T <: FieldElement}
   R = base_ring(A)
   n = cols(A)
   t = R()
   for i = 1:n
      if !iszero(A[m, i])
         h = -A[m, i]
         r = P[i]
         if r != 0
            A[m, i] = R()
            for j = i + 1:L[r]
               t = mul!(t, A[r, j], h)
               A[m, j] = addeq!(A[m, j], t)
            end
         else
            h = inv(A[m, i])
            A[m, i] = R(1)
            for j = i + 1:L[m]
               A[m, j] = mul!(A[m, j], A[m, j], h)
            end
            P[i] = m
            return i
         end
      end
   end
   return 0
end

function reduce_row!(A::Nemo.MatElem{T}, P::Array{Int}, L::Array{Int}, m::Int) where {T <: RingElement}
   R = base_ring(A)
   n = cols(A)
   t = R()
   c = R(1)
   c1 = 0
   for i = 1:n
      if !iszero(A[m, i])
         h = -A[m, i]
         r = P[i]
         if r != 0
            d = A[r, i]
            A[m, i] = R()
            for j = i + 1:L[r]
               t = mul!(t, A[r, j], h)
               A[m, j] = mul!(A[m, j], A[m, j], d)
               A[m, j] = addeq!(A[m, j], t)
            end
            for j = L[r] + 1:L[m]
               A[m, j] = mul!(A[m, j], A[m, j], d)
            end
            if c1 > 0 && P[c1] < P[i]
               for j = i + 1:L[m]
                  A[m, j] = divexact(A[m, j], c)
               end
            end
            c1 = i
            c = d
         else
            P[i] = m
            return i
         end
      else
         r = P[i]
         if r != 0
            for j = i + 1:L[m]
               A[m, j] = mul!(A[m, j], A[m, j], A[r, i])
            end
         end
      end
   end
   return 0
end

###############################################################################
#
#   Determinant
#
###############################################################################

function det_clow(M::Nemo.MatElem{T}) where {T <: RingElement}
   R = base_ring(M)
   n = rows(M)
   if n == 0
      return one(R)
   end
   A = Array{T}(n, n)
   B = Array{T}(n, n)
   C = R()
   for i = 1:n
      for j = 1:n
         A[i, j] = i == j ? R(1) : R(0)
         B[i, j] = R()
      end
   end
   for k = 1:n - 1
      for i = 1:n
         for j = 1:i
            if !iszero(A[i, j])
               for m = j + 1:n
                  C = mul!(C, A[i, j], M[i, m])
                  B[m, j] = addeq!(B[m, j], C)
               end
               for m = j + 1:n
                  C = mul!(C, A[i, j], M[i, j])
                  B[m, m] = addeq!(B[m, m], -C)
               end
            end
         end
      end
      Temp = A
      A = B
      B = Temp
      if k != n - 1
         for i = 1:n
            for j = 1:i
               B[i, j] = R()
            end
         end
      end
   end
   D = R()
   for i = 1:n
      for j = 1:i
         if !iszero(A[i, j])
            D -= A[i, j]*M[i, j]
         end
      end
   end
   return isodd(n) ? -D : D
end

function det_df(M::Nemo.MatElem{T}) where {T <: RingElement}
   R = base_ring(M)
   S, z = PolynomialRing(R, "z")
   n = rows(M)
   p = charpoly(S, M)
   d = coeff(p, 0)
   return isodd(n) ? -d : d
end

function det_fflu(M::Nemo.MatElem{T}) where {T <: RingElement}
   n = rows(M)
   if n == 0
      return base_ring(M)()
   end
   A = deepcopy(M)
   P = PermGroup(n)()
   r, d = fflu!(P, A)
   return r < n ? base_ring(M)() : (parity(P) == 0 ? d : -d)
end

doc"""
    det{T <: FieldElement}(M::Nemo.MatElem{T})
> Return the determinant of the matrix $M$. We assume $M$ is square.
"""
function det(M::Nemo.MatElem{T}) where {T <: FieldElement}
   rows(M) != cols(M) && error("Not a square matrix in det")
   if rows(M) == 0
      return one(base_ring(M))
   end
   return det_fflu(M)
end

doc"""
    det{T <: RingElement}(M::Nemo.MatElem{T})
> Return the determinant of the matrix $M$. We assume $M$ is square.
"""
function det(M::Nemo.MatElem{T}) where {T <: RingElement}
   rows(M) != cols(M) && error("Not a square matrix in det")
   if rows(M) == 0
      return one(base_ring(M))
   end
   try
      return det_fflu(M)
   catch
      return det_df(M)
   end
end

function det_interpolation(M::Nemo.MatElem{T}) where {T <: PolyElem}
   n = rows(M)
   !isdomain_type(elem_type(typeof(base_ring(base_ring(M))))) &&
          error("Generic interpolation requires a domain type")
   R = base_ring(M)
   if n == 0
      return R()
   end
   maxlen = 0
   for i = 1:n
      for j = 1:n
         maxlen = max(maxlen, length(M[i, j]))
      end
   end
   if maxlen == 0
      return R()
   end
   bound = n*(maxlen - 1) + 1
   x = Array{elem_type(base_ring(R))}(bound)
   d = Array{elem_type(base_ring(R))}(bound)
   X = MatrixSpace(base_ring(R), n, n)()
   b2 = div(bound, 2)
   pt1 = base_ring(R)(1 - b2)
   for i = 1:bound
      x[i] = base_ring(R)(i - b2)
      (x[i] == pt1 && i != 1) && error("Not enough interpolation points in ring")
      for j = 1:n
         for k = 1:n
            X[j, k] = evaluate(M[j, k], x[i])
         end
      end
      d[i] = det(X)
   end
   return interpolate(R, x, d)
end

function det(M::Nemo.MatElem{T}) where {S <: FinFieldElem, T <: PolyElem{S}}
   rows(M) != cols(M) && error("Not a square matrix in det")
   if rows(M) == 0
      return one(base_ring(M))
   end
   return det_popov(M)
end

function det(M::Nemo.MatElem{T}) where {T <: PolyElem}
   rows(M) != cols(M) && error("Not a square matrix in det")
   if rows(M) == 0
      return one(base_ring(M))
   end
   try
      return det_interpolation(M)
   catch
      # no point trying fflu, since it probably fails
      # for same reason as det_interpolation
      return det_df(M)
   end
end

###############################################################################
#
#   Rank
#
###############################################################################

doc"""
    rank{T <: RingElement}(M::Nemo.MatElem{T})
> Return the rank of the matrix $M$.
"""
function rank(M::Nemo.MatElem{T}) where {T <: RingElement}
   n = rows(M)
   if n == 0
      return 0
   end
   A = deepcopy(M)
   P = PermGroup(n)()
   r, d = fflu!(P, A)
   return r
end

doc"""
    rank{T <: FieldElement}(M::Nemo.MatElem{T})
> Return the rank of the matrix $M$.
"""
function rank(M::Nemo.MatElem{T}) where {T <: FieldElement}
   n = rows(M)
   if n == 0
      return 0
   end
   A = deepcopy(M)
   P = PermGroup(n)()
   return lufact!(P, A)
end

###############################################################################
#
#   Linear solving
#
###############################################################################

function solve_fflu(A::MatElem{T}, b::MatElem{T}) where {T <: RingElement}
   base_ring(A) != base_ring(b) && error("Base rings don't match in solve_fflu")
   rows(A) != cols(A) && error("Non-square matrix in solve_fflu")
   rows(A) != rows(b) && error("Dimensions don't match in solve_fflu")
   FFLU = deepcopy(A)
   p = PermGroup(rows(A))()
   r, d = fflu!(p, FFLU)
   r < rows(A) && error("Singular matrix in solve_fflu")
   return solve_fflu_precomp(p, FFLU, b), d
end

function solve_fflu_precomp(p::Generic.perm, FFLU::MatElem{T}, b::MatElem{T}) where {T <: RingElement}
   x = p * b
   n = rows(x)
   m = cols(x)
   R = base_ring(FFLU)

   t = base_ring(b)()
   s = base_ring(b)()
   minus_one = R(-1)

   for k in 1:m
      for i in 1:(n - 1)
         t = mul!(t, x[i, k], minus_one)
         for j in (i + 1):n
            if i == 1
              x[j, k] = x[j, k] * FFLU[i, i]
            else
              x[j, k] = mul!(x[j, k], x[j, k], FFLU[i, i])
            end
            s = mul!(s, FFLU[j, i], t)
            x[j, k] = addeq!(x[j, k], s)
            if i > 1
                x[j, k] = divexact(x[j, k], FFLU[i - 1, i - 1])
            end
         end
      end

      for i in (n - 1):-1:1
         if i > 1
            x[i, k] = mul!(x[i, k], x[i, k], FFLU[n, n])
         else
            x[i, k] = x[i, k] * FFLU[n, n]
         end
         for j in (i + 1):n
            t = mul!(t, x[j, k], FFLU[i, j])
            t = mul!(t, t, minus_one)
            x[i, k] = addeq!(x[i, k], t)
         end
         x[i, k] = divexact(x[i, k], FFLU[i, i])
      end
   end
   return x
end

function solve_lu(A::MatElem{T}, b::MatElem{T}) where {T <: FieldElement}
   base_ring(A) != base_ring(b) && error("Base rings don't match in solve_lu")
   rows(A) != cols(A) && error("Non-square matrix in solve_lu")
   rows(A) != rows(b) && error("Dimensions don't match in solve_lu")

   if rows(A) == 0 || cols(A) == 0
      return b
   end

   LU = deepcopy(A)
   p = PermGroup(rows(A))()
   r = lufact!(p, LU)
   r < rows(A) && error("Singular matrix in solve_lu")
   return solve_lu_precomp(p, LU, b)
end

function solve_lu_precomp(p::Generic.perm, LU::MatElem{T}, b::MatElem{T}) where {T <: FieldElement}
   x = p * b
   n = rows(x)
   m = cols(x)
   R = base_ring(LU)

   t = base_ring(b)()
   s = base_ring(b)()
   minus_one = R(-1)

   for k in 1:m
      x[1, k] = deepcopy(x[1, k])
      for i in 2:n
         for j in 1:(i - 1)
            # x[i, k] = x[i, k] - LU[i, j] * x[j, k]
            t = mul!(t, LU[i, j], x[j, k])
            t = mul!(t, t, minus_one)
            if j == 1
               x[i, k] = x[i, k] + t #LU[i, j] * x[j, k]
            else
               x[i, k] = addeq!(x[i, k], t)
            end
         end
      end

      # Now every entry of x is a proper copy, so we can change the entries
      # as much as we want.

      x[n, k] = divexact(x[n, k], LU[n, n])

      for i in (n - 1):-1:1
         for j in (i + 1):n
            #x[i, k] = x[i, k] - x[j, k] * LU[i, j]
            t = mul!(t, x[j, k], LU[i, j])
            t = mul!(t, t, minus_one)
            x[i, k] = addeq!(x[i, k], t)
         end
         x[i, k] = divexact(x[i, k], LU[i, i])
      end
   end
   return x
end


function backsolve!(A::Nemo.MatElem{T}, b::Nemo.MatElem{T}) where {T <: FieldElement}
   m = rows(A)
   h = cols(b)
   R = base_ring(A)
   t = R()
   for i = m:-1:1
      d = -inv(A[i, i])
      for k = 1:h
         b[i, k] = -b[i, k]
         for j = i + 1:m
            t = mul!(t, A[i, j], b[j, k])
            b[i, k] = addeq!(b[i, k], t)
         end
         b[i, k] = mul!(b[i, k], b[i, k], d)
      end
   end
end

function solve_ff(M::Nemo.MatElem{T}, b::Nemo.MatElem{T}) where {T <: FieldElement}
   base_ring(M) != base_ring(b) && error("Base rings don't match in solve")
   rows(M) != cols(M) && error("Non-square matrix in solve")
   rows(M) != rows(b) && error("Dimensions don't match in solve")
   m = rows(M)
   x, d = solve_fflu(M, b)
   for i in 1:rows(x)
      for j in 1:cols(x)
         x[i, j] = divexact(x[i, j], d)
      end
   end
   return x
end

function solve_with_det(M::Nemo.MatElem{T}, b::Nemo.MatElem{T}) where {T <: RingElement}
   # We cannot use solve_fflu directly, since it forgot about the (parity of
   # the) permutation.
   rows(M) != cols(M) && error("Non-square matrix")
   R = base_ring(M)
   FFLU = deepcopy(M)
   p = PermGroup(rows(M))()
   r, d = fflu!(p, FFLU)
   if r < rows(M)
      error("Singular matrix in solve_with_det")
   end
   x = solve_fflu_precomp(p, FFLU, b)
   # Now M*x = d*b, but d is only sign(P) * det(M)
   if parity(p) != 0
      minus_one = R(-1)
      for k in 1:cols(x)
         for i in 1:rows(x)
            # We are allowed to modify x in-place.
            x[i, k] = mul!(x[i, k], x[i, k], minus_one)
         end
      end
      d = mul!(d, d, minus_one)
   end
   return x, d
end

function solve_with_det(M::Nemo.MatElem{T}, b::Nemo.MatElem{T}) where {T <: PolyElem}
   x, d = solve_interpolation(M, b)
   return x, d
end

function solve_ff(M::Nemo.MatElem{T}, b::Nemo.MatElem{T}) where {T <: RingElement}
   m = rows(M)
   n = cols(M)
   if m == 0 || n == 0
      return b, base_ring(M)()
   end
   return solve_fflu(M, b)
end

function solve_interpolation(M::Nemo.MatElem{T}, b::Nemo.MatElem{T}) where {T <: PolyElem}
   m = rows(M)
   h = cols(b)
   if m == 0
      return b, base_ring(M)()
   end
   R = base_ring(M)
   maxlen = 0
   for i = 1:m
      for j = 1:m
         maxlen = max(maxlen, length(M[i, j]))
      end
   end
   maxlen == 0 && error("Matrix is singular in solve")
   maxlenb = 0
   for i = 1:m
      for j = 1:h
         maxlenb = max(maxlenb, length(b[i, j]))
      end
   end
   # bound from xd = (M*)b where d is the det
   bound = (maxlen - 1)*(m - 1) + max(maxlenb, maxlen)
   tmat = matrix(base_ring(R), 0, 0, elem_type(base_ring(R))[])
   V = Array{typeof(tmat)}(bound)
   d = Array{elem_type(base_ring(R))}(bound)
   y = Array{elem_type(base_ring(R))}(bound)
   bj = Array{elem_type(base_ring(R))}(bound)
   X = similar(tmat, m, m)
   Y = similar(tmat, m, h)
   x = similar(b)
   b2 = div(bound, 2)
   pt1 = base_ring(R)(1 - b2)
   l = 1
   i = 1
   while l <= bound
      y[l] = base_ring(R)(i - b2)
      (y[l] == pt1 && i != 1) && error("Not enough interpolation points in ring")
      for j = 1:m
         for k = 1:m
            X[j, k] = evaluate(M[j, k], y[l])
         end
         for k = 1:h
            Y[j, k] = evaluate(b[j, k], y[l])
         end
      end
      try
         V[l], d[l] = solve_with_det(X, Y)
         l = l + 1
      catch e
         if !(e isa ErrorException)
            rethrow(e)
         end
      end

      # We tested i values and for each of them it was not solvable.
      # Thus for i values the matrix X is singular.

      if i > bound
         error("Singular matrix in solve_interpolation")
      end

      i = i + 1
   end
   for k = 1:h
      for i = 1:m
         for j = 1:bound
            bj[j] = V[j][i, k]
         end
         x[i, k] = interpolate(R, y, bj)
      end
   end
   return x, interpolate(R, y, d)
end

doc"""
    solve{T <: FieldElement}(M::Nemo.MatElem{T}, b::Nemo.MatElem{T})
> Given a non-singular $n\times n$ matrix over a field and an $n\times m$
> matrix over the same field, return $x$ an
> $n\times m$ matrix $x$ such that $Ax = b$.
> If $A$ is singular an exception is raised.
"""
function solve(M::Nemo.MatElem{T}, b::Nemo.MatElem{T}) where {T <: FieldElement}
    return solve_ringelem(M, b)
end

doc"""
    solve_rational{T <: RingElement}(M::Nemo.MatElem{T}, b::Nemo.MatElem{T})
> Given a non-singular $n\times n$ matrix over a ring and an $n\times m$
> matrix over the same ring, return a tuple $x, d$ consisting of an
> $n\times m$ matrix $x$ and a denominator $d$ such that $Ax = db$. The
> denominator will be the determinant of $A$ up to sign. If $A$ is singular an
> exception is raised.
"""
function solve_rational(M::Nemo.MatElem{T}, b::Nemo.MatElem{T}) where T <: RingElement
   return solve_ringelem(M, b)
end

function solve_ringelem(M::Nemo.MatElem{T}, b::Nemo.MatElem{T}) where {T <: RingElement}
   base_ring(M) != base_ring(b) && error("Base rings don't match in solve")
   rows(M) != cols(M) && error("Non-square matrix in solve")
   rows(M) != rows(b) && error("Dimensions don't match in solve")
   return solve_ff(M, b)
end

function solve_rational(M::Nemo.MatElem{T}, b::Nemo.MatElem{T}) where {T <: PolyElem}
   base_ring(M) != base_ring(b) && error("Base rings don't match in solve")
   rows(M) != cols(M) && error("Non-square matrix in solve")
   rows(M) != rows(b) && error("Dimensions don't match in solve")
   try
      return solve_interpolation(M, b)
   catch e
      if !isa(e, ErrorException)
         rethrow(e)
      end
      return solve_ff(M, b)
   end
end

###############################################################################
#
#   Upper triangular solving
#
###############################################################################

doc"""
    solve_triu{T <: FieldElement}(U::Nemo.MatElem{T}, b::Nemo.MatElem{T}, unit=false)
> Given a non-singular $n\times n$ matrix over a field which is upper
> triangular, and an $n\times m$ matrix over the same field, return an
> $n\times m$ matrix $x$ such that $Ax = b$. If $A$ is singular an exception
> is raised. If unit is true then $U$ is assumed to have ones on its
> diagonal, and the diagonal will not be read.
"""
function solve_triu(U::Nemo.MatElem{T}, b::Nemo.MatElem{T}, unit::Bool = false) where {T <: FieldElement}
   n = rows(U)
   m = cols(b)
   R = base_ring(U)
   X = similar(b)
   Tinv = Array{elem_type(R)}(n)
   tmp = Array{elem_type(R)}(n)
   if unit == false
      for i = 1:n
         Tinv[i] = inv(U[i, i])
      end
   end
   t = R()
   for i = 1:m
      for j = 1:n
         tmp[j] = X[j, i]
      end
      for j = n:-1:1
         s = R()
         for k = j + 1:n
            t = mul!(t, U[j, k], tmp[k])
            s = addeq!(s, t)
         end
         s = b[j, i] - s
         if unit == false
            s = mul!(s, s, Tinv[j])
         end
         tmp[j] = s
      end
      for j = 1:n
         X[j, i] = tmp[j]
      end
   end
   return X
end

###############################################################################
#
#   Inverse
#
###############################################################################

doc"""
    inv{T <: RingElement}(M::Nemo.MatElem{T})
> Given a non-singular $n\times n$ matrix over a ring the tuple $X, d$
> consisting of an $n\times n$ matrix $X$ and a denominator $d$ such that
> $AX = dI_n$, where $I_n$ is the $n\times n$ identity matrix. The denominator
> will be the determinant of $A$ up to sign. If $A$ is singular an exception
> is raised.
"""
function inv(M::Nemo.MatElem{T}) where {T <: RingElement}
   cols(M) != rows(M) && error("Matrix not square in invert")
   n = cols(M)
   X = eye(M)
   A = deepcopy(M)
   X, d = solve_fflu(A, X)
   return X, d
end

doc"""
    inv{T <: FieldElement}(M::Nemo.MatElem{T})
> Given a non-singular $n\times n$ matrix over a field, return an
> $n\times n$ matrix $X$ such that $AX = I_n$ where $I_n$ is the $n\times n$
> identity matrix. If $A$ is singular an exception is raised.
"""
function inv(M::Nemo.MatElem{T}) where {T <: FieldElement}
   cols(M) != rows(M) && error("Matrix not square in invert")
   n = cols(M)
   X = eye(M)
   A = solve_lu(M, X)
   return A
end

###############################################################################
#
#   Nullspace
#
###############################################################################

doc"""
    nullspace{T <: RingElement}(M::Nemo.MatElem{T})
> Returns a tuple $(\nu, N)$ consisting of the nullity $\nu$ of $M$ and
> a basis $N$ (consisting of column vectors) for the right nullspace of $M$,
> i.e. such that $MN$ is the zero matrix. If $M$ is an $m\times n$ matrix
> $N$ will be an $n\times \nu$ matrix. Note that the nullspace is taken to be
> the vector space kernel over the fraction field of the base ring if the
> latter is not a field. In Nemo we use the name ``kernel'' for a function to
> compute an integral kernel.
"""
function nullspace(M::Nemo.MatElem{T}) where {T <: RingElement}
   n = cols(M)
   rank, d, A = rref(M)
   nullity = n - rank
   R = base_ring(M)
   U = similar(M, n, nullity)
   if rank == 0
      for i = 1:nullity
         U[i, i] = R(1)
      end
   elseif nullity != 0
      pivots = Array{Int}(rank)
      nonpivots = Array{Int}(nullity)
      j = k = 1
      for i = 1:rank
         while A[i, j] == 0
            nonpivots[k] = j
            j += 1
            k += 1
         end
         pivots[i] = j
         j += 1
      end
      while k <= nullity
         nonpivots[k] = j
         j += 1
         k += 1
      end
      d = -A[1, pivots[1]]
      for i = 1:nullity
         for j = 1:rank
            U[pivots[j], i] = A[j, nonpivots[i]]
         end
         U[nonpivots[i], i] = d
      end
   end
   return nullity, U
end

doc"""
    nullspace{T <: FieldElement}(M::Nemo.MatElem{T})
> Returns a tuple $(\nu, N)$ consisting of the nullity $\nu$ of $M$ and
> a basis $N$ (consisting of column vectors) for the right nullspace of $M$,
> i.e. such that $MN$ is the zero matrix. If $M$ is an $m\times n$ matrix
> $N$ will be an $n\times \nu$ matrix. Note that the nullspace is taken to be
> the vector space kernel over the fraction field of the base ring if the
> latter is not a field. In Nemo we use the name ``kernel'' for a function to
> compute an integral kernel.
"""
function nullspace(M::Nemo.MatElem{T}) where {T <: FieldElement}
   m = rows(M)
   n = cols(M)
   rank, A = rref(M)
   nullity = n - rank
   R = base_ring(M)
   X = similar(M, n, nullity)
   if rank == 0
      for i = 1:nullity
         X[i, i] = R(1)
      end
   elseif nullity != 0
      pivots = Array{Int}(max(m, n))
      np = rank
      j = k = 1
      for i = 1:rank
         while A[i, j] == 0
            pivots[np + k] = j
            j += 1
            k += 1
         end
         pivots[i] = j
         j += 1
      end
      while k <= nullity
         pivots[np + k] = j
         j += 1
         k += 1
      end
      for i = 1:nullity
         for j = 1:rank
            X[pivots[j], i] = -A[j, pivots[np + i]]
         end
         X[pivots[np + i], i] = R(1)
      end
   end
   return nullity, X
end

###############################################################################
#
#   Hessenberg form
#
###############################################################################

function hessenberg!(A::Nemo.MatElem{T}) where {T <: RingElement}
   rows(A) != cols(A) && error("Dimensions don't match in hessenberg")
   R = base_ring(A)
   n = rows(A)
   u = R()
   t = R()
   for m = 2:n - 1
      i = m + 1
      while i <= n && A[i, m - 1] == 0
         i += 1
      end
      if i != n + 1
         if !iszero(A[m, m - 1])
            i = m
         end
         h = -inv(A[i, m - 1])
         if i > m
            for j = m - 1:n
               A[i, j], A[m, j] = A[m, j], A[i, j]
            end
            for j = 1:n
               A[j, i], A[j, m] = A[j, m], A[j, i]
            end
         end
         for i = m + 1:n
            if !iszero(A[i, m - 1])
               u = mul!(u, A[i, m - 1], h)
               for j = m:n
                  t = mul!(t, u, A[m, j])
                  A[i, j] = addeq!(A[i, j], t)
               end
               u = -u
               for j = 1:n
                  t = mul!(t, u, A[j, i])
                  A[j, m] = addeq!(A[j, m], t)
               end
               A[i, m - 1] = R()
            end
         end
      end
   end
end

doc"""
    hessenberg(A::Nemo.MatElem{T}) where {T <: RingElement}
> Returns the Hessenberg form of $M$, i.e. an upper Hessenberg matrix
> which is similar to $M$. The upper Hessenberg form has nonzero entries
> above and on the diagonal and in the diagonal line immediately below the
> diagonal.
"""
function hessenberg(A::Nemo.MatElem{T}) where {T <: RingElement}
   rows(A) != cols(A) && error("Dimensions don't match in hessenberg")
   M = deepcopy(A)
   hessenberg!(M)
   return M
end

doc"""
    ishessenberg{T <: RingElement}(A::Nemo.MatElem{T})
> Returns `true` if $M$ is in Hessenberg form, otherwise returns `false`.
"""
function ishessenberg(A::Nemo.MatElem{T}) where {T <: RingElement}
   n = rows(A)
   for i = 3:n
      for j = 1:i - 2
         if !iszero(A[i, j])
            return false
         end
      end
   end
   return true
end

###############################################################################
#
#   Characteristic polynomial
#
###############################################################################

function charpoly_hessenberg!(S::Ring, A::Nemo.MatElem{T}) where {T <: RingElement}
   rows(A) != cols(A) && error("Dimensions don't match in charpoly")
   R = base_ring(A)
   base_ring(S) != base_ring(A) && error("Cannot coerce into polynomial ring")
   n = rows(A)
   if n == 0
      return S(1)
   end
   if n == 1
      return gen(S) - A[1, 1]
   end
   hessenberg!(A)
   P = Array{elem_type(S)}(n + 1)
   P[1] = S(1)
   x = gen(S)
   for m = 1:n
      P[m + 1] = (x - A[m, m])*P[m]
      t = R(1)
      for i = 1:m - 1
         t = mul!(t, t, A[m - i + 1, m - i])
         P[m + 1] -= t*A[m - i, m]*P[m - i]
      end
   end
   return P[n + 1]
end

function charpoly_danilevsky_ff!(S::Ring, A::Nemo.MatElem{T}) where {T <: RingElement}
   rows(A) != cols(A) && error("Dimensions don't match in charpoly")
   R = base_ring(A)
   base_ring(S) != base_ring(A) && error("Cannot coerce into polynomial ring")
   n = rows(A)
   if n == 0
      return S(1)
   end
   if n == 1
      return gen(S) - A[1, 1]
   end
   d = R(1)
   t = R()
   V = Array{T}(n)
   W = Array{T}(n)
   pol = S(1)
   i = 1
   while i < n
      h = A[n - i + 1, n - i]
      while h == 0
         k = 1
         while k < n - i && A[n - i + 1, n - i - k] == 0
            k += 1
         end
         if k == n - i
            b = S()
            fit!(b, i + 1)
            b = setcoeff!(b, i, R(1))
            for k = 1:i
               b = setcoeff!(b, k - 1, -A[n - i + 1, n - k + 1]*d)
            end
            pol *= b
            n -= i
            i = 1
            if n == 1
               pol *= (gen(S) - A[1, 1]*d)
               return pol
            end
         else
            for j = 1:n
               A[n - i - k, j], A[n - i, j] = A[n - i, j], A[n - i - k, j]
            end
            for j = 1:n - i + 1
               A[j, n - i - k], A[j, n - i] = A[j, n - i], A[j, n - i - k]
            end
         end
         h = A[n - i + 1, n - i]
      end
      for j = 1:n
         V[j] = -A[n - i + 1, j]
         W[j] = deepcopy(A[n - i + 1, j])
      end
      for j = 1:n - i
         for k = 1:n - i - 1
            t = mul!(t, A[j, n - i], V[k])
            A[j, k] = mul!(A[j, k], A[j, k], h)
            A[j, k] = addeq!(A[j, k], t)
         end
         for k = n - i + 1:n
            t = mul!(t, A[j, n - i], V[k])
            A[j, k] = mul!(A[j, k], A[j, k], h)
            A[j, k] = addeq!(A[j, k], t)
         end
      end
      for k = 1:n
         A[n - i + 1, k] = R()
      end
      for j = 1:n - i
         for k = 1:n - i - 1
            A[j, k] = mul!(A[j, k], A[j, k], d)
         end
         for k = n - i + 1:n
            A[j, k] = mul!(A[j, k], A[j, k], d)
         end
      end
      A[n - i + 1, n - i] = deepcopy(h)
      for j = 1:n - i - 1
         s = R()
         for k = 1:n - i
            t = mul!(t, A[k, j], W[k])
            s = addeq!(s, t)
         end
         A[n - i, j] = s
      end
      for j = n - i:n - 1
         s = R()
         for k = 1:n - i
            t = mul!(t, A[k, j], W[k])
            s = addeq!(s, t)
         end
         t = mul!(t, h, W[j + 1])
         s = addeq!(s, t)
         A[n - i, j] = s
      end
      s = R()
      for k = 1:n - i
         t = mul!(t, A[k, n], W[k])
         s = addeq!(s, t)
      end
      A[n - i, n] = s
      for k = 1:n
         A[n - i, k] = mul!(A[n - i, k], A[n - i, k], d)
      end
      d = inv(h)
      i += 1
   end
   b = S()
   fit!(b, n + 1)
   b = setcoeff!(b, n, R(1))
   for i = 1:n
      b = setcoeff!(b, i - 1, -A[1, n - i + 1]*d)
   end
   return pol*b
end

function charpoly_danilevsky!(S::Ring, A::Nemo.MatElem{T}) where {T <: RingElement}
   rows(A) != cols(A) && error("Dimensions don't match in charpoly")
   R = base_ring(A)
   base_ring(S) != base_ring(A) && error("Cannot coerce into polynomial ring")
   n = rows(A)
   if n == 0
      return S(1)
   end
   if n == 1
      return gen(S) - A[1, 1]
   end
   t = R()
   V = Array{T}(n)
   W = Array{T}(n)
   pol = S(1)
   i = 1
   while i < n
      h = A[n - i + 1, n - i]
      while h == 0
         k = 1
         while k < n - i && A[n - i + 1, n - i - k] == 0
            k += 1
         end
         if k == n - i
            b = S()
            fit!(b, i + 1)
            b = setcoeff!(b, i, R(1))
            for k = 1:i
               b = setcoeff!(b, k - 1, -A[n - i + 1, n - k + 1])
            end
            pol *= b
            n -= i
            i = 1
            if n == 1
               pol *= (gen(S) - A[1, 1])
               return pol
            end
         else
            for j = 1:n
               A[n - i - k, j], A[n - i, j] = A[n - i, j], A[n - i - k, j]
            end
            for j = 1:n - i + 1
               A[j, n - i - k], A[j, n - i] = A[j, n - i], A[j, n - i - k]
            end
         end
         h = A[n - i + 1, n - i]
      end
      h = -inv(h)
      for j = 1:n
         V[j] = A[n - i + 1, j]*h
         W[j] = deepcopy(A[n - i + 1, j])
      end
      h = -h
      for j = 1:n - i
         for k = 1:n - i - 1
            t = mul!(t, A[j, n - i], V[k])
            A[j, k] = addeq!(A[j, k], t)
         end
         for k = n - i + 1:n
            t = mul!(t, A[j, n - i], V[k])
            A[j, k] = addeq!(A[j, k], t)
         end
         A[j, n - i] = mul!(A[j, n - i], A[j, n - i], h)
      end
      for j = 1:n - i - 1
         s = R()
         for k = 1:n - i
            t = mul!(t, A[k, j], W[k])
            s = addeq!(s, t)
         end
         A[n - i, j] = s
      end
      for j = n - i:n - 1
         s = R()
         for k = 1:n - i
            t = mul!(t, A[k, j], W[k])
            s = addeq!(s, t)
         end
         s = addeq!(s, W[j + 1])
         A[n - i, j] = s
      end
      s = R()
      for k = 1:n - i
         t = mul!(t, A[k, n], W[k])
         s = addeq!(s, t)
      end
      A[n - i, n] = s
      i += 1
   end
   b = S()
   fit!(b, n + 1)
   b = setcoeff!(b, n, R(1))
   for i = 1:n
      b = setcoeff!(b, i - 1, -A[1, n - i + 1])
   end
   return pol*b
end

doc"""
    charpoly{T <: RingElement}(V::Ring, Y::Nemo.MatElem{T})
> Returns the characteristic polynomial $p$ of the matrix $M$. The
> polynomial ring $R$ of the resulting polynomial must be supplied
> and the matrix is assumed to be square.
"""
function charpoly(V::Ring, Y::Nemo.MatElem{T}) where {T <: RingElement}
   rows(Y) != cols(Y) && error("Dimensions don't match in charpoly")
   R = base_ring(Y)
   base_ring(V) != base_ring(Y) && error("Cannot coerce into polynomial ring")
   n = rows(Y)
   if n == 0
      return V(1)
   end
   F = Array{elem_type(R)}(n)
   A = Array{elem_type(R)}(n)
   M = Array{elem_type(R)}(n - 1, n)
   F[1] = -Y[1, 1]
   for i = 2:n
      F[i] = R()
      for j = 1:i
         M[1, j] = Y[j, i]
      end
      A[1] = Y[i, i]
      p = R()
      for j = 2:i - 1
         for k = 1:i
            s = R()
            for l = 1:i
               p = mul!(p, Y[k, l], M[j - 1, l])
               s = addeq!(s, p)
            end
            M[j, k] = s
         end
         A[j] = M[j, i]
      end
      s = R()
      for j = 1:i
         p = mul!(p, Y[i, j], M[i - 1, j])
         s = addeq!(s, p)
      end
      A[i] = s
      for j = 1:i
         s = -F[j]
         for k = 1:j - 1
            p = mul!(p, A[k], F[j - k])
            s = addeq!(s, p)
         end
         F[j] = -s - A[j]
     end
   end
   z = gen(V)
   f = z^n
   for i = 1:n
      f = setcoeff!(f, n - i, F[i])
   end
   return f
end

###############################################################################
#
#   Minimal polynomial
#
###############################################################################

# We let the rows of A be the Krylov sequence v, Mv, M^2v, ... where v
# is a standard basis vector. We find a minimal polynomial P_v by finding
# a linear relation amongst the rows (rows are added one at a time until
# a relation is found). We then append the rows from A to B and row reduce
# by all the rows already in B. We then look for a new standard basis vector
# v not in the span of the rows in B and repeat the above. The arrays P1 and
# P2 are because we can't efficiently swap rows. The i-th entry encodes the
# row of A (in the case of P1) or B (in the case of P2) which has a pivot in
# column i. The arrays L1 and L2 encode the number of columns that contain
# nonzero entries for each row of A and B respectively. These are required
# by the reduce_row! function. If charpoly_only is set to true, the function
# will return just the polynomial obtained from the first Krylov subspace.
# If the charpoly is the minpoly, this returned polynomial will be the
# charpoly iff it has degree n. Otherwise it is meaningless (but it is
# extremely fast to compute over some fields).

doc"""
    minpoly{T <: FieldElement}(S::Ring, M::Nemo.MatElem{T}, charpoly_only = false)
> Returns the minimal polynomial $p$ of the matrix $M$. The polynomial ring $R$
> of the resulting polynomial must be supplied and the matrix must be square.
"""
function minpoly(S::Ring, M::Nemo.MatElem{T}, charpoly_only::Bool = false) where {T <: FieldElement}
   rows(M) != cols(M) && error("Not a square matrix in minpoly")
   base_ring(S) != base_ring(M) && error("Unable to coerce polynomial")
   n = rows(M)
   if n == 0
      return S(1)
   end
   R = base_ring(M)
   p = S(1)
   A = similar(M, n + 1, 2n + 1)
   B = similar(M, n, n)
   L1 = [n + i for i in 1:n + 1]
   L2 = [n for i in 1:n]
   P2 = zeros(Int, n)
   P2[1] = 1
   c2 = 1
   r2 = 1
   first_poly = true
   while r2 <= n
      P1 = [0 for i in 1:2n + 1]
      v = similar(M, n, 1)
      for j = 1:n
         B[r2, j] = v[j, 1]
         A[1, j] = R()
      end
      P1[c2] = 1
      P2[c2] = r2
      v[c2, 1] = R(1)
      B[r2, c2] = v[c2, 1]
      A[1, c2] = R(1)
      A[1, n + 1] = R(1)
      indep = true
      r1 = 1
      c1 = 0
      while c1 <= n && r1 <= n
         r1 += 1
         r2 = indep ? r2 + 1 : r2
         v = M*v
         for j = 1:n
            A[r1, j] = deepcopy(v[j, 1])
         end
         for j = n + 1:n + r1 - 1
            A[r1, j] = R(0)
         end
         A[r1, n + r1] = R(1)
         c1 = reduce_row!(A, P1, L1, r1)
         if indep && r2 <= n && !first_poly
            for j = 1:n
               B[r2, j] = deepcopy(v[j, 1])
            end
            c = reduce_row!(B, P2, L2, r2)
            indep = c != 0
         end
      end
      if first_poly
         for j = 1:n
            P2[j] = P1[j]
         end
         r2 = r1
      end
      c = 0
      for j = c2 + 1:n
         if P2[j] == 0
            c = j
            break
         end
      end
      c2 = c
      b = S()
      fit!(b, r1)
      h = inv(A[r1, n + r1])
      for i = 1:r1
         b = setcoeff!(b, i - 1, A[r1, n + i]*h)
      end
      p = lcm(p, b)
      if charpoly_only == true
         return p
      end
      if first_poly && r2 <= n
         for j = 1:r1 - 1
            for k = 1:n
               B[j, k] = A[j, k]
            end
         end
      end
      first_poly = false
   end
   return p
end

doc"""
    minpoly{T <: RingElement}(S::Ring, M::Nemo.MatElem{T}, charpoly_only = false)
> Returns the minimal polynomial $p$ of the matrix $M$. The polynomial ring $R$
> of the resulting polynomial must be supplied and the matrix must be square.
"""
function minpoly(S::Ring, M::Nemo.MatElem{T}, charpoly_only::Bool = false) where {T <: RingElement}
   rows(M) != cols(M) && error("Not a square matrix in minpoly")
   base_ring(S) != base_ring(M) && error("Unable to coerce polynomial")
   n = rows(M)
   if n == 0
      return S(1)
   end
   R = base_ring(M)
   p = S(1)
   A = similar(M, n + 1, 2n + 1)
   B = similar(M, n, n)
   L1 = [n + i for i in 1:n + 1]
   L2 = [n for i in 1:n]
   P2 = zeros(Int, n)
   P2[1] = 1
   c2 = 1
   r2 = 1
   first_poly = true
   while r2 <= n
      P1 = [0 for i in 1:2n + 1]
      v = similar(M, n, 1)
      for j = 1:n
         B[r2, j] = v[j, 1]
         A[1, j] = R()
      end
      P1[c2] = 1
      P2[c2] = r2
      v[c2, 1] = R(1)
      B[r2, c2] = v[c2, 1]
      for s = 1:c2 - 1
         if P2[s] != 0
            B[r2, c2] *= B[P2[s], s]
         end
      end
      A[1, c2] = R(1)
      A[1, n + 1] = R(1)
      indep = true
      r1 = 1
      c1 = 0
      while c1 <= n && r1 <= n
         r1 += 1
         r2 = indep ? r2 + 1 : r2
         v = M*v
         for j = 1:n
            A[r1, j] = deepcopy(v[j, 1])
         end
         for j = n + 1:n + r1 - 1
            A[r1, j] = R(0)
         end
         A[r1, n + r1] = R(1)
         c1 = reduce_row!(A, P1, L1, r1)
         if indep && r2 <= n && !first_poly
            for j = 1:n
               B[r2, j] = deepcopy(v[j, 1])
            end
            c = reduce_row!(B, P2, L2, r2)
            indep = c != 0
         end
      end
      if first_poly
         for j = 1:n
            P2[j] = P1[j]
         end
         r2 = r1
      end
      c = 0
      for j = c2 + 1:n
         if P2[j] == 0
            c = j
            break
         end
      end
      c2 = c
      b = S()
      fit!(b, r1)
      for i = 1:r1
         b = setcoeff!(b, i - 1, A[r1, n + i])
      end
      b = reverse(b, r1)
      b = primpart(b)
      b = reverse(b, r1)
      p = lcm(p, b)
      if charpoly_only == true
         return divexact(p, canonical_unit(p))
      end
      if first_poly && r2 <= n
         for j = 1:r1 - 1
            for k = 1:n
               B[j, k] = A[j, k]
            end
         end
      end
      first_poly = false
   end
   return divexact(p, canonical_unit(p))
end

###############################################################################
#
#   Hermite Normal Form
#
###############################################################################

function hnf_cohen(A::MatElem{T}) where {T <: RingElement}
   H, U = hnf_cohen_with_trafo(A)
   return H
end

function hnf_cohen_with_trafo(A::MatElem{T}) where {T <: RingElement}
   H = deepcopy(A)
   m = rows(H)
   U = eye(A, m)
   hnf_cohen!(H, U)
   return H, U
end

function hnf_cohen!(H::MatElem{T}, U::MatElem{T}) where {T <: RingElement}
   m = rows(H)
   n = cols(H)
   l = min(m, n)
   k = 1
   t = base_ring(H)()
   t1 = base_ring(H)()
   t2 = base_ring(H)()
   for i = 1:l
      for j = k+1:m
         if iszero(H[j,i])
            continue
         end
         d, u, v = gcdx(H[k,i], H[j,i])
         a = divexact(H[k,i], d)
         b = -divexact(H[j,i], d)
         for c = i:n
            t = deepcopy(H[j,c])
            t1 = mul!(t1, a, H[j,c])
            t2 = mul!(t2, b, H[k,c])
            H[j,c] = add!(H[j,c], t1, t2)
            t1 = mul!(t1, u, H[k,c])
            t2 = mul!(t2, v, t)
            H[k,c] = add!(H[k,c], t1, t2)
         end
         for c = 1:m
            t = deepcopy(U[j,c])
            t1 = mul!(t1, a, U[j,c])
            t2 = mul!(t2, b, U[k,c])
            U[j,c] = add!(U[j,c], t1, t2)
            t1 = mul!(t1, u, U[k,c])
            t2 = mul!(t2, v, t)
            U[k,c] = add!(U[k,c], t1, t2)
         end
      end
      if iszero(H[k,i])
         continue
      end
      cu = canonical_unit(H[k,i])
      if cu != 1
         for c = i:n
            H[k,c] = divexact(H[k,c],cu)
        end
         for c = 1:m
            U[k,c] = divexact(U[k,c],cu)
         end
      end
      for j = 1:k-1
         q = -div(H[j,i], H[k, i])
         for c = i:n
            t = mul!(t, q, H[k,c])
            H[j,c] = addeq!(H[j,c], t)
         end
         for c = 1:m
            t = mul!(t, q, U[k,c])
            U[j,c] = addeq!(U[j,c], t)
         end
      end
      k += 1
   end
   return nothing
end

#  Hermite normal form via Kannan-Bachem for matrices of full column rank
#
#  This is the algorithm of Kannan, Bachem, "Polynomial algorithms for computing
#  the Smith and Hermite normal forms of an integer matrix", Siam J. Comput.,
#  Vol. 8, No. 4, pp. 499-507.

doc"""
    hnf_minors(A::Mat) -> Mat
> Compute the upper right row Hermite normal form of $A$ using the algorithm
> of Kannan-Bachem.
"""
function hnf_minors(A::MatElem{T}) where {T <: RingElement}
   H = deepcopy(A)
   _hnf_minors!(H, similar(A, 0, 0), Val{false})
   return H
end

doc"""
    hnf_minors_with_trafo(A::Mat) -> Mat, Mat
> Compute the upper right row Hermite normal form $H$ of $A$ and an invertible
> matrix $U$ with $UA = H$ using the algorithm of Kannan-Bachem.
"""
function hnf_minors_with_trafo(A::MatElem{T}) where {T <: RingElement}
   H = deepcopy(A)
   U = similar(A, rows(A), rows(A))
   _hnf_minors!(H, U, Val{true})
   return H, U
end

function _hnf_minors!(H::MatElem{T}, U::MatElem{T}, with_transform::Type{Val{S}} = Val{false}) where {T <: RingElement, S}
   m = rows(H)
   n = cols(H)

   l = m
   k = 0

   R = base_ring(H)

   with_trafo = with_transform == Val{true} ? true : false

   if with_trafo
      for i in 1:m
         for j in 1:m
            if j == i
               U[i, j] = one(R)
            else
               U[i, j] = zero(R)
            end
         end
      end
   end

   t = zero(R)
   b = zero(R)
   t2 = zero(R)
   r1d = zero(R)
   r2d = zero(R)
   q = zero(R)
   minus_one = base_ring(H)(-1)

   while k <= n - 1
      k = k + 1
      if k == 1 && !iszero(H[k, k])
         for j2 in 1:n
            H[k, j2] = deepcopy(H[k, j2])
         end
      end

      # We have a matrix of form ( * * * * )
      #                          ( 0 * * * )
      #                          ( 0 0 * * ) <- k - 1
      #                          ( * * * * ) <- k
      # We want to produce zeroes in the first k entries
      # of row k.

      for j in 1:k-1
         # Since we want to mutate the k-th row, first make a copy
         if j == 1
            for j2 in j:n
               H[k, j2] = deepcopy(H[k, j2])
            end
         end

         # Shortcuts
         if iszero(H[k, j])
            continue
         end

         di, q = divides(H[k, j], H[j, j])
         if di
            q = mul!(q, q, minus_one)
            for j2 in j:n
               t = mul!(t, q, H[j, j2])
               H[k, j2] = add!(H[k, j2], H[k, j2], t)
            end
            if with_trafo
               for j2 in 1:m
                  t = mul!(t, q, U[j, j2])
                  U[k, j2] = add!(U[k, j2], U[k, j2], t)
               end
            end
            continue
         end

         # Generic case
         d, u, v = gcdx(H[j, j], H[k, j])

         r1d = divexact(H[j, j], d)
         r2d = divexact(H[k, j], d)

         mul!(r2d, r2d, minus_one)
         for j2 in j:n
            b = mul!(b, u, H[j, j2])
            t2 = mul!(t2, v, H[k, j2])
            H[k, j2] = mul!(H[k, j2], H[k, j2], r1d)
            t = mul!(t, r2d, H[j, j2])
            H[k, j2] = add!(H[k, j2], H[k, j2], t)
            H[j, j2] = add!(H[j, j2], b, t2)
         end
         if with_trafo
            for j2 in 1:m
               b = mul!(b, u, U[j, j2])
               t2 = mul!(t2, v, U[k, j2])
               U[k, j2] = mul!(U[k, j2], U[k, j2], r1d)
               t = mul!(t, r2d, U[j, j2])
               U[k, j2] = add!(U[k, j2], U[k, j2], t)
               U[j, j2] = add!(U[j, j2], b, t2)
            end
         end
      end

      if iszero(H[k, k])
         swap_rows!(H, k, l)
         if with_trafo
            swap_rows!(U, k, l)
         end
         l = l - 1
         k = k - 1
         continue
      end

      u = canonical_unit(H[k, k])
      if !isone(u)
         u = R(inv(u))
         for j in k:n
            H[k, j] = mul!(H[k, j], H[k, j], u)
         end
         if with_trafo
            for j in 1:m
               U[k, j] = mul!(U[k, j], U[k, j], u)
            end
         end
      end

      for i in (k - 1):-1:1
         for j in (i + 1):k
            q = div(H[i, j], H[j, j])
            if iszero(q)
              continue
            end
            q = mul!(q, q, minus_one)
            for j2 in j:n
               t = mul!(t, q, H[j, j2])
               H[i, j2] = add!(H[i, j2], H[i, j2], t)
            end
            if with_trafo
               for j2 in 1:m
                  t = mul!(t, q, U[j, j2])
                  U[i, j2] = add!(U[i, j2], U[i, j2], t)
               end
            end
         end
      end
      l = m
   end

   # Now the matrix is of form
   # ( * * * )
   # ( 0 * * )
   # ( 0 0 * )
   # ( * * * ) <- n + 1
   # ( * * * )

   for k in (n + 1):m
      for j in 1:n
         if j == 1
            for j2 in 1:n
               H[k, j2] = deepcopy(H[k, j2])
            end
         end

         # We do the same shortcuts as above
         if iszero(H[k, j])
            continue
         end

         di, q = divides(H[k, j], H[j, j])
         if di
            q = mul!(q, q, minus_one)
            for j2 in j:n
               t = mul!(t, q, H[j, j2])
               H[k, j2] = add!(H[k, j2], H[k, j2], t)
            end
            if with_trafo
               for j2 in 1:m
                  t = mul!(t, q, U[j, j2])
                  U[k, j2] = add!(U[k, j2], U[k, j2], t)
               end
            end
            continue
         end

         d, u, v = gcdx(H[j, j], H[k, j])
         r1d = divexact(H[j, j], d)
         r2d = divexact(H[k, j], d)
         mul!(r2d, r2d, minus_one)
         for j2 in j:n
            b = mul!(b, u, H[j, j2])
            t2 = mul!(t2, v, H[k, j2])
            H[k, j2] = mul!(H[k, j2], H[k, j2], r1d)
            t = mul!(t, r2d, H[j, j2])
            H[k, j2] = add!(H[k, j2], H[k, j2], t)
            H[j, j2] = add!(H[j, j2], b, t2)
         end
         if with_trafo
            for j2 in 1:m
               b = mul!(b, u, U[j, j2])
               t2 = mul!(t2, v, U[k, j2])
               U[k, j2] = mul!(U[k, j2], U[k, j2], r1d)
               t = mul!(t, r2d, U[j, j2])
               U[k, j2] = add!(U[k, j2], U[k, j2], t)
               U[j, j2] = add!(U[j, j2], b, t2)
            end
         end
      end
      for i in n:-1:1
         for j in (i + 1):n
            q = div(H[i, j], H[j, j])
            if iszero(q)
              continue
            end
            q = mul!(q, q, minus_one)
            for j2 in j:n
               t = mul!(t, q, H[j, j2])
               H[i, j2] = add!(H[i, j2], H[i, j2], t)
            end
            if with_trafo
               for j2 in 1:m
                  t = mul!(t, q, U[j, j2])
                  U[i, j2] = add!(U[i, j2], U[i, j2], t)
               end
            end
         end
      end
   end
   return H
end

#  Hermite normal form for arbitrary matrices via a modification of the
#  Kannan-Bachem algorithm

function hnf_kb(A::MatElem{T}) where {T <: RingElement}
   return _hnf_kb(A, Val{false})
end

function hnf_kb_with_trafo(A::MatElem{T}) where {T <: RingElement}
   return _hnf_kb(A, Val{true})
end

function _hnf_kb(A, trafo::Type{Val{T}} = Val{false}) where T
   H = deepcopy(A)
   m = rows(H)
   if trafo == Val{true}
      U = eye(A, m)
      hnf_kb!(H, U, true)
      return H, U
   else
      U = similar(A, 0, 0)
      hnf_kb!(H, U, false)
      return H
   end
end

function kb_search_first_pivot(H, start_element::Int = 1)
   for r = start_element:rows(H)
      for c = start_element:cols(H)
         if !iszero(H[r,c])
            return r, c
         end
      end
   end
   return 0, 0
end

function kb_reduce_row!(H::MatElem{T}, U::MatElem{T}, pivot::Array{Int, 1}, c::Int, with_trafo::Bool) where {T <: RingElement}
   r = pivot[c]
   t = base_ring(H)()
   for i = c+1:cols(H)
      p = pivot[i]
      if p == 0
         continue
      end
      q = -div(H[r,i], H[p,i])
      for j = i:cols(H)
         t = mul!(t, q, H[p,j])
         H[r,j] = addeq!(H[r,j], t)
      end
      if with_trafo
         for j = 1:cols(U)
            t = mul!(t, q, U[p,j])
            U[r,j] = addeq!(U[r,j], t)
         end
      end
   end
   return nothing
end

function kb_reduce_column!(H::MatElem{T}, U::MatElem{T}, pivot::Array{Int, 1}, c::Int, with_trafo::Bool, start_element::Int = 1) where {T <: RingElement}
   r = pivot[c]
   t = base_ring(H)()
   for i = start_element:c-1
      p = pivot[i]
      if p == 0
         continue
      end
      q = -div(H[p,c],H[r,c])
      for j = c:cols(H)
         t = mul!(t, q, H[r,j])
         H[p,j] = addeq!(H[p,j], t)
      end
      if with_trafo
         for j = 1:cols(U)
            t = mul!(t, q, U[r,j])
            U[p,j] = addeq!(U[p,j], t)
         end
      end
   end
   return nothing
end

function kb_canonical_row!(H, U, r::Int, c::Int, with_trafo::Bool)
   cu = canonical_unit(H[r,c])
   if cu != 1
      for j = c:cols(H)
         H[r,j] = divexact(H[r,j],cu)
      end
      if with_trafo
         for j = 1:cols(U)
            U[r,j] = divexact(U[r,j],cu)
         end
      end
   end
   return nothing
end

function kb_sort_rows!(H::MatElem{T}, U::MatElem{T}, pivot::Array{Int, 1}, with_trafo::Bool, start_element::Int = 1) where {T <:RingElement}
   m = rows(H)
   n = cols(H)
   pivot2 = zeros(Int, m)
   for i = 1:n
      if pivot[i] == 0
         continue
      end
      pivot2[pivot[i]] = i
   end

   r1 = start_element
   for i = start_element:n
      r2 = pivot[i]
      if r2 == 0
         continue
      end
      if r1 != r2
         swap_rows!(H, r1, r2)
         with_trafo ? swap_rows!(U, r1, r2) : nothing
         p = pivot2[r1]
         pivot[i] = r1
         if p != 0
            pivot[p] = r2
         end
         pivot2[r1] = i
         pivot2[r2] = p
      end
      r1 += 1
      if r1 == m
         break
      end
   end
   return nothing
end

function hnf_kb!(H, U, with_trafo::Bool = false, start_element::Int = 1)
   m = rows(H)
   n = cols(H)
   pivot = zeros(Int, n)
   row1, col1 = kb_search_first_pivot(H, start_element)
   if row1 == 0
      return nothing
   end
   pivot[col1] = row1
   kb_canonical_row!(H, U, row1, col1, with_trafo)
   pivot_max = col1
   t = base_ring(H)()
   t1 = base_ring(H)()
   t2 = base_ring(H)()
   for i=row1:m-1
      new_pivot = false
      for j = start_element:pivot_max
         if iszero(H[i+1,j])
            continue
         end
         if pivot[j] == 0
            pivot[j] = i+1
            kb_reduce_row!(H, U, pivot, j, with_trafo)
            pivot_max = max(pivot_max, j)
            new_pivot = true
         else
            p = pivot[j]
            d, u, v = gcdx(H[p,j],H[i+1,j])
            a = divexact(H[p,j],d)
            b = -divexact(H[i+1,j],d)
            for c = j:n
               t = deepcopy(H[i+1,c])
               t1 = mul!(t1, a, H[i+1,c])
               t2 = mul!(t2, b, H[p,c])
               H[i+1,c] = add!(H[i+1,c], t1, t2)
               t1 = mul!(t1, u, H[p,c])
               t2 = mul!(t2, v, t)
               H[p,c] = add!(H[p,c], t1, t2)
            end
            if with_trafo
               for c = 1:m
                  t = deepcopy(U[i+1,c])
                  t1 = mul!(t1, a, U[i+1,c])
                  t2 = mul!(t2, b, U[p,c])
                  U[i+1,c] = add!(U[i+1,c], t1, t2)
                  t1 = mul!(t1, u, U[p,c])
                  t2 = mul!(t2, v, t)
                  U[p,c] = add!(U[p,c], t1, t2)
               end
            end
         end
         kb_canonical_row!(H, U, pivot[j], j, with_trafo)
         kb_reduce_column!(H, U, pivot, j, with_trafo, start_element)
         if new_pivot
            break
         end
      end
      if !new_pivot
         for c = pivot_max+1:n
            if !iszero(H[i+1,c])
               pivot[c] = i+1
               kb_canonical_row!(H, U, pivot[c], c, with_trafo)
               kb_reduce_column!(H, U, pivot, c, with_trafo, start_element)
               pivot_max = max(pivot_max, c)
               break
            end
         end
      end
   end
   kb_sort_rows!(H, U, pivot, with_trafo, start_element)
   return nothing
end

doc"""
    hnf{T <: RingElement}(A::Mat{T}) -> Mat{T}
> Return the upper right row Hermite normal form of $A$.
"""
function hnf(A::MatElem{T}) where {T <: RingElement}
  return hnf_kb(A)
end

doc"""
    hnf{T <: RingElement}(A::Mat{T}) -> Mat{T}, Mat{T}
> Return the upper right row Hermite normal form $H$ of $A$ together with
> invertible matrix $U$ such that $UA = H$.
"""
function hnf_with_trafo(A)
  return hnf_kb_with_trafo(A)
end

###############################################################################
#
#   Smith Normal Form
#
###############################################################################

function snf_kb(A::Mat{T}) where {T <: RingElement}
   return _snf_kb(A, Val{false})
end

function snf_kb_with_trafo(A::Mat{T}) where {T <: RingElement}
   return _snf_kb(A, Val{true})
end

function _snf_kb(A::Mat{T}, trafo::Type{Val{V}} = Val{false}) where {V, T <: RingElement}
   S = deepcopy(A)
   m = rows(S)
   n = cols(S)
   if trafo == Val{true}
      U = eye(A, m)
      K = eye(A, n)
      snf_kb!(S, U, K, true)
      return S, U, K
   else
      U = similar(A, 0, 0)
      K = U
      snf_kb!(S, U, K, false)
      return S
   end
end

function kb_clear_row!(S::Mat{T}, K::Mat{T}, i::Int, with_trafo::Bool) where {T <: RingElement}
   m = rows(S)
   n = cols(S)
   t = base_ring(S)()
   t1 = base_ring(S)()
   t2 = base_ring(S)()
   for j = i+1:n
      if iszero(S[i,j])
         continue
      end
      d, u, v = gcdx(S[i,i], S[i,j])
      a = divexact(S[i,i], d)
      b = -divexact(S[i,j], d)
      for r = i:m
         t = deepcopy(S[r,j])
         t1 = mul!(t1, a, S[r,j])
         t2 = mul!(t2, b, S[r,i])
         S[r,j] = add!(S[r,j], t1, t2)
         t1 = mul!(t1, u, S[r,i])
         t2 = mul!(t2, v, t)
         S[r,i] = add!(S[r,i], t1, t2)
      end
      if with_trafo
         for r = 1:n
            t = deepcopy(K[r,j])
            t1 = mul!(t1, a, K[r,j])
            t2 = mul!(t2, b, K[r,i])
            K[r,j] = add!(K[r,j], t1, t2)
            t1 = mul!(t1, u, K[r,i])
            t2 = mul!(t2, v, t)
            K[r,i] = add!(K[r,i], t1, t2)
         end
      end
   end
   return nothing
end

function snf_kb!(S::Mat{T}, U::Mat{T}, K::Mat{T}, with_trafo::Bool = false) where {T <: RingElement}
   m = rows(S)
   n = cols(S)
   l = min(m,n)
   i = 1
   t = base_ring(S)()
   t1 = base_ring(S)()
   t2 = base_ring(S)()
   while i<=l
      kb_clear_row!(S, K, i, with_trafo)
      hnf_kb!(S, U, with_trafo, i)
      c = i+1
      while c <= n && iszero(S[i, c])
         c+=1
      end
      if c != n+1
         continue
      end
      i+=1
   end
   for i = 1:l-1
      if iszero(S[i,i]) && iszero(S[i+1,i+1])
         continue
      end
      d, u, v = gcdx(S[i,i], S[i+1,i+1])
      if with_trafo
         q = -divexact(S[i+1,i+1], d)
         t1 = mul!(t1, q, v)
         for c = 1:m
            t = deepcopy(U[i,c])
            U[i,c] = addeq!(U[i,c], U[i+1,c])
            t2 = mul!(t2, t1, U[i+1,c])
            U[i+1,c] = addeq!(U[i+1,c], t2)
            t2 = mul!(t2, t1, t)
            U[i+1,c] = addeq!(U[i+1,c], t2)
         end
         q1 = -divexact(S[i+1,i+1], d)
         q2 = divexact(S[i,i], d)
         for r = 1:n
            t = deepcopy(K[r,i])
            t1 = mul!(t1, K[r,i], u)
            t2 = mul!(t2, K[r,i+1], v)
            K[r,i] = add!(K[r,i], t1, t2)
            t1 = mul!(t1, t, q1)
            t2 = mul!(t2, K[r,i+1], q2)
            K[r,i+1] = add!(K[r,i+1], t1, t2)
         end
      end
      S[i+1,i+1] = divexact(S[i,i]*S[i+1,i+1],d)
      S[i,i] = d
   end
   return nothing
end

function snf(a::Mat{T}) where {T <: RingElement}
  return snf_kb(a)
end

function snf_with_trafo(a::Mat{T}) where {T <: RingElement}
  return snf_kb_with_trafo(a)
end

################################################################################
#
#   Popov Form
#
################################################################################

doc"""
    weak_popov{T <: PolyElem}(A::Mat{T})
> Return the weak Popov form of $A$.
"""
function weak_popov(A::Mat{T}) where {T <: PolyElem}
   return _weak_popov(A, Val{false})
end

doc"""
    weak_popov_with_trafo{T <: PolyElem}(A::Mat{T})
> Compute a tuple $(P, U)$ where $P$ is the weak Popov form of $A$ and $U$
> is a transformation matrix so that $P = UA$.
"""
function weak_popov_with_trafo(A::Mat{T}) where {T <: PolyElem}
   return _weak_popov(A, Val{true})
end

function _weak_popov(A::Mat{T}, trafo::Type{Val{S}} = Val{false}) where {T <: PolyElem, S}
   P = deepcopy(A)
   m = rows(P)
   W = similar(A, 0, 0)
   if trafo == Val{true}
      U = eye(A, m)
      weak_popov!(P, W, U, false, true)
      return P, U
   else
      U = similar(A, 0, 0)
      weak_popov!(P, W, U, false, false)
      return P
   end
end

doc"""
    extended_weak_popov{T <: PolyElem}(A::Mat{T}, V::Mat{T})
> Compute the weak Popov form $P$ of $A$ by applying simple row transformations
> on $A$ and a vector $W$ by applying the same transformations on the vector $V$.
> Return the tuple $(P, W)$.
"""
function extended_weak_popov(A::Mat{T}, V::Mat{T}) where {T <: PolyElem}
   return _extended_weak_popov(A, V, Val{false})
end

doc"""
    extended_weak_popov_with_trafo{T <: PolyElem}(A::Mat{T}, V::Mat{T})
> Compute the weak Popov form $P$ of $A$ by applying simple row transformations
> on $A$, a vector $W$ by applying the same transformations on the vector $V$,
> and a transformation matrix $U$ so that $P = UA$.
> Return the tuple $(P, W, U)$.
"""
function extended_weak_popov_with_trafo(A::Mat{T}, V::Mat{T}) where {T <: PolyElem}
   return _extended_weak_popov(A, V, Val{true})
end

function _extended_weak_popov(A::Mat{T}, V::Mat{T}, trafo::Type{Val{S}} = Val{false}) where {T <: PolyElem, S}
   @assert rows(V) == rows(A) && cols(V) == 1
   P = deepcopy(A)
   W = deepcopy(V)
   m = rows(P)
   if trafo == Val{true}
      U = eye(A)
      weak_popov!(P, W, U, true, true)
      return P, W, U
   else
      U = similar(A, 0, 0)
      weak_popov!(P, W, U, true, false)
      return P, W
   end
end

function find_pivot_popov(P::Mat{T}, r::Int, last_col::Int = 0) where {T <: PolyElem}
   last_col == 0 ? n = cols(P) : n = last_col
   pivot = n
   for c = n-1:-1:1
      if degree(P[r,c]) > degree(P[r,pivot])
         pivot = c
      end
   end
   return pivot
end

function init_pivots_popov(P::Mat{T}, last_row::Int = 0, last_col::Int = 0) where {T <: PolyElem}
   last_row == 0 ? m = rows(P) : m = last_row
   last_col == 0 ? n = cols(P) : n = last_col
   pivots = Array{Array{Int,1}}(n)
   for i = 1:n
      pivots[i] = Array{Int}(0)
   end
   # pivots[i] contains the indices of the rows in which the pivot element is in the ith column.
   for r = 1:m
      pivot = find_pivot_popov(P, r, last_col)
      !iszero(P[r,pivot]) ? push!(pivots[pivot], r) : nothing
   end
   return pivots
end

function weak_popov!(P::Mat{T}, W::Mat{T}, U::Mat{T}, extended::Bool = false,
                                       with_trafo::Bool = false, last_row::Int = 0, last_col::Int = 0) where {T <: PolyElem}
   pivots = init_pivots_popov(P, last_row, last_col)
   weak_popov_with_pivots!(P, W, U, pivots, extended, with_trafo, last_row, last_col)
   return nothing
end

#=
The weak Popov form is defined by T. Mulders and A. Storjohann in
"On lattice reduction for polynomial matrices"
=#
function weak_popov_with_pivots!(P::Mat{T}, W::Mat{T}, U::Mat{T}, pivots::Array{Array{Int,1}},
                                                   extended::Bool = false, with_trafo::Bool = false, last_row::Int = 0, last_col::Int = 0) where {T <: PolyElem}
   last_row == 0 ? m = rows(P) : m = last_row
   last_col == 0 ? n = cols(P) : n = last_col
   @assert length(pivots) >= n

   t = base_ring(P)()
   change = true
   while change
      change = false
      for i = 1:n
         if length(pivots[i]) <= 1
            continue
         end
         change = true
         # Reduce with the pivot of minimal degree.
         # TODO: Remove the list comprehension as soon as indmin(f, A) works
         #pivotInd = indmin(degree(P[j, i]) for j in pivots[i])
         pivotInd = indmin([degree(P[j, i]) for j in pivots[i]])
         pivot = pivots[i][pivotInd]
         for j = 1:length(pivots[i])
            if j == pivotInd
               continue
            end
            q = -div(P[pivots[i][j],i], P[pivot,i])
            for c = 1:n
               t = mul!(t, q, P[pivot,c])
               P[pivots[i][j],c] = addeq!(P[pivots[i][j],c], t)
            end
            if with_trafo
               for c = 1:cols(U)
                  t = mul!(t, q, U[pivot,c])
                  U[pivots[i][j],c] = addeq!(U[pivots[i][j],c], t)
               end
            end
            if extended
               t = mul!(t, q, W[pivot,1])
               W[pivots[i][j],1] = addeq!(W[pivots[i][j],1], t)
            end
         end
         old_pivots = pivots[i]
         pivots[i] = [pivot]
         for j = 1:length(old_pivots)
            if j == pivotInd
               continue
            end
            p = find_pivot_popov(P, old_pivots[j], last_col)
            !iszero(P[old_pivots[j],p]) ? push!(pivots[p], old_pivots[j]) : nothing
         end
      end
   end
   return nothing
end

doc"""
    rank_profile_popov{T <: PolyElem}(A::Mat{T})
> Return an array of $r$ row indices such that these rows of $A$ are linearly
> independent, where $r$ is the rank of $A$.
"""
function rank_profile_popov(A::Mat{T}) where {T <: PolyElem}
   B = deepcopy(A)
   m = rows(A)
   n = cols(A)
   U = similar(A, 0, 0)
   V = U
   r = 0
   rank_profile = Array{Int,1}(0)
   pivots = Array{Array{Int,1}}(n)
   for i = 1:n
      pivots[i] = Array{Int}(0)
   end
   p = find_pivot_popov(B, 1)
   if !iszero(B[1,p])
      push!(pivots[p], 1)
      r = 1
      push!(rank_profile, 1)
   end
   for i = 2:m
      p = find_pivot_popov(B, i)
      !iszero(B[i,p]) ? push!(pivots[p], i) : nothing
      weak_popov_with_pivots!(B, V, U, pivots, false, false, i)
      s = 0
      for j = 1:n
         # length(pivots[j]) is either 1 or 0, since B is in weak
         # Popov form.
         s += length(pivots[j])
      end
      if s != r
         push!(rank_profile, i)
         r = s
      end
   end
   return rank_profile
end

function det_popov(A::Mat{T}) where {T <: PolyElem}
   rows(A) != cols(A) && error("Not a square matrix in det_popov.")
   B = deepcopy(A)
   n = cols(B)
   R = base_ring(B)
   det = one(R)
   U = similar(A, 0, 0)
   V = U
   t = R()
   pivots1 = init_pivots_popov(B)
   weak_popov_with_pivots!(B, V, U, pivots1, false, false)
   pivots = zeros(Int, n)
   diag_elems = zeros(Int, n)
   for i = 1:n
      try pivots[i] = pivots1[i][1]
      catch
         # If there is no pivot in the ith column, A has not full rank.
         return R(0)
      end
   end
   for i = n-1:-1:1
      # "Remove" the column i+1 and compute a weak Popov Form of the
      # remaining matrix.
      r1 = pivots[i+1]
      c = find_pivot_popov(B, r1, i)
      # If the pivot B[r1,c] is zero then the row is zero.
      while !iszero(B[r1,c])
         r2 = pivots[c]
         if degree(B[r2,c]) > degree(B[r1,c])
            r1, r2 = r2, r1
            pivots[c] = r2
         end
         q = -div(B[r1,c],B[r2,c])
         for j = 1:i+1
            t = mul!(t, q, B[r2,j])
            B[r1,j] = addeq!(B[r1,j], t)
         end
         c = find_pivot_popov(B, r1, i)
      end
      if iszero(B[r1, i+1])
         return R(0)
      end
      diag_elems[i+1] = r1
      det = mul!(det, det, B[r1,i+1])
   end
   det = mul!(det, det, B[pivots[1],1])
   diag_elems[1] = pivots[1]
   number_of_swaps = 0
   # Adjust the sign of det by sorting the diagonal elements.
   for i = 1:n
      while diag_elems[i] != i
         r = diag_elems[i]
         diag_elems[i] = diag_elems[r]
         diag_elems[r] = r
         number_of_swaps += 1
      end
   end
   if number_of_swaps%2 == 1
      det = mul!(det, det, R(-1))
   end
   return det
end

doc"""
    popov{T <: PolyElem}(A::Mat{T})
> Return the Popov form of $A$.
"""
function popov(A::Mat{T}) where {T <: PolyElem}
   return _popov(A, Val{false})
end

doc"""
    popov_with_trafo{T <: PolyElem}(A::Mat{T})
> Compute a tuple $(P, U)$ where $P$ is the Popov form of $A$ and $U$
> is a transformation matrix so that $P = UA$.
"""
function popov_with_trafo(A::Mat{T}) where {T <: PolyElem}
   return _popov(A, Val{true})
end

function _popov(A::Mat{T}, trafo::Type{Val{S}} = Val{false}) where {T <: PolyElem, S}
   P = deepcopy(A)
   m = rows(P)
   if trafo == Val{true}
      U = eye(A, m)
      popov!(P, U, true)
      return P, U
   else
      U = similar(A, 0, 0)
      popov!(P, U, false)
      return P
   end
end

function asc_order_popov!(P::Mat{T}, U::Mat{T}, pivots::Array{Array{Int,1}}, with_trafo::Bool) where {T <: PolyElem}
   m = rows(P)
   n = cols(P)
   pivots2 = Array{NTuple{3,Int},1}(m)
   for r = 1:m
      pivots2[r] = (r,n,-1)
   end
   for c = 1:n
      if length(pivots[c]) == 0
         continue
      end
      r = pivots[c][1]
      pivots2[r] = (r, c, degree(P[r,c]))
   end
   sort!(pivots2, lt = (x,y) -> ( x[3] < y[3] || ( x[3] == y[3] && x[2] <= y[2] ) ))
   row_nums = [ i for i = 1:m ]
   for r = 1:m
      if pivots2[r][3] != -1
         c = pivots2[r][2]
         pivots[c] = [r]
      end
      i = pivots2[r][1]
      r2 = row_nums[i]
      if r == r2
         continue
      end
      swap_rows!(P, r, r2)
      with_trafo ? swap_rows!(U, r, r2) : nothing
      j = findfirst(row_nums, r)
      row_nums[i] = r
      row_nums[j] = r2
   end
   return nothing
end

function popov!(P::Mat{T}, U::Mat{T}, with_trafo::Bool = false) where {T <: PolyElem}
   m = rows(P)
   n = cols(P)
   W = similar(U, 0, 0)
   pivots = init_pivots_popov(P)
   weak_popov_with_pivots!(P, W, U, pivots, false, with_trafo)
   asc_order_popov!(P, U, pivots, with_trafo)
   t = base_ring(P)()
   for i = 1:n
      if length(pivots[i]) == 0
         continue
      end
      pivot = pivots[i][1]
      d = degree(P[pivot,i])
      for r = 1:pivot-1
         if degree(P[r,i]) < d
            continue
         end
         q = -div(P[r,i],P[pivot,i])
         for c = 1:n
            t = mul!(t, q, P[pivot,c])
            P[r,c] = addeq!(P[r,c], t)
         end
         if with_trafo
            for c = 1:cols(U)
               t = mul!(t, q, U[pivot,c])
               U[r,c] = addeq!(U[r,c], t)
            end
         end
      end
   end
   for i = 1:n
      if length(pivots[i]) == 0
         continue
      end
      r = pivots[i][1]
      cu = canonical_unit(P[r,i])
      if cu != 1
         for j = 1:n
            P[r,j] = divexact(P[r,j],cu)
         end
         if with_trafo
            for j = 1:cols(U)
               U[r,j] = divexact(U[r,j],cu)
            end
         end
      end
   end
   return nothing
end

function hnf_via_popov(A::Mat{T}) where {T <: PolyElem}
   return _hnf_via_popov(A, Val{false})
end

function hnf_via_popov_with_trafo(A::Mat{T}) where {T <: PolyElem}
   return _hnf_via_popov(A, Val{true})
end

function _hnf_via_popov(A::Mat{T}, trafo::Type{Val{S}} = Val{false}) where {T <: PolyElem, S}
   H = deepcopy(A)
   m = rows(H)
   if trafo == Val{true}
      U = eye(A, m)
      hnf_via_popov!(H, U, true)
      return H, U
   else
      U = similar(A, 0, 0)
      hnf_via_popov!(H, U, false)
      return H
   end
end

function hnf_via_popov_reduce_row!(H::Mat{T}, U::Mat{T}, pivots_hermite::Array{Int}, r::Int, with_trafo::Bool) where {T <: PolyElem}
   n = cols(H)
   t = base_ring(H)()
   for c = 1:n
      if pivots_hermite[c] == 0
         continue
      end
      pivot = pivots_hermite[c]
      q = -div(H[r,c],H[pivot,c])
      for j = c:n
         t = mul!(t, q, H[pivot,j])
         H[r,j] = addeq!(H[r,j], t)
      end
      if with_trafo
         for j = 1:cols(U)
            t = mul!(t, q, U[pivot,j])
            U[r,j] = addeq!(U[r,j], t)
         end
      end
   end
   return nothing
end

function hnf_via_popov_reduce_column!(H::Mat{T}, U::Mat{T}, pivots_hermite::Array{Int}, c::Int, with_trafo::Bool) where {T <: PolyElem}
   m = rows(H)
   n = cols(H)
   t = base_ring(H)()
   r = pivots_hermite[c]
   for i = 1:m
      if i == r
         continue
      end
      if degree(H[i,c]) < degree(H[r,c])
         continue
      end
      q = -div(H[i,c],H[r,c])
      for j = 1:n
         t = mul!(t, q, H[r,j])
         H[i,j] = addeq!(H[i,j], t)
      end
      if with_trafo
         for j = 1:cols(U)
            t = mul!(t, q, U[r,j])
            U[i,j] = addeq!(U[i,j], t)
         end
      end
   end
   return nothing
end

function hnf_via_popov!(H::Mat{T}, U::Mat{T}, with_trafo::Bool = false) where {T <: PolyElem}
   m = rows(H)
   n = cols(H)
   R = base_ring(H)
   W = similar(H, 0, 0)
   t = R()
   pivots = init_pivots_popov(H)
   weak_popov_with_pivots!(H, W, U, pivots, false, with_trafo)
   pivots_popov = zeros(Int, n)
   for j = 1:n
      try pivots_popov[j] = pivots[j][1] end
   end
   pivots_hermite = zeros(Int, n)
   for i = n-1:-1:1
      # "Remove" the column i+1 and compute a weak Popov Form of the
      # remaining matrix.
      r1 = pivots_popov[i+1]
      if r1 == 0
         continue
      end
      c = find_pivot_popov(H, r1, i)
      new_pivot = true
      # If the pivot H[r1,c] is zero then the row is zero.
      while !iszero(H[r1,c])
         r2 = pivots_popov[c]
         if r2 == 0
            pivots_popov[c] = r1
            new_pivot = false
            break
         end
         if degree(H[r2,c]) > degree(H[r1,c])
            r1, r2 = r2, r1
            pivots_popov[c] = r2
         end
         q = -div(H[r1,c],H[r2,c])
         for j = 1:n
            t = mul!(t, q, H[r2,j])
            H[r1,j] = addeq!(H[r1,j], t)
         end
         if with_trafo
            for j = 1:cols(U)
               t = mul!(t, q, U[r2,j])
               U[r1,j] = addeq!(U[r1,j], t)
            end
         end
         hnf_via_popov_reduce_row!(H, U, pivots_hermite, r1, with_trafo)
         c = find_pivot_popov(H, r1, i)
      end
      new_pivot ? nothing : continue
      pivots_hermite[i+1] = r1
      hnf_via_popov_reduce_column!(H, U, pivots_hermite, i+1, with_trafo)
      l = pivots_popov[i]
      hnf_via_popov_reduce_row!(H, U, pivots_hermite, l, with_trafo)
   end
   pivots_hermite[1] = pivots_popov[1]
   kb_sort_rows!(H, U, pivots_hermite, with_trafo)
   for c = 1:n
      if pivots_hermite[c] == 0
         continue
      end
      kb_canonical_row!(H, U, pivots_hermite[c], c, with_trafo)
   end
   return nothing
end

###############################################################################
#
#   Transforms
#
###############################################################################

doc"""
    similarity!{T <: RingElement}(A::Nemo.MatElem{T}, r::Int, d::T)
> Applies a similarity transform to the $n\times n$ matrix $M$ in-place. Let
> $P$ be the $n\times n$ identity matrix that has had all zero entries of row
> $r$ replaced with $d$, then the transform applied is equivalent to
> $M = P^{-1}MP$. We require $M$ to be a square matrix. A similarity transform
> preserves the minimal and characteristic polynomials of a matrix.
"""
function similarity!(A::Nemo.MatElem{T}, r::Int, d::T) where {T <: RingElement}
   n = rows(A)
   t = base_ring(A)()
   for i = 1:n
      for j = 1:r - 1
         t = mul!(t, A[i, r], d)
         A[i, j] = addeq!(A[i, j], t)
      end
      for j = r + 1:n
         t = mul!(t, A[i, r], d)
         A[i, j] = addeq!(A[i, j], t)
      end
   end
   d = -d
   for i = 1:n
      for j = 1:r - 1
         t = mul!(t, A[j, i], d)
         A[r, i] = addeq!(A[r, i], t)
      end
      for j = r + 1:n
         t = mul!(t, A[j, i], d)
         A[r, i] = addeq!(A[r, i], t)
      end
   end
end

###############################################################################
#
#   Row swapping
#
###############################################################################

doc"""
    swap_rows(a::Nemo.MatElem, i::Int, j::Int)
> Return a matrix $b$ with the entries of $a$, where the $i$th and $j$th
> row are swapped.
"""
function swap_rows(a::Nemo.MatElem, i::Int, j::Int)
   (1<=i<=rows(a) && 1<=j<=rows(a)) || throw(BoundsError())
   b = deepcopy(a)
   swap_rows!(b, i, j)
   return b
end

doc"""
    swap_rows!(a::Nemo.MatElem, i::Int, j::Int)
> Swap the $i$th and $j$th row of $a$.
"""
function swap_rows!(a::Nemo.MatElem, i::Int, j::Int)
   (1<=i<=rows(a) && 1<=j<=rows(a)) || throw(BoundsError())
   for k=1:cols(a)
      x = a[i,k]
      a[i,k] = a[j,k]
      a[j,k] = x
   end
end

###############################################################################
#
#   Concatenation
#
###############################################################################

doc"""
    hcat(a::Nemo.MatElem, b::Nemo.MatElem)
> Return the horizontal concatenation of $a$ and $b$. Assumes that the
> number of rows is the same in $a$ and $b$.
"""
function hcat(a::Nemo.MatElem, b::Nemo.MatElem)
   rows(a) != rows(b) && error("Incompatible number of rows in hcat")
   c = similar(a, rows(a), cols(a) + cols(b))
   n = cols(a)
   for i = 1:rows(a)
      for j = 1:cols(a)
         c[i, j] = a[i, j]
      end
      for j = 1:cols(b)
         c[i, n + j] = b[i, j]
      end
   end
   return c
end

doc"""
    vcat(a::Nemo.MatElem, b::Nemo.MatElem)
> Return the vertical concatenation of $a$ and $b$. Assumes that the
> number of columns is the same in $a$ and $b$.
"""
function vcat(a::Nemo.MatElem, b::Nemo.MatElem)
   cols(a) != cols(b) && error("Incompatible number of columns in vcat")
   c = similar(a, rows(a) + rows(b), cols(a))
   n = rows(a)
   for i = 1:rows(a)
      for j = 1:cols(a)
         c[i, j] = a[i, j]
      end
   end
   for i = 1:rows(b)
      for j = 1:cols(a)
         c[n + i, j] = b[i, j]
      end
   end
   return c
end

###############################################################################
#
#   Random generation
#
###############################################################################

function rand(S::Nemo.MatSpace, v...)
   M = S()
   R = base_ring(S)
   for i = 1:rows(M)
      for j = 1:cols(M)
         M[i, j] = rand(R, v...)
      end
   end
   return M
end

function randmat_triu(S::Nemo.MatSpace, v...)
   M = S()
   R = base_ring(S)
   for i = 1:rows(M)
      for j = 1:i - 1
         M[i, j] = R()
      end
      for j = i:cols(M)
         M[i, j] = rand(R, v...)
      end
      while M[i, i] == 0
         M[i, i] = rand(R, v...)
      end
   end
   return M
end

function randmat_with_rank(S::Generic.MatSpace{T}, rank::Int, v...) where {T <: Nemo.RingElement}
   M = S()
   R = base_ring(S)
   for i = 1:rank
      for j = 1:i - 1
         M[i, j] = R()
      end
      M[i, i] = rand(R, v...)
      while M[i, i] == 0
         M[i, i] = rand(R, v...)
      end
      for j = i + 1:cols(M)
         M[i, j] = rand(R, v...)
      end
   end
   for i = rank + 1:rows(M)
      for j = 1:cols(M)
         M[i, j] = R()
      end
   end
   m = rows(M)
   if m > 1
      for i = 1:4*m
         r1 = rand(1:m)
         r2 = rand(1:m - 1)
         r2 = r2 >= r1 ? r2 + 1 : r2
         d = rand(-5:5)
         for j = 1:cols(M)
            M[r1, j] = M[r1, j] + d*M[r2, j]
         end
      end
   end
   return M
end

###############################################################################
#
#   Promotion rules
#
###############################################################################

promote_rule(::Type{Mat{T}}, ::Type{Mat{T}}) where T <: RingElement = Mat{T}

function promote_rule(::Type{Mat{T}}, ::Type{U}) where {T <: RingElement, U <: RingElement}
   promote_rule(T, U) == T ? Mat{T} : Union{}
end

###############################################################################
#
#   Parent object call overload
#
###############################################################################

function (a::MatSpace{T})() where {T <: RingElement}
   R = base_ring(a)
   entries = Array{T}(a.rows, a.cols)
   for i = 1:a.rows
      for j = 1:a.cols
         entries[i, j] = zero(R)
      end
   end
   z = Mat{T}(entries)
   z.base_ring = R
   return z
end

function (a::MatSpace{T})(b::S) where {S <: RingElement, T <: RingElement}
   R = base_ring(a)
   entries = Array{T}(a.rows, a.cols)
   rb = R(b)
   for i = 1:a.rows
      for j = 1:a.cols
         if i != j
            entries[i, j] = zero(R)
         else
            entries[i, j] = rb
         end
      end
   end
   z = Mat{T}(entries)
   z.base_ring = R
   return z
end

function (a::MatSpace{T})(b::Mat{T}) where {T <: RingElement}
   parent(b) != a && error("Unable to coerce matrix")
   return b
end

function (a::MatSpace{T})(b::Array{T, 2}) where T <: RingElement
   R = base_ring(a)
   _check_dim(a.rows, a.cols, b)
   for i = 1:a.rows
      for j = 1:a.cols
         b[i, j] = R(b[i, j])
      end
   end
   z = Mat{T}(b)
   z.base_ring = R
   return z
end

function (a::MatSpace{T})(b::Array{S, 2}) where {S <: RingElement, T <: RingElement}
   R = base_ring(a)
   _check_dim(a.rows, a.cols, b)
   entries = Array{T}(a.rows, a.cols)
   for i = 1:a.rows
      for j = 1:a.cols
         entries[i, j] = R(b[i, j])
      end
   end
   z = Mat{T}(entries)
   z.base_ring = R
   return z
end

function (a::MatSpace{T})(b::Array{S, 1}) where {S <: RingElement, T <: RingElement}
   _check_dim(a.rows, a.cols, b)
   b = reshape(b, a.cols, a.rows)'
   z = a(b)
   return z
end

################################################################################
#
#   Matrix constructors
#
################################################################################

doc"""
    matrix(R::Ring, arr::Array{T, 2}) where {T} -> MatElem{T}

> Constructs the matrix over $R$ with entries as in `arr`.
"""
function matrix(R::Ring, arr::Array{T, 2}) where {T}
   arr_coerce = map(R, arr)
   z = Mat{elem_type(R)}(arr_coerce)
   z.base_ring = R
   return z
end

doc"""
    matrix(R::Ring, r::Int, c::Int, arr::Array{T, 1}) where {T} -> MatElem{T}

> Constructs the $r \times c$ matrix over $R$, where the entries are taken
> row-wise from `arr`.
"""
function matrix(R::Ring, r::Int, c::Int, arr::Array{T, 1}) where T
   _check_dim(r, c, arr)
   arr_coerce = map(R, arr)
   z = Mat{elem_type(R)}(r, c, arr_coerce)
   z.base_ring = R
   return z
end

################################################################################
#
#   Zero matrix
#
################################################################################

doc"""
    zero_matrix(R::Ring, r::Int, c::Int) -> MatElem

> Return the $r \times c$ zero matrix over $R$.
"""
function zero_matrix(R::Ring, r::Int, c::Int)
   arr = Array{elem_type(R)}(r, c)
   for i in 1:r
      for j in 1:c
         arr[i, j] = zero(R)
      end
   end
   z = Mat{elem_type(R)}(arr)
   z.base_ring = R
   return z
end

################################################################################
#
#   Identity matrix
#
################################################################################

doc"""
    identity_matrix(R::Ring, n::Int) -> MatElem

> Return the $n \times n$ identity matrix over $R$.
"""
function identity_matrix(R::Ring, n::Int)
   arr = zero_matrix(R, n, n)
   for i in 1:n
      arr[i, i] = one(R)
   end
   z = Mat{elem_type(R)}(arr)
   z.base_ring = R
   return z
end

###############################################################################
#
#   MatrixSpace constructor
#
###############################################################################

doc"""
    MatrixSpace(R::Nemo.Ring, r::Int, c::Int, cached::Bool = true)
> Return parent object corresponding to the space of $r\times c$ matrices over
> the ring $R$. If `cached == true` (the default), the returned parent object
> is cached so that it can returned by future calls to the constructor with the
> same dimensions and base ring.
"""
function MatrixSpace(R::Nemo.Ring, r::Int, c::Int, cached::Bool = true)
   T = elem_type(R)
   return MatSpace{T}(R, r, c, cached)
end
