###############################################################################
#
#   Matrix.jl : Generic mxn matrices over rings
#
###############################################################################

export MatricSpace, GenMat, GenMatSpace, fflu!, fflu, solve_triu, isrref,
       charpoly_danilevsky!, charpoly_danilevsky_ff!, hessenberg!, hessenberg,
       ishessenberg, charpoly_hessenberg!, minpoly, typed_hvcat, typed_hcat,
       powers, similarity!

###############################################################################
#
#   Data type and parent object methods
#
###############################################################################

parent_type{T}(::Type{GenMat{T}}) = GenMatSpace{T}

elem_type{T <: RingElem}(::GenMatSpace{T}) = GenMat{T}

doc"""
    base_ring{T <: RingElem}(S::MatSpace{T})
> Return the base ring $R$ of the given matrix space.
"""
base_ring{T <: RingElem}(a::MatSpace{T}) = a.base_ring::parent_type(T)

doc"""
    base_ring(r::MatElem)
> Return the base ring $R$ of the matrix space that the supplied matrix $r$
> belongs to.
"""
base_ring(a::MatElem) = base_ring(parent(a))

doc"""
    parent(a::MatElem)
> Return the parent object of the given matrix.
"""
parent(a::MatElem) = a.parent

function check_parent(a::MatElem, b::MatElem)
   parent(a) != parent(b) && 
                error("Incompatible matrix spaces in matrix operation")
end

###############################################################################
#
#   Basic manipulation
#
###############################################################################    

function Base.hash(a::MatElem, h::UInt)
   b = 0x3e4ea81eb31d94f4%UInt
   for i in 1:rows(a)
      for j in 1:cols(a)
         b $= hash(a[i, j], h) $ h
         b = (b << 1) | (b >> (sizeof(Int)*8 - 1))
      end
   end
   return b
end

doc"""
    rows(a::MatElem)
> Return the number of rows of the given matrix.
"""
rows(a::MatElem) = parent(a).rows

doc"""
    cols(a::MatElem)
> Return the number of columns of the given matrix.
"""
cols(a::MatElem) = parent(a).cols

function getindex{T <: RingElem}(a::MatElem{T}, r::Int, c::Int)
   return a.entries[r, c]
end
 
function setindex!{T <: RingElem}(a::MatElem{T}, d::T, r::Int, c::Int)
   a.entries[r, c] = d
end

setindex_t!{T <: RingElem}(a::MatElem{T}, d::T, r::Int, c::Int) = setindex!(a, d, c, r)

doc"""
    zero(a::MatSpace)
> Construct the zero matrix in the given matrix space.
"""
zero(a::MatSpace) = a()

doc"""
    one(a::MatSpace)
> Construct the matrix in the given matrix space with ones down the diagonal
> and zeroes elsewhere.
"""
one(a::MatSpace) = a(1)

doc"""
    iszero(a::MatElem)
> Return `true` if the supplied matrix $a$ is the zero matrix, otherwise
> return `false`.
"""
function iszero(a::MatElem)
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
    isone(a::MatElem)
> Return `true` if the supplied matrix $a$ is diagonal with ones along the
> diagonal, otherwise return `false`.
"""
function isone(a::MatElem)
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

function deepcopy_internal{T <: RingElem}(d::MatElem{T}, dict::ObjectIdDict)
   entries = Array{T}(rows(d), cols(d))
   for i = 1:rows(d)
      for j = 1:cols(d)
         entries[i, j] = deepcopy(d[i, j])
      end
   end
   return parent(d)(entries)
end

###############################################################################
#
#   Canonicalisation
#
###############################################################################

canonical_unit(a::MatElem) = canonical_unit(a[1, 1])

###############################################################################
#
#   String I/O
#
###############################################################################

function show(io::IO, a::MatSpace)
   print(io, "Matrix Space of ")
   print(io, a.rows, " rows and ", a.cols, " columns over ")
   print(io, base_ring(a))
end

function show(io::IO, a::MatElem)
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

show_minus_one{T <: RingElem}(::Type{MatElem{T}}) = false

###############################################################################
#
#   Unary operations
#
###############################################################################

doc"""
    -(a::MatElem)
> Return $-a$.
"""
function -(x::MatElem)
   par = parent(x)
   return par(-x.entries)
end

###############################################################################
#
#   Binary operations
#
###############################################################################

doc"""
    +{T <: RingElem}(a::MatElem{T}, b::MatElem{T})
> Return $a + b$.
"""
function +{T <: RingElem}(x::MatElem{T}, y::MatElem{T})
   check_parent(x, y)
   r = parent(x)()
   for i = 1:rows(x)
      for j = 1:cols(x)
         r[i, j] = x[i, j] + y[i, j]
      end
   end
   return r
end

doc"""
    -{T <: RingElem}(a::MatElem{T}, b::MatElem{T})
> Return $a - b$.
"""
function -{T <: RingElem}(x::MatElem{T}, y::MatElem{T})
   check_parent(x, y)
   r = parent(x)()
   for i = 1:rows(x)
      for j = 1:cols(x)
         r[i, j] = x[i, j] - y[i, j]
      end
   end
   return r
end

doc"""
    *{T <: RingElem}(a::MatElem{T}, b::MatElem{T})
> Return $a\times b$.
"""
function *{T <: RingElem}(x::MatElem{T}, y::MatElem{T})
   cols(x) != rows(y) && error("Incompatible matrix dimensions")
   if rows(x) == cols(y) && rows(x) == cols(x)
      parz = parent(x)
   else
      parz = MatrixSpace(base_ring(x), rows(x), cols(y))
   end
   A = Array{T}(rows(x), cols(y))
   C = base_ring(x)()
   for i = 1:rows(x)
      for j = 1:cols(y)
         A[i, j] = base_ring(x)()
         for k = 1:cols(x)
            mul!(C, x[i, k], y[k, j])
            addeq!(A[i, j], C)
         end
      end
   end
   return parz(A)
end

###############################################################################
#
#   Ad hoc binary operators
#
###############################################################################

doc"""
    *(x::Integer, y::MatElem)
> Return $x\times y$.
"""
function *(x::Integer, y::MatElem)
   z = similar(y.entries)
   parz = parent(y)
   for i = 1:rows(y)
      for j = 1:cols(y)
         z[i, j] = x*y[i, j]
      end
   end
   return parz(z)
end

doc"""
    *(x::fmpz, y::MatElem)
> Return $x\times y$.
"""
function *(x::fmpz, y::MatElem)
   z = similar(y.entries)
   parz = parent(y)
   for i = 1:rows(y)
      for j = 1:cols(y)
         z[i, j] = x*y[i, j]
      end
   end
   return parz(z)
end

doc"""
    *{T <: RingElem}(x::T, y::MatElem{T})
> Return $x\times y$.
"""
function *{T <: RingElem}(x::T, y::MatElem{T})
   z = similar(y.entries)
   parz = parent(y)
   for i = 1:rows(y)
      for j = 1:cols(y)
         z[i, j] = x*y[i, j]
      end
   end
   return parz(z)
end

doc"""
    *(x::MatElem, y::Integer)
> Return $x\times y$.
"""
*(x::MatElem, y::Integer) = y*x

doc"""
    *(x::MatElem, y::fmpz)
> Return $x\times y$.
"""
*(x::MatElem, y::fmpz) = y*x

doc"""
    *{T <: RingElem}(x::MatElem{T}, y::T)
> Return $x\times y$.
"""
*{T <: RingElem}(x::MatElem{T}, y::T) = y*x

doc"""
    +(x::Integer, y::MatElem)
> Return $S(x) + y$ where $S$ is the parent of $y$.
"""
function +(x::Integer, y::MatElem)
   z = similar(y.entries)
   parz = parent(y)
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
   return parz(z)
end

doc"""
    +(x::MatElem, y::Integer)
> Return $x + S(y)$ where $S$ is the parent of $x$.
"""
+(x::MatElem, y::Integer) = y + x

doc"""
    +(x::fmpz, y::MatElem)
> Return $S(x) + y$ where $S$ is the parent of $y$.
"""
function +(x::fmpz, y::MatElem)
   z = similar(y.entries)
   parz = parent(y)
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
   return parz(z)
end

doc"""
    +(x::MatElem, y::fmpz)
> Return $x + S(y)$ where $S$ is the parent of $x$.
"""
+(x::MatElem, y::fmpz) = y + x

doc"""
    +{T <: RingElem}(x::T, y::MatElem{T})
> Return $S(x) + y$ where $S$ is the parent of $y$.
"""
function +{T <: RingElem}(x::T, y::MatElem{T})
   z = similar(y.entries)
   parz = parent(y)
   for i = 1:rows(y)
      for j = 1:cols(y)
         if i != j
            z[i, j] = deepcopy(y[i, j])
         else
            z[i, j] = y[i, j] + x
         end
      end
   end
   return parz(z)
end

doc"""
    +{T <: RingElem}(x::MatElem{T}, y::T)
> Return $x + S(y)$ where $S$ is the parent of $x$.
"""
+{T <: RingElem}(x::MatElem{T}, y::T) = y + x

doc"""
    -(x::Integer, y::MatElem)
> Return $S(x) - y$ where $S$ is the parent of $y$.
"""
function -(x::Integer, y::MatElem)
   z = similar(y.entries)
   parz = parent(y)
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
   return parz(z)
end

doc"""
    -(x::MatElem, y::Integer)
> Return $x - S(y)$, where $S$ is the parent of $x$
"""
function -(x::MatElem, y::Integer) 
   z = similar(x.entries)
   parz = parent(x)
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
   return parz(z)
end

doc"""
    -(x::fmpz, y::MatElem)
> Return $S(x) - y$ where $S$ is the parent of $y$.
"""
function -(x::fmpz, y::MatElem)
   z = similar(y.entries)
   parz = parent(y)
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
   return parz(z)
end

doc"""
    -(x::MatElem, y::fmpz)
> Return $x - S(y)$, where $S$ is the parent of $x$
"""
function -(x::MatElem, y::fmpz) 
   z = similar(x.entries)
   parz = parent(x)
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
   return parz(z)
end

doc"""
    -{T <: RingElem}(x::T, y::MatElem{T})
> Return $S(x) - y$ where $S$ is the parent of $y$.
"""
function -{T <: RingElem}(x::T, y::MatElem{T})
   z = similar(y.entries)
   parz = parent(y)
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
   return parz(z)
end

doc"""
    -{T <: RingElem}(x::MatElem{T}, y::T)
> Return $x - S(y)$, where $S$ is the parent of $a$.
"""
function -{T <: RingElem}(x::MatElem{T}, y::T) 
   z = similar(x.entries)
   parz = parent(x)
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
   return parz(z)
end

###############################################################################
#
#   Powering
#
###############################################################################

doc"""
    ^(a::MatElem, b::Int)
> Return $a^b$. We require $b \geq 0$ and that the matrix $a$ is square.
"""
function ^{T <: RingElem}(a::MatElem{T}, b::Int)
   b < 0 && throw(DomainError())
   rows(a) != cols(a) && error("Incompatible matrix dimensions in power")
   # special case powers of x for constructing polynomials efficiently
   if b == 0
      return one(parent(a))
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
    powers{T <: RingElem}(a::MatElem{T}, d::Int)
> Return an array of matrices $M$ wher $M[i + 1] = a^i$ for $i = 0..d$
"""
function powers{T <: RingElem}(a::MatElem{T}, d::Int)
   rows(a) != cols(a) && error("Dimensions do not match in powers")
   d <= 0 && throw(DomainError())
   S = parent(a)
   A = Array{MatElem{T}}(d + 1)
   A[1] = one(S)
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
    =={T <: RingElem}(x::MatElem{T}, y::MatElem{T})
> Return `true` if $x == y$ arithmetically, otherwise return `false`. Recall
> that power series to different precisions may still be arithmetically
> equal to the minimum of the two precisions.
"""
function =={T <: RingElem}(x::MatElem{T}, y::MatElem{T})
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
    isequal{T <: RingElem}(x::MatElem{T}, y::MatElem{T})
> Return `true` if $x == y$ exactly, otherwise return `false`. This function is
> useful in cases where the entries of the matrices are inexact, e.g. power
> series. Only if the power series are precisely the same, to the same precision,
> are they declared equal by this function.
"""
function isequal{T <: RingElem}(x::MatElem{T}, y::MatElem{T})
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
    ==(x::MatElem, y::Integer)
> Return `true` if $x == S(y)$ arithmetically, where $S$ is the parent of $x$,
> otherwise return `false`.
"""
function ==(x::MatElem, y::Integer) 
   for i = 1:min(rows(x), cols(x))
      if x[i, i] != y
         return false
      end
   end
   for i = 1:rows(x)
      for j = 1:cols(x)
         if i != j && x[i, j] != 0
            return false
         end
      end
   end
   return true
end

doc"""
    ==(x::Integer, y::MatElem)
> Return `true` if $S(x) == y$ arithmetically, where $S$ is the parent of $y$,
> otherwise return `false`.
"""
==(x::Integer, y::MatElem) = y == x

doc"""
    ==(x::MatElem, y::fmpz)
> Return `true` if $x == S(y)$ arithmetically, where $S$ is the parent of $x$,
> otherwise return `false`.
"""
function ==(x::MatElem, y::fmpz) 
   for i = 1:min(rows(x), cols(x))
      if x[i, i] != y
         return false
      end
   end
   for i = 1:rows(x)
      for j = 1:cols(x)
         if i != j && x[i, j] != 0
            return false
         end
      end
   end
   return true
end

doc"""
    ==(x::fmpz, y::MatElem)
> Return `true` if $S(x) == y$ arithmetically, where $S$ is the parent of $y$,
> otherwise return `false`.
"""
=={T <: RingElem}(x::fmpz, y::MatElem{T}) = y == x

doc"""
    =={T <: RingElem}(x::MatElem{T}, y::T)
> Return `true` if $x == S(y)$ arithmetically, where $S$ is the parent of $x$,
> otherwise return `false`.
"""
function =={T <: RingElem}(x::MatElem{T}, y::T) 
   for i = 1:min(rows(x), cols(x))
      if x[i, i] != y
         return false
      end
   end
   for i = 1:rows(x)
      for j = 1:cols(x)
         if i != j && x[i, j] != 0
            return false
         end
      end
   end
   return true
end

doc"""
    =={T <: RingElem}(x::T, y::MatElem{T})
> Return `true` if $S(x) == y$ arithmetically, where $S$ is the parent of $y$,
> otherwise return `false`.
"""
=={T <: RingElem}(x::T, y::MatElem{T}) = y == x

###############################################################################
#
#   Ad hoc exact division
#
###############################################################################

doc"""
    divexact(x::MatElem, y::Integer)
> Return $x/y$, i.e. the matrix where each of the entries has been divided by
> $y$. Each division is expected to be exact.
"""
function divexact(x::MatElem, y::Integer)
   z = similar(x.entries)
   parz = parent(x)
   for i = 1:rows(x)
      for j = 1:cols(x)
         z[i, j] = divexact(x[i, j], y)
      end
   end
   return parz(z)
end

doc"""
    divexact(x::MatElem, y::fmpz)
> Return $x/y$, i.e. the matrix where each of the entries has been divided by
> $y$. Each division is expected to be exact.
"""
function divexact(x::MatElem, y::fmpz)
   z = similar(x.entries)
   parz = parent(x)
   for i = 1:rows(x)
      for j = 1:cols(x)
         z[i, j] = divexact(x[i, j], y)
      end
   end
   return parz(z)
end

doc"""
    divexact{T <: RingElem}(x::MatElem{T}, y::T)
> Return $x/y$, i.e. the matrix where each of the entries has been divided by
> $y$. Each division is expected to be exact.
"""
function divexact{T <: RingElem}(x::MatElem{T}, y::T)
   z = similar(x.entries)
   parz = parent(x)
   for i = 1:rows(x)
      for j = 1:cols(x)
         z[i, j] = divexact(x[i, j], y)
      end
   end
   return parz(z)
end

###############################################################################
#
#   Transpose
#
###############################################################################

doc"""
    transpose(x::MatElem)
> Return the transpose of the given matrix.
"""
function transpose(x::MatElem)
   if rows(x) == cols(x)
      par = parent(x)
   else
      par = MatrixSpace(base_ring(x), cols(x), rows(x))
   end
   return par(permutedims(x.entries, [2, 1]))
end

###############################################################################
#
#   Gram matrix
#
###############################################################################

doc"""
    gram(x::MatElem)
> Return the Gram matrix of $x$, i.e. if $x$ is an $r\times c$ matrix return
> the $r\times r$ matrix whose entries $i, j$ are the dot products of the
> $i$-th and $j$-th rows, respectively.
"""
function gram(x::MatElem)
   if rows(x) == cols(x)
      parz = parent(x)
   else
      parz = MatrixSpace(base_ring(x), rows(x), rows(x))
   end
   z = parz()   
   for i = 1:rows(x)
      for j = 1:rows(x)
         z[i, j] = zero(base_ring(x))
         for k = 1:cols(x)
            z[i, j] += x[i, k]*x[j, k]
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
    trace(x::MatElem)
> Return the trace of the matrix $a$, i.e. the sum of the diagonal elements. We
> require the matrix to be square.
"""
function trace(x::MatElem)
   rows(x) != cols(x) && error("Not a square matrix in trace")
   d = zero(base_ring(x))
   for i = 1:rows(x)
      addeq!(d, x[i, i])
   end
   return d
end

###############################################################################
#
#   Content
#
###############################################################################

doc"""
    content(x::MatElem)
> Return the content of the matrix $a$, i.e. the greatest common divisor of all
> its entries, assuming it exists.
"""
function content(x::MatElem)
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
    *(P::perm, x::MatElem)
> Apply the pemutation $P$ to the rows of the matrix $x$ and return the result.
"""
function *(P::perm, x::MatElem)
   z = parent(x)()
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
         
function lufact!{T <: FieldElem}(P::perm, A::MatElem{T})
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
            if A[i, c] != 0
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
            mul!(t, A[r, j], q)
            u = A[i, j]
            addeq!(u, t)
            A[i, j] = u
         end
         A[i, c] = R()
         A[i, rank] = -q
      end
      r += 1
      c += 1
   end
   return rank
end

doc"""
    lufact{T <: FieldElem}(A::MatElem{T}, P = FlintPermGroup(rows(A)))
> Return a tuple $r, p, L, U$ consisting of the rank of $A$, a permutation
> $p$ of $A$ belonging to $P$, a lower triangular matrix $L$ and an upper
> triangular matrix $U$ such that $p(A) = LU$, where $p(A)$ stands for the
> matrix whose rows are the given permutation $p$ of the rows of $A$.
"""
function lufact{T <: FieldElem}(A::MatElem{T}, P = FlintPermGroup(rows(A)))
   m = rows(A)
   n = cols(A)
   P.n != m && error("Permutation does not match matrix")
   p = P()
   R = base_ring(A)
   U = deepcopy(A)
   if m == n
      L = parent(A)()
   else
      L = MatrixSpace(R, m, m)()
   end
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

function fflu!{T <: RingElem}(P::perm, A::MatElem{T})
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
            if A[i, c] != 0
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
            u = A[i, j]
            mul!(u, u, q)
            mul!(t, A[i, c], A[r, j])
            addeq!(u, t)
            if r > 1
               A[i, j] = divexact(u, d)
            else
               A[i, j] = -u
            end
         end
      end
      d = -A[r, c]
      d2 = A[r, c]
      r += 1
      c += 1
   end
   return rank, d2
end

function fflu!{T <: FieldElem}(P::perm, A::MatElem{T})
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
            if A[i, c] != 0
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
            u = A[i, j]
            mul!(u, u, q)
            mul!(t, A[i, c], A[r, j])
            addeq!(u, t)
            if r > 1
               mul!(u, u, d)
               A[i, j] = u
            else
               A[i, j] = -u
            end
         end
      end
      d = -inv(A[r, c])
      d2 = A[r, c]
      r += 1
      c += 1
   end
   return rank, d2
end

doc"""
    fflu{T <: RingElem}(A::MatElem{T}, P = FlintPermGroup(rows(A)))
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
function fflu{T <: RingElem}(A::MatElem{T}, P = FlintPermGroup(rows(A)))
   m = rows(A)
   n = cols(A)
   P.n != m && error("Permutation does not match matrix")
   p = P()
   R = base_ring(A)
   U = deepcopy(A)
   if m == n
      L = parent(A)()
   else
      L = MatrixSpace(R, m, m)()
   end
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

function rref!{T <: RingElem}(A::MatElem{T})
   m = rows(A)
   n = cols(A)
   R = base_ring(A)
   P = FlintPermGroup(m)()
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
            mul!(t, A[i, pivots[np + k]], d)
            for j = i + 1:rank
               mul!(q, A[i, pivots[j]], A[j, pivots[np + k]])
               addeq!(t, q)
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
    rref{T <: RingElem}(M::MatElem{T})
> Returns a tuple $(r, d, A)$ consisting of the rank $r$ of $M$ and a
> denominator $d$ in the base ring of $M$ and a matrix $A$ such that $A/d$ is
> the reduced row echelon form of $M$. Note that the denominator is not usually
> minimal.
"""
function rref{T <: RingElem}(M::MatElem{T})
   A = deepcopy(M)
   r, d = rref!(A)
   return r, d, A
end

function rref!{T <: FieldElem}(A::MatElem{T})
   m = rows(A)
   n = cols(A)
   R = base_ring(A)
   P = FlintPermGroup(m)()
   rnk = lufact!(P, A)
   if rnk == 0
      return 0
   end
   for i = 1:m
      for j = 1:min(rnk, i - 1)
         A[i, j] = R()
      end
   end
   U = MatrixSpace(R, rnk, rnk)()
   V = MatrixSpace(R, rnk, n - rnk)()
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
    rref{T <: FieldElem}(M::MatElem{T})
> Returns a tuple $(r, A)$ consisting of the rank $r$ of $M$ and a reduced row
> echelon form $A$ of $M$.
"""
function rref{T <: FieldElem}(M::MatElem{T})
   A = deepcopy(M)
   r = rref!(A)
   return r, A
end

doc"""
    isrref{T <: RingElem}(M::MatElem{T})
> Return `true` if $M$ is in reduced row echelon form, otherwise return
> `false`.
"""
function isrref{T <: RingElem}(M::MatElem{T})
   m = rows(M)
   n = cols(M)
   c = 1
   for r = 1:m
      for i = 1:c - 1
         if M[r, i] != 0
            return false
         end
      end
      while c <= n && M[r, c] == 0
         c += 1
      end
      if c <= n
         for i = 1:r - 1
            if M[i, c] != 0
               return false
            end
         end
      end   
   end
   return true
end

doc"""
    isrref{T <: FieldElem}(M::MatElem{T})
> Return `true` if $M$ is in reduced row echelon form, otherwise return
> `false`.
"""
function isrref{T <: FieldElem}(M::MatElem{T})
   m = rows(M)
   n = cols(M)
   c = 1
   for r = 1:m
      for i = 1:c - 1
         if M[r, i] != 0
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
            if M[i, c] != 0
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

function reduce_row!{T <: FieldElem}(A::MatElem{T}, P::Array{Int}, L::Array{Int}, m::Int)
   R = base_ring(A)
   n = cols(A)
   t = R()
   for i = 1:n
      if A[m, i] != 0
         h = -A[m, i]
         r = P[i]
         if r != 0
            A[m, i] = R()
            for j = i + 1:L[r]
               mul!(t, A[r, j], h)
               s = A[m, j]
               addeq!(s, t)
               A[m, j] = s
            end 
         else
            h = inv(A[m, i])
            A[m, i] = R(1)
            for j = i + 1:L[m]
               s = A[m, j]
               mul!(s, s, h)
               A[m, j] = s
            end
            P[i] = m
            return i
         end
      end
   end
   return 0
end

function reduce_row!{T <: RingElem}(A::MatElem{T}, P::Array{Int}, L::Array{Int}, m::Int)
   R = base_ring(A)
   n = cols(A)
   t = R()
   c = R(1)
   c1 = 0
   for i = 1:n
      if A[m, i] != 0
         h = -A[m, i]
         r = P[i]
         if r != 0
            d = A[r, i]
            A[m, i] = R()
            for j = i + 1:L[r]
               mul!(t, A[r, j], h)
               s = A[m, j]
               mul!(s, s, d)
               addeq!(s, t)
               A[m, j] = s
            end 
            for j = L[r] + 1:L[m]
               s = A[m, j]
               mul!(s, s, d)
               A[m, j] = s
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
               s = A[m, j]
               mul!(s, s, A[r, i])
               A[m, j] = s
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

function det_clow{T <: RingElem}(M::MatElem{T})
   R = base_ring(M)
   n = rows(M)
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
                  mul!(C, A[i, j], M[i, m])
                  addeq!(B[m, j], C)
               end
               for m = j + 1:n
                  mul!(C, A[i, j], M[i, j])
                  addeq!(B[m, m], -C)
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

function det_df{T <: RingElem}(M::MatElem{T})
   R = base_ring(M)
   S, z = PolynomialRing(R, "z")
   n = rows(M)
   p = charpoly(S, M)
   d = coeff(p, 0)
   return isodd(n) ? -d : d
end

function det_fflu{T <: RingElem}(M::MatElem{T})
   n = rows(M)
   if n == 0
      return base_ring(M)()
   end
   A = deepcopy(M)
   P = FlintPermGroup(n)()
   r, d = fflu!(P, A)
   return r < n ? base_ring(M)() : (parity(P) == 0 ? d : -d)
end

doc"""
    det{T <: FieldElem}(M::MatElem{T})
> Return the determinant of the matrix $M$. We assume $M$ is square.
"""
function det{T <: FieldElem}(M::MatElem{T})
   rows(M) != cols(M) && error("Not a square matrix in det")
   return det_fflu(M)
end

doc"""
    det{T <: RingElem}(M::MatElem{T})
> Return the determinant of the matrix $M$. We assume $M$ is square.
"""
function det{T <: RingElem}(M::MatElem{T})
   try
      return det_fflu(M)
   catch
      return det_df(M)
   end
end

function det_interpolation{T <: PolyElem}(M::MatElem{T})
   n = rows(M)
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
   S = MatrixSpace(base_ring(R), n, n)
   X = S()
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

function det{T <: PolyElem}(M::MatElem{T})
   rows(M) != cols(M) && error("Not a square matrix in det")
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
    rank{T <: RingElem}(M::MatElem{T})
> Return the rank of the matrix $M$.
"""
function rank{T <: RingElem}(M::MatElem{T})
   n = rows(M)
   if n == 0
      return 0
   end
   A = deepcopy(M)
   P = FlintPermGroup(n)()
   r, d = fflu!(P, A)
   return r
end

doc"""
    rank{T <: FieldElem}(M::MatElem{T})
> Return the rank of the matrix $M$.
"""
function rank{T <: FieldElem}(M::MatElem{T})
   n = rows(M)
   if n == 0
      return 0
   end
   A = deepcopy(M)
   P = FlintPermGroup(n)()
   return lufact!(P, A)   
end

###############################################################################
#
#   Linear solving
#
###############################################################################

function backsolve!{T <: FieldElem}(A::MatElem{T}, b::MatElem{T})
   m = rows(A)
   h = cols(b)
   R = base_ring(A)
   t = R()
   for i = m:-1:1
      d = -inv(A[i, i])
      for k = 1:h
         u = -b[i, k]
         for j = i + 1:m
            mul!(t, A[i, j], b[j, k])
            addeq!(u, t)
         end
         mul!(u, u, d)
         b[i, k] = u
      end 
   end
end

function solve!{T <: FieldElem}(A::MatElem{T}, b::MatElem{T})
   m = rows(A)
   n = cols(A)
   h = cols(b)
   r = 1
   c = 1
   R = base_ring(A)
   d = R(1)
   if m == 0 || n == 0
      return
   end
   t = R()
   while r <= m && c <= n
      if A[r, c] == 0
         i = r + 1
         while i <= m
            if A[i, c] != 0
               for j = 1:n
                  A[i, j], A[r, j] = A[r, j], A[i, j]
               end
               for j = 1:h
                  b[i, j], b[r, j] = b[r, j], b[i, j]
               end
               break
            end
            i += 1
         end
         i > m && error("Matrix is singular in solve")
      end
      q = -A[r, c]
      for i = r + 1:m
         for j = 1:h
            mul!(t, A[i, c], b[r, j])
            u = b[i, j]
            mul!(u, u, A[r, c])
            addeq!(u, -t)
            b[i, j] = u 
         end
         for j = c + 1:n
            u = A[i, j]
            mul!(u, u, q)
            mul!(t, A[i, c], A[r, j])
            addeq!(u, t)
            if r > 1
               mul!(u, u, d)
               A[i, j] = u
            else
               A[i, j] = -u
            end
         end
         if r > 1
            for j = 1:h
               u = b[i, j]
               mul!(u, u, -d)
               b[i, j] = u
            end
         end
      end
      d = -inv(A[r, c])
      r += 1
      c += 1
   end
   backsolve!(A, b)
end

function solve_ff{T <: FieldElem}(M::MatElem{T}, b::MatElem{T})
   base_ring(M) != base_ring(b) && error("Base rings don't match in solve")
   rows(M) != cols(M) && error("Non-square matrix in solve")
   rows(M) != rows(b) && error("Dimensions don't match in solve")
   m = rows(M)
   A = deepcopy(M)
   x = deepcopy(b)
   solve!(A, x)
   return x
end

function solve_with_det{T <: FieldElem}(M::MatElem{T}, b::MatElem{T})
   m = rows(M)
   h = cols(b)
   A = deepcopy(M)
   x = deepcopy(b)
   solve!(A, x)
   d = A[m, m]
   for i = 1:m
      for j = 1:h
         u = x[i, j]
         mul!(u, u, d)
         x[i, j] = u
      end
   end   
   return x, d
end

function solve_with_det{T <: RingElem}(M::MatElem{T}, b::MatElem{T})
   return solve(M, b)
end

function backsolve!{T <: RingElem}(A::MatElem{T}, b::MatElem{T})
   m = rows(A)
   h = cols(b)
   R = base_ring(A)
   t = R()
   d = A[m, m]
   for k = 1:h
      b[m, k] = -b[m, k]
   end
   for i = m - 1:-1:1
      q = -A[i, i]
      for k = 1:h
         u = b[i, k]
         mul!(u, u, d)
         for j = i + 1:m
            mul!(t, A[i, j], b[j, k])
            addeq!(u, t)
         end
         b[i, k] = divexact(u, q)
      end 
   end
   for i = 1:m
      for k = 1:h
         b[i, k] = -b[i, k]
      end
   end
   return d
end

function solve!{T <: RingElem}(A::MatElem{T}, b::MatElem{T})
   m = rows(A)
   n = cols(A)
   h = cols(b)
   r = 1
   c = 1
   R = base_ring(A)
   d = R(1)
   if m == 0 || n == 0
      return
   end
   t = R()
   while r <= m && c <= n
      if A[r, c] == 0
         i = r + 1
         while i <= m
            if A[i, c] != 0
               for j = 1:n
                  A[i, j], A[r, j] = A[r, j], A[i, j]
               end
               for j = 1:h
                  b[i, j], b[r, j] = b[r, j], b[i, j]
               end
               break
            end
            i += 1
         end
         i > m && error("Matrix is singular in solve")
      end
      q = -A[r, c]
      for i = r + 1:m
         for j = 1:h
            mul!(t, A[i, c], b[r, j])
            u = b[i, j]
            mul!(u, u, A[r, c])
            addeq!(u, -t)
            b[i, j] = u
         end 
         for j = c + 1:n
            u = A[i, j]
            mul!(u, u, q)
            mul!(t, A[i, c], A[r, j])
            addeq!(u, t)
            if r > 1
               A[i, j] = divexact(u, d)
            else
               A[i, j] = -u
            end
         end
         if r > 1
            for j = 1:h
               b[i, j] = divexact(b[i, j], -d)
            end
         end
      end
      d = -A[r, c]
      r += 1
      c += 1
   end
   return backsolve!(A, b)
end

function solve_ff{T <: RingElem}(M::MatElem{T}, b::MatElem{T})
   m = rows(M)
   n = cols(M)
   if m == 0 || n == 0
      return b, base_ring(M)()
   end
   A = deepcopy(M)
   x = deepcopy(b)
   d = solve!(A, x)
   return x, d
end

function solve_interpolation{T <: PolyElem}(M::MatElem{T}, b::MatElem{T})
   m = rows(M)
   h = cols(b)
   if m == 0
      return b, base_ring(M)()
   end  
   R = base_ring(M)
   S = MatrixSpace(base_ring(R), m, m)
   U = MatrixSpace(base_ring(R), m, h)
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
   V = Array{elem_type(U)}(bound)
   d = Array{elem_type(base_ring(R))}(bound)
   y = Array{elem_type(base_ring(R))}(bound)
   bj = Array{elem_type(base_ring(R))}(bound)
   X = S()
   Y = U()
   x = parent(b)()
   b2 = div(bound, 2)
   pt1 = base_ring(R)(1 - b2)
   for i = 1:bound
      y[i] = base_ring(R)(i - b2)
      (y[i] == pt1 && i != 1) && error("Not enough interpolation points in ring")
      for j = 1:m
         for k = 1:m
            X[j, k] = evaluate(M[j, k], y[i])
         end
         for k = 1:h
            Y[j, k] = evaluate(b[j, k], y[i])
         end
      end
      V[i], d[i] = solve_with_det(X, Y)
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
    solve{T <: RingElem}(M::MatElem{T}, b::MatElem{T})
> Given a non-singular $n\times n$ matrix over a ring and an $n\times m$
> matrix over the same ring, return a tuple $x, d$ consisting of an
> $n\times m$ matrix $x$ and a denominator $d$ such that $Ax = db$. The
> denominator will be the determinant of $A$ up to sign. If $A$ is singular an
> exception is raised.
"""
function solve{T}(M::MatElem{T}, b::MatElem{T})
   return solve_ringelem(M, b)
end

function solve_ringelem{T <: RingElem}(M::MatElem{T}, b::MatElem{T})
   base_ring(M) != base_ring(b) && error("Base rings don't match in solve")
   rows(M) != cols(M) && error("Non-square matrix in solve")
   rows(M) != rows(b) && error("Dimensions don't match in solve")
   return solve_ff(M, b)
end

function solve{T <: PolyElem}(M::MatElem{T}, b::MatElem{T})
   base_ring(M) != base_ring(b) && error("Base rings don't match in solve")
   rows(M) != cols(M) && error("Non-square matrix in solve")
   rows(M) != rows(b) && error("Dimensions don't match in solve")
   try
      return solve_interpolation(M, b)
   catch
      return solve_ff(M, b)
   end
end

###############################################################################
#
#   Upper triangular solving
#
###############################################################################

doc"""
    solve_triu{T <: FieldElem}(U::MatElem{T}, b::MatElem{T}, unit=false)
> Given a non-singular $n\times n$ matrix over a field which is upper
> triangular, and an $n\times m$ matrix over the same field, return an
> $n\times m$ matrix $x$ such that $Ax = b$. If $A$ is singular an exception
> is raised. If unit is true then $U$ is assumed to have ones on its
> diagonal, and the diagonal will not be read.
"""
function solve_triu{T <: FieldElem}(U::MatElem{T}, b::MatElem{T}, unit=false)
   n = rows(U)
   m = cols(b)
   R = base_ring(U)
   X = parent(b)()
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
            mul!(t, U[j, k], tmp[k])
            addeq!(s, t)
         end
         s = b[j, i] - s
         if unit == false
            mul!(s, s, Tinv[j])
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
    inv{T <: RingElem}(M::MatElem{T})
> Given a non-singular $n\times n$ matrix over a ring the tuple $X, d$
> consisting of an $n\times n$ matrix $X$ and a denominator $d$ such that
> $AX = dI_n$, where $I_n$ is the $n\times n$ identity matrix. The denominator
> will be the determinant of $A$ up to sign. If $A$ is singular an exception 
> is raised.
"""
function inv{T <: RingElem}(M::MatElem{T})
   cols(M) != rows(M) && error("Matrix not square in invert")
   n = cols(M)
   X = one(parent(M))
   A = deepcopy(M)
   d = solve!(A, X)
   return X, d
end

doc"""
    inv{T <: FieldElem}(M::MatElem{T})
> Given a non-singular $n\times n$ matrix over a field, return an
> $n\times n$ matrix $X$ such that $AX = I_n$ where $I_n$ is the $n\times n$
> identity matrix. If $A$ is singular an exception is raised.
"""
function inv{T <: FieldElem}(M::MatElem{T})
   cols(M) != rows(M) && error("Matrix not square in invert")
   n = cols(M)
   X = one(parent(M))
   A = deepcopy(M)
   solve!(A, X)
   return X
end

###############################################################################
#
#   Nullspace
#
###############################################################################

doc"""
    nullspace{T <: RingElem}(M::MatElem{T})
> Returns a tuple $(\nu, N)$ consisting of the nullity $\nu$ of $M$ and
> a basis $N$ (consisting of column vectors) for the right nullspace of $M$,
> i.e. such that $MN$ is the zero matrix. If $M$ is an $m\times n$ matrix
> $N$ will be an $n\times \nu$ matrix. Note that the nullspace is taken to be
> the vector space kernel over the fraction field of the base ring if the
> latter is not a field. In Nemo we use the name ``kernel'' for a function to
> compute an integral kernel.
"""
function nullspace{T <: RingElem}(M::MatElem{T})
   n = cols(M)
   rank, d, A = rref(M)
   nullity = n - rank
   R = base_ring(M)
   U = MatrixSpace(R, n, nullity)()
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
    nullspace{T <: FieldElem}(M::MatElem{T})
> Returns a tuple $(\nu, N)$ consisting of the nullity $\nu$ of $M$ and
> a basis $N$ (consisting of column vectors) for the right nullspace of $M$,
> i.e. such that $MN$ is the zero matrix. If $M$ is an $m\times n$ matrix
> $N$ will be an $n\times \nu$ matrix. Note that the nullspace is taken to be
> the vector space kernel over the fraction field of the base ring if the
> latter is not a field. In Nemo we use the name ``kernel'' for a function to
> compute an integral kernel.
"""
function nullspace{T <: FieldElem}(M::MatElem{T})
   m = rows(M)
   n = cols(M)
   rank, A = rref(M)
   nullity = n - rank
   R = base_ring(M)
   X = MatrixSpace(R, n, nullity)()
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

function hessenberg!{T <: RingElem}(A::MatElem{T})
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
         if A[m, m - 1] != 0
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
            if A[i, m - 1] != 0
               mul!(u, A[i, m - 1], h)
               for j = m:n
                  mul!(t, u, A[m, j])
                  s = A[i, j]
                  addeq!(s, t)
                  A[i, j] = s
               end
               u = -u
               for j = 1:n
                  mul!(t, u, A[j, i])
                  s = A[j, m]
                  addeq!(s, t)
                  A[j, m] = s
               end
               A[i, m - 1] = R()
            end
         end
      end
   end
end

doc"""
    hessenberg{T <: RingElem}(A::MatElem{T})
> Returns the Hessenberg form of $M$, i.e. an upper Hessenberg matrix
> which is similar to $M$. The upper Hessenberg form has nonzero entries
> above and on the diagonal and in the diagonal line immediately below the
> diagonal.
"""
function hessenberg{T <: RingElem}(A::MatElem{T})
   rows(A) != cols(A) && error("Dimensions don't match in hessenberg")
   M = deepcopy(A)
   hessenberg!(M)
   return M
end

doc"""
    ishessenberg{T <: RingElem}(A::MatElem{T})
> Returns `true` if $M$ is in Hessenberg form, otherwise returns `false`.
"""
function ishessenberg{T <: RingElem}(A::MatElem{T})
   n = rows(A)
   for i = 3:n
      for j = 1:i - 2
         if A[i, j] != 0
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

function charpoly_hessenberg!{T <: RingElem}(S::Ring, A::MatElem{T})
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
         mul!(t, t, A[m - i + 1, m - i])
         P[m + 1] -= t*A[m - i, m]*P[m - i]
      end
   end
   return P[n + 1]
end

function charpoly_danilevsky_ff!{T <: RingElem}(S::Ring, A::MatElem{T})
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
            setcoeff!(b, i, R(1))
            for k = 1:i
               setcoeff!(b, k - 1, -A[n - i + 1, n - k + 1]*d)
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
            mul!(t, A[j, n - i], V[k])
            u = A[j, k]*h
            addeq!(u, t)
            A[j, k] = u
         end
         for k = n - i + 1:n
            mul!(t, A[j, n - i], V[k])
            u = A[j, k]*h
            addeq!(u, t)
            A[j, k] = u
         end
      end
      for k = 1:n
         A[n - i + 1, k] = R()
      end
      for j = 1:n - i
         for k = 1:n - i - 1
            mul!(A[j, k], A[j, k], d)
         end
         for k = n - i + 1:n
            mul!(A[j, k], A[j, k], d)
         end
      end
      A[n - i + 1, n - i] = deepcopy(h)
      for j = 1:n - i - 1
         s = R()
         for k = 1:n - i
            mul!(t, A[k, j], W[k])
            addeq!(s, t)
         end
         A[n - i, j] = s
      end
      for j = n - i:n - 1
         s = R()
         for k = 1:n - i
            mul!(t, A[k, j], W[k])
            addeq!(s, t)
         end
         mul!(t, h, W[j + 1])
         addeq!(s, t)
         A[n - i, j] = s
      end
      s = R()
      for k = 1:n - i
         mul!(t, A[k, n], W[k])
         addeq!(s, t)
      end
      A[n - i, n] = s
      for k = 1:n
         mul!(A[n - i, k], A[n - i, k], d)
      end
      d = inv(h)
      i += 1
   end
   b = S()
   fit!(b, n + 1)
   setcoeff!(b, n, R(1))
   for i = 1:n
      setcoeff!(b, i - 1, -A[1, n - i + 1]*d)
   end
   return pol*b
end

function charpoly_danilevsky!{T <: RingElem}(S::Ring, A::MatElem{T})
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
            setcoeff!(b, i, R(1))
            for k = 1:i
               setcoeff!(b, k - 1, -A[n - i + 1, n - k + 1])
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
            mul!(t, A[j, n - i], V[k])
            u = A[j, k]
            addeq!(u, t)
            A[j, k] = u
         end
         for k = n - i + 1:n
            mul!(t, A[j, n - i], V[k])
            u = A[j, k]
            addeq!(u, t)
            A[j, k] = u
         end
         u = A[j, n - i]
         mul!(u, u, h)
         A[j, n - i] = u
      end
      for j = 1:n - i - 1
         s = R()
         for k = 1:n - i
            mul!(t, A[k, j], W[k])
            addeq!(s, t)
         end
         A[n - i, j] = s
      end
      for j = n - i:n - 1
         s = R()
         for k = 1:n - i
            mul!(t, A[k, j], W[k])
            addeq!(s, t)
         end
         addeq!(s, W[j + 1])
         A[n - i, j] = s
      end
      s = R()
      for k = 1:n - i
         mul!(t, A[k, n], W[k])
         addeq!(s, t)
      end
      A[n - i, n] = s
      i += 1
   end
   b = S()
   fit!(b, n + 1)
   setcoeff!(b, n, R(1))
   for i = 1:n
      setcoeff!(b, i - 1, -A[1, n - i + 1])
   end
   return pol*b
end

doc"""
    charpoly{T <: RingElem}(V::Ring, Y::MatElem{T})
> Returns the characteristic polynomial $p$ of the matrix $M$. The
> polynomial ring $R$ of the resulting polynomial must be supplied
> and the matrix is assumed to be square.
"""
function charpoly{T <: RingElem}(V::Ring, Y::MatElem{T})
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
               mul!(p, Y[k, l], M[j - 1, l])
               addeq!(s, p)
            end
            M[j, k] = s
         end
         A[j] = M[j, i]
      end 
      s = R()
      for j = 1:i
         mul!(p, Y[i, j], M[i - 1, j])
         addeq!(s, p)
      end
      A[i] = s
      for j = 1:i
         s = -F[j]
         for k = 1:j - 1
            mul!(p, A[k], F[j - k])
            addeq!(s, p)
         end
         F[j] = -s - A[j]
     end
   end
   z = gen(V)
   f = z^n
   for i = 1:n
      setcoeff!(f, n - i, F[i])
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
    minpoly{T <: FieldElem}(S::Ring, M::MatElem{T}, charpoly_only = false)
> Returns the minimal polynomial $p$ of the matrix $M$. The polynomial ring $R$
> of the resulting polynomial must be supplied and the matrix must be square.
"""
function minpoly{T <: FieldElem}(S::Ring, M::MatElem{T}, charpoly_only = false)
   rows(M) != cols(M) && error("Not a square matrix in minpoly")
   base_ring(S) != base_ring(M) && error("Unable to coerce polynomial")
   n = rows(M)
   if n == 0
      return S(1)
   end
   R = base_ring(M)
   p = S(1)
   A = MatrixSpace(R, n + 1, 2n + 1)()
   B = MatrixSpace(R, n, n)()
   U = MatrixSpace(R, n, 1)
   L1 = [n + i for i in 1:n + 1]
   L2 = [n for i in 1:n]
   P2 = zeros(Int, n)
   P2[1] = 1
   c2 = 1
   r2 = 1
   first_poly = true
   while r2 <= n
      P1 = [0 for i in 1:2n + 1]
      v = zero(U)
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
         setcoeff!(b, i - 1, A[r1, n + i]*h)
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
    minpoly{T <: RingElem}(S::Ring, M::MatElem{T}, charpoly_only = false)
> Returns the minimal polynomial $p$ of the matrix $M$. The polynomial ring $R$
> of the resulting polynomial must be supplied and the matrix must be square.
"""
function minpoly{T <: RingElem}(S::Ring, M::MatElem{T}, charpoly_only = false)
   rows(M) != cols(M) && error("Not a square matrix in minpoly")
   base_ring(S) != base_ring(M) && error("Unable to coerce polynomial")
   n = rows(M)
   if n == 0
      return S(1)
   end
   R = base_ring(M)
   p = S(1)
   A = MatrixSpace(R, n + 1, 2n + 1)()
   B = MatrixSpace(R, n, n)()
   U = MatrixSpace(R, n, 1)
   L1 = [n + i for i in 1:n + 1]
   L2 = [n for i in 1:n]
   P2 = zeros(Int, n)
   P2[1] = 1
   c2 = 1
   r2 = 1
   first_poly = true
   while r2 <= n
      P1 = [0 for i in 1:2n + 1]
      v = zero(U)
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
      for i = 1:r1
         setcoeff!(b, i - 1, A[r1, n + i])
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
#   Transforms
#
###############################################################################

doc"""
    similarity!{T <: RingElem}(A::MatElem{T}, r::Int, d::T)
> Applies a similarity transform to the $n\times n$ matrix $M$ in-place. Let
> $P$ be the $n\times n$ identity matrix that has had all zero entries of row
> $r$ replaced with $d$, then the transform applied is equivalent to
> $M = P^{-1}MP$. We require $M$ to be a square matrix. A similarity transform
> preserves the minimal and characteristic polynomials of a matrix.
"""
function similarity!{T <: RingElem}(A::MatElem{T}, r::Int, d::T)
   n = rows(A)
   t = base_ring(A)()
   for i = 1:n
      for j = 1:r - 1
         mul!(t, A[i, r], d)
         s = A[i, j]
         addeq!(s, t)
         A[i, j] = s
      end
      for j = r + 1:n
         mul!(t, A[i, r], d)
         s = A[i, j]
         addeq!(s, t)
         A[i, j] = s
      end
   end
   d = -d
   for i = 1:n
      s = A[r, i]
      for j = 1:r - 1
         mul!(t, A[j, i], d)
         addeq!(s, t)
      end
      for j = r + 1:n
         mul!(t, A[j, i], d)
         addeq!(s, t)
      end
      A[r, i] = s
   end
end

###############################################################################
#
#   Row swapping
#
###############################################################################

doc"""
    swap_rows(a::MatElem, i::Int, j::Int)
> Return a matrix $b$ with the entries of $a$, where the $i$th and $j$th 
> row are swapped.
"""
function swap_rows(a::MatElem, i::Int, j::Int)
   (1<=i<=rows(a) && 1<=j<=rows(a)) || throw(BoundsError())  
   b = deepcopy(a)
   swap_rows!(b, i, j)
   return b
end

doc"""
    swap_rows!(a::MatElem, i::Int, j::Int)
> Swap the $i$th and $j$th row of $a$.
"""
function swap_rows!(a::MatElem, i::Int, j::Int)
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
    hcat(a::MatElem, b::MatElem)
> Return the horizontal concatenation of $a$ and $b$. Assumes that the
> number of rows is the same in $a$ and $b$.
"""
function hcat(a::MatElem, b::MatElem)
   rows(a) != rows(b) && error("Incompatible number of rows in hcat")
   c = MatrixSpace(base_ring(a), rows(a), cols(a) + cols(b))()
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
    vcat(a::MatElem, b::MatElem)
> Return the vertical concatenation of $a$ and $b$. Assumes that the
> number of columns is the same in $a$ and $b$.
"""
function vcat(a::MatElem, b::MatElem)
   cols(a) != cols(b) && error("Incompatible number of columns in vcat")
   c = MatrixSpace(base_ring(a), rows(a) + rows(b), cols(a))()
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
#   Promotion rules
#
###############################################################################

Base.promote_rule{T <: RingElem, V <: Integer}(::Type{GenMat{T}}, ::Type{V}) = GenMat{T}

Base.promote_rule{T <: RingElem}(::Type{GenMat{T}}, ::Type{T}) = GenMat{T}

Base.promote_rule{T <: RingElem}(::Type{GenMat{T}}, ::Type{fmpz}) = GenMat{T}

function promote_rule1{T <: RingElem, U <: RingElem}(::Type{GenMat{T}}, ::Type{GenMat{U}})
   U == T ? GenMat{T} : Union{}
end

function Base.promote_rule{T <: RingElem, U <: RingElem}(::Type{GenMat{T}}, ::Type{U})
   Base.promote_rule(T, U) == T ? GenMat{T} : promote_rule1(U, GenMat{T})
end

###############################################################################
#
#   Parent object call overload
#
###############################################################################

function (a::GenMatSpace{T}){T <: RingElem}(b::fmpz_mat)
  if a.rows != rows(b) || a.cols != cols(b)
    error("incompatible matrix dimensions")
  end
  A = a()
  R = base_ring(a)
  for i=1:a.rows
    for j=1:a.cols
      A[i,j] = R(b[i,j])
    end
  end
  return A
end

function (a::GenMatSpace{T}){T <: RingElem}(b::RingElem)
   return a(base_ring(a)(b))
end

function (a::GenMatSpace{T}){T <: RingElem}()
   entries = Array{T}(a.rows, a.cols)
   for i = 1:a.rows
      for j = 1:a.cols
         entries[i, j] = zero(base_ring(a))
      end
   end
   z = GenMat{T}(entries)
   z.parent = a
   return z
end

function (a::GenMatSpace{T}){T <: RingElem}(b::Integer)
   entries = Array{T}(a.rows, a.cols)
   for i = 1:a.rows
      for j = 1:a.cols
         if i != j
            entries[i, j] = zero(base_ring(a))
         else
            entries[i, j] = base_ring(a)(b)
         end
      end
   end
   z = GenMat{T}(entries)
   z.parent = a
   return z
end

function (a::GenMatSpace{T}){T <: RingElem}(b::fmpz)
   entries = Array{T}(a.rows, a.cols)
   for i = 1:a.rows
      for j = 1:a.cols
         if i != j
            entries[i, j] = zero(base_ring(a))
         else
            entries[i, j] = base_ring(a)(b)
         end
      end
   end
   z = GenMat{T}(entries)
   z.parent = a
   return z
end

function (a::GenMatSpace{T}){T <: RingElem}(b::T)
   parent(b) != base_ring(a) && error("Unable to coerce to matrix")
   entries = Array{T}(a.rows, a.cols)
   for i = 1:a.rows
      for j = 1:a.cols
         if i != j
            entries[i, j] = zero(base_ring(a))
         else
            entries[i, j] = deepcopy(b)
         end
      end
   end
   z = GenMat{T}(entries)
   z.parent = a
   return z
end

function (a::GenMatSpace{T}){T <: RingElem}(b::GenMat{T})
   parent(b) != a && error("Unable to coerce matrix")
   return b
end

function (a::GenMatSpace{T}){T <: RingElem}(b::Array{T, 2})
   if length(b) > 0
      parent(b[1, 1]) != base_ring(a) && error("Unable to coerce to matrix")
   end
   _check_dim(a.rows, a.cols, b)
   z = GenMat{T}(b)
   z.parent = a
   return z
end

function (a::GenMatSpace{T}){T <: RingElem}(b::Array{T, 1})
   if length(b) > 0
      parent(b[1]) != base_ring(a) && error("Unable to coerce to matrix")
   end
   _check_dim(a.rows, a.cols, b)
   b = reshape(b, a.rows, a.cols)'
   z = GenMat{T}(b)
   z.parent = a
   return z
end

(a::GenMatSpace)(b::Array{fmpz, 2}) = a(map(base_ring(a), b))

(a::GenMatSpace)(b::Array{fmpz, 1}) = a(map(base_ring(a), b))

(a::GenMatSpace){T <: Integer}(b::Array{T, 2}) = a(map(base_ring(a), b))

(a::GenMatSpace){T <: Integer}(b::Array{T, 1}) = a(map(base_ring(a), b))

function Base.Matrix{T}(R::Ring, r::Int, c::Int, a::Array{T,2})
   M = MatrixSpace(R, r, c)
   return M(a)
end

function Base.Matrix{T}(R::Ring, r::Int, c::Int, a::Array{T,1})
   M = MatrixSpace(R, r, c)
   return M(a)
end

###############################################################################
#
#   MatrixSpace constructor
#
###############################################################################

doc"""
    MatrixSpace(R::Ring, r::Int, c::Int; cached=true)
> Return parent object corresponding to the space of $r\times c$ matrices over
> the ring $R$. If `cached == true` (the default), the returned parent object
> is cached so that it can returned by future calls to the constructor with the
> same dimensions and base ring.
"""
function MatrixSpace(R::Ring, r::Int, c::Int; cached=true)
   T = elem_type(R)
   return GenMatSpace{T}(R, r, c, cached)
end

function typed_hvcat(R::Ring, dims, d...)
   T = elem_type(R)
   r = length(dims)
   c = dims[1]
   A = Array{T}(r, c)
   for i = 1:r
      dims[i] != c && throw(ArgumentError("row $i has mismatched number of columns (expected $c, got $(dims[i]))"))
      for j = 1:c
         A[i, j] = R(d[(i - 1)*c + j])
      end
   end 
   S = MatrixSpace(R, r, c)
   return S(A)
end

function typed_hcat(R::Ring, d...)
   T = elem_type(R)
   r = length(d)
   A = Array{T}(1, r)
   for i = 1:r
      A[1, i] = R(d[i])
   end
   S = MatrixSpace(R, 1, r)
   return S(A)
end
