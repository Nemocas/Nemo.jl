###############################################################################
#
#   Matrix.jl : Generic mxn matrices over rings
#
###############################################################################

export MatricSpace, GenMat, GenMatrixSpace, fflu!, fflu, solve_triu, is_rref,
       charpoly_danilevsky!, charpoly_danilevsky_ff!, hessenberg!, hessenberg,
       is_hessenberg, charpoly_hessenberg!, minpoly, typed_hvcat, typed_hcat,
       powers, similarity!

###############################################################################
#
#   Data type and parent object methods
#
###############################################################################

parent_type{T}(::Type{GenMat{T}}) = GenMatrixSpace{T}

elem_type{T <: RingElem}(::GenMatrixSpace{T}) = GenMat{T}

base_ring{T}(a::GenMatrixSpace{T}) = a.base_ring::parent_type(T)

base_ring(a::MatElem) = base_ring(parent(a))

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

rows(a::MatElem) = parent(a).rows

cols(a::MatElem) = parent(a).cols

function getindex{T <: RingElem}(a::MatElem{T}, r::Int, c::Int)
   return a.entries[r, c]
end
 
function setindex!{T <: RingElem}(a::MatElem{T}, d::T, r::Int, c::Int)
   a.entries[r, c] = d
end

setindex_t!{T <: RingElem}(a::MatElem{T}, d::T, r::Int, c::Int) = setindex!(a, d, c, r)

zero(a::GenMatrixSpace) = a()

one(a::GenMatrixSpace) = a(1)

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

function deepcopy{T <: RingElem}(d::MatElem{T})
   entries = Array(T, rows(d), cols(d))
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

function show(io::IO, a::GenMatrixSpace)
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

function -(x::MatElem)
   par = parent(x)
   return par(-x.entries)
end

function transpose(x::MatElem)
   if rows(x) == cols(x)
      par = parent(x)
   else
      par = GenMatrixSpace(base_ring(x), cols(x), rows(x))
   end
   return par(x.entries')
end

###############################################################################
#
#   Binary operations
#
###############################################################################

function +{T <: RingElem}(x::MatElem{T}, y::MatElem{T})
   check_parent(x, y)
   parz = parent(x)
   return parz(x.entries + y.entries)
end

function -{T <: RingElem}(x::MatElem{T}, y::MatElem{T})
   check_parent(x, y)
   parz = parent(x)
   return parz(x.entries - y.entries)
end

function *{T <: RingElem}(x::MatElem{T}, y::MatElem{T})
   cols(x) != rows(y) && error("Incompatible matrix dimensions")
   if rows(x) == cols(y) && rows(x) == cols(x)
      parz = parent(x)
   else
      parz = MatrixSpace(base_ring(x), rows(x), cols(y))
   end
   A = Array(T, rows(x), cols(y))
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

function *{T <: RingElem}(x::Integer, y::MatElem{T})
   z = similar(y.entries)
   parz = parent(y)
   for i = 1:rows(y)
      for j = 1:cols(y)
         z[i, j] = x*y[i, j]
      end
   end
   return parz(z)
end

function *{T <: RingElem}(x::fmpz, y::MatElem{T})
   z = similar(y.entries)
   parz = parent(y)
   for i = 1:rows(y)
      for j = 1:cols(y)
         z[i, j] = x*y[i, j]
      end
   end
   return parz(z)
end

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

*{T <: RingElem}(x::MatElem{T}, y::Integer) = y*x

*{T <: RingElem}(x::MatElem{T}, y::fmpz) = y*x

*{T <: RingElem}(x::MatElem{T}, y::T) = y*x

function +{T <: RingElem}(x::Integer, y::MatElem{T})
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

+{T <: RingElem}(x::MatElem{T}, y::Integer) = y + x

function +{T <: RingElem}(x::fmpz, y::MatElem{T})
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

+{T <: RingElem}(x::MatElem{T}, y::fmpz) = y + x

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

+{T <: RingElem}(x::MatElem{T}, y::T) = y + x

function -{T <: RingElem}(x::Integer, y::MatElem{T})
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

function -{T <: RingElem}(x::MatElem{T}, y::Integer) 
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

function -{T <: RingElem}(x::fmpz, y::MatElem{T})
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

function -{T <: RingElem}(x::MatElem{T}, y::fmpz) 
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

function powers{T <: RingElem}(a::MatElem{T}, d::Int)
   rows(a) != cols(a) && error("Dimensions do not match in powers")
   d <= 0 && throw(DomainError())
   S = parent(a)
   A = Array(MatElem{T}, d + 1)
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

###############################################################################
#
#   Ad hoc comparisons
#
###############################################################################

function =={T <: RingElem}(x::MatElem{T}, y::Integer) 
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

=={T <: RingElem}(x::Integer, y::MatElem{T}) = y == x

function =={T <: RingElem}(x::MatElem{T}, y::fmpz) 
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

=={T <: RingElem}(x::fmpz, y::MatElem{T}) = y == x

###############################################################################
#
#   Ad hoc exact division
#
###############################################################################

function divexact{T <: RingElem}(x::MatElem{T}, y::Integer)
   z = similar(x.entries)
   parz = parent(x)
   for i = 1:rows(x)
      for j = 1:cols(x)
         z[i, j] = divexact(x[i, j], y)
      end
   end
   return parz(z)
end

function divexact{T <: RingElem}(x::MatElem{T}, y::fmpz)
   z = similar(x.entries)
   parz = parent(x)
   for i = 1:rows(x)
      for j = 1:cols(x)
         z[i, j] = divexact(x[i, j], y)
      end
   end
   return parz(z)
end

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
#   Gram matrix
#
###############################################################################

function gram{T <: RingElem}(x::MatElem{T})
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

function trace{T <: RingElem}(x::MatElem{T})
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

function content{T <: RingElem}(x::MatElem{T})
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

function *{T <: RingElem}(P::perm, x::MatElem{T})
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
      pivots = Array(Int, n)
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
   pivots = Array(Int, n)
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

function rref{T <: FieldElem}(M::MatElem{T})
   A = deepcopy(M)
   r = rref!(A)
   return r, A
end

function is_rref{T <: RingElem}(M::MatElem{T})
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

function is_rref{T <: FieldElem}(M::MatElem{T})
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
   A = Array(T, n, n)
   B = Array(T, n, n)
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

function det{T <: FieldElem}(M::MatElem{T})
   rows(M) != cols(M) && error("Not a square matrix in det")
   return det_fflu(M)
end

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

function solve_interpolation{T <: RingElem}(M::MatElem{PolyElem{T}}, b::MatElem{PolyElem{T}})
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
   V = Array(elem_type(U), bound)
   d = Array(elem_type(base_ring(R)), bound)
   y = Array(elem_type(base_ring(R)), bound)
   bj = Array(elem_type(base_ring(R)), bound)
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

function solve{T <: RingElem}(M::MatElem{T}, b::MatElem{T})
   base_ring(M) != base_ring(b) && error("Base rings don't match in solve")
   rows(M) != cols(M) && error("Non-square matrix in solve")
   rows(M) != rows(b) && error("Dimensions don't match in solve")
   return solve_ff(M, b)
end

function solve{T <: RingElem}(M::MatElem{PolyElem{T}}, b::MatElem{PolyElem{T}})
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

function solve_triu{T <: FieldElem}(U::MatElem{T}, b::MatElem{T}, unit=false)
   n = rows(U)
   m = cols(b)
   R = base_ring(U)
   X = parent(b)()
   Tinv = Array(elem_type(R), n)
   tmp = Array(elem_type(R), n)
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

function inv{T <: RingElem}(M::MatElem{T})
   cols(M) != rows(M) && error("Matrix not square in invert")
   n = cols(M)
   X = one(parent(M))
   A = deepcopy(M)
   d = solve!(A, X)
   return X, d
end

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
      pivots = Array(Int, rank)
      nonpivots = Array(Int, nullity)
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
      pivots = Array(Int, max(m, n))
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

function hessenberg{T <: RingElem}(A::MatElem{T})
   rows(A) != cols(A) && error("Dimensions don't match in hessenberg")
   M = deepcopy(A)
   hessenberg!(M)
   return M
end

function is_hessenberg{T <: RingElem}(A::MatElem{T})
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
   P = Array(elem_type(S), n + 1)
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
   V = Array(T, n)
   W = Array(T, n)
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
   V = Array(T, n)
   W = Array(T, n)
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

function charpoly{T <: RingElem}(V::Ring, Y::MatElem{T})
   rows(Y) != cols(Y) && error("Dimensions don't match in det")
   R = base_ring(Y)
   base_ring(V) != base_ring(Y) && error("Cannot coerce into polynomial ring")
   n = rows(Y)
   if n == 0
      return V(1)
   end
   F = Array(elem_type(R), n)
   A = Array(elem_type(R), n)
   M = Array(elem_type(R), n - 1, n)
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

function Base.call{T <: RingElem}(a::GenMatrixSpace{T}, b::RingElem)
   return a(base_ring(a)(b))
end

function Base.call{T <: RingElem}(a::GenMatrixSpace{T})
   entries = Array(T, a.rows, a.cols)
   for i = 1:a.rows
      for j = 1:a.cols
         entries[i, j] = zero(base_ring(a))
      end
   end
   z = GenMat{T}(entries)
   z.parent = a
   return z
end

function Base.call{T <: RingElem}(a::GenMatrixSpace{T}, b::Integer)
   entries = Array(T, a.rows, a.cols)
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

function Base.call{T <: RingElem}(a::GenMatrixSpace{T}, b::T)
   parent(b) != base_ring(a) && error("Unable to coerce to matrix")
   entries = Array(T, a.rows, a.cols)
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

function Base.call{T <: RingElem}(a::GenMatrixSpace{T}, b::GenMat{T})
   parent(b) != a && error("Unable to coerce matrix")
   return b
end

function Base.call{T <: RingElem}(a::GenMatrixSpace{T}, b::Array{T, 2})
   if length(b) > 0
      parent(b[1, 1]) != base_ring(a) && error("Unable to coerce to matrix")
   end
   z = GenMat{T}(b)
   z.parent = a
   return z
end

###############################################################################
#
#   MatrixSpace constructor
#
###############################################################################

function MatrixSpace(R::Ring, r::Int, c::Int; cached=true)
   T = elem_type(R)
   return GenMatrixSpace{T}(R, r, c, cached)
end

function typed_hvcat(R::Ring, dims, d...)
   T = elem_type(R)
   r = length(dims)
   c = dims[1]
   A = Array(T, r, c)
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
   A = Array(T, 1, r)
   for i = 1:r
      A[1, i] = R(d[i])
   end
   S = MatrixSpace(R, 1, r)
   return S(A)
end
