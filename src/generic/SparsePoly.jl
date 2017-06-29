###############################################################################
#
#   SparsePoly.jl : Generic sparse univariate polynomials over rings
#
###############################################################################

export GenSparsePoly, GenSparsePolyRing, SparsePolynomialRing

###############################################################################
#
#   Data type and parent object methods
#
###############################################################################

parent_type{T <: RingElem}(::Type{GenSparsePoly{T}}) = GenSparsePolyRing{T}

elem_type{T <: RingElem}(::Type{GenSparsePolyRing{T}}) = GenSparsePoly{T}

var(a::GenSparsePolyRing) = a.S

function gen{T}(a::GenSparsePolyRing{T})
   return a([one(base_ring(a))], [UInt(1)])
end

###############################################################################
#
#   Basic manipulation
#
###############################################################################

function coeff(x::GenSparsePoly, i::Int)
   i < 0 && throw(DomainError())
   return x.coeffs[i + 1]
end

one(R::GenSparsePolyRing) = R(1)

zero(R::GenSparsePolyRing) = R(0)
 
iszero(x::GenSparsePoly) = length(x) = 0

isone(x::GenSparsePoly) = x == 1

length(x::GenSparsePoly) = x.length

lead(x::GenSparsePoly) = x.length == 0 ? base_ring(x)() : x.coeffs[x.length]

trail(x::GenSparsePoly) = x.length == 0 ? base_ring(x)() : x.coeffs[1]

function normalise(a::GenSparsePoly, n::Int)
   while n > 0 && iszero(a.coeffs[n]) 
      n -= 1
   end
   return n
end

base_ring(a::GenSparsePoly) = base_ring(parent(a))

base_ring{T <: RingElem}(R::GenSparsePolyRing{T}) = R.base_ring::parent_type(T)

parent(a::GenSparsePoly) = a.parent

function deepcopy{T <: RingElem}(a::GenSparsePoly{T})
   Re = deepcopy(a.exps)
   Rc = Array(T, a.length)
   for i = 1:a.length
      Rc[i] = deepcopy(a.coeffs[i])
   end
   return parent(a)(Rc, Re)
end

###############################################################################
#
#   String I/O
#
###############################################################################

function show{T <: RingElem}(io::IO, x::GenSparsePoly{T})
    len = length(x)
    U = string(var(parent(x)))
    if len == 0
      print(io, base_ring(x)(0))
    else
      for i = 1:len
        c = coeff(x, len - i)
        bracket = needs_parentheses(c)
        if i != 1 && !isnegative(c)
           print(io, "+")
        end
        X = x.exps[len - i + 1]
        if !isone(c) && (c != -1 || show_minus_one(typeof(c)))
          if bracket
            print(io, "(")
          end
          show(io, c)
          if bracket
            print(io, ")")
          end
          if c != 1 && !(c == -1 && !show_minus_one(typeof(c))) && X != UInt(0)
             print(io, "*")
          end
        end
        if c == -1 && !show_minus_one(typeof(c))
          print(io, "-")
        end
        if X == UInt(0)
          if c == 1
             print(io, c)
          elseif c == -1 && !show_minus_one(typeof(c))
             print(io, 1)
          end
        end
        n = reinterpret(Int, X)
        if n != 0
           print(io, U)
        end
        if n > 1
           print(io, "^", n)
        end
      end
    end
end

function show(io::IO, p::GenSparsePolyRing)
   print(io, "Sparse Univariate Polynomial Ring in ")
   print(io, string(p.S))
   print(io, " over ")
   show(io, base_ring(p))
end

show_minus_one{T <: RingElem}(::Type{GenSparsePoly{T}}) = show_minus_one(T)

needs_parentheses{T <: RingElem}(a::GenSparsePoly{T}) = length(a) > 1

isnegative(x::GenSparsePoly) = length(x) <= 1 && isnegative(coeff(x, 0))

###############################################################################
#
#   Arithmetic functions
#
###############################################################################

function -{T <: RingElem}(a::GenSparsePoly{T})
   r = parent(a)()
   fit!(r, length(a))
   for i = 1:length(a)
      r.coeffs[i] = -a.coeffs[i]
      r.exps[i] = a.exps[i]
   end
   r.length = a.length
   return r
end

function +{T <: RingElem}(a::GenSparsePoly{T}, b::GenSparsePoly{T})
   par = parent(a)
   r = par()
   fit!(r, length(a) + length(b))
   i = 1
   j = 1
   k = 1
   while i <= length(a) && j <= length(b)
      if a.exps[i] < b.exps[j]
         r.coeffs[k] = a.coeffs[i]
         r.exps[k] = a.exps[i]
         i += 1
      elseif a.exps[i] == b.exps[j]
         c = a.coeffs[i] + b.coeffs[j]
         if c != 0
            r.coeffs[k] = c
            r.exps[k] = a.exps[i]
         else
            k -= 1
         end
         i += 1
         j += 1
      else
         r.coeffs[k] = b.coeffs[j]
         r.exps[k] = b.exps[j]
         j += 1
      end
      k += 1
   end
   while i <= length(a)
      r.coeffs[k] = a.coeffs[i]
      r.exps[k] = a.exps[i]
      i += 1
      k += 1
   end
   while j <= length(b)
      r.coeffs[k] = b.coeffs[j]
      r.exps[k] = b.exps[j]
      j += 1
      k += 1
   end
   r.length = k - 1
   return r
end

function -{T <: RingElem}(a::GenSparsePoly{T}, b::GenSparsePoly{T})
   par = parent(a)
   r = par()
   fit!(r, length(a) + length(b))
   i = 1
   j = 1
   k = 1
   while i <= length(a) && j <= length(b)
      if a.exps[i] < b.exps[j]
         r.coeffs[k] = a.coeffs[i]
         r.exps[k] = a.exps[i]
         i += 1
      elseif a.exps[i] == b.exps[j]
         c = a.coeffs[i] - b.coeffs[j]
         if c != 0
            r.coeffs[k] = c
            r.exps[k] = a.exps[i]
         else
            k -= 1
         end
         i += 1
         j += 1
      else
         r.coeffs[k] = -b.coeffs[j]
         r.exps[k] = b.exps[j]
         j += 1
      end
      k += 1
   end
   while i <= length(a)
      r.coeffs[k] = a.coeffs[i]
      r.exps[k] = a.exps[i]
      i += 1
      k += 1
   end
   while j <= length(b)
      r.coeffs[k] = -b.coeffs[j]
      r.exps[k] = b.exps[j]
      j += 1
      k += 1
   end
   r.length = k - 1
   return r
end

immutable heap_sr
   exp::UInt
   n::Int
end

function isless(a::heap_sr, b::heap_sr)
   return a.exp < b.exp
end

function ==(a::heap_sr, b::heap_sr)
   return a.exp == b.exp
end

function heapinsert!(xs::Array{heap_sr, 1}, ys::Array{heap_t, 1}, m::Int, exp::UInt)
   i = n = length(xs) + 1
   @inbounds if i != 1 && exp == xs[1].exp
      ys[m] = heap_t(ys[m].i, ys[m].j, xs[1].n)
      xs[1] = heap_sr(exp, m)
      return
   end
   @inbounds while (j = heapparent(i)) >= 1
      if exp == xs[j].exp
         ys[m] = heap_t(ys[m].i, ys[m].j, xs[j].n)
         xs[j] = heap_sr(exp, m)
         return
      elseif exp < xs[j].exp
         i = j
      else
         break
      end
   end
   push!(xs, heap_sr(exp, 0))
   @inbounds while n > i
      xs[n] = xs[heapparent(n)]
      n >>= 1
   end
   xs[i] = heap_sr(exp, m)
   return
end

function heappop!(xs::Array{heap_sr, 1})
   s = length(xs)
   x = xs[1]
   i = 1
   j = 2
   @inbounds while j < s
      if xs[j].exp >= xs[j + 1].exp
         j += 1
      end
      xs[i] = xs[j]
      i = j
      j *= 2
   end
   exp = xs[s].exp
   j = i >> 1
   @inbounds while i > 1 && exp < xs[j].exp
      xs[i] = xs[j]
      i = j
      j >>= 1
   end
   xs[i] = xs[s]
   pop!(xs)
   return
end

function mul_johnson{T <: RingElem}(a::GenSparsePoly{T}, b::GenSparsePoly{T})
   par = parent(a)
   R = base_ring(par)
   m = length(a)
   n = length(b)
   if m == 0 || n == 0
      return par()
   end
   H = Array(heap_sr, 0)
   I = Array(heap_t, 0)
   # set up heap
   push!(H, heap_sr(a.exps[1] + b.exps[1], 1))
   push!(I, heap_t(1, 1, 0))
   r_alloc = max(m, n) + n
   Rc = Array(T, r_alloc)
   Re = Array(UInt, r_alloc)
   k = 0
   c = R()
   Q = Array(Int, 0)
   while !isempty(H)
      exp = H[1].exp
      k += 1
      if k > r_alloc
         r_alloc *= 2
         resize!(Rc, r_alloc)
         resize!(Re, r_alloc)
      end
      first = true
      @inbounds while !isempty(H) && H[1].exp == exp
         x = H[1]
         heappop!(H)
         v = I[x.n]
         if first
            Rc[k] = a.coeffs[v.i]*b.coeffs[v.j]
            Re[k] = exp
            first = false
         else
            Rc[k] = addmul!(Rc[k], a.coeffs[v.i], b.coeffs[v.j], c)
         end
         if v.j < n || v.j == 1
            push!(Q, x.n)
         end
         while (xn = v.next) != 0
            v = I[xn]
            Rc[k] = addmul!(Rc[k], a.coeffs[v.i], b.coeffs[v.j], c)
            if v.j < n || v.j == 1
               push!(Q, xn)
            end
         end
      end
      @inbounds while !isempty(Q)
         xn = pop!(Q)
         v = I[xn]
         if v.j == 1 && v.i < m
            push!(I, heap_t(v.i + 1, 1, 0))
            Collections.heappush!(H, heap_sr(a.exps[v.i + 1] + b.exps[1], length(I)))       
         end
         if v.j < n
            I[xn] = heap_t(v.i, v.j + 1, 0)
            heapinsert!(H, I, xn, a.exps[v.i] + b.exps[v.j + 1]) # either chain or insert v into heap   
         end
      end
      if Rc[k] == 0
         k -= 1
      end
   end
   resize!(Rc, k)
   resize!(Re, k)
   return parent(a)(Rc, Re)
end

function *{T <: RingElem}(a::GenSparsePoly{T}, b::GenSparsePoly{T})
   m = length(a)
   n = length(b)
   if m == 0 || n == 0
      return parent(a)()
   end
   if m < n
      return b*a
   end
   r = a*b.coeffs[1]
   r = shift_left!(r, b.exps[1])
   for i = 2:n
      s = a*b.coeffs[i]
      s = shift_left!(s, b.exps[i])
      r += s
   end
   return r
end

###############################################################################
#
#   Euclidean division
#
###############################################################################

function divrem{T <: RingElem}(a::GenSparsePoly{T}, b::GenSparsePoly{T})
   par = parent(a)
   R = base_ring(par)
   m = length(a)
   n = length(b)
   maxn = a.exps[m]
   n == 0 && error("Division by zero in divrem_monagan_pearce")
   if a.exps[m] < b.exps[n]
      return par(), a
   end
   H = Array(heap_sr, 0)
   I = Array(heap_t, 0)
   # set up heap
   push!(H, heap_sr(maxn - a.exps[m], 1))
   push!(I, heap_t(0, 1, 0))
   q_alloc = max(m - n, n)
   r_alloc = n
   Qc = Array(T, q_alloc)
   Qe = Array(UInt, q_alloc)
   Rc = Array(T, r_alloc)
   Re = Array(UInt, r_alloc)
   k = 0
   l = 0
   s = n
   c = R()
   qc = R()
   m1 = -R(1)
   mb = -b.coeffs[n]
   Q = Array(Int, 0)
   reuse = Array(Int, 0)
   while !isempty(H)
      exp = H[1].exp
      k += 1
      if k > q_alloc
         q_alloc *= 2
         resize!(Qc, q_alloc)
         resize!(Qe, q_alloc)
      end
      @inbounds while !isempty(H) && H[1].exp == exp
         x = H[1]
         heappop!(H)
         v = I[x.n]
         if v.i == 0
            qc = addmul!(qc, a.coeffs[m + 1 - v.j], m1, c)
         else
            qc = addmul!(qc, b.coeffs[n + 1 - v.i], Qc[v.j], c)
         end
         if v.i != 0 || v.j < m
            push!(Q, x.n)
         else
            push!(reuse, x.n)
         end
         while (xn = v.next) != 0
            v = I[xn]
            if v.i == 0
               qc = addmul!(qc, a.coeffs[m + 1 - v.j], m1, c)
            else
               qc = addmul!(qc, b.coeffs[n + 1 - v.i], Qc[v.j], c)
            end
            if v.i != 0 || v.j < m
               push!(Q, xn)
            else
               push!(reuse, xn)
            end
         end
      end
      @inbounds while !isempty(Q)
         xn = pop!(Q)
         v = I[xn]
         if v.i == 0
            I[xn] = heap_t(0, v.j + 1, 0)
            heapinsert!(H, I, xn, maxn - a.exps[m - v.j]) # either chain or insert into heap   
         elseif v.j < k - 1
            I[xn] = heap_t(v.i, v.j + 1, 0)
            heapinsert!(H, I, xn, maxn - b.exps[n + 1 - v.i] - Qe[v.j + 1]) # either chain or insert into heap
         elseif v.j == k - 1
            s += 1
            push!(reuse, xn)
         end  
      end
      if qc == 0
         k -= 1
      else
         d1 = maxn - b.exps[n] >= exp
         texp = maxn - b.exps[n] - exp
         if !d1
            l += 1
            if l >= r_alloc
               r_alloc *= 2
               resize!(Rc, r_alloc)
               resize!(Re, r_alloc)
            end
            Rc[l] = -qc
            Re[l] = maxn - exp
            k -= 1
         else
            tq, tr = divrem(qc, mb)
            if tr != 0
               l += 1
               if l >= r_alloc
                  r_alloc *= 2
                  resize!(Rc, r_alloc)
                  resize!(Re, r_alloc)
               end
               Rc[l] = -tr
               Re[l] = maxn - exp 
            end
            if tq != 0
               Qc[k] = tq
               Qe[k] = texp
               for i = 2:s
                  if !isempty(reuse)
                     xn = pop!(reuse)
                     I[xn] = heap_t(i, k, 0)
                     heapinsert!(H, I, xn, maxn - b.exps[n + 1 - i] - Qe[k]) # either chain or insert into heap
                  else
                     push!(I, heap_t(i, k, 0))
                     Collections.heappush!(H, heap_sr(maxn - b.exps[n + 1 - i] - Qe[k], length(I)))
                  end
               end                 
               s = 1
            else
               k -= 1
            end
         end
      end
      qc = zero!(qc)
   end
   resize!(Qc, k)
   resize!(Qe, k)
   resize!(Rc, l)
   resize!(Re, l)
   reverse!(Qc)
   reverse!(Qe)
   reverse!(Rc)
   reverse!(Re)
   return parent(a)(Qc, Qe), parent(a)(Rc, Re)
end

###############################################################################
#
#   Ad hoc arithmetic functions
#
###############################################################################

function *{T <: RingElem}(a::GenSparsePoly{T}, n::Integer)
   r = parent(a)()
   fit!(r, length(a))
   j = 1
   for i = 1:length(a)
      c = a.coeffs[i]*n
      if c != 0
         r.coeffs[j] = c 
         r.exps[j] = a.exps[i]
         j += 1
      end
   end
   r.length = j - 1
   return r
end

function *{T <: RingElem}(a::GenSparsePoly{T}, n::fmpz)
   r = parent(a)()
   fit!(r, length(a))
   j = 1
   for i = 1:length(a)
      c = a.coeffs[i]*n
      if c != 0
         r.coeffs[j] = c 
         r.exps[j] = a.exps[i]
         j += 1
      end
   end
   r.length = j - 1
   return r
end

function *{T <: RingElem}(a::GenSparsePoly{T}, n::T)
   r = parent(a)()
   fit!(r, length(a))
   j = 1
   for i = 1:length(a)
      c = a.coeffs[i]*n
      if c != 0
         r.coeffs[j] = c 
         r.exps[j] = a.exps[i]
         j += 1
      end
   end
   r.length = j - 1
   return r
end

*{T <: RingElem}(n::T, a::GenSparsePoly{T}) = a*n

*{T <: RingElem}(n::Integer, a::GenSparsePoly{T}) = a*n

*{T <: RingElem}(n::fmpz, a::GenSparsePoly{T}) = a*n

+{T <: RingElem}(a::GenSparsePoly{T}, b::Integer) = a + parent(a)(b)

+{T <: RingElem}(a::GenSparsePoly{T}, b::fmpz) = a + parent(a)(b)

-{T <: RingElem}(a::GenSparsePoly{T}, b::Integer) = a - parent(a)(b)

-{T <: RingElem}(a::GenSparsePoly{T}, b::fmpz) = a - parent(a)(b)

+{T <: RingElem}(a::Integer, b::GenSparsePoly{T}) = parent(b)(a) + b

+{T <: RingElem}(a::fmpz, b::GenSparsePoly{T}) = parent(b)(a) + b

-{T <: RingElem}(a::Integer, b::GenSparsePoly{T}) = parent(b)(a) - b

-{T <: RingElem}(a::fmpz, b::GenSparsePoly{T}) = parent(b)(a) - b

###############################################################################
#
#   Comparison
#
###############################################################################

function =={T <: RingElem}(a::GenSparsePoly{T}, b::GenSparsePoly{T})
   if length(a) != length(b)
      return false
   end
   for i = 1:length(a)
      if a.exps[i] != b.exps[i] || a.coeffs[i] != b.coeffs[i]
         return false
      end
   end
   return true
end

###############################################################################
#
#   Ad hoc comparison
#
###############################################################################

function =={T <: RingElem}(a::GenSparsePoly{T}, b::Int)
   return length(a) == 0 ? b == 0 : a.length == 1 && 
          a.exps[1] == 0 && a.coeffs[1] == b
end

=={T <: RingElem}(a::Int, b::GenSparsePoly{T}) = b == a

function =={T <: RingElem}(a::GenSparsePoly{T}, b::fmpz)
   return length(a) == 0 ? b == 0 : a.length == 1 &
          a.exps[1] == 0 && a.coeffs[1] == b
end

=={T <: RingElem}(a::fmpz, b::GenSparsePoly{T}) = b == a

###############################################################################
#
#   Powering
#
###############################################################################

function from_exp(R::Ring, a::UInt)
   return R(fmpz(a))
end

# Implement fps algorithm from "Sparse polynomial powering with heaps" by
# Monagan and Pearce, except that we modify the algorithm to return terms
# in ascending order and we fix some issues in the original algorithm
# http://www.cecm.sfu.ca/CAG/papers/SparsePowering.pdf

function pow_fps{T <: RingElem}(f::GenSparsePoly{T}, k::Int)
   par = parent(f)
   R = base_ring(par)
   m = length(f)
   H = Array(heap_sr, 0) # heap
   I = Array(heap_t, 0) # auxilliary data for heap nodes
   # set up output poly coeffs and exponents (corresponds to h in paper)
   r_alloc = k*(m - 1) + 1
   Rc = Array(T, r_alloc)
   Re = Array(UInt, r_alloc)
   rnext = 1
   # set up g coeffs and exponents (corresponds to g in paper)
   g_alloc = k*(m - 1) + 1
   gc = Array(T, g_alloc)
   ge = Array(UInt, g_alloc)
   gnext = 1
   # set up heap
   gc[1] = f.coeffs[1]^(k-1)
   ge[1] = f.exps[1]*(k - 1)
   Rc[1] = f.coeffs[1]*gc[1]
   Re[1] = f.exps[1]*k
   push!(H, heap_sr(f.exps[2] + ge[1], 1))
   push!(I, heap_t(2, 1, 0))
   Q = Array(Int, 0) # corresponds to Q in paper
   topbit = -1 << (sizeof(Int)*8 - 1)
   mask = ~topbit
   largest = fill(topbit, m) # largest j s.t. (i, j) has been in heap
   largest[2] = 1
   # precompute some values
   fik = Array(T, m)
   for i = 1:m
      fik[i] = from_exp(R, f.exps[i])*(k - 1)
   end
   kp1f1 = k*from_exp(R, f.exps[1])
   gi = Array(T, 1)
   gi[1] = -from_exp(R, ge[1])
   finalexp = f.exps[m]*(k - 1) + f.exps[1]
   # temporary space
   t1 = R()
   C = R() # corresponds to C in paper
   SS = R() # corresponds to S in paper
   temp = R() # temporary space for addmul
   temp2 = R() # temporary space for add
   # begin algorithm
   while !isempty(H)
      exp = H[1].exp
      gnext += 1
      rnext += 1
      if gnext > g_alloc
         g_alloc *= 2
         resize!(gc, g_alloc)
         resize!(ge, g_alloc)
      end
      if rnext > r_alloc
         r_alloc *= 2
         resize!(Rc, r_alloc)
         resize!(Re, r_alloc)
      end
      first = true
      C = zero!(C)
      SS = zero!(SS)
      while !isempty(H) && H[1].exp == exp
         x = H[1]
         heappop!(H)
         v = I[x.n]
         largest[v.i] |= topbit
         t1 = mul!(t1, f.coeffs[v.i], gc[v.j])
         SS = addeq!(SS, t1)
         if exp <= finalexp
            temp2 = add!(temp2, fik[v.i], gi[v.j])
            C = addmul!(C, temp2, t1, temp)
         end
         if first
            ge[gnext] = exp - f.exps[1]
            first = false
         end
         push!(Q, x.n)
         while (xn = v.next) != 0
            v = I[xn]
            largest[v.i] |= topbit
            t1 = mul!(t1, f.coeffs[v.i], gc[v.j])
            SS = addeq!(SS, t1)
            if exp <= finalexp
               temp2 = add!(temp2, fik[v.i], gi[v.j])
               C = addmul!(C, temp2, t1, temp)
            end
            push!(Q, xn)
         end
      end
      reuse = 0
      while !isempty(Q)
         xn = pop!(Q)
         v = I[xn]
         if v.i < m && largest[v.i + 1] == ((v.j - 1) | topbit)
            I[xn] = heap_t(v.i + 1, v.j, 0)
            heapinsert!(H, I, xn, f.exps[v.i + 1] + ge[v.j]) # either chain or insert v into heap   
            largest[v.i + 1] = v.j
         else
            reuse = xn
         end
         if v.j < gnext - 1 && (largest[v.i] & mask) <  v.j + 1
            if reuse != 0
               I[reuse] = heap_t(v.i, v.j + 1, 0)
               heapinsert!(H, I, reuse, f.exps[v.i] + ge[v.j + 1]) # either chain or insert v into heap
               reuse = 0   
            else
               push!(I, heap_t(v.i, v.j + 1, 0))
               Collections.heappush!(H, heap_sr(f.exps[v.i] + ge[v.j + 1], length(I)))
            end
            largest[v.i] = v.j + 1     
         end
      end
      if C != 0
         temp = divexact(C, from_exp(R, exp) - kp1f1)
         SS = addeq!(SS, temp)
         gc[gnext] = divexact(temp, f.coeffs[1])
         push!(gi, -from_exp(R, ge[gnext]))
         if (largest[2] & topbit) != 0
            push!(I, heap_t(2, gnext, 0))
            Collections.heappush!(H, heap_sr(f.exps[2] + ge[gnext], length(I)))   
            largest[2] = gnext
         end
      end
      if SS != 0
         Rc[rnext] = SS
         Re[rnext] = ge[gnext] + f.exps[1]
         SS = R()
      else
         rnext -= 1
      end
      if C == 0
         gnext -= 1
      end
   end
   resize!(Rc, rnext)
   resize!(Re, rnext)
   return parent(f)(Rc, Re)
end

function ^{T <: RingElem}(a::GenSparsePoly{T}, b::Int)
   b < 0 && throw(DomainError())
   # special case powers of x for constructing polynomials efficiently
   if length(a) == 0
      return parent(a)()
   elseif length(a) == 1
      return parent(a)([coeff(a, 0)^b], [a.exps[1]*b])
   elseif b == 0
      return parent(a)(1)
   elseif b == 1
      return a
   elseif b == 2
      return a*a
   else
      z = a*a
      for i = 3:b
         z *= a
      end
      return z
   end
end

###############################################################################
#
#   Exact division
#
###############################################################################

function divides_monagan_pearce{T <: RingElem}(a::GenSparsePoly{T}, b::GenSparsePoly{T})
   par = parent(a)
   R = base_ring(par)
   m = length(a)
   n = length(b)
   n == 0 && error("Division by zero in divides_monagan_pearce")
   if m == 0
      return true, par()
   end
   H = Array(heap_sr, 0)
   I = Array(heap_t, 0)
   # set up heap
   push!(H, heap_sr(a.exps[1], 1))
   push!(I, heap_t(0, 1, 0))
   q_alloc = max(m - n, n)
   Qc = Array(T, q_alloc)
   Qe = Array(UInt, q_alloc)
   k = 0
   s = n
   c = R()
   qc = R()
   m1 = -R(1)
   mb = -b.coeffs[1]
   Q = Array(Int, 0)
   reuse = Array(Int, 0)
   while !isempty(H)
      exp = H[1].exp
      k += 1
      if k > q_alloc
         q_alloc *= 2
         resize!(Qc, q_alloc)
         resize!(Qe, q_alloc)
      end
      first = true
      d1 = false
      @inbounds while !isempty(H) && H[1].exp == exp
         x = H[1]
         heappop!(H)
         v = I[x.n]
         if first
            d1 = exp >= b.exps[1]
            Qe[k] = exp - b.exps[1]
            first = false
         end
         if v.i == 0
            qc = addmul!(qc, a.coeffs[v.j], m1, c)
         else
            qc = addmul!(qc, b.coeffs[v.i], Qc[v.j], c)
         end
         if v.i != 0 || v.j < m
            push!(Q, x.n)
         else
            push!(reuse, x.n)
         end
         while (xn = v.next) != 0
            v = I[xn]
            if v.i == 0
               qc = addmul!(qc, a.coeffs[v.j], m1, c)
            else
               qc = addmul!(qc, b.coeffs[v.i], Qc[v.j], c)
            end
            if v.i != 0 || v.j < m
               push!(Q, xn)
            else
               push!(reuse, xn)
            end
         end
      end
      @inbounds while !isempty(Q)
         xn = pop!(Q)
         v = I[xn]
         if v.i == 0
            I[xn] = heap_t(0, v.j + 1, 0)
            heapinsert!(H, I, xn, a.exps[v.j + 1]) # either chain or insert into heap   
         elseif v.j < k - 1
            I[xn] = heap_t(v.i, v.j + 1, 0)
            heapinsert!(H, I, xn, b.exps[v.i] + Qe[v.j + 1]) # either chain or insert into heap
         elseif v.j == k - 1
            s += 1
            push!(reuse, xn)
         end  
      end
      if qc == 0
         k -= 1
      else
         d2, Qc[k] = divides(qc, mb)
         if !d1 || !d2
             return false, par()
         end
         for i = 2:s
            if !isempty(reuse)
               xn = pop!(reuse)
               I[xn] = heap_t(i, k, 0)
               heapinsert!(H, I, xn, b.exps[i] + Qe[k]) # either chain or insert into heap
            else
               push!(I, heap_t(i, k, 0))
               Collections.heappush!(H, heap_sr(b.exps[i] + Qe[k], length(I)))
            end                 
         end
         s = 1
      end
      qc = zero!(qc)
   end
   resize!(Qc, k)
   resize!(Qe, k)
   return true, parent(a)(Qc, Qe)
end

function divides{T <: RingElem}(a::GenSparsePoly{T}, b::GenSparsePoly{T})
   d1 = a.exps[a.length]
   d2 = b.exps[b.length] - b.exps[1]
   q_alloc = b.length
   Qe = Array(UInt, q_alloc)
   Qc = Array(T, q_alloc)
   r = a
   k = 0
   while length(r) > 0
      if r.exps[1] + d2 > d1
         return false, parent(a)()
      end
      flag, c = divides(r.coeffs[1], b.coeffs[1])
      if !flag
         return false, parent(a)()
      end
      s = b*c
      if r.exps[1] < b.exps[1]
         return false, parent(a)()
      end
      d = r.exps[1] - b.exps[1]
      k += 1
      if k > q_alloc
         q_alloc *= 2
         resize!(Qe, q_alloc)
         resize!(Qc, q_alloc)
      end
      Qe[k] = d
      Qc[k] = c
      s = shift_left!(s, d)
      r -= s
   end
   resize!(Qe, k)
   resize!(Qc, k)
   return true, parent(a)(Qc, Qe)
end

function divexact{T <: RingElem}(a::GenSparsePoly{T}, b::GenSparsePoly{T})
   d, q = divides(a, b)
   d == false && error("Not an exact division in divexact")
   return q
end

###############################################################################
#
#   Ad hoc exact division
#
###############################################################################

function divides{T <: RingElem}(a::GenSparsePoly{T}, b::T)
   len = a.length
   Qc = Array(T, len)
   for i = 1:len
      flag, Qc[i] = divides(a.coeffs[i], b)
      if !flag
         return false, parent(a)()
      end
   end
   return true, parent(a)(Qc, a.exps)
end

function divexact{T <: RingElem}(a::GenSparsePoly{T}, b::T)
   len = length(a)
   exps = deepcopy(a.exps)
   coeffs = [divexact(a.coeffs[i], b) for i in 1:len]
   return parent(a)(coeffs, exps)
end

###############################################################################
#
#   Pseudodivision
#
###############################################################################

function pseudodivrem{T <: RingElem}(a::GenSparsePoly{T}, b::GenSparsePoly{T})
   par = parent(a)
   R = base_ring(par)
   m = length(a)
   n = length(b)
   maxn = a.exps[m]
   n == 0 && error("Division by zero in pseudodivrem")
   if a.exps[m] < b.exps[n]
      return par(), a
   end
   H = Array(heap_sr, 0)
   I = Array(heap_t, 0)
   # set up heap
   push!(H, heap_sr(maxn - a.exps[m], 1))
   push!(I, heap_t(0, 1, 0))
   q_alloc = max(m - n + 1, n)
   r_alloc = n
   p_alloc = max(m - n + 1, 1)
   Qc = Array(T, q_alloc)
   Qe = Array(UInt, q_alloc)
   Qpow = Array(Int, q_alloc)
   Rc = Array(T, r_alloc)
   Re = Array(UInt, r_alloc)
   Pc = Array(T, p_alloc)
   Pc[1] = b.coeffs[n]
   p = -1 # current power of lead(b)
   p_max = 1
   k = 0
   l = 0
   s = n
   c = R()
   m1 = -R(1)
   mb = -b.coeffs[n]
   Q = Array(Int, 0)
   reuse = Array(Int, 0)
   while !isempty(H)
      exp = H[1].exp
      qc = R()
      k += 1
      if k > q_alloc
         q_alloc *= 2
         resize!(Qc, q_alloc)
         resize!(Qe, q_alloc)
         resize!(Qpow, q_alloc)
      end
      p += 1
      if p > p_alloc
         p_alloc *= 2
         resize!(Pc, p_alloc)
      end
      if p > p_max
         Pc[p] = Pc[p - 1]*b.coeffs[n]
         p_max = p
      end
      @inbounds while !isempty(H) && H[1].exp == exp
         x = H[1]
         heappop!(H)
         v = I[x.n]
         if v.i == 0
            # cannot use mul! here since Pc[p] is one variable less
            if p > 0
               tc = a.coeffs[m + 1 - v.j]*(-Pc[p])
            else
               tc = -a.coeffs[m + 1 - v.j]
            end
            qc = addeq!(qc, tc)
         else
            if p - Qpow[v.j] - 1 > 0
               tc = Qc[v.j]*Pc[p - Qpow[v.j] - 1]
            else
               tc = Qc[v.j]
            end
            qc = addmul!(qc, b.coeffs[n + 1 - v.i], tc, c)
         end
         if v.i != 0 || v.j < m
            push!(Q, x.n)
         else
            push!(reuse, x.n)
         end
         while (xn = v.next) != 0
            v = I[xn]
            if v.i == 0
               if p > 0
                  tc = a.coeffs[m + 1 - v.j]*(-Pc[p])
               else
                  tc = -a.coeffs[m + 1 - v.j]
               end
               qc = addeq!(qc, tc)
            else
               if p - Qpow[v.j] - 1 > 0
                  tc = Qc[v.j]*Pc[p - Qpow[v.j] - 1]
               else
                  tc = Qc[v.j]
               end
               qc = addmul!(qc, b.coeffs[n + 1 - v.i], tc, c)
            end
            if v.i != 0 || v.j < m
               push!(Q, xn)
            else
               push!(reuse, xn)
            end
         end
      end
      @inbounds while !isempty(Q)
         xn = pop!(Q)
         v = I[xn]
         if v.i == 0
            I[xn] = heap_t(0, v.j + 1, 0)
            heapinsert!(H, I, xn, maxn - a.exps[m - v.j]) # either chain or insert into heap   
         elseif v.j < k - 1
            I[xn] = heap_t(v.i, v.j + 1, 0)
            heapinsert!(H, I, xn, maxn - b.exps[n + 1 - v.i] - Qe[v.j + 1]) # either chain or insert into heap
         elseif v.j == k - 1
            s += 1
            push!(reuse, xn)
         end  
      end
      if qc == 0
         k -= 1
         p -= 1
      else
         d1 = maxn - b.exps[n] >= exp
         texp = maxn - b.exps[n] - exp
         if !d1
            l += 1
            if l >= r_alloc
               r_alloc *= 2
               resize!(Rc, r_alloc)
               resize!(Re, r_alloc)
            end
            Rc[l] = -qc
            Re[l] = maxn - exp
            k -= 1
            p -= 1
         else
            Qc[k] = -qc
            Qe[k] = texp
            Qpow[k] = p
            for i = 2:s
               if !isempty(reuse)
                  xn = pop!(reuse)
                  I[xn] = heap_t(i, k, 0)
                  heapinsert!(H, I, xn, maxn - b.exps[n + 1 - i] - Qe[k]) # either chain or insert into heap
               else
                  push!(I, heap_t(i, k, 0))
                  Collections.heappush!(H, heap_sr(maxn - b.exps[n + 1 - i] - Qe[k], length(I)))
               end
            end                 
            s = 1
         end
      end
   end
   resize!(Qc, k)
   resize!(Qe, k)
   for i = 1:k
      if p > Qpow[i]
         Qc[i] = mul!(Qc[i], Qc[i], Pc[p - Qpow[i]])
      else
         Qc[i] = Qc[i]
      end
   end
   resize!(Rc, l)
   resize!(Re, l)
   reverse!(Qc)
   reverse!(Qe)
   reverse!(Rc)
   reverse!(Re)
   if a.exps[m] - b.exps[n] > p
      k1 = reinterpret(Int, a.exps[m] - b.exps[n] - p)
      p1 = b.coeffs[n]^k1
      return parent(a)(Qc, Qe)*p1, parent(a)(Rc, Re)*p1
   else
      return parent(a)(Qc, Qe), parent(a)(Rc, Re)
   end
end

function pseudorem_monagan_pearce{T <: RingElem}(a::GenSparsePoly{T}, b::GenSparsePoly{T})
   par = parent(a)
   R = base_ring(par)
   m = length(a)
   n = length(b)
   maxn = a.exps[m]
   n == 0 && error("Division by zero in pseudodivrem")
   if a.exps[m] < b.exps[n]
      return a
   end
   H = Array(heap_sr, 0)
   I = Array(heap_t, 0)
   # set up heap
   push!(H, heap_sr(maxn - a.exps[m], 1))
   push!(I, heap_t(0, 1, 0))
   q_alloc = max(m - n + 1, n)
   r_alloc = n
   p_alloc = max(m - n + 1, 1)
   Qc = Array(T, q_alloc)
   Qe = Array(UInt, q_alloc)
   Qpow = Array(Int, q_alloc)
   Rc = Array(T, r_alloc)
   Re = Array(UInt, r_alloc)
   Pc = Array(T, p_alloc)
   Pc[1] = b.coeffs[n]
   p = -1 # current power of lead(b)
   p_max = 1
   k = 0
   l = 0
   s = n
   c = R()
   m1 = -R(1)
   mb = -b.coeffs[n]
   Q = Array(Int, 0)
   reuse = Array(Int, 0)
   while !isempty(H)
      exp = H[1].exp
      qc = R()
      k += 1
      if k > q_alloc
         q_alloc *= 2
         resize!(Qc, q_alloc)
         resize!(Qe, q_alloc)
         resize!(Qpow, q_alloc)
      end
      p += 1
      if p > p_alloc
         p_alloc *= 2
         resize!(Pc, p_alloc)
      end
      if p > p_max
         Pc[p] = Pc[p - 1]*b.coeffs[n]
         p_max = p
      end
      @inbounds while !isempty(H) && H[1].exp == exp
         x = H[1]
         heappop!(H)
         v = I[x.n]
         if v.i == 0
            # cannot use mul! here since Pc[p] is one variable less
            if p > 0
               tc = a.coeffs[m + 1 - v.j]*(-Pc[p])
            else
               tc = -a.coeffs[m + 1 - v.j]
            end
            qc = addeq!(qc, tc)
         else
            if p - Qpow[v.j] - 1 > 0
               tc = Qc[v.j]*Pc[p - Qpow[v.j] - 1]
            else
               tc = Qc[v.j]
            end
            qc = addmul!(qc, b.coeffs[n + 1 - v.i], tc, c)
         end
         if v.i != 0 || v.j < m
            push!(Q, x.n)
         else
            push!(reuse, x.n)
         end
         while (xn = v.next) != 0
            v = I[xn]
            if v.i == 0
               if p > 0
                  tc = a.coeffs[m + 1 - v.j]*(-Pc[p])
               else
                  tc = -a.coeffs[m + 1 - v.j]
               end
               qc = addeq!(qc, tc)
            else
               if p - Qpow[v.j] - 1 > 0
                  tc = Qc[v.j]*Pc[p - Qpow[v.j] - 1]
               else
                  tc = Qc[v.j]
               end
               qc = addmul!(qc, b.coeffs[n + 1 - v.i], tc, c)
            end
            if v.i != 0 || v.j < m
               push!(Q, xn)
            else
               push!(reuse, xn)
            end
         end
      end
      @inbounds while !isempty(Q)
         xn = pop!(Q)
         v = I[xn]
         if v.i == 0
            I[xn] = heap_t(0, v.j + 1, 0)
            heapinsert!(H, I, xn, maxn - a.exps[m - v.j]) # either chain or insert into heap   
         elseif v.j < k - 1
            I[xn] = heap_t(v.i, v.j + 1, 0)
            heapinsert!(H, I, xn, maxn - b.exps[n + 1 - v.i] - Qe[v.j + 1]) # either chain or insert into heap
         elseif v.j == k - 1
            s += 1
            push!(reuse, xn)
         end  
      end
      if qc == 0
         k -= 1
         p -= 1
      else
         d1 = maxn - b.exps[n] >= exp
         texp = maxn - b.exps[n] - exp
         if !d1
            l += 1
            if l >= r_alloc
               r_alloc *= 2
               resize!(Rc, r_alloc)
               resize!(Re, r_alloc)
            end
            Rc[l] = -qc
            Re[l] = maxn - exp
            k -= 1
            p -= 1
         else
            Qc[k] = -qc
            Qe[k] = texp
            Qpow[k] = p
            for i = 2:s
               if !isempty(reuse)
                  xn = pop!(reuse)
                  I[xn] = heap_t(i, k, 0)
                  heapinsert!(H, I, xn, maxn - b.exps[n + 1 - i] - Qe[k]) # either chain or insert into heap
               else
                  push!(I, heap_t(i, k, 0))
                  Collections.heappush!(H, heap_sr(maxn - b.exps[n + 1 - i] - Qe[k], length(I)))
               end
            end                 
            s = 1
         end
      end
   end
   resize!(Rc, l)
   resize!(Re, l)
   reverse!(Rc)
   reverse!(Re)
   if a.exps[m] - b.exps[n] > p
      k1 = reinterpret(Int, a.exps[m] - b.exps[n] - p)
      p1 = b.coeffs[n]^k1
      return parent(a)(Rc, Re)*p1
   else
      return parent(a)(Rc, Re)
   end
end

function pseudorem{T <: RingElem}(a::GenSparsePoly{T}, b::GenSparsePoly{T})
   par = parent(a)
   R = base_ring(par)
   m = length(a)
   n = length(b)
   n == 0 && throw(DivideError())
   if a.exps[a.length] < b.exps[b.length]
      return deepcopy(a)
   end
   k = reinterpret(Int, a.exps[a.length] - b.exps[b.length]) + 1
   l = lead(b)
   while a.length > 0 && a.exps[a.length] >= b.exps[b.length]
      s = lead(a)*b
      s = shift_left!(s, a.exps[a.length] - b.exps[b.length])
      a = a*l - s
      k -= 1
   end
   return a*l^k
end

###############################################################################
#
#   Evaluation
#
###############################################################################

function evaluate{S <: RingElem, T <: RingElem}(a::GenSparsePoly{T}, b::S)
   if a.length == 0
      return a
   end
   r = a.coeffs[a.length]
   for i = 1:a.length - 1
      r *= b^(reinterpret(Int, a.exps[a.length - i + 1] - a.exps[a.length - i]))
      r += a.coeffs[a.length - i]
   end
   return r
end

function evaluate{T <: RingElem}(a::GenSparsePoly{T}, b::Integer)
   if a.length == 0
      return a
   end
   r = a.coeffs[a.length]
   for i = 1:a.length - 1
      r *= fmpz(b)^(reinterpret(Int, a.exps[a.length - i + 1] - a.exps[a.length - i]))
      r += a.coeffs[a.length - i]
   end
   return r
end

###############################################################################
#
#   GCD, content and primitive part
#
###############################################################################

# Evaluate the coefficients of the polynomials at random points and try to work
# out the likely degree of the gcd of the two input polys
function gcd_likely_degree{T <: RingElem}(a::GenSparsePoly{GenMPoly{T}}, b::GenSparsePoly{GenMPoly{T}})
   N = base_ring(a).N
   if a.length == 0
      if b.length == 0
         return 0
      else
         return reinterpret(Int, b.exps[b.length])
      end
   elseif b.length == 0
      return reinterpret(Int, a.exps[a.length])
   end
   # check we are not in the univariate case, to prevent infinite recursion
   constant_coeffs = true
   for i = 1:a.length
      if !isconstant(a.coeffs[i])
         constant_coeffs = false
         break
      end
   end
   if constant_coeffs
      return 0
   end   
   # try to find two evaluations of a and b with the same gcd
   num_good = 0
   iter = 0
   len = 0
   V = base_ring(a)
   R = base_ring(base_ring(a))
   U = elem_type(R)
   M = Array(Array{U, 1}, 0)
   while num_good < 2 && iter < 10
      i = 1
      A = Array(U, 0)
      # find a new set of points to evaluate at
      while i < 10
         A = [R(rand(-10:10)) for i in 1:N]
         newvals = true
         for j = 1:length(M)
            if A == M[j]
               newvals = false
               break
            end
         end
         if newvals
            break
         end
         i += 1
      end
      if i == 10
         return 0 # unable to find distinct evaluation points
      end
      # check none of the coefficients became zero when evaluated
      Ac = [V(evaluate(a.coeffs[i], A)) for i in 1:a.length]
      Bc = [V(evaluate(b.coeffs[i], A)) for i in 1:b.length]
      is_degen = false
      for i = 1:a.length
         if Ac[i] == 0
            is_degen = true
         end
      end
      for i = 1:b.length
         if Bc[i] == 0
            is_degen = true
         end
      end
      # take gcd of evaluated polys, check if deg of evaluated gcd has gone up
      if !is_degen
         a1 = parent(a)(Ac, a.exps)
         b1 = parent(b)(Bc, b.exps)
         g = gcd(a1, b1, true)
         glen = g.length == 0 ? 0 : reinterpret(Int, g.exps[g.length])
         if glen > len
            len = glen
            num_good = 1
            resize!(M, 0)
            push!(M, A)
         elseif glen == len && glen != 0
            num_good += 1
            push!(M, A)
         end
      end
      iter += 1
   end
   if iter == 10
      return 0 # too many iterations - shouldn't happen
   else
      return len # probably length of the multivariate gcd
   end
end

function gcd{T <: RingElem}(a::GenSparsePoly{T}, b::GenSparsePoly{T}, ignore_content=false)
   # ensure degree in main variable of a is at least that of b
   if b.exps[b.length] > a.exps[a.length]
      (a, b) = (b, a)
   end
   if b == 0
      return deepcopy(a)
   end
   if b == 1
      return deepcopy(b)
   end
   # compute gcd of contents and divide content out
   if !ignore_content
      c1 = content(a)
      c2 = content(b)
      c = gcd(c1, c2)
      a = divexact(a, c1)
      b = divexact(b, c2)
   end
   # check if we are in the univariate case
   constant_coeffs = true
   for i = 1:a.length
      if !isconstant(a.coeffs[i])
         constant_coeffs = false
         break
      end
   end
   if constant_coeffs
      for i = 1:b.length
         if !isconstant(b.coeffs[i])
            constant_coeffs = false
            break
         end
      end
   end
   # if we are in univariate case, convert to dense, take gcd, convert back
   if constant_coeffs
      # convert polys to univariate dense
      R, x = PolynomialRing(base_ring(base_ring(a)), "\$")
      f = R()
      g = R()
      fit!(f, reinterpret(Int, a.exps[a.length] + 1))
      fit!(g, reinterpret(Int, b.exps[b.length] + 1))
      for i = 1:a.length
         f = setcoeff!(f, reinterpret(Int, a.exps[i]), a.coeffs[i].coeffs[1])
      end
      for i = 1:b.length
         g = setcoeff!(g, reinterpret(Int, b.exps[i]), b.coeffs[i].coeffs[1])
      end
      # take gcd of univariate dense polys
      h = gcd(f, g)
      # convert back to sparse polys
      nonzero = 0
      Ac = Array(T, 0)
      Ae = Array(UInt, 0)
      for i = 1:length(h)
         ci = coeff(h, i - 1)
         if ci != 0
            push!(Ac, base_ring(a)(ci))
            push!(Ae, UInt(i - 1))
         end
      end
      r = parent(a)(Ac, Ae)
      if !ignore_content
         return c*r
      else
         return r
      end
   end
   # compute likely degree of gcd
   deg = 0 # gcd_likely_degree(a, b)
   # is the lead/trail term a monomial
   lead_monomial = lead(a).length == 1 || lead(b).length == 1
   trail_monomial = trail(a).length == 1 || trail(b).length == 1
   lead_a = lead(a)
   lead_b = lead(b)
   # psr algorithm
   g = one(base_ring(a))
   h = one(base_ring(a))
   while true
      adeg = reinterpret(Int,  a.exps[a.length])
      bdeg = reinterpret(Int,  b.exps[b.length])
      # optimisation: if degree of b is equal to likely degree of gcd, try
      # exact division, learned from Bernard Parisse
      if bdeg == deg
         flag, q = divides(a, b)
         if flag
            break
         end
      end
      d = reinterpret(Int, adeg - bdeg)
      r = pseudorem(a, b)
      # zero remainder
      if r == 0
         break
      end
      # constant remainder
      if r.length == 1 && r.exps[1] == 0
         b = one(parent(a))
         break
      end
      (a, b) = (b, divexact(r, g*h^d))
      g = lead(a)
      if d > 1
         h = divexact(g^d, h^(d - 1))
      else
         h = h^(1 - d)*g^d
      end
   end
   # sometimes don't care about content, e.g. when computing likely gcd degree
   if !ignore_content
      # remove content from b as cheaply as possible, as per Bernard Parisse
      if lead(b).length != 1 && trail(b).length != 1
         if lead_monomial # lead term monomial, so content contains rest
            d = divexact(lead(b), term_content(lead(b)))
            b = divexact(b, d)
         elseif trail_monomial # trail term is monomial, so ditto
            d = divexact(trail(b), term_content(trail(b)))
            b = divexact(b, d)
         else 
            glead = gcd(lead_a, lead_b)
            if glead.length == 1 # gcd of lead coeffs monomial
               d = divexact(lead(b), term_content(lead(b)))
               b = divexact(b, d)
            else # last ditched attempt to find easy content
               h = gcd(lead(b), glead)
               h = divexact(h, term_content(h))
               flag, q = divides(b, h)
               if flag
                  b = q
               end
            end
         end
      end
      # remove any monomial content
      b1 = term_content(b.coeffs[1])
      for i = 2:b.length
         b1 = gcd(b1, term_content(b.coeffs[i]))
         if isone(b1)
            break
         end
      end
      b = divexact(b, b1)
      # remove any stubborn content and put back actual content
      return c*primpart(b)
   else
      return b
   end
end

function content{T <: RingElem}(a::GenSparsePoly{T})
   for i = 1:length(a)
      if a.coeffs[i].length == 1
         z = term_content(a.coeffs[1])
         for j = 2:length(a)
            if isone(z)
               return z
            end
            z = gcd(z, term_content(a.coeffs[j]))
         end
         return z
      end
   end
   z = coeff(a, 0)
   for i = 2:length(a)
      if z == 1
         break
      end
      z = gcd(coeff(a, i - 1), z)
   end
   return z
end

function primpart{T <: RingElem}(a::GenSparsePoly{T})
   d = content(a)
   return divexact(a, d)
end

###############################################################################
#
#   Unsafe functions
#
###############################################################################

function fit!{T <: RingElem}(a::GenSparsePoly{T}, n::Int)
   if length(a.coeffs) < n
      resize!(a.coeffs, n)
      resize!(a.exps, n)
   end
   return nothing
end

function addmul!{T <: RingElem}(a::GenSparsePoly{T}, b::GenSparsePoly{T}, c::GenSparsePoly{T}, d::GenSparsePoly{T})
   t = b*c
   t += a
   a.coeffs = t.coeffs
   a.exps = t.exps
   a.length = t.length
   return a
end

function mul!{T <: RingElem}(a::GenSparsePoly{T}, b::GenSparsePoly{T}, c::GenSparsePoly{T})
   t = b*c
   a.coeffs = t.coeffs
   a.exps = t.exps
   a.length = t.length
   return a
end

function addeq!{T <: RingElem}(a::GenSparsePoly{T}, b::GenSparsePoly{T})
   t = a + b
   a.coeffs = t.coeffs
   a.exps = t.exps
   a.length = t.length
   return a
end

function add!{T <: RingElem}(a::GenSparsePoly{T}, b::GenSparsePoly{T}, c::GenSparsePoly{T})
   t = b + c
   a.coeffs = t.coeffs
   a.exps = t.exps
   a.length = t.length
   return a
end

function zero!{T <: RingElem}(a::GenSparsePoly{T})
   a.length = 0
   return a
end

function shift_left!{T <: RingElem}(a::GenSparsePoly{T}, n::UInt)
   for i = 1:length(a)
      a.exps[i] += n
   end
   return a
end

###############################################################################
#
#   Promotion rules
#
###############################################################################

promote_rule{T <: RingElem, V <: Integer}(::Type{GenSparsePoly{T}}, ::Type{V}) = GenSparsePoly{T}

promote_rule{T <: RingElem}(::Type{GenSparsePoly{T}}, ::Type{T}) = GenSparsePoly{T}

function promote_rule1{T <: RingElem, U <: RingElem}(::Type{GenSparsePoly{T}}, ::Type{GenSparsePoly{U}})
   promote_rule(T, GenSparsePoly{U}) == T ? GenSparsePoly{T} : Union{}
end

function promote_rule{T <: RingElem, U <: RingElem}(::Type{GenSparsePoly{T}}, ::Type{U})
   promote_rule(T, U) == T ? GenSparsePoly{T} : promote_rule1(U, GenSparsePoly{T})
end

###############################################################################
#
#   Parent object call overload
#
###############################################################################

function (a::GenSparsePolyRing{T}){T <: RingElem}(b::RingElem)
   return a([base_ring(a)(b)], [UInt(0)])
end

function (a::GenSparsePolyRing{T}){T <: RingElem}()
   z = GenSparsePoly{T}()
   z.parent = a
   return z
end

function (a::GenSparsePolyRing{T}){T <: RingElem}(b::Integer)
   z = GenSparsePoly{T}(base_ring(a)(b))
   z.parent = a
   return z
end

function (a::GenSparsePolyRing{T}){T <: RingElem}(b::T)
   parent(b) != base_ring(a) && error("Unable to coerce to polynomial")
   z = GenSparsePoly{T}(b)
   z.parent = a
   return z
end

function (a::GenSparsePolyRing{T}){T <: RingElem}(b::GenSparsePoly{T})
   parent(b) != a && error("Unable to coerce polynomial")
   return b
end

function (a::GenSparsePolyRing{T}){T <: RingElem}(b::Array{T, 1}, m::Array{UInt, 1})
   if length(b) > 0
      parent(b[1]) != base_ring(a) && error("Unable to coerce to polynomial")
   end
   z = GenSparsePoly{T}(b, m)
   z.parent = a
   return z
end

###############################################################################
#
#   PolynomialRing constructor
#
###############################################################################

doc"""
    SparsePolynomialRing(R::Ring, s::String; cached::Bool = true)
> Given a base ring `R` and a string `s` specifying how the generator
> (variable) should be printed, return a tuple `S, x` representing the new
> polynomial ring $T = R[x1, x2, ...]$ and the generator $x$ of the polynomial
> ring. By default the parent object `T` will depend only on `R` and `x` and
> will be cached. Setting the optional argument `cached` to `false` will
> prevent the parent object `T` from being cached.
"""
function SparsePolynomialRing(R::Ring, s::String; cached::Bool = true)
   U = Symbol(s)
   T = elem_type(R)

   par = GenSparsePolyRing{T}(R, U, cached)

   return par, gen(par)
end
