###############################################################################
#
#   MPoly.jl : Generic sparse distributed multivariate polynomials over rings
#
###############################################################################

export GenMPoly, GenMPolyRing, max_degrees, gens, divides,
       main_variable_extract, main_variable_insert

export NewInt

import Base: min

type NewIntParent <: Ring
end

zz = NewIntParent()

immutable NewInt <: RingElem
   d::Int
   NewInt(a::Int) = new(a)
   NewInt() = new(0)
end

parent(a::NewInt) = zz

+(a::NewInt, b::NewInt) = NewInt(a.d + b.d)

*(a::NewInt, b::NewInt) = NewInt(a.d*b.d)

-(a::NewInt) = NewInt(-a)

+(a::NewInt, b::Int) = NewInt(a.d + b)

+(a::Int, b::NewInt) = NewInt(a + b.d)

*(a::NewInt, b::Int) = NewInt(a.d*b)

*(a::Int, b::NewInt) = NewInt(a*b.d)

function mul!(a::NewInt, b::NewInt, c::NewInt)
   a.d = b.d*c.d
end

function addeq!(a::NewInt, b::NewInt)
   a.d += b.d
   return
end

function addmul!(a::NewInt, b::NewInt, c::NewInt, d::NewInt)
   NewInt(a.d + b.d*c.d)
end

function (a::NewIntParent)()
   return NewInt(0)
end

function (a::NewIntParent)(b::Int)
   return NewInt(b)
end

elem_type(::Nemo.NewIntParent) = NewInt
 
parent_type(::Type{Nemo.NewInt}) = NewIntParent

needs_parentheses(::Nemo.NewInt) = false

isone(a::Nemo.NewInt) = a.d == 1

iszero(a::Nemo.NewInt) = a.d == 0

base_ring(a::Nemo.NewInt) = Union{}

base_ring(a::Nemo.NewIntParent) = Union{}

==(a::NewInt, b::Int) = a.d == b

==(a::Int, b::NewInt) = a == b.d

==(a::NewInt, b::NewInt) = a.d == b.d

isnegative(a::Nemo.NewInt) = a.d < 0

###############################################################################
#
#   Data type and parent object methods
#
###############################################################################

parent_type{T, S, N}(::Type{GenMPoly{T, S, N}}) = GenMPolyRing{T, S, N}

elem_type{T <: RingElem, S, N}(::GenMPolyRing{T, S, N}) = GenMPoly{T, S, N}

vars(a::GenMPolyRing) = a.S

function gens{T <:RingElem, S, N}(a::GenMPolyRing{T, S, N})
   if S == :lex
      return [a([base_ring(a)(1)], [tuple([UInt(i == j) for j in 1:a.num_vars]...)])
           for i in 1:a.num_vars]
   elseif S == :deglex
      return [a([base_ring(a)(1)], [tuple(UInt(1), [UInt(i == j) for j in 1:a.num_vars]...)])
           for i in 1:a.num_vars]
   elseif S == :revlex
      return [a([base_ring(a)(1)], [tuple([UInt(N - i + 1 == j) for j in 1:a.num_vars]...)])
           for i in 1:a.num_vars]
   else # S == :degrevlex
      return [a([base_ring(a)(1)], [tuple(UInt(1), [UInt(N - i == j) for j in 1:a.num_vars]...)])
           for i in 1:a.num_vars]
   end
end

###############################################################################
#
#   Monomial operations
#
###############################################################################

function divides{N}(a::NTuple{N, UInt}, b::NTuple{N, UInt}, mask::UInt)
   diff = ntuple(i -> reinterpret(UInt, reinterpret(Int, a[i])
                    - reinterpret(Int, b[i])), Val{N})
   for i = 1:N
      if (diff[i] & mask) != 0
         return false, diff
      end
   end
   return true, diff
end

function cmp{T <: RingElem, S, N}(a::NTuple{N, UInt},
                                  b::NTuple{N, UInt}, R::GenMPolyRing{T, S, N})
   i = 1
   while i < N && a[i] == b[i]
      i += 1
   end
   return reinterpret(Int, a[i] - b[i])
end

function max_degrees{T <: RingElem, S, N}(f::GenMPoly{T, S, N})
   biggest = zeros(Int, N)
   A = f.exps
   for i = 1:length(f)
      v = A[i]
      for j = 1:N
         if reinterpret(Int, v[j]) > biggest[j]
            biggest[j] = reinterpret(Int, v[j])
         end
      end
   end
   b = biggest[1]
   for i = 2:N
      if biggest[i] > b
         b = biggest[i]
      end
   end
   return biggest, b
end

###############################################################################
#
#   Basic manipulation
#
###############################################################################

function coeff(x::GenMPoly, i::Int)
   i < 0 && throw(DomainError())
   return x.coeffs[i + 1]
end

length(x::GenMPoly) = x.length

num_vars(x::GenMPoly) = parent(x).num_vars

isone(x::GenMPoly) = x.length == 1 && iszero(x.exps[1]) && x.coeffs[1] == 1

iszero(x::GenMPoly) = x.length == 0

isconstant(x::GenMPoly) = x.length == 0 || (x.length == 1 && iszero(x.exps[1]))

function normalise(a::GenMPoly, n::Int)
   while n > 0 && iszero(a.coeffs[n]) 
      n -= 1
   end
   return n
end

function deepcopy{T <: RingElem, S, N}(a::GenMPoly{T, S, N})
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

function show{T <: RingElem, S, N}(io::IO, x::GenMPoly{T, S, N})
    len = length(x)
    U = [string(x) for x in vars(parent(x))]
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
        if (S == :revlex || S == :degrevlex)
           X = reverse(X)
        end
        if !isone(c) && (c != -1 || show_minus_one(typeof(c)))
          if bracket
            print(io, "(")
          end
          show(io, c)
          if bracket
            print(io, ")")
          end
          if c != 1 && !(c == -1 && !show_minus_one(typeof(c))) && X != zero(NTuple{N, UInt})
             print(io, "*")
          end
        end
        if c == -1 && !show_minus_one(typeof(c))
          print(io, "-")
        end
        d = (S == :deglex) ? 1 : 0
        if X == zero(NTuple{N, UInt})
          if c == 1
             print(io, c)
          elseif c == -1 && !show_minus_one(typeof(c))
             print(io, 1)
          end
        end
        fst = true
        for j = 1:num_vars(x)
          n = reinterpret(Int, X[j + d])
          if n != 0
            if fst
               print(io, U[j])
               fst = false
            else
               print(io, "*", U[j])
            end
            if n != 1
              print(io, "^", n)
            end
          end
        end      
    end
  end
end

function show(io::IO, p::GenMPolyRing)
   const max_vars = 5 # largest number of variables to print
   n = p.num_vars
   print(io, "Multivariate Polynomial Ring in ")
   if n > max_vars
      print(io, p.num_vars)
      print(io, " variables ")
   end
   for i = 1:min(n - 1, max_vars - 1)
      print(io, string(p.S[i]), ", ")
   end
   if n > max_vars
      print(io, "..., ")
   end
   print(io, string(p.S[n]))
   print(io, " over ")
   show(io, base_ring(p))
end

show_minus_one{T <: RingElem, S, N}(::Type{GenMPoly{T, S, N}}) = show_minus_one(T)

needs_parentheses(x::GenMPoly) = length(x) > 1

isnegative(x::GenMPoly) = length(x) == 1 && iszero(x.exps[1]) && isnegative(x.coeffs[1])

###############################################################################
#
#   Arithmetic functions
#
###############################################################################

function -{T <: RingElem, S, N}(a::GenMPoly{T, S, N})
   r = parent(a)()
   fit!(r, length(a))
   for i = 1:length(a)
      r.coeffs[i] = -a.coeffs[i]
      r.exps[i] = a.exps[i]
   end
   r.length = a.length
   return r
end

function +{T <: RingElem, S, N}(a::GenMPoly{T, S, N}, b::GenMPoly{T, S, N})
   par = parent(a)
   r = par()
   fit!(r, length(a) + length(b))
   i = 1
   j = 1
   k = 1
   while i <= length(a) && j <= length(b)
      cmpexp = cmp(a.exps[i], b.exps[j], par)
      if cmpexp < 0
         r.coeffs[k] = a.coeffs[i]
         r.exps[k] = a.exps[i]
         i += 1
      elseif cmpexp == 0
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

function -{T <: RingElem, S, N}(a::GenMPoly{T, S, N}, b::GenMPoly{T, S, N})
   par = parent(a)
   r = par()
   fit!(r, length(a) + length(b))
   i = 1
   j = 1
   k = 1
   while i <= length(a) && j <= length(b)
      cmpexp = cmp(a.exps[i], b.exps[j], par)
      if cmpexp < 0
         r.coeffs[k] = a.coeffs[i]
         r.exps[k] = a.exps[i]
         i += 1
      elseif cmpexp == 0
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

function do_copy{T <: RingElem, S, N}(Ac::Array{T, 1}, Bc::Array{T, 1},
               Ae::Array{NTuple{N, UInt}, 1}, Be::Array{NTuple{N, UInt}, 1}, 
        s1::Int, r::Int, n1::Int, par::GenMPolyRing{T, S, N})
   for i = 1:n1
      Bc[r + i] = Ac[s1 + i]
      Be[r + i] = Ae[s1 + i]
   end
   return n1
end

function do_merge{T <: RingElem, S, N}(Ac::Array{T, 1}, Bc::Array{T, 1},
               Ae::Array{NTuple{N, UInt}, 1}, Be::Array{NTuple{N, UInt}, 1}, 
        s1::Int, s2::Int, r::Int, n1::Int, n2::Int, par::GenMPolyRing{T, S, N})
   i = 1
   j = 1
   k = 1
   while i <= n1 && j <= n2
      cmpexp = cmp(Ae[s1 + i], Ae[s2 + j], par)
      if cmpexp < 0
         Bc[r + k] = Ac[s1 + i]
         Be[r + k] = Ae[s1 + i]
         i += 1
      elseif cmpexp == 0
         addeq!(Ac[s1 + i], Ac[s2 + j])
         if Ac[s1 + i] != 0
            Bc[r + k] = Ac[s1 + i]
            Be[r + k] = Ae[s1 + i]
         else
            k -= 1
         end
         i += 1
         j += 1
      else
         Bc[r + k] = Ac[s2 + j]
         Be[r + k] = Ae[s2 + j]
         j += 1
      end
      k += 1
   end
   while i <= n1
      Bc[r + k] = Ac[s1 + i]
      Be[r + k] = Ae[s1 + i]
      i += 1
      k += 1
   end
   while j <= n2
      Bc[r + k] = Ac[s2 + j]
      Be[r + k] = Ae[s2 + j]
      j += 1
      k += 1
   end
   return k - 1
end

function mul_classical{T <: RingElem, S, N}(a::GenMPoly{T, S, N}, b::GenMPoly{T, S, N})
   par = parent(a)
   R = base_ring(par)
   m = length(a)
   n = length(b)
   if m == 0 || n == 0
      return par()
   end
   a_alloc = max(m, n) + n
   b_alloc = max(m, n) + n
   Ac = Array(T, a_alloc)
   Bc = Array(T, b_alloc)
   Ae = Array(NTuple{N, UInt}, a_alloc)
   Be = Array(NTuple{N, UInt}, b_alloc)
   Am = Array(Int, 64) # 64 is upper bound on max(log m, log n)
   Bm = Array(Int, 64) # ... num polys merged (power of 2)
   Ai = Array(Int, 64) # index of polys in A minus 1
   Bi = Array(Int, 64) # index of polys in B minus 1
   An = Array(Int, 64) # lengths of polys in A
   Bn = Array(Int, 64) # lengths of polys in B
   Anum = 0 # number of polys in A
   Bnum = 0 # number of polys in B
   sa = 0 # number of used locations in A
   sb = 0 # number of used locations in B
   for i = 1:m # loop over monomials in a
      # check space
      if sa + n > a_alloc
         a_alloc = max(2*a_alloc, sa + n)
         resize!(Ac, a_alloc)
         resize!(Ae, a_alloc)
      end
      # compute monomial by polynomial product and store in A
      c = a.coeffs[i]
      d = a.exps[i]
      k = 1
      for j = 1:n
         s = Ac[sa + k] = c*b.coeffs[j]
         if s != 0
            Ae[sa + k] = d + b.exps[j]
            k += 1
         end
      end
      k -= 1
      Anum += 1
      Am[Anum] = 1
      Ai[Anum] = sa
      An[Anum] = k
      sa += k
      # merge similar sized polynomials from A to B...
      while Anum > 1 && (Am[Anum] == Am[Anum - 1])
         # check space
         want = sb + An[Anum] + An[Anum - 1]
         if want > b_alloc
            b_alloc = max(2*b_alloc, want)
            resize!(Bc, b_alloc)
            resize!(Be, b_alloc)            
         end
         # do merge to B
         k = do_merge(Ac, Bc, Ae, Be, Ai[Anum - 1], Ai[Anum], 
                                               sb, An[Anum - 1], An[Anum], par)
         Bnum += 1
         Bm[Bnum] = 2*Am[Anum]
         Bi[Bnum] = sb
         Bn[Bnum] = k
         sb += k
         sa -= An[Anum]
         sa -= An[Anum - 1]
         Anum -= 2
         # merge similar sized polynomials from B to A...
         if Bnum > 1 && (Bm[Bnum] == Bm[Bnum - 1])
            # check space
            want = sa + Bn[Bnum] + Bn[Bnum - 1]
            if want > a_alloc
               a_alloc = max(2*a_alloc, want)
               resize!(Ac, a_alloc)
               resize!(Ae, a_alloc)            
            end
            # do merge to A
            k = do_merge(Bc, Ac, Be, Ae, Bi[Bnum - 1], Bi[Bnum], 
                                               sa, Bn[Bnum - 1], Bn[Bnum], par)
            Anum += 1
            Am[Anum] = 2*Bm[Bnum]
            Ai[Anum] = sa
            An[Anum] = k
            sa += k
            sb -= Bn[Bnum]
            sb -= Bn[Bnum - 1]
            Bnum -= 2
         end
      end
   end 
   # Add all irregular sized polynomials together
   while Anum + Bnum > 1
      # Find the smallest two polynomials
      if Anum == 0 || Bnum == 0
         c1 = c2 = (Anum == 0) ? 2 : 1
      elseif Anum + Bnum == 2
         c1 = (Am[Anum] < Bm[Bnum]) ? 1 : 2
         c2 = 3 - c1
      elseif Am[Anum] < Bm[Bnum]
         c1 = 1
         c2 = (Anum == 1 || (Bnum > 1 && Bm[Bnum] < Am[Anum - 1])) ? 2 : 1
      else
         c1 = 2
         c2 = (Bnum == 1 || (Anum > 1 && Am[Anum] < Bm[Bnum - 1])) ? 1 : 2
      end
      # If both polys are on side A, merge to side B
      if c1 == 1 && c2 == 1
         # check space
         want = sb + An[Anum] + An[Anum - 1]
         if want > b_alloc
            b_alloc = max(2*b_alloc, want)
            resize!(Bc, b_alloc)
            resize!(Be, b_alloc)            
         end
         # do merge to B
         k = do_merge(Ac, Bc, Ae, Be, Ai[Anum - 1], Ai[Anum], 
                                               sb, An[Anum - 1], An[Anum], par)
         Bnum += 1
         Bm[Bnum] = 2*Am[Anum - 1]
         Bi[Bnum] = sb
         Bn[Bnum] = k
         sb += k
         sa -= An[Anum]
         sa -= An[Anum - 1]
         Anum -= 2
      # If both polys are on side B, merge to side A
      elseif c1 == 2 && c2 == 2
         # check space
         want = sa + Bn[Bnum] + Bn[Bnum - 1]
         if want > a_alloc
            a_alloc = max(2*a_alloc, want)
            resize!(Ac, a_alloc)
            resize!(Ae, a_alloc)            
         end
         # do merge to A
         k = do_merge(Bc, Ac, Be, Ae, Bi[Bnum - 1], Bi[Bnum], 
                                            sa, Bn[Bnum - 1], Bn[Bnum], par)
         Anum += 1
         Am[Anum] = 2*Bm[Bnum - 1]
         Ai[Anum] = sa
         An[Anum] = k
         sa += k
         sb -= Bn[Bnum]
         sb -= Bn[Bnum - 1]
         Bnum -= 2
      # Polys are on different sides, move from smallest side to largest
      else
         # smallest poly on side A, move to B
         if c1 == 1
            # check space
            want = sb + An[Anum]
            if want > b_alloc
               b_alloc = max(2*b_alloc, want)
               resize!(Bc, b_alloc)
               resize!(Be, b_alloc)            
            end
            # do copy to B
            k = do_copy(Ac, Bc, Ae, Be, Ai[Anum], sb, An[Anum], par)
            Bnum += 1
            Bm[Bnum] = Am[Anum]
            Bi[Bnum] = sb
            Bn[Bnum] = k
            sb += k
            sa -= An[Anum]
            Anum -= 1
         # smallest poly on side B, move to A
         else
            # check space
            want = sa + Bn[Bnum]
            if want > a_alloc
               a_alloc = max(2*a_alloc, want)
               resize!(Ac, a_alloc)
               resize!(Ae, a_alloc)            
            end
            # do copy to A
            k = do_copy(Bc, Ac, Be, Ae, Bi[Bnum], sa, Bn[Bnum], par)
            Anum += 1
            Am[Anum] = Bm[Bnum]
            Ai[Anum] = sa
            An[Anum] = k
            sa += k
            sb -= Bn[Bnum]
            Bnum -= 1
         end
      end
   end
   # Result is on side A
   if Anum == 1
      resize!(Ac, An[1])
      resize!(Ae, An[1])
      return parent(a)(Ac, Ae)
   # Result is on side B
   else
      resize!(Bc, Bn[1])
      resize!(Be, Bn[1])
      return parent(a)(Bc, Be)
   end
end

abstract heap

immutable heap_s{N}
   exp::NTuple{N, UInt}
   n::Int
end

immutable heap_t
   i::Int
   j::Int
   next::Int   
end

immutable nheap_t
   i::Int
   j::Int
   p::Int # polynomial, for heap algorithms that work with multiple polynomials
   next::Int
end

function isless{N}(a::heap_s{N}, b::heap_s{N})
   return a.exp < b.exp
end

function =={N}(a::heap_s{N}, b::heap_s{N})
   return a.exp == b.exp
end

heapleft(i::Int) = 2i
heapright(i::Int) = 2i + 1
heapparent(i::Int) = div(i, 2)

# either chain (exp, x) or insert into heap
function heapinsert!{N}(xs::Array{heap_s{N}, 1}, ys::Array{heap_t, 1}, m::Int, exp::NTuple{N, UInt})
   i = n = length(xs) + 1
   @inbounds if i != 1 && exp == xs[1].exp
      ys[m] = heap_t(ys[m].i, ys[m].j, xs[1].n)
      xs[1] = heap_s{N}(exp, m)
      return
   end
   @inbounds while (j = heapparent(i)) >= 1
      if exp == xs[j].exp
         ys[m] = heap_t(ys[m].i, ys[m].j, xs[j].n)
         xs[j] = heap_s{N}(exp, m)
         return
      elseif exp < xs[j].exp
         i = j
      else
         break
      end
   end
   push!(xs, heap_s{N}(exp, 0))
   @inbounds while n > i
      xs[n] = xs[heapparent(n)]
      n >>= 1
   end
   xs[i] = heap_s{N}(exp, m)
   return
end

function nheapinsert!{N}(xs::Array{heap_s{N}, 1}, ys::Array{nheap_t, 1}, m::Int, exp::NTuple{N, UInt}, p::Int)
   i = n = length(xs) + 1
   @inbounds if i != 1 && exp == xs[1].exp
      ys[m] = nheap_t(ys[m].i, ys[m].j, p, xs[1].n)
      xs[1] = heap_s{N}(exp, m)
      return
   end
   @inbounds while (j = heapparent(i)) >= 1
      if exp == xs[j].exp
         ys[m] = nheap_t(ys[m].i, ys[m].j, p, xs[j].n)
         xs[j] = heap_s{N}(exp, m)
         return
      elseif exp < xs[j].exp
         i = j
      else
         break
      end
   end
   push!(xs, heap_s{N}(exp, 0))
   @inbounds while n > i
      xs[n] = xs[heapparent(n)]
      n >>= 1
   end
   xs[i] = heap_s{N}(exp, m)
   return
end

function heappop!{N}(xs::Array{heap_s{N}, 1})
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

function mul_johnson{T <: RingElem, S, N}(a::GenMPoly{T, S, N}, b::GenMPoly{T, S, N})
   par = parent(a)
   R = base_ring(par)
   m = length(a)
   n = length(b)
   if m == 0 || n == 0
      return par()
   end
   H = Array(heap_s{N}, 0)
   I = Array(heap_t, 0)
   # set up heap
   push!(H, heap_s{N}(a.exps[1] + b.exps[1], 1))
   push!(I, heap_t(1, 1, 0))
   r_alloc = max(m, n) + n
   Rc = Array(T, r_alloc)
   Re = Array(NTuple{N, UInt}, r_alloc)
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
            addmul!(Rc[k], a.coeffs[v.i], b.coeffs[v.j], c)
         end
         if v.j < n || v.j == 1
            push!(Q, x.n)
         end
         while (xn = v.next) != 0
            v = I[xn]
            addmul!(Rc[k], a.coeffs[v.i], b.coeffs[v.j], c)
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
            Collections.heappush!(H, heap_s{N}(a.exps[v.i + 1] + b.exps[1], length(I)))       
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

function pack_monomials{M, N}(a::Array{NTuple{M, UInt}, 1}, b::Array{NTuple{N, UInt}, 1}, k::Int, bits::Int)
   A = Array(UInt, M)
   for i = 1:length(a)
      m = 0
      n = 1
      v = UInt(0)
      c = b[i]
      for j = 1:N
         v += c[j]
         m += 1
         if m == k
            m = 0
            A[n] = v
            n += 1
            v = UInt(0)
         else
            v <<= bits
         end
      end
      if m != 0
         A[n] = (v << bits*(k - m - 1))
      end
      a[i] = ntuple(i -> A[i], Val{M})
   end
end

function unpack_monomials{M, N}(a::Array{NTuple{N, UInt}, 1}, b::Array{NTuple{M, UInt}, 1}, k::Int, bits::Int)
   A = Array(UInt, N)
   mask = (UInt(1) << bits) - UInt(1)
   for i = 1:length(b)
      c = b[i]
      m = 1
      n = 1
      for j = 1:N
         A[j] = ((c[n] >> ((k - m) * bits)) & mask)
         if m == k
            m = 1
            n += 1
         else
            m += 1
         end
      end
      a[i] = ntuple(i -> A[i], Val{N})
   end
end

function *{T <: RingElem, S, N}(a::GenMPoly{T, S, N}, b::GenMPoly{T, S, N})
   v1, d1 = max_degrees(a)
   v2, d2 = max_degrees(b)
   v = v1 + v2
   d = 0
   for i = 1:length(v)
      if v[i] > d
         d = v[i]
      end
   end
   bits = 8
   max_e = 2^(bits - 1)
   while d >= max_e
      bits *= 2
      max_e = 2^(bits - 1)
   end
   word_bits = sizeof(Int)*8
   k = div(word_bits, bits)
   if k != 1
      M = div(N + k - 1, k)
      e1 = Array(NTuple{M, UInt}, length(a))
      e2 = Array(NTuple{M, UInt}, length(b))
      pack_monomials(e1, a.exps, k, bits)
      pack_monomials(e2, b.exps, k, bits)
      par = GenMPolyRing{T, S, M}(base_ring(a), parent(a).S)
      a1 = par(a.coeffs, e1)
      b1 = par(b.coeffs, e2)
      a1.length = a.length
      b1.length = b.length
      r1 = mul_johnson(a1, b1)
      er = Array(NTuple{N, UInt}, length(r1))
      unpack_monomials(er, r1.exps, k, bits)
   else
      r1 = mul_johnson(a, b)
      er = r1.exps
   end
   return parent(a)(r1.coeffs, er)
end

###############################################################################
#
#   Ad hoc arithmetic functions
#
###############################################################################

function *{T <: RingElem, S, N}(a::GenMPoly{T, S, N}, n::Integer)
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

function *{T <: RingElem, S, N}(a::GenMPoly{T, S, N}, n::fmpz)
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

function *{T <: RingElem, S, N}(a::GenMPoly{T, S, N}, n::T)
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

*{T <: RingElem, S, N}(n::Integer, a::GenMPoly{T, S, N}) = a*n

*{T <: RingElem, S, N}(n::fmpz, a::GenMPoly{T, S, N}) = a*n

*{T <: RingElem, S, N}(n::T, a::GenMPoly{T, S, N}) = a*n

+{T <: RingElem, S, N}(a::GenMPoly{T, S, N}, b::T) = a + parent(a)(b)

+{T <: RingElem, S, N}(a::GenMPoly{T, S, N}, b::Integer) = a + parent(a)(b)

+{T <: RingElem, S, N}(a::GenMPoly{T, S, N}, b::fmpz) = a + parent(a)(b)

-{T <: RingElem, S, N}(a::GenMPoly{T, S, N}, b::T) = a - parent(a)(b)

-{T <: RingElem, S, N}(a::GenMPoly{T, S, N}, b::Integer) = a - parent(a)(b)

-{T <: RingElem, S, N}(a::GenMPoly{T, S, N}, b::fmpz) = a - parent(a)(b)

+{T <: RingElem, S, N}(a::T, b::GenMPoly{T, S, N}) = parent(b)(a) + b

+{T <: RingElem, S, N}(a::Integer, b::GenMPoly{T, S, N}) = parent(b)(a) + b

+{T <: RingElem, S, N}(a::fmpz, b::GenMPoly{T, S, N}) = parent(b)(a) + b

-{T <: RingElem, S, N}(a::T, b::GenMPoly{T, S, N}) = parent(b)(a) - b

-{T <: RingElem, S, N}(a::Integer, b::GenMPoly{T, S, N}) = parent(b)(a) - b

-{T <: RingElem, S, N}(a::fmpz, b::GenMPoly{T, S, N}) = parent(b)(a) - b

###############################################################################
#
#   Comparison functions
#
###############################################################################

function =={T <: RingElem, S, N}(a::GenMPoly{T, S, N}, b::GenMPoly{T, S, N})
   if a.length != b.length
      return false
   end
   for i = 1:a.length
      if a.exps[i] != b.exps[i] || a.coeffs[i] != b.coeffs[i]
         return false
      end
   end
   return true
end

###############################################################################
#
#   Ad hoc comparison functions
#
###############################################################################

function =={T <: RingElem, S, N}(a::GenMPoly{T, S, N}, n::Integer)
   if n == 0
      return a.length == 0
   elseif a.length == 1
      return a.coeffs[1] == n && iszero(a.exps[1])
   end
   return false
end

function =={T <: RingElem, S, N}(a::GenMPoly{T, S, N}, n::fmpz)
   if n == 0
      return a.length == 0
   elseif a.length == 1
      return a.coeffs[1] == n && iszero(a.exps[1])
   end
   return false
end

function =={T <: RingElem, S, N}(a::GenMPoly{T, S, N}, n::T)
   if n == 0
      return a.length == 0
   elseif a.length == 1
      return a.coeffs[1] == n && iszero(a.exps[1])
   end
   return false
end

###############################################################################
#
#   Powering
#
###############################################################################

function from_exp{N}(R::Ring, a::NTuple{N, UInt})
   z = fmpz(a[1])
   for i = 2:N
      z <<= sizeof(UInt)*8
      z += a[i]
   end
   return R(z)
end

# Implement fps algorithm from "Sparse polynomial powering with heaps" by
# Monagan and Pearce, except that we modify the algorithm to return terms
# in ascending order and we fix some issues in the original algorithm
# http://www.cecm.sfu.ca/CAG/papers/SparsePowering.pdf

function pow_fps{T <: RingElem, S, N}(f::GenMPoly{T, S, N}, k::Int)
   par = parent(f)
   R = base_ring(par)
   m = length(f)
   H = Array(heap_s{N}, 0) # heap
   I = Array(heap_t, 0) # auxilliary data for heap nodes
   # set up output poly coeffs and exponents (corresponds to h in paper)
   r_alloc = k*(m - 1) + 1
   Rc = Array(T, r_alloc)
   Re = Array(NTuple{N, UInt}, r_alloc)
   rnext = 1
   # set up g coeffs and exponents (corresponds to g in paper)
   g_alloc = k*(m - 1) + 1
   gc = Array(T, g_alloc)
   ge = Array(NTuple{N, UInt}, g_alloc)
   gnext = 1
   # set up heap
   gc[1] = f.coeffs[1]^(k-1)
   ge[1] = f.exps[1]*(k - 1)
   Rc[1] = f.coeffs[1]*gc[1]
   Re[1] = f.exps[1]*k
   push!(H, heap_s{N}(f.exps[2] + ge[1], 1))
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
      zero!(C) 
      zero!(SS)
      while !isempty(H) && H[1].exp == exp
         x = H[1]
         heappop!(H)
         v = I[x.n]
         largest[v.i] |= topbit
         mul!(t1, f.coeffs[v.i], gc[v.j])
         addeq!(SS, t1)
         if exp <= finalexp
            add!(temp2, fik[v.i], gi[v.j])
            addmul!(C, temp2, t1, temp)
         end
         if first
            ge[gnext] = exp - f.exps[1]
            first = false
         end
         push!(Q, x.n)
         while (xn = v.next) != 0
            v = I[xn]
            largest[v.i] |= topbit
            mul!(t1, f.coeffs[v.i], gc[v.j])
            addeq!(SS, t1)
            if exp <= finalexp
               add!(temp2, fik[v.i], gi[v.j])
               addmul!(C, temp2, t1, temp)
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
               Collections.heappush!(H, heap_s{N}(f.exps[v.i] + ge[v.j + 1], length(I)))
            end
            largest[v.i] = v.j + 1     
         end
      end
      if C != 0
         temp = divexact(C, from_exp(R, exp) - kp1f1)
         addeq!(SS, temp)
         gc[gnext] = divexact(temp, f.coeffs[1])
         push!(gi, -from_exp(R, ge[gnext]))
         if (largest[2] & topbit) != 0
            push!(I, heap_t(2, gnext, 0))
            Collections.heappush!(H, heap_s{N}(f.exps[2] + ge[gnext], length(I)))   
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

function ^{T <: RingElem, S, N}(a::GenMPoly{T, S, N}, b::Int)
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
      v, d = max_degrees(a)
      d *= b
      bits = 8
      max_e = 2^(bits - 1)
      while d >= max_e
         bits *= 2
         max_e = 2^(bits - 1)
      end
      word_bits = sizeof(Int)*8
      k = div(word_bits, bits)
      if k != 1
         M = div(N + k - 1, k)
         e1 = Array(NTuple{M, UInt}, length(a))
         pack_monomials(e1, a.exps, k, bits)
         par = GenMPolyRing{T, S, M}(base_ring(a), parent(a).S)
         a1 = par(a.coeffs, e1)
         a1.length = a.length
         r1 = pow_fps(a1, b)
         er = Array(NTuple{N, UInt}, length(r1))
         unpack_monomials(er, r1.exps, k, bits)
      else
         r1 = pow_fps(a, b)
         er = r1.exps
      end
      return parent(a)(r1.coeffs, er)
   end
end

###############################################################################
#
#   Exact division
#
###############################################################################

function divides_monagan_pearce{T <: RingElem, S, N}(a::GenMPoly{T, S, N}, b::GenMPoly{T, S, N}, bits::Int)
   par = parent(a)
   R = base_ring(par)
   m = length(a)
   n = length(b)
   n == 0 && error("Division by zero in divides_monagan_pearce")
   if m == 0
      return true, par()
   end
   mask1 = UInt(1) << (bits - 1)
   mask = UInt(0)
   for i = 1:div(sizeof(UInt)*8, bits)
      mask = (mask << bits) + mask1
   end
   H = Array(heap_s{N}, 0)
   I = Array(heap_t, 0)
   # set up heap
   push!(H, heap_s{N}(a.exps[1], 1))
   push!(I, heap_t(0, 1, 0))
   q_alloc = max(m - n, n)
   Qc = Array(T, q_alloc)
   Qe = Array(NTuple{N, UInt}, q_alloc)
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
            d1, Qe[k] = divides(exp, b.exps[1], mask)
            first = false
         end
         if v.i == 0
            addmul!(qc, a.coeffs[v.j], m1, c)
         else
            addmul!(qc, b.coeffs[v.i], Qc[v.j], c)
         end
         if v.i != 0 || v.j < m
            push!(Q, x.n)
         else
            push!(reuse, x.n)
         end
         while (xn = v.next) != 0
            v = I[xn]
            if v.i == 0
               addmul!(qc, a.coeffs[v.j], m1, c)
            else
               addmul!(qc, b.coeffs[v.i], Qc[v.j], c)
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
               Collections.heappush!(H, heap_s{N}(b.exps[i] + Qe[k], length(I)))
            end                 
         end
         s = 1
      end
      zero!(qc)
   end
   resize!(Qc, k)
   resize!(Qe, k)
   return true, parent(a)(Qc, Qe)
end

function divides{T <: RingElem, S, N}(a::GenMPoly{T, S, N}, b::GenMPoly{T, S, N})
   v1, d1 = max_degrees(a)
   v2, d2 = max_degrees(b)
   d = max(d1, d2)
   bits = 8
   max_e = 2^(bits - 1)
   while d >= max_e
      bits *= 2
      max_e = 2^(bits - 1)
   end
   word_bits = sizeof(Int)*8
   k = div(word_bits, bits)
   if k != 1
      M = div(N + k - 1, k)
      e1 = Array(NTuple{M, UInt}, length(a))
      e2 = Array(NTuple{M, UInt}, length(b))
      pack_monomials(e1, a.exps, k, bits)
      pack_monomials(e2, b.exps, k, bits)
      par = GenMPolyRing{T, S, M}(base_ring(a), parent(a).S)
      a1 = par(a.coeffs, e1)
      b1 = par(b.coeffs, e2)
      a1.length = a.length
      b1.length = b.length
      flag, q = divides_monagan_pearce(a1, b1, bits)
      eq = Array(NTuple{N, UInt}, length(q))
      unpack_monomials(eq, q.exps, k, bits)
   else
      flag, q = divides_monagan_pearce(a, b, bits)
      eq = q.exps
   end
   return flag, parent(a)(q.coeffs, eq)
end

function divexact{T <: RingElem, S, N}(a::GenMPoly{T, S, N}, b::GenMPoly{T, S, N})
   d, q = divides(a, b)
   d == false && error("Not an exact division in divexact")
   return q
end

###############################################################################
#
#   Euclidean division
#
###############################################################################

function divrem_monagan_pearce{T <: RingElem, S, N}(a::GenMPoly{T, S, N}, b::GenMPoly{T, S, N}, bits::Int, maxn::NTuple{N, UInt})
   par = parent(a)
   R = base_ring(par)
   m = length(a)
   n = length(b)
   n == 0 && error("Division by zero in divrem_monagan_pearce")
   if m == 0
      return par(), par()
   end
   mask1 = UInt(1) << (bits - 1)
   mask = UInt(0)
   for i = 1:div(sizeof(UInt)*8, bits)
      mask = (mask << bits) + mask1
   end
   H = Array(heap_s{N}, 0)
   I = Array(heap_t, 0)
   # set up heap
   push!(H, heap_s{N}(maxn - a.exps[m], 1))
   push!(I, heap_t(0, 1, 0))
   q_alloc = max(m - n, n)
   r_alloc = n
   Qc = Array(T, q_alloc)
   Qe = Array(NTuple{N, UInt}, q_alloc)
   Rc = Array(T, r_alloc)
   Re = Array(NTuple{N, UInt}, r_alloc)
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
            addmul!(qc, a.coeffs[m + 1 - v.j], m1, c)
         else
            addmul!(qc, b.coeffs[n + 1 - v.i], Qc[v.j], c)
         end
         if v.i != 0 || v.j < m
            push!(Q, x.n)
         else
            push!(reuse, x.n)
         end
         while (xn = v.next) != 0
            v = I[xn]
            if v.i == 0
               addmul!(qc, a.coeffs[m + 1 - v.j], m1, c)
            else
               addmul!(qc, b.coeffs[n + 1 - v.i], Qc[v.j], c)
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
         d1, texp = divides(maxn - b.exps[n], exp, mask)
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
                     Collections.heappush!(H, heap_s{N}(maxn - b.exps[n + 1 - i] - Qe[k], length(I)))
                  end
               end                 
               s = 1
            else
               k -= 1
            end
         end
      end
      zero!(qc)
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

function divrem{T <: RingElem, S, N}(a::GenMPoly{T, S, N}, b::GenMPoly{T, S, N})
   v1, d1 = max_degrees(a)
   v2, d2 = max_degrees(b)
   d = max(d1, d2)
   bits = 8
   max_e = 2^(bits - 1)
   while d >= max_e
      bits *= 2
      max_e = 2^(bits - 1)
   end
   maxexp = [ntuple(i -> UInt(max(v1[i], v2[i])), Val{N})]
   word_bits = sizeof(Int)*8
   k = div(word_bits, bits)
   if k != 1
      M = div(N + k - 1, k)
      e1 = Array(NTuple{M, UInt}, length(a))
      e2 = Array(NTuple{M, UInt}, length(b))
      maxn = Array(NTuple{M, UInt}, 1)
      pack_monomials(maxn, maxexp, k, bits)
      pack_monomials(e1, a.exps, k, bits)
      pack_monomials(e2, b.exps, k, bits)
      par = GenMPolyRing{T, S, M}(base_ring(a), parent(a).S)
      a1 = par(a.coeffs, e1)
      b1 = par(b.coeffs, e2)
      a1.length = a.length
      b1.length = b.length
      q, r = divrem_monagan_pearce(a1, b1, bits, maxn[1])
      eq = Array(NTuple{N, UInt}, length(q))
      er = Array(NTuple{N, UInt}, length(r))
      unpack_monomials(eq, q.exps, k, bits)
      unpack_monomials(er, r.exps, k, bits)
   else
      q, r = divrem_monagan_pearce(a, b, bits, maxn[1])
      eq = q.exps
      er = r.exps
   end
   return parent(a)(q.coeffs, eq), parent(a)(r.coeffs, er)
end

function divrem_monagan_pearce{T <: RingElem, S, N}(a::GenMPoly{T, S, N}, b::Array{GenMPoly{T, S, N}, 1}, bits::Int, maxn::NTuple{N, UInt})
   par = parent(a)
   R = base_ring(par)
   len = length(b)
   m = length(a)
   n = [length(b[i]) for i in 1:len]
   for i = 1:len
      n[i] == 0 && error("Division by zero in divrem_monagan_pearce")
   end
   if m == 0
      return [par() for i in 1:len], [par() for i in 1:len]
   end
   mask1 = UInt(1) << (bits - 1)
   mask = UInt(0)
   for i = 1:div(sizeof(UInt)*8, bits)
      mask = (mask << bits) + mask1
   end
   H = Array(heap_s{N}, 0)
   I = Array(nheap_t, 0)
   # set up heap
   push!(H, heap_s{N}(maxn - a.exps[m], 1))
   push!(I, nheap_t(0, 1, 0, 0))
   q_alloc = [max(m - n[i], n[i]) for i in 1:len]
   r_alloc = n[1]
   Qc = [Array(T, q_alloc[i]) for i in 1:len]
   Qe = [Array(NTuple{N, UInt}, q_alloc[i]) for i in 1:len]
   Rc = Array(T, r_alloc)
   Re = Array(NTuple{N, UInt}, r_alloc)
   k = [0 for i in 1:len]
   l = 0
   s = [n[i] for i in 1:len]
   c = R()
   qc = R()
   m1 = -R(1)
   mb = [-b[i].coeffs[n[i]] for i in 1:len]
   Q = Array(Int, 0)
   reuse = Array(Int, 0)
   while !isempty(H)
      exp = H[1].exp
      @inbounds while !isempty(H) && H[1].exp == exp
         x = H[1]
         heappop!(H)
         v = I[x.n]
         if v.i == 0
            addmul!(qc, a.coeffs[m + 1 - v.j], m1, c)
         else
            addmul!(qc, b[v.p].coeffs[n[v.p] + 1 - v.i], Qc[v.p][v.j], c)
         end
         if v.i != 0 || v.j < m[v.p]
            push!(Q, x.n)
         else
            push!(reuse, x.n)
         end
         while (xn = v.next) != 0
            v = I[xn]
            if v.i == 0
               addmul!(qc, a.coeffs[m + 1 - v.j], m1, c)
            else
               addmul!(qc, b[v.p].coeffs[n[v.p] + 1 - v.i], Qc[v.p][v.j], c)
            end
            if v.i != 0 || v.j < m[v.p]
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
            I[xn] = nheap_t(0, v.j + 1, 0, 0)
            nheapinsert!(H, I, xn, maxn - a.exps[m - v.j], 0) # either chain or insert into heap   
         elseif v.j < k[v.p]
            I[xn] = nheap_t(v.i, v.j + 1, v.p, 0)
            nheapinsert!(H, I, xn, maxn - b[v.p].exps[n[v.p] + 1 - v.i] - Qe[v.p][v.j + 1], v.p) # either chain or insert into heap
         elseif v.j == k[v.p]
            s[v.p] += 1
            push!(reuse, xn)
         end  
      end
      if qc != 0
         div_flag = false
         for w = 1:len
            d1, texp = divides(maxn - b[w].exps[n[w]], exp, mask)
            if d1
               d2, tq = divides(qc, mb[w])
               if d2
                  div_flag = true
                  k[w] += 1
                  if k[w] > q_alloc[w]
                     q_alloc[w] *= 2
                     resize!(Qc[w], q_alloc[w])
                     resize!(Qe[w], q_alloc[w])
                  end
                  Qc[w][k[w]] = tq
                  Qe[w][k[w]] = texp
                  for i = 2:s[w]
                     if !isempty(reuse)
                        xn = pop!(reuse)
                        I[xn] = nheap_t(i, k[w], w, 0)
                        nheapinsert!(H, I, xn, maxn - b[w].exps[n[w] + 1 - i] - Qe[w][k[w]], w) # either chain or insert into heap
                     else
                        push!(I, nheap_t(i, k[w], w, 0))
                        Collections.heappush!(H, heap_s{N}(maxn - b[w].exps[n[w] + 1 - i] - Qe[w][k[w]], length(I)))
                     end
                  end                 
                  s[w] = 1
                  break
               end
            end
         end
         if !div_flag
            l += 1
            if l >= r_alloc
               r_alloc *= 2
               resize!(Rc, r_alloc)
               resize!(Re, r_alloc)
            end
            Rc[l] = -qc
            Re[l] = maxn - exp
         end
      end
      zero!(qc)
   end
   for i = 1:len
      resize!(Qc[i], k[i])
      resize!(Qe[i], k[i])
      reverse!(Qc[i])
      reverse!(Qe[i])
   end
   resize!(Rc, l)
   resize!(Re, l)
   reverse!(Rc)
   reverse!(Re)
   return [parent(a)(Qc[i], Qe[i]) for i in 1:len], parent(a)(Rc, Re)
end

function divrem{T <: RingElem, S, N}(a::GenMPoly{T, S, N}, b::Array{GenMPoly{T, S, N}, 1})
   v1, d = max_degrees(a)
   len = length(b)
   for i = 1:len
      v2, d2 = max_degrees(b[i])
      for j = 1:N
         v1[j] = max(v1[j], v2[j])
      end
      d = max(d, d2)
   end
   bits = 8
   max_e = 2^(bits - 1)
   while d >= max_e
      bits *= 2
      max_e = 2^(bits - 1)
   end
   maxexp = [ntuple(i -> UInt(v1[i]), Val{N})]
   word_bits = sizeof(Int)*8
   k = div(word_bits, bits)
   if k != 1
      M = div(N + k - 1, k)
      e1 = Array(NTuple{M, UInt}, length(a))
      e2 = [Array(NTuple{M, UInt}, length(b[i])) for i in 1:len]
      maxn = Array(NTuple{M, UInt}, 1)
      pack_monomials(maxn, maxexp, k, bits)
      pack_monomials(e1, a.exps, k, bits)
      for i = 1:len
         pack_monomials(e2[i], b[i].exps, k, bits)
      end
      par = GenMPolyRing{T, S, M}(base_ring(a), parent(a).S)
      a1 = par(a.coeffs, e1)
      a1.length = a.length
      b1 = [par(b[i].coeffs, e2[i]) for i in 1:len]
      for i = 1:len
         b1[i].length = b[i].length
      end
      q, r = divrem_monagan_pearce(a1, b1, bits, maxn[1])
      eq = [Array(NTuple{N, UInt}, length(q[i])) for i in 1:len]
      for i = 1:len
         unpack_monomials(eq[i], q[i].exps, k, bits)  
      end
      er = Array(NTuple{N, UInt}, length(r))
      unpack_monomials(er, r.exps, k, bits)  
   else
      q, r = divrem_monagan_pearce(a, b, bits, maxn[1])
      eq = [q[i].exps for i in 1:len]
      er = r.exps
   end
   return [parent(a)(q[i].coeffs, eq[i]) for i in 1:len], parent(a)(r.coeffs, er)
end

################################################################################
#
#   Remove and valuation
#
################################################################################

doc"""
    remove(z::GenMPoly, p::GenMPoly)
> Computes the valuation of $z$ at $p$, that is, the largest $k$ such that
> $p^k$ divides $z$. Additionally, $z/p^k$ is returned as well.
>
> See also `valuation`, which only returns the valuation.
"""
function remove{T <: RingElem, S, N}(z::GenMPoly{T, S, N}, p::GenMPoly{T, S, N})
   check_parent(z,p)
   z == 0 && error("Not yet implemented")
   fl, q = divides(z, p)
   if !fl
      return 0, z
   end
   v = 0
   qn = q
   while fl
      q = qn
      fl, qn = divides(q, p)
      v += 1
   end
   return v, q
end

doc"""
    valuation(z::GenMPoly, p::GenMPoly)
> Computes the valuation of $z$ at $p$, that is, the largest $k$ such that
> $p^k$ divides $z$.
>
> See also `remove`, which also returns $z/p^k$.
"""
function valuation{T <: RingElem, S, N}(z::GenMPoly{T, S, N}, p::GenMPoly{T, S, N})
  v, _ = remove(z, p)
  return v
end

###############################################################################
#
#   Evaluation
#
###############################################################################

function evaluate{T <: RingElem, S, N}(a::GenMPoly{T, S, N}, A::Array{T})
   if iszero(a)
      return base_ring(a)()
   end
   if S == :lex || S == :revlex
      start_var = 1
   else
      start_var = 2
   end
   while a.length > 1 || (a.length == 1 && !iszero(a.exps[a.length]))
      k = main_variable(a, start_var)
      p = main_variable_extract(a, k)
      a = evaluate(p, A[k])
   end
   if a.length == 0
      return base_ring(a)()
   else
      return a.coeffs[1]
   end
end

function evaluate{T <: RingElem, S, N, U <: Integer}(a::GenMPoly{T, S, N}, A::Array{U})
   if iszero(a)
      return base_ring(a)()
   end
   if S == :lex || S == :revlex
      start_var = 1
   else
      start_var = 2
   end
   while a.length > 1 || (a.length == 1 && !iszero(a.exps[a.length]))
      k = main_variable(a, start_var)
      p = main_variable_extract(a, k)
      a = evaluate(p, A[k])
   end
   if a.length == 0
      return base_ring(a)()
   else
      return a.coeffs[1]
   end
end

function evaluate{T <: RingElem, S, N}(a::GenMPoly{T, S, N}, A::Array{fmpz})
   if iszero(a)
      return base_ring(a)()
   end
   if S == :lex || S == :revlex
      start_var = 1
   else
      start_var = 2
   end
   while a.length > 1 || (a.length == 1 && !iszero(a.exps[a.length]))
      k = main_variable(a, start_var)
      p = main_variable_extract(a, k)
      a = evaluate(p, A[k])
   end
   if a.length == 0
      return base_ring(a)()
   else
      return a.coeffs[1]
   end
end

###############################################################################
#
#   GCD
#
###############################################################################

function gcd{T <: RingElem, S, N}(a::GenMPoly{T, S, N}, b::GenMPoly{T, S, N})
   if a.length == 0
      return b
   elseif b.length == 0
      return a
   end
   if a == 1
      return deepcopy(a)
   end
   if b == 1
      return deepcopy(b)
   end
   # get degrees in each variable
   v1, d1 = max_degrees(a)
   v2, d2 = max_degrees(b)
   # check if both polys are constant
   if d1 == 0 && d2 == 0
      return parent(a)(gcd(a.coeffs[1], b.coeffs[1]))
   end
   if S == :lex || S == :revlex
      start_var = 1
   else
      start_var = 2
   end
   # check for cases where degree is 0 in one of the variables for one poly
   for k = start_var:N
      if v1[k] == 0 && v2[k] != 0
         p2 = main_variable_extract(b, k)
         return gcd(a, content(p2))
      end
      if v2[k] == 0 && v1[k] != 0
         p1 = main_variable_extract(a, k)
         return gcd(content(p1), b)
      end
   end
   # count number of terms in lead coefficient, for each variable
   lead1 = zeros(Int, N)
   lead2 = zeros(Int, N)
   for i = start_var:N
      if v1[i] != 0
         for j = 1:length(a)
            if a.exps[j][i] == v1[i]
               lead1[i] += 1
            end
         end
      end
      if v2[i] != 0
         for j = 1:length(b)
            if b.exps[j][i] == v2[i]
               lead2[i] += 1
            end
         end
      end
   end
   # heuristic to decide optimal variable k to choose as main variable
   # it basically looks for low degree in the main variable, but
   # heavily weights monomial leading term
   k = 0
   m = Inf
   for i = start_var:N
      if v1[i] != 0
         if v1[i] >= v2[i]
            c = max(log(lead2[i])*v1[i]*v2[i], log(2)*v2[i])
            if c < m
               m = c
               k = i
            end
         else
            c = max(log(lead1[i])*v2[i]*v1[i], log(2)*v1[i])
            if c < m
               m = c
               k = i
            end
         end
      end
   end
   # write polys in terms of main variable k, do gcd, then convert back
   p1 = main_variable_extract(a, k)
   p2 = main_variable_extract(b, k)
   g = gcd(p1, p2)
   return main_variable_insert(g, k)
end

function term_content{T <: RingElem, S, N}(a::GenMPoly{T, S, N})
   if a.length <= 1
      return a
   end
   Ce = Array(NTuple{N, UInt}, 1)
   Cc = Array(T, 1)
   Ce[1] = a.exps[1]
   for i = 2:a.length
      Ce[1] = min(Ce[1], a.exps[i])
      if iszero(Ce[1])
         break
      end
   end
   Cc[1] = a.coeffs[1]
   for i = 2:a.length
      Cc[1] = gcd(Cc[1], a.coeffs[i])
      if isone(Cc[1])
         break
      end
   end
   return parent(a)(Cc, Ce)
end

###############################################################################
#
#   Conversions
#
###############################################################################

# determine the number of the first variable for which there is a nonzero exp
# we start at variable k
function main_variable{T <: RingElem, S, N}(a::GenMPoly{T, S, N}, k::Int)
   for i = k:N
      for j = 1:a.length
         if a.exps[j][i] != 0
            return i
         end
      end
   end
   return 0
end

# return an array of all the starting positions of terms in the main variable k
function main_variable_terms{T <: RingElem, S, N}(a::GenMPoly{T, S, N}, k::Int)
   A = Array(Int, 0)
   current_term = typemax(UInt)
   for i = 1:a.length
      if a.exps[i][k] != current_term
         push!(A, i)
         current_term = a.exps[i][k]
      end
   end
   return A
end

# return the coefficient as a sparse distributed polynomial, of the term in variable
# k starting at position n 
function main_variable_coefficient_lex{T <: RingElem, S, N}(a::GenMPoly{T, S, N}, k::Int, n::Int)
   exp = a.exps[n][k]
   Ae = Array(NTuple{N, UInt}, 0)
   Ac = Array(T, 0)
   for i = n:a.length
      if a.exps[i][k] != exp
         break
      end
      push!(Ae, ntuple(j -> j == k ? UInt(0) : a.exps[i][j], Val{N}))
      push!(Ac, a.coeffs[i])
   end
   return parent(a)(Ac, Ae)
end

function main_variable_coefficient{T <: RingElem, S, N}(a::GenMPoly{T, S, N}, k::Int, n::Int, ::Type{Val{:lex}})
   return main_variable_coefficient_lex(a, k, n)
end

function main_variable_coefficient{T <: RingElem, S, N}(a::GenMPoly{T, S, N}, k::Int, n::Int, ::Type{Val{:revlex}})
   return main_variable_coefficient_lex(a, k, n)
end

function main_variable_coefficient_deglex{T <: RingElem, S, N}(a::GenMPoly{T, S, N}, k::Int, n::Int)
   exp = a.exps[n][k]
   Ae = Array(NTuple{N, UInt}, 0)
   Ac = Array(T, 0)
   for i = n:a.length
      if a.exps[i][k] != exp
         break
      end
      push!(Ae, ntuple(j -> j == 1 ? a.exps[i][1] - a.exps[i][k] : (j == k ? UInt(0) : a.exps[i][j]), Val{N}))
      push!(Ac, a.coeffs[i])
   end
   return parent(a)(Ac, Ae)
end

function main_variable_coefficient{T <: RingElem, S, N}(a::GenMPoly{T, S, N}, k::Int, n::Int, ::Type{Val{:deglex}})
   return main_variable_coefficient_deglex(a, k, n)
end

function main_variable_coefficient{T <: RingElem, S, N}(a::GenMPoly{T, S, N}, k::Int, n::Int, ::Type{Val{:degrevlex}})
   return main_variable_coefficient_deglex(a, k, n)
end

function main_variable_extract{T <: RingElem, S, N}(a::GenMPoly{T, S, N}, k::Int)
   V = [(a.exps[i][k], i) for i in 1:length(a)]
   sort!(V)
   Rc = [a.coeffs[V[i][2]] for i in 1:length(a)]
   Re = [a.exps[V[i][2]] for i in 1:length(a)]
   a2 = parent(a)(Rc, Re)
   A = main_variable_terms(a2, k)
   Pe = Array(UInt, length(A))
   Pc = Array(GenMPoly{T, S, N}, length(A))
   for i = 1:length(A)
      Pe[i] = a2.exps[A[i]][k]
      Pc[i] = main_variable_coefficient(a2, k, A[i], Val{S})
   end
   if S == :lex || S == :revlex
      sym = parent(a).S[k]
   else
      sym = parent(a).S[k - 1]
   end
   R = GenSparsePolyRing{GenMPoly{T, S, N}}(parent(a), sym, true)
   return R(Pc, Pe)
end

function main_variable_insert_lex{T <: RingElem, S, N}(a::GenSparsePoly{GenMPoly{T, S, N}}, k::Int)
   V = [(ntuple(i -> i == k ? a.exps[r] : a.coeffs[r].exps[s][i], Val{N}), r, s) for
       r in 1:length(a) for s in 1:length(a.coeffs[r])]
   sort!(V)
   Rc = [a.coeffs[V[i][2]].coeffs[V[i][3]] for i in 1:length(V)]
   Re = [V[i][1] for i in 1:length(V)]
   return base_ring(a)(Rc, Re)
end

function main_variable_insert_deglex{T <: RingElem, S, N}(a::GenSparsePoly{GenMPoly{T, S, N}}, k::Int)
   V = [(ntuple(i -> i == 1 ? a.exps[r] + a.coeffs[r].exps[s][1] : (i == k ? a.exps[r] :
        a.coeffs[r].exps[s][i]), Val{N}), r, s) for r in 1:length(a) for s in 1:length(a.coeffs[r])]
   sort!(V)
   Rc = [a.coeffs[V[i][2]].coeffs[V[i][3]] for i in 1:length(V)]
   Re = [V[i][1] for i in 1:length(V)]
   return base_ring(a)(Rc, Re)
end

function main_variable_insert{T <: RingElem, S, N}(a::GenSparsePoly{GenMPoly{T, S, N}}, k::Int)
   if S == :lex || S == :revlex
      return main_variable_insert_lex(a, k)
   else
      return main_variable_insert_deglex(a, k)
   end
end

###############################################################################
#
#   Unsafe functions
#
###############################################################################

function mul!{T <: RingElem, S, N}(a::GenMPoly{T, S, N}, b::GenMPoly{T, S, N}, c::GenMPoly{T, S, N})
   t = b*c
   a.coeffs = t.coeffs
   a.exps = t.exps
   a.length = t.length
   return
end

function addeq!{T <: RingElem, S, N}(a::GenMPoly{T, S, N}, b::GenMPoly{T, S, N})
   t = a + b
   a.coeffs = t.coeffs
   a.exps = t.exps
   a.length = t.length
   return
end

function fit!{T <: RingElem, S, N}(a::GenMPoly{T, S, N}, n::Int)
   if length(a.coeffs) < n
      resize!(a.coeffs, n)
      resize!(a.exps, n)
   end
end

function zero!{T <: RingElem, S, N}(a::GenMPoly{T, S, N})
   a.length = 0
end

###############################################################################
#
#   Promotion rules
#
###############################################################################

Base.promote_rule{T <: RingElem, V <: Integer}(::Type{GenMPoly{T}}, ::Type{V}) = GenMPoly{T}

Base.promote_rule{T <: RingElem}(::Type{GenMPoly{T}}, ::Type{T}) = GenMPoly{T}

function promote_rule1{T <: RingElem, U <: RingElem}(::Type{GenMPoly{T}}, ::Type{GenMPoly{U}})
   Base.promote_rule(T, GenMPoly{U}) == T ? GenMPoly{T} : Union{}
end

function Base.promote_rule{T <: RingElem, U <: RingElem}(::Type{GenMPoly{T}}, ::Type{U})
   Base.promote_rule(T, U) == T ? GenMPoly{T} : promote_rule1(U, GenMPoly{T})
end

###############################################################################
#
#   Parent object call overload
#
###############################################################################

function (a::GenMPolyRing{T, S, N}){T <: RingElem, S, N}(b::RingElem)
   return a(base_ring(a)(b), a.vars)
end

function (a::GenMPolyRing{T, S, N}){T <: RingElem, S, N}()
   z = GenMPoly{T, S, N}()
   z.parent = a
   return z
end

function (a::GenMPolyRing{T, S, N}){T <: RingElem, S, N}(b::Integer)
   z = GenMPoly{T, S, N}(base_ring(a)(b))
   z.parent = a
   return z
end

function (a::GenMPolyRing{T, S, N}){T <: RingElem, S, N}(b::T)
   parent(b) != base_ring(a) && error("Unable to coerce to polynomial")
   z = GenMPoly{T, S, N}(b)
   z.parent = a
   return z
end

function (a::GenMPolyRing{T, S, N}){T <: RingElem, S, N}(b::PolyElem{T})
   parent(b) != a && error("Unable to coerce polynomial")
   return b
end

function (a::GenMPolyRing{T, S, N}){T <: RingElem, S, N}(b::Array{T, 1}, m::Array{NTuple{N, UInt}, 1})
   if length(b) > 0
      parent(b[1]) != base_ring(a) && error("Unable to coerce to polynomial")
   end
   z = GenMPoly{T, S, N}(b, m)
   z.parent = a
   return z
end

###############################################################################
#
#   PolynomialRing constructor
#
###############################################################################

doc"""
    PolynomialRing(R::Ring, s::Array{String, 1}; cached::Bool = true, S::Symbol = :lex)
> Given a base ring `R` and an array of strings `s` specifying how the
> generators (variables) should be printed, return a tuple `S, x1, x2, ...`
> representing the new polynomial ring $T = R[x1, x2, ...]$ and the generators
> $x1, x2, ...$ of the polynomial ring. By default the parent object `T` will
> depend only on `R` and `x1, x2, ...` and will be cached. Setting the optional
> argument `cached` to `false` will prevent the parent object `T` from being
> cached. `S` is a symbol corresponding to the ordering of the polynomial and
> can be one of `:lex`, `:deglex`, `:revlex` or `:degrevlex`.
"""
function PolynomialRing(R::Ring, s::Array{String, 1}; cached::Bool = true, ordering::Symbol = :lex)
   U = [Symbol(x) for x in s]
   T = elem_type(R)
   N = (ordering == :deglex || ordering == :degrevlex) ? length(U) + 1 : length(U)
   parent_obj = GenMPolyRing{T, ordering, N}(R, U, cached)

   return tuple(parent_obj, gens(parent_obj))
end
