###############################################################################
#
#   Poly.jl : Generic polynomials over rings
#
###############################################################################

export GenPoly, GenPolynomialRing, PolynomialRing, hash, coeff, isgen, lead,
       var, truncate, mullow, reverse, shift_left, shift_right, divexact,
       pseudorem, pseudodivrem, gcd, degree, content, primpart, evaluate, 
       compose, derivative, integral, resultant, discriminant, gcdx, zero, one,
       gen, length, iszero, normalise, isone, isunit, addeq!, mul!, fit!,
       setcoeff!, mulmod, powmod, invmod, lcm, divrem, mod, gcdinv,
       canonical_unit, var, chebyshev_t, chebyshev_u, set_length!,
       mul_classical, sqr_classical, mul_ks, subst, mul_karatsuba,
       pow_multinomial, monomial_to_newton!, newton_to_monomial!

###############################################################################
#
#   Data type and parent object methods
#
###############################################################################

parent_type{T}(::Type{GenPoly{T}}) = GenPolynomialRing{T}

elem_type{T <: RingElem}(::PolyRing{T}) = GenPoly{T}

base_ring{T}(a::PolyRing{T}) = a.base_ring::parent_type(T)

base_ring(a::PolyElem) = base_ring(parent(a))

parent(a::PolyElem) = a.parent

var(a::PolyRing) = a.S

function check_parent(a::PolyElem, b::PolyElem)
   parent(a) != parent(b) && 
                error("Incompatible polynomial rings in polynomial operation")
end

###############################################################################
#
#   Basic manipulation
#
###############################################################################    

function Base.hash(a::PolyElem, h::UInt)
   b = 0x53dd43cd511044d1%UInt
   for i in 0:length(a) - 1
      b $= hash(coeff(a, i), h) $ h
      b = (b << 1) | (b >> (sizeof(Int)*8 - 1))
   end
   return b
end

function normalise(a::PolyElem, len::Int)
   while len > 0 && iszero(a.coeffs[len]) 
      len -= 1
   end
   return len
end

function set_length!(a::PolyElem, len::Int)
   a.length = len
end

length(x::PolyElem) = x.length

degree(x::PolyElem) = length(x) - 1

coeff(a::PolyElem, n::Int) = n >= length(a) ? base_ring(a)(0) : a.coeffs[n + 1]

lead(a::PolyElem) = length(a) == 0 ? base_ring(a)(0) : coeff(a, length(a) - 1)

zero(a::PolyRing) = a(0)

one(a::PolyRing) = a(1)

gen(a::PolyRing) = a([zero(base_ring(a)), one(base_ring(a))])

iszero(a::PolyElem) = length(a) == 0

isone(a::PolyElem) = length(a) == 1 && isone(coeff(a, 0))

function isgen(a::PolyElem)
    return length(a) == 2 && iszero(coeff(a, 0)) && isone(coeff(a, 1))
end

isunit(a::PolyElem) = length(a) == 1 && isunit(coeff(a, 0))

function deepcopy{T <: RingElem}(a::PolyElem{T})
   coeffs = Array(T, length(a))
   for i = 1:length(a)
      coeffs[i] = deepcopy(coeff(a, i - 1))
   end
   return parent(a)(coeffs)
end

###############################################################################
#
#   Canonicalisation
#
###############################################################################

canonical_unit{T <: RingElem}(x::PolyElem{T}) = canonical_unit(lead(x))

###############################################################################
#
#   AbstractString{} I/O
#
###############################################################################

function show{T <: RingElem}(io::IO, x::PolyElem{T})
   len = length(x)
   S = var(parent(x))

   if len == 0
      print(io, base_ring(x)(0))
   else
      for i = 1:len - 1
         c = coeff(x, len - i)
         bracket = needs_parentheses(c)
         if !iszero(c)
            if i != 1 && !is_negative(c)
               print(io, "+")
            end
            if !isone(c) && (c != -1 || show_minus_one(typeof(c)))
               if bracket
                  print(io, "(")
               end
               show(io, c)
               if bracket
                  print(io, ")")
               end
               print(io, "*")
            end
            if c == -1 && !show_minus_one(typeof(c))
               print(io, "-")
            end
            print(io, string(S))
            if len - i != 1
               print(io, "^")
               print(io, len - i)
            end
         end
      end
      c = coeff(x, 0)
      bracket = needs_parentheses(c)
      if !iszero(c)
         if len != 1 && !is_negative(c)
            print(io, "+")
         end
         if bracket
            print(io, "(")
         end
         show(io, c)
         if bracket
            print(io, ")")
         end
      end
   end
end

function show{T <: RingElem}(io::IO, p::GenPolynomialRing{T})
   print(io, "Univariate Polynomial Ring in ")
   print(io, string(var(p)))
   print(io, " over ")
   show(io, p.base_ring)
end

needs_parentheses(x::PolyElem) = length(x) > 1

is_negative(x::PolyElem) = length(x) <= 1 && is_negative(coeff(x, 0))

show_minus_one{T <: RingElem}(::Type{PolyElem{T}}) = show_minus_one(T)

###############################################################################
#
#   Unary operations
#
###############################################################################

function -{T <: RingElem}(a::PolyElem{T})
   len = length(a)
   d = Array(T, len)
   for i = 1:len
      d[i] = -coeff(a, i - 1)
   end
   z = parent(a)(d)
   set_length!(z, len)
   return z
end

###############################################################################
#
#   Binary operations
#
###############################################################################

function +{T <: RingElem}(a::PolyElem{T}, b::PolyElem{T})
   check_parent(a, b)
   lena = length(a)
   lenb = length(b)
   lenz = max(lena, lenb)
   d = Array(T, lenz)
   i = 1

   while i <= min(lena, lenb)
      d[i] = coeff(a, i - 1) + coeff(b, i - 1)
      i += 1
   end

   while i <= lena
      d[i] = coeff(a, i - 1)
      i += 1
   end

   while i <= lenb
      d[i] = coeff(b, i - 1)
      i += 1
   end

   z = parent(a)(d)

   set_length!(z, normalise(z, i - 1))

   return z
end

function -{T <: RingElem}(a::PolyElem{T}, b::PolyElem{T})
   check_parent(a, b)
   lena = length(a)
   lenb = length(b)
   lenz = max(lena, lenb)
   d = Array(T, lenz)
   i = 1

   while i <= min(lena, lenb)
      d[i] = coeff(a, i - 1) - coeff(b, i - 1)
      i += 1
   end

   while i <= lena
      d[i] = coeff(a, i - 1)
      i += 1
   end

   while i <= lenb
      d[i] = -coeff(b, i - 1)
      i += 1
   end

   z = parent(a)(d)

   set_length!(z, normalise(z, i - 1))

   return z
end

function mul_karatsuba{T <: RingElem}(a::GenPoly{T}, b::GenPoly{T})
   # we assume len(a) != 0 != lenb and parent(a) == parent(b)

   lena = length(a)
   lenb = length(b)

   m = div(max(lena, lenb) + 1, 2)

   if m < lena
      a1 = shift_right(a, m)
      a0 = truncate(a, m)
   else
      return a*truncate(b, m) + shift_left(a*shift_right(b, m), m)
   end

   if a !== b
      if m < lenb
         b1 = shift_right(b, m)
         b0 = truncate(b, m)
      else
         return b*truncate(a, m) + shift_left(b*shift_right(a, m), m)
      end
   else
      b1 = a1
      b0 = a0
   end

   z0 = a0*b0
   z2 = a1*b1
   if a !== b
      z1 = (a1 + a0)*(b1 + b0) - z2 - z0
   else
      s = a1 + a0
      z1 = s*s - z2 - z0
   end

   A = Array(T, lena + lenb - 1)

   for i = 1:length(z0)
      A[i] = coeff(z0, i - 1)
   end
   for i = length(z0) + 1:2m
      A[i] = base_ring(a)()
   end

   for i = 1:length(z2)
      A[2m + i] = coeff(z2, i - 1)
   end

   r = parent(a)(A)

   for i = 1:length(z1)
      addeq!(r.coeffs[i + m], coeff(z1, i - 1))
   end

   return r
end

function mul_ks{T <: PolyElem}(a::GenPoly{T}, b::GenPoly{T})
   lena = length(a)
   lenb = length(b)
   if lena == 0 || lenb == 0
      return parent(a)()
   end
   maxa = 0
   nza = 0
   for i = 1:lena
      lenc = length(coeff(a, i - 1))
      maxa = max(lenc, maxa)
      nza += (lenc == 0 ? 0 : 1)
   end
   if a !== b
      maxb = 0
      nzb = 0
      for i = 1:lenb
         lenc = length(coeff(b, i - 1))
         maxb = max(lenc, maxb)
         nzb += (lenc == 0 ? 0 : 1)
      end
   else
      maxb = maxa
      nzb = nza
   end
   if nza*nzb < 4*max(lena, lenb)
      return mul_classical(a, b)
   end
   m = maxa + maxb - 1
   z = base_ring(base_ring(a))()
   A1 = Array(elem_type(base_ring(base_ring(a))), m*lena)
   for i = 1:lena
      c = coeff(a, i - 1)
      for j = 1:length(c)
         A1[(i - 1)*m + j] = coeff(c, j - 1)
      end
      for j = length(c) + 1:m
         A1[(i - 1)*m + j] = z
      end
   end
   ksa = base_ring(a)(A1)
   if a !== b
      A2 = Array(elem_type(base_ring(base_ring(a))), m*lenb)
      for i = 1:lenb
         c = coeff(b, i - 1)
         for j = 1:length(c)
            A2[(i - 1)*m + j] = coeff(c, j - 1)
         end
         for j = length(c) + 1:m
            A2[(i - 1)*m + j] = z
         end
      end
      ksb = base_ring(b)(A2)
   else
      ksb = ksa
   end
   p = ksa*ksb
   r = parent(a)()
   lenr = lena + lenb - 1
   fit!(r, lenr)
   for i = 1:lenr
      fit!(r.coeffs[i], m)
      for j = 1:m
         setcoeff!(r.coeffs[i], j - 1, coeff(p, (i - 1)*m + j - 1))
      end
   end
   set_length!(r, normalise(r, lenr))
   return r
end

function mul_classical{T <: RingElem}(a::GenPoly{T}, b::GenPoly{T})
   lena = length(a)
   lenb = length(b)

   if lena == 0 || lenb == 0
      return parent(a)()
   end

   t = base_ring(a)()

   lenz = lena + lenb - 1
   d = Array(T, lenz)
   
   for i = 1:lena
      d[i] = coeff(a, i - 1)*coeff(b, 0)
   end

   for i = 2:lenb
      d[lena + i - 1] = a.coeffs[lena]*coeff(b, i - 1)
   end
   
   for i = 1:lena - 1
      for j = 2:lenb
         mul!(t, coeff(a, i - 1), b.coeffs[j])
         addeq!(d[i + j - 1], t)
      end
   end
   
   z = parent(a)(d)
        
   set_length!(z, normalise(z, lenz))

   return z
end

function *{T <: RingElem}(a::PolyElem{T}, b::PolyElem{T})
   check_parent(a, b)
   return mul_classical(a, b)
end

###############################################################################
#
#   Ad hoc binary operators
#
###############################################################################

function *{T <: RingElem}(a::Integer, b::PolyElem{T})
   len = length(b)
   d = Array(T, len)
   for i = 1:len
      d[i] = a*coeff(b, i - 1)
   end
   z = parent(b)(d)
   set_length!(z, normalise(z, len))
   return z
end

function *{T <: RingElem}(a::fmpz, b::PolyElem{T})
   len = length(b)
   d = Array(T, len)
   for i = 1:len
      d[i] = a*coeff(b, i - 1)
   end
   z = parent(b)(d)
   set_length!(z, normalise(z, len))
   return z
end

*(a::PolyElem, b::Integer) = b*a

*(a::PolyElem, b::fmpz) = b*a

###############################################################################
#
#   Powering
#
###############################################################################

function pow_multinomial{T <: RingElem}(a::PolyElem{T}, e::Int)
   e < 0 && throw(DomainError())
   lena = length(a)
   lenz = (lena - 1) * e + 1
   res = Array(T, lenz)
   for k = 1:lenz
      res[k] = base_ring(a)()
   end
   d = base_ring(a)()
   first = coeff(a, 0)
   res[1] = first ^ e
   for k = 1 : lenz - 1
      u = -k
      for i = 1 : min(k, lena - 1)
         t = coeff(a, i) * res[(k - i) + 1]
         u += e + 1
         addeq!(res[k + 1], t * u)
      end
      addeq!(d, first)
      res[k + 1] = divexact(res[k + 1], d)
   end
   z = parent(a)(res)
   set_length!(z, normalise(z, lenz))
   return z
end

function ^{T <: RingElem}(a::PolyElem{T}, b::Int)
   b < 0 && throw(DomainError())
   # special case powers of x for constructing polynomials efficiently
   if isgen(a)
      d = Array(T, b + 1)
      d[b + 1] = coeff(a, 1)
      for i = 1:b
         d[i] = coeff(a, 0)
      end
      z = parent(a)(d)
      set_length!(z, b + 1)
      return z
   elseif length(a) == 0
      return zero(parent(a))
   elseif length(a) == 1
      return parent(a)(coeff(a, 0)^b)
   elseif b == 0
      return one(parent(a))
   else
      if T <: FieldElem
         zn = 0
         while iszero(coeff(a, zn))
            zn += 1
         end
         if length(a) - zn < 8 && b > 4
             f = shift_right(a, zn)
             return shift_left(pow_multinomial(f, b), zn*b) 
         end
      end
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

###############################################################################
#
#   Comparisons
#
###############################################################################

function =={T <: RingElem}(x::PolyElem{T}, y::PolyElem{T})
   check_parent(x, y)
   if length(x) != length(y)
      return false
   else
      for i = 1:length(x)
         if coeff(x, i - 1) != coeff(y, i - 1)
            return false
         end
      end
   end
   return true
end

function isequal{T <: RingElem}(x::PolyElem{T}, y::PolyElem{T})
   if parent(x) != parent(y)
      return false
   end
   if length(x) != length(y)
      return false
   end
   for i = 1:length(x)
      if !isequal(coeff(x, i - 1), coeff(y, i - 1))
         return false
      end
   end
   return true
end

###############################################################################
#
#   Ad hoc comparisons
#
###############################################################################

==(x::PolyElem, y::Integer) = ((length(x) == 0 && y == 0)
                        || (length(x) == 1 && coeff(x, 0) == y))

==(x::Integer, y::PolyElem) = y == x

###############################################################################
#
#   Truncation
#
###############################################################################

function truncate{T <: RingElem}(a::PolyElem{T}, n::Int)
   n < 0 && throw(DomainError())
   
   lena = length(a)

   if lena <= n
      return a
   end

   lenz = min(lena, n)
   d = Array(T, lenz)

   for i = 1:lenz
      d[i] = coeff(a, i - 1)
   end

   z = parent(a)(d)

   set_length!(z, normalise(z, lenz))

   return z
end

function mullow{T <: RingElem}(a::PolyElem{T}, b::PolyElem{T}, n::Int)
   check_parent(a, b)
   lena = length(a)
   lenb = length(b)

   if lena == 0 || lenb == 0
      return zero(parent(a))
   end

   if n < 0
      n = 0
   end

   t = T()

   lenz = min(lena + lenb - 1, n)

   d = Array(T, lenz)

   for i = 1:min(lena, lenz)
      d[i] = coeff(a, i - 1)*coeff(b, 0)
   end

   if lenz > lena
      for j = 2:min(lenb, lenz - lena + 1)
          d[lena + j - 1] = coeff(a, lena - 1)*coeff(b, j - 1)
      end
   end

   for i = 1:lena - 1
      if lenz > i
         for j = 2:min(lenb, lenz - i + 1)
            mul!(t, coeff(a, i - 1), b.coeffs[j])
            addeq!(d[i + j - 1], t)
         end
      end
   end
     
   z = parent(a)(d)
   
   set_length!(z, normalise(z, lenz))

   return z
end

###############################################################################
#
#   Reversal
#
###############################################################################

function reverse{T <: RingElem}(x::PolyElem{T}, len::Int)
   len < 0 && throw(DomainError())
   v = Array(T, len)
   for i = 1:len
      v[i] = coeff(x, len - i)
   end
   r = parent(x)(v)
   set_length!(r, normalise(r, len))
   return r
end

function reverse(x::PolyElem)
   reverse(x, length(x))
end

###############################################################################
#
#   Shifting
#
###############################################################################

function shift_left{T <: RingElem}(x::PolyElem{T}, len::Int)
   len < 0 && throw(DomainError())
   if len == 0
      return x
   end
   xlen = length(x)
   v = Array(T, xlen + len)
   for i = 1:len
      v[i] = zero(base_ring(x))
   end
   for i = 1:xlen
      v[i + len] = coeff(x, i - 1)
   end
   return parent(x)(v)
end

function shift_right{T <: RingElem}(x::PolyElem{T}, len::Int)
   len < 0 && throw(DomainError())
   xlen = length(x)
   if len >= xlen
      return zero(parent(x))
   end
   if len == 0
      return x
   end
   v = Array(T, xlen - len)
   for i = 1:xlen - len
      v[i] = coeff(x, i + len - 1)
   end
   return parent(x)(v)
end

###############################################################################
#
#   Modular arithmetic
#
###############################################################################

function mulmod{T <: Union{ResElem, FieldElem}}(a::PolyElem{T}, b::PolyElem{T}, d::PolyElem{T})
   check_parent(a, b)
   check_parent(a, d)
   return mod(a*b, d)
end

function powmod{T <: Union{ResElem, FieldElem}}(a::PolyElem{T}, b::Int, d::PolyElem{T})
   check_parent(a, d)
   if length(a) == 0
      return zero(parent(a))
   elseif length(a) == 1
      return parent(a)(coeff(a, 0)^b)
   elseif b == 0
      return one(parent(a))
   else
      if b < 0
         a = invmod(a, d)
         b = -b
      end
      bit = ~((~UInt(0)) >> 1)
      while (UInt(bit) & b) == 0
         bit >>= 1
      end
      z = a
      bit >>= 1
      while bit !=0
         z = mulmod(z, z, d)
         if (UInt(bit) & b) != 0
            z = mulmod(z, a, d)
         end
         bit >>= 1
      end
      return z
   end
end

function invmod{T <: Union{ResElem, FieldElem}}(a::PolyElem{T}, b::PolyElem{T})
   check_parent(a, b)
   g, z = gcdinv(a, b)
   if g != 1
      error("Impossible inverse in invmod")
   end
   return z
end

###############################################################################
#
#   Exact division
#
###############################################################################

function divexact{T <: RingElem}(f::PolyElem{T}, g::PolyElem{T})
   check_parent(f, g)
   g == 0 && throw(DivideError())
   if f == 0
      return zero(parent(f))
   end
   lenq = length(f) - length(g) + 1
   d = Array(T, lenq)
   for i = 1:lenq
      d[i] = zero(base_ring(f))
   end
   x = gen(parent(f))
   leng = length(g)
   while length(f) >= leng
      lenf = length(f)
      q1 = d[lenf - leng + 1] = divexact(coeff(f, lenf - 1), coeff(g, leng - 1))
      f = f - shift_left(q1*g, lenf - leng)
   end
   q = parent(f)(d)
   set_length!(q, lenq)
   return q
end

###############################################################################
#
#   Ad hoc exact division
#
###############################################################################

function divexact{T <: RingElem}(a::PolyElem{T}, b::T)
   b == 0 && throw(DivideError())
   d = Array(T, length(a))
   for i = 1:length(a)
      d[i] = divexact(coeff(a, i - 1), b)
   end
   z = parent(a)(d)
   set_length!(z, length(a))
   return z
end

function divexact{T <: RingElem}(a::PolyElem{T}, b::Integer)
   b == 0 && throw(DivideError())
   d = Array(T, length(a))
   for i = 1:length(a)
      d[i] = divexact(coeff(a, i - 1), b)
   end
   z = parent(a)(d)
   set_length!(z, length(a))
   return z
end

###############################################################################
#
#   Euclidean division
#
###############################################################################

function mod{T <: Union{ResElem, FieldElem}}(f::PolyElem{T}, g::PolyElem{T})
   check_parent(f, g)
   if length(g) == 0
      raise(DivideError())
   end
   if length(f) >= length(g)
      f = deepcopy(f)
      b = lead(g)
      g = inv(b)*g
      x = gen(parent(f))
      c = base_ring(f)()
      while length(f) >= length(g)
         l = -lead(f)
         for i = 1:length(g)
            mul!(c, coeff(g, i - 1), l)
            u = coeff(f, i + length(f) - length(g) - 1)
            addeq!(u, c)
            setcoeff!(f, i + length(f) - length(g) - 1, u)
         end
         set_length!(f, normalise(f, length(f)))
      end
   end
   return f
end

function divrem{T <: Union{ResElem, FieldElem}}(f::PolyElem{T}, g::PolyElem{T})
   check_parent(f, g)
   if length(g) == 0
      raise(DivideError())
   end
   if length(f) < length(g)
      return zero(parent(f)), f
   end
   f = deepcopy(f)
   binv = inv(lead(g)) 
   g = binv*g
   x = gen(parent(f))
   qlen = length(f) - length(g) + 1
   d = Array(T, qlen)
   for i = 1:qlen
      d[i] = zero(base_ring(f))
   end
   q = parent(f)(d)
   c = base_ring(f)()
   while length(f) >= length(g)
      q1 = lead(f)
      l = -q1
      setcoeff!(q, length(f) - length(g), q1*binv)
      for i = 1:length(g)
         mul!(c, coeff(g, i - 1), l)
         u = coeff(f, i + length(f) - length(g) - 1)
         addeq!(u, c)
         setcoeff!(f, i + length(f) - length(g) - 1, u)
      end
      set_length!(f, normalise(f, length(f)))
   end
   return q, f
end

###############################################################################
#
#   Pseudodivision
#
###############################################################################

function pseudorem{T <: RingElem}(f::PolyElem{T}, g::PolyElem{T})
   check_parent(f, g)
   g == 0 && throw(DivideError())
   b = coeff(g, length(g) - 1)
   x = gen(parent(f))
   while length(f) >= length(g)
      f = f*b - shift_left(coeff(f, length(f) - 1)*g, length(f) - length(g))
   end
   return f
end

function pseudodivrem{T <: RingElem}(f::PolyElem{T}, g::PolyElem{T})
   check_parent(f, g)
   g == 0 && throw(DivideError())
   if length(f) < length(g)
      return zero(parent(f)), f
   end
   lenq = length(f) - length(g) + 1
   v = Array(T, lenq)
   for i = 1:lenq
      v[i] = zero(base_ring(f))
   end
   q = parent(f)(v)
   b = coeff(g, length(g) - 1)
   x = gen(parent(f))
   while length(f) >= length(g)
      for i = length(f) - length(g) + 2:lenq
         setcoeff!(q, i - 1, coeff(q, i - 1) * b)
      end
      setcoeff!(q, length(f) - length(g), coeff(f, length(f) - 1))
      f = f*b - shift_left(coeff(f, length(f) - 1)*g, length(f) - length(g))
   end
   while lenq > 0 && coeff(q, lenq - 1) == 0
      lenq -= 1
   end
   set_length!(q, lenq)
   return q, f
end

###############################################################################
#
#   Content, primitive part, GCD and LCM
#
###############################################################################

function gcd{T <: RingElem}(a::PolyElem{T}, b::PolyElem{T})
   check_parent(a, b)
   if length(b) > length(a)
      (a, b) = (b, a)
   end
   if b == 0
      return a
   end
   if b == 1
      return b
   end
   c = gcd(content(a), content(b))
   a = divexact(a, c)
   b = divexact(b, c)
   g = one(parent(a))
   h = one(parent(a))
   while true
      d = length(a) - length(b)
      r = pseudorem(a, b)
      if r == 0
         break
      end
      if length(r) == 1
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
   return c*primpart(b)
end

function gcd{T <: Union{ResElem, FieldElem}}(a::PolyElem{T}, b::PolyElem{T})
   check_parent(a, b)
   if length(a) > length(b)
      (a, b) = (b, a)
   end
   if b == 0
      return a
   end
   g = gcd(content(a), content(b))
   a = divexact(a, g)
   b = divexact(b, g)
   while a != 0
      (a, b) = (mod(b, a), a)
   end
   b = g*b
   return inv(lead(b))*b
end

function lcm(a::PolyElem, b::PolyElem)
   check_parent(a, b)
   return a*divexact(b, gcd(a, b))
end

function content(a::PolyElem)
   z = coeff(a, 0)
   for i = 2:length(a)
      z = gcd(z, coeff(a, i - 1))
   end
   return z
end

function primpart(a::PolyElem)
   d = content(a)
   return divexact(a, d)
end

###############################################################################
#
#   Evaluation/composition
#
###############################################################################

function evaluate{T <: RingElem}(a::PolyElem{T}, b::T)
   i = length(a)
   if i == 0
       return zero(base_ring(a))
   end
   if i > 25
      return subst(a, b)
   end
   z = coeff(a, i - 1)
   while i > 1
      i -= 1
      z = z*b + coeff(a, i - 1)
   end
   return z
end

function evaluate{T <: RingElem}(a::PolyElem{T}, b::Integer)
   return evaluate(a, base_ring(a)(b))
end

function evaluate{T <: RingElem}(a::PolyElem{T}, b::fmpz)
   return evaluate(a, base_ring(a)(b))
end

function compose(a::PolyElem, b::PolyElem)
   i = length(a)
   if i == 0
       return zero(parent(a))
   end
   if i*length(b) > 25
      return subst(a, b)
   end
   z = coeff(a, i - 1)
   while i > 1
      i -= 1
      z = z*b + coeff(a, i - 1)
   end
   return z
end

###############################################################################
#
#   Derivative
#
###############################################################################

function derivative{T <: RingElem}(a::PolyElem{T})
   if a == 0
      return zero(parent(a))
   end
   len = length(a)
   d = Array(T, len - 1)
   for i = 1:len - 1
      d[i] = i*coeff(a, i)
   end
   z = parent(a)(d)
   set_length!(z, normalise(z, len - 1))
   return z
end

###############################################################################
#
#   Integral
#
###############################################################################

function integral{T <: Union{ResElem, FieldElem}}(x::PolyElem{T})
   len = length(x)
   v = Array(T, len + 1)
   v[1] = zero(base_ring(x))
   for i = 1:len
      v[i + 1] = divexact(coeff(x, i - 1), base_ring(x)(i))
   end
   p = parent(x)(v)
   len += 1
   while len > 0 && coeff(p, len - 1) == 0 # cannot use normalise here
      len -= 1
   end
   set_length!(p, len)
   return p
end

###############################################################################
#
#   Resultant
#
###############################################################################

function resultant{T <: RingElem}(a::PolyElem{T}, b::PolyElem{T})
   check_parent(a, b)
   if length(a) == 0 || length(b) == 0
      return zero(base_ring(a))
   end
   sgn = 1
   if length(a) < length(b)
      a, b = b, a
      if iseven(length(a)) && iseven(length(b))
         sgn = -sgn
      end
   end
   lena = length(a)
   lenb = length(b)
   if lenb == 1
      return coeff(b, 0)^(lena - 1)
   end
   c1 = content(a)
   c2 = content(b)
   A = divexact(a, c1)
   B = divexact(b, c2)
   g = one(base_ring(a))
   h = one(base_ring(a))
   while lenb > 1
      d = lena - lenb
      if iseven(lena) && iseven(lenb)
         sgn = -sgn
      end
      B, A = pseudorem(A, B), B
      lena = lenb
      lenb = length(B)
      if lenb == 0
         return zero(base_ring(a)) 
      end
      s = h^d
      B = divexact(B, g*s)
      g = lead(A)
      h = divexact(h*g^d, s)
   end
   s = divexact(h*lead(B)^(lena - 1), h^(lena - 1))
   res = c1^(lenb - 1)*c2^(lena - 1)*s*sgn
end

function resultant_lehmer{T <: Union{ResElem, FieldElem}}(a::PolyElem{T}, b::PolyElem{T})
   const crossover = 40
   R = base_ring(a)
   check_parent(a, b)
   if length(a) == 0 || length(b) == 0
      return zero(base_ring(a))
   end
   sgn = 1
   if length(a) < length(b)
      a, b = b, a
      if iseven(length(a)) && iseven(length(b))
         sgn = -sgn
      end
   end
   lenA = length(a)
   lenB = length(b)
   if lenB == 1
      return coeff(b, 0)^(lenA - 1)
   end
   c1 = content(a)
   c2 = content(b)
   A = divexact(a, c1)
   B = divexact(b, c2)
   s = R(1)
   while lenB > crossover/2 + 1
      shift = max(lenA - crossover, 0)
      a = shift_right(A, shift)
      b = shift_right(B, shift)
      u1, v1 = R(1), R(0)
      u2, v2 = R(0), R(1)
      lena = lenA - shift
      lenb = lenB - shift
      if lenb > crossover/2 + 1
         A = truncate(A, shift)
         B = truncate(B, shift)
         while lenb > crossover/2 + 1
            if iseven(lena + shift) && iseven(lenb + shift)
               sgn = -sgn
            end
            (q, b), a = divrem(a, b), b
            u1, u2 = u2, u1 - q*u2
            v1, v2 = v2, v1 - q*v2
            s *= lead(a)^(lena - length(b))
            lena = lenb
            lenb = length(b)
         end
         A, B = u1*A + v1*B + shift_left(a, shift), u2*A + v2*B + shift_left(b, shift)
      else
         if iseven(lenA) && iseven(lenB)
               sgn = -sgn
         end
         B, A = mod(A, B), B
         s *= lead(A)^(lenA - length(B))
      end
      lenA = length(A)
      lenB = length(B)
      if lenB == 0
         return zero(base_ring(a)), parent(A)(1), parent(A)(1)
      end
   end
   while lenB > 1
      if iseven(lenA) && iseven(lenB)
         sgn = -sgn
      end
      B, A = mod(A, B), B
      s *= lead(A)^(lenA - length(B))
      lenA = lenB
      lenB = length(B)
      if lenB == 0
         return zero(base_ring(A)) 
      end
   end
   s *= lead(B)^(lenA - 1)
   return c1^(lenB - 1)*c2^(lenA - 1)*s*sgn
end

function resultant{T <: Union{ResElem, FieldElem}}(a::PolyElem{T}, b::PolyElem{T})
   check_parent(a, b)
   if length(a) == 0 || length(b) == 0
      return zero(base_ring(a))
   end
   sgn = 1
   if length(a) < length(b)
      a, b = b, a
      if iseven(length(a)) && iseven(length(b))
         sgn = -sgn
      end
   end
   lena = length(a)
   lenb = length(b)
   if lenb == 1
      return coeff(b, 0)^(lena - 1)
   end
   c1 = content(a)
   c2 = content(b)
   A = divexact(a, c1)
   B = divexact(b, c2)
   s = base_ring(A)(1)
   lena = length(A)
   lenb = length(B)
   while lenb > 1
      if iseven(lena) && iseven(lenb)
         sgn = -sgn
      end
      B, A = mod(A, B), B
      s *= lead(A)^(lena - length(B))
      lena = lenb
      lenb = length(B)
      if lenb == 0
         return zero(base_ring(a)) 
      end
   end
   s *= lead(B)^(lena - 1)
   return c1^(lenb - 1)*c2^(lena - 1)*s*sgn
end

###############################################################################
#
#   Discriminant
#
###############################################################################

function discriminant(a::PolyElem)
   d = derivative(a)
   z = resultant(a, d)
   if length(a) - length(d) == 1
      z = divexact(z, lead(a))
   else
      z = z*lead(a)^(length(a) - length(d) - 2)
   end
   mod4 = (length(a) + 3) % 4 # degree mod 4
   return mod4 == 2 || mod4 == 3 ? -z : z
end

###############################################################################
#
#   GCDX
#
###############################################################################

function gcdx{T <: RingElem}(a::PolyElem{T}, b::PolyElem{T})
   check_parent(a, b)
   sgn = 1
   swap = false
   if length(a) < length(b)
      a, b = b, a
      swap = true
      if iseven(length(a)) && iseven(length(b))
         sgn = -sgn
      end
   end
   lena = length(a)
   lenb = length(b)
   (lena <= 1 || lenb <= 1) && error("Constant polynomial in gcdx")  
   c1 = content(a)
   c2 = content(b)
   A = divexact(a, c1)
   B = divexact(b, c2)
   g = one(base_ring(a))
   h = one(base_ring(a))
   u1, u2 = one(parent(a)), zero(parent(a))
   v1, v2 = zero(parent(a)), one(parent(a))
   while lenb > 1
      d = lena - lenb
      if iseven(lena) && iseven(lenb)
         sgn = -sgn
      end
      (Q, B), A = pseudodivrem(A, B), B
      lena = lenb
      lenb = length(B)
      if lenb == 0
         return zero(base_ring(a)), zero(parent(a)), zero(parent(a))
      end
      s = h^d
      B = divexact(B, g*s)
      t = lead(A)^(d + 1)
      u2, u1 = divexact(u1*t - Q*u2, g*s), u2
      v2, v1 = divexact(v1*t - Q*v2, g*s), v2 
      g = lead(A)
      h = divexact(h*g^d, s)
   end
   s = divexact(h*lead(B)^(lena - 1), h^(lena - 1))
   res = c1^(length(b) - 1)*c2^(length(a) - 1)*s*sgn
   u2 *= c1^(length(b) - 2)*c2^(length(a) - 1)
   v2 *= c1^(length(b) - 1)*c2^(length(a) - 2)
   if swap
      u2, v2 = v2, u2
   end
   return res, u2, v2
end

function gcdx{T <: Union{ResElem, FieldElem}}(a::PolyElem{T}, b::PolyElem{T})
   check_parent(a, b)
   if length(a) == 0
      return b, zero(parent(a)), one(parent(a))
   end
   if length(b) == 0
      return a, one(parent(a)), zero(parent(a))
   end
   swap = false
   if length(a) < length(b)
      a, b = b, a
      swap = true
   end
   lena = length(a)
   lenb = length(b)
   c1 = content(a)
   c2 = content(b)
   A = divexact(a, c1)
   B = divexact(b, c2)
   u1, u2 = inv(c1), zero(parent(a))
   v1, v2 = zero(parent(a)), inv(c2)
   while lenb > 0
      d = lena - lenb
      (Q, B), A = divrem(A, B), B
      lena = lenb
      lenb = length(B)
      u2, u1 = u1 - Q*u2, u2
      v2, v1 = v1 - Q*v2, v2 
   end
   if swap
      u1, v1 = v1, u1
   end
   d = gcd(c1, c2)
   A, u1, v1 = d*A, d*u1, d*v1
   d = inv(lead(A))
   return d*A, d*u1, d*v1
end

function gcdinv{T <: Union{ResElem, FieldElem}}(a::PolyElem{T}, b::PolyElem{T})
   check_parent(a, b)
   if length(a) == 0
      if length(b) == 0
         return zero(base_ring(a)), zero(parent(a))
      else
         d = inv(lead(b))
         return b*d, zero(parent(a))
      end
   end
   if length(b) == 0
      d = inv(lead(b))
      return a*d, d
   end
   if length(a) < length(b)
      a, b = b, a
      u1, u2 = zero(parent(a)), one(parent(a))
   else
      u1, u2 = one(parent(a)), zero(parent(a))
   end
   lena = length(a)
   lenb = length(b)
   c1 = content(a)
   c2 = content(b)
   A = divexact(a, c1)
   B = divexact(b, c2)
   u1 *= inv(c1)
   u2 *= inv(c2)
   while lenb > 0
      d = lena - lenb
      (Q, B), A = divrem(A, B), B
      lena = lenb
      lenb = length(B)
      u2, u1 = u1 - Q*u2, u2
   end
   d = gcd(c1, c2)
   A, u1 = d*A, d*u1
   d = inv(lead(A))
   return d*A, d*u1
end

###############################################################################
#
#   Newton representation
#
###############################################################################

function monomial_to_newton!{T <: RingElem}(P::Array{T, 1}, roots::Array{T, 1})
   n = length(roots)
   if n > 0
      R = parent(roots[1])
      t = R()
      for i = 1:n - 1
         for j = n - 1:-1:i
            mul!(t, P[j + 1], roots[i])
            addeq!(P[j], t)
         end
      end
   end
   return
end

function newton_to_monomial!{T <: RingElem}(P::Array{T, 1}, roots::Array{T, 1})
   n = length(roots)
   if n > 0
      R = parent(roots[1])
      t = R()
      for i = n - 1:-1:1
         d = -roots[i]
         for j = i:n - 1
            mul!(t, P[j + 1], d)
            addeq!(P[j], t)
         end
      end
   end
   return
end

###############################################################################
#
#   Interpolation
#
###############################################################################

function interpolate{T <: RingElem}(S::PolyRing, x::Array{T, 1}, y::Array{T, 1})
   length(x) != length(y) && error("Array lengths don't match in interpolate")
   n = length(x)
   if n == 0
      return S()
   elseif n == 1
      return S(y[1])
   end
   R = base_ring(S)
   parent(y[1]) != R && error("Polynomial ring does not match inputs")
   P = Array{T}(n)
   for i = 1:n
      P[i] = deepcopy(y[i])
   end
   for i = 2:n
      t = P[i - 1]
      for j = i:n
         p = P[j] - t
         q = x[j] - x[j - i + 1]
         t = P[j]
         P[j] = divexact(p, q)
      end
   end
   newton_to_monomial!(P, x)
   r = S(P)
   set_length!(r, normalise(r, n))
   return r
end

###############################################################################
#
#   Special functions
#
###############################################################################

function chebyshev_t_pair{S <: PolyElem}(n::Int, x::S)
   if n == 0
      return one(parent(x)), x
   elseif n == 1
      return x, one(parent(x))
   elseif n < 0
      a, b = chebyshev_t_pair(1 - n, x)
      return b, a
   elseif iseven(n)
      a, b = chebyshev_t_pair(n >> 1, x)
      return 2*(a*a) - 1, 2*(a*b) - x
   else
      a, b = chebyshev_t_pair((n >> 1) + 1, x)
      return 2*(a*b) - x, 2*(b*b) - 1
   end
end

function chebyshev_t{S <: PolyElem}(n::Int, x::S)
   if n == 0
      return one(parent(x))
   elseif n == 1
      return x
   elseif n < 0
      return chebyshev_t(-n, x)
   elseif iseven(n)
      a = chebyshev_t(n >> 1, x)
      return 2*(a*a) - 1
   else
      a, b = chebyshev_t_pair((n >> 1) + 1, x)
      return 2*(a*b) - x
   end
end

function chebyshev_u_pair{S <: PolyElem}(n::Int, x::S)
   if n == 0
      return one(parent(x)), zero(parent(x))
   elseif n == 1
      return 2*x, one(parent(x))
   elseif n == -1
      return zero(parent(x)), -one(parent(x))
   elseif n < -1
      a, b = chebyshev_u_pair(-1 - n, x)
      return -b, -a
   elseif iseven(n)
      a, b = chebyshev_u_pair(n >> 1, x)
      return (a+b)*(a - b), 2*b*(a-x*b)
   else
      a, b = chebyshev_u_pair(n >> 1, x)
      return 2*a*(x*a - b), (a + b)*(a - b)
   end
end

function chebyshev_u{S <: PolyElem}(n::Int, x::S)
   if n == 0
      return one(parent(x))
   elseif n == 1
      return 2*x
   elseif n == -1
      return zero(parent(x))
   elseif n < -1
      return -chebyshev_u(-2 - n, x)
   elseif iseven(n)
      a, b = chebyshev_u_pair(n >> 1, x)
      return (a + b)*(a - b)
   else
      a, b = chebyshev_u_pair(n >> 1, x)
      return 2*a*(x*a - b)
   end
end

###############################################################################
#
#   Unsafe functions
#
###############################################################################

function fit!{T <: RingElem}(c::PolyElem{T}, n::Int)
   if length(c) < n
      t = c.coeffs
      c.coeffs = Array(T, n)
      for i = 1:length(c)
         c.coeffs[i] = t[i]
      end
      for i = length(c) + 1:n
         c.coeffs[i] = zero(base_ring(c))
      end
   end
end

function setcoeff!{T <: RingElem}(c::PolyElem{T}, n::Int, a::T)
   if a != 0 || n + 1 <= length(c)
      fit!(c, n + 1)
      c.coeffs[n + 1] = a
      c.length = max(length(c), n + 1)
      # don't normalise
   end
end

function mul!{T <: RingElem}(c::PolyElem{T}, a::PolyElem{T}, b::PolyElem{T})
   lena = length(a)
   lenb = length(b)

   if lena == 0 || lenb == 0
      c.length = 0
   else
      if a === c
         a = deepcopy(a)
      end
      if b === c
         b = deepcopy(b)
      end

      t = base_ring(a)()

      lenc = lena + lenb - 1
      fit!(c, lenc)

      for i = 1:lena
         mul!(c.coeffs[i], coeff(a, i - 1), coeff(b, 0))
      end

      for i = 2:lenb
         mul!(c.coeffs[lena + i - 1], a.coeffs[lena], coeff(b, i - 1))
      end

      for i = 1:lena - 1
         for j = 2:lenb
            mul!(t, coeff(a, i - 1), b.coeffs[j])
            addeq!(c.coeffs[i + j - 1], t)
         end
      end
        
      c.length = normalise(c, lenc)
   end
end

function addeq!{T <: RingElem}(c::PolyElem{T}, a::PolyElem{T})
   lenc = length(c)
   lena = length(a)
   len = max(lenc, lena)
   fit!(c, len)
   for i = 1:lena
      addeq!(c.coeffs[i], coeff(a, i - 1))
   end
   c.length = normalise(c, len)
end

###############################################################################
#
#   Promotion rules
#
###############################################################################

Base.promote_rule{T <: RingElem, V <: Integer}(::Type{GenPoly{T}}, ::Type{V}) = GenPoly{T}

Base.promote_rule{T <: RingElem}(::Type{GenPoly{T}}, ::Type{T}) = GenPoly{T}

function promote_rule1{T <: RingElem, U <: RingElem}(::Type{GenPoly{T}}, ::Type{GenPoly{U}})
   Base.promote_rule(T, GenPoly{U}) == T ? GenPoly{T} : Union{}
end

function Base.promote_rule{T <: RingElem, U <: RingElem}(::Type{GenPoly{T}}, ::Type{U})
   Base.promote_rule(T, U) == T ? GenPoly{T} : promote_rule1(U, GenPoly{T})
end

###############################################################################
#
#   Polynomial substitution
#
###############################################################################

function subst{T <: RingElem}(f::PolyElem{T}, a)
   S = parent(a)
   n = degree(f)
   if n < 0
      return S()
   elseif n == 0
      return coeff(f, 0)*S(1)
   elseif n == 1
      return coeff(f, 0)*S(1) + coeff(f, 1)*a
   end
   d1 = isqrt(n)
   d = div(n, d1)
   A = powers(a, d)
   s = coeff(f, d1*d)*A[1]
   for j = 1:min(n - d1*d, d - 1)
      c = coeff(f, d1*d + j)
      if c != 0
         s += c*A[j + 1]
      end
   end
   for i = 1:d1
      s *= A[d + 1]
      s += coeff(f, (d1 - i)*d)*A[1]
      for j = 1:min(n - (d1 - i)*d, d - 1)
         c = coeff(f, (d1 - i)*d + j)
         if c != 0
            s += c*A[j + 1]
         end
      end
   end
   return s
end

###############################################################################
#
#   Parent object call overload
#
###############################################################################

function Base.call{T <: RingElem}(a::GenPolynomialRing{T}, b::RingElem)
   return a(base_ring(a)(b))
end

function Base.call{T <: RingElem}(a::GenPolynomialRing{T})
   z = GenPoly{T}()
   z.parent = a
   return z
end

function Base.call{T <: RingElem}(a::GenPolynomialRing{T}, b::Integer)
   z = GenPoly{T}(base_ring(a)(b))
   z.parent = a
   return z
end

function Base.call{T <: RingElem}(a::GenPolynomialRing{T}, b::T)
   parent(b) != base_ring(a) && error("Unable to coerce to polynomial")
   z = GenPoly{T}(b)
   z.parent = a
   return z
end

function Base.call{T <: RingElem}(a::GenPolynomialRing{T}, b::PolyElem{T})
   parent(b) != a && error("Unable to coerce polynomial")
   return b
end

function Base.call{T <: RingElem}(a::GenPolynomialRing{T}, b::Array{T, 1})
   if length(b) > 0
      parent(b[1]) != base_ring(a) && error("Unable to coerce to polynomial")
   end
   z = GenPoly{T}(b)
   z.parent = a
   return z
end

###############################################################################
#
#   PolynomialRing constructor
#
###############################################################################

function PolynomialRing(R::Ring, s::AbstractString{}; cached::Bool = true)
   S = Symbol(s)
   T = elem_type(R)
   parent_obj = GenPolynomialRing{T}(R, S, cached)

   return parent_obj, parent_obj([R(0), R(1)])
end

# S, x = R["x"] syntax
getindex(R::Ring, s::ASCIIString) = PolynomialRing(R, s)

getindex{T}(R::Tuple{Ring,T}, s::ASCIIString) = PolynomialRing(R[1], s)
