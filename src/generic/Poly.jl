###############################################################################
#
#   Poly.jl : Generic polynomials over rings
#
###############################################################################

export GenPoly, GenPolyRing, PolynomialRing, hash, coeff, isgen, lead,
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

parent_type{T}(::Type{GenPoly{T}}) = GenPolyRing{T}

elem_type{T <: RingElem}(::GenPolyRing{T}) = GenPoly{T}

doc"""
    base_ring(R::PolyRing)
> Return the base ring of the given polynomial ring.
"""
base_ring{T}(R::PolyRing{T}) = R.base_ring::parent_type(T)

doc"""
    base_ring(a::PolyElem)
> Return the base ring of the polynomial ring of the given polynomial.
"""
base_ring(a::PolyElem) = base_ring(parent(a))

doc"""
    parent(a::PolyElem)
> Return the parent of the given polynomial.
"""
parent(a::PolyElem) = a.parent

doc"""
    var(a::PolyRing)
> Return the internal name of the generator of the polynomial ring. Note that
> this is returned as a `Symbol` not a `String`.
"""
var(a::PolyRing) = a.S

doc"""
    vars(a::PolyRing)
> Return an array of the variable names for the polynomial ring. Note that
> this is returned as an array of `Symbol` not `String`.
"""
vars(a::PolyRing) = [a.S]

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

function normalise(a::GenPoly, n::Int)
   while n > 0 && iszero(a.coeffs[n]) 
      n -= 1
   end
   return n
end

function set_length!(a::PolyElem, n::Int)
   a.length = n
end

length(a::PolyElem) = a.length

doc"""
    degree(a::PolyElem)
> Return the degree of the given polynomial. This is defined to be one less
> than the length, even for constant polynomials.
"""
degree(a::PolyElem) = length(a) - 1

doc"""
    modulus{T <: ResElem}(a::PolyElem{T})
> Return the modulus of the coefficients of the given polynomial.
"""
modulus{T <: ResElem}(a::PolyElem{T}) = modulus(base_ring(a))

coeff(a::GenPoly, n::Int) = n >= length(a) ? base_ring(a)(0) : a.coeffs[n + 1]

doc"""
    lead(x::PolyElem)
> Return the leading coefficient of the given polynomial. This will be the
> nonzero coefficient of the term with highest degree unless the polynomial
> in the zero polynomial, in which case a zero coefficient is returned.
"""
lead(a::PolyElem) = length(a) == 0 ? base_ring(a)(0) : coeff(a, length(a) - 1)

doc"""
    zero(R::PolyRing)
> Return the zero polynomial in the given polynomial ring.
"""
zero(R::PolyRing) = R(0)

doc"""
    one(R::PolyRing)
> Return the constant polynomial $1$ in the given polynomial ring.
"""
one(R::PolyRing) = R(1)

doc"""
    gen(R::PolyRing)
> Return the generator of the given polynomial ring.
"""
gen(R::PolyRing) = R([zero(base_ring(R)), one(base_ring(R))])

doc"""
    iszero(a::PolyElem)
> Return `true` if the given polynomial is zero, otherwise return `false`.
"""
iszero(a::PolyElem) = length(a) == 0

doc"""
    isone(a::PolyElem)
> Return `true` if the given polynomial is the constant polynomial $1$,
> otherwise return `false`.
"""
isone(a::PolyElem) = length(a) == 1 && isone(coeff(a, 0))

doc"""
    isgen(a::PolyElem)
> Return `true` if the given polynomial is the constant generator of its
> polynomial ring, otherwise return `false`.
"""
function isgen(a::PolyElem)
    return length(a) == 2 && iszero(coeff(a, 0)) && isone(coeff(a, 1))
end

doc"""
    isunit(a::PolyElem)
> Return `true` if the given polynomial is a unit in its polynomial ring,
> otherwise return `false`.
"""
isunit(a::PolyElem) = length(a) == 1 && isunit(coeff(a, 0))

function deepcopy_internal{T <: RingElem}(a::GenPoly{T}, dict::ObjectIdDict)
   coeffs = Array{T}(length(a))
   for i = 1:length(a)
      coeffs[i] = deepcopy(a.coeffs[i])
   end
   return parent(a)(coeffs)
end

###############################################################################
#
#   Canonicalisation
#
###############################################################################

canonical_unit(x::PolyElem) = canonical_unit(lead(x))

###############################################################################
#
#   String I/O
#
###############################################################################

function show(io::IO, x::PolyElem)
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

function show(io::IO, p::PolyRing)
   print(io, "Univariate Polynomial Ring in ")
   print(io, string(var(p)))
   print(io, " over ")
   show(io, base_ring(p))
end

needs_parentheses(x::PolyElem) = length(x) > 1

is_negative(x::PolyElem) = length(x) <= 1 && is_negative(coeff(x, 0))

show_minus_one{T <: RingElem}(::Type{GenPoly{T}}) = show_minus_one(T)

###############################################################################
#
#   Unary operations
#
###############################################################################

doc"""
    -(a::PolyElem)
> Return $-a$.
"""
function -(a::PolyElem)
   len = length(a)
   z = parent(a)()
   fit!(z, len)
   for i = 1:len
      setcoeff!(z, i - 1, -coeff(a, i - 1))
   end
   set_length!(z, len)
   return z
end

###############################################################################
#
#   Binary operations
#
###############################################################################

doc"""
    +{T <: RingElem}(a::PolyElem{T}, b::PolyElem{T})
> Return $a + b$.
"""
function +{T <: RingElem}(a::PolyElem{T}, b::PolyElem{T})
   check_parent(a, b)
   lena = length(a)
   lenb = length(b)
   lenz = max(lena, lenb)
   z = parent(a)()
   fit!(z, lenz)
   i = 1
   while i <= min(lena, lenb)
      setcoeff!(z, i - 1, coeff(a, i - 1) + coeff(b, i - 1))
      i += 1
   end
   while i <= lena
      setcoeff!(z, i - 1, coeff(a, i - 1))
      i += 1
   end
   while i <= lenb
      setcoeff!(z, i - 1, coeff(b, i - 1))
      i += 1
   end
   set_length!(z, normalise(z, i - 1))
   return z
end

doc"""
    -{T <: RingElem}(a::PolyElem{T}, b::PolyElem{T})
> Return $a - b$.
"""
function -{T <: RingElem}(a::PolyElem{T}, b::PolyElem{T})
   check_parent(a, b)
   lena = length(a)
   lenb = length(b)
   lenz = max(lena, lenb)
   z = parent(a)()
   fit!(z, lenz)
   i = 1
   while i <= min(lena, lenb)
      setcoeff!(z, i - 1, coeff(a, i - 1) - coeff(b, i - 1))
      i += 1
   end
   while i <= lena
      setcoeff!(z, i - 1, coeff(a, i - 1))
      i += 1
   end
   while i <= lenb
      setcoeff!(z, i - 1, -coeff(b, i - 1))
      i += 1
   end
   set_length!(z, normalise(z, i - 1))
   return z
end

function mul_karatsuba{T <: RingElem}(a::PolyElem{T}, b::PolyElem{T})
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
   r = parent(a)()
   fit!(r, lena + lenb - 1)
   for i = 1:length(z0)
      setcoeff!(r, i - 1, coeff(z0, i - 1))
   end
   for i = length(z0) + 1:2m
      setcoeff!(r, i - 1, base_ring(a)())
   end
   for i = 1:length(z2)
      setcoeff!(r, 2m + i - 1, coeff(z2, i - 1))
   end
   for i = 1:length(z1)
      addeq!(r.coeffs[i + m], coeff(z1, i - 1))
   end
   return r
end

function mul_ks{T <: PolyElem}(a::PolyElem{T}, b::PolyElem{T})
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
   A1 = Array{elem_type(base_ring(base_ring(a)))}(m*lena)
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
      A2 = Array{elem_type(base_ring(base_ring(a)))}(m*lenb)
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

function mul_classical{T <: RingElem}(a::PolyElem{T}, b::PolyElem{T})
   lena = length(a)
   lenb = length(b)
   if lena == 0 || lenb == 0
      return parent(a)()
   end
   t = base_ring(a)()
   lenz = lena + lenb - 1
   d = Array{T}(lenz)
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

doc"""
    *{T <: RingElem}(a::PolyElem{T}, b::PolyElem{T})
> Return $a\times b$.
"""
function *{T <: RingElem}(a::PolyElem{T}, b::PolyElem{T})
   check_parent(a, b)
   return mul_classical(a, b)
end

###############################################################################
#
#   Ad hoc binary operators
#
###############################################################################

doc"""
    *{T <: RingElem}(a::T, b::PolyElem{T})
> Return $a\times b$.
"""
function *{T <: RingElem}(a::T, b::PolyElem{T})
   len = length(b)
   z = parent(b)()
   fit!(z, len)
   for i = 1:len
      setcoeff!(z, i - 1, a*coeff(b, i - 1))
   end
   set_length!(z, normalise(z, len))
   return z
end

doc"""
    *(a::Integer, b::PolyElem)
> Return $a\times b$.
"""
function *(a::Integer, b::PolyElem)
   len = length(b)
   z = parent(b)()
   fit!(z, len)
   for i = 1:len
      setcoeff!(z, i - 1, a*coeff(b, i - 1))
   end
   set_length!(z, normalise(z, len))
   return z
end

doc"""
    *(a::fmpz, b::PolyElem)
> Return $a\times b$.
"""
function *(a::fmpz, b::PolyElem)
   len = length(b)
   z = parent(b)()
   fit!(z, len)
   for i = 1:len
      setcoeff!(z, i - 1, a*coeff(b, i - 1))
   end
   set_length!(z, normalise(z, len))
   return z
end

doc"""
    *{T <: RingElem}(a::PolyElem{T}, b::T)
> Return $a\times b$.
"""
*{T <: RingElem}(a::PolyElem{T}, b::T) = b*a

doc"""
    *(a::PolyElem, b::Integer)
> Return $a\times b$.
"""
*(a::PolyElem, b::Integer) = b*a

doc"""
    *(a::PolyElem, b::fmpz)
> Return $a\times b$.
"""
*(a::PolyElem, b::fmpz) = b*a

doc"""
    +{T <: RingElem}(a::T, b::PolyElem{T})
> Return $a + b$.
"""
+{T <: RingElem}(a::T, b::PolyElem{T}) = parent(b)(a) + b

doc"""
    +(a::Integer, b::PolyElem)
> Return $a + b$.
"""
+(a::Integer, b::PolyElem) = parent(b)(a) + b

doc"""
    +(a::fmpz, b::PolyElem)
> Return $a + b$.
"""
+(a::fmpz, b::PolyElem) = parent(b)(a) + b

doc"""
    +{T <: RingElem}(a::PolyElem{T}, b::T)
> Return $a + b$.
"""
+{T <: RingElem}(a::PolyElem{T}, b::T) = b + a

doc"""
    +(a::PolyElem, b::Integer)
> Return $a + b$.
"""
+(a::PolyElem, b::Integer) = b + a

doc"""
    +(a::PolyElem, b::fmpz)
> Return $a + b$.
"""
+(a::PolyElem, b::fmpz) = b + a

doc"""
    -{T <: RingElem}(a::T, b::PolyElem{T})
> Return $a - b$.
"""
-{T <: RingElem}(a::T, b::PolyElem{T}) = parent(b)(a) - b

doc"""
    -(a::Integer, b::PolyElem)
> Return $a - b$.
"""
-(a::Integer, b::PolyElem) = parent(b)(a) - b

doc"""
    -(a::fmpz, b::PolyElem)
> Return $a - b$.
"""
-(a::fmpz, b::PolyElem) = parent(b)(a) - b

doc"""
    -{T <: RingElem}(a::PolyElem{T}, b::T)
> Return $a - b$.
"""
-{T <: RingElem}(a::PolyElem{T}, b::T) = a - parent(a)(b)

doc"""
    -(a::PolyElem, b::Integer)
> Return $a - b$.
"""
-(a::PolyElem, b::Integer) = a - parent(a)(b)

doc"""
    -(a::PolyElem, b::fmpz)
> Return $a - b$.
"""
-(a::PolyElem, b::fmpz) = a - parent(a)(b)

###############################################################################
#
#   Powering
#
###############################################################################

function pow_multinomial{T <: RingElem}(a::PolyElem{T}, e::Int)
   e < 0 && throw(DomainError())
   lena = length(a)
   lenz = (lena - 1) * e + 1
   res = Array{T}(lenz)
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

doc"""
    ^{T <: RingElem}(a::PolyElem{T}, b::Int)
> Return $a^b$. We require $b \geq 0$.
"""
function ^{T <: RingElem}(a::PolyElem{T}, b::Int)
   b < 0 && throw(DomainError())
   # special case powers of x for constructing polynomials efficiently
   if isgen(a)
      z = parent(a)()
      fit!(z, b + 1)
      setcoeff!(z, b, coeff(a, 1))
      for i = 1:b
         setcoeff!(z, i - 1, coeff(a, 0))
      end
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

doc"""
    =={T <: RingElem}(x::PolyElem{T}, y::PolyElem{T})
> Return `true` if $x == y$ arithmetically, otherwise return `false`. Recall
> that power series to different precisions may still be arithmetically
> equal to the minimum of the two precisions.
"""
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

doc"""
    isequal{T <: RingElem}(x::PolyElem{T}, y::PolyElem{T})
> Return `true` if $x == y$ exactly, otherwise return `false`. This function is
> useful in cases where the coefficients of the polynomial are inexact, e.g.
> power series. Only if the power series are precisely the same, to the same
> precision, are they declared equal by this function.
"""
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

doc"""
    =={T <: RingElem}(x::PolyElem{T}, y::T)
> Return `true` if $x == y$ arithmetically, otherwise return `false`.
"""
=={T <: RingElem}(x::PolyElem{T}, y::T) = ((length(x) == 0 && y == 0)
                        || (length(x) == 1 && coeff(x, 0) == y))

doc"""
    =={T <: RingElem}(x::T, y::PolyElem{T})
> Return `true` if $x == y$ arithmetically, otherwise return `false`.
"""
=={T <: RingElem}(x::T, y::PolyElem{T}) = y == x

doc"""
    ==(x::PolyElem, y::Integer)
> Return `true` if $x == y$ arithmetically, otherwise return `false`.
"""
==(x::PolyElem, y::Integer) = ((length(x) == 0 && y == 0)
                        || (length(x) == 1 && coeff(x, 0) == y))

doc"""
    ==(x::Integer, y::PolyElem)
> Return `true` if $x == y$ arithmetically, otherwise return `false`.
"""
==(x::Integer, y::PolyElem) = y == x

doc"""
    ==(x::PolyElem, y::fmpz)
> Return `true` if $x == y$ arithmetically, otherwise return `false`.
"""
==(x::PolyElem, y::fmpz) = ((length(x) == 0 && y == 0)
                        || (length(x) == 1 && coeff(x, 0) == y))

doc"""
    ==(x::fmpz, y::PolyElem)
> Return `true` if $x == y$ arithmetically, otherwise return `false`.
"""
==(x::fmpz, y::PolyElem) = y == x

###############################################################################
#
#   Truncation
#
###############################################################################

doc"""
    truncate(a::PolyElem, n::Int)
> Return $a$ truncated to $n$ terms.
"""
function truncate(a::PolyElem, n::Int)
   n < 0 && throw(DomainError())
   lena = length(a)
   if lena <= n
      return a
   end
   lenz = min(lena, n)
   z = parent(a)()
   fit!(a, lenz)
   for i = 1:lenz
      setcoeff!(z, i - 1, coeff(a, i - 1))
   end
   set_length!(z, normalise(z, lenz))
   return z
end

doc"""
    mullow{T <: RingElem}(a::PolyElem{T}, b::PolyElem{T}, n::Int)
> Return $a\times b$ truncated to $n$ terms.
"""
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
   t = base_ring(a)()
   lenz = min(lena + lenb - 1, n)
   d = Array{T}(lenz)
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

doc"""
    reverse(x::PolyElem, len::Int)
> Return the reverse of the polynomial $x$, thought of as a polynomial of
> the given length (the polynomial will be notionally truncated or padded with
> zeroes before the leading term if necessary to match the specified length). 
> The resulting polynomial is normalised. If `len` is negative we throw a
> `DomainError()`.
"""
function reverse(x::PolyElem, len::Int)
   len < 0 && throw(DomainError())
   r = parent(x)()
   fit!(r, len)
   for i = 1:len
      setcoeff!(r, i - 1, coeff(x, len - i))
   end
   set_length!(r, normalise(r, len))
   return r
end

doc"""
    reverse(x::PolyElem)
> Return the reverse of the polynomial $x$, i.e. the leading coefficient
> of $x$ becomes the constant coefficient of the result, etc. The resulting
> polynomial is normalised.
"""
function reverse(x::PolyElem)
   reverse(x, length(x))
end

###############################################################################
#
#   Shifting
#
###############################################################################

doc"""
    shift_left(x::PolyElem, n::Int)
> Return the polynomial $f$ shifted left by $n$ terms, i.e. multiplied by
> $x^n$.
"""
function shift_left(f::PolyElem, n::Int)
   n < 0 && throw(DomainError())
   if n == 0
      return f
   end
   flen = length(f)
   r = parent(f)()
   fit!(r, flen + n)
   for i = 1:n
      setcoeff!(r, i - 1, zero(base_ring(f)))
   end
   for i = 1:flen
      setcoeff!(r, i + n - 1, coeff(f, i - 1))
   end
   return r
end

doc"""
    shift_right(f::PolyElem, n::Int)
> Return the polynomial $f$ shifted right by $n$ terms, i.e. divided by
> $x^n$.
"""
function shift_right(f::PolyElem, n::Int)
   n < 0 && throw(DomainError())
   flen = length(f)
   if n >= flen
      return zero(parent(f))
   end
   if n == 0
      return f
   end
   r = parent(f)()
   fit!(r, flen - n)
   for i = 1:flen - n
      setcoeff!(r, i - 1, coeff(f, i + n - 1))
   end
   return r
end

###############################################################################
#
#   Modular arithmetic
#
###############################################################################

doc"""
    mulmod{T <: Union{ResElem, FieldElem}}(a::PolyElem{T}, b::PolyElem{T}, d::PolyElem{T})
> Return $a\times b \pmod{d}$.
"""
function mulmod{T <: Union{ResElem, FieldElem}}(a::PolyElem{T}, b::PolyElem{T}, d::PolyElem{T})
   check_parent(a, b)
   check_parent(a, d)
   return mod(a*b, d)
end

doc"""
    powmod{T <: Union{ResElem, FieldElem}}(a::PolyElem{T}, b::Int, d::PolyElem{T})
> Return $a^b \pmod{d}$. There are no restrictions on $b$.
"""
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

doc"""
    invmod{T <: Union{ResElem, FieldElem}}(a::PolyElem{T}, b::PolyElem{T})
> Return $a^{-1} \pmod{d}$.
"""
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

doc"""
    divexact{T <: RingElem}(a::PolyElem{T}, b::PolyElem{T})
> Return $a/b$ where the quotient is expected to be exact.
"""
function divexact{T <: RingElem}(f::PolyElem{T}, g::PolyElem{T})
   check_parent(f, g)
   g == 0 && throw(DivideError())
   if f == 0
      return zero(parent(f))
   end
   lenq = length(f) - length(g) + 1
   d = Array{T}(lenq)
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

doc"""
    divexact{T <: RingElem}(a::PolyElem{T}, b::T)
> Return $a/b$ where the quotient is expected to be exact.
"""
function divexact{T <: RingElem}(a::PolyElem{T}, b::T)
   b == 0 && throw(DivideError())
   z = parent(a)()
   fit!(z, length(a))
   for i = 1:length(a)
      setcoeff!(z, i - 1, divexact(coeff(a, i - 1), b))
   end
   set_length!(z, length(a))
   return z
end

doc"""
    divexact(a::PolyElem, b::Integer)
> Return $a/b$ where the quotient is expected to be exact.
"""
function divexact(a::PolyElem, b::Integer)
   b == 0 && throw(DivideError())
   z = parent(a)()
   fit!(z, length(a))
   for i = 1:length(a)
      setcoeff!(z, i - 1, divexact(coeff(a, i - 1), b))
   end
   set_length!(z, length(a))
   return z
end

doc"""
    divexact(a::PolyElem, b::fmpz)
> Return $a/b$ where the quotient is expected to be exact.
"""
function divexact(a::PolyElem, b::fmpz)
   b == 0 && throw(DivideError())
   z = parent(a)()
   fit!(z, length(a))
   for i = 1:length(a)
      setcoeff!(z, i - 1, divexact(coeff(a, i - 1), b))
   end
   set_length!(z, length(a))
   return z
end

###############################################################################
#
#   Euclidean division
#
###############################################################################

doc"""
    mod{T <: Union{ResElem, FieldElem}}(f::PolyElem{T}, g::PolyElem{T})
> Return $f \pmod{g}$.
"""
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

doc"""
    divrem{T <: Union{ResElem, FieldElem}}(f::PolyElem{T}, g::PolyElem{T})
> Return a tuple $(q, r)$ such that $f = qg + r$ where $q$ is the euclidean
> quotient of $f$ by $g$.
"""
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
   q = parent(f)()
   fit!(q, qlen)
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

doc"""
    pseudorem{T <: RingElem}(f::PolyElem{T}, g::PolyElem{T})
> Return the pseudoremainder of $a$ divided by $b$. If $b = 0$ we throw a 
> `DivideError()`.
"""
function pseudorem{T <: RingElem}(f::PolyElem{T}, g::PolyElem{T})
   check_parent(f, g)
   g == 0 && throw(DivideError())
   k = length(f) - length(g) + 1
   b = coeff(g, length(g) - 1)
   x = gen(parent(f))
   while length(f) >= length(g)
      f = f*b - shift_left(coeff(f, length(f) - 1)*g, length(f) - length(g))
      k -= 1
   end
   return f*b^k
end

doc"""
    pseudodivrem{T <: RingElem}(f::PolyElem{T}, g::PolyElem{T})
> Return a tuple $(q, r)$ consisting of the pseudoquotient and pseudoremainder 
> of $a$ divided by $b$. If $b = 0$ we throw a `DivideError()`.
"""
function pseudodivrem{T <: RingElem}(f::PolyElem{T}, g::PolyElem{T})
   check_parent(f, g)
   g == 0 && throw(DivideError())
   if length(f) < length(g)
      return zero(parent(f)), f
   end
   lenq = length(f) - length(g) + 1
   k = lenq
   q = parent(f)()
   fit!(q, lenq)
   b = coeff(g, length(g) - 1)
   x = gen(parent(f))
   while length(f) >= length(g)
      for i = length(f) - length(g) + 2:lenq
         setcoeff!(q, i - 1, coeff(q, i - 1) * b)
      end
      setcoeff!(q, length(f) - length(g), coeff(f, length(f) - 1))
      f = f*b - shift_left(coeff(f, length(f) - 1)*g, length(f) - length(g))
      k -= 1
   end
   while lenq > 0 && coeff(q, lenq - 1) == 0
      lenq -= 1
   end
   set_length!(q, lenq)
   s = b^k
   return q*s, f*s
end

################################################################################
#
#   Remove and valuation
#
################################################################################

#CF TODO: use squaring for fast large valuation

doc"""
    remove{T <: RingElem}(z::PolyElem{T}, p::PolyElem{T})
> Computes the valuation of $z$ at $p$, that is, the largest $k$ such that
> $p^k$ divides $z$. Additionally, $z/p^k$ is returned as well.
>
> See also `valuation`, which only returns the valuation.
"""
function remove{T <: RingElem}(z::PolyElem{T}, p::PolyElem{T})
  check_parent(z,p)
  z == 0 && error("Not yet implemented")
  q, r = divrem(z, p)
  if !iszero(r)
    return 0, z
  end
  v = 0
  qn = q
  while iszero(r)
    q = qn
    qn, r = divrem(q, p)
    v += 1
  end
  return v, q
end

doc"""
    valuation{T <: RingElem}(z::PolyElem{T}, p::PolyElem{T})
> Computes the valuation of $z$ at $p$, that is, the largest $k$ such that
> $p^k$ divides $z$.
>
> See also `remove`, which also returns $z/p^k$.
"""
function valuation{T <: RingElem}(z::PolyElem{T}, p::PolyElem{T})
  v, _ = remove(z, p)
  return v
end

###############################################################################
#
#   Content, primitive part, GCD and LCM
#
###############################################################################

doc"""
    gcd{T <: RingElem}(a::PolyElem{T}, b::PolyElem{T})
> Return a greatest common divisor of $a$ and $b$ if it exists.
"""
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
   c1 = content(a)
   c2 = content(b)
   a = divexact(a, c1)
   b = divexact(b, c2)
   c = gcd(c1, c2)
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

doc"""
    lcm{T <: RingElem}(a::PolyElem{T}, b::PolyElem{T})
> Return a least common multiple of $a$ and $b$ if it exists.
"""
function lcm{T <: RingElem}(a::PolyElem{T}, b::PolyElem{T})
   check_parent(a, b)
   return a*divexact(b, gcd(a, b))
end

doc"""
    content(a::PolyElem)
> Return the content of $a$, i.e. the greatest common divisor of its
> coefficients.
"""
function content(a::PolyElem)
   z = coeff(a, 0)
   for i = 2:length(a)
      z = gcd(z, coeff(a, i - 1))
   end
   return z
end

doc"""
    primpart(a::PolyElem)
> Return the primitive part of $a$, i.e. the polynomial divided by its content.
"""
function primpart(a::PolyElem)
   d = content(a)
   return divexact(a, d)
end

###############################################################################
#
#   Evaluation/composition
#
###############################################################################

doc"""
    evaluate{T <: RingElem}(a::PolyElem{T}, b::T)
> Evaluate the polynomial $a$ at the value $b$ and return the result.
"""
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

doc"""
    evaluate(a::PolyElem, b::Integer)
> Evaluate the polynomial $a$ at the value $b$ and return the result.
"""
function evaluate(a::PolyElem, b::Integer)
   return evaluate(a, base_ring(a)(b))
end

doc"""
    evaluate(a::PolyElem, b::fmpz)
> Evaluate the polynomial $a$ at the value $b$ and return the result.
"""
function evaluate(a::PolyElem, b::fmpz)
   return evaluate(a, base_ring(a)(b))
end

doc"""
    compose(a::PolyElem, b::PolyElem)
> Compose the polynomial $a$ with the polynomial $b$ and return the result,
> i.e. return $a\circ b$.
"""
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

doc"""
    derivative(a::PolyElem)
> Return the derivative of the polynomial $a$.
"""
function derivative(a::PolyElem)
   if a == 0
      return zero(parent(a))
   end
   len = length(a)
   z = parent(a)()
   fit!(z, len - 1)
   for i = 1:len - 1
      setcoeff!(z, i - 1, i*coeff(a, i))
   end
   set_length!(z, normalise(z, len - 1))
   return z
end

###############################################################################
#
#   Integral
#
###############################################################################

doc"""
    integral{T <: Union{ResElem, FieldElem}}(x::PolyElem{T})
> Return the integral of the polynomial $a$.
"""
function integral{T <: Union{ResElem, FieldElem}}(x::PolyElem{T})
   len = length(x)
   p = parent(x)()
   fit!(p, len + 1)
   setcoeff!(p, 0, zero(base_ring(x)))
   for i = 1:len
      setcoeff!(p, i, divexact(coeff(x, i - 1), base_ring(x)(i)))
   end
   len += 1
   while len > 0 && coeff(p, len - 1) == 0 # FIXME: cannot use normalise here
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

doc"""
    resultant{T <: RingElem}(a::PolyElem{T}, b::PolyElem{T})
> Return the resultant of the $a$ and $b$.
"""
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

doc"""
    discriminant(a::PolyElem)
> Return the discrimnant of the given polynomial.
"""
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

doc"""
    gcdx{T <: RingElem}(a::PolyElem{T}, b::PolyElem{T})
> Return a tuple $(r, s, t)$ such that $r$ is the resultant of $a$ and $b$ and
> such that $r = a\times s + b\times t$.
"""
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

doc"""
    gcdx{T <: Union{ResElem, FieldElem}}(a::PolyElem{T}, b::PolyElem{T})
> Return a tuple $(g, s, t)$ such that $g$ is the greatest common divisor of
> $a$ and $b$ and such that $r = a\times s + b\times t$.
"""
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

doc"""
    gcdinv{T <: Union{ResElem, FieldElem}}(a::PolyElem{T}, b::PolyElem{T})
> Return a tuple $(g, s)$ such that $g$ is the greatest common divisor of $a$
> and $b$ and such that $s = a^{-1} \pmod{b}$. This function is useful for
> inverting modulo a polynomial and checking that it really was invertible.
"""
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

doc"""
    monomial_to_newton!{T <: RingElem}(P::Array{T, 1}, roots::Array{T, 1})
> Converts a polynomial $p$, given as an array of coefficients, in-place
> from its coefficients given in the standard monomial basis to the Newton
> basis for the roots $r_0, r_1, \ldots, r_{n-2}$. In other words, this
> determines output coefficients $c_i$ such that
> $$c_0 + c_1(x-r_0) + c_2(x-r_0)(x-r_1) + \ldots + c_{n-1}(x-r_0)(x-r_1)\cdots(x-r_{n-2})$$
> is equal to the input polynomial.
"""
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

doc"""
    newton_to_monomial!{T <: RingElem}(P::Array{T, 1}, roots::Array{T, 1})
> Converts a polynomial $p$, given as an array of coefficients, in-place
> from its coefficients given in the Newton basis for the roots
> $r_0, r_1, \ldots, r_{n-2}$ to the standard monomial basis. In other words,
> this evaluates
> $$c_0 + c_1(x-r_0) + c_2(x-r_0)(x-r_1) + \ldots + c_{n-1}(x-r_0)(x-r_1)\cdots(x-r_{n-2})$$
> where $c_i$ are the input coefficients given by $p$.
"""
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

doc"""
    interpolate{T <: RingElem}(S::PolyRing, x::Array{T, 1}, y::Array{T, 1})
> Given two arrays of values $xs$ and $ys$ of the same length $n$, find
> the polynomial $f$ in the polynomial ring $R$ of length at most $n$ such that
> $f$ has the value $ys$ at the points $xs$. The values in the arrays $xs$ and
> $ys$ must belong to the base ring of the polynomial ring $R$.
"""
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

function chebyshev_t_pair(n::Int, x::PolyElem)
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

doc"""
    chebyshev_t(n::Int, x::PolyElem)
> Return the Chebyshev polynomial of the first kind $T_n(x)$, defined by 
> $T_n(x) = \cos(n \cos^{-1}(x))$.
"""
function chebyshev_t(n::Int, x::PolyElem)
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

function chebyshev_u_pair(n::Int, x::PolyElem)
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

doc"""
    chebyshev_u(n::Int, x::PolyElem)
> Return the Chebyshev polynomial of the first kind $U_n(x)$, defined by 
> $(n+1) U_n(x) = T'_{n+1}(x)$.
"""
function chebyshev_u(n::Int, x::PolyElem)
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

function fit!{T <: RingElem}(c::GenPoly{T}, n::Int)
   if length(c.coeffs) < n
      t = c.coeffs
      c.coeffs = Array{T}(n)
      for i = 1:length(c)
         c.coeffs[i] = t[i]
      end
      for i = length(c) + 1:n
         c.coeffs[i] = zero(base_ring(c))
      end
   end
   nothing
end

function zero!{T <: RingElem}(c::GenPoly{T})
   c.length = 0
   nothing
end

function setcoeff!{T <: RingElem}(c::GenPoly{T}, n::Int, a::T)
   if a != 0 || n + 1 <= length(c)
      fit!(c, n + 1)
      c.coeffs[n + 1] = a
      c.length = max(length(c), n + 1)
      # don't normalise
   end
   nothing
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
   nothing
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
   nothing
end

function add!{T <: RingElem}(c::PolyElem{T}, a::PolyElem{T}, b::PolyElem{T})
   lena = length(a)
   lenb = length(b)
   len = max(lena, lenb)
   fit!(c, len)
   i = 1
   while i <= 1:min(lena, lenb)
      add!(c.coeffs[i], coeff(a, i - 1), coeff(b, i - 1))
   end
   while i <= lena
      setcoeff!(c, i - 1, coeff(a, i - 1))
      i += 1
   end
   while i <= lenb
      setcoeff!(c, i - 1, coeff(b, i - 1))
      i += 1
   end
   c.length = normalise(c, len)
   nothing
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

doc"""
    subst{T <: RingElem}(f::PolyElem{T}, a::Any)
> Evaluate the polynomial $f$ at $a$. Note that $a$ can be anything, whether
> a ring element or not.
"""
function subst{T <: RingElem}(f::PolyElem{T}, a::Any)
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

function (a::GenPolyRing{T}){T <: RingElem}(b::RingElem)
   return a(base_ring(a)(b))
end

function (a::GenPolyRing{T}){T <: RingElem}()
   z = GenPoly{T}()
   z.parent = a
   return z
end

function (a::GenPolyRing{T}){T <: RingElem}(b::Integer)
   z = GenPoly{T}(base_ring(a)(b))
   z.parent = a
   return z
end

function (a::GenPolyRing{T}){T <: RingElem}(b::T)
   parent(b) != base_ring(a) && error("Unable to coerce to polynomial")
   z = GenPoly{T}(b)
   z.parent = a
   return z
end

function (a::GenPolyRing{T}){T <: RingElem}(b::PolyElem{T})
   parent(b) != a && error("Unable to coerce polynomial")
   return b
end

function (a::GenPolyRing{T}){T <: RingElem}(b::Array{T, 1})
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

doc"""
    PolynomialRing(R::Ring, s::AbstractString; cached::Bool = true)
> Given a base ring `R` and string `s` specifying how the generator (variable)
> should be printed, return a tuple `S, x` representing the new polynomial
> ring $S = R[x]$ and the generator $x$ of the ring. By default the parent
> object `S` will depend only on `R` and `x` and will be cached. Setting the
> optional argument `cached` to `false` will prevent the parent object `S` from
> being cached.
"""
function PolynomialRing(R::Ring, s::AbstractString; cached::Bool = true)
   S = Symbol(s)
   T = elem_type(R)
   parent_obj = GenPolyRing{T}(R, S, cached)

   return parent_obj, parent_obj([R(0), R(1)])
end

# S, x = R["x"] syntax
getindex(R::Ring, s::String) = PolynomialRing(R, s)

getindex{T}(R::Tuple{Ring,T}, s::String) = PolynomialRing(R[1], s)
