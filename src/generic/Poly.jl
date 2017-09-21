###############################################################################
#
#   Poly.jl : Generic polynomials over rings
#
###############################################################################

export PolynomialRing, hash, coeff, isgen, lead,
       var, truncate, mullow, reverse, shift_left, shift_right, divexact,
       pseudorem, pseudodivrem, gcd, degree, content, primpart, evaluate, 
       compose, derivative, integral, resultant, discriminant, gcdx, zero, one,
       gen, length, iszero, normalise, isone, isunit, addeq!, mul!, fit!,
       setcoeff!, mulmod, powmod, invmod, lcm, divrem, mod, gcdinv, resx,
       canonical_unit, var, chebyshev_t, chebyshev_u, set_length!,
       mul_classical, mul_ks, subst, mul_karatsuba, trail,
       pow_multinomial, monomial_to_newton!, newton_to_monomial!, ismonomial,
       base_ring, parent_type, elem_type, check_parent, promote_rule,
       needs_parentheses, isnegative, show_minus_one, remove, zero!, add!,
       interpolate

###############################################################################
#
#   Data type and parent object methods
#
###############################################################################

parent_type(::Type{Poly{T}}) where T <: RingElement = PolyRing{T}

elem_type(::Type{PolyRing{T}}) where T <: RingElement = Poly{T}

doc"""
    base_ring(R::Nemo.PolyRing)
> Return the base ring of the given polynomial ring.
"""
base_ring(R::Nemo.PolyRing{T}) where T <: RingElement = R.base_ring::parent_type(T)

doc"""
    base_ring(a::Nemo.PolyElem)
> Return the base ring of the polynomial ring of the given polynomial.
"""
base_ring(a::Nemo.PolyElem) = base_ring(parent(a))

doc"""
    parent(a::Nemo.PolyElem)
> Return the parent of the given polynomial.
"""
parent(a::Nemo.PolyElem) = a.parent

isexact(R::Nemo.PolyRing) = isexact(base_ring(R))

doc"""
    var(a::Nemo.PolyRing)
> Return the internal name of the generator of the polynomial ring. Note that
> this is returned as a `Symbol` not a `String`.
"""
var(a::Nemo.PolyRing) = a.S

doc"""
    vars(a::Nemo.PolyRing)
> Return an array of the variable names for the polynomial ring. Note that
> this is returned as an array of `Symbol` not `String`.
"""
vars(a::Nemo.PolyRing) = [a.S]

function check_parent(a::Nemo.PolyElem, b::Nemo.PolyElem)
   parent(a) != parent(b) && 
                error("Incompatible polynomial rings in polynomial operation")
end

###############################################################################
#
#   Basic manipulation
#
###############################################################################    

function Base.hash(a::Nemo.PolyElem, h::UInt)
   b = 0x53dd43cd511044d1%UInt
   for i in 0:length(a) - 1
      b = xor(b, xor(hash(coeff(a, i), h), h))
      b = (b << 1) | (b >> (sizeof(Int)*8 - 1))
   end
   return b
end

function setcoeff!(c::Poly{T}, n::Int, a::T) where {T <: RingElement}
   if !iszero(a) || n + 1 <= length(c)
      fit!(c, n + 1)
      c.coeffs[n + 1] = a
      c.length = max(length(c), n + 1)
      # don't normalise
   end
   return c
end

function normalise(a::Poly, n::Int)
   while n > 0 && iszero(a.coeffs[n]) 
      n -= 1
   end
   return n
end

length(a::Nemo.PolyElem) = a.length

doc"""
    degree(a::Nemo.PolyElem)
> Return the degree of the given polynomial. This is defined to be one less
> than the length, even for constant polynomials.
"""
degree(a::Nemo.PolyElem) = length(a) - 1

doc"""
    modulus{T <: ResElem}(a::Nemo.PolyElem{T})
> Return the modulus of the coefficients of the given polynomial.
"""
modulus(a::Nemo.PolyElem{T}) where {T <: ResElem} = modulus(base_ring(a))

coeff(a::Poly, n::Int) = n >= length(a) ? base_ring(a)(0) : a.coeffs[n + 1]

doc"""
    lead(x::Nemo.PolyElem)
> Return the leading coefficient of the given polynomial. This will be the
> nonzero coefficient of the term with highest degree unless the polynomial
> in the zero polynomial, in which case a zero coefficient is returned.
"""
lead(a::Nemo.PolyElem) = length(a) == 0 ? base_ring(a)(0) : coeff(a, length(a) - 1)

doc"""
    trail(x::Nemo.PolyElem)
> Return the trailing coefficient of the given polynomial. This will be the
> nonzero coefficient of the term with lowest degree unless the polynomial
> in the zero polynomial, in which case a zero coefficient is returned.
"""
function trail(a::Nemo.PolyElem)
   if iszero(a)
      return base_ring(a)(0)
   else
      for i = 1:length(a)
         c = coeff(a, i - 1)
         if !iszero(c)
            return c
         end
      end
      return coeff(a, length(a) - 1)
   end
end

doc"""
    zero(R::Nemo.PolyRing)
> Return the zero polynomial in the given polynomial ring.
"""
zero(R::Nemo.PolyRing) = R(0)

doc"""
    one(R::Nemo.PolyRing)
> Return the constant polynomial $1$ in the given polynomial ring.
"""
one(R::Nemo.PolyRing) = R(1)

doc"""
    gen(R::Nemo.PolyRing)
> Return the generator of the given polynomial ring.
"""
gen(R::Nemo.PolyRing) = R([zero(base_ring(R)), one(base_ring(R))])

doc"""
    iszero(a::Nemo.PolyElem)
> Return `true` if the given polynomial is zero, otherwise return `false`.
"""
iszero(a::Nemo.PolyElem) = length(a) == 0

doc"""
    isone(a::Nemo.PolyElem)
> Return `true` if the given polynomial is the constant polynomial $1$,
> otherwise return `false`.
"""
isone(a::Nemo.PolyElem) = length(a) == 1 && isone(coeff(a, 0))

doc"""
    isgen(a::Nemo.PolyElem)
> Return `true` if the given polynomial is the constant generator of its
> polynomial ring, otherwise return `false`.
"""
function isgen(a::Nemo.PolyElem)
    return length(a) == 2 && iszero(coeff(a, 0)) && isone(coeff(a, 1))
end

doc"""
    isunit(a::Nemo.PolyElem)
> Return `true` if the given polynomial is a unit in its polynomial ring,
> otherwise return `false`.
"""
isunit(a::Nemo.PolyElem) = length(a) == 1 && isunit(coeff(a, 0))

isterm(a::T) where {T <: RingElement} = true

doc"""
    isterm(a::Nemo.PolyElem)
> Return `true` if the given polynomial is has one term. This function is
> recursive, with all scalar types returning true.
"""
function isterm(a::Nemo.PolyElem)
   if !isterm(lead(a))
      return false
   end
   for i = 1:length(a) - 1
      if !iszero(coeff(a, i - 1))
         return false
      end
   end
   return true
end

ismonomial(a::T) where {T <: RingElement} = isone(a)

doc"""
    ismonomial(a::Nemo.PolyElem)
> Return `true` if the given polynomial is a monomial. 
"""
function ismonomial(a::Nemo.PolyElem)
   if !ismonomial(lead(a))
      return false
   end
   for i = 1:length(a) - 1
      if !iszero(coeff(a, i - 1))
         return false
      end
   end
   return true
end

function deepcopy_internal(a::Poly{T}, dict::ObjectIdDict) where {T <: RingElement} 
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

canonical_unit(x::Nemo.PolyElem) = canonical_unit(lead(x))

###############################################################################
#
#   String I/O
#
###############################################################################

function show(io::IO, x::Nemo.PolyElem)
   len = length(x)
   S = var(parent(x))
   if len == 0
      print(io, base_ring(x)(0))
   else
      for i = 1:len - 1
         c = coeff(x, len - i)
         bracket = needs_parentheses(c)
         if !iszero(c)
            if i != 1 && !isnegative(c)
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
         if len != 1 && !isnegative(c)
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

function show(io::IO, p::Nemo.PolyRing)
   print(io, "Univariate Polynomial Ring in ")
   print(io, string(var(p)))
   print(io, " over ")
   show(io, base_ring(p))
end

needs_parentheses(x::Nemo.PolyElem) = length(x) > 1

isnegative(x::Nemo.PolyElem) = length(x) <= 1 && isnegative(coeff(x, 0))

show_minus_one(::Type{Poly{T}}) where {T <: RingElement} = show_minus_one(T)

###############################################################################
#
#   Unary operations
#
###############################################################################

doc"""
    -(a::Nemo.PolyElem)
> Return $-a$.
"""
function -(a::Nemo.PolyElem)
   len = length(a)
   z = parent(a)()
   fit!(z, len)
   for i = 1:len
      z = setcoeff!(z, i - 1, -coeff(a, i - 1))
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
    +{T <: RingElement}(a::Nemo.PolyElem{T}, b::Nemo.PolyElem{T})
> Return $a + b$.
"""
function +(a::Nemo.PolyElem{T}, b::Nemo.PolyElem{T}) where {T <: RingElement}
   check_parent(a, b)
   lena = length(a)
   lenb = length(b)
   lenz = max(lena, lenb)
   z = parent(a)()
   fit!(z, lenz)
   i = 1
   while i <= min(lena, lenb)
      z = setcoeff!(z, i - 1, coeff(a, i - 1) + coeff(b, i - 1))
      i += 1
   end
   while i <= lena
      z = setcoeff!(z, i - 1, deepcopy(coeff(a, i - 1)))
      i += 1
   end
   while i <= lenb
      z = setcoeff!(z, i - 1, deepcopy(coeff(b, i - 1)))
      i += 1
   end
   set_length!(z, normalise(z, i - 1))
   return z
end

doc"""
    -{T <: RingElement}(a::Nemo.PolyElem{T}, b::Nemo.PolyElem{T})
> Return $a - b$.
"""
function -(a::Nemo.PolyElem{T}, b::Nemo.PolyElem{T}) where {T <: RingElement}
   check_parent(a, b)
   lena = length(a)
   lenb = length(b)
   lenz = max(lena, lenb)
   z = parent(a)()
   fit!(z, lenz)
   i = 1
   while i <= min(lena, lenb)
      z = setcoeff!(z, i - 1, coeff(a, i - 1) - coeff(b, i - 1))
      i += 1
   end
   while i <= lena
      z = setcoeff!(z, i - 1, deepcopy(coeff(a, i - 1)))
      i += 1
   end
   while i <= lenb
      z = setcoeff!(z, i - 1, -coeff(b, i - 1))
      i += 1
   end
   set_length!(z, normalise(z, i - 1))
   return z
end

function mul_karatsuba(a::Nemo.PolyElem{T}, b::Nemo.PolyElem{T}) where {T <: RingElement}
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
      r = setcoeff!(r, i - 1, coeff(z0, i - 1))
   end
   for i = length(z0) + 1:2m
      r = setcoeff!(r, i - 1, base_ring(a)())
   end
   for i = 1:length(z2)
      r = setcoeff!(r, 2m + i - 1, coeff(z2, i - 1))
   end
   for i = 1:length(z1)
      u = coeff(r, i + m - 1)
      u = addeq!(u, coeff(z1, i - 1))
      setcoeff!(r, i + m - 1, u)
   end
   return r
end

function mul_ks(a::Nemo.PolyElem{T}, b::Nemo.PolyElem{T}) where {T <: Nemo.PolyElem}
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
      u = coeff(r, i - 1)
      fit!(u, m)
      for j = 1:m
         u = setcoeff!(u, j - 1, coeff(p, (i - 1)*m + j - 1))
      end
      setcoeff!(r, i - 1, u)
   end
   set_length!(r, normalise(r, lenr))
   return r
end

function mul_classical(a::Nemo.PolyElem{T}, b::Nemo.PolyElem{T}) where {T <: RingElement}
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
         t = mul!(t, coeff(a, i - 1), b.coeffs[j])
         d[i + j - 1] = addeq!(d[i + j - 1], t)
      end
   end
   z = parent(a)(d)
   set_length!(z, normalise(z, lenz))
   return z
end

doc"""
    *{T <: RingElement}(a::Nemo.PolyElem{T}, b::Nemo.PolyElem{T})
> Return $a\times b$.
"""
function *(a::Nemo.PolyElem{T}, b::Nemo.PolyElem{T}) where {T <: RingElement}
   check_parent(a, b)
   return mul_classical(a, b)
end

###############################################################################
#
#   Ad hoc binary operators
#
###############################################################################

doc"""
    *{T <: RingElem}(a::T, b::Nemo.PolyElem{T})
> Return $a\times b$.
"""
function *(a::T, b::Nemo.PolyElem{T}) where {T <: RingElem}
   len = length(b)
   z = parent(b)()
   fit!(z, len)
   for i = 1:len
      z = setcoeff!(z, i - 1, a*coeff(b, i - 1))
   end
   set_length!(z, normalise(z, len))
   return z
end

doc"""
    *(a::Union{Integer, Rational}, b::Nemo.PolyElem)
> Return $a\times b$.
"""
function *(a::Union{Integer, Rational}, b::Nemo.PolyElem)
   len = length(b)
   z = parent(b)()
   fit!(z, len)
   for i = 1:len
      z = setcoeff!(z, i - 1, a*coeff(b, i - 1))
   end
   set_length!(z, normalise(z, len))
   return z
end

doc"""
    *{T <: RingElem}(a::Nemo.PolyElem{T}, b::T)
> Return $a\times b$.
"""
*(a::Nemo.PolyElem{T}, b::T) where {T <: RingElem} = b*a

doc"""
    *(a::Nemo.PolyElem, b::Union{Integer, Rational})
> Return $a\times b$.
"""
*(a::Nemo.PolyElem, b::Union{Integer, Rational}) = b*a

###############################################################################
#
#   Powering
#
###############################################################################

function pow_multinomial(a::Nemo.PolyElem{T}, e::Int) where {T <: RingElement}
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
         res[k + 1] = addeq!(res[k + 1], t * u)
      end
      d = addeq!(d, first)
      res[k + 1] = divexact(res[k + 1], d)
   end
   z = parent(a)(res)
   set_length!(z, normalise(z, lenz))
   return z
end

doc"""
    ^{T <: RingElement}(a::Nemo.PolyElem{T}, b::Int)
> Return $a^b$. We require $b \geq 0$.
"""
function ^(a::Nemo.PolyElem{T}, b::Int) where {T <: RingElement}
   b < 0 && throw(DomainError())
   # special case powers of x for constructing polynomials efficiently
   R = parent(a)
   if isgen(a)
      z = R()
      fit!(z, b + 1)
      z = setcoeff!(z, b, coeff(a, 1))
      for i = 1:b
         z = setcoeff!(z, i - 1, coeff(a, 0))
      end
      set_length!(z, b + 1)
      return z
   elseif length(a) == 0
      return zero(R)
   elseif length(a) == 1
      return R(coeff(a, 0)^b)
   elseif b == 0
      return one(R)
   else
      if T <: FieldElement && characteristic(base_ring(R)) == 0
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
    =={T <: RingElement}(x::Nemo.PolyElem{T}, y::Nemo.PolyElem{T})
> Return `true` if $x == y$ arithmetically, otherwise return `false`. Recall
> that power series to different precisions may still be arithmetically
> equal to the minimum of the two precisions.
"""
function ==(x::Nemo.PolyElem{T}, y::Nemo.PolyElem{T}) where {T <: RingElement}
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
    isequal{T <: RingElement}(x::Nemo.PolyElem{T}, y::Nemo.PolyElem{T})
> Return `true` if $x == y$ exactly, otherwise return `false`. This function is
> useful in cases where the coefficients of the polynomial are inexact, e.g.
> power series. Only if the power series are precisely the same, to the same
> precision, are they declared equal by this function.
"""
function isequal(x::Nemo.PolyElem{T}, y::Nemo.PolyElem{T}) where {T <: RingElement}
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
    =={T <: RingElem}(x::Nemo.PolyElem{T}, y::T)
> Return `true` if $x == y$.
"""
==(x::Nemo.PolyElem{T}, y::T) where T <: RingElem = ((length(x) == 0 && y == 0)
                        || (length(x) == 1 && coeff(x, 0) == y))

doc"""
    ==(x::Nemo.PolyElem, y::Union{Integer, Rational})
> Return `true` if $x == y$ arithmetically, otherwise return `false`.
"""
==(x::Nemo.PolyElem, y::Union{Integer, Rational}) = ((length(x) == 0 && y == 0)
                        || (length(x) == 1 && coeff(x, 0) == y))

doc"""
    =={T <: RingElem}(x::T, y::Nemo.PolyElem{T})
> Return `true` if $x = y$.
"""
==(x::T, y::Nemo.PolyElem{T}) where T <: RingElem = y == x

doc"""
    ==(x::Union{Integer, Rational}, y::Nemo.PolyElem)
> Return `true` if $x == y$ arithmetically, otherwise return `false`.
"""
==(x::Union{Integer, Rational}, y::Nemo.PolyElem) = y == x

###############################################################################
#
#   Truncation
#
###############################################################################

doc"""
    truncate(a::Nemo.PolyElem, n::Int)
> Return $a$ truncated to $n$ terms.
"""
function truncate(a::Nemo.PolyElem, n::Int)
   n < 0 && throw(DomainError())
   lena = length(a)
   if lena <= n
      return a
   end
   lenz = min(lena, n)
   z = parent(a)()
   fit!(z, lenz)
   for i = 1:lenz
      z = setcoeff!(z, i - 1, coeff(a, i - 1))
   end
   set_length!(z, normalise(z, lenz))
   return z
end

doc"""
    mullow{T <: RingElement}(a::Nemo.PolyElem{T}, b::Nemo.PolyElem{T}, n::Int)
> Return $a\times b$ truncated to $n$ terms.
"""
function mullow(a::Nemo.PolyElem{T}, b::Nemo.PolyElem{T}, n::Int) where {T <: RingElement}
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
            t = mul!(t, coeff(a, i - 1), b.coeffs[j])
            d[i + j - 1] = addeq!(d[i + j - 1], t)
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
    reverse(x::Nemo.PolyElem, len::Int)
> Return the reverse of the polynomial $x$, thought of as a polynomial of
> the given length (the polynomial will be notionally truncated or padded with
> zeroes before the leading term if necessary to match the specified length). 
> The resulting polynomial is normalised. If `len` is negative we throw a
> `DomainError()`.
"""
function reverse(x::Nemo.PolyElem, len::Int)
   len < 0 && throw(DomainError())
   r = parent(x)()
   fit!(r, len)
   for i = 1:len
      z = setcoeff!(r, i - 1, coeff(x, len - i))
   end
   set_length!(r, normalise(r, len))
   return r
end

doc"""
    reverse(x::Nemo.PolyElem)
> Return the reverse of the polynomial $x$, i.e. the leading coefficient
> of $x$ becomes the constant coefficient of the result, etc. The resulting
> polynomial is normalised.
"""
function reverse(x::Nemo.PolyElem)
   reverse(x, length(x))
end

###############################################################################
#
#   Shifting
#
###############################################################################

doc"""
    shift_left(x::Nemo.PolyElem, n::Int)
> Return the polynomial $f$ shifted left by $n$ terms, i.e. multiplied by
> $x^n$.
"""
function shift_left(f::Nemo.PolyElem, n::Int)
   n < 0 && throw(DomainError())
   if n == 0
      return f
   end
   flen = length(f)
   r = parent(f)()
   fit!(r, flen + n)
   for i = 1:n
      r = setcoeff!(r, i - 1, zero(base_ring(f)))
   end
   for i = 1:flen
      r = setcoeff!(r, i + n - 1, coeff(f, i - 1))
   end
   return r
end

doc"""
    shift_right(f::Nemo.PolyElem, n::Int)
> Return the polynomial $f$ shifted right by $n$ terms, i.e. divided by
> $x^n$.
"""
function shift_right(f::Nemo.PolyElem, n::Int)
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
      r = setcoeff!(r, i - 1, coeff(f, i + n - 1))
   end
   return r
end

###############################################################################
#
#   Modular arithmetic
#
###############################################################################

doc"""
    mulmod{T <: Union{Nemo.ResElem, FieldElement}}(a::Nemo.PolyElem{T}, b::Nemo.PolyElem{T}, d::Nemo.PolyElem{T})
> Return $a\times b \pmod{d}$.
"""
function mulmod(a::Nemo.PolyElem{T}, b::Nemo.PolyElem{T}, d::Nemo.PolyElem{T}) where {T <: Union{Nemo.ResElem, FieldElement}}
   check_parent(a, b)
   check_parent(a, d)
   return mod(a*b, d)
end

doc"""
    powmod{T <: Union{Nemo.ResElem, FieldElement}}(a::Nemo.PolyElem{T}, b::Int, d::Nemo.PolyElem{T})
> Return $a^b \pmod{d}$. There are no restrictions on $b$.
"""
function powmod(a::Nemo.PolyElem{T}, b::Int, d::Nemo.PolyElem{T}) where {T <: Union{Nemo.ResElem, FieldElement}}
   check_parent(a, d)
   if length(a) == 0
      z = zero(parent(a))
   elseif length(a) == 1
      z = parent(a)(coeff(a, 0)^b)
   elseif b == 0
      z = one(parent(a))
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
   end
   if length(z) >= length(d)
      z = mod(z, d)
   end
   return z
end

doc"""
    invmod{T <: Union{Nemo.ResElem, FieldElement}}(a::Nemo.PolyElem{T}, b::Nemo.PolyElem{T})
> Return $a^{-1} \pmod{d}$.
"""
function invmod(a::Nemo.PolyElem{T}, b::Nemo.PolyElem{T}) where {T <: Union{Nemo.ResElem, FieldElement}}
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
    divexact{T <: RingElement}(a::Nemo.PolyElem{T}, b::Nemo.PolyElem{T})
> Return $a/b$ where the quotient is expected to be exact.
"""
function divexact(f::Nemo.PolyElem{T}, g::Nemo.PolyElem{T}) where {T <: RingElement}
   check_parent(f, g)
   iszero(g) && throw(DivideError())
   if iszero(f)
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
    divexact{T <: RingElem}(a::Nemo.PolyElem{T}, b::T)
> Return $a/b$ where the quotient is expected to be exact.
"""
function divexact(a::Nemo.PolyElem{T}, b::T) where {T <: RingElem}
   iszero(b) && throw(DivideError())
   z = parent(a)()
   fit!(z, length(a))
   for i = 1:length(a)
      z = setcoeff!(z, i - 1, divexact(coeff(a, i - 1), b))
   end
   set_length!(z, length(a))
   return z
end

doc"""
    divexact(a::Nemo.PolyElem, b::Union{Integer, Rational})
> Return $a/b$ where the quotient is expected to be exact.
"""
function divexact(a::Nemo.PolyElem, b::Union{Integer, Rational})
   iszero(b) && throw(DivideError())
   z = parent(a)()
   fit!(z, length(a))
   for i = 1:length(a)
      z = setcoeff!(z, i - 1, divexact(coeff(a, i - 1), b))
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
    mod{T <: Union{Nemo.ResElem, FieldElement}}(f::Nemo.PolyElem{T}, g::Nemo.PolyElem{T})
> Return $f \pmod{g}$.
"""
function mod(f::Nemo.PolyElem{T}, g::Nemo.PolyElem{T}) where {T <: Union{Nemo.ResElem, FieldElement}}
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
            c = mul!(c, coeff(g, i - 1), l)
            u = coeff(f, i + length(f) - length(g) - 1)
            u = addeq!(u, c)
            f = setcoeff!(f, i + length(f) - length(g) - 1, u)
         end
         set_length!(f, normalise(f, length(f)))
      end
   end
   return f
end

doc"""
    divrem{T <: Union{Nemo.ResElem, FieldElement}}(f::Nemo.PolyElem{T}, g::Nemo.PolyElem{T})
> Return a tuple $(q, r)$ such that $f = qg + r$ where $q$ is the euclidean
> quotient of $f$ by $g$.
"""
function divrem(f::Nemo.PolyElem{T}, g::Nemo.PolyElem{T}) where {T <: Union{Nemo.ResElem, FieldElement}}
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
      q = setcoeff!(q, length(f) - length(g), q1*binv)
      for i = 1:length(g)
         c = mul!(c, coeff(g, i - 1), l)
         u = coeff(f, i + length(f) - length(g) - 1)
         u = addeq!(u, c)
         f = setcoeff!(f, i + length(f) - length(g) - 1, u)
      end
      set_length!(f, normalise(f, length(f)))
   end
   return q, f
end

doc"""
    div{T <: Union{Nemo.ResElem, FieldElement}}(f::Nemo.PolyElem{T}, g::Nemo.PolyElem{T})
> Return a tuple $q$ such that $f = qg + r$ where $q$ is the euclidean
> quotient of $f$ by $g$.
"""
function div(f::Nemo.PolyElem{T}, g::Nemo.PolyElem{T}) where {T <: Union{Nemo.ResElem, FieldElement}}
   q, r = divrem(f, g)
   return q
end

###############################################################################
#
#   Pseudodivision
#
###############################################################################

doc"""
    pseudorem{T <: RingElement}(f::Nemo.PolyElem{T}, g::Nemo.PolyElem{T})
> Return the pseudoremainder of $a$ divided by $b$. If $b = 0$ we throw a 
> `DivideError()`.
"""
function pseudorem(f::Nemo.PolyElem{T}, g::Nemo.PolyElem{T}) where {T <: RingElement}
   check_parent(f, g)
   g == 0 && throw(DivideError())
   if length(f) < length(g)
      return f
   end
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
    pseudodivrem{T <: RingElement}(f::Nemo.PolyElem{T}, g::Nemo.PolyElem{T})
> Return a tuple $(q, r)$ consisting of the pseudoquotient and pseudoremainder 
> of $a$ divided by $b$. If $b = 0$ we throw a `DivideError()`.
"""
function pseudodivrem(f::Nemo.PolyElem{T}, g::Nemo.PolyElem{T}) where {T <: RingElement}
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
         q = setcoeff!(q, i - 1, coeff(q, i - 1) * b)
      end
      q = setcoeff!(q, length(f) - length(g), coeff(f, length(f) - 1))
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
    remove{T <: RingElement}(z::Nemo.PolyElem{T}, p::Nemo.PolyElem{T})
> Computes the valuation of $z$ at $p$, that is, the largest $k$ such that
> $p^k$ divides $z$. Additionally, $z/p^k$ is returned as well.
>
> See also `valuation`, which only returns the valuation.
"""
function remove(z::Nemo.PolyElem{T}, p::Nemo.PolyElem{T}) where {T <: RingElement}
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
    valuation{T <: RingElement}(z::Nemo.PolyElem{T}, p::Nemo.PolyElem{T})
> Computes the valuation of $z$ at $p$, that is, the largest $k$ such that
> $p^k$ divides $z$.
>
> See also `remove`, which also returns $z/p^k$.
"""
function valuation(z::Nemo.PolyElem{T}, p::Nemo.PolyElem{T}) where {T <: RingElement}
  v, _ = remove(z, p)
  return v
end

doc"""
    divides{T <: RingElement}(f::Nemo.PolyElem{T}, g::Nemo.PolyElem{T})
> Returns a pair consisting of a flag which is set to `true` if $f$ divides
> $g$ and `false` otherwise, and a polynomial $h$ such that $f = gh$ if
> such a polynomial exists. If not, the value of $h$ is undetermined.
"""
function divides(f::Nemo.PolyElem{T}, g::Nemo.PolyElem{T}) where {T <: RingElement}
   check_parent(f, g)
   if length(g) == 0
      raise(DivideError())
   end
   if length(f) == 0
      return true, parent(f)()
   end
   if length(f) < length(g)
      return false, parent(f)()
   end
   f = deepcopy(f)
   g_lead = lead(g) 
   qlen = length(f) - length(g) + 1
   q = parent(f)()
   fit!(q, qlen)
   c = base_ring(f)()
   while length(f) >= length(g)
      q1 = lead(f)
      flag, d = divides(q1, g_lead)
      if !flag
         return false, parent(f)()
      end
      q = setcoeff!(q, length(f) - length(g), d)
      d = -d
      for i = 1:length(g)
         c = mul!(c, coeff(g, i - 1), d)
         u = coeff(f, i + length(f) - length(g) - 1)
         u = addeq!(u, c)
         f = setcoeff!(f, i + length(f) - length(g) - 1, u)
      end
      set_length!(f, normalise(f, length(f)))
   end
   return f == 0, q
end

doc"""
    divides{T <: RingElement}(f::Nemo.PolyElem{T}, g::T)
> Returns a pair consisting of a flag which is set to `true` if $g$ divides
> $f$ and `false` otherwise, and a polynomial $h$ such that $f = gh$ if
> such a polynomial exists. If not, the value of $h$ is undetermined.
"""
function divides(z::Nemo.PolyElem{T}, x::T) where {T <: RingElement}
   parent(x) != base_ring(z) && error("Wrong parents in divides")
   q = parent(z)()
   fit!(q, length(z))
   flag = true
   for i = 1:length(z)
      flag, c = divides(coeff(z, i - 1), x)
      if !flag
         break
      end
      q = setcoeff!(q, i - 1, c)
   end
   set_length!(q, flag ? length(z) : 0)
   return flag, q
end

###############################################################################
#
#   Content, primitive part, GCD and LCM
#
###############################################################################

function term_gcd(a::T, b::T) where {T <: RingElement}
   return gcd(a, b)
end

function term_content(a::T) where {T <: RingElement}
   return a
end

function term_gcd(a::Nemo.PolyElem{T}, b::Nemo.PolyElem{T}) where {T <: RingElement}
   d = min(degree(a), degree(b))
   x = gen(parent(a))
   return term_gcd(coeff(a, degree(a)), coeff(b, degree(b)))*x^d
end

function term_content(a::Nemo.PolyElem{T}) where {T <: RingElement}
   for i = 1:length(a)
      c = coeff(a, i - 1)
      if !iszero(c)
         g = term_content(c)
         for j = i + 1:length(a)
            c = coeff(a, j - 1)
            if !iszero(c)
               g = term_gcd(g, term_content(c))
            end
         end
         x = gen(parent(a))
         return g*x^(i - 1)
      end
   end
   return parent(a)()
end

doc"""
    gcd{T <: RingElement}(a::Nemo.PolyElem{T}, b::Nemo.PolyElem{T})
> Return a greatest common divisor of $a$ and $b$ if it exists.
"""
function gcd(a::Nemo.PolyElem{T}, b::Nemo.PolyElem{T}, ignore_content::Bool = false) where {T <: RingElement}
   check_parent(a, b)
   if length(b) > length(a)
      (a, b) = (b, a)
   end
   if iszero(b)
      if a == 0
         return a
      else
         return divexact(a, canonical_unit(lead(a)))
      end
   end
   if isone(b)
      return b
   end
   if !ignore_content
      c1 = content(a)
      c2 = content(b)
      a = divexact(a, c1)
      b = divexact(b, c2)
      c = gcd(c1, c2)
   end
   lead_monomial = isterm(lead(a)) || isterm(lead(b))
   trail_monomial = isterm(trail(a)) || isterm(trail(b))
   lead_a = lead(a)
   lead_b = lead(b)
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
   if !ignore_content
      if !isterm(lead(b)) && !isterm(trail(b))
         if lead_monomial # lead term monomial, so content contains rest
            d = divexact(lead(b), term_content(lead(b)))
            b = divexact(b, d)
         elseif trail_monomial # trail term is monomial, so ditto
            d = divexact(trail(b), term_content(trail(b)))
            b = divexact(b, d)
         else
            glead = gcd(lead_a, lead_b)
            if isterm(glead)
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
      b = divexact(b, content(b))
   end
   b = c*b
   return divexact(b, canonical_unit(lead(b)))
end

function gcd(a::Nemo.PolyElem{T}, b::Nemo.PolyElem{T}) where {T <: Union{Nemo.ResElem, FieldElement}}
   check_parent(a, b)
   if length(a) > length(b)
      (a, b) = (b, a)
   end
   if iszero(b)
      if iszero(a)
         return(a)
      else
         return inv(lead(a))*a
      end
   end
   g = gcd(content(a), content(b))
   a = divexact(a, g)
   b = divexact(b, g)
   while !iszero(a)
      (a, b) = (mod(b, a), a)
   end
   b = g*b
   return inv(lead(b))*b
end

doc"""
    lcm{T <: RingElement}(a::Nemo.PolyElem{T}, b::Nemo.PolyElem{T})
> Return a least common multiple of $a$ and $b$ if it exists.
"""
function lcm(a::Nemo.PolyElem{T}, b::Nemo.PolyElem{T}) where {T <: RingElement}
   check_parent(a, b)
   return a*divexact(b, gcd(a, b))
end

doc"""
    content(a::Nemo.PolyElem)
> Return the content of $a$, i.e. the greatest common divisor of its
> coefficients.
"""
function content(a::Nemo.PolyElem)
   z = coeff(a, 0)
   for i = 2:length(a)
      z = gcd(z, coeff(a, i - 1))
   end
   if z == 0
      return z
   else
      return divexact(z, canonical_unit(z))
   end
end

doc"""
    primpart(a::Nemo.PolyElem)
> Return the primitive part of $a$, i.e. the polynomial divided by its content.
"""
function primpart(a::Nemo.PolyElem)
   d = content(a)
   if d == 0
      return 0
   else
      return divexact(a, d)
   end
end

###############################################################################
#
#   Evaluation/composition
#
###############################################################################

doc"""
    evaluate{T <: RingElement}(a::Nemo.PolyElem{T}, b::T)
> Evaluate the polynomial $a$ at the value $b$ and return the result.
"""
function evaluate(a::Nemo.PolyElem, b::T) where {T <: RingElement}
   i = length(a)
   R = base_ring(a)
   if i == 0
       return zero(R)
   end
   if i > 25
      return subst(a, b)
   end
   z = R(coeff(a, i - 1))
   while i > 1
      i -= 1
      z = z*b + R(coeff(a, i - 1))
   end
   return z
end

doc"""
    compose(a::Nemo.PolyElem, b::Nemo.PolyElem)
> Compose the polynomial $a$ with the polynomial $b$ and return the result,
> i.e. return $a\circ b$.
"""
function compose(a::Nemo.PolyElem, b::Nemo.PolyElem)
   i = length(a)
   R = parent(a)
   if i == 0
       return zero(R)
   end
   if i*length(b) > 25
      return subst(a, b)
   end
   z = R(coeff(a, i - 1))
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
    derivative(a::Nemo.PolyElem)
> Return the derivative of the polynomial $a$.
"""
function derivative(a::Nemo.PolyElem)
   if a == 0
      return zero(parent(a))
   end
   len = length(a)
   z = parent(a)()
   fit!(z, len - 1)
   for i = 1:len - 1
      z = setcoeff!(z, i - 1, i*coeff(a, i))
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
    integral{T <: Union{Nemo.ResElem, FieldElement}}(x::Nemo.PolyElem{T})
> Return the integral of the polynomial $a$.
"""
function integral(x::Nemo.PolyElem{T}) where {T <: Union{Nemo.ResElem, FieldElement}}
   len = length(x)
   p = parent(x)()
   fit!(p, len + 1)
   p = setcoeff!(p, 0, zero(base_ring(x)))
   for i = 1:len
      p = setcoeff!(p, i, divexact(coeff(x, i - 1), base_ring(x)(i)))
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

# Dichotomous Lazard, computes Se0 from Sd0 and Sd1. See the paper,
# "Optimizations of the subresultant algorithm" by Lionel Ducos, J. Pure and
# Appl. Algebra 2000.
function subresultant_lazard(Sd0::Nemo.PolyElem{T}, Sd1::Nemo.PolyElem{T}) where T <: RingElement
   n = length(Sd0) - length(Sd1) - 1
   if n == 0
      return Sd1
   end
   x = lead(Sd1)
   y = lead(Sd0)
   a = 1 << (ndigits(n, 2) - 1) # floor(log_2(a))
   c = x
   n = n - 1
   while a != 1
      a = a >> 1
      c = divexact(c*c, y)
      if n >= a
         c = divexact(c*x, y)
         n -= a
      end
   end
   return divexact(c*Sd1, y)
end

# Ducos optimised calculation of Se1. See the paper, "Optimizations of the
# subresultant algorithm" by Lionel Ducos, J. Pure and Appl. Algebra 2000.
function subresultant_ducos(A::Nemo.PolyElem{T}, Sd1::Nemo.PolyElem{T}, Se0::Nemo.PolyElem{T}, sd::T) where T <: RingElement
   d1 = length(A)
   e1 = length(Sd1)
   cd1 = lead(Sd1)
   se = lead(Se0)
   D = parent(A)()
   fit!(D, d1 - 1)
   for j = 0:e1 - 2
      setcoeff!(D, j, se*coeff(A, j))
   end
   set_length!(D, normalise(D, e1 - 1))
   Hj = parent(A)()
   fit!(Hj, e1)
   setcoeff!(Hj, e1 - 1, se)
   Hj -= Se0
   D += coeff(A, e1 - 1)*Hj
   for j = e1:d1 - 2
      Hj = shift_left(Hj, 1)
      Hj -= divexact(coeff(Hj, e1 - 1)*Sd1, cd1)
      D += coeff(A, j)*Hj
   end
   D = divexact(D, lead(A))
   Hj = shift_left(Hj, 1)
   r = divexact((Hj + D)*cd1 - coeff(Hj, e1 - 1)*Sd1, sd)
   return iseven(d1 - e1) ? -r : r
end

doc"""
    resultant{T <: RingElement}(a::Nemo.PolyElem{T}, b::Nemo.PolyElem{T})
> Return the resultant of the $a$ and $b$.
"""
# See the paper, "Optimizations of the subresultant algorithm" by Lionel
# Ducos, J. Pure and Appl. Algebra 2000.
function resultant_ducos(p::Nemo.PolyElem{T}, q::Nemo.PolyElem{T}) where {T <: RingElement}
   check_parent(p, q)
   if length(p) == 0 || length(q) == 0
      return zero(base_ring(p))
   end
   sgn = 1
   if length(p) < length(q)
      p, q = q, p
      if iseven(length(p)) && iseven(length(q))
         sgn = -sgn
      end
   end
   lp = length(p)
   lq = length(q)
   if lq == 1
      return coeff(q, 0)^(lp - 1)
   end
   c1 = content(p)
   c2 = content(q)
   p = divexact(p, c1)
   q = divexact(q, c2)
   sd = lead(q)^(lp - lq)
   Sd0 = parent(p)()
   A = q
   B = pseudorem(p, -A)
   while true
      d1 = length(A)
      e1 = length(B)
      if e1 == 0
         return zero(base_ring(p))
      end
      Sd1 = B
      delta = d1 - e1
      if delta > 1
         if length(Sd0) == 0
            C = divexact(lead(B)^(delta - 1)*B, sd^(delta - 1))
         else
            C = subresultant_lazard(Sd0, Sd1)
         end
      else
         C = B
      end
      if e1 == 1
         return coeff(C, 0)*c1^(lq - 1)*c2^(lp - 1)*sgn
      end
      B = subresultant_ducos(A, Sd1, C, sd)
      Sd0 = C
      Sd1 = B
      A = C
      sd = lead(A)
   end
end

# details can be found in, "Optimizations of the subresultant algorithm" by
# Lionel Ducos, J. Pure and Appl. Algebra 2000. Note, the resultant is
# the constant coefficient of S_0 (aka S_00 in other sources)
function resultant_subresultant(p::Nemo.PolyElem{T}, q::Nemo.PolyElem{T}) where {T <: RingElement}
   check_parent(p, q)
   if length(p) == 0 || length(q) == 0
      return zero(base_ring(p))
   end
   sgn = 1
   if length(p) < length(q)
      p, q = q, p
      if iseven(length(p)) && iseven(length(q))
         sgn = -sgn
      end
   end
   lp = length(p)
   lq = length(q)
   if lq == 1
      return coeff(q, 0)^(lp - 1)
   end
   s = lead(q)^(lp - lq)
   S = parent(p)()
   A = q
   B = pseudorem(p, -q)
   while true
      d1 = length(A)
      e1 = length(B)
      if e1 == 0
         return zero(base_ring(p))
      end
      S = B
      delta = d1 - e1
      if delta > 1
         C = divexact(lead(B)^(delta - 1)*B, s^(delta - 1))
         S = C
      else
         C = B
      end
      if e1 == 1
         return coeff(S, 0)*sgn
      end
      B = divexact(pseudorem(A, -B), s^delta*lead(A))
      A = C
      s = lead(A)
   end
end

function resultant_lehmer(a::Nemo.PolyElem{T}, b::Nemo.PolyElem{T}) where {T <: Union{Nemo.ResElem, FieldElement}}
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
   lA = lenA = length(a)
   lB = lenB = length(b)
   if lenB == 1
      return coeff(b, 0)^(lA - 1)
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
   return c1^(lB - 1)*c2^(lA - 1)*s*sgn
end

function resultant_sylvester(p::Nemo.PolyElem{T}, q::Nemo.PolyElem{T}) where T <: RingElement
   check_parent(p, q)
   R = base_ring(p)
   if length(p) == 0 || length(q) == 0
      return zero(R)
   end
   m = degree(p)
   n = degree(q)
   M = Nemo.MatrixSpace(R, m + n, m + n)()
   for i = 1:n
      for j = m:-1:0
         M[i, m - j + i] = coeff(p, j)
      end
   end
   for i = 1:m
      for j = n:-1:0
         M[i + n, n - j + i] = coeff(q, j)
       end 
   end
   return det_df(M)
end

function resultant(p::Nemo.PolyElem{T}, q::Nemo.PolyElem{T}) where {T <: RingElement}
  R = parent(p)
  if !isexact(R)
     return resultant_sylvester(p, q)
  end
  try
     return resultant_ducos(p, q)
  catch 
     return resultant_sylvester(p, q) 
  end
end

function resultant_euclidean(a::Nemo.PolyElem{T}, b::Nemo.PolyElem{T}) where T <: Union{Nemo.ResElem, FieldElement}
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
   la = lena = length(a)
   lb = lenb = length(b)
   if lenb == 1
      return coeff(b, 0)^(la - 1)
   end
   c1 = content(a)
   c2 = content(b)
   A = divexact(a, c1)
   B = divexact(b, c2)
   s = base_ring(A)(1)
   la = lena = length(A)
   lb = lenb = length(B)
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
   return c1^(lb - 1)*c2^(la - 1)*s*sgn
end

function resultant(a::Nemo.PolyElem{T}, b::Nemo.PolyElem{T}) where {T <: Union{Nemo.ResElem, FieldElement}}
   try
      return resultant_euclidean(a, b)
   catch
      return resultant_sylvester(a, b)
   end
end

###############################################################################
#
#   Discriminant
#
###############################################################################

doc"""
    discriminant(a::Nemo.PolyElem)
> Return the discrimnant of the given polynomial.
"""
function discriminant(a::Nemo.PolyElem)
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
#   RESX
#
###############################################################################

doc"""
    resx{T <: RingElement}(a::Nemo.PolyElem{T}, b::Nemo.PolyElem{T})
> Return a tuple $(r, s, t)$ such that $r$ is the resultant of $a$ and $b$ and
> such that $r = a\times s + b\times t$.
"""
function resx(a::Nemo.PolyElem{T}, b::Nemo.PolyElem{T}) where {T <: RingElement}
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
   if lenb == 0
      return zero(base_ring(a)), zero(parent(a)), zero(parent(a))
   end
   (lena <= 1 && lenb <= 1) && error("Constant polynomials in resx")
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
   if length(b) > 1
      u2 *= c1^(length(b) - 2)*c2^(length(a) - 1)*sgn
   else
      u2 *= c2^(length(a) - 1)*sgn
      u2 = divexact(u2, c1)
   end
   if length(a) > 1
      v2 *= c1^(length(b) - 1)*c2^(length(a) - 2)*sgn
   else
      v2 *= c1^(length(b) - 1)*sgn
      v2 = divexact(v2, c2)
   end
   if lena != 2
      if lena > 1
         d1 = lead(B)^(lena - 2)
         d2 = h^(lena - 2)
         u2 = divexact(u2*d1, d2)
         v2 = divexact(v2*d1, d2)
      else
         u2 = divexact(u2*h, lead(B))
         v2 = divexact(v2*h, lead(B))
      end
   end
   if swap
      u2, v2 = v2, u2
   end
   u = canonical_unit(res)
   res = divexact(res, u)
   u2 = divexact(u2, u)
   v2 = divexact(v2, u)
   return res, u2, v2
end

###############################################################################
#
#   GCDX
#
###############################################################################

doc"""
    gcdx{T <: Union{Nemo.ResElem, FieldElement}}(a::Nemo.PolyElem{T}, b::Nemo.PolyElem{T})
> Return a tuple $(g, s, t)$ such that $g$ is the greatest common divisor of
> $a$ and $b$ and such that $r = a\times s + b\times t$.
"""
function gcdx(a::Nemo.PolyElem{T}, b::Nemo.PolyElem{T}) where {T <: Union{Nemo.ResElem, FieldElement}}
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
   u1, u2 = parent(a)(inv(c1)), zero(parent(a))
   v1, v2 = zero(parent(a)), parent(a)(inv(c2))
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
    gcdinv{T <: Union{Nemo.ResElem, FieldElement}}(a::Nemo.PolyElem{T}, b::Nemo.PolyElem{T})
> Return a tuple $(g, s)$ such that $g$ is the greatest common divisor of $a$
> and $b$ and such that $s = a^{-1} \pmod{b}$. This function is useful for
> inverting modulo a polynomial and checking that it really was invertible.
"""
function gcdinv(a::Nemo.PolyElem{T}, b::Nemo.PolyElem{T}) where {T <: Union{Nemo.ResElem, FieldElement}}
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
    monomial_to_newton!{T <: RingElement}(P::Array{T, 1}, roots::Array{T, 1})
> Converts a polynomial $p$, given as an array of coefficients, in-place
> from its coefficients given in the standard monomial basis to the Newton
> basis for the roots $r_0, r_1, \ldots, r_{n-2}$. In other words, this
> determines output coefficients $c_i$ such that
> $$c_0 + c_1(x-r_0) + c_2(x-r_0)(x-r_1) + \ldots + c_{n-1}(x-r_0)(x-r_1)\cdots(x-r_{n-2})$$
> is equal to the input polynomial.
"""
function monomial_to_newton!(P::Array{T, 1}, roots::Array{T, 1}) where {T <: RingElement}
   n = length(roots)
   if n > 0
      R = parent(roots[1])
      t = R()
      for i = 1:n - 1
         for j = n - 1:-1:i
            t = mul!(t, P[j + 1], roots[i])
            P[j] = addeq!(P[j], t)
         end
      end
   end
   return
end

doc"""
    newton_to_monomial!{T <: RingElement}(P::Array{T, 1}, roots::Array{T, 1})
> Converts a polynomial $p$, given as an array of coefficients, in-place
> from its coefficients given in the Newton basis for the roots
> $r_0, r_1, \ldots, r_{n-2}$ to the standard monomial basis. In other words,
> this evaluates
> $$c_0 + c_1(x-r_0) + c_2(x-r_0)(x-r_1) + \ldots + c_{n-1}(x-r_0)(x-r_1)\cdots(x-r_{n-2})$$
> where $c_i$ are the input coefficients given by $p$.
"""
function newton_to_monomial!(P::Array{T, 1}, roots::Array{T, 1}) where {T <: RingElement}
   n = length(roots)
   if n > 0
      R = parent(roots[1])
      t = R()
      for i = n - 1:-1:1
         d = -roots[i]
         for j = i:n - 1
            t = mul!(t, P[j + 1], d)
            P[j] = addeq!(P[j], t)
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
    interpolate{T <: RingElement}(S::Nemo.PolyRing, x::Array{T, 1}, y::Array{T, 1})
> Given two arrays of values $xs$ and $ys$ of the same length $n$, find
> the polynomial $f$ in the polynomial ring $R$ of length at most $n$ such that
> $f$ has the value $ys$ at the points $xs$. The values in the arrays $xs$ and
> $ys$ must belong to the base ring of the polynomial ring $R$. If no such
> polynomial exists, an exception is raised.
"""
function interpolate(S::Nemo.PolyRing, x::Array{T, 1}, y::Array{T, 1}) where {T <: RingElement}
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
         flag, P[j] = divides(p, q)
         flag == false && error("Not an exact division in interpolate")
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

function chebyshev_t_pair(n::Int, x::Nemo.PolyElem)
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
    chebyshev_t(n::Int, x::Nemo.PolyElem)
> Return the Chebyshev polynomial of the first kind $T_n(x)$, defined by 
> $T_n(x) = \cos(n \cos^{-1}(x))$.
"""
function chebyshev_t(n::Int, x::Nemo.PolyElem)
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

function chebyshev_u_pair(n::Int, x::Nemo.PolyElem)
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
    chebyshev_u(n::Int, x::Nemo.PolyElem)
> Return the Chebyshev polynomial of the first kind $U_n(x)$, defined by 
> $(n+1) U_n(x) = T'_{n+1}(x)$.
"""
function chebyshev_u(n::Int, x::Nemo.PolyElem)
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

function set_length!(c::Poly{T}, n::Int) where T <: RingElement
   if n < c.length
      for i = n + 1:c.length
         c.coeffs[i] = zero!(c.coeffs[i])
      end
   end
   c.length = n
end
   
function fit!(c::Poly{T}, n::Int) where {T <: RingElement}
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
   return nothing
end

function zero!(c::Poly{T}) where {T <: RingElement}
   set_length!(c, 0)
   return c
end

function mul!(c::Nemo.PolyElem{T}, a::Nemo.PolyElem{T}, b::Nemo.PolyElem{T}) where {T <: RingElement}
   lena = length(a)
   lenb = length(b)

   if lena == 0 || lenb == 0
      set_length!(c, 0)
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
         c.coeffs[i] = mul!(c.coeffs[i], coeff(a, i - 1), coeff(b, 0))
      end

      for i = 2:lenb
         c.coeffs[lena + i - 1] = mul!(c.coeffs[lena + i - 1], coeff(a, lena - 1), coeff(b, i - 1))
      end

      for i = 1:lena - 1
         for j = 2:lenb
            t = mul!(t, coeff(a, i - 1), coeff(b, j - 1))
            c.coeffs[i + j - 1] = addeq!(c.coeffs[i + j - 1], t)
         end
      end
        
      set_length!(c, normalise(c, lenc))
   end
   return c
end

function addeq!(c::Nemo.PolyElem{T}, a::Nemo.PolyElem{T}) where {T <: RingElement}
   lenc = length(c)
   lena = length(a)
   len = max(lenc, lena)
   fit!(c, len)
   for i = 1:lena
      c.coeffs[i] = addeq!(c.coeffs[i], coeff(a, i - 1))
   end
   set_length!(c, normalise(c, len))
   return c
end

function add!(c::Nemo.PolyElem{T}, a::Nemo.PolyElem{T}, b::Nemo.PolyElem{T}) where {T <: RingElement}
   lena = length(a)
   lenb = length(b)
   len = max(lena, lenb)
   fit!(c, len)
   i = 1
   while i <= min(lena, lenb)
      c.coeffs[i] = add!(c.coeffs[i], coeff(a, i - 1), coeff(b, i - 1))
      i += 1
   end
   while i <= lena
      c = setcoeff!(c, i - 1, coeff(a, i - 1))
      i += 1
   end
   while i <= lenb
      c = setcoeff!(c, i - 1, coeff(b, i - 1))
      i += 1
   end
   set_length!(c, normalise(c, len))
   return c
end

function addmul!(z::Nemo.PolyElem{T}, x::Nemo.PolyElem{T}, y::Nemo.PolyElem{T}, c::Nemo.PolyElem{T}) where {T <: RingElement}
   c = mul!(c, x, y)
   z = addeq!(z, c)
   return z
end

###############################################################################
#
#   Random elements
#
###############################################################################

function rand(S::Nemo.PolyRing, deg_range::UnitRange{Int}, v...)
   R = base_ring(S)
   f = S()
   x = gen(S)
   for i = 0:rand(deg_range)
      f += rand(R, v...)*x^i
   end
   return f
end

###############################################################################
#
#   Promotion rules
#
###############################################################################

promote_rule(::Type{Poly{T}}, ::Type{Poly{T}}) where T <: RingElement = Poly{T}
  
function promote_rule(::Type{Poly{T}}, ::Type{U}) where {T <: RingElement, U <: RingElement}
   promote_rule(T, U) == T ? Poly{T} : Union{}
end

###############################################################################
#
#   Polynomial substitution
#
###############################################################################

doc"""
    subst{T <: RingElement}(f::Nemo.PolyElem{T}, a::Any)
> Evaluate the polynomial $f$ at $a$. Note that $a$ can be anything, whether
> a ring element or not.
"""
function subst(f::Nemo.PolyElem{T}, a::Any) where {T <: RingElement}
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
      if !iszero(c)
         s += c*A[j + 1]
      end
   end
   for i = 1:d1
      s *= A[d + 1]
      s += coeff(f, (d1 - i)*d)*A[1]
      for j = 1:min(n - (d1 - i)*d, d - 1)
         c = coeff(f, (d1 - i)*d + j)
         if !iszero(c)
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

function (a::PolyRing{T})(b::RingElement) where {T <: RingElement}
   return a(base_ring(a)(b))
end

function (a::PolyRing{T})() where {T <: RingElement}
   z = Poly{T}()
   z.parent = a
   return z
end

function (a::PolyRing{T})(b::Union{Integer, Rational}) where {T <: RingElement}
   z = Poly{T}(base_ring(a)(b))
   z.parent = a
   return z
end

function (a::PolyRing{T})(b::T) where {T <: RingElement}
   parent(b) != base_ring(a) && error("Unable to coerce to polynomial")
   z = Poly{T}(b)
   z.parent = a
   return z
end

function (a::PolyRing{T})(b::Nemo.PolyElem{T}) where {T <: RingElement}
   parent(b) != a && error("Unable to coerce polynomial")
   return b
end

function (a::PolyRing{T})(b::Array{T, 1}) where T <: RingElement
   R = base_ring(a)
   for i = 1:length(b)
      b[i] = R(b[i])
   end
   z = Poly{T}(b)
   z.parent = a
   return z
end

function (a::PolyRing{T})(b::Array{S, 1}) where {S <: RingElement, T <: RingElement}
   R = base_ring(a)
   len = length(b)
   entries = Array{T}(len)
   for i = 1:length(b)
      entries[i] = R(b[i])
   end
   z = Poly{T}(entries)
   z.parent = a
   return z
end

###############################################################################
#
#   PolynomialRing constructor
#
###############################################################################

doc"""
    PolynomialRing(R::Nemo.Ring, s::AbstractString; cached::Bool = true)
> Given a base ring `R` and string `s` specifying how the generator (variable)
> should be printed, return a tuple `S, x` representing the new polynomial
> ring $S = R[x]$ and the generator $x$ of the ring. By default the parent
> object `S` will depend only on `R` and `x` and will be cached. Setting the
> optional argument `cached` to `false` will prevent the parent object `S` from
> being cached.
"""
function PolynomialRing(R::Nemo.Ring, s::AbstractString; cached::Bool = true)
   S = Symbol(s)
   T = elem_type(R)
   parent_obj = PolyRing{T}(R, S, cached)

   return parent_obj, parent_obj([R(0), R(1)])
end
