###############################################################################
#
#   CapRelSeries.jl : Power series over rings, capped relative precision
#
###############################################################################    

export GenRelSeries, GenRelSeriesRing, PowerSeriesRing, O, valuation, exp,
       precision, max_precision, set_prec!

###############################################################################
#
#   Data type and parent object methods
#
###############################################################################

doc"""
    O{T}(a::SeriesElem{T})
> Returns $0 + O(x^\mbox{deg}(a))$. Usually this function is called with $x^n$
> as parameter. Then the function returns the power series $0 + O(x^n)$, which
> can be used to set the precision of a power series when constructing it.
"""
function O{T}(a::SeriesElem{T})
   prec = length(a) - 1
   prec < 0 && throw(DomainError())
   return parent(a)(Array(T, 0), 0, prec)
end

parent_type{T}(::Type{GenRelSeries{T}}) = GenRelSeriesRing{T}

doc"""
    parent(a::SeriesElem)
> Return the parent of the given power series.
"""
parent(a::SeriesElem) = a.parent

elem_type{T <: RingElem}(::GenRelSeriesRing{T}) = GenRelSeries{T}

doc"""
    base_ring(R::SeriesRing)
> Return the base ring of the given power series ring.
"""
base_ring{T}(R::SeriesRing{T}) = R.base_ring::parent_type(T)

doc"""
    base_ring(a::SeriesElem)
> Return the base ring of the power series ring of the given power series.
"""
base_ring(a::SeriesElem) = base_ring(parent(a))

doc"""
    var(a::SeriesRing)
> Return the internal name of the generator of the power series ring. Note that
> this is returned as a `Symbol` not a `String`.
"""
var(a::SeriesRing) = a.S

function check_parent(a::SeriesElem, b::SeriesElem)
   parent(a) != parent(b) && 
             error("Incompatible power series rings in power series operation")
end

###############################################################################
#
#   Basic manipulation
#
###############################################################################    
   
function Base.hash(a::SeriesElem, h::UInt)
   b = 0xb44d6896204881f3%UInt
   for i in 0:length(a) - 1
      b $= hash(coeff(a, i), h) $ h
      b = (b << 1) | (b >> (sizeof(Int)*8 - 1))
   end
   return b
end

length(x::SeriesElem) = x.length

precision(x::SeriesElem) = x.prec

doc"""
    max_precision(R::SeriesRing)
> Return the maximum relative precision of power series in the given power
> series ring.
"""
max_precision(R::SeriesRing) = R.prec_max

function normalise(a::GenRelSeries, len::Int)
   while len > 0 && iszero(a.coeffs[len])
      len -= 1
   end
   return len
end

function set_length!(a::SeriesElem, len::Int)
   a.length = len
end

function set_prec!(a::SeriesElem, prec::Int)
   a.prec = prec
end

function coeff(a::GenRelSeries, n::Int)
   n < 0  && throw(DomainError())
   return n >= length(a) ? zero(base_ring(a)) : a.coeffs[n + 1]
end

doc"""
    zero(R::SeriesRing)
> Return $0 + O(x^n)$ where $n$ is the maximum precision of the power series
> ring $R$.
"""
zero(R::SeriesRing) = R(0)

doc"""
    zero(R::SeriesRing)
> Return $1 + O(x^n)$ where $n$ is the maximum precision of the power series
> ring $R$.
"""
one(R::SeriesRing) = R(1)

doc"""
    zero(R::SeriesRing)
> Return the generator of the power series ring, i.e. $x + O(x^{n + 1})$ where
> $n$ is the maximum precision of the power series ring $R$.
"""
function gen{T}(R::SeriesRing{T})
   S = base_ring(R)
   return R([S(0), S(1)], 2, max_precision(R) + 1)
end

doc"""
    iszero(a::SeriesElem)
> Return `true` if the given power series is arithmetically equal to zero to
> its current precision, otherwise return `false`.
"""
iszero(a::SeriesElem) = length(a) == 0

doc"""
    isone(a::SeriesElem)
> Return `true` if the given power series is arithmetically equal to one to
> its current precision, otherwise return `false`.
"""
function isone(a::SeriesElem)
   return length(a) == 1 && isone(coeff(a, 0))
end

doc"""
    isgen(a::SeriesElem)
> Return `true` if the given power series is arithmetically equal to the
> generator of its power series ring to its current precision, otherwise return
> `false`.
"""
function isgen(a::SeriesElem)
   return length(a) == 2 && iszero(coeff(a, 0)) && isone(coeff(a, 1))
end

doc"""
    isunit(a::SeriesElem)
> Return `true` if the given power series is arithmetically equal to a unit,
> i.e. is invertible, otherwise return `false`.
"""
isunit(a::SeriesElem) = isunit(coeff(a, 0))

doc"""
    valuation(a::SeriesElem)
> Return the valuation of the given power series, i.e. the degree of the first
> nonzero term (or the precision if it is arithmetically zero).
"""
function valuation(a::SeriesElem)
   if length(a) == 0
      return precision(a)
   end
   for i = 1:length(a)
      if coeff(a, i - 1) != 0
         return i - 1
      end
   end
   error("Power series is not normalised")
end

doc"""
    modulus{T <: ResElem}(a::SeriesElem{T})
> Return the modulus of the coefficients of the given polynomial.
"""
modulus{T <: ResElem}(a::SeriesElem{T}) = modulus(base_ring(a))

function deepcopy{T <: RingElem}(a::SeriesElem{T})
   coeffs = Array(T, length(a))
   for i = 1:length(a)
      coeffs[i] = deepcopy(coeff(a, i - 1))
   end
   return parent(a)(coeffs, length(a), precision(a))
end

###############################################################################
#
#   AbstractString{} I/O
#
###############################################################################

function show{T <: RingElem}(io::IO, x::SeriesElem{T})
   len = length(x)

   if len == 0
      print(io, zero(base_ring(x)))
   else
      coeff_printed = false
      c = coeff(x, 0)
      bracket = needs_parentheses(c)
      if !iszero(c)
         if bracket
            print(io, "(")
         end
         show(io, c)
         if bracket
            print(io, ")")
         end
         coeff_printed = true
      end
      for i = 1:len - 1
         c = coeff(x, i)
         bracket = needs_parentheses(c)
         if !iszero(c)
            if coeff_printed && !is_negative(c)
               print(io, "+")
            end
            if !isone(c) && (c != -1 || show_minus_one(elem_type(base_ring(x))))
               if bracket
                  print(io, "(")
               end
               show(io, c)
               if bracket
                  print(io, ")")
               end
               print(io, "*")
            end
            if c == -1 && !show_minus_one(elem_type(base_ring(x)))
               print(io, "-")
            end
            print(io, string(var(parent(x))))
            if i != 1
               print(io, "^")
               print(io, i)
            end
            coeff_printed = true
         end
      end
   end
   print(io, "+O(", string(var(parent(x))), "^", precision(x), ")")
end

function show{T <: RingElem}(io::IO, a::SeriesRing{T})
   print(io, "Univariate power series ring in ", var(a), " over ")
   show(io, base_ring(a))
end

needs_parentheses(x::SeriesElem) = length(x) > 1

is_negative(x::SeriesElem) = length(x) <= 1 && is_negative(coeff(x, 0))

show_minus_one{T <: RingElem}(::Type{SeriesElem{T}}) = show_minus_one(T)

###############################################################################
#
#   Unary operators
#
###############################################################################

doc"""
    -(a::SeriesElem)
> Return $-a$.
"""
function -{T <: RingElem}(a::SeriesElem{T})
   len = length(a)
   z = parent(a)()
   set_prec!(z, precision(a))
   fit!(z, len)
   for i = 1:len
      setcoeff!(z, i - 1, -coeff(a, i - 1))
   end
   return z
end

###############################################################################
#
#   Binary operators
#
###############################################################################

doc"""
    +{T <: RingElem}(a::SeriesElem{T}, b::SeriesElem{T})
> Return $a + b$.
"""
function +{T <: RingElem}(a::SeriesElem{T}, b::SeriesElem{T})
   check_parent(a, b)
   lena = length(a)
   lenb = length(b)
   prec = min(precision(a), precision(b))
   lena = min(lena, prec)
   lenb = min(lenb, prec)
   lenz = max(lena, lenb)
   z = parent(a)()
   fit!(z, lenz)
   set_prec!(z, prec)
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
    -{T <: RingElem}(a::SeriesElem{T}, b::SeriesElem{T})
> Return $a - b$.
"""
function -{T <: RingElem}(a::SeriesElem{T}, b::SeriesElem{T})
   check_parent(a, b)
   lena = length(a)
   lenb = length(b)
   prec = min(precision(a), precision(b))
   lena = min(lena, prec)
   lenb = min(lenb, prec)
   lenz = max(lena, lenb)
   z = parent(a)()
   fit!(z, lenz)
   set_prec!(z, prec)
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

doc"""
    *{T <: RingElem}(a::SeriesElem{T}, b::SeriesElem{T})
> Return $a\times b$.
"""
function *{T <: RingElem}(a::SeriesElem{T}, b::SeriesElem{T})
   check_parent(a, b)
   lena = length(a)
   lenb = length(b)
   aval = valuation(a)
   bval = valuation(b)
   prec = min(precision(a) + bval, precision(b) + aval)
   lena = min(lena, prec)
   lenb = min(lenb, prec)
   if lena == 0 || lenb == 0
      return parent(a)(Array(T, 0), 0, prec)
   end
   t = base_ring(a)()
   lenz = min(lena + lenb - 1, prec)
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
            mul!(t, coeff(a, i - 1), coeff(b, j - 1))
            addeq!(d[i + j - 1], t)
         end
      end
   end        
   z = parent(a)(d, lenz, prec)
   set_length!(z, normalise(z, lenz))
   return z
end

###############################################################################
#
#   Ad hoc binary operators
#
###############################################################################

doc"""
    *{T <: RingElem}(a::T, b::SeriesElem{T})
> Return $a\times b$.
"""
function *{T <: RingElem}(a::T, b::SeriesElem{T})
   len = length(b)
   z = parent(b)()
   fit!(z, len)
   set_prec!(z, precision(b))
   for i = 1:len
      setcoeff!(z, i - 1, a*coeff(b, i - 1))
   end
   set_length!(z, normalise(z, len))
   return z
end

doc"""
    *{T <: RingElem}(a::Integer, b::SeriesElem{T})
> Return $a\times b$.
"""
function *{T <: RingElem}(a::Integer, b::SeriesElem{T})
   len = length(b)
   z = parent(b)()
   fit!(z, len)
   set_prec!(z, precision(b))
   for i = 1:len
      setcoeff!(z, i - 1, a*coeff(b, i - 1))
   end
   set_length!(z, normalise(z, len))
   return z
end

doc"""
    *{T <: RingElem}(a::fmpz, b::SeriesElem{T})
> Return $a\times b$.
"""
function *{T <: RingElem}(a::fmpz, b::SeriesElem{T})
   len = length(b)
   z = parent(b)()
   fit!(z, len)
   set_prec!(z, precision(b))
   for i = 1:len
      setcoeff!(z, i - 1, a*coeff(b, i - 1))
   end
   set_length!(z, normalise(z, len))
   return z
end

doc"""
    *{T <: RingElem}(a::SeriesElem{T}, b::T)
> Return $a\times b$.
"""
*{T <: RingElem}(a::SeriesElem, b::T) = b*a

doc"""
    *{T <: RingElem}(a::SeriesElem{T}, b::Integer)
> Return $a\times b$.
"""
*(a::SeriesElem, b::Integer) = b*a

doc"""
    *{T <: RingElem}(a::SeriesElem{T}, b::fmpz)
> Return $a\times b$.
"""
*(a::SeriesElem, b::fmpz) = b*a

doc"""
    +{T <: RingElem}(a::T, b::SeriesElem{T})
> Return $a + b$.
"""
+{T <: RingElem}(a::T, b::SeriesElem{T}) = parent(b)(a) + b

doc"""
    +(a::Integer, b::SeriesElem)
> Return $a + b$.
"""
+(a::Integer, b::SeriesElem) = parent(b)(a) + b

doc"""
    +(a::fmpz, b::SeriesElem)
> Return $a + b$.
"""
+(a::fmpz, b::SeriesElem) = parent(b)(a) + b

doc"""
    +{T <: RingElem}(a::SeriesElem{T}, b::T)
> Return $a + b$.
"""
+{T <: RingElem}(a::SeriesElem{T}, b::T) = b + a

doc"""
    +(a::SeriesElem, b::Integer)
> Return $a + b$.
"""
+(a::SeriesElem, b::Integer) = b + a

doc"""
    +(a::SeriesElem, b::fmpz)
> Return $a + b$.
"""
+(a::SeriesElem, b::fmpz) = b + a

doc"""
    -{T <: RingElem}(a::T, b::SeriesElem{T})
> Return $a - b$.
"""
-{T <: RingElem}(a::T, b::SeriesElem{T}) = parent(b)(a) - b

doc"""
    -(a::Integer, b::SeriesElem)
> Return $a - b$.
"""
-(a::Integer, b::SeriesElem) = parent(b)(a) - b

doc"""
    -(a::fmpz, b::SeriesElem)
> Return $a - b$.
"""
-(a::fmpz, b::SeriesElem) = parent(b)(a) - b

doc"""
    -{T <: RingElem}(a::SeriesElem{T}, b::T)
> Return $a - b$.
"""
-{T <: RingElem}(a::SeriesElem{T}, b::T) = a - parent(a)(b)

doc"""
    -(a::SeriesElem, b::Integer)
> Return $a - b$.
"""
-(a::SeriesElem, b::Integer) = a - parent(a)(b)

doc"""
    -(a::SeriesElem, b::fmpz)
> Return $a - b$.
"""
-(a::SeriesElem, b::fmpz) = a - parent(a)(b)

###############################################################################
#
#   Shifting
#
###############################################################################

doc"""
    shift_left(x::SeriesElem, n::Int)
> Return the power series $f$ shifted left by $n$ terms, i.e. multiplied by
> $x^n$.
"""
function shift_left{T <: RingElem}(x::SeriesElem{T}, len::Int)
   len < 0 && throw(DomainError())
   xlen = length(x)
   if xlen == 0
      z = zero(parent(x))
      set_prec!(z, precision(x) + len)
      return z
   end
   z = parent(x)()
   fit!(z, xlen + len)
   set_prec!(z, precision(x) + len)
   for i = 1:len
      setcoeff!(z, i - 1, zero(base_ring(x)))
   end
   for i = 1:xlen
      setcoeff!(z, i + len - 1, coeff(x, i - 1))
   end
   return z
end

doc"""
    shift_right(f::SeriesElem, n::Int)
> Return the power series $f$ shifted right by $n$ terms, i.e. divided by
> $x^n$.
"""
function shift_right{T <: RingElem}(x::SeriesElem{T}, len::Int)
   len < 0 && throw(DomainError())
   xlen = length(x)
   if len >= xlen
      z = zero(parent(x))
      set_prec!(z, max(0, precision(x) - len))
      return z
   end
   z = parent(x)()
   fit!(z, xlen - len)
   set_prec!(z, precision(x) - len)
   for i = 1:xlen - len
      setcoeff!(z, i - 1, coeff(x, i + len - 1))
   end
   return z
end

###############################################################################
#
#   Truncation
#
###############################################################################

doc"""
    truncate(a::SeriesElem, n::Int)
> Return $a$ truncated to $n$ terms.
"""
function truncate{T <: RingElem}(a::SeriesElem{T}, prec::Int)
   prec < 0 && throw(DomainError())
   len = length(a)
   if precision(a) <= prec
      return a
   end
   z = parent(a)()
   fit!(z, prec)
   set_prec!(z, prec)
   for i = 1:min(prec, len)
      setcoeff!(z, i - 1, coeff(a, i - 1))
   end
   for i = len + 1:prec
      setcoeff!(z, i - 1, zero(base_ring(a)))
   end
   set_length!(z, normalise(z, prec))
   return z
end

###############################################################################
#
#   Powering
#
###############################################################################

doc"""
    ^{T <: RingElem}(a::SeriesElem{T}, b::Int)
> Return $a^b$. We require $b \geq 0$.
"""
function ^{T <: RingElem}(a::SeriesElem{T}, b::Int)
   b < 0 && throw(DomainError())
   # special case powers of x for constructing power series efficiently
   if isgen(a)
      z = parent(a)()
      fit!(z, b + 1)
      set_prec!(z, precision(a) + b - 1)
      setcoeff!(z, b, coeff(a, 1))
      for i = 1:b
         setcoeff!(z, i - 1, coeff(a, 0))
      end
      return z
   elseif length(a) == 0
      z = parent(a)()
      set_prec!(z, precision(a) + (b - 1)*valuation(a))
      return z
   elseif length(a) == 1
      z = parent(a)(coeff(a, 0)^b)
      set_prec!(z, precision(a))
      return z
   elseif b == 0
      z = one(parent(a))
      set_prec!(z, precision(a))
      return z
   else
      bit = ~((~UInt(0)) >> 1)
      while (UInt(bit) & b) == 0
         bit >>= 1
      end
      z = a
      bit >>= 1
      while bit !=0
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
#   Comparison
#
###############################################################################

doc"""
    =={T <: RingElem}(x::SeriesElem{T}, y::SeriesElem{T})
> Return `true` if $x == y$ arithmetically, otherwise return `false`. Recall
> that power series to different precisions may still be arithmetically
> equal to the minimum of the two precisions.
"""
function =={T <: RingElem}(x::SeriesElem{T}, y::SeriesElem{T})
   check_parent(x, y)
   prec = min(precision(x), precision(y))
   m1 = min(length(x), length(y))
   m2 = max(length(x), length(y))
   m1 = min(m1, prec)
   m2 = min(m2, prec)
   if length(x) >= m2
      for i = m1 + 1: m2
         if coeff(x, i - 1) != 0
            return false
          end
      end
   else
      for i = m1 + 1: m2
         if coeff(y, i - 1) != 0
            return false
          end
      end
   end
   for i = 1:m1
      if coeff(x, i - 1) != coeff(y, i - 1)
         return false
      end
   end
   return true
end

doc"""
    isequal{T <: RingElem}(x::SeriesElem{T}, y::SeriesElem{T})
> Return `true` if $x == y$ exactly, otherwise return `false`. Only if the
> power series are precisely the same, to the same precision, are they declared
> equal by this function.
"""
function isequal{T <: RingElem}(x::SeriesElem{T}, y::SeriesElem{T})
   if parent(x) != parent(y)
      return false
   end
   if precision(x) != precision(y) || length(x) != length(y)
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
#   Ad hoc comparison
#
###############################################################################

doc"""
    =={T <: RingElem}(x::SeriesElem{T}, y::T)
> Return `true` if $x == y$ arithmetically, otherwise return `false`.
"""
=={T <: RingElem}(x::SeriesElem{T}, y::T) = precision(x) == 0 ||
           ((length(x) == 0 && y == 0) || (length(x) == 1 && coeff(x, 0) == y))

doc"""
    =={T <: RingElem}(x::T, y::SeriesElem{T})
> Return `true` if $x == y$ arithmetically, otherwise return `false`.
"""
=={T <: RingElem}(x::T, y::SeriesElem{T}) = y == x

doc"""
    ==(x::SeriesElem, y::Integer)
> Return `true` if $x == y$ arithmetically, otherwise return `false`.
"""
==(x::SeriesElem, y::Integer) = precision(x) == 0 || ((length(x) == 0 && y == 0)
                                       || (length(x) == 1 && coeff(x, 0) == y))

doc"""
    ==(x::SeriesElem, y::fmpz)
> Return `true` if $x == y$ arithmetically, otherwise return `false`.
"""
==(x::SeriesElem, y::fmpz) = precision(x) == 0 || ((length(x) == 0 && y == 0)
                                       || (length(x) == 1 && coeff(x, 0) == y))

doc"""
    ==(x::Integer, y::SeriesElem)
> Return `true` if $x == y$ arithmetically, otherwise return `false`.
"""
==(x::Integer, y::SeriesElem) = y == x

doc"""
    ==(x::fmpz, y::SeriesElem)
> Return `true` if $x == y$ arithmetically, otherwise return `false`.
"""
==(x::fmpz, y::SeriesElem) = y == x

###############################################################################
#
#   Exact division
#
###############################################################################

doc"""
    divexact{T <: RingElem}(a::SeriesElem{T}, b::SeriesElem{T})
> Return $a/b$. Requires $b$ to be invertible.
"""
function divexact{T <: RingElem}(x::SeriesElem{T}, y::SeriesElem{T})
   check_parent(x, y)
   y == 0 && throw(DivideError())
   v2 = valuation(y)
   if v2 != 0
      v1 = valuation(x)
      if v1 >= v2
         x = shift_right(x, v2)
         y = shift_right(y, v2)
      end
   end
   y = truncate(y, precision(x))
   return x*inv(y)
end

###############################################################################
#
#   Ad hoc exact division
#
###############################################################################

doc"""
    divexact{T <: RingElem}(a::SeriesElem{T}, b::Integer)
> Return $a/b$ where the quotient is expected to be exact.
"""
function divexact{T <: RingElem}(x::SeriesElem{T}, y::Integer)
   y == 0 && throw(DivideError())
   lenx = length(x)
   z = parent(x)()
   fit!(z, lenx)
   set_prec!(z, precision(x))
   for i = 1:lenx
      setcoeff!(z, i - 1, divexact(coeff(x, i - 1), y))
   end
   return z
end

doc"""
    divexact{T <: RingElem}(a::SeriesElem{T}, b::fmpz)
> Return $a/b$ where the quotient is expected to be exact.
"""
function divexact{T <: RingElem}(x::SeriesElem{T}, y::fmpz)
   y == 0 && throw(DivideError())
   lenx = length(x)
   z = parent(x)()
   fit!(z, lenx)
   set_prec!(z, precision(x))
   for i = 1:lenx
      setcoeff!(z, i - 1, divexact(coeff(x, i - 1), y))
   end
   return z
end

doc"""
    divexact{T <: RingElem}(a::SeriesElem{T}, b::T)
> Return $a/b$ where the quotient is expected to be exact.
"""
function divexact{T <: RingElem}(x::SeriesElem{T}, y::T)
   y == 0 && throw(DivideError())
   lenx = length(x)
   z = parent(x)()
   fit!(z, lenx)
   set_prec!(z, precision(x))
   for i = 1:lenx
      setcoeff!(z, i - 1, divexact(coeff(x, i - 1), y))
   end
   return z
end

###############################################################################
#
#   Inversion
#
###############################################################################

doc"""
   inv(a::SeriesElem)
> Return the inverse of the power series $a$, i.e. $1/a$.
"""
function inv(a::SeriesElem)
   a == 0 && throw(DivideError())
   !isunit(a) && error("Unable to invert power series")
   a1 = coeff(a, 0)
   ainv = parent(a)()
   fit!(ainv, precision(a))
   set_prec!(ainv, precision(a))
   if precision(a) != 0
      setcoeff!(ainv, 0, divexact(one(base_ring(a)), a1))
   end
   a1 = -a1
   for n = 2:precision(a)
      s = coeff(a, 1)*coeff(ainv, n - 2)
      for i = 2:min(n, length(a)) - 1
         s += coeff(a, i)*coeff(ainv, n - i - 1)
      end
      setcoeff!(ainv, n - 1, divexact(s, a1))
   end
   set_length!(ainv, normalise(ainv, precision(a)))
   return ainv
end

###############################################################################
#
#   Special functions
#
###############################################################################

doc"""
    exp(a::SeriesElem)
> Return the exponential of the power series $a$.
"""
function exp(a::SeriesElem)
   if a == 0
      z = one(parent(a))
      set_prec!(z, precision(a))
      return z
   end
   z = parent(a)()
   fit!(z, precision(a))
   set_prec!(z, precision(a))
   setcoeff!(z, 0, exp(coeff(a, 0)))
   len = length(a)
   for k = 1 : precision(a) - 1
      s = zero(base_ring(a))
      for j = 1 : min(k + 1, len) - 1
         s += j * coeff(a, j) * coeff(z, k - j)
      end
      !isunit(base_ring(a)(k)) && error("Unable to divide in exp")
      setcoeff!(z, k, divexact(s, k))
   end
   set_length!(z, normalise(z, precision(a)))
   return z
end

###############################################################################
#
#   Unsafe functions
#
###############################################################################

function fit!{T <: RingElem}(c::GenRelSeries{T}, n::Int)
   if length(c.coeffs) < n
      t = c.coeffs
      c.coeffs = Array(T, n)
      for i = 1:c.length
         c.coeffs[i] = t[i]
      end
      for i = length(c) + 1:n
         c.coeffs[i] = zero(base_ring(c))
      end
   end
end

function setcoeff!{T <: RingElem}(c::GenRelSeries{T}, n::Int, a::T)
   if (a != 0 && precision(c) > n) || n + 1 <= c.length
      fit!(c, n + 1)
      c.coeffs[n + 1] = a
      c.length = max(length(c), n + 1)
      # don't normalise
   end
end

function mul!{T <: RingElem}(c::GenRelSeries{T}, a::GenRelSeries{T}, b::GenRelSeries{T})
   lena = length(a)
   lenb = length(b)

   aval = valuation(a)
   bval = valuation(b)

   prec = min(precision(a) + bval, precision(b) + aval)
   
   lena = min(lena, prec)
   lenb = min(lenb, prec)
   
   if lena == 0 || lenb == 0
      c.length = 0
   else
      t = base_ring(a)()

      lenc = min(lena + lenb - 1, prec)
      fit!(c, lenc)

      for i = 1:min(lena, lenc)
         mul!(c.coeffs[i], coeff(a, i - 1), coeff(b, 0))
      end

      if lenc > lena
         for i = 2:min(lenb, lenc - lena + 1)
            mul!(c.coeffs[lena + i - 1], coeff(a, lena - 1), coeff(b, i - 1))
         end
      end

      for i = 1:lena - 1
         if lenc > i
            for j = 2:min(lenb, lenc - i + 1)
               mul!(t, coeff(a, i - 1), coeff(b, j - 1))
               addeq!(c.coeffs[i + j - 1], t)
            end
         end
      end
        
      c.length = normalise(c, lenc)
   end
   c.prec = prec
end

function addeq!{T <: RingElem}(c::GenRelSeries{T}, a::GenRelSeries{T})
   lenc = length(c)
   lena = length(a)
   
   prec = min(precision(a), precision(c))
   
   lena = min(lena, prec)
   lenc = min(lenc, prec)

   len = max(lenc, lena)
   fit!(c, len)
   for i = 1:lena
      addeq!(c.coeffs[i], coeff(a, i - 1))
   end
   c.length = normalise(c, len)
   c.prec = prec
end

###############################################################################
#
#   Promotion rules
#
###############################################################################

function Base.promote_rule{T <: RingElem, V <: Integer}(::Type{GenRelSeries{T}}, ::Type{V})
   return GenRelSeries{T}
end

function Base.promote_rule{T <: RingElem}(::Type{GenRelSeries{T}}, ::Type{T})
   return GenRelSeries{T}
end

function promote_rule1{T <: RingElem, U <: RingElem}(::Type{GenRelSeries{T}}, ::Type{GenRelSeries{U}})
   Base.promote_rule(T, GenRelSeries{U}) == T ? GenRelSeries{T} : Union{}
end

function Base.promote_rule{T <: RingElem, U <: RingElem}(::Type{GenRelSeries{T}}, ::Type{U})
   Base.promote_rule(T, U) == T ? GenRelSeries{T} : promote_rule1(U, GenRelSeries{T})
end

###############################################################################
#
#   Parent object call overload
#
###############################################################################

function Base.call{T <: RingElem}(a::GenRelSeriesRing{T}, b::RingElem)
   return a(base_ring(a)(b))
end

function Base.call{T <: RingElem}(a::GenRelSeriesRing{T})
   z = GenRelSeries{T}(Array(T, 0), 0, a.prec_max)
   z.parent = a
   return z
end

function Base.call{T <: RingElem}(a::GenRelSeriesRing{T}, b::Integer)
   if b == 0
      z = GenRelSeries{T}(Array(T, 0), 0, a.prec_max)
   else
      z = GenRelSeries{T}([base_ring(a)(b)], 1, a.prec_max)
   end
   z.parent = a
   return z
end

function Base.call{T <: RingElem}(a::GenRelSeriesRing{T}, b::fmpz)
   if b == 0
      z = GenRelSeries{T}(Array(T, 0), 0, a.prec_max)
   else
      z = GenRelSeries{T}([base_ring(a)(b)], 1, a.prec_max)
   end
   z.parent = a
   return z
end

function Base.call{T <: RingElem}(a::GenRelSeriesRing{T}, b::T)
   parent(b) != base_ring(a) && error("Unable to coerce to power series")
   if b == 0
      z = GenRelSeries{T}(Array(T, 0), 0, a.prec_max)
   else
      z = GenRelSeries{T}([b], 1, a.prec_max)
   end
   z.parent = a
   return z
end

function Base.call{T <: RingElem}(a::GenRelSeriesRing{T}, b::SeriesElem{T})
   parent(b) != a && error("Unable to coerce power series")
   return b
end

function Base.call{T <: RingElem}(a::GenRelSeriesRing{T}, b::Array{T, 1}, len::Int, prec::Int)
   if length(b) > 0
      parent(b[1]) != base_ring(a) && error("Unable to coerce to power series")
   end
   z = GenRelSeries{T}(b, len, prec)
   z.parent = a
   return z
end

###############################################################################
#
#   PowerSeriesRing constructor
#
###############################################################################

doc"""
   PowerSeriesRing(R::Ring, prec::Int, s::AbstractString{}; cached=true)
> Return a tuple $(S, x)$ consisting of the parent object `S` of a power series
> ring over the given base ring and a generator `x` for the power series ring.
> The maximum relative precision of power series in the ring is set to `prec`.
> The supplied string `s` specifies the way the generator of the power series
> ring will be printed. By default, the parent object `S` will be cached so
> that supplying the same base ring, string and precision in future will return
> the same parent object and generator. If caching of the parent object is not
> required, `cached` can be set to `false`.
"""
function PowerSeriesRing(R::Ring, prec::Int, s::AbstractString{}; cached=true)
   S = Symbol(s)
   T = elem_type(R)
   parent_obj = GenRelSeriesRing{T}(R, prec, S, cached)

   return parent_obj, parent_obj([R(0), R(1)], 2, prec + 1)
end
