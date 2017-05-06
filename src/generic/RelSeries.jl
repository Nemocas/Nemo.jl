###############################################################################
#
#   RelSeries.jl : Power series over rings, capped relative precision
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
    O{T}(a::RelSeriesElem{T})
> Returns $0 + O(x^\mbox{deg}(a))$. Usually this function is called with $x^n$
> as parameter. Then the function returns the power series $0 + O(x^n)$, which
> can be used to set the precision of a power series when constructing it.
"""
function O{T}(a::RelSeriesElem{T})
   val = pol_length(a) + valuation(a) - 1
   val < 0 && throw(DomainError())
   return parent(a)(Array{T}(0), 0, val, val)
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
   for i in 0:pol_length(a) - 1
      b $= hash(polcoeff(a, i), h) $ h
      b = (b << 1) | (b >> (sizeof(Int)*8 - 1))
   end
   return b
end

pol_length(x::RelSeriesElem) = x.length

precision(x::RelSeriesElem) = x.prec

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

function set_val!(a::SeriesElem, val::Int)
   a.val = val
end

function polcoeff(a::GenRelSeries, n::Int)
   n < 0  && throw(DomainError())
   return n >= pol_length(a) ? zero(base_ring(a)) : a.coeffs[n + 1]
end

function coeff(a::RelSeriesElem, n::Int)
   if n < valuation(a)
      return base_ring(a)()
   else
      return polcoeff(a, n - valuation(a))
   end
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
    gen{T}(R::GenRelSeriesRing{T})
> Return the generator of the power series ring, i.e. $x + O(x^{n + 1})$ where
> $n$ is the maximum precision of the power series ring $R$.
"""
function gen{T}(R::GenRelSeriesRing{T})
   S = base_ring(R)
   return R([S(1)], 1, max_precision(R) + 1, 1)
end

doc"""
    iszero(a::RelSeriesElem)
> Return `true` if the given power series is arithmetically equal to zero to
> its current precision, otherwise return `false`.
"""
iszero(a::RelSeriesElem) = pol_length(a) == 0

doc"""
    isone(a::RelSeriesElem)
> Return `true` if the given power series is arithmetically equal to one to
> its current precision, otherwise return `false`.
"""
function isone(a::RelSeriesElem)
   return valuation(a) == 0 && pol_length(a) == 1 && isone(polcoeff(a, 0))
end

doc"""
    isgen(a::GenRelSeries)
> Return `true` if the given power series is arithmetically equal to the
> generator of its power series ring to its current precision, otherwise return
> `false`.
"""
function isgen(a::RelSeriesElem)
   return valuation(a) == 1 && pol_length(a) == 1 && isone(polcoeff(a, 0))
end

doc"""
    isunit(a::RelSeriesElem)
> Return `true` if the given power series is arithmetically equal to a unit,
> i.e. is invertible, otherwise return `false`.
"""
isunit(a::RelSeriesElem) = valuation(a) == 0 && isunit(polcoeff(a, 0))

doc"""
    valuation(a::RelSeriesElem)
> Return the valuation of the given power series, i.e. the degree of the first
> nonzero term (or the precision if it is arithmetically zero).
"""
valuation(a::RelSeriesElem) = a.val

doc"""
    modulus{T <: ResElem}(a::SeriesElem{T})
> Return the modulus of the coefficients of the given polynomial.
"""
modulus{T <: ResElem}(a::SeriesElem{T}) = modulus(base_ring(a))

function deepcopy_internal{T <: RingElem}(a::GenRelSeries{T}, dict::ObjectIdDict)
   coeffs = Array{T}(pol_length(a))
   for i = 1:pol_length(a)
      coeffs[i] = deepcopy(polcoeff(a, i - 1))
   end
   return parent(a)(coeffs, pol_length(a), precision(a), valuation(a))
end

function renormalize!(z::RelSeriesElem)
   i = 0
   zlen = pol_length(z)
   zval = valuation(z)
   zprec = precision(z)
   while i < zlen && polcoeff(z, i) == 0
      i += 1
   end
   set_prec!(z, zprec)
   if i == zlen
      set_length!(z, 0)
      set_val!(z, zprec)
   else
      set_val!(z, zval + i)
      for j = 1:zlen - i
         setcoeff!(z, j - 1, polcoeff(z, j + i - 1))
      end
      set_length!(z, zlen - i)
   end
   return nothing
end

###############################################################################
#
#   AbstractString I/O
#
###############################################################################

function show{T <: RingElem}(io::IO, x::RelSeriesElem{T})
   len = pol_length(x)
   if len == 0
      print(io, zero(base_ring(x)))
   else
      coeff_printed = false
      for i = 0:len - 1
         c = polcoeff(x, i)
         bracket = needs_parentheses(c)
         if !iszero(c)
            if coeff_printed && !isnegative(c)
               print(io, "+")
            end
            if i + valuation(x) != 0
               if !isone(c) && (c != -1 || show_minus_one(elem_type(base_ring(x))))
                  if bracket
                     print(io, "(")
                  end
                  print(io, c)
                  if bracket
                     print(io, ")")
                  end
                  if i + valuation(x) != 0
                     print(io, "*")
                  end
               end
               if c == -1 && !show_minus_one(elem_type(base_ring(x)))
                  print(io, "-")
               end
               print(io, string(var(parent(x))))
               if i + valuation(x) != 1
                  print(io, "^")
                  print(io, valuation(x) + i)
               end
            else
               print(io, c)
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

needs_parentheses(x::SeriesElem) = pol_length(x) > 1

isnegative(x::SeriesElem) = pol_length(x) <= 1 && isnegative(polcoeff(x, 0))

show_minus_one{T <: RingElem}(::Type{SeriesElem{T}}) = show_minus_one(T)

###############################################################################
#
#   Unary operators
#
###############################################################################

doc"""
    -(a::RelSeriesElem)
> Return $-a$.
"""
function -{T <: RingElem}(a::RelSeriesElem{T})
   len = pol_length(a)
   z = parent(a)()
   set_prec!(z, precision(a))
   set_val!(z, valuation(a))
   fit!(z, len)
   for i = 1:len
      setcoeff!(z, i - 1, -polcoeff(a, i - 1))
   end
   return z
end

###############################################################################
#
#   Binary operators
#
###############################################################################

doc"""
    +{T <: RingElem}(a::RelSeriesElem{T}, b::RelSeriesElem{T})
> Return $a + b$.
"""
function +{T <: RingElem}(a::RelSeriesElem{T}, b::RelSeriesElem{T})
   check_parent(a, b)
   lena = pol_length(a)
   lenb = pol_length(b)
   vala = valuation(a)
   valb = valuation(b)
   valz = min(vala, valb)
   prec = min(precision(a), precision(b))
   mina = min(vala + lena, prec)
   minb = min(valb + lenb, prec)
   lenz = max(mina, minb) - valz
   R = base_ring(a)
   z = parent(a)()
   fit!(z, lenz)
   set_prec!(z, prec)
   set_val!(z, valz)
   if vala >= valb
      for i = 1:min(lenb, vala - valb)
         setcoeff!(z, i - 1, polcoeff(b, i - 1))
      end
      for i = lenb + 1:vala - valb
         setcoeff!(z, i - 1, R())
      end
      for i = vala - valb + 1:lenb
         setcoeff!(z, i - 1, polcoeff(a, i - vala + valb - 1) + polcoeff(b, i - 1))
      end
      for i = max(lenb, vala - valb) + 1:lena + vala - valb
         setcoeff!(z, i - 1, polcoeff(a, i - vala + valb - 1))
      end
      for i = lena + vala - valb + 1:lenb
         setcoeff!(z, i - 1, polcoeff(b, i - 1))
      end
   else
      for i = 1:min(lena, valb - vala)
         setcoeff!(z, i - 1, polcoeff(a, i - 1))
      end
      for i = lena + 1:valb - vala
         setcoeff!(z, i - 1, R())
      end
      for i = valb - vala + 1:lena
         setcoeff!(z, i - 1, polcoeff(a, i - 1) + polcoeff(b, i - valb + vala - 1))
      end
      for i = max(lena, valb - vala) + 1:lenb + valb - vala
         setcoeff!(z, i - 1, polcoeff(b, i - valb + vala - 1))
      end
      for i = lenb + valb - vala + 1:lena
         setcoeff!(z, i - 1, polcoeff(a, i - 1))
      end
   end
   set_length!(z, normalise(z, lenz))
   renormalize!(z)
   return z
end
  
doc"""
    -{T <: RingElem}(a::RelSeriesElem{T}, b::RelSeriesElem{T})
> Return $a - b$.
"""
function -{T <: RingElem}(a::RelSeriesElem{T}, b::RelSeriesElem{T})
   check_parent(a, b)
   lena = pol_length(a)
   lenb = pol_length(b)
   vala = valuation(a)
   valb = valuation(b)
   valz = min(vala, valb)
   prec = min(precision(a), precision(b))
   mina = min(vala + lena, prec)
   minb = min(valb + lenb, prec)
   lenz = max(mina, minb) - valz
   R = base_ring(a)
   z = parent(a)()
   fit!(z, lenz)
   set_prec!(z, prec)
   set_val!(z, valz)
   if vala >= valb
      for i = 1:min(lenb, vala - valb)
         setcoeff!(z, i - 1, -polcoeff(b, i - 1))
      end
      for i = lenb + 1:vala - valb
         setcoeff!(z, i - 1, R())
      end
      for i = vala - valb + 1:lenb
         setcoeff!(z, i - 1, polcoeff(a, i - vala + valb - 1) - polcoeff(b, i - 1))
      end
      for i = max(lenb, vala - valb) + 1:lena + vala - valb
         setcoeff!(z, i - 1, polcoeff(a, i - vala + valb - 1))
      end
      for i = lena + vala - valb + 1:lenb
         setcoeff!(z, i - 1, -polcoeff(b, i - 1))
      end
   else
      for i = 1:min(lena, valb - vala)
         setcoeff!(z, i - 1, polcoeff(a, i - 1))
      end
      for i = lena + 1:valb - vala
         setcoeff!(z, i - 1, R())
      end
      for i = valb - vala + 1:lena
         setcoeff!(z, i - 1, polcoeff(a, i - 1) - polcoeff(b, i - valb + vala - 1))
      end
      for i = max(lena, valb - vala) + 1:lenb + valb - vala
         setcoeff!(z, i - 1, -polcoeff(b, i - valb + vala - 1))
      end
      for i = lenb + valb - vala + 1:lena
         setcoeff!(z, i - 1, polcoeff(a, i - 1))
      end
   end
   set_length!(z, normalise(z, lenz))
   renormalize!(z)
   return z
end

doc"""
    *{T <: RingElem}(a::RelSeriesElem{T}, b::RelSeriesElem{T})
> Return $a\times b$.
"""
function *{T <: RingElem}(a::RelSeriesElem{T}, b::RelSeriesElem{T})
   check_parent(a, b)
   lena = pol_length(a)
   lenb = pol_length(b)
   aval = valuation(a)
   bval = valuation(b)
   zval = aval + bval
   prec = min(precision(a) - aval, precision(b) - bval)
   lena = min(lena, prec)
   lenb = min(lenb, prec)
   if lena == 0 || lenb == 0
      return parent(a)(Array{T}(0), 0, prec + zval, zval)
   end
   t = base_ring(a)()
   lenz = min(lena + lenb - 1, prec)
   d = Array{T}(lenz)
   for i = 1:min(lena, lenz)
      d[i] = polcoeff(a, i - 1)*polcoeff(b, 0)
   end
   if lenz > lena
      for j = 2:min(lenb, lenz - lena + 1)
          d[lena + j - 1] = polcoeff(a, lena - 1)*polcoeff(b, j - 1)
      end
   end
   for i = 1:lena - 1
      if lenz > i
         for j = 2:min(lenb, lenz - i + 1)
            mul!(t, polcoeff(a, i - 1), polcoeff(b, j - 1))
            addeq!(d[i + j - 1], t)
         end
      end
   end        
   z = parent(a)(d, lenz, prec + zval, zval)
   set_length!(z, normalise(z, lenz))
   renormalize!(z)
   return z
end

###############################################################################
#
#   Ad hoc binary operators
#
###############################################################################

doc"""
    *{T <: RingElem}(a::T, b::RelSeriesElem{T})
> Return $a\times b$.
"""
function *{T <: RingElem}(a::T, b::RelSeriesElem{T})
   len = pol_length(b)
   z = parent(b)()
   fit!(z, len)
   set_prec!(z, precision(b))
   set_val!(z, valuation(b))
   for i = 1:len
      setcoeff!(z, i - 1, a*polcoeff(b, i - 1))
   end
   set_length!(z, normalise(z, len))
   renormalize!(z)
   return z
end

doc"""
    *{T <: RingElem}(a::Integer, b::RelSeriesElem{T})
> Return $a\times b$.
"""
function *{T <: RingElem}(a::Integer, b::RelSeriesElem{T})
   len = pol_length(b)
   z = parent(b)()
   fit!(z, len)
   set_prec!(z, precision(b))
   set_val!(z, valuation(b))
   for i = 1:len
      setcoeff!(z, i - 1, a*polcoeff(b, i - 1))
   end
   set_length!(z, normalise(z, len))
   renormalize!(z)
   return z
end

doc"""
    *{T <: RingElem}(a::fmpz, b::RelSeriesElem{T})
> Return $a\times b$.
"""
function *{T <: RingElem}(a::fmpz, b::RelSeriesElem{T})
   len = pol_length(b)
   z = parent(b)()
   fit!(z, len)
   set_prec!(z, precision(b))
   set_val!(z, valuation(b))
   for i = 1:len
      setcoeff!(z, i - 1, a*polcoeff(b, i - 1))
   end
   set_length!(z, normalise(z, len))
   renormalize!(z)
   return z
end

doc"""
    *{T <: RingElem}(a::RelSeriesElem{T}, b::T)
> Return $a\times b$.
"""
*{T <: RingElem}(a::RelSeriesElem{T}, b::T) = b*a

doc"""
    *{T <: RingElem}(a::RelSeriesElem{T}, b::Integer)
> Return $a\times b$.
"""
*(a::RelSeriesElem, b::Integer) = b*a

doc"""
    *{T <: RingElem}(a::RelSeriesElem{T}, b::fmpz)
> Return $a\times b$.
"""
*(a::RelSeriesElem, b::fmpz) = b*a

doc"""
    +{T <: RingElem}(a::T, b::RelSeriesElem{T})
> Return $a + b$.
"""
+{T <: RingElem}(a::T, b::RelSeriesElem{T}) = parent(b)(a) + b

doc"""
    +(a::Integer, b::RelSeriesElem)
> Return $a + b$.
"""
+(a::Integer, b::RelSeriesElem) = parent(b)(a) + b

doc"""
    +(a::fmpz, b::RelSeriesElem)
> Return $a + b$.
"""
+(a::fmpz, b::RelSeriesElem) = parent(b)(a) + b

doc"""
    +{T <: RingElem}(a::RelSeriesElem{T}, b::T)
> Return $a + b$.
"""
+{T <: RingElem}(a::RelSeriesElem{T}, b::T) = b + a

doc"""
    +(a::RelSeriesElem, b::Integer)
> Return $a + b$.
"""
+(a::RelSeriesElem, b::Integer) = b + a

doc"""
    +(a::RelSeriesElem, b::fmpz)
> Return $a + b$.
"""
+(a::RelSeriesElem, b::fmpz) = b + a

doc"""
    -{T <: RingElem}(a::T, b::RelSeriesElem{T})
> Return $a - b$.
"""
-{T <: RingElem}(a::T, b::RelSeriesElem{T}) = parent(b)(a) - b

doc"""
    -(a::Integer, b::RelSeriesElem)
> Return $a - b$.
"""
-(a::Integer, b::RelSeriesElem) = parent(b)(a) - b

doc"""
    -(a::fmpz, b::RelSeriesElem)
> Return $a - b$.
"""
-(a::fmpz, b::RelSeriesElem) = parent(b)(a) - b

doc"""
    -{T <: RingElem}(a::RelSeriesElem{T}, b::T)
> Return $a - b$.
"""
-{T <: RingElem}(a::RelSeriesElem{T}, b::T) = a - parent(a)(b)

doc"""
    -(a::RelSeriesElem, b::Integer)
> Return $a - b$.
"""
-(a::RelSeriesElem, b::Integer) = a - parent(a)(b)

doc"""
    -(a::RelSeriesElem, b::fmpz)
> Return $a - b$.
"""
-(a::RelSeriesElem, b::fmpz) = a - parent(a)(b)

###############################################################################
#
#   Shifting
#
###############################################################################

doc"""
    shift_left(x::RelSeriesElem, n::Int)
> Return the power series $f$ shifted left by $n$ terms, i.e. multiplied by
> $x^n$.
"""
function shift_left{T <: RingElem}(x::RelSeriesElem{T}, len::Int)
   len < 0 && throw(DomainError())
   xlen = pol_length(x)
   if xlen == 0
      z = zero(parent(x))
      set_prec!(z, precision(x) + len)
      set_val!(z, valuation(x) + len)
      return z
   end
   z = parent(x)()
   fit!(z, xlen)
   set_prec!(z, precision(x) + len)
   set_val!(z, valuation(x) + len)
   for i = 1:xlen
      setcoeff!(z, i - 1, polcoeff(x, i - 1))
   end
   return z
end

doc"""
    shift_right(f::RelSeriesElem, n::Int)
> Return the power series $f$ shifted right by $n$ terms, i.e. divided by
> $x^n$.
"""
function shift_right{T <: RingElem}(x::RelSeriesElem{T}, len::Int)
   len < 0 && throw(DomainError())
   xlen = pol_length(x)
   xval = valuation(x)
   xprec = precision(x)
   z = parent(x)()
   if len >= xlen + xval
      set_prec!(z, max(0, xprec - len))
      set_val!(z, max(0, xprec - len))
   else
      zlen = min(xlen + xval - len, xlen)
      fit!(z, zlen)
      set_prec!(z, max(0, xprec - len))
      set_val!(z, max(0, xval - len))
      for i = 1:zlen
         setcoeff!(z, i - 1, polcoeff(x, i + xlen  - zlen - 1))
      end
      renormalize!(z)
   end
   return z
end

###############################################################################
#
#   Truncation
#
###############################################################################

doc"""
    truncate(a::RelSeriesElem, n::Int)
> Return $a$ truncated to $n$ terms.
"""
function truncate{T <: RingElem}(a::RelSeriesElem{T}, prec::Int)
   prec < 0 && throw(DomainError())
   alen = pol_length(a)
   aprec = precision(a)
   aval = valuation(a)
   if aprec + aval <= prec
      return a
   end
   z = parent(a)()
   set_prec!(z, prec)
   if prec <= aval
      set_length!(z, 0)
      set_val!(z, prec)
   else
      fit!(z, prec - aval)
      for i = 1:min(prec - aval, alen)
         setcoeff!(z, i - 1, polcoeff(a, i - 1))
      end
      set_length!(z, normalise(z, prec - aval))
      set_val!(z, aval)
   end
   return z
end

###############################################################################
#
#   Powering
#
###############################################################################

doc"""
    ^{T <: RingElem}(a::RelSeriesElem{T}, b::Int)
> Return $a^b$. We require $b \geq 0$.
"""
function ^{T <: RingElem}(a::RelSeriesElem{T}, b::Int)
   b < 0 && throw(DomainError())
   # special case powers of x for constructing power series efficiently
   if isgen(a)
      z = parent(a)()
      fit!(z, 1)
      set_prec!(z, b + precision(a) - 1)
      setcoeff!(z, 0, polcoeff(a, 0))
      set_val!(z, b)
      set_length!(z, 1)
      return z
   elseif pol_length(a) == 0
      z = parent(a)()
      set_prec!(z, b*valuation(a))
      set_val!(z, b*valuation(a))
      return z
   elseif pol_length(a) == 1
      z = parent(a)(polcoeff(a, 0)^b)
      set_prec!(z, (b - 1)*valuation(a) + precision(a))
      set_val!(z, b*valuation(a))
      return z
   elseif b == 0
      z = one(parent(a))
      set_prec!(z, precision(a) - valuation(a))
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
    =={T <: RingElem}(x::RelSeriesElem{T}, y::RelSeriesElem{T})
> Return `true` if $x == y$ arithmetically, otherwise return `false`. Recall
> that power series to different precisions may still be arithmetically
> equal to the minimum of the two precisions.
"""
function =={T <: RingElem}(x::RelSeriesElem{T}, y::RelSeriesElem{T})
   check_parent(x, y)
   xval = valuation(x)
   xprec = precision(x)
   yval = valuation(y)
   yprec = precision(y)
   prec = min(xprec, yprec)
   if prec <= xval && prec <= yval
      return true
   end
   if xval != yval
      return false
   end
   xlen = min(pol_length(x), prec - xval)
   ylen = min(pol_length(y), prec - yval)
   if xlen != ylen
      return false
   end
   for i = 1:xlen
      if polcoeff(x, i - 1) != polcoeff(y, i - 1)
         return false
      end
   end
   return true
end

doc"""
    isequal{T <: RingElem}(x::RelSeriesElem{T}, y::RelSeriesElem{T})
> Return `true` if $x == y$ exactly, otherwise return `false`. Only if the
> power series are precisely the same, to the same precision, are they declared
> equal by this function.
"""
function isequal{T <: RingElem}(x::RelSeriesElem{T}, y::RelSeriesElem{T})
   if parent(x) != parent(y)
      return false
   end
   if precision(x) != precision(y) || pol_length(x) != pol_length(y) ||
      valuation(x) != valuation(y)
      return false
   end
   for i = 1:pol_length(x)
      if !isequal(polcoeff(x, i - 1), polcoeff(y, i - 1))
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
    =={T <: RingElem}(x::RelSeriesElem{T}, y::T)
> Return `true` if $x == y$ arithmetically, otherwise return `false`.
"""
=={T <: RingElem}(x::RelSeriesElem{T}, y::T) = precision(x) == 0 ||
           ((pol_length(x) == 0 && y == 0) || (pol_length(x) == 1 && 
             valuation(x) == 0 && polcoeff(x, 0) == y))

doc"""
    =={T <: RingElem}(x::T, y::RelSeriesElem{T})
> Return `true` if $x == y$ arithmetically, otherwise return `false`.
"""
=={T <: RingElem}(x::T, y::RelSeriesElem{T}) = y == x

doc"""
    ==(x::RelSeriesElem, y::Integer)
> Return `true` if $x == y$ arithmetically, otherwise return `false`.
"""
==(x::RelSeriesElem, y::Integer) = precision(x) == 0 ||
                  ((pol_length(x) == 0 && y == 0) || (pol_length(x) == 1 && 
                    valuation(x) == 0 && polcoeff(x, 0) == y))

doc"""
    ==(x::RelSeriesElem, y::fmpz)
> Return `true` if $x == y$ arithmetically, otherwise return `false`.
"""
==(x::RelSeriesElem, y::fmpz) = precision(x) == 0 ||
                  ((pol_length(x) == 0 && y == 0) || (pol_length(x) == 1 && 
                    valuation(x) == 0 && polcoeff(x, 0) == y))

doc"""
    ==(x::Integer, y::RelSeriesElem)
> Return `true` if $x == y$ arithmetically, otherwise return `false`.
"""
==(x::Integer, y::RelSeriesElem) = y == x

doc"""
    ==(x::fmpz, y::RelSeriesElem)
> Return `true` if $x == y$ arithmetically, otherwise return `false`.
"""
==(x::fmpz, y::RelSeriesElem) = y == x

###############################################################################
#
#   Exact division
#
###############################################################################

doc"""
    divexact{T <: RingElem}(a::RelSeriesElem{T}, b::RelSeriesElem{T})
> Return $a/b$. Requires $b$ to be invertible.
"""
function divexact{T <: RingElem}(x::RelSeriesElem{T}, y::RelSeriesElem{T})
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
    divexact{T <: RingElem}(a::RelSeriesElem{T}, b::Integer)
> Return $a/b$ where the quotient is expected to be exact.
"""
function divexact{T <: RingElem}(x::RelSeriesElem{T}, y::Integer)
   y == 0 && throw(DivideError())
   lenx = pol_length(x)
   z = parent(x)()
   fit!(z, lenx)
   set_prec!(z, precision(x))
   set_val!(z, valuation(x))
   for i = 1:lenx
      setcoeff!(z, i - 1, divexact(polcoeff(x, i - 1), y))
   end
   return z
end

doc"""
    divexact{T <: RingElem}(a::RelSeriesElem{T}, b::fmpz)
> Return $a/b$ where the quotient is expected to be exact.
"""
function divexact{T <: RingElem}(x::RelSeriesElem{T}, y::fmpz)
   y == 0 && throw(DivideError())
   lenx = pol_length(x)
   z = parent(x)()
   fit!(z, lenx)
   set_prec!(z, precision(x))
   set_val!(z, valuation(x))
   for i = 1:lenx
      setcoeff!(z, i - 1, divexact(polcoeff(x, i - 1), y))
   end
   return z
end

doc"""
    divexact{T <: RingElem}(a::RelSeriesElem{T}, b::T)
> Return $a/b$ where the quotient is expected to be exact.
"""
function divexact{T <: RingElem}(x::RelSeriesElem{T}, y::T)
   y == 0 && throw(DivideError())
   lenx = pol_length(x)
   z = parent(x)()
   fit!(z, lenx)
   set_prec!(z, precision(x))
   set_val!(z, valuation(x))
   for i = 1:lenx
      setcoeff!(z, i - 1, divexact(polcoeff(x, i - 1), y))
   end
   return z
end

###############################################################################
#
#   Inversion
#
###############################################################################

doc"""
   inv(a::RelSeriesElem)
> Return the inverse of the power series $a$, i.e. $1/a$.
"""
function inv(a::RelSeriesElem)
   a == 0 && throw(DivideError())
   !isunit(a) && error("Unable to invert power series")
   a1 = polcoeff(a, 0)
   ainv = parent(a)()
   fit!(ainv, precision(a))
   set_prec!(ainv, precision(a))
   if precision(a) != 0
      setcoeff!(ainv, 0, divexact(one(base_ring(a)), a1))
   end
   a1 = -a1
   for n = 2:precision(a)
      s = polcoeff(a, 1)*polcoeff(ainv, n - 2)
      for i = 2:min(n, pol_length(a)) - 1
         s += polcoeff(a, i)*polcoeff(ainv, n - i - 1)
      end
      setcoeff!(ainv, n - 1, divexact(s, a1))
   end
   set_length!(ainv, normalise(ainv, precision(a)))
   set_val!(ainv, 0)
   return ainv
end

###############################################################################
#
#   Special functions
#
###############################################################################

doc"""
    exp(a::RelSeriesElem)
> Return the exponential of the power series $a$.
"""
function exp(a::RelSeriesElem)
   if a == 0
      z = one(parent(a))
      set_prec!(z, precision(a))
      set_val!(z, valuation(a))
      return z
   end
   z = parent(a)()
   R = base_ring(a)
   vala = valuation(a)
   preca = precision(a)
   fit!(z, preca)
   set_prec!(z, preca)
   c = vala == 0 ? polcoeff(a, 0) : R()
   setcoeff!(z, 0, exp(c))
   len = pol_length(a) + vala
   for k = 1 : preca - 1
      s = R()
      for j = 1 : min(k + 1, len) - 1
         c = j >= vala ? polcoeff(a, j - vala) : R()
         s += j * c * polcoeff(z, k - j)
      end
      !isunit(R(k)) && error("Unable to divide in exp")
      setcoeff!(z, k, divexact(s, k))
   end
   set_length!(z, normalise(z, preca))
   set_val!(z, 0)
   return z
end

###############################################################################
#
#   Unsafe functions
#
###############################################################################

function zero!{T <: RingElem}(a::GenRelSeries{T})
   a.length = 0
   a.prec = parent(a).prec_max
end

function fit!{T <: RingElem}(c::GenRelSeries{T}, n::Int)
   if length(c.coeffs) < n
      t = c.coeffs
      c.coeffs = Array{T}(n)
      for i = 1:c.length
         c.coeffs[i] = t[i]
      end
      for i = pol_length(c) + 1:n
         c.coeffs[i] = zero(base_ring(c))
      end
   end
end

function setcoeff!{T <: RingElem}(c::GenRelSeries{T}, n::Int, a::T)
   if (a != 0 && precision(c) > n) || n + 1 <= c.length
      fit!(c, n + 1)
      c.coeffs[n + 1] = a
      c.length = max(pol_length(c), n + 1)
      # don't normalise
   end
end

function mul!{T <: RingElem}(c::GenRelSeries{T}, a::GenRelSeries{T}, b::GenRelSeries{T})
   lena = pol_length(a)
   lenb = pol_length(b)
   aval = valuation(a)
   bval = valuation(b)
   prec = min(precision(a) - aval, precision(b) - bval)
   lena = min(lena, prec)
   lenb = min(lenb, prec)
   if lena <= 0 || lenb <= 0
      c.length = 0
   else
      t = base_ring(a)()
      lenc = min(lena + lenb - 1, prec)
      fit!(c, lenc)
      for i = 1:min(lena, lenc)
         mul!(c.coeffs[i], polcoeff(a, i - 1), polcoeff(b, 0))
      end
      if lenc > lena
         for i = 2:min(lenb, lenc - lena + 1)
            mul!(c.coeffs[lena + i - 1], polcoeff(a, lena - 1), polcoeff(b, i - 1))
         end
      end
      for i = 1:lena - 1
         if lenc > i
            for j = 2:min(lenb, lenc - i + 1)
               mul!(t, polcoeff(a, i - 1), polcoeff(b, j - 1))
               addeq!(c.coeffs[i + j - 1], t)
            end
         end
      end        
      c.length = normalise(c, lenc)
   end
   c.val = a.val + b.val
   c.prec = prec + c.val
   renormalize!(z)
   return nothing
end

function addeq!{T <: RingElem}(c::GenRelSeries{T}, a::GenRelSeries{T})
   lenc = pol_length(c)
   lena = pol_length(a)
   precc = precision(c)
   preca = precision(a)
   valc = valuation(c)
   vala = valuation(a)
   prec = min(precc, preca)
   vala = min(vala, prec)
   valc = min(valc, prec)
   lena = min(lena, max(0, prec - vala))
   lenc = min(lenc, max(0, prec - valc))
   valr = min(vala, valc)
   lenr = max(lena + vala, lenc + valc) - valr
   R = base_ring(c)
   fit!(c, lenr)
   if valc > vala
      for i = lena:-1:1
         c.coeffs[i + valc - vala] = c.coeffs[i]
      end
      for i = 1:min(valc, lena)
         c.coeffs[i] = a.coeffs[i]
      end
      for i = lena + 1:valc
         c.coeffs[i] = R()
      end
      for i = valc + 1:min(lena, lenc + valc - vala)
         addeq!(c.coeffs[i], a.coeffs[i])
      end
      for i = lenc + valc - vala + 1:lena
         c.coeffs[i] = a.coeffs[i]
      end
   else
      for i = 1:min(lena, lenc - vala + valc)
         addeq!(c.coeffs[i + vala - valc], a.coeffs[i])
      end
      for i = lenc + 1:lena + vala - valc
         c.coeffs[i] = a.coeffs[i - vala + valc]
      end
   end
   c.length = normalise(c, lenr)
   c.prec = prec
   c.val = valr
   renormalise!(c)
end

function add!{T <: RingElem}(c::SeriesElem{T}, a::SeriesElem{T}, b::SeriesElem{T})
   lena = length(a)
   lenb = length(b)
   prec = min(precision(a), precision(b))
   lena = min(lena, prec)
   lenb = min(lenb, prec)
   lenc = max(lena, lenb)
   fit!(c, lenc)
   set_prec!(c, prec)
   i = 1
   while i <= min(lena, lenb)
      setcoeff!(c, i - 1, coeff(a, i - 1) + coeff(b, i - 1))
      i += 1
   end
   while i <= lena
      setcoeff!(c, i - 1, coeff(a, i - 1))
      i += 1
   end
   while i <= lenb
      setcoeff!(c, i - 1, coeff(b, i - 1))
      i += 1
   end
   set_length!(c, normalise(c, i - 1))
   nothing
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

function (a::GenRelSeriesRing{T}){T <: RingElem}(b::RingElem)
   return a(base_ring(a)(b))
end

function (a::GenRelSeriesRing{T}){T <: RingElem}()
   z = GenRelSeries{T}(Array{T}(0), 0, a.prec_max, a.prec_max)
   z.parent = a
   return z
end

function (a::GenRelSeriesRing{T}){T <: RingElem}(b::Integer)
   if b == 0
      z = GenRelSeries{T}(Array{T}(0), 0, a.prec_max, a.prec_max)
   else
      z = GenRelSeries{T}([base_ring(a)(b)], 1, a.prec_max, 0)
   end
   z.parent = a
   return z
end

function (a::GenRelSeriesRing{T}){T <: RingElem}(b::fmpz)
   if b == 0
      z = GenRelSeries{T}(Array{T}(0), 0, a.prec_max, a.prec_max)
   else
      z = GenRelSeries{T}([base_ring(a)(b)], 1, a.prec_max, 0)
   end
   z.parent = a
   return z
end

function (a::GenRelSeriesRing{T}){T <: RingElem}(b::T)
   parent(b) != base_ring(a) && error("Unable to coerce to power series")
   if b == 0
      z = GenRelSeries{T}(Array{T}(0), 0, a.prec_max, a.prec_max)
   else
      z = GenRelSeries{T}([b], 1, a.prec_max, 0)
   end
   z.parent = a
   return z
end

function (a::GenRelSeriesRing{T}){T <: RingElem}(b::RelSeriesElem{T})
   parent(b) != a && error("Unable to coerce power series")
   return b
end

function (a::GenRelSeriesRing{T}){T <: RingElem}(b::Array{T, 1}, len::Int, prec::Int, val::Int)
   if length(b) > 0
      parent(b[1]) != base_ring(a) && error("Unable to coerce to power series")
   end
   z = GenRelSeries{T}(b, len, prec, val)
   z.parent = a
   return z
end

###############################################################################
#
#   PowerSeriesRing constructor
#
###############################################################################

doc"""
   PowerSeriesRing(R::Ring, prec::Int, s::AbstractString; cached=true, model=:capped_relative)
> Return a tuple $(S, x)$ consisting of the parent object `S` of a power series
> ring over the given base ring and a generator `x` for the power series ring.
> The maximum precision of power series in the ring is set to `prec`. If the
> model is set to `:capped_relative` this is taken as a maximum relative
> precision, and if it is set to `:capped_absolute` this is take to be a 
> maximum absolute precision. The supplied string `s` specifies the way the
> generator of the power series ring will be printed. By default, the parent
> object `S` will be cached so that supplying the same base ring, string and
> precision in future will return the same parent object and generator. If
> caching of the parent object is not required, `cached` can be set to `false`.
"""
function PowerSeriesRing(R::Ring, prec::Int, s::AbstractString; cached=true, model=:capped_relative)
   S = Symbol(s)
   T = elem_type(R)
   
   if model == :capped_relative
      parent_obj = GenRelSeriesRing{T}(R, prec, S, cached)
   elseif model == :capped_absolute
      parent_obj = GenAbsSeriesRing{T}(R, prec, S, cached)
   end

   return parent_obj, gen(parent_obj)
end
