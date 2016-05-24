###############################################################################
#
#   CapRelSeries.jl : Power series over rings, capped relative precision
#
###############################################################################    

export GenCapRelSeries, GenCapRelPowerSeriesRing, PowerSeriesRing, O, valuation, exp,
       precision, max_precision, set_prec!

###############################################################################
#
#   Data type and parent object methods
#
###############################################################################

function O{T}(a::SeriesElem{T})
   prec = length(a) - 1
   prec < 0 && throw(DomainError())
   z = GenCapRelSeries{T}(Array(T, 0), 0, prec)
   z.parent = parent(a)
   return z
end

parent_type{T}(::Type{GenCapRelSeries{T}}) = GenCapRelPowerSeriesRing{T}

parent(a::SeriesElem) = a.parent

elem_type{T <: RingElem}(::GenCapRelPowerSeriesRing{T}) = GenCapRelSeries{T}

base_ring{T}(R::SeriesRing{T}) = R.base_ring::parent_type(T)

base_ring(a::SeriesElem) = base_ring(parent(a))

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

max_precision(R::SeriesRing) = R.prec_max

function normalise(a::SeriesElem, len::Int)
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

function coeff(a::SeriesElem, n::Int)
   n < 0  && throw(DomainError())
   return n >= length(a) ? zero(base_ring(a)) : a.coeffs[n + 1]
end

zero(R::SeriesRing) = R(0)

one(R::SeriesRing) = R(1)

function gen{T}(R::SeriesRing{T})
   S = base_ring(R)
   z = GenCapRelSeries{T}([S(0), S(1)], 2, max_precision(R) + 1)
   z.parent = R
   return z
end

iszero(a::SeriesElem) = length(a) == 0

function isone(a::SeriesElem)
   return length(a) == 1 && isone(coeff(a, 0))
end

function isgen(a::SeriesElem)
   return length(a) == 2 && iszero(coeff(a, 0)) && isone(coeff(a, 1))
end

isunit(a::SeriesElem) = isunit(coeff(a, 0))

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

function -{T <: RingElem}(a::SeriesElem{T})
   len = length(a)
   d = Array(T, len)
   for i = 1:len
      d[i] = -coeff(a, i - 1)
   end
   return parent(a)(d, len, precision(a))
end

###############################################################################
#
#   Binary operators
#
###############################################################################

function +{T <: RingElem}(a::SeriesElem{T}, b::SeriesElem{T})
   check_parent(a, b)
   lena = length(a)
   lenb = length(b)
         
   prec = min(precision(a), precision(b))
 
   lena = min(lena, prec)
   lenb = min(lenb, prec)

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

   z = parent(a)(d, i - 1, prec)

   set_length!(z, normalise(z, i - 1))

   return z
end
  
function -{T <: RingElem}(a::SeriesElem{T}, b::SeriesElem{T})
   check_parent(a, b)
   lena = length(a)
   lenb = length(b)
   
   prec = min(precision(a), precision(b))
   
   lena = min(lena, prec)
   lenb = min(lenb, prec)
   
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

   z = parent(a)(d, i - 1, prec)

   set_length!(z, normalise(z, i - 1))

   return z
end

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

function *{T <: RingElem}(a::Integer, b::SeriesElem{T})
   len = length(b)
   d = Array(T, len)
   for i = 1:len
      d[i] = a*coeff(b, i - 1)
   end
   z = parent(b)(d, len, precision(b))
   set_length!(z, normalise(z, len))
   return z
end

function *{T <: RingElem}(a::fmpz, b::SeriesElem{T})
   len = length(b)
   d = Array(T, len)
   for i = 1:len
      d[i] = a*coeff(b, i - 1)
   end
   z = parent(b)(d, len, precision(b))
   set_length!(z, normalise(z, len))
   return z
end

*(a::SeriesElem, b::Integer) = b*a

*(a::SeriesElem, b::fmpz) = b*a

###############################################################################
#
#   Shifting
#
###############################################################################

function shift_left{T <: RingElem}(x::SeriesElem{T}, len::Int)
   len < 0 && throw(DomainError())
   xlen = length(x)
   if xlen == 0
      return parent(x)(Array(T, 0), 0, precision(x) + len)
   end
   v = Array(T, xlen + len)
   for i = 1:len
      v[i] = zero(base_ring(x))
   end
   for i = 1:xlen
      v[i + len] = coeff(x, i - 1)
   end
   return parent(x)(v, xlen + len, precision(x) + len)
end

function shift_right{T <: RingElem}(x::SeriesElem{T}, len::Int)
   len < 0 && throw(DomainError())
   xlen = length(x)
   if len >= xlen
      return parent(x)(Array(T, 0), 0, max(0, precision(x) - len))
   end
   v = Array(T, xlen - len)
   for i = 1:xlen - len
      v[i] = coeff(x, i + len - 1)
   end
   return parent(x)(v, xlen - len, precision(x) - len)
end

###############################################################################
#
#   Truncation
#
###############################################################################

function truncate{T <: RingElem}(x::SeriesElem{T}, prec::Int)
   prec < 0 && throw(DomainError())
   len = length(x)
   if precision(x) <= prec
      return x
   end
   d = Array(T, prec)
   for i = 1:min(prec, len)
      d[i] = coeff(x, i - 1)
   end
   for i = len + 1:prec
      d[i] = zero(base_ring(x))
   end
   z = parent(x)(d, prec, prec)
   set_length!(z, normalise(z, prec))
   return z
end

###############################################################################
#
#   Powering
#
###############################################################################

function ^{T <: RingElem}(a::SeriesElem{T}, b::Int)
   b < 0 && throw(DomainError())
   # special case powers of x for constructing power series efficiently
   if isgen(a)
      d = Array(T, b + 1)
      d[b + 1] = coeff(a, 1)
      for i = 1:b
         d[i] = coeff(a, 0)
      end
      return parent(a)(d, b + 1, precision(a) + b - 1)
   elseif length(a) == 0
      return parent(a)(Array(T, 0), 0, precision(a) + (b - 1)*valuation(a))
   elseif length(a) == 1
      return parent(a)([coeff(a, 0)^b], 1, precision(a))
   elseif b == 0
      return parent(a)([one(base_ring(a))], 1, precision(a))
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

==(x::SeriesElem, y::Int) = precision(x) == 0 || ((length(x) == 0 && y == 0)
                                       || (length(x) == 1 && coeff(x, 0) == y))

==(x::SeriesElem, y::fmpz) = precision(x) == 0 || ((length(x) == 0 && y == 0)
                                       || (length(x) == 1 && coeff(x, 0) == y))

==(x::Int, y::SeriesElem) = y == x

==(x::fmpz, y::SeriesElem) = y == x

###############################################################################
#
#   Exact division
#
###############################################################################

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

function divexact{T <: RingElem}(x::SeriesElem{T}, y::Integer)
   y == 0 && throw(DivideError())
   lenx = length(x)
   d = Array(T, lenx)
   for i = 1:lenx
      d[i] = divexact(coeff(x, i - 1), y)
   end
   return parent(x)(d, lenx, precision(x))
end

function divexact{T <: RingElem}(x::SeriesElem{T}, y::fmpz)
   y == 0 && throw(DivideError())
   lenx = length(x)
   d = Array(T, lenx)
   for i = 1:lenx
      d[i] = divexact(coeff(x, i - 1), y)
   end
   return parent(x)(d, lenx, precision(x))
end

function divexact{T <: RingElem}(x::SeriesElem{T}, y::T)
   y == 0 && throw(DivideError())
   lenx = length(x)
   d = Array(T, lenx)
   for i = 1:lenx
      d[i] = divexact(coeff(x, i - 1), y)
   end
   return parent(x)(d, lenx, precision(x))
end

###############################################################################
#
#   Inversion
#
###############################################################################

function inv{T <: RingElem}(a::SeriesElem{T})
   a == 0 && throw(DivideError())
   !isunit(a) && error("Unable to invert power series")
   a1 = coeff(a, 0)
   d = Array(T, precision(a))
   if precision(a) != 0
      d[1] = divexact(one(base_ring(a)), a1)
   end
   a1 = -a1
   for n = 2:precision(a)
      s = coeff(a, 1)*d[n - 1]
      for i = 2:min(n, length(a)) - 1
         s += coeff(a, i)*d[n - i]
      end
      d[n] = divexact(s, a1)
   end
   ainv = parent(a)(d, precision(a), precision(a))
   set_length!(ainv, normalise(ainv, precision(a)))
   return ainv
end

###############################################################################
#
#   Special functions
#
###############################################################################

function exp{T <: RingElem}(a::SeriesElem{T})
   if a == 0
      return parent(a)([one(base_ring(a))], 1, precision(a))
   end
   d = Array(T, precision(a))
   d[0 + 1] = exp(coeff(a, 0))
   len = length(a)
   for k = 1 : precision(a) - 1
      s = zero(base_ring(a))
      for j = 1 : min(k + 1, len) - 1
         s += j * coeff(a, j) * d[k - j + 1]
      end
      !isunit(base_ring(a)(k)) && error("Unable to divide in exp")
      d[k + 1] = divexact(s, k)
   end
   b = parent(a)(d, precision(a), precision(a))
   set_length!(b, normalise(b, precision(a)))
   return b
end

###############################################################################
#
#   Unsafe functions
#
###############################################################################

function fit!{T <: RingElem}(c::SeriesElem{T}, n::Int)
   if c.length < n
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

function setcoeff!{T <: RingElem}(c::SeriesElem{T}, n::Int, a::T)
   if (a != 0 && precision(c) > n) || n + 1 <= c.length
      fit!(c, n + 1)
      c.coeffs[n + 1] = a
      c.length = max(length(c), n + 1)
      # don't normalise
   end
end

function mul!{T <: RingElem}(c::SeriesElem{T}, a::SeriesElem{T}, b::SeriesElem{T})
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

function addeq!{T <: RingElem}(c::SeriesElem{T}, a::SeriesElem{T})
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

function Base.promote_rule{T <: RingElem, V <: Integer}(::Type{GenCapRelSeries{T}}, ::Type{V})
   return GenCapRelSeries{T}
end

function Base.promote_rule{T <: RingElem}(::Type{GenCapRelSeries{T}}, ::Type{T})
   return GenCapRelSeries{T}
end

function promote_rule1{T <: RingElem, U <: RingElem}(::Type{GenCapRelSeries{T}}, ::Type{GenCapRelSeries{U}})
   Base.promote_rule(T, GenCapRelSeries{U}) == T ? GenCapRelSeries{T} : Union{}
end

function Base.promote_rule{T <: RingElem, U <: RingElem}(::Type{GenCapRelSeries{T}}, ::Type{U})
   Base.promote_rule(T, U) == T ? GenCapRelSeries{T} : promote_rule1(U, GenCapRelSeries{T})
end

###############################################################################
#
#   Parent object call overload
#
###############################################################################

function Base.call{T <: RingElem}(a::GenCapRelPowerSeriesRing{T}, b::RingElem)
   return a(base_ring(a)(b))
end

function Base.call{T <: RingElem}(a::GenCapRelPowerSeriesRing{T})
   z = GenCapRelSeries{T}(Array(T, 0), 0, a.prec_max)
   z.parent = a
   return z
end

function Base.call{T <: RingElem}(a::GenCapRelPowerSeriesRing{T}, b::Integer)
   if b == 0
      z = GenCapRelSeries{T}(Array(T, 0), 0, a.prec_max)
   else
      z = GenCapRelSeries{T}([base_ring(a)(b)], 1, a.prec_max)
   end
   z.parent = a
   return z
end

function Base.call{T <: RingElem}(a::GenCapRelPowerSeriesRing{T}, b::T)
   parent(b) != base_ring(a) && error("Unable to coerce to power series")
   if b == 0
      z = GenCapRelSeries{T}(Array(T, 0), 0, a.prec_max)
   else
      z = GenCapRelSeries{T}([b], 1, a.prec_max)
   end
   z.parent = a
   return z
end

function Base.call{T <: RingElem}(a::GenCapRelPowerSeriesRing{T}, b::SeriesElem{T})
   parent(b) != a && error("Unable to coerce power series")
   return b
end

function Base.call{T <: RingElem}(a::GenCapRelPowerSeriesRing{T}, b::Array{T, 1}, len::Int, prec::Int)
   if length(b) > 0
      parent(b[1]) != base_ring(a) && error("Unable to coerce to power series")
   end
   z = GenCapRelSeries{T}(b, len, prec)
   z.parent = a
   return z
end

###############################################################################
#
#   PowerSeriesRing constructor
#
###############################################################################

function PowerSeriesRing(R::Ring, prec::Int, s::AbstractString{}; cached=true)
   S = Symbol(s)
   T = elem_type(R)
   parent_obj = GenCapRelPowerSeriesRing{T}(R, prec, S, cached)

   return parent_obj, parent_obj([R(0), R(1)], 2, prec + 1)
end
