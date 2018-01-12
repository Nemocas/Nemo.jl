###############################################################################
#
#   LaurentSeries.jl : Laurent series over rings and fields,
#                      capped relative precision
#
###############################################################################

###############################################################################
#
#   Data type and parent object methods
#
###############################################################################

doc"""
    O{T <: RingElement}(a::LaurentSeriesElem{T})
> Returns $0 + O(x^\mbox{deg}(a))$. Usually this function is called with $x^n$
> as parameter. Then the function returns the power series $0 + O(x^n)$, which
> can be used to set the precision of a power series when constructing it.
"""
function O(a::LaurentSeriesElem{T}) where T <: RingElement
   val = pol_length(a) + valuation(a) - 1
   return parent(a)(Array{T}(0), 0, val, val)
end

parent_type(::Type{T}) where {S <: RingElement, T <: LaurentSeriesRingElem{S}} = LaurentSeriesRing{S}

parent_type(::Type{T}) where {S <: FieldElement, T <: LaurentSeriesFieldElem{S}} = LaurentSeriesField{S}

doc"""
    parent(a::LaurentSeriesElem)
> Return the parent of the given power series.
"""
parent(a::LaurentSeriesElem) = a.parent

elem_type(::Type{T}) where {S <: RingElement, T <: LaurentSeriesRing{S}} = LaurentSeriesRingElem{S}

elem_type(::Type{T}) where {S <: FieldElement, T <: LaurentSeriesField{S}} = LaurentSeriesFieldElem{S}

doc"""
    base_ring(R::LaurentSeriesRing)
> Return the base ring of the given power series ring.
"""
base_ring(R::LaurentSeriesRing{T}) where T <: RingElement = R.base_ring::parent_type(T)

doc"""
    base_ring(R::LaurentSeriesField)
> Return the base ring of the given power series ring.
"""
base_ring(R::LaurentSeriesField{T}) where T <: FieldElement = R.base_ring::parent_type(T)

doc"""
    base_ring(a::LaurentSeriesElem)
> Return the base ring of the power series ring of the given power series.
"""
base_ring(a::LaurentSeriesElem) = base_ring(parent(a))

function isdomain_type(::Type{T}) where {S <: RingElement, T <: LaurentSeriesElem{S}}
   return isdomain_type(S)
end

isexact_type(a::Type{T}) where T <: LaurentSeriesElem = false

doc"""
    var(a::LaurentSeriesRing)
> Return the internal name of the generator of the power series ring. Note that
> this is returned as a `Symbol` not a `String`.
"""
var(a::LaurentSeriesRing) = a.S

doc"""
    var(a::LaurentSeriesField)
> Return the internal name of the generator of the power series ring. Note that
> this is returned as a `Symbol` not a `String`.
"""
var(a::LaurentSeriesField) = a.S

function check_parent(a::LaurentSeriesElem, b::LaurentSeriesElem)
   parent(a) != parent(b) &&
             error("Incompatible power series rings in power series operation")
end

###############################################################################
#
#   Basic manipulation
#
###############################################################################

function Base.hash(a::LaurentSeriesElem, h::UInt)
   b = 0xb163af5694734274%UInt
   for i in 0:pol_length(a) - 1
      b = xor(b, hash(polcoeff(a, i), h), h)
      b = (b << 1) | (b >> (sizeof(Int)*8 - 1))
   end
   return b
end

doc"""
    pol_length(a::LaurentSeriesElem)
> Return the length of the polynomial underlying the given power series. This
> will be zero if the power series has no nonzero terms.
"""
pol_length(a::LaurentSeriesElem) = a.length

doc"""
    precision(a::LaurentSeriesElem)
> Return the precision of the given power series in absolute terms. This will
> be the sum of the valuation and the length of the underlying polynomial.
"""
precision(a::LaurentSeriesElem) = a.prec

doc"""
    valuation(a::LaurentSeriesElem)
> Return the valuation of the given power series, i.e. the degree of the first
> nonzero term (or the precision if it is arithmetically zero).
"""
valuation(a::LaurentSeriesElem) = a.val

doc"""
    max_precision(R::LaurentSeriesRing)
> Return the maximum relative precision of power series in the given power
> series ring.
"""
max_precision(R::LaurentSeriesRing) = R.prec_max

doc"""
    max_precision(R::LaurentSeriesField)
> Return the maximum relative precision of power series in the given power
> series ring.
"""
max_precision(R::LaurentSeriesField) = R.prec_max

function normalise(a::LaurentSeriesElem, len::Int)
   while len > 0 && iszero(a.coeffs[len])
      len -= 1
   end
   return len
end

function set_length!(a::LaurentSeriesElem, len::Int)
   a.length = len
end

function set_prec!(a::LaurentSeriesElem, prec::Int)
   a.prec = prec
end

function set_val!(a::LaurentSeriesElem, val::Int)
   a.val = val
end

function polcoeff(a::LaurentSeriesElem, n::Int)
   n < 0  && throw(DomainError())
   return n >= pol_length(a) ? zero(base_ring(a)) : a.coeffs[n + 1]
end

function coeff(a::LaurentSeriesElem, n::Int)
   if n < valuation(a)
      return base_ring(a)()
   else
      return polcoeff(a, n - valuation(a))
   end
end

doc"""
    zero(R::LaurentSeriesRing)
> Return $0 + O(x^n)$ where $n$ is the maximum precision of the power series
> ring $R$.
"""
zero(R::LaurentSeriesRing) = R(0)

doc"""
    zero(R::LaurentSeriesField)
> Return $0 + O(x^n)$ where $n$ is the maximum precision of the power series
> ring $R$.
"""
zero(R::LaurentSeriesField) = R(0)

doc"""
    one(R::LaurentSeriesRing)
> Return $1 + O(x^n)$ where $n$ is the maximum precision of the power series
> ring $R$.
"""
one(R::LaurentSeriesField) = R(1)

doc"""
    one(R::LaurentSeriesField)
> Return $1 + O(x^n)$ where $n$ is the maximum precision of the power series
> ring $R$.
"""
one(R::LaurentSeriesRing) = R(1)

doc"""
    gen(R::LaurentSeriesRing)
> Return the generator of the power series ring, i.e. $x + O(x^{n + 1})$ where
> $n$ is the maximum precision of the power series ring $R$.
"""
function gen(R::LaurentSeriesRing)
   S = base_ring(R)
   return R([S(1)], 1, max_precision(R) + 1, 1)
end

doc"""
    gen(R::LaurentSeriesField)
> Return the generator of the power series ring, i.e. $x + O(x^{n + 1})$ where
> $n$ is the maximum precision of the power series ring $R$.
"""
function gen(R::LaurentSeriesField)
   S = base_ring(R)
   return R([S(1)], 1, max_precision(R) + 1, 1)
end

doc"""
    iszero(a::LaurentSeriesElem)
> Return `true` if the given power series is arithmetically equal to zero to
> its current precision, otherwise return `false`.
"""
iszero(a::LaurentSeriesElem) = pol_length(a) == 0

doc"""
    isone(a::LaurentSeriesElem)
> Return `true` if the given power series is arithmetically equal to one to
> its current precision, otherwise return `false`.
"""
function isone(a::LaurentSeriesElem)
   return valuation(a) == 0 && pol_length(a) == 1 && isone(polcoeff(a, 0))
end

doc"""
    isgen(a::LaurentSeriesElem)
> Return `true` if the given power series is arithmetically equal to the
> generator of its power series ring to its current precision, otherwise return
> `false`.
"""
function isgen(a::LaurentSeriesElem)
   return valuation(a) == 1 && pol_length(a) == 1 && isone(polcoeff(a, 0))
end

doc"""
    isunit(a::LaurentSeriesElem)
> Return `true` if the given power series is arithmetically equal to a unit,
> i.e. is invertible, otherwise return `false`.
"""
isunit(a::LaurentSeriesElem) = valuation(a) == 0 && isunit(polcoeff(a, 0))

doc"""
    modulus{T <: ResElem}(a::LaurentSeriesElem{T})
> Return the modulus of the coefficients of the given power series.
"""
modulus(a::LaurentSeriesElem{T}) where {T <: ResElem} = modulus(base_ring(a))

function deepcopy_internal(a::LaurentSeriesElem{T}, dict::ObjectIdDict) where {T <: RingElement}
   coeffs = Array{T}(pol_length(a))
   for i = 1:pol_length(a)
      coeffs[i] = deepcopy(polcoeff(a, i - 1))
   end
   return parent(a)(coeffs, pol_length(a), precision(a), valuation(a))
end

function renormalize!(z::LaurentSeriesElem)
   i = 0
   zlen = pol_length(z)
   zval = valuation(z)
   zprec = precision(z)
   while i < zlen && iszero(polcoeff(z, i))
      i += 1
   end
   set_prec!(z, zprec)
   if i == zlen
      set_length!(z, 0)
      set_val!(z, zprec)
   else
      set_val!(z, zval + i)
      for j = 1:zlen - i
         z = setcoeff!(z, j - 1, polcoeff(z, j + i - 1))
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

function show(io::IO, x::LaurentSeriesElem)
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

function show(io::IO, a::LaurentSeriesRing)
   print(io, "Laurent series ring in ", var(a), " over ")
   show(io, base_ring(a))
end

function show(io::IO, a::LaurentSeriesField)
   print(io, "Laurent series field in ", var(a), " over ")
   show(io, base_ring(a))
end

needs_parentheses(x::LaurentSeriesElem) = pol_length(x) > 1

isnegative(x::LaurentSeriesElem) = pol_length(x) <= 1 && isnegative(polcoeff(x, 0))

show_minus_one(::Type{LaurentSeriesElem{T}}) where {T <: RingElement} = show_minus_one(T)

###############################################################################
#
#   Unary operators
#
###############################################################################

doc"""
    -(a::LaurentSeriesElem)
> Return $-a$.
"""
function -(a::LaurentSeriesElem)
   len = pol_length(a)
   z = parent(a)()
   set_prec!(z, precision(a))
   set_val!(z, valuation(a))
   fit!(z, len)
   for i = 1:len
      z = setcoeff!(z, i - 1, -polcoeff(a, i - 1))
   end
   return z
end

###############################################################################
#
#   Binary operators
#
###############################################################################

doc"""
    +{T <: RingElement}(a::LaurentSeriesElem{T}, b::LaurentSeriesElem{T})
> Return $a + b$.
"""
function +(a::LaurentSeriesElem{T}, b::LaurentSeriesElem{T}) where {T <: RingElement}
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
         z = setcoeff!(z, i - 1, polcoeff(b, i - 1))
      end
      for i = lenb + 1:min(vala - valb, lenz)
         z = setcoeff!(z, i - 1, R())
      end
      for i = vala - valb + 1:lenb
         z = setcoeff!(z, i - 1, polcoeff(a, i - vala + valb - 1) + polcoeff(b, i - 1))
      end
      for i = max(lenb, vala - valb) + 1:lena + vala - valb
         z = setcoeff!(z, i - 1, polcoeff(a, i - vala + valb - 1))
      end
      for i = lena + vala - valb + 1:lenb
         z = setcoeff!(z, i - 1, polcoeff(b, i - 1))
      end
   else
      for i = 1:min(lena, valb - vala)
         z = setcoeff!(z, i - 1, polcoeff(a, i - 1))
      end
      for i = lena + 1:min(valb - vala, lenz)
         z = setcoeff!(z, i - 1, R())
      end
      for i = valb - vala + 1:lena
         z = setcoeff!(z, i - 1, polcoeff(a, i - 1) + polcoeff(b, i - valb + vala - 1))
      end
      for i = max(lena, valb - vala) + 1:lenb + valb - vala
         z = setcoeff!(z, i - 1, polcoeff(b, i - valb + vala - 1))
      end
      for i = lenb + valb - vala + 1:lena
         z = setcoeff!(z, i - 1, polcoeff(a, i - 1))
      end
   end
   set_length!(z, normalise(z, lenz))
   renormalize!(z)
   return z
end

doc"""
    -{T <: RingElement}(a::LaurentSeriesElem{T}, b::LaurentSeriesElem{T})
> Return $a - b$.
"""
function -(a::LaurentSeriesElem{T}, b::LaurentSeriesElem{T}) where {T <: RingElement}
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
         z = setcoeff!(z, i - 1, -polcoeff(b, i - 1))
      end
      for i = lenb + 1:min(vala - valb, lenz)
         z = setcoeff!(z, i - 1, R())
      end
      for i = vala - valb + 1:lenb
         z = setcoeff!(z, i - 1, polcoeff(a, i - vala + valb - 1) - polcoeff(b, i - 1))
      end
      for i = max(lenb, vala - valb) + 1:lena + vala - valb
         z = setcoeff!(z, i - 1, polcoeff(a, i - vala + valb - 1))
      end
      for i = lena + vala - valb + 1:lenb
         z = setcoeff!(z, i - 1, -polcoeff(b, i - 1))
      end
   else
      for i = 1:min(lena, valb - vala)
         z = setcoeff!(z, i - 1, polcoeff(a, i - 1))
      end
      for i = lena + 1:min(valb - vala, lenz)
         z = setcoeff!(z, i - 1, R())
      end
      for i = valb - vala + 1:lena
         z = setcoeff!(z, i - 1, polcoeff(a, i - 1) - polcoeff(b, i - valb + vala - 1))
      end
      for i = max(lena, valb - vala) + 1:lenb + valb - vala
         z = setcoeff!(z, i - 1, -polcoeff(b, i - valb + vala - 1))
      end
      for i = lenb + valb - vala + 1:lena
         z = setcoeff!(z, i - 1, polcoeff(a, i - 1))
      end
   end
   set_length!(z, normalise(z, lenz))
   renormalize!(z)
   return z
end

doc"""
    *{T <: RingElement}(a::LaurentSeriesElem{T}, b::LaurentSeriesElem{T})
> Return $a\times b$.
"""
function *(a::LaurentSeriesElem{T}, b::LaurentSeriesElem{T}) where {T <: RingElement}
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
            t = mul!(t, polcoeff(a, i - 1), polcoeff(b, j - 1))
            d[i + j - 1] = addeq!(d[i + j - 1], t)
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
    *{T <: RingElem}(a::T, b::LaurentSeriesElem{T})
> Return $a\times b$.
"""
function *(a::T, b::LaurentSeriesElem{T}) where {T <: RingElem}
   len = pol_length(b)
   z = parent(b)()
   fit!(z, len)
   set_prec!(z, precision(b))
   set_val!(z, valuation(b))
   for i = 1:len
      z = setcoeff!(z, i - 1, a*polcoeff(b, i - 1))
   end
   set_length!(z, normalise(z, len))
   renormalize!(z)
   return z
end

doc"""
    *(a::Union{Integer, Rational, AbstractFloat}, b::LaurentSeriesElem)
> Return $a\times b$.
"""
function *(a::Union{Integer, Rational, AbstractFloat}, b::LaurentSeriesElem)
   len = pol_length(b)
   z = parent(b)()
   fit!(z, len)
   set_prec!(z, precision(b))
   set_val!(z, valuation(b))
   for i = 1:len
      z = setcoeff!(z, i - 1, a*polcoeff(b, i - 1))
   end
   set_length!(z, normalise(z, len))
   renormalize!(z)
   return z
end

doc"""
    *{T <: RingElem}(a::LaurentSeriesElem{T}, b::T)
> Return $a\times b$.
"""
*(a::LaurentSeriesElem{T}, b::T) where {T <: RingElem} = b*a

doc"""
    *(a::LaurentSeriesElem, b::Union{Integer, Rational, AbstractFloat})
> Return $a\times b$.
"""
*(a::LaurentSeriesElem, b::Union{Integer, Rational, AbstractFloat}) = b*a

###############################################################################
#
#   Shifting
#
###############################################################################

doc"""
    shift_left(x::LaurentSeriesElem, n::Int)
> Return the power series $f$ shifted left by $n$ terms, i.e. multiplied by
> $x^n$.
"""
function shift_left(x::LaurentSeriesElem{T}, len::Int) where {T <: RingElement}
   z = deepcopy(x)
   set_prec!(z, precision(x) + len)
   set_val!(z, valuation(x) + len)
   return z
end

doc"""
    shift_right(f::LaurentSeriesElem, n::Int)
> Return the power series $f$ shifted right by $n$ terms, i.e. divided by
> $x^n$.
"""
function shift_right(x::LaurentSeriesElem{T}, len::Int) where {T <: RingElement}
   z = deepcopy(x)
   set_prec!(z, precision(x) - len)
   set_val!(z, valuation(x) - len)
   return z
end

###############################################################################
#
#   Truncation
#
###############################################################################

doc"""
    truncate(a::LaurentSeriesElem, n::Int)
> Return $a$ truncated to (absolute) precision $n$.
"""
function truncate(a::LaurentSeriesElem{T}, prec::Int) where {T <: RingElement}
   alen = pol_length(a)
   aprec = precision(a)
   aval = valuation(a)
   if aprec <= prec
      return a
   end
   z = parent(a)()
   set_prec!(z, prec)
   if prec <= aval
      set_length!(z, 0)
      set_val!(z, prec)
   else
      fit!(z, prec - aval)
      set_val!(z, aval)
      for i = 1:min(prec - aval, alen)
         z = setcoeff!(z, i - 1, polcoeff(a, i - 1))
      end
      set_length!(z, normalise(z, prec - aval))
   end
   return z
end

# Intended only for internal use, does not renormalize, assumes n >= 0
# Only efficient if valuation(a) == valuation(b) == 0
function mullow(a::LaurentSeriesElem{T}, b::LaurentSeriesElem{T}, n::Int) where {T <: RingElement}
   lena = pol_length(a)
   lenb = pol_length(b)
   if lena == 0 || lenb == 0
      return zero(parent(a))
   end
   prec = min(precision(a), precision(b))
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
            t = mul!(t, coeff(a, i - 1), coeff(b, j - 1))
            d[i + j - 1] = addeq!(d[i + j - 1], t)
         end
      end
   end
   z = parent(a)(d, lenz, prec, 0)
   set_length!(z, normalise(z, lenz))
   return z
end

###############################################################################
#
#   Powering
#
###############################################################################

doc"""
    ^{T <: RingElement}(a::LaurentSeriesElem{T}, b::Int)
> Return $a^b$. We require $b \geq 0$.
"""
function ^(a::LaurentSeriesElem{T}, b::Int) where {T <: RingElement}
   # special case powers of x for constructing power series efficiently
   if pol_length(a) == 0
      z = parent(a)()
      set_prec!(z, b*valuation(a))
      set_val!(z, b*valuation(a))
      return z
   elseif b == 0
      # in fact, the result would be exact 1 if we had exact series
      z = one(parent(a))
      return z
   elseif isgen(a)
      z = parent(a)()
      fit!(z, 1)
      set_prec!(z, b + precision(a) - 1)
      set_val!(z, b)
      z = setcoeff!(z, 0, polcoeff(a, 0))
      set_length!(z, 1)
      return z
   elseif pol_length(a) == 1
      z = parent(a)(polcoeff(a, 0)^b)
      set_prec!(z, (b - 1)*valuation(a) + precision(a))
      set_val!(z, b*valuation(a))
      return z
   elseif b == 1
      return deepcopy(a)
   elseif b == -1
      return inv(a)
   end

   if b < 0
      a = inv(a)
      b = -b
   end

   bit = ~((~UInt(0)) >> 1)
   while (UInt(bit) & b) == 0
      bit >>= 1
   end
   val = valuation(a)
   a = shift_right(a, val)
   prec = precision(a)
   z = a
   bit >>= 1
   while bit !=0
      z = mullow(z, z, prec)
      if (UInt(bit) & b) != 0
         z = mullow(z, a, prec)
      end
      bit >>= 1
   end
   set_val!(z, b*val)
   set_prec!(z, b*val + prec)
   renormalize!(z)
   return z
end

###############################################################################
#
#   Comparison
#
###############################################################################

doc"""
    =={T <: RingElement}(x::LaurentSeriesElem{T}, y::LaurentSeriesElem{T})
> Return `true` if $x == y$ arithmetically, otherwise return `false`. Recall
> that power series to different precisions may still be arithmetically
> equal to the minimum of the two precisions.
"""
function ==(x::LaurentSeriesElem{T}, y::LaurentSeriesElem{T}) where {T <: RingElement}
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
   xlen = normalise(x, min(pol_length(x), prec - xval))
   ylen = normalise(y, min(pol_length(y), prec - yval))
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
    isequal{T <: RingElement}(x::LaurentSeriesElem{T}, y::LaurentSeriesElem{T})
> Return `true` if $x == y$ exactly, otherwise return `false`. Only if the
> power series are precisely the same, to the same precision, are they declared
> equal by this function.
"""
function isequal(x::LaurentSeriesElem{T}, y::LaurentSeriesElem{T}) where {T <: RingElement}
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
    =={T <: RingElem}(x::LaurentSeriesElem{T}, y::T)
> Return `true` if $x == y$ arithmetically, otherwise return `false`.
"""
==(x::LaurentSeriesElem{T}, y::T) where {T <: RingElem} = precision(x) == 0 ||
           ((pol_length(x) == 0 && iszero(y)) || (pol_length(x) == 1 &&
             valuation(x) == 0 && polcoeff(x, 0) == y))

doc"""
    =={T <: RingElem}(x::T, y::LaurentSeriesElem{T})
> Return `true` if $x == y$ arithmetically, otherwise return `false`.
"""
==(x::T, y::LaurentSeriesElem{T}) where {T <: RingElem} = y == x

doc"""
    ==(x::LaurentSeriesElem, y::Union{Integer, Rational, AbstractFloat})
> Return `true` if $x == y$ arithmetically, otherwise return `false`.
"""
==(x::LaurentSeriesElem, y::Union{Integer, Rational, AbstractFloat}) = precision(x) == 0 ||
                  ((pol_length(x) == 0 && iszero(y)) || (pol_length(x) == 1 &&
                    valuation(x) == 0 && polcoeff(x, 0) == y))

doc"""
    ==(x::Union{Integer, Rational, AbstractFloat}, y::LaurentSeriesElem)
> Return `true` if $x == y$ arithmetically, otherwise return `false`.
"""
==(x::Union{Integer, Rational, AbstractFloat}, y::LaurentSeriesElem) = y == x

###############################################################################
#
#   Approximation
#
###############################################################################

function Base.isapprox(f::LaurentSeriesElem, g::LaurentSeriesElem; atol::Real=sqrt(eps()))
   check_parent(f, g)
   nmin = min(precision(f), precision(g))
   i = 1
   while i <= nmin
      if !isapprox(coeff(f, i - 1), coeff(g, i - 1); atol=atol)
         return false
      end
      i += 1
   end
   return true
end

###############################################################################
#
#   Exact division
#
###############################################################################

doc"""
    divexact{T <: RingElement}(a::LaurentSeriesElem{T}, b::LaurentSeriesElem{T})
> Return $a/b$. Requires $b$ to be invertible.
"""
function divexact(x::LaurentSeriesElem{T}, y::LaurentSeriesElem{T}) where {T <: RingElement}
   check_parent(x, y)
   iszero(y) && throw(DivideError())
   v2 = valuation(y)
   if v2 != 0
      x = shift_right(x, v2)
      y = shift_right(y, v2)
   end
   y = truncate(y, precision(x) - valuation(x) + valuation(y))
   return x*inv(y)
end

###############################################################################
#
#   Ad hoc exact division
#
###############################################################################

doc"""
    divexact(a::LaurentSeriesElem, b::Union{Integer, Rational, AbstractFloat})
> Return $a/b$ where the quotient is expected to be exact.
"""
function divexact(x::LaurentSeriesElem, y::Union{Integer, Rational, AbstractFloat})
   y == 0 && throw(DivideError())
   lenx = pol_length(x)
   z = parent(x)()
   fit!(z, lenx)
   set_prec!(z, precision(x))
   set_val!(z, valuation(x))
   for i = 1:lenx
      z = setcoeff!(z, i - 1, divexact(polcoeff(x, i - 1), y))
   end
   return z
end

doc"""
    divexact{T <: RingElem}(a::LaurentSeriesElem{T}, b::T)
> Return $a/b$ where the quotient is expected to be exact.
"""
function divexact(x::LaurentSeriesElem{T}, y::T) where {T <: RingElem}
   iszero(y) && throw(DivideError())
   lenx = pol_length(x)
   z = parent(x)()
   fit!(z, lenx)
   set_prec!(z, precision(x))
   set_val!(z, valuation(x))
   for i = 1:lenx
      z = setcoeff!(z, i - 1, divexact(polcoeff(x, i - 1), y))
   end
   return z
end

###############################################################################
#
#   Inversion
#
###############################################################################

doc"""
   inv(a::LaurentSeriesElem)
> Return the inverse of the power series $a$, i.e. $1/a$.
"""
function inv(a::LaurentSeriesElem)
   iszero(a) && throw(DivideError())
   a1 = polcoeff(a, 0)
   ainv = parent(a)()
   fit!(ainv, precision(a) - valuation(a))
   set_prec!(ainv, precision(a) - 2*valuation(a))
   set_val!(ainv, -valuation(a))
   !isunit(a1) && error("Unable to invert power series")
   if precision(a) - valuation(a) != 0
      ainv = setcoeff!(ainv, 0, divexact(one(base_ring(a)), a1))
   end
   a1 = -a1
   for n = 2:precision(a) - valuation(a)
      s = polcoeff(a, 1)*polcoeff(ainv, n - 2)
      for i = 2:min(n, pol_length(a)) - 1
         s += polcoeff(a, i)*polcoeff(ainv, n - i - 1)
      end
      ainv = setcoeff!(ainv, n - 1, divexact(s, a1))
   end
   set_length!(ainv, normalise(ainv, precision(a) - valuation(a)))
   return ainv
end

###############################################################################
#
#   Special functions
#
###############################################################################

doc"""
    exp(a::LaurentSeriesElem)
> Return the exponential of the power series $a$.
"""
function Base.exp(a::LaurentSeriesElem)
   if iszero(a)
      z = one(parent(a))
      set_prec!(z, precision(a))
      return z
   end
   vala = valuation(a)
   preca = precision(a)
   vala < 0 && error("Valuation must be non-negative in exp")
   z = parent(a)()
   R = base_ring(a)
   fit!(z, preca)
   set_prec!(z, preca)
   set_val!(z, 0)
   c = vala == 0 ? polcoeff(a, 0) : R()
   z = setcoeff!(z, 0, Nemo.exp(c))
   len = pol_length(a) + vala
   for k = 1 : preca - 1
      s = R()
      for j = 1 : min(k + 1, len) - 1
         c = j >= vala ? polcoeff(a, j - vala) : R()
         s += j * c * polcoeff(z, k - j)
      end
      !isunit(R(k)) && error("Unable to divide in exp")
      z = setcoeff!(z, k, divexact(s, k))
   end
   set_length!(z, normalise(z, preca))
   return z
end

###############################################################################
#
#   Unsafe functions
#
###############################################################################

function zero!(a::LaurentSeriesElem)
   a.length = 0
   a.prec = parent(a).prec_max
   return a
end

function fit!(c::LaurentSeriesElem{T}, n::Int) where {T <: RingElement}
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
   return nothing
end

function setcoeff!(c::LaurentSeriesElem{T}, n::Int, a::T) where {T <: RingElement}
   if (a != 0 && precision(c) - valuation(c) > n) || n + 1 <= c.length
      fit!(c, n + 1)
      c.coeffs[n + 1] = a
      c.length = max(pol_length(c), n + 1)
      # don't normalise
   end
   return c
end

function mul!(c::LaurentSeriesElem{T}, a::LaurentSeriesElem{T}, b::LaurentSeriesElem{T}) where {T <: RingElement}
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
         c.coeffs[i] = mul!(c.coeffs[i], polcoeff(a, i - 1), polcoeff(b, 0))
      end
      if lenc > lena
         for i = 2:min(lenb, lenc - lena + 1)
            c.coeffs[lena + i - 1] = mul!(c.coeffs[lena + i - 1], polcoeff(a, lena - 1), polcoeff(b, i - 1))
         end
      end
      for i = 1:lena - 1
         if lenc > i
            for j = 2:min(lenb, lenc - i + 1)
               t = mul!(t, polcoeff(a, i - 1), polcoeff(b, j - 1))
               c.coeffs[i + j - 1] = addeq!(c.coeffs[i + j - 1], t)
            end
         end
      end
      c.length = normalise(c, lenc)
   end
   c.val = a.val + b.val
   c.prec = prec + c.val
   renormalize!(c)
   return c
end

function addeq!(c::LaurentSeriesElem{T}, a::LaurentSeriesElem{T}) where {T <: RingElement}
   lenc = pol_length(c)
   lena = pol_length(a)
   valc = valuation(c)
   vala = valuation(a)
   valr = min(vala, valc)
   precc = precision(c)
   preca = precision(a)
   prec = min(precc, preca)
   mina = min(vala + lena, prec)
   minc = min(valc + lenc, prec)
   lenr = max(mina, minc) - valr
   R = base_ring(c)
   fit!(c, lenr)
   if valc >= vala
      for i = lenc + valc - vala:-1:max(lena, valc - vala) + 1
         t = c.coeffs[i]
         c.coeffs[i] = c.coeffs[i - valc + vala]
         c.coeffs[i - valc + vala] = t
      end
      for i = lena:-1:valc - vala + 1
         c.coeffs[i] = add!(c.coeffs[i], c.coeffs[i - valc + vala], a.coeffs[i])
      end
      for i = 1:min(lena, valc - vala)
         c.coeffs[i] = a.coeffs[i]
      end
      for i = lena + 1:min(valc - vala, lenr)
         c.coeffs[i] = R()
      end
      for i = lenc + valc - vala + 1:lena
         c.coeffs[i] = a.coeffs[i]
      end
   else
      for i = lenc + 1:min(vala - valc, lenr)
         c.coeffs[i] = R()
      end
      for i = vala - valc + 1:lenc
         c.coeffs[i] = addeq!(c.coeffs[i], a.coeffs[i - vala + valc])
      end
      for i = max(lenc, vala - valc) + 1:lena + vala - valc
         c.coeffs[i] = a.coeffs[i - vala + valc]
      end
   end
   c.length = normalise(c, lenr)
   c.prec = prec
   c.val = valr
   renormalize!(c)
   return c
end

function add!(c::LaurentSeriesElem{T}, a::LaurentSeriesElem{T}, b::LaurentSeriesElem{T}) where {T <: RingElement}
   if c === a
      return addeq!(c, b)
   elseif c === b
      return addeq!(c, a)
   end
   lena = pol_length(a)
   lenb = pol_length(b)
   valb = valuation(b)
   vala = valuation(a)
   valr = min(vala, valb)
   precb = precision(b)
   preca = precision(a)
   prec = min(precb, preca)
   mina = min(vala + lena, prec)
   minb = min(valb + lenb, prec)
   lenr = max(mina, minb) - valr
   R = base_ring(c)
   fit!(c, lenr)
   c.prec = prec
   c.val = valr
   if vala > valb
      for i = 1:min(lenb, vala - valb)
         c.coeffs[i] = b.coeffs[i]
      end
      for i = lenb + 1:vala - valb
         c.coeffs[i] = R()
      end
      for i = vala - valb + 1:lenb
         c.coeffs[i] = add!(c.coeffs[i], a.coeffs[i - vala + valb], b.coeffs[i])
      end
      for i = max(lenb, vala - valb) + 1:lena + vala - valb
         c.coeffs[i] = a.coeffs[i - vala + valb]
      end
      for i = lena + vala - valb + 1:lenb
         c.coeffs[i] = b.coeffs[i]
      end
   else
      for i = 1:min(lena, valb - vala)
         c.coeffs[i] = a.coeffs[i]
      end
      for i = lena + 1:valb - vala
         c.coeffs[i] = R()
      end
      for i = valb - vala + 1:lena
         c.coeffs[i] = add!(c.coeffs[i], a.coeffs[i], b.coeffs[i - valb + vala])
      end
      for i = max(lena, valb - vala) + 1:lenb + valb - vala
         c.coeffs[i] = b.coeffs[i - valb + vala]
      end
      for i = lenb + valb - vala + 1:lena
         c.coeffs[i] = a.coeffs[i]
      end
   end
   set_length!(c, normalise(c, lenr))
   renormalize!(c)
   return c
end

###############################################################################
#
#   Random elements
#
###############################################################################

function rand(S::LaurentSeriesRing, val_range::UnitRange{Int}, v...)
   R = base_ring(S)
   f = S()
   x = gen(S)
   for i = 0:S.prec_max - 1
      f += rand(R, v...)*x^i
   end
   return shift_left(f, rand(val_range))
end

function rand(S::LaurentSeriesField, val_range::UnitRange{Int}, v...)
   R = base_ring(S)
   f = S()
   x = gen(S)
   for i = 0:S.prec_max - 1
      f += rand(R, v...)*x^i
   end
   return shift_left(f, rand(val_range))
end

###############################################################################
#
#   Promotion rules
#
###############################################################################

promote_rule(::Type{LaurentSeriesElem{T}}, ::Type{LaurentSeriesElem{T}}) where T <: RingElement = RelSeries{T}

function promote_rule(::Type{LaurentSeriesElem{T}}, ::Type{U}) where {T <: RingElement, U <: RingElement}
   promote_rule(T, U) == T ? LaurentSeriesElem{T} : Union{}
end

###############################################################################
#
#   Parent object call overload
#
###############################################################################

function (R::LaurentSeriesRing{T})(b::RingElement) where {T <: RingElement}
   return R(base_ring(R)(b))
end

function (R::LaurentSeriesField{T})(b::RingElement) where {T <: FieldElement}
   return R(base_ring(R)(b))
end

function (R::LaurentSeriesRing{T})() where {T <: RingElement}
   z = LaurentSeriesRingElem{T}(Array{T}(0), 0, R.prec_max, R.prec_max)
   z.parent = R
   return z
end

function (R::LaurentSeriesField{T})() where {T <: FieldElement}
   z = LaurentSeriesFieldElem{T}(Array{T}(0), 0, R.prec_max, R.prec_max)
   z.parent = R
   return z
end

function (R::LaurentSeriesRing{T})(b::Union{Integer, Rational, AbstractFloat}) where {T <: RingElement}
   if b == 0
      z = LaurentSeriesRingElem{T}(Array{T}(0), 0, R.prec_max, R.prec_max)
   else
      z = LaurentSeriesRingElem{T}([base_ring(R)(b)], 1, R.prec_max, 0)
   end
   z.parent = R
   return z
end

function (R::LaurentSeriesField{T})(b::Union{Rational, AbstractFloat}) where {T <: FieldElement}
   if b == 0
      z = LaurentSeriesFieldElem{T}(Array{T}(0), 0, R.prec_max, R.prec_max)
   else
      z = LaurentSeriesFieldElem{T}([base_ring(R)(b)], 1, R.prec_max, 0)
   end
   z.parent = R
   return z
end

function (R::LaurentSeriesRing{T})(b::T) where {T <: RingElem}
   parent(b) != base_ring(R) && error("Unable to coerce to power series")
   if iszero(b)
      z = LaurentSeriesRingElem{T}(Array{T}(0), 0, R.prec_max, R.prec_max)
   else
      z = LaurentSeriesRingElem{T}([b], 1, R.prec_max, 0)
   end
   z.parent = R
   return z
end

function (R::LaurentSeriesField{T})(b::T) where {T <: FieldElem}
   parent(b) != base_ring(R) && error("Unable to coerce to power series")
   if iszero(b)
      z = LaurentSeriesFieldElem{T}(Array{T}(0), 0, R.prec_max, R.prec_max)
   else
      z = LaurentSeriesFieldElem{T}([b], 1, R.prec_max, 0)
   end
   z.parent = R
   return z
end

function (R::LaurentSeriesRing{T})(b::LaurentSeriesElem{T}) where {T <: RingElement}
   parent(b) != R && error("Unable to coerce power series")
   return b
end

function (R::LaurentSeriesField{T})(b::LaurentSeriesElem{T}) where {T <: FieldElement}
   parent(b) != R && error("Unable to coerce power series")
   return b
end

function (R::LaurentSeriesRing{T})(b::Array{T, 1}, len::Int, prec::Int, val::Int) where {T <: RingElement}
   if length(b) > 0
      parent(b[1]) != base_ring(R) && error("Unable to coerce to power series")
   end
   z = LaurentSeriesRingElem{T}(b, len, prec, val)
   z.parent = R
   return z
end

function (R::LaurentSeriesField{T})(b::Array{T, 1}, len::Int, prec::Int, val::Int) where {T <: RingElement}
   if length(b) > 0
      parent(b[1]) != base_ring(R) && error("Unable to coerce to power series")
   end
   z = LaurentSeriesFieldElem{T}(b, len, prec, val)
   z.parent = R
   return z
end

###############################################################################
#
#   PowerSeriesRing constructor
#
###############################################################################

doc"""
   PowerSeriesRing(R::Nemo.Ring, prec::Int, s::AbstractString; cached=true, model=:capped_relative)
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
function LaurentSeriesRing(R::Nemo.Ring, prec::Int, s::AbstractString; cached=true)
   S = Symbol(s)
   T = elem_type(R)

   parent_obj = LaurentSeriesRing{T}(R, prec, S, cached)

   return parent_obj, gen(parent_obj)
end

function LaurentSeriesRing(R::Nemo.Field, prec::Int, s::AbstractString; cached=true)
   S = Symbol(s)
   T = elem_type(R)

   parent_obj = LaurentSeriesField{T}(R, prec, S, cached)

   return parent_obj, gen(parent_obj)
end

function LaurentSeriesField(R::Nemo.Field, prec::Int, s::AbstractString; cached=true)
   S = Symbol(s)
   T = elem_type(R)

   parent_obj = LaurentSeriesField{T}(R, prec, S, cached)

   return parent_obj, gen(parent_obj)
end
