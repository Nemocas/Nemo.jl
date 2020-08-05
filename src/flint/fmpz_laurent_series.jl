###############################################################################
#
#   fmpz_laurent_series.jl : Laurent series over Flint fmpz
#
###############################################################################

export fmpz_laurent_series, FmpzLaurentSeriesRing

###############################################################################
#
#   Data type and parent object methods
#
###############################################################################

@doc Markdown.doc"""
    O(a::fmpz_laurent_series)
> Returns $0 + O(x^\mbox{val}(a))$. Usually this function is called with $x^n$
> as parameter. Then the function returns the power series $0 + O(x^n)$, which
> can be used to set the precision of a power series when constructing it.
"""
function O(a::fmpz_laurent_series)
   val = valuation(a)
   return parent(a)(Vector{fmpz}(undef, 0), 0, val, val, 1)
end

parent_type(::Type{fmpz_laurent_series}) = FmpzLaurentSeriesRing

@doc Markdown.doc"""
    parent(a::fmpz_laurent_series)
> Return the parent of the given power series.
"""
parent(a::fmpz_laurent_series) = a.parent

elem_type(::Type{FmpzLaurentSeriesRing}) = fmpz_laurent_series

@doc Markdown.doc"""
    base_ring(R::FmpzLaurentSeriesRing)
> Return the base ring of the given power series ring.
"""
base_ring(R::FmpzLaurentSeriesRing) = R.base_ring

@doc Markdown.doc"""
    base_ring(a::fmpz_laurent_series)
> Return the base ring of the power series ring of the given power series.
"""
base_ring(a::fmpz_laurent_series) = base_ring(parent(a))

isdomain_type(::Type{fmpz_laurent_series}) = true

isexact_type(a::Type{fmpz_laurent_series}) = false

@doc Markdown.doc"""
    var(a::FmpzLaurentSeriesRing)
> Return the internal name of the generator of the power series ring. Note that
> this is returned as a `Symbol` not a `String`.
`"""
var(a::FmpzLaurentSeriesRing) = a.S

function check_parent(a::fmpz_laurent_series, b::fmpz_laurent_series)
   parent(a) != parent(b) &&
             error("Incompatible power series rings in Laurent series operation")
end

###############################################################################
#
#   Basic manipulation
#
###############################################################################

function Base.hash(a::fmpz_laurent_series, h::UInt)
   b = 0xb163af5694734274%UInt
   for i in 0:pol_length(a) - 1
      b = xor(b, hash(polcoeff(a, i), h))
      b = (b << 1) | (b >> (sizeof(Int)*8 - 1))
   end
   b = xor(b, hash(scale(a), h))
   return b
end

@doc Markdown.doc"""
    pol_length(a::fmpz_laurent_series)
> Return the length of the polynomial underlying the given power series. This
> will be zero if the power series has no nonzero terms.
"""
pol_length(a::fmpz_laurent_series) = a.length

@doc Markdown.doc"""
    precision(a::fmpz_laurent_series)
> Return the precision of the given power series in absolute terms. This will
> be the sum of the valuation and the length of the underlying polynomial.
"""
precision(a::fmpz_laurent_series) = a.prec

@doc Markdown.doc"""
    valuation(a::fmpz_laurent_series)
> Return the valuation of the given power series, i.e. the degree of the first
> nonzero term (or the precision if it is arithmetically zero).
"""
valuation(a::fmpz_laurent_series) = a.val

@doc Markdown.doc"""
    scale(a::fmpz_laurent_series)
> Return the scale factor of the polynomial underlying the given power series.
"""
scale(a::fmpz_laurent_series) = a.scale

@doc Markdown.doc"""
    max_precision(R::FmpzLaurentSeriesRing)
> Return the maximum relative precision of power series in the given power
> series ring.
"""
max_precision(R::FmpzLaurentSeriesRing) = R.prec_max

@doc Markdown.doc"""
    exp_gcd(a::fmpz_laurent_series)
> Return the GCD of the exponents of the polynomial underlying the given Laurent series.
"""
function exp_gcd(a::fmpz_laurent_series)
   n = 0
   s = scale(a)
   for i = 1:pol_length(a) - 1
      if n == 1
         return n
      end
      if polcoeff(a, i) != 0
         n = gcd(n, i)
      end
   end
   return n
end

function set_precision!(a::fmpz_laurent_series, prec::Int)
   a.prec = prec
   return a
end

function set_valuation!(a::fmpz_laurent_series, val::Int)
   a.val = val
   return a
end

function set_scale!(a::fmpz_laurent_series, scale::Int)
   a.scale = scale
   return a
end

function polcoeff(a::fmpz_laurent_series, n::Int)
   if n < 0
      return fmpz(0)
   end
   z = fmpz()
   ccall((:fmpz_poly_get_coeff_fmpz, libflint), Nothing,
         (Ref{fmpz}, Ref{fmpz_laurent_series}, Int), z, a, n)
   return z
end

function coeff(a::fmpz_laurent_series, n::Int)
   if n < valuation(a)
      return base_ring(a)()
   else
      i = n - valuation(a)
      if mod(i, scale(a)) != 0
         return base_ring(a)()
      else
         return polcoeff(a, div(i, scale(a)))
      end
   end
end

@doc Markdown.doc"""
    rescale!(a::fmpz_laurent_series)
> Rescale the polynomial underlying the series so that the GCD of its exponents is 1.
> This is only used internally, since the result of every user facing function is a
> rescaled series.
"""
function rescale!(a::fmpz_laurent_series)
   s = exp_gcd(a)
   if s > 1
      zlen = div(pol_length(a) - 1, s) + 1
      for i = 1:zlen - 1
         # TODO: use fmpz_swap
         t = polcoeff(a, i)
         a = setcoeff!(a, i, polcoeff(a, i*s))
         a = setcoeff!(a, i*s, t)
      end
      a = set_scale!(a, s*scale(a))
   elseif pol_length(a) <= 1
      a = set_scale!(a, 1)
   end
   return a
end

@doc Markdown.doc"""
    downscale(a::fmpz_laurent_series, n::Int)
> Inflate the underlying polynomial by a factor of $n$. This inserts zero coefficients
> for padding. It is assumed that the scale factor of $a$ is divisible by $n$.
"""
function downscale(a::fmpz_laurent_series, n::Int)
   n <= 0 && throw(DomainError(n, "Scale must be positive"))
   lena = pol_length(a)
   if n == 1 || lena == 0
      return a
   end
   R = base_ring(a)
   lenz = (lena - 1)*n + 1
   d = fmpz_laurent_series()
   j = 0
   pn = 0
   for i = 0:lenz - 1
      if i == pn
         setcoeff!(d, i, polcoeff(a, j))
         j += 1
         pn += n
      end
   end
   S = typeof(a)
   d = set_precision!(d, precision(a))
   d = set_valuation!(d, valuation(a))
   d = set_scale!(d, div(scale(a), n))
   d.parent = parent(a)
   return d
end

@doc Markdown.doc"""
    upscale(a::fmpz_laurent_series, n::Int)
> Deflate the underlying polynomial by a factor of $n$. This removes zero coefficients
> that were inserted for padding. It is assumed that the spacing between nonzero coefficients
> of $a$ is divisible by $n$.
"""
function upscale(a::fmpz_laurent_series, n::Int)
   n <= 0 && throw(DomainError(n, "Scale must be positive"))
   lena = pol_length(a)
   if n == 1 || lena == 0
      return a
   end
   R = base_ring(a)
   lenz = div(lena - 1, n) + 1
   d = fmpz_laurent_series()
   j = 0
   for i = 0:lenz - 1
      setcoeff!(d, i, polcoeff(a, j))
      j += n
   end
   S = typeof(a)
   d = set_precision!(d, precision(a))
   d = set_valuation!(d, valuation(a))
   d = set_scale!(d, scale(a)*n)
   d.parent = parent(a)
   return d
end

@doc Markdown.doc"""
    zero(R::FmpzLaurentSeriesRing)
> Return $0 + O(x^n)$ where $n$ is the maximum precision of the power series
> ring $R$.
"""
zero(R::FmpzLaurentSeriesRing) = R(0)

@doc Markdown.doc"""
    one(R::FmpzLaurentSeriesRing)
> Return $1 + O(x^n)$ where $n$ is the maximum precision of the power series
> ring $R$.
"""
one(R::FmpzLaurentSeriesRing) = R(1)

@doc Markdown.doc"""
    gen(R::FmpzLaurentSeriesRing)
> Return the generator of the power series ring, i.e. $x + O(x^{n + 1})$ where
> $n$ is the maximum precision of the power series ring $R$.
"""
function gen(R::FmpzLaurentSeriesRing)
   S = base_ring(R)
   return R([S(1)], 1, max_precision(R) + 1, 1, 1)
end

@doc Markdown.doc"""
    iszero(a::fmpz_laurent_series)
> Return `true` if the given power series is arithmetically equal to zero to
> its current precision, otherwise return `false`.
"""
iszero(a::fmpz_laurent_series) = pol_length(a) == 0

@doc Markdown.doc"""
    isone(a::fmpz_laurent_series)
> Return `true` if the given power series is arithmetically equal to one to
> its current precision, otherwise return `false`.
"""
function isone(a::fmpz_laurent_series)
   return valuation(a) == 0 && pol_length(a) == 1 && isone(polcoeff(a, 0))
end

@doc Markdown.doc"""
    isgen(a::fmpz_laurent_series)
> Return `true` if the given power series is arithmetically equal to the
> generator of its power series ring to its current precision, otherwise return
> `false`.
"""
function isgen(a::fmpz_laurent_series)
   return valuation(a) == 1 && pol_length(a) == 1 && isone(polcoeff(a, 0))
end

@doc Markdown.doc"""
    isunit(a::fmpz_laurent_series)
> Return `true` if the given power series is arithmetically equal to a unit,
> i.e. is invertible, otherwise return `false`.
"""
isunit(a::fmpz_laurent_series) = valuation(a) == 0 && isunit(polcoeff(a, 0))

function deepcopy_internal(a::fmpz_laurent_series, dict::IdDict)
   d = fmpz_laurent_series(a)
   d = set_precision!(d, precision(a))
   d = set_valuation!(d, valuation(a))
   d = set_scale!(d, scale(a))
   d.parent = parent(a)
   return d
end

function renormalize!(z::fmpz_laurent_series)
   i = 0
   zlen = pol_length(z)
   zval = valuation(z)
   zprec = precision(z)
   while i < zlen && iszero(polcoeff(z, i))
      i += 1
   end
   if i == zlen
      zero!(z)
      z = set_precision!(z, zprec)
      z = set_valuation!(z, zprec)
      z = set_scale!(z, 1)
   elseif i != 0
      z = set_precision!(z, zprec)
      R = base_ring(z)
      z = set_valuation!(z, zval + i*scale(z))
      for j = 1:zlen - i
         z = setcoeff!(z, j - 1, polcoeff(z, j + i - 1))
      end
      for j = zlen - i + 1:zlen
         z = setcoeff!(z, j - 1, R())
      end
   end
   return nothing
end

characteristic(::FmpzLaurentSeriesRing) = 0

###############################################################################
#
#   AbstractString I/O
#
###############################################################################

function show(io::IO, x::fmpz_laurent_series)
   len = pol_length(x)
   if len == 0
      print(io, zero(base_ring(x)))
   else
      coeff_printed = false
      sc = scale(x)
      for i = 0:len - 1
         c = polcoeff(x, i)
         bracket = needs_parentheses(c)
         if !iszero(c)
            if coeff_printed && !displayed_with_minus_in_front(c)
               print(io, "+")
            end
            if i*sc + valuation(x) != 0
               if !isone(c) && (c != -1 || show_minus_one(elem_type(base_ring(x))))
                  if bracket
                     print(io, "(")
                  end
                  print(io, c)
                  if bracket
                     print(io, ")")
                  end
                  if i*sc + valuation(x) != 0
                     print(io, "*")
                  end
               end
               if c == -1 && !show_minus_one(elem_type(base_ring(x)))
                  print(io, "-")
               end
               print(io, string(var(parent(x))))
               if i*sc + valuation(x) != 1
                  print(io, "^")
                  print(io, valuation(x) + i*sc)
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

function show(io::IO, a::FmpzLaurentSeriesRing)
   print(io, "Laurent series ring in ", var(a), " over ")
   show(io, base_ring(a))
end

needs_parentheses(x::fmpz_laurent_series) = pol_length(x) > 1

displayed_with_minus_in_front(x::fmpz_laurent_series) = pol_length(x) <= 1 && displayed_with_minus_in_front(polcoeff(x, 0))

show_minus_one(::Type{fmpz_laurent_series})  = show_minus_one(T)

###############################################################################
#
#   Unary operators
#
###############################################################################

@doc Markdown.doc"""
    -(a::fmpz_laurent_series)
> Return $-a$.
"""
function -(a::fmpz_laurent_series)
   len = pol_length(a)
   z = parent(a)()
   z = set_precision!(z, precision(a))
   z = set_valuation!(z, valuation(a))
   z = set_scale!(z, scale(a))
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

@doc Markdown.doc"""
    +(a::fmpz_laurent_series, b::fmpz_laurent_series)
> Return $a + b$.
"""
function +(a::fmpz_laurent_series, b::fmpz_laurent_series)
   check_parent(a, b)
   lena = pol_length(a)
   lenb = pol_length(b)
   vala = valuation(a)
   valb = valuation(b)
   valz = min(vala, valb)
   prec = min(precision(a), precision(b))
   sa = scale(a)
   sb = scale(b)
   if lena == 1
      sa = sb
   elseif lenb == 1
      sb = sa
   end
   sz = gcd(gcd(sa, sb), abs(vala - valb))
   mina = min(vala + lena*sa, prec)
   minb = min(valb + lenb*sb, prec)
   lenz = max(mina, minb) - valz
   lenz = div(lenz + sz - 1, sz)
   R = base_ring(a)
   z = parent(a)()
   z = set_precision!(z, prec)
   z = set_valuation!(z, valz)
   z = set_scale!(z, sz)
   pa = vala
   pb = valb
   j = 0
   k = 0
   for i = 0: lenz - 1
      pi = valz + sz*i
      if pi == pa && pi < mina
         if pi == pb && pi < minb
            z = setcoeff!(z, i, polcoeff(a, j) + polcoeff(b, k))
            pb += sb
            k += 1
         else
            z = setcoeff!(z, i, polcoeff(a, j))
         end
         j += 1
         pa += sa
      elseif pi == pb && pi < minb
         z = setcoeff!(z, i, polcoeff(b, k))
         k += 1
         pb += sb
      end
   end
   renormalize!(z)
   z = rescale!(z)
   return z
end

@doc Markdown.doc"""
    -(a::fmpz_laurent_series, b::fmpz_laurent_series)
> Return $a - b$.
"""
function -(a::fmpz_laurent_series, b::fmpz_laurent_series)
   check_parent(a, b)
   lena = pol_length(a)
   lenb = pol_length(b)
   vala = valuation(a)
   valb = valuation(b)
   valz = min(vala, valb)
   prec = min(precision(a), precision(b))
   sa = scale(a)
   sb = scale(b)
   if lena == 1
      sa = sb
   elseif lenb == 1
      sb = sa
   end
   sz = gcd(gcd(sa, sb), abs(vala - valb))
   mina = min(vala + lena*sa, prec)
   minb = min(valb + lenb*sb, prec)
   lenz = max(mina, minb) - valz
   lenz = div(lenz + sz - 1, sz)
   R = base_ring(a)
   z = parent(a)()
   z = set_precision!(z, prec)
   z = set_valuation!(z, valz)
   z = set_scale!(z, sz)
   pa = vala
   pb = valb
   j = 0
   k = 0
   for i = 0: lenz - 1
      pi = valz + sz*i
      if pi == pa && pi < mina
         if pi == pb && pi < minb
            z = setcoeff!(z, i, polcoeff(a, j) - polcoeff(b, k))
            pb += sb
            k += 1
         else
            z = setcoeff!(z, i, polcoeff(a, j))
         end
         j += 1
         pa += sa
      elseif pi == pb && pi < minb
         z = setcoeff!(z, i, -polcoeff(b, k))
         k += 1
         pb += sb
      end
   end
   renormalize!(z)
   z = rescale!(z)
   return z
end

@doc Markdown.doc"""
    *(a::fmpz_laurent_series, b::fmpz_laurent_series)
> Return $a\times b$.
"""
function *(a::fmpz_laurent_series, b::fmpz_laurent_series)
   check_parent(a, b)
   lena = pol_length(a)
   lenb = pol_length(b)
   if lena > lenb
      return b*a
   end
   aval = valuation(a)
   bval = valuation(b)
   zval = aval + bval
   prec = min(precision(a) - aval, precision(b) - bval)
   sa = scale(a)
   sb = scale(b)
   if lena == 1
      sa = sb
   elseif lenb == 1
      sb = sa
   end
   sz = gcd(sa, sb)
   lena = min(lena*sa, prec)
   lenb = min(lenb*sb, prec)
   if lena == 0 || lenb == 0
      return parent(a)(Vector{fmpz}(undef, 0), 0, prec + zval, zval, 1)
   end
   t = base_ring(a)()
   da = div(sa, sz)
   db = div(sb, sz)
   a = downscale(a, da)
   b = downscale(b, db)
   lena = pol_length(a)
   lenb = pol_length(b)
   lenz = div(prec + sz - 1, sz)
   z = parent(a)()
   ccall((:fmpz_poly_mullow, libflint), Nothing,
                (Ref{fmpz_laurent_series}, Ref{fmpz_laurent_series}, Ref{fmpz_laurent_series}, Int),
               z, a, b, lenz)
   z = set_precision!(z, prec + zval)
   z = set_valuation!(z, zval)
   z = set_scale!(z, sz)
   z = rescale!(z)
   return z
end

###############################################################################
#
#   Ad hoc binary operators
#
###############################################################################

@doc Markdown.doc"""
    *(a::fmpz, b::fmpz_laurent_series)
> Return $a\times b$.
"""
function *(a::fmpz, b::fmpz_laurent_series)
   len = pol_length(b)
   z = parent(b)()
   z = set_precision!(z, precision(b))
   z = set_valuation!(z, valuation(b))
   z = set_scale!(z, scale(b))
   for i = 1:len
      z = setcoeff!(z, i - 1, a*polcoeff(b, i - 1))
   end
   renormalize!(z)
   return z
end

@doc Markdown.doc"""
    *(a::Integer, b::fmpz_laurent_series)
> Return $a\times b$.
"""
function *(a::Integer, b::fmpz_laurent_series)
   len = pol_length(b)
   z = parent(b)()
   z = set_precision!(z, precision(b))
   z = set_valuation!(z, valuation(b))
   z = set_scale!(z, scale(b))
   for i = 1:len
      z = setcoeff!(z, i - 1, a*polcoeff(b, i - 1))
   end
   renormalize!(z)
   return z
end

@doc Markdown.doc"""
    *(a::fmpz_laurent_series, b::fmpz)
> Return $a\times b$.
"""
*(a::fmpz_laurent_series, b::fmpz) = b*a

@doc Markdown.doc"""
    *(a::fmpz_laurent_series, b::Integer)
> Return $a\times b$.
"""
*(a::fmpz_laurent_series, b::Integer) = b*a

###############################################################################
#
#   Shifting
#
###############################################################################

@doc Markdown.doc"""
    shift_left(x::fmpz_laurent_series, n::Int)
> Return the power series $x$ shifted left by $n$ terms, i.e. multiplied by
> $x^n$.
"""
function shift_left(x::fmpz_laurent_series, n::Int)
   z = deepcopy(x)
   z = set_precision!(z, precision(x) + n)
   z = set_valuation!(z, valuation(x) + n)
   return z
end

@doc Markdown.doc"""
    shift_right(x::fmpz_laurent_series, n::Int)
> Return the power series $x$ shifted right by $n$ terms, i.e. divided by
> $x^n$.
"""
function shift_right(x::fmpz_laurent_series, n::Int)
   z = deepcopy(x)
   z = set_precision!(z, precision(x) - n)
   z = set_valuation!(z, valuation(x) - n)
   return z
end

###############################################################################
#
#   Truncation
#
###############################################################################

@doc Markdown.doc"""
    truncate(a::fmpz_laurent_series, n::Int)
> Return $a$ truncated to (absolute) precision $n$.
"""
function truncate(a::fmpz_laurent_series, n::Int)
   alen = pol_length(a)
   aprec = precision(a)
   aval = valuation(a)
   if aprec <= n
      return a
   end
   z = parent(a)()
   z = set_precision!(z, n)
   if n <= aval
      z = set_valuation!(z, n)
      z = set_scale!(z, 1)
   else
      sa = scale(a)
      zlen = div(n - aval + sa - 1, sa)
      zlen = min(zlen, alen)
      z = set_valuation!(z, aval)
      for i = 0:zlen - 1
         z = setcoeff!(z, i, polcoeff(a, i))
      end
      z = set_scale!(z, sa)
      z = rescale!(z)
   end
   return z
end

# Intended only for internal use, does not renormalize or rescale, assumes n >= 0
# Requires valuation(a) == valuation(b) == 0 and scale(a) == scale(b)
function mullow(a::fmpz_laurent_series, b::fmpz_laurent_series, n::Int)
   lena = pol_length(a)
   lenb = pol_length(b)
   if lena == 0 || lenb == 0
      z = zero(parent(a))
      zprec = valuation(a) + valuation(b)
      z = set_valuation!(z, zprec)
      z = set_precision!(z, zprec)
      z = set_scale!(z, scale(a))
      return z
   end
   s = scale(a)
   prec = min(precision(a), precision(b))
   z = parent(a)()
   lenz = min(lena + lenb - 1, div(n + s - 1, s))
   ccall((:fmpz_poly_mullow, libflint), Nothing,
                (Ref{fmpz_laurent_series}, Ref{fmpz_laurent_series}, Ref{fmpz_laurent_series}, Int),
               z, a, b, lenz)
   z = set_precision!(z, prec)
   z = set_valuation!(z, valuation(a) + valuation(b))
   z = set_scale!(z, s)
   return z
end

###############################################################################
#
#   Inflation/deflation
#
###############################################################################

function inflate(a::fmpz_laurent_series, b::Int)
    d = fmpz_laurent_series(a)
    d = set_precision!(d, b*precision(a))
    d = set_valuation!(d, b*valuation(a))
    d = set_scale!(d, b*scale(a))
    d.parent = parent(a)
    return d
end

function deflate(a::fmpz_laurent_series, b::Int)
    d = fmpz_laurent_series(a)
    d = set_precision!(d, div(precision(a), b))
    d = set_valuation!(d, div(valuation(a), b))
    d = set_scale!(d, div(scale(a), b))
    d.parent = parent(a)
    return d
end

###############################################################################
#
#   Powering
#
###############################################################################

@doc Markdown.doc"""
    ^(a::fmpz_laurent_series, b::Int)
> Return $a^b$. We require $b \geq 0$.
"""
function ^(a::fmpz_laurent_series, b::Int)
   # special case powers of x for constructing power series efficiently
   if pol_length(a) == 0
      z = parent(a)()
      z = set_precision!(z, b*valuation(a))
      z = set_valuation!(z, b*valuation(a))
      z = set_scale!(z, 1)
      return z
   elseif b == 0
      # in fact, the result would be exact 1 if we had exact series
      z = one(parent(a))
      return z
   elseif isgen(a)
      z = parent(a)()
      z = set_precision!(z, b + precision(a) - 1)
      z = set_valuation!(z, b)
      z = setcoeff!(z, 0, polcoeff(a, 0))
      z = set_scale!(z, 1)
      return z
   elseif pol_length(a) == 1
      z = parent(a)(polcoeff(a, 0)^b)
      z = set_precision!(z, (b - 1)*valuation(a) + precision(a))
      z = set_valuation!(z, b*valuation(a))
      z = set_scale!(z, 1)
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
   z = set_valuation!(z, b*val)
   z = set_precision!(z, b*val + prec)
   if pol_length(z) <= 1
      z = set_scale!(z, 1)
   end
   renormalize!(z)
   z = rescale!(z)
   return z
end

###############################################################################
#
#   Comparison
#
###############################################################################

@doc Markdown.doc"""
    ==(x::fmpz_laurent_series, y::fmpz_laurent_series)
> Return `true` if $x == y$ arithmetically, otherwise return `false`. Recall
> that power series to different precisions may still be arithmetically
> equal to the minimum of the two precisions.
"""
function ==(x::fmpz_laurent_series, y::fmpz_laurent_series)
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
   sx = scale(x)
   sy = scale(y)
   xlen = min(pol_length(x), div(prec - xval + sx - 1, sx))
   ylen = min(pol_length(y), div(prec - yval + sy - 1, sy))
   i = 0
   j = 0
   while i < xlen && j < ylen
      while iszero(polcoeff(x, i)) && i < xlen
         i += 1
      end
      while iszero(polcoeff(y, j)) && j < ylen
         j += 1
      end
      if i < xlen && j < ylen
         if i*sx != j*sy || polcoeff(x, i) != polcoeff(y, j)
            return false
         end
         i += 1
         j += 1
      end
   end
   while i < xlen
      if polcoeff(x, i) != 0
         return false
      end
      i += 1
   end
   while j < ylen
      if polcoeff(y, j) != 0
         return false
      end
      j += 1
   end
   return true
end

@doc Markdown.doc"""
    isequal(x::fmpz_laurent_series, y::fmpz_laurent_series)
> Return `true` if $x == y$ exactly, otherwise return `false`. Only if the
> power series are precisely the same, to the same precision, are they declared
> equal by this function.
"""
function isequal(x::fmpz_laurent_series, y::fmpz_laurent_series)
   if parent(x) != parent(y)
      return false
   end
   if precision(x) != precision(y) || pol_length(x) != pol_length(y) ||
      valuation(x) != valuation(y) || scale(x) != scale(y)
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

@doc Markdown.doc"""
    ==(x::fmpz_laurent_series, y::T) where {T <: RingElem}
> Return `true` if $x == y$ arithmetically, otherwise return `false`.
"""
==(x::fmpz_laurent_series, y::T) where {T <: RingElem} = precision(x) == 0 ||
           ((pol_length(x) == 0 && iszero(y)) || (pol_length(x) == 1 &&
             valuation(x) == 0 && polcoeff(x, 0) == y))

@doc Markdown.doc"""
    ==(x::T, y::fmpz_laurent_series) where {T <: RingElem}
> Return `true` if $x == y$ arithmetically, otherwise return `false`.
"""
==(x::T, y::fmpz_laurent_series) where {T <: RingElem} = y == x

@doc Markdown.doc"""
    ==(x::fmpz_laurent_series, y::Union{Integer, Rational, AbstractFloat})
> Return `true` if $x == y$ arithmetically, otherwise return `false`.
"""
==(x::fmpz_laurent_series, y::Union{Integer, Rational, AbstractFloat}) = precision(x) == 0 ||
                  ((pol_length(x) == 0 && iszero(y)) || (pol_length(x) == 1 &&
                    valuation(x) == 0 && polcoeff(x, 0) == y))

@doc Markdown.doc"""
    ==(x::Union{Integer, Rational, AbstractFloat}, y::fmpz_laurent_series)
> Return `true` if $x == y$ arithmetically, otherwise return `false`.
"""
==(x::Union{Integer, Rational, AbstractFloat}, y::fmpz_laurent_series) = y == x

###############################################################################
#
#   Exact division
#
###############################################################################

@doc Markdown.doc"""
    divexact(a::fmpz_laurent_series, b::fmpz_laurent_series)
> Return $a/b$.
"""
function divexact(a::fmpz_laurent_series, b::fmpz_laurent_series)
   check_parent(a, b)
   lena = pol_length(a)
   lenb = pol_length(b)
   aval = valuation(a)
   bval = valuation(b)
   zval = aval - bval
   prec = min(precision(a) - aval, precision(b) - bval)
   sa = scale(a)
   sb = scale(b)
   if lena == 1
      sa = sb
   elseif lenb == 1
      sb = sa
   end
   sz = gcd(sa, sb)
   lena = min(lena*sa, prec)
   lenb = min(lenb*sb, prec)
   lenb == 0 && throw(DivideError())
   if lena == 0
      return parent(a)(Vector{fmpz}(undef, 0), 0, prec + zval, zval, 1)
   end
   t = base_ring(a)()
   da = div(sa, sz)
   db = div(sb, sz)
   a = downscale(a, da)
   b = downscale(b, db)
   lena = pol_length(a)
   lenb = pol_length(b)
   lenz = div(prec + sz - 1, sz)
   z = parent(a)()
   ccall((:fmpz_poly_div_series, libflint), Nothing,
                (Ref{fmpz_laurent_series}, Ref{fmpz_laurent_series}, Ref{fmpz_laurent_series}, Int),
               z, a, b, lenz)
   z = set_precision!(z, prec + zval)
   z = set_valuation!(z, zval)
   z = set_scale!(z, sz)
   z = rescale!(z)
   return z
end

###############################################################################
#
#   Ad hoc exact division
#
###############################################################################

@doc Markdown.doc"""
    divexact(x::fmpz_laurent_series, y::Union{Integer, Rational, AbstractFloat})
> Return $x/y$ where the quotient is expected to be exact.
"""
function divexact(x::fmpz_laurent_series, y::Union{Integer, Rational, AbstractFloat})
   y == 0 && throw(DivideError())
   lenx = pol_length(x)
   z = parent(x)()
   z = set_precision!(z, precision(x))
   z = set_valuation!(z, valuation(x))
   z = set_scale!(z, scale(x))
   for i = 1:lenx
      z = setcoeff!(z, i - 1, divexact(polcoeff(x, i - 1), y))
   end
   return z
end

@doc Markdown.doc"""
    divexact(x::fmpz_laurent_series, y::T) where {T <: RingElem}
> Return $x/y$ where the quotient is expected to be exact.
"""
function divexact(x::fmpz_laurent_series, y::T) where {T <: RingElem}
   iszero(y) && throw(DivideError())
   lenx = pol_length(x)
   z = parent(x)()
   z = set_precision!(z, precision(x))
   z = set_valuation!(z, valuation(x))
   z = set_scale!(z, scale(x))
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

@doc Markdown.doc"""
    inv(a::fmpz_laurent_series)
> Return the inverse of the power series $a$, i.e. $1/a$.
"""
function inv(a::fmpz_laurent_series)
   iszero(a) && throw(DivideError())
   a1 = polcoeff(a, 0)
   ainv = parent(a)()
   sa = scale(a)
   lenz = div(precision(a) - valuation(a) + sa - 1, sa)
   ainv = set_precision!(ainv, precision(a) - 2*valuation(a))
   ainv = set_valuation!(ainv, -valuation(a))
   ainv = set_scale!(ainv, sa)
   !isunit(a1) && error("Unable to invert power series")
   z = parent(a)()
   ccall((:fmpz_poly_inv_series, libflint), Nothing,
                (Ref{fmpz_laurent_series}, Ref{fmpz_laurent_series}, Int),
               ainv, a, lenz)
   ainv = rescale!(ainv)
   return ainv
end

###############################################################################
#
#   Square root
#
###############################################################################

@doc Markdown.doc"""
    sqrt(a::fmpz_laurent_series)
> Return the square root of the power series $a$.
"""
function sqrt(a::fmpz_laurent_series)
   aval = valuation(a)
   !iseven(aval) && error("Not a square in sqrt")
   R = base_ring(a)
   !isdomain_type(elem_type(R)) && error("Sqrt not implemented over non-integral domains")
   aval2 = div(aval, 2)
   prec = precision(a) - aval
   if prec == 0
      asqrt = parent(a)()
      asqrt = set_precision!(asqrt, aval2)
      asqrt = set_valuation!(asqrt, aval2)
      asqrt = set_scale!(asqrt, 1)
      return asqrt
   end
   asqrt = parent(a)()
   s = scale(a)
   zlen = div(prec + s - 1, s)
   asqrt = set_precision!(asqrt, prec + aval2)
   asqrt = set_valuation!(asqrt, aval2)
   flag = Bool(ccall((:fmpz_poly_sqrt_series, libflint), Cint,
          (Ref{fmpz_laurent_series}, Ref{fmpz_laurent_series}, Int),
                asqrt, a, zlen))
   flag == false && error("Not a square in sqrt")
   asqrt = set_scale!(asqrt, s)
   asqrt = rescale!(asqrt)
   return asqrt
end

###############################################################################
#
#   Special functions
#
###############################################################################

@doc Markdown.doc"""
    exp(a::fmpz_laurent_series)
> Return the exponential of the power series $a$.
"""
function exp(a::fmpz_laurent_series)
   if iszero(a)
      z = one(parent(a))
      z = set_precision!(z, precision(a))
      return z
   end
   vala = valuation(a)
   preca = precision(a)
   vala < 0 && error("Valuation must be non-negative in exp")
   sc = scale(a)
   gs = gcd(sc, gcd(vala, preca))
   if sc != gs
      a = downscale(a, div(sc, gs))
      sc = gs
   end
   S = parent(a)
   if sc != 1
      vala = div(vala, sc)
      preca = div(preca, sc)
      a = fmpz_laurent_series(a)
      a = set_valuation!(a, vala)
      a = set_precision!(a, preca)
      a = set_scale!(a, 1)
      a.parent = S
   end
   z = parent(a)()
   R = base_ring(a)
   z = set_precision!(z, preca)
   z = set_valuation!(z, 0)
   c = vala == 0 ? polcoeff(a, 0) : R()
   z = setcoeff!(z, 0, exp(c))
   len = pol_length(a) + vala
   for k = 1 : preca - 1
      s = R()
      for j = 1 : min(k + 1, len) - 1
         c = j >= vala ? polcoeff(a, j - vala) : R()
         s += j * c * polcoeff(z, k - j)
      end
      flag, q = divides(s, fmpz(k))
      !flag && error("Unable to divide in exp")
      z = setcoeff!(z, k, q)
   end
   z = inflate(z, sc)
   z = rescale!(z)
   return z
end

###############################################################################
#
#   Unsafe functions
#
###############################################################################

function zero!(a::fmpz_laurent_series)
  ccall((:fmpz_poly_zero, libflint), Nothing,
                   (Ref{fmpz_laurent_series},), a)
   a = set_precision!(a, parent(a).prec_max)
   a = set_valuation!(a, parent(a).prec_max)
   a = set_scale!(a, 1)
   return a
end

function setcoeff!(c::fmpz_laurent_series, n::Int, a::fmpz)
   ccall((:fmpz_poly_set_coeff_fmpz, libflint), Nothing,
                (Ref{fmpz_laurent_series}, Int, Ref{fmpz}),
               c, n, a)
   return c
end

function mul!(c::fmpz_laurent_series, a::fmpz_laurent_series, b::fmpz_laurent_series)
   lena = pol_length(a)
   lenb = pol_length(b)
   if lena > lenb
      return mul!(c, b, a)
   end
   aval = valuation(a)
   bval = valuation(b)
   prec = min(precision(a) - aval, precision(b) - bval)
   sa = scale(a)
   sb = scale(b)
   if lena == 1
      sa = sb
   elseif lenb == 1
      sb = sa
   end
   sz = gcd(sa, sb)
   lena = min(lena*sa, prec)
   lenb = min(lenb*sb, prec)
   if lena == 0 || lenb == 0
      ccall((:fmpz_poly_zero, libflint), Nothing,
                (Ref{fmpz_laurent_series},), c)
      c = set_precision!(c, prec + aval + bval)
      c = set_valuation!(c, aval + bval)
      c = set_scale!(c, 1)
      return c
   else
      t = base_ring(a)()
      da = div(sa, sz)
      db = div(sb, sz)
      a = downscale(a, da)
      b = downscale(b, db)
      lena = pol_length(a)
      lenb = pol_length(b)
      lenc = min(lena + lenb - 1, div(prec + sz - 1, sz))
      ccall((:fmpz_poly_mullow, libflint), Nothing,
         (Ref{fmpz_laurent_series}, Ref{fmpz_laurent_series}, Ref{fmpz_laurent_series}, Int), c, a, b, lenc)
   end
   c = set_valuation!(c, aval + bval)
   c = set_precision!(c, prec + c.val)
   c = set_scale!(c, sz)
   renormalize!(c)
   c = rescale!(c)
   return c
end

function addeq!(c::fmpz_laurent_series, a::fmpz_laurent_series)
   b = deepcopy(c)
   return add!(c, b, a)
end

function add!(c::fmpz_laurent_series, a::fmpz_laurent_series, b::fmpz_laurent_series)
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
   sa = scale(a)
   sb = scale(b)
   if lena == 1
      sa = sb
   elseif lenb == 1
      sb = sa
   end
   sz = gcd(gcd(sa, sb), abs(vala - valb))
   mina = min(vala + lena*sa, prec)
   minb = min(valb + lenb*sb, prec)
   lenr = max(mina, minb) - valr
   R = base_ring(c)
   c = set_precision!(c, prec)
   c = set_valuation!(c, valr)
   c = set_scale!(c, sz)
      pa = vala
   pb = valb
   j = 0
   k = 0
   t = base_ring(a)()
   zc = base_ring(a)()
   for i = 0: lenr - 1
      pi = valr + sc*i
      if pi == pa && pi < mina
         if pi == pb && pi < minb
            add!(t, polcoeff(a, j), polcoeff(b, k))
            setcoeff!(c, i, t)
            pb += sb
            k += 1
         else
            setcoeff!(c, i, polcoeff(a, j))
         end
         j += 1
         pa += sa
      elseif pi == pb && pi < minb
         setcoeff(c, i, polcoeff(b, k))
         k += 1
         pb += sb
      else
         setcoeff!(c, i, zc)
      end
   end
   renormalize!(c)
   c = rescale!(c)
   return c
end

###############################################################################
#
#   Special functions
#
###############################################################################

@doc Markdown.doc"""
    eta_qexp(x::fmpz_laurent_series)
> Return the $q$-series for the eta function, without the leading $q^{1/24}$. It is
> currently assumed that $x$ is a power of the generator of the Laurent series ring.
"""
function eta_qexp(x::fmpz_laurent_series)
   pol_length(x) != 1 || polcoeff(x, 0) != 1 && error("Series composition not implemented")
   z = parent(x)()
   zscale = valuation(x)
   prec = max_precision(parent(x))
   ccall((:fmpz_poly_eta_qexp, libflint), Nothing,
                                          (Ref{fmpz_laurent_series}, Int, Int), z, 1, div(prec + zscale - 1, zscale))
   z = set_scale!(z, zscale)
   z = set_valuation!(z, 0)
   z = set_precision!(z, prec)
   return z
end

###############################################################################
#
#   Random elements
#
###############################################################################

function rand(rng::AbstractRNG, S::FmpzLaurentSeriesRing, val_range::UnitRange{Int}, v...)
   R = base_ring(S)
   f = S()
   x = gen(S)
   for i = 0:S.prec_max - 1
      f += rand(rng, R, v...)*x^i
   end
   return shift_left(f, rand(rng, val_range))
end

###############################################################################
#
#   Promotion rules
#
###############################################################################

promote_rule(::Type{fmpz_laurent_series}, ::Type{T}) where {T <: Integer} = fmpz_laurent_series

promote_rule(::Type{fmpz_laurent_series}, ::Type{fmpz}) = fmpz_laurent_series

###############################################################################
#
#   Parent object call overload
#
###############################################################################

function (R::FmpzLaurentSeriesRing)()
   z = fmpz_laurent_series(Vector{fmpz}(undef, 0), 0, R.prec_max, R.prec_max, 1)
   z.parent = R
   return z
end

function (R::FmpzLaurentSeriesRing)(b::Integer)
   if b == 0
      z = fmpz_laurent_series(Vector{fmpz}(undef, 0), 0, R.prec_max, R.prec_max, 1)
   else
      z = fmpz_laurent_series([fmpz(b)], 1, R.prec_max, 0, 1)
   end
   z.parent = R
   return z
end

function (R::FmpzLaurentSeriesRing)(b::fmpz)
   if iszero(b)
      z = fmpz_laurent_series(Vector{fmpz}(undef, 0), 0, R.prec_max, R.prec_max, 1)
   else
      z = fmpz_laurent_series([b], 1, R.prec_max, 0, 1)
   end
   z.parent = R
   return z
end

function (R::FmpzLaurentSeriesRing)(b::fmpz_laurent_series)
   return b
end

function (R::FmpzLaurentSeriesRing)(b::Array{fmpz, 1}, len::Int, prec::Int, val::Int, scale::Int, rescale::Bool = true)
   z = fmpz_laurent_series(b, len, prec, val, scale)
   z.parent = R
   if rescale
      z = rescale!(z)
   end
   return z
end

###############################################################################
#
#   PowerSeriesRing constructor
#
###############################################################################

@doc Markdown.doc"""
    LaurentSeriesRing(R::FlintIntegerRing, prec::Int, s::AbstractString; cached=true)
> Return a tuple $(S, x)$ consisting of the parent object `S` of a Laurent series
> ring over ZZ and a generator `x` for the Laurent series ring.
> The maximum precision of the series in the ring is set to `prec`. This is taken as a
> maximum relative precision. The supplied string `s` specifies the way the
> generator of the Laurent series ring will be printed. By default, the parent
> object `S` will be cached so that supplying the same base ring, string and
> precision in future will return the same parent object and generator. If
> caching of the parent object is not required, `cached` can be set to `false`.
"""
function LaurentSeriesRing(R::FlintIntegerRing, prec::Int, s::AbstractString; cached=true)
   S = Symbol(s)

   parent_obj = FmpzLaurentSeriesRing(prec, S, cached)

   return parent_obj, gen(parent_obj)
end
