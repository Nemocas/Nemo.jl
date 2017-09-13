###############################################################################
#
#   Fraction.jl : generic fraction fields
#
###############################################################################

export FractionField, num, den

###############################################################################
#
#   Data type and parent object methods
#
###############################################################################

parent_type(::Type{Frac{T}}) where T <: RingElem = FracField{T}

elem_type(::Type{FracField{T}}) where {T <: RingElem} = Frac{T}

doc"""
    base_ring{T}(S::Nemo.FracField{T})
> Return the base ring $R$ of the given fraction field.
"""
base_ring(a::Nemo.FracField{T}) where T <: RingElem = a.base_ring::parent_type(T)

doc"""
    base_ring{T}(r::Nemo.FracElem)
> Return the base ring $R$ of the fraction field that the supplied
> element $a$ belongs to.
"""
base_ring(a::Nemo.FracElem) = base_ring(parent(a))

doc"""
    parent(a::Nemo.FracElem)
> Return the parent object of the given fraction element.
"""
parent(a::Nemo.FracElem) = a.parent

function check_parent(a::Nemo.FracElem, b::Nemo.FracElem)
   parent(a) != parent(b) && error("Incompatible rings in fraction field operation")
end

###############################################################################
#
#   Constructors
#
###############################################################################

function //(x::T, y::T) where {T <: RingElem}
   iszero(y) && throw(DivideError())
   g = gcd(x, y)
   z = Frac{T}(divexact(x, g), divexact(y, g))
   try
      z.parent = FracDict[R]
   catch
      z.parent = FractionField(parent(x))
   end
   return z
end

//(x::T, y::Union{Integer, Rational}) where {T <: RingElem} = x//parent(x)(y)
                                          
//(x::Union{Integer, Rational}, y::T) where {T <: RingElem} = parent(y)(x)//y

//(x::T, y::Nemo.FracElem{T}) where {T <: RingElem} = parent(y)(x)//y

//(x::Nemo.FracElem{T}, y::T) where {T <: RingElem} = x//parent(x)(y)

###############################################################################
#
#   Basic manipulation
#
###############################################################################

function Base.hash(a::Nemo.FracElem, h::UInt)
   b = 0x8a30b0d963237dd5%UInt
   return xor(b, hash(num(a), h), hash(den(a), h), h)
end

function num(a::Nemo.FracElem)
   u = canonical_unit(a.den)
   return divexact(a.num, u)
end

function den(a::Nemo.FracElem)
   u = canonical_unit(a.den)
   return divexact(a.den, u)
end

doc"""
    zero(R::Nemo.FracField)
> Return $0/1$ in the given fraction field.
"""
zero(R::Nemo.FracField) = R(0)

doc"""
    one(R::Nemo.FracField)
> Return $1/1$ in the given fraction field.
"""
one(R::Nemo.FracField) = R(1)

doc"""
    iszero(a::Nemo.FracElem)
> Return `true` if the supplied element $a$ is zero in the fraction field it
> belongs to, otherwise return `false`.
"""
iszero(a::Nemo.FracElem) = iszero(num(a))

doc"""
    isone(a::Nemo.FracElem)
> Return `true` if the supplied element $a$ is one in the fraction field it
> belongs to, otherwise return `false`.
"""
isone(a::Nemo.FracElem) = num(a) == den(a)

doc"""
    isunit(a::Nemo.FracElem)
> Return `true` if the supplied element $a$ is invertible in the fraction field
> it belongs to, i.e. the numerator is nonzero, otherwise return `false`.
"""
isunit(a::Nemo.FracElem) = !iszero(num(a))

function deepcopy_internal(a::Frac{T}, dict::ObjectIdDict) where {T <: RingElem}
   v = Frac{T}(deepcopy(num(a)), deepcopy(den(a)))
   v.parent = parent(a)
   return v
end 

###############################################################################
#
#   Canonicalisation
#
###############################################################################

canonical_unit(a::Nemo.FracElem) = a

###############################################################################
#
#   AbstractString I/O
#
###############################################################################

function show(io::IO, x::Nemo.FracElem)
   u = canonical_unit(den(x))
   n = divexact(num(x), u)
   d = divexact(den(x), u);
   if d != 1 && needs_parentheses(n)
      print(io, "(")
   end
   print(io, n)
   if d != 1
      if needs_parentheses(n)
         print(io, ")")
      end
      print(io, "//")
      if needs_parentheses(d)
         print(io, "(")
      end
      print(io, d)
      if needs_parentheses(d)
         print(io, ")")
      end
   end
end

function show(io::IO, a::Nemo.FracField)
   print(io, "Fraction field of ", base_ring(a))
end

needs_parentheses(x::Nemo.FracElem) = isone(den(x)) && needs_parentheses(num(x))

isnegative(x::Nemo.FracElem) = !needs_parentheses(num(x)) && isnegative(num(x))

show_minus_one(::Type{Nemo.FracElem{T}}) where {T <: RingElem} = show_minus_one(T)

###############################################################################
#
#   Unary operators
#
###############################################################################

doc"""
    -(a::Nemo.FracElem)
> Return $-a$.
"""
function -(a::Nemo.FracElem)
   return parent(a)(-num(a), den(a))
end

###############################################################################
#
#   Binary operators
#
###############################################################################

doc"""
    +{T <: RingElem}(a::Nemo.FracElem{T}, b::Nemo.FracElem{T})
> Return $a + b$.
"""
function +(a::Nemo.FracElem{T}, b::Nemo.FracElem{T}) where {T <: RingElem}
   check_parent(a, b)
   n = num(a)*den(b) + num(b)*den(a)
   d = den(a)*den(b)
   g = gcd(n, d)
   return parent(a)(divexact(n, g), divexact(d, g))
end

doc"""
    -{T <: RingElem}(a::Nemo.FracElem{T}, b::Nemo.FracElem{T})
> Return $a - b$.
"""
function -(a::Nemo.FracElem{T}, b::Nemo.FracElem{T}) where {T <: RingElem}
   check_parent(a, b)
   n = num(a)*den(b) - num(b)*den(a)
   d = den(a)*den(b)
   g = gcd(n, d)
   return parent(a)(divexact(n, g), divexact(d, g))
end

doc"""
    *{T <: RingElem}(a::Nemo.FracElem{T}, b::Nemo.FracElem{T})
> Return $a\times b$.
"""
function *(a::Nemo.FracElem{T}, b::Nemo.FracElem{T}) where {T <: RingElem}
   check_parent(a, b)
   g1 = gcd(num(a), den(b))
   g2 = gcd(num(b), den(a))
   n = divexact(num(a), g1)*divexact(num(b), g2)
   d = divexact(den(a), g2)*divexact(den(b), g1)
   return parent(a)(n, d)
end

###############################################################################
#
#   Ad hoc binary operators
#
###############################################################################

doc"""
    *(a::Nemo.FracElem, b::Union{Integer, Rational})
> Return $a\times b$.
"""
function *(a::Nemo.FracElem, b::Union{Integer, Rational})
   c = base_ring(a)(b)
   g = gcd(den(a), c)
   n = num(a)*divexact(c, g)
   d = divexact(den(a), g)
   return parent(a)(n, d)
end

doc"""
    *(a::Union{Integer, Rational}, b::Nemo.FracElem)
> Return $a\times b$.
"""
function *(a::Union{Integer, Rational}, b::Nemo.FracElem)
   c = base_ring(b)(a)
   g = gcd(den(b), c)
   n = num(b)*divexact(c, g)
   d = divexact(den(b), g)
   return parent(b)(n, d)
end

doc"""
    *{T <: RingElem}(a::Nemo.FracElem{T}, b::T)
> Return $a\times b$.
"""
function *(a::Nemo.FracElem{T}, b::T) where {T <: RingElem}
   g = gcd(den(a), b)
   n = num(a)*divexact(b, g)
   d = divexact(den(a), g)
   return parent(a)(n, d)
end

doc"""
    *{T <: RingElem}(a::T, b::Nemo.FracElem{T})
> Return $a\times b$.
"""
function *(a::T, b::Nemo.FracElem{T}) where {T <: RingElem}
   g = gcd(den(b), a)
   n = num(b)*divexact(a, g)
   d = divexact(den(b), g)
   return parent(b)(n, d)
end

doc"""
    +(a::Nemo.FracElem, b::Union{Integer, Rational})
> Return $a + b$.
"""
function +(a::Nemo.FracElem, b::Union{Integer, Rational})
   n = num(a) + den(a)*b
   d = den(a)
   g = gcd(n, d)
   return parent(a)(divexact(n, g), divexact(d, g))
end

doc"""
    -(a::Nemo.FracElem, b::Union{Integer, Rational})
> Return $a - b$.
"""
function -(a::Nemo.FracElem, b::Union{Integer, Rational})
   n = num(a) - den(a)*b
   d = den(a)
   g = gcd(n, d)
   return parent(a)(divexact(n, g), divexact(d, g))
end

doc"""
    +(a::Union{Integer, Rational}, b::Nemo.FracElem)
> Return $a + b$.
"""
+(a::Union{Integer, Rational}, b::Nemo.FracElem) = b + a

doc"""
    -(a::Union{Integer, Rational}, b::Nemo.FracElem)
> Return $a - b$.
"""
function -(a::Union{Integer, Rational}, b::Nemo.FracElem)
   n = a*den(b) - num(b)
   d = den(b)
   g = gcd(n, d)
   return parent(b)(divexact(n, g), divexact(d, g))
end

doc"""
    +{T <: RingElem}(a::Nemo.FracElem{T}, b::T)
> Return $a + b$.
"""
function +(a::Nemo.FracElem{T}, b::T) where {T <: RingElem}
   n = num(a) + den(a)*b
   d = den(a)
   g = gcd(n, d)
   return parent(a)(divexact(n, g), divexact(d, g))
end

doc"""
    -{T <: RingElem}(a::Nemo.FracElem{T}, b::T)
> Return $a - b$.
"""
function -(a::Nemo.FracElem{T}, b::T) where {T <: RingElem}
   n = num(a) - den(a)*b
   d = den(a)
   g = gcd(n, d)
   return parent(a)(divexact(n, g), divexact(d, g))
end

doc"""
    +{T <: RingElem}(a::T, b::Nemo.FracElem{T})
> Return $a + b$.
"""
+(a::T, b::Nemo.FracElem{T}) where {T <: RingElem} = b + a

doc"""
    -{T <: RingElem}(a::T, b::Nemo.FracElem{T})
> Return $a - b$.
"""
function -(a::T, b::Nemo.FracElem{T}) where {T <: RingElem}
   n = a*den(b) - num(b)
   d = den(b)
   g = gcd(n, d)
   return parent(b)(divexact(n, g), divexact(d, g))
end

###############################################################################
#
#   Comparisons
#
###############################################################################

doc"""
    =={T <: RingElem}(x::Nemo.FracElem{T}, y::Nemo.FracElem{T})
> Return `true` if $x == y$ arithmetically, otherwise return `false`. Recall
> that power series to different precisions may still be arithmetically
> equal to the minimum of the two precisions.
"""
function ==(x::Nemo.FracElem{T}, y::Nemo.FracElem{T}) where {T <: RingElem}
   check_parent(x, y)
   return (den(x) == den(y) && num(x) == num(y)) || (num(x)*den(y) == den(x)*num(y))
end

doc"""
    isequal{T <: RingElem}(x::Nemo.FracElem{T}, y::Nemo.FracElem{T})
> Return `true` if $x == y$ exactly, otherwise return `false`. This function is
> useful in cases where the numerators and denominators of the fractions are
> inexact, e.g. power series. Only if the power series are precisely the same,
> to the same precision, are they declared equal by this function.
"""
function isequal(x::Nemo.FracElem{T}, y::Nemo.FracElem{T}) where {T <: RingElem}
   if parent(x) != parent(y)
      return false
   end
   return isequal(num(x)*den(y), den(x)*num(y))
end

###############################################################################
#
#   Ad hoc comparisons
#
###############################################################################

doc"""
    ==(x::Nemo.FracElem, y::Union{Integer, Rational})
> Return `true` if $x == y$ arithmetically, otherwise return `false`.
"""
function ==(x::Nemo.FracElem, y::Union{Integer, Rational})
   return (isone(den(x)) && num(x) == y) || (num(x) == den(x)*y)
end

doc"""
    ==(x::Union{Integer, Rational}, y::Nemo.FracElem)
> Return `true` if $x == y$ arithmetically, otherwise return `false`.
"""
==(x::Union{Integer, Rational}, y::Nemo.FracElem) = y == x

doc"""
    =={T <: RingElem}(x::Nemo.FracElem{T}, y::T)
> Return `true` if $x == y$ arithmetically, otherwise return `false`.
"""
function ==(x::Nemo.FracElem{T}, y::T) where {T <: RingElem}
   return (isone(den(x)) && num(x) == y) || (num(x) == den(x)*y)
end

doc"""
    =={T <: RingElem}(x::T, y::Nemo.FracElem{T})
> Return `true` if $x == y$ arithmetically, otherwise return `false`.
"""
==(x::T, y::Nemo.FracElem{T}) where {T <: RingElem} = y == x

###############################################################################
#
#   Inversion
#
###############################################################################

doc"""
    inv(a::Nemo.FracElem)
> Return the inverse of the fraction $a$.
"""
function inv(a::Nemo.FracElem)
   iszero(num(a)) && throw(DivideError())
   return parent(a)(den(a), num(a))
end

###############################################################################
#
#   Exact division
#
###############################################################################

doc"""
    divexact{T <: RingElem}(a::Nemo.FracElem{T}, b::Nemo.FracElem{T})
> Return $a/b$.
"""
function divexact(a::Nemo.FracElem{T}, b::Nemo.FracElem{T}) where {T <: RingElem}
   check_parent(a, b)
   g1 = gcd(num(a), num(b))
   g2 = gcd(den(b), den(a))
   n = divexact(num(a), g1)*divexact(den(b), g2)
   d = divexact(den(a), g2)*divexact(num(b), g1)
   return parent(a)(n, d)
end

###############################################################################
#
#   Ad hoc exact division
#
###############################################################################

doc"""
    divexact(a::Nemo.FracElem, b::Union{Integer, Rational})
> Return $a/b$.
"""
function divexact(a::Nemo.FracElem, b::Union{Integer, Rational})
   b == 0 && throw(DivideError())
   c = base_ring(a)(b)
   g = gcd(num(a), c)
   n = divexact(num(a), g)
   d = den(a)*divexact(c, g)
   return parent(a)(n, d)
end

doc"""
    divexact(a::Union{Integer, Rational}, b::Nemo.FracElem)
> Return $a/b$.
"""
function divexact(a::Union{Integer, Rational}, b::Nemo.FracElem)
   iszero(b) && throw(DivideError())
   c = base_ring(b)(a)
   g = gcd(num(b), c)
   n = den(b)*divexact(c, g)
   d = divexact(num(b), g)
   return parent(b)(n, d)
end

doc"""
    divexact{T <: RingElem}(a::Nemo.FracElem{T}, b::T)
> Return $a/b$.
"""
function divexact(a::Nemo.FracElem{T}, b::T) where {T <: RingElem}
   iszero(b) && throw(DivideError())
   g = gcd(num(a), b)
   n = divexact(num(a), g)
   d = den(a)*divexact(b, g)
   return parent(a)(n, d)
end

doc"""
    divexact{T <: RingElem}(a::T, b::Nemo.FracElem{T})
> Return $a/b$.
"""
function divexact(a::T, b::Nemo.FracElem{T}) where {T <: RingElem}
   iszero(b) && throw(DivideError())
   g = gcd(num(b), a)
   n = den(b)*divexact(a, g)
   d = divexact(num(b), g)
   return parent(b)(n, d)
end

function divides(a::Nemo.FracElem{T}, b::Nemo.FracElem{T}) where {T <: RingElem}
   iszero(b) && error("Division by zero in divides")
   return true, divexact(a, b)
end

###############################################################################
#
#   Powering
#
###############################################################################

doc"""
    ^(a::Nemo.FracElem, b::Int)
> Return $a^b$.
"""
function ^(a::Nemo.FracElem{T}, b::Int) where {T <: RingElem}
   if b < 0
      a = inv(a)
      b = -b
   end
   return parent(a)(num(a)^b, den(a)^b)
end

###############################################################################
#
#   GCD
#
###############################################################################

doc"""
    gcd{T <: RingElem}(a::Nemo.FracElem{T}, b::Nemo.FracElem{T})
> Return a greatest common divisor of $a$ and $b$ if one exists. N.B: we define
> the GCD of $a/b$ and $c/d$ to be gcd$(ad, bc)/bd$, reduced to lowest terms.
> This requires the existence of a greatest common divisor function for the
> base ring.
"""
function gcd(a::Nemo.FracElem{T}, b::Nemo.FracElem{T}) where {T <: RingElem}
   check_parent(a, b)
   n = gcd(num(a)*den(b), den(a)*num(b))
   d = den(a)*den(b)
   g = gcd(n, d)
   return parent(a)(divexact(n, g), divexact(d, g))
end

################################################################################
#
#   Remove and valuation
#
################################################################################

doc"""
    remove{T <: RingElem}(z::Nemo.FracElem{T}, p::T)
> Return the tuple $n, x$ such that $z = p^nx$ where $x$ has valuation $0$ at
> $p$.
"""
function remove(z::Nemo.FracElem{T}, p::T) where {T <: RingElem}
   iszero(z) && error("Not yet implemented")
   v, d = remove(den(z), p)
   w, n = remove(num(z), p)
   return w-v, n//d
end 

doc"""
    valuation{T <: RingElem}(z::Nemo.FracElem{T}, p::T)
> Return the valuation of $z$ at $p$.
"""
function valuation(z::Nemo.FracElem{T}, p::T) where {T <: RingElem}
   v, _ = remove(z, p)
   return v
end
  
###############################################################################
#
#   Unsafe operators and functions
#
###############################################################################

function zero!(c::Nemo.FracElem)
   c.num = zero!(c.num)
   if !isone(c.den)
      c.den = one(parent(c))
   end
   return c
end

function mul!(c::Nemo.FracElem{T}, a::Nemo.FracElem{T}, b::Nemo.FracElem{T}) where {T <: RingElem}
   g1 = gcd(num(a), den(b))
   g2 = gcd(num(b), den(a))
   c.num = divexact(num(a), g1)*divexact(num(b), g2)
   c.den = divexact(den(a), g2)*divexact(den(b), g1)
   return c
end

function addeq!(c::Nemo.FracElem{T}, a::Nemo.FracElem{T}) where {T <: RingElem}
   n = c.num*den(a) + num(a)*c.den
   c.den = mul!(c.den, c.den, den(a))
   g = gcd(n, c.den)
   c.num = divexact(n, g)
   c.den = divexact(c.den, g)
   return c
end

function add!(c::Nemo.FracElem{T}, a::Nemo.FracElem{T}, b::Nemo.FracElem{T}) where {T <: RingElem}
   n = c.num*den(a) + num(a)*c.den
   d = c.den*den(a)
   g = gcd(n, d)
   c.num = divexact(n, g)
   c.den = divexact(d, g)
   return c
end

function addeq!(c::Nemo.FracElem{T}, a::Nemo.FracElem{T}, b::Nemo.FracElem{T}) where {T <: RingElem}
   n = num(b)*den(a) + num(a)*den(b)
   c.den = mul!(c.den, den(b), den(a))
   g = gcd(n, d)
   c.num = divexact(n, g)
   c.den = divexact(c.den, g)
   return c
end

###############################################################################
#
#   Random functions
#
###############################################################################
   
function rand(S::Nemo.FracField{T}, v...) where {T <: RingElem}
   R = base_ring(S)
   n = rand(R, v...)
   d = R()
   while d == 0
      d = rand(R, v...)
   end
   return S(n, d)
end

###############################################################################
#
#   Promotion rules
#
###############################################################################

promote_rule(::Type{Frac{T}}, ::Type{Frac{T}}) where T <: RingElement = Frac{T}

function promote_rule(::Type{Frac{T}}, ::Type{U}) where {T <: RingElem, U <: RingElem}
   promote_rule(T, U) == T ? Frac{T} : Union{}
end

###############################################################################
#
#   Parent object call overloading
#
###############################################################################

function (a::FracField{T})(b::RingElement) where {T <: RingElement}
   return a(base_ring(a)(b))
end

function (a::FracField{T})() where {T <: RingElement}
   z = Frac{T}(zero(base_ring(a)), one(base_ring(a)))
   z.parent = a
   return z
end

function (a::FracField{T})(b::T) where {T <: RingElement}
   parent(b) != base_ring(a) && error("Could not coerce to fraction")
   z = Frac{T}(b, one(base_ring(a)))
   z.parent = a
   return z
end

function (a::FracField{T})(b::T, c::T) where {T <: RingElement}
   parent(b) != base_ring(a) && error("Could not coerce to fraction")
   parent(c) != base_ring(a) && error("Could not coerce to fraction")
   z = Frac{T}(b, c)
   z.parent = a
   return z
end

function (a::FracField{T})(b::T, c::Union{Integer, Rational}) where {T <: RingElement}
   parent(b) != base_ring(a) && error("Could not coerce to fraction")
   z = Frac{T}(b, base_ring(a)(c))
   z.parent = a
   return z
end

function (a::FracField{T})(b::Union{Integer, Rational}, c::T) where {T <: RingElement}
   parent(c) != base_ring(a) && error("Could not coerce to fraction")
   z = Frac{T}(base_ring(a)(b), c)
   z.parent = a
   return z
end

function (a::FracField{T})(b::Union{Integer, Rational}) where {T <: RingElement}
   z = Frac{T}(base_ring(a)(b), one(base_ring(a)))
   z.parent = a
   return z
end

function (a::FracField{T})(b::Integer, c::Integer) where {T <: RingElement}
   z = Frac{T}(base_ring(a)(b), base_ring(a)(c))
   z.parent = a
   return z
end

function (a::FracField{T})(b::Frac{T}) where {T <: RingElement}
   a != parent(b) && error("Could not coerce to fraction")
   return b
end

###############################################################################
#
#   FractionField constructor
#
###############################################################################

doc"""
    FractionField(R::Nemo.Ring; cached=true)
> Return the parent object of the fraction field over the given base ring $R$.
> If `cached == true` (the default), the returned parent object is cached so
> that it will always be returned by a call to the constructor when the same
> base ring $R$ is supplied.
"""
function FractionField(R::Nemo.Ring; cached=true)
   R2 = R
   T = elem_type(R)
   
   return FracField{T}(R, cached)
end

