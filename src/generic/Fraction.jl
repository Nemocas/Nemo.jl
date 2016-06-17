###############################################################################
#
#   Fraction.jl : generic fraction fields
#
###############################################################################

export FractionField, GenFrac, GenFracField, num, den

###############################################################################
#
#   Data type and parent object methods
#
###############################################################################

parent_type{T}(::Type{GenFrac{T}}) = GenFracField{T}

elem_type{T <: RingElem}(::GenFracField{T}) = GenFrac{T}

doc"""
    base_ring{T}(S::FracField{T})
> Return the base ring $R$ of the given fraction field.
"""
base_ring{T}(a::FracField{T}) = a.base_ring::parent_type(T)

doc"""
    base_ring{T}(r::FracElem)
> Return the base ring $R$ of the fraction field that the supplied
> element $a$ belongs to.
"""
base_ring(a::FracElem) = base_ring(parent(a))

doc"""
    parent(a::FracElem)
> Return the parent object of the given fraction element.
"""
parent(a::FracElem) = a.parent

function check_parent(a::FracElem, b::FracElem)
   parent(a) != parent(b) && error("Incompatible rings in fraction field operation")
end

###############################################################################
#
#   Constructors
#
###############################################################################

function //{T <: RingElem}(x::T, y::T)
   y == 0 && throw(DivideError())
   g = gcd(x, y)
   z = GenFrac{T}(divexact(x, g), divexact(y, g))
   try
      z.parent = GenFracDict[R]
   catch
      z.parent = FractionField(parent(x))
   end
   return z
end

//{T <: RingElem}(x::T, y::Integer) = x//parent(x)(y)

//{T <: RingElem}(x::Integer, y::T) = parent(y)(x)//y

# disambiguation
//{T <: RingElem}(x::FracElem{T}, y::FracElem{T}) = divexact(x, y)

//{T <: RingElem}(x::T, y::FracElem{T}) = parent(y)(x)//y

//{T <: RingElem}(x::FracElem{T}, y::T) = x//parent(x)(y)

###############################################################################
#
#   Basic manipulation
#
###############################################################################

function Base.hash(a::FracElem, h::UInt)
   b = 0x8a30b0d963237dd5%UInt
   return b $ hash(num(a), h) $ hash(den(a), h) $ h
end

function num(a::FracElem)
   u = canonical_unit(a.den)
   return divexact(a.num, u)
end

function den(a::FracElem)
   u = canonical_unit(a.den)
   return divexact(a.den, u)
end

doc"""
    zero(R::FracField)
> Return $0/1$ in the given fraction field.
"""
zero(R::FracField) = R(0)

doc"""
    one(R::FracField)
> Return $1/1$ in the given fraction field.
"""
one(R::FracField) = R(1)

doc"""
    iszero(a::FracElem)
> Return `true` if the supplied element $a$ is zero in the fraction field it
> belongs to, otherwise return `false`.
"""
iszero(a::FracElem) = iszero(num(a))

doc"""
    isone(a::FracElem)
> Return `true` if the supplied element $a$ is one in the fraction field it
> belongs to, otherwise return `false`.
"""
isone(a::FracElem) = num(a) == den(a)

doc"""
    isunit(a::FracElem)
> Return `true` if the supplied element $a$ is invertible in the fraction field
> it belongs to, i.e. the numerator is nonzero, otherwise return `false`.
"""
isunit(a::FracElem) = num(a) != 0

function deepcopy{T <: RingElem}(a::GenFrac{T})
   v = GenFrac{T}(deepcopy(num(a)), deepcopy(den(a)))
   v.parent = parent(a)
   return v
end 

###############################################################################
#
#   Canonicalisation
#
###############################################################################

canonical_unit(a::FracElem) = a

###############################################################################
#
#   AbstractString{} I/O
#
###############################################################################

function show(io::IO, x::FracElem)
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

function show(io::IO, a::FracField)
   print(io, "Fraction field of ", base_ring(a))
end

needs_parentheses(x::FracElem) = den(x) == 1 && needs_parentheses(num(x))

is_negative(x::FracElem) = !needs_parentheses(num(x)) && is_negative(num(x))

show_minus_one{T <: RingElem}(::Type{FracElem{T}}) = show_minus_one(T)

###############################################################################
#
#   Unary operators
#
###############################################################################

doc"""
    -(a::FracElem)
> Return $-a$.
"""
function -{T <: RingElem}(a::FracElem{T})
   return parent(a)(-num(a), den(a))
end

###############################################################################
#
#   Binary operators
#
###############################################################################

doc"""
    +{T <: RingElem}(a::FracElem{T}, b::FracElem{T})
> Return $a + b$.
"""
function +{T <: RingElem}(a::FracElem{T}, b::FracElem{T})
   check_parent(a, b)
   n = num(a)*den(b) + num(b)*den(a)
   d = den(a)*den(b)
   g = gcd(n, d)
   return parent(a)(divexact(n, g), divexact(d, g))
end

doc"""
    -{T <: RingElem}(a::FracElem{T}, b::FracElem{T})
> Return $a - b$.
"""
function -{T <: RingElem}(a::FracElem{T}, b::FracElem{T})
   check_parent(a, b)
   n = num(a)*den(b) - num(b)*den(a)
   d = den(a)*den(b)
   g = gcd(n, d)
   return parent(a)(divexact(n, g), divexact(d, g))
end

doc"""
    *{T <: RingElem}(a::FracElem{T}, b::FracElem{T})
> Return $a\times b$.
"""
function *{T <: RingElem}(a::FracElem{T}, b::FracElem{T})
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
    *(a::FracElem, b::Integer)
> Return $a\times b$.
"""
function *(a::FracElem, b::Integer)
   c = base_ring(a)(b)
   g = gcd(den(a), c)
   n = num(a)*divexact(c, g)
   d = divexact(den(a), g)
   return parent(a)(n, d)
end

doc"""
    *(a::Integer, b::FracElem)
> Return $a\times b$.
"""
function *(a::Integer, b::FracElem)
   c = base_ring(b)(a)
   g = gcd(den(b), c)
   n = num(b)*divexact(c, g)
   d = divexact(den(b), g)
   return parent(b)(n, d)
end

doc"""
    *(a::FracElem, b::fmpz)
> Return $a\times b$.
"""
function *(a::FracElem, b::fmpz)
   c = base_ring(a)(b)
   g = gcd(den(a), c)
   n = num(a)*divexact(c, g)
   d = divexact(den(a), g)
   return parent(a)(n, d)
end

doc"""
    *(a::fmpz, b::FracElem)
> Return $a\times b$.
"""
function *(a::fmpz, b::FracElem)
   c = base_ring(b)(a)
   g = gcd(den(b), c)
   n = num(b)*divexact(c, g)
   d = divexact(den(b), g)
   return parent(b)(n, d)
end

doc"""
    *{T <: RingElem}(a::FracElem{T}, b::T)
> Return $a\times b$.
"""
function *{T <: RingElem}(a::FracElem{T}, b::T)
   g = gcd(den(a), b)
   n = num(a)*divexact(b, g)
   d = divexact(den(a), g)
   return parent(a)(n, d)
end

doc"""
    *{T <: RingElem}(a::T, b::FracElem{T})
> Return $a\times b$.
"""
function *{T <: RingElem}(a::T, b::FracElem{T})
   g = gcd(den(b), a)
   n = num(b)*divexact(a, g)
   d = divexact(den(b), g)
   return parent(b)(n, d)
end

doc"""
    +(a::FracElem, b::Integer)
> Return $a + b$.
"""
function +(a::FracElem, b::Integer)
   n = num(a) + den(a)*b
   d = den(a)
   g = gcd(n, d)
   return parent(a)(divexact(n, g), divexact(d, g))
end

doc"""
    +(a::FracElem, b::fmpz)
> Return $a + b$.
"""
function +(a::FracElem, b::fmpz)
   n = num(a) + den(a)*b
   d = den(a)
   g = gcd(n, d)
   return parent(a)(divexact(n, g), divexact(d, g))
end

doc"""
    -(a::FracElem, b::Integer)
> Return $a - b$.
"""
function -(a::FracElem, b::Integer)
   n = num(a) - den(a)*b
   d = den(a)
   g = gcd(n, d)
   return parent(a)(divexact(n, g), divexact(d, g))
end

doc"""
    -(a::FracElem, b::fmpz)
> Return $a - b$.
"""
function -(a::FracElem, b::fmpz)
   n = num(a) - den(a)*b
   d = den(a)
   g = gcd(n, d)
   return parent(a)(divexact(n, g), divexact(d, g))
end

doc"""
    +(a::Integer, b::FracElem)
> Return $a + b$.
"""
+(a::Integer, b::FracElem) = b + a

doc"""
    +(a::fmpz, b::FracElem)
> Return $a + b$.
"""
+(a::fmpz, b::FracElem) = b + a

doc"""
    -(a::Integer, b::FracElem)
> Return $a - b$.
"""
function -(a::Integer, b::FracElem)
   n = a*den(b) - num(b)
   d = den(b)
   g = gcd(n, d)
   return parent(b)(divexact(n, g), divexact(d, g))
end

doc"""
    -(a::fmpz, b::FracElem)
> Return $a - b$.
"""
function -(a::fmpz, b::FracElem)
   n = a*den(b) - num(b)
   d = den(b)
   g = gcd(n, d)
   return parent(b)(divexact(n, g), divexact(d, g))
end

doc"""
    +{T <: RingElem}(a::FracElem{T}, b::T)
> Return $a + b$.
"""
function +{T <: RingElem}(a::FracElem{T}, b::T)
   n = num(a) + den(a)*b
   d = den(a)
   g = gcd(n, d)
   return parent(a)(divexact(n, g), divexact(d, g))
end

doc"""
    -{T <: RingElem}(a::FracElem{T}, b::T)
> Return $a - b$.
"""
function -{T <: RingElem}(a::FracElem{T}, b::T)
   n = num(a) - den(a)*b
   d = den(a)
   g = gcd(n, d)
   return parent(a)(divexact(n, g), divexact(d, g))
end

doc"""
    +{T <: RingElem}(a::T, b::FracElem{T})
> Return $a + b$.
"""
+{T <: RingElem}(a::T, b::FracElem{T}) = b + a

doc"""
    -{T <: RingElem}(a::T, b::FracElem{T})
> Return $a - b$.
"""
function -{T <: RingElem}(a::T, b::FracElem{T})
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
    =={T <: RingElem}(x::FracElem{T}, y::FracElem{T})
> Return `true` if $x == y$ arithmetically, otherwise return `false`. Recall
> that power series to different precisions may still be arithmetically
> equal to the minimum of the two precisions.
"""
function =={T <: RingElem}(x::FracElem{T}, y::FracElem{T})
   check_parent(x, y)
   return (den(x) == den(y) && num(x) == num(y)) || (num(x)*den(y) == den(x)*num(y))
end

doc"""
    isequal{T <: RingElem}(x::FracElem{T}, y::FracElem{T})
> Return `true` if $x == y$ exactly, otherwise return `false`. This function is
> useful in cases where the numerators and denominators of the fractions are
> inexact, e.g. power series. Only if the power series are precisely the same,
> to the same precision, are they declared equal by this function.
"""
function isequal{T <: RingElem}(x::FracElem{T}, y::FracElem{T})
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
    ==(x::FracElem, y::Integer)
> Return `true` if $x == y$ arithmetically, otherwise return `false`.
"""
function ==(x::FracElem, y::Integer)
   return (den(x) == 1 && num(x) == y) || (num(x) == den(x)*y)
end

doc"""
    ==(x::Integer, y::FracElem)
> Return `true` if $x == y$ arithmetically, otherwise return `false`.
"""
==(x::Integer, y::FracElem) = y == x

doc"""
    ==(x::FracElem, y::fmpz)
> Return `true` if $x == y$ arithmetically, otherwise return `false`.
"""
function ==(x::FracElem, y::fmpz)
   return (den(x) == 1 && num(x) == y) || (num(x) == den(x)*y)
end

doc"""
    ==(x::fmpz, y::FracElem)
> Return `true` if $x == y$ arithmetically, otherwise return `false`.
"""
==(x::fmpz, y::FracElem) = y == x

doc"""
    =={T <: RingElem}(x::FracElem{T}, y::T)
> Return `true` if $x == y$ arithmetically, otherwise return `false`.
"""
function =={T <: RingElem}(x::FracElem{T}, y::T)
   return (den(x) == 1 && num(x) == y) || (num(x) == den(x)*y)
end

doc"""
    =={T <: RingElem}(x::T, y::FracElem{T})
> Return `true` if $x == y$ arithmetically, otherwise return `false`.
"""
=={T <: RingElem}(x::T, y::FracElem{T}) = y == x

###############################################################################
#
#   Inversion
#
###############################################################################

doc"""
    inv(a::FracElem)
> Return the inverse of the fraction $a$.
"""
function inv(a::FracElem)
   num(a) == 0 && throw(DivideError())
   return parent(a)(den(a), num(a))
end

###############################################################################
#
#   Exact division
#
###############################################################################

doc"""
    divexact{T <: RingElem}(a::FracElem{T}, b::FracElem{T})
> Return $a/b$.
"""
function divexact{T <: RingElem}(a::FracElem{T}, b::FracElem{T})
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
    divexact(a::FracElem, b::Integer)
> Return $a/b$.
"""
function divexact(a::FracElem, b::Integer)
   b == 0 && throw(DivideError())
   c = base_ring(a)(b)
   g = gcd(num(a), c)
   n = divexact(num(a), g)
   d = den(a)*divexact(c, g)
   return parent(a)(n, d)
end

doc"""
    divexact(a::Integer, b::FracElem)
> Return $a/b$.
"""
function divexact(a::Integer, b::FracElem)
   b == 0 && throw(DivideError())
   c = base_ring(b)(a)
   g = gcd(num(b), c)
   n = den(b)*divexact(c, g)
   d = divexact(num(b), g)
   return parent(b)(n, d)
end

doc"""
    divexact(a::FracElem, b::fmpz)
> Return $a/b$.
"""
function divexact(a::FracElem, b::fmpz)
   b == 0 && throw(DivideError())
   c = base_ring(a)(b)
   g = gcd(num(a), c)
   n = divexact(num(a), g)
   d = den(a)*divexact(c, g)
   return parent(a)(n, d)
end

doc"""
    divexact(a::fmpz, b::FracElem)
> Return $a/b$.
"""
function divexact(a::fmpz, b::FracElem)
   b == 0 && throw(DivideError())
   c = base_ring(b)(a)
   g = gcd(num(b), c)
   n = den(b)*divexact(c, g)
   d = divexact(num(b), g)
   return parent(b)(n, d)
end

# remove ambiguity
divexact{T <: RingElem}(a::FracElem{T}, b::PolyElem{T}) = error("Not supported")

# remove ambiguity
divexact{T <: RingElem}(a::PolyElem{T}, b::FracElem{T}) = error("Not supported")

doc"""
    divexact{T <: RingElem}(a::FracElem{T}, b::T)
> Return $a/b$.
"""
function divexact{T <: RingElem}(a::FracElem{T}, b::T)
   b == 0 && throw(DivideError())
   g = gcd(num(a), b)
   n = divexact(num(a), g)
   d = den(a)*divexact(b, g)
   return parent(a)(n, d)
end

doc"""
    divexact{T <: RingElem}(a::T, b::FracElem{T})
> Return $a/b$.
"""
function divexact{T <: RingElem}(a::T, b::FracElem{T})
   b == 0 && throw(DivideError())
   g = gcd(num(b), a)
   n = den(b)*divexact(a, g)
   d = divexact(num(b), g)
   return parent(b)(n, d)
end

###############################################################################
#
#   Powering
#
###############################################################################

doc"""
    ^(a::FracElem, b::Int)
> Return $a^b$.
"""
function ^{T <: RingElem}(a::FracElem{T}, b::Int)
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
    gcd{T <: RingElem}(a::FracElem{T}, b::FracElem{T})
> Return a greatest common divisor of $a$ and $b$ if one exists. N.B: we define
> the GCD of $a/b$ and $c/d$ to be gcd$(ad, bc)/bd$, reduced to lowest terms.
> This requires the existence of a greatest common divisor function for the
> base ring.
"""
function gcd{T <: RingElem}(a::FracElem{T}, b::FracElem{T})
   check_parent(a, b)
   n = gcd(num(a)*den(b), den(a)*num(b))
   d = den(a)*den(b)
   g = gcd(n, d)
   return parent(a)(divexact(n, g), divexact(d, g))
end

###############################################################################
#
#   Unsafe operators and functions
#
###############################################################################

function mul!{T <: RingElem}(c::FracElem{T}, a::FracElem{T}, b::FracElem{T})
   g1 = gcd(num(a), den(b))
   g2 = gcd(num(b), den(a))
   c.num = divexact(num(a), g1)*divexact(num(b), g2)
   c.den = divexact(den(a), g2)*divexact(den(b), g1)
end

function addeq!{T <: RingElem}(c::FracElem{T}, a::FracElem{T})
   n = c.num*den(a) + num(a)*c.den
   d = c.den*den(a)
   g = gcd(n, d)
   c.num = divexact(n, g)
   c.den = divexact(d, g)
end

###############################################################################
#
#   Promotion rules
#
###############################################################################

Base.promote_rule{T <: RingElem}(::Type{GenFrac{T}}, ::Type{T}) = GenFrac{T}

Base.promote_rule{T <: RingElem, U <: Integer}(::Type{GenFrac{T}}, ::Type{U}) = GenFrac{T}

function promote_rule1{T <: RingElem, U <: RingElem}(::Type{GenFrac{T}}, ::Type{GenFrac{U}})
   Base.promote_rule(T, GenFrac{U}) == T ? GenFrac{T} : Union{}
end

function Base.promote_rule{T <: RingElem, U <: RingElem}(::Type{GenFrac{T}}, ::Type{U})
   Base.promote_rule(T, U) == T ? GenFrac{T} : promote_rule1(U, GenFrac{T})
end

###############################################################################
#
#   Parent object call overloading
#
###############################################################################

function Base.call{T <: RingElem}(a::GenFracField{T}, b::RingElem)
   return a(base_ring(a)(b))
end

function Base.call{T <: RingElem}(a::GenFracField{T})
   z = GenFrac{T}(zero(base_ring(a)), one(base_ring(a)))
   z.parent = a
   return z
end

function Base.call{T <: RingElem}(a::GenFracField{T}, b::fmpz)
   z = GenFrac{T}(base_ring(a)(b), one(base_ring(a)))
   z.parent = a
   return z
end

function Base.call{T <: RingElem}(a::GenFracField{T}, b::T)
   parent(b) != base_ring(a) && error("Could not coerce to fraction")
   z = GenFrac{T}(b, one(base_ring(a)))
   z.parent = a
   return z
end

function Base.call{T <: RingElem}(a::GenFracField{T}, b::T, c::T)
   parent(b) != base_ring(a) && error("Could not coerce to fraction")
   parent(c) != base_ring(a) && error("Could not coerce to fraction")
   z = GenFrac{T}(b, c)
   z.parent = a
   return z
end

function Base.call{T <: RingElem}(a::GenFracField{T}, b::T, c::Integer)
   parent(b) != base_ring(a) && error("Could not coerce to fraction")
   z = GenFrac{T}(b, base_ring(a)(c))
   z.parent = a
   return z
end

function Base.call{T <: RingElem}(a::GenFracField{T}, b::Integer, c::T)
   parent(c) != base_ring(a) && error("Could not coerce to fraction")
   z = GenFrac{T}(base_ring(a)(b), c)
   z.parent = a
   return z
end

function Base.call{T <: RingElem}(a::GenFracField{T}, b::Integer)
   z = GenFrac{T}(base_ring(a)(b), one(base_ring(a)))
   z.parent = a
   return z
end

function Base.call{T <: RingElem}(a::GenFracField{T}, b::Integer, c::Integer)
   z = GenFrac{T}(base_ring(a)(b), base_ring(a)(c))
   z.parent = a
   return z
end

function Base.call{T <: RingElem}(a::GenFracField{T}, b::GenFrac{T})
   a != parent(b) && error("Could not coerce to fraction")
   return b
end

###############################################################################
#
#   FractionField constructor
#
###############################################################################

doc"""
    FractionField(R::Ring; cached=true)
> Return the parent object of the fraction field over the given base ring $R$.
> If `cached == true` (the default), the returned parent object is cached so
> that it will always be returned by a call to the constructor when the same
> base ring $R$ is supplied.
"""
function FractionField(R::Ring; cached=true)
   R2 = R
   T = elem_type(R)
   
   return GenFracField{T}(R, cached)
end

