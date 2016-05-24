###############################################################################
#
#   Fraction.jl : generic fraction fields
#
###############################################################################

export FractionField, GenFraction, GenFractionField, num, den

###############################################################################
#
#   Data type and parent object methods
#
###############################################################################

parent_type{T}(::Type{GenFraction{T}}) = GenFractionField{T}

elem_type{T <: RingElem}(::GenFractionField{T}) = GenFraction{T}

base_ring{T}(a::FracField{T}) = a.base_ring::parent_type(T)

base_ring(a::FracElem) = base_ring(parent(a))

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
   z = GenFraction{T}(divexact(x, g), divexact(y, g))
   try
      z.parent = GenFractionDict[R]
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

zero(R::FracField) = R(0)

one(R::FracField) = R(1)

iszero(a::FracElem) = iszero(num(a))

isone(a::FracElem) = num(a) == den(a)

isunit(a::FracElem) = num(a) != 0

function deepcopy{T <: RingElem}(a::FracElem{T})
   v = GenFraction{T}(deepcopy(num(a)), deepcopy(den(a)))
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

function -{T <: RingElem}(a::FracElem{T})
   return parent(a)(-num(a), den(a))
end

###############################################################################
#
#   Binary operators
#
###############################################################################

function +{T <: RingElem}(a::FracElem{T}, b::FracElem{T})
   check_parent(a, b)
   n = num(a)*den(b) + num(b)*den(a)
   d = den(a)*den(b)
   g = gcd(n, d)
   return parent(a)(divexact(n, g), divexact(d, g))
end

function -{T <: RingElem}(a::FracElem{T}, b::FracElem{T})
   check_parent(a, b)
   n = num(a)*den(b) - num(b)*den(a)
   d = den(a)*den(b)
   g = gcd(n, d)
   return parent(a)(divexact(n, g), divexact(d, g))
end

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

function *{T <: RingElem}(a::FracElem{T}, b::Integer)
   c = base_ring(a)(b)
   g = gcd(den(a), c)
   n = num(a)*divexact(c, g)
   d = divexact(den(a), g)
   return parent(a)(n, d)
end

function *{T <: RingElem}(a::Integer, b::FracElem{T})
   c = base_ring(b)(a)
   g = gcd(den(b), c)
   n = num(b)*divexact(c, g)
   d = divexact(den(b), g)
   return parent(b)(n, d)
end

function *{T <: RingElem}(a::FracElem{T}, b::T)
   g = gcd(den(a), b)
   n = num(a)*divexact(b, g)
   d = divexact(den(a), g)
   return parent(a)(n, d)
end

function *{T <: RingElem}(a::T, b::FracElem{T})
   g = gcd(den(b), a)
   n = num(b)*divexact(a, g)
   d = divexact(den(b), g)
   return parent(b)(n, d)
end

function +{T <: RingElem}(a::FracElem{T}, b::Integer)
   n = num(a) + den(a)*b
   d = den(a)
   g = gcd(n, d)
   return parent(a)(divexact(n, g), divexact(d, g))
end

function -{T <: RingElem}(a::FracElem{T}, b::Integer)
   n = num(a) - den(a)*b
   d = den(a)
   g = gcd(n, d)
   return parent(a)(divexact(n, g), divexact(d, g))
end

+{T <: RingElem}(a::Integer, b::FracElem{T}) = b + a

function -{T <: RingElem}(a::Integer, b::FracElem{T})
   n = a*den(b) - num(b)
   d = den(b)
   g = gcd(n, d)
   return parent(b)(divexact(n, g), divexact(d, g))
end

function +{T <: RingElem}(a::FracElem{T}, b::T)
   n = num(a) + den(a)*b
   d = den(a)
   g = gcd(n, d)
   return parent(a)(divexact(n, g), divexact(d, g))
end

function -{T <: RingElem}(a::FracElem{T}, b::T)
   n = num(a) - den(a)*b
   d = den(a)
   g = gcd(n, d)
   return parent(a)(divexact(n, g), divexact(d, g))
end

+{T <: RingElem}(a::T, b::FracElem{T}) = b + a

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

function =={T <: RingElem}(x::FracElem{T}, y::FracElem{T})
   check_parent(x, y)
   return (den(x) == den(y) && num(x) == num(y)) || (num(x)*den(y) == den(x)*num(y))
end

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

function ==(x::FracElem, y::Integer)
   return (den(x) == 1 && num(x) == y) || (num(x) == den(x)*y)
end

==(x::Integer, y::FracElem) = y == x

function =={T <: RingElem}(x::FracElem{T}, y::T)
   return (den(x) == 1 && num(x) == y) || (num(x) == den(x)*y)
end

=={T <: RingElem}(x::T, y::FracElem{T}) = y == x

###############################################################################
#
#   Inversion
#
###############################################################################

function inv{T <: RingElem}(a::FracElem{T})
   num(a) == 0 && throw(DivideError())
   return parent(a)(den(a), num(a))
end

###############################################################################
#
#   Exact division
#
###############################################################################

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

function divexact{T <: RingElem}(a::FracElem{T}, b::Integer)
   b == 0 && throw(DivideError())
   c = base_ring(a)(b)
   g = gcd(num(a), c)
   n = divexact(num(a), g)
   d = den(a)*divexact(c, g)
   return parent(a)(n, d)
end

function divexact{T <: RingElem}(a::Integer, b::FracElem{T})
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

function divexact{T <: RingElem}(a::FracElem{T}, b::T)
   b == 0 && throw(DivideError())
   g = gcd(num(a), b)
   n = divexact(num(a), g)
   d = den(a)*divexact(b, g)
   return parent(a)(n, d)
end

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

Base.promote_rule{T <: RingElem}(::Type{GenFraction{T}}, ::Type{T}) = GenFraction{T}

Base.promote_rule{T <: RingElem, U <: Integer}(::Type{GenFraction{T}}, ::Type{U}) = GenFraction{T}

function promote_rule1{T <: RingElem, U <: RingElem}(::Type{GenFraction{T}}, ::Type{GenFraction{U}})
   Base.promote_rule(T, GenFraction{U}) == T ? GenFraction{T} : Union{}
end

function Base.promote_rule{T <: RingElem, U <: RingElem}(::Type{GenFraction{T}}, ::Type{U})
   Base.promote_rule(T, U) == T ? GenFraction{T} : promote_rule1(U, GenFraction{T})
end

###############################################################################
#
#   Parent object call overloading
#
###############################################################################

function Base.call{T <: RingElem}(a::GenFractionField{T}, b::RingElem)
   return a(base_ring(a)(b))
end

function Base.call{T <: RingElem}(a::GenFractionField{T})
   z = GenFraction{T}(zero(base_ring(a)), one(base_ring(a)))
   z.parent = a
   return z
end

function Base.call{T <: RingElem}(a::GenFractionField{T}, b::T)
   parent(b) != base_ring(a) && error("Could not coerce to fraction")
   z = GenFraction{T}(b, one(base_ring(a)))
   z.parent = a
   return z
end

function Base.call{T <: RingElem}(a::GenFractionField{T}, b::T, c::T)
   parent(b) != base_ring(a) && error("Could not coerce to fraction")
   parent(c) != base_ring(a) && error("Could not coerce to fraction")
   z = GenFraction{T}(b, c)
   z.parent = a
   return z
end

function Base.call{T <: RingElem}(a::GenFractionField{T}, b::T, c::Integer)
   parent(b) != base_ring(a) && error("Could not coerce to fraction")
   z = GenFraction{T}(b, base_ring(a)(c))
   z.parent = a
   return z
end

function Base.call{T <: RingElem}(a::GenFractionField{T}, b::Integer, c::T)
   parent(c) != base_ring(a) && error("Could not coerce to fraction")
   z = GenFraction{T}(base_ring(a)(b), c)
   z.parent = a
   return z
end

function Base.call{T <: RingElem}(a::GenFractionField{T}, b::Integer)
   z = GenFraction{T}(base_ring(a)(b), one(base_ring(a)))
   z.parent = a
   return z
end

function Base.call{T <: RingElem}(a::GenFractionField{T}, b::Integer, c::Integer)
   z = GenFraction{T}(base_ring(a)(b), base_ring(a)(c))
   z.parent = a
   return z
end

function Base.call{T <: RingElem}(a::GenFractionField{T}, b::GenFraction{T})
   a != parent(b) && error("Could not coerce to fraction")
   return b
end

###############################################################################
#
#   FractionField constructor
#
###############################################################################

function FractionField(R::Ring; cached=true)
   R2 = R
   T = elem_type(R)
   
   return GenFractionField{T}(R, cached)
end

