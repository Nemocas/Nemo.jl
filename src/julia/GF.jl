###############################################################################
#
#   GF.jl : Julia finite fields
#
###############################################################################

export GF

###############################################################################
#
#   Type and parent object methods
#
###############################################################################

parent_type(::Type{gfelem{T}}) where T <: Integer = GFField{T}

elem_type(::Type{GFField{T}}) where T <: Integer = gfelem{T}

doc"""
    base_ring(a::GFField)
> Returns `Union{}` as this field is not dependent on another field.
"""
base_ring(a::GFField) = Union{}

doc"""
    base_ring(a::gfelem)
> Returns `Union{}` as this field is not dependent on another field.
"""
base_ring(a::gfelem) = Union{}

doc"""
    parent(a::gfelem)
> Returns the parent of the given finite field element.
"""
parent(a::gfelem) = a.parent

isexact_type(::Type{gfelem{T}}) where T <: Integer = true

isdomain_type(::Type{gfelem{T}}) where T <: Integer = true

function check_parent(a::gfelem, b::gfelem)
   a.parent != b.parent && error("Operations on distinct finite fields not supported")
end

###############################################################################
#
#   Basic manipulation
#
###############################################################################

function Base.hash(a::gfelem, h::UInt)
   b = 0xe08f2b4ea1cd2a13%UInt
   return xor(xor(hash(a.d), h), b)
end

doc"""
    zero{T <: Integer}(a::GFField{T})
> Return the additive identity, zero, in the given finite field.
"""
function zero(R::GFField{T}) where T <: Integer
   return gfelem{T}(T(0), R)
end

doc"""
    one{T <: Integer}(a::GFField{T})
> Return the additive identity, zero, in the given finite field.
"""
function one(R::GFField{T}) where T <: Integer
      return gfelem{T}(T(1), R)
end

doc"""
    iszero{T <: Integer}(a::gfelem{T})
> Returns true if the given element of the finite field is zero.
"""
iszero(a::gfelem{T}) where T <: Integer = a.d == 0

doc"""
    isone{T <: Integer}(a::gfelem{T})
> Returns true if the given element of the finite field is one.
"""
isone(a::gfelem{T}) where T <: Integer = a.d == 1

doc"""
    isunit(a::gfelem)
> Return `true` if the given finite field element is invertible, i.e. nonzero,
> otherwise return `false`.
"""
isunit(a::gfelem) = a.d != 0

doc"""
    characteristic(R::GFField)
> Return the characteristic of the given finite field.
"""
function characteristic(R::GFField)
   return R.p
end

doc"""
    order(R::GFField)
> Return the order, i.e. the number of element in, the given finite field.
"""
function order(R::GFField)
   return R.p
end

doc"""
    degree(R::GFField)
> Return the degree of the given finite field.
"""
function degree(R::GFField)
   return 1
end

function deepcopy_internal(a::gfelem{T}, dict::ObjectIdDict) where T <: Integer
   R = parent(a)
   return gfelem{T}(deepcopy(a.d), R)
end

###############################################################################
#
#   Canonicalisation
#
###############################################################################

canonical_unit(x::gfelem) = x

###############################################################################
#
#   AbstractString I/O
#
###############################################################################

function show(io::IO, x::gfelem)
   print(io, x.d)
end

function show(io::IO, R::GFField)
   print(io, "Finite field F_", R.p)
end

needs_parentheses(x::gfelem) = false

isnegative(x::gfelem) = false

show_minus_one(::Type{gfelem{T}}) where T <: Integer = true

###############################################################################
#
#   Unary operations
#
###############################################################################

function -(x::gfelem{T}) where T <: Integer
   if x.d == 0
      return deepcopy(x)
   else
      R = parent(x)
      return gfelem{T}(R.p - x.d, R)
   end
end

###############################################################################
#
#   Binary operations
#
###############################################################################

function +(x::gfelem{T}, y::gfelem{T}) where T <: Integer
   check_parent(x, y)
   R = parent(x)
   p = characteristic(R)::T
   d = x.d + y.d - p
   if d < 0
      return gfelem{T}(d + p, R)
   else
      return gfelem{T}(d, R)
   end
end

function -(x::gfelem{T}, y::gfelem{T}) where T <: Integer
   check_parent(x, y)
   R = parent(x)
   p = characteristic(R)::T
   d = x.d - y.d
   if d < 0
      return gfelem{T}(d + p, R)
   else
      return gfelem{T}(d, R)
   end
end

function *(x::gfelem{T}, y::gfelem{T}) where T <: Integer
   check_parent(x, y)
   R = parent(x)
   return R(widen(x.d)*widen(y.d))
end

###############################################################################
#
#   Ad hoc binary operators
#
###############################################################################

function *(x::Integer, y::gfelem{T}) where T <: Integer
   R = parent(y)
   return R(widen(x)*widen(y.d))
end

*(x::gfelem{T}, y::Integer) where T <: Integer = y*x

###############################################################################
#
#   Powering
#
###############################################################################

function ^(x::gfelem{T}, y::Integer) where T <: Integer
   R = parent(x)
   p = R.p::T
   if x.d == 0
      return deepcopy(x)
   end
   if y < 0
      x = inv(x)
      y = -y
   end
   if y >= p - 1
      y1 = y%(p - 1)
      y = convert(T, y1)::T
   else
      y = convert(T, y)::T
   end
   if y == 0
      return one(R)
   elseif y == 1
      return x
   end
   bit = T(1) << (ndigits(y, 2) - 1)
   z = x
   bit >>= 1
   while bit != 0
      z = z*z
      if (bit & y) != 0
         z *= x
      end
      bit >>= 1
   end
   return z
end

###############################################################################
#
#   Comparison
#
###############################################################################

function ==(x::gfelem{T}, y::gfelem{T}) where T <: Integer
   check_parent(x, y)
   return x.d == y.d
end

###############################################################################
#
#   Inversion
#
###############################################################################

function inv(x::gfelem{T}) where T <: Integer
   x == 0 && throw(DivideError())
   R = parent(x)
   p = R.p::T
   g, s, t = gcdx(x.d, p)
   g != 1 && error("Characteristic not prime in ", R)
   return R(s)
end

###############################################################################
#
#   Exact division
#
###############################################################################

function divexact(x::gfelem{T}, y::gfelem{T}) where T <: Integer
   check_parent(x, y)
   return x*inv(y)
end

divides(a::gfelem{T}, b::gfelem{T}) where T <: Integer = true, divexact(a, b)

###############################################################################
#
#   Unsafe functions
#
###############################################################################

function zero!(z::gfelem{T}) where T <: Integer
   R = parent(z)
   d = zero!(z.d)
   return gfelem{T}(d, R)
end

function mul!(z::gfelem{T}, x::gfelem{T}, y::gfelem{T}) where T <: Integer
   return x*y
end

function mul!(z::gfelem{BigInt}, x::gfelem{BigInt}, y::gfelem{BigInt})
   R = parent(x)
   p = R.p::BigInt
   d = mul!(z.d, x.d, y.d)
   if d >= p
      return gfelem{BigInt}(d%p, R)
   else
      return gfelem{BigInt}(d, R)
   end
end

function addeq!(z::gfelem{T}, x::gfelem{T}) where T <: Integer
   R = parent(x)
   p = R.p::T
   d = addeq!(z.d, x.d)
   if d < p
      return gfelem{T}(d, R)
   else
      return gfelem{T}(d - p, R)
   end
end

function add!(z::gfelem{T}, x::gfelem{T}, y::gfelem{T}) where T <: Integer
   R = parent(x)
   p = R.p::T
   d = add!(z.d, x.d, y.d)
   if d < p
      return gfelem{T}(d, R)
   else
      return gfelem{T}(d - p, R)
   end
end

###############################################################################
#
#   Random functions
#
###############################################################################

function rand(R::GFField{T}) where T <: Integer
   p = R.p::T
   d = rand(0:p - 1)
   return gfelem{T}(d, R)
end

###############################################################################
#
#   Promotions
#
###############################################################################

promote_rule(::Type{gfelem{S}}, ::Type{T}) where {S <: Integer, T <: Integer} = gfelem{S}

###############################################################################
#
#   Parent object call overload
#
###############################################################################

function (R::GFField{T})() where T <: Integer
   return gfelem{T}(T(0), R)
end

function (R::GFField{T})(a::Integer) where T <: Integer
   p = R.p::T
   d = convert(T, a%p)::T
   if d < 0
      d += p
   end
   return gfelem{T}(d, R)
end

function (R::GFField{T})(a::gfelem{T}) where T <: Integer
   parent(a) != R && error("Coercion between finite fields not implemented")
   return a
end

###############################################################################
#
#   GF(p) constructor
#
###############################################################################

function GF(p::T) where T <: Integer
   p <= 0 && error("Characteristic is not prime in GF(p)")
   return GFField{T}(p)
end
