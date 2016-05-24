###############################################################################
#
#   Residue.jl : generic residue rings (modulo a principal ideal)
#
###############################################################################

export ResidueRing, GenRes, GenResRing, inv, modulus, data

###############################################################################
#
#   Data type and parent object methods
#
###############################################################################

parent_type{T}(::Type{GenRes{T}}) = GenResRing{T}

elem_type{T <: RingElem}(::GenResRing{T}) = GenRes{T}

base_ring{T}(a::ResRing{T}) = a.base_ring::parent_type(T)

base_ring(a::ResElem) = base_ring(parent(a))

parent(a::ResElem) = a.parent

function check_parent_type{T <: RingElem}(a::ResRing{T}, b::ResRing{T})
   # exists only to check types of parents agree
end
   
function check_parent(a::ResElem, b::ResElem)
   if parent(a) != parent(b)
      check_parent_type(parent(a), parent(b))
      a.hash != b.hash && error("Incompatible moduli in residue operation")
   end
end

###############################################################################
#
#   Basic manipulation
#
###############################################################################

function Base.hash(a::ResElem, h::UInt)
   b = 0x539c1c8715c1adc2%UInt
   return b $ hash(data(a), h) $ h
end

function modulus(R::ResRing)
   return R.modulus
end

function modulus(a::ResElem)
   return modulus(parent(a))
end

data(a::ResElem) = a.data

zero(R::ResRing) = R(0)

one(R::ResRing) = R(1)

iszero(a::ResElem) = iszero(data(a))

isone(a::ResElem) = isone(data(a))

function isunit(a::ResElem)
   g, ainv = gcdinv(data(a), modulus(a))
   return g == 1
end

deepcopy(a::ResElem) = parent(a)(deepcopy(data(a)))

###############################################################################
#
#   Canonicalisation
#
###############################################################################

canonical_unit(a::ResElem) = a

###############################################################################
#
#   AbstractString{} I/O
#
###############################################################################

function show(io::IO, x::ResElem)
   print(io, data(x))
end

function show(io::IO, a::ResRing)
   print(io, "Residue ring of ", base_ring(a), " modulo ", modulus(a))
end

needs_parentheses(x::ResElem) = needs_parentheses(data(x))

is_negative(x::ResElem) = is_negative(data(x))

show_minus_one{T <: RingElem}(::Type{ResElem{T}}) = true

###############################################################################
#
#   Unary operations
#
###############################################################################

function -{T <: RingElem}(a::ResElem{T})
   parent(a)(-data(a))
end

###############################################################################
#
#   Binary operators
#
###############################################################################

function +{T <: RingElem}(a::ResElem{T}, b::ResElem{T})
   check_parent(a, b)
   return parent(a)(data(a) + data(b))
end

function -{T <: RingElem}(a::ResElem{T}, b::ResElem{T})
   check_parent(a, b)
   return parent(a)(data(a) - data(b))
end

function *{T <: RingElem}(a::ResElem{T}, b::ResElem{T})
   check_parent(a, b)
   return parent(a)(data(a) * data(b))
end

###############################################################################
#
#   Ad hoc binary operations
#
###############################################################################

*{T <: RingElem}(a::ResElem{T}, b::Integer) = parent(a)(data(a) * b)

*{T <: RingElem}(a::Integer, b::ResElem{T}) = parent(b)(a * data(b))

+{T <: RingElem}(a::ResElem{T}, b::Integer) = parent(a)(data(a) + b)

+{T <: RingElem}(a::Integer, b::ResElem{T}) = parent(b)(a + data(b))

-{T <: RingElem}(a::ResElem{T}, b::Integer) = parent(a)(data(a) - b)

-{T <: RingElem}(a::Integer, b::ResElem{T}) = parent(b)(a - data(b))

###############################################################################
#
#   Powering
#
###############################################################################

function ^{T <: RingElem}(a::ResElem{T}, b::Int)
   parent(a)(powmod(data(a), b, modulus(a)))
end

###############################################################################
#
#   Comparison
#
###############################################################################

function =={T <: RingElem}(a::ResElem{T}, b::ResElem{T})
   check_parent(a, b)
   return data(a) == data(b)
end

###############################################################################
#
#   Ad hoc comparison
#
###############################################################################

function =={T <: RingElem}(a::ResElem{T}, b::Integer)
   z = base_ring(a)(b)
   return data(a) == mod(z, modulus(a))
end

function =={T <: RingElem}(a::Integer, b::ResElem{T})
   z = base_ring(b)(a)
   return data(b) == mod(z, modulus(b))
end

###############################################################################
#
#   Inversion
#
###############################################################################

function inv{T <: RingElem}(a::ResElem{T})
   g, ainv = gcdinv(data(a), modulus(a))
   if g != 1
      error("Impossible inverse in inv")
   end
   return parent(a)(ainv)
end

###############################################################################
#
#   Exact division
#
###############################################################################

function divexact{T <: RingElem}(a::ResElem{T}, b::ResElem{T})
   check_parent(a, b)
   g, binv = gcdinv(data(b), modulus(b))
   if g != 1
      error("Impossible inverse in divexact")
   end
   return parent(a)(data(a) * binv)
end

###############################################################################
#
#   GCD
#
###############################################################################

function gcd{T <: RingElem}(a::ResElem{T}, b::ResElem{T})
   check_parent(a, b)
   return parent(a)(gcd(gcd(data(a), modulus(a)), data(b)))
end

###############################################################################
#
#   Unsafe functions
#
###############################################################################

function mul!{T <: RingElem}(c::ResElem{T}, a::ResElem{T}, b::ResElem{T})
   c.data = mod(data(a)*data(b), modulus(a))
end

function addeq!{T <: RingElem}(c::ResElem{T}, a::ResElem{T})
   c.data = mod(c.data + data(a), modulus(a))
end

###############################################################################
#
#   Promotion rules
#
###############################################################################

Base.promote_rule{T <: RingElem}(::Type{GenRes{T}}, ::Type{T}) = GenRes{T}

Base.promote_rule{T <: RingElem, U <: Integer}(::Type{GenRes{T}}, ::Type{U}) = GenRes{T}

function promote_rule1{T <: RingElem, U <: RingElem}(::Type{GenRes{T}}, ::Type{GenRes{U}})
   Base.promote_rule(T, GenRes{U}) == T ? GenRes{T} : Union{}
end

function Base.promote_rule{T <: RingElem, U <: RingElem}(::Type{GenRes{T}}, ::Type{U})
   Base.promote_rule(T, U) == T ? GenRes{T} : promote_rule1(U, GenRes{T})
end

###############################################################################
#
#   Parent object call overloading
#
###############################################################################

function Base.call{T <: RingElem}(a::GenResRing{T}, b::RingElem)
   return a(base_ring(a)(b))
end

function Base.call{T <: RingElem}(a::GenResRing{T})
   z = GenRes{T}(zero(base_ring(a)))
   z.parent = a
   return z
end

function Base.call{T <: RingElem}(a::GenResRing{T}, b::Integer)
   z = GenRes{T}(mod(base_ring(a)(b), modulus(a)))
   z.parent = a
   return z
end

function Base.call{T <: RingElem}(a::GenResRing{T}, b::T)
   base_ring(a) != parent(b) && error("Operation on incompatible objects")
   z = GenRes{T}(mod(b, modulus(a)))
   z.parent = a
   return z
end

function Base.call{T <: RingElem}(a::GenResRing{T}, b::ResElem{T})
   a != parent(b) && error("Operation on incompatible objects")
   return b
end

###############################################################################
#
#   ResidueRing constructor
#
###############################################################################

function ResidueRing{T <: RingElem}(R::Ring, el::T; cached=true)
   parent(el) != R && error("Modulus is not an element of the specified ring")
   el == 0 && throw(DivideError())
   
   return GenResRing{T}(el, cached)
end

function ResidueRing(R::FlintIntegerRing, el::Integer; cached=true)
   el == 0 && throw(DivideError())
   
   return GenResRing{fmpz}(R(el), cached)
end
