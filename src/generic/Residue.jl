###############################################################################
#
#   Residue.jl : generic residue rings (modulo a principal ideal)
#
###############################################################################

export ResidueRing, GenResidue, GenResidueRing, inv, modulus, data

###############################################################################
#
#   Data type and parent object methods
#
###############################################################################

parent_type{T}(::Type{GenResidue{T}}) = GenResidueRing{T}

elem_type{T <: RingElem}(::GenResidueRing{T}) = GenResidue{T}

base_ring{T}(a::ResRing{T}) = a.base_ring::parent_type(T)

base_ring(a::ResidueElem) = base_ring(parent(a))

parent(a::ResidueElem) = a.parent

function check_parent_type{T <: RingElem}(a::ResRing{T}, b::ResRing{T})
   # exists only to check types of parents agree
end
   
function check_parent(a::ResidueElem, b::ResidueElem)
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

function Base.hash(a::ResidueElem, h::UInt)
   b = 0x539c1c8715c1adc2%UInt
   return b $ hash(data(a), h) $ h
end

function modulus(R::ResRing)
   return R.modulus
end

function modulus(a::ResidueElem)
   return modulus(parent(a))
end

data(a::ResidueElem) = a.data

zero(R::ResRing) = R(0)

one(R::ResRing) = R(1)

iszero(a::ResidueElem) = iszero(data(a))

isone(a::ResidueElem) = isone(data(a))

function isunit(a::ResidueElem)
   g, ainv = gcdinv(data(a), modulus(a))
   return g == 1
end

deepcopy(a::ResidueElem) = parent(a)(deepcopy(data(a)))

###############################################################################
#
#   Canonicalisation
#
###############################################################################

canonical_unit(a::ResidueElem) = a

###############################################################################
#
#   AbstractString{} I/O
#
###############################################################################

function show(io::IO, x::ResidueElem)
   print(io, data(x))
end

function show(io::IO, a::ResRing)
   print(io, "Residue ring of ", base_ring(a), " modulo ", modulus(a))
end

needs_parentheses(x::ResidueElem) = needs_parentheses(data(x))

is_negative(x::ResidueElem) = is_negative(data(x))

show_minus_one{T <: RingElem}(::Type{ResidueElem{T}}) = true

###############################################################################
#
#   Unary operations
#
###############################################################################

function -{T <: RingElem}(a::ResidueElem{T})
   parent(a)(-data(a))
end

###############################################################################
#
#   Binary operators
#
###############################################################################

function +{T <: RingElem}(a::ResidueElem{T}, b::ResidueElem{T})
   check_parent(a, b)
   return parent(a)(data(a) + data(b))
end

function -{T <: RingElem}(a::ResidueElem{T}, b::ResidueElem{T})
   check_parent(a, b)
   return parent(a)(data(a) - data(b))
end

function *{T <: RingElem}(a::ResidueElem{T}, b::ResidueElem{T})
   check_parent(a, b)
   return parent(a)(data(a) * data(b))
end

###############################################################################
#
#   Ad hoc binary operations
#
###############################################################################

*{T <: RingElem}(a::ResidueElem{T}, b::Integer) = parent(a)(data(a) * b)

*{T <: RingElem}(a::Integer, b::ResidueElem{T}) = parent(b)(a * data(b))

+{T <: RingElem}(a::ResidueElem{T}, b::Integer) = parent(a)(data(a) + b)

+{T <: RingElem}(a::Integer, b::ResidueElem{T}) = parent(b)(a + data(b))

-{T <: RingElem}(a::ResidueElem{T}, b::Integer) = parent(a)(data(a) - b)

-{T <: RingElem}(a::Integer, b::ResidueElem{T}) = parent(b)(a - data(b))

###############################################################################
#
#   Powering
#
###############################################################################

function ^{T <: RingElem}(a::ResidueElem{T}, b::Int)
   parent(a)(powmod(data(a), b, modulus(a)))
end

###############################################################################
#
#   Comparison
#
###############################################################################

function =={T <: RingElem}(a::ResidueElem{T}, b::ResidueElem{T})
   check_parent(a, b)
   return data(a) == data(b)
end

###############################################################################
#
#   Ad hoc comparison
#
###############################################################################

function =={T <: RingElem}(a::ResidueElem{T}, b::Integer)
   z = base_ring(a)(b)
   return data(a) == mod(z, modulus(a))
end

function =={T <: RingElem}(a::Integer, b::ResidueElem{T})
   z = base_ring(b)(a)
   return data(b) == mod(z, modulus(b))
end

###############################################################################
#
#   Inversion
#
###############################################################################

function inv{T <: RingElem}(a::ResidueElem{T})
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

function divexact{T <: RingElem}(a::ResidueElem{T}, b::ResidueElem{T})
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

function gcd{T <: RingElem}(a::ResidueElem{T}, b::ResidueElem{T})
   check_parent(a, b)
   return parent(a)(gcd(gcd(data(a), modulus(a)), data(b)))
end

###############################################################################
#
#   Unsafe functions
#
###############################################################################

function mul!{T <: RingElem}(c::ResidueElem{T}, a::ResidueElem{T}, b::ResidueElem{T})
   c.data = mod(data(a)*data(b), modulus(a))
end

function addeq!{T <: RingElem}(c::ResidueElem{T}, a::ResidueElem{T})
   c.data = mod(c.data + data(a), modulus(a))
end

###############################################################################
#
#   Promotion rules
#
###############################################################################

Base.promote_rule{T <: RingElem}(::Type{GenResidue{T}}, ::Type{T}) = GenResidue{T}

Base.promote_rule{T <: RingElem, U <: Integer}(::Type{GenResidue{T}}, ::Type{U}) = GenResidue{T}

function promote_rule1{T <: RingElem, U <: RingElem}(::Type{GenResidue{T}}, ::Type{GenResidue{U}})
   Base.promote_rule(T, GenResidue{U}) == T ? GenResidue{T} : Union{}
end

function Base.promote_rule{T <: RingElem, U <: RingElem}(::Type{GenResidue{T}}, ::Type{U})
   Base.promote_rule(T, U) == T ? GenResidue{T} : promote_rule1(U, GenResidue{T})
end

###############################################################################
#
#   Parent object call overloading
#
###############################################################################

function Base.call{T <: RingElem}(a::GenResidueRing{T}, b::RingElem)
   return a(base_ring(a)(b))
end

function Base.call{T <: RingElem}(a::GenResidueRing{T})
   z = GenResidue{T}(zero(base_ring(a)))
   z.parent = a
   return z
end

function Base.call{T <: RingElem}(a::GenResidueRing{T}, b::Integer)
   z = GenResidue{T}(mod(base_ring(a)(b), modulus(a)))
   z.parent = a
   return z
end

function Base.call{T <: RingElem}(a::GenResidueRing{T}, b::T)
   base_ring(a) != parent(b) && error("Operation on incompatible objects")
   z = GenResidue{T}(mod(b, modulus(a)))
   z.parent = a
   return z
end

function Base.call{T <: RingElem}(a::GenResidueRing{T}, b::ResidueElem{T})
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
   
   return GenResidueRing{T}(el, cached)
end

function ResidueRing(R::FlintIntegerRing, el::Integer; cached=true)
   el == 0 && throw(DivideError())
   
   return GenResidueRing{fmpz}(R(el), cached)
end
