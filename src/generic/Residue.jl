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

doc"""
    base_ring{T <: RingElem}(S::ResRing{T})
> Return the base ring $R$ of the given residue ring $S = R/(a)$.
"""
base_ring{T <: RingElem}(S::ResRing{T}) = S.base_ring::parent_type(T)

doc"""
    base_ring(r::ResElem)
> Return the base ring $R$ of the residue ring $R/(a)$ that the supplied
> element $r$ belongs to.
"""
base_ring(r::ResElem) = base_ring(parent(r))

doc"""
    parent(a::ResElem)
> Return the parent object of the given residue element.
"""
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

doc"""
    modulus(R::ResRing)
> Return the modulus $a$ of the given residue ring $S = R/(a)$.
"""
function modulus(S::ResRing)
   return S.modulus
end

doc"""
    modulus(R::ResRing)
> Return the modulus $a$ of the residue ring $S = R/(a)$ that the supplied
> residue $r$ belongs to.
"""
function modulus(r::ResElem)
   return modulus(parent(r))
end

data(a::ResElem) = a.data

doc"""
    zero(R::ResRing)
> Return the zero element of the given residue ring, i.e. $0 \pmod{a}$ where
> $a$ is the modulus of the residue ring.
"""
zero(R::ResRing) = R(0)

doc"""
    zero(R::ResRing)
> Return $1 \pmod{a}$ where $a$ is the modulus of the residue ring.
"""
one(R::ResRing) = R(1)

doc"""
    iszero(a::ResElem)
> Return `true` if the supplied element $a$ is zero in the residue ring it
> belongs to, otherwise return `false`.
"""
iszero(a::ResElem) = iszero(data(a))

doc"""
    isone(a::ResElem)
> Return `true` if the supplied element $a$ is one in the residue ring it
> belongs to, otherwise return `false`.
"""
isone(a::ResElem) = isone(data(a))

doc"""
    iszero(a::ResElem)
> Return `true` if the supplied element $a$ is invertible in the residue ring
> it belongs to, otherwise return `false`.
"""
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

doc"""
    -(a::ResElem)
> Return $-a$.
"""
function -{T <: RingElem}(a::ResElem{T})
   parent(a)(-data(a))
end

###############################################################################
#
#   Binary operators
#
###############################################################################

doc"""
    +{T <: RingElem}(a::ResElem{T}, b::ResElem{T})
> Return $a + b$.
"""
function +{T <: RingElem}(a::ResElem{T}, b::ResElem{T})
   check_parent(a, b)
   return parent(a)(data(a) + data(b))
end

doc"""
    -{T <: RingElem}(a::ResElem{T}, b::ResElem{T})
> Return $a - b$.
"""
function -{T <: RingElem}(a::ResElem{T}, b::ResElem{T})
   check_parent(a, b)
   return parent(a)(data(a) - data(b))
end

doc"""
    *{T <: RingElem}(a::ResElem{T}, b::ResElem{T})
> Return $a\times b$.
"""
function *{T <: RingElem}(a::ResElem{T}, b::ResElem{T})
   check_parent(a, b)
   return parent(a)(data(a) * data(b))
end

###############################################################################
#
#   Ad hoc binary operations
#
###############################################################################

doc"""
    *(a::ResElem, b::Integer)
> Return $a\times b$.
"""
*(a::ResElem, b::Integer) = parent(a)(data(a) * b)

doc"""
    *(a::ResElem, b::fmpz)
> Return $a\times b$.
"""
*(a::ResElem, b::fmpz) = parent(a)(data(a) * b)

doc"""
    *{T <: RingElem}(a::ResElem{T}, b::T)
> Return $a\times b$.
"""
*{T <: RingElem}(a::ResElem{T}, b::T) = parent(a)(data(a) * b)

doc"""
    *(a::Integer, b::ResElem)
> Return $a\times b$.
"""
*(a::Integer, b::ResElem) = parent(b)(a * data(b))

doc"""
    *(a::fmpz, b::ResElem)
> Return $a\times b$.
"""
*(a::fmpz, b::ResElem) = parent(b)(a * data(b))

doc"""
    *{T <: RingElem}(a::T, b::ResElem{T})
> Return $a\times b$.
"""
*{T <: RingElem}(a::T, b::ResElem{T}) = parent(b)(a * data(b))

doc"""
    +(a::ResElem, b::Integer)
> Return $a + b$.
"""
+(a::ResElem, b::Integer) = parent(a)(data(a) + b)

doc"""
    +(a::ResElem, b::Integer)
> Return $a + b$.
"""
+(a::ResElem, b::fmpz) = parent(a)(data(a) + b)

doc"""
    +{T <: RingElem}(a::ResElem{T}, b::T)
> Return $a + b$.
"""
+{T <: RingElem}(a::ResElem{T}, b::T) = parent(a)(data(a) + b)

doc"""
    +(a::Integer, b::ResElem)
> Return $a + b$.
"""
+(a::Integer, b::ResElem) = parent(b)(a + data(b))

doc"""
    +(a::fmpz, b::ResElem)
> Return $a + b$.
"""
+(a::fmpz, b::ResElem) = parent(b)(a + data(b))

doc"""
    +{T <: RingElem}(a::T, b::ResElem{T})
> Return $a + b$.
"""
+{T <: RingElem}(a::T, b::ResElem{T}) = parent(b)(a + data(b))

doc"""
    -(a::ResElem, b::Integer)
> Return $a - b$.
"""
-(a::ResElem, b::Integer) = parent(a)(data(a) - b)

doc"""
    -(a::ResElem, b::fmpz)
> Return $a - b$.
"""
-(a::ResElem, b::fmpz) = parent(a)(data(a) - b)

doc"""
    -{T <: RingElem}(a::ResElem{T}, b::T)
> Return $a - b$.
"""
-{T <: RingElem}(a::ResElem{T}, b::T) = parent(a)(data(a) - b)

doc"""
    -(a::Integer, b::ResElem)
> Return $a - b$.
"""
-(a::Integer, b::ResElem) = parent(b)(a - data(b))

doc"""
    -(a::fmpz, b::ResElem)
> Return $a - b$.
"""
-(a::fmpz, b::ResElem) = parent(b)(a - data(b))

doc"""
    -{T <: RingElem}(a::T, b::ResElem{T})
> Return $a - b$.
"""
-{T <: RingElem}(a::T, b::ResElem{T}) = parent(b)(a - data(b))

###############################################################################
#
#   Powering
#
###############################################################################

doc"""
    ^(a::ResElem, b::Int)
> Return $a^b$.
"""
function ^(a::ResElem, b::Int)
   parent(a)(powmod(data(a), b, modulus(a)))
end

###############################################################################
#
#   Comparison
#
###############################################################################

doc"""
    =={T <: RingElem}(x::ResElem{T}, y::ResElem{T})
> Return `true` if $x == y$ arithmetically, otherwise return `false`. Recall
> that power series to different precisions may still be arithmetically
> equal to the minimum of the two precisions.
"""
function =={T <: RingElem}(a::ResElem{T}, b::ResElem{T})
   check_parent(a, b)
   return data(a) == data(b)
end

doc"""
    isequal{T <: RingElem}(x::ResElem{T}, y::ResElem{T})
> Return `true` if $x == y$ exactly, otherwise return `false`. This function is
> useful in cases where the data of the residues are inexact, e.g. power series
> Only if the power series are precisely the same, to the same precision, are
> they declared equal by this function.
"""
function isequal{T <: RingElem}(a::ResElem{T}, b::ResElem{T})
   check_parent(a, b)
   return isequal(data(a), data(b))
end

###############################################################################
#
#   Ad hoc comparison
#
###############################################################################

doc"""
    ==(x::ResElem, y::Integer)
> Return `true` if $x == y$ arithmetically, otherwise return `false`.
"""
function ==(a::ResElem, b::Integer)
   z = base_ring(a)(b)
   return data(a) == mod(z, modulus(a))
end

doc"""
    ==(x::Integer, y::ResElem)
> Return `true` if $x == y$ arithmetically, otherwise return `false`.
"""
function ==(a::Integer, b::ResElem)
   z = base_ring(b)(a)
   return data(b) == mod(z, modulus(b))
end

doc"""
    ==(x::ResElem, y::fmpz)
> Return `true` if $x == y$ arithmetically, otherwise return `false`.
"""
function ==(a::ResElem, b::fmpz)
   z = base_ring(a)(b)
   return data(a) == mod(z, modulus(a))
end

doc"""
    ==(x::fmpz, y::ResElem)
> Return `true` if $x == y$ arithmetically, otherwise return `false`.
"""
function ==(a::fmpz, b::ResElem)
   z = base_ring(b)(a)
   return data(b) == mod(z, modulus(b))
end

doc"""
    =={T <: RingElem}(x::ResElem{T}, y::T)
> Return `true` if $x == y$ arithmetically, otherwise return `false`.
"""
function =={T <: RingElem}(a::ResElem{T}, b::T)
   z = base_ring(a)(b)
   return data(a) == mod(z, modulus(a))
end

doc"""
    =={T <: RingElem}(x::T, y::ResElem{T})
> Return `true` if $x == y$ arithmetically, otherwise return `false`.
"""
function =={T <: RingElem}(a::T, b::ResElem{T})
   z = base_ring(b)(a)
   return data(b) == mod(z, modulus(b))
end

# Prevent ambiguities

==(a::GenRes{fmpz}, b::fmpz) = a.data == mod(b, modulus(a))

==(a::fmpz, b::GenRes{fmpz}) = mod(a, modulus(b)) == b.data

###############################################################################
#
#   Inversion
#
###############################################################################

doc"""
    inv(a::ResElem)
> Return the inverse of the element $a$ in the residue ring. If an impossible
> inverse is encountered, an exception is raised.
"""
function inv(a::ResElem)
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

doc"""
    divexact{T <: RingElem}(a::ResElem{T}, b::ResElem{T})
> Return $a/b$ where the quotient is expected to be exact.
"""
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

doc"""
    gcd{T <: RingElem}(a::ResElem{T}, b::ResElem{T})
> Return a greatest common divisor of $a$ and $b$ if one exists. This is done
> by taking the greatest common divisor of the data associated with the
> supplied residues and taking its greatest common divisor with the modulus.
"""
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

function Base.call{T <: RingElem}(a::GenResRing{T}, b::fmpz)
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

doc"""
    ResidueRing{T <: RingElem}(R::Ring, a::T; cached=true)
> Create the residue ring $R/(a)$ where $a$ is an element of the ring $R$. We
> require $a \neq 0$. If `cached == true` (the default) then the resulting
> residue ring parent object is cached and returned for any subsequent calls
> to the constructor with the same base ring $R$ and element $a$. 
"""
function ResidueRing{T <: RingElem}(R::Ring, a::T; cached=true)
   parent(a) != R && error("Modulus is not an element of the specified ring")
   a == 0 && throw(DivideError())
   
   return GenResRing{T}(a, cached)
end

function ResidueRing(R::FlintIntegerRing, a::Integer; cached=true)
   a == 0 && throw(DivideError())
   
   return GenResRing{fmpz}(R(a), cached)
end
