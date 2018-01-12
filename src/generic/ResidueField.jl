###############################################################################
#
#   ResidueField.jl : generic residue fields (modulo a principal ideal)
#
###############################################################################

export ResidueField

###############################################################################
#
#   Data type and parent object methods
#
###############################################################################

parent_type(::Type{ResF{T}}) where T <: RingElement = ResField{T}

elem_type(::Type{ResField{T}}) where {T <: RingElement} = ResF{T}

doc"""
    base_ring{T <: RingElement}(S::Nemo.ResField{T})
> Return the base ring $R$ of the given residue ring $S = R/(a)$.
"""
base_ring(S::Nemo.ResField{T}) where {T <: RingElement} = S.base_ring::parent_type(T)

doc"""
    base_ring(r::Nemo.ResFieldElem)
> Return the base ring $R$ of the residue ring $R/(a)$ that the supplied
> element $r$ belongs to.
"""
base_ring(r::Nemo.ResFieldElem) = base_ring(parent(r))

doc"""
    parent(a::Nemo.ResFieldElem)
> Return the parent object of the given residue element.
"""
parent(a::Nemo.ResFieldElem) = a.parent

isdomain_type(a::Type{T}) where T <: Nemo.ResFieldElem = false

function isexact_type(a::Type{T}) where {S <: RingElement, T <: Nemo.ResFieldElem{S}}
   return isexact_type(S)
end

function check_parent_type(a::Nemo.ResField{T}, b::Nemo.ResField{T}) where {T <: RingElement}
   # exists only to check types of parents agree
end

function check_parent(a::Nemo.ResFieldElem, b::Nemo.ResFieldElem)
   if parent(a) != parent(b)
      check_parent_type(parent(a), parent(b))
      modulus(parent(a)) != modulus(parent(b)) && error("Incompatible moduli in residue operation") #CF: maybe extend to divisibility?
   end
end

###############################################################################
#
#   Basic manipulation
#
###############################################################################

function Base.hash(a::Nemo.ResFieldElem, h::UInt)
   b = 0x539c1c8715c1adc2%UInt
   return xor(b, xor(hash(data(a), h), h))
end

doc"""
    modulus(R::Nemo.ResField)
> Return the modulus $a$ of the given residue ring $S = R/(a)$.
"""
function modulus(S::Nemo.ResField)
   return S.modulus
end

doc"""
    modulus(R::Nemo.ResFieldElem)
> Return the modulus $a$ of the residue ring $S = R/(a)$ that the supplied
> residue $r$ belongs to.
"""
function modulus(r::Nemo.ResFieldElem)
   return modulus(parent(r))
end

doc"""
    characteristic(R::Nemo.ResField)
> Return the modulus $a$ of the residue ring $S = R/(a)$ that the supplied
> residue $r$ belongs to.
"""
function characteristic(r::Nemo.ResField)
   R = base_ring(r)
   while R != Union{}
      if typeof(R) <: Field
         return characteristic(R)
      end
      R = base_ring(R)
   end
   return characteristic(base_ring(R))
end

doc"""
    characteristic{T <: Integer}(R::Nemo.ResField{T})
> Return the modulus $a$ of the residue ring $S = R/(a)$ that the supplied
> residue $r$ belongs to.
"""
function characteristic(r::Nemo.ResField{T}) where T <: Integer
   return modulus(r)
end

data(a::Nemo.ResFieldElem) = a.data

doc"""
    zero(R::Nemo.ResField)
> Return the zero element of the given residue ring, i.e. $0 \pmod{a}$ where
> $a$ is the modulus of the residue ring.
"""
zero(R::Nemo.ResField) = R(0)

doc"""
    one(R::Nemo.ResField)
> Return $1 \pmod{a}$ where $a$ is the modulus of the residue ring.
"""
one(R::Nemo.ResField) = R(1)

doc"""
    iszero(a::Nemo.ResFieldElem)
> Return `true` if the supplied element $a$ is zero in the residue ring it
> belongs to, otherwise return `false`.
"""
iszero(a::Nemo.ResFieldElem) = iszero(data(a))

doc"""
    isone(a::Nemo.ResFieldElem)
> Return `true` if the supplied element $a$ is one in the residue ring it
> belongs to, otherwise return `false`.
"""
isone(a::Nemo.ResFieldElem) = isone(data(a))

doc"""
    isunit(a::Nemo.ResFieldElem)
> Return `true` if the supplied element $a$ is invertible in the residue ring
> it belongs to, otherwise return `false`.
"""
function isunit(a::Nemo.ResFieldElem)
   g = gcd(data(a), modulus(a))
   return isone(g)
end

deepcopy_internal(a::Nemo.ResFieldElem, dict::ObjectIdDict) =
   parent(a)(deepcopy(data(a)))

###############################################################################
#
#   Canonicalisation
#
###############################################################################

function canonical_unit(x::Nemo.ResFieldElem{<:Union{Integer, RingElem}})
  if iszero(x)
    return one(parent(x))
  end
  return x
end

###############################################################################
#
#   AbstractString I/O
#
###############################################################################

function show(io::IO, x::Nemo.ResFieldElem)
   print(io, data(x))
end

function show(io::IO, a::Nemo.ResField)
   print(io, "Residue field of ", base_ring(a), " modulo ", modulus(a))
end

needs_parentheses(x::Nemo.ResFieldElem) = needs_parentheses(data(x))

isnegative(x::Nemo.ResFieldElem) = isnegative(data(x))

show_minus_one(::Type{ResF{T}}) where {T <: RingElement} = true

###############################################################################
#
#   Unary operations
#
###############################################################################

doc"""
    -(a::Nemo.ResFieldElem)
> Return $-a$.
"""
function -(a::Nemo.ResFieldElem)
   parent(a)(-data(a))
end

###############################################################################
#
#   Binary operators
#
###############################################################################

doc"""
    +{T <: RingElement}(a::Nemo.ResFieldElem{T}, b::Nemo.ResFieldElem{T})
> Return $a + b$.
"""
function +(a::Nemo.ResFieldElem{T}, b::Nemo.ResFieldElem{T}) where {T <: RingElement}
   check_parent(a, b)
   return parent(a)(data(a) + data(b))
end

doc"""
    -{T <: RingElement}(a::Nemo.ResFieldElem{T}, b::Nemo.ResFieldElem{T})
> Return $a - b$.
"""
function -(a::Nemo.ResFieldElem{T}, b::Nemo.ResFieldElem{T}) where {T <: RingElement}
   check_parent(a, b)
   return parent(a)(data(a) - data(b))
end

doc"""
    *{T <: RingElement}(a::Nemo.ResFieldElem{T}, b::Nemo.ResFieldElem{T})
> Return $a\times b$.
"""
function *(a::Nemo.ResFieldElem{T}, b::Nemo.ResFieldElem{T}) where {T <: RingElement}
   check_parent(a, b)
   return parent(a)(data(a) * data(b))
end

###############################################################################
#
#   Ad hoc binary operations
#
###############################################################################

doc"""
    *(a::Nemo.ResFieldElem, b::Union{Integer, Rational, AbstractFloat})
> Return $a\times b$.
"""
*(a::Nemo.ResFieldElem, b::Union{Integer, Rational, AbstractFloat}) = parent(a)(data(a) * b)

doc"""
    *{T <: RingElem}(a::Nemo.ResFieldElem{T}, b::T)
> Return $a\times b$.
"""
*(a::Nemo.ResFieldElem{T}, b::T) where {T <: RingElem} = parent(a)(data(a) * b)

doc"""
    *(a::Union{Integer, Rational, AbstractFloat}, b::Nemo.ResFieldElem)
> Return $a\times b$.
"""
*(a::Union{Integer, Rational, AbstractFloat}, b::Nemo.ResFieldElem) = parent(b)(a * data(b))

doc"""
    *{T <: RingElem}(a::T, b::Nemo.ResFieldElem{T})
> Return $a\times b$.
"""
*(a::T, b::Nemo.ResFieldElem{T}) where {T <: RingElem} = parent(b)(a * data(b))

doc"""
    +(a::Nemo.ResFieldElem, b::Union{Integer, Rational, AbstractFloat})
> Return $a + b$.
"""
+(a::Nemo.ResFieldElem, b::Union{Integer, Rational, AbstractFloat}) = parent(a)(data(a) + b)

doc"""
    +{T <: RingElem}(a::Nemo.ResFieldElem{T}, b::T)
> Return $a + b$.
"""
+(a::Nemo.ResFieldElem{T}, b::T) where {T <: RingElem} = parent(a)(data(a) + b)

doc"""
    +(a::Union{Integer, Rational, AbstractFloat}, b::Nemo.ResFieldElem)
> Return $a + b$.
"""
+(a::Union{Integer, Rational, AbstractFloat}, b::Nemo.ResFieldElem) = parent(b)(a + data(b))

doc"""
    +{T <: RingElem}(a::T, b::Nemo.ResFieldElem{T})
> Return $a + b$.
"""
+(a::T, b::Nemo.ResFieldElem{T}) where {T <: RingElem} = parent(b)(a + data(b))

doc"""
    -(a::Nemo.ResFieldElem, b::Union{Integer, Rational, AbstractFloat})
> Return $a - b$.
"""
-(a::Nemo.ResFieldElem, b::Union{Integer, Rational, AbstractFloat}) = parent(a)(data(a) - b)

doc"""
    -{T <: RingElem}(a::Nemo.ResFieldElem{T}, b::T)
> Return $a - b$.
"""
-(a::Nemo.ResFieldElem{T}, b::T) where {T <: RingElem} = parent(a)(data(a) - b)

doc"""
    -(a::Union{Integer, Rational, AbstractFloat}, b::Nemo.ResFieldElem)
> Return $a - b$.
"""
-(a::Union{Integer, Rational, AbstractFloat}, b::Nemo.ResFieldElem) = parent(b)(a - data(b))

doc"""
    -{T <: RingElem}(a::T, b::Nemo.ResFieldElem{T})
> Return $a - b$.
"""
-(a::T, b::Nemo.ResFieldElem{T}) where {T <: RingElem} = parent(b)(a - data(b))

###############################################################################
#
#   Powering
#
###############################################################################

doc"""
    ^(a::Nemo.ResFieldElem, b::Int)
> Return $a^b$.
"""
function ^(a::Nemo.ResFieldElem, b::Int)
   parent(a)(powmod(data(a), b, modulus(a)))
end

###############################################################################
#
#   Comparison
#
###############################################################################

doc"""
    =={T <: RingElement}(x::Nemo.ResFieldElem{T}, y::Nemo.ResFieldElem{T})
> Return `true` if $x == y$ arithmetically, otherwise return `false`. Recall
> that power series to different precisions may still be arithmetically
> equal to the minimum of the two precisions.
"""
function ==(a::Nemo.ResFieldElem{T}, b::Nemo.ResFieldElem{T}) where {T <: RingElement}
   check_parent(a, b)
   return data(a) == data(b)
end

doc"""
    isequal{T <: RingElement}(x::Nemo.ResFieldElem{T}, y::Nemo.ResFieldElem{T})
> Return `true` if $x == y$ exactly, otherwise return `false`. This function is
> useful in cases where the data of the residues are inexact, e.g. power series
> Only if the power series are precisely the same, to the same precision, are
> they declared equal by this function.
"""
function isequal(a::Nemo.ResFieldElem{T}, b::Nemo.ResFieldElem{T}) where {T <: RingElement}
   check_parent(a, b)
   return isequal(data(a), data(b))
end

###############################################################################
#
#   Ad hoc comparison
#
###############################################################################

doc"""
    ==(x::Nemo.ResFieldElem, y::Union{Integer, Rational, AbstractFloat})
> Return `true` if $x == y$ arithmetically, otherwise return `false`.
"""
function ==(a::Nemo.ResFieldElem, b::Union{Integer, Rational, AbstractFloat})
   z = base_ring(a)(b)
   return data(a) == mod(z, modulus(a))
end

doc"""
    ==(x::Union{Integer, Rational, AbstractFloat}, y::Nemo.ResFieldElem)
> Return `true` if $x == y$ arithmetically, otherwise return `false`.
"""
function ==(a::Union{Integer, Rational, AbstractFloat}, b::Nemo.ResFieldElem)
   z = base_ring(b)(a)
   return data(b) == mod(z, modulus(b))
end

doc"""
    =={T <: RingElem}(x::Nemo.ResFieldElem{T}, y::T)
> Return `true` if $x == y$ arithmetically, otherwise return `false`.
"""
function ==(a::Nemo.ResFieldElem{T}, b::T) where {T <: RingElem}
   z = base_ring(a)(b)
   return data(a) == mod(z, modulus(a))
end

doc"""
    =={T <: RingElem}(x::T, y::Nemo.ResFieldElem{T})
> Return `true` if $x == y$ arithmetically, otherwise return `false`.
"""
function ==(a::T, b::Nemo.ResFieldElem{T}) where {T <: RingElem}
   z = base_ring(b)(a)
   return data(b) == mod(z, modulus(b))
end

###############################################################################
#
#   Inversion
#
###############################################################################

doc"""
    inv(a::Nemo.ResFieldElem)
> Return the inverse of the element $a$ in the residue ring. If an impossible
> inverse is encountered, an exception is raised.
"""
function inv(a::Nemo.ResFieldElem)
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
    divexact{T <: RingElement}(a::Nemo.ResFieldElem{T}, b::Nemo.ResFieldElem{T})
> Return $a/b$ where the quotient is expected to be exact.
"""
function divexact(a::Nemo.ResFieldElem{T}, b::Nemo.ResFieldElem{T}) where {T <: RingElement}
   check_parent(a, b)
   fl, q = divides(a, b)
   if !fl
      error("Impossible inverse in divexact")
   end
   return q
end

function divides(a::Nemo.ResFieldElem{T}, b::Nemo.ResFieldElem{T}) where {T <: RingElement}
   check_parent(a, b)
   iszero(b) && error("Division by zero in divides")
   return true, a*inv(b)
end

###############################################################################
#
#   GCD
#
###############################################################################

doc"""
    gcd{T <: RingElement}(a::Nemo.ResFieldElem{T}, b::Nemo.ResFieldElem{T})
> Return a greatest common divisor of $a$ and $b$ if one exists. This is done
> by taking the greatest common divisor of the data associated with the
> supplied residues and taking its greatest common divisor with the modulus.
"""
function gcd(a::Nemo.ResFieldElem{T}, b::Nemo.ResFieldElem{T}) where {T <: RingElement}
   check_parent(a, b)
   return parent(a)(gcd(gcd(data(a), modulus(a)), data(b)))
end

###############################################################################
#
#   Unsafe functions
#
###############################################################################

function zero!(a::Nemo.ResFieldElem{T}) where {T <: RingElement}
   a.data = zero!(a.data)
   return a
end

function mul!(c::Nemo.ResFieldElem{T}, a::Nemo.ResFieldElem{T}, b::Nemo.ResFieldElem{T}) where {T <: RingElement}
   c.data = mod(data(a)*data(b), modulus(a))
   return c
end

function addeq!(c::Nemo.ResFieldElem{T}, a::Nemo.ResFieldElem{T}) where {T <: RingElement}
   c.data = mod(data(c) + data(a), modulus(a))
   return c
end

function add!(c::Nemo.ResFieldElem{T}, a::Nemo.ResFieldElem{T}, b::Nemo.ResFieldElem{T}) where {T <: RingElement}
   c.data = mod(data(a) + data(b), modulus(a))
   return c
end

###############################################################################
#
#   Random functions
#
###############################################################################

function rand(S::Nemo.ResField{T}, v...) where {T <: RingElement}
   R = base_ring(S)
   return S(rand(R, v...))
end

###############################################################################
#
#   Promotion rules
#
###############################################################################

promote_rule(::Type{ResF{T}}, ::Type{ResF{T}}) where T <: RingElement = ResF{T}

function promote_rule(::Type{ResF{T}}, ::Type{U}) where {T <: RingElement, U <: RingElement}
   promote_rule(T, U) == T ? ResF{T} : Union{}
end

###############################################################################
#
#   Parent object call overloading
#
###############################################################################

function (a::ResField{T})(b::RingElement) where {T <: RingElement}
   return a(base_ring(a)(b))
end

function (a::ResField{T})() where {T <: RingElement}
   z = ResF{T}(zero(base_ring(a)))
   z.parent = a
   return z
end

function (a::ResField{T})(b::Integer) where {T <: RingElement}
   z = ResF{T}(mod(base_ring(a)(b), modulus(a)))
   z.parent = a
   return z
end

function (a::ResField{T})(b::T) where {T <: RingElem}
   base_ring(a) != parent(b) && error("Operation on incompatible objects")
   z = ResF{T}(mod(b, modulus(a)))
   z.parent = a
   return z
end

function (a::ResField{T})(b::Nemo.ResFieldElem{T}) where {T <: RingElement}
   a != parent(b) && error("Operation on incompatible objects")
   return b
end

###############################################################################
#
#   ResidueField constructor
#
###############################################################################

doc"""
    ResidueField{T <: RingElement}(R::Nemo.Ring, a::RingElement; cached::Bool=true)
> Create the residue ring $R/(a)$ where $a$ is an element of the ring $R$. We
> require $a \neq 0$. If `cached == true` (the default) then the resulting
> residue ring parent object is cached and returned for any subsequent calls
> to the constructor with the same base ring $R$ and element $a$.
"""
function ResidueField(R::Nemo.Ring, a::RingElement; cached::Bool = true)
   iszero(a) && throw(DivideError())
   T = elem_type(R)

   return ResField{T}(R(a), cached)
end

###############################################################################
#
#   NumberField constructor (mainly for test code)
#
###############################################################################

function NumberField(a::Nemo.Generic.Poly{Rational{BigInt}}, s::AbstractString, t = "\$"; cached = true)
   S = parent(a)
   R = ResidueField(S, a, cached=cached)
   x = gen(S)
   return R, R(x)
end

function  gen(R::Nemo.Generic.ResField{Nemo.Generic.Poly{Rational{BigInt}}})
   return R(gen(base_ring(R)))
end
