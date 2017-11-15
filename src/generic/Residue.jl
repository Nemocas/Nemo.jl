###############################################################################
#
#   Residue.jl : generic residue rings (modulo a principal ideal)
#
###############################################################################

export ResidueRing, inv, modulus, data

###############################################################################
#
#   Data type and parent object methods
#
###############################################################################

parent_type(::Type{Res{T}}) where T <: RingElement = ResRing{T}

elem_type(::Type{ResRing{T}}) where {T <: RingElement} = Res{T}

doc"""
    base_ring{T <: RingElement}(S::Nemo.ResRing{T})
> Return the base ring $R$ of the given residue ring $S = R/(a)$.
"""
base_ring(S::Nemo.ResRing{T}) where {T <: RingElement} = S.base_ring::parent_type(T)

doc"""
    base_ring(r::Nemo.ResElem)
> Return the base ring $R$ of the residue ring $R/(a)$ that the supplied
> element $r$ belongs to.
"""
base_ring(r::Nemo.ResElem) = base_ring(parent(r))

doc"""
    parent(a::Nemo.ResElem)
> Return the parent object of the given residue element.
"""
parent(a::Nemo.ResElem) = a.parent

isdomain_type(a::Type{T}) where T <: Nemo.ResElem = false

function isexact_type(a::Type{T}) where {S <: RingElement, T <: Nemo.ResElem{S}}
   return isexact_type(S)
end

function check_parent_type(a::Nemo.ResRing{T}, b::Nemo.ResRing{T}) where {T <: RingElement}
   # exists only to check types of parents agree
end

function check_parent(a::Nemo.ResElem, b::Nemo.ResElem)
   if parent(a) != parent(b)
      check_parent_type(parent(a), parent(b))
      parent(a).modulus != parent(b).modulus && error("Incompatible moduli in residue operation") #CF: maybe extend to divisibility?
   end
end

###############################################################################
#
#   Basic manipulation
#
###############################################################################

function Base.hash(a::Nemo.ResElem, h::UInt)
   b = 0x539c1c8715c1adc2%UInt
   return xor(b, xor(hash(data(a), h), h))
end

doc"""
    modulus(R::Nemo.ResRing)
> Return the modulus $a$ of the given residue ring $S = R/(a)$.
"""
function modulus(S::Nemo.ResRing)
   return S.modulus
end

doc"""
    modulus(R::Nemo.ResRing)
> Return the modulus $a$ of the residue ring $S = R/(a)$ that the supplied
> residue $r$ belongs to.
"""
function modulus(r::Nemo.ResElem)
   return modulus(parent(r))
end

data(a::Nemo.ResElem) = a.data

doc"""
    zero(R::Nemo.ResRing)
> Return the zero element of the given residue ring, i.e. $0 \pmod{a}$ where
> $a$ is the modulus of the residue ring.
"""
zero(R::Nemo.ResRing) = R(0)

doc"""
    one(R::Nemo.ResRing)
> Return $1 \pmod{a}$ where $a$ is the modulus of the residue ring.
"""
one(R::Nemo.ResRing) = R(1)

doc"""
    iszero(a::Nemo.ResElem)
> Return `true` if the supplied element $a$ is zero in the residue ring it
> belongs to, otherwise return `false`.
"""
iszero(a::Nemo.ResElem) = iszero(data(a))

doc"""
    isone(a::Nemo.ResElem)
> Return `true` if the supplied element $a$ is one in the residue ring it
> belongs to, otherwise return `false`.
"""
isone(a::Nemo.ResElem) = isone(data(a))

doc"""
    isunit(a::Nemo.ResElem)
> Return `true` if the supplied element $a$ is invertible in the residue ring
> it belongs to, otherwise return `false`.
"""
function isunit(a::Nemo.ResElem)
   g = gcd(data(a), modulus(a))
   return isone(g)
end

deepcopy_internal(a::Nemo.ResElem, dict::ObjectIdDict) =
   parent(a)(deepcopy(data(a)))

###############################################################################
#
#   Canonicalisation
#
###############################################################################

function canonical_unit(x::Nemo.ResElem{<:Union{Integer, RingElem}})
 #the simple return x does not work
  # - if x == 0, this is not a unit
  # - if R is not a field....
  if iszero(x)
    return one(parent(x))
  end
  g = gcd(modulus(x), data(x))
  u = divexact(data(x), g)
  a, b = ppio(modulus(x), u)
  if isone(a)
    r = u
  elseif isone(b)
    r = b
  else
    r = crt(one(parent(a)), a, u, b)
  end
  return parent(x)(r)
end

###############################################################################
#
#   AbstractString I/O
#
###############################################################################

function show(io::IO, x::Nemo.ResElem)
   print(io, data(x))
end

function show(io::IO, a::Nemo.ResRing)
   print(io, "Residue ring of ", base_ring(a), " modulo ", modulus(a))
end

needs_parentheses(x::Nemo.ResElem) = needs_parentheses(data(x))

isnegative(x::Nemo.ResElem) = isnegative(data(x))

show_minus_one(::Type{Res{T}}) where {T <: RingElement} = true

###############################################################################
#
#   Unary operations
#
###############################################################################

doc"""
    -(a::Nemo.ResElem)
> Return $-a$.
"""
function -(a::Nemo.ResElem)
   parent(a)(-data(a))
end

###############################################################################
#
#   Binary operators
#
###############################################################################

doc"""
    +{T <: RingElement}(a::Nemo.ResElem{T}, b::Nemo.ResElem{T})
> Return $a + b$.
"""
function +(a::Nemo.ResElem{T}, b::Nemo.ResElem{T}) where {T <: RingElement}
   check_parent(a, b)
   return parent(a)(data(a) + data(b))
end

doc"""
    -{T <: RingElement}(a::Nemo.ResElem{T}, b::Nemo.ResElem{T})
> Return $a - b$.
"""
function -(a::Nemo.ResElem{T}, b::Nemo.ResElem{T}) where {T <: RingElement}
   check_parent(a, b)
   return parent(a)(data(a) - data(b))
end

doc"""
    *{T <: RingElement}(a::Nemo.ResElem{T}, b::Nemo.ResElem{T})
> Return $a\times b$.
"""
function *(a::Nemo.ResElem{T}, b::Nemo.ResElem{T}) where {T <: RingElement}
   check_parent(a, b)
   return parent(a)(data(a) * data(b))
end

###############################################################################
#
#   Ad hoc binary operations
#
###############################################################################

doc"""
    *(a::Nemo.ResElem, b::Union{Integer, Rational, AbstractFloat})
> Return $a\times b$.
"""
*(a::Nemo.ResElem, b::Union{Integer, Rational, AbstractFloat}) = parent(a)(data(a) * b)

doc"""
    *{T <: RingElem}(a::Nemo.ResElem{T}, b::T)
> Return $a\times b$.
"""
*(a::Nemo.ResElem{T}, b::T) where {T <: RingElem} = parent(a)(data(a) * b)

doc"""
    *(a::Union{Integer, Rational, AbstractFloat}, b::Nemo.ResElem)
> Return $a\times b$.
"""
*(a::Union{Integer, Rational, AbstractFloat}, b::Nemo.ResElem) = parent(b)(a * data(b))

doc"""
    *{T <: RingElem}(a::T, b::Nemo.ResElem{T})
> Return $a\times b$.
"""
*(a::T, b::Nemo.ResElem{T}) where {T <: RingElem} = parent(b)(a * data(b))

doc"""
    +(a::Nemo.ResElem, b::Union{Integer, Rational, AbstractFloat})
> Return $a + b$.
"""
+(a::Nemo.ResElem, b::Union{Integer, Rational, AbstractFloat}) = parent(a)(data(a) + b)

doc"""
    +{T <: RingElem}(a::Nemo.ResElem{T}, b::T)
> Return $a + b$.
"""
+(a::Nemo.ResElem{T}, b::T) where {T <: RingElem} = parent(a)(data(a) + b)

doc"""
    +(a::Union{Integer, Rational, AbstractFloat}, b::Nemo.ResElem)
> Return $a + b$.
"""
+(a::Union{Integer, Rational, AbstractFloat}, b::Nemo.ResElem) = parent(b)(a + data(b))

doc"""
    +{T <: RingElem}(a::T, b::Nemo.ResElem{T})
> Return $a + b$.
"""
+(a::T, b::Nemo.ResElem{T}) where {T <: RingElem} = parent(b)(a + data(b))

doc"""
    -(a::Nemo.ResElem, b::Union{Integer, Rational, AbstractFloat})
> Return $a - b$.
"""
-(a::Nemo.ResElem, b::Union{Integer, Rational, AbstractFloat}) = parent(a)(data(a) - b)

doc"""
    -{T <: RingElem}(a::Nemo.ResElem{T}, b::T)
> Return $a - b$.
"""
-(a::Nemo.ResElem{T}, b::T) where {T <: RingElem} = parent(a)(data(a) - b)

doc"""
    -(a::Union{Integer, Rational, AbstractFloat}, b::Nemo.ResElem)
> Return $a - b$.
"""
-(a::Union{Integer, Rational, AbstractFloat}, b::Nemo.ResElem) = parent(b)(a - data(b))

doc"""
    -{T <: RingElem}(a::T, b::Nemo.ResElem{T})
> Return $a - b$.
"""
-(a::T, b::Nemo.ResElem{T}) where {T <: RingElem} = parent(b)(a - data(b))

###############################################################################
#
#   Powering
#
###############################################################################

doc"""
    ^(a::Nemo.ResElem, b::Int)
> Return $a^b$.
"""
function ^(a::Nemo.ResElem, b::Int)
   parent(a)(powmod(data(a), b, modulus(a)))
end

###############################################################################
#
#   Comparison
#
###############################################################################

doc"""
    =={T <: RingElement}(x::Nemo.ResElem{T}, y::Nemo.ResElem{T})
> Return `true` if $x == y$ arithmetically, otherwise return `false`. Recall
> that power series to different precisions may still be arithmetically
> equal to the minimum of the two precisions.
"""
function ==(a::Nemo.ResElem{T}, b::Nemo.ResElem{T}) where {T <: RingElement}
   check_parent(a, b)
   return data(a) == data(b)
end

doc"""
    isequal{T <: RingElement}(x::Nemo.ResElem{T}, y::Nemo.ResElem{T})
> Return `true` if $x == y$ exactly, otherwise return `false`. This function is
> useful in cases where the data of the residues are inexact, e.g. power series
> Only if the power series are precisely the same, to the same precision, are
> they declared equal by this function.
"""
function isequal(a::Nemo.ResElem{T}, b::Nemo.ResElem{T}) where {T <: RingElement}
   check_parent(a, b)
   return isequal(data(a), data(b))
end

###############################################################################
#
#   Ad hoc comparison
#
###############################################################################

doc"""
    ==(x::Nemo.ResElem, y::Union{Integer, Rational, AbstractFloat})
> Return `true` if $x == y$ arithmetically, otherwise return `false`.
"""
function ==(a::Nemo.ResElem, b::Union{Integer, Rational, AbstractFloat})
   z = base_ring(a)(b)
   return data(a) == mod(z, modulus(a))
end

doc"""
    ==(x::Union{Integer, Rational, AbstractFloat}, y::Nemo.ResElem)
> Return `true` if $x == y$ arithmetically, otherwise return `false`.
"""
function ==(a::Union{Integer, Rational, AbstractFloat}, b::Nemo.ResElem)
   z = base_ring(b)(a)
   return data(b) == mod(z, modulus(b))
end

doc"""
    =={T <: RingElem}(x::Nemo.ResElem{T}, y::T)
> Return `true` if $x == y$ arithmetically, otherwise return `false`.
"""
function ==(a::Nemo.ResElem{T}, b::T) where {T <: RingElem}
   z = base_ring(a)(b)
   return data(a) == mod(z, modulus(a))
end

doc"""
    =={T <: RingElem}(x::T, y::Nemo.ResElem{T})
> Return `true` if $x == y$ arithmetically, otherwise return `false`.
"""
function ==(a::T, b::Nemo.ResElem{T}) where {T <: RingElem}
   z = base_ring(b)(a)
   return data(b) == mod(z, modulus(b))
end

###############################################################################
#
#   Inversion
#
###############################################################################

doc"""
    inv(a::Nemo.ResElem)
> Return the inverse of the element $a$ in the residue ring. If an impossible
> inverse is encountered, an exception is raised.
"""
function inv(a::Nemo.ResElem)
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
    divexact{T <: RingElement}(a::Nemo.ResElem{T}, b::Nemo.ResElem{T})
> Return $a/b$ where the quotient is expected to be exact.
"""
function divexact(a::Nemo.ResElem{T}, b::Nemo.ResElem{T}) where {T <: RingElement}
   check_parent(a, b)
   fl, q = divides(a, b)
   if !fl
      error("Impossible inverse in divexact")
   end
   return q
end

function divides(a::Nemo.ResElem{T}, b::Nemo.ResElem{T}) where {T <: RingElement}
   check_parent(a, b)
   iszero(b) && error("Division by zero in divides")
   if iszero(a)
      return true, a 
   end
   A = data(a)
   B = data(b)
   R = parent(a)
   m = modulus(R)
   gb = gcd(B, m)
   ub = divexact(B, gb)
   q, r = divrem(A, gb)
   if !iszero(r)
     return false, b
   end
   _, x = ppio(m, ub)
   rs = R(q*invmod(ub, x))
   return true, rs
end

###############################################################################
#
#   GCD
#
###############################################################################

doc"""
    gcd{T <: RingElement}(a::Nemo.ResElem{T}, b::Nemo.ResElem{T})
> Return a greatest common divisor of $a$ and $b$ if one exists. This is done
> by taking the greatest common divisor of the data associated with the
> supplied residues and taking its greatest common divisor with the modulus.
"""
function gcd(a::Nemo.ResElem{T}, b::Nemo.ResElem{T}) where {T <: RingElement}
   check_parent(a, b)
   return parent(a)(gcd(gcd(data(a), modulus(a)), data(b)))
end

###############################################################################
#
#   Unsafe functions
#
###############################################################################

function zero!(a::Nemo.ResElem{T}) where {T <: RingElement}
   a.data = zero!(a.data)
   return a
end

function mul!(c::Nemo.ResElem{T}, a::Nemo.ResElem{T}, b::Nemo.ResElem{T}) where {T <: RingElement}
   c.data = mod(data(a)*data(b), modulus(a))
   return c
end

function addeq!(c::Nemo.ResElem{T}, a::Nemo.ResElem{T}) where {T <: RingElement}
   c.data = mod(c.data + data(a), modulus(a))
   return c
end

function add!(c::Nemo.ResElem{T}, a::Nemo.ResElem{T}, b::Nemo.ResElem{T}) where {T <: RingElement}
   c.data = mod(data(a) + data(b), modulus(a))
   return c
end

###############################################################################
#
#   Random functions
#
###############################################################################

function rand(S::Nemo.ResRing{T}, v...) where {T <: RingElement}
   R = base_ring(S)
   return S(rand(R, v...))
end

###############################################################################
#
#   Promotion rules
#
###############################################################################

promote_rule(::Type{Res{T}}, ::Type{Res{T}}) where T <: RingElement = Res{T}

function promote_rule(::Type{Res{T}}, ::Type{U}) where {T <: RingElement, U <: RingElement}
   promote_rule(T, U) == T ? Res{T} : Union{}
end

###############################################################################
#
#   Parent object call overloading
#
###############################################################################

function (a::ResRing{T})(b::RingElement) where {T <: RingElement}
   return a(base_ring(a)(b))
end

function (a::ResRing{T})() where {T <: RingElement}
   z = Res{T}(zero(base_ring(a)))
   z.parent = a
   return z
end

function (a::ResRing{T})(b::Integer) where {T <: RingElement}
   z = Res{T}(mod(base_ring(a)(b), modulus(a)))
   z.parent = a
   return z
end

function (a::ResRing{T})(b::T) where {T <: RingElem}
   base_ring(a) != parent(b) && error("Operation on incompatible objects")
   z = Res{T}(mod(b, modulus(a)))
   z.parent = a
   return z
end

function (a::ResRing{T})(b::Nemo.ResElem{T}) where {T <: RingElement}
   a != parent(b) && error("Operation on incompatible objects")
   return b
end

###############################################################################
#
#   ResidueRing constructor
#
###############################################################################

doc"""
    ResidueRing{T <: RingElement}(R::Nemo.Ring, a::RingElement; cached::Bool=true)
> Create the residue ring $R/(a)$ where $a$ is an element of the ring $R$. We
> require $a \neq 0$. If `cached == true` (the default) then the resulting
> residue ring parent object is cached and returned for any subsequent calls
> to the constructor with the same base ring $R$ and element $a$.
"""
function ResidueRing(R::Nemo.Ring, a::RingElement; cached::Bool = true)
   iszero(a) && throw(DivideError())
   T = elem_type(R)

   return ResRing{T}(R(a), cached)
end
