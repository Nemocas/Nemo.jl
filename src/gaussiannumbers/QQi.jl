export fmpqi, QQi

###############################################################################
#
#   Data type and parent methods
#
###############################################################################

function elem_type(::Type{FlintQQiField})
  return fmpqi
end

function parent_type(::Type{fmpqi})
  return FlintQQiField
end

function parent(::fmpqi)
  return QQi
end

base_ring(a::FlintQQiField) = ZZi

base_ring(a::fmpqi) = ZZi

isdomain_type(::Type{fmpqi}) = true

###############################################################################
#
#   AbstractString I/O
#
###############################################################################

function expressify(a::fmpqi; context = nothing)
  return Expr(:call, :+, real(a), Expr(:call, :*, imag(a)))
end

function Base.show(io::IO, a::fmpqi)
  AbstractAlgebra.show_via_expressify(io, a)
end

function Base.show(io::IO, a::FlintQQiField)
  print(io, "QQ[i]")
end

###############################################################################
#
#   Constructors
#
###############################################################################

function fmpqi()
  return fmpqi(fmpzi())
end

function fmpqi(a::fmpzi)
  return fmpqi(a, fmpz(1))
end

function (a::FlintQQiField)(b::Union{Integer, fmpz, fmpzi})
  return fmpqi(ZZi(b))
end

function (a::FlintQQiField)(b::Union{Rational, fmpq})
  return fmpqi(ZZi(b), fmpz(denominator(b)))
end

function //(a::Union{Integer, fmpz, Rational, fmpq, fmpzi, fmpqi},
            b::Union{fmpzi, fmpqi})
  return divexact(QQi(a), QQi(b))
end

function //(a::Union{fmpzi, fmpqi},
            b::Union{Integer, fmpz, Rational, fmpq, fmpzi, fmpqi})
  return divexact(QQi(a), QQi(b))
end

###############################################################################
#
#   Parent object call overloads
#
###############################################################################

function (a::FlintZZiRing)(b::fmpqi)
  isone(b.den) || error("cannot coerce")
  return deepcopy(b.num)
end

function (a::FlintRationalField)
  iszero(b.num.y) || error("cannot coerce")
  return a.num.x//a.den
end

function (a::FlintIntegerRing)(b::fmpqi)
  iszero(b.num.y) && isone(b.den) || error("cannot coerce")
  return deepcopy(b.num.x)
end

###############################################################################
#
#   Conversions and promotions
#
###############################################################################

promote_rule(a::Type{fmpzi}, b::Type{fmpz}) = fmpzi
promote_rule(a::Type{fmpz}, b::Type{fmpzi}) = fmpzi
promote_rule(a::Type{fmpqi}, b::Type{fmpz}) = fmpqi
promote_rule(a::Type{fmpz}, b::Type{fmpqi}) = fmpqi
promote_rule(a::Type{fmpqi}, b::Type{fmpzi}) = fmpqi
promote_rule(a::Type{fmpzi}, b::Type{fmpqi}) = fmpqi
promote_rule(a::Type{fmpqi}, b::Type{fmpq}) = fmpqi
promote_rule(a::Type{fmpq}, b::Type{fmpqi}) = fmpqi

function Base.convert(::Type{Complex{Rational{T}}}, a::fmpqi) where T
  return Complex{Rational{T}}(Base.convert(Rational{T}, real(a)),
                              Base.convert(Rational{T}, real(b)))
end

###############################################################################
#
#   Hashing
#
###############################################################################

function Base.hash(a::fmpqi, h::UInt)
  return hash(a.num, xor(hash(a.den, h), 0x6edeadc6d0447c19%UInt))
end

###############################################################################
#
#   Basic manipulation
#
###############################################################################

function real(a::fmpqi)
  return a.num.x//a.den
end

function imag(a::fmpqi)
  return a.num.y//a.den
end

function abs2(a::fmpqi)
  return abs2(a.num)//a.den^2
end

function ==(a::fmpqi, b::fmpqi)
  return a.den == b.den && a.num == b.num
end

###############################################################################
#
#   Canonicalisation
#
###############################################################################

canonical_unit(a::fmpqi) = a

###############################################################################
#
#   addition, subtraction, multiplication
#
###############################################################################


