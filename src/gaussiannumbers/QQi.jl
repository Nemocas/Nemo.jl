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
#   Constructors
#
###############################################################################

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
#   Canonicalisation
#
###############################################################################

canonical_unit(a::fmpqi) = a

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

