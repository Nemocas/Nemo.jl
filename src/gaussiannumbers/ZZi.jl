###############################################################################
#
#   Data type and parent methods
#
###############################################################################

function elem_type(::Type{FlintZZiRing})
  return fmpzi
end

function parent_type(::Type{fmpzi})
  return FlintZZiRing
end

function parent(::fmpzi)
  return ZZi
end

###############################################################################
#
#   Basic manipulation
#
###############################################################################

function real(a::fmpzi)
  return deepcopy(a.x)
end

function imag(a::fmpzi)
  return deepcopy(a.y)
end

###############################################################################
#
#   Hashing
#
###############################################################################

function Base.hash(a::fmpzi, h::UInt)
  return hash(a.x, xor(hash(a.y, h), 0x94405bdfac6c8acd%UInt))
end

###############################################################################
#
#   Canonicalisation
#
###############################################################################

# return k with canonical_unit(a) = i^-k
function canonical_unit_i_pow(a::fmpzi)
  s = cmp(a.x, a.y)
  if s == 0
    t = cmp(a.x, 0)
    return t < 0 ? 2 : 0
  else
    t = cmpabs(a.x, a.y)
    if s > 0
      return t <= 0 ? 1 : 0
    else
      return t <= 0 ? 3 : 2
    end
  end
end

function mul_i_pow!(z::fmpzi, k::Int)
  k = mod(k%UInt, 4)
  if k == 1
    neg!(z.y, z.y)
    swap!(z.x, z.y)
  elseif k == 2
    neg!(z.x, z.x)
    neg!(z.y, z.y)
  elseif k == 3
    neg!(z.x, z.x)
    swap!(z.x, z.y)
  end
  return z
end

# for -pi/4 < angle(a/canonical_unit(a)) <= pi/4
function canonical_unit(a::fmpzi)
  k = canonical_unit_i_pow(a)
  if k == 0
    return fmpzi(1,0)
  elseif k == 1
    return fmpzi(0,-1)
  elseif k == 2
    return fmpzi(-1,0)
  else
    return fmpzi(0,1)
  end
end

function unit_canonicalize!(z::fmpzi)
  return mul_i_pow!(z, canonical_unit_i_pow(z))
end

###############################################################################
#
#   AbstractString I/O
#
###############################################################################

function expressify(a::fmpzi; context = nothing)
  return Expr(:call, :+, a.x, Expr(:call, :*, a.y, :im))
end

function Base.show(io::IO, a::fmpzi)
  AbstractAlgebra.show_via_expressify(io, a)
end

function Base.show(io::IO, a::FlintZZiRing)
  print(io, "ZZ[i]")
end

