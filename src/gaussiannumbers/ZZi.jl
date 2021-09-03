export fmpzi, ZZi

include("fmpz_extras.jl")

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

###############################################################################
#
#   Constructors
#
###############################################################################

function fmpzi()
  return fmpzi(fmpz(), fmpz())
end

function fmpzi(a::Union{Integer, fmpz})
  return fmpzi(fmpz(a), fmpz(0))
end

function (a::FlintZZiRing)(b::Union{Integer, fmpz})
  return fmpzi(fmpz(b), fmpz(0))
end

function (a::FlintZZiRing)(b::Union{Integer, fmpz}, c::Union{Integer, fmpz})
  return fmpzi(fmpz(b), fmpz(c))
end

function (R::FlintZZiRing)(a::Complex{T}) where T <: Integer
  return ZZi(fmpz(real(a)), fmpz(imag(a)))
end

function *(a::fmpz, b::Complex{T}) where T <: Integer
  return fmpzi(a*real(b), a*imag(b))
end

function *(b::Complex{T}, a::fmpz) where T <: Integer
  return fmpzi(a*real(b), a*imag(b))
end

function +(a::fmpz, b::Complex{T}) where T <: Integer
  return fmpzi(a + real(b), imag(b))
end

function +(b::Complex{T}, a::fmpz) where T <: Integer
  return fmpzi(a + real(b), imag(b))
end

function -(a::fmpz, b::Complex{T}) where T <: Integer
  return fmpzi(a - real(b), -imag(b))
end

function -(b::Complex{T}, a::fmpz) where T <: Integer
  return fmpzi(real(b) - a, imag(b))
end

###############################################################################
#
#   Parent object call overloads
#
###############################################################################

function (a::FlintIntegerRing)(b::fmpzi)
  iszero(b.y) || error("cannot coerce")
  return deepcopy(b.x)
end

function (a::FlintZZiRing)(b::fmpzi)
  return b
end

###############################################################################
#
#   Conversions and promotions
#
###############################################################################

function Base.convert(::Type{Complex{T}}, a::fmpzi) where T <: Integer
  return Complex{T}(Base.convert(T, real(a)), Base.convert(T, imag(a)))
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
#   Basic manipulation
#
###############################################################################

function real(a::fmpzi)
  return deepcopy(a.x)
end

function imag(a::fmpzi)
  return deepcopy(a.y)
end

function conj(a::fmpzi)
  return fmpzi(deepcopy(a.x), -a.y)
end

function abs2(a::fmpzi)
  return a.x^2 + a.y^2
end

function zero(a::FlintZZiRing)
  return fmpzi(fmpz(0), fmpz(0))
end

function one(a::FlintZZiRing)
  return fmpzi(fmpz(1), fmpz(0))
end

function iszero(a::fmpzi)
  return iszero(a.x) && iszero(a.y)
end

function isone(a::fmpzi)
  return isone(a.x) && iszero(a.y)
end

function nbits(a::fmpzi)
  return nbits(a.x) + nbits(a.y)
end

function ==(a::fmpzi, b::fmpzi)
  return a.x == b.x && a.y == b.y
end

function zero!(z::fmpzi)
  zero!(z.x)
  zero!(z.y)
  return z
end

function one!(z::fmpzi)
  one!(z.x)
  zero!(z.y)
  return z
end

function set!(z::fmpzi, a::fmpzi)
  set!(z.x, a.x)
  set!(z.y, a.y)
  return z
end

function swap!(a::fmpzi, b::fmpzi)
  swap!(a.x, b.x)
  swap!(a.y, b.y)
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
# addition, subtraction, multiplication
#
###############################################################################

function add!(z::fmpzi, a::fmpzi, b::fmpzi)
  add!(z.x, a.x, b.x)
  add!(z.y, a.y, b.y)
  return z
end

function add!(z::fmpzi, a::fmpzi, b::Union{Integer, fmpz})
  add!(z.x, a.x, b)
  set!(z.y, a.y)
  return z
end

function add!(z::fmpzi, b::Union{Integer, fmpz}, a::fmpzi)
  return add!(z, a, b)
end

function +(a::fmpzi, b::Union{Integer, fmpz, fmpzi})
  return add!(fmpzi(), a, b)
end

function +(a::Union{Integer, fmpz}, b::fmpzi)
  return add!(fmpzi(), a, b)
end


function sub!(z::fmpzi, a::fmpzi, b::fmpzi)
  sub!(z.x, a.x, b.x)
  sub!(z.y, a.y, b.y)
  return z
end

function sub!(z::fmpzi, a::fmpzi, b::Union{Integer, fmpz})
  sub!(z.x, a.x, b)
  set!(z.y, a.y)
  return z
end

function sub!(z::fmpzi, a::Union{Integer, fmpz}, b::fmpzi)
  sub!(z.x, a, b.x)
  neg!(z.y, b.y)
  return z
end

function -(a::fmpzi, b::Union{Integer, fmpz, fmpzi})
  return sub!(fmpzi(), a, b)
end

function -(a::Union{Integer, fmpz}, b::fmpzi)
  return sub!(fmpzi(), a, b)
end


function neg!(z::fmpzi, a::fmpzi)
  neg!(z.x, a.x)
  neg!(z.y, a.y)
  return z
end

function -(a::fmpzi)
  return neg!(fmpzi(), a)
end


# output does not alias input
function _muleq!(z::fmpzi, b::fmpzi)
  zx = submul!(mul!(fmpz(), z.x, b.x), z.y, b.y)
  mul!(z.y, z.y, b.x)
  addmul!(z.y, z.x, b.y)
  swap!(z.x, zx)
  return z
end

function muleq!(z::fmpzi, b::fmpzi)
  if z !== b
    return _muleq!(z, b)
  else
    zx = submul!(mul!(fmpz(), z.x, b.x), z.y, b.y)
    zy = addmul!(mul!(fmpz(), z.y, b.x), z.x, b.y)
    swap!(z.x, zx)
    swap!(z.y, zy)
    return z
  end
end

# output does not alias either input
function _mul!(z::fmpzi, a::fmpzi, b::fmpzi)
  mul!(z.x, a.x, b.x)
  submul!(z.x, a.y, b.y)
  mul!(z.y, a.y, b.x)
  addmul!(z.y, a.x, b.y)
  return z
end

function mul!(z::fmpzi, a::fmpzi, b::fmpzi)
  if z !== a
    if z !== b
      return _mul!(z, a, b)
    else
      return _muleq!(z, a)
    end
  else
    return muleq!(z, b)
  end
end

function mul!(z::fmpzi, a::fmpzi, b::Union{Integer, fmpz})
  mul!(z.x, a.x, b)
  mul!(z.y, a.y, b)
  return z
end

function mul!(z::fmpzi, a::Union{Integer, fmpz}, b::fmpzi)
  return mul!(z, b, a)
end

function *(a::fmpzi, b::fmpzi)
  return _mul!(fmpzi(), a, b)
end

function *(a::fmpzi, b::Union{Integer, fmpz})
  return mul!(fmpzi(), a, b)
end

function *(a::Union{Integer, fmpz}, b::fmpzi)
  return mul!(fmpzi(), a, b)
end


function addmul!(z::fmpzi, a::fmpzi, b::fmpz)
  addmul!(z.x, a.x, b)
  addmul!(z.y, a.y, b)
  return z
end

function addmul!(z::fmpzi, a::fmpzi, b::fmpzi)
  if z !== a && z !== b
    addmul!(z.x, a.x, b.x)
    submul!(z.x, a.y, b.y)
    addmul!(z.y, a.y, b.x)
    addmul!(z.y, a.x, b.y)
    return z
  else
    return addmul!(z, a, b, fmpzi())
  end
end

function addmul!(z::fmpzi, a::fmpzi, b::fmpzi, t::fmpzi)
  _mul!(t, a, b)
  return add!(z, z, t)
end


function submul!(z::fmpzi, a::fmpzi, b::fmpz)
  submul!(z.x, a.x, b)
  submul!(z.y, a.y, b)
  return z
end

function submul!(z::fmpzi, a::fmpzi, b::fmpzi)
  if z !== a && z !== b
    submul!(z.x, a.x, b.x)
    addmul!(z.x, a.y, b.y)
    submul!(z.y, a.x, b.y)
    submul!(z.y, a.y, b.x)
    return z
  else
    return submul!(z, a, b, fmpzi())
  end
end

function submul!(z::fmpzi, a::fmpzi, b::fmpzi, t::fmpzi)
  _mul!(t, a, b)
  return sub!(z, z, t)
end

###############################################################################
#
# division
#
###############################################################################

function divrem(a::fmpzi, b::fmpz)
  qx, rx = ndivrem(a.x, b)
  qy, ry = ndivrem(a.y, b)
  return fmpzi(qx, qy), fmpzi(rx, ry)
end

function divrem(a::fmpz, b::fmpzi)
  d = abs2(b)
  qx, r = ndivrem(a*b.x, d)
  qy, r = ndivrem(-a*b.y, d)
  q = fmpzi(qx, qy)
  return q, a - q*b
end

function divrem(a::fmpzi, b::fmpzi)
  d = abs2(b)
  qx, r = ndivrem(a.x*b.x + a.y*b.y, d)
  qy, r = ndivrem(a.y*b.x - a.x*b.y, d)
  q = fmpzi(qx, qy)
  return q, a - q*b
end

function div(a::fmpzi, b::fmpzi)
  return divrem(a, b)[1]
end

function rem(a::fmpzi, b::fmpzi)
  return divrem(a, b)[2]
end

function mod(a::fmpzi, b::fmpzi)
  return divrem(a, b)[2]
end

function divides(a::fmpzi, b::Union{fmpz, fmpzi})
  q, r = divrem(a, b)
  return iszero(r), q
end

function divexact!(z::fmpzi, a::fmpzi, b::Union{Integer, fmpz})
  divexact!(z.x, a.x, b)
  divexact!(z.y, a.y, b)
  return z
end

function divexact!(z::fmpzi, a::fmpzi, b::fmpzi)
  A = a.x*b.x + a.y*b.y
  B = a.y*b.x - a.x*b.y
  C = abs2(b)
  divexact!(z.x, A, C)
  divexact!(z.y, B, C)
  return z
end

function divexact(a::fmpzi, b::Union{Integer, fmpz, fmpzi}; check=true)
  if check
    ok, q = divides(a, b)
    ok || throw("non-exact division")
    return q
  else
    return divexact!(fmpzi(), a, b)
  end
end

function isunit(a::fmpzi)
  return iszero(a.y) && isunit(a.x) || iszero(a.x) && isunit(a.y)
end

function inv(a::fmpzi)
  isunit(a) || error("not invertible")
  return fmpzi(deepcopy(a.x), -a.y)
end


###############################################################################
#
# powering
#
###############################################################################

function sqr!(z::fmpzi, a::fmpzi, t::fmpzi)
  mul!(t.x, a.x, a.x)
  mul!(t.y, a.y, a.y)
  nx = size(a.x)
  ny = size(a.y)
  if 5 < nx < 2*ny && 5 < ny < 2*nx
    # (x+y)^2-x^2-y^2
    add!(z.x, a.x, a.y)
    mul!(z.y, z.x, z.x)
    sub!(z.y, z.y, t.x)
    sub!(z.y, z.y, t.y)
    # x^2-y^2
    sub!(z.x, t.x, t.y)
  else
    mul!(z.y, a.x, a.y)
    sub!(z.x, t.x, t.y)
    ccall((:fmpz_mul_2exp, libflint), Nothing,
          (Ref{fmpz}, Ref{fmpz}, UInt),
          z.y, z.y, 1)
  end
  return z
end

function _pow!(z::fmpzi, Z::fmpzi, n::UInt)
  if n < 2
    @assert n == 1
    return set!(z, Z)
  end
  t = fmpzi()
  while iseven(n)
    sqr!(z, Z, t); Z = z
    n >>= 1
  end
  if n > 1
    x = fmpzi()
    X = Z
    while !iszero(n >>= 1)
      sqr!(x, X, t); X = x
      if isodd(n)
        _mul!(t, Z, x)
        swap!(z, t); Z = z
      end
    end
  end
  return z
end

function pow!(z::fmpzi, a::fmpzi, n::Union{Int, UInt})
  if n < 0
    return _pow!(z, inv!(z, a), (-n)%UInt)
  elseif n > 0
    return _pow!(z, a, (+n)%UInt)
  else
    return one!(z)
  end
end

function ^(a::fmpzi, n::Union{Int, UInt})
  return pow!(fmpzi(), a, n)
end

###############################################################################
#
# gcd
#
###############################################################################

gcd(a::fmpzi, b::fmpz) = gcd(a, fmpzi(b))
gcd(a::fmpz, b::fmpzi) = gcd(fmpzi(a), b)

function gcd(a::fmpzi, b::fmpzi)
  while !iszero(b)
    (a, b) = (b, mod(a, b))
  end
  return divexact(a, canonical_unit(a))
end

function gcdx(a::fmpzi, b::fmpzi)
  R = parent(a)
  if iszero(a)
    if iszero(b)
      return zero(R), zero(R), zero(R)
    else
      d = canonical_unit(b)
      return divexact(b, d), zero(R), inv(d)
    end
  elseif iszero(b)
    d = canonical_unit(a)
    return divexact(a, d), inv(d), zero(R)
  end
  u1, u2 = one(R), zero(R)
  v1, v2 = zero(R), one(R)
  while !iszero(b)
    (q, b), a = divrem(a, b), b
    u2, u1 = u1 - q*u2, u2
    v2, v1 = v1 - q*v2, v2
  end
  d = canonical_unit(a)
  return divexact(a, d), divexact(u1, d), divexact(v1, d)
end


###############################################################################
#
# factor
#
###############################################################################

# a[b] += c
function addeqindex!(a::Fac, c::Int, b)
  c > 0 || return
  setindex!(a.fac, c + get(a.fac, b, 0), b)
end

function _sum_of_squares(p::fmpz)
  @assert isone(mod(p, UInt(4)))
  x = fmpz(2542238945270620497)
  while !isdivisible_by(x^2+1, p)
    x = powermod(rand(fmpz(2):(p-2)), div(p-1,4), p)
  end
  return gcd(fmpzi(x, 1), p)
end

function factor(a::fmpzi)
  f = Fac{fmpzi}()
  g = gcd(a.x, a.y)
  f.unit = divexact(a, g)   # throw if a=0
  c = fmpzi(1, 1)
  for (p, e) in factor(g)
    if isone(mod(p, UInt(4)))
      c1 = _sum_of_squares(p)
      addeqindex!(f, e, c1)
      addeqindex!(f, e, conj(c1))
    elseif p == 2
      addeqindex!(f, 2*e, c)
      mul_i_pow!(f.unit, -e)
    else
      setindex!(f, e, fmpzi(p, 0))
    end
  end
  if mod(f.unit.x, UInt(2)) == mod(f.unit.y, UInt(2))
    f.unit = divexact(f.unit, c)
    addeqindex!(f, 1, c)
  end
  for (p, e) in factor(abs2(f.unit))
    c1 = _sum_of_squares(p)
    if !isdivisible_by(f.unit, c1)
      neg!(c1.y, c1.y)
    end
    f.unit = divexact(f.unit, c1^e)
    addeqindex!(f, e, c1)
  end
  @assert isunit(f.unit)
  return f
end

