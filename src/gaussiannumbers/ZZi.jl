###############################################################################
#
#   Data type and parent methods
#
###############################################################################

elem_type(::Type{ZZiRing}) = ZZiRingElem

parent_type(::Type{ZZiRingElem}) = ZZiRing

parent(a::ZZiRingElem) = ZZi

base_ring_type(::Type{ZZiRing}) = ZZRing

base_ring(a::ZZiRing) = ZZ

is_domain_type(::Type{ZZiRingElem}) = true

characteristic(a::ZZiRing) = 0

###############################################################################
#
#   AbstractString I/O
#
###############################################################################

function expressify(a::ZZiRingElem; context = nothing)
  return Expr(:call, :+, real(a), Expr(:call, :*, a.y, :im))
end

function Base.show(io::IO, a::ZZiRingElem)
  AbstractAlgebra.show_via_expressify(io, a)
end

function Base.show(io::IO, a::ZZiRing)
  # deliberately no @show_name or @show_special here as this is a singleton type
  if is_terse(io)
    io = pretty(io)
    print(io, LowercaseOff(), "ZZ[im]")
  else
    io = pretty(io)
    print(io, LowercaseOff(), "Gaussian integer ring")
  end
end

###############################################################################
#
#   Constructors
#
###############################################################################

function ZZiRingElem()
  return ZZiRingElem(ZZRingElem(), ZZRingElem())
end

function ZZiRingElem(a::IntegerUnion)
  return ZZiRingElem(ZZRingElem(a), ZZRingElem(0))
end

function (a::ZZiRing)()
  return ZZiRingElem()
end

function (a::ZZiRing)(b::IntegerUnion)
  return ZZiRingElem(ZZRingElem(b), ZZRingElem(0))
end

function (a::ZZiRing)(b::IntegerUnion, c::IntegerUnion)
  return ZZiRingElem(ZZRingElem(b), ZZRingElem(c))
end

function (R::ZZiRing)(a::Complex{T}) where T <: Integer
  return ZZi(ZZRingElem(real(a)), ZZRingElem(imag(a)))
end

###############################################################################
#
#   Parent object call overloads
#
###############################################################################

function (a::ZZRing)(b::ZZiRingElem)
  iszero(b.y) || error("cannot coerce")
  return real(b)
end

function (a::ZZiRing)(b::ZZiRingElem)
  return b
end

###############################################################################
#
#   Conversions
#
###############################################################################

# see adhoc section for promotions

function Base.convert(::Type{Complex{T}}, a::ZZiRingElem) where T <: Integer
  return Complex{T}(Base.convert(T, real(a)), Base.convert(T, imag(a)))
end

function Base.convert(::Type{ZZiRingElem}, a::Complex{T}) where T <: Integer
  return ZZiRingElem(convert(ZZRingElem, real(a)), convert(ZZRingElem, imag(a)))
end

function Base.convert(::Type{ZZiRingElem}, a::IntegerUnion)
  return ZZiRingElem(convert(ZZRingElem, a), ZZRingElem(0))
end

###############################################################################
#
#   Hashing
#
###############################################################################

function Base.hash(a::ZZiRingElem, h::UInt)
  return hash(real(a), xor(hash(a.y, h), 0x94405bdfac6c8acd%UInt))
end

###############################################################################
#
#   Random generation
#
###############################################################################

function rand_bits(::ZZiRing, b::Int)
  t = rand(0:b)
  return ZZiRingElem(rand_bits(ZZ, t), rand_bits(ZZ, b - t))
end

###############################################################################
#
#   Conformance test element generation
#
###############################################################################

function ConformanceTests.generate_element(R::Nemo.ZZiRing)
  return rand_bits(R, rand(0:200))
end

###############################################################################
#
#   Basic manipulation
#
###############################################################################

# ???
function deepcopy_internal(a::ZZiRingElem, d::IdDict)
  return ZZiRingElem(deepcopy_internal(real(a), d), deepcopy_internal(a.y, d))
end

function deepcopy_internal(a::ZZiRing, d::IdDict)
  return a
end

function real(a::ZZiRingElem)
  return a.x
end

function imag(a::ZZiRingElem)
  return a.y
end

function conj(a::ZZiRingElem)
  return ZZiRingElem(real(a), -a.y)
end

function abs2(a::ZZiRingElem)
  return real(a)^2 + a.y^2
end

function zero(a::ZZiRing)
  return ZZiRingElem(ZZRingElem(0), ZZRingElem(0))
end

function one(a::ZZiRing)
  return ZZiRingElem(ZZRingElem(1), ZZRingElem(0))
end

function iszero(a::ZZiRingElem)
  return iszero(real(a)) && iszero(a.y)
end

function isone(a::ZZiRingElem)
  return isone(real(a)) && iszero(a.y)
end

function nbits(a::ZZiRingElem)
  return nbits(real(a)) + nbits(a.y)
end

function zero!(z::ZZiRingElem)
  zero!(real(z))
  zero!(z.y)
  return z
end

function one!(z::ZZiRingElem)
  one!(real(z))
  zero!(z.y)
  return z
end

function set!(z::ZZiRingElem, a::ZZiRingElem)
  set!(real(z), real(a))
  set!(z.y, a.y)
  return z
end

function swap!(a::ZZiRingElem, b::ZZiRingElem)
  swap!(real(a), real(b))
  swap!(a.y, b.y)
end

###############################################################################
#
#   Canonicalisation
#
###############################################################################

# return k with canonical_unit(a) = i^-k
function canonical_unit_i_pow(a::ZZiRingElem)
  s = cmp(real(a), a.y)
  if s == 0
    t = cmp(real(a), 0)
    return t < 0 ? 2 : 0
  else
    t = cmpabs(real(a), a.y)
    if s > 0
      return t <= 0 ? 1 : 0
    else
      return t <= 0 ? 3 : 2
    end
  end
end

function mul_i_pow!(z::ZZiRingElem, k::Int)
  k = mod(k%UInt, 4)
  if k == 1
    neg!(z.y, z.y)
    swap!(real(z), z.y)
  elseif k == 2
    neg!(real(z), real(z))
    neg!(z.y, z.y)
  elseif k == 3
    neg!(real(z), real(z))
    swap!(real(z), z.y)
  end
  return z
end

# for -pi/4 < angle(a/canonical_unit(a)) <= pi/4
function canonical_unit(a::ZZiRingElem)
  k = canonical_unit_i_pow(a)
  if k == 0
    return ZZiRingElem(1,0)
  elseif k == 1
    return ZZiRingElem(0,-1)
  elseif k == 2
    return ZZiRingElem(-1,0)
  else
    return ZZiRingElem(0,1)
  end
end

###############################################################################
#
#   equality
#
###############################################################################

function ==(a::ZZiRingElem, b::ZZiRingElem)
  return real(a) == real(b) && a.y == b.y
end

function ==(a::ZZiRingElem, b::Complex{T}) where T <: Integer
  return real(a) == real(b) && a.y == imag(b)
end

function ==(b::Complex{T}, a::ZZiRingElem) where T <: Integer
  return a == b
end

function ==(a::ZZiRingElem, b::IntegerUnion)
  return iszero(a.y) && real(a) == b
end

function ==(b::IntegerUnion, a::ZZiRingElem)
  return a == b
end

###############################################################################
#
#   addition, subtraction, multiplication
#
###############################################################################

function add!(z::ZZiRingElem, a::ZZiRingElem, b::ZZiRingElem)
  add!(real(z), real(a), real(b))
  add!(z.y, a.y, b.y)
  return z
end

function add!(z::ZZiRingElem, a::ZZiRingElem, b::IntegerUnion)
  add!(real(z), real(a), b)
  set!(z.y, a.y)
  return z
end

function add!(z::ZZiRingElem, b::IntegerUnion, a::ZZiRingElem)
  return add!(z, a, b)
end

function +(a::ZZiRingElem, b::Union{Integer, ZZRingElem, ZZiRingElem})
  return add!(ZZiRingElem(), a, b)
end

function +(a::IntegerUnion, b::ZZiRingElem)
  return add!(ZZiRingElem(), a, b)
end


function sub!(z::ZZiRingElem, a::ZZiRingElem, b::ZZiRingElem)
  sub!(real(z), real(a), real(b))
  sub!(z.y, a.y, b.y)
  return z
end

function sub!(z::ZZiRingElem, a::ZZiRingElem, b::IntegerUnion)
  sub!(real(z), real(a), b)
  set!(z.y, a.y)
  return z
end

function sub!(z::ZZiRingElem, a::IntegerUnion, b::ZZiRingElem)
  sub!(real(z), a, real(b))
  neg!(z.y, b.y)
  return z
end

function -(a::ZZiRingElem, b::Union{Integer, ZZRingElem, ZZiRingElem})
  return sub!(ZZiRingElem(), a, b)
end

function -(a::IntegerUnion, b::ZZiRingElem)
  return sub!(ZZiRingElem(), a, b)
end


function neg!(z::ZZiRingElem, a::ZZiRingElem)
  neg!(real(z), real(a))
  neg!(z.y, a.y)
  return z
end

function -(a::ZZiRingElem)
  return neg!(ZZiRingElem(), a)
end

# output does not alias input
function _muleq!(z::ZZiRingElem, b::ZZiRingElem)
  zx = submul!(mul!(ZZRingElem(), real(z), real(b)), z.y, b.y)
  mul!(z.y, z.y, real(b))
  addmul!(z.y, real(z), b.y)
  swap!(real(z), zx)
  return z
end

function muleq!(z::ZZiRingElem, b::ZZiRingElem)
  if z !== b
    return _muleq!(z, b)
  else
    zx = submul!(mul!(ZZRingElem(), real(z), real(b)), z.y, b.y)
    zy = addmul!(mul!(ZZRingElem(), z.y, real(b)), real(z), b.y)
    swap!(real(z), zx)
    swap!(z.y, zy)
    return z
  end
end

# output does not alias either input
function _mul!(z::ZZiRingElem, a::ZZiRingElem, b::ZZiRingElem)
  mul!(real(z), real(a), real(b))
  submul!(real(z), a.y, b.y)
  mul!(z.y, a.y, real(b))
  addmul!(z.y, real(a), b.y)
  return z
end

function mul!(z::ZZiRingElem, a::ZZiRingElem, b::ZZiRingElem)
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

function mul!(z::ZZiRingElem, a::ZZiRingElem, b::IntegerUnion)
  mul!(real(z), real(a), b)
  mul!(z.y, a.y, b)
  return z
end

function mul!(z::ZZiRingElem, a::IntegerUnion, b::ZZiRingElem)
  return mul!(z, b, a)
end

function *(a::ZZiRingElem, b::ZZiRingElem)
  return _mul!(ZZiRingElem(), a, b)
end

function *(a::ZZiRingElem, b::IntegerUnion)
  return mul!(ZZiRingElem(), a, b)
end

function *(a::IntegerUnion, b::ZZiRingElem)
  return mul!(ZZiRingElem(), a, b)
end


function addmul!(z::ZZiRingElem, a::ZZiRingElem, b::ZZRingElem)
  addmul!(real(z), real(a), b)
  addmul!(z.y, a.y, b)
  return z
end

function addmul!(z::ZZiRingElem, a::ZZiRingElem, b::ZZiRingElem)
  if z !== a && z !== b
    addmul!(real(z), real(a), real(b))
    submul!(real(z), a.y, b.y)
    addmul!(z.y, a.y, real(b))
    addmul!(z.y, real(a), b.y)
    return z
  else
    return addmul!(z, a, b, ZZiRingElem())
  end
end

function submul!(z::ZZiRingElem, a::ZZiRingElem, b::ZZRingElem)
  submul!(real(z), real(a), b)
  submul!(z.y, a.y, b)
  return z
end

function submul!(z::ZZiRingElem, a::ZZiRingElem, b::ZZiRingElem)
  if z !== a && z !== b
    submul!(real(z), real(a), real(b))
    addmul!(real(z), a.y, b.y)
    submul!(z.y, real(a), b.y)
    submul!(z.y, a.y, real(b))
    return z
  else
    return submul!(z, a, b, ZZiRingElem())
  end
end

###############################################################################
#
#   division
#
###############################################################################

# for z = mod(a, b) choose the unique representative z so that
# the complex number z/b = x + i*y is in the following box
#     -1/2 <= x < 1/2
#     -1/2 <= y < 1/2

function divrem(a::ZZiRingElem, b::ZZRingElem)
  qx, rx = ncdivrem(real(a), b)
  qy, ry = ncdivrem(a.y, b)
  return ZZiRingElem(qx, qy), ZZiRingElem(rx, ry)
end

function divrem(a::ZZiRingElem, b::ZZiRingElem)
  d = abs2(b)
  qx, r = ncdivrem(real(a)*real(b) + a.y*b.y, d)
  qy, r = ncdivrem(a.y*real(b) - real(a)*b.y, d)
  q = ZZiRingElem(qx, qy)
  return q, a - q*b
end

divrem(a::ZZiRingElem, b::Integer) = divrem(a, ZZRingElem(b))

function Base.div(a::ZZiRingElem, b::ZZiRingElem)
  return divrem(a, b)[1]
end

function rem(a::ZZiRingElem, b::ZZiRingElem)
  return divrem(a, b)[2]
end

function mod(a::ZZiRingElem, b::ZZiRingElem)
  return divrem(a, b)[2]
end

function divides(a::ZZiRingElem, b::Union{Integer, ZZRingElem, ZZiRingElem})
  if iszero(b)
    return iszero(a), b
  end
  q, r = divrem(a, b)
  return iszero(r), q
end

function divexact!(z::ZZiRingElem, a::ZZiRingElem, b::IntegerUnion)
  divexact!(real(z), real(a), b)
  divexact!(z.y, a.y, b)
  return z
end

function divexact!(z::ZZiRingElem, a::ZZiRingElem, b::ZZiRingElem)
  A = real(a)*real(b) + a.y*b.y
  B = a.y*real(b) - real(a)*b.y
  C = abs2(b)
  divexact!(real(z), A, C)
  divexact!(z.y, B, C)
  return z
end

function divexact(a::ZZiRingElem, b::Union{Integer, ZZRingElem, ZZiRingElem}; check=true)
  if check
    ok, q = divides(a, b)
    ok || throw("non-exact division")
    return q
  else
    return divexact!(ZZiRingElem(), a, b)
  end
end

function is_unit(a::ZZiRingElem)
  return iszero(a.y) && is_unit(real(a)) || iszero(real(a)) && is_unit(a.y)
end

function inv(a::ZZiRingElem)
  is_unit(a) || error("not invertible")
  return ZZiRingElem(real(a), -a.y)
end

###############################################################################
#
#   powering
#
###############################################################################

function sqr!(z::ZZiRingElem, a::ZZiRingElem, t::ZZiRingElem)
  mul!(real(t), real(a), real(a))
  mul!(t.y, a.y, a.y)
  nx = size(real(a))
  ny = size(a.y)
  if 5 < nx < 2*ny && 5 < ny < 2*nx
    # (x+y)^2-x^2-y^2
    add!(real(z), real(a), a.y)
    mul!(z.y, real(z), real(z))
    sub!(z.y, z.y, real(t))
    sub!(z.y, z.y, t.y)
    # x^2-y^2
    sub!(real(z), real(t), t.y)
  else
    mul!(z.y, real(a), a.y)
    sub!(real(z), real(t), t.y)
    @ccall libflint.fmpz_mul_2exp(z.y::Ref{ZZRingElem}, z.y::Ref{ZZRingElem}, 1::UInt)::Nothing
  end
  return z
end

function _pow!(z::ZZiRingElem, Z::ZZiRingElem, n::UInt)
  if n < 2
    @assert n == 1
    return set!(z, Z)
  end
  t = ZZiRingElem()
  while iseven(n)
    sqr!(z, Z, t); Z = z
    n >>= 1
  end
  if n > 1
    x = ZZiRingElem()
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

function pow!(z::ZZiRingElem, a::ZZiRingElem, n::Union{Int, UInt})
  if n < 0
    return _pow!(z, inv(a), (-n)%UInt)
  elseif n > 0
    return _pow!(z, a, (+n)%UInt)
  else
    return one!(z)
  end
end

function ^(a::ZZiRingElem, n::Union{Int, UInt})
  return pow!(ZZiRingElem(), a, n)
end

###############################################################################
#
# generic functionality for Euclidean interface: why doesn't AA do this?
#
###############################################################################

function mulmod(a::ZZiRingElem, b::ZZiRingElem, c::ZZiRingElem)
  return mod(a*b, c)
end

function powermod(a::ZZiRingElem, b::Int, c::ZZiRingElem)
  if b < 0
    return mod(invmod(a,c)^((-b)%UInt), c)
  else
    return mod(a^b, c)
  end
end

function invmod(a::ZZiRingElem, b::ZZiRingElem)
  g, x, y = gcdx(a, b)
  isone(g) || error("impossible inverse")
  return x
end

function remove(a::ZZiRingElem, b::ZZiRingElem)
  if (iszero(b) || is_unit(b))
    throw(ArgumentError("Second argument must be a non-zero non-unit"))
  end
  if iszero(a)
    return (0, zero(parent(a))) # questionable case, consistent with ZZRingElem
  end
  v = 0
  while begin; (ok, q) = divides(a, b); ok; end
    a = q
    v += 1
  end
  return v, a
end

function valuation(a::ZZiRingElem, b::ZZiRingElem)
  return remove(a, b)[1]
end

function lcm(a::ZZiRingElem, b::ZZiRingElem)
  g = gcd(a, b)
  iszero(g) && return g
  return a*divexact(b, g)
end

###############################################################################
#
#   gcd
#
###############################################################################

function _divexact(a, b)
  return isone(b) ? a : divexact(a, b; check = false)
end

function smod(a::ZZRingElem, b::ZZRingElem)
  z = ZZRingElem()
  @ccall libflint.fmpz_smod(z::Ref{ZZRingElem}, a::Ref{ZZRingElem}, b::Ref{ZZRingElem})::Nothing
  return z
end

function gcd(a::ZZiRingElem, b::ZZiRingElem)
  if iszero(b.y)
    return gcd(a, real(b))
  elseif iszero(real(b))
    return gcd(a, b.y)
  elseif iszero(a.y)
    return gcd(b, real(a))
  elseif iszero(real(a))
    return gcd(b, a.y)
  end
  #  ax ay           g*1 g*B
  # -ay ax  row ops   0  g*A
  #  bx by  ------>   0   0
  # -by bx            0   0
  ga, ua, va = gcdx(real(a), a.y)
  gb, ub, vb = gcdx(real(b), b.y)
  g, u, v = gcdx(ga, gb)
  axog = _divexact(real(a), g)
  ayog = _divexact(a.y, g)
  bxog = _divexact(real(b), g)
  byog = _divexact(b.y, g)
  m1 = ayog*ua - axog*va
  m2 = _divexact(real(a),ga)*axog + _divexact(a.y,ga)*ayog
  m3 = byog*ub - bxog*vb
  m4 = _divexact(real(b),gb)*bxog + _divexact(b.y,gb)*byog
  (m1, m3) = (m1*u + m3*v, _divexact(ga,g)*m3 - _divexact(gb,g)*m1)
  A = gcd(m2, m3, m4)
  v, _ = _shortest_l_infinity(ZZRingElem(1), mod(m1, A), A)
  z = isone(g) ? ZZiRingElem(v[1], v[2]) : ZZiRingElem(v[1]*g, v[2]*g)
  return mul_i_pow!(z, canonical_unit_i_pow(z))
end

function gcd(a::ZZiRingElem, b::ZZRingElem)
  if iszero(b)
    return mul_i_pow!(deepcopy(a), canonical_unit_i_pow(a))
  end
  #  ax ay           g*1 g*B
  # -ay ax  row ops   0  g*A
  #  b  0   ------>   0   0
  #  0  b             0   0
  ax = cmpabs(real(a), b) < 0 ? real(a) : smod(real(a), b)
  ay = cmpabs(a.y, b) < 0 ? a.y : smod(a.y, b)
  if iszero(ax)
    return ZZiRingElem(gcd(ay, b), zero(ZZRingElem))
  elseif iszero(ay)
    return ZZiRingElem(gcd(ax, b), zero(ZZRingElem))
  end
  ga, ua, va = gcdx(ax, ay)
  g, u, _ = gcdx(ga, b)
  axog = _divexact(ax, g)
  ayog = _divexact(ay, g)
  m1 = ayog*ua - axog*va
  m2 = _divexact(ax,ga)*axog + _divexact(ay,ga)*ayog
  A = gcd(m2, _divexact(b, g))
  v, _ = _shortest_l_infinity(ZZRingElem(1), mod(m1*u, A), A)
  z = isone(g) ? ZZiRingElem(v[1], v[2]) : ZZiRingElem(v[1]*g, v[2]*g)
  return mul_i_pow!(z, canonical_unit_i_pow(z))
end

function gcd(a::ZZiRingElem, b::Integer)
  return gcd(a, ZZRingElem(b))
end

function gcd(b::Union{ZZRingElem, Integer}, a::ZZiRingElem)
  return gcd(a, ZZRingElem(b))
end

function gcdx(a::ZZiRingElem, b::ZZiRingElem)
  if iszero(a)
    if iszero(b)
      return (zero(ZZi), zero(ZZi), zero(ZZi))
    else
      u = canonical_unit(b)
      return (divexact(b, u), zero(ZZi), inv(u))
    end
  elseif iszero(b)
    u = canonical_unit(a)
    return (divexact(a, u), inv(u), zero(ZZi))
  end
  m = zero_matrix(ZZ, 4, 2)
  m[1,1] =  real(a); m[1,2] = a.y
  m[2,1] = -a.y; m[2,2] = real(a)
  m[3,1] =  real(b); m[3,2] = b.y
  m[4,1] = -b.y; m[4,2] = real(b)
  v, t = shortest_l_infinity_with_transform(m)
  z = ZZiRingElem(v[1], v[2])
  w1 = ZZiRingElem(t[1], t[2])
  w2 = ZZiRingElem(t[3], t[4])
  k = canonical_unit_i_pow(z)
  g = mul_i_pow!(z,k)
  x = mul_i_pow!(w1,k)
  y = mul_i_pow!(w2,k)
  # at this point g = x*a + y*b and neither a nor b is zero
  # make sure x = mod(x, b/g) for our "canonical" gcdx
  # unexpected problem: q is quite large!
  q, x = divrem(x, divexact(b, g))
  y += q*divexact(a, g)
  return (g, x, y)
end

###############################################################################
#
#   factor
#
###############################################################################

# a[b] += c
function addeqindex!(a::Fac, c::Int, b)
  c > 0 || return
  setindex!(a.fac, c + get(a.fac, b, 0), b)
end

function _sum_of_squares(p::ZZRingElem)
  @assert isone(mod(p, UInt(4)))
  x = ZZRingElem(2542238945270620497)
  while !is_divisible_by(x^2+1, p)
    x = powermod(rand(ZZRingElem(2):(p-2)), div(p-1,4), p)
  end
  return gcd(ZZiRingElem(x, 1), p)
end

function factor(a::ZZiRingElem)
  iszero(a) && throw(ArgumentError("Argument must be non-zero"))
  f = Fac{ZZiRingElem}()
  g = gcd(real(a), a.y)
  f.unit = divexact(a, g)   # throw if a=0
  c = ZZiRingElem(1, 1)
  for (p, e) in factor(g)
    if isone(mod(p, UInt(4)))
      c1 = _sum_of_squares(p)
      addeqindex!(f, e, c1)
      addeqindex!(f, e, conj(c1))
    elseif p == 2
      addeqindex!(f, 2*e, c)
      mul_i_pow!(f.unit, -e)
    else
      setindex!(f, e, ZZiRingElem(p, 0))
    end
  end
  if mod(real(f.unit), UInt(2)) == mod(f.unit.y, UInt(2))
    f.unit = divexact(f.unit, c, check=false)
    addeqindex!(f, 1, c)
  end
  for (p, e) in factor(abs2(f.unit))
    c1 = _sum_of_squares(p)
    if !is_divisible_by(f.unit, c1)
      c1 = conj(c1)
    end
    f.unit = divexact(f.unit, c1^e)
    addeqindex!(f, e, c1)
  end
  @assert is_unit(f.unit)
  return f
end

###############################################################################
#
# adhoc
#
###############################################################################

# +,-,* operations defined above
promote_rule(a::Type{ZZiRingElem}, b::Type{ZZRingElem}) = ZZiRingElem
promote_rule(a::Type{ZZRingElem}, b::Type{ZZiRingElem}) = ZZiRingElem
promote_rule(a::Type{ZZiRingElem}, b::Type{<:Integer}) = ZZiRingElem
promote_rule(a::Type{<:Integer}, b::Type{ZZiRingElem}) = ZZiRingElem

promote_rule(a::Type{ZZRingElem}, b::Type{<:Complex{<:Integer}}) = ZZiRingElem
promote_rule(a::Type{<:Complex{<:Integer}}, b::Type{ZZRingElem}) = ZZiRingElem
*(a::ZZRingElem, b::Complex{<:Integer}) = ZZiRingElem(a*real(b), a*imag(b))
*(b::Complex{<:Integer}, a::ZZRingElem) = ZZiRingElem(a*real(b), a*imag(b))
+(a::ZZRingElem, b::Complex{<:Integer}) = ZZiRingElem(a + real(b), imag(b))
+(b::Complex{<:Integer}, a::ZZRingElem) = ZZiRingElem(a + real(b), imag(b))
-(a::ZZRingElem, b::Complex{<:Integer}) = ZZiRingElem(a - real(b), -imag(b))
-(b::Complex{<:Integer}, a::ZZRingElem) = ZZiRingElem(real(b) - a, imag(b))

promote_rule(a::Type{ZZiRingElem}, b::Type{<:Complex{<:Integer}}) = ZZiRingElem
promote_rule(a::Type{<:Complex{<:Integer}}, b::Type{ZZiRingElem}) = ZZiRingElem
*(a::ZZiRingElem, b::Complex{<:Integer}) = a*ZZi(b)
*(b::Complex{<:Integer}, a::ZZiRingElem) = a*ZZi(b)
+(a::ZZiRingElem, b::Complex{<:Integer}) = ZZiRingElem(real(a) + real(b), a.y + imag(b))
+(b::Complex{<:Integer}, a::ZZiRingElem) = ZZiRingElem(real(a) + real(b), a.y + imag(b))
-(a::ZZiRingElem, b::Complex{<:Integer}) = ZZiRingElem(real(a) - real(b), a.y - imag(b))
-(b::Complex{<:Integer}, a::ZZiRingElem) = ZZiRingElem(real(b) - real(a), imag(b) - a.y)

