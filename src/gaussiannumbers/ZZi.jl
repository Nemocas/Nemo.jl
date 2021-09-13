export fmpzi, FlintZZi, FlintZZiRing

###############################################################################
#
#   Data type and parent methods
#
###############################################################################

elem_type(::Type{FlintZZiRing}) = fmpzi

parent_type(::Type{fmpzi}) = FlintZZiRing

parent(a::fmpzi) = FlintZZi

base_ring(a::FlintZZiRing) = FlintZZ

base_ring(a::fmpzi) = FlintZZ

isdomain_type(::Type{fmpzi}) = true

characteristic(a::FlintZZiRing) = 0

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
   print(io, "ZZ[im]")
end

###############################################################################
#
#   Constructors
#
###############################################################################

function fmpzi()
   return fmpzi(fmpz(), fmpz())
end

function fmpzi(a::IntegerUnion)
   return fmpzi(fmpz(a), fmpz(0))
end

function (a::FlintZZiRing)()
   return fmpzi()
end

function (a::FlintZZiRing)(b::IntegerUnion)
   return fmpzi(fmpz(b), fmpz(0))
end

function (a::FlintZZiRing)(b::IntegerUnion, c::IntegerUnion)
   return fmpzi(fmpz(b), fmpz(c))
end

function (R::FlintZZiRing)(a::Complex{T}) where T <: Integer
   return FlintZZi(fmpz(real(a)), fmpz(imag(a)))
end

###############################################################################
#
#   Parent object call overloads
#
###############################################################################

function (a::FlintIntegerRing)(b::fmpzi)
   iszero(b.y) || error("cannot coerce")
   return b.x
end

function (a::FlintZZiRing)(b::fmpzi)
   return b
end

###############################################################################
#
#   Conversions
#
###############################################################################

# see adhoc section for promotions

function Base.convert(::Type{Complex{T}}, a::fmpzi) where T <: Integer
   return Complex{T}(Base.convert(T, real(a)), Base.convert(T, imag(a)))
end

function Base.convert(::Type{fmpzi}, a::Complex{T}) where T <: Integer
   return fmpzi(convert(fmpz, real(a)), convert(fmpz, imag(a)))
end

function Base.convert(::Type{fmpzi}, a::IntegerUnion)
   return fmpzi(convert(fmpz, a), fmpz(0))
end

###############################################################################
#
#   Hashing
#
###############################################################################

function Base.hash(a::fmpzi, h::UInt)
   return hash(a.x, xor(hash(a.y, h), 0x94405bdfac6c8acd%UInt))
end

function Base.hash(a::FlintZZiRing)
   return 0xc76722b5285975da%UInt
end

###############################################################################
#
#   Random generation
#
###############################################################################

function rand_bits(::FlintZZiRing, b::Int)
   t = rand(0:b)
   return fmpzi(rand_bits(FlintZZ, t), rand_bits(FlintZZ, b - t))
end

###############################################################################
#
#   Basic manipulation
#
###############################################################################

# ???
function deepcopy_internal(a::fmpzi, d::IdDict)
   return fmpzi(deepcopy_internal(a.x, d), deepcopy_internal(a.y, d))
end

function deepcopy_internal(a::FlintZZiRing, d::IdDict)
   return a
end

function real(a::fmpzi)
   return a.x
end

function imag(a::fmpzi)
   return a.y
end

function conj(a::fmpzi)
   return fmpzi(a.x, -a.y)
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

###############################################################################
#
#   equality
#
###############################################################################

function ==(a::fmpzi, b::fmpzi)
   return a.x == b.x && a.y == b.y
end

function ==(a::fmpzi, b::Complex{T}) where T <: Integer
   return a.x == real(b) && a.y == imag(b)
end

function ==(b::Complex{T}, a::fmpzi) where T <: Integer
   return a == b
end

function ==(a::fmpzi, b::IntegerUnion)
   return iszero(a.y) && a.x == b
end

function ==(b::IntegerUnion, a::fmpzi)
   return a == b
end

###############################################################################
#
#   addition, subtraction, multiplication
#
###############################################################################

function addeq!(z::fmpzi, a::fmpzi)
   add!(z.x, z.x, a.x)
   add!(z.y, z.y, a.y)
   return z
end

function add!(z::fmpzi, a::fmpzi, b::fmpzi)
   add!(z.x, a.x, b.x)
   add!(z.y, a.y, b.y)
   return z
end

function add!(z::fmpzi, a::fmpzi, b::IntegerUnion)
   add!(z.x, a.x, b)
   set!(z.y, a.y)
   return z
end

function add!(z::fmpzi, b::IntegerUnion, a::fmpzi)
   return add!(z, a, b)
end

function +(a::fmpzi, b::Union{Integer, fmpz, fmpzi})
   return add!(fmpzi(), a, b)
end

function +(a::IntegerUnion, b::fmpzi)
   return add!(fmpzi(), a, b)
end


function sub!(z::fmpzi, a::fmpzi, b::fmpzi)
   sub!(z.x, a.x, b.x)
   sub!(z.y, a.y, b.y)
   return z
end

function sub!(z::fmpzi, a::fmpzi, b::IntegerUnion)
   sub!(z.x, a.x, b)
   set!(z.y, a.y)
   return z
end

function sub!(z::fmpzi, a::IntegerUnion, b::fmpzi)
   sub!(z.x, a, b.x)
   neg!(z.y, b.y)
   return z
end

function -(a::fmpzi, b::Union{Integer, fmpz, fmpzi})
   return sub!(fmpzi(), a, b)
end

function -(a::IntegerUnion, b::fmpzi)
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

function mul!(z::fmpzi, a::fmpzi, b::IntegerUnion)
   mul!(z.x, a.x, b)
   mul!(z.y, a.y, b)
   return z
end

function mul!(z::fmpzi, a::IntegerUnion, b::fmpzi)
   return mul!(z, b, a)
end

function *(a::fmpzi, b::fmpzi)
   return _mul!(fmpzi(), a, b)
end

function *(a::fmpzi, b::IntegerUnion)
   return mul!(fmpzi(), a, b)
end

function *(a::IntegerUnion, b::fmpzi)
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
#   division
#
###############################################################################

# for z = mod(a, b) choose the unique representative z so that
# the complex number z/b = x + i*y is in the following box
#     -1/2 <= x < 1/2
#     -1/2 <= y < 1/2

function divrem(a::fmpzi, b::fmpz)
   qx, rx = ncdivrem(a.x, b)
   qy, ry = ncdivrem(a.y, b)
   return fmpzi(qx, qy), fmpzi(rx, ry)
end

function divrem(a::fmpzi, b::fmpzi)
   d = abs2(b)
   qx, r = ncdivrem(a.x*b.x + a.y*b.y, d)
   qy, r = ncdivrem(a.y*b.x - a.x*b.y, d)
   q = fmpzi(qx, qy)
   return q, a - q*b
end

function Base.div(a::fmpzi, b::fmpzi)
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

function divexact!(z::fmpzi, a::fmpzi, b::IntegerUnion)
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
   return fmpzi(a.x, -a.y)
end

###############################################################################
#
#   powering
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
      return _pow!(z, inv(a), (-n)%UInt)
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
# generic functionality for Euclidean interface: why doesn't AA do this?
#
###############################################################################

function mulmod(a::fmpzi, b::fmpzi, c::fmpzi)
   return mod(a*b, c)
end

function powermod(a::fmpzi, b::Int, c::fmpzi)
   if b < 0
      return mod(invmod(a,c)^((-b)%UInt), c)
   else
      return mod(a^b, c)
   end
end

function invmod(a::fmpzi, b::fmpzi)
   g, x, y = gcdx(a, b)
   isone(g) || error("impossible inverse")
   return x
end

function gcdinv(a::fmpzi, b::fmpzi)
   g, x, y = gcdx(a, b)
   return (g, x)
end

function remove(a::fmpzi, b::fmpzi)
   if (iszero(b) || isunit(b))
      throw(ArgumentError("Second argument must be a non-zero non-unit"))
   end
   if iszero(a)
      return (0, zero(parent(a))) # questionable case, consistent with fmpz
   end
   v = 0
   while begin; (ok, q) = divides(a, b); ok; end
      a = q
      v += 1
   end
   return v, a
end

function valuation(a::fmpzi, b::fmpzi)
   return remove(a, b)[1]
end

function lcm(a::fmpzi, b::fmpzi)
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

function smod(a::fmpz, b::fmpz)
   z = fmpz()
   ccall((:fmpz_smod, libflint), Nothing,
         (Ref{fmpz}, Ref{fmpz}, Ref{fmpz}), z, a, b)
   return z
end

function gcd(a::fmpzi, b::fmpzi)
   if iszero(b.y)
      return gcd(a, b.x)
   elseif iszero(b.x)
      return gcd(a, b.y)
   elseif iszero(a.y)
      return gcd(b, a.x)
   elseif iszero(a.x)
      return gcd(b, a.y)
   end
   #  ax ay           g*1 g*B
   # -ay ax  row ops   0  g*A
   #  bx by  ------>   0   0
   # -by bx            0   0
   ga, ua, va = gcdx(a.x, a.y)
   gb, ub, vb = gcdx(b.x, b.y)
   g, u, v = gcdx(ga, gb)
   axog = _divexact(a.x, g)
   ayog = _divexact(a.y, g)
   bxog = _divexact(b.x, g)
   byog = _divexact(b.y, g)
   m1 = ayog*ua - axog*va
   m2 = _divexact(a.x,ga)*axog + _divexact(a.y,ga)*ayog
   m3 = byog*ub - bxog*vb
   m4 = _divexact(b.x,gb)*bxog + _divexact(b.y,gb)*byog
   (m1, m3) = (m1*u + m3*v, _divexact(ga,g)*m3 - _divexact(gb,g)*m1)
   A = gcd(m2, m3, m4)
   v, _ = _shortest_l_infinity(fmpz(1), mod(m1, A), A)
   z = isone(g) ? fmpzi(v[1], v[2]) : fmpzi(v[1]*g, v[2]*g)
   return mul_i_pow!(z, canonical_unit_i_pow(z))
end

function gcd(a::fmpzi, b::fmpz)
   if iszero(b)
      return mul_i_pow!(deepcopy(a), canonical_unit_i_pow(a))
   end
   #  ax ay           g*1 g*B
   # -ay ax  row ops   0  g*A
   #  b  0   ------>   0   0
   #  0  b             0   0
   ax = cmpabs(a.x, b) < 0 ? a.x : smod(a.x, b)
   ay = cmpabs(a.y, b) < 0 ? a.y : smod(a.y, b)
   if iszero(ax)
      return fmpzi(gcd(ay, b), zero(fmpz))
   elseif iszero(ay)
      return fmpzi(gcd(ax, b), zero(fmpz))
   end
   ga, ua, va = gcdx(ax, ay)
   g, u, _ = gcdx(ga, b)
   axog = _divexact(ax, g)
   ayog = _divexact(ay, g)
   m1 = ayog*ua - axog*va
   m2 = _divexact(ax,ga)*axog + _divexact(ay,ga)*ayog
   A = gcd(m2, _divexact(b, g))
   v, _ = _shortest_l_infinity(fmpz(1), mod(m1*u, A), A)
   z = isone(g) ? fmpzi(v[1], v[2]) : fmpzi(v[1]*g, v[2]*g)
   return mul_i_pow!(z, canonical_unit_i_pow(z))
end

function gcd(a::fmpzi, b::Integer)
  return gcd(a, fmpz(b))
end

function gcd(b::Union{fmpz, Integer}, a::fmpzi)
  return gcd(a, fmpz(b))
end

function gcdx(a::fmpzi, b::fmpzi)
   if iszero(a)
      if iszero(b)
         return (zero(FlintZZi), zero(FlintZZi), zero(FlintZZi))
      else
         u = canonical_unit(b)
         return (divexact(b, u), zero(FlintZZi), inv(u))
      end
   elseif iszero(b)
      u = canonical_unit(a)
      return (divexact(a, u), inv(u), zero(FlintZZi))
   end
   m = zero_matrix(FlintZZ, 4, 2)
   m[1,1] =  a.x; m[1,2] = a.y
   m[2,1] = -a.y; m[2,2] = a.x
   m[3,1] =  b.x; m[3,2] = b.y
   m[4,1] = -b.y; m[4,2] = b.x
   v, t = shortest_l_infinity_with_transform(m)
   z = fmpzi(v[1], v[2])
   w1 = fmpzi(t[1], t[2])
   w2 = fmpzi(t[3], t[4])
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
      f.unit = divexact(f.unit, c, check=false)
      addeqindex!(f, 1, c)
   end
   for (p, e) in factor(abs2(f.unit))
      c1 = _sum_of_squares(p)
      if !isdivisible_by(f.unit, c1)
         c1 = conj(c1)
      end
      f.unit = divexact(f.unit, c1^e)
      addeqindex!(f, e, c1)
   end
   @assert isunit(f.unit)
   return f
end

###############################################################################
#
# adhoc
#
###############################################################################

# +,-,* operations defined above
promote_rule(a::Type{fmpzi}, b::Type{fmpz}) = fmpzi
promote_rule(a::Type{fmpz}, b::Type{fmpzi}) = fmpzi
promote_rule(a::Type{fmpzi}, b::Type{<:Integer}) = fmpzi
promote_rule(a::Type{<:Integer}, b::Type{fmpzi}) = fmpzi

promote_rule(a::Type{fmpz}, b::Type{<:Complex{<:Integer}}) = fmpzi
promote_rule(a::Type{<:Complex{<:Integer}}, b::Type{fmpz}) = fmpzi
*(a::fmpz, b::Complex{<:Integer}) = fmpzi(a*real(b), a*imag(b))
*(b::Complex{<:Integer}, a::fmpz) = fmpzi(a*real(b), a*imag(b))
+(a::fmpz, b::Complex{<:Integer}) = fmpzi(a + real(b), imag(b))
+(b::Complex{<:Integer}, a::fmpz) = fmpzi(a + real(b), imag(b))
-(a::fmpz, b::Complex{<:Integer}) = fmpzi(a - real(b), -imag(b))
-(b::Complex{<:Integer}, a::fmpz) = fmpzi(real(b) - a, imag(b))

promote_rule(a::Type{fmpzi}, b::Type{<:Complex{<:Integer}}) = fmpzi
promote_rule(a::Type{<:Complex{<:Integer}}, b::Type{fmpzi}) = fmpzi
*(a::fmpzi, b::Complex{<:Integer}) = a*FlintZZi(b)
*(b::Complex{<:Integer}, a::fmpzi) = a*FlintZZi(b)
+(a::fmpzi, b::Complex{<:Integer}) = fmpzi(a.x + real(b), a.y + imag(b))
+(b::Complex{<:Integer}, a::fmpzi) = fmpzi(a.x + real(b), a.y + imag(b))
-(a::fmpzi, b::Complex{<:Integer}) = fmpzi(a.x - real(b), a.y - imag(b))
-(b::Complex{<:Integer}, a::fmpzi) = fmpzi(real(b) - a.x, imag(b) - a.y)

