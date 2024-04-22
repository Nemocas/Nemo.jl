###############################################################################
#
#   ZZModRingElem.jl : Nemo ZZModRingElem (integers modulo large n)
#
###############################################################################

###############################################################################
#
#   Type and parent object methods
#
###############################################################################

parent_type(::Type{ZZModRingElem}) = ZZModRing

elem_type(::Type{ZZModRing}) = ZZModRingElem

base_ring(a::ZZModRing) = ZZ

parent(a::ZZModRingElem) = a.parent

function check_parent(a::ZZModRingElem, b::ZZModRingElem)
   a.parent != b.parent && error("Operations on distinct residue rings not supported")
end

###############################################################################
#
#   Basic manipulation
#
###############################################################################

function Base.hash(a::ZZModRingElem, h::UInt)
   b = 0x2fbb6980039a0fec%UInt
   return xor(xor(hash(a.data), h), b)
end

lift(a::ZZModRingElem) = data(a)

function zero(R::ZZModRing)
   return ZZModRingElem(ZZRingElem(0), R)
end

function one(R::ZZModRing)
   if R.n == 1
      return ZZModRingElem(ZZRingElem(0), R)
   else
      return ZZModRingElem(ZZRingElem(1), R)
   end
end

iszero(a::ZZModRingElem) = iszero(a.data)

isone(a::ZZModRingElem) = a.parent.n == 1 ? iszero(a.data) : isone(a.data)

is_unit(a::ZZModRingElem) = a.parent.n == 1 ? iszero(a.data) : isone(gcd(a.data, a.parent.n))

modulus(R::ZZModRing) = R.n

function deepcopy_internal(a::ZZModRingElem, dict::IdDict)
   R = parent(a)
   return ZZModRingElem(deepcopy(a.data), R)
end

characteristic(R::ZZModRing) = modulus(R)

###############################################################################
#
#   Canonicalisation
#
###############################################################################

function canonical_unit(x::ZZModRingElem)
  # the simple return x does not work
  #  - if x == 0, this is not a unit
  #  - if R is not a field....
  if iszero(x)
    return parent(x)(0)
  end
  g = gcd(modulus(x), data(x))
  u = divexact(data(x), g)
  a, b = ppio(modulus(x), u)
  if isone(a)
    r = u
  elseif isone(b)
    r = b
  else
    r = crt(ZZRingElem(1), a, u, b)
  end
  return parent(x)(r)
end

###############################################################################
#
#   AbstractString I/O
#
###############################################################################

function show(io::IO, R::ZZModRing)
   if get(io, :supercompact, false)
      # no nested printing
      io = pretty(io)
      print(io, LowercaseOff(), "ZZ/($(R.n))")
   else
      # nested printing allowed, preferably supercompact
      print(io, "Integers modulo ", R.n)
   end
end

function expressify(a::ZZModRingElem; context = nothing)
   return a.data
end

function show(io::IO, a::ZZModRingElem)
   print(io, a.data)
end

###############################################################################
#
#   Unary operations
#
###############################################################################

function -(x::ZZModRingElem)
   if iszero(x.data)
      return deepcopy(x)
   else
      R = parent(x)
      return ZZModRingElem(R.n - x.data, R)
   end
end

###############################################################################
#
#   Binary operations
#
###############################################################################

function +(x::ZZModRingElem, y::ZZModRingElem)
   check_parent(x, y)
   R = parent(x)
   n = modulus(R)
   d = x.data + y.data - n
   if d < 0
      return ZZModRingElem(d + n, R)
   else
      return ZZModRingElem(d, R)
   end
end

function -(x::ZZModRingElem, y::ZZModRingElem)
   check_parent(x, y)
   R = parent(x)
   n = modulus(R)
   d = x.data - y.data
   if d < 0
      return ZZModRingElem(d + n, R)
   else
      return ZZModRingElem(d, R)
   end
end

function *(x::ZZModRingElem, y::ZZModRingElem)
   check_parent(x, y)
   R = parent(x)
   d = ZZRingElem()
   ccall((:fmpz_mod_mul, libflint), Nothing, (Ref{ZZRingElem}, Ref{ZZRingElem}, Ref{ZZRingElem}, Ref{fmpz_mod_ctx_struct}),
             d, x.data, y.data, R.ninv)
   return ZZModRingElem(d, R)
end

###############################################################################
#
#   Ad hoc binary operators
#
###############################################################################

function *(x::Integer, y::ZZModRingElem)
   R = parent(y)
   return R(x*y.data)
end

*(x::ZZModRingElem, y::Integer) = y*x

+(x::ZZModRingElem, y::Integer) = x + parent(x)(y)

+(x::Integer, y::ZZModRingElem) = y + x

-(x::ZZModRingElem, y::Integer) = x - parent(x)(y)

-(x::Integer, y::ZZModRingElem) = parent(y)(x) - y

###############################################################################
#
#   Powering
#
###############################################################################

function ^(x::ZZModRingElem, y::Int)
   R = parent(x)
   if y < 0
      x = inv(x)
      y = -y
   end
   d = ZZRingElem()
   ccall((:fmpz_mod_pow_ui, libflint), Nothing,
	 (Ref{ZZRingElem}, Ref{ZZRingElem}, UInt, Ref{fmpz_mod_ctx_struct}),
             d, x.data, y, R.ninv)
   return ZZModRingElem(d, R)
end


###############################################################################
#
#   Comparison
#
###############################################################################

function ==(x::ZZModRingElem, y::ZZModRingElem)
   check_parent(x, y)
   return x.data == y.data
end

###############################################################################
#
#   Ad hoc comparison
#
###############################################################################

==(x::ZZModRingElem, y::Integer) = x == parent(x)(y)

==(x::Integer, y::ZZModRingElem) = parent(y)(x) == y

==(x::ZZModRingElem, y::ZZRingElem) = x == parent(x)(y)

==(x::ZZRingElem, y::ZZModRingElem) = parent(y)(x) == y

###############################################################################
#
#   Inversion
#
###############################################################################

function inv(x::ZZModRingElem)
   R = parent(x)
   (iszero(x) && R.n != 1) && throw(DivideError())
   if R.n == 1
      return deepcopy(x)
   end
   s = ZZRingElem()
   g = ZZRingElem()
   ccall((:fmpz_gcdinv, libflint), Nothing,
	 (Ref{ZZRingElem}, Ref{ZZRingElem}, Ref{ZZRingElem}, Ref{ZZRingElem}),
            g, s, x.data, R.n)
   g != 1 && error("Impossible inverse in ", R)
   return ZZModRingElem(s, R)
end

###############################################################################
#
#   Exact division
#
###############################################################################

function divexact(x::ZZModRingElem, y::ZZModRingElem; check::Bool=true)
   check_parent(x, y)
   fl, q = divides(x, y)
   if !fl
     error("Impossible inverse in ", parent(x))
   end
   return q
end

function divides(a::ZZModRingElem, b::ZZModRingElem)
   check_parent(a, b)
   if iszero(a)
      return true, a
   end
   A = data(a)
   B = data(b)
   R = parent(a)
   m = modulus(R)
   gb = gcd(B, m)
   q, r = divrem(A, gb)
   if !iszero(r)
      return false, b
   end
   ub = divexact(B, gb)
   b1 = ZZRingElem()
   ccall((:fmpz_invmod, libflint), Nothing, (Ref{ZZRingElem}, Ref{ZZRingElem}, Ref{ZZRingElem}),
           b1, ub, divexact(m, gb))
   rr = R(q)*b1
   return true, rr
end

###############################################################################
#
#   GCD
#
###############################################################################

function gcd(x::ZZModRingElem, y::ZZModRingElem)
   check_parent(x, y)
   R = parent(x)
   d = gcd(gcd(x.data, R.n), y.data)
   if d == R.n
      return ZZModRingElem(0, R)
   else
      return ZZModRingElem(d, R)
   end
end

@doc raw"""
    gcdx(a::ZZModRingElem, b::ZZModRingElem)

Compute the extended gcd with the Euclidean structure inherited from
$\mathbb{Z}$.
"""
function gcdx(a::ZZModRingElem, b::ZZModRingElem)
   m = modulus(a)
   R = parent(a)
   g, u, v = gcdx(a.data, b.data)
   G, U, V = gcdx(g, m)
   return R(G), R(U)*R(u), R(U)*R(v)
end

###############################################################################
#
#   Unsafe functions
#
###############################################################################

function zero!(z::ZZModRingElem)
   ccall((:fmpz_set_ui, libflint), Nothing,
		(Ref{ZZRingElem}, UInt), z.data, UInt(0))
   return z
end

function mul!(z::ZZModRingElem, x::ZZModRingElem, y::ZZModRingElem)
   R = parent(z)
   ccall((:fmpz_mod_mul, libflint), Nothing,
	 (Ref{ZZRingElem}, Ref{ZZRingElem}, Ref{ZZRingElem}, Ref{fmpz_mod_ctx_struct}),
	      z.data, x.data, y.data, R.ninv)
   return z
end

function addeq!(z::ZZModRingElem, x::ZZModRingElem)
   R = parent(z)
   ccall((:fmpz_mod_add, libflint), Nothing,
	 (Ref{ZZRingElem}, Ref{ZZRingElem}, Ref{ZZRingElem}, Ref{fmpz_mod_ctx_struct}),
	    z.data, z.data, x.data, R.ninv)
   return z
end

function add!(z::ZZModRingElem, x::ZZModRingElem, y::ZZModRingElem)
   R = parent(z)
   ccall((:fmpz_mod_add, libflint), Nothing,
	(Ref{ZZRingElem}, Ref{ZZRingElem}, Ref{ZZRingElem}, Ref{fmpz_mod_ctx_struct}),
	   z.data, x.data, y.data, R.ninv)
   return z
end

###############################################################################
#
#   Random functions
#
###############################################################################

# define rand(::ZZModRing)

Random.Sampler(::Type{RNG}, R::ZZModRing, n::Random.Repetition) where {RNG<:AbstractRNG} =
   Random.SamplerSimple(R, Random.Sampler(RNG, BigInt(0):BigInt(R.n)-1, n))

function rand(rng::AbstractRNG, R::Random.SamplerSimple{ZZModRing})
   n = rand(rng, R.data)
   ZZModRingElem(ZZRingElem(n), R[])
end

Random.gentype(::Type{ZZModRing}) = elem_type(ZZModRing)

# define rand(make(::ZZModRing, arr)), where arr is any abstract array with integer or ZZRingElem entries

RandomExtensions.maketype(R::ZZModRing, _) = elem_type(R)

rand(rng::AbstractRNG, sp::SamplerTrivial{<:Make2{ZZModRingElem,ZZModRing,<:AbstractArray{<:IntegerUnion}}}) =
   sp[][1](rand(rng, sp[][2]))

# define rand(::ZZModRing, arr), where arr is any abstract array with integer or ZZRingElem entries

rand(r::Random.AbstractRNG, R::ZZModRing, b::AbstractArray) = rand(r, make(R, b))

rand(R::ZZModRing, b::AbstractArray) = rand(Random.GLOBAL_RNG, R, b)

###############################################################################
#
#   Promotions
#
###############################################################################

promote_rule(::Type{ZZModRingElem}, ::Type{T}) where T <: Integer = ZZModRingElem

promote_rule(::Type{ZZModRingElem}, ::Type{ZZRingElem}) = ZZModRingElem

###############################################################################
#
#   Parent object call overload
#
###############################################################################

function (R::ZZModRing)()
   return ZZModRingElem(ZZRingElem(0), R)
end

function (R::ZZModRing)(a::Integer)
   n = R.n
   d = ZZRingElem(a)%n
   if d < 0
      d += n
   end
   return ZZModRingElem(d, R)
end

function (R::ZZModRing)(a::ZZRingElem)
   d = ZZRingElem()
   ccall((:fmpz_mod, libflint), Nothing, (Ref{ZZRingElem}, Ref{ZZRingElem}, Ref{ZZRingElem}),
             d, a, R.n)
   return ZZModRingElem(d, R)
end

function (R::ZZModRing)(a::Union{fpFieldElem, zzModRingElem, FpFieldElem, ZZModRingElem})
   S = parent(a)
   if S === R
      return a
   else
      is_divisible_by(modulus(S), modulus(R)) || error("incompatible parents")
      return R(data(a))
   end
end

###############################################################################
#
#   ZZModRingElem constructor
#
###############################################################################

function residue_ring(R::ZZRing, n::ZZRingElem; cached::Bool=true)
   # Modulus of zero cannot be supported. E.g. Flint library could not be expected to
   # do matrices over Z/0 using a Z/nZ type. The former is multiprecision, the latter not.
   n <= 0 && throw(DomainError(n, "Modulus must be positive"))
   S = ZZModRing(n, cached)
   f = Generic.EuclideanRingResidueMap(R, S)
   return S, f
end

function residue_ring(R::ZZRing, n::Integer; cached::Bool=true)
   return residue_ring(R, ZZRingElem(n))
end
