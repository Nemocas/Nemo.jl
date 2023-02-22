###############################################################################
#
#   nmod.jl : Nemo nmod (integers modulo small n)
#
###############################################################################

export zzModRingElem

###############################################################################
#
#   Type and parent object methods
#
###############################################################################

parent_type(::Type{zzModRingElem}) = zzModRing

elem_type(::Type{zzModRing}) = zzModRingElem

base_ring(a::zzModRing) = FlintZZ

base_ring(a::zzModRingElem) = FlintZZ

parent(a::zzModRingElem) = a.parent

function check_parent(a::zzModRingElem, b::zzModRingElem)
   a.parent != b.parent && error("Operations on distinct residue rings not supported")
end

###############################################################################
#
#   Basic manipulation
#
###############################################################################

function Base.hash(a::zzModRingElem, h::UInt)
   b = 0x1812aa3492cbf4d9%UInt
   return xor(xor(hash(a.data), h), b)
end

lift(a::zzModRingElem) = ZZRingElem(data(a))

function zero(R::zzModRing)
   return zzModRingElem(UInt(0), R)
end

function one(R::zzModRing)
   if R.n == 1
      return zzModRingElem(UInt(0), R)
   else
      return zzModRingElem(UInt(1), R)
   end
end

iszero(a::zzModRingElem) = a.data == 0

isone(a::zzModRingElem) = a.parent.n == 1 ? a.data == 0 : a.data == 1

is_unit(a::zzModRingElem) = a.parent.n == 1 ? a.data == 0 : gcd(a.data, a.parent.n) == 1

modulus(R::zzModRing) = R.n

function deepcopy_internal(a::zzModRingElem, dict::IdDict)
   R = parent(a)
   return zzModRingElem(deepcopy(a.data), R)
end

characteristic(R::zzModRing) = ZZRingElem(modulus(R))

###############################################################################
#
#   Canonicalisation
#
###############################################################################

function canonical_unit(x::zzModRingElem)
  #the simple return x does not work
  # - if x == 0, this is not a unit
  # - if R is not a field....
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
    r = crt(ZZRingElem(1), ZZRingElem(a), ZZRingElem(u), ZZRingElem(b))
  end
  return parent(x)(r)
end

###############################################################################
#
#   AbstractString I/O
#
###############################################################################

function show(io::IO, R::zzModRing)
   print(io, "Integers modulo ", signed(widen(R.n)))
end

function expressify(a::Nemo.zzModRingElem; context = nothing)
    return a.data
end

function show(io::IO, a::zzModRingElem)
   print(io, signed(widen(a.data)))
end

###############################################################################
#
#   Unary operations
#
###############################################################################

function -(x::zzModRingElem)
   if x.data == 0
      return deepcopy(x)
   else
      R = parent(x)
      return zzModRingElem(R.n - x.data, R)
   end
end

###############################################################################
#
#   Binary operations
#
###############################################################################

function +(x::zzModRingElem, y::zzModRingElem)
   check_parent(x, y)
   R = parent(x)
   n = modulus(R)
   d = x.data + y.data - n
   if d > x.data
      return zzModRingElem(d + n, R)
   else
      return zzModRingElem(d, R)
   end
end

function -(x::zzModRingElem, y::zzModRingElem)
   check_parent(x, y)
   R = parent(x)
   n = modulus(R)
   d = x.data - y.data
   if d > x.data
      return zzModRingElem(d + n, R)
   else
      return zzModRingElem(d, R)
   end
end

function *(x::zzModRingElem, y::zzModRingElem)
   check_parent(x, y)
   R = parent(x)
   d = ccall((:n_mulmod2_preinv, libflint), UInt, (UInt, UInt, UInt, UInt),
             x.data, y.data, R.n, R.ninv)
   return zzModRingElem(d, R)
end

###############################################################################
#
#   Ad hoc binary operators
#
###############################################################################

function *(x::Integer, y::zzModRingElem)
   R = parent(y)
   return R(widen(x)*signed(widen(y.data)))
end

*(x::zzModRingElem, y::Integer) = y*x

function *(x::Int, y::zzModRingElem)
   R = parent(y)
   if x < 0
      d = ccall((:n_mulmod2_preinv, libflint), UInt, (UInt, UInt, UInt, UInt),
             UInt(-x), y.data, R.n, R.ninv)
      return -zzModRingElem(d, R)
   else
      d = ccall((:n_mulmod2_preinv, libflint), UInt, (UInt, UInt, UInt, UInt),
             UInt(x), y.data, R.n, R.ninv)
      return zzModRingElem(d, R)
   end
end

*(x::zzModRingElem, y::Int) = y*x

function *(x::UInt, y::zzModRingElem)
   R = parent(y)
   d = ccall((:n_mulmod2_preinv, libflint), UInt, (UInt, UInt, UInt, UInt),
             UInt(x), y.data, R.n, R.ninv)
   return zzModRingElem(d, R)
end

*(x::zzModRingElem, y::UInt) = y*x

+(x::zzModRingElem, y::Integer) = x + parent(x)(y)

+(x::Integer, y::zzModRingElem) = y + x

-(x::zzModRingElem, y::Integer) = x - parent(x)(y)

-(x::Integer, y::zzModRingElem) = parent(y)(x) - y

###############################################################################
#
#   Powering
#
###############################################################################

function ^(x::zzModRingElem, y::Int)
   R = parent(x)
   if y < 0
      x = inv(x)
      y = -y
   end
   d = ccall((:n_powmod2_preinv, libflint), UInt, (UInt, Int, UInt, UInt),
             UInt(x.data), y, R.n, R.ninv)
   return zzModRingElem(d, R)
end

###############################################################################
#
#   Comparison
#
###############################################################################

function ==(x::zzModRingElem, y::zzModRingElem)
   check_parent(x, y)
   return x.data == y.data
end

###############################################################################
#
#   Ad hoc comparison
#
###############################################################################

==(x::zzModRingElem, y::Integer) = x == parent(x)(y)

==(x::Integer, y::zzModRingElem) = parent(y)(x) == y

==(x::zzModRingElem, y::ZZRingElem) = x == parent(x)(y)

==(x::ZZRingElem, y::zzModRingElem) = parent(y)(x) == y

###############################################################################
#
#   Inversion
#
###############################################################################

function inv(x::zzModRingElem)
   R = parent(x)
   (iszero(x) && R.n != 1) && throw(DivideError())
   if R.n == 1
      return deepcopy(x)
   end
   #s = [UInt(0)]
   s = Ref{UInt}()
   g = ccall((:n_gcdinv, libflint), UInt, (Ptr{UInt}, UInt, UInt),
         s, x.data, R.n)
   g != 1 && error("Impossible inverse in ", R)
   return zzModRingElem(s[], R)
end

###############################################################################
#
#   Exact division
#
###############################################################################

function divexact(x::zzModRingElem, y::zzModRingElem; check::Bool=true)
   check_parent(x, y)
   fl, q = divides(x, y)
   if !fl
     error("Impossible inverse in ", parent(x))
   end
   return q
end

function divides(a::zzModRingElem, b::zzModRingElem)
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
   # The Julia invmod function does not give the correct result for me
   b1 = ccall((:n_invmod, libflint), UInt, (UInt, UInt),
           ub, divexact(m, gb))
   rr = R(q)*b1
   return true, rr
end

###############################################################################
#
#   GCD
#
###############################################################################

function gcd(x::zzModRingElem, y::zzModRingElem)
   check_parent(x, y)
   R = parent(x)
   d = gcd(gcd(x.data, R.n), y.data)
   if d == R.n
      return zzModRingElem(0, R)
   else
      return zzModRingElem(d, R)
   end
end

@doc Markdown.doc"""
    gcdx(a::zzModRingElem, b::zzModRingElem)

Compute the extended gcd with the Euclidean structure inherited from
$\mathbb{Z}$.
"""
function gcdx(a::zzModRingElem, b::zzModRingElem)
   m = modulus(a)
   R = parent(a)
   g, u, v = gcdx(ZZRingElem(a.data), ZZRingElem(b.data))
   G, U, V = gcdx(g, ZZRingElem(m))
   return R(G), R(U)*R(u), R(U)*R(v)
end

###############################################################################
#
#   Unsafe functions
#
###############################################################################

function zero!(z::zzModRingElem)
   R = parent(z)
   return zzModRingElem(UInt(0), R)
end

function mul!(z::zzModRingElem, x::zzModRingElem, y::zzModRingElem)
   return x*y
end

function addeq!(z::zzModRingElem, x::zzModRingElem)
   return z + x
end

function add!(z::zzModRingElem, x::zzModRingElem, y::zzModRingElem)
   return x + y
end

###############################################################################
#
#   Random functions
#
###############################################################################

# define rand(::zzModRing)

Random.Sampler(::Type{RNG}, R::zzModRing, n::Random.Repetition) where {RNG<:AbstractRNG} =
   Random.SamplerSimple(R, Random.Sampler(RNG, UInt(0):R.n - 1, n))

rand(rng::AbstractRNG, R::Random.SamplerSimple{zzModRing}) = zzModRingElem(rand(rng, R.data), R[])

Random.gentype(::Type{zzModRing}) = elem_type(zzModRing)

# define rand(make(R::zzModRing, arr)), where arr is any abstract array with integer or ZZRingElem entries

RandomExtensions.maketype(R::zzModRing, _) = elem_type(R)

rand(rng::AbstractRNG, sp::SamplerTrivial{<:Make2{zzModRingElem,zzModRing,<:AbstractArray{<:IntegerUnion}}}) =
   sp[][1](rand(rng, sp[][2]))

# define rand(R::zzModRing, arr), where arr is any abstract array with integer or ZZRingElem entries

rand(r::Random.AbstractRNG, R::zzModRing, b::AbstractArray) = rand(r, make(R, b))

rand(R::zzModRing, b::AbstractArray) = rand(Random.GLOBAL_RNG, R, b)

###############################################################################
#
#   Promotions
#
###############################################################################

promote_rule(::Type{zzModRingElem}, ::Type{T}) where T <: Integer = zzModRingElem

promote_rule(::Type{zzModRingElem}, ::Type{ZZRingElem}) = zzModRingElem

###############################################################################
#
#   Parent object call overload
#
###############################################################################

function (R::zzModRing)()
   return zzModRingElem(UInt(0), R)
end

function (R::zzModRing)(a::Integer)
   n = R.n
   d = a%signed(widen(n))
   if d < 0
      d += n
   end
   return zzModRingElem(UInt(d), R)
end

function (R::zzModRing)(a::Int)
   n = R.n
   ninv = R.ninv
   if reinterpret(Int, n) > 0 && a < 0
      a %= Int(n)
   end
   d = reinterpret(UInt, a)
   if a < 0
      d += n
   end
   if d >= n
      d = ccall((:n_mod2_preinv, libflint), UInt, (UInt, UInt, UInt),
             d, n, ninv)
   end
   return zzModRingElem(d, R)
end

function (R::zzModRing)(a::UInt)
   n = R.n
   ninv = R.ninv
   a = ccall((:n_mod2_preinv, libflint), UInt, (UInt, UInt, UInt),
             a, n, ninv)
   return zzModRingElem(a, R)
end

function (R::zzModRing)(a::ZZRingElem)
   d = ccall((:fmpz_fdiv_ui, libflint), UInt, (Ref{ZZRingElem}, UInt),
             a, R.n)
   return zzModRingElem(d, R)
end

function (R::zzModRing)(a::Union{fpFieldElem, zzModRingElem, FpFieldElem, ZZModRingElem})
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
#   zzModRingElem constructor
#
###############################################################################

function ResidueRing(R::ZZRing, n::Int; cached::Bool=true)
   # Modulus of zero cannot be supported. E.g. Flint library could not be expected to
   # do matrices over Z/0 using a Z/nZ type. The former is multiprecision, the latter not.
   n <= 0 && throw(DomainError(n, "Modulus must be positive"))
   return zzModRing(UInt(n), cached)
end

function ResidueRing(R::ZZRing, n::UInt; cached::Bool=true)
   return zzModRing(n, cached)
end
