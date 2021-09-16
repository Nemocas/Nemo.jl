###############################################################################
#
#   fmpz_mod.jl : Nemo fmpz_mod (integers modulo large n)
#
###############################################################################

export fmpz_mod

###############################################################################
#
#   Type and parent object methods
#
###############################################################################

parent_type(::Type{fmpz_mod}) = FmpzModRing

elem_type(::Type{FmpzModRing}) = fmpz_mod

base_ring(a::FmpzModRing) = FlintZZ

base_ring(a::fmpz_mod) = FlintZZ

parent(a::fmpz_mod) = a.parent

function check_parent(a::fmpz_mod, b::fmpz_mod)
   a.parent != b.parent && error("Operations on distinct residue rings not supported")
end

###############################################################################
#
#   Basic manipulation
#
###############################################################################

function Base.hash(a::fmpz_mod, h::UInt)
   b = 0x2fbb6980039a0fec%UInt
   return xor(xor(hash(a.data), h), b)
end

lift(a::fmpz_mod) = data(a)

function zero(R::FmpzModRing)
   return fmpz_mod(fmpz(0), R)
end

function one(R::FmpzModRing)
   if R.n == 1
      return fmpz_mod(fmpz(0), R)
   else
      return fmpz_mod(fmpz(1), R)
   end
end

iszero(a::fmpz_mod) = iszero(a.data)

isone(a::fmpz_mod) = a.parent.n == 1 ? iszero(a.data) : isone(a.data)

isunit(a::fmpz_mod) = a.parent.n == 1 ? iszero(a.data) : isone(gcd(a.data, a.parent.n))

modulus(R::FmpzModRing) = R.n

function deepcopy_internal(a::fmpz_mod, dict::IdDict)
   R = parent(a)
   return fmpz_mod(deepcopy(a.data), R)
end

characteristic(R::FmpzModRing) = modulus(R)

###############################################################################
#
#   Canonicalisation
#
###############################################################################

function canonical_unit(x::fmpz_mod)
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
    r = crt(fmpz(1), a, u, b)
  end
  return parent(x)(r)
end

###############################################################################
#
#   AbstractString I/O
#
###############################################################################

function show(io::IO, R::FmpzModRing)
   print(io, "Integers modulo ", R.n)
end

function expressify(a::fmpz_mod; context = nothing)
   return a.data
end

function show(io::IO, a::fmpz_mod)
   print(io, a.data)
end

needs_parentheses(x::fmpz_mod) = false

###############################################################################
#
#   Unary operations
#
###############################################################################

function -(x::fmpz_mod)
   if iszero(x.data)
      return deepcopy(x)
   else
      R = parent(x)
      return fmpz_mod(R.n - x.data, R)
   end
end

###############################################################################
#
#   Binary operations
#
###############################################################################

function +(x::fmpz_mod, y::fmpz_mod)
   check_parent(x, y)
   R = parent(x)
   n = modulus(R)
   d = x.data + y.data - n
   if d < 0
      return fmpz_mod(d + n, R)
   else
      return fmpz_mod(d, R)
   end
end

function -(x::fmpz_mod, y::fmpz_mod)
   check_parent(x, y)
   R = parent(x)
   n = modulus(R)
   d = x.data - y.data
   if d < 0
      return fmpz_mod(d + n, R)
   else
      return fmpz_mod(d, R)
   end
end

function *(x::fmpz_mod, y::fmpz_mod)
   check_parent(x, y)
   R = parent(x)
   d = fmpz()
   ccall((:fmpz_mod_mul, libflint), Nothing, (Ref{fmpz}, Ref{fmpz}, Ref{fmpz}, Ref{fmpz_mod_ctx_struct}),
             d, x.data, y.data, R.ninv)
   return fmpz_mod(d, R)
end

###############################################################################
#
#   Ad hoc binary operators
#
###############################################################################

function *(x::Integer, y::fmpz_mod)
   R = parent(y)
   return R(x*y.data)
end

*(x::fmpz_mod, y::Integer) = y*x

+(x::fmpz_mod, y::Integer) = x + parent(x)(y)

+(x::Integer, y::fmpz_mod) = y + x

-(x::fmpz_mod, y::Integer) = x - parent(x)(y)

-(x::Integer, y::fmpz_mod) = parent(y)(x) - y

###############################################################################
#
#   Powering
#
###############################################################################

function ^(x::fmpz_mod, y::Int)
   R = parent(x)
   if y < 0
      x = inv(x)
      y = -y
   end
   d = fmpz()
   ccall((:fmpz_mod_pow_ui, libflint), Nothing,
	 (Ref{fmpz}, Ref{fmpz}, UInt, Ref{fmpz_mod_ctx_struct}),
             d, x.data, y, R.ninv)
   return fmpz_mod(d, R)
end


###############################################################################
#
#   Comparison
#
###############################################################################

function ==(x::fmpz_mod, y::fmpz_mod)
   check_parent(x, y)
   return x.data == y.data
end

###############################################################################
#
#   Ad hoc comparison
#
###############################################################################

==(x::fmpz_mod, y::Integer) = x == parent(x)(y)

==(x::Integer, y::fmpz_mod) = parent(y)(x) == y

==(x::fmpz_mod, y::fmpz) = x == parent(x)(y)

==(x::fmpz, y::fmpz_mod) = parent(y)(x) == y

###############################################################################
#
#   Inversion
#
###############################################################################

function inv(x::fmpz_mod)
   R = parent(x)
   (iszero(x) && R.n != 1) && throw(DivideError())
   if R.n == 1
      return deepcopy(x)
   end
   s = fmpz()
   g = fmpz()
   ccall((:fmpz_gcdinv, libflint), Nothing,
	 (Ref{fmpz}, Ref{fmpz}, Ref{fmpz}, Ref{fmpz}),
            g, s, x.data, R.n)
   g != 1 && error("Impossible inverse in ", R)
   return fmpz_mod(s, R)
end

###############################################################################
#
#   Exact division
#
###############################################################################

function divexact(x::fmpz_mod, y::fmpz_mod; check::Bool=true)
   check_parent(x, y)
   fl, q = divides(x, y)
   if !fl
     error("Impossible inverse in ", parent(x))
   end
   return q
end

function divides(a::fmpz_mod, b::fmpz_mod)
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
   b1 = fmpz()
   ccall((:fmpz_invmod, libflint), Nothing, (Ref{fmpz}, Ref{fmpz}, Ref{fmpz}),
           b1, ub, divexact(m, gb))
   rr = R(q)*b1
   return true, rr
end

###############################################################################
#
#   GCD
#
###############################################################################

function gcd(x::fmpz_mod, y::fmpz_mod)
   check_parent(x, y)
   R = parent(x)
   d = gcd(gcd(x.data, R.n), y.data)
   if d == R.n
      return fmpz_mod(0, R)
   else
      return fmpz_mod(d, R)
   end
end

@doc Markdown.doc"""
    gcdx(a::fmpz_mod, b::fmpz_mod)

Compute the extended gcd with the Euclidean structure inherited from
$\mathbb{Z}$.
"""
function gcdx(a::fmpz_mod, b::fmpz_mod)
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

function zero!(z::fmpz_mod)
   ccall((:fmpz_set_ui, libflint), Nothing,
		(Ref{fmpz}, UInt), z.data, UInt(0))
   return z
end

function mul!(z::fmpz_mod, x::fmpz_mod, y::fmpz_mod)
   R = parent(z)
   ccall((:fmpz_mod_mul, libflint), Nothing,
	 (Ref{fmpz}, Ref{fmpz}, Ref{fmpz}, Ref{fmpz_mod_ctx_struct}),
	      z.data, x.data, y.data, R.ninv)
   return z
end

function addeq!(z::fmpz_mod, x::fmpz_mod)
   R = parent(z)
   ccall((:fmpz_mod_add, libflint), Nothing,
	 (Ref{fmpz}, Ref{fmpz}, Ref{fmpz}, Ref{fmpz_mod_ctx_struct}),
	    z.data, z.data, x.data, R.ninv)
   return z
end

function add!(z::fmpz_mod, x::fmpz_mod, y::fmpz_mod)
   R = parent(z)
   ccall((:fmpz_mod_add, libflint), Nothing,
	(Ref{fmpz}, Ref{fmpz}, Ref{fmpz}, Ref{fmpz_mod_ctx_struct}),
	   z.data, x.data, y.data, R.ninv)
   return z
end

###############################################################################
#
#   Random functions
#
###############################################################################

# define rand(::FmpzModRing)

Random.Sampler(::Type{RNG}, R::FmpzModRing, n::Random.Repetition) where {RNG<:AbstractRNG} =
   Random.SamplerSimple(R, Random.Sampler(RNG, BigInt(0):BigInt(R.n)-1, n))

function rand(rng::AbstractRNG, R::Random.SamplerSimple{FmpzModRing})
   n = rand(rng, R.data)
   fmpz_mod(fmpz(n), R[])
end

Random.gentype(::Type{FmpzModRing}) = elem_type(FmpzModRing)

# define rand(make(::FmpzModRing, arr)), where arr is any abstract array with integer or fmpz entries

RandomExtensions.maketype(R::FmpzModRing, _) = elem_type(R)

rand(rng::AbstractRNG, sp::SamplerTrivial{<:Make2{fmpz_mod,FmpzModRing,<:AbstractArray{<:IntegerUnion}}}) =
   sp[][1](rand(rng, sp[][2]))

# define rand(::FmpzModRing, arr), where arr is any abstract array with integer or fmpz entries

rand(r::Random.AbstractRNG, R::FmpzModRing, b::AbstractArray) = rand(r, make(R, b))

rand(R::FmpzModRing, b::AbstractArray) = rand(Random.GLOBAL_RNG, R, b)

###############################################################################
#
#   Promotions
#
###############################################################################

promote_rule(::Type{fmpz_mod}, ::Type{T}) where T <: Integer = fmpz_mod

###############################################################################
#
#   Parent object call overload
#
###############################################################################

function (R::FmpzModRing)()
   return fmpz_mod(fmpz(0), R)
end

function (R::FmpzModRing)(a::Integer)
   n = R.n
   d = fmpz(a)%n
   if d < 0
      d += n
   end
   return fmpz_mod(d, R)
end

function (R::FmpzModRing)(a::fmpz)
   d = fmpz()
   ccall((:fmpz_mod, libflint), Nothing, (Ref{fmpz}, Ref{fmpz}, Ref{fmpz}),
             d, a, R.n)
   return fmpz_mod(d, R)
end

function (R::FmpzModRing)(a::fmpz_mod)
   return a
end

###############################################################################
#
#   fmpz_mod constructor
#
###############################################################################

function ResidueRing(R::FlintIntegerRing, n::fmpz; cached::Bool=true)
   # Modulus of zero cannot be supported. E.g. Flint library could not be expected to
   # do matrices over Z/0 using a Z/nZ type. The former is multiprecision, the latter not.
   n <= 0 && throw(DomainError(n, "Modulus must be positive"))
   return FmpzModRing(n, cached)
end

function ResidueRing(R::FlintIntegerRing, n::Integer; cached::Bool=true)
   return ResidueRing(R, fmpz(n))
end
