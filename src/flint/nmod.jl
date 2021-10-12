###############################################################################
#
#   nmod.jl : Nemo nmod (integers modulo small n)
#
###############################################################################

export nmod

###############################################################################
#
#   Type and parent object methods
#
###############################################################################

parent_type(::Type{nmod}) = NmodRing

elem_type(::Type{NmodRing}) = nmod

base_ring(a::NmodRing) = FlintZZ

base_ring(a::nmod) = FlintZZ

parent(a::nmod) = a.parent

function check_parent(a::nmod, b::nmod)
   a.parent != b.parent && error("Operations on distinct residue rings not supported")
end

###############################################################################
#
#   Basic manipulation
#
###############################################################################

function Base.hash(a::nmod, h::UInt)
   b = 0x1812aa3492cbf4d9%UInt
   return xor(xor(hash(a.data), h), b)
end

lift(a::nmod) = fmpz(data(a))

function zero(R::NmodRing)
   return nmod(UInt(0), R)
end

function one(R::NmodRing)
   if R.n == 1
      return nmod(UInt(0), R)
   else
      return nmod(UInt(1), R)
   end
end

iszero(a::nmod) = a.data == 0

isone(a::nmod) = a.parent.n == 1 ? a.data == 0 : a.data == 1

isunit(a::nmod) = a.parent.n == 1 ? a.data == 0 : gcd(a.data, a.parent.n) == 1

modulus(R::NmodRing) = R.n

function deepcopy_internal(a::nmod, dict::IdDict)
   R = parent(a)
   return nmod(deepcopy(a.data), R)
end

characteristic(R::NmodRing) = fmpz(modulus(R))

###############################################################################
#
#   Canonicalisation
#
###############################################################################

function canonical_unit(x::nmod)
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
    r = crt(fmpz(1), fmpz(a), fmpz(u), fmpz(b))
  end
  return parent(x)(r)
end

###############################################################################
#
#   AbstractString I/O
#
###############################################################################

function show(io::IO, R::NmodRing)
   print(io, "Integers modulo ", signed(widen(R.n)))
end

function expressify(a::Nemo.nmod; context = nothing)
    return a.data
end

function show(io::IO, a::nmod)
   print(io, signed(widen(a.data)))
end

needs_parentheses(x::nmod) = false

###############################################################################
#
#   Unary operations
#
###############################################################################

function -(x::nmod)
   if x.data == 0
      return deepcopy(x)
   else
      R = parent(x)
      return nmod(R.n - x.data, R)
   end
end

###############################################################################
#
#   Binary operations
#
###############################################################################

function +(x::nmod, y::nmod)
   check_parent(x, y)
   R = parent(x)
   n = modulus(R)
   d = x.data + y.data - n
   if d > x.data
      return nmod(d + n, R)
   else
      return nmod(d, R)
   end
end

function -(x::nmod, y::nmod)
   check_parent(x, y)
   R = parent(x)
   n = modulus(R)
   d = x.data - y.data
   if d > x.data
      return nmod(d + n, R)
   else
      return nmod(d, R)
   end
end

function *(x::nmod, y::nmod)
   check_parent(x, y)
   R = parent(x)
   d = ccall((:n_mulmod2_preinv, libflint), UInt, (UInt, UInt, UInt, UInt),
             x.data, y.data, R.n, R.ninv)
   return nmod(d, R)
end

###############################################################################
#
#   Ad hoc binary operators
#
###############################################################################

function *(x::Integer, y::nmod)
   R = parent(y)
   return R(widen(x)*signed(widen(y.data)))
end

*(x::nmod, y::Integer) = y*x

function *(x::Int, y::nmod)
   R = parent(y)
   if x < 0
      d = ccall((:n_mulmod2_preinv, libflint), UInt, (UInt, UInt, UInt, UInt),
             UInt(-x), y.data, R.n, R.ninv)
      return -nmod(d, R)
   else
      d = ccall((:n_mulmod2_preinv, libflint), UInt, (UInt, UInt, UInt, UInt),
             UInt(x), y.data, R.n, R.ninv)
      return nmod(d, R)
   end
end

*(x::nmod, y::Int) = y*x

function *(x::UInt, y::nmod)
   R = parent(y)
   d = ccall((:n_mulmod2_preinv, libflint), UInt, (UInt, UInt, UInt, UInt),
             UInt(x), y.data, R.n, R.ninv)
   return nmod(d, R)
end

*(x::nmod, y::UInt) = y*x

+(x::nmod, y::Integer) = x + parent(x)(y)

+(x::Integer, y::nmod) = y + x

-(x::nmod, y::Integer) = x - parent(x)(y)

-(x::Integer, y::nmod) = parent(y)(x) - y

###############################################################################
#
#   Powering
#
###############################################################################

function ^(x::nmod, y::Int)
   R = parent(x)
   if y < 0
      x = inv(x)
      y = -y
   end
   d = ccall((:n_powmod2_preinv, libflint), UInt, (UInt, Int, UInt, UInt),
             UInt(x.data), y, R.n, R.ninv)
   return nmod(d, R)
end

###############################################################################
#
#   Comparison
#
###############################################################################

function ==(x::nmod, y::nmod)
   check_parent(x, y)
   return x.data == y.data
end

###############################################################################
#
#   Ad hoc comparison
#
###############################################################################

==(x::nmod, y::Integer) = x == parent(x)(y)

==(x::Integer, y::nmod) = parent(y)(x) == y

==(x::nmod, y::fmpz) = x == parent(x)(y)

==(x::fmpz, y::nmod) = parent(y)(x) == y

###############################################################################
#
#   Inversion
#
###############################################################################

function inv(x::nmod)
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
   return nmod(s[], R)
end

###############################################################################
#
#   Exact division
#
###############################################################################

function divexact(x::nmod, y::nmod; check::Bool=true)
   check_parent(x, y)
   fl, q = divides(x, y)
   if !fl
     error("Impossible inverse in ", parent(x))
   end
   return q
end

function divides(a::nmod, b::nmod)
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

function gcd(x::nmod, y::nmod)
   check_parent(x, y)
   R = parent(x)
   d = gcd(gcd(x.data, R.n), y.data)
   if d == R.n
      return nmod(0, R)
   else
      return nmod(d, R)
   end
end

@doc Markdown.doc"""
    gcdx(a::nmod, b::nmod)

Compute the extended gcd with the Euclidean structure inherited from
$\mathbb{Z}$.
"""
function gcdx(a::nmod, b::nmod)
   m = modulus(a)
   R = parent(a)
   g, u, v = gcdx(fmpz(a.data), fmpz(b.data))
   G, U, V = gcdx(g, fmpz(m))
   return R(G), R(U)*R(u), R(U)*R(v)
end

###############################################################################
#
#   Unsafe functions
#
###############################################################################

function zero!(z::nmod)
   R = parent(z)
   return nmod(UInt(0), R)
end

function mul!(z::nmod, x::nmod, y::nmod)
   return x*y
end

function addeq!(z::nmod, x::nmod)
   return z + x
end

function add!(z::nmod, x::nmod, y::nmod)
   return x + y
end

###############################################################################
#
#   Random functions
#
###############################################################################

# define rand(::NmodRing)

Random.Sampler(::Type{RNG}, R::NmodRing, n::Random.Repetition) where {RNG<:AbstractRNG} =
   Random.SamplerSimple(R, Random.Sampler(RNG, UInt(0):R.n - 1, n))

rand(rng::AbstractRNG, R::Random.SamplerSimple{NmodRing}) = nmod(rand(rng, R.data), R[])

Random.gentype(::Type{NmodRing}) = elem_type(NmodRing)

# define rand(make(R::NmodRing, arr)), where arr is any abstract array with integer or fmpz entries

RandomExtensions.maketype(R::NmodRing, _) = elem_type(R)

rand(rng::AbstractRNG, sp::SamplerTrivial{<:Make2{nmod,NmodRing,<:AbstractArray{<:IntegerUnion}}}) =
   sp[][1](rand(rng, sp[][2]))

# define rand(R::NmodRing, arr), where arr is any abstract array with integer or fmpz entries

rand(r::Random.AbstractRNG, R::NmodRing, b::AbstractArray) = rand(r, make(R, b))

rand(R::NmodRing, b::AbstractArray) = rand(Random.GLOBAL_RNG, R, b)

###############################################################################
#
#   Promotions
#
###############################################################################

promote_rule(::Type{nmod}, ::Type{T}) where T <: Integer = nmod

###############################################################################
#
#   Parent object call overload
#
###############################################################################

function (R::NmodRing)()
   return nmod(UInt(0), R)
end

function (R::NmodRing)(a::Integer)
   n = R.n
   d = a%signed(widen(n))
   if d < 0
      d += n
   end
   return nmod(UInt(d), R)
end

function (R::NmodRing)(a::Int)
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
   return nmod(d, R)
end

function (R::NmodRing)(a::UInt)
   n = R.n
   ninv = R.ninv
   a = ccall((:n_mod2_preinv, libflint), UInt, (UInt, UInt, UInt),
             a, n, ninv)
   return nmod(a, R)
end

function (R::NmodRing)(a::fmpz)
   d = ccall((:fmpz_fdiv_ui, libflint), UInt, (Ref{fmpz}, UInt),
             a, R.n)
   return nmod(d, R)
end

function (R::NmodRing)(a::Union{gfp_elem, nmod, gfp_fmpz_elem, fmpz_mod})
   S = parent(a)
   if S === R
      return a
   else
      isdivisible_by(modulus(S), modulus(R)) || error("incompatible parents")
      return R(data(a))
   end
end

###############################################################################
#
#   nmod constructor
#
###############################################################################

function ResidueRing(R::FlintIntegerRing, n::Int; cached::Bool=true)
   # Modulus of zero cannot be supported. E.g. Flint library could not be expected to
   # do matrices over Z/0 using a Z/nZ type. The former is multiprecision, the latter not.
   n <= 0 && throw(DomainError(n, "Modulus must be positive"))
   return NmodRing(UInt(n), cached)
end

function ResidueRing(R::FlintIntegerRing, n::UInt; cached::Bool=true)
   return NmodRing(n, cached)
end
