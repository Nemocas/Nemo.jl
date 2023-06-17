###############################################################################
#
#   gfp_elem.jl : Nemo gfp_elem (integers modulo small n)
#
###############################################################################

export fpFieldElem

###############################################################################
#
#   Type and parent object methods
#
###############################################################################

parent_type(::Type{fpFieldElem}) = fpField

elem_type(::Type{fpField}) = fpFieldElem

base_ring(a::fpField) = Union{}

base_ring(a::fpFieldElem) = Union{}

parent(a::fpFieldElem) = a.parent

function check_parent(a::fpFieldElem, b::fpFieldElem)
   a.parent != b.parent && error("Operations on distinct Galois fields not supported")
end

is_domain_type(::Type{fpFieldElem}) = true

###############################################################################
#
#   Basic manipulation
#
###############################################################################

function Base.hash(a::fpFieldElem, h::UInt)
   b = 0x749c75e438001387%UInt
   return xor(xor(hash(a.data), h), b)
end

data(a::fpFieldElem) = a.data

lift(a::fpFieldElem) = ZZRingElem(data(a))

function zero(R::fpField)
   return fpFieldElem(UInt(0), R)
end

function one(R::fpField)
   return fpFieldElem(UInt(1), R)
end

iszero(a::fpFieldElem) = a.data == 0

isone(a::fpFieldElem) = a.data == 1

is_unit(a::fpFieldElem) = a.data != 0

modulus(R::fpField) = R.n

function deepcopy_internal(a::fpFieldElem, dict::IdDict)
   R = parent(a)
   return fpFieldElem(deepcopy(a.data), R)
end

order(R::fpField) = ZZRingElem(R.n)

characteristic(R::fpField) = ZZRingElem(R.n)

degree(::fpField) = 1

###############################################################################
#
#   Canonicalisation
#
###############################################################################

function canonical_unit(x::fpFieldElem)
  return x
end

###############################################################################
#
#   AbstractString I/O
#
###############################################################################

function show(io::IO, a::fpField)
   if get(io, :supercompact, false)
      # no nested printing
      io = pretty(io)
      print(io, LowercaseOff(), "GF($(signed(widen(a.n))))")
   else
      # nested printing allowed, preferably supercompact
      print(io, "Finite field of characteristic ", signed(widen(a.n)))
   end
end

function expressify(a::fpFieldElem; context = nothing)
    return a.data
end

function show(io::IO, a::fpFieldElem)
   print(io, signed(widen(a.data)))
end

###############################################################################
#
#   Unary operations
#
###############################################################################

function -(x::fpFieldElem)
   if x.data == 0
      return deepcopy(x)
   else
      R = parent(x)
      return fpFieldElem(R.n - x.data, R)
   end
end

###############################################################################
#
#   Binary operations
#
###############################################################################

function +(x::fpFieldElem, y::fpFieldElem)
   check_parent(x, y)
   R = parent(x)
   n = modulus(R)
   d = x.data + y.data - n
   if d > x.data
      return fpFieldElem(d + n, R)
   else
      return fpFieldElem(d, R)
   end
end

function -(x::fpFieldElem, y::fpFieldElem)
   check_parent(x, y)
   R = parent(x)
   n = modulus(R)
   d = x.data - y.data
   if d > x.data
      return fpFieldElem(d + n, R)
   else
      return fpFieldElem(d, R)
   end
end

function *(x::fpFieldElem, y::fpFieldElem)
   check_parent(x, y)
   R = parent(x)
   d = ccall((:n_mulmod2_preinv, libflint), UInt, (UInt, UInt, UInt, UInt),
             x.data, y.data, R.n, R.ninv)
   return fpFieldElem(d, R)
end

###############################################################################
#
#   Ad hoc binary operators
#
###############################################################################

function *(x::Integer, y::fpFieldElem)
   R = parent(y)
   return R(widen(x)*signed(widen(y.data)))
end

*(x::fpFieldElem, y::Integer) = y*x

function *(x::Int, y::fpFieldElem)
   R = parent(y)
   if x < 0
      d = ccall((:n_mulmod2_preinv, libflint), UInt, (UInt, UInt, UInt, UInt),
             UInt(-x), y.data, R.n, R.ninv)
      return -fpFieldElem(d, R)
   else
      d = ccall((:n_mulmod2_preinv, libflint), UInt, (UInt, UInt, UInt, UInt),
             UInt(x), y.data, R.n, R.ninv)
      return fpFieldElem(d, R)
   end
end

*(x::fpFieldElem, y::Int) = y*x

function *(x::UInt, y::fpFieldElem)
   R = parent(y)
   d = ccall((:n_mulmod2_preinv, libflint), UInt, (UInt, UInt, UInt, UInt),
             UInt(x), y.data, R.n, R.ninv)
   return fpFieldElem(d, R)
end

*(x::fpFieldElem, y::UInt) = y*x

+(x::fpFieldElem, y::Integer) = x + parent(x)(y)

+(x::Integer, y::fpFieldElem) = y + x

-(x::fpFieldElem, y::Integer) = x - parent(x)(y)

-(x::Integer, y::fpFieldElem) = parent(y)(x) - y

*(x::ZZRingElem, y::fpFieldElem) = BigInt(x)*y

*(x::fpFieldElem, y::ZZRingElem) = y*x

+(x::fpFieldElem, y::ZZRingElem) = x + parent(x)(y)

+(x::ZZRingElem, y::fpFieldElem) = y + x

-(x::fpFieldElem, y::ZZRingElem) = x - parent(x)(y)

-(x::ZZRingElem, y::fpFieldElem) = parent(y)(x) - y

###############################################################################
#
#   Powering
#
###############################################################################

function ^(x::fpFieldElem, y::Int)
   R = parent(x)
   if y < 0
      x = inv(x)
      y = -y
   end
   d = ccall((:n_powmod2_preinv, libflint), UInt, (UInt, Int, UInt, UInt),
             UInt(x.data), y, R.n, R.ninv)
   return fpFieldElem(d, R)
end

###############################################################################
#
#   Comparison
#
###############################################################################

function ==(x::fpFieldElem, y::fpFieldElem)
   check_parent(x, y)
   return x.data == y.data
end

###############################################################################
#
#   Ad hoc comparison
#
###############################################################################

==(x::fpFieldElem, y::Integer) = x == parent(x)(y)

==(x::Integer, y::fpFieldElem) = parent(y)(x) == y

==(x::fpFieldElem, y::ZZRingElem) = x == parent(x)(y)

==(x::ZZRingElem, y::fpFieldElem) = parent(y)(x) == y

###############################################################################
#
#   Inversion
#
###############################################################################

function inv(x::fpFieldElem)
   R = parent(x)
   iszero(x) && throw(DivideError())
   xinv = ccall((:n_invmod, libflint), UInt, (UInt, UInt),
            x.data, R.n)
   return fpFieldElem(xinv, R)
end

###############################################################################
#
#   Exact division
#
###############################################################################

function divexact(x::fpFieldElem, y::fpFieldElem; check::Bool=true)
   check_parent(x, y)
   y == 0 && throw(DivideError())
   R = parent(x)
   yinv = ccall((:n_invmod, libflint), UInt, (UInt, UInt),
           y.data, R.n)
   d = ccall((:n_mulmod2_preinv, libflint), UInt, (UInt, UInt, UInt, UInt),
             x.data, yinv, R.n, R.ninv)
   return fpFieldElem(d, R)
end

function divides(a::fpFieldElem, b::fpFieldElem)
   check_parent(a, b)
   if iszero(a)
      return true, a
   end
   if iszero(b)
      return false, a
   end
   return true, divexact(a, b)
end

###############################################################################
#
#   Square root
#
###############################################################################

function Base.sqrt(a::fpFieldElem; check::Bool=true)
   R = parent(a)
   if iszero(a)
      return zero(R)
   end
   r = ccall((:n_sqrtmod, libflint), UInt, (UInt, UInt), a.data, R.n)
   check && iszero(r) && error("Not a square in sqrt")
   return fpFieldElem(r, R)
end

function is_square(a::fpFieldElem)
   R = parent(a)
   if iszero(a) || R.n == 2
      return true
   end
   r = ccall((:n_jacobi, libflint), Cint, (UInt, UInt), a.data, R.n)
   return isone(r)
end

function is_square_with_sqrt(a::fpFieldElem)
   R = parent(a)
   if iszero(a) || R.n == 2
      return true, a
   end
   r = ccall((:n_sqrtmod, libflint), UInt, (UInt, UInt), a.data, R.n)
   if iszero(r)
      return false, zero(R)
   end
   return true, fpFieldElem(r, R)
end

###############################################################################
#
#   Unsafe functions
#
###############################################################################

function zero!(z::fpFieldElem)
   R = parent(z)
   return fpFieldElem(UInt(0), R)
end

function mul!(z::fpFieldElem, x::fpFieldElem, y::fpFieldElem)
   return x*y
end

function addeq!(z::fpFieldElem, x::fpFieldElem)
   return z + x
end

function add!(z::fpFieldElem, x::fpFieldElem, y::fpFieldElem)
   return x + y
end

###############################################################################
#
#   Random functions
#
###############################################################################

# define rand(::fpField)

Random.Sampler(::Type{RNG}, R::fpField, n::Random.Repetition) where {RNG<:AbstractRNG} =
   Random.SamplerSimple(R, Random.Sampler(RNG, UInt(0):R.n - 1, n))

rand(rng::AbstractRNG, R::Random.SamplerSimple{fpField}) =
   fpFieldElem(rand(rng, R.data), R[])

# define rand(make(::fpField, arr)), where arr is any abstract array with integer or ZZRingElem entries

RandomExtensions.maketype(R::fpField, _) = elem_type(R)

rand(rng::AbstractRNG, sp::SamplerTrivial{<:Make2{fpFieldElem,fpField,<:AbstractArray{<:IntegerUnion}}}) =
   sp[][1](rand(rng, sp[][2]))

# define rand(::fpField, arr), where arr is any abstract array with integer or ZZRingElem entries

rand(rng::AbstractRNG, R::fpField, b::AbstractArray) = rand(rng, make(R, b))

rand(R::fpField, b::AbstractArray) = rand(Random.GLOBAL_RNG, R, b)

###############################################################################
#
#   Promotions
#
###############################################################################

promote_rule(::Type{fpFieldElem}, ::Type{T}) where T <: Integer = fpFieldElem

promote_rule(::Type{fpFieldElem}, ::Type{ZZRingElem}) = fpFieldElem

###############################################################################
#
#   Parent object call overload
#
###############################################################################

function (R::fpField)()
   return fpFieldElem(UInt(0), R)
end

function (R::fpField)(a::Integer)
   n = R.n
   d = a%signed(widen(n))
   if d < 0
      d += n
   end
   return fpFieldElem(UInt(d), R)
end

function (R::fpField)(a::Int)
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
   return fpFieldElem(d, R)
end

function (R::fpField)(a::UInt)
   n = R.n
   ninv = R.ninv
   a = ccall((:n_mod2_preinv, libflint), UInt, (UInt, UInt, UInt),
             a, n, ninv)
   return fpFieldElem(a, R)
end

function (R::fpField)(a::ZZRingElem)
   d = ccall((:fmpz_fdiv_ui, libflint), UInt, (Ref{ZZRingElem}, UInt),
             a, R.n)
   return fpFieldElem(d, R)
end

function (R::fpField)(a::QQFieldElem)
   num = numerator(a, false)
   den = denominator(a, false)
   n = ccall((:fmpz_fdiv_ui, libflint), UInt, (Ref{ZZRingElem}, UInt),
             num, R.n)
   d = ccall((:fmpz_fdiv_ui, libflint), UInt, (Ref{ZZRingElem}, UInt),
             den, R.n)
   V = [UInt(0)]
   g = ccall((:n_gcdinv, libflint), UInt, (Ptr{UInt}, UInt, UInt), V, d, R.n)
   g != 1 && error("Unable to coerce")
   return R(n)*R(V[1])
end

function (R::fpField)(a::Union{fpFieldElem, zzModRingElem, FpFieldElem, ZZModRingElem})
   S = parent(a)
   if S === R
      return a
   else
      is_divisible_by(modulus(S), modulus(R)) || error("incompatible parents")
      return R(data(a))
   end
end

function (R::fpField)(a::Vector{<:IntegerUnion})
   is_one(length(a)) || error("Coercion impossible")
   return R(a[1])
end

###############################################################################
#
#   GF constructor
#
###############################################################################

function GF(n::Int; cached::Bool=true)
   (n <= 0) && throw(DomainError(n, "Characteristic must be positive"))
   un = UInt(n)
   !is_prime(un) && throw(DomainError(n, "Characteristic must be prime"))
   return fpField(un, cached)
end

function GF(n::UInt; cached::Bool=true)
   un = UInt(n)
   !is_prime(un) && throw(DomainError(n, "Characteristic must be prime"))
   return fpField(un, cached)
end
