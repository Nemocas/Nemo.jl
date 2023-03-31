###############################################################################
#
#   gfp_fmpz_elem.jl: Galois fields Z/pZ (large n)
#
###############################################################################

export FpFieldElem

###############################################################################
#
#   Type and parent object methods
#
###############################################################################

parent_type(::Type{FpFieldElem}) = FpField

elem_type(::Type{FpField}) = FpFieldElem

base_ring(a::FpField) = Union{}

base_ring(a::FpFieldElem) = Union{}

parent(a::FpFieldElem) = a.parent

function check_parent(a::FpFieldElem, b::FpFieldElem)
   a.parent != b.parent && error("Operations on distinct residue rings not supported")
end

is_domain_type(::Type{FpFieldElem}) = true

###############################################################################
#
#   Basic manipulation
#
###############################################################################

function Base.hash(a::FpFieldElem, h::UInt)
   b = 0x6b61d2959976f517%UInt
   return xor(xor(hash(a.data), h), b)
end

data(a::FpFieldElem) = a.data

lift(a::FpFieldElem) = data(a)

iszero(a::FpFieldElem) = iszero(a.data)

isone(a::FpFieldElem) = isone(a.data)

function zero(R::FpField)
   return FpFieldElem(zero(ZZRingElem), R)
end

function one(R::FpField)
   if R.n == 1
      return FpFieldElem(ZZRingElem(0), R)
   else
      return FpFieldElem(ZZRingElem(1), R)
   end
end

is_unit(a::FpFieldElem) = a.data != 0

modulus(R::FpField) = R.n

characteristic(F::FpField) = modulus(F)

order(F::FpField) = modulus(F)

degree(::FpField) = 1

function deepcopy_internal(a::FpFieldElem, dict::IdDict)
   R = parent(a)
   return FpFieldElem(deepcopy(a.data), R)
end

###############################################################################
#
#   Canonicalisation
#
###############################################################################

canonical_unit(x::FpFieldElem) = x

###############################################################################
#
#   AbstractString I/O
#
###############################################################################

function show(io::IO, a::FpField)
   if get(io, :supercompact, false)
      # no nested printing
      io = pretty(io)
      print(io, LowercaseOff(), "GF(", a.n, ")")
   else
      # nested printing allowed, preferably supercompact
      print(io, "Finite field of characteristic ", a.n)
   end
end

function expressify(a::FpFieldElem; context = nothing)
   return a.data
end

function show(io::IO, a::FpFieldElem)
   print(io, a.data)
end

###############################################################################
#
#   Unary operations
#
###############################################################################

function -(x::FpFieldElem)
   if iszero(x.data)
      return deepcopy(x)
   else
      R = parent(x)
      return FpFieldElem(R.n - x.data, R)
   end
end

###############################################################################
#
#   Binary operations
#
###############################################################################

function +(x::FpFieldElem, y::FpFieldElem)
   check_parent(x, y)
   R = parent(x)
   n = modulus(R)
   d = x.data + y.data - n
   if d < 0
      return FpFieldElem(d + n, R)
   else
      return FpFieldElem(d, R)
   end
end

function -(x::FpFieldElem, y::FpFieldElem)
   check_parent(x, y)
   R = parent(x)
   n = modulus(R)
   d = x.data - y.data
   if d < 0
      return FpFieldElem(d + n, R)
   else
      return FpFieldElem(d, R)
   end
end

function *(x::FpFieldElem, y::FpFieldElem)
   check_parent(x, y)
   R = parent(x)
   d = ZZRingElem()
   ccall((:fmpz_mod_mul, libflint), Nothing,
         (Ref{ZZRingElem}, Ref{ZZRingElem}, Ref{ZZRingElem}, Ref{fmpz_mod_ctx_struct}),
				                d, x.data, y.data, R.ninv)
   return FpFieldElem(d, R)
end

###############################################################################
#
#   Ad hoc binary operators
#
###############################################################################

function *(x::Integer, y::FpFieldElem)
   R = parent(y)
   return R(x*y.data)
end

*(x::FpFieldElem, y::Integer) = y*x

*(x::FpFieldElem, y::ZZRingElem) = x*parent(x)(y)

*(x::ZZRingElem, y::FpFieldElem) = y*x

+(x::FpFieldElem, y::Integer) = x + parent(x)(y)

+(x::Integer, y::FpFieldElem) = y + x

+(x::FpFieldElem, y::ZZRingElem) = x + parent(x)(y)

+(x::ZZRingElem, y::FpFieldElem) = y + x

-(x::FpFieldElem, y::Integer) = x - parent(x)(y)

-(x::Integer, y::FpFieldElem) = parent(y)(x) - y

-(x::FpFieldElem, y::ZZRingElem) = x - parent(x)(y)

-(x::ZZRingElem, y::FpFieldElem) = parent(y)(x) - y

###############################################################################
#
#   Powering
#
###############################################################################

function ^(x::FpFieldElem, y::Int)
   R = parent(x)
   if y < 0
      x = inv(x)
      y = -y
   end
   d = ZZRingElem()
   ccall((:fmpz_mod_pow_ui, libflint), Nothing,
	 (Ref{ZZRingElem}, Ref{ZZRingElem}, UInt, Ref{fmpz_mod_ctx_struct}),
						 d, x.data, y, R.ninv)
   return FpFieldElem(d, R)
end

function ^(x::FpFieldElem, y::ZZRingElem)
   R = parent(x)
   z = R()
   if 0 == ccall((:fmpz_mod_pow_fmpz, libflint), Cint,
                 (Ref{ZZRingElem}, Ref{ZZRingElem}, Ref{ZZRingElem}, Ref{fmpz_mod_ctx_struct}),
                 z.data, x.data, y, R.ninv)
      if iszero(x)
         throw(DivideError())
      else
         error("Impossible inverse")
      end
   end
   return z
end

function ^(x::FpFieldElem, y::Integer)
   return x^ZZRingElem(y)
end

###############################################################################
#
#   Comparison
#
###############################################################################

function ==(x::FpFieldElem, y::FpFieldElem)
   check_parent(x, y)
   return x.data == y.data
end

###############################################################################
#
#   Ad hoc comparison
#
###############################################################################

==(x::FpFieldElem, y::Integer) = x == parent(x)(y)

==(x::Integer, y::FpFieldElem) = parent(y)(x) == y

==(x::FpFieldElem, y::ZZRingElem) = x == parent(x)(y)

==(x::ZZRingElem, y::FpFieldElem) = parent(y)(x) == y

###############################################################################
#
#   Inversion
#
###############################################################################

function inv(x::FpFieldElem)
   R = parent(x)
   iszero(x) && throw(DivideError())
   s = ZZRingElem()
   g = ZZRingElem()
   ccall((:fmpz_gcdinv, libflint), Nothing,
	 (Ref{ZZRingElem}, Ref{ZZRingElem}, Ref{ZZRingElem}, Ref{ZZRingElem}),
					         g, s, x.data, R.n)
   return FpFieldElem(s, R)
end

###############################################################################
#
#   Exact division
#
###############################################################################

function divexact(x::FpFieldElem, y::FpFieldElem; check::Bool=true)
   check_parent(x, y)
   iszero(y) && throw(DivideError())
   return x*inv(y)
end

function divides(a::FpFieldElem, b::FpFieldElem)
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

function Base.sqrt(a::FpFieldElem; check::Bool=true)
   R = parent(a)
   if iszero(a)
      return zero(R)
   end
   z = ZZRingElem()
   flag = ccall((:fmpz_sqrtmod, libflint), Bool,
                (Ref{ZZRingElem}, Ref{ZZRingElem}, Ref{ZZRingElem}),
                 z, a.data, R.n)
   check && !flag && error("Not a square in sqrt")
   return FpFieldElem(z, R)
end

function is_square(a::FpFieldElem)
   R = parent(a)
   if iszero(a) || R.n == 2
      return true
   end
   r = ccall((:fmpz_jacobi, libflint), Cint, (Ref{ZZRingElem}, Ref{ZZRingElem}),
                                              a.data, R.n)
   return isone(r)
end

function is_square_with_sqrt(a::FpFieldElem)
   R = parent(a)
   if iszero(a) || R.n == 2
      return true, a
   end
   z = ZZRingElem()
   r = ccall((:fmpz_sqrtmod, libflint), Cint,
             (Ref{ZZRingElem}, Ref{ZZRingElem}, Ref{ZZRingElem}),
              z, a.data, R.n)
   if iszero(r)
      return false, zero(R)
   end
   return true, FpFieldElem(z, R)
end

###############################################################################
#
#   Unsafe functions
#
###############################################################################

function zero!(z::FpFieldElem)
   ccall((:fmpz_set_ui, libflint), Nothing,
	 (Ref{ZZRingElem}, UInt), z.data, UInt(0))
   return z
end

function mul!(z::FpFieldElem, x::FpFieldElem, y::FpFieldElem)
   R = parent(z)
   ccall((:fmpz_mod_mul, libflint), Nothing,
	 (Ref{ZZRingElem}, Ref{ZZRingElem}, Ref{ZZRingElem}, Ref{fmpz_mod_ctx_struct}),
					   z.data, x.data, y.data, R.ninv)
   return z
end

function addeq!(z::FpFieldElem, x::FpFieldElem)
   R = parent(z)
   ccall((:fmpz_mod_add, libflint), Nothing,
         (Ref{ZZRingElem}, Ref{ZZRingElem}, Ref{ZZRingElem}, Ref{fmpz_mod_ctx_struct}),
		                          z.data, z.data, x.data, R.ninv)
   return z
end

function add!(z::FpFieldElem, x::FpFieldElem, y::FpFieldElem)
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

# define rand(::FpField)

Random.Sampler(::Type{RNG}, R::FpField, n::Random.Repetition) where {RNG<:AbstractRNG} =
   Random.SamplerSimple(R, Random.Sampler(RNG, BigInt(0):BigInt(R.n)-1, n))

function rand(rng::AbstractRNG, R::Random.SamplerSimple{FpField})
   n = rand(rng, R.data)
   FpFieldElem(ZZRingElem(n), R[])
end

# define rand(make(::FpField, arr)), where arr is any abstract array with integer or ZZRingElem entries

RandomExtensions.maketype(R::FpField, _) = elem_type(R)

rand(rng::AbstractRNG,
     sp::SamplerTrivial{<:Make2{FpFieldElem,FpField,<:AbstractArray{<:IntegerUnion}}}) =
        sp[][1](rand(rng, sp[][2]))

# define rand(::FpField, arr), where arr is any abstract array with integer or ZZRingElem entries

rand(r::Random.AbstractRNG, R::FpField, b::AbstractArray) = rand(r, make(R, b))

rand(R::FpField, b::AbstractArray) = rand(Random.GLOBAL_RNG, R, b)

###############################################################################
#
#   Promotions
#
###############################################################################

function promote_rule(::Type{FpFieldElem}, ::Type{T}) where T <: Integer
   return FpFieldElem
end

promote_rule(::Type{FpFieldElem}, ::Type{ZZRingElem}) = FpFieldElem

###############################################################################
#
#   Parent object call overload
#
###############################################################################

function (R::FpField)()
   return FpFieldElem(ZZRingElem(0), R)
end

function (R::FpField)(a::Integer)
   n = R.n
   d = ZZRingElem(a)%n
   if d < 0
      d += n
   end
   return FpFieldElem(d, R)
end

function (R::FpField)(a::ZZRingElem)
   d = ZZRingElem()
   ccall((:fmpz_mod, libflint), Nothing, (Ref{ZZRingElem}, Ref{ZZRingElem}, Ref{ZZRingElem}),
				              d, a, R.n)
   return FpFieldElem(d, R)
end

function (R::FpField)(a::Union{fpFieldElem, zzModRingElem, FpFieldElem, ZZModRingElem})
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
#   GF constructor
#
###############################################################################

function GF(n::ZZRingElem; cached::Bool=true)
   (n <= 0) && throw(DomainError(n, "Characteristic must be positive"))
   !is_probable_prime(n) && throw(DomainError(n, "Characteristic must be prime"))
   return FpField(n, cached)
end
