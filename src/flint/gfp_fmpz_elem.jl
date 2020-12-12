###############################################################################
#
#   gfp_fmpz_elem.jl : Galois fields Z/pZ (large n)
#
###############################################################################

###############################################################################
#
#   Type and parent object methods
#
###############################################################################

parent_type(::Type{gfp_fmpz_elem}) = GaloisFmpzField

elem_type(::Type{GaloisFmpzField}) = gfp_fmpz_elem

base_ring(a::GaloisFmpzField) = Union{}

base_ring(a::gfp_fmpz_elem) = Union{}

parent(a::gfp_fmpz_elem) = a.parent

function check_parent(a::gfp_fmpz_elem, b::gfp_fmpz_elem)
   a.parent != b.parent && error("Operations on distinct residue rings not supported")
end

isdomain_type(::Type{gfp_fmpz_elem}) = true

###############################################################################
#
#   Basic manipulation
#
###############################################################################

function Base.hash(a::gfp_fmpz_elem, h::UInt)
   b = 0x6b61d2959976f517%UInt
   return xor(xor(hash(a.data), h), b)
end

iszero(a::gfp_fmpz_elem) = a.data == 0

isone(a::gfp_fmpz_elem) = a.data == 1

function zero(R::GaloisFmpzField)
   return gfp_fmpz_elem(fmpz(0), R)
end

function one(R::GaloisFmpzField)
   if R.n == 1
      return gfp_fmpz_elem(fmpz(0), R)
   else
      return gfp_fmpz_elem(fmpz(1), R)
   end
end

isunit(a::gfp_fmpz_elem) = a.data != 0

modulus(R::GaloisFmpzField) = R.n

@doc Markdown.doc"""
    characteristic(F::GaloisFmpzField)

Return the characteristic of the given Galois field.
"""
characteristic(F::GaloisFmpzField) = modulus(F)

@doc Markdown.doc"""
    order(F::GaloisFmpzField)

Return the order, i.e. the number of elements in, the given Galois field.
"""
order(F::GaloisFmpzField) = modulus(F)

function deepcopy_internal(a::gfp_fmpz_elem, dict::IdDict)
   R = parent(a)
   return gfp_fmpz_elem(deepcopy(a.data), R)
end

###############################################################################
#
#   Canonicalisation
#
###############################################################################

canonical_unit(x::gfp_fmpz_elem) = x

###############################################################################
#
#   AbstractString I/O
#
###############################################################################

function show(io::IO, R::GaloisFmpzField)
   print(io, "Galois field with characteristic ", R.n)
end

function show(io::IO, a::gfp_fmpz_elem)
   print(io, a.data)
end

needs_parentheses(x::gfp_fmpz_elem) = false

displayed_with_minus_in_front(x::gfp_fmpz_elem) = false

show_minus_one(::Type{gfp_fmpz_elem}) = true

###############################################################################
#
#   Unary operations
#
###############################################################################

function -(x::gfp_fmpz_elem)
   if x.data == 0
      return deepcopy(x)
   else
      R = parent(x)
      return gfp_fmpz_elem(R.n - x.data, R)
   end
end

###############################################################################
#
#   Binary operations
#
###############################################################################

function +(x::gfp_fmpz_elem, y::gfp_fmpz_elem)
   check_parent(x, y)
   R = parent(x)
   n = modulus(R)
   d = x.data + y.data - n
   if d < 0
      return gfp_fmpz_elem(d + n, R)
   else
      return gfp_fmpz_elem(d, R)
   end
end

function -(x::gfp_fmpz_elem, y::gfp_fmpz_elem)
   check_parent(x, y)
   R = parent(x)
   n = modulus(R)
   d = x.data - y.data
   if d < 0
      return gfp_fmpz_elem(d + n, R)
   else
      return gfp_fmpz_elem(d, R)
   end
end

function *(x::gfp_fmpz_elem, y::gfp_fmpz_elem)
   check_parent(x, y)
   R = parent(x)
   d = fmpz()
   ccall((:fmpz_mod_mul, libflint), Nothing,
         (Ref{fmpz}, Ref{fmpz}, Ref{fmpz}, Ref{fmpz_mod_ctx_struct}),
				                d, x.data, y.data, R.ninv)
   return gfp_fmpz_elem(d, R)
end

###############################################################################
#
#   Ad hoc binary operators
#
###############################################################################

function *(x::Integer, y::gfp_fmpz_elem)
   R = parent(y)
   return R(x*y.data)
end

*(x::gfp_fmpz_elem, y::Integer) = y*x

*(x::gfp_fmpz_elem, y::fmpz) = x*parent(x)(y)

*(x::fmpz, y::gfp_fmpz_elem) = y*x

+(x::gfp_fmpz_elem, y::Integer) = x + parent(x)(y)

+(x::Integer, y::gfp_fmpz_elem) = y + x

+(x::gfp_fmpz_elem, y::fmpz) = x + parent(x)(y)

+(x::fmpz, y::gfp_fmpz_elem) = y + x

-(x::gfp_fmpz_elem, y::Integer) = x - parent(x)(y)

-(x::Integer, y::gfp_fmpz_elem) = parent(y)(x) - y

-(x::gfp_fmpz_elem, y::fmpz) = x - parent(x)(y)

-(x::fmpz, y::gfp_fmpz_elem) = parent(y)(x) - y

###############################################################################
#
#   Powering
#
###############################################################################

function ^(x::gfp_fmpz_elem, y::Int)
   R = parent(x)
   if y < 0
      x = inv(x)
      y = -y
   end
   d = fmpz()
   ccall((:fmpz_mod_pow_ui, libflint), Nothing,
	 (Ref{fmpz}, Ref{fmpz}, UInt, Ref{fmpz_mod_ctx_struct}),
						 d, x.data, y, R.ninv)
   return gfp_fmpz_elem(d, R)
end

###############################################################################
#
#   Comparison
#
###############################################################################

function ==(x::gfp_fmpz_elem, y::gfp_fmpz_elem)
   check_parent(x, y)
   return x.data == y.data
end

###############################################################################
#
#   Ad hoc comparison
#
###############################################################################

==(x::gfp_fmpz_elem, y::Integer) = x == parent(x)(y)

==(x::Integer, y::gfp_fmpz_elem) = parent(y)(x) == y

==(x::gfp_fmpz_elem, y::fmpz) = x == parent(x)(y)

==(x::fmpz, y::gfp_fmpz_elem) = parent(y)(x) == y

###############################################################################
#
#   Inversion
#
###############################################################################

function inv(x::gfp_fmpz_elem)
   R = parent(x)
   iszero(x) && throw(DivideError())
   s = fmpz()
   g = fmpz()
   ccall((:fmpz_gcdinv, libflint), Nothing,
	 (Ref{fmpz}, Ref{fmpz}, Ref{fmpz}, Ref{fmpz}),
					         g, s, x.data, R.n)
   return gfp_fmpz_elem(s, R)
end

###############################################################################
#
#   Exact division
#
###############################################################################

function divexact(x::gfp_fmpz_elem, y::gfp_fmpz_elem)
   check_parent(x, y)
   iszero(y) && throw(DivideError())
   return x*inv(y)
end

function divides(a::gfp_fmpz_elem, b::gfp_fmpz_elem)
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
#   Unsafe functions
#
###############################################################################

function zero!(z::gfp_fmpz_elem)
   ccall((:fmpz_set_ui, libflint), Nothing,
	 (Ref{fmpz}, UInt), z.data, UInt(0))
   return z
end

function mul!(z::gfp_fmpz_elem, x::gfp_fmpz_elem, y::gfp_fmpz_elem)
   R = parent(z)
   ccall((:fmpz_mod_mul, libflint), Nothing,
	 (Ref{fmpz}, Ref{fmpz}, Ref{fmpz}, Ref{fmpz_mod_ctx_struct}),
					   z.data, x.data, y.data, R.ninv)
   return z
end

function addeq!(z::gfp_fmpz_elem, x::gfp_fmpz_elem)
   R = parent(z)
   ccall((:fmpz_mod_add, libflint), Nothing,
         (Ref{fmpz}, Ref{fmpz}, Ref{fmpz}, Ref{fmpz_mod_ctx_struct}),
		                          z.data, z.data, x.data, R.ninv)
   return z
end

function add!(z::gfp_fmpz_elem, x::gfp_fmpz_elem, y::gfp_fmpz_elem)
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

# define rand(::GaloisFmpzField)

Random.Sampler(::Type{RNG}, R::GaloisFmpzField, n::Random.Repetition) where {RNG<:AbstractRNG} =
   Random.SamplerSimple(R, Random.Sampler(RNG, BigInt(0):BigInt(R.n)-1, n))

function rand(rng::AbstractRNG, R::Random.SamplerSimple{GaloisFmpzField})
   n = rand(rng, R.data)
   gfp_fmpz_elem(fmpz(n), R[])
end

# define rand(make(::GaloisFmpzField, n:m))

RandomExtensions.maketype(R::GaloisFmpzField, _) = elem_type(R)

rand(rng::AbstractRNG,
     sp::SamplerTrivial{<:Make2{gfp_fmpz_elem,GaloisFmpzField,UnitRange{Int}}}) =
        sp[][1](rand(rng, sp[][2]))

# define rand(::GaloisFmpzField, n:m)

rand(r::Random.AbstractRNG, R::GaloisFmpzField, b::UnitRange{Int}) = rand(r, make(R, b))

rand(R::GaloisFmpzField, b::UnitRange{Int}) = rand(Random.GLOBAL_RNG, R, b)

###############################################################################
#
#   Promotions
#
###############################################################################

function promote_rule(::Type{gfp_fmpz_elem}, ::Type{T}) where T <: Integer
   return gfp_fmpz_elem
end

###############################################################################
#
#   Parent object call overload
#
###############################################################################

function (R::GaloisFmpzField)()
   return gfp_fmpz_elem(fmpz(0), R)
end

function (R::GaloisFmpzField)(a::Integer)
   n = R.n
   d = fmpz(a)%n
   if d < 0
      d += n
   end
   return gfp_fmpz_elem(d, R)
end

function (R::GaloisFmpzField)(a::fmpz)
   d = fmpz()
   ccall((:fmpz_mod, libflint), Nothing, (Ref{fmpz}, Ref{fmpz}, Ref{fmpz}),
				              d, a, R.n)
   return gfp_fmpz_elem(d, R)
end

function (R::GaloisFmpzField)(a::gfp_fmpz_elem)
   return a
end

###############################################################################
#
#   GF constructor
#
###############################################################################

function GF(n::fmpz; cached::Bool=true)
   (n <= 0) && throw(DomainError(n, "Characteristic must be positive"))
   !isprobable_prime(n) && throw(DomainError(n, "Characteristic must be prime"))
   return GaloisFmpzField(n, cached)
end
