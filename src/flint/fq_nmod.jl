###############################################################################
#
#   fq_nmod.jl : Flint finite fields
#
###############################################################################

export fq_nmod, FqNmodFiniteField

###############################################################################
#
#   Type and parent object methods
#
###############################################################################

parent_type(::Type{fq_nmod}) = FqNmodFiniteField

elem_type(::Type{FqNmodFiniteField}) = fq_nmod

base_ring(a::FqNmodFiniteField) = Union{}

base_ring(a::fq_nmod) = Union{}

parent(a::fq_nmod) = a.parent

isdomain_type(::Type{fq_nmod}) = true

function check_parent(a::fq_nmod, b::fq_nmod)
   a.parent != b.parent && error("Operations on distinct finite fields not supported")
end

###############################################################################
#
#   Basic manipulation
#
###############################################################################

function Base.hash(a::fq_nmod, h::UInt)
   b = 0x78e5f766c8ace18d%UInt
   for i in 0:degree(parent(a)) - 1
      b = xor(b, xor(hash(coeff(a, i), h), h))
      b = (b << 1) | (b >> (sizeof(Int)*8 - 1))
   end
   return b
end

function coeff(x::fq_nmod, n::Int)
   n < 0 && throw(DomainError(n, "Index must be non-negative"))
   return ccall((:nmod_poly_get_coeff_ui, libflint), UInt,
                (Ref{fq_nmod}, Int), x, n)
end

function zero(a::FqNmodFiniteField)
   d = a()
   ccall((:fq_nmod_zero, libflint), Nothing,
         (Ref{fq_nmod}, Ref{FqNmodFiniteField}), d, a)
   return d
end

function one(a::FqNmodFiniteField)
   d = a()
   ccall((:fq_nmod_one, libflint), Nothing,
         (Ref{fq_nmod}, Ref{FqNmodFiniteField}), d, a)
   return d
end

function gen(a::FqNmodFiniteField)
   d = a()
   ccall((:fq_nmod_gen, libflint), Nothing,
         (Ref{fq_nmod}, Ref{FqNmodFiniteField}), d, a)
   return d
end

iszero(a::fq_nmod) = ccall((:fq_nmod_is_zero, libflint), Bool,
                     (Ref{fq_nmod}, Ref{FqNmodFiniteField}), a, a.parent)

isone(a::fq_nmod) = ccall((:fq_nmod_is_one, libflint), Bool,
                    (Ref{fq_nmod}, Ref{FqNmodFiniteField}), a, a.parent)

isgen(a::fq_nmod) = a == gen(parent(a)) # there is no isgen in flint

isunit(a::fq_nmod) = ccall((:fq_nmod_is_invertible, libflint), Bool,
                     (Ref{fq_nmod}, Ref{FqNmodFiniteField}), a, a.parent)

function characteristic(a::FqNmodFiniteField)
   d = fmpz()
   ccall((:__fq_nmod_ctx_prime, libflint), Nothing,
         (Ref{fmpz}, Ref{FqNmodFiniteField}), d, a)
   return d
end

function order(a::FqNmodFiniteField)
   d = fmpz()
   ccall((:fq_nmod_ctx_order, libflint), Nothing,
         (Ref{fmpz}, Ref{FqNmodFiniteField}), d, a)
   return d
end

function degree(a::FqNmodFiniteField)
   return ccall((:fq_nmod_ctx_degree, libflint), Int,
                (Ref{FqNmodFiniteField},), a)
end

function deepcopy_internal(d::fq_nmod, dict::IdDict)
   z = fq_nmod(parent(d), d)
   z.parent = parent(d)
   return z
end

###############################################################################
#
#   Canonicalisation
#
###############################################################################

canonical_unit(x::fq_nmod) = x

###############################################################################
#
#   AbstractString I/O
#
###############################################################################

function AbstractAlgebra.expressify(a::fq_nmod; context = nothing)
   x = unsafe_string(reinterpret(Cstring, a.parent.var))
   d = degree(a.parent)

   sum = Expr(:call, :+)
   for k in (d - 1):-1:0
        c = coeff(a, k)
        if !iszero(c)
            xk = k < 1 ? 1 : k == 1 ? x : Expr(:call, :^, x, k)
            if isone(c)
                push!(sum.args, Expr(:call, :*, xk))
            else
                push!(sum.args, Expr(:call, :*, expressify(c, context = context), xk))
            end
        end
    end
    return sum
end

show(io::IO, a::fq_nmod) = print(io, AbstractAlgebra.obj_to_string(a, context = io))

function show(io::IO, a::FqNmodFiniteField)
   print(io, "Finite field of degree ", degree(a))
   print(io, " over F_", characteristic(a))
end

###############################################################################
#
#   Unary operations
#
###############################################################################

function -(x::fq_nmod)
   z = parent(x)()
   ccall((:fq_nmod_neg, libflint), Nothing,
       (Ref{fq_nmod}, Ref{fq_nmod}, Ref{FqNmodFiniteField}), z, x, x.parent)
   return z
end

###############################################################################
#
#   Binary operations
#
###############################################################################

function +(x::fq_nmod, y::fq_nmod)
   check_parent(x, y)
   z = parent(y)()
   ccall((:fq_nmod_add, libflint), Nothing,
         (Ref{fq_nmod}, Ref{fq_nmod}, Ref{fq_nmod}, Ref{FqNmodFiniteField}),
                                                       z, x, y, y.parent)
   return z
end

function -(x::fq_nmod, y::fq_nmod)
   check_parent(x, y)
   z = parent(y)()
   ccall((:fq_nmod_sub, libflint), Nothing,
         (Ref{fq_nmod}, Ref{fq_nmod}, Ref{fq_nmod}, Ref{FqNmodFiniteField}),
                                                       z, x, y, y.parent)
   return z
end

function *(x::fq_nmod, y::fq_nmod)
   check_parent(x, y)
   z = parent(y)()
   ccall((:fq_nmod_mul, libflint), Nothing,
         (Ref{fq_nmod}, Ref{fq_nmod}, Ref{fq_nmod}, Ref{FqNmodFiniteField}),
                                                       z, x, y, y.parent)
   return z
end

###############################################################################
#
#   Ad hoc binary operators
#
###############################################################################

function *(x::Int, y::fq_nmod)
   z = parent(y)()
   ccall((:fq_nmod_mul_si, libflint), Nothing,
         (Ref{fq_nmod}, Ref{fq_nmod}, Int, Ref{FqNmodFiniteField}),
                                               z, y, x, y.parent)
   return z
end

*(x::fq_nmod, y::Int) = y*x

*(x::Integer, y::fq_nmod) = fmpz(x)*y

*(x::fq_nmod, y::Integer) = y*x

function *(x::fmpz, y::fq_nmod)
   z = parent(y)()
   ccall((:fq_nmod_mul_fmpz, libflint), Nothing,
         (Ref{fq_nmod}, Ref{fq_nmod}, Ref{fmpz}, Ref{FqNmodFiniteField}),
                                                    z, y, x, y.parent)
   return z
end

*(x::fq_nmod, y::fmpz) = y*x

+(x::fq_nmod, y::Integer) = x + parent(x)(y)

+(x::Integer, y::fq_nmod) = y + x

+(x::fq_nmod, y::fmpz) = x + parent(x)(y)

+(x::fmpz, y::fq_nmod) = y + x

-(x::fq_nmod, y::Integer) = x - parent(x)(y)

-(x::Integer, y::fq_nmod) = parent(y)(x) - y

-(x::fq_nmod, y::fmpz) = x - parent(x)(y)

-(x::fmpz, y::fq_nmod) = parent(y)(x) - y

###############################################################################
#
#   Powering
#
###############################################################################

function ^(x::fq_nmod, y::Int)
   if y < 0
      x = inv(x)
      y = -y
   end
   z = parent(x)()
   ccall((:fq_nmod_pow_ui, libflint), Nothing,
         (Ref{fq_nmod}, Ref{fq_nmod}, Int, Ref{FqNmodFiniteField}),
                                               z, x, y, x.parent)
   return z
end

function ^(x::fq_nmod, y::fmpz)
   if y < 0
      x = inv(x)
      y = -y
   end
   z = parent(x)()
   ccall((:fq_nmod_pow, libflint), Nothing,
         (Ref{fq_nmod}, Ref{fq_nmod}, Ref{fmpz}, Ref{FqNmodFiniteField}),
                                                    z, x, y, x.parent)
   return z
end

###############################################################################
#
#   Comparison
#
###############################################################################

function ==(x::fq_nmod, y::fq_nmod)
   check_parent(x, y)
   ccall((:fq_nmod_equal, libflint), Bool,
       (Ref{fq_nmod}, Ref{fq_nmod}, Ref{FqNmodFiniteField}), x, y, y.parent)
end

###############################################################################
#
#   Ad hoc comparison
#
###############################################################################

==(x::fq_nmod, y::Integer) = x == parent(x)(y)

==(x::fq_nmod, y::fmpz) = x == parent(x)(y)

==(x::Integer, y::fq_nmod) = parent(y)(x) == y

==(x::fmpz, y::fq_nmod) = parent(y)(x) == y

###############################################################################
#
#   Inversion
#
###############################################################################

function inv(x::fq_nmod)
   iszero(x) && throw(DivideError())
   z = parent(x)()
   ccall((:fq_nmod_inv, libflint), Nothing,
       (Ref{fq_nmod}, Ref{fq_nmod}, Ref{FqNmodFiniteField}), z, x, x.parent)
   return z
end

###############################################################################
#
#   Exact division
#
###############################################################################

function divexact(x::fq_nmod, y::fq_nmod)
   check_parent(x, y)
   iszero(y) && throw(DivideError())
   z = parent(y)()
   ccall((:fq_nmod_div, libflint), Nothing,
         (Ref{fq_nmod}, Ref{fq_nmod}, Ref{fq_nmod}, Ref{FqNmodFiniteField}),
                                                       z, x, y, y.parent)
   return z
end

function divides(a::fq_nmod, b::fq_nmod)
   if iszero(a)
      return true, zero(parent(a))
   end
   if iszero(b)
      return false, zero(parent(a))
   end
   return true, divexact(a, b)
end

###############################################################################
#
#   Ad hoc exact division
#
###############################################################################

divexact(x::fq_nmod, y::Integer) = divexact(x, parent(x)(y))

divexact(x::fq_nmod, y::fmpz) = divexact(x, parent(x)(y))

divexact(x::Integer, y::fq_nmod) = divexact(parent(y)(x), y)

divexact(x::fmpz, y::fq_nmod) = divexact(parent(y)(x), y)

###############################################################################
#
#   Special functions
#
###############################################################################

function square_root(x::fq_nmod)
   z = parent(x)()
   res = Bool(ccall((:fq_nmod_sqrt, libflint), Cint,
                    (Ref{fq_nmod}, Ref{fq_nmod}, Ref{FqNmodFiniteField}),
                    z, x, x.parent))
   res || error("Not a square in sqrt")
   return z
end

function issquare(x::fq_nmod)
   return Bool(ccall((:fq_nmod_is_square, libflint), Cint,
                     (Ref{fq_nmod}, Ref{FqNmodFiniteField}),
                     x, x.parent))
end

function issquare_with_square_root(x::fq_nmod)
   z = parent(x)()
   flag = ccall((:fq_nmod_sqrt, libflint), Cint,
                (Ref{fq_nmod}, Ref{fq_nmod}, Ref{FqNmodFiniteField}),
                z, x, x.parent)
   return (Bool(flag), z)
end

function pth_root(x::fq_nmod)
   z = parent(x)()
   ccall((:fq_nmod_pth_root, libflint), Nothing,
       (Ref{fq_nmod}, Ref{fq_nmod}, Ref{FqNmodFiniteField}), z, x, x.parent)
   return z
end

function tr(x::fq_nmod)
   z = fmpz()
   ccall((:fq_nmod_trace, libflint), Nothing,
         (Ref{fmpz}, Ref{fq_nmod}, Ref{FqNmodFiniteField}), z, x, x.parent)
   return parent(x)(z)
end

function norm(x::fq_nmod)
   z = fmpz()
   ccall((:fq_nmod_norm, libflint), Nothing,
         (Ref{fmpz}, Ref{fq_nmod}, Ref{FqNmodFiniteField}), z, x, x.parent)
   return parent(x)(z)
end

function frobenius(x::fq_nmod, n = 1)
   z = parent(x)()
   ccall((:fq_nmod_frobenius, libflint), Nothing,
         (Ref{fq_nmod}, Ref{fq_nmod}, Int, Ref{FqNmodFiniteField}),
                                               z, x, n, x.parent)
   return z
end

###############################################################################
#
#   Unsafe functions
#
###############################################################################

function zero!(z::fq_nmod)
   ccall((:fq_nmod_zero, libflint), Nothing,
        (Ref{fq_nmod}, Ref{FqNmodFiniteField}), z, z.parent)
   return z
end

function mul!(z::fq_nmod, x::fq_nmod, y::fq_nmod)
   ccall((:fq_nmod_mul, libflint), Nothing,
         (Ref{fq_nmod}, Ref{fq_nmod}, Ref{fq_nmod}, Ref{FqNmodFiniteField}),
                                                       z, x, y, y.parent)
   return z
end

function addeq!(z::fq_nmod, x::fq_nmod)
   ccall((:fq_nmod_add, libflint), Nothing,
         (Ref{fq_nmod}, Ref{fq_nmod}, Ref{fq_nmod}, Ref{FqNmodFiniteField}),
                                                       z, z, x, x.parent)
   return z
end

function add!(z::fq_nmod, x::fq_nmod, y::fq_nmod)
   ccall((:fq_nmod_add, libflint), Nothing,
         (Ref{fq_nmod}, Ref{fq_nmod}, Ref{fq_nmod}, Ref{FqNmodFiniteField}),
                                                       z, x, y, x.parent)
   return z
end

###############################################################################
#
#   Random functions
#
###############################################################################

# define rand(::FqNmodFiniteField)

Random.Sampler(::Type{RNG}, R::FqNmodFiniteField, n::Random.Repetition) where {RNG<:AbstractRNG} =
   Random.SamplerSimple(R, Random.Sampler(RNG, BigInt(0):BigInt(order(R))-1, n))

function rand(rng::AbstractRNG, R::Random.SamplerSimple{FqNmodFiniteField})
   F = R[]
   x = gen(F)
   z = zero(F)
   p = characteristic(F)
   n = fmpz(rand(rng, R.data))
   xi = one(F)
   while !iszero(n)
      n, r = divrem(n, p)
      z += r*xi
      xi *= x
   end
   return z
end

Random.gentype(::Type{FqNmodFiniteField}) = elem_type(FqNmodFiniteField)

# define rand(make(::FqNmodFiniteField, n:m))

RandomExtensions.maketype(R::FqNmodFiniteField, _) = elem_type(R)

rand(rng::AbstractRNG, sp::SamplerTrivial{<:Make2{fq_nmod,FqNmodFiniteField,UnitRange{Int}}}) =
   sp[][1](rand(rng, sp[][2]))

# define rand(::FqNmodFiniteField, n:m)

rand(r::Random.AbstractRNG, R::FqNmodFiniteField, b::UnitRange{Int}) = rand(r, make(R, b))

rand(R::FqNmodFiniteField, b::UnitRange{Int}) = rand(Random.GLOBAL_RNG, R, b)

###############################################################################
#
#   Iteration
#
###############################################################################

# the two definitions are merged (with `Union`) so that this doesn't produce a compilation
# warning due to similar definitions in Hecke
Base.iterate(F::Union{FqNmodFiniteField,FqFiniteField}) =
   zero(F), zeros(F isa FqNmodFiniteField ? UInt : fmpz, degree(F))

function Base.iterate(F::Union{FqNmodFiniteField,FqFiniteField}, coeffs::Vector)
   deg = length(coeffs)
   char = F isa FqNmodFiniteField ? F.p : # cheaper than calling characteristic(F)::fmpz
                                    characteristic(F)
   allzero = true
   for d = 1:deg
      if allzero
         coeffs[d] += 1
         if coeffs[d] == char
            coeffs[d] = 0
         else
            allzero = false
         end
      else
         break
      end
   end
   allzero && return nothing

   elt = F()
   for d = 1:deg
      if F isa FqNmodFiniteField
         ccall((:nmod_poly_set_coeff_ui, libflint), Nothing,
               (Ref{fq_nmod}, Int, UInt), elt, d - 1, coeffs[d])
      else
         ccall((:fmpz_poly_set_coeff_fmpz, libflint), Nothing,
               (Ref{fq}, Int, Ref{fmpz}), elt, d - 1, coeffs[d])
      end
   end
   elt, coeffs
end

# Base.length(F) and Base.eltype(F) are defined in AbstractAlgebra

###############################################################################
#
#   Promotions
#
###############################################################################

promote_rule(::Type{fq_nmod}, ::Type{T}) where {T <: Integer} = fq_nmod

promote_rule(::Type{fq_nmod}, ::Type{fmpz}) = fq_nmod

###############################################################################
#
#   Parent object call overload
#
###############################################################################

function (a::FqNmodFiniteField)()
   z = fq_nmod(a)
   z.parent = a
   return z
end

(a::FqNmodFiniteField)(b::Integer) = a(fmpz(b))

function (a::FqNmodFiniteField)(b::Int)
   z = fq_nmod(a, b)
   z.parent = a
   return z
end

function (a::FqNmodFiniteField)(b::fmpz)
   z = fq_nmod(a, b)
   z.parent = a
   return z
end

function (a::FqNmodFiniteField)(b::fq_nmod)
    k = parent(b)
    da = degree(a)
    dk = degree(k)
    if k == a
        return b
    elseif dk < da
        da % dk != 0 && error("Coercion impossible")
        f = embed(k, a)
        return f(b)
    else
        dk % da != 0 && error("Coercion impossible")
        f = preimage_map(a, k)
        return f(b)
    end
end

###############################################################################
#
#   FlintFiniteField constructor
#
###############################################################################

function FlintFiniteField(char::Int, deg::Int, s::AbstractString; cached = true)
   S = Symbol(s)
   parent_obj = FqNmodFiniteField(fmpz(char), deg, S, cached)

   return parent_obj, gen(parent_obj)
end

function FlintFiniteField(pol::Zmodn_poly, s::AbstractString; cached = true, check::Bool=true)
   S = Symbol(s)
   parent_obj = FqNmodFiniteField(pol, S, cached, check=check)

   return parent_obj, gen(parent_obj)
end

################################################################################
#
#   FqNmodFiniteField Modulus
#
################################################################################

@doc Markdown.doc"""
    modulus(k::FqNmodFiniteField)

>Returns the modulus defining the finite field $k$.
"""
function modulus(k::FqNmodFiniteField)
    p::Int = characteristic(k)
    R = PolynomialRing(ResidueRing(ZZ, p), "T")[1]
    Q = R()
    P = ccall((:fq_nmod_ctx_modulus, libflint), Ref{nmod_poly},
                 (Ref{FqNmodFiniteField},), k)
    ccall((:nmod_poly_set, libflint), Nothing,
          (Ref{nmod_poly}, Ref{nmod_poly}),
          Q, P)

    return Q
end
