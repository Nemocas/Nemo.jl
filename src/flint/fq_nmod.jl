###############################################################################
#
#   fqPolyRepFieldElem.jl : Flint finite fields
#
###############################################################################

export fqPolyRepFieldElem, fqPolyRepField, coeffs_raw

###############################################################################
#
#   Type and parent object methods
#
###############################################################################

parent_type(::Type{fqPolyRepFieldElem}) = fqPolyRepField

elem_type(::Type{fqPolyRepField}) = fqPolyRepFieldElem

base_ring(a::fqPolyRepField) = Union{}

base_ring(a::fqPolyRepFieldElem) = Union{}

parent(a::fqPolyRepFieldElem) = a.parent

is_domain_type(::Type{fqPolyRepFieldElem}) = true

function check_parent(a::fqPolyRepFieldElem, b::fqPolyRepFieldElem)
   a.parent != b.parent && error("Operations on distinct finite fields not supported")
end

###############################################################################
#
#   Basic manipulation
#
###############################################################################

function Base.hash(a::fqPolyRepFieldElem, h::UInt)
   b = 0x78e5f766c8ace18d%UInt
   for i in 0:degree(parent(a)) - 1
      b = xor(b, xor(hash(coeff(a, i), h), h))
      b = (b << 1) | (b >> (sizeof(Int)*8 - 1))
   end
   return b
end

function coeff(x::fqPolyRepFieldElem, n::Int)
   n < 0 && throw(DomainError(n, "Index must be non-negative"))
   return ccall((:nmod_poly_get_coeff_ui, libflint), UInt,
                (Ref{fqPolyRepFieldElem}, Int), x, n)
end

function coeffs_raw(x::fqPolyRepFieldElem)
   @GC.preserve x begin
      len = degree(parent(x))
      V = unsafe_wrap(Vector{UInt}, reinterpret(Ptr{UInt}, x.coeffs), x.length)
      Vcopy = Vector{UInt}(undef, len)
      for i = 1:x.length
         Vcopy[i] = V[i]
      end
      for i = x.length + 1:len
         Vcopy[i] = 0
      end
   end
   return Vcopy
end

# set the i-th coeff of x to c, internal use only
function setindex_raw!(x::fqPolyRepFieldElem, c::UInt, i::Int)
   len = degree(parent(x))
   i > len - 1 && error("Index out of range")
   ccall((:nmod_poly_set_coeff_ui, libflint), Nothing,
         (Ref{fqPolyRepFieldElem}, Int, UInt), x, i, c)
   return x
end

function zero(a::fqPolyRepField)
   d = a()
   ccall((:fq_nmod_zero, libflint), Nothing,
         (Ref{fqPolyRepFieldElem}, Ref{fqPolyRepField}), d, a)
   return d
end

function one(a::fqPolyRepField)
   d = a()
   ccall((:fq_nmod_one, libflint), Nothing,
         (Ref{fqPolyRepFieldElem}, Ref{fqPolyRepField}), d, a)
   return d
end

function gen(a::fqPolyRepField)
   d = a()
   ccall((:fq_nmod_gen, libflint), Nothing,
         (Ref{fqPolyRepFieldElem}, Ref{fqPolyRepField}), d, a)
   return d
end

iszero(a::fqPolyRepFieldElem) = ccall((:fq_nmod_is_zero, libflint), Bool,
                     (Ref{fqPolyRepFieldElem}, Ref{fqPolyRepField}), a, a.parent)

isone(a::fqPolyRepFieldElem) = ccall((:fq_nmod_is_one, libflint), Bool,
                    (Ref{fqPolyRepFieldElem}, Ref{fqPolyRepField}), a, a.parent)

is_gen(a::fqPolyRepFieldElem) = a == gen(parent(a)) # there is no is_gen in flint

is_unit(a::fqPolyRepFieldElem) = ccall((:fq_nmod_is_invertible, libflint), Bool,
                     (Ref{fqPolyRepFieldElem}, Ref{fqPolyRepField}), a, a.parent)

function characteristic(a::fqPolyRepField)
   d = ZZRingElem()
   ccall((:__fq_nmod_ctx_prime, libflint), Nothing,
         (Ref{ZZRingElem}, Ref{fqPolyRepField}), d, a)
   return d
end

function order(a::fqPolyRepField)
   d = ZZRingElem()
   ccall((:fq_nmod_ctx_order, libflint), Nothing,
         (Ref{ZZRingElem}, Ref{fqPolyRepField}), d, a)
   return d
end

function degree(a::fqPolyRepField)
   return ccall((:fq_nmod_ctx_degree, libflint), Int,
                (Ref{fqPolyRepField},), a)
end

function deepcopy_internal(d::fqPolyRepFieldElem, dict::IdDict)
   z = fqPolyRepFieldElem(parent(d), d)
   z.parent = parent(d)
   return z
end

###############################################################################
#
#   Canonicalisation
#
###############################################################################

canonical_unit(x::fqPolyRepFieldElem) = x

###############################################################################
#
#   AbstractString I/O
#
###############################################################################

function expressify(a::fqPolyRepFieldElem; context = nothing)
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

show(io::IO, a::fqPolyRepFieldElem) = print(io, AbstractAlgebra.obj_to_string(a, context = io))

function show(io::IO, a::fqPolyRepField)
   print(io, "Finite field of degree ", degree(a))
   print(io, " over F_", characteristic(a))
end

###############################################################################
#
#   Unary operations
#
###############################################################################

function -(x::fqPolyRepFieldElem)
   z = parent(x)()
   ccall((:fq_nmod_neg, libflint), Nothing,
       (Ref{fqPolyRepFieldElem}, Ref{fqPolyRepFieldElem}, Ref{fqPolyRepField}), z, x, x.parent)
   return z
end

###############################################################################
#
#   Binary operations
#
###############################################################################

function +(x::fqPolyRepFieldElem, y::fqPolyRepFieldElem)
   check_parent(x, y)
   z = parent(y)()
   ccall((:fq_nmod_add, libflint), Nothing,
         (Ref{fqPolyRepFieldElem}, Ref{fqPolyRepFieldElem}, Ref{fqPolyRepFieldElem}, Ref{fqPolyRepField}),
                                                       z, x, y, y.parent)
   return z
end

function -(x::fqPolyRepFieldElem, y::fqPolyRepFieldElem)
   check_parent(x, y)
   z = parent(y)()
   ccall((:fq_nmod_sub, libflint), Nothing,
         (Ref{fqPolyRepFieldElem}, Ref{fqPolyRepFieldElem}, Ref{fqPolyRepFieldElem}, Ref{fqPolyRepField}),
                                                       z, x, y, y.parent)
   return z
end

function *(x::fqPolyRepFieldElem, y::fqPolyRepFieldElem)
   check_parent(x, y)
   z = parent(y)()
   ccall((:fq_nmod_mul, libflint), Nothing,
         (Ref{fqPolyRepFieldElem}, Ref{fqPolyRepFieldElem}, Ref{fqPolyRepFieldElem}, Ref{fqPolyRepField}),
                                                       z, x, y, y.parent)
   return z
end

###############################################################################
#
#   Ad hoc binary operators
#
###############################################################################

function *(x::Int, y::fqPolyRepFieldElem)
   z = parent(y)()
   ccall((:fq_nmod_mul_si, libflint), Nothing,
         (Ref{fqPolyRepFieldElem}, Ref{fqPolyRepFieldElem}, Int, Ref{fqPolyRepField}),
                                               z, y, x, y.parent)
   return z
end

*(x::fqPolyRepFieldElem, y::Int) = y*x

*(x::Integer, y::fqPolyRepFieldElem) = ZZRingElem(x)*y

*(x::fqPolyRepFieldElem, y::Integer) = y*x

function *(x::ZZRingElem, y::fqPolyRepFieldElem)
   z = parent(y)()
   ccall((:fq_nmod_mul_fmpz, libflint), Nothing,
         (Ref{fqPolyRepFieldElem}, Ref{fqPolyRepFieldElem}, Ref{ZZRingElem}, Ref{fqPolyRepField}),
                                                    z, y, x, y.parent)
   return z
end

*(x::fqPolyRepFieldElem, y::ZZRingElem) = y*x

+(x::fqPolyRepFieldElem, y::Integer) = x + parent(x)(y)

+(x::Integer, y::fqPolyRepFieldElem) = y + x

+(x::fqPolyRepFieldElem, y::ZZRingElem) = x + parent(x)(y)

+(x::ZZRingElem, y::fqPolyRepFieldElem) = y + x

-(x::fqPolyRepFieldElem, y::Integer) = x - parent(x)(y)

-(x::Integer, y::fqPolyRepFieldElem) = parent(y)(x) - y

-(x::fqPolyRepFieldElem, y::ZZRingElem) = x - parent(x)(y)

-(x::ZZRingElem, y::fqPolyRepFieldElem) = parent(y)(x) - y

###############################################################################
#
#   Powering
#
###############################################################################

function ^(x::fqPolyRepFieldElem, y::Int)
   if y < 0
      x = inv(x)
      y = -y
   end
   z = parent(x)()
   ccall((:fq_nmod_pow_ui, libflint), Nothing,
         (Ref{fqPolyRepFieldElem}, Ref{fqPolyRepFieldElem}, Int, Ref{fqPolyRepField}),
                                               z, x, y, x.parent)
   return z
end

function ^(x::fqPolyRepFieldElem, y::ZZRingElem)
   if y < 0
      x = inv(x)
      y = -y
   end
   z = parent(x)()
   ccall((:fq_nmod_pow, libflint), Nothing,
         (Ref{fqPolyRepFieldElem}, Ref{fqPolyRepFieldElem}, Ref{ZZRingElem}, Ref{fqPolyRepField}),
                                                    z, x, y, x.parent)
   return z
end

###############################################################################
#
#   Comparison
#
###############################################################################

function ==(x::fqPolyRepFieldElem, y::fqPolyRepFieldElem)
   check_parent(x, y)
   ccall((:fq_nmod_equal, libflint), Bool,
       (Ref{fqPolyRepFieldElem}, Ref{fqPolyRepFieldElem}, Ref{fqPolyRepField}), x, y, y.parent)
end

###############################################################################
#
#   Ad hoc comparison
#
###############################################################################

==(x::fqPolyRepFieldElem, y::Integer) = x == parent(x)(y)

==(x::fqPolyRepFieldElem, y::ZZRingElem) = x == parent(x)(y)

==(x::Integer, y::fqPolyRepFieldElem) = parent(y)(x) == y

==(x::ZZRingElem, y::fqPolyRepFieldElem) = parent(y)(x) == y

###############################################################################
#
#   Inversion
#
###############################################################################

function inv(x::fqPolyRepFieldElem)
   iszero(x) && throw(DivideError())
   z = parent(x)()
   ccall((:fq_nmod_inv, libflint), Nothing,
       (Ref{fqPolyRepFieldElem}, Ref{fqPolyRepFieldElem}, Ref{fqPolyRepField}), z, x, x.parent)
   return z
end

###############################################################################
#
#   Exact division
#
###############################################################################

function divexact(x::fqPolyRepFieldElem, y::fqPolyRepFieldElem; check::Bool=true)
   check_parent(x, y)
   iszero(y) && throw(DivideError())
   z = parent(y)()
   ccall((:fq_nmod_div, libflint), Nothing,
         (Ref{fqPolyRepFieldElem}, Ref{fqPolyRepFieldElem}, Ref{fqPolyRepFieldElem}, Ref{fqPolyRepField}),
                                                       z, x, y, y.parent)
   return z
end

function divides(a::fqPolyRepFieldElem, b::fqPolyRepFieldElem)
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

divexact(x::fqPolyRepFieldElem, y::Integer; check::Bool=true) = divexact(x, parent(x)(y); check=check)

divexact(x::fqPolyRepFieldElem, y::ZZRingElem; check::Bool=true) = divexact(x, parent(x)(y); check=check)

divexact(x::Integer, y::fqPolyRepFieldElem; check::Bool=true) = divexact(parent(y)(x), y; check=check)

divexact(x::ZZRingElem, y::fqPolyRepFieldElem; check::Bool=true) = divexact(parent(y)(x), y; check=check)

###############################################################################
#
#   Special functions
#
###############################################################################

function sqrt(x::fqPolyRepFieldElem; check::Bool=true)
   z = parent(x)()
   res = Bool(ccall((:fq_nmod_sqrt, libflint), Cint,
                    (Ref{fqPolyRepFieldElem}, Ref{fqPolyRepFieldElem}, Ref{fqPolyRepField}),
                    z, x, x.parent))
   check && !res && error("Not a square")
   return z
end

function is_square(x::fqPolyRepFieldElem)
   return Bool(ccall((:fq_nmod_is_square, libflint), Cint,
                     (Ref{fqPolyRepFieldElem}, Ref{fqPolyRepField}),
                     x, x.parent))
end

function is_square_with_sqrt(x::fqPolyRepFieldElem)
   z = parent(x)()
   flag = ccall((:fq_nmod_sqrt, libflint), Cint,
                (Ref{fqPolyRepFieldElem}, Ref{fqPolyRepFieldElem}, Ref{fqPolyRepField}),
                z, x, x.parent)
   return (Bool(flag), z)
end

function pth_root(x::fqPolyRepFieldElem)
   z = parent(x)()
   ccall((:fq_nmod_pth_root, libflint), Nothing,
       (Ref{fqPolyRepFieldElem}, Ref{fqPolyRepFieldElem}, Ref{fqPolyRepField}), z, x, x.parent)
   return z
end

function tr(x::fqPolyRepFieldElem)
   z = ZZRingElem()
   ccall((:fq_nmod_trace, libflint), Nothing,
         (Ref{ZZRingElem}, Ref{fqPolyRepFieldElem}, Ref{fqPolyRepField}), z, x, x.parent)
   return parent(x)(z)
end

function norm(x::fqPolyRepFieldElem)
   z = ZZRingElem()
   ccall((:fq_nmod_norm, libflint), Nothing,
         (Ref{ZZRingElem}, Ref{fqPolyRepFieldElem}, Ref{fqPolyRepField}), z, x, x.parent)
   return parent(x)(z)
end

function frobenius(x::fqPolyRepFieldElem, n = 1)
   z = parent(x)()
   ccall((:fq_nmod_frobenius, libflint), Nothing,
         (Ref{fqPolyRepFieldElem}, Ref{fqPolyRepFieldElem}, Int, Ref{fqPolyRepField}),
                                               z, x, n, x.parent)
   return z
end

###############################################################################
#
#   Lift
#
###############################################################################

function lift(R::fpPolyRing, x::fqPolyRepFieldElem)
   c = R()
   ccall((:fq_nmod_get_nmod_poly, libflint), Nothing,
         (Ref{fpPolyRingElem}, Ref{fqPolyRepFieldElem}, Ref{fqPolyRepField}),
                                                      c, x, parent(x))
   return c
end

###############################################################################
#
#   Unsafe functions
#
###############################################################################

function zero!(z::fqPolyRepFieldElem)
   ccall((:fq_nmod_zero, libflint), Nothing,
        (Ref{fqPolyRepFieldElem}, Ref{fqPolyRepField}), z, z.parent)
   return z
end

function mul!(z::fqPolyRepFieldElem, x::fqPolyRepFieldElem, y::fqPolyRepFieldElem)
   ccall((:fq_nmod_mul, libflint), Nothing,
         (Ref{fqPolyRepFieldElem}, Ref{fqPolyRepFieldElem}, Ref{fqPolyRepFieldElem}, Ref{fqPolyRepField}),
                                                       z, x, y, y.parent)
   return z
end

function addeq!(z::fqPolyRepFieldElem, x::fqPolyRepFieldElem)
   ccall((:fq_nmod_add, libflint), Nothing,
         (Ref{fqPolyRepFieldElem}, Ref{fqPolyRepFieldElem}, Ref{fqPolyRepFieldElem}, Ref{fqPolyRepField}),
                                                       z, z, x, x.parent)
   return z
end

function add!(z::fqPolyRepFieldElem, x::fqPolyRepFieldElem, y::fqPolyRepFieldElem)
   ccall((:fq_nmod_add, libflint), Nothing,
         (Ref{fqPolyRepFieldElem}, Ref{fqPolyRepFieldElem}, Ref{fqPolyRepFieldElem}, Ref{fqPolyRepField}),
                                                       z, x, y, x.parent)
   return z
end

###############################################################################
#
#   Random functions
#
###############################################################################

# define rand(::fqPolyRepField)

Random.Sampler(::Type{RNG}, R::fqPolyRepField, n::Random.Repetition) where {RNG<:AbstractRNG} =
   Random.SamplerSimple(R, Random.Sampler(RNG, BigInt(0):BigInt(order(R))-1, n))

function rand(rng::AbstractRNG, R::Random.SamplerSimple{fqPolyRepField})
   F = R[]
   x = gen(F)
   z = zero(F)
   p = characteristic(F)
   n = ZZRingElem(rand(rng, R.data))
   xi = one(F)
   while !iszero(n)
      n, r = divrem(n, p)
      z += r*xi
      xi *= x
   end
   return z
end

Random.gentype(::Type{fqPolyRepField}) = elem_type(fqPolyRepField)

# define rand(make(::fqPolyRepField, arr)), where arr is any abstract array with integer or ZZRingElem entries

RandomExtensions.maketype(R::fqPolyRepField, _) = elem_type(R)

rand(rng::AbstractRNG, sp::SamplerTrivial{<:Make2{fqPolyRepFieldElem,fqPolyRepField,<:AbstractArray{<:IntegerUnion}}}) =
   sp[][1](rand(rng, sp[][2]))

# define rand(::fqPolyRepField, arr), where arr is any abstract array with integer or ZZRingElem entries

rand(r::Random.AbstractRNG, R::fqPolyRepField, b::AbstractArray) = rand(r, make(R, b))

rand(R::fqPolyRepField, b::AbstractArray) = rand(Random.GLOBAL_RNG, R, b)

###############################################################################
#
#   Iteration
#
###############################################################################

# the two definitions are merged (with `Union`) so that this doesn't produce a compilation
# warning due to similar definitions in Hecke
Base.iterate(F::Union{fqPolyRepField,FqPolyRepField}) =
   zero(F), zeros(F isa fqPolyRepField ? UInt : ZZRingElem, degree(F))

function Base.iterate(F::Union{fqPolyRepField,FqPolyRepField}, coeffs::Vector)
   deg = length(coeffs)
   char = F isa fqPolyRepField ? F.p : # cheaper than calling characteristic(F)::ZZRingElem
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
      if F isa fqPolyRepField
         ccall((:nmod_poly_set_coeff_ui, libflint), Nothing,
               (Ref{fqPolyRepFieldElem}, Int, UInt), elt, d - 1, coeffs[d])
      else
         ccall((:fmpz_poly_set_coeff_fmpz, libflint), Nothing,
               (Ref{FqPolyRepFieldElem}, Int, Ref{ZZRingElem}), elt, d - 1, coeffs[d])
      end
   end
   elt, coeffs
end

# Base.length(F) and Base.eltype(F) are defined in AbstractAlgebra

################################################################################
#
#   fqPolyRepField Modulus
#
################################################################################

@doc raw"""
    modulus(k::fqPolyRepField, var::String="T")

Return the modulus defining the finite field $k$.
"""
function modulus(k::fqPolyRepField, var::String="T")
    p::Int = characteristic(k)
    Q = polynomial(GF(p), [], var)
    GC.@preserve k begin
        P = ccall((:fq_nmod_ctx_modulus, libflint), Ptr{fpPolyRingElem},
                     (Ref{fqPolyRepField},), k)
        ccall((:nmod_poly_set, libflint), Nothing,
              (Ref{fpPolyRingElem}, Ptr{fpPolyRingElem}),
              Q, P)
    end
    return Q
end

#function defining_polynomial(k::fqPolyRepField)
#   F = fpField(UInt(characteristic(k)))
#   Fx, = polynomial_ring(F, "x", cached = false)
#   return defining_polynomial(Fx, k)
#end
#
#function defining_polynomial(R::fpPolyRing, k::fqPolyRepField)
#   Q = R()
#   GC.@preserve k begin
#      P = ccall((:fq_nmod_ctx_modulus, libflint), Ptr{fpPolyRingElem},
#                (Ref{fqPolyRepField},), k)
#      ccall((:nmod_poly_set, libflint), Nothing,
#            (Ref{fpPolyRingElem}, Ptr{fpPolyRingElem}),
#            Q, P)
#   end
#   return Q
#end

###############################################################################
#
#   Promotions
#
###############################################################################

promote_rule(::Type{fqPolyRepFieldElem}, ::Type{T}) where {T <: Integer} = fqPolyRepFieldElem

promote_rule(::Type{fqPolyRepFieldElem}, ::Type{ZZRingElem}) = fqPolyRepFieldElem

###############################################################################
#
#   Parent object call overload
#
###############################################################################

function (a::fqPolyRepField)()
   z = fqPolyRepFieldElem(a)
   z.parent = a
   return z
end

(a::fqPolyRepField)(b::Integer) = a(ZZRingElem(b))

function (a::fqPolyRepField)(b::Int)
   z = fqPolyRepFieldElem(a, b)
   z.parent = a
   return z
end

function (a::fqPolyRepField)(b::ZZRingElem)
   z = fqPolyRepFieldElem(a, b)
   z.parent = a
   return z
end

function (a::fqPolyRepField)(b::fqPolyRepFieldElem)
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

function fqPolyRepFieldElem(a::fqPolyRepField, b::Vector{UInt})
   r = a()
   len = degree(a)
   length(b) != len && error("Vector does not have correct length")
   norm = 0
   V = unsafe_wrap(Vector{UInt}, reinterpret(Ptr{UInt}, r.coeffs), len)
   for i = 1:len
      V[i] = b[i]
      if b[i] != 0
         norm = i
      end
   end
   r.length = norm
   return r
end

###############################################################################
#
#   FlintFiniteField constructor
#
###############################################################################

function FlintFiniteField(char::Int, deg::Int, s::Union{AbstractString,Symbol} = :o; cached = true)
   S = Symbol(s)
   parent_obj = fqPolyRepField(ZZRingElem(char), deg, S, cached)

   return parent_obj, gen(parent_obj)
end

function FlintFiniteField(pol::Zmodn_poly, s::Union{AbstractString,Symbol} = :o; cached = true, check::Bool=true)
   S = Symbol(s)
   parent_obj = fqPolyRepField(pol, S, cached, check=check)

   return parent_obj, gen(parent_obj)
end

function FlintFiniteField(F::fqPolyRepField, deg::Int,
                          s::Union{AbstractString,Symbol} = :o; cached = true)
    return fqPolyRepField(characteristic(F), deg, Symbol(s), cached)
end
