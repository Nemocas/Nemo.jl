###############################################################################
#
#   FqPolyRepFieldElem.jl : Flint finite fields
#
###############################################################################

export FlintFiniteField, characteristic, order, FqPolyRepFieldElem, FqPolyRepField, frobenius,
       pth_root, tr, norm

###############################################################################
#
#   Type and parent object methods
#
###############################################################################

parent_type(::Type{FqPolyRepFieldElem}) = FqPolyRepField

elem_type(::Type{FqPolyRepField}) = FqPolyRepFieldElem

base_ring(a::FqPolyRepField) = Union{}

base_ring(a::FqPolyRepFieldElem) = Union{}

parent(a::FqPolyRepFieldElem) = a.parent

is_domain_type(::Type{FqPolyRepFieldElem}) = true

function check_parent(a::FqPolyRepFieldElem, b::FqPolyRepFieldElem)
   a.parent != b.parent && error("Operations on distinct finite fields not supported")
end

###############################################################################
#
#   Basic manipulation
#
###############################################################################

function Base.hash(a::FqPolyRepFieldElem, h::UInt)
   b = 0xb310fb6ea97e1f1a%UInt
   z = ZZRingElem()
   for i in 0:degree(parent(a)) - 1
      ccall((:fmpz_poly_get_coeff_fmpz, libflint), Nothing,
               (Ref{ZZRingElem}, Ref{FqPolyRepFieldElem}, Int), z, a, i)
      b = xor(b, xor(hash(z, h), h))
      b = (b << 1) | (b >> (sizeof(Int)*8 - 1))
   end
   return b
end

@doc raw"""
    coeff(x::FqPolyRepFieldElem, n::Int)

Return the degree $n$ coefficient of the polynomial representing the given
finite field element.
"""
function coeff(x::FqPolyRepFieldElem, n::Int)
   n < 0 && throw(DomainError(n, "Index must be non-negative"))
   z = ZZRingElem()
   ccall((:fmpz_poly_get_coeff_fmpz, libflint), Nothing,
               (Ref{ZZRingElem}, Ref{FqPolyRepFieldElem}, Int), z, x, n)
   return z
end

function zero(a::FqPolyRepField)
   d = a()
   ccall((:fq_zero, libflint), Nothing, (Ref{FqPolyRepFieldElem}, Ref{FqPolyRepField}), d, a)
   return d
end

function one(a::FqPolyRepField)
   d = a()
   ccall((:fq_one, libflint), Nothing, (Ref{FqPolyRepFieldElem}, Ref{FqPolyRepField}), d, a)
   return d
end

@doc raw"""
    gen(a::FqPolyRepField)

Return the generator of the finite field. Note that this is only guaranteed
to be a multiplicative generator if the finite field is generated by a
Conway polynomial automatically.
"""
function gen(a::FqPolyRepField)
   d = a()
   ccall((:fq_gen, libflint), Nothing, (Ref{FqPolyRepFieldElem}, Ref{FqPolyRepField}), d, a)
   return d
end

iszero(a::FqPolyRepFieldElem) = ccall((:fq_is_zero, libflint), Bool,
                     (Ref{FqPolyRepFieldElem}, Ref{FqPolyRepField}), a, a.parent)

isone(a::FqPolyRepFieldElem) = ccall((:fq_is_one, libflint), Bool,
                    (Ref{FqPolyRepFieldElem}, Ref{FqPolyRepField}), a, a.parent)

@doc raw"""
    is_gen(a::FqPolyRepFieldElem)

Return `true` if the given finite field element is the generator of the
finite field, otherwise return `false`.
"""
is_gen(a::FqPolyRepFieldElem) = a == gen(parent(a))

is_unit(a::FqPolyRepFieldElem) = ccall((:fq_is_invertible, libflint), Bool,
                     (Ref{FqPolyRepFieldElem}, Ref{FqPolyRepField}), a, a.parent)

function characteristic(a::FqPolyRepField)
   d = ZZRingElem()
   ccall((:__fq_ctx_prime, libflint), Nothing,
         (Ref{ZZRingElem}, Ref{FqPolyRepField}), d, a)
   return d
end

function order(a::FqPolyRepField)
   d = ZZRingElem()
   ccall((:fq_ctx_order, libflint), Nothing,
         (Ref{ZZRingElem}, Ref{FqPolyRepField}), d, a)
   return d
end

@doc raw"""
    degree(a::FqPolyRepField)

Return the degree of the given finite field.
"""
function degree(a::FqPolyRepField)
   return ccall((:fq_ctx_degree, libflint), Int, (Ref{FqPolyRepField},), a)
end

function deepcopy_internal(d::FqPolyRepFieldElem, dict::IdDict)
   z = FqPolyRepFieldElem(parent(d), d)
   return z
end

###############################################################################
#
#   Canonicalisation
#
###############################################################################

canonical_unit(x::FqPolyRepFieldElem) = x

###############################################################################
#
#   AbstractString I/O
#
###############################################################################

function expressify(a::FqPolyRepFieldElem; context = nothing)
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

show(io::IO, a::FqPolyRepFieldElem) = print(io, AbstractAlgebra.obj_to_string(a, context = io))

function show(io::IO, a::FqPolyRepField)
   if get(io, :supercompact, false)
      # no nested printing
      io = pretty(io)
      print(io, LowercaseOff(), "GF($(characteristic(a))^$(degree(a)))")
   else
      # nested printing allowed, preferably supercompact
      print(io, "Finite field of degree ", degree(a))
      print(io, " over GF(", characteristic(a),")")
   end
end

###############################################################################
#
#   Unary operations
#
###############################################################################

function -(x::FqPolyRepFieldElem)
   z = parent(x)()
   ccall((:fq_neg, libflint), Nothing,
         (Ref{FqPolyRepFieldElem}, Ref{FqPolyRepFieldElem}, Ref{FqPolyRepField}), z, x, x.parent)
   return z
end

###############################################################################
#
#   Binary operations
#
###############################################################################

function +(x::FqPolyRepFieldElem, y::FqPolyRepFieldElem)
   check_parent(x, y)
   z = parent(y)()
   ccall((:fq_add, libflint), Nothing,
        (Ref{FqPolyRepFieldElem}, Ref{FqPolyRepFieldElem}, Ref{FqPolyRepFieldElem}, Ref{FqPolyRepField}), z, x, y, y.parent)
   return z
end

function -(x::FqPolyRepFieldElem, y::FqPolyRepFieldElem)
   check_parent(x, y)
   z = parent(y)()
   ccall((:fq_sub, libflint), Nothing,
        (Ref{FqPolyRepFieldElem}, Ref{FqPolyRepFieldElem}, Ref{FqPolyRepFieldElem}, Ref{FqPolyRepField}), z, x, y, y.parent)
   return z
end

function *(x::FqPolyRepFieldElem, y::FqPolyRepFieldElem)
   check_parent(x, y)
   z = parent(y)()
   ccall((:fq_mul, libflint), Nothing,
        (Ref{FqPolyRepFieldElem}, Ref{FqPolyRepFieldElem}, Ref{FqPolyRepFieldElem}, Ref{FqPolyRepField}), z, x, y, y.parent)
   return z
end

###############################################################################
#
#   Ad hoc binary operators
#
###############################################################################

function *(x::Int, y::FqPolyRepFieldElem)
   z = parent(y)()
   ccall((:fq_mul_si, libflint), Nothing,
         (Ref{FqPolyRepFieldElem}, Ref{FqPolyRepFieldElem}, Int, Ref{FqPolyRepField}), z, y, x, y.parent)
   return z
end

*(x::Integer, y::FqPolyRepFieldElem) = ZZRingElem(x)*y

*(x::FqPolyRepFieldElem, y::Integer) = y*x

function *(x::ZZRingElem, y::FqPolyRepFieldElem)
   z = parent(y)()
   ccall((:fq_mul_fmpz, libflint), Nothing,
         (Ref{FqPolyRepFieldElem}, Ref{FqPolyRepFieldElem}, Ref{ZZRingElem}, Ref{FqPolyRepField}),
                                            z, y, x, y.parent)
   return z
end

*(x::FqPolyRepFieldElem, y::ZZRingElem) = y*x

+(x::FqPolyRepFieldElem, y::Integer) = x + parent(x)(y)

+(x::Integer, y::FqPolyRepFieldElem) = y + x

+(x::FqPolyRepFieldElem, y::ZZRingElem) = x + parent(x)(y)

+(x::ZZRingElem, y::FqPolyRepFieldElem) = y + x

-(x::FqPolyRepFieldElem, y::Integer) = x - parent(x)(y)

-(x::Integer, y::FqPolyRepFieldElem) = parent(y)(x) - y

-(x::FqPolyRepFieldElem, y::ZZRingElem) = x - parent(x)(y)

-(x::ZZRingElem, y::FqPolyRepFieldElem) = parent(y)(x) - y

###############################################################################
#
#   Powering
#
###############################################################################

function ^(x::FqPolyRepFieldElem, y::Int)
   if y < 0
      x = inv(x)
      y = -y
   end
   z = parent(x)()
   ccall((:fq_pow_ui, libflint), Nothing,
         (Ref{FqPolyRepFieldElem}, Ref{FqPolyRepFieldElem}, Int, Ref{FqPolyRepField}), z, x, y, x.parent)
   return z
end

function ^(x::FqPolyRepFieldElem, y::ZZRingElem)
   if y < 0
      x = inv(x)
      y = -y
   end
   z = parent(x)()
   ccall((:fq_pow, libflint), Nothing,
         (Ref{FqPolyRepFieldElem}, Ref{FqPolyRepFieldElem}, Ref{ZZRingElem}, Ref{FqPolyRepField}),
                                            z, x, y, x.parent)
   return z
end

###############################################################################
#
#   Comparison
#
###############################################################################

function ==(x::FqPolyRepFieldElem, y::FqPolyRepFieldElem)
   check_parent(x, y)
   ccall((:fq_equal, libflint), Bool,
         (Ref{FqPolyRepFieldElem}, Ref{FqPolyRepFieldElem}, Ref{FqPolyRepField}), x, y, y.parent)
end

###############################################################################
#
#   Ad hoc comparison
#
###############################################################################

==(x::FqPolyRepFieldElem, y::Integer) = x == parent(x)(y)

==(x::FqPolyRepFieldElem, y::ZZRingElem) = x == parent(x)(y)

==(x::Integer, y::FqPolyRepFieldElem) = parent(y)(x) == y

==(x::ZZRingElem, y::FqPolyRepFieldElem) = parent(y)(x) == y

###############################################################################
#
#   Exact division
#
###############################################################################

function divexact(x::FqPolyRepFieldElem, y::FqPolyRepFieldElem; check::Bool=true)
   check_parent(x, y)
   iszero(y) && throw(DivideError())
   z = parent(y)()
   ccall((:fq_div, libflint), Nothing,
        (Ref{FqPolyRepFieldElem}, Ref{FqPolyRepFieldElem}, Ref{FqPolyRepFieldElem}, Ref{FqPolyRepField}), z, x, y, y.parent)
   return z
end

function divides(a::FqPolyRepFieldElem, b::FqPolyRepFieldElem)
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

divexact(x::FqPolyRepFieldElem, y::Integer; check::Bool=true) = divexact(x, parent(x)(y); check=check)

divexact(x::FqPolyRepFieldElem, y::ZZRingElem; check::Bool=true) = divexact(x, parent(x)(y); check=check)

divexact(x::Integer, y::FqPolyRepFieldElem; check::Bool=true) = divexact(parent(y)(x), y; check=check)

divexact(x::ZZRingElem, y::FqPolyRepFieldElem; check::Bool=true) = divexact(parent(y)(x), y; check=check)

###############################################################################
#
#   Inversion
#
###############################################################################

function inv(x::FqPolyRepFieldElem)
   iszero(x) && throw(DivideError())
   z = parent(x)()
   ccall((:fq_inv, libflint), Nothing,
         (Ref{FqPolyRepFieldElem}, Ref{FqPolyRepFieldElem}, Ref{FqPolyRepField}), z, x, x.parent)
   return z
end

###############################################################################
#
#   Special functions
#
###############################################################################

function sqrt(x::FqPolyRepFieldElem; check::Bool=true)
   z = parent(x)()
   res = Bool(ccall((:fq_sqrt, libflint), Cint,
                    (Ref{FqPolyRepFieldElem}, Ref{FqPolyRepFieldElem}, Ref{FqPolyRepField}),
                    z, x, x.parent))
   check && !res && error("Not a square")
   return z
end

function is_square(x::FqPolyRepFieldElem)
   return Bool(ccall((:fq_is_square, libflint), Cint,
                     (Ref{FqPolyRepFieldElem}, Ref{FqPolyRepField}),
                     x, x.parent))
end

function is_square_with_sqrt(x::FqPolyRepFieldElem)
   z = parent(x)()
   flag = ccall((:fq_sqrt, libflint), Cint,
                (Ref{FqPolyRepFieldElem}, Ref{FqPolyRepFieldElem}, Ref{FqPolyRepField}),
                z, x, x.parent)
   return (Bool(flag), z)
end

@doc raw"""
    pth_root(x::FqPolyRepFieldElem)

Return the $p$-th root of $x$ in the finite field of characteristic $p$. This
is the inverse operation to the Frobenius map $\sigma_p$.
"""
function pth_root(x::FqPolyRepFieldElem)
   z = parent(x)()
   ccall((:fq_pth_root, libflint), Nothing,
         (Ref{FqPolyRepFieldElem}, Ref{FqPolyRepFieldElem}, Ref{FqPolyRepField}), z, x, x.parent)
   return z
end

@doc raw"""
    tr(x::FqPolyRepFieldElem)

Return the trace of $x$. This is an element of $\mathbb{F}_p$, but the value returned
is this value embedded in the original finite field.
"""
function tr(x::FqPolyRepFieldElem)
   z = ZZRingElem()
   ccall((:fq_trace, libflint), Nothing,
         (Ref{ZZRingElem}, Ref{FqPolyRepFieldElem}, Ref{FqPolyRepField}), z, x, x.parent)
   return parent(x)(z)
end

@doc raw"""
    norm(x::FqPolyRepFieldElem)

Return the norm of $x$. This is an element of $\mathbb{F}_p$, but the value returned
is this value embedded in the original finite field.
"""
function norm(x::FqPolyRepFieldElem)
   z = ZZRingElem()
   ccall((:fq_norm, libflint), Nothing,
         (Ref{ZZRingElem}, Ref{FqPolyRepFieldElem}, Ref{FqPolyRepField}), z, x, x.parent)
   return parent(x)(z)
end

@doc raw"""
    frobenius(x::FqPolyRepFieldElem, n = 1)

Return the iterated Frobenius $\sigma_p^n(x)$ where $\sigma_p$ is the
Frobenius map sending the element $a$ to $a^p$ in the finite field of
characteristic $p$. By default the Frobenius map is applied $n = 1$ times if
$n$ is not specified.
"""
function frobenius(x::FqPolyRepFieldElem, n = 1)
   z = parent(x)()
   ccall((:fq_frobenius, libflint), Nothing,
         (Ref{FqPolyRepFieldElem}, Ref{FqPolyRepFieldElem}, Int, Ref{FqPolyRepField}), z, x, n, x.parent)
   return z
end

###############################################################################
#
#   Lift
#
###############################################################################

@doc raw"""
    lift(R::FpPolyRing, x::FqPolyRepFieldElem)

Lift the finite field element `x` to a polynomial over the prime field.
"""
function lift(R::FpPolyRing, x::FqPolyRepFieldElem)
   c = R()
   ccall((:fq_get_fmpz_mod_poly, libflint), Nothing,
         (Ref{FpPolyRingElem}, Ref{FqPolyRepFieldElem}, Ref{FqPolyRepField}),
                                                     c, x, parent(x))
   return c
end

###############################################################################
#
#   Unsafe functions
#
###############################################################################

function zero!(z::FqPolyRepFieldElem)
   ccall((:fq_zero, libflint), Nothing,
        (Ref{FqPolyRepFieldElem}, Ref{FqPolyRepField}), z, z.parent)
   return z
end

function mul!(z::FqPolyRepFieldElem, x::FqPolyRepFieldElem, y::FqPolyRepFieldElem)
   ccall((:fq_mul, libflint), Nothing,
        (Ref{FqPolyRepFieldElem}, Ref{FqPolyRepFieldElem}, Ref{FqPolyRepFieldElem}, Ref{FqPolyRepField}), z, x, y, y.parent)
   return z
end

function addeq!(z::FqPolyRepFieldElem, x::FqPolyRepFieldElem)
   ccall((:fq_add, libflint), Nothing,
        (Ref{FqPolyRepFieldElem}, Ref{FqPolyRepFieldElem}, Ref{FqPolyRepFieldElem}, Ref{FqPolyRepField}), z, z, x, x.parent)
   return z
end

function add!(z::FqPolyRepFieldElem, x::FqPolyRepFieldElem, y::FqPolyRepFieldElem)
   ccall((:fq_add, libflint), Nothing,
        (Ref{FqPolyRepFieldElem}, Ref{FqPolyRepFieldElem}, Ref{FqPolyRepFieldElem}, Ref{FqPolyRepField}), z, x, y, x.parent)
   return z
end

###############################################################################
#
#   Random functions
#
###############################################################################

Random.Sampler(::Type{RNG}, K::FinField, n::Random.Repetition) where {RNG<:AbstractRNG} =
   Random.SamplerSimple(K, Random.Sampler(RNG, BigInt(0):BigInt(characteristic(K) - 1), n))

function rand(rng::AbstractRNG, Ksp::Random.SamplerSimple{<:FinField})
   K = Ksp[]
   r = degree(K)
   alpha = gen(K)
   res = zero(K)
   for i = 0 : (r-1)
      c = rand(rng, Ksp.data)
      res += c * alpha^i
   end
   return res
end

Random.gentype(::Type{T}) where {T<:FinField} = elem_type(T)

################################################################################
#
#   fqPolyRepField Modulus
#
################################################################################

@doc raw"""
    modulus(k::FqPolyRepField, var::VarName=:T)

Return the modulus defining the finite field $k$.
"""
function modulus(k::FqPolyRepField, var::VarName=:T)
    p = characteristic(k)
    Q = polynomial(GF(p), [], Symbol(var))
    P = ccall((:fq_ctx_modulus, libflint), Ref{FpPolyRingElem},
                 (Ref{FqPolyRepField},), k)
    ccall((:fmpz_mod_poly_set, libflint), Nothing,
          (Ref{FpPolyRingElem}, Ref{FpPolyRingElem}, Ref{FpField}),
          Q, P, base_ring(Q))

    return Q
end

#function defining_polynomial(k::FqPolyRepField)
#   F = FpField(characteristic(k))
#   Fx, = polynomial_ring(F, "x", cached = false)
#   return defining_polynomial(Fx, k)
#end
#
#function defining_polynomial(R::FpPolyRing, k::FqPolyRepField)
#   Q = R()
#   GC.@preserve k begin
#      P = ccall((:fq_ctx_modulus, libflint), Ptr{FpPolyRingElem},
#                (Ref{FqPolyRepField},), k)
#      ccall((:fmpz_mod_poly_set, libflint), Nothing,
#            (Ref{FpPolyRingElem}, Ptr{FpPolyRingElem}),
#            Q, P)
#   end
#   return Q
#end

###############################################################################
#
#   Promotions
#
###############################################################################

promote_rule(::Type{FqPolyRepFieldElem}, ::Type{T}) where {T <: Integer} = FqPolyRepFieldElem

promote_rule(::Type{FqPolyRepFieldElem}, ::Type{ZZRingElem}) = FqPolyRepFieldElem

###############################################################################
#
#   Parent object call overload
#
###############################################################################

function (a::FqPolyRepField)()
   z = FqPolyRepFieldElem(a)
   return z
end

(a::FqPolyRepField)(b::Integer) = a(ZZRingElem(b))

function (a::FqPolyRepField)(b::Int)
   z = FqPolyRepFieldElem(a, b)
   z.parent = a
   return z
end

function (a::FqPolyRepField)(b::ZZRingElem)
   z = FqPolyRepFieldElem(a, b)
   z.parent = a
   return z
end

function (a::FqPolyRepField)(b::FqPolyRepFieldElem)
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

function (a::FqPolyRepField)(b::Vector{<:IntegerUnion})
   da = degree(a)
   db = length(b)
   da < db && error("Coercion impossible")
   F = GF(characteristic(a), cached = false)
   return FqPolyRepFieldElem(a, polynomial(F, b))
end

###############################################################################
#
#   FlintFiniteField constructor
#
###############################################################################

@doc raw"""
    FlintFiniteField(char::ZZRingElem, deg::Int, s::VarName; cached = true)
Returns a tuple $S, x$ consisting of a finite field parent object $S$ and
generator $x$ for the finite field of the given characteristic and degree.
The string $s$ is used to designate how the finite field generator will be
printed. The characteristic must be prime. When a Conway polynomial is known,
the field is generated using the Conway polynomial. Otherwise a random
sparse, irreducible polynomial is used. The generator of the field is
guaranteed to be a multiplicative generator only if the field is generated by
a Conway polynomial. We require the degree to be positive.
"""
function FlintFiniteField(char::ZZRingElem, deg::Int, s::VarName = :o; cached = true)
   parent_obj = FqPolyRepField(char, deg, Symbol(s), cached)
   return parent_obj, gen(parent_obj)
end

@doc raw"""
    FlintFiniteField(pol::Union{ZZModPolyRingElem, FpPolyRingElem}, s::VarName; cached = true, check = true)
Returns a tuple $S, x$ consisting of a finite field parent object $S$ and
generator $x$ for the finite field over $F_p$ defined by the given
polynomial, i.e. $\mathbb{F}_p[t]/(pol)$. The characteristic is specified by
the modulus of `pol`. The polynomial is required to be irreducible, but this
is not checked. The base ring of the polynomial is required to be a field, which
is checked by default. Use `check = false` to disable the check.
The string $s$ is used to designate how the finite field
generator will be printed. The generator will not be multiplicative in
general.
"""
function FlintFiniteField(pol::Union{ZZModPolyRingElem, FpPolyRingElem},
                          s::VarName = :o; cached = true, check::Bool=true)
   parent_obj = FqPolyRepField(pol, Symbol(s), cached, check=check)
   return parent_obj, gen(parent_obj)
end

@doc raw"""
    FlintFiniteField(F::FqPolyRepField, deg::Int, s::VarName; cached = true)

Return a finite field with the same type as `F` but with a possibly different
degree `deg` over the prime subfield.
"""
function FlintFiniteField(F::FqPolyRepField, deg::Int, s::VarName = :o; cached = true)
    return FqPolyRepField(characteristic(F), deg, Symbol(s), cached)
end
