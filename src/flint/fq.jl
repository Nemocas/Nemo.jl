###############################################################################
#
#   fq.jl : Flint finite fields
#
###############################################################################

export FlintFiniteField, characteristic, order, fq, FqFiniteField, frobenius,
       pth_root, tr, norm

###############################################################################
#
#   Type and parent object methods
#
###############################################################################

parent_type(::Type{fq}) = FqFiniteField

elem_type(::Type{FqFiniteField}) = fq

base_ring(a::FqFiniteField) = Union{}

base_ring(a::fq) = Union{}

parent(a::fq) = a.parent

is_domain_type(::Type{fq}) = true

function check_parent(a::fq, b::fq)
   a.parent != b.parent && error("Operations on distinct finite fields not supported")
end

###############################################################################
#
#   Basic manipulation
#
###############################################################################

function Base.hash(a::fq, h::UInt)
   b = 0xb310fb6ea97e1f1a%UInt
   z = fmpz()
   for i in 0:degree(parent(a)) - 1
      ccall((:fmpz_poly_get_coeff_fmpz, libflint), Nothing,
               (Ref{fmpz}, Ref{fq}, Int), z, a, i)
      b = xor(b, xor(hash(z, h), h))
      b = (b << 1) | (b >> (sizeof(Int)*8 - 1))
   end
   return b
end

@doc Markdown.doc"""
    coeff(x::fq, n::Int)

Return the degree $n$ coefficient of the polynomial representing the given
finite field element.
"""
function coeff(x::fq, n::Int)
   n < 0 && throw(DomainError(n, "Index must be non-negative"))
   z = fmpz()
   ccall((:fmpz_poly_get_coeff_fmpz, libflint), Nothing,
               (Ref{fmpz}, Ref{fq}, Int), z, x, n)
   return z
end

function zero(a::FqFiniteField)
   d = a()
   ccall((:fq_zero, libflint), Nothing, (Ref{fq}, Ref{FqFiniteField}), d, a)
   return d
end

function one(a::FqFiniteField)
   d = a()
   ccall((:fq_one, libflint), Nothing, (Ref{fq}, Ref{FqFiniteField}), d, a)
   return d
end

@doc Markdown.doc"""
    gen(a::FqFiniteField)

Return the generator of the finite field. Note that this is only guaranteed
to be a multiplicative generator if the finite field is generated by a
Conway polynomial automatically.
"""
function gen(a::FqFiniteField)
   d = a()
   ccall((:fq_gen, libflint), Nothing, (Ref{fq}, Ref{FqFiniteField}), d, a)
   return d
end

iszero(a::fq) = ccall((:fq_is_zero, libflint), Bool,
                     (Ref{fq}, Ref{FqFiniteField}), a, a.parent)

isone(a::fq) = ccall((:fq_is_one, libflint), Bool,
                    (Ref{fq}, Ref{FqFiniteField}), a, a.parent)

@doc Markdown.doc"""
    is_gen(a::fq)

Return `true` if the given finite field element is the generator of the
finite field, otherwise return `false`.
"""
is_gen(a::fq) = a == gen(parent(a))

is_unit(a::fq) = ccall((:fq_is_invertible, libflint), Bool,
                     (Ref{fq}, Ref{FqFiniteField}), a, a.parent)

function characteristic(a::FqFiniteField)
   d = fmpz()
   ccall((:__fq_ctx_prime, libflint), Nothing,
         (Ref{fmpz}, Ref{FqFiniteField}), d, a)
   return d
end

function order(a::FqFiniteField)
   d = fmpz()
   ccall((:fq_ctx_order, libflint), Nothing,
         (Ref{fmpz}, Ref{FqFiniteField}), d, a)
   return d
end

@doc Markdown.doc"""
    degree(a::FqFiniteField)

Return the degree of the given finite field.
"""
function degree(a::FqFiniteField)
   return ccall((:fq_ctx_degree, libflint), Int, (Ref{FqFiniteField},), a)
end

function deepcopy_internal(d::fq, dict::IdDict)
   z = fq(parent(d), d)
   return z
end

###############################################################################
#
#   Canonicalisation
#
###############################################################################

canonical_unit(x::fq) = x

###############################################################################
#
#   AbstractString I/O
#
###############################################################################

function expressify(a::fq; context = nothing)
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

show(io::IO, a::fq) = print(io, AbstractAlgebra.obj_to_string(a, context = io))

function show(io::IO, a::FqFiniteField)
   print(io, "Finite field of degree ", degree(a))
   print(io, " over F_", characteristic(a))
end

###############################################################################
#
#   Unary operations
#
###############################################################################

function -(x::fq)
   z = parent(x)()
   ccall((:fq_neg, libflint), Nothing,
         (Ref{fq}, Ref{fq}, Ref{FqFiniteField}), z, x, x.parent)
   return z
end

###############################################################################
#
#   Binary operations
#
###############################################################################

function +(x::fq, y::fq)
   check_parent(x, y)
   z = parent(y)()
   ccall((:fq_add, libflint), Nothing,
        (Ref{fq}, Ref{fq}, Ref{fq}, Ref{FqFiniteField}), z, x, y, y.parent)
   return z
end

function -(x::fq, y::fq)
   check_parent(x, y)
   z = parent(y)()
   ccall((:fq_sub, libflint), Nothing,
        (Ref{fq}, Ref{fq}, Ref{fq}, Ref{FqFiniteField}), z, x, y, y.parent)
   return z
end

function *(x::fq, y::fq)
   check_parent(x, y)
   z = parent(y)()
   ccall((:fq_mul, libflint), Nothing,
        (Ref{fq}, Ref{fq}, Ref{fq}, Ref{FqFiniteField}), z, x, y, y.parent)
   return z
end

###############################################################################
#
#   Ad hoc binary operators
#
###############################################################################

function *(x::Int, y::fq)
   z = parent(y)()
   ccall((:fq_mul_si, libflint), Nothing,
         (Ref{fq}, Ref{fq}, Int, Ref{FqFiniteField}), z, y, x, y.parent)
   return z
end

*(x::Integer, y::fq) = fmpz(x)*y

*(x::fq, y::Integer) = y*x

function *(x::fmpz, y::fq)
   z = parent(y)()
   ccall((:fq_mul_fmpz, libflint), Nothing,
         (Ref{fq}, Ref{fq}, Ref{fmpz}, Ref{FqFiniteField}),
                                            z, y, x, y.parent)
   return z
end

*(x::fq, y::fmpz) = y*x

+(x::fq, y::Integer) = x + parent(x)(y)

+(x::Integer, y::fq) = y + x

+(x::fq, y::fmpz) = x + parent(x)(y)

+(x::fmpz, y::fq) = y + x

-(x::fq, y::Integer) = x - parent(x)(y)

-(x::Integer, y::fq) = parent(y)(x) - y

-(x::fq, y::fmpz) = x - parent(x)(y)

-(x::fmpz, y::fq) = parent(y)(x) - y

###############################################################################
#
#   Powering
#
###############################################################################

function ^(x::fq, y::Int)
   if y < 0
      x = inv(x)
      y = -y
   end
   z = parent(x)()
   ccall((:fq_pow_ui, libflint), Nothing,
         (Ref{fq}, Ref{fq}, Int, Ref{FqFiniteField}), z, x, y, x.parent)
   return z
end

function ^(x::fq, y::fmpz)
   if y < 0
      x = inv(x)
      y = -y
   end
   z = parent(x)()
   ccall((:fq_pow, libflint), Nothing,
         (Ref{fq}, Ref{fq}, Ref{fmpz}, Ref{FqFiniteField}),
                                            z, x, y, x.parent)
   return z
end

###############################################################################
#
#   Comparison
#
###############################################################################

function ==(x::fq, y::fq)
   check_parent(x, y)
   ccall((:fq_equal, libflint), Bool,
         (Ref{fq}, Ref{fq}, Ref{FqFiniteField}), x, y, y.parent)
end

###############################################################################
#
#   Ad hoc comparison
#
###############################################################################

==(x::fq, y::Integer) = x == parent(x)(y)

==(x::fq, y::fmpz) = x == parent(x)(y)

==(x::Integer, y::fq) = parent(y)(x) == y

==(x::fmpz, y::fq) = parent(y)(x) == y

###############################################################################
#
#   Exact division
#
###############################################################################

function divexact(x::fq, y::fq; check::Bool=true)
   check_parent(x, y)
   iszero(y) && throw(DivideError())
   z = parent(y)()
   ccall((:fq_div, libflint), Nothing,
        (Ref{fq}, Ref{fq}, Ref{fq}, Ref{FqFiniteField}), z, x, y, y.parent)
   return z
end

function divides(a::fq, b::fq)
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

divexact(x::fq, y::Integer; check::Bool=true) = divexact(x, parent(x)(y); check=check)

divexact(x::fq, y::fmpz; check::Bool=true) = divexact(x, parent(x)(y); check=check)

divexact(x::Integer, y::fq; check::Bool=true) = divexact(parent(y)(x), y; check=check)

divexact(x::fmpz, y::fq; check::Bool=true) = divexact(parent(y)(x), y; check=check)

###############################################################################
#
#   Inversion
#
###############################################################################

function inv(x::fq)
   iszero(x) && throw(DivideError())
   z = parent(x)()
   ccall((:fq_inv, libflint), Nothing,
         (Ref{fq}, Ref{fq}, Ref{FqFiniteField}), z, x, x.parent)
   return z
end

###############################################################################
#
#   Special functions
#
###############################################################################

function sqrt(x::fq; check::Bool=true)
   z = parent(x)()
   res = Bool(ccall((:fq_sqrt, libflint), Cint,
                    (Ref{fq}, Ref{fq}, Ref{FqFiniteField}),
                    z, x, x.parent))
   check && !res && error("Not a square")
   return z
end

function is_square(x::fq)
   return Bool(ccall((:fq_is_square, libflint), Cint,
                     (Ref{fq}, Ref{FqFiniteField}),
                     x, x.parent))
end

function is_square_with_sqrt(x::fq)
   z = parent(x)()
   flag = ccall((:fq_sqrt, libflint), Cint,
                (Ref{fq}, Ref{fq}, Ref{FqFiniteField}),
                z, x, x.parent)
   return (Bool(flag), z)
end

@doc Markdown.doc"""
    pth_root(x::fq)

Return the $p$-th root of $x$ in the finite field of characteristic $p$. This
is the inverse operation to the Frobenius map $\sigma_p$.
"""
function pth_root(x::fq)
   z = parent(x)()
   ccall((:fq_pth_root, libflint), Nothing,
         (Ref{fq}, Ref{fq}, Ref{FqFiniteField}), z, x, x.parent)
   return z
end

@doc Markdown.doc"""
    tr(x::fq)

Return the trace of $x$. This is an element of $\mathbb{F}_p$, but the value returned
is this value embedded in the original finite field.
"""
function tr(x::fq)
   z = fmpz()
   ccall((:fq_trace, libflint), Nothing,
         (Ref{fmpz}, Ref{fq}, Ref{FqFiniteField}), z, x, x.parent)
   return parent(x)(z)
end

@doc Markdown.doc"""
    norm(x::fq)

Return the norm of $x$. This is an element of $\mathbb{F}_p$, but the value returned
is this value embedded in the original finite field.
"""
function norm(x::fq)
   z = fmpz()
   ccall((:fq_norm, libflint), Nothing,
         (Ref{fmpz}, Ref{fq}, Ref{FqFiniteField}), z, x, x.parent)
   return parent(x)(z)
end

@doc Markdown.doc"""
    frobenius(x::fq, n = 1)

Return the iterated Frobenius $\sigma_p^n(x)$ where $\sigma_p$ is the
Frobenius map sending the element $a$ to $a^p$ in the finite field of
characteristic $p$. By default the Frobenius map is applied $n = 1$ times if
$n$ is not specified.
"""
function frobenius(x::fq, n = 1)
   z = parent(x)()
   ccall((:fq_frobenius, libflint), Nothing,
         (Ref{fq}, Ref{fq}, Int, Ref{FqFiniteField}), z, x, n, x.parent)
   return z
end

###############################################################################
#
#   Lift
#
###############################################################################

@doc Markdown.doc"""
    lift(R::GFPFmpzPolyRing, x::fq)

Lift the finite field element `x` to a polynomial over the prime field.
"""
function lift(R::GFPFmpzPolyRing, x::fq)
   c = R()
   ccall((:fq_get_fmpz_mod_poly, libflint), Nothing,
         (Ref{gfp_fmpz_poly}, Ref{fq}, Ref{FqFiniteField}),
                                                     c, x, parent(x))
   return c
end

###############################################################################
#
#   Unsafe functions
#
###############################################################################

function zero!(z::fq)
   ccall((:fq_zero, libflint), Nothing,
        (Ref{fq}, Ref{FqFiniteField}), z, z.parent)
   return z
end

function mul!(z::fq, x::fq, y::fq)
   ccall((:fq_mul, libflint), Nothing,
        (Ref{fq}, Ref{fq}, Ref{fq}, Ref{FqFiniteField}), z, x, y, y.parent)
   return z
end

function addeq!(z::fq, x::fq)
   ccall((:fq_add, libflint), Nothing,
        (Ref{fq}, Ref{fq}, Ref{fq}, Ref{FqFiniteField}), z, z, x, x.parent)
   return z
end

function add!(z::fq, x::fq, y::fq)
   ccall((:fq_add, libflint), Nothing,
        (Ref{fq}, Ref{fq}, Ref{fq}, Ref{FqFiniteField}), z, x, y, x.parent)
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
#   FqNmodFiniteField Modulus
#
################################################################################

@doc Markdown.doc"""
    modulus(k::FqFiniteField, var::String="T")

Return the modulus defining the finite field $k$.
"""
function modulus(k::FqFiniteField, var::String="T")
    p = characteristic(k)
    Q = polynomial(GF(p), [], var)
    P = ccall((:fq_ctx_modulus, libflint), Ref{gfp_fmpz_poly},
                 (Ref{FqFiniteField},), k)
    ccall((:fmpz_mod_poly_set, libflint), Nothing,
          (Ref{gfp_fmpz_poly}, Ref{gfp_fmpz_poly}, Ref{GaloisFmpzField}),
          Q, P, base_ring(Q))

    return Q
end

###############################################################################
#
#   Promotions
#
###############################################################################

promote_rule(::Type{fq}, ::Type{T}) where {T <: Integer} = fq

promote_rule(::Type{fq}, ::Type{fmpz}) = fq

###############################################################################
#
#   Parent object call overload
#
###############################################################################

function (a::FqFiniteField)()
   z = fq(a)
   return z
end

(a::FqFiniteField)(b::Integer) = a(fmpz(b))

function (a::FqFiniteField)(b::Int)
   z = fq(a, b)
   z.parent = a
   return z
end

function (a::FqFiniteField)(b::fmpz)
   z = fq(a, b)
   z.parent = a
   return z
end

function (a::FqFiniteField)(b::fq)
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

@doc Markdown.doc"""
    FlintFiniteField(char::fmpz, deg::Int, s::AbstractString; cached = true)
Returns a tuple $S, x$ consisting of a finite field parent object $S$ and
generator $x$ for the finite field of the given characteristic and degree.
The string $s$ is used to designate how the finite field generator will be
printed. The characteristic must be prime. When a Conway polynomial is known,
the field is generated using the Conway polynomial. Otherwise a random
sparse, irreducible polynomial is used. The generator of the field is
guaranteed to be a multiplicative generator only if the field is generated by
a Conway polynomial. We require the degree to be positive.
"""
function FlintFiniteField(char::fmpz, deg::Int, s::Union{AbstractString,Symbol} = :o; cached = true)
   S = Symbol(s)
   parent_obj = FqFiniteField(char, deg, S, cached)
   return parent_obj, gen(parent_obj)
end

@doc Markdown.doc"""
    FlintFiniteField(pol::Union{fmpz_mod_poly, gfp_fmpz_poly}, s::Union{AbstractString,Symbol}; cached = true, check = true)
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
function FlintFiniteField(pol::Union{fmpz_mod_poly, gfp_fmpz_poly},
                          s::Union{AbstractString,Symbol} = :o; cached = true, check::Bool=true)
   S = Symbol(s)
   parent_obj = FqFiniteField(pol, S, cached, check=check)

   return parent_obj, gen(parent_obj)
end

@doc Markdown.doc"""
    FlintFiniteField(F::FqFiniteField, deg::Int, s::AbstractString; cached = true)

Return a finite field with the same type as `F` but with a possibly different
degree `deg` over the prime subfield.
"""
function FlintFiniteField(F::FqFiniteField, deg::Int,
                          s::Union{AbstractString,Symbol} = :o; cached = true)
    return FqFiniteField(characteristic(F), deg, Symbol(s), cached)
end
