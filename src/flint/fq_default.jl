###############################################################################
#
#   fq_default.jl : Flint finite fields
#
###############################################################################

export FqFiniteField, fq_default, FqDefaultFiniteField

###############################################################################
#
#   Type and parent object methods
#
###############################################################################

parent_type(::Type{fq_default}) = FqDefaultFiniteField

elem_type(::Type{FqDefaultFiniteField}) = fq_default

base_ring(a::FqDefaultFiniteField) = Union{}

base_ring(a::fq_default) = Union{}

parent(a::fq_default) = a.parent

is_domain_type(::Type{fq_default}) = true

function check_parent(a::fq_default, b::fq_default)
   a.parent != b.parent && error("Operations on distinct finite fields not supported")
end

###############################################################################
#
#   Basic manipulation
#
###############################################################################

function Base.hash(a::fq_default, h::UInt)
   b = 0xb310fb6ea97e1f1a%UInt
   for i in 0:_degree(parent(a)) - 1
      b = xor(b, xor(hash(_coeff(a, i), h), h))
      b = (b << 1) | (b >> (sizeof(Int)*8 - 1))
   end
   return b
end

function _coeff(x::fq_default, n::Int)
   n < 0 && throw(DomainError(n, "Index must be non-negative"))
   z = fmpz()
   ccall((:fq_default_get_coeff_fmpz, libflint), Nothing,
               (Ref{fmpz}, Ref{fq_default}, Int, Ref{FqDefaultFiniteField}),
                                                           z, x, n, parent(x))
   return z
end

function zero(a::FqDefaultFiniteField)
   d = a()
   ccall((:fq_default_zero, libflint), Nothing, (Ref{fq_default}, Ref{FqDefaultFiniteField}), d, a)
   return d
end

function one(a::FqDefaultFiniteField)
   d = a()
   ccall((:fq_default_one, libflint), Nothing, (Ref{fq_default}, Ref{FqDefaultFiniteField}), d, a)
   return d
end

function _gen(a::FqDefaultFiniteField)
   d = a()
   ccall((:fq_default_gen, libflint), Nothing, (Ref{fq_default}, Ref{FqDefaultFiniteField}), d, a)
   return d
end

iszero(a::fq_default) = ccall((:fq_default_is_zero, libflint), Bool,
                     (Ref{fq_default}, Ref{FqDefaultFiniteField}), a, a.parent)

isone(a::fq_default) = ccall((:fq_default_is_one, libflint), Bool,
                    (Ref{fq_default}, Ref{FqDefaultFiniteField}), a, a.parent)

_is_gen(a::fq_default) = a == _gen(parent(a))

is_unit(a::fq_default) = ccall((:fq_default_is_invertible, libflint), Bool,
                     (Ref{fq_default}, Ref{FqDefaultFiniteField}), a, a.parent)

function characteristic(a::FqDefaultFiniteField)
   d = fmpz()
   ccall((:fq_default_ctx_prime, libflint), Nothing,
         (Ref{fmpz}, Ref{FqDefaultFiniteField}), d, a)
   return d
end

function order(a::FqDefaultFiniteField)
   d = fmpz()
   ccall((:fq_default_ctx_order, libflint), Nothing,
         (Ref{fmpz}, Ref{FqDefaultFiniteField}), d, a)
   return d
end

function _degree(a::FqDefaultFiniteField)
    return ccall((:fq_default_ctx_degree, libflint), Int, (Ref{FqDefaultFiniteField},), a)
end

function deepcopy_internal(d::fq_default, dict::IdDict)
   z = fq_default(parent(d), d)
   return z
end

###############################################################################
#
#   Lifts and conversions
#
###############################################################################

function lift(R::FlintIntegerRing, x::fq_default)
  z = R()
  ok = ccall((:fq_default_get_fmpz, libflint), Cint,
             (Ref{fmpz}, Ref{fq_default}, Ref{FqDefaultFiniteField}),
             z, x, parent(x))
  ok == 0 && error("cannot lift")
  return z
end

function lift(R::FmpzPolyRing, x::fq_default)
   p = R()
   ccall((:fq_default_get_fmpz_poly, libflint), Nothing,
         (Ref{fmpz_poly}, Ref{fq_default}, Ref{FqDefaultFiniteField}),
          p, x, parent(x))
   return p
end

function (R::NmodPolyRing)(x::fq_default)
   p = R()
   ccall((:fq_default_get_nmod_poly, libflint), Nothing,
         (Ref{nmod_poly}, Ref{fq_default}, Ref{FqDefaultFiniteField}),
          p, x, parent(x))
   return p
end

function (R::GFPPolyRing)(x::fq_default)
   p = R()
   ccall((:fq_default_get_nmod_poly, libflint), Nothing,
         (Ref{gfp_poly}, Ref{fq_default}, Ref{FqDefaultFiniteField}),
          p, x, parent(x))
   return p
end

function (R::FmpzModPolyRing)(x::fq_default)
   p = R()
   ccall((:fq_default_get_fmpz_mod_poly, libflint), Nothing,
         (Ref{fmpz_mod_poly}, Ref{fq_default}, Ref{FqDefaultFiniteField}),
          p, x, parent(x))
   return p
end

function (R::GFPFmpzPolyRing)(x::fq_default)
   p = R()
   ccall((:fq_default_get_fmpz_mod_poly, libflint), Nothing,
         (Ref{gfp_fmpz_poly}, Ref{fq_default}, Ref{FqDefaultFiniteField}),
          p, x, parent(x))
   return p
end

# with fq
function _unchecked_coerce(a::FqFiniteField, b::fq_default)
    x = fmpz_poly()
    ccall((:fq_default_get_fmpz_poly, libflint), Nothing,
         (Ref{fmpz_poly}, Ref{fq_default}, Ref{FqDefaultFiniteField}),
          x, b, parent(b))
    return fq(a, x)
end

function _unchecked_coerce(a::FqDefaultFiniteField, b::fq)
    x = fmpz_poly()
    ccall((:fq_get_fmpz_poly, libflint), Nothing,
         (Ref{fmpz_poly}, Ref{fq}, Ref{FqFiniteField}),
          x, b, parent(b))
    return fq_default(a, x)
end

# with nmod
function _unchecked_coerce(a::GaloisField, b::fq_default)
    iszero(b) && return zero(a)
    x = fmpz_poly()
    ccall((:fq_default_get_fmpz_poly, libflint), Nothing,
         (Ref{fmpz_poly}, Ref{fq_default}, Ref{FqDefaultFiniteField}),
          x, b, parent(b))
    return a(coeff(x, 0))
end

function _unchecked_coerce(a::FqDefaultFiniteField, b::gfp_elem)
    return fq_default(a, lift(b))
end

# with fq_nmod
function _unchecked_coerce(a::FqNmodFiniteField, b::fq_default)
    x = nmod_poly(UInt(characteristic(a)))
    ccall((:fq_default_get_nmod_poly, libflint), Nothing,
         (Ref{nmod_poly}, Ref{fq_default}, Ref{FqDefaultFiniteField}),
          x, b, parent(b))
    y = a()
    ccall((:fq_nmod_set_nmod_poly, libflint), Nothing,
         (Ref{fq_nmod}, Ref{nmod_poly}, Ref{FqNmodFiniteField}),
          y, x, a)
    return y
end

function _unchecked_coerce(a::FqDefaultFiniteField, b::fq_nmod)
    x = nmod_poly(UInt(characteristic(parent(b))))
    ccall((:fq_nmod_get_nmod_poly, libflint), Nothing,
         (Ref{nmod_poly}, Ref{fq_nmod}, Ref{FqNmodFiniteField}),
          x, b, parent(b))
    return fq_default(a, x)
end


###############################################################################
#
#   Canonicalisation
#
###############################################################################

canonical_unit(x::fq_default) = x

###############################################################################
#
#   AbstractString I/O
#
###############################################################################

function expressify(a::fq_default; context = nothing)
   x = a.parent.var
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

show(io::IO, a::fq_default) = print(io, AbstractAlgebra.obj_to_string(a, context = io))

function show(io::IO, a::FqDefaultFiniteField)
   print(io, "Finite field of degree ", _degree(a))
   print(io, " over F_", characteristic(a))
   if !is_absolute(a)
     print(io, " (over ", base_field(a), ")")
   end
end

###############################################################################
#
#   Unary operations
#
###############################################################################

function -(x::fq_default)
   z = parent(x)()
   ccall((:fq_default_neg, libflint), Nothing,
         (Ref{fq_default}, Ref{fq_default}, Ref{FqDefaultFiniteField}), z, x, x.parent)
   return z
end

###############################################################################
#
#   Binary operations
#
###############################################################################

function +(x::fq_default, y::fq_default)
   check_parent(x, y)
   z = parent(y)()
   ccall((:fq_default_add, libflint), Nothing,
        (Ref{fq_default}, Ref{fq_default}, Ref{fq_default}, Ref{FqDefaultFiniteField}), z, x, y, y.parent)
   return z
end

function -(x::fq_default, y::fq_default)
   check_parent(x, y)
   z = parent(y)()
   ccall((:fq_default_sub, libflint), Nothing,
        (Ref{fq_default}, Ref{fq_default}, Ref{fq_default}, Ref{FqDefaultFiniteField}), z, x, y, y.parent)
   return z
end

function *(x::fq_default, y::fq_default)
   check_parent(x, y)
   z = parent(y)()
   ccall((:fq_default_mul, libflint), Nothing,
        (Ref{fq_default}, Ref{fq_default}, Ref{fq_default}, Ref{FqDefaultFiniteField}), z, x, y, y.parent)
   return z
end

###############################################################################
#
#   Ad hoc binary operators
#
###############################################################################

function *(x::Int, y::fq_default)
   z = parent(y)()
   ccall((:fq_default_mul_si, libflint), Nothing,
         (Ref{fq_default}, Ref{fq_default}, Int, Ref{FqDefaultFiniteField}), z, y, x, y.parent)
   return z
end

*(x::Integer, y::fq_default) = fmpz(x)*y

*(x::fq_default, y::Integer) = y*x

function *(x::fmpz, y::fq_default)
   z = parent(y)()
   ccall((:fq_default_mul_fmpz, libflint), Nothing,
         (Ref{fq_default}, Ref{fq_default}, Ref{fmpz}, Ref{FqDefaultFiniteField}),
                                            z, y, x, y.parent)
   return z
end

*(x::fq_default, y::fmpz) = y*x

+(x::fq_default, y::Integer) = x + parent(x)(y)

+(x::Integer, y::fq_default) = y + x

+(x::fq_default, y::fmpz) = x + parent(x)(y)

+(x::fmpz, y::fq_default) = y + x

-(x::fq_default, y::Integer) = x - parent(x)(y)

-(x::Integer, y::fq_default) = parent(y)(x) - y

-(x::fq_default, y::fmpz) = x - parent(x)(y)

-(x::fmpz, y::fq_default) = parent(y)(x) - y

###############################################################################
#
#   Powering
#
###############################################################################

function ^(x::fq_default, y::Int)
   if y < 0
      x = inv(x)
      y = -y
   end
   z = parent(x)()
   ccall((:fq_default_pow_ui, libflint), Nothing,
         (Ref{fq_default}, Ref{fq_default}, Int, Ref{FqDefaultFiniteField}), z, x, y, x.parent)
   return z
end

function ^(x::fq_default, y::fmpz)
   if y < 0
      x = inv(x)
      y = -y
   end
   z = parent(x)()
   ccall((:fq_default_pow, libflint), Nothing,
         (Ref{fq_default}, Ref{fq_default}, Ref{fmpz}, Ref{FqDefaultFiniteField}),
                                            z, x, y, x.parent)
   return z
end

###############################################################################
#
#   Comparison
#
###############################################################################

function ==(x::fq_default, y::fq_default)
   check_parent(x, y)
   ccall((:fq_default_equal, libflint), Bool,
         (Ref{fq_default}, Ref{fq_default}, Ref{FqDefaultFiniteField}), x, y, y.parent)
end

###############################################################################
#
#   Ad hoc comparison
#
###############################################################################

==(x::fq_default, y::Integer) = x == parent(x)(y)

==(x::fq_default, y::fmpz) = x == parent(x)(y)

==(x::Integer, y::fq_default) = parent(y)(x) == y

==(x::fmpz, y::fq_default) = parent(y)(x) == y

###############################################################################
#
#   Exact division
#
###############################################################################

function divexact(x::fq_default, y::fq_default; check::Bool=true)
   check_parent(x, y)
   iszero(y) && throw(DivideError())
   z = parent(y)()
   ccall((:fq_default_div, libflint), Nothing,
        (Ref{fq_default}, Ref{fq_default}, Ref{fq_default}, Ref{FqDefaultFiniteField}), z, x, y, y.parent)
   return z
end

function divides(a::fq_default, b::fq_default)
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

divexact(x::fq_default, y::Integer; check::Bool=true) = divexact(x, parent(x)(y); check=check)

divexact(x::fq_default, y::fmpz; check::Bool=true) = divexact(x, parent(x)(y); check=check)

divexact(x::Integer, y::fq_default; check::Bool=true) = divexact(parent(y)(x), y; check=check)

divexact(x::fmpz, y::fq_default; check::Bool=true) = divexact(parent(y)(x), y; check=check)

###############################################################################
#
#   Inversion
#
###############################################################################

function inv(x::fq_default)
   iszero(x) && throw(DivideError())
   z = parent(x)()
   ccall((:fq_default_inv, libflint), Nothing,
         (Ref{fq_default}, Ref{fq_default}, Ref{FqDefaultFiniteField}), z, x, x.parent)
   return z
end

###############################################################################
#
#   Special functions
#
###############################################################################

function sqrt(x::fq_default)
   z = parent(x)()
   res = Bool(ccall((:fq_default_sqrt, libflint), Cint,
                    (Ref{fq_default}, Ref{fq_default}, Ref{FqDefaultFiniteField}),
                    z, x, x.parent))
   res || error("Not a square")
   return z
end

function is_square(x::fq_default)
   return Bool(ccall((:fq_default_is_square, libflint), Cint,
                     (Ref{fq_default}, Ref{FqDefaultFiniteField}),
                     x, x.parent))
end

function is_square_with_sqrt(x::fq_default)
   z = parent(x)()
   flag = ccall((:fq_default_sqrt, libflint), Cint,
                (Ref{fq_default}, Ref{fq_default}, Ref{FqDefaultFiniteField}),
                z, x, x.parent)
   return (Bool(flag), z)
end

@doc Markdown.doc"""
    pth_root(x::fq_default)

Return the $p$-th root of $x$ in the finite field of characteristic $p$. This
is the inverse operation to the absolute Frobenius map.
"""
function pth_root(x::fq_default)
   z = parent(x)()
   ccall((:fq_default_pth_root, libflint), Nothing,
         (Ref{fq_default}, Ref{fq_default}, Ref{FqDefaultFiniteField}), z, x, x.parent)
   return z
end

function _tr(x::fq_default)
   z = fmpz()
   ccall((:fq_default_trace, libflint), Nothing,
         (Ref{fmpz}, Ref{fq_default}, Ref{FqDefaultFiniteField}), z, x, x.parent)
   return z
end

function _norm(x::fq_default)
   z = fmpz()
   ccall((:fq_default_norm, libflint), Nothing,
         (Ref{fmpz}, Ref{fq_default}, Ref{FqDefaultFiniteField}), z, x, x.parent)
   return z
end

function _frobenius(x::fq_default, n = 1)
   z = parent(x)()
   ccall((:fq_default_frobenius, libflint), Nothing,
         (Ref{fq_default}, Ref{fq_default}, Int, Ref{FqDefaultFiniteField}), z, x, n, x.parent)
   return z
end

###############################################################################
#
#   Unsafe functions
#
###############################################################################

function zero!(z::fq_default)
   ccall((:fq_default_zero, libflint), Nothing,
        (Ref{fq_default}, Ref{FqDefaultFiniteField}), z, z.parent)
   z.poly = nothing
   return z
end

function mul!(z::fq_default, x::fq_default, y::fq_default)
   ccall((:fq_default_mul, libflint), Nothing,
        (Ref{fq_default}, Ref{fq_default}, Ref{fq_default}, Ref{FqDefaultFiniteField}), z, x, y, y.parent)
   z.poly = nothing
   return z
end

function addeq!(z::fq_default, x::fq_default)
   ccall((:fq_default_add, libflint), Nothing,
        (Ref{fq_default}, Ref{fq_default}, Ref{fq_default}, Ref{FqDefaultFiniteField}), z, z, x, x.parent)
   z.poly = nothing
   return z
end

function add!(z::fq_default, x::fq_default, y::fq_default)
   ccall((:fq_default_add, libflint), Nothing,
        (Ref{fq_default}, Ref{fq_default}, Ref{fq_default}, Ref{FqDefaultFiniteField}), z, x, y, x.parent)
   z.poly = nothing
   return z
end

###############################################################################
#
#   Random functions
#
###############################################################################

# define rand(::FqDefaultFiniteField)

Random.Sampler(::Type{RNG}, R::FqDefaultFiniteField, n::Random.Repetition) where {RNG<:AbstractRNG} =
   Random.SamplerSimple(R, Random.Sampler(RNG, BigInt(0):BigInt(order(R))-1, n))

function rand(rng::AbstractRNG, R::Random.SamplerSimple{FqDefaultFiniteField})
   F = R[]
   x = _gen(F)
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

Random.gentype(::Type{FqDefaultFiniteField}) = elem_type(FqDefaultFiniteField)

# define rand(make(::FqDefaultFiniteField, arr)), where arr is any abstract array with integer or fmpz entries

RandomExtensions.maketype(R::FqDefaultFiniteField, _) = elem_type(R)

rand(rng::AbstractRNG, sp::SamplerTrivial{<:Make2{fq_default,FqDefaultFiniteField,<:AbstractArray{<:IntegerUnion}}}) =
   sp[][1](rand(rng, sp[][2]))

# define rand(::FqDefaultFiniteField, arr), where arr is any abstract array with integer or fmpz entries

rand(r::Random.AbstractRNG, R::FqDefaultFiniteField, b::AbstractArray) = rand(r, make(R, b))

rand(R::FqDefaultFiniteField, b::AbstractArray) = rand(Random.GLOBAL_RNG, R, b)

###############################################################################
#
#   Modulus
#
###############################################################################

function modulus(k::FqDefaultFiniteField, var::String="T")
    p = characteristic(k)
    Q = polynomial(GF(p), [], var)
    ccall((:fq_default_ctx_modulus, libflint), Nothing,
          (Ref{gfp_fmpz_poly}, Ref{FqDefaultFiniteField}),
          Q, k)
    return Q
end


###############################################################################
#
#   Promotions
#
###############################################################################

promote_rule(::Type{fq_default}, ::Type{T}) where {T <: Integer} = fq_default

promote_rule(::Type{fq_default}, ::Type{fmpz}) = fq_default

###############################################################################
#
#   Parent object call overload
#
###############################################################################

function (a::FqDefaultFiniteField)()
   z = fq_default(a)
   return z
end

(a::FqDefaultFiniteField)(b::Integer) = a(fmpz(b))

function (a::FqDefaultFiniteField)(b::Int)
   z = fq_default(a, b)
   return z
end

function (a::FqDefaultFiniteField)(b::fmpz)
   z = fq_default(a, b)
   return z
end

function (a::FqDefaultFiniteField)(b::fmpz_poly)
   z = fq_default(a, b)
   return z
end

function (a::FqDefaultFiniteField)(b::Union{nmod_poly, gfp_poly})
   characteristic(parent(b)) != characteristic(a) &&
                        error("Incompatible characteristic")
   z = fq_default(a, b)
   return z
end

function (a::FqDefaultFiniteField)(b::Union{fmpz_mod_poly, gfp_fmpz_poly})
   characteristic(parent(b)) != characteristic(a) &&
                        error("Incompatible characteristic")
   z = fq_default(a, b)
   return z
end

###############################################################################
#
#   FlintFiniteField constructor
#
###############################################################################

@doc Markdown.doc"""
    NGFiniteField(char::IntegerUnion, deg::Int, s::AbstractString; cached = true, check = true)

Returns a tuple $S, x$ consisting of a finite field $S$ of degree `deg` with
characteristic `char` and generator $x$ for the finite field of the given
characteristic and degree. The string $s$ is used to designate how the finite
field generator will be
printed.
"""
function NGFiniteField(char::IntegerUnion, deg::Int, s::AbstractString = "o"; cached = true, check::Bool = true)
   check && !is_prime(char) && error("Characteristic must be prime")
   _char = fmpz(char)
   S = Symbol(s)
   parent_obj = FqDefaultFiniteField(_char, deg, S, cached)

   return parent_obj, _gen(parent_obj)
end

@doc Markdown.doc"""
    NGFiniteField(pol::Union{fmpz_mod_poly, gfp_fmpz_poly}, s::AbstractString; cached = true, check = true)

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
function NGFiniteField(pol::Union{fmpz_mod_poly, gfp_fmpz_poly, nmod_poly, gfp_poly},
                          s::AbstractString; cached = true, check::Bool=true)
   S = Symbol(s)
   parent_obj = FqDefaultFiniteField(pol, S, cached, check=check)

   return parent_obj, _gen(parent_obj)
end
