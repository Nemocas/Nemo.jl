###############################################################################
#
#   FqFieldElem.jl : Flint finite fields
#
###############################################################################

###############################################################################
#
#   Type and parent object methods
#
###############################################################################

parent_type(::Type{FqFieldElem}) = FqField

elem_type(::Type{FqField}) = FqFieldElem

base_ring(a::FqField) = Union{}

parent(a::FqFieldElem) = a.parent

is_domain_type(::Type{FqFieldElem}) = true

function check_parent(a::FqFieldElem, b::FqFieldElem)
   a.parent != b.parent && error("Operations on distinct finite fields not supported")
end

###############################################################################
#
#   Basic manipulation
#
###############################################################################

function Base.hash(a::FqFieldElem, h::UInt)
   b = 0xb310fb6ea97e1f1a%UInt
   for i in 0:_degree(parent(a)) - 1
      b = xor(b, xor(hash(_coeff(a, i), h), h))
      b = (b << 1) | (b >> (sizeof(Int)*8 - 1))
   end
   return b
end

function _coeff(x::FqFieldElem, n::Int)
   n < 0 && throw(DomainError(n, "Index must be non-negative"))
   z = ZZRingElem()
   ccall((:fq_default_get_coeff_fmpz, libflint), Nothing,
               (Ref{ZZRingElem}, Ref{FqFieldElem}, Int, Ref{FqField}),
                                                           z, x, n, parent(x))
   return z
end

function zero(a::FqField)
   d = a()
   ccall((:fq_default_zero, libflint), Nothing, (Ref{FqFieldElem}, Ref{FqField}), d, a)
   return d
end

function one(a::FqField)
   d = a()
   ccall((:fq_default_one, libflint), Nothing, (Ref{FqFieldElem}, Ref{FqField}), d, a)
   return d
end

function _gen(a::FqField)
   d = a()
   ccall((:fq_default_gen, libflint), Nothing, (Ref{FqFieldElem}, Ref{FqField}), d, a)
   return d
end

iszero(a::FqFieldElem) = ccall((:fq_default_is_zero, libflint), Bool,
                     (Ref{FqFieldElem}, Ref{FqField}), a, a.parent)

isone(a::FqFieldElem) = ccall((:fq_default_is_one, libflint), Bool,
                    (Ref{FqFieldElem}, Ref{FqField}), a, a.parent)

_is_gen(a::FqFieldElem) = a == _gen(parent(a))

is_unit(a::FqFieldElem) = ccall((:fq_default_is_invertible, libflint), Bool,
                     (Ref{FqFieldElem}, Ref{FqField}), a, a.parent)

function characteristic(a::FqField)
   d = ZZRingElem()
   ccall((:fq_default_ctx_prime, libflint), Nothing,
         (Ref{ZZRingElem}, Ref{FqField}), d, a)
   return d
end

function order(a::FqField)
   d = ZZRingElem()
   ccall((:fq_default_ctx_order, libflint), Nothing,
         (Ref{ZZRingElem}, Ref{FqField}), d, a)
   return d
end

function _degree(a::FqField)
    return ccall((:fq_default_ctx_degree, libflint), Int, (Ref{FqField},), a)
end

function deepcopy_internal(d::FqFieldElem, dict::IdDict)
   z = FqFieldElem(parent(d), d)
   return z
end

###############################################################################
#
#   Lifts and conversions
#
###############################################################################

function lift(R::ZZRing, x::FqFieldElem)
  z = R()
  ok = ccall((:fq_default_get_fmpz, libflint), Cint,
             (Ref{ZZRingElem}, Ref{FqFieldElem}, Ref{FqField}),
             z, x, parent(x))
  ok == 0 && error("cannot lift")
  return z
end

function lift(R::ZZPolyRing, x::FqFieldElem)
   p = R()
   !parent(x).isstandard && error("Cannot lift to integer polynomial")
   ccall((:fq_default_get_fmpz_poly, libflint), Nothing,
         (Ref{ZZPolyRingElem}, Ref{FqFieldElem}, Ref{FqField}),
          p, x, parent(x))
   return p
end

function (R::zzModPolyRing)(x::FqFieldElem)
   p = R()
   ccall((:fq_default_get_nmod_poly, libflint), Nothing,
         (Ref{zzModPolyRingElem}, Ref{FqFieldElem}, Ref{FqField}),
          p, x, parent(x))
   return p
end

function (R::fpPolyRing)(x::FqFieldElem)
   p = R()
   ccall((:fq_default_get_nmod_poly, libflint), Nothing,
         (Ref{fpPolyRingElem}, Ref{FqFieldElem}, Ref{FqField}),
          p, x, parent(x))
   return p
end

function (R::ZZModPolyRing)(x::FqFieldElem)
   p = R()
   ccall((:fq_default_get_fmpz_mod_poly, libflint), Nothing,
         (Ref{ZZModPolyRingElem}, Ref{FqFieldElem}, Ref{FqField}),
          p, x, parent(x))
   return p
end

function (R::FpPolyRing)(x::FqFieldElem)
   p = R()
   ccall((:fq_default_get_fmpz_mod_poly, libflint), Nothing,
         (Ref{FpPolyRingElem}, Ref{FqFieldElem}, Ref{FqField}),
          p, x, parent(x))
   return p
end

# with FqPolyRepFieldElem
function _unchecked_coerce(a::FqPolyRepField, b::FqFieldElem)
    x = ZZPolyRingElem()
    ccall((:fq_default_get_fmpz_poly, libflint), Nothing,
         (Ref{ZZPolyRingElem}, Ref{FqFieldElem}, Ref{FqField}),
          x, b, parent(b))
    return FqPolyRepFieldElem(a, x)
end

function _unchecked_coerce(a::FqField, b::FqPolyRepFieldElem)
    x = ZZPolyRingElem()
    ccall((:fq_get_fmpz_poly, libflint), Nothing,
         (Ref{ZZPolyRingElem}, Ref{FqPolyRepFieldElem}, Ref{FqPolyRepField}),
          x, b, parent(b))
    return FqFieldElem(a, x)
end

# with zzModRingElem
function _unchecked_coerce(a::fpField, b::FqFieldElem)
    iszero(b) && return zero(a)
    return a(lift(ZZ, b))
end

function _unchecked_coerce(a::FqField, b::fpFieldElem)
    return FqFieldElem(a, lift(b))
end

# with ZZModRingElem
function _unchecked_coerce(a::FpField, b::FqFieldElem)
    iszero(b) && return zero(a)
    return a(lift(ZZ, b))
end

function _unchecked_coerce(a::FqField, b::FpFieldElem)
    return FqFieldElem(a, lift(b))
end

# with fqPolyRepFieldElem
function _unchecked_coerce(a::fqPolyRepField, b::FqFieldElem)
    x = zzModPolyRingElem(UInt(characteristic(a)))
    ccall((:fq_default_get_nmod_poly, libflint), Nothing,
         (Ref{zzModPolyRingElem}, Ref{FqFieldElem}, Ref{FqField}),
          x, b, parent(b))
    y = a()
    ccall((:fq_nmod_set_nmod_poly, libflint), Nothing,
         (Ref{fqPolyRepFieldElem}, Ref{zzModPolyRingElem}, Ref{fqPolyRepField}),
          y, x, a)
    return y
end

function _unchecked_coerce(a::FqField, b::fqPolyRepFieldElem)
    x = zzModPolyRingElem(UInt(characteristic(parent(b))))
    ccall((:fq_nmod_get_nmod_poly, libflint), Nothing,
         (Ref{zzModPolyRingElem}, Ref{fqPolyRepFieldElem}, Ref{fqPolyRepField}),
          x, b, parent(b))
    return FqFieldElem(a, x)
end

################################################################################
#
#  Convenience conversion maps
#
################################################################################

_FQ_DEFAULT_FQ_ZECH   = 1
_FQ_DEFAULT_FQ_NMOD   = 2
_FQ_DEFAULT_FQ        = 3
_FQ_DEFAULT_NMOD      = 4
_FQ_DEFAULT_FMPZ_NMOD = 5

mutable struct CanonicalFqDefaultMap{T} <: Map{FqField, T, SetMap, CanonicalFqDefaultMap}
  D::FqField
  C::T
end

domain(f::CanonicalFqDefaultMap) = f.D

codomain(f::CanonicalFqDefaultMap) = f.C

mutable struct CanonicalFqDefaultMapInverse{T} <: Map{T, FqField, SetMap, CanonicalFqDefaultMapInverse}
  D::T
  C::FqField
end

domain(f::CanonicalFqDefaultMapInverse) = f.D

codomain(f::CanonicalFqDefaultMapInverse) = f.C

function _fq_default_ctx_type(F::FqField)
  return ccall((:fq_default_ctx_type, libflint), Cint, (Ref{FqField},),  F)
end

function _get_raw_type(::Type{fqPolyRepField}, F::FqField)
  @assert _fq_default_ctx_type(F) == 2
  Rx, _ = polynomial_ring(GF(UInt(characteristic(F))), "x", cached = false)
  m = map_coefficients(x -> _coeff(x, 0), defining_polynomial(F), parent = Rx)
  return fqPolyRepField(m, :$, false)
end

function _get_raw_type(::Type{FqPolyRepField}, F::FqField)
  @assert _fq_default_ctx_type(F) == 3
  Rx, _ = polynomial_ring(GF(characteristic(F)), "x", cached = false)
  m = map_coefficients(x -> _coeff(x, 0), defining_polynomial(F), parent = Rx)
  return FqPolyRepField(m, :$, false)
end

function canonical_raw_type(::Type{T}, F::FqField) where {T}
  C = _get_raw_type(T, F)
  return CanonicalFqDefaultMap{T}(F, C)
end

function _get_raw_type(::Type{fpField}, F::FqField)
  @assert _fq_default_ctx_type(F) == 4
  return GF(UInt(order(F)))
end

function _get_raw_type(::Type{FpField}, F::FqField)
  @assert _fq_default_ctx_type(F) == 5
  return GF(order(F))
end

# image/preimage

function image(f::CanonicalFqDefaultMap, x::FqFieldElem)
  @assert parent(x) === f.D
  return _unchecked_coerce(f.C, x)
end

function preimage(f::CanonicalFqDefaultMap, x)
  @assert parent(x) === f.C
  return _unchecked_coerce(f.D, x)
end

(f::CanonicalFqDefaultMap)(x::FqFieldElem) = image(f, x)

# inv

function inv(f::CanonicalFqDefaultMap{T}) where {T}
  return CanonicalFqDefaultMapInverse{T}(f.C, f.D)
end

# image/preimage for inv

function image(f::CanonicalFqDefaultMapInverse, x)
  @assert parent(x) === f.D
  _unchecked_coerce(f.C, x)
end

function preimage(f::CanonicalFqDefaultMapInverse, x::FqFieldElem)
  @assert parent(x) === f.C
  _unchecked_coerce(f.D, x)
end

(f::CanonicalFqDefaultMapInverse)(x) = image(f, x)

###############################################################################
#
#   Canonicalisation
#
###############################################################################

canonical_unit(x::FqFieldElem) = x

###############################################################################
#
#   AbstractString I/O
#
###############################################################################

function expressify(a::FqFieldElem; context = nothing)
   x = a.parent.var
   d = degree(a.parent)

   sum = Expr(:call, :+)
   for k in (d - 1):-1:0
        c = is_absolute(parent(a)) ? _coeff(a, k) : coeff(a, k)
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

show(io::IO, a::FqFieldElem) = print(io, AbstractAlgebra.obj_to_string(a, context = io))

function show(io::IO, a::FqField)
  io = pretty(io)
  if get(io, :supercompact, false)
    print(io, LowercaseOff(), "GF($(order(base_field(a)))", degree(a) > 1 ? "^$(degree(a))" : "", ")")
  else
    if is_absolute(a)
      # nested printing allowed, preferably supercompact
      print(io, "Finite field of degree ", degree(a), " over ")
      print(IOContext(io, :supercompact => true), base_field(a))
    else
      # nested printing allowed, preferably supercompact
      print(io, "Relative finite field of degree ", degree(a), " over ")
      print(IOContext(io, :supercompact => true), base_field(a))
    end
  end
end

###############################################################################
#
#   Unary operations
#
###############################################################################

function -(x::FqFieldElem)
   z = parent(x)()
   ccall((:fq_default_neg, libflint), Nothing,
         (Ref{FqFieldElem}, Ref{FqFieldElem}, Ref{FqField}), z, x, x.parent)
   return z
end

###############################################################################
#
#   Binary operations
#
###############################################################################

function +(x::FqFieldElem, y::FqFieldElem)
   if parent(x) === parent(y)
     z = parent(y)()
     ccall((:fq_default_add, libflint), Nothing,
          (Ref{FqFieldElem}, Ref{FqFieldElem}, Ref{FqFieldElem}, Ref{FqField}), z, x, y, y.parent)
     return z
   end
   return +(_promote(x, y)...)
end

function -(x::FqFieldElem, y::FqFieldElem)
   if parent(x) === parent(y)
     z = parent(y)()
     ccall((:fq_default_sub, libflint), Nothing,
          (Ref{FqFieldElem}, Ref{FqFieldElem}, Ref{FqFieldElem}, Ref{FqField}), z, x, y, y.parent)
     return z
   end
   return -(_promote(x, y)...)
end

function *(x::FqFieldElem, y::FqFieldElem)
   if parent(x) === parent(y)
     z = parent(y)()
     ccall((:fq_default_mul, libflint), Nothing,
          (Ref{FqFieldElem}, Ref{FqFieldElem}, Ref{FqFieldElem}, Ref{FqField}), z, x, y, y.parent)
     return z
   end
   return *(_promote(x, y)...)
end

###############################################################################
#
#   Ad hoc binary operators
#
###############################################################################

function *(x::Int, y::FqFieldElem)
   z = parent(y)()
   ccall((:fq_default_mul_si, libflint), Nothing,
         (Ref{FqFieldElem}, Ref{FqFieldElem}, Int, Ref{FqField}), z, y, x, y.parent)
   return z
end

*(x::Integer, y::FqFieldElem) = ZZRingElem(x)*y

*(x::FqFieldElem, y::Integer) = y*x

function *(x::ZZRingElem, y::FqFieldElem)
   z = parent(y)()
   ccall((:fq_default_mul_fmpz, libflint), Nothing,
         (Ref{FqFieldElem}, Ref{FqFieldElem}, Ref{ZZRingElem}, Ref{FqField}),
                                            z, y, x, y.parent)
   return z
end

*(x::FqFieldElem, y::ZZRingElem) = y*x

+(x::FqFieldElem, y::Integer) = x + parent(x)(y)

+(x::Integer, y::FqFieldElem) = y + x

+(x::FqFieldElem, y::ZZRingElem) = x + parent(x)(y)

+(x::ZZRingElem, y::FqFieldElem) = y + x

-(x::FqFieldElem, y::Integer) = x - parent(x)(y)

-(x::Integer, y::FqFieldElem) = parent(y)(x) - y

-(x::FqFieldElem, y::ZZRingElem) = x - parent(x)(y)

-(x::ZZRingElem, y::FqFieldElem) = parent(y)(x) - y

###############################################################################
#
#   Powering
#
###############################################################################

function ^(x::FqFieldElem, y::Int)
   if y < 0
      x = inv(x)
      y = -y
   end
   z = parent(x)()
   ccall((:fq_default_pow_ui, libflint), Nothing,
         (Ref{FqFieldElem}, Ref{FqFieldElem}, Int, Ref{FqField}), z, x, y, x.parent)
   return z
end

function ^(x::FqFieldElem, y::ZZRingElem)
   if y < 0
      x = inv(x)
      y = -y
   end
   z = parent(x)()
   ccall((:fq_default_pow, libflint), Nothing,
         (Ref{FqFieldElem}, Ref{FqFieldElem}, Ref{ZZRingElem}, Ref{FqField}),
                                            z, x, y, x.parent)
   return z
end

###############################################################################
#
#   Comparison
#
###############################################################################

function ==(x::FqFieldElem, y::FqFieldElem)
   check_parent(x, y)
   ccall((:fq_default_equal, libflint), Bool,
         (Ref{FqFieldElem}, Ref{FqFieldElem}, Ref{FqField}), x, y, y.parent)
end

###############################################################################
#
#   Ad hoc comparison
#
###############################################################################

==(x::FqFieldElem, y::Integer) = x == parent(x)(y)

==(x::FqFieldElem, y::ZZRingElem) = x == parent(x)(y)

==(x::Integer, y::FqFieldElem) = parent(y)(x) == y

==(x::ZZRingElem, y::FqFieldElem) = parent(y)(x) == y

###############################################################################
#
#   Exact division
#
###############################################################################

function divexact(x::FqFieldElem, y::FqFieldElem; check::Bool=true)
   if parent(x) === parent(y)
     iszero(y) && throw(DivideError())
     z = parent(y)()
     ccall((:fq_default_div, libflint), Nothing,
          (Ref{FqFieldElem}, Ref{FqFieldElem}, Ref{FqFieldElem}, Ref{FqField}), z, x, y, y.parent)
     return z
   end
   return divexact(_promote(x, y)...)
end

function divides(a::FqFieldElem, b::FqFieldElem)
   if parent(a) === parent(b)
     if iszero(a)
        return true, zero(parent(a))
     end
     if iszero(b)
        return false, zero(parent(a))
     end
     return true, divexact(a, b)
   end
   return divides(_promote(a, b)...)
end

###############################################################################
#
#   Ad hoc exact division
#
###############################################################################

divexact(x::FqFieldElem, y::Integer; check::Bool=true) = divexact(x, parent(x)(y); check=check)

divexact(x::FqFieldElem, y::ZZRingElem; check::Bool=true) = divexact(x, parent(x)(y); check=check)

divexact(x::Integer, y::FqFieldElem; check::Bool=true) = divexact(parent(y)(x), y; check=check)

divexact(x::ZZRingElem, y::FqFieldElem; check::Bool=true) = divexact(parent(y)(x), y; check=check)

###############################################################################
#
#   Inversion
#
###############################################################################

function inv(x::FqFieldElem)
   iszero(x) && throw(DivideError())
   z = parent(x)()
   ccall((:fq_default_inv, libflint), Nothing,
         (Ref{FqFieldElem}, Ref{FqFieldElem}, Ref{FqField}), z, x, x.parent)
   return z
end

###############################################################################
#
#   Special functions
#
###############################################################################

function sqrt(x::FqFieldElem)
   z = parent(x)()
   res = Bool(ccall((:fq_default_sqrt, libflint), Cint,
                    (Ref{FqFieldElem}, Ref{FqFieldElem}, Ref{FqField}),
                    z, x, x.parent))
   res || error("Not a square")
   return z
end

function is_square(x::FqFieldElem)
   return Bool(ccall((:fq_default_is_square, libflint), Cint,
                     (Ref{FqFieldElem}, Ref{FqField}),
                     x, x.parent))
end

function is_square_with_sqrt(x::FqFieldElem)
   z = parent(x)()
   flag = ccall((:fq_default_sqrt, libflint), Cint,
                (Ref{FqFieldElem}, Ref{FqFieldElem}, Ref{FqField}),
                z, x, x.parent)
   return (Bool(flag), z)
end

@doc raw"""
    pth_root(x::FqFieldElem)

Return the $p$-th root of $x$ in the finite field of characteristic $p$. This
is the inverse operation to the absolute Frobenius map.
"""
function pth_root(x::FqFieldElem)
   z = parent(x)()
   ccall((:fq_default_pth_root, libflint), Nothing,
         (Ref{FqFieldElem}, Ref{FqFieldElem}, Ref{FqField}), z, x, x.parent)
   return z
end

function _tr(x::FqFieldElem)
   z = ZZRingElem()
   ccall((:fq_default_trace, libflint), Nothing,
         (Ref{ZZRingElem}, Ref{FqFieldElem}, Ref{FqField}), z, x, x.parent)
   return z
end

function _norm(x::FqFieldElem)
   z = ZZRingElem()
   ccall((:fq_default_norm, libflint), Nothing,
         (Ref{ZZRingElem}, Ref{FqFieldElem}, Ref{FqField}), z, x, x.parent)
   return z
end

function _frobenius(x::FqFieldElem, n = 1)
   z = parent(x)()
   ccall((:fq_default_frobenius, libflint), Nothing,
         (Ref{FqFieldElem}, Ref{FqFieldElem}, Int, Ref{FqField}), z, x, n, x.parent)
   return z
end

###############################################################################
#
#   Unsafe functions
#
###############################################################################

function zero!(z::FqFieldElem)
   ccall((:fq_default_zero, libflint), Nothing,
        (Ref{FqFieldElem}, Ref{FqField}), z, z.parent)
   z.poly = nothing
   return z
end

function mul!(z::FqFieldElem, x::FqFieldElem, y::FqFieldElem)
   ccall((:fq_default_mul, libflint), Nothing,
        (Ref{FqFieldElem}, Ref{FqFieldElem}, Ref{FqFieldElem}, Ref{FqField}), z, x, y, y.parent)
   z.poly = nothing
   return z
end

function addeq!(z::FqFieldElem, x::FqFieldElem)
   ccall((:fq_default_add, libflint), Nothing,
        (Ref{FqFieldElem}, Ref{FqFieldElem}, Ref{FqFieldElem}, Ref{FqField}), z, z, x, x.parent)
   z.poly = nothing
   return z
end

function add!(z::FqFieldElem, x::FqFieldElem, y::FqFieldElem)
   ccall((:fq_default_add, libflint), Nothing,
        (Ref{FqFieldElem}, Ref{FqFieldElem}, Ref{FqFieldElem}, Ref{FqField}), z, x, y, x.parent)
   z.poly = nothing
   return z
end

###############################################################################
#
#   Random functions
#
###############################################################################

# define rand(::FqField)

Random.Sampler(::Type{RNG}, R::FqField, n::Random.Repetition) where {RNG<:AbstractRNG} =
   Random.SamplerSimple(R, Random.Sampler(RNG, BigInt(0):BigInt(order(R))-1, n))

function rand(rng::AbstractRNG, R::Random.SamplerSimple{FqField})
   F = R[]
   x = _gen(F)
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

Random.gentype(::Type{FqField}) = elem_type(FqField)

# define rand(make(::FqField, arr)), where arr is any abstract array with integer or ZZRingElem entries

RandomExtensions.maketype(R::FqField, _) = elem_type(R)

rand(rng::AbstractRNG, sp::SamplerTrivial{<:Make2{FqFieldElem,FqField,<:AbstractArray{<:IntegerUnion}}}) =
   sp[][1](rand(rng, sp[][2]))

# define rand(::FqField, arr), where arr is any abstract array with integer or ZZRingElem entries

rand(r::Random.AbstractRNG, R::FqField, b::AbstractArray) = rand(r, make(R, b))

rand(R::FqField, b::AbstractArray) = rand(Random.GLOBAL_RNG, R, b)

###############################################################################
#
#   Modulus
#
###############################################################################

function modulus(R::FpPolyRing, k::FqField)
    Q = R()
    ccall((:fq_default_ctx_modulus, libflint), Nothing,
          (Ref{FpPolyRingElem}, Ref{FqField}),
          Q, k)
    return Q
end

function modulus(k::FqField, var::String="T")
    p = characteristic(k)
    Q = polynomial(GF(p), [], var, cached = false)
    ccall((:fq_default_ctx_modulus, libflint), Nothing,
          (Ref{FpPolyRingElem}, Ref{FqField}),
          Q, k)
    return Q
end

###############################################################################
#
#   Promotions
#
###############################################################################

promote_rule(::Type{FqFieldElem}, ::Type{T}) where {T <: Integer} = FqFieldElem

promote_rule(::Type{FqFieldElem}, ::Type{ZZRingElem}) = FqFieldElem

###############################################################################
#
#   Parent object call overload
#
###############################################################################

function (a::FqField)()
   z = FqFieldElem(a)
   return z
end

(a::FqField)(b::Integer) = a(ZZRingElem(b))

function (a::FqField)(b::Int)
   z = FqFieldElem(a, b)
   return z
end

function (a::FqField)(b::ZZRingElem)
   z = FqFieldElem(a, b)
   return z
end

function (a::FqField)(b::Rational{<:Integer})
   d = a(denominator(b))
   is_zero(d) && error("Denominator not invertible")
   return a(numerator(b))/d
end

function (a::FqField)(b::QQFieldElem)
   d = a(denominator(b))
   is_zero(d) && error("Denominator not invertible")
   return a(numerator(b))/d
end

function (a::FqField)(b::ZZPolyRingElem)
   if a.isstandard
     z = FqFieldElem(a, b)
   else
     return a.forwardmap(parent(defining_polynomial(a))(b))
   end
   return z
end

function (a::FqField)(b::Union{zzModPolyRingElem, fpPolyRingElem})
   characteristic(parent(b)) != characteristic(a) &&
                        error("Incompatible characteristic")
   z = FqFieldElem(a, b)
   return z
end

function (a::FqField)(b::Union{ZZModPolyRingElem, FpPolyRingElem})
   characteristic(parent(b)) != characteristic(a) &&
                        error("Incompatible characteristic")
   z = FqFieldElem(a, b)
   return z
end

###############################################################################
#
#   FlintFiniteField constructor
#
###############################################################################

@doc raw"""
    NGFiniteField(char::IntegerUnion, deg::Int, s::VarName; cached = true, check = true)

Returns a tuple $S, x$ consisting of a finite field $S$ of degree `deg` with
characteristic `char` and generator $x$ for the finite field of the given
characteristic and degree. The string $s$ is used to designate how the finite
field generator will be
printed.
"""
function NGFiniteField(char::IntegerUnion, deg::Int, s::VarName = :o; cached = true, check::Bool = true)
   check && !is_prime(char) && error("Characteristic must be prime")
   _char = ZZRingElem(char)
   S = Symbol(s)
   parent_obj = FqField(_char, deg, S, cached)

   return parent_obj, _gen(parent_obj)
end

# @doc raw"""
#     NGFiniteField(pol::Union{ZZModPolyRingElem, FpPolyRingElem}, s::VarName; cached = true, check = true)
# 
# Returns a tuple $S, x$ consisting of a finite field parent object $S$ and
# generator $x$ for the finite field over $F_p$ defined by the given
# polynomial, i.e. $\mathbb{F}_p[t]/(pol)$. The characteristic is specified by
# the modulus of `pol`. The polynomial is required to be irreducible, but this
# is not checked. The base ring of the polynomial is required to be a field, which
# is checked by default. Use `check = false` to disable the check.
# The string $s$ is used to designate how the finite field
# generator will be printed. The generator will not be multiplicative in
# general.
# """
# function NGFiniteField(pol::Union{ZZModPolyRingElem, FpPolyRingElem, zzModPolyRingElem, fpPolyRingElem},
#                           s::VarName; cached = true, check::Bool=true)
#    S = Symbol(s)
#    parent_obj = FqField(pol, S, cached, check=check)
# 
#    return parent_obj, _gen(parent_obj)
# end
# 
# 

# The following code is used in the intersection code
function FlintFiniteField(F::FqField, deg::Int, s::VarName = :o; cached = true)
    return FqField(characteristic(F), deg, Symbol(s), cached)
end

