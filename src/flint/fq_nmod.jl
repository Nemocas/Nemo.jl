###############################################################################
#
#   fqPolyRepFieldElem.jl : FLINT finite fields
#
###############################################################################

###############################################################################
#
#   Type and parent object methods
#
###############################################################################

parent_type(::Type{fqPolyRepFieldElem}) = fqPolyRepField

elem_type(::Type{fqPolyRepField}) = fqPolyRepFieldElem

base_ring_type(::Type{fqPolyRepField}) = Union{}

parent(a::fqPolyRepFieldElem) = a.parent

is_domain_type(::Type{fqPolyRepFieldElem}) = true

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
  return @ccall libflint.nmod_poly_get_coeff_ui(x::Ref{fqPolyRepFieldElem}, n::Int)::UInt
end

function coeffs_raw(x::fqPolyRepFieldElem)
  GC.@preserve x begin
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

function setcoeff!(x::fqPolyRepFieldElem, n::Int, u::UInt)
  @ccall libflint.nmod_poly_set_coeff_ui(x::Ref{fqPolyRepFieldElem}, n::Int, u::UInt)::Nothing
  return x
end

# set the i-th coeff of x to c, internal use only
function setindex_raw!(x::fqPolyRepFieldElem, c::UInt, i::Int)
  len = degree(parent(x))
  i > len - 1 && error("Index out of range")
  setcoeff!(x, i, c)
  return x
end

zero(a::fqPolyRepField) = zero!(a())

one(a::fqPolyRepField) = one!(a())

iszero(a::fqPolyRepFieldElem) = @ccall libflint.fq_nmod_is_zero(a::Ref{fqPolyRepFieldElem}, a.parent::Ref{fqPolyRepField})::Bool

isone(a::fqPolyRepFieldElem) = @ccall libflint.fq_nmod_is_one(a::Ref{fqPolyRepFieldElem}, a.parent::Ref{fqPolyRepField})::Bool

function gen(a::fqPolyRepField)
  d = a()
  @ccall libflint.fq_nmod_gen(d::Ref{fqPolyRepFieldElem}, a::Ref{fqPolyRepField})::Nothing
  return d
end

is_gen(a::fqPolyRepFieldElem) = a == gen(parent(a)) # there is no is_gen in flint

function characteristic(a::fqPolyRepField)
  return ZZ(a.n)
end

function order(a::fqPolyRepField)
  d = ZZRingElem()
  @ccall libflint.fq_nmod_ctx_order(d::Ref{ZZRingElem}, a::Ref{fqPolyRepField})::Nothing
  return d
end

function degree(a::fqPolyRepField)
  return @ccall libflint.fq_nmod_ctx_degree(a::Ref{fqPolyRepField})::Int
end

function deepcopy_internal(d::fqPolyRepFieldElem, dict::IdDict)
  z = fqPolyRepFieldElem(parent(d), d)
  return z
end

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
  @show_name(io, a)
  @show_special(io, a)
  if is_terse(io)
    io = pretty(io)
    print(io, LowercaseOff(), "GF($(characteristic(a))", degree(a)>1 ? "^$(degree(a))" : "", ")")
  else
    print(io, "Finite field of degree ", degree(a), " over ")
    print(io, "GF($(characteristic(a)))")
  end
end

function pretty_lt(x::fqPolyRepFieldElem, y::fqPolyRepFieldElem)
  vx = UInt[]
  found = false
  for i in degree(parent(x)):-1:0
    if !found && is_zero(coeff(x, i))
      continue
    end
    found = true
    pushfirst!(vx, coeff(x, i))
  end
  vy = UInt[]
  found = false
  for i in degree(parent(y)):-1:0
    if !found && is_zero(coeff(y, i))
      continue
    end
    found = true
    pushfirst!(vy, coeff(y, i))
  end
  return pretty_lt_lex(vx, vy)
end
pretty_eq(x::fqPolyRepFieldElem, y::fqPolyRepFieldElem) = (x == y)


###############################################################################
#
#   Unary operations
#
###############################################################################

-(x::fqPolyRepFieldElem) = neg!(parent(x)(), x)

###############################################################################
#
#   Binary operations
#
###############################################################################

function +(x::fqPolyRepFieldElem, y::fqPolyRepFieldElem)
  check_parent(x, y)
  z = parent(y)()
  @ccall libflint.fq_nmod_add(z::Ref{fqPolyRepFieldElem}, x::Ref{fqPolyRepFieldElem}, y::Ref{fqPolyRepFieldElem}, y.parent::Ref{fqPolyRepField})::Nothing
  return z
end

function -(x::fqPolyRepFieldElem, y::fqPolyRepFieldElem)
  check_parent(x, y)
  z = parent(y)()
  @ccall libflint.fq_nmod_sub(z::Ref{fqPolyRepFieldElem}, x::Ref{fqPolyRepFieldElem}, y::Ref{fqPolyRepFieldElem}, y.parent::Ref{fqPolyRepField})::Nothing
  return z
end

function *(x::fqPolyRepFieldElem, y::fqPolyRepFieldElem)
  check_parent(x, y)
  z = parent(y)()
  @ccall libflint.fq_nmod_mul(z::Ref{fqPolyRepFieldElem}, x::Ref{fqPolyRepFieldElem}, y::Ref{fqPolyRepFieldElem}, y.parent::Ref{fqPolyRepField})::Nothing
  return z
end

###############################################################################
#
#   Ad hoc binary operators
#
###############################################################################

function *(x::Int, y::fqPolyRepFieldElem)
  z = parent(y)()
  @ccall libflint.fq_nmod_mul_si(z::Ref{fqPolyRepFieldElem}, y::Ref{fqPolyRepFieldElem}, x::Int, y.parent::Ref{fqPolyRepField})::Nothing
  return z
end

*(x::fqPolyRepFieldElem, y::Int) = y*x

*(x::Integer, y::fqPolyRepFieldElem) = ZZRingElem(x)*y

*(x::fqPolyRepFieldElem, y::Integer) = y*x

function *(x::ZZRingElem, y::fqPolyRepFieldElem)
  z = parent(y)()
  @ccall libflint.fq_nmod_mul_fmpz(z::Ref{fqPolyRepFieldElem}, y::Ref{fqPolyRepFieldElem}, x::Ref{ZZRingElem}, y.parent::Ref{fqPolyRepField})::Nothing
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
  @ccall libflint.fq_nmod_pow_ui(z::Ref{fqPolyRepFieldElem}, x::Ref{fqPolyRepFieldElem}, y::Int, x.parent::Ref{fqPolyRepField})::Nothing
  return z
end

function ^(x::fqPolyRepFieldElem, y::ZZRingElem)
  if y < 0
    x = inv(x)
    y = -y
  end
  z = parent(x)()
  @ccall libflint.fq_nmod_pow(z::Ref{fqPolyRepFieldElem}, x::Ref{fqPolyRepFieldElem}, y::Ref{ZZRingElem}, x.parent::Ref{fqPolyRepField})::Nothing
  return z
end

###############################################################################
#
#   Comparison
#
###############################################################################

function ==(x::fqPolyRepFieldElem, y::fqPolyRepFieldElem)
  check_parent(x, y)
  @ccall libflint.fq_nmod_equal(x::Ref{fqPolyRepFieldElem}, y::Ref{fqPolyRepFieldElem}, y.parent::Ref{fqPolyRepField})::Bool
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
  @ccall libflint.fq_nmod_inv(z::Ref{fqPolyRepFieldElem}, x::Ref{fqPolyRepFieldElem}, x.parent::Ref{fqPolyRepField})::Nothing
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
  @ccall libflint.fq_nmod_div(z::Ref{fqPolyRepFieldElem}, x::Ref{fqPolyRepFieldElem}, y::Ref{fqPolyRepFieldElem}, y.parent::Ref{fqPolyRepField})::Nothing
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
  res = Bool(@ccall libflint.fq_nmod_sqrt(z::Ref{fqPolyRepFieldElem}, x::Ref{fqPolyRepFieldElem}, x.parent::Ref{fqPolyRepField})::Cint)
  check && !res && error("Not a square")
  return z
end

function is_square(x::fqPolyRepFieldElem)
  return Bool(@ccall libflint.fq_nmod_is_square(x::Ref{fqPolyRepFieldElem}, x.parent::Ref{fqPolyRepField})::Cint)
end

function is_square_with_sqrt(x::fqPolyRepFieldElem)
  z = parent(x)()
  flag = @ccall libflint.fq_nmod_sqrt(z::Ref{fqPolyRepFieldElem}, x::Ref{fqPolyRepFieldElem}, x.parent::Ref{fqPolyRepField})::Cint
  return (Bool(flag), z)
end

function pth_root(x::fqPolyRepFieldElem)
  z = parent(x)()
  @ccall libflint.fq_nmod_pth_root(z::Ref{fqPolyRepFieldElem}, x::Ref{fqPolyRepFieldElem}, x.parent::Ref{fqPolyRepField})::Nothing
  return z
end

function tr(x::fqPolyRepFieldElem)
  z = ZZRingElem()
  @ccall libflint.fq_nmod_trace(z::Ref{ZZRingElem}, x::Ref{fqPolyRepFieldElem}, x.parent::Ref{fqPolyRepField})::Nothing
  return parent(x)(z)
end

function norm(x::fqPolyRepFieldElem)
  z = ZZRingElem()
  @ccall libflint.fq_nmod_norm(z::Ref{ZZRingElem}, x::Ref{fqPolyRepFieldElem}, x.parent::Ref{fqPolyRepField})::Nothing
  return parent(x)(z)
end

function frobenius(x::fqPolyRepFieldElem, n = 1)
  z = parent(x)()
  return frobenius!(z, x, n)
end

function frobenius!(a::fqPolyRepFieldElem, b::fqPolyRepFieldElem, i::Int = 1)
  @ccall libflint.fq_nmod_frobenius(a::Ref{fqPolyRepFieldElem}, b::Ref{fqPolyRepFieldElem}, i::Int, parent(a)::Ref{fqPolyRepField})::Nothing
  return a
end

function frobenius_matrix(F::fqPolyRepField, n::Int=1)
  a = frobenius(gen(F), n)
  k = Native.GF(Int(characteristic(F)))
  m = zero_matrix(k, degree(F), degree(F))
  @ccall libflint.fq_nmod_embed_composition_matrix_sub(m::Ref{fpMatrix}, a::Ref{fqPolyRepFieldElem}, F::Ref{fqPolyRepField}, degree(F)::Int)::Nothing
  @ccall libflint.nmod_mat_transpose(m::Ref{fpMatrix}, m::Ref{fpMatrix})::Nothing
  return m
end

###############################################################################
#
#   Lift
#
###############################################################################

function lift(R::fpPolyRing, x::fqPolyRepFieldElem)
  c = R()
  @ccall libflint.fq_nmod_get_nmod_poly(c::Ref{fpPolyRingElem}, x::Ref{fqPolyRepFieldElem}, parent(x)::Ref{fqPolyRepField})::Nothing
  return c
end

###############################################################################
#
#   Unsafe functions
#
###############################################################################

function zero!(z::fqPolyRepFieldElem)
  @ccall libflint.fq_nmod_zero(z::Ref{fqPolyRepFieldElem}, z.parent::Ref{fqPolyRepField})::Nothing
  return z
end

function one!(z::fqPolyRepFieldElem)
  @ccall libflint.fq_nmod_one(z::Ref{fqPolyRepFieldElem}, z.parent::Ref{fqPolyRepField})::Nothing
  return z
end

function neg!(z::fqPolyRepFieldElem, a::fqPolyRepFieldElem)
  @ccall libflint.fq_nmod_neg(z::Ref{fqPolyRepFieldElem}, a::Ref{fqPolyRepFieldElem}, a.parent::Ref{fqPolyRepField})::Nothing
  return z
end

function set!(z::fqPolyRepFieldElem, x::fqPolyRepFieldElemOrPtr)
  @ccall libflint.fq_nmod_set(z::Ref{fqPolyRepFieldElem}, x::Ref{fqPolyRepFieldElem}, parent(z)::Ref{fqPolyRepField})::Nothing
end

function set!(z::fqPolyRepFieldElem, x::ZZRingElemOrPtr)
  @ccall libflint.fq_nmod_set_fmpz(z::Ref{fqPolyRepFieldElem}, x::Ref{ZZRingElem}, parent(z)::Ref{fqPolyRepField})::Nothing
end

function set!(z::fqPolyRepFieldElem, x::Int)
  @ccall libflint.fq_nmod_set_si(z::Ref{fqPolyRepFieldElem}, x::Int, parent(z)::Ref{fqPolyRepField})::Nothing
end

function set!(z::fqPolyRepFieldElem, x::UInt)
  @ccall libflint.fq_nmod_set_ui(z::Ref{fqPolyRepFieldElem}, x::UInt, parent(z)::Ref{fqPolyRepField})::Nothing
end

function set!(z::fqPolyRepFieldElem, x::fpPolyRingElemOrPtr)
  @ccall libflint.fq_nmod_set_nmod_poly(z::Ref{fqPolyRepFieldElem}, x::Ref{fpPolyRingElem}, parent(z)::Ref{fqPolyRepField})::Nothing
end

function mul!(z::fqPolyRepFieldElem, x::fqPolyRepFieldElem, y::fqPolyRepFieldElem)
  @ccall libflint.fq_nmod_mul(z::Ref{fqPolyRepFieldElem}, x::Ref{fqPolyRepFieldElem}, y::Ref{fqPolyRepFieldElem}, y.parent::Ref{fqPolyRepField})::Nothing
  return z
end

function add!(z::fqPolyRepFieldElem, x::fqPolyRepFieldElem, y::fqPolyRepFieldElem)
  @ccall libflint.fq_nmod_add(z::Ref{fqPolyRepFieldElem}, x::Ref{fqPolyRepFieldElem}, y::Ref{fqPolyRepFieldElem}, x.parent::Ref{fqPolyRepField})::Nothing
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

rand(R::fqPolyRepField, b::AbstractArray) = rand(Random.default_rng(), R, b)

###############################################################################
#
#   Iteration
#
###############################################################################

Base.iterate(F::Union{fqPolyRepField,FqPolyRepField}) =
zero(F), zeros(F isa fqPolyRepField ? UInt : ZZRingElem, degree(F))

function Base.iterate(F::Union{fqPolyRepField,FqPolyRepField}, coeffs::Vector)
  deg = length(coeffs)
  char = characteristic(F)

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
      @ccall libflint.nmod_poly_set_coeff_ui(elt::Ref{fqPolyRepFieldElem}, (d - 1)::Int, coeffs[d]::UInt)::Nothing
    else
      @ccall libflint.fmpz_poly_set_coeff_fmpz(elt::Ref{FqPolyRepFieldElem}, (d - 1)::Int, coeffs[d]::Ref{ZZRingElem})::Nothing
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
    modulus(k::fqPolyRepField, var::VarName=:T)

Return the modulus defining the finite field $k$.
"""
function modulus(k::fqPolyRepField, var::VarName=:T)
  p::Int = characteristic(k)
  Q = polynomial(Native.GF(p), [], var)
  GC.@preserve k begin
    P = @ccall libflint.fq_nmod_ctx_modulus(k::Ref{fqPolyRepField})::Ptr{fpPolyRingElem}
    @ccall libflint.nmod_poly_set(Q::Ref{fpPolyRingElem}, P::Ptr{fpPolyRingElem})::Nothing
  end
  return Q
end

function defining_polynomial(k::fqPolyRepField)
  F = fpField(UInt(characteristic(k)))
  Fx, = polynomial_ring(F, "x", cached = false)
  return defining_polynomial(Fx, k)
end

function defining_polynomial(R::fpPolyRing, k::fqPolyRepField)
  Q = R()
  GC.@preserve k begin
    P = @ccall libflint.fq_nmod_ctx_modulus(k::Ref{fqPolyRepField})::Ptr{fpPolyRingElem}
    @ccall libflint.nmod_poly_set(Q::Ref{fpPolyRingElem}, P::Ptr{fpPolyRingElem})::Nothing
  end
  return Q
end

###############################################################################
#
#   Promotions
#
###############################################################################

promote_rule(::Type{fqPolyRepFieldElem}, ::Type{T}) where {T <: Integer} = fqPolyRepFieldElem

promote_rule(::Type{fqPolyRepFieldElem}, ::Type{ZZRingElem}) = fqPolyRepFieldElem

promote_rule(::Type{fqPolyRepFieldElem}, ::Type{fpFieldElem}) = fqPolyRepFieldElem

###############################################################################
#
#   Parent object call overload
#
###############################################################################

function (a::fqPolyRepField)()
  z = fqPolyRepFieldElem(a)
  return z
end

(a::fqPolyRepField)(b::Integer) = a(ZZRingElem(b))

function (a::fqPolyRepField)(b::Int)
  z = fqPolyRepFieldElem(a, b)
  return z
end

function (a::fqPolyRepField)(b::ZZRingElem)
  z = fqPolyRepFieldElem(a, b)
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

function (a::fqPolyRepField)(b::Vector{<:IntegerUnion})
  da = degree(a)
  db = length(b)
  da == db || error("Coercion impossible")
  F = Native.GF(Int(characteristic(a)), cached = false)
  z = fqPolyRepFieldElem(a, polynomial(F, b))
  return z
end

function (A::fqPolyRepField)(x::fpFieldElem)
  @assert characteristic(A) == characteristic(parent(x))
  return A(lift(x))
end

function (F::fqPolyRepField)(a::zzModRingElem)
  @assert is_divisible_by(characteristic(parent(a)), characteristic(F)) "incompatible parents"
  return F(a.data)
end

function (k::fqPolyRepField)(a::QQFieldElem)
  return k(numerator(a)) // k(denominator(a))
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
#   Minimal polynomial and characteristic polynomial
#
###############################################################################

function minpoly(a::fqPolyRepFieldElem)
  Fp = Native.GF(Int(characteristic(parent(a))), cached=false)
  Rx, _ = polynomial_ring(Fp, cached=false)
  return minpoly(Rx, a)
end

function minpoly(Rx::fpPolyRing, a::fqPolyRepFieldElem)
  @assert characteristic(base_ring(Rx)) == characteristic(parent(a))
  c = fqPolyRepFieldElem[a]
  fa = frobenius(a)
  while !(fa in c)
    push!(c, fa)
    fa = frobenius(fa)
  end
  St = polynomial_ring(parent(a), cached=false)[1]
  f = prod([gen(St) - x for x = c], init=one(St))
  g = Rx()
  for i = 0:degree(f)
    setcoeff!(g, i, coeff(coeff(f, i), 0))
  end
  return g
end

function charpoly(a::fqPolyRepFieldElem)
  Fp = Native.GF(Int(characteristic(parent(a))), cached=false)
  Rx, _ = polynomial_ring(Fp, cached=false)
  return charpoly(Rx, a)
end

function charpoly(Rx::fpPolyRing, a::fqPolyRepFieldElem)
  g = minpoly(Rx, a)
  return g^div(degree(parent(a)), degree(g))
end

###############################################################################
#
#   Representation matrix
#
###############################################################################

function representation_matrix(a::fqPolyRepFieldElem)
  F = parent(a)
  k = Native.GF(Int(characteristic(F)))
  m = zero_matrix(k, degree(F), degree(F))
  @ccall libflint.fq_nmod_embed_mul_matrix(m::Ref{fpMatrix}, a::Ref{fqPolyRepFieldElem}, F::Ref{fqPolyRepField})::Nothing
  @ccall libflint.nmod_mat_transpose(m::Ref{fpMatrix}, m::Ref{fpMatrix})::Nothing
  return m
end
