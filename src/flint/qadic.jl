###############################################################################
#
#   qadic.jl : flint qadic numbers
#
###############################################################################

###############################################################################
#
#   Data type and parent object methods
#
###############################################################################

@doc raw"""
    O(R::QadicField, m::ZZRingElem)

Construct the value $0 + O(p^n)$ given $m = p^n$. An exception results if $m$
is not found to be a power of `p = prime(R)`.
"""
function O(R::QadicField, m::ZZRingElem)
  if isone(m)
    N = 0
  else
    p = prime(R)
    if m == p
      N = 1
    else
      N = flog(m, p)
      p^(N) != m && error("Not a power of p in p-adic O()")
    end
  end
  d = QadicFieldElem(N)
  d.parent = R
  return d
end

@doc raw"""
    O(R::QadicField, m::QQFieldElem)

Construct the value $0 + O(p^n)$ given $m = p^n$. An exception results if $m$
is not found to be a power of `p = prime(R)`.
"""
function O(R::QadicField, m::QQFieldElem)
  d = denominator(m)
  if isone(d)
    return O(R, numerator(m))
  end
  !isone(numerator(m)) && error("Not a power of p in p-adic O()")
  p = prime(R)
  if d == p
    N = -1
  else
    N = -flog(d, p)
    p^(-N) != d && error("Not a power of p in p-adic O()")
  end
  r = QadicFieldElem(N)
  r.parent = R
  return r
end

@doc raw"""
    O(R::QadicField, m::Integer)

Construct the value $0 + O(p^n)$ given $m = p^n$. An exception results if $m$
is not found to be a power of `p = prime(R)`.
"""
O(R::QadicField, m::Integer) = O(R, ZZRingElem(m))

elem_type(::Type{QadicField}) = QadicFieldElem

base_ring_type(::Type{QadicField}) = typeof(Union{})

base_ring(a::QadicField) = Union{}

parent(a::QadicFieldElem) = a.parent

is_domain_type(::Type{QadicFieldElem}) = true

is_exact_type(R::Type{QadicFieldElem}) = false

parent_type(::Type{QadicFieldElem}) = QadicField

function _prime(R::QadicField, n::Int = 1)
  z = ZZRingElem()
  @ccall libflint.padic_ctx_pow_ui(z::Ref{ZZRingElem}, n::UInt, R::Ref{QadicField})::Nothing
  return z
end

function base_field(K::QadicField)
  L = get_attribute!(K, :base_field) do
    return PadicField(prime(K), precision(K), cached = false)
  end::PadicField
  # Should not be here, but Hecke needs it
  setprecision!(L, precision(K))
  return L
end

###############################################################################
#
#   Basic manipulation
#
###############################################################################

function Base.deepcopy_internal(a::QadicFieldElem, dict::IdDict{Any, Any})
  z = parent(a)()
  z.N = a.N
  @ccall libflint.qadic_set(z::Ref{QadicFieldElem}, a::Ref{QadicFieldElem}, parent(a)::Ref{QadicField})::Nothing
  return z
end

function Base.hash(a::QadicFieldElem, h::UInt)
  return xor(hash(lift(QQPolyRing(QQ, :x), a), h),
             xor(hash([prime(parent(a)),degree(parent(a))], h), h))
end

function degree(R::QadicField)
  return @ccall libflint.qadic_ctx_degree(R::Ref{QadicField})::Int
end

@doc raw"""
    prime(R::QadicField)

Return the prime $p$ for the given $q$-adic field.
"""
function prime(R::QadicField)
  z = ZZRingElem()
  @ccall libflint.padic_ctx_pow_ui(z::Ref{ZZRingElem}, 1::UInt, R::Ref{QadicField})::Nothing
  return z
end

@doc raw"""
    precision(a::QadicFieldElem)

Return the precision of the given $q$-adic field element, i.e. if the element
is known to $O(p^n)$ this function will return $n$.
"""
precision(a::QadicFieldElem) = a.N

@doc raw"""
    valuation(a::QadicFieldElem)

Return the valuation of the given $q$-adic field element, i.e. if the given
element is divisible by $p^n$ but not a higher power of $q$ then the function
will return $n$.
"""
function valuation(a::QadicFieldElem)
  iszero(a) ? precision(a) : @ccall libflint.qadic_val(a::Ref{QadicFieldElem})::Int
end

@doc raw"""
    lift(R::QQPolyRing, a::QadicFieldElem)

Return a lift of the given $q$-adic field element to $\mathbb{Q}[x]$.
"""
function lift(R::QQPolyRing, a::QadicFieldElem)
  ctx = parent(a)
  r = R()
  @ccall libflint.padic_poly_get_fmpq_poly(r::Ref{QQPolyRingElem}, a::Ref{QadicFieldElem}, ctx::Ref{QadicField})::Nothing
  return r
end

@doc raw"""
    lift(R::ZZPolyRing, a::QadicFieldElem)

Return a lift of the given $q$-adic field element to $\mathbb{Z}[x]$ if possible.
"""
function lift(R::ZZPolyRing, a::QadicFieldElem)
  ctx = parent(a)
  r = R()
  res = Bool(@ccall libflint.padic_poly_get_fmpz_poly(r::Ref{ZZPolyRingElem}, a::Ref{QadicFieldElem}, ctx::Ref{QadicField})::Cint)
  !res && error("Unable to lift")
  return r
end

function zero(R::QadicField; precision::Int=precision(R))
  z = QadicFieldElem(precision)
  @ccall libflint.qadic_zero(z::Ref{QadicFieldElem})::Nothing
  z.parent = R
  return z
end

function one(R::QadicField; precision::Int=precision(R))
  z = QadicFieldElem(precision)
  @ccall libflint.qadic_one(z::Ref{QadicFieldElem})::Nothing
  z.parent = R
  return z
end

iszero(a::QadicFieldElem) = Bool(@ccall libflint.qadic_is_zero(a::Ref{QadicFieldElem})::Cint)

isone(a::QadicFieldElem) = Bool(@ccall libflint.qadic_is_one(a::Ref{QadicFieldElem})::Cint)

is_unit(a::QadicFieldElem) = !Bool(@ccall libflint.qadic_is_zero(a::Ref{QadicFieldElem})::Cint)

characteristic(R::QadicField) = 0

function shift_right(a::QadicFieldElem, n::Int)
  b = deepcopy(a)
  b.val -= n
  return b
end

function shift_left(a::QadicFieldElem, n::Int)
  b = deepcopy(a)
  b.val += n
  return b
end

###############################################################################
#
#   AbstractString I/O
#
###############################################################################

function var(Q::QadicField)
  return Symbol(unsafe_string(Q.var))
end

function expressify(b::QadicFieldElem, x = var(parent(b)); context = nothing)
  R = base_field(parent(b))
  if iszero(b)
    return 0
  end
  sum = Expr(:call, :+)
  c = R(precision = precision(parent(b)))
  for i in degree(parent(b)):-1:0
    @ccall libflint.padic_poly_get_coeff_padic(c::Ref{PadicFieldElem}, b::Ref{QadicFieldElem}, i::Int, parent(b)::Ref{QadicField})::Nothing
    ec = expressify(c, context = context)
    if !iszero(c)
      if iszero(i)
        push!(sum.args, ec)
      elseif isone(i)
        push!(sum.args, Expr(:call, :*, ec, x))
      else
        push!(sum.args, Expr(:call, :*, ec, Expr(:call, :^, x, i)))
      end
    end
  end
  return sum
end

function show(io::IO, a::QadicFieldElem)
  print(io, AbstractAlgebra.obj_to_string(a, context = io))
end

function show(io::IO, R::QadicField)
  @show_name(io, R)
  @show_special(io, R)
  if is_terse(io)
    io = pretty(io)
    print(io, LowercaseOff(), "QQ_$(prime(R))^$(degree(R))")
  else
    print(io, "Unramified extension of $(prime(R))-adic numbers of degree $(degree(R))")
  end
end

###############################################################################
#
#   Canonicalisation
#
###############################################################################

canonical_unit(x::QadicFieldElem) = x

###############################################################################
#
#   Unary operators
#
###############################################################################

function -(x::QadicFieldElem)
  if iszero(x)
    return x
  end
  ctx = parent(x)
  z = QadicFieldElem(x.N)
  @ccall libflint.qadic_neg(z::Ref{QadicFieldElem}, x::Ref{QadicFieldElem}, ctx::Ref{QadicField})::Nothing
  z.parent = ctx
  return z
end

###############################################################################
#
#   Binary operators
#
###############################################################################

function +(x::QadicFieldElem, y::QadicFieldElem)
  check_parent(x, y)
  ctx = parent(x)
  z = QadicFieldElem(min(x.N, y.N))
  z.parent = ctx
  @ccall libflint.qadic_add(z::Ref{QadicFieldElem}, x::Ref{QadicFieldElem}, y::Ref{QadicFieldElem}, ctx::Ref{QadicField})::Nothing
  return z
end

function -(x::QadicFieldElem, y::QadicFieldElem)
  check_parent(x, y)
  ctx = parent(x)
  z = QadicFieldElem(min(x.N, y.N))
  z.parent = ctx
  @ccall libflint.qadic_sub(z::Ref{QadicFieldElem}, x::Ref{QadicFieldElem}, y::Ref{QadicFieldElem}, ctx::Ref{QadicField})::Nothing
  return z
end

function *(x::QadicFieldElem, y::QadicFieldElem)
  check_parent(x, y)
  ctx = parent(x)
  z = QadicFieldElem(min(x.N + valuation(y), y.N + valuation(x)))
  z.parent = ctx
  @ccall libflint.qadic_mul(z::Ref{QadicFieldElem}, x::Ref{QadicFieldElem}, y::Ref{QadicFieldElem}, ctx::Ref{QadicField})::Nothing
  return z
end

###############################################################################
#
#   Ad hoc binary operators
#
###############################################################################

+(a::QadicFieldElem, b::Integer) = a + parent(a)(b)

+(a::QadicFieldElem, b::ZZRingElem) = a + parent(a)(b)

+(a::QadicFieldElem, b::QQFieldElem) = a + parent(a)(b)

+(a::Integer, b::QadicFieldElem) = b + a

+(a::ZZRingElem, b::QadicFieldElem) = b + a

+(a::QQFieldElem, b::QadicFieldElem) = b + a

-(a::QadicFieldElem, b::Integer) = a - parent(a)(b)

-(a::QadicFieldElem, b::ZZRingElem) = a - parent(a)(b)

-(a::QadicFieldElem, b::QQFieldElem) = a - parent(a)(b)

-(a::Integer, b::QadicFieldElem) = parent(b)(a) - b

-(a::ZZRingElem, b::QadicFieldElem) = parent(b)(a) - b

-(a::QQFieldElem, b::QadicFieldElem) = parent(b)(a) - b

*(a::QadicFieldElem, b::Integer) = a*parent(a)(b)

*(a::QadicFieldElem, b::ZZRingElem) = a*parent(a)(b)

*(a::QadicFieldElem, b::QQFieldElem) = a*parent(a)(b)

*(a::Integer, b::QadicFieldElem) = b*a

*(a::ZZRingElem, b::QadicFieldElem) = b*a

*(a::QQFieldElem, b::QadicFieldElem) = b*a

^(a::QadicFieldElem, b::QadicFieldElem) = exp(b * log(a))

//(a::QadicFieldElem, b::QadicFieldElem) = divexact(a, b)

# TODO: As of 15 May 2024 the following two methods don't work in Nemo because
# `(::QadicField)(::PadicFieldElem)` which is needed for the promotion is in
# Hecke
//(a::PadicFieldElem, b::QadicFieldElem) = divexact(a, b)
//(a::QadicFieldElem, b::PadicFieldElem) = divexact(a, b)

###############################################################################
#
#   Comparison
#
###############################################################################

function ==(a::QadicFieldElem, b::QadicFieldElem)
  check_parent(a, b)
  ctx = parent(a)
  z = QadicFieldElem(min(a.N, b.N))
  @ccall libflint.qadic_sub(z::Ref{QadicFieldElem}, a::Ref{QadicFieldElem}, b::Ref{QadicFieldElem}, ctx::Ref{QadicField})::Nothing
  return Bool(@ccall libflint.qadic_is_zero(z::Ref{QadicFieldElem})::Cint)
end

function isequal(a::QadicFieldElem, b::QadicFieldElem)
  if parent(a) != parent(b)
    return false
  end
  return a.N == b.N && a == b
end

###############################################################################
#
#   Ad hoc comparison
#
###############################################################################

==(a::QadicFieldElem, b::Integer) = a == parent(a)(b)

==(a::QadicFieldElem, b::ZZRingElem) = a == parent(a)(b)

==(a::QadicFieldElem, b::QQFieldElem) = a == parent(a)(b)

==(a::Integer, b::QadicFieldElem) = parent(b)(a) == b

==(a::ZZRingElem, b::QadicFieldElem) = parent(b)(a) == b

==(a::QQFieldElem, b::QadicFieldElem) = parent(b)(a) == b

###############################################################################
#
#   Powering
#
###############################################################################

^(q::QadicFieldElem, n::Int) = q^ZZRingElem(n)

function ^(a::QadicFieldElem, n::ZZRingElem)
  ctx = parent(a)
  if n < 0
    return inv(a)^(-n)
  end
  if valuation(a) == 0
    z = QadicFieldElem(a.N) #if expo is ZZRingElem, Int(n) would throw an error
  else             #for units (v==0) this is fine hower.
    z = QadicFieldElem(a.N + (Int(n) - 1)*valuation(a))
  end
  z.parent = ctx
  @ccall libflint.qadic_pow(z::Ref{QadicFieldElem}, a::Ref{QadicFieldElem}, n::Ref{ZZRingElem}, ctx::Ref{QadicField})::Nothing
  return z
end

###############################################################################
#
#   Exact division
#
###############################################################################

function divexact(a::QadicFieldElem, b::QadicFieldElem; check::Bool=true)
  iszero(b) && throw(DivideError())
  return a * inv(b)
end

###############################################################################
#
#   Ad hoc exact division
#
###############################################################################

divexact(a::QadicFieldElem, b::Integer; check::Bool=true) = a*(ZZRingElem(1)//ZZRingElem(b))

divexact(a::QadicFieldElem, b::ZZRingElem; check::Bool=true) = a*(1//b)

divexact(a::QadicFieldElem, b::QQFieldElem; check::Bool=true) = a*inv(b)

divexact(a::Integer, b::QadicFieldElem; check::Bool=true) = ZZRingElem(a)*inv(b)

divexact(a::ZZRingElem, b::QadicFieldElem; check::Bool=true) = inv((ZZRingElem(1)//a)*b)

divexact(a::QQFieldElem, b::QadicFieldElem; check::Bool=true) = inv(inv(a)*b)

###############################################################################
#
#   Inversion
#
###############################################################################

function inv(a::QadicFieldElem)
  iszero(a) && throw(DivideError())
  ctx = parent(a)
  z = QadicFieldElem(a.N - 2*valuation(a))
  z.parent = ctx
  @ccall libflint.qadic_inv(z::Ref{QadicFieldElem}, a::Ref{QadicFieldElem}, ctx::Ref{QadicField})::Cint
  return z
end

###############################################################################
#
#   Divides
#
###############################################################################

function divides(a::QadicFieldElem, b::QadicFieldElem)
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
#   GCD
#
###############################################################################

function gcd(x::QadicFieldElem, y::QadicFieldElem)
  check_parent(x, y)
  if iszero(x) && iszero(y)
    z = zero(parent(x))
  else
    z = one(parent(x))
  end
  return z
end

###############################################################################
#
#   Square root
#
###############################################################################

function Base.sqrt(a::QadicFieldElem; check::Bool=true)
  av = valuation(a)
  check && (av % 2) != 0 && error("Unable to take qadic square root")
  ctx = parent(a)
  z = QadicFieldElem(a.N - div(av, 2))
  z.parent = ctx
  res = Bool(@ccall libflint.qadic_sqrt(z::Ref{QadicFieldElem}, a::Ref{QadicFieldElem}, ctx::Ref{QadicField})::Cint)
  check && !res && error("Square root of p-adic does not exist")
  return z
end

###############################################################################
#
#   Special functions
#
###############################################################################

@doc raw"""
    exp(a::QadicFieldElem)

Return the $p$-adic exponential of $a$, assuming the $p$-adic exponential
function converges at $a$.
"""
function Base.exp(a::QadicFieldElem)
  !iszero(a) && valuation(a) <= 0 && throw(DomainError(a, "Valuation must be positive"))
  ctx = parent(a)
  z = QadicFieldElem(a.N)
  z.parent = ctx
  res = Bool(@ccall libflint.qadic_exp(z::Ref{QadicFieldElem}, a::Ref{QadicFieldElem}, ctx::Ref{QadicField})::Cint)
  !res && error("Unable to compute exponential")
  return z
end

@doc raw"""
    log(a::QadicFieldElem)

Return the $p$-adic logarithm of $a$, assuming the $p$-adic logarithm
converges at $a$.
"""
function log(a::QadicFieldElem)
  av = valuation(a)
  (av > 0 || av < 0 || iszero(a)) && throw(DomainError(a, "Valuation must be zero"))
  av = valuation(a-1)
  ctx = parent(a)
  if av == 0
    qm1 = _prime(ctx, degree(ctx)) - 1
    a = a^qm1
  end

  ctx = parent(a)
  z = QadicFieldElem(a.N)
  z.parent = ctx
  res = Bool(@ccall libflint.qadic_log(z::Ref{QadicFieldElem}, a::Ref{QadicFieldElem}, ctx::Ref{QadicField})::Cint)
  !res && error("Unable to compute logarithm")
  if av == 0
    z = divexact(z, qm1)
  end
  return z
end

@doc raw"""
    teichmuller(a::QadicFieldElem)

Return the Teichmuller lift of the $q$-adic value $a$. We require the
valuation of $a$ to be non-negative. The precision of the output will be the
same as the precision of the input. For convenience, if $a$ is congruent to
zero modulo $q$ we return zero. If the input is not valid an exception is
thrown.
"""
function teichmuller(a::QadicFieldElem)
  valuation(a) < 0 && throw(DomainError(a, "Valuation must be non-negative"))
  ctx = parent(a)
  z = QadicFieldElem(a.N)
  z.parent = ctx
  @ccall libflint.qadic_teichmuller(z::Ref{QadicFieldElem}, a::Ref{QadicFieldElem}, ctx::Ref{QadicField})::Nothing
  return z
end

@doc raw"""
    frobenius(a::QadicFieldElem, e::Int = 1)

Return the image of the $e$-th power of Frobenius on the $q$-adic value $a$.
The precision of the output will be the same as the precision of the input.
"""
function frobenius(a::QadicFieldElem, e::Int = 1)
  ctx = parent(a)
  z = QadicFieldElem(a.N)
  z.parent = ctx
  @ccall libflint.qadic_frobenius(z::Ref{QadicFieldElem}, a::Ref{QadicFieldElem}, e::Int, ctx::Ref{QadicField})::Nothing
  return z
end

###############################################################################
#
#   Unsafe operators
#
###############################################################################

function zero!(z::QadicFieldElem; precision::Int=precision(parent(z)))
  z.N = precision
  ctx = parent(z)
  @ccall libflint.qadic_zero(z::Ref{QadicFieldElem}, ctx::Ref{QadicField})::Nothing
  return z
end

function mul!(z::QadicFieldElem, x::QadicFieldElem, y::QadicFieldElem)
  z.N = min(x.N + valuation(y), y.N + valuation(x))
  ctx = parent(x)
  @ccall libflint.qadic_mul(z::Ref{QadicFieldElem}, x::Ref{QadicFieldElem}, y::Ref{QadicFieldElem}, ctx::Ref{QadicField})::Nothing
  return z
end

function add!(z::QadicFieldElem, x::QadicFieldElem, y::QadicFieldElem)
  z.N = min(x.N, y.N)
  ctx = parent(x)
  @ccall libflint.qadic_add(z::Ref{QadicFieldElem}, x::Ref{QadicFieldElem}, y::Ref{QadicFieldElem}, ctx::Ref{QadicField})::Nothing
  return z
end

###############################################################################
#
#   Trace and norm
#
###############################################################################

function tr(r::QadicFieldElem)
  t = base_field(parent(r))()
  @ccall libflint.qadic_trace(t::Ref{PadicFieldElem}, r::Ref{QadicFieldElem}, parent(r)::Ref{QadicField})::Nothing
  return t
end

function norm(r::QadicFieldElem)
  t = base_field(parent(r))()
  @ccall libflint.qadic_norm(t::Ref{PadicFieldElem}, r::Ref{QadicFieldElem}, parent(r)::Ref{QadicField})::Nothing
  return t
end

###############################################################################
#
#   Conversions and promotions
#
###############################################################################

promote_rule(::Type{QadicFieldElem}, ::Type{T}) where {T <: Integer} = QadicFieldElem

promote_rule(::Type{QadicFieldElem}, ::Type{Rational{V}}) where {V <: Integer} = QadicFieldElem

promote_rule(::Type{QadicFieldElem}, ::Type{ZZRingElem}) = QadicFieldElem

promote_rule(::Type{QadicFieldElem}, ::Type{QQFieldElem}) = QadicFieldElem

promote_rule(::Type{QadicFieldElem}, ::Type{PadicFieldElem}) = QadicFieldElem

###############################################################################
#
#   Parent object overloads
#
###############################################################################

function (R::QadicField)(; precision::Int=precision(R))
  z = QadicFieldElem(precision)
  z.parent = R
  return z
end

function gen(R::QadicField; precision::Int=precision(R))
  z = QadicFieldElem(precision)
  @ccall libflint.qadic_gen(z::Ref{QadicFieldElem}, R::Ref{QadicField})::Nothing
  z.parent = R
  return z
end

function (R::QadicField)(a::UInt; precision::Int=precision(R))
  if a == 0
    z = QadicFieldElem(precision)
    z.parent = R
    return z
  end
  v = valuation(a, prime(R))
  z = QadicFieldElem(precision + v)
  @ccall libflint.qadic_set_ui(z::Ref{QadicFieldElem}, a::UInt, R::Ref{QadicField})::Nothing
  z.parent = R
  return z
end

function (R::QadicField)(a::Int; precision::Int=precision(R))
  if a == 0
    z = QadicFieldElem(precision)
    z.parent = R
    return z
  end
  v = valuation(a, prime(R))
  z = QadicFieldElem(precision + v)
  @ccall libflint.padic_poly_set_si(z::Ref{QadicFieldElem}, a::Int, R::Ref{QadicField})::Nothing
  z.parent = R
  return z
end

function (R::QadicField)(n::ZZRingElem; precision::Int=precision(R))
  if iszero(n) || isone(n)
    N = 0
  else
    p = prime(R)
    N = valuation(n, p)
  end
  z = QadicFieldElem(N + precision)
  @ccall libflint.padic_poly_set_fmpz(z::Ref{QadicFieldElem}, n::Ref{ZZRingElem}, R::Ref{QadicField})::Nothing
  z.parent = R
  return z
end

function (R::QadicField)(n::QQFieldElem; precision::Int=precision(R))
  m = denominator(n)
  if isone(m)
    return R(numerator(n); precision = precision)
  end
  p = prime(R)
  if m == p
    N = -1
  else
    N = -remove(m, p)[1]
  end
  z = QadicFieldElem(N + precision)
  @ccall libflint.padic_poly_set_fmpq(z::Ref{QadicFieldElem}, n::Ref{QQFieldElem}, R::Ref{QadicField})::Nothing
  z.parent = R
  return z
end

function (R::QadicField)(n::ZZPolyRingElem; precision::Int=precision(R))
  z = QadicFieldElem(precision)
  @ccall libflint.qadic_set_fmpz_poly(z::Ref{QadicFieldElem}, n::Ref{ZZPolyRingElem}, R::Ref{QadicField})::Nothing
  z.parent = R
  return z
end

function (R::QadicField)(n::QQPolyRingElem; precision::Int=precision(R))

  if degree(n) > degree(R) + 1
    error("Polynomial degree larger than degree of qadic field.")
  end
  m = denominator(n)
  p = prime(R)
  if m == p
    N = -1
  else
    N = -remove(m, p)[1]
  end
  z = QadicFieldElem(N + precision)
  @ccall libflint.padic_poly_set_fmpq_poly(z::Ref{QadicFieldElem}, n::Ref{QQPolyRingElem}, R::Ref{QadicField})::Nothing
  z.parent = R
  return z
end

function (R::QadicField)(b::Rational{<:Integer}; precision::Int=precision(R))
  return R(QQFieldElem(b); precision)
end

(R::QadicField)(n::Integer; precision::Int=precision(R)) = R(ZZRingElem(n); precision)

function (R::QadicField)(n::QadicFieldElem)
  parent(n) != R && error("Unable to coerce into q-adic field")
  return n
end

###############################################################################
#
#   As p-adic polynomial
#
###############################################################################

function (Rx::Generic.PolyRing{PadicFieldElem})(a::QadicFieldElem)
  Qq = parent(a)
  #@assert Rx === parent(defining_polynomial(Qq))
  R = base_ring(Rx)
  coeffs = Vector{PadicFieldElem}(undef, degree(Qq))
  for i = 1:length(coeffs)
    c = R()
    @ccall libflint.padic_poly_get_coeff_padic(c::Ref{PadicFieldElem}, a::Ref{QadicFieldElem}, (i - 1)::Int, parent(a)::Ref{QadicField})::Nothing
    coeffs[i] = c
  end
  return Rx(coeffs)
end

function coeff(x::QadicFieldElem, i::Int)
  R = base_field(parent(x))
  c = R()
  @ccall libflint.padic_poly_get_coeff_padic(c::Ref{PadicFieldElem}, x::Ref{QadicFieldElem}, i::Int, parent(x)::Ref{QadicField})::Nothing
  return c
end

function setcoeff!(x::QadicFieldElem, i::Int, y::PadicFieldElem)
  @ccall libflint.padic_poly_set_coeff_padic(x::Ref{QadicFieldElem}, i::Int, y::Ref{PadicFieldElem}, parent(x)::Ref{QadicField})::Nothing
end

function setcoeff!(x::QadicFieldElem, i::Int, y::UInt)
  return setcoeff!(x, i, ZZRingElem(y))
end

function setcoeff!(x::QadicFieldElem, i::Int, y::ZZRingElem)
  R = base_field(parent(x))
  Y = R(ZZRingElem(y))
  @ccall libflint.padic_poly_set_coeff_padic(x::Ref{QadicFieldElem}, i::Int, Y::Ref{PadicFieldElem}, parent(x)::Ref{QadicField})::Nothing
end

Base.length(a::QadicFieldElem) = a.length

###############################################################################
#
#   QadicField constructor
#
###############################################################################

# Kept for backwards compatibility; the user facing constructor is `qadic_field`
function QadicField(p::Integer, d::Int, prec::Int = 64, var::String = "a"; cached::Bool = true)
  return QadicField(ZZRingElem(p), d, prec, var, cached = cached)
end

@doc raw"""
    qadic_field(p::Integer, d::Int, var::String = "a"; precision::Int=64, cached::Bool=true, check::Bool=true)
    qadic_field(p::ZZRingElem, d::Int, var::String = "a"; precision::Int=64, cached::Bool=true, check::Bool=true)

Return an unramified extension $K$ of degree $d$ of a $p$-adic field for the given
prime $p$.
The generator of $K$ is printed as `var`.

The default absolute precision of elements of $K$ may be set with `precision`.

See also [`unramified_extension`](@ref).
"""
qadic_field

function qadic_field(p::Integer, d::Int, var::String = "a"; precision::Int=64, cached::Bool=true, check::Bool=true)
  return qadic_field(ZZRingElem(p), d, var; precision=precision, cached=cached, check=check)
end

function qadic_field(p::ZZRingElem, d::Int, var::String = "a"; precision::Int=64, cached::Bool=true, check::Bool=true)
  return QadicField(p, d, precision, var; cached=cached, check=check)
end

@doc raw"""
    unramified_extension(Qp::PadicField, d::Int, var::String = "a"; precision::Int=64, cached::Bool=true)

Return an unramified extension $K$ of degree $d$ of the given $p$-adic field `Qp`.
The generator of $K$ is printed as `var`.

The default absolute precision of elements of $K$ may be set with `precision`.
"""
function unramified_extension(K::PadicField, d::Int, var::String = "a"; precision::Int=precision(K), cached::Bool = true)
  L, a = QadicField(prime(K), d, precision, var, cached = cached, check = false, base_field = K)
  return L, a
end

###############################################################################
#
#   Precision handling
#
###############################################################################

precision(Q::QadicField) = Q.prec_max

function Base.setprecision(q::QadicFieldElem, N::Int)
  r = parent(q)()
  r.N = N
  @ccall libflint.padic_poly_set(r::Ref{QadicFieldElem}, q::Ref{QadicFieldElem}, parent(q)::Ref{QadicField})::Nothing
  return r
end

function setprecision!(q::QadicFieldElem, N::Int)
  q.N = N
  @ccall libflint.qadic_reduce(q::Ref{QadicFieldElem}, parent(q)::Ref{QadicField})::Nothing
  return q
end

function setprecision!(Q::QadicField, n::Int)
  Q.prec_max = n
  setprecision!(base_field(Q), n)
  return Q
end

function setprecision!(f::Generic.Poly{QadicFieldElem}, N::Int)
  for i = 1:length(f)
    setprecision!(f.coeffs[i], N)
  end
  set_length!(f, normalise(f, length(f)))
  return f
end

function Base.setprecision(f::Generic.Poly{QadicFieldElem}, N::Int)
  # map_coefficients handles the coefficient 0 weirdly, so we have to set the
  # 'global' precision
  g = with_precision(base_ring(f), N) do
    map_coefficients(x -> setprecision(x, N), f, parent=parent(f))
  end
  return g
end

function with_precision(f, K::QadicField, n::Int)
  @assert n >= 0
  old = precision(K)
  old_base = precision(base_field(K))
  setprecision!(K, n)
  setprecision!(base_field(K), n)
  v = try
    f()
  finally
    setprecision!(K, old)
    setprecision!(base_field(K), old_base)
  end
  return v
end

Base.setprecision(f::Function, K::QadicField, n::Int) = with_precision(f, K, n)
