###############################################################################
#
#   QQFieldElem.jl : FLINT rationals
#
###############################################################################

###############################################################################
#
#   Data type and parent methods
#
###############################################################################

QQFieldElem(a::Rational{BigInt}) = QQFieldElem(ZZRingElem(a.num), ZZRingElem(a.den))

function QQFieldElem(a::Rational{Int})
  r = QQFieldElem()
  set!(r, numerator(a), UInt(denominator(a)))
  return r
end

QQFieldElem(a::Rational{T}) where {T <: Integer} = QQFieldElem(numerator(a), denominator(a))

QQFieldElem(a::Integer) = QQFieldElem(flintify(a))

QQFieldElem(a::Integer, b::Integer) = QQFieldElem(ZZRingElem(a), ZZRingElem(b))

QQFieldElem(a::ZZRingElem, b::Integer) = QQFieldElem(a, ZZRingElem(b))

QQFieldElem(a::Integer, b::ZZRingElem) = QQFieldElem(ZZRingElem(a), b)

parent(a::QQFieldElem) = QQ

parent_type(::Type{QQFieldElem}) = QQField

elem_type(::Type{QQField}) = QQFieldElem

base_ring_type(::Type{QQField}) = ZZRing

base_ring(a::QQField) = ZZ

is_domain_type(::Type{QQFieldElem}) = true

###############################################################################
#
#   Hashing
#
###############################################################################

function Base.hash(a::QQFieldElem, h::UInt)
  return GC.@preserve a _hash_integer(a.num, _hash_integer(a.den, h))
end

###############################################################################
#
#   Constructors
#
###############################################################################

function //(x::ZZRingElem, y::ZZRingElem)
  iszero(y) && throw(DivideError())
  g = gcd(x, y)
  return QQFieldElem(divexact(x, g), divexact(y, g))
end

//(x::ZZRingElem, y::Integer) = x//ZZRingElem(y)

//(x::Integer, y::ZZRingElem) = ZZRingElem(x)//y

###############################################################################
#
#   Basic manipulation
#
###############################################################################

function numerator(a::QQFieldElem)
  return numerator!(ZZRingElem(), a)
end

function denominator(a::QQFieldElem)
  return denominator!(ZZRingElem(), a)
end

@doc raw"""
    sign(a::QQFieldElem)

Return the sign of $a$ ($-1$, $0$ or $1$) as a fraction.
"""
sign(a::QQFieldElem) = QQFieldElem(sign(numerator(a)))

sign(::Type{Int}, a::QQFieldElem) = Int(@ccall libflint.fmpq_sgn(a::Ref{QQFieldElem})::Cint)

Base.signbit(a::QQFieldElem) = signbit(sign(Int, a))

is_negative(n::QQFieldElem) = sign(Int, n) < 0
is_positive(n::QQFieldElem) = sign(Int, n) > 0

function abs(a::QQFieldElem)
  z = QQFieldElem()
  @ccall libflint.fmpq_abs(z::Ref{QQFieldElem}, a::Ref{QQFieldElem})::Nothing
  return z
end

zero(_::QQField) = QQFieldElem(0)

one(_::QQField) = QQFieldElem(1)

zero(::Type{QQFieldElem}) = QQFieldElem(0)

one(::Type{QQFieldElem}) = QQFieldElem(1)


is_one(a::QQFieldElemOrPtr) = isinteger(a) && is_one(_num_ptr(a))

is_zero(a::QQFieldElemOrPtr) = is_zero(_num_ptr(a))

isinteger(a::QQFieldElemOrPtr) = is_one(_den_ptr(a))

isfinite(::QQFieldElem) = true

isinf(::QQFieldElem) = false


@doc raw"""
    height(a::QQFieldElem)

Return the height of the fraction $a$, namely the largest of the absolute
values of the numerator and denominator.
"""
function height(a::QQFieldElem)
  temp = ZZRingElem()
  @ccall libflint.fmpq_height(temp::Ref{ZZRingElem}, a::Ref{QQFieldElem})::Nothing
  return temp
end

@doc raw"""
    height_bits(a::QQFieldElem)

Return the number of bits of the height of the fraction $a$.
"""
function height_bits(a::QQFieldElem)
  return @ccall libflint.fmpq_height_bits(a::Ref{QQFieldElem})::Int
end

function deepcopy_internal(a::QQFieldElem, dict::IdDict)
  z = QQFieldElem()
  set!(z, a)
  return z
end

characteristic(::QQField) = 0

@doc raw"""
    floor(a::QQFieldElem)

Return the greatest integer that is less than or equal to $a$. The result is
returned as a rational with denominator $1$.
"""
Base.floor(a::QQFieldElem) = floor(QQFieldElem, a)
Base.floor(::Type{QQFieldElem}, a::QQFieldElem) = QQFieldElem(floor(ZZRingElem, a), 1)
Base.floor(::Type{ZZRingElem}, a::QQFieldElem) = fdiv(numerator(a), denominator(a))

@doc raw"""
    ceil(a::QQFieldElem)

Return the least integer that is greater than or equal to $a$. The result is
returned as a rational with denominator $1$.
"""
Base.ceil(a::QQFieldElem) = ceil(QQFieldElem, a)
Base.ceil(::Type{QQFieldElem}, a::QQFieldElem) = QQFieldElem(ceil(ZZRingElem, a), 1)
Base.ceil(::Type{ZZRingElem}, a::QQFieldElem) = cdiv(numerator(a), denominator(a))

Base.trunc(a::QQFieldElem) = trunc(QQFieldElem, a)
Base.trunc(::Type{QQFieldElem}, a::QQFieldElem) = QQFieldElem(trunc(ZZRingElem, a), 1)
Base.trunc(::Type{ZZRingElem}, a::QQFieldElem) = is_positive(a) ? floor(ZZRingElem, a) : ceil(ZZRingElem, a)

Base.round(x::QQFieldElem, ::RoundingMode{:Up}) = ceil(x)
Base.round(::Type{T}, x::QQFieldElem, ::RoundingMode{:Up}) where T <: RingElement = ceil(T, x)

Base.round(x::QQFieldElem, ::RoundingMode{:Down}) = floor(x)
Base.round(::Type{T}, x::QQFieldElem, ::RoundingMode{:Down}) where T <: RingElement = floor(T, x)

Base.round(x::QQFieldElem, ::RoundingMode{:Nearest}) = round(QQFieldElem, x, RoundNearest)
function Base.round(::Type{T}, x::QQFieldElem, ::RoundingMode{:Nearest}) where T <: RingElement
  d = denominator(x)
  n = numerator(x)
  if d == 2
    if mod(n, 4) == 1
      if n > 0
        return Base.div(n, d)
      else
        return Base.div(n, d) - 1
      end
    else
      if n > 0
        return Base.div(n, d) + 1
      else
        return Base.div(n, d)
      end
    end
  end

  return floor(T, x + 1 // 2)
end

Base.round(x::QQFieldElem, ::RoundingMode{:NearestTiesAway}) = sign(x) * floor(abs(x) + 1 // 2)
function Base.round(::Type{T}, x::QQFieldElem, ::RoundingMode{:NearestTiesAway}) where T <: RingElement
  tmp = floor(T, abs(x) + 1 // 2)
  return is_positive(x) ? tmp : -tmp
end

Base.round(a::QQFieldElem) = round(QQFieldElem, a)
Base.round(::Type{T}, a::QQFieldElem) where T <: RingElement =  round(T, a, RoundNearestTiesAway)


nbits(a::QQFieldElem) = nbits(numerator(a)) + nbits(denominator(a))

###############################################################################
#
#   Canonicalisation
#
###############################################################################

canonical_unit(a::QQFieldElem) = a

###############################################################################
#
#   AbstractString I/O
#
###############################################################################

function expressify(a::QQFieldElem; context = nothing)
  n = numerator(a)
  d = denominator(a)
  if isone(d)
    return n
  else
    return Expr(:call, ://, n, d)
  end
end

function show(io::IO, a::QQFieldElem)
  print(io, numerator(a))
  if denominator(a) != 1
    print(io, "//", denominator(a))
  end
end

function show(io::IO, ::MIME"text/plain", a::QQField)
  print(io, "Rational field")
end

function show(io::IO, a::QQField)
  # deliberately no @show_name or @show_special here as this is a singleton type
  if is_terse(io)
    print(pretty(io), LowercaseOff(), "QQ")
  else
    print(io, "Rational field")
  end
end

###############################################################################
#
#   Unary operators
#
###############################################################################

-(a::QQFieldElem) = neg!(QQFieldElem(), a)

conj(x::QQFieldElem) = x

###############################################################################
#
#   Binary operators
#
###############################################################################

+(a::QQFieldElem, b::QQFieldElem) = add!(QQFieldElem(), a, b)

-(a::QQFieldElem, b::QQFieldElem) = sub!(QQFieldElem(), a, b)

*(a::QQFieldElem, b::QQFieldElem) = mul!(QQFieldElem(), a, b)

###############################################################################
#
#   Ad hoc binary operators
#
###############################################################################

for T in (ZZRingElem, Int, UInt, Integer, Rational)
  @eval begin
    +(a::QQFieldElem, b::$T) = add!(QQFieldElem(), a, b)
    +(a::$T, b::QQFieldElem) = add!(QQFieldElem(), a, b)

    -(a::QQFieldElem, b::$T) = sub!(QQFieldElem(), a, b)
    -(a::$T, b::QQFieldElem) = sub!(QQFieldElem(), a, b)

    *(a::QQFieldElem, b::$T) = mul!(QQFieldElem(), a, b)
    *(a::$T, b::QQFieldElem) = mul!(QQFieldElem(), a, b)
  end
end

*(a::QQFieldElem, b::AbstractFloat) = Rational(a) * b
*(a::AbstractFloat, b::QQFieldElem) = a * Rational(b)

###############################################################################
#
#   Comparison
#
###############################################################################

function cmp(a::QQFieldElemOrPtr, b::QQFieldElemOrPtr)
  @ccall libflint.fmpq_cmp(a::Ref{QQFieldElem}, b::Ref{QQFieldElem})::Cint
end

function cmp(a::QQFieldElemOrPtr, b::ZZRingElemOrPtr)
  @ccall libflint.fmpq_cmp_fmpz(a::Ref{QQFieldElem}, b::Ref{ZZRingElem})::Cint
end

function cmp(a::QQFieldElemOrPtr, b::Int)
  @ccall libflint.fmpq_cmp_si(a::Ref{QQFieldElem}, b::Int)::Cint
end

function cmp(a::QQFieldElemOrPtr, b::UInt)
  @ccall libflint.fmpq_cmp_ui(a::Ref{QQFieldElem}, b::UInt)::Cint
end

cmp(a::QQFieldElemOrPtr, b::Integer) = cmp(a, flintify(b))

cmp(a::Union{ZZRingElemOrPtr, Integer}, b::QQFieldElemOrPtr) = -cmp(b, a)

function ==(a::QQFieldElem, b::QQFieldElem)
  return @ccall libflint.fmpq_equal(a::Ref{QQFieldElem}, b::Ref{QQFieldElem})::Bool
end

function isless(a::QQFieldElem, b::QQFieldElem)
  return cmp(a, b) < 0
end

function Base.isapprox(x::QQFieldElem, y::QQFieldElem;
                       atol::Real=0, rtol::Real=0,
                       nans::Bool=false, norm::Function=abs)
    return x == y ||
        (norm(x - y) <= max(atol, rtol*max(norm(x), norm(y))))
end

###############################################################################
#
#   Ad hoc comparison
#
###############################################################################

function ==(a::QQFieldElem, b::Int)
  return @ccall libflint.fmpq_equal_si(a::Ref{QQFieldElem}, b::Int)::Bool
end

==(a::Int, b::QQFieldElem) = b == a

function ==(a::QQFieldElem, b::ZZRingElem)
  return @ccall libflint.fmpq_equal_fmpz(a::Ref{QQFieldElem}, b::Ref{ZZRingElem})::Bool
end

==(a::ZZRingElem, b::QQFieldElem) = b == a

==(a::QQFieldElem, b::Rational{T}) where {T <: Integer} = a == QQFieldElem(b)

==(a::Rational{T}, b::QQFieldElem) where {T <: Integer} = b == a

function isless(a::QQFieldElem, b::IntegerUnion)
  return cmp(a, b) < 0
end

function isless(a::IntegerUnion, b::QQFieldElem)
  return cmp(a, b) < 0
end

isless(a::Rational{T}, b::QQFieldElem) where {T <: Integer} = isless(QQFieldElem(a), b)

isless(a::QQFieldElem, b::Rational{T}) where {T <: Integer} = isless(a, QQFieldElem(b))

isless(a::Float64, b::QQFieldElem) = isless(a, BigFloat(b))

isless(a::QQFieldElem, b::Float64) = isless(BigFloat(a), b)

###############################################################################
#
#   Powering
#
###############################################################################

function ^(a::QQFieldElem, b::Int)
  iszero(a) && b < 0 && throw(DivideError())
  temp = QQFieldElem()
  @ccall libflint.fmpq_pow_si(temp::Ref{QQFieldElem}, a::Ref{QQFieldElem}, b::Int)::Nothing
  return temp
end

function ^(a::QQFieldElem, k::ZZRingElem)
  is_zero(a) && return QQFieldElem(is_zero(k) ? 1 : 0)
  is_one(a) && return QQFieldElem(1)
  a == -1 && return QQFieldElem(isodd(k) ? -1 : 1)
  return a^Int(k)
end

###############################################################################
#
#   Shifting
#
###############################################################################

@doc raw"""
    >>(a::QQFieldElem, b::Int)

Return $a/2^b$.
"""
function >>(a::QQFieldElem, b::Int)
  z = QQFieldElem()
  @ccall libflint.fmpq_div_2exp(z::Ref{QQFieldElem}, a::Ref{QQFieldElem}, b::Int)::Nothing
  return z
end

@doc raw"""
    <<(a::QQFieldElem, b::Int)

Return $a \times 2^b$.
"""
function <<(a::QQFieldElem, b::Int)
  z = QQFieldElem()
  @ccall libflint.fmpq_mul_2exp(z::Ref{QQFieldElem}, a::Ref{QQFieldElem}, b::Int)::Nothing
  return z
end

###############################################################################
#
#   Square root
#
###############################################################################

function Base.sqrt(a::QQFieldElem; check::Bool=true)
  snum = sqrt(numerator(a); check=check)
  sden = sqrt(denominator(a); check=check)
  return QQFieldElem(snum, sden)
end

function is_square(a::QQFieldElem)
  if !is_square(numerator(a)) || !is_square(denominator(a))
    return false
  end
  return true
end

function is_square_with_sqrt(a::QQFieldElem)
  f1, s1 = is_square_with_sqrt(numerator(a))
  if !f1
    return false, zero(QQFieldElem)
  end
  f2, s2 = is_square_with_sqrt(denominator(a))
  if !f2
    return false, zero(QQFieldElem)
  end
  return true, QQFieldElem(s1, s2)
end

@doc raw"""
    root(x::QQFieldElem, n::Int; check::Bool=true)

Return the $n$-the root of $x$. We require $n > 0$ and that
$x \geq 0$ if $n$ is even. By default the function tests whether the input was
a perfect $n$-th power and if not raises an exception. If `check=false` this
check is omitted.
"""
function root(x::QQFieldElem, n::Int; check::Bool=true)
  num = root(numerator(x), n; check=check)
  den = root(denominator(x), n; check=check)
  return QQFieldElem(num, den)
end

###############################################################################
#
#   Power detection
#
###############################################################################

@doc raw"""
    is_perfect_power_with_data(a::QQFieldElem) -> Int, QQFieldElem
    is_perfect_power_with_data(a::Rational) -> Int, Rational

Return $e$, $r$ such that $a = r^e$ with $e$ maximal. Note: $1 = 1^0$.
"""
function is_perfect_power_with_data(a::QQFieldElem)
  e, r = is_perfect_power_with_data(numerator(a))
  if e == 1
    return e, a
  end
  f, s = is_perfect_power_with_data(denominator(a))
  g = gcd(e, f)
  return g, r^Base.div(e, g) // s^Base.div(f, g)
end

function is_perfect_power_with_data(a::Rational)
  T = typeof(denominator(a))
  e, r = is_perfect_power_with_data(QQFieldElem(a))
  return e, T(numerator(r)) // T(denominator(r))
end

function is_power(a::QQFieldElem, n::Int)
  fl, nu = is_power(numerator(a), n)
  if !fl
    return fl, a
  end
  fl, de = is_power(denominator(a), n)
  return fl, QQFieldElem(nu, de)
end

###############################################################################
#
#   Inversion
#
###############################################################################

function inv(a::QQFieldElem)
  if iszero(a)
    error("Element not invertible")
  end
  z = QQFieldElem()
  @ccall libflint.fmpq_inv(z::Ref{QQFieldElem}, a::Ref{QQFieldElem})::Nothing
  return z
end

###############################################################################
#
#   Exact division
#
###############################################################################

function divexact(a::QQFieldElem, b::QQFieldElem; check::Bool=true)
  iszero(b) && throw(DivideError())
  return divexact!(QQFieldElem(), a, b)
end

div(a::QQFieldElem, b::QQFieldElem) = divexact(a, b)

function rem(a::QQFieldElem, b::QQFieldElem)
  iszero(b) && throw(DivideError())
  return QQFieldElem(0)
end

###############################################################################
#
#   Ad hoc exact division
#
###############################################################################

function divexact(a::QQFieldElem, b::ZZRingElem; check::Bool=true)
  iszero(b) && throw(DivideError())
  return divexact!(QQFieldElem(), a, b)
end

function divexact(a::ZZRingElem, b::QQFieldElem; check::Bool=true)
  iszero(b) && throw(DivideError())
  return inv(b)*a
end

divexact(a::QQFieldElem, b::Integer; check::Bool=true) = divexact(a, ZZRingElem(b); check=check)

function divexact(a::Integer, b::QQFieldElem; check::Bool=true)
  iszero(b) && throw(DivideError())
  return inv(b)*a
end

divexact(a::QQFieldElem, b::Rational{T}; check::Bool=true) where {T <: Integer} = divexact(a, QQFieldElem(b); check=check)

divexact(a::Rational{T}, b::QQFieldElem; check::Bool=true) where {T <: Integer} = divexact(QQFieldElem(a), b; check=check)

//(a::QQFieldElem, b::ZZRingElem) = divexact(a, b)

//(a::QQFieldElem, b::QQFieldElem) = divexact(a, b)

//(a::QQFieldElem, b::Integer) = divexact(a, b)

//(a::QQFieldElem, b::Rational{<:Integer}) = divexact(a, b)

function //(a::ZZRingElem, b::QQFieldElem)
  return QQFieldElem(a) // b
end

###############################################################################
#
#   Modular arithmetic
#
###############################################################################

@doc raw"""
    mod(a::QQFieldElem, b::ZZRingElem)
    mod(a::QQFieldElem, b::Integer)

Return $a \pmod{b}$ where $b$ is an integer coprime to the denominator of
$a$.

# Examples

```jldoctest
julia> mod(-ZZ(2)//3, 7)
4

julia> mod(ZZ(1)//2, ZZ(5))
3
```
"""
function mod(a::QQFieldElem, b::ZZRingElem)
  iszero(b) && throw(DivideError())
  z = ZZRingElem()
  @ccall libflint.fmpq_mod_fmpz(z::Ref{ZZRingElem}, a::Ref{QQFieldElem}, b::Ref{ZZRingElem})::Nothing
  return z
end

mod(a::QQFieldElem, b::Integer) = mod(a, ZZRingElem(b))

###############################################################################
#
#   GCD
#
###############################################################################

function gcd(a::QQFieldElem, b::QQFieldElem)
  z = QQFieldElem()
  @ccall libflint.fmpq_gcd(z::Ref{QQFieldElem}, a::Ref{QQFieldElem}, b::Ref{QQFieldElem})::Nothing
  return z
end

################################################################################
#
#   Ad hoc Remove and valuation
#
################################################################################

function remove(a::QQFieldElem, b::IntegerUnion)
  b <= 1 && error("Factor <= 1")
  a == 0 && error("Not yet implemented")
  remove!(deepcopy(a), ZZ(b))
end

function valuation(a::QQFieldElem, b::IntegerUnion)
  b <= 1 && error("Factor <= 1")
  a == 0 && error("Not yet implemented")
  valuation!(deepcopy(a), ZZ(b))
end

function remove!(a::QQFieldElem, b::ZZRingElem)
  nr = _num_ptr(a)
  vn, nr = remove!(nr, b)
  #QQFieldElem's are simplified: either num OR den will be non-trivial
  if !is_zero(vn)
    return vn, a
  end
  nr = _den_ptr(a)
  vn, nr = remove!(nr, b)
  return -vn, a
end

function valuation!(a::QQFieldElem, b::ZZRingElem)
  nr = _num_ptr(a)
  vn, nr = remove!(nr, b)
  #QQFieldElem's are simplified: either num OR den will be non-trivial
  if !is_zero(vn)
    return vn
  end
  nr = _den_ptr(a)
  vn, nr = remove!(nr, b)
  return -vn
end

###############################################################################
#
#   Rational reconstruction
#
###############################################################################

@doc raw"""
    reconstruct(a::ZZRingElem, m::ZZRingElem)
    reconstruct(a::ZZRingElem, m::Integer)
    reconstruct(a::Integer, m::ZZRingElem)
    reconstruct(a::Integer, m::Integer)

Attempt to return a rational number $n/d$ such that
$0 \leq |n| \leq \lfloor\sqrt{m/2}\rfloor$ and
$0 < d \leq \lfloor\sqrt{m/2}\rfloor$ such that gcd$(n, d) = 1$ and
$a \equiv nd^{-1} \pmod{m}$. If no solution exists, an exception is thrown.

# Examples

```jldoctest
julia> a = reconstruct(7, 13)
1//2

julia> b = reconstruct(ZZ(15), 31)
-1//2

julia> c = reconstruct(ZZ(123), ZZ(237))
9//2
```
"""
function reconstruct(a::ZZRingElem, m::ZZRingElem)
  success, c = unsafe_reconstruct(a, m)
  if !success
    error("Impossible rational reconstruction")
  end
  return c
end

reconstruct(a::ZZRingElem, m::Integer) =  reconstruct(a, ZZRingElem(m))

reconstruct(a::Integer, m::ZZRingElem) =  reconstruct(ZZRingElem(a), m)

reconstruct(a::Integer, m::Integer) =  reconstruct(ZZRingElem(a), ZZRingElem(m))

@doc raw"""
    reconstruct(a::ZZRingElem, m::ZZRingElem, N::ZZRingElem, D::ZZRingElem)

Attempt to return a rational number $n/d$ such that $0 \leq |n| \leq N$ and $0 < d \leq D$
such that $2 N D < m$, gcd$(n, d) = 1$, and $a \equiv nd^{-1} \pmod{m}$.

Returns a tuple (`success`, `n/d`), where `success` signals the success of reconstruction.
"""
function reconstruct(a::ZZRingElem, m::ZZRingElem, N::ZZRingElem, D::ZZRingElem)
  c = QQFieldElem()
  success = Bool(@ccall libflint.fmpq_reconstruct_fmpz_2(c::Ref{QQFieldElem}, a::Ref{ZZRingElem}, m::Ref{ZZRingElem}, N::Ref{ZZRingElem}, D::Ref{ZZRingElem})::Cint)
  return success, c
end

@doc raw"""
   unsafe_reconstruct(a::ZZRingElem, m::ZZRingElem)

Same as [`reconstruct`](@ref), but does not throw if reconstruction fails.
Returns a tuple (`success`, `n/d`), where `success` signals the success of reconstruction.
"""
function unsafe_reconstruct(a::ZZRingElem, m::ZZRingElem)
  c = QQFieldElem()
  success = Bool(@ccall libflint.fmpq_reconstruct_fmpz(c::Ref{QQFieldElem}, a::Ref{ZZRingElem}, m::Ref{ZZRingElem})::Cint)
  return success, c
end

###############################################################################
#
#   Rational enumeration
#
###############################################################################

@doc raw"""
    next_minimal(a::QQFieldElem)

Given $a$, return the next rational number in the sequence obtained by
enumerating all positive denominators $q$, and for each $q$ enumerating
the numerators $1 \le p < q$ in order and generating both $p/q$ and $q/p$,
but skipping all gcd$(p,q) \neq 1$. Starting with zero, this generates
every non-negative rational number once and only once, with the first
few entries being $0, 1, 1/2, 2, 1/3, 3, 2/3, 3/2, 1/4, 4, 3/4, 4/3, \ldots$.
This enumeration produces the rational numbers in order of minimal height.
It has the disadvantage of being somewhat slower to compute than the
Calkin-Wilf enumeration. If $a < 0$ we throw a `DomainError()`.

# Examples

```jldoctest
julia> next_minimal(ZZ(2)//3)
3//2
```
"""
function next_minimal(a::QQFieldElem)
  a < 0 && throw(DomainError(a, "Argument must be non-negative"))
  c = QQFieldElem()
  @ccall libflint.fmpq_next_minimal(c::Ref{QQFieldElem}, a::Ref{QQFieldElem})::Nothing
  return c
end

@doc raw"""
    next_signed_minimal(a::QQFieldElem)

Given a signed rational number $a$ assumed to be in canonical form,
return the next element in the minimal-height sequence generated by
`next_minimal` but with negative numbers interleaved. The sequence begins
$0, 1, -1, 1/2, -1/2, 2, -2, 1/3, -1/3, \ldots$. Starting with zero, this
generates every rational number once and only once, in order of minimal
height.

# Examples

```jldoctest
julia> next_signed_minimal(-ZZ(21)//31)
31//21
```
"""
function next_signed_minimal(a::QQFieldElem)
  c = QQFieldElem()
  @ccall libflint.fmpq_next_signed_minimal(c::Ref{QQFieldElem}, a::Ref{QQFieldElem})::Nothing
  return c
end

@doc raw"""
    next_calkin_wilf(a::QQFieldElem)

Return the next number after $a$ in the breadth-first traversal of the
Calkin-Wilf tree. Starting with zero, this generates every non-negative
rational number once and only once, with the first few entries being
$0, 1, 1/2, 2, 1/3, 3/2, 2/3, 3, 1/4, 4/3, 3/5, 5/2, 2/5, \ldots$.
Despite the appearance of the initial entries, the Calkin-Wilf enumeration
does not produce the rational numbers in order of height: some small
fractions will appear late in the sequence. This order has the advantage of
being faster to produce than the minimal-height order.

# Examples

```jldoctest
julia> next_calkin_wilf(ZZ(321)//113)
113//244
```
"""
function next_calkin_wilf(a::QQFieldElem)
  a < 0 && throw(DomainError(a, "Argument must be non-negative"))
  c = QQFieldElem()
  @ccall libflint.fmpq_next_calkin_wilf(c::Ref{QQFieldElem}, a::Ref{QQFieldElem})::Nothing
  return c
end

@doc raw"""
    next_signed_calkin_wilf(a::QQFieldElem)

Given a signed rational number $a$ returns the next element in the
Calkin-Wilf sequence with negative numbers interleaved. The sequence begins
$0, 1, -1, 1/2, -1/2, 2, -2, 1/3, -1/3, \ldots$. Starting with zero, this
generates every rational number once and only once, but not in order of
minimal height.

# Examples

```jldoctest
julia> next_signed_calkin_wilf(-ZZ(51)//(17))
1//4
```
"""
function next_signed_calkin_wilf(a::QQFieldElem)
  c = QQFieldElem()
  @ccall libflint.fmpq_next_signed_calkin_wilf(c::Ref{QQFieldElem}, a::Ref{QQFieldElem})::Nothing
  return c
end

###############################################################################
#
#   Special functions
#
###############################################################################

@doc raw"""
    harmonic(n::Int)

Return the harmonic number $H_n = 1 + 1/2 + 1/3 + \cdots + 1/n$.
Table lookup is used for $H_n$ whose numerator and denominator
fit in a single limb. For larger $n$, a divide and conquer strategy is used.

# Examples

```jldoctest
julia> a = harmonic(12)
86021//27720
```
"""
function harmonic(n::Int)
  n < 0 && throw(DomainError(n, "Index must be non-negative"))
  c = QQFieldElem()
  @ccall libflint.fmpq_harmonic_ui(c::Ref{QQFieldElem}, n::Int)::Nothing
  return c
end

@doc raw"""
    bernoulli(n::Int)

Return the Bernoulli number $B_n$ for non-negative $n$.

See also [`bernoulli_cache`](@ref).

# Examples

```jldoctest
julia> d = bernoulli(12)
-691//2730
```
"""
function bernoulli(n::Int)
  n < 0 && throw(DomainError(n, "Index must be non-negative"))
  c = QQFieldElem()
  @ccall libflint.bernoulli_fmpq_ui(c::Ref{QQFieldElem}, n::Int)::Nothing
  return c
end

@doc raw"""
    bernoulli_cache(n::Int)

Precomputes and caches all the Bernoulli numbers up to $B_n$.
This is much faster than repeatedly calling `bernoulli(k)`.
Once cached, subsequent calls to `bernoulli(k)` for any $k \le n$
will read from the cache, making them virtually free.

See also [`bernoulli`](@ref).

# Examples

```jldoctest
julia> bernoulli_cache(100)

julia> e = bernoulli(100)
-94598037819122125295227433069493721872702841533066936133385696204311395415197247711//33330
```
"""
function bernoulli_cache(n::Int)
  n = n + 1
  n < 0 && throw(DomainError(n, "Index must be non-negative"))
  @ccall libflint.bernoulli_cache_compute(n::Int)::Nothing
end

@doc raw"""
    dedekind_sum(h::ZZRingElem, k::ZZRingElem)

Return the Dedekind sum $s(h,k)$ for arbitrary $h$ and $k$.

# Examples

```jldoctest
julia> b = dedekind_sum(12, 13)
-11//13

julia> c = dedekind_sum(-120, ZZ(1305))
-575//522
```
"""
function dedekind_sum(h::ZZRingElem, k::ZZRingElem)
  c = QQFieldElem()
  @ccall libflint.fmpq_dedekind_sum(c::Ref{QQFieldElem}, h::Ref{ZZRingElem}, k::Ref{ZZRingElem})::Nothing
  return c
end

dedekind_sum(h::ZZRingElem, k::Integer) = dedekind_sum(h, ZZRingElem(k))

dedekind_sum(h::Integer, k::ZZRingElem) = dedekind_sum(ZZRingElem(h), k)

dedekind_sum(h::Integer, k::Integer) = dedekind_sum(ZZRingElem(h), ZZRingElem(k))

log(a::QQFieldElem) = log(numerator(a)) - log(denominator(a))
log(a::ZZRingElem, b::QQFieldElem) = log(b) / log(a)

###############################################################################
#
#  Simplest between
#
###############################################################################

function _fmpq_simplest_between(l_num::ZZRingElem, l_den::ZZRingElem,
    r_num::ZZRingElem, r_den::ZZRingElem)
  n = ZZRingElem()
  d = ZZRingElem()

  @ccall libflint._fmpq_simplest_between(n::Ref{ZZRingElem}, d::Ref{ZZRingElem}, l_num::Ref{ZZRingElem}, l_den::Ref{ZZRingElem}, r_num::Ref{ZZRingElem}, r_den::Ref{ZZRingElem})::Nothing

  return n//d
end

@doc raw"""
      simplest_between(l::QQFieldElem, r::QQFieldElem)

Return the simplest fraction in the closed interval $[l, r]$. A canonical
fraction $a_1 / b_1$ is defined to be simpler than $a_2 / b_2$ if and only if
$b_1 < b_2$ or $b_1 = b_2$ and $a_1 < a_2$.


# Examples

```jldoctest
julia> simplest_between(QQ(1//10), QQ(3//10))
1//4
```
"""
function simplest_between(l::QQFieldElem, r::QQFieldElem)
  z = QQFieldElem()
  @ccall libflint.fmpq_simplest_between(z::Ref{QQFieldElem}, l::Ref{QQFieldElem}, r::Ref{QQFieldElem})::Nothing
  return z
end


###############################################################################
#
#   Unsafe operators and functions
#
###############################################################################

_num_ptr(c::QQFieldElem) = Ptr{ZZRingElem}(pointer_from_objref(c))
_num_ptr(c::Ptr{QQFieldElem}) = Ptr{ZZRingElem}(c)
_num_ptr(c::Ref{QQFieldElem}) = _num_ptr(c[])
_den_ptr(c::QQFieldElemOrPtr) = _num_ptr(c) + sizeof(ZZRingElem)

function zero!(c::QQFieldElemOrPtr)
  @ccall libflint.fmpq_zero(c::Ref{QQFieldElem})::Nothing
  return c
end

function one!(c::QQFieldElemOrPtr)
  set!(c, 1)
end

function neg!(z::QQFieldElemOrPtr, a::QQFieldElemOrPtr)
  @ccall libflint.fmpq_neg(z::Ref{QQFieldElem}, a::Ref{QQFieldElem})::Nothing
  return z
end

function set!(c::QQFieldElemOrPtr, a::QQFieldElemOrPtr)
  @ccall libflint.fmpq_set(c::Ref{QQFieldElem}, a::Ref{QQFieldElem})::Nothing
  return c
end

function set!(c::QQFieldElemOrPtr, a::Int, b::UInt = UInt(1))
  @ccall libflint.fmpq_set_si(c::Ref{QQFieldElem}, a::Int, b::UInt)::Nothing
  return c
end

function set!(c::QQFieldElemOrPtr, a::UInt, b::UInt = UInt(1))
  @ccall libflint.fmpq_set_ui(c::Ref{QQFieldElem}, a::UInt, b::UInt)::Nothing
  return c
end

function set!(c::QQFieldElemOrPtr, a::ZZRingElemOrPtr, b::ZZRingElemOrPtr)
  @ccall libflint.fmpq_set_fmpz_frac(c::Ref{QQFieldElem}, a::Ref{ZZRingElem}, b::Ref{ZZRingElem})::Nothing
  return c
end

function set!(c::QQFieldElemOrPtr, a::Union{Integer,ZZRingElemOrPtr})
  GC.@preserve c begin
    set!(_num_ptr(c), a)
    one!(_den_ptr(c))
  end
  return c
end

function numerator!(z::ZZRingElemOrPtr, y::QQFieldElemOrPtr)
  GC.@preserve y begin
    set!(z, _num_ptr(y))
  end
  return z
end

function denominator!(z::ZZRingElemOrPtr, y::QQFieldElemOrPtr)
  GC.@preserve y begin
    set!(z, _den_ptr(y))
  end
  return z
end

#

function add!(c::QQFieldElemOrPtr, a::QQFieldElemOrPtr, b::QQFieldElemOrPtr)
  @ccall libflint.fmpq_add(c::Ref{QQFieldElem}, a::Ref{QQFieldElem}, b::Ref{QQFieldElem})::Nothing
  return c
end

function add!(c::QQFieldElemOrPtr, a::QQFieldElemOrPtr, b::ZZRingElemOrPtr)
  @ccall libflint.fmpq_add_fmpz(c::Ref{QQFieldElem}, a::Ref{QQFieldElem}, b::Ref{ZZRingElem})::Nothing
  return c
end

function add!(c::QQFieldElemOrPtr, a::QQFieldElemOrPtr, b::Int)
  @ccall libflint.fmpq_add_si(c::Ref{QQFieldElem}, a::Ref{QQFieldElem}, b::Int)::Nothing
  return c
end

function add!(c::QQFieldElemOrPtr, a::QQFieldElemOrPtr, b::UInt)
  @ccall libflint.fmpq_add_ui(c::Ref{QQFieldElem}, a::Ref{QQFieldElem}, b::Int)::Nothing
  return c
end

add!(c::QQFieldElemOrPtr, a::QQFieldElemOrPtr, b::Union{Integer, Rational}) = add!(c, a, flintify(b))
add!(c::QQFieldElemOrPtr, a::Union{ZZRingElemOrPtr, Integer, Rational}, b::QQFieldElemOrPtr) = add!(c, b, a)

#

function sub!(z::QQFieldElemOrPtr, a::QQFieldElemOrPtr, b::QQFieldElemOrPtr)
  @ccall libflint.fmpq_sub(z::Ref{QQFieldElem}, a::Ref{QQFieldElem}, b::Ref{QQFieldElem})::Nothing
  return z
end

function sub!(z::QQFieldElemOrPtr, a::QQFieldElemOrPtr, b::ZZRingElemOrPtr)
  @ccall libflint.fmpq_sub_fmpz(z::Ref{QQFieldElem}, a::Ref{QQFieldElem}, b::Ref{ZZRingElem})::Nothing
  return z
end

function sub!(z::QQFieldElemOrPtr, a::QQFieldElemOrPtr, b::Int)
  @ccall libflint.fmpq_sub_si(z::Ref{QQFieldElem}, a::Ref{QQFieldElem}, b::Int)::Nothing
  return z
end

function sub!(z::QQFieldElemOrPtr, a::QQFieldElemOrPtr, b::UInt)
  @ccall libflint.fmpq_sub_ui(z::Ref{QQFieldElem}, a::Ref{QQFieldElem}, b::UInt)::Nothing
  return z
end

sub!(c::QQFieldElemOrPtr, a::QQFieldElemOrPtr, b::Union{Integer, Rational}) = sub!(c, a, flintify(b))
sub!(c::QQFieldElemOrPtr, a::Union{ZZRingElemOrPtr, Integer, Rational}, b::QQFieldElemOrPtr) = neg!(sub!(c, b, a))

#

function mul!(c::QQFieldElemOrPtr, a::QQFieldElemOrPtr, b::QQFieldElemOrPtr)
  @ccall libflint.fmpq_mul(c::Ref{QQFieldElem}, a::Ref{QQFieldElem}, b::Ref{QQFieldElem})::Nothing
  return c
end

function mul!(c::QQFieldElemOrPtr, a::QQFieldElemOrPtr, b::ZZRingElemOrPtr)
  @ccall libflint.fmpq_mul_fmpz(c::Ref{QQFieldElem}, a::Ref{QQFieldElem}, b::Ref{ZZRingElem})::Nothing
  return c
end

function mul!(c::QQFieldElemOrPtr, a::QQFieldElemOrPtr, b::Int)
  @ccall libflint.fmpq_mul_si(c::Ref{QQFieldElem}, a::Ref{QQFieldElem}, b::Int)::Nothing
  return c
end

function mul!(c::QQFieldElemOrPtr, a::QQFieldElemOrPtr, b::UInt)
  @ccall libflint.fmpq_mul_ui(c::Ref{QQFieldElem}, a::Ref{QQFieldElem}, b::UInt)::Nothing
  return c
end

mul!(c::QQFieldElemOrPtr, a::QQFieldElemOrPtr, b::Union{Integer, Rational}) = mul!(c, a, flintify(b))
mul!(c::QQFieldElemOrPtr, a::Union{ZZRingElemOrPtr, Integer, Rational}, b::QQFieldElemOrPtr) = mul!(c, b, a)

#

function addmul!(c::QQFieldElemOrPtr, a::QQFieldElemOrPtr, b::QQFieldElemOrPtr)
  @ccall libflint.fmpq_addmul(c::Ref{QQFieldElem}, a::Ref{QQFieldElem}, b::Ref{QQFieldElem})::Nothing
  return c
end

function submul!(c::QQFieldElemOrPtr, a::QQFieldElemOrPtr, b::QQFieldElemOrPtr)
  @ccall libflint.fmpq_submul(c::Ref{QQFieldElem}, a::Ref{QQFieldElem}, b::Ref{QQFieldElem})::Nothing
  return c
end

#

function divexact!(z::QQFieldElemOrPtr, a::QQFieldElemOrPtr, b::QQFieldElemOrPtr)
  @ccall libflint.fmpq_div(z::Ref{QQFieldElem}, a::Ref{QQFieldElem}, b::Ref{QQFieldElem})::Nothing
  return z
end

function divexact!(z::QQFieldElemOrPtr, a::QQFieldElemOrPtr, b::ZZRingElemOrPtr)
  @ccall libflint.fmpq_div_fmpz(z::Ref{QQFieldElem}, a::Ref{QQFieldElem}, b::Ref{ZZRingElem})::Nothing
  return z
end

###############################################################################
#
#   Parent object call overloads
#
###############################################################################

(a::QQField)() = zero(a)

function (a::QQField)(b::Rational)
  # work around Julia bug, https://github.com/JuliaLang/julia/issues/32569
  if denominator(b) < 0
    return QQFieldElem(ZZRingElem(numerator(b)), -ZZRingElem(denominator(b)))
  else
    return QQFieldElem(numerator(b), denominator(b))
  end
end

(a::QQField)(b::Integer) = QQFieldElem(b)

(a::QQField)(b::Int, c::Int) = QQFieldElem(b, c)

(a::QQField)(b::ZZRingElem) = QQFieldElem(b)

(a::QQField)(b::Integer, c::Integer) = QQFieldElem(b, c)

(a::QQField)(b::ZZRingElem, c::Integer) = QQFieldElem(b, c)

(a::QQField)(b::Integer, c::ZZRingElem) = QQFieldElem(b, c)

(a::QQField)(b::ZZRingElem, c::ZZRingElem) = QQFieldElem(b, c)

(a::QQField)(b::QQFieldElem) = b

function (a::ZZRing)(b::QQFieldElem)
  is_one(_den_ptr(b)) || error("Denominator must be 1")
  return numerator(b)
end

function (::ZZRing)(x::Rational{<:IntegerUnion})
  @assert denominator(x) == 1
  return ZZRingElem(numerator(x))
end

###############################################################################
#
#   Random generation
#
###############################################################################

@doc raw"""
    rand_bits(::QQField, b::Int)

Return a random signed rational whose numerator and denominator both have $b$
bits before canonicalisation. Note that the resulting numerator and
denominator can be smaller than $b$ bits.
"""
function rand_bits(::QQField, b::Int)
  b > 0 || throw(DomainError(b, "Bit count must be positive"))
  z = QQFieldElem()
  @ccall libflint.fmpq_randbits(z::Ref{QQFieldElem}, _flint_rand_states[Threads.threadid()]::Ref{rand_ctx}, b::Int)::Nothing
  return z
end

###############################################################################
#
#   Conformance test element generation
#
###############################################################################

function ConformanceTests.generate_element(R::QQField)
  return rand_bits(ZZ, rand(0:100))//rand_bits(ZZ, rand(1:100))
end

###############################################################################
#
#   Conversions and promotions
#
###############################################################################

convert(::Type{QQFieldElem}, a::Integer) = QQFieldElem(a)

convert(::Type{QQFieldElem}, a::Rational) = QQFieldElem(a)

convert(::Type{QQFieldElem}, a::ZZRingElem) = QQFieldElem(a)

Base.promote_rule(::Type{QQFieldElem}, ::Type{T}) where {T <: Integer} = QQFieldElem

Base.promote_rule(::Type{QQFieldElem}, ::Type{ZZRingElem}) = QQFieldElem

promote_rule(::Type{QQFieldElem}, ::Type{ZZRingElem}) = QQFieldElem

promote_rule(::Type{QQFieldElem}, ::Type{T} where {T <: Integer}) = QQFieldElem

promote_rule(::Type{QQFieldElem}, ::Type{Rational{T}} where {T <: Integer}) = QQFieldElem

Base.promote_rule(::Type{QQFieldElem}, ::Type{Rational{T}}) where {T <: Integer} = QQFieldElem

function Base.convert(::Type{Rational{T}}, a::QQFieldElem) where T <: Integer
  return Rational{T}(a)
end

function Base.Rational{T}(z::QQFieldElem) where T <: Integer
  return Rational{T}(T(numerator(z)), T(denominator(z)))
end

function Base.Rational{BigInt}(z::QQFieldElem)
  r = Rational{BigInt}(0)
  @ccall libflint.fmpq_get_mpz_frac(r.num::Ref{BigInt}, r.den::Ref{BigInt}, z::Ref{QQFieldElem})::Nothing
  return r
end

Rational(z::QQFieldElem) = Rational{BigInt}(z)

function Base.Rational{T}(z::ZZRingElem) where T <: Integer
  return Rational{T}(T(z))
end

Rational(z::ZZRingElem) = Rational{BigInt}(z)

function convert(R::Type{Rational{T}}, a::ZZRingElem) where T <: Integer
  return R(T(a))
end

@inline __get_rounding_mode() = Base.Rounding.rounding_raw(BigFloat)

function Base.BigFloat(a::QQFieldElem)
  r = BigFloat(0)
  @ccall libflint.fmpq_get_mpfr(r::Ref{BigFloat}, a::Ref{QQFieldElem}, __get_rounding_mode()::Int32)::Cint
  return r
end

Base.Float64(a::QQFieldElem) = Float64(BigFloat(a))

###############################################################################
#
#   Convenience methods for arithmetics (since `QQFieldElem` and `ZZRingElem` are not `Number` types)
#
###############################################################################

//(v::Vector{QQFieldElem}, x::QQFieldElem) = v .// x
/(v::Vector{QQFieldElem}, x::QQFieldElem) = v ./ x
*(x::QQFieldElem, v::Vector{QQFieldElem}) = x .* v
*(v::Vector{QQFieldElem}, x::QQFieldElem) = v .* x

//(v::Vector{QQFieldElem}, x::ZZRingElem) = v .// x
/(v::Vector{QQFieldElem}, x::ZZRingElem) = v ./ x
*(x::ZZRingElem, v::Vector{QQFieldElem}) = x .* v
*(v::Vector{QQFieldElem}, x::ZZRingElem) = v .* x


###############################################################################
#
#   fraction_field constructor
#
###############################################################################

fraction_field(base::ZZRing) = QQ
