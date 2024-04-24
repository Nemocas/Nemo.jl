###############################################################################
#
#   QQFieldElem.jl : Flint rationals
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
  ccall((:fmpq_set_si, libflint), Nothing, (Ref{QQFieldElem}, Int64, UInt64), r, numerator(a), denominator(a))
  return r
end

QQFieldElem(a::Rational{T}) where {T <: Integer} = QQFieldElem(numerator(a), denominator(a))

QQFieldElem(a::Integer) = QQFieldElem(ZZRingElem(a), ZZRingElem(1))

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
   return _hash_integer(a.num, _hash_integer(a.den, h))
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
   z = ZZRingElem()
   ccall((:fmpq_numerator, libflint), Nothing, (Ref{ZZRingElem}, Ref{QQFieldElem}), z, a)
   return z
end

function denominator(a::QQFieldElem)
   z = ZZRingElem()
   ccall((:fmpq_denominator, libflint), Nothing, (Ref{ZZRingElem}, Ref{QQFieldElem}), z, a)
   return z
end

@doc raw"""
    sign(a::QQFieldElem)

Return the sign of $a$ ($-1$, $0$ or $1$) as a fraction.
"""
sign(a::QQFieldElem) = QQFieldElem(sign(numerator(a)))

sign(::Type{Int}, a::QQFieldElem) = Int(ccall((:fmpq_sgn, libflint), Cint, (Ref{QQFieldElem},), a))

Base.signbit(a::QQFieldElem) = signbit(sign(Int, a))

is_negative(n::QQFieldElem) = sign(Int, n) < 0
is_positive(n::QQFieldElem) = sign(Int, n) > 0

function abs(a::QQFieldElem)
   z = QQFieldElem()
   ccall((:fmpq_abs, libflint), Nothing, (Ref{QQFieldElem}, Ref{QQFieldElem}), z, a)
   return z
end

zero(_::QQField) = QQFieldElem(0)

one(_::QQField) = QQFieldElem(1)

zero(::Type{QQFieldElem}) = QQFieldElem(0)

one(::Type{QQFieldElem}) = QQFieldElem(1)


function isone(a::QQFieldElem)
   return Bool(ccall((:fmpq_is_one, libflint), Cint, (Ref{QQFieldElem}, ), a))
end

function iszero(a::QQFieldElem)
   return Bool(ccall((:fmpq_is_zero, libflint), Cint, (Ref{QQFieldElem}, ), a))
end

is_unit(a::QQFieldElem) = !iszero(a)

isinteger(a::QQFieldElem) = isone(denominator(a))

isfinite(::QQFieldElem) = true

isinf(::QQFieldElem) = false


@doc raw"""
    height(a::QQFieldElem)

Return the height of the fraction $a$, namely the largest of the absolute
values of the numerator and denominator.
"""
function height(a::QQFieldElem)
   temp = ZZRingElem()
   ccall((:fmpq_height, libflint), Nothing, (Ref{ZZRingElem}, Ref{QQFieldElem}), temp, a)
   return temp
end

@doc raw"""
    height_bits(a::QQFieldElem)

Return the number of bits of the height of the fraction $a$.
"""
function height_bits(a::QQFieldElem)
   return ccall((:fmpq_height_bits, libflint), Int, (Ref{QQFieldElem},), a)
end

function deepcopy_internal(a::QQFieldElem, dict::IdDict)
   z = QQFieldElem()
   ccall((:fmpq_set, libflint), Nothing, (Ref{QQFieldElem}, Ref{QQFieldElem}), z, a)
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

function -(a::QQFieldElem)
   z = QQFieldElem()
   ccall((:fmpq_neg, libflint), Nothing, (Ref{QQFieldElem}, Ref{QQFieldElem}), z, a)
   return z
end

###############################################################################
#
#   Binary operators
#
###############################################################################

function +(a::QQFieldElem, b::QQFieldElem)
   z = QQFieldElem()
   ccall((:fmpq_add, libflint), Nothing,
         (Ref{QQFieldElem}, Ref{QQFieldElem}, Ref{QQFieldElem}), z, a, b)
   return z
end

function -(a::QQFieldElem, b::QQFieldElem)
   z = QQFieldElem()
   ccall((:fmpq_sub, libflint), Nothing,
         (Ref{QQFieldElem}, Ref{QQFieldElem}, Ref{QQFieldElem}), z, a, b)
   return z
end

function *(a::QQFieldElem, b::QQFieldElem)
   z = QQFieldElem()
   ccall((:fmpq_mul, libflint), Nothing,
         (Ref{QQFieldElem}, Ref{QQFieldElem}, Ref{QQFieldElem}), z, a, b)
   return z
end

###############################################################################
#
#   Ad hoc binary operators
#
###############################################################################

function +(a::QQFieldElem, b::Int)
   z = QQFieldElem()
   ccall((:fmpq_add_si, libflint), Nothing,
         (Ref{QQFieldElem}, Ref{QQFieldElem}, Int), z, a, b)
   return z
end

function +(a::QQFieldElem, b::ZZRingElem)
   z = QQFieldElem()
   ccall((:fmpq_add_fmpz, libflint), Nothing,
         (Ref{QQFieldElem}, Ref{QQFieldElem}, Ref{ZZRingElem}), z, a, b)
   return z
end

+(a::Int, b::QQFieldElem) = b + a

+(a::ZZRingElem, b::QQFieldElem) = b + a

+(a::QQFieldElem, b::Rational{T}) where {T <: Integer} = a + QQFieldElem(b)

+(a::Rational{T}, b::QQFieldElem) where {T <: Integer} = b + a

function -(a::QQFieldElem, b::Int)
   z = QQFieldElem()
   ccall((:fmpq_sub_si, libflint), Nothing,
         (Ref{QQFieldElem}, Ref{QQFieldElem}, Int), z, a, b)
   return z
end

function -(a::QQFieldElem, b::ZZRingElem)
   z = QQFieldElem()
   ccall((:fmpq_sub_fmpz, libflint), Nothing,
         (Ref{QQFieldElem}, Ref{QQFieldElem}, Ref{ZZRingElem}), z, a, b)
   return z
end

-(a::QQFieldElem, b::Rational{T}) where {T <: Integer} = a - QQFieldElem(b)

-(a::Rational{T}, b::QQFieldElem) where {T <: Integer} = QQFieldElem(a) - b

function *(a::QQFieldElem, b::ZZRingElem)
   z = QQFieldElem()
   ccall((:fmpq_mul_fmpz, libflint), Nothing,
         (Ref{QQFieldElem}, Ref{QQFieldElem}, Ref{ZZRingElem}), z, a, b)
   return z
end

*(a::ZZRingElem, b::QQFieldElem) = b*a

function -(a::ZZRingElem, b::QQFieldElem)
   n = a*denominator(b) - numerator(b)
   d = denominator(b)
   g = gcd(n, d)
   return parent(b)(divexact(n, g), divexact(d, g))
end

*(a::QQFieldElem, b::Rational{T}) where {T <: Integer} = a * QQFieldElem(b)

*(a::Rational{T}, b::QQFieldElem) where {T <: Integer} = b * a

###############################################################################
#
#   Comparison
#
###############################################################################

function ==(a::QQFieldElem, b::QQFieldElem)
   return ccall((:fmpq_equal, libflint), Bool,
                (Ref{QQFieldElem}, Ref{QQFieldElem}), a, b)
end

function isless(a::QQFieldElem, b::QQFieldElem)
   return ccall((:fmpq_cmp, libflint), Cint,
                (Ref{QQFieldElem}, Ref{QQFieldElem}), a, b) < 0
end

###############################################################################
#
#   Ad hoc comparison
#
###############################################################################

function ==(a::QQFieldElem, b::Int)
   return ccall((:fmpq_equal_si, libflint), Bool, (Ref{QQFieldElem}, Int), a, b)
end

==(a::Int, b::QQFieldElem) = b == a

function ==(a::QQFieldElem, b::ZZRingElem)
   return ccall((:fmpq_equal_fmpz, libflint), Bool,
                (Ref{QQFieldElem}, Ref{ZZRingElem}), a, b)
end

==(a::ZZRingElem, b::QQFieldElem) = b == a

==(a::QQFieldElem, b::Rational{T}) where {T <: Integer} = a == QQFieldElem(b)

==(a::Rational{T}, b::QQFieldElem) where {T <: Integer} = b == a

function isless(a::QQFieldElem, b::Integer)
   z = QQFieldElem(b)
   return ccall((:fmpq_cmp, libflint), Cint,
                (Ref{QQFieldElem}, Ref{QQFieldElem}), a, z) < 0
end

function isless(a::Integer, b::QQFieldElem)
   z = QQFieldElem(a)
   return ccall((:fmpq_cmp, libflint), Cint,
                (Ref{QQFieldElem}, Ref{QQFieldElem}), z, b) < 0
end

function isless(a::QQFieldElem, b::ZZRingElem)
   z = QQFieldElem(b)
   return ccall((:fmpq_cmp, libflint), Cint,
                (Ref{QQFieldElem}, Ref{QQFieldElem}), a, z) < 0
end

function isless(a::ZZRingElem, b::QQFieldElem)
   z = QQFieldElem(a)
   return ccall((:fmpq_cmp, libflint), Cint,
                (Ref{QQFieldElem}, Ref{QQFieldElem}), z, b) < 0
end

isless(a::Rational{T}, b::QQFieldElem) where {T <: Integer} = isless(QQFieldElem(a), b)

isless(a::QQFieldElem, b::Rational{T}) where {T <: Integer} = isless(a, QQFieldElem(b))

###############################################################################
#
#   Powering
#
###############################################################################

function ^(a::QQFieldElem, b::Int)
   iszero(a) && b < 0 && throw(DivideError())
   temp = QQFieldElem()
   ccall((:fmpq_pow_si, libflint), Nothing,
         (Ref{QQFieldElem}, Ref{QQFieldElem}, Int), temp, a, b)
   return temp
end

function ^(a::QQFieldElem, k::ZZRingElem)
   if a == 0
      if k == 0
         return QQFieldElem(1)
      end
      return QQFieldElem(0)
   end

   if a == 1
      return QQFieldElem(1)
   end
   if a == -1
      if isodd(k)
         return QQFieldElem(-1)
      else
         return QQFieldElem(1)
      end
   end
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
   ccall((:fmpq_div_2exp, libflint), Nothing,
         (Ref{QQFieldElem}, Ref{QQFieldElem}, Int), z, a, b)
   return z
end

@doc raw"""
    <<(a::QQFieldElem, b::Int)

Return $a \times 2^b$.
"""
function <<(a::QQFieldElem, b::Int)
   z = QQFieldElem()
   ccall((:fmpq_mul_2exp, libflint), Nothing,
         (Ref{QQFieldElem}, Ref{QQFieldElem}, Int), z, a, b)
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
#   Inversion
#
###############################################################################


function inv(a::QQFieldElem)
    if iszero(a)
       error("Element not invertible")
    end
    z = QQFieldElem()
    ccall((:fmpq_inv, libflint), Nothing, (Ref{QQFieldElem}, Ref{QQFieldElem}), z, a)
    return z
 end

###############################################################################
#
#   Exact division
#
###############################################################################

function divexact(a::QQFieldElem, b::QQFieldElem; check::Bool=true)
   iszero(b) && throw(DivideError())
   z = QQFieldElem()
   ccall((:fmpq_div, libflint), Nothing,
         (Ref{QQFieldElem}, Ref{QQFieldElem}, Ref{QQFieldElem}), z, a, b)
   return z
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
   z = QQFieldElem()
   ccall((:fmpq_div_fmpz, libflint), Nothing,
         (Ref{QQFieldElem}, Ref{QQFieldElem}, Ref{ZZRingElem}), z, a, b)
   return z
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
   ccall((:fmpq_mod_fmpz, libflint), Nothing,
         (Ref{ZZRingElem}, Ref{QQFieldElem}, Ref{ZZRingElem}), z, a, b)
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
   ccall((:fmpq_gcd, libflint), Nothing,
         (Ref{QQFieldElem}, Ref{QQFieldElem}, Ref{QQFieldElem}), z, a, b)
   return z
end

################################################################################
#
#   Ad hoc Remove and valuation
#
################################################################################

remove(a::QQFieldElem, b::Integer) = remove(a, ZZRingElem(b))

valuation(a::QQFieldElem, b::Integer) = valuation(a, ZZRingElem(b))

function remove!(a::QQFieldElem, b::ZZRingElem)
   nr = ccall((:fmpq_numerator_ptr, libflint), Ptr{ZZRingElem}, (Ref{QQFieldElem},), a)
   vn = ccall((:fmpz_remove, libflint), Clong, (Ptr{ZZRingElem}, Ptr{ZZRingElem}, Ref{ZZRingElem}), nr, nr, b)
   #QQFieldElem's are simplified: either num OR den will be non-trivial
   if vn != 0
      return vn, a
   end
   nr = ccall((:fmpq_denominator_ptr, libflint), Ptr{ZZRingElem}, (Ref{QQFieldElem},), a)
   vn = ccall((:fmpz_remove, libflint), Clong, (Ptr{ZZRingElem}, Ptr{ZZRingElem}, Ref{ZZRingElem}), nr, nr, b)
   return -vn, a
end

function valuation!(a::QQFieldElem, b::ZZRingElem)
   nr = ccall((:fmpq_numerator_ptr, libflint), Ptr{ZZRingElem}, (Ref{QQFieldElem},), a)
   vn = ccall((:fmpz_remove, libflint), Clong, (Ptr{ZZRingElem}, Ptr{ZZRingElem}, Ref{ZZRingElem}), nr, nr, b)
   #QQFieldElem's are simplified: either num OR den will be non-trivial
   if vn != 0
      return vn
   end
   nr = ccall((:fmpq_denominator_ptr, libflint), Ptr{ZZRingElem}, (Ref{QQFieldElem},), a)
   vn = ccall((:fmpz_remove, libflint), Clong, (Ptr{ZZRingElem}, Ptr{ZZRingElem}, Ref{ZZRingElem}), nr, nr, b)
   return -vn
end

###############################################################################
#
#   Rational reconstruction
#
###############################################################################

@doc raw"""
    reconstruct(a::ZZRingElem, b::ZZRingElem)
    reconstruct(a::ZZRingElem, b::Integer)
    reconstruct(a::Integer, b::ZZRingElem)
    reconstruct(a::Integer, b::Integer)

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
function reconstruct(a::ZZRingElem, b::ZZRingElem)
   c = QQFieldElem()
   if !Bool(ccall((:fmpq_reconstruct_fmpz, libflint), Cint,
                  (Ref{QQFieldElem}, Ref{ZZRingElem}, Ref{ZZRingElem}), c, a, b))
      error("Impossible rational reconstruction")
   end
   return c
end

reconstruct(a::ZZRingElem, b::Integer) =  reconstruct(a, ZZRingElem(b))

reconstruct(a::Integer, b::ZZRingElem) =  reconstruct(ZZRingElem(a), b)

reconstruct(a::Integer, b::Integer) =  reconstruct(ZZRingElem(a), ZZRingElem(b))

@doc raw"""
   unsafe_reconstruct(a::ZZRingElem, b::ZZRingElem)
Same as [`reconstruct`](@ref), but does not throw if the reconstruction fails.
Returns a tuple (`success`, `n/d`), where `success` signals the success of reconstruction.
"""
function unsafe_reconstruct(a::ZZRingElem, b::ZZRingElem)
   c = QQFieldElem()
   success = Bool(ccall((:fmpq_reconstruct_fmpz, libflint), Cint,
                     (Ref{QQFieldElem}, Ref{ZZRingElem}, Ref{ZZRingElem}), c, a, b))
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
   ccall((:fmpq_next_minimal, libflint), Nothing, (Ref{QQFieldElem}, Ref{QQFieldElem}), c, a)
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
   ccall((:fmpq_next_signed_minimal, libflint), Nothing,
         (Ref{QQFieldElem}, Ref{QQFieldElem}), c, a)
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
   ccall((:fmpq_next_calkin_wilf, libflint), Nothing,
         (Ref{QQFieldElem}, Ref{QQFieldElem}), c, a)
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
   ccall((:fmpq_next_signed_calkin_wilf, libflint), Nothing,
         (Ref{QQFieldElem}, Ref{QQFieldElem}), c, a)
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
   ccall((:fmpq_harmonic_ui, libflint), Nothing, (Ref{QQFieldElem}, Int), c, n)
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
   ccall((:bernoulli_fmpq_ui, libarb), Nothing, (Ref{QQFieldElem}, Int), c, n)
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
   ccall((:bernoulli_cache_compute, libarb), Nothing, (Int,), n)
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
   ccall((:fmpq_dedekind_sum, libflint), Nothing,
         (Ref{QQFieldElem}, Ref{ZZRingElem}, Ref{ZZRingElem}), c, h, k)
   return c
end

dedekind_sum(h::ZZRingElem, k::Integer) = dedekind_sum(h, ZZRingElem(k))

dedekind_sum(h::Integer, k::ZZRingElem) = dedekind_sum(ZZRingElem(h), k)

dedekind_sum(h::Integer, k::Integer) = dedekind_sum(ZZRingElem(h), ZZRingElem(k))

###############################################################################
#
#  Simplest between
#
###############################################################################

function _fmpq_simplest_between(l_num::ZZRingElem, l_den::ZZRingElem,
                                r_num::ZZRingElem, r_den::ZZRingElem)
   n = ZZRingElem()
   d = ZZRingElem()

   ccall((:_fmpq_simplest_between, libflint), Nothing,
         (Ref{ZZRingElem}, Ref{ZZRingElem}, Ref{ZZRingElem}, Ref{ZZRingElem}, Ref{ZZRingElem}, Ref{ZZRingElem}),
         n, d, l_num, l_den, r_num, r_den)

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
   ccall((:fmpq_simplest_between, libflint), Nothing,
         (Ref{QQFieldElem}, Ref{QQFieldElem}, Ref{QQFieldElem}), z, l, r)
   return z
end


###############################################################################
#
#   Unsafe operators and functions
#
###############################################################################

function zero!(c::QQFieldElem)
   ccall((:fmpq_zero, libflint), Nothing,
         (Ref{QQFieldElem},), c)
   return c
end

function mul!(c::QQFieldElem, a::QQFieldElem, b::QQFieldElem)
   ccall((:fmpq_mul, libflint), Nothing,
         (Ref{QQFieldElem}, Ref{QQFieldElem}, Ref{QQFieldElem}), c, a, b)
   return c
end

function mul!(c::QQFieldElem, a::QQFieldElem, b::ZZRingElem)
   ccall((:fmpq_mul_fmpz, libflint), Nothing,
         (Ref{QQFieldElem}, Ref{QQFieldElem}, Ref{ZZRingElem}), c, a, b)
   return c
end

mul!(c::QQFieldElem, a::ZZRingElem, b::QQFieldElem) = mul!(c, b, a)

function mul!(c::QQFieldElem, a::QQFieldElem, b::Int)
   ccall((:fmpq_mul_si, libflint), Nothing,
         (Ref{QQFieldElem}, Ref{QQFieldElem}, Int), c, a, b)
   return c
end

mul!(c::QQFieldElem, a::Int, b::QQFieldElem) = mul!(c, b, a)


function addmul!(c::QQFieldElem, a::QQFieldElem, b::QQFieldElem)
   ccall((:fmpq_addmul, libflint), Nothing,
         (Ref{QQFieldElem}, Ref{QQFieldElem}, Ref{QQFieldElem}), c, a, b)
   return c
end


addeq!(c::QQFieldElem, a::QQFieldElem) = add!(c, c, a)

function add!(c::QQFieldElem, a::QQFieldElem, b::QQFieldElem)
   ccall((:fmpq_add, libflint), Nothing,
         (Ref{QQFieldElem}, Ref{QQFieldElem}, Ref{QQFieldElem}), c, a, b)
   return c
end

function add!(c::QQFieldElem, a::QQFieldElem, b::ZZRingElem)
   ccall((:fmpq_add_fmpz, libflint), Nothing,
         (Ref{QQFieldElem}, Ref{QQFieldElem}, Ref{ZZRingElem}), c, a, b)
   return c
end

add!(c::QQFieldElem, a::ZZRingElem, b::QQFieldElem) = add!(c, b, a)

function add!(c::QQFieldElem, a::QQFieldElem, b::Int)
   ccall((:fmpq_add_si, libflint), Nothing,
      (Ref{QQFieldElem}, Ref{QQFieldElem}, Int), c, a, b)
   return c
end

add!(c::QQFieldElem, a::Int, b::QQFieldElem) = add!(c, b, a)

@inline function sub!(z::QQFieldElem, a::QQFieldElem, b::QQFieldElem)
   ccall((:fmpq_sub, libflint), Nothing, (Ref{QQFieldElem}, Ref{QQFieldElem}, Ref{QQFieldElem}), z, a, b)
   return z
end

@inline function sub!(z::QQFieldElem, a::QQFieldElem, b::ZZRingElem)
   ccall((:fmpq_sub_fmpz, libflint), Nothing,
      (Ref{QQFieldElem}, Ref{QQFieldElem}, Ref{ZZRingElem}), z, a, b)
   return z
end

@inline function neg!(z::QQFieldElem, a::QQFieldElem)
   ccall((:fmpq_neg, libflint), Nothing, (Ref{QQFieldElem}, Ref{QQFieldElem}), z, a)
   return z
end

###############################################################################
#
#   Parent object call overloads
#
###############################################################################

(a::QQField)() = QQFieldElem(ZZRingElem(0), ZZRingElem(1))

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
   z = ZZRingElem()
   ccall((:fmpq_denominator, libflint), Nothing, (Ref{ZZRingElem}, Ref{QQFieldElem}), z, b)
   isone(z) || error("Denominator must be 1")
   ccall((:fmpq_numerator, libflint), Nothing, (Ref{ZZRingElem}, Ref{QQFieldElem}), z, b)
   return z
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
   ccall((:fmpq_randbits, libflint), Nothing, (Ref{QQFieldElem}, Ptr{Cvoid}, Int),
         z, _flint_rand_states[Threads.threadid()].ptr, b)
   return z
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
   ccall((:fmpq_get_mpz_frac, libflint), Nothing,
         (Ref{BigInt}, Ref{BigInt}, Ref{QQFieldElem}), r.num, r.den, z)
   return r
end

Rational(z::QQFieldElem) = Rational{BigInt}(z)

function Base.Rational{T}(z::ZZRingElem) where T <: Integer
   return Rational{T}(T(z))
end

Rational(z::ZZRingElem) = Rational{BigInt}(z)


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
