###############################################################################
#
#   fmpq.jl : Flint rationals
#
###############################################################################

export fmpq, FlintQQ, FractionField, Rational, FlintRationalField, height,
       height_bits, isless, reconstruct, next_minimal, next_signed_minimal,
       next_calkin_wilf, next_signed_calkin_wilf, dedekind_sum, harmonic,
       bernoulli, bernoulli_cache, rand_bits, simplest_between

###############################################################################
#
#   Data type and parent methods
#
###############################################################################

fmpq(a::Rational{BigInt}) = fmpq(fmpz(a.num), fmpz(a.den))

function fmpq(a::Rational{Int})
  r = fmpq()
  ccall((:fmpq_set_si, libflint), Nothing, (Ref{fmpq}, Int64, UInt64), r, numerator(a), denominator(a))
  return r
end

fmpq(a::Rational{T}) where {T <: Integer} = fmpq(numerator(a), denominator(a))

fmpq(a::Integer) = fmpq(fmpz(a), fmpz(1))

fmpq(a::Integer, b::Integer) = fmpq(fmpz(a), fmpz(b))

fmpq(a::fmpz, b::Integer) = fmpq(a, fmpz(b))

fmpq(a::Integer, b::fmpz) = fmpq(fmpz(a), b)

parent(a::fmpq) = FlintQQ

parent_type(::Type{fmpq}) = FlintRationalField

elem_type(::Type{FlintRationalField}) = fmpq

base_ring(a::FlintRationalField) = FlintZZ

base_ring(a::fmpq) = FlintZZ

isdomain_type(::Type{fmpq}) = true

###############################################################################
#
#   Hashing
#
###############################################################################

function Base.hash(a::fmpq, h::UInt)
   return _hash_integer(a.num, _hash_integer(a.den, h))
end

###############################################################################
#
#   Constructors
#
###############################################################################

function //(x::fmpz, y::fmpz)
   iszero(y) && throw(DivideError())
   g = gcd(x, y)
   return fmpq(divexact(x, g), divexact(y, g))
end

//(x::fmpz, y::Integer) = x//fmpz(y)

//(x::Integer, y::fmpz) = fmpz(x)//y

###############################################################################
#
#   Basic manipulation
#
###############################################################################

function numerator(a::fmpq)
   z = fmpz()
   ccall((:fmpq_numerator, libflint), Nothing, (Ref{fmpz}, Ref{fmpq}), z, a)
   return z
end

function denominator(a::fmpq)
   z = fmpz()
   ccall((:fmpq_denominator, libflint), Nothing, (Ref{fmpz}, Ref{fmpq}), z, a)
   return z
end

@doc Markdown.doc"""
    sign(a::fmpq)

Return the sign of $a$ ($-1$, $0$ or $1$) as a fraction.
"""
sign(a::fmpq) = fmpq(sign(numerator(a)))

@doc Markdown.doc"""
    abs(a::fmpq)

Return the absolute value of $a$.
"""
function abs(a::fmpq)
   z = fmpq()
   ccall((:fmpq_abs, libflint), Nothing, (Ref{fmpq}, Ref{fmpq}), z, a)
   return z
end

zero(a::FlintRationalField) = fmpq(0)

# Exists only to support Julia functionality (no guarantees)
zero(::Type{fmpq}) = fmpq(0)

one(a::FlintRationalField) = fmpq(1)

function isone(a::fmpq)
   return Bool(ccall((:fmpq_is_one, libflint), Cint, (Ref{fmpq}, ), a))
end

function iszero(a::fmpq)
   return Bool(ccall((:fmpq_is_zero, libflint), Cint, (Ref{fmpq}, ), a))
end

isunit(a::fmpq) = !iszero(a)

@doc Markdown.doc"""
    height(a::fmpq)

Return the height of the fraction $a$, namely the largest of the absolute
values of the numerator and denominator.
"""
function height(a::fmpq)
   temp = fmpz()
   ccall((:fmpq_height, libflint), Nothing, (Ref{fmpz}, Ref{fmpq}), temp, a)
   return temp
end

@doc Markdown.doc"""
    height_bits(a::fmpq)

Return the number of bits of the height of the fraction $a$.
"""
function height_bits(a::fmpq)
   return ccall((:fmpq_height_bits, libflint), Int, (Ref{fmpq},), a)
end

function deepcopy_internal(a::fmpq, dict::IdDict)
   z = fmpq()
   ccall((:fmpq_set, libflint), Nothing, (Ref{fmpq}, Ref{fmpq}), z, a)
   return z
end

characteristic(::FlintRationalField) = 0

@doc Markdown.doc"""
    floor(a::fmpq)

Returns the greatest integer that is less than or equal to $a$. The result is
returned as a rational with denominator $1$.
"""
Base.floor(a::fmpq) = fmpq(fdiv(numerator(a), denominator(a)), 1)

@doc Markdown.doc"""
    ceil(a::fmpq)

Returns the least integer that is greater than or equal to $a$. The result is
returned as a rational with denominator $1$.
"""
Base.ceil(a::fmpq) = fmpq(cdiv(numerator(a), denominator(a)), 1)

###############################################################################
#
#   Canonicalisation
#
###############################################################################

canonical_unit(a::fmpq) = a

###############################################################################
#
#   AbstractString I/O
#
###############################################################################

function expressify(a::fmpq; context = nothing)::Any
    n = numerator(a)
    d = denominator(a)
    if isone(d)
        return n
    else
        return Expr(:call, ://, n, d)
    end
end

function show(io::IO, a::fmpq)
   print(io, numerator(a))
   if denominator(a) != 1
      print(io, "//", denominator(a))
   end
end

function show(io::IO, a::FlintRationalField)
   print(io, "Rational Field")
end

needs_parentheses(x::fmpq) = false

###############################################################################
#
#   Unary operators
#
###############################################################################

function -(a::fmpq)
   z = fmpq()
   ccall((:fmpq_neg, libflint), Nothing, (Ref{fmpq}, Ref{fmpq}), z, a)
   return z
end

###############################################################################
#
#   Binary operators
#
###############################################################################

function +(a::fmpq, b::fmpq)
   z = fmpq()
   ccall((:fmpq_add, libflint), Nothing,
         (Ref{fmpq}, Ref{fmpq}, Ref{fmpq}), z, a, b)
   return z
end

function -(a::fmpq, b::fmpq)
   z = fmpq()
   ccall((:fmpq_sub, libflint), Nothing,
         (Ref{fmpq}, Ref{fmpq}, Ref{fmpq}), z, a, b)
   return z
end

function *(a::fmpq, b::fmpq)
   z = fmpq()
   ccall((:fmpq_mul, libflint), Nothing,
         (Ref{fmpq}, Ref{fmpq}, Ref{fmpq}), z, a, b)
   return z
end

###############################################################################
#
#   Ad hoc binary operators
#
###############################################################################

function +(a::fmpq, b::Int)
   z = fmpq()
   ccall((:fmpq_add_si, libflint), Nothing,
         (Ref{fmpq}, Ref{fmpq}, Int), z, a, b)
   return z
end

function +(a::fmpq, b::fmpz)
   z = fmpq()
   ccall((:fmpq_add_fmpz, libflint), Nothing,
         (Ref{fmpq}, Ref{fmpq}, Ref{fmpz}), z, a, b)
   return z
end

+(a::Int, b::fmpq) = b + a

+(a::fmpz, b::fmpq) = b + a

+(a::fmpq, b::Rational{T}) where {T <: Integer} = a + fmpq(b)

+(a::Rational{T}, b::fmpq) where {T <: Integer} = b + a

function -(a::fmpq, b::Int)
   z = fmpq()
   ccall((:fmpq_sub_si, libflint), Nothing,
         (Ref{fmpq}, Ref{fmpq}, Int), z, a, b)
   return z
end

function -(a::fmpq, b::fmpz)
   z = fmpq()
   ccall((:fmpq_sub_fmpz, libflint), Nothing,
         (Ref{fmpq}, Ref{fmpq}, Ref{fmpz}), z, a, b)
   return z
end

-(a::fmpq, b::Rational{T}) where {T <: Integer} = a - fmpq(b)

-(a::Rational{T}, b::fmpq) where {T <: Integer} = fmpq(a) - b

function *(a::fmpq, b::fmpz)
   z = fmpq()
   ccall((:fmpq_mul_fmpz, libflint), Nothing,
         (Ref{fmpq}, Ref{fmpq}, Ref{fmpz}), z, a, b)
   return z
end

*(a::fmpz, b::fmpq) = b*a

function -(a::fmpz, b::fmpq)
   n = a*denominator(b) - numerator(b)
   d = denominator(b)
   g = gcd(n, d)
   return parent(b)(divexact(n, g), divexact(d, g))
end

*(a::fmpq, b::Rational{T}) where {T <: Integer} = a * fmpq(b)

*(a::Rational{T}, b::fmpq) where {T <: Integer} = b * a

//(a::fmpq, b::Rational{T}) where {T <: Integer} = a//fmpq(b)

###############################################################################
#
#   Comparison
#
###############################################################################

function ==(a::fmpq, b::fmpq)
   return ccall((:fmpq_equal, libflint), Bool,
                (Ref{fmpq}, Ref{fmpq}), a, b)
end

@doc Markdown.doc"""
    isless(a::fmpq, b::fmpq)

Return `true` if $a < b$, otherwise return `false`.
"""
function isless(a::fmpq, b::fmpq)
   return ccall((:fmpq_cmp, libflint), Cint,
                (Ref{fmpq}, Ref{fmpq}), a, b) < 0
end

###############################################################################
#
#   Ad hoc comparison
#
###############################################################################

function ==(a::fmpq, b::Int)
   return ccall((:fmpq_equal_si, libflint), Bool, (Ref{fmpq}, Int), a, b)
end

==(a::Int, b::fmpq) = b == a

function ==(a::fmpq, b::fmpz)
   return ccall((:fmpq_equal_fmpz, libflint), Bool,
                (Ref{fmpq}, Ref{fmpz}), a, b)
end

==(a::fmpz, b::fmpq) = b == a

==(a::fmpq, b::Rational{T}) where {T <: Integer} = a == fmpq(b)

==(a::Rational{T}, b::fmpq) where {T <: Integer} = b == a

@doc Markdown.doc"""
    isless(a::fmpq, b::Integer)

Return `true` if $a < b$, otherwise return `false`.
"""
function isless(a::fmpq, b::Integer)
   z = fmpq(b)
   return ccall((:fmpq_cmp, libflint), Cint,
                (Ref{fmpq}, Ref{fmpq}), a, z) < 0
end

@doc Markdown.doc"""
    isless(a::Integer, b::fmpq)

Return `true` if $a < b$, otherwise return `false`.
"""
function isless(a::Integer, b::fmpq)
   z = fmpq(a)
   return ccall((:fmpq_cmp, libflint), Cint,
                (Ref{fmpq}, Ref{fmpq}), z, b) < 0
end

@doc Markdown.doc"""
    isless(a::fmpq, b::fmpz)

Return `true` if $a < b$, otherwise return `false`.
"""
function isless(a::fmpq, b::fmpz)
   z = fmpq(b)
   return ccall((:fmpq_cmp, libflint), Cint,
                (Ref{fmpq}, Ref{fmpq}), a, z) < 0
end

@doc Markdown.doc"""
    isless(a::fmpz, b::fmpq)

Return `true` if $a < b$, otherwise return `false`.
"""
function isless(a::fmpz, b::fmpq)
   z = fmpq(a)
   return ccall((:fmpq_cmp, libflint), Cint,
                (Ref{fmpq}, Ref{fmpq}), z, b) < 0
end

isless(a::Rational{T}, b::fmpq) where {T <: Integer} = isless(fmpq(a), b)

isless(a::fmpq, b::Rational{T}) where {T <: Integer} = isless(a, fmpq(b))

###############################################################################
#
#   Powering
#
###############################################################################

function ^(a::fmpq, b::Int)
   iszero(a) && b < 0 && throw(DivideError())
   temp = fmpq()
   ccall((:fmpq_pow_si, libflint), Nothing,
         (Ref{fmpq}, Ref{fmpq}, Int), temp, a, b)
   return temp
end

###############################################################################
#
#   Shifting
#
###############################################################################

@doc Markdown.doc"""
    >>(a::fmpq, b::Int)

Return $2^b/a$.
"""
function >>(a::fmpq, b::Int)
   z = fmpq()
   ccall((:fmpq_div_2exp, libflint), Nothing,
         (Ref{fmpq}, Ref{fmpq}, Int), z, a, b)
   return z
end

@doc Markdown.doc"""
    <<(a::fmpq, b::Int)

Return $2^b\times a$.
"""
function <<(a::fmpq, b::Int)
   z = fmpq()
   ccall((:fmpq_mul_2exp, libflint), Nothing,
         (Ref{fmpq}, Ref{fmpq}, Int), z, a, b)
   return z
end

###############################################################################
#
#   Square root
#
###############################################################################

function Base.sqrt(a::fmpq)
    snum = sqrt(numerator(a))
    sden = sqrt(denominator(a))
    return fmpq(snum, sden)
 end

###############################################################################
#
#   Inversion
#
###############################################################################


function inv(a::fmpq)
    if iszero(a)
       throw(error("Element not invertible"))
    end
    z = fmpq()
    ccall((:fmpq_inv, libflint), Nothing, (Ref{fmpq}, Ref{fmpq}), z, a)
    return z
 end

 ###############################################################################
#
#   Exact division
#
###############################################################################

function divexact(a::fmpq, b::fmpq)
   iszero(b) && throw(DivideError())
   z = fmpq()
   ccall((:fmpq_div, libflint), Nothing,
         (Ref{fmpq}, Ref{fmpq}, Ref{fmpq}), z, a, b)
   return z
end

div(a::fmpq, b::fmpq) = divexact(a, b)

function rem(a::fmpq, b::fmpq)
   iszero(b) && throw(DivideError())
   return fmpq(0)
end

###############################################################################
#
#   Ad hoc exact division
#
###############################################################################

function divexact(a::fmpq, b::fmpz)
   iszero(b) && throw(DivideError())
   z = fmpq()
   ccall((:fmpq_div_fmpz, libflint), Nothing,
         (Ref{fmpq}, Ref{fmpq}, Ref{fmpz}), z, a, b)
   return z
end

function divexact(a::fmpz, b::fmpq)
   iszero(b) && throw(DivideError())
   return inv(b)*a
end

divexact(a::fmpq, b::Integer) = divexact(a, fmpz(b))

function divexact(a::Integer, b::fmpq)
   iszero(b) && throw(DivideError())
   return inv(b)*a
end

divexact(a::fmpq, b::Rational{T}) where {T <: Integer} = divexact(a, fmpq(b))

divexact(a::Rational{T}, b::fmpq) where {T <: Integer} = divexact(fmpq(a), b)

###############################################################################
#
#   Modular arithmetic
#
###############################################################################

@doc Markdown.doc"""
    mod(a::fmpq, b::fmpz)

Return $a \pmod{b}$ where $b$ is an integer coprime to the denominator of
$a$.
"""
function mod(a::fmpq, b::fmpz)
   iszero(b) && throw(DivideError())
   z = fmpz()
   ccall((:fmpq_mod_fmpz, libflint), Nothing,
         (Ref{fmpz}, Ref{fmpq}, Ref{fmpz}), z, a, b)
   return z
end

@doc Markdown.doc"""
    mod(a::fmpq, b::Integer)

Return $a \pmod{b}$ where $b$ is an integer coprime to the denominator of
$a$.
"""
mod(a::fmpq, b::Integer) = mod(a, fmpz(b))

###############################################################################
#
#   GCD
#
###############################################################################

function gcd(a::fmpq, b::fmpq)
   z = fmpq()
   ccall((:fmpq_gcd, libflint), Nothing,
         (Ref{fmpq}, Ref{fmpq}, Ref{fmpq}), z, a, b)
   return z
end

################################################################################
#
#   Ad hoc Remove and valuation
#
################################################################################

remove(a::fmpq, b::Integer) = remove(a, fmpz(b))

valuation(a::fmpq, b::Integer) = valuation(a, fmpz(b))

###############################################################################
#
#   Rational reconstruction
#
###############################################################################

@doc Markdown.doc"""
    reconstruct(a::fmpz, b::fmpz)

Attempt to find a rational number $n/d$ such that
$0 \leq |n| \leq \lfloor\sqrt{m/2}\rfloor$ and
$0 < d \leq \lfloor\sqrt{m/2}\rfloor$ such that gcd$(n, d) = 1$ and
$a \equiv nd^{-1} \pmod{m}$. If no solution exists, an exception is thrown.
"""
function reconstruct(a::fmpz, b::fmpz)
   c = fmpq()
   if !Bool(ccall((:fmpq_reconstruct_fmpz, libflint), Cint,
                  (Ref{fmpq}, Ref{fmpz}, Ref{fmpz}), c, a, b))
      error("Impossible rational reconstruction")
   end
   return c
end

@doc Markdown.doc"""
    reconstruct(a::fmpz, b::Integer)

Attempt to find a rational number $n/d$ such that
$0 \leq |n| \leq \lfloor\sqrt{m/2}\rfloor$ and
$0 < d \leq \lfloor\sqrt{m/2}\rfloor$ such that gcd$(n, d) = 1$ and
$a \equiv nd^{-1} \pmod{m}$. If no solution exists, an exception is thrown.
"""
reconstruct(a::fmpz, b::Integer) =  reconstruct(a, fmpz(b))

@doc Markdown.doc"""
    reconstruct(a::Integer, b::fmpz)

Attempt to find a rational number $n/d$ such that
$0 \leq |n| \leq \lfloor\sqrt{m/2}\rfloor$ and
$0 < d \leq \lfloor\sqrt{m/2}\rfloor$ such that gcd$(n, d) = 1$ and
$a \equiv nd^{-1} \pmod{m}$. If no solution exists, an exception is thrown.
"""
reconstruct(a::Integer, b::fmpz) =  reconstruct(fmpz(a), b)

@doc Markdown.doc"""
    reconstruct(a::Integer, b::Integer)

Attempt to find a rational number $n/d$ such that
$0 \leq |n| \leq \lfloor\sqrt{m/2}\rfloor$ and
$0 < d \leq \lfloor\sqrt{m/2}\rfloor$ such that gcd$(n, d) = 1$ and
$a \equiv nd^{-1} \pmod{m}$. If no solution exists, an exception is thrown.
"""
reconstruct(a::Integer, b::Integer) =  reconstruct(fmpz(a), fmpz(b))

###############################################################################
#
#   Rational enumeration
#
###############################################################################

@doc Markdown.doc"""
    next_minimal(a::fmpq)

Given $a$, returns the next rational number in the sequence obtained by
enumerating all positive denominators $q$, and for each $q$ enumerating
the numerators $1 \le p < q$ in order and generating both $p/q$ and $q/p$,
but skipping all gcd$(p,q) \neq 1$. Starting with zero, this generates
every nonnegative rational number once and only once, with the first
few entries being $0, 1, 1/2, 2, 1/3, 3, 2/3, 3/2, 1/4, 4, 3/4, 4/3, \ldots$.
This enumeration produces the rational numbers in order of minimal height.
It has the disadvantage of being somewhat slower to compute than the
Calkin-Wilf enumeration. If $a < 0$ we throw a `DomainError()`.
"""
function next_minimal(a::fmpq)
   a < 0 && throw(DomainError(a, "Argument must be non-negative"))
   c = fmpq()
   ccall((:fmpq_next_minimal, libflint), Nothing, (Ref{fmpq}, Ref{fmpq}), c, a)
   return c
end

@doc Markdown.doc"""
    next_signed_minimal(a::fmpq)

Given a signed rational number $a$ assumed to be in canonical form,
returns the next element in the minimal-height sequence generated by
`next_minimal` but with negative numbers interleaved. The sequence begins
$0, 1, -1, 1/2, -1/2, 2, -2, 1/3, -1/3, \ldots$. Starting with zero, this
generates every rational number once and only once, in order of minimal
height.
"""
function next_signed_minimal(a::fmpq)
   c = fmpq()
   ccall((:fmpq_next_signed_minimal, libflint), Nothing,
         (Ref{fmpq}, Ref{fmpq}), c, a)
   return c
end

@doc Markdown.doc"""
    next_calkin_wilf(a::fmpq)

Given $a$ return the next number in the breadth-first traversal of the
Calkin-Wilf tree. Starting with zero, this generates every nonnegative
rational number once and only once, with the first few entries being
$0, 1, 1/2, 2, 1/3, 3/2, 2/3, 3, 1/4, 4/3, 3/5, 5/2, 2/5, \ldots$.
Despite the appearance of the initial entries, the Calkin-Wilf enumeration
does not produce the rational numbers in order of height: some small
fractions will appear late in the sequence. This order has the advantage of
being faster to produce than the minimal-height order.
"""
function next_calkin_wilf(a::fmpq)
   a < 0 && throw(DomainError(a, "Argument must be non-negative"))
   c = fmpq()
   ccall((:fmpq_next_calkin_wilf, libflint), Nothing,
         (Ref{fmpq}, Ref{fmpq}), c, a)
   return c
end

@doc Markdown.doc"""
    next_signed_calkin_wilf(a::fmpq)

Given a signed rational number $a$ returns the next element in the
Calkin-Wilf sequence with negative numbers interleaved. The sequence begins
$0, 1, -1, 1/2, -1/2, 2, -2, 1/3, -1/3, \ldots$. Starting with zero, this
generates every rational number once and only once, but not in order of
minimal height.
"""
function next_signed_calkin_wilf(a::fmpq)
   c = fmpq()
   ccall((:fmpq_next_signed_calkin_wilf, libflint), Nothing,
         (Ref{fmpq}, Ref{fmpq}), c, a)
   return c
end

###############################################################################
#
#   Special functions
#
###############################################################################

@doc Markdown.doc"""
    harmonic(n::Int)

Computes the harmonic number $H_n = 1 + 1/2 + 1/3 + \cdots + 1/n$.
Table lookup is used for $H_n$ whose numerator and denominator
fit in a single limb. For larger $n$, a divide and conquer strategy is used.
"""
function harmonic(n::Int)
   n < 0 && throw(DomainError(n, "Index must be non-negative"))
   c = fmpq()
   ccall((:fmpq_harmonic_ui, libflint), Nothing, (Ref{fmpq}, Int), c, n)
   return c
end

@doc Markdown.doc"""
    bernoulli(n::Int)

Computes the Bernoulli number $B_n$ for nonnegative $n$.
"""
function bernoulli(n::Int)
   n < 0 && throw(DomainError(n, "Index must be non-negative"))
   c = fmpq()
   ccall((:bernoulli_fmpq_ui, libarb), Nothing, (Ref{fmpq}, Int), c, n)
   return c
end

@doc Markdown.doc"""
    bernoulli_cache(n::Int)

Precomputes and caches all the Bernoulli numbers up to $B_n$.
This is much faster than repeatedly calling `bernoulli(k)`.
Once cached, subsequent calls to `bernoulli(k)` for any $k \le n$
will read from the cache, making them virtually free.
"""
function bernoulli_cache(n::Int)
   n = n + 1
   n < 0 && throw(DomainError(n, "Index must be non-negative"))
   ccall((:bernoulli_cache_compute, libarb), Nothing, (Int,), n)
end

@doc Markdown.doc"""
    dedekind_sum(h::fmpz, k::fmpz)
"""
function dedekind_sum(h::fmpz, k::fmpz)
   c = fmpq()
   ccall((:fmpq_dedekind_sum, libflint), Nothing,
         (Ref{fmpq}, Ref{fmpz}, Ref{fmpz}), c, h, k)
   return c
end

@doc Markdown.doc"""
    dedekind_sum(h::fmpz, k::Integer)

Computes the Dedekind sum $s(h,k)$ for arbitrary $h$ and $k$.
"""
dedekind_sum(h::fmpz, k::Integer) = dedekind_sum(h, fmpz(k))

@doc Markdown.doc"""
    dedekind_sum(h::Integer, k::fmpz)

Computes the Dedekind sum $s(h,k)$ for arbitrary $h$ and $k$.
"""
dedekind_sum(h::Integer, k::fmpz) = dedekind_sum(fmpz(h), k)

@doc Markdown.doc"""
    dedekind_sum(h::Integer, k::Integer)

Computes the Dedekind sum $s(h,k)$ for arbitrary $h$ and $k$.
"""
dedekind_sum(h::Integer, k::Integer) = dedekind_sum(fmpz(h), fmpz(k))

###############################################################################
#
#  Simplest between
#
###############################################################################

function _fmpq_simplest_between(l_num::fmpz, l_den::fmpz,
                                r_num::fmpz, r_den::fmpz)
   n = fmpz()
   d = fmpz()

   ccall((:_fmpq_simplest_between, libflint), Nothing,
         (Ref{fmpz}, Ref{fmpz}, Ref{fmpz}, Ref{fmpz}, Ref{fmpz}, Ref{fmpz}),
         n, d, l_num, l_den, r_num, r_den)

   return n//d
end

@doc Markdown.doc"""
      simplest_between(l::fmpq, r::fmpq)

Return the simplest fraction in the closed interval `[l, r]`. A canonical
fraction `a_1/b_1` is defined to be simpler than `a_2/b_2` iff `b_1 < b_2` or
`b_1 = b_2` and `a_1 < a_2`.
"""
function simplest_between(l::fmpq, r::fmpq)
   z = fmpq()
   ccall((:fmpq_simplest_between, libflint), Nothing,
         (Ref{fmpq}, Ref{fmpq}, Ref{fmpq}), z, l, r)
   return z
end


###############################################################################
#
#   Unsafe operators and functions
#
###############################################################################

function zero!(c::fmpq)
   ccall((:fmpq_zero, libflint), Nothing,
         (Ref{fmpq},), c)
   return c
end

function mul!(c::fmpq, a::fmpq, b::fmpq)
   ccall((:fmpq_mul, libflint), Nothing,
         (Ref{fmpq}, Ref{fmpq}, Ref{fmpq}), c, a, b)
   return c
end

function addeq!(c::fmpq, a::fmpq)
   ccall((:fmpq_add, libflint), Nothing,
         (Ref{fmpq}, Ref{fmpq}, Ref{fmpq}), c, c, a)
   return c
end

function add!(c::fmpq, a::fmpq, b::fmpq)
   ccall((:fmpq_add, libflint), Nothing,
         (Ref{fmpq}, Ref{fmpq}, Ref{fmpq}), c, a, b)
   return c
end

###############################################################################
#
#   Conversions to/from flint Julia rationals
#
###############################################################################

function Rational(z::fmpq)
   r = Rational{BigInt}(0)
   ccall((:fmpq_get_mpz_frac, libflint), Nothing,
         (Ref{BigInt}, Ref{BigInt}, Ref{fmpq}), r.num, r.den, z)
   return r
end

function Rational(z::fmpz)
   return Rational{BigInt}(BigInt(z))
end

###############################################################################
#
#   Parent object call overloads
#
###############################################################################

(a::FlintRationalField)() = fmpq(fmpz(0), fmpz(1))

function (a::FlintRationalField)(b::Rational)
   # work around Julia bug, https://github.com/JuliaLang/julia/issues/32569
   if denominator(b) < 0
      return fmpq(fmpz(numerator(b)), -fmpz(denominator(b)))
   else
      return fmpq(numerator(b), denominator(b))
   end
end

(a::FlintRationalField)(b::Integer) = fmpq(b)

(a::FlintRationalField)(b::Int, c::Int) = fmpq(b, c)

(a::FlintRationalField)(b::fmpz) = fmpq(b)

(a::FlintRationalField)(b::Integer, c::Integer) = fmpq(b, c)

(a::FlintRationalField)(b::fmpz, c::Integer) = fmpq(b, c)

(a::FlintRationalField)(b::Integer, c::fmpz) = fmpq(b, c)

(a::FlintRationalField)(b::fmpz, c::fmpz) = fmpq(b, c)

(a::FlintRationalField)(b::fmpq) = b

###############################################################################
#
#   Random generation
#
###############################################################################

@doc Markdown.doc"""
    rand_bits(::FlintRationalField, b::Int)

Return a random signed rational whose numerator and denominator both have $b$
bits before canonicalisation. Note that the resulting numerator and
denominator can be smaller than $b$ bits.
"""
function rand_bits(::FlintRationalField, b::Int)
   b > 0 || throw(DomainError(b, "Bit count must be positive"))
   z = fmpq()
   ccall((:fmpq_randbits, libflint), Nothing, (Ref{fmpq}, Ptr{Cvoid}, Int),
         z, _flint_rand_states[Threads.threadid()].ptr, b)
   return z
end

###############################################################################
#
#   Conversions and promotions
#
###############################################################################

convert(::Type{fmpq}, a::Integer) = fmpq(a)

convert(::Type{fmpq}, a::fmpz) = fmpq(a)

Base.promote_rule(::Type{fmpq}, ::Type{T}) where {T <: Integer} = fmpq

Base.promote_rule(::Type{fmpq}, ::Type{fmpz}) = fmpq

promote_rule(::Type{fmpq}, ::Type{fmpz}) = fmpq

Base.promote_rule(::Type{fmpq}, ::Type{Rational{T}}) where {T <: Integer} = fmpq

convert(::Type{Rational{BigInt}}, a::fmpq) = Rational(a)

###############################################################################
#
#   FractionField constructor
#
###############################################################################

FractionField(base::FlintIntegerRing) = FlintQQ
