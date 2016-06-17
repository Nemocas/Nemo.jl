###############################################################################
#
#   fmpq.jl : Flint rationals
#
###############################################################################

export fmpq, FlintQQ, FractionField, Rational, FlintRationalField, height,
       height_bits, isless, reconstruct, next_minimal, next_signed_minimal,
       next_calkin_wilf, next_signed_calkin_wilf, dedekind_sum, harmonic,
       bernoulli, bernoulli_cache

###############################################################################
#
#   Data type and parent methods
#
###############################################################################

fmpq(a::Rational{BigInt}) = fmpq(fmpz(a.num), fmpz(a.den))

fmpq(a::Integer) = fmpq(fmpz(a), fmpz(1))

fmpq(a::Integer, b::Integer) = fmpq(fmpz(a), fmpz(b))

fmpq(a::fmpz, b::Integer) = fmpq(a, fmpz(b))

fmpq(a::Integer, b::fmpz) = fmpq(fmpz(a), b)

parent(a::fmpq) = FlintQQ

parent_type(::Type{fmpq}) = FlintRationalField

elem_type(::FlintRationalField) = fmpq

base_ring(a::FlintRationalField) = FlintZZ

base_ring(a::fmpq) = FlintZZ

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
   y == 0 && throw(DivideError())
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

function num(a::fmpq)
   z = fmpz()
   ccall((:fmpq_numerator, :libflint), Void, (Ptr{fmpz}, Ptr{fmpq}), &z, &a)
   return z
end

function den(a::fmpq)
   z = fmpz()
   ccall((:fmpq_denominator, :libflint), Void, (Ptr{fmpz}, Ptr{fmpq}), &z, &a)
   return z
end

doc"""
    abs(a::fmpq)
> Return the absolute value of $a$.
"""
function abs(a::fmpq)
   z = fmpq()
   ccall((:fmpq_abs, :libflint), Void, (Ptr{fmpq}, Ptr{fmpq}), &z, &a)
   return z
end

zero(a::FlintRationalField) = fmpq(0)

one(a::FlintRationalField) = fmpq(1)

isone(a::fmpq) = a == 1

iszero(a::fmpq) = a == 0

isunit(a::fmpq) = a != 0

doc"""
    height(a::fmpq)
> Return the height of the fraction $a$, namely the largest of the absolute
> values of the numerator and denominator.
"""
function height(a::fmpq)
   temp = fmpz()
   ccall((:fmpq_height, :libflint), Void, (Ptr{fmpz}, Ptr{fmpq}), &temp, &a)
   return temp
end

doc"""
    height_bits(a::fmpq)
> Return the number of bits of the height of the fraction $a$.
"""
function height_bits(a::fmpq)
   return ccall((:fmpq_height_bits, :libflint), Int, (Ptr{fmpq},), &a)
end

function deepcopy(a::fmpq)
   z = fmpq()
   ccall((:fmpq_set, :libflint), Void, (Ptr{fmpq}, Ptr{fmpq}), &z, &a)
   return z
end

###############################################################################
#
#   Canonicalisation
#
###############################################################################

canonical_unit(a::fmpq) = a

###############################################################################
#
#   AbstractString{} I/O
#
###############################################################################

function show(io::IO, a::FlintRationalField)
   print(io, "Rational Field")
end

function show(io::IO, a::fmpq)
   print(io, num(a))
   if den(a) != 1
      print(io, "//", den(a))
   end
end

needs_parentheses(x::fmpq) = false

is_negative(x::fmpq) = x < 0

show_minus_one(::Type{fmpq}) = false

###############################################################################
#
#   Unary operators
#
###############################################################################

function -(a::fmpq)
   z = fmpq()
   ccall((:fmpq_neg, :libflint), Void, (Ptr{fmpq}, Ptr{fmpq}), &z, &a)
   return z
end

###############################################################################
#
#   Binary operators
#
###############################################################################

function +(a::fmpq, b::fmpq)
   z = fmpq()
   ccall((:fmpq_add, :libflint), Void, 
         (Ptr{fmpq}, Ptr{fmpq}, Ptr{fmpq}), &z, &a, &b)
   return z
end

function -(a::fmpq, b::fmpq)
   z = fmpq()
   ccall((:fmpq_sub, :libflint), Void, 
         (Ptr{fmpq}, Ptr{fmpq}, Ptr{fmpq}), &z, &a, &b)
   return z
end

function *(a::fmpq, b::fmpq)
   z = fmpq()
   ccall((:fmpq_mul, :libflint), Void, 
         (Ptr{fmpq}, Ptr{fmpq}, Ptr{fmpq}), &z, &a, &b)
   return z
end

###############################################################################
#
#   Ad hoc binary operators
#
###############################################################################

function +(a::fmpq, b::Int)
   z = fmpq()
   ccall((:fmpq_add_si, :libflint), Void, 
         (Ptr{fmpq}, Ptr{fmpq}, Int), &z, &a, b)
   return z
end

function +(a::fmpq, b::fmpz)
   z = fmpq()
   ccall((:fmpq_add_fmpz, :libflint), Void, 
         (Ptr{fmpq}, Ptr{fmpq}, Ptr{fmpz}), &z, &a, &b)
   return z
end

+(a::Int, b::fmpq) = b + a

+(a::fmpz, b::fmpq) = b + a

function -(a::fmpq, b::Int)
   z = fmpq()
   ccall((:fmpq_sub_si, :libflint), Void, 
         (Ptr{fmpq}, Ptr{fmpq}, Int), &z, &a, b)
   return z
end

function -(a::fmpq, b::fmpz)
   z = fmpq()
   ccall((:fmpq_sub_fmpz, :libflint), Void, 
         (Ptr{fmpq}, Ptr{fmpq}, Ptr{fmpz}), &z, &a, &b)
   return z
end

function *(a::fmpq, b::fmpz)
   z = fmpq()
   ccall((:fmpq_mul_fmpz, :libflint), Void, 
         (Ptr{fmpq}, Ptr{fmpq}, Ptr{fmpz}), &z, &a, &b)
   return z
end

*(a::fmpz, b::fmpq) = b*a

function -(a::fmpz, b::fmpq)
   n = a*den(b) - num(b)
   d = den(b)
   g = gcd(n, d)
   return parent(b)(divexact(n, g), divexact(d, g))
end
###############################################################################
#
#   Comparison
#
###############################################################################

function ==(a::fmpq, b::fmpq)
   return ccall((:fmpq_equal, :libflint), Bool, 
                (Ptr{fmpq}, Ptr{fmpq}), &a, &b)
end

doc"""
    isless(a::fmpq, b::fmpq)
> Return `true` if $a < b$, otherwise return `false`.
"""
function isless(a::fmpq, b::fmpq)
   return ccall((:fmpq_cmp, :libflint), Cint, 
                (Ptr{fmpq}, Ptr{fmpq}), &a, &b) < 0
end

###############################################################################
#
#   Ad hoc comparison
#
###############################################################################

function ==(a::fmpq, b::Int)
   return ccall((:fmpq_equal_si, :libflint), Bool, (Ptr{fmpq}, Int), &a, b)
end

==(a::Int, b::fmpq) = b == a

function ==(a::fmpq, b::fmpz)
   return ccall((:fmpq_equal_fmpz, :libflint), Bool, 
                (Ptr{fmpq}, Ptr{fmpz}), &a, &b)
end

==(a::fmpz, b::fmpq) = b == a

doc"""
    isless(a::fmpq, b::Integer)
> Return `true` if $a < b$, otherwise return `false`.
"""
function isless(a::fmpq, b::Integer)
   z = fmpq(b)
   return ccall((:fmpq_cmp, :libflint), Cint,
                (Ptr{fmpq}, Ptr{fmpq}), &a, &z) < 0
end

doc"""
    isless(a::Integer, b::fmpq)
> Return `true` if $a < b$, otherwise return `false`.
"""
function isless(a::Integer, b::fmpq)
   z = fmpq(a)
   return ccall((:fmpq_cmp, :libflint), Cint,
                (Ptr{fmpq}, Ptr{fmpq}), &z, &b) < 0
end

doc"""
    isless(a::fmpq, b::fmpz)
> Return `true` if $a < b$, otherwise return `false`.
"""
function isless(a::fmpq, b::fmpz)
   z = fmpq(b)
   return ccall((:fmpq_cmp, :libflint), Cint,
                (Ptr{fmpq}, Ptr{fmpq}), &a, &z) < 0
end

doc"""
    isless(a::fmpz, b::fmpq)
> Return `true` if $a < b$, otherwise return `false`.
"""
function isless(a::fmpz, b::fmpq)
   z = fmpq(a)
   return ccall((:fmpq_cmp, :libflint), Cint,
                (Ptr{fmpq}, Ptr{fmpq}), &z, &b) < 0
end

###############################################################################
#
#   Powering
#
###############################################################################

function ^(a::fmpq, b::Int)
   temp = fmpq()
   ccall((:fmpq_pow_si, :libflint), Void, 
         (Ptr{fmpq}, Ptr{fmpq}, Int), &temp, &a, b)
   return temp
end

###############################################################################
#
#   Shifting
#
###############################################################################

doc"""
    <<(a::fmpq, b::Int)
> Return $2^b/a$.
"""
function >>(a::fmpq, b::Int)
   z = fmpq()
   ccall((:fmpq_div_2exp, :libflint), Void, 
         (Ptr{fmpq}, Ptr{fmpq}, Int), &z, &a, b)
   return z
end

doc"""
    <<(a::fmpq, b::Int)
> Return $2^b\times a$.
"""
function <<(a::fmpq, b::Int)
   z = fmpq()
   ccall((:fmpq_mul_2exp, :libflint), Void, 
         (Ptr{fmpq}, Ptr{fmpq}, Int), &z, &a, b)
   return z
end

###############################################################################
#
#   Inversion
#
###############################################################################

function inv(a::fmpq)
   z = fmpq()
   ccall((:fmpq_inv, :libflint), Void, (Ptr{fmpq}, Ptr{fmpq}), &z, &a)
   return z
end

###############################################################################
#
#   Exact division
#
###############################################################################

function divexact(a::fmpq, b::fmpq)
   z = fmpq()
   ccall((:fmpq_div, :libflint), Void,
         (Ptr{fmpq}, Ptr{fmpq}, Ptr{fmpq}), &z, &a, &b)
   return z
end

###############################################################################
#
#   Ad hoc exact division
#
###############################################################################

function divexact(a::fmpq, b::fmpz)
   z = fmpq()
   ccall((:fmpq_div_fmpz, :libflint), Void,
         (Ptr{fmpq}, Ptr{fmpq}, Ptr{fmpz}), &z, &a, &b)
   return z
end

divexact(a::fmpz, b::fmpq) = inv(b)*a

divexact(a::fmpq, b::Integer) = divexact(a, fmpz(b))

divexact(a::Integer, b::fmpq) = inv(b)*a

###############################################################################
#
#   Modular arithmetic
#
###############################################################################

doc"""
    mod(a::fmpq, b::fmpz)
> Return $a \pmod{b}$ where $b$ is an integer coprime to the denominator of
> $a$.
"""
function mod(a::fmpq, b::fmpz)
   z = fmpz()
   ccall((:fmpq_mod_fmpz, :libflint), Void,
         (Ptr{fmpz}, Ptr{fmpq}, Ptr{fmpz}), &z, &a, &b)
   return z
end

doc"""
    mod(a::fmpq, b::Integer)
> Return $a \pmod{b}$ where $b$ is an integer coprime to the denominator of
> $a$.
"""
mod(a::fmpq, b::Integer) = mod(a, fmpz(b))

###############################################################################
#
#   GCD
#
###############################################################################

function gcd(a::fmpq, b::fmpq)
   z = fmpq()
   ccall((:fmpq_gcd, :libflint), Void, 
         (Ptr{fmpq}, Ptr{fmpq}, Ptr{fmpq}), &z, &a, &b)
   return z
end

###############################################################################
#
#   Rational reconstruction
#
###############################################################################

doc"""
    reconstruct(a::fmpz, b::fmpz)
> Attempt to find a rational number $n/d$ such that 
> $0 \leq |n| \leq \lfloor\sqrt{m/2}\rfloor$ and 
> $0 < d \leq \lfloor\sqrt{m/2}\rfloor$ such that gcd$(n, d) = 1$ and
> $a \equiv nd^{-1} \pmod{m}$. If no solution exists, an exception is thrown.
"""
function reconstruct(a::fmpz, b::fmpz)
   c = fmpq()
   if !Bool(ccall((:fmpq_reconstruct_fmpz, :libflint), Cint, 
                  (Ptr{fmpq}, Ptr{fmpz}, Ptr{fmpz}), &c, &a, &b))
      error("Impossible rational reconstruction")
   end
   return c
end

doc"""
    reconstruct(a::fmpz, b::Integer)
> Attempt to find a rational number $n/d$ such that 
> $0 \leq |n| \leq \lfloor\sqrt{m/2}\rfloor$ and 
> $0 < d \leq \lfloor\sqrt{m/2}\rfloor$ such that gcd$(n, d) = 1$ and
> $a \equiv nd^{-1} \pmod{m}$. If no solution exists, an exception is thrown.
"""
reconstruct(a::fmpz, b::Integer) =  reconstruct(a, fmpz(b))

doc"""
    reconstruct(a::Integer, b::fmpz)
> Attempt to find a rational number $n/d$ such that 
> $0 \leq |n| \leq \lfloor\sqrt{m/2}\rfloor$ and 
> $0 < d \leq \lfloor\sqrt{m/2}\rfloor$ such that gcd$(n, d) = 1$ and
> $a \equiv nd^{-1} \pmod{m}$. If no solution exists, an exception is thrown.
"""
reconstruct(a::Integer, b::fmpz) =  reconstruct(fmpz(a), b)

doc"""
    reconstruct(a::Integer, b::Integer)
> Attempt to find a rational number $n/d$ such that 
> $0 \leq |n| \leq \lfloor\sqrt{m/2}\rfloor$ and 
> $0 < d \leq \lfloor\sqrt{m/2}\rfloor$ such that gcd$(n, d) = 1$ and
> $a \equiv nd^{-1} \pmod{m}$. If no solution exists, an exception is thrown.
"""
reconstruct(a::Integer, b::Integer) =  reconstruct(fmpz(a), fmpz(b))

###############################################################################
#
#   Rational enumeration
#
###############################################################################

doc"""
    next_minimal(a::fmpq)
> Given $x$, returns the next rational number in the sequence obtained by
> enumerating all positive denominators $q$, and for each $q$ enumerating
> the numerators $1 \le p < q$ in order and generating both $p/q$ and $q/p$,
> but skipping all gcd$(p,q) \neq 1$. Starting with zero, this generates
> every nonnegative rational number once and only once, with the first
> few entries being $0, 1, 1/2, 2, 1/3, 3, 2/3, 3/2, 1/4, 4, 3/4, 4/3, \ldots$.
> This enumeration produces the rational numbers in order of minimal height. 
> It has the disadvantage of being somewhat slower to compute than the
> Calkin-Wilf enumeration. If $x < 0$ we throw a `DomainError()`.
"""
function next_minimal(a::fmpq)
   a < 0 && throw(DomainError())
   c = fmpq()
   ccall((:fmpq_next_minimal, :libflint), Void, (Ptr{fmpq}, Ptr{fmpq}), &c, &a)
   return c
end

doc"""
    next_signed_minimal(a::fmpq)
> Given a signed rational number $x$ assumed to be in canonical form, 
> returns the next element in the minimal-height sequence generated by 
> `next_minimal` but with negative numbers interleaved. The sequence begins
> $0, 1, -1, 1/2, -1/2, 2, -2, 1/3, -1/3, \ldots$. Starting with zero, this
> generates every rational number once and only once, in order of minimal
> height.
"""
function next_signed_minimal(a::fmpq)
   c = fmpq()
   ccall((:fmpq_next_signed_minimal, :libflint), Void, 
         (Ptr{fmpq}, Ptr{fmpq}), &c, &a)
   return c
end

doc"""
    next_calkin_wilf(a::fmpq)
> Given $x$ return the next number in the breadth-first traversal of the
> Calkin-Wilf tree. Starting with zero, this generates every nonnegative
> rational number once and only once, with the first few entries being
> $0, 1, 1/2, 2, 1/3, 3/2, 2/3, 3, 1/4, 4/3, 3/5, 5/2, 2/5, \ldots$.
> Despite the appearance of the initial entries, the Calkin-Wilf enumeration 
> does not produce the rational numbers in order of height: some small
> fractions will appear late in the sequence. This order has the advantage of
> being faster to produce than the minimal-height order.
"""
function next_calkin_wilf(a::fmpq)
   a < 0 && throw(DomainError())
   c = fmpq()
   ccall((:fmpq_next_calkin_wilf, :libflint), Void, 
         (Ptr{fmpq}, Ptr{fmpq}), &c, &a)
   return c
end

doc"""
    next_signed_calkin_wilf(a::fmpq)
> Given a signed rational number $x$ returns the next element in the
> Calkin-Wilf sequence with negative numbers interleaved. The sequence begins
> $0, 1, -1, 1/2, -1/2, 2, -2, 1/3, -1/3, \ldots$. Starting with zero, this
> generates every rational number once and only once, but not in order of
> minimal height.
"""
function next_signed_calkin_wilf(a::fmpq)
   c = fmpq()
   ccall((:fmpq_next_signed_calkin_wilf, :libflint), Void, 
         (Ptr{fmpq}, Ptr{fmpq}), &c, &a)
   return c
end

###############################################################################
#
#   Special functions
#
###############################################################################

doc"""
    harmonic(n::Int)
> Computes the harmonic number $H_n = 1 + 1/2 + 1/3 + \cdots + 1/n$.
> Table lookup is used for $H_n$ whose numerator and denominator 
> fit in a single limb. For larger $n$, a divide and conquer strategy is used.
"""
function harmonic(n::Int)
   n < 0 && throw(DomainError())
   c = fmpq()
   ccall((:fmpq_harmonic_ui, :libflint), Void, (Ptr{fmpq}, Int), &c, n)
   return c
end

doc"""
    bernoulli(n::Int)
> Computes the Bernoulli number $B_n$ for nonnegative $n$.
"""
function bernoulli(n::Int)
   n < 0 && throw(DomainError())
   c = fmpq()
   ccall((:bernoulli_fmpq_ui, :libarb), Void, (Ptr{fmpq}, Int), &c, n)
   return c
end

doc"""
    bernoulli_cache(n::Int)
> Precomputes and caches all the Bernoulli numbers up to $B_n$.
> This is much faster than repeatedly calling `bernoulli(k)`.
> Once cached, subsequent calls to `bernoulli(k)` for any $k \le n$
> will read from the cache, making them virtually free.
"""
function bernoulli_cache(n::Int)
   n = n + 1
   n < 0 && throw(DomainError())
   ccall((:bernoulli_cache_compute, :libarb), Void, (Int,), n)
end

doc"""
    dedekind_sum(h::fmpz, k::fmpz)
"""
function dedekind_sum(h::fmpz, k::fmpz)
   c = fmpq()
   ccall((:fmpq_dedekind_sum, :libflint), Void, 
         (Ptr{fmpq}, Ptr{fmpz}, Ptr{fmpz}), &c, &h, &k)
   return c
end

doc"""
    dedekind_sum(h::fmpz, k::Integer)
> Computes the Dedekind sum $s(h,k)$ for arbitrary $h$ and $k$.
"""
dedekind_sum(h::fmpz, k::Integer) = dedekind_sum(h, fmpz(k))

doc"""
    dedekind_sum(h::Integer, k::fmpz)
> Computes the Dedekind sum $s(h,k)$ for arbitrary $h$ and $k$.
"""
dedekind_sum(h::Integer, k::fmpz) = dedekind_sum(fmpz(h), k)

doc"""
    dedekind_sum(h::Integer, k::Integer) 
> Computes the Dedekind sum $s(h,k)$ for arbitrary $h$ and $k$.
"""
dedekind_sum(h::Integer, k::Integer) = dedekind_sum(fmpz(h), fmpz(k))

###############################################################################
#
#   Unsafe operators and functions
#
###############################################################################

function mul!(c::fmpq, a::fmpq, b::fmpq)
   ccall((:fmpq_mul, :libflint), Void,
         (Ptr{fmpq}, Ptr{fmpq}, Ptr{fmpq}), &c, &a, &b)
end

function addeq!(c::fmpq, a::fmpq)
   ccall((:fmpq_add, :libflint), Void,
         (Ptr{fmpq}, Ptr{fmpq}, Ptr{fmpq}), &c, &c, &a)
end

###############################################################################
#
#   Conversions to/from flint Julia rationals
#
###############################################################################
   
function Rational(z::fmpq)
   r = Rational{BigInt}(0)
   ccall((:fmpq_get_mpz_frac, :libflint), Void, 
         (Ptr{BigInt}, Ptr{BigInt}, Ptr{fmpq}), &r.num, &r.den, &z)
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

call(a::FlintRationalField) = fmpq(fmpz(0), fmpz(1))

call(a::FlintRationalField, b::Rational{BigInt}) = fmpq(b) 

call(a::FlintRationalField, b::Integer) = fmpq(b)

call(a::FlintRationalField, b::Int, c::Int) = fmpq(b, c)

call(a::FlintRationalField, b::fmpz) = fmpq(b)

call(a::FlintRationalField, b::Integer, c::Integer) = fmpq(b, c)

call(a::FlintRationalField, b::fmpz, c::Integer) = fmpq(b, c)

call(a::FlintRationalField, b::Integer, c::fmpz) = fmpq(b, c)

call(a::FlintRationalField, b::fmpz, c::fmpz) = fmpq(b, c)

call(a::FlintRationalField, b::fmpq) = b

###############################################################################
#
#   Conversions and promotions
#
###############################################################################

convert(::Type{fmpq}, a::Integer) = fmpq(a)

convert(::Type{fmpq}, a::fmpz) = fmpq(a)

Base.promote_rule{T <: Integer}(::Type{fmpq}, ::Type{T}) = fmpq

Base.promote_rule(::Type{fmpq}, ::Type{fmpz}) = fmpq

convert(::Type{Rational{BigInt}}, a::fmpq) = Rational(a)

###############################################################################
#
#   FractionField constructor
#
###############################################################################

FractionField(base::FlintIntegerRing) = FlintQQ

