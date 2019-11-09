###############################################################################
#
#   fmpz.jl : BigInts
#
###############################################################################

# Copyright (c) 2009-2014: Jeff Bezanson, Stefan Karpinski, Viral B. Shah,
# and other contributors:
#
# https://github.com/JuliaLang/julia/contributors
#
# Copyright (C) 2014, 2015 William Hart
# Copyright (C) 2015, Claus Fieker
#
# Permission is hereby granted, free of charge, to any person obtaining
# a copy of this software and associated documentation files (the
# "Software"), to deal in the Software without restriction, including
# without limitation the rights to use, copy, modify, merge, publish,
# distribute, sublicense, and/or sell copies of the Software, and to
# permit persons to whom the Software is furnished to do so, subject to
# the following conditions:
#
# The above copyright notice and this permission notice shall be
# included in all copies or substantial portions of the Software.
#
# THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND,
# EXPRESS OR IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF
# MERCHANTABILITY, FITNESS FOR A PARTICULAR PURPOSE AND
# NONINFRINGEMENT. IN NO EVENT SHALL THE AUTHORS OR COPYRIGHT HOLDERS BE
# LIABLE FOR ANY CLAIM, DAMAGES OR OTHER LIABILITY, WHETHER IN AN ACTION
# OF CONTRACT, TORT OR OTHERWISE, ARISING FROM, OUT OF OR IN CONNECTION
# WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS IN THE SOFTWARE.

export fmpz, FlintZZ, FlintIntegerRing, parent, show, convert, hash, fac, bell,
       binom, isprime, fdiv, cdiv, tdiv, div, rem, mod, gcd, lcm, invmod,
       powmod, abs, divrem, isqrt, popcount, prevpow2, nextpow2, ndigits, dec,
       bin, oct, hex, base, one, zero, divexact, fits, sign, nbits, deepcopy,
       tdivpow2, fdivpow2, cdivpow2, flog, clog, cmpabs, clrbit!, setbit!,
       combit!, crt, divisible, divisor_lenstra, fdivrem, tdivrem, fmodpow2,
       gcdinv, isprobable_prime, issquare, jacobi, remove, root, size, isqrtrem,
       sqrtmod, trailing_zeros, sigma, eulerphi, fib, moebiusmu, primorial,
       risingfac, numpart, canonical_unit, needs_parentheses, displayed_with_minus_in_front,
       show_minus_one, addeq!, mul!, isunit, isequal,
       iszero, rand

###############################################################################
#
#   Data type and parent methods
#
###############################################################################

parent_type(::Type{fmpz}) = FlintIntegerRing

@doc Markdown.doc"""
    parent(a::fmpz)
> Returns the unique Flint integer parent object `FlintZZ`.
"""
parent(a::fmpz) = FlintZZ

elem_type(::Type{FlintIntegerRing}) = fmpz

@doc Markdown.doc"""
    base_ring(a::FlintIntegerRing)
> Returns `Union{}` as this ring is not dependent on another ring.
"""
base_ring(a::FlintIntegerRing) = Union{}

@doc Markdown.doc"""
    base_ring(a::fmpz)
> Returns `Union{}` as the parent ring is not dependent on another ring.
"""
base_ring(a::fmpz) = Union{}

isdomain_type(::Type{fmpz}) = true

################################################################################
#
#   Hashing
#
################################################################################

# Similar to hash for BigInt found in julia/base

function _fmpz_is_small(a::fmpz)
   return __fmpz_is_small(a.d)
end

function _fmpz_limbs(a::fmpz)
   return __fmpz_limbs(a.d)
end

function hash_integer(a::fmpz, h::UInt)
   return _hash_integer(a.d, h)
end

function hash(a::fmpz, h::UInt)
   return hash_integer(a, h)
end

function __fmpz_is_small(a::Int)
   return (unsigned(a) >> (Sys.WORD_SIZE - 2) != 1)
end

function __fmpz_limbs(a::Int)
   if __fmpz_is_small(a)
      return Cint(0)
   end
   b = unsafe_load(convert(Ptr{Cint}, unsigned(a)<<2), 2)
   return b
end

function _hash_integer(a::Int, h::UInt)
   s::Cint = __fmpz_limbs(a)
   s == 0 && return Base.hash_integer(a, h)
   # get the pointer after the first two Cint
   d = convert(Ptr{Ptr{UInt}}, unsigned(a) << 2) + 2*sizeof(Cint)
   p = unsafe_load(d)
   b = unsafe_load(p)
   h = xor(Base.hash_uint(xor(ifelse(s < 0, -b, b), h)), h)
   for k = 2:abs(s)
      h = xor(Base.hash_uint(xor(unsafe_load(p, k), h)), h)
   end
   return h
end

###############################################################################
#
#   Basic manipulation
#
###############################################################################

function deepcopy_internal(a::fmpz, dict::IdDict)
   z = fmpz()
   ccall((:fmpz_set, :libflint), Nothing, (Ref{fmpz}, Ref{fmpz}), z, a)
   return z
end

@doc Markdown.doc"""
    one(R::FlintIntegerRing)
> Return the integer $1$.
"""
one(R::FlintIntegerRing) = fmpz(1)

@doc Markdown.doc"""
    zero(R::FlintIntegerRing)
> Return the integer $1$.
"""
zero(R::FlintIntegerRing) = fmpz(0)


@doc Markdown.doc"""
    sign(a::fmpz)
> Returns the sign of $a$, i.e. $+1$, $0$ or $-1$.
"""
sign(a::fmpz) = Int(ccall((:fmpz_sgn, :libflint), Cint, (Ref{fmpz},), a))

@doc Markdown.doc"""
    fits(::Type{Int}, a::fmpz)
> Returns `true` if the given integer fits into an `Int`, otherwise returns
> `false`.
"""
fits(::Type{Int}, a::fmpz) = ccall((:fmpz_fits_si, :libflint), Bool,
                                   (Ref{fmpz},), a)

@doc Markdown.doc"""
    fits(::Type{UInt}, a::fmpz)
> Returns `true` if the given integer fits into a `UInt`, otherwise returns
> `false`.
"""
fits(::Type{UInt}, a::fmpz) = sign(a) < 0 ? false :
              ccall((:fmpz_abs_fits_ui, :libflint), Bool, (Ref{fmpz},), a)

@doc Markdown.doc"""
    size(a::fmpz)
> Returns the number of limbs required to store the absolute value of $a$.
"""
size(a::fmpz) = Int(ccall((:fmpz_size, :libflint), Cint, (Ref{fmpz},), a))

@doc Markdown.doc"""
    isunit(a::fmpz)
> Return `true` if the given integer is a unit, i.e. $\pm 1$, otherwise return
> `false`.
"""
isunit(a::fmpz) = ccall((:fmpz_is_pm1, :libflint), Bool, (Ref{fmpz},), a)

@doc Markdown.doc"""
    iszero(a::fmpz)
> Return `true` if the given integer is zero, otherwise return `false`.
"""
iszero(a::fmpz) = ccall((:fmpz_is_zero, :libflint), Bool, (Ref{fmpz},), a)

@doc Markdown.doc"""
    isone(a::fmpz)
> Return `true` if the given integer is one, otherwise return `false`.
"""
isone(a::fmpz) = ccall((:fmpz_is_one, :libflint), Bool, (Ref{fmpz},), a)

@doc Markdown.doc"""
    denominator(a::fmpz)
> Returns the denominator of $a$ thought of as a rational. Always returns $1$.
"""
function denominator(a::fmpz)
   return fmpz(1)
end

@doc Markdown.doc"""
    numerator(a::fmpz)
> Returns the numerator of $a$ thought of as a rational. Always returns $a$.
"""
function numerator(a::fmpz)
   return a
end

###############################################################################
#
#   AbstractString I/O
#
###############################################################################

string(x::fmpz) = dec(x)

show(io::IO, x::fmpz) = print(io, string(x))

show(io::IO, a::FlintIntegerRing) = print(io, "Integer Ring")

needs_parentheses(x::fmpz) = false

displayed_with_minus_in_front(x::fmpz) = x < 0

show_minus_one(::Type{fmpz}) = false

###############################################################################
#
#   Canonicalisation
#
###############################################################################

canonical_unit(x::fmpz) = x < 0 ? fmpz(-1) : fmpz(1)

###############################################################################
#
#   Unary operators and functions, e.g. -fmpz(12), ~fmpz(12)
#
###############################################################################

function -(x::fmpz)
    z = fmpz()
    ccall((:__fmpz_neg, :libflint), Nothing, (Ref{fmpz}, Ref{fmpz}), z, x)
    return z
end

function ~(x::fmpz)
    z = fmpz()
    ccall((:fmpz_complement, :libflint), Nothing, (Ref{fmpz}, Ref{fmpz}), z, x)
    return z
end

function abs(x::fmpz)
    z = fmpz()
    ccall((:fmpz_abs, :libflint), Nothing, (Ref{fmpz}, Ref{fmpz}), z, x)
    return z
end

###############################################################################
#
#   Binary operators and functions
#
###############################################################################

# Metaprogram to define functions +, -, *, gcd, lcm,
#                                 &, |, $ (xor)

for (fJ, fC) in ((:+, :add), (:-,:sub), (:*, :mul),
                 (:&, :and), (:|, :or), (:xor, :xor))
   @eval begin
      function ($fJ)(x::fmpz, y::fmpz)
         z = fmpz()
         ccall(($(string(:fmpz_, fC)), :libflint), Nothing,
               (Ref{fmpz}, Ref{fmpz}, Ref{fmpz}), z, x, y)
         maybe_finalizer(z)
      end
   end
end

# Metaprogram to define functions fdiv, cdiv, tdiv, div, mod

for (fJ, fC) in ((:fdiv, :fdiv_q), (:cdiv, :cdiv_q), (:tdiv, :tdiv_q),
                 (:div, :tdiv_q))
    @eval begin
        function ($fJ)(x::fmpz, y::fmpz)
            iszero(y) && throw(DivideError())
            z = fmpz()
            ccall(($(string(:fmpz_, fC)), :libflint), Nothing,
                  (Ref{fmpz}, Ref{fmpz}, Ref{fmpz}), z, x, y)
            return z
        end
    end
end

function divexact(x::fmpz, y::fmpz)
    iszero(y) && throw(DivideError())
    z = fmpz()
    ccall((:fmpz_divexact, :libflint), Nothing,
          (Ref{fmpz}, Ref{fmpz}, Ref{fmpz}), z, x, y)
    z
end

function rem(x::fmpz, c::fmpz)
    iszero(c) && throw(DivideError())
    q = fmpz()
    r = fmpz()
    ccall((:fmpz_tdiv_qr, :libflint), Nothing,
          (Ref{fmpz}, Ref{fmpz}, Ref{fmpz}, Ref{fmpz}), q, r, x, abs(c))
    return r
end

###############################################################################
#
#   Ad hoc binary operators
#
###############################################################################

function +(x::fmpz, c::Int)
    z = fmpz()
    if c >= 0
       ccall((:fmpz_add_ui, :libflint), Nothing,
             (Ref{fmpz}, Ref{fmpz}, Int), z, x, c)
    else
       ccall((:fmpz_sub_ui, :libflint), Nothing,
             (Ref{fmpz}, Ref{fmpz}, Int), z, x, -c)
    end
    return z
end

+(c::Int, x::fmpz) = x + c

function -(x::fmpz, c::Int)
    z = fmpz()
    if c >= 0
       ccall((:fmpz_sub_ui, :libflint), Nothing,
             (Ref{fmpz}, Ref{fmpz}, Int), z, x, c)
    else
       ccall((:fmpz_add_ui, :libflint), Nothing,
             (Ref{fmpz}, Ref{fmpz}, Int), z, x, -c)
    end
    return z
end

function -(c::Int, x::fmpz)
    z = fmpz()
    if c >= 0
       ccall((:fmpz_sub_ui, :libflint), Nothing,
             (Ref{fmpz}, Ref{fmpz}, Int), z, x, c)
    else
       ccall((:fmpz_add_ui, :libflint), Nothing,
             (Ref{fmpz}, Ref{fmpz}, Int), z, x, -c)
    end
    ccall((:__fmpz_neg, :libflint), Nothing,
          (Ref{fmpz}, Ref{fmpz}), z, z)
    return z
end

function *(x::fmpz, c::Int)
    z = fmpz()
    ccall((:fmpz_mul_si, :libflint), Nothing,
          (Ref{fmpz}, Ref{fmpz}, Int), z, x, c)
    return z
end

*(c::Int, x::fmpz) = x * c

+(a::fmpz, b::Integer) = a + fmpz(b)

+(a::Integer, b::fmpz) = fmpz(a) + b

-(a::fmpz, b::Integer) = a - fmpz(b)

-(a::Integer, b::fmpz) = fmpz(a) - b

*(a::fmpz, b::Integer) = a*fmpz(b)

*(a::Integer, b::fmpz) = fmpz(a)*b

###############################################################################
#
#   Ad hoc exact division
#
###############################################################################

function divexact(x::fmpz, y::Int)
    y == 0 && throw(DivideError())
    z = fmpz()
    ccall((:fmpz_divexact_si, :libflint), Nothing,
          (Ref{fmpz}, Ref{fmpz}, Int), z, x, y)
    z
end

divexact(x::fmpz, y::Integer) = divexact(x, fmpz(y))

divexact(x::Integer, y::fmpz) = divexact(fmpz(x), y)

###############################################################################
#
#   Ad hoc division
#
###############################################################################

function rem(x::fmpz, c::Int)
    c == 0 && throw(DivideError())
    r = ccall((:fmpz_tdiv_ui, :libflint), Int, (Ref{fmpz}, Int), x, abs(c))
    return sign(x) > 0 ? r : -r
end

function tdivpow2(x::fmpz, c::Int)
    c < 0 && throw(DomainError("Exponent must be non-negative: $c"))
    z = fmpz()
    ccall((:fmpz_tdiv_q_2exp, :libflint), Nothing,
          (Ref{fmpz}, Ref{fmpz}, Int), z, x, c)
    return z
end

function fdivpow2(x::fmpz, c::Int)
    c < 0 && throw(DomainError("Exponent must be non-negative: $c"))
    z = fmpz()
    ccall((:fmpz_fdiv_q_2exp, :libflint), Nothing,
          (Ref{fmpz}, Ref{fmpz}, Int), z, x, c)
    return z
end

function fmodpow2(x::fmpz, c::Int)
    c < 0 && throw(DomainError("Exponent must be non-negative: $c"))
    z = fmpz()
    ccall((:fmpz_fdiv_r_2exp, :libflint), Nothing,
          (Ref{fmpz}, Ref{fmpz}, Int), z, x, c)
    return z
end

function cdivpow2(x::fmpz, c::Int)
    c < 0 && throw(DomainError("Exponent must be non-negative: $c"))
    z = fmpz()
    ccall((:fmpz_cdiv_q_2exp, :libflint), Nothing,
          (Ref{fmpz}, Ref{fmpz}, Int), z, x, c)
    return z
end

function div(x::fmpz, c::Int)
    c == 0 && throw(DivideError())
    z = fmpz()
    ccall((:fmpz_tdiv_q_si, :libflint), Nothing,
          (Ref{fmpz}, Ref{fmpz}, Int), z, x, c)
    return z
end

function tdiv(x::fmpz, c::Int)
    c == 0 && throw(DivideError())
    z = fmpz()
    ccall((:fmpz_tdiv_q_si, :libflint), Nothing,
          (Ref{fmpz}, Ref{fmpz}, Int), z, x, c)
    return z
end

function fdiv(x::fmpz, c::Int)
    c == 0 && throw(DivideError())
    z = fmpz()
    ccall((:fmpz_fdiv_q_si, :libflint), Nothing,
          (Ref{fmpz}, Ref{fmpz}, Int), z, x, c)
    return z
end

function cdiv(x::fmpz, c::Int)
    c == 0 && throw(DivideError())
    z = fmpz()
    ccall((:fmpz_cdiv_q_si, :libflint), Nothing,
          (Ref{fmpz}, Ref{fmpz}, Int), z, x, c)
    return z
end

rem(x::Integer, y::fmpz) = rem(fmpz(x), y)

mod(x::Integer, y::fmpz) = mod(fmpz(x), y)

div(x::Integer, y::fmpz) = div(fmpz(x), y)

###############################################################################
#
#   Division with remainder
#
###############################################################################

function divrem(x::fmpz, y::fmpz)
    iszero(y) && throw(DivideError())
    z1 = fmpz()
    z2 = fmpz()
    ccall((:fmpz_tdiv_qr, :libflint), Nothing,
          (Ref{fmpz}, Ref{fmpz}, Ref{fmpz}, Ref{fmpz}), z1, z2, x, y)
    z1, z2
end

function tdivrem(x::fmpz, y::fmpz)
    iszero(y) && throw(DivideError())
    z1 = fmpz()
    z2 = fmpz()
    ccall((:fmpz_tdiv_qr, :libflint), Nothing,
          (Ref{fmpz}, Ref{fmpz}, Ref{fmpz}, Ref{fmpz}), z1, z2, x, y)
    z1, z2
end

function fdivrem(x::fmpz, y::fmpz)
    iszero(y) && throw(DivideError())
    z1 = fmpz()
    z2 = fmpz()
    ccall((:fmpz_fdiv_qr, :libflint), Nothing,
          (Ref{fmpz}, Ref{fmpz}, Ref{fmpz}, Ref{fmpz}), z1, z2, x, y)
    z1, z2
end

###############################################################################
#
#   Powering
#
###############################################################################

function ^(x::fmpz, y::Int)
    if isone(x); return x; end
    if x == -1; return isodd(y) ? x : -x; end
    if y < 0; throw(DomainError("Exponent must be non-negative: $y")); end
    if y > typemax(UInt); throw(DomainError("Exponent too large")); end
    if y == 0; return one(FlintZZ); end
    if y == 1; return x; end
    z = fmpz()
    ccall((:fmpz_pow_ui, :libflint), Nothing, (Ref{fmpz}, Ref{fmpz}, UInt), z, x, UInt(y))
    return z
end

###############################################################################
#
#   Comparison
#
###############################################################################

function cmp(x::fmpz, y::fmpz)
    Int(ccall((:fmpz_cmp, :libflint), Cint,
              (Ref{fmpz}, Ref{fmpz}), x, y))
end

==(x::fmpz, y::fmpz) = cmp(x,y) == 0

<=(x::fmpz, y::fmpz) = cmp(x,y) <= 0

>=(x::fmpz, y::fmpz) = cmp(x,y) >= 0

<(x::fmpz, y::fmpz) = cmp(x,y) < 0

>(x::fmpz, y::fmpz) = cmp(x,y) > 0

function cmpabs(x::fmpz, y::fmpz)
    Int(ccall((:fmpz_cmpabs, :libflint), Cint,
              (Ref{fmpz}, Ref{fmpz}), x, y))
end

isless(x::fmpz, y::fmpz) = x < y

###############################################################################
#
#   Ad hoc comparison
#
###############################################################################

function cmp(x::fmpz, y::Int)
    Int(ccall((:fmpz_cmp_si, :libflint), Cint, (Ref{fmpz}, Int), x, y))
end

==(x::fmpz, y::Int) = cmp(x,y) == 0

<=(x::fmpz, y::Int) = cmp(x,y) <= 0

>=(x::fmpz, y::Int) = cmp(x,y) >= 0

<(x::fmpz, y::Int) = cmp(x,y) < 0

>(x::fmpz, y::Int) = cmp(x,y) > 0

==(x::Int, y::fmpz) = cmp(y,x) == 0

<=(x::Int, y::fmpz) = cmp(y,x) >= 0

>=(x::Int, y::fmpz) = cmp(y,x) <= 0

<(x::Int, y::fmpz) = cmp(y,x) > 0

>(x::Int, y::fmpz) = cmp(y,x) < 0

function cmp(x::fmpz, y::UInt)
    Int(ccall((:fmpz_cmp_ui, :libflint), Cint, (Ref{fmpz}, UInt), x, y))
end

==(x::fmpz, y::UInt) = cmp(x,y) == 0

<=(x::fmpz, y::UInt) = cmp(x,y) <= 0

>=(x::fmpz, y::UInt) = cmp(x,y) >= 0

<(x::fmpz, y::UInt) = cmp(x,y) < 0

>(x::fmpz, y::UInt) = cmp(x,y) > 0

==(x::UInt, y::fmpz) = cmp(y,x) == 0

<=(x::UInt, y::fmpz) = cmp(y,x) >= 0

>=(x::UInt, y::fmpz) = cmp(y,x) <= 0

<(x::UInt, y::fmpz) = cmp(y,x) > 0

>(x::UInt, y::fmpz) = cmp(y,x) < 0

###############################################################################
#
#   Shifting
#
###############################################################################

@doc Markdown.doc"""
    <<(x::fmpz, c::Int)
> Return $2^cx$ where $c \geq 0$.
"""
function <<(x::fmpz, c::Int)
    c < 0 && throw(DomainError("Exponent must be non-negative: $c"))
    c == 0 && return x
    z = fmpz()
    ccall((:fmpz_mul_2exp, :libflint), Nothing,
          (Ref{fmpz}, Ref{fmpz}, Int), z, x, c)
    maybe_finalizer(z)
    return z
end

@doc Markdown.doc"""
    >>(x::fmpz, c::Int)
> Return $x/2^c$, discarding any remainder, where $c \geq 0$.
"""
function >>(x::fmpz, c::Int)
    c < 0 && throw(DomainError("Exponent must be non-negative: $c"))
    c == 0 && return x
    z = fmpz()
    ccall((:fmpz_fdiv_q_2exp, :libflint), Nothing,
          (Ref{fmpz}, Ref{fmpz}, Int), z, x, c)
    return z
end

###############################################################################
#
#   Modular arithmetic
#
###############################################################################

@doc Markdown.doc"""
    mod(x::fmpz, y::fmpz)
> Return the remainder after division of $x$ by $y$. The remainder will be the
> least nonnegative remainder.
"""
function mod(x::fmpz, y::fmpz)
   z = fmpz()
   ccall((:fmpz_mod, :libflint), Nothing,
         (Ref{fmpz}, Ref{fmpz}, Ref{fmpz}), z, x, y)
   return z
end

@doc Markdown.doc"""
    mod(x::fmpz, y::Int)
> Return the remainder after division of $x$ by $y$. The remainder will be the
> least nonnegative remainder.
"""
function mod(x::fmpz, c::Int)
    c == 0 && throw(DivideError())
    if c > 0
        return Int(ccall((:fmpz_fdiv_ui, :libflint), Base.GMP.Limb, (Ref{fmpz}, Base.GMP.Limb), x, c))
    else
        r = ccall((:fmpz_fdiv_ui, :libflint), Base.GMP.Limb, (Ref{fmpz}, Base.GMP.Limb), x, -c)
        return r == 0 ? 0 : r + c
    end
end

@doc Markdown.doc"""
    mod(x::fmpz, y::UInt)
> Return the remainder after division of $x$ by $y$. The remainder will be the
> least nonnegative remainder.
"""
function mod(x::fmpz, c::UInt)
    c == 0 && throw(DivideError())
    ccall((:fmpz_fdiv_ui, :libflint), Base.GMP.Limb, (Ref{fmpz}, Base.GMP.Limb), x, c)
end

@doc Markdown.doc"""
    powmod(x::fmpz, p::fmpz, m::fmpz)
> Return $x^p (\mod m)$. The remainder will be in the range $[0, m)$
"""
function powmod(x::fmpz, p::fmpz, m::fmpz)
    m <= 0 && throw(DomainError("Exponent must be non-negative: $m"))
    if p < 0
       x = invmod(x, m)
       p = -p
    end
    r = fmpz()
    ccall((:fmpz_powm, :libflint), Nothing,
          (Ref{fmpz}, Ref{fmpz}, Ref{fmpz}, Ref{fmpz}),
          r, x, p, m)
    return r
end

@doc Markdown.doc"""
    powmod(x::fmpz, p::Int, m::fmpz)
> Return $x^p (\mod m)$. The remainder will be in the range $[0, m)$
"""
function powmod(x::fmpz, p::Int, m::fmpz)
    m <= 0 && throw(DomainError("Exponent must be non-negative: $m"))
    if p < 0
       x = invmod(x, m)
       p = -p
    end
    r = fmpz()
    ccall((:fmpz_powm_ui, :libflint), Nothing,
          (Ref{fmpz}, Ref{fmpz}, Int, Ref{fmpz}),
          r, x, p, m)
    return r
end

@doc Markdown.doc"""
    invmod(x::fmpz, m::fmpz)
> Return $x^{-1} (\mod m)$. The remainder will be in the range $[0, m)$
"""
function invmod(x::fmpz, m::fmpz)
    m <= 0 && throw(DomainError("Modulus must be non-negative: $m"))
    z = fmpz()
    if isone(m)
        return fmpz(0)
    end
    if ccall((:fmpz_invmod, :libflint), Cint,
             (Ref{fmpz}, Ref{fmpz}, Ref{fmpz}), z, x, m) == 0
       error("Impossible inverse in invmod")
    end
    return z
end

@doc Markdown.doc"""
    sqrtmod(x::fmpz, m::fmpz)
> Return a square root of $x (\mod m)$ if one exists. The remainder will be in
> the range $[0, m)$. We require that $m$ is prime, otherwise the algorithm may
> not terminate.
"""
function sqrtmod(x::fmpz, m::fmpz)
    m <= 0 && throw(DomainError("Modulus must be non-negative: $m"))
    z = fmpz()
    if (ccall((:fmpz_sqrtmod, :libflint), Cint,
              (Ref{fmpz}, Ref{fmpz}, Ref{fmpz}), z, x, m) == 0)
        error("no square root exists")
    end
    return z
end

@doc Markdown.doc"""
    crt(r1::fmpz, m1::fmpz, r2::fmpz, m2::fmpz, signed=false)
> Find $r$ such that $r \equiv r_1 (\mod m_1)$ and $r \equiv r_2 (\mod m_2)$.
> If `signed = true`, $r$ will be in the range $-m_1m_2/2 < r \leq m_1m_2/2$.
> If `signed = false` the value will be in the range $0 \leq r < m_1m_2$.
"""
function crt(r1::fmpz, m1::fmpz, r2::fmpz, m2::fmpz, signed=false)
   z = fmpz()
   ccall((:fmpz_CRT, :libflint), Nothing,
          (Ref{fmpz}, Ref{fmpz}, Ref{fmpz}, Ref{fmpz}, Ref{fmpz}, Cint),
          z, r1, m1, r2, m2, signed)
   return z
end

@doc Markdown.doc"""
    crt(r1::fmpz, m1::fmpz, r2::Int, m2::Int, signed=false)
> Find $r$ such that $r \equiv r_1 (\mod m_1)$ and $r \equiv r_2 (\mod m_2)$.
> If `signed = true`, $r$ will be in the range $-m_1m_2/2 < r \leq m_1m_2/2$.
> If `signed = false` the value will be in the range $0 \leq r < m_1m_2$.
"""
function crt(r1::fmpz, m1::fmpz, r2::Int, m2::Int, signed = false)
   z = fmpz()
   r2 < 0 && throw(DomainError("Second residue must be non-negative: $r2"))
   m2 < 0 && throw(DomainError("Second modulus must be non-negative: $m2"))
   ccall((:fmpz_CRT_ui, :libflint), Nothing,
          (Ref{fmpz}, Ref{fmpz}, Ref{fmpz}, Int, Int, Cint),
          z, r1, m1, r2, m2, signed)
   return z
end

###############################################################################
#
#   Integer logarithm
#
###############################################################################

@doc Markdown.doc"""
    flog(x::fmpz, c::fmpz)
> Return the floor of the logarithm of $x$ to base $c$.
"""
function flog(x::fmpz, c::fmpz)
    c <= 0 && throw(DomainError("Base must be non-negative: $c"))
    x <= 0 && throw(DomainError("Argument must be non-negative: $x"))
    return ccall((:fmpz_flog, :libflint), Int,
                 (Ref{fmpz}, Ref{fmpz}), x, c)
end

@doc Markdown.doc"""
    clog(x::fmpz, c::fmpz)
> Return the ceiling of the logarithm of $x$ to base $c$.
"""
function clog(x::fmpz, c::fmpz)
    c <= 0 && throw(DomainError("Base must be non-negative: $c"))
    x <= 0 && throw(DomainError("Argument must be non-negative: $x"))
    return ccall((:fmpz_clog, :libflint), Int,
                 (Ref{fmpz}, Ref{fmpz}), x, c)
end

@doc Markdown.doc"""
    flog(x::fmpz, c::Int)
> Return the floor of the logarithm of $x$ to base $c$.
"""
function flog(x::fmpz, c::Int)
    c <= 0 && throw(DomainError("Base must be non-negative: $c"))
    return ccall((:fmpz_flog_ui, :libflint), Int,
                 (Ref{fmpz}, Int), x, c)
end

@doc Markdown.doc"""
    clog(x::fmpz, c::Int)
> Return the ceiling of the logarithm of $x$ to base $c$.
"""
function clog(x::fmpz, c::Int)
    c <= 0 && throw(DomainError("Base must be non-negative: $c"))
    return ccall((:fmpz_clog_ui, :libflint), Int,
                 (Ref{fmpz}, Int), x, c)
end

###############################################################################
#
#   GCD and LCM
#
###############################################################################

@doc Markdown.doc"""
    gcd(x::fmpz, y::fmpz)
> Return the greatest common divisor of $x$ and $y$. The returned result will
> always be nonnegative and will be zero iff $x$ and $y$ are zero.
"""
function gcd(x::fmpz, y::fmpz)
   z = fmpz()
   ccall((:fmpz_gcd, :libflint), Nothing,
         (Ref{fmpz}, Ref{fmpz}, Ref{fmpz}), z, x, y)
   return z
end

@doc Markdown.doc"""
    gcd(x::Array{fmpz, 1})
> Return the greatest common divisor of the elements of $x$. The returned
> result will always be nonnegative and will be zero iff all elements of $x$
> are zero.
"""
function gcd(x::Array{fmpz, 1})
   if length(x) == 0
      error("Array must not be empty")
   elseif length(x) == 1
      return x[1]
   end

   z = fmpz()
   ccall((:fmpz_gcd, :libflint), Nothing,
         (Ref{fmpz}, Ref{fmpz}, Ref{fmpz}), z, x[1], x[2])

   for i in 3:length(x)
      ccall((:fmpz_gcd, :libflint), Nothing,
            (Ref{fmpz}, Ref{fmpz}, Ref{fmpz}), z, z, x[i])
      if isone(z)
         return z
      end
   end

   return z
end

@doc Markdown.doc"""
    lcm(x::fmpz, y::fmpz)
> Return the least common multiple of $x$ and $y$. The returned result will
> always be nonnegative and will be zero iff $x$ and $y$ are zero.
"""
function lcm(x::fmpz, y::fmpz)
   z = fmpz()
   ccall((:fmpz_lcm, :libflint), Nothing,
         (Ref{fmpz}, Ref{fmpz}, Ref{fmpz}), z, x, y)
   return z
end

@doc Markdown.doc"""
    lcm(x::Array{fmpz, 1})
> Return the least common multiple of the elements of $x$. The returned result
> will always be nonnegative and will be zero iff the elements of $x$ are zero.
"""
function lcm(x::Array{fmpz, 1})
   if length(x) == 0
      error("Array must not be empty")
   elseif length(x) == 1
      return x[1]
   end

   z = fmpz()
   ccall((:fmpz_lcm, :libflint), Nothing,
         (Ref{fmpz}, Ref{fmpz}, Ref{fmpz}), z, x[1], x[2])

   for i in 3:length(x)
      ccall((:fmpz_lcm, :libflint), Nothing,
            (Ref{fmpz}, Ref{fmpz}, Ref{fmpz}), z, z, x[i])
   end

   return z
end

###############################################################################
#
#   Extended GCD
#
###############################################################################

@doc Markdown.doc"""
    gcdx(a::fmpz, b::fmpz)
> Return a tuple $g, s, t$ such that $g$ is the greatest common divisor of $a$
> and $b$ and integers $s$ and $t$ such that $g = as + bt$.
"""
function gcdx(a::fmpz, b::fmpz)
    if iszero(b) # shortcut this to ensure consistent results with gcdx(a,b)
        return a < 0 ? (-a, -one(FlintZZ), zero(FlintZZ)) : (a, one(FlintZZ), zero(FlintZZ))
    end
    g = fmpz()
    s = fmpz()
    t = fmpz()
    ccall((:fmpz_xgcd, :libflint), Nothing,
        (Ref{fmpz}, Ref{fmpz}, Ref{fmpz}, Ref{fmpz}, Ref{fmpz}),
        g, s, t, a, b)
    g, s, t
end

@doc Markdown.doc"""
    gcdinv(a::fmpz, b::fmpz)
> Return a tuple $g, s$ where $g$ is the greatest common divisor of $a$ and
> $b$ and where $s$ is the inverse of $a$ modulo $b$ if $g = 1$. This function
> can be used to detect impossible inverses, i.e. where $a$ and $b$ are not
> coprime, and to yield the common factor of $a$ and $b$ if they are not
> coprime. We require $b \geq a \geq 0$.
"""
function gcdinv(a::fmpz, b::fmpz)
   a < 0 && throw(DomainError("First argument must be non-negative: $a"))
   b < a && throw(DomainError("First argument $a must be smaller than second argument $b"))
   g = fmpz()
   s = fmpz()
   ccall((:fmpz_gcdinv, :libflint), Nothing,
        (Ref{fmpz}, Ref{fmpz}, Ref{fmpz}, Ref{fmpz}),
        g, s, a, b)
   return g, s
end

###############################################################################
#
#   Roots
#
###############################################################################

@doc Markdown.doc"""
    isqrt(x::fmpz)
> Return the floor of the square root of $x$.
"""
function isqrt(x::fmpz)
    x < 0 && throw(DomainError("Argument must be non-negative: $x"))
    z = fmpz()
    ccall((:fmpz_sqrt, :libflint), Nothing, (Ref{fmpz}, Ref{fmpz}), z, x)
    return z
end

@doc Markdown.doc"""
    isqrtrem(x::fmpz)
> Return a tuple $s, r$ consisting of the floor $s$ of the square root of $x$
> and the remainder $r$, i.e. such that $x = s^2 + r$. We require $x \geq 0$.
"""
function isqrtrem(x::fmpz)
    x < 0 && throw(DomainError("Argument must be non-negative: $x"))
    s = fmpz()
    r = fmpz()
    ccall((:fmpz_sqrtrem, :libflint), Nothing,
          (Ref{fmpz}, Ref{fmpz}, Ref{fmpz}), s, r, x)
    return s, r
end

@doc Markdown.doc"""
    sqrt(x::fmpz)
> Return the square root $s$ of $x$ if $x$ is a square, otherwise raise an
> exception. We require $x \geq 0$.
"""
function Base.sqrt(x::fmpz)
    x < 0 && throw(DomainError("Argument must be non-negative: $x"))
    s = fmpz()
    r = fmpz()
    ccall((:fmpz_sqrtrem, :libflint), Nothing,
          (Ref{fmpz}, Ref{fmpz}, Ref{fmpz}), s, r, x)
    r != 0 && error("Not a square in sqrt")
    return s
end

@doc Markdown.doc"""
    root(x::fmpz, n::Int)
> Return the floor of the $n$-the root of $x$. We require $n > 0$ and that
> $x \geq 0$ if $n$ is even.
"""
function root(x::fmpz, n::Int)
   x < 0 && iseven(n) && throw(DomainError("Argument $x must be positive if exponent $n is even"))
   n <= 0 && throw(DomainError("Exponent must be non-negative: $n"))
   z = fmpz()
   ccall((:fmpz_root, :libflint), Nothing,
         (Ref{fmpz}, Ref{fmpz}, Int), z, x, n)
   return z
end

###############################################################################
#
#   Factorization
#
###############################################################################

function _factor(a::fmpz)
   F = fmpz_factor()
   ccall((:fmpz_factor, :libflint), Nothing, (Ref{fmpz_factor}, Ref{fmpz}), F, a)
   res = Dict{fmpz, Int}()
   for i in 1:F.num
     z = fmpz()
     ccall((:fmpz_factor_get_fmpz, :libflint), Nothing,
           (Ref{fmpz}, Ref{fmpz_factor}, Int), z, F, i - 1)
     res[z] = unsafe_load(F.exp, i)
   end
   return res, canonical_unit(a)
end

################################################################################
#
#   ECM
#
################################################################################

function _ecm(a::fmpz, B1::UInt, B2::UInt, ncrv::UInt,
             rnd = _flint_rand_states[Threads.threadid()])
  f = fmpz()
  r = ccall((:fmpz_factor_ecm, :libflint), Int32,
            (Ref{fmpz}, UInt, UInt, UInt, Ptr{Cvoid}, Ref{fmpz}),
            f, ncrv, B1, B2, rnd.ptr, a)
  return r, f
end

function _ecm(a::fmpz, B1::Int, B2::Int, ncrv::Int,
             rnd = _flint_rand_states[Threads.threadid()])
  return _ecm(a, UInt(B1), UInt(B2), UInt(ncrv), rnd)
end

function ecm(a::fmpz, max_digits::Int = div(ndigits(a), 2) + 1,
             rnd = _flint_rand_states[Threads.threadid()],
             B1 = _ecm_B1s[Threads.threadid()],
             nC = _ecm_nCs[Threads.threadid()])
  n = ndigits(a, 10)
  B1s = 15

  i = 1
  s = div(max_digits-15, 5) + 2
  s = max(i, s)
  while i <= s
    e, f = _ecm(a, B1[i]*1000, B1[i]*1000*100, nC[i], rnd)
    if e != 0
      return (e,f)
    end
    i += 1
    if i > length(B1)
      return (e, f)
    end
  end
  return (Int32(0), a)
end

################################################################################
#
#   Factor trial range
#
################################################################################

function _factor_trial_range(N::fmpz, start::Int = 0, np::Int = 10^5)
   F = fmpz_factor()
   ccall((:fmpz_factor_trial_range, :libflint), Nothing,
         (Ref{Nemo.fmpz_factor}, Ref{fmpz}, UInt, UInt), F, N, start, np)
   res = Dict{fmpz, Int}()
   for i in 1:F.num
     z = fmpz()
     ccall((:fmpz_factor_get_fmpz, :libflint), Nothing,
           (Ref{fmpz}, Ref{Nemo.fmpz_factor}, Int), z, F, i - 1)
     res[z] = unsafe_load(F.exp, i)
   end
   return res, canonical_unit(N)
end

@doc Markdown.doc"""
    factor(a::fmpz)
> Return a factorisation of $a$ using a `Fac` struct (see the documentation on
> factorisation in Nemo).
"""
function factor(a::fmpz)
   if iszero(a)
      throw(ArgumentError("Argument is not non-zero"))
   end
   fac, z = _factor(a)
   return Fac(z, fac)
end

###############################################################################
#
#   Number theoretic/combinatorial
#
###############################################################################

@doc Markdown.doc"""
    divisible(x::fmpz, y::fmpz)
> Return `true` if $x$ is divisible by $y$, otherwise return `false`. We
> require $x \neq 0$.
"""
function divisible(x::fmpz, y::fmpz)
   iszero(y) && throw(DivideError())
   Bool(ccall((:fmpz_divisible, :libflint), Cint,
              (Ref{fmpz}, Ref{fmpz}), x, y))
end

@doc Markdown.doc"""
    divisible(x::fmpz, y::Int)
> Return `true` if $x$ is divisible by $y$, otherwise return `false`. We
> require $x \neq 0$.
"""
function divisible(x::fmpz, y::Int)
   y == 0 && throw(DivideError())
   Bool(ccall((:fmpz_divisible_si, :libflint), Cint,
              (Ref{fmpz}, Int), x, y))
end

@doc Markdown.doc"""
    issquare(x::fmpz)
> Return `true` if $x$ is a square, otherwise return `false`.
"""
issquare(x::fmpz) = Bool(ccall((:fmpz_is_square, :libflint), Cint,
                               (Ref{fmpz},), x))

isprime(x::UInt) = Bool(ccall((:n_is_prime, :libflint), Cint, (UInt,), x))

@doc Markdown.doc"""
    isprime(x::fmpz)
> Return `true` if $x$ is a prime number, otherwise return `false`.
"""
function isprime(x::fmpz)
  !isprobable_prime(x) && return false
  return Bool(ccall((:fmpz_is_prime, :libflint), Cint, (Ref{fmpz},), x))
end

@doc Markdown.doc"""
    isprime(x::Int)
> Return `true` if $x$ is a prime number, otherwise return `false`.
"""
function isprime(n::Int)
  if n < 0
    return false
  end
  return isprime(n % UInt)
end

@doc Markdown.doc"""
    isprobable_prime(x::fmpz)
> Return `true` if $x$ is very probably a prime number, otherwise return
> `false`. No counterexamples are known to this test, but it is conjectured
> that infinitely many exist.
"""
isprobable_prime(x::fmpz) = Bool(ccall((:fmpz_is_probabprime, :libflint), Cint,
                                      (Ref{fmpz},), x))

@doc Markdown.doc"""
    remove(x::fmpz, y::fmpz)
> Return the tuple $n, z$ such that $x = y^nz$ where $y$ and $z$ are coprime.
"""
function remove(x::fmpz, y::fmpz)
   iszero(y) && throw(DivideError())
   z = fmpz()
   num = ccall((:fmpz_remove, :libflint), Int,
               (Ref{fmpz}, Ref{fmpz}, Ref{fmpz}), z, x, y)
   return num, z
end

remove(x::fmpz, y::Integer) = remove(x, fmpz(y))

remove(x::Integer, y::fmpz) = remove(fmpz(x), y)

remove(x::Integer, y::Integer) = remove(fmpz(x), fmpz(y))

@doc Markdown.doc"""
    valuation(x::fmpz, y::fmpz)
> Return the largest $n$ such that $y^n$ divides $x$.
"""
function valuation(x::fmpz, y::fmpz)
   n, _ = remove(x, y)
   return n
end

valuation(x::fmpz, y::Integer) = valuation(x, fmpz(y))

valuation(x::Integer, y::fmpz) = valuation(fmpz(x), y)

valuation(x::Integer, y::Integer) = valuation(fmpz(x), fmpz(y))

@doc Markdown.doc"""
    divisor_lenstra(n::fmpz, r::fmpz, m::fmpz)
> If $n$ has a factor which lies in the residue class $r (\mod m)$ for
> $0 < r < m < n$, this function returns such a factor. Otherwise it returns
> $0$. This is only efficient if $m$ is at least the cube root of $n$. We
> require gcd$(r, m) = 1$ and this condition is not checked.
"""
function divisor_lenstra(n::fmpz, r::fmpz, m::fmpz)
   r <= 0 && throw(DomainError("Residue class must be non-negative: $r"))
   m <= r && throw(DomainError("Modulus must be bigger than residue class: $m"))
   n <= m && throw(DomainError("Argument must be bigger than modulus: $n"))
   z = fmpz()
   if !Bool(ccall((:fmpz_divisor_in_residue_class_lenstra, :libflint),
       Cint, (Ref{fmpz}, Ref{fmpz}, Ref{fmpz}, Ref{fmpz}), z, n, r, m))
      z = 0
   end
   return z
end

@doc Markdown.doc"""
    fac(x::Int)
> Return the factorial of $x$, i.e. $x! = 1.2.3\ldots x$. We require
> $x \geq 0$.
"""
function fac(x::Int)
    x < 0 && throw(DomainError("Argument must be non-negative: $n"))
    z = fmpz()
    ccall((:fmpz_fac_ui, :libflint), Nothing, (Ref{fmpz}, UInt), z, x)
    return z
end

@doc Markdown.doc"""
    risingfac(x::fmpz, y::Int)
> Return the rising factorial of $x$, i.e. $x(x + 1)(x + 2)\ldots (x + n - 1)$.
> If $n < 0$ we throw a `DomainError()`.
"""
function risingfac(x::fmpz, y::Int)
    y < 0 && throw(DomainError("Argument must be non-negative: $y"))
    z = fmpz()
    ccall((:fmpz_rfac_ui, :libflint), Nothing,
          (Ref{fmpz}, Ref{fmpz}, UInt), z, x, y)
    return z
end

@doc Markdown.doc"""
    risingfac(x::Int, y::Int)
> Return the rising factorial of $x$, i.e. $x(x + 1)(x + 2)\ldots (x + n - 1)$.
> If $n < 0$ we throw a `DomainError()`.
"""
function risingfac(x::Int, y::Int)
    y < 0 && throw(DomainError("Argument must be non-negative: $y"))
    z = fmpz()
    if x < 0
       if y <= -x # we don't pass zero
          z = isodd(y) ? -risingfac(-x - y + 1, y) :
                          risingfac(-x - y + 1, y)
       end
    else
       ccall((:fmpz_rfac_uiui, :libflint), Nothing,
             (Ref{fmpz}, UInt, UInt), z, x, y)
    end
    return z
end

@doc Markdown.doc"""
    primorial(x::Int)
>  Return the primorial of $x$, i.e. the product of all primes less than or
> equal to $x$. If $x < 0$ we throw a `DomainError()`.
"""
function primorial(x::Int)
    x < 0 && throw(DomainError("Argument must be non-negative: $x"))
    z = fmpz()
    ccall((:fmpz_primorial, :libflint), Nothing,
          (Ref{fmpz}, UInt), z, x)
    return z
end

@doc Markdown.doc"""
    fib(x::Int)
>  Return the $x$-th Fibonacci number $F_x$. We define $F_1 = 1$, $F_2 = 1$ and
> $F_{i + 1} = F_i + F_{i - 1}$ for all $i > 2$. We require $n \geq 0$. For
> convenience, we define $F_0 = 0$.
"""
function fib(x::Int)
    x < 0 && throw(DomainError("Argument must be non-negative: $x"))
    z = fmpz()
    ccall((:fmpz_fib_ui, :libflint), Nothing,
          (Ref{fmpz}, UInt), z, x)
    return z
end

@doc Markdown.doc"""
    bell(x::Int)
> Return the Bell number $B_x$.
"""
function bell(x::Int)
    x < 0 && throw(DomainError("Argument must be non-negative: $x"))
    z = fmpz()
    ccall((:arith_bell_number, :libflint), Nothing,
          (Ref{fmpz}, UInt), z, x)
    return z
end

@doc Markdown.doc"""
    binom(n::Int, k::Int)
> Return the binomial coefficient $\frac{n!}{(n - k)!k!}$. If $n, k < 0$ or
> $k > n$ we return $0$.
"""
function binom(n::Int, k::Int)
    n < 0 && return fmpz(0)
    k < 0 && return fmpz(0)
    z = fmpz()
    ccall((:fmpz_bin_uiui, :libflint), Nothing,
          (Ref{fmpz}, UInt, UInt), z, n, k)
    return z
end

@doc Markdown.doc"""
    moebiusmu(x::fmpz)
> Returns the Moebius mu function of $x$ as an `Int`. The value
> returned is either $-1$, $0$ or $1$. If $x < 0$ we throw a `DomainError()`.
"""
function moebiusmu(x::fmpz)
   x < 0 && throw(DomainError("Argument must be non-negative: $x"))
   return Int(ccall((:fmpz_moebius_mu, :libflint), Cint,
                    (Ref{fmpz},), x))
end

@doc Markdown.doc"""
    jacobi(x::fmpz, y::fmpz)
> Return the value of the Jacobi symbol $\left(\frac{x}{y}\right)$. If
> $y \leq x$ or $x < 0$, we throw a `DomainError()`.
"""
function jacobi(x::fmpz, y::fmpz)
   y <= x && throw(DomainError("Second argument must be larger than first argument: $x $y"))
   x < 0 && throw(DomainError("First argument must be non-negative: $x"))
   return Int(ccall((:fmpz_jacobi, :libflint), Cint,
                    (Ref{fmpz}, Ref{fmpz}), x, y))
end

@doc Markdown.doc"""
    sigma(x::fmpz, y::Int)
> Return the value of the sigma function, i.e. $\sum_{0 < d \;| x} d^y$. If
> $y < 0$ we throw a `DomainError()`.
"""
function sigma(x::fmpz, y::Int)
   y < 0 && throw(DomainError("Second argument must be non-negative: $y"))
   z = fmpz()
   ccall((:fmpz_divisor_sigma, :libflint), Nothing,
         (Ref{fmpz}, Ref{fmpz}, Int), z, x, y)
   return z
end

@doc Markdown.doc"""
    eulerphi(x::fmpz)
> Return the value of the Euler phi function at $x$, i.e. the number of
> positive integers less than $x$ that are coprime with $x$.
"""
function eulerphi(x::fmpz)
   x < 0 && throw(DomainError("Argument must be non-negative: $x"))
   z = fmpz()
   ccall((:fmpz_euler_phi, :libflint), Nothing,
         (Ref{fmpz}, Ref{fmpz}), z, x)
   return z
end

@doc Markdown.doc"""
    numpart(x::Int)
> Return the number of partitions of $x$. This function is not available on
> Windows 64.
"""
function numpart(x::Int)
   if (Sys.iswindows() ? true : false) && Int == Int64
      error("not yet supported on win64")
   end
   x < 0 && throw(DomainError("Argument must be non-negative: $x"))
   z = fmpz()
   ccall((:partitions_fmpz_ui, :libarb), Nothing,
         (Ref{fmpz}, UInt), z, x)
   return z
end

@doc Markdown.doc"""
    numpart(x::fmpz)
> Return the number of partitions of $x$. This function is not available on
> Windows 64.
"""
function numpart(x::fmpz)
   if (Sys.iswindows() ? true : false) && Int == Int64
      error("not yet supported on win64")
   end
   x < 0 && throw(DomainError("Argument must be non-negative: $x"))
   z = fmpz()
   ccall((:partitions_fmpz_fmpz, :libarb), Nothing,
         (Ref{fmpz}, Ref{fmpz}, Int), z, x, 0)
   return z
end

###############################################################################
#
#   Number bases/digits
#
###############################################################################

@doc Markdown.doc"""
    bin(n::fmpz)
> Return $n$ as a binary string.
"""
bin(n::fmpz) = base(n, 2)

@doc Markdown.doc"""
    oct(n::fmpz)
> Return $n$ as a octal string.
"""
oct(n::fmpz) = base(n, 8)

@doc Markdown.doc"""
    dec(n::fmpz)
> Return $n$ as a decimal string.
"""
dec(n::fmpz) = base(n, 10)

@doc Markdown.doc"""
    hex(n::fmpz) = base(n, 16)
> Return $n$ as a hexadecimal string.
"""
hex(n::fmpz) = base(n, 16)

@doc Markdown.doc"""
    base(n::fmpz, b::Integer)
> Return $n$ as a string in base $b$. We require $2 \leq b \leq 62$.
"""
function base(n::fmpz, b::Integer)
    2 <= b <= 62 || error("invalid base: $b")
    p = ccall((:fmpz_get_str,:libflint), Ptr{UInt8},
              (Ptr{UInt8}, Cint, Ref{fmpz}), C_NULL, b, n)
    s = unsafe_string(p)
    ccall((:flint_free, :libflint), Nothing, (Ptr{UInt8},), p)
    return s
end

function ndigits_internal(x::fmpz, b::Integer = 10)
    # fmpz_sizeinbase might return an answer 1 too big
    n = Int(ccall((:fmpz_sizeinbase, :libflint), UInt,
                  (Ref{fmpz}, Int32), x, b))
    abs(x) < fmpz(b)^(n - 1) ? n - 1 : n
end

@doc Markdown.doc"""
    ndigits(x::fmpz, b::Integer = 10)
> Return the number of digits of $x$ in the base $b$ (default is $b = 10$).
"""
ndigits(x::fmpz, b::Integer = 10) = iszero(x) ? 1 : ndigits_internal(x, b)

@doc Markdown.doc"""
    nbits(x::fmpz)
> Return the number of binary bits of $x$. We return zero if $x = 0$.
"""
nbits(x::fmpz) = iszero(x) ? 0 : Int(ccall((:fmpz_sizeinbase, :libflint), UInt,
                  (Ref{fmpz}, Int32), x, 2))  # docu states: always correct
                                #if base is power of 2

###############################################################################
#
#   Bit fiddling
#
###############################################################################

@doc Markdown.doc"""
    popcount(x::fmpz)
> Return the number of ones in the binary representation of $x$.
"""
popcount(x::fmpz) = Int(ccall((:fmpz_popcnt, :libflint), UInt,
                              (Ref{fmpz},), x))

@doc Markdown.doc"""
    prevpow2(x::fmpz)
> Return the previous power of $2$ up to including $x$.
"""
prevpow2(x::fmpz) = x < 0 ? -prevpow2(-x) :
                            (x <= 2 ? x : one(FlintZZ) << (ndigits(x, 2) - 1))

@doc Markdown.doc"""
    nextpow2(x::fmpz)
> Return the next power of $2$ that is at least $x$.
"""
nextpow2(x::fmpz) = x < 0 ? -nextpow2(-x) :
                            (x <= 2 ? x : one(FlintZZ) << ndigits(x - 1, 2))

@doc Markdown.doc"""
    trailing_zeros(x::fmpz)
> Count the trailing zeros in the binary representation of $x$.
"""
trailing_zeros(x::fmpz) = ccall((:fmpz_val2, :libflint), Int,
                                (Ref{fmpz},), x)

###############################################################################
#
#   Bitwise operations (unsafe)
#
###############################################################################

@doc Markdown.doc"""
    clrbit!(x::fmpz, c::Int)
> Clear bit $c$ of $x$, where the least significant bit is the $0$-th bit. Note
> that this function modifies its input in-place.
"""
function clrbit!(x::fmpz, c::Int)
    c < 0 && throw(DomainError("Second argument must be non-negative: $c"))
    ccall((:fmpz_clrbit, :libflint), Nothing, (Ref{fmpz}, Int), x, c)
end

@doc Markdown.doc"""
    setbit!(x::fmpz, c::Int)
> Set bit $c$ of $x$, where the least significant bit is the $0$-th bit. Note
> that this function modifies its input in-place.
"""
function setbit!(x::fmpz, c::Int)
    c < 0 && throw(DomainError("Second argument must be non-negative: $c"))
    ccall((:fmpz_setbit, :libflint), Nothing, (Ref{fmpz}, Int), x, c)
end

@doc Markdown.doc"""
    combit!(x::fmpz, c::Int)
> Complement bit $c$ of $x$, where the least significant bit is the $0$-th bit.
> Note that this function modifies its input in-place.
"""
function combit!(x::fmpz, c::Int)
   c < 0 && throw(DomainError("Second argument must be non-negative: $c"))
   hasfinalizer = coeff_is_mpz(x)
   ccall((:fmpz_combit, :libflint), Nothing, (Ref{fmpz}, Int), x, c)
   !hasfinalizer && maybe_finalizer(x)
   nothing
end

###############################################################################
#
#   Unsafe operators
#
###############################################################################

function mul!(z::fmpz, x::fmpz, y::fmpz)
   ccall((:fmpz_mul, :libflint), Nothing,
         (Ref{fmpz}, Ref{fmpz}, Ref{fmpz}), z, x, y)
   return z
end

function addmul!(z::fmpz, x::fmpz, y::fmpz, c::fmpz)
   ccall((:fmpz_addmul, :libflint), Nothing,
         (Ref{fmpz}, Ref{fmpz}, Ref{fmpz}), z, x, y)
   return z
end

function addeq!(z::fmpz, x::fmpz)
   ccall((:fmpz_add, :libflint), Nothing,
         (Ref{fmpz}, Ref{fmpz}, Ref{fmpz}), z, z, x)
   return z
end

function add!(z::fmpz, x::fmpz, y::fmpz)
   ccall((:fmpz_add, :libflint), Nothing,
         (Ref{fmpz}, Ref{fmpz}, Ref{fmpz}), z, x, y)
   return z
end

function zero!(z::fmpz)
   ccall((:fmpz_zero, :libflint), Nothing,
         (Ref{fmpz},), z)
   return z
end

###############################################################################
#
#   Parent object overloads
#
###############################################################################

(::FlintIntegerRing)() = fmpz()

(::FlintIntegerRing)(a::Integer) = fmpz(a)

(::FlintIntegerRing)(a::AbstractString) = fmpz(a)

(::FlintIntegerRing)(a::fmpz) = a

(::FlintIntegerRing)(a::Float64) = fmpz(a)

(::FlintIntegerRing)(a::Float32) = fmpz(Float64(a))

(::FlintIntegerRing)(a::Float16) = fmpz(Float64(a))

(::FlintIntegerRing)(a::BigFloat) = fmpz(BigInt(a))

###############################################################################
#
#   String parser
#
###############################################################################

function parse(::Type{fmpz}, s::String, base::Int = 10)
    s = string(s)
    sgn = s[1] == '-' ? -1 : 1
    i = 1 + (sgn == -1)
    z = fmpz()
    err = ccall((:fmpz_set_str, :libflint),
               Int32, (Ref{fmpz}, Ptr{UInt8}, Int32),
               z, string(SubString(s, i)), base)
    err == 0 || error("Invalid big integer: $(repr(s))")
    return sgn < 0 ? -z : z
end

###############################################################################
#
#   Random generation
#
###############################################################################

function rand(rng::AbstractRNG, R::FlintIntegerRing, n::UnitRange{Int})
   return R(rand(rng, n))
end

###############################################################################
#
#   Constructors
#
###############################################################################

fmpz(s::AbstractString) = parse(fmpz, s)

fmpz(z::Integer) = fmpz(BigInt(z))

fmpz(z::Float16) = fmpz(Float64(z))

fmpz(z::Float32) = fmpz(Float64(z))

fmpz(z::BigFloat) = fmpz(BigInt(z))

###############################################################################
#
#   Conversions and promotions
#
###############################################################################

convert(::Type{fmpz}, a::Integer) = fmpz(a)

function (::Type{BigInt})(a::fmpz)
   r = BigInt()
   ccall((:fmpz_get_mpz, :libflint), Nothing, (Ref{BigInt}, Ref{fmpz}), r, a)
   return r
end

convert(::Type{BigInt}, a::fmpz) = BigInt(a)

function (::Type{Int})(a::fmpz)
   (a > typemax(Int) || a < typemin(Int)) && throw(InexactError())
   return ccall((:fmpz_get_si, :libflint), Int, (Ref{fmpz},), a)
end

convert(::Type{Int}, a::fmpz) = Int(a)

function (::Type{UInt})(a::fmpz)
   (a > typemax(UInt) || a < 0) && throw(InexactError())
   return ccall((:fmpz_get_ui, :libflint), UInt, (Ref{fmpz}, ), a)
end

convert(::Type{UInt}, a::fmpz) = UInt(a)

function (::Type{Float64})(n::fmpz)
    # rounds to zero
    ccall((:fmpz_get_d, :libflint), Float64, (Ref{fmpz},), n)
end

convert(::Type{Float64}, n::fmpz) = Float64(n)

(::Type{Float32})(n::fmpz) = Float32(Float64(n))

convert(::Type{Float32}, n::fmpz) = Float32(n)

(::Type{Float16})(n::fmpz) = Float16(Float64(n))

convert(::Type{Float16}, n::fmpz) = Float16(n)

(::Type{BigFloat})(n::fmpz) = BigFloat(BigInt(n))

convert(::Type{BigFloat}, n::fmpz) = BigFloat(n)

Base.promote_rule(::Type{fmpz}, ::Type{T}) where {T <: Integer} = fmpz

promote_rule(::Type{fmpz}, ::Type{T}) where {T <: Integer} = fmpz
