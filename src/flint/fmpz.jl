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
       binom, isprime, fdiv, cdiv, tdiv, div, rem, mod, gcd, xgcd, lcm, invmod,
       powmod, abs, divrem, isqrt, popcount, prevpow2, nextpow2, ndigits, dec,
       bin, oct, hex, base, one, zero, divexact, fits, sign, nbits, deepcopy,
       tdivpow2, fdivpow2, cdivpow2, flog, clog, cmpabs, clrbit!, setbit!,
       combit!, crt, divisible, divisor_lenstra, fdivrem, tdivrem, fmodpow2,
       gcdinv, isprobabprime, issquare, jacobi, remove, root, size, isqrtrem,
       sqrtmod, trailing_zeros, sigma, eulerphi, fib, moebiusmu, primorial,
       risingfac, numpart, canonical_unit, needs_parentheses, is_negative,
       isless, show_minus_one, parseint, addeq!, mul!, isunit, isequal, num, den

###############################################################################
#
#   Data type and parent methods
#
###############################################################################

parent_type(::Type{fmpz}) = FlintIntegerRing

parent(a::fmpz) = FlintZZ

elem_type(::FlintIntegerRing) = fmpz

base_ring(a::FlintIntegerRing) = Union{}

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
   return (unsigned(a) >> (WORD_SIZE - 2) !=1 )
end

function __fmpz_limbs(a::Int)
   if __fmpz_is_small(a)
      return 0
   end
   b = unsafe_load(convert(Ptr{Cint}, unsigned(a)<<2), 2)
   return b
end

function _hash_integer(a::Int, h::UInt)  
   s = __fmpz_limbs(a)
   s == 0 && return Base.hash_integer(a, h)
   # get the pointer after the first two Cint
   d = convert(Ptr{Ptr{UInt}}, unsigned(a) << 2) + 2*sizeof(Cint)
   p = unsafe_load(d)
   b = unsafe_load(p)
   h = Base.hash_uint(ifelse(s < 0, -b, b) $ h) $ h
   for k = 2:abs(s)
      h = Base.hash_uint(unsafe_load(p, k) $ h) $ h
   end
   return h
end

###############################################################################
#
#   Basic manipulation
#
###############################################################################

function deepcopy(a::fmpz)
   z = fmpz()
   ccall((:fmpz_set, :libflint), Void, (Ptr{fmpz}, Ptr{fmpz}), &z, &a)
   return z
end

one(::FlintIntegerRing) = fmpz(1)

zero(::FlintIntegerRing) = fmpz(0)

sign(a::fmpz) = Int(ccall((:fmpz_sgn, :libflint), Cint, (Ptr{fmpz},), &a))

fits(::Type{Int}, a::fmpz) = ccall((:fmpz_fits_si, :libflint), Bool, 
                                   (Ptr{fmpz},), &a)

fits(::Type{UInt}, a::fmpz) = sign(a) < 0 ? false : 
              ccall((:fmpz_abs_fits_ui, :libflint), Bool, (Ptr{fmpz},), &a)

size(a::fmpz) = Int(ccall((:fmpz_size, :libflint), Cint, (Ptr{fmpz},), &a))

isunit(a::fmpz) = ccall((:fmpz_is_pm1, :libflint), Bool, (Ptr{fmpz},), &a)

iszero(a::fmpz) = ccall((:fmpz_is_zero, :libflint), Bool, (Ptr{fmpz},), &a)

isone(a::fmpz) = ccall((:fmpz_is_one, :libflint), Bool, (Ptr{fmpz},), &a)

function den(a::fmpz)
   return fmpz(1)
end

function num(a::fmpz)
   return a
end

###############################################################################
#
#   Canonicalisation
#
###############################################################################

canonical_unit(x::fmpz) = x < 0 ? fmpz(-1) : fmpz(1)

###############################################################################
#
#   Binary operators and functions
#
###############################################################################

# Metaprogram to define functions +, -, *, gcd, lcm, 
#                                 &, |, $

for (fJ, fC) in ((:+, :add), (:-,:sub), (:*, :mul),
                 (:gcd, :gcd), (:lcm, :lcm),
                 (:&, :and), (:|, :or), (:$, :xor))
    @eval begin
        function ($fJ)(x::fmpz, y::fmpz)
            z = fmpz()
            ccall(($(string(:fmpz_, fC)), :libflint), Void, 
                  (Ptr{fmpz}, Ptr{fmpz}, Ptr{fmpz}), &z, &x, &y)
            return z
        end
    end
end

# Metaprogram to define functions fdiv, cdiv, tdiv, div, mod

for (fJ, fC) in ((:fdiv, :fdiv_q), (:cdiv, :cdiv_q), (:tdiv, :tdiv_q), 
                 (:div, :tdiv_q), (:mod, :mod))
    @eval begin
        function ($fJ)(x::fmpz, y::fmpz)
            y == 0 && throw(DivideError())
            z = fmpz()
            ccall(($(string(:fmpz_, fC)), :libflint), Void, 
                  (Ptr{fmpz}, Ptr{fmpz}, Ptr{fmpz}), &z, &x, &y)
            return z
        end
    end
end

function divexact(x::fmpz, y::fmpz)
    y == 0 && throw(DivideError())
    z = fmpz()
    ccall((:fmpz_divexact, :libflint), Void, 
          (Ptr{fmpz}, Ptr{fmpz}, Ptr{fmpz}), &z, &x, &y)
    z
end

function flog(x::fmpz, c::fmpz)
    c <= 0 && throw(DomainError())
    x <= 0 && throw(DomainError())
    return ccall((:fmpz_flog, :libflint), Int, 
                 (Ptr{fmpz}, Ptr{fmpz}), &x, &c)
end

function clog(x::fmpz, c::fmpz)
    c <= 0 && throw(DomainError())
    x <= 0 && throw(DomainError())
    return ccall((:fmpz_clog, :libflint), Int, 
                 (Ptr{fmpz}, Ptr{fmpz}), &x, &c)
end

function rem(x::fmpz, c::fmpz)
    c == 0 && throw(DivideError())
    q = fmpz()
    r = fmpz()
    ccall((:fmpz_tdiv_qr, :libflint), Void, 
          (Ptr{fmpz}, Ptr{fmpz}, Ptr{fmpz}, Ptr{fmpz}), &q, &r, &x, &c)
    return r
end

###############################################################################
#
#   Unsafe operators
#
###############################################################################

function mul!(z::fmpz, x::fmpz, y::fmpz)
   ccall((:fmpz_mul, :libflint), Void, 
         (Ptr{fmpz}, Ptr{fmpz}, Ptr{fmpz}), &z, &x, &y)
end

function addeq!(z::fmpz, x::fmpz)
   ccall((:fmpz_add, :libflint), Void, 
         (Ptr{fmpz}, Ptr{fmpz}, Ptr{fmpz}), &z, &z, &x)
end

function add!(z::fmpz, x::fmpz, y::fmpz)
   ccall((:fmpz_add, :libflint), Void, 
         (Ptr{fmpz}, Ptr{fmpz}, Ptr{fmpz}), &z, &x, &y)
end


###############################################################################
#
#   Ad hoc binary operators
#
###############################################################################

function +(x::fmpz, c::Int)
    z = fmpz()
    if c >= 0
       ccall((:fmpz_add_ui, :libflint), Void, 
             (Ptr{fmpz}, Ptr{fmpz}, Int), &z, &x, c)
    else
       ccall((:fmpz_sub_ui, :libflint), Void, 
             (Ptr{fmpz}, Ptr{fmpz}, Int), &z, &x, -c)
    end
    return z
end

+(c::Int, x::fmpz) = x + c

function -(x::fmpz, c::Int)
    z = fmpz()
    if c >= 0
       ccall((:fmpz_sub_ui, :libflint), Void, 
             (Ptr{fmpz}, Ptr{fmpz}, Int), &z, &x, c)
    else
       ccall((:fmpz_add_ui, :libflint), Void, 
             (Ptr{fmpz}, Ptr{fmpz}, Int), &z, &x, -c)
    end
    return z
end

function -(c::Int, x::fmpz)
    z = fmpz()
    if c >= 0
       ccall((:fmpz_sub_ui, :libflint), Void, 
             (Ptr{fmpz}, Ptr{fmpz}, Int), &z, &x, c)
    else
       ccall((:fmpz_add_ui, :libflint), Void, 
             (Ptr{fmpz}, Ptr{fmpz}, Int), &z, &x, -c)
    end
    ccall((:__fmpz_neg, :libflint), Void, 
          (Ptr{fmpz}, Ptr{fmpz}), &z, &z)
    return z
end

function *(x::fmpz, c::Int)
    z = fmpz()
    ccall((:fmpz_mul_si, :libflint), Void, 
          (Ptr{fmpz}, Ptr{fmpz}, Int), &z, &x, c)
    return z
end

*(c::Int, x::fmpz) = x * c

###############################################################################
#
#   Shifting
#
###############################################################################

function <<(x::fmpz, c::Int)
    c < 0 && throw(DomainError())
    c == 0 && return x
    z = fmpz()
    ccall((:fmpz_mul_2exp, :libflint), Void,
          (Ptr{fmpz}, Ptr{fmpz}, Int), &z, &x, c)
    return z
end

function >>(x::fmpz, c::Int)
    c < 0 && throw(DomainError())
    c == 0 && return x
    z = fmpz()
    ccall((:fmpz_fdiv_q_2exp, :libflint), Void, 
          (Ptr{fmpz}, Ptr{fmpz}, Int), &z, &x, c)
    return z
end

###############################################################################
#
#   Ad hoc division
#
###############################################################################

function rem(x::fmpz, c::Int)
    c < 0 && throw(DomainError())
    c == 0 && throw(DivideError())
    r = ccall((:fmpz_tdiv_ui, :libflint), Int, (Ptr{fmpz}, Int), &x, c)
    return sign(x) < 0 ? -r : r
end

function tdivpow2(x::fmpz, c::Int)
    c < 0 && throw(DomainError())
    z = fmpz()
    ccall((:fmpz_tdiv_q_2exp, :libflint), Void, 
          (Ptr{fmpz}, Ptr{fmpz}, Int), &z, &x, c)
    return z
end

function fdivpow2(x::fmpz, c::Int)
    c < 0 && throw(DomainError())
    z = fmpz()
    ccall((:fmpz_fdiv_q_2exp, :libflint), Void,
          (Ptr{fmpz}, Ptr{fmpz}, Int), &z, &x, c)
    return z
end

function fmodpow2(x::fmpz, c::Int)
    c < 0 && throw(DomainError())
    z = fmpz()
    ccall((:fmpz_fdiv_r_2exp, :libflint), Void, 
          (Ptr{fmpz}, Ptr{fmpz}, Int), &z, &x, c)
    return z
end

function cdivpow2(x::fmpz, c::Int)
    c < 0 && throw(DomainError())
    z = fmpz()
    ccall((:fmpz_cdiv_q_2exp, :libflint), Void, 
          (Ptr{fmpz}, Ptr{fmpz}, Int), &z, &x, c)
    return z
end

function div(x::fmpz, c::Int)
    c == 0 && throw(DivideError())
    z = fmpz()
    ccall((:fmpz_tdiv_q_si, :libflint), Void, 
          (Ptr{fmpz}, Ptr{fmpz}, Int), &z, &x, c)
    return z
end

function tdiv(x::fmpz, c::Int)
    c == 0 && throw(DivideError())
    z = fmpz()
    ccall((:fmpz_tdiv_q_si, :libflint), Void, 
          (Ptr{fmpz}, Ptr{fmpz}, Int), &z, &x, c)
    return z
end

function fdiv(x::fmpz, c::Int)
    c == 0 && throw(DivideError())
    z = fmpz()
    ccall((:fmpz_fdiv_q_si, :libflint), Void, 
          (Ptr{fmpz}, Ptr{fmpz}, Int), &z, &x, c)
    return z
end

function cdiv(x::fmpz, c::Int)
    c == 0 && throw(DivideError())
    z = fmpz()
    ccall((:fmpz_cdiv_q_si, :libflint), Void, 
          (Ptr{fmpz}, Ptr{fmpz}, Int), &z, &x, c)
    return z
end

function mod(x::fmpz, c::Int)
    c < 0 && throw(DomainError())
    c == 0 && throw(DivideError())
    ccall((:fmpz_fdiv_ui, :libflint), Int, (Ptr{fmpz}, Int), &x, c)
end

function divexact(x::fmpz, y::Int)
    y == 0 && throw(DivideError())
    z = fmpz()
    ccall((:fmpz_divexact_si, :libflint), Void, 
          (Ptr{fmpz}, Ptr{fmpz}, Int), &z, &x, y)
    z
end

###############################################################################
#
#   Powering
#
###############################################################################

function ^(x::fmpz, y::Int)
    if y < 0; throw(DomainError()); end
    if x == 1; return x; end
    if x == -1; return isodd(y) ? x : -x; end
    if y > typemax(UInt); throw(DomainError()); end
    if y == 0; return one(FlintZZ); end
    if y == 1; return x; end
    z = fmpz()
    ccall((:fmpz_pow_ui, :libflint), Void, (Ptr{fmpz}, Ptr{fmpz}, UInt), &z, &x, UInt(y))
    return z
end

###############################################################################
#
#   Unary operators and functions, e.g. -fmpz(12), ~fmpz(12)
#
###############################################################################

function -(x::fmpz)
    z = fmpz()
    ccall((:__fmpz_neg, :libflint), Void, (Ptr{fmpz}, Ptr{fmpz}), &z, &x)
    return z
end

function ~(x::fmpz)
    z = fmpz()
    ccall((:fmpz_complement, :libflint), Void, (Ptr{fmpz}, Ptr{fmpz}), &z, &x)
    return z
end

function abs(x::fmpz)
    z = fmpz()
    ccall((:fmpz_abs, :libflint), Void, (Ptr{fmpz}, Ptr{fmpz}), &z, &x)
    return z
end

###############################################################################
#
#   Division with remainder
#
###############################################################################

function divrem(x::fmpz, y::fmpz)
    y == 0 && throw(DivideError())
    z1 = fmpz()
    z2 = fmpz()
    ccall((:fmpz_tdiv_qr, :libflint), Void, 
          (Ptr{fmpz}, Ptr{fmpz}, Ptr{fmpz}, Ptr{fmpz}), &z1, &z2, &x, &y)
    z1, z2
end

function tdivrem(x::fmpz, y::fmpz)
    y == 0 && throw(DivideError())
    z1 = fmpz()
    z2 = fmpz()
    ccall((:fmpz_tdiv_qr, :libflint), Void,
          (Ptr{fmpz}, Ptr{fmpz}, Ptr{fmpz}, Ptr{fmpz}), &z1, &z2, &x, &y)
    z1, z2
end

function fdivrem(x::fmpz, y::fmpz)
    y == 0 && throw(DivideError())
    z1 = fmpz()
    z2 = fmpz()
    ccall((:fmpz_fdiv_qr, :libflint), Void, 
          (Ptr{fmpz}, Ptr{fmpz}, Ptr{fmpz}, Ptr{fmpz}), &z1, &z2, &x, &y)
    z1, z2
end

###############################################################################
#
#   Modular arithmetic
#
###############################################################################

function powmod(x::fmpz, p::fmpz, m::fmpz)
    m <= 0 && throw(DomainError())
    if p < 0
       x = invmod(x, m)
       p = -p
    end
    r = fmpz()
    ccall((:fmpz_powm, :libflint), Void,
          (Ptr{fmpz}, Ptr{fmpz}, Ptr{fmpz}, Ptr{fmpz}),
          &r, &x, &p, &m)
    return r 
end

function powmod(x::fmpz, p::Int, m::fmpz)
    m <= 0 && throw(DomainError())
    if p < 0
       x = invmod(x, m)
       p = -p
    end
    r = fmpz()
    ccall((:fmpz_powm_ui, :libflint), Void,
          (Ptr{fmpz}, Ptr{fmpz}, Int, Ptr{fmpz}),
          &r, &x, p, &m)
    return r 
end

function invmod(x::fmpz, y::fmpz)
    y <= 0 && throw(DomainError())
    z = fmpz()
    if y == 1
        return fmpz(0)
    end
    if ccall((:fmpz_invmod, :libflint), Cint, 
             (Ptr{fmpz}, Ptr{fmpz}, Ptr{fmpz}), &z, &x, &y) == 0
       error("Impossible inverse in invmod")
    end
    return z
end

function sqrtmod(x::fmpz, y::fmpz)
    y <= 0 && throw(DomainError())
    z = fmpz()
    if (ccall((:fmpz_sqrtmod, :libflint), Cint, 
              (Ptr{fmpz}, Ptr{fmpz}, Ptr{fmpz}), &z, &x, &y) == 0)
        error("no square root exists")
    end
    return z
end

function crt(r1::fmpz, m1::fmpz, r2::fmpz, m2::fmpz, signed=false)
   z = fmpz()
   ccall((:fmpz_CRT, :libflint), Void,
          (Ptr{fmpz}, Ptr{fmpz}, Ptr{fmpz}, Ptr{fmpz}, Ptr{fmpz}, Cint),
          &z, &r1, &m1, &r2, &m2, signed)
   return z
end

function crt(r1::fmpz, m1::fmpz, r2::Int, m2::Int, signed=false)
   z = fmpz()
   r2 < 0 && throw(DomainError())
   m2 < 0 && throw(DomainError())
   ccall((:fmpz_CRT_ui, :libflint), Void,
          (Ptr{fmpz}, Ptr{fmpz}, Ptr{fmpz}, Int, Int, Cint),
          &z, &r1, &m1, r2, m2, signed)
   return z
end

###############################################################################
#
#   Extended GCD
#
###############################################################################

function gcdx(a::fmpz, b::fmpz)
    if b == 0 # shortcut this to ensure consistent results with gcdx(a,b)
        return a < 0 ? (-a, -one(FlintZZ), zero(FlintZZ)) : (a, one(FlintZZ), zero(FlintZZ))
    end
    g = fmpz()
    s = fmpz()
    t = fmpz()
    ccall((:fmpz_xgcd, :libflint), Void,
        (Ptr{fmpz}, Ptr{fmpz}, Ptr{fmpz}, Ptr{fmpz}, Ptr{fmpz}),
        &g, &s, &t, &a, &b)
    g, s, t
end

function gcdinv(a::fmpz, b::fmpz)
   a < 0 && throw(DomainError())
   b < a && throw(DomainError())
   g = fmpz()
   s = fmpz()
   ccall((:fmpz_gcdinv, :libflint), Void,
        (Ptr{fmpz}, Ptr{fmpz}, Ptr{fmpz}, Ptr{fmpz}),
        &g, &s, &a, &b)
   return g, s
end

###############################################################################
#
#   Comparison
#
###############################################################################

function isless(x::fmpz, y::fmpz)
    Int(ccall((:fmpz_cmp, :libflint), Cint, 
              (Ptr{fmpz}, Ptr{fmpz}), &x, &y)) < 0
end

==(x::fmpz, y::fmpz) = cmp(x,y) == 0

<=(x::fmpz, y::fmpz) = cmp(x,y) <= 0

>=(x::fmpz, y::fmpz) = cmp(x,y) >= 0

<(x::fmpz, y::fmpz) = cmp(x,y) < 0

>(x::fmpz, y::fmpz) = cmp(x,y) > 0

function cmpabs(x::fmpz, y::fmpz)
    Int(ccall((:fmpz_cmpabs, :libflint), Cint, 
              (Ptr{fmpz}, Ptr{fmpz}), &x, &y))
end

###############################################################################
#
#   Ad hoc comparison
#
###############################################################################

function cmp(x::fmpz, y::Int)
    Int(ccall((:fmpz_cmp_si, :libflint), Cint, (Ptr{fmpz}, Int), &x, y))
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
    Int(ccall((:fmpz_cmp_ui, :libflint), Cint, (Ptr{fmpz}, UInt), &x, y))
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
#   Bit fiddling
#
###############################################################################

popcount(x::fmpz) = Int(ccall((:fmpz_popcnt, :libflint), UInt, 
                              (Ptr{fmpz},), &x))

prevpow2(x::fmpz) = x < 0 ? -prevpow2(-x) :
                            (x <= 2 ? x : one(FlintZZ) << (ndigits(x, 2) - 1))

nextpow2(x::fmpz) = x < 0 ? -nextpow2(-x) : 
                            (x <= 2 ? x : one(FlintZZ) << ndigits(x - 1, 2))

trailing_zeros(x::fmpz) = ccall((:fmpz_val2, :libflint), Int, 
                                (Ptr{fmpz},), &x)

###############################################################################
#
#   Bitwise operations (unsafe)
#
###############################################################################

function clrbit!(x::fmpz, c::Int)
    c < 0 && throw(DomainError())
    ccall((:fmpz_clrbit, :libflint), Void, (Ptr{fmpz}, Int), &x, c)
end

function setbit!(x::fmpz, c::Int)
    c < 0 && throw(DomainError())
    ccall((:fmpz_setbit, :libflint), Void, (Ptr{fmpz}, Int), &x, c)
end

function combit!(x::fmpz, c::Int)
    c < 0 && throw(DomainError())
    ccall((:fmpz_combit, :libflint), Void, (Ptr{fmpz}, Int), &x, c)
end

###############################################################################
#
#   Roots
#
###############################################################################

function isqrt(x::fmpz)
    x < 0 && throw(DomainError())
    z = fmpz()
    ccall((:fmpz_sqrt, :libflint), Void, (Ptr{fmpz}, Ptr{fmpz}), &z, &x)
    return z
end

function isqrtrem(x::fmpz)
    x < 0 && throw(DomainError())
    s = fmpz()
    r = fmpz()
    ccall((:fmpz_sqrtrem, :libflint), Void, 
          (Ptr{fmpz}, Ptr{fmpz}, Ptr{fmpz}), &s, &r, &x)
    return s, r
end

function root(x::fmpz, y::Int) 
   x < 0 && iseven(y) && throw(DomainError())
   y <= 0 && throw(DomainError())
   z = fmpz()
   ccall((:fmpz_root, :libflint), Void,
         (Ptr{fmpz}, Ptr{fmpz}, Int), &z, &x, y)
   return z
end

###############################################################################
#
#   Integer logarithm
#
###############################################################################

function flog(x::fmpz, c::Int)
    c <= 0 && throw(DomainError())
    return ccall((:fmpz_flog_ui, :libflint), Int, 
                 (Ptr{fmpz}, Int), &x, c)
end

function clog(x::fmpz, c::Int)
    c <= 0 && throw(DomainError())
    return ccall((:fmpz_clog_ui, :libflint), Int, 
                 (Ptr{fmpz}, Int), &x, c)
end
    
################################################################################
##
##   Array arithmetic
##
################################################################################
#
# CF: Julia has build-in mapreduce stuff. The native prod/ sum is actually
#     slightly better than this, it is the "same" algorithm as the
#     fmpz_vec_prod function in C.
#     If this becomes critical, I have an even better one in Hecke...
#function sum(arr::AbstractArray{fmpz})
#    n = fmpz(0)
#    for i in arr
#        ccall((:fmpz_add, :libflint), Void,
#            (Ptr{fmpz}, Ptr{fmpz}, Ptr{fmpz}),
#            &n, &n, &i)
#    end
#    return n
#end
#
#function prod(arr::AbstractArray{fmpz})
#    n = fmpz(1)
#    for i in arr
#        ccall((:fmpz_mul, :libflint), Void,
#            (Ptr{fmpz}, Ptr{fmpz}, Ptr{fmpz}),
#            &n, &n, &i)
#    end
#    return n
#end
#
###############################################################################
#
#   Number theoretic/combinatorial
#
###############################################################################

function divisible(x::fmpz, y::fmpz)
   y == 0 && throw(DivideError())
   Bool(ccall((:fmpz_divisible, :libflint), Cint, 
              (Ptr{fmpz}, Ptr{fmpz}), &x, &y))
end

function divisible(x::fmpz, y::Int)
   y == 0 && throw(DivideError())
   Bool(ccall((:fmpz_divisible_si, :libflint), Cint, 
              (Ptr{fmpz}, Int), &x, y))
end

issquare(x::fmpz) = Bool(ccall((:fmpz_is_square, :libflint), Cint, 
                               (Ptr{fmpz},), &x))

is_prime(x::UInt) = Bool(ccall((:n_is_prime, :libflint), Cint, (UInt,), x))
# flint's fmpz_is_prime doesn't work yet
isprime(x::fmpz) = Bool(ccall((:fmpz_is_probabprime, :libflint), Cint, 
                              (Ptr{fmpz},), &x))

isprobabprime(x::fmpz) = Bool(ccall((:fmpz_is_probabprime, :libflint), Cint, 
                                    (Ptr{fmpz},), &x))

function remove(x::fmpz, y::fmpz) 
   y == 0 && throw(DivideError())
   z = fmpz()
   num = ccall((:fmpz_remove, :libflint), Int, 
               (Ptr{fmpz}, Ptr{fmpz}, Ptr{fmpz}), &z, &x, &y)
   return num, z
end

function divisor_lenstra(n::fmpz, r::fmpz, m::fmpz)
   r <= 0 && throw(DomainError())
   m <= r && throw(DomainError())
   n <= m && throw(DomainError())
   z = fmpz()
   if !Bool(ccall((:fmpz_divisor_in_residue_class_lenstra, :libflint), 
       Cint, (Ptr{fmpz}, Ptr{fmpz}, Ptr{fmpz}, Ptr{fmpz}), &z, &n, &r, &m))
      z = 0
   end
   return z
end

function fac(x::Int)
    x < 0 && throw(DomainError())
    z = fmpz()
    ccall((:fmpz_fac_ui, :libflint), Void, (Ptr{fmpz}, UInt), &z, x)
    return z
end

function risingfac(x::fmpz, y::Int)
    y < 0 && throw(DomainError())
    z = fmpz()
    ccall((:fmpz_rfac_ui, :libflint), Void, 
          (Ptr{fmpz}, Ptr{fmpz}, UInt), &z, &x, y)
    return z
end

function risingfac(x::Int, y::Int)
    y < 0 && throw(DomainError())
    z = fmpz()
    if x < 0
       if y <= -x # we don't pass zero
          z = isodd(y) ? -risingfac(-x - y + 1, y) : 
                          risingfac(-x - y + 1, y)
       end
    else
       ccall((:fmpz_rfac_uiui, :libflint), Void, 
             (Ptr{fmpz}, UInt, UInt), &z, x, y)
    end
    return z
end

function primorial(x::Int)
    x < 0 && throw(DomainError()) 
    z = fmpz()
    ccall((:fmpz_primorial, :libflint), Void, 
          (Ptr{fmpz}, UInt), &z, x)
    return z
end

function fib(x::Int)
    x < 0 && throw(DomainError())
    z = fmpz()
    ccall((:fmpz_fib_ui, :libflint), Void, 
          (Ptr{fmpz}, UInt), &z, x)
    return z
end

function bell(x::Int)
    x < 0 && throw(DomainError())
    z = fmpz()
    ccall((:arith_bell_number, :libflint), Void, 
          (Ptr{fmpz}, UInt), &z, x)
    return z
end

function binom(n::Int, k::Int)
    n < 0 && return fmpz(0)
    k < 0 && return fmpz(0)
    z = fmpz()
    ccall((:fmpz_bin_uiui, :libflint), Void, 
          (Ptr{fmpz}, UInt, UInt), &z, n, k)
    return z
end

function moebiusmu(x::fmpz) 
   x < 0 && throw(DomainError())
   return Int(ccall((:fmpz_moebius_mu, :libflint), Cint, 
                    (Ptr{fmpz},), &x))
end

function jacobi(x::fmpz, y::fmpz) 
   y <= x && throw(DomainError())
   x < 0 && throw(DomainError())
   return Int(ccall((:fmpz_jacobi, :libflint), Cint, 
                    (Ptr{fmpz}, Ptr{fmpz}), &x, &y))
end

function sigma(x::fmpz, y::Int) 
   y < 0 && throw(DomainError())
   z = fmpz()
   ccall((:fmpz_divisor_sigma, :libflint), Void, 
         (Ptr{fmpz}, Ptr{fmpz}, Int), &z, &x, y)
   return z
end

function eulerphi(x::fmpz) 
   x < 0 && throw(DomainError())
   z = fmpz()
   ccall((:fmpz_euler_phi, :libflint), Void, 
         (Ptr{fmpz}, Ptr{fmpz}), &z, &x)
   return z
end

function numpart(x::Int) 
   if (@windows? true : false) && Int == Int64
      error("not yet supported on win64")
   end
   x < 0 && throw(DomainError())
   z = fmpz()
   ccall((:partitions_fmpz_ui, :libarb), Void, 
         (Ptr{fmpz}, UInt), &z, x)
   return z
end

function numpart(x::fmpz) 
   if (@windows? true : false) && Int == Int64
      error("not yet supported on win64")
   end
   x < 0 && throw(DomainError())
   z = fmpz()
   ccall((:partitions_fmpz_fmpz, :libarb), Void, 
         (Ptr{fmpz}, Ptr{fmpz}, Int), &z, &x, 0)
   return z
end

###############################################################################
#
#   AbstractString{} I/O
#
###############################################################################

string(x::fmpz) = dec(x)

show(io::IO, x::fmpz) = print(io, string(x))

show(io::IO, a::FlintIntegerRing) = print(io, "Integer Ring")

needs_parentheses(x::fmpz) = false

is_negative(x::fmpz) = x < 0

show_minus_one(::Type{fmpz}) = false

###############################################################################
#
#   Number bases/digits
#
###############################################################################

bin(n::fmpz) = base(n, 2)

oct(n::fmpz) = base(n, 8)

dec(n::fmpz) = base(n, 10)

hex(n::fmpz) = base(n, 16)

function base(n::fmpz, b::Integer)
    2 <= b <= 62 || error("invalid base: $b")
    p = ccall((:fmpz_get_str,:libflint), Ptr{UInt8}, 
              (Ptr{UInt8}, Cint, Ptr{fmpz}), C_NULL, b, &n)
    len = Int(ccall(:strlen, Csize_t, (Ptr{UInt8},), p))
    s = ASCIIString{}{}((pointer_to_array(p, len, false)))
    ccall((:flint_free, :libflint), Void, (Ptr{UInt8},), p)
    return s
end

function ndigits_internal(x::fmpz, b::Integer = 10)
    # fmpz_sizeinbase might return an answer 1 too big
    n = Int(ccall((:fmpz_sizeinbase, :libflint), UInt, 
                  (Ptr{fmpz}, Int32), &x, b))
    abs(x) < fmpz(b)^(n - 1) ? n - 1 : n
end

ndigits(x::fmpz, b::Integer = 10) = x == 0 ? 1 : ndigits_internal(x, b)

nbits(x::fmpz) = x == 0 ? 0 : Int(ccall((:fmpz_sizeinbase, :libflint), UInt,
                  (Ptr{fmpz}, Int32), &x, 2))  # docu states: always correct
                                #if base is power of 2

###############################################################################
#
#   Parent object overloads
#
###############################################################################

call(::FlintIntegerRing) = fmpz()

call(::FlintIntegerRing, a::Integer) = fmpz(a)

call(::FlintIntegerRing, a::AbstractString{}) = fmpz(a)

call(::FlintIntegerRing, a::fmpz) = a

call(::FlintIntegerRing, a::Float64) = fmpz(a)

call(::FlintIntegerRing, a::Float32) = fmpz(Float64(a))

call(::FlintIntegerRing, a::Float16) = fmpz(Float64(a))

call(::FlintIntegerRing, a::BigFloat) = fmpz(BigInt(a))

###############################################################################
#
#   AbstractString{} parser
#
###############################################################################

function parseint(::Type{fmpz}, s::AbstractString{}, base::Int = 10)
    s = bytestring(s)
    sgn = s[1] == '-' ? -1 : 1
    i = 1 + (sgn == -1)
    z = fmpz()
    err = ccall((:fmpz_set_str, :libflint),
               Int32, (Ptr{fmpz}, Ptr{UInt8}, Int32),
               &z, bytestring(SubString{}{}(s, i)), base)
    err == 0 || error("Invalid big integer: $(repr(s))")
    return sgn < 0 ? -z : z
end

###############################################################################
#
#   Constructors
#
###############################################################################

fmpz(s::AbstractString{}) = parseint(fmpz, s)

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

function convert(::Type{BigInt}, a::fmpz)
   r = BigInt()
   ccall((:fmpz_get_mpz, :libflint), Void, (Ptr{BigInt}, Ptr{fmpz}), &r, &a)
   return r
end

function convert(::Type{Int}, a::fmpz) 
   (a > typemax(Int) || a < typemin(Int)) && throw(InexactError())
   return ccall((:fmpz_get_si, :libflint), Int, (Ptr{fmpz},), &a)
end

function convert(::Type{UInt}, a::fmpz)
   (a > typemax(UInt) || a < 0) && throw(InexactError())
   return ccall((:fmpz_get_ui, :libflint), UInt, (Ptr{fmpz}, ), &a)
end

function convert(::Type{Float64}, n::fmpz)
    # rounds to zero
    ccall((:fmpz_get_d, :libflint), Float64, (Ptr{fmpz},), &n)
end

convert(::Type{Float32}, n::fmpz) = Float32(Float64(n))

convert(::Type{Float16}, n::fmpz) = Float16(Float64(n))

convert(::Type{BigFloat}, n::fmpz) = BigFloat(BigInt(n))

Base.promote_rule{T <: Integer}(::Type{fmpz}, ::Type{T}) = fmpz
