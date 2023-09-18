###############################################################################
#
#   ZZRingElem.jl : BigInts
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

# do not export div and divrem
export ZZRingElem, FlintZZ, ZZRing, parent, show, convert, hash, bell,
       is_perfect_power, is_prime, is_prime_power_with_data, fdiv, cdiv, tdiv, rem, mod, gcd, lcm, invmod, powermod, abs,
       isqrt, popcount, prevpow2, nextpow2, ndigits, dec, bin, oct, hex, base,
       one, zero, divexact, fits, sign, nbits, deepcopy, tdivpow2, fdivpow2,
       cdivpow2, flog, clog, cmpabs, clrbit!, setbit!, combit!, crt,
       crt_with_lcm, divisible, divisors, prime_divisors, divisor_lenstra,
       fmodpow2,
       gcdinv, gcd_with_cofactors,
       is_probable_prime, jacobi_symbol, kronecker_symbol, remove, root, size,
       isqrtrem, sqrtmod, trailing_zeros, divisor_sigma, euler_phi, fibonacci,
       mod!, moebius_mu, primorial, rising_factorial, number_of_partitions,
       canonical_unit, is_unit, isequal, addeq!, mul!, fmma!, fmms!, is_square,
       sqrt, is_square_with_sqrt, next_prime, ndivrem, iszero, rand, rand_bits,
       binomial, factorial, rand_bits_prime, iroot, tdivrem, fdivrem, cdivrem,
       ntdivrem, nfdivrem, ncdivrem, tstbit, neg!, lcm!, gcd!, submul!

###############################################################################
#
#   Data type and parent methods
#
###############################################################################

parent_type(::Type{ZZRingElem}) = ZZRing

@doc raw"""
    parent(a::ZZRingElem)

Returns the unique Flint integer parent object `FlintZZ`.
"""
parent(a::ZZRingElem) = FlintZZ

elem_type(::Type{ZZRing}) = ZZRingElem

@doc raw"""
    base_ring(a::ZZRing)

Returns `Union{}` as this ring is not dependent on another ring.
"""
base_ring(a::ZZRing) = Union{}

@doc raw"""
    base_ring(a::ZZRingElem)

Returns `Union{}` as the parent ring is not dependent on another ring.
"""
base_ring(a::ZZRingElem) = Union{}

is_domain_type(::Type{ZZRingElem}) = true

###############################################################################
#
#   Misc.
#
###############################################################################

# `length` should return an Integer, so BigInt seems appropriate as ZZRingElem is not <: Integer
# this method is useful in particular to enable rand(ZZ(n):ZZ(m))
function Base.length(r::StepRange{ZZRingElem})
    n = div((last(r) - first(r)) + step(r), step(r))
    isempty(r) ? zero(BigInt) : BigInt(n)
end

################################################################################
#
#   Hashing
#
################################################################################

# Similar to hash for BigInt found in julia/base

@inline function _fmpz_is_small(a::ZZRingElem)
   return __fmpz_is_small(a.d)
end

@inline function _fmpz_limbs(a::ZZRingElem)
   return __fmpz_limbs(a.d)
end

function hash_integer(a::ZZRingElem, h::UInt)
   return _hash_integer(a.d, h)
end

function hash(a::ZZRingElem, h::UInt)
   return hash_integer(a, h)
end

@inline function __fmpz_is_small(a::Int)
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

function deepcopy_internal(a::ZZRingElem, dict::IdDict)
   z = ZZRingElem()
   ccall((:fmpz_set, libflint), Nothing, (Ref{ZZRingElem}, Ref{ZZRingElem}), z, a)
   return z
end

characteristic(R::ZZRing) = 0

one(::ZZRing) = ZZRingElem(1)

zero(::ZZRing) = ZZRingElem(0)

one(::Type{ZZRingElem}) = ZZRingElem(1)

zero(::Type{ZZRingElem}) = ZZRingElem(0)

one(::ZZRingElem) = ZZRingElem(1)

zero(::ZZRingElem) = ZZRingElem(0)

@doc raw"""
    sign(a::ZZRingElem)

Return the sign of $a$, i.e. $+1$, $0$ or $-1$.
"""
sign(a::ZZRingElem) = ZZRingElem(ccall((:fmpz_sgn, libflint), Cint, (Ref{ZZRingElem},), a))

sign(::Type{Int}, a::ZZRingElem) = Int(ccall((:fmpz_sgn, libflint), Cint, (Ref{ZZRingElem},), a))

Base.signbit(a::ZZRingElem) = signbit(sign(Int, a))

@doc raw"""
    fits(::Type{Int}, a::ZZRingElem)

Return `true` if $a$ fits into an `Int`, otherwise return `false`.
"""
fits(::Type{Int}, a::ZZRingElem) = ccall((:fmpz_fits_si, libflint), Bool,
                                   (Ref{ZZRingElem},), a)

@doc raw"""
    fits(::Type{UInt}, a::ZZRingElem)

Return `true` if $a$ fits into a `UInt`, otherwise return `false`.
"""
fits(::Type{UInt}, a::ZZRingElem) = a < 0 ? false :
              ccall((:fmpz_abs_fits_ui, libflint), Bool, (Ref{ZZRingElem},), a)

if Culong !== UInt
    function fits(::Type{Culong}, a::ZZRingElem)
        return 0 <= a && a <= UInt(typemax(Culong))
    end
end

@doc raw"""
    size(a::ZZRingElem)

Return the number of limbs required to store the absolute value of $a$.
"""
size(a::ZZRingElem) = Int(ccall((:fmpz_size, libflint), Cint, (Ref{ZZRingElem},), a))

is_unit(a::ZZRingElem) = ccall((:fmpz_is_pm1, libflint), Bool, (Ref{ZZRingElem},), a)

iszero(a::ZZRingElem) = ccall((:fmpz_is_zero, libflint), Bool, (Ref{ZZRingElem},), a)

isone(a::ZZRingElem) = ccall((:fmpz_is_one, libflint), Bool, (Ref{ZZRingElem},), a)

isinteger(::ZZRingElem) = true

isfinite(::ZZRingElem) = true

isinf(::ZZRingElem) = false

@doc raw"""
    denominator(a::ZZRingElem)

Return the denominator of $a$ thought of as a rational. Always returns $1$.
"""
function denominator(a::ZZRingElem)
   return ZZRingElem(1)
end

@doc raw"""
    numerator(a::ZZRingElem)

Return the numerator of $a$ thought of as a rational. Always returns $a$.
"""
function numerator(a::ZZRingElem)
   return a
end

isodd(a::ZZRingElem)  = ccall((:fmpz_is_odd,  libflint), Cint, (Ref{ZZRingElem},), a) % Bool
iseven(a::ZZRingElem) = ccall((:fmpz_is_even, libflint), Cint, (Ref{ZZRingElem},), a) % Bool

###############################################################################
#
#   AbstractString I/O
#
###############################################################################

# ZZRingElem is allowed as a leaf, and the following code is needed by AA's api
expressify(x::ZZRingElem; context = nothing) = x

function AbstractAlgebra.get_syntactic_sign_abs(obj::ZZRingElem)
    return obj < 0 ? (-1, -obj) : (1, obj)
end

AbstractAlgebra.is_syntactic_one(x::ZZRingElem) = isone(x)

AbstractAlgebra.is_syntactic_zero(x::ZZRingElem) = iszero(x)

function AbstractAlgebra.print_obj(S::AbstractAlgebra.printer, mi::MIME,
                                               obj::ZZRingElem, left::Int, right::Int)
   AbstractAlgebra.print_integer_string(S, mi, string(obj), left, right)
end

string(x::ZZRingElem) = dec(x)

show(io::IO, x::ZZRingElem) = print(io, string(x))

function show(io::IO, a::ZZRing)
   if get(io, :supercompact, false)
      io = pretty(io)
      # no nested printing
      print(io, LowercaseOff(), "ZZ")
   else
      # nested printing allowed, preferably supercompact
      print(io, "Integer ring")
   end
end

###############################################################################
#
#   Canonicalisation
#
###############################################################################

canonical_unit(x::ZZRingElem) = x < 0 ? ZZRingElem(-1) : ZZRingElem(1)

###############################################################################
#
#   Unary operators and functions, e.g. -ZZRingElem(12), ~ZZRingElem(12)
#
###############################################################################

function -(x::ZZRingElem)
    z = ZZRingElem()
    ccall((:fmpz_neg, libflint), Nothing, (Ref{ZZRingElem}, Ref{ZZRingElem}), z, x)
    return z
end

function ~(x::ZZRingElem)
    z = ZZRingElem()
    ccall((:fmpz_complement, libflint), Nothing, (Ref{ZZRingElem}, Ref{ZZRingElem}), z, x)
    return z
end

function abs(x::ZZRingElem)
    z = ZZRingElem()
    ccall((:fmpz_abs, libflint), Nothing, (Ref{ZZRingElem}, Ref{ZZRingElem}), z, x)
    return z
end

floor(x::ZZRingElem) = x

ceil(x::ZZRingElem) = x

floor(::Type{ZZRingElem}, x::ZZRingElem) = x

ceil(::Type{ZZRingElem}, x::ZZRingElem) = x

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
        function ($fJ)(x::ZZRingElem, y::ZZRingElem)
            z = ZZRingElem()
            ccall(($(string(:fmpz_, fC)), libflint), Nothing,
                  (Ref{ZZRingElem}, Ref{ZZRingElem}, Ref{ZZRingElem}), z, x, y)
            return z
        end
    end
end

# Metaprogram to define functions fdiv, cdiv, tdiv, div

for (fJ, fC) in ((:fdiv, :fdiv_q), (:cdiv, :cdiv_q), (:tdiv, :tdiv_q),
                 (:div, :fdiv_q))
    @eval begin
        function ($fJ)(x::ZZRingElem, y::ZZRingElem)
            iszero(y) && throw(DivideError())
            z = ZZRingElem()
            ccall(($(string(:fmpz_, fC)), libflint), Nothing,
                  (Ref{ZZRingElem}, Ref{ZZRingElem}, Ref{ZZRingElem}), z, x, y)
            return z
        end
    end
end

# N.B. we do not export the internal definition of div
# which agrees with the internal definition of AbstractAlgebra
# Here we set Base.div to a version that agrees with Base
function Base.div(x::ZZRingElem, y::ZZRingElem)
    iszero(y) && throw(DivideError())
    z = ZZRingElem()
    ccall((:fmpz_tdiv_q, libflint), Nothing,
	  (Ref{ZZRingElem}, Ref{ZZRingElem}, Ref{ZZRingElem}), z, x, y)
    return z
end

function divexact(x::ZZRingElem, y::ZZRingElem; check::Bool=true)
    iszero(y) && throw(DivideError())
    z = ZZRingElem()
    if check
       r = ZZRingElem()
       ccall((:fmpz_tdiv_qr, libflint), Nothing,
             (Ref{ZZRingElem}, Ref{ZZRingElem}, Ref{ZZRingElem}, Ref{ZZRingElem}), z, r, x, y)
       r != 0 && throw(ArgumentError("Not an exact division"))
    else
       ccall((:fmpz_divexact, libflint), Nothing,
                           (Ref{ZZRingElem}, Ref{ZZRingElem}, Ref{ZZRingElem}), z, x, y)
    end
    return z
end

function divides(x::ZZRingElem, y::ZZRingElem)
   z = ZZRingElem()
   res = ccall((:fmpz_divides, libflint), Bool,
                           (Ref{ZZRingElem}, Ref{ZZRingElem}, Ref{ZZRingElem}), z, x, y)
   return res, z
end

divides(x::ZZRingElem, y::Integer) = divides(x, ZZRingElem(y))

function is_divisible_by(x::ZZRingElem, y::ZZRingElem)
   if iszero(x)
      return true
   elseif iszero(y)
      return false
   elseif iseven(y) && isodd(x)
      return false
   elseif nbits(y) > nbits(x)
      return false
   else
      flag, q = divides(x, y)
      return flag
   end
end

function is_divisible_by(x::ZZRingElem, y::Integer)
   if iszero(x)
      return true
   elseif iszero(y)
      return false
   elseif iseven(y) && isodd(x)
      return false
   elseif ndigits(y, base=2) > nbits(x)
      return false
   else
      r = mod(x, y)
      return r == 0
   end
end

function is_divisible_by(x::Integer, y::ZZRingElem)
   return is_divisible_by(ZZRingElem(x), y)
end

function rem(x::ZZRingElem, c::ZZRingElem)
    iszero(c) && throw(DivideError())
    q = ZZRingElem()
    r = ZZRingElem()
    ccall((:fmpz_tdiv_qr, libflint), Nothing,
          (Ref{ZZRingElem}, Ref{ZZRingElem}, Ref{ZZRingElem}, Ref{ZZRingElem}), q, r, x, c)
    return r
end

function rem(a::ZZRingElem, b::UInt)
   return ccall((:fmpz_fdiv_ui, libflint), UInt, (Ref{ZZRingElem}, UInt), a, b)
end

###############################################################################
#
#   Ad hoc binary operators
#
###############################################################################

function +(x::ZZRingElem, c::Int)
    z = ZZRingElem()
    if c >= 0
       ccall((:fmpz_add_ui, libflint), Nothing,
             (Ref{ZZRingElem}, Ref{ZZRingElem}, Int), z, x, c)
    else
       ccall((:fmpz_sub_ui, libflint), Nothing,
             (Ref{ZZRingElem}, Ref{ZZRingElem}, Int), z, x, -c)
    end
    return z
end

+(c::Int, x::ZZRingElem) = x + c

function -(x::ZZRingElem, c::Int)
    z = ZZRingElem()
    if c >= 0
       ccall((:fmpz_sub_ui, libflint), Nothing,
             (Ref{ZZRingElem}, Ref{ZZRingElem}, Int), z, x, c)
    else
       ccall((:fmpz_add_ui, libflint), Nothing,
             (Ref{ZZRingElem}, Ref{ZZRingElem}, Int), z, x, -c)
    end
    return z
end

function -(c::Int, x::ZZRingElem)
    z = ZZRingElem()
    if c >= 0
       ccall((:fmpz_sub_ui, libflint), Nothing,
             (Ref{ZZRingElem}, Ref{ZZRingElem}, Int), z, x, c)
    else
       ccall((:fmpz_add_ui, libflint), Nothing,
             (Ref{ZZRingElem}, Ref{ZZRingElem}, Int), z, x, -c)
    end
    ccall((:fmpz_neg, libflint), Nothing,
          (Ref{ZZRingElem}, Ref{ZZRingElem}), z, z)
    return z
end

function *(x::ZZRingElem, c::Int)
    z = ZZRingElem()
    ccall((:fmpz_mul_si, libflint), Nothing,
          (Ref{ZZRingElem}, Ref{ZZRingElem}, Int), z, x, c)
    return z
end

*(c::Int, x::ZZRingElem) = x * c

+(a::ZZRingElem, b::Integer) = a + ZZRingElem(b)

+(a::Integer, b::ZZRingElem) = ZZRingElem(a) + b

-(a::ZZRingElem, b::Integer) = a - ZZRingElem(b)

-(a::Integer, b::ZZRingElem) = ZZRingElem(a) - b

*(a::ZZRingElem, b::Integer) = a*ZZRingElem(b)

*(a::Integer, b::ZZRingElem) = ZZRingElem(a)*b

###############################################################################
#
#   Ad hoc exact division
#
###############################################################################

divexact(x::ZZRingElem, y::Integer; check::Bool=true) = divexact(x, ZZRingElem(y); check=check)

divexact(x::Integer, y::ZZRingElem; check::Bool=true) = divexact(ZZRingElem(x), y; check=check)

###############################################################################
#
#   Ad hoc division
#
###############################################################################

function tdivpow2(x::ZZRingElem, c::Int)
    c < 0 && throw(DomainError(c, "Exponent must be non-negative"))
    z = ZZRingElem()
    ccall((:fmpz_tdiv_q_2exp, libflint), Nothing,
          (Ref{ZZRingElem}, Ref{ZZRingElem}, Int), z, x, c)
    return z
end

function fdivpow2(x::ZZRingElem, c::Int)
    c < 0 && throw(DomainError(c, "Exponent must be non-negative"))
    z = ZZRingElem()
    ccall((:fmpz_fdiv_q_2exp, libflint), Nothing,
          (Ref{ZZRingElem}, Ref{ZZRingElem}, Int), z, x, c)
    return z
end

function fmodpow2(x::ZZRingElem, c::Int)
    c < 0 && throw(DomainError(c, "Exponent must be non-negative"))
    z = ZZRingElem()
    ccall((:fmpz_fdiv_r_2exp, libflint), Nothing,
          (Ref{ZZRingElem}, Ref{ZZRingElem}, Int), z, x, c)
    return z
end

function cdivpow2(x::ZZRingElem, c::Int)
    c < 0 && throw(DomainError(c, "Exponent must be non-negative"))
    z = ZZRingElem()
    ccall((:fmpz_cdiv_q_2exp, libflint), Nothing,
          (Ref{ZZRingElem}, Ref{ZZRingElem}, Int), z, x, c)
    return z
end

function tdiv(x::ZZRingElem, c::Int)
    c == 0 && throw(DivideError())
    z = ZZRingElem()
    ccall((:fmpz_tdiv_q_si, libflint), Nothing,
          (Ref{ZZRingElem}, Ref{ZZRingElem}, Int), z, x, c)
    return z
end

function fdiv(x::ZZRingElem, c::Int)
    c == 0 && throw(DivideError())
    z = ZZRingElem()
    ccall((:fmpz_fdiv_q_si, libflint), Nothing,
          (Ref{ZZRingElem}, Ref{ZZRingElem}, Int), z, x, c)
    return z
end

function cdiv(x::ZZRingElem, c::Int)
    c == 0 && throw(DivideError())
    z = ZZRingElem()
    ccall((:fmpz_cdiv_q_si, libflint), Nothing,
          (Ref{ZZRingElem}, Ref{ZZRingElem}, Int), z, x, c)
    return z
end

rem(x::Integer, y::ZZRingElem) = rem(ZZRingElem(x), y)

rem(x::ZZRingElem, y::Integer) = rem(x, ZZRingElem(y))

mod(x::Integer, y::ZZRingElem) = mod(ZZRingElem(x), y)

@doc raw"""
    mod(x::ZZRingElem, y::Integer)

Return the remainder after division of $x$ by $y$. The remainder will be
closer to zero than $y$ and have the same sign, or it will be zero.
"""
mod(x::ZZRingElem, y::Integer) = mod(x, ZZRingElem(y))

div(x::Integer, y::ZZRingElem) = div(ZZRingElem(x), y)

# Note Base.div is different to Nemo.div
Base.div(x::Integer, y::ZZRingElem) = Base.div(ZZRingElem(x), y)

div(x::ZZRingElem, y::Integer) = div(x, ZZRingElem(y))

# Note Base.div is different to Nemo.div
Base.div(x::ZZRingElem, y::Integer) = Base.div(x, ZZRingElem(y))

divrem(x::ZZRingElem, y::Integer) = divrem(x, ZZRingElem(y))

divrem(x::Integer, y::ZZRingElem) = divrem(ZZRingElem(x), y)

Base.divrem(x::ZZRingElem, y::Int) = (Base.div(x, y), Base.rem(x, y))

###############################################################################
#
#   Division with remainder
#
###############################################################################

function divrem(x::ZZRingElem, y::ZZRingElem)
    iszero(y) && throw(DivideError())
    z1 = ZZRingElem()
    z2 = ZZRingElem()
    ccall((:fmpz_fdiv_qr, libflint), Nothing,
          (Ref{ZZRingElem}, Ref{ZZRingElem}, Ref{ZZRingElem}, Ref{ZZRingElem}), z1, z2, x, y)
    z1, z2
end

# N.B. Base.divrem differs from Nemo.divrem
function Base.divrem(x::ZZRingElem, y::ZZRingElem)
    iszero(y) && throw(DivideError())
    z1 = ZZRingElem()
    z2 = ZZRingElem()
    ccall((:fmpz_tdiv_qr, libflint), Nothing,
          (Ref{ZZRingElem}, Ref{ZZRingElem}, Ref{ZZRingElem}, Ref{ZZRingElem}), z1, z2, x, y)
    z1, z2
end

function tdivrem(x::ZZRingElem, y::ZZRingElem)
    iszero(y) && throw(DivideError())
    z1 = ZZRingElem()
    z2 = ZZRingElem()
    ccall((:fmpz_tdiv_qr, libflint), Nothing,
          (Ref{ZZRingElem}, Ref{ZZRingElem}, Ref{ZZRingElem}, Ref{ZZRingElem}), z1, z2, x, y)
    z1, z2
end

function fdivrem(x::ZZRingElem, y::ZZRingElem)
    iszero(y) && throw(DivideError())
    z1 = ZZRingElem()
    z2 = ZZRingElem()
    ccall((:fmpz_fdiv_qr, libflint), Nothing,
          (Ref{ZZRingElem}, Ref{ZZRingElem}, Ref{ZZRingElem}, Ref{ZZRingElem}), z1, z2, x, y)
    z1, z2
end

function cdivrem(x::ZZRingElem, y::ZZRingElem)
    iszero(y) && throw(DivideError())
    z1 = ZZRingElem()
    z2 = ZZRingElem()
    ccall((:fmpz_cdiv_qr, libflint), Nothing,
          (Ref{ZZRingElem}, Ref{ZZRingElem}, Ref{ZZRingElem}, Ref{ZZRingElem}), z1, z2, x, y)
    z1, z2
end

function ntdivrem(x::ZZRingElem, y::ZZRingElem)
    iszero(y) && throw(DivideError())
    z1 = ZZRingElem()
    z2 = ZZRingElem()
    ccall((:fmpz_ndiv_qr, libflint), Nothing,
          (Ref{ZZRingElem}, Ref{ZZRingElem}, Ref{ZZRingElem}, Ref{ZZRingElem}), z1, z2, x, y)
    return z1, z2
end

function nfdivrem(a::ZZRingElem, b::ZZRingElem)
   q, r = tdivrem(a, b)
   c = ccall((:fmpz_cmp2abs, libflint), Cint, (Ref{ZZRingElem}, Ref{ZZRingElem}), b, r)
   if c <= 0
      if sign(Int, b) != sign(Int, r)
         sub!(q, q, UInt(1))
         add!(r, r, b)
      elseif c < 0
         add!(q, q, UInt(1))
         sub!(r, r, b)
      end
   end
   return (q, r)
end

function ncdivrem(a::ZZRingElem, b::ZZRingElem)
   q, r = tdivrem(a, b)
   c = ccall((:fmpz_cmp2abs, libflint), Cint, (Ref{ZZRingElem}, Ref{ZZRingElem}), b, r)
   if c <= 0
      if sign(Int, b) == sign(Int, r)
         add!(q, q, UInt(1))
         sub!(r, r, b)
      elseif c < 0
         sub!(q, q, UInt(1))
         add!(r, r, b)
      end
   end
   return (q, r)
end

function ndivrem(x::ZZRingElem, y::ZZRingElem)
    iszero(y) && throw(DivideError())
    z1 = ZZRingElem()
    z2 = ZZRingElem()
    ccall((:fmpz_ndiv_qr, libflint), Nothing,
          (Ref{ZZRingElem}, Ref{ZZRingElem}, Ref{ZZRingElem}, Ref{ZZRingElem}), z1, z2, x, y)
    z1, z2
end

###############################################################################
#
#   Inversion
#
###############################################################################

function inv(x::ZZRingElem)
   if isone(x)
      return ZZRingElem(1)
   elseif x == -1
      return ZZRingElem(-1)
   end
   iszero(x) && throw(DivideError())
   throw(ArgumentError("not a unit"))
end

###############################################################################
#
#   Powering
#
###############################################################################

function ^(x::ZZRingElem, y::Union{Int, UInt, ZZRingElem})
   if isone(x) || iszero(y)
      one(x)
   elseif x == -1
      isodd(y) ? deepcopy(x) : one(x)
   elseif y < 0
      throw(DomainError(y, "Exponent must be non-negative"))
   elseif isone(y)
      deepcopy(x)
   else
      z = ZZRingElem()
      ccall((:fmpz_pow_ui, libflint), Nothing,
            (Ref{ZZRingElem}, Ref{ZZRingElem}, UInt), z, x, UInt(y))
      z
   end
end

###############################################################################
#
#   Comparison
#
###############################################################################

function cmp(x::ZZRingElem, y::ZZRingElem)
    Int(ccall((:fmpz_cmp, libflint), Cint,
              (Ref{ZZRingElem}, Ref{ZZRingElem}), x, y))
end

==(x::ZZRingElem, y::ZZRingElem) = cmp(x,y) == 0

<=(x::ZZRingElem, y::ZZRingElem) = cmp(x,y) <= 0

<(x::ZZRingElem, y::ZZRingElem) = cmp(x,y) < 0

function cmpabs(x::ZZRingElem, y::ZZRingElem)
    Int(ccall((:fmpz_cmpabs, libflint), Cint,
              (Ref{ZZRingElem}, Ref{ZZRingElem}), x, y))
end

isless(x::ZZRingElem, y::ZZRingElem) = x < y

isless(x::ZZRingElem, y::Integer) = x < ZZRingElem(y)

isless(x::Integer, y::ZZRingElem) = ZZRingElem(x) < y

###############################################################################
#
#   Ad hoc comparison
#
###############################################################################

function cmp(x::ZZRingElem, y::Int)
    Int(ccall((:fmpz_cmp_si, libflint), Cint, (Ref{ZZRingElem}, Int), x, y))
end

==(x::ZZRingElem, y::Int) = cmp(x,y) == 0

<=(x::ZZRingElem, y::Int) = cmp(x,y) <= 0

<(x::ZZRingElem, y::Int) = cmp(x,y) < 0

==(x::Int, y::ZZRingElem) = cmp(y,x) == 0

<=(x::Int, y::ZZRingElem) = cmp(y,x) >= 0

<(x::Int, y::ZZRingElem) = cmp(y,x) > 0

function cmp(x::ZZRingElem, y::UInt)
    Int(ccall((:fmpz_cmp_ui, libflint), Cint, (Ref{ZZRingElem}, UInt), x, y))
end

==(x::ZZRingElem, y::UInt) = cmp(x,y) == 0

<=(x::ZZRingElem, y::UInt) = cmp(x,y) <= 0

<(x::ZZRingElem, y::UInt) = cmp(x,y) < 0

==(x::UInt, y::ZZRingElem) = cmp(y,x) == 0

<=(x::UInt, y::ZZRingElem) = cmp(y,x) >= 0

<(x::UInt, y::ZZRingElem) = cmp(y,x) > 0

###############################################################################
#
#   Shifting
#
###############################################################################

@doc raw"""
    <<(x::ZZRingElem, c::Int)

Return $2^cx$ where $c \geq 0$.
"""
function <<(x::ZZRingElem, c::Int)
    c < 0 && throw(DomainError(c, "Exponent must be non-negative"))
    c == 0 && return x
    z = ZZRingElem()
    ccall((:fmpz_mul_2exp, libflint), Nothing,
          (Ref{ZZRingElem}, Ref{ZZRingElem}, Int), z, x, c)
    return z
end

@doc raw"""
    >>(x::ZZRingElem, c::Int)

Return $x/2^c$, discarding any remainder, where $c \geq 0$.
"""
function >>(x::ZZRingElem, c::Int)
    c < 0 && throw(DomainError(c, "Exponent must be non-negative"))
    c == 0 && return x
    z = ZZRingElem()
    ccall((:fmpz_fdiv_q_2exp, libflint), Nothing,
          (Ref{ZZRingElem}, Ref{ZZRingElem}, Int), z, x, c)
    return z
end

###############################################################################
#
#   Modular arithmetic
#
###############################################################################

function mod!(r::ZZRingElem, x::ZZRingElem, y::ZZRingElem)
   ccall((:fmpz_fdiv_r, libflint), Nothing,
         (Ref{ZZRingElem}, Ref{ZZRingElem}, Ref{ZZRingElem}), r, x, y)
   return r
end

function mod(x::ZZRingElem, y::ZZRingElem)
   iszero(y) && throw(DivideError())
   r = ZZRingElem()
   return mod!(r, x, y)
end


function mod(x::ZZRingElem, c::UInt)
    c == 0 && throw(DivideError())
    ccall((:fmpz_fdiv_ui, libflint), Base.GMP.Limb, (Ref{ZZRingElem}, Base.GMP.Limb), x, c)
end

@doc raw"""
    powermod(x::ZZRingElem, p::ZZRingElem, m::ZZRingElem)

Return $x^p (\mod m)$. The remainder will be in the range $[0, m)$
"""
function powermod(x::ZZRingElem, p::ZZRingElem, m::ZZRingElem)
    m <= 0 && throw(DomainError(m, "Exponent must be non-negative"))
    if p < 0
       x = invmod(x, m)
       p = -p
    end
    r = ZZRingElem()
    ccall((:fmpz_powm, libflint), Nothing,
          (Ref{ZZRingElem}, Ref{ZZRingElem}, Ref{ZZRingElem}, Ref{ZZRingElem}),
          r, x, p, m)
    return r
end

@doc raw"""
    powermod(x::ZZRingElem, p::Int, m::ZZRingElem)

Return $x^p (\mod m)$. The remainder will be in the range $[0, m)$
"""
function powermod(x::ZZRingElem, p::Int, m::ZZRingElem)
    m <= 0 && throw(DomainError(m, "Exponent must be non-negative"))
    if p < 0
       x = invmod(x, m)
       p = -p
    end
    r = ZZRingElem()
    ccall((:fmpz_powm_ui, libflint), Nothing,
          (Ref{ZZRingElem}, Ref{ZZRingElem}, Int, Ref{ZZRingElem}),
          r, x, p, m)
    return r
end

#square-and-multiply algorithm to compute f^e mod g
function powermod(f::T, e::ZZRingElem, g::T) where {T}
   #small exponent -> use powermod
   if nbits(e) <= 63
      return powermod(f, Int(e), g)
   else
      #go through binary representation of exponent and multiply with res
      #or (res and f)
      res = parent(f)(1)
      for b = bits(e)
         res = mod(res^2, g)
         if b
            res = mod(res * f, g)
         end
      end
      return res
   end
end

@doc raw"""
    invmod(x::ZZRingElem, m::ZZRingElem)

Return $x^{-1} (\mod m)$. The remainder will be in the range $[0, m)$
"""
function invmod(x::ZZRingElem, m::ZZRingElem)
    m <= 0 && throw(DomainError(m, "Modulus must be non-negative"))
    z = ZZRingElem()
    if isone(m)
        return ZZRingElem(0)
    end
    if ccall((:fmpz_invmod, libflint), Cint,
             (Ref{ZZRingElem}, Ref{ZZRingElem}, Ref{ZZRingElem}), z, x, m) == 0
       error("Impossible inverse in invmod")
    end
    return z
end

@doc raw"""
    sqrtmod(x::ZZRingElem, m::ZZRingElem)

Return a square root of $x (\mod m)$ if one exists. The remainder will be in
the range $[0, m)$. We require that $m$ is prime, otherwise the algorithm may
not terminate.

# Examples

```jldoctest
julia> sqrtmod(ZZ(12), ZZ(13))
5
```
"""
function sqrtmod(x::ZZRingElem, m::ZZRingElem)
    m <= 0 && throw(DomainError(m, "Modulus must be non-negative"))
    z = ZZRingElem()
    if (ccall((:fmpz_sqrtmod, libflint), Cint,
              (Ref{ZZRingElem}, Ref{ZZRingElem}, Ref{ZZRingElem}), z, x, m) == 0)
        error("no square root exists or modulus is not prime")
    end
    return z
end

function _normalize_crt(r::ZZRingElem, m::ZZRingElem, signed)
   s = sign(Int, m)
   if s > 0
      return signed ? nfdivrem(r, m)[2] : fdivrem(r, m)[2]
   elseif s < 0
      return signed ? ncdivrem(r, m)[2] : cdivrem(r, m)[2]
   else
      return r
   end
end

function _normalize_crt_with_lcm(r::ZZRingElem, m::ZZRingElem, signed)
   s = sign(Int, m)
   if s == 0
      return (r, m)
   elseif s < 0
      m = -m
   end
   return (signed ? nfdivrem(r, m)[2] : fdivrem(r, m)[2], m)
end

@doc raw"""
    crt(r1::ZZRingElem, m1::ZZRingElem, r2::ZZRingElem, m2::ZZRingElem, signed=false; check::Bool=true)
    crt(r1::ZZRingElem, m1::ZZRingElem, r2::Union{Int, UInt}, m2::Union{Int, UInt}, signed=false; check::Bool=true)
    crt(r::Vector{ZZRingElem}, m::Vector{ZZRingElem}, signed=false; check::Bool=true)
    crt_with_lcm(r1::ZZRingElem, m1::ZZRingElem, r2::ZZRingElem, m2::ZZRingElem, signed=false; check::Bool=true)
    crt_with_lcm(r1::ZZRingElem, m1::ZZRingElem, r2::Union{Int, UInt}, m2::Union{Int, UInt}, signed=false; check::Bool=true)
    crt_with_lcm(r::Vector{ZZRingElem}, m::Vector{ZZRingElem}, signed=false; check::Bool=true)

As per the AbstractAlgebra `crt` interface, with the following option.
If `signed = true`, the solution is the range $(-m/2, m/2]$, otherwise it is in
the range $[0,m)$, where $m$ is the least common multiple of the moduli.

# Examples

```jldoctest
julia> crt(ZZ(5), ZZ(13), ZZ(7), ZZ(37), true)
44

julia> crt(ZZ(5), ZZ(13), 7, 37, true)
44
```
"""
function crt(r1::ZZRingElem, m1::ZZRingElem, r2::ZZRingElem, m2::ZZRingElem, signed=false; check::Bool=true)
   r, m = AbstractAlgebra._crt_with_lcm_stub(r1, m1, r2, m2; check=check)
   return _normalize_crt(r, m, signed)
end

function crt(r::Vector{ZZRingElem}, m::Vector{ZZRingElem}, signed=false; check::Bool=true)
   r, m = AbstractAlgebra._crt_with_lcm_stub(r, m; check=check)
   return _normalize_crt(r, m, signed)
end

function crt_with_lcm(r1::ZZRingElem, m1::ZZRingElem, r2::ZZRingElem, m2::ZZRingElem, signed=false; check::Bool=true)
   r, m = AbstractAlgebra._crt_with_lcm_stub(r1, m1, r2, m2; check=check)
   return _normalize_crt_with_lcm(r, m, signed)
end

function crt_with_lcm(r::Vector{ZZRingElem}, m::Vector{ZZRingElem}, signed=false; check::Bool=true)
   r, m = AbstractAlgebra._crt_with_lcm_stub(r, m; check=check)
   return _normalize_crt_with_lcm(r, m, signed)
end

# requires a < b
function _gcdinv(a::UInt, b::UInt)
   s = Ref{UInt}()
   g = ccall((:n_gcdinv, libflint), UInt,
             (Ptr{UInt}, UInt, UInt),
             s, a, b)
   return g, s[]
end

function submod(a::UInt, b::UInt, n::UInt)
   return a >= b ? a - b : a - b + n
end

function _crt_with_lcm(r1::ZZRingElem, m1::ZZRingElem, r2::UInt, m2::UInt; check::Bool=true)
   if iszero(m2)
      check && !is_divisible_by(r2 - r1, m1) && error("no crt solution")
      return (ZZRingElem(r2), ZZRingElem(m2))
   end
   if r2 >= m2
      r2 = mod(r2, m2)
   end
   if iszero(m1)
      check && mod(r1, m2) != r2 && error("no crt solution")
      return (r1, m1)
   end
   g, s = _gcdinv(mod(m1, m2), m2)
   diff = submod(r2, mod(r1, m2), m2)
   if isone(g)
      return (r1 + mulmod(diff, s, m2)*m1, m1*m2)
   else
      m2og = divexact(m2, g; check=false)
      diff = divexact(diff, g; check=check)
      return (r1 + mulmod(diff, s, m2og)*m1, m1*m2og)
   end
end

function _crt_with_lcm(r1::ZZRingElem, m1::ZZRingElem, r2::Union{Int, UInt},
                                        m2::Union{Int, UInt}; check::Bool=true)
   if iszero(m2)
      check && !is_divisible_by(r2 - r1, m1) && error("no crt solution")
      return (ZZRingElem(r2), ZZRingElem(m2))
   end
   m2 = abs(m2)%UInt
   r2 = r2 < 0 ? mod(r2, m2) : UInt(r2)
   return _crt_with_lcm(r1, m1, r2::UInt, m2; check=check)
end

function crt(r1::ZZRingElem, m1::ZZRingElem, r2::Union{Int, UInt},
                        m2::Union{Int, UInt}, signed = false; check::Bool=true)
   r, m = _crt_with_lcm(r1, m1, r2, m2; check=check)
   return _normalize_crt(r, m, signed)
end

function crt_with_lcm(r1::ZZRingElem, m1::ZZRingElem, r2::Union{Int, UInt},
                        m2::Union{Int, UInt}, signed = false; check::Bool=true)
   r, m = _crt_with_lcm(r1, m1, r2, m2; check=check)
   return _normalize_crt_with_lcm(r, m, signed)
end

###############################################################################
#
#   Integer logarithm
#
###############################################################################

@doc raw"""
    flog(x::ZZRingElem, c::ZZRingElem)
    flog(x::ZZRingElem, c::Int)

Return the floor of the logarithm of $x$ to base $c$.

# Examples

```jldoctest
julia> flog(ZZ(12), ZZ(2))
3

julia> flog(ZZ(12), 3)
2

```
"""
function flog(x::ZZRingElem, c::ZZRingElem)
    c <= 0 && throw(DomainError(c, "Base must be non-negative"))
    x <= 0 && throw(DomainError(x, "Argument must be non-negative"))
    return ccall((:fmpz_flog, libflint), Int,
                 (Ref{ZZRingElem}, Ref{ZZRingElem}), x, c)
end

function flog(x::ZZRingElem, c::Int)
    c <= 0 && throw(DomainError(c, "Base must be non-negative"))
    return ccall((:fmpz_flog_ui, libflint), Int,
                 (Ref{ZZRingElem}, Int), x, c)
end

@doc raw"""
    clog(x::ZZRingElem, c::ZZRingElem)
    clog(x::ZZRingElem, c::Int)

Return the ceiling of the logarithm of $x$ to base $c$.

# Examples

```jldoctest
julia> clog(ZZ(12), ZZ(2))
4

julia> clog(ZZ(12), 3)
3

```
"""
function clog(x::ZZRingElem, c::ZZRingElem)
    c <= 0 && throw(DomainError(c, "Base must be non-negative"))
    x <= 0 && throw(DomainError(x, "Argument must be non-negative"))
    return ccall((:fmpz_clog, libflint), Int,
                 (Ref{ZZRingElem}, Ref{ZZRingElem}), x, c)
end

function clog(x::ZZRingElem, c::Int)
    c <= 0 && throw(DomainError(c, "Base must be non-negative"))
    return ccall((:fmpz_clog_ui, libflint), Int,
                 (Ref{ZZRingElem}, Int), x, c)
end

###############################################################################
#
#   GCD and LCM
#
###############################################################################

@doc raw"""
    gcd(x::ZZRingElem, y::ZZRingElem, z::ZZRingElem...)

Return the greatest common divisor of $(x, y, ...)$. The returned result will
always be non-negative and will be zero iff all inputs are zero.
"""
function gcd(x::ZZRingElem, y::ZZRingElem, z::ZZRingElem...)
   d = ZZRingElem()
   ccall((:fmpz_gcd, libflint), Nothing,
         (Ref{ZZRingElem}, Ref{ZZRingElem}, Ref{ZZRingElem}), d, x, y)
   length(z) == 0 && return d

   for ix in 1:length(z)
     ccall((:fmpz_gcd, libflint), Nothing,
           (Ref{ZZRingElem}, Ref{ZZRingElem}, Ref{ZZRingElem}), d, d, z[ix])
   end
   return d
end

@doc raw"""
    gcd(x::Vector{ZZRingElem})

Return the greatest common divisor of the elements of $x$. The returned
result will always be non-negative and will be zero iff all elements of $x$
are zero.
"""
function gcd(x::Vector{ZZRingElem})
   if length(x) == 0
      error("Array must not be empty")
   elseif length(x) == 1
      return x[1]
   end

   z = ZZRingElem()
   ccall((:fmpz_gcd, libflint), Nothing,
         (Ref{ZZRingElem}, Ref{ZZRingElem}, Ref{ZZRingElem}), z, x[1], x[2])

   for i in 3:length(x)
      ccall((:fmpz_gcd, libflint), Nothing,
            (Ref{ZZRingElem}, Ref{ZZRingElem}, Ref{ZZRingElem}), z, z, x[i])
      if isone(z)
         return z
      end
   end

   return z
end

@doc raw"""
    lcm(x::ZZRingElem, y::ZZRingElem, z::ZZRingElem...)

Return the least common multiple of $(x, y, ...)$. The returned result will
always be non-negative and will be zero if any input is zero.
"""
function lcm(x::ZZRingElem, y::ZZRingElem, z::ZZRingElem...)
   m = ZZRingElem()
   ccall((:fmpz_lcm, libflint), Nothing,
         (Ref{ZZRingElem}, Ref{ZZRingElem}, Ref{ZZRingElem}), m, x, y)
   length(z) == 0 && return m

   for ix in 1:length(z)
     ccall((:fmpz_lcm, libflint), Nothing,
           (Ref{ZZRingElem}, Ref{ZZRingElem}, Ref{ZZRingElem}), m, m, z[ix])
   end
   return m
end

@doc raw"""
    lcm(x::Vector{ZZRingElem})

Return the least common multiple of the elements of $x$. The returned result
will always be non-negative and will be zero iff the elements of $x$ are zero.
"""
function lcm(x::Vector{ZZRingElem})
   if length(x) == 0
      error("Array must not be empty")
   elseif length(x) == 1
      return x[1]
   end

   z = ZZRingElem()
   ccall((:fmpz_lcm, libflint), Nothing,
         (Ref{ZZRingElem}, Ref{ZZRingElem}, Ref{ZZRingElem}), z, x[1], x[2])

   for i in 3:length(x)
      ccall((:fmpz_lcm, libflint), Nothing,
            (Ref{ZZRingElem}, Ref{ZZRingElem}, Ref{ZZRingElem}), z, z, x[i])
   end

   return z
end

gcd(a::ZZRingElem, b::Integer) = gcd(a, ZZRingElem(b))

gcd(a::Integer, b::ZZRingElem) = gcd(ZZRingElem(a), b)

lcm(a::ZZRingElem, b::Integer) = lcm(a, ZZRingElem(b))

lcm(a::Integer, b::ZZRingElem) = lcm(ZZRingElem(a), b)

###############################################################################
#
#   Extended GCD
#
###############################################################################

@doc raw"""
    gcdx(a::ZZRingElem, b::ZZRingElem)

Return a tuple $g, s, t$ such that $g$ is the greatest common divisor of $a$
and $b$ and integers $s$ and $t$ such that $g = as + bt$.
"""
function gcdx(a::ZZRingElem, b::ZZRingElem)
  # Just to conform with Julia's definition
  a == b == 0 && return zero(FlintZZ), one(FlintZZ), zero(FlintZZ)

  d = FlintZZ()
  x = FlintZZ()
  y = FlintZZ()
  ccall((:fmpz_xgcd_canonical_bezout, libflint), Nothing,
        (Ref{ZZRingElem}, Ref{ZZRingElem}, Ref{ZZRingElem}, Ref{ZZRingElem}, Ref{ZZRingElem}), d, x, y, a, b)
  return d, x, y
end

@doc raw"""
    gcdinv(a::ZZRingElem, b::ZZRingElem)

Return a tuple $g, s$ where $g$ is the greatest common divisor of $a$ and
$b$ and where $s$ is the inverse of $a$ modulo $b$ if $g = 1$. This function
can be used to detect impossible inverses, i.e. where $a$ and $b$ are not
coprime, and to yield the common factor of $a$ and $b$ if they are not
coprime. We require $b \geq a \geq 0$.
"""
function gcdinv(a::ZZRingElem, b::ZZRingElem)
   a < 0 && throw(DomainError(a, "First argument must be non-negative"))
   b < a && throw(DomainError((a, b), "First argument must be smaller than second argument"))
   g = ZZRingElem()
   s = ZZRingElem()
   ccall((:fmpz_gcdinv, libflint), Nothing,
        (Ref{ZZRingElem}, Ref{ZZRingElem}, Ref{ZZRingElem}, Ref{ZZRingElem}),
        g, s, a, b)
   return g, s
end

gcdx(a::ZZRingElem, b::Integer) = gcdx(a, ZZRingElem(b))

gcdx(a::Integer, b::ZZRingElem) = gcdx(ZZRingElem(a), b)

gcdinv(a::ZZRingElem, b::Integer) = gcdinv(a, ZZRingElem(b))

gcdinv(a::Integer, b::ZZRingElem) = gcdinv(ZZRingElem(a), b)

###############################################################################
#
#   Roots
#
###############################################################################

sqrt_moduli = [3, 5, 7, 8]
sqrt_residues = [[0, 1], [0, 1, 4], [0, 1, 2, 4], [0, 1, 4]]

@doc raw"""
    isqrt(x::ZZRingElem)

Return the floor of the square root of $x$.

# Examples

```jldoctest
julia> isqrt(ZZ(13))
3

```
"""
function isqrt(x::ZZRingElem)
    x < 0 && throw(DomainError(x, "Argument must be non-negative"))
    z = ZZRingElem()
    ccall((:fmpz_sqrt, libflint), Nothing, (Ref{ZZRingElem}, Ref{ZZRingElem}), z, x)
    return z
end

@doc raw"""
    isqrtrem(x::ZZRingElem)

Return a tuple $s, r$ consisting of the floor $s$ of the square root of $x$
and the remainder $r$, i.e. such that $x = s^2 + r$. We require $x \geq 0$.

# Examples

```jldoctest
julia> isqrtrem(ZZ(13))
(3, 4)

```
"""
function isqrtrem(x::ZZRingElem)
    x < 0 && throw(DomainError(x, "Argument must be non-negative"))
    s = ZZRingElem()
    r = ZZRingElem()
    ccall((:fmpz_sqrtrem, libflint), Nothing,
          (Ref{ZZRingElem}, Ref{ZZRingElem}, Ref{ZZRingElem}), s, r, x)
    return s, r
end

function Base.sqrt(x::ZZRingElem; check=true)
    x < 0 && throw(DomainError(x, "Argument must be non-negative"))
    if check
       for i = 1:length(sqrt_moduli)
          res = mod(x, sqrt_moduli[i])
          !(res in sqrt_residues[i]) && error("Not a square")
       end
       s = ZZRingElem()
       r = ZZRingElem()
       ccall((:fmpz_sqrtrem, libflint), Nothing,
          (Ref{ZZRingElem}, Ref{ZZRingElem}, Ref{ZZRingElem}), s, r, x)
       !iszero(r) && error("Not a square")
    else
       s = ZZRingElem()
       ccall((:fmpz_sqrt, libflint), Nothing, (Ref{ZZRingElem}, Ref{ZZRingElem}), s, x)
    end
    return s
end

is_square(x::ZZRingElem) = Bool(ccall((:fmpz_is_square, libflint), Cint,
                               (Ref{ZZRingElem},), x))

function is_square_with_sqrt(x::ZZRingElem)
    if x < 0
       return false, zero(ZZRingElem)
    end
    for i = 1:length(sqrt_moduli)
       res = mod(x, sqrt_moduli[i])
       if !(res in sqrt_residues[i])
          return false, zero(ZZRingElem)
       end
    end
    s = ZZRingElem()
    r = ZZRingElem()
    ccall((:fmpz_sqrtrem, libflint), Nothing,
          (Ref{ZZRingElem}, Ref{ZZRingElem}, Ref{ZZRingElem}), s, r, x)
    if !iszero(r)
       return false, zero(ZZRingElem)
    end
    return true, s
end

@doc raw"""
    root(x::ZZRingElem, n::Int; check::Bool=true)

Return the $n$-the root of $x$. We require $n > 0$ and that
$x \geq 0$ if $n$ is even. By default the function tests whether the input was
a perfect $n$-th power and if not raises an exception. If `check=false` this
check is omitted.

# Examples

```jldoctest
julia> root(ZZ(27), 3; check=true)
3
```
"""
function root(x::ZZRingElem, n::Int; check::Bool=true)
   x < 0 && iseven(n) && throw(DomainError((x, n), "Argument `x` must be positive if exponent `n` is even"))
   n <= 0 && throw(DomainError(n, "Exponent must be positive"))
   z = ZZRingElem()
   res = ccall((:fmpz_root, libflint), Bool,
         (Ref{ZZRingElem}, Ref{ZZRingElem}, Int), z, x, n)
   check && !res && error("Not a perfect n-th power (n = $n)")
   return z
end

@doc raw"""
    iroot(x::ZZRingElem, n::Int)

Return the integer truncation of the $n$-the root of $x$ (round towards zero).
We require $n > 0$ and that $x \geq 0$ if $n$ is even.

# Examples

```jldoctest
julia> iroot(ZZ(13), 3)
2
```
"""
function iroot(x::ZZRingElem, n::Int)
   x < 0 && iseven(n) && throw(DomainError((x, n), "Argument `x` must be positive if exponent `n` is even"))
   n <= 0 && throw(DomainError(n, "Exponent must be positive"))
   z = ZZRingElem()
   ccall((:fmpz_root, libflint), Bool,
         (Ref{ZZRingElem}, Ref{ZZRingElem}, Int), z, x, n)
   return z
end

###############################################################################
#
#   Factorization
#
###############################################################################

function _factor(a::ZZRingElem)
   F = fmpz_factor()
   ccall((:fmpz_factor, libflint), Nothing, (Ref{fmpz_factor}, Ref{ZZRingElem}), F, a)
   res = Dict{ZZRingElem, Int}()
   for i in 1:F.num
     z = ZZRingElem()
     ccall((:fmpz_factor_get_fmpz, libflint), Nothing,
           (Ref{ZZRingElem}, Ref{fmpz_factor}, Int), z, F, i - 1)
     res[z] = unsafe_load(F.exp, i)
   end
   return res, canonical_unit(a)
end

function factor(a::T) where T <: Union{Int, UInt}
   if iszero(a)
      throw(ArgumentError("Argument is not non-zero"))
   end
   u = sign(a)
   a = u < 0 ? -a : a
   F = n_factor()
   ccall((:n_factor, libflint), Nothing, (Ref{n_factor}, UInt), F, a)
   res = Dict{T, Int}()
   for i in 1:F.num
     z = F.p[i]
     res[z] = F.exp[i]
   end
   return Fac(u, res)
end

################################################################################
#
#   ECM
#
################################################################################

function _ecm(a::ZZRingElem, B1::UInt, B2::UInt, ncrv::UInt,
             rnd = _flint_rand_states[Threads.threadid()])
  f = ZZRingElem()
  r = ccall((:fmpz_factor_ecm, libflint), Int32,
            (Ref{ZZRingElem}, UInt, UInt, UInt, Ptr{Cvoid}, Ref{ZZRingElem}),
            f, ncrv, B1, B2, rnd.ptr, a)
  return r, f
end

function _ecm(a::ZZRingElem, B1::Int, B2::Int, ncrv::Int,
             rnd = _flint_rand_states[Threads.threadid()])
  return _ecm(a, UInt(B1), UInt(B2), UInt(ncrv), rnd)
end

function ecm(a::ZZRingElem, max_digits::Int = div(ndigits(a), 2) + 1,
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

function _factor_trial_range(N::ZZRingElem, start::Int = 0, np::Int = 10^5)
   F = fmpz_factor()
   ccall((:fmpz_factor_trial_range, libflint), Nothing,
         (Ref{Nemo.fmpz_factor}, Ref{ZZRingElem}, UInt, UInt), F, N, start, np)
   res = Dict{ZZRingElem, Int}()
   for i in 1:F.num
     z = ZZRingElem()
     ccall((:fmpz_factor_get_fmpz, libflint), Nothing,
           (Ref{ZZRingElem}, Ref{Nemo.fmpz_factor}, Int), z, F, i - 1)
     res[z] = unsafe_load(F.exp, i)
   end
   return res, canonical_unit(N)
end

@doc raw"""
    factor(a::ZZRingElem)
    factor(a::UInt)
    factor(a::Int)

Return a factorisation of $a$ using a `Fac` struct (see the documentation on
factorisation in Nemo).

# Examples

```jldoctest
julia> factor(ZZ(12))
1 * 2^2 * 3

julia> factor(UInt(12))
1 * 2^2 * 3

julia> factor(12)
1 * 2^2 * 3

```
"""
function factor(a::ZZRingElem)
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

@doc raw"""
    divisible(x::ZZRingElem, y::ZZRingElem)

Return `true` if $x$ is divisible by $y$, otherwise return `false`. We
require $x \neq 0$.
"""
function divisible(x::ZZRingElem, y::ZZRingElem)
   iszero(y) && throw(DivideError())
   Bool(ccall((:fmpz_divisible, libflint), Cint,
              (Ref{ZZRingElem}, Ref{ZZRingElem}), x, y))
end

@doc raw"""
    divisible(x::ZZRingElem, y::Int)

Return `true` if $x$ is divisible by $y$, otherwise return `false`. We
require $x \neq 0$.
"""
function divisible(x::ZZRingElem, y::Int)
   y == 0 && throw(DivideError())
   Bool(ccall((:fmpz_divisible_si, libflint), Cint,
              (Ref{ZZRingElem}, Int), x, y))
end

@doc raw"""
    divisors(a::Union{Int, ZZRingElem})

Return the positive divisors of $a$ in an array, not necessarily in growing
order. We require $a \neq 0$.
"""
function divisors end

function divisors(a::ZZRingElem)
   iszero(a) && throw(DomainError("Argument must be non-zero"))

   divs = ZZRingElem[one(FlintZZ)]
   isone(a) && return divs

   for (p,e) in factor(a)
      ndivs = copy(divs)
      for i = 1:e
         map!(d -> p*d, ndivs, ndivs)
         append!(divs, ndivs)
      end
   end

   return divs
end

divisors(a::Int) = Int.(divisors(FlintZZ(a)))

@doc raw"""
    prime_divisors(a::ZZRingElem)

Return the prime divisors of $a$ in an array. We require $a \neq 0$.
"""
function prime_divisors(a::ZZRingElem)
   iszero(a) && throw(DomainError("Argument must be non-zero"))
   ZZRingElem[p for (p, e) in factor(a)]
end

@doc raw"""
    prime_divisors(a::Int)

Return the prime divisors of $a$ in an array. We require $a \neq 0$.
"""
prime_divisors(a::Int) = Int.(prime_divisors(FlintZZ(a)))

is_prime(x::UInt) = Bool(ccall((:n_is_prime, libflint), Cint, (UInt,), x))

@doc raw"""
    is_prime(x::ZZRingElem)
    is_prime(x::Int)

Return `true` if $x$ is a prime number, otherwise return `false`.

# Examples

```jldoctest
julia> is_prime(ZZ(13))
true
```
"""
function is_prime(x::ZZRingElem)
  !is_probable_prime(x) && return false
  return Bool(ccall((:fmpz_is_prime, libflint), Cint, (Ref{ZZRingElem},), x))
end

function is_prime(n::Int)
  if n < 0
    return false
  end
  return is_prime(n % UInt)
end

@doc raw"""
    is_probable_prime(x::ZZRingElem)

Return `true` if $x$ is very probably a prime number, otherwise return
`false`. No counterexamples are known to this test, but it is conjectured
that infinitely many exist.
"""
is_probable_prime(x::ZZRingElem) = Bool(ccall((:fmpz_is_probabprime, libflint), Cint,
                                      (Ref{ZZRingElem},), x))

@doc raw"""
    next_prime(x::ZZRingElem, proved = true)

Return the smallest prime strictly greater than $x$.
If a second argument of `false` is specified, the return is only probably prime.
"""
function next_prime(x::ZZRingElem, proved::Bool = true)
   z = ZZRingElem()
   ccall((:fmpz_nextprime, libflint), Nothing,
         (Ref{ZZRingElem}, Ref{ZZRingElem}, Cint),
         z, x, proved)
   return z
end

function next_prime(x::UInt, proved::Bool = true)
   if (Base.GMP.BITS_PER_LIMB == 64 && x >= 0xffffffffffffffc5) ||
      (Base.GMP.BITS_PER_LIMB == 32 && x >= 0xfffffffb)
         error("No larger single-limb prime exists")
   end
   return ccall((:n_nextprime, libflint), UInt,
                (UInt, Cint),
                x, proved)
end

function next_prime(x::Int, proved::Bool = true)
   return x < 2 ? 2 : Int(next_prime(x % UInt, proved))
end

function remove!(a::ZZRingElem, b::ZZRingElem)
   v = ccall((:fmpz_remove, libflint), Clong, (Ref{ZZRingElem}, Ref{ZZRingElem}, Ref{ZZRingElem}), a, a, b)
   return v, a
end

function remove(x::ZZRingElem, y::ZZRingElem)
   iszero(y) && throw(DivideError())
   y <= 1 && error("Factor <= 1")
   z = ZZRingElem()
   num = ccall((:fmpz_remove, libflint), Int,
               (Ref{ZZRingElem}, Ref{ZZRingElem}, Ref{ZZRingElem}), z, x, y)
   return num, z
end

remove(x::ZZRingElem, y::Integer) = remove(x, ZZRingElem(y))

remove(x::Integer, y::ZZRingElem) = remove(ZZRingElem(x), y)

function remove(a::UInt, b::UInt)
   b <= 1 && error("Factor <= 1")
   a == 0 && error("Not yet implemented")
   q = Ref(a)
   binv = ccall((:n_precompute_inverse, libflint), Float64, (UInt,), b)
   v = ccall((:n_remove2_precomp, libflint), Cint,
             (Ptr{UInt}, UInt, Float64),
             q, b, binv)
   return (Int(v), q[])
end

function remove(a::Int, b::Int)
   b <= 1 && error("Factor <= 1")
   v, q = remove(abs(a)%UInt, b%UInt)
   return (v, a < 0 ? -q%Int : q%Int)
end

function remove(a::BigInt, b::BigInt)
   b <= 1 && error("Factor <= 1")
   a == 0 && error("Not yet implemented")
   q = BigInt()
   v =  ccall((:__gmpz_remove, :libgmp), Culong,
              (Ref{BigInt}, Ref{BigInt}, Ref{BigInt}),
              q, a, b)
   return (Int(v), q)
end

function remove(x::Integer, y::Integer)
   v, q = remove(ZZRingElem(x), ZZRingElem(y))
   return (v, convert(promote_type(typeof(x), typeof(y)), q))
end

@doc raw"""
    valuation(x::ZZRingElem, y::ZZRingElem)

Return the largest $n$ such that $y^n$ divides $x$.
"""
function valuation(x::ZZRingElem, y::ZZRingElem)
   iszero(x) && error("Not yet implemented")
   n, _ = remove(x, y)
   return n
end

valuation(x::ZZRingElem, y::Integer) = valuation(x, ZZRingElem(y))

valuation(x::Integer, y::ZZRingElem) = valuation(ZZRingElem(x), y)

valuation(x::Integer, y::Integer) = valuation(ZZRingElem(x), ZZRingElem(y))

@doc raw"""
    divisor_lenstra(n::ZZRingElem, r::ZZRingElem, m::ZZRingElem)

If $n$ has a factor which lies in the residue class $r (\mod m)$ for
$0 < r < m < n$, this function returns such a factor. Otherwise it returns
$0$. This is only efficient if $m$ is at least the cube root of $n$. We
require gcd$(r, m) = 1$ and this condition is not checked.
"""
function divisor_lenstra(n::ZZRingElem, r::ZZRingElem, m::ZZRingElem)
   r <= 0 && throw(DomainError(r, "Residue class must be non-negative"))
   m <= r && throw(DomainError((m, r), "Modulus must be bigger than residue class"))
   n <= m && throw(DomainError((n, m), "Argument must be bigger than modulus"))
   z = ZZRingElem()
   if !Bool(ccall((:fmpz_divisor_in_residue_class_lenstra, libflint),
       Cint, (Ref{ZZRingElem}, Ref{ZZRingElem}, Ref{ZZRingElem}, Ref{ZZRingElem}), z, n, r, m))
      z = 0
   end
   return z
end

@doc raw"""
    factorial(x::ZZRingElem)

Return the factorial of $x$, i.e. $x! = 1.2.3\ldots x$. We require
$x \geq 0$.

# Examples

```jldoctest
julia> factorial(ZZ(100))
93326215443944152681699238856266700490715968264381621468592963895217599993229915608941463976156518286253697920827223758251185210916864000000000000000000000000
```
"""
function factorial(x::ZZRingElem)
    x < 0 && throw(DomainError(x, "Argument must be non-negative"))
    z = ZZRingElem()
    ccall((:fmpz_fac_ui, libflint), Nothing, (Ref{ZZRingElem}, UInt), z, UInt(x))
    return z
end

@doc raw"""
    rising_factorial(x::ZZRingElem, n::Int)

Return the rising factorial of $x$, i.e. $x(x + 1)(x + 2)\ldots (x + n - 1)$.
If $n < 0$ we throw a `DomainError()`.
"""
function rising_factorial(x::ZZRingElem, n::Int)
    n < 0 && throw(DomainError(n, "Argument must be non-negative"))
    z = ZZRingElem()
    ccall((:fmpz_rfac_ui, libflint), Nothing,
          (Ref{ZZRingElem}, Ref{ZZRingElem}, UInt), z, x, UInt(n))
    return z
end

@doc raw"""
    rising_factorial(x::ZZRingElem, n::ZZRingElem)

Return the rising factorial of $x$, i.e. $x(x + 1)(x + 2)\cdots (x + n - 1)$.
If $n < 0$ we throw a `DomainError()`.
"""
rising_factorial(x::ZZRingElem, n::ZZRingElem) = rising_factorial(x, Int(n))

@doc raw"""
    rising_factorial(x::Int, n::Int)

Return the rising factorial of $x$, i.e. $x(x + 1)(x + 2)\ldots (x + n - 1)$.
If $n < 0$ we throw a `DomainError()`.
"""
function rising_factorial(x::Int, n::Int)
    n < 0 && throw(DomainError(n, "Argument must be non-negative"))
    z = ZZRingElem()
    if x < 0
       if n <= -x # we don't pass zero
          z = isodd(n) ? -rising_factorial(-x - n + 1, n) :
                          rising_factorial(-x - n + 1, n)
       end
    else
       ccall((:fmpz_rfac_uiui, libflint), Nothing,
             (Ref{ZZRingElem}, UInt, UInt), z, x, n)
    end
    return Int(z)
end

@doc raw"""
    primorial(x::Int)

Return the primorial of $x$, i.e. the product of all primes less than or
equal to $x$. If $x < 0$ we throw a `DomainError()`.
"""
function primorial(x::Int)
    x < 0 && throw(DomainError(x, "Argument must be non-negative"))
    z = ZZRingElem()
    ccall((:fmpz_primorial, libflint), Nothing,
          (Ref{ZZRingElem}, UInt), z, UInt(x))
    return Int(z)
end

@doc raw"""
    primorial(x::ZZRingElem)

Return the primorial of $x$, i.e. the product of all primes less than or
equal to $x$. If $x < 0$ we throw a `DomainError()`.
"""
function primorial(x::ZZRingElem)
    x < 0 && throw(DomainError(x, "Argument must be non-negative"))
    z = ZZRingElem()
    ccall((:fmpz_primorial, libflint), Nothing,
          (Ref{ZZRingElem}, UInt), z, UInt(x))
    return z
end

@doc raw"""
    fibonacci(x::Int)

Return the $x$-th Fibonacci number $F_x$. We define $F_1 = 1$, $F_2 = 1$ and
$F_{i + 1} = F_i + F_{i - 1}$ for all integers $i$.
"""
function fibonacci(x::Int)
    z = ZZRingElem()
    ccall((:fmpz_fib_ui, libflint), Nothing,
          (Ref{ZZRingElem}, UInt), z, UInt(abs(x)))
    return x < 0 ? (iseven(x) ? -Int(z) : Int(z)) : Int(z)
end

@doc raw"""
    fibonacci(x::ZZRingElem)

Return the $x$-th Fibonacci number $F_x$. We define $F_1 = 1$, $F_2 = 1$ and
$F_{i + 1} = F_i + F_{i - 1}$ for all integers $i$.
"""
function fibonacci(x::ZZRingElem)
    z = ZZRingElem()
    ccall((:fmpz_fib_ui, libflint), Nothing,
          (Ref{ZZRingElem}, UInt), z, UInt(abs(x)))
    return x < 0 ? (iseven(x) ? -z : z) : z
end

@doc raw"""
    bell(x::Int)

Return the Bell number $B_x$.
"""
function bell(x::Int)
    x < 0 && throw(DomainError(x, "Argument must be non-negative"))
    z = ZZRingElem()
    ccall((:arith_bell_number, libflint), Nothing,
          (Ref{ZZRingElem}, UInt), z, UInt(x))
    return Int(z)
end

@doc raw"""
    bell(x::ZZRingElem)

Return the Bell number $B_x$.
"""
function bell(x::ZZRingElem)
    x < 0 && throw(DomainError(x, "Argument must be non-negative"))
    z = ZZRingElem()
    ccall((:arith_bell_number, libflint), Nothing,
          (Ref{ZZRingElem}, UInt), z, UInt(x))
    return z
end

# fmpz_bin_uiui doesn't always work on UInt input as it just wraps mpz_bin_uiui,
# which has silly gnu problems on windows
# TODO: fib_ui, pow_ui, fac_ui ditto
function _binomial!(z::ZZRingElem, n::Culong, k::Culong)
    ccall((:fmpz_bin_uiui, libflint), Nothing,
          (Ref{ZZRingElem}, UInt, UInt), z, UInt(n), UInt(k))
    return z
end

function _binomial(n::ZZRingElem, k::ZZRingElem)
    @assert k >= 0
    z = ZZRingElem(1)
    if fits(Culong, n) && fits(Culong, k)
        _binomial!(z, Culong(n), Culong(k))
    elseif fits(UInt, k)
        for K in UInt(1):UInt(k)
            mul!(z, z, n - (K - 1))
            divexact!(z, z, K)
        end
    else
        # if called with n >= k
        error("result of binomial($n, $k) probably is too large")
    end
    return z
end


@doc raw"""
    binomial(n::ZZRingElem, k::ZZRingElem)

Return the binomial coefficient $\frac{n (n-1) \cdots (n-k+1)}{k!}$.
If $k < 0$ we return $0$, and the identity
`binomial(n, k) == binomial(n - 1, k - 1) + binomial(n - 1, k)` always holds
for integers `n` and `k`.
"""
function binomial(n::ZZRingElem, k::ZZRingElem)
    ksgn = cmp(k, 0)
    ksgn > 0 || return ZZRingElem(ksgn == 0)
    # k > 0 now
    negz = false
    if n < 0
        n = k - n - 1
        negz = isodd(k)
    end
    if n < k
        z = ZZRingElem(0)
    elseif 2*k <= n
        z = _binomial(n, k)
    else
        z = _binomial(n, n - k)
    end
    return negz ? neg!(z, z) : z
end

@doc raw"""
    binomial(n::UInt, k::UInt, ::ZZRing)

Return the binomial coefficient $\frac{n!}{(n - k)!k!}$ as an `ZZRingElem`.
"""
function binomial(n::UInt, k::UInt, ::ZZRing)
    z = ZZRingElem()
    ccall((:fmpz_bin_uiui, libflint), Nothing,
          (Ref{ZZRingElem}, UInt, UInt), z, n, k)
    return z
end

@doc raw"""
    moebius_mu(x::ZZRingElem)

Return the Moebius mu function of $x$ as an `Int`. The value
returned is either $-1$, $0$ or $1$. If $x \leq 0$ we throw a `DomainError()`.
"""
function moebius_mu(x::ZZRingElem)
   x <= 0 && throw(DomainError(x, "Argument must be positive"))
   return Int(ccall((:fmpz_moebius_mu, libflint), Cint,
                    (Ref{ZZRingElem},), x))
end

@doc raw"""
    moebius_mu(x::Int)

Return the Moebius mu function of $x$ as an `Int`. The value
returned is either $-1$, $0$ or $1$. If $x \leq 0$ we throw a `DomainError()`.
"""
moebius_mu(x::Int) = moebius_mu(ZZRingElem(x))

@doc raw"""
    jacobi_symbol(x::ZZRingElem, y::ZZRingElem)

Return the value of the Jacobi symbol $\left(\frac{x}{y}\right)$. The modulus
$y$ must be odd and positive, otherwise a `DomainError` is thrown.
"""
function jacobi_symbol(x::ZZRingElem, y::ZZRingElem)
   (y <= 0 || iseven(y)) && throw(DomainError(y, "Modulus must be odd and positive"))
   if x < 0 || x >= y
      x = mod(x, y)
   end
   return Int(ccall((:fmpz_jacobi, libflint), Cint,
                    (Ref{ZZRingElem}, Ref{ZZRingElem}), x, y))
end

@doc raw"""
    jacobi_symbol(x::Int, y::Int)

Return the value of the Jacobi symbol $\left(\frac{x}{y}\right)$. The modulus
$y$ must be odd and positive, otherwise a `DomainError` is thrown.
"""
function jacobi_symbol(x::Int, y::Int)
   (y <= 0 || mod(y, 2) == 0) && throw(DomainError(y, "Modulus must be odd and positive"))
   if x < 0 || x >= y
      x = mod(x, y)
   end
   return Int(ccall((:n_jacobi, libflint), Cint, (Int, UInt), x, UInt(y)))
end

@doc raw"""
    kronecker_symbol(x::ZZRingElem, y::ZZRingElem)
    kronecker_symbol(x::Int, y::Int)

Return the value of the Kronecker symbol $\left(\frac{x}{y}\right)$.
The definition is as per Henri Cohen's book, "A Course in Computational
Algebraic Number Theory", Definition 1.4.8.
"""
function kronecker_symbol(x::Int, y::Int)
   return Int(ccall((:z_kronecker, libflint), Cint,
                    (Int, Int), x, y))
end

function kronecker_symbol(x::ZZRingElem, y::ZZRingElem)
   return Int(ccall((:fmpz_kronecker, libflint), Cint,
                    (Ref{ZZRingElem}, Ref{ZZRingElem}), x, y))
end

@doc raw"""
    divisor_sigma(x::ZZRingElem, y::Int)
    divisor_sigma(x::ZZRingElem, y::ZZRingElem)
    divisor_sigma(x::Int, y::Int)

Return the value of the sigma function, i.e. $\sum_{0 < d \;| x} d^y$. If
$x \leq 0$ or $y < 0$ we throw a `DomainError()`.

# Examples

```jldoctest
julia> divisor_sigma(ZZ(32), 10)
1127000493261825

julia> divisor_sigma(ZZ(32), ZZ(10))
1127000493261825

julia> divisor_sigma(32, 10)
1127000493261825
```
"""
function divisor_sigma(x::ZZRingElem, y::Int)
   x <= 0 && throw(DomainError(x, "Argument must be positive"))
   y < 0 && throw(DomainError(y, "Power must be non-negative"))
   z = ZZRingElem()
   ccall((:fmpz_divisor_sigma, libflint), Nothing,
         (Ref{ZZRingElem}, UInt, Ref{ZZRingElem}), z, UInt(y), x)
   return z
end

divisor_sigma(x::ZZRingElem, y::ZZRingElem) = divisor_sigma(x, Int(y))
divisor_sigma(x::Int, y::Int) = Int(divisor_sigma(ZZRingElem(x), y))

@doc raw"""
    euler_phi(x::ZZRingElem)
    euler_phi(x::Int)

Return the value of the Euler phi function at $x$, i.e. the number of
positive integers up to $x$ (inclusive) that are coprime with $x$. An
exception is raised if $x \leq 0$.

# Examples

```jldoctest
julia> euler_phi(ZZ(12480))
3072

julia> euler_phi(12480)
3072
```
"""
function euler_phi(x::ZZRingElem)
   x <= 0 && throw(DomainError(x, "Argument must be positive"))
   z = ZZRingElem()
   ccall((:fmpz_euler_phi, libflint), Nothing,
         (Ref{ZZRingElem}, Ref{ZZRingElem}), z, x)
   return z
end

euler_phi(x::Int) = Int(euler_phi(ZZRingElem(x)))

@doc raw"""
    number_of_partitions(x::Int)
    number_of_partitions(x::ZZRingElem)

Return the number of partitions of $x$.

# Examples

```jldoctest
julia> number_of_partitions(100)
190569292

julia> number_of_partitions(ZZ(1000))
24061467864032622473692149727991
```
"""
function number_of_partitions(x::Int)
   if x < 0
      return 0
   end
   z = ZZRingElem()
   ccall((:partitions_fmpz_ui, libarb), Nothing,
         (Ref{ZZRingElem}, UInt), z, x)
   return Int(z)
end

function number_of_partitions(x::ZZRingElem)
   z = ZZRingElem()
   if x < 0
      return z
   end
   ccall((:partitions_fmpz_fmpz, libarb), Nothing,
         (Ref{ZZRingElem}, Ref{ZZRingElem}, Int), z, x, 0)
   return z
end

###############################################################################
#
#   Number bases/digits
#
###############################################################################

@doc raw"""
    bin(n::ZZRingElem)

Return $n$ as a binary string.

# Examples

```jldoctest
julia> bin(ZZ(12))
"1100"
```
"""
bin(n::ZZRingElem) = base(n, 2)

@doc raw"""
    oct(n::ZZRingElem)

Return $n$ as a octal string.

# Examples

```jldoctest
julia> oct(ZZ(12))
"14"
```
"""
oct(n::ZZRingElem) = base(n, 8)

@doc raw"""
    dec(n::ZZRingElem)

Return $n$ as a decimal string.

# Examples

```jldoctest
julia> dec(ZZ(12))
"12"
```
"""
dec(n::ZZRingElem) = base(n, 10)

@doc raw"""
    hex(n::ZZRingElem) = base(n, 16)

Return $n$ as a hexadecimal string.

# Examples

```jldoctest
julia> hex(ZZ(12))
"c"
```
"""
hex(n::ZZRingElem) = base(n, 16)

@doc raw"""
    base(n::ZZRingElem, b::Integer)

Return $n$ as a string in base $b$. We require $2 \leq b \leq 62$.

# Examples

```jldoctest
julia> base(ZZ(12), 13)
"c"
```
"""
function base(n::ZZRingElem, b::Integer)
    2 <= b <= 62 || error("invalid base: $b")
    p = ccall((:fmpz_get_str,libflint), Ptr{UInt8},
              (Ptr{UInt8}, Cint, Ref{ZZRingElem}), C_NULL, b, n)
    s = unsafe_string(p)
    ccall((:flint_free, libflint), Nothing, (Ptr{UInt8},), p)
    return s
end

@doc raw"""
    ndigits(x::ZZRingElem, b::Integer)

Return the number of digits of $x$ in the base $b$ (default is $b = 10$).

# Examples

```jldoctest
julia> ndigits(ZZ(12), 3)
3
```
"""
function Base.ndigits(x::ZZRingElem, b::Integer)::Int
   ndigits(x, base=b)
end

function Base.ndigits(a::ZZRingElem; base::Integer = 10, pad::Integer = 1)
   iszero(a) && return max(pad, 1)
   return max(pad, 1+flog(abs(a), ZZRingElem(abs(base))))
end

Base.digits(n::ZZRingElem; base::Integer = 10, pad::Integer = 1) =
   digits(typeof(base), n, base = base, pad = pad)

function Base.digits(T::Type{<:Integer}, n::ZZRingElem; base::Integer = 10, pad::Integer = 1)
   digits!(zeros(T, ndigits(n, base=base, pad=pad)), n, base=base)
end

function Base.digits!(a::AbstractVector{T}, n::ZZRingElem; base::Integer = 10) where T<:Integer
   2 <= base || throw(DomainError(base, "base must be ≥ 2"))
   Base.hastypemax(T) && abs(base) - 1 > typemax(T) &&
       throw(ArgumentError("type $T too small for base $base"))
   isempty(a) && return a

   if nbits(n)/ndigits(base, base = 2) > 100
     c = div(div(nbits(n), ndigits(base, base = 2)), 2)
     nn = ZZRingElem(base)^c
     q, r = divrem(n, nn)

     digits!(view(a, 1:c), r, base = base)
     digits!(view(a, c+1:length(a)), q, base = base)
     return a
   end

   for i in eachindex(a)
      n, r = divrem(n, base)
      a[i] = r
   end
   return a
end

@doc raw"""
    nbits(x::ZZRingElem)

Return the number of binary bits of $x$. We return zero if $x = 0$.

# Examples

```jldoctest
julia> nbits(ZZ(12))
4
```
"""
nbits(x::ZZRingElem) = iszero(x) ? 0 : Int(ccall((:fmpz_bits, libflint), Clong,
                  (Ref{ZZRingElem},), x))  

###############################################################################
#
#   Bit fiddling
#
###############################################################################

@doc raw"""
    popcount(x::ZZRingElem)

Return the number of ones in the binary representation of $x$.

# Examples

```jldoctest
julia> popcount(ZZ(12))
2
```
"""
popcount(x::ZZRingElem) = Int(ccall((:fmpz_popcnt, libflint), UInt,
                              (Ref{ZZRingElem},), x))

@doc raw"""
    prevpow2(x::ZZRingElem)

Return the previous power of $2$ up to including $x$.
"""
prevpow2(x::ZZRingElem) = x < 0 ? -prevpow2(-x) :
                            (x <= 2 ? x : one(FlintZZ) << (ndigits(x, 2) - 1))

@doc raw"""
    nextpow2(x::ZZRingElem)

Return the next power of $2$ that is at least $x$.

# Examples

```jldoctest
julia> nextpow2(ZZ(12))
16
```
"""
nextpow2(x::ZZRingElem) = x < 0 ? -nextpow2(-x) :
                            (x <= 2 ? x : one(FlintZZ) << ndigits(x - 1, 2))

@doc raw"""
    trailing_zeros(x::ZZRingElem)

Return the number of trailing zeros in the binary representation of $x$.
"""
trailing_zeros(x::ZZRingElem) = ccall((:fmpz_val2, libflint), Int,
                                (Ref{ZZRingElem},), x)

###############################################################################
#
#   Bitwise operations (unsafe)
#
###############################################################################

@doc raw"""
    clrbit!(x::ZZRingElem, c::Int)

Clear bit $c$ of $x$, where the least significant bit is the $0$-th bit. Note
that this function modifies its input in-place.

# Examples

```jldoctest
julia> a = ZZ(12)
12

julia> clrbit!(a, 3)

julia> a
4
```
"""
function clrbit!(x::ZZRingElem, c::Int)
    c < 0 && throw(DomainError(c, "Second argument must be non-negative"))
    ccall((:fmpz_clrbit, libflint), Nothing, (Ref{ZZRingElem}, UInt), x, c)
end

@doc raw"""
    setbit!(x::ZZRingElem, c::Int)

Set bit $c$ of $x$, where the least significant bit is the $0$-th bit. Note
that this function modifies its input in-place.

# Examples

```jldoctest
julia> a = ZZ(12)
12

julia> setbit!(a, 0)

julia> a
13
```
"""
function setbit!(x::ZZRingElem, c::Int)
    c < 0 && throw(DomainError(c, "Second argument must be non-negative"))
    ccall((:fmpz_setbit, libflint), Nothing, (Ref{ZZRingElem}, UInt), x, c)
end

@doc raw"""
    combit!(x::ZZRingElem, c::Int)

Complement bit $c$ of $x$, where the least significant bit is the $0$-th bit.
Note that this function modifies its input in-place.

# Examples

```jldoctest
julia> a = ZZ(12)
12

julia> combit!(a, 2)

julia> a
8
```
"""
function combit!(x::ZZRingElem, c::Int)
    c < 0 && throw(DomainError(c, "Second argument must be non-negative"))
    ccall((:fmpz_combit, libflint), Nothing, (Ref{ZZRingElem}, UInt), x, c)
end

@doc raw"""
    tstbit(x::ZZRingElem, c::Int)

Return bit $i$ of x (numbered from 0) as `true` for 1 or `false` for 0.

# Examples

```jldoctest
julia> a = ZZ(12)
12

julia> tstbit(a, 0)
false

julia> tstbit(a, 2)
true
```
"""
function tstbit(x::ZZRingElem, c::Int)
   return c >= 0 && Bool(ccall((:fmpz_tstbit, libflint), Cint,
                               (Ref{ZZRingElem}, UInt),
                               x, c))
end

###############################################################################
#
#   Unsafe operators
#
###############################################################################

function zero!(z::ZZRingElem)
   ccall((:fmpz_zero, libflint), Nothing,
         (Ref{ZZRingElem},), z)
   return z
end

function one!(z::ZZRingElem)
   ccall((:fmpz_set_ui, libflint), Nothing,
         (Ref{ZZRingElem}, UInt),
         z, 1)
   return z
end

function set!(z::ZZRingElem, a::ZZRingElem)
   ccall((:fmpz_set, libflint), Nothing,
         (Ref{ZZRingElem}, Ref{ZZRingElem}),
         z, a)
   return z
end

function swap!(a::ZZRingElem, b::ZZRingElem)
   ccall((:fmpz_swap, libflint), Nothing,
         (Ref{ZZRingElem}, Ref{ZZRingElem}),
         a, b)
end

function addeq!(z::ZZRingElem, x::ZZRingElem)
   ccall((:fmpz_add, libflint), Nothing,
         (Ref{ZZRingElem}, Ref{ZZRingElem}, Ref{ZZRingElem}), z, z, x)
   return z
end

function add!(z::ZZRingElem, x::ZZRingElem, y::ZZRingElem)
   ccall((:fmpz_add, libflint), Nothing,
         (Ref{ZZRingElem}, Ref{ZZRingElem}, Ref{ZZRingElem}), z, x, y)
   return z
end

function add!(z::ZZRingElem, x::ZZRingElem, y::Int)
   ccall((:fmpz_add_si, libflint), Nothing,
         (Ref{ZZRingElem}, Ref{ZZRingElem}, Int), z, x, y)
   return z
end

function add!(z::ZZRingElem, a::ZZRingElem, b::UInt)
   ccall((:fmpz_add_ui, libflint), Nothing,
         (Ref{ZZRingElem}, Ref{ZZRingElem}, UInt),
         z, a, b)
   return z
end

function add!(a::ZZRingElem, b::ZZRingElem, c::Ptr{Int})
   ccall((:fmpz_add, Nemo.libflint), Nothing, (Ref{ZZRingElem}, Ref{ZZRingElem}, Ptr{Int}), a, b, c)
   return a
end

add!(z::ZZRingElem, a::ZZRingElem, b::Integer) = add!(z, a, ZZRingElem(b))
add!(z::ZZRingElem, x::Int, y::ZZRingElem) = add!(z, y, x)

function neg!(z::ZZRingElem, a::ZZRingElem)
   ccall((:fmpz_neg, libflint), Nothing,
         (Ref{ZZRingElem}, Ref{ZZRingElem}),
         z, a)
   return z
end

function neg!(a::ZZRingElem)
   return neg!(a,a)
end

function sub!(z::ZZRingElem, a::ZZRingElem, b::ZZRingElem)
   ccall((:fmpz_sub, libflint), Nothing,
         (Ref{ZZRingElem}, Ref{ZZRingElem}, Ref{ZZRingElem}),
         z, a, b)
   return z
end

function sub!(z::ZZRingElem, a::ZZRingElem, b::Int)
   ccall((:fmpz_sub_si, libflint), Nothing,
         (Ref{ZZRingElem}, Ref{ZZRingElem}, Int),
         z, a, b)
   return z
end

function sub!(z::ZZRingElem, a::ZZRingElem, b::UInt)
   ccall((:fmpz_sub_ui, libflint), Nothing,
         (Ref{ZZRingElem}, Ref{ZZRingElem}, UInt),
         z, a, b)
   return z
end

function sub!(z::ZZRingElem, a::ZZRingElem, b::Integer)
   return sub!(z, a, ZZRingElem(b))
end

function sub!(z::ZZRingElem, b::Integer, a::ZZRingElem)
   sub!(z, a, b)
   return neg!(z, z)
end


function mul!(z::ZZRingElem, x::ZZRingElem, y::ZZRingElem)
   ccall((:fmpz_mul, libflint), Nothing,
         (Ref{ZZRingElem}, Ref{ZZRingElem}, Ref{ZZRingElem}), z, x, y)
   return z
end

function mul!(z::ZZRingElem, x::ZZRingElem, y::Int)
   ccall((:fmpz_mul_si, libflint), Nothing,
         (Ref{ZZRingElem}, Ref{ZZRingElem}, Int), z, x, y)
   return z
end

function mul!(z::ZZRingElem, a::ZZRingElem, b::UInt)
   ccall((:fmpz_mul_ui, libflint), Nothing,
         (Ref{ZZRingElem}, Ref{ZZRingElem}, UInt),
         z, a, b)
   return z
end

mul!(z::ZZRingElem, a::ZZRingElem, b::Integer) = mul!(z, a, ZZRingElem(b))

mul!(z::ZZRingElem, x::Int, y::ZZRingElem) = mul!(z, y, x)

function mul!(a::ZZRingElem, b::ZZRingElem, c::Ptr{Int})
   ccall((:fmpz_mul, Nemo.libflint), Nothing, (Ref{ZZRingElem}, Ref{ZZRingElem}, Ptr{Int}), a, b, c)
   return a
end

function addmul!(z::ZZRingElem, x::ZZRingElem, y::ZZRingElem)
   ccall((:fmpz_addmul, libflint), Nothing,
         (Ref{ZZRingElem}, Ref{ZZRingElem}, Ref{ZZRingElem}), z, x, y)
   return z
end

addmul!(z::ZZRingElem, x::ZZRingElem, y::ZZRingElem, ::ZZRingElem) = addmul!(z, x, y)

function addmul!(z::ZZRingElem, x::ZZRingElem, y::Int)
   ccall((:fmpz_addmul_si, libflint), Nothing,
         (Ref{ZZRingElem}, Ref{ZZRingElem}, Int), z, x, y)
   return z
end

addmul!(z::ZZRingElem, x::ZZRingElem, y::Int, ::ZZRingElem) = addmul!(z, x, y)

function submul!(z::ZZRingElem, a::ZZRingElem, b::ZZRingElem)
   ccall((:fmpz_submul, libflint), Nothing,
         (Ref{ZZRingElem}, Ref{ZZRingElem}, Ref{ZZRingElem}),
         z, a, b)
   return z
end

@doc raw"""
    fmma!(r::ZZRingElem, a::ZZRingElem, b::ZZRingElem, c::ZZRingElem, d::ZZRingElem)

Return $r = a b + c d$, changing $r$ in-place.
"""
function fmma!(r::ZZRingElem, a::ZZRingElem, b::ZZRingElem, c::ZZRingElem, d::ZZRingElem)
   ccall((:fmpz_fmma, libflint), Nothing,
         (Ref{ZZRingElem}, Ref{ZZRingElem}, Ref{ZZRingElem}, Ref{ZZRingElem}, Ref{ZZRingElem}), r, a, b, c, d)
   return r
end

@doc raw"""
    fmms!(r::ZZRingElem, a::ZZRingElem, b::ZZRingElem, c::ZZRingElem, d::ZZRingElem)

Return $r = a b - c d$, changing $r$ in-place.
"""
function fmms!(r::ZZRingElem, a::ZZRingElem, b::ZZRingElem, c::ZZRingElem, d::ZZRingElem)
   ccall((:fmpz_fmms, libflint), Nothing,
         (Ref{ZZRingElem}, Ref{ZZRingElem}, Ref{ZZRingElem}, Ref{ZZRingElem}, Ref{ZZRingElem}), r, a, b, c, d)
   return r
end


function divexact!(z::ZZRingElem, a::ZZRingElem, b::UInt)
   ccall((:fmpz_divexact_ui, libflint), Nothing,
         (Ref{ZZRingElem}, Ref{ZZRingElem}, UInt),
         z, a, b)
   return z
end

function divexact!(z::ZZRingElem, a::ZZRingElem, b::ZZRingElem)
   ccall((:fmpz_divexact, libflint), Nothing,
         (Ref{ZZRingElem}, Ref{ZZRingElem}, Ref{ZZRingElem}),
         z, a, b)
   return z
end

function pow!(z::ZZRingElem, a::ZZRingElem, b::Union{Int, UInt})
   ccall((:fmpz_pow_ui, libflint), Nothing,
         (Ref{ZZRingElem}, Ref{ZZRingElem}, UInt),
         z, a, UInt(b))
   return z
end

function lcm!(z::ZZRingElem, x::ZZRingElem, y::ZZRingElem)
   ccall((:fmpz_lcm, libflint), Nothing,
       (Ref{ZZRingElem}, Ref{ZZRingElem}, Ref{ZZRingElem}), z, x, y)
   return z
end

function gcd!(z::ZZRingElem, x::ZZRingElem, y::ZZRingElem)
   ccall((:fmpz_gcd, libflint), Nothing,
       (Ref{ZZRingElem}, Ref{ZZRingElem}, Ref{ZZRingElem}), z, x, y)
   return z
end

###############################################################################
#
#   Parent object overloads
#
###############################################################################

(::ZZRing)() = ZZRingElem()

(::ZZRing)(a::Integer) = ZZRingElem(a)

(::ZZRing)(a::AbstractString) = ZZRingElem(a)

(::ZZRing)(a::ZZRingElem) = a

(::ZZRing)(a::Float64) = ZZRingElem(a)

(::ZZRing)(a::Float32) = ZZRingElem(Float64(a))

(::ZZRing)(a::Float16) = ZZRingElem(Float64(a))

(::ZZRing)(a::BigFloat) = ZZRingElem(BigInt(a))

@doc raw"""
    (ZZ::ZZRing)(x)

Coerce `x` into an element of $\mathbb Z$. Note that `ZZ(x)` is equivalent to [`ZZRingElem(x)`](@ref).

# Examples

```jldoctest
julia> ZZ(2)
2

julia> ZZ(2)^100
1267650600228229401496703205376
```
"""
(::ZZRing)(x)

###############################################################################
#
#   String parser
#
###############################################################################

function parse(::Type{ZZRingElem}, s::AbstractString, base::Int = 10)
    if s[1] == '-'
        sgn = -1
        s2 = string(SubString(s, 2))
    else
        sgn = 1
        s2 = string(s)
    end
    z = ZZRingElem()
    err = ccall((:fmpz_set_str, libflint),
               Int32, (Ref{ZZRingElem}, Ptr{UInt8}, Int32),
               z, s2, base)
    err == 0 || error("Invalid big integer: $(repr(s))")
    return sgn < 0 ? -z : z
end

###############################################################################
#
#   Random generation
#
###############################################################################

RandomExtensions.maketype(R::ZZRing, _) = ZZRingElem

# define rand(make(ZZ, n:m))
rand(rng::AbstractRNG, sp::SamplerTrivial{<:Make2{ZZRingElem,ZZRing}}) =
        sp[][1](rand(rng, sp[][2]))

rand(rng::AbstractRNG, R::ZZRing, n::AbstractArray) = R(rand(rng, n))

rand(R::ZZRing, n::AbstractArray) = rand(Random.GLOBAL_RNG, R, n)

@doc raw"""
    rand_bits(::ZZRing, b::Int)

Return a random signed integer whose absolute value has $b$ bits.
"""
function rand_bits(::ZZRing, b::Int)
   b >= 0 || throw(DomainError(b, "Bit count must be non-negative"))
   z = ZZRingElem()
   ccall((:fmpz_randbits, libflint), Nothing,(Ref{ZZRingElem}, Ptr{Cvoid}, Int),
         z, _flint_rand_states[Threads.threadid()].ptr, b)
   return z
end

@doc raw"""
    rand_bits_prime(::ZZRing, n::Int, proved::Bool=true)

Return a random prime number with the given number of bits. If only a
probable prime is required, one can pass `proved=false`.
"""
function rand_bits_prime(::ZZRing, n::Int, proved::Bool = true)
   n < 2 && throw(DomainError(n, "No primes with that many bits"))
   z = ZZRingElem()
   ccall((:fmpz_randprime, libflint), Nothing,
	 (Ref{ZZRingElem}, Ptr{Cvoid}, Int, Cint),
	  z, _flint_rand_states[Threads.threadid()].ptr, n, Cint(proved))
   return z
end

# rand in a range
# this mirrors the implementation for BigInt in the Random module

using Base.GMP: Limb, MPZ

# "views" a non-small ZZRingElem as a readonly BigInt
# the caller must call GC.@preserve appropriately
function _as_bigint(z::ZZRingElem)
   @assert !_fmpz_is_small(z)
   unsafe_load(Ptr{BigInt}(z.d << 2))
end


Random.Sampler(::Type{<:AbstractRNG}, r::StepRange{ZZRingElem, ZZRingElem}, ::Random.Repetition) =
   SamplerFmpz(r)

struct SamplerFmpz <: Random.Sampler{ZZRingElem}
   a::ZZRingElem           # first
   m::BigInt         # range length - 1
   nlimbs::Int       # number of limbs in generated BigInt's (z ∈ [0, m])
   nlimbsmax::Int    # max number of limbs for z+a
   mask::Limb        # applied to the highest limb

   ## diverges from Random.SamplerBigInt:
   step::ZZRingElem
end

function SamplerFmpz(r::StepRange{ZZRingElem, ZZRingElem})
   r1 = first(r)
   r2 = last(r)
   s = step(r)

   if isone(s)
      m = BigInt(r2 - r1)
   else
      m = length(r)::BigInt # type assertion in case length is changed to return an ZZRingElem
      MPZ.sub_ui!(m, 1)
   end

   m < 0 && throw(ArgumentError("range must be non-empty"))
   nd = ndigits(m, base=2)
   nlimbs, highbits = divrem(nd, 8*sizeof(Limb))
   highbits > 0 && (nlimbs += 1)
   mask = highbits == 0 ? ~zero(Limb) : one(Limb)<<highbits - one(Limb)
   GC.@preserve r1 r2 begin
      a1 = _fmpz_is_small(r1) ? 1 : _as_bigint(r1).size
      a2 = _fmpz_is_small(r2) ? 1 : _as_bigint(r2).size
   end
   nlimbsmax = max(nlimbs, abs(a1), abs(a2))
   return SamplerFmpz(r1, m, nlimbs, nlimbsmax, mask, s)
end

function rand(rng::AbstractRNG, sp::SamplerFmpz)
   z = ZZRingElem()
   # this make sure z is backed up by an mpz_t object:
   ccall((:fmpz_init2, libflint), Nothing, (Ref{ZZRingElem}, UInt), z, sp.nlimbsmax)
   @assert !_fmpz_is_small(z)
   GC.@preserve z begin
      x = _as_bigint(z)
      limbs = Random.UnsafeView(x.d, sp.nlimbs)
      while true
         rand!(rng, limbs)
         limbs[end] &= sp.mask
         MPZ.mpn_cmp(x, sp.m, sp.nlimbs) <= 0 && break
      end
      # adjust x.size (normally done by mpz_limbs_finish, in GMP version >= 6)
      sz = sp.nlimbs
      while sz > 0
         limbs[sz] != 0 && break
         sz -= 1
      end
      # write sz in the .size field of the mpz object
      unsafe_store!(Ptr{Cint}(z.d << 2) + sizeof(Cint), sz)
   end
   if !isone(sp.step)
      mul!(z, z, sp.step)
   end
   add!(z, z, sp.a)
end


###############################################################################
#
#   Constructors
#
###############################################################################

ZZRingElem(s::AbstractString) = parse(ZZRingElem, s)

ZZRingElem(z::Integer) = ZZRingElem(BigInt(z))

ZZRingElem(z::Float16) = ZZRingElem(Float64(z))

ZZRingElem(z::Float32) = ZZRingElem(Float64(z))

ZZRingElem(z::BigFloat) = ZZRingElem(BigInt(z))

###############################################################################
#
#   Conversions and promotions
#
###############################################################################

convert(::Type{ZZRingElem}, a::Integer) = ZZRingElem(a)

function (::Type{BigInt})(a::ZZRingElem)
   r = BigInt()
   ccall((:fmpz_get_mpz, libflint), Nothing, (Ref{BigInt}, Ref{ZZRingElem}), r, a)
   return r
end

function (::Type{Int})(a::ZZRingElem)
   (a > typemax(Int) || a < typemin(Int)) && throw(InexactError(:convert, Int, a))
   return ccall((:fmpz_get_si, libflint), Int, (Ref{ZZRingElem},), a)
end

function (::Type{UInt})(a::ZZRingElem)
   (a > typemax(UInt) || a < 0) && throw(InexactError(:convert, UInt, a))
   return ccall((:fmpz_get_ui, libflint), UInt, (Ref{ZZRingElem}, ), a)
end

if Culong !== UInt
    function (::Type{Culong})(a::ZZRingElem)
       fits(Culong, a) || throw(InexactError(:convert, Culong, a))
       return ccall((:fmpz_get_ui, libflint), UInt, (Ref{ZZRingElem}, ), a)%Culong
    end
end

(::Type{T})(a::ZZRingElem) where T <: Union{Int8, Int16, Int32} = T(Int(a))

(::Type{T})(a::ZZRingElem) where T <: Union{UInt8, UInt16, UInt32} = T(UInt(a))

convert(::Type{T}, a::ZZRingElem) where T <: Integer = T(a)

function (::Type{Float64})(n::ZZRingElem)
    # rounds to zero
    ccall((:fmpz_get_d, libflint), Float64, (Ref{ZZRingElem},), n)
end

convert(::Type{Float64}, n::ZZRingElem) = Float64(n)

(::Type{Float32})(n::ZZRingElem) = Float32(Float64(n))

convert(::Type{Float32}, n::ZZRingElem) = Float32(n)

(::Type{Float16})(n::ZZRingElem) = Float16(Float64(n))

convert(::Type{Float16}, n::ZZRingElem) = Float16(n)

(::Type{BigFloat})(n::ZZRingElem) = BigFloat(BigInt(n))

convert(::Type{BigFloat}, n::ZZRingElem) = BigFloat(n)

Base.promote_rule(::Type{ZZRingElem}, ::Type{T}) where {T <: Integer} = ZZRingElem

promote_rule(::Type{ZZRingElem}, ::Type{T}) where {T <: Integer} = ZZRingElem

###############################################################################
#
#  Perfect power detection
#
###############################################################################

# 1, 0, -1 are perfect powers
# ex is not guaranteed to be maximal
function _is_perfect_power(a::ZZRingElem)
  rt = ZZRingElem()
  ex = ccall((:fmpz_is_perfect_power, libflint), Int, (Ref{ZZRingElem}, Ref{ZZRingElem}), rt, a)
  return rt, ex
end

@doc raw"""
    is_perfect_power(a::IntegerUnion)

Returns whether $a$ is a perfect power, that is, whether $a = m^r$ for some
integer $m$ and $r > 1$.
"""
function is_perfect_power(a::ZZRingElem)
  _, ex = _is_perfect_power(a)
  return ex > 0
end

is_perfect_power(a::Integer) = is_perfect_power(ZZRingElem(a))

# Returns $e$, $r$ such that $a = r^e$ with $e$ maximal. Note: $1 = 1^0$.
function _maximal_integer_root(a::ZZRingElem)
  if iszero(a)
    error("must not be zero")
  end
  if isone(a)
    return 0, a
  end
  if a < 0
    e, r = _maximal_integer_root(-a)
    if isone(e)
      return 1, a
    end
    v, s = iszero(e) ? (0, 0) : remove(e, 2)
    return s, -r^(2^v)
  end
  rt = ZZRingElem()
  e = 1
  while true
    rt, ex = _is_perfect_power(a)
    if ex == 1 || ex == 0
      return e, a
    end
    e *= ex
    a = rt
  end
end

@doc raw"""
    is_prime_power(q::IntegerUnion) -> Bool

Returns whether $q$ is a prime power.
"""
is_prime_power(::IntegerUnion)

function is_prime_power(q::ZZRingElem)
  iszero(q) && return false
  e, a = _maximal_integer_root(q)
  return isprime(a)
end

is_prime_power(q::Integer) = is_prime_power(ZZRingElem(q))

@doc raw"""
    is_prime_power_with_data(q::IntegerUnion) -> Bool, ZZRingElem, Int

Returns a flag indicating whether $q$ is a prime power and integers $e, p$ such
that $q = p^e$. If $q$ is a prime power, than $p$ is a prime.
"""
is_prime_power_with_data(::IntegerUnion)

function is_prime_power_with_data(q::ZZRingElem)
  iszero(q) && return false, 1, q
  e, a = _maximal_integer_root(q)
  return isprime(a), e, a
end

function is_prime_power_with_data(q::Integer)
  e, a = _maximal_integer_root(ZZRingElem(q))
  return isprime(a), e, typeof(q)(a)
end

###############################################################################
#
#   Convenience methods for arithmetics (since `ZZRingElem` is not a `Number` type)
#
###############################################################################

//(v::Vector{ZZRingElem}, x::ZZRingElem) = v .// x
*(x::ZZRingElem, v::Vector{ZZRingElem}) = x .* v
*(v::Vector{ZZRingElem}, x::ZZRingElem) = v .* x
