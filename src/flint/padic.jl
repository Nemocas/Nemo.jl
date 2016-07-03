###############################################################################
#
#   padic.jl : flint padic numbers
#
###############################################################################

export FlintPadicField, padic, prime, teichmuller, log, sqrt

###############################################################################
#
#   Data type and parent object methods
#
###############################################################################

doc"""
    O(R::FlintPadicField, m::fmpz)
> Construct the value $0 + O(p^n)$ given $m = p^n$. An exception results if $m$
> is not found to be a power of `p = prime(R)`.
"""
function O(R::FlintPadicField, m::fmpz)
   if m == 1
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
   d = padic(N)
   d.parent = R
   return d
end

doc"""
    O(R::FlintPadicField, m::fmpq)
> Construct the value $0 + O(p^n)$ given $m = p^n$. An exception results if $m$
> is not found to be a power of `p = prime(R)`.
"""
function O(R::FlintPadicField, m::fmpq)
   d = den(m)
   if d == 1
      return O(R, num(m))
   end
   num(m) != 1 && error("Not a power of p in p-adic O()")
   p = prime(R)
   if d == p
      N = -1
   else
     N = -flog(d, p) 
     p^(-N) != d && error("Not a power of p in p-adic O()")
   end
   r = padic(N)
   r.parent = R
   return r
end

doc"""
    O(R::FlintPadicField, m::Integer)
> Construct the value $0 + O(p^n)$ given $m = p^n$. An exception results if $m$
> is not found to be a power of `p = prime(R)`.
"""
O(R::FlintPadicField, m::Integer) = O(R, fmpz(m))

elem_type(::FlintPadicField) = padic

doc"""
    base_ring(a::FlintPadicField)
> Returns `Union{}` as this field is not dependent on another field.
"""
base_ring(a::FlintPadicField) = Union{}

doc"""
    base_ring(a::padic)
> Returns `Union{}` as this field is not dependent on another field.
"""
base_ring(a::padic) = Union{}

doc"""
    parent(a::padic)
> Returns the parent of the given p-adic field element.
"""
parent(a::padic) = a.parent

function check_parent(a::padic, b::padic)
   parent(a) != parent(b) && 
      error("Incompatible padic rings in padic operation")
end

###############################################################################
#
#   Basic manipulation
#
###############################################################################

function Base.hash(a::padic, h::UInt)
   return hash(lift(FlintQQ, a), h) $ hash(prime(parent(a)), h) $ h
end

doc"""
    prime(R::FlintPadicField)
> Return the prime $p$ for the given $p$-adic field.
"""
function prime(R::FlintPadicField)
   z = fmpz()
   ccall((:padic_ctx_pow_ui, :libflint), Void, 
         (Ptr{fmpz}, Int, Ptr{FlintPadicField}), &z, 1, &R)
   return z 
end

doc"""
    precision(a::padic)
> Return the precision of the given $p$-adic field element, i.e. if the element
> is known to $O(p^n)$ this function will return $n$.
"""
precision(a::padic) = a.N

doc"""
    valuation(a::padic) 
> Return the valuation of the given $p$-adic field element, i.e. if the given
> element is divisible by $p^n$ but not a higher power of $p$ then the function
> will return $n$.
"""
valuation(a::padic) = a.v

doc"""
    lift(R::FlintRationalField, a::padic)
> Return a lift of the given $p$-adic field element to $\mathbb{Q}$.
"""
function lift(R::FlintRationalField, a::padic)
    ctx = parent(a)
    r = fmpq()
    ccall((:padic_get_fmpq, :libflint), Void,
          (Ptr{fmpq}, Ptr{padic}, Ptr{FlintPadicField}), &r, &a, &ctx)
    return r
end

doc"""
    lift(R::FlintIntegerRing, a::padic)
> Return a lift of the given $p$-adic field element to $\mathbb{Z}$.
"""
function lift(R::FlintIntegerRing, a::padic)
    ctx = parent(a)
    r = fmpz()
    ccall((:padic_get_fmpz, :libflint), Void,
          (Ptr{fmpz}, Ptr{padic}, Ptr{FlintPadicField}), &r, &a, &ctx)
    return r
end

doc"""
    zero(R::FlintPadicField)
> Return zero in the given $p$-adic field, to the default precision.
"""
function zero(R::FlintPadicField)
   z = padic(R.prec_max)
   ccall((:padic_zero, :libflint), Void, (Ptr{padic},), &z)
   z.parent = R
   return z
end

doc"""
    one(R::FlintPadicField)
> Return zero in the given $p$-adic field, to the default precision.
"""
function one(R::FlintPadicField)
   z = padic(R.prec_max)
   ccall((:padic_one, :libflint), Void, (Ptr{padic},), &z)
   z.parent = R
   return z
end

doc"""
    iszero(a::padic)
> Return `true` if the given p-adic field element is zero, otherwise return
> `false`.
"""
iszero(a::padic) = Bool(ccall((:padic_is_zero, :libflint), Cint, 
                              (Ptr{padic},), &a))

doc"""
    isone(a::padic)
> Return `true` if the given p-adic field element is one, otherwise return
> `false`.
"""
isone(a::padic) = Bool(ccall((:padic_is_one, :libflint), Cint,
                             (Ptr{padic},), &a))

doc"""
    isunit(a::padic)
> Return `true` if the given p-adic field element is invertible, i.e. nonzero,
> otherwise return `false`.
"""
isunit(a::padic) = !Bool(ccall((:padic_is_zero, :libflint), Cint, 
                              (Ptr{padic},), &a))

###############################################################################
#
#   AbstractString{} I/O
#
###############################################################################

function show(io::IO, x::padic)
   ctx = parent(x)
   cstr = ccall((:padic_get_str, :libflint), Ptr{UInt8}, 
               (Ptr{Void}, Ptr{padic}, Ptr{FlintPadicField}),
                   C_NULL, &x, &ctx)

   print(io, bytestring(cstr))

   ccall((:flint_free, :libflint), Void, (Ptr{UInt8},), cstr)
   print(io, " + O(")
   print(io, prime(ctx))
   print(io, "^$(x.N))")
end

function show(io::IO, R::FlintPadicField)
   print(io, "Field of ", prime(R), "-adic numbers")
end

needs_parentheses(x::padic) = true

is_negative(x::padic) = false

show_minus_one(::FlintPadicField) = true

###############################################################################
#
#   Canonicalisation
#
###############################################################################

canonical_unit(x::padic) = x

###############################################################################
#
#   Unary operators
#
###############################################################################

function -(x::padic)
   if iszero(x)
      return x
   end
   ctx = parent(x)
   z = padic(x.N)
   ccall((:padic_neg, :libflint), Void, 
         (Ptr{padic}, Ptr{padic}, Ptr{FlintPadicField}), 
                     &z, &x, &ctx)
   z.parent = ctx
   return z
end

###############################################################################
#
#   Binary operators
#
###############################################################################

function +(x::padic, y::padic)
   check_parent(x, y)
   ctx = parent(x)
   z = padic(min(x.N, y.N))
   z.parent = ctx
   ccall((:padic_add, :libflint), Void, 
         (Ptr{padic}, Ptr{padic}, Ptr{padic}, Ptr{FlintPadicField}), 
               &z, &x, &y, &ctx)
   return z
end

function -(x::padic, y::padic)
   check_parent(x, y)
   ctx = parent(x)
   z = padic(min(x.N, y.N))
   z.parent = ctx
   ccall((:padic_sub, :libflint), Void, 
         (Ptr{padic}, Ptr{padic}, Ptr{padic}, Ptr{FlintPadicField}), 
                  &z, &x, &y, &ctx)
   return z
end

function *(x::padic, y::padic)
   check_parent(x, y)
   ctx = parent(x)
   z = padic(min(x.N + y.v, y.N + x.v))
   z.parent = ctx
   ccall((:padic_mul, :libflint), Void, 
         (Ptr{padic}, Ptr{padic}, Ptr{padic}, Ptr{FlintPadicField}), 
               &z, &x, &y, &ctx)
   return z
end

###############################################################################
#
#   Ad hoc binary operators
#
###############################################################################

+(a::padic, b::Integer) = a + parent(a)(b)

+(a::padic, b::fmpz) = a + parent(a)(b)

+(a::padic, b::fmpq) = a + parent(a)(b)

+(a::Integer, b::padic) = b + a

+(a::fmpz, b::padic) = b + a

+(a::fmpq, b::padic) = b + a

-(a::padic, b::Integer) = a - parent(a)(b)

-(a::padic, b::fmpz) = a - parent(a)(b)

-(a::padic, b::fmpq) = a - parent(a)(b)

-(a::Integer, b::padic) = parent(b)(a) - b

-(a::fmpz, b::padic) = parent(b)(a) - b

-(a::fmpq, b::padic) = parent(b)(a) - b

*(a::padic, b::Integer) = a*parent(a)(b)

*(a::padic, b::fmpz) = a*parent(a)(b)

*(a::padic, b::fmpq) = a*parent(a)(b)

*(a::Integer, b::padic) = b*a

*(a::fmpz, b::padic) = b*a

*(a::fmpq, b::padic) = b*a

###############################################################################
#
#   Comparison
#
###############################################################################

function ==(a::padic, b::padic)
   check_parent(a, b)
   ctx = parent(a)
   z = padic(min(a.N, b.N))
   ccall((:padic_sub, :libflint), Void, 
         (Ptr{padic}, Ptr{padic}, Ptr{padic}, Ptr{FlintPadicField}), 
               &z, &a, &b, &ctx)
   return Bool(ccall((:padic_is_zero, :libflint), Cint, 
                (Ptr{padic},), &z))
end

function isequal(a::padic, b::padic)
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

==(a::padic, b::Integer) = a == parent(a)(b)

==(a::padic, b::fmpz) = a == parent(a)(b)

==(a::padic, b::fmpq) = a == parent(a)(b)

==(a::Integer, b::padic) = parent(b)(a) == b

==(a::fmpz, b::padic) = parent(b)(a) == b

==(a::fmpq, b::padic) = parent(b)(a) == b

###############################################################################
#
#   Powering
#
###############################################################################

function ^(a::padic, n::Int)
   ctx = parent(a)
   z = padic(a.N + (n - 1)*a.v)
   z.parent = ctx
   ccall((:padic_pow_si, :libflint), Void, 
                (Ptr{padic}, Ptr{padic}, Int, Ptr{FlintPadicField}), 
               &z, &a, n, &ctx)
   return z
end

###############################################################################
#
#   Exact division
#
###############################################################################

function divexact(a::padic, b::padic)
   b == 0 && throw(DivideError())
   check_parent(a, b)
   ctx = parent(a)
   z = padic(min(a.N - b.v, b.N - 2*b.v + a.v))
   z.parent = ctx
   ccall((:padic_div, :libflint), Cint, 
         (Ptr{padic}, Ptr{padic}, Ptr{padic}, Ptr{FlintPadicField}),
               &z, &a, &b, &ctx)
   return z
end

###############################################################################
#
#   Ad hoc exact division
#
###############################################################################

divexact(a::padic, b::Integer) = a*(fmpz(1)//fmpz(b))

divexact(a::padic, b::fmpz) = a*(1//b)

divexact(a::padic, b::fmpq) = a*inv(b)

divexact(a::Integer, b::padic) = fmpz(a)*inv(b)

divexact(a::fmpz, b::padic) = inv((fmpz(1)//a)*b)

divexact(a::fmpq, b::padic) = inv(inv(a)*b)

###############################################################################
#
#   Inversion
#
###############################################################################

doc"""
    inv(a::padic)
> Returns $a^{-1}$. If $a = 0$ a `DivideError()` is thrown.
"""
function inv(a::padic)
   a == 0 && throw(DivideError())
   ctx = parent(a)
   z = padic(a.N - 2*a.v)
   z.parent = ctx
   ccall((:padic_inv, :libflint), Cint, 
         (Ptr{padic}, Ptr{padic}, Ptr{FlintPadicField}), &z, &a, &ctx)
   return z
end

###############################################################################
#
#   GCD
#
###############################################################################

doc"""
    gcd(x::padic, y::padic)
> Returns the greatest common divisor of $x$ and $y$, i.e. the function returns
> $1$ unless both $a$ and $b$ are $0$, in which case it returns $0$.
"""
function gcd(x::padic, y::padic)
   check_parent(x, y)
   if x == 0 && y == 0 
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

doc"""
    sqrt(a::padic)
> Return the $p$-adic square root of $a$. We define this only when the
> valuation of $a$ is even. The precision of the output will be
> precision$(a) -$ valuation$(a)/2$. If the square root does not exist, an
> exception is thrown.
"""
function sqrt(a::padic)
   (a.v % 2) != 0 && error("Unable to take padic square root")
   ctx = parent(a)
   z = padic(a.N - div(a.v, 2))
   z.parent = ctx
   res = Bool(ccall((:padic_sqrt, :libflint), Cint, 
                    (Ptr{padic}, Ptr{padic}, Ptr{FlintPadicField}), &z, &a, &ctx))      
   !res && error("Square root of p-adic does not exist")
   return z
end

###############################################################################
#
#   Special functions
#
###############################################################################

doc"""
    exp(a::padic)
> Return the $p$-adic exponential of $a$. We define this only when the
> valuation of $a$ is positive (unless $a = 0$). The precision of the output
> will be the same as the precision of the input. If the input is not valid an
> exception is thrown.
"""
function exp(a::padic) 
   a != 0 && a.v <= 0 && throw(DomainError())
   ctx = parent(a)
   z = padic(a.N)
   z.parent = ctx
   res = Bool(ccall((:padic_exp, :libflint), Cint, 
                    (Ptr{padic}, Ptr{padic}, Ptr{FlintPadicField}), &z, &a, &ctx))
   !res && error("Unable to compute exponential")
   return z
end

doc"""
    log(a::padic)
> Return the $p$-adic logarithm of $a$. We define this only when the valuation
> of $a$ is zero (but not for $a == 0$). The precision of the output will be
> the same as the precision of the input. If the input is not valid an
> exception is thrown.
"""
function log(a::padic) 
   (a.v > 0 || a.v < 0 || a == 0) && throw(DomainError())
   ctx = parent(a)
   z = padic(a.N)
   z.parent = ctx
   res = Bool(ccall((:padic_log, :libflint), Cint,
                    (Ptr{padic}, Ptr{padic}, Ptr{FlintPadicField}), &z, &a, &ctx))
   !res && error("Unable to compute logarithm")
   return z
end

doc"""
    teichmuller(a::padic)
> Return the Teichmuller lift of the $p$-adic value $a$. We require the
> valuation of $a$ to be nonnegative. The precision of the output will be the
> same as the precision of the input. For convenience, if $a$ is congruent to
> zero modulo $p$ we return zero. If the input is not valid an exception is
> thrown.
"""
function teichmuller(a::padic)
   a.v < 0 && throw(DomainError())
   ctx = parent(a)
   z = padic(a.N)
   z.parent = ctx
   ccall((:padic_teichmuller, :libflint), Void,
         (Ptr{padic}, Ptr{padic}, Ptr{FlintPadicField}), &z, &a, &ctx)
   return z
end
  
###############################################################################
#
#   Unsafe operators
#
###############################################################################

function mul!(z::padic, x::padic, y::padic)
   z.N = min(x.N + y.v, y.N + x.v)
   ctx = parent(x)
   ccall((:padic_mul, :libflint), Void, 
         (Ptr{padic}, Ptr{padic}, Ptr{padic}, Ptr{FlintPadicField}), 
               &z, &x, &y, &ctx)
end

function addeq!(x::padic, y::padic)
   x.N = min(x.N, y.N)
   ctx = parent(x)
   ccall((:padic_add, :libflint), Void, 
         (Ptr{padic}, Ptr{padic}, Ptr{padic}, Ptr{FlintPadicField}), 
               &x, &x, &y, &ctx)
end

###############################################################################
#
#   Conversions and promotions
#
###############################################################################

promote_rule{T<:Integer}(::Type{padic}, ::Type{T}) = padic

promote_rule(::Type{padic}, ::Type{fmpz}) = padic

promote_rule(::Type{padic}, ::Type{fmpq}) = padic

###############################################################################
#
#   Parent object overloads
#
###############################################################################

function Base.call(R::FlintPadicField)
   z = padic(R.prec_max)
   z.parent = R
   return z
end

function Base.call(R::FlintPadicField, n::fmpz)
   if n == 1
      N = 0
   else
      p = prime(R)
      N, = remove(n, p) 
   end
   z = padic(N + R.prec_max)
   ccall((:padic_set_fmpz, :libflint), Void, 
         (Ptr{padic}, Ptr{fmpz}, Ptr{FlintPadicField}), &z, &n, &R)
   z.parent = R
   return z
end

function Base.call(R::FlintPadicField, n::fmpq)
   m = den(n)
   if m == 1
      return R(num(n))
   end
   p = prime(R)
   if m == p
      N = -1
   else
     N = -flog(m, p) 
   end
   z = padic(N + R.prec_max)
   ccall((:padic_set_fmpq, :libflint), Void, 
         (Ptr{padic}, Ptr{fmpq}, Ptr{FlintPadicField}), &z, &n, &R)
   z.parent = R
   return z
end

Base.call(R::FlintPadicField, n::Integer) = R(fmpz(n))

function Base.call(R::FlintPadicField, n::padic)
   parent(n) != R && error("Unable to coerce into p-adic field")
   return n
end

###############################################################################
#
#   FlintPadicField constructor
#
###############################################################################

# inner constructor is also used directly

doc"""
    FlintPadicField(p::Integer, prec::Int)
> Returns the parent object for the $p$-adic field for given prime $p$, where
> the default absolute precision of elements of the field is given by `prec`.
"""
function FlintPadicField(p::Integer, prec::Int)
   return FlintPadicField(fmpz(p), prec)
end
