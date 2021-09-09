###############################################################################
#
#   padic.jl : flint padic numbers
#
###############################################################################

export FlintPadicField, padic, prime, teichmuller, log, get_printing_mode,
       set_printing_mode

###############################################################################
#
#   Data type and parent object methods
#
###############################################################################

@doc Markdown.doc"""
    O(R::FlintPadicField, m::fmpz)

Construct the value $0 + O(p^n)$ given $m = p^n$. An exception results if $m$
is not found to be a power of `p = prime(R)`.
"""
function O(R::FlintPadicField, m::fmpz)
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
   d = padic(N)
   d.parent = R
   return d
end

@doc Markdown.doc"""
    O(R::FlintPadicField, m::fmpq)

Construct the value $0 + O(p^n)$ given $m = p^n$. An exception results if $m$
is not found to be a power of `p = prime(R)`.
"""
function O(R::FlintPadicField, m::fmpq)
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
   r = padic(N)
   r.parent = R
   return r
end

@doc Markdown.doc"""
    O(R::FlintPadicField, m::Integer)

Construct the value $0 + O(p^n)$ given $m = p^n$. An exception results if $m$
is not found to be a power of `p = prime(R)`.
"""
O(R::FlintPadicField, m::Integer) = O(R, fmpz(m))

elem_type(::Type{FlintPadicField}) = padic

base_ring(a::FlintPadicField) = Union{}

base_ring(a::padic) = Union{}

parent(a::padic) = a.parent

isdomain_type(::Type{padic}) = true

isexact_type(R::Type{padic}) = false

function check_parent(a::padic, b::padic)
   parent(a) != parent(b) &&
      error("Incompatible padic rings in padic operation")
end

parent_type(::Type{padic}) = FlintPadicField

###############################################################################
#
#   Basic manipulation
#
###############################################################################

function Base.deepcopy_internal(a::padic, dict::IdDict)
   z = parent(a)()
   ccall((:padic_set, libflint), Nothing,
         (Ref{padic}, Ref{padic}, Ref{FlintPadicField}), z, a, parent(a))
   z.N = a.N      # set does not transfer N - neither should it.
   return z
end

function Base.hash(a::padic, h::UInt)
   return xor(hash(lift(FlintQQ, a), h), xor(hash(prime(parent(a)), h), h))
end

@doc Markdown.doc"""
    prime(R::FlintPadicField)

Return the prime $p$ for the given $p$-adic field.
"""
function prime(R::FlintPadicField)
   z = fmpz()
   ccall((:padic_ctx_pow_ui, libflint), Nothing,
         (Ref{fmpz}, Int, Ref{FlintPadicField}), z, 1, R)
   return z
end

@doc Markdown.doc"""
    precision(a::padic)

Return the precision of the given $p$-adic field element, i.e. if the element
is known to $O(p^n)$ this function will return $n$.
"""
precision(a::padic) = a.N

@doc Markdown.doc"""
    valuation(a::padic)

Return the valuation of the given $p$-adic field element, i.e. if the given
element is divisible by $p^n$ but not a higher power of $p$ then the function
will return $n$.
"""
valuation(a::padic) = iszero(a) ? a.N : a.v

@doc Markdown.doc"""
    lift(R::FlintRationalField, a::padic)

Return a lift of the given $p$-adic field element to $\mathbb{Q}$.
"""
function lift(R::FlintRationalField, a::padic)
    ctx = parent(a)
    r = fmpq()
    ccall((:padic_get_fmpq, libflint), Nothing,
          (Ref{fmpq}, Ref{padic}, Ref{FlintPadicField}), r, a, ctx)
    return r
end

@doc Markdown.doc"""
    lift(R::FlintIntegerRing, a::padic)

Return a lift of the given $p$-adic field element to $\mathbb{Z}$.
"""
function lift(R::FlintIntegerRing, a::padic)
    ctx = parent(a)
    r = fmpz()
    ccall((:padic_get_fmpz, libflint), Nothing,
          (Ref{fmpz}, Ref{padic}, Ref{FlintPadicField}), r, a, ctx)
    return r
end

function zero(R::FlintPadicField)
   z = padic(R.prec_max)
   ccall((:padic_zero, libflint), Nothing, (Ref{padic},), z)
   z.parent = R
   return z
end

function one(R::FlintPadicField)
   z = padic(R.prec_max)
   ccall((:padic_one, libflint), Nothing, (Ref{padic},), z)
   z.parent = R
   return z
end

iszero(a::padic) = Bool(ccall((:padic_is_zero, libflint), Cint,
                              (Ref{padic},), a))

isone(a::padic) = Bool(ccall((:padic_is_one, libflint), Cint,
                             (Ref{padic},), a))

isunit(a::padic) = !Bool(ccall((:padic_is_zero, libflint), Cint,
                              (Ref{padic},), a))

characteristic(R::FlintPadicField) = 0

###############################################################################
#
#   String I/O
#
###############################################################################

const PADIC_PRINTING_MODE = Ref(Cint(1))

@doc Markdown.doc"""
    get_printing_mode(::Type{FlintPadicField})

Get the printing mode for the elements of the p-adic field `R`.
"""
function get_printing_mode(::Type{FlintPadicField})
   return flint_padic_printing_mode[PADIC_PRINTING_MODE[] + 1]
end

@doc Markdown.doc"""
    set_printing_mode(::Type{FlintPadicField}, printing::Symbol)

Set the printing mode for the elements of the p-adic field `R`. Possible values
are `:terse`, `:series` and `:val_unit`.
"""
function set_printing_mode(::Type{FlintPadicField}, printing::Symbol)
   if printing == :terse
      PADIC_PRINTING_MODE[] = 0
   elseif printing == :series
      PADIC_PRINTING_MODE[] = 1
   elseif printing == :val_unit
      PADIC_PRINTING_MODE[] = 2
   else
      error("Invalid printing mode: $printing")
   end
   return printing
end

function expressify(x::padic; context = nothing)
   p = BigInt(prime(parent(x)))
   pmode = PADIC_PRINTING_MODE[]
   sum = Expr(:call, :+)
   if iszero(x)
      push!(sum.args, 0)
   elseif pmode == 0  # terse
      push!(sum.args, expressify(lift(FlintQQ, x), context = context))
   else
      pp = prime(parent(x))
      p = BigInt(pp)
      v = valuation(x)
      u = BigInt(lift(FlintZZ, x*parent(x)(pp)^-v))
      if pmode == 1  # series
         d = digits(u, base=p)
      else  # val_unit
         d = [u]
      end
      for i in 0:length(d)-1
         ppower = Expr(:call, :^, p, i + v)
         push!(sum.args, Expr(:call, :*, d[i + 1], ppower))
      end
   end
   push!(sum.args, Expr(:call, :O, Expr(:call, :^, p, x.N)))
   return sum
end

function show(io::IO, a::padic)
   print(io, AbstractAlgebra.obj_to_string(a, context = io))
end

function show(io::IO, R::FlintPadicField)
   print(io, "Field of ", prime(R), "-adic numbers")
end

needs_parentheses(x::padic) = true

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
   ccall((:padic_neg, libflint), Nothing,
         (Ref{padic}, Ref{padic}, Ref{FlintPadicField}),
                     z, x, ctx)
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
   ccall((:padic_add, libflint), Nothing,
         (Ref{padic}, Ref{padic}, Ref{padic}, Ref{FlintPadicField}),
               z, x, y, ctx)
   return z
end

function -(x::padic, y::padic)
   check_parent(x, y)
   ctx = parent(x)
   z = padic(min(x.N, y.N))
   z.parent = ctx
   ccall((:padic_sub, libflint), Nothing,
         (Ref{padic}, Ref{padic}, Ref{padic}, Ref{FlintPadicField}),
                  z, x, y, ctx)
   return z
end

function *(x::padic, y::padic)
   check_parent(x, y)
   ctx = parent(x)
   z = padic(min(x.N + y.v, y.N + x.v))
   z.parent = ctx
   ccall((:padic_mul, libflint), Nothing,
         (Ref{padic}, Ref{padic}, Ref{padic}, Ref{FlintPadicField}),
               z, x, y, ctx)
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
   ccall((:padic_sub, libflint), Nothing,
         (Ref{padic}, Ref{padic}, Ref{padic}, Ref{FlintPadicField}),
               z, a, b, ctx)
   return Bool(ccall((:padic_is_zero, libflint), Cint,
                (Ref{padic},), z))
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
   ccall((:padic_pow_si, libflint), Nothing,
                (Ref{padic}, Ref{padic}, Int, Ref{FlintPadicField}),
               z, a, n, ctx)
   return z
end

###############################################################################
#
#   Exact division
#
###############################################################################

function divexact(a::padic, b::padic; check::Bool=true)
   iszero(b) && throw(DivideError())
   check_parent(a, b)
   ctx = parent(a)
   z = padic(min(a.N - b.v, b.N - 2*b.v + a.v))
   z.parent = ctx
   ccall((:padic_div, libflint), Cint,
         (Ref{padic}, Ref{padic}, Ref{padic}, Ref{FlintPadicField}),
               z, a, b, ctx)
   return z
end

###############################################################################
#
#   Ad hoc exact division
#
###############################################################################

divexact(a::padic, b::Integer; check::Bool=true) = a*(fmpz(1)//fmpz(b))

divexact(a::padic, b::fmpz; check::Bool=true) = a*(1//b)

divexact(a::padic, b::fmpq; check::Bool=true) = a*inv(b)

divexact(a::Integer, b::padic; check::Bool=true) = fmpz(a)*inv(b)

divexact(a::fmpz, b::padic; check::Bool=true) = inv((fmpz(1)//a)*b)

divexact(a::fmpq, b::padic; check::Bool=true) = inv(inv(a)*b)

###############################################################################
#
#   Inversion
#
###############################################################################

function inv(a::padic)
   iszero(a) && throw(DivideError())
   ctx = parent(a)
   z = padic(a.N - 2*a.v)
   z.parent = ctx
   ccall((:padic_inv, libflint), Cint,
         (Ref{padic}, Ref{padic}, Ref{FlintPadicField}), z, a, ctx)
   return z
end

###############################################################################
#
#   Divides
#
###############################################################################

function divides(a::padic, b::padic)
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

function gcd(x::padic, y::padic)
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

function Base.sqrt(a::padic; check::Bool=true)
   check && (a.v % 2) != 0 && error("Unable to take padic square root")
   ctx = parent(a)
   z = padic(a.N - div(a.v, 2))
   z.parent = ctx
   res = Bool(ccall((:padic_sqrt, libflint), Cint,
                    (Ref{padic}, Ref{padic}, Ref{FlintPadicField}), z, a, ctx))
   check && !res && error("Square root of p-adic does not exist")
   return z
end

###############################################################################
#
#   Special functions
#
###############################################################################

function Base.exp(a::padic)
   !iszero(a) && a.v <= 0 && throw(DomainError(a, "Valuation must be positive"))
   ctx = parent(a)
   z = padic(a.N)
   z.parent = ctx
   res = Bool(ccall((:padic_exp, libflint), Cint,
                    (Ref{padic}, Ref{padic}, Ref{FlintPadicField}), z, a, ctx))
   !res && error("Unable to compute exponential")
   return z
end

function log(a::padic)
   ctx = parent(a)
   z = padic(a.N)
   z.parent = ctx
   res = Bool(ccall((:padic_log, libflint), Cint,
                    (Ref{padic}, Ref{padic}, Ref{FlintPadicField}), z, a, ctx))
   !res && error("Unable to compute logarithm")
   return z
end

@doc Markdown.doc"""
    teichmuller(a::padic)

Return the Teichmuller lift of the $p$-adic value $a$. We require the
valuation of $a$ to be nonnegative. The precision of the output will be the
same as the precision of the input. For convenience, if $a$ is congruent to
zero modulo $p$ we return zero. If the input is not valid an exception is
thrown.
"""
function teichmuller(a::padic)
   a.v < 0 && throw(DomainError(a.v, "Valuation must be non-negative"))
   ctx = parent(a)
   z = padic(a.N)
   z.parent = ctx
   ccall((:padic_teichmuller, libflint), Nothing,
         (Ref{padic}, Ref{padic}, Ref{FlintPadicField}), z, a, ctx)
   return z
end

###############################################################################
#
#   Unsafe operators
#
###############################################################################

function zero!(z::padic)
   z.N = parent(z).prec_max
   ctx = parent(z)
   ccall((:padic_zero, libflint), Nothing,
         (Ref{padic}, Ref{FlintPadicField}), z, ctx)
   return z
end

function mul!(z::padic, x::padic, y::padic)
   z.N = min(x.N + y.v, y.N + x.v)
   ctx = parent(x)
   ccall((:padic_mul, libflint), Nothing,
         (Ref{padic}, Ref{padic}, Ref{padic}, Ref{FlintPadicField}),
               z, x, y, ctx)
   return z
end

function addeq!(x::padic, y::padic)
   x.N = min(x.N, y.N)
   ctx = parent(x)
   ccall((:padic_add, libflint), Nothing,
         (Ref{padic}, Ref{padic}, Ref{padic}, Ref{FlintPadicField}),
               x, x, y, ctx)
   return x
end

function add!(z::padic, x::padic, y::padic)
   z.N = min(x.N, y.N)
   ctx = parent(x)
   ccall((:padic_add, libflint), Nothing,
         (Ref{padic}, Ref{padic}, Ref{padic}, Ref{FlintPadicField}),
               z, x, y, ctx)
   return z
end

###############################################################################
#
#   Conversions and promotions
#
###############################################################################

promote_rule(::Type{padic}, ::Type{T}) where {T <: Integer} = padic

promote_rule(::Type{padic}, ::Type{fmpz}) = padic

promote_rule(::Type{padic}, ::Type{fmpq}) = padic

###############################################################################
#
#   Parent object overloads
#
###############################################################################

function (R::FlintPadicField)()
   z = padic(R.prec_max)
   z.parent = R
   return z
end

function (R::FlintPadicField)(n::fmpz)
   if isone(n)
      N = 0
   else
      p = prime(R)
      N, = remove(n, p)
   end
   z = padic(N + R.prec_max)
   ccall((:padic_set_fmpz, libflint), Nothing,
         (Ref{padic}, Ref{fmpz}, Ref{FlintPadicField}), z, n, R)
   z.parent = R
   return z
end

function (R::FlintPadicField)(n::fmpq)
   m = denominator(n)
   if isone(m)
      return R(numerator(n))
   end
   p = prime(R)
   if m == p
      N = -1
   else
     N = -remove(m, p)[1]
   end
   z = padic(N + R.prec_max)
   ccall((:padic_set_fmpq, libflint), Nothing,
         (Ref{padic}, Ref{fmpq}, Ref{FlintPadicField}), z, n, R)
   z.parent = R
   return z
end

(R::FlintPadicField)(n::Integer) = R(fmpz(n))

function (R::FlintPadicField)(n::padic)
   parent(n) != R && error("Unable to coerce into p-adic field")
   return n
end

###############################################################################
#
#   FlintPadicField constructor
#
###############################################################################

# inner constructor is also used directly

@doc Markdown.doc"""
    FlintPadicField(p::Integer, prec::Int; kw...)

Returns the parent object for the $p$-adic field for given prime $p$, where
the default absolute precision of elements of the field is given by `prec`.
"""
function FlintPadicField(p::Integer, prec::Int; kw...)
   return FlintPadicField(fmpz(p), prec; kw...)
end
