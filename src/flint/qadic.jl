###############################################################################
#
#   qadic.jl : flint qadic numbers
#
###############################################################################

export FlintQadicField, qadic, prime, teichmuller, log

###############################################################################
#
#   Data type and parent object methods
#
###############################################################################

@doc Markdown.doc"""
    O(R::FlintQadicField, m::fmpz)

Construct the value $0 + O(p^n)$ given $m = p^n$. An exception results if $m$
is not found to be a power of `p = prime(R)`.
"""
function O(R::FlintQadicField, m::fmpz)
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
   d = qadic(N)
   d.parent = R
   return d
end

@doc Markdown.doc"""
    O(R::FlintQadicField, m::fmpq)

Construct the value $0 + O(p^n)$ given $m = p^n$. An exception results if $m$
is not found to be a power of `p = prime(R)`.
"""
function O(R::FlintQadicField, m::fmpq)
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
   r = qadic(N)
   r.parent = R
   return r
end

@doc Markdown.doc"""
    O(R::FlintQadicField, m::Integer)

Construct the value $0 + O(p^n)$ given $m = p^n$. An exception results if $m$
is not found to be a power of `p = prime(R)`.
"""
O(R::FlintQadicField, m::Integer) = O(R, fmpz(m))

elem_type(::Type{FlintQadicField}) = qadic

@doc Markdown.doc"""
    base_ring(a::FlintQadicField)

Returns `Union{}` as this field is not dependent on another field.
"""
base_ring(a::FlintQadicField) = Union{}

@doc Markdown.doc"""
    base_ring(a::qadic)

Returns `Union{}` as this field is not dependent on another field.
"""
base_ring(a::qadic) = Union{}

@doc Markdown.doc"""
    parent(a::qadic)

Returns the parent of the given p-adic field element.
"""
parent(a::qadic) = a.parent

isdomain_type(::Type{qadic}) = true

isexact_type(R::Type{qadic}) = false

function check_parent(a::qadic, b::qadic)
   parent(a) != parent(b) &&
      error("Incompatible qadic rings in qadic operation")
end

parent_type(::Type{qadic}) = FlintQadicField

###############################################################################
#
#   Basic manipulation
#
###############################################################################

function Base.deepcopy_internal(a::qadic, dict::IdDict{Any, Any})
   z = parent(a)()
   ccall((:qadic_set, libflint), Nothing,
         (Ref{qadic}, Ref{qadic}, Ref{FlintQadicField}), z, a, parent(a))
   z.N = a.N
   return z
end

function Base.hash(a::qadic, h::UInt)
   return xor(hash(lift(FmpqPolyRing(FlintQQ, :x), a), h),
              xor(hash([prime(parent(a)),degree(parent(a))], h), h))
end

function degree(R::FlintQadicField)
   return ccall((:qadic_ctx_degree, libflint), Int, (Ref{FlintQadicField}, ), R)
end

@doc Markdown.doc"""
    prime(R::FlintQadicField)

Return the prime $p$ for the given $q$-adic field.
"""
function prime(R::FlintQadicField)
   z = fmpz()
   ccall((:padic_ctx_pow_ui, libflint), Nothing,
         (Ref{fmpz}, UInt, Ref{FlintQadicField}), z, 1, R)
   return z
end

@doc Markdown.doc"""
    precision(a::qadic)

Return the precision of the given $q$-adic field element, i.e. if the element
is known to $O(p^n)$ this function will return $n$.
"""
precision(a::qadic) = a.N

@doc Markdown.doc"""
    valuation(a::qadic)

Return the valuation of the given $q$-adic field element, i.e. if the given
element is divisible by $p^n$ but not a higher power of $q$ then the function
will return $n$.
"""
function valuation(a::qadic)
    iszero(a) ? precision(a) : ccall((:qadic_val, libflint), Int, (Ref{qadic}, ), a)
end

@doc Markdown.doc"""
    lift(R::FmpqPolyRing, a::qadic)

Return a lift of the given $q$-adic field element to $\mathbb{Q}[x]$.
"""
function lift(R::FmpqPolyRing, a::qadic)
   ctx = parent(a)
   r = R()
   ccall((:padic_poly_get_fmpq_poly, libflint), Nothing,
         (Ref{fmpq_poly}, Ref{qadic}, Ref{FlintQadicField}), r, a, ctx)
   return r
end

@doc Markdown.doc"""
    lift(R::FmpzPolyRing, a::qadic)

Return a lift of the given $q$-adic field element to $\mathbb{Z}[x]$ if possible.
"""
function lift(R::FmpzPolyRing, a::qadic)
   ctx = parent(a)
   r = R()
   res = Bool(ccall((:padic_poly_get_fmpz_poly, libflint), Cint,
                    (Ref{fmpz_poly}, Ref{qadic}, Ref{FlintQadicField}), r, a, ctx))
   !res && error("Unable to lift")
   return r
end

@doc Markdown.doc"""
    zero(R::FlintQadicField)

Return zero in the given $q$-adic field, to the default precision.
"""
function zero(R::FlintQadicField)
   z = qadic(R.prec_max)
   ccall((:qadic_zero, libflint), Nothing, (Ref{qadic},), z)
   z.parent = R
   return z
end

@doc Markdown.doc"""
    one(R::FlintQadicField)

Return zero in the given $q$-adic field, to the default precision.
"""
function one(R::FlintQadicField)
   z = qadic(R.prec_max)
   ccall((:qadic_one, libflint), Nothing, (Ref{qadic},), z)
   z.parent = R
   return z
end

@doc Markdown.doc"""
    iszero(a::qadic)

Return `true` if the given p-adic field element is zero, otherwise return
`false`.
"""
iszero(a::qadic) = Bool(ccall((:qadic_is_zero, libflint), Cint,
                              (Ref{qadic},), a))

@doc Markdown.doc"""
    isone(a::qadic)

Return `true` if the given p-adic field element is one, otherwise return
`false`.
"""
isone(a::qadic) = Bool(ccall((:qadic_is_one, libflint), Cint,
                             (Ref{qadic},), a))

@doc Markdown.doc"""
    isunit(a::qadic)

Return `true` if the given p-adic field element is invertible, i.e. nonzero,
otherwise return `false`.
"""
isunit(a::qadic) = !Bool(ccall((:qadic_is_zero, libflint), Cint,
                              (Ref{qadic},), a))

characteristic(R::FlintQadicField) = 0

###############################################################################
#
#   AbstractString I/O
#
###############################################################################

function expressify(b::qadic, x = :a; context = nothing)
   R = FlintPadicField(prime(parent(b)), parent(b).prec_max)
   if iszero(b)
      return 0
   end
   sum = Expr(:call, :+)
   c = R()
   for i in degree(parent(b)):-1:0
      ccall((:padic_poly_get_coeff_padic, libflint), Nothing,
            (Ref{padic}, Ref{qadic}, Int, Ref{FlintQadicField}),
            c, b, i, parent(b))
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

function show(io::IO, a::qadic)
   print(io, AbstractAlgebra.obj_to_string(a, context = io))
end

function show(io::IO, R::FlintQadicField)
   print(io, "Unramified extension of $(prime(R))-adic numbers of degree $(degree(R))")
end

###############################################################################
#
#   Canonicalisation
#
###############################################################################

canonical_unit(x::qadic) = x

###############################################################################
#
#   Unary operators
#
###############################################################################

function -(x::qadic)
   if iszero(x)
      return x
   end
   ctx = parent(x)
   z = qadic(x.N)
   ccall((:qadic_neg, libflint), Nothing,
         (Ref{qadic}, Ref{qadic}, Ref{FlintQadicField}),
                     z, x, ctx)
   z.parent = ctx
   return z
end

###############################################################################
#
#   Binary operators
#
###############################################################################

function +(x::qadic, y::qadic)
   check_parent(x, y)
   ctx = parent(x)
   z = qadic(min(x.N, y.N))
   z.parent = ctx
   ccall((:qadic_add, libflint), Nothing,
         (Ref{qadic}, Ref{qadic}, Ref{qadic}, Ref{FlintQadicField}),
               z, x, y, ctx)
   return z
end

function -(x::qadic, y::qadic)
   check_parent(x, y)
   ctx = parent(x)
   z = qadic(min(x.N, y.N))
   z.parent = ctx
   ccall((:qadic_sub, libflint), Nothing,
         (Ref{qadic}, Ref{qadic}, Ref{qadic}, Ref{FlintQadicField}),
                  z, x, y, ctx)
   return z
end

function *(x::qadic, y::qadic)
   check_parent(x, y)
   ctx = parent(x)
   z = qadic(min(x.N + valuation(y), y.N + valuation(x)))
   z.parent = ctx
   ccall((:qadic_mul, libflint), Nothing,
         (Ref{qadic}, Ref{qadic}, Ref{qadic}, Ref{FlintQadicField}),
               z, x, y, ctx)
   return z
end

###############################################################################
#
#   Ad hoc binary operators
#
###############################################################################

+(a::qadic, b::Integer) = a + parent(a)(b)

+(a::qadic, b::fmpz) = a + parent(a)(b)

+(a::qadic, b::fmpq) = a + parent(a)(b)

+(a::Integer, b::qadic) = b + a

+(a::fmpz, b::qadic) = b + a

+(a::fmpq, b::qadic) = b + a

-(a::qadic, b::Integer) = a - parent(a)(b)

-(a::qadic, b::fmpz) = a - parent(a)(b)

-(a::qadic, b::fmpq) = a - parent(a)(b)

-(a::Integer, b::qadic) = parent(b)(a) - b

-(a::fmpz, b::qadic) = parent(b)(a) - b

-(a::fmpq, b::qadic) = parent(b)(a) - b

*(a::qadic, b::Integer) = a*parent(a)(b)

*(a::qadic, b::fmpz) = a*parent(a)(b)

*(a::qadic, b::fmpq) = a*parent(a)(b)

*(a::Integer, b::qadic) = b*a

*(a::fmpz, b::qadic) = b*a

*(a::fmpq, b::qadic) = b*a

###############################################################################
#
#   Comparison
#
###############################################################################

function ==(a::qadic, b::qadic)
   check_parent(a, b)
   ctx = parent(a)
   z = qadic(min(a.N, b.N))
   ccall((:qadic_sub, libflint), Nothing,
         (Ref{qadic}, Ref{qadic}, Ref{qadic}, Ref{FlintQadicField}),
               z, a, b, ctx)
   return Bool(ccall((:qadic_is_zero, libflint), Cint,
                (Ref{qadic},), z))
end

function isequal(a::qadic, b::qadic)
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

==(a::qadic, b::Integer) = a == parent(a)(b)

==(a::qadic, b::fmpz) = a == parent(a)(b)

==(a::qadic, b::fmpq) = a == parent(a)(b)

==(a::Integer, b::qadic) = parent(b)(a) == b

==(a::fmpz, b::qadic) = parent(b)(a) == b

==(a::fmpq, b::qadic) = parent(b)(a) == b

###############################################################################
#
#   Powering
#
###############################################################################

^(q::qadic, n::Int) = q^fmpz(n)

function ^(a::qadic, n::fmpz)
   ctx = parent(a)
   if n < 0
      return inv(a)^(-n)
   end
   z = qadic(a.N + (Int(n) - 1)*valuation(a))
   z.parent = ctx
   ccall((:qadic_pow, libflint), Nothing,
                 (Ref{qadic}, Ref{qadic}, Ref{fmpz}, Ref{FlintQadicField}),
               z, a, n, ctx)
   return z
end

###############################################################################
#
#   Exact division
#
###############################################################################

function divexact(a::qadic, b::qadic)
   iszero(b) && throw(DivideError())
   return a * inv(b)
end

###############################################################################
#
#   Ad hoc exact division
#
###############################################################################

divexact(a::qadic, b::Integer) = a*(fmpz(1)//fmpz(b))

divexact(a::qadic, b::fmpz) = a*(1//b)

divexact(a::qadic, b::fmpq) = a*inv(b)

divexact(a::Integer, b::qadic) = fmpz(a)*inv(b)

divexact(a::fmpz, b::qadic) = inv((fmpz(1)//a)*b)

divexact(a::fmpq, b::qadic) = inv(inv(a)*b)

###############################################################################
#
#   Inversion
#
###############################################################################

@doc Markdown.doc"""
    inv(a::qadic)

Returns $a^{-1}$. If $a = 0$ a `DivideError()` is thrown.
"""
function inv(a::qadic)
   iszero(a) && throw(DivideError())
   ctx = parent(a)
   z = qadic(a.N - 2*valuation(a))
   z.parent = ctx
   ccall((:qadic_inv, libflint), Cint,
         (Ref{qadic}, Ref{qadic}, Ref{FlintQadicField}), z, a, ctx)
   return z
end

###############################################################################
#
#   Divides
#
###############################################################################

@doc Markdown.doc"""
    divides(f::qadic, g::qadic)

Returns a pair consisting of a flag which is set to `true` if $g$ divides
$f$ and `false` otherwise, and a value $h$ such that $f = gh$ if
such a value exists. If not, the value of $h$ is undetermined.
"""
function divides(a::qadic, b::qadic)
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

@doc Markdown.doc"""
    gcd(x::qadic, y::qadic)

Returns the greatest common divisor of $x$ and $y$, i.e. the function returns
$1$ unless both $a$ and $b$ are $0$, in which case it returns $0$.
"""
function gcd(x::qadic, y::qadic)
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

@doc Markdown.doc"""
    sqrt(a::qadic)

Return the $q$-adic square root of $a$. We define this only when the
valuation of $a$ is even. The precision of the output will be
precision$(a) -$ valuation$(a)/2$. If the square root does not exist, an
exception is thrown.
"""
function Base.sqrt(a::qadic)
   av = valuation(a)
   (av % 2) != 0 && error("Unable to take qadic square root")
   ctx = parent(a)
   z = qadic(a.N - div(av, 2))
   z.parent = ctx
   res = Bool(ccall((:qadic_sqrt, libflint), Cint,
                    (Ref{qadic}, Ref{qadic}, Ref{FlintQadicField}), z, a, ctx))
   !res && error("Square root of p-adic does not exist")
   return z
end

###############################################################################
#
#   Special functions
#
###############################################################################

@doc Markdown.doc"""
    exp(a::qadic)

Return the $q$-adic exponential of $a$. We define this only when the
valuation of $a$ is positive (unless $a = 0$). The precision of the output
will be the same as the precision of the input. If the input is not valid an
exception is thrown.
"""
function Base.exp(a::qadic)
   !iszero(a) && valuation(a) <= 0 && throw(DomainError(a, "Valuation must be positive"))
   ctx = parent(a)
   z = qadic(a.N)
   z.parent = ctx
   res = Bool(ccall((:qadic_exp, libflint), Cint,
                    (Ref{qadic}, Ref{qadic}, Ref{FlintQadicField}), z, a, ctx))
   !res && error("Unable to compute exponential")
   return z
end

@doc Markdown.doc"""
    log(a::qadic)

Return the $q$-adic logarithm of $a$. We define this only when the valuation
of $a$ is zero (but not for $a == 0$). The precision of the output will be
the same as the precision of the input. If the input is not valid an
exception is thrown.
"""
function log(a::qadic)
   av = valuation(a)
   (av > 0 || av < 0 || iszero(a)) && throw(DomainError(a, "Valuation must be zero"))
   ctx = parent(a)
   z = qadic(a.N)
   z.parent = ctx
   res = Bool(ccall((:qadic_log, libflint), Cint,
                    (Ref{qadic}, Ref{qadic}, Ref{FlintQadicField}), z, a, ctx))
   !res && error("Unable to compute logarithm")
   return z
end

@doc Markdown.doc"""
    teichmuller(a::qadic)

Return the Teichmuller lift of the $q$-adic value $a$. We require the
valuation of $a$ to be nonnegative. The precision of the output will be the
same as the precision of the input. For convenience, if $a$ is congruent to
zero modulo $q$ we return zero. If the input is not valid an exception is
thrown.
"""
function teichmuller(a::qadic)
   valuation(a) < 0 && throw(DomainError(a, "Valuation must be non-negative"))
   ctx = parent(a)
   z = qadic(a.N)
   z.parent = ctx
   ccall((:qadic_teichmuller, libflint), Nothing,
         (Ref{qadic}, Ref{qadic}, Ref{FlintQadicField}), z, a, ctx)
   return z
end

@doc Markdown.doc"""
    frobenius(a::qadic, e::Int = 1)

Return the image of the $e$-th power of Frobenius on the $q$-adic value $a$.
The precision of the output will be the same as the precision of the input.
"""
function frobenius(a::qadic, e::Int = 1)
   ctx = parent(a)
   z = qadic(a.N)
   z.parent = ctx
   ccall((:qadic_frobenius, libflint), Nothing,
         (Ref{qadic}, Ref{qadic}, Int, Ref{FlintQadicField}), z, a, e, ctx)
   return z
end

###############################################################################
#
#   Unsafe operators
#
###############################################################################

function zero!(z::qadic)
   z.N = parent(z).prec_max
   ctx = parent(z)
   ccall((:qadic_zero, libflint), Nothing,
         (Ref{qadic}, Ref{FlintQadicField}), z, ctx)
   return z
end

function mul!(z::qadic, x::qadic, y::qadic)
   z.N = min(x.N + valuation(y), y.N + valuation(x))
   ctx = parent(x)
   ccall((:qadic_mul, libflint), Nothing,
         (Ref{qadic}, Ref{qadic}, Ref{qadic}, Ref{FlintQadicField}),
               z, x, y, ctx)
   return z
end

function addeq!(x::qadic, y::qadic)
   x.N = min(x.N, y.N)
   ctx = parent(x)
   ccall((:qadic_add, libflint), Nothing,
         (Ref{qadic}, Ref{qadic}, Ref{qadic}, Ref{FlintQadicField}),
               x, x, y, ctx)
   return x
end

function add!(z::qadic, x::qadic, y::qadic)
   z.N = min(x.N, y.N)
   ctx = parent(x)
   ccall((:qadic_add, libflint), Nothing,
         (Ref{qadic}, Ref{qadic}, Ref{qadic}, Ref{FlintQadicField}),
               z, x, y, ctx)
   return z
end

###############################################################################
#
#   Conversions and promotions
#
###############################################################################

promote_rule(::Type{qadic}, ::Type{T}) where {T <: Integer} = qadic

promote_rule(::Type{qadic}, ::Type{Rational{V}}) where {V <: Integer} = qadic

promote_rule(::Type{qadic}, ::Type{fmpz}) = qadic

promote_rule(::Type{qadic}, ::Type{fmpq}) = qadic

promote_rule(::Type{qadic}, ::Type{padic}) = qadic

###############################################################################
#
#   Parent object overloads
#
###############################################################################

function (R::FlintQadicField)()
   z = qadic(R.prec_max)
   z.parent = R
   return z
end

function gen(R::FlintQadicField)
   z = qadic(R.prec_max)
   ccall((:qadic_gen, libflint), Nothing,
         (Ref{qadic}, Ref{FlintQadicField}), z, R)
   z.parent = R
   return z
end

function (R::FlintQadicField)(a::UInt)
   if a == 0
     z = qadic(R.prec_max)
     z.parent = R
     return z
   end
   v = valuation(a, prime(R))
   z = qadic(R.prec_max + v)
   ccall((:qadic_set_ui, libflint), Nothing,
         (Ref{qadic}, UInt, Ref{FlintQadicField}), z, a, R)
   z.parent = R
   return z
end

function (R::FlintQadicField)(a::Int)
   if a == 0
     z = qadic(R.prec_max)
     z.parent = R
     return z
   end
   v = valuation(a, prime(R))
   z = qadic(R.prec_max + v)
   ccall((:padic_poly_set_si, libflint), Nothing,
         (Ref{qadic}, Int, Ref{FlintQadicField}), z,a, R)
   z.parent = R
   return z
end

function (R::FlintQadicField)(n::fmpz)
   if isone(n)
      N = 0
   else
      p = prime(R)
      N = valuation(n, p)
   end
   z = qadic(N + R.prec_max)
   ccall((:padic_poly_set_fmpz, libflint), Nothing,
         (Ref{qadic}, Ref{fmpz}, Ref{FlintQadicField}), z, n, R)
   z.parent = R
   return z
end

function (R::FlintQadicField)(n::fmpq)
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
   z = qadic(N + R.prec_max)
   ccall((:padic_poly_set_fmpq, libflint), Nothing,
         (Ref{qadic}, Ref{fmpq}, Ref{FlintQadicField}), z, n, R)
   z.parent = R
   return z
end

function (R::FlintQadicField)(n::fmpz_poly)
   z = qadic(R.prec_max)
   ccall((:padic_poly_set_fmpz_poly, libflint), Nothing,
         (Ref{qadic}, Ref{fmpz_poly}, Ref{FlintQadicField}), z, n, R)
   z.parent = R
   return z
end

function (R::FlintQadicField)(n::fmpq_poly)

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
   z = qadic(N + R.prec_max)
   ccall((:padic_poly_set_fmpq_poly, libflint), Nothing,
         (Ref{qadic}, Ref{fmpq_poly}, Ref{FlintQadicField}), z, n, R)
   z.parent = R
   return z
end

function (R::FlintQadicField)(b::Rational{<:Integer})
   return R(fmpq(b))
end

(R::FlintQadicField)(n::Integer) = R(fmpz(n))

function (R::FlintQadicField)(n::qadic)
   parent(n) != R && error("Unable to coerce into q-adic field")
   return n
end

###############################################################################
#
#   FlintQadicField constructor
#
###############################################################################

# inner constructor is also used directly

@doc Markdown.doc"""
    FlintQadicField(p::Integer, d::Int, prec::Int)

Returns the parent object for the $q$-adic field for given prime $p$ and
degree $d$, where the default absolute precision of elements of the field
is given by `prec`.
"""
function FlintQadicField(p::Integer, d::Int, prec::Int)
   return FlintQadicField(fmpz(p), d, prec)
end
