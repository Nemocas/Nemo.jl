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

@doc raw"""
    O(R::FlintQadicField, m::ZZRingElem)

Construct the value $0 + O(p^n)$ given $m = p^n$. An exception results if $m$
is not found to be a power of `p = prime(R)`.
"""
function O(R::FlintQadicField, m::ZZRingElem)
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

@doc raw"""
    O(R::FlintQadicField, m::QQFieldElem)

Construct the value $0 + O(p^n)$ given $m = p^n$. An exception results if $m$
is not found to be a power of `p = prime(R)`.
"""
function O(R::FlintQadicField, m::QQFieldElem)
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

@doc raw"""
    O(R::FlintQadicField, m::Integer)

Construct the value $0 + O(p^n)$ given $m = p^n$. An exception results if $m$
is not found to be a power of `p = prime(R)`.
"""
O(R::FlintQadicField, m::Integer) = O(R, ZZRingElem(m))

elem_type(::Type{FlintQadicField}) = qadic

@doc raw"""
    base_ring(a::FlintQadicField)

Returns `Union{}` as this field is not dependent on another field.
"""
base_ring(a::FlintQadicField) = Union{}

@doc raw"""
    base_ring(a::qadic)

Returns `Union{}` as this field is not dependent on another field.
"""
base_ring(a::qadic) = Union{}

parent(a::qadic) = a.parent

is_domain_type(::Type{qadic}) = true

is_exact_type(R::Type{qadic}) = false

function check_parent(a::qadic, b::qadic)
   parent(a) != parent(b) &&
      error("Incompatible qadic rings in qadic operation")
end

parent_type(::Type{qadic}) = FlintQadicField

function _prime(R::FlintQadicField, n::Int = 1)
   z = ZZRingElem()
   ccall((:padic_ctx_pow_ui, libflint), Nothing,
         (Ref{ZZRingElem}, UInt, Ref{FlintQadicField}), z, n, R)
   return z
end

###############################################################################
#
#   Basic manipulation
#
###############################################################################

function Base.deepcopy_internal(a::qadic, dict::IdDict{Any, Any})
   z = parent(a)()
   z.N = a.N
   ccall((:qadic_set, libflint), Nothing,
         (Ref{qadic}, Ref{qadic}, Ref{FlintQadicField}), z, a, parent(a))
   return z
end

function Base.hash(a::qadic, h::UInt)
   return xor(hash(lift(QQPolyRing(FlintQQ, :x), a), h),
              xor(hash([prime(parent(a)),degree(parent(a))], h), h))
end

function degree(R::FlintQadicField)
   return ccall((:qadic_ctx_degree, libflint), Int, (Ref{FlintQadicField}, ), R)
end

@doc raw"""
    prime(R::FlintQadicField)

Return the prime $p$ for the given $q$-adic field.
"""
function prime(R::FlintQadicField)
   z = ZZRingElem()
   ccall((:padic_ctx_pow_ui, libflint), Nothing,
         (Ref{ZZRingElem}, UInt, Ref{FlintQadicField}), z, 1, R)
   return z
end

@doc raw"""
    precision(a::qadic)

Return the precision of the given $q$-adic field element, i.e. if the element
is known to $O(p^n)$ this function will return $n$.
"""
precision(a::qadic) = a.N

@doc raw"""
    valuation(a::qadic)

Return the valuation of the given $q$-adic field element, i.e. if the given
element is divisible by $p^n$ but not a higher power of $q$ then the function
will return $n$.
"""
function valuation(a::qadic)
    iszero(a) ? precision(a) : ccall((:qadic_val, libflint), Int, (Ref{qadic}, ), a)
end

@doc raw"""
    lift(R::QQPolyRing, a::qadic)

Return a lift of the given $q$-adic field element to $\mathbb{Q}[x]$.
"""
function lift(R::QQPolyRing, a::qadic)
   ctx = parent(a)
   r = R()
   ccall((:padic_poly_get_fmpq_poly, libflint), Nothing,
         (Ref{QQPolyRingElem}, Ref{qadic}, Ref{FlintQadicField}), r, a, ctx)
   return r
end

@doc raw"""
    lift(R::ZZPolyRing, a::qadic)

Return a lift of the given $q$-adic field element to $\mathbb{Z}[x]$ if possible.
"""
function lift(R::ZZPolyRing, a::qadic)
   ctx = parent(a)
   r = R()
   res = Bool(ccall((:padic_poly_get_fmpz_poly, libflint), Cint,
                    (Ref{ZZPolyRingElem}, Ref{qadic}, Ref{FlintQadicField}), r, a, ctx))
   !res && error("Unable to lift")
   return r
end

function zero(R::FlintQadicField)
   z = qadic(R.prec_max)
   ccall((:qadic_zero, libflint), Nothing, (Ref{qadic},), z)
   z.parent = R
   return z
end

function one(R::FlintQadicField)
   z = qadic(R.prec_max)
   ccall((:qadic_one, libflint), Nothing, (Ref{qadic},), z)
   z.parent = R
   return z
end

iszero(a::qadic) = Bool(ccall((:qadic_is_zero, libflint), Cint,
                              (Ref{qadic},), a))

isone(a::qadic) = Bool(ccall((:qadic_is_one, libflint), Cint,
                             (Ref{qadic},), a))

is_unit(a::qadic) = !Bool(ccall((:qadic_is_zero, libflint), Cint,
                              (Ref{qadic},), a))

characteristic(R::FlintQadicField) = 0

###############################################################################
#
#   AbstractString I/O
#
###############################################################################

function var(Q::FlintQadicField)
  return Symbol(unsafe_string(Q.var))
end

function expressify(b::qadic, x = var(parent(b)); context = nothing)
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

+(a::qadic, b::ZZRingElem) = a + parent(a)(b)

+(a::qadic, b::QQFieldElem) = a + parent(a)(b)

+(a::Integer, b::qadic) = b + a

+(a::ZZRingElem, b::qadic) = b + a

+(a::QQFieldElem, b::qadic) = b + a

-(a::qadic, b::Integer) = a - parent(a)(b)

-(a::qadic, b::ZZRingElem) = a - parent(a)(b)

-(a::qadic, b::QQFieldElem) = a - parent(a)(b)

-(a::Integer, b::qadic) = parent(b)(a) - b

-(a::ZZRingElem, b::qadic) = parent(b)(a) - b

-(a::QQFieldElem, b::qadic) = parent(b)(a) - b

*(a::qadic, b::Integer) = a*parent(a)(b)

*(a::qadic, b::ZZRingElem) = a*parent(a)(b)

*(a::qadic, b::QQFieldElem) = a*parent(a)(b)

*(a::Integer, b::qadic) = b*a

*(a::ZZRingElem, b::qadic) = b*a

*(a::QQFieldElem, b::qadic) = b*a

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

==(a::qadic, b::ZZRingElem) = a == parent(a)(b)

==(a::qadic, b::QQFieldElem) = a == parent(a)(b)

==(a::Integer, b::qadic) = parent(b)(a) == b

==(a::ZZRingElem, b::qadic) = parent(b)(a) == b

==(a::QQFieldElem, b::qadic) = parent(b)(a) == b

###############################################################################
#
#   Powering
#
###############################################################################

^(q::qadic, n::Int) = q^ZZRingElem(n)

function ^(a::qadic, n::ZZRingElem)
   ctx = parent(a)
   if n < 0
      return inv(a)^(-n)
   end
   if valuation(a) == 0
     z = qadic(a.N) #if expo is ZZRingElem, Int(n) would throw an error
   else             #for units (v==0) this is fine hower.
     z = qadic(a.N + (Int(n) - 1)*valuation(a))
   end
   z.parent = ctx
   ccall((:qadic_pow, libflint), Nothing,
                 (Ref{qadic}, Ref{qadic}, Ref{ZZRingElem}, Ref{FlintQadicField}),
               z, a, n, ctx)
   return z
end

###############################################################################
#
#   Exact division
#
###############################################################################

function divexact(a::qadic, b::qadic; check::Bool=true)
   iszero(b) && throw(DivideError())
   return a * inv(b)
end

###############################################################################
#
#   Ad hoc exact division
#
###############################################################################

divexact(a::qadic, b::Integer; check::Bool=true) = a*(ZZRingElem(1)//ZZRingElem(b))

divexact(a::qadic, b::ZZRingElem; check::Bool=true) = a*(1//b)

divexact(a::qadic, b::QQFieldElem; check::Bool=true) = a*inv(b)

divexact(a::Integer, b::qadic; check::Bool=true) = ZZRingElem(a)*inv(b)

divexact(a::ZZRingElem, b::qadic; check::Bool=true) = inv((ZZRingElem(1)//a)*b)

divexact(a::QQFieldElem, b::qadic; check::Bool=true) = inv(inv(a)*b)

###############################################################################
#
#   Inversion
#
###############################################################################

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

function Base.sqrt(a::qadic; check::Bool=true)
   av = valuation(a)
   check && (av % 2) != 0 && error("Unable to take qadic square root")
   ctx = parent(a)
   z = qadic(a.N - div(av, 2))
   z.parent = ctx
   res = Bool(ccall((:qadic_sqrt, libflint), Cint,
                    (Ref{qadic}, Ref{qadic}, Ref{FlintQadicField}), z, a, ctx))
   check && !res && error("Square root of p-adic does not exist")
   return z
end

###############################################################################
#
#   Special functions
#
###############################################################################

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

function log(a::qadic)
   av = valuation(a)
   (av > 0 || av < 0 || iszero(a)) && throw(DomainError(a, "Valuation must be zero"))
   av = valuation(a-1)
   ctx = parent(a)
   if av == 0
     qm1 = _prime(ctx, degree(ctx)) - 1
     a = a^qm1
   end

   ctx = parent(a)
   z = qadic(a.N)
   z.parent = ctx
   res = Bool(ccall((:qadic_log, libflint), Cint,
                    (Ref{qadic}, Ref{qadic}, Ref{FlintQadicField}), z, a, ctx))
   !res && error("Unable to compute logarithm")
   if av == 0
     z = divexact(z, qm1)
   end
   return z
end

@doc raw"""
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

@doc raw"""
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

promote_rule(::Type{qadic}, ::Type{ZZRingElem}) = qadic

promote_rule(::Type{qadic}, ::Type{QQFieldElem}) = qadic

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
   if degree(R) == 1
      # Work around flint limitation
      # https://github.com/wbhart/flint2/issues/898
      a = ZZRingElem()
      GC.@preserve R begin
         ccall((:fmpz_set, libflint), Nothing, (Ref{ZZRingElem}, Ptr{ZZRingElem}),
                                               a, reinterpret(Ptr{ZZRingElem}, R.a))
      end
      return R(-a)
   end

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

function (R::FlintQadicField)(n::ZZRingElem)
   if iszero(n) || isone(n)
      N = 0
   else
      p = prime(R)
      N = valuation(n, p)
   end
   z = qadic(N + R.prec_max)
   ccall((:padic_poly_set_fmpz, libflint), Nothing,
         (Ref{qadic}, Ref{ZZRingElem}, Ref{FlintQadicField}), z, n, R)
   z.parent = R
   return z
end

function (R::FlintQadicField)(n::QQFieldElem)
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
         (Ref{qadic}, Ref{QQFieldElem}, Ref{FlintQadicField}), z, n, R)
   z.parent = R
   return z
end

function (R::FlintQadicField)(n::ZZPolyRingElem, pr::Int = R.prec_max)
   z = qadic(pr)
   ccall((:qadic_set_fmpz_poly, libflint), Nothing,
         (Ref{qadic}, Ref{ZZPolyRingElem}, Ref{FlintQadicField}), z, n, R)
   z.parent = R
   return z
end

function (R::FlintQadicField)(n::QQPolyRingElem)

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
         (Ref{qadic}, Ref{QQPolyRingElem}, Ref{FlintQadicField}), z, n, R)
   z.parent = R
   return z
end

function (R::FlintQadicField)(b::Rational{<:Integer})
   return R(QQFieldElem(b))
end

(R::FlintQadicField)(n::Integer) = R(ZZRingElem(n))

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

@doc raw"""
    FlintQadicField(p::Integer, d::Int, prec::Int, var::String = "a")

Returns the parent object for the $q$-adic field for given prime $p$ and
degree $d$, where the default absolute precision of elements of the field
is given by `prec` and the generator is printed as `var`.
"""
function FlintQadicField(p::Integer, d::Int, prec::Int, var::String = "a"; cached::Bool = true)
   return FlintQadicField(ZZRingElem(p), d, prec, var, cached = cached)
end
