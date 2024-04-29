###############################################################################
#
#   flint_puiseux_series.jl : Puiseux series over Flint rings and fields
#
###############################################################################

###############################################################################
#
#   Data type and parent object methods
#
###############################################################################

laurent_ring(R::FlintPuiseuxSeriesRing{T}) where T <: RingElem = R.laurent_ring::parent_type(T)

laurent_ring(R::FlintPuiseuxSeriesField{T}) where T <: FieldElem = R.laurent_ring::parent_type(T)

@doc raw"""
    O(a::FlintPuiseuxSeriesElem{T}) where T <: RingElem

Returns $0 + O(x^\mathrm{val}(a))$. Usually this function is called with $x^n$
as parameter for some rational $n$. Then the function returns the Puiseux series
$0 + O(x^n)$, which can be used to set the precision of a Puiseux series when
constructing it.
"""
function O(a::FlintPuiseuxSeriesElem{T}) where T <: RingElem
   val = valuation(a)
   par = parent(a)
   x = gen(laurent_ring(par))
   laur = O(x^numerator(val))
   return parent(a)(laur, denominator(val))
end

parent_type(::Type{FlintPuiseuxSeriesRingElem{T}}) where T <: RingElem = FlintPuiseuxSeriesRing{T}

parent_type(::Type{FlintPuiseuxSeriesFieldElem{T}}) where T <: FieldElem = FlintPuiseuxSeriesField{T}

parent(a::FlintPuiseuxSeriesElem) = a.parent

elem_type(::Type{FlintPuiseuxSeriesRing{T}}) where T <: RingElem = FlintPuiseuxSeriesRingElem{T}

elem_type(::Type{FlintPuiseuxSeriesField{T}}) where T <: FieldElem = FlintPuiseuxSeriesFieldElem{T}

base_ring_type(R::Type{FlintPuiseuxSeriesRing{T}}) where T <: RingElem = base_ring_type(parent_type(T))

base_ring_type(R::Type{FlintPuiseuxSeriesField{T}}) where T <: FieldElem = base_ring_type(parent_type(T))

base_ring(R::FlintPuiseuxSeriesRing{T}) where T <: RingElem = base_ring(laurent_ring(R))

base_ring(R::FlintPuiseuxSeriesField{T}) where T <: FieldElem = base_ring(laurent_ring(R))

max_precision(R::FlintPuiseuxSeriesRing{T}) where T <: RingElem = max_precision(laurent_ring(R))

max_precision(R::FlintPuiseuxSeriesField{T}) where T <: FieldElem = max_precision(laurent_ring(R))

function is_domain_type(::Type{T}) where {S <: RingElem, T <: FlintPuiseuxSeriesElem{S}}
   return is_domain_type(S)
end

is_exact_type(a::Type{T}) where T <: FlintPuiseuxSeriesElem = false

function check_parent(a::FlintPuiseuxSeriesElem, b::FlintPuiseuxSeriesElem)
   parent(a) != parent(b) &&
             error("Incompatible Puiseux series rings in Puiseux series operation")
end

function characteristic(R::FlintPuiseuxSeriesRing{T}) where T <: RingElem
   return characteristic(base_ring(R))
end

function characteristic(R::FlintPuiseuxSeriesField{T}) where T <: FieldElem
   return characteristic(base_ring(R))
end

###############################################################################
#
#   Basic manipulation
#
###############################################################################

function Base.hash(a::FlintPuiseuxSeriesElem, h::UInt)
   b = 0xec4c3951832c37f0%UInt
   b = xor(b, hash(a.data, h))
   b = xor(b, hash(a.scale, h))
   return b
end

precision(a::FlintPuiseuxSeriesElem) = precision(a.data)//a.scale

valuation(a::FlintPuiseuxSeriesElem) = valuation(a.data)//a.scale

scale(a::FlintPuiseuxSeriesElem) = a.scale

function coeff(a::FlintPuiseuxSeriesElem, n::Int)
   s = scale(a)
   return coeff(a.data, n*s)
end

function coeff(a::FlintPuiseuxSeriesElem, r::Rational{Int})
   s = scale(a)
   n = numerator(r)
   d = denominator(r)
   if mod(s, d) != 0
      return base_ring(a)()
   end
   return coeff(a.data, n*div(s, d))
end

zero(R::FlintPuiseuxSeriesRing) = R(0)

zero(R::FlintPuiseuxSeriesField) = R(0)

one(R::FlintPuiseuxSeriesField) = R(1)

one(R::FlintPuiseuxSeriesRing) = R(1)

function gen(R::FlintPuiseuxSeriesRing)
   S = laurent_ring(R)
   return R(gen(S), 1)
end

function gen(R::FlintPuiseuxSeriesField)
   S = laurent_ring(R)
   return R(gen(S), 1)
end

@doc raw"""
    iszero(a::FlintPuiseuxSeriesElem)

Return `true` if the given Puiseux series is arithmetically equal to zero to
its current precision, otherwise return `false`.
"""
iszero(a::FlintPuiseuxSeriesElem) = iszero(a.data)

@doc raw"""
    isone(a::FlintPuiseuxSeriesElem)

Return `true` if the given Puiseux series is arithmetically equal to one to
its current precision, otherwise return `false`.
"""
function isone(a::FlintPuiseuxSeriesElem)
   return isone(a.data)
end

@doc raw"""
    is_gen(a::FlintPuiseuxSeriesElem)

Return `true` if the given Puiseux series is arithmetically equal to the
generator of its Puiseux series ring to its current precision, otherwise return
`false`.
"""
function is_gen(a::FlintPuiseuxSeriesElem)
   return valuation(a) == 1 && pol_length(a.data) == 1 && isone(polcoeff(a.data, 0))
end

@doc raw"""
    is_unit(a::FlintPuiseuxSeriesElem)

Return `true` if the given Puiseux series is arithmetically equal to a unit,
i.e. is invertible, otherwise return `false`.
"""
is_unit(a::FlintPuiseuxSeriesElem) = valuation(a) == 0 && is_unit(polcoeff(a.data, 0))

@doc raw"""
    modulus(a::FlintPuiseuxSeriesElem)

Return the modulus of the coefficients of the given Puiseux series.
"""
modulus(a::FlintPuiseuxSeriesElem) = modulus(base_ring(a))

@doc raw"""
    rescale!(a::FlintPuiseuxSeriesElem)

Rescale so that the scale of the given Puiseux series and the scale of the underlying
Laurent series are coprime. This function is used internally, as all user facing
functions are assumed to rescale their output.
"""
function rescale!(a::FlintPuiseuxSeriesElem)
   if !iszero(a)
      d = gcd(a.scale, gcd(scale(a.data), gcd(valuation(a.data), precision(a.data))))
      if d != 1
         a.data = set_scale!(a.data, div(scale(a.data), d))
         a.data = set_precision!(a.data, div(precision(a.data), d))
         a.data = set_valuation!(a.data, div(valuation(a.data), d))
         a.scale = div(a.scale, d)
      end
   else
      d = gcd(precision(a.data), a.scale)
      if d != 1
         a.data = set_precision!(a.data, div(precision(a.data), d))
         a.data = set_valuation!(a.data, div(valuation(a.data), d))
         a.scale = div(a.scale, d)
      end
   end
   return a
end

function deepcopy_internal(a::FlintPuiseuxSeriesElem, dict::IdDict)
    return parent(a)(deepcopy(a.data), a.scale)
end

###############################################################################
#
#   AbstractString I/O
#
###############################################################################

function expressify(a::FlintPuiseuxSeriesElem,
                                    x = var(parent(a.data)); context = nothing)
   sum = Expr(:call, :+)
   for i in 0:pol_length(a.data) - 1
      c = polcoeff(a.data, i)
      if !iszero(c)
         q = (i*scale(a.data) + valuation(a.data))//a.scale
         xk = iszero(q) ? 1 : isone(q) ? x :
                           Expr(:call, :^, x, expressify(q, context = context))
         if isone(c)
             push!(sum.args, xk)
         else
             push!(sum.args, Expr(:call, :*, expressify(c, context = context), xk))
         end
      end
   end
   q = precision(a.data)//a.scale
   push!(sum.args, Expr(:call, :O,
                         Expr(:call, :^, x, expressify(q, context = context))))
   return sum
end

function Base.show(io::IO, ::MIME"text/plain", a::FlintPuiseuxSeriesElem)
   print(io, AbstractAlgebra.obj_to_string(a, context = io))
end

function Base.show(io::IO, a::FlintPuiseuxSeriesElem)
   print(io, AbstractAlgebra.obj_to_string(a, context = io))
end

function show(io::IO, p::FlintPuiseuxSeriesRing)
   if get(io, :supercompact, false)
      print(io, "Puiseux series ring")
   else
      io = pretty(io)
      print(io, "Puiseux series ring in ", var(laurent_ring(p)), " over ")
      print(IOContext(io, :supercompact => true), Lowercase(), base_ring(p))
   end
end

function show(io::IO, p::FlintPuiseuxSeriesField)
   if get(io, :supercompact, false)
      print(io, "Puiseux series field")
   else
      io = pretty(io)
      print(io, "Puiseux series field in ", var(laurent_ring(p)), " over ")
      print(IOContext(io, :supercompact => true), Lowercase(), base_ring(p))
   end
end

###############################################################################
#
#   Unary operators
#
###############################################################################

function -(a::FlintPuiseuxSeriesElem)
   R = parent(a)
   return R(-a.data, a.scale)
end

###############################################################################
#
#   Binary operators
#
###############################################################################

function +(a::FlintPuiseuxSeriesElem{T}, b::FlintPuiseuxSeriesElem{T}) where T <: RingElem
    s = gcd(a.scale, b.scale)
    zscale = div(a.scale*b.scale, s)
    ainf = div(a.scale, s)
    binf = div(b.scale, s)
    z = parent(a)(inflate(a.data, binf) + inflate(b.data, ainf), zscale)
    z = rescale!(z)
    return z
end

function -(a::FlintPuiseuxSeriesElem{T}, b::FlintPuiseuxSeriesElem{T}) where T <: RingElem
    s = gcd(a.scale, b.scale)
    zscale = div(a.scale*b.scale, s)
    ainf = div(a.scale, s)
    binf = div(b.scale, s)
    z = parent(a)(inflate(a.data, binf) - inflate(b.data, ainf), zscale)
    z = rescale!(z)
    return z
end

function *(a::FlintPuiseuxSeriesElem{T}, b::FlintPuiseuxSeriesElem{T}) where T <: RingElem
    s = gcd(a.scale, b.scale)
    zscale = div(a.scale*b.scale, s)
    ainf = div(a.scale, s)
    binf = div(b.scale, s)
    z = parent(a)(inflate(a.data, binf)*inflate(b.data, ainf), zscale)
    z = rescale!(z)
    return z
end

###############################################################################
#
#   Ad hoc binary operations
#
###############################################################################

function *(x::FlintPuiseuxSeriesElem, y::Integer)
   z = parent(x)(x.data*y, x.scale)
   z = rescale!(z)
   return z
end

*(x::Integer, y::FlintPuiseuxSeriesElem) = y*x

###############################################################################
#
#   Exact division
#
###############################################################################

function divexact(a::FlintPuiseuxSeriesElem{T}, b::FlintPuiseuxSeriesElem{T}; check::Bool=true) where T <: RingElem
    s = gcd(a.scale, b.scale)
    zscale = div(a.scale*b.scale, s)
    ainf = div(a.scale, s)
    binf = div(b.scale, s)
    z = parent(a)(divexact(inflate(a.data, binf), inflate(b.data, ainf); check=check), zscale)
    z = rescale!(z)
    return z
end

###############################################################################
#
#   Inversion
#
###############################################################################

function inv(a::FlintPuiseuxSeriesElem{T}) where T <: RingElement
   z = parent(a)(inv(a.data), a.scale)
   z = rescale!(z)
   return z
end

###############################################################################
#
#   Powering
#
###############################################################################

function ^(a::FlintPuiseuxSeriesElem{T}, b::Int) where T <: RingElem
   # special case powers of x for constructing power series efficiently
   if iszero(a.data)
      return parent(a)(a.data^b, a.scale)
   elseif b == 0
      # in fact, the result would be exact 1 if we had exact series
      return one(parent(a))
   elseif pol_length(a.data) == 1
      return parent(a)(a.data^b, a.scale)
   elseif b == 1
      return deepcopy(a)
   elseif b == -1
      return inv(a)
   end

   if b < 0
      a = inv(a)
      b = -b
   end

   z = parent(a)(a.data^b, a.scale)
   z = rescale!(z)
   return z
end

function ^(a::FlintPuiseuxSeriesElem{T}, b::Rational{Int}) where T <: RingElem
   (pol_length(a.data) != 1 || polcoeff(a.data, 0) != 1) && error("Rational power not implemented")
   z = parent(a)(a.data^numerator(b), a.scale*denominator(b))
   z = rescale!(z)
   return z
end

###############################################################################
#
#   Comparison
#
###############################################################################

function ==(a::FlintPuiseuxSeriesElem{T}, b::FlintPuiseuxSeriesElem{T}) where T <: RingElement
    s = gcd(a.scale, b.scale)
    zscale = div(a.scale*b.scale, s)
    ainf = div(a.scale, s)
    binf = div(b.scale, s)
    return inflate(a.data, binf) == inflate(b.data, ainf)
end

function isequal(a::FlintPuiseuxSeriesElem{T}, b::FlintPuiseuxSeriesElem{T}) where T <: RingElement
   return a.scale == b.scale && isequal(a.data, b.data)
end

###############################################################################
#
#   Ad hoc comparison
#
###############################################################################

==(x::FlintPuiseuxSeriesElem, y::Integer) = x.data == y

==(x::Integer, y::FlintPuiseuxSeriesElem) = y == x

###############################################################################
#
#   Square root
#
###############################################################################

function sqrt(a::FlintPuiseuxSeriesElem{T}; check::Bool=true) where T <: RingElem
   val = valuation(a.data)
   S = parent(a)
   if mod(val, 2) != 0
      return S(sqrt(inflate(a.data, 2); check=check), a.scale*2)
   else
      return S(sqrt(a.data; check=check), a.scale)
   end
end

###############################################################################
#
#   Exponential
#
###############################################################################

function exp(a::FlintPuiseuxSeriesElem{T}) where T <: RingElem
   z = parent(a)(exp(a.data), a.scale)
   z = rescale!(z)
   return z
end

###############################################################################
#
#   Random elements
#
###############################################################################

const FlintPuiseuxSeriesRingOrField = Union{FlintPuiseuxSeriesRing,FlintPuiseuxSeriesField}

RandomExtensions.maketype(S::FlintPuiseuxSeriesRingOrField, _, _) = elem_type(S)

RandomExtensions.make(S::FlintPuiseuxSeriesRingOrField, val_range::AbstractUnitRange{Int},
                      scale_range::AbstractUnitRange{Int}, vs...) =
   make(S, scale_range, make(laurent_ring(S), val_range, vs...))

function rand(rng::AbstractRNG,
              sp::SamplerTrivial{<:Make3{<:RingElement,
                                         <:FlintPuiseuxSeriesRingOrField,
                                         <:AbstractUnitRange{Int}}})
   S, scale_range, v = sp[][1:end]
   (first(scale_range) <= 0 || last(scale_range) <= 0) && error("Scale must be positive")
   return S(rand(rng, v), rand(rng, scale_range))
end

function rand(rng::AbstractRNG, S::FlintPuiseuxSeriesRingOrField,
              val_range::AbstractUnitRange{Int}, scale_range::AbstractUnitRange{Int}, v...)
   rand(rng, make(S, val_range, scale_range, v...))
end

rand(S::FlintPuiseuxSeriesRingOrField, val_range, scale_range, v...) =
   rand(Random.GLOBAL_RNG, S, val_range, scale_range, v...)

###############################################################################
#
#   Unsafe operations
#
###############################################################################

function zero!(a::FlintPuiseuxSeriesElem{T}) where T <: RingElem
   zero!(a.data)
   a.data = set_scale!(a.data, 1)
   return a
end

function mul!(c::FlintPuiseuxSeriesElem{T}, a::FlintPuiseuxSeriesElem{T}, b::FlintPuiseuxSeriesElem{T}) where T <: RingElem
    s = gcd(a.scale, b.scale)
    zscale = div(a.scale*b.scale, s)
    ainf = div(a.scale, s)
    binf = div(b.scale, s)
    c.data = mul!(c.data, inflate(a.data, binf), inflate(b.data, ainf))
    c.scale = zscale
    c = rescale!(c)
    return c
end

function add!(c::FlintPuiseuxSeriesElem{T}, a::FlintPuiseuxSeriesElem{T}, b::FlintPuiseuxSeriesElem{T}) where T <: RingElem
    s = gcd(a.scale, b.scale)
    zscale = div(a.scale*b.scale, s)
    ainf = div(a.scale, s)
    binf = div(b.scale, s)
    c.data = add!(c.data, inflate(a.data, binf), inflate(b.data, ainf))
    c.scale = zscale
    c = rescale!(c)
    return c
end

function addeq!(c::FlintPuiseuxSeriesElem{T}, a::FlintPuiseuxSeriesElem{T}) where T <: RingElem
    s = gcd(c.scale, a.scale)
    zscale = div(c.scale*a.scale, s)
    ainf = div(a.scale, s)
    cinf = div(c.scale, s)
    cnew = inflate(c.data, ainf)
    c.data = addeq!(cnew, inflate(a.data, cinf))
    c.scale = zscale
    c = rescale!(c)
    return c
end

###############################################################################
#
#   Promotion rules
#
###############################################################################

promote_rule(::Type{FlintPuiseuxSeriesRingElem{T}}, ::Type{FlintPuiseuxSeriesRingElem{T}}) where T <: RingElem = FlintPuiseuxSeriesRingElem{T}

promote_rule(::Type{FlintPuiseuxSeriesFieldElem{T}}, ::Type{FlintPuiseuxSeriesFieldElem{T}}) where T <: RingElem = FlintPuiseuxSeriesRingElem{T}

function promote_rule(::Type{FlintPuiseuxSeriesRingElem{T}}, ::Type{U}) where {T <: RingElem, U <: RingElement}
   promote_rule(T, U) == T ? FlintPuiseuxSeriesRingElem{T} : Union{}
end

function promote_rule(::Type{FlintPuiseuxSeriesFieldElem{T}}, ::Type{U}) where {T <: RingElem, U <: RingElement}
   promote_rule(T, U) == T ? FlintPuiseuxSeriesFieldElem{T} : Union{}
end

###############################################################################
#
#   Parent object call overload
#
###############################################################################

function (R::FlintPuiseuxSeriesRing{T})(b::RingElement) where T <: RingElem
   # TODO this method applies to b::ZZPolyRingElem but is broken
   return R(base_ring(R)(b))
end

function (R::FlintPuiseuxSeriesField{T})(b::RingElement) where T <: RingElem
   return R(base_ring(R)(b))
end

function (R::FlintPuiseuxSeriesRing{T})() where T <: RingElem
   z = FlintPuiseuxSeriesRingElem{T}(laurent_ring(R)(), 1)
   z.parent = R
   return z
end

function (R::FlintPuiseuxSeriesField{T})() where T <: RingElem
   z = FlintPuiseuxSeriesFieldElem{T}(laurent_ring(R)(), 1)
   z.parent = R
   return z
end

function (R::FlintPuiseuxSeriesRing{T})(b::T, scale::Int) where T <: RingElem
   z = FlintPuiseuxSeriesRingElem{T}(b, scale)
   z.parent = R
   z = rescale!(z)
   return z
end

function (R::FlintPuiseuxSeriesField{T})(b::T, scale::Int) where T <: RingElem
   z = FlintPuiseuxSeriesFieldElem{T}(b, scale)
   z.parent = R
   z = rescale!(z)
   return z
end

function (R::FlintPuiseuxSeriesRing{T})(b::Union{Integer, Rational}) where T <: RingElem
   z = FlintPuiseuxSeriesRingElem{T}(laurent_ring(R)(b), 1)
   z.parent = R
   return z
end

function (R::FlintPuiseuxSeriesField{T})(b::Rational) where T <: RingElem
   z = FlintPuiseuxSeriesFieldElem{T}(laurent_ring(R)(b), 1)
   z.parent = R
   return z
end

function (R::FlintPuiseuxSeriesRing{T})(b::T) where T <: RingElem
   parent(b) != laurent_ring(R) && error("Unable to coerce to Puiseux series")
   z = FlintPuiseuxSeriesRingElem{T}(b, 1)
   z.parent = R
   return z
end

function (R::FlintPuiseuxSeriesField{T})(b::T) where T <: FieldElem
   parent(b) != laurent_ring(R) && error("Unable to coerce to Puiseux series")
   z = FlintPuiseuxSeriesFieldElem{T}(b, 1)
   z.parent = R
   return z
end

function (R::FlintPuiseuxSeriesRing{T})(b::FlintPuiseuxSeriesRingElem{T}) where T <: RingElem
   parent(b) != R && error("Unable to coerce Puiseux series")
   return b
end

function (R::FlintPuiseuxSeriesField{T})(b::FlintPuiseuxSeriesRingElem{T}) where T <: RingElem
   parent(b) != R && error("Unable to coerce Puiseux series")
   return b
end
